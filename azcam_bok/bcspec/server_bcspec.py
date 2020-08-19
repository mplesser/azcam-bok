import os
import sys
import datetime
import types

from PySide2.QtWidgets import QApplication

import azcam
import azcam.server
import azcam.logging
from azcam.genpars import GenPars
import azcam.shortcuts_server
from azcam.displays.ds9display import Ds9Display
from azcam.systemheader import SystemHeader
from azcam.controllers.controller_arc import ControllerArc
from azcam.tempcons.tempcon_arc import TempConArc
from azcam.exposures.exposure_arc import ExposureArc
from azcam.cmdserver import CommandServer
from azcam.webserver.web_server import WebServer

from instrument_bcspec import BCSpecInstrument

common = os.path.abspath(os.path.dirname(__file__))
common = os.path.abspath(os.path.join(common, "../common"))
azcam.utils.add_searchfolder(common)
from telescope_bok import BokTCS

# ****************************************************************
# define folders for system
# ****************************************************************
azcam.db.systemname = "bcspec"
azcam.db.systemfolder = os.path.dirname(__file__)
azcam.db.systemfolder = azcam.utils.fix_path(azcam.db.systemfolder)
azcam.db.datafolder = os.path.join("/data", azcam.db.systemname)
azcam.db.datafolder = azcam.utils.fix_path(azcam.db.datafolder)
azcam.db.parfile = f"{azcam.db.datafolder}/parameters_{azcam.db.systemname}.ini"

# ****************************************************************
# enable logging
# ****************************************************************
tt = datetime.datetime.strftime(datetime.datetime.now(), "%d%b%y_%H%M%S")
azcam.db.logfile = os.path.join(azcam.db.datafolder, "logs", f"server_{tt}.log")
azcam.logging.start_logging(azcam.db.logfile, "123")

azcam.log(f"Configuring for BCSpec")

# ****************************************************************
# define and start command server
# ****************************************************************
cmdserver = CommandServer()
cmdserver.port = 2402
azcam.log(f"Starting command server listening on port {cmdserver.port}")
# cmdserver.welcome_message = "Welcome - azcam-itl server"
cmdserver.start()

# ****************************************************************
# create Qt app
# ****************************************************************
app = QApplication(sys.argv)
azcam.db.qtapp = app

# ****************************************************************
# controller
# ****************************************************************
controller = ControllerArc()
controller.timing_board = "gen1"
controller.clock_boards = ["gen1"]
controller.video_boards = ["gen1"]
controller.utility_board = "gen1"
controller.set_boards()
controller.video_gain = 1
controller.video_speed = 1
# controller.camserver.set_server("10.30.1.34", 2405)
controller.camserver.set_server("bokccd5", 2405)
controller.utility_file = os.path.join(azcam.db.systemfolder, "dspcode", "dsputility", "util1.lod")
controller.pci_file = os.path.join(azcam.db.systemfolder, "dspcode", "dsppci", "pci1.lod")
controller.timing_file = os.path.join(
    azcam.db.systemfolder, "dspcode", "dsptiming", "tim1_norm_LR.lod"
)

# ****************************************************************
# temperature controller
# ****************************************************************
tempcon = TempConArc()
tempcon.control_temperature = -135.0
tempcon.set_calibrations([1, 1, 3])

# ****************************************************************
# dewar
# ****************************************************************
controller.header.set_keyword("DEWAR", "bcspec_dewar", "Dewar name")

# ****************************************************************
# exposure
# ****************************************************************
exposure = ExposureArc()
exposure.filetype = azcam.db.filetypes["FITS"]
exposure.image.filetype = azcam.db.filetypes["FITS"]
exposure.display_image = 0
exposure.filename.folder = azcam.db.datafolder
exposure.filename.folder = "/home/bokobs"
exposure.image.remote_imageserver_filename = "azcamimage.fits"
remote_imageserver_host = "10.30.1.2"  # bart
remote_imageserver_port = 6543
# exposure.set_remote_server(remote_imageserver_host, remote_imageserver_port)
exposure.set_remote_server()

# detector
detector_bcspec = {
    "name": "1200x800",
    "description": "STA 1200x800 CCD",
    "ref_pixel": [600, 400],
    "format": [1200, 18, 0, 20, 800, 0, 0, 0, 0],
    "focalplane": [1, 1, 1, 1, "0"],
    "roi": [1, 1200, 1, 800, 2, 2],
    "extension_position": [[1, 1]],
    "jpg_order": [1, 1],
    "ctype": ["LINEAR", "LINEAR"],
}
exposure.set_detpars(detector_bcspec)

# ****************************************************************
# custom keyword change
# ****************************************************************
def finish(self):
    """
    Allow custom operations at end of exposure.
    Set custom header values.
    """

    # self is exposure instance
    ref1 = 1.0
    ref2 = 1.0
    self.image.header.set_keyword("CRPIX1", ref1, "Coordinate reference pixel")
    self.image.header.set_keyword("CRPIX2", ref2, "Coordinate reference pixel")

    CD1_1 = 1.0
    CD1_2 = 0.0
    CD2_1 = 0.0
    CD2_2 = 1.0
    self.image.header.set_keyword("CD1_1", CD1_1, "Coordinate matrix")
    self.image.header.set_keyword("CD1_2", CD1_2, "Coordinate matrix")
    self.image.header.set_keyword("CD2_1", CD2_1, "Coordinate matrix")
    self.image.header.set_keyword("CD2_2", CD2_2, "Coordinate matrix")

    return


exposure.finish = types.MethodType(finish, exposure)

# ****************************************************************
# instrument
# ****************************************************************
instrument = BCSpecInstrument()

# ****************************************************************
# telescope
# ****************************************************************
telescope = BokTCS()

# ****************************************************************
# system header template
# ****************************************************************
template = os.path.join(azcam.db.datafolder, "templates", "FitsTemplate_bcspec_master.txt")
system = SystemHeader("bcspec", template)

# ****************************************************************
# display
# ****************************************************************
display = Ds9Display()

# ****************************************************************
# read par file
# ****************************************************************
azcam.db.genpars = GenPars()
pardict = azcam.db.genpars.parfile_read(azcam.db.parfile)["azcamserver"]
azcam.utils.update_pars(0, pardict)
wd = azcam.db.genpars.get_par(pardict, "wd", "default")
azcam.utils.curdir(wd)

# ****************************************************************
# web server
# ****************************************************************
webserver = WebServer()
webserver.start()

# ****************************************************************
# GUIs
# ****************************************************************
if 1:
    import start_azcamtool

# ****************************************************************
# define names to imported into namespace when using cli
# # ****************************************************************
azcam.db.cli_cmds.update({"azcam": azcam, "db": azcam.db})

# ****************************************************************
# finish
# ****************************************************************
azcam.log("Configuration complete")
