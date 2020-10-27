# azcamserver config file for 90prime

import datetime
import os
import sys

from azcam_arc.controller_arc import ControllerArc
from azcam_arc.exposure_arc import ExposureArc
from azcam_bok.common.telescope_bok import BokTCS
from azcam_cryocon.tempcon_cryoconm24 import TempConCryoCon
from azcam_ds9.ds9display import Ds9Display

import azcam.shortcuts
from azcam.cmdserver import CommandServer
from azcam.genpars import GenPars
from azcam.header import Header
from azcam.server import azcam
from azcam.webserver.web_server import WebServer

# ****************************************************************
# parse command line arguments
# ****************************************************************
try:
    i = sys.argv.index("-system")
    option = sys.argv[i + 1]
except ValueError:
    option = "menu"

# ****************************************************************
# define folders for system
# ****************************************************************
azcam.db.rootfolder = os.path.abspath(os.path.dirname(__file__))
azcam.db.rootfolder = os.path.normpath(azcam.db.rootfolder).replace("\\", "/")

azcam.db.systemname = "90prime"
azcam.db.systemfolder = os.path.dirname(__file__)
azcam.db.systemfolder = azcam.utils.fix_path(azcam.db.systemfolder)
azcam.db.datafolder = os.path.join("/data", azcam.db.systemname)
azcam.db.datafolder = azcam.utils.fix_path(azcam.db.datafolder)

# ****************************************************************
# configuration menu
# ****************************************************************
print("90prime Startup Menu\n")
menu_options = {
    "90prime (standard mode)": "normal",
    "90primeOne": "90primeone",
    "90prime with overscan rows": "overscan",
    "90prime FAST mode (with overscan rows)": "fast",
    "CSS mode": "css",
}
if option == "menu":
    option = azcam.utils.show_menu(menu_options)

CSS = 0
if "90primeone" in option:
    parfile = os.path.join(azcam.db.datafolder, "parameters_90prime_one.ini")
    template = os.path.join(
        azcam.db.datafolder, "templates", "FitsTemplate_90PrimeOne_master.txt"
    )
    timingfile = os.path.join(
        azcam.db.systemfolder,
        "dspcode",
        "dsptiming_90primeone",
        "90PrimeOne_config0.lod",
    )
    cmdport = 2432
elif "normal" in option:
    parfile = os.path.join(azcam.db.datafolder, "parameters_90prime_normal.ini")
    template = os.path.join(
        azcam.db.datafolder, "templates", "FitsTemplate_90Prime_master.txt"
    )
    timingfile = os.path.join(
        azcam.db.systemfolder, "dspcode", "dsptiming_90prime", "90Prime_config0.lod"
    )
    cmdport = 2402
elif "fast" in option:
    parfile = os.path.join(azcam.db.datafolder, "parameters_90prime_fast.ini")
    template = os.path.join(
        azcam.db.datafolder, "templates", "FitsTemplate_90Prime_master.txt"
    )
    timingfile = os.path.join(
        azcam.db.systemfolder, "dspcode", "dsptiming_fast", "90Prime_config1.lod"
    )
    cmdport = 2402
elif "overscan" in option:
    parfile = os.path.join(azcam.db.datafolder, "parameters_90prime_overscan.ini")
    template = os.path.join(
        azcam.db.datafolder, "templates", "FitsTemplate_90Prime_master.txt"
    )
    timingfile = os.path.join(
        azcam.db.systemfolder, "dspcode", "dsptiming_90prime", "90Prime_config0.lod"
    )
elif "css" in option:
    print("90Prime for CSS")
    CSS = 1
    parfile = os.path.join(azcam.db.datafolder, "parameters_90prime_css.ini")
    template = os.path.join(
        azcam.db.datafolder, "templates", "FitsTemplate_90Prime_css.txt"
    )
    timingfile = os.path.join(
        azcam.db.systemfolder, "dspcode", "dsptiming_90prime", "90Prime_config0.lod"
    )
    cmdport = 2422
else:
    raise azcam.AzcamError("bad server configuration")
azcam.db.parfile = parfile

# ****************************************************************
# enable logging
# ****************************************************************
tt = datetime.datetime.strftime(datetime.datetime.now(), "%d%b%y_%H%M%S")
azcam.db.logfile = os.path.join(azcam.db.datafolder, "logs", f"server_{tt}.log")
azcam.logging.start_logging(azcam.db.logfile, "123")

azcam.log("Configuring for 90prime")

# ****************************************************************
# define and start command server
# ****************************************************************
cmdserver = CommandServer()
cmdserver.port = cmdport
azcam.log(f"Starting cmdserver - listening on port {cmdserver.port}")
# cmdserver.welcome_message = "Welcome - azcam-itl server"
cmdserver.start()

# ****************************************************************
# controller
# ****************************************************************
controller = ControllerArc()
controller.timing_board = "arc22"
controller.clock_boards = ["arc32"]
controller.video_boards = ["arc47", "arc47", "arc47", "arc47"]
controller.set_boards()
controller.video_gain = 1
controller.video_speed = 1
controller.camserver.set_server("localhost", 2405)
controller.pci_file = os.path.join(
    azcam.db.systemfolder, "dspcode", "dsppci", "pci3.lod"
)
controller.timing_file = timingfile

# ****************************************************************
# temperature controller
# ****************************************************************
tempcon = TempConCryoCon()
tempcon.control_temperature = -135.0
# tempcon.host = "10.0.0.45"
tempcon.host = "10.30.3.32"
tempcon.init_commands = [
    "input A:units C",
    "input B:units C",
    "input C:units C",
    "input A:isenix 2",
    "input B:isenix 2",
    "loop 1:type pid",
    "loop 1:range mid",
    "loop 1:maxpwr 100",
]

# ****************************************************************
# dewar
# ****************************************************************
controller.header.set_keyword("DEWAR", "90prime", "Dewar name")

# ****************************************************************
# exposure
# ****************************************************************
exposure = ExposureArc()
exposure.filetype = azcam.db.filetypes["MEF"]
exposure.image.filetype = azcam.db.filetypes["MEF"]
exposure.update_headers_in_background = 1
exposure.display_image = 0
exposure.remote_imageserver_filename = "azcamimage.fits"
exposure.image.server_type = "dataserver"
remote_imageserver_host = "10.30.1.2"
remote_imageserver_port = 6543
exposure.set_remote_server(remote_imageserver_host, remote_imageserver_port)
# exposure.set_remote_server()

# ****************************************************************
# focus script - server-side
# ****************************************************************
from azcam_focus.focus_server import FocusServer

focus = FocusServer()
azcam.db.cli_cmds["focus"] = focus
focus.focus_component = "instrument"
focus.focus_type = "step"

# ****************************************************************
# instrument
# ****************************************************************
from azcam_bok.primefocus.instrument_pf import PrimeFocusInstrument

instrument = PrimeFocusInstrument()
azcam.db.coord_object = "instrument"

# ****************************************************************
# telescope
# ****************************************************************
telescope = BokTCS()

# ****************************************************************
# system header template
# ****************************************************************
sysheader = Header("90prime", template)
sysheader.set_header("system", 0)

# ****************************************************************
# detector
# ****************************************************************
if "90primeone" in option:
    from azcam_bok.primefocus.detector_bok90prime import \
        detector_bok90prime_one

    exposure.set_detpars(detector_bok90prime_one)
else:
    from azcam_bok.primefocus.detector_bok90prime import detector_bok90prime

    if "overscan" in option:
        detector_bok90prime["format"] = [4032 * 2, 6, 0, 20, 4096 * 2, 0, 0, 20, 0]
    exposure.set_detpars(detector_bok90prime)

# ****************************************************************
# display
# ****************************************************************
display = Ds9Display()

# ****************************************************************
# system-specific
# ****************************************************************
if CSS:
    from azcam_bok.primefocus.css import CSS

    css = CSS()
    azcam.db.cli_cmds["css"] = css
    exposure.image.server_type = "azcam"
    exposure.set_remote_server("10.30.6.2", 6543)
    exposure.filename.folder = "/home/css"

# ****************************************************************
# read par file
# ****************************************************************
azcam.db.genpars = GenPars()
pardict = azcam.db.genpars.parfile_read(parfile)["azcamserver"]
azcam.utils.update_pars(0, pardict)
wd = azcam.db.genpars.get_par(pardict, "wd", "default")
azcam.utils.curdir(wd)

# ****************************************************************
# web server
# ****************************************************************
from azcam.webserver.web_server import WebServer

webserver = WebServer()

import azcam_exptool
import azcam_status
import azcam_webobs

webserver.start()

# ****************************************************************
# camera server
# ****************************************************************
import azcam_bok.primefocus.restart_cameraserver

# ****************************************************************
# GUIs
# ****************************************************************
if 1:
    import azcam_bok.common.start_azcamtool

# ****************************************************************
# finish
# ****************************************************************
azcam.log("Configuration complete")
