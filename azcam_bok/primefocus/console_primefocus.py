# azcamconsole config file

import os
import sys
import datetime
import threading

from PySide2.QtWidgets import QApplication

import azcam
import azcam.console
from azcam.console import api
import azcam.shortcuts_console
from azcam.displays.ds9display import Ds9Display
from focus.focus import Focus
from observe.observe import Observe
from azcam.genpars import GenPars

azcam.log("Loading azcam-bok environment")

# ****************************************************************
# files and folders
# ****************************************************************
azcam.db.systemname = "90prime"
azcam.db.systemfolder = f"{os.path.dirname(__file__)}"
azcam.utils.add_searchfolder(azcam.db.systemfolder, 0)  # top level only
azcam.utils.add_searchfolder(os.path.join(azcam.db.systemfolder, "common"), 1)
azcam.db.datafolder = os.path.join("/data", azcam.db.systemname)
azcam.db.parfile = f"{azcam.db.datafolder}/parameters_{azcam.db.systemname}_console.ini"

# ****************************************************************
# start logging
# ****************************************************************
tt = datetime.datetime.strftime(datetime.datetime.now(), "%d%b%y_%H%M%S")
azcam.db.logfile = os.path.join(azcam.db.datafolder, "logs", f"console_{tt}.log")
azcam.utils.start_logging(azcam.db.logfile)
azcam.log(f"Configuring console for {azcam.db.systemname}")

# ****************************************************************
# display
# ****************************************************************
display = Ds9Display()
dthread = threading.Thread(target=display.initialize, args=[])
dthread.start()  # thread just for speed

# ****************************************************************
# create Qt app
# ****************************************************************
app = QApplication(sys.argv)
azcam.db.qtapp = app

# ****************************************************************
# add scripts to sys.path for Run
# ****************************************************************
azcam.utils.add_searchfolder(os.path.join(azcam.db.systemfolder, "scripts"))

# ****************************************************************
# observe script
# ****************************************************************
observe = Observe()
observe.move_telescope_during_readout = 1
azcam.db.cli_cmds["observe"] = observe

# ****************************************************************
# focus script
# ****************************************************************
focus = Focus()
azcam.db.cli_cmds["focus"] = focus
focus.focus_component = "instrument"
focus.focus_type = "step"

# ****************************************************************
# try to connect to azcam
# ****************************************************************
connected = api.connect()  # default host and port
if connected:
    azcam.log("Connected to azcamserver")
else:
    azcam.log("Not connected to azcamserver")

# ****************************************************************
# read par file
# ****************************************************************
azcam.db.genpars = GenPars()
pardict = azcam.db.genpars.parfile_read(azcam.db.parfile)["azcamconsole"]
azcam.utils.update_pars(0, pardict)
wd = azcam.db.genpars.get_par(pardict, "wd", "default")
azcam.utils.curdir(wd)

# ****************************************************************
# define names to imported into namespace when using cli
# # ****************************************************************
azcam.db.cli_cmds.update({"azcam": azcam, "db": azcam.db, "api": api})

# ****************************************************************
# clean namespace
# # ****************************************************************
del azcam.focalplane, azcam.displays, azcam.shortcuts_console
del azcam.header, azcam.image
