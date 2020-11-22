# azcamconsole config file for mont4k

import datetime
import os
import sys
import threading

from azcam_ds9.ds9display import Ds9Display
from azcam_observe.observe import Observe

import azcam.shortcuts
from azcam.console import azcam
from azcam.genpars import GenPars

azcam.log("Loading console environment for BCSpec")

# ****************************************************************
# files and folders
# ****************************************************************
azcam.db.systemname = "bcspec"
azcam.db.systemfolder = f"{os.path.dirname(__file__)}"
azcam.db.datafolder = os.path.join("/data", azcam.db.systemname)
azcam.db.parfile = f"{azcam.db.datafolder}/parameters_{azcam.db.systemname}.ini"

# ****************************************************************
# start logging
# ****************************************************************
tt = datetime.datetime.strftime(datetime.datetime.now(), "%d%b%y_%H%M%S")
azcam.db.logger.logfile = os.path.join(azcam.db.datafolder, "logs", f"console_{tt}.log")
azcam.db.logger.start_logging()
azcam.log(f"Configuring console for {azcam.db.systemname}")

# ****************************************************************
# display
# ****************************************************************
display = Ds9Display()
dthread = threading.Thread(target=display.initialize, args=[])
dthread.start()  # thread just for speed

# ****************************************************************
# observe script
# ****************************************************************
observe = Observe()
azcam.db.cli_cmds["observe"] = observe

# ****************************************************************
# try to connect to azcamserver
# ****************************************************************
connected = azcam.api.serverconn.connect(port=2442)
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
