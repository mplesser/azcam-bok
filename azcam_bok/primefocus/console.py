# azcamconsole config file

import os
import sys
import datetime
import threading

from azcam.console import azcam
import azcam.shortcuts
from azcam.genpars import GenPars
from azcam_ds9.ds9display import Ds9Display
from azcam_focus.focus import Focus
from azcam_observe.observe import Observe

azcam.log("Loading console environment 90Prime")

# ****************************************************************
# files and folders
# ****************************************************************
azcam.db.systemname = "90prime"
azcam.db.systemfolder = f"{os.path.dirname(__file__)}"
azcam.db.datafolder = os.path.join("/data", azcam.db.systemname)
azcam.db.parfile = f"{azcam.db.datafolder}/parameters_{azcam.db.systemname}_console.ini"

# ****************************************************************
# start logging
# ****************************************************************
tt = datetime.datetime.strftime(datetime.datetime.now(), "%d%b%y_%H%M%S")
azcam.db.logfile = os.path.join(azcam.db.datafolder, "logs", f"console_{tt}.log")
azcam.logging.start_logging(azcam.db.logfile)
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
# try to connect to azcamserver
# ****************************************************************
ports = [2402, 2412, 2422, 2432]
connected = 0
for port in ports:
    connected = azcam.api.connect(port=port)
    if connected:
        break

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
