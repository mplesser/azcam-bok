# azcamconsole config file

import datetime
import os
import threading

import azcam
import azcam.console
import azcam.shortcuts_console
from azcam_ds9.ds9display import Ds9Display
from azcam_focus.focus import Focus
from azcam_observe.observe import Observe

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
    connected = azcam.api.server.connect(port=port)
    if connected:
        break

if connected:
    azcam.log("Connected to azcamserver")
else:
    azcam.log("Not connected to azcamserver")

# ****************************************************************
# read par file
# ****************************************************************
pardict = azcam.api.config.parfile_read(azcam.db.parfile)
azcam.api.config.update_pars(0, pardict["azcamconsole"])
