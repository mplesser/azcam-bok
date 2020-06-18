import azcam

# imports for azcamconsole command line
if azcam.db.is_azcamconsole:

    from azcam_bok.common.azcamconsole_shortcuts import *

    from astropy.io import fits as pyfits
    from matplotlib import pylab

# imports for azcamserver command line
elif azcam.db.is_azcamserver:

    from azcam_bok.common.azcamserver_shortcuts import *

# common configuration for server and console
from azcam.console import api

# put the db["objects"] items in the current name space for CLI use
for obj in azcam.db.objects:
    globals()[obj] = azcam.db.objects[obj]
