"""
restart_cameraserver

This is just a fix for PC hang issue...
09Oct19 MPL
"""

import os

import azcam

filepath = "C:\\azcam\\camera_servers\\installer64_19.3\\RestartServiceAdmin.bat.lnk"

print("Restarting cameraserver")
if os.path.exists(filepath):
    os.system(filepath)
else:
    azcam.log("Command file for restart_cameraserver not found")
