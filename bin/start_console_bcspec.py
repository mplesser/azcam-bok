"""
Script to start an azcamconsole application.

Usage: Execute this file from File Explorer
"""

import os
import sys
from pathlib import Path, PurePosixPath

rootfolder = Path(__file__).resolve().parent.parent
rootfolder = rootfolder / "azcam_bok/bcspec"
rootfolder = str(PurePosixPath(rootfolder))

# select which python to use (virtual environments)
python = "ipython.exe"
interactive = "-i"  # "-i" or ""

# parse arguments for command script
if len(sys.argv) > 1:
    arguments = sys.argv[1:]
else:
    arguments = [""]

profile = "azcamconsole"

imports = f"import sys; sys.path.append('{rootfolder}')"
import_command = f"{imports};import console_bcspec; from azcam.cli import *"

# execute
cl = (
    f"{python} --profile {profile} "
    f"--TerminalInteractiveShell.term_title_format={profile} {interactive} "
    f'-c "{import_command}" -- {" ".join(arguments)}'
)
os.system(cl)
