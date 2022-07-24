#!/usr/bin/env python3

# Import standard modules ...
import glob
import shutil

# Import my modules ...
try:
    import pyguymer3
    import pyguymer3.media
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

# ******************************************************************************

print("Making \"createMask3.mp4\" ...")

# Set list ...
frames = sorted(glob.glob("createMask3_mask????.png"))

# Save 25fps MP4 ...
vname = pyguymer3.media.images2mp4(frames)
shutil.move(vname, "createMask3.mp4")

# Clean up ...
del frames
