#!/usr/bin/env python3

# Import standard modules ...
import glob
import os

# Import special modules ...
try:
    import matplotlib
    matplotlib.use("Agg")                                                       # NOTE: http://matplotlib.org/faq/howto_faq.html#matplotlib-in-a-web-application-server
    import matplotlib.pyplot
except:
    raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"")
try:
    import numpy
except:
    raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"")

# Import my modules ...
try:
    import pyguymer3
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH")

# ******************************************************************************

# Set image size ...
nx = 4320                                                                       # [px]
ny = 2160                                                                       # [px]

# Loop over BINs ...
for bname in sorted(glob.glob("mask????.bin")):
    # Deduce PNG name and skip if it exists ...
    pname = bname[:-4] + ".png"
    if os.path.exists(pname):
        continue

    print("Creating \"{:s}\" ...".format(pname))

    # Load shrunk mask ...
    shrunkMask = numpy.fromfile(bname, dtype = numpy.float32).reshape(ny, nx)

    # Make image ...
    img = numpy.zeros((ny, nx, 3), dtype = numpy.uint8)

    # Loop over y-axis ...
    for iy in range(ny):
        # Loop over x-axis ...
        for ix in range(nx):
            # Determine colours ...
            r, g, b, a = matplotlib.pyplot.cm.jet(shrunkMask[iy, ix])

            # Set pixel ...
            img[iy, ix, 0] = numpy.uint8(255.0 * r)
            img[iy, ix, 1] = numpy.uint8(255.0 * g)
            img[iy, ix, 2] = numpy.uint8(255.0 * b)

    # Save PNG ...
    pyguymer3.save_array_as_PNG(img, pname, ftype_req = 0)
    pyguymer3.exiftool(pname)
    pyguymer3.optipng(pname)
