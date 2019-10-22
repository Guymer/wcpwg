#!/usr/bin/env python3

# Import standard modules ...
import os

# Import special modules ...
try:
    import cartopy
    import cartopy.crs
except:
    raise Exception("run \"pip install --user cartopy\"")
try:
    import matplotlib
    matplotlib.use("Agg")                                                       # NOTE: http://matplotlib.org/faq/howto_faq.html#matplotlib-in-a-web-application-server
    import matplotlib.pyplot
except:
    raise Exception("run \"pip install --user matplotlib\"")

# Import my modules ...
try:
    import pyguymer3
except:
    raise Exception("you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH")

# ******************************************************************************

# Configure cartopy ...
os.environ["CARTOPY_USER_BACKGROUNDS"] = os.getcwd()

# Configure matplotlib ...
matplotlib.pyplot.rcParams.update({"font.size" : 8})

# Set extents of the regions ...
extUS = [
    -125.0, # left
     -66.0, # right
      24.0, # bottom
      50.0, # top
]

# ******************************************************************************

# Define PNG file name and check if it exists already ...
pfile = "diffUS.png"
if not os.path.exists(pfile):
    print("Making \"{:s}\" ...".format(pfile))

    # Create plot ...
    fg = matplotlib.pyplot.figure(figsize = (9, 4), dpi = 300)
    ax = matplotlib.pyplot.axes(
        projection = cartopy.crs.Orthographic(
            central_longitude = 0.5 * (extUS[0] + extUS[1]),
             central_latitude = 0.5 * (extUS[2] + extUS[3])
        )
    )
    ax.set_extent(extUS)

    # Add background images ...
    pyguymer3.add_map_background(ax, name = "diff", resolution = "diff", extent = extUS)

    # Add coastlines ...
    ax.coastlines(resolution = "10m", color = "blue", linewidth = 0.1)

    # Save plot ...
    fg.savefig(pfile, bbox_inches = "tight", dpi = 300, pad_inches = 0.1)
    pyguymer3.exiftool(pfile)
    pyguymer3.optipng(pfile)
    matplotlib.pyplot.close("all")

# ******************************************************************************

# Define PNG file name and check if it exists already ...
pfile = "flagsUS.png"
if not os.path.exists(pfile):
    print("Making \"{:s}\" ...".format(pfile))

    # Create plot ...
    fg = matplotlib.pyplot.figure(figsize = (9, 4), dpi = 300)
    ax = matplotlib.pyplot.axes(
        projection = cartopy.crs.Orthographic(
            central_longitude = 0.5 * (extUS[0] + extUS[1]),
             central_latitude = 0.5 * (extUS[2] + extUS[3])
        )
    )
    ax.set_extent(extUS)

    # Add background images ...
    pyguymer3.add_map_background(ax, name = "flags", resolution = "flags", extent = extUS)

    # Add coastlines ...
    ax.coastlines(resolution = "10m", color = "blue", linewidth = 0.1)

    # Save plot ...
    fg.savefig(pfile, bbox_inches = "tight", dpi = 300, pad_inches = 0.1)
    pyguymer3.exiftool(pfile)
    pyguymer3.optipng(pfile)
    matplotlib.pyplot.close("all")
