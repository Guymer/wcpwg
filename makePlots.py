#!/usr/bin/env python3

# Import standard modules ...
import os

# Import special modules ...
try:
    import cartopy
    import cartopy.crs
except:
    raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
try:
    import matplotlib
    matplotlib.use("Agg")                                                       # NOTE: https://matplotlib.org/gallery/user_interfaces/canvasagg.html
    import matplotlib.pyplot
except:
    raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None

# Import my modules ...
try:
    import pyguymer3
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

# ******************************************************************************

# Configure cartopy ...
os.environ["CARTOPY_USER_BACKGROUNDS"] = os.getcwd()

# Configure matplotlib ...
matplotlib.pyplot.rcParams.update({"font.size" : 8})

# Set extents of the regions ...
extCL = [
     -76.0, # left
     -66.0, # right
     -54.0, # bottom
     -17.0, # top
]
extCO = [
    -110.0, # left
    -102.0, # right
     +36.0, # bottom
     +42.0, # top
]
extUS = [
    -125.0, # left
     -66.0, # right
     +25.0, # bottom
     +50.0, # top
]

# ******************************************************************************

# Define PNG file name and check if it exists already ...
pfile = "diffCL.png"
if not os.path.exists(pfile):
    print("Making \"{:s}\" ...".format(pfile))

    # Create plot ...
    fg = matplotlib.pyplot.figure(figsize = (9, 9), dpi = 300)
    ax = matplotlib.pyplot.axes(
        projection = cartopy.crs.Orthographic(
            central_longitude = 0.5 * (extCL[0] + extCL[1]),
             central_latitude = 0.5 * (extCL[2] + extCL[3])
        )
    )
    ax.set_extent(extCL)
    ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")

    # Add background image ...
    pyguymer3.add_map_background(ax, name = "diff", resolution = "diff", extent = extCL)

    # Add coastlines ...
    ax.coastlines(resolution = "10m", color = "blue", linewidth = 0.1)

    # Save plot ...
    fg.savefig(pfile, bbox_inches = "tight", dpi = 300, pad_inches = 0.1)
    pyguymer3.optimize_image(pfile, strip = True)
    matplotlib.pyplot.close(fg)

# ******************************************************************************

# Define PNG file name and check if it exists already ...
pfile = "diffCO.png"
if not os.path.exists(pfile):
    print("Making \"{:s}\" ...".format(pfile))

    # Create plot ...
    fg = matplotlib.pyplot.figure(figsize = (9, 9), dpi = 300)
    ax = matplotlib.pyplot.axes(
        projection = cartopy.crs.Orthographic(
            central_longitude = 0.5 * (extCO[0] + extCO[1]),
             central_latitude = 0.5 * (extCO[2] + extCO[3])
        )
    )
    ax.set_extent(extCO)
    ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")

    # Add background image ...
    pyguymer3.add_map_background(ax, name = "diff", resolution = "diff", extent = extCO)

    # Add coastlines ...
    ax.coastlines(resolution = "10m", color = "blue", linewidth = 0.1)

    # Find file containing all the populated places shapes ...
    shape_file = cartopy.io.shapereader.natural_earth(
        resolution = "10m",
          category = "cultural",
              name = "populated_places"
    )

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(shape_file).records():
        # Check that populated place is in the United States of America ...
        if record.attributes["ADM0NAME"] == "United States of America":
            # Check that populated place is in Colorado ...
            if record.attributes["ADM1NAME"] == "Colorado":
                # Annotate the plot ...
                # NOTE: https://stackoverflow.com/a/25421922
                ax.plot(
                    record.geometry.x,
                    record.geometry.y,
                    "o",
                    color = "blue",
                    transform = cartopy.crs.PlateCarree()
                )
                ax.annotate(
                    record.attributes["NAME"],
                    xy = (record.geometry.x, record.geometry.y),
                    xycoords = cartopy.crs.PlateCarree()._as_mpl_transform(ax),
                    xytext = (3, 2),
                    textcoords = "offset points",
                    color = "blue"
                )

    # Save plot ...
    fg.savefig(pfile, bbox_inches = "tight", dpi = 300, pad_inches = 0.1)
    pyguymer3.optimize_image(pfile, strip = True)
    matplotlib.pyplot.close(fg)

# ******************************************************************************

# Define PNG file name and check if it exists already ...
pfile = "diffUS.png"
if not os.path.exists(pfile):
    print("Making \"{:s}\" ...".format(pfile))

    # Create plot ...
    fg = matplotlib.pyplot.figure(figsize = (9, 9), dpi = 300)
    ax = matplotlib.pyplot.axes(
        projection = cartopy.crs.Orthographic(
            central_longitude = 0.5 * (extUS[0] + extUS[1]),
             central_latitude = 0.5 * (extUS[2] + extUS[3])
        )
    )
    ax.set_extent(extUS)
    ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")

    # Add background image ...
    pyguymer3.add_map_background(ax, name = "diff", resolution = "diff", extent = extUS)

    # Add coastlines ...
    ax.coastlines(resolution = "10m", color = "blue", linewidth = 0.1)

    # Save plot ...
    fg.savefig(pfile, bbox_inches = "tight", dpi = 300, pad_inches = 0.1)
    pyguymer3.optimize_image(pfile, strip = True)
    matplotlib.pyplot.close(fg)

# ******************************************************************************

# Define PNG file name and check if it exists already ...
pfile = "flagsCL.png"
if not os.path.exists(pfile):
    print("Making \"{:s}\" ...".format(pfile))

    # Create plot ...
    fg = matplotlib.pyplot.figure(figsize = (9, 9), dpi = 300)
    ax = matplotlib.pyplot.axes(
        projection = cartopy.crs.Orthographic(
            central_longitude = 0.5 * (extCL[0] + extCL[1]),
             central_latitude = 0.5 * (extCL[2] + extCL[3])
        )
    )
    ax.set_extent(extCL)
    ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")

    # Add background image ...
    pyguymer3.add_map_background(ax, name = "flags", resolution = "flags", extent = extCL)

    # Add coastlines ...
    ax.coastlines(resolution = "10m", color = "blue", linewidth = 0.1)

    # Save plot ...
    fg.savefig(pfile, bbox_inches = "tight", dpi = 300, pad_inches = 0.1)
    pyguymer3.optimize_image(pfile, strip = True)
    matplotlib.pyplot.close(fg)

# ******************************************************************************

# Define PNG file name and check if it exists already ...
pfile = "flagsCO.png"
if not os.path.exists(pfile):
    print("Making \"{:s}\" ...".format(pfile))

    # Create plot ...
    fg = matplotlib.pyplot.figure(figsize = (9, 9), dpi = 300)
    ax = matplotlib.pyplot.axes(
        projection = cartopy.crs.Orthographic(
            central_longitude = 0.5 * (extCO[0] + extCO[1]),
             central_latitude = 0.5 * (extCO[2] + extCO[3])
        )
    )
    ax.set_extent(extCO)
    ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")

    # Add background image ...
    pyguymer3.add_map_background(ax, name = "flags", resolution = "flags", extent = extCO)

    # Add coastlines ...
    ax.coastlines(resolution = "10m", color = "blue", linewidth = 0.1)

    # Find file containing all the populated places shapes ...
    shape_file = cartopy.io.shapereader.natural_earth(
        resolution = "10m",
          category = "cultural",
              name = "populated_places"
    )

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(shape_file).records():
        # Check that populated place is in the United States of America ...
        if record.attributes["ADM0NAME"] == "United States of America":
            # Check that populated place is in Colorado ...
            if record.attributes["ADM1NAME"] == "Colorado":
                # Annotate the plot ...
                # NOTE: https://stackoverflow.com/a/25421922
                ax.plot(
                    record.geometry.x,
                    record.geometry.y,
                    "o",
                    color = "blue",
                    transform = cartopy.crs.PlateCarree()
                )
                ax.annotate(
                    record.attributes["NAME"],
                    xy = (record.geometry.x, record.geometry.y),
                    xycoords = cartopy.crs.PlateCarree()._as_mpl_transform(ax),
                    xytext = (3, 2),
                    textcoords = "offset points",
                    color = "blue"
                )

    # Save plot ...
    fg.savefig(pfile, bbox_inches = "tight", dpi = 300, pad_inches = 0.1)
    pyguymer3.optimize_image(pfile, strip = True)
    matplotlib.pyplot.close(fg)

# ******************************************************************************

# Define PNG file name and check if it exists already ...
pfile = "flagsUS.png"
if not os.path.exists(pfile):
    print("Making \"{:s}\" ...".format(pfile))

    # Create plot ...
    fg = matplotlib.pyplot.figure(figsize = (9, 9), dpi = 300)
    ax = matplotlib.pyplot.axes(
        projection = cartopy.crs.Orthographic(
            central_longitude = 0.5 * (extUS[0] + extUS[1]),
             central_latitude = 0.5 * (extUS[2] + extUS[3])
        )
    )
    ax.set_extent(extUS)
    ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")

    # Add background image ...
    pyguymer3.add_map_background(ax, name = "flags", resolution = "flags", extent = extUS)

    # Add coastlines ...
    ax.coastlines(resolution = "10m", color = "blue", linewidth = 0.1)

    # Save plot ...
    fg.savefig(pfile, bbox_inches = "tight", dpi = 300, pad_inches = 0.1)
    pyguymer3.optimize_image(pfile, strip = True)
    matplotlib.pyplot.close(fg)
