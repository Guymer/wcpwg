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
    matplotlib.use("Agg")                                                       # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
    import matplotlib.pyplot
except:
    raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None

# Import my modules ...
try:
    import pyguymer3
    import pyguymer3.geo
    import pyguymer3.image
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
]                                                                               # [°]
extCO = [
    -110.0, # left
    -102.0, # right
     +36.0, # bottom
     +42.0, # top
]                                                                               # [°]
extUS = [
    -125.0, # left
     -66.0, # right
     +25.0, # bottom
     +50.0, # top
]                                                                               # [°]

# Set fields ...
fields = ["diff", "flags"]

# ******************************************************************************

# Loop over fields ...
for field in fields:
    # Define PNG file name and check if it exists already ...
    pfile = f"{field}CL.png"
    if not os.path.exists(pfile):
        print(f"Making \"{pfile}\" ...")

        # Create figure ...
        fg = matplotlib.pyplot.figure(
                dpi = 300,
            figsize = (9, 9),
        )

        # Create axis ...
        ax = fg.add_subplot(
            projection = cartopy.crs.Orthographic(
                central_longitude = 0.5 * (extCL[0] + extCL[1]),
                 central_latitude = 0.5 * (extCL[2] + extCL[3]),
            )
        )

        # Configure axis ...
        ax.coastlines(
            resolution = "10m",
                 color = "blue",
             linewidth = 1.0,
        )
        ax.set_extent(extCL)
        ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")
        pyguymer3.geo.add_map_background(
            ax,
                extent = extCL,
                  name = field,
            resolution = field,
        )

        # Configure figure ...
        fg.tight_layout()

        # Save figure ...
        fg.savefig(
            pfile,
                   dpi = 300,
            pad_inches = 0.1,
        )
        matplotlib.pyplot.close(fg)

        # Optimize PNG ...
        pyguymer3.image.optimize_image(pfile, strip = True)

    # **************************************************************************

    # Define PNG file name and check if it exists already ...
    pfile = f"{field}CO.png"
    if not os.path.exists(pfile):
        print(f"Making \"{pfile}\" ...")

        # Create figure ...
        fg = matplotlib.pyplot.figure(
                dpi = 300,
            figsize = (9, 9),
        )

        # Create axis ...
        ax = fg.add_subplot(
            projection = cartopy.crs.Orthographic(
                central_longitude = 0.5 * (extCO[0] + extCO[1]),
                 central_latitude = 0.5 * (extCO[2] + extCO[3]),
            )
        )

        # Configure axis ...
        ax.coastlines(
            resolution = "10m",
                 color = "blue",
             linewidth = 1.0,
        )
        ax.set_extent(extCO)
        ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")
        pyguymer3.geo.add_map_background(
            ax,
                extent = extCO,
                  name = field,
            resolution = field,
        )

        # Find file containing all the populated places shapes ...
        sfile = cartopy.io.shapereader.natural_earth(
              category = "cultural",
                  name = "populated_places",
            resolution = "10m",
        )

        # Loop over records ...
        for record in cartopy.io.shapereader.Reader(sfile).records():
            # Create short-hands ...
            neAdmin0Name = pyguymer3.geo.getRecordAttribute(record, "ADM0NAME")
            neAdmin1Name = pyguymer3.geo.getRecordAttribute(record, "ADM1NAME")
            neName = pyguymer3.geo.getRecordAttribute(record, "NAME")

            # Check that populated place is in the United States of America ...
            if neAdmin0Name == "United States of America":
                # Check that populated place is in Colorado ...
                if neAdmin1Name == "Colorado":
                    # Annotate the plot ...
                    # NOTE: https://stackoverflow.com/a/25421922
                    ax.plot(
                        record.geometry.x,
                        record.geometry.y,
                        "o",
                            color = "blue",
                        transform = cartopy.crs.PlateCarree(),
                    )
                    ax.annotate(
                        neName,
                             color = "blue",
                        textcoords = "offset points",
                                xy = (record.geometry.x, record.geometry.y),
                          xycoords = cartopy.crs.PlateCarree()._as_mpl_transform(ax),
                            xytext = (3, 2),
                    )

        # Configure figure ...
        fg.tight_layout()

        # Save figure ...
        fg.savefig(
            pfile,
                   dpi = 300,
            pad_inches = 0.1,
        )
        matplotlib.pyplot.close(fg)

        # Optimize PNG ...
        pyguymer3.image.optimize_image(pfile, strip = True)

    # **************************************************************************

    # Define PNG file name and check if it exists already ...
    pfile = f"{field}US.png"
    if not os.path.exists(pfile):
        print(f"Making \"{pfile}\" ...")

        # Create figure ...
        fg = matplotlib.pyplot.figure(
                dpi = 300,
            figsize = (9, 9),
        )

        # Create axis ...
        ax = fg.add_subplot(
            projection = cartopy.crs.Orthographic(
                central_longitude = 0.5 * (extUS[0] + extUS[1]),
                 central_latitude = 0.5 * (extUS[2] + extUS[3]),
            )
        )

        # Configure axis ...
        ax.coastlines(
            resolution = "10m",
                 color = "blue",
             linewidth = 1.0,
        )
        ax.set_extent(extUS)
        ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")
        pyguymer3.geo.add_map_background(
            ax,
                extent = extUS,
                  name = field,
            resolution = field,
        )

        # Configure figure ...
        fg.tight_layout()

        # Save figure ...
        fg.savefig(
            pfile,
                   dpi = 300,
            pad_inches = 0.1,
        )
        matplotlib.pyplot.close(fg)

        # Optimize PNG ...
        pyguymer3.image.optimize_image(pfile, strip = True)
