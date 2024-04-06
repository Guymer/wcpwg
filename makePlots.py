#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.11/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import os

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : os.path.expanduser("~/.local/share/cartopy_cache"),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "backend" : "Agg",                                       # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                    "figure.dpi" : 300,
                "figure.figsize" : (9.6, 7.2),                                  # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                     "font.size" : 8,
            }
        )
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

    # **************************************************************************

    # Configure Cartopy ...
    os.environ["CARTOPY_USER_BACKGROUNDS"] = os.getcwd()

    # Set extents of the regions ...
    extCL = [
         -74.0, # left
         -63.0, # right
         -29.0, # bottom
         -18.0, # top
    ]                                                                           # [°]
    extCO = [
        -110.0, # left
        -103.0, # right
         +36.0, # bottom
         +42.0, # top
    ]                                                                           # [°]
    extUS = [
        -125.0, # left
         -66.0, # right
         +24.0, # bottom
         +50.0, # top
    ]                                                                           # [°]

    # Set fields ...
    fields = ["diff", "flags"]

    # **************************************************************************

    # Loop over fields ...
    for field in fields:
        # Define PNG file name and check if it exists already ...
        pfile = f"{field}CL.png"
        if not os.path.exists(pfile):
            print(f"Making \"{pfile}\" ...")

            # Create figure ...
            fg = matplotlib.pyplot.figure(figsize = (7.2, 7.2))

            # Create axis ...
            ax = pyguymer3.geo.add_topDown_axis(
                fg,
                -60.0,
                -35.0,
                dist = 2000.0e3,
            )

            # Configure axis ...
            ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")
            pyguymer3.geo.add_coastlines(
                ax,
                colorName = "blue",
                linewidth = 1.0,
            )
            pyguymer3.geo.add_map_background(
                ax,
                    extent = extCL,
                      name = field,
                resolution = field,
            )
            pyguymer3.geo.add_horizontal_gridlines(
                ax,
                locs = range(-90, +91, 1),
            )
            pyguymer3.geo.add_vertical_gridlines(
                ax,
                locs = range(-180, +181, 1),
            )

            # Configure figure ...
            fg.tight_layout()

            # Save figure ...
            fg.savefig(pfile)
            matplotlib.pyplot.close(fg)

            # Optimize PNG ...
            pyguymer3.image.optimize_image(pfile, strip = True)

        # **********************************************************************

        # Define PNG file name and check if it exists already ...
        pfile = f"{field}CO.png"
        if not os.path.exists(pfile):
            print(f"Making \"{pfile}\" ...")

            # Create figure ...
            fg = matplotlib.pyplot.figure(figsize = (7.2, 7.2))

            # Create axis ...
            ax = pyguymer3.geo.add_topDown_axis(
                fg,
                -106.25,
                 +39.0,
                dist = 350.0e3,
            )

            # Configure axis ...
            ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")
            pyguymer3.geo.add_coastlines(
                ax,
                colorName = "blue",
                linewidth = 1.0,
            )
            pyguymer3.geo.add_map_background(
                ax,
                    extent = extCO,
                      name = field,
                resolution = field,
            )
            pyguymer3.geo.add_horizontal_gridlines(
                ax,
                locs = range(-90, +91, 1),
            )
            pyguymer3.geo.add_vertical_gridlines(
                ax,
                locs = range(-180, +181, 1),
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
                        ax.plot(
                            record.geometry.x,
                            record.geometry.y,
                            "o",
                                color = "blue",
                            transform = cartopy.crs.PlateCarree(),
                        )
                        pyguymer3.geo.add_annotation(
                            ax,
                            record.geometry.x,
                            record.geometry.y,
                            neName,
                                      colorName = "blue",
                            horizontalalignment = "left",
                                     txtOffsetX = 4,
                                     txtOffsetY = 2,
                        )

            # Configure figure ...
            fg.tight_layout()

            # Save figure ...
            fg.savefig(pfile)
            matplotlib.pyplot.close(fg)

            # Optimize PNG ...
            pyguymer3.image.optimize_image(pfile, strip = True)

        # **********************************************************************

        # Define PNG file name and check if it exists already ...
        pfile = f"{field}US.png"
        if not os.path.exists(pfile):
            print(f"Making \"{pfile}\" ...")

            # Create figure ...
            fg = matplotlib.pyplot.figure(figsize = (7.2, 7.2))

            # Create axis ...
            ax = pyguymer3.geo.add_topDown_axis(
                fg,
                -97.0,
                +40.0,
                dist = 2400.0e3,
            )

            # Configure axis ...
            ax.set_title("Where is ≤ 2,500m ASL but is not accessible?")
            pyguymer3.geo.add_coastlines(
                ax,
                colorName = "blue",
                linewidth = 1.0,
            )
            pyguymer3.geo.add_map_background(
                ax,
                    extent = extUS,
                      name = field,
                resolution = field,
            )
            pyguymer3.geo.add_horizontal_gridlines(
                ax,
                locs = range(-90, +91, 1),
            )
            pyguymer3.geo.add_vertical_gridlines(
                ax,
                locs = range(-180, +181, 1),
            )

            # Configure figure ...
            fg.tight_layout()

            # Save figure ...
            fg.savefig(pfile)
            matplotlib.pyplot.close(fg)

            # Optimize PNG ...
            pyguymer3.image.optimize_image(pfile, strip = True)
