#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import json
    import os
    import pathlib
    import shutil

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy").expanduser(),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import geojson
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "axes.xmargin" : 0.01,
                       "axes.ymargin" : 0.01,
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",                                 # NOTE: See https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html
                     "image.resample" : False,
            }
        )
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Make some plots.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--coastlines-resolution",
        choices = [
            "c",                        # crude
            "l",                        # low
            "i",                        # intermediate
            "h",                        # high
            "f",                        # full
        ],
        default = "f",                  # full
           dest = "coastlinesRes",
           help = "the resolution of the coastlines",
           type = str,
    )
    parser.add_argument(
        "--chunksize",
        default = 1048576,
           help = "the size of the chunks of any files which are read in (in bytes)",
           type = int,
    )
    parser.add_argument(
        "--data-path",
        default = f"{pyguymer3.__path__[0]}/data",
           dest = "dataPath",
           help = "the path to the PyGuymer3 \"data\" folder",
           type = str,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--eps",
        default = 1.0e-12,
           dest = "eps",
           help = "the tolerance of the Vincenty formula iterations",
           type = float,
    )
    parser.add_argument(
        "--exiftool-path",
        default = shutil.which("exiftool"),
           dest = "exiftoolPath",
           help = "the path to the \"exiftool\" binary",
           type = str,
    )
    parser.add_argument(
        "--gifsicle-path",
        default = shutil.which("gifsicle"),
           dest = "gifsiclePath",
           help = "the path to the \"gifsicle\" binary",
           type = str,
    )
    parser.add_argument(
        "--GLOBE-scale",
        default = "32km",
           dest = "globeScale",
           help = "the scale of the GeoJSON files of elevation derived from the \"GLOBE\" dataset",
           type = str,
    )
    parser.add_argument(
        "--jpegtran-path",
        default = shutil.which("jpegtran"),
           dest = "jpegtranPath",
           help = "the path to the \"jpegtran\" binary",
           type = str,
    )
    parser.add_argument(
        "--nAng",
        default = 361,
           dest = "nAng",
           help = "the number of angles around each circle",
           type = int,
    )
    parser.add_argument(
        "--NE-resolution",
        choices = [
             "10m",
             "50m",
            "110m",
        ],
        default = "10m",
           dest = "neRes",
           help = "the resolution of the \"NE\" dataset",
           type = str,
    )
    parser.add_argument(
        "--nIter",
        default = 1000000,
           dest = "nIter",
           help = "the maximum number of iterations (particularly the Vincenty formula)",
           type = int,
    )
    parser.add_argument(
        "--optipng-path",
        default = shutil.which("optipng"),
           dest = "optipngPath",
           help = "the path to the \"optipng\" binary",
           type = str,
    )
    parser.add_argument(
        "--RAM-limit",
        default = 1073741824,
           dest = "ramLimit",
           help = "the maximum RAM usage of each \"large\" array (in bytes)",
           type = int,
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    parser.add_argument(
        "--tolerance",
        default = 1.0e-10,
           dest = "tol",
           help = "the Euclidean distance that defines two points as being the same (in degrees)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    print("Loading \"data/globe.json\" ...")

    # Load metadata dictionary from JSON ...
    with open("data/globe.json", mode = "rt", encoding = "utf-8") as fObj:
        meta = json.load(fObj)

    # **************************************************************************

    # Loop over regions ...
    for name, midLon, midLat, maxDist in [
        ("Chile"       ,  -68.29554064832485, -23.392273677187635, 1000.0e3),   # NOTE: "midLon" and "midLat" are the ".centroid" of the inaccessible Polygon of interest for "scale=01km".
        ("Colorado"    , -106.59078388813973, +39.045973661657364,  300.0e3),   # NOTE: "midLon" and "midLat" are the ".centroid" of the 2,500 m elevation Polygon of interest for "scale=32km".
        ("UnitedStates",  -97.763           , +39.517            , 2392.7e3),   # NOTE: "midLon", "midLat" and "maxDist" are from FMC.
    ]:
        print(f"Processing \"{name}\" ...")

        # Loop over scales ...
        for scale, npx in meta["scales"].items():
            # Create short-hand and skip if this plot already exists ...
            pName = f"output/tileScale=32km/{scale}/{name}.png"
            if os.path.exists(pName):
                continue

            print(f"  Making \"{pName}\" ...")

            # Define maps's field-of-view ...
            fov = pyguymer3.geo.buffer(
                shapely.geometry.point.Point(midLon, midLat),
                maxDist,
                   debug = args.debug,
                     eps = args.eps,
                    fill = -1.0,
                    nAng = args.nAng,
                   nIter = args.nIter,
                ramLimit = args.ramLimit,
                    simp = -1.0,
                     tol = args.tol,
            )

            # Create figure ...
            fg = matplotlib.pyplot.figure(figsize = (7.2, 7.2))

            # Create axis ...
            ax = pyguymer3.geo.add_axis(
                fg,
                       add_background = True,
                       add_coastlines = True,
                        add_gridlines = True,
                 coastlines_edgecolor = "none",
                 coastlines_facecolor = "darkkhaki",
                coastlines_resolution = args.coastlinesRes,
                    coastlines_zorder = 1.5,
                                debug = args.debug,
                                 dist = maxDist,
                                  eps = args.eps,
                                  fov = fov,
                     gridlines_zorder = 2.0,
                                  lat = midLat,
                                  lon = midLon,
                                nIter = args.nIter,
                            onlyValid = True,
                             ramLimit = args.ramLimit,
                               repair = False,
                                  tol = args.tol,
            )

            # ******************************************************************

            print(f"    Loading \"{args.dataPath}/geojson/globe/scale={args.globeScale}/elev=2500m.geojson\" ...")

            # Load GeometryCollection from GeoJSON ...
            with open(f"{args.dataPath}/geojson/globe/scale={args.globeScale}/elev=2500m.geojson", mode = "rt", encoding = "utf-8") as fObj:
                geoms = shapely.geometry.shape(
                    geojson.load(fObj)
                )

            # Create a list of Polygons to plot (taking in to account the
            # field-of-view to clip them by) ...
            polys = []
            for poly in pyguymer3.geo.extract_polys(
                geoms,
                onlyValid = True,
                   repair = False,
            ):
                if poly.disjoint(fov):
                    continue
                polys.append(poly.intersection(fov))

            # Plot Polygons ...
            ax.add_geometries(
                polys,
                cartopy.crs.PlateCarree(),
                edgecolor = "none",
                facecolor = "lightgrey",
                   zorder = 1.6,
            )

            # ******************************************************************

            print(f"    Loading \"output/tileScale=32km/{scale}/inaccessible.geojson\" ...")

            # Load GeometryCollection from GeoJSON ...
            with open(f"output/tileScale=32km/{scale}/inaccessible.geojson", mode = "rt", encoding = "utf-8") as fObj:
                geoms = shapely.geometry.shape(
                    geojson.load(fObj)
                )

            # Create a list of Polygons to plot (taking in to account the
            # field-of-view to clip them by) ...
            polys = []
            for poly in pyguymer3.geo.extract_polys(
                geoms,
                onlyValid = True,
                   repair = False,
            ):
                if poly.disjoint(fov):
                    continue
                polys.append(poly.intersection(fov))

            # Plot Polygons ...
            ax.add_geometries(
                polys,
                cartopy.crs.PlateCarree(),
                edgecolor = "none",
                facecolor = "red",
                   zorder = 1.7,
            )

            # ******************************************************************

            # Check if this plot is of Colorado ...
            if name == "Colorado":
                # Find file containing all the populated places shapes ...
                sName = cartopy.io.shapereader.natural_earth(
                      category = "cultural",
                          name = "populated_places",
                    resolution = args.neRes,
                )

                print(f"    Loading \"{sName}\" ...")

                # Loop over records ...
                for record in cartopy.io.shapereader.Reader(sName).records():
                    # Create short-hands ...
                    neAdmin0Name = pyguymer3.geo.getRecordAttribute(record, "ADM0NAME")
                    neAdmin1Name = pyguymer3.geo.getRecordAttribute(record, "ADM1NAME")
                    neName = pyguymer3.geo.getRecordAttribute(record, "NAME")

                    # Check that populated place is in the United States of
                    # America ...
                    if neAdmin0Name == "United States of America":
                        # Check that populated place is in Colorado ...
                        if neAdmin1Name == "Colorado":
                            # Check that the populated place is within the
                            # field-of-view ...
                            if fov.contains(record.geometry):
                                # Annotate the plot ...
                                ax.plot(
                                    record.geometry.x,
                                    record.geometry.y,
                                    "*",
                                        color = "black",
                                    transform = cartopy.crs.PlateCarree(),
                                       zorder = 1.8,
                                )
                                pyguymer3.geo.add_annotation(
                                    ax,
                                    record.geometry.x,
                                    record.geometry.y,
                                    neName,
                                                  color = "black",
                                                  debug = args.debug,
                                    horizontalalignment = "left",
                                             txtOffsetX = 4,
                                             txtOffsetY = 2,
                                      verticalalignment = "center",
                                                 zorder = 1.8,
                                )

            # ******************************************************************

            # Configure axis ...
            ax.set_title(f"Where is ≤ 2,500 m ASL but is not accessible at ({midLon:+.3f}°, {midLat:+.3f}°) ± {0.001 * maxDist:,.1f} km?")

            # Configure figure ...
            fg.tight_layout()

            # Save figure ...
            fg.savefig(pName)
            matplotlib.pyplot.close(fg)

            # Optimize PNG ...
            pyguymer3.image.optimise_image(
                pName,
                   chunksize = args.chunksize,
                       debug = args.debug,
                exiftoolPath = args.exiftoolPath,
                gifsiclePath = args.gifsiclePath,
                jpegtranPath = args.jpegtranPath,
                 optipngPath = args.optipngPath,
                       strip = True,
                     timeout = args.timeout,
            )
