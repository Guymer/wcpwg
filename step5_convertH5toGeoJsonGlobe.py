#!/usr/bin/env python3

"""Convert "GLOBE" dataset H5 files to GeoJSON files"""

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import json
    import os

    # Import special modules ...
    try:
        import h5py
    except:
        raise Exception("\"h5py\" is not installed; run \"pip install --user h5py\"") from None
    try:
        import geojson
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Convert \"GLOBE\" dataset H5 files to GeoJSON files.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--indiv",
        action = "store_true",
          help = "save each Polygon individually too",
    )
    args = parser.parse_args()

    # **************************************************************************

    # Create short-hands ...
    geojsonSizeMax = 1024.0                                                     # [Mpx]
    nx = 43200                                                                  # [px]
    ny = 21600                                                                  # [px]
    res = 1                                                                     # [km]

    # Make a dictionary of metadata ...
    meta = {
            "nx" : nx,
            "ny" : ny,
           "res" : res,
        "scales" : {},
    }

    # Loop over possible shrink scales ...
    for iShrinkScale in range(11):
        # Determine shrink scale ...
        shrinkScale = pow(2, iShrinkScale)

        # Skip this shrink scale if it is not an integer division of both axes
        # of the array ...
        if nx % shrinkScale != 0:
            continue
        if ny % shrinkScale != 0:
            continue

        # Create short-hands ...
        nxScaled = nx // shrinkScale                                            # [px]
        nyScaled = ny // shrinkScale                                            # [px]
        scaleStub = f"scale={res * shrinkScale:02d}km"

        # Populate dictionary of metadata ...
        meta["scales"][scaleStub] = nxScaled * nyScaled                         # [px]

        print(f"Processing scale \"{scaleStub}\" ...")

        # Find out how many mega-pixels there are at this shrink scale ...
        mega = float(nxScaled * nyScaled) / 1.0e6                               # [Mpx]

        # Skip this shrink scale if the output would be too big ...
        if mega > geojsonSizeMax:
            print(f"  Skipping (the HDF input is {mega:5.1f} Mpx).")
            continue

        # Loop over HDF5 files backwards ...
        for hName in sorted(glob.glob(f"output/tileScale=??km/{scaleStub}/inaccessible.h5")):
            # Deduce GeoJSON name and skip this HDF5 file if it already exists ...
            jName = f'{hName.removesuffix(".h5")}.geojson'
            if os.path.exists(jName):
                # print(f"  Skipping \"{jName}\" (it already exists).")
                continue

            print(f"  Making \"{jName}\" ...")

            # ******************************************************************

            # Initialize list ...
            polys = []

            print(f"    Loading \"{hName}\" ...")

            # Open HDF5 file ...
            with h5py.File(hName, "r") as hObj:
                # Loop over rings ...
                for iRing in range(hObj.attrs["nRings"]):
                    # Create short-hand ...
                    key = f"ring={iRing:06d}"

                    # Extract a dirty copy of the data ...
                    dirtyLons = hObj[key]["lons"][:]                            # [°]
                    dirtyLats = hObj[key]["lats"][:]                            # [°]

                    # Create a clean list of the coordinates ...
                    # NOTE: This does two things:
                    #         * it does not include duplicate points (see
                    #           explanation below); and
                    #         * it retroactively removes points which are not
                    #           required because the line has not changed
                    #           direction.
                    # NOTE: Not including duplicate points is required as some
                    #       of the Polygons touch themselves. According to the
                    #       Shapely documentation, a Polygon can only touch
                    #       itself once. However, the first coordinate should
                    #       always get a free pass (but only one free pass mind
                    #       you) as the loop is always closed and so the first
                    #       coordinate will always be duplicated (i.e., at the
                    #       other end). See:
                    #         * https://shapely.readthedocs.io/en/stable/manual.html#polygons
                    coords = [
                        (dirtyLons[0], dirtyLats[0]),
                    ]                                                           # [°], [°]
                    for iCoord in range(1, dirtyLats.size - 1):
                        if ((dirtyLats == dirtyLats[iCoord]) * (dirtyLons == dirtyLons[iCoord])).sum() == 1:
                            if len(coords) >= 2:
                                if (coords[-2][0] == coords[-1][0] == dirtyLons[iCoord]) or (coords[-2][1] == coords[-1][1] == dirtyLats[iCoord]):
                                    coords = coords[:-1]                        # [°], [°]
                            coords.append((dirtyLons[iCoord], dirtyLats[iCoord]))   # [°], [°]
                    coords.append(coords[0])                                    # [°], [°]

                    # Create a Polygon from the list of coordinates ...
                    poly = shapely.geometry.polygon.Polygon(coords)
                    pyguymer3.geo.check(poly)

                    # Append Polygon to list ...
                    polys.append(poly)

            print(f"    There are {len(polys):,d} polys.")

            # Skip this collection if there aren't any Polygons ...
            if len(polys) == 0:
                print("    Skipping (there aren't any Polygons).")
                continue

            # Sort list of Polygons by area (from largest to smallest) ...
            # NOTE: This will hopefully stop peaks inside rings being identified
            #       as dips.
            polys = sorted(
                polys,
                    key = lambda poly: poly.area,
                reverse = True,
            )

            # ******************************************************************

            # Initialize progress ...
            progress = 0.0                                                      # [%]

            print(f"    Correcting holes ... {progress:7.3f}%", end = "\r")

            # Start infinite loop ...
            while True:
                # Initialize flag ...
                foundHole = False

                # Loop over Polygons ...
                for iOuter, outerPoly in enumerate(polys):
                    # Loop over Polygons ...
                    for iInner, innerPoly in enumerate(polys):
                        # Skip this inner Polygon if it is the same one as in
                        # the outer loop ...
                        if iInner == iOuter:
                            continue

                        # Skip this inner Polygon if it is not contained in the
                        # outer Polygon ...
                        if not outerPoly.contains(innerPoly):
                            continue

                        # Set flag and progress ...
                        foundHole = True
                        progress = 100.0 * float(iOuter) / float(len(polys) - 1)    # [%]

                        # Make a new Polygon containing this inner Polygon as a
                        # new hole in the outer Polygon ...
                        exterior = outerPoly.exterior
                        interiors = []
                        for interior in outerPoly.interiors:
                            interiors.append(interior)
                        interiors.append(innerPoly.exterior)
                        poly = shapely.geometry.polygon.Polygon(exterior, holes = interiors)
                        pyguymer3.geo.check(poly)

                        # Overwrite the outer Polygon with this new Polygon ...
                        polys[iOuter] = poly

                        # Remove this inner Polygon from the list ...
                        del polys[iInner]

                        # Stop looping ...
                        break

                    # Check if a new hole was found ...
                    if foundHole:
                        # Stop looping ...
                        break

                # Check if a new hole was not found ...
                if not foundHole:
                    # Stop looping ...
                    break

                print(f"    Correcting holes ... {progress:7.3f}%", end = "\r")

            # Clear the line ...
            print()

            print(f"    There are {len(polys):,d} polys.")

            # ******************************************************************

            print("    Saving GeoJSON ...")

            # Make a GeometryCollection of these Polygons ...
            # NOTE: Some of these Polygons may touch each other. According to
            #       the Shapely documentation, a MultiPolygon can only touch
            #       itself once. Therefore, I make a GeometryCollection instead.
            #       See:
            #         * https://shapely.readthedocs.io/en/stable/manual.html#collections
            #         * https://shapely.readthedocs.io/en/stable/manual.html#collections-of-polygons
            coll = shapely.geometry.collection.GeometryCollection(polys)

            # Save GeometryCollection as a GeoJSON ...
            # NOTE: As of 4/Aug/2025, the Python module "geojson" just converts
            #       the object to a Python dictionary and then it just calls the
            #       standard "json.dump()" function to format the Python
            #       dictionary as text. There is no way to specify the precision
            #       of the written string. Fortunately, if you have no shame,
            #       then you can load and then dump the string again, see:
            #         * https://stackoverflow.com/a/29066406
            with open(jName, "wt", encoding = "utf-8") as fObj:
                json.dump(
                    json.loads(
                        geojson.dumps(
                            coll,
                            ensure_ascii = False,
                                  indent = 4,
                               sort_keys = True,
                        ),
                        parse_float = lambda x: round(float(x), 4),             # NOTE: 0.0001° is approximately 11.1 m.
                    ),
                    fObj,
                    ensure_ascii = False,
                          indent = 4,
                       sort_keys = True,
                )

            # ******************************************************************

            # Check if the user wants to save each Polygon individually ...
            if args.indiv:
                # Loop over Polygons ...
                for iPoly, poly in enumerate(polys):
                    # Deduce GeoJSON name ...
                    jName = f'{hName.removesuffix(".h5")}.poly={iPoly:06d}.area={poly.area:012.6f}.geojson'

                    print(f"    Making \"{jName}\" ...")

                    # Save Polygon as a GeoJSON ...
                    # NOTE: As of 4/Aug/2025, the Python module "geojson" just
                    #       converts the object to a Python dictionary and then
                    #       it just calls the standard "json.dump()" function to
                    #       format the Python dictionary as text. There is no
                    #       way to specify the precision of the written string.
                    #       Fortunately, if you have no shame, then you can load
                    #       and then dump the string again, see:
                    #         * https://stackoverflow.com/a/29066406
                    with open(jName, "wt", encoding = "utf-8") as fObj:
                        json.dump(
                            json.loads(
                                geojson.dumps(
                                    poly,
                                    ensure_ascii = False,
                                          indent = 4,
                                       sort_keys = True,
                                ),
                                parse_float = lambda x: round(float(x), 4),     # NOTE: 0.0001° is approximately 11.1 m.
                            ),
                            fObj,
                            ensure_ascii = False,
                                  indent = 4,
                               sort_keys = True,
                        )

    # **************************************************************************

    print("Making \"data/globe.json\" ...")

    # Save metadata dictionary as a JSON ...
    with open("data/globe.json", "wt", encoding = "utf-8") as fObj:
        json.dump(
            meta,
            fObj,
            ensure_ascii = False,
                  indent = 4,
               sort_keys = True,
        )
