#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import json

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Convert BIN files to PNG images.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--maximum-size",
        default = 250.0,
           dest = "maxSize",
           help = "the maximum size of image to make a PNG for (in mega-pixels)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Load colour tables and create short-hand ...
    with open(f"{pyguymer3.__path__[0]}/data/json/colourTables.json", "rt", encoding = "utf-8") as fObj:
        colourTables = json.load(fObj)
    r2o2g = numpy.array(colourTables["r2o2g"]).astype(numpy.uint8)
    turbo = numpy.array(colourTables["turbo"]).astype(numpy.uint8)

    # Define the size of the dataset ...
    nx = 43200                                                                  # [px]
    ny = 21600                                                                  # [px]

    # Find files ...
    fNames = sorted(glob.glob("compareMasksOutput/*.*") + glob.glob("createMask?output/*.*"))

    # Loop over files ...
    for bName in fNames:
        # Skip this file if it is not a BIN file ...
        if not bName.endswith(".bin"):
            continue

        # Create short-hand and skip this BIN file if the associated PNG file
        # already exists ...
        pName = f'{bName.removesuffix(".bin")}.png'
        if pName in fNames:
            print(f"Skipping \"{bName}\" (the PNG already exists).")
            continue

        # Figure out what to do with it ...
        match bName:
            case "compareMasksOutput/diff_scale=01km.bin":
                print(f"Skipping \"{bName}\" (it contains FORTRAN \"LOGICAL kind\" data).")
                continue
            case "compareMasksOutput/diff_scale=02km.bin" | "compareMasksOutput/diff_scale=04km.bin" | "compareMasksOutput/diff_scale=08km.bin" | "compareMasksOutput/diff_scale=16km.bin" | "compareMasksOutput/diff_scale=32km.bin":
                # Find scale ...
                scale = int(bName.split("=")[1][:2])

                # Find out how many mega-pixels there are and skip this BIN if
                # the PNG would be too big ...
                mega = float((nx // scale) * (ny // scale)) / 1.0e6             # [Mpx]
                if mega > args.maxSize:
                    print(f"Skipping \"{bName}\" (the PNG would be {mega:,.1f} Mpx).")
                    continue

                print(f"Making \"{pName}\" ...")

                # Load data ...
                data = numpy.fromfile(
                    bName,
                    dtype = numpy.float32,
                ).reshape(ny // scale, nx // scale, 1)

                # Scale data from 0 to 255 ...
                data *= 255.0
                numpy.place(data, data <   0.0,   0.0)
                numpy.place(data, data > 255.0, 255.0)
                data = data.astype(numpy.uint8)

                # Make PNG ...
                src = pyguymer3.image.makePng(
                    data,
                    calcAdaptive = True,
                     calcAverage = True,
                        calcNone = True,
                       calcPaeth = True,
                         calcSub = True,
                          calcUp = True,
                         choices = "all",
                           debug = args.debug,
                             dpi = None,
                          levels = [9,],
                       memLevels = [9,],
                         modTime = None,
                        palUint8 = turbo,
                      strategies = None,
                          wbitss = [15,],
                )
                with open(pName, "wb") as fObj:
                    fObj.write(src)
            case "compareMasksOutput/flags_scale=01km.bin":
                # Find out how many mega-pixels there are and skip this BIN if
                # the PNG would be too big ...
                mega = float(nx * ny) / 1.0e6                                   # [Mpx]
                if mega > args.maxSize:
                    print(f"Skipping \"{bName}\" (the PNG would be {mega:,.1f} Mpx).")
                    continue

                print(f"Making \"{pName}\" ...")

                # Load data ...
                data = numpy.fromfile(
                    bName,
                    dtype = numpy.int8,
                ).astype(numpy.uint8).reshape(ny, nx, 1)

                # Make PNG ...
                src = pyguymer3.image.makePng(
                    data,
                    calcAdaptive = True,
                     calcAverage = True,
                        calcNone = True,
                       calcPaeth = True,
                         calcSub = True,
                          calcUp = True,
                         choices = "all",
                           debug = args.debug,
                             dpi = None,
                          levels = [9,],
                       memLevels = [9,],
                         modTime = None,
                        palUint8 = numpy.array(
                        [
                            [255,   0,   0],
                            [255, 127,   0],
                            [  0, 255,   0],
                        ],
                        dtype = numpy.uint8,
                    ),
                      strategies = None,
                          wbitss = [15,],
                )
                with open(pName, "wb") as fObj:
                    fObj.write(src)
            case "compareMasksOutput/flags_scale=02km.bin" | "compareMasksOutput/flags_scale=04km.bin" | "compareMasksOutput/flags_scale=08km.bin" | "compareMasksOutput/flags_scale=16km.bin" | "compareMasksOutput/flags_scale=32km.bin":
                # Find scale ...
                scale = int(bName.split("=")[1][:2])

                # Find out how many mega-pixels there are and skip this BIN if
                # the PNG would be too big ...
                mega = float((nx // scale) * (ny // scale)) / 1.0e6             # [Mpx]
                if mega > args.maxSize:
                    print(f"Skipping \"{bName}\" (the PNG would be {mega:,.1f} Mpx).")
                    continue

                print(f"Making \"{pName}\" ...")

                # Load data ...
                data = numpy.fromfile(
                    bName,
                    dtype = numpy.float32,
                ).reshape(ny // scale, nx // scale, 1)

                # Scale data from 0 to 255, mapping from 0 to 2 ...
                data = 255.0 * (data / 2.0)
                numpy.place(data, data <   0.0,   0.0)
                numpy.place(data, data > 255.0, 255.0)
                data = data.astype(numpy.uint8)

                # Make PNG ...
                src = pyguymer3.image.makePng(
                    data,
                    calcAdaptive = True,
                     calcAverage = True,
                        calcNone = True,
                       calcPaeth = True,
                         calcSub = True,
                          calcUp = True,
                         choices = "all",
                           debug = args.debug,
                             dpi = None,
                          levels = [9,],
                       memLevels = [9,],
                         modTime = None,
                        palUint8 = r2o2g,
                      strategies = None,
                          wbitss = [15,],
                )
                with open(pName, "wb") as fObj:
                    fObj.write(src)
            case _:
                raise ValueError(f"there is no case which matches \"{bName}\"") from None
