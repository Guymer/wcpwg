#!/usr/bin/env python3

"""Convert BIN arrays to PNG images"""

# Define function ...
def bin2png(
    fNameIn,
    nxScaledIn,
    nyScaledIn,
    /,
    *,
    debug = __debug__,
):
    """Convert a BIN array to a PNG image

    Parameters
    ----------
    fNameIn : str
        the path to the input BIN array
    nxScaledIn : int
        the number of pixels in the x-axis
    nyScaledIn : int
        the number of pixels in the y-axis
    debug : bool, optional
        print debug messages

    Notes
    -----
    Copyright 2026 Thomas Guymer [1]_

    References
    ----------
    .. [1] Guymer, https://codeberg.org/guymer
    """

    # **************************************************************************

    # Import standard modules ...
    import json
    import os

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

    # Create short-hand and return if the PNG already exists ...
    png = f'{fNameIn.removesuffix(".bin")}.png'
    if os.path.exists(png):
        return

    print(f"  Converting \"{fNameIn}\" to \"{png}\" ...")

    # Load colour tables and create short-hand ...
    with open(f"{pyguymer3.__path__[0]}/data/json/colourTables.json", "rt", encoding = "utf-8") as fObj:
        colourTables = json.load(fObj)
    turbo = numpy.array(colourTables["turbo"]).astype(numpy.uint8)

    # Open BIN and scale from 0 to 255 ...
    img = (
        255.0 * numpy.fromfile(
            fNameIn,
            dtype = numpy.float32,
        ).reshape(nyScaledIn, nxScaledIn, 1)
    ).astype(numpy.uint8)

    # Save NumPy array as a PNG ...
    src = pyguymer3.image.makePng(
        img,
        calcAdaptive = True,
         calcAverage = True,
            calcNone = True,
           calcPaeth = True,
             calcSub = True,
              calcUp = True,
             choices = "all",
               debug = debug,
                 dpi = None,
              levels = [9,],
           memLevels = [9,],
             modTime = None,
            palUint8 = turbo,
          strategies = None,
              wbitss = [15,],
    )
    with open(png, "wb") as fObj:
        fObj.write(src)

# ******************************************************************************

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import multiprocessing
    import os

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Convert BIN arrays to PNG images.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--number-of-children",
        default = os.cpu_count() - 1,   # TODO: Once I ditch Python 3.11 and
                                        #       Python 3.12 then I can use
                                        #       "os.process_cpu_count()" instead.
           dest = "nChild",
           help = "the number of child \"multiprocessing\" processes to use when converting the images",
           type = int,
    )
    parser.add_argument(
        "--timeout",
        default = 21600.0,              # NOTE: Would normally be "60.0".
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Create short-hands ...
    nx = 43200                                                                  # [px]
    ny = 21600                                                                  # [px]
    res = 1                                                                     # [km]

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

        print(f"Processing scale \"{scaleStub}\" ...")

        # Create a pool of workers ...
        with multiprocessing.Pool(args.nChild) as pObj:
            # Initialize list ...
            results = []

            # Loop over BINs ...
            for fName in sorted(
                glob.glob(f"output/tileScale=??km/{scaleStub}/accessible.bin")
                + glob.glob(f"output/tileScale=??km/{scaleStub}/below2500m.bin")
                + glob.glob(f"output/tileScale=??km/{scaleStub}/inaccessible.bin")
            ):
                # Add job to convert the BIN to the worker pool ...
                results.append(
                    pObj.apply_async(
                        bin2png,
                        (
                            fName,
                            nxScaled,
                            nyScaled,
                        ),
                        {
                            "debug" : args.debug,
                        },
                    )
                )

            print("  Waiting for child \"multiprocessing\" processes to finish ...")

            # Loop over results ...
            for result in results:
                # Get result ...
                _ = result.get(args.timeout)

                # Check result ...
                if not result.successful():
                    # Cry ...
                    raise Exception("\"multiprocessing.Pool().apply_async()\" was not successful") from None

            # Close the pool of worker processes and wait for all of the tasks to
            # finish ...
            # NOTE: The "__exit__()" call of the context manager for
            #       "multiprocessing.Pool()" calls "terminate()" instead of
            #       "join()", so I must manage the end of the pool of worker
            #       processes myself.
            pObj.close()
            pObj.join()
