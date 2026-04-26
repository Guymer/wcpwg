#!/usr/bin/env python3

"""Convert PGM images to PNG images"""

# Define function ...
def pgm2png(
    pgm,
    /,
    *,
             debug = __debug__,
    maxImagePixels = 1073741824,
):
    """Convert a PGM image to a PNG image (removing the PGM image afterwards)

    Parameters
    ----------
    pgm : str
        the path to the input PGM image
    debug : bool, optional
        print debug messages
    maxImagePixels : int, optional
        the maximum number of pixels in an image, to prevent decompression bombs

    Notes
    -----
    Copyright 2026 Thomas Guymer [1]_

    References
    ----------
    .. [1] Guymer, https://codeberg.org/guymer
    """

    # **************************************************************************

    # Import standard modules ...
    import os

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = maxImagePixels                             # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create short-hand ...
    png = f'{pgm.removesuffix(".pgm")}.png'

    print(f"Converting \"{pgm}\" to \"{png}\" ...")

    # Open PGM and convert to NumPy array ...
    with PIL.Image.open(pgm) as iObj:
        img = numpy.array(iObj).reshape((iObj.height, iObj.width, 1))

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
            palUint8 = None,
          strategies = None,
              wbitss = [15,],
    )
    with open(png, "wb") as fObj:
        fObj.write(src)

    # Remove PGM ...
    os.remove(pgm)

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
            description = "Convert PGM images to PNG images.",
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

    # Create a pool of workers ...
    with multiprocessing.Pool(args.nChild) as pObj:
        # Initialize list ...
        results = []

        # Loop over PGMs backwards ...
        for fName in sorted(glob.glob("output/tileScale=??km/scale=??km/inaccessible.pgm"))[::-1]:
            # Add job to convert the PGM to the worker pool ...
            results.append(
                pObj.apply_async(
                    pgm2png,
                    (
                        fName,
                    ),
                    {
                        "debug" : args.debug,
                    },
                )
            )

        print("Waiting for child \"multiprocessing\" processes to finish ...")

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
