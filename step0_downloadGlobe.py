#!/usr/bin/env python3

"""Download the "GLOBE" dataset"""

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import os

    # Import my modules ...
    try:
        import pyguymer3
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Download the \"GLOBE\" dataset.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--timeout",
        default = 3600.0,               # NOTE: Would normally be "60.0".
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    parser.add_argument(
        "--url",
        default = "https://www.ngdc.noaa.gov/mgg/topo/DATATILES/elev/all10g.zip",
           help = "the URL to the \"GLOBE\" dataset",
           type = str,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Check if the output folder does not exist yet ...
    if not os.path.exists("data"):
        # Make output folder ...
        os.mkdir("data")

    # Check if the ZIP file does not exist yet ...
    if not os.path.exists("data/globe.zip"):
        print("Downloading \"data/globe.zip\" ...")

        # Start session ...
        with pyguymer3.start_session() as sess:
            # Download the ZIP file ...
            if not pyguymer3.download_file(
                sess,
                args.url,
                "data/globe.zip",
                  debug = args.debug,
                timeout = args.timeout,
                 verify = True,
            ):
                raise Exception(f"failed to download \"{args.url}\"") from None
