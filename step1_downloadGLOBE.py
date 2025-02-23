#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import os

    # Import my modules ...
    try:
        import pyguymer3
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Check if the ZIP file does not exist yet ...
    if not os.path.exists("all10g.zip"):
        print("Downloading \"all10g.zip\" ...")

        # Start session ...
        with pyguymer3.start_session() as sess:
            # Download the ZIP file ...
            if not pyguymer3.download_file(sess, "https://www.ngdc.noaa.gov/mgg/topo/DATATILES/elev/all10g.zip", "all10g.zip"):
                raise Exception("download failed", "https://www.ngdc.noaa.gov/mgg/topo/DATATILES/elev/all10g.zip") from None
