#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import glob
    import shutil

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.media
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    # **************************************************************************

    print("Making \"createMask3.mp4\" ...")

    # Set list ...
    frames = sorted(glob.glob("createMask3_mask????.png"))

    # Save 25fps MP4 ...
    vname = pyguymer3.media.images2mp4(frames)
    shutil.move(vname, "createMask3.mp4")

    # **************************************************************************

    # Set widths ...
    # NOTE: By inspection, the PNG frames are 2,160 px wide.
    widths = [512, 1024, 2048]                                                  # [px]

    # Loop over widths ...
    for width in widths:
        print(f"Making \"createMask3{width:04d}px.mp4\" ...")

        # Set list ...
        frames = sorted(glob.glob("createMask3_mask????.png"))

        # Save 25fps MP4 ...
        vname = pyguymer3.media.images2mp4(frames, screenWidth = width, screenHeight = width)
        shutil.move(vname, f"createMask3{width:04d}px.mp4")
