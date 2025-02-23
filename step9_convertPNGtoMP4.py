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
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Find the frames ...
    frames = sorted(glob.glob("createMask3output/mask????.png"))

    # **************************************************************************

    print("Making \"createMask3.mp4\" ...")

    # Save 25fps MP4 ...
    vname = pyguymer3.media.images2mp4(
        frames,
    )
    shutil.move(vname, "createMask3.mp4")

    # **************************************************************************

    # Set maximum sizes ...
    # NOTE: By inspection, the PNG frames are 2,160 px wide.
    maxSizes = [512, 1024, 2048]                                                # [px]

    # Loop over maximum sizes ...
    for maxSize in maxSizes:
        print(f"Making \"createMask3{maxSize:04d}px.mp4\" ...")

        # Save 25fps MP4 ...
        vname = pyguymer3.media.images2mp4(
            frames,
            screenHeight = maxSize,
             screenWidth = maxSize,
        )
        shutil.move(vname, f"createMask3{maxSize:04d}px.mp4")
