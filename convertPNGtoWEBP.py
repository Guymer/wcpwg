#!/usr/bin/env python3

# Import standard modules ...
import glob

# Import special modules ...
try:
    import PIL
    import PIL.Image
except:
    raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

# Import my modules ...
try:
    import pyguymer3
    import pyguymer3.media
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

# Configure PIL to open images up to 1 GiP ...
PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                                 # [px]

# ******************************************************************************

print("Making \"createMask3.webp\" ...")

# Initialize list ...
images = []

# Loop over frames ...
for frame in sorted(glob.glob("createMask3_mask????.png")):
    # Open image as RGB (even if it is paletted) ...
    with PIL.Image.open(frame) as iObj:
        image = iObj.convert("RGB")

    # Append it to the list ...
    images.append(image)

# Save 25fps WEBP ...
pyguymer3.media.images2webp(
    images,
    "createMask3.webp",
    strip = True,
)

# Clean up ...
del images

# ******************************************************************************

# Set widths ...
# NOTE: By inspection, the PNG frames are 2160px wide.
widths = [256, 512, 1024, 2048]                                                 # [px]

# Loop over widths ...
for width in widths:
    print(f"Making \"createMask3{width:04d}px.webp\" ...")

    # Initialize list ...
    images = []

    # Loop over frames ...
    for frame in sorted(glob.glob("createMask3_mask????.png")):
        # Open image as RGB (even if it is paletted) ...
        with PIL.Image.open(frame) as iObj:
            image = iObj.convert("RGB")

        # Calculate height ...
        ratio = float(image.width) / float(image.height)                        # [px/px]
        height = round(float(width) / ratio)                                    # [px]

        # Downscale the image and append it to the list ...
        images.append(image.resize((width, height), resample = PIL.Image.Resampling.LANCZOS))

    # Save 25fps WEBP ...
    pyguymer3.media.images2webp(
        images,
        f"createMask3{width:04d}px.webp",
        strip = True,
    )

    # Clean up ...
    del images
