#!/usr/bin/env python3

# Import standard modules ...
import glob

# Import special modules ...
try:
    import PIL
    import PIL.Image
except:
    raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

# Configure PIL to open images up to 1 GiP ...
PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                                 # [px]

# ******************************************************************************

print("Making \"createMask3.webp\" ...")

# Initialize list ...
images = []

# Loop over frames ...
for frame in sorted(glob.glob("createMask3_mask????.png")):
    # Open image as RGB (even if it is paletted) ...
    image = PIL.Image.open(frame).convert("RGB")

    # Append it to the list ...
    images.append(image)

# Save 25fps WEBP ...
images[0].save("createMask3.webp", lossless = True, quality = 100, method = 6, save_all = True, append_images = images[1:], duration = 40, loop = 0, minimize_size = True)

# Clean up ...
for image in images:
    image.close()
del images
