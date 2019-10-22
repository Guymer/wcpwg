#!/usr/bin/env python3

# Import standard modules ...
import math

# Import special modules ...
try:
    import cartopy
    import cartopy.crs
except:
    raise Exception("run \"pip install --user cartopy\"")

# Find file containing all the country shapes ...
shape_file = cartopy.io.shapereader.natural_earth(
    resolution = "110m",
      category = "cultural",
          name = "admin_0_countries"
)

# Loop over records ...
for record in cartopy.io.shapereader.Reader(shape_file).records():
    # Check if this country is one that I want ...
    if record.attributes["NAME"] in ["Chile", "United States of America"]:
        print("{:s}:".format(record.attributes["NAME"]))
        for geometry in record.geometry:
            x1, y1, x2, y2 = geometry.bounds
            print(
                "    {:+6.1f} <= x <= {:+6.1f}    {:+6.1f} <= y <= {:+6.1f}".format(
                    math.floor(x1),
                    math.ceil(x2),
                    math.floor(y1),
                    math.ceil(y2)
                )
            )
