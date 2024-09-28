#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import math
    import os

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : os.path.expanduser("~/.local/share/cartopy_cache"),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    # **************************************************************************

    # Find file containing all the country shapes ...
    sfile = cartopy.io.shapereader.natural_earth(
          category = "cultural",
              name = "admin_0_countries",
        resolution = "110m",
    )

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(sfile).records():
        # Create short-hand ...
        neName = pyguymer3.geo.getRecordAttribute(record, "NAME")

        # Check if this country is one that I want ...
        if neName in ["Chile", "United States of America"]:
            print(f"{neName}:")
            for poly in pyguymer3.geo.extract_polys(record.geometry):
                x1, y1, x2, y2 = poly.bounds                                    # [°], [°], [°], [°]
                print(f"    {math.floor(x1):+4.0f}° <= x <= {math.ceil(x2):+4.0f}°    {math.floor(y1):+4.0f}° <= y <= {math.ceil(y2):+4.0f}°")

    # **************************************************************************

    # Find file containing all the states/provinces shapes ...
    sfile = cartopy.io.shapereader.natural_earth(
          category = "cultural",
              name = "admin_1_states_provinces",
        resolution = "110m",
    )

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(sfile).records():
        # Create short-hand ...
        neName = pyguymer3.geo.getRecordAttribute(record, "NAME")

        # Check if this state/province is one that I want ...
        if neName in ["Colorado"]:
            print(f"{neName}:")
            for poly in pyguymer3.geo.extract_polys(record.geometry):
                x1, y1, x2, y2 = poly.bounds                                    # [°], [°], [°], [°]
                print(f"    {math.floor(x1):+4.0f}° <= x <= {math.ceil(x2):+4.0f}°    {math.floor(y1):+4.0f}° <= y <= {math.ceil(y2):+4.0f}°")
