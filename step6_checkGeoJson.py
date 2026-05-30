#!/usr/bin/env python3

"""Check GeoJSON files"""

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import glob

    # Import special modules ...
    try:
        import geojson
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # **************************************************************************

    # Loop over GeoJSONs ...
    for fName in sorted(glob.glob("output/tileScale=??km/scale=??km/inaccessible.geojson")):
        # Load GeometryCollection ...
        with open(fName, "rt", encoding = "utf-8") as fObj:
            geoms = shapely.geometry.shape(
                geojson.load(fObj)
            )

        # Check GeometryCollection ...
        assert isinstance(geoms, shapely.geometry.collection.GeometryCollection), type(geoms)
        assert geoms.is_valid, shapely.validation.explain_validity(geoms)
        assert not geoms.is_empty, "a GeometryCollection is empty"

        # Loop over items in GeometryCollection ...
        for geom in geoms.geoms:
            # Check item ...
            assert isinstance(geom, shapely.geometry.polygon.Polygon), type(geom)
            assert geom.is_valid, shapely.validation.explain_validity(geom)
            assert not geom.is_empty, "a Polygon is empty"
