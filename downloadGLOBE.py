#!/usr/bin/env python3

# Import standard modules ...
import os

# Import special modules ...
try:
    import requests
except:
    raise Exception("\"requests\" is not installed; run \"pip install --user requests\"")

# Import my modules ...
try:
    import pyguymer3
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH")

# ******************************************************************************

# Check if the ZIP file does not exist yet ...
if not os.path.exists("all10g.zip"):
    print("Downloading \"all10g.zip\" ...")

    # Start session ...
    sess = requests.Session()
    sess.allow_redirects = True
    sess.headers.update({"Accept" : "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"})
    sess.headers.update({"Accept-Language" : "en-GB,en;q=0.5"})
    sess.headers.update({"Accept-Encoding" : "gzip, deflate"})
    sess.headers.update({"DNT" : "1"})
    sess.headers.update({"Upgrade-Insecure-Requests" : "1"})
    sess.headers.update({"User-Agent" : "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:68.0) Gecko/20100101 Firefox/68.0"})
    sess.max_redirects = 5

    # Download the ZIP file ...
    if not pyguymer3.download_file(sess, "https://www.ngdc.noaa.gov/mgg/topo/DATATILES/elev/all10g.zip", "all10g.zip"):
        raise Exception("download failed", "https://www.ngdc.noaa.gov/mgg/topo/DATATILES/elev/all10g.zip")

    # End session ...
    sess.close()
