#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import special modules ...
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "backend" : "Agg",                                       # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                    "figure.dpi" : 300,
                "figure.figsize" : (9.6, 7.2),                                  # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                     "font.size" : 8,
            }
        )
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    # **************************************************************************

    # Create figure ...
    fg = matplotlib.pyplot.figure()

    # Create axis ...
    ax = fg.add_subplot()

    # Load data and plot it ...
    i, n = numpy.loadtxt(
        "createMask1_oneSweep.csv",
        delimiter = ",",
            dtype = numpy.uint32,
         skiprows = 2,
           unpack = True,
    )
    ax.plot(
        i,
        100.0 * n.astype(numpy.float64) / 43200.0 / 21600.0,
        label = "global-only; one sweep",
    )

    # Load data and plot it ...
    i, n = numpy.loadtxt(
        "createMask1_fourSweeps.csv",
        delimiter = ",",
            dtype = numpy.uint32,
         skiprows = 2,
           unpack = True,
    )
    ax.plot(
        i,
        100.0 * n.astype(numpy.float64) / 43200.0 / 21600.0,
        label = "global-only; four sweeps",
    )

    # Load data and plot it ...
    i, n = numpy.loadtxt(
        "createMask2_oneSweep.csv",
        delimiter = ",",
            dtype = numpy.uint32,
         skiprows = 2,
           unpack = True,
    )
    ax.plot(
        i,
        100.0 * n.astype(numpy.float64) / 43200.0 / 21600.0,
        label = "global and tiled; one sweep",
    )

    # Load data and plot it ...
    i, n = numpy.loadtxt(
        "createMask2_fourSweeps.csv",
        delimiter = ",",
            dtype = numpy.uint32,
         skiprows = 2,
           unpack = True,
    )
    ax.plot(
        i,
        100.0 * n.astype(numpy.float64) / 43200.0 / 21600.0,
        label = "global and tiled; four sweeps",
    )

    # Configure axis ...
    ax.grid()
    ax.legend(loc = "lower right")
    ax.set_xlabel("Step Number")
    ax.set_xlim(0, 200)
    ax.set_ylabel("Fraction Of Dataset That Is Accessible [%]")

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig("step12_convergence.png")
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(
        "step12_convergence.png",
        strip = True,
    )
