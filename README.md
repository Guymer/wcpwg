# Where Can Pregnant Women Go? (WCPWG)

This all started after reading the [NHS guidance on exercise in pregnancy](https://www.nhs.uk/conditions/pregnancy-and-baby/pregnancy-exercise/).

## Workflow

1. Download the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset (by running [downloadGLOBE.py](downloadGLOBE.py))
2. Convert the ZIP file of the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset to a BIN file (by running [convertZIPtoBIN.py](convertZIPtoBIN.py))
3. Compile the FORTRAN programs (by running [compile.sh](compile.sh))
4. Create the boolean mask (by running any of: [createMask1.F90](createMask1.F90), [createMask2.F90](createMask2.F90) or [createMask3.F90](createMask3.F90))
    * [createMask1.F90](createMask1.F90) applies the algorithm globally and loops until no more pixels are masked (or `nmax` is reached)
    * [createMask2.F90](createMask2.F90) applies the algorithm globally **then on tiles** and loops **over both stages** until no more pixels are masked (or `nmax` is reached)
    * [createMask3.F90](createMask3.F90) is the same as [createMask2.F90](createMask2.F90) but has extra output to make a pretty blog post
5. Compare the output between versions (by running `join -t, createMask1.csv createMask2.csv > createMask.csv`)
6. Convert all generated PBM images to PNG images (by running [convertPBMtoPNG.sh](convertPBMtoPNG.sh))
7. Convert all generated PPM images to PNG images (by running [convertPPMtoPNG.sh](convertPPMtoPNG.sh))

## To Do

* Obviously, the [compile.sh](compile.sh) needs to be replaced by a real Makefile at some point.
