# Where Can Pregnant Women Go? (WCPWG)

This all started after reading the [NHS guidance on exercise in pregnancy](https://www.nhs.uk/conditions/pregnancy-and-baby/pregnancy-exercise/).

## Workflow

1. Download the GLOBE dataset (by running [downloadGLOBE.py](downloadGLOBE.py))
2. Convert the ZIP file of the GLOBE dataset to a BIN file (by running [convertZIPtoBIN.py](convertZIPtoBIN.py))
3. Create the boolean mask (by running [compile.sh](compile.sh) to compile [createMask2.F90](createMask2.F90) and then run it by `./createMask2`)
4. Convert all generated PPM images to PNG images (by running [convertPPMtoPNG.sh](convertPPMtoPNG.sh))

## To Do

* Obviously, the [compile.sh](compile.sh) needs to be replaced by a real Makefile at some point.
