# Where Can Pregnant Women Go? (WCPWG)

This all started after reading the [NHS guidance on exercise in pregnancy](https://www.nhs.uk/conditions/pregnancy-and-baby/pregnancy-exercise/).

1. download the GLOBE dataset (by running [downloadGLOBE.py](downloadGLOBE.py))
2. convert the ZIP file of the GLOBE dataset to a BIN file (by running [convertZIPtoBIN.py](convertZIPtoBIN.py))
3. create the boolean mask (by running [compile.sh](compile.sh) to compile [createMask.F90](createMask.F90) and then run it by `./createMask`)
4. convert all created PPM images to PNG images (by running [convertPPMtoPNG.sh](convertPPMtoPNG.sh))
