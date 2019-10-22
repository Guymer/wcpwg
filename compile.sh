#!/usr/bin/env bash

# Clean up ...
rm -f *.o

# Define options ...
LANG_OPTS="-ffree-form -ffree-line-length-none -frecursive"
WARN_OPTS="-Wall -Wextra -Waliasing -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure"
MACH_OPTS="-m64"

# Compile ...
mpif90 -c ${LANG_OPTS} ${WARN_OPTS} ${MACH_OPTS} -Ifortranlib mod_funcs.F90
mpif90 -c ${LANG_OPTS} ${WARN_OPTS} ${MACH_OPTS} -Ifortranlib createMask1.F90
mpif90 -c -fopenmp ${LANG_OPTS} ${WARN_OPTS} ${MACH_OPTS} -Ifortranlib createMask2.F90
mpif90 -c -fopenmp ${LANG_OPTS} ${WARN_OPTS} ${MACH_OPTS} -Ifortranlib createMask3.F90
mpif90 -o createMask1 ${LANG_OPTS} ${WARN_OPTS} ${MACH_OPTS} createMask1.o mod_funcs.o fortranlib/*.o -Lfortranlib
mpif90 -o createMask2 -fopenmp ${LANG_OPTS} ${WARN_OPTS} ${MACH_OPTS} createMask2.o mod_funcs.o fortranlib/*.o -Lfortranlib
mpif90 -o createMask3 -fopenmp ${LANG_OPTS} ${WARN_OPTS} ${MACH_OPTS} createMask3.o mod_funcs.o fortranlib/*.o -Lfortranlib
