#!/usr/bin/env bash

# Clean up ...
rm -f *.o

# Define options ...
LANG_OPTS="-ffree-form -ffree-line-length-none -frecursive"
WARN_OPTS="-Wall -Wextra -Waliasing -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure"
MACH_OPTS="-m64"

# Compile ...
mpif90 -c ${LANG_OPTS} ${WARN_OPTS} ${MACH_OPTS} -Ifortranlib createMask.F90
mpif90 -o createMask ${LANG_OPTS} ${WARN_OPTS} ${MACH_OPTS} createMask.o fortranlib/*.o -Lfortranlib
