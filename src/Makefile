# ******************************************************************************
# *                                 VARIABLES                                  *
# ******************************************************************************

COVERAGE  ?= false
DEBUG     ?= false
FTNLIB    ?= $(abspath ../fortranlib)
LANG_OPTS := -fopenmp -ffree-form -ffree-line-length-none -frecursive -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -fPIC
WARN_OPTS := -Wall -Wextra -Waliasing -Wcharacter-truncation -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wtabs
MACH_OPTS := -march=native -m64

# ******************************************************************************
# *                                  BINARIES                                  *
# ******************************************************************************

CUT     := $(shell which cut            2> /dev/null || echo "ERROR")
FC      := $(shell which gfortran-mp-14 2> /dev/null || echo "ERROR")
GREP    := $(shell which grep           2> /dev/null || echo "ERROR")
PYTHON3 := $(shell which python3.12     2> /dev/null || echo "ERROR")
RM      := $(shell which rm             2> /dev/null || echo "ERROR")

# ******************************************************************************
# *                             DYNAMIC VARIABLES                              *
# ******************************************************************************

ifeq ($(COVERAGE), true)
	LANG_OPTS += -g -O0 -fprofile-abs-path --coverage
else ifeq ($(DEBUG), true)
	LANG_OPTS += -g -fcheck=all
else
	LANG_OPTS += -O2
endif

# ******************************************************************************
# *                               CHECK BINARIES                               *
# ******************************************************************************

ifeq ($(CUT),ERROR)
    $(error The binary "cut" is not installed)
endif
ifeq ($(FC),ERROR)
    $(error The binary "fc" is not installed)
endif
ifeq ($(GREP),ERROR)
    $(error The binary "grep" is not installed)
endif
ifeq ($(PYTHON3),ERROR)
    $(error The binary "python3" is not installed)
endif
ifeq ($(RM),ERROR)
    $(error The binary "rm" is not installed)
endif

# ******************************************************************************
# *                            CHECK PYTHON MODULES                            *
# ******************************************************************************

# ifneq ($(shell $(PYTHON3) -c "import numpy; print(0)" 2> /dev/null),0)
#     $(error The Python module "numpy" is not installed)
# endif

# ******************************************************************************
# *                             DERIVED VARIABLES                              *
# ******************************************************************************

# SUFFIX ?= $(shell $(PYTHON3) -c "import sysconfig; print(sysconfig.get_config_var(\"EXT_SUFFIX\"))")

# ******************************************************************************
# *                           USER-SPECIFIED TARGETS                           *
# ******************************************************************************

# "gmake -r [all]"   = "gmake -r compile" (default)
all:				compile

# "gmake -r clean"   = removes the compiled code
clean:
	$(RM) -f compareMasks createMask1 createMask2 createMask3 *.gcda *.gcno *.mod *.o *.so
	$(MAKE) -r -C $(FTNLIB) COVERAGE=$(COVERAGE) DEBUG=$(DEBUG) FC=$(FC) PYTHON3=$(PYTHON3) clean

# "gmake -r compile" = compiles the code
compile:			compareMasks												\
					createMask1													\
					createMask2													\
					createMask3

# "gmake -r help"    = print this help
help:
	echo "These are the available options:"
	$(GREP) -E "^# \"gmake -r " Makefile | $(CUT) -c 2-

# ******************************************************************************
# *                            ENVIRONMENT SETTINGS                            *
# ******************************************************************************

.SILENT: help

# ******************************************************************************
# *                        INTERNALLY-SPECIFIED TARGETS                        *
# ******************************************************************************

# NOTE: There was a bug in NumPy (using "meson" to build) where "f2py" would
#       copy the file to a build folder and, therefore, the relative paths to
#       external libraries would break. To work around this I prepend the
#       current directory to the library path to make it an absolute path. See:
#         * https://github.com/numpy/numpy/issues/25344

# NOTE: See https://numpy.org/doc/stable/f2py/buildtools/distutils-to-meson.html

$(FTNLIB)/%.mod																	\
$(FTNLIB)/%.o &:	$(FTNLIB)/%.F90
	$(MAKE) -r -C $(FTNLIB) COVERAGE=$(COVERAGE) DEBUG=$(DEBUG) FC=$(FC) PYTHON3=$(PYTHON3) $*.o

mod_funcs.mod																	\
mod_funcs.o &:		$(FTNLIB)/mod_safe.mod										\
					mod_funcs.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) -I$(FTNLIB) mod_funcs.F90

compareMasks.o:		$(FTNLIB)/mod_safe.mod										\
					compareMasks.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) -I$(FTNLIB) compareMasks.F90 -o $@

createMask1.o:		$(FTNLIB)/mod_safe.mod										\
					mod_funcs.mod												\
					createMask1.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) -I$(FTNLIB) createMask1.F90 -o $@

createMask2.o:		$(FTNLIB)/mod_safe.mod										\
					mod_funcs.mod												\
					createMask2.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) -I$(FTNLIB) createMask2.F90 -o $@

createMask3.o:		$(FTNLIB)/mod_safe.mod										\
					mod_funcs.mod												\
					createMask3.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) -I$(FTNLIB) createMask3.F90 -o $@

compareMasks:		$(FTNLIB)/mod_safe.o										\
					compareMasks.o
	$(FC) $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) compareMasks.o $(FTNLIB)/mod_safe.o -o $@

createMask1:		$(FTNLIB)/mod_safe.o										\
					mod_funcs.o													\
					createMask1.o
	$(FC) $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) createMask1.o $(FTNLIB)/mod_safe.o mod_funcs.o -o $@

createMask2:		$(FTNLIB)/mod_safe.o										\
					mod_funcs.o													\
					createMask2.o
	$(FC) $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) createMask2.o $(FTNLIB)/mod_safe.o mod_funcs.o -o $@

createMask3:		$(FTNLIB)/mod_safe.o										\
					mod_funcs.o													\
					createMask3.o
	$(FC) $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) createMask3.o $(FTNLIB)/mod_safe.o mod_funcs.o -o $@
