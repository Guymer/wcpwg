# Find executables ...
CUT  := $(shell which cut    2> /dev/null || echo "ERROR")
FC   := $(shell which mpif90 2> /dev/null || echo "ERROR")
GREP := $(shell which grep   2> /dev/null || echo "ERROR")
RM   := $(shell which rm     2> /dev/null || echo "ERROR")

# Set defaults ...
DEBUG  ?= false
FTNLIB ?= fortranlib
LIBDIR ?= /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib

# ******************************************************************************

# Set compiler flags ...
DEBG_OPTS := -g -fcheck=all
LANG_OPTS := -ffree-form -ffree-line-length-none -frecursive -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -fPIC
WARN_OPTS := -Wall -Wextra -Waliasing -Wcharacter-truncation -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wtabs
OPTM_OPTS := -O2
MACH_OPTS := -march=native -m64

# If the user wants debugging then append the debugging flags to the language
# flags ...
ifeq ($(DEBUG), true)
	LANG_OPTS += $(DEBG_OPTS)
endif

# Check binaries ...
ifeq ($(CUT),ERROR)
    $(error The binary "cut" is not installed)
endif
ifeq ($(FC),ERROR)
    $(error The binary "fc" is not installed)
endif
ifeq ($(GREP),ERROR)
    $(error The binary "grep" is not installed)
endif
ifeq ($(RM),ERROR)
    $(error The binary "rm" is not installed)
endif

# Check Python modules ...
# ifneq ($(shell $(PYTHON3) -c "import numpy; print(0)" 2> /dev/null),0)
    # $(error The Python module "numpy" is not installed)
# endif

# ******************************************************************************

# "gmake [all]"   = "make compile" (default)
all:				compile

# "gmake clean"   = removes the compiled code
clean:				$(RM)
	$(RM) -f compareMasks createMask1 createMask2 createMask3 *.mod *.o
	$(MAKE) -r -C $(FTNLIB) clean

# "gmake compile" = compiles the code
compile:			mod_funcs.o													\
					compareMasks												\
					createMask1													\
					createMask2													\
					createMask3

# "gmake help"    = print this help
help:				$(GREP)														\
					$(CUT)
	echo "These are the available options:"
	$(GREP) -E "^# \"gmake " Makefile | $(CUT) -c 2-

# ******************************************************************************

.SILENT: help

# ******************************************************************************

# NOTE: As of 01/Nov/2019 there is still a bug in "gcc9" from MacPorts which
#       results in it being unable to find some system libraries. Below are
#       links to the MacPorts ticket and the GCC ticket as well as the reference
#       for my chosen (hopefully temporary) workaround.
#         * https://trac.macports.org/ticket/59113
#         * https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90835
#         * https://stackoverflow.com/a/58081934

$(FTNLIB)/%.mod																	\
$(FTNLIB)/%.o:	$(FTNLIB)/Makefile												\
				$(FTNLIB)/%.F90
	$(MAKE) -r -C $(FTNLIB) DEBUG=$(DEBUG) $*.o

mod_funcs.mod																	\
mod_funcs.o:	$(FC)															\
				$(FTNLIB)/mod_safe.mod											\
				mod_funcs.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) -I$(FTNLIB) mod_funcs.F90

compareMasks.o:	$(FC)															\
				$(FTNLIB)/mod_safe.mod											\
				compareMasks.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) -I$(FTNLIB) compareMasks.F90 -o $@

createMask1.o:	$(FC)															\
				$(FTNLIB)/mod_safe.mod											\
				mod_funcs.mod													\
				createMask1.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) -I$(FTNLIB) createMask1.F90 -o $@

createMask2.o:	$(FC)															\
				$(FTNLIB)/mod_safe.mod											\
				mod_funcs.mod													\
				createMask2.F90
	$(FC) -c -fopenmp $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) -I$(FTNLIB) createMask2.F90 -o $@

createMask3.o:	$(FC)															\
				$(FTNLIB)/mod_safe.mod											\
				mod_funcs.mod													\
				createMask3.F90
	$(FC) -c -fopenmp $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) -I$(FTNLIB) createMask3.F90 -o $@

compareMasks:	$(FC)															\
				$(FTNLIB)/mod_safe.o											\
				compareMasks.o
	$(FC) $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) compareMasks.o $(FTNLIB)/mod_safe.o -L$(LIBDIR) -o $@

createMask1:	$(FC)															\
				$(FTNLIB)/mod_safe.o											\
				mod_funcs.o														\
				createMask1.o
	$(FC) $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) createMask1.o $(FTNLIB)/mod_safe.o mod_funcs.o -L$(LIBDIR) -o $@

createMask2:	$(FC)															\
				$(FTNLIB)/mod_safe.o											\
				mod_funcs.o														\
				createMask2.o
	$(FC) -fopenmp $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) createMask2.o $(FTNLIB)/mod_safe.o mod_funcs.o -L$(LIBDIR) -o $@

createMask3:	$(FC)															\
				$(FTNLIB)/mod_safe.o											\
				mod_funcs.o														\
				createMask3.o
	$(FC) -fopenmp $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) createMask3.o $(FTNLIB)/mod_safe.o mod_funcs.o -L$(LIBDIR) -o $@
