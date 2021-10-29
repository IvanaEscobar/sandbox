#
# To install the Acoustics Toolbox:
#
# 1) Uncomment the appropriate lines below to select your FORTRAN compiler
#    (also be sure to comment out all of the lines corresponding to the other compilers).

# 2) If you're using gfortran check the -march switch that selects the chip you're using.
#    Usually -march=native works

# 3) From a command line shell, run:
#    % make clean
#    % make

# on some machines you need to say -mcmodel=medium (or large) to allow for variables larger than 2 gig

# *** Windows ***
# If you don't have a FORTRAN compiler, the MinGW ("Minimalist GNU for Windows") compiler suite
# is a good choice as it is much easier to install than Cygwin. See http://www.mingw.org/ for details.
# Not sure if this is still necessary, but we used to have to change the options -O3 to -O2 below when using gfortran

# *** Linux ***
# Most Linux distributions have gfortran already packaged and it can be installed with the respective 
# package manager (e.g. apt-get, dnf, yum). The packaged versions of the LAPACK library are generally
# compatible with Krakel. If you want statically linked executables, "-static" works with gfortran.

# *** Mac ***
# The option to create an executable that uses a static library has been a problem.
# The gcc community seems to feel that dynamic vs. static libraries is an issue of religious importance.
# I prefer static libraries because users have a lot fewer problems installing the code when they don't have
# to worry about getting the LD_LIBRARY_PATH set, or the fact that some programs (e.g. Matlab) may reset
# that path to an incompatible version.
# -static is supposed to work if gfortran has been compiled to enable that option.
# However, crt0.o comes up as misssing
# -Bstatic worked

# The make utility tends to get confused with modules because it does not necessarily update the *.mod file
# when it compiles the *.f90 file. (The *.mod file contains interface information that doesn't necessarily change
# even then the *.f90 file has changed.)
# As a result, you may find that make keeps compiling a module, that was already compiled.
# A 'make clean' will fix that
# ______________________________________________________________________________

# *** ifort
# These lines are used under Mac OSX; the syntax is different under Windows
# !!!!!!! You also need to use xiar instead of ar
# That change needs to be made in at/misc/Makefile and at/tslib/Makefile
# Note that the -assume byterecl is important, otherwise the output files have the wrong format

# If you use the -parallel option, the compiler links into a dynamic library for OpenMP
# Then you need to make sure that dylib is in the path

# Choose the best architecture (target machine) using the -x switch
# According to the latest benchmarks on polyhedron, it looks like -parallel is very helpful

# need -heap-arrays to avoid stack overflows for big runs ...
export FC=ifort
# export FFLAGS= -O3 -parallel -axSSE4.2 -nologo -inline-level=2 -assume byterecl -threads -heap-arrays
export FFLAGS= -fast -axAVX                           -parallel              -nologo -inline-level=2 -assume byterecl -threads -heap-arrays
export FFLAGS= -O3 -axAVX              -funroll-loops -parallel -no-prec-div -nologo -inline-level=2 -assume byterecl -threads -heap-arrays
export FFLAGS= -O3 -axAVX              -funroll-loops -parallel -no-prec-div -nologo -inline-level=2 -assume byterecl -threads -heap-arrays
export FFLAGS= -O3 -xHost -qopt-report -funroll-loops -parallel -no-prec-div -nologo -inline-level=2 -assume byterecl -threads -heap-arrays
export FFLAGS= -O3 -xHost              -funroll-loops           -no-prec-div -nologo -inline-level=2 -assume byterecl -threads -heap-arrays

# compilation diagnostics on:
# export FFLAGS= -O3 -fast -parallel -ipo -nostandard-realloc-lhs -nologo -inline-level=2 -assume byterecl -threads -heap-arrays -check -traceback
# export FFLAGS= -O3 -fast           -ipo -nostandard-realloc-lhs -nologo -inline-level=2 -assume byterecl -threads -heap-arrays -check -traceback

export FFLAGS= -W0 -WB -convert big_endian -assume byterecl -fPIC -mcmodel=large -shared-intel -132 -O3

# ______________________________________________________________________________

export RM=rm
export CC=gcc
export CFLAGS=-g
#export FFLAGS+= -I../misc -I../tslib -I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include
export FFLAGS+= -I../misc -I../tslib

# KRAKEL is commented out below because it requires the LAPACK library.
# If you have the LAPACK library installed on your system, first edit the
# LAPACK_LIBS variable below to point to your installation, then you can
# uncomment the make commands below (uncomment both the "all" and "install"
# targets).

export LAPACK_LIBS = -llapack

all:
	(cd misc;	make -k all)
	(cd tslib;	make -k all)
	(cd Bellhop;	make -k all)
	(cd Kraken;	make -k all)
	(cd KrakenField;	make -k all)
	# (cd Krakel;	make -k all)
	(cd Scooter;	make -k all)
	@echo " "
	@echo "***********************************"
	@echo "***** Acoustics Toolbox built *****"
	@echo "***********************************"

install:
	(cd misc;	make -k all)
	(cd tslib;	make -k all)
	(cd Bellhop;	make -k install)
	(cd Kraken;	make -k install)
	(cd KrakenField;        make -k install)
	# (cd Krakel;	make -k install)
	(cd Scooter;	make -k install)
	@echo " "
	@echo "***************************************"
	@echo "***** Acoustics Toolbox installed *****"
	@echo "***************************************"

clean:
	-rm -f bin/*.exe
	find . -name '*.dSYM' -exec rm -r {} +
	find . -name '*.png'  -exec rm -r {} +
	find . -name '*.eps'  -exec rm -r {} +
	find . -name '*.mod'  -exec rm -r {} +
	find . -name '*.grn'  -exec rm -r {} +
	find . -name '*.shd'  -exec rm -r {} +
	find . -name '*.shd.mat'  -exec rm -r {} +
	find . -name '*.prt'  -exec rm -r {} +
	(cd misc;	make -k -i clean)
	(cd tslib;	make -k -i clean)
	(cd Bellhop;	make -k -i clean)
	(cd Kraken;	make -k -i clean)
	(cd KrakenField;	make -k -i clean)
	(cd Krakel;	make -k -i clean)
	(cd Scooter;	make -k -i clean)
	(cd tests;	make -k -i clean)

