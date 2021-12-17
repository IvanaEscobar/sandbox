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

# *** Linux ***
# Most Linux distributions have gfortran already packaged and it can be installed with the respective 
# package manager (e.g. apt-get, dnf, yum). The packaged versions of the LAPACK library are generally
# compatible with Krakel. If you want statically linked executables, "-static" works with gfortran.

# The make utility tends to get confused with modules because it does not necessarily update the *.mod file
# when it compiles the *.f90 file. (The *.mod file contains interface information that doesn't necessarily change
# even then the *.f90 file has changed.)
# As a result, you may find that make keeps compiling a module, that was already compiled.
# A 'make clean' will fix that
# ______________________________________________________________________________

# *** GNU Compiler Collection GFORTRAN

# use -march=generic if you get warning messages about instructions that don't make sense
# -march=generic assumes an old Intel architecture that the newer versions can all execute (slowly)
# -march=native should normally be the best; however, it produced AVX instructions on the Mac that the default assembler could not process
# -O2 was the highest level of optimization that worked under Windows

# -static can be used to tell gfortran not to rely on a dynamic link library (the compiler may or may not support)
# -static does not seem to work on Macs though, and produces larger executables

# Have had various problems where some installed dynamic link library is incompatible with the one the compiler used and expects at run time
# For instance, Matlab changes paths and may point to an incompatible library.
# One user found that it was necessary to delete /usr/local/gfortran/lib/libquadmath.dylib to force a static link. See:
# http://stackoverflow.com/questions/17590525/correct-way-to-statically-link-in-gfortran-libraries-on-osx
#
# The -Wa,-q flag can be used to select the Mac CLANG assembler instead of the GNU assembler
# At one time that was necessary to get the AVX operations; however, I saw no speed benefit
# -march=corei7-avx works on my Mac

export FC=gfortran

# export FFLAGS= -march=native  -Wall -std=gnu -O3 -ffast-math -funroll-all-loops -msse3 -fomit-frame-pointer -mtune=native -Q
# export FFLAGS= -march=corei7 -Bstatic -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation       -std=f2008 -O3 -ffast-math -funroll-all-loops -fomit-frame-pointer -mtune=native
# export FFLAGS= -march=corei7 -Bstatic -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation        -std=f2008 -O3 -ffast-math -funroll-all-loops -fomit-frame-pointer
#export FFLAGS= -march=native -Bstatic -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation         -std=gnu  -O2 -ffast-math -funroll-all-loops -fomit-frame-pointer -mtune=native
export FFLAGS= -march=native -Bstatic -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation         -std=gnu  -O3 -ffast-math -funroll-all-loops -fomit-frame-pointer
# export FFLAGS= -p -g -march=native -Bstatic -Wa,-q -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation -std=gnu -O3 -ffast-math -funroll-all-loops -fcheck=all

# Compilation and run-time diagnostics on:
# omni.env fails trap=invalid
# export FFLAGS= -march=native -ffpe-trap=invalid,zero,overflow -Wall                  -std=gnu -O1 -fcheck=all -fbacktrace
# export FFLAGS= -march=native -ffpe-trap=zero,overflow         -Wall                  -std=gnu -O1 -fcheck=all -fbacktrace
# export FFLAGS= -march=native -ffpe-trap=zero,overflow         -Wall -pedantic-errors -std=gnu -O1 -fcheck=all -fbacktrace

# ______________________________________________________________________________

export RM=rm
export CC=gcc
export CFLAGS=-g
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

