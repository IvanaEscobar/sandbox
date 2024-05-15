FC = gfortran
LINK = gfortran
CC = cpp
FFLAGS = -fimplicit-none -fPIC -ffixed-line-length-132 -ffree-line-length-0 -fallow-argument-mismatch
FOPTIM = -O3 -ftree-vectorize -funroll-loops
F90FLAGS = -fimplicit-none -fPIC -ffixed-line-length-132 -ffree-line-length-0 -fallow-argument-mismatch -x f95
F90OPTIM = -O3 -ftree-vectorize -funroll-loops
CFLAGS = -traditional -P

EXECUTABLE = mitgcmuv

F77_SRC_FILES = \
 main.FOR \
 the_main_loop.FOR
F90_SRC_FILES = \
 pkg_mod.FOR90
H_SRC_FILES =\
 CPP_OPTIONS.h \
 PKG.h

F77_PP_SRC_FILES = $(F77_SRC_FILES:.FOR=.f)
F90_PP_SRC_FILES = $(F90_SRC_FILES:.FOR90=.f90)
OBJFILES = $(F77_SRC_FILES:.FOR=.o) $(F90_SRC_FILES:.FOR90=.o)

#OBJECTS   = pkg_mod.o the_main_loop.o main.o

# Declare implicit rules (for make utilities with missing or broken build rules)

.SUFFIXES: .o .f .f90 .FOR .FOR90
	
all: fwd_exe_target
fwd_exe_target:
	@echo "Start compile"
	@echo "*************"
	@echo " "
	make -f Makefile $(EXECUTABLE)

$(EXECUTABLE): $(F77_SRC_FILES) $(F90_SRC_FILES) $(H_SRC_FILES) $(OBJFILES)
	@echo Creating $@ ...
	$(LINK) -o $@ $(FFLAGS) $(FOPTIM) $(OBJFILES)

clean:
	-rm -f *.o *.mod f_exec
Clean:
	-rm -f *.o *.mod f_exec *.f90 *.f

obj: $(OBJFILES)
small_f: $(F77_PP_SRC_FILES) $(F90_PP_SRC_FILES)

# process header files
CPPCMD = cat $< | $(CC) $(CFLAGS)

.FOR.f:
	$(CPPCMD) > $@
.f.o:
	$(FC) $(FFLAGS) $(FOPTIM) -c $< 

.FOR90.f90:
	$(CPPCMD) > $@
.f90.o:
	$(FC) $(F90FLAGS) $(FOPTIM) -c $< 

# Declare explicit dependencies (and explicit build rules as needed)

# DEPENDENCIES:
main.f: CPP_OPTIONS.h 
pkg_mod.f90: CPP_OPTIONS.h PKG.h
the_main_loop.f: CPP_OPTIONS.h pkg_mod.o

