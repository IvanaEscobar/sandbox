# makefile for gcov-fortran

FC=gfortran

SOURCE=main.f90 sub1.f90 sub2.f90 sub3.f90 sub4.f90
OBJ=main.o sub1.o sub2.o sub3.o sub4.o
#GCOV=/usr/local/Cellar/gcc/4.9.1/bin/gcov-4.9
GCOV=/usr/local/bin/gcov-11

ALL: test

test: $(OBJ)
	$(FC) -Wall --coverage -O0 -g -o test $(OBJ)

%.o: %.f90
	$(FC) -Wall --coverage -O0 -g -c $<

cov: $(SOURCE)
	$(GCOV) *.f90

html: $(SOURCE)
	lcov --gcov-tool $(GCOV) --capture --directory . --output-file coverage.info
	genhtml --output-directory html coverage.info
	open html/index.html

clean:
	rm -rf *.mod *.o

cleancov:
	rm -rf html/ *.gcda *.gcno coverage.info

cleanall: clean cleancov
