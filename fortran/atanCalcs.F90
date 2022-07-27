! ------------------------------------------------------
! what is the difference in output of ATAN and ATAN2 w reals only 
! ------------------------------------------------------

      PROGRAM atanCalc 
        USE, INTRINSIC :: iso_fortran_env, ONLY: dp=>real64
        IMPLICIT NONE
        
        ! Arrays
        REAL(dp) arr(10), arrImag(10)
        INTEGER i

        arr = [(i**2 * 3.1416/ 180., i=1,10)]
        arrImag = [(i * 3.1416/180., i=1,10)]

        arr = ATAN(arrImag, arr) - ATAN2(arrImag, arr)

        WRITE(*,*) "Array = ", arr
      END PROGRAM atanCalc
