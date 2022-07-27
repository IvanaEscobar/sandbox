! ------------------------------------------------------
! divide by zero to see what happens
! ------------------------------------------------------

      PROGRAM divideByZero 
        USE, INTRINSIC :: iso_fortran_env, ONLY: dp=>real64
        IMPLICIT NONE
        
        ! Arrays
        REAL(dp) arr(10)
        INTEGER i
        REAL(dp) zero 

        zero = 0.0
        arr = [(i**2/zero, i=1,10)]
        arr(1:3) = 500
      
        WRITE(*,*) "Array = ", arr
      END PROGRAM divideByZero
