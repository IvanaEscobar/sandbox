! ------------------------------------------------------
! what does minloc function give you
! ------------------------------------------------------

      PROGRAM minlocOutput 
        USE, INTRINSIC :: iso_fortran_env, ONLY: dp=>real64
        IMPLICIT NONE
        
        ! Arrays
        REAL(dp) arr(10)
        INTEGER i, iLoc(1)

        arr = [(i**2, i=1,10)]
        arr(1:3) = 500
      
        iLoc = MINLOC( arr, MASK=arr < 100  ) 
        WRITE(*,*) "Array = ", arr
        WRITE(*,*) iLoc
      END PROGRAM minlocOutput
