! ------------------------------------------------------
! print array elements
! ------------------------------------------------------

      PROGRAM  writeArray
        USE, INTRINSIC :: iso_fortran_env, ONLY: dp=>real64
        IMPLICIT NONE
        
        ! Arrays
        REAL(dp) arr(10)
        INTEGER i 

        ! DO i=1,10
        !     arr(i) = i**2
        ! ENDDO
        arr = [(i**2, i=1,10)]
        arr(1:3) = 500
      
        WRITE(*,*) "Array = ", arr
      END PROGRAM  writeArray
