! ------------------------------------------------------
! print array elements
! ------------------------------------------------------

      PROGRAM  writeArray
        USE, INTRINSIC :: iso_fortran_env, ONLY: dp=>real64
        IMPLICIT NONE
        
        ! Arrays
        REAL(dp), allocatable :: arr(:)
        REAL(dp) :: IHOP_arr(1)
        INTEGER i 

        DO i=1,1
            IHOP_arr(i) = 1+i
        ENDDO
        arr = IHOP_arr

        !ALLOCATE( arr(MAX(3,1)), Stat=i )
        if (allocated(arr)) then
            WRITE(*,*) arr
        end if
        ! DO i=1,10
        !     arr(i) = i**2
        ! ENDDO

        !arr = [(i**2, i=1,10)]
        !arr(1:3) = 500
      
        WRITE(*,*) "Array = ", arr
      END PROGRAM  writeArray
