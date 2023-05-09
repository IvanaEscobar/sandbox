! ------------------------------------------------------
! print array elements
! ------------------------------------------------------

      PROGRAM  writeKind
        IMPLICIT NONE
        
        ! Arrays
        !REAL(KIND=dp) :: x_dp
        REAL(KIND=8) :: x_kind8
        REAL :: x_none

        !x_dp = -999.9
        x_kind8 = -999.9
        x_none = -999.9

        WRITE(*,*) "No spec = ", x_none 
        !WRITE(*,*) "dp = ", x_dp
        WRITE(*,*) "kind8 = ", x_kind8
        WRITE(*,*) "diff ", -999.9 - x_kind8

      END PROGRAM  writeKind
