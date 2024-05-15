
!BOP
! !ROUTINE: CPP_OPTIONS.h
! !INTERFACE:
! #include "CPP_OPTIONS.h"

! !DESCRIPTION:
! *==================================================================*
! | main CPP options file for the model:
! | Control which optional features to compile in model/src code.
! *==================================================================*
!EOP

! CPP flags controlling particular source code features

! o a simple cpp_options include file


MODULE pkg_mod

contains
    subroutine f( a, x, y )
        implicit none
! == Global Variables ==
!BOP
!     !ROUTINE: PKG.h
!     !INTERFACE:
!     #include PKG.h
!
!     !DESCRIPTION:
!     *================================================================*
!     | PKG.h
!     | o Header file defining "PKG" parameters and variables
!     *================================================================*
!EOP

!     Package flag
      LOGICAL PKG_RUN_THIS

      COMMON /f90_pkg/                                                                                                             &
     &                      PKG_RUN_THIS

!-- COMMON /PKG_PARAMS_I/ PKG Integer-type parameters:
!   PKG_iter   :: GCM iteration to run
!   PKG_iter2  :: GCM iteration to run 2

      INTEGER pkg_iter
      INTEGER pkg_iter2

      COMMON /PKG_PARAMS_I/                                                                                                            &
     &      PKG_iter, PKG_iter2


! == Local Variables ==
        real, intent(in ) :: a, x
        real, intent(out) :: y
    
! == Subroutine Code ==
        if (a.GT.1) then
            ! Do something with the header common blocks
            PKG_RUN_THIS=.true.
            PKG_iter = 10
            PKG_iter2 = PKG_iter + 10
        end if

        if (PKG_RUN_THIS) then
            ! common block parameters won't show up in AD
            y=a*x**4 + PKG_iter + PKG_iter2
        end if

    return
    end
END MODULE pkg_mod
