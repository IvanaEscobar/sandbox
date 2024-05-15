
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


      subroutine the_main_loop
! mimicing MITgcm's top level routine

! === Uses and F90 modules ===
     ! The entire module is public
      use pkg_mod
      implicit none

! === Global Variables ===
      
! === Local Variables === 
      real :: a,x,y

! === Program Code ===
      !read *, a, x
      a=2
      x=1.4

!      call f(a,x,y)
!      print '(F10.3)', y
      print *, a

      return
      end subroutine ! the_main_loop
