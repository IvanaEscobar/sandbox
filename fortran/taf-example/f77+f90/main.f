
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


      program main
! === Uses ===
      implicit none

      call the_main_loop
      print '(A)', "MAIN END"

      end program ! main
