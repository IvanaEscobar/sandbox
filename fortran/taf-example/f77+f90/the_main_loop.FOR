#include "CPP_OPTIONS.h"

      subroutine the_main_loop
! mimicing MITgcm's top level routine

! === Uses and F90 modules ===
#ifdef ALLOW_PKG
     ! The entire module is public
      use pkg_mod
#endif
      implicit none

! === Global Variables ===
      
! === Local Variables === 
      real :: a,x,y

! === Program Code ===
      !read *, a, x
      a=2
      x=1.4

#ifdef ALLOW_PKG
!      call f(a,x,y)
!      print '(F10.3)', y
      print *, a
#endif

      return
      end subroutine ! the_main_loop
