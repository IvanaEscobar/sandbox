#include "CPP_OPTIONS.h"

      program main
! === Uses ===
      implicit none

#ifdef ALLOW_PKG
      call the_main_loop
#endif
      print '(A)', "MAIN END"

      end program ! main
