#define _RL90 8
!!C CPP options file for IHOP
!!C Use this file for selecting options within package "Ihop"
!
!#ifndef IHOP_OPTIONS_H
!#define IHOP_OPTIONS_H
!#include "PACKAGES_CONFIG.h"
!#include "CPP_OPTIONS.h"
!
!#ifdef ALLOW_IHOP
!!C Place CPP define/undef flag here
!
!!C to reduce memory storage, disable unused array with those CPP flags :
!#define IHOP_3D_STATE
!#define IHOP_2D_STATE
!#define IHOP_TENDENCY
!
!#undef IHOP_SPECIAL_COMPILE_OPTION1
!
!#define IHOP_SPECIAL_COMPILE_OPTION2
!
!#endif /* ALLOW_IHOP */
!#endif /* IHOP_OPTIONS_H */
!
!!CEH3 ;;; Local Variables: ***
!!CEH3 ;;; mode:fortran ***
!!CEH3 ;;; End: ***
