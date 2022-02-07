! CPP options file for BELLHOP
! Use this file for selecting options within package "Bellhop"

#ifndef BELLHOP_OPTIONS_H
#define BELLHOP_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS_90.h"

#ifdef ALLOW_BELLHOP
! Place CPP define/undef flag here

! to reduce memory storage, disable unused array with those CPP flags :
#define BELLHOP_3D_STATE
#define BELLHOP_2D_STATE
#define BELLHOP_TENDENCY

#undef BELLHOP_SPECIAL_COMPILE_OPTION1

#define BELLHOP_SPECIAL_COMPILE_OPTION2

#endif /* ALLOW_BELLHOP */
#endif /* BELLHOP_OPTIONS_H */

!EH3 ;;; Lo!al Variables: ***
!EH3 ;;; mode:fortran ***
!EH3 ;;; End: ***
