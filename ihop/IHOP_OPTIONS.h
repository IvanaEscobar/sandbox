#ifndef IHOP_OPTIONS_H
#define IHOP_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

!BOP
! !ROUTINE: IHOP_OPTINOS.h
! !INTERFACE:
! #include "IHOP_OPTIONS.h"

! !DESCRIPTION:
! CPP options file for IHOP package:
! Use this file for selecting options within package "ihop"
!EOP

#ifdef ALLOW_IHOP
!C Place CPP define/undef flag here

!C to reduce memory storage, disable unused array with those CPP flags :
#define IHOP_3D_STATE
#define IHOP_2D_STATE
#define IHOP_TENDENCY

#undef IHOP_SPECIAL_COMPILE_OPTION1

#define IHOP_SPECIAL_COMPILE_OPTION2

#endif /* ALLOW_IHOP */
#endif /* IHOP_OPTIONS_H */

!CEH3 ;;; Local Variables: ***
!CEH3 ;;; mode:fortran ***
!CEH3 ;;; End: ***
