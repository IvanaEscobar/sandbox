C CPP options file for BELLHOP
C Use this file for selecting options within package "Bellhop"

#ifndef BELLHOP_OPTIONS_H
#define BELLHOP_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_BELLHOP
C Place CPP define/undef flag here

C to reduce memory storage, disable unused array with those CPP flags :
#define BELLHOP_3D_STATE
#define BELLHOP_2D_STATE
#define BELLHOP_TENDENCY

#undef BELLHOP_SPECIAL_COMPILE_OPTION1

#define BELLHOP_SPECIAL_COMPILE_OPTION2

#endif /* ALLOW_BELLHOP */
#endif /* BELLHOP_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
