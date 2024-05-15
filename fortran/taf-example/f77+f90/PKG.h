#ifdef ALLOW_PKG
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

#endif /* ALLOW_PKG */
