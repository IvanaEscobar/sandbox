#ifdef ALLOW_IHOP
!CBOP
!C     !ROUTINE: IHOP.h
!C     !INTERFACE:
!C     #include IHOP.h
!
!C     !DESCRIPTION:
!C     *================================================================*
!C     | IHOP.h
!C     | o Header file defining "ihop" parameters and variables
!C     *================================================================*
!CEOP

!C     Package flag
      LOGICAL IHOP_MNC
      LOGICAL IHOP_MDSIO
      COMMON /IHOP_PACKAGE/                                                                                                             &
     &                      IHOP_MNC, IHOP_MDSIO 

!C     IHOP parameters
!C--   COMMON /IHOP_PARAMS_L/ IHOP logical-type parameters:
!C     IHOP_bellOn       :: true if bellhop driver needs to run
      LOGICAL IHOP_bellOn

      COMMON /IHOP_PARAMS_L/                                                                                                             &
     &      IHOP_bellOn

!C-- COMMON /IHOP_PARAMS_C/ IHOP Character-type parameters
!C   IHOP_fileroot :: File name for reading in an environment

      CHARACTER*(100) IHOP_fileroot

      COMMON /IHOP_PARAMS_C/                                                                                                             &
     &      IHOP_fileroot

#ifdef IHOP_3D_STATE
!C     IHOP 3-dim. fields
      _RL ihop_ssp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /IHOP_STATE_3D/                                                                                                             &
     &    ihop_ssp 
#endif /* IHOP_3D_STATE */
#ifdef IHOP_2D_STATE
!C     IHOP 2-dim. fields
      _RL ihop_SST (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myPa_Surf2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /IHOP_STATE_2D/                                                                                                             &
     &    ihop_SST, myPa_Surf2
#endif /* IHOP_2D_STATE */

#ifdef IHOP_TENDENCY
#endif /* IHOP_TENDENCY */

#endif /* ALLOW_IHOP */

!CEH3 ;;; Local Variables: ***
!CEH3 ;;; mode:fortran ***
!CEH3 ;;; End: ***
