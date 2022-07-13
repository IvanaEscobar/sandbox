#ifdef ALLOW_IHOP

CBOP
C     !ROUTINE: IHOP_PARAMS.h
C     !INTERFACE:
C     #include IHOP_PARAMS.h

C     !DESCRIPTION:
C     *================================================================*
C     | IHOP.h
C     | o Header file defining "ihop" parameters and variables
C     *================================================================*
CEOP

C     IHOP parameters
C--   COMMON /IHOP_PARAMS_L/ IHOP logical-type parameters:
C     IHOP_bellOn       :: true if bellhop driver needs to run
      LOGICAL IHOP_bellOn
C      LOGICAL 
C     &      IHOP_MNC, IHOP_MDSIO

      COMMON /IHOP_PARAMS_L/
     &      IHOP_bellOn
C     &      IHOP_MNC, IHOP_MDSIO

#ifdef IHOP_3D_STATE
C     IHOP 3-dim. fields
      _RL myPa_StatScal1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatScal2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatVelU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatVelV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /IHOP_STATE_3D/
     &    myPa_StatScal1, myPa_StatScal2,
     &    myPa_StatVelU,  myPa_StatVelV
#endif /* IHOP_3D_STATE */
#ifdef IHOP_2D_STATE
C     IHOP 2-dim. fields
      _RL myPa_Surf1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myPa_Surf2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /IHOP_STATE_2D/
     &    myPa_Surf1, myPa_Surf2
#endif /* IHOP_2D_STATE */

#endif /* ALLOW_IHOP */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
