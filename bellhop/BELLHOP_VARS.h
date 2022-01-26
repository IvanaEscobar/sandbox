#ifdef ALLOW_BELLHOP

#ifdef BELLHOP_3D_STATE
C     Bellhop 3-dim. fields
      _RL myPa_StatScal1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatScal2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatVelU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatVelV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /MYPA_STATE_3D/
     &    myPa_StatScal1, myPa_StatScal2,
     &    myPa_StatVelU,  myPa_StatVelV
#endif /* BELLHOP_3D_STATE */
#ifdef BELLHOP_2D_STATE
C     Bellhop 2-dim. fields
      _RL bellhop_SST (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myPa_Surf2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /MYPA_STATE_2D/
     &    bellhop_SST, myPa_Surf2
#endif /* BELLHOP_2D_STATE */

#endif /* ALLOW_BELLHOP */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
