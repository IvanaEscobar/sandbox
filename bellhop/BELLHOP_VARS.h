#ifdef ALLOW_BELLHOP

#ifdef BELLHOP_3D_STATE
C     Bellhop 3-dim. fields
      _RL myPa_VarStatScal1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_VarStatScal2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_VarStatVelU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_VarStatVelV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /BELLHOP_STATE_3D/
     &    myPa_VarStatScal1, myPa_VarStatScal2,
     &    myPa_VarStatVelU,  myPa_VarStatVelV
#endif /* BELLHOP_3D_STATE */
#ifdef BELLHOP_2D_STATE
C     Bellhop 2-dim. fields
      _RL bellhop_SST (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myPa_VarSurf2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /BELLHOP_STATE_2D/
     &    bellhop_SST, myPa_VarSurf2
#endif /* BELLHOP_2D_STATE */

#endif /* ALLOW_BELLHOP */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
