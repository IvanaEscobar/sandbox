#ifdef ALLOW_IHOP

#ifdef IHOP_3D_STATE
C     Ihop 3-dim. fields
      _RL myPa_VarStatScal1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_VarStatScal2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_VarStatVelU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_VarStatVelV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /IHOP_STATE_3D/
     &    myPa_VarStatScal1, myPa_VarStatScal2,
     &    myPa_VarStatVelU,  myPa_VarStatVelV
#endif /* IHOP_3D_STATE */
#ifdef IHOP_2D_STATE
C     ihop 2-dim. fields
      _RL ihop_SST (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myPa_VarSurf2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /IHOP_STATE_2D/
     &    ihop_SST, myPa_VarSurf2
#endif /* IHOP_2D_STATE */

#endif /* ALLOW_IHOP */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
