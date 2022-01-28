#ifdef ALLOW_BELLHOP

C     Package flag
      LOGICAL myPa_MNC
      LOGICAL myPa_MDSIO
      COMMON /MYPA_PACKAGE/
     &                     myPa_MNC, myPa_MDSIO

C     BELLHOP parameters
      LOGICAL bellOn
      LOGICAL module_is_initialized
      _RL     mixTS

      LOGICAL myPa_StaV_Cgrid
      LOGICAL myPa_Tend_Cgrid
      LOGICAL myPa_applyTendT
      LOGICAL myPa_applyTendS
      LOGICAL myPa_applyTendU
      LOGICAL myPa_applyTendV

C-    additional parameters:
      LOGICAL myPa_doSwitch1
      LOGICAL myPa_doSwitch2
      INTEGER myPa_index1
      INTEGER myPa_index2
      _RL myPa_param1
      _RL myPa_param2
      CHARACTER*(MAX_LEN_FNAM) myPa_string1
      CHARACTER*(MAX_LEN_FNAM) myPa_string2

C-    file names for initial conditions:
      CHARACTER*(MAX_LEN_FNAM) myPa_Scal1File
      CHARACTER*(MAX_LEN_FNAM) myPa_Scal2File
      CHARACTER*(MAX_LEN_FNAM) myPa_VelUFile
      CHARACTER*(MAX_LEN_FNAM) myPa_VelVFile
      CHARACTER*(MAX_LEN_FNAM) myPa_Surf1File
      CHARACTER*(MAX_LEN_FNAM) myPa_Surf2File

      COMMON /BELLHOP_PARAMS_L/
     &       bellOn, module_is_initialized, 
     &       myPa_StaV_Cgrid, myPa_Tend_Cgrid,
     &       myPa_applyTendT, myPa_applyTendS,
     &       myPa_applyTendU, myPa_applyTendV,
     &       myPa_doSwitch1, myPa_doSwitch2
      COMMON /BELLHOP_PARAMS_I/ myPa_index1, myPa_index2
      COMMON /BELLHOP_PARAMS_R/
     &       mixTS,
     &       myPa_param1, myPa_param2
      COMMON /BELLHOP_PARAMS_C/ myPa_string1, myPa_string2,
     &       myPa_Scal1File, myPa_Scal2File,
     &       myPa_VelUFile,  myPa_VelVFile,
     &       myPa_Surf1File, myPa_Surf2File

#ifdef BELLHOP_3D_STATE
C     BELLHOP 3-dim. fields
      _RL myPa_StatScal1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatScal2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatVelU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatVelV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /BELLHOP_STATE_3D/
     &    myPa_StatScal1, myPa_StatScal2,
     &    myPa_StatVelU,  myPa_StatVelV
#endif /* BELLHOP_3D_STATE */
#ifdef BELLHOP_2D_STATE
C     BELLHOP 2-dim. fields
      _RL myPa_Surf1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myPa_Surf2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /BELLHOP_STATE_2D/
     &    myPa_Surf1, myPa_Surf2
#endif /* BELLHOP_2D_STATE */

#ifdef BELLHOP_TENDENCY
      _RL myPa_TendScal1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_TendScal2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_TendVelU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_TendVelV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /BELLHOP_TENDENCY/
     &    myPa_TendScal1, mypa_TendScal2,
     &    myPa_TendVelU,  mypa_TendVelV
#endif /* BELLHOP_TENDENCY */

#endif /* ALLOW_BELLHOP */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
