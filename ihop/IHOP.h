#ifdef ALLOW_IHOP
!BOP
!     !ROUTINE: IHOP.h
!     !INTERFACE:
!     #include IHOP.h
!
!     !DESCRIPTION:
!     *================================================================*
!     | IHOP.h
!     | o Header file defining "ihop" parameters and variables
!     *================================================================*
!EOP

!     Package flag
      LOGICAL IHOP_MNC
      LOGICAL IHOP_MDSIO
      COMMON /IHOP_PACKAGE/                                                                                                             &
     &                      IHOP_MNC, IHOP_MDSIO 

!     IHOP parameters
!     ===============
!--   COMMON /IHOP_PARAMS_L/ IHOP logical-type parameters:
!     IHOP_bellOn       :: true if bellhop driver needs to run (UNUSED)

      LOGICAL IHOP_bellOn

      COMMON /IHOP_PARAMS_L/                                                                                                             &
     &      IHOP_bellOn

!-- COMMON /IHOP_PARAMS_C/ IHOP Character-type parameters:
!   IHOP_fileroot   :: File name for reading in an environment
!   IHOP_title      :: Title name for writing into output files 
!   IHOP_topopt     :: SSP interpolation, top boundary type
!   IHOP_botopt     :: bottom boundary type
!   IHOP_runopt     :: run type (R/E/A)

      CHARACTER*(100) IHOP_fileroot
      CHARACTER*(100) IHOP_title
      CHARACTER*(6) IHOP_topopt
      CHARACTER*(2) IHOP_botopt
      CHARACTER*(1) IHOP_runopt

      COMMON /IHOP_PARAMS_C/                                                                                                             &
     &      IHOP_fileroot, IHOP_title, IHOP_topopt, IHOP_botopt, IHOP_runopt

!-- COMMON /IHOP_PARAMS_I/ IHOP Integer-type parameters:
!   IHOP_nrays  :: No. of rays to propagate

      INTEGER IHOP_nrays
      INTEGER IHOP_nsd
      INTEGER IHOP_nrd
      INTEGER IHOP_nrr

      COMMON /IHOP_PARAMS_I/                                                                                                            &
     &      IHOP_nsd,                                                                                                                   &
     &      IHOP_nrd, IHOP_nrr,                                                                                                         &
     &      IHOP_nrays

!-- COMMON /IHOP_PARAMS_R/ IHOP Real-type parameters:
!   IHOP_freq           :: frequency (Hz)
!   IHOP_depth          :: depth of bottom (m)
!   IHOP_bcsound        :: bottom sound speed (m/s) 
!   IHOP_bcsoundshear   :: shear bottom sound speed (m/s) 
!   IHOP_bcsoundI       :: IMAG bottom sound speed (m/s) 
!   IHOP_bcsoundshearI  :: IMAG shear bottom sound speed (m/s) 
!   IHOP_brho           :: bottom density (kg/m^3)
!   IHOP_sd             :: source depth (m)
!   IHOP_rd             :: receiver depth (m)
!   IHOP_rr             :: receiver ranges (km)
!   IHOP_alpha          :: bearing launch angles (degrees)
!   IHOP_step           :: step length (m)
!   IHOP_zbox           :: acoustic domain depth (m)
!   IHOP_rbox           :: acoustic domain range (km)

      _RL IHOP_freq
      _RL IHOP_depth
      _RL IHOP_bcsound
      _RL IHOP_bcsoundshear 
      _RL IHOP_bcsoundI
      _RL IHOP_bcsoundshearI 
      _RL IHOP_brho
      _RL IHOP_sd (nsd)
      _RL IHOP_rd (nrd)
      _RL IHOP_rr (nrr)
      _RL IHOP_alpha (2)
      _RL IHOP_step
      _RL IHOP_zbox
      _RL IHOP_rbox

      COMMON /IHOP_PARAMS_R/                                                                                                            &
     &      IHOP_freq, IHOP_depth, IHOP_bcsound, IHOP_bcsoundshear, IHOP_brho,                                                          &
     &      IHOP_bcsoundI, IHOP_bcsoundshearI, IHOP_sd, IHOP_rd,                                                                        &
     &      IHOP_rr, IHOP_alpha, IHOP_step, IHOP_zbox, IHOP_rbox



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

!EH3 ;;; Local Variables: ***
!EH3 ;;; mode:fortran ***
!EH3 ;;; End: ***
