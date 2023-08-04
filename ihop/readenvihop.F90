#include "IHOP_OPTIONS.h"
!BOP
! !INTERFACE:
MODULE readEnviHop
! <CONTACT EMAIL="ivana@utexas.edu">
!   Ivana Escobar
! </CONTACT>

  ! mbp 12/2018, based on much older subroutine

  USE ihop_mod,     only: PRTFile, RAYFile, DELFile, ARRFile, SHDFile, &
                        Title, Beam
  USE ssp_mod,      only: EvaluateSSP, HSInfo, Bdry, SSP, zTemp, alphaR, betaR,&
                          alphaI, betaI, rhoR, betaPowerLaw, fT
  USE atten_mod,    only: CRCI, T, Salinity, pH, z_bar, iBio, NBioLayers, bio

! ! USES
  implicit none
!  == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "IHOP_SIZE.h"
#include "IHOP.h"

  PRIVATE

! public interfaces
!=======================================================================

  public    ReadEnvironment, OpenOutputFiles
  
!=======================================================================

CONTAINS
  SUBROUTINE ReadEnvironment( FileRoot, myThid )

    ! Routine to read in and print input data
    ! Note that default values of SSP, DENSITY, Attenuation will not work

    USE angle_mod,  only: ReadRayElevationAngles, ReadRayBearingAngles
    USE srPos_mod,  only: Pos, ReadSxSy, ReadSzRz, ReadRcvrRanges,         &
#ifdef IHOP_THREED
                              ReadRcvrBearings, &
#endif /* IHOP_THREED */
                              ReadFreqVec

    ! == Routine arguments ==
    ! myThid :: Thread number for this instance of the routine.
    CHARACTER*(MAX_LEN_MBUF) :: msgBuf
    INTEGER, INTENT( IN ) :: myThid

    REAL (KIND=_RL90),  PARAMETER   :: c0 = 1500.0
    CHARACTER (LEN=80), INTENT(IN ) :: FileRoot
    INTEGER            :: NMedia, iostat, i
    REAL               :: ZMin, ZMax
    REAL (KIND=_RL90)  :: x( 2 ), c, cimag, gradc( 2 ), crr, crz, czz, rho, &
                          Depth
    CHARACTER (LEN= 2) :: AttenUnit
    CHARACTER (LEN=10) :: PlotType

    WRITE( PRTFile, * ) 'BELLHOP/BELLHOP3D'
    WRITE( PRTFile, * )

    ! Prepend model name to title
#ifdef IHOP_THREED
    WRITE(msgBuf,'(2A)') 'READENVIHOP ReadEnvironment: ', & 
                         '3D not supported in ihop'
    CALL PRINT_ERROR( msgBuf, myThid )
    STOP 'ABNORMAL END: S/R ReadEnvironment'
    Title( 1 :11 ) = 'BELLHOP3D- '
    Title( 12:80 ) = IHOP_title
#else /* IHOP_THREED */
    Title( 1 : 9 ) = 'BELLHOP- '
    Title( 10:80 ) = IHOP_title
#endif /* IHOP_THREED */

    WRITE( PRTFile, * ) Title
    WRITE( PRTFile, '('' frequency = '', G11.4, '' Hz'', / )' ) IHOP_freq

    ! *** Top Boundary ***
    Bdry%Top%HS%Opt = IHOP_topopt
    CALL ReadTopOpt( Bdry%Top%HS%Opt, Bdry%Top%HS%BC, AttenUnit, FileRoot, &
                     myThid )

    IF ( Bdry%Top%HS%BC == 'A' ) WRITE( PRTFile, &
        "( //, '   z (m)     alphaR (m/s)   betaR  rho (g/cm^3)  alphaI     betaI', / )" )

    CALL TopBot( IHOP_freq, AttenUnit, Bdry%Top%HS, myThid )

    ! *** Ocean SSP ***
    Bdry%Bot%HS%Depth = IHOP_depth
    x = [ 0. _d 0, Bdry%Bot%HS%Depth ]   ! tells SSP Depth to read to

    WRITE( PRTFile, * )
    WRITE( PRTFile, FMT = "( ' Depth = ', F10.2, ' m' )" ) Bdry%Bot%HS%Depth
    WRITE( PRTFile, * ) 'Top options: ', Bdry%Top%HS%Opt

       WRITE( msgBuf, * ) 'Escobar: in Readenvi: BEFORE EVALUATESSP'
       CALL PRINT_ERROR( msgBuf, myThid )
    CALL EvaluateSSP( x, c, cimag, gradc, crr, crz, czz, rho, IHOP_freq, 'INI', myThid )
       WRITE( msgBuf, * ) 'Escobar: in Readenvi: AFTER EVALUATESSP'
       CALL PRINT_ERROR( msgBuf, myThid )

    Bdry%Top%HS%Depth = SSP%z( 1 )   ! first SSP point is top depth

    ! *** Bottom Boundary ***
    ! bottom depth should perhaps be set the same way?
    Bdry%Bot%HS%Opt = IHOP_botopt 
    WRITE( PRTFile, * )
    WRITE( PRTFile, * ) 'Bottom options: ', Bdry%Bot%HS%Opt

    SELECT CASE ( Bdry%Bot%HS%Opt( 2 : 2 ) )
    CASE ( '~', '*' )
        WRITE( PRTFile, * ) '    Bathymetry file selected'
    CASE( ' ' )
    CASE DEFAULT
        WRITE(msgBuf,'(2A)') 'READENVIHOP ReadEnvironment: ', & 
                             'Unknown bottom option letter in second position'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R ReadEnvironment'
    END SELECT

    Bdry%Bot%HS%BC = Bdry%Bot%HS%Opt( 1 : 1 )
    CALL TopBot( IHOP_freq, AttenUnit, Bdry%Bot%HS, myThid )

    ! *** source and receiver locations ***

    CALL ReadSxSy( myThid ) ! Read source/receiver x-y coordinates
    ZMin = SNGL( Bdry%Top%HS%Depth )
    ZMax = SNGL( Bdry%Bot%HS%Depth )

    Pos%NSz = IHOP_nsd
    Pos%NRz = IHOP_nrd
    CALL AllocateSR( Pos%NSz, Pos%Sz, IHOP_sd )
    CALL AllocateSR( Pos%NRz, Pos%Rz, IHOP_rd )
    CALL ReadSzRz( ZMin, ZMax, myThid )

    Pos%NRr = IHOP_nrr
    CALL AllocateSR( Pos%NRr, Pos%Rr, IHOP_rr )
    CALL ReadRcvrRanges( myThid )

#ifdef IHOP_THREED
    CALL ReadRcvrBearings( myThid )
#endif /* IHOP_THREED */
    CALL ReadfreqVec( IHOP_freq,  Bdry%Top%HS%Opt( 6:6 ), myThid )

    ! *** run type ***
    Beam%RunType = IHOP_runopt
    CALL ReadRunType( Beam%RunType, PlotType, myThid )

    Depth = Zmax - Zmin   ! water depth
    CALL ReadRayElevationAngles( IHOP_freq, Depth, Bdry%Top%HS%Opt, &
        Beam%RunType, myThid )
#ifdef IHOP_THREED
    CALL ReadRayBearingAngles( IHOP_freq, Bdry%Top%HS%Opt, Beam%RunType, myThid )
#endif /* IHOP_THREED */

    WRITE( PRTFile, * )
    WRITE( PRTFile, * ) '___________________________________________________', &
                        '_______________________'
    WRITE( PRTFile, * )

    ! Limits for tracing beams
#ifdef IHOP_THREED
    WRITE(msgBuf,'(2A)') 'READENVIHOP ReadEnvironment: ', & 
                         '3D not supported in ihop'
    CALL PRINT_ERROR( msgBuf, myThid )
    STOP 'ABNORMAL END: S/R ReadEnvironment'
    !READ(  ENVFile, * ) Beam%deltas, Beam%Box%x, Beam%Box%y, Beam%Box%z
    Beam%Box%x = 1000.0 * Beam%Box%x   ! convert km to m
    Beam%Box%y = 1000.0 * Beam%Box%y   ! convert km to m

    ! Automatic step size selection
    IF ( Beam%deltas == 0.0 ) Beam%deltas = &
        ( Bdry%Bot%HS%Depth - Bdry%Top%HS%Depth ) / 10.0   
    ! WRITE( PRTFile, '('' IHOP_frequency = '', G11.4, '' Hz'', / )' ) IHOP_freq

    WRITE( PRTFile, * )
    WRITE( PRTFile, & 
           fmt = '(  '' Step length,       deltas = '', G11.4, '' m'' )' ) &
         Beam%deltas
    WRITE( PRTFile, * )
    WRITE( PRTFile, &
           fmt = '(  '' Maximum ray x-range, Box%x = '', G11.4, '' m'' )' ) &
         Beam%Box%x
    WRITE( PRTFile, &
           fmt = '(  '' Maximum ray y-range, Box%y = '', G11.4, '' m'' )' ) &
         Beam%Box%y
    WRITE( PRTFile, &
           fmt = '(  '' Maximum ray z-range, Box%z = '', G11.4, '' m'' )' )&
         Beam%Box%z
#else /* IHOP_THREED */
    Beam%deltas = IHOP_step
    Beam%Box%z  = IHOP_zbox
    Beam%Box%r  = IHOP_rbox
    WRITE( PRTFile, * )
    IF ( Beam%deltas == 0.0 ) THEN ! Automatic step size option
        Beam%deltas = ( Bdry%Bot%HS%Depth - Bdry%Top%HS%Depth ) / 10.0   
        WRITE( PRTFile, &
               fmt = '(  '' Step length,       deltas = '', G11.4, '' m (automatic step)'' )' ) & 
             Beam%deltas
    ELSE
         WRITE( PRTFile, &
                fmt = '(  '' Step length,       deltas = '', G11.4, '' m'' )' ) & 
              Beam%deltas
    END IF
    WRITE( PRTFile, * )
    WRITE( PRTFile, &
           fmt = '(  '' Maximum ray depth, Box%z  = '', G11.4, '' m'' )' ) &
         Beam%Box%z
    WRITE( PRTFile, &
           fmt = '(  '' Maximum ray range, Box%r  = '', G11.4, ''km'' )' ) &
         Beam%Box%r

    Beam%Box%r = 1000.0 * Beam%Box%r   ! convert km to m
#endif /* IHOP_THREED */

    ! *** Beam characteristics ***
       Beam%Type( 4 : 4 ) = Beam%RunType( 7 : 7 )   ! selects beam shift option
          
       SELECT CASE ( Beam%Type( 4 : 4 ) )
       CASE ( 'S' )
          WRITE( PRTFile, * ) 'Beam shift in effect'
       CASE DEFAULT
          WRITE( PRTFile, * ) 'No beam shift in effect'
       END SELECT

       ! no worry about the beam type if this is a ray trace run
       IF ( Beam%RunType( 1:1 ) /= 'R' .OR. Beam%RunType( 1:1 ) /= 'E' ) THEN 

       ! Beam%Type( 1 : 1 ) is
       !   'G' or '^' Geometric hat beams in Cartesian coordinates
       !   'g' Geometric hat beams in ray-centered coordinates
       !   'B' Geometric Gaussian beams in Cartesian coordinates
       !   'b' Geometric Gaussian beams in ray-centered coordinates
       !   'S' Simple Gaussian beams
       !   'C' Cerveny Gaussian beams in Cartesian coordinates
       !   'R' Cerveny Gaussian beams in Ray-centered coordinates
       ! Beam%Type( 2 : 2 ) controls the setting of the beam width
       !   'F' space Filling
       !   'M' minimum width
       !   'W' WKB beams
       ! Beam%Type( 3 : 3 ) controls curvature changes on boundary reflections
       !   'D' Double
       !   'S' Single
       !   'Z' Zero
       ! Beam%Type( 4 : 4 ) selects whether beam shifts are implemented on 
       ! boundary reflection
       !   'S' yes
       !   'N' no

       ! Curvature change can cause overflow in grazing case
       ! Suppress by setting BeamType( 3 : 3 ) = 'Z'

       Beam%Type( 1 : 1 ) = Beam%RunType( 2 : 2 )
       SELECT CASE ( Beam%Type( 1 : 1 ) )
! geometric hat beams, geometric Gaussian beams, or simple Gaussian beams
       CASE ( 'G', 'g' , '^', 'B', 'b', 'S' )   
! Cerveny Gaussian Beams; read extra lines to specify the beam options
       CASE ( 'R', 'C' )   
          !READ(  ENVFile, * ) Beam%Type( 2 : 3 ), Beam%epsMultiplier, Beam%rLoop
          WRITE( PRTFile, * )
          WRITE( PRTFile, * )
          WRITE( PRTFile, * ) 'Type of beam = ', Beam%Type( 1 : 1 )

          SELECT CASE ( Beam%Type( 3 : 3 ) )
          CASE ( 'D' )
             WRITE( PRTFile, * ) 'Curvature doubling invoked'
          CASE ( 'Z' )
             WRITE( PRTFile, * ) 'Curvature zeroing invoked'
          CASE ( 'S' )
             WRITE( PRTFile, * ) 'Standard curvature condition'
          CASE DEFAULT
                WRITE(msgBuf,'(2A)') 'READENVIHOP ReadEnvironment: ', & 
                                     'Unknown curvature condition'
                CALL PRINT_ERROR( msgBuf, myThid )
                STOP 'ABNORMAL END: S/R ReadEnvironment'
          END SELECT

          WRITE( PRTFile, * ) 'UNUSED epsMultiplier', Beam%epsMultiplier
          WRITE( PRTFile, * ) 'Range for choosing beam width', Beam%rLoop

          ! Images, windows
          !READ(  ENVFile, * ) Beam%Nimage, Beam%iBeamWindow, Beam%Component
          WRITE( PRTFile, * )
          WRITE( PRTFile, * ) 'Number of images, Nimage  = ', Beam%Nimage
          WRITE( PRTFile, * ) 'Beam windowing parameter  = ', Beam%iBeamWindow
          WRITE( PRTFile, * ) 'Component                 = ', Beam%Component
       CASE DEFAULT
            WRITE(msgBuf,'(2A)') 'READENVIHOP ReadEnvironment: ', & 
                                'Unknown beam type (second letter of run type)'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R ReadEnvironment'
       END SELECT
    END IF

    WRITE( PRTFile, * )
  END SUBROUTINE ReadEnvironment

  !**********************************************************************!

  SUBROUTINE ReadTopOpt( TopOpt, BC, AttenUnit, FileRoot, myThid )

    ! == Routine Arguments ==
    ! myThid :: Thread number for this instance of the routine
    CHARACTER*(MAX_LEN_MBUF) :: msgBuf
    INTEGER, INTENT(IN) :: myThid

    CHARACTER (LEN= 6), INTENT( OUT ) :: TopOpt
    CHARACTER (LEN= 1), INTENT( OUT ) :: BC         ! Boundary condition type
    CHARACTER (LEN= 2), INTENT( OUT ) :: AttenUnit
    CHARACTER (LEN=80), INTENT( IN  ) :: FileRoot
    INTEGER            :: iostat

    TopOpt = IHOP_topopt
    WRITE( PRTFile, * )

    SSP%Type  = TopOpt( 1 : 1 )
    BC        = TopOpt( 2 : 2 )
    AttenUnit = TopOpt( 3 : 4 )
    SSP%AttenUnit = AttenUnit

    ! SSP approximation options
    SELECT CASE ( SSP%Type )
    CASE ( 'N' )
       WRITE( PRTFile, * ) '    N2-linear approximation to SSP'
    CASE ( 'C' )
       WRITE( PRTFile, * ) '    C-linear approximation to SSP'
    CASE ( 'P' )
       WRITE( PRTFile, * ) '    PCHIP approximation to SSP'
    CASE ( 'S' )
       WRITE( PRTFile, * ) '    Spline approximation to SSP'
    CASE ( 'Q' )
       WRITE( PRTFile, * ) '    Quad approximation to SSP'
    CASE ( 'A' )
       WRITE( PRTFile, * ) '    Analytic SSP option'
    CASE DEFAULT
        WRITE(msgBuf,'(2A)') 'READENVIHOP ReadTopOpt: ', & 
                             'Unknown option for SSP approximation'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R ReadTopOpt'
    END SELECT

    ! Attenuation options

    SELECT CASE ( AttenUnit( 1 : 1 ) )
    CASE ( 'N' )
       WRITE( PRTFile, * ) '    Attenuation units: nepers/m'
    CASE ( 'F' )
       WRITE( PRTFile, * ) '    Attenuation units: dB/mkHz'
    CASE ( 'M' )
       WRITE( PRTFile, * ) '    Attenuation units: dB/m'
    CASE ( 'W' )
       WRITE( PRTFile, * ) '    Attenuation units: dB/wavelength'
    CASE ( 'Q' )
       WRITE( PRTFile, * ) '    Attenuation units: Q'
    CASE ( 'L' )
       WRITE( PRTFile, * ) '    Attenuation units: Loss parameter'
    CASE DEFAULT
        WRITE(msgBuf,'(2A)') 'READENVIHOP ReadTopOpt: ', & 
                             'Unknown attenuation units'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R ReadTopOpt'
    END SELECT

    ! optional addition of volume attenuation using standard formulas

    SELECT CASE ( AttenUnit( 2 : 2 ) )
    CASE ( 'T' )
       WRITE( PRTFile, * ) '    THORP volume attenuation added'
    CASE ( 'F' )
       WRITE( PRTFile, * ) '    Francois-Garrison volume attenuation added'
       !READ(  ENVFile, * ) T, Salinity, pH, z_bar
       WRITE( PRTFile, &
              "( ' T = ', G11.4, 'degrees   S = ', G11.4, ' psu   pH = ', G11.4, ' z_bar = ', G11.4, ' m' )" ) &
            T, Salinity, pH, z_bar
    CASE ( 'B' )
       WRITE( PRTFile, * ) '    Biological attenaution'
       !READ( ENVFile, *  ) NBioLayers
       WRITE( PRTFile, * ) '      Number of Bio Layers = ', NBioLayers

       DO iBio = 1, NBioLayers
          !READ( ENVFile, *  ) bio( iBio )%Z1, bio( iBio )%Z2, bio( iBio )%f0, &
          !                    bio( iBio )%Q, bio( iBio )%a0
          WRITE( PRTFile, * ) '      Top    of layer = ', bio( iBio )%Z1, ' m'
          WRITE( PRTFile, * ) '      Bottom of layer = ', bio( iBio )%Z2, ' m'
          WRITE( PRTFile, * ) '      Resonance frequency = ', bio( iBio )%f0, &
                              ' Hz'
          WRITE( PRTFile, * ) '      Q  = ', bio( iBio )%Q
          WRITE( PRTFile, * ) '      a0 = ', bio( iBio )%a0
       END DO
    CASE ( ' ' )
    CASE DEFAULT
        WRITE(msgBuf,'(2A)') 'READENVIHOP ReadTopOpt: ', & 
                             'Unknown top option letter in fourth position'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R ReadTopOpt'
    END SELECT

    SELECT CASE ( TopOpt( 5 : 5 ) )
    CASE ( '~', '*' )
       WRITE( PRTFile, * ) '    Altimetry file selected'
    CASE ( '-', '_', ' ' )
    CASE DEFAULT
        WRITE(msgBuf,'(2A)') 'READENVIHOP ReadTopOpt: ', & 
                             'Unknown top option letter in fifth position'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R ReadTopOpt'
    END SELECT

    SELECT CASE ( TopOpt( 6 : 6 ) )
    CASE ( 'I' )
       WRITE( PRTFile, * ) '    Development options enabled'
    CASE ( ' ' )
    CASE DEFAULT
        WRITE(msgBuf,'(2A)') 'READENVIHOP ReadTopOpt: ', & 
                             'Unknown top option letter in sixth position'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R ReadTopOpt'
    END SELECT

  END SUBROUTINE ReadTopOpt

  !**********************************************************************!

  SUBROUTINE ReadRunType( RunType, PlotType, myThid )

    ! Read the RunType variable and print to .prt file

    USE srPos_mod, only: Pos

    ! == Routine Arguments ==
    ! myThid :: Thread number for this instance of the routine
    CHARACTER*(MAX_LEN_MBUF) :: msgBuf
    INTEGER, INTENT(IN) :: myThid

    CHARACTER (LEN= 7), INTENT( INOUT ) :: RunType
    CHARACTER (LEN=10), INTENT( OUT ) :: PlotType

    WRITE( PRTFile, * )

    SELECT CASE ( RunType( 1 : 1 ) )
    CASE ( 'R' )
       WRITE( PRTFile, * ) 'Ray trace run'
    CASE ( 'E' )
       WRITE( PRTFile, * ) 'Eigenray trace run'
    CASE ( 'I' )
       WRITE( PRTFile, * ) 'Incoherent TL calculation'
    CASE ( 'S' )
       WRITE( PRTFile, * ) 'Semi-coherent TL calculation'
    CASE ( 'C' )
       WRITE( PRTFile, * ) 'Coherent TL calculation'
    CASE ( 'A' )
       WRITE( PRTFile, * ) 'Arrivals calculation, ASCII  file output'
    CASE ( 'a' )
       WRITE( PRTFile, * ) 'Arrivals calculation, binary file output'
    CASE DEFAULT
        WRITE(msgBuf,'(2A)') 'READENVIHOP ReadRunType: ', & 
            'Unknown RunType selected'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R ReadRunType'
    END SELECT

    SELECT CASE ( RunType( 2 : 2 ) )
    CASE ( 'C' )
       WRITE( PRTFile, * ) 'Cartesian beams'
    CASE ( 'R' )
       WRITE( PRTFile, * ) 'Ray centered beams'
    CASE ( 'S' )
       WRITE( PRTFile, * ) 'Simple gaussian beams'
    CASE ( 'b' )
       WRITE( PRTFile, * ) 'Geometric gaussian beams in ray-centered coordinates'
    CASE ( 'B' )
       WRITE( PRTFile, * ) 'Geometric gaussian beams in Cartesian coordinates'
    CASE ( 'g' )
       WRITE( PRTFile, * ) 'Geometric hat beams in ray-centered coordinates'
    CASE DEFAULT
       RunType( 2 : 2 ) = 'G'
       WRITE( PRTFile, * ) 'Geometric hat beams in Cartesian coordinates'
    END SELECT

    SELECT CASE ( RunType( 4 : 4 ) )
    CASE ( 'R' )
       WRITE( PRTFile, * ) 'Point source (cylindrical coordinates)'
    CASE ( 'X' )
       WRITE( PRTFile, * ) 'Line source (Cartesian coordinates)'
    CASE DEFAULT
       RunType( 4 : 4 ) = 'R'
       WRITE( PRTFile, * ) 'Point source (cylindrical coordinates)'
    END SELECT

    SELECT CASE ( RunType( 5 : 5 ) )
    CASE ( 'R' )
       WRITE( PRTFile, * ) 'Rectilinear receiver grid: Receivers at', &
                           ' ( Rr( ir ), Rz( ir ) ) )'
       PlotType = 'rectilin  '
    CASE ( 'I' )
       WRITE( PRTFile, * ) 'Irregular grid: Receivers at Rr( : ) x Rz( : )'
       IF ( Pos%NRz /= Pos%NRr ) THEN
            WRITE(msgBuf,'(2A)') 'READENVIHOP ReadRunType: ', & 
                    'Irregular grid option selected with NRz not equal to Nr'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R ReadRunType'
       END IF
       PlotType = 'irregular '
    CASE DEFAULT
       WRITE( PRTFile, * ) 'Rectilinear receiver grid: Receivers at', &
                           ' Rr( : ) x Rz( : )'
       RunType( 5 : 5 ) = 'R'
       PlotType = 'rectilin  '
    END SELECT

    SELECT CASE ( RunType( 6 : 6 ) )
    CASE ( '2' )
       WRITE( PRTFile, * ) 'N x 2D calculation (neglects horizontal refraction)'
    CASE ( '3' )
       WRITE( PRTFile, * ) '3D calculation'
    CASE DEFAULT
       RunType( 6 : 6 ) = '2'
    END SELECT

  END SUBROUTINE ReadRunType

  !**********************************************************************!

  SUBROUTINE TopBot( freq, AttenUnit, HS, myThid )

    ! Handles top and bottom boundary conditions

    ! == Routine Arguments ==
    ! myThid :: Thread number for this instance of the routine
    CHARACTER*(MAX_LEN_MBUF) :: msgBuf
    INTEGER, INTENT(IN) :: myThid

    REAL (KIND=_RL90), INTENT( IN    ) :: freq  ! frequency
    CHARACTER (LEN=2), INTENT( IN    ) :: AttenUnit
    TYPE ( HSInfo ),   INTENT( INOUT ) :: HS
    REAL (KIND=_RL90) :: Mz, vr, alpha2_f     ! values related to grain size

    ! Echo to PRTFile user's choice of boundary condition

    SELECT CASE ( HS%BC )
    CASE ( 'V' )
       WRITE( PRTFile, * ) '    Surface modeled as a VACUUM'
    CASE ( 'R' )
       WRITE( PRTFile, * ) '    Perfectly RIGID'
    CASE ( 'A' )
       WRITE( PRTFile, * ) '    ACOUSTO-ELASTIC half-space'
    CASE ( 'G' )
       WRITE( PRTFile, * ) '    Grain size to define half-space'
    CASE ( 'F' )
       WRITE( PRTFile, * ) '    FILE used for reflection loss'
    CASE ( 'W' )
       WRITE( PRTFile, * ) '    Writing an IRC file'
    CASE ( 'P' )
       WRITE( PRTFile, * ) '    reading PRECALCULATED IRC'
    CASE DEFAULT
        WRITE(msgBuf,'(2A)') 'READENVIHOP TopBot: ', & 
                             'Unknown boundary condition type'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R TopBot'
    END SELECT

    ! ****** Read in BC parameters depending on particular choice ******

    HS%cp  = 0.0
    HS%cs  = 0.0
    HS%rho = 0.0

    SELECT CASE ( HS%BC )
    CASE ( 'A' )                  ! *** Half-space properties ***
       ! IEsco23: MISSING IF BOTTOM BC CHECK
       zTemp    = IHOP_depth
       alphaR   = IHOP_bcsound
       betaR    = IHOP_bcsoundshear
       rhoR     = IHOP_brho
       alphaI   = IHOP_bcsoundI
       betaI    = IHOP_bcsoundshearI
       WRITE( PRTFile, FMT = "( F10.2, 3X, 2F10.2, 3X, F6.2, 3X, 2F10.4 )" ) &
            zTemp, alphaR, betaR, rhoR, alphaI, betaI
       ! dummy parameters for a layer with a general power law for attenuation
       ! these are not in play because the AttenUnit for this is not allowed yet
       !freq0         = freq
       betaPowerLaw  = 1.0
       ft            = 1000.0

       HS%cp  = CRCI( zTemp, alphaR, alphaI, freq, freq, AttenUnit, &
                      betaPowerLaw, ft, myThid )
       HS%cs  = CRCI( zTemp, betaR,  betaI,  freq, freq, AttenUnit, &
                      betaPowerLaw, ft, myThid )

       HS%rho = rhoR
    CASE ( 'G' )            ! *** Grain size (formulas from UW-APL HF Handbook)
       ! These formulas are from the UW-APL Handbook
       ! The code is taken from older Matlab and is unnecesarily verbose
       ! vr   is the sound speed ratio
       ! rhor is the density ratio
       !READ(  ENVFile, *    ) zTemp, Mz
       WRITE( PRTFile, FMT = "( F10.2, 3X, F10.2 )" ) zTemp, Mz

       IF ( Mz >= -1 .AND. Mz < 1 ) THEN
          vr   = 0.002709 * Mz**2 - 0.056452 * Mz + 1.2778
          rhor = 0.007797 * Mz**2 - 0.17057  * Mz + 2.3139
       ELSE IF ( Mz >= 1 .AND. Mz < 5.3 ) THEN
          vr   = -0.0014881 * Mz**3 + 0.0213937 * Mz**2 - 0.1382798 * Mz &
               + 1.3425
          rhor = -0.0165406 * Mz**3 + 0.2290201 * Mz**2 - 1.1069031 * Mz &
               + 3.0455
       ELSE
          vr   = -0.0024324 * Mz + 1.0019
          rhor = -0.0012973 * Mz + 1.1565
       END IF

       IF ( Mz >= -1 .AND. Mz < 0 ) THEN
          alpha2_f = 0.4556
       ELSE IF ( Mz >= 0 .AND. Mz < 2.6 ) THEN
          alpha2_f = 0.4556 + 0.0245 * Mz
       ELSE IF( Mz >= 2.6 .AND. Mz < 4.5 ) THEN
          alpha2_f = 0.1978 + 0.1245 * Mz
       ELSE IF( Mz >= 4.5 .AND. Mz < 6.0 ) THEN
          alpha2_f = 8.0399 - 2.5228 * Mz + 0.20098 * Mz ** 2
       ELSE IF( Mz >= 6.0 .AND. Mz < 9.5 ) THEN
          alpha2_f = 0.9431 - 0.2041 * Mz + 0.0117 * Mz ** 2
       ELSE
          alpha2_f =  0.0601
       END IF

       ! AttenUnit = 'L'   ! loss parameter
!!! following uses a reference sound speed of 1500 ???
!!! should be sound speed in the water, just above the sediment
       ! the term vr / 1000 converts vr to units of m per ms 
       alphaR = vr * 1500.0
       ! loss parameter Sect. IV., Eq. (4) of handbook
       alphaI = alpha2_f * ( vr / 1000 ) * 1500.0 * log( 10.0 ) / ( 40.0 * PI )

       HS%cp  = CRCI( zTemp, alphaR, alphaI, freq, freq, 'L ', &
                      betaPowerLaw, ft, myThid )
       HS%cs  = 0.0
       HS%rho = rhoR
       WRITE( PRTFile, &
              FMT = "( 'Converted sound speed =', 2F10.2, 3X, 'density = ', F10.2, 3X, 'loss parm = ', F10.4 )" ) &
            HS%cp, rhor, alphaI
    END SELECT

  END SUBROUTINE TopBot

  ! **********************************************************************!

  SUBROUTINE OpenOutputFiles( FileRoot )
    ! Write appropriate header information

    USE angle_mod,  only: Angles
    USE srPos_mod,  only: Pos

    CHARACTER (LEN=80), INTENT( IN ) :: FileRoot
    REAL               :: atten
    CHARACTER (LEN=10) :: PlotType

    SELECT CASE ( Beam%RunType( 1 : 1 ) )
    CASE ( 'R', 'E' )   ! Ray trace or Eigenrays
       OPEN ( FILE = TRIM( FileRoot ) // '.ray', UNIT = RAYFile, &
              FORM = 'FORMATTED' )
       WRITE( RAYFile, * ) '''', Title( 1 : 50 ), ''''
       WRITE( RAYFile, * ) IHOP_freq
       WRITE( RAYFile, * ) Pos%NSx, Pos%NSy, Pos%NSz
       WRITE( RAYFile, * ) Angles%Nalpha, Angles%Nbeta
       WRITE( RAYFile, * ) Bdry%Top%HS%Depth
       WRITE( RAYFile, * ) Bdry%Bot%HS%Depth

#ifdef IHOP_THREED
       WRITE( RAYFile, * ) '''xyz'''
#else /* IHOP_THREED */
       WRITE( RAYFile, * ) '''rz'''
#endif /* IHOP_THREED */

    CASE ( 'A' )        ! arrival file in ascii format
       OPEN ( FILE = TRIM( FileRoot ) // '.arr', UNIT = ARRFile, &
              FORM = 'FORMATTED' )

# ifdef IHOP_THREED
       WRITE( ARRFile, * ) '''3D'''
# else /* IHOP_THREED */
       WRITE( ARRFile, * ) '''2D'''
# endif /* IHOP_THREED */

       WRITE( ARRFile, * ) IHOP_freq

       ! write source locations
# ifdef IHOP_THREED
       WRITE( ARRFile, * ) Pos%NSx,    Pos%Sx(    1 : Pos%NSx )
       WRITE( ARRFile, * ) Pos%NSy,    Pos%Sy(    1 : Pos%NSy )
       WRITE( ARRFile, * ) Pos%NSz,    Pos%Sz(    1 : Pos%NSz )
# else /* IHOP_THREED */
       WRITE( ARRFile, * ) Pos%NSz,    Pos%Sz(    1 : Pos%NSz )
# endif /* IHOP_THREED */

       ! write receiver locations
       WRITE( ARRFile, *    ) Pos%NRz,    Pos%Rz(    1 : Pos%NRz )
       WRITE( ARRFile, *    ) Pos%NRr,    Pos%Rr(    1 : Pos%NRr )
# ifdef IHOP_THREED
       WRITE( ARRFile, * ) Pos%Ntheta, Pos%theta( 1 : Pos%Ntheta )
# endif /* IHOP_THREED */

       ! IEsco22: add to arrivals output
       OPEN ( FILE = TRIM( FileRoot ) // '.ray', UNIT = RAYFile, &
              FORM = 'FORMATTED' )
       WRITE( RAYFile, * ) '''', Title( 1 : 50 ), ''''
       WRITE( RAYFile, * ) IHOP_freq
       WRITE( RAYFile, * ) Pos%NSx, Pos%NSy, Pos%NSz
       WRITE( RAYFile, * ) Angles%Nalpha, Angles%Nbeta
       WRITE( RAYFile, * ) Bdry%Top%HS%Depth
       WRITE( RAYFile, * ) Bdry%Bot%HS%Depth

# ifdef IHOP_THREED
       WRITE( RAYFile, * ) '''xyz'''
# else /* IHOP_THREED */
       WRITE( RAYFile, * ) '''rz'''
# endif /* IHOP_THREED */

       OPEN ( FILE = TRIM( FileRoot ) // '.delay', UNIT = DELFile, &
              FORM = 'FORMATTED' )
       WRITE( DELFile, * ) '''', Title( 1 : 50 ), ''''
       WRITE( DELFile, * ) IHOP_freq
       WRITE( DELFile, * ) Pos%NSx, Pos%NSy, Pos%NSz
       WRITE( DELFile, * ) Angles%Nalpha, Angles%Nbeta
       WRITE( DELFile, * ) Bdry%Top%HS%Depth
       WRITE( DELFile, * ) Bdry%Bot%HS%Depth

#ifdef IHOP_THREED
       WRITE( DELFile, * ) '''xyz'''
# else /* IHOP_THREED */
       WRITE( DELFile, * ) '''rz'''
# endif /* IHOP_THREED */
    CASE ( 'a' )        ! arrival file in binary format
       OPEN ( FILE = TRIM( FileRoot ) // '.arr', UNIT = ARRFile, &
              FORM = 'UNFORMATTED' )

# ifdef IHOP_THREED
       WRITE( ARRFile ) '''3D'''
# else /* IHOP_THREED */
       WRITE( ARRFile ) '''2D'''
# endif /* IHOP_THREED */

       WRITE( ARRFile    ) SNGL( IHOP_freq )

       ! write source locations
# ifdef IHOP_THREED
       WRITE( ARRFile    ) Pos%NSx,    Pos%Sx(    1 : Pos%NSx )
       WRITE( ARRFile    ) Pos%NSy,    Pos%Sy(    1 : Pos%NSy )
       WRITE( ARRFile    ) Pos%NSz,    Pos%Sz(    1 : Pos%NSz )
# else /* IHOP_THREED */
       WRITE( ARRFile    ) Pos%NSz,    Pos%Sz(    1 : Pos%NSz )
# endif /* IHOP_THREED */

       ! write receiver locations
       WRITE( ARRFile       ) Pos%NRz,    Pos%Rz(    1 : Pos%NRz )
       WRITE( ARRFile       ) Pos%NRr,    Pos%Rr(    1 : Pos%NRr )
# ifdef IHOP_THREED
       WRITE( ARRFile    ) Pos%Ntheta, Pos%theta( 1 : Pos%Ntheta )
# endif /* IHOP_THREED */

    CASE DEFAULT
       atten = 0.0

       ! following to set PlotType has alread been done in READIN if that was 
       ! used for input
       SELECT CASE ( Beam%RunType( 5 : 5 ) )
       CASE ( 'R' )
          PlotType = 'rectilin  '
       CASE ( 'I' )
          PlotType = 'irregular '
       CASE DEFAULT
          PlotType = 'rectilin  '
       END SELECT

       CALL WriteSHDHeader( TRIM( FileRoot ) // '.shd', Title, REAL( IHOP_freq ), &
                         atten, PlotType )
    END SELECT

  END SUBROUTINE OpenOutputFiles

  !**********************************************************************!

  SUBROUTINE WriteSHDHeader( FileName, Title, freq0, Atten, PlotType )

    USE srPos_mod,  only: Pos, Nfreq, freqVec

    ! Write header to disk file

    REAL,      INTENT( IN ) :: freq0, Atten      ! Nominal frequency, stabilizing attenuation (for wavenumber integration only)
    CHARACTER, INTENT( IN ) :: FileName*( * )    ! Name of the file (could be a shade file or a Green's function file)
    CHARACTER, INTENT( IN ) :: Title*( * )       ! Arbitrary title
    CHARACTER, INTENT( IN ) :: PlotType*( 10 )   ! 
    INTEGER :: LRecl

    ! receiver bearing angles
    IF ( .NOT. ALLOCATED( Pos%theta ) ) THEN
       ALLOCATE( Pos%theta( 1 ) )
       Pos%theta( 1 ) = 0   ! dummy bearing angle
       Pos%Ntheta     = 1
    END IF

    ! source x-coordinates
    IF ( .NOT. ALLOCATED( Pos%Sx ) ) THEN
       ALLOCATE( Pos%Sx( 1 ) )
       Pos%sx( 1 ) = 0      ! dummy x-coordinate
       Pos%NSx     = 1
    END IF

    ! source y-coordinates
    IF ( .NOT. ALLOCATED( Pos%Sy ) ) THEN
       ALLOCATE( Pos%Sy( 1 ) )
       Pos%sy( 1 ) = 0      ! dummy y-coordinate
       Pos%NSy     = 1
    END IF

    IF ( PlotType( 1 : 2 ) /= 'TL' ) THEN
       ! MAX( 41, ... ) below because Title is already 40 words (or 80 bytes)
 ! words/record (NRr doubled for complex pressure storage)
       LRecl = MAX( 41, 2 * Nfreq, Pos%Ntheta, Pos%NSx, Pos%NSy, Pos%NSz, &
                    Pos%NRz, 2 * Pos%NRr )  

       OPEN ( FILE = FileName, UNIT = SHDFile, STATUS = 'REPLACE', &
              ACCESS = 'DIRECT', RECL = 4 * LRecl, FORM = 'UNFORMATTED')
       WRITE( SHDFile, REC = 1  ) LRecl, Title( 1 : 80 )
       WRITE( SHDFile, REC = 2  ) PlotType
       WRITE( SHDFile, REC = 3  ) Nfreq, Pos%Ntheta, Pos%NSx, Pos%NSy, Pos%NSz,& 
                                  Pos%NRz, Pos%NRr, freq0, atten
       WRITE( SHDFile, REC = 4  ) freqVec(   1 : Nfreq )
       WRITE( SHDFile, REC = 5  ) Pos%theta( 1 : Pos%Ntheta )

       WRITE( SHDFile, REC = 6  ) Pos%Sx( 1 : Pos%NSx )
       WRITE( SHDFile, REC = 7  ) Pos%Sy( 1 : Pos%NSy )
       WRITE( SHDFile, REC = 8  ) Pos%Sz( 1 : Pos%NSz )

       WRITE( SHDFile, REC = 9  ) Pos%Rz( 1 : Pos%NRz )
       WRITE( SHDFile, REC = 10 ) Pos%Rr( 1 : Pos%NRr )

    ELSE   ! compressed format for TL from FIELD3D
  ! words/record (NR doubled for complex pressure storage)
       LRecl = MAX( 41, 2 * Nfreq, Pos%Ntheta, Pos%NSz, Pos%NRz, 2 * Pos%NRr ) 

       OPEN ( FILE = FileName, UNIT = SHDFile, STATUS = 'REPLACE', &
              ACCESS = 'DIRECT', RECL = 4 * LRecl, FORM = 'UNFORMATTED')
       WRITE( SHDFile, REC = 1  ) LRecl, Title( 1 : 80 )
       WRITE( SHDFile, REC = 2  ) PlotType
       WRITE( SHDFile, REC = 3  ) Nfreq, Pos%Ntheta, Pos%NSx, Pos%NSy, Pos%NSz,& 
                                  Pos%NRz, Pos%NRr, freq0, atten
       WRITE( SHDFile, REC = 4  ) freqVec(   1 : Nfreq )
       WRITE( SHDFile, REC = 5  ) Pos%theta( 1 : Pos%Ntheta )

       WRITE( SHDFile, REC = 6  ) Pos%Sx( 1 ), Pos%Sx( Pos%NSx )
       WRITE( SHDFile, REC = 7  ) Pos%Sy( 1 ), Pos%Sy( Pos%NSy )
       WRITE( SHDFile, REC = 8  ) Pos%Sz( 1 : Pos%NSz )

       WRITE( SHDFile, REC = 9  ) Pos%Rz( 1 : Pos%NRz )
       WRITE( SHDFile, REC = 10 ) Pos%Rr( 1 : Pos%NRr )
    END IF

  END SUBROUTINE WriteSHDHeader

  !**********************************************************************!

  SUBROUTINE WriteSHDField( P, NRz, NRr, IRec )

    ! Write the field to disk

    INTEGER, INTENT( IN )    :: NRz, NRr      ! # of receiver depths, ranges
    COMPLEX, INTENT( IN )    :: P( NRz, NRr ) ! Pressure field
    INTEGER, INTENT( INOUT ) :: iRec          ! last record read
    INTEGER                  :: iRz

    DO iRz = 1, NRz
       iRec = iRec + 1
       WRITE( SHDFile, REC = iRec ) P( iRz, : )
    END DO

  END SUBROUTINE WriteSHDField

  !**********************************************************************!

  SUBROUTINE AllocateSR( Nx, x_out, x_in )

    ! Allocate and populate Pos structure from data.ihop

    INTEGER,          INTENT( IN  ) :: Nx    
    REAL(KIND=_RL90), INTENT( IN  ) :: x_in(:)
    REAL(KIND=_RL90), ALLOCATABLE, INTENT( OUT ) :: x_out(:)
    INTEGER                         :: i

    IF ( ALLOCATED(x_out) ) DEALLOCATE(x_out)
    ALLOCATE( x_out(MAX(3, Nx)) )
    x_out(3) = -999.9

    DO i = 1, Nx
        x_out(i) = x_in(i)
    END DO

  END SUBROUTINE AllocateSR

  !**********************************************************************!

END MODULE readEnviHop
