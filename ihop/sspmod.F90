#include "IHOP_OPTIONS.h"
!BOP
! !INTERFACE:
MODULE sspMod
! <CONTACT EMAIL="ivana@utexas.edu">
!   Ivana Escobar
! </CONTACT>

  ! Holds SSP input by user and associated variables

  ! This module is very similar to the one used by the other programs in the 
  ! Acoustics Toolbox. However, it returns the SSP *and* its derivatives

  ! Also, a greater premium has been placed on returning this info quickly, 
  ! since BELLHOP calls it at every step so more information is pre-computed

  USE ihop_fatalError,   only: ERROUT
  USE splinec,      only: cspline, splineall
  USE iHopParams,   only: PRTFile, SSPFile

  IMPLICIT NONE
! == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "IHOP_SIZE.h"
#include "IHOP.h"

  PRIVATE

! public interfaces
!=======================================================================

    public EvaluateSSP, HSInfo, Bdry, SSP, zTemp, alphaR, betaR, alphaI, &
           betaI, rhoR, betaPowerLaw, fT, iSegz, iSegr

!=======================================================================

! INPUT/OUTPUT PARAMETERS:

! LOCAL VARIABLES
! == Local Variables ==
  CHARACTER*(MAX_LEN_MBUF) msgBuf
  INTEGER bi,bj
  INTEGER i,j,k

! LEGACY VARIABLES
! == Legacy Local Variables ==
  INTEGER, PARAMETER     :: MaxSSP = 20001
  INTEGER                :: iSegr = 1, iSegx = 1, iSegy = 1, iSegz = 1
  INTEGER                :: iostat, iallocstat
  INTEGER,           PRIVATE :: iz
  REAL (KIND=_RL90), PRIVATE :: Depth, W
  REAL (KIND=_RL90)          :: zTemp, betaPowerLaw = 1, fT = 1D20
  ! DEFAULT values, BELLHOP only uses alphaR
  REAL (KIND=_RL90)          :: alphaR = 1500, betaR = 0, alphaI = 0, &
                                betaI = 0, rhoR = 1
                            
! TYPE STRUCTURES
! == Type Structures ==
  TYPE rxyz_vector
    REAL (KIND=_RL90), ALLOCATABLE :: r(:), x(:), y(:), z(:)
  END TYPE rxyz_vector

  ! SSP
  TYPE SSPStructure
    INTEGER                 :: NPts, Nr, Nx, Ny, Nz
    REAL    (KIND=_RL90)    :: z( MaxSSP ), rho( MaxSSP )
    COMPLEX (KIND=_RL90)    :: c( MaxSSP ), cz( MaxSSP ), n2( MaxSSP ), &
                               n2z( MaxSSP ), cSpline( 4, MaxSSP )
    REAL    (KIND=_RL90), ALLOCATABLE   :: cMat( :, : ),     czMat( :, : ), &
                                           cMat3( :, :, : ), czMat3( :, :, : )
    TYPE ( rxyz_vector ) :: Seg
    CHARACTER (LEN=1)    :: Type
    CHARACTER (LEN=2)    :: AttenUnit
    ! for PCHIP coefs.
    COMPLEX (KIND=_RL90)    :: cCoef( 4, MaxSSP ), CSWork( 4, MaxSSP )   
  END TYPE SSPStructure

  TYPE( SSPStructure ) :: SSP

  ! *** Halfspace properties structure ***

  TYPE HSInfo
     ! compressional and shear wave speeds/attenuations in user units
     REAL   (KIND=_RL90)    :: alphaR, alphaI, betaR, betaI    
     REAL   (KIND=_RL90)    :: rho, Depth        ! density, depth
     COMPLEX(KIND=_RL90)    :: cP, cS            ! P-wave, S-wave speeds
     CHARACTER (LEN=1)      :: BC                ! Boundary condition type
     CHARACTER (LEN=6)      :: Opt
  END TYPE HSInfo

  TYPE BdryPt
     TYPE( HSInfo )   :: HS
  END TYPE

  TYPE BdryType
     TYPE( BdryPt )   :: Top, Bot
  END TYPE BdryType

  TYPE(BdryType) :: Bdry
!EOP

CONTAINS
  SUBROUTINE EvaluateSSP( x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, &
      myThid )

    ! Call the particular profile routine indicated by the SSP%Type and 
    ! perform Task
    !   Task = 'TAB' to tabulate cp, cs, rhoT 
    !   Task = 'INI' to initialize

    ! == Routine Arguments ==
    ! myThid :: Thread number for this instance of the routine
    INTEGER, INTENT(IN) :: myThid

    ! == Local Variables ==
    REAL (KIND=_RL90), INTENT( IN  ) :: freq
    REAL (KIND=_RL90), INTENT( IN  ) :: x( 2 )  ! r-z SSP evaluation point
    CHARACTER( LEN=3), INTENT( IN  ) :: Task
    REAL (KIND=_RL90), INTENT( OUT ) :: c, cimag, gradc( 2 ), crr, crz, czz, rho
    REAL (KIND=_RL90)                :: gradc_3d( 3 ), cxx, cyy, cxy, cxz, cyz
    REAL (KIND=_RL90)                :: x3( 3 )

    SELECT CASE ( SSP%Type )
    CASE ( 'N' )  !  N2-linear profile option
       CALL n2Linear( x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, myThid  )
    CASE ( 'C' )  !  C-linear profile option
       CALL cLinear(  x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, myThid  )
    CASE ( 'P' )  !  monotone PCHIP ACS profile option
       CALL cPCHIP(   x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, myThid  )
    CASE ( 'S' )  !  Cubic spline profile option
       CALL cCubic(   x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, myThid  )
    CASE ( 'Q' )
       CALL Quad(     x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, myThid  )
    CASE ( 'A' )  !  Analytic profile option
       CALL Analytic( x, c, cimag, gradc, crr, crz, czz, rho, Task, myThid  )
    CASE DEFAULT
       WRITE( PRTFile, * ) 'Profile option: ', SSP%Type
       CALL ERROUT( 'EvaluateSSP', 'Invalid SSP profile option' )
    END SELECT

  END SUBROUTINE EvaluateSSP
  
!**********************************************************************!

  SUBROUTINE n2Linear( x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, myThid  )

    ! N2-linear interpolation of SSP data

    ! == Routine Arguments ==
    ! myThid :: Thread number for this instance of the routine
    INTEGER, INTENT(IN) :: myThid

    ! == Local Variables ==
    REAL (KIND=_RL90), INTENT( IN  ) :: freq
    REAL (KIND=_RL90), INTENT( IN  ) :: x( 2 )  ! r-z SSP evaluation point
    CHARACTER (LEN=3), INTENT( IN  ) :: Task
    REAL (KIND=_RL90), INTENT( OUT ) :: c, cimag, gradc( 2 ), crr, crz, czz, &
                                        rho ! sound speed and its derivatives
    
    IF ( Task == 'INI' ) THEN   ! read in SSP data
       ! *** Task 'INI' for initialization ***

       Depth = x( 2 )
       CALL ReadSSP( Depth, freq )
              
       SSP%n2(  1 : SSP%NPts ) = 1.0 / SSP%c( 1 : SSP%NPts )**2
       !IEsco23 Test this: SSP%n2(  1 : SSP%Nz ) = 1.0 / SSP%c( 1 : SSP%Nz )**2

       ! compute gradient, n2z
       DO iz = 2, SSP%Npts
          SSP%n2z( iz - 1 ) = ( SSP%n2(   iz ) - SSP%n2(   iz - 1 ) ) / &
                              ( SSP%z(    iz ) - SSP%z(    iz - 1 ) )
       END DO
    ELSE                         ! return SSP info

       IF ( x( 2 ) < SSP%z( iSegz ) .OR. x( 2 ) > SSP%z( iSegz + 1 ) ) THEN
          DO iz = 2, SSP%NPts   ! Search for bracketting Depths
!IEsco23 Test this: 
!          DO iz = 2, SSP%Nz   ! Search for bracketting Depths
             IF ( x( 2 ) < SSP%z( iz ) ) THEN
                iSegz = iz - 1
                EXIT
             END IF
          END DO
       END IF

       W = ( x( 2 ) - SSP%z( iSegz ) ) / ( SSP%z( iSegz + 1 ) - SSP%z( iSegz ) )

       c     = REAL(  1.0D0 / SQRT( ( 1.0D0 - W ) * SSP%n2( iSegz ) &
               + W * SSP%n2( iSegz + 1 ) ) )
       cimag = AIMAG( 1.0D0 / SQRT( ( 1.0D0 - W ) * SSP%n2( iSegz ) &
               + W * SSP%n2( iSegz + 1 ) ) )

       gradc = [ 0.0D0, -0.5D0 * c * c * c * REAL( SSP%n2z( iSegz ) ) ]
       crr   = 0.0d0
       crz   = 0.0d0
       czz   = 3.0d0 * gradc( 2 ) * gradc( 2 ) / c

       rho   = ( 1.0D0 - W ) * SSP%rho( iSegz ) + W * SSP%rho( iSegz + 1 )
    END IF

  END SUBROUTINE n2Linear

  !**********************************************************************!

  SUBROUTINE cLinear( x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, myThid  )

    ! c-linear interpolation of SSP data

    ! == Routine Arguments ==
    ! myThid :: Thread number for this instance of the routine
    INTEGER, INTENT(IN) :: myThid

    ! == Local Variables ==
    REAL (KIND=_RL90), INTENT( IN  ) :: freq
    REAL (KIND=_RL90), INTENT( IN  ) :: x( 2 )  ! r-z SSP evaluation point
    CHARACTER (LEN=3), INTENT( IN  ) :: Task
    REAL (KIND=_RL90), INTENT( OUT ) :: c, cimag, gradc( 2 ), crr, crz, czz, rho ! sound speed and its derivatives
    
    IF ( Task == 'INI' ) THEN   ! read in SSP data
       ! *** Task 'INI' for initialization ***

       Depth     = x( 2 )
       CALL ReadSSP( Depth, freq )
    ELSE                        ! return SSP info

       IF ( x( 2 ) < SSP%z( iSegz ) .OR. x( 2 ) > SSP%z( iSegz + 1 ) ) THEN
          DO iz = 2, SSP%NPts   ! Search for bracketting Depths
!IEsco23 Test this: 
!          DO iz = 2, SSP%Nz   ! Search for bracketting Depths
             IF ( x( 2 ) < SSP%z( iz ) ) THEN
                iSegz = iz - 1
                EXIT
             END IF
          END DO
       END IF

       c     = REAL(  SSP%c( iSegz ) + ( x( 2 ) - SSP%z( iSegz ) ) * SSP%cz( iSegz ) )
       cimag = AIMAG( SSP%c( iSegz ) + ( x( 2 ) - SSP%z( iSegz ) ) * SSP%cz( iSegz ) )
       gradc = [ 0.0D0, REAL( SSP%cz( iSegz ) ) ]
       crr   = 0.0d0
       crz   = 0.0d0
       czz   = 0.0d0

       W     = ( x( 2 ) - SSP%z( iSegz ) ) / ( SSP%z( iSegz + 1 ) - SSP%z( iSegz ) )
       rho   = ( 1.0D0 - W ) * SSP%rho( iSegz ) + W * SSP%rho( iSegz + 1 )
    END IF

  END SUBROUTINE cLinear

  !**********************************************************************!

  SUBROUTINE cPCHIP( x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, myThid  )

    ! This implements the monotone piecewise cubic Hermite interpolating
    ! polynomial (PCHIP) algorithm for the interpolation of the sound speed c.

    USE pchipMod,  only: PCHIP

    ! == Routine Arguments ==
    ! myThid :: Thread number for this instance of the routine
    INTEGER, INTENT(IN) :: myThid

    ! == Local Variables ==
    REAL (KIND=_RL90), INTENT( IN  ) :: freq
    REAL (KIND=_RL90), INTENT( IN  ) :: x( 2 )  ! r-z SSP evaluation point
    CHARACTER (LEN=3), INTENT( IN  ) :: Task
    REAL (KIND=_RL90), INTENT( OUT ) :: c, cimag, gradc( 2 ), crr, crz, czz, &
                                        rho ! sound speed and its derivatives
    REAL    (KIND=_RL90) :: xt
    COMPLEX (KIND=_RL90) :: c_cmplx

    IF ( Task == 'INI' ) THEN   ! read in SSP data
       ! *** Task 'INI' for initialization ***

       Depth = x( 2 )
       CALL ReadSSP( Depth, freq )

       !                                                               2      3
       ! compute coefficients of std cubic polynomial: c0 + c1*x + c2*x + c3*x
       !
       CALL PCHIP( SSP%z, SSP%c, SSP%NPts, SSP%cCoef, SSP%CSWork )
!IEsco23 Test this: 
!       CALL PCHIP( SSP%z, SSP%c, SSP%Nz, SSP%cCoef, SSP%CSWork )

    ELSE    ! return SSP info, recall iSegz is initiated to 1

       IF ( x( 2 ) < SSP%z( iSegz ) .OR. x( 2 ) > SSP%z( iSegz + 1 ) ) THEN
          DO iz = 2, SSP%NPts   ! Search for bracketting Depths
!IEsco23 Test this: 
!          DO iz = 2, SSP%Nz   ! Search for bracketting Depths
             IF ( x( 2 ) < SSP%z( iz ) ) THEN
                iSegz = iz - 1
                EXIT
             END IF
          END DO
       END IF

       xt = x( 2 ) - SSP%z( iSegz )
       c_cmplx = SSP%cCoef( 1, iSegz ) &
             + ( SSP%cCoef( 2, iSegz ) &
             + ( SSP%cCoef( 3, iSegz ) &
             +   SSP%cCoef( 4, iSegz ) * xt ) * xt ) * xt

       c     = REAL(  c_cmplx )
       cimag = AIMAG( c_cmplx )

       gradc = [ 0.0D0, &
                 REAL( SSP%cCoef( 2, iSegz ) + ( 2.0D0 * SSP%cCoef( 3, iSegz ) &
                       + 3.0D0 * SSP%cCoef( 4, iSegz ) * xt ) * xt ) ]

       crr   = 0.0D0
       crz   = 0.0D0
       czz   = REAL( 2.0D0 * SSP%cCoef( 3, iSegz ) + &
                     6.0D0 * SSP%cCoef( 4, iSegz ) * xt )   ! dgradc(2)/dxt

       W     = ( x( 2 ) - SSP%z( iSegz ) ) / &
               ( SSP%z( iSegz + 1 ) - SSP%z( iSegz ) )
       ! linear interp of density
       rho   = ( 1.0D0 - W ) * SSP%rho( iSegz ) + W * SSP%rho( iSegz + 1 )  

    END IF

  END SUBROUTINE cPCHIP

  !**********************************************************************!

  SUBROUTINE cCubic( x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, myThid  )

    ! Cubic spline interpolation

    ! == Routine Arguments ==
    ! myThid :: Thread number for this instance of the routine
    INTEGER, INTENT(IN) :: myThid

    ! == Local Variables ==
    REAL (KIND=_RL90), INTENT( IN )  :: freq
    REAL (KIND=_RL90), INTENT( IN  ) :: x( 2 )  ! r-z SSP evaluation point
    CHARACTER (LEN=3), INTENT( IN  ) :: Task
    REAL (KIND=_RL90), INTENT( OUT ) :: c, cimag, gradc( 2 ), crr, crz, czz, &
                                        rho ! sound speed and its derivatives
    INTEGER                 :: iBCBeg, iBCEnd
    REAL     (KIND=_RL90)   :: hSpline
    COMPLEX  (KIND=_RL90)   :: c_cmplx, cz_cmplx, czz_cmplx
    
    IF ( Task == 'INI' ) THEN
       ! *** Task 'INI' for initialization ***
       
       Depth     = x( 2 )
       CALL ReadSSP( Depth, freq )

       SSP%cSpline( 1, 1 : SSP%NPts ) = SSP%c( 1 : SSP%NPts )
!IEsco23 Test this: 
!       SSP%cSpline( 1, 1 : SSP%Nz ) = SSP%c( 1 : SSP%Nz )
       
       ! Compute spline coefs
       iBCBeg = 0
       iBCEnd = 0
       CALL CSpline( SSP%z, SSP%cSpline( 1, 1 ), SSP%NPts, iBCBeg, iBCEnd, SSP%NPts )
!IEsco23 Test this: 
!       CALL CSpline( SSP%z, SSP%cSpline( 1, 1 ), SSP%Nz, iBCBeg, iBCEnd, SSP%Nz )
    ELSE

       ! *** Section to return SSP info ***

       IF ( x( 2 ) < SSP%z( iSegz ) .OR. x( 2 ) > SSP%z( iSegz + 1 ) ) THEN
          DO iz = 2, SSP%NPts   ! Search for bracketting Depths
!IEsco23 Test this: 
!          DO iz = 2, SSP%Nz   ! Search for bracketting Depths
             IF ( x( 2 ) < SSP%z( iz ) ) THEN
                iSegz = iz - 1
                EXIT
             END IF
          END DO

       END IF

       hSpline = x( 2 ) - SSP%z( iSegz )

       ! c   = Spline(   SSP%cSpline( 1, iSegz ), hSpline )
       ! cz  = SplineX(  SSP%cSpline( 1, iSegz ), hSpline )
       ! czz = SplineXX( SSP%cSpline( 1, iSegz ), hSpline )

       CALL SplineALL( SSP%cSpline( 1, iSegz ), hSpline, c_cmplx, cz_cmplx, czz_cmplx )

       c     = DBLE(  c_cmplx )
       cimag = AIMAG( c_cmplx )
       gradc = [ 0.0D0, DBLE( cz_cmplx ) ]
       czz   = DBLE( czz_cmplx )
       crr   = 0.0d0
       crz   = 0.0d0

       ! linear interpolation for density
       W   = ( x( 2 ) - SSP%z( iSegz ) ) / ( SSP%z( iSegz + 1 ) - SSP%z( iSegz ) )
       rho = ( 1.0D0 - W ) * SSP%rho( iSegz ) + W * SSP%rho( iSegz + 1 )
    END IF

  END SUBROUTINE cCubic

  !**********************************************************************!

  SUBROUTINE Quad( x, c, cimag, gradc, crr, crz, czz, rho, freq, Task, myThid  )

    ! Bilinear quadrilatteral interpolation of SSP data in 2D, SSP%Type = 'Q'
    ! IEsco22: Assuming an SSPFile is required. Missing defensive check for SSPFile

    ! == Routine Arguments ==
    ! myThid :: Thread number for this instance of the routine
    INTEGER, INTENT(IN) :: myThid

    ! == Local Variables ==
    REAL (KIND=_RL90), INTENT( IN  ) :: freq
    REAL (KIND=_RL90), INTENT( IN  ) :: x( 2 )  ! r-z SSP evaluation point
    CHARACTER (LEN=3), INTENT( IN  ) :: Task
    REAL (KIND=_RL90), INTENT( OUT ) :: c, cimag, gradc( 2 ), crr, crz, czz, &
                                        rho ! sound speed and its derivatives
    INTEGER             :: AllocateStatus, irT, iz2
    REAL (KIND=_RL90)   :: c1, c2, cz1, cz2, cr, cz, s1, s2, delta_r, delta_z
    
    IF ( Task == 'INI' ) THEN
       ! *** Task 'INI' for initialization ***
       
        Depth = x( 2 )
        IF (useSSPFile .EQ. .TRUE.) THEN
            CALL ReadSSP( Depth, freq )
        ELSE
            CALL ExtractSSP(Depth, freq, myThid)
        END IF

        ! calculate cz
        DO irT = 1, SSP%Nr
            DO iz2 = 2, SSP%Nz
                delta_z = ( SSP%z( iz2 ) - SSP%z( iz2-1 ) )
                SSP%czMat( iz2-1, irT ) = ( SSP%cMat( iz2  , irT ) - &
                                            SSP%cMat( iz2-1, irT ) ) / delta_z
            END DO
        END DO

        RETURN

    ELSE ! Task == 'TAB'
       ! *** Section to return SSP info ***

       ! IESCO22: iSegz is the depth index containing x depth
       ! find depth-layer where x(2) in ( SSP%z( iSegz ), SSP%z( iSegz+1 ) )
       IF ( x( 2 ) < SSP%z( iSegz ) .OR. x( 2 ) > SSP%z( iSegz + 1 ) ) THEN
          DO iz = 2, SSP%Nz   ! Search for bracketting Depths
             IF ( x( 2 ) < SSP%z( iz ) ) THEN
                iSegz = iz - 1
                EXIT
             END IF
          END DO
       END IF

       ! Check that x is inside the box where the sound speed is defined
       IF ( x( 1 ) < SSP%Seg%r( 1 ) .OR. x( 1 ) > SSP%Seg%r( SSP%Nr ) ) THEN
          WRITE( PRTFile, * ) 'ray is outside the box where the ocean ',&
                              'soundspeed is defined'
          WRITE( PRTFile, * ) ' x = ( r, z ) = ', x
          CALL ERROUT( 'SSPMOD: Quad', &
              'ray is outside the box where the soundspeed is defined' )
       END IF

       ! find range-segment where x(1) in [ SSP%Seg%r( iSegr ), SSP%Seg%r( iSegr+1 ) )
       IF ( x( 1 ) < SSP%Seg%r( iSegr ) .OR. x( 1 ) >= SSP%Seg%r( iSegr + 1 ) ) THEN
          DO irT = 2, SSP%Nr   ! Search for bracketting segment ranges
             IF ( x( 1 ) < SSP%Seg%r( irT ) ) THEN
                iSegr = irT - 1
                EXIT
             END IF
          END DO
       END IF

       ! for depth, x(2), get the sound speed at both ends of range segment
       cz1 = SSP%czMat( iSegz, iSegr   )
       cz2 = SSP%czMat( iSegz, iSegr+1 )

       ! IESCO22: s2 is distance btwn field point, x(2), and ssp depth @ iSegz
       s2      = x( 2 )           - SSP%z( iSegz )            
       delta_z = SSP%z( iSegz+1 ) - SSP%z( iSegz )
       IF (delta_z <= 0 .OR. s2 > delta_z) CALL ERROUT('SSPMOD: Quad', &
           'depth is not monotonically increasing in SSPFile')
       
       c1 = SSP%cMat( iSegz, iSegr   ) + s2*cz1
       c2 = SSP%cMat( iSegz, iSegr+1 ) + s2*cz2

       ! s1 = proportional distance of x(1) in range
       delta_r = SSP%Seg%r( iSegr+1 ) - SSP%Seg%r( iSegr )
       s1 = ( x( 1 ) - SSP%Seg%r( iSegr ) ) / delta_r
       s1 = MIN( s1, 1.0D0 )   ! piecewise constant extrapolation for ranges outside SSPFile box
       s1 = MAX( s1, 0.0D0 )   ! "

       c = ( 1.0D0-s1 )*c1 + s1*c2 ! c @ x

       ! interpolate the attenuation !!!! SSP in ENVFile needs to match first column of SSPFile
       s2    = s2 / delta_z   ! normalize depth layer
       cimag = AIMAG( ( 1.0D0-s2 )*SSP%c( Isegz ) + s2*SSP%c( Isegz+1 ) )   ! volume attenuation is taken from the single c(z) profile

       cz  = ( 1.0D0-s1 )*cz1 + s1*cz2 ! cz @ x

       cr  = ( c2  - c1  ) / delta_r ! SSPFile grid cr
       crz = ( cz2 - cz1 ) / delta_r ! SSPFile grid crz

       gradc = [ cr, cz ]
       crr   = 0.0D0
       czz   = 0.0D0

       ! linear interpolation for density
       W   = ( x( 2 ) - SSP%z( iSegz ) ) / ( SSP%z( iSegz + 1 ) - SSP%z( iSegz ) )
       rho = ( 1.0D0 - W ) * SSP%rho( iSegz ) + W * SSP%rho( iSegz + 1 )
    END IF

    !IESCO22: for thesis, czz=crr=0, and rho=1 at all times
  END SUBROUTINE Quad

!**********************************************************************!

  SUBROUTINE Analytic( x, c, cimag, gradc, crr, crz, czz, rho, Task, myThid  )

    ! == Routine Arguments ==
    ! myThid :: Thread number for this instance of the routine
    INTEGER, INTENT(IN) :: myThid

    ! == Local Variables ==
    REAL (KIND=_RL90), INTENT( IN  ) :: x( 2 )
    CHARACTER (LEN=3), INTENT( IN  ) :: Task
    REAL (KIND=_RL90), INTENT( OUT ) :: c, cimag, gradc( 2 ), crr, crz, czz, rho
    REAL (KIND=_RL90)                :: c0, cr, cz, DxtDz, xt

    IF ( Task == 'INI' ) THEN
       WRITE( PRTFile, * ) 'Analytic SSP option'
       SSP%NPts = 2
       SSP%z(1) = 0.0
       SSP%z(2) = IHOP_depth
    END IF

    iSegz = 1
    c0    = 1500.0
    rho   = 1.0

    ! homogeneous halfspace was removed since BELLHOP needs to get gradc just a little below the boundaries, on ray reflection

!!$  IF ( x( 2 ) < 5000.0 ) THEN
    xt    = 2.0 * ( x( 2 ) - 1300.0 ) / 1300.0
    DxtDz = 2.0 / 1300.0
    c     = C0 * ( 1.0 + 0.00737*( xt - 1.0 + EXP( -xt ) ) )
    cimag = 0.
    cz    = C0 * 0.00737 * ( 1.0 - EXP( -xt ) ) * DxtDz
    czz   = C0 * 0.00737 * EXP( -xt ) * DxtDz ** 2
!!$  ELSE
!!$     ! Homogeneous half-space
!!$     xt   = 2.0 * ( 5000.0 - 1300.0 ) / 1300.0
!!$     c    = C0 * ( 1.0 + 0.00737 * ( xt - 1.0 + EXP( -xt ) ) )
!!$     cz   = 0.0
!!$     czz  = 0.0
!!$  END IF

    cr = 0.0
    gradc = [ cr, cz ]
    crz = 0.0
    crr = 0.0

    RETURN
  END SUBROUTINE Analytic

!**********************************************************************!

  SUBROUTINE ReadSSP( Depth, freq )
    ! reads SSP in m/s from .ssp file and convert to AttenUnit (ie. Nepers/m)
    ! Populates SSPStructure: SSP

    USE attenMod, only: CRCI

    REAL (KIND=_RL90), INTENT(IN) :: Depth, freq
    INTEGER :: iz2,k

    ! OPEN SSPFile to read
    OPEN ( FILE = TRIM( IHOP_fileroot ) // '.ssp', UNIT = SSPFile, &
        FORM = 'FORMATTED', STATUS = 'OLD', IOSTAT = iostat )
    IF ( IOSTAT /= 0 ) THEN   ! successful open?
       WRITE( PRTFile, * ) 'SSPFile = ', TRIM( IHOP_fileroot ) // '.ssp'
       CALL ERROUT( 'READENV: ReadTopOpt', 'Unable to open the SSP file' )
    END IF

    ! Write relevant diagnostics
    WRITE( PRTFile, * ) "Sound Speed Field" 
    WRITE( PRTFile, * ) '____________________________________________________',&
                        '______________________'
    WRITE( PRTFile, * ) 

    READ( SSPFile,  * ) SSP%Nr, SSP%Nz
    IF (SSP%Nr .GT. 1) WRITE( PRTFile, * ) 'Using range-dependent sound speed'
    IF (SSP%Nr .EQ. 1) WRITE( PRTFile, * ) 'Using range-independent sound speed'

    WRITE( PRTFile, * ) 'Number of SSP ranges = ', SSP%Nr
    WRITE( PRTFile, * ) 'Number of SSP depths = ', SSP%Nz

    ALLOCATE( SSP%cMat( SSP%Nz, SSP%Nr ), &
              SSP%czMat( SSP%Nz-1, SSP%Nr ), &
              SSP%Seg%r( SSP%Nr ), &
              STAT = iallocstat )
    IF ( iallocstat /= 0 ) CALL ERROUT( 'SSPMOD: Quad', &
                                        'Insufficient memory to store SSP' )

    READ( SSPFile,  * ) SSP%Seg%r( 1 : SSP%Nr )
    WRITE( PRTFile, * )
    WRITE( PRTFile, * ) 'Profile ranges (km):'
    WRITE( PRTFile, FMT="( F10.2 )"  ) SSP%Seg%r( 1 : SSP%Nr )
    SSP%Seg%r = 1000.0 * SSP%Seg%r   ! convert km to m

    READ( SSPFile,  * ) SSP%z( 1 : SSP%Nz )
!#ifdef IHOP_DEBUG
    WRITE( PRTFile, * )
    WRITE( PRTFile, * ) 'Profile depths (m):'
    WRITE( PRTFile, FMT="( F10.2 )"  ) SSP%z( 1 : SSP%Nz )
!#endif

    ! IEsco23: contain read of ssp in this subroutine only 
    ! IEsco23: change to allocatable memory since we should know Nz
#ifdef IHOP_DEBUG
    WRITE( PRTFile, * )
    WRITE( PRTFile, * ) 'Sound speed matrix:'
    WRITE( PRTFile, * ) ' Depth (m )     Soundspeed (m/s)'
#endif
    DO iz2 = 1, SSP%Nz
       READ(  SSPFile, * ) SSP%cMat( iz2, : )
#ifdef IHOP_DEBUG
       WRITE( PRTFile, FMT="( 12F10.2 )"  ) SSP%z( iz2 ), SSP%cMat( iz2, : )
#endif
    END DO
    CLOSE( SSPFile )

    WRITE( PRTFile, * )
    WRITE( PRTFile, * ) 'Sound speed profile:'
    WRITE( PRTFile, "( '      z         alphaR      betaR     rho        alphaI     betaI'    )" )
    WRITE( PRTFile, "( '     (m)         (m/s)      (m/s)   (g/cm^3)      (m/s)     (m/s)', / )" )
    
    WRITE( PRTFile, * ) '____________________________________________________',&
                        '______________________'
    WRITE( PRTFile, * )
    SSP%NPts = 1
    DO iz = 1, MaxSSP 
       alphaR = SSP%cMat( iz, 1 )
       WRITE( PRTFile, FMT="( F10.2, 3X, 2F10.2, 3X, F6.2, 3X, 2F10.4 )" ) &
           SSP%z( iz ), alphaR, betaR, rhoR, alphaI, betaI

       SSP%c(   iz ) = CRCI( SSP%z( iz ), alphaR, alphaI, freq, freq, &
                             SSP%AttenUnit, betaPowerLaw, fT )
       SSP%rho( iz ) = rhoR !IEsco22: set to a default value of 1

       ! verify depths are monotone increasing
       IF ( iz > 1 ) THEN
          IF ( SSP%z( iz ) .LE. SSP%z( iz - 1 ) ) THEN
              WRITE( PRTFile, * ) 'Bad depth in SSP: ', SSP%z( iz )
              CALL ERROUT( 'ReadSSP', &
                  'The depths in the SSP must be monotone increasing' )
          END IF
       END IF

       ! compute gradient, cz
       IF ( iz > 1 ) SSP%cz( iz - 1 )  = ( SSP%c( iz ) - SSP%c( iz - 1 ) ) / &
                                         ( SSP%z( iz ) - SSP%z( iz - 1 ) )

       ! Did we read the last point?
       IF ( ABS( SSP%z( iz ) - Depth ) < 100. * EPSILON( 1.0e0 ) ) THEN
          !IF ( SSP%NPts == 1 ) THEN
          !    WRITE( PRTFile, * ) '#SSP points: ', SSP%NPts
          !    CALL ERROUT( 'ReadSSP', 'The SSP must have at least 2 points' )
          !END IF

          RETURN
       ENDIF

       SSP%NPts = SSP%NPts + 1
    END DO
 
    ! Fall through means too many points in the profile
    WRITE( PRTFile, * ) 'Max. #SSP points: ', MaxSSP
    CALL ERROUT( 'ReadSSP', 'Number of SSP points exceeds limit' )

  END SUBROUTINE ReadSSP

  SUBROUTINE ExtractSSP( Depth, freq, myThid )
      ! Extracts SSP from MITgcm grid points

      use attenMod, only: CRCI

      ! == Routine Arguments ==
      ! myThid :: Thread number for this instance of the routine
      INTEGER, INTENT(IN) :: myThid
      REAL (KIND=_RL90), INTENT(IN) :: Depth, freq

      ! == Local Variables ==
      INTEGER ii

      SSP%Nz = Nr
      SSP%Nr = IHOP_NPTS_RANGE

      ALLOCATE( SSP%cMat( SSP%Nz, SSP%Nr ), &
                SSP%czMat( SSP%Nz-1, SSP%Nr ), &
                SSP%Seg%r( SSP%Nr ), &
                STAT = iallocstat )
      IF ( iallocstat /= 0 ) CALL ERROUT( 'SSPMOD: ExtractSSP', &
                                      'Insufficient memory to store SSP' )

      ! set SSP%Seg%r from data.ihop > ihop_ranges
      SSP%Seg%r(1:SSP%Nr) = ihop_ranges

      ! set SSP%z from rC, rkSign=-1 used bc ihop uses +ive depths
      SSP%z(1:SSP%Nz) = rkSign*rC(1:SSP%Nz)

      ! ssp extraction to get SSP%cMat
      !=============================================
      ! Option 1: COMPARING LAT LON VALUES DIRECTLY
      !=============================================
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO i=1,sNx
         IF (xC(i,1,bi,bj) .EQ. ihop_xc) THEN
          DO j=1,sNy
           DO ii=1,IHOP_NPTS_RANGE
            IF (yC(i,j,bi,bj) .EQ. ihop_yc(ii)) THEN
             SSP%cMat(:,ii) = ihop_ssp(i,j,:,bi,bj)
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDDO
      !=============================================
      ! END Option 1
      !=============================================

      ! set vector structured c, rho, and cz for first range point
      DO iz = 1,SSP%Nz
        alphaR = SSP%cMat( iz, 1 )
        
        SSP%c(   iz ) = CRCI( SSP%z(iz), alphaR, alphaI, freq, freq, &
                              SSP%AttenUnit, betaPowerLaw, fT )
        SSP%rho( iz ) = rhoR

        IF ( iz > 1 ) THEN
            IF ( SSP%z( iz ) .LE. SSP%z( iz-1 ) ) THEN
                WRITE( PRTFile, * ) 'Bad depth in SSP: ', SSP%z(iz)
                CALL ERROUT( 'ReadSSP', &
                    'The depths in the SSP must be monotone increasing' )
            END IF
        END IF

        IF ( iz>1 ) SSP%cz( iz-1 ) = ( SSP%c( iz ) - SSP%c( iz-1 ) ) / &
                                     ( SSP%z( iz ) - SSP%z( iz-1 ) )

        IF ( ABS( SSP%z( iz )-Depth ) < 100. * EPSILON( 1.0e0 ) ) RETURN

      END DO

      ! Write relevant diagnostics
      WRITE( PRTFile, * ) "Sound Speed Field" 
      WRITE( PRTFile, * ) '________________________________________________', &
          '__________________________'
      WRITE( PRTFile, * ) 
  
      IF (SSP%Nr .GT. 1) WRITE( PRTFile,* ) 'Using range-dependent sound speed'
      IF (SSP%Nr .EQ. 1) WRITE( PRTFile,* ) 'Using range-independent sound speed'
  
      WRITE( PRTFile, * ) 'Number of SSP ranges = ', SSP%Nr
      WRITE( PRTFile, * ) 'Number of SSP depths = ', SSP%Nz
  
      WRITE( PRTFile, * )
      WRITE( PRTFile, * ) 'Profile ranges (km):'
      WRITE( PRTFile, FMT="( F10.2 )"  ) SSP%Seg%r( 1:SSP%Nr )
      SSP%Seg%r = 1000.0 * SSP%Seg%r   ! convert km to m
#ifdef IHOP_DEBUG
      WRITE( PRTFile, * )
      WRITE( PRTFile, * ) 'Sound speed matrix:'
      WRITE( PRTFile, * ) ' Depth (m )     Soundspeed (m/s)'
#endif
      DO ii = 1, SSP%Nz
#ifdef IHOP_DEBUG
         WRITE( PRTFile, FMT="( 12F10.2 )"  ) SSP%z( ii ), SSP%cMat( ii, : )
#endif
      END DO

  END SUBROUTINE ExtractSSP

END MODULE sspMod
