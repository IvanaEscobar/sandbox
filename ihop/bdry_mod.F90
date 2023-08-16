#include "IHOP_OPTIONS.h"
!BOP
! !INTERFACE:
MODULE bdry_mod
    ! <CONTACT EMAIL="ivana@utexas.edu">
    !   Ivana Escobar
    ! </CONTACT>

  ! Loads altimetry (top bdry) and bathymetry (bottom bdry) data
  ! IEsco22: want to rname this for clarity to readTopBot since it takes user
  ! input and populates various structures used in the code

  USE monotonic_mod,    only: monotonic
  USE ihop_mod,         only: PRTFile, ATIFile, BTYFile

  IMPLICIT NONE
! == Global variables ==
#include "EEPARAMS.h"

  PRIVATE

! public interfaces
!=======================================================================

    public ReadATI, ReadBTY, GetTopSeg, GetBotSeg, Bot, Top, &
           IsegTop, IsegBot, rTopSeg, rBotSeg,&
           iSmallStepCtr, atiType, btyType, NATIPts, NBTYPts

!=======================================================================

  INTEGER, PARAMETER :: Number_to_Echo = 21
  INTEGER            :: IsegTop, IsegBot ! indices point to current active segment
  INTEGER, PROTECTED :: NATIPts = 2, NBTYPts = 2
  INTEGER            :: ii, IOStat, IAllocStat, iSmallStepCtr = 0

  ! range intervals defining the current active segment
  REAL (KIND=_RL90)  :: rTopseg( 2 ), rBotseg( 2 )  
  CHARACTER  (LEN=2) :: atiType= 'LS', btyType = 'LS'

  ! ***Halfspace properties***
  TYPE HSInfo2
     ! compressional and shear wave speeds/attenuations in user units
     REAL     (KIND=_RL90) :: alphaR, alphaI, betaR, betaI  
     REAL     (KIND=_RL90) :: rho, Depth        ! density, depth
     COMPLEX  (KIND=_RL90) :: cP, cS            ! P-wave, S-wave speeds
     CHARACTER (LEN=1) :: BC                    ! Boundary condition type
     CHARACTER (LEN=6) :: Opt
  END TYPE

  ! ***Boundary properties***
  TYPE BdryPt
     REAL (KIND=_RL90) :: x( 2 ), &     ! segment coordinate
                          t( 2 ), &     ! segment tangent
                          n( 2 )        ! segment outward
     REAL (KIND=_RL90) :: Len, Kappa    ! length and curvature of a segement
     ! For the curvilinear grid option
     REAL (KIND=_RL90) :: Nodet( 2 ), & ! tangent at the node
                          Noden( 2 )    ! normal at the node 
     REAL (KIND=_RL90) :: Dx, Dxx, &    ! 1st, 2nd derivatives wrt depth
                          Dss           ! derivative along tangent
     TYPE( HSInfo2 )   :: HS
  END TYPE

  TYPE(BdryPt), ALLOCATABLE :: Top( : ), Bot( : )

CONTAINS
  SUBROUTINE ReadATI( FileRoot, TopATI, DepthT, myThid ) 
    ! Reads in the top altimetry

    INTEGER, INTENT( IN ) :: myThid

    CHARACTER (LEN= 1), INTENT( IN ) :: TopATI
    REAL (KIND=_RL90),  INTENT( IN ) :: DepthT
    REAL (KIND=_RL90),  ALLOCATABLE  :: phi( : )
    CHARACTER (LEN=80), INTENT( IN ) :: FileRoot

    SELECT CASE ( TopATI )
    CASE ( '~', '*' )
       WRITE( PRTFile, * ) '______________________________________________', &
                           '____________________________'
       WRITE( PRTFile, * )
       WRITE( PRTFile, * ) 'Using top-altimetry file'

       OPEN( UNIT = ATIFile,   FILE = TRIM( FileRoot ) // '.ati', &
             STATUS = 'OLD', IOSTAT = IOStat, ACTION = 'READ' )
        IF ( IOsTAT /= 0 ) THEN
            WRITE( PRTFile, * ) 'ATIFile = ', TRIM( FileRoot ) // '.ati'
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadATI', &
                'Unable to open altimetry file'
            STOP 'ABNORMAL END: S/R ReadATI'
        END IF

       READ(  ATIFile, * ) atiType
       AltiType: SELECT CASE ( atiType( 1 : 1 ) )
       CASE ( 'C' )
          WRITE( PRTFile, * ) 'Curvilinear Interpolation'
       CASE ( 'L' )
          WRITE( PRTFile, * ) 'Piecewise linear interpolation'
       CASE DEFAULT
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadATI', &
                       'Unknown option for selecting altimetry interpolation'
            STOP 'ABNORMAL END: S/R ReadATI'
       END SELECT AltiType

       READ(  ATIFile, * ) NatiPts
       WRITE( PRTFile, * ) 'Number of altimetry points = ', NatiPts
       ! we'll be extending the altimetry to infinity to the left and right
       NatiPts = NatiPts + 2  

       ALLOCATE( Top(  NatiPts ), phi( NatiPts ), Stat = IAllocStat )
       IF ( IAllocStat /= 0 ) THEN
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadATI', &
                'Insufficient memory for altimetry data: reduce # ati points'
            STOP 'ABNORMAL END: S/R ReadATI'
        END IF

       WRITE( PRTFile, * )
       WRITE( PRTFile, * ) ' Range (km)  Depth (m)'

       atiPt: DO ii = 2, NatiPts - 1

          SELECT CASE ( atiType( 2 : 2 ) )
          CASE ( 'S', '' )
            READ(  ATIFile, * ) Top( ii )%x
            IF ( ii < Number_to_Echo .OR. ii == NatiPts ) THEN   
                WRITE( PRTFile, FMT = "(2G11.3)" ) Top( ii )%x 
            END IF
          CASE ( 'L' )
             READ(  ATIFile, * ) Top( ii )%x, Top( ii )%HS%alphaR, &
                                 Top( ii )%HS%betaR, Top( ii )%HS%rho, &
                                 Top( ii )%HS%alphaI, Top( ii )%HS%betaI
             IF ( ii < Number_to_Echo .OR. ii == NatiPts ) THEN   
                WRITE( PRTFile, FMT = "(7G11.3)" ) &
                    Top( ii )%x, Top( ii )%HS%alphaR, Top( ii )%HS%betaR, &
                    Top( ii )%HS%rho, Top( ii )%HS%alphaI, Top( ii )%HS%betaI
             END IF
          CASE DEFAULT
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadATI', &
                            'Unknown option for selecting altimetry option'
            STOP 'ABNORMAL END: S/R ReadATI'
          END SELECT

          IF ( Top( ii )%x( 2 ) < DepthT ) THEN
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadATI', &
                'Altimetry rises above highest point in the sound speed profile'
            STOP 'ABNORMAL END: S/R ReadATI'
          END IF
       END DO atiPt

       CLOSE( ATIFile )

       Top( : )%x( 1 ) = 1000.0 * Top( : )%x( 1 )   ! Convert ranges in km to m

    CASE DEFAULT   ! no altimetry given, use SSP depth for flat top
        ALLOCATE( Top( 2 ), Stat = IAllocStat )
        IF ( IAllocStat /= 0 ) THEN
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadATI', &
                                    'Insufficient memory for altimetry data'
            STOP 'ABNORMAL END: S/R ReadATI'
        END IF
        Top( 1 )%x = [ -sqrt( huge( Top( 1 )%x( 1 ) ) ) / 1.0d5, DepthT ]
        Top( 2 )%x = [  sqrt( huge( Top( 1 )%x( 1 ) ) ) / 1.0d5, DepthT ]
    END SELECT

    CALL ComputeBdryTangentNormal( Top, 'Top' )

    IF ( .NOT. monotonic( Top%x( 1 ), NAtiPts ) ) THEN
        WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadATI', &
                        'Altimetry ranges are not monotonically increasing'
        STOP 'ABNORMAL END: S/R ReadATI'
    END IF 
 
  END SUBROUTINE ReadATI

  ! **********************************************************************!

  SUBROUTINE ReadBTY( FileRoot, BotBTY, DepthB, myThid )

    ! Reads in the bottom bathymetry

    INTEGER, INTENT( IN ) :: myThid

    CHARACTER (LEN= 1), INTENT( IN ) :: BotBTY
    REAL (KIND=_RL90),  INTENT( IN ) :: DepthB
    CHARACTER (LEN=80), INTENT( IN ) :: FileRoot

    SELECT CASE ( BotBTY )
    CASE ( '~', '*' )
       WRITE( PRTFile, * ) '________________________________________________', &
                           '__________________________'
       WRITE( PRTFile, * )
       WRITE( PRTFile, * ) 'Using bottom-bathymetry file'

       OPEN( UNIT = BTYFile, FILE = TRIM( FileRoot ) // '.bty', STATUS = 'OLD',& 
             IOSTAT = IOStat, ACTION = 'READ' )
        IF ( IOsTAT /= 0 ) THEN
            WRITE( PRTFile, * ) 'BTYFile = ', TRIM( FileRoot ) // '.bty'
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadBTY: ', &
                                 'Unable to open bathymetry file'
            STOP 'ABNORMAL END: S/R ReadBTY'
       END IF
 
       READ( BTYFile, * ) btyType
 
       BathyType: SELECT CASE ( btyType( 1 : 1 ) )
       CASE ( 'C' )
          WRITE( PRTFile, * ) 'Curvilinear Interpolation'
       CASE ( 'L' )
          WRITE( PRTFile, * ) 'Piecewise linear interpolation'
       CASE DEFAULT
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadBTY: ', &
                    'Unknown option for selecting bathymetry interpolation'
            STOP 'ABNORMAL END: S/R ReadBTY'
       END SELECT BathyType


       READ(  BTYFile, * ) NbtyPts
       WRITE( PRTFile, * ) 'Number of bathymetry points = ', NbtyPts

        ! we'll be extending the bathymetry to infinity on both sides
        NbtyPts = NbtyPts + 2  
        ALLOCATE( Bot( NbtyPts ), Stat = IAllocStat )
        IF ( IAllocStat /= 0 ) THEN
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadBTY: ', &
                'Insufficient memory for bathymetry data: reduce # bty points'
            STOP 'ABNORMAL END: S/R ReadBTY'
        END IF
        
       WRITE( PRTFile, * )
       BathyTypeB: SELECT CASE ( btyType( 2 : 2 ) )
       CASE ( 'S', '' )
          WRITE( PRTFile, * ) 'Short format (bathymetry only)'
          WRITE( PRTFile, * ) ' Range (km)  Depth (m)'
       CASE ( 'L' )
          WRITE( PRTFile, * ) 'Long format (bathymetry and geoacoustics)'
          WRITE( PRTFile, "( ' Range (km)  Depth (m)  alphaR (m/s)  betaR  rho (g/cm^3)  alphaI     betaI', / )" )
       CASE DEFAULT
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadBTY: ', &
                    'Unknown option for selecting bathymetry interpolation'
            STOP 'ABNORMAL END: S/R ReadBTY'
       END SELECT BathyTypeB

       btyPt: DO ii = 2, NbtyPts - 1

          SELECT CASE ( btyType( 2 : 2 ) )
          CASE ( 'S', '' )   ! short format
             READ(  BTYFile, * ) Bot( ii )%x
             IF ( ii < Number_to_Echo .OR. ii == NbtyPts ) THEN  
                WRITE( PRTFile, FMT = "(2G11.3)" ) Bot( ii )%x
             END IF
          CASE ( 'L' )       ! long format
             READ(  BTYFile, * ) Bot( ii )%x, Bot( ii )%HS%alphaR, &
                                 Bot( ii )%HS%betaR, Bot( ii )%HS%rho, &
                                 Bot( ii )%HS%alphaI, Bot( ii )%HS%betaI
             IF ( ii < Number_to_Echo .OR. ii == NbtyPts ) THEN   
                WRITE( PRTFile, FMT="( F10.2, F10.2, 3X, 2F10.2, 3X, F6.2, 3X, 2F10.4 )" ) &
                   Bot( ii )%x, Bot( ii )%HS%alphaR, Bot( ii )%HS%betaR, &
                   Bot( ii )%HS%rho, Bot( ii )%HS%alphaI, Bot( ii )%HS%betaI
             END IF
          CASE DEFAULT
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadBTY: ', &
                    'Unknown option for selecting bathymetry interpolation'
            STOP 'ABNORMAL END: S/R ReadBTY'
          END SELECT

          IF ( Bot( ii )%x( 2 ) > DepthB ) THEN
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadBTY: ', &
                'Bathymetry drops below lowest point in the sound speed profile'
            STOP 'ABNORMAL END: S/R ReadBTY'
          END IF
 
       END DO btyPt

       CLOSE( BTYFile )

       Bot( : )%x( 1 ) = 1000.0 * Bot( : )%x( 1 )   ! Convert ranges in km to m

    CASE DEFAULT   ! no bathymetry given, use SSP depth for flat bottom
        WRITE( PRTFile, * ) 'No BTYFile; assuming flat bottom'
        ALLOCATE( Bot( 2 ), Stat = IAllocStat )
        IF ( IAllocStat /= 0 ) THEN
            WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadBTY: ', &
                                 'Insufficient memory for bathymetry data'
            STOP 'ABNORMAL END: S/R ReadBTY'
        END IF
        Bot( 1 )%x = [ -sqrt( huge( Bot( 1 )%x( 1 ) ) ) / 1.0d5, DepthB ]
        Bot( 2 )%x = [  sqrt( huge( Bot( 1 )%x( 1 ) ) ) / 1.0d5, DepthB ]
    END SELECT

    CALL ComputeBdryTangentNormal( Bot, 'Bot' )

    IF ( .NOT. monotonic( Bot%x( 1 ), NBtyPts ) ) THEN
        WRITE(errorMessageUnit,'(2A)') 'BDRYMOD ReadBTY: ', &
                    'Bathymetry ranges are not monotonically increasing'
        STOP 'ABNORMAL END: S/R ReadBTY'
    END IF 

  END SUBROUTINE ReadBTY

  ! **********************************************************************!

  SUBROUTINE ComputeBdryTangentNormal( Bdry, BotTop )

    ! Does some pre-processing on the boundary points to pre-compute segment
    ! lengths  (%Len),
    ! tangents (%t, %nodet),
    ! normals  (%n, %noden), and
    ! curvatures (%kappa)
    !
    ! The boundary is also extended with a constant depth to infinity to cover 
    ! cases where the ray exits the domain defined by the user

    INTEGER                          :: NPts = 0
    REAL (KIND=_RL90), ALLOCATABLE   :: phi( : )
    REAL (KIND=_RL90)                :: sss
    TYPE(BdryPt)                     :: Bdry( : )
    CHARACTER (LEN=3),  INTENT( IN ) :: BotTop ! Flag indicating bottom or top reflection
    CHARACTER (LEN=2)                :: CurvilinearFlag = '-'

    SELECT CASE ( BotTop )
    CASE ( 'Bot' )
       NPts = NbtyPts
       CurvilinearFlag = btyType
    CASE ( 'Top' )
       NPts = NatiPts
       CurvilinearFlag = atiType
    END SELECT

    ! extend the bathymetry to +/- infinity in a piecewise constant fashion

    Bdry( 1    )%x( 1 ) = -sqrt( huge( Bdry( 1 )%x( 1 ) ) ) / 1.0d5
    Bdry( 1    )%x( 2 ) = Bdry( 2        )%x( 2 )
    Bdry( 1    )%HS     = Bdry( 2        )%HS
    Bdry( NPts )%x( 1 ) = +sqrt( huge( Bdry( 1 )%x( 1 ) ) ) / 1.0d5
    Bdry( NPts )%x( 2 ) = Bdry( NPts - 1 )%x( 2 )
    Bdry( NPts )%HS     = Bdry( NPts - 1 )%HS

    ! compute tangent and outward-pointing normal to each bottom segment
    ! tBdry( 1, : ) = xBdry( 1, 2:NPts ) - xBdry( 1, 1:NPts - 1 )
    ! tBdry( 2, : ) = xBdry( 2, 2:NPts ) - xBdry( 2, 1:NPts - 1 )
    ! above caused compiler problems

    BoundaryPt: DO ii = 1, NPts - 1
       Bdry( ii )%t   = Bdry( ii + 1 )%x      - Bdry( ii )%x
       Bdry( ii )%Dx  = Bdry( ii )%t( 2 ) / Bdry( ii )%t( 1 )   ! 1st derivative

       ! normalize the tangent vector
       Bdry( ii )%Len = NORM2( Bdry( ii )%t )
       Bdry( ii )%t   = Bdry( ii )%t / Bdry( ii )%Len

       SELECT CASE ( BotTop )
       CASE ( 'Bot' )
          Bdry( ii )%n( 1 ) = -Bdry( ii )%t( 2 )
          Bdry( ii )%n( 2 ) = +Bdry( ii )%t( 1 )
       CASE ( 'Top' )
          Bdry( ii )%n( 1 ) = +Bdry( ii )%t( 2 )
          Bdry( ii )%n( 2 ) = -Bdry( ii )%t( 1 )
       END SELECT

    END DO BoundaryPt

    ! curvilinear option: compute tangent and normal at node by averaging 
    ! normals on adjacent segments
    IF ( CurvilinearFlag( 1 : 1 ) == 'C' ) THEN 
       ! averaging two centered differences is equivalent to forming a single 
       ! centered difference of two steps ...
       DO ii = 2, NPts - 1
          sss = Bdry( ii - 1 )%Len / ( Bdry( ii - 1 )%Len + Bdry( ii )%Len )
          sss = 0.5
          Bdry( ii )%Nodet = ( 1.0 - sss ) * Bdry( ii-1 )%t + sss * Bdry( ii )%t
       END DO

       Bdry( 1    )%Nodet = [ 1.0, 0.0 ]   ! tangent left-end node
       Bdry( NPts )%Nodet = [ 1.0, 0.0 ]   ! tangent right-end node

       SELECT CASE ( BotTop )
       CASE ( 'Bot' )
          Bdry( : )%Noden( 1 ) = -Bdry( : )%Nodet( 2 )
          Bdry( : )%Noden( 2 ) = +Bdry( : )%Nodet( 1 )
       CASE ( 'Top' )
          Bdry( : )%Noden( 1 ) = +Bdry( : )%Nodet( 2 )
          Bdry( : )%Noden( 2 ) = -Bdry( : )%Nodet( 1 )
       END SELECT

       ! compute curvature in each segment
       ALLOCATE( phi( NPts ), Stat = IAllocStat )
       ! phi is the angle at each node
       phi = atan2( Bdry( : )%Nodet( 2 ), Bdry( : )%Nodet( 1 ) )

       DO ii = 1, NPts - 1
          ! this is curvature = dphi/ds
          Bdry( ii )%kappa = ( phi( ii+1 ) - phi( ii ) ) / Bdry( ii )%Len
          ! second derivative
          Bdry( ii )%Dxx   = ( Bdry( ii+1 )%Dx     - Bdry( ii )%Dx     ) / &   
                             ( Bdry( ii+1 )%x( 1 ) - Bdry( ii )%x( 1 ) )
          ! derivative in direction of tangent
          Bdry( ii )%Dss   = Bdry( ii )%Dxx * Bdry( ii )%t( 1 )**3   

          Bdry( ii )%kappa = Bdry( ii )%Dss   !over-ride kappa !!!!!
       END DO
    ELSE
       Bdry%kappa = 0
    END IF

  END SUBROUTINE ComputeBdryTangentNormal

  ! **********************************************************************!

  SUBROUTINE GetTopSeg( r, myThid )

    ! Get the Top segment info (index and range interval) for range, r

    INTEGER, INTENT( IN ) :: myThid

    INTEGER IsegTopT( 1 )
    REAL (KIND=_RL90), INTENT( IN ) :: r

    IsegTopT = MAXLOC( Top( : )%x( 1 ), Top( : )%x( 1 ) < r )

    ! IsegTop MUST LIE IN [ 1, NatiPts-1 ]
    IF ( IsegTopT( 1 ) > 0 .AND. IsegTopT( 1 ) < NatiPts ) THEN  
       IsegTop = IsegTopT( 1 )
       ! segment limits in range
       rTopSeg = [ Top( IsegTop )%x( 1 ), Top( IsegTop+1 )%x( 1 ) ]   
    ELSE
        WRITE( PRTFile, * ) 'r = ', r
        WRITE( PRTFile, * ) 'rLeft  = ', Top( 1       )%x( 1 )
        WRITE( PRTFile, * ) 'rRight = ', Top( NatiPts )%x( 1 )
        WRITE(errorMessageUnit,'(2A)') 'BDRYMOD GetTopSeg', &
                             'Top altimetry undefined above the ray'
        STOP 'ABNORMAL END: S/R GetTopSeg'
    ENDIF

  END SUBROUTINE GetTopSeg

  ! **********************************************************************!

  SUBROUTINE GetBotSeg( r, myThid )

    ! Get the Bottom segment info (index and range interval) for range, r
    INTEGER, INTENT( IN ) :: myThid

    INTEGER IsegBotT( 1 )
    REAL (KIND=_RL90), INTENT( IN ) :: r

    IsegBotT = MAXLOC( Bot( : )%x( 1 ), Bot( : )%x( 1 ) < r )
    ! IsegBot MUST LIE IN [ 1, NbtyPts-1 ]
    IF ( IsegBotT( 1 ) > 0 .AND. IsegBotT( 1 ) < NbtyPts ) THEN
       IsegBot = IsegBotT( 1 )   
       ! segment limits in range
       rBotSeg = [ Bot( IsegBot )%x( 1 ), Bot( IsegBot + 1 )%x( 1 ) ]
    ELSE
        WRITE( PRTFile, * ) 'r = ', r
        WRITE( PRTFile, * ) 'rLeft  = ', Bot( 1       )%x( 1 )
        WRITE( PRTFile, * ) 'rRight = ', Bot( NbtyPts )%x( 1 )
        WRITE(errorMessageUnit,'(2A)') 'BDRYMOD GetBotSeg', &
                             'Bottom bathymetry undefined below the source'
        STOP 'ABNORMAL END: S/R GetBotSeg'
    ENDIF

  END SUBROUTINE GetBotSeg

END MODULE bdry_mod
