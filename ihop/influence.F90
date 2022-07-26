#include "IHOP_OPTIONS.h"
!BOP
! !INTERFACE:
MODULE influence
    ! <CONTACT EMAIL="ivana@utexas.edu">
    !   Ivana Escobar
    ! </CONTACT>

  ! Compute the beam influence, i.e. the contribution of a single beam to the 
  ! complex pressure
  ! mbp 12/2018, based on much older subroutines

  USE iHopMod       ! added to get BeamStructure: Beam
  USE iHopParams,   only: pi, i, RadDeg, PRTFile, MaxN
  USE srPositions,  only: Pos
! sspMod used to construct image beams in the Cerveny style beam routines
  USE SSPMod,       only: EvaluateSSP, Bdry 
  USE arrMod,       only: WriteArrivalsASCII, WriteArrivalsBinary, AddArr
  USE writeRay,     only: WriteRay2D

  IMPLICIT NONE
  PRIVATE

! public interfaces
!=======================================================================

    public InfluenceGeoHatRayCen, InfluenceSGB, InfluenceGeoGaussianCart, &
           InfluenceGeoHatCart, ScalePressure

!=======================================================================

  INTEGER,              PRIVATE :: iz, ir, iS
  REAL    (KIND=_RL90), PRIVATE :: Ratio1 = 1.0D0 ! scale factor for a line source
  REAL    (KIND=_RL90), PRIVATE :: W, s, n, Amp, phase, const, phaseInt, &
                                   q0, q, qold, RcvrDeclAngle, rA, rB
  COMPLEX (KIND=_RL90), PRIVATE :: delay

CONTAINS
  SUBROUTINE InfluenceGeoHatRayCen( U, alpha, dalpha )

    ! Geometrically-spreading beams with a hat-shaped beam in ray-centered 
    ! coordinates

    REAL (KIND=_RL90), INTENT( IN    ) :: alpha, dalpha ! take-off angle
    COMPLEX,           INTENT( INOUT ) :: U( NRz_per_range, Pos%NRr )   ! complex pressure field
    INTEGER              :: irA, irB, II
    REAL (KIND=_RL90)    :: nA, nB, zr, L, dq( Beam%Nsteps - 1 )
    REAL (KIND=_RL90)    :: znV( Beam%Nsteps ), rnV( Beam%Nsteps ), &
                            RcvrDeclAngleV ( Beam%Nsteps )
    COMPLEX (KIND=_RL90) :: dtau( Beam%Nsteps - 1 )

    !!! need to add logic related to NRz_per_range

    q0           = ray2D( 1 )%c / Dalpha   ! Reference for J = q0 / q
    SrcDeclAngle = RadDeg * alpha          ! take-off angle in degrees

    dq   = ray2D( 2:Beam%Nsteps )%q( 1 ) - ray2D( 1:Beam%Nsteps-1 )%q( 1 )
    dtau = ray2D( 2:Beam%Nsteps )%tau    - ray2D( 1:Beam%Nsteps-1 )%tau

    ! Set the ray-centered coordinates (znV, rnV)
    ! pre-calculate ray normal based on tangent with c(s) scaling
    znV = -ray2D( 1:Beam%Nsteps )%t( 1 ) * ray2D( 1:Beam%Nsteps )%c
    rnV =  ray2D( 1:Beam%Nsteps )%t( 2 ) * ray2D( 1:Beam%Nsteps )%c

    RcvrDeclAngleV( 1:Beam%Nsteps ) = RadDeg * &
        ATAN2( ray2D( 1:Beam%Nsteps )%t( 2 ), ray2D( 1:Beam%Nsteps )%t( 1 ) )

    ! During reflection imag(q) is constant and adjacent normals cannot bracket 
    ! a segment of the TL line, so no special treatment is necessary
 
    ! point source (cylindrical coordinates): default behavior
    IF ( Beam%RunType( 4:4 ) == 'R' ) Ratio1 = SQRT( ABS( COS( alpha ) ) ) 

    ray2D( 1:Beam%Nsteps )%Amp = Ratio1 * SQRT( ray2D( 1:Beam%Nsteps )%c ) &
                        * ray2D( 1:Beam%Nsteps )%Amp   ! pre-apply some scaling

    RcvrDepths: DO iz = 1, NRz_per_range
       zR = Pos%Rz( iz )

       phase = 0.0
       qOld  = ray2D( 1 )%q( 1 ) ! used to track KMAH index
       
       ! If normal is parallel to horizontal receiver line
       IF ( ABS( znV( 1 ) ) < 1D-6 ) THEN   
          nA  = 1D10
          rA  = 1D10
          irA = 1
       ELSE
          nA  = ( zR - ray2D( 1 )%x( 2 ) ) / znV( 1 )
          rA  = ray2D( 1 )%x( 1 ) + nA*rnV( 1 )
          !!! Find index of receiver: assumes uniform spacing in Pos%Rr
          irA = MAX( MIN( INT( ( rA - Pos%Rr( 1 ) ) / Pos%Delta_r )+1,  &
                          Pos%NRr ),                                    &
                     1 )
       END IF

       Stepping: DO iS = 2, Beam%Nsteps
          ! Compute ray-centered coordinates, (znV, rnV)
 
          ! If normal is parallel to TL-line, skip to next step on ray
          IF ( ABS( znV( iS ) ) < 1D-10 ) CYCLE Stepping  
          nB  = ( zR - ray2D( iS )%x( 2 ) ) / znV( iS )
          rB  = ray2D( iS )%x( 1 ) + nB * rnV( iS )

          ! Find index of receiver: assumes uniform spacing in Pos%Rr
          irB = MAX( MIN( INT( ( rB - Pos%Rr( 1 ) ) / Pos%Delta_r )+1,  &
                          Pos%NRr ),&
                     1 )

          ! detect and skip duplicate points (happens at boundary reflection)
          IF ( ABS( ray2D( iS )%x( 1 ) - ray2D( iS-1 )%x( 1 ) )     &
               < 1.0D3 * SPACING( ray2D( iS )%x( 1 ) )              &
               .OR. irA == irB ) THEN
             rA  = rB
             nA  = nB
             irA = irB
             CYCLE Stepping
          END IF

          !!! this should be pre-computed
          q  = ray2D( iS-1 )%q( 1 )
          ! if phase shifts at caustics
          IF (     q <= 0.0d0 .AND. qOld > 0.0d0    &
              .OR. q >= 0.0d0 .AND. qOld < 0.0d0 )  &
              phase = phase + pi/2.  
          qOld = q

          RcvrDeclAngle = RcvrDeclAngleV( iS )

          ! *** Compute contributions to bracketted receivers ***
          II = 0
          IF ( irB <= irA ) II = 1   ! going backwards in range
    
          ! Compute influence for each rcvr
          RcvrRanges: DO ir = irA + 1 - II, irB + II, SIGN( 1, irB-irA )  
             W = ( Pos%Rr( ir ) - rA ) / ( rB - rA ) ! relative range between rR
             n = ABS( nA              + W*( nB - nA ) )
             q = ray2D( iS-1 )%q( 1 ) + W*dq( iS-1 )  ! interpolated amplitude, IESCO22: isn't q a unit normal aka no units?
             L = ABS( q ) / q0   ! beam radius

             IF ( n < L ) THEN   ! in beamwindow: update delay, Amp, phase
                delay    = ray2D( iS-1 )%tau + W*dtau( iS-1 ) 
                const    = ray2D( iS )%Amp / SQRT( ABS( q ) ) 
                W        = ( L - n ) / L ! hat function: 1 on center, 0 on edge
                Amp      = const*W
                phaseInt = ray2D( iS-1 )%Phase + phase
                !!! this should be precomputed
                IF (     q <= 0.0d0 .AND. qOld > 0.0d0      &
                    .OR. q >= 0.0d0 .AND. qOld < 0.0d0 )    &
                    phaseInt = phase + pi/2.   ! phase shifts at caustics

                CALL ApplyContribution( U( iz, ir ) )
             END IF
          END DO RcvrRanges
          rA  = rB
          nA  = nB
          irA = irB
       END DO Stepping
    END DO RcvrDepths

  END SUBROUTINE InfluenceGeoHatRayCen

  ! **********************************************************************!

  SUBROUTINE InfluenceGeoHatCart( U, alpha, Dalpha )

    ! Geometric, hat-shaped beams in Cartesisan coordinates

    REAL (KIND=_RL90), INTENT( IN    ) :: alpha, Dalpha ! take-off angle, angular spacing
    COMPLEX,           INTENT( INOUT ) :: U( NRz_per_range, Pos%NRr ) ! complex pressure field
    INTEGER              :: irT( 1 ), irTT
    REAL (KIND=_RL90)    :: x_ray( 2 ), rayt( 2 ), rayn( 2 ), &
                            x_rcvr( 2, NRz_per_range ), rLen, RadiusMax, &
                            zMin, zMax, dqds
    COMPLEX (KIND=_RL90) :: dtauds

    q0           = ray2D( 1 )%c / Dalpha   ! Reference for J = q0 / q
    SrcDeclAngle = RadDeg * alpha          ! take-off angle in degrees
    phase        = 0.0
    qOld         = ray2D( 1 )%q( 1 )       ! used to track KMAH index
    rA           = ray2D( 1 )%x( 1 )       ! range at start of ray

    ! what if never satistified?
    ! what if there is a single receiver (ir = 0 possible)

    ! find index of first receiver to the right of rA
    irT = MINLOC( Pos%Rr( 1 : Pos%NRr ), MASK = Pos%Rr( 1 : Pos%NRr ) > rA )   
    ir  = irT( 1 )
    ! if ray is left-traveling, get the first receiver to the left of rA
    IF ( ray2D( 1 )%t( 1 ) < 0.0d0 .AND. ir > 1 ) ir = ir - 1  

    ! point source: the default option
    IF ( Beam%RunType( 4 : 4 ) == 'R' ) Ratio1 = SQRT( ABS( COS( alpha ) ) )  

    Stepping: DO iS = 2, Beam%Nsteps
       rB     = ray2D( iS     )%x( 1 )
       x_ray  = ray2D( iS - 1 )%x

       ! compute normalized tangent (compute it because we need to measure the 
       ! step length)
       rayt = ray2D( iS )%x - ray2D( iS - 1 )%x
       rlen = NORM2( rayt )
       ! if duplicate point in ray, skip to next step along the ray
       IF ( rlen < 1.0D3 * SPACING( ray2D( iS )%x( 1 ) ) ) &
           CYCLE Stepping  
       rayt = rayt / rlen                    ! unit tangent to ray
       rayn = [ -rayt( 2 ), rayt( 1 ) ]      ! unit normal  to ray
       RcvrDeclAngle = RadDeg * ATAN2( rayt( 2 ), rayt( 1 ) )

       dqds   = ray2D( iS )%q( 1 ) - ray2D( iS - 1 )%q( 1 )
       dtauds = ray2D( iS )%tau    - ray2D( iS - 1 )%tau

       q  = ray2D( iS - 1 )%q( 1 )
       IF ( q <= 0.0d0 .AND. qOld > 0.0d0 .OR. q >= 0.0d0 .AND. qOld < 0.0d0 ) &
           phase = phase + pi / 2.   ! phase shifts at caustics
       qold = q

       RadiusMax = MAX( ABS( ray2D( iS - 1 )%q( 1 ) ), &
                        ABS( ray2D( iS )%q( 1 ) ) ) &
            / q0 / ABS( rayt( 1 ) ) ! beam radius projected onto vertical line

       ! depth limits of beam
       IF ( ABS( rayt( 1 ) ) > 0.5 ) THEN   ! shallow angle ray
          zmin   = min( ray2D( iS-1 )%x( 2 ), ray2D( iS )%x( 2 ) ) - RadiusMax
          zmax   = max( ray2D( iS-1 )%x( 2 ), ray2D( iS )%x( 2 ) ) + RadiusMax
       ELSE                                 ! steep angle ray
          zmin = -HUGE( zmin )
          zmax = +HUGE( zmax )
       END IF

       ! compute beam influence for this segment of the ray
       RcvrRanges: DO
          ! is Rr( ir ) contained in [ rA, rB )? Then compute beam influence
          IF ( Pos%Rr( ir ) >= MIN( rA, rB ) &
               .AND. Pos%Rr( ir ) < MAX( rA, rB ) ) THEN
             
             x_rcvr( 1, 1 : NRz_per_range ) = Pos%Rr( ir )
             IF ( Beam%RunType( 5 : 5 ) == 'I' ) THEN
                x_rcvr( 2, 1 ) = Pos%Rz( ir ) ! irregular grid
             ELSE
                x_rcvr( 2, 1:NRz_per_range ) = Pos%Rz( 1:NRz_per_range )   ! rectilinear grid
             END IF

             RcvrDepths: DO iz = 1, NRz_per_range
                ! is x_rcvr( 2, iz ) contained in ( zmin, zmax )?
                IF ( x_rcvr( 2, iz ) < zmin &
                     .OR. x_rcvr( 2, iz ) > zmax ) CYCLE RcvrDepths
                ! proportional distance along ray
                s = DOT_PRODUCT( x_rcvr( :, iz ) - x_ray, rayt ) / rlen 
                ! normal distance to ray
                n = ABS( DOT_PRODUCT( x_rcvr( :, iz ) - x_ray, rayn ) )      
                ! interpolated amplitude
                q = ray2D( iS-1 )%q( 1 ) + s * dqds               
                ! beam radius
                RadiusMax = ABS( q / q0 )                                   

                IF ( n < RadiusMax ) THEN
                   ! interpolated delay
                   delay    = ray2D( iS - 1 )%tau + s * dtauds              
                   const    = Ratio1 * SQRT( ray2D( iS )%c / ABS( q ) ) &
                              * ray2D( iS )%Amp
                   ! hat function: 1 on center, 0 on edge
                   W        = ( RadiusMax - n ) / RadiusMax   
                   Amp      = const * W
                   phaseInt = ray2D( iS - 1 )%Phase + phase
                   IF ( q <= 0.0d0 .AND. qOld > 0.0d0 &
                        .OR. q >= 0.0d0 .AND. qOld < 0.0d0 ) &
                    phaseInt = phase + pi / 2.   ! phase shifts at caustics
                    ! IESCO22: shouldn't this be = phaseInt + pi/2

                   CALL ApplyContribution( U( iz, ir ) )
                END IF
             END DO RcvrDepths
          END IF

          ! bump receiver index, ir, towards rB
          IF ( Pos%Rr( ir ) < rB ) THEN
             IF ( ir >= Pos%NRr        ) EXIT  ! go to next step on ray
             irTT = ir + 1                     ! bump right
             IF ( Pos%Rr( irTT ) >= rB ) EXIT
          ELSE
             IF ( ir <= 1              ) EXIT  ! go to next step on ray
             irTT = ir - 1                     ! bump left
             IF ( Pos%Rr( irTT ) <= rB ) EXIT
          END IF
          ir = irTT
       END DO RcvrRanges

       WRITE( PRTFile, * ) "a = ", alpha, "; RadiusMax = ", RadiusMax
       rA = rB
    END DO Stepping

  END SUBROUTINE InfluenceGeoHatCart

  ! **********************************************************************!

  SUBROUTINE InfluenceGeoGaussianCart( U, alpha, Dalpha )

    ! Geometric, Gaussian beams in Cartesian coordintes
    
    ! beam window: kills beams outside e**(-0.5 * ibwin**2 )
    INTEGER,           PARAMETER       :: BeamWindow = 4 
    REAL (KIND=_RL90), INTENT( IN    ) :: alpha, dalpha ! take-off angle, angular spacing
    COMPLEX,           INTENT( INOUT ) :: U( NRz_per_range, Pos%NRr )  ! complex pressure field
    INTEGER              :: irT( 1 ), irTT
    REAL (KIND=_RL90)    :: x_ray( 2 ), rayt( 2 ), rayn( 2 ), x_rcvr( 2 ), &
                            rLen, RadiusMax, zMin, zMax, sigma, lambda, A, dqds
    COMPLEX (KIND=_RL90) :: dtauds

    q0           = ray2D( 1 )%c / Dalpha   ! Reference for J = q0 / q
    SrcDeclAngle = RadDeg * alpha          ! take-off angle in degrees
    phase        = 0
    qOld         = ray2D( 1 )%q( 1 )       ! used to track KMAH index
    rA           = ray2D( 1 )%x( 1 )       ! range at start of ray

    ! what if never satistified?
    ! what if there is a single receiver (ir = 0 possible)

    ! irT: find index of first receiver to the right of rA
    irT = MINLOC( Pos%Rr( 1 : Pos%NRr ), MASK = Pos%Rr( 1 : Pos%NRr ) > rA )     
    ir  = irT( 1 )

    ! if ray is left-traveling, get the first receiver to the left of rA
    IF ( ray2D( 1 )%t( 1 ) < 0.0d0 .AND. ir > 1 ) ir = ir - 1  

    ! sqrt( 2 * pi ) represents a sum of Gaussians in free space
    IF ( Beam%RunType( 4 : 4 ) == 'R' ) THEN
       Ratio1 = SQRT( ABS( COS( alpha ) ) ) / SQRT( 2. * pi )   ! point source
    ELSE
       Ratio1 = 1 / SQRT( 2. * pi )                             ! line  source
    END IF

    Stepping: DO iS = 2, Beam%Nsteps

       rB    = ray2D( iS     )%x( 1 )
       x_ray = ray2D( iS - 1 )%x

       ! compute normalized tangent (compute it because we need to measure the 
       ! step length)
       rayt = ray2D( iS )%x - ray2D( iS - 1 )%x
       rlen = NORM2( rayt )

       ! if duplicate point in ray, skip to next step along the ray
       IF ( rlen < 1.0D3 * SPACING( ray2D( iS )%x( 1 ) ) ) CYCLE Stepping  
       rayt = rayt / rlen
       rayn = [ -rayt( 2 ), rayt( 1 ) ]      ! unit normal to ray
       RcvrDeclAngle = RadDeg * ATAN2( rayt( 2 ), rayt( 1 ) )

       dqds   = ray2D( iS )%q( 1 ) - ray2D( iS - 1 )%q( 1 )
       dtauds = ray2D( iS )%tau    - ray2D( iS - 1 )%tau

       q  = ray2D( iS - 1 )%q( 1 )
       IF ( q <= 0.0 .AND. qOld > 0.0 &
            .OR. q >= 0.0 .AND. qOld < 0.0 ) &
        phase = phase + pi / 2.   ! phase shifts at caustics
       qold = q

       ! calculate beam width
       lambda    = ray2D( iS - 1 )%c / freq
       sigma     = MAX( ABS( ray2D( iS-1 )%q( 1 ) ), ABS( ray2D( iS )%q( 1 ) ) )&
           / q0 / ABS( rayt( 1 ) ) ! beam radius projected onto vertical line
       sigma     = MAX( sigma, &
                    MIN( 0.2 * freq * REAL( ray2D( iS )%tau ), pi * lambda ) )
       RadiusMax = BeamWindow * sigma

       ! depth limits of beam
       IF ( ABS( rayt( 1 ) ) > 0.5 ) THEN   ! shallow angle ray
          zmin   = min( ray2D( iS-1 )%x( 2 ), ray2D( iS )%x( 2 ) ) - RadiusMax
          zmax   = max( ray2D( iS-1 )%x( 2 ), ray2D( iS )%x( 2 ) ) + RadiusMax
       ELSE                                 ! steep angle ray
          zmin = -HUGE( zmin )
          zmax = +HUGE( zmax )
       END IF

       ! compute beam influence for this segment of the ray
       RcvrRanges: DO
          ! is Rr( ir ) contained in [ rA, rB )? Then compute beam influence
          IF ( Pos%Rr( ir ) >= MIN( rA, rB ) &
               .AND. Pos%Rr( ir ) < MAX( rA, rB ) ) THEN

             RcvrDepths: DO iz = 1, NRz_per_range
                IF ( Beam%RunType( 5 : 5 ) == 'I' ) THEN
                   x_rcvr = [ Pos%Rr( ir ), Pos%Rz( ir ) ]   ! irregular   grid
                ELSE
                   x_rcvr = [ Pos%Rr( ir ), Pos%Rz( iz ) ]   ! rectilinear grid
                END IF
                IF ( x_rcvr( 2 ) < zmin &
                     .OR. x_rcvr( 2 ) > zmax ) CYCLE RcvrDepths

                ! proportional distance along ray
                s = DOT_PRODUCT( x_rcvr - x_ray, rayt ) / rlen  
                ! normal distance to ray
                n = ABS( DOT_PRODUCT( x_rcvr - x_ray, rayn ) )       
                ! interpolated amplitude
                q = ray2D( iS-1 )%q( 1 ) + s * dqds               
                ! beam radius
                sigma  = ABS( q / q0 )                                    
                sigma  = MAX( sigma, &
                    MIN( 0.2 * freq * REAL( ray2D( iS )%tau ), pi * lambda ) ) 

                IF ( n < BeamWindow * sigma ) THEN   ! Within beam window?
                   A        = ABS( q0 / q )
                   delay    = ray2D( iS-1 )%tau + s * dtauds ! interpolated delay
                   const    = Ratio1 * SQRT( ray2D( iS )%c / ABS( q ) ) &
                              * ray2D( iS )%Amp
                   ! W : Gaussian decay
                   W        = EXP( -0.5 * ( n / sigma ) ** 2 ) / ( sigma * A )   
                   Amp      = const * W
                   phaseInt = ray2D( iS )%Phase + phase
                   IF ( q <= 0.0d0 .AND. qOld > 0.0d0 &
                        .OR. q >= 0.0d0 .AND. qOld < 0.0d0 ) &
                    phaseInt = phase + pi / 2.  ! phase shifts at caustics

                   CALL ApplyContribution( U( iz, ir ) )
                END IF
             END DO RcvrDepths
          END IF

          ! receiver not bracketted; bump receiver index, ir, towards rB
          IF ( rB > Pos%Rr( ir ) ) THEN
             IF ( ir >= Pos%NRr        ) EXIT   ! go to next step on ray
             irTT = ir + 1                      ! bump right
             IF ( Pos%Rr( irTT ) >= rB ) EXIT   ! go to next step on ray
          ELSE
             IF ( ir <= 1              ) EXIT   ! go to next step on ray
             irTT = ir - 1                      ! bump left
             IF ( Pos%Rr( irTT ) <= rB ) EXIT   ! go to next step on ray
          END IF
          ir = irTT

       END DO RcvrRanges

       rA = rB
    END DO Stepping

  END SUBROUTINE InfluenceGeoGaussianCart

  ! **********************************************************************!
  
  SUBROUTINE ApplyContribution( U )
    COMPLEX, INTENT( INOUT ) :: U
    
    SELECT CASE( Beam%RunType( 1 : 1 ) )
    CASE ( 'E' )                ! eigenrays
       CALL WriteRay2D( SrcDeclAngle, iS )
    CASE ( 'A', 'a' )           ! arrivals
       CALL AddArr( omega, iz, ir, Amp, phaseInt, delay, SrcDeclAngle, &
                    RcvrDeclAngle, ray2D( iS )%NumTopBnc, &
                    ray2D( iS )%NumBotBnc )
    CASE ( 'C' )                ! coherent TL
       U = U + CMPLX( Amp * EXP( -i * ( omega * delay - phaseInt ) ) )
    CASE ( 'S', 'I' )                ! incoherent/semicoherent TL
       IF ( Beam%Type( 1:1 ) == 'B' ) THEN   ! Gaussian beam
          U = U + SNGL( SQRT( 2. * pi ) &
                  * ( const * EXP( AIMAG( omega * delay ) ) )**2 * W )
       ELSE
          U = U + SNGL( &
                    ( const * EXP( AIMAG( omega * delay ) ) )**2 * W )
       END IF
    CASE DEFAULT                ! incoherent/semicoherent TL
       IF ( Beam%Type( 1:1 ) == 'B' ) THEN   ! Gaussian beam
          U = U + SNGL( SQRT( 2. * pi ) &
                  * ( const * EXP( AIMAG( omega * delay ) ) )**2 * W )
       ELSE
          U = U + SNGL( &
                    ( const * EXP( AIMAG( omega * delay ) ) )**2 * W )
       END IF
    END SELECT

  END SUBROUTINE ApplyContribution
                 
  ! **********************************************************************!

  SUBROUTINE InfluenceSGB( U, alpha, Dalpha )

    ! Bucker's Simple Gaussian Beams in Cartesian coordinates

    REAL (KIND=_RL90), INTENT( IN    ) :: alpha, dalpha ! take-off angle, angular spacing
    COMPLEX,           INTENT( INOUT ) :: U( NRz_per_range, Pos%NRr ) ! complex pressure field
    REAL (KIND=_RL90)     :: x( 2 ), rayt( 2 ), A, beta, cn, CPA, deltaz, DS, & 
                             sint, SX1, thet
    COMPLEX (KIND=_RL90)  :: contri, tau

    Ratio1 = SQRT(  COS( alpha ) )
    phase  = 0
    qOld   = 1.0
    BETA   = 0.98  ! Beam Factor
    A      = -4.0 * LOG( BETA ) / Dalpha**2
    CN     = Dalpha * SQRT( A / pi )
    rA     = ray2D( 1 )%x( 1 )
    ir     = 1

    Stepping: DO iS = 2, Beam%Nsteps

       rB = ray2D( iS )%x( 1 )

       ! phase shifts at caustics
       q  = ray2D( iS - 1 )%q( 1 )
       IF ( q < 0.0d0 .AND. qOld >= 0.0d0 &
            .OR. q > 0.0d0 .AND. qOld <= 0.0d0 ) &
        phase = phase + pi / 2.
       qold = q

       RcvrRanges: DO WHILE ( ABS( rB - rA ) > 1.0D3 * SPACING( rA ) &
           .AND. rB > Pos%Rr( ir ) )   ! Loop over bracketted receiver ranges

          W    = ( Pos%Rr( ir ) - rA ) / ( rB - rA )
          x    = ray2D( iS-1 )%x    + W * ( ray2D( iS )%x    - ray2D( iS-1 )%x )
          rayt = ray2D( iS-1 )%t    + W * ( ray2D( iS )%t    - ray2D( iS-1 )%t )
          q    = ray2D( iS-1 )%q(1) + W * ( ray2D( iS )%q(1) - ray2D( iS-1 )%q(1) )
          tau  = ray2D( iS-1 )%tau  + W * ( ray2D( iS )%tau  - ray2D( iS-1 )%tau )

          ! following is incorrect because ray doesn't always use a step of deltas
          SINT = ( iS-1 ) * Beam%deltas + W * Beam%deltas

          IF ( q < 0.0d0 .AND. qOld >= 0.0d0 &
               .OR. q > 0.0d0 .AND. qOld <= 0.0d0 ) &
            phase = phase + pi / 2. ! phase shifts at caustics

          RcvrDepths: DO iz = 1, NRz_per_range
             deltaz =  Pos%Rz( iz ) - x( 2 )   ! ray to rcvr distance
             ! Adeltaz    = ABS( deltaz )
             ! IF ( Adeltaz < RadiusMax ) THEN
             SELECT CASE( Beam%RunType( 1 : 1 ) )
             CASE ( 'E' )         ! eigenrays
                SrcDeclAngle = RadDeg * alpha   ! take-off angle in degrees
                CALL WriteRay2D( SrcDeclAngle, iS )
             CASE DEFAULT         ! coherent TL
                CPA    = ABS( deltaz * ( rB - rA ) ) / SQRT( ( rB - rA )**2 &
                         + ( ray2D( iS )%x( 2 ) - ray2D( iS-1 )%x( 2 ) )**2  )
                DS     = SQRT( deltaz **2 - CPA **2 )
                SX1    = SINT + DS
                thet   = ATAN( CPA / SX1 )
                delay  = tau + rayt( 2 ) * deltaz
                contri = Ratio1 * CN * ray2D( iS )%Amp &
                       * EXP( -A * thet**2 &
                              - i*( omega*delay - ray2D( iS )%Phase - phase ) )&
                       / SQRT( SX1 )
                U( iz, ir ) = U( iz, ir ) + CMPLX( contri )
             END SELECT
             ! END IF
          END DO RcvrDepths

          qOld = q
          ir   = ir + 1
          IF ( ir > Pos%NRr ) RETURN
       END DO RcvrRanges

       rA = rB
    END DO Stepping

  END SUBROUTINE InfluenceSGB

  ! **********************************************************************!

  SUBROUTINE ScalePressure( Dalpha, c, r, U, NRz, Nr, RunType, freq )

    ! Scale the pressure field

    INTEGER,           INTENT( IN    ) :: NRz, Nr
    REAL (KIND=_RL90), INTENT( IN    ) :: r( Nr )   ! Rr ranges
    REAL (KIND=_RL90), INTENT( IN    ) :: Dalpha, freq, c ! angular spacing between rays, source frequency, nominal sound speed
    COMPLEX,           INTENT( INOUT ) :: U( NRz, Nr )    ! Pressure field
    CHARACTER (LEN=5), INTENT( IN    ) :: RunType
    REAL (KIND=_RL90)                  :: const, factor

    ! Compute scale factor for field
    SELECT CASE ( RunType( 2 : 2 ) )
    CASE ( 'C' )   ! Cerveny Gaussian beams in Cartesian coordinates
       const = -Dalpha * SQRT( freq ) / c
    CASE ( 'R' )   ! Cerveny Gaussian beams in Ray-centered coordinates
       const = -Dalpha * SQRT( freq ) / c
    CASE DEFAULT
       const = -1.0
    END SELECT

    ! If incoherent run, convert intensity to pressure
    IF ( RunType( 1 : 1 ) /= 'C' ) U = SQRT( REAL( U ) ) 

    ! scale and/or incorporate cylindrical spreading
    Ranges: DO ir = 1, Nr
       IF ( RunType( 4 : 4 ) == 'X' ) THEN   ! line source
          factor = -4.0 * SQRT( pi ) * const
       ELSE                                  ! point source
          IF ( r ( ir ) == 0 ) THEN
             factor = 0.0D0         ! avoid /0 at origin, return pressure = 0
          ELSE
             factor = const / SQRT( ABS( r( ir ) ) )
          END IF
       END IF
       U( :, ir ) = SNGL( factor ) * U( :, ir )
    END DO Ranges

  END SUBROUTINE ScalePressure

  ! **********************************************************************!

  REAL (KIND=_RL90) FUNCTION Hermite( x, x1, x2 )

    ! Calculates a smoothing function based on the h0 hermite cubic
    ! x is the point where the function is to be evaluated
    ! returns:
    ! [  0, x1  ] = 1
    ! [ x1, x2  ] = cubic taper from 1 to 0
    ! [ x2, inf ] = 0

    REAL (KIND=_RL90 ), INTENT( IN  ) :: x, x1, x2
    REAL (KIND=_RL90 )                :: Ax, u

    Ax  = ABS( x  )

    IF ( Ax <= x1 ) THEN
       Hermite = 1.0d0
    ELSE IF ( Ax >= x2 ) THEN
       Hermite = 0.0d0
    ELSE
       u       = ( Ax - x1 ) / ( x2 - x1 )
       Hermite = ( 1.0d0 + 2.0d0 * u ) * ( 1.0d0 - u ) ** 2
    END IF


  END FUNCTION Hermite

END MODULE influence
