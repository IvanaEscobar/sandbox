#include "IHOP_OPTIONS.h"
!BOP
! !INTERFACE:
MODULE iHopMod
! <CONTACT EMAIL="ivana@utexas.edu">
!   Ivana Escobar
! </CONTACT>

! <OVERVIEW>
!    Defines modules used in BELLHOP
! </OVERVIEW>

! <DESCRIPTION>
! </DESCRIPTION>

  USE iHopParams,   only: MaxN ! 10000
  IMPLICIT NONE
  PRIVATE

  ! Reduce MaxN (= max # of steps along a ray) to reduce storage
  ! Note space is wasted in NumTopBnc, NumBotBnc ...

! public interfaces
!=======================================================================

    public Nrz_per_range, iStep, freq, omega, SrcDeclAngle, Title, &
           BeamStructure, Beam, &
           ray2D, ray2DPt

!=======================================================================

  INTEGER            :: Nrz_per_range, iStep
  REAL (KIND=_RL90)  :: freq, omega, SrcDeclAngle, SrcAzimAngle
  CHARACTER (LEN=80) :: Title

  ! *** Beam structure ***

  TYPE rxyz
     REAL (KIND=_RL90) :: r, x, y, z
  END TYPE rxyz

  TYPE BeamStructure
     INTEGER           :: NBeams, Nimage, Nsteps, iBeamWindow
     REAL (KIND=_RL90) :: deltas, epsMultiplier = 1, rLoop
     CHARACTER (LEN=1) :: Component ! Pressure or displacement
     CHARACTER (LEN=4) :: Type = 'G S '
     CHARACTER (LEN=7) :: RunType
     TYPE( rxyz )      :: Box
  END TYPE BeamStructure

  TYPE( BeamStructure ) :: Beam

  ! *** ray structure ***

  TYPE ray2DPt
     INTEGER                :: NumTopBnc, NumBotBnc
     REAL    (KIND=_RL90)   :: x( 2 ), t( 2 ), p( 2 ), q( 2 ), c, Amp, Phase
     COMPLEX (KIND=_RL90)   :: tau
  END TYPE ray2DPt
  TYPE( ray2DPt )      :: ray2D( MaxN )

END MODULE iHopMod
