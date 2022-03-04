!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                   !!
!!                   GNU General Public License                      !!
!!                                                                   !!
!! This file is part of the Flexible Modeling System (FMS).          !!
!!                                                                   !!
!! FMS is free software; you can redistribute it and/or modify       !!
!! it and are expected to follow the terms of the GNU General Public !!
!! License as published by the Free Software Foundation.             !!
!!                                                                   !!
!! FMS is distributed in the hope that it will be useful,            !!
!! but WITHOUT ANY WARRANTY; without even the implied warranty of    !!
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     !!
!! GNU General Public License for more details.                      !!
!!                                                                   !!
!! You should have received a copy of the GNU General Public License !!
!! along with FMS; if not, write to:                                 !!
!!          Free Software Foundation, Inc.                           !!
!!          59 Temple Place, Suite 330                               !!
!!          Boston, MA  02111-1307  USA                              !!
!! or see:                                                           !!
!!          http://www.gnu.org/licenses/gpl.txt                      !!
!!                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#include "BELLHOP_OPTIONS_90.h"
!BOP
! !INTERFACE:
MODULE constants_mod
! <CONTACT EMAIL="ivana@utexas.edu">
!   Ivana Escobar
! </CONTACT>

! <OVERVIEW>
!    Defines useful constants for Underwater Acoustics.
! </OVERVIEW>

! <DESCRIPTION>
!   Constants are defined as real parameters,
!   except for PI and RADIAN, which are calculated in constants_init to promote
!   consistency and accuracy among various compilers.
!
!   Constants are accessed through the "use" statement.
! </DESCRIPTION>

  ! SAVE
  implicit none
  private

   REAL (KIND=_RL90), PUBLIC, PARAMETER :: pi = 3.1415926535898D0, &
                                           RadDeg = 180.0D0 / pi, &
                                           DegRad = pi / 180.0D0 
   COMPLEX (KIND=_RL90), PUBLIC, PARAMETER :: i = ( 0.0D0, 1.0D0 )

   INTEGER, PUBLIC, PARAMETER :: ENVFile = 5, PRTFile = 6, RAYFile = 21, &
                                 SHDFile = 25, ARRFile = 36, SSPFile = 40, &
                                 MaxN = 10000
  ! Reduce MaxN (= max # of steps along a ray) to reduce storage
  ! Note space is wasted in NumTopBnc, NumBotBnc ...

END MODULE constants_mod
