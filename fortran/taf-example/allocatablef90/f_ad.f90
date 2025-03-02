!                           DISCLAIMER
! 
!   This file was generated by TAF version 6.1.5
! 
!   FASTOPT DISCLAIMS  ALL  WARRANTIES,  EXPRESS  OR  IMPLIED,
!   INCLUDING (WITHOUT LIMITATION) ALL IMPLIED  WARRANTIES  OF
!   MERCHANTABILITY  OR FITNESS FOR A PARTICULAR PURPOSE, WITH
!   RESPECT TO THE SOFTWARE AND USER PROGRAMS.   IN  NO  EVENT
!   SHALL  FASTOPT BE LIABLE FOR ANY LOST OR ANTICIPATED PROF-
!   ITS, OR ANY INDIRECT, INCIDENTAL, EXEMPLARY,  SPECIAL,  OR
!   CONSEQUENTIAL  DAMAGES, WHETHER OR NOT FASTOPT WAS ADVISED
!   OF THE POSSIBILITY OF SUCH DAMAGES.
! 
!                           Haftungsbeschraenkung
!   FastOpt gibt ausdruecklich keine Gewaehr, explizit oder indirekt,
!   bezueglich der Brauchbarkeit  der Software  fuer einen bestimmten
!   Zweck.   Unter  keinen  Umstaenden   ist  FastOpt   haftbar  fuer
!   irgendeinen Verlust oder nicht eintretenden erwarteten Gewinn und
!   allen indirekten,  zufaelligen,  exemplarischen  oder  speziellen
!   Schaeden  oder  Folgeschaeden  unabhaengig  von einer eventuellen
!   Mitteilung darueber an FastOpt.
! 

subroutine f_ad( a, x, x_ad, y, y_ad, i )
!******************************************************************
!******************************************************************
!** This routine was generated by Automatic differentiation.     **
!** FastOpt: Transformation of Algorithm in Fortran, TAF 6.1.5   **
!******************************************************************
!******************************************************************
implicit none

!==============================================
! declare parameters
!==============================================
integer, parameter :: ikind2 = 8

!==============================================
! declare arguments
!==============================================
real(kind=8), intent(in) :: a
integer, intent(in) :: i
real(kind=8), intent(in) :: x(i)
real(kind=8), intent(inout) :: x_ad(i)
real(kind=8), intent(inout) :: y(i)
real(kind=8), intent(inout) :: y_ad(i)

!==============================================
! declare local variables
!==============================================
integer :: ii

!----------------------------------------------
! FUNCTION AND TAPE COMPUTATIONS
!----------------------------------------------
do ii = 1, i
  y(ii) = a*x(ii)**4
end do

!----------------------------------------------
! ADJOINT COMPUTATIONS
!----------------------------------------------
do ii = 1, i
  x_ad(ii) = x_ad(ii)+4*y_ad(ii)*a*x(ii)**3
  y_ad(ii) = 0._ikind2
end do

end subroutine f_ad

subroutine f( a, x, y, i )
!******************************************************************
!******************************************************************
!** This routine was generated by Automatic differentiation.     **
!** FastOpt: Transformation of Algorithm in Fortran, TAF 6.1.5   **
!******************************************************************
!******************************************************************
implicit none

!==============================================
! declare arguments
!==============================================
real(kind=8), intent(in) :: a
integer :: i
real(kind=8), intent(in) :: x(i)
real(kind=8), intent(out) :: y(i)

!==============================================
! declare local variables
!==============================================
integer :: ii

do ii = 1, i
  y(ii) = a*x(ii)**4
end do
end subroutine f

