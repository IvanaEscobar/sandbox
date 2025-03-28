C                           DISCLAIMER
C 
C   This file was generated by TAF version 5.9.12
C 
C   FASTOPT DISCLAIMS  ALL  WARRANTIES,  EXPRESS  OR  IMPLIED,
C   INCLUDING (WITHOUT LIMITATION) ALL IMPLIED  WARRANTIES  OF
C   MERCHANTABILITY  OR FITNESS FOR A PARTICULAR PURPOSE, WITH
C   RESPECT TO THE SOFTWARE AND USER PROGRAMS.   IN  NO  EVENT
C   SHALL  FASTOPT BE LIABLE FOR ANY LOST OR ANTICIPATED PROF-
C   ITS, OR ANY INDIRECT, INCIDENTAL, EXEMPLARY,  SPECIAL,  OR
C   CONSEQUENTIAL  DAMAGES, WHETHER OR NOT FASTOPT WAS ADVISED
C   OF THE POSSIBILITY OF SUCH DAMAGES.
C 
C                           Haftungsbeschraenkung
C   FastOpt gibt ausdruecklich keine Gewaehr, explizit oder indirekt,
C   bezueglich der Brauchbarkeit  der Software  fuer einen bestimmten
C   Zweck.   Unter  keinen  Umstaenden   ist  FastOpt   haftbar  fuer
C   irgendeinen Verlust oder nicht eintretenden erwarteten Gewinn und
C   allen indirekten,  zufaelligen,  exemplarischen  oder  speziellen
C   Schaeden  oder  Folgeschaeden  unabhaengig  von einer eventuellen
C   Mitteilung darueber an FastOpt.
C 
      program    get_csound
C******************************************************************
C******************************************************************
C** This routine was generated by Automatic differentiation.     **
C** FastOpt: Transformation of Algorithm in Fortran, TAF 5.9.12  **
C******************************************************************
C******************************************************************
      implicit none

C==============================================
C declare parameters
C==============================================
      integer, parameter :: n = 10

C==============================================
C declare local variables
C==============================================
      double precision :: csound(n,n)
      integer :: i
      integer :: j
      double precision :: pres(n,n)
      double precision :: sal(n,n)
      double precision :: temp(n,n)

C==============================================
C declare external procedures and functions
C==============================================
      double precision, external :: fwd_c

      do i = 1, n
        do j = 1, n
          sal(i,j) = 34.0+1.0d-2*i+1.0d-2*j
          temp(i,j) = 21.0+i+j
          pres(i,j) = 10.0
        end do
      end do
      do i = 1, n
        do j = 1, n
          csound(i,j) = fwd_c(sal(i,j),temp(i,j),pres(i,j))
        end do
      end do
      end program    get_csound
