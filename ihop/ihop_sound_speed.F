#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#include "IHOP_OPTIONS.h"

CBOP
C     !ROUTINE: IHOP_SOUND_SPEED
C     !INTERFACE:
      SUBROUTINE IHOP_SOUND_SPEED(
     I                       myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R IHOP_SOUND_SPEED
C     | o Diagnose speed of sound in seawater
C     |   from the algorithm by Chen and Millero (1977).
C     |   This is NOT the sound-speed that can be derived from
C     |   the equation of state (EOS). It is independent of
C     |   the model setup specific EOS.
C     |
C     | o Reference:
C     | C. Chen and F. J. Millero, "Speed of sound in seawater at 
C     | high pressures,"
C     | J. Acoust. Soc. Am. 672.5, 1129-1135 (1977).
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
C#include "SURFACE.h"
#include "DYNVARS.h"
#include "EOS.h"
#include "IHOP_SIZE.h"
#include "IHOP.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myThid :: Thread number for this instance of the routine.
      INTEGER myThid

#ifdef ALLOW_IHOP 
C     !FUNCTIONS:
      _RL SW_TEMP
      EXTERNAL SW_TEMP
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi,bj
      INTEGER i,j,k
      INTEGER ii
      _RL c0,cw,a,b,d
      _RL pres,sal,temp
      LOGICAL calc_soundSpeed
CEOP

C      calc_soundSpeed = .FALSE.

C--   switch on this flag if Sound-Speed needed in any cost fct
c#ifdef ALLOW_CSOUND_COST
      calc_soundSpeed = .TRUE.
c#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        calc_soundSpeed = calc_soundSpeed
     &               .OR. DIAGNOSTICS_IS_ON( 'ihop_ssp', myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      IF ( calc_soundSpeed ) THEN
       c0   = 1402.388 _d 0
       cw   = 0. _d 0
       a    = 0. _d 0
       b    = 0. _d 0
       d    = 0. _d 0
       pres = 0. _d 0
       sal  = 0. _d 0
       temp = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
            ihop_ssp(i,j,k,bi,bj) = 0.0 _d 0
           ENDDO
          ENDDO
         ENDDO
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
            IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN
C pressure in dbar (for SW_TEMP)
             pres = rhoConst*( totPhiHyd(i,j,k,bi,bj)
     &               - rC(k)*gravity )*SItodBar
             temp = SW_TEMP( SALT(i,j,k,bi,bj),
     &                 THETA(i,j,k,bi,bj), pres, zeroRL )
             sal  = SALT(i,j,k,bi,bj)
C convert pressure to bar for Chen and Millero algorithm
             pres = pres/(1. _d 1)
             cw   = c0                      + 5.0383 _d 0*temp      +
     &              -5.81090 _d -2*temp**2  + 3.3432 _d -4*temp**3  + 
     &              -1.47797 _d -6*temp**4  + 3.1419 _d -9*temp**5  +
     &              (1.53563 _d -1          + 6.8999 _d -4*temp     +
     &               -8.1829 _d -6*temp**2  + 1.3632 _d -7*temp**3  +
     &               -6.1260 _d -10*temp**4)*pres                   + 
     &              (3.12600 _d -5          - 1.7111 _d -6*temp     +
     &               2.59860 _d -8*temp**2  - 2.5353 _d -10*temp**3 +
     &               1.04150 _d -12*temp**4)*pres**2                +
     &              (-9.7729 _d -9          + 3.8513 _d -10*temp    +  
     &               -2.3654 _d -12*temp**2)*pres**3
             a    = 1.389 _d 0              - 1.2620 _d -2*temp     + 
     &              7.166 _d -5*temp**2     + 2.0080 _d -6*temp**3  +
     &              -3.21 _d -8*temp**4                             +
     &              (9.47420 _d -5          - 1.2583 _d -5*temp     +
     &               -6.4928 _d -8*temp**2  + 1.0515 _d -8*temp**3  +
     &               -2.0142 _d -10*temp**4)*pres                   +
     &              (-3.9064 _d -7          + 9.1061 _d -9*temp     +
     &               -1.6009 _d -10*temp**2                         + 
     &               7.99400 _d -12*temp**3)*pres**2                +
     &              (1.10000 _d -10         + 6.6510 _d -12*temp    +
     &               -3.3910 _d -13*temp**2)*pres**3
             b    = -1.9220 _d -2           - 4.4200 _d -5*temp     + 
     &              (7.3637 _d -5           + 1.7950 _d -7*temp)*pres
             d    = 1.727 _d -3             - 7.9836 _d -6*pres 
             ihop_ssp(i,j,k,bi,bj) = cw + a*sal + b*sal**(3./2.)    +
     &                               d*sal**2
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
!#ifdef ALLOW_DIAGNOSTICS
!       IF ( useDiagnostics ) THEN
!        CALL IHOP_DIAGNOSTICS_FILL(bi,bj,myThid)
!       ENDIF
!#endif /* ALLOW_DIAGNOSTICS */

C-    end-if calc_soundSpeed
      ENDIF

#endif /* ALLOW_IHOP */ 
!!=============================================
!! Option 1: COMPARING LAT LON VALUES DIRECTLY
!!=============================================
!      WRITE(msgBuf, *) 'ESCOBAR option1: '
!      CALL PRINT_ERROR(msgBuf, myThid)
!
!      DO bj=myByLo(myThid),myByHi(myThid)
!       DO bi=myBxLo(myThid),myBxHi(myThid)
!        DO j=1,sNy
!         IF (yC(1,j,bi,bj) .EQ. ihop_yc) THEN
!          DO i=1,sNx
!           DO ii=1,IHOP_NPTS_RANGE
!            IF (xC(i,j,bi,bj) .EQ. ihop_xc(ii)) THEN
!             WRITE(msgBuf, *) 'ESCOBAR: ',
!     &       xC( i,j,bi,bj), yC(i,j,bi,bj)
!             CALL PRINT_ERROR(msgBuf, myThid)
!            ENDIF
!           ENDDO
!          ENDDO
!         ENDIF
!        ENDDO
!       ENDDO
!      ENDDO
!!=============================================
!! END Option 1
!!=============================================
!!=============================================
!! Option 2: COMPARING GLOBAL INDICES
!!=============================================
!      WRITE(msgBuf, *) 'ESCOBAR option2: '
!      CALL PRINT_ERROR(msgBuf, myThid)
!
!      DO bj=myByLo(myThid),myByHi(myThid)
!       DO bi=myBxLo(myThid),myBxHi(myThid)
!        DO j=1,sNy
!         IF (j+sNy*(bj-1) .EQ. ihop_yc_index) THEN
!          DO i=1,sNx
!           DO ii=1,IHOP_NPTS_RANGE
!            IF ( i+sNx*(bi-1) .EQ. ihop_xc_index(ii) ) THEN
!             WRITE(msgBuf, *) 'ESCOBAR: ',
!     &       xC(i,j,bi,bj),yC(i,j,bi,bj), ihop_ssp(i,j,:,bi,bj)
!             CALL PRINT_ERROR(msgBuf, myThid)
!            ENDIF
!           ENDDO
!          ENDDO
!         ENDIF
!        ENDDO
!       ENDDO
!      ENDDO

      RETURN
      END
