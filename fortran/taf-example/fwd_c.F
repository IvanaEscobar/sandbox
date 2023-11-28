CBOP
C     !ROUTINE: CHEN_MILLERO
C     !INTERFACE:
      REAL*8 FUNCTION fwd_c(sal, temp, pres)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | FXN: CHEN_MILLERO
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

C     !INPUT/OUTPUT PARAMETERS:


C     !LOCAL VARIABLES:
C     == Local variables ==
      Real*8 c0,cw,a,b,d
      Real*8 pres,sal,temp
CEOP

C initiate parameters
      c0   = 1402.388D0
      cw   = 0.D0
      a    = 0.D0
      b    = 0.D0
      d    = 0.D0

C convert pressure to bar for Chen and Millero algorithm
      pres = pres/(1.D1)
C calculate sound speed 
      cw   = c0                   + 5.0383D0*temp      +
     &       -5.81090D-2*temp**2  + 3.3432D-4*temp**3  + 
     &       -1.47797D-6*temp**4  + 3.1419D-9*temp**5  +
     &       (1.53563D-1          + 6.8999D-4*temp     +
     &        -8.1829D-6*temp**2  + 1.3632D-7*temp**3  +
     &        -6.1260D-10*temp**4)*pres                + 
     &       (3.12600D-5          - 1.7111D-6*temp     +
     &        2.59860D-8*temp**2  - 2.5353D-10*temp**3 +
     &        1.04150D-12*temp**4)*pres**2             +
     &       (-9.7729D-9          + 3.8513D-10*temp    +  
     &        -2.3654D-12*temp**2)*pres**3
      a    = 1.389D0              - 1.2620D-2*temp     + 
     &       7.166D-5*temp**2     + 2.0080D-6*temp**3  +
     &       -3.21D-8*temp**4                          +
     &       (9.47420D-5          - 1.2583D-5*temp     +
     &        -6.4928D-8*temp**2  + 1.0515D-8*temp**3  +
     &        -2.0142D-10*temp**4)*pres                +
     &       (-3.9064D-7          + 9.1061D-9*temp     +
     &        -1.6009D-10*temp**2                      + 
     &        7.99400D-12*temp**3)*pres**2             +
     &       (1.10000D-10         + 6.6510D-12*temp    +
     &        -3.3910D-13*temp**2)*pres**3
      b    = -1.9220D-2           - 4.4200D-5*temp     + 
     &       (7.3637D-5           + 1.7950D-7*temp)*pres
      d    = 1.727D-3             - 7.9836D-6*pres 
      fwd_c  = cw + a*sal + b*sal**(3./2.) + d*sal**2

      RETURN
      END
