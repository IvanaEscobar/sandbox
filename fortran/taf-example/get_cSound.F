      PROGRAM get_cSound
      IMPLICIT NONE
      INTEGER i, j, N
      PARAMETER (N=10)
      REAL*8 sal(N, N), temp(N, N), pres(N, N), cSound(N, N), fwd_c

      EXTERNAL fwd_c

C     Initialize the arrays sal, temp, and pres
      DO i = 1, N
          DO j = 1, N
              sal(i, j) = 34.0+1.0D-2*i+1.0D-2*j   ! Initialize salinity values
              temp(i, j) = 21.0+i+j  ! Initialize temperature values
              pres(i, j) = 10.0  ! Initialize pressure values
          END DO
      END DO

C     Calculate the speed of sound
      DO i = 1, N
          DO j = 1, N
              cSound(i, j) = fwd_c(sal(i, j), temp(i, j), pres(i, j))
C             PRINT '(F12.4,A)', cSound(i,j), ', '
          END DO
      END DO

      END
