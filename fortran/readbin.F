      SUBROUTINE READBIN_XY( filename, matVar ) 
          CHARACTER(LEN=80) filename
          REAL matVar(4,6)
          INTEGER i, j, iostat

          OPEN(UNIT=1, FILE=filename, FORM='BINARY', STATUS='OLD',
     &         ACTION='READ', IOSTAT=iostat)

          IF ( iostat /= 0 ) THEN
           RETURN
          END IF

          DO j=1,ncols
           DO i=1,nrows
            READ(1) matVar(i,j)
           ENDDO
          ENDDO

          CLOSE(UNIT=1)
      END SUBROUTINE READBIN_XY
