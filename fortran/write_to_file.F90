! ------------------------------------------------------
! write some content to a file
! ------------------------------------------------------

      PROGRAM toFILE
        IMPLICIT NONE
        INTEGER, PARAMETER ::   fileID = 99, &
                                arrSize = 10
        INTEGER            ::   i, ioalloc
        REAL (KIND=8)      ::   arr(arrSize)
        CHARACTER (LEN=13) ::   FMT1

        ! Transform data
        arr = [( 13*SIN(i*3.1), i=1,arrSize )]
        arr(3:5) = 5.5
        
        ! ====================
        ! No format for output
        ! ====================
        ! Open a NEW file
        OPEN(fileID, file='unformat.dat', iostat=ioalloc, status='new')
        ! Write to file 
        DO i=1,arrSize
            WRITE(fileID, *) "index: ", i, " value = ", arr(i) 
        ENDDO
        ! Close file
        CLOSE(fileID)

        ! ====================
        ! Formatted output
        ! ====================
        FMT1 = '(A,I2,A,F8.2)'
        ! Open a NEW file
        OPEN(fileID+1, file='format.dat', iostat=ioalloc, status='new')
        ! Write to file 
        DO i=1,arrSize
            WRITE(fileID+1, FMT1) "index: ", i, " value = ", arr(i) 
        ENDDO
        ! Close file
        CLOSE(fileID+1)

      END PROGRAM toFILE
