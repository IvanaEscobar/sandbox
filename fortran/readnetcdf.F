C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: IHOP_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE READNETCDF( filename, ihopnx, ihopny, data1, data2,
     &                      data3 )

C     !DESCRIPTION:
C     Initialize IHOP constant

C     !USES:
      IMPLICIT NONE
C     !INPUT/OUTPUT PARAMETERS:
#include "netcdf.inc"
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER, PARAMETER :: NC_FILENAME_LENGTH=80, IHOP_MAX_NC_SIZE=10
      INTEGER :: i,j,k
      INTEGER :: ncid, dimid1, dimid2, varid1, varid2, varid3
      INTEGER :: ihopnx, ihopny, err
      REAL :: data1vec( IHOP_MAX_NC_SIZE*IHOP_MAX_NC_SIZE )
      REAL :: data1(IHOP_MAX_NC_SIZE, IHOP_MAX_NC_SIZE), 
     &        data2(IHOP_MAX_NC_SIZE, IHOP_MAX_NC_SIZE),
     &        data3(IHOP_MAX_NC_SIZE, IHOP_MAX_NC_SIZE) 
      CHARACTER(LEN=NC_FILENAME_LENGTH) :: filename
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C Read NetCDF input: setting the acoustic domain
      err = NF_OPEN(TRIM(filename), NF_NOWRITE, ncid)
      IF (err .NE. NF_NOERR) THEN
          PRINT *, 'Error opening the NetCDF IHOP input file.'
          RETURN
      END IF


C Get dimension IDs
      err = NF_INQ_DIMID( ncid, 'nrange', dimid1 )
      err = NF_INQ_DIMID( ncid, 'nidw', dimid2 )
C get dimension lengths
      err = NF_INQ_DIMLEN( ncid, dimid1, ihopnx )
      err = NF_INQ_DIMLEN( ncid, dimid2, ihopny )

      IF ( ihopnx>IHOP_MAX_NC_SIZE .OR. ihopny>IHOP_MAX_NC_SIZE ) THEN
       PRINT *, 'Error: Dimensions exceed maximum allowable size'
       STOP
      END IF

C Get data variable IDs
      err = NF_INQ_VARID( ncid, 'ihop_xc', varid1 )
      err = NF_INQ_VARID( ncid, 'ihop_yc', varid2 )
      err = NF_INQ_VARID( ncid, 'ihop_idw_weights', varid3 )

C get data variables
      err = NF_GET_VAR( ncid, varid1, data1vec )
C     PRINT *, 'first row:'
C     PRINT *, data1(1,1), data1(1,2), data1(1,3), data1(1,4),
C    &         data1(1,5), data1(1,6), data1(1,7), data1(1,8),
C    &         data1(1,9), data1(1,10)

C     PRINT *, 'first col:'
C     PRINT *, data1(1,1), data1(2,1), data1(3,1), data1(4,1),
C    &         data1(5,1), data1(6,1), data1(7,1), data1(8,1),
C    &         data1(9,1), data1(10,1)
      err = NF_GET_VAR( ncid, varid2, data2 )
C     PRINT *, data2(1,1), data2(1,2), data2(1,3), data2(1,4),
C    &         data2(1,5), data2(1,6)
      err = NF_GET_VAR( ncid, varid3, data3 )
C     PRINT *, data3(1,1), data3(1,2), data3(1,3), data3(1,4),
C    &         data3(1,5), data3(1,6)
      IF (err .NE. NF_NOERR) THEN
          PRINT *, 'Error reading the NetCDF IHOP input file.'
          RETURN
      END IF

C 'reshape' data variables to 2D array correctly
      DO j=1,ihopny
       DO i=1,ihopnx
        data1(i,j) = data1vec(k)
        k = k+1
       END DO
      END DO

C Close NetCDF file
      err = NF_CLOSE( ncid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      END SUBROUTINE READNETCDF 

      PROGRAM PrintNCContents
#include "netcdf.inc"

          INTEGER, PARAMETER :: IHOP_MAX_NC_SIZE=10
          INTEGER :: i,j, nx, ny
          REAL :: matrix1 (IHOP_MAX_NC_SIZE, IHOP_MAX_NC_SIZE),
     &            matrix2 (IHOP_MAX_NC_SIZE, IHOP_MAX_NC_SIZE),
     &            matrix3 (IHOP_MAX_NC_SIZE, IHOP_MAX_NC_SIZE)
          CHARACTER(LEN=80) :: filename

          filename='myfile.nc'

          CALL READNETCDF( filename, nx, ny, matrix1, matrix2, matrix3 )
          PRINT *, nx, ny
C         PRINT *, matrix1(1,1), matrix1(1,2), matrix1(1,3), 
C    &         matrix1(1,4), matrix1(1,5), matrix1(1,6)

          
          DO j=1,ny
           DO i=1,nx
c           PRINT *, 'matrix1(',i,',',j,'): ', matrix1(i,j)
c           PRINT *, 'matrix2(',i,',',j,'): ', matrix2(i,j)
c           PRINT *, 'matrix3(',i,',',j,'): ', matrix3(i,j)
           ENDDO
          ENDDO
        END PROGRAM PrintNCContents
