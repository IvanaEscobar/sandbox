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
      INTEGER :: ndims, nvars, ngats, unlimid
      INTEGER :: ihopnx, ihopny, err
      REAL :: data1vec( IHOP_MAX_NC_SIZE*IHOP_MAX_NC_SIZE )
      REAL :: data2vec( IHOP_MAX_NC_SIZE*IHOP_MAX_NC_SIZE )
      REAL :: data3vec( IHOP_MAX_NC_SIZE*IHOP_MAX_NC_SIZE )
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

      err = NF_INQ(ncid, ndims, nvars, ngats, unlimid)
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

      IF (err .NE. NF_NOERR) THEN
       PRINT *, 'Error: Not the right data variables'
       STOP
      END IF
C get data variables
      err = NF_GET_VAR( ncid, varid1, data1vec )
      err = NF_GET_VAR( ncid, varid2, data2vec )
      err = NF_GET_VAR( ncid, varid3, data3vec )
      
      IF (err .NE. NF_NOERR) THEN
       PRINT *, 'Error reading the NetCDF IHOP input file.'
       RETURN
      END IF

C 'reshape' data variables to 2D array
      k=1
      DO i=1,ihopnx
       DO j=1,ihopny
        data1(i,j) = data1vec(k)
        data2(i,j) = data2vec(k)
        data3(i,j) = data3vec(k)
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

          DO i=1,nx
           DO j=1,ny
c           PRINT *, 'matrix1(',i,',',j,'): ', matrix1(i,j)
c           PRINT *, 'matrix2(',i,',',j,'): ', matrix2(i,j)
            PRINT *, 'matrix3(',i,',',j,'): ', matrix3(i,j)
           ENDDO
          ENDDO
        END PROGRAM PrintNCContents
