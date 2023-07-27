#include "IHOP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: IHOP_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE IHOP_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize IHOP constant

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "IHOP_SIZE.h"
#include "IHOP.h"
#include "netcdf.inc"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_IHOP
C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER :: iUnit
      INTEGER :: err, ncid, dimid1, dimid2, varid1, varid2, varid3
      INTEGER :: k, i, j
      _RL :: data1vec( IHOP_MAX_NC_SIZE*IHOP_MAX_NC_SIZE )
      _RL :: data2vec( IHOP_MAX_NC_SIZE*IHOP_MAX_NC_SIZE )
      _RL :: data3vec( IHOP_MAX_NC_SIZE*IHOP_MAX_NC_SIZE )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C Read NetCDF input: setting the acoustic domain
      err = NF_OPEN(TRIM(IHOP_interpfile), NF_NOWRITE, ncid)
      IF (err .NE. NF_NOERR) THEN
        WRITE(msgBuf, '(2A)') 'S/R IHOP_INIT_FIXED: ',
     &  'NetCDF IHOP input file NOT found'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R IHOP_INIT_FIXED'
      END IF

C Get dimension IDs
      err = NF_INQ_DIMID( ncid, 'nrange', dimid1 )
      err = NF_INQ_DIMID( ncid, 'nidw', dimid2 )
C get dimension lengths
      err = NF_INQ_DIMLEN( ncid, dimid1, IHOP_npts_range )
      err = NF_INQ_DIMLEN( ncid, dimid2, IHOP_npts_idw )

      IF ( IHOP_npts_range>IHOP_MAX_NC_SIZE .OR. 
     &     IHOP_npts_idw>IHOP_MAX_NC_SIZE ) THEN
        WRITE(msgBuf, '(2A)') 'S/R IHOP_INIT_FIXED: reading dims',
     &  'NetCDF Dimensions exceed maximum allowable size'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R IHOP_INIT_FIXED'
      END IF

C Get data variable IDs
      err = NF_INQ_VARID( ncid, 'ihop_xc', varid1 )
      err = NF_INQ_VARID( ncid, 'ihop_yc', varid2 )
      err = NF_INQ_VARID( ncid, 'ihop_idw_weights', varid3 )

      IF (err .NE. NF_NOERR) THEN
        WRITE(msgBuf, '(2A)') 'S/R IHOP_INIT_FIXED: ',
     &  'NetCDF IHOP data variable names are incorrect'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R IHOP_INIT_FIXED'
      END IF

C get data variables
      err = NF_GET_VAR( ncid, varid1, data1vec )
      err = NF_GET_VAR( ncid, varid2, data2vec )
      err = NF_GET_VAR( ncid, varid3, data3vec )
      
      IF (err .NE. NF_NOERR) THEN
        WRITE(msgBuf, '(2A)') 'S/R IHOP_INIT_FIXED: ',
     &  'NetCDF IHOP data variables are incorrect'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R IHOP_INIT_FIXED'
      END IF

C 'reshape' data variables to 2D array
      k=1
      DO i=1,IHOP_npts_range
       DO j=1,IHOP_npts_idw
        ihop_xc(i,j) = data1vec(k)
        ihop_yc(i,j) = data2vec(k)
        ihop_idw_weights(i,j) = data3vec(k)
        k = k+1
       END DO
      END DO

C Close NetCDF file
      err = NF_CLOSE( ncid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_MNC
      IF (useMNC) THEN
C       Initialize the MNC variable types for IHOP
        CALL IHOP_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL IHOP_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_IHOP */
      RETURN
      END
