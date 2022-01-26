#include "BELLHOP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BELLHOP_INIT_FIXED

C !INTERFACE:
      SUBROUTINE BELLHOP_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize BELLHOP variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "BELLHOP_PARAMS.h"
#include "BELLHOP_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER axes(4)
      INTEGER i,j,k,bi,bj
      _RL bellhop_test

      namelist / bellhop_nml /
     &   bellOn, mixTS

      bellhop_test = 1.3

      _BARRIER
      _BEGIN_MASTER(myThid)
      module_is_initialized =.false.
      bellOn                =.false.
      mixTS                 = 0.03
      WRITE(msgBuf,'(A)') 'BELLHOP_INIT_FIXED: opening data.bellhop'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.bellhop', 'BELLHOP_INIT_FIXED',
     O                     iUnit,
     I                     myThid )

C      Read parameters from open data file
      READ(UNIT=iUnit,NML=bellhop_nml)
      WRITE(msgBuf,'(A)')
     &     'BELLHOP_INIT_FIXED: finished reading data.bellhop'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
C     Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

#ifdef ALLOW_MNC
      IF (useMNC) THEN
        CALL BELLHOP_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL BELLHOP_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
