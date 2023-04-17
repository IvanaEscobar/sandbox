#include "IHOP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 
C     !ROUTINE: IHOP_READPARMS

C     !INTERFACE:
      SUBROUTINE IHOP_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize IHOP variables and constants, read in data.ihop

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "IHOP.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_IHOP
C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /IHOP_PARM01/
     &      IHOP_bellOn,
     &      IHOP_fileroot
C     &      IHOP_MNC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useIHOP ) THEN
C-    pkg IHOP is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.ihop is found
         CALL PACKAGES_UNUSED_MSG( 'useIHOP', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values for IHOP
      IHOP_bellOn   =.TRUE.
      IHOP_fileroot = ' '

C     Open and read data.ihop file
      WRITE(msgBuf,'(A)') 'IHOP_READPARMS: opening data.ihop'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.ihop', 'IHOP_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=IHOP_PARM01)
      WRITE(msgBuf,'(A)')
     &    'IHOP_READPARMS: finished reading data.ihop'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)

C     Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

CC     Make sure that we locally honor the global MNC on/off flag
C      IHOP_MNC = IHOP_MNC .AND. useMNC
C#ifndef ALLOW_MNC
CC     Fix to avoid running without getting any output:
C      IHOP_MNC = .FALSE.
C#endif
C      IHOP_MDSIO = (.NOT. IHOP_MNC) .OR. outputTypesInclusive

      _END_MASTER(myThid)
C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_IHOP */

      RETURN
      END
