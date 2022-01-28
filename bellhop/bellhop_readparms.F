#include "BELLHOP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BELLHOP_READPARMS

C !INTERFACE:
      SUBROUTINE BELLHOP_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize BELLHOP variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BELLHOP_PARAMS.h"
#include "BELLHOP_VARS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_BELLHOP

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /BELLHOP_PARM01/
     &       myPa_MNC,
     &       myPa_StaV_Cgrid, myPa_Tend_Cgrid,
     &       myPa_applyTendT, myPa_applyTendS,
     &       myPa_applyTendU, myPa_applyTendV,
     &       myPa_doSwitch1,  myPa_doSwitch2,
     &       myPa_index1,     myPa_index2,
     &       myPa_param1,     myPa_param2,
     &       myPa_string1,    myPa_string2,
     &       myPa_Scal1File,  myPa_Scal2File,
     &       myPa_VelUFile,   myPa_VelVFile,
     &       myPa_Surf1File,  myPa_Surf2File

      IF ( .NOT.useBELLHOP ) THEN
C-    pkg BELLHOP is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.bellhop is found
         CALL PACKAGES_UNUSED_MSG( 'useBELLHOP', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values for BELLHOP
      myPa_MNC         = useMNC
      myPa_StaV_Cgrid  = .TRUE.
      myPa_Tend_Cgrid  = .TRUE.
      myPa_applyTendT  = .FALSE.
      myPa_applyTendS  = .FALSE.
      myPa_applyTendU  = .FALSE.
      myPa_applyTendV  = .FALSE.
C-    additional parameters:
      myPa_doSwitch1   = .FALSE.
      myPa_doSwitch2   = .FALSE.
      myPa_index1      = 0
      myPa_index2      = 0
      myPa_param1      = 0. _d 0
      myPa_param2      = 0. _d 0
      myPa_string1     = ' '
      myPa_string2     = ' '
C-    file names for initial conditions:
      myPa_Scal1File   = ' '
      myPa_Scal2File   = ' '
      myPa_VelUFile    = ' '
      myPa_VelVFile    = ' '
      myPa_Surf1File   = ' '
      myPa_Surf2File   = ' '

      WRITE(msgBuf,'(A)') 'BELLHOP_READPARMS: opening data.bellhop'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.bellhop', 'BELLHOP_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=BELLHOP_PARM01)
      WRITE(msgBuf,'(A)')
     &    'BELLHOP_READPARMS: finished reading data.bellhop'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
C     Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C     Make sure that we locally honor the global MNC on/off flag
      myPa_MNC = myPa_MNC .AND. useMNC
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
      myPa_MNC = .FALSE.
#endif
      myPa_MDSIO = (.NOT. myPa_MNC) .OR. outputTypesInclusive

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_BELLHOP */

      RETURN
      END
