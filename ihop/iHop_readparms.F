#include "IHOP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: IHOP_READPARMS

C !INTERFACE:
      SUBROUTINE IHOP_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize IHOP variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "IHOP_PARAMS.h"
#include "IHOP_VARS.h"

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

      IF ( .NOT.useIHOP ) THEN
C-    pkg IHOP is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.bellhop is found
         CALL PACKAGES_UNUSED_MSG( 'useIHOP', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values for IHOP
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

      WRITE(msgBuf,'(A)') 'IHOP_READPARMS: opening data.bellhop'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.bellhop', 'IHOP_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=IHOP_PARM01)
      WRITE(msgBuf,'(A)')
     &    'IHOP_READPARMS: finished reading data.bellhop'
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

#endif /* ALLOW_IHOP */

      RETURN
      END
