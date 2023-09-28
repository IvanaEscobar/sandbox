#include "CPP_EEOPTIONS.h"

!--  File global_vec_sum.F: Perform a global sum on a tiled-array of vectors.
!--   Contents
!--   o GLOBAL_MAT_SUM_R4
!--   o GLOBAL_MAT_SUM_R8
!--   o GLOBAL_MAT_SUM_INT

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
!BOP
! ROUTINE: GLOBAL_MAT_SUM_MOD
! INTERFACE:
MODULE global_mat_sum_mod
! <CONTACT EMAIL="ivana@utexas.edu">
!   Ivana Escobar
! </CONTACT>

#ifdef   ALLOW_USE_MPI
    use mpi
#endif /* ALLOW_USE_MPI */
!   USES:
    IMPLICIT NONE
!   GLOBAL VARIABLES:
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

    PRIVATE

!   PUBLIC INTERFACES
!===============================================================================
    public global_mat_sum
!===============================================================================
    INTERFACE global_mat_sum
        MODULE PROCEDURE global_mat_sum_r4, global_mat_sum_r8!, &
            !global_mat_sum_int
    END INTERFACE global_mat_sum

CONTAINS
!   !FUCTION: GLOBAL_MAT_SUM_R4

!   !INTERFACE:
    FUNCTION GLOBAL_MAT_SUM_R4( & 
        ndim, nval, &
        sumPhi,     &
        myThid )

!   !DESCRIPTION:
!   Sum the matrix over tiles and then sum the result over all MPI
!   processes. Within a process only one thread (Master) does the sum.
!   The same thread also does the inter-process sum for example with MPI
!   and then writes the result into a shared location. All threads wait
!   until the sum is available.
!   Warning: Only works if argument array "sumPhi" is shared by all threads.

!   !INPUT/OUTPUT PARAMETERS:
!   sumPhi   :: input/output array
!   myThid   :: thread ID
    INTEGER, INTENT(IN)     :: ndim, nval, myThid
    Real*4,  INTENT(INOUT)  :: sumPhi(ndim,nSx,nSy)
!EOP

!   !LOCAL VARIABLES:
!   mpiRC   :: MPI return code
    INTEGER :: i, bi,bj
    Real*4  :: tmp1(nval), tmp2(nval)
#ifdef   ALLOW_USE_MPI
    INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

    _BARRIER
    _BEGIN_MASTER( myThid )

!   Sum over all tiles
    DO i = 1,nval
        tmp1(i) = 0.
    ENDDO
    DO bj = 1,nSy
        DO bi = 1,nSx
            DO i = 1,nval
                tmp1(i) = tmp1(i) + sumPhi( i, bi,bj )
            ENDDO
        ENDDO
    ENDDO

!   Invoke MPI if necessary
    IF ( usingMPI ) THEN

#ifdef ALLOW_USE_MPI
        CALL MPI_Allreduce(tmp1,tmp2,nval,MPI_REAL, &
             MPI_SUM,MPI_COMM_MODEL,mpiRC)
#endif /* ALLOW_USE_MPI */

!     Copy the results to the first location of the input array
        DO i = 1,nval
            sumPhi( i, 1,1 ) = tmp2(i)
        ENDDO

    ELSE
!   Copy the results to the first location of the input array
        DO i = 1,nval
            sumPhi( i, 1,1 ) = tmp1(i)
        ENDDO

    ENDIF

    _END_MASTER( myThid )
    _BARRIER

    RETURN
    END !FUNCTION GLOBAL_MAT_SUM_R4

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
!BOP
!   !FUNCTION: GLOBAL_MAT_SUM_R8

!   !INTERFACE:
    FUNCTION GLOBAL_MAT_SUM_R8( &
         ndim, nval, &
         sumPhi, &
         myThid )

!   !DESCRIPTION:
!   Sum the matrix over tiles and then sum the result over all MPI
!   processes. Within a process only one thread (Master) does the sum.
!   The same thread also does the inter-process sum for example with MPI
!   and then writes the result into a shared location. All threads wait
!   until the sum is available.
!   Warning: Only works if argument array "sumPhi" is shared by all threads.

!   !INPUT/OUTPUT PARAMETERS:
!   sumPhi   :: input/output array
!   myThid   :: thread ID
    INTEGER, INTENT(IN)     :: ndim, nval, myThid
    Real*8,  INTENT(INOUT)  :: sumPhi(ndim,nSx,nSy)
!EOP

!   !LOCAL VARIABLES:
!   mpiRC   :: MPI return code
    INTEGER :: i, bi,bj
    Real*8  :: tmp1(nval), tmp2(nval)
#ifdef   ALLOW_USE_MPI
    INTEGER :: mpiRC
#endif /* ALLOW_USE_MPI */

    _BARRIER
    _BEGIN_MASTER( myThid )

!   Sum over all tiles
    DO i = 1,nval
        tmp1(i) = 0.
    ENDDO
    DO bj = 1,nSy
        DO bi = 1,nSx
            DO i = 1,nval
                tmp1(i) = tmp1(i) + sumPhi( i, bi,bj )
            ENDDO
        ENDDO
    ENDDO

!   Invoke MPI if necessary
    IF ( usingMPI ) THEN

#ifdef ALLOW_USE_MPI
        CALL MPI_Allreduce(tmp1,tmp2,nval,MPI_DOUBLE_PRECISION, &
             MPI_SUM,MPI_COMM_MODEL,mpiRC)
#endif /* ALLOW_USE_MPI */

!   Copy the results to the first location of the input array
        DO i = 1,nval
            sumPhi( i, 1,1 ) = tmp2(i)
        ENDDO

    ELSE
!   Copy the results to the first location of the input array
        DO i = 1,nval
            sumPhi( i, 1,1 ) = tmp1(i)
        ENDDO

    ENDIF

    _END_MASTER( myThid )
    _BARRIER

    RETURN
    END !FUNCTION GLOBAL_MAT_SUM_R8

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
!BOP
!     !ROUTINE: GLOBAL_MAT_SUM_INT

!     !INTERFACE:
      SUBROUTINE GLOBAL_MAT_SUM_INT( &
           ndim, nval, &
           sumPhi, &
           myThid )

!     !DESCRIPTION:
!     Sum the vector over tiles and then sum the result over all MPI
!     processes. Within a process only one thread (Master) does the sum.
!     The same thread also does the inter-process sum for example with MPI
!     and then writes the result into a shared location. All threads wait
!     until the sum is available.
!     Warning: Only works if argument array "sumPhi" is shared by all threads.

!     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
!#include "GLOBAL_SUM.h"

!     !INPUT/OUTPUT PARAMETERS:
!     sumPhi   :: input/output array
!     myThid   :: thread ID
      INTEGER, INTENT(IN) :: ndim, nval, myThid
      INTEGER, INTENT(INOUT) :: sumPhi(ndim,nSx,nSy)
!EOP

!     !LOCAL VARIABLES:
!     mpiRC    :: MPI return code
      INTEGER :: i, bi,bj
      INTEGER :: tmp1(nval), tmp2(nval)
#ifdef   ALLOW_USE_MPI
      INTEGER :: mpiRC
#endif /* ALLOW_USE_MPI */

      _BARRIER
      _BEGIN_MASTER( myThid )

!     Sum over all tiles
      DO i = 1,nval
        tmp1(i) = 0
      ENDDO
      DO bj = 1,nSy
        DO bi = 1,nSx
          DO i = 1,nval
            tmp1(i) = tmp1(i) + sumPhi( i, bi,bj )
          ENDDO
        ENDDO
      ENDDO

!     Invoke MPI if necessary
      IF ( usingMPI ) THEN

#ifdef  ALLOW_USE_MPI
        CALL MPI_Allreduce(tmp1,tmp2,nval,MPI_INTEGER, &
             MPI_SUM,MPI_COMM_MODEL,mpiRC)
#endif /*  ALLOW_USE_MPI */

!     Copy the results to the first location of the input array
        DO i = 1,nval
          sumPhi( i, 1,1 ) = tmp2(i)
        ENDDO

      ELSE
!     Copy the results to the first location of the input array
        DO i = 1,nval
          sumPhi( i, 1,1 ) = tmp1(i)
        ENDDO

      ENDIF

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
END MODULE global_mat_sum_mod
