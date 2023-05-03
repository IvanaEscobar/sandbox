!BOP
!    !ROUTINE: IHOP_SIZE.h
!    !INTERFACE:
! #include IHOP_SIZE.h

!    !DESCRIPTION: \bv
!     ==================================================================
!     IHOP_SIZE.h
!     ==================================================================
!     Contains IHOP source receiver array dimension
!     \ev
!EOP

!   nsd    :: No. of sound sources at range of 0 m
!   nrd    :: No. of sound receivers at a single range
!   nrr    :: No. of sound receivers at a single depth
!     =============================
!     Number of Sources:
!     =============================
      INTEGER nsd
#ifdef IHOP_MULTIPLE_SOURCES
      PARAMETER ( nsd=10 )
#else 
      PARAMETER ( nsd=1 )
#endif
    
!     Number of Receivers:
!     =============================
      INTEGER nrd
      INTEGER nrr
#ifdef IHOP_MULTIPLE_RECEIVERS
      PARAMETER ( nrd=30 )
      PARAMETER ( nrr=30 )
#else 
      PARAMETER ( nrd=1 )
      PARAMETER ( nrr=1 )
#endif

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
