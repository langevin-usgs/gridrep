      MODULE GRMODS
C
      IMPLICIT NONE
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::
     1   XV, YV, ELV, XN, YN, ZN
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::
     1   ICV
      INTEGER, DIMENSION(:), ALLOCATABLE ::
     1   ICN, ICETOP, ICEBOT
      LOGICAL, DIMENSION(:), ALLOCATABLE ::
     1   LCHIGH
C
      END MODULE GRMODS