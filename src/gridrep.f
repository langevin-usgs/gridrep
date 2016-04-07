      PROGRAM GRIDREP
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDUC, LDHC, LDUN, LDHN, LDHI, LDHL, LDVI, LDVL, LBOTH
      CHARACTER CDUM*1, CVIEW*4
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....READ INPUT AND OPEN OUTPUT FILE.
      PRINT *, "Reading ..."
      CALL READIN(CVIEW, XVIEW, YVIEW, ZVIEW,
     1   OVECX, OVECY, OVECZ, OANGL, RADNOD,
     2   LDUC, REDUC, GRNUC, BLUUC, TRNUC,
     3   LDHC, REDHC, GRNHC, BLUHC, TRNHC,
     4   LDUN, REDUN, GRNUN, BLUUN, TRNUN,
     5   LDHN, REDHN, GRNHN, BLUHN, TRNHN,
     6   LDHI, REDHI, GRNHI, BLUHI, TRNHI,
     7   LDHL, REDHL, GRNHL, BLUHL, TRNHL,
     8   LDVI, REDVI, GRNVI, BLUVI, TRNVI,
     9   LDVL, REDVL, GRNVL, BLUVL, TRNVL,
     T   LBOTH)
C
C.....DRAW GRID.
      PRINT *, "Drawing ..."
      CALL DRWGRD(CVIEW, XVIEW, YVIEW, ZVIEW,
     1   OVECX, OVECY, OVECZ, OANGL, RADNOD,
     2   LDUC, REDUC, GRNUC, BLUUC, TRNUC,
     3   LDHC, REDHC, GRNHC, BLUHC, TRNHC,
     4   LDUN, REDUN, GRNUN, BLUUN, TRNUN,
     5   LDHN, REDHN, GRNHN, BLUHN, TRNHN,
     6   LDHI, REDHI, GRNHI, BLUHI, TRNHI,
     7   LDHL, REDHL, GRNHL, BLUHL, TRNHL,
     8   LDVI, REDVI, GRNVI, BLUVI, TRNVI,
     9   LDVL, REDVL, GRNVL, BLUVL, TRNVL,
     T   LBOTH)
C
C.....SHUT THINGS DOWN GRACEFULLY.
      CALL BYEBYE()
C
      END
C
C=======================================================================
C
      SUBROUTINE READIN(CVIEW, XVIEW, YVIEW, ZVIEW,
     1   OVECX, OVECY, OVECZ, OANGL, RADNOD,
     2   LDUC, REDUC, GRNUC, BLUUC, TRNUC,
     3   LDHC, REDHC, GRNHC, BLUHC, TRNHC,
     4   LDUN, REDUN, GRNUN, BLUUN, TRNUN,
     5   LDHN, REDHN, GRNHN, BLUHN, TRNHN,
     6   LDHI, REDHI, GRNHI, BLUHI, TRNHI,
     7   LDHL, REDHL, GRNHL, BLUHL, TRNHL,
     8   LDVI, REDVI, GRNVI, BLUVI, TRNVI,
     9   LDVL, REDVL, GRNVL, BLUVL, TRNVL,
     T   LBOTH)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDUC, LDHC, LDUN, LDHN, LDHI, LDHL, LDVI, LDVL, LBOTH
      CHARACTER CVIEW*4, CHIGH*4
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....READ 2D VERTEX COORDINATES.
      OPEN(UNIT=20, FILE='example.ver', STATUS='OLD')
      READ(20,*) NVER
      ALLOCATE (XV(NVER),YV(NVER))
      DO 200 I=1,NVER
         READ(20,*) IV, XV(IV), YV(IV)
  200 CONTINUE
      CLOSE(20)
C
C.....READ ELEVATIONS.
      OPEN(UNIT=24, FILE='example.elv', STATUS='OLD')
      READ(24,*) NELV
      ALLOCATE (ELV(NELV))
      DO 240 I=1,NELV
         READ(24,*) IE, ELV(IE)
  240 CONTINUE
      CLOSE(24)
C
C.....READ CELLS.
      OPEN(UNIT=26, FILE='example.cel', STATUS='OLD')
      READ(26,*) NCEL, NCVMAX
      ALLOCATE (ICN(NCEL),ICETOP(NCEL),ICEBOT(NCEL),ICV(NCEL,NCVMAX+1))
      ICV = 0
      DO 260 I=1,NCEL
         READ(26,*) IC, ICN(IC), ICETOP(IC), ICEBOT(IC),
     1      (ICV(IC,IV), IV=1,NCVMAX)
         IF (ICV(IC,1).LE.0) THEN
            PRINT *, "ERROR -- ",
     1         "First vertex of a cell must be primary (index > 0)."
            STOP
         END IF
         IVREP = NCVMAX + 1
         DO 250 IV=1,NCVMAX
            IF (ICV(IC,IV).EQ.0) THEN
               IVREP = IV
               EXIT
            END IF
  250    CONTINUE
         ICV(IC,IVREP) = ICV(IC,1)
  260 CONTINUE
      CLOSE(26)
C
C.....READ 3D NODE COORDINATES.
      OPEN(UNIT=28, FILE='example.nod', STATUS='OLD')
      READ(28,*) NNOD
      ALLOCATE (XN(NNOD),YN(NNOD),ZN(NNOD))
      DO 300 I=1,NNOD
         READ(28,*) IN, XN(IN), YN(IN), ZN(IN)
  300 CONTINUE
      CLOSE(28)
C
C.....READ DRAWING INFORMATION.
      OPEN(UNIT=50, FILE='example.see', STATUS='OLD')
      READ(50,*) CVIEW
      IF (CVIEW.NE."AUTO") THEN
         BACKSPACE(50)
         READ(50,*) XVIEW, YVIEW, ZVIEW, OVECX, OVECY, OVECZ, OANGL
      END IF
      READ(50,*) RADNOD
      READ(50,*) LDUC, REDUC, GRNUC, BLUUC, TRNUC
      READ(50,*) LDHC, REDHC, GRNHC, BLUHC, TRNHC
      READ(50,*) LDUN, REDUN, GRNUN, BLUUN, TRNUN
      READ(50,*) LDHN, REDHN, GRNHN, BLUHN, TRNHN
      READ(50,*) LDHI, REDHI, GRNHI, BLUHI, TRNHI
      READ(50,*) LDHL, REDHL, GRNHL, BLUHL, TRNHL
      READ(50,*) LDVI, REDVI, GRNVI, BLUVI, TRNVI
      READ(50,*) LDVL, REDVL, GRNVL, BLUVL, TRNVL
      READ(50,*) LBOTH
      READ(50,*) CHIGH
      ALLOCATE (LCHIGH(NCEL))
      IF (CHIGH.EQ."ALL") THEN
         LCHIGH = .TRUE.
      ELSE IF (CHIGH.EQ."NONE") THEN
         LCHIGH = .FALSE.
      ELSE IF (CHIGH.EQ."SOME") THEN
         DO 400 I=1,NCEL
            READ(50,*) IC, LCHIGH(IC)
  400    CONTINUE
      ELSE
         PRINT *, "ERROR -- Invalid CHIGH."
         STOP
      END IF
      CLOSE(50)
C
C.....OPEN OUTPUT FILE.
      OPEN(UNIT=60, FILE='example.x3d', STATUS='UNKNOWN')
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE DRWGRD(CVIEW, XVIEW, YVIEW, ZVIEW,
     1   OVECX, OVECY, OVECZ, OANGL, RADNOD,
     2   LDUC, REDUC, GRNUC, BLUUC, TRNUC,
     3   LDHC, REDHC, GRNHC, BLUHC, TRNHC,
     4   LDUN, REDUN, GRNUN, BLUUN, TRNUN,
     5   LDHN, REDHN, GRNHN, BLUHN, TRNHN,
     6   LDHI, REDHI, GRNHI, BLUHI, TRNHI,
     7   LDHL, REDHL, GRNHL, BLUHL, TRNHL,
     8   LDVI, REDVI, GRNVI, BLUVI, TRNVI,
     9   LDVL, REDVL, GRNVL, BLUVL, TRNVL,
     T   LBOTH)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDUC, LDHC, LDUN, LDHN, LDHI, LDHL, LDVI, LDVL, LBOTH
      LOGICAL LHIGH, LINSID
      CHARACTER CVIEW*4
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....X3D HEADER INFO.
      CALL X3DHDR()
C
C.....VIEWPOINT AND BACKGROUND.
      CALL VPTBCK(CVIEW, XVIEW, YVIEW, ZVIEW,
     1   OVECX, OVECY, OVECZ, OANGL)
C
C.....UNHIGHLIGHTED NODES AS SPHERES.
      IF (LDUN) CALL DRWNOD(.FALSE., RADNOD, REDUN, GRNUN, BLUUN, TRNUN)
C
C.....HIGHLIGHTED NODES AS SPHERES.
      IF (LDHN) CALL DRWNOD(.TRUE., RADNOD, REDHN, GRNHN, BLUHN, TRNHN)
C
C.....UNHIGHLIGHTED CELL OUTLINES AS INDEXED LINE SET.
      IF (LDUC) CALL DRWCEL(.FALSE., REDUC, GRNUC, BLUUC, TRNUC)
C
C.....HIGHLIGHTED CELL OUTLINES AS INDEXED LINE SET.
      IF (LDHC) CALL DRWCEL(.TRUE., REDHC, GRNHC, BLUHC, TRNHC)
C
C.....HORIZONTAL CONNECTIONS IN HIGHLIGHTED CELLS.
      IF (LDHI.OR.LDHL) CALL DRWHCO(LDHI, REDHI, GRNHI, BLUHI, TRNHI,
     1   LDHL, REDHL, GRNHL, BLUHL, TRNHL, LBOTH)
C
C.....VERTICAL CONNECTIONS IN HIGHLIGHTED CELLS.
      IF (LDVI.OR.LDVL) CALL DRWVCO(LDVI, REDVI, GRNVI, BLUVI, TRNVI,
     1   LDVL, REDVL, GRNVL, BLUVL, TRNVL, LBOTH)
C
C.....CLOSE REMAINING TAGS.
      WRITE(60,'(2X,A/A)') "</Scene>", "</X3D>"
C
      RETURN
      END
C
C=======================================================================
C
      INTEGER FUNCTION NUMENT(ILIST, NMAX, IMARK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ILIST(NMAX)
C
C.....FIND THE NUMBER OF ENTRIES IN ILIST, NOT COUNTING LIST TERMINATOR
C        IMARK.  ASSUME THE LIST HAS AT LEAST ONE NON-TERMINATOR ENTRY.
      NUMENT = 1
      DO 200 I=2,NMAX
         IF (ILIST(I).EQ.IMARK) THEN
            EXIT
         ELSE
            NUMENT = NUMENT + 1
         END IF
  200 CONTINUE
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE X3DHDR()
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....X3D HEADER INFO.
      WRITE(60,'(A/2A/4A)') '<?xml version="1.0" encoding="UTF-8"?>',
     1   '<!DOCTYPE X3D PUBLIC "ISO//Web3D//DTD X3D 3.0',
     2   '//EN" "http://www.web3d.org/specifications/x3d-3.0.dtd">',
     3   "<X3D profile='Interchange' version='3.0' ",
     4   "xmlns:xsd='http://www.w3.org/2001/XMLSchema-instance' ",
     5   "xsd:noNamespaceSchemaLocation=",
     6   "'http://www.web3d.org/specifications/x3d-3.0.xsd'>"
      WRITE(60,'(2X,A/4X,A/2X,A)') "<head>",
     1   "<meta content='example.x3d' name='title'/>", "</head>"
      WRITE(60,'(2X,A)') "<Scene>"
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE VPTBCK(CVIEW, XVIEW, YVIEW, ZVIEW,
     1   OVECX, OVECY, OVECZ, OANGL)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER CVIEW*4
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....FIND BOUNDING BOX.
      CALL BBOX(XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX)
C
C.....VIEWPOINT AND BACKGROUND.
      XMID = 5D-1*(XMIN + XMAX)
      YMID = 5D-1*(YMIN + YMAX)
      ZMID = 5D-1*(ZMIN + ZMAX)
      IF (CVIEW.EQ."AUTO") THEN
         XVIEW = XMID
         YVIEW = YMID
         XSPAN = XMAX - XMIN
         YSPAN = YMAX - YMIN
         ZVIEW = ZMAX + 2D0*MAX(XSPAN, YSPAN)
         OVECX = 0D0
         OVECY = 0D0
         OVECZ = 1D0
         OANGL = 0D0
      END IF
      WRITE(60,'(4X,A/6X,A,3(2X,G),A/6X,A,4(2X,G),A/6X,A,3(2X,G),A)')
     1   "<Viewpoint description='centered_above'",
     2   "position='", XVIEW, YVIEW, ZVIEW, "'",
     3   "orientation='", OVECX, OVECY, OVECZ, OANGL, "'",
     4   "centerOfRotation='", XMID, YMID, ZMID, "'/>"
      WRITE(60,'(4X,A)') "<Background skyColor='1. 1. 1.'/>"
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE BBOX(XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....FIND BOUNDING BOX.
      XMIN = HUGE(1D0)
      XMAX = -HUGE(1D0)
      YMIN = HUGE(1D0)
      YMAX = -HUGE(1D0)
      ZMIN = HUGE(1D0)
      ZMAX = -HUGE(1D0)
      DO 200 IV=1,NVER
         XMIN = MIN(XV(IV), XMIN)
         XMAX = MAX(XV(IV), XMAX)
         YMIN = MIN(YV(IV), YMIN)
         YMAX = MAX(YV(IV), YMAX)
  200 CONTINUE
      DO 220 IE=1,NELV
         ZMIN = MIN(ELV(IE), ZMIN)
         ZMAX = MAX(ELV(IE), ZMAX)
  220 CONTINUE
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE DRWNOD(LHIGH, RADNOD, RED, GRN, BLU, TRN)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LHIGH
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....NODES AS SPHERES.
      DO 450 IC=1,NCEL
         IF (LCHIGH(IC).NE.LHIGH) CYCLE
         IN = ICN(IC)
         WRITE(60,'(6X,A,3(2X,G),A)') "<Transform translation='",
     1      XN(IN), YN(IN), ZN(IN), "'>"
         WRITE(60,'(8X,A)') "<Shape>"
C........APPEARANCE.
         WRITE(60,'(10X,A/12X,A,A,3(F6.3,1X),A,A,F6.3,A/10X,A)')
     1   "<Appearance>","<Material diffuseColor='0 0 0' ",
     2   "emissiveColor='", RED, GRN, BLU, "' ",
     3   "transparency='", TRN, "'/>", "</Appearance>"
         WRITE(60,'(10X,A,F6.3,A)') "<Sphere radius='", RADNOD, "'/>"
         WRITE(60,'(8X,A/6X,A)') "</Shape>", "</Transform>"
  450 CONTINUE
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE DRWCEL(LHIGH, RED, GRN, BLU, TRN)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LHIGH
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....CELL OUTLINES AS INDEXED LINE SET.
      WRITE(60,'(6X,A)') "<Shape>"
C.....APPEARANCE.
         WRITE(60,'(8X,A/10X,A,A,3(F6.3,1X),A,A,F6.3,A/8X,A)')
     1   "<Appearance>","<Material diffuseColor='0 0 0' ",
     2   "emissiveColor='", RED, GRN, BLU, "' ",
     3   "transparency='", TRN, "'/>", "</Appearance>"
C.....INDEXED LINE SET.
      WRITE(60,'(8X,A)') "<IndexedLineSet coordIndex='"
      ICOUNT = -1
      DO 700 IC=1,NCEL
         IF (LCHIGH(IC).EQ.LHIGH) CALL CELLNS(IC, ICOUNT)
  700 CONTINUE
C.....POINT COORDINATES FOR INDEXED LINE SET.
      WRITE(60,'(10X,A/10X,A)') "'>", "<Coordinate point='"
      ICOUNT = -1
      DO 880 IC=1,NCEL
         IF (LCHIGH(IC).EQ.LHIGH) CALL CELPTS(IC, ICOUNT)
  880 CONTINUE
      WRITE(60,'(10X,A)') "'/>"
      WRITE(60,'(8X,A/6X,A)') "</IndexedLineSet>", "</Shape>"
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE CELLNS(IC, ICOUNT)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IPV(NCVMAX+1)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
      IPV = ICV(IC,:)
      CALL REMNEG(IPV)
C
      IC0 = ICOUNT
      IVL = NUMENT(IPV, NCVMAX+1, IPV(1))
C.....CELL TOP.
      WRITE(60,'(10X,999I)') (ICOUNT+IV, IV=1,IVL), ICOUNT+1, -1
      ICOUNT = ICOUNT + IVL
C.....CELL BOTTOM.
      WRITE(60,'(10X,999I)') (ICOUNT+IV, IV=1,IVL), ICOUNT+1, -1
      ICOUNT = ICOUNT + IVL
C.....CELL'S VERTICAL EDGES.
      WRITE(60,'(10X,999I)') (IC0+IV, IC0+IVL+IV, -1, IV=1,IVL)
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE CELPTS(IC, ICOUNT)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IPV(NCVMAX+1)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
      IPV = ICV(IC,:)
      CALL REMNEG(IPV)
C
      IVL = NUMENT(IPV, NCVMAX+1, IPV(1))
C.....CELL TOP.
      ZTOP = ELV(ICETOP(IC))
      DO 600 IV=1,IVL
          ICV1 = IPV(IV)
          IF (ICV1.LE.0) CYCLE
          WRITE(60,'(10X,3G)') XV(ICV1), YV(ICV1), ZTOP
  600 CONTINUE
      ICOUNT = ICOUNT + IVL
C.....CELL BOTTOM.
      ZBOT = ELV(ICEBOT(IC))
      DO 620 IV=1,IVL
          ICV1 = IPV(IV)
          IF (ICV1.LE.0) CYCLE
          WRITE(60,'(10X,3G)') XV(ICV1), YV(ICV1), ZBOT
  620 CONTINUE
      ICOUNT = ICOUNT + IVL
C
      RETURN
      END
C
C
C=======================================================================
C
      SUBROUTINE DRWHCO(LDHI, REDHI, GRNHI, BLUHI, TRNHI,
     1   LDHL, REDHL, GRNHL, BLUHL, TRNHL, LBOTH)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDHI, LDHL, LBOTH
      LOGICAL LT1B2, LT2B1, LOVER
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....DRAW HORIZONTAL-CONNECTION INTERFACES AND PERPENDICULAR LENGTHS
C        FOR HIGHLIGHTED CELLS.
      DO 500 IC1=1,NCEL
         IF (.NOT.LCHIGH(IC1)) CYCLE
         DO 400 IC2=1,NCEL        ! kluge: brute-force search
            IF ((LBOTH.AND.(.NOT.LCHIGH(IC2))).OR.(IC2.EQ.IC1)) CYCLE
            CALL HCONN(IC1, IC2, LOVER, IVA, IVB, ZHI, ZLO)
            IF (LOVER) THEN
               XA = XV(IVA)
               YA = YV(IVA)
               XB = XV(IVB)
               YB = YV(IVB)
               IF (LDHI) CALL DRWREC(XA, YA, XB, YB, ZHI, ZLO,
     1            REDHI, GRNHI, BLUHI, TRNHI)
               IF (LDHL) THEN
                  INOD = ICN(IC1)
                  XNOD = XN(INOD)
                  YNOD = YN(INOD)
                  ZNOD = ZN(INOD)
                  CALL XYINT(XNOD, YNOD, XA, YA, XB, YB, XINT, YINT)
                  ZINT = ZNOD
                  CALL DRWLIN(XNOD, YNOD, ZNOD, XINT, YINT, ZINT,
     1               REDHL, GRNHL, BLUHL, TRNHL)
               END IF
            END IF
  400    CONTINUE
  500 CONTINUE
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE HCONN(IC1, IC2, LOVER, IVA, IVB, ZHI, ZLO)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LT1B2, LT2B1, LOVER, LSHARE
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....CHECK FOR HORIZONTAL CONNECTION.
      ITOP1 = ICETOP(IC1)
      IBOT1 = ICEBOT(IC1)
      ITOP2 = ICETOP(IC2)
      IBOT2 = ICEBOT(IC2)
      ZTOP1 = ELV(ITOP1)
      ZBOT1 = ELV(IBOT1)
      ZTOP2 = ELV(ITOP2)
      ZBOT2 = ELV(IBOT2)
      LT1B2 = (ZTOP1.GT.ZBOT2)
      LT2B1 = (ZTOP2.GT.ZBOT1)
      IF (LT1B2.AND.LT2B1) THEN
         CALL ESHARE(IC1, IC2, LSHARE, IVA, IVB)
         LOVER = LSHARE
         IF (LOVER) THEN
            ZHI = MIN(ZTOP1, ZTOP2)
            ZLO = MAX(ZBOT1, ZBOT2)
         ELSE
            IVA = 0
            IVB = 0
            ZHI = -HUGE(1D0)
            ZLO = -HUGE(1D0)
         END IF
      ELSE
         LOVER = .FALSE.
         IVA = 0
         IVB = 0
         ZHI = -HUGE(1D0)
         ZLO = -HUGE(1D0)
      END IF
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE ESHARE(IC1, IC2, LSHARE, IVA, IVB)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LSHARE, LGOT1
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....CHECK FOR SHARED EDGE.  TRUE IF THERE EXIST TWO SHARED VERTICES,
C        EACH OF WHICH IS A "PRIMARY" VERTEX (AS DISTINGUISHED FROM A
C        "REFINEMENT" VERTEX) FOR AT LEAST ONE OF THE TWO CELLS.
C
      LSHARE = .FALSE.
      LGOT1 = 0
      IVA = 0
      IVB = 0
      IVL1 = NUMENT(ICV(IC1,:), NCVMAX+1, ICV(IC1,1))
      IVL2 = NUMENT(ICV(IC2,:), NCVMAX+1, ICV(IC2,1))
      DO 300 IV1=1,IVL1
         IPV1 = ICV(IC1,IV1)
         IPV1AB = IABS(IPV1)
         DO 200 IV2=1,IVL2
            IPV2 = ICV(IC2,IV2)
            IPV2AB = IABS(IPV2)
            IF (IPV1AB.EQ.IPV2AB) THEN
               IF ((IPV1.GT.0).OR.(IPV2.GT.0)) THEN
                  IF (.NOT.LGOT1) THEN
                     IVA = IPV1AB
                     LGOT1 = .TRUE.
                     CYCLE
                  ELSE
                     IVB = IPV1AB
                     LSHARE = .TRUE.
                     GOTO 999
                  END IF
               END IF
            END IF
  200    CONTINUE
  300 CONTINUE
C
  999 RETURN
      END
C
C=======================================================================
C
      SUBROUTINE XYINT(XNOD, YNOD, XA, YA, XB, YB, XINT, YINT)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....FIND INTERSECTION WITH EDGE AB OF PERPENDICULAR LINE PASSING
C        THROUGH NODE.
C
      ANUM = (XB - XA)*(XNOD - XB) + (YB - YA)*(YNOD - YB)
      ADEN = (XB - XA)*(XB - XA) + (YB - YA)*(YB - YA)
      ALPHA = -ANUM/ADEN
      OMALP = 1D0 - ALPHA
      XINT = ALPHA*XA + OMALP*XB
      YINT = ALPHA*YA + OMALP*YB
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE DRWVCO(LDVI, REDVI, GRNVI, BLUVI, TRNVI,
     1   LDVL, REDVL, GRNVL, BLUVL, TRNVL, LBOTH)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDVI, LDVL, LBOTH
      LOGICAL LOVER
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....DRAW VERTICAL-CONNECTION INTERFACES AND PERPENDICULAR LENGTHS
C        FOR HIGHLIGHTED CELLS.
      DO 500 IC1=1,NCEL
         IF (.NOT.LCHIGH(IC1)) CYCLE
         DO 400 IC2=1,NCEL        ! kluge: brute-force search
            IF ((LBOTH.AND.(.NOT.LCHIGH(IC2))).OR.(IC2.EQ.IC1)) CYCLE
            CALL VCONN(IC1, IC2, LOVER, ICSM, ICLG, Z)
            IF (LOVER) THEN
               IF (LDVI) THEN
                  CALL DRWPOL(ICV(ICSM,:), Z, REDVI, GRNVI, BLUVI,
     1               TRNVI)
               END IF
               IF (LDVL) THEN
                  INOD = ICN(IC1)
                  X = XN(INOD)
                  Y = YN(INOD)
                  ZNOD = ZN(INOD)
                  CALL DRWLIN(X, Y, ZNOD, X, Y, Z, REDVL, GRNVL, BLUVL,
     1               TRNVL)
               END IF
            END IF
  400    CONTINUE
  500 CONTINUE
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE VCONN(IC1, IC2, LOVER, ICSM, ICLG, Z)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LT1B2, LT2B1, LOVER, PTINPG
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
      EXTERNAL PTINPG
C
C.....CHECK FOR VERTICAL CONNECTION.
      ITOP1 = ICETOP(IC1)
      IBOT1 = ICEBOT(IC1)
      ITOP2 = ICETOP(IC2)
      IBOT2 = ICEBOT(IC2)
      LT1B2 = (ITOP1.EQ.IBOT2)
      LT2B1 = (ITOP2.EQ.IBOT1)
      IF (LT1B2.OR.LT2B1) THEN
         AREA1 = ARPO(ICV(IC1,:))
         AREA2 = ARPO(ICV(IC2,:))
         IF (AREA1.LT.AREA2) THEN
            ICSM = IC1
            ICLG = IC2
         ELSE
            ICSM = IC2
            ICLG = IC1
         END IF
         LOVER = PTINPG(XN(ICSM), YN(ICSM), ICV(ICLG,:))
         IF (LOVER) THEN
            IF (LT1B2) THEN
               Z = ELV(ITOP1)
            ELSE
               Z = ELV(ITOP2)
            END IF
         ELSE
            ICSM = 0
            ICLG = 0
            Z = -HUGE(1D0)
         END IF
      ELSE
         LOVER = .FALSE.
         ICSM = 0
         ICLG = 0
         Z = -HUGE(1D0)
      END IF
C
      RETURN
      END
C
C=======================================================================
C
      FUNCTION ARPO(IPV)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IPV(NCVMAX+1)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
      CALL REMNEG(IPV)
C
C.....AREA OF A POLYGON GIVEN ITS VERTICES.
      IVL = NUMENT(IPV, NCVMAX+1, IPV(1))
      SUM = 0D0
      DO 200 IV=1,IVL
         XA = XV(IPV(IV))
         YA = YV(IPV(IV))
         XB = XV(IPV(IV+1))
         YB = YV(IPV(IV+1))
         SUM = SUM + XA*YB - YA*XB
  200 CONTINUE
      ARPO = 5D-1*SUM
C
      RETURN
      END
C
C=======================================================================
C
      LOGICAL FUNCTION PTINPG(XC, YC, IPV)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IPV(NCVMAX+1)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
      CALL REMNEG(IPV)
C
C.....CHECK FOR POINT IN POLYGON.
      PTINPG = .TRUE.
      DO 300 IV=1,NCVMAX
         IVA = IV
         IVB = IV + 1
         IPVA = IPV(IVA)
         IPVB = IPV(IVB)
         XA = XV(IPVA)
         YA = YV(IPVA)
         XB = XV(IPVB)
         YB = YV(IPVB)
         TERM = (XB - XA)*(YC - YA) - (YB - YA)*(XC - XA)
         IF (TERM.LE.0D0) THEN
            PTINPG = .FALSE.
            EXIT
         ELSE IF (IPVB.EQ.IPV(1)) THEN
            EXIT
         END IF
  300 CONTINUE
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE DRWPOL(IPV, Z, RED, GRN, BLU, TRN)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IPV(NCVMAX+1)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....POLYGON AS INDEXED FACE SET.
      WRITE(60,'(6X,A)') "<Shape>"
C.....APPEARANCE.
      WRITE(60,'(8X,A/10X,A,A,3(F6.3,1X),A,A,F6.3,A/8X,A)')
     1   "<Appearance>","<Material diffuseColor='0 0 0' ",
     2   "emissiveColor='", RED, GRN, BLU, "' ",
     3   "transparency='", TRN, "'/>", "</Appearance>"
C.....INDEXED FACE SET.
      WRITE(60,'(8X,A)') "<IndexedFaceSet solid='false' coordIndex='"
      CALL POLLNS(IPV)
C.....POINT COORDINATES FOR INDEXED FACE SET.
      WRITE(60,'(10X,A/10X,A)') "'>", "<Coordinate point='"
      CALL POLPTS(IPV, Z)
      WRITE(60,'(10X,A)') "'/>"
      WRITE(60,'(8X,A/6X,A)') "</IndexedFaceSet>", "</Shape>"
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE REMNEG(IPV)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IPV(NCVMAX+1)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....REMOVE NEGATIVE VERTEX NUMBERS.
      IVNEW = 0
      DO 100 IV=1,NCVMAX+1
         IPV1 = IPV(IV)
         IF (IPV1.GT.0) THEN
            IVNEW = IVNEW + 1
            IPV(IVNEW) = IPV1
         END IF
  100 CONTINUE
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE DRWLIN(X1, Y1, Z1, X2, Y2, Z2, RED, GRN, BLU, TRN)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....SINGLE LINE AS LINE SET.
      WRITE(60,'(6X,A)') "<Shape>"
C.....APPEARANCE.
      WRITE(60,'(8X,A/10X,A,A,3(F6.3,1X),A,A,F6.3,A/8X,A)')
     1   "<Appearance>","<Material diffuseColor='0 0 0' ",
     2   "emissiveColor='", RED, GRN, BLU, "' ",
     3   "transparency='", TRN, "'/>", "</Appearance>"
C.....LINE SET.
      WRITE(60,'(8X,A)') "<LineSet vertexCount='2'>"
      WRITE(60,'(10X,A,6(2X,G),A)') "<Coordinate point='",
     1   X1, Y1, Z1, X2, Y2, Z2, "'/>"
      WRITE(60,'(8X,A/6X,A)') "</LineSet>", "</Shape>"
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE DRWREC(XA, YA, XB, YB, ZHI, ZLO,
     1         RED, GRN, BLU, TRN)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
C.....RECTANGLE AS INDEXED FACE SET.
      WRITE(60,'(6X,A)') "<Shape>"
C.....APPEARANCE.
      WRITE(60,'(8X,A/10X,A,A,3(F6.3,1X),A,A,F6.3,A/8X,A)')
     1   "<Appearance>","<Material diffuseColor='0 0 0' ",
     2   "emissiveColor='", RED, GRN, BLU, "' ",
     3   "transparency='", TRN, "'/>", "</Appearance>"
C.....INDEXED FACE SET.
      WRITE(60,'(8X,A,A)') "<IndexedFaceSet solid='false' coordIndex='",
     1   "0 1 2 3 0 -1'>"
C.....POINT COORDINATES FOR INDEXED FACE SET.
      WRITE(60,'(10X,A,4(/10X,3G)/10X,A)') "<Coordinate point='",
     1   XA, YA, ZHI, XB, YB, ZHI, XB, YB, ZLO, XA, YA, ZLO, "'/>"
      WRITE(60,'(8X,A/6X,A)') "</IndexedFaceSet>", "</Shape>"
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE POLLNS(IPV)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IPV(NCVMAX+1)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
      CALL REMNEG(IPV)
C
      IVL = NUMENT(IPV, NCVMAX+1, IPV(1))
      WRITE(60,'(10X,999I)') (IV, IV=0,IVL-1), 0, -1
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE POLPTS(IPV, Z)
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IPV(NCVMAX+1)
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
      CALL REMNEG(IPV)
C
      IVL = NUMENT(IPV, NCVMAX+1, IPV(1))
      DO 600 IV=1,IVL
          IPV1 = IPV(IV)
          WRITE(60,'(10X,3G)') XV(IPV1), YV(IPV1), Z
  600 CONTINUE
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE BYEBYE()
      USE GRMODS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER CDUM*1
      COMMON /NDIMS/ NVER,NELV,NCEL,NCVMAX,NNOD
C
      CLOSE(60)
C
      DEALLOCATE (XV,YV)
      DEALLOCATE (ELV)
      DEALLOCATE (ICN,ICETOP,ICEBOT,ICV)
      DEALLOCATE (LCHIGH)
      DEALLOCATE (XN,YN,ZN)
C
      PRINT *, "Done."
      READ(*,'(A)') CDUM
C
      RETURN
      END