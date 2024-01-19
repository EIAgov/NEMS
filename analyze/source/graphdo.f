C               ::: GRAPHDO.FOR 3-21-92 :::
C
C LAST DATE:  12-12-90
C              2-04-91...Changed err msg for null submatrix
C
C This contains the following subroutines for the GRAPH module of ANALYZE.
C
C     AGRAPH.....Execute GRAPH command (link with outside)
C     GRAPH0.....Clear graph (MUST CALL UPON READING NEW LP)
C     GPHNEW.....Construct new Bigraph from submatrix
C     GPHROW.....Construct Row Digraph
C     GPHCOL.....Construct Column Digraph
C     GPHDBG.....Graph debug (called from SYSDBG)
C
      SUBROUTINE AGRAPH(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCGRAPH.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCGRAPH)
CI$$INSERT DCANAL
CI$$INSERT DCGRAPH
C
C This parses CLIST(FIRST:LAST) to execute the GRAPH command.
C ...GOES TO OUTPUT
C
C Syntax:  GRAPH [ SET  [ROW | COL]
C                | DISPLAY  [node] [//options]
C                | COPY
C                | {SAVE| LOAD} [filespec] ]
C
      CHARACTER*128 CLIST
C LOCAL
      PARAMETER    (LCLOPT = 5)
      CHARACTER*8  OPTION(LCLOPT)
      LOGICAL*1    PICK(2),SW
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
CD      IF(SWDBG)THEN
CD         PRINT *,' ENTERED AGRAPH WITH CLIST(',FIRST,':)='
CD         PRINT *,' ',CLIST(FIRST:LAST+1)
CD         PAUSE = PAUSE - 2
CD         IF(PAUSE.LE.0)CALL SYSDBG
CD      ENDIF
C
C PARSE GRAPH COMMAND
      OPTION(1) = 'SET     '
      OPTION(2) = 'DISPLAY '
      OPTION(3) = 'COPY    '
      OPTION(4) = 'SAVE    '
      OPTION(5) = 'LOAD    '
      NUMBER = LCLOPT
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)RETURN
      IF(NUMBER.EQ.0)THEN
C WRITE SUMMARY
         CALL GRSUMR
         RETURN
      ENDIF
C
C BRANCH ON NUMBER
      GOTO (100,200,300,500,500),NUMBER
C
100   CONTINUE
C SET [ROW COL] [//{attribute}]
      CALL FLOOK(CLIST,FIRST,LAST,'//',BEGATR)
      IF(BEGATR.GT.0)THEN
         L = LAST
         LAST = BEGATR-1
      ENDIF
      OPTION(1) = 'ROW  '
      OPTION(2) = 'COL  '
      NUMBER = 2
      CALL FOPSET(CLIST,FIRST,LAST,OPTION,NUMBER,PICK,RCODE)
      IF(RCODE.NE.0)RETURN
C
      IF(BEGATR.GT.0)THEN
         FIRST = BEGATR+2
         CALL GPHTRN(CLIST,FIRST,L,RCODE)
         IF(RCODE.NE.0)RETURN
      ENDIF
      CALL GPHNEW(RCODE)
      IF(RCODE.NE.0)RETURN
      IF(BGROWS.EQ.0 .OR. BGCOLS.EQ.0)THEN
         IF(SWMSG)PRINT *,' NO GRAPH IS GENERATED'
         CALL GRAPH0
         RETURN
      ENDIF
      IF(PICK(1))CALL GPHROW
      IF(PICK(2))CALL GPHCOL
      RETURN
C
200   CONTINUE
C DISPLAY [{ROW | COL}] [//options]
      IF(BGROWS.EQ.0)THEN
         PRINT *,' NO GRAPH IS SET'
         RCODE = 1
      ELSE
         CALL GPHDSP(CLIST,FIRST,LAST,RCODE)
      ENDIF
      RETURN
C
300   CONTINUE
C COPY
      IF(BGROWS.EQ.0)THEN
         PRINT *,' NO GRAPH IS SET'
         RCODE = 1
      ELSE
         SW = .TRUE.
         DO 350 I=1,BGROWS
            ROW = ROWNOD(I)
            CALL GPUTMP('ROW ',ROW,SW)
350      CONTINUE
         CALL ASETSB('ROW ')
C
         DO 360 J=1,BGCOLS
            COL = COLNOD(J)
            CALL GPUTMP('COL ',COL,SW)
360      CONTINUE
      ENDIF
      CALL ASETSB('COL ')
      RETURN
C
500   CONTINUE
C SAVE | LOAD [filespec]
      CALL GRPHIO(OPTION(NUMBER),CLIST,FIRST,LAST,RCODE)
      RETURN
C
C ** AGRAPH ENDS HERE
      END
      SUBROUTINE GRAPH0
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCGRAPH.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCGRAPH)
CI$$INSERT DCANAL
CI$$INSERT DCGRAPH
C
C  This clears graph...MUST BE CALLED FOR NEW LP.
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      BGROWS = 0
      BGCOLS = 0
      RGNODE = 0
      CGNODE = 0
      VERGPH = '1.0'
      RETURN
C
C ** GRAPH0 ENDS HERE
      END
      SUBROUTINE GPHNEW(RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCGRAPH.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCGRAPH)
CI$$INSERT DCANAL
CI$$INSERT DCGRAPH
C
C This constructs new graph.
C
      DIMENSION    GPHMAP(PMXROW)
      EQUIVALENCE (GPHMAP(1),SCRIND(1))
C LOCAL
      CHARACTER*1 CHAR
      LOGICAL*1   SW
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
CD      IF(SWDBG)THEN
CD        PRINT *,' ENTERED GPHNEW'
CD         PAUSE = PAUSE - 1
CD         IF(PAUSE.LE.0)CALL SYSDBG
CD      ENDIF
C
      CALL GRAPH0
      CALL GETMAP('ROW ',OBJNUM,SW)
      IF(SW)THEN
C REMOVE OBJECTIVE ROW FROM SUBMATRIX
         SW = .FALSE.
         CALL GPUTMP('ROW ',OBJNUM,SW)
C ...SET ROW STATS (AND SWSBNZ BECOMES FALSE)
         CALL ASETSB('ROW ')
      ENDIF
      IF(.NOT.SWSBNZ)THEN
C REMOVE NULL ROWS AND COLUMNS FROM SUBMATRIX
         CALL GMAPNZ(NRCSUB(1),RCSUB1(1),NRCSUB(2),RCSUB1(2),NZSUB)
         SWSBNZ = .TRUE.
      ENDIF
      IF(NZSUB.EQ.0)THEN
         PRINT *,' ** SUBMATRIX IS NULL...NO GRAPH IS SET'
         RETURN
      ENDIF
C FORM BIGRAPH ADJACENCY MATRIX
      BGROWS = NRCSUB(1)
      BGCOLS = NRCSUB(2)
      IF(BGROWS.GT.PBGROW .OR. BGCOLS.GT.PBGCOL)THEN
         PRINT *,' Sorry, submatrix is too large'
         PRINT *,' ...Bigraph is limited to ',PBGROW,'x',PBGCOL
         RCODE = 1
         CALL GRAPH0
         RETURN
      ENDIF
C
C INITIALIZE ADJACENCY MATRIX OF FUNDAMENTAL BIGRAPH
      DO 50 I=1,BGROWS
      DO 50 J=1,BGCOLS
50    BGMAT(I,J) = ' '
C
C SET NODE LIST
      NODE = 0
      DO 100 I=1,NROWS
         CALL GETMAP('ROW ',I,SW)
         IF(.NOT.SW)THEN
           GPHMAP(I) = 0
         ELSE
           NODE = NODE+1
           ROWNOD(NODE) = I
           GPHMAP(I) = NODE
         ENDIF
100   CONTINUE
C
110   CONTINUE
      N = NRCSUB(2)
      NODE = 0
      DO 200 J=RCSUB1(2),NCOLS
         CALL GETMAP('COL ',J,SW)
         IF(.NOT.SW)GOTO 200
         NODE = NODE+1
         COLNOD(NODE) = J
         CALL GETCOL(J,NZ,MAXLST,ROWLST,VALLST)
         DO 150 I=1,NZ
           ROW = ROWLST(I)
           BGROW = GPHMAP(ROW)
           IF(BGROW.EQ.0)GOTO 150
C PUT SIGN OF NONZERO INTO BIGRAPH ADJACENCY MATRIX
           IF(VALLST(I).GT.0.)THEN
              BGMAT(BGROW,NODE) = '+'
           ELSE
              BGMAT(BGROW,NODE) = '-'
           ENDIF
150      CONTINUE
         IF(NODE.EQ.N)GOTO 210
200   CONTINUE
C
210   CONTINUE
C REVERSE SIGNS OF <= ROWS (WITHOUT RANGES)
      DO 500 I=1,BGROWS
         ROW = ROWNOD(I)
         CALL GETYPE(ROW,CHAR)
         CALL GETBND('ROW ',ROW,VL,VU)
         IF(CHAR.EQ.'L'.AND.VL.LE.-VINF)THEN
C ...REVERSE
            DO 400 J=1,BGCOLS
               IF(BGMAT(I,J).EQ.'+')THEN
                  BGMAT(I,J) = '-'
               ELSE IF(BGMAT(I,J).EQ.'-')THEN
                  BGMAT(I,J) = '+'
               ENDIF
400         CONTINUE
         ENDIF
500   CONTINUE
C   FUNDAMENTAL BIGRAPH IS SET...BGMAT = ADJACENCY MATRIX
      RETURN
C
C ** GPHNEW ENDS HERE
      END
      SUBROUTINE GPHROW
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCGRAPH.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCGRAPH)
CI$$INSERT DCANAL
CI$$INSERT DCGRAPH
C
C This constructs Row Digraph.
C
C LOCAL
      CHARACTER*1 CHAR,CHAR1
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
C INITIALIZE ROW GRAPH ADJACENCY
      RGNODE = BGROWS
      DO 50 I=1,RGNODE
      DO 50 J=1,RGNODE
50    RGMAT(I,J) = ' '
C
      IF(RGNODE.EQ.1 .OR. BGCOLS.EQ.0)RETURN
C
C ::: LOOP OVER COLUMNS OF BIGRAPH :::
      DO 800 J=1,BGCOLS
C FORM LISTS OF NEGATIVE NONZEROES (BOTTOM OF SCRIND)
C           AND POSITIVE NONZEROES (TOP OF SCRIND)
         NNEG = 0
         NPOS = 0
C
         DO 200 I=1,BGROWS
            CHAR = BGMAT(I,J)
            IF(CHAR.EQ.'-')THEN
C ENTER -
               NNEG = NNEG+1
               SCRIND(BGROWS+NNEG) = I
            ELSE IF(CHAR.EQ.'+')THEN
C ENTER +
               NPOS = NPOS + 1
               SCRIND(NPOS) = I
            ENDIF
200      CONTINUE
C
         IF(NNEG.EQ.0 .AND. NPOS.GT.0)THEN
C COLUMN (J) IS NON-NEGATIVE...ADD TAIL-LESS ARC TO ROWS WITH +
            DO 300 K=1,NPOS
               I = SCRIND(K)
               IF(RGMAT(I,I).EQ.'+')THEN
                  RGMAT(I,I) = '*'
               ELSE
                  RGMAT(I,I) = '-'
               ENDIF
300         CONTINUE
         ELSE IF(NPOS.EQ.0 .AND. NNEG.GT.0)THEN
C COLUMN (J) IS NON-POSITIVE...ADD HEADLESS ARC TO ROWS WITH -
            DO 350 K=1,NNEG
               I = SCRIND(BGROWS+K)
               IF(RGMAT(I,I).EQ.'-')THEN
                  RGMAT(I,I) = '*'
               ELSE
                  RGMAT(I,I) = '+'
               ENDIF
350         CONTINUE
         ELSE
C COLUMN (J) IS LINKING...PUT ARC FROM EVERY - TO EVERY +
            DO 600 I=1,NNEG
               FROM = SCRIND(BGROWS+I)
               DO 500 K=1,NPOS
                  TO = SCRIND(K)
                  RGMAT(FROM,TO) = '1'
500            CONTINUE
600         CONTINUE
         ENDIF
C NEXT COLUMN
800   CONTINUE
C
C FINALLY, SET TAIL-LESS/HEADLESS ARCS FOR NON-ZERO RHS'S
      DO 900 I=1,RGNODE
         CHAR = RGMAT(I,I)
         IF(CHAR.EQ.'*')GOTO 900
C
         ROW = ROWNOD(I)
         CALL GETYPE(ROW,CHAR1)
         CALL GETBND('ROW ',ROW,VL,VU)
         IF(CHAR1.EQ.'L'.AND.VL.LE.-VINF)THEN
C REVERSE INEQUALITY
            VL = -VU
            VU = VINF
         ENDIF
C
C   ---->(ROW)--->
C   :         :
C   :         :....LOWER BOUND > 0 | UPPER BOUND < 0
C   :...UPPER BOUND > 0 | LOWER BOUND < 0
C
         IF(VL.GT.0. .OR. VU.LT.0.)THEN
            IF(CHAR.EQ.'-')THEN
               RGMAT(I,I) = '*'
               GOTO 900
            ENDIF
            CHAR = '+'
            RGMAT(I,I) = CHAR
         ENDIF
         IF((VU.GT.0. .AND. VU.LT. VINF) .OR.
     1      (VL.LT.0. .AND. VL.GT.-VINF) )THEN
            IF(VL.LT.VU)THEN
               IF(CHAR.EQ.'+')THEN
                  RGMAT(I,I) = '*'
               ELSE
                  RGMAT(I,I) = '-'
               ENDIF
            ENDIF
         ENDIF
900   CONTINUE
C
C NOW RGMAT = ADJACENCY OF ROWS, WHERE
C  RGMAT(I,I) = + MEANS     (I)---->
C               - MEANS --->(I)
C               * MEANS --->(I)--->
      RETURN
C
C ** GPHROW ENDS HERE
      END
      SUBROUTINE GPHCOL
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCGRAPH.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCGRAPH)
CI$$INSERT DCANAL
CI$$INSERT DCGRAPH
C
C This constructs Column Graph.
C
C LOCAL
      CHARACTER*1 CHAR
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
C INITIALIZE COLUMN GRAPH ADJACENCY
      CGNODE = BGCOLS
      DO 50 I=1,CGNODE
      DO 50 J=1,CGNODE
50    CGMAT(I,J) = ' '
C
      IF(CGNODE.EQ.1 .OR. BGROWS.EQ.0)RETURN
C
C ::: LOOP OVER ROWS OF BIGRAPH :::
      DO 800 I=1,BGROWS
C FORM LISTS OF NEGATIVE NONZEROES (BOTTOM OF SCRIND)
C           AND POSITIVE NONZEROES (TOP OF SCRIND)
         NNEG = 0
         NPOS = 0
C
         DO 200 J=1,BGCOLS
            CHAR = BGMAT(I,J)
            IF(CHAR.EQ.'-')THEN
C ENTER -
               NNEG = NNEG+1
               SCRIND(BGCOLS+NNEG) = J
            ELSE IF(CHAR.EQ.'+')THEN
C ENTER +
               NPOS = NPOS + 1
               SCRIND(NPOS) = J
            ENDIF
200      CONTINUE
C
         IF(NNEG.EQ.0 .AND. NPOS.GT.0)THEN
C ROW (I) IS NON-NEGATIVE...ADD HEADLESS ARC TO COLUMNS WITH +
            DO 300 K=1,NPOS
               J = SCRIND(K)
               IF(CGMAT(J,J).EQ.'-')THEN
                  CGMAT(J,J) = '*'
               ELSE
                  CGMAT(J,J) = '+'
               ENDIF
300         CONTINUE
         ELSE IF(NPOS.EQ.0 .AND. NNEG.GT.0)THEN
C ROW (I) IS NON-POSITIVE...ADD TAIL-LESS ARC TO COLUMNS WITH -
            DO 350 K=1,NNEG
               J = SCRIND(BGCOLS+K)
               IF(CGMAT(J,J).EQ.'+')THEN
                  CGMAT(J,J) = '*'
               ELSE
                  CGMAT(J,J) = '-'
               ENDIF
350         CONTINUE
         ELSE
C ROW (I) IS LINKING...PUT ARC FROM EVERY + TO EVERY -
            DO 600 J=1,NNEG
               TO = SCRIND(BGCOLS+J)
               DO 500 K=1,NPOS
                  FROM = SCRIND(K)
                  CGMAT(FROM,TO) = '1'
500            CONTINUE
600         CONTINUE
         ENDIF
C NEXT ROW
800   CONTINUE
C
C FINALLY, SET TAIL-LESS/HEADLESS ARCS FOR NON-ZERO BOUNDS
      DO 900 J=1,CGNODE
         CHAR = CGMAT(J,J)
         IF(CHAR.EQ.'*')GOTO 900
C
         COL = COLNOD(J)
         CALL GETBND('COL ',COL,VL,VU)
C
C   ---->[COL]--->
C   :         :
C   :         :....LOWER BOUND > 0 | UPPER BOUND < 0
C   :...UPPER BOUND > 0 | LOWER BOUND < 0
C
C FIXED LEVELS HAVE ONLY 1:  ---> IF X > 0, <--- IF X < 0
         IF(VL.GT.0. .OR. VU.LT.0.)THEN
            IF(CHAR.EQ.'-')THEN
               CGMAT(J,J) = '*'
               GOTO 900
            ENDIF
            CHAR = '+'
            CGMAT(J,J) = CHAR
         ENDIF
         IF((VU.GT.0. .AND. VU.LT. VINF) .OR.
     1      (VL.LT.0. .AND. VL.GT.-VINF) )THEN
            IF(VL.LT.VU)THEN
               IF(CHAR.EQ.'+')THEN
                  CGMAT(J,J) = '*'
               ELSE
                  CGMAT(J,J) = '-'
               ENDIF
            ENDIF
         ENDIF
900   CONTINUE
C
C NOW CGMAT = ADJACENCY OF COLUMNS, WHERE
C  CGMAT(J,J) = + MEANS     [J]---->
C               - MEANS --->[J]
C               * MEANS --->[J]--->
      RETURN
C
C ** GPHCOL ENDS HERE
      END
      SUBROUTINE GPHDBG
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCGRAPH.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCGRAPH)
CI$$INSERT DCANAL
CI$$INSERT DCGRAPH
C
C This is debug routine entered from SYSDBG
C
C LOCAL
      CHARACTER*1 CHAR
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(PAUSE.LT.0) READ(*,1)CHAR
      PAUSE = PAUSE0
      PRINT *,' ENTERED GRDEBG...VERSION = ',VERGPH
      PRINT *,' PBGROW=',PBGROW,' PBGCOL=',PBGCOL
C
10    PRINT *,' Row, Col, Bigraph, Attributes'
      READ(*,1)CHAR
1     FORMAT(A1)
      IF(CHAR.EQ.' ')RETURN
C
      LINE = 1
      IF(CHAR.EQ.'B')THEN
         PRINT *,' BGROWS=',BGROWS,' BGCOLS=',BGCOLS
         IF(BGROWS.GT.0)THEN
            PRINT *,(COLNOD(J),J=1,BGCOLS)
            LINE = 3
            DO 100 I=1,BGROWS
               CALL FPRMPT(LINE,*900)
               PRINT *,ROWNOD(I),':',(BGMAT(I,J),J=1,BGCOLS)
100         CONTINUE
         ENDIF
      ELSE IF(CHAR.EQ.'R')THEN
         PRINT *,' RGNODE=',RGNODE
         IF(RGNODE.GT.0)THEN
            DO 200 I=1,RGNODE
               CALL FPRMPT(LINE,*900)
               PRINT *,ROWNOD(I),':',(RGMAT(I,J), J=1,RGNODE)
200         CONTINUE
         ENDIF
      ELSE IF(CHAR.EQ.'C')THEN
         PRINT *,' CGNODE=',CGNODE
         IF(CGNODE.GT.0)THEN
            DO 300 I=1,CGNODE
               CALL FPRMPT(LINE,*900)
               PRINT *,COLNOD(I),':',(CGMAT(I,J), J=1,CGNODE)
300         CONTINUE
         ENDIF
      ELSE IF(CHAR.EQ.'A')THEN
         PRINT *,NUMATR,' ATTRIBUTES: ',(ATRTYP(I),I=0,NUMATR)
      ELSE
         PRINT *,'?',CHAR
      ENDIF
      GOTO 10
C
900   RETURN
C
C ** GPHDBG ENDS HERE
      END
