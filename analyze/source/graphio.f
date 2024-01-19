C               ::: GRAPHIO.FOR  6-18-92 :::
C
C Last date:  12-15-90...Added NOHEADER option to syntax display
C              9-19-91...Added boxes for nodes displayed (GRDSP1)
C              9-24-91...Suppressed DIGRAPH head when coming from
C                        rule file
C             10-19-91...Replaced OPEN with FLOPEN
C              6-04-92...Fixed calling GPHROW, GPHCOL (RCODE removed)
C
C This contains the following subroutines for the GRAPH module of ANALYZE.
C
C     GRSUMR.....Graph summary (to OUTPUT)
C     GPHDSP.....Execute GRAPH DISPLAY
C     GRDISP.....Display graph
C     GRDSP1.....Display 1 node
C     GRPHIO.....Save|Load graph (unformatted)
C
      SUBROUTINE GRSUMR
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
C This writes a summary of the graph (to OUTPUT, using FTEXT)
C
C LOCAL
      PARAMETER (MARGIN=10,INDENT=-5)
      CHARACTER*128 CLIST
      CHARACTER*8   STRINT
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      LINE = 1
C
      IF(BGROWS.EQ.0 .AND. BGCOLS.EQ.0)THEN
         CLIST = 'NO GRAPH HAS BEEN SET'
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*900)
         RETURN
      ENDIF
C
      CLIST = 'GRAPH SUMMARY'
      CALL FCENTR(CLIST,FIRST,LAST)
      CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*900)
C
      CALL FI2C(STRINT,BGROWS,FIRST)
      CLIST = 'Bigraph has '//STRINT(FIRST:)//' Row Nodes,'
      CALL FI2C(STRINT,BGCOLS,FIRST)
      CALL FSLEN(CLIST,80,LAST)
      CLIST(LAST+2:) = STRINT(FIRST:)//' Column Nodes and'
      CALL FSLEN(CLIST,80,LAST)
      N = 0
      DO 10 I=1,BGROWS
      DO 10 J=1,BGCOLS
         IF(BGMAT(I,J).NE.' ')N=N+1
10    CONTINUE
      CALL FI2C(STRINT,N,FIRST)
      CLIST(LAST+2:) = STRINT(FIRST:)//' Links'
C BIGRAPH STATS
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*900)
C
C ROW DIGRAPH STATS
      IF(RGNODE.EQ.0)THEN
         CLIST = 'ROW DIGRAPH NOT SET;'
      ELSE
         N = 0
         DO 100 I=1,RGNODE-1
         DO 100 J=I,RGNODE
            IF(RGMAT(I,J).NE.' ')N=N+1
100      CONTINUE
         CALL FI2C(STRINT,N,FIRST)
         CLIST = 'Row Digraph has '//STRINT(FIRST:)//' Links;'
      ENDIF
      CALL FSLEN(CLIST,40,LAST)
C COLUMN DIGRAPH STATS
      IF(CGNODE.EQ.0)THEN
         CLIST(LAST+2:) = 'COLUMN DIGRAPH NOT SET.'
      ELSE
         N = 0
         DO 200 I=1,CGNODE-1
         DO 200 J=I,CGNODE
            IF(CGMAT(I,J).NE.' ')N=N+1
200      CONTINUE
         CALL FI2C(STRINT,N,FIRST)
         CLIST(LAST+2:) = 'Column Digraph has '//
     1                    STRINT(FIRST:)//' Links.'
      ENDIF
      CALL FSLEN(CLIST,80,LAST)
C
      CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*900)
C
900   RETURN
C
C ** GRSUMR ENDS HERE
      END
      SUBROUTINE GPHDSP(CLIST,FIRST,LAST,RCODE)
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
C This displays graph/path (to OUTPUT).
C   Syntax:  GRAPH DISPLAY {type} [//options]
C
C       type   := BIGRAPH | ROW | COL
C       options:  DIAGRAM [GRAPHIC|PRINTABL]| MATRIX | SYNTAX
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*8  OPTION(3),FORMAT,FORM2
      LOGICAL*1    PICK(3)
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
C CHECK THAT GRAPH HAS BEEN SET
      IF(BGROWS.EQ.0 .OR. BGCOLS.EQ.0)THEN
         PRINT *,' GRAPH NOT SET'
         RETURN
      ENDIF
C SET DEFAULTS
      FORMAT = 'DIAGRAM'
      FORM2  = 'GRAPHIC'
C SEE IF OPTION HAS BEEN SPECIFIED
      CALL FLOOK(CLIST,FIRST,LAST,'//',IOPTN)
      IF(IOPTN.GT.0)THEN
C ...YES, PARSE FOR SPEC
         F = IOPTN+2
         OPTION(1) = 'DIAGRAM'
         OPTION(2) = 'MATRIX '
         OPTION(3) = 'SYNTAX'
         NUMBER = 3
         CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
         IF(RCODE.NE.0)RETURN
         IF(NUMBER.GT.0)THEN
            FORMAT = OPTION(NUMBER)
            IF(NUMBER.EQ.1)THEN
C DIAGRAM...GET OPTION
               OPTION(1) = 'GRAPHIC'
               OPTION(2) = 'PRINTABL'
               NUMBER = 2
               CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
               IF(RCODE.NE.0)RETURN
               IF(NUMBER.GT.0)FORM2 = OPTION(NUMBER)
            ELSE IF(NUMBER.EQ.3)THEN
C SYNTAX...SET DEFAULT
               FORM2 = 'HEADER  '
C       ...GET OPTION
               OPTION(1) = 'HEADER  '
               OPTION(2) = 'NOHEADER'
               NUMBER = 2
               CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
               IF(RCODE.NE.0)RETURN
               IF(NUMBER.GT.0)FORM2 = OPTION(NUMBER)
            ENDIF
         ENDIF
         LAST = IOPTN-1
      ENDIF
C OPTION IS SET...PARSE FOR TYPES
      OPTION(1) = 'BIGRAPH'
      OPTION(2) = 'ROW    '
      OPTION(3) = 'COL    '
      NUMBER = 3
      RCODE = -1
      CALL FOPSET(CLIST,FIRST,LAST,OPTION,NUMBER,PICK,RCODE)
      IF(RCODE.NE.0)RETURN
      IF(PICK(2).AND.RGNODE.EQ.0)CALL GPHROW
      IF(PICK(3).AND.CGNODE.EQ.0)CALL GPHCOL
C NOW PICK IS SET TO DISPLAY TYPES: 1=BIGRAPH, 2=ROW GRAPH, 3=COL GRAPH
C
      LINE = 0
      IF(PICK(1))CALL GRDISP('BIGRAPH',FORMAT,FORM2,LINE,RCODE,*9000)
      IF(PICK(2))CALL GRDISP('ROW ',FORMAT,FORM2,LINE,RCODE,*9000)
      IF(PICK(3))CALL GRDISP('COL ',FORMAT,FORM2,LINE,RCODE,*9000)
C
9000  RETURN
C
C ** GPHDSP ENDS HERE
      END
      SUBROUTINE GRDISP(TYPE,FORMAT,FORM2,LINE,RCODE,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
      INCLUDE 'DCANAL.'
      INCLUDE 'DCGRAPH.'
CITSO      INCLUDE (DCFLIP)
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCGRAPH)
CI$$INSERT DCFLIP
CI$$INSERT DCANAL
CI$$INSERT DCGRAPH
C
C This displays graph (to OUTPUT).
C ...Alternate return is for user abort.
C
      CHARACTER*(*) TYPE,FORMAT,FORM2
C
C LOCAL
      CHARACTER*128 CLIST
      CHARACTER*8   STR8
      CHARACTER*1   CHAR
      PARAMETER (MARGIN=1,INDENT=0)
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF(RCODE.NE.0)RETURN
      IF(.NOT.SWRULE)THEN
         CLIST = TYPE
         IF(TYPE.NE.'BIGRAPH')CLIST(5:)='DIGRAPH'
         CALL FCENTR(CLIST,FIRST,LAST)
         CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
      ENDIF
C
      IF(FORMAT.EQ.'DIAGRAM')GOTO 1000
      IF(FORMAT.EQ.'SYNTAX') GOTO 5000
C
C DISPLAY ADJACENCY MATRIX
C
      IF(TYPE.EQ.'BIGRAPH')THEN
         CLIST = 'USE PICTURE TO SEE ADJACENCY MATRIX OF BIGRAPH'
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
      ELSE IF(TYPE.EQ.'ROW ')THEN
         CLIST = ' '
         LAST = NAMELN+4
         STR8 = ' '
         DO 200 I=1,RGNODE
            STR8(7:) = '00'
            CALL FI2C(STR8,I,FIRST)
            CLIST(LAST:) = STR8(7:8)
            LAST = LAST+3
200      CONTINUE
         END = LAST
         IF(END.GT.SCRWTH)THEN
           PRINT *,' SCREEN WIDTH TOO SMALL FOR DISPLAY...MUST =',END
            RETURN
         ENDIF
         CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
C
         DO 250 I=1,RGNODE
            CLIST = ' '
            STR8(7:) = '00'
            CALL FI2C(STR8,I,FIRST)
            CLIST = STR8(7:8)
            ROW = ROWNOD(I)
            CALL GETNAM('ROW ',ROW,CLIST(4:))
C
            LAST = NAMELN + 4
            DO 150 I1=1,RGNODE
               CLIST(LAST:) = RGMAT(I,I1)
               LAST = LAST+3
150         CONTINUE
            CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
250      CONTINUE
C
      ELSE IF(TYPE.EQ.'COL ')THEN
         CLIST = ' '
         LAST = NAMELN+4
         STR8 = ' '
         DO 300 I=1,CGNODE
            STR8(7:) = '00'
            CALL FI2C(STR8,I,FIRST)
            CLIST(LAST:) = STR8(7:8)
            LAST = LAST+3
300      CONTINUE
         END = LAST
         IF(END.GT.SCRWTH)THEN
           PRINT *,' SCREEN WIDTH TOO SMALL FOR DISPLAY...MUST =',END
            RETURN
         ENDIF
         CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
C
         DO 450 I=1,CGNODE
            CLIST = ' '
            STR8(7:) = '00'
            CALL FI2C(STR8,I,FIRST)
            CLIST = STR8(7:8)
            COL = COLNOD(I)
            CALL GETNAM('COL ',COL,CLIST(4:))
C
            LAST = NAMELN + 4
            DO 350 I1=1,CGNODE
               CLIST(LAST:) = CGMAT(I,I1)
               LAST = LAST+3
350         CONTINUE
            CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
450      CONTINUE
      ENDIF
C
      RETURN
C
1000  CONTINUE
C
C          FORMAT = DIAGRAM [GRAPHIC | PRINTABL]
C                            ::::::::::::::::::
C                                  FORM2
C
      IF(TYPE.EQ.'BIGRAPH')THEN
C FUNDAMENTAL DIGRAPH
         PRINT *,' BIGRAPH DIAGRAM DISPLAY NOT READY'
         RETURN
      ENDIF
      IF(TYPE.EQ.'ROW ')THEN
C ROW DIGRAPH
         DO 2500 I=1,RGNODE
            ROW = ROWNOD(I)
C FORM LIST OF SUCCESSORS AND PREDECESSORS
C ...USING SCRIND...SUCC GROWS FROM TOP-DOWN;  PRED GROWS BOTTOM-UP
            CHAR = RGMAT(I,I)
            IF(CHAR.EQ.'+' .OR. CHAR.EQ.'*')THEN
C   HEADLESS OUT-ARC
               NSUCC = 1
               SCRIND(1) = 0
            ELSE
               NSUCC = 0
            ENDIF
            IF(CHAR.EQ.'-' .OR. CHAR.EQ.'*')THEN
C    TAIL-LESS IN-ARC
               NPRED = 1
               SCRIND(PMXROW-1) = 0
            ELSE
               NPRED = 0
            ENDIF
C LOOP OVER OTHER NODES FOR PREDS AND SUCCS
            DO 2100 I1=1,RGNODE
               IF(RGMAT(I,I1).EQ.'1')THEN
C ..NODE I1 IS SUCCESSOR OF ROW
                  NSUCC = NSUCC+1
                  SCRIND(NSUCC) = ROWNOD(I1)
               ENDIF
               IF(RGMAT(I1,I).EQ.'1')THEN
C ...NODE I1 IS PREDECESSOR OF ROW
                  NPRED = NPRED+1
                  SCRIND(PMXROW - NPRED) = ROWNOD(I1)
               ENDIF
2100        CONTINUE
            CALL GRDSP1(TYPE,ROW,NPRED,NSUCC,FORM2,LINE,*9000)
C NEXT ROW
2500     CONTINUE
      ELSE IF(TYPE.EQ.'COL ')THEN
C COLUMN DIGRAPH
         DO 3500 I=1,CGNODE
            COL = COLNOD(I)
            CHAR = CGMAT(I,I)
            IF(CHAR.EQ.'+' .OR. CHAR.EQ.'*')THEN
C   HEADLESS OUT-ARC
               NSUCC = 1
               SCRIND(1) = 0
            ELSE
               NSUCC = 0
            ENDIF
            IF(CHAR.EQ.'-' .OR. CHAR.EQ.'*')THEN
C    TAIL-LESS IN-ARC
               NPRED = 1
               SCRIND(PMXROW-1) = 0
            ELSE
               NPRED = 0
            ENDIF
C LOOP OVER OTHER NODES FOR PREDS AND SUCCS
            DO 3100 I1=1,CGNODE
               IF(CGMAT(I,I1).EQ.'1')THEN
C ..NODE I1 IS SUCCESSOR OF COL
                  NSUCC = NSUCC+1
                  SCRIND(NSUCC) = COLNOD(I1)
               ENDIF
               IF(CGMAT(I1,I).EQ.'1')THEN
C ...NODE I1 IS PREDECESSOR OF COL
                  NPRED = NPRED+1
                  SCRIND(PMXROW - NPRED) = COLNOD(I1)
               ENDIF
3100        CONTINUE
            CALL GRDSP1(TYPE,COL,NPRED,NSUCC,FORM2,LINE,*9000)
C NEXT COLUMN
3500     CONTINUE
      ENDIF
C
      RETURN
C
5000  CONTINUE
C          FORMAT = SYNTAX [HEADER | NOHEADER]
C                           :::::::::::::::::
C                                 FORM2
C
      IF(.NOT.SWEXPL)THEN
         PRINT *,' NO SYNTAX HAS BEEN READIN'
         RCODE = 1
         RETURN
      ENDIF
      CALL GPHSYN(TYPE,FORM2,LINE,RCODE,*9000)
      RETURN
C
9000  RETURN 1
C
C ** GRDISP ENDS HERE
      END
      SUBROUTINE GRDSP1(TYPE,NUMBER,NPRED,NSUCC,FORMAT,LINE,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
      INCLUDE 'DCANAL.'
      INCLUDE 'DCGRAPH.'
CITSO      INCLUDE (DCFLIP)
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCGRAPH)
CI$$INSERT DCFLIP
CI$$INSERT DCANAL
CI$$INSERT DCGRAPH
C
C This displays successors and predecessors of one node (TYPE NUMBER)
C    TYPE is ROW or COL
C    NPRED = Number of predecessors (stored in SCRIND(PMXROW-1),...)
C    NSUCC = Number of successors (stored in SCRIND(1),....)
C    FORMAT = DIAGRAM | GRAPHIC | PRINTABL   (DIAGRAM same as GRAPHIC)
C ...Alternate return is for user abort.
C
      CHARACTER*(*) TYPE, FORMAT
C
C LOCAL
      CHARACTER*80 CLIST
      PARAMETER (MARGIN=1,INDENT=0,ARRLEN=8)
C               ..........................: LENGTH OF ARROW
C               :
      CHARACTER*8 ARROW0(2),ARRIN1(2),ARRIN2(2),ARRINL(2),
     1            ARROT1(2),ARROT2(2),ARROTL(2)
      CHARACTER*1 SWCORN(2),SECORN(2),NWCORN(2),NECORN(2),
     1            HORLIN(2),VERLIN(2)
C
C                           GRAPHIC    PRINTABL
C                           12345678   12345678
CLAHEY      DATA   ARROW0 /'ÄÄÄÄÄÄÄ','------->'/
CLAHEY      DATA   ARRIN1 /'ÄÄÄÄÂÄÄ','---->.--'/
CLAHEY      DATA   ARRIN2 /'ÄÄÄÄ´  ','---->:  '/
CLAHEY      DATA   ARRINL /'ÄÄÄÄÙ  ','---->''' /
CLAHEY      DATA   ARROT1 /'ÄÄÂÄÄÄÄ','--.---->'/
CLAHEY      DATA   ARROT2 /'  ÃÄÄÄÄ','  :---->'/
CLAHEY      DATA   ARROTL /'  ÀÄÄÄÄ','  `---->'/
C
      DATA   ARROW0 /'ÄÄÄÄÄÄÄ>','------->'/
      DATA   ARRIN1 /'ÄÄÄÄ>ÂÄÄ','---->.--'/
      DATA   ARRIN2 /'ÄÄÄÄ>´  ','---->:  '/
      DATA   ARRINL /'ÄÄÄÄ>Ù  ','---->''' /
      DATA   ARROT1 /'ÄÄÂÄÄÄÄ>','--.---->'/
      DATA   ARROT2 /'  ÃÄÄÄÄ>','  :---->'/
      DATA   ARROTL /'  ÀÄÄÄÄ>','  `---->'/
C
      DATA   NWCORN /'Ú' , '.'/
      DATA   NECORN /'¿' , '.'/
      DATA   SECORN /'Ù' , ''''/
      DATA   SWCORN /'À' , '`'/
      DATA   HORLIN /'Ä' , '-'/
      DATA   VERLIN /'³' , '|'/
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
C SET ARROW TYPES (GRAPHIC OR PRINTABL)
      IF(FORMAT.EQ.'PRINTABL')THEN
         ARRTYP = 2
      ELSE
         ARRTYP = 1
      ENDIF
C
C                     <--n-->    <--n+2-->    <--n-->
C SET TABS:            pred  ÄÄ->| node  |ÄÄ-> succ
C                                :       :...OUT
C                                :...IN
C   (n=NAMELN)
      IN  = NAMELN + ARRLEN + 1
      OUT = IN + NAMELN + 1
      CLIST = ' '
C TOP OF BOX
      CLIST(IN:)  = NWCORN(ARRTYP)
      CLIST(OUT:) = NECORN(ARRTYP)
      DO 50 K=IN+1,OUT-1
50    CLIST(K:K) = HORLIN(ARRTYP)
      LAST = OUT
      CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
C
      IF(NPRED.GT.0)THEN
C SET 1ST PRED
         PRED = SCRIND(PMXROW-1)
         IF(PRED.GT.0)CALL GETNAM(TYPE,PRED,CLIST)
         IF(NPRED.EQ.1)THEN
            CLIST(IN-ARRLEN:IN-1) = ARROW0(ARRTYP)
         ELSE
            CLIST(IN-ARRLEN:IN-1) = ARRIN1(ARRTYP)
         ENDIF
      ENDIF
      P = 2
C GET NAME OF NODE (TYPE NUMBER)
      CALL GETNAM(TYPE,NUMBER,CLIST(IN+1:))
      CLIST(IN:IN)   = VERLIN(ARRTYP)
      CLIST(OUT:OUT) = VERLIN(ARRTYP)
C
      IF(NSUCC.GT.0)THEN
C SET 1ST SUCC
         IF(NSUCC.EQ.1)THEN
            CLIST(OUT+1:) = ARROW0(ARRTYP)
         ELSE
            CLIST(OUT+1:) = ARROT1(ARRTYP)
         ENDIF
         SUCC = SCRIND(1)
         IF(SUCC.GT.0)CALL GETNAM(TYPE,SUCC,CLIST(OUT+ARRLEN+1:))
      ENDIF
      S = 2
C
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
C DRAW BOTTOM OF BOX
      CLIST(IN:)  = SWCORN(ARRTYP)
      CLIST(OUT:) = SECORN(ARRTYP)
      DO 75 K=IN+1,OUT-1
75    CLIST(K:K) = HORLIN(ARRTYP)
      IF(NPRED.LE.1 .AND. NSUCC.LE.1)THEN
         CALL FTEXT(CLIST,OUT,MARGIN,INDENT,LINE,'CLEAR ',*9000)
         RETURN
      ENDIF
C
C LOOP OVER REMAINING PREDS/SUCCS
100   CONTINUE
      IF(P.GT.NPRED .AND. S.GT.NSUCC)RETURN
      IF(P.LE.NPRED)THEN
         PRED = SCRIND(PMXROW-P)
         CALL GETNAM(TYPE,PRED,CLIST(1:IN-ARRLEN-1))
         IF(P.EQ.NPRED)THEN
            CLIST(IN-ARRLEN:IN-1) = ARRINL(ARRTYP)
         ELSE
            CLIST(IN-ARRLEN:IN-1) = ARRIN2(ARRTYP)
         ENDIF
         P = P+1
      ENDIF
      IF(S.LE.NSUCC)THEN
         IF(S.EQ.NSUCC)THEN
            CLIST(OUT+1:) = ARROTL(ARRTYP)
         ELSE
            CLIST(OUT+1:) = ARROT2(ARRTYP)
         ENDIF
         SUCC = SCRIND(S)
         CALL GETNAM(TYPE,SUCC,CLIST(OUT+ARRLEN+1:))
         S = S+1
      ENDIF
C
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
      CLIST = ' '
      GOTO 100
C
9000  RETURN 1
C
C ** GRDSP1 ENDS HERE
      END
      SUBROUTINE GRPHIO(WHAT,CLIST,FIRST,LAST,RCODE)
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
C This saves|loads graph (unformatted).
C       Syntax:  {SAVE | LOAD} [filespec]
C
C The filespec is the GRAPH type (in _SETUP), and the default filename
C is the problem name (so we may overwrite previously saved graph).
C
      CHARACTER*(*) WHAT
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*64  FILNAM
      CHARACTER*1   CHAR
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
C GET FILE NAME
      CALL FTOKEN(CLIST,FIRST,LAST,FILNAM,64,CHAR)
      IF(FILNAM.EQ.' ') FILNAM = PRBNAM
      CALL FSETNM('GRAPH ',FILNAM,BEGEXT,RCODE)
      IF(RCODE.NE.0)RETURN
C
      CALL FLOPEN(PCKFIL,FILNAM,'UNFORMATTED','UNKNOWN',*1313)
C
      IF(WHAT.EQ.'SAVE ')THEN
         WRITE(PCKFIL,ERR=1300)BGROWS,BGCOLS,RGNODE,CGNODE
         WRITE(PCKFIL,ERR=1300)(ROWNOD(I),I=1,BGROWS),
     1                         (COLNOD(J),J=1,BGCOLS)
         WRITE(PCKFIL,ERR=1300)( (BGMAT(I,J), J=1,BGCOLS), I=1,BGROWS)
         IF(RGNODE.GT.0)WRITE(PCKFIL,ERR=1300)
     1                         ( (RGMAT(I,J), I=1,RGNODE-1), J=I,RGNODE)
         IF(CGNODE.GT.0)WRITE(PCKFIL,ERR=1300)
     1                         ( (CGMAT(I,J), I=1,CGNODE-1), J=I,CGNODE)
      ELSE
         BGR = BGROWS
         BGC = BGCOLS
         RGN = RGNODE
         CGN = CGNODE
         READ(PCKFIL,ERR=1300)BGROWS,BGCOLS,RGNODE,CGNODE
         IF(BGROWS.GT.PBGROW .OR. BGCOLS.GT.PBGCOL)THEN
            PRINT *,' ** DIMENSION DISCREPENCY...GRAPH FILE TOO LARGE'
C RESTORE RESIDENT GRAPH NODE COUNTS
            BGROWS = BGR
            BGCOLS = BGC
            RGNODE = RGN
            CGNODE = CGN
            GOTO 1313
         ENDIF
C OK, READ REST OF GRAPH INFO
         READ(PCKFIL,ERR=1300)(ROWNOD(I),I=1,BGROWS),
     1                         (COLNOD(J),J=1,BGCOLS)
         READ(PCKFIL,ERR=1300)( (BGMAT(I,J), J=1,BGCOLS), I=1,BGROWS)
         IF(RGNODE.GT.0)READ(PCKFIL,ERR=1300)
     1                        ( (RGMAT(I,J),I=1,RGNODE-1), J=I,RGNODE)
         IF(CGNODE.GT.0)READ(PCKFIL,ERR=1300)
     1                        ( (CGMAT(I,J), I=1,CGNODE-1), J=I,CGNODE)
      ENDIF
C
      CLOSE(PCKFIL)
      RETURN
C
1300  CONTINUE
      PRINT *,' ** ',WHAT(1:4),' IO ERROR WITH ',FILNAM,
     1        '...UNIT=',PCKFIL
      IF(WHAT(:4).EQ.'LOAD')THEN
         IF(SWDBG)CALL SYSDBG
         PRINT *,' ...NO GRAPH IS SET'
         CALL GRAPH0
      ENDIF
1313  CLOSE(PCKFIL)
      RCODE = 1
      RETURN
C
C ** GRPHIO ENDS HERE
      END
