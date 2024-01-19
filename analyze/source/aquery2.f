C               ::: AQUERY2.FOR  7-31-95 :::
C
C Earlier dates deleted
C    5-12-94...Fixed bug in AQLTRW when NAMELN > 12 (CC 7-29-95)
C    7-29-95...Moved ATALLY to AQUERY1.FOR
C
C This contains the following subroutines for ANALYZE
C
C    ACOUNT.....counts rows, cols, nonzeroes in submatrix
C    AQLIST.....executes the LIST command
C    AQEQN......lists equations in submatrix
C    ALSTRC.....lists nonzeroes in submatrix by row or column
C    AQLTAB.....lists submatrix in TABLEAU form (ADDED 7-25-94)
C    AQLTRW.....lists 1 row in tableau (ADDED 7-25-94)
C    APICTR.....pictures nonzeroes in submatrix
C
      SUBROUTINE ACOUNT(CLIST,FIRST,LAST,FILOUT,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This counts nonzeroes, parsing CLIST(FIRST:LAST)
C
C ...RCODE = 10 if there are either no rows or no columns
C               and FILOUT = 0
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME,CNAME
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C FIRST GET ROW CONDITIONAL...absence means use current setting
      CALL ACONDN(CLIST,FIRST,LAST,'ROW ',RNAME,RCNUM,RCODE)
      IF(RCODE.NE.0)RETURN
C
      IF(RNAME.NE.' ')THEN
         CALL ASBNEW('ROW ',RNAME,RCNUM)
      ELSE IF(FILOUT.EQ.0)THEN
         IF(NRCSUB(1).EQ.0)THEN
            IF(SWMSG)PRINT *,' There are no rows in the submatrix'
            RETURN
         ENDIF
      ENDIF
C
C NOW THE COLUMN CONDITIONAL
C
      CALL ACONDN(CLIST,FIRST,LAST,'COL ',CNAME,RCNUM,RCODE)
      IF(RCODE.NE.0)RETURN
C
      IF(CNAME.NE.' ')THEN
         CALL ASBNEW('COL ',CNAME,RCNUM)
      ELSE IF(FILOUT.EQ.0)THEN
         IF(NRCSUB(2).EQ.0)THEN
            IF(SWMSG)PRINT *,' There are no columns in the submatrix'
            RETURN
         ENDIF
      ENDIF
C
      IF(.NOT.SWSBNZ)THEN
C TALLY NONZEROES OF SUBMATRIX AND REMOVE NULL ROWS AND COLUMNS
         CALL GMAPNZ(NRCSUB(1),RCSUB1(1),NRCSUB(2),RCSUB1(2),NZSUB)
         SWSBNZ = .TRUE.
      ENDIF
C
      IF(FILOUT.EQ.0)RETURN
C    (FILOUT = 0 means caller just wanted conditional parsed)
C
C WRITE COUNT TO OUTPUT
C
      CLIST = ' Submatrix has'
      RNAME = ' '
      CALL FI2C(RNAME,NRCSUB(1),FIRST)
      CLIST(17:) = RNAME(FIRST:8)//' row'
      CALL FSLEN(CLIST,30,LAST)
      IF(NRCSUB(1).NE.1)THEN
         LAST = LAST+1
         CLIST(LAST:)='s'
      ENDIF
      CALL FI2C(RNAME,NRCSUB(2),FIRST)
      CLIST(LAST+1:) = ', '//RNAME(FIRST:8)//' column'
      CALL FSLEN(CLIST,50,LAST)
      IF(NRCSUB(2).NE.1)THEN
         LAST = LAST+1
         CLIST(LAST:)='s'
      ENDIF
      CALL FI2C(RNAME,NZSUB,FIRST)
      CLIST(LAST+1:) = ', and '//RNAME(FIRST:8)//' nonzero'
      CALL FSLEN(CLIST,64,LAST)
      IF(NZSUB.NE.1)THEN
         CLIST(LAST+1:)='es'
         LAST = LAST+2
      ENDIF
      LINE = 1
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
900   RETURN
C
C ** ACOUNT ENDS HERE
      END
      SUBROUTINE AQLIST(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCFLIP)
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCFLIP
CI$$INSERT DCANAL
C
C This executes the LIST command.
C    Syntax:  LIST [row condnl[,column condnl]] [//option]
C      options: {COLUMN | ROW | EQUATION | DUAL | TABLEAU [options]}
C         TABLEAU options: {OBJ={FIRST | LAST}, WIDTH=number}
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*4   ROWCOL
      CHARACTER*1   CHAR
      CHARACTER*8   OPTION(5)
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      LINE = 1
C SET OPTIONS
      OPTION(1) = 'COLUMN  '
      OPTION(2) = 'ROW     '
      OPTION(3) = 'EQUATION'
      OPTION(4) = 'DUAL    '
      OPTION(5) = 'TABLEAU '
C SEE IF PARAMETER SPECIFIED
      CALL FLOOK(CLIST,FIRST,LAST,'//',IPARM)
      IF( IPARM.EQ.0 )THEN
C NO OPTION...SET DEFAULT = EQUATION LISTING
         NUMBER = 3
      ELSE
C PARSE OPTION
         F = IPARM+2
         NUMBER = 5
         RCODE = 1
         CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
         IF( RCODE.NE.0 )RETURN
         IF( NUMBER.EQ.5 )THEN
C //TABLEAU ...PARSE ITS OPTIONS
C   SET DEFAULTS
              LOCOBJ = 0
              CWIDTH = 12
C                      == WILL INCREASE TO NAMELN, IF NECESSARY
500         CONTINUE
C   SET OPTIONS
              OPTION(1) = 'OBJ '
              OPTION(2) = 'WIDTH'
              N = 2
              CALL FOPTN(CLIST,F,LAST,OPTION,N,RCODE)
              IF( RCODE.NE.0 )RETURN
              IF( N.EQ.1 )THEN
C     OBJ = {FIRST | LAST}
                 OPTION(1) = 'FIRST'
                 OPTION(2) = 'LAST '
                 LOCOBJ = 2
                 RCODE = 1
                 CALL FOPTN(CLIST,F,LAST,OPTION,LOCOBJ,RCODE)
                 IF( RCODE.NE.0 )RETURN
                 GOTO 500
              ENDIF
              IF( N.EQ.2 )THEN
C     WIDTH = number
                 CALL FVRNG(CLIST,F,LAST,VL,VU,VINF,CHAR,RCODE)
                 IF( RCODE.NE.0 )RETURN
                 IF( NAMELN.LT.12 )THEN
                    MAX = 12
                 ELSE
                    MAX = NAMELN
                 ENDIF
                 IF( VL.GT.MAX .OR. VL.LT.1. )THEN
                    PRINT *,' ** WIDTH MUST BE 1 TO',MAX
                    RCODE = 1
                    RETURN
                 ENDIF
                 CWIDTH = VL+.1
                 GOTO 500
              ENDIF
         ENDIF
C   SET LAST FOR PARSING CONDITIONALS
         LAST = IPARM-1
      ENDIF
C === END OPTION PARSING ... OPTION(NUMBER) CHOSEN ===
C SET SUBMATRIX...I.E., PARSE CONDITIONALS, IF ANY
      CALL ACOUNT(CLIST,FIRST,LAST,0,RCODE)
      IF( RCODE.NE.0 .OR. NZSUB.EQ.0 )RETURN
C
      IF(NUMBER.LT.3)THEN
C //COLUMN | ROW
         ROWCOL = OPTION(NUMBER)(1:3)//' '
         CALL ALSTRC(ROWCOL,LINE)
      ELSE IF( NUMBER.LT.5 )THEN
C //EQUATION | DUAL
         CALL AQEQN(OPTION(NUMBER),LINE,RCODE)
      ELSE
C //TABLEAU
         CALL AQLTAB(LINE,LOCOBJ,CWIDTH,RCODE)
      ENDIF
C
9000  RETURN
C
C ** AQLIST ENDS HERE
      END
      SUBROUTINE AQEQN(OPTION,LINE,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCFLIP)
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCFLIP
CI$$INSERT DCANAL
C
C This lists the equations in the submatrix to OUTPUT (calling FTEXT).
C ...DCFLIP is needed for SCRWTH to control tabs.
C ...RCODE = 0 iff all is well
C
      CHARACTER*(*) OPTION
C                   :....EQUATION | DUAL
C LOCAL
      CHARACTER*16  RNAME,CNAME
      CHARACTER*128 CLIST
      LOGICAL*1     SW,SW1
C
      CHARACTER*1   ROWTYP,TIMES
      CHARACTER*4   ROWCOL,OTHER
      CHARACTER*8   STRVAL
      CHARACTER*12  STR12
      PARAMETER (PMXEQN=PMXROW)
C                :....IF THIS IS INCREASED, ALL ANALSUBS MUST BE
C                     RECOMPILED BECAUSE COMMON/ALIST/ WILL CHANGE
      DIMENSION     JLIST(PMXEQN)
      EQUIVALENCE  (JLIST(1),SCRIND(1))
      DATA TIMES/' '/
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      IF(OPTION.EQ.'EQUATION')THEN
         RC = 1
         NUMRC = NROWS
         ROWCOL = 'ROW '
         OTHER  = 'COL '
      ELSE
         RC = 2
         NUMRC = NCOLS
         ROWCOL = 'COL '
         OTHER  = 'ROW '
      ENDIF
C INITIALIZE NUMBER OF EQUATIONS REMAINING TO BE LISTED
      REMAIN = NRCSUB(RC)
      IF(REMAIN.EQ.0)RETURN
C
C    ::: LOOP OVER ROWS | COLUMNS :::
C
      DO 1000 NUMBER=RCSUB1(RC),NUMRC
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF(.NOT.SW)GOTO 1000
         REMAIN = REMAIN-1
C ROWCOL IS IN SUBMATRIX
         CALL GETNAM(ROWCOL,NUMBER,RNAME)
C ...GET ITS NONZEROES
         IF(ROWCOL.EQ.'ROW ')THEN
            CALL GETROW(NUMBER,JLIST,VALLST,PMXEQN,NZ,RCODE)
            IF(RCODE.NE.0)THEN
               CLIST = '** ROW '//RNAME(:NAMELN)//
     1                ' IS TOO DENSE TO SHOW IN FULL'
               CALL FSLEN(CLIST,80,LAST)
               CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
               RCODE = 0
            ENDIF
C ...GET ROW'S BOUNDS
            CALL GETBND(ROWCOL,NUMBER,VL,VU)
         ELSE
            CALL GETCOL(NUMBER,N,MAXLST,JLIST,VALLST)
C REMOVE ROWS NOT IN SUBMATRIX
            NZ = 0
            V = 0.
            DO 150 I=1,N
               ROW = JLIST(I)
               CALL GETMAP(OTHER,ROW,SW)
               IF(SW)THEN
                  IF(ROW.EQ.OBJNUM)THEN
                     IF(OPTNAM.EQ.'MINIMIZE')THEN
                        V = VALLST(I)
                     ELSE
                        V = -VALLST(I)
                     ENDIF
                  ELSE
C ADD TO NONZERO LIST (JLIST)
                     NZ = NZ+1
                     JLIST(NZ) = ROW
C    NEGATE SIGN
                     VALLST(NZ) = -VALLST(I)
                  ENDIF
               ENDIF
150         CONTINUE
            IF(V.NE.0.)THEN
               NZ = NZ+1
               VALLST(NZ) = V
               JLIST(NZ) = 0
            ENDIF
         ENDIF
C
         IF(NZ.EQ.0)GOTO 900
         CLIST = ' '
C
C NOW LIST EQUATION ....CHANGED FR2C TO FR2C12, 2-6-94
         IF(ROWCOL.EQ.'ROW ')THEN
C PRIMAL...FIRST b [>,<]= ...
            CALL GETYPE(NUMBER,ROWTYP)
            IF(ROWTYP.EQ.'E')THEN
               CALL FR2C12(CLIST(1:12),VL)
               CLIST(14:14) = '='
            ELSE IF(ROWTYP.EQ.'G'.OR.ROWTYP.EQ.'R')THEN
               CALL FR2C12(CLIST(1:12),VL)
               CLIST(13:14) = '<='
            ELSE IF(ROWTYP.EQ.'L')THEN
               CALL FR2C12(CLIST(1:12),VU)
               CLIST(13:14) = '>='
            ELSE IF(ROWTYP.EQ.'N')THEN
               IF(RNAME.EQ.OBJNAM)THEN
                  CLIST(5:7) = OPTNAM(:3)
               ELSE
                  CLIST(3:6) = 'Free'
               ENDIF
            ENDIF
            FIRST = 16
         ELSE
C ...DUAL
            FIRST = 1
         ENDIF
C FIRST LINE
         CLIST(FIRST:) = RNAME(:NAMELN)//'='
         CALL FSLEN(CLIST,40,KPOINT)
         TAB = KPOINT
         IF(JLIST(NZ).EQ.0)THEN
C PUT COST VALUE IN DUAL EQUATION
            CALL FR2C(CLIST(KPOINT+1:),VALLST(NZ))
            KPOINT = KPOINT + 9
            NZ = NZ-1
            SW1 = .TRUE.
         ELSE
            SW1 = .FALSE.
         ENDIF
C
C        ::: LOOP OVER NONZEROES OF ROWCOL IN SUBMATRIX :::
C
C  ...FIRST THE UNITY COEFFICIENTS
C
         FLDWTH = NAMELN+3
C
         DO 600 K=1,NZ
C ENTER SIGN
            IF(VALLST(K).EQ.-1.)THEN
               CLIST(KPOINT+1:) = '-'
            ELSE IF(VALLST(K).EQ.1.)THEN
               IF(SW1)CLIST(KPOINT+1:) = '+'
            ELSE
               GOTO 600
            ENDIF
            SW1 = .TRUE.
C COEF=1...LIST TERM
            TERM = JLIST(K)
            CALL GETNAM(OTHER,TERM,CNAME)
C ...ENTER [DUAL] ACTIVITY'S NAME
            CLIST(KPOINT+3:) = CNAME
            CALL FSLEN(CLIST,SCRWTH,KPOINT)
            IF(KPOINT+FLDWTH.GE.SCRWTH)THEN
C WRITE LINE
               CALL FTEXT(CLIST,KPOINT,1,0,LINE,'CLEAR ',*9000)
               KPOINT = TAB
            ELSE
               KPOINT = KPOINT+1
            ENDIF
C NEXT NONZERO
600      CONTINUE
C  ::: END UNITY LOOP :::
C
C NOW THE GENERAL COEFFICIENTS
C
         FLDWTH = NAMELN+12
C
         DO 700 K=1,NZ
C CHECK BUFFER
            IF(KPOINT+FLDWTH.GE.SCRWTH)THEN
C WRITE LINE
               CALL FTEXT(CLIST,KPOINT,1,0,LINE,'CLEAR ',*9000)
               KPOINT = TAB
            ENDIF
C ...ENTER SIGN
            IF(VALLST(K).LT.0.)THEN
               CLIST(KPOINT+1:) = '-'
               V = -VALLST(K)
            ELSE
               V = VALLST(K)
               IF(SW1)CLIST(KPOINT+1:) = '+'
            ENDIF
            IF(V.EQ.1.)THEN
               CLIST(KPOINT+1:) = ' '
               GOTO 700
            ENDIF
            SW1 = .TRUE.
C GENERAL COEFFICIENT (V)
            CALL FR2C(STRVAL,V)
            CLIST(KPOINT+3:) = STRVAL
C ...ENTER ACTIVITY'S NAME
            TERM = JLIST(K)
            CALL GETNAM(OTHER,TERM,CNAME)
            CALL FSLEN(CLIST,SCRWTH,KPOINT)
            CLIST(KPOINT+1:) = TIMES//CNAME
            CALL FSLEN(CLIST,SCRWTH,KPOINT)
            KPOINT = KPOINT+1
C NEXT NONZERO
700      CONTINUE
C  ::: END LOOP FOR GENERAL COEFFICIENTS :::
C
C      ::: END LOOP OVER NONZEROES :::
C
         IF(ROWCOL.EQ.'ROW ')THEN
C FINALLY, ... <= VU (IF RANGE)
            IF(ROWTYP.EQ.'R')THEN
               CALL FR2CLJ(STR12,VU,LAST)
               CALL FSLEN(CLIST,128,KPOINT)
               KPOINT = KPOINT+2
               IF(KPOINT+LAST+4.GT.SCRWTH)THEN
                  CALL FTEXT(CLIST,KPOINT,1,0,LINE,'CLEAR ',*9000)
                  KPOINT = 1
               ELSE IF(KPOINT.LE.TAB)THEN
                  KPOINT = 9
               ENDIF
               CLIST(KPOINT:) = '<= '//STR12
            ENDIF
         ENDIF
         CALL FSLEN(CLIST,128,KPOINT)
         IF(KPOINT.GT.0)
     1      CALL FTEXT(CLIST,KPOINT,1,0,LINE,'CLEAR ',*9000)
C NEXT ROWCOL
900   CONTINUE
C ANY REMAIN?
         IF(REMAIN.LE.0)RETURN
C -YES, SO LOOP TO NEXT ROWCOL
1000  CONTINUE
C     ::: END LOOP OVER ROWCOLS :::
C
9000  RETURN
C
C ** AQEQN ENDS HERE
      END
      SUBROUTINE ALSTRC(ROWCOL,LINE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This lists the equations in the submatrix to OUTPUT (calling FTEXT).
C ...by (ROWCOL)
C
      CHARACTER*4   ROWCOL
C LOCAL
      CHARACTER*128 CLIST
      LOGICAL*1     SW
      CHARACTER*4   COLROW
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      IF(ROWCOL.EQ.'ROW ')THEN
         COLROW = 'COL '
         RC1    = RCSUB1(1)
         RCLAST = NROWS
         NRC    = NRCSUB(1)
      ELSE
         COLROW = 'ROW '
         RC1    = RCSUB1(2)
         RCLAST = NCOLS
         NRC    = NRCSUB(2)
      ENDIF
C
      TAB1 = NAMELN+2
      TAB2 = TAB1 + NAMELN+2
      LAST = TAB2 + 13
C
C    ::: LOOP OVER ROWS | COLUMNS :::
C
      DO 1000 NUMBER=RC1,RCLAST
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF(.NOT.SW)GOTO 1000
         NRC = NRC - 1
C ROW/COL IS IN SUBMATRIX
         CALL GETNAM(ROWCOL,NUMBER,CLIST)
C ...GET ITS NONZEROES
         IF(ROWCOL.EQ.'ROW ')THEN
            CALL GETROW(NUMBER,ROWLST,VALLST,MAXLST,NZ,RCODE)
         ELSE
            CALL GETCOL(NUMBER,NZ,MAXLST,ROWLST,VALLST)
            IF(NZ.GT.MAXLST)THEN
               NZ = MAXLST
               RCODE = 1
            ENDIF
         ENDIF
         IF(NZ.EQ.0)GOTO 900
C
C       ::: LOOP OVER NONZEROES :::
         DO 500 I=1,NZ
            CALL GETMAP(COLROW,ROWLST(I),SW)
            IF(.NOT.SW)GOTO 500
            CALL GETNAM(COLROW,ROWLST(I),CLIST(TAB1:))
            CALL FR2C12(CLIST(TAB2:),VALLST(I))
            L = LAST
            CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*9000)
            CLIST = ' '
500      CONTINUE
C
         IF(RCODE.NE.0)THEN
            CLIST = '** '//ROWCOL//' IS TOO DENSE TO SHOW IN FULL'
            CALL FSLEN(CLIST,80,L)
            CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*9000)
            RCODE = 0
         ENDIF
C
900      CONTINUE
C ANY REMAIN?
         IF(NRC.LE.0)RETURN
C -YES
1000  CONTINUE
C
C     ::: END MAIN LOOP OVER ROWS/COLS :::
C
9000  RETURN
C
C ** ALSTRC ENDS HERE
      END
      SUBROUTINE AQLTAB(LINE,LOCOBJ,CWIDTH,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCANAL
CI$$INSERT DCFLIP
C
C This lists submatrix in tableau form....DCFLIP needed for SCRWTH
C
C       LOCOBJ = 1 IF OBJ GOES FIRST
C              = 2 IF OBJ GOES LAST
C              = 0 IF OBJ GOES IN NAME SORT ORDER
C       CWIDTH = COLUMN WIDTH (<= 12, BUT INCREASES IF NAMELN > 12)
C
C LOCAL
      PARAMETER    (LCLMAX=20)
C                          :...MAX NUMBER OF COLUMNS IN SUBMATRIX
      INTEGER       LCLCOL(LCLMAX)
      LOGICAL*1     SW,SWRHS,SWOBJ
      CHARACTER*128 CLIST
      CHARACTER*16  UNDERL
      DATA UNDERL/'================'/
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      NC = NRCSUB(2)
      IF( NC.GT.LCLMAX )THEN
         PRINT *,NC,' ARE TOO MANY COLUMNS...MAX=',LCLMAX
         RCODE = 1
         RETURN
      ENDIF
      NR = NRCSUB(1)
      IF( CWIDTH.LT.NAMELN )CWIDTH=NAMELN
      TAB = NAMELN+2
      NEED = TAB + (CWIDTH+1)*NC
      IF( NEED.GT.SCRWTH )THEN
         PRINT *,' ** SCREEN WIDTH TOO SMALL...NEED',NEED
         RCODE = 1
         RETURN
      ENDIF
C PRINT COLUMN NAMES
      CLIST  = ' '
      CPOINT = TAB
      K = 0
      DO 200 J=RCSUB1(2),NCOLS
         CALL GETMAP('COL ',J,SW)
         IF( .NOT.SW )GOTO 200
         K = K+1
         CALL GETNAM('COL ',J,CLIST(CPOINT:))
         CPOINT = CPOINT + CWIDTH+1
         LCLCOL(K) = J
         IF( K.EQ.NC )GOTO 210
200   CONTINUE
210   CONTINUE
C LCLCOL(K)=J MEANS COLUMN J IS K-TH TABLEAU COLUMN
C
C IF ENOUGH ROOM, INCLUDE RHS
      SWRHS = (CPOINT+15.LT.SCRWTH)
C         :...= T IFF RHS INCLUDED
      IF( SWRHS )CLIST(CPOINT+3:)=RHSNAM
      LAST = SCRWTH-1
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C UNDERLINE
      CLIST = ' '
      CPOINT= TAB
      REMAIN = NC
      DO 300 J=RCSUB1(2),NCOLS
         CALL GETMAP('COL ',J,SW)
         IF( .NOT.SW )GOTO 300
         CLIST(CPOINT:) = UNDERL(:CWIDTH)
         CPOINT = CPOINT + CWIDTH+1
         IF( REMAIN.EQ.1 )GOTO 310
         REMAIN = REMAIN-1
300   CONTINUE
310   CONTINUE
      IF( SWRHS )CLIST(CPOINT+3:)=UNDERL(:CWIDTH)
      LAST = SCRWTH-1
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C COLUMN HEADS PRINTED...PREPARE TO LOOP OVER ROWS
      REMAIN = NR
      CALL GETMAP('ROW ',OBJNUM,SWOBJ)
      IF( SWOBJ .AND. LOCOBJ.EQ.1 )THEN
C OBJ IS IN SUBMATRIX AND GOES FIRST
         CALL AQLTRW(OBJNUM,NC,LCLCOL,TAB,SWRHS,CWIDTH,CLIST,RCODE)
         LAST = SCRWTH-1
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      ENDIF
      DO 900 I=RCSUB1(1),NROWS
         CALL GETMAP('ROW ',I,SW)
         IF( .NOT.SW )GOTO 900
         REMAIN = REMAIN-1
         IF( I.EQ.OBJNUM .AND. LOCOBJ.NE.0 )GOTO 900
         CALL AQLTRW(I,NC,LCLCOL,TAB,SWRHS,CWIDTH,CLIST,RCODE)
         LAST = SCRWTH-1
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
         IF( REMAIN.EQ.0 )GOTO 910
900   CONTINUE
910   CONTINUE
C
      IF( SWOBJ .AND. LOCOBJ.EQ.2 )THEN
C OBJ IS IN SUBMATRIX AND GOES LAST
         CALL AQLTRW(OBJNUM,NC,LCLCOL,TAB,SWRHS,CWIDTH,CLIST,RCODE)
         LAST = SCRWTH-1
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      ENDIF
C
9000  RETURN
C
C ** AQLTAB ENDS HERE
      END
      SUBROUTINE AQLTRW(ROW,NC,LCLCOL,TAB,SWRHS,CWIDTH,CLIST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This lists ROW in tableau form....caller responsible for total width.
C       NC = Number of columns in submatrix = Length of LCLCOL
C       LCLCOL(k)=j means column j is in k-th tableau position
C       TAB is where columns begin
C       SWRHS  = T means include RHS
C       CWIDTH = Column width (>= NAMELN)
C       CLIST is what we return
C       RODE = 0 IFF ALL IS WELL
C
      INTEGER       LCLCOL(NC)
      LOGICAL*1     SWRHS
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  CNAME
      CHARACTER*12  STR12
      CHARACTER*1   CHAR
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C INITIALIZE CLIST WITH ROW NAME
      CALL GETNAM('ROW ',ROW,CNAME)
      CLIST = CNAME
      CALL GETROW(ROW,ROWLST,VALLST,PMXROW,NZ,RCODE)
      IF( RCODE.NE.0 )RETURN
C
      IF( NZ.EQ.0 )GOTO 900
C LOOP OVER ROW'S NONZEROES (ALL NZ COLS ARE IN SUBMATRIX)
      DO 500 J=1,NZ
C   FIND TABLEAU POSITION
         DO 450 K=1,NC
            IF( LCLCOL(K).EQ.ROWLST(J) )GOTO 460
450      CONTINUE
C OH, OH...COULD NOT FIND SUBMATRIX COL
           PRINT *,' ** SYSERR IN AQLTRW...NZ,J',NZ,J,ROWLST(J)
           PRINT *,NC,' LCLCOL:',(LCLCOL(K),K=1,NC)
           RCODE = 13
           RETURN
460      CONTINUE
         CPOINT = TAB + (K-1)*(CWIDTH+1)
         CALL FR2CLJ(STR12,VALLST(J),LAST)
CC 7-29-95
         IF( CWIDTH.LT.12 )THEN
            CLIST(CPOINT:CPOINT+CWIDTH-1) = STR12(:CWIDTH)
         ELSE
            CLIST(CPOINT:CPOINT+CWIDTH-1) = STR12
         ENDIF
CC =======
500   CONTINUE
C
900   CONTINUE
C NOW CLIST = TABLEAU ROW, EXCEPT RHS
      IF( SWRHS )THEN
C ADD RHS
         CPOINT = TAB + NC*(CWIDTH+1) + 1
         IF( ROW.EQ.OBJNUM )THEN
            IF( SOLST.EQ.'OPTIMAL ' )THEN
               CLIST(CPOINT:) = '= '//OPTNAM(:3)
            ELSE IF( SOLST.EQ.'UNBOUNDED' )THEN
               IF( CWIDTH.GE.12 )THEN
                  CLIST(CPOINT:) = '...UNBOUNDED'
               ELSE
                  CLIST(CPOINT:) = '...UNBD'
               ENDIF
            ELSE IF( SOLST.EQ.'FEASIBLE' )THEN
               IF( CWIDTH.GE.12 )THEN
                  CLIST(CPOINT:) = '...'//OPTNAM
               ELSE
                  CLIST(CPOINT:) = '...'//OPTNAM(:3)
               ENDIF
            ENDIF
         ELSE
            CALL GETYPE(ROW,CHAR)
            CALL GETBND('ROW ',ROW,VL,VU)
            IF( CHAR.EQ.'E' )THEN
               CALL FR2CLJ(STR12,VL,LAST)
               CLIST(CPOINT:) = ' ='//STR12
            ELSE IF( CHAR.EQ.'L' )THEN
               CALL FR2CLJ(STR12,VU,LAST)
               CLIST(CPOINT:) = '<='//STR12
            ELSE IF( CHAR.EQ.'G' )THEN
               CALL FR2CLJ(STR12,VL,LAST)
               CLIST(CPOINT:) = '>='//STR12
            ELSE IF( CHAR.EQ.'N' )THEN
               CLIST(CPOINT:) = 'Free'
            ELSE
               CLIST(CPOINT:) = 'Range'
            ENDIF
         ENDIF
      ENDIF
C
      RETURN
C
C ** AQLTRW ENDS HERE
      END
      SUBROUTINE APICTR(CLIST,FIRST,LAST,FILOUT,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCANAL
CI$$INSERT DCFLIP
C
C This pictures nonzeroes in submatrix to FILOUT
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME,CNAME
      LOGICAL*1     SW,SWRC
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      CALL ACOUNT(CLIST,FIRST,LAST,0,RCODE)
      IF(RCODE.NE.0)RETURN
C
      CLIST = 'Picture of submatrix'
      CALL FCENTR(CLIST,I1,I2)
      WRITE(FILOUT,1)(CLIST(I:I),I=1,I2)
1     FORMAT(128A1)
C
      LINE   = 1
CC 5-12-95
      MXCOLS = SCRWTH - NAMELN - 2
CC      MXCOLS = SCRWTH - 10
      COL1   = RCSUB1(2)
      NMPRNT = 0
C
100   CONTINUE
C    PRINT PAGE OF PICTURE
C    =====================
      NMCOLS = NRCSUB(2) - NMPRNT
      IF(NMCOLS.GT.MXCOLS)THEN
           NMCOLS = MXCOLS
           SPACE = 1
           SWRC = .FALSE.
      ELSE IF(NMCOLS.LE.MXCOLS/2 - 6)THEN
           SWRC = .TRUE.
           SPACE = 2
      ELSE IF(NMCOLS.GT.MXCOLS - 6)THEN
           SWRC = .FALSE.
           SPACE = 1
      ELSE
           SWRC = .TRUE.
           SPACE = 1
      ENDIF
C
C    (SWRC = TRUE means row form also pictured...see below
C    ...SPACE = SCReen COLumn spacing (use 2 if possible; else, 1)
C
      NMPRNT = NMPRNT + NMCOLS
      ROW1 = RCSUB1(1)
      NR = NRCSUB(1)
400   CONTINUE
C
C PRINT COLUMN NAMES VERTICALLY
C
      DO 500 K=1,NAMELN
           NC = NMCOLS
           CLIST = ' '
           SCRCOL = NAMELN + 3
C
           DO 450 COLUMN=COL1,NCOLS
             CALL GETMAP('COL ',COLUMN,SW)
             IF(.NOT.SW)GOTO 450
             NC = NC - 1
             CALL GETNAM('COL ',COLUMN,CNAME)
             CLIST(SCRCOL:SCRCOL) = CNAME(K:K)
             IF(NC.EQ.0)GOTO 475
             SCRCOL = SCRCOL + SPACE
450        CONTINUE
C
475        LASTC = COLUMN
C
C    ...SUPPRESS TRAILING BLANKS IN COLUMN NAMES
           CALL FSLEN(CLIST,128,LS)
           IF(LS.EQ.0)GOTO 510
C
C    NOW CLIST HAS K-TH CHARACTER OF COLUMN NAMES
C
           WRITE(FILOUT,1)(CLIST(I:I),I=1,SCRCOL)
C
500   CONTINUE
C
510   CONTINUE
      LINE = LINE + K
C
C WRITE BY ROWS
C
      DO 900 ROW=ROW1,NROWS
           CALL GETMAP('ROW ',ROW,SW)
           IF(.NOT.SW)GOTO 900
           NR = NR - 1
C
           CALL GETNAM('ROW ',ROW,RNAME)
           CLIST = ' '//RNAME
           SCRCOL = NAMELN + 3
C
           NC = NMCOLS
C
           DO 700 COLUMN=COL1,NCOLS
             CALL GETMAP('COL ',COLUMN,SW)
             IF(.NOT.SW)GOTO 700
C
             NC = NC -1
             CALL GETSGN(ROW,COLUMN,CLIST(SCRCOL:SCRCOL))
C
             IF(NC.EQ.0)GOTO 710
             SCRCOL = SCRCOL + SPACE
700        CONTINUE
C
C SUPPRESS ROW IF NULL
C
710        IF(CLIST(NAMELN+1:).EQ.' ')GOTO 890
C                   :...WAS 10 (FIXED 7-30-93)
C
           CALL FPRMPT(LINE,*999)
           IF(LINE.EQ.0)THEN
              ROW1 = ROW
              GOTO 400
           ENDIF
C    ADD RHS INFO IF ROOM
           IF(SWRC)THEN
             SCRCOL = SCRCOL + 2
             SW = .TRUE.
C           (SW USED TO DETERMINE IF RHS CHARS NEEDED...SEE BELOW)
             IF(RNAME.EQ.OBJNAM)THEN
                 CLIST(SCRCOL:)='= '//OPTNAM(:3)
                 SW = .FALSE.
             ELSE
                 CALL GETBND('ROW ',ROW,VL,VU)
                 IF(VL.GT.-VINF)THEN
                    V = VL
                    IF(VU.LE.VL)THEN
                       CLIST(SCRCOL:)='= '
                    ELSE IF(VU.GE.VINF)THEN
                       CLIST(SCRCOL:)='> '
                    ELSE
                       CLIST(SCRCOL:)='r '
                    ENDIF
C
                ELSE IF(VU.GE.VINF)THEN
C          FREE ROW (NOT OBJ)
                    CLIST(SCRCOL:)='free'
                    SW = .FALSE.
C
                ELSE
                    V = VU
                    CLIST(SCRCOL:)='< '
               ENDIF
C
               IF(SW)THEN
C          SET RHS CHAR (+ - 0)
                 IF(V.LT.0.0)THEN
                    CLIST(SCRCOL+3:) = '-'
                 ELSE IF(V.GT.0.0)THEN
                    CLIST(SCRCOL+3:) = '+'
                 ELSE
                    CLIST(SCRCOL+3:) = '0'
                 ENDIF
               ENDIF
             ENDIF
C
             CALL FSLEN(CLIST,SCRWTH,SCRCOL)
           ENDIF
C
C    NOW WRITE LINE FOR ROW
C
           WRITE(FILOUT,1)(CLIST(I:I),I=1,SCRCOL)
890     IF(NR.EQ.0)GOTO 910
C
900   CONTINUE
C
910   CONTINUE
      IF(NMPRNT.LT.NRCSUB(2))THEN
         CALL FPRMPT(LINE,*999)
         CLIST = ' Picture continued'
         CALL FCENTR(CLIST,I1,I2)
         WRITE(FILOUT,1)
         WRITE(FILOUT,1)(CLIST(I:I),I=1,I2)
         COL1 = LASTC + 1
         GOTO 100
      ENDIF
C
999   RETURN
C
C ** APICTR ENDS HERE
      END
