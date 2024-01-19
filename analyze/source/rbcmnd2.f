C                  ::: RBCMND2.FOR  4-16-94 :::
C
C LAST DATE:  Earlier dates deleted
C             4-28-93...Added RULASK
C             6-20-93...Fixed bug (re default) in RULASK
C             6-29-93...Changed RUFIND to allow ELEMENT *
C             7-03-93...Changes for EXTABL
C             1-14-93...Changed FR2C to FR2CLJ in RUCALC
C
C This contains the following subroutines for the RULEBASE module of ANALYZE
C
C   RULEIF.....executes IF     command
C   RUSKIP.....executes SKIP   command
C   RUGOTO.....executes GOTO   command
C   RULNXT.....executes NEXT   command
C   RUCALC.....executes CALC   command
C   RULOOK.....executes LOOKUP command
C   RUFIND.....executes FIND   command
C   RUFORM.....executes FORM   command
C   RULASK.....executes ASK    command
C
C ROUTINES THAT READ RULE FILE (RULFIL) USE DCFLIP AND CALL FLRDLN
C (FOR INCLUDE)
C
      SUBROUTINE RULEIF(CLIST,FIRST,LAST,SW,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCRULE.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCRULE)
CI$$INSERT DCANAL
CI$$INSERT DCRULE
C
C This determines whether IF condition is true (SW)
C
      CHARACTER*128 CLIST
      LOGICAL*1     SW
C LOCAL
      CHARACTER*1   CHAR
C
      CHARACTER*32  S1,S2
      CHARACTER*2   RULREL
C  ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
C SKIP UNTIL RELATIONAL IS REACHED (< = >)
C
      DO 10 I1=FIRST,LAST
         CHAR = CLIST(I1:I1)
         IF(CHAR.EQ.'=' .OR. CHAR.EQ.'<' .OR. CHAR.EQ.'>')GOTO 20
10    CONTINUE
C
      PRINT *,' ** RULEBASE ERROR...IF RELATION MISSING'
      RCODE = 1
      RETURN
C
20    CONTINUE
      RULREL = CLIST(I1:I1+1)
25    IF(I1.EQ.FIRST)THEN
         S1 = ' '
      ELSE IF(CLIST(FIRST:FIRST).EQ.' ')THEN
C   REMOVE LEADING BLANKS
         FIRST = FIRST + 1
         GOTO 25
      ELSE
         S1 = CLIST(FIRST:I1-1)
      ENDIF
C NOW S1 HAS FIRST STRING...PARSE UNTIL 'THEN' IS REACHED
      FIRST = I1 + 2
      DO 30 I2=FIRST,LAST-5
         IF(CLIST(I2:I2+3).EQ.'THEN')GOTO 40
30    CONTINUE
C
      PRINT *,' ** RULEBASE ERROR...IF MISSING THEN'
      RCODE = 1
      RETURN
C
40    CONTINUE
      IF(I2.EQ.FIRST)THEN
         S2 = ' '
      ELSE IF(CLIST(FIRST:FIRST).EQ.' ')THEN
         FIRST = FIRST + 1
         GOTO 40
      ELSE
         S2 = CLIST(FIRST:I2-1)
      ENDIF
C ...S2 HAS SECOND STRING
      FIRST = I2 + 4
C REMOVE EMBEDDED BLANKS
      L = 32
      CALL FSQUEZ(S1,L)
      L = 32
      CALL FSQUEZ(S2,L)
C COMPARE
      IF(RULREL.EQ.'= ')THEN
         SW = (S1.EQ.S2)
         RETURN
      ELSE IF(RULREL.EQ.'<>'.OR.RULREL.EQ.'><')THEN
         SW = (S1.NE.S2)
         RETURN
      ENDIF
C ...NUMERIC
      IF(S1.EQ.'*')THEN
         V1 = VINF
      ELSE IF(S1.EQ.'-*')THEN
         V1 = -VINF
      ELSE
         CALL FC2R(S1,V1,RCODE)
         IF(RCODE.NE.0)RETURN
      ENDIF
      IF(S2.EQ.'*')THEN
         V2 = VINF
      ELSE IF(S2.EQ.'-*')THEN
         V2 = -VINF
      ELSE
         CALL FC2R(S2,V2,RCODE)
         IF(RCODE.NE.0)RETURN
      ENDIF
      IF(RULREL.EQ.'< ')THEN
         SW = (V1.LT.V2)
      ELSE IF(RULREL.EQ.'> ')THEN
         SW = (V1.GT.V2)
      ELSE IF(RULREL.EQ.'<=')THEN
         SW = (V1.LE.V2)
      ELSE IF(RULREL.EQ.'>=')THEN
         SW = (V1.GE.V2)
      ELSE
         PRINT *,' ** RULEBASE ERROR...',
     1           'IF RELATIONAL ERROR...',RULREL,' NOT RECOGNIZED'
         RCODE = 1
      ENDIF
C
      RETURN
C
C ** RULEIF ENDS HERE ****
      END
      SUBROUTINE RUSKIP(CLIST,FIRST,LAST,RCODE,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
      INCLUDE 'DCANAL.'
      INCLUDE 'DCRULE.'
CITSO      INCLUDE (DCFLIP)
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCRULE)
CI$$INSERT DCFLIP
CI$$INSERT DCANAL
CI$$INSERT DCRULE
C
C This executes SKIP number-of-lines | LOOP | ENDLOOP
C                                      :      :...1 after loop
C                                      :.....to NEXT statement
C Alternate RETURN is for end of file when reading.
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME
      CHARACTER*1   CHAR
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
      IF(SWLOOP.AND.RNAME.EQ.'LOOP')THEN
C SET LOOP POINTER (ILOOP) TO JUST BEFORE END OF LOOP, SO NEXT IS EXECUTED
         ILOOP = NRLOOP - 1
         RETURN
      ENDIF
C PARSE FOR NUMBER OF LINES TO SKIP
      CALL FC2I(RNAME,8,NUMBER,RCODE)
      IF(RCODE.NE.0)GOTO 1300
C NOW NUMBER OF LINES TO SKIP IS SET
C
      IF(SWLOOP)THEN
C ADVANCE LOOP STATEMENT POINTER (ILOOP)
         ILOOP = ILOOP + NUMBER
         SWLOOP = (ILOOP.LT.NRLOOP)
C (LOOP CLOSES IF WE SKIP PAST ITS LAST STATEMENT)
         IF(SWLOOP)RETURN
C WE MUST SKIP MORE LINES OUTSIDE OF LOOP
         NUMBER = ILOOP - NRLOOP
C NUMBER = 0 MEANS WE SKIP TO THE NEXT LINE AFTER LOOP (SAME AS ENDLOOP)
C        > 0 MEANS WE SKIP (NUMBER) PAST END OF LOOP
      ENDIF
C
100   IF(NUMBER.GT.0)THEN
         CALL FLRDLN(CLIST,RULFIL,RCODE)
         NUMBER = NUMBER-1
         IF(RCODE)9000,100,1300
      ENDIF
C
      RETURN
C
1300  RCODE = 13
      PRINT *,' ** SYSIO ERR...SKIP...',RNAME
      RETURN
C
9000  RETURN 1
C
C ** RUSKIP ENDS HERE
      END
      SUBROUTINE RUGOTO(CLIST,FIRST,LAST,RCODE,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
      INCLUDE 'DCANAL.'
      INCLUDE 'DCRULE.'
CITSO      INCLUDE (DCFLIP)
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCRULE)
CI$$INSERT DCFLIP
CI$$INSERT DCANAL
CI$$INSERT DCRULE
C
C This executes GOTO label
C Alternate RETURN is for end of file (label not found) when reading.
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME,CNAME
      CHARACTER*1   CHAR
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C GET LABEL
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
      IF(RNAME.EQ.' ')RETURN 1
      IF(RNAME.EQ.'TOP')THEN
C GOTO TOP OF FILE
         REWIND(RULFIL)
         IF(SWINCL)THEN
            SWINCL = .FALSE.
            CLOSE(INCFIL)
         ENDIF
         INPLIN = 0
         SWLOOP = .FALSE.
         RETURN
      ENDIF
C
      CNAME = ':'//RNAME
      IF(SWLOOP)THEN
C LOOK FOR LABEL IN LOOP FIRST (CAN BE ANYWHERE)
         DO 50 I=1,NRLOOP
            IF(RULOOP(I).EQ.CNAME)THEN
               ILOOP = I
               RETURN
            ENDIF
50       CONTINUE
C ...NOW WE'RE OUT OF LOOP
         SWLOOP = .FALSE.
      ENDIF
C
C WE MUST NOW LOOK FOR LABEL PAST LOOP
100   CALL FLRDLN(CLIST,RULFIL,RCODE)
      IF(RCODE)9000,101,1300
C
101   IF(CLIST.NE.CNAME)GOTO 100
C  GOT IT!...
      RETURN
C
1300  RCODE = 13
      PRINT *,' ** SYSIO ERR...GOTO ',CNAME
      RETURN
C
9000  CONTINUE
      PRINT *,' ** RULEBASE ERROR...GOTO ?',CNAME
      RETURN 1
C
C ** RUGOTO ENDS HERE
      END
      SUBROUTINE RULNXT(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCRULE.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCRULE)
CI$$INSERT DCANAL
CI$$INSERT DCRULE
C
C This executes the NEXT command...advances ROW and/or COLUMN
C ...sets RULROW/RULCOL = 0 after last one
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*4   ROWCOL
      LOGICAL*1     SW
C :::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::
      CALL AGETRC(CLIST,FIRST,LAST,ROWCOL,RCODE)
      IF(RCODE.NE.0)GOTO 1300
      IF(ROWCOL.EQ.'ROW '.OR.ROWCOL.EQ.' ')THEN
         NUMBER = RULROW + 1
         IF(NUMBER.GT.NROWS)THEN
            RULROW = 0
         ELSE
            DO 10 RULROW=NUMBER,NROWS
               CALL GETMAP('ROW ',RULROW,SW)
               IF(SW)GOTO 90
10          CONTINUE
            RULROW = 0
         ENDIF
      ENDIF
90    CONTINUE
      IF(ROWCOL.EQ.'COL '.OR.ROWCOL.EQ.' ')THEN
         NUMBER = RULCOL + 1
         IF(NUMBER.GT.NCOLS)THEN
            RULCOL = 0
         ELSE
            DO 100 RULCOL=NUMBER,NCOLS
               CALL GETMAP('COL ',RULCOL,SW)
               IF(SW)RETURN
100         CONTINUE
            RULCOL = 0
         ENDIF
      ENDIF
C
      RETURN
C
1300  PRINT *,' ** RULEBASE ERROR...NEXT ',CLIST(:LAST)
      RCODE = 1
      RETURN
C
C ** RULNXT ENDS HERE
      END
      SUBROUTINE RUCALC(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCRULE.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCRULE)
CI$$INSERT DCANAL
CI$$INSERT DCRULE
C
C This calculates an expression and puts into user-defined variable
C
C SYNTAX:  $CALC name [=] expression
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*32  STRVAL
      CHARACTER*16  RNAME
      CHARACTER*1   CHAR,RUCOP
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::
C GET NAME OF VARIABLE
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
      IF(RNAME.EQ.' ')THEN
         PRINT *,' ** RULEBASE ERROR...MISSING VARIABLE NAME'
         GOTO 1300
      ENDIF
      CALL RUGETP(RNAME,STRVAL,RCODE)
      IF(RCODE.NE.0)GOTO 1300
C INITIALIZE VALUE
      CALL FC2R(STRVAL,V,RCODE)
      IF(RCODE.NE.0)GOTO 1300
C BEFORE PARSING EXPRESSION, LET US REMOVE  -+  -- +- ++
C (WHICH MAY BE PRESENT DUE TO RUTRAN INCLUDING SIGN ON VALUE RETRIEVAL)
      K=FIRST
10    IF(CLIST(K:K).NE.'-')GOTO 19
         KMINUS=K
11       K = K+1
         CHAR = CLIST(K:K)
         IF(CHAR.EQ.' ')GOTO 11
         IF(CHAR.EQ.'+')THEN
            CLIST(K:K)=' '
         ELSE IF(CHAR.EQ.'-')THEN
            CLIST(KMINUS:KMINUS)='+'
            CLIST(K:K) = ' '
         ENDIF
19    CONTINUE
20    IF(CLIST(K:K).NE.'+')GOTO 29
         KPLUS =K
21       K = K+1
         CHAR = CLIST(K:K)
         IF(CHAR.EQ.' ')GOTO 21
         IF(CHAR.EQ.'+')THEN
            CLIST(K:K)=' '
         ELSE IF(CHAR.EQ.'-')THEN
            CLIST(KPLUS:KPLUS)='-'
            CLIST(K:K) = ' '
         ENDIF
29    CONTINUE
      IF(K.LT.LAST)THEN
         K=K+1
         GOTO 10
      ENDIF
C PARSE EXPRESSION
      CALL FTOKEN(CLIST,FIRST,LAST,STRVAL,32,CHAR)
      IF(STRVAL.EQ.' ')GOTO 900
C
C STRVAL MUST BE ONE OF:  constant, current variable (RNAME),
C        %Vkey (intrinsic), %%user-param (defined previously by SET)
C However, RUTRAN already replaced keys with (string) values, so
C  STRVAL must be constant or current variable.
C
      IF(STRVAL(:8).EQ.RNAME)THEN
         V1 = V
      ELSE
C CONSTANT...CONVERT
         CALL FC2R(STRVAL,V1,RCODE)
         IF(RCODE.NE.0)GOTO 1300
      ENDIF
C  PARSE LOOP...V1 IS SET...LOOK FOR OPERATOR (RUCOP)
C GET OPERATOR (* + - /)
100   CALL FTOKEN(CLIST,FIRST,LAST,RUCOP,1,CHAR)
      IF(RUCOP.EQ.' ')THEN
         V = V1
         GOTO 900
      ENDIF
C NOW GET 2ND OPERAND (V2)
      CALL FTOKEN(CLIST,FIRST,LAST,STRVAL,32,CHAR)
      IF(STRVAL.EQ.' ')GOTO 1300
      IF(STRVAL(:8).EQ.RNAME)THEN
         V2 = V
      ELSE
C CONSTANT...CONVERT
         CALL FC2R(STRVAL,V2,RCODE)
         IF(RCODE.NE.0)GOTO 1300
      ENDIF
C WE NOW HAVE V1 RUCOP V2
      IF(RUCOP.EQ.'+')THEN
         V1 = V1 + V2
      ELSE IF(RUCOP.EQ.'-')THEN
         V1 = V1 - V2
      ELSE IF(RUCOP.EQ.'*')THEN
         V1 = V1*V2
      ELSE IF(RUCOP.EQ.'/')THEN
         IF(ABS(V2).LT.VTOLAB)THEN
            PRINT *,' ** RULEBASE ERROR...ATTEMPT TO DIVIDE BY 0'
            GOTO 1300
         ENDIF
         V1 = V1/V2
      ELSE
         GOTO 1300
      ENDIF
      GOTO 100
C
900   CONTINUE
C STORE V INTO USER'S PARAM
      CALL FR2CLJ(STRVAL,V,LAST)
920   CLIST = RNAME//STRVAL
      FIRST = 1
      CALL FSLEN(CLIST,40,LAST)
      CALL RUPUTP(CLIST,FIRST,LAST,RCODE)
      IF(RCODE.EQ.0)RETURN
C
1300  PRINT *,' ** RULEBASE ERROR...CALC ',CLIST(:LAST)
      CLIST = ' '
      CLIST(FIRST:) = '?'
      PRINT *,'                          ',CLIST(:FIRST)
      RCODE = 1
      RETURN
C
C ** RUCALC ENDS HERE ****
      END
      SUBROUTINE RULOOK(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCRULE.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCRULE)
CI$$INSERT DCANAL
CI$$INSERT DCRULE
C
C This executes LOOKUP command.
C Syntax:  LOOKUP {X|D|C|XL|XU} column name
C                |{Y|P|S|YL|YU} row name
C                | NZ row,column
C                | BLOCK {ROW COL} name
C
C RESULT IS PUT INTO VLOOK.
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME,CNAME
      CHARACTER*4   ROWCOL
      CHARACTER*1   CHAR
C  ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
      FR = FIRST
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
      IF(RNAME(1:1).EQ.'X'.OR.RNAME.EQ.'D'.OR.RNAME.EQ.'C')THEN
         ROWCOL = 'COL'
      ELSE IF(RNAME(1:1).EQ.'Y'.OR.RNAME.EQ.'P'.OR.RNAME.EQ.'S'.OR.
     1        RNAME.EQ.'NZ')THEN
         ROWCOL = 'ROW'
      ELSE IF(RNAME.EQ.'BLOCK')THEN
         GOTO 1000
      ELSE
         FIRST = FR
         GOTO 1300
      ENDIF
      FC = FIRST
      CALL FTOKEN(CLIST,FIRST,LAST,CNAME,8,CHAR)
      IF(CNAME.EQ.' ')THEN
         CLIST(LAST:) = ' ** no name'
         LAST = LAST + 12
         GOTO 1300
      ENDIF
      CALL GETNUM(ROWCOL,CNAME,NUMBER)
      IF(NUMBER.EQ.0)THEN
         FIRST = FC
         GOTO 1300
      ENDIF
C
      IF(RNAME.EQ.'X'.OR.RNAME.EQ.'Y')THEN
C LEVEL
         CALL GETSOL(ROWCOL,NUMBER,VLOOK,VP,CHAR,STNUM)
      ELSE IF(RNAME.EQ.'D'.OR.RNAME.EQ.'P')THEN
C PRICE
         CALL GETSOL(ROWCOL,NUMBER,VX,VLOOK,CHAR,STNUM)
      ELSE IF(RNAME(2:2).EQ.'L')THEN
C LO BOUND
         CALL GETBND(ROWCOL,NUMBER,VLOOK,VU)
      ELSE IF(RNAME(2:2).EQ.'U')THEN
C UP BOUND
         CALL GETBND(ROWCOL,NUMBER,VL,VLOOK)
      ELSE IF(RNAME.EQ.'NZ')THEN
C NONZERO
         FR = FIRST
         CALL FTOKEN(CLIST,FIRST,LAST,CNAME,8,CHAR)
         IF(CNAME.EQ.' ')THEN
            FIRST = FR
            GOTO 1300
         ENDIF
         CALL GETNUM('COL ',CNAME,NUM)
         IF(NUM.EQ.0)THEN
            FIRST = FR
            GOTO 1300
         ENDIF
         CALL GETCOL(NUM,NZ,MAXLST,ROWLST,VALLST)
         VLOOK = 0.
         IF(NZ.EQ.0)RETURN
         DO 200 I=1,NZ
            IF(ROWLST(I).EQ.NUMBER)THEN
               VLOOK = VALLST(I)
               RETURN
            ENDIF
200      CONTINUE
      ELSE IF(RNAME.EQ.'C')THEN
C OBJ
         CALL GETRIM(ROWCOL,NUMBER,VL,VU,VLOOK,NZ)
      ELSE IF(RNAME.EQ.'S')THEN
         CALL GETSOL(ROWCOL,NUMBER,VY,VP,CHAR,STNUM)
         CALL GETBND(ROWCOL,NUMBER,VL,VU)
         IF(VL.GT.-VINF)THEN
            VLOOK = VY - VL
         ELSE IF(VU.LT.VINF)THEN
            VLOOK = VU - VY
         ELSE
            VLOOK = VINF
         ENDIF
      ELSE
         FIRST = FR
         GOTO 1300
      ENDIF
      RETURN
C
1000  CONTINUE
C BLOCK {ROW COL} name
      CALL FTOKEN(CLIST,FIRST,LAST,ROWCOL,4,CHAR)
      IF(ROWCOL.NE.'ROW '.AND.ROWCOL.NE.'COL ')GOTO 1300
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
C NOW RNAME = BLOCK NAME...LOOKUP
      CALL BLOC8(ROWCOL,RNAME,BLOCK,RCODE)
      IF(RCODE.NE.0)THEN
         RCODE = 0
         VLOOK = 0.
      ELSE
         VLOOK = BNUMBR(BLOCK)
      ENDIF
C
      RETURN
C
1300  PRINT *,' ** RULEBASE ERROR...LOOKUP ',CLIST(:LAST)
      CLIST = ' '
      CLIST(FIRST:) = '?'
      PRINT *,'                            ',CLIST(:FIRST)
      RCODE = 1
      RETURN
C
C ** RULOOK ENDS HERE
      END
      SUBROUTINE RUFIND(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCRULE.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCRULE)
CI$$INSERT DCANAL
CI$$INSERT DCRULE
C
C This executes FIND command.
C   Syntax:  $FIND {{ROW | COLUMN | SET | ELEMENT set} string
C
      CHARACTER*128 CLIST
C FIRST, LAST AND CLIST ARE OVER-WRITTEN (USED TO PUT PARAMS)
C LOCAL
      CHARACTER*1   CHAR
      CHARACTER*128 OBLIST
      CHARACTER*8   OBJECT,OPTION(5),STR8
C  ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
      OPTION(1) = 'ROW '
      OPTION(2) = 'COLUMN'
      OPTION(3) = 'SET '
      OPTION(4) = 'ELEMENT'
      NUMBER = 4
      RCODE = 1
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF( RCODE.NE.0 )RETURN
      OBJECT = OPTION(NUMBER)
C INITIALIZE COLLECTION
      OBLIST = ' '
C
      IF( OBJECT.EQ.'ELEMENT' )THEN
C GET SET TO CONTAIN ELEMENT
         CALL FTOKEN(CLIST,FIRST,LAST,STR8,8,CHAR)
         IF( STR8.EQ.' ' )THEN
            PRINT *,' ** RULEBASE ERROR: FIND ELEMENT REQUIRES SET'
            RCODE = 2
            RETURN
         ENDIF
C LOOKUP SET TO BE SURE IT'S THERE
         IF( NRSYN.GT.0 )THEN
            IF( STR8.EQ.'*' )GOTO 110
            DO 100 I=1,NENTY
               BEGSET = EXINDX(I-1) + 1
               IF( STR8.EQ.EXKEY(BEGSET) )GOTO 110
100         CONTINUE
         ENDIF
C FALL THRU MEANS SET NOT THERE
         PRINT *,' ** RULEBASE ERROR: CANNOT FIND SET ',STR8
         RCODE = 3
         RETURN
C
110      CONTINUE
         OBLIST = STR8
C NOW OBLIST = NAME OF SPECIFIED SET (INPUT TO EXFIND)
      ENDIF
C
      CALL EXFIND(CLIST(FIRST:LAST+1),OBJECT,OBLIST)
C     ==============================================
C     OBJECTS ARE NOW COLLECTED INTO OBLIST (MAYBE NULL)
C PUT INTO PARAMS %%1, ..., %%9
      FIRST = 1
      CALL FSLEN(OBLIST,128,LAST)
      DO 9100 I=1,9
C ADD CLASS AS PARAM
         CALL FI2C(STR8,I,F)
         CLIST = STR8(F:F)//'='
         CALL FTOKEN(OBLIST,FIRST,LAST,CLIST(3:),16,CHAR)
         F = 1
         L = 18
         CALL RUPUTP(CLIST,F,L,RCODE)
         IF( RCODE.NE.0 )RETURN
9100  CONTINUE
C
      RETURN
C
C ** RUFIND ENDS HERE
      END
      SUBROUTINE RUFORM(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCRULE.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCRULE)
CI$$INSERT DCANAL
CI$$INSERT DCRULE
C
C This executes FORM command.
C   Syntax:  $FORM {ROW | COLUMN} class set member
C Resulting name mask is put into parameter %%1.
C If class is null, * is returned.  If set is null, class is returned.
C
C If class does not contain set, the member and its meaning are
C used to search other domain sets.  If this fails, the parameter
C value is set to null (user should skip this class).
C
      CHARACTER*128 CLIST
C LOCAL
      LOGICAL*1     SW
      CHARACTER*1   CHAR
      CHARACTER*128 CTEMP
      CHARACTER*16  RCNAME
      CHARACTER*8   OPTION(2),STR8,CLASS,SET,MEMBER
      DATA    OPTION/'ROW ','COLUMN'/
C  ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
      NUMBER = 2
      RCODE = -1
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF( RCODE.NE.0 )RETURN
      CALL FTOKEN(CLIST,FIRST,LAST,CLASS ,8,CHAR)
      CALL FTOKEN(CLIST,FIRST,LAST,SET   ,8,CHAR)
      CALL FTOKEN(CLIST,FIRST,LAST,MEMBER,8,CHAR)
      IF( CLASS.EQ.' ' )THEN
         RCNAME = '*'
         GOTO 9000
      ENDIF
      RCNAME = CLASS
      IF( SET.EQ.' ' )GOTO 9000
C WE NEED THE POSITION OF SET IN CLASS
      IF( NUMBER.EQ.1 )THEN
C ROW
         I0 = IRSYN
         NUMBER = NRSYN
      ELSE IF( NUMBER.EQ.2 )THEN
C COL
         I0 = ICSYN
         NUMBER = NCSYN
      ENDIF
C
      IF( NUMBER.EQ.0 )RETURN
C
      CALL FSLEN(CLASS,8,LC)
      CALL FSLEN(SET,8,LS)
      CALL FSLEN(MEMBER,8,LM)
C FORM STRING (CTEMP) IN CASE WE NEED TO TRANSLATE (SEE BELOW)
C ::: LOOP OVER SETS TO FIND SET :::
      IF( NENTY.EQ.0 )GOTO 113
      DO 100 I=1,NENTY
         BEGSET = EXINDX(I-1)+1
         IF( SET.EQ.EXKEY(BEGSET) )GOTO 150
100   CONTINUE
113   PRINT *,' ** SYSERR RUFORM...SET=',SET
      RCODE = 13
      RETURN
150   CONTINUE
C NOW FIND MEMBER
      ENDSET = EXINDX(I)
      IF( ENDSET.LE.BEGSET )GOTO 213
      DO 200 I=BEGSET+1,ENDSET
         IF( MEMBER.EQ.EXKEY(I) )GOTO 250
200   CONTINUE
213   PRINT *,' ** SYSERR RUFORM...MEMBER=',MEMBER
      RCODE = 13
      RETURN
250   CONTINUE
C SET SLIDING MASK = MEANING OF MEMBER
      CTEMP = '"'//EXTABL(I)
C SET NAME MASK TO JUST THE CLASS NAME, BUT WITH TRAILING MASK CHARS
C (SO WE CAN INSERT MEMBER INTO POSITION)
C               1234567890123456
      RCNAME = '****************'
      RCNAME(1:LC) = CLASS(1:LC)
      IF( NAMELN.LT.16 )RCNAME(NAMELN+1:) = ' '
C
C ::: LOOP OVER ROW|COL CLASSES :::
      DO 900 I=I0+1,I0+NUMBER
         IF( CLASS.NE.EXKEY(I) )GOTO 900
C THIS IS ROW/COL CLASS...FIND SET IN DOMAIN
         STR8 = '&'//SET(:LS)//':'
         CALL FLOOKF(EXTABL(I),1,80,STR8,POSN)
         IF( POSN.GT.0 )THEN
C SET IS IN SYNTAX MAP...FIX ITS POSITION
            POSN = POSN + LS + 2
            STR8 = EXTABL(I)(POSN:POSN+1)
            CALL FC2I(STR8,8,POSN,RCODE)
            IF( RCODE.NE.0 )RETURN
            RCNAME(POSN:) = MEMBER
            GOTO 9000
         ENDIF
C SET IS NOT IN SYNTAX MAP...SEE IF CLASS SYNTAX HAS MEANING OF MEMBER
         CALL FMATCH(CTEMP,EXTABL(I),' ',SW)
         IF( SW )GOTO 9000
C            :...IT DOES, SO XFER TO 9000 (WITH RCNAME = CLASS)
C CLASS DOES NOT HAVE MEMBER MEANING IN IT
C SEE IF SOME DOMAIN SET HAS MEMBER MEANING
C THE RESULT OF THIS EFFORT WILL BE EITHER TO FIND A FIXED POSITION
C FOR THE NAME MASK OR TO RETURN NULL NAME MASK TO CALLER (913).
         LOOK = 1
C   TOP OF LOOP TO ENTER MEMBER INTO DOMAIN POSITION
750      CONTINUE
           CALL FLOOKF(EXTABL(I),LOOK,80,'&',LOC)
           IF( LOC.EQ.0 )GOTO 913
           CALL FLOOKF(EXTABL(I),LOC+1,80,':',LOOK)
           IF( LOOK.EQ.0 )GOTO 913
C EXTABL(I)(LOC+1:LOOK-1) = SET NAME
           STR8 = EXTABL(I)(LOC+1:LOOK-1)
           L = 1
           CALL EXENTY(STR8,MEMBER,CLIST,L,RCODE)
           IF( RCODE.NE.0.OR.CLIST(1:2).EQ.' ?' )THEN
              RCODE = 0
              GOTO 790
           ENDIF
C NOW SET (STR8) HAS MEMBER AND CLIST = MEANING OF MEMBER IN SET
           CALL FMATCH(CTEMP,CLIST,' ',SW)
           IF( .NOT.SW )GOTO 790
C THIS SET MEMBER MATCHES...PUT INTO MASK
           STR8 = EXTABL(I)(LOOK+1:LOOK+2)
           CALL FC2I(STR8,8,POSN,RCODE)
           IF( RCODE.NE.0 )RETURN
C NOW POSN = POSITION IN NAME WHERE MEMBER GOES
           RCNAME(POSN:POSN+LM-1) = MEMBER(1:LM)
           GOTO 9000
790      CONTINUE
C   BOTTOM OF LOOP TO ENTER MEMBER INTO DOMAIN POSITION
         LOOK = LOOK+3
         IF( LOOK-61 )750,750,913
900   CONTINUE
C
913   CONTINUE
C WE FAILED TO FIND A POSITION FOR MEMBER
C ...TELL CALLER NOT TO USE THIS CLASS
      RCNAME = ' '
C
9000  CONTINUE
C PUT RCNAME INTO PARAMETER %%1
      F = 1
      L = 18
      CTEMP = '1='//RCNAME
      CALL RUPUTP(CTEMP,F,L,RCODE)
C
      RETURN
C
C ** RUFORM ENDS HERE
      END
      SUBROUTINE RULASK(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
      INCLUDE 'DCANAL.'
      INCLUDE 'DCRULE.'
CITSO      INCLUDE (DCFLIP)
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCRULE)
CI$$INSERT DCFLIP
CI$$INSERT DCANAL
CI$$INSERT DCRULE
C          ......DCFLIP NEEDED FOR CHRNUM
C
C This executes the ASK command.
C       Syntax:  $ASK [{answer list}] [message string]
C                      :...........:
C              BRACES MANDATORY TO DELIMIT LIST
C Answer is put into parameter %%1.  First member of answer list is
C default.  Abbreviations allowed, and %%1 will contain full answer.
C If no answer list, response(s) are 8-char tokens put into %%1,...,%%9
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*80  STR80
      CHARACTER*8   OPTION(9),STR8
      CHARACTER*1   CHAR
C  ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      CALL FLOOKF(CLIST,FIRST,LAST,'}',ENDANS)
CIEIA      IF( ENDBAS.EQ.0 )CALL FLOOKF(CLIST,FIRST,LAST,'>',ENDANS)
CIEIAC                                                    :...REPLACES }
      NUMBER = 0
      IF( ENDANS.GT.0 )THEN
C ANSWER LIST PRESENT...PARSE OPTIONS
         BEGANS = FIRST
CIEIA         CALL FLOOKF(CLIST,FIRST,LAST,'<',BEGANS)
CIEIAC                                      :...REPLACES {
CIEIA         IF( BEGANS.GT.0 )BEGANS = BEGANS+1
         FIRST  = ENDANS+1
         ENDANS = ENDANS-1
C PARSE OPTIONS IN CLIST(BEGANS:ENDANS)
100      CONTINUE
         CALL FTOKEN(CLIST,BEGANS,ENDANS,STR8,8,CHAR)
         IF( STR8.NE.' ' )THEN
            IF( NUMBER.EQ.9 )THEN
               PRINT *,' ** TOO MANY ANSWERS IN ASK COMMAND...MAX = 9'
               RCODE = 1
               RETURN
            ENDIF
            NUMBER = NUMBER+1
            OPTION(NUMBER) = STR8
            GOTO 100
         ENDIF
         IF( NUMBER.EQ.0 )THEN
            PRINT *,' ** ANSWER LIST EMPTY IN ASK COMMAND'
            RCODE = 1
            RETURN
         ENDIF
      ENDIF
C ===== OPTIONS SET...MESSAGE IS CLIST(FIRST:LAST) =====
      IF( FIRST.LE.LAST )PRINT *,' ',CLIST(FIRST:LAST)
C GIVE USER PROMPT
      IF( NUMBER.EQ.0 )THEN
         CLIST = ' ENTER:'
      ELSE
         CLIST = ' ANSWER ('
         LAST = 9
         DO 400 I=1,NUMBER
            CLIST(LAST+1:) = OPTION(I)
            CALL FSLEN(CLIST,80,LAST)
            CLIST(LAST+1:) = ','
            LAST = LAST+2
400      CONTINUE
         CALL FSLEN(CLIST,80,LAST)
         CLIST(LAST:) = '):'
      ENDIF
C
      CALL FSLEN(CLIST,80,LAST)
      PRINT *,CLIST(1:LAST)
      READ(*,'(A80)')STR80
      FIRST = 1
      LAST = 80
      IF( NUMBER.EQ.0 )THEN
C NO OPTION LIST...PARSE TOKENS AND PUT INTO PARAMS 1-9 (MAY BE NULL)
         L = 13
         DO 600 I=1,9
            CALL FTOKEN(STR80,FIRST,LAST,STR8,8,CHAR)
            CLIST = CHRNUM(I)//' = '//STR8
            F = 1
            CALL RUPUTP(CLIST,F,L,RCODE)
            IF( RCODE.NE.0 )RETURN
600      CONTINUE
         RETURN
      ENDIF
C OPTION LIST PRESENT...ALLOW 3 TRIES (USER MIGHT ERR)
      NTRIES = 3
C TOP OF TRIES LOOP TO GET ANSWER FROM USER
500   CONTINUE
C ============
      IF( STR80.EQ.' ' )THEN
C USER PRESSED ENTER...ANSWER = DEFAULT
         N = 1
      ELSE
C USER ENTERED OPTION...PARSE
         N = NUMBER
         RCODE = -1
         CALL FOPTN(STR80,FIRST,LAST,OPTION,N,RCODE)
         IF( RCODE.NE.0 )THEN
C USER ERROR
            IF( NTRIES.LE.0 )RETURN
C ...TRY AGAIN
            RCODE = 0
            NTRIES = NTRIES - 1
            IF( NTRIES.EQ.0 )THEN
               PRINT *,' LAST TRY...'
            ELSE
               PRINT *,' TRY AGAIN...'
            ENDIF
            READ(*,'(A80)')STR80
            FIRST = 1
            GOTO 500
         ENDIF
      ENDIF
C ===== ANSWER = OPTION(N) ... PUT INTO PARAM 1 =====
      CLIST = '1 = '//OPTION(N)
      FIRST = 1
      LAST  = 13
      CALL RUPUTP(CLIST,FIRST,LAST,RCODE)
C
      RETURN
C
C ** RULASK ENDS HERE
      END
