C             ::: GETIOMAT.FOR  7-16-95 :::
C
C Dates: ealier dates deleted
C       2-06-94...Changed format 301 from G12 to G13
C       2-19-94...Added PRBNAM fix (CC)
C       7-06-95...Added name LENGTH to calling sequence
C
C This contains the following GETMAT IO subroutine.
C
C     GETMAT.....reads matrix file
C
      SUBROUTINE GETMAT(SWMSG,LENGTH,VTZERO,MXNROW,MXNCOL,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This reads matrix file from unit DATFIL, presumed open
C ...PCKFIL is used as scratch unit, UNFORMATTED
C
C ...DATFIL and PCKFIL are closed before RETURN in any case.
C
C SUBROUTINES CALLED:  GADDNZ  GETNUM  GETVID  GETRIM
C
      LOGICAL*1    SWMSG
C                  :...MESSAGE SWITCH
      REAL         VTZERO
C                  :...ZERO TOLERANCE FOR ADDING NONZERO
C                      (CAN CHANGE VTOLAB)
      INTEGER      MXNROW,MXNCOL,LENGTH
C                  :      :      :...Range Checked
C                  :......:....MAX NUMBER ALLOWED BY CALLER
C LOCAL VARIABLES
      CHARACTER*16 CNAME,RNAME1,RNAME2,SOSNAM,SETNAM,ERRNAM,
     1             FNAME,LNAME
      CHARACTER*8  SECTN,MARKER,SOSORG,SOSEND
      CHARACTER*1  CHR1,ROWTYP
      CHARACTER*2  BNDTYP
      CHARACTER*80 CHR80
      PARAMETER (MXWARN=5)
C                :...MAX NUMBER OF WARNING MESSAGES
C
C CHR1 reads column 1 in sections to skip comment card (*)
C ROWTYP....N (free), G (>), L (<), E (=)
C           0         1      2      3
C BNDTYP....MI        LO     UP     FX     PL
C
C The following are section names (unix users see below)
      CHARACTER*8  NAMSEC,ROWSEC,COLSEC,RHSSEC,RNGSEC,BNDSEC
C
C CALLS  GETRIM, GETVID, GETNUM, GADDNZ
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
      IF( LENGTH.LT.1 .OR. LENGTH.GT.10 )THEN
         PRINT *,' ** NAME LENGTH OUT OF RANGE...',LENGTH,
     1           ' (MUST BE 1 TO 10)'
         RCODE = 1
         RETURN
      ENDIF
      NAMELN = LENGTH
C CHECK FOR ABSOLUTE TOLERANCE REDUCTION (PERMANANENT FOR SESSION)
      IF( VTZERO.LE.0. .OR. VTZERO.GE.1. )THEN
C USER HAS NOT SET ZERO TOLERANCE...SET = ABSOLUTE TOLERANCE
          VTZERO = VTOLAB
      ELSE IF( VTZERO.LE.VTOLAB )THEN
C CALLER HAS LEGITIMATE ZERO TOLERANCE, LESS THAN CURRENT SETTING
C            :...IE, > 0 AND < 1                  (VTOLAB)
         VTOLAB = VTZERO
         IF( SWMSG )PRINT *,' Changed absolute tolerance to',VTOLAB
      ENDIF
C The following need to be changed to lower case if that is what
C the unix user wants (see below).
      MARKER = '''MARKER'''
      SOSORG = '''SOSORG'''
      SOSEND = '''SOSEND'''
      NAMSEC = 'NAME'
      ROWSEC = 'ROWS'
      COLSEC = 'COLUMNS'
      RHSSEC = 'RHS'
      RNGSEC = 'RANGES'
      BNDSEC = 'BOUNDS'
C Here is an autoedit that will over-write with lower case
C (Replace CLCED with null.)
CLCED      MARKER = '''marker'''
CLCED      SOSORG = '''sosorg'''
CLCED      SOSEND = '''sosend'''
CLCED      NAMSEC = 'name'
CLCED      ROWSEC = 'rows'
CLCED      COLSEC = 'columns'
CLCED      RHSSEC = 'rhs'
CLCED      RNGSEC = 'ranges'
CLCED      BNDSEC = 'bounds'
C
C INITIALIZE
      CALL GNEWLP
      NWARNV = MXWARN
      LINE = 0
C READ NAME CARD
      SECTN=NAMSEC
10    READ(DATFIL,1,END=1380,ERR=1313)SETNAM,PRBNAM
1     FORMAT(A8,T15,A8)
      LINE = LINE+1
      IF(SETNAM.EQ.SECTN)GOTO 19
C SKIP IF LINE IS COMMENT (* IN COLUMN 1) OR BLANK
      IF(SETNAM(1:1).EQ.'*' .OR.
     1   (SETNAM.EQ.' ' .AND. PRBNAM.EQ.' '))GOTO 10
C
C FILE IS NOT STANDARD MPS MATRIX FILE...TRY TO DIAGNOSE
      IF(SETNAM(1:1).EQ.' ')THEN
          PRINT *,'  ** SEEMS LIKE FIRST COLUMN IS BLANK',
     1            ' THROUGHOUT...PLEASE CORRECT'
      ELSE
          PRINT *,' ** MATRIX FILE IS NOT IN MPS FORMAT...',
     1            'LAST LINE READ:'
          PRINT *,LINE,':',SETNAM,PRBNAM
      ENDIF
      GOTO 1399
C
19    CONTINUE
C NAME CARD READ
      IF( SWMSG )PRINT *,SECTN,PRBNAM
CC ADDED 7-6-95
      IF( PRBNAM.EQ.' ' )THEN
         PRBNAM = 'NoName'
         PRINT *,' Warning: problem name is blank...set = ',PRBNAM(:8)
      ELSE IF( PRBNAM(1:1).EQ.' ' )THEN
         CALL FSQUEZ(PRBNAM,8)
         PRINT *,' Warning: problem name did not begin in column 15',
     1           ' ...Reset = ',PRBNAM
      ENDIF
CC ===========
C SET FREQUENCY CHECK
      FREQCH = 200
      FREQCH = 1000
C ========= BEGIN ROWS SECTION =========
      SECTN  = ROWSEC
      IRNAME = 0
C SKIP TO 'ROWS' CARD
50    READ(DATFIL,1,END=1380,ERR=1313)SETNAM
      LINE = LINE+1
      IF(SETNAM.NE.SECTN)GOTO 50
C
C INITIALIZE ROW LINKS...USING BOTTOM OF VALUE ARRAY (I4VAL)
      LLINK  = MAXVAL - MAXRC
      RLINK  = LLINK  - MAXRC
      IRINFO = IFREE
C ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C  Rows will be put into binary tree, right-threaded for inorder
C  traversal to obtain sorted list.  During rows section, IRINFO
C  indexes are row types; after completion, these will be row counts.
C ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C INITIALIZE NAME VARIABLES (TO BE READ)
      CNAME = ' '
      RNAME1= ' '
      CALL GPTNAM(IRNAME,CNAME)
      I4VAL(LLINK) = 0
      I4VAL(RLINK) = 0
C INITIALIZE FREQUENCY
      CHKFRQ = FREQCH
C INITIALIZE FIRST AND LAST NAMES
      FNAME = '~'
      LNAME = ' '
      BOTTOM= 0
      TOP   = 0
C
C       ::: TOP OF ROWS SECTION READIN LOOP :::
C
100   READ(DATFIL,101,END=1380,ERR=1313)CHR1,BNDTYP,RNAME1
101   FORMAT(A1,A2,T5,A10)
      LINE = LINE+1
      IF(CHR1.EQ.'*')GOTO 100
      IF(CHR1.EQ.COLSEC(1:1))GOTO 190
      IF(CHR1.NE.' ')GOTO 1310
C CHECK ROW NAME LENGTH
      IF( RNAME1(NAMELN+1:).NE.' ' )THEN
         PRINT *,' ** ROW ',RNAME1,' EXCEEDS NAMELN =',NAMELN
         GOTO 1390
      ENDIF
C SET ROW TYPE (MAY BE IN COL 2 OR 3)
      ROWTYP = BNDTYP(1:1)
      IF(ROWTYP.EQ.' ')ROWTYP = BNDTYP(2:2)
      NROWS = NROWS + 1
      IF( NROWS.GT.MXNROW ) GOTO 185
C
      CHKFRQ = CHKFRQ-1
      IF(CHKFRQ.LE.0 .AND. SWMSG)THEN
         PRINT *,' ROW',NROWS,': ',RNAME1(:NAMELN),' TYPE=',ROWTYP
         CHKFRQ = FREQCH
      ENDIF
C
C ENTER TYPE INTO ROW'S INFO (TEMPORARILY)
C
      IF(ROWTYP.EQ.'N' .OR. ROWTYP.EQ.'n')THEN
          INDEX(IRINFO + NROWS) = 0
          NRFREE = NRFREE + 1
C  SET OBJ IF FIRST FREE ROW
          IF(OBJNAM.EQ.' ')OBJNAM = RNAME1
      ELSE IF(ROWTYP.EQ.'G' .OR. ROWTYP.EQ.'g')THEN
          INDEX(IRINFO + NROWS) = 1
      ELSE IF(ROWTYP.EQ.'L' .OR. ROWTYP.EQ.'l')THEN
          INDEX(IRINFO + NROWS) = 2
      ELSE IF(ROWTYP.EQ.'E' .OR. ROWTYP.EQ.'e')THEN
          INDEX(IRINFO + NROWS) = 3
      ELSE
          PRINT *,' ** ROW TYPE ',ROWTYP,' NOT RECOGNIZED'
          GOTO 1390
      ENDIF
C
C LOCATE ROW IN TREE (AND TEST FOR DUPLICATE)
C
C ............................................................
C : The following is Milt Gutterman's improvement for tree   :
C : insertion (from GTREIN in LPRENSRT.FOR).                 :
C : Speedup occurs if names are already in sort order.       :
C :..........................................................:
C IS NEW NAME LAST?
      IF(RNAME1.GT.LNAME)THEN
C YES, INSERT TO RIGHT OF BOTTOM (AND UPDATE LNAME)
         LNAME = RNAME1
         I4VAL(RLINK + NROWS)  = I4VAL(RLINK + BOTTOM)
         I4VAL(RLINK + BOTTOM) = NROWS
         BOTTOM= NROWS
         IF(NROWS.EQ.1)THEN
            FNAME = RNAME1
            TOP   = 1
         ENDIF
         GOTO 180
      ENDIF
C IS NEW NAME FIRST?
      IF(RNAME1.LT.FNAME)THEN
C YES, INSERT TO LEFT OF TOP (AND UPDATE FNAME)
         FNAME = RNAME1
         I4VAL(LLINK + TOP)    = NROWS
         I4VAL(RLINK + NROWS)  = -TOP
         TOP   = NROWS
         IF(NROWS.EQ.1)THEN
            LNAME = RNAME1
            BOTTOM= 1
         ENDIF
         GOTO 180
      ENDIF
C ............................................................
C : End of Milt's improvement                                :
C :..........................................................:
      PARENT = 0
C ...LOOP TO LOCATE AND INSERT
150   IF(RNAME1.EQ.NAME(IRNAME+PARENT))THEN
          PRINT *,' Duplicate row...',RNAME1,' ...ignored'
          IF(INDEX(IRINFO + NROWS).EQ.0)NRFREE = NRFREE - 1
          NROWS = NROWS - 1
          GOTO 100
      ELSE IF(RNAME1.LT.NAME(IRNAME + PARENT))THEN
C LOOK LEFT
          LOC = I4VAL(LLINK + PARENT)
          IF(LOC.GT.0)THEN
C ...MOVE LEFT
             PARENT = LOC
             GOTO 150
          ELSE
C ...INSERT LEFT
             I4VAL(LLINK + PARENT) = NROWS
             I4VAL(RLINK + NROWS)  = -PARENT
          ENDIF
      ELSE
C LOOK RIGHT
          LOC = I4VAL(RLINK + PARENT)
          IF(LOC.GT.0)THEN
C ...MOVE RIGHT
             PARENT = LOC
             GOTO 150
          ELSE
C ...INSERT RIGHT
             I4VAL(RLINK + NROWS) = I4VAL(RLINK + PARENT)
             I4VAL(RLINK + PARENT) = NROWS
          ENDIF
      ENDIF
C
C FINISH PUTTING ROW IN TREE
180   CONTINUE
      I4VAL(LLINK + NROWS) = 0
      CALL GPTNAM(IRNAME + NROWS,RNAME1)
C
C CHECK FOR OVERFLOW
C
      IF(NROWS.LT.MAXRC - 2)GOTO 100
C
185   CONTINUE
      PRINT *,' ** ',NROWS,' ARE TOO MANY ROWS'
      GOTO 1390
C
C   ::: END ROWS SECTION :::
C
190   CONTINUE
C
C   LAST CARD HAD 'C' (or 'c') IN COLUMN 1...ASSUME COLUMNS SECTION
C
C CHECK THAT WE GOT A FREE ROW (OBJ)
      IF( NRFREE.EQ.0 )THEN
         PRINT *,' ** NO FREE ROW (N-type)...ERROR ASSUMED'
         GOTO 1390
      ENDIF
C
C CHECK THAT WE GOT A NONFREE ROW
      IF( NRFREE.EQ.NROWS )THEN
         PRINT *,' ** NO NON-FREE ROW...ERROR ASSUMED'
         GOTO 1390
      ENDIF
C
      IF(SWMSG)PRINT *,NROWS,' rows...',NRFREE,' free'
C
C       WRITE SORTED ROW LIST TO PCKFIL
C       --------------------------------
C
      CLOSE(PCKFIL)
C UNIX AND XENIX NEED RECORD LENGTHS (RECL) FOR UNFORMATTED FILES
CIXNX      OPEN(PCKFIL,STATUS='SCRATCH',FORM='UNFORMATTED',ERR=1350,
CIXNX     1     RECL=2048)
CIXNX      GOTO 222
CIEIAC   FILEINF NEEDED FOR SCRATCH FILE TO PROVIDE ENOUGH SPACE
CIEIA      CALL FILEINF(RCODE,'TRK',50,'SECOND',100,'RECFM','VBS',
CIEIA     1             'LRECL',-1,'BLKSIZE',32760)
      OPEN(PCKFIL,STATUS='SCRATCH',FORM='UNFORMATTED',ERR=1350)
222   REWIND(PCKFIL)
      NCHECK = NROWS
C
C     TRAVERSE ROWS INORDER
C
      ROW=I4VAL(LLINK)
      IF(ROW.EQ.0)GOTO 1950
C   MOVE DOWN LEFT LEG OF TREE
1910  IF(I4VAL(LLINK+ROW).GT.0)THEN
          ROW=I4VAL(LLINK+ROW)
          GOTO 1910
      ENDIF
C
C     WRITE ROW
C    -----------
1920  WRITE(PCKFIL,ERR=1360)NAME(IRNAME+ROW),INDEX(IRINFO+ROW)
      NCHECK = NCHECK - 1
C
C GET INORDER SUCCESSOR OF ROW
C
1950  IF(I4VAL(RLINK+ROW).LT.0)THEN
          ROW = -I4VAL(RLINK+ROW)
          GOTO 1920
      ELSE IF(I4VAL(RLINK+ROW).GT.0)THEN
          ROW = I4VAL(RLINK+ROW)
          GOTO 1910
      ENDIF
C
C NOW WE HAVE WRITTEN LAST ROW...RIGHT LINK = 0
C
      IF(NCHECK.NE.0)THEN
          PRINT *,' **',NCHECK
          I = ROW
          GOTO 1355
      ENDIF
C
C SET POINTERS, KNOWING NROWS
C
      IRLBND = IRINFO + NROWS
      IRUBND = IRLBND + NROWS
      IRSTAT = IRUBND + NROWS
      ICINFO = IRSTAT + NROWS + 1
C
      ICNAME = IRNAME + NROWS + 1
      CNAME = ' '
      CALL GPTNAM(ICNAME,CNAME)
C
C WRITE A DUMMY RECORD TO AVOID EOF
      DUMMY = 0.
      WRITE(PCKFIL,ERR=1360)OBJNAM,DUMMY
C
C READ ROWS BACK (SORTED)
C
      REWIND(PCKFIL)
C
      DO 1989 I=1,NROWS
         READ(PCKFIL,ERR=1355,END=1359)RNAME1,INDEX(IRINFO+I)
         LINE = LINE+1
         CALL GPTNAM(IRNAME+I,RNAME1)
1989  CONTINUE
C (DO LOOP USED BECAUSE SOME COMPILERS DO NOT WORK WITH IMPLIED LOOP)
C
C SET ROW BOUNDS AND INITIALIZE COUNTS
C
      OBJNUM = 0
      DO 1990 ROW=1,NROWS
           TYP=INDEX(IRINFO+ROW)
           INDEX(IRINFO+ROW)=0
           IF(TYP.EQ.0)THEN
               INDEX(IRLBND+ROW)=-INF
               INDEX(IRUBND+ROW)= INF
C            SEE IF FREE ROW MATCHES OBJ NAME
               IF(OBJNAM.EQ.NAME(IRNAME+ROW))OBJNUM = ROW
C
           ELSE IF(TYP.EQ.1)THEN
               INDEX(IRLBND+ROW)= 0
               INDEX(IRUBND+ROW)= INF
           ELSE IF(TYP.EQ.2)THEN
               INDEX(IRLBND+ROW)=-INF
               INDEX(IRUBND+ROW)= 0
           ELSE
               INDEX(IRLBND+ROW)= 0
               INDEX(IRUBND+ROW)= 0
           ENDIF
1990   CONTINUE
C
C CHECK THAT OBJ WAS SET
      IF(OBJNUM.EQ.0)THEN
          PRINT *,' Objective row ',OBJNAM,' not in matrix file'
          GOTO 1399
      ENDIF
C
C PREPARE FOR COLUMNS SECTION
      MAXCOL = MAXRC - NROWS - 1
      IF( MAXCOL.GT.MXNCOL )MAXCOL = MXNCOL
      I4VAL(LLINK) = 0
      I4VAL(RLINK) = 0
      IFREE = ICINFO + MAXCOL + 1
      SECTN  = COLSEC
      SOSNAM = ' '
C
      CHKFRQ = FREQCH
C INITIALIZE CURRENT COLUMN NAME (SETNAM)
      SETNAM=' '
      MAXLEN=0
C INITIALIZE FIRST AND LAST NAMES
      FNAME = '~'
      LNAME = ' '
      TOP   = 0
      BOTTOM= 0
C
C NONZERO INDEXES GROW FROM BOTTOM OF INDEX
      INONZ=MAXIND
C
C READ NONZEROES IN COLUMNS SECTION
200   CNAME  = ' '
      RNAME2 = ' '
      RNAME1 = ' '
      READ(DATFIL,201,END=1380,ERR=1313)CHR1,CNAME,
     1    RNAME1,V1,RNAME2,V2
201   FORMAT(A1,T5,A10,T15,A10, G12.5,T40,A10, G12.5)
CC                             :              :...REMOVED T50 2-5-94
CC                             :...REMOVED T25 2-5-94
      LINE = LINE+1
      IF(CHR1.EQ.'*')GOTO 200
      IF(CHR1.EQ.RNGSEC(1:1))GOTO 290
      IF(CHR1.NE.' ')GOTO 1310
C CHECK NAME LENGTH
      IF( CNAME(NAMELN+1:).NE.' ' )THEN
         PRINT *,' ** COLUMN ',CNAME,' EXCEEDS NAME LENGTH =',
     1           NAMELN
         GOTO 1390
      ENDIF
      IF(RNAME1(1:8).EQ.MARKER)THEN
         IF(RNAME2(1:8).EQ.SOSEND)THEN
            SOSNAM = ' '
         ELSE IF(RNAME2(1:8).EQ.SOSORG)THEN
            SOSNAM = CNAME
C        ELSE ...IGNORE MARKER
         ENDIF
         GOTO 200
      ENDIF
      IF(CNAME.EQ.' ')CNAME = SETNAM
      IF( CNAME.NE.SETNAM )THEN
C ========== NEW COLUMN ===============
         IF( NCOLS.EQ.MAXCOL )THEN
            PRINT *,' **',NCOLS,' ARE TOO MANY COLUMNS'
            GOTO 1390
         ENDIF
         NCOLS = NCOLS+1
C CHECK FREQUENCY OF LETTING USER KNOW WE'RE HERE
         CHKFRQ = CHKFRQ - 1
         IF(CHKFRQ.LE.0 .AND. SWMSG)THEN
            PRINT *,' COLUMN',NCOLS,': ',CNAME(:NAMELN),
     1              '...', NONZER,' NONZEROES SO FAR'
            CHKFRQ = FREQCH
         ENDIF
C OK, LET'S MOVE AHEAD
         SETNAM = CNAME
         BASE(IFREE+NCOLS)=INONZ
C        :...long integer to hold the beginning of column's
C             nonzeroes (INONZ)...INFO holds count until sorted
C INSERT INTO TREE (SEE ROWS SECTION FOR DETAILS)
C ............................................................
C : The following is Milt Gutterman's improvement for tree   :
C : insertion (from GTREIN in LPRENSRT.FOR).                 :
C : Speedup occurs if names are already in sort order.       :
C :..........................................................:
C IS NEW NAME LAST?
         IF( CNAME.GT.LNAME )THEN
C YES, INSERT TO RIGHT OF BOTTOM (AND UPDATE LNAME)
            LNAME = CNAME
            I4VAL(RLINK + NCOLS)  = I4VAL(RLINK + BOTTOM)
            I4VAL(RLINK + BOTTOM) = NCOLS
            BOTTOM= NCOLS
            IF(NCOLS.EQ.1)THEN
               FNAME = CNAME
               TOP   = 1
            ENDIF
            GOTO 280
         ENDIF
C IS NEW NAME FIRST?
         IF(CNAME.LT.FNAME)THEN
C YES, INSERT TO LEFT OF TOP (AND UPDATE FNAME)
            FNAME = CNAME
            I4VAL(LLINK + TOP)    = NCOLS
            I4VAL(RLINK + NCOLS)  = -TOP
            TOP   = NCOLS
            IF(NCOLS.EQ.1)THEN
               LNAME = CNAME
               BOTTOM= 1
            ENDIF
            GOTO 280
         ENDIF
C ............................................................
C : End of Milt's improvement                                :
C :..........................................................:
         PARENT = 0
250      CONTINUE
           IF( CNAME.EQ.NAME(ICNAME+PARENT) )THEN
              PRINT *,' ** COLUMN ',CNAME(:NAMELN),' IS OUT OF ORDER'
              GOTO 1390
           ELSE IF( CNAME.LT.NAME(ICNAME+PARENT) )THEN
              IF( I4VAL(LLINK+PARENT).GT.0 )THEN
                   PARENT=I4VAL(LLINK+PARENT)
                   GOTO 250
              ELSE
                   I4VAL(LLINK+PARENT)=NCOLS
                   I4VAL(RLINK+NCOLS)=-PARENT
              ENDIF
           ELSE
              IF(I4VAL(RLINK+PARENT).GT.0)THEN
                   PARENT=I4VAL(RLINK+PARENT)
                   GOTO 250
              ELSE
                   I4VAL(RLINK+NCOLS)=I4VAL(RLINK+PARENT)
                   I4VAL(RLINK+PARENT)=NCOLS
              ENDIF
           ENDIF
C FALL THRU MEANS WE FOUND LOCATION TO INSERT
280      CONTINUE
C ...FINISH INSERTION
         I4VAL(LLINK+NCOLS)=0
         CALL GPTNAM(ICNAME+NCOLS,CNAME)
         BASE(ICINFO+NCOLS)=0
C IS THIS IN AN SOS?
         IF(SOSNAM.NE.' ')THEN
C -YES, SO PUT 1. INTO SOS ROW
            V = 1.
            CALL GADDNZ(SOSNAM,V,VTZERO,RCODE)
            IF( RCODE.GT.0 )GOTO 1370
            IF( RCODE.LT.0 )THEN
               PRINT *,' ** SYSERR...SOS ',SOSNAM,V
               GOTO 1390
            ENDIF
         ENDIF
C === END OF INITIALIZATION OF NEW COLUMN ===
      ENDIF
C
C ADD FIRST NONZERO FROM INPUT LINE
      CALL GADDNZ(RNAME1,V1,VTZERO,RCODE)
      IF( RCODE.GT.0 )GOTO 1370
         IF( RCODE.LT.0 )THEN
C SUPPRESS MESSAGE IF REALLY ZERO OR IF MANY MESSAGES HAVE BEEN ISSUED
            IF( ABS(V1).GT.0 .AND. NWARNV.GT.0 )THEN
               NWARNV = NWARNV-1
               PRINT *,' Warning: coefficient of ',RNAME1(:NAMELN),
     1                 ' in column ',CNAME(:NAMELN)
               PRINT *,'        =',V1,'...too small to add'
            ENDIF
            RCODE = 0
         ENDIF
C ADD SECOND NONZERO, IF PRESENT
      IF(RNAME2.EQ.' ')GOTO 200
C ...IT IS
      CALL GADDNZ(RNAME2,V2,VTZERO,RCODE)
      IF( RCODE )275,200,1370
275   IF( ABS(V2).GT.0 .AND. NWARNV.GT.0 )THEN
         NWARNV = NWARNV-1
         PRINT *,' Warning: coefficient of ',RNAME2(:NAMELN),
     1           '=',V2,'...too small to add'
      ENDIF
      RCODE = 0
      GOTO 200
C
C       ::: END COLUMNS SECTION :::
290   CONTINUE
      IF(SWMSG)PRINT *,NCOLS,' columns...',NONZER,' nonzeroes'
C
C SET COLUMN POINTERS, NOW THAT NCOLS IS KNOWN
      ICLBND = ICINFO + NCOLS + 1
      ICUBND = ICLBND + NCOLS
      ICSTAT = ICUBND + NCOLS
C   ...AND FIRST NONZERO LOCATION
      INONZ  = ICSTAT + NCOLS + 1
C
C TRAVERSE COLUMN TREE INORDER, WRITING NAME, INFO AND NONZERO LIST
      REWIND(PCKFIL)
      COLUMN = I4VAL(LLINK)
      IF( COLUMN.EQ.0 )GOTO 2950
C
C MOVE DOWN LEFT LEG
2910  CONTINUE
        IF( I4VAL(LLINK+COLUMN).LE.0 )GOTO 2920
        COLUMN = I4VAL(LLINK+COLUMN)
      GOTO 2910
C
C WRITE COLUMN NAME AND COUNT FIRST
2920  CONTINUE
      NZ=BASE(ICINFO+COLUMN)
      WRITE(PCKFIL,ERR=1360)NAME(ICNAME+COLUMN),NZ
C ...WRITE NONZEROES
      IF(NZ.GT.0)THEN
                I1=BASE(IFREE+COLUMN)
                I2=I1 - 2*NZ + 1
                WRITE(PCKFIL,ERR=1361)(INDEX(I),I=I1,I2,-1)
      ENDIF
C
C GET INORDER SUCCESSOR OF COLUMN
2950  IF(I4VAL(RLINK+COLUMN).LT.0)THEN
                COLUMN=-I4VAL(RLINK+COLUMN)
                GOTO 2920
      ELSE IF(I4VAL(RLINK+COLUMN).GT.0)THEN
                COLUMN=I4VAL(RLINK+COLUMN)
                GOTO 2910
      ENDIF
C
C       ::: TRAVERSAL COMPLETE ...RIGHT LINK = 0 :::
C
C WRITE A DUMMY RECORD TO AVOID EOF
      RNAME1 = '_END'
      WRITE(PCKFIL,ERR=1360)RNAME1,NZ
C
C READ BACK SORTED COLUMN LIST AND SET INDEX POINTERS INTO INFO
C
      REWIND(PCKFIL)
C
       MAXLEN=0
       I1=INONZ
       BASE(ICINFO)=0
C
       DO 2980 COLUMN=1,NCOLS
              READ(PCKFIL,ERR=1356,END=1359)CNAME,NZ
              LINE = LINE+1
              IF(CNAME.EQ.RNAME1)GOTO 2981
              CALL GPTNAM(ICNAME+COLUMN,CNAME)
              IF(NZ.GT.0)THEN
                I2 = I1 + 2*NZ - 1
                READ(PCKFIL,ERR=1357,END=1359)(INDEX(I),I=I1,I2)
                LINE = LINE+1
                I1=I2+1
              ENDIF
C
              BASE(ICINFO+COLUMN)=BASE(ICINFO+COLUMN-1)+NZ
C       ...INITIALIZE BOUNDS AND UPDATE MAX LENGTH
              INDEX(ICLBND+COLUMN)=0
              INDEX(ICUBND+COLUMN)=INF
              IF(MAXLEN.LT.NZ)THEN
                 MAXLEN=NZ
                 CMAXLN=COLUMN
              ENDIF
2980   CONTINUE
2981   CONTINUE
C ======== COLUMNS NOW RESIDE IN SORT ORDER ==========
C
C PREPARE FOR RHS SECTION
       IFREE = INONZ + 2*NONZER + 1
       SECTN = RHSSEC
       CNAME = RHSNAM
       RNAME1 = ' '
       RNAME2 = ' '
C
300   READ(DATFIL,301,END=1300,ERR=1313)CHR1,SETNAM,
     1    RNAME1,V1,RNAME2,V2
301   FORMAT(A1,T5,A10, A10,T25,G13.5,T40,A10, G13.5)
      LINE = LINE+1
        IF(CHR1.EQ.'*')GOTO 300
        IF(CHR1.NE.' ')GOTO 390
C
C CHECK RHS SETNAME
        IF(CNAME.EQ.'  ')CNAME = SETNAM
        IF(CNAME.NE.SETNAM)THEN
C SEE IF THIS IS FIRST ONE...TO BE SET NOW
           IF(CNAME.NE.'none')GOTO 300
C ...IT IS...SET RHS NAME
           CNAME  = SETNAM
           RHSNAM = SETNAM
        ENDIF
C
C LOOKUP ROW
350     CALL GETNUM('ROW ',RNAME1,NUMBER)
          IF( NUMBER.EQ.0 )GOTO 1371
C GET VALUE INDEX
          IF( V1.LT.0.0 )THEN
             V = -V1
             SIGN = -1
          ELSE
             V = V1
             SIGN = 1
          ENDIF
C
          ID=0
          IF( ABS(V).GT.VTOLAB )THEN
             CALL GETVID(V,ID)
             IF( ID.EQ.0 )GOTO 1370
          ENDIF
C
C OVERWRITE 0 BOUNDS IN ROW NUMBER (NONE ===> FREE ROW, NOT ADDED)
C
          IF( INDEX(IRLBND+NUMBER).EQ.0 )INDEX(IRLBND+NUMBER)=ID*SIGN
          IF( INDEX(IRUBND+NUMBER).EQ.0 )INDEX(IRUBND+NUMBER)=ID*SIGN
          IF(RNAME2.NE.' ')THEN
C ADD 2ND RHS
             RNAME1 = RNAME2
             RNAME2 = ' '
             V1 = V2
             GOTO 350
          ENDIF
C
C GO GET NEXT RHS CARD INPUT
      GOTO 300
C =============== END RHS SECTION ================
390   CONTINUE
      IF( SWMSG )PRINT *,' RHS section complete...NAME = ',CNAME
      IF( CHR1.EQ.RNGSEC(1:1) )GOTO 400
      IF( CHR1.EQ.BNDSEC(1:1) )GOTO 500
      IF( CHR1.EQ.'E' .OR. CHR1.EQ.'e' )GOTO 900
      GOTO 1310
C
C ===== BEGIN RANGE SECTION =====
400      SECTN  = RNGSEC
         CNAME  = RNGNAM
         ERRNAM = ' '
C
410      READ(DATFIL,301,END=1300,ERR=1313)CHR1,SETNAM,
     1       RNAME1,V,RNAME2,V2
         LINE = LINE+1
         IF(CHR1.EQ.'*')GOTO 410
         IF(CHR1.NE.' ')GOTO 490
C
C CHECK SETNAME
         IF(CNAME.EQ.' ')CNAME = SETNAM
         IF(CNAME.NE.SETNAM)THEN
C ...IS THIS FIRST ONE TO BE ENTERED NOW?
                IF(CNAME.NE.'none')GOTO 410
C       YES...
                CNAME=SETNAM
                RNGNAM=SETNAM
         ENDIF
C
C LOOKUP ROW
C
450      CALL GETNUM('ROW ',RNAME1,NUMBER)
         IF(NUMBER.EQ.0)THEN
C ROW NOT DEFINED
            ERRNAM = RNAME1
            GOTO 410
         ENDIF
C
C GET BOUND INDEX TO BE CHANGED...
C
C    ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C    If L = -INF, it must be that U < INF (free row cannot be
C            ranged).  In this case, L becomes U-v.
C    If U = INF, it must be that L > -INF.  In this case,
C            U becomes L+v.
C    If L = U, we adjust to L+v or U+v according to v<0 or v>0, resp.
C    Error returns if:
C       -INF < L < U < INF (we must have already ranged this row)
C       -INF = L & U = INF  (free row)
C    ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
         CALL GETRIM('ROW ',NUMBER,VL,VU,VC,NZ)
         IF(VL.LE.-VINF)THEN
            IF(VU.GE.VINF)THEN
                   PRINT *,' Attempt to range free row ',RNAME1,
     1                     '...ignored'
                   GOTO 410
            ENDIF
            V = VU-ABS(V)
            I = IRLBND+NUMBER
         ELSE IF(VU.LT.VINF)THEN
                IF(VL.LT.VU)THEN
                   PRINT *,' Row ',RNAME1,' has second range'
                   GOTO 1310
                ENDIF
C     RANGE EQUATION...SIGN DETERMINES ADJUSTMENT
                IF(V.LT.0.0)THEN
                   V = VL + V
                   I = IRLBND + NUMBER
                ELSE
                   V = VU + V
                   I = IRUBND + NUMBER
                ENDIF
         ELSE
                V = VL+ABS(V)
                I = IRUBND+NUMBER
         ENDIF
C
C GET VALUE INDEX OF V TO ENTER INTO INDEX(I)
         IF( V.LT.0. )THEN
                V = -V
                SIGN = -1
         ELSE IF( V.GT.0. )THEN
                SIGN = 1
         ENDIF
C
         IF(V.LT.VTOLAB)THEN
                ID = 0
         ELSE
                CALL GETVID(V,ID)
                IF(ID.EQ.0)GOTO 1310
                ID = SIGN*ID
         ENDIF
C
         INDEX(I) = ID
         IF(RNAME2.NE.' ')THEN
C    ADD 2ND RANGE
            RNAME1 = RNAME2
            RNAME2 = ' '
            V = V2
            GOTO 450
         ENDIF
C
C GO GET NEXT RANGE ENTRY
         GOTO 410
C        ========
C  ::: END RANGE SECTION :::
C
490      CONTINUE
         IF(ERRNAM.NE.' ')THEN
            PRINT *,' Warning: some rows in RANGE section,',
     1              ' like ',ERRNAM(:NAMELN),', were not defined.'
         ENDIF
         IF(SWMSG)PRINT *,' RANGE complete...NAME = ',CNAME
C
         IF(CHR1.EQ.'E' .OR. CHR1.EQ.'e')GOTO 900
         IF(CHR1.NE.BNDSEC(1:1))GOTO 1310
C
C BOUNDS SECTION
500      CONTINUE
         SECTN=BNDSEC
         CNAME=BNDNAM
         ERRNAM = ' '
C
510    READ(DATFIL,511,END=1300,ERR=1313)CHR1,BNDTYP,SETNAM,RNAME1,V
511    FORMAT(A1,T2,A2,T5,A10, A10, G13.5)
CC  2-5-94                    :    :  :...CHANGED FROM G12.5
CC                            :    :...REMOVED T25
CC                            :...REMOVED T15
       LINE = LINE+1
       IF(CHR1.EQ.'*')GOTO 510
       IF(CHR1.NE.' ')GOTO 590
C
C CHECK BOUNDSET NAME
         IF(CNAME.EQ.' ')CNAME = SETNAM
         IF(CNAME.NE.SETNAM)THEN
                IF(CNAME.NE.'none')GOTO 510
                CNAME=SETNAM
                BNDNAM=SETNAM
         ENDIF
C
C LOOKUP COLUMN
C
         CALL GETNUM('COL ',RNAME1,NUMBER)
         IF(NUMBER.EQ.0)THEN
C COLUMN NOT DEFINED
            ERRNAM = RNAME1
            GOTO 510
         ENDIF
C SET IF FREE COLUMN WITHOUT V
         IF(BNDTYP.EQ.'MI'.OR.BNDTYP.EQ.'FR' .OR.
     1      BNDTYP.EQ.'mi'.OR.BNDTYP.EQ.'fr')THEN
                INDEX(ICLBND+NUMBER)=-INF
                GOTO 510
         ENDIF
C BOUND TYPE 'PL' IS JUST VANILLA (IGNORE)
         IF(BNDTYP.EQ.'PL' .OR. BNDTYP.EQ.'pl')GOTO 510
C BOUND TYPE 'BV' ACCOMMODATES FORTLP SPEC FOR BINARY VARIABLE (0-1)
         IF(BNDTYP.EQ.'BV' .OR. BNDTYP.EQ.'bv')THEN
                INDEX(ICUBND+NUMBER)=1
                GOTO 510
         ENDIF
C
C BND TYPE IS UP,LO,FX...SET VALUE INDEX
         IF( V.LT.0.0 )THEN
            V=-V
            SIGN=-1
         ELSE IF(V.GT.0.0)THEN
            SIGN= 1
         ENDIF
         IF( V.LT.VTOLAB )THEN
            ID=0
         ELSE
            CALL GETVID(V,ID)
            IF(ID.EQ.0)GOTO 1370
         ENDIF
C
         IF(BNDTYP.EQ.'UP' .OR. BNDTYP.EQ.'up')THEN
                INDEX(ICUBND+NUMBER)=ID*SIGN
         ELSE IF(BNDTYP.EQ.'LO' .OR. BNDTYP.EQ.'lo')THEN
                INDEX(ICLBND+NUMBER)=ID*SIGN
         ELSE IF(BNDTYP.EQ.'FX' .OR. BNDTYP.EQ.'fx')THEN
                INDEX(ICLBND+NUMBER)=ID*SIGN
                INDEX(ICUBND+NUMBER)=ID*SIGN
                NCFIX = NCFIX + 1
         ELSE
                PRINT *,' ** BOUND TYPE ',BNDTYP,' NOT RECOGNIZED'
                GOTO 1390
         ENDIF
C
C GO GET NEXT BOUND
         GOTO 510
C ========= END BOUNDS SECTION ===========
590      CONTINUE
         IF(ERRNAM.NE.' ')THEN
            PRINT *,' Warning: some columns in BOUNDS section,',
     1              ' like ',ERRNAM(:NAMELN),',were not defined.'
         ENDIF
         IF(SWMSG)PRINT *,' BOUNDS complete...NAME = ',CNAME
C
         IF(CHR1.EQ.RNGSEC(1:1))GOTO 400
         IF(CHR1.NE.'E' .AND. CHR1.NE.'e')GOTO 1310
C
C    ::: DONE :::
900   CONTINUE
      IF( SWMSG )PRINT *,' Matrix read in '
C
C SET VALUE POINTERS FOR SOLUTION AND FREE SPACE
      IRSOLV = NVALS
      ICSOLV = IRSOLV + NROWS
      IVFREE = ICSOLV + NCOLS + 1
      ENDNAM = (ICNAME + NCOLS)*NAMELN
C
C SET ROW WITH MAX LENGTH (RMAXLN)
      MAXL = 0
      DO 990 I=1,NROWS
         IF(   (INDEX(IRLBND+I).EQ.-INF .AND. INDEX(IRUBND+I).EQ.INF)
     1    .OR. (INDEX(IRINFO+I).LE.MAXL) )GOTO 990
C NOT FREE ROW AND LENGTH IS NEW MAX
         RMAXLN = I
         MAXL   = INDEX(IRINFO+I)
990   CONTINUE
C
      GOTO 9000
C
C ** ERROR RETURNS
1300  PRINT *,' ** MISSING ',SECTN
      GOTO 1390
1310  PRINT *,' ** ',CHR1,' NOT EXPECTED'
      GOTO 1390
1313  PRINT *,' ** IO ERROR READING MATRIX FILE'
      GOTO 1390
C
C  ERRORS READING PCKFIL
1350  PRINT *,' ** ERROR OPENING SCRATCH FILE...POSSIBLE OS ERROR'
      GOTO 1399
1355  PRINT *,' ** SYSIOERR...R5',I
      GOTO 1390
1356  PRINT *,' ** SYSIOERR...R6',COLUMN
      GOTO 1390
1357  PRINT *,' ** SYSIOERR...R7',I
      GOTO 1390
1359  PRINT *,' ** SYSIOERR...EOF'
      GOTO 1390
1360  PRINT *,' ** SYSIOERR...W0',ROW
      GOTO 1390
1361  PRINT *,' ** SYSIOERR...W1',I
      GOTO 1390
1370  PRINT *,' ** COULD NOT ADD NONZERO',V
      GOTO 1390
1371  PRINT *,' ** COULD NOT FIND ROW ',RNAME1
      GOTO 1390
1380  PRINT *,' ** PREMATURE END OF FILE REACHED IN ',SECTN
      GOTO 1399
1390  CONTINUE
      BACKSPACE(DATFIL,ERR=1399)
      READ(DATFIL,1301,ERR=1399,END=1399)CHR80
      PRINT *,' ...Last line read is:'
1301  FORMAT(A79)
      PRINT *,' ',CHR80
C
C ALL ERROR RETURNS COME HERE
1399  RCODE = 1
      PRINT *,' ** READ MATRIX ABORTED AT LINE',LINE,
     1        '...SECTION=',SECTN
      IF( SWDBG )THEN
         PRINT *,' DATFIL,PCKFIL =',DATFIL,PCKFIL
         CALL GDEBUG
      ENDIF
C
C  ALL RETURNS COME HERE
9000  CLOSE(DATFIL)
      CLOSE(PCKFIL)
      IF(NWARNV.EQ.0)PRINT *,' Warning:',
     1   '  other coefficients may have been too small to add.'
      RETURN
C
C ** GETMAT ENDS HERE
      END
