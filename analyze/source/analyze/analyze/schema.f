C                  ::: SCHEMA.FOR  3-19-94 :::
C
C LAST DATE: 12-26-90...Fixed tabs in SCHREL
C             7-07-91...Changed update in SCHPUT
C                       Put safeguard in SCHRDP
C            10-05-91...Replaced OPEN with FLOPEN
C             6-03-92...Added SCHDEL
C             3-19-94...Added PRHEAD to calling sequence of SCHDSP
C
C This contains the SCHEMA routines (for MODLER and ANALYZE).
C  (Linking routine, ASCHMA or MSCHMA is with host system.)
C
C     SCHCLR.....clear schema
C     SCHPUT.....put entry into schema
C     SCHSUM.....write summary (dimensions)
C     SCHDSP.....display schema
C     SCHREL.....display in relational format
C     SCHWRP.....write packed schema (unformatted)
C     SCHRDP.....read  packed schema (unformatted)
C     SCHFIL.....open schema file
C     SCHDEL.....delete ROW|COL class
C     SCHDBG.....debug
C
      SUBROUTINE SCHCLR(NR,NC,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCSCHEMA
C
C This clears schema and prepares for a new one having NR row classes
C and NC column classes.
C ...Alternate return is if there is not enough space allocated.
C
C NR=NC=0 MEANS CALLER JUST WANTS TO SEE IF WE'RE HERE
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF(NR.GT.PMSROW .OR. NC.GT.PMSCOL)THEN
         PRINT *,' Sorry, not enough space has been allocated',
     1           ' for schema'
         RETURN 1
      ENDIF
C
      SCHNR = NR
C             :....NUMBER OF ROW STRIPS
      SCHNC = NC
C             :....NUMBER OF COLUMN STRIPS
      SCHRLN= 0
C             :....GREATEST ROW NAME LENGTH
      SCHCLN= 0
C             :....GREATEST COLUMN NAME LENGTH
C
      IF(NR.EQ.0 .OR. NC.EQ.0)RETURN
C
C INITIALIZE STRINGS IN SCHEMA ARRAYS TO NULL
      DO 200 ROW=1,SCHNR
         SCHROW(ROW) = ' '
         DO 100 COL=1,SCHNC
100      SCHDAT(ROW,COL) = ' '
200   CONTINUE
C
      DO 300 COL=1,SCHNC
300   SCHCOL(COL) = ' '
C
      RETURN
C
C ** SCHCLR ENDS HERE
      END
      SUBROUTINE SCHPUT(ROWSTR,COLSTR,DATSTR,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCSCHEMA
C
C This puts into the schema the bi-strip:        COLSTR
C                                         ROWSTR DATSTR
C ...Alternate return is for SYSERR
C
      CHARACTER*(*) ROWSTR,COLSTR,DATSTR
      PARAMETER (PMSLEN=16)
C                :...MAX STRIP NAME LENGTH
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
C CHECK STRIP NAMES (CANNOT BE NULL)
      IF(ROWSTR.EQ.' '.OR.COLSTR.EQ.' ')THEN
         PRINT *,' ** ROW OR COL STRIP NAME IS NULL'
         RETURN 1
      ENDIF
C SEE IF ROWSTR HAS ALREADY BEEN ENTERED
      DO 100 ROW=1,SCHNR
         IF(ROWSTR.EQ.SCHROW(ROW))GOTO 110
         IF(SCHROW(ROW).EQ.' ')THEN
            SCHROW(ROW) = ROWSTR
C UPDATE NAME LENGTH
            CALL FSLEN(SCHROW(ROW),PMSLEN,L)
            IF(SCHRLN.LT.L)SCHRLN=L
            GOTO 110
         ENDIF
100   CONTINUE
C OH, OH
      PRINT *,' ** SYSERR IN SCHPUT...ROWSTR=',ROWSTR
      RETURN
C
110   CONTINUE
C SEE IF COLSTR HAS ALREADY BEEN ENTERED
      DO 200 COL=1,SCHNC
         IF(COLSTR.EQ.SCHCOL(COL))GOTO 210
         IF(SCHCOL(COL).EQ.' ')THEN
            SCHCOL(COL) = COLSTR
C UPDATE NAME LENGTH
            CALL FSLEN(SCHCOL(COL),PMSLEN,L)
            IF(SCHCLN.LT.L)SCHCLN=L
            GOTO 210
         ENDIF
200   CONTINUE
C OH, OH
      PRINT *,' ** SYSERR IN SCHPUT...COLSTR=',COLSTR
      RETURN
C
C OK, NOW WE HAVE THE ROW AND COLUMN STRIP NUMBERS
210   CONTINUE
C ...STORE THE DATA STRING (CAN BE NULL)
      SCHDAT(ROW,COL) = DATSTR
C UPDATE NAME LENGTH
      CALL FSLEN(SCHDAT(ROW,COL),PMSLEN,L)
      IF(SCHCLN.LT.L)SCHCLN=L
C
      RETURN
C
C ** SCHPUT ENDS HERE
      END
      SUBROUTINE SCHSUM
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCSCHEMA
C
C This writes schema dimensions...using FTEXT for output.
C
C LOCAL
      CHARACTER*80 CLIST
      CHARACTER*8  STR8
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      CLIST = 'Schema has'
      LAST = 12
      IF(SCHNR.EQ.1)THEN
         CLIST(LAST:) = '1 row strip'
      ELSE
         CALL FI2C(STR8,SCHNR,FIRST)
         CLIST(LAST:) = STR8(FIRST:)//' row strips'
      ENDIF
      CALL FSLEN(CLIST,30,LAST)
      IF(SCHNC.EQ.1)THEN
         CLIST(LAST+2:) = 'and 1 column strip.'
      ELSE
         CALL FI2C(STR8,SCHNC,FIRST)
         CLIST(LAST+2:) = 'and '//STR8(FIRST:)//' column strips.'
      ENDIF
      CALL FSLEN(CLIST,80,LAST)
      LINE = 1
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
900   RETURN
C
C ** SCHSUM ENDS HERE
      END
      SUBROUTINE SCHDSP(LINE,PRHEAD,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCSCHEMA.'
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCSCHEMA)
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCSCHEMA
CI$$INSERT DCFLIP
C
C This displays schema, calling FTEXT for output.
C ...RCODE=1 if screen width is too small for display.
C
      LOGICAL PRHEAD
C             :...=T IFF PRINT HEADER (1 LINE)
C LOCAL
      CHARACTER*128 CLIST
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF(SWFDBG)THEN
         PRINT *,' ENTERED SCHDSP WITH LINE=',LINE
         PRINT *,' SCHNR,SCHNC =',SCHNR,SCHNC
         PRINT *,' SCHRLN,SCHCLN =',SCHRLN,SCHCLN
         FPAUSE = FPAUSE-1
         IF(FPAUSE.LE.0)CALL FDEBUG
      ENDIF
C CALCULATE WIDTH NEEDED ON A PAGE
      PAGEWD = SCHRLN + 1 + (SCHCLN+1)*SCHNC
C              :                 :     :...Number of column strips
C              :                 :...Length of column strip
C              :...Length of row strip name
C
      IF(PAGEWD.LT.4)THEN
         PRINT *,' ** SYSERR DETECTED IN SCHDSP...',PAGEWD
         RETURN
      ENDIF
C
      IF(LINE.LE.0)LINE = 1
      IF(PAGEWD.GT.128)THEN
         IF(RCODE.NE.0)THEN
            PRINT *,' Sorry, width too great for rectangular format'
         ELSE
            CALL SCHREL(LINE)
         ENDIF
         RETURN
      ENDIF
C
      CLIST = ' '
      DO 20 K=SCHRLN+1,PAGEWD
20    CLIST(K:K) = ':'
      K = (SCHRLN + PAGEWD)/2 - 6
      IF(K.LE.0)K=1
      CLIST(K:K+13) = ' BLOCK SCHEMA '
C ADDED 8-9-94
      IF( PRHEAD )THEN
         LAST = PAGEWD
         CALL FTEXT(CLIST,LAST,1,0,LINE,'KEEP ',*900)
      ENDIF
      IF( PAGEWD.GT.SCRWTH )THEN
         IF( RCODE.NE.0 )THEN
            PRINT *,' Sorry, width too great for rectangular format'
         ELSE
            CALL SCHREL(LINE)
         ENDIF
         RETURN
      ENDIF
C
C OK, WE HAVE PRINTED BLOCK SCHEMA HEADING AND KNOW THE WIDTH OF THE
C     OUTPUT DEVICE IS ENOUGH (>= PAGEWD).
C
C DISPLAY COLUMN STRINGS...EACH COLUMN WIDTH = LONGEST NAME (SCHCLN)
      CLIST = ' '
      POINTR= SCHRLN+2
C
C ::: LOOP OVER COLUMNS TO DISPLAY HEADS OF SCHEMA :::
C
      DO 100 COL=1,SCHNC
         CLIST(POINTR:) = SCHCOL(COL)
         POINTR = POINTR + SCHCLN+1
100   CONTINUE
C
      LAST = PAGEWD
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
C  ::: LOOP OVER ROWS :::
C
      DO 500 ROW=1,SCHNR
         CLIST = SCHROW(ROW)
         POINTR= SCHRLN+2
C
         DO 400 COL=1,SCHNC
            CLIST(POINTR:) = SCHDAT(ROW,COL)
            POINTR = POINTR + SCHCLN+1
400      CONTINUE
C
         CALL FSLEN(CLIST,128,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
500   CONTINUE
C  ::: END LOOP OVER ROWS :::
C
      DO 800 K=1,PAGEWD
800   CLIST(K:K) = '-'
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
900   RETURN
C
C ** SCHDSP ENDS HERE
      END
      SUBROUTINE SCHREL(LINE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCSCHEMA
C
C This displays schema in relational format.
C
      CHARACTER*128 CLIST
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      CLIST = 'Row Strip'
      TABCOL= SCHRLN+2
      IF(TABCOL.LT.11)TABCOL=11
      CLIST(TABCOL:) = 'Column Strip'
      TABCEL = SCHCLN+2
      IF(TABCEL.LT.25)TABCEL = 25
      TABCEL = TABCEL+TABCOL
      CLIST(TABCEL:) = 'Cell'
      END = TABCEL+SCHCLN+1
      LAST = END
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
      DO 100 ROW=1,SCHNR
         CLIST = SCHROW(ROW)
      DO 100 COL=1,SCHNC
         CLIST(TABCOL:) = SCHCOL(COL)
         CLIST(TABCEL:) = SCHDAT(ROW,COL)
         LAST = END
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
100   CONTINUE
C
900   RETURN
C
C ** SCHREL ENDS HERE
      END
      SUBROUTINE SCHWRP(PCKFIL,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCSCHEMA
C
C This writes schema to (PCKFIL), presumed open (unformatted)
C
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF(SCHNR.LE.0 .OR. SCHNC.LE.0)THEN
         PRINT *,' NO SCHEMA IN MEMORY'
         RCODE = -1
         RETURN
      ENDIF
C
      WRITE(PCKFIL,ERR=1300)SCHNR,SCHNC,SCHRLN,SCHCLN
      WRITE(PCKFIL,ERR=1300)(SCHROW(I),I=1,SCHNR)
      WRITE(PCKFIL,ERR=1300)(SCHCOL(J),J=1,SCHNC)
      WRITE(PCKFIL,ERR=1300)( (SCHDAT(I,J),J=1,SCHNC), I=1,SCHNR)
      RETURN
C
1300  PRINT *,' ** I/O ERROR...UNIT',PCKFIL
      RCODE = 1
      RETURN
C
C ** SCHWRP ENDS HERE
      END
      SUBROUTINE SCHRDP(PCKFIL,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCSCHEMA
C
C This reads schema from (PCKFIL), presumed open (unformatted).
C
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      READ(PCKFIL,ERR=1300,END=1300)SCHNR,SCHNC,SCHRLN,SCHCLN
      IF(SCHNR.LE.0 .OR. SCHNR.GT.PMSROW .OR.
     1   SCHNC.LE.0 .OR. SCHNC.GT.PMSCOL)GOTO 1300
      READ(PCKFIL,ERR=1300,END=1300)(SCHROW(I),I=1,SCHNR)
      READ(PCKFIL,ERR=1300,END=1300)(SCHCOL(J),J=1,SCHNC)
      READ(PCKFIL,ERR=1300,END=1300)( (SCHDAT(I,J),J=1,SCHNC),
     1                               I=1,SCHNR)
      RETURN
C
1300  PRINT *,' ** I/O ERROR...UNIT',PCKFIL
      RCODE = 1
      RETURN
C
C ** SCHRDP ENDS HERE
      END
      SUBROUTINE SCHFIL(CLIST,FIRST,LAST,TYPE,UNIT,NAMLEN,DEFNAM,
     1                  RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This parses CLIST(FIRST:LAST) for filespec and opens schema file.
C ...RCODE  = 0 IFF ALL IS WELL
C     TYPE  = FORMATTED | UNFORMATTED
C     UNIT  = FILE UNIT NUMBER (DATFIL OR PCKFIL FROM CALLER)
C     NAMLEN= MAX LENGTH OF FILE NAME
C     DEFNAM= DEFAULT NAME (BEFORE PREFIX AND SUFFIX)
C
      CHARACTER*(*) CLIST,TYPE,DEFNAM
C LOCAL
      CHARACTER*64  FILNAM
      CHARACTER*1   CHAR
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF(UNIT.LE.0 .OR. NAMLEN.LT.4 .OR. NAMLEN.GT.64 .OR.
     1   (TYPE.NE.'FORMATTED' .AND. TYPE.NE.'UNFORMATTED')
     X  )THEN
         PRINT *,' ** SYSERR ENTERING SCHFIL...'
         PRINT *,' UNIT=',UNIT,' NAMLEN=',NAMLEN,' TYPE=',TYPE
         RCODE = 13
         RETURN
      ENDIF
C PARSE FOR FILENAME
      CALL FTOKEN(CLIST,FIRST,LAST,FILNAM,NAMLEN,CHAR)
      IF(FILNAM.EQ.' ')FILNAM=DEFNAM
         IF(FILNAM.EQ.' ')THEN
            PRINT *,' ** SCHEMA FILE NAME REQUIRED'
            RCODE = 1
            RETURN
         ENDIF
      CALL FSETNM('SCHEMA ',FILNAM,BEGEXT,RCODE)
      IF(RCODE.NE.0)RETURN
C
      CALL FLOPEN(UNIT,FILNAM,TYPE,'UNKNOWN',*1300)
      RETURN
C
1300  CONTINUE
      RCODE = 1
      RETURN
C
C  ** SCHFIL ENDS HERE
      END
      SUBROUTINE SCHDEL(ROWCOL,CLASS,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCSCHEMA
C
C This deletes ROWCOL CLASS
      CHARACTER*(*) ROWCOL,CLASS
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF( ROWCOL.EQ.'ROW ' )THEN
         IF( SCHNR.EQ.0 )GOTO 1310
C FIND ROW CLASS
         DO 100 I=1,SCHNR
            IF( SCHROW(I).EQ.CLASS )GOTO 150
            CALL FLOOKF(SCHROW(I),2,14,'(',K)
            IF( K.GT.0 )THEN
               IF( SCHROW(I)(1:K-1).EQ.CLASS )GOTO 150
            ENDIF
100      CONTINUE
         GOTO 1310
150      CONTINUE
         SCHNR = SCHNR-1
         DO 200 I1=I,SCHNR
200      SCHROW(I1) = SCHROW(I1+1)
      ELSE IF( ROWCOL.EQ.'COL ' )THEN
         IF( SCHNC.EQ.0 )GOTO 1310
C FIND COL CLASS
         DO 600 I=1,SCHNC
            IF( SCHCOL(I).EQ.CLASS )GOTO 650
            CALL FLOOKF(SCHCOL(I),2,14,'(',K)
            IF( K.GT.0 )THEN
               IF( SCHCOL(I)(1:K-1).EQ.CLASS )GOTO 650
            ENDIF
600      CONTINUE
         GOTO 1310
650      CONTINUE
         SCHNC = SCHNC-1
         DO 800 I1=I,SCHNC
800      SCHCOL(I1) = SCHCOL(I1+1)
      ELSE
         PRINT *,' ** ?',ROWCOL
         GOTO 1390
      ENDIF
      RETURN
C
1310  PRINT *,' ** ',ROWCOL,CLASS,' NOT FOUND'
1390  RCODE = -1
      RETURN
C
C ** SCHDEL ENDS HERE
      END
      SUBROUTINE SCHDBG
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCSCHEMA
C
C This is DEBUG for schema module (called by host SYSDBG).
C
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      PRINT *,' ENTERED SCHDBG...10.05'
      PRINT *,' PMSROW=',PMSROW,' PMSCOL=',PMSCOL
      PRINT *,' SCHNR=',SCHNR,  ' SCHNC=',SCHNC,
     1        ' SCHRLN=',SCHRLN,' SCHCLN=',SCHCLN
      PRINT *,' SCHROW:',(SCHROW(I),I=1,SCHNR)
      PRINT *,' SCHCOL:',(SCHCOL(I),I=1,SCHNC)
      LINE = 30
      CALL FPRMPT(LINE,*900)
      IF(SCHNC.GT.0 .AND. SCHNR.GT.0)THEN
         DO 100 I=1,SCHNR
            CALL FPRMPT(LINE,*900)
            PRINT *,' DATA ENTRIES FOR',I,':'
            DO 50 J=1,SCHNC,2
               CALL FPRMPT(LINE,*900)
               PRINT *,' ',SCHDAT(I,J),SCHDAT(I,J+1)
50          CONTINUE
100      CONTINUE
      ENDIF
C
900   RETURN
C
C ** SCHDBG ENDS HERE
      END
