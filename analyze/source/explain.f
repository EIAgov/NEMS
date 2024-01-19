C               ::: EXPLAIN.FOR 10-22-94 :::
C
C LAST DATES: earlier dates deleted
C             7-02-93...Changed EXFIND to allow ELEMENT *
C                       (* means all sets)
C             7-04-93...Minor change (see CC)
C                       Changes for EXTABL;  Added EXLOC8
C             1-15-94...Changed tabs in EXSYN for members
C
C This contains the following subroutines.
C
C    EXINIT.....initializes the EXPLAIN module
C    EXPLAN.....executes EXPLAIN command
C    EXREAD.....reads syntax file
C    EXTRC......translates row/col (syntactic)
C    EXENTY.....translates entity set
C    ERCMAP.....translates row/col map (direct)
C    EXSYN......gives syntax of rows/cols/sets...SYNTAX command
C    ESYNMP.....translate syntax map
C    EXLOC8.....locate set, row or col class from name
C    EXFIND.....find object from string
C    EDEBUG.....debug (entered from SYSDBG)
C
      SUBROUTINE EXINIT
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This initializes EXPLAIN module
C
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      VEREXP = '2.04'
C
      EXCHAR = '&'
C               :........SPECIAL CHAR TO IDENTIFY SET REFERENCE
      SWEXPL = .FALSE.
C               :........TRUE IFF SYNTAX FILE HAS BEEN READ
      SYNLEN = 8
C               :........MAX FIELD LENGTH (=NUMBER CHARS IN KEY)
      EXINDX(0) = 0
      MAXENT = PMXENT
      MAXTBL = PMXSYN
      NUMATR = 0
C
      RETURN
C
C ** EXINIT ENDS HERE
      END
      SUBROUTINE EXPLAN(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This parses CLIST(FIRST:LAST) and executes EXPLAIN command.
C  Output goes to FTEXT.
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME
      CHARACTER*4   ROWCOL
      LOGICAL*1     SW
C
      PARAMETER (MAXERR=5,MARGIN=5,INDENT=-4)
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      IF(.NOT.SWEXPL)THEN
         PRINT *,' Syntax file not readin'
         RCODE = 1
         RETURN
      ENDIF
C PARSE CONDITIONAL
      CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
      IF(RCODE.NE.0)RETURN
C SET ROW/COL ACCORDING TO DISPLAY SWITCH
      IF(SWDISP)THEN
           ROWCOL = 'COL'
           K = 2
           LAST = NCOLS
      ELSE
           ROWCOL = 'ROW'
           K = 1
           LAST = NROWS
      ENDIF
C
      NRC = NRCSUB(K)
      IF(NRC.EQ.0)THEN
           PRINT *,'No ',ROWCOL,'in submatrix'
           RETURN
      ENDIF
C INITIALIZE ERROR AND LINE COUNTS
      NERR = 0
      LINE = 1
C
      DO 100 I=RCSUB1(K),LAST
           CALL GETMAP(ROWCOL,I,SW)
           IF(.NOT.SW)GOTO 100
           NRC = NRC - 1
C OK, ROW/COL I IS IN SUBMATRIX, SO PREPARE TO EXPLAIN IT
C
           CALL GETNAM(ROWCOL,I,RNAME)
           CALL EXTRC(ROWCOL,RNAME,CLIST,ENDCL,RCODE)
           IF(RCODE.NE.0)THEN
              IF(NERR.GE.MAXERR)RETURN
              NERR = NERR + 1
              RCODE = 0
           ENDIF
C
           CALL FTEXT(CLIST,ENDCL,MARGIN,INDENT,LINE,'CLEAR',*900)
           IF(NRC.EQ.0)RETURN
100   CONTINUE
C
900   RETURN
C
C ** EXPLAN ENDS HERE
      END
      SUBROUTINE EXREAD(RCODE)
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
C This reads syntax from DATFIL, assumed open
C ...DCFLIP IS FOR THE INCLUDE
C
C LOCAL
      CHARACTER*128 CLIST
      CHARACTER*8   STRSYN,ATRNAM
      CHARACTER*1   CHAR
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
C INITIALIZE SET (ENTITY) STUFF
      NENTY = 0
      IRSYN = 0
      NCSYN = 0
      EPOINT = 0
C INITIALIZE INCLUDE
      INPLIN = 0
      SWINCL = .FALSE.
C
C READ ENTITY (SET) TABLES
100   CALL FLRDLN(CLIST,DATFIL,RCODE)
      IF( RCODE )900,101,13
C                :   :   :....FATAL ERROR
C                :   :...OK
C                :...EOF
101   CONTINUE
      IF(CLIST(1:10).EQ.'ROW SYNTAX')GOTO 190
C CLIST CONTAINS ENTITY (SET) TABLE INFO
C
         IF( EPOINT.EQ.MAXTBL )GOTO 1310
         FIRST = 1
         CALL FSLEN(CLIST,128,LAST)
C
C    GET KEY
C
         EPOINT = EPOINT + 1
         CALL FTOKEN(CLIST,FIRST,LAST,EXKEY(EPOINT),SYNLEN,CHAR)
         EXTABL(EPOINT) = CLIST(FIRST:)
         IF(CLIST(1:1).NE.' ')THEN
C          NEW SET
C
            IF(NENTY.EQ.MAXENT)THEN
               PRINT *,' Sorry, set limit reached (',MAXENT,')'
               GOTO 1300
            ENDIF
C       SET POINTER TO BOTTOM OF OLD ENTITY (SET)
            EXINDX(NENTY) = EPOINT - 1
C
            NENTY = NENTY + 1
            ATTR  = 0
            IF(CLIST(LAST:LAST).EQ.'=')THEN
C        = MEANS JOIN SETS LISTED IN NEXT LINE
               CALL FSLEN(EXTABL(EPOINT),80,LAST)
               EXTABL(EPOINT)(LAST:) = ' '
               CALL FLRDLN(CLIST,DATFIL,RCODE)
               IF(RCODE)900,149,13
149            FIRST = 1
               CALL FSLEN(CLIST,128,LAST)
C
C        ::: LOOP OVER ENTITY SETS SPECIFIED AFTER = :::
C
150            CALL FTOKEN(CLIST,FIRST,LAST,STRSYN,SYNLEN,CHAR)
               IF(STRSYN.NE.' ')THEN
C      ...COPY MEMBERS OF STRSYN
                 DO 160 E=1,NENTY-1
                   ETOP = EXINDX(E-1) + 1
                   IF(STRSYN.EQ.EXKEY(ETOP))GOTO 170
160              CONTINUE
                 PRINT *,' ** CANNOT RECOGNIZE ',STRSYN
                 GOTO 1300
170              CONTINUE
C SET E IS IN UNION
                 IF(ATTRIB(E).GT.0)THEN
C ...CHECK ATTRIBUTE
                    IF(ATTR.EQ.0)THEN
                       ATTR = ATTRIB(E)
                    ELSE IF(ATTR.GT.0.AND.ATTRIB(E).NE.ATTR)THEN
                       PRINT *,' Warning: Attribute conflict for ',
     1                         EXKEY(EPOINT)
                       ATTR = -1
                    ENDIF
                 ENDIF
                 NUMBER = EXINDX(E) - ETOP
C ...NUMBER = NUMBER OF MEMBERS
                 IF(EPOINT+NUMBER.GT.MAXTBL)GOTO 1310
                 DO 180 E=1,NUMBER
                    EXKEY(EPOINT+E) = EXKEY(ETOP+E)
                    EXTABL(EPOINT+E)= EXTABL(ETOP+E)
180              CONTINUE
                 EPOINT = EPOINT + NUMBER
                 GOTO 150
               ENDIF
C NEW SET ADDED...STORE ITS ATTRIBUTE
               IF(ATTR.LT.0)THEN
                  ATTRIB(NENTY) = 0
               ELSE
                  ATTRIB(NENTY) = ATTR
               ENDIF
            ELSE
C NEW SET IS EXPLICIT...SET ITS ATTRIBUTE
               ATTRIB(NENTY) = 0
               CALL FLOOK(EXTABL(EPOINT),1,80,'//',BEGATR)
               IF(BEGATR.GT.0)THEN
                  FIRST = BEGATR+2
                  CALL FTOKEN(EXTABL(EPOINT),FIRST,80,ATRNAM,8,CHAR)
                  IF(NUMATR.GT.0)THEN
                     DO 185 NUMBER=1,NUMATR
                        IF(ATRNAM.EQ.ATRTYP(NUMBER))GOTO 189
185                  CONTINUE
                  ENDIF
C ADD ATTRIBUTE
                  IF(NUMATR.GE.PMXATR)THEN
                     PRINT *,' Sorry, not enough space to add',
     1                       ' attribute ',ATRNAM
                     NUMBER = 0
                  ELSE
                     NUMATR = NUMATR+1
                     ATRTYP(NUMATR) = ATRNAM
                     NUMBER = NUMATR
                  ENDIF
189               ATTRIB(NENTY) = NUMBER
                  EXTABL(EPOINT)(BEGATR:) = ' '
               ENDIF
            ENDIF
          ENDIF
C
      GOTO 100
C
190   CONTINUE
C
C END ENTITY (SET) TABLES...BEGIN ROW SYNTAX
C
      IRSYN = EPOINT
      EXINDX(NENTY) = IRSYN
C
200   CONTINUE
      CALL FLRDLN(CLIST,DATFIL,RCODE)
      IF( RCODE )900,201,13
201      IF( CLIST(1:13).EQ.'COLUMN SYNTAX' )GOTO 290
         IF( CLIST(1:6).EQ.'ENDATA' )GOTO 910
         IF( EPOINT.EQ.MAXTBL )GOTO 1310
C
         FIRST = 1
         CALL FSLEN(CLIST,81,LAST)
         IF( LAST.GT.80 )THEN
            PRINT *,' SYNTAX MAP TOO LONG...SEE MODEL MANAGER'
            GOTO 1300
         ENDIF
C
         EPOINT = EPOINT + 1
         CALL FTOKEN(CLIST,FIRST,LAST,EXKEY(EPOINT),SYNLEN,CHAR)
         EXTABL(EPOINT) = CLIST(FIRST:)
      GOTO 200
C
290   CONTINUE
C
C BEGIN COLUMN SYNTAX
C
      ICSYN = EPOINT
C
300   CONTINUE
      CALL FLRDLN(CLIST,DATFIL,RCODE)
      IF( RCODE )900,301,13
301      IF( CLIST(1:6).EQ.'ENDATA' )THEN
            NCSYN = EPOINT - ICSYN
            GOTO 910
         ENDIF
C
         IF(EPOINT.EQ.MAXTBL)GOTO 1310
C
         FIRST = 1
         CALL FSLEN(CLIST,81,LAST)
         IF( LAST.GT.80 )THEN
            PRINT *,' ** SYNTAX MAP TOO LONG...SEE MODEL MANAGER'
            GOTO 1300
         ENDIF
C
         EPOINT = EPOINT + 1
         CALL FTOKEN(CLIST,FIRST,LAST,EXKEY(EPOINT),SYNLEN,CHAR)
         EXTABL(EPOINT) = CLIST(FIRST:)
      GOTO 300
C
900   CONTINUE
      IF( SWMSG )PRINT *,' END OF FILE REACHED...ENDATA ASSUMED'
      RCODE = 0
C
C END OF SYNTAX
C
910   CONTINUE
      IF( EPOINT.LT.MAXTBL )EXTABL(EPOINT+1)=' '
      IF( IRSYN.GT.0 )NRSYN = EPOINT - IRSYN - NCSYN
      SWEXPL = NRSYN .GT. 0
      IF( .NOT.SWEXPL )PRINT *,' ** NO ROW SYNTAX...SEE MODEL MANAGER'
C
C ALL RETURNS COME HERE
9000  CONTINUE
      CLOSE(DATFIL)
      IF( SWINCL )THEN
C CLOSE THE INCLUDE FILE
         CLOSE(INCFIL)
         SWINCL = .FALSE.
      ENDIF
      RETURN
C
13    CONTINUE
      PRINT *,' ** IO ERROR READING SYNTAX FILE'
      GOTO 1390
1300  CONTINUE
      PRINT *,' LAST LINE READ:'
      PRINT *,' ',CLIST(:79)
      GOTO 1390
1310  PRINT *,' Sorry, not enough space to store syntax info'
1390  CONTINUE
      SWEXPL = .FALSE.
      RCODE = 1
      GOTO 9000
C
C ** EXREAD ENDS HERE
      END
      SUBROUTINE EXTRC(ROWCOL,RNAME,CLIST,ENDCL,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This translates row/col (RNAME) into CLIST(:ENDCL)
C
      CHARACTER*128 CLIST
      CHARACTER*16  RNAME
      CHARACTER*4   ROWCOL
C LOCAL
      CHARACTER*16  CNAME
      LOGICAL*1     SW
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      IF(ROWCOL.EQ.'ROW')THEN
           CLIST = ' Row '//RNAME
           CALL FSLEN(CLIST,21,ENDCL)
           ENDCL = ENDCL + 2
           IF(RNAME.EQ.OBJNAM)THEN
                CLIST(ENDCL:) = 'is the objective ('
     1                 //OPTNAM(1:8)//').'
                CALL FSLEN(CLIST,80,ENDCL)
                RETURN
           ENDIF
           FIRST = IRSYN + 1
           LAST  = IRSYN + NRSYN
      ELSE
           CLIST = ' Column '//RNAME
           CALL FSLEN(CLIST,24,ENDCL)
           ENDCL = ENDCL + 2
           IF(NCSYN.EQ.0)GOTO 1300
           FIRST = ICSYN + 1
           LAST  = ICSYN + NCSYN
      ENDIF
C
C LOOKUP ROWCOL CLASS
C
      DO 50 I=FIRST,LAST
           CNAME = EXKEY(I)
           CALL FMATCH(CNAME,RNAME,' ',SW)
           IF(SW)GOTO 90
50    CONTINUE
C
C NO MATCH...SEE IF WILD CHAR IS PRESENT
C
      IF(EXKEY(LAST).NE.EXCHAR)GOTO 1300
C ...YES
      I = LAST
C
90    CALL ESYNMP(I,CLIST,RNAME,ENDCL,RCODE)
C
      RETURN
C
1300  RCODE = 1
      CLIST(ENDCL:) = ' has no translation...see model manager'
      CALL FSLEN(CLIST,128,ENDCL)
      RETURN
C
C ** EXTRC ENDS HERE
      END
      SUBROUTINE EXENTY(STRSYN,STRELM,CLIST,ENDCL,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This translates entity set whose key = STRSYN and
C                        whose element = STRELM
C    (STRELM blank means give 'some '&ET)
C
C Result is put into CLIST(ENDCL:)
C
C RCODE = 0  means all is well
C
      CHARACTER*(*) STRELM,STRSYN
      CHARACTER*(*) CLIST
C LOCAL
      CHARACTER*8   SET,ELEMNT
C               :...= SIZE OF EXKEY
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      IF(ENDCL.GE.121)THEN
         PRINT *,' ** MAX LENGTH (128) EXCEEDED...'
         GOTO 1313
      ENDIF
C COPY STRINGS (TO ENSURE LENGTH = 8)
      SET    = STRSYN
      ELEMNT = STRELM
C
C LOOKUP ENTITY SET
      DO 100 I=1,NENTY
         EPOINT = EXINDX(I-1) + 1
         IF( EXKEY(EPOINT).EQ.SET )GOTO 110
100   CONTINUE
C
      RCODE = 1
      CLIST(ENDCL:) = ' ?'//SET
      ENDCL = ENDCL + SYNLEN + 2
      RETURN
C
110   CONTINUE
C
      IF( ELEMNT.EQ.' ' )THEN
C CALLER DOESN'T HAVE ELEMENT SPECIFIED...ENTER 'some'
         CLIST(ENDCL:) = 'some'
         ENDCL = ENDCL + 5
         GOTO 900
      ENDIF
C
C LOOKUP MEMBER (ELEMNT)
C
      FIRST = EPOINT + 1
      LAST  = EXINDX(I)
      IF(LAST.LT.FIRST)GOTO 1300
C
      DO 200 EPOINT=FIRST,LAST
         IF(EXKEY(EPOINT).EQ.ELEMNT)GOTO 900
200   CONTINUE
      EPOINT = LAST
      IF(EXKEY(LAST).EQ.EXCHAR)GOTO 900
C
1300  CLIST(ENDCL:) = '?'//ELEMNT
      ENDCL = ENDCL + SYNLEN + 2
1313  CONTINUE
      IF( .NOT.SWRULE )PRINT *,' ** UNABLE TO COMPLETE TRANSLATION **'
      RCODE = 1
      RETURN
C
900   CONTINUE
      CALL FSLEN(EXTABL(EPOINT),80,LS)
      IF(ENDCL + LS.GE.128)THEN
         CLIST(ENDCL:) = EXTABL(EPOINT)(:125-ENDCL)//'**'
         ENDCL = 128
         GOTO 1313
      ENDIF
C
      CLIST(ENDCL:) = EXTABL(EPOINT)(:LS)
      ENDCL = ENDCL + LS
C
      RETURN
C
C ** EXENTY ENDS HERE
      END
      SUBROUTINE ERCMAP(EPOINT,CLIST,ENDCL,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This translates row/col syntax map in EXTABL(EPOINT)
C ...result is put into CLIST(ENDCL:)
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      IF(EXKEY(EPOINT).EQ.EXCHAR)THEN
           CLIST(ENDCL:) = 'anything (else)'
      ELSE
           CLIST(ENDCL:) = EXKEY(EPOINT)
      ENDIF
C
      CALL FSLEN(CLIST,64,ENDCL)
      ENDCL = ENDCL + 2
      RNAME = ' '
      CALL ESYNMP(EPOINT,CLIST,RNAME,ENDCL,RCODE)
C
      RETURN
C
C ** ERCMAP ENDS HERE
      END
      SUBROUTINE EXSYN(CLIST,FIRST,LAST,LINE,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This gives syntax of rows/cols/sets (using FTEXT for output).
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME
      CHARACTER*8   OPTION(4),STR8
      CHARACTER*1   CHAR
      LOGICAL*1     SWOPTN(4),SW
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      OPTION(1) = 'SETS    '
      OPTION(2) = 'ROWS    '
      OPTION(3) = 'COLUMNS '
      OPTION(4) = 'GENERATE'
C
C   PARSE CLIST TO GET SPECS FROM {ROWS,COLUMNS,SETS}
C
      NUMBER = 4
      CALL FOPSET(CLIST,FIRST,LAST,OPTION,NUMBER,SWOPTN,RCODE)
      IF( RCODE.NE.0 )RETURN
      IF( SWOPTN(1).AND.SWOPTN(2).AND.SWOPTN(3).AND.SWOPTN(4) )
C USER SPECIFIED *...DO NOT GENERATE
     1   SWOPTN(4) = .FALSE.
C
      IF( SWOPTN(4) )THEN
C GENERATE ...ADDED 7-2-93
             SWOPTN(4) = .FALSE.
         IF( SWOPTN(1).OR.SWOPTN(2).OR.SWOPTN(3) )THEN
            PRINT *,' ** GENERATE MUST BE CHOSEN BY ITSELF'
            RCODE = 1
         ELSE
            CALL EXGENR(CLIST,FIRST,LAST,RCODE)
         ENDIF
         RETURN
      ENDIF
C ================
C ROWS, COLS, SETS
      IF( .NOT.SWEXPL )THEN
         PRINT *,' No syntax...use READIN SYNTAX'
         RCODE = 1
         RETURN
      ENDIF
C
      RNAME = ' '
C SET LEFT MARGIN FOR FTEXT
      MARGIN =  6
      INDENT = -4
C
      IF(SWOPTN(1))THEN
C SHOW SETS
C   SEE IF SET NAME MASK SPECIFIED
           CALL FTOKEN(CLIST,FIRST,LAST,STR8,8,CHAR)
           IF(STR8.EQ.' ')STR8 = '*'
C   NOW STR8 = SET NAME MASK (* FOR ALL SETS, WHICH IS DEFAULT)
           IF(STR8.EQ.'*')THEN
C   ALL SETS...FIRST THE NUMBER OF THEM (NENTY)
              CALL FI2C(RNAME,NENTY,F)
              CLIST = ' '//RNAME(F:8)//' set'
              CALL FSLEN(CLIST,25,LAST)
              IF(NENTY.NE.1)THEN
                 LAST = LAST+1
                 CLIST(LAST:)='s'
              ENDIF
              CLIST(LAST+1:)='...'
              LAST = LAST+3
              CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR',*900)
           ENDIF
C
           IF(NENTY.EQ.0)GOTO 290
           EPOINT = 0
C
C       LOOP OVER ENTITY SETS
           DO 200 I=1,NENTY
              EPOINT = EPOINT + 1
              NUMBER = EXINDX(I) - EXINDX(I-1) - 1
              CALL FMATCH(STR8,EXKEY(EPOINT),'*',SW)
              IF(.NOT.SW)THEN
                 EPOINT = EPOINT+NUMBER
                 GOTO 200
              ENDIF
C  LIST THIS SET (NUMBER = NUMBER OF MEMBERS)
              CLIST = ' Set '//EXKEY(EPOINT)
              CALL FSLEN(CLIST,32,L)
              CLIST(L+2:) = '= '//EXTABL(EPOINT)
              CALL FSLEN(CLIST,128,LAST)
              CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR',*900)
C
              IF(NUMBER.EQ.0)GOTO 200
C TABS FOR MEMBERS
              TAB =MARGIN+2
              TAB2=INDENT+2
C                         :....CHANGED FROM 5, 10-22-94
              DO 150 K=1,NUMBER
                 EPOINT = EPOINT + 1
                 CLIST = EXKEY(EPOINT)
                 CALL FSLEN(CLIST,32,L)
                 CLIST(L+2:) = '= '//EXTABL(EPOINT)
                 CALL FSLEN(CLIST,128,LAST)
                 CALL FTEXT(CLIST,LAST,TAB,TAB2,LINE,'CLEAR',*900)
150           CONTINUE
200        CONTINUE
      ENDIF
C
290   CONTINUE
C
      IF(SWOPTN(2))THEN
C SHOW ROW SYNTAX
           CALL FI2C(RNAME,NRSYN,F)
           CLIST = 'Row syntax has '//RNAME(F:8)//' class'
           CALL FSLEN(CLIST,64,LAST)
           IF(NRSYN.NE.1)THEN
              CLIST(LAST+1:)='es'
              LAST = LAST+2
           ENDIF
           CALL FTEXT(CLIST,LAST,1,0,LINE,'EJECT ',*900)
           IF(NRSYN.EQ.0)GOTO 390
C
C       LOOP OVER ROW CLASSES
           DO 300 I=IRSYN+1,IRSYN+NRSYN
              CLIST = 'A row that begins with'
              CALL FSLEN(CLIST,128,ENDCL)
              ENDCL = ENDCL + 2
              CALL ERCMAP(I,CLIST,ENDCL,RCODE)
              IF(RCODE.NE.0)GOTO 1300
C
              LAST  = 79
              IF(LAST.GT.ENDCL)THEN
                 LAST = ENDCL
              ELSE
250              IF(CLIST(LAST:LAST).NE.' '.AND.
     1              CLIST(LAST:LAST).NE.'.'.AND.
     2              CLIST(LAST:LAST).NE.',')THEN
                    LAST = LAST - 1
                    GOTO 250
                 ENDIF
              ENDIF
C
              CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR',*900)
300        CONTINUE
      ENDIF
C
390   CONTINUE
C
      IF(SWOPTN(3))THEN
C SHOW COLUMN CLASSES
           CALL FI2C(RNAME,NCSYN,F)
           CLIST = 'Column syntax has '//RNAME(F:8)//' class'
           CALL FSLEN(CLIST,64,LAST)
           IF(NCSYN.NE.1)THEN
              CLIST(LAST+1:)='es'
              LAST = LAST+2
           ENDIF
           CALL FTEXT(CLIST,LAST,1,0,LINE,'EJECT ',*900)
           IF(NCSYN.EQ.0)GOTO 490
C
C     LOOP OVER COLUMN CLASSES
           DO 400 I=ICSYN+1,ICSYN+NCSYN
              CLIST = 'A column that begins with'
              CALL FSLEN(CLIST,128,ENDCL)
              ENDCL = ENDCL + 2
              CALL ERCMAP(I,CLIST,ENDCL,RCODE)
              IF(RCODE.NE.0)GOTO 1300
C
              LAST  = 79
              IF(LAST.GT.ENDCL)THEN
                 LAST = ENDCL
              ELSE
350              IF(CLIST(LAST:LAST).NE.' '.AND.
     1              CLIST(LAST:LAST).NE.'.'.AND.
     2              CLIST(LAST:LAST).NE.',')THEN
                    LAST = LAST - 1
                    GOTO 350
                 ENDIF
              ENDIF
C
              CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR',*900)
400        CONTINUE
      ENDIF
C
490   CONTINUE
C
900   RETURN
C
1300  PRINT *,' ** ',(CLIST(L:L),L=1,ENDCL)
      RETURN
C
C ** EXSYN ENDS HERE
      END
      SUBROUTINE ESYNMP(EPOINT,CLIST,RNAME,ENDCL,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This translates syntax map EXTABL(EPOINT) into CLIST(ENDCL:).
C RNAME = ROW/COLUMN NAME (relevant only if map is for row/column;
C                          else, blank)
C
      CHARACTER*128 CLIST
      CHARACTER*16  RNAME
C LOCAL
      CHARACTER*80  STR80
      CHARACTER*8   STRELM,STRSYN
      CHARACTER*1   CHAR
      PARAMETER  (MARGIN=5,INDENT=-4)
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      STR80 = EXTABL(EPOINT)
      CALL FSLEN(STR80,80,LS)
      IF(LS.EQ.0)RETURN
      IF(ENDCL + LS.GE.121)GOTO 1300
C
C  LOOP OVER CHARS IN SYNTAX MAP, SUBSTITUTING ENTITY (SET) REFERENCES
      I = 1
      LINE = 1
C
100   CONTINUE
        IF(STR80(I:I).NE.EXCHAR)THEN
C SIMPLY COPY CHAR TO OUR TEXT
            CLIST(ENDCL:ENDCL) = STR80(I:I)
            IF(ENDCL.GE.127)
     1        CALL FTEXT(CLIST,ENDCL,MARGIN,INDENT,LINE,'KEEP',*900)
            ENDCL = ENDCL + 1
            I = I + 1
         ELSE
C    GET ENTITY SET REFERENCED (BY EXCHAR)...ENDED BY COLON (:)
            CALL FLOOK(STR80,I+2,I+SYNLEN-1,':',COLON)
            IF(COLON.EQ.0)THEN
               PRINT *,' ** : MISSING IN SYNTAX...SEE MODEL MANAGER'
               RCODE = 2
               RETURN
            ENDIF
C
C WE HAVE STR80(I+1:COLON-1)=CODE NAME OF ENTITY SET, AND
C STR80(COLON+1:COLON+d)=START POSITION OF FIELD IN ROW/COLUMN NAME
C
              STRSYN = STR80(I+1:COLON-1)
              LENSYN = COLON - I - 1
              I = COLON+1
              CALL FVRNG(STR80,I,I+2,VL,VU,VINF,CHAR,RCODE)
              IF(RCODE.NE.0)RETURN
              FIELD = VL+.1
CCC ADDED FIELD END POSITION TO OVER-RIDE SET NAME LENGTH
              IF( VU.GT.VL )THEN
                 FLDLEN = VU - VL + 1.1
              ELSE
                 FLDLEN = LENSYN
              ENDIF
C
              IF(RNAME.EQ.' ')THEN
C NO ELEMENT TO TRANSLATE
                 STRELM = ' '
              ELSE
C GET ELEMENT FROM RNAME
                  ENDFLD = FIELD + FLDLEN-1
                  IF( FIELD.LT.1 .OR. ENDFLD.GT.NAMLEN )THEN
                     PRINT *,' ** FIELD OUT OF RANGE...',FIELD
                     RCODE = 1
                     RETURN
                  ENDIF
                  STRELM = RNAME(FIELD:ENDFLD)
C         ...STRELM = ELEMENT OF SET IN RNAME
              ENDIF
C
              CALL EXENTY(STRSYN,STRELM,CLIST,ENDCL,RCODE)
              IF( RCODE.NE.0 )RETURN
C PUT IN THE DELIMETER (LIKE BLANK, EQUAL, COMMA,...) WHEN WE
C PARSED FOR THE FIELD IN RNAME (SIMPLY THE CHAR FROM FVRNG)
              ENDCL = ENDCL+1
              CLIST(ENDCL:)=CHAR
           ENDIF
C
      IF( I.LE.LS )GOTO 100
C DONE
      CALL FSLEN(CLIST,128,ENDCL)
      IF( CLIST(ENDCL:).NE.'.' .AND. ENDCL.LT.128 )THEN
CC                             ================== ADDED 1-12-94
         ENDCL = ENDCL + 1
         CLIST(ENDCL:) = '.'
      ENDIF
C
900   RETURN
C
1300  PRINT *,' ** TRANSLATION HAS TOO MANY CHARACTERS TO COMPLETE'
      IF( SWDBG )PRINT *,' ...1300 IN ESYNMP'
      RCODE = 1
      RETURN
C
C ** ESYNMP ENDS HERE
      END
      SUBROUTINE EXLOC8(WHAT,RNAME,NUMBER)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This locates WHAT := SET | ROW | COL, given name (RNAME)
C ...Returns NUMBER=0 if not found.
C
      CHARACTER*(*) WHAT,RNAME
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      IF( WHAT.EQ.'SET ' )THEN
         GOTO 500
      ENDIF
      IF( WHAT.EQ.'ROW ' )THEN
         ISYN = IRSYN
         NUM  = NRSYN
      ELSE IF( WHAT.EQ.'COL ' )THEN
         ISYN = ICSYN
         NUM  = NCSYN
      ELSE
         GOTO 1300
      ENDIF
      IF( NUM.EQ.0 )GOTO 1300
C LOCATE ROW OR COL CLASS
      DO 200 I=ISYN+1,ISYN+NUM
         IF( EXKEY(I).EQ.RNAME )THEN
            NUMBER = I-ISYN
            RETURN
         ENDIF
200   CONTINUE
      GOTO 1300
C
500   CONTINUE
C LOCATE SET
      DO 600 NUMBER=1,NENTY
         EPOINT = EXINDX(NUMBER-1) + 1
         IF( EXKEY(EPOINT).EQ.RNAME )RETURN
600   CONTINUE
C
1300  NUMBER = 0
      IF( SWDBG )PRINT *,' ...1300 IN EXLOC8 ',WHAT,' ',RNAME
      RETURN
C
C ** EXLOC8 ENDS HERE
      END
      SUBROUTINE EXFIND(STRING,OBJECT,OBLIST)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This finds STRING in OBJECT, returning OBLIST = matching list.
C       OBJECT := ROW | COLUMN | SET | ELEMENT
C Upon entrance, if OBJECT = ELEMENT, OBLIST = set name | * (all sets)
C If not found, returned OBLIST is null.
C MAX NUMBER OF OBJECTS RETURNED = 9
C
      CHARACTER*(*) STRING,OBJECT,OBLIST
C LOCAL
      LOGICAL*1     SW
      CHARACTER*128 CLIST
      CHARACTER*80  STR80
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
C INITIALIZE OBJECT LIST (CLIST WILL BECOME OBLIST) TO BE NULL
      CLIST = ' '
      LENGTH = 0
      NOBJ   = 0
C PUT SLIDING MASK CHAR
      STR80 = '"'//STRING
C
      IF( OBJECT.EQ.'ROW' )THEN
C ROW
         I0 = IRSYN
         NUMBER = NRSYN
      ELSE IF( OBJECT.EQ.'COLUMN' )THEN
C COLUMN
         I0 = ICSYN
         NUMBER = NCSYN
      ELSE
         GOTO 1000
      ENDIF
C ROW|COL HAS NUMBER CLASSES
      IF( NUMBER.EQ.0 )GOTO 9000
C COLLECT ROW|COL CLASSES
      DO 100 I=I0+1,I0+NUMBER
         CALL FMATCH(STR80,EXTABL(I),' ',SW)
         IF( SW )THEN
            CALL FSLEN(EXKEY(I),8,L)
            CLIST(LENGTH+1:) = EXKEY(I)(1:L)//','
            LENGTH = LENGTH + L+1
            NOBJ = NOBJ+1
            IF( NOBJ.EQ.9 )GOTO 9000
         ENDIF
100   CONTINUE
      GOTO 9000
C
1000  CONTINUE
C SET | ELEMENT
      IF( NENTY.EQ.0 )GOTO 9000
C
      DO 1200 I=1,NENTY
         BEGSET = EXINDX(I-1) + 1
         IF( OBJECT.EQ.'SET' )THEN
C COMPARE STRING WITH SET'S MEANING
            CALL FMATCH(STR80,EXTABL(BEGSET),' ',SW)
            IF( SW )THEN
               CALL FSLEN(EXKEY(BEGSET),8,L)
               CLIST(LENGTH+1:) = EXKEY(BEGSET)(:L)//','
               LENGTH = LENGTH + L+1
               NOBJ = NOBJ+1
               IF( NOBJ.EQ.9 )GOTO 9000
            ENDIF
         ELSE
C SEE IF THIS IS THE SET TO LOOK FOR ELEMENT
            IF( OBLIST(1:8).NE.EXKEY(BEGSET) .AND. OBLIST(1:1).NE.'*' )
     1         GOTO 1200
C ...IT IS, SO INTERROGATE ITS MEMBERS
            ENDSET = EXINDX(I)
            DO 1100 M=BEGSET+1,ENDSET
               CALL FMATCH(STR80,EXTABL(M),' ',SW)
               IF( SW )THEN
C ...MEMBER MATCHES STRING, SO ADD TO LIST
                  IF( OBLIST(1:1).EQ.'*' )THEN
C    UNLESS ALREADY THERE (FROM ANOTHER SET)
                     CALL FLOOK(CLIST,1,LENGTH+1,EXKEY(M),IM)
                     IF( IM.GT.0 )GOTO 1100
                  ENDIF
                  CALL FSLEN(EXKEY(M),8,L)
                  CLIST(LENGTH+1:) = EXKEY(M)(:L)//','
                  LENGTH = LENGTH + L+1
                  NOBJ = NOBJ+1
                  IF( NOBJ.EQ.9 )GOTO 9000
               ENDIF
1100        CONTINUE
            IF( OBLIST(1:1).NE.'*' )GOTO 9000
         ENDIF
C NEXT SET
1200  CONTINUE
C
9000  CONTINUE
C ALL RETURNS COME HERE
      IF( LENGTH.GT.0 )CLIST(LENGTH:) = ' '
C                                  :...REMOVE LAST COMMA
      OBLIST = CLIST(:LENGTH+1)
      RETURN
C
C ** EXFIND ENDS HERE
      END
      SUBROUTINE EDEBUG
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      PRINT *,' EDEBUG ENTERED...'
      IF(PAUSE.LT.0)THEN
         LINE = 99
         CALL FPRMPT(LINE,*900)
      ELSE
         LINE = 5
      ENDIF
C
      PRINT *,' NENTY=',NENTY,', NRSYN=',NRSYN,', NCSYN=',NCSYN
      PRINT *,' IRSYN=',IRSYN,', ICSYN=',ICSYN
      PRINT *,' MAXENT=',MAXENT,' MAXTBL=',MAXTBL
C
      PRINT *,' I, EXINDX, ATTRIB:'
      DO 50 I=0,NENTY,5
         WRITE(*,51)(II,EXINDX(II),ATTRIB(II),II=I,I+4)
51       FORMAT(5(1X,I3,':',I4,1X,I3))
50    CONTINUE
      LINE = LINE+NENTY/5+1
C
      DO 100 I=1,MAXTBL
         CALL FPRMPT(LINE,*900)
         WRITE(*,2)I,EXKEY(I),EXTABL(I)
2        FORMAT(I4,'=',A8,' ',A66)
100   CONTINUE
C
900   RETURN
C
C ** EDEBUG ENDS HERE
      END
