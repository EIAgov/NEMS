C                 ::: FLDICTRY.FOR  7-29-95 :::
C
C Earlier dates deleted
C       2-02-93...added sliding mask match in FQUEST
C       10-22-94...Cosmetic changes to error messages in FDICIN
C
C This file contains the following FLIP routines.
C
C    FDICIN......Read dictionary (if any)
C    FQUEST......Execute ? query
C    FDTEXT......Prints text for FQUEST (added 1-30-93)
C
      SUBROUTINE FDICIN(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This executes the _DICTNRY command (including initial load).
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
      CHARACTER*64 FILNAM
      CHARACTER*8  OPTION(3)
      INTEGER      IOSTAT
      LOGICAL      EXIST
      CHARACTER*128 CLIST
      LOGICAL*1    SWQUST
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      IF(CLIST(1:1).NE.'*')GOTO 1000
C     ...........................................
C     : THIS IS INITIAL LOAD (CALLED BY FLINIT) :
C     :.........................................:
      MAXDIC = FMXDIC
      DICTXT(0) = '(none)'
      CALL FSLEN(SYSNAM,8,LS)
      FILNAM = SYSNAM
C ADD PREFIX AND UPDATE SUFFIX
      CALL FSETNM('DICTNARY',FILNAM,BEGEXT,RCODE)
      IF(RCODE.NE.0)RETURN
      CALL FINQUR(FILNAM,IOSTAT,EXIST,*999)
      IF(IOSTAT.NE.0 .OR. .NOT.EXIST)RETURN
C
C  LOAD (NEW) DICTIONARY
10    NUMDIC = 0
      SWQUST = .FALSE.
C           :..Record whether ? is key in dictionary
C
15    CONTINUE
      CALL FLOPEN(HLPFIL,FILNAM,'FORMATTED','OLD',*900)
      SWINCL = .FALSE.
      INPLIN = 0
      DICTXT(0) = FILNAM
C
C NOW USE FILNAM TO HOLD ERROR MESSAGE
      FILNAM = ' '
      CLIST  = ' '
C
C   ::: TOP OF LOOP TO READ DICTIONARY :::
100   CALL FLRDLN(CLIST,HLPFIL,RCODE)
        IF( RCODE.NE.0 .OR. CLIST.EQ.'ENDATA' )GOTO 900
C ECHO IF $ IN COLUMN 1
        IF(CLIST(1:1).EQ.'$')THEN
           PRINT *,' ',CLIST(2:SCRWTH)
           GOTO 100
        ENDIF
C
        CALL FSLEN(CLIST,127,LAST)
C SKIP LEADING BLANKS (BEFORE KEY)
        CALL FLOOKF(CLIST,1,LAST,' ',FIRST)
C IS THIS A CONTINUATION OF PREVIOUS ENTRY?
         IF(CLIST(FIRST:FIRST+2).EQ.'...')THEN
C YES, SKIP THE ELLIPSIS...
            FIRST = FIRST+3
         ELSE
C MAYBE NOT, LOOK FOR BREAK (:=)
             CALL FLOOKF(CLIST,FIRST,LAST,':=',BREAK)
             IF( BREAK.GT.0 )GOTO 190
C NO BREAK...ASSUME THIS IS CONTINUATION OF LAST ENTRY
         ENDIF
C
C ARRIVED AS FALL THRU FROM THE ABOVE IF...THEN...ELSE
180      CONTINUE
C ADD CONTINUATION WITH BLANK AS KEY
         NUMDIC = NUMDIC + 1
         DICKEY(NUMDIC) = ' '
         GOTO 200
C
190      CONTINUE
C NEW ENTRY...ADD TO DICTIONARY
         NUMDIC = NUMDIC + 1
         DICKEY(NUMDIC) = CLIST(FIRST:BREAK-1)
         SWQUST = SWQUST .OR. DICKEY(NUMDIC).EQ.'?'
         FIRST = BREAK + 2
C
C NOW ADD TEXT (FOR NEW ENTRY OR FOR CONTINUATION)
C
C SKIP LEADING BLANKS (BEFORE TEXT)
200      IF(CLIST(FIRST:FIRST).EQ.' ')THEN
             FIRST = FIRST + 1
             GOTO 200
         ENDIF
         DICTXT(NUMDIC) = CLIST(FIRST:)
      IF(NUMDIC.LT.MAXDIC)THEN
         DICKEY(NUMDIC+1) = '+'
C                            :....This is to suppress continuation
C                                 (indicated in general by blank key).
         GOTO 100
      ENDIF
C
C  ** DICTIONARY OVERFLOW...INCREASE DIMENSIONS OF DICKEY AND DICTXT
C     (SEE  DCFLIP) AND SET MAXDIC (IN FDICIN).
C
      PRINT *,' Dictionary is full.'
C
900   CLOSE(HLPFIL)
      IF(SWINCL)THEN
         CLOSE(INCFIL)
         SWINCL = .FALSE.
      ENDIF
      IF(.NOT.SWQUST)THEN
C ? NOT A KEY...WE NEED TO ADD IT
         IF(NUMDIC.EQ.MAXDIC)THEN
C !NO ROOM TO ADD IT
CC 7-29-95            PRINT *,' Warning:  Dictionary is full.'
         ELSE
C ...PUT IT AT END OF LIST
            NUMDIC = NUMDIC + 1
            DICKEY(NUMDIC)='?'
            DICTXT(NUMDIC)='<phrase> to lookup <phrase> in dictionary'
         ENDIF
      ENDIF
CC 7-29-95
999   RETURN
C      ......................................
C      : EXECUTE _DICTNRY COMMAND FROM USER :
C      :....................................:
1000  CONTINUE
      OPTION(1) = 'CLEAR'
      OPTION(2) = 'LOAD'
      OPTION(3) = 'MERGE'
      NUMBER = 3
      RCODE = 0
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)RETURN
C
      IF(NUMBER.EQ.0)THEN
C SIMPLY GIVE LOADED DICTIONARY NAME
         CLIST = '      Dictionary = '//DICTXT(0)
         CALL FSLEN(CLIST,100,L)
         WRITE(OUTPUT,1001)(CLIST(I:I),I=1,SCRWTH)
1001     FORMAT(100A1)
         IF(L.GT.SCRWTH)
     1      WRITE(OUTPUT,1001)(CLIST(I:I),I=SCRWTH+1,L)
         RETURN
      ELSE IF(NUMBER.EQ.1)THEN
C CLEAR...SIMPLY SET NUMBER OF ENTRIES (NUMDIC) = 0
         NUMDIC = 0
         DICTXT(0) = '(none)'
         RETURN
      ENDIF
C LOAD OR MERGE NEED FILENAME
      CALL FILEIN(CLIST,FIRST,LAST,'DICTNARY',FILNAM,RCODE)
      IF(RCODE.NE.0)RETURN
C
C OK, BRANCH TO LOAD OR MERGE
      CALL FSLEN(FILNAM,FLNLEN,LS)
      IF(NUMBER.EQ.2)THEN
C ...LOAD
         GOTO 10
      ELSE
C ...MERGE
         SWQUST = .TRUE.
         GOTO 15
      ENDIF
C
C ** FDICIN ENDS HERE
      END
      SUBROUTINE FQUEST(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This executes ? query.
C Syntax:  ? [phrase]
C
C  If no phrase, introduction is given (from file sysname.HLP)
C  If phrase is specified, dictionary is used (if available).
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*128 CTEMP
      CHARACTER*64  FILNAM
      LOGICAL*1     SW
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      IF(CLIST(FIRST:).EQ.' ')THEN
C INTRODUCTION REQUESTED
          CLIST = SYSNAM
          FIRST = 1
          LAST  = 8
          CALL FILEIN(CLIST,FIRST,LAST,'HELP    ',FILNAM,RCODE)
          IF(RCODE.NE.0)RETURN
C
          CALL FLOPEN(HLPFIL,FILNAM,'FORMATTED','UNKNOWN',*13)
          LINE = 1
          CALL FPRINT(LINE,*90)
      ELSE IF(NUMDIC.GT.0)THEN
C LOOKUP IN DICTIONARY
          NMATCH = 0
250       IF(CLIST(FIRST:FIRST).EQ.' ')THEN
              FIRST = FIRST + 1
              GOTO 250
          ENDIF
          FILNAM = CLIST(FIRST:)
          CALL FSLEN(FILNAM,32,KEYLEN)
C WE HAVE SKIPPED LEADING BLANKS AND PUT THE KEY INTO FILNAM
C                                                     :..SIMPLY HOLDING
C (ALLOWING UP TO 32 CHARS FOR KEY).  NOW READY TO LOOK IN DICTIONARY.
C
          LINE = 1
          ENTRY = 1
C    ::: LOOP OVER DICTIONARY ENTRIES :::
300       CONTINUE
CC 10-22
             IF(DICKEY(ENTRY)(:KEYLEN).EQ.FILNAM .OR.
     1                                    FILNAM.EQ.'*')GOTO 310
C NOT EXACT MATCH...USE FMATCH IN CASE THERE IS A SLIDING MASK
              CALL FMATCH(FILNAM,DICKEY(ENTRY),'*',SW)
              IF( .NOT.SW )GOTO 390
310           CONTINUE
CC =====
C ENTRY MATCHES, SO SET FIRST LINE OF TEXT
              NMATCH = NMATCH + 1
              CLIST = ' '//DICKEY(ENTRY)
              CALL FSLEN(CLIST,32,LAST)
C NOW ADD ENTRY'S TEXT TO END OF KEY
              CLIST(LAST+2:) = DICTXT(ENTRY)
C
C ::: TOP OF LOOP TO COLLECT TEXT AND PRINT :::
350       CONTINUE
            CALL FSLEN(CLIST,128,LAST)
C PRINT TEXT, IF NECESSARY, TO REDUCE CLIST BELOW SCREEN WIDTH
            IF( LAST.GT.SCRWTH )CALL FDTEXT(CLIST,LAST,LINE,*90)
C SEE IF TEXT CONTINUES
            IF( DICKEY(ENTRY+1).EQ.' ' )THEN
C IT DOES, SO ADD IT
               ENTRY = ENTRY + 1
               CTEMP = DICTXT(ENTRY)
               CALL FSLEN(CTEMP,80,LT)
               IF( LAST+LT .LT. 126 )THEN
                  CLIST(LAST+2:) = CTEMP(1:LT)
               ELSE
C WE CAN ADD ONLY PART OF NEW TEXT TO CLIST (THEN PRINT TO MAKE ROOM)
                  L = 126 - LAST
C MOVE BACK TO BLANK
                  IBLANK = L
320               CONTINUE
                  IF( CTEMP(IBLANK:IBLANK).NE.' ' )THEN
                     IBLANK = IBLANK-1
                     IF( IBLANK.GT.0 )GOTO 320
                  ENDIF
                  CALL FLOOK(CTEMP,1,L,'...',IELIPS)
                  IF( IELIPS.GT.IBLANK )IBLANK = IELIPS+2
                  IF( IBLANK.EQ.0 )THEN
                     CALL FDTEXT(CLIST,LAST,LINE,*90)
                     CLIST(LAST+2:) = CTEMP
                     GOTO 350
                  ENDIF
                  CLIST(LAST+2:) = CTEMP(1:IBLANK)
                  LAST = LAST+2+IBLANK
                  CALL FDTEXT(CLIST,LAST,LINE,*90)
C REMOVE LEADING BLANKS IN CONTINUATION OF TEXT
325               CONTINUE
                  IBLANK = IBLANK+1
                  IF( CTEMP(IBLANK:IBLANK).EQ.' ' )THEN
                     IBLANK = IBLANK+1
                     IF( IBLANK.GT.LT )GOTO 350
C                        :... TEXT CONTINUATION HAPPENED TO BECOME NULL
                     GOTO 325
                  ENDIF
                  CLIST(LAST+2:) = CTEMP(IBLANK:LT)
               ENDIF
               GOTO 350
            ENDIF
C FINALLY, PRINT LAST LINE OF ENTRY'S TEXT
          CALL FSLEN(CLIST,128,LAST)
          IF( LAST.GT.0 )CALL FTEXT(CLIST,LAST,5,-4,LINE,'CLEAR',*90)
390     CONTINUE
        ENTRY = ENTRY+1
        IF( ENTRY.LE.NUMDIC )GOTO 300
C     ::: BOTTOM OF DICTIONARY LOOP :::
          CALL FPRMPT(LINE,*90)
             IF(NMATCH.EQ.0)THEN
                PRINT *,' No entries match ',FILNAM
                CALL FSLEN(DICTXT(0),64,LS)
                PRINT *,' ...To see entire dictionary, enter ? *'
             ENDIF
      ELSE
          PRINT *,' Sorry, no dictionary available'
          RCODE = -1
      ENDIF
      RETURN
C
13    PRINT *,' ** I/O Error status ...'
      RCODE = 1
90    RETURN
C
C ** FQUEST ENDS HERE
      END
      SUBROUTINE FDTEXT(CLIST,LAST,LINE,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This prints text, CLIST(:LAST) (using FTEXT), then indents remaining
C text, if any.
C ...Alternate return is user abort (from FTEXT).
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*128 CTEMP
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      CALL FTEXT(CLIST,LAST,5,-4,LINE,'KEEP',*900)
      IF( LAST.EQ.0 )THEN
         LAST = 3
      ELSE
C INDENT REMAINING TEXT
         CTEMP = CLIST
         I = 1
C SKIP LEADING BLANKS IN REMAINING TEXT
100      CONTINUE
         IF( CTEMP(I:I).EQ.' ' )THEN
            I = I+1
            GOTO 100
         ENDIF
         CLIST = '    '//CTEMP(I:LAST)
         CALL FSLEN(CLIST,128,LAST)
      ENDIF
      RETURN
C
900   RETURN 1
C
C ** FDTEXT ENDS HERE
      END
