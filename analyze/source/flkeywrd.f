C                 ::: FLKEYWRD.FOR  8-15-95 :::
C
C LAST DATES:  7-29-90...Replaced OPEN with FLOPEN
C       6-03-92...Changed tabs in FKDISP (8-5-95)
C                 Replaced WRITE with FTEXT
C       8-05-95...Caused underlining to work in FKDISP
C
C This file contains the following FLIP routines.
C
C     FLKEYW...executes _KEYWORD command (called by FLIPMAIN)
C     FKEYIN...reads keywords (if any)
C     FKDISP...displays keys
C     FKCHEK...checks admissibility of a value
C     FKEYWR...writes keys (to output)
C     FKDEBG...debug (uses FLIP's debug switch)
C
      SUBROUTINE FLKEYW(CLIST,FIRST,LAST,SWMSG,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
C  This executes _KEYWORD [command].
C
C  Absent command, this displays the keywords (previously loaded).
C  The command options are:   LOAD, MERGE, CLEAR, SET, WRITE
C  Following LOAD and MERGE, is the filename (_SETUP controls default
C         prefix and suffix).
C  Following SET is the syntax:    name value
C
       CHARACTER*(*) CLIST
       LOGICAL*1     SWMSG
C
C                  DECLARATIONS FOR FKEYWORD ROUTINES
C                  ::::::::::::::::::::::::::::::::::
       CHARACTER*16  FKNAME(100),FKDEFT(100),FKVALU(100),FKADMV(300)
       INTEGER*2     FKVPTR(0:100)
       COMMON/FLKEYC/FKNAME,    FKDEFT,    FKVALU,    FKADMV
       COMMON/FLKEYN/MXFKEY,MXFADM,NFKEYS,FKVPTR
C                  ::::::::::::::::::::::::::::::::::
C LOCAL
       CHARACTER*8   OPTION(6)
       CHARACTER*16  KEYNAM,KEYVAL
       CHARACTER*1   CHAR
       LOGICAL*1     SW
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::
      IF(CLIST(FIRST:).EQ.' ')THEN
C DISPLAY ALL KEYS
          KEYNAM='*'
          CALL FKDISP(KEYNAM)
          RETURN
       ENDIF
C GET OPTION
       NUMBER = 6
       OPTION(1) = 'CLEAR'
       OPTION(2) = 'LOAD'
       OPTION(3) = 'MERGE'
       OPTION(4) = 'WRITE'
       OPTION(5) = 'DISPLAY'
       OPTION(6) = 'SET'
       RCODE = 1
       CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
       IF(RCODE.NE.0)RETURN
C
       IF(NUMBER.EQ.1)THEN
C CLEAR KEYS
          NFKEYS = 0
       ELSE IF(NUMBER.LT.4)THEN
C LOAD/MERGE
          CALL FKEYIN(OPTION(NUMBER),CLIST,FIRST,LAST,RCODE)
       ELSE IF(NUMBER.EQ.4)THEN
C WRITE
          CALL FKEYWR(CLIST,FIRST,LAST,RCODE)
       ELSE IF(NUMBER.EQ.5)THEN
C DISPLAY
          CALL FTOKEN(CLIST,FIRST,LAST,KEYNAM,16,CHAR)
          CALL FKDISP(KEYNAM)
       ELSE
          GOTO 100
       ENDIF
       RETURN
C
100    CONTINUE
C SET keyword = value [...]
       IF(NFKEYS.EQ.0)THEN
          PRINT *,' NO KEYWORDS HAVE BEEN LOADED'
          RETURN
       ENDIF
C
C  ::: TOP OF LOOP OVER SETTINGS (MAY BE MORE THAN 1 PER LINE)
500    CONTINUE
C GET keyword (MASK)
       CALL FTOKEN(CLIST,FIRST,LAST,KEYNAM,16,CHAR)
       IF(KEYNAM.EQ.' ')RETURN
C GET VALUE
       CALL FTOKEN(CLIST,FIRST,LAST,KEYVAL,16,CHAR)
       IF(KEYVAL.EQ.'*' .OR. KEYVAL.EQ.' ')THEN
C SET VALUE=DEFAULT
          KEYVAL = 'DEFAULT'
       ENDIF
C LOOP OVER KEYS TO SET THOSE THAT MATCH keyword
       NMATCH = 0
       NSET   = 0
       RCODE  = 0
C
       DO 900 K=1,NFKEYS
          CALL FMATCH(KEYNAM,FKNAME(K),MASK,SW)
          IF(.NOT.SW)GOTO 900
          NMATCH = NMATCH+1
C ...THIS MATCHES...CHECK ADMISSIBILITY OF VALUE
             CALL FKCHEK(KEYVAL,K,RCODE)
             IF(RCODE.NE.0)THEN
                RCODE = 0
             ELSE
C      THE VALUE (FKVALU(K)) WAS SET BY FKCHEK
                NSET = NSET + 1
             ENDIF
900    CONTINUE
C  ::: END OF MAIN LOOP OVER KEYS :::
C
       IF(NSET.LT.NMATCH .AND. SWMSG)THEN
          PRINT *,' Warning:',NMATCH,' key(s) matched ',KEYNAM
          PRINT *,' but only',NSET,' of them were admissible.'
       ENDIF
C                         ________
                          GOTO 500
C                         ~~~~~~~~
C
C ** FLKEYW ENDS HERE
      END
      SUBROUTINE FKEYIN(OPTION,CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
C  This LOADs|MERGEs keys from file (to be parsed from CLIST(FIRST:LAST))
C
       CHARACTER*(*) OPTION
       CHARACTER*(*) CLIST
C                  DECLARATIONS FOR FKEYWORD ROUTINES
C                  ::::::::::::::::::::::::::::::::::
       CHARACTER*16  FKNAME(100),FKDEFT(100),FKVALU(100),FKADMV(300)
       INTEGER*2     FKVPTR(0:100)
       COMMON/FLKEYC/FKNAME,    FKDEFT,    FKVALU,    FKADMV
       COMMON/FLKEYN/MXFKEY,MXFADM,NFKEYS,FKVPTR
C                  ::::::::::::::::::::::::::::::::::
C LOCAL
       CHARACTER*16  KEYNAM,KEYDEF,KEYVAL
       CHARACTER*64  FILNAM
       LOGICAL       EXIST
       CHARACTER*1   CHAR
       LOGICAL*1     SW
C ::::::::::::::::::: BEGIN :::::::::::::::::::
       IF(OPTION(1:1).NE.'*')GOTO 100
C
C           ..................................
C           : This is initial call from MAIN :
C           :................................:
C
C   The KEY file has the form:
C        name default  value_spec [...]
C
C   where the name of the keyword is 1 to 16 characters with no embedded
C   delimeters (like blanks, commas, equals).  Each value_spec is 1 to
C   16 characters.  If the key is to have numeric values, there is only
C   one value_spec and it must be one of:
C
C          REAL       REAL_POS            REAL_NEG
C                     REAL_NONPOS         REAL_NONNEG
C          INTEGER    INTEGER_POS         INTEGER_NEG
C                     INTEGER_NONPOS      INTEGER_NONNEG
C
C   If the key is not numeric, an arbitrary list is specified (with the
C   first member after the name the default value);  or, the CHARACTER
C   type may be defined as:
C          key name     default         CHARACTER
C
C INITIALIZE NUMBERS....MXFKEY = MAX NUMBER OF KEYS
C                       MXFADM = MAX NUMBER OF ADMISSIBLE VALUES
C                       NFKEYS = NUMBER OF KEYS
C                       FKVPTR(0) = 0 (ALWAYS)
C NOTE: ADMISSIBLE VALUES FOR KEY K IS IN FKADMV(I1),...,FKADMV(I2),
C       WHERE I1 = FKVPTR(K-1) + 1   AND  I2 = FKVPTR(K).
C
C       I2-I1+1 = NUMBER OF ADMISSIBLE VALUES = 1 IFF NUMERIC
C  :::::::::::::::::: SET DIMENSION LIMITS :::::::::::::::::::::::::::
      MXFKEY = 100
      MXFADM = 300
      NFKEYS = 0
C ...AND THE POINTER FOR KEY '0' (FOR EASE OF RETRIEVAL)
      FKVPTR(0) = 0
C SEE IF THERE IS A KEYWORD FILE WITH filename=SYSNAM
      FILNAM = SYSNAM
      CALL FSETNM('KEYWORD ',FILNAM,BEGEXT,RCODE)
      IF(RCODE.NE.0)THEN
         RCODE = 0
         RETURN
      ENDIF
      CALL FINQUR(FILNAM,IOSTAT,EXIST,*99)
      IF(EXIST .AND. IOSTAT.EQ.0)THEN
         OPTION = 'LOAD'
         GOTO 110
      ENDIF
99    RETURN
C :::::::::::: HERE IS WHERE WE LOAD | MERGE KEY FILE :::::::::::
100   CONTINUE
C PREPARE TO OPEN FILE
       CALL FILEIN(CLIST,FIRST,LAST,'KEYWORD ',FILNAM,RCODE)
       IF(RCODE.NE.0)RETURN
110    CONTINUE
C ....USING UNIT HLPFIL
       CALL FLOPEN(HLPFIL,FILNAM,'FORMATTED','OLD',*1300)
       SWINCL = .FALSE.
       INPLIN = 0
C
       IF(OPTION.EQ.'LOAD')NFKEYS=0
C
       MAXKEY = MXFKEY - NFKEYS
       IVPTR = FKVPTR(NFKEYS)
C
       DO 900 K=1,MAXKEY
          CALL FLRDLN(CLIST,HLPFIL,RCODE)
          IF(RCODE)990,800,1310
800       IF(CLIST.EQ.'ENDATA')GOTO 990
C GET KEY NAME
          FIRST = 1
          CALL FSLEN(CLIST,128,LAST)
          CALL FTOKEN(CLIST,FIRST,LAST,KEYNAM,16,CHAR)
C
          IF(NFKEYS.GT.0)THEN
C ...CHECK FOR CONFLICT
             DO 250 I=1,NFKEYS
                CALL FMATCH(KEYNAM,FKNAME(I),' ',SW)
                IF(SW)THEN
                   PRINT *,' ** REJECTED KEY ',KEYNAM,
     1                     '...CONFLICTS WITH ',FKNAME(I)
                   GOTO 1300
                ENDIF
250          CONTINUE
          ENDIF
C
C OK, GET ITS DEFAULT VALUE
          CALL FTOKEN(CLIST,FIRST,LAST,KEYDEF,16,CHAR)
          IF(KEYDEF.EQ.' ')THEN
             PRINT *,' ** MISSING DEFAULT VALUE OF ',KEYNAM
             GOTO 1300
          ENDIF
C
C COLLECT (OTHER) ADMISSIBLE VALUES
500       CONTINUE
            CALL FTOKEN(CLIST,FIRST,LAST,KEYVAL,16,CHAR)
            IF(KEYVAL.EQ.' ')GOTO 590
C CHECK SPACE FOR ADMISSIBLE VALUE(S)
            IF(IVPTR.GE.MXFADM)THEN
                PRINT *,' Sorry, not enough (value) space to add ',
     1                  KEYNAM
                GOTO 1300
            ENDIF
C OK, PUT INTO ADMISSIBLE VALUE LIST (FKADMV)
            IVPTR=IVPTR+1
            FKADMV(IVPTR) = KEYVAL
          GOTO 500
590       CONTINUE
C
C ADD NEW KEY TO LISTS
          NFKEYS = NFKEYS+1
          FKNAME(NFKEYS) = KEYNAM
          FKDEFT(NFKEYS) = KEYDEF
          FKVPTR(NFKEYS) = IVPTR
C CHECK THAT DEFAULT IS ADMISSIBLE
          CALL FKCHEK(KEYDEF,NFKEYS,RCODE)
          IF(RCODE.NE.0)THEN
             PRINT *,' DEFAULT OF ',KEYNAM,' (=',KEYDEF,
     1               ') IS NOT ADMISSIBLE'
             NFKEYS = NFKEYS-1
             GOTO 1300
          ENDIF
C OK  (FKCHEK SET FKVALU=FKDEFT)
900    CONTINUE
       PRINT *,'**',MXFKEY,' ARE TOO MANY KEYS...CONTACT SUPPORT'
       GOTO 1300
C
990    CONTINUE
C ALL AFTER OPEN(HLPFIL) RETURNS COME HERE
       IF(SWINCL)THEN
          CLOSE(INCFIL)
          SWINCL = .FALSE.
       ENDIF
       CLOSE(HLPFIL)
       RETURN
C
C ALL ERROR RETURNS COME HERE
1300   CONTINUE
       IF(NFKEYS.GT.0)PRINT *,' LAST KEY=',FKNAME(NFKEYS)
       RCODE = 1
       GOTO 990
1310   PRINT *,' ** IO ERROR ON UNIT',HLPFIL,' READING KEYS'
       GOTO 1300
C
C ** FKEYIN ENDS HERE
      END
      SUBROUTINE FKDISP(KEYNAM)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
C  This displays keys (to OUTPUT, presumed open).
C  It is entered directly from FLIPSYS if the user specified
C                    _KEYWORD
C  (no setting).
C
      CHARACTER*(*) KEYNAM
C
C                  DECLARATIONS FOR FKEYWORD ROUTINES
C                  ::::::::::::::::::::::::::::::::::
      CHARACTER*16  FKNAME(100),FKDEFT(100),FKVALU(100),FKADMV(300)
      INTEGER*2     FKVPTR(0:100)
      COMMON/FLKEYC/FKNAME,    FKDEFT,    FKVALU,    FKADMV
      COMMON/FLKEYN/MXFKEY,MXFADM,NFKEYS,FKVPTR
C                  ::::::::::::::::::::::::::::::::::
C LOCAL
      CHARACTER*128 CLIST
      LOGICAL*1     SW,SWHEAD
      DATA TABNAM/1/,TABDEF/18/,TABCUR/36/,TABADM/54/
C ::::::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
      PRINT *,NFKEYS,' KEYS...'
      IF(NFKEYS.EQ.0)RETURN
      SWHEAD = .FALSE.
      LINE   = 1
C
C  ::: LOOP OVER KEYWORDS :::
         I1 = 1
      DO 500 K=1,NFKEYS
C DOES KEY MATCH KEYNAME?
         CALL FMATCH(KEYNAM,FKNAME(K),MASK,SW)
         IF(.NOT.SW)THEN
C -NO, JUST ADVANCE POINTER TO TOP OF NEXT KEY
            I1 = FKVPTR(K)+1
            GOTO 500
         ENDIF
C -YES, DID WE PRINT HEADER?
         IF(SWHEAD)GOTO 90
C   -NO, PRINT HEADER
           CLIST = ' '
           CLIST(TABDEF:) = 'DEFAULT'
           CLIST(TABCUR:) = 'CURRENT'
           CLIST(TABADM:) = 'ADMISSIBLE'
           CALL FSLEN(CLIST,128,LAST)
           CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
           CLIST(TABNAM:) = 'KEY '
           CLIST(TABDEF:) = ' VALUE'
           CLIST(TABCUR:) = ' VALUE'
           CLIST(TABADM:) = '  VALUES'
           CALL FSLEN(CLIST,128,LAST)
           CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C UNDERLINE
           LAST = TABADM+15
           CLIST = ' '
           DO 10 I=TABNAM,LAST
10         CLIST(I:I) = '-'
           CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C ::::::::::::::::: HEADER HAS NOW BEEN PRINTED ::::::::::::::
         SWHEAD = .TRUE.
C
90       CONTINUE
         I2 = FKVPTR(K)
         CLIST = FKNAME(K)
         CLIST(TABDEF:) = FKDEFT(K)
         CLIST(TABCUR:) = FKVALU(K)
C LOOP OVER ADMISSIBLE VALUES (MAY BE JUST 1 KEYWORD, OR LIST OF > 1)
100      CLIST(TABADM:) = FKADMV(I1)
           CALL FSLEN(CLIST,128,LAST)
           CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
           CLIST = ' '
           I1 = I1 + 1
         IF(I1.LE.I2)GOTO 100
C NEXT KEY
500   CONTINUE
C
900   RETURN
C
C ** FKDISP ENDS HERE
      END
      SUBROUTINE FKCHEK(KEYVAL,KEY,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
C  This checks admissibility of a value (KEYVAL) for (KEY) number.
C
C  RCODE = 0 UPON RETURN IFF ALL IS WELL, IN WHICH CASE FKVALU(KEY) IS SET.
C  IF ALL IS NOT WELL, BUT RCODE=0 UPON ENTRANCE, THEN NO ERROR MESSAGE
C  IS PRINTED.
C
       CHARACTER*(*) KEYVAL
C
C                  DECLARATIONS FOR FKEYWORD ROUTINES
C                  ::::::::::::::::::::::::::::::::::
       CHARACTER*16  FKNAME(100),FKDEFT(100),FKVALU(100),FKADMV(300)
       INTEGER*2     FKVPTR(0:100)
       COMMON/FLKEYC/FKNAME,    FKDEFT,    FKVALU,    FKADMV
       COMMON/FLKEYN/MXFKEY,MXFADM,NFKEYS,FKVPTR
C                  ::::::::::::::::::::::::::::::::::
C LOCAL
       LOGICAL*1     SW
C ::::::::::::::::::: BEGIN :::::::::::::::::::
       RC = 0
C      :.....INTERNALLY USED RETURN CODE (RCODE'S VALUE IS NEEDED)
C
       IF(KEYVAL.EQ.'DEFAULT' .OR. KEYVAL.EQ.' ')THEN
          FKVALU(KEY)=FKDEFT(KEY)
C         ~~~~~~~~~~~~~~~~~~~~~~~
          GOTO 700
       ENDIF
C SET POINTERS TO ADMISSIBLE VALUES (BESIDES DEFAULT)
       I1=FKVPTR(KEY-1)+1
       I2=FKVPTR(KEY)
       N = I2 - I1 + 1
C CHECK IF KEY IS NUMERIC
       IF(N.NE.1)GOTO 500
C
       IF(FKADMV(I1)(1:4).EQ.'REAL' .OR.
     1    FKADMV(I1)(1:7).EQ.'INTEGER')THEN
C -YES, CONVERT AND CHECK.
             CALL FC2R(KEYVAL,VALKEY,RC)
             IF(RC.NE.0)GOTO 800
C ...CONVERSION SUCCESSFUL...CHECK SIGN
             IF(FKADMV(I1).EQ.'REAL_POS')THEN
                    IF(VALKEY.LE.0.0)GOTO 800
             ELSE IF(FKADMV(I1).EQ.'INTEGER_POS')THEN
                    IF(VALKEY.LT.1. .OR. VALKEY.GT.32000)GOTO 800
             ELSE IF(FKADMV(I1).EQ.'REAL_NEG')THEN
                    IF(VALKEY.GE.0.0)GOTO 800
             ELSE IF(FKADMV(I1).EQ.'INTEGER_NEG')THEN
                    IF(VALKEY.GT.-1. .OR. VALKEY.LT.-32000)GOTO 800
             ELSE IF(FKADMV(I1).EQ.'REAL_NONPOS')THEN
                    IF(VALKEY.GT.0.0)GOTO 800
             ELSE IF(FKADMV(I1).EQ.'INTEGER_NONPOS')THEN
                    IF(VALKEY.GT.0. .OR. VALKEY.LT.-32000)GOTO 800
             ELSE IF(FKADMV(I1).EQ.'REAL_NONNEG')THEN
                    IF(VALKEY.LT.0.0)GOTO 800
             ELSE IF(FKADMV(I1).EQ.'INTEGER_NONNEG')THEN
                    IF(VALKEY.LT.0. .OR. VALKEY.GT.32000)GOTO 800
             ELSE IF(FKADMV(I1).EQ.'INTEGER')THEN
                    IF(ABS(VALKEY).GT.32000)GOTO 800
             ENDIF
C ...NUMERIC VALUE IS ADMISSIBLE, SO
             FKVALU(KEY)=KEYVAL
C            ~~~~~~~~~~~~~~~~~~
             GOTO 700
         ENDIF
C KEY IS NOT NUMERIC TYPE...SEE IF IT'S CHARACTER (WITHOUT LIST)
C
         IF(FKADMV(I1).EQ.'CHARACTER')THEN
C -YES, SO SIMPLY SET VALUE TO WHATEVER THE USER SET
            FKVALU(KEY)=KEYVAL
C           ~~~~~~~~~~~~~~~~~~
            GOTO 700
         ENDIF
C
500      CONTINUE
C
C -NO, LOOK UP IN ADMISSIBILITY LIST
C  ...FIRST CHECK THE DEFAULT (USER MAY HAVE SET = default)
         CALL FMATCH(KEYVAL,FKDEFT(KEY),' ',SW)
         IF(SW)THEN
             FKVALU(KEY) = FKDEFT(KEY)
C            ~~~~~~~~~~~~~~~~~~~~~~~~~
             GOTO 700
         ENDIF
C SET POINTERS (I1,I2) TO ADMISSIBLE VALUES (BESIDES DEFAULT)
         F = 1
         L = 16
         RC = RCODE
         CALL FOPTN(KEYVAL,F,L,FKADMV(I1),N,RC)
         IF(RC.NE.0 .OR. N.EQ.0)GOTO 800
C OK, SET CURRENT KEY VALUE TO THE ONE MATCHED (USER MAY HAVE ABBREVIATED)
         FKVALU(KEY) = FKADMV(I1+N-1)
C        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
700   CONTINUE
C  KEYVAL IS ADMISSIBLE IN KEY
      RCODE = 0
      RETURN
C
800   CONTINUE
C  KEYVAL IS NOT ADMISSIBLE IN KEY
      IF(RCODE.NE.0)THEN
C CALLER WANTS MESSAGE PRINTED
         PRINT *,' ',KEYVAL,' is not admissible for ',FKNAME(KEY)
      ELSE
         RCODE = 1
      ENDIF
      RETURN
C
C ** FKCHEK ENDS HERE
      END
      SUBROUTINE FKEYWR(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
C This writes keys to OUTPUT file, presumed open.
C
       CHARACTER*(*) CLIST
C
C                  DECLARATIONS FOR FKEYWORD ROUTINES
C                  ::::::::::::::::::::::::::::::::::
       CHARACTER*16  FKNAME(100),FKDEFT(100),FKVALU(100),FKADMV(300)
       INTEGER*2     FKVPTR(0:100)
       COMMON/FLKEYC/FKNAME,    FKDEFT,    FKVALU,    FKADMV
       COMMON/FLKEYN/MXFKEY,MXFADM,NFKEYS,FKVPTR
C                  ::::::::::::::::::::::::::::::::::
C LOCAL
      CHARACTER*8   OPTION
      CHARACTER*1   CHAR,SUBCHR
      LOGICAL*1     SW
C ::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::
      IF(NFKEYS.EQ.0)THEN
         PRINT *,' NO KEYS ARE LOADED'
         RETURN
      ENDIF
C DO WE SUBSTITUTE A CHARACTER?
      CALL FTOKEN(CLIST,FIRST,LAST,OPTION,8,CHAR)
      IF(OPTION.NE.' ')THEN
         CALL FMATCH(OPTION,'EXCEPT  ',' ',SW)
         IF(.NOT.SW)THEN
            PRINT *,' ** COULD NOT RECOGNIZE ',OPTION
            RCODE = -1
            RETURN
         ENDIF
C -YES, SO GET SUBSTITUTE CHARACTER
         CALL FTOKEN(CLIST,FIRST,LAST,SUBCHR,1,CHAR)
         PRINT *,' ...WILL SUBSTITUTE BLANK FOR ',SUBCHR
C -NO, SO MAKE SUBSTITUTE=BLANK
      ELSE
         SUBCHR=' '
      ENDIF
C
      CLIST = '*      '//SYSNAM//' GENERATED THESE KEY VALUES'
      WRITE(OUTPUT,1,ERR=1300)CLIST
1     FORMAT(A79)
      CLIST = '*'
      WRITE(OUTPUT,1,ERR=1300)CLIST
      CLIST = '* (* IN COLUMN 1 FOR THOSE AT DEFAULT VALUES)'
      WRITE(OUTPUT,1,ERR=1300)CLIST
      CLIST = '*'
      WRITE(OUTPUT,1,ERR=1300)CLIST
      IF(SUBCHR.NE.' ')THEN
         CLIST = '*  NOTE: '//SYSNAM//' REPLACED '//SUBCHR//' BY BLANK'
         WRITE(OUTPUT,1,ERR=1300)CLIST
         CLIST = '*'
         WRITE(OUTPUT,1,ERR=1300)CLIST
      ENDIF
C
      DO 500 K=1,NFKEYS
         IF(FKVALU(K).EQ.FKDEFT(K))THEN
            CLIST = '*'
         ELSE
            CLIST = ' '
         ENDIF
         CLIST( 6:)=FKNAME(K)
         CLIST(34:)=FKVALU(K)
         IF(SUBCHR.NE.' ')THEN
C SUBSTITUTE CHARACTER FIRST
            DO 150 I=6,50
               IF(CLIST(I:I).EQ.SUBCHR)CLIST(I:I)=' '
150         CONTINUE
         ENDIF
C
         WRITE(OUTPUT,1,ERR=1300)CLIST
500   CONTINUE
C
      RETURN
C
1300  PRINT *,' ** IO ERROR ON OUTPUT FILE'
      RCODE=1
      RETURN
C
C ** FKEYWR ENDS HERE
      END
      SUBROUTINE FKDEBG
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
C This is debug for KEYWORD module.
C
C                  DECLARATIONS FOR FKEYWORD ROUTINES
C                  ::::::::::::::::::::::::::::::::::
       CHARACTER*16  FKNAME(100),FKDEFT(100),FKVALU(100),FKADMV(300)
       INTEGER*2     FKVPTR(0:100)
       COMMON/FLKEYC/FKNAME,    FKDEFT,    FKVALU,    FKADMV
       COMMON/FLKEYN/MXFKEY,MXFADM,NFKEYS,FKVPTR
C                  ::::::::::::::::::::::::::::::::::
C ::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::
      PRINT *,' FKDEBG ENTERED...'
      LINE = SCRWTH+1
      CALL FPRMPT(LINE,*999)
      FLPAUS = FPAUS0
      PRINT *,' MXFKEY=',MXFKEY,' MXFADM=',MXFADM,
     1        ' NFKEYS=',NFKEYS
      PRINT *,' FKVPTR(0)=',FKVPTR(0)
      PRINT *,' NAME        DEFAULT        CURRENT            PTR'
C
      LINE=5
      DO 500 K=1,NFKEYS
         CALL FPRMPT(LINE,*510)
         WRITE(*,1)FKNAME(K),FKDEFT(K),FKVALU(K),FKVPTR(K)
1        FORMAT(3(1X,A16),I4)
500   CONTINUE
C
510   CONTINUE
      N = FKVPTR(NFKEYS)
      PRINT *,N,' ADMISSIBLE VALUES....'
      IF(N.EQ.0)RETURN
      LINE = LINE+1
C
      DO 590 I=1,N,2
         CALL FPRMPT(LINE,*999)
         PRINT *,I,':',FKADMV(I),'     ',I+1,':',FKADMV(I+1)
590   CONTINUE
C
999   RETURN
C
C ** FKDEBG ENDS HERE
      END
