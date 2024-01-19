C               ::: GUPCKLIB.FOR  8-17-95 :::
C
C Last update:  9-5-94
C
C The following are copies from the FLIP library to be linked with
C GUPCKGEN.FOR.
C
C  FLPARSE:  FTOKEN, FLOOK,  FLOOKF, FVRNG, FMATCH, FI2C, FC2I, FSQUEZ
C  FLSCREEN: FPAUSE, FCLOCK, FLTIME, FGTCHR
C  FLSTRING: FSINIT, FSLEN,  FSCASE
C  FLRAND:   FC2R
C
      SUBROUTINE FTOKEN(CLIST,FIRST,LAST,TOKEN,TLENTH,DELMTR)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C GET TOKEN FROM CLIST(FIRST:LAST)
C ...FIRST IS ADVANCED 1 PAST DELIMETER
C ...DLMTR = DELIMETER (FOR CALLING PROGRAM'S USE)
C TOKEN = NULL IF NO TOKEN IS FOUND (EG, FIRST > LAST)
C TLENTH = MAX LENGTH OF TOKEN
C
       PARAMETER (NDLMTR=6)
C                        :...number of delimeters (besides space)
       CHARACTER*(*) CLIST
       CHARACTER*(*) TOKEN
       INTEGER       TLENTH
       CHARACTER*1   DELMTR,DELTBL(NDLMTR)
C
       DATA DELTBL / '=', ',', '(', ')','{','}'  /
C                     1    2    3    4   5   6
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      TOKEN=' '
      IF(FIRST.GT.LAST)RETURN
C SKIP LEADING BLANKS
      DO 10 I=FIRST,LAST
         IF(CLIST(I:I).NE.' ')GOTO 100
10    CONTINUE
C ...BLANK LINE
      FIRST=LAST+1
      RETURN
C
100   CONTINUE
C BEGIN AT FIRST NONBLANK
      FIRST=I
      I2=FIRST+TLENTH-1
      IF(I2.GT.LAST)I2=LAST
C ...GET TOKEN
110   CONTINUE
C
      DO 200 I=FIRST,I2
          DELMTR=CLIST(I:I)
          IF(DELMTR.EQ.' ')GOTO 290
          DO 210 D=1,NDLMTR
210       IF(DELMTR.EQ.DELTBL(D))GOTO 290
200   CONTINUE
      I=I2+1
      DELMTR=CLIST(I:I)
C
290   CONTINUE
      IF(FIRST.LT.I)TOKEN=CLIST(FIRST:I-1)
      FIRST=I+1
      IF(DELMTR.NE.' '.OR.FIRST.GT.LAST)RETURN
C SKIP BLANKS TO GET CORRECT DELIMETER (MAY NOT BE BLANK)
      DO 300 I=FIRST,LAST
         IF(CLIST(I:I).NE.' ')GOTO 310
300   CONTINUE
      FIRST=LAST+1
      RETURN
C
310   CONTINUE
C FIRST NONBLANK AFTER TOKEN...SEE IF DELIMETER
      DO 350 D=1,NDLMTR
         IF(CLIST(I:I).EQ.DELTBL(D))THEN
            FIRST=I+1
            DELMTR=CLIST(I:I)
            RETURN
         ENDIF
350   CONTINUE
      FIRST=I
      RETURN
C
C *** FTOKEN ENDS HERE
      END
      SUBROUTINE FLOOK(CLIST,FIRST,LAST,STRING,IAMIT)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This looks at CLIST(FIRST:LAST) to see if it contains STRING.
C If not, IAMIT=0;  else, CLIST(IAMIT:IAMIT+L-1)=STRING, where L
C is the length of STRING (L <= 32).
C
C ...Search is backward direction (see FLOOKF for forward direction).
C
C Note:  FIRST and LAST are unchanged.
      CHARACTER*(*) CLIST,STRING
      CHARACTER*32  STR32
C :::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      STR32=STRING
      CALL FSLEN(STR32,32,L)
      IF(L.EQ.0 .OR. FIRST+L-1.GT.LAST)THEN
         IAMIT=0
         RETURN
      ENDIF
C SEARCH CLIST(FIRST:LAST) BACKWARDS (RIGHT TO LEFT)
      DO 100 IAMIT=LAST-L+1,FIRST,-1
         IF(CLIST(IAMIT:IAMIT+L-1).EQ.STR32(1:L))RETURN
100   CONTINUE
      IAMIT=0
      RETURN
C
C  ** FLOOK ENDS HERE
      END
      SUBROUTINE FLOOKF(CLIST,FIRST,LAST,STRING,IAMIT)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This looks at CLIST(FIRST:LAST) to see if it contains STRING.
C If not, IAMIT=0;  else, CLIST(IAMIT:IAMIT+L-1)=STRING, where L
C is the length of STRING (<= 32).
C
C If STRING is null, search is for first non-blank
C    (ie, caller wants to skip blanks)
C
C ...Search is forward direction.
C
C Note:  FIRST and LAST are unchanged.
      CHARACTER*(*) CLIST,STRING
      CHARACTER*32  STR32
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      STR32=STRING
      CALL FSLEN(STR32,32,L)
      IF(L.EQ.0)THEN
C SEARCH FOR FIRST NON-BLANK
         DO 10 IAMIT=FIRST,LAST
10       IF(CLIST(IAMIT:IAMIT).NE.' ')RETURN
         IAMIT = LAST+1
         RETURN
      ENDIF
C SEE IF STRING FITS WITHIN FIRST:LAST
      IF(FIRST+L-1.GT.LAST)THEN
         IAMIT=0
         RETURN
      ENDIF
C OK, SEARCH CLIST(FIRST:LAST) FORWARDS (LEFT TO RIGHT)
      DO 100 IAMIT=FIRST,LAST-L+1
         IF(CLIST(IAMIT:IAMIT+L-1).EQ.STR32(1:L))RETURN
100   CONTINUE
      IAMIT=0
      RETURN
C
C  ** FLOOKF ENDS HERE
      END
      SUBROUTINE FVRNG(CLIST,FIRST,LAST,VL,VU,VINF,CHAR,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C PARSE FOR VALUE RANGE
C   V  OR  VL/VU  OR  -*/VU   OR   VL/*   OR   *  OR  -*/*
C (* MEANS INFINITY)
C ...CHAR=LAST DELIMETER (RETURNED TO CALLER)
C
       CHARACTER*(*) CLIST
       REAL          VL,VU,VINF
       CHARACTER*1   CHAR
C LOCAL
       CHARACTER*64  STR64
       CHARACTER*32  STRVAL
       CHARACTER*1   RNGDLM
C SET RANGE DELIMITER
       DATA RNGDLM/'/'/
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
C GET RANGE (TOKEN OF 64 CHARS)
      CALL FTOKEN(CLIST,FIRST,LAST,STR64,64,CHAR)
      IF(STR64.EQ.' ')THEN
         PRINT *,' ** Value expected'
         RCODE = -1
         RETURN
      ENDIF
C SEE IF RANGE IS PRESENT
      CALL FLOOK(STR64,1,64,RNGDLM,IRANGE)
      IF(IRANGE.EQ.0)THEN
         STRVAL = STR64
      ELSE
         STRVAL = STR64(1:IRANGE-1)
      ENDIF
C SET VL
      IF(STRVAL.EQ.'*')THEN
         VL = VINF
      ELSE IF(STRVAL.EQ.'-*')THEN
         VL = -VINF
      ELSE
         CALL FC2R(STRVAL,VL,RCODE)
         IF(RCODE.NE.0)RETURN
      ENDIF
C SET VU
      IF(IRANGE.EQ.0)THEN
C SPECIFIC VALUE (EQUALITY)
         VU = VL
C          NOTE:  INFINITY IS A TRUE VALUE WITH SAME SYNTAX
C                 AS FINITE VALUES--IE, V=* WILL RETURN VL=VU=VINF,
C                 V=-* WILL RETURN VL=VU=-VINF, AND V=-*/* (OR */*)
C                 WILL RETURN VL=-VINF, VU=VINF.
      ELSE
C RANGE
         STRVAL = STR64(IRANGE+1:)
         IF(STRVAL.EQ.' ')THEN
            PRINT *,' ** UPPER VALUE OF RANGE EXPECTED AFTER ',RNGDLM
            RCODE = -1
         ELSE IF(STRVAL.EQ.'*')THEN
            VU = VINF
         ELSE
C CONVERT STRING TO SET UPPER BOUND OF RANGE
            CALL FC2R(STRVAL,VU,RCODE)
         ENDIF
C ADJUST VL=-VINF IF CALLER HAD */v (SEE ABOVE NOTE)
         IF(VL.GE.VINF)VL = -VINF
      ENDIF
C
      IF(VL.GT.VU)THEN
C   HERE IS WHERE BOUNDS OUT OF ORDER ARE ADJUSTED
C   ...SECOND VALUE WILL BE TREATED AS INCREMENT TO FIRST.
C      FOR EXAMPLE, 1000/5 RETURNS VL=1000 AND VU=1005;
C                  1000/-5 RETURNS VL= 995 AND VU=1000.
         IF(VU.GE.0)THEN
            VU = VL + VU
         ELSE
            V  = VL + VU
            VU = VL
            VL = V
         ENDIF
      ENDIF
      RETURN
C
C  ** FVRNG ENDS HERE
      END
      SUBROUTINE FMATCH(NAME1,NAME2,MASKIN,ANSWER)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C TEST IF NAME1 MATCHES NAME2
C ...ANSWER = TRUE IFF EACH NON-MASK CHARACTER OF NAME1 EQUALS THE
C      CORRESPONDING CHARACTER OF NAME2
C NOTE: MASKIN=' ' ASKS IF NAME1 IS A TELESCOPIC ABBREVIATION OF NAME2
C       TRAILING BLANKS IN NAME1 ARE SAME AS MASK CHARS.
C
C  EXAMPLES:  'ABC*D','ABCXD','*' ===> ANSWER=TRUE
C             'ABC'  ,'ABCD', any ===> ANSWER=TRUE
C             'A'    ,'ABCD', any ===> ANSWER=TRUE
C             'A'    ,'a',    any ===> ANSWER=FALSE
C
       CHARACTER*(*) NAME1,NAME2
       CHARACTER*32  A,B
C                :......MAX LENGTH OF NAME1 & NAME2
       CHARACTER*1   MASKIN
       LOGICAL*1     ANSWER
C LOCAL
       CHARACTER*1   SMCHAR
       DATA          SMCHAR /'"'/
C
C SMCHAR IS A SLIDING MASK CHAR, ADDED 3-24-92, TO ALLOW A NAME
C MATCH THAT IS POSITION INDEPENDENT.  IF SMCHAR IS THE FIRST CHAR
C OF NAME1, THE TEST LOOKS FOR THE REMAINING STRING IN NAME2.
C EXAMPLES:
C      NAME1  NAME2       MASKIN   ANSWER
C      "ABC   XXXABCYYY   any      TRUE
C      "ABC   XAXBC       any      FALSE
C      "      any         any      TRUE
C      "A*B   XAYB        *        TRUE
C      "A*B   XAB         any      FALSE
C
C GENERAL MASK HAS FORM:   [fixed]["sliding]
C OLD MASK IS FIXED (NO " PRESENT).  ABOVE EXAMPLES HAVE fixed NULL,
C SO THE SLIDING MASK CAN MATCH ANYWHERE.  HERE ARE EXAMPLES OF THE
C GENERAL FORM:
C      NAME1  NAME2       MASKIN   ANSWER
C      A"BC   XXXABCYYY   any      FALSE
C      A"BC   AXBC        any      TRUE
C      C"A*B  CXXAYB      *        TRUE
C      C"A*B  CAB         any      FALSE
C      *"A    AX          any      FALSE
C      *"A    XA          *        TRUE
C
C NOTE FROM LAST 2 CASES THAT THE SLIDING MASK MUST MATCH THE
C SUBSTRING THAT REMAINS AFTER THE FIXED MASK IS MATCHED -- EG,
C AX FAILS TO MATCH *"A BECAUSE THE A IS NOT AFTER POSITION 1
C :::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      A = NAME1
      B = NAME2
      ANSWER = A.EQ.B
      IF(ANSWER)RETURN
CC 7-27-95
      IF( MASKIN.EQ.' ' )CALL FSCASE(A,1,32)
C CALLER WANTS TO MATCH TELOSCOPIC ABBREV...MUST BE KEYWORD (EG, COMMAND)
C CONVERT A TO UPPER CASE (JUST LOCALLY HERE) TO MAKE CASE INSENSITIVE
CC =======
      CALL FLOOKF(A,1,32,SMCHAR,ISLIDE)
      IF(ISLIDE.GT.0)GOTO 500
C WE HAVE FIXED MASK (NO SLIDING MASK)
      DO 100 I=1,32
       IF(A(I:I).EQ.' ')GOTO 200
       IF(A(I:I).NE.B(I:I).AND.A(I:I).NE.MASKIN)RETURN
100   CONTINUE
200   CONTINUE
      ANSWER=.TRUE.
      RETURN
C
500   CONTINUE
C NAME1 HAS SLIDING MASK CHAR AT ISLIDE
      IF(ISLIDE.GT.1)THEN
C FIRST MATCH FIXED MASK
         DO 550 I=1,ISLIDE-1
            IF(A(I:I).NE.B(I:I).AND.A(I:I).NE.MASKIN)RETURN
550      CONTINUE
      ENDIF
C FIXED MASK MATCHES...NOW MATCH SLIDING MASK
      A = NAME1(ISLIDE+1:)
      B = NAME2(ISLIDE:)
      IF(A.EQ.B.OR.A.EQ.MASKIN)GOTO 200
C               ~~~~~~~~~~~~~~ADDED 4-28-93
      IF(A.EQ.' '.OR.B.EQ.' ')RETURN
      CALL FSLEN(B,32,LB)
      CALL FSLEN(A,32,LA)
C REMOVE TRAILING MASK CHARS FROM A
650   CONTINUE
      IF(A(LA:LA).EQ.MASKIN)THEN
         IF(LA.EQ.0)GOTO 200
C                  (IF A IS ALL MASK CHARS, ANSWER=TRUE)
         A(LA:LA) = ' '
         LA = LA-1
         IF(LA.GT.0)GOTO 650
         GOTO 200
      ENDIF
      IF(LA.GT.LB)RETURN
C WE NOW KNOW THAT THE LENGTH OF A (LA) IS <= LENGTH OF B (LB), SO
C A MATCH IS POSSIBLE.
      DO 800 K=0,LB-LA
C START COMPARING A AT B(K+1:)
         DO 750 I=1,LA
750      IF(A(I:I).NE.MASKIN .AND. A(I:I).NE.B(K+I:K+I))GOTO 800
C WE HAVE A MATCH...A MATCHES B(K+1:K+LA)
         GOTO 200
800   CONTINUE
C
C NO MATCH
      RETURN
C
C ** FMATCH ENDS HERE
      END
      SUBROUTINE FI2C(STRING,INT,FIRST)
C     ===============
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C CONVERT POSITIVE INTEGER (INT) TO STRING
C ...RIGHT JUSTIFIED WITH FIRST DIGIT (IE, NONBLANK) RETURNED
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*(*) STRING
C ::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      NUMBER=INT
C
C USE HORNER'S RULE
C
      DO 100 FIRST=8,1,-1
         DIGIT = NUMBER - 10*(NUMBER/10)
         STRING(FIRST:FIRST) = CHRNUM(DIGIT)
         NUMBER = NUMBER/10
         IF(NUMBER.EQ.0)RETURN
100   CONTINUE
C
      FIRST = 1
      RETURN
C
C **  FI2C ENDS HERE
      END
      SUBROUTINE FC2I(STRING,LENGTH,INT,RCODE)
C     ===============
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C CONVERT STRING TO INTEGER...NON-DIGIT/NON-BLANK YIELDS RCODE=-1
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*(*) STRING
      INTEGER       LENGTH
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      IF(STRING.EQ.'*')THEN
C INFINITY
         INT = 32000
         RETURN
      ENDIF
      INT = 0
C
      DO 100 I=1,LENGTH
         DO 50 DIGIT=0,9
           IF(STRING(I:I).EQ.CHRNUM(DIGIT))THEN
              INT = INT*10 + DIGIT
              GOTO 100
           ENDIF
50       CONTINUE
C STRING(I:I) NOT A DIGIT
         IF(STRING(I:I).NE.' ')RCODE = -1
         RETURN
100   CONTINUE
C
      RETURN
C
C  ** FC2I ENDS HERE
      END
      SUBROUTINE FSQUEZ(STRING,LENGTH)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C SQUEEZE BLANKS OUT OF STRING (LEFT JUSTIFY)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*(*) STRING
      CHARACTER*128 CLIST
      INTEGER       LENGTH
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      IF(LENGTH.LE.0)RETURN
      CLIST = ' '
      L = 0
      DO 100 K=1,LENGTH
         IF(STRING(K:K).NE.' ')THEN
            L=L+1
            CLIST(L:L) = STRING(K:K)
         ENDIF
100   CONTINUE
C
      LENGTH = L
      STRING = CLIST(:LENGTH+1)
      RETURN
C
C  ** FSQUEZ ENDS HERE
      END
      SUBROUTINE FPAUSE(PAUSE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C PROMPT FOR PAUSE VALUE
C
      CHARACTER*12 STRVAL
      LOGICAL*1    SW
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      STRVAL = ' '
1     PRINT *,' ENTER PAUSE VALUE (',PAUSE,') '
      READ(*,'(A12)')STRVAL
      IF(STRVAL.EQ.' ')RETURN
      IF(STRVAL.EQ.'*')THEN
         PAUSE = 32000
         RETURN
      ENDIF
C PARSE VALUE
      RCODE = 0
      SW = STRVAL(1:1).EQ.'-'
      IF(SW)STRVAL(1:1) = ' '
      CALL FC2I(STRVAL,12,INT,RCODE)
      IF(RCODE.EQ.0)THEN
         IF(SW)THEN
            PAUSE = -INT
         ELSE
            PAUSE = INT
         ENDIF
         RETURN
      ENDIF
      PRINT *,' ?',STRVAL
      GOTO 1
C
C ** FPAUSE ENDS HERE
      END
      SUBROUTINE FCLOCK(SECOND)
C     =================
C Returns time in seconds, if available.  If not, returns -1.
      INTEGER SECOND
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      SECOND = -1
C LAHEY TIMER GIVES NUMBER OF TICKS SINCE MIDNIGHT
CLAHEY      CALL TIMER(SECOND)
C ...EACH TICK = 100-TH OF A SECOND
CLAHEY      SECOND = SECOND/100
C RISC CLOCK GIVES CPU CLOCK TIME IN SECONDS
CAIX       CALL CLOCK(SECOND)
      RETURN
C
C ** FCLOCK ENDS HERE
      END
      SUBROUTINE FLTIME(ELAPSE,MAXTIM,*)
C     =================
C Time interrupt...alternate return means user aborts.
C    ELAPSE = Elapsed time (caller got this).
C    MAXTIM = Setting of time for interrupt...user can change.
C
      INTEGER     ELAPSE,MAXTIM
C LOCAL
      CHARACTER*1  CHAR
      CHARACTER*12 STR12
      INTEGER     TIME
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      PRINT *,ELAPSE,' = ELAPSED TIME...Continue ',
     1     '(Y/N, or T to change TIME)? '
      CALL FGTCHR('YNT','Y',CHAR)
      IF( CHAR.EQ.'N' )RETURN 1
      IF( CHAR.EQ.'T' )THEN
C USER WANTS TO CHANGE MAXTIM
10       PRINT *,MAXTIM,' = TIME...Enter change (nothing to accept)'
         READ(*,'(A12)')STR12
         IF( STR12.EQ.' ' )RETURN
         RCODE = 0
         CALL FC2I(STR12,12,TIME,RCODE)
         IF( RCODE.NE.0 .OR. TIME.LT.0 )THEN
            PRINT *,' Enter TIME as a positive integer'
         ELSE
            MAXTIM = TIME
         ENDIF
         GOTO 10
      ENDIF
C
      RETURN
C
C ** FLTIME ENDS HERE
      END
      SUBROUTINE FGTCHR(STRING,DEFALT,RESPNS)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
c
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
C This gets 1-char response from terminal.  Choices are (STRING)
C with default (DEFAULT).
      CHARACTER*(*) STRING
      CHARACTER*1   DEFALT,RESPNS
C
C       STRING null causes a SYSERR message
C       Max length of string = 8 (= number of responses)
C       DEFALT null makes a response mandatory;  else, DEFALT is a
C          char in STRING and RESPNS=DEFALT if user just presses enter
C       Upon return, RESPNS = blank means user refuses to answer
C          (wants to abort)
C
C ADAPTED FROM ROUTINE WRITTEN BY Dave R. Heltne, Optimization Dept.,
C Shell Development Co., May 11, 1992.
C
C LOCAL
      CHARACTER*8 STR8
      CHARACTER*1 ANSWER
      LOGICAL*1   SW
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      STR8 = STRING
      CALL FSLEN(STR8,8,NUMBER)
      IF(NUMBER.LE.0)THEN
         PRINT *,' ** SYSERR FGTCHR...NULL STRING'
         GOTO 1390
      ENDIF
      SW = .FALSE.
C          :..SAYS USER HAS NOT RESPONDED YET
10    CONTINUE
      ANSWER = ' '
      READ(*,FMT='(A1)') ANSWER
CITSO      READ(*,FMT='(A1)') ANSWER
CICMS      READ(TTYIN,FMT='(A1)',END=100,ERR=1300)ANSWER
100   CONTINUE
CICMS      REWIND TTYIN
      IF(ANSWER.EQ.' ')THEN
C RESPONSE = NULL
         IF(DEFALT.NE.' ')THEN
C ...USE DEFAULT
            RESPNS = DEFALT
            RETURN
         ENDIF
C ...NO DEFAULT...ABORT IF 2ND ATTEMPT
         GOTO 1300
      ENDIF
CICMS      CALL FUPPER(ANSWER)
C
      IF(ANSWER.EQ.'?')THEN
         PRINT *,' ENTER ONE OF:',(' ',STR8(I:I),I=1,NUMBER)
         IF(DEFALT.NE.' ')PRINT *,' ...DEFAULT IS ',DEFALT
         GOTO 10
      ENDIF
C USER GAVE ANSWER...LOOKUP IN STRING
      DO 500 I=1,NUMBER
         IF(ANSWER.EQ.STRING(I:I))THEN
C OK
            RESPNS = ANSWER
            RETURN
         ENDIF
500   CONTINUE
C
C ERROR
1300  CONTINUE
      IF(.NOT.SW)THEN
         PRINT *,' MUST ENTER ONE OF:',(' ',STR8(I:I),I=1,NUMBER)
         PRINT *,' ...TRY AGAIN '
         SW = .TRUE.
         GOTO 10
      ENDIF
1390  PRINT *,' ...ABORTING'
      RESPNS = ' '
      RETURN
C
C ** FGTCHR ENDS HERE
      END
      SUBROUTINE FSINIT
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C THIS ENTRY CLEARS ALL STRINGS AND INITIALIZES
C (MUST BE CALLED BEFORE OTHER ENTRIES)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      NUMSTR=0
C
      ASCII( 0) = '0'
      ASCII( 1) = '1'
      ASCII( 2) = '2'
      ASCII( 3) = '3'
      ASCII( 4) = '4'
      ASCII( 5) = '5'
      ASCII( 6) = '6'
      ASCII( 7) = '7'
      ASCII( 8) = '8'
      ASCII( 9) = '9'
      ASCII(10) = 'A'
      ASCII(11) = 'B'
      ASCII(12) = 'C'
      ASCII(13) = 'D'
      ASCII(14) = 'E'
      ASCII(15) = 'F'
      ASCII(16) = 'G'
      ASCII(17) = 'H'
      ASCII(18) = 'I'
      ASCII(19) = 'J'
      ASCII(20) = 'K'
      ASCII(21) = 'L'
      ASCII(22) = 'M'
      ASCII(23) = 'N'
      ASCII(24) = 'O'
      ASCII(25) = 'P'
      ASCII(26) = 'Q'
      ASCII(27) = 'R'
      ASCII(28) = 'S'
      ASCII(29) = 'T'
      ASCII(30) = 'U'
      ASCII(31) = 'V'
      ASCII(32) = 'W'
      ASCII(33) = 'X'
      ASCII(34) = 'Y'
      ASCII(35) = 'Z'
      ASCII(36) = '0'
C
      RETURN
C
C ** FSINIT ENDS HERE
      END
      SUBROUTINE FSLEN(STRING,MAX,LENGTH)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C RETURNS TRUE LENGTH OF STRING (EXCLUDING TRAILING BLANKS)
C
       CHARACTER*(*) STRING
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      DO 10 LENGTH=MAX,1,-1
10    IF(STRING(LENGTH:LENGTH).NE.' ')RETURN
      LENGTH = 0
      RETURN
C
C ** FSLEN ENDS HERE
      END
      SUBROUTINE FSCASE(STRING,FIRST,LAST)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C CONVERT LOWER TO UPPER CASE IN STRING(FIRST:LAST)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*(*) STRING
C LOCAL
      CHARACTER*1   CHAR, LOWCAS(26)
      DATA LOWCAS/'a','b','c','d','e','f','g','h','i','j','k','l','m',
     1            'n','o','p','q','r','s','t','u','v','w','x','y','z'/
C :::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      DO 100 I=FIRST,LAST
         CHAR = STRING(I:I)
         DO 50 K=1,26
            IF( CHAR.EQ.LOWCAS(K) )THEN
               STRING(I:I) = ASCII(K+9)
               GOTO 100
            ENDIF
50       CONTINUE
100   CONTINUE
      RETURN
C
C  ** FSCASE ENDS HERE
      END
      SUBROUTINE FC2R(STRING,VALUE,RCODE)
C     ===============
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C CONVERT STRING TO VALUE
C WARNING: THIS IS FOR PARSING LINE, NOT EFFICIENT FOR DATA...
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
       CHARACTER*(*) STRING
C LOCAL
       CHARACTER*32  SLOCAL
       CHARACTER*1   CHAR
       REAL          VALUE
       PARAMETER     (VINF = 1.0E+20)
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
C INITIALIZE
      VALUE = 0.0
C COPY TO SLOCAL (NOTE: MAX LENGTH OF STRING = 32)
      SLOCAL = STRING
      CALL FSLEN(SLOCAL,32,LENGTH)
      IF(LENGTH.EQ.0)RETURN
C
C LOOK FOR SIGN
      SIGN = 1
      DO 10 K=1,LENGTH
         CHAR = SLOCAL(K:K)
         IF(CHAR.EQ.' ')GOTO 10
         IF(CHAR.EQ.'-')THEN
             SIGN = -1
             GOTO 20
         ELSE IF(CHAR.EQ.'+')THEN
             GOTO 20
         ELSE
             GOTO 30
         ENDIF
10    CONTINUE
C STRING = '-' or '+' (NO NUMBERS TAKEN AS 0)
      RETURN
C
20    L = K + 1
C SKIP BLANKS
      DO 25 K=L,LENGTH
         CHAR = SLOCAL(K:K)
         IF(CHAR.NE.' ')GOTO 30
25    CONTINUE
      RETURN
C
30    CONTINUE
C NOW SIGN IS SET AND SLOCAL(K:12) CONTAINS REAL VALUE
      IF(SLOCAL(K:K).EQ.'*')THEN
         VALUE = SIGN*VINF
         RETURN
      ENDIF
C
40    CONTINUE
C BEGIN NUMERIC PARSE...INITIALIZE EXPONENT (EXPNT)
      EXPNT = 0
C
C USING HORNER'S RULE TO PARSE STRING(K:)
C
      DO 100  L=K,LENGTH
         CHAR = SLOCAL(L:L)
         DO 50 DIGIT=0,9
            IF(CHAR.EQ.CHRNUM(DIGIT))THEN
                VALUE = 10.*VALUE + FLOAT(DIGIT)
                GOTO 100
            ENDIF
50       CONTINUE
C
C     SEE IF DECIMAL POINT
         IF(CHAR.EQ.'.')THEN
            IF(EXPNT.NE.0)GOTO 13
            EXPNT = L - LENGTH
         ELSE IF(CHAR.EQ.'E'.OR.CHAR.EQ.'D')THEN
            GOTO 200
         ELSE
            GOTO 13
         ENDIF
100   CONTINUE
C
C INTEGER OR F-FORMAT...ADJUST FOR EXPONENT AND SIGN
C
      GOTO 900
C
200   CONTINUE
C
C E-FORMAT (OR D-)
C  ...ADJUST EXPONENT, WHICH WAS SET BY LENGTH
      IF(EXPNT.NE.0)EXPNT = EXPNT + LENGTH - L + 1
      L = L + 1
C SEE IF EXPONENT HAS SIGN
      CHAR = SLOCAL(L:L)
      ESIGN = 1
      IF(CHAR.EQ.'-')THEN
         ESIGN = -1
         L = L + 1
      ELSE IF(CHAR.EQ.'+')THEN
         L = L + 1
      ENDIF
C
      CHAR = SLOCAL(L:L)
C PARSE EXPONENT (1 OR 2 DIGITS)
      DO 210 E=0,9
         IF(CHAR.EQ.CHRNUM(E))GOTO 220
210   CONTINUE
      GOTO 13
220   CONTINUE
C
      IF(L.LT.LENGTH)THEN
          IF(L+1.LT.LENGTH)GOTO 13
          CHAR = SLOCAL(LENGTH:)
C     ...2ND DIGIT OF EXPONENT
          DO 230 DIGIT=0,9
             IF(CHAR.EQ.CHRNUM(DIGIT))THEN
                E = 10*E + DIGIT
                GOTO 250
             ENDIF
230       CONTINUE
          GOTO 13
      ENDIF
C
C COMBINE EXPONENTS (EXPNT FROM DECIMAL POINT AND E FROM E-FORMAT)
250   CONTINUE
      EXPNT = EXPNT + E*ESIGN
C
900   CONTINUE
      VALUE = SIGN*VALUE*(10.**EXPNT)
      RETURN
C
C ** ERROR RETURN
13    PRINT *,' ** NUMERIC FORMAT ERROR...',STRING
      SLOCAL = ' '
      SLOCAL(L:) = '?'
      PRINT *,'                          ',SLOCAL(:L)
      RCODE = 1
      RETURN
C
C ** FC2R ENDS HERE
      END
