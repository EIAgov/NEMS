C              ::: FLPARSE.FOR  8-08-95 :::
C
C Earlier dates deleted.
C       7-25-94...Added case insensitive test in FMATCH (CC 7-27)
C       7-27-95...Added FCL2IN
C       8-08-95...Cosmetic change in FC2I
C
C This file contains the following FLIP subroutines.
C
C For parsing and related functions.
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C    FTOKEN...parses string for next token
C    FOPTN....parses string for option from list   (1 choice)
C    FOPSET...parses string for option(s) from set (may be multiple)
C    FLOOK....sees if special string has been specified (from rear)
C    FLOOKF...sees if special string has been specified (from front)
C    FVRNG....parses numeric range
C    FCL2IN...parses for non-negative integer
C    FMATCH...determines if one string matches another
C    FI2C.....convert integer to character
C    FC2I.....convert character to integer
C    FSQUEZ...squeeze blanks out of string
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
      SUBROUTINE FOPTN(CLIST,FIRST,LAST,OPLIST,NUMBER,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C GET OPTION NUMBER FROM CLIST
C ...OPLIST=OPTIONS LIST
C ...NUMBER=LENGTH OF OPLIST AT INPUT (=NUMBER OF OPTIONS)
C          =OPTION NUMBER AT OUTPUT (=0 IF NONE)
C RCODE=-1 IF OPTION NOT IN LIST
C UPON ENTRANCE, RCODE=0 MEANS OPTION IS NOT MANDATORY
C                (NO MESSAGE PRINTED)
       CHARACTER*(*) OPLIST(2)
       CHARACTER*(*) CLIST
C LOCAL
       CHARACTER*128 CTEMP
       CHARACTER*16  TOKEN
       CHARACTER*1   CHAR
       LOGICAL*1     SW
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      IF(NUMBER.LE.0)RETURN
      CALL FTOKEN(CLIST,FIRST,LAST,TOKEN,16,CHAR)
      IF(TOKEN.EQ.' ')THEN
C NO OPTION SPECIFIED...PRINT ERROR MESSAGE IF MANDATORY
C ...IE,
         IF(RCODE.NE.0)THEN
            GOTO 1300
         ENDIF
C NOT MANDATORY, SO JUST RETURN NO OPTION SPECIFIED
C ...IE,
         NUMBER = 0
         RETURN
      ENDIF
C
C SEARCH FOR OPTION
      N = 0
      CALL FSLEN(TOKEN,16,LS)
C
      DO 10 I=1,NUMBER
         CALL FMATCH(TOKEN,OPLIST(I),' ',SW)
         IF(SW)THEN
            IF(N.GT.0)THEN
               PRINT *,TOKEN(:LS),' ambiguous...could be ',
     1                 OPLIST(I),' or ',OPLIST(N)
               RCODE = -1
               RETURN
            ELSE
               N = I
            ENDIF
         ENDIF
10    CONTINUE
C
      IF(N.GT.0)THEN
C GOT OPTION
         NUMBER = N
         RCODE = 0
         RETURN
      ENDIF
C NO MATCH
      PRINT *,' ** ',TOKEN(:LS),' option not recognized'
C
C ERROR RETURN
1300  CONTINUE
      IF( NUMBER.EQ.1 )THEN
         PRINT *,' ...Option must be ',OPLIST(1)
         GOTO 1390
      ENDIF
      PRINT *,' ...Option must be one of:'
C GIVE OPTIONS
C FIRST, GET LONGEST LENGTH
      LMAX = 1
      DO 1305 I=1,NUMBER
         TOKEN = OPLIST(I)
         CALL FSLEN(TOKEN,16,LS)
         IF(LS.GT.LMAX)LMAX = LS
1305  CONTINUE
      N = 6*(NUMBER/6)
      IF( N.GT.0 )THEN
         DO 1310 I=1,N,6
            PRINT *,' ',OPLIST(I)(:LMAX),' ',OPLIST(I+1)(:LMAX),
     2            ' ',OPLIST(I+2)(:LMAX),' ',OPLIST(I+3)(:LMAX),
     3            ' ',OPLIST(I+4)(:LMAX),' ',OPLIST(I+5)(:LMAX)
1310     CONTINUE
      ENDIF
      PRINT *,(' ',OPLIST(I)(:LMAX),I=N+1,NUMBER)
1390  RCODE = -1
CC ADDED 7-25-94
      IF( FIRST.LT.80 )THEN
         CTEMP = ' '
         CTEMP(FIRST:) = ':.Parser is here'
         PRINT *,' ',CLIST(:78)
         PRINT *,' ',CTEMP(:78)
      ENDIF
      RETURN
C
C  ** FOPTN ENDS HERE
      END
      SUBROUTINE FOPSET(CLIST,FIRST,LAST,OPLIST,NUMBER,PICK,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C GET OPTION(S) PICKED FROM OPLIST(1),...,OPLIST(NUMBER)
C
       CHARACTER*8   OPLIST(2),TOKEN
       CHARACTER*(*) CLIST
       LOGICAL*1     PICK(2)
C
C PICK(I) = .TRUE. IFF I-TH OPTION WAS PICKED.
C
C  *********************************************************************
C    OPTIONS MUST BE SEPARATED BY COMMAS (NOT BLANKS), SO PARSE STOPS
C    WHEN FIRST TIME DELIMETER IS NOT COMMA.
C  *********************************************************************
C
C RCODE = -1 IF OPTION PICKED THAT IS NOT IN LIST
C
C UPON ENTRANCE, RCODE=0 MEANS PICK NOT MANDATORY
C             (THEN PICK(I)=.FALSE. ALL I=1,...,NUMBER)
C ...RCODE <> 0 MEANS AT LEAST 1 PICK IS MANDATORY
C
C FIRST IS ADVANCED PAST THE OPTIONS PICKED AND THE DELIMETER (CHAR) IS
C INSERTED INTO CLIST(FIRST-1:FIRST-1)
C
C THE USER CAN PICK ALL OPTIONS BY *
C
       CHARACTER*1   CHAR
       LOGICAL*1     SW,SWPICK
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
C INITIALIZE ALL PICKS AS FALSE
      DO 5 I=1,NUMBER
5     PICK(I) = .FALSE.
      SWPICK = .FALSE.
C LOOK FOR AN OPTION
      CALL FTOKEN(CLIST,FIRST,LAST,TOKEN,8,CHAR)
      IF(TOKEN.EQ.' ')THEN
C NO OPTION SPECIFIED...PRINT ERROR MESSAGE IF MANDATORY
C ...IE,
         IF(RCODE.NE.0)THEN
            PRINT *,' ** Option missing...specify at least 1 of:'
            GOTO 1300
         ENDIF
C NOT MANDATORY, SO JUST RETURN
         RETURN
      ENDIF
C
      IF(TOKEN.EQ.'*')THEN
C PICK ALL OPTIONS
         DO 25 I=1,NUMBER
25       PICK(I) = .TRUE.
         RCODE = 0
         GOTO 900
      ENDIF
C
C SEARCH FOR OPTION
90    N = 0
C
      DO 100 I=1,NUMBER
         CALL FMATCH(TOKEN,OPLIST(I),' ',SW)
         IF(SW)THEN
            IF(N.GT.0)THEN
               PRINT *,TOKEN,' ambiguous...could be ',OPLIST(I),
     1                ' or ',OPLIST(N)
               RCODE = -1
               RETURN
            ELSE
               N = I
            ENDIF
         ENDIF
100   CONTINUE
C
      IF(N.GT.0)THEN
         PICK(N) = .TRUE.
         SWPICK = .TRUE.
         IF(CHAR.EQ.',')THEN
            CALL FTOKEN(CLIST,FIRST,LAST,TOKEN,8,CHAR)
            GOTO 90
         ENDIF
         RCODE = 0
      ELSE
C NO MATCH
         IF(.NOT.SWPICK .AND. TOKEN.EQ.' ')
C ...NO OPTION WAS PICKED AND WAS NOT REQUIRED
     1       RETURN
C  THIS WAS SUPPOSED TO BE AN OPTION OR OPTION PICKED NOT IN LIST
         CALL FSLEN(TOKEN,8,LS)
         PRINT *,' ** ',TOKEN(:LS),' option not recognized...must be'
         IF(NUMBER.EQ.1)THEN
            PRINT *,' ',OPLIST(1)
            GOTO 1390
         ENDIF
         PRINT *,'    one of:'
         GOTO 1300
      ENDIF
C OK, GOT OPTIONS PICKED...PUT CHAR OF LAST PARSE FOR CALLER
900   CLIST(FIRST-1:FIRST-1) = CHAR
      RETURN
C
C ERROR RETURN
1300  CONTINUE
C GIVE OPTIONS
      N = 8*(NUMBER/8)
      IF(N.GT.0)THEN
         DO 1310 I=1,N,8
            PRINT *,' ',OPLIST(I),' ',OPLIST(I+1),' ',OPLIST(I+2),
     1             ' ',OPLIST(I+3),' ',OPLIST(I+4),' ',OPLIST(I+5),
     2             ' ',OPLIST(I+6),' ',OPLIST(I+7)
1310     CONTINUE
      ENDIF
      PRINT *,(' ',OPLIST(I),I=N+1,NUMBER)
1390  RCODE = -1
C
      RETURN
C
C  ** FOPSET ENDS HERE
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
      SUBROUTINE FCL2IN(CLIST,FIRST,LAST,INT,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C Parse CLIST(FIRST:LAST) for non-negative integer (INT).
C RCODE > 0 if error
C
       CHARACTER*(*) CLIST
C LOCAL
       CHARACTER*12  STRVAL
       CHARACTER*1   CHAR
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      CALL FTOKEN(CLIST,FIRST,LAST,STRVAL,12,CHAR)
      CALL FC2I(STRVAL,12,INT,RCODE)
      RETURN
C
C  ** FCL2IN ENDS HERE
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
