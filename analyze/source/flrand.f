C            ::: FLRAND.FOR  6-30-95 :::
C
C Last dates:  earlier dates deleted
C             12-15-94...Changed FPURFY (5-11-95 & 6-1-95)
C              6-01-95...Added FR2CLF
C
C This file contains the following FLIP subroutines.
C
C Random numbers.
C ~~~~~~~~~~~~~~
C    FRANDU......give uniform random number
C    FRANDS......give random value from a standard distribution
C    FRANDP......give random integer from a probability distribution
C    FPURFY......purify a real value
C    FC2R........convert character to real
C    FR2C........convert real to character
C    FR2C12......convert real to 12-character string
C    FR2CLJ......convert real to 12-character string and left justify
C    FR2CRJ......convert real to 12-character string and right justify
C    FR2CLF......FRC2LJ with decimal put into number (for F format)
C
      SUBROUTINE FRANDU(VALRND)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This returns a uniform random number (VALRND)
C ...using quadratic congruential method with parameters (MULT, MOD) from:
C
C  R.E. Nance and C.Overstreet, Jr., "A Random Number Generator for Small
C               Word Length Computers," Proceedings of ACM, 1973.
C
C Note: We are using MOD = 2**16 (for 16-bit words) to ensure
C       portability.  This is thus a "quick & dirty" random number generator.
C
C       USERS ARE ADVISED TO USE THEIR OWN LIBRARY FUNCTION IF AVAILABLE
C       ================================================================
C         Simply replace the code here with:
C             VALRND = randfunc(...)
C         to return a uniform random number (VALRND) on (0,1)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      PARAMETER (MULT=411,MOD=16 384)
C ...RND0 AND RND1 WERE INITIALIZED IN MAIN (0 AND 1, RESPECTIVELY)
C
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      RNDVAL = RND1*RND1 + MULT*RND0
      RNDVAL = RNDVAL - MOD*(RNDVAL/MOD)
      RND0 = RND1
      RND1 = RNDVAL
C
      VALRND = RNDVAL/FLOAT(MOD)
      RETURN
C
C ** FRANDU ENDS HERE
      END
      SUBROUTINE FRANDS(NAME,VPARM1,VPARM2,VALRND,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C RETURNS RANDOM VALUE (VALRND) FROM A STANDARD DISTRIBUTION (NAME)
C HAVING 2 REAL PARAMETERS (VPARM1, VPARM2)
C
C NAME = UNIFORM  ==> [VPARM1, VPARM2] is interval
C      = NORMAL   ==> VPARM1 = Mean, VPARM2 = Std. deviation
C      = TRIANGLE ==> [VPARM1, VPARM2]
C      = EXPONENT ==> VPARM1 = Mean rate (VPARM2 not used)
C
C ...ALTERNATE RETURN IS FOR FATAL ERROR
C
      CHARACTER*(*) NAME
      REAL          VPARM1,VPARM2,VALRND
      LOGICAL*1     SW
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      CALL FRANDU(VALRND)
      CALL FMATCH(NAME,'UNIFORM',' ',SW)
      IF(.NOT.SW)GOTO 100
C UNIFORM DISTRIBUTION
          VALRND = VPARM1 + VALRND*(VPARM2 - VPARM1)
          RETURN
100   CALL FMATCH(NAME,'TRIANGLE',' ',SW)
      IF(.NOT.SW)GOTO 200
C TRIANGLE DISTRIBUTION
         IF(VALRND.LT.0.5)THEN
            VALRND = VPARM1 + (VPARM2 - VPARM1)*SQRT(VALRND/2)
         ELSE
            VALRND = VPARM2 - (VPARM2 - VPARM1)*SQRT((1-VALRND)/2)
         ENDIF
         RETURN
200   CALL FMATCH(NAME,'NORMAL',' ',SW)
      IF(.NOT.SW)GOTO 300
C NORMAL DISTRIBUTION
         IF(VPARM2.LE.1.0E-05)THEN
C ...SPIKE (ESSENTIALLY ZERO VARIANCE)
            VALRND = VPARM1
            RETURN
         ENDIF
C
         DO 210 I=1,11
            CALL FRANDU(V)
            VALRND = VALRND + V
210      CONTINUE
         VALRND = VALRND - 6.
C ...NOW VALRND IS N(0,1)...ADJUST FOR MEAN (VPARM1) AND STD. DEVIATION (VPARM2)
         VALRND = VPARM1 + VPARM2*VALRND
         RETURN
300   CALL FMATCH(NAME,'EXPONENT',' ',SW)
      IF(.NOT.SW)GOTO 400
C EXPONENTIAL DISTRIBUTION
      IF(VPARM1.LE.1.0E-06)THEN
         PRINT *,' ** SYSERR FRANDS...EXP',VPARM1
         RETURN 1
      ENDIF
      VALRND = -LOG(VALRND)/VPARM1
      RETURN
C
400   CONTINUE
C DISTRIBUTION NOT RECOGNIZED...** SYSERR
      PRINT *,' ** SYSERR FRANDS...',NAME
      RETURN 1
C
C ** FRANDS ENDS HERE
      END
      SUBROUTINE FRANDP(NLO,NUP,PRMASS,RNDVAL,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C THIS COMPUTES RANDOM NUMBER IN RANGE NLO TO NUP USING PROBABILITY MASS
C FUNCTION (PRMASS).  CONDITIONAL PROBABILITY IS COMPUTED BY NORMALIZING
C                  Pr(n) = PRMASS(n)/VTOTAL,
C WHERE   VTOTAL = PRMASS(NLO) + ... + PRMASS(NUP).
C
      INTEGER NLO,NUP,RNDVAL
      REAL    PRMASS(NUP)
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      IF(NUP.EQ.NLO)THEN
C TRIVIAL DISTRIBUTION (ALL MASS AT NLO=NUP)
         RNDVAL = NLO
         RETURN
      ENDIF
C
      VTOTAL = 0.
      DO 10 K=NLO,NUP
10    VTOTAL = VTOTAL + PRMASS(K)
C
      CALL FRANDU(VALRND)
      VMASS = 0.
C
      DO 100 RNDVAL = NLO,NUP
         VMASS = VMASS + PRMASS(RNDVAL)
         IF(VMASS.GE.VTOTAL*VALRND)RETURN
100   CONTINUE
C
      PRINT *,' ** SYSERR FRANDP...',NLO,NUP,VMASS,RNDVAL
      RETURN 1
C
C ** FRANDP ENDS HERE
      END
      SUBROUTINE FPURFY(V,VTOL)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
      INCLUDE 'DCFLIP.'
C
C Purify V using tolerance = VTOL > 0
C
C Example:   V = 123.666 666 666 6, VTOL = 1.0E-6
C         MANTSA = 123
C    (V - FLOAT(MANTSA) )/ VTOL = ( .666 666 666 6 )/E-6
C                               = 666 666 .666 6
C                                +        .5
C                       FRAC    = 666 667 (.1666 TRUNCATED)
C   NOW  V = 123 + E-6 * (666 667) = 123.666 667 00...00
C                                              :::::::::
C                                               PURIFIED
      REAL V,VTOL
C Also, the returned value will be < greatest integer (VINF), so
C caller can set INT = V safely.
C
C LOCAL
      INTEGER MANTSA,FRAC
      PARAMETER (VINF=2.**31)
C                    :...A BIT LESS THAN LARGEST INTEGER
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( SWFDBG )THEN
         PRINT *,' ENTERED FPURFY WITH V,VTOL=',V,VTOL
         FLPAUS = FLPAUS-1
         IF( FLPAUS.LE.0 )CALL FDEBUG
      ENDIF
      IF( V.LT.-VTOL )THEN
         VABS = -V
         SIGN = -1
      ELSE IF( V.GT.VTOL )THEN
         VABS =  V
         SIGN =  1
      ELSE
         V = 0.
         RETURN
      ENDIF

      IF( VABS.GE.VINF )THEN
         V = SIGN*VINF
         RETURN
      ENDIF
      MANTSA = VABS
      DIGITS = -LOG(VTOL) + .5
      IF( DIGITS.LE.0 )THEN
         V = SIGN*FLOAT(MANTSA)
         RETURN
      ENDIF
      VMULT = 10.**DIGITS
150   CONTINUE
      VFRAC = (VABS - FLOAT(MANTSA)) * VMULT + .5
      IF( VFRAC.GE.VINF )THEN
         VMULT = VMULT/10.
         GOTO 150
      ENDIF
      FRAC = VFRAC
      V = FLOAT(MANTSA) + FLOAT(FRAC)/VMULT
      IF( SIGN.LT.0 )V = -V
C
      RETURN
C
C ** FPURFY ENDS HERE
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
C
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
C NOW SIGN IS SET AND SLOCAL(K:LENGTH) CONTAINS REAL VALUE
      IF(SLOCAL(K:K).EQ.'*')THEN
         VALUE = SIGN*VINF
         RETURN
      ENDIF
C
40    CONTINUE
C BEGIN NUMERIC PARSE...INITIALIZE EXPONENT (EXPNT)
      EXPNT = 0
C
C USING HORNER'S RULE TO PARSE STRING(K:LENGTH)
C
      DO 100  L=K,LENGTH
         CHAR = SLOCAL(L:L)
         DO 50 DIGIT=0,9
            IF(CHAR.EQ.CHRNUM(DIGIT))THEN
                VALUE = 10.*VALUE + FLOAT(DIGIT)
                GOTO 100
            ENDIF
50       CONTINUE
C   NOT A DIGIT...SEE IF DECIMAL POINT
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
      SUBROUTINE FR2C(STRING,VALUE)
C     ===============
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C CONVERT REAL VALUE TO STRING (MAX LENGTH = 8 CHARS)
C
      CHARACTER*(*) STRING
C LOCAL
      CHARACTER*1   STRSGN
      CHARACTER*8   DUMMY,STRLCL
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      IF(VALUE.LT.0)THEN
         STRSGN = '-'
         V = -VALUE
      ELSE IF(VALUE.GT.0)THEN
         STRSGN = ' '
         V = VALUE
      ELSE
         STRING = ' 0'
         RETURN
      ENDIF
      IF(V.GT.1.0E+15)THEN
         STRING = STRSGN//'*'
         RETURN
      ENDIF
      IF(V.GE.1.0E+06)THEN
         STRING = STRSGN//'(> E+6)'
         RETURN
      ENDIF
      IF(V.LE.1.0E-06)THEN
         STRING = STRSGN//'(< E-6)'
         RETURN
      ENDIF
      IF(ABS(V-1.).LE.1.E-8)THEN
         STRING = STRSGN//'1'
         RETURN
      ENDIF
C === END SPECIAL CASES ===
      STRLCL = STRSGN
C GET INTEGER PART
      IF( V.LT.1. )THEN
         VEXP = 1.0E-7
      ELSE IF( V.LT.10. )THEN
         VEXP = 1.0E-6
      ELSE IF( V.LT.100. )THEN
         VEXP = 1.0E-5
      ELSE
         VEXP = 1.0E-4
      ENDIF
      MANTSA = V + VEXP
      DUMMY = ' '
      CALL FI2C(DUMMY,MANTSA,FIRST)
C SET MANTISSA
      STRLCL(2:) = DUMMY(FIRST:8)//'.'
C USE REMAINING CHARS (IF ANY) FOR FRACTION
      CALL FSLEN(STRLCL,8,LAST)
      IF( LAST.GE.8 )GOTO 999
C
C GET FRACTIONAL PART
      FRAC = 8-LAST
C ...FRAC = # DIGITS IN FRACTIONAL PART
      VFRAC =  (V-FLOAT(MANTSA) + VEXP)*(10.**FRAC)
C                :                  :       :.....SHIFT
C                :                  :... BUMP UP
C                :....... TRUE FRACTIONAL PART
      IF( VFRAC.LE.0 )GOTO 999
      INTVAL = VFRAC
CC 12-8-94
      VFRAC  = VFRAC - FLOAT(INTVAL)
      IF( VFRAC.GT. .5 )THEN
C ROUND UP
         INTVAL = INTVAL+1
         IF( INTVAL.GT.10**FRAC )THEN
C ...INCREASE MANTISSA AND DROP FRACTION
            MANTSA = MANTSA+1
            DUMMY = ' '
            CALL FI2C(DUMMY,MANTSA,FIRST)
            STRLCL(2:) = DUMMY(FIRST:8)
            GOTO 999
         ENDIF
      ENDIF
      DUMMY = '00000000'
      CALL FI2C(DUMMY,INTVAL,FIRST)
C REMOVE TRAILING 0'S FROM FRACTION
      L = 8
50    IF(DUMMY(L:L).EQ.'0')THEN
         DUMMY(L:) = ' '
         L = L-1
         IF( L.GT.0 )GOTO 50
      ENDIF
      STRLCL(LAST+1:) = DUMMY(9-FRAC:)
C                              :.... SHIFT TO WHERE FRAC BEGINS
999   CONTINUE
C REMOVE DECIMAL IF FRACTIONAL PART = 0
      CALL FSLEN(STRLCL,8,L)
      IF(STRLCL(L:L).EQ.'.') STRLCL(L:) = ' '
C FINALLY, COPY LOCAL STRING (STRLCL) TO CALLER'S STRING
      STRING = STRLCL
      RETURN
C
C *** FR2C ENDS HERE
      END
      SUBROUTINE FR2C12(STRING,VALUE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C CONVERT REAL VALUE TO STRING (12-CHAR FORMAT)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*(*) STRING
      REAL          VALUE
C LOCAL
      CHARACTER*1   STRSGN
      CHARACTER*4   STREXP
      CHARACTER*8   STR8
      REAL          V
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      STRING = ' '
      IF( VALUE.LT.0. )THEN
         STRSGN = '-'
         V = -VALUE
      ELSE IF( VALUE.GT.0. )THEN
         STRSGN = ' '
         V = VALUE
      ELSE
         STRING =' 0'
         RETURN
      ENDIF
C SPECIAL CASES (*, negl, 1)
      IF( V.GE.1.0E+15 )THEN
C  INFINITY (*)
         STRING = STRSGN//'*'
         RETURN
      ENDIF
      IF( V.LE.1.0E-15 )THEN
         STRING = 'negligible'
         RETURN
      ENDIF
      IF( ABS(V-1.).LE.1.0E-8 )THEN
         STRING = STRSGN//'1'
         RETURN
      ENDIF
C
C NOW E-15 < V < E+15 AND STRSGN HAS SIGN (BLANK OR -)
C ALSO, V <> 1
CC 12-8-94
      STR8 = ' '
C USE FR2C (8-CHAR) FOR 'NORMAL' RANGE
      IF( V.LT.1.0E6 .AND. V.GE.1. )THEN
         CALL FR2C(STR8,VALUE)
         STRING = STR8
         RETURN
      ENDIF
      IF( V.GE.1.0E6 )THEN
C WE HAVE A LARGE VALUE...WE CAN JUST SET LARGER EXPONENT
         EXP = LOG10(V)-1
         V = V/(10.**EXP)
         STR8(7:) = '0'
         CALL FI2C(STR8,EXP,FIRST)
         STREXP = 'E+'//STR8(7:8)
         CALL FR2C(STR8,V)
         STRING = STRSGN//STR8(2:8)//STREXP
         RETURN
      ENDIF
C WE HAVE A FRACTION...WE WANT MORE PRECISION
C GET 1ST 8 DIGITS
      FRAC = V*(10.**8) + .5
      IF( FRAC.GT.0 )THEN
         STR8 = '00000000'
         CALL FI2C(STR8,FRAC,FIRST)
         CALL FLOOKF(STR8,2,8,'00000',I)
         IF( I.GT.0 )THEN
            IF( STR8(I-1:I-1).NE.'0' )STR8(I:) = ' '
         ENDIF
         STRING = STRSGN//'.'//STR8
         RETURN
      ENDIF
C E-15 < V < E-8...USE EXPONENT
      EXP  = -LOG10(V)-1
      STR8(7:) = '0'
      CALL FI2C(STR8,EXP,FIRST)
      STREXP = 'E-'//STR8(7:8)
      FRAC = V*(10.**(EXP+6)) + .5
      STR8 = '00000000'
      CALL FI2C(STR8,FRAC,FIRST)
      STRING = STRSGN//'.'//STR8(3:)//STREXP
C
      RETURN
C
C  ** FR2C12 ENDS HERE
      END
      SUBROUTINE FR2CLJ(STRING,VALUE,LAST)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This converts VALUE to STRING (Length >= 12)...Left justified.
C       STRING(1:LAST) = string of VALUE
C
      CHARACTER*(*) STRING
      REAL          VALUE
      INTEGER       LAST
C LOCAL
      CHARACTER*12  STR12
      CHARACTER*8   STR8
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( VALUE.EQ.0. )THEN
         LAST = 2
         STRING = ' 0'
         RETURN
      ENDIF
      STR8 = ' '
C COPY STRING
      STR12 = STRING
C CONVERT VALUE
      CALL FR2C12(STR12,VALUE)
50    CONTINUE
C SQUEEZE OUT BLANKS
      LAST = 12
      CALL FSQUEZ(STR12,LAST)
      CALL FLOOK(STR12,1,LAST,'.',DECMAL)
      IF( DECMAL.GT.0 )THEN
C REMOVE TRAILING 0'S AFTER DECIMAL
         CALL FLOOK(STR12,DECMAL,LAST,'E',EXP)
         IF( EXP.EQ.0 )THEN
            LEN = LAST
         ELSE
            LEN = EXP-1
         ENDIF
         DO 100 L=LEN,DECMAL,-1
            IF( STR12(L:L).NE.'0' )GOTO 110
            STR12(L:L) = ' '
100      CONTINUE
110      CONTINUE
         CALL FSQUEZ(STR12,LAST)
      ENDIF
C
      IF( STR12(LAST:LAST).EQ.'.' )LAST = LAST-1
C
      IF( STR12(1:LAST).EQ.'0' )THEN
C VALUE TOO SMALL
         IF( STR8.NE.' ' )THEN
C ...WE ALREADY TRIED TO CONVERT HERE...VALUE STILL TOO SMALL
            STRING = 'negligible'
            LAST   = 10
            RETURN
         ENDIF
C ...TRY TO CONVERT HERE
         IF( VALUE.LT.0. )THEN
            STR12 = '-0.'
            MANTSA= (-VALUE)*(10.**9)
         ELSE
            STR12 = ' 0.'
            MANTSA=  VALUE*(10.**9)
         ENDIF
         STR8 = '0000000'
         CALL FI2C(STR8,MANTSA,LAST)
         STR12(4:) = STR8
         GOTO 50
      ENDIF
C SPACE ADDED IF NO SIGN (4-8-93)
      IF( STR12(1:1).EQ.'-' .OR. STR12(1:1).EQ.'+' )THEN
         STRING = STR12(1:LAST)
      ELSE
         STRING = ' '//STR12(1:LAST)
         LAST = LAST+1
      ENDIF
      RETURN
C
C  ** FR2CLJ ENDS HERE
      END
      SUBROUTINE FR2CRJ(STRING,VALUE,FIRST)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This converts VALUE to STRING (Length >= 12) and right justifies.
C       STRING(FIRST:12) = string of VALUE
C
      CHARACTER*(*) STRING
      REAL          VALUE
      INTEGER       FIRST
C LOCAL
      CHARACTER*12  STR12,BLNK12
      DATA        BLNK12/'            '/
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C COPY STRING
      STR12  = STRING
      STRING = BLNK12
C CONVERT VALUE WITH LEFT JUSTIFY
      CALL FR2CLJ( STR12,VALUE,L )
      FIRST = 13-L
C
      STRING(FIRST:12) = STR12(1:L)
      RETURN
C
C  ** FR2CRJ ENDS HERE
      END
      SUBROUTINE FR2CLF(STRING,VALUE,LAST)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This converts VALUE to STRING (Length >= 12)...Left justified.
C Unlike FR2CLJ, a decimal is included in the string.
C       STRING(1:LAST) = string of VALUE
C
      CHARACTER*(*) STRING
C LOCAL
      CHARACTER*12  STR12
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C COPY STRING
      STR12  = STRING
C CONVERT VALUE WITH LEFT JUSTIFY
      CALL FR2CLJ(STR12,VALUE,LAST)
      CALL FLOOK(STR12,1,LAST,'.',DECIMAL)
      IF( DECIMAL.GT.0 )GOTO 900
C NO DECIMAL
      CALL FLOOK(STR12,2,LAST,'E',E)
      LAST = LAST+1
      IF( E.EQ.0 )THEN
C INSERT AT END
         STR12(LAST:) = '.'
      ELSE
C INSERT BEFORE E
         STRING = STR12(1:E-1)//'.'//STR12(E:12)
         STR12  = STRING
      ENDIF
C
900   STRING = STR12(:LAST)
      RETURN
C
C ** FR2CLF ENDS HERE
      END
