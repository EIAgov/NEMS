C                  ::: SCHEQN.FOR  1-12-94 :::
C
C  LAST DATES:  2-13-92...Added SWSUM to SCHEQN
C               3-07-92...Shortened header
C               3-19-93...Relocated DOMSTR=' ' to have no domain
C
C This contains the following SCHEMA routines (for ANALYZE).
C  (Linking routine is ASCHMA.)
C
C     SCHEQN.....display in equation format
C     SCHTRM.....form term for schema cell
C
      SUBROUTINE SCHEQN(LINE,BOUND,RHS,SWSUM,*)
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
C This displays schema equations. Cell entries are presumed numerical.
C ...Alternate return is user abort (from FTEXT).
C
      LOGICAL*1 SWSUM
C               :...TRUE MEANS USE 'Sum';  FALSE MEANS IMPLIED SUM
C
C Using SCRWTH from DCFLIP.
C
C  BOUND = 0 (NO BOUNDS) OR 2 (LAST 2 ROWS ARE BOUNDS)
C  RHS   = 0 (NO RHS), 1 (LAST COL IS RHS)
C
C          X(I,J) Y(J)     RHS
C     COST  d01    d02     ...MIN
C     S(I)  d11            r1 b1
C     D(J)  d21    d22     r2 b2   <--- SCHNR - BOUND : BOUND = 2
C     :LO    0      l2
C     :UP    u1     *
C
C       MINIMIZE COST SUBJECT TO:
C        COST = SUM[ I,J | d01 X(I,J)] + SUM[J | d02 Y(J)]
C        S(I) = SUM[ J | d11 X(I,J)] r1 b1
C        D(J) = SUM[ I | d21 X(I,J)] + d22 Y(J) r2 b2
C
C               0 <= X(I,J) <= u1
C              l2 <= Y(J)
C
C WITH SWSUM = FALSE, THE EQNS BECOME:
C        COST = d01 X(I,J) + d02 Y(J)
C        S(I) = d11 X(I,J) r1 b1
C        D(J) = d21 X(I,J) + d22 Y(J) r2 b2
C
C LOCAL
      CHARACTER*128 CLIST
      CHARACTER*1   CHAR
      CHARACTER*32  DOMSTR,STRVAL
      PARAMETER     (PMSSET=7)
C                        :...MAX NUMBER OF SETS IN AN EQUATION DOMAIN
      CHARACTER*4   SETNAM(PMSSET),SET
C               :...MAX LENGTH OF SET NAME
      LOGICAL*1     SW,SWRNG
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF(SCHNR.LE.0 .OR. SCHNC.LE.0)THEN
         PRINT *,' NO SCHEMA IN MEMORY'
         RETURN
      ENDIF
      IF(LINE.LT.0)LINE=0
      TAB  = SCHRLN+2
C          = INDENTATION FOR NEW LINES OF SAME EQUATION
      NEQN = SCHNR - BOUND
C          = NUMBER OF EQUATIONS  (1ST NEQN ROWS OF SCHEMA)
      NACT = SCHNC - RHS
C          = NUMBER OF ACTIVITIES (1ST NACT COLUMNS OF SCHEMA)
      SWRNG= .FALSE.
C            :...MEANS NO RANGE HAS BEEN PRINTED
C
      CLIST = 'Schema Equations'
      CALL FCENTR(CLIST,I,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*990)
C
C LOOP OVER SCHEMA ROWS THAT ARE EQUATIONS
      DO 300 I=1,NEQN
         CLIST = SCHROW(I)(:SCHRLN)//' ='
         LAST = TAB
         SW = .TRUE.
C           :...MEANS WE ARE AT 1ST TERM
         MARGIN = 1
C           :...WILL CHANGE TO TAB AFTER 1 LINE IS WRITTEN
C FORM DOMAIN OF EQUATION
         NSETS = 0
         CALL FLOOK(SCHROW(I),2,14,'(',BEGDOM)
         IF(BEGDOM.GT.0)THEN
            BEGDOM = BEGDOM+1
            CALL FLOOK(SCHROW(I),BEGDOM,16,')',ENDOM)
            ENDOM = ENDOM-1
C
100         CHAR = ' '
            CALL FTOKEN(SCHROW(I),BEGDOM,ENDOM,SET,4,CHAR)
              IF(NSETS.GE.PMSSET)THEN
                 PRINT *,' ** Sorry, domain has too many sets...',
     1                   ' Max =',PMSSET
                 RETURN 1
              ENDIF
              NSETS = NSETS+1
              SETNAM(NSETS) = SET
            IF(CHAR.EQ.',')GOTO 100
         ENDIF
C NOW SETNAM(1)x...xSETNAM(NSETS) = DOMAIN OF EQUATION
C
C    LOOP OVER SCHEMA COLUMNS THAT ARE ACTIVITIES
         DO 200 J=1,NACT
            IF(SCHDAT(I,J).EQ.' ')GOTO 200
C FIND DOMAIN OF SUMMATION = SETS IN DOMAIN OF ACTIVITY THAT ARE
C                            NOT IN DOMAIN OF EQUATION
CC THE FOLLOWING LINE WAS PUT HERE (RATHER THAN WHEN BEGDOM.GT.0) 1-12-94
            DOMSTR = ' '
            CALL FLOOK(SCHCOL(J),2,14,'(',BEGDOM)
            IF( BEGDOM.GT.0 )THEN
               BEGDOM = BEGDOM+1
               CALL FLOOK(SCHCOL(J),BEGDOM,16,')',ENDOM)
               ENDOM = ENDOM-1
               IDOM   = 0
150            CONTINUE
               CHAR = ' '
               CALL FTOKEN(SCHCOL(J),BEGDOM,ENDOM,SET,4,CHAR)
                 IF(NSETS.GT.0)THEN
C   SEE IF SET IS ALREADY IN DOMAIN OF EQUATION
                    DO 175 S=1,NSETS
                       IF(SET.EQ.SETNAM(S))GOTO 190
175                 CONTINUE
                 ENDIF
C   NO, PUT SET IN SUM DOMAIN
                 DOMSTR(IDOM+1:) = SET
                 CALL FSLEN(DOMSTR,32,IDOM)
                 IDOM = IDOM+1
                 DOMSTR(IDOM:) = ','
190            IF(CHAR.EQ.',')GOTO 150
               IF(IDOM.GT.0)DOMSTR(IDOM:) = ' '
            ENDIF
C NOW DOMSTR = DOMAIN OF SUM = SETS IN DOMAIN OF ACTIVITY THAT ARE
C                              NOT IN DOMAIN OF EQUATION
            CALL SCHTRM(I,J,1,STRVAL)
            CALL FSLEN(STRVAL,32,LEN)
            IF(SW)THEN
C THIS IS FIRST TERM...DO NOT INCLUDE +
               IF(STRVAL(1:1).EQ.'+')STRVAL(1:1)=' '
               SW = .FALSE.
            ENDIF
            IF(STRVAL(LEN:LEN).EQ.']')SWRNG = .TRUE.
C
            CALL FSLEN(SCHCOL(J),16,LC)
C SEE IF WE HAVE ENOUGH ROOM TO ADD TERM ON THIS LINE
            NEWLEN = LAST+LEN+LC+2
            IF(IDOM.GT.0)NEWLEN = NEWLEN+IDOM+8
            IF(NEWLEN.GE.SCRWTH)THEN
C ...NO, SO REDUCE BUFFER BEFORE ADDING THIS TERM
               CALL FTEXT(CLIST,LAST,MARGIN,0,LINE,'KEEP ',*990)
               MARGIN = TAB
            ENDIF
C OK, SEE IF WE FORM SUM
            IF(DOMSTR.NE.' '.AND.SWSUM)THEN
C ENTER SUM[... IF SWSUM = TRUE
               IF(LEN.GT.1)THEN
                  CLIST(LAST+2:) = STRVAL(1:1)//' Sum['//DOMSTR(:IDOM)
     1                //'|'//STRVAL(2:LEN)//' '//SCHCOL(J)(:LC)//']'
               ELSE
                  CLIST(LAST+2:) = STRVAL(1:1)//' Sum['//DOMSTR(:IDOM)
     1                  //'| '//SCHCOL(J)(:LC)//']'
               ENDIF
            ELSE
C NO
               CLIST(LAST+2:) = STRVAL(:LEN)//' '//SCHCOL(J)
            ENDIF
            CALL FSLEN(CLIST,127,LAST)
200      CONTINUE
C     ::: END LOOP OVER ACTIVITIES :::
C
         IF(RHS.GT.0)THEN
C PUT CONSTRAINT RELATION (AFTER CHECKING BUFFER)
            IF(LAST+SCHCLN.GE.SCRWTH)THEN
C   WRITE BUFFER (CLIST)
               CALL FTEXT(CLIST,LAST,MARGIN,0,LINE,'KEEP ',*990)
               MARGIN = TAB
            ENDIF
            CALL FLOOK(SCHDAT(I,SCHNC),1,3,'=',IREL)
            IF(IREL.EQ.0)THEN
C   NO RELATION (EG, MIN OR MAX)
               CLIST(LAST+2:) = SCHDAT(I,SCHNC)
            ELSE
C   PUT RELATION
               CLIST(LAST+2:) = SCHDAT(I,SCHNC)(:IREL)
               LAST = LAST+IREL+2
C   PUT RHS VALUE
               J = SCHNC
               CALL SCHTRM(I,J,IREL+1,STRVAL)
               IF(STRVAL(1:1).EQ.'+')THEN
                  CLIST(LAST:) = STRVAL(2:)
               ELSE
                  CLIST(LAST:) = STRVAL
               ENDIF
            ENDIF
            CALL FSLEN(STRVAL,32,LEN)
            IF(STRVAL(LEN:LEN).EQ.']')SWRNG = .TRUE.
            CALL FSLEN(CLIST,127,LAST)
            CALL FTEXT(CLIST,LAST,MARGIN,0,LINE,'CLEAR ',*990)
         ENDIF
300   CONTINUE
C  :::::::::::::::::: END OF EQUATIONS ::::::::::::::::::
C
      IF(BOUND.GT.0)THEN
C GIVE BOUNDS
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*990)
C   GET COL STRIP WIDTH (MAY BE < SCHCLN, WHICH INCLUDES CELL WIDTHS)
         TAB = 1
         DO 450 J=1,NACT
            CALL FSLEN(SCHCOL(J),16,LEN)
            IF(LEN.GT.TAB)TAB = LEN
450      CONTINUE
C
         I = SCHNR-1
C          = ROW STRIP OF LOWER BOUNDS
         TAB = TAB + 32
C            = TAB FOR UPPER BOUNDS
C
         DO 500 J=1,NACT
            CLIST = ' '
            CALL SCHTRM(I,J,1,STRVAL)
            IF(STRVAL(1:1).EQ.'+')STRVAL(1:1)=' '
            LC = 32
            CALL FSQUEZ(STRVAL,LC)
            IF(STRVAL(LC:LC).EQ.']')SWRNG = .TRUE.
            CLIST(27-LC:) = STRVAL
            CLIST(28:) = '<= '//SCHCOL(J)
            IF(SCHDAT(SCHNR,J).NE.'*')THEN
C   ENTER UPPER BOUND
               IF(SCHDAT(SCHNR,J)(1:1).EQ.'=')THEN
                  CLIST(28:28) = ' '
                  CLIST(TAB:) = '(Fixed Levels)'
               ELSE
                  CALL SCHTRM(I+1,J,1,STRVAL)
                  IF(STRVAL(1:1).EQ.'+')STRVAL(1:1)=' '
                  LC = 32
                  CALL FSQUEZ(STRVAL,LC)
                  IF(STRVAL(LC:LC).EQ.']')SWRNG = .TRUE.
                  CLIST(TAB:) = '<= '//STRVAL
               ENDIF
            ENDIF
            CALL FSLEN(CLIST,80,LAST)
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*990)
500      CONTINUE
      ENDIF
C  :::::::::::::::::::: END OF BOUNDS :::::::::::::::::::::::
C
      IF(SWRNG)THEN
C PUT FOOTNOTE ABOUT NONZERO RANGE
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*990)
         CLIST = 'The notation [a/b] means the coefficients, '//
     1           'which can depend upon the domain member, '//
     2           'is in the range a to b (see SCHEMA.DOC).'
         CALL FSLEN(CLIST,127,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*990)
      ENDIF
C
900   RETURN
C
990   RETURN 1
C
C ** SCHEQN ENDS HERE
      END
      SUBROUTINE SCHTRM(I,J,FIRST,TERM)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCSCHEMA
C
C This forms term for cell entry, using SCHDAT(I,J)(FIRST:16).
C Sign of term will be first char of TERM.
C
      CHARACTER*(*) TERM
C LOCAL
      CHARACTER*16  MYTERM
      CHARACTER*1   SIGN
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF(FIRST.GT.16 .OR. FIRST.LT.1)THEN
         PRINT *,' ** SYSERR IN SCHTRM...FIRST=',FIRST
         TERM = ' '
         RETURN
      ENDIF
      MYTERM = SCHDAT(I,J)(FIRST:)
      CALL FSLEN(MYTERM,16,LAST)
      IF(LAST.EQ.0)THEN
         TERM = ' '
         RETURN
      ENDIF
C SKIP LEADING BLANKS
      F = 1
10    IF(MYTERM(F:F).EQ.' ')THEN
         F = F+1
         GOTO 10
      ENDIF
C NOW MYTERM(F:LAST) CONTAINS NONZERO SPEC
      CALL FLOOK(MYTERM,F,LAST,'/',IRANGE)
      IF(IRANGE.GT.0)THEN
C NONZERO RANGE...ENCLOSE IN SQUARE BRACKETS
         TERM = '+ '//'['//MYTERM(F:LAST)//']'
      ELSE
C UNIQUE VALUE...FIRST GET SIGN
         IF(MYTERM(F:F).EQ.'-')THEN
            SIGN = '-'
            F = F+1
         ELSE
            SIGN = '+'
            IF(MYTERM(F:F).EQ.'+')F = F+1
         ENDIF
         IF(MYTERM(F:).EQ.'1')THEN
            TERM = SIGN
         ELSE
            TERM = SIGN//' '//MYTERM(F:)
         ENDIF
      ENDIF
C
      RETURN
C
C ** SCHTRM ENDS HERE
      END
