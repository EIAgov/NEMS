C                 ::: ARATE2.FOR  7-29-95 :::
C
C Earlier dates deleted
C    6-24-94...Removed CHCHAR in syntax form
C    BEGIN 12.5
C    7-28-94...Added WIDTH option to TABLEAU (CWIDTH passed to ARTABL)
C    1-07-95...Put blank after STR16 in ARTABL (CC 7-29-95)
C
C This contains the following ANALYZE subroutines.
C
C    ARATDS.....display a rate of substitution and update.
C    ARTABL.....displays rates in tableau format...added 3-1-93
C    AFTRAN.....gets rates of nonbasic row or column...added 3-9-93
C
      SUBROUTINE ARATDS(CNAME,ROWCOL,NUMBER,VRATE,VCIMP,
C     =================
     1          SWRNG,SWC,SWX,SWEQN,SWRSYN,SWDP,SWROW,SWCOL,
     2          TAB1,TABR,TABL,TABG,LINE,
     3          MINNAM,VMIN,MAXNAM,VMAX,VRSUM,*)
C
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C DCFLIP NEEDED FOR SCRWTH...ADDED 3-9-93
C
C This displays a rate of substitution for ROWCOL NUMBER and updates
C Least and Greatest Values.
C
C ...Alternate return is when user aborts (from FTEXT)
C
      CHARACTER*16 MINNAM,MAXNAM,CNAME
      CHARACTER*4  ROWCOL
      LOGICAL*1    SWRNG,SWC,SWX,SWEQN,SWRSYN,SWDP,SWROW,SWCOL
C
C CNAME = Impulse variable name (may be needed for EQUATION format)
C VCIMP = Impulse variable cost (may be needed for EQUATION format)
C
C ROWCOL NUMBER = Response variable
C VRATE = Rate to be displayed
C
C Format Switches: SWC   Show Response Costs (tabular or equation format)
C                  SWDP  Show obj x rate (instead of Least and Greatest)
C                  SWX   Show Response Levels (tabular format)
C                  SWEQN Equation format
C                  SWROW Displays row responses
C                  SWCOL Displays col responses
C                  SWRNG Suppresses all printing (just updates)
C                  SWSYN Show syntax
C TABs:  TAB1=2nd  column (after name of response variable)
C        TABR=Rate column (used for both tabular and equation formats)
C        TABL=Least    Value column (tabular)
C        TABG=Greatest Value column (tabular)
C            =Net rate for equation + obj
C Updates:
C        VMIN   = Least    Value
C        VMAX   = Greatest Value
C        MINNAM = Name of Least blockage    (basic response only)
C        MAXNAM = Name of Greatest blockage   "
C
C LOCAL
      CHARACTER*128 CLIST,CTEMP
      CHARACTER*16  RNAME,STR16
      CHARACTER*1   STAT,CHAR
      LOGICAL*1     SWLO
      CHARACTER*2   CHCHAR
      DATA          CHCHAR/' ë'/
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF((.NOT.SWROW .AND. ROWCOL.EQ.'ROW ') .OR.
     1   (.NOT.SWCOL .AND. ROWCOL.EQ.'COL '))RETURN
C GET RESPONSE VARIABLE'S INFO
      CALL GETNAM(ROWCOL,NUMBER,RNAME)
      CALL GETRIM(ROWCOL,NUMBER,VL,VU,VC,NZ)
      CALL GETSOL(ROWCOL,NUMBER,VX,VP,STAT,STNUM)
      SWLO = VX.GE.VL
C
C       WE HAVE  Basic = Level + VRATE*Nonbasic
C If IMPULSE = Nonbasic, Value range (VMIN, VMAX) found
C    from Basic Response:  VL-VX <= VRATE*X(CNAME) <= VU-VX.
C If IMPULSE = Basic, Value range found from extreme of the
C    contribution of the nonbasic range:  VRATE*[VL,VU]
C
C The infeasible response is a bit different (see below).  In
C all cases of a basic response (feasible or not), blockage occurs
C when a variable's status changes.
C
C                       DECISION TABLE
C                       ==============
C        RESPONSE CONDITION             ACTION
C       ====================       ===============
C       STAT SGN(R)  VL   VU          VL      VU
C BASIC
C        B    +      -*    *          -*       *
C        B    +      -*    v          -*       U
C        B    +       v    *           L       *
C        B    +       v    v           L       U
C        B    -      -*    *          -*       *
C        B    -      -*    v           U       *
C        B    -       v    *          -*       L
C        B    -       v    v           U       L
C        I    +      any  <VX         -U       *
C        I    +      >VX  any         -*      -L
C        I    -      any  <VX         -*       U
C        I    -      >VX  any          L       *
C       ------------------------------------------
C              L=(VL-VX)/VRATE;   U=(VU-VX)/VRATE
C NONBASIC
C       LU    +      -*    *          -*       *
C                    -*    v          -*       U
C                     v    *           L       *
C                     v    v           L       U
C       LU    -      -*    *          -*       *
C                    -*    v           U       *
C                     v    *          -*       L
C                     v    v           U       L
C       ==========================================
C BRANCH ON CONDITION TO SET VL, VU
      IF(STAT.EQ.'I')THEN
C WE HAVE A SIGN SWITCH AND BLOCKAGE REMOVAL (SEE TABLE)
         IF(SWLO)THEN
C VX > VU (VIOLATES UPPER BOUND)
            IF(VRATE.GT.0.)THEN
               VL = (VU-VX)/VRATE
               VU = VINF
            ELSE
               VU = (VU-VX)/VRATE
               VL = -VINF
            ENDIF
         ELSE
C VX < VL (VIOLATES LOWER BOUND)
            IF(VRATE.GT.0.)THEN
               VU = (VL-VX)/VRATE
               VL = -VINF
            ELSE
               VL = (VL-VX)/VRATE
               VU = VINF
            ENDIF
         ENDIF
         GOTO 500
      ENDIF
C RESPONSE VARIABLE IS FEASIBLE (BASIC OR NOT)
      IF(VRATE.LT.0.)THEN
C ...SWITCH BOUNDS FOR NEGATIVE RATE
         V = -VL
         VL= -VU
         VU = V
         VR = -VRATE
      ELSE
         VR = VRATE
      ENDIF
C NOW VR=ABS(VRATE), WHICH WE USE FOR BASIC FEASIBLE AND FOR NONBASIC
      IF(STAT.EQ.'L'.OR.STAT.EQ.'U')THEN
         STAT = 'N'
C RESPONSE IS NONBASIC
         IF(VL.GT.-VINF)THEN
C ...MULTIPLY BY RATE
            VL=VL*VR
C ...UPDATE ACCUMULATION
            IF(VMIN.GT.-VINF)VMIN = VMIN+VL
         ELSE
            VMIN = -VINF
         ENDIF
         IF(VU.LT. VINF)THEN
            VU=VU*VR
            IF(VMAX.LT. VINF)VMAX = VMAX+VU
         ELSE
            VMAX = VINF
         ENDIF
         GOTO 1000
      ENDIF
C
C AT THIS POINT, RESPONSE IS BASIC AND FEASIBLE
      IF(VL.GT.-VINF)VL=VL/VR - VX/VRATE
      IF(VU.LT. VINF)VU=VU/VR - VX/VRATE
C AT THIS POINT, RESPONSE IS BASIC (FEASIBLE OR NOT)
500   CONTINUE
C UPDATE BLOCKAGE FROM BASIC RESPONSE
      IF(VL.GT.VMIN)THEN
         VMIN   = VL
         MINNAM = RNAME
      ENDIF
      IF(VU.LT.VMAX)THEN
         VMAX   = VU
         MAXNAM = RNAME
      ENDIF
C
1000  CONTINUE
C
C =============================================================
C       END OF COMPUTATION...PREPARE TO WRITE
C =============================================================
C
      IF(SWRNG)RETURN
C
C NOW (VL, VU) IS THE RANGE FOR THE CONDITION:
C   IF RESPONSE IS BASIC, RANGE IS FOR LEVEL OF NONBASIC IMPULSE
C   IF RESPONSE IS NONBASIC, RANGE IS FOR LEVEL OF BASIC IMPULSE
C
      MARGIN = 1
      INDENT = 0
      CLIST = ' '
      IF( SWRSYN .AND. .NOT.SWDP )THEN
C SYNTAX FORMAT
CC 7-28-94
         MARGIN = 4
         INDENT = -3
CC =======
         IF(ROWCOL.EQ.'ROW '.AND.NUMBER.EQ.OBJNUM)THEN
            CTEMP = ROWCOL//RNAME
            CALL FSLEN(CTEMP,30,LAST)
            CTEMP(LAST+2:) = '(objective).'
         ELSE
            CALL EXTRC(ROWCOL,RNAME,CLIST,LAST,RCODE)
            RCODE = 0
C                   :...IF EXTRC RETURNED RCODE <> 0, THIS
C                       MEANS THE SYNTAX HAS NO TRANSLATION,
C                       BUT WE SHALL PROCEED ANYWAY (USER WILL
C                       SEE MESSAGE ABOUT THIS)
C STRIP AWAY {ROW|COL} name
            FIRST = 1
            CALL FTOKEN(CLIST,FIRST,LAST,STR16, 8,CHAR)
            CALL FTOKEN(CLIST,FIRST,LAST,STR16,16,CHAR)
            IF(STAT.EQ.'N')THEN
               CTEMP = 'change in'
               L = 11
            ELSE
               CTEMP = ' '
               L = 1
            ENDIF
            CTEMP(L:) = ROWCOL//'that '//CLIST(FIRST:LAST)
         ENDIF
C NOW CTEMP = [change in] {ROW|COL} that <translation>.
C              :...present iff nonbasic response variable
C PUT SIGN OF RATE
         IF(VRATE.GT.0.)THEN
            CLIST = ' +'
            V = VRATE
         ELSE
            CLIST = ' -'
            V = -VRATE
         ENDIF
C ...NOW THE RATE
         CALL FR2CLJ( STR16,V,L )
         FIRST = 14-L
C    PUT rate x <translation>
         CLIST(FIRST+1:) = STR16(1:L)//' x '//CTEMP
         IF(ROWCOL.EQ.'COL '.AND. SOLST(1:6).NE.'INFEAS')THEN
C    COLUMN RESPONSE...ADD PARENTHETICAL:  (with <obj>=<cost>, ...)
            CTEMP = '(with '//OBJNAM
            CALL FSLEN(CTEMP,32,LAST)
            CALL FR2CLJ( STR16,VC,L )
            CTEMP(LAST+1:) = '='//STR16(1:L)
C    NOW CTEMP HAS PARENTHETICAL
            CALL FSLEN(CLIST,128,LAST)
            IF(CLIST(LAST:LAST).EQ.'.')THEN
               CLIST(LAST:)=' '
               LAST = LAST-1
            ENDIF
            IF(LAST.GT.80)THEN
               CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'KEEP ',*9000)
               IF(LAST.EQ.0)LAST = MARGIN-2
               INDENT = 0
CC 7-28-94     ^^^^^^^^^^
            ENDIF
            CLIST(LAST+2:) = CTEMP
C    NOW CLIST MIGHT BE LONG, SO CHECK (AND PRINT A LINE IF NECESSARY)
            CALL FSLEN(CLIST,128,LAST)
            LAST = LAST+1
            CLIST(LAST:) = ','
CC 7-28-94
CC            IF(LAST.GT.80) CALL FTEXT(CLIST,LAST,MARGIN,INDENT,
CC     1                                LINE,'KEEP ',*9000)
            IF( LAST.GT.80 )THEN
               CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'KEEP ',*9000)
               INDENT = 0
            ENDIF
CC ========
C    PUT NET RATE
            CLIST(LAST+1:) = ' so Net Rate ='
            LAST = LAST+17
            V = VRATE*VC
C   ...AND ADD TO SUM OF NET RATES
            VRSUM = VRSUM + V
            CALL FR2CLJ( STR16,V,L )
            CLIST(LAST:) = STR16(1:L)
            LAST = LAST+L-1
            CLIST(LAST+1:) = ').'
         ENDIF
      ELSE IF( SWEQN )THEN
C EQUATION FORMAT:
C   Basic = Level + rate x Nonbasic [...Net rate of obj = C*rate]
C                                    :...if SWC
C For Response = Nonbasic, the Level and Net rate are suppressed
C
         IF(STAT.EQ.'N')THEN
C BASIC = IMPULSE VARIABLE (SUPPRESS ITS LEVEL)
            TAB = TAB1+2
            CLIST(TAB+14:) = ' x '//CHCHAR//RNAME
         ELSE
C BASIC = RESPONSE VAR.
            CLIST = RNAME(:NAMELN)//' ='
            CALL FR2CLJ( CLIST(NAMELN+4:),VX,L )
            TAB = TABR+2
            CLIST(TAB+14:) = ' x '//CHCHAR//CNAME
            IF( SWC )THEN
C DISPLAY OBJ RATE
               IF( ROWCOL.EQ.'ROW '.AND.NUMBER.EQ.OBJNUM )THEN
                  V = VCIMP
               ELSE
                  V = VRATE*VC
               ENDIF
               CALL FR2CRJ( STR16,V,F )
               CLIST(TABG:) = STR16(1:12)
C ...ACCUMULATE SUM OF COST RATES
               VRSUM = VRSUM + V
            ENDIF
         ENDIF
C
C INSERT SIGNED RATE VALUE INTO CLIST(TAB:TAB+13) (RIGHT-JUSTIFIED)
         IF(VRATE.GT.0.)THEN
            CLIST(TAB:TAB) = '+'
            CALL FR2CRJ( CLIST(TAB+2:TAB+13),VRATE,F )
         ELSE
            CLIST(TAB:TAB) = '-'
            CALL FR2CRJ( CLIST(TAB+2:TAB+13),-VRATE,F )
         ENDIF
C   ======= END OF EQUATION FORMAT ======
      ELSE
C  TABULAR FORM...SET INTO FIXED FIELDS
         CLIST = RNAME
         IF( SWX )THEN
C ...LEVEL SPECIFIED
            CALL FR2CRJ( CLIST(TAB1-1:),VX,F )
         ELSE IF( SWC )THEN
C ...OBJECTIVE SPECIFIED
            CALL FR2CRJ( CLIST(TAB1-1:),VC,F )
         ENDIF
C NOW THE RATE
         CALL FR2CRJ( CLIST(TABR-1:),VRATE,F )
         IF( SWDP )THEN
C OBJ x RATE
            V = VC*VRATE
            CALL FR2CRJ( CLIST(TABL-1:),V,F )
            VRSUM = VRSUM + V
CC ADDED 3-9-93
            IF( SWRSYN )THEN
C SHOW ITS SYNTAX (AS IN SHOWCOL)
               CALL EXTRC('COL ',RNAME,CTEMP,LS,RCODE)
               IF( RCODE.EQ.0 )THEN
C STRIP AWAY COL NAME
                  F = 1
                  CALL FTOKEN(CTEMP,F,LS,CNAME,8,STAT)
                  CALL FTOKEN(CTEMP,F,LS,CNAME,NAMELN,STAT)
                  IF( CTEMP(LS:LS).EQ.'.' )THEN
C REMOVE PERIOD AT END OF SYNTAX
                     CTEMP(LS:) = ' '
                     LS = LS-1
                  ENDIF
                  CLIST(TABL+13:) = CTEMP(F:LS)
                  CALL FSLEN(CLIST,128,LAST)
                  IF( LAST.GE.SCRWTH )THEN
C TRUNCATE SYNTAX TO FIT WITHIN SCREEN WIDTH
                     LAST = SCRWTH-MARGIN-1
                     CLIST(LAST-2:) = '...'
                  ENDIF
               ELSE
                  RCODE = 0
               ENDIF
            ENDIF
         ELSE
C LEAST AND GREATEST VARIATION
            CALL FR2CRJ( CLIST(TABL-1:),VL,F )
            CALL FR2CRJ( CLIST(TABG-1:),VU,F )
         ENDIF
      ENDIF
C
C FINALLY, WRITE THE RESPONSE LINE
      CALL FSLEN(CLIST,128,LAST)
      CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
      RETURN
C
C USER ABORT (FROM FTEXT)
9000  RETURN 1
C
C ** ARATDS ENDS HERE
      END
      SUBROUTINE ARTABL(SWC,SWX,CWIDTH,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C          DCFLIP NEEDED FOR SCREEN WIDTH
C
C This displays rates of substitution for submatrix in tableau format.
C
C                    CWIDTH = column width
      LOGICAL*1      SWC, SWX
C                    :    :...= T MEANS SHOW LEVELS
C                    :...= T MEANS SHOW COSTS
C LOCAL
      CHARACTER*16   STR16
CC 7-29-95  PUT 2 BLANKS AFTER STR16, IN CASE CWIDTH >= 16
      CHARACTER*18   STR18
      EQUIVALENCE  (STR16,STR18)
      CHARACTER*4    ROWCOL
      CHARACTER*1    STAT
      LOGICAL*1      SW,SWOBJ,SWU
C                             :...= T MEANS UNDERLINE NONBASICS
C                                   (DONE IF ROOM)
      PARAMETER    (LCLMXB=PMXROW)
C                          :...MAX # BASICS IN TABLEAU
      CHARACTER*16  LCLNAM(LCLMXB)
      CHARACTER*1   LCLSTA(LCLMXB)
      INTEGER       LCLPIV(LCLMXB)
      REAL          LCLOBJ(LCLMXB),LCLEVL(LCLMXB)
      LOGICAL*1     FTNDGN,FTNINF
      CHARACTER*16  UNDERL
      CHARACTER*1   NZMAP(PMXROW),CHRDGN,     CHRINF
C FOOTNOTES:
      DATA                        CHRDGN/'!'/,CHRINF/'*'/
C                                         :           :...INFEASIBLE
C                                         :...DEGENERATE
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
CC 7-29-95
      STR18 = ' '
CC =======
      IF( CWIDTH.LT.NAMELN )CWIDTH = NAMELN
C NOW COLUMN WIDTH (CWIDTH) IS GREATER OF SPEC AND NAME LENGTH (NAMELN)
C ====== USING STATEMENT NUMBERS 5000-5999 (OUT OF SEQUENCE) ======
C INITIALIZE NONZERO MAP (NZMAP)
      DO 5000 I=1,NROWS
5000  NZMAP(I) = ' '
C ::: LOOP OVER BASICS TO UPDATE NZMAP :::
      DO 5100 I=1,NROWS
         CALL GETBVP(I,ROWCOL,NUMBER)
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF( SW )NZMAP(I) = '0'
5100  CONTINUE
C NOW NZMAP(I) = blank IF I-TH BASIC IS NOT IN SUBMAT
C              = 0     IF I-TH BASIC IS IN SUBMAT
      REMAIN = NRCSUB(2)
      IF( REMAIN.EQ.0 )GOTO 5290
C ::: LOOP OVER SUBMAT NONBASIC COLUMNS :::
      DO 5200 J=RCSUB1(2),NCOLS
         CALL GETMAP('COL ',J,SW)
         IF( .NOT.SW )GOTO 5200
         REMAIN = REMAIN-1
         CALL GETSOL('COL ',J,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'B' .OR. STAT.EQ.'I' )GOTO 5200
         CALL AFTRAN('COL ',J)
C ...NOW ZVALUE(i) = RATE OF i-TH BASIC VARIABLE WRT J
         DO 5150 I=1,NROWS
            IF( NZMAP(I).EQ.'0' )THEN
C I-TH BASIC VAR IS IN SUBMAT AND THIS IS 1ST NONZERO
                  VABS = DABS( ZVALUE(I) )
                  IF( VABS.GT.VTOLAB )NZMAP(I)='*'
               ENDIF
5150        CONTINUE
            IF( REMAIN.LE.0 )GOTO 5290
5200     CONTINUE
C
5290     CONTINUE
C
      REMAIN = NRCSUB(1)
      IF( REMAIN.EQ.0 )GOTO 5490
C ::: LOOP OVER SUBMAT NONBASIC ROWS :::
      DO 5400 J=RCSUB1(1),NROWS
         CALL GETMAP('ROW ',J,SW)
         IF( .NOT.SW )GOTO 5400
         REMAIN = REMAIN-1
         CALL GETSOL('ROW ',J,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'B' .OR. STAT.EQ.'I' )GOTO 5400
         CALL AFTRAN('ROW ',J)
C ...NOW ZVALUE(i) = RATE OF i-TH BASIC VARIABLE WRT J
         DO 5350 I=1,NROWS
            IF( NZMAP(I).EQ.'0' )THEN
                  VABS = DABS( ZVALUE(I) )
                  IF( VABS.GT.VTOLAB )NZMAP(I)='*'
               ENDIF
5350        CONTINUE
            IF( REMAIN.LE.0 )GOTO 5490
5400     CONTINUE
C
5490     CONTINUE
C NOW NZMAP(I) = *     IF SOME NONBASIC HAS NONZERO RATE FOR I-TH BASIC
C              = blank IF I-TH BASIC NOT IN SUBMAT
C              = 0     IF I-TH BASIC IN SUBMAT BUT ALL RATES = 0
         NR = 0
         NC = 0
         DO 5700 I=1,NROWS
            IF( NZMAP(I).NE.'0' )GOTO 5700
C I-TH BASIC HAS ALL ZERO RATES (FOR SUBMAT NONBASICS)
            CALL GETBVP(I,ROWCOL,NUMBER)
            IF( ROWCOL.EQ.'ROW ' )THEN
               NR = NR+1
            ELSE
               NC = NC+1
            ENDIF
C ...DELETE FROM SUBMAT
            SW = .FALSE.
            CALL GPUTMP(ROWCOL,NUMBER,SW)
5700     CONTINUE
         IF( NR+NC.GT.0 )THEN
            IF( SWMSG )PRINT *,NR+NC,
     1         ' BASICS DELETED FROM SUBMATRIX...ALL ZERO RATES'
            IF( NR.GT.0 )CALL ASETSB('ROW ')
            IF( NC.GT.0 )CALL ASETSB('COL ')
         ENDIF
C =============== END OF MODIFICATION =================
C
C INITIALZE FOR TALLY OF NUMBERS IN SUBMATRIX
      LCLNBC = 0
C         :...NUMBER OF BASIC COLUMNS
      LCLNNR = 0
C         :...NUMBER OF NONBASIC ROWS
      LCLNNC = 0
C         :...NUMBER OF NONBASIC COLUMNS
C     LCLNON = NUMBER OF NONBASICS = LCLNNR + LCLNNC (ASSIGNED BELOW)
C     LCLBAS = NUMBER OF BASICS (ASSIGNED BELOW)
      IBR1   = 0
C        :...FIRST BASIC ROW IN SUBMATRIX
      IBC1   = 0
C        :...FIRST BASIC COL IN SUBMATRIX
      INR1   = 0
C        :...FIRST NONBASIC ROW IN SUBMATRIX
      INC1   = 0
C        :...FIRST NONBASIC COL IN SUBMATRIX
C INITIALIZE FOOTNOTE SWITCHES
      FTNDGN = .FALSE.
      FTNINF = .FALSE.
C
      REMAIN = NRCSUB(2)
      IF( REMAIN.EQ.0 )GOTO 150
C
C ::: LOOP OVER COLUMNS IN SUBMATRIX TO TALLY NUMBERS :::
      DO 100 J=RCSUB1(2),NCOLS
         CALL GETMAP('COL ',J,SW)
         IF( .NOT.SW )GOTO 100
         REMAIN = REMAIN-1
         CALL GETSOL('COL ',J,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'L' .OR. STAT.EQ.'U' )THEN
            LCLNNC = LCLNNC+1
            IF( INC1.EQ.0 )INC1 = J
         ELSE
            LCLNBC = LCLNBC+1
            IF( LCLNBC.GT.LCLMXB )GOTO 1310
            CALL GETNAM('COL ',J,LCLNAM(LCLNBC))
            CALL GETBND('COL ',J,VL,VU)
            IF( VX.LT.VL .OR. VX.GT.VU )THEN
               LCLSTA(LCLNBC) = CHRINF
               FTNINF = .TRUE.
            ELSE IF( VX.LE.VL .OR. VX.GE.VU )THEN
               LCLSTA(LCLNBC) = CHRDGN
               FTNDGN = .TRUE.
            ELSE
               LCLSTA(LCLNBC) = ' '
            ENDIF
            CALL GETPIV(PIVROW,'COL ',J)
            LCLPIV(LCLNBC) = PIVROW
            IF( SWC )CALL GETRIM('COL ',J,VL,VU,LCLOBJ(LCLNBC),NZ)
            LCLEVL(LCLNBC) = VX
            IF( IBC1.EQ.0 )IBC1 = J
         ENDIF
         IF( REMAIN.EQ.0 )GOTO 150
100   CONTINUE
C
150   CONTINUE
      LCLBAS = LCLNBC
      REMAIN = NRCSUB(1)
      IF( REMAIN.EQ.0 )GOTO 250
C
      SWOBJ = .FALSE.
C ::: LOOP OVER ROWS IN SUBMATRIX TO TALLY NUMBERS :::
      DO 200 I=RCSUB1(1),NROWS
         CALL GETMAP('ROW ',I,SW)
         IF( .NOT.SW )GOTO 200
         REMAIN = REMAIN-1
         IF( I.EQ.OBJNUM )THEN
C OBJ IS IN SUBMATRIX...BUT PUT IT LAST
             SWOBJ = .TRUE.
             GOTO 200
         ENDIF
         CALL GETSOL('ROW ',I,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'L' .OR. STAT.EQ.'U' )THEN
            LCLNNR = LCLNNR+1
            IF( INR1.EQ.0 )INR1 = I
         ELSE
            LCLBAS = LCLBAS+1
            IF( LCLBAS.GT.LCLMXB )GOTO 1310
            CALL GETNAM('ROW ',I,LCLNAM(LCLBAS))
            CALL GETBND('ROW ',I,VL,VU)
            IF( VX.LT.VL .OR. VX.GT.VU )THEN
               LCLSTA(LCLBAS) = CHRINF
               FTNINF = .TRUE.
            ELSE IF( VX.LE.VL .OR. VX.GE.VU )THEN
               LCLSTA(LCLBAS) = CHRDGN
               FTNDGN = .TRUE.
            ELSE
               LCLSTA(LCLBAS) = ' '
            ENDIF
            LCLPIV(LCLBAS) = I
            LCLOBJ(LCLBAS) = VINF
            LCLEVL(LCLBAS) = VX
            IF( IBR1.EQ.0 )IBR1 = I
         ENDIF
         IF( REMAIN.EQ.0 )GOTO 250
200   CONTINUE
C
250   CONTINUE
      IF( SWOBJ )THEN
C ADD OBJ ROW
         LCLBAS = LCLBAS+1
         IF( LCLBAS.GT.LCLMXB )GOTO 1310
         LCLNAM(LCLBAS) = OBJNAM
         LCLPIV(LCLBAS) = OBJNUM
         LCLSTA(LCLBAS) = ' '
         CALL GETSOL('ROW ',OBJNUM,LCLEVL(LCLBAS),VP,STAT,STNUM)
         IF( OPTNAM(:3).EQ.'MIN' )THEN
            LCLOBJ(LCLBAS) = -1.
         ELSE
            LCLOBJ(LCLBAS) = 1.
         ENDIF
C CHECK THAT SUBMATRIX HAS AT LEAST ONE BASIC
      ELSE IF( LCLBAS.EQ.0 )THEN
         PRINT *,' ** SUBMATRIX HAS NO BASIC ROWS OR COLUMNS'
         GOTO 1390
      ENDIF
      LCLNON = LCLNNR+LCLNNC
C CHECK THAT SUBMATRIX HAS AT LEAST ONE NONBASIC
      IF( LCLNON.EQ.0 )THEN
         PRINT *,' ** SUBMATRIX HAS NO NONBASIC ROWS OR COLUMNS'
         GOTO 1390
      ENDIF
C ========== SETUP SCREEN ==========
      COLWTH = CWIDTH + 1
C INITIALIZE WHERE LEVEL AND COST COLUMNS BEGIN
      TABX = -COLWTH + 1
      TOPX = 0
      TABC = -COLWTH + 1
      TOPC = 0
      IF( SWC )THEN
C USER ASKED FOR COSTS (OBJ)
         IF( LCLNBC.GT.0 )TABC = 1
C                            :...MAKE COST COLUMN AT LEFT
         IF( LCLNNC.GT.0 )TOPC = 1
C                            :...MAKE COST ROW AT TOP
      ENDIF
C BASIC NAMES GO NEXT
      TABAS = TABC + COLWTH
      IF( SWX )THEN
C MAKE LEVEL COLUMN AFTER BASIC NAMES
         TABX  = TABAS + NAMELN + 1
C MAKE LEVEL ROW AFTER COSTS
         TOPX  = TOPC + 1
C NONBASIC NAMES BEGIN AFTER LEVEL
         TOPNB = TOPX + 1
         TABNB = TABX + COLWTH
      ELSE
C NONBASIC NAMES BEGIN AFTER BASICS AND COSTS
         TOPNB = TOPC  + 1
         TABNB = TABAS + NAMELN + 1
      ENDIF
C                            TABNB
C                            |
C                            |<-COLWTH->|
C [obj]
C                    [level]
C                            nonbasic name  <=== TOPNB
C                            =============  <--- Underline only if
C [obj]   basic name [level]                 enough room (see below)
C         <-NAMELN->
C
      MYWDTH = TABNB + LCLNON*COLWTH
      IF( MYWDTH.GE.SCRWTH )THEN
         PRINT *,' ** SCREEN WIDTH TOO SMALL...MUST BE AT LEAST',
     1            MYWDTH+1
         GOTO 1390
      ENDIF
      MYLEN  = TOPNB + LCLBAS + 1
      IF( MYLEN.LT.SCRLEN )GOTO 500
C NEED TO REDUCE SCREEN LENGTH (MYLEN)
      IF( MYLEN.EQ.SCRLEN.AND.TOPX.GT.0 )THEN
C SUPPRESS LEVEL ROW
         TOPX  = 0
         TOPNB = TOPNB - 1
      ELSE IF( MYLEN.EQ.SCRLEN.AND.TOPC.GT.0 )THEN
C SUPPRESS OBJ ROW
         TOPC = 0
         TOPNB = TOPNB - 1
      ELSE
         PRINT *,' ** SCREEN LENGTH TOO SMALL...MUST BE AT LEAST',
     1            MYLEN+1
         PRINT *,' Note: You can spool output, then print to'//
     1           ' have vertical scrolling.'
         GOTO 1390
      ENDIF
C
500   CONTINUE
C     ========
C SET BASE LINE # FOR BASICS (TOPBAS) TO = NONBASIC LINE (TOPNB)
C ...SO TOPBAS+I = LINE FOR I-TH BASIC
      TOPBAS = TOPNB
C FOOTNOTES
      IF( FTNINF )THEN
         IF( MYLEN+1.LT.SCRLEN )THEN
            MYLEN = MYLEN+1
         ELSE
            FTNINF  = .FALSE.
            FTNDGN  = .FALSE.
         ENDIF
      ENDIF
      IF( FTNDGN )THEN
         IF( MYLEN+1.LT.SCRLEN )THEN
            MYLEN = MYLEN+1
         ELSE
            FTNDGN  = .FALSE.
         ENDIF
      ENDIF
C
      IF( MYLEN+1.LT.SCRLEN )THEN
C ADD UNDERLINING
         SWU = .TRUE.
         UNDERL = '================'
         UNDERL(COLWTH:) = ' '
         TOPBAS = TOPBAS + 1
         MYLEN = MYLEN + 1
      ELSE
         SWU = .FALSE.
      ENDIF
C INITIALIZE SCREEN
      CALL SCRCLR(MYWDTH,MYLEN,*1390)
      IF( TOPC.GT.0 )THEN
C OBJ ROW
         STR16 = OBJNAM
         IF( TABC.EQ.TABX )THEN
            CALL FSLEN(STR16,16,L)
            STR16(L+1:) = ':'
         ENDIF
         IF( TABC.GT.0 )THEN
C ...AND COL
            CALL SCRTXT(STR16,TABC,TOPC,*1390)
         ELSE
C ...NOT COL
            STR16 = OBJNAM
            IF( TABX.GT.0 )THEN
               TAB = TABX
               CALL FSLEN(STR16,16,L)
               STR16(L+1:) = ':'
            ELSE
               TAB = 1
            ENDIF
            CALL SCRTXT(STR16,TAB,TOPC,*1390)
         ENDIF
      ELSE IF( TABC.GT.0 )THEN
C OBJ COL, NOT ROW
         CALL SCRTXT(OBJNAM,TABC,TOPNB,*1390)
      ENDIF
      IF( TABX.GT.0 .AND. TOPX.GT.0 )THEN
C LEVEL ROW AND COL
         CALL SCRTXT('Level',TABX,TOPX,*1390)
      ELSE IF( TABX.GT.0 )THEN
C LEVEL COL, NOT ROW
         CALL SCRTXT('Level',TABX,TOPNB,*1390)
      ENDIF
C PUT HEADER FOR BASICS
CC 6-24-94 Replacing 'Basic' with STR16 in header
      IF( NAMELN.GE.5 )THEN
         STR16 = 'Basic'
      ELSE IF( NAMELN.EQ.4 )THEN
         STR16 = 'Bas.'
      ELSE
         STR16 = ' '
      ENDIF
      IF( SWU )THEN
         CALL SCRTXT(STR16,TABAS,TOPNB  ,*1390)
         CALL SCRTXT('=====',TABAS,TOPNB+1,*1390)
      ELSE
         CALL SCRTXT(STR16,TABAS,TOPNB,*1390)
      ENDIF
CC =======
C
C PUT NAMES [AND COSTS] [AND LEVELS] OF BASICS TO SCREEN
      DO 1500 I=1,LCLBAS
         CALL SCRTXT(LCLNAM(I),TABAS,TOPBAS+I,*1390)
         CALL SCRTXT(LCLSTA(I),TABAS+NAMELN,TOPBAS+I,*1390)
         IF( TABC.GT.0 .AND. LCLOBJ(I).NE.VINF )THEN
C                            = VINF MEANS BASIC IS A ROW, EXCEPT OBJ
C                              (NO COST ENTRY)
C ...PUT ACTIVITY COST
            CALL FR2CLJ(STR16,LCLOBJ(I),L)
            IF( L.GT.CWIDTH )L=CWIDTH
            CALL SCRTXT(STR16(:L),TABC,TOPBAS+I,*1390)
         ENDIF
         IF( TABX.GT.0 )THEN
C ...PUT LEVEL
            CALL FR2CLJ(STR16,LCLEVL(I),L)
            IF( L.GT.CWIDTH )L=CWIDTH
            CALL SCRTXT(STR16(:L),TABX,TOPBAS+I,*1390)
         ENDIF
1500  CONTINUE
C
C PUT FOOTNOTES
      BOTTOM = TOPBAS+LCLBAS+1
      IF( FTNINF )THEN
         CALL SCRTXT(CHRINF//' INFEASIBLE',TABAS+NAMELN,BOTTOM,*1390)
         BOTTOM = BOTTOM+1
      ENDIF
      IF( FTNDGN )THEN
         CALL SCRTXT(CHRDGN//' DEGENERATE',TABAS+NAMELN,BOTTOM,*1390)
      ENDIF
C PUT NAMES OF NONBASICS AT TOP AND ENTER THEIR RATES
      COLPSN = TABNB
      IF( INC1.EQ.0 )GOTO 2900
C NONBASIC COLUMNS
      REMAIN = LCLNNC
C
      DO 2800 J=INC1,NCOLS
         CALL GETMAP('COL ',J,SW)
         IF( .NOT.SW )GOTO 2800
         CALL GETST('COL ',J,STAT,STNUM)
         IF( STAT.NE.'L' .AND. STAT.NE.'U' )GOTO 2800
C COL J IS NONBASIC
         REMAIN = REMAIN-1
C ...PUT NAME
         CALL GETNAM('COL ',J,STR16)
         CALL SCRTXT(STR16(:COLWTH),COLPSN,TOPNB,*1390)
C ...PUT UNDERLINE
         IF( SWU )CALL SCRTXT(UNDERL,COLPSN,TOPNB+1,*1390)
         IF( SWC.OR.SWX )CALL GETRIM('COL ',J,VL,VU,VC,NZ)
         IF( TOPC.GT.0 )THEN
C ...PUT COSTS
            CALL FR2CLJ(STR16,VC,L)
            IF( L.GT.CWIDTH )L=CWIDTH
            CALL SCRTXT(STR16(:L),COLPSN,TOPC,*1390)
         ENDIF
         IF( TOPX.GT.0 )THEN
C ...PUT LEVELS
            IF( STAT.EQ.'L' )THEN
               CALL FR2CLJ(STR16,VL,L)
            ELSE
               CALL FR2CLJ(STR16,VU,L)
            ENDIF
            IF( L.GT.CWIDTH )L=CWIDTH
            CALL SCRTXT(STR16(:L),COLPSN,TOPX,*1390)
         ENDIF
C GET ITS RATES
         CALL AFTRAN('COL ',J)
C ...NOW ZVALUE(i) = RATE OF i-TH BASIC VARIABLE
C
C ENTER RATES OF BASIC VARIABLES
         DO 2500 I=1,LCLBAS
            PIVOT = LCLPIV(I)
            VRATE = ZVALUE(PIVOT)
C PUT RATE TO SCREEN
            CALL FR2CLJ(STR16,VRATE,L)
            IF( L.GT.CWIDTH )L=CWIDTH
            CALL SCRTXT(STR16(:L),COLPSN,TOPBAS+I,*1390)
2500     CONTINUE
C ADVANCE TABLEAU COLUMN POSITION (COLPSN)
         COLPSN = COLPSN + COLWTH
         IF( REMAIN.EQ.0 )GOTO 2900
2800  CONTINUE
C
2900  CONTINUE
      IF( INR1.EQ.0 )GOTO 3900
C NONBASIC ROWS
      REMAIN = LCLNNR
C
      DO 3800 J=INR1,NROWS
         CALL GETMAP('ROW ',J,SW)
         IF( .NOT.SW )GOTO 3800
         CALL GETST('ROW ',J,STAT,STNUM)
         IF( STAT.NE.'L' .AND. STAT.NE.'U' )GOTO 3800
C ROW J IS NONBASIC
         REMAIN = REMAIN-1
C ...PUT NAME
         CALL GETNAM('ROW ',J,STR16)
         CALL SCRTXT(STR16(:COLWTH),COLPSN,TOPNB,*1390)
C ...PUT UNDERLINE
         IF( SWU )CALL SCRTXT(UNDERL,COLPSN,TOPNB+1,*1390)
         IF( TOPX.GT.0 )THEN
C ...PUT LEVELS
            CALL GETRIM('ROW ',J,VL,VU,VC,NZ)
            IF( STAT.EQ.'L' )THEN
               CALL FR2CLJ(STR16,VL,L)
            ELSE
               CALL FR2CLJ(STR16,VU,L)
            ENDIF
            IF( L.GT.CWIDTH )L=CWIDTH
            CALL SCRTXT(STR16(:L),COLPSN,TOPX,*1390)
         ENDIF
C GET ITS RATES
         CALL AFTRAN('ROW ',J)
C NOW ZVALUE(i) = RATE OF i-TH BASIC VARIABLE
C ENTER RATES OF BASIC VARIABLES
         DO 3500 I=1,LCLBAS
            PIVOT = LCLPIV(I)
            VRATE = ZVALUE(PIVOT)
C PUT RATE TO SCREEN
            CALL FR2CLJ(STR16,VRATE,L)
            IF( L.GT.CWIDTH )L=CWIDTH
            CALL SCRTXT(STR16(:L),COLPSN,TOPBAS+I,*1390)
3500     CONTINUE
C
         COLPSN = COLPSN + COLWTH
         IF( REMAIN.EQ.0 )GOTO 3900
3800  CONTINUE
C
3900  CONTINUE
C =============== SCREEN NOW SET ... PRINT ==============
      LINE = 0
      CALL SCRPRT(LINE,MYLEN,*9000)
C
9000  RETURN
C
C ERROR RETURNS
1310  CONTINUE
      PRINT *,' ** SORRY, TOO MANY BASICS IN SUBMATRIX...MAX =',LCLMXB
1390  CONTINUE
      RCODE = 1
      RETURN
C
C ** ARTABL ENDS HERE
      END
      SUBROUTINE AFTRAN(ROWCOL,NUMBER)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This computes rates of substitution for nonbasic ROWCOL NUMBER
C and puts them into ZVALUE (in COMMON).
C
      CHARACTER*(*) ROWCOL
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      CALL GALPHA(ROWCOL,NUMBER,ZVALUE,NROWS)
      IF(ROWCOL.EQ.'ROW ')ZVALUE(NUMBER)=-1.
      CALL GFTRAN(ZVALUE,NROWS)
C
C ** AFTRAN ENDS HERE
      END
