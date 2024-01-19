C              ::: ARATE1.FOR 7-29-95 :::
C
C Earlier dates deleted
C       3-29-94...Added WIDTH option to TABLEAU
C       1-07-95...Blockage footnote in tabular format (CC 7-29)
C
C This contains the following ANALYZE subroutine.
C
C    ARATE......execute RATEOF command
C
      SUBROUTINE ARATE(CLIST,FIRST,LAST,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C          :...NEEDED FOR SCRWTH
C
C This gives rates of substitution and range information
C ...Uses FTEXT for output.
C
C SYNTAX:   RATEOF  [ROW|COL [conditional]] [//options]
C   options := EQUATION ...form:  basic = level + rate x nonbasic
C              OBJECTIV ...tabular with objective coefficient
C              C ..........same as OBJECTIV
C              D ..........reduced cost factors (obj x rate)
C              LEVEL ......tabular with level
C              P ..........price factors (obj x rate)
C              X or Y .....same as LEVEL
C              SYNTAX .....syntactic translation
C              RANGE ......range only (no rates)
C                    ...if from rule file, range is not printed; it
C                       is put into params:  VMIN,VMAX,MINNAM,MAXNAM
C              TABLEAU ....tableau form of output (not working yet)
C
C  The options yield the following possible output formats.
C
C //EQUATION          (or simply //E)
C   BASIC = current level + rate x NONBASIC                   Form 1
C
C No parameter spec:
C
C   <type>       Rate of   Least      Greatest                Form 2
C   Variable     Subst.    Value      Value
C   =============================================
C
C
C //LEVEL              (or simply //L or //X or //Y)
C
C   <type>       <type>      Rate of   Least      Greatest    Form 3
C   Variable     Level       Subst.    Value      Value
C   =======================================================
C
C <type> := Basic | Nonbasic   (opposite of impulse)
C
C //OBJECTIV          (or simply //O or //C)
C
C   <type>       Objective   Rate of   Least      Greatest    Form 4
C   Variable     Coeff.      Subst.    Value      Value
C   =========================================================
C
C //D or P [,SYNTAX]  (for nonbasic impulse only)
C
C   Basic        Objective   Rate of   Net Effect             Form 5
C   Activity     Coeff.      Subst.    (Obj x Rate)
C   =================================================
C                                                       [...syntax]
C
C The meaning of "Least" and "Greatest" Value depends on the type of
C impulse (which is opposite the type of the response).
C
C   If the impulse variable is Nonbasic, each (basic) response variable
C     imposes bounds on the impulse level, and these are displayed.
C     Then, the least Greatest Value and greatest Least Value are
C     given at the end.
C
C   If the impulse variable is Basic, each (nonbasic) response variable
C     contributes to the total (myopic) range of the basic variable,
C     and these are displayed.  Then, the sum of the Least Values and
C     of the Greatest Values are given at the end.
C
C //SYNTAX            (or simply //S)                         Form 6
C
C TABLEAU   [C/OBJ, LEV/X/Y, WIDTH=number]                    Form 7
C                     Nonbasic
C      Basic       cols      rows
C      ========  ========  =========
C       cols
C       rows
C  :           :...Levels go here if specified
C  :...Costs go 1st, if specified
C
C For basic impulse:
C   Level of <translation>
C   = <rate>xLevel of <translation>
C     <rate>xLevel of <translation>
C       ...
C
C For nonbasic impulse:
C   Level of <translation> affects the following.
C     Increases/Decreases <translation> at rate = <|rate|>
C       ...
C
C The first part of <translation> is 'equation that' or
C 'activity that', which presumes syntax begins with verb.
C
C                         Algorithm Notes
C                         ===============
C The rate of substitution R(u,v) is defined as the marginal rate
C of change in the "response variable" u with respect to the
C "impulse variable" v.  The equation that determines this is a
C re-write of the original system, y=Ax, putting the basics on the
C left (some of which are y-variables and some are x-variables) and
C the nonbasics on the right.  Then, the general form is:
C                    u = B* N* v,
C where B* is the (full) basis inverse, which contains -basic columns
C from A (note the negative sign) and +unit vectors for the logicals;
C and, N* contains +nonbasic columns from A and -unit vectors for the
C nonbasic logicals.
C
C For the rate we seek to compute, consider the form:
C       Basic var. = Current level + Rate x Nonbasic Var.
C The Rate is found as follows.
C
C If u is the Basic Variable and v is the Nonbasic Variable, we FTRAN
C the nonbasic column, say N(j), which is either a column from A or
C it's a -unit column, the latter being the case for a Nonbasic Row (y)
C variable.  We FTRAN once for the impulse variable (v) and use the
C inner-product to obtain the Rate for each basic variable.
C
C If u is the Nonbasic Variable and v is the Basic Variable, we BTRAN
C the unit vector, e(p), where p=pivot row of u.  Then, for each
C Nonbasic response variable, the Rate is the inner product of its
C column vector with this BTRAN vector.
C ===================================================================
C
      CHARACTER*(*) CLIST
C
C LOCAL
C   IMPULSE STAT
      CHARACTER*1  STIMP
C   IMPULSE TYPE (ROW|COL)
      CHARACTER*4  BRC
C   RESPONSE TYPE FOR HEADER
      CHARACTER*8  RESPHD
C   BLOCKING RESPONSES (IF RESPONSE IS BASIC)
      CHARACTER*16 MINNAM,MAXNAM
C   FORMAT SWITCHES
      LOGICAL*1    SWRNG,SWC,SWX,SWEQN,SWRSYN,SWD,SWP,SWDP,SWTABL
C
      CHARACTER*128 CTEMP
      CHARACTER*16  CNAME,RNAME
      CHARACTER*8   OPTION(11)
      CHARACTER*4   ROWCOL
      CHARACTER*1   STAT,CHAR
      LOGICAL*1     SW,SWROW,SWCOL
      PARAMETER     (FREQMS=PMXROW)
C                    :...MESSAGE FREQUENCY FOR BAGNDA
      CHARACTER*2   CHCHAR
      DATA          CHCHAR/' ë'/
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF(.NOT.SWRATE)THEN
C BASIS MUST BE SETUP
         IF(SWMSG)THEN
            PRINT *,' Submatrix will be cleared...please reset after',
     1              ' basis is setup.'
            PRINT *,' ...To avoid this next time write packed file',
     1              ' after basis is setup.'
         ENDIF
         CALL BAGNDA(ZVALUE,MAXLST,SWMSG,FREQMS,RCODE)
C                    :....PRESUMES VALLST IS DIMENSIONED 2*MAXLST
         SWRATE = RCODE.EQ.0
         IF(.NOT.SWRATE)THEN
            IF( RCODE.EQ.2 )THEN
               PRINT *,' ...Solution is not basic'
            ELSE
               PRINT *,' ...Basis not setup to support RATEOF'
            ENDIF
            RETURN
         ENDIF
      ENDIF
C
C SET DEFAULT OUTPUT OPTIONS
      SWEQN = .FALSE.
      SWC   = .FALSE.
      SWD   = .FALSE.
      SWP   = .FALSE.
      SWX   = .FALSE.
      SWRSYN= .FALSE.
      SWROW = .TRUE.
      SWCOL = .TRUE.
      SWRNG = .FALSE.
      SWDP  = .FALSE.
      SWTABL= .FALSE.
C SEE IF // IS PRESENT (OPTIONS TO CONTROL OUTPUT FORMAT)
      CALL FLOOK(CLIST,FIRST,LAST,'//',IPARM)
      IF( IPARM.EQ.0 )GOTO 90
C PARSE OPTIONS
      F = IPARM+2
      OPTION(1) = 'EQUATION'
      OPTION(2) = 'LEVEL'
      OPTION(3) = 'X'
      OPTION(4) = 'Y'
      OPTION(5) = 'OBJECTIV'
      OPTION(6) = 'C'
      OPTION(7) = 'SYNTAX'
      OPTION(8) = 'RANGE'
      OPTION(9) = 'D'
      OPTION(10)= 'P'
      OPTION(11)= 'TABLEAU'
      NUMBER = 11
C
      RCODE = -1
      CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
      IF( RCODE.NE.0 )RETURN
      SWTABL = NUMBER.EQ.11
      IF( SWTABL )THEN
C TABLEAU FORMAT (FORM 7)
         OPTION(1) = 'C'
         OPTION(2) = 'OBJECTIV'
         OPTION(3) = 'X'
         OPTION(4) = 'Y'
         OPTION(5) = 'LEVEL'
         OPTION(6) = 'WIDTH'
C DEFAULT COLUMN WIDTH (BEFORE CONSIDERING NAME LENGTH)
         CWIDTH = 12
50       CONTINUE
C ...SEE IF OBJECTIV/C | X/Y/LEVEL | WIDTH=number SPECIFIED
           NUMBER = 6
           CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
           IF( RCODE.NE.0 )RETURN
           IF( NUMBER.EQ.0 )GOTO 80
C              ====================
           IF( NUMBER.LE.2 )THEN
              SWC = .TRUE.
           ELSE IF( NUMBER.LT.6 )THEN
              SWX = .TRUE.
           ELSE
C WIDTH SPECIFIED...GET NUMBER
              CALL FVRNG(CLIST,F,LAST,VL,VU,VINF,CHAR,RCODE)
              IF( RCODE.NE.0 )RETURN
              IF( VL.GE.16. )THEN
                 CWIDTH = 16
              ELSE
                 CWIDTH = VL + .1
              ENDIF
           ENDIF
         GOTO 50
      ENDIF
C OK, SET SWITCH FOR OUTPUT OPTION
      IF( NUMBER.EQ.1 )THEN
         SWEQN = .TRUE.
         OPTION(1) = 'OBJECTIV'
         OPTION(2) = 'ROWS'
         OPTION(3) = 'COLUMNS'
         NUMBER = 3
         F0 = F
         CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
         IF( RCODE.NE.0 )RETURN
         IF( NUMBER.EQ.0 )GOTO 80
         SWC = NUMBER.EQ.1
         IF(.NOT.SWC)F = F0
      ELSE IF( NUMBER.EQ.9.OR.NUMBER.EQ.10 )THEN
         SWD = NUMBER.EQ.9
         SWP = NUMBER.EQ.10
         IF( SWEXPL )THEN
C SEE IF SYNTAX WANTED AS WELL
            SWRSYN = SWSYN
C                  :...DEFAULT DETERMINED BY SYNTAX SWITCH
            OPTION(1) = 'SYNTAX'
            OPTION(2) = 'NOSYNTAX'
            NUMBER = 2
            CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
            IF( RCODE.NE.0 )RETURN
            IF( NUMBER.EQ.1 )SWRSYN = .TRUE.
            IF( NUMBER.EQ.2 )SWRSYN = .FALSE.
         ENDIF
      ELSE
         IF( NUMBER.LT.5 )THEN
            SWX = .TRUE.
         ELSE IF( NUMBER.LT.7 )THEN
            SWC = .TRUE.
         ELSE IF( NUMBER.EQ.7 )THEN
            IF(.NOT.SWEXPL)THEN
               PRINT *,' ** NO SYNTAX HAS BEEN READIN'
               RCODE = 1
               RETURN
            ENDIF
            SWRSYN = .TRUE.
         ELSE
            SWRNG = .TRUE.
            IF( NUMBER.EQ.8 )THEN
C //RANGE [TABLE]
C          :...SEE IF TABLE FORMAT
               OPTION(1) = 'TABLE '
               NUMBER = 1
               CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
               IF( RCODE.NE.0 )RETURN
               SWX = NUMBER.GT.0
            ENDIF
         ENDIF
      ENDIF
C WE NOW HAVE THE FOLLOWING SWITCHES CORRESPONDING TO OUTPUT FORMAT
C SWRNG = F in all these cases;  form for range output (without rates)
C is one line.  This is used in rule files, or when someone just
C wants the range because the non-zero rates are too numerous.
C
C SWC SWD SWP SWX SWEQN SWRSYN
C  F   F   F   F    T     F    Form 1 (NUMBER = 1)
C  F   F   F   F    F     F    Form 2 (NUMBER = 0...No option spec)
C  F   F   F   T    F     F    Form 3 (NUMBER = 2,3,4)
C  T   F   F   F    F     F    Form 4 (NUMBER = 5,6)
C  F   T   F   F    F     *    Form 5 (NUMBER = 9)...added 12-13-92
C  F   F   T   F    F     *    Form 5 (NUMBER =10)...added 12-13-92
C  F   F   F   F    F     T    Form 6 (NUMBER = 7)
C  *   F   F   *    F     F    Form 7 (NUMBER = 11)
C Form 5 can have syntax or not...added 3-9-93
C Form 7 can have costs and/or levels...added 3-11-93
C
C These are the only recognized switch combinations.  In forms 1-4 and
C 6, at most one switch is True.  In form 5, SWSYN can be T or F.  In
C form 7, SWC and/or SWX can be T or F.
C
      SWDP = SWD .OR. SWP
      IF( NUMBER.GT.0 .AND. .NOT.SWDP )THEN
C SEE IF ROW | COL SPECIFIED
         OPTION(1) = 'ROWS'
         OPTION(2) = 'COLUMNS'
         NUMBER = 2
         CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
         IF(RCODE.NE.0)RETURN
         SWROW = NUMBER.EQ.0 .OR. NUMBER.EQ.1
         SWCOL = NUMBER.EQ.0 .OR. NUMBER.EQ.2
      ELSE IF( SWDP )THEN
C PUT OBJ COEF. COLUMN
         SWC = .TRUE.
C RESTRICT RESPONSES TO COLUMNS
         SWROW = .FALSE.
      ENDIF
C =============================
80    CONTINUE
C ===== END OF //OPTIONS ======
      LAST = IPARM-1
C
90    CONTINUE
C PARSE CONDITIONAL
      CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
      IF( RCODE.NE.0 )RETURN
      IF( SWTABL )THEN
C TABLEAU FORMAT
         CALL ARTABL(SWC,SWX,CWIDTH,RCODE)
         RETURN
      ENDIF
C  === NOT TABLEAU ===
      IF( SWDP )THEN
C SET DISPLAY TO COLUMNS (SWD) OR ROWS (SWP)...OVER-RIDE SWITCH (SWDISP)
         SWDISP = SWD
C REMOVE BASICS
         IF( SWD )THEN
            CTEMP = 'DEL COL * S=BI'
         ELSE
            CTEMP = 'DEL ROW * S=BI'
         ENDIF
         F = 1
         L = 15
         CALL ASBSET(CTEMP,F,L,RCODE)
         IF( RCODE.NE.0 )THEN
            PRINT *,' ** SYSERR ARATE 90'
            RETURN
         ENDIF
      ENDIF
C
      IF( SWDISP )THEN
         ROWCOL = 'COL '
         K = 2
         LASTRC = NCOLS
      ELSE
         ROWCOL = 'ROW '
         K = 1
         LASTRC = NROWS
      ENDIF
C
      NRC = NRCSUB(K)
      IF(NRC.EQ.0)THEN
         PRINT *,' No ',ROWCOL(:3),'s in Submatrix'
         RETURN
      ENDIF
C NRC = NUMBER OF ROWS|COLUMNS IN SUBMATRIX
C       (IMPULSES, TAKEN 1 AT A TIME)
C
C PREPARE TO LOOP OVER ROW|COL IMPULSES TO DISPLAY RESPONSE RATES
      LINE = 1
      IF( SWRNG.AND.SWX )THEN
C RANGE ONLY, BUT IN TABLE FORMAT (ADDED 3-29-93)...PUT HEADER
         CTEMP = ' '
         CTEMP(NAMELN+ 2:) = 'Current'
         CTEMP(NAMELN+42:) = '=== Implied Range ==='
         CALL FSLEN(CTEMP,80,L)
         IF( L+1.GE.SCRWTH )THEN
            PRINT *,' Sorry, screen width too small...need',L+2
            RCODE = 1
            RETURN
         ENDIF
         CALL FTEXT(CTEMP,L,1,0,LINE,'CLEAR ',*9000)
C
         CTEMP(NAMELN+ 2:) = 'Level'
         CTEMP(NAMELN+15:) = 'LO'
         CTEMP(NAMELN+28:) = 'UP'
         CTEMP(NAMELN+42:) = 'Min'
         CTEMP(NAMELN+55:) = 'Max'
         CALL FSLEN(CTEMP,90,L)
         CALL FTEXT(CTEMP,L,1,0,LINE,'CLEAR ',*9000)
      ENDIF
C
      IF( .NOT.SWRSYN .OR. SWDP )THEN
C SET TAB1 (FOR TABULAR AND EQUATION FORMATS)
         IF(NAMELN.LT.9)THEN
            TAB1 = 11
         ELSE
            TAB1 = NAMELN + 2
         ENDIF
         TABR = TAB1
C SET TABS FOR OTHER FIELDS
         IF( NAMELN.LE.12 )THEN
            WIDTH = 13
         ELSE
            WIDTH = NAMELN+1
         ENDIF
         IF( SWEQN )THEN
            TABL = TAB1 + WIDTH
            TABR = TABL
            IF( SWC )THEN
               TABG = TABR + 2*WIDTH
            ELSE
               TABG = TABL + WIDTH
            ENDIF
         ELSE
C ...TABULAR OUTPUT
            IF( SWC.OR.SWX )TABR = TABR+13
            TABL = TABR + WIDTH
            IF( SWDP )THEN
               TABG = TABL
            ELSE
               TABG = TABL + WIDTH
            ENDIF
         ENDIF
C ...LENGTH OF UNDERLINE
         LUNDER = TABG + 12
      ENDIF
C
C    ::: MAIN LOOP OVER ROWS|COLUMNS :::
C
      DO 1000 NUMBER=RCSUB1(K),LASTRC
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF(.NOT.SW)GOTO 1000
         NRC = NRC - 1
         CALL GETNAM(ROWCOL,NUMBER,CNAME)
         CALL GETSOL(ROWCOL,NUMBER,VXIMP,VPIMP,STIMP,STNUM)
         CALL GETRIM(ROWCOL,NUMBER,VLIMP,VUIMP,VCIMP,NZIMP)
C
C IMPULSE VARIABLE IS [ROWCOL CNAME] (NUMBER) WITH
C    STAT=STIMP, LEVEL=VXIMP, PRICE=VPIMP
C    BOUNDS = VLIMP, VUIMP  AND  COST=VCIMP
C
         IF( SWRNG )GOTO 200
C        =================== RANGE...SKIP PRINTING OF RATES
C
         IF( SWRSYN .AND. .NOT.SWDP )THEN
C SYNTAX OPTION (EXCLUSIVELY)
            IF(ROWCOL.EQ.'ROW '.AND.NUMBER.EQ.OBJNUM)THEN
               CTEMP = ROWCOL//RNAME
               CALL FSLEN(CTEMP,30,LAST)
               CTEMP(LAST+2:) = '(objective)'
            ELSE
C   TRANSLATE ROW|COL
               CALL EXTRC(ROWCOL,CNAME,CTEMP,LAST,RCODE)
               RCODE = 0
C                      :...IF EXTRC RETURNED RCODE <> 0, THIS
C                          MEANS THE SYNTAX HAS NO TRANSLATION,
C                          BUT WE SHALL PROCEED ANYWAY (USER WILL
C                          SEE MESSAGE ABOUT THIS)
C   REMOVE {ROW COLUMN} <name>
               FIRST = 1
               CALL FTOKEN(CTEMP,FIRST,LAST,RNAME, 8,STAT)
               CALL FTOKEN(CTEMP,FIRST,LAST,RNAME,16,STAT)
               CLIST = ROWCOL//'that '//CTEMP(FIRST:LAST-1)
               CTEMP = CLIST
            ENDIF
            CALL FSLEN(CTEMP,120,LAST)
C   FORM HEADER
            CLIST = 'Change in level of '//CTEMP(:LAST)
            CALL FSLEN(CLIST,120,LAST)
            IF(STIMP.EQ.'L'.OR.STIMP.EQ.'U')THEN
               CLIST(LAST+2:) = 'affects the following.'
            ELSE
               LAST = LAST+2
               CLIST(LAST:) = '='
            ENDIF
C   PRINT HEADER
            CALL FSLEN(CLIST,100,LAST)
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
         ELSE
C NOT SYNTAX OPTION
C  PRINT INFO FOR THIS VARIABLE BEFORE WRITING RATES OF SUBSTITUTION
            CLIST = ' Rates of substitution for '//ROWCOL//CNAME
            CALL FSLEN(CLIST,128,LAST)
            IF( SWRSYN )THEN
C SHOW ITS SYNTAX (AS IN SHOWCOL)
               CALL EXTRC(ROWCOL,CNAME,CTEMP,LS,RCODE)
               IF( RCODE.EQ.0 )THEN
C STRIP AWAY COL NAME
                  F = 1
                  CALL FTOKEN(CTEMP,F,LS,CNAME,8,STAT)
                  CALL FTOKEN(CTEMP,F,LS,CNAME,NAMELN,STAT)
                  CLIST(LAST+1:) = '...'//CTEMP(F:LS)
                  CALL FSLEN(CLIST,128,LAST)
               ELSE
                  RCODE = 0
               ENDIF
            ENDIF
            CALL FTEXT(CLIST,LAST,5,-4,LINE,'CLEAR ',*9000)
C SHOW SOLUTION VALUES
            CALL FR2CLJ( RNAME,VXIMP,LAST )
            CLIST = ' ...Status='//STIMP//' Level='//RNAME(1:LAST)
            CALL FSLEN(CLIST,60,LAST)
            IF( SWD )THEN
               CALL FR2CLJ( RNAME,VCIMP,L)
               CALL FSLEN(OBJNAM,16,L)
               CLIST(LAST+2:) = OBJNAM(:L)//'='//RNAME
               CALL FSLEN(CLIST,90,LAST)
            ENDIF
            CALL FR2CLJ( RNAME,VPIMP,L )
            CLIST(LAST+3:) = 'Price='//RNAME(1:L)
            IF( SWEQN .AND. SWC .AND. STIMP.NE.'B'.AND.STIMP.NE.'I' )
C EQUATION FORM WITH OBJ RATES...PUT HEADER
     1         CLIST(TABG:) = 'Net rate of'//CHCHAR//OBJNAM
            CALL FSLEN(CLIST,128,LAST)
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
         ENDIF
C =============== END HEADER INFO ================
200      CONTINUE
C =================================================
         IF( STIMP.EQ.'L'.OR.STIMP.EQ.'U' )THEN
C SIMPLIFY STAT (STIMP) FOR COMPARISONS LATER
            STIMP='N'
         ELSE
C IMPULSE VARIABLE IS BASIC, SO RESPONSE VARIABLES ARE NONBASIC
C ...IN THIS CASE WE MUST BTRAN THE PIVOT ROW
            CALL GETPIV(PIVOT,ROWCOL,NUMBER)
            CALL GALPHA('ROW ',PIVOT,ZVALUE,NROWS)
            CALL GBTRAN(ZVALUE,NROWS)
         ENDIF
C ...STIMP = 'N' IFF IMPULSE VARIABLE IS NONBASIC
C
         IF( SWRNG.OR.SWEQN.OR.(SWRSYN .AND. .NOT.SWDP) )GOTO 500
C
C  PREPARE HEADER FOR TABULAR OUTPUT
            IF( STIMP.EQ.'N' )THEN
C IMPULSE VARIABLE IS NONBASIC, SO RESPONSE VARIABLES ARE BASIC
               RESPHD = 'Basic'
            ELSE
C IMPULSE VARIABLE IS BASIC, SO RESPONSE VARIABLES ARE NONBASIC
               RESPHD = 'Nonbasic'
            ENDIF
            CLIST = RESPHD
C  BEGIN HEADER
         IF(SWC)THEN
            CALL FSLEN(OBJNAM,NAMELN,L)
            IF(L.LE.12)THEN
               CLIST(TAB1+2:) = OBJNAM
            ELSE
               CLIST(TAB1+2:) = 'Objective'
            ENDIF
         ELSE IF(SWX)THEN
            CLIST(TAB1:) = '   '//RESPHD
         ENDIF
         CLIST(TABR:) =        '  Rate of'
         IF( SWDP )THEN
            CLIST(TABL:) = '  Net Rate'
         ELSE
            CLIST(TABL:) =        '   Least'
            CLIST(TABG:) =        '   Greatest'
         ENDIF
         LAST = LUNDER
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C
C  SECOND LINE OF HEADER
         IF( SWDP )THEN
            CLIST = 'Activity'
         ELSE
            CLIST = 'Variable'
         ENDIF
         IF( SWC )CLIST(TAB1:) = 'Coefficient'
         IF( SWX )CLIST(TAB1:) = '   Level'
         CLIST(TABR:)        = 'Substitution'
         IF( SWDP )THEN
            CALL FSLEN(OBJNAM,16,L)
            CLIST(TABL:) = '('//OBJNAM(:L)//' x Rate)'
         ELSE
            IF(STIMP.EQ.'N')THEN
               CLIST(TABL:) =     '   Change'
               CLIST(TABG:) =     '   Change'
            ELSE
               CLIST(TABL:) =     '   Term'
               CLIST(TABG:) =     '   Term'
            ENDIF
         ENDIF
         LAST = LUNDER
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C
C UNDERLINE HEADER
         DO 490 I=1,LUNDER
490      CLIST(I:I) = '='
         LAST = LUNDER
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C     ==== TABLE HEADER COMPLETE ====
C
500                    CONTINUE
C  ==== WE ARRIVE HERE REGARDLESS OF FORMAT OPTION ====
C
         IF( STIMP.EQ.'N' )THEN
C IMPULSE VARIABLE IS NONBASIC (SO RESPONSES ARE BASIC)
C ...GET RATES
               CALL AFTRAN(ROWCOL,NUMBER)
C ...ZVALUE NOW CONTAINS THE RATE OF EACH BASIC RESPONSE
C ...INITIALIZE BLOCKAGE (LEAST AND GREATEST)
               VMIN   = -VINF
               MINNAM = '(no block)'
               MAXNAM = MINNAM
            ELSE
C IMPULSE VARIABLE IS BASIC (SO RESPONSES ARE NONBASIC)
               VMIN = 0.
            ENDIF
            VMAX   = -VMIN
C VMIN, VMAX = LEAST, GREATEST (RANGE OR BLOCKAGE)
C INITIALIZE SUM (VRSUM)
            VRSUM  = 0.
C           :...TOTAL NET RATE WRT OBJ (IN SYN, D, AND EQ+OBJ OPTIONS)
C
C  LOOP OVER ROWS TO GET RATES AND RANGES
C
            DO 550 I=1,NROWS
               IF( STIMP.NE.'N' )THEN
C IMPULSE IS BASIC
C ...TRANSFER TO SINGLE PRECISION (FOR COLUMN LOOP LATER)
                  VALLST(I) = ZVALUE(I)
                  CALL GETST('ROW ',I,STAT,STNUM)
                  IF( STAT.EQ.'B'.OR.STAT.EQ.'I' )GOTO 550
C ...RESPONSE ROW (I) IS NONBASIC, SO SHOW ITS RATE
                  BRC = 'ROW '
                  NUMBR = I
                  VRATE = -ZVALUE(I)
C NOTE: THE RATE IS THE INNER PRODUCT OF BTRAN VECTOR (ZVALUE)
C       WITH  - I-TH UNIT VECTOR, OR SIMPLY -ZVALUE(I).
C
               ELSE
C IMPULSE IS NONBASIC
C ...GET IDENTITY OF BASIC VARIABLE PIVOTING ON ROW I
                  CALL GETBVP(I,BRC,NUMBR)
                  VRATE = ZVALUE(I)
               ENDIF
               IF(ABS(VRATE).LE.VTOLAB)GOTO 550
C   RATE (VRATE) NOT 0, SO PROCEED
C
C NOW BRC = ROW | COL = TYPE OF RESPONSE VARIABLE
C     NUMBR = INDEX NUMBER OF RESPONSE VARIABLE
C            .....[BRC NUMBR] THUS SPECIFIES RESPONSE VARIABLE
C     VRATE = RATE OF SUBSTITUTION
C READY FOR DISPLAY AND UPDATE
              CALL ARATDS(CNAME,BRC,NUMBR,VRATE,VCIMP,
     1                    SWRNG,SWC,SWX,SWEQN,SWRSYN,SWDP,SWROW,SWCOL,
     2                    TAB1,TABR,TABL,TABG,LINE,
     3                    MINNAM,VMIN,MAXNAM,VMAX,VRSUM,*9000)
C NEXT ROW
550         CONTINUE
C    ::: END LOOP OVER ROWS :::
C
            IF(STIMP.EQ.'N')GOTO 800
C
C IMPULSE VARIABLE IS BASIC, SO WE MUST NOW LOOP OVER COLUMNS
C
            DO 600 COL=1,NCOLS
               CALL GETST('COL ',COL,STAT,STNUM)
               IF(STAT.EQ.'B'.OR.STAT.EQ.'I')GOTO 600
C COL IS NONBASIC
C ...GET RATE FROM INNER PRODUCT OF ITS COLUMN WITH VALLST
C    ...RECALL VALLST = ZVALUE = BTRAN(E):
C               E = UNIT VECTOR FOR PIVOT POSITION OF IMPULSE
C
               CALL GPRDCT(VALLST,COL,VRATE)
               IF(ABS(VRATE).LE.VTOLAB)GOTO 600
C READY FOR DISPLAY AND UPDATE
               CALL ARATDS(CNAME,'COL ',COL,VRATE,VCIMP,
     1                     SWRNG,SWC,SWX,SWEQN,SWRSYN,SWDP,SWROW,SWCOL,
     2                     TAB1,TABR,TABL,TABG,LINE,
     3                     MINNAM,VMIN,MAXNAM,VMAX,VRSUM,*9000)
600         CONTINUE
C   ::: END OF COLUMN LOOP FOR BASIC IMPULSE :::
C
800         CONTINUE
C THIS IMPULSE VARIABLE IS DONE
            IF( SWRNG )GOTO 899
            IF( SWRSYN .AND. .NOT.SWDP )THEN
C SYNTAX FORMAT (EXCLUSIVELY)
               IF( SWCOL.AND.STIMP.EQ.'N' )THEN
C ...WRITE TOTAL NET RATE
                  CALL FR2CLJ( RNAME,VRSUM,L )
                  CLIST = '...Total Net Rate = '//RNAME(1:L)
                  CALL FSLEN(CLIST,50,LAST)
                  CLIST(LAST+2:) = '= effect on '//OBJNAM
                  CALL FSLEN(CLIST,50,LAST)
                  CLIST(LAST+2:) = 'from changes induced in the'//
     1                   ' basic levels.'
C
                  IF(ROWCOL.EQ.'COL ')THEN
C NOTE THAT REDUCED COST = INPUT COST + NET RATE
                     CALL FSLEN(CLIST,128,LAST)
                     CALL FTEXT(CLIST,LAST,1,0,LINE,'KEEP ',*9000)
                     CALL GETRIM(ROWCOL,NUMBER,VL,VU,VC,NZ)
                     CALL FR2CLJ( CNAME,VC,L )
                     CLIST(LAST+2:) = 'Note that Reduced cost = '
     1                                //OBJNAM
                     CALL FSLEN(CLIST,127,LAST)
                     IF(LAST.GT.60)
     1                  CALL FTEXT(CLIST,LAST,1,0,LINE,'KEEP ',*9000)
                     CLIST(LAST+2:) = 'Coefficient + Net Rate = '
     1                               //CNAME(1:L)
                     CALL FSLEN(CLIST,128,LAST)
                     IF(RNAME(1:1).EQ.'-')THEN
                        CLIST(LAST+2:) = '- '//RNAME(2:)
                     ELSE
                        CLIST(LAST+2:) = '+ '//RNAME
                     ENDIF
                     V = VC + VRSUM
                     CALL FR2CLJ( CNAME,V,L )
                     CALL FSLEN(CLIST,128,LAST)
                     CLIST(LAST+2:) = '= '//CNAME(1:L)
                  ENDIF
                  CALL FSLEN(CLIST,128,LAST)
                  CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
               ENDIF
               GOTO 990
            ENDIF
C NOT SYNTAX FORMAT...UNDERLINE ITS OUTPUT
            DO 890 K=1,LUNDER
890         CLIST(K:K) = '-'
            LAST = LUNDER
            IF( SWEQN.AND.SWC .AND. STIMP.EQ.'N' )THEN
C EQUATION FORMAT WITH NET RANGES SHOWN
C ...GIVE TOTAL NET RATE (NOT RANGES)
               CALL FR2CRJ( RNAME,VRSUM,F )
               CLIST(TABG-9:) = ' Total = '//RNAME(1:12)
               CALL FSLEN(CLIST,80,LAST)
               CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
               GOTO 990
            ENDIF
C PRINT UNDERLINING
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
            IF( SWDP )THEN
C D FORMAT...GIVE TOTAL NET RATE (NOT RANGES)
               CALL FR2CRJ( RNAME,VRSUM,F )
               CLIST(TABL-9:) = ' Total ='//RNAME(1:12)
               CALL FSLEN(CLIST,80,LAST)
               CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
               GOTO 990
            ENDIF
C
C GIVE FINAL BLOCKAGES (NONBASIC IMPULSE) OR SUMS (BASIC IMPULSE)
C
899         CONTINUE
            IF( STIMP.EQ.'N' )THEN
C ADD CURRENT IMPULSE LEVEL TO ITS RANGE (OF CHANGE)
               VMIN = VMIN + VXIMP
               VMAX = VMAX + VXIMP
            ENDIF
C RANGE = [VMIN, VMAX], BLOCKED BY MINNAM, MAXNAM
C ADDED 3-29-93
C =============
            IF( SWRNG.AND.SWX )THEN
               CTEMP = CNAME
               CALL FR2CLJ( CTEMP(NAMELN+ 2:),VXIMP,F )
               CALL FR2CLJ( CTEMP(NAMELN+15:),VLIMP,F )
               CALL FR2CLJ( CTEMP(NAMELN+28:),VUIMP,F )
               CALL FR2CLJ( CTEMP(NAMELN+42:),VMIN, F )
               CALL FR2CLJ( CTEMP(NAMELN+55:),VMAX, F )
               CALL FSLEN(CTEMP,90,L)
               CALL FTEXT(CTEMP,L,1,0,LINE,'CLEAR ',*9000)
               GOTO 990
            ENDIF
C =============
            CLIST = ' '
            CALL FSLEN(CNAME,16,LC)
            CLIST(TABL-LC-11:) = CNAME(:LC)//' Range:'
            CALL FSLEN(CLIST,80,LC)
            CALL FR2CRJ( CLIST(TABL-1:),VMIN,F )
            CALL FR2CRJ( CLIST(TABG-1:),VMAX,F )
            IF( SWRNG.AND.SWRULE.AND..NOT.SWX )THEN
C RANGE OPTION FROM RULE FILE...DO NOT PRINT RANGE
C ...PUT RANGE AND BLOCKAGE INTO PARAMS, THEN RETURN
               CTEMP = 'VMIN = '//CLIST(TABL-1:TABL+11)
               FIRST = 1
               CALL FSLEN(CTEMP,30,LAST)
               CALL RUPUTP(CTEMP,FIRST,LAST,RCODE)
               IF(RCODE.NE.0)RETURN
               CTEMP = 'VMAX = '//CLIST(TABG-1:TABG+11)
               FIRST = 1
               CALL FSLEN(CTEMP,30,LAST)
               CALL RUPUTP(CTEMP,FIRST,LAST,RCODE)
               IF(RCODE.NE.0)RETURN
               CTEMP = 'MINNAM = '//MINNAM
               FIRST = 1
               CALL FSLEN(CTEMP,30,LAST)
               CALL RUPUTP(CTEMP,FIRST,LAST,RCODE)
               IF(RCODE.NE.0)RETURN
               CTEMP = 'MAXNAM = '//MAXNAM
               FIRST = 1
               CALL FSLEN(CTEMP,30,LAST)
               CALL RUPUTP(CTEMP,FIRST,LAST,RCODE)
               RETURN
            ENDIF
C PRINT RANGE
            LAST = LUNDER
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
            IF( STIMP.EQ.'N' )THEN
C PRINT BLOCKING NAMES (RIGHT JUSTIFIED IN FIELD)
               CLIST(TABL-13:) = 'Blockage:'
               CALL FSLEN(MINNAM,NAMELN,LAST)
               IF( LAST.GT.12 )LAST=12
               CLIST(TABL+11-LAST:) = MINNAM
               CALL FSLEN(MAXNAM,NAMELN,LAST)
               IF( LAST.GT.12 )LAST=12
               CLIST(TABG+11-LAST:) = MAXNAM
               LAST = LUNDER
               CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
            ENDIF
C TEST FOR EARLY DEPARTURE FROM LOOP
C  (WE MAY HAVE DONE ALL IMPULSES IN SUBMATRIX)
990         IF( NRC.EQ.0 )GOTO 1010
C NO, SKIP LINE (UNLESS RANGE OPTION)
            IF( .NOT.SWRNG )
     1          CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C ...NEXT IMPULSE VARIABLE
1000  CONTINUE
C   ::: END OF MAIN LOOP OVER IMPULSES IN SUBMATRIX :::
C
1010  CONTINUE
C
9000  RETURN
C
C ** ARATE ENDS HERE
      END
