C               ::: SCRPLOTS.FOR 12-09-94 :::
C
C LAST DATE:  Previous entries deleted
C              7-25-93...Added SLABEL to SCRSTP (CHANGES ARGUMENTS)
C              8-14-93...Increased MARGIN (from 5 to 8) in SCRSTP
C                        Increased lengths of X/Y LABELs (from 8 to 16)
C              8-18-93...Purified YVALUE in SCRSTP before computing NEWY
C              8-20-93...Added line in SCRSTP (CC 12-9-94)
C
C This contains routines for the SCREEN/GRAPH module.
C
C       SCRBAR.....Plot a bar graph
C       SCRSTP.....Plot a step function
C       SCRAXS.....Draw axes
C
      SUBROUTINE SCRBAR(FORM,HEADER,XLABEL,YLABEL,NUMBER,LABEL,VALUE,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
C
C This plots a bar graph.
C
C              HEADER
C     YLABEL|
C           |
C           |
C           |
C           |   .-------.
C           |   | Value |
C           |   |       |
C           `---`-------'---------------------------------- XLABEL
C               LABEL(1)  ...
C
C SCREEN IS FIRST CLEARED
C ...Alternate return is error or out of screen range.
C
      CHARACTER*(*) FORM,HEADER,XLABEL,YLABEL
      INTEGER       NUMBER
      REAL          VALUE(1)
      CHARACTER*8   LABEL(1)
C LOCAL
      PARAMETER     (MAXBAR=20)
      CHARACTER*12  STRVAL(MAXBAR),STR12,STR12X
      CHARACTER*128 CLIST
      REAL          SCALE
      INTEGER       WIDTH
      CHARACTER*1   MOVE(5)
      INTEGER*2     AMOUNT(4)
C SET PATH MOVES TO DRAW BAR
      DATA          MOVE/'U','R','D','L',' '/
      DATA          AMOUNT(4)/1/
C AMOUNT(1)=AMOUNT(3)=BAR HEIGHT;  AMOUNT(2)=BAR WIDTH
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK NUMBER OF BARS
      IF(NUMBER.LE.0)RETURN
      IF(NUMBER.GT.MAXBAR)THEN
         PRINT *,' Sorry,',NUMBER,' are too many bars...Max=',MAXBAR
         RETURN 1
      ENDIF
C CHECK SCREEN SIZE
      IF(SCRWTH.LT.10 .OR. SCRLEN.LT.10)THEN
         PRINT *,' SCREEN TOO SMALL...MUST BE AT LEAST 10x10'
         RETURN 1
      ENDIF
C LOOP OVER BARS TO GET WIDTH AND HEIGHT
      BARWTH = 4
      HEIGHT = 0
      DO 100 N=1,NUMBER
         IF(VALUE(N).LT.0.)THEN
            PRINT *,' ** Negative bar height not allowed'
            RETURN 1
         ENDIF
         HT = VALUE(N)+.5
         IF(HT.GT.HEIGHT)HEIGHT = HT
C GET STRING OF VALUE
         CALL FR2C(STR12,VALUE(N))
         CALL FSLEN(STR12,12,L)
         IF(L.GT.BARWTH)THEN
C TRY TO REDUCE STRING WIDTH BY SHORTENING FRACTIONAL PART
            CALL FLOOK(STR12,1,L,'.',I)
            IF(I.GT.0)THEN
               STR12X = STR12(1:I-1)
               CALL FC2I(STR12X,12,MANTSA,RCODE)
               IF(RCODE.NE.0)RETURN 1
               IF(MANTSA.GT.10)THEN
                  L = I+1
               ELSE IF(MANTSA.GT.0)THEN
                  L = I+2
               ENDIF
               STR12X = STR12(1:L)
               STR12 = STR12X
               IF(L.GT.BARWTH)BARWTH=L
            ENDIF
         ENDIF
C NOW BAR WIDTH IS ENOUGH FOR VALUE...STORE INTO STRING (STRVAL)
         STRVAL(N) = STR12(:L)
C MAKE BAR WIDTH ENOUGH FOR LABEL
         CALL FSLEN(LABEL(N),8,L)
         IF(L.GT.BARWTH)BARWTH=L
100   CONTINUE
C NOW BARWTH = MAX WIDTH OF LABELs (BUT, AT LEAST 4)
C     HEIGHT = MAX VALUE
C
      SPACE = 1
      WIDTH = NUMBER*(BARWTH+1)
      HMAX  = SCRLEN-7
      IF(HEIGHT.GT.HMAX)THEN
         SCALE = FLOAT(HEIGHT)/FLOAT(HMAX)
         HEIGHT= HMAX
      ELSE
         SCALE = 1.
      ENDIF
      HEIGHT = HEIGHT+2
C                     :...FOR VALUE OF HIGHEST BAR
C INITIALIZE SCREEN
      CLIST = YLABEL
      CALL FSLEN(CLIST,8,L)
      TOTWTH = WIDTH+L+2
      CLIST = XLABEL
      CALL FSLEN(CLIST,8,L)
      TOTWTH = TOTWTH+L+2
      IF(TOTWTH.LT.SCRWTH)THEN
         WIDTH = WIDTH+1
         TOTWTH = TOTWTH+1
      ENDIF
      IF(TOTWTH+NUMBER.LT.SCRWTH)THEN
         WIDTH = WIDTH+NUMBER
         SPACE = 2
         TOTWTH = TOTWTH+NUMBER
      ENDIF
      TOTLEN = HEIGHT+3
      CALL SCRCLR(TOTWTH,TOTLEN,*1310)
C NOW SCREEN IS CLEARED AND DIMENSIONS ARE OK
C DRAW AXES AND PUT HEADER
      BEGPLT = 1
      CALL SCRAXS(FORM,BEGPLT,WIDTH,HEIGHT,HEADER,XLABEL,YLABEL,
     1                  ORIGNX,ORIGNY,*1390)
C PREPARE TO DRAW BARS
      BARX     = ORIGNX+SPACE
      AMOUNT(2)= BARWTH
C
C ::: LOOP TO PUT BARS :::
C
      DO 500 N=1,NUMBER
C PUT BAR LABEL
         CALL SCRTXT(LABEL(N),BARX+1,ORIGNY+1,*1390)
C COMPUTE BAR HEIGHT (SCALED)
         HEIGHT = VALUE(N)/SCALE + .5
         IF(HEIGHT.GT.0)THEN
C DRAW BAR AS BOX
            AMOUNT(1) = HEIGHT
            AMOUNT(3) = HEIGHT
            CALL SCRPTH(FORM,BARX,ORIGNY,MOVE,AMOUNT,*1390)
         ENDIF
C PUT BAR VALUE
         CALL SCRTXT(STRVAL(N),BARX,ORIGNY-HEIGHT-1,*1390)
C ADVANCE HORIZONTAL LOCATION (BARX) TO NEXT BAR
         BARX = BARX + BARWTH + SPACE
500   CONTINUE
C
C           PRINT SCREEN
C           ~~~~~~~~~~~~
      LINE = 0
      CALL SCRPRT(LINE,TOTLEN,*9000)
9000  RETURN
C
C SOME ERROR RETURNS
1310  PRINT *,' Sorry, graph too wide for screen display'
1390  RETURN 1
C
C ** SCRBAR ENDS HERE
      END
      SUBROUTINE SCRSTP(FORM,HEADER,XLABEL,YLABEL,NUMBER,
     1                  XVALUE,YVALUE,SLABEL,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
C
C This draws a step function with (NUMBER) of steps.
C
C              HEADER                SLABEL(3)
C     YLABEL|                       .----------.
C           |            SLABEL(2)  |          | SLABEL(4)
C           |           .-----------'          `---------
C           |SLABEL(1)  |
C     YVALUE|-----------'
C           |
C           |
C           `---------------------------------------------- XLABEL
C                  XVALUE
C
C SCREEN IS FIRST CLEARED
C ...Alternate return is error or out of screen range.
C
      INTEGER       NUMBER
      CHARACTER*(*) FORM,HEADER,XLABEL,YLABEL,SLABEL(NUMBER)
      REAL          XVALUE(NUMBER),YVALUE(NUMBER)
C LOCAL
      LOGICAL*1     SWINF
      CHARACTER*16  STR16
      CHARACTER*12  STR12,STR122
      PARAMETER     (MXSTEP=30)
C                           :...MAX NUMBER OF STEPS IN PLOT
      CHARACTER*1   MOVE(2*MXSTEP+1)
      INTEGER*2     AMOUNT(2*MXSTEP)
      INTEGER       WIDTH,HEIGHT
      REAL          XRANGE,YRANGE,XSCALE,YSCALE
      PARAMETER     (VINF=1.0E+10, VZERO=1.0E-06)
C                         :              :...USED FOR MIN SCALES
C                         :...INFINITY FOR WIDTH OR HEIGHT OF STEP
C THE FOLLOWING ARE NEEDED BECAUSE I CANNOT FIND BUG IN SCRPTH
      INTEGER       HOR(MXSTEP),VER(MXSTEP)
      CHARACTER*1   NWCORN(2),SWCORN(2)
      DATA  NWCORN /   'Ú',       '.' /
      DATA  SWCORN /   'À',       '`' /
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK NUMBER OF STEPS
      IF( NUMBER.LE.0 )RETURN
      IF( NUMBER.GT.MXSTEP )THEN
         PRINT *,' Sorry,',NUMBER,' are too many steps...Max=',
     1           MXSTEP
         RETURN 1
      ENDIF
      MIN = 6*NUMBER
      IF( MIN.GT.72 )MIN=72
      IF( SCRWTH.LT.MIN )THEN
         PRINT *,' Sorry, screen width must be at least',MIN
         RETURN 1
      ENDIF
      MIN = 6*NUMBER
      IF( MIN.GT.12 )MIN=12
      IF( SCRLEN.LT.MIN )THEN
         PRINT *,' Sorry, screen length must be at least',MIN
         RETURN 1
      ENDIF
C
      SWINF = XVALUE(NUMBER).GE.VINF
      IF( SWINF )XVALUE(NUMBER) = 0.
      XRANGE = 0.
      YRANGE = 0.
      XMAX   = XVALUE(1)
      YMIN   = YVALUE(1)
C LOOP OVER STEPS TO GET TOTAL WIDTH AND HEIGHT
      DO 100 N=1,NUMBER
         IF( XVALUE(N).LT.0. .OR. YVALUE(N).LT.0. )THEN
            PRINT *,' ** NEGATIVE STEP WIDTH/HEIGHT NOT ALLOWED'
            PRINT *,' ...STEP',N,' HAS X,Y =',XVALUE(N),YVALUE(N)
            RETURN 1
         ENDIF
         IF( YVALUE(N).GE.VINF )THEN
            PRINT *,' ** CANNOT HAVE STEP WITH INFINITE HEIGHT'
            PRINT *,' ...STEP',N,' HAS Y =',YVALUE(N)
            RETURN 1
         ENDIF
         IF( XVALUE(N).GE.VINF )THEN
            PRINT *,' ** CANNOT HAVE STEP WITH INFINITE WIDTH,',
     1              ' EXCEPT LAST'
            PRINT *,' ...STEP',N,' HAS X =',XVALUE(N)
            RETURN 1
         ENDIF
         XRANGE = XRANGE + XVALUE(N)
         IF( XVALUE(N).GT.XMAX   ) XMAX   = XVALUE(N)
         IF( YVALUE(N).GT.YRANGE ) YRANGE = YVALUE(N)
         IF( YVALUE(N).LT.YMIN   ) YMIN   = YVALUE(N)
100   CONTINUE
C
      IF( SWINF )THEN
C LAST STEP HAS INFINITE WIDTH
         IF( XMAX.LE.0. )THEN
C ...ALL STEPS HAVE 0 WIDTH (MAYBE NO OTHER STEP)...SET THIS ONE = 1.
            XVALUE(NUMBER) = 1.
            XRANGE = 1.
         ELSE
C ...SET WIDTH = MAX OF OTHER STEPS
            XVALUE(NUMBER) = XMAX
            XRANGE = XRANGE + XMAX
         ENDIF
      ENDIF
C
C SET WIDTH OF X-AXIS
      TOTWTH = SCRWTH-2
      TOTLEN = SCRLEN-2
      WIDTH = TOTWTH - 3
      STR16 = YLABEL
      CALL FSLEN(STR16,16,MARGIN)
      IF( MARGIN.LT.8 )THEN
C MOVE YLABEL TO RIGHT TO LEAVE MARGIN FOR YVALUE
         YLABEL = ' '
         YLABEL(9-MARGIN:) = STR16
         MARGIN = 8
      ENDIF
      WIDTH = WIDTH - MARGIN
      STR16 = XLABEL
      CALL FSLEN(STR16,16,L)
      WIDTH = WIDTH - L - 1
C NOW WIDTH = TOTAL SCREEN WIDTH MINUS SPACE FOR LABELS AND AXIS
C COMPUTE SCALES
      XSCALE = XRANGE/FLOAT(WIDTH)
      IF( XSCALE.LT.VZERO )XSCALE = VZERO
      HEIGHT = TOTLEN-6
      YRANGE = YRANGE - YMIN
      YSCALE = YRANGE/FLOAT(HEIGHT)
      IF( YSCALE.LT.VZERO )YSCALE = VZERO
      HEIGHT = HEIGHT+2
C
C NOW   XVALUE / XSCALE       = STEP WIDTH  ON SCREEN
C    (YVALUE-YMIN)/YSCALE + 1 = STEP HEIGHT ON SCREEN
C INITIALIZE SCREEN
      CALL SCRCLR(TOTWTH,TOTLEN,*1310)
C DRAW AXES AND PUT HEADER
      BEGPLT = 1
      CALL SCRAXS(FORM,BEGPLT,WIDTH,HEIGHT,HEADER,XLABEL,YLABEL,
     1                  ORIGNX,ORIGNY,*1390)
C
C  ::: LOOP OVER STEPS TO ESTABLISH PATH :::
C
      M = 0
      OLDY = 1
      LOCX = ORIGNX
      XSUM = 0.
      LASTX= 1
C
      DO 500 N=1,NUMBER
         M = M+1
C CONVERT YVALUE TO STRING
         CALL FR2CRJ(STR122,YVALUE(N),F)
         STR12 = STR122(F:)
C CONVERT STRING BACK TO Y (PURIFIES)
         CALL FC2R(STR12,Y,RCODE)
         NEWY = (Y-YMIN)/YSCALE + 1.
C PUT YVALUE
         CALL FSLEN(STR12,12,L)
         IF( L.GT.MARGIN )THEN
            STR122 = '>'//STR12(:MARGIN-1)
CC 12-9-94
            L = MARGIN
         ELSE
            STR122 = STR12(:L)
         ENDIF
         CALL SCRTXT(STR122,MARGIN-L+1,ORIGNY-NEWY,*1390)
C ENTER MOVES
         IF( NEWY.GT.OLDY )THEN
            MOVE(M) = 'U'
            AMOUNT(M) = NEWY-OLDY
         ELSE
            MOVE(M) = 'D'
            AMOUNT(M) = OLDY-NEWY
         ENDIF
         OLDY = NEWY
         M = M+1
         MOVE(M) = 'R'
         AMOUNT(M) = XVALUE(N)/XSCALE + .5
C RECORD LOCATION OF STEP
         VER(N) = ORIGNY-NEWY
         HOR(N) = LOCX
C ADVANCE X-LOCATION
         LOCX = LOCX + AMOUNT(M)
C PUT XVALUE
         XSUM = XSUM + XVALUE(N)
         CALL FR2CRJ(STR122,XSUM,F)
         STR12 = STR122(F:)
         CALL FSLEN(STR12,12,L)
C CHECK THAT STEP WIDTH IS WIDE ENOUGH FOR VALUE
         BEGX = LOCX-L
         IF( BEGX.GT.LASTX .AND. N.LT.NUMBER )THEN
            CALL SCRTXT(STR12,BEGX,ORIGNY+1,*1390)
            LASTX = LOCX
         ELSE IF( N.EQ.NUMBER )THEN
            IF( SWINF )THEN
               BEGX = LOCX
               STR12 = '*'
            ENDIF
            CALL SCRTXT(STR12,BEGX+1,ORIGNY+1,*1390)
         ENDIF
500   CONTINUE
C
      MOVE(M+1) = ' '
      CALL SCRPTH(FORM,ORIGNX,ORIGNY-1,MOVE,AMOUNT,*1390)
C HERE IS WHERE I MUST PUT CORNERS IN DUE TO BUG I CANNOT FIND
      IF(FORM.EQ.'GRAPHIC')THEN
         K = 1
      ELSE
         K = 2
      ENDIF
      M = 3
      DO 700 N=2,NUMBER
         IF(MOVE(M).EQ.'U')THEN
            CALL SCRTXT(NWCORN(K),HOR(N),VER(N),*1390)
         ELSE
            CALL SCRTXT(SWCORN(K),HOR(N),VER(N),*1390)
         ENDIF
         M = M+2
700   CONTINUE
C
C PUT SLABEL (TRUNCATED TO FIT STEP WIDTH)
      DO 800 N=1,NUMBER
         STR16 = SLABEL(N)
         IF( STR16.EQ.' ' )GOTO 800
C TRUNCATE TO FIT STEP WIDTH WITHOUT GOING INTO NEXT STEP
         IF( N.LT.NUMBER )THEN
            SWIDTH = HOR(N+1) - HOR(N)
         ELSE
            SWIDTH = SCRWTH - HOR(N) - 1
         ENDIF
         IF( SWIDTH.LE.16 )STR16(SWIDTH:) = ' '
         CALL SCRTXT(STR16,HOR(N)+1,VER(N)-1,*1390)
800   CONTINUE
C
C           PRINT SCREEN
C           ~~~~~~~~~~~~
      LINE = 0
      CALL SCRPRT(LINE,TOTLEN,*9000)
9000  RETURN
C
C SOME ERROR RETURNS
1310  PRINT *,' Sorry, graph too wide for screen display'
1390  RETURN 1
C
C ** SCRSTP ENDS HERE
      END
      SUBROUTINE SCRAXS(FORM,BEGPLT,WIDTH,HEIGHT,HEADER,XLABEL,YLABEL,
     1                  ORIGNX,ORIGNY,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This draws axes.
C            HEADER (begins after y-axis)
C     YLABEL|                              HEIGHT
C           |                                |
C           |                                |
C           |                                |
C           |                                |
C           |                                |
C           |                                |
C           `---------------------------------------------- XLABEL
C           <--------------- WIDTH ----------------------->
C
C   ORIGIN RETURNED (ORIGNX, ORIGNY)
C   BEGPLT IS INCREMENTED (BY 1) IF HEADER
C   WIDTH IS INCREASED BY LENGTHS OF YLABEL AND XLABEL
C ...Alternate return screen error.
C
      CHARACTER*(*) FORM,HEADER,XLABEL,YLABEL
      INTEGER       BEGPLT,WIDTH,HEIGHT,ORIGNX,ORIGNY
C LOCAL
      CHARACTER*128 CLIST
      CHARACTER*12  STR12
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C SET LABEL AND MARGIN FOR Y-AXIS
      IF(YLABEL.EQ.' ')THEN
         ORIGNX = 1
      ELSE
         STR12 = YLABEL
         CALL FSLEN(STR12,12,L)
         ORIGNX = L+1
      ENDIF
      IF(HEADER.NE.' ')THEN
C PUT HEADER
         CLIST = HEADER
         CALL SCRTXT(CLIST,ORIGNX+1,BEGPLT,*1390)
         BEGPLT = BEGPLT+1
      ENDIF
      ORIGNY = BEGPLT + HEIGHT
C DRAW AXES
      CALL SCRLIN(FORM,ORIGNX,BEGPLT,ORIGNX+WIDTH,ORIGNY,*1390)
C PUT AXIS LABELS AND INCREASE WIDTH
      IF(YLABEL.NE.' ')THEN
         CALL SCRTXT(YLABEL, 1,BEGPLT, *1390)
         WIDTH = WIDTH+ORIGNX
      ENDIF
      IF(XLABEL.NE.' ')THEN
         CALL SCRTXT(XLABEL, WIDTH+2,ORIGNY, *1390)
         STR12 = XLABEL
         CALL FSLEN(STR12,12,L)
         WIDTH = WIDTH+L+1
      ENDIF
C
      RETURN
C
1390  RETURN 1
C
C ** SCRAXS ENDS HERE
      END
