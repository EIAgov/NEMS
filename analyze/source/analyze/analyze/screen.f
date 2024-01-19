C                 ::: SCREEN.FOR  3-09-93 :::
C
C LAST DATE: 10-20-91...INCREASED NUMBER OF MOVES IN SCRPTH
C            12-19-91...Removed SWFDBG
C             6-18-92...Corrected bug in SCRPRT (VER)
C             7-11-92...Corrected "   "  SCRTXT (VER)
C
C SCREEN MANAGEMENT ROUTINES:
C
C     SCRCLR.....clears screen and sets width and length
C     SCRBOX.....draws box
C     SCRLIN.....draws line
C     SCRPTH.....draws path
C     SCRARC.....puts arc
C     SCRARR.....puts arrow head (left or right)
C     SCRTXT.....puts text
C     SCRFIL.....fills area with char
C     SCRDEL.....deletes (erases) portion of screen
C     SCRLEN.....determines screen length
C     SCRPRT.....prints screen (uses FTEXT)
C
      SUBROUTINE SCRCLR(W,L,*)
C     =================
      INCLUDE 'DCSCREEN.'
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCSCREEN)
CITSO      INCLUDE (DCFLIP)
C
C This clears screen and sets width and length.
C
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK DIMENSIONS
      IF(W.LT.1 .OR. W.GT.MAXWTH .OR. W.GT.SCRWTH .OR.
     1   L.LT.1 .OR. L.GT.MAXLEN .OR. L.GT.SCRLEN)THEN
         PRINT *,' ** SCREEN DIMENSIONS OUT OF RANGE',W,L
         RETURN 1
      ENDIF
      WIDTH = W
      LENGTH= L
      DO 10 I=0,W+1
      DO 10 J=0,L+1
10    SCREEN(I,J) = ' '
C
      RETURN
C
C ** SCRCLR ENDS HERE
      END
      SUBROUTINE SCRBOX(FORM,NWHOR,NWVER,SEHOR,SEVER,*)
C     =================
      INCLUDE 'DCSCREEN.'
CITSO      INCLUDE (DCSCREEN)
C
C This draws box from NW corner to SE corner.
C
C FORM = GRAPHIC:
C
C              NWHOR,NWVER      SEHOR,NWVER
C                     ÚÄÄÄÄÄÄÄÄÄÄ¿
C                     ³          ³
C                     ³          ³
C                     ÀÄÄÄÄÄÄÄÄÄÄÙ
C              NWHOR,SEVER      SEHOR,SEVER
C
C
C FORM = PRINTABL:
C
C              NWHOR,NWVER      SEHOR,NWVER
C                     .----------.
C                     |          |
C                     |          |
C                     `----------`
C              NWHOR,SEVER      SEHOR,SEVER
C
C ...Alternate return is error.
      CHARACTER*(*) FORM
      INCLUDE 'DCSCRDAT.'
CITSO      INCLUDE (DCSCRDAT)
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK COORDINATES
      IF(NWHOR.LT.1.OR.NWVER.LT.1.OR.SEHOR.LT.1.OR.SEVER.LT.1.OR.
     1   NWHOR.GT.WIDTH.OR.SEHOR.GT.WIDTH.OR.
     2   NWVER.GT.LENGTH.OR.SEVER.GT.LENGTH.OR.
     3   NWHOR.GE.SEHOR-1.OR.NWVER.GE.SEVER-1)THEN
            PRINT *,' ** SYSERR SCREEN BOX...NW:',NWHOR,NWVER
            PRINT *,'                        SE:',SEHOR,SEVER
         IF(NWHOR.GT.WIDTH .OR. SEHOR.GT.WIDTH)
     1      PRINT *,' ...WIDTH=',WIDTH
         IF(NWVER.GT.LENGTH.OR. SEVER.GT.LENGTH)
     1      PRINT *,' ...LENGTH=',LENGTH
         RETURN 1
      ENDIF
C SET FORM
      IF(FORM.EQ.'GRAPHIC')THEN
         LINTYP = 1
      ELSE IF(FORM.EQ.'PRINTABL')THEN
         LINTYP = 2
      ELSE
         PRINT *,' ** SYSERR...SCREEN FORM ',FORM,'?'
         RETURN 1
      ENDIF
C SET CORNERS
      SCREEN(NWHOR,NWVER) = NWCORN(LINTYP)
      SCREEN(NWHOR,SEVER) = SWCORN(LINTYP)
      SCREEN(SEHOR,NWVER) = NECORN(LINTYP)
      SCREEN(SEHOR,SEVER) = SECORN(LINTYP)
C SET HORIZONTAL LINES
      DO 100 I=NWHOR+1,SEHOR-1
         SCREEN(I,NWVER) = HORLIN(LINTYP)
         SCREEN(I,SEVER) = HORLIN(LINTYP)
100   CONTINUE
C SET VERTICAL LINES
      DO 200 I=NWVER+1,SEVER-1
         SCREEN(NWHOR,I) = VERLIN(LINTYP)
         SCREEN(SEHOR,I) = VERLIN(LINTYP)
200   CONTINUE
C
      RETURN
C
C ** SCRBOX ENDS HERE
      END
      SUBROUTINE SCRLIN(FORM,HOR1,VER1,HOR2,VER2,*)
C     =================
      INCLUDE 'DCSCREEN.'
CITSO      INCLUDE (DCSCREEN)
C
C This draws line from (HOR1,VER1) to (HOR2,VER2) (note no diagonals)
C
C Case 1. HOR1,VER1
C              ³
C              ³
C              ³
C         HOR2=HOR1,VER2
C Case 2. HOR1,VER1 ÄÄÄÄÄÄÄÄÄÄ  HOR2,VER2=VER1
C Case 3. HOR1,VER1
C              ³
C              ³
C              ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄ  HOR2,VER2
C Case 4. HOR2,VER2 ÄÄÄÄÄÄÄÄÄÄ¿
C                             ³
C                             ³
C                               HOR1,VER1
C Case 5.      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ  HOR1,VER1
C              ³
C              ³
C        HOR2,VER2
C Case 6.                       HOR2,VER2
C                                ³
C                                ³
C        HOR1,VER1 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
C
C ...Alternate return is error.
C
      CHARACTER*(*) FORM
      INTEGER HOR1,VER1,HOR2,VER2
C
      INCLUDE 'DCSCRDAT.'
CITSO      INCLUDE (DCSCRDAT)
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK COORDINATES
      IF(HOR1.LE.0.OR.HOR2.LE.0 .OR. VER1.LE.0.OR.VER2.LE.0 .OR.
     1   HOR1.GT.WIDTH  .OR. HOR2.GT.WIDTH .OR.
     2   VER1.GT.LENGTH .OR. VER2.GT.LENGTH)THEN
            PRINT *,' ** SYSERR...SCREEN LINE ERROR...HOR1,VER1:',
     1              HOR1,VER1
            PRINT *,'                                 HOR2,VER2:',
     1              HOR2,VER2
         IF(HOR1.GT.WIDTH .OR. HOR2.GT.WIDTH)
     1      PRINT *,' ...WIDTH =',WIDTH
         IF(VER1.GT.LENGTH.OR. VER2.GT.LENGTH)
     1      PRINT *,' ...LENGTH =',LENGTH
         RETURN 1
      ENDIF
C SET FORM
      IF(FORM.EQ.'GRAPHIC')THEN
         LINTYP = 1
      ELSE IF(FORM.EQ.'PRINTABL')THEN
         LINTYP = 2
      ELSE
         PRINT *,' ** SYSERR...SCREEN FORM ',FORM,'?'
         RETURN 1
      ENDIF
C
      IF(HOR1.EQ.HOR2)THEN
C CASE 1
         IF(VER1.LT.VER2)THEN
            DO 100 I=VER1,VER2
100         SCREEN(HOR1,I) = VERLIN(LINTYP)
         ELSE
            DO 110 I=VER2,VER1
110         SCREEN(HOR1,I) = VERLIN(LINTYP)
         ENDIF
      ELSE IF(VER1.EQ.VER2)THEN
C CASE 2
         IF(HOR1.LT.HOR2)THEN
            DO 200 I=HOR1,HOR2
200         SCREEN(I,VER1) = HORLIN(LINTYP)
         ELSE
            DO 210 I=HOR2,HOR1
210         SCREEN(I,VER1) = HORLIN(LINTYP)
         ENDIF
      ELSE IF(HOR1.LT.HOR2 .AND. VER1.LT.VER2)THEN
C CASE 3
         DO 300 I=VER1,VER2
300      SCREEN(HOR1,I) = VERLIN(LINTYP)
         DO 310 I=HOR1,HOR2
310      SCREEN(I,VER2) = HORLIN(LINTYP)
         SCREEN(HOR1,VER2) = SWCORN(LINTYP)
      ELSE IF(HOR1.GT.HOR2 .AND. VER1.GT.VER2)THEN
C CASE 4
         DO 400 I=VER2,VER1
400      SCREEN(HOR1,I) = VERLIN(LINTYP)
         DO 410 I=HOR2,HOR1
410      SCREEN(I,VER2) = HORLIN(LINTYP)
         SCREEN(HOR1,VER2) = NECORN(LINTYP)
      ELSE IF(HOR1.GT.HOR2 .AND. VER1.LT.VER2)THEN
C CASE 5
         DO 500 I=VER1,VER2
500      SCREEN(HOR1,I) = VERLIN(LINTYP)
         DO 510 I=HOR2,HOR1
510      SCREEN(I,VER2) = HORLIN(LINTYP)
         SCREEN(HOR1,VER2) = SECORN(LINTYP)
      ELSE IF(HOR1.LT.HOR2 .AND. VER1.GT.VER2)THEN
C CASE 6
         DO 600 I=VER2,VER1
600      SCREEN(HOR1,I) = VERLIN(LINTYP)
         DO 610 I=HOR1,HOR2
610      SCREEN(I,VER2) = HORLIN(LINTYP)
         SCREEN(HOR1,VER2) = NWCORN(LINTYP)
      ENDIF
C
      RETURN
C
C ** SCRLIN ENDS HERE
      END
      SUBROUTINE SCRPTH(FORM,HOR,VER,MOVE,AMOUNT,*)
C     =================
      INCLUDE 'DCSCREEN.'
CITSO      INCLUDE (DCSCREEN)
C
C This draws path from (HOR,VER) according to moves:
C       MOVE(i) = RIGHT | LEFT | UP | DOWN
C       AMOUNT(i) = integer number of spaces to move
C Path terminates with MOVE(i) = blank
C
C ...Alternate return is error.
C
      CHARACTER*(*) FORM,MOVE(2)
      INTEGER       HOR,VER
      INTEGER*2     AMOUNT(2)
C LOCAL
      PARAMETER     (MAXPTH=50)
      INTEGER*2   HORLST(MAXPTH),VERLST(MAXPTH),HOR1,VER1,HOR2,VER2
      CHARACTER*1 DIREC,DIROLD,CHRLFT,CHRGHT,CHRUP,CHRDN,CHAR
      LOGICAL*1   SWLFT,SWRGT,SWUP,SWDN
C
      INCLUDE 'DCSCRDAT.'
CITSO      INCLUDE (DCSCRDAT)
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C SET FORM
      IF(FORM.EQ.'GRAPHIC')THEN
         LINTYP = 1
      ELSE IF(FORM.EQ.'PRINTABL')THEN
         LINTYP = 2
      ELSE
         PRINT *,' ** SYSERR...SCREEN PATH FORM ',FORM,'?'
         RETURN 1
      ENDIF
C INITIALIZE POSITION
      H = HOR
      V = VER
      DIROLD = ' '
      NUMBER = 1
C DRAW LINES
1000  CONTINUE
         IF(MOVE(NUMBER).EQ.' ')GOTO 1100
         DIREC = MOVE(NUMBER)(1:1)
         AMT = AMOUNT(NUMBER)
         IF(DIREC.EQ.'R')THEN
C RIGHT
            HNEW = H + AMT
            VNEW = V
            IF(HNEW.GT.WIDTH)HNEW = WIDTH
            DO 100 J=H,HNEW
               CHAR = SCREEN(J,V)
               IF(CHAR.EQ.NECORN(LINTYP).OR.
     1            CHAR.EQ.NWCORN(LINTYP))THEN
C    TEE DOWN
                  SCREEN(J,V) = TEEDN(LINTYP)
               ELSE IF(CHAR.EQ.SECORN(LINTYP).OR.
     1                 CHAR.EQ.SWCORN(LINTYP))THEN
C    TEE UP
                  SCREEN(J,V) = TEEUP(LINTYP)
               ELSE IF(CHAR.NE.TEEDN(LINTYP).AND.
     1                 CHAR.NE.TEEUP(LINTYP))THEN
C    HORIZONTAL LINE
                  SCREEN(J,V) = HORLIN(LINTYP)
               ENDIF
100         CONTINUE
C    PUT CORNER (IF COMING FROM VERTICAL DIRECTION)
            IF(DIROLD.EQ.'U')THEN
C       *               *              (IF * NOT |)
C       -----   becomes .-----
C       |               |
               IF(SCREEN(H,V-1).NE.VERLIN(LINTYP))
     1            SCREEN(H,V) = NWCORN(LINTYP)
            ENDIF
            IF(DIROLD.EQ.'D')THEN
C       |               |
C       -----   becomes `-----
C       *               *               (IF * NOT |)
               IF(SCREEN(H,V+1).NE.VERLIN(LINTYP))
     1            SCREEN(H,V) = SWCORN(LINTYP)
            ENDIF
         ELSE IF(DIREC.EQ.'L')THEN
C LEFT
            HNEW = H - AMT
            IF(HNEW.LT.1)HNEW = 1
            VNEW = V
            DO 200 J=HNEW,H
               CHAR = SCREEN(J,V)
               IF(CHAR.EQ.NECORN(LINTYP).OR.
     1            CHAR.EQ.NWCORN(LINTYP))THEN
C    TEE DOWN
                  SCREEN(J,V) = TEEDN(LINTYP)
               ELSE IF(CHAR.EQ.SECORN(LINTYP).OR.
     1                 CHAR.EQ.SWCORN(LINTYP))THEN
C    TEE UP
                  SCREEN(J,V) = TEEUP(LINTYP)
               ELSE IF(CHAR.NE.TEEDN(LINTYP).AND.
     1                 CHAR.NE.TEEUP(LINTYP))THEN
C    HORIZONTAL LINE
                  SCREEN(J,V) = HORLIN(LINTYP)
               ENDIF
200         CONTINUE
C    PUT CORNER (IF COMING FROM VERTICAL DIRECTION)
            IF(DIROLD.EQ.'U')THEN
               IF(SCREEN(H,V-1).NE.VERLIN(LINTYP))
     1            SCREEN(H,V) = NECORN(LINTYP)
            ENDIF
            IF(DIROLD.EQ.'D')THEN
               IF(SCREEN(H,V+1).NE.VERLIN(LINTYP))
     1            SCREEN(H,V) = SECORN(LINTYP)
            ENDIF
         ELSE IF(DIREC.EQ.'U')THEN
C UP
            HNEW = H
            VNEW = V - AMT
            IF(VNEW.LT.1)VNEW = 1
            DO 300 J=VNEW,V
               CHAR = SCREEN(H,J)
               IF(CHAR.EQ.SWCORN(LINTYP).OR.
     1            CHAR.EQ.NWCORN(LINTYP))THEN
C    TEE RIGHT
                  SCREEN(H,J) = TEERGT(LINTYP)
               ELSE IF(CHAR.EQ.SECORN(LINTYP).OR.
     1                 CHAR.EQ.NECORN(LINTYP))THEN
C    TEE LEFT
                  SCREEN(H,J) = TEELFT(LINTYP)
               ELSE IF(CHAR.NE.TEERGT(LINTYP).AND.
     1                 CHAR.NE.TEELFT(LINTYP))THEN
C    VERTICAL LINE
                  SCREEN(H,J) = VERLIN(LINTYP)
               ENDIF
300         CONTINUE
C    PUT CORNER (IF COMING FROM HORIZONTAL DIRECTION)
            IF(DIROLD.EQ.'R')THEN
               IF(SCREEN(H+1,V).NE.HORLIN(LINTYP))
     1            SCREEN(H,V) = SECORN(LINTYP)
            ENDIF
            IF(DIROLD.EQ.'L')THEN
               IF(SCREEN(H-1,V).NE.HORLIN(LINTYP))
     1            SCREEN(H,V) = SWCORN(LINTYP)
            ENDIF
         ELSE IF(DIREC.EQ.'D')THEN
C DOWN
            HNEW = H
            VNEW = V + AMT
            IF(VNEW.GT.LENGTH)VNEW = LENGTH
            DO 400 J=V,VNEW
               CHAR = SCREEN(H,J)
               IF(CHAR.EQ.SWCORN(LINTYP).OR.
     1            CHAR.EQ.NWCORN(LINTYP))THEN
C    TEE RIGHT
                  SCREEN(H,J) = TEERGT(LINTYP)
               ELSE IF(CHAR.EQ.SECORN(LINTYP).OR.
     1                 CHAR.EQ.NECORN(LINTYP))THEN
C    TEE LEFT
                  SCREEN(H,J) = TEELFT(LINTYP)
               ELSE IF(CHAR.NE.TEERGT(LINTYP).AND.
     1                 CHAR.NE.TEELFT(LINTYP))THEN
C    VERTICAL LINE
                  SCREEN(H,J) = VERLIN(LINTYP)
               ENDIF
400         CONTINUE
C    PUT CORNER (IF COMING FROM HORIZONTAL DIRECTION)
            IF(DIROLD.EQ.'R')THEN
               IF(SCREEN(H+1,V).NE.HORLIN(LINTYP))
     1            SCREEN(H,V) = NECORN(LINTYP)
            ENDIF
            IF(DIROLD.EQ.'L')THEN
               IF(SCREEN(H-1,V).NE.HORLIN(LINTYP))
     1            SCREEN(H,V) = NWCORN(LINTYP)
            ENDIF
         ELSE
            PRINT *,' ** SYSERR...SCREEN PATH MOVE ',MOVE(NUMBER),'?'
            RETURN 1
         ENDIF
900      DIROLD = DIREC
         HORLST(NUMBER) = H
         VERLST(NUMBER) = V
         H = HNEW
         V = VNEW
         IF(NUMBER.GE.MAXPTH)THEN
            PRINT *,' ** SYSERR',NUMBER,' ARE TOO MANY MOVES IN PATH'
            RETURN 1
         ENDIF
         NUMBER = NUMBER+1
      GOTO 1000
C
C END OF MOVES
1100  CONTINUE
      IF(NUMBER.LE.1)RETURN
      HORLST(NUMBER) = H
      VERLST(NUMBER) = V
C
C   === DRAW CROSSES AND TEES ===
C
      HOR1 = HORLST(1)
      VER1 = VERLST(1)
C ::: LOOP OVER MOVES :::
      DO 1500 I=2,NUMBER
         HOR2 = HORLST(I)
         VER2 = VERLST(I)
C FIRST, THE CORNER (c)
C  |                      |                       |
C -c-  ===> CROSS         c-  ===> TEERGT        -c   ===> TEELFT
C  |                      |                       |
C
C                         |
C -c-  ===> TEEDN        -c-  ===> TEEUP
C  |
C
         SWLFT = SCREEN(HOR1-1,VER1).EQ.HORLIN(LINTYP).OR.
     L           SCREEN(HOR1-1,VER1).EQ.TEERGT(LINTYP).OR.
     L           SCREEN(HOR1-1,VER1).EQ.TEEUP (LINTYP).OR.
     L           SCREEN(HOR1-1,VER1).EQ.TEEDN (LINTYP).OR.
     L           SCREEN(HOR1-1,VER1).EQ.SWCORN(LINTYP).OR.
     L           SCREEN(HOR1-1,VER1).EQ.NWCORN(LINTYP).OR.
     L           SCREEN(HOR1-1,VER1).EQ.CROSS (LINTYP)
         SWRGT = SCREEN(HOR1+1,VER1).EQ.HORLIN(LINTYP).OR.
     R           SCREEN(HOR1+1,VER1).EQ.TEELFT(LINTYP).OR.
     R           SCREEN(HOR1+1,VER1).EQ.TEEUP (LINTYP).OR.
     R           SCREEN(HOR1+1,VER1).EQ.TEEDN (LINTYP).OR.
     R           SCREEN(HOR1+1,VER1).EQ.SECORN(LINTYP).OR.
     R           SCREEN(HOR1+1,VER1).EQ.NECORN(LINTYP).OR.
     R           SCREEN(HOR1+1,VER1).EQ.CROSS (LINTYP)
         SWUP  = SCREEN(HOR1,VER1-1).EQ.VERLIN(LINTYP).OR.
     U           SCREEN(HOR1,VER1-1).EQ.TEEDN (LINTYP).OR.
     U           SCREEN(HOR1,VER1-1).EQ.TEERGT(LINTYP).OR.
     U           SCREEN(HOR1,VER1-1).EQ.TEELFT(LINTYP).OR.
     U           SCREEN(HOR1,VER1-1).EQ.NECORN(LINTYP).OR.
     U           SCREEN(HOR1,VER1-1).EQ.NWCORN(LINTYP).OR.
     U           SCREEN(HOR1,VER1-1).EQ.CROSS (LINTYP)
         SWDN  = SCREEN(HOR1,VER1+1).EQ.VERLIN(LINTYP).OR.
     D           SCREEN(HOR1,VER1+1).EQ.TEEUP (LINTYP).OR.
     D           SCREEN(HOR1,VER1+1).EQ.TEERGT(LINTYP).OR.
     D           SCREEN(HOR1,VER1+1).EQ.TEELFT(LINTYP).OR.
     D           SCREEN(HOR1,VER1+1).EQ.SECORN(LINTYP).OR.
     D           SCREEN(HOR1,VER1+1).EQ.SWCORN(LINTYP).OR.
     D           SCREEN(HOR1,VER1+1).EQ.CROSS (LINTYP)
         C = 0
         IF(SWLFT)C = 1
         IF(SWRGT)C = C+1
         IF(SWUP )C = C+1
         IF(SWDN )C = C+1
         IF(C.EQ.3)THEN
            IF(.NOT.SWLFT)SCREEN(HOR1,VER1) = TEERGT(LINTYP)
            IF(.NOT.SWRGT)SCREEN(HOR1,VER1) = TEELFT(LINTYP)
            IF(.NOT.SWUP )SCREEN(HOR1,VER1) = TEEDN(LINTYP)
            IF(.NOT.SWDN )SCREEN(HOR1,VER1) = TEEUP(LINTYP)
         ELSE IF(C.EQ.4)THEN
            SCREEN(HOR1,VER1) = CROSS(LINTYP)
         ENDIF
C
         IF(HOR1.EQ.HOR2)THEN
C WE MOVED VERTICALLY, SO LOOK FOR HORIZONTAL CROSSINGS
            IF(VER1.LT.VER2-1)THEN
               J1 = VER1+1
               J2 = VER2-1
            ELSE IF(VER2.LT.VER1-1)THEN
               J1 = VER2+1
               J2 = VER1-1
            ELSE
               GOTO 1490
            ENDIF
            DO 1200 J=J1,J2
               CHRGHT = SCREEN(HOR1+1,J)
               CHRLFT = SCREEN(HOR1-1,J)
               IF( (CHRGHT.EQ.HORLIN(LINTYP).OR.
     R              CHRGHT.EQ.CROSS(LINTYP) .OR.
     R              CHRGHT.EQ.SECORN(LINTYP).OR.
     R              CHRGHT.EQ.NECORN(LINTYP).OR.
     R              CHRGHT.EQ.TEELFT(LINTYP)   ).AND.
     L             (CHRLFT.EQ.HORLIN(LINTYP).OR.
     L              CHRLFT.EQ.CROSS(LINTYP) .OR.
     L              CHRLFT.EQ.SWCORN(LINTYP).OR.
     L              CHRLFT.EQ.NWCORN(LINTYP).OR.
     L              CHRLFT.EQ.TEERGT(LINTYP)   ) )THEN
C
C WE HAVE *|* WHERE * IS -, +, A CORNER OR A TEE
                  CHRUP = SCREEN(HOR1,J-1)
                  CHRDN = SCREEN(HOR1,J+1)
                  IF( (CHRUP.EQ.VERLIN(LINTYP).OR.
     U                 CHRUP.EQ.CROSS(LINTYP) .OR.
     U                 CHRUP.EQ.SECORN(LINTYP).OR.
     U                 CHRUP.EQ.SWCORN(LINTYP).OR.
     U                 CHRUP.EQ.NECORN(LINTYP).OR.
     U                 CHRUP.EQ.NWCORN(LINTYP).OR.
     U                 CHRUP.EQ.TEEDN(LINTYP) ).AND.
     D                (CHRDN.EQ.VERLIN(LINTYP).OR.
     D                 CHRDN.EQ.CROSS(LINTYP) .OR.
     D                 CHRDN.EQ.SECORN(LINTYP).OR.
     D                 CHRDN.EQ.SWCORN(LINTYP).OR.
     D                 CHRDN.EQ.NECORN(LINTYP).OR.
     D                 CHRDN.EQ.NWCORN(LINTYP).OR.
     D                 CHRDN.EQ.TEEUP(LINTYP) )
C CROSS
     +               ) SCREEN(HOR1,J) = CROSS(LINTYP)
                  IF(CHRUP.EQ.' ' .AND.
     D                (CHRDN.EQ.VERLIN(LINTYP).OR.
     D                 CHRDN.EQ.CROSS(LINTYP) .OR.
     D                 CHRDN.EQ.SECORN(LINTYP).OR.
     D                 CHRDN.EQ.SWCORN(LINTYP).OR.
     D                 CHRDN.EQ.TEEUP(LINTYP) )
C TEE DOWN
     T               ) SCREEN(HOR1,J) = TEEDN(LINTYP)
                  IF(CHRDN.EQ.' ' .AND.
     U                (CHRUP.EQ.VERLIN(LINTYP).OR.
     U                 CHRUP.EQ.CROSS(LINTYP) .OR.
     U                 CHRUP.EQ.NWCORN(LINTYP).OR.
     U                 CHRUP.EQ.NECORN(LINTYP).OR.
     U                 CHRUP.EQ.TEEDN(LINTYP) )
C TEE UP
     T               ) SCREEN(HOR1,J) = TEEUP(LINTYP)
               ELSE IF( CHRLFT.EQ.' ' .AND.
     L                 (CHRGHT.EQ.HORLIN(LINTYP).OR.
     L                  CHRGHT.EQ.CROSS(LINTYP) )
     T                )THEN
C TEE LEFT
                       SCREEN(HOR1,J) = TEELFT(LINTYP)
               ELSE IF( CHRGHT.EQ.' ' .AND.
     R                 (CHRLFT.EQ.HORLIN(LINTYP).OR.
     R                  CHRLFT.EQ.CROSS(LINTYP) )
     T                )THEN
C TEE RIGHT
                       SCREEN(HOR1,J) = TEERGT(LINTYP)
               ENDIF
1200        CONTINUE
C
         ELSE IF(VER1.EQ.VER2)THEN
C WE MOVED HORIZONTALLY, SO LOOK FOR VERTICAL CROSSINGS
            IF(SCREEN(HOR1,VER1-1).EQ.VERLIN(LINTYP) .AND.
     1         SCREEN(HOR1,VER1+1).EQ.VERLIN(LINTYP)) CHAR = 'T'
            IF(HOR1.LT.HOR2-1)THEN
               J1 = HOR1+1
               J2 = HOR2-1
               IF(CHAR.EQ.'T') SCREEN(HOR1,VER1) = TEERGT(LINTYP)
            ELSE IF(HOR2.LT.HOR1-1)THEN
               J1 = HOR2+1
               J2 = HOR1-1
            ELSE
               GOTO 1490
            ENDIF
            DO 1250 J=J1,J2
               CHRUP = SCREEN(J,VER1-1)
               CHRDN = SCREEN(J,VER1+1)
               IF( (CHRUP.EQ.VERLIN(LINTYP).OR.
     U              CHRUP.EQ.CROSS(LINTYP) .OR.
     U              CHRUP.EQ.SECORN(LINTYP).OR.
     U              CHRUP.EQ.SWCORN(LINTYP).OR.
     U              CHRUP.EQ.NECORN(LINTYP).OR.
     U              CHRUP.EQ.NWCORN(LINTYP).OR.
     U              CHRUP.EQ.TEEDN(LINTYP) ).AND.
     D             (CHRDN.EQ.VERLIN(LINTYP).OR.
     D              CHRDN.EQ.CROSS(LINTYP) .OR.
     D              CHRDN.EQ.SECORN(LINTYP).OR.
     D              CHRDN.EQ.SWCORN(LINTYP).OR.
     D              CHRDN.EQ.NECORN(LINTYP).OR.
     D              CHRDN.EQ.NWCORN(LINTYP).OR.
     D              CHRDN.EQ.TEEUP(LINTYP) )  )THEN
C          *
C WE HAVE  -  WHERE * IS |, +, A CORNER, A TEE
C          *
                  CHRLFT = SCREEN(J-1,VER1)
                  CHRGHT = SCREEN(J+1,VER1)
                  IF( (CHRLFT.EQ.HORLIN(LINTYP).OR.
     L                 CHRLFT.EQ.CROSS(LINTYP) .OR.
     L                 CHRLFT.EQ.TEERGT(LINTYP) ).AND.
     R                (CHRGHT.EQ.HORLIN(LINTYP).OR.
     R                 CHRGHT.EQ.CROSS(LINTYP) .OR.
     R                 CHRGHT.EQ.TEELFT(LINTYP) )
C CROSS
     +              ) SCREEN(J,VER1) = CROSS(LINTYP)
                  IF(CHRLFT.EQ.' ' .AND.
     R                (CHRGHT.EQ.HORLIN(LINTYP).OR.
     R                 CHRGHT.EQ.CROSS(LINTYP) .OR.
     R                 CHRGHT.EQ.TEELFT(LINTYP) )
C TEE RIGHT
     T              ) SCREEN(J,VER1) = TEERGT(LINTYP)
                  IF(CHRGHT.EQ.' ' .AND.
     L                (CHRLFT.EQ.HORLIN(LINTYP).OR.
     L                 CHRLFT.EQ.CROSS(LINTYP) .OR.
     L                 CHRLFT.EQ.TEERGT(LINTYP) )
C TEE LEFT
     T               ) SCREEN(J,VER1) = TEELFT(LINTYP)
               ELSE IF( CHRDN.NE.HORLIN(LINTYP) .AND.
     U                 (CHRUP.EQ.VERLIN(LINTYP).OR.
     U                  CHRUP.EQ.CROSS(LINTYP) .OR.
     U                  CHRUP.EQ.NWCORN(LINTYP).OR.
     U                  CHRUP.EQ.NECORN(LINTYP) )
     T                )THEN
C TEE UP
                       SCREEN(J,VER1) = TEEUP(LINTYP)
               ELSE IF( CHRUP.NE.HORLIN(LINTYP) .AND.
     D                 (CHRDN.EQ.VERLIN(LINTYP).OR.
     D                  CHRDN.EQ.CROSS(LINTYP) .OR.
     D                  CHRDN.EQ.SWCORN(LINTYP).OR.
     D                  CHRDN.EQ.SECORN(LINTYP) )
     T                )THEN
C TEE DOWN
                       SCREEN(J,VER1) = TEEDN(LINTYP)
               ENDIF
1250        CONTINUE
         ENDIF
1490     HOR1 = HOR2
         VER1 = VER2
C NEXT MOVE
1500  CONTINUE
C
      RETURN
C
C ** SCRPTH ENDS HERE
      END
      SUBROUTINE SCRARC(FORM,LINE,TAIL,HEAD,*)
C     =================
      INCLUDE 'DCSCREEN.'
CITSO      INCLUDE (DCSCREEN)
C
C This puts arc at vertical position = LINE
C
C   TAIL----->HEAD   or  HEAD<----TAIL
C
C ...Alternate return is error.
C
      CHARACTER*(*) FORM
C
      INCLUDE 'DCSCRDAT.'
CITSO      INCLUDE (DCSCRDAT)
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK COORDINATES
      IF(LINE.LT.1.OR.LINE.GT.LENGTH)THEN
         PRINT *,' ** SYSERR...SCREEN ARC...LINE=',LINE
         RETURN 1
      ENDIF
      L = TAIL - HEAD
      IF(L.LT.0)L = -L
      IF(TAIL.LT.1 .OR. TAIL.GT.WIDTH .OR.
     1   HEAD.LT.1 .OR. HEAD.GT.WIDTH .OR.
     2   L.LT.2)THEN
         PRINT *,' ** SYSERR...SCREEN ARC...TAIL,HEAD=',TAIL,HEAD
         IF(L.LT.2)PRINT *,' ...ARC LENGTH =',L,'...SHOULD BE > 1'
         RETURN 1
      ENDIF
C SET FORM
      IF(FORM.EQ.'GRAPHIC')THEN
         LINTYP = 1
      ELSE IF(FORM.EQ.'PRINTABL')THEN
         LINTYP = 2
      ELSE
         PRINT *,' ** SYSERR...SCREEN FORM ',FORM,'?'
         RETURN 1
      ENDIF
C DRAW LINE BETWEEN TAIL AND HEAD
      CALL SCRLIN(FORM,TAIL,LINE,HEAD,LINE,*1300)
C PUT ARROW HEAD AND SET HORIZONTAL RANGE H1-H2
      IF(TAIL.LT.HEAD)THEN
C TAIL ----> HEAD
         SCREEN(HEAD,LINE) = ARIGHT(LINTYP)
         H1 = TAIL
         H2 = HEAD-1
      ELSE
C HEAD <---- TAIL
         SCREEN(HEAD,LINE) = ARLEFT(LINTYP)
         H1 = HEAD+1
         H2 = TAIL
      ENDIF
      IF(LINE.EQ.1 .OR. LINE.EQ.WIDTH)RETURN
C FIX CROSSINGS
      DO 500 H=H1,H2
         IF(SCREEN(H,LINE+1).EQ.VERLIN(LINTYP).AND.
     1      SCREEN(H,LINE-1).EQ.VERLIN(LINTYP))
     +      SCREEN(H,LINE) = CROSS(LINTYP)
500   CONTINUE
C
      RETURN
C
1300  RETURN 1
C
C ** SCRARC ENDS HERE
      END
      SUBROUTINE SCRARR(FORM,HOR,VER,DIREC,*)
C     =================
      INCLUDE 'DCSCREEN.'
CITSO      INCLUDE (DCSCREEN)
C
C This puts arrow head at HOR,VER...DIREC = LEFT | RIGHT
C ...Alternate return is error.
C
      CHARACTER*(*) FORM,DIREC
      INTEGER       HOR,VER
      INCLUDE 'DCSCRDAT.'
CITSO      INCLUDE (DCSCRDAT)
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK COORDINATES
      IF(HOR.LT.1.OR.HOR.GT.WIDTH.OR.VER.LT.1.OR.VER.GT.LENGTH)THEN
         PRINT *,' ** SYSERR...SCREEN ARROW...HOR,VER:',HOR,VER
         RETURN 1
      ENDIF
C SET FORM
      IF(FORM.EQ.'GRAPHIC')THEN
         LINTYP = 1
      ELSE IF(FORM.EQ.'PRINTABL')THEN
         LINTYP = 2
      ELSE
         PRINT *,' ** SYSERR...SCREEN FORM ',FORM,'?'
         RETURN 1
      ENDIF
      IF(DIREC.EQ.'LEFT')THEN
         SCREEN(HOR,VER) = ARLEFT(LINTYP)
      ELSE IF(DIREC.EQ.'RIGHT')THEN
         SCREEN(HOR,VER) = ARIGHT(LINTYP)
      ELSE
         PRINT *,' ** SYSERR...SCREEN ARROW ERROR...?',DIREC
         RETURN 1
      ENDIF
C
      RETURN
C
C ** SCRARR ENDS HERE
      END
      SUBROUTINE SCRTXT(CLIST,HOR,VER,*)
C     =================
      INCLUDE 'DCSCREEN.'
CITSO      INCLUDE (DCSCREEN)
C
C This puts text (CLIST) at HOR,VER
C Max text length = MIN[128,WIDTH]
C ...Alternate return is error.
C
      CHARACTER*(*) CLIST
      INTEGER       HOR,VER
C LOCAL
      CHARACTER*128 CTEMP
      INCLUDE 'DCSCRDAT.'
CITSO      INCLUDE (DCSCRDAT)
C
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(CLIST.EQ.' ')RETURN
C CHECK COORDINATES
      IF(HOR.LT.1.OR.HOR.GT.WIDTH.OR.VER.LT.1.OR.VER.GT.LENGTH)THEN
         PRINT *,' ** SYSERR...SCREEN TEXT ERROR...HOR,VER:',HOR,VER
         PRINT *,'    WIDTH, LENGTH =',WIDTH,LENGTH
         RETURN 1
      ENDIF
C COPY TEXT
      CTEMP = CLIST
      CALL FSLEN(CTEMP,128,LAST)
C CHECK LENGTH
      IF(LAST.GT.WIDTH-HOR)LAST = WIDTH-HOR
C OK, PUT TEXT ON SCREEN
      DO 100 I=1,LAST
100   SCREEN(HOR+I-1,VER) = CTEMP(I:I)
      RETURN
C
C ** SCRTXT ENDS HERE
      END
      SUBROUTINE SCRFIL(NWHOR,NWVER,SEHOR,SEVER,CHAR)
C     =================
      INCLUDE 'DCSCREEN.'
CITSO      INCLUDE (DCSCREEN)
C
C This fills area defined by box corners with CHAR.
C
      CHARACTER*1 CHAR
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK COORDINATES
      IF(NWHOR.LT.1.OR.NWVER.LT.1.OR.SEHOR.LT.1.OR.SEVER.LT.1.OR.
     1   NWHOR.GT.WIDTH .OR.SEHOR.GT.WIDTH .OR.
     2   NWVER.GT.LENGTH.OR.SEVER.GT.LENGTH.OR.
     3   NWHOR.GT.SEHOR .OR.NWVER.GT.SEVER)  RETURN
C
      DO 100 I=NWHOR,SEHOR
      DO 100 J=NWVER,SEVER
100   SCREEN(I,J) = CHAR
C
      RETURN
C
C ** SCRFIL ENDS HERE
      END
      SUBROUTINE SCRDEL(NWHOR,NWVER,SEHOR,SEVER)
C     =================
      INCLUDE 'DCSCREEN.'
CITSO      INCLUDE (DCSCREEN)
C
C This deletes (erases) in box from NW corner to SE corner.
C
C
C           NWHOR,NWVER
C                     |
C                     xxxxxxxxxxxx
C                     xxxxxxxxxxxx
C                     xxxxxxxxxxxx
C                                |
C                                SEHOR,SEVER
C
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK COORDINATES
      IF(NWHOR.LT.1.OR.NWVER.LT.1.OR.SEHOR.LT.1.OR.SEVER.LT.1.OR.
     1   NWHOR.GT.WIDTH .OR.SEHOR.GT.WIDTH .OR.
     2   NWVER.GT.LENGTH.OR.SEVER.GT.LENGTH.OR.
     3   NWHOR.GT.SEHOR .OR.NWVER.GT.SEVER)  RETURN
C
      DO 100 I=NWHOR,SEHOR
      DO 100 J=NWVER,SEVER
100   SCREEN(I,J) = ' '
C
      RETURN
C
C ** SCRDEL ENDS HERE
      END
      SUBROUTINE SCRLEN(L,*)
C     =================
      INCLUDE 'DCSCREEN.'
CITSO      INCLUDE (DCSCREEN)
C
C This prints SCREEN, using FTEXT.
C
C ...LINE is incremented.
C ...Alternate return is error.
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(WIDTH.LE.1 .OR. LENGTH.LE.1)RETURN 1
C DETERMINE LAST NON-NULL LINE
      DO 100 L=LENGTH,1,-1
         DO 50 K=1,WIDTH
50       IF(SCREEN(K,L).NE.' ')RETURN
100   CONTINUE
      RETURN
C
C ** SCRLEN ENDS HERE
      END
      SUBROUTINE SCRPRT(LINE,L,*)
C     =================
      INCLUDE 'DCSCREEN.'
CITSO      INCLUDE (DCSCREEN)
C
C This prints lines 1 thru L of SCREEN, using FTEXT.
C     If L > LENGTH of SCREEN, L is truncated to LENGTH.
C ...LINE is incremented.
C ...Alternate return is user abort or error.
C
C LOCAL
      CHARACTER*128 CLIST
      INTEGER       HOR,VER
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(WIDTH.LE.1 .OR. LENGTH.LE.1)RETURN 1
      IF(L.LE.0)RETURN
      IF(L.GT.LENGTH)THEN
C TRUNCATE TO SCREEN LENGTH (WAS DECLARED IN SCRCLR)
         LV = LENGTH
      ELSE
         LV = L
      ENDIF
C PRINT LINES 1 THRU LV
      DO 100 VER=1,LV
         CLIST = ' '
         DO 50 HOR=1,WIDTH
50       CLIST(HOR:HOR) = SCREEN(HOR,VER)
         CALL FSLEN(CLIST,128,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
100   CONTINUE
      RETURN
C
900   RETURN 1
C
C ** SCRPRT ENDS HERE
      END
