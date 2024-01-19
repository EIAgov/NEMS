C            ::: ASHOW.FOR  7-31-95 :::
C
C Created 7-31-95, but took from AQUERY2.FOR
C      8-19-93...Changed tabs in AQSHOW (CC 5-26-95)
C      5-26-95...Added footnote to AQSHOW (CC 7-17-95)
C      7-17-95...Added IO option to AQSHOW (CC 7-31-95)
C
C This contains the following subroutines for ANALYZE
C
C    AQSHOW...executes the SHOW command
C    AQSHTC...shows ARRAY format (like TRANCOL)
C    AQSHTJ...shows 1 page of cols of ARRAY
C    AQSHRC...converts real to string for cell entry
C
      SUBROUTINE AQSHOW(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C ...DCFLIP USED FOR SCRWTH
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This shows ROWS or COLUMNS in submatrix (using FTEXT)
C   Syntax:  SHOW [{{ROW | COLUMN} [conditional]
C                 | PLOT [row cond [,col cond]]] [//options]
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*128 CTEMP
      CHARACTER*16  RNAME,CNAME
      CHARACTER*12  STRSUM
      CHARACTER*5   RCFACT
      CHARACTER*8   OPTION(4)
      CHARACTER*4   ROWCOL,COLROW
      CHARACTER*1   STAT
      LOGICAL*1     SW,SHOSYN,PICK(4),SHOWIO
      EQUIVALENCE (SHOWIO,PICK(4))
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C SEE IF PLOT SPECIFIED
      F = FIRST
      CALL FTOKEN(CLIST,F,LAST,RNAME,8,STAT)
      IF(RNAME.NE.' ')THEN
         CALL FMATCH(RNAME,'PLOT ',' ',SW)
         IF(SW)THEN
            CALL STEPLT(CLIST,F,LAST,RCODE)
            RETURN
         ENDIF
      ENDIF
C USUAL SHOW COMMAND (NOT PLOT)
C SET DEFAULT FOR INCLUDING SYNTAX (SHOWIO IS SET BY PICK(4))
      SHOSYN = SWSYN
C SEE IF OPTION SPECIFIED
      CALL FLOOK(CLIST,FIRST,LAST,'//',I)
      IF(I.GT.0)THEN
C ...YES, CHECK SPEC
         F = I+2
         OPTION(1) = 'ARRAY '
         OPTION(2) = 'SYNTAX '
         OPTION(3) = 'NOSYNTAX'
         OPTION(4) = 'IO '
         NUMBER = 4
         RCODE = -1
         CALL FOPSET(CLIST,F,LAST,OPTION,NUMBER,PICK,RCODE)
         IF(RCODE.NE.0)RETURN
         IF( PICK(2) )THEN
            IF( .NOT.SWEXPL )THEN
               PRINT *,' ** SYNTAX NOT AVAILABLE...USE READIN'
            ELSE
               SHOSYN = .TRUE.
            ENDIF
         ELSE IF( PICK(3) )THEN
            SHOSYN = .FALSE.
         ENDIF
C BACKUP IN CLIST
         LAST = I-1
      ELSE
C NO OPTION SET...USE DEFAULTS
         SHOSYN = SWSYN
         PICK(1) = .FALSE.
         SHOWIO = .FALSE.
      ENDIF
      SHOSYN = SHOSYN .AND. SWEXPL
C                           :...REQUIRED TO HAVE SYNTAX AVIALABLE
C OPTIONS SET...SHOSYN = TRUE IFF SHOW SYNTAX (AND AVAILABLE)
C               SHOWIO = TRUE IFF SHOW IO FORMAT (SEE SHOWIO.DOC)
C               PICK(1)= TRUE IFF ARRAY FORMAT
C SET SUBMATRIX
      CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
      IF( RCODE.NE.0 )RETURN
      IF( PICK(1) )SWDISP = .TRUE.
      IF( SWDISP )THEN
C SHOW COL
         NRC = NRCSUB(2)
         COLROW = 'ROW '
         ROWCOL = 'COL '
         RC1    = RCSUB1(2)
         RCLAST = NCOLS
         RCFACT = 'Price'
      ELSE
C SHOW ROW
         NRC = NRCSUB(1)
         COLROW = 'COL '
         ROWCOL = 'ROW '
         RC1    = RCSUB1(1)
         RCLAST = NROWS
         RCFACT = 'Level'
      ENDIF
C
      LINE = 0
CC 7-29-95   = WAS 1
      IF( NRC.EQ.0 )THEN
         CLIST = 'SUBMATRIX HAS NO '//ROWCOL(:3)//'S'
         CALL FSLEN(CLIST,60,L)
         CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
         RETURN
      ENDIF
C
      IF( PICK(1).AND.ROWCOL.EQ.'COL ' )THEN
C ========== SHOW ARRAY FORMAT ============
         CALL AQSHTC(SHOSYN,RCODE)
         RETURN
      ENDIF
C ===== PREPARE TO SHOW REGULAR AND IO FORMATS =====
      TAB1 = NAMELN+2
CC 5-26-95          :...WAS 1
      IF(TAB1.LT.9)TAB1 = 9
      TAB2 = TAB1 + 14
      TAB3 = TAB2 + 14
CC 5-26-95           :...WAS 13
      ENDU = TAB3 + 12
C
C    :::LOOP OVER ROWS|COLUMNS:::
C
      DO 500 NUMBER=RC1,RCLAST
           CALL GETMAP(ROWCOL,NUMBER,SW)
           IF(.NOT.SW)GOTO 500
           NRC = NRC - 1
C
           CALL GETNAM(ROWCOL,NUMBER,CNAME)
           CALL GETRIM(ROWCOL,NUMBER,VL,VU,VC,NZ)
C
           CALL FI2C(RNAME,NZ,F)
           CLIST = ROWCOL//CNAME(:NAMELN)//' HAS '//RNAME(F:8)
     1            //' NONZERO'
           CALL FSLEN(CLIST,80,L)
           IF(NZ.NE.1)THEN
              CLIST(L+1:) = 'ES'
              L = L+2
           ENDIF
           IF(SHOSYN)THEN
C SHOW ITS SYNTAX
              CALL EXTRC(ROWCOL,CNAME,CTEMP,LS,RCODE)
              IF(RCODE.EQ.0)THEN
C STRIP AWAY ROW NAME
                 F = 1
                 CALL FTOKEN(CTEMP,F,LS,CNAME,8,STAT)
                 CALL FTOKEN(CTEMP,F,LS,CNAME,NAMELN,STAT)
                 IF(CTEMP(LS:LS).EQ.'.')THEN
C REMOVE PERIOD AT END OF SYNTAX
                    CTEMP(LS:) = ' '
                    LS = LS-1
                 ENDIF
                 CLIST(L+1:) = '...'//CTEMP(F:)
                 CALL FSLEN(CLIST,128,L)
              ELSE
                 RCODE = 0
              ENDIF
           ENDIF
           CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
C
           CLIST = 'BOUNDS:'
           CALL FR2C12(RNAME,VL)
           LR =12
           CALL FSQUEZ(RNAME,LR)
           CLIST(20:) = 'L='//RNAME
           CALL FR2C12(RNAME,VU)
           LR =12
           CALL FSQUEZ(RNAME,LR)
           CLIST(35:) = 'U='//RNAME
           L = ENDU
           CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
C
           CALL GETSOL(ROWCOL,NUMBER,VX,VP,STAT,STNUM)
           CLIST = 'SOLUTION:  STAT='//STAT
           CALL FR2C12(RNAME,VX)
CC 7-17-95 (1 LINE HERE AND 1 BELOW)
           IF( ROWCOL.EQ.'ROW ' )STRSUM = RNAME
           LR =12
           CALL FSQUEZ(RNAME,LR)
           CLIST(20:) = 'X='//RNAME
           CALL FR2C12(RNAME,VP)
           IF( ROWCOL.EQ.'COL ' )STRSUM = RNAME
C NOW STRSUM = THEORETICALLY CORRECT SUM OF FACTORS (STRING FORM)
CC =======
           LR =12
           CALL FSQUEZ(RNAME,LR)
           CLIST(35:) = 'D='//RNAME
           IF(ROWCOL.EQ.'ROW ')THEN
              CLIST(20:20) = 'Y'
              CLIST(35:35) = 'P'
           ENDIF
           L = ENDU
           CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
C
           IF( NZ.EQ.0 )GOTO 400
C
           NZMORE = 0
C SHOW NONZEROES IN SUBMATRIX (NOT IF FACTOR=0 AND IO FORMAT)
           IF( ROWCOL.EQ.'COL ' )THEN
              CALL GETCOL(NUMBER,N,MAXLST,ROWLST,VALLST)
              CLIST = 'Row'
              IF( N.GT.MAXLST )THEN
C LIST TRUNCATED (SHOULDN'T HAPPEN HERE BECAUSE MAXLST >= NROWS)
                 NZMORE = N-MAXLST
                 N      = MAXLST
              ENDIF
           ELSE
              CALL GETROW(NUMBER,ROWLST,VALLST,MAXLST,N,RCODE)
              IF( RCODE.NE.0 )THEN
C N WAS TRUNCATED TO MAXLST
                 NZMORE = 1
                 RCODE  = 0
              ENDIF
              CLIST = 'Column'
           ENDIF
           CLIST(TAB1:) = ' Coefficient'
           CLIST(TAB2:) = '    '//RCFACT
           CLIST(TAB3:) = '   Factor'
           L = ENDU
           CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
C UNDERLINE
           DO 250 K=1,ENDU
250        CLIST(K:K) = '-'
           L = ENDU
           CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
C
C      ::: LOOP OVER NONZEROES :::
           NS   = 0
           ZSUM = 0.
           NIO  = 0
           DO 300 I=1,N
               CALL GETMAP(COLROW,ROWLST(I),SW)
               IF( .NOT.SW )GOTO 300
               NS = NS+1
C COEF IN SUBMATRIX
               CALL GETNAM(COLROW,ROWLST(I),RNAME)
               CLIST = RNAME
               CALL GETSOL(COLROW,ROWLST(I),VX,VP,STAT,STNUM)
               IF(RCFACT(1:1).EQ.'P')THEN
C FACTOR = -PRICE*COEFF
                  VX = -VP*VALLST(I)
               ELSE
C FACTOR =  LEVEL*COEFF
                  VP = VX
                  VX = VX*VALLST(I)
               ENDIF
C NOW VP = PRICE | LEVEL  AND  VX = FACTOR
CC 7-31-95
               IF( SHOWIO )THEN
C SHOW IFF INPUT
                  IF( VX.GE.0. )GOTO 300
                  NIO = NIO+1
               ENDIF
CC =======
C ...SUM THE FACTOR
               ZSUM = ZSUM + VX
               CALL FR2C12(CLIST(TAB1:),VALLST(I))
               CALL FR2C12(CLIST(TAB2:),VP)
               CALL FR2C12(CLIST(TAB3:),VX)
               IF( SHOSYN )THEN
C SHOW SYNTAX
                  CALL EXTRC(COLROW,RNAME,CTEMP,LAST,RCODE)
                  IF(RCODE.EQ.0)THEN
C  STRIP AWAY ROWCOL NAME
                     F = 1
                     CALL FTOKEN(CTEMP,F,LAST,RNAME,8,STAT)
                     CALL FTOKEN(CTEMP,F,LAST,RNAME,NAMELN,STAT)
                     IF(CTEMP(LAST:LAST).EQ.'.')THEN
                        CTEMP(LAST:) = ' '
                        LAST = LAST-1
                     ENDIF
C  NOW CTEMP(F:LAST) = SYNTAX
                     IF(ENDU+LAST-F+2.LT.SCRWTH)THEN
                        CLIST(ENDU+1:) = CTEMP(F:)
                     ELSE
C  MUST PUT SYNTAX ON SEPARATE LINE
                        LAST = ENDU
                        CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
                        CLIST = '...'//CTEMP(F:)
                     ENDIF
                  ELSE
                     RCODE = 0
                  ENDIF
               ENDIF
C  PRINT OUTPUT LINE
               CALL FSLEN(CLIST,128,LAST)
               CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C  NEXT NONZERO
300        CONTINUE
C      ::: END LOOP OVER NONZEROES
CC 7-31-95
           IF( .NOT.SHOWIO )GOTO 390
              ZSINPT = ZSUM
C SHOW SUM OF INPUTS (IF MORE THAN 1)
              IF( NIO.GT.1 )THEN
                 VX = ZSUM
                 CALL FR2C12(RNAME,VX)
                 CLIST = ' '
                 CLIST(TAB2-5:) = 'Total Input ='
                 CLIST(TAB3:) = RNAME(:12)
                 CALL FSLEN(CLIST,70,LAST)
                 CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
              ENDIF
C LOOP AGAIN FOR OUTPUTS
           NIO  = 0
           ZSUM = 0.
           DO 350 I=1,N
               CALL GETMAP(COLROW,ROWLST(I),SW)
               IF( .NOT.SW )GOTO 350
C COEF IN SUBMATRIX
               CALL GETNAM(COLROW,ROWLST(I),RNAME)
               CLIST = RNAME
               CALL GETSOL(COLROW,ROWLST(I),VX,VP,STAT,STNUM)
               IF(RCFACT(1:1).EQ.'P')THEN
C FACTOR = -PRICE*COEFF
                  VX = -VP*VALLST(I)
               ELSE
C FACTOR =  LEVEL*COEFF
                  VP = VX
                  VX = VX*VALLST(I)
               ENDIF
C NOW VP = PRICE | LEVEL  AND  VX = FACTOR
CC 7-31-95
               IF( SHOWIO )THEN
C SHOW IFF OUTPUT
                  IF( VX.LE.0. )GOTO 350
                  NIO = NIO+1
               ENDIF
CC =======
C ...SUM THE FACTOR
               ZSUM = ZSUM + VX
               CALL FR2C12(CLIST(TAB1:),VALLST(I))
               CALL FR2C12(CLIST(TAB2:),VP)
               CALL FR2C12(CLIST(TAB3:),VX)
               IF( SHOSYN )THEN
C SHOW SYNTAX
                  CALL EXTRC(COLROW,RNAME,CTEMP,LAST,RCODE)
                  IF(RCODE.EQ.0)THEN
C  STRIP AWAY ROWCOL NAME
                     F = 1
                     CALL FTOKEN(CTEMP,F,LAST,RNAME,8,STAT)
                     CALL FTOKEN(CTEMP,F,LAST,RNAME,NAMELN,STAT)
                     IF(CTEMP(LAST:LAST).EQ.'.')THEN
                        CTEMP(LAST:) = ' '
                        LAST = LAST-1
                     ENDIF
C  NOW CTEMP(F:LAST) = SYNTAX
                     IF(ENDU+LAST-F+2.LT.SCRWTH)THEN
                        CLIST(ENDU+1:) = CTEMP(F:)
                     ELSE
C  MUST PUT SYNTAX ON SEPARATE LINE
                        LAST = ENDU
                        CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
                        CLIST = '...'//CTEMP(F:)
                     ENDIF
                  ELSE
                     RCODE = 0
                  ENDIF
               ENDIF
C  PRINT OUTPUT LINE
               CALL FSLEN(CLIST,128,LAST)
               CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C  NEXT NONZERO
350        CONTINUE
C      ::: END LOOP OVER NONZEROES
C ===== GET HERE IFF SHOW IO =====
C SHOW SUM OF OUTPUTS (IF MORE THAN 1)
           IF( NIO.GT.1 )THEN
              VX = ZSUM
              CALL FR2C12(RNAME,VX)
              CLIST = ' '
              CLIST(TAB2-5:) = 'Total Output ='
              CLIST(TAB3:) = RNAME(:12)
              CALL FSLEN(CLIST,70,LAST)
              CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
           ENDIF
           ZSUM = ZSUM + ZSINPT
C ============================================================
390        CONTINUE
CC =======
C UNDERLINE TABLE
           DO 410 K=1,ENDU
410        CLIST(K:K) = '='
           LAST = ENDU
           CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
           IF( NZMORE.GT.0 )THEN
              CLIST='Sorry, not enough space to show other nonzeroes.'
              CALL FSLEN(CLIST,70,LAST)
              CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
CC 7-17-195
C SUPPRESS FOOTNOTE
              STRSUM = ' '
CC ========
           ENDIF
C SEE IF TOTAL NUMBER OF NONZEROES (NZ) IS MORE THAN WHAT'S IN SUBMATRIX
           IF( NS.LT.NZ )THEN
C ...IT IS, SO LET USER KNOW HOW MANY WERE SHOWN
              IF( NS.EQ.1 )THEN
                 CLIST = '...1 nonzero in submatrix'
              ELSE
                 CALL FI2C(RNAME,NS,F)
                 CLIST = '...'//RNAME(F:8)//' nonzeroes in submatrix'
              ENDIF
CC 7-17-95
C SUPPRESS FOOTNOTE
              STRSUM = ' '
CC ========
           ENDIF
C SHOW SUM OF FACTORS
           VX = ZSUM
           CALL FR2C12(RNAME,VX)
           CLIST(TAB3-5:) = 'SUM: '//RNAME(:12)
           CALL FSLEN(CLIST,70,LAST)
           CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
CC 7-17-95
           IF( STRSUM.NE.' ' .AND. STRSUM.NE.RNAME(:12) )THEN
C ENTER FOOTNOTE FOR SUM
              CLIST = 'The SUM differs from its resident value, '
     1              //STRSUM
              CALL FSLEN(CLIST,80,LAST)
              CLIST(LAST+1:) = ', due to rounding.'
              CALL FSLEN(CLIST,100,LAST)
              CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
           ENDIF
CC ========
C
400     IF(NRC.EQ.0)RETURN
C NEXT ROW | COL  (SKIP LINE)
        CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
500   CONTINUE
C   ::: END MAIN LOOP OVER ROWS|COLUMNS :::
C
900   RETURN
C
C ** AQSHOW ENDS HERE
      END
      SUBROUTINE AQSHTC(SHOSYN,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C ...DCFLIP USED FOR SCRWTH
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This shows COLUMNS in submatrix (using FTEXT) in ARRAY format
C (like TRANCOL).
C
C  <-NAMELN-> <-HWIDTH->
C              Col_name
C              [syntax]...could be > 1 line
C             ---------------                           <--SWIDTH-->
C  LEVEL:          X                                    [syntax]
C  LO:             XLO
C  UP:             XUP
C  DJ:             D
C  <objective>    coef
C  Row_name       coef                                  [r rhsval]
C  ...            ...
C
C  Width of stub = Name length (NAMELN), which is 6 to 16
C  HWIDTH = Width of head = Max{NAMELN,12}
C  SWIDTH = Width of syntax (depends)
C  CPPAGE = Number of columns per page (depends on screen width)
C
      LOGICAL*1 SHOSYN
C               :...= TRUE MEANS SHOW SYNTAX
C
C LOCAL
      CHARACTER*128 CLIST
      LOGICAL*1     SW
      INTEGER       COLNUM(10)
C                          :...MAX # COLS PER PAGE
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C SET LAYOUT
      IF( SHOSYN )THEN
C TRY ALLOWING 40 CHARS
         SWIDTH = 40
      ELSE
         SWIDTH = 0
      ENDIF
      HWIDTH = NAMELN
      IF( HWIDTH.LT.12 )HWIDTH = 12
      IF( SCRWTH.LT.NAMELN+HWIDTH+18 )THEN
         PRINT *,' Sorry, screen width to low...need at least',
     1           NAMELN+HWIDTH+18
         RCODE = 1
         RETURN
      ENDIF
C
      REMAIN = NRCSUB(2)
      CPPAGE = (SCRWTH-NAMELN-SWIDTH-2)/(HWIDTH+1)
      IF( CPPAGE.LT.3 .AND. SHOSYN )THEN
C REDUCE LENGTH OF SYNTAX FIELD (FOR ROWS)
         CPPAGE = 3
         SWIDTH = SCRWTH-NAMELN-3*HWIDTH-5
      ELSE IF( .NOT.SHOSYN .AND. REMAIN.GT.CPPAGE )THEN
C NO SYNTAX AND MORE THAN 1 PAGE NEEDED...MAYBE MAKE ROOM FOR RHS
         IF( CPPAGE.GT.3 )CPPAGE = CPPAGE-1
      ENDIF
C ======== LAYOUT PARAMS SET ========
      NC = 0
      CLIST = ' '
C      ::: LOOP OVER COLS :::
      DO 500 J=RCSUB1(2),NCOLS
         CALL GETMAP('COL ',J,SW)
         IF( .NOT.SW )GOTO 500
         REMAIN = REMAIN-1
         IF( NC.LT.CPPAGE )THEN
C ADD COL (J) TO OUR LIST FOR THIS PAGE
            NC = NC+1
            COLNUM(NC) = J
         ELSE
C WE HAVE FULL PAGE OF COLS...SHOW THEM
            CALL AQSHTJ(NC,COLNUM,HWIDTH,SWIDTH,RCODE,*900)
C NOW PUT THIS COLUMN ON NEW PAGE
            NC = 1
            COLNUM(1) = J
C FORCE PROMPT FOR NEXT PAGE
            LAST = 1
            LINE = SCRWTH+1
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR',*900)
         ENDIF
C
         IF( REMAIN.EQ.0 )GOTO 800
500   CONTINUE
C
800   CONTINUE
      IF( NC.GT.0 )
     1   CALL AQSHTJ(NC,COLNUM,HWIDTH,SWIDTH,RCODE,*900)
C ============= DONE ============
C
900   RETURN
C
C ** AQSHTC ENDS HERE
      END
      SUBROUTINE AQSHTJ(NC,COLNUM,HWIDTH,SWIDTH,RCODE,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C ...DCFLIP USED FOR SCRWTH
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This shows a page of columns (called only by AQSHTC)
C ...Alternate return is user abort (from FTEXT).
C
      INTEGER NC,COLNUM(NC),HWIDTH,SWIDTH,RCODE
C                :          :      :...Syntax field width (= 0 if no syntax)
C                :          :...Head width (of cols)
C                :...List of cols to be shown
C LOCAL
      CHARACTER*128 CLIST,CTEMP
      CHARACTER*16  CNAME,RNAME,UNDRLN
      CHARACTER*1   CHAR
      LOGICAL*1     SW
      DATA          UNDRLN/'----------------'/
CC 6-7-93...CHANGED CALL FR2C12 TO AQSHRC
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      LINE = 1
C LINE 1 = COL NAMES
      CLIST = ' '
      TAB = NAMELN+1
      DO 100 L=1,NC
         J = COLNUM(L)
         CALL GETNAM('COL ',J,CNAME)
         CLIST(TAB:) = CNAME
         TAB = TAB+HWIDTH+1
         SCRIND(L) = 1
100   CONTINUE
C
      CALL FTEXT(CLIST,TAB,1,0,LINE,'CLEAR ',*9000)
      IF( SWIDTH.EQ.0 )GOTO 500
C
200   CONTINUE
C COLUMN SYNTAX
      CLIST = ' '
      TAB = NAMELN+1
      SW = .FALSE.
C          :...MEANS NO MORE SYNTAX LINES
      DO 290 L=1,NC
         J = COLNUM(L)
         CALL GETNAM('COL ',J,CNAME)
         LAST = 1
         CALL EXTRC('COL ',CNAME,CTEMP,LAST,RCODE)
         IF( RCODE.NE.0 )RETURN
C BEGIN SYNTAX WHERE WE LEFT OFF (STORED IN SCRIND)
         BEGSYN = SCRIND(L)
         IF( BEGSYN.LE.LAST )THEN
C MORE SYNTAX FOR THIS COL
            SW = .TRUE.
            IF( BEGSYN.EQ.1 )THEN
C STRIP AWAY COL NAME
               CALL FTOKEN(CTEMP,BEGSYN,LAST,RNAME,8,CHAR)
               CALL FTOKEN(CTEMP,BEGSYN,LAST,RNAME,NAMELN,CHAR)
            ENDIF
            IF( CTEMP(LAST:LAST).EQ.'.' )THEN
               CTEMP(LAST:) = ' '
               LAST = LAST-1
            ENDIF
C END SYNTAX BEFORE HEAD WIDTH
            ENDSYN = BEGSYN+HWIDTH-1
            IF( ENDSYN.GT.LAST )THEN
               ENDSYN = LAST
               SCRIND(L) = LAST+2
            ELSE
C BACKUP TO A BLANK OR PUNCTUATION MARK
250            CONTINUE
                 CHAR = CTEMP(ENDSYN:ENDSYN)
                 IF( CHAR.NE.' '.AND.CHAR.NE.','.AND.CHAR.NE.')' )THEN
                    ENDSYN = ENDSYN-1
                    IF( ENDSYN.GT.BEGSYN )GOTO 250
                    ENDSYN = BEGSYN+HWIDTH-1
                 ENDIF
               SCRIND(L) = ENDSYN+1
            ENDIF
            CLIST(TAB:) = CTEMP(BEGSYN:ENDSYN)
         ENDIF
         TAB = TAB+HWIDTH+1
290   CONTINUE
C
      IF( SW )THEN
C PRINT LINE OF SYNTAX AND GO AGAIN
         CALL FTEXT(CLIST,TAB,1,0,LINE,'CLEAR ',*9000)
         GOTO 200
      ENDIF
C
500   CONTINUE
C END OF SYNTAX...UNDERLINE
      CLIST = ' '
      TAB = NAMELN+1
      DO 590 L=1,NC
         CLIST(TAB:) = UNDRLN(1:HWIDTH)
         TAB = TAB+HWIDTH+1
590   CONTINUE
C
      CALL FTEXT(CLIST,TAB,1,0,LINE,'CLEAR ',*9000)
C
C GET BOUNDS AND OBJ, AND PUT INTO VALLST (3 VALUES PER COLUMN)
      IPOINT = 1
      DO 650 L=1,NC
         J = COLNUM(L)
         CALL GETRIM('COL ',J,VL,VU,VC,NZ)
         VALLST(IPOINT)   = VL
         VALLST(IPOINT+1) = VU
         VALLST(IPOINT+2) = VC
         IPOINT = IPOINT+3
650   CONTINUE
C
C LEVELS
      IF( NAMELN.GT.4 )THEN
         CLIST = 'Level:'
      ELSE
         CLIST = 'Lev.'
      ENDIF
      TAB = NAMELN+1
      DO 700 L=1,NC
         J = COLNUM(L)
         CALL GETSOL('COL ',J,VX,VP,CHAR,STNUM)
         CALL AQSHRC(CNAME,VX)
         CLIST(TAB:) = CNAME
         TAB = TAB+HWIDTH+1
700   CONTINUE
      CALL FTEXT(CLIST,TAB,1,0,LINE,'CLEAR ',*9000)
C
C LOWER BOUNDS
      CLIST = 'LO:'
      TAB = NAMELN+1
      IPOINT = 1
      DO 720 L=1,NC
         J = COLNUM(L)
         CALL AQSHRC(CNAME,VALLST(IPOINT))
         CLIST(TAB:) = CNAME
         TAB = TAB+HWIDTH+1
         IPOINT = IPOINT+3
720   CONTINUE
      CALL FTEXT(CLIST,TAB,1,0,LINE,'CLEAR ',*9000)
C
C UPPER BOUNDS
      CLIST = 'UP:'
      TAB = NAMELN+1
      IPOINT = 2
      DO 740 L=1,NC
         J = COLNUM(L)
         CALL AQSHRC(CNAME,VALLST(IPOINT))
         CLIST(TAB:) = CNAME
         TAB = TAB+HWIDTH+1
         IPOINT = IPOINT+3
740   CONTINUE
      CALL FTEXT(CLIST,TAB,1,0,LINE,'CLEAR ',*9000)
C
C OBJECTIVE COEFS
      CLIST = OBJNAM
      CALL FSLEN(CLIST,16,L)
      CLIST(L+1:) = ':'
      TAB = NAMELN+1
      IPOINT = 3
      DO 750 L=1,NC
         J = COLNUM(L)
         CALL AQSHRC(CNAME,VALLST(IPOINT))
         CLIST(TAB:) = CNAME
         TAB = TAB+HWIDTH+1
         IPOINT = IPOINT+3
750   CONTINUE
      CALL FTEXT(CLIST,TAB,1,0,LINE,'CLEAR ',*9000)
C
C REDUCED COSTS
      CLIST = 'DJ:'
      TAB = NAMELN+1
      DO 760 L=1,NC
         J = COLNUM(L)
         CALL GETSOL('COL ',J,VX,VP,CHAR,STNUM)
         CALL AQSHRC(CNAME,VP)
         CLIST(TAB:) = CNAME
         TAB = TAB+HWIDTH+1
760   CONTINUE
      CALL FTEXT(CLIST,TAB,1,0,LINE,'CLEAR ',*9000)
C
C NOW THE ROWS (EXCEPT OBJ)
C FIRST, USE AMAP (EQIV SCRIND) TO KEEP TRACK OF ROWS ALREADY PRINTED
      DO 800 I=1,NROWS
         CALL GETMAP('ROW ',I,SW)
         AMAP(I) = .NOT.SW
C WE USE AMAP(I) = TRUE TO MEAN SKIP ROW
C ...EITHER NOT IN SUBMATRIX OR ALREADY PRINTED
800   CONTINUE
      AMAP(OBJNUM) = .TRUE.
C
      TAB = NAMELN+1
      CLIST = ' '
      BEGSYN = NAMELN+2 + NC*(HWIDTH+1)
C
C  ::: LOOP OVER COLUMNS :::
      DO 1100 L=1,NC
         J = COLNUM(L)
         CALL GETCOL(J,NZ,MAXLST,ROWLST,VALLST)
         IF( NZ.EQ.0 )GOTO 1090
C   ::: LOOP OVER NONZEROES OF COLUMN (J) :::
         DO 1000 I=1,NZ
            ROW = ROWLST(I)
            IF( AMAP(ROW) )GOTO 1000
            AMAP(ROW) = .TRUE.
C PREPARE TO PRINT ROW...GET NAME
            CALL GETNAM('ROW ',ROW,RNAME)
            CLIST = RNAME
            CALL AQSHRC(CNAME,VALLST(I))
            CLIST(TAB:) = CNAME
            IF( L.LT.NC )THEN
C SEE IF OTHER COLS HAVE THIS ROW
               DO 900 LL=L+1,NC
                  JJ = COLNUM(LL)
                  CALL GETAIJ(ROW,JJ,V)
                  IF( V.NE.0. )THEN
                     CALL AQSHRC(CNAME,V)
                     TABLL = TAB + (LL-L)*(HWIDTH+1)
                     CLIST(TABLL:TABLL+HWIDTH) = CNAME
                  ENDIF
900            CONTINUE
            ENDIF
C END OF ROW COEFFICIENTS
            IF( SWIDTH.GT.0 )THEN
C ROW SYNTAX
               LAST = 1
               CALL EXTRC('ROW ',RNAME,CTEMP,LAST,RCODE)
               IF( RCODE.NE.0 )RETURN
C STRIP AWAY ROW NAME
               F = 1
               CALL FTOKEN(CTEMP,F,LAST,RNAME,8,CHAR)
               CALL FTOKEN(CTEMP,F,LAST,RNAME,NAMELN,CHAR)
               IF( CTEMP(LAST:LAST).EQ.'.' )THEN
                  CTEMP(LAST:) = ' '
                  LAST = LAST-1
               ENDIF
               CLIST(BEGSYN:) = CTEMP(F:LAST)
            ELSE IF( BEGSYN+14.LT.SCRWTH .AND. BEGSYN.LT.114 )THEN
C NO SYNTAX, BUT THERE IS ROOM TO PUT RHS
               CALL GETBND('ROW ',ROW,VL,VU)
               CALL GETYPE(ROW,CHAR)
               IF( CHAR.EQ.'E' )THEN
                  CLIST(BEGSYN:) = '='
                  CALL FR2CLJ(CLIST(BEGSYN+2:),VL,LC)
               ELSE IF( CHAR.EQ.'G' )THEN
                  CLIST(BEGSYN:) = '>'
                  CALL FR2CLJ(CLIST(BEGSYN+2:),VL,LC)
               ELSE IF( CHAR.EQ.'L' )THEN
                  CLIST(BEGSYN:) = '<'
                  CALL FR2CLJ(CLIST(BEGSYN+2:),VU,LC)
               ELSE IF( CHAR.EQ.'N' )THEN
                  CLIST(BEGSYN:) = 'Free'
               ELSE IF( CHAR.EQ.'R' )THEN
                  IF( BEGSYN+28.LT.SCRWTH .AND. BEGSYN.LT.100 )THEN
                     CLIST(BEGSYN:) = '<'
                     CALL FR2CLJ(CLIST(BEGSYN+1:),VL,LC)
                     CLIST(BEGSYN+LC+3:) = '& >'
                     CALL FR2CLJ(CLIST(BEGSYN+LC+4:),VU,LC)
                  ELSE
                     CLIST(BEGSYN:) = 'Range'
                  ENDIF
               ELSE
                  CLIST(BEGSYN:) = CHAR//'?'
               ENDIF
            ENDIF
C PRINT ROW
            CALL FSLEN(CLIST,128,LAST)
            IF( LAST.GE.SCRWTH )THEN
               LAST = SCRWTH-2
               CLIST(LAST-2:) = '...'
            ENDIF
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
1000     CONTINUE
C ::: END OF ROWS WITH NONZEROES IN COL (J) :::
1090     CONTINUE
C ADVANCE TAB TO BEGIN NEXT COL
         TAB = TAB + HWIDTH + 1
C ::: READY FOR NEXT COLUMN IN LIST (LOOP INDEX L)
1100  CONTINUE
C  ====================== DONE ===================
      RETURN
C
9000  RETURN 1
C
C ** AQSHTJ ENDS HERE
      END
      SUBROUTINE AQSHRC(STRING,V)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This converts real (V) to STRING for cell entry in array.
C       s1234567[.yyy] | sX.Y with length of Y > 3
C
      CHARACTER*(*) STRING
C LOCAL
      CHARACTER*12  STR12,FRAC
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      CALL FR2CRJ(STR12,V,FIRST)
      CALL FSLEN(STR12,12,L)
      CALL FLOOKF(STR12,1,11,'.',DECMAL)
      STRING = STR12
      IF( DECMAL.EQ.0 )THEN
C SHIFT STR12 LEFT, UP TO 4 SPACES
         IF( FIRST.GE.4 )FIRST = 5
         STRING = STR12(FIRST:12)
      ELSE
         FRAC = STR12(DECMAL+1:)
         CALL FSLEN(FRAC,11,LF)
         IF( LF.GE.3 )RETURN
C MOVE STR12 LEFT TO HAVE TRAILING BLANKS (DECIMAL ALIGNED)
         STRING = STR12(4-LF:12)
      ENDIF
C
      RETURN
C
C ** AQSHRC ENDS HERE
      END
