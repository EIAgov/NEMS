C             ::: GETIOSUB.FOR  7-01-95 :::
C
C LAST DATE: Earlier dates deleted
C        1-23-94...Changed G13 to G12...MINOS failed with G13
C        2-06-94...Changed GWRSUB to allow NAMELN > 10 (CC)
C                  Replaced FR2C12 with FR2CLF in GWRDUL
C
C This contains the following GETMAT IO subroutine.
C
C     GWRSUB.....writes submatrix (contains MPS format)
C     GWRDUL.....writes dual matrix file (MPS format)
C
C RCODE = return code = 0 if all is well
C
      SUBROUTINE GWRSUB(SWMSG,FILE,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This writes submatrix to FILE, presumed open.
C ...FILE is closed before return.
C
      LOGICAL*1     SWMSG
C LOCAL
      LOGICAL*1     SW,SWRNG,SWRHS
      CHARACTER*1   CHAR
      CHARACTER*2   BNDTYP
      CHARACTER*16  RNAME,CNAME,FREROW
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
CC 6-30-95 ALLOWING NAMELN > 10
      WRITE(FILE,1,ERR=13)PRBNAM
1     FORMAT('NAME',T15,A16,' FROM ANALYZE (SUBMATRIX)'
     1      /'ROWS')
      SWRNG = .FALSE.
      FREROW = ' '
C
      DO 100 ROW=1,NROWS
         CALL GETMAP('ROW ',ROW,SW)
         IF(.NOT.SW )GOTO 100
C ROW IS IN SUBMATRIX...GET ITS TYPE
         CALL GETYPE(ROW,CHAR)
         IF(CHAR.EQ.'R')THEN
C ...RANGE (DECLARE AS G-ROW)
            CHAR='G'
            SWRNG = .TRUE.
         ELSE IF(CHAR.EQ.'N')THEN
            CALL GETNAM('ROW ',ROW,FREROW)
         ENDIF
C
         CALL GETNAM('ROW ',ROW,RNAME)
         WRITE(FILE,51,ERR=13)CHAR,RNAME
51       FORMAT(1X,A1,T5,A16)
CC 6-30-95                == WAS 10
100   CONTINUE
C
      IF( FREROW.EQ.' ' )THEN
C NO FREE ROW...PUT IN OBJECTIVE TO AVOID ERROR WHEN READING
         CHAR = 'N'
         WRITE(FILE,51,ERR=13)CHAR,OBJNAM
         FREROW = OBJNAM
         IF( SWMSG )PRINT *,' Row ',OBJNAM(:NAMELN),
     1             ' added to ROWS section'
      ENDIF
C
      WRITE(FILE,191,ERR=13)
191   FORMAT('COLUMNS')
C
      DO 500 COL=1,NCOLS
         CALL GETMAP('COL ',COL,SW)
         IF(.NOT.SW)GOTO 500
         CNAME = NAME(ICNAME + COL)
C COLUMN IS IN SUBMATRIX...WRITE ITS NONZEROES (SUPPRESS IF NULL)
         NZ = BASE(ICINFO + COL) - BASE(ICINFO + COL-1)
         I1 = INONZ + 2*BASE(ICINFO+COL-1)
         I2 = I1 + 2*NZ - 1
C
         RNAME = ' '
C   ::: LOOP OVER COL'S NONZEROES :::
         DO 400 I=I1,I2,2
            R = INDEX(I)
            CALL GETMAP('ROW ',R,SW)
            IF(.NOT.SW)GOTO 400
            CALL GETVAL(V,I+1)
CC 6-30-95
C! For MPS comparisons, write one nonzero per line, even if NAMELN is small
C!            IF( NAMELN.GT.10 )THEN
C WRITE 1 NONZERO WITH 16-CHAR NAMES (IGNORE STD FIELD WIDTHS)
                WRITE(FILE,402,ERR=13)CNAME,NAME(IRNAME+R),V
C402             FORMAT(T5,A16,T25,A16,T50,G12.6)
402             FORMAT(T5,A10,A10,F20.8)
                GOTO 400
C!            ENDIF
CC =======
            IF(RNAME.EQ.' ')THEN
C DEFER WRITING THIS NONZERO
               RNAME = NAME(IRNAME + R)
               V1 = V
            ELSE
C WRITE 2 NONZEROES FOR COL
               WRITE(FILE,401,ERR=13)CNAME,RNAME,V1,NAME(IRNAME+R),V
401            FORMAT(T5,A10,A10,G12.6,T40,A10,G12.6)
C                            :   :             :...COLUMN 50
C                            :   :...COLUMN 25
C                            :...COLUMN 15
C (REMOVED T15, T25 AND T50 BECAUSE SOME COMPILERS GET CONFUSED)
C ...DECREASED FROM G13 TO G12
               RNAME = ' '
            ENDIF
400      CONTINUE
         IF( RNAME.NE.' ' )WRITE(FILE,401,ERR=13)CNAME,RNAME,V1
C NEXT COLUMN
500   CONTINUE
C
      WRITE(FILE,591,ERR=13)
591   FORMAT('RHS')
      SWRHS = .FALSE.
C
C  ::: LOOP OVER ROWS (AGAIN) TO WRITE RHS VALUES
C
      DO 600 ROW=1,NROWS
         CALL GETMAP('ROW ',ROW,SW)
         IF(.NOT.SW)GOTO 600
C ROW IS IN SUBMATRIX...DETERMINE RHS FROM BOUNDS (NOTE: RANGE IS G-ROW)
         CALL GETBND('ROW ',ROW,VL,VU)
C
         IF(VL.LE.-VINF)THEN
            IF(VU.LT. VINF)THEN
               V = VU
            ELSE
               GOTO 600
            ENDIF
         ELSE
            V = VL
         ENDIF
         IF(ABS(V).LE.VTOLAB)GOTO 600
C
         CALL GETNAM('ROW ',ROW,RNAME)
CC 6-30-95
         IF( NAMELN.LE.10 )THEN
            WRITE(FILE,601,ERR=13)RHSNAM,RNAME,V
601         FORMAT(T5,A10,A10,F20.8)
         ELSE
            WRITE(FILE,602,ERR=13)RHSNAM,RNAME,V
602         FORMAT(T5,A16,T25,A16,T50,F20.8)
         ENDIF
CC ======= (JUST HAD 601)
         SWRHS = .TRUE.
600   CONTINUE
C :::::::::::::::::: END RHS SECTION ::::::::::::::::::::
C
      IF( .NOT.SWRHS )THEN
C ALL RHS'S = 0...SOME SOLVERS DON'T LIKE THIS, SO PUT 0 RHS
C       ON FREE ROW (FREROW)
         V = 0.
         WRITE(FILE,601,ERR=13)RHSNAM,FREROW,V
         IF( SWMSG )PRINT *,' 0 RHS PUT FOR ',FREROW(:NAMELN)
      ENDIF
C
      IF( .NOT.SWRNG )GOTO 1000
C
      WRITE(FILE,650,ERR=13)
650   FORMAT('RANGES')
C
C  ::: LOOP OVER ROWS (AGAIN) TO WRITE RANGE VALUES
C
      DO 800 ROW=1,NROWS
         CALL GETMAP('ROW ',ROW,SW)
         IF(.NOT.SW)GOTO 800
C ROW IS IN SUBMATRIX...GET ITS TYPE
         CALL GETYPE(ROW,CHAR)
         IF(CHAR.NE.'R')GOTO 800
C ROW IS RANGE...GET ITS BOUNDS
         CALL GETBND('ROW ',ROW,VL,VU)
         V = VU - VL
C                    = RANGE
         CALL GETNAM('ROW ',ROW,RNAME)
CC 6-30-95
         IF( NAMELN.LE.10 )THEN
            WRITE(FILE,601,ERR=13)RNGNAM,RNAME,V
         ELSE
            WRITE(FILE,602,ERR=13)RNGNAM,RNAME,V
         ENDIF
CC ======= (JUST HAD 601)
800   CONTINUE
C
C
1000  CONTINUE
      IF( BNDNAM.EQ.'none' )GOTO 1900
      WRITE(FILE,1001,ERR=13)
1001  FORMAT('BOUNDS')
      VINFX = VINF/8
C
C  ::: LOOP OVER COLUMNS TO GET BOUNDS :::
C
      DO 1200 COL=1,NCOLS
         CALL GETMAP('COL ',COL,SW)
         IF(.NOT.SW)GOTO 1200
C COL IS IN SUBMATRIX...GET ITS BOUNDS
         CALL GETBND('COL ',COL,VL,VU)
         IF(VL.EQ.0.0 .AND. VU.GE.VINF)GOTO 1200
C COL BOUNDED (OR FREED)
         CNAME = NAME(ICNAME + COL)
C
         IF(VL.GE.VU)THEN
            BNDTYP = 'FX'
CC 6-90-95
            IF( NAMELN.LE.10 )THEN
               WRITE(FILE,1201,ERR=13)BNDTYP,BNDNAM,CNAME,VL
1201           FORMAT(1X,A2,T5,A10,A10,F20.8)
            ELSE
               WRITE(FILE,1202,ERR=13)BNDTYP,BNDNAM,CNAME,VL
1202           FORMAT(1X,A2,T5,A16,T25,A16,T50,F20.8)
            ENDIF
CC ======= (AND BELOW)
         ELSE
            IF(VL.GT.-VINFX .AND. ABS(VL).GE.VTOLAB)THEN
               BNDTYP = 'LO'
               IF( NAMELN.LE.10 )THEN
                  WRITE(FILE,1201,ERR=13)BNDTYP,BNDNAM,CNAME,VL
               ELSE
                  WRITE(FILE,1202,ERR=13)BNDTYP,BNDNAM,CNAME,VL
               ENDIF
            ELSE IF(VL.LE.-VINFX)THEN
               IF(VU.GE.VINFX)THEN
                  BNDTYP = 'FR'
               ELSE
                  BNDTYP = 'MI'
               ENDIF
               IF( NAMELN.LE.10 )THEN
                  WRITE(FILE,1201,ERR=13)BNDTYP,BNDNAM,CNAME
               ELSE
                  WRITE(FILE,1202,ERR=13)BNDTYP,BNDNAM,CNAME
               ENDIF
            ENDIF
            IF(VU.LT.VINFX)THEN
               BNDTYP = 'UP'
               IF( NAMELN.LE.10 )THEN
                  WRITE(FILE,1201,ERR=13)BNDTYP,BNDNAM,CNAME,VU
               ELSE
                  WRITE(FILE,1202,ERR=13)BNDTYP,BNDNAM,CNAME,VU
               ENDIF
            ENDIF
         ENDIF
C NEXT COLUMN
1200  CONTINUE
C
1900  CONTINUE
        WRITE(FILE,1901,ERR=13)
1901    FORMAT('ENDATA')
C
C ALL NORMAL RETURNS COME HERE
9000  CONTINUE
      RCODE = 0
      CLOSE(FILE)
      RETURN
C
13    CLOSE(FILE)
      PRINT *,' ** IO ERROR WRITING MATRIX FILE...',RCODE
      RCODE = 1
      RETURN
C
C ** GWRSUB ENDS HERE
      END
      SUBROUTINE GWRDUL(SWMSG,FILE,NSPACE,ISPACE,VSPACE,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This writes a matrix file for the dual of the LP.
C ...FILE is presumed open (formatted), and is closed before return.
C
C   Primal:  Opt cx : Ax {=|<=|>=} b, L <= x <= U
C   Dual:    Opt DUAL_OBJ = p*b + l*L - u*U : pA + l - u = c
C                           p(i) {unr.|<= 0|>= 0}
C                           l, u >= 0
C Sign of p(i) associated with sense of primal row, depends on primal OPT.
C (Dual sense of Opt opposite, but does not affect matrix file.)
C
C Dual row j <---> primal variable j (same name)
C      If L(j) = U(j), row j is free (N type)
C      If L(j) = 0 < U(j), row is L type (l(j) not defined)
C      If U(j) = 0 > L(j), row is G type (u(j) not defined)
C      Else, row j is E type (both l(j) and u(j) defined)
C Dual col p(i) <---> primal row i (same name)
C   If L(j) <> 0, U(j), -*,  l(j) <---> x(j) >= L(j)
C   If U(j) <> 0, L(j),  *, -u(j) <---> x(j) <= U(j)
C
      LOGICAL*1  SWMSG
      INTEGER    ISPACE(NSPACE)
      REAL       VSPACE(NSPACE)
C                       :... >= LENGTH OF LONGEST ROW (PASSED TO GETROW)
C LOCAL VARIABLES
      LOGICAL*1    SW,SW2
      CHARACTER*80 CHR80
      CHARACTER*16 CNAME,RNAME
      CHARACTER*1  CHAR
      CHARACTER*16 DOBJ,DRHS,DBND
      PARAMETER  (TABMSG=5)
C :::::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
CC 6-30-95 ALLOWING NAMELN > 10 (Related deletions not marked with CC)
C NAME CARD
      CHR80      = 'NAME'
      CHR80(15:) = PRBNAM
      CHR80(35:) = 'DUAL FROM ANALYZE (SUBMATRIX)'
      WRITE(FILE,1,ERR=1300) CHR80
1     FORMAT(A79)
      DOBJ = RHSNAM(:NAMELN)
      DRHS = OBJNAM
      DBND = 'DUAL_BND'
C ROWS SECTION
      CHR80 = 'ROWS'
      WRITE(FILE,1,ERR=1300) CHR80
C
      CHR80 = ' N  '//DOBJ
      WRITE(FILE,1,ERR=1300) CHR80
      NUMBER = 0
C LOOP OVER COLUMNS (DUAL ROWS)
      DO 100 J=1,NCOLS
         CALL GETMAP('COL ',J,SW)
         IF( .NOT.SW )GOTO 100
         CALL GETBND('COL ',J,VL,VU)
         IF( VL.EQ.0. .AND. VL.LT.VU )THEN
C L=0 < U
            CHR80 = ' L'
         ELSE IF( VU.EQ.0. .AND. VL.LT.VU )THEN
C U=0 > L
            CHR80 = ' G'
         ELSE IF( VL.LT.VU .OR. VL.NE.0. )THEN
C EITHER (L < U AND L,U <> 0) OR L=U <> 0
            CHR80 = ' E'
         ELSE
C L=U=0
            CHR80 = ' N'
         ENDIF
         CALL GETNAM('COL ',J,CHR80(5:))
         WRITE(FILE,1,ERR=1300) CHR80
         NUMBER = NUMBER+1
100   CONTINUE
C
      IF( NUMBER.EQ.0 )THEN
         PRINT *,' ** NO ROWS'
         GOTO 1390
      ENDIF
      IF( SWMSG )THEN
         CHR80 = ' '
         RNAME = ' '
         CALL FI2C(RNAME,NUMBER,F)
         CHR80(TABMSG:) = RNAME(:8)//'Dual row'
         CALL FSLEN(CHR80,60,LC)
         IF( NUMBER.GT.1 )THEN
            LC = LC+1
            CHR80(LC:) = 's'
         ENDIF
         PRINT *,CHR80(:LC)
      ENDIF
C
C COLUMNS SECTION
      CHR80 = 'COLUMNS'
      WRITE(FILE,1,ERR=1300) CHR80
C
      NUMBER = 0
      NZDUAL = 0
      DO 500 I=1,NROWS
         CALL GETMAP('ROW ',I,SW)
         IF( .NOT.SW )GOTO 500
         CALL GETYPE(I,CHAR)
         IF( CHAR.EQ.'N' )GOTO 500
C ROW I IS IN SUBMATRIX AND IS NOT A FREE ROW
         CALL GETNAM('ROW ',I,RNAME)
         CALL GETBND('ROW ',I,VL,VU)
         IF( CHAR.EQ.'G' .OR. CHAR.EQ.'E' )THEN
            VB = VL
         ELSE IF( CHAR.EQ.'L' )THEN
            VB = VU
         ELSE
            PRINT *,' ** ROW ',RNAME(:NAMELN),' RANGE NOT ALLOWED'
            GOTO 1390
         ENDIF
         CHR80 = ' '
         CHR80(5:) = RNAME
CC 6-30-95
         IF( NAMELN.LE.10 )THEN
            TABC1 = 15
            TABV1 = 25
            TABC2 = 40
            TABV2 = 50
         ELSE
            TABC1 = 22
            TABV1 = 39
            TABC2 = 60
            TABV2 = TABV1
         ENDIF
CC =======
         IF( ABS(VB).GT.VTOLAB )THEN
CC 6-30-95
            CHR80(TABC1:) = DOBJ
            CALL FR2CLF(CHR80(TABV1:),VB,LV)
            NZDUAL = NZDUAL+1
            TABC = TABC2
            TABV = TABV2
         ELSE
            TABC = TABC1
            TABV = TABV1
CC =======
         ENDIF
         CALL GETROW(I,ISPACE,VSPACE,NSPACE,NZ,RCODE)
         IF( RCODE.NE.0 )GOTO 1390
         NUMBER = NUMBER+1
         IF( NZ.GT.0 )THEN
C LOOP OVER ROW'S NONZEROES (COLS IN ISPACE ARE IN SUBMATRIX)
            DO 300 J=1,NZ
               COL = ISPACE(J)
               CALL GETMAP('COL ',COL,SW)
               IF( .NOT.SW )GOTO 300
               NZDUAL = NZDUAL+1
               CALL GETNAM('COL ',COL,CHR80(TABC:))
               CALL FR2CLF(CHR80(TABV:),VSPACE(J),LV)
               IF( TABV.EQ.TABV2 )THEN
C WRITE 2 NONZEROES (JUST 1 IF NAMELN > 10)
                  WRITE(FILE,1,ERR=1300) CHR80
                  TABC = TABC1
                  TABV = TABV1
                  CHR80(TABC:) = ' '
               ELSE
                  TABC = TABC2
                  TABV = TABV2
               ENDIF
300         CONTINUE
         ENDIF
         IF( TABC.EQ.TABC2 )WRITE(FILE,1,ERR=1300) CHR80
500   CONTINUE
C
      IF( NUMBER.EQ.0 )THEN
         PRINT *,' ** NO COLUMNS'
         GOTO 1390
      ENDIF
      IF( NZDUAL.EQ.0 )THEN
         PRINT *,' ** NO NONZERO COEFFICIENTS'
         GOTO 1390
      ENDIF
      IF( SWMSG )THEN
         RNAME = ' '
         CALL FI2C(RNAME,NUMBER,F)
         CHR80 = ' '
         IF( NUMBER.EQ.1 )THEN
            CHR80(TABMSG:) = RNAME(:8)//'Dual column  with'
         ELSE
            CHR80(TABMSG:) = RNAME(:8)//'Dual columns with'
         ENDIF
         CALL FSLEN(CHR80,70,LC)
         LC = LC+2
         RNAME = ' '
         CALL FI2C(RNAME,NZDUAL,F)
         CHR80(LC+2:) = RNAME(:8)//' nonzero'
         CALL FSLEN(CHR80,75,LC)
         IF( NZDUAL.GT.1 )THEN
            CHR80(LC+1:) = 'es'
            LC = LC+2
         ENDIF
         PRINT *,CHR80(:LC)
      ENDIF
C
      NLO = 0
      NUP = 0
      NFR = 0
      CHR80 = ' '
      SW2   = .FALSE.
C             :...MEANS NAME LENGTH DID NOT HAVE TO INCREASE
C
      DO 700 J=1,NCOLS
         CALL GETMAP('COL ',J,SW)
         IF( .NOT.SW )GOTO 700
         CALL GETNAM('COL ',J,CNAME)
         CALL FSLEN(CNAME,16,LC)
         CALL GETBND('COL ',J,VL,VU)
         IF( VL.GT.-VINF .AND. VL.NE.0. .AND. VL.LT.VU )THEN
C 0 <> L < U AND L FINITE ===> GENERATE COLUMN IN DUAL WITH 1
C                              IN ROW CNAME AND VL IN DUAL OBJ
            IF( LC.LT.NAMELN .OR. NAMELN.LT.10 )THEN
C APPEND l AT END OF COLUMN NAME
               CHR80( 5:) = CNAME(:LC)//'l'
               IF( LC.EQ.NAMELN )SW2 = .TRUE.
               NLO = NLO+1
               CHR80(TABC1:) = CNAME
               CHR80(TABV1:) = ' 1.'
               IF( NAMELN.LE.10 )THEN
                  CHR80(TABC2:) = DOBJ
                  CALL FR2CLF(CHR80(TABV2:),VL,LV)
               ELSE
                  WRITE(FILE,1,ERR=1300) CHR80
                  CHR80(TABC1:) = DOBJ
                  CALL FR2CLF(CHR80(TABV1:),VL,LV)
               ENDIF
               NZDUAL = NZDUAL+2
               WRITE(FILE,1,ERR=1300) CHR80
            ELSE
               PRINT *,' Warning:  Did not generate dual row for ',
     1                  CNAME(:NAMELN),' >=',VL
            ENDIF
         ENDIF
         IF( VU.LT.VINF .AND. VU.NE.0. .AND. VL.LT.VU )THEN
C 0 <> U > L AND U FINITE ===> GENERATE COLUMN IN DUAL WITH -1
C                              IN ROW CNAME AND VU IN DUAL OBJ
            IF( LC.LT.NAMELN .OR. NAMELN.LT.10 )THEN
C APPEND u AT END OF COLUMN NAME
               NUP = NUP+1
               CHR80( 5:)    = CNAME(:LC)//'u'
               CHR80(TABC1:) = CNAME
               CHR80(TABV1:) = '-1.'
               IF( NAMELN.LE.10 )THEN
                  CHR80(TABC2:) = DOBJ
                  CALL FR2CLF(CHR80(TABV2:),-VU,LV)
               ELSE
                  WRITE(FILE,1,ERR=1300) CHR80
                  CHR80(TABC1:) = DOBJ
                  CALL FR2CLF(CHR80(TABV1:),-VU,LV)
               ENDIF
               NZDUAL = NZDUAL+2
               WRITE(FILE,1,ERR=1300) CHR80
            ELSE
               PRINT *,' Warning:  Did not generate dual row for ',
     1                  CNAME(:NAMELN),' <=',VU
            ENDIF
         ENDIF
         IF( VL.GE.VU .AND. VU.NE.0. )THEN
C L=U <> 0 (FREE DUAL COL VAR)
            IF( LC.LT.NAMELN .OR. NAMELN.LT.10 )THEN
C APPEND f AT END OF COLUMN NAME
               NFR = NFR+1
               CHR80( 5:)    = CNAME(:LC)//'f'
               CHR80(TABC1:) = CNAME
               CHR80(TABV1:) = ' 1.'
               IF( NAMELN.LE.10 )THEN
                  CHR80(TABC2:) = DOBJ
                  CALL FR2CLF(CHR80(TABV2:),VU,LV)
               ELSE
                  WRITE(FILE,1,ERR=1300) CHR80
                  CHR80(TABC1:) = DOBJ
                  CALL FR2CLF(CHR80(TABV1:),VU,LV)
               ENDIF
               NZDUAL = NZDUAL+2
               WRITE(FILE,1,ERR=1300) CHR80
            ELSE
               PRINT *,' Warning:  Did not generate dual row for ',
     1                  CNAME(:NAMELN),' free'
            ENDIF
         ENDIF
700   CONTINUE
C
      NUMBER = NUMBER + NLO + NUP + NFR
      IF( SWMSG )THEN
         RNAME = ' '
         CALL FI2C(RNAME,NLO,F)
         CHR80 = ' '
         CHR80(TABMSG:) = '...'//RNAME(F:8)//' L-columns,'
         CALL FSLEN(CHR80,50,LC)
         RNAME = ' '
         CALL FI2C(RNAME,NUP,F)
         CHR80(LC+2:) = RNAME(F:8)//'U-columns'
         CALL FSLEN(CHR80,70,LC)
         RNAME = ' '
         CALL FI2C(RNAME,NFR,F)
         CHR80(LC+2:) = RNAME(F:8)//'free columns'
         CALL FSLEN(CHR80,70,LC)
         PRINT *,CHR80(:LC)
         RNAME = ' '
         CALL FI2C(RNAME,NUMBER,F)
         CHR80(TABMSG:) = RNAME(:8)//' Total dual columns with'
         CALL FSLEN(CHR80,70,LC)
         RNAME = ' '
         CALL FI2C(RNAME,NZDUAL,F)
         CHR80(LC+2:) = RNAME(F:8)//'nonzeroes'
         CALL FSLEN(CHR80,70,LC)
         PRINT *,CHR80(:LC)
         IF( SW2 )THEN
            RNAME = ' '
            CALL FI2C(RNAME,NAMELN+1,F)
            CHR80(TABMSG:) = '...Name length increased to '//RNAME(F:8)
            CALL FSLEN(CHR80,70,LC)
            PRINT *,CHR80(:LC)
         ENDIF
      ENDIF
C
C RHS SECTION
      CHR80 = 'RHS'
      WRITE(FILE,1,ERR=1300) CHR80
C
      CHR80 = ' '
      CHR80(5:) = DRHS
      NUMBER = 0
      CALL GETMAP('ROW ',OBJNUM,SW2)
C SW2 = T IFF OBJ IS IN SUBMATRIX (WILL USE RHS=0 OTHERWISE)
C
      IF( SW2 )THEN
C RHS = C (OBJ IS IN SUBMATRIX)
C   ::: LOOP OVER COLS (DUAL ROWS) :::
         DO 900 J=1,NCOLS
            CALL GETMAP('COL ',J,SW)
            IF( .NOT.SW )GOTO 900
            CALL GETNAM('COL ',J,CNAME)
            CALL GETRIM('COL ',J,VL,VU,VC,NZ)
            IF( ABS(VC).LE.VTOLAB )GOTO 900
            CHR80(TABC1:) = CNAME
            CALL FR2CLF(CHR80(TABV1:),VC,LV)
            WRITE(FILE,1,ERR=1300) CHR80
            NUMBER = NUMBER+1
900      CONTINUE
C    ::: END LOOP OVER COLS :::
         IF( SWMSG )THEN
            RNAME = ' '
            CALL FI2C(RNAME,NUMBER,F)
            CHR80 = ' '
            CHR80(TABMSG:) = RNAME(:8)//' RHS nonzero'
            CALL FSLEN(CHR80,60,LC)
            IF( NUMBER.NE.1 )THEN
               CHR80(LC+2:) = 'es'
               LC = LC+2
            ENDIF
            PRINT *,CHR80(:LC)
         ENDIF
      ELSE IF( SWMSG )THEN
         CHR80(TABMSG:) = ' RHS = 0 (' //OBJNAM
         CALL FSLEN(CHR80,30,LC)
         CHR80(LC+2:) = 'not in submatrix)'
         CALL FSLEN(CHR80,60,LC)
         PRINT *,CHR80(:LC)
      ENDIF
C
      IF( NUMBER.EQ.0 )THEN
C RHS = 0, SO WRITE 0 RHS OF DUAL OBJ FOR MINOS
         CHR80(TABC1:) = DOBJ
         CHR80(TABV1:) = '0.0'
         WRITE(FILE,1,ERR=1300) CHR80
      ENDIF
C
C BOUNDS SECTION
      CHR80 = 'BOUNDS'
      WRITE(FILE,1,ERR=1300) CHR80
      NUMBER = 0
C
      CHR80 = ' '
      DO 1100 I=1,NROWS
         CALL GETMAP('ROW ',I,SW)
         IF( .NOT.SW )GOTO 1100
         CALL GETYPE(I,CHAR)
         IF( CHAR.EQ.'N' )GOTO 1100
         CALL GETNAM('ROW ',I,RNAME)
         IF( ( CHAR.EQ.'G' .AND. OPT.EQ. 1 ) .OR.
     1       ( CHAR.EQ.'L' .AND. OPT.EQ.-1 ) )THEN
C DUAL VAR IS <= 0
            CHR80 = ' MI '
         ELSE IF( CHAR.EQ.'E' )THEN
C DUAL VAR IS FREE
            CHR80 = ' FR '
         ELSE
            GOTO 1100
         ENDIF
         CHR80(5:) = DBND
         CALL GETNAM('ROW ',I,CHR80(TABC1:))
         WRITE(FILE,1,ERR=1300) CHR80
         NUMBER = NUMBER+1
         IF( CHR80(2:3).EQ.'MI' )THEN
            CHR80(2:3) = 'UP'
            CHR80(TABV1:) = '0.'
            WRITE(FILE,1,ERR=1300) CHR80
            NUMBER = NUMBER+1
         ENDIF
1100  CONTINUE
C
      IF( NFR.GT.0 )THEN
C FREE DUAL COLUMNS (L=U <> 0)
         CHR80 = ' FR '//DBND
         DO 1200 J=1,NCOLS
            CALL GETMAP('COL ',J,SW)
            IF( .NOT.SW )GOTO 1200
            CALL GETBND('COL ',J,VL,VU)
            IF( VL.GE.VU .OR. VL.EQ.0. )GOTO 1200
            CALL GETNAM('COL ',J,CHR80(TABC1:))
            WRITE(FILE,1,ERR=1300) CHR80
            NUMBER = NUMBER+1
            IF( NFR.EQ.1 )GOTO 1210
            NFR = NFR-1
1200     CONTINUE
1210     CONTINUE
      ENDIF
C
      IF( SWMSG )THEN
         RNAME = ' '
         CALL FI2C(RNAME,NUMBER,F)
         CHR80 = ' '
         CHR80(TABMSG:) = RNAME(:8)//'Dual bound'
         CALL FSLEN(CHR80,70,LC)
         IF( NUMBER.NE.1 )THEN
            LC = LC+1
            CHR80(LC:) = 's'
         ENDIF
         PRINT *,CHR80(:LC)
      ENDIF
C ENDATA
      CHR80 = 'ENDATA'
      WRITE(FILE,1,ERR=1300) CHR80
      GOTO 9000
C
C ALL ERROR RETURNS COME HERE
1300  PRINT *,' ** IO ERROR WRITING MATRIX FILE'
1390  RCODE = 1
      PRINT *,' ...WRITE DUAL MATRIX FILE ABORTED'
C
C ALL RETURNS COME HERE
9000  CLOSE(FILE)
      RETURN
C
C ** GWRDUL ENDS HERE
      END
