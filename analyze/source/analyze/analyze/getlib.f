C             ::: GETLIB.FOR  8-24-95 :::
C
C Earlier dates deleted
C       9-20-92...Added GETSTP
C       3-12-93...Modified GETSTP (Removed GBPROD...See NEW)
C       8-11-95...Changed VP=OPT to VP=-1 in GETSOL
C
C This contains the following GETMAT subroutines.
C
C     NAME.....FUNCTION that gives row/column name from element number
C     GPTNAM...puts row/column name
C     GETNUM...gets number from name
C     GETNAM...gets name from number
C     GETBND...gets bound values
C     GETRIM...gets rim values
C     GETYPE...gets type of row (for MPS format)
C     GETSOL...gets solution values of row or col
C     GETST....gets solution status of row or col
C     GETSTP...gets solution type (overall)...Added 3-12-93
C     GETFNM...gets first name match
C     GETCOL...gets column nonzeroes
C     GETROW...gets row nozeroes
C     GETSGN...gets sign of nonzero
C     GETAIJ...gets value of nonzero
C     GETVID...gets value index (adds to pool)
C     GETVAL...gets value from index
C     GETBVP...gets basic variable from pivot position
C     GETPIV...gets pivot position from basic variable
C     GPRDCT...gets inner product of two vectors
C
      FUNCTION NAME(LOC)
C     =============
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gives row/column name for K=1,...,NROWS+NCOLS
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      NAME = ' '
      IF(LOC.EQ.0)RETURN
      I = (LOC-1)*NAMELN
      DO 10 K=1,NAMELN
10    NAME(K:K) = NAMERC(I+K)
      RETURN
C
C ** NAME FUNCTION ENDS HERE
      END
      SUBROUTINE GPTNAM(NUMBER,CNAME)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This puts name of row/column (CNAME) as (NUMBER)
C
      CHARACTER*(*)  CNAME
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C SET LOCATION (LOC) IN NAME ARRAY
      LOC = (NUMBER-1)*NAMELN
C LOOP TO COPY CHARS OF NAME
      DO 10 K=1,NAMELN
10    NAMERC(LOC+K) = CNAME(K:K)
      RETURN
C
C ** GPTNAM ENDS HERE
      END
      SUBROUTINE GETNUM(ROWCOL,CNAME,NUMBER)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets NUMBER of ROWCOL CNAME ...NUMBER=0 if not found
C Binary search used (presumes names sorted)
C
         CHARACTER*4   ROWCOL
         CHARACTER*(*) CNAME
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C SET POINTERS FOR ROW VS. COL
         IF(ROWCOL.EQ.'ROW ')THEN
                IF(CNAME.EQ.OBJNAM)THEN
                   NUMBER = OBJNUM
                   RETURN
                ENDIF
                INAME =IRNAME
                NHIGH =NROWS
         ELSE IF(ROWCOL.EQ.'COL ')THEN
                INAME =ICNAME
                NHIGH =NCOLS
         ELSE
                PRINT *,' ** SYSERR GNUM',ROWCOL,'...please report'
                NUMBER = 0
                RETURN
         ENDIF
C
         NLOW =1
C
C LOOP FOR BINARY SEARCH
100      CONTINUE
         NUMBER=(NLOW+NHIGH)/2
C
         IF(NAME(INAME+NUMBER).EQ.CNAME)THEN
                RETURN
         ELSE IF(NAME(INAME+NUMBER).LT.CNAME)THEN
C MOVE DOWN
                NLOW=NUMBER+1
         ELSE
C MOVE UP
                NHIGH=NUMBER-1
         ENDIF
      IF(NLOW.LE.NHIGH)GOTO 100
C     ~~~~~~~~~~~~~~~~~~~~~~~~~  NOT FOUND
      NUMBER=0
      RETURN
C
C ** GETNUM ENDS HERE
      END
      SUBROUTINE GETNAM(ROWCOL,NUMBER,CNAME)
C     ==================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets name (CNAME) from NUMBER
C
      CHARACTER*4 ROWCOL
      CHARACTER*(*) CNAME
C :::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C LOCATION IN NAME ARRAY DEPENDS ON ROW VS. COL
      IF(ROWCOL.EQ.'ROW ')THEN
             CNAME=NAME(IRNAME+NUMBER)
      ELSE IF(ROWCOL.EQ.'COL ')THEN
             CNAME=NAME(ICNAME+NUMBER)
      ELSE
             PRINT *,' ** SYSERR GETNAM ',ROWCOL,'...please report'
             CNAME = '**SYSERR'
      ENDIF
C
      RETURN
C
C ** GETNAM ENDS HERE
      END
      SUBROUTINE GETBND(ROWCOL,NUMBER,VL,VU)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets bounds:  VL,VU for row/column NUMBER
C
      CHARACTER*4 ROWCOL
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C LOCATIONS DEPEND UPON ROW VS. COL
      IF(ROWCOL.EQ.'ROW ')THEN
                CALL GETVAL(VL,IRLBND+NUMBER)
                CALL GETVAL(VU,IRUBND+NUMBER)
      ELSE IF(ROWCOL.EQ.'COL ')THEN
                CALL GETVAL(VL,ICLBND+NUMBER)
                CALL GETVAL(VU,ICUBND+NUMBER)
      ELSE
                PRINT *,' ** SYSERR GB',ROWCOL,'...please report'
      ENDIF
C
      RETURN
C
C ** GETBND ENDS HERE
      END
      SUBROUTINE GETRIM(ROWCOL,NUMBER,VL,VU,VC,NZ)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets rim for row/col NUMBER...
C
C       VL = Lower bound
C       VU = Upper bound
C       VC = Objective value
C       NZ = Number of nonzeroes
C
         CHARACTER*4 ROWCOL
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C BOUNDS
         CALL GETBND(ROWCOL,NUMBER,VL,VU)
C OBJ
         IF(ROWCOL.EQ.'ROW ')THEN
                IF(NUMBER.EQ.OBJNUM)THEN
                  VC=OPT
                ELSE
                  VC=0.0
                ENDIF
                NZ=INDEX(IRINFO+NUMBER)
         ELSE IF(ROWCOL.EQ.'COL ')THEN
                VC=0.0
                I1=INONZ + 2*BASE(ICINFO+NUMBER-1)
                I2=INONZ + 2*BASE(ICINFO+NUMBER) - 1
                DO 200 I=I1,I2,2
                   IF(INDEX(I).EQ.OBJNUM)THEN
                      CALL GETVAL(VC,I+1)
                      GOTO 210
                   ENDIF
200             CONTINUE
210             CONTINUE
                NZ=BASE(ICINFO+NUMBER)-BASE(ICINFO+NUMBER-1)
         ELSE
                PRINT *,' ** SYSERR GRM',ROWCOL,'...please report'
         ENDIF
         RETURN
C
C ** GETRIM ENDS HERE
      END
      SUBROUTINE GETYPE(NUMBER,CHAR)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gives type of row (NUMBER) ===> CHAR
      CHARACTER*1 CHAR
C
C       BOUNDS
C       LO  UP      TYPE
C       ~~~~~~~~~~~~~~~~
C       -*   *       N  null (free row)
C       -*   u       L  <= u
C        v   *       G  >= v
C        v = u       E   = v
C        v < u       R  >= v and <= u
C       ~~~~~~~~~~~~~~~~
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
C GET ROW BOUNDS (VL,VU)
      CALL GETBND('ROW ',NUMBER,VL,VU)
C SET TYPE ACCORDING TO TABLE
      IF(VL.LE.-VINF)THEN
         IF(VU.GE.VINF)THEN
            CHAR = 'N'
         ELSE
            CHAR = 'L'
         ENDIF
      ELSE IF(VL.GE.VU)THEN
         CHAR = 'E'
      ELSE IF(VU.GE.VINF)THEN
         CHAR = 'G'
      ELSE
         CHAR = 'R'
      ENDIF
C
      RETURN
C
C ** GETYPE ENDS HERE
      END
      SUBROUTINE GETSOL(ROWCOL,NUMBER,VX,VP,STAT,STNUM)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets solution values and status...
C
C       VX      = Level
C       VP      = Price
C       STAT    = Status (L,U,B,I)
C       STNUM   =         1,2,3,4
C
C
      CHARACTER*4 ROWCOL
      CHARACTER*1 STAT
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C GET STATUS OF ROWCOL NUMBER
      CALL GETST(ROWCOL,NUMBER,STAT,STNUM)
C
      IF(ROWCOL.EQ.'ROW ')THEN
             IF(STAT.EQ.'L')THEN
                  CALL GETVAL(VX,IRLBND + NUMBER)
                  VP = VALUE(IRSOLV + NUMBER)
             ELSE IF(STAT.EQ.'U')THEN
                  CALL GETVAL(VX,IRUBND + NUMBER)
                  VP = VALUE(IRSOLV + NUMBER)
             ELSE IF(STAT.EQ.'B')THEN
                  VX = VALUE(IRSOLV + NUMBER)
                  IF(NUMBER.EQ.OBJNUM.AND.SOLST.NE.'INFEASIBLE')THEN
CC 8-24-95                   VP = OPT
                             VP = -1.
                  ELSE
                             VP = 0.0
                  ENDIF
             ELSE
                  VX = VALUE(IRSOLV + NUMBER)
                  CALL GETBND(ROWCOL,NUMBER,VL,VU)
                  IF(VX.LT.VL)THEN
                             VP = 1.0
                  ELSE
                             VP =-1.0
                  ENDIF
             ENDIF
      ELSE IF(ROWCOL.EQ.'COL ')THEN
             IF(STAT.EQ.'L')THEN
                   CALL GETVAL(VX,ICLBND + NUMBER)
                   VP = VALUE(ICSOLV + NUMBER)
             ELSE IF(STAT.EQ.'U')THEN
                   CALL GETVAL(VX,ICUBND + NUMBER)
                   VP = VALUE(ICSOLV + NUMBER)
             ELSE IF(STAT.EQ.'B')THEN
                   VX = VALUE(ICSOLV + NUMBER)
                   VP = 0.0
             ELSE
                   VX = VALUE(ICSOLV + NUMBER)
                   CALL GETBND(ROWCOL,NUMBER,VL,VU)
                   IF(VX.LT.VL)THEN
                             VP =  1.0
                   ELSE
                             VP = -1.0
                   ENDIF
             ENDIF
      ELSE
             PRINT *,' ** SYSERR GSL',ROWCOL,'...please report'
      ENDIF
C
      RETURN
C
C ** GETSOL ENDS HERE
      END
      SUBROUTINE GETST(ROWCOL,NUMBER,STAT,STNUM)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets status (L,U,B,I) of row/col NUMBER
C
       CHARACTER*4 ROWCOL
       CHARACTER*1 STAT
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C LOCATION DEPENDS UPON ROW VS. COL
       IF(ROWCOL.EQ.'ROW ')THEN
             STNUM=INDEX(IRSTAT+NUMBER)
       ELSE IF(ROWCOL.EQ.'COL ')THEN
             STNUM=INDEX(ICSTAT+NUMBER)
       ELSE
             PRINT *,' ** SYSERR GST',ROWCOL,'...please report'
             STAT = '*'
             RETURN
       ENDIF
C
C UNPACK STAT (bottom 3 bits have status code)
C
       IF(STNUM.LT.0)STNUM = -STNUM
       STNUM=STNUM - (STNUM/8)*8
       STAT=CHARST(STNUM)
C
       RETURN
C
C ** GETST ENDS HERE
      END
      SUBROUTINE GETSTP(BASIC,COMPL)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets solution type:  COMPL = S | N | U | ?
C                                   :   :   :   :...Not set
C                                   :   :   :...Unknown
C                                   :   :...Not strictly complementary
C                                   :...Strictly complementary
C                           BASIC = B | N | U | ?
C                                   :   :   :   :...Not set
C                                   :   :   :...Unknown
C                                   :   :...Not basic
C                                   :...Basic
C IF STCOMP=0 UPON ENTRANCE (NOT SET), IT IS SET HERE.
C
       CHARACTER*(*) COMPL,BASIC
       CHARACTER*1  STRCMP(0:3),  STAT,      STRBAS(0:3)
       DATA         STRCMP/'?','S','N','U'/, STRBAS/'?','B','N','U'/
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      IF( IPIVOT.GT.0 )STBASC = 1
      IF( STCOMP.GT.0 )GOTO 900
C STCOMP NOT SET
      IF( SOLST.EQ.'INFEASIBLE' )THEN
         STCOMP = 2
         GOTO 900
      ENDIF
C SET COMPLEMENTARITY STATUS...ASSUME STRICTLY COMP. AND LOOP
      STCOMP = 1
C ...ALSO TALLY DIMENSION TO SEE IF NOT BASIC
      NUMBER = 0
      DO 100 I=1,NROWS
         CALL GETSOL('ROW ',I,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'B' )THEN
            IF( I.EQ.OBJNUM )GOTO 100
            CALL GETBND('ROW ',I,VL,VU)
            IF( VX.GT.VL .AND. VX.LT.VU )GOTO 100
         ELSE
            NUMBER = NUMBER+1
            IF( ABS(VP).GE.VTOLAB )GOTO 100
         ENDIF
C NOT STRICTLY COMPLEMENTARY
         STCOMP = 2
         IF( STBASC.GT.0 )GOTO 900
C ...KEEP GOING TO GET DIMENSION FOR SETTING STBASC
100   CONTINUE
C
C ::: NOW CHECK COLUMNS :::
      DO 200 I=1,NCOLS
         CALL GETSOL('COL ',I,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'B' )THEN
            NUMBER = NUMBER-1
            CALL GETBND('COL ',I,VL,VU)
            IF( VX.GT.VL .AND. VX.LT.VU )GOTO 200
         ELSE
            IF( ABS(VP).GE.VTOLAB )GOTO 200
         ENDIF
C NOT STRICTLY COMPLEMENTARY
          STCOMP = 2
          IF( STBASC.GT.0 )GOTO 900
200   CONTINUE
C
C IF WE GET HERE, NUMBER = # NONBASIC ROWS - # BASIC COLS
      IF( NUMBER.NE.0 )STBASC = 2
C ============================
900    CONTINUE
C ======== SET STATS =========
       COMPL = STRCMP(STCOMP)
       BASIC = STRBAS(STBASC)
       RETURN
C
C ** GETSTP ENDS HERE
      END
      SUBROUTINE GETFNM(ROWCOL,CNAME,NUMBER)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets first name match of CNAME, returning NUMBER
C
C ...NUMBER=0 if no match
C
      CHARACTER*4   ROWCOL
      CHARACTER*16  RNAME
      CHARACTER*(*) CNAME
      LOGICAL*1     ANSWER
C LOCAL
      CHARACTER*1   SMCHAR
      DATA          SMCHAR /'"'/
C
C CALLS FMATCH (WHICH MUST HAVE SAME SLIDING MASK CHAR)
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C Replace MASK character in CNAME with blank (lower ASCII code)
C
       RNAME=CNAME
       DO 10 I=1,16
10     IF(RNAME(I:I).EQ.GMASK)RNAME(I:I)=' '
C
C Use binary search in names (RNAME has adjusted mask...blanks replace *)
C
       IF(ROWCOL.EQ.'ROW ')THEN
                IBASE = IRNAME
                MAXNUM= NROWS
       ELSE IF(ROWCOL.EQ.'COL ')THEN
                IBASE = ICNAME
                MAXNUM= NCOLS
       ELSE
                PRINT *,' ** SYSERR GFN',ROWCOL,'...please report'
                NUMBER = 1
                RETURN
       ENDIF
C
       NLOW = 1
C SEE IF RNAME HAS A SLIDING MASK
       CALL FLOOKF(RNAME,1,16,SMCHAR,I)
       IF(I.GT.0)GOTO 190
C         :...MEANS RNAME CONTAINS SMCHAR...
C  CANNOT USE BINARY SEARCH FOR FLOOR WITH A SLIDING MASK
C
C RNAME DOES NOT CONTAIN SLIDING MASK, SO PROCEED WITH BINARY SEARCH
       NHIGH=MAXNUM
C
C   TOP OF BINARY SEARCH LOOP
100    CONTINUE
         NUMBER=(NLOW+NHIGH)/2
         IF(NAME(IBASE+NUMBER).EQ.RNAME)THEN
                RETURN
         ELSE IF(NAME(IBASE+NUMBER).LT.RNAME)THEN
                NLOW=NUMBER+1
         ELSE
                NHIGH=NUMBER-1
         ENDIF
      IF(NHIGH.GT.NLOW)GOTO 100
C
190   CONTINUE
C
C FLOOR FOUND...BEGIN LINEAR SEARCH (CNAME has original mask)
C
      DO 200 NUMBER=NLOW,MAXNUM
          CALL FMATCH(CNAME,NAME(IBASE+NUMBER),GMASK,ANSWER)
          IF(ANSWER)RETURN
200   CONTINUE
C NO MATCH
      NUMBER=0
      RETURN
C
C ** GETFNM ENDS HERE
      END
      SUBROUTINE GETCOL(J,NZ,MAXLST,ROWLST,VALLST)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets nonzeroes of column J
C
C       MAXLST  = Length of ROWLST and VALLST
C       NZ      = Number of nonzeroes returned
C                  ...> MAXLST means truncated
C       ROWLST = List of row indexes
C       VALLST = List of values
C
      DIMENSION ROWLST(2), VALLST(2)
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C SET NUMBER OF NONZEROES (NZ)
      NZ = BASE(ICINFO + J) - BASE(ICINFO + J-1)
      IF(NZ.EQ.0)RETURN
C
      I1=INONZ + 2*BASE(ICINFO+J-1)
      IF(NZ.GT.MAXLST)THEN
            I2 = I1 + 2*MAXLST - 1
      ELSE
            I2 = I1 + 2*NZ - 1
      ENDIF
C
            K=0
      DO 100 I=I1,I2,2
            K=K+1
            ROWLST(K)=INDEX(I)
            CALL GETVAL(VALLST(K),I+1)
100   CONTINUE
      RETURN
C
C ** GETCOL ENDS HERE
      END
      SUBROUTINE GETROW(I,JLIST,VLIST,MAX,NZ,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
      DIMENSION  JLIST(MAX), VLIST(MAX)
C
C This gets nonzeroes in row I of submatrix columns
C
C    NZ = Number of nonzeroes returned = size of JLIST,VLIST
C    RCODE = 1 means NZ = MAX and there are more nonzeroes
C
C LOCAL
      LOGICAL*1  SW
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C INITIALIZE NUMBER OF NONZEROES (NZ)
      NZ = 0
C ...NONZERO POINTER
      I1 = INONZ + 2*BASE(ICINFO)
C
C ::: LOOP OVER COLUMNS :::
C
      DO 900 J=1,NCOLS
         NZ OF J = BASE(ICINFO + J) - BASE(ICINFO + J-1)
         I2 = I1 + 2*NZ OF J - 1
C SEE IF COLUMN J IS IN SUBMATRIX
         CALL GETMAP('COL ',J,SW)
         IF(.NOT.SW)GOTO 800
C OK, IT IS
C    ::: LOOP OVER NONZEROES OF COLUMN J TO SEE IF ROW I IS THERE :::
         DO 500 K=I1,I2,2
            IF(INDEX(K).EQ.I)GOTO 600
500      CONTINUE
         GOTO 800
C
600      CONTINUE
C WE HAVE NONZERO AT (I,J)...ADD TO LIST
         IF(NZ.EQ.MAX)THEN
            RCODE = 1
            RETURN
         ENDIF
         NZ = NZ + 1
         JLIST(NZ) = J
         CALL GETVAL(VLIST(NZ),K+1)
800      CONTINUE
C PREPARE FOR NEXT COLUMN
         I1 = I2 + 1
900   CONTINUE
C
      RETURN
C
C ** GETROW ENDS HERE
      END
      SUBROUTINE GETSGN(ROW,COLUMN,CHAR)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets CHARacter of nonzero in ROW,COLUMN
C
C ...blank if nonzero is absent...0 if a zero is there
C
      CHARACTER*1 CHAR
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C INITIALIZE
      CHAR = ' '
      I1 = INONZ + 2*BASE(ICINFO + COLUMN - 1)
      I2 = INONZ + 2*BASE(ICINFO + COLUMN) - 1
C
      IF(I2.LE.I1)RETURN
C
C LOOP OVER COLUMN'S NONZEROES TO SEE IF ROW IS PRESENT
C
      DO 100 I=I1,I2,2
           IF(INDEX(I).EQ.ROW)THEN
              IF(INDEX(I+1).LT.0)THEN
                             CHAR = '-'
              ELSE IF(INDEX(I+1).GT.0)THEN
                             CHAR = '+'
              ELSE
                             CHAR = '0'
              ENDIF
              RETURN
           ENDIF
100   CONTINUE
C
      RETURN
C
C ** GETSGN ENDS HERE
      END
      SUBROUTINE GETAIJ(ROW,COLUMN,V)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets Value of nonzero in ROW,COLUMN:  V = A(ROW,COLUMN)
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      I1 = INONZ + 2*BASE(ICINFO + COLUMN - 1)
      I2 = INONZ + 2*BASE(ICINFO + COLUMN) - 1
C LOOP OVER COLUMN'S NONZEROES TO SEE IF ROW IS PRESENT
      DO 100 I=I1,I2,2
         IF( INDEX(I).EQ.ROW )THEN
C GOTCHA
            CALL GETVAL(V,I+1)
            RETURN
         ENDIF
100   CONTINUE
C
C ROW NOT IN COL'S LIST, SO COEF = 0
      V = 0.
      RETURN
C
C ** GETAIJ ENDS HERE
      END
      SUBROUTINE GETVID(V,ID)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C Get value index (ID) for value (V > 0)
C
C ID = 0 if no more space;  else, ID will be in range 1-NVALS
C       and VALUE(ID)=V
C
C WE USE THE (LESS EFFICIENT) METHOD OF DIVISION BECAUSE MACHINES
C NUMBER BYTES DIFFERENTLY...PREVIOUS VERSION NEEDED BOTTOM(i) TO
C USE EVEN i FOR SOME MACHINES AND ODD i FOR OTHERS.  THE PRESENT
C METHOD WAS SUGGESTED BY MILTON M. GUTTERMAN, AMOCO OIL CO.
C
         INTEGER*4  WORD
         EQUIVALENCE (VLOCAL,WORD)
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C
C ~~~~~~~~~~~~~~~~~~ DATA STRUCTURE ~~~~~~~~~~~~~~~~~~~~~~~~~~~
C       The middle segment of the VALUE array (EQUIVALENCEd in
C       DCGETMAT to I4VAL) is used to hold list heads and
C                    :...CHANGED FROM I2, 6-22-92
C       links during value pool creation.
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C REMOVED 6-22-92:         HEAD  = 2*MAXVPL + 1
         HEAD  = MAXVPL + 1
         LINK  = HEAD + 255
C
C HEAD holds list heads into pool;  it is the range of the
C       hash function (0-255)
C
C LINK is parallel to the value pool;  it holds links of values
C       with the same hash value (ie, in same list)
C
C ----------------------- PROCEDURE --------------------------------
C
C       Hash V by the arithmetic differencing of its 4 bytes into
C       an integer 0-255.  This determines the list to which V
C       belongs.  If the list is empty, V must be added to the
C       pool;  else, traverse the list (using links) to find a
C       resident value within absolute tolerance.  If one is
C       found, its index value is returned;  else, V must be added.
C       to pool.
C
C       If V must be added to pool, space is checked first.  If no
C       more space is available, 0 is returned;  else, V is added
C ------------------------------------------------------------------
C COPY V TO VLOCAL SO WE CAN USE ITS INTEGER VALUE (WORD)
      VLOCAL=V
C NOW WE EXTRACT THE 4 BYTES AS INTEGER VALUES
      BYTE3 = WORD/256
      BYTE4 = WORD - 256*BYTE3
      BYTE2 = BYTE3/256
      BYTE3 = BYTE3 - 256*BYTE2
      BYTE1 = BYTE2/256
      BYTE2 = BYTE2 - 256*BYTE1
C WE HAVE NOW SEPARATED THE 4 BYTES OF V INTO 4 INTEGER VALUES, EACH
C BETWEEN 0 AND 255...COMPUTE LIST (HASH) VALUE SIMILAR TO XOR
C
      LIST = BYTE1 - BYTE2 + BYTE3 - BYTE4
      LIST = LIST - 256*(LIST/256)
      IF(LIST.LT.0)LIST = 256 + LIST
C
C Now LIST = HASH VALUE (0-255) INTO HEAD
C
C TRAVERSE LIST                 NOTE: I4VAL REPLACED I2VAL
         ID=I4VAL(HEAD+LIST)
100      IF(ID.GT.0)THEN
             IF(ABS(V-VALUE(ID)).LE.VTOLAB + VTOLRE*V)RETURN
             ID=I4VAL(LINK+ID)
             GOTO 100
         ENDIF
C NEW ENTRANT...CHECK FOR SPACE
         IF(NVALS.EQ.MAXVPL)THEN
              ID=0
         ELSE
C OK, ADD V
              NVALS=NVALS+1
              ID=NVALS
              VALUE(NVALS)=V
              I4VAL(LINK+NVALS)=I4VAL(HEAD+LIST)
              I4VAL(HEAD+LIST)=ID
         ENDIF
C
      RETURN
C
C ** GETVID ENDS HERE
      END
      SUBROUTINE GETVAL(V,ID)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets value (V) from it INDEX(ID)
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      I = INDEX(ID)
      IF(I.GE.0)THEN
          V = VALUE(I)
      ELSE
          V = -VALUE(-I)
      ENDIF
C
      RETURN
C
C ** GETVAL ENDS HERE
      END
      SUBROUTINE GETBVP(ROW,ROWCOL,NUMBER)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gives ROWCOL NUMBER for pivot position ROW
C ...ASSUMES IPIVOT > 0 (ie, caller must know pivot info is stored)
C
      CHARACTER*4 ROWCOL
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C SET NUMBER = PIVOT INDEX FOR ROW
      NUMBER = INDEX(IPIVOT + ROW)
      IF(NUMBER.GT.0)THEN
           ROWCOL = 'COL '
      ELSE
           ROWCOL = 'ROW '
           NUMBER = -NUMBER
      ENDIF
C
      RETURN
C
C ** GETBVP ENDS HERE
      END
      SUBROUTINE GETPIV(ROW,ROWCOL,NUMBER)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gives pivot position (ROW) for ROWCOL NUMBER (basic)
C ...ASSUMES IPIVOT > 0 (ie, caller must know pivot info is stored)
C ...ROW = 0 IF ROWCOL NUMBER NOT FOUND
C
      CHARACTER*4 ROWCOL
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C LOOP OVER ROWS TO FIND PIVOT OF NUMBER
      DO 100 ROW=1,NROWS
         PIVOT = INDEX(IPIVOT + ROW)
         IF( PIVOT.EQ.NUMBER .AND. ROWCOL.EQ.'COL ')RETURN
         IF(-PIVOT.EQ.NUMBER .AND. ROWCOL.EQ.'ROW ')RETURN
100   CONTINUE
C
      ROW = 0
C
      RETURN
C
C ** GETPIV ENDS HERE
      END
      SUBROUTINE GPRDCT(VECTOR,COLUMN,VPROD)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C Get VPROD = VECTOR*A(COLUMN)
C
      DIMENSION VECTOR(100)
C
C ASSUMES CALLER HAS VECTOR LONG ENOUGH FOR NROWS
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C INITIALIZE
      ZPROD = 0.0
      I1 = INONZ + 2*BASE(ICINFO+COLUMN-1)
      I2 = INONZ + 2*BASE(ICINFO+COLUMN) - 1
C
      DO 100 I=I1,I2,2
           ROW = INDEX(I)
           CALL GETVAL(V,I+1)
           ZPROD = ZPROD + V*VECTOR(ROW)
100   CONTINUE
C
      VPROD = ZPROD
      RETURN
C
C ** GPRDCT ENDS HERE
      END
