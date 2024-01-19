C               ::: AEDIT.FOR  8-22-95 :::
C
C Earlier dates deleted
C       5-29-95...Cosmetic change to AVERFY
C       7-17-95...SUB C * in AVERFY changed to SUB COL *
C       7-29-95...Cosmetic changes to AVERFY + submat set
C       7-30-95...Added SWMSG to GVERFY; Added ABRVER and AKEYS0
C       8-14-95...Added VTOL0C to GVERFY args
C
C This contains the following subroutines for ANALYZE
C
C    ANFREE...free bound/limit
C    ANFIX....fix level
C    AROUND...rounds level
C    AVERFY...verifies solution values
C    ABRVER...reports results from BASIS REFRESH and VERIFY
C    AKEYS0...initialize keyworded options (TIME and tolerances)
C    AKEYIN...parse for certain keys
C
      SUBROUTINE ANFREE(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This edits LP
C       Syntax:  FREE {LOWER,UPPER} [{ROW | COL} [cond]]
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*4   ROWCOL
      CHARACTER*8   OPTION(2),WHAT
      LOGICAL*1     PICK(2),SW,SWOK
      DATA OPTION/'LOWER ','UPPER '/
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C GET BOUND/LIMIT OPTION
      NUMBER = 2
      RCODE = -1
      CALL FOPSET(CLIST,FIRST,LAST,OPTION,NUMBER,PICK,RCODE)
      IF(RCODE.NE.0)RETURN
C SET ROW|COLUMN [conditional]....USING ADDRIM
      CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
      IF(RCODE.NE.0)RETURN
C NOW SUBMATRIX IS SET AND PICK(1) = TRUE IFF FREE LOWER
C                          PICK(2) = TRUE IFF FREE UPPER
C PICK(1) .OR. PICK(2) IS TRUE
      IF(.NOT.PICK(1))THEN
         WHAT = 'UPPER'
      ELSE IF(.NOT.PICK(2))THEN
         WHAT = 'LOWER'
      ELSE
         WHAT = 'BOTH '
      ENDIF
C
C SET ROWCOL
      IF(SWDISP)THEN
         ROWCOL = 'COL '
         NRC = NRCSUB(2)
         I1  = RCSUB1(2)
         I2  = NCOLS
      ELSE
         ROWCOL = 'ROW '
         NRC = NRCSUB(1)
         I1  = RCSUB1(1)
         I2  = NROWS
      ENDIF
      IF(NRC.EQ.0)THEN
         IF(SWMSG)THEN
            IF(ROWCOL.EQ.'ROW ')THEN
               PRINT *,' There are no rows in submatrix'
            ELSE
               PRINT *,' There are no columns in submatrix'
            ENDIF
         ENDIF
         RETURN
      ENDIF
C
      SWOK = .TRUE.
C  ::: LOOP OVER SUBMATRIX :::
C
      DO 500 NUMBER=I1,I2
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF(.NOT.SW)GOTO 500
C ROWCOL (NUMBER) IS IN SUBMATRIX
         NRC = NRC - 1
         CALL GETFRE(WHAT,ROWCOL,NUMBER,RCODE)
         IF( RCODE.NE.0 )THEN
            SWOK = .FALSE.
            RCODE = 0
         ENDIF
         IF( NRC.EQ.0 )GOTO 510
500   CONTINUE
C  ::: END LOOP OVER SUBMATRIX :::
C
510   CONTINUE
      IF( .NOT.SWOK )THEN
         PRINT *,' Warning:  Solution corrupted.'
         SWRATE = .FALSE.
         SOLST = 'Unknown'
      ENDIF
C NRFREE MIGHT HAVE CHANGED
      CALL GETBAS(PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,SOLST,SOLNAM,
     2  NROWS,NCOLS,NONZER,NVALS,NONES,OBJNUM,
     3  NRFREE,NCFIX,MAXRLN,MAXCLN,NAMELN,VINF,VTOLAB,VTOLRE)
C
      RETURN
C
C ** ANFREE ENDS HERE
      END
      SUBROUTINE ANFIX(CLIST,FIRST,LAST,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This edits LP
C       Syntax:  FIX value [{ROW | COL} [cond]]
C
      CHARACTER*128 CLIST
C LOCAL
      LOGICAL*1     SW
      CHARACTER*1   CHAR
      CHARACTER*4   ROWCOL
      CHARACTER*12  STRVAL
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C GET VALUE
      CALL FTOKEN(CLIST,FIRST,LAST,STRVAL,12,CHAR)
      CALL FC2R(STRVAL,V,RCODE)
      IF( RCODE.NE.0 )RETURN
C SET ROW|COLUMN [conditional]....USING ADDRIM
      CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
      IF(RCODE.NE.0)RETURN
C SET ROWCOL
      IF(SWDISP)THEN
         ROWCOL = 'COL '
         NRC = NRCSUB(2)
         I1  = RCSUB1(2)
         I2  = NCOLS
      ELSE
         ROWCOL = 'ROW '
         NRC = NRCSUB(1)
         I1  = RCSUB1(1)
         I2  = NROWS
      ENDIF
      IF(NRC.EQ.0)THEN
         IF( SWMSG )THEN
            IF(ROWCOL.EQ.'ROW ')THEN
               PRINT *,' There are no rows in submatrix'
            ELSE
               PRINT *,' There are no columns in submatrix'
            ENDIF
         ENDIF
         RETURN
      ENDIF
C
C  ::: LOOP OVER SUBMATRIX :::
C
      DO 500 NUMBER=I1,I2
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF( .NOT.SW )GOTO 500
C ROWCOL (NUMBER) IS IN SUBMATRIX
         NRC = NRC - 1
         CALL GETFIX(ROWCOL,NUMBER,V,RCODE)
         IF( RCODE.NE.0 )GOTO 900
         IF( NRC.EQ.0 )GOTO 510
500   CONTINUE
C  ::: END LOOP OVER SUBMATRIX :::
C
510   CONTINUE
      IF( SWMSG )PRINT *,' Warning: solution may be corrupted',
     1           '...use BASIS REFRESH'
C NCFIX AND BNDNAM MIGHT HAVE CHANGED
900   CALL GETBAS(PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,SOLST,SOLNAM,
     2  NROWS,NCOLS,NONZER,NVALS,NONES,OBJNUM,
     3  NRFREE,NCFIX,MAXRLN,MAXCLN,NAMELN,VINF,VTOLAB,VTOLRE)
      RETURN
C
C ** ANFIX ENDS HERE
      END
      SUBROUTINE AROUND(CLIST,FIRST,LAST,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This edits LP
C       Syntax:  ROUND [cond] [//TRUNCATE]
C
      CHARACTER*128 CLIST
C LOCAL
      LOGICAL*1     SW
      CHARACTER*1   CHBND
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      IF( FIRST.LE.LAST )THEN
C SET SUBMATRIX
        FIRST = FIRST-2
        CLIST(FIRST:FIRST) = 'C'
        CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
        IF( RCODE.NE.0 )RETURN
      ELSE
        SWDISP = .TRUE.
      ENDIF
C
C  ::: LOOP OVER SUBMATRIX COLS :::
C
      NRC = NRCSUB(2)
      IF( NRC.EQ.0 )THEN
         IF( SWMSG )PRINT *,' NO COLUMNS IN SUBMATRIX'
         RETURN
      ENDIF
      NUM = 0
      DO 500 COL=RCSUB1(2),NCOLS
         CALL GETMAP('COL ',COL,SW)
         IF( .NOT.SW )GOTO 500
C COL IS IN SUBMATRIX
         NRC = NRC - 1
         CALL GROUND(COL,CHBND,VBND,RCODE)
         IF( RCODE.NE.0 )GOTO 510
         IF( CHBND.NE.' ' )NUM = NUM+1
         IF( NRC.EQ.0 )GOTO 510
500   CONTINUE
C  ::: END LOOP OVER SUBMATRIX :::
C
510   CONTINUE
      IF( NUM.EQ.0 )THEN
         IF( SWMSG )PRINT *,' No columns changed'
         RETURN
      ENDIF
      IF( SWMSG )PRINT *,NUM,' Column(s) became infeasible',
     1                       '...use BASIS REFRESH'
C NCFIX AND BNDNAM MIGHT HAVE CHANGED
      CALL GETBAS(PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,SOLST,SOLNAM,
     2  NROWS,NCOLS,NONZER,NVALS,NONES,OBJNUM,
     3  NRFREE,NCFIX,MAXRLN,MAXCLN,NAMELN,VINF,VTOLAB,VTOLRE)
C
      RETURN
C
C ** AROUND ENDS HERE
      END
      SUBROUTINE AVERFY(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C          :...NEEDED FOR OUTPUT FILE UNIT
C
C This verifies solution values.
C    Syntax:  VERIFY [BASE | VALUES] [//[FREQ=freq][TIME=seconds]]
C
      CHARACTER*128 CLIST
C NOTE:  CLIST IS USED FOR OUTPUT
C        ========================
C LOCAL
      CHARACTER*16  NEWST,RCNAME,RNAME,CNAME
      CHARACTER*4   RC
      CHARACTER*8   OPTION(10)
      CHARACTER*1   RCKEY
      LOGICAL*1     SWBASE,SWVALS,SWREPL,SWPRML,SWDUAL
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C SET DEFAULTS (IN COMMON)
      CALL AKEYS0
C SET ABREFR SWITCH VALUES
      SWREPL = .FALSE.
      SWPRML = .TRUE.
      SWDUAL = .TRUE.
      CALL FLOOK(CLIST,FIRST,LAST,'//',ISPEC)
      IF( ISPEC.EQ.0 )GOTO 200
C PARSE SPECS
      F = ISPEC + 2
      L = LAST
      LAST = ISPEC-1
      OPTION(1) = 'FREQ '
      OPTION(2) = 'TIME '
100   NUMBER = 2
        CALL FOPTN(CLIST,F,L,OPTION,NUMBER,RCODE)
        IF( RCODE.NE.0 )RETURN
        IF( NUMBER.EQ.0 )GOTO 200
        CALL AKEYIN(OPTION(NUMBER),CLIST,F,L,RCODE)
        IF( RCODE.NE.0 )RETURN
      GOTO 100
200   CONTINUE
C ==== OPTIONS SET...GET [BASE | VALUES] ====
      OPTION(1) = 'BASE '
      OPTION(2) = 'VALUES '
      NUMBER = 2
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF( RCODE.NE.0 )RETURN
      SWBASE = (NUMBER.EQ.0) .OR. (NUMBER.EQ.1)
      SWVALS = (NUMBER.EQ.0) .OR. (NUMBER.EQ.2)
C ======= PARSE COMPLETE =======
      CALL GVERFY(SWBASE,SWVALS,ZVALUE,NROWS,CLIST,
     1    SWMSG,FREQ0,MAXTIM,
     2    VAXDIF,VRXDIF,VAPDIF,VRPDIF,VAXINF,VRXINF,VAPINF,VTOL0C,
     3    NPRIML,VPAVG,VPMAX,RNAME,  NDUAL,VDAVG,VDMAX,CNAME,
     4    NEWST, RC,RCNAME,RCKEY,VRC,VRCDIF, *1390)
C
      IF( NEWST.EQ.'BASE CHANGED' )THEN
         CALL GETBAS(PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,SOLST,SOLNAM,
     2               NROWS, NCOLS, NONZER,NVALS, NONES, OBJNUM,
     3               NRFREE,NCFIX, MAXRLN,MAXCLN,NAMELN,
     4               VINF,  VTOLAB,VTOLRE)
         IF( SWVALS )THEN
            CLIST = ' Solution values not verified'//
     1           ' (Re-enter if BASE problem resolved)'
            CALL FSLEN(CLIST,128,LAST)
            IF( OUTPUT.NE.TTYOUT )PRINT *,' ',CLIST(:LAST)
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
            RETURN
         ENDIF
      ENDIF
      IF( .NOT.SWVALS )RETURN
C VALUES WERE VERIFIED...SET SUBMATRIX STATS
      CALL ASETSB('ROW ')
      CALL ASETSB('COL ')
C ...SHOULD HAVE NPRIML=NRCSUB(1) AND NDUAL=NRCSUB(2)
C WRITE RESULTS TO OUTPUT (SAME AS BASIS REFRESH)
      CLIST = 'VERIFY Results'
      CALL ABRVER(CLIST,SWREPL,SWPRML,SWDUAL,
     1    NPRIML,VPAVG,VPMAX,RNAME, 0,0.,0.,' ',
     2    0,0.,0.,' ', NDUAL,VDAVG,VDMAX,CNAME,
     3    0,0.,0.,' ',
     4    NEWST, RC,RCNAME,RCKEY,VRC,VRCDIF)
C
9000  RETURN
C
1390  RCODE = 13
      RETURN
C
C ** AVERFY ENDS HERE
      END
      SUBROUTINE ABRVER(CLIST,SWREPL,SWPRML,SWDUAL,
     1    RNUMP,RAVGP,RMAXP,RNAMEP, CNUMP,CAVGP,CMAXP,CNAMEP,
     2    RNUMD,RAVGD,RMAXD,RNAMED, CNUMD,CAVGD,CMAXD,CNAMED,
     3    CNUMB,CAVGB,CMAXB,CNAMEB,
     4    NEWST, ROWCOL,RCNAME,RCKEY,VRC,VRCDIF)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCANAL
CI$$INSERT DCFLIP
C ...NEED DCFLIP FOR SCRWTH
C
C This reports results from REFRESH or VERIFY.
C ...SWREPL = T (for REFRESH) ===> set SOLST=NEWST after report.
C
      CHARACTER*128    CLIST
      LOGICAL*1        SWREPL,SWPRML,SWDUAL
      INTEGER          RNUMP, CNUMP, RNUMD, CNUMD, CNUMB
      REAL             RAVGP, CAVGP, RAVGD, CAVGD, CAVGB
      REAL             RMAXP, CMAXP, RMAXD, CMAXD, CMAXB
      CHARACTER*(*)    RNAMEP,CNAMEP,RNAMED,CNAMED,CNAMEB
      CHARACTER*(*)    NEWST, RCNAME,ROWCOL,RCKEY
C
C See GBREFR and GVERFY for dictionary of input and output arguments.
C
C LOCAL
      CHARACTER*12   STRVAL,STR12
      integer*2 tabd,width
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
C HEADER
      LINE = 0
      CALL FCENTR(CLIST,FIRST,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      TAB0 = 26
      WIDTH= 17
      IF( SWPRML )THEN
         TABP = TAB0
      ELSE
         TABP = TAB0 - WIDTH
      ENDIF
      IF( SWDUAL )TABD = TABP + WIDTH
      IF( SWPRML )CLIST(TABP:) = 'Primal Levels'
      IF( SWDUAL )CLIST(TABD:) = 'Dual Prices'
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      IF( SWPRML )CLIST(TABP:) = '============='
      IF( SWDUAL )CLIST(TABD:) = '============='
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C END HEADER, BEGIN TOLERANCES
      CLIST = 'Discrepancy tolerances'
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      CLIST = '  Absolute'
      CALL FR2CLJ(STRVAL,VAXDIF,LS)
      IF( SWPRML )CLIST(TABP+1:) = STRVAL
      CALL FR2CLJ(STRVAL,VAPDIF,LS)
      IF( SWDUAL )CLIST(TABD+1:) = STRVAL
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      CLIST = '  Relative'
      CALL FR2CLJ(STRVAL,VRXDIF,LS)
      IF( SWPRML )CLIST(TABP+1:) = STRVAL
      CALL FR2CLJ(STRVAL,VRPDIF,LS)
      IF( SWDUAL )CLIST(TABD+1:) = STRVAL
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      CLIST = 'Infeasibility tolerances'
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      CLIST = '  Absolute'
      CALL FR2CLJ(STRVAL,VAXINF,LS)
      IF( SWPRML )CLIST(TABP+1:) = STRVAL
      CALL FR2CLJ(STRVAL,VAPINF,LS)
      IF( SWDUAL )CLIST(TABD+1:) = STRVAL
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      CLIST = '  Relative'
      CALL FR2CLJ(STRVAL,VRXINF,LS)
      IF( SWPRML )CLIST(TABP+1:) = STRVAL
      CALL FR2CLJ(STRVAL,VRPINF,LS)
      IF( SWDUAL )CLIST(TABD+1:) = STRVAL
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C END TOLERANCES, BEGIN DISCREPANCIES
      CLIST = 'Row Discrepancies'
      IF( SWPRML )THEN
         STRVAL = ' '
         CALL FI2C(STRVAL,RNUMP,F)
         CLIST(TABP:) = STRVAL
      ENDIF
      IF( SWDUAL )THEN
         STRVAL = ' '
         CALL FI2C(STRVAL,RNUMD,F)
         CLIST(TABD:) = STRVAL
      ENDIF
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      IF( RNUMP.GT.0 .OR. RNUMD.GT.0 )THEN
         CLIST = '  Average magnitude'
         IF( RNUMP.GT.0 )THEN
            CALL FR2CLJ(STRVAL,RAVGP,LS)
            CLIST(TABP+1:) = STRVAL
         ENDIF
         IF( RNUMD.GT.0 )THEN
            CALL FR2CLJ(STRVAL,RAVGD,LS)
            CLIST(TABD+1:) = STRVAL
         ENDIF
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
         IF( RNUMP.GT.1 .OR. RNUMD.GT.1 )THEN
            CLIST = '  Worst'
            IF( RNUMP.GE.1 )THEN
               CALL FR2CLJ(STRVAL,RMAXP,LS)
               CLIST(TABP+1:) = STRVAL
            ENDIF
            IF( RNUMD.GE.1 )THEN
               CALL FR2CLJ(STRVAL,RMAXD,LS)
               CLIST(TABD+1:) = STRVAL
            ENDIF
            CALL FSLEN(CLIST,80,LAST)
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
         ENDIF
      ENDIF
      IF( RNUMP.GT.0 .OR. RNUMD.GT.0 )THEN
         IF( RNUMP.GT.0 )CLIST(TABP+1:) = RNAMEP
         IF( RNUMD.GT.0 )CLIST(TABD+1:) = RNAMED
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
         IF( RNUMP.GT.0 )THEN
            CALL GETNUM('ROW ',RNAMEP,I)
            CALL GETST ('ROW ',I,STRVAL,N)
            CLIST(TABP+1:) = '(STAT='//STRVAL(1:1)//')'
         ENDIF
         IF( RNUMD.GT.0 )THEN
            CALL GETNUM('ROW ',RNAMED,I)
            CALL GETST ('ROW ',I,STRVAL,N)
            CLIST(TABD+1:) = '(STAT='//STRVAL(1:1)//')'
         ENDIF
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      ENDIF
C
      STRVAL = ' '
      CLIST = 'Column Discrepancies'
      IF( SWPRML )THEN
         STRVAL = ' '
         CALL FI2C(STRVAL,CNUMP,F)
         CLIST(TABP:) = STRVAL
      ENDIF
      IF( SWDUAL )THEN
         STRVAL = ' '
         CALL FI2C(STRVAL,CNUMD,F)
         CLIST(TABD:) = STRVAL
         IF( CNUMB.GT.0 )THEN
            STRVAL = ' '
            CALL FI2C(STRVAL,CNUMB,F)
            CLIST(TABD+WIDTH:) = STRVAL
         ENDIF
      ENDIF
      CALL FSLEN(CLIST,128,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      IF( CNUMP.GT.0 .OR. CNUMD.GT.0 .OR. CNUMB.GT.0 )THEN
         CLIST = '  Average magnitude'
         IF( CNUMP.GT.0 )THEN
            CALL FR2CLJ(STRVAL,CAVGP,LS)
            CLIST(TABP+1:) = STRVAL
         ENDIF
         IF( CNUMD.GT.0 )THEN
            CALL FR2CLJ(STRVAL,CAVGD,LS)
            CLIST(TABD+1:) = STRVAL
         ENDIF
         IF( CNUMB.GT.0 )THEN
            CALL FR2CLJ(STRVAL,CAVGB,LS)
            CLIST(TABD+WIDTH+1:) = STRVAL
         ENDIF
         CALL FSLEN(CLIST,128,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
         IF( CNUMP.GT.1 .OR. CNUMD.GT.1 .OR. CNUMB.GT.1 )THEN
            CLIST = '  Worst'
            IF( CNUMP.GE.1 )THEN
               CALL FR2CLJ(STRVAL,CMAXP,LS)
               CLIST(TABP+1:) = STRVAL
            ENDIF
            IF( CNUMD.GE.1 )THEN
               CALL FR2CLJ(STRVAL,CMAXD,LS)
               CLIST(TABD+1:) = STRVAL
            ENDIF
            IF( CNUMB.GT.1 )THEN
               CALL FR2CLJ(STRVAL,CMAXB,LS)
               CLIST(TABD+WIDTH:) = STRVAL
            ENDIF
            CALL FSLEN(CLIST,128,LAST)
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
         ENDIF
      ENDIF
      IF( CNUMP.GT.0 .OR. CNUMD.GT.0 .OR. CNUMB.GT.0 )THEN
         IF( CNUMP.GT.0 )CLIST(TABP+1:) = CNAMEP
         IF( CNUMD.GT.0 )CLIST(TABD+1:) = CNAMED
         IF( CNUMB.GT.0 )CLIST(TABD+WIDTH+1:) = CNAMEB
         CALL FSLEN(CLIST,128,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
         IF( CNUMP.GT.0 )THEN
            CALL GETNUM('COL ',CNAMEP,I)
            CALL GETST ('COL ',I,STRVAL,N)
            CLIST(TABP+1:) = '(STAT='//STRVAL(1:1)//')'
         ENDIF
         IF( CNUMD.GT.0 )THEN
            CALL GETNUM('COL ',CNAMED,I)
            CALL GETST ('COL ',I,STRVAL,N)
            CLIST(TABD+1:) = '(STAT='//STRVAL(1:1)//')'
         ENDIF
         IF( CNUMB.GT.0 )THEN
            CALL GETNUM('COL ',CNAMEB,I)
            CALL GETST ('COL ',I,STRVAL,N)
            CLIST(TABD+WIDTH+1:) = '(STAT='//STRVAL(1:1)//')'
         ENDIF
         CALL FSLEN(CLIST,128,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      ENDIF
      IF( CNUMB.GT.0 )THEN
C PUT SENTENCE TO EXPLAIN NONBASIC D'S
         IF( CNUMB.EQ.1 )THEN
            CLIST = 'The column discrepancy at the right is'
         ELSE
            CALL FI2C(STRVAL,CNUMB,F)
            CLIST = 'The '//STRVAL(F:8)//
     1              ' column discrepancies at the right are'
         ENDIF
         CALL FSLEN(CLIST,120,LAST)
         IF( SWREPL )THEN
            CLIST(LAST+2:) = 'from new prices.'
         ELSE
            CLIST(LAST+2:) = 'from original prices.'
         ENDIF
         CALL FSLEN(CLIST,128,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      ENDIF
C
      IF( NEWST.EQ.SOLST )RETURN
C
C ===== SOLUTION STATUS CHANGES =====
      CALL FSLEN(SOLST,16,LS)
      CLIST = 'Using computed values, solution status becomes '//NEWST
      CALL FSLEN(CLIST,80,LAST)
      CLIST(LAST+2:) = '(from '//SOLST
      CALL FSLEN(CLIST,120,LAST)
      CLIST(LAST+1:) = ').'
      LAST = LAST+2
C     =========================
      IF( SWREPL )SOLST = NEWST
C     =========================
      IF( RCNAME.EQ.' ' )GOTO 8000
C ROWCOL RCNAME RCKEY TELLS WHY STATUS HAS CHANGED
      CALL FSLEN(RCNAME,16,LRC)
      IF( LAST+LRC+5.GE.SCRWTH )
     1   CALL FTEXT(CLIST,LAST,1,0,LINE,'KEEP ',*9000)
      IF( LAST.EQ.0 )THEN
         CLIST = ROWCOL//RCNAME
      ELSE
         CLIST(LAST+3:) = ROWCOL//RCNAME
      ENDIF
      CALL FSLEN(CLIST,128,LAST)
      CLIST(LAST+2:) = RCKEY
      LAST = LAST+3
      CLIST(LAST:) = '='
      CALL FR2CLJ(STRVAL,VRC,LS)
      IF( LAST+LS.GE.SCRWTH )
     1   CALL FTEXT(CLIST,LAST,1,0,LINE,'KEEP ',*9000)
      CALL FR2CLJ(STR12,VRCDIF,LS)
      IF( STR12.EQ.STRVAL )THEN
         CLIST(LAST+2:) = STRVAL(:LS)//' (= discrepancy'
      ELSE
         CLIST(LAST+2:) = STRVAL(:LS)//' (discrepancy='//STR12
      ENDIF
      CALL FSLEN(CLIST,128,LAST)
      CLIST(LAST+1:) = ').'
      LAST = LAST+2
      IF( LAST.GE.SCRWTH )
     1   CALL FTEXT(CLIST,LAST,1,0,LINE,'KEEP ',*9000)
C
      CLIST(LAST+2:) = 'To see the breakdown'
      CALL FSLEN(CLIST,128,LAST)
      IF( LAST.GT.100 )THEN
         CALL FTEXT(CLIST,LAST,1,0,LINE,'KEEP ',*9000)
         IF( LAST.EQ.0 )LAST = -1
      ENDIF
      CLIST(LAST+2:) = '(called factors), enter'
      CALL FSLEN(CLIST,128,LAST)
      IF( LAST+19+LRC.GE.SCRWTH )THEN
         CALL FTEXT(CLIST,LAST,1,0,LINE,'KEEP ',*9000)
         IF( LAST.EQ.0 )LAST=-1
      ENDIF
      IF( ROWCOL.EQ.'ROW ' )THEN
         CLIST(LAST+2:) = 'C '//RCNAME(:LRC)//',* ; SHO ROW //IO'
      ELSE
         CLIST(LAST+2:) = 'C *,'//RCNAME(:LRC)//' ; SHO COL //IO'
      ENDIF
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C
8000  CONTINUE
C WE GET HERE UPON COMPLETION OF REPORT (NO ABORT)
      IF( CLIST.NE.' ' )THEN
C WRITE LAST LINE
         CALL FSLEN(CLIST,128,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      ENDIF
C
C NORMAL RETURNS AND USER ABORTS
9000  RETURN
C
C ** ABRVER ENDS HERE
      END
      SUBROUTINE AKEYS0
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This sets default key values, passed in common.
C (If keys are not loaded, default is set here.)
C
C                  DECLARATIONS FOR FLKEYWRD ROUTINES
C                  ::::::::::::::::::::::::::::::::::
      CHARACTER*16  FKNAME(100),FKDEFT(100),FKVALU(100),FKADMV(300)
      INTEGER*2     FKVPTR(0:100)
      COMMON/FLKEYC/FKNAME,    FKDEFT,    FKVALU,    FKADMV
      COMMON/FLKEYN/MXFKEY,MXFADM,NFKEYS,FKVPTR
C LOCAL
      CHARACTER*16  STRVAL
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C SET KEY NAMES
      AKEYNM( 1) = 'TIME      '
      AKEYNM( 2) = 'FREQUENCY '
      AKEYNM(11) = 'AXYDIF    '
      AKEYNM(12) = 'RXYDIF    '
      AKEYNM(13) = 'ADPDIF    '
      AKEYNM(14) = 'RDPDIF    '
      AKEYNM(15) = 'AXYINF    '
      AKEYNM(16) = 'RXYINF    '
      AKEYNM(17) = 'ADPINF    '
      AKEYNM(17) = 'RDPINF    '
      AKEYNM(19) = 'TOL0_ALPHA'
      AKEYNM(20) = 'TOL0_PIVOT'
      AKEYNM(21) = 'TOL0_COEF '
      AKEYNM(22) = 'TOLAX_FIX '
      AKEYNM(23) = 'TOLRX_FIX '
C SET DEFAULTS IN CASE WE DON'T FIND KEYS
      CALL FCLOCK(TIME0)
      MAXTIM = 30
      FREQ0  = 1
      VAXDIF = 1.0E-6
      VRXDIF = .001
      VAPDIF = 1.0E-6
      VRPDIF = .001
      VAXINF = 1.0E-5
      VRXINF = .01
      VAPINF = 1.0E-5
      VRPINF = .01
      VTOL0A = 1.0E-14
      VTOL0P = 1.0E-12
      VTOL0C = 1.0E-10
      VTAFIX = 1.0E-6
      VTRFIX = .01
      IF( NFKEYS.EQ.0 )RETURN
C
C  ::: LOOP OVER KEYWORDS :::
      DO 100 K=1,NFKEYS
         STRVAL = FKVALU(K)
         LS = 16
         CALL FSQUEZ(STRVAL,LS)
         RCODE  = 0
         CALL FC2R(STRVAL,V,RCODE)
         IF( RCODE.NE.0 )GOTO 100
C VALUE IS NUMERIC...SEE IF KEY IS ONE OF OURS
C ...INTEGER
         DO 50 I=AKEYI1,AKEYI2
            IF( FKNAME(K).EQ.AKEYNM(I) )THEN
               AKEYIV(I) = V + .1
               GOTO 100
            ENDIF
50       CONTINUE
C ...REAL
         DO 75 I=AKEYR1,AKEYR2
            IF( FKNAME(K).EQ.AKEYNM(I) )THEN
               AKEYRV(I) = V
               GOTO 100
            ENDIF
75       CONTINUE
100   CONTINUE
C
      RETURN
C
C ** AKEYS0 ENDS HERE
      END
      SUBROUTINE AKEYIN(WHAT,CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This parses for WHAT = TIME | FREQ (value is in common).
C
      CHARACTER*(*) WHAT
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*1   CHAR
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::
C GET NON-NEGATIVE INTEGER
      CALL FCL2IN(CLIST,FIRST,LAST,INT,RCODE)
      IF( RCODE.NE.0 )RETURN
      IF( WHAT.EQ.'FREQ ' )THEN
         FREQ0 = INT
      ELSE
C MAX TIME SPECIFIED...SEE IF CLOCK IS AVAILABLE
         CALL FCLOCK(TIME0)
         IF( TIME0.LT.0 )THEN
            PRINT *,' CLOCK NOT AVAILABLE...Continue (Y/N)? '
            CALL FGTCHR('YN','Y',CHAR)
            IF( CHAR.EQ.'N' )GOTO 1300
C ...MAXTIM WILL NOT MATTER BECAUSE ELAPSED TIME WILL = 0
         ENDIF
         MAXTIM = INT
      ENDIF
      RETURN
C
1300  CONTINUE
C USER ABORT
      RCODE = -1
      RETURN
C
C ** AKEYIN ENDS HERE
      END
