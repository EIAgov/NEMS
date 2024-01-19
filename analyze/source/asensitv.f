C              ::: ASENSITV.FOR  8-17-95 :::
C
C Earlier dates deleted
C    4-24-94...Allow MAXITR=0 in AREDUC
C    7-21-95...Put TIME parameter on REDUCE command
C              Added MAXTIM to CALL REDUCE
C    7-30-95...New args for CALL RED...
C
C This contains the following ANALYZE subroutines.
C
C    AREDUC.....execute REDUCE command
C    ATRACE.....execute TRACE command
C    ATRADD.....add column to path
C    ARENAM.....execute RENAME command
C    AGGRG8.....aggregate row or column
C
      SUBROUTINE AREDUC(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCANAL
CI$$INSERT DCFLIP
C
C This executes REDUCE command.
C   Syntax:  REDUCE [row_condnl] [,col condnl] [//specs]
C     specs are any of:
C   1 BINARY = name       ...specifies binary variables
C   2 INTEGER= name       ...specifies integer variables
C   3 LOGICAL             ...specifies logical implications test
C   4 MAXITER= number     ...specifies max number of iteration
C   5 OVERWRIT            ...specifies overwrite bounds
C   6 REPORT = {SB}|NONE  ...specifies reports to be written
C   7 VERIFY              ...specifies verify reductions
C                            (compare with resident solution)
C   8 PHASE  = number     ...specifies highest phase to execute
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*1   CHAR
      CHARACTER*4   REPORT
      CHARACTER*8   OPTION(10),SPEC
      CHARACTER*16  NAME01,NAMINT,RNAME
      LOGICAL*1     SW,SWMAT,LGLTST,RPTBND,RPTSUM,VERIFY
      PARAMETER    (PMXITR=25)
C                          :....DEFAULT MAXITER
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
C SET DEFAULTS FOR PARAMETER SETTINGS
      CALL AKEYS0
      NAME01 = ' '
      NAMINT = ' '
      LGLTST = .FALSE.
      MAXITR = PMXITR
      SWMAT  = .FALSE.
      REPORT = 'S'
      VERIFY = .FALSE.
      PHASE  = 2
C SEE IF SPECS
      CALL FLOOK(CLIST,FIRST,LAST,'//',IPARM)
      IF( IPARM.EQ.0 )GOTO 1000
C PARSE PARAMETER SPECS FIRST
C
      OPTION(1) = 'BINARY  '
      OPTION(2) = 'INTEGER '
      OPTION(3) = 'LOGICAL '
      OPTION(4) = 'MAXITER '
      OPTION(5) = 'OVERWRIT'
      OPTION(6) = 'REPORT  '
      OPTION(7) = 'VERIFY  '
      OPTION(8) = 'PHASE   '
      IFIRST = IPARM+2
C ...GET (NEXT) PARAMETER SPECIFICATION
100   NUMBER = 8
        CALL FOPTN(CLIST,IFIRST,LAST,OPTION,NUMBER,RCODE)
        IF( RCODE.NE.0 )RETURN
        IF( NUMBER.EQ.0 )GOTO 900
        SPEC = OPTION(NUMBER)
        IF( SPEC.EQ.'BINARY ' )THEN
C BINARY = name
           CALL FTOKEN(CLIST,IFIRST,LAST,NAME01,NAMELN,CHAR)
           IF( NAME01.EQ.' ' )THEN
              IF( SWMSG )PRINT *,' BINARY NAME MASK ASSUMED = *'
              NAME01 = '*'
              GOTO 900
           ENDIF
        ELSE IF( SPEC.EQ.'INTEGER ' )THEN
C INTEGER= name
           CALL FTOKEN(CLIST,IFIRST,LAST,NAMINT,NAMELN,CHAR)
           IF(NAMINT.EQ.' ')THEN
              IF( SWMSG )PRINT *,' INTEGER NAME MASK ASSUMED = *'
              NAMINT = '*'
              GOTO 900
           ENDIF
        ELSE IF( SPEC.EQ.'MAXITER ' )THEN
C MAXITER = number
           CALL FVRNG(CLIST,IFIRST,LAST,VL,VU,VINF,CHAR,RCODE)
           IF( RCODE.NE.0 )RETURN
           IF( VU.GE.32000. )THEN
              MAXITR = 32000
           ELSE IF( VU.LT.0. )THEN
              PRINT *,' ** MAXITER CANNOT BE NEGATIVE'
              RCODE = 1
              RETURN
           ELSE
              MAXITR = VU+.1
           ENDIF
        ELSE IF( SPEC.EQ.'LOGICAL ' )THEN
C LOGICAL
           LGLTST = .TRUE.
        ELSE IF( SPEC.EQ.'OVERWRIT' )THEN
C OVERWRIT
           SWMAT = .TRUE.
        ELSE IF( SPEC.EQ.'REPORT ' )THEN
C REPORT = {SB} | NONE
           CALL FTOKEN(CLIST,IFIRST,LAST,REPORT,4,CHAR)
           IF( REPORT.EQ.' ' )THEN
              PRINT *,' ** REPORT OPTION NOT SPECIFIED'
              RCODE = 1
              RETURN
           ENDIF
           CALL FMATCH(REPORT,'NONE',' ',SW)
           CALL FSLEN(REPORT,4,L)
           CALL FSCASE(REPORT,1,L)
C CHECK STRING (MUST BE A COMBINATION OF SB)
           DO 250 I=1,L
              CHAR = REPORT(I:I)
              IF( CHAR.NE.'S'.AND.CHAR.NE.'B' )THEN
                 PRINT *,' ** REPORT OPTION ',REPORT(1:L),' NOT ',
     1           'RECOGNIZED...MUST BE NONE OR A COMBINATION OF SB'
                 RCODE = 1
                 RETURN
              ENDIF
250        CONTINUE
        ELSE IF( SPEC.EQ.'VERIFY ' )THEN
C VERIFY
           VERIFY = .TRUE.
         ELSE IF( SPEC.EQ.'PHASE ' )THEN
C PHASE = number
           CALL FCL2IN(CLIST,IFIRST,LAST,PHASE,RCODE)
           IF( RCODE.NE.0 )RETURN
        ENDIF
       GOTO 100
C      ~~~~~~~~ END OF PARAMETER SPEC...SET LAST FOR CONDITIONAL
900   CONTINUE
      LAST = IPARM-1
C
1000  CONTINUE
C SET REPORT SWITCHES
      CALL FLOOK(REPORT,1,4,'S',I)
      RPTSUM = (I.GT.0)
      CALL FLOOK(REPORT,1,4,'B',I)
      RPTBND = (I.GT.0)
C NOTE:  IF REPORT=NONE, RPT SWITCHES WILL BE FALSE
C
C PARSE CONDITIONAL (AS IN COUNT COMMAND)
      CALL ACOUNT(CLIST,FIRST,LAST,0,RCODE)
      IF( RCODE.NE.0 )RETURN
      IF( NRCSUB(1).EQ.0.OR.NRCSUB(2).EQ.0 )THEN
         PRINT *,' No rows or columns in submatrix...',
     1           'REDUCE not executed'
         RETURN
      ENDIF
C   =======================================================
C GET READY
      CALL GSETOP(OPTNAM)
C GET SET
      CALL REDSET(CLIST,OUTPUT,MAXITR,SWMSG,FREQ0,MAXTIM,
     1    VAXDIF,VRXDIF,VAPDIF,VRPDIF,VAXINF,VRXINF,VAPINF,
     2    VTAFIX,VTRFIX,VTOL0C,VTOL0A,VTOL0P,
     3    NAME01,NAMINT,LGLTST,PHASE,SWMAT,RPTBND,RPTSUM,*13000,*9000)
C GO
      CALL REDUCE(ZVALUE,NROWS,CLIST,
     1    NAME01,NAMINT,PHASE,SWMAT,RPTBND,RPTSUM,RCODE)
      IF( RCODE.GT.0 )RETURN
      IF( RCODE.LT.0 )GOTO 8000
      IF( LGLTST )THEN
C EXECUTE LOGICAL IMPLICATIONS TESTS (FROM HERE, SO WE CAN PASS LSTs)
         IF( SWMSG )PRINT *,' Performing logical tests'
         CALL REDLGL(CLIST,ROWLST,VALLST,MAXLST,NAME01,NAMINT,NUMLGL,
     1               RCODE)
         IF( RCODE.GT.0 )RETURN
         IF( RCODE.LT.0 )GOTO 8000
         IF( SWMSG )PRINT *,NUMLGL,' Implications Written'
      ENDIF
C
      IF( .NOT.VERIFY )RETURN
C VERIFY REDUCTIONS (IF RESIDENT SOLUTION IS OPTIMAL)
      IF( SOLST.NE.'OPTIMAL' )RETURN
      IF( SWMSG )PRINT *,' Verifying reductions'
C ROWS
      NDISCR = 0
      DO 6100 I=1,NROWS
         CALL GETMAP('ROW ',I,SW)
         IF( .NOT.SW )GOTO 6100
         CALL REDGET('ROW ',I,VXL,VXU,VPL,VPU)
         CALL GETSOL('ROW ',I,VX,VP,CHAR,DUMMY)
         IF( VX.LT.VXL - VTOLAB - VTOLRE*ABS(VXL) .OR.
     1       VX.GT.VXU + VTOLAB + VTOLRE*ABS(VX) )THEN
            CALL GETNAM('ROW ',I,RNAME)
            IF( SCRWTH.GT.95 )THEN
               WRITE(OUTPUT,6101,ERR=13000)RNAME,VX,VXL,VXU
6101           FORMAT(' Level of ',A16,' = ',G15.9,
     1             ' Reduced bounds: ',G15.9,' to ',G15.9 )
            ELSE IF( SCRWTH.GT.78 .AND. NAMELN.LT.9 )THEN
               WRITE(OUTPUT,6102,ERR=13000)RNAME,VX,VXL,VXU
6102           FORMAT(' Level of ',A8,'=',G13.8,
     1             ' Reduced bounds: ',G13.8,' to ',G13.8 )
            ELSE
               WRITE(OUTPUT,6103,ERR=13000)RNAME,VX,VXL,VXU
6103           FORMAT(' Level of ',A8,' = ',G15.9/
     1             ' Reduced bounds: ',G15.9,' to ',G15.9 )
            ENDIF
            NDISCR = NDISCR + 1
         ENDIF
6100  CONTINUE
C
      WRITE(OUTPUT,6109,ERR=13000)NDISCR
6109  FORMAT(I6,' Row Discrepancies')
C
C COLUMNS
      NDISCR = 0
      DO 6300 I=1,NCOLS
         CALL GETMAP('COL ',I,SW)
         IF( .NOT.SW )GOTO 6300
         CALL REDGET('COL ',I,VXL,VXU,VPL,VPU)
         CALL GETSOL('COL ',I,VX,VP,CHAR,DUMMY)
         IF( VX.LT.VXL - VTOLAB - VTOLRE*ABS(VXL) .OR.
     1       VX.GT.VXU + VTOLAB + VTOLRE*ABS(VX) )THEN
            CALL GETNAM('COL ',I,RNAME)
            IF( SCRWTH.GT.95 )THEN
               WRITE(OUTPUT,6101,ERR=13000)RNAME,VX,VXL,VXU
            ELSE IF( SCRWTH.GT.78 .AND. NAMELN.LT.9 )THEN
               WRITE(OUTPUT,6102,ERR=13000)RNAME,VX,VXL,VXU
            ELSE
               WRITE(OUTPUT,6103,ERR=13000)RNAME,VX,VXL,VXU
            ENDIF
            NDISCR = NDISCR + 1
         ENDIF
6300  CONTINUE
C
      WRITE(OUTPUT,6309,ERR=13000)NDISCR
6309  FORMAT(I6,' Column Discrepancies')
C
      RETURN
C
C INFEASIBILITY DETECTED
8000  CONTINUE
      IF( SOLST.EQ.'FEASIBLE' .OR. SOLST.EQ.'OPTIMAL' )THEN
         WRITE(OUTPUT,8001,ERR=13000)
8001     FORMAT(/' Warning:  Resident solution claims to be feasible'
     1          /' ...Try using BASIS REFRESH',
     2           '...If REDUCE is in error, please report.' )
         IF( OUTPUT.NE.TTYOUT )WRITE(*,8001)
      ENDIF
      RCODE = 0
C
9000  RETURN
C
13000 RCODE = 1
      RETURN
C
C ** AREDUC ENDS HERE
      END
      SUBROUTINE ATRACE(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This executes TRACE command, which traces flow in submatrix,
C resulting in new submatrix with "complete substructure".
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME
      CHARACTER*1   CHAR,STAT
      LOGICAL*1     SW
      LOGICAL*1     SWCOVR
      CHARACTER*8   OPTION(1)
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
C CHECK MEMORY...need 2*NROWS bytes in AMAP (EQUIVALENCEd to SCRIND)
      IF( NROWS.GT.SCRIND(0) )THEN
         PRINT *,' Sorry, not enough scratch space to trace...'//
     1           'need',NROWS
         RCODE = 1
         RETURN
      ENDIF
C
      MAXSUB= NROWS
C SEE IF SPECIFICATION
      CALL FLOOK(CLIST,FIRST,LAST,'//',IPARM)
      IF(IPARM.EQ.0)GOTO 9
C YES...PARSE PARAMETER SPECS FIRST
      OPTION(1)='MAXSUB'
      IFIRST = IPARM+2
      NUMBER = 1
      RCODE = -1
      CALL FOPTN(CLIST,IFIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)RETURN
C ...GET number
         CALL FVRNG(CLIST,IFIRST,LAST,VL,VU,VINF,CHAR,RCODE)
         IF(RCODE.NE.0)RETURN
         IF(VL.GT.32000.)THEN
C number = infinity
            MAXSUB = 32000
         ELSE
            MAXSUB=VL+.1
            IF(MAXSUB.LT.1)THEN
               PRINT *,' MAX ITERATIONS CANNOT =',MAXSUB
               RCODE = 1
               RETURN
            ENDIF
         ENDIF
C PREPARE FOR CONDITIONAL PARSE
         LAST = IPARM-1
C
9     CONTINUE
C
C SET SUBMATRIX
      CALL ACOUNT(CLIST,FIRST,LAST,0,RCODE)
      IF(RCODE.NE.0)RETURN
C
C INITIALIZE ROW IO
C
      TRNEG   = 0
      TRPOS   = TRNEG + NROWS
C
      DO 10 I=1,NROWS
           CALL GETBND('ROW ',I,VL,VU)
C CONSIDER + COVERED IF THERE IS NO LOWER BOUND
           AMAP(TRPOS + I) = VL.LE.-VINF
C CONSIDER - COVERED IF THERE IS NO UPPER BOUND
           AMAP(TRNEG + I) = VU.GE. VINF
C THESE COVERS MEAN THAT IF A + ENTERS, ITS - IS THERE TO BALANCE IT.
C
C     FOR EXAMPLE, SUPPOSE A COLUMN IS ADDED AND HAS + IN ROW.
C     THEN, AT THIS POINT WE WOULD SEEK A - TO BALANCE IT, BUT
C     IF THE ROW IS TYPE G, WE HAVE + >= VL (NO UPPER BOUND), SO
C     THE ADDED COLUMN'S LEVEL COULD INCREASE WITHOUT VIOLATION.
C     THIS IS EQUIVALENT TO ASSUMING THE - IS COVERED.
C
C     SIMILARLY, IF WE ADD A COLUMN WITH - IN ROW AND IF THE ROW IS
C     TYPE L, WE HAVE - <= U (NO LOWER BOUND), SO AGAIN NO LIMIT
C     FOR INCREASING THE LEVEL, SO WE NEED NOT SEEK + FOR BALANCE.
C
C FREE ROWS ARE COVERED (BUT REMAIN IN THE SUBMATRIX)
10    CONTINUE
C
C    LOOP OVER COLUMNS IN SUBMATRIX TO SET IO AND ADD THEIR ROWS
C
      NRC = NRCSUB(2)
      DO 100 J=RCSUB1(2),NCOLS
           CALL GETMAP('COL ',J,SW)
           IF(.NOT.SW)GOTO 100
C COLUMN J IS IN SUBMATRIX
           NRC = NRC - 1
C ...ADD ITS ROWS
           CALL ATRADD(J,RCODE)
           IF(RCODE.NE.0)GOTO 9000
C EARLY EXIT IF WE'VE SEEN ALL SUBMATRIX COLUMNS
           IF(NRC.LE.0)GOTO 110
100   CONTINUE
C
110   CONTINUE
C  NEVER TO RETURN TO (OR ABOVE) STATEMENT 110
      LINE = 1
C
C          ::: TOP OF MAIN LOOP :::
C
C TEST IF ALL ROWS ARE COVERED (IE, SUBMATRIX "PATH" IS COMPLETE)
C
300   NRC = NRCSUB(1)
      IF(NRC.GT.MAXSUB)THEN
CC            :...CHANGED FROM GE 4-24-94
         PRINT *,' *** TRACE ABORTED...Maximum size reached...',NRC
         GOTO 9000
      ENDIF
C
      DO 500 RCOVER=RCSUB1(1),NROWS
           IF(NRC.EQ.0)GOTO 510
           CALL GETMAP('ROW ',RCOVER,SW)
           IF(.NOT.SW)GOTO 500
           NRC = NRC - 1
C
           IF(.NOT.(AMAP(TRPOS+RCOVER).AND.AMAP(TRNEG+RCOVER))
     1       )GOTO 590
500   CONTINUE
C
510   IF(SWMSG)PRINT *,' TRACE complete...submatrix is set'
      GOTO 9000
C
590   CONTINUE
C
C ROW (RCOVER) IS NOT COVERED...SEARCH FOR NEW COLUMN TO ADD TO PATH
C
C INITIALIZE MERITS
      OPTCVR = -NROWS
C     :....NET NUMBER OF UNCOVERED ROWS COVERED BY BEST COLUMN
      OPTRNK = 6
C OPTRNK is best rank, where:
C    Rank = 1 if STAT = I
C         = 2 if     L < X < U
C         = 3 if 0 < L = X < U
C         = 4 if     L < X = U
C         = 5 if 0 < L = X = U
      OPTCOL = 0
C
C     LOOP OVER COLUMNS
C
      DO 900 J=1,NCOLS
           CALL GETMAP('COL ',J,SW)
           IF(SW)GOTO 900
C COLUMN (J) IS NOT IN SUBMATRIX...EVALUATE ITS MERIT
C (COLUMN MUST BE ABLE TO COVER SOME UNCOVERED ROW IN PATH)
C
           CALL GETCOL(J,NZ,MAXLST,ROWLST,VALLST)
           IF(NZ.GT.MAXLST)THEN
              PRINT *,' Sorry, not enough space to complete trace'
              RCODE = 1
              GOTO 9000
           ENDIF
C
           NETCVR = 0
           SWCOVR = .FALSE.
C
           DO 700 I=1,NZ
              ROW = ROWLST(I)
              CALL GETMAP('ROW ',ROW,SW)
              IF(SW)THEN
C          ROW IS IN SUBMATRIX...SEE IF COLUMN ADDS COVER
                 IF((.NOT.AMAP(TRNEG+ROW).AND.VALLST(I).LT.0.0) .OR.
     1              (.NOT.AMAP(TRPOS+ROW).AND.VALLST(I).GT.0.0))THEN
                      SWCOVR = .TRUE.
                      NETCVR = NETCVR + 1
                 ENDIF
              ELSE
C          ROW IS NOT IN SUBMATRIX...SEE IF COLUMN ADDS UNCOVERED ROW
                 IF((.NOT.AMAP(TRNEG+ROW).AND.VALLST(I).GT.0.0) .OR.
     1              (.NOT.AMAP(TRPOS+ROW).AND.VALLST(I).LT.0.0)
     2             )NETCVR = NETCVR - 1
              ENDIF
700        CONTINUE
C
           IF(.NOT.SWCOVR)GOTO 900
C
C COLUMN J COVERS A ROW IN PATH...SEE IF RANK IS BETTER THAN BEST
C
           CALL GETSOL('COL ',J,VX,VD,STAT,STNUM)
           IF(STAT.EQ.'I')THEN
              RANK = 1
           ELSE
              CALL GETBND('COL ',J,VL,VU)
              IF(VL.LT.VX.AND.VX.LT.VU)THEN
                 RANK = 2
              ELSE IF(VX.LT.VU.AND.VL.GT.0.)THEN
                 RANK = 3
              ELSE IF(VL.LT.VX.AND.VX.GE.VU)THEN
                 RANK = 4
              ELSE IF(VL.GE.VX.AND.VL.GT.0.)THEN
                 RANK = 5
              ELSE
                 GOTO 900
              ENDIF
           ENDIF
           IF(RANK.GT.OPTRNK)GOTO 900
C RANK IS AT LEAST AS GOOD AS BEST
           IF(NETCVR.GT.OPTCVR.OR.RANK.LT.OPTRNK)THEN
C EITHER RANK IS STRICTLY BETTER, OR IT IS EQUAL BUT COVER IS BETTER
C HENCE, COLUMN MERIT IS BETTER...MAKE IT THE BEST SO FAR
              OPTCVR = NETCVR
              OPTRNK = RANK
              OPTCOL = J
           ENDIF
900   CONTINUE
C
      IF(OPTCOL.EQ.0)GOTO 1000
C
910   CONTINUE
C ADD OPTCOL TO PATH
      CALL ATRADD(OPTCOL,RCODE)
      IF( RCODE.NE.0 )RETURN
C
C ::: BRANCH TO TOP OF MAIN LOOP :::
              GOTO 300
C             ========
1000  CONTINUE
C NO COLUMN COVERS ROW (RCOVER)...CHECK BOUNDS
C
C CASES:     1. ROW IS NULL (AND NOT FREE)
C            2. ROW = +...+  (NO COL WITH -)
C            3. ROW = -...-  (NO COL WITH +)
C
C OUTCOMES:  1. WE MUST HAVE VL <= 0 <= VU (OR INFEASIBLE)
C            2. RHS COVERS IF VL > 0 OR 0 < VU < *
C            3. RHS COVERS IF VU < 0 OR * < VL < 0
C FAILURE OCCURS IF:
C     WE HAVE CASE 2 WITH VL <= 0 AND EITHER VU <=0 OR VU=*
C     WE HAVE CASE 3 WITH VU >= 0 AND EITHER VL >=0 OR VL=*
C ...THE FAILURE IS CORRECT (IE, INFEASIBILITY OR REDUNDANCY DETECTED)
C    IF ACTIVITY LOWER BOUNDS ARE NON-NEGATIVE
C  FOR EXAMPLE, THE CASE 2 FAILURE MEANS WE HAVE:
C       0|- <= +...+    AND EITHER  +...+ <= 0|- OR NO UPPER BOUND
C       :::::...REDUNDANT IF L>=0        ::::...INFEASIBLE OR REDUNDANT
C
      CALL GETBND('ROW ',RCOVER,VL,VU)
      IF(.NOT. ( AMAP(TRNEG+RCOVER).OR.AMAP(TRPOS+RCOVER) ) )THEN
C
C CASE 1...NEITHER + NOR - IS IN THIS ROW
C    (WE MUST HAVE STARTED WITH THIS ROW IN THE SUBMATRIX SINCE
C     IT COULD NOT HAVE BEEN ADDED BY AN ADDED COLUMN.)
C
         IF( VL.LE.0. .AND. VU.GE.0. )THEN
C OK, NULL ROW IS COVERED
            AMAP(TRNEG+RCOVER) = .TRUE.
            AMAP(TRPOS+RCOVER) = .TRUE.
         ELSE
            GOTO 1200
         ENDIF
C
      ELSE IF( .NOT. AMAP(TRNEG+RCOVER) )THEN
C
C CASE 2...WE NEED A NEGATIVE COVER
C
         IF( VL.GT.0. .OR. (VU.GT.0..AND.VU.LT.VINF) )THEN
C RHS COVERS
            AMAP(TRNEG+RCOVER) = .TRUE.
         ELSE
            GOTO 1200
         ENDIF
      ELSE
C
C CASE 3...WE NEED A POSITIVE COVER
C
         IF( VU.LT.0. .OR. (VL.LT.0..AND.VL.GT.-VINF) )THEN
C RHS COVERS
            AMAP(TRPOS+RCOVER) = .TRUE.
         ELSE
            GOTO 1200
         ENDIF
      ENDIF
C RHS COVER SUCCESSFUL
      IF( SWMSG )THEN
C  WRITE MESSAGE TO OUTPUT FILE (USING FTEXT)
         CALL GETNAM('ROW ',RCOVER,RNAME)
         CLIST = 'ROW '//RNAME(:NAMELN)//' COVERED BY RHS'
         CALL FSLEN(CLIST,60,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
      ENDIF
C
C       BRANCH TO TOP OF MAIN LOOP
                GOTO 300
C               ========
1200  CONTINUE
C FAILURE TO COVER ROW (RCOVER)
      CALL GETNAM('ROW ',RCOVER,RNAME)
      IF( SOLST.EQ.'INFEASIBLE' )GOTO 8000
C
C THIS MAY HAVE OCCURRED DUE TO OUR LIMITED CHOICE OF COVERING COLS
C (EG, DISMISSING REDUNDANCY).  BEGIN BY INTERROGATING ITS COLUMNS AGAIN.
C
C LOOP OVER COLUMNS TO FIND A COVER
      DO 2000 OPTCOL=1,NCOLS
         CALL GETSGN(RCOVER,OPTCOL,CHAR)
         IF( ( .NOT.AMAP(TRNEG+RCOVER) .AND. CHAR.EQ.'-' ) .OR.
     1       ( .NOT.AMAP(TRPOS+RCOVER) .AND. CHAR.EQ.'+' )
     G     )GOTO 910
2000  CONTINUE
C WE GET HERE IF WE REALLY CANNOT NOT COVER
C
8000  CONTINUE
      CLIST = '** CANNOT COVER ROW '//RNAME
      CALL FSLEN(CLIST,60,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C
9000  CONTINUE
C ================= DONE ==================
C SET SUBMATRIX
      CALL ASETSB('ROW ')
      CALL ASETSB('COL ')
      CALL GMAPNZ(NRCSUB(1),RCSUB1(1),NRCSUB(2),RCSUB1(2),NZSUB)
C
      RETURN
C
C ** ATRACE ENDS HERE
      END
      SUBROUTINE ATRADD(NUMBER,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This adds column NUMBER into submatrix for trace (+ its rows)
C  Note:  if resident solution is optimal,
C         a row with 0 price is not added
C
C LOCAL
      CHARACTER*16  CNAME
      CHARACTER*1   STAT
      LOGICAL*1     SW
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      CALL GETCOL(NUMBER,NZ,MAXLST,ROWLST,VALLST)
      IF(NZ.GT.MAXLST)THEN
         CALL GETNAM('COL ',NUMBER,CNAME)
         PRINT *,' Sorry, column ',CNAME(:NAMELN),
     1          ' has too many nonzeroes (Limit=',MAXLST,')'
         RCODE = 1
         RETURN
      ENDIF
C
C  ::: LOOP OVER NONZEROES OF COLUMN (NUMBER) :::
C
      DO 100 I=1,NZ
           ROW = ROWLST(I)
           AMAP(TRPOS+ROW) = AMAP(TRPOS+ROW).OR.VALLST(I).GT.0.0
           AMAP(TRNEG+ROW) = AMAP(TRNEG+ROW).OR.VALLST(I).LT.0.0
C NOW AMAP(TRPOS+ROW) = TRUE MEANS ROW HAS + COVERED
C     AMAP(TRNEG+ROW) = TRUE MEANS ROW HAS - COVERED
           CALL GETMAP('ROW ',ROW,SW)
           IF(SW)GOTO 100
C ROW IS NOT IN SUBMATRIX...ADD
           IF(SOLST(1:3).EQ.'OPT')THEN
C ...ONLY IF BINDING OPTIMAL SOLUTION
              CALL GETSOL('ROW ',ROW,VX,VP,STAT,STNUM)
              IF(ABS(VP).LE.VTOLAB)GOTO 100
C       NOTE: THIS TEST ALLOWS OBJECTIVE TO ENTER SUBMATRIX, BUT
C             IT'S COMPLETELY COVERED SINCE IT'S A FREE ROW.
           ENDIF
C OK, NOW ADD ROW TO SUBMATRIX
           SW = .TRUE.
           CALL GPUTMP('ROW ',ROW,SW)
           NRCSUB(1) = NRCSUB(1) + 1
           IF(ROW.LT.RCSUB1(1))RCSUB1(1) = ROW
C NEXT NONZERO OF COLUMN
100    CONTINUE
C   ::: END LOOP OVER COLUMN'S NONZEROES :::
C
C FINALLY, ADD COLUMN TO SUBMATRIX (IF NOT ALREADY THERE)
      CALL GETMAP('COL ',NUMBER,SW)
      IF(.NOT.SW)THEN
C ADD COLUMN NUMBER TO PATH
           SW = .TRUE.
           CALL GPUTMP('COL ',NUMBER,SW)
           NRCSUB(2) = NRCSUB(2) + 1
           IF(NUMBER.LT.RCSUB1(2))RCSUB1(2) = NUMBER
      ENDIF
C
      RETURN
C
C ** ATRADD ENDS HERE
      END
      SUBROUTINE ARENAM(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This executes RENAME command.
C  Syntax: RENAME {PROBLEM | BOUNDS | RANGES | RHS | SOLVER} name [,...]
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*1   CHAR
      CHARACTER*8   OPTION(5)
      CHARACTER*16  NAME
      DATA OPTION/'PROBLEM ','BOUNDS ','RANGES ','RHS ','SOLVER '/
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
100   CONTINUE
        NUMBER = 5
        RCODE = -1
        CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
        IF( RCODE.NE.0 )RETURN
        CALL FTOKEN(CLIST,FIRST,LAST,NAME,16,CHAR)
        IF( NAME.EQ.' ' )THEN
           PRINT *,' ** MISSING '//OPTION(NUMBER)//'NAME'
           RCODE = -1
           RETURN
        ENDIF
C
        IF( NUMBER.EQ.1 )THEN
           PRBNAM = NAME
        ELSE IF( NUMBER.EQ.2 )THEN
           BNDNAM = NAME
        ELSE IF( NUMBER.EQ.3 )THEN
           RNGNAM = NAME
        ELSE IF( NUMBER.EQ.4 )THEN
           RHSNAM = NAME
        ELSE
           SOLNAM = NAME
        ENDIF
      IF( FIRST.LT.LAST )GOTO 100
C
      NAME = SOLNAM
      CALL GRENAM(PRBNAM,RHSNAM,RNGNAM,BNDNAM,NAME)
      RETURN
C
C ** ARENAM ENDS HERE
      END
      SUBROUTINE AGGRG8(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This executes AGGREGAT command.
C       Syntax:  AGGREGAT [ROW | COLUMN [conditional]]
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME,CNAME
      CHARACTER*1   STAT
      CHARACTER*4   ROWCOL
      LOGICAL*1     SW
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C SET ROW|COLUMN [conditional]....USING ADDRIM
      CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
      IF(RCODE.NE.0)RETURN
      LINE = 0
C
C 1. BOUNDS
      VLOWER = 0.
      VUPPER = 0.
      IF(SWDISP)THEN
C COLUMN AGGREGATION
         PRINT *,' Sorry, column aggregation not working yet.'
         RETURN
CC         I1= RCSUB1(2)
CC         N = NCOLS
CC         ROWCOL = 'COL '
      ELSE
C ROW AGGREGATION
         I1= RCSUB1(1)
         N = NROWS
         ROWCOL = 'ROW '
      ENDIF
C
      DO 100 I=I1,N
         CALL GETMAP(ROWCOL,I,SW)
         IF(.NOT.SW)GOTO 100
         CALL GETBND(ROWCOL,I,VL,VU)
         CALL GETSOL(ROWCOL,I,VX,VP,STAT,STNUM)
         IF(VP.GT.0.)THEN
            VLOWER = VLOWER + VP*VL
            VUPPER = VUPPER + VP*VU
         ELSE
C REVERSE LIMITS IN AGGREGATE (NEGATIVE MULTIPLIER)
            VLOWER = VLOWER + VP*VU
            VUPPER = VUPPER + VP*VL
         ENDIF
100   CONTINUE
C
      CLIST = 'Aggregate '//ROWCOL//'range:'
      CALL FSLEN(CLIST,50,LAST)
      CALL FR2C12(RNAME,VLOWER)
      CALL FR2C12(CNAME,VUPPER)
      CLIST(LAST+2:) = RNAME(:12)//' to '//CNAME(:12)
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C
      CLIST = 'Coefficient:'
      CALL FSLEN(CLIST,20,LAST)
      TAB = 35
      CLIST(TAB+2:) = 'Range'
      LAST = TAB+7
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C
      VLOWER = 0.
      VUPPER = 0.
C
C 2. COEFFICIENTS
C
      IF(SWDISP)GOTO 1000
C
C LOOP OVER (ALL) COLUMNS (NOT JUST SUBMATRIX)
      DO 500 J=1,NCOLS
         CALL GETNAM('COL ',J,CNAME)
         CALL GETCOL(J,NZ,MAXLST,ROWLST,VALLST)
         IF(NZ.GT.MAXLST)THEN
            PRINT *,' ** NOT ENOUGH SPACE TO COMPLETE AGGREGATION'
            PRINT *,' ...STOPPED AT COLUMN ',CNAME
            RCODE = 1
            RETURN
         ENDIF
         Z = 0.
C        :...ACCUMULATE AGGREGATE COEF IN DOUBLE PRECISION
         ZC = 0.
C        :...COMPLEMENT OF Z:  REDUCED COST = -P*A = Z + ZC
         NZC = 0
C ::: LOOP OVER COLUMN'S NONZEROES :::
         DO 400 I=1,NZ
            ROW = ROWLST(I)
            CALL GETMAP('ROW ',ROW,SW)
            CALL GETSOL('ROW ',ROW,V,VP,STAT,STNUM)
            IF(SW)THEN
C ROW IS IN SUBMATRIX...ADD TO AGGREGATE COEFFICIENT
               Z = Z + VP*VALLST(I)
            ELSE
C ADD ROW'S CONTRIBUTION TO COMPLEMENT OF AGGREGATE COEF
               ZC = ZC + VP*VALLST(I)
               NZC = NZC+1
            ENDIF
400      CONTINUE
C
         CALL GETSOL('COL ',J,VX,VP,STAT,STNUM)
         IF(STAT.EQ.'B' .AND. NZC.EQ.0)GOTO 500
C    -------------------------------------------------------
C    THE ABOVE SAYS THAT ALL OF THE COLUMN'S ROWS ARE IN THE
C    SUBMATRIX (NZC=0) AND ITS REDUCED COST = 0 (STAT=B).
C    THUS, ITS TRUE AGGREGATE COEFFICIENT = 0 (NO ERROR).
C    -------------------------------------------------------
C
C THEORETICALLY, Z+ZC = P*A(J) = -REDUCED COST = -VP
C ...SET ERROR
         VERR = ABS(VP + Z + ZC)
C SET COEF (V)
         V    = Z
         SW = ABS(V).GE.VTOLAB + VERR
C EXCLUDE COLUMN J FROM SUBMATRIX IF COEF TOO SMALL
         CALL GPUTMP('COL ',J,SW)
         IF(.NOT.SW)GOTO 500
C COLUMN J IS PUT INTO SUBMATRIX BECAUSE ITS AGGREGATE COEFFICIENT
C IS NOT ZERO...PREPARE DISPLAY
         CALL FR2C12(RNAME,V)
         CLIST = RNAME//CNAME
         CALL GETBND('COL ',J,VL,VU)
         IF(V.LT.0.)THEN
C SWITCH BOUNDS
            VTEMP = VL
            VL = -VU
            VU = -VTEMP
            V  = -V
         ENDIF
C NOW RANGE IS [V*VL, V*VU]
         IF(VL.LE.-VINF)THEN
            CLIST(TAB:) = '-*'
            VLOWER = -VINF
         ELSE
            VL = V*VL
            CALL FR2C12(CLIST(TAB:),VL)
            VLOWER = VLOWER + VL
         ENDIF
         IF(VU.GE.VINF)THEN
            CLIST(TAB+15:) = '*'
            VUPPER = VINF
         ELSE
            VU = V*VU
            CALL FR2C12(CLIST(TAB+15:),VU)
            VUPPER = VUPPER + VU
         ENDIF
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
500   CONTINUE
C
      CALL ASETSB('COL ')
      CALL FR2C12(RNAME,VLOWER)
      CALL FR2C12(CNAME,VUPPER)
      CLIST = '         ...Myopic range: '//
     1       RNAME(:12)//' to ' //CNAME(:12)
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*9000)
C
      RETURN
C
1000  CONTINUE
C     ========
      PRINT *,' ** COLUMN AGGREGATION NOT WORKING'
C
9000  RETURN
C
C ** AGGRG8 ENDS HERE
      END
