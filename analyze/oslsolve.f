      PROGRAM MAIN
C LAST REVISIONS: 4-03-92  H.J. GREENBERG
C                 5-24-92 Changed X row|col stat to be L or U
C                 7-16-92 Created SUBROUTINE OSLANL, which allows
C                         16 char names (output header changed)
C                 9-22-92 Changed OSLANL to receive DSPACE in common
C                 3-26-93 Fixed '(none)' to be 'none'
C                 6-09-93 Changed header to 2 lines (for lrecl=80)
C
C THIS IS A SIMPLE MAIN PROGRAM THAT WRITES AN OSL SOLUTION FILE
C THAT IS RECOGNIZED BY ANALYZE UNDER AIX (RS 6000).  THE MAIN
C PART OF THIS PROGRAM IS SAMPLE.F, WHICH IS A SAMPLE THAT COMES
C WITH OSL.
C
      IMPLICIT INTEGER (A-U)

C     Space to Use
      PARAMETER    (MAXSPC=900 000)
      REAL*8       DSPACE(MAXSPC)
      COMMON/OSLCOM/DSPACE
      INTEGER*4    MSPACE(2*MAXSPC)
      CHARACTER*8  CSPACE(MAXSPC)
      EQUIVALENCE (CSPACE,DSPACE,MSPACE)

      PARAMETER  (MPSFIL=10, OSLFIL=11)
C FILE UNIT 10 = MATRIX FILE (INPUT); 11 = OSL SOLUTION FILE (OUTPUT)
C (MUST AGREE WITH SCRIPT FILE, oslsolve.s).

C   Control Variable Arrays

      INCLUDE(OSLR)
      INCLUDE(OSLI)
      INCLUDE(OSLN)
      INCLUDE(OSLC)
C
      CHARACTER*1  CHAR
      INTEGER*4    RTCOD
      DATA         RTCOD /0/
C
C   Describe work space and allow room for one matrix.
C
      CALL EKKDSCA(RTCOD,DSPACE,MAXSPC,1)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKDSCA',RTCOD)
C
C   Describe the model.
C
      CALL EKKDSCM(RTCOD,DSPACE,1,1)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKDSCM',RTCOD)
C
C   Set minimization (default but do it anyway) and init timer
C
      CALL EKKRGET(RTCOD,DSPACE,OSLR,OSLRLN)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKRGET',RTCOD)
      Rmaxmin = +1.0d0
      CALL EKKRSET(RTCOD,DSPACE,OSLR,OSLRLN)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKRSET',RTCOD)
C
C   Set Log frequency to every 100 iterations
C
      CALL EKKIGET(RTCOD,DSPACE,OSLI,OSLILN)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKIGET',RTCOD)
      Ilogfreq = 100
      CALL EKKISET(RTCOD,DSPACE,OSLI,OSLILN)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKISET',RTCOD)
C
C   Pass model with MPS File.
C
      CALL EKKMPS(RTCOD,DSPACE,MPSFIL,2,0)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKMPS ',RTCOD)
C
c   Scale the model
C
      CALL EKKSCAL(RTCOD,DSPACE)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKSCAL',RTCOD)
C
C   Presolve the problem.
C
      CALL EKKPRSL(RTCOD,DSPACE,15,3)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKPRSL',RTCOD)
C
C   Crash the problem.
C
      CALL EKKCRSH(RTCOD,DSPACE,1)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKCRSH',RTCOD)
C
C  Solve using simplex: random pricing first, then Devex.
C
      CALL EKKSSLV(RTCOD,DSPACE,1,2)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKSSLV',RTCOD)
C
C   Postsolve the problem.
C
      CALL EKKPSSL(RTCOD,DSPACE,15)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKPSSL',RTCOD)
C
C  Solve with "values" to guarantee a basic terminal solution.
C
      CALL EKKSSLV(RTCOD,DSPACE,1,3)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKSSLV',RTCOD)
C
      PRINT *,' Ready to write solution file for ANALYZE'
      PRINT *,' Press Enter (or any character to abort)'
      READ(*,'(A1)') CHAR
      IF( CHAR.NE.' ' )STOP
C
C  **************** Print the solution for ANALYZE *****************
C
      OPEN( OSLFIL,ERR=1300 )
      CALL OSLANL( RTCOD,OSLFIL )
      IF( RTCOD.EQ.0 )THEN
         PRINT *,' OSL solution file written.'
      ELSE
         CALL CHKRT('OSLANL',RTCOD)
      ENDIF
      STOP
C
1300  PRINT *,' ** I/O ERROR OPENING UNIT',OSLFIL
      STOP
C
C MAIN PROGRAM ENDS HERE
      END
      SUBROUTINE OSLANL( RTCOD,OSLFIL )
C     =================
      IMPLICIT INTEGER (A-U)
C
C This writes OSL solution to unit OSLFIL, presumed open.
C
C OSL DECLARATIONS
      INTEGER      RTCOD,OSLFIL
      PARAMETER    (MAXSPC=900 000)
      REAL*8       DSPACE(  MAXSPC)
      COMMON/OSLCOM/DSPACE
      INTEGER*4    MSPACE(2*MAXSPC)
      CHARACTER*8  CSPACE(  MAXSPC)
      EQUIVALENCE (CSPACE,DSPACE,MSPACE)
C
      INCLUDE(OSLR)
      INCLUDE(OSLI)
      INCLUDE(OSLN)
      INCLUDE(OSLC)
C LOCAL
      INTEGER      I,ISTAT
      CHARACTER*16 SOLSTAT
      CHARACTER*16 PSTAT(0:2)/'OPTIMAL','INFEASIBLE','UNBOUNDED'/
C ALLOWING 16 CHAR NAMES
      CHARACTER*16 RCNAME,PRBNAM,RHSNAM,RNGNAM,BNDNAM
      CHARACTER*1  STAT(0:7)/'X','L','U','F','B','I','I','?'/
C                  :...OSL STATUS OF A ROW OR COLUMN
      CHARACTER*1  CHAR
      CHARACTER*3  OPT
      REAL         LEVEL, PRICE
      INTEGER      Warnings
      PARAMETER    (MaxWarn = 5)
C                             :...Max # warning messages
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C GET INFO
      CALL EKKCGET( RTCOD,DSPACE,OSLC,OSLCLN )
        IF( RTCOD.NE.0 )RETURN
      CALL EKKIGET( RTCOD,DSPACE,OSLI,OSLILN )
        IF( RTCOD.NE.0 )RETURN
      CALL EKKNGET( RTCOD,DSPACE,OSLN,OSLNLN )
        IF( RTCOD.NE.0 )RETURN
      CALL EKKRGET( RTCOD,DSPACE,OSLR,OSLRLN )
        IF( RTCOD.NE.0 )RETURN
C
      IF( Inumchar.LT.1.OR.Inumchar.GT.16 )THEN
         PRINT *,' ** Number of characters in names (Inumchar)'//
     1           ' out of range'
         PRINT *,'   ...',Inumchar,' must be 1 to 16'
         RTCOD = 1
         CLOSE( OSLFIL )
         RETURN
      ENDIF
      Warnings = 0
C Set sense of optimization (OPT)
      IF( Rmaxmin.lt.0. )THEN
         OPT   = 'MAX'
         PRICE =  1.0
      ELSE
         OPT   = 'MIN'
         PRICE = -1.0
      ENDIF
C Set problem solution status
      IF( Iprobstat.lt.3 )THEN
         SOLSTAT = PSTAT(Iprobstat)
      ELSE
         IF( Rsumpinf.le.0. )THEN
            SOLSTAT = 'FEASIBLE'
         ELSE
            SOLSTAT = 'Unknown'
         ENDIF
      ENDIF
C Set Names
      PRBNAM = Cname
      RHSNAM = Crhs
      RNGNAM = Crange
      BNDNAM = Cbound
      IF( RNGNAM.EQ.' ' )RNGNAM = 'none'
      IF( BNDNAM.EQ.' ' )BNDNAM = 'none'
C Write Header
      WRITE(OSLFIL,1,ERR=1300)PRBNAM,OPT,SOLSTAT, RHSNAM,RNGNAM,BNDNAM
1     FORMAT('*  OSL SOLUTION ---> ANALYZE'
     1  /'* NAME',T19,'OPT SOLSTAT'
     2  / 1X, A16, 1X, A3, 1X,A16
     3  /'* RHS',T18,'RANGE',T35,'BOUND'
     4  / 1X,A16,1X, A16,    1X,  A16
     5  /'* ROWS (Including Objective)'  )
C
C ROWS SECTION
C   First the Objective row
      RCNAME = Cobjective(1:Inumchar)
      CHAR   = 'B'
      LEVEL  = Robjvalue
C     PRICE was set above with OPT
      WRITE(OSLFIL,2,ERR=1300) RCNAME,CHAR,LEVEL,PRICE
2     FORMAT( 1X,              A16,1X, A1, 2(1X,G15.6) )
C
C   Now loop over other rows
C
      DO I=1,Inumrows
         RCNAME=  Cspace( Nrownames + I-1 )(1:Inumchar)
         LEVEL =  DSPACE( Nrowacts  + I-1 )
         PRICE = -DSPACE( Nrowduals + I-1 )
C                :...OSL PUTS WRONG SIGN ON ROW PRICES
         ISTAT = ISHFT( MSPACE(Nrowstat + I-1), -29 )
         CHAR  = STAT(ISTAT)
         IF( CHAR.EQ.'X' )THEN
C ROW IS EQUATION...PUT L OR U ACCORDING TO OPT AND SIGN OF PRICE
            IF( (SOLSTAT.EQ.'OPTIMAL') .AND.
     1         ( (OPT.EQ.'MAX' .AND. PRICE.GT.0. )  .OR.
     2           (OPT.EQ.'MIN' .AND. PRICE.LT.0. )
     3         ) )THEN
               CHAR = 'U'
            ELSE
               CHAR = 'L'
            ENDIF
         ELSE IF( CHAR.EQ.'B' )THEN
C ROW IS BASIC...CHECK FEASIBILITY
            VLO = DSPACE( Nrowlower + I-1 )
            VUP = DSPACE( Nrowupper + I-1 )
            IF( LEVEL.LT.VLO .OR. LEVEL.GT.VUP )THEN
               CHAR = 'I'
               IF( SOLSTAT.NE.'INFEASIBLE' )THEN
                  PRINT *,' Warning:  OSL STAT =',SOLSTAT,
     1                    ' but solution is infeasible'
                  PRINT *,' (Changing status = INFEASIBLE)'
                  SOLSTAT = 'INFEASIBLE'
               ENDIF
            ENDIF
         ENDIF
         WRITE(OSLFIL,2,ERR=1300) RCNAME,CHAR,LEVEL,PRICE
         IF( SOLSTAT.EQ.'OPTIMAL' )THEN
C CHECK OPTIMALITY
            IF((CHAR.EQ.'L' .AND. OPT.EQ.'MIN' .AND. PRICE.LT.0.) .OR.
     1         (CHAR.EQ.'U' .AND. OPT.EQ.'MIN' .AND. PRICE.GT.0.) .OR.
     2         (CHAR.EQ.'L' .AND. OPT.EQ.'MAX' .AND. PRICE.GT.0.) .OR.
     3         (CHAR.EQ.'U' .AND. OPT.EQ.'MAX' .AND. PRICE.LT.0.)
     X        ) THEN
              Warnings = Warnings + 1
              IF( Warnings.LT.MaxWarn ) PRINT *,' Warning: Row ',
     1           RCNAME(1:Inumchar),' does not have optimal dual price'
            ENDIF
         ENDIF
      END DO
C
C COLUMNS SECTION
      WRITE( OSLFIL,'(9H* COLUMNS)',ERR=1300)
C
      DO I=1,Inumcols
         RCNAME= Cspace( Ncolnames  + I-1 )(1:Inumchar)
         LEVEL = DSPACE( Ncolsol    + I-1 )
         PRICE = DSPACE( Ncolrcosts + I-1 )
         ISTAT = ISHFT( MSPACE(Ncolstat + I-1), -29 )
         CHAR  = STAT(ISTAT)
         IF( CHAR.EQ.'X' )THEN
C COLUMN IS FIXED...PUT L OR U ACCORDING TO OPT AND SIGN OF PRICE
            IF( (SOLSTAT.EQ.'OPTIMAL') .AND.
     1         ( (OPT.EQ.'MAX' .AND. PRICE.GT.0. )  .OR.
     2           (OPT.EQ.'MIN' .AND. PRICE.LT.0. )
     3         ) )THEN
               CHAR = 'U'
            ELSE
               CHAR = 'L'
            ENDIF
         ELSE IF( CHAR.EQ.'B' )THEN
C COLUMN IS BASIC...CHECK FEASIBILITY
            VLO = DSPACE( Ncollower + I-1 )
            VUP = DSPACE( Ncolupper + I-1 )
            IF( LEVEL.LT.VLO .OR. LEVEL.GT.VUP )THEN
               CHAR = 'I'
               IF( SOLSTAT.NE.'INFEASIBLE' )THEN
                  PRINT *,' Warning:  OSL STAT =',SOLSTAT,
     1                    ' but solution is infeasible'
                  PRINT *,' (Changing status = INFEASIBLE)'
                  SOLSTAT = 'INFEASIBLE'
               ENDIF
            ENDIF
         ENDIF
         WRITE(OSLFIL,2,ERR=1300) RCNAME,CHAR,LEVEL,PRICE
         IF( SOLSTAT.EQ.'OPTIMAL' )THEN
C CHECK OPTIMALITY
            IF((CHAR.EQ.'L' .AND. OPT.EQ.'MIN' .AND. PRICE.LT.0.) .OR.
     1         (CHAR.EQ.'U' .AND. OPT.EQ.'MIN' .AND. PRICE.GT.0.) .OR.
     2         (CHAR.EQ.'L' .AND. OPT.EQ.'MAX' .AND. PRICE.GT.0.) .OR.
     3         (CHAR.EQ.'U' .AND. OPT.EQ.'MAX' .AND. PRICE.LT.0.)
     X        ) THEN
              Warnings = Warnings + 1
              IF( Warnings.LT.MaxWarn ) PRINT *,' Warning: Column ',
     1           RCNAME(1:Inumchar),' does not have optimal dual price'
            ENDIF
         ENDIF
      END DO
C
      IF( Warnings.GT.0 .AND. SOLSTAT.EQ.'OPTIMAL' )THEN
         PRINT *,'...Changing solution status = FEASIBLE (not OPTIMAL)'
         SOLSTAT = 'FEASIBLE'
      ENDIF
C NORMAL RETURN
      CLOSE( OSLFIL )
      RETURN
C
1300  PRINT *,' ** I/O ERROR WRITING UNIT',OSLFIL
      CLOSE( OSLFIL )
      RTCOD = 13
      RETURN
C
C ** OSLANL ENDS HERE
      END
      SUBROUTINE CHKRT(RTNAME,RTCOD)
C********************************************************************
C  This subroutine prints a nonzero return code.
C********************************************************************
C
      CHARACTER*(*) RTNAME
      INTEGER*4     RTCOD
C
      WRITE(6,9000) RTNAME,RTCOD
      RTCOD = 0
C
9000  FORMAT (1X,'********** ',A7,' return code of ',I4,' **********')
      RETURN
      END
