      PROGRAM MAIN
C Dates:  2-25-94; 3-28-94
C    7-21-95...THIS CONSIDERS THAT MPS FILE (MATNAM) COULD BE BINARY
C              AND RECOGNIZED MODEL FILE
C    8-20-95...Compute phase 1 prices here (also GUPCKW changed)
C    8-28-95...Drop binary MPS and solve no matter what
C
C THIS IS A MAIN PROGRAM THAT USES GUPCKGEN TO CREATE A PACKED FILE.
c (Based on sample.f, which comes with osl.)
C
      IMPLICIT INTEGER (A-U)

C OSL Space
      PARAMETER    (MAXSPC=900 000)
      REAL*8       DSPACE(MAXSPC)
      COMMON/OSLCOM/DSPACE
      INTEGER*4    MSPACE(2*MAXSPC)
      CHARACTER*8  CSPACE(MAXSPC)
      EQUIVALENCE (CSPACE,DSPACE,MSPACE)
C Control Variable Arrays
      INCLUDE(OSLR)
      INCLUDE(OSLI)
      INCLUDE(OSLN)
      INCLUDE(OSLC)

C Local
      PARAMETER  (FILEIN=10, FILEOUT=11)
      CHARACTER*12 FORM
      INTEGER      RTCOD
      LOGICAL      EXIST
      CHARACTER*64 FILNAM,PCKNAM,FileName
      CHARACTER*8  FileType,EKK
      CHARACTER*4  VERSN
      LOGICAL*1    SWMSG
      CHARACTER*1  CHAR
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      CALL GINITL(FILEIN,FILEOUT,VERSN)
      RTCOD = 0
      SWMSG = .TRUE.
10    CONTINUE
      FileName = ' '
      FileType = 'MATRIX'
      EKK      = 'EKKMPS'
      FORM     = 'FORMATTED'
      CLOSE( FILEIN  )
      CLOSE( FILEOUT )
      PRINT *,' Welcome to the OSL-ANALYZE Utility'
C GET FILE NAME
100   CONTINUE
        IF( FileName.EQ.' ' )THEN
           PRINT *,' Enter file name (* to stop): '
        ELSE
           CALL FSLEN(FileName,64,LENFN)
           PRINT *,' File name =',FileName(:LENFN)
           PRINT *,' Enter new name (return to accept, * to stop): '
        ENDIF
        READ(*,'(A64)')FILNAM
        IF( FILNAM.EQ.'*' )STOP
        IF( FILNAM.EQ.' ' )GOTO 190
C USER GAVE ANOTHER FILE NAME
        FileName = FILNAM
      GOTO 100
C     ========
190   CONTINUE
      INQUIRE(FILE=FileName,EXIST=EXIST,ERR=1300)
      IF( .NOT.EXIST )THEN
         PRINT *,' ** ',FileName(:LENFN),' DOES NOT EXIST'
         GOTO 100
      ENDIF
      FileType = 'MATRIX'
      EKK      = 'EKKMPS'
      FORM     = 'FORMATTED'
C GET FILE TYPE
200   CONTINUE
        PRINT *,' '
        PRINT *,' File types are:'
        PRINT *,'   f> Formatted matrix (MPS), created by EKKBCDO'
        PRINT *,'   m> Model, created by EKKPTMD'
        PRINT *,' File type currently = ',FileType
        PRINT *,' Enter new file type (return to accept, * to abort):'
        READ(*,'(A1)')CHAR
        IF( CHAR.EQ.'*' )STOP
        IF( CHAR.EQ.' ' )GOTO 290
        IF( CHAR.EQ.'f' )THEN
           FileType = 'MATRIX'
           EKK      = 'EKKMPS'
           FORM     = 'FORMATTED'
        ELSE IF( CHAR.EQ.'m' )THEN
           FileType = 'MODEL'
           EKK      = 'EKKGTMD'
           FORM     = 'UNFORMATTED'
        ELSE
           PRINT *,' ?',CHAR,' (enter f or m)'
        ENDIF
      GOTO 200
290   CONTINUE
C
      OPEN(FILEIN,FILE=FileName,FORM=FORM,STATUS='OLD',ERR=1301)

C ============== End Preliminaries to get input file ===========
C Now we use OSL

C   Describe work space and allow room for one matrix.
      CALL EKKDSCA(RTCOD,DSPACE,MAXSPC,1)
      IF (RTCOD .NE. 0) CALL CHKRT('EKKDSCA',RTCOD)
C   Describe the model.
      CALL EKKDSCM(RTCOD,DSPACE,1,1)
      IF (RTCOD .NE. 0) CALL CHKRT('EKKDSCM',RTCOD)
C   Set minimization (default but do it anyway) and init timer
      CALL EKKRGET(RTCOD,DSPACE,OSLR,OSLRLN)
      IF (RTCOD .NE. 0) CALL CHKRT('EKKRGET',RTCOD)
      Rmaxmin = +1.0d0
      CALL EKKRSET(RTCOD,DSPACE,OSLR,OSLRLN)
      IF (RTCOD .NE. 0) CALL CHKRT('EKKRSET',RTCOD)
C   Set Log frequency to every 100 iterations
      CALL EKKIGET(RTCOD,DSPACE,OSLI,OSLILN)
      IF (RTCOD .NE. 0) CALL CHKRT('EKKIGET',RTCOD)
      Ilogfreq = 100
      CALL EKKISET(RTCOD,DSPACE,OSLI,OSLILN)
      IF (RTCOD .NE. 0) CALL CHKRT('EKKISET',RTCOD)
      RTCOD = 0

      IF( SWMSG )PRINT *,' Calling ',EKK,'...Unit=',FILEIN
      IF( FileType.EQ.'MATRIX' )THEN
         CALL EKKMPS(RTCOD,DSPACE,FILEIN,1,0)
      ELSE
         CALL EKKGTMD(RTCOD,DSPACE,FILEIN)
      ENDIF
      CLOSE( FILEIN )
      IF (RTCOD .NE. 0)THEN
        CALL CHKRT(EKK,RTCOD)
        GOTO 10
      ENDIF
      IF( SWMSG )PRINT *,' ',FileType(:LENFT),' ',FileName(:LENFN),
     1                   ' has been read with ',EKK,'...Solving LP'

C   Scale the model
      CALL EKKSCAL(RTCOD,DSPACE)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKSCAL',RTCOD)
C   Presolve the problem.
      CALL EKKPRSL(RTCOD,DSPACE,15,3)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKPRSL',RTCOD)
C   Crash the problem.
      CALL EKKCRSH(RTCOD,DSPACE,1)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKCRSH',RTCOD)
C  Solve using simplex: random pricing first, then Devex.
      CALL EKKSSLV(RTCOD,DSPACE,1,2)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKSSLV',RTCOD)
C   Postsolve the problem.
      CALL EKKPSSL(RTCOD,DSPACE,15)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKPSSL',RTCOD)
C  Solve with "values" to guarantee a basic terminal solution.
      CALL EKKSSLV(RTCOD,DSPACE,1,3)
        IF (RTCOD .NE. 0) CALL CHKRT('EKKSSLV',RTCOD)
C =========== End OSL to have matrix + solution in core ===============

      IF( RTCOD.NE.0 )THEN
         PRINT *,' RETURN CODE (RTCOD) =',RTCOD
         PRINT *,' Enter * to abort '
         READ(*,'(A1)')CHAR
         IF( CHAR.EQ.'*' )GOTO 10
      ENDIF

C **************** Create Packed file for ANALYZE *****************

C SET DEFAULT PACKED FILE NAME
      CALL FLOOK(FileName,2,LENFN,'.',BEGEXT)
      PCKNAM = FileName(:BEGEXT)//'PCK'
      IF( SWMSG )THEN
         PRINT *,' Ready to write packed file for ',Cname
         IF( Rmaxmin.LT.0. )THEN
            EKK = 'MAXIMIZE'
         ELSE
            EKK = 'MINIMIZE'
         ENDIF
         PRINT *,' ...',EKK,' ',Cobjective
         PRINT *,' '
      ENDIF

1000  CONTINUE
        PRINT *,' Packed file:',PCKNAM
        PRINT *,' Enter new name (return to accept, * to abort): '
        READ(*,'(A64)') FILNAM
        IF( FILNAM.EQ.' ' )GOTO 1090
        IF( FILNAM.EQ.'*' )GOTO 10
C USER GAVE ANOTHER FILE NAME
         PCKNAM = FILNAM
      GOTO 1000
C     =========
1090  CONTINUE
      CLOSE( FILEOUT )
      CALL FSLEN(PCKNAM,64,LPCKN)
      INQUIRE(FILE=PCKNAM,EXIST=EXIST,ERR=1300)
      IF( EXIST )THEN
         PRINT *,' ',PCKNAM(:LPCKN),' EXISTS...REPLACE (Y/N)? '
1010     READ(*,'(A1)')CHAR
         IF( CHAR.EQ.'N' .OR.  CHAR.EQ.'n' )GOTO 1000
         IF( CHAR.NE.'Y' .AND. CHAR.NE.'y' )THEN
            PRINT *,' Please enter Y (to replace) or N (to rename) '
            GOTO 1010
         ENDIF
      ENDIF
      OPEN( FILEOUT,FILE=PCKNAM,FORM='UNFORMATTED',STATUS='UNKNOWN',
     1      ERR=1302 )
C
      IF( SWMSG )PRINT *,' Begin writing packed file ',PCKNAM(:LPCKN)

      CALL OSLPCK( RTCOD,FILEOUT )
      IF( RTCOD.EQ.0 )THEN
         PRINT *,' ANALYZE packed file written to ',PCKNAM(:LPCKN)
      ELSE
         CALL CHKRT('OSLPCK',RTCOD)
      ENDIF
      GOTO 10
C
C ERRORS
1300  PRINT *,' ** ERROR INQUIRING ABOUT ',FileName(:LENFN)
      GOTO 9000
1301  PRINT *,' ** IO ERROR OPENING ',FileName(:LENFN),
     1        '...UNIT =',FILEIN
      GOTO 9000
1302  PRINT *,' ** IO ERROR OPENING ',PCKNAM(:LPCKN),
     1        '...UNIT =',FILEOUT
      GOTO 9000
C ALL COME HERE
9000  CONTINUE
      PRINT *,' READY FOR ANOTHER MATRIX'
      GOTO 10
9900  CONTINUE
C STOP INDICATED
      STOP
C
C MAIN PROGRAM ENDS HERE
      END
      SUBROUTINE OSLPCK( RTCOD,PCKOUT )
C     =================
      IMPLICIT INTEGER (A-U)
C
C This writes packed file to unit PCKOUT, presumed open as UNFORMATTED.
C
C OSL DECLARATIONS
      INTEGER      RTCOD,PCKOUT
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
      LOGICAL*1    SWMSG/.TRUE./,SWPCK/.TRUE./
      INTEGER      I,ISTAT
      CHARACTER*16 SOLST,OPTNAM
      CHARACTER*16 PSTAT(0:2)/'OPTIMAL','INFEASIBLE','UNBOUNDED'/
      CHARACTER*4  ROWCOL
C ALLOWING 16 CHAR NAMES
      CHARACTER*16 RCNAME,PRBNAM,RHSNAM,RNGNAM,BNDNAM,OBJNAM
      CHARACTER*1  STAT(0:7)/'X','L','U','F','B','I','I','?'/
C                  :...OSL STATUS OF A ROW OR COLUMN
      CHARACTER*1  CHAR
      REAL         LEVEL,PRICE,Dummy
      PARAMETER    (MaxNZ=2000, MaxNrows=5000, Dummy=123456789.)
C                         :              :...Max # rows
C                         :...Max # nonzeroes in a col
      CHARACTER*16 RNAME(MaxNZ)
      INTEGER      RLIST(MaxNZ)
      REAL         VCOEF(MaxNZ)
      REAL*8       ZCOEF(MaxNZ), ZPRICE(MaxNrows)
      EQUIVALENCE  (ZCOEF(1),ZPRICE(1))
C                            ========= USED ONLY IF LP IS INFEASIBLE
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
         CLOSE( PCKOUT )
         RETURN
      ENDIF
      Warnings = 0
C Set sense of optimization
      IF( Rmaxmin.lt.0. )THEN
         OPTNAM = 'MAX'
      ELSE
         OPTNAM = 'MIN'
      ENDIF
C Set problem solution status
      IF( Iprobstat.lt.3 )THEN
         SOLST = PSTAT(Iprobstat)
      ELSE
         IF( Rsumpinf.le.0. )THEN
            SOLST = 'FEASIBLE'
         ELSE
            SOLST = 'Unknown'
         ENDIF
      ENDIF
C Set Names
      PRBNAM = Cname
      RHSNAM = Crhs
      RNGNAM = Crange
      BNDNAM = Cbound
      IF( RNGNAM.EQ.' ' )RNGNAM = 'none'
      IF( BNDNAM.EQ.' ' )BNDNAM = 'none'
      OBJNAM = Cobjective(1:Inumchar)
C Set numbers
      VINF  = 1.0E+20
      VTOLA = 1.0E-10
      VTOLR = 1.0E-12
      NROWS = Inumrows + 1
C                      :...add obj
      NCOLS = Inumcols
      NONZER= Inumels
C             :...does not include obj
C MAKE PASS THRU OBJ COEFS TO TALLY NONZEROES
      DO I=1,NCOLS
         VC = DSPACE( Nobjective + I-1 )
         IF( ABS(VC).GT.VTOLAB )NONZER = NONZER+1
      END DO
C
C Initialize GUPCK
      CALL GUPCK0(PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,OPTNAM,
     1            'OSL ',SOLST, Inumchar,NROWS,NCOLS,NONZER,
     2            VINF,VTOLA,VTOLR,PCKOUT,RTCOD)
      IF( RTCOD.NE.0 )RETURN
C Initialize Rows
      CALL GUPCKR(' ', 0, Dummy,Dummy,Dummy,Dummy,' ',RTCOD)
      IF( RTCOD.NE.0 )RETURN
C Enter each row (except obj)
      DO I=1,Inumrows
         RCNAME=  Cspace( Nrownames + I-1 )(1:Inumchar)
         LEVEL =  DSPACE( Nrowacts  + I-1 )
         PRICE = -DSPACE( Nrowduals + I-1 )
C                :...OSL PUTS WRONG SIGN ON ROW PRICES
         ISTAT = ISHFT( MSPACE(Nrowstat + I-1), -29 )
         CHAR  = STAT(ISTAT)
         VLO   = DSPACE( Nrowlower + I-1 )
         VUP   = DSPACE( Nrowupper + I-1 )
         IF( CHAR.EQ.'X' )THEN
C ROW IS EQUATION...PUT L OR U ACCORDING TO SENSE OF OPT AND SIGN OF PRICE
            IF( (SOLST.EQ.'OPTIMAL') .AND.
     1          ( (OPTNAM.EQ.'MAX' .AND. PRICE.GT.0. ) .OR.
     2            (OPTNAM.EQ.'MIN' .AND. PRICE.LT.0. )
     3        ) )THEN
               CHAR = 'U'
            ELSE
               CHAR = 'L'
            ENDIF
         ELSE IF( CHAR.EQ.'B' )THEN
C ROW IS BASIC...CHECK FEASIBILITY
            IF( LEVEL.LT.VLO .OR. LEVEL.GT.VUP )THEN
               CHAR = 'I'
               IF( SOLST.NE.'INFEASIBLE' )THEN
                  PRINT *,' Warning:  OSL STAT =',SOLST,
     1                    ' but solution is infeasible'
                  PRINT *,' (Changing status = INFEASIBLE)'
                  SOLST = 'INFEASIBLE'
               ENDIF
            ENDIF
         ENDIF
C
         CALL GUPCKR(RCNAME,I,VLO,VUP,LEVEL,PRICE,CHAR,RTCOD)
         IF( RTCOD.NE.0 )RETURN
      END DO
C
C Enter obj
      VLO = -VINF
      VUP =  VINF
      LEVEL = Robjvalue
      IF( OPTNAM.EQ.'MIN' )THEN
         PRICE = -1.
      ELSE
         PRICE = 1.
      ENDIF
      CALL GUPCKR(OBJNAM,NROWS,VLO,VUP,LEVEL,PRICE,'B',RTCOD)
      IF( RTCOD.NE.0 )RETURN
C End rows
      CALL GUPCKR(' ',NROWS+1,Dummy,Dummy,Dummy,Dummy,' ',RTCOD)
      IF( RTCOD.NE.0 )RETURN

C Initialize columns
      NZ = 1
      CALL GUPCKC(' ', 0, NZ,RNAME,VCOEF,Dummy,Dummy,Dummy,Dummy,
     1            ' ',RTCOD)
      IF( RTCOD.NE.0 )RETURN
C
      DO I=1,NCOLS
         RCNAME= Cspace( Ncolnames  + I-1 )(1:Inumchar)
         LEVEL = DSPACE( Ncolsol    + I-1 )
         PRICE = DSPACE( Ncolrcosts + I-1 )
         ISTAT = ISHFT( MSPACE(Ncolstat + I-1), -29 )
         CHAR  = STAT(ISTAT)
         VLO   = DSPACE( Ncollower + I-1 )
         VUP   = DSPACE( Ncolupper + I-1 )
         IF( CHAR.EQ.'X' )THEN
C COLUMN IS FIXED...PUT L OR U ACCORDING TO SENSE OF OPT AND SIGN OF PRICE
            IF( (SOLST.EQ.'OPTIMAL') .AND.
     1          ( (OPTNAM.EQ.'MAX' .AND. PRICE.GT.0.) .OR.
     2            (OPTNAM.EQ.'MIN' .AND. PRICE.LT.0.)
     3        ) )THEN
               CHAR = 'U'
            ELSE
               CHAR = 'L'
            ENDIF
         ELSE IF( CHAR.EQ.'B' )THEN
C COLUMN IS BASIC...CHECK FEASIBILITY
            IF( LEVEL.LT.VLO .OR. LEVEL.GT.VUP )THEN
               CHAR = 'I'
               IF( SOLST.NE.'INFEASIBLE' )THEN
                  PRINT *,' Warning:  OSL STAT =',SOLST,
     1                    ' but solution is infeasible'
                  PRINT *,' (Changing status = INFEASIBLE)'
                  SOLST = 'INFEASIBLE'
               ENDIF
            ENDIF
         ENDIF
C GET COL'S NONZEROES
         NZ = MaxNZ
         CALL EKKCOL(RTCOD,DSPACE,2,I,NZ,ZCOEF,RLIST)
         IF( RTCOD .NE. 0 )CALL CHKRT('EKKCOL',RTCOD)
         IF( NZ.GT.MaxNZ )THEN
            PRINT *,' ** Col ',RCNAME(:Inumchar),' has',NZ,
     1              ' nonzeroes...Max =',MaxNZ
C ** MUST STOP BECAUSE EKKCOL MAY HAVE OVERWRITTEN ZCOEF, RLIST
            STOP
         ENDIF
C ...COPY THEM
         DO L=1,NZ
            VCOEF(L) = ZCOEF(L)
            ROW = RLIST(L)
            RNAME(L) = Cspace( Nrownames + ROW-1 )(1:Inumchar)
         END DO
C ...LOOK AT OBJ
         VC = DSPACE( Nobjective + I-1 )
         IF( ABS(VC).GT.VTOLAB )THEN
C ADD OBJ
            IF( NZ.GT.MaxNZ )THEN
               PRINT *,' ** COL ',RCNAME(:Inumchar),' has',NZ,
     1                 ' nonzeroes...Max =',MaxNZ
               RTCOD = 12
               RETURN
            ENDIF
            NZ = NZ + 1
            RNAME(NZ) = OBJNAM
            VCOEF(NZ) = VC
         ENDIF
C
         CALL GUPCKC(RCNAME,I,NZ,RNAME,VCOEF,VLO,VUP,LEVEL,PRICE,
     1               CHAR,RTCOD)
         IF( RTCOD.NE.0 )RETURN
      END DO
C
C End columns
      CALL GUPCKC(' ',NCOLS+1,1,RNAME,VCOEF,Dummy,Dummy,Dummy,Dummy,
     1            ' ',RTCOD)
      IF( RTCOD.GT.0 )RETURN
C
      IF( RTCOD.LT.0 )THEN
C BASIS COULD NOT BE SETUP, BUT OK TO WRITE PACKED FILE
         RTCOD = 0
      ELSE IF( SOLST.EQ.'INFEASIBLE' .AND. MaxNrows.GE.NROWS )THEN
C BASIS IS SETUP, AND WE MUST COMPUTE PHASE 1 PRICES HERE
C ...FORM PRICE VECTOR
         DO I=1,NROWS
            ZPRICE(I) = 0.
            CALL GETBVP(I,ROWCOL,J)
            CALL GETSOL(ROWCOL,J,VX,VP,CHAR,STNUM)
            IF( CHAR.EQ.'I' )THEN
               CALL GETBND(ROWCOL,J,VL,VU)
               IF( VX.LT.VL )THEN
                  ZPRICE(I) = 1.
               ELSE IF( VX.GT.VU )THEN
                  ZPRICE(I) = -1.
               ENDIF
            ENDIF
         END DO
C ...BTRAN IT
         CALL GBTRAN(ZPRICE,NROWS)
C NOW ZPRICE(i) = PHASE 1 PRICE OF ROW i...PUT THESE INTO THE SOLUTION

         DO I=1,NROWS
            CALL GETSOL('ROW ',I,VX,VP,CHAR,STNUM)
            IF( CHAR.EQ.'L' .OR. CHAR.EQ.'U' )THEN
               VP = ZPRICE(I)
               CALL GPTSOL('ROW ',I,CHAR,VP)
            ENDIF
         END DO
C ...SET REDUCED COSTS OF NONBASIC COLS
         DO J=1,NCOLS
            CALL GETSOL('COL ',J,VX,VP,CHAR,STNUM)
            IF( CHAR.EQ.'L' .OR. CHAR.EQ.'U' )THEN
               CALL GETCOL(J,NZJ,MaxNZ,RLIST,VCOEF)
               IF( NZJ.GT.MaxNZ )THEN
C OH, OH...WE CANNOT FINISH...LET CALLER KNOW
                  IF( SWMSG )PRINT *,
     1              ' Warning: Could not finish price computation',
     2              '...Use BASIS REFRESH in ANALYZE'
                  GOTO 9000
               ENDIF
C LOOP OVER COLUMN J'S NONZEROES TO ACCUMULATE REDUCED COST
               ZDJ = 0.
               DO I=1,NZJ
                  ROW = RLIST(I)
                  ZDJ = ZDJ - ZPRICE(ROW)*VCOEF(I)
               END DO
               VP = ZDJ
C ...OK, STORE IT
               CALL GPTSOL('COL ',J,CHAR,VP)
            ENDIF
         END DO
         IF( SWMSG )PRINT *,' Phase 1 prices have been entered'
      ENDIF
C
9000  CONTINUE
C Finally, write packed file
      CALL GUPCKW(SWMSG,SWPCK,RTCOD)
      RETURN
C
C ** OSLPCK ENDS HERE
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
9000  FORMAT (1X,'********** ',A7,' return code of ',I4,' **********')
      RETURN
      END
