! $Header: m:/default/source/RCS/refine.f,v 1.853 2020/10/29 19:36:23 ESH Exp $
!
! LIQUID FUELS MARKET MODULE (LFMM)
!       FOR THE NATIONAL ENERGY MODELING SYSTEM (NEMS)

!     FILE NAME: refine.f
!     CONTACT:

!     PARAMETERS:
!     VARIABLES:PMMCOM1,PMMOUT,PMMRPT,MPBLK,QBLK,INTOUT,WRENEW,COGEN,EMISSION,
!               CONVFACT,NCNTRL,PARAMETR,OMLBUF,OGSMOUT,NGTDMOUT,NGTDMREP,
!               WFINC2,EMMPARM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!     REFINE: MAIN CONTROLING SUBROUTINE FOR THE LFMM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE REFINE
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'pmmftab'
      include 'pmmout'
      include 'convfact'
      include 'mpblk'
      include 'qblk'
      include 'pmmrpt'
      include 'macout'
      include 'gdxiface'
      include 'gamsglobalsf'
      include 'intout'
      include 'lfmmout'
      include 'ngtdmrep'
      include 'continew'
      include 'tranrep'
      include 'emablk'
      include 'ponroad'
      include 'indrep'
      include 'cogen'

      integer i,j,IR,k,l
      integer JSTEOphaseYr       ! number of adjSTEO phaseout years (use orig adjSTEO for "STEO years" in side cases)
      integer iret
      character*500 cmdline
      character*125 args
      Integer*4 iWaitMS
      LOGICAL ISTATUS, LEXIST, PREEXIST
      integer jret
      CHARACTER*16 cmd
      CHARACTER*16 LFMMSTRING(40)           ! these are passed to intercv.f - see below for wordier message
	  CHARACTER*40 GAMSVERS

      real hhcoef(60)
      real wticoef, hhave, hhavelag, wtiave, wtiavelag, lpgbase
      integer icd

! VARIABLES USED FOR BENCHMARKING HISTORICAL AND STEO fuel use prices
      REAL*4         SIDE_QCLRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QDSRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QELRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QLGRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QNGRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QOTRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QCCRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QRSRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QSGRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QRLRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_CGREFGEN(MNUMCR,MNUMYR,MNUMCGF,2)
      REAL*4         SIDE_CGREFCAP(MNUMCR,MNUMYR,MNUMCGF)
      REAL*4         SIDE_CGREFQ(MNUMCR,MNUMYR,MNUMCGF)

! VARIABLES HOLDING HISTORICAL BENCHMARKING (87$/MMBtu)
      REAL*4         PASINadjHist(MNUMCR)
      REAL*4         PDSCMadjHist(MNUMCR)
      REAL*4         PDSELadjHist(MNUMCR)
      REAL*4         PDSINadjHist(MNUMCR)
      REAL*4         PDSRSadjHist(MNUMCR)
      REAL*4         PDSTRadjHist(MNUMCR)
      REAL*4         PDSTRHWYadjHist(MNUMCR)
      REAL*4         PJFTRadjHist(MNUMCR)
      REAL*4         PMGCMadjHist(MNUMCR)
      REAL*4         PMGINadjHist(MNUMCR)
      REAL*4         PMGTRadjHist(MNUMCR)
      REAL*4         PPFINadjHist(MNUMCR)
      REAL*4         PRHELadjHist(MNUMCR)
      REAL*4         PRLTRadjHist(MNUMCR)
      REAL*4         PRHTRadjHist(MNUMCR)
      REAL*4         PRLCMadjHist(MNUMCR)
      REAL*4         PRLELadjHist(MNUMCR)
      REAL*4         PRLINadjHist(MNUMCR)
      REAL*4         PETTRadjHist(MNUMCR)

! VARIABLES HOLDING STEO BENCHMARKING (87$/MMBtu)
      REAL*4         PASINadjSTEO(MNUMCR)
      REAL*4         PDSCMadjSTEO(MNUMCR)
      REAL*4         PDSELadjSTEO(MNUMCR)
      REAL*4         PDSINadjSTEO(MNUMCR)
      REAL*4         PDSRSadjSTEO(MNUMCR)
      REAL*4         PDSTRadjSTEO(MNUMCR)
      REAL*4         PDSTRHWYadjSTEO(MNUMCR)
      REAL*4         PJFTRadjSTEO(MNUMCR)
      REAL*4         PMGCMadjSTEO(MNUMCR)
      REAL*4         PMGINadjSTEO(MNUMCR)
      REAL*4         PMGTRadjSTEO(MNUMCR)
      REAL*4         PPFINadjSTEO(MNUMCR)
      REAL*4         PRHELadjSTEO(MNUMCR)
      REAL*4         PRLTRadjSTEO(MNUMCR)
      REAL*4         PRHTRadjSTEO(MNUMCR)
      REAL*4         PRLCMadjSTEO(MNUMCR)
      REAL*4         PRLELadjSTEO(MNUMCR)
      REAL*4         PRLINadjSTEO(MNUMCR)
      REAL*4         PETTRadjSTEO(MNUMCR)

      REAL*4         PETHMadjSTEO(MNUMCR)

      COMMON /LFMM_HISTCALIB/ PASINadjHist, PDSCMadjHist, PDSELadjHist, PDSINadjHist,     &
                              PDSRSadjHist, PDSTRadjHist, PDSTRHWYadjHist, PJFTRadjHist,  &
                              PMGCMadjHist, PMGINadjHist, PMGTRadjHist, PPFINadjHist,     &
                              PRHELadjHist, PRHTRadjHist, PRLCMadjHist, PRLELadjHist,     &
                              PRLINadjHist, PETTRadjHist, PRLTRadjHist,                   &
                              PASINadjSTEO, PDSCMadjSTEO, PDSELadjSTEO, PDSINadjSTEO,     &
                              PDSRSadjSTEO, PDSTRadjSTEO, PDSTRHWYadjSTEO, PJFTRadjSTEO,  &
                              PMGCMadjSTEO, PMGINadjSTEO, PMGTRadjSTEO, PPFINadjSTEO,     &
                              PRHELadjSTEO, PRHTRadjSTEO, PRLCMadjSTEO, PRLELadjSTEO,     &
                              PRLINadjSTEO, PETTRadjSTEO, PETHMadjSTEO, PRLTRadjSTEO


      COMMON /LFMM_STEOFUEL/  SIDE_QCLRF, SIDE_QDSRF, SIDE_QELRF, SIDE_QLGRF, SIDE_QNGRF, &
                              SIDE_QOTRF, SIDE_QCCRF, SIDE_QRSRF, SIDE_QSGRF, SIDE_QRLRF, &
                              SIDE_CGREFGEN, SIDE_CGREFCAP, SIDE_CGREFQ

! from pmmcom1:
      INTEGER      HISTCALFYR        ! first hist year for historical calibration
      INTEGER      HISTCALLYR        ! last  hist year for historical calibration
      INTEGER      LFadjsteoON       ! FLAG to overwrite/adj STEO (1=on, 0=off)
      INTEGER      LFadjhistON       ! FLAG to overwrite/adj HIST (1=on, 0=off)
      INTEGER      STEOLYR
      INTEGER      HISTLYR
      COMMON /LFMM_LAST_HIST/ HISTLYR, HISTCALFYR, HISTCALLYR, STEOLYR, LFadjsteoON, LFadjhistON
      REAL    LFGAMVERR
      INTEGER LFGAMVERI, RTOVALUE
      EXTERNAL RTOVALUE
	  EXTERNAL RTOSTRING

      INTEGER      CO2PLFMM

      REAL WEIGHTEDPRICE(MNCRUD+1)
      
!      real resiweights(1:9)/0.172,0.161,-0.102,-0.183,0.111,0.0582,0.0385,-0.0416,0.120/
!      real commweights(1:9)/0.0188,0.0753,-0.0797,-0.121,0.0369,-0.0164,-0.0210,-0.00942,0.0487/
!      real tranweights(1:9)/-0.0233,-0.0159,0.00396,-0.0106,0.0254,0.0200,-0.00514,-0.0240,-0.000260/
!      real resinat, commnat, trannat

      LFMMSTRING(1:40)=(/'Optimal solution', &
                         'Local optimal   ', &
                         'Unbounded model ', &
                         'Infeasible model', &
                         'Local infeasible', &
                         'Ended early inf ', &
                         'Ended early feas', &
                         'Integer solution', &
                         'Ended early non ', &
                         'No feas int sol ', &
                         'License problem ', &
                         'Error - no cause', &
                         'Error - no sol  ', &
                         'No sol returned ', &
                         'Feasible CNS    ', &
                         'Local feas CNS  ', &
                         'Singular CNS    ', &
                         'Unbounded no sol', &
                         'Infeas no sol   ', &
                         'Unspecified LFMM', &
                         'Normal complete ', &
                         'Too many iters  ', &
                         'Too much time   ', &
                         'Solve difficulty', &
                         'Too many evals  ', &
                         'No capability   ', &
                         'Sublicense prob ', &
                         'User interrupt  ', &
                         'Setup failure   ', &
                         'Solver failure  ', &
                         'Internal solve  ', &
                         'Solve skipped   ', &
                         'System failure  ', &
                         'Unspecified LFMM', &
                         'Unspecified LFMM', &
                         'Unspecified LFMM', &
                         'Unspecified LFMM', &
                         'Unspecified LFMM', &
                         'Unspecified LFMM', &
                         'Unspecified LFMM'/)

      IF (CURIYR.EQ.1 .AND. CURITR.EQ.1) CALL RFHIST1
      IF (CURIYR.EQ.1 .AND. CURITR.EQ.1) INQUIRE(FILE='lfshell_err.lst',EXIST=PREEXIST)

      IWaitMs = 1000 * 60 * 15

      IF (CURIYR.GE.21) THEN  !start Refinery Model in 2010

! Call routine to write initial NEMS Fortran variables to GDX, preserves restart into GDX for later comparisons
        IF (CURIYR.EQ.21 .AND. CURITR.EQ.1 .AND. CURIRUN.EQ.1) THEN
            Call Write_INIT_GDX

            DO I = 1,4
              DO J = 1,MNUMYR
                  RFSCREDPRC(I,J) = 0
              ENDDO
            ENDDO
            DO I = 1, MNUMPR
              DO J = 1, MNUMYR
                REFGAIN(I,1,J) = 0.0
                REFGAIN(I,2,J) = 0.0
              ENDDO
           ENDDO

        ENDIF

! ===================== begin setting of PMGTRadjSTEO =======================

      IF (CURIYR .GT. HISTLYR) THEN
        IF (LFadjsteoON .EQ. 1) THEN
         JSTEOphaseYr = 5         ! number of adjSTEO phaseout years (use orig adjSTEO for "STEO years" in side cases)
                                  ! shorten name, and use later, below
         J = JSTEOphaseYr         ! number of adjSTEO phaseout years (use orig adjSTEO for "STEO years" in side cases)
         IF ( (CURIYR .GT. STEOLYR)   .AND. (CURIYR .LE. STEOLYR+J) ) THEN
          IF (CURITR .EQ. 1) THEN

!!!        write(6,*) 'PMGTRadjSTEO1x: ', curiyr+1989, PMGTRadjSTEO(2)

           PMGTRadjSTEO(:) = PMGTRadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +4)

!!!!!      DO I=1,MNUMCR-2
!!!!!        write(6,*) 'PMGTRadjSTEO2x: ', curiyr+1989, PMGTRadjSTEO(2)
!!!!!      ENDDO

          ENDIF   ! end phase out of adjSTEO

         ENDIF   ! end setting of phaseout for adjSTEO

        ELSEIF (CURITR .EQ. 1) THEN
          PMGTRadjSTEO(:) = 0.0

        ENDIF   ! do not adjSTEO

! check if do not want to bench to STEO
        IF (LFadjsteoON .EQ. 0) THEN
          PMGTRadjSTEO(:) = 0.0
        ENDIF
      ENDIF     ! end STEO-adjust of end-use prices

! ===================== end setting of PMGTRadjSTEO =======================

        Call E85_Demand_Curve
! Call routine to write NEMS Fortran variables to GDX for LFMM
        Call Write_GDX

       
        Istatus=.false.
        LFREFRENT = RTOVALUE('REFRENT ',0)
        LFEXPLEASE = RTOVALUE('EXPLEASE',1)
		
		!reading GAMS version path
		call RTOSTRING('GAMSVERS',GAMSVERS)	 
        cmdline=GAMSVERS
        args = 'LFshell.gms lo=2 lf=LFMM.log al=1 errmsg=1'
        if ((CURIYR .GE. LASTYR) .AND. (FCRL .EQ. 1)) then
          args = trim(args) // ' s=lfrestart'
        endif
!        WRITE(6,'(A,3I4)') 'mc6 (CURIYR,LASTYR,FCRL) : ', CURIYR, LASTYR, FCRL
!        WRITE(6,'(A,A)') 'mc6 GAMS args: ', args
        call OSCall(iwaitMS,cmdline,args,iRet)
        If (iret.eq.1) then
             Istatus=.true.
        else
             CONTINO=0            !  set to FAIL
             REASONO='GAMS call failed'
        endif
write (6,'(" The return status from LFMM in ",I4," is ",I2)') curcalyr,iret

! Call routine to read from LFMM GDX to NEMS Fortran variables
        Call Read_GDX

! temp set WTI_PRICE = RFCRUDEWHP for RR2 (mnumpr=2), LtSweet(mncrude=1)
!====== if (curiyr+1989 .gt. 2015)  WTI_PRICE(curiyr) = RFCRUDEWHP(2,1,curiyr)
        if (curiyr+1989 .gt. HISTLYR)  WTI_PRICE(curiyr) = RFCRUDEWHP(2,1,curiyr)

!-----------------------------------------------------------------------------
! Call lfmmsh.bat which appends any GAMS error messages from lfshell.lst into lfdebug.txt
! lffindstr.bat:
!    findstr /I "err" lfshell.lst | findstr /I /V "if(err" | findstr /I /V "LFDBG" >> lfdebug.txt
!    cmd /c exit 0

        cmd='lffindstr.bat'
!        cmd='M:\ogs\pmm_prj\public\lfmmsh.bat'
        call callsys(jret,cmd)
!-----------------------------------------------------------------------------

      ENDIF   ! IF (CURIYR.GE.21) THEN  !start Refinery Model in 2010

! For the record, these are the possible return codes from GAMS
!  1  Optimal solution achieved
!  2  Local optimal solution achieved (Only type found in an NLP)
!  3  Unbounded model found
!  4  Infeasible model found
!  5  Locally infeasible model found (in NLPs)
!  6  Solver terminated early and model was infeasible
!  7  Solver terminated early and model was feasible but not yet optimal
!  8  Integer solution model found
!  9  Solver terminated early with a non integer solution found (only in MIPs)
! 10  No feasible integer solution could be found
! 11  Licensing problem
! 12  Error achieved - No cause known
! 13  Error achieved - No solution attained
! 14  No solution returned from CNS models
! 15  Feasible in a CNS models
! 16  Locally feasible in a CNS models
! 17  Singular in a CNS models
! 18  Unbounded - no solution
! 19  Infeasible - no solution
! insert our own return code, "none of the above":
! 20  Unspecified LFMM

! The next are return codes for setting up the LP.  Note it won't return 21 for normal solver completion.

! 21 NORMAL COMPLETION:  The solver terminated in a normal way.
! 22 ITERATION INTERRUPT:  The solver was interrupted because it used too many iterations. (option iterlim)
! 23 RESOURCE INTERRUPT:  The solver was interrupted because it used too much time. (option reslim)
! 24 TERMINATED BY SOLVER:  The solver encountered difficulty and was unable to continue.
! 25 EVALUATION ERROR LIMIT:  Too many evaluations of nonlinear terms at undetermined values.
! 26 CAPABILITY PROBLEMS:  The solver does not have the capability required by the model.
! 27 LICENSING PROBLEMS:  The solver cannot find the appropriate license key for a specific subsolver.
! 28 USER INTERRUPT:  An interrupt was sent to the solver. (via IDE interrupt button or Control+C command)
! 29 ERROR SETUP FAILURE:  The solver encountered a fatal failure during problem set-up time.
! 30 ERROR SOLVER FAILURE:  The solver encountered a fatal error.
! 31 ERROR INTERNAL SOLVER FAILURE:  The solver encountered an internal fatal error.
! 32 SOLVE PROCESSING SKIPPED:  The entire solve step has been skipped. This happens if execution errors were encountered and the GAMS
! 33 ERROR SYSTEM FAILURE:  A completely unknown or unexpected error condition.


      IF (LFMMCODE .LT. 1 .OR. LFMMCODE .GT. 40) LFMMCODE = 40
      IF (LFMMCODE .GT. 1) THEN
          IF (CONTINO .EQ. 1) THEN
             CONTINO=0            !  set to FAIL
             REASONO=LFMMSTRING(LFMMCODE)
          ENDIF
      ENDIF
      IF (CONTINO .EQ. 1) THEN
        INQUIRE(FILE='lfshell_err.lst',EXIST=LEXIST)
        IF (LEXIST .AND. .NOT. PREEXIST) THEN
           CONTINO=0            !  set to FAIL
           REASONO='lfshell_err file'
        ENDIF
      ENDIF

!      WEIGHTEDPRICE = 0.0
!      DO I=1,MNCRUD
!        WEIGHTEDPRICE(I) =  &
!          P_CRUDE_IMPORTS(1,I,CURIYR+1989) * Q_CRUDE_IMPORTA(1,I,CURIYR+1989) + &
!          P_CRUDE_IMPORTS(2,I,CURIYR+1989) * Q_CRUDE_IMPORTA(2,I,CURIYR+1989) + &
!          P_CRUDE_IMPORTS(3,I,CURIYR+1989) * Q_CRUDE_IMPORTA(3,I,CURIYR+1989) + &
!          P_CRUDE_IMPORTS(4,I,CURIYR+1989) * Q_CRUDE_IMPORTA(4,I,CURIYR+1989) + &
!          P_CRUDE_IMPORTS(5,I,CURIYR+1989) * Q_CRUDE_IMPORTA(5,I,CURIYR+1989) + &
!          P_CRUDE_IMPORTS(6,I,CURIYR+1989) * Q_CRUDE_IMPORTA(6,I,CURIYR+1989) + &
!          P_CRUDE_IMPORTS(7,I,CURIYR+1989) * Q_CRUDE_IMPORTA(7,I,CURIYR+1989) + &
!          P_CRUDE_IMPORTS(8,I,CURIYR+1989) * Q_CRUDE_IMPORTA(8,I,CURIYR+1989)
!        WEIGHTEDPRICE(MNCRUD+1) = WEIGHTEDPRICE(MNCRUD+1) + WEIGHTEDPRICE(I)
!        IF (Q_CRUDE_IMPORTA(MNUMPR,I,CURIYR+1989) .NE. 0.0) THEN
!          P_CRUDE_IMPORTS(MNUMPR,I,CURIYR+1989) = WEIGHTEDPRICE(I) /  &
!                                                  Q_CRUDE_IMPORTA(MNUMPR,I,CURIYR+1989)
!        ELSE
!          P_CRUDE_IMPORTS(MNUMPR,I,CURIYR+1989) = 0.0
!        ENDIF
!      ENDDO
!
!      IF (sum(Q_CRUDE_IMPORTA(MNUMPR,:,CURIYR+1989)) .NE. 0.0) THEN
!         IT_WOP(CURIYR,1) = WEIGHTEDPRICE(MNCRUD+1) / sum(Q_CRUDE_IMPORTA(MNUMPR,:,CURIYR+1989))
!      ELSE
!         IT_WOP(CURIYR,1) = 0.0
!      ENDIF


      IR = MNUMCR
      RFQPRDT(1:10,CURIYR) = 0.0
      RFQPRDT(IR,CURIYR) = RFQLG(IR,CURIYR) + RFQJF(IR,CURIYR) + RFQDS(IR,CURIYR) + &
              RFQMG(IR,CURIYR) + &                                      ! includes 100% of E85
              RFQRL(IR,CURIYR) + RFQRH(IR,CURIYR) + &
              RFQOTH(IR,CURIYR) + RFQKS(IR,CURIYR) + RFQPCK(IR,CURIYR) + &
              RFQPF(IR,CURIYR) + RFQARO(IR,CURIYR) + RFQSTG(IR,CURIYR) + RFMETM85(MNUMPR,CURIYR)

!!!   RFPRDDIESEL(MNUMCR,CURIYR) = REFINE_PROD(1,4,CURIYR) * 1000.

! share it out to the regions based on last historical year
      DO IR=1,9
         IF (RFPRDDIESEL(MNUMCR,HISTLYR) .NE. 0.0) &
            RFPRDDIESEL(IR,CURIYR) = RFPRDDIESEL(MNUMCR,CURIYR) * RFPRDDIESEL(IR,HISTLYR)/RFPRDDIESEL(MNUMCR,HISTLYR)
      ENDDO

      IF (CURIYR.LE.HISTLYR) THEN
        CALL RFHIST1
      ELSE IF (CURIYR.EQ.LASTYR .AND. FCRL.EQ.1) THEN
        CALL RFHIST1
      ENDIF

! set up petcoke prices based off of historical electric power plant coal prices
      CO2PLFMM = RTOVALUE('CO2PLFMM',1)
      do icd = 1,9

         if (CO2PLFMM.EQ.1) then
           ppcin(icd,curiyr) = 0.89 * pclel(icd,curiyr) + JPCIN(curiyr)
         else
           ppcin(icd,curiyr) = 0.89 * pclel(icd,curiyr)
         endif
!        write (199,*) curiyr,'petcoke price (1987$/MMBTU) in region ',icd,' = ',ppcin(icd,curiyr)

      end do
      ppcin(mnumcr,curiyr) = 0.0 ! get national petcoke price as a weighted quantity average
      do icd = 1,9
         ppcin(mnumcr,curiyr) = ppcin(mnumcr,curiyr) + (qpcin(icd,curiyr)/sum(qpcin(1:9,curiyr))) * ppcin(icd,curiyr)
      end do
!     write (199,*) curiyr,'petcoke price national (1987$/MMBTU) = ',ppcin(mnumcr,curiyr)

!===========================================
!
! Begin historical/STEO benchmarking price adjustments
! 12-18-15, em4
!
!===========================================

      IF (CURIYR .GT. HISTLYR) THEN

! 3/8/16 em4 save STEO years for side case overwrites
        IF (STEOLYR .GT. HISTLYR) THEN
         IF ((CURIYR.GT.HISTLYR) .AND. (CURIYR.LE.STEOLYR)) THEN

          J=CURIYR
          DO I =1,MNUMCR
           QCLRF(I,J) = SIDE_QCLRF(I,J)
           QDSRF(I,J) = SIDE_QDSRF(I,J)
           QELRF(I,J) = SIDE_QELRF(I,J)
           QLGRF(I,J) = SIDE_QLGRF(I,J)
           QNGRF(I,J) = SIDE_QNGRF(I,J)
           QOTRF(I,J) = SIDE_QOTRF(I,J)
           QCCRF(I,J) = SIDE_QCCRF(I,J)
           QRSRF(I,J) = SIDE_QRSRF(I,J)
           QSGRF(I,J) = SIDE_QSGRF(I,J)
           QRLRF(I,J) = SIDE_QRLRF(I,J)
          ENDDO

          J=CURIYR
          DO L =1,MNUMCGF                          ! fuel type consumed
           DO I=1,MNUMCR
            DO K=1,2                                   ! self, grid
             CGREFGEN(I,J,L,K) =SIDE_CGREFGEN(I,J,L,K)
            ENDDO
            CGREFCAP(I,J,L)    =SIDE_CGREFCAP(I,J,L)
            CGREFQ(I,J,L)      =SIDE_CGREFQ(I,J,L)
           ENDDO
          ENDDO

         ENDIF
        ENDIF


!!!!!  IF ( CURIYR .EQ. HISTLYR+1) THEN
!!!!!   DO I=1,MNUMCR-2
!!!!!     write(6,*) 'HISTCALFYR2 PRLEL: ',CURIYR+1989,I,PRLEL(I,CURIYR)
!!!!!     write(6,*) 'HISTCALFYR2 PRLEL: ',CURIYR+1989,I,PRLELadjHist(I), PRLELadjSTEO(I)
!!!!!   ENDDO
!!!!!   DO I=1,MNUMCR-2
!!!!!     write(6,*) 'HISTCALFYR2 PDSTR: ',CURIYR+1989,I,PDSTR(I,CURIYR)
!!!!!     write(6,*) 'HISTCALFYR2 PDSTR: ',CURIYR+1989,I,PDSTRadjHist(I), PDSTRadjSTEO(I)
!!!!!   ENDDO
!!!!!   DO I=1,MNUMCR-2
!!!!!     write(6,*) 'HISTCALFYR2 PJFTR: ',CURIYR+1989,I,PJFTR(I,CURIYR)
!!!!!     write(6,*) 'HISTCALFYR2 PJFTR: ',CURIYR+1989,I,PJFTRadjHist(I), PJFTRadjSTEO(I)
!!!!!   ENDDO
!!!!!   DO I=1,MNUMCR-2
!!!!!     write(6,*) 'HISTCALFYR2 PPFIN: ',CURIYR+1989,I,PPFIN(I,CURIYR)
!!!!!     write(6,*) 'HISTCALFYR2 PPFIN: ',CURIYR+1989,I,PPFINadjHist(I), PPFINadjSTEO(I)
!!!!!   ENDDO
!!!!!   DO I=1,MNUMCR-2
!!!!!     write(6,*) 'HISTCALFYR2 PETTR: ',CURIYR+1989,I,PETTR(I,CURIYR)
!!!!!     write(6,*) 'HISTCALFYR2 PETTR: ',CURIYR+1989,I,PETTRadjHist(I), PETTRadjSTEO(I)
!!!!!   ENDDO
!!!!!  ENDIF

! PETHMadjSTEO only available for total US (not by CD)
! and, phase out over (J=)10 years, not 5 as for product prices below
        IF (LFadjsteoON .EQ. 1) THEN
         J = 10        ! number of adjSTEO phaseout years (use orig adjSTEO for "STEO years" in side cases)
         IF ( (CURIYR .GT. STEOLYR)   .AND. (CURIYR .LE. STEOLYR+J) ) THEN
          IF (CURITR .EQ. 1) THEN
           PETHMadjSTEO(MNUMCR) = PETHMadjSTEO(MNUMCR) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
          ENDIF   ! end phase out of adjSTEO

         ENDIF   ! end setting of phaseout for adjSTEO
        ELSEIF (CURITR .EQ. 1) THEN
          PETHMadjSTEO(:) = 0.0
        ENDIF   ! do not adjSTEO


        IF (LFadjsteoON .EQ. 1) THEN
         J = JSTEOphaseYr         ! number of adjSTEO phaseout years (use orig adjSTEO for "STEO years" in side cases)
         IF ( (CURIYR .GT. STEOLYR)   .AND. (CURIYR .LE. STEOLYR+J) ) THEN
          IF (CURITR .EQ. 1) THEN

!!!        write(6,*) 'PMGTRadjSTEO1: ', curiyr+1989, PMGTRadjSTEO(2)

           PASINadjSTEO(:) = PASINadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PDSCMadjSTEO(:) = PDSCMadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PDSELadjSTEO(:) = PDSELadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PDSINadjSTEO(:) = PDSINadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PDSRSadjSTEO(:) = PDSRSadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PDSTRadjSTEO(:) = PDSTRadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PDSTRHWYadjSTEO(:) = PDSTRHWYadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PJFTRadjSTEO(:) = PJFTRadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
 
           PMGCMadjSTEO(:) = PMGCMadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PMGINadjSTEO(:) = PMGINadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
!JSTEOphaseYr PMGTRadjSTEO(:) = PMGTRadjSTEO(:) *    &
!JSTEOphaseYr                 ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
!NO        PPFINadjSTEO(:) = PPFINadjSTEO(:) *    &
!NO                        ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)

!NO        PRHELadjSTEO(:) = PRHELadjSTEO(:) *    &
!NO                        ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
!NO        PRHTRadjSTEO(:) = PRHTRadjSTEO(:) *    &
!NO                        ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PPFINadjSTEO(:) = 0.0
           PRHELadjSTEO(:) = 0.0
           PRLTRadjSTEO(:) = 0.0
           PRHTRadjSTEO(:) = 0.0
           PRLCMadjSTEO(:) = PRLCMadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PRLELadjSTEO(:) = PRLELadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PRLINadjSTEO(:) = PRLINadjSTEO(:) *    &
                           ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
!NO        PETTRadjSTEO(:) = PETTRadjSTEO(:) *    &
!NO                        ((STEOLYR+J) - CURIYR) / ((STEOLYR+J) - CURIYR +1)
           PETTRadjSTEO(:) = 0.0
 
!!!!!      DO I=1,MNUMCR-2
!!!!!        write(6,*) 'HISTCALFYR3 PRLEL: ',CURIYR+1989,I,PRLELadjHist(I), PRLELadjSTEO(I)
!!!!!      ENDDO
!!!!!      DO I=1,MNUMCR-2
!!!!!        write(6,*) 'HISTCALFYR3 PDSTR: ',CURIYR+1989,I,PDSTRadjHist(I), PDSTRadjSTEO(I)
!!!!!      ENDDO
!!!!!      DO I=1,MNUMCR-2
!!!!!        write(6,*) 'HISTCALFYR3 PJFTR: ',CURIYR+1989,I,PJFTRadjHist(I), PJFTRadjSTEO(I)
!!!!!      ENDDO
!!!!!      DO I=1,MNUMCR-2
!!!!!        write(6,*) 'HISTCALFYR3 PMGTR: ',CURIYR+1989,I,PMGTRadjHist(I), PMGTRadjSTEO(I)
!!!!!      ENDDO
!!!!!      DO I=1,MNUMCR-2
!!!!!        write(6,*) 'HISTCALFYR3 PETTR: ',CURIYR+1989,I,PETTRadjHist(I), PETTRadjSTEO(I)
!!!!!      ENDDO

          ENDIF   ! end phase out of adjSTEO

         ENDIF   ! end setting of phaseout for adjSTEO

        ELSEIF (CURITR .EQ. 1) THEN
          PASINadjSTEO(:) = 0.0
          PDSCMadjSTEO(:) = 0.0
          PDSELadjSTEO(:) = 0.0
          PDSINadjSTEO(:) = 0.0
          PDSRSadjSTEO(:) = 0.0
          PDSTRadjSTEO(:) = 0.0
          PDSTRHWYadjSTEO(:) = 0.0
          PJFTRadjSTEO(:) = 0.0

          PMGCMadjSTEO(:) = 0.0
          PMGINadjSTEO(:) = 0.0
          PMGTRadjSTEO(:) = 0.0
          PPFINadjSTEO(:) = 0.0

          PRHELadjSTEO(:) = 0.0
          PRLTRadjSTEO(:) = 0.0
          PRHTRadjSTEO(:) = 0.0
          PRLCMadjSTEO(:) = 0.0
          PRLELadjSTEO(:) = 0.0
          PRLINadjSTEO(:) = 0.0
          PETTRadjSTEO(:) = 0.0

!!!!!     DO I=1,MNUMCR-2
!!!!!       write(6,*) 'HISTCALFYR3 PRLEL: ',CURIYR+1989,I,PRLELadjHist(I), PRLELadjSTEO(I)
!!!!!     ENDDO
!!!!!     DO I=1,MNUMCR-2
!!!!!       write(6,*) 'HISTCALFYR3 PDSTR: ',CURIYR+1989,I,PDSTRadjHist(I), PDSTRadjSTEO(I)
!!!!!     ENDDO
!!!!!     DO I=1,MNUMCR-2
!!!!!       write(6,*) 'HISTCALFYR3 PJFTR: ',CURIYR+1989,I,PJFTRadjHist(I), PJFTRadjSTEO(I)
!!!!!     ENDDO
!!!!!     DO I=1,MNUMCR-2
!!!!!       write(6,*) 'HISTCALFYR3 PPFIN: ',CURIYR+1989,I,PPFINadjHist(I), PPFINadjSTEO(I)
!!!!!     ENDDO
!!!!!     DO I=1,MNUMCR-2
!!!!!       write(6,*) 'HISTCALFYR3 PETTR: ',CURIYR+1989,I,PETTRadjHist(I), PETTRadjSTEO(I)
!!!!!     ENDDO

        ENDIF   ! do not adjSTEO

! check if do not want to bench to STEO
        IF (LFadjsteoON .EQ. 0) THEN
          PASINadjSTEO(:) = 0.0
          PDSCMadjSTEO(:) = 0.0
          PDSELadjSTEO(:) = 0.0
          PDSINadjSTEO(:) = 0.0
          PDSRSadjSTEO(:) = 0.0
          PDSTRadjSTEO(:) = 0.0
          PDSTRHWYadjSTEO(:) = 0.0
          PJFTRadjSTEO(:) = 0.0

          PETHMadjSTEO(:) = 0.0

          PMGCMadjSTEO(:) = 0.0
          PMGINadjSTEO(:) = 0.0
          PMGTRadjSTEO(:) = 0.0
          PPFINadjSTEO(:) = 0.0

          PRHELadjSTEO(:) = 0.0
          PRLTRadjSTEO(:) = 0.0
          PRHTRadjSTEO(:) = 0.0
          PRLCMadjSTEO(:) = 0.0
          PRLELadjSTEO(:) = 0.0
          PRLINadjSTEO(:) = 0.0
          PETTRadjSTEO(:) = 0.0
        ENDIF

! check if do not want to calibrate to hist
!!!     IF (LFadjhistON .EQ. 0) THEN
          PASINadjHIST(:) = 0.0
          PDSCMadjHIST(:) = 0.0
          PDSELadjHIST(:) = 0.0
          PDSINadjHIST(:) = 0.0
          PDSRSadjHIST(:) = 0.0
          PDSTRadjHIST(:) = 0.0
          PDSTRHWYadjHIST(:) = 0.0
          PJFTRadjHIST(:) = 0.0

          PMGCMadjHIST(:) = 0.0
          PMGINadjHIST(:) = 0.0
          PMGTRadjHIST(:) = 0.0
          PPFINadjHIST(:) = 0.0

          PRHELadjHIST(:) = 0.0
          PRLTRadjHIST(:) = 0.0
          PRHTRadjHIST(:) = 0.0
          PRLCMadjHIST(:) = 0.0
          PRLELadjHIST(:) = 0.0
          PRLINadjHIST(:) = 0.0
          PETTRadjHIST(:) = 0.0
!!!     ENDIF

        DO I=1,MNUMCR-2
          PASIN(I,CURIYR) = PASIN(I,CURIYR) + PASINadjHist(I) + PASINadjSTEO(I)
          PDSCM(I,CURIYR) = PDSCM(I,CURIYR) + PDSCMadjHist(I) + PDSCMadjSTEO(I)
          PDSEL(I,CURIYR) = PDSEL(I,CURIYR) + PDSELadjHist(I) + PDSELadjSTEO(I)
          PDSIN(I,CURIYR) = PDSIN(I,CURIYR) + PDSINadjHist(I) + PDSINadjSTEO(I)
          PDSRS(I,CURIYR) = PDSRS(I,CURIYR) + PDSRSadjHist(I) + PDSRSadjSTEO(I)
          PDSTR(I,CURIYR) = PDSTR(I,CURIYR) + PDSTRadjHist(I) + PDSTRadjSTEO(I)
          PDSTRHWY(I,CURIYR) = PDSTRHWY(I,CURIYR) + PDSTRadjHist(I) + PDSTRadjSTEO(I)
          PJFTR(I,CURIYR) = PJFTR(I,CURIYR) + PJFTRadjHist(I) + PJFTRadjSTEO(I)
          PKSCM(I,CURIYR) = PJFTR(I,CURIYR)
          PKSIN(I,CURIYR) = PJFTR(I,CURIYR)
          PKSRS(I,CURIYR) = PJFTR(I,CURIYR)

!      Survey of LPG data discountinued in 2011, so bogus data in rfhist
!      will use new (Aug 2012) Baysian algorithm to define historical/STEO years

          PMGCM(I,CURIYR) = PMGCM(I,CURIYR) + PMGCMadjHist(I) + PMGCMadjSTEO(I)
          PMGIN(I,CURIYR) = PMGIN(I,CURIYR) + PMGINadjHist(I) + PMGINadjSTEO(I)
          PMGTR(I,CURIYR) = PMGTR(I,CURIYR) + PMGTRadjHist(I) + PMGTRadjSTEO(I)
          PPFIN(I,CURIYR) = PPFIN(I,CURIYR) + PPFINadjHist(I) + PPFINadjSTEO(I)
          PRHEL(I,CURIYR) = PRHEL(I,CURIYR) + PRHELadjHist(I) + PRHELadjSTEO(I)
          PRLTR(I,CURIYR) = PRLTR(I,CURIYR) + PRLTRadjHist(I) + PRLTRadjSTEO(I)
          PRHTR(I,CURIYR) = PRHTR(I,CURIYR) + PRHTRadjHist(I) + PRHTRadjSTEO(I)
          PRLCM(I,CURIYR) = PRLCM(I,CURIYR) + PRLCMadjHist(I) + PRLCMadjSTEO(I)
          PRLEL(I,CURIYR) = PRLEL(I,CURIYR) + PRLELadjHist(I) + PRLELadjSTEO(I)
          PRLIN(I,CURIYR) = PRLIN(I,CURIYR) + PRLINadjHist(I) + PRLINadjSTEO(I)
          PETTR(I,CURIYR) = PETTR(I,CURIYR) + PETTRadjHist(I) + PETTRadjSTEO(I)
        ENDDO

      ENDIF     ! end STEO-adjust of end-use prices

      PETHM(MNUMCR,CURIYR) = PETHM(MNUMCR,CURIYR) + PETHMadjSTEO(MNUMCR)


      END                                        !  END OF REFINE SUBROUTINE


!****************************************************************
!*    Subroutine WRITE_GDX
!*
!*    Writes data to GDX for LFMM
!****************************************************************

      SUBROUTINE WRITE_GDX

      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'gdxiface' !DSA!
      include 'gamsglobalsf'
      include 'fdict'
      include 'mpblk'
      include 'qblk'
      include 'emmparm'
      include 'cdsparms'
      include 'uso2grp'
      include 'emission'
      include 'pmmrpt'
      include 'ogsmout'
      include 'lfmmout'
      INTEGER NTIMITR
      PARAMETER (NTIMITR=11)

      integer I,J,K,L

! +++ FILER Declarations:
      INTEGER*4 FUNITI,FUNITO,FRETCD,FRTYPE,FSOURC,FUNFMT
      CHARACTER*100 FNAMEI,FNAMEO
      CHARACTER*18 FNAMEI18
! +++ End FILER Declarations

      LOGICAL NEW
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      FRETCD=0
      FSOURC=1          ! Get variable list from a file
      FNAMEI18='VARLISTL'  ! DD name for input file with variable names
      NEW=.FALSE.
      FUNITI=FILE_MGR('O',FNAMEI18,NEW)
      FUNFMT=5
      FRTYPE=1
      FNAMEI=' '
      FNAMEO='NEM_TO_LFMM1.gdx'

!Temporary patch to fix Nans
      DO I = 1,MNUMCR
        DO J=35,MNUMYR
          IF (ISNAN(QOTAS(I,J))) QOTAS(I,J) = QOTAS(I,46)
          IF (ISNAN(QOTTR(I,J))) QOTTR(I,J) = QOTTR(I,46)
        ENDDO
      ENDDO
      DO I = 1,MNUMCR
        DO J=35,MNUMYR
          IF (ISNAN(POTAS(I,J))) POTAS(I,J) = POTAS(I,46)
          IF (ISNAN(POTTR(I,J))) POTTR(I,J) = POTTR(I,46)
        ENDDO
      ENDDO
      DO I=1,MX_NCOALS
        DO J=1,11
          DO K=1,ECP_D_FPH
            DO L=1,MNUMYR
              IF(ISNAN(XCL_PECP(I,J,K,L))) XCL_PECP(I,J,K,L)=0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      DO I=1,MNUMCR
        DO J=1,MNPOLLUT
          DO K=1,MNUMYR
               IF(ISNAN(EMTRC(I,J,K))) EMTRC(I,J,K)=EMTRC(I,J,K-1)
          ENDDO
        ENDDO
      ENDDO
      DO I=1,MNUMPR
        DO J=1,MNUMYR
               IF(ISNAN(RFIMTP(I,J))) RFIMTP(I,J)=0
        ENDDO
      ENDDO
      DO I=1,MNUMPR
        DO J=1,MNUMYR
          DO K=1,2
               IF(ISNAN(RFPQUFC(I,J,K))) RFPQUFC(I,J,K)=0
          ENDDO
        ENDDO
      ENDDO
      DO I=1,MNCRUD
        DO J=1,MNUMYR
               IF(ISNAN(OGCRDHEAT(I,J))) OGCRDHEAT(I,J)=0
        ENDDO
      ENDDO

   !Write variables to GDX from NEMS restart file
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

! debug - check variable PASIN, RFSCREDPRC
       WRITE(6,1013) CURIRUN,CURCALYR,CURITR,NCRL,(PASIN(I,CURIYR),I=1,MNUMCR)
       WRITE(6,1013) CURIRUN,CURCALYR,CURITR,NCRL,(PDSCM(I,CURIYR),I=1,MNUMCR)
       WRITE(6,1013) CURIRUN,CURCALYR,CURITR,NCRL,(RFSCREDPRC(I,CURIYR),I=1,4)
1013   FORMAT(1X,"CHECK RUN TO LFMM",4(":",I4),11(1x,F9.4))

      End subroutine

!****************************************************************
!*    Subroutine WRITE_GDXyr
!*
!*    Writes data to GDX for LFMM
!****************************************************************

!!!      SUBROUTINE WRITE_GDXyr

!!!      IMPLICIT NONE

!!!      include 'parametr'
!!!      include 'ncntrl'
!!!      include 'gdxiface' !DSA!
!!!      include 'gamsglobalsf'
!!!      include 'fdict'
!!!      include 'mpblk'
!!!      include 'qblk'
!!!      include 'emmparm'
!!!      include 'cdsparms'
!!!      include 'uso2grp'
!!!      include 'emission'
!!!      include 'pmmrpt'
!!!      include 'ogsmout'
!!!      include 'lfmmout'
!!!      INTEGER NTIMITR
!!!      PARAMETER (NTIMITR=11)

     !GDX OUTPUT VARS
!!!      integer I,J,K,L
!!!      CHARACTER*2 YRVAR

! +++ FILER Declarations:
!!!      INTEGER*4 FUNITI,FUNITO,FRETCD,FRTYPE,FSOURC,FUNFMT
!!!      CHARACTER*100 FNAMEI,FNAMEO
!!!      CHARACTER*18 FNAMEI18
! +++ End FILER Declarations

!!!      LOGICAL NEW
!!!      INTEGER FILE_MGR
!!!      EXTERNAL FILE_MGR


!!!      WRITE(YRVAR,'(I2)') CURIYR
!!!      YRVAR=TRIM(ADJUSTL(YRVAR))
!!!      FRETCD=0
!!!      FSOURC=1          ! Get variable list from a file
!!!      FNAMEI18='VARLISTL'  ! DD name for input file with variable names
!!!      NEW=.FALSE.
!!!      FUNITI=FILE_MGR('O',FNAMEI18,NEW)
!!!      FUNFMT=5
!!!      FRTYPE=1
!!!      FNAMEI=' '
!!!      FNAMEO='NEM_TO_LFMM'//YRVAR// '.gdx'
!!!      write(6,*)'dbg ',CURIYR,YRVAR,FNAMEO


!Temporary patch to fix Nans
!!!      DO I = 1,MNUMCR
!!!        DO J=35,MNUMYR
!!!          IF (ISNAN(QOTAS(I,J))) QOTAS(I,J) = QOTAS(I,46)
!!!          IF (ISNAN(QOTTR(I,J))) QOTTR(I,J) = QOTTR(I,46)
!!!        ENDDO
!!!      ENDDO
!!!      DO I = 1,MNUMCR
!!!        DO J=35,MNUMYR
!!!          IF (ISNAN(POTAS(I,J))) POTAS(I,J) = POTAS(I,46)
!!!          IF (ISNAN(POTTR(I,J))) POTTR(I,J) = POTTR(I,46)
!!!        ENDDO
!!!      ENDDO
!!!      DO I=1,MX_NCOALS
!!!        DO J=1,11
!!!          DO K=1,ECP_D_FPH
!!!            DO L=1,MNUMYR
!!!              IF(ISNAN(XCL_PECP(I,J,K,L))) XCL_PECP(I,J,K,L)=0
!!!            ENDDO
!!!          ENDDO
!!!        ENDDO
!!!      ENDDO
!!!       DO I=1,MNUMCR
!!!        DO J=1,MNPOLLUT
!!!          DO K=1,MNUMYR
!!!               IF(ISNAN(EMTRC(I,J,K))) EMTRC(I,J,K)=EMTRC(I,J,K-1)
!!!          ENDDO
!!!        ENDDO
!!!      ENDDO
!!!      DO I=1,MNUMPR
!!!        DO J=1,MNUMYR
!!!               IF(ISNAN(RFIMTP(I,J))) RFIMTP(I,J)=0
!!!        ENDDO
!!!      ENDDO
!!!      DO I=1,MNUMPR
!!!        DO J=1,MNUMYR
!!!          DO K=1,2
!!!               IF(ISNAN(RFPQUFC(I,J,K))) RFPQUFC(I,J,K)=0
!!!          ENDDO
!!!        ENDDO
!!!      ENDDO
!!!      DO I=1,MNCRUD
!!!        DO J=1,MNUMYR
!!!               IF(ISNAN(OGCRDHEAT(I,J))) OGCRDHEAT(I,J)=0
!!!        ENDDO
!!!      ENDDO

!Write variables to GDX from NEMS restart file
!!!      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

! debug - check variable PASIN
!!!       WRITE(6,1013) CURIRUN,CURCALYR,CURITR,NCRL,(PASIN(I,CURIYR),I=1,MNUMCR)
!!!       WRITE(6,1013) CURIRUN,CURCALYR,CURITR,NCRL,(RFSCREDPRC(I,CURIYR),I=1,4)
!!!1013   FORMAT(1X,"CHECK RUN TO LFMM",4(":",I4),11(1x,F9.4))

!!!      End subroutine


!****************************************************************
!*    Subroutine WRITE_INIT_GDX
!*
!*    Writes initial restart data to GDX for LFMM
!****************************************************************

      SUBROUTINE WRITE_INIT_GDX

      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'gdxiface' !DSA!
      include 'gamsglobalsf'
      include 'fdict'
      include 'mpblk'
      include 'lfmmout'
      INTEGER NTIMITR
      PARAMETER (NTIMITR=11)

! +++ FILER Declarations:
      INTEGER*4 FUNITI,FUNITO,FRETCD,FRTYPE,FSOURC,FUNFMT
      CHARACTER*100 FNAMEI,FNAMEO
      CHARACTER*18 FNAMEI18
! +++ End FILER Declarations

      LOGICAL NEW
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      FRETCD=0
      FSOURC=1          ! Get variable list from a file
      FNAMEI18='VARLISTL'  ! DD name for input file with variable names
      NEW=.FALSE.
      FUNITI=FILE_MGR('O',FNAMEI18,NEW)
      FUNFMT=5
      FRTYPE=1
      FNAMEI=' '
      FNAMEO='NEM_TO_LFMM_INIT.gdx'

!Write variables to GDX from NEMS restart file
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

      End subroutine

!****************************************************************
!*    Subroutine READ_GDX
!*
!*    Writes data to GDX for LFMM
!****************************************************************

      SUBROUTINE READ_GDX

      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'gdxiface' !DSA!
      include 'gamsglobalsf'
      include 'fdict'
      include 'mpblk'
      include 'lfmmout'

      integer i

! +++ FILER Declarations:
      INTEGER*4 FUNITI,FUNITO,FRETCD,FRTYPE,FSOURC,FUNFMT
      CHARACTER*100 FNAMEI,FNAMEO
      CHARACTER*18 FNAMEI18
! +++ End FILER Declarations

      LOGICAL NEW
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      FRETCD=0
 !     FSOURC=0

      FSOURC=1          ! Get variable list from a file
      FNAMEI18='VARLISTL'  ! DD name for input file with variable names
 !     FNAMEI18='VARL2N'  ! DD name for input file with variable names
      NEW=.FALSE.
      FUNITO=FILE_MGR('O',FNAMEI18,NEW)

      FUNFMT=5
      FRTYPE=2
!      FNAMEI='NEM_TO_LFMM1.gdx'
      FNAMEI='LFMM_TO_NEMS.gdx'
      FNAMEO=' '
!Read variables from GDX to NEMS restart file
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

      End subroutine read_gdx


!****************************************************************
!*    Subroutine E85_Demand_Curve
!*
!*
!****************************************************************

      SUBROUTINE E85_Demand_Curve

      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'pq'
      include 'macout'
      include 'convfact'
      include 'tranrep'
      include 'mxpblk'
      include 'mxqblk'
      include 'emablk'
      include 'emoblk'
      include 'gdxiface' !DSA!
      include 'gamsglobalsf'

!     -----------
      INTEGER*4  E85_Steps
      PARAMETER (E85_Steps=55)
      INTEGER*4 LFMM_PERIODS
      PARAMETER (LFMM_PERIODS=3)

! VARIABLES HOLDING HISTORICAL BENCHMARKING (87$/MMBtu)
      REAL*4         PASINadjHist(MNUMCR)
      REAL*4         PDSCMadjHist(MNUMCR)
      REAL*4         PDSELadjHist(MNUMCR)
      REAL*4         PDSINadjHist(MNUMCR)
      REAL*4         PDSRSadjHist(MNUMCR)
      REAL*4         PDSTRadjHist(MNUMCR)
      REAL*4         PDSTRHWYadjHist(MNUMCR)
      REAL*4         PJFTRadjHist(MNUMCR)
      REAL*4         PMGCMadjHist(MNUMCR)
      REAL*4         PMGINadjHist(MNUMCR)
      REAL*4         PMGTRadjHist(MNUMCR)
      REAL*4         PPFINadjHist(MNUMCR)
      REAL*4         PRHELadjHist(MNUMCR)
      REAL*4         PRLTRadjHist(MNUMCR)
      REAL*4         PRHTRadjHist(MNUMCR)
      REAL*4         PRLCMadjHist(MNUMCR)
      REAL*4         PRLELadjHist(MNUMCR)
      REAL*4         PRLINadjHist(MNUMCR)
      REAL*4         PETTRadjHist(MNUMCR)

! VARIABLES HOLDING STEO BENCHMARKING (87$/MMBtu)
      REAL*4         PASINadjSTEO(MNUMCR)
      REAL*4         PDSCMadjSTEO(MNUMCR)
      REAL*4         PDSELadjSTEO(MNUMCR)
      REAL*4         PDSINadjSTEO(MNUMCR)
      REAL*4         PDSRSadjSTEO(MNUMCR)
      REAL*4         PDSTRadjSTEO(MNUMCR)
      REAL*4         PDSTRHWYadjSTEO(MNUMCR)
      REAL*4         PJFTRadjSTEO(MNUMCR)
      REAL*4         PMGCMadjSTEO(MNUMCR)
      REAL*4         PMGINadjSTEO(MNUMCR)
      REAL*4         PMGTRadjSTEO(MNUMCR)
      REAL*4         PPFINadjSTEO(MNUMCR)
      REAL*4         PRHELadjSTEO(MNUMCR)
      REAL*4         PRLTRadjSTEO(MNUMCR)
      REAL*4         PRHTRadjSTEO(MNUMCR)
      REAL*4         PRLCMadjSTEO(MNUMCR)
      REAL*4         PRLELadjSTEO(MNUMCR)
      REAL*4         PRLINadjSTEO(MNUMCR)
      REAL*4         PETTRadjSTEO(MNUMCR)
      REAL*4         PETHMadjSTEO(MNUMCR)

      COMMON /LFMM_HISTCALIB/ PASINadjHist, PDSCMadjHist, PDSELadjHist, PDSINadjHist,     &
                              PDSRSadjHist, PDSTRadjHist, PDSTRHWYadjHist, PJFTRadjHist,  &
                              PMGCMadjHist, PMGINadjHist, PMGTRadjHist, PPFINadjHist,     &
                              PRHELadjHist, PRHTRadjHist, PRLCMadjHist, PRLELadjHist,     &
                              PRLINadjHist, PETTRadjHist, PRLTRadjHist,                   &
                              PASINadjSTEO, PDSCMadjSTEO, PDSELadjSTEO, PDSINadjSTEO,     &
                              PDSRSadjSTEO, PDSTRadjSTEO, PDSTRHWYadjSTEO, PJFTRadjSTEO,  &
                              PMGCMadjSTEO, PMGINadjSTEO, PMGTRadjSTEO, PPFINadjSTEO,     &
                              PRHELadjSTEO, PRHTRadjSTEO, PRLCMadjSTEO, PRLELadjSTEO,     &
                              PRLINadjSTEO, PETTRadjSTEO, PETHMadjSTEO, PRLTRadjSTEO

      COMMON /TEMP_LFMMOUT/ P_E85STP_t, Q_E85STP_t, A_E85STP_t, P_E85_IC_t, P_E85_SC_t, Trill_Btu_to_Bgal, Trill_Btu_to_MBBL_CD, &
         D_MMBtu_to_D_BBL_CD, MOGASPRICE

      REAL*4     P_E85STP_t(E85_Steps,MNUMCR,MNXYR)
      REAL*4     Q_E85STP_t(E85_Steps,MNUMCR,MNXYR)     ! length of each step of E85 avail curve, trill Btu/yr
      REAL*4     Q_E85STPcum_t(E85_Steps,MNUMCR,MNXYR)  ! cummulative Q on each step of E85 avail curve, trill Btu/yr
      REAL*4     A_E85STP_t(E85_Steps,MNUMCR,MNXYR)
      REAL*4     P_E85_IC_t(E85_Steps,MNUMCR,MNXYR)
      REAL*4     P_E85_SC_t(E85_Steps,MNUMCR,MNXYR)
      REAL*4     Trill_Btu_to_Bgal(MNXYR)
      REAL*4     Trill_Btu_to_MBBL_CD(MNXYR)
      REAL*4     D_MMBtu_to_D_BBL_CD(MNXYR)
      REAL*4     MOGASPRICE(MNUMCR,MNXYR)
      REAL*4     E85_Percents(E85_Steps) !combines lower & upper
      REAL*4     PoE85(MNUMCR,MNXYR)     !E85 Po for avail curve new design
      REAL*4     QoE85(MNUMCR,MNXYR)     !E85 Qo for avail curve new design
      REAL*4     E85MGexpPrRatio         !E85 to MG expected price ratio
      REAL*4     E85_Lower_ELAS
      REAL*4     E85_Upper_ELAS
      REAL*4     E85_ELAS(E85_Steps)     !combines lower & upper

      REAL*8     MG_PRC
      REAL*8     LFMM_FFVSHR
      REAL*8     Q_FFV(0:MNUMCR,MNXYR), NPV_AVG_QFFV(3)
      REAL*8     Q_E85(0:MNUMCR,MNXYR), NPV_AVG_QE85(3)
      REAL*8     PRCNT_QETTR(3)
      REAL*8     Q_MG(0:MNUMCR,MNXYR), NPV_AVG_QMG(3)
      REAL*8     PVV_DEN, PVV_NUM
      REAL*8     DR
      REAL*8     INT_IC
      REAL*8     INT_SC
      REAL*8     Inc_Avail
      REAL*8     Cost_per_Station
      REAL*8     TRGNE85
      REAL*8     ETHNE85
      REAL*8     Infrastructure_Cost(MNUMCR)
      REAL*8     Total_Stations(MNUMCR)
      REAL*8     Total_Station_Cost
      REAL*8     Temp_SC
      REAL*8     Number_Stations
      REAL*8     E85_Lower_Percents(E85_Steps)
      REAL*8     E85_Upper_Percents(E85_Steps)
      REAL*8     GDP(MNXYR)
      REAL*8     STA_CST_87
      REAL*8     INC_CST_87(MNUMCR)
      REAL*8     E85_Avail(MNUMCR,MNXYR)
      REAL*8     E85AVAIL_Growth(MNUMCR) !regional E85 availability growth rate
      REAL*8     Test_Avail(MNUMCR,MNXYR)
      REAL*8     End_Avail(MNUMCR,MNXYR)
      REAL*8     Step_Price(E85_Steps,MNUMCR,MNXYR)
      REAL*8     Q_E85_Lower(MNUMCR,MNXYR)
      REAL*8     Q_E85_Upper(MNUMCR,MNXYR)
      REAL*8     Q_E85_BP_Start_t(E85_Steps,MNUMCR,MNXYR)
      REAL*8     Q_E85_BP_End_t(E85_Steps,MNUMCR,MNXYR)
      REAL*8     Q_E85_MP_t(E85_Steps,MNUMCR,MNXYR)
      REAL*8     CFETQt(MNXYR)
      REAL*8     CFTGQt(MNXYR)
      REAL*8     CFRGQt(MNXYR)
      REAL*8     Upper_QTY
      REAL*8     Upper_Price
      REAL*8     Lower_QTY
      REAL*8     Lower_Price
      REAL*8     Trial_QTY
      REAL*8     Trial_Price
      REAL*8     Test_Price

      INTEGER*4  E85curveSwitch   ! 1=original E85 curve, 2=NEW E85 curve (hockey stick)
      INTEGER*4  E85_stepsLOOP    ! use curve data to define number of steps on NEW E85 curve
      INTEGER*4  IPER, JPER, LYR, SV_LYR, I_CD, I_Step, Test_Trial, Max_Trial, I_Trial, LCFSOPT, AB32SW, LEGIRA 
      INTEGER*4  NYR_IC
      INTEGER*4  NYR_SC
      INTEGER*4  LFMM_FPH, LFMM_PERIOD(3)
      INTEGER*4  MAP_YEAR_PERIOD(MNXYR)
      INTEGER*4  AEOLSTYR, AYR
      INTEGER*4  Infrastructure_Year, Station_Cost_Year
      INTEGER*4  RTOVALUE
      INTEGER*4  CO2PLFMM, CRUDEXP
      INTEGER*4 How_many, I_read, I_index
      INTEGER*4  E85availLYR                 ! sets last year for historical E85AVAIL data
      LOGICAL NEW
      INTEGER FILE_MGR, INVEST_UNIT
      EXTERNAL FILE_MGR

      INTEGER    I
      character*25 CMNXYR(MNXYR),CMNUMCR(MNUMCR), CE85STP(E85_Steps), CMNUMYR(MNUMYR)
      character*2 x1
      integer*8 pgdxB
      character*25 i2,j2,k,l,m
      logical ok,ok2
      character*100 errMsg
      INTEGER*4 ErrNr

      NEW=.FALSE.
      INVEST_UNIT=FILE_MGR('O','RFINVEST          ',NEW)

      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(I4)') Infrastructure_Year

      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(I4)') Station_Cost_Year

      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(F4.2)') INT_IC
      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(I4)') NYR_IC
      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(F4.2)') INT_SC
      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(I4)') NYR_SC
      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(F6.4)') Inc_Avail
      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(F8.1)') Cost_per_Station

      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(F4.2)') TRGNE85
      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(F4.2)') ETHNE85

      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(I2)') How_many
      DO I_read=1,How_many
         READ(INVEST_UNIT,'(I2,1X,F9.1)') I_index, Infrastructure_Cost(I_index)
      ENDDO

      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(I2)') How_many
      DO I_read=1,How_many
         READ(INVEST_UNIT,'(I2,1X,F9.1)') I_index, Total_Stations(I_index)
      ENDDO

      CALL PMM_NEXTDATA(INVEST_UNIT)
      ! 1 = original curve, 2 = NEW curve DESIGN (Hockey stick curve)
      READ(INVEST_UNIT,'(I2)') E85curveSwitch

! original curve
      IF (E85curveSwitch .EQ. 1) then

! skip data for NEW DESIGN Curve
        CALL PMM_NEXTDATA(INVEST_UNIT)
        CALL PMM_NEXTDATA(INVEST_UNIT)
        CALL PMM_NEXTDATA(INVEST_UNIT)
        CALL PMM_NEXTDATA(INVEST_UNIT)
        CALL PMM_NEXTDATA(INVEST_UNIT)

        CALL PMM_NEXTDATA(INVEST_UNIT)
        READ(INVEST_UNIT,'(I2)') How_many
        DO I_read=1,How_many
           READ(INVEST_UNIT,'(I2,1X,F24.19)') I_index, E85_Lower_Percents(I_index)
        ENDDO

        CALL PMM_NEXTDATA(INVEST_UNIT)
        READ(INVEST_UNIT,'(I2)') How_many
        DO I_read=1,How_many
           READ(INVEST_UNIT,'(I2,1X,F24.19)') I_index, E85_Upper_Percents(I_index)
           E85_stepsLOOP = I_Index
           IF (E85_stepsLOOP .GT. E85_steps) write(6,*) 'ERROR refine: E85_stepsLOOP > E85_steps...CHECK THIS'
           IF (E85_stepsLOOP .GT. E85_steps) write(6,*) 'ERROR refine: E85_stepsLOOP=',E85_stepsLOOP,' E85_steps=',E85_steps
        ENDDO

! NEW curve DESIGN
      ELSEIF (E85curveSwitch .EQ. 2) then
        CALL PMM_NEXTDATA(INVEST_UNIT)
        ! elasticity for lower portion of hockey stick curve
        READ(INVEST_UNIT,'(1X,F5.3)') E85_Lower_ELAS

        CALL PMM_NEXTDATA(INVEST_UNIT)
        ! elasticity for upper portion of hockey stick curve
        READ(INVEST_UNIT,'(1X,F5.3)') E85_Upper_ELAS

        CALL PMM_NEXTDATA(INVEST_UNIT)
        ! E85 to MG expected price ratio
        READ(INVEST_UNIT,'(1X,F5.3)') E85MGexpPrRatio

        CALL PMM_NEXTDATA(INVEST_UNIT)
        E85_Lower_Percents = 0.0
        READ(INVEST_UNIT,'(I2)') How_many
        DO I_read=1,How_many
           READ(INVEST_UNIT,'(I2,1X,F10.5)') I_index, E85_Lower_Percents(I_index)
        ENDDO

        CALL PMM_NEXTDATA(INVEST_UNIT)
        E85_Upper_Percents = 0.0
        READ(INVEST_UNIT,'(I2)') How_many
        DO I_read=1,How_many
           READ(INVEST_UNIT,'(I2,1X,F10.5)') I_index, E85_Upper_Percents(I_index)
           E85_stepsLOOP = I_Index
           IF (E85_stepsLOOP .GT. E85_steps) write(6,*) 'ERROR refine: E85_stepsLOOP > E85_steps...CHECK THIS'
           IF (E85_stepsLOOP .GT. E85_steps) write(6,*) 'ERROR refine: E85_stepsLOOP=',E85_stepsLOOP,' E85_steps=',E85_steps
        ENDDO
        
        CALL PMM_NEXTDATA(INVEST_UNIT)
        READ(INVEST_UNIT,'(I2)') How_many
        DO I_read=1,How_many
          READ(INVEST_UNIT,'(I2,1X,F13.7)') I_index, E85AVAIL_Growth(I_index)
        ENDDO
        
! use above data to fill single array for ELAS
        DO I_Step = 1 , E85_StepsLOOP
          IF (E85_Lower_Percents(I_Step) .GT. 0.0) then
            E85_ELAS(I_Step) = E85_Lower_Elas
            E85_Percents(I_Step) = E85_Lower_Percents(I_Step)
          ELSEIF (E85_Upper_Percents(I_Step) .GT. 0.0) then
            E85_ELAS(I_Step) = E85_Upper_Elas
            E85_Percents(I_Step) = E85_Upper_Percents(I_Step)
          ENDIF
        ENDDO
      ! IF (CURITR.EQ.1) THEN
      !   write(6,*) ' error refine E85_Lower_Elas= ', curiyr+1989, E85_Lower_ELAS
      !   write(6,*) ' error refine E85_Upper_Elas= ', curiyr+1989, E85_Upper_ELAS
      !   write(6,*) ' error refine E85_Elas( 1)  = ', curiyr+1989, E85_ELAS(1)    
      !   write(6,*) ' error refine E85_Elas(11)  = ', curiyr+1989, E85_ELAS(11)    
      ! ENDIF

! skip data for Original Curve
        CALL PMM_NEXTDATA(INVEST_UNIT)
        CALL PMM_NEXTDATA(INVEST_UNIT)
      
      ENDIF  !end special read of E85 curve data

      
      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(I2)') How_many
      DO I_read=1,How_many
         READ(INVEST_UNIT,'(I2,1X,I2)') I_index, LFMM_PERIOD(I_index)
      ENDDO

      CALL PMM_NEXTDATA(INVEST_UNIT)
      READ(INVEST_UNIT,'(F4.2)') DR
      
      CALL PMM_NEXTDATA(INVEST_UNIT)  !
      READ(INVEST_UNIT,'(I2)') How_many
      DO I_read=1,How_many
          READ(INVEST_UNIT,'(11X,I4,8X,9F8.4)') I_index, (E85AVAIL(I,(I_index-1989)),I=1,MNUMCR-2)        ! Read in Historical E85 availability
          E85availLYR = I_index       ! sets last year for historical E85AVAIL data
      ENDDO

      
      INVEST_UNIT=FILE_MGR('C','RFINVEST          ',NEW)

      AEOLSTYR = RTOVALUE("AEOLSTYR",0)  ! Get calendar year for last year of AEO projection MNXYR.
      AYR = AEOLSTYR - 1989
      LYR = CURIYR - 1

      LCFSOPT  = RTOVALUE('LCFSOPT ',0)
      AB32SW   = RTOVALUE('AB32SW  ',1)
      CO2PLFMM = RTOVALUE('CO2PLFMM',1)
      CRUDEXP  = RTOVALUE('CRUDEXP ',0)
      LEGIRA   = RTOVALUE('LEGIRA  ',1)

      MAP_YEAR_PERIOD = 0
      LFMM_FPH = 0
      DO IPER = 1 , LFMM_PERIODS
         LFMM_FPH = LFMM_FPH + LFMM_PERIOD(IPER)
         DO JPER = 1 , LFMM_PERIOD(IPER)
            LYR = LYR + 1
            MAP_YEAR_PERIOD(LYR) = IPER
            IF (LYR .LE. AYR) THEN
               CFETQt(LYR) = CFETQ(LYR)
               CFTGQt(LYR) = CFTGQ(LYR)
               CFRGQt(LYR) = CFRGQ(LYR)
            ELSE
               CFETQt(LYR) = CFETQ(AYR)
               CFTGQt(LYR) = CFTGQ(AYR)
               CFRGQt(LYR) = CFRGQ(AYR)
            END IF
            Trill_Btu_to_Bgal(LYR) = 1.0 / ((1.0 - TRGNE85) * CFETQt(LYR) + TRGNE85 * CFRBOB(MIN(LYR,MNUMYR))) * 42.0 / 1000.0
            Trill_Btu_to_MBBL_CD(LYR) = 1.0 /  ((1.0 - TRGNE85) * CFETQt(LYR) + TRGNE85 * CFRBOB(MIN(LYR,MNUMYR))) / 365.0 * 1000.0
            D_MMBtu_to_D_BBL_CD(LYR) = CFE85Q(MIN(LYR,MNUMYR))
         END DO
      END DO

      DO LYR = 1 , MNXYR
         IF (LYR .LE. AYR) THEN
            GDP(LYR) = MC_JPGDP(LYR)
         ELSE
            GDP(LYR) = GDP(AYR) * 1.02
         END IF
      END DO

      Q_FFV = 0.0
      Q_E85 = 0.0
      Q_MG  = 0.0

      MOGASPRICE = 0.0

      DO I_CD = 1 , MNUMCR - 2
         E85_Avail(I_CD,CURIYR-1) = E85AVAIL(I_CD,CURIYR-1)
      END DO

! ================
! can keep the following loop for original and NEW E85 avail curve design, June 2020
! ================
      DO LYR = CURIYR, CURIYR + LFMM_FPH - 1
         DO I_CD = 1 , MNUMCR - 2
            IF (LYR .LE. AYR) THEN
               Q_FFV(I_CD,LYR) = QFFV(I_CD,LYR)
               Q_E85(I_CD,LYR) = QETTR(I_CD,LYR)
               Q_MG(I_CD,LYR) = QMGAS(I_CD,LYR)

               IF (CO2PLFMM.EQ.1) THEN
                  MOGASPRICE(I_CD,LYR) = PMGTR(I_CD,LYR)
               ELSE
                  MOGASPRICE(I_CD,LYR) = PMGTR(I_CD,LYR) + JMGTR(LYR)
               ENDIF

               E85_Avail(I_CD,LYR) = MAX(E85AVAIL(I_CD,LYR), E85_Avail(I_CD,LYR-1))
            ELSE
               Q_FFV(I_CD,LYR) = XQFFV(I_CD,LYR)
               Q_E85(I_CD,LYR) = XQETTR(I_CD,LYR)
               Q_MG(I_CD,LYR) = XQMGAS(I_CD,LYR)

               IF (CO2PLFMM.EQ.1) THEN
                  MOGASPRICE(I_CD,LYR) = XPMGAS(I_CD,LYR)
               ELSE
                  MOGASPRICE(I_CD,LYR) = XPMGAS(I_CD,LYR) + JMGTR(AYR)
               ENDIF

               E85_Avail(I_CD,LYR) = E85_Avail(I_CD,AYR)
             END IF

          IF (E85curveSwitch .EQ. 1) then    
       
            IF (LYR .LE. 30 ) THEN
               ! Don't let availability rise before 2013 since 2012 and prior are set using historical numbers from the tran model
               Test_Avail(I_CD,LYR) = E85_Avail(I_CD,LYR)
               End_Avail(I_CD,LYR)  = E85_Avail(I_CD,LYR)
            ELSEIF (LYR .LT. 33) THEN
               Test_Avail(I_CD,LYR) = E85_Avail(I_CD,LYR-1)
               End_Avail(I_CD,LYR)  = Max(E85_Avail(I_CD,LYR-1) * 1.1, E85_Avail(I_CD,LYR-1) + 0.01)
             ! End_Avail(I_CD,LYR)  = E85_Avail(I_CD,LYR-1)
            ELSE
               Test_Avail(I_CD,LYR) = E85_Avail(I_CD,LYR-1)
               End_Avail(I_CD,LYR)  = E85_Avail(I_CD,LYR-1) + 0.001
             ! End_Avail(I_CD,LYR)  = E85_Avail(I_CD,LYR-1)
               IF (LYR.LE.61) E85AVAIL(I_CD,LYR) = End_Avail(I_CD,LYR)
            ENDIF
            
!for new curve, E85 avail growth rate:
          ELSEIF (E85curveSwitch .EQ. 2) then
            IF (LYR .LE. 30 ) THEN
               ! Don't let availability rise before 2013 since 2012 and prior are set using historical numbers from the tran model
               Test_Avail(I_CD,LYR) = E85_Avail(I_CD,LYR)
             ! End_Avail(I_CD,LYR)  = E85_Avail(I_CD,LYR)
            ELSEIF (LYR .LT. 33) THEN
               Test_Avail(I_CD,LYR) = E85_Avail(I_CD,LYR-1)
             ! End_Avail(I_CD,LYR)  = E85_Avail(I_CD,LYR-1)
            ELSE
               Test_Avail(I_CD,LYR)  = E85_Avail(I_CD,LYR-1) * (1+ E85AVAIL_Growth(I_CD))
               IF (LYR.LE.61) E85_Avail(I_CD,LYR) = Test_Avail(I_CD,LYR)
            ENDIF
      
          ENDIF
            Q_FFV(0,LYR) = Q_FFV(0,LYR) + Q_FFV(I_CD,LYR)
            Q_E85(0,LYR) = Q_E85(0,LYR) + Q_E85(I_CD,LYR)
            Q_MG(0,LYR)  = Q_MG(0,LYR)  + Q_MG(I_CD,LYR)
         END DO
      END DO
      Step_Price = 0.0

! ================
! can keep the following loop for NEW E85 avail curve design, June 2020
! ================
      SV_LYR = CURIYR - 1
      DO IPER = 1 , LFMM_PERIODS
         PVV_NUM = 0.0
         PVV_DEN = 0.0
         LYR = SV_LYR
         DO JPER = 1 , LFMM_PERIOD(IPER)
            LYR = LYR + 1
            PVV_NUM = PVV_NUM + Q_FFV(0,LYR) / ((1.0 + DR) ** JPER)
            PVV_DEN = PVV_DEN + 1.0 / ((1.0 + DR) ** JPER)
         END DO
         NPV_AVG_QFFV(IPER) = PVV_NUM / PVV_DEN

         PVV_NUM = 0.0
         PVV_DEN = 0.0
         LYR = SV_LYR
         DO JPER = 1 , LFMM_PERIOD(IPER)
            LYR = LYR + 1
            PVV_NUM = PVV_NUM + Q_E85(0,LYR) / ((1.0 + DR) ** JPER)
            PVV_DEN = PVV_DEN + 1.0 / ((1.0 + DR) ** JPER)
         END DO
         NPV_AVG_QE85(IPER) = PVV_NUM / PVV_DEN

         PVV_NUM = 0.0
         PVV_DEN = 0.0
         LYR = SV_LYR
         DO JPER = 1 , LFMM_PERIOD(IPER)
            LYR = LYR + 1
            PVV_NUM = PVV_NUM + Q_MG(0,LYR) / ((1.0 + DR) ** JPER)
            PVV_DEN = PVV_DEN + 1.0 / ((1.0 + DR) ** JPER)
         END DO
         NPV_AVG_QMG(IPER) = PVV_NUM / PVV_DEN

         IF (NPV_AVG_QFFV(IPER) .GT. 0.0) THEN
            PRCNT_QETTR(IPER) = MIN( 0.99 , MAX(0.01 , (NPV_AVG_QE85(IPER) / NPV_AVG_QFFV(IPER)) ) )
         ELSE
            PRCNT_QETTR(IPER) = 0.5
         END IF

         SV_LYR = LYR

      END DO


! ================
! For NEW E85 avail curve design ONLY, June 2020
! ================
      IF (E85curveSwitch .EQ. 2) then   !NEW E85 curve DESIGN
        A_E85STP_t = 0.0
        P_E85STP_t = 0.0
        Q_E85STP_t = 0.0
        Q_E85STPcum_t = 0.0
        PoE85      = 0.0
        QoE85      = 0.0

! ================= begin new design for E85 avail curve ==================
! NEW DESIGN for E85 availability curve
!  - shape of hockey stick around Qo
!  - 10 steps to the left of Qo with flatter slope (elasticity)
!  - 10 steps to the right of Qo with steep slope (elasticity)
!  - Po = 1.2 * mogasprice
!  - Qo calculated in refine.f using TRAN's logit function
! Elas = %change in Q / %change in P


! 1.Define Po Qo, 1987$/MMBtu, trillion Btu/yr

        DO I_CD = 1 , MNUMCR - 2
           LYR = CURIYR - 1
           DO IPER = 1 , LFMM_PERIODS
              DO JPER = 1 , LFMM_PERIOD(IPER)
                 LYR = LYR + 1
                 PoE85(I_CD,LYR) = E85MGexpPrRatio * MOGASPRICE(I_CD,LYR) 
              !  PoE85(I_CD,LYR) =            1.20 * MOGASPRICE(I_CD,LYR) 

              !  IF (CURIYR.EQ.36 .AND. CURITR.EQ.1) THEN    !36=2025
              !   write(6,*) ' error refine PoE85  = ', LYR+1989, I_CD, PoE85(I_CD,LYR)
              !  ENDIF

                 Trial_Price = PoE85(I_CD,LYR)
                 MG_PRC = MOGASPRICE(I_CD,LYR)
                 QoE85(I_CD,LYR) = Q_FFV(I_CD,LYR) * LFMM_FFVSHR(I_CD,Trial_Price,MG_PRC,Test_Avail(I_CD,LYR))

! 2.Define length of each step on curve (trillion Btu/yr)
!   and cummulative Q for each step on curve (trillion Btu/yr)
                 DO I_Step = 1 , E85_StepsLOOP
                    Q_E85STP_t(I_Step,I_CD,LYR) = QoE85(I_CD,LYR) * E85_Percents(I_Step)
                    IF (I_STEP .EQ. 1) THEN 
                       Q_E85STPcum_t(I_Step,I_CD,LYR) = Q_E85STP_t(I_Step,I_CD,LYR) 
                    ELSE
                       Q_E85STPcum_t(I_Step,I_CD,LYR) = Q_E85STPcum_t(I_Step-1,I_CD,LYR) &
                                                      + Q_E85STP_t(I_Step,I_CD,LYR) 
                    ENDIF

! 3.Define price (P) for each step on curve (1987$/MMBtu)
!   using new curve design (hockey stick)
                    IF (E85_ELAS(I_Step) .GT. 0.0 .and. QoE85(I_CD,LYR) .GT. 0.0) then
                       P_E85STP_t(I_Step,I_CD,LYR) = PoE85(I_CD,LYR) *            &
                          ( (1/E85_ELAS(I_Step)) *                                &
                            ((Q_E85STPcum_t(I_Step,I_CD,LYR) - QoE85(I_CD,LYR)) / &
                              QoE85(I_CD,LYR)) +1.0  )
                    ENDIF

                    A_E85STP_t(I_Step,I_CD,LYR) = Test_Avail(I_CD,LYR)
                 ENDDO
              ENDDO
           ENDDO
        ENDDO

      ENDIF       ! end new design for E85 avail curve

! ================= end new design for E85 avail curve ==================



! ================
! For original E85 avail curve design ONLY, June 2020
! ================
      IF (E85curveSwitch .EQ. 1) then      ! original E85 curve design

        Q_E85STP_t = 0.0
        P_E85STP_t = 0.0

        LYR = CURIYR - 1
        DO IPER = 1 , LFMM_PERIODS
           DO JPER = 1 , LFMM_PERIOD(IPER)
              LYR = LYR + 1
              DO I_CD = 1 , MNUMCR - 2

!     Center E85 demand curves around either the maximum of the amount that was used last iteration or 10% of total FFV demand
                 Q_E85_Lower(I_CD,LYR) = max(0.1*Q_FFV(I_CD,LYR),Q_E85(I_CD,LYR))
                 Q_E85_Upper(I_CD,LYR) = Q_FFV(I_CD,LYR) - Q_E85_Lower(I_CD,LYR)
                 DO I_Step = 1 , E85_Steps
                    Q_E85STP_t(I_Step,I_CD,LYR) = E85_Lower_Percents(I_Step) * Q_E85_Lower(I_CD,LYR) + &
                                                  E85_Upper_Percents(I_Step) * Q_E85_Upper(I_CD,LYR)
                 ENDDO
              ENDDO
           ENDDO
        ENDDO

      ENDIF      ! end setting for original E85 curve

! ================
! can keep the following loop for original and NEW E85 avail curve design, June 2020
! ================

!     Calculate Incremental InfraStructure Cost for each E85 Demand Step

      STA_CST_87 = Cost_per_Station / GDP(Station_Cost_Year-1989) * 0.000001
      DO I_CD = 1 , MNUMCR - 2
         INC_CST_87(I_CD) = Infrastructure_Cost(I_CD) / GDP(Infrastructure_Year-1989)
      END DO

      P_E85_IC_t = 0.0
      P_E85_SC_t = 0.0

      LYR = CURIYR - 1
      DO IPER = 1 , LFMM_PERIODS
         DO JPER = 1 , LFMM_PERIOD(IPER)
            LYR = LYR + 1
            DO I_CD = 1 , MNUMCR - 2

               Q_E85_BP_Start_t(1,I_CD,LYR) = 0.0
               DO I_Step = 1 , E85_StepsLOOP
                  IF (I_Step .LT. E85_StepsLOOP) THEN
                     Q_E85_BP_Start_t(I_Step+1,I_CD,LYR) = Q_E85_BP_Start_t(I_Step,I_CD,LYR) + Q_E85STP_t(I_Step,I_CD,LYR)
                  END IF
                  Q_E85_BP_End_t(I_Step,I_CD,LYR) = Q_E85_BP_Start_t(I_Step,I_CD,LYR) + Q_E85STP_t(I_Step,I_CD,LYR)
                  Q_E85_MP_t(I_Step,I_CD,LYR) = 0.5 * (Q_E85_BP_Start_t(I_Step,I_CD,LYR) + Q_E85_BP_End_t(I_Step,I_CD,LYR))
                  IF (Q_E85_MP_t(I_Step,I_CD,LYR) .GT. 0.0) THEN
                     P_E85_IC_t(I_Step,I_CD,LYR) = (Q_E85_MP_t(I_Step,I_CD,LYR) * Trill_Btu_to_Bgal(LYR) * ETHNE85 * INC_CST_87(I_CD) * &
                        ((INT_IC * ((1.0 + INT_IC) ** NYR_IC)) / (((1.0 + INT_IC) ** NYR_IC) - 1.0))) / &
                        Q_E85_MP_t(I_Step,I_CD,LYR)
                  END IF

!                 WRITE(6,1005) CURIRUN, CURIYR+1989, CURITR, LYR+1989, IPER, I_CD, I_Step, Q_E85_BP_Start_t(I_Step,I_CD,LYR), &
!                    Q_E85_MP_t(I_Step,I_CD,LYR), Q_E85_BP_END_t(I_Step,I_CD,LYR), P_E85_IC_t(I_Step,I_CD,LYR)
!1005             FORMAT(1X,"E85_DEMAND_STEPS",7(":",I4),4(":",F12.3))

                  Number_Stations = Total_Stations(I_CD) * (Test_Avail(I_CD,LYR) - E85_Avail(I_CD,LYR-1))
                  Total_Station_Cost = Number_Stations * STA_CST_87 * ((INT_SC  * ((1.0 + INT_SC) ** NYR_SC)) / &
                     (((1.0 + INT_SC) ** NYR_SC) - 1.0))
                  Temp_SC = Total_Station_Cost / Q_E85_MP_t(I_Step,I_CD,LYR)
                  P_E85_SC_t(I_Step,I_CD,LYR) = Temp_SC

               END DO
            END DO
         END DO
      END DO


! ================
! For original E85 avail curve design ONLY, June 2020
! ================
      IF (E85curveSwitch .EQ. 1) then      ! original E85 curve design

        A_E85STP_t = 0.0
        P_E85_SC_t = 0.0
        DO I_CD = 1 , MNUMCR - 2
           LYR = CURIYR - 1
           DO IPER = 1 , LFMM_PERIODS
              DO JPER = 1 , LFMM_PERIOD(IPER)
                 LYR = LYR + 1
                 IF (Q_FFV(I_CD,LYR) .GT. 0.0) THEN
                    DO I_Step = 1 , E85_StepsLOOP
                       Test_Price = 0.0
                       DO WHILE (Test_Avail(I_CD,LYR) .LE. End_Avail(I_CD,LYR))
                          Number_Stations = Total_Stations(I_CD) * (Test_Avail(I_CD,LYR) - E85_Avail(I_CD,LYR-1))
                          Total_Station_Cost = Number_Stations * STA_CST_87 * ((INT_SC  * ((1.0 + INT_SC) ** NYR_SC)) / &
                             (((1.0 + INT_SC) ** NYR_SC) - 1.0))
                          Temp_SC = Total_Station_Cost / Q_E85_MP_t(I_Step,I_CD,LYR)
                          Lower_Price = MOGASPRICE(I_CD,LYR) * 0.001
                          MG_PRC = MOGASPRICE(I_CD,LYR)
                          Upper_QTY = Q_FFV(I_CD,LYR) * LFMM_FFVSHR(I_CD,Lower_Price,MG_PRC,Test_Avail(I_CD,LYR))
                          Upper_Price = MOGASPRICE(I_CD,LYR) * 2.0
                          Lower_QTY = Q_FFV(I_CD,LYR) * LFMM_FFVSHR(I_CD,Upper_Price,MG_PRC,Test_Avail(I_CD,LYR))
                          IF (Q_E85_MP_t(I_Step,I_CD,LYR) .GE. Lower_QTY .AND. Q_E85_MP_t(I_Step,I_CD,LYR) .LE. Upper_Qty) THEN
                             I_Trial = 0
                             Test_Trial = 0
                             Max_Trial = 100
                             DO WHILE (I_Trial .LE. Max_Trial .AND. Test_Trial .EQ. 0)
                                I_Trial = I_Trial + 1
                                Trial_Price = (Upper_Price + Lower_Price) * 0.5
                                Trial_QTY = Q_FFV(I_CD,LYR) * LFMM_FFVSHR(I_CD,Trial_Price,MG_PRC,Test_Avail(I_CD,LYR))
                                IF ((Q_E85_MP_t(I_Step,I_CD,LYR) .GE. Trial_QTY - 0.001) .AND. (Q_E85_MP_t(I_Step,I_CD,LYR) .LE. Trial_QTY + 0.001)) THEN
                                   Test_Trial = 1
                                ELSEIF (Q_E85_MP_t(I_Step,I_CD,LYR) .GE. Trial_QTY) THEN
                                   Lower_QTY = Trial_QTY
                                   Upper_Price = Trial_Price
                                ELSE
                                   Upper_QTY = Trial_QTY
                                   Lower_Price = Trial_Price
                                END IF
                             END DO
                             Test_Price = Trial_Price - P_E85_IC_t(I_Step,I_CD,LYR) - Temp_SC
                          ELSE IF (Q_E85_MP_t(I_Step,I_CD,LYR) .LT. Lower_QTY) THEN
                             Test_Price = Upper_Price - P_E85_IC_t(I_Step,I_CD,LYR) - Temp_SC
                          ELSE IF (Q_E85_MP_t(I_Step,I_CD,LYR) .GT. Upper_Qty) THEN
                             Test_Price = Lower_Price - P_E85_IC_t(I_Step,I_CD,LYR) - Temp_SC
                          END IF
  
                          IF (Test_Price .GT. Step_Price(I_Step,I_CD,LYR)) THEN
                             Step_Price(I_Step,I_CD,LYR) = Test_Price
                             P_E85_SC_t(I_Step,I_CD,LYR) = Temp_SC
                             IF (I_Step .GT. 1) THEN
                                P_E85STP_t(I_Step,I_CD,LYR) = MIN(P_E85STP_t(I_Step-1,I_CD,LYR),Test_Price)
                             ELSE
                                P_E85STP_t(I_Step,I_CD,LYR) = Test_Price
                             END IF
                             A_E85STP_t(I_Step,I_CD,LYR) = MIN(Test_Avail(I_CD,LYR) , End_Avail(I_CD,LYR))
                          END IF
  
                          Test_Avail(I_CD,LYR)  = Test_Avail(I_CD,LYR) + INC_Avail
                       END DO
                       Test_Avail(I_CD,LYR) = Min(A_E85STP_t(I_Step,I_CD,LYR) , End_Avail(I_CD,LYR))
                    END DO
                 END IF
              END DO
           END DO
        END DO

      ENDIF      ! end setting for original E85 curve


    ! IF (CURIYR.EQ.36 .AND. CURITR.EQ.1) THEN    !36=2025
    !   Do I_CD = 1,mnumcr-2
    !     write(6,*) ' error refine PoE85  = ', curiyr+1989, I_CD, PoE85(I_CD,curiyr)
    !   EndDo
    ! ENDIF


      ok = gdxCreate(pgdxB,errMsg)

      IF (.not.ok) then
         write(6,'(a,a)') 'Error creating .GDX file:  ', trim(errMsg)
         stop
      ENDIF
      ok2 = gdxOpenWrite(pgdxB,"E85.gdx","refine.f",ErrNr)
      Write(6,*) 'Opening E85.gdx'

      DO I = 1, MNXYR
         write(CMNXYR(i),*) I+1989
         CMNXYR(i)=TRIM(ADJUSTL(CMNXYR(i)))
      ENDDO
      DO I = 1, MNUMCR
         write(CMNUMCR(i),*) I
         CMNUMCR(i)='CenDiv' // TRIM(ADJUSTL(CMNUMCR(i)))
      ENDDO
      DO I = 1, E85_Steps
         Write(x1,'(I2.2)') I
         CE85STP(i)= 'E85STP_' // x1
      ENDDO

      call WriteGDXElement(pgdxB, 0, 'MNXYR', 'Expectation years',       1, MNXYR, 1, 1, 1, 1,      CMNXYR,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 0, 'CenDiv', 'census regions',         1, MNUMCR, 1, 1, 1, 1,     CMNUMCR,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 0, 'E85_Steps', 'steps for E85 curve', 1, E85_Steps, 1, 1, 1, 1,  CE85STP,j2,k,l,m,'I')

      call WriteGDXElement(pgdxB, 1, 'QoE85', 'Qo for E85 hockey stick curve tril Btu', &
                                      QoE85, MNUMCR,MNXYR, 1, 1, 1, CMNUMCR,CMNXYR,k, l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'PoE85', 'Po for E85 hockey stick curve 87dollar per MMBtu', &
                                      PoE85, MNUMCR,MNXYR, 1, 1, 1, CMNUMCR,CMNXYR,k, l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'E85_StepsLOOP', 'number of steps on E85 curve, orig or new curve type', &
                                      E85_StepsLOOP, 1,1,1,1,1,  i2,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 1, 'E85curveSwitch', 'SWITCH btwn 1-original and 2-new E85 curve shape', &
                                      E85curveSwitch, 1,1,1,1,1,  i2,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 1, 'E85_ELAS', 'Elasticity for each step of hockey stick curve', &
                                      E85_ELAS, E85_Steps, 1, 1, 1, 1, CE85STP,j2,k, l,m,'R')

      call WriteGDXElement(pgdxB, 1, 'PMGTRadjSTEO', 'STEO adj PMGTR 87dollar per MMBtu', &
                                      PMGTRadjSTEO, MNUMCR, 1, 1, 1, 1, CMNUMCR,j2,k, l,m,'R')

      call WriteGDXElement(pgdxB, 1, 'Q_E85STP_t', 'E85 incremental step quantity', &
                                      Q_E85STP_t, E85_Steps,MNUMCR,MNXYR, 1, 1, CE85STP,CMNUMCR,CMNXYR,l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'P_E85STP_t', 'E85 Step Price total', &
                                      P_E85STP_t, E85_Steps,MNUMCR,MNXYR, 1, 1, CE85STP,CMNUMCR,CMNXYR,l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'P_E85_IC_t', 'E85 Step Price - Infrastructure cost', &
                                      P_E85_IC_t, E85_Steps,MNUMCR,MNXYR, 1, 1, CE85STP,CMNUMCR,CMNXYR,l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'P_E85_SC_t', 'E85 Step Price - Station Cost', &
                                      P_E85_SC_t, E85_Steps,MNUMCR,MNXYR, 1, 1, CE85STP,CMNUMCR,CMNXYR,l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'A_E85STP_t', 'E85 Optimal Availability at each step', &
                                      A_E85STP_t, E85_Steps,MNUMCR,MNXYR, 1, 1, CE85STP,CMNUMCR,CMNXYR,l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'Trill_Btu_to_Bgal', 'E85 conversion from trill Btu to billion gallons', &
                                      Trill_Btu_to_Bgal, MNXYR, 1, 1, 1, 1, CMNXYR,j2,k, l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'Trill_Btu_to_MBBL_CD', 'E85 conversion from trill Btu to thous barrels per day', &
                                      Trill_Btu_to_MBBL_CD, MNXYR, 1, 1, 1, 1, CMNXYR,j2,k, l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'D_MMBtu_to_D_BBL_CD', 'E85 conversion factor mmBtu per barrel', &
                                      D_MMBtu_to_D_BBL_CD, MNXYR, 1, 1, 1, 1, CMNXYR,j2,k, l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'MOGASPRICE', 'E85 incremental step quantity', &
                                      MOGASPRICE, MNUMCR,MNXYR, 1, 1, 1, CMNUMCR,CMNXYR,k, l,m,'R')
      call WriteGDXElement(pgdxB, 1, 'LCFSOPT', 'LCFS SWITCH', LCFSOPT, 1,1,1,1,1,  i2,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 1, 'AB32SW', 'AB32 SWITCH', AB32SW, 1,1,1,1,1,  i2,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 1, 'LEGIRA', 'IRA SWITCH', LEGIRA, 1,1,1,1,1,  i2,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 1, 'TAX_FLAG', 'EPMCNTL MAIN TAX SWITCH', TAX_FLAG, 1,1,1,1,1,  i2,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 1, 'ELEC_FLAG', 'EPMCNTL EMM ONLY TAX SWITCH', ELEC_FLAG, 1,1,1,1,1,  i2,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 1, 'TRAN_FLAG', 'EPMCNTL TRAN ONLY TAX SWITCH', TRAN_FLAG, 1,1,1,1,1,  i2,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 1, 'CO2PLFMM', 'CO2PLFMM SWITCH', CO2PLFMM, 1,1,1,1,1,  i2,j2,k,l,m,'I')
      call WriteGDXElement(pgdxB, 1, 'CRUDEXP', 'CRUDEXP SWITCH', CRUDEXP, 1,1,1,1,1,  i2,j2,k,l,m,'I')

      call WriteGDXElement(pgdxB, 1, 'E85availLYR', 'Last year for historical E85AVAIL data', E85availLYR, 1,1,1,1,1,  i2,j2,k,l,m,'I')

      Write(6,'(a)') 'Done with E85 curves.  Closing E85.gdx'
      ok2 = gdxClose(pgdxB)

      write(6,*) 'lfmm y,i,pdscm 1,2: ',curiyr+1989,curitr,pdscm(1,curiyr),pdscm(2,curiyr)
 


      RETURN
      END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! logit function for E85 fuel share
! Use double precision to reduce precision problems
! From a numerical analysis perspective, this is dodgy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      REAL*8 FUNCTION LFMM_FFVSHR(thisRegion,ETHPRICE,MOGASPRICE,AVAIL)

      IMPLICIT NONE

      include 'parametr'
      include 'tranrep'

      REAL*8 AVAIL ! Availability
      REAL*8 ETHPRICE ! E85 price
      REAL*8 MOGASPRICE  ! Gas Price
      REAL*8 ALTPRR  ! E85 price function
      REAL*8 GASPRR  !  gas price function

      INTEGER thisRegion   ! Census region
! ----------

      GASPRR = DEXP( DBLE(0.125 * MOGASPRICE * FCLOGIT4))
      ALTPRR = DEXP( DBLE(FCLOGIT0(thisRegion) + (0.125*ETHPRICE*FCLOGIT1) - (FCLOGIT2 * DEXP(AVAIL*FCLOGIT3))) )
      LFMM_FFVSHR = ALTPRR/(ALTPRR + GASPRR)

      RETURN
      END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE PMM_NEXTDATA(UNITRFH)

      IMPLICIT NONE

      INTEGER*4 UNITRFH
      CHARACTER*1 CH


! READ UNTIL @ IN 1ST COLUMN

      CH = 'A'
      DO WHILE (CH .NE. '@')
        READ(UNITRFH,10) CH
      ENDDO
  10  FORMAT(A1)

      RETURN
      END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!*    Subroutine RFHIST1
!*
!*    Read historical data
!****************************************************************

      SUBROUTINE RFHIST1                         !  START RFHIST1 SUBROU
      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'pmmftab'
      include 'emmparm'
      include 'pmmout'
      include 'pmmrpt'
      include 'cogen'
      include 'convfact'
      include 'wrenew'                           ! Added for Ethanoel energy Wbr 10-22-02
      include 'ponroad'
      include 'mpblk'
      include 'qblk'
      include 'indrep'
      include 'intout'
      include 'macout'
      include 'lfmmout'
      include 'ogsmout'
      include 'steoblock'
      include 'gdxiface' !DSA!
      include 'gamsglobalsf'
      
      INTEGER*4      I
      INTEGER*4      J
      INTEGER*4      K
      INTEGER*4      L
      INTEGER*4      HISTDBUG
      INTEGER*4      RLHISTYR
      INTEGER*4      NEWPSAYR      !/16/

      character*25 CMNUMCR(MNUMCR), CMNUMYR(MNUMYR), CMNUMPR(MNUMPR), C_MM(2)
      integer*8 pgdxB
      character*25 i2,j2,k2,l2,m2
      logical ok,ok2
      character*100 errMsg
      INTEGER*4 ErrNr, MM

      REAL*4         TOT
      REAL*4         ETHIMPST(MNUMYR)
      REAL*4         rfBDPRPUS(MNUMYR)
      REAL*4         rfBDTCPUS(MNUMYR)
      REAL*4         rfEOPRPUS(MNUMYR)
      REAL*4         rfEOTCPUS(MNUMYR)
      REAL*4         BIOPRD1(MNUMYR)
      REAL*4         BIOPRD2(MNUMYR)
      REAL*4         BIOPRD3(MNUMYR)
      REAL*4         BIOPRD4(MNUMYR)
      REAL*4         ETHPRD1(MNUMYR)
      REAL*4         ETHPRD2(MNUMYR)
      REAL*4         ETHPRD3(MNUMYR)
      REAL*4         ETHPRD4(MNUMYR)
      REAL*4         BIOCON1(MNUMYR)
      REAL*4         BIOCON2(MNUMYR)
      REAL*4         TRGPRD(MNUMYR)
      REAL*4         RFGPRD(MNUMYR)
      REAL*4         tempPLG(MNUMCR)

! VARIABLES USED FOR BENCHMARKING HISTORICAL AND STEO fuel use prices
      REAL*4         SIDE_QCLRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QDSRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QELRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QLGRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QNGRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QOTRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QCCRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QRSRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QSGRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_QRLRF(MNUMCR,MNUMYR)
      REAL*4         SIDE_CGREFGEN(MNUMCR,MNUMYR,MNUMCGF,2)
      REAL*4         SIDE_CGREFCAP(MNUMCR,MNUMYR,MNUMCGF)
      REAL*4         SIDE_CGREFQ(MNUMCR,MNUMYR,MNUMCGF)

! VARIABLES USED FOR BENCHMARKING HISTORICAL AND STEO PRICES (87$/MMBtu)
      REAL*4         modelPASIN(MNUMCR,MNUMYR)
      REAL*4         modelPDSCM(MNUMCR,MNUMYR)
      REAL*4         modelPDSEL(MNUMCR,MNUMYR)
      REAL*4         modelPDSIN(MNUMCR,MNUMYR)
      REAL*4         modelPDSRS(MNUMCR,MNUMYR)
      REAL*4         modelPDSTR(MNUMCR,MNUMYR)
      REAL*4         modelPDSTRHWY(MNUMCR,MNUMYR)
      REAL*4         modelPJFTR(MNUMCR,MNUMYR)
      REAL*4         modelPKSCM(MNUMCR,MNUMYR)
      REAL*4         modelPKSIN(MNUMCR,MNUMYR)
      REAL*4         modelPKSRS(MNUMCR,MNUMYR)
      REAL*4         modelPLGCM(MNUMCR,MNUMYR)
      REAL*4         modelPLGIN(MNUMCR,MNUMYR)
      REAL*4         modelPLGRS(MNUMCR,MNUMYR)
      REAL*4         modelPLGTR(MNUMCR,MNUMYR)
      REAL*4         modelPMGCM(MNUMCR,MNUMYR)
      REAL*4         modelPMGIN(MNUMCR,MNUMYR)
      REAL*4         modelPMGTR(MNUMCR,MNUMYR)
      REAL*4         modelPPFIN(MNUMCR,MNUMYR)
      REAL*4         modelPRHEL(MNUMCR,MNUMYR)
      REAL*4         modelPRLTR(MNUMCR,MNUMYR)
      REAL*4         modelPRHTR(MNUMCR,MNUMYR)
      REAL*4         modelPRLCM(MNUMCR,MNUMYR)
      REAL*4         modelPRLEL(MNUMCR,MNUMYR)
      REAL*4         modelPRLIN(MNUMCR,MNUMYR)
      REAL*4         modelPETTR(MNUMCR,MNUMYR)

      REAL*4         modelPETHM(MNUMCR,MNUMYR)

      REAL*4         tempPETTR(MNUMCR,MNUMYR)
      REAL*4         tempPMGTR(MNUMCR,MNUMYR)

! VARIABLES HOLDING HISTORICAL BENCHMARKING (87$/MMBtu)
      REAL*4         PASINadjHist(MNUMCR)
      REAL*4         PDSCMadjHist(MNUMCR)
      REAL*4         PDSELadjHist(MNUMCR)
      REAL*4         PDSINadjHist(MNUMCR)
      REAL*4         PDSRSadjHist(MNUMCR)
      REAL*4         PDSTRadjHist(MNUMCR)
      REAL*4         PDSTRHWYadjHist(MNUMCR)
      REAL*4         PJFTRadjHist(MNUMCR)
      REAL*4         PMGCMadjHist(MNUMCR)
      REAL*4         PMGINadjHist(MNUMCR)
      REAL*4         PMGTRadjHist(MNUMCR)
      REAL*4         PPFINadjHist(MNUMCR)
      REAL*4         PRHELadjHist(MNUMCR)
      REAL*4         PRLTRadjHist(MNUMCR)
      REAL*4         PRHTRadjHist(MNUMCR)
      REAL*4         PRLCMadjHist(MNUMCR)
      REAL*4         PRLELadjHist(MNUMCR)
      REAL*4         PRLINadjHist(MNUMCR)
      REAL*4         PETTRadjHist(MNUMCR)

! VARIABLES HOLDING STEO BENCHMARKING (87$/MMBtu)
      REAL*4         PASINadjSTEO(MNUMCR)
      REAL*4         PDSCMadjSTEO(MNUMCR)
      REAL*4         PDSELadjSTEO(MNUMCR)
      REAL*4         PDSINadjSTEO(MNUMCR)
      REAL*4         PDSRSadjSTEO(MNUMCR)
      REAL*4         PDSTRadjSTEO(MNUMCR)
      REAL*4         PDSTRHWYadjSTEO(MNUMCR)
      REAL*4         PJFTRadjSTEO(MNUMCR)
      REAL*4         PMGCMadjSTEO(MNUMCR)
      REAL*4         PMGINadjSTEO(MNUMCR)
      REAL*4         PMGTRadjSTEO(MNUMCR)
      REAL*4         PPFINadjSTEO(MNUMCR)
      REAL*4         PRHELadjSTEO(MNUMCR)
      REAL*4         PRLTRadjSTEO(MNUMCR)
      REAL*4         PRHTRadjSTEO(MNUMCR)
      REAL*4         PRLCMadjSTEO(MNUMCR)
      REAL*4         PRLELadjSTEO(MNUMCR)
      REAL*4         PRLINadjSTEO(MNUMCR)
      REAL*4         PETTRadjSTEO(MNUMCR)

      REAL*4         PETHMadjSTEO(MNUMCR)

      COMMON /LFMM_HISTCALIB/ PASINadjHist, PDSCMadjHist, PDSELadjHist, PDSINadjHist,     &
                              PDSRSadjHist, PDSTRadjHist, PDSTRHWYadjHist, PJFTRadjHist,  &
                              PMGCMadjHist, PMGINadjHist, PMGTRadjHist, PPFINadjHist,     &
                              PRHELadjHist, PRHTRadjHist, PRLCMadjHist, PRLELadjHist,     &
                              PRLINadjHist, PETTRadjHist, PRLTRadjHist,                   &
                              PASINadjSTEO, PDSCMadjSTEO, PDSELadjSTEO, PDSINadjSTEO,     &
                              PDSRSadjSTEO, PDSTRadjSTEO, PDSTRHWYadjSTEO, PJFTRadjSTEO,  &
                              PMGCMadjSTEO, PMGINadjSTEO, PMGTRadjSTEO, PPFINadjSTEO,     &
                              PRHELadjSTEO, PRHTRadjSTEO, PRLCMadjSTEO, PRLELadjSTEO,     &
                              PRLINadjSTEO, PETTRadjSTEO, PETHMadjSTEO, PRLTRadjSTEO

      COMMON /LFMM_STEOFUEL/  SIDE_QCLRF, SIDE_QDSRF, SIDE_QELRF, SIDE_QLGRF, SIDE_QNGRF, &
                              SIDE_QOTRF, SIDE_QCCRF, SIDE_QRSRF, SIDE_QSGRF, SIDE_QRLRF, &
                              SIDE_CGREFGEN, SIDE_CGREFCAP, SIDE_CGREFQ

! from include file pmmcom1:
      REAL           RFETHMCT(MNUMPR,MNUMYR)    ! MERCHANT ETHANOL CONSUMP
      REAL           OTHPRDSP(MNUMPR,MNUMYR)    ! Total Other Liquids Product Supplied
      REAL           RFETHETB(MNUMPR,MNUMYR)    ! QUANT. E85
      REAL           CRDOTHTOT(MNUMPR,MNUMYR)   ! OTH CRD TOT
      REAL           CRDUNACC(MNUMPR,MNUMYR)    ! UNACCNT CRD
      REAL           CRDSTWDR(MNUMPR,MNUMYR)    ! CRD STCK WITHDWL
      LOGICAL   STILL_READ, DO_ONCE/.TRUE./
      CHARACTER*35 Label
      LOGICAL   L_OVWBSYR             ! switch to overwrite STEO

      CHARACTER*1 DUMMY
      REAL*4         SBO_lbgal, CRN_dummy
      REAL*4         YGR_lbgal, YGR_dummy
      Integer*4    CDcol
      Integer*4    SBO_hyr, SBO_hyr1, SBO_hyr2, CDindex(MNUMCR)
      Integer*4    CRN_hyr, CRN_hyr1, CRN_hyr2
      Integer*4    YGR_hyr, YGR_hyr1, YGR_hyr2

      INTEGER      OGDIST2REFREG(REFREG,OGDIST)
      INTEGER      PMMRGNS, DOMREFREG
      INTEGER      DMDRGNS
      INTEGER      PMMBSYR
      INTEGER      HISTCALFYR        ! first hist year for historical calibration
      INTEGER      HISTCALLYR        ! last  hist year for historical calibration
      INTEGER      LFadjsteoON       ! FLAG to overwrite/adj STEO (1=on, 0=off)
      INTEGER      LFadjhistON       ! FLAG to overwrite/adj HIST (1=on, 0=off)
      INTEGER      LFadjvolON        ! FLAG to activate STEO volume adjustments in the LFMM (1=on, 0=off)
      INTEGER      NumPhaseoutYrs    ! Number of years to phase out volume adjustments
      REAL         PhaseoutFactor    ! Percent allowed change from last STEO year per year/100

      INTEGER      STEOLYR
      INTEGER      HISTLYR
      INTEGER      HISTLYR_2
      COMMON /LFMM_LAST_HIST/ HISTLYR, HISTCALFYR, HISTCALLYR, STEOLYR, LFadjsteoON, LFadjhistON, &
                              LFadjvolON, NumPhaseoutYrs, PhaseoutFactor

! for file manager call:
      LOGICAL        NEW                         !  FILE MANAGER
      CHARACTER*18   FNAME(4)
      INTEGER*4      UNITrfadj                   !  write to file rfadjbfo.txt
      INTEGER*4      IUNIT1
      INTEGER*4      FILE_MGR
      EXTERNAL       FILE_MGR

      DATA FNAME /'MU1PRDS','MU2PRDS','RFHIST','REFOGMAP'/

      L_OVWBSYR = 'ON '
      DOMREFREG = 8
      PMMRGNS = 5
      DMDRGNS = 9
!end patch

! HARDCODED  4-13-15 em4, made consistent with ngpl_2_refreg in gams, and OGSM mapping
! HARDCODED: need to change if:
!            - ngpl_2_refreg in lfnonpetroleum.xlsx changes
!            - OGSM changes its NGPL reg to LFMM region mapping
!            - GLOBAL variable for NGPL and LFMM region mapping is established

      NEW=.FALSE.
      IF (DO_ONCE) THEN
         STILL_READ=.TRUE.
         OGDIST2REFREG = 0
         IUNIT1=FILE_MGR('O',FNAME(4),NEW)         !  open file
         READ(IUNIT1,'(//)')                       !  skip header lines
         DO WHILE (STILL_READ)
            READ(IUNIT1,'(I,I,A35)') I, K, Label
            IF (I .NE. 9999) THEN
               ! RefReg-1 since 9 is Maritime Canada/Caribbean
               IF (K .LE. 0 .OR. K .GT. REFREG-1) THEN
                   WRITE (6,'(" OGDIST mapping gone awry.  Encountered RefReg outside jurisdiction",I4)') K
                   CYCLE
               ELSE IF (I .LE. 0 .OR. I .GT. OGDIST) THEN
                   WRITE (6,'(" OGDIST mapping gone awry.  Encountered OGSM district outside jurisdiction",I4)') I
                   CYCLE
               ELSE
                   OGDIST2REFREG(K,I) = 1
!                  WRITE(6,'(" Reading.    Refinery region=",I4,"  OGSM district=",I4,"  In array=",I4)') K,I,OGDIST2REFREG(K,I)
               ENDIF
            ELSE
               STILL_READ=.FALSE.
            ENDIF
         ENDDO
         DO_ONCE=.FALSE.
      ENDIF


      IF (CURCALYR .LT. 2010) THEN
      DO J = 1,2                                  !  read markup files
        IUNIT1=FILE_MGR('O',FNAME(J),NEW)         !  open file
        READ(IUNIT1,90) DUMMY
        DO I = 1,MNUMYR                           ! Reads loop over Census, files have all years
          READ(IUNIT1,94) (MGMUTR(K,I,J), K=1,9)
          IF (J.EQ.2.) THEN                       !  Add 1% locality tax based on current nominal value
            DO K = 1, DMDRGNS
              MGMUTR(K,I,J)=MGMUTR(K,I,J)+ (PMGTR(K,I)*.01)
            ENDDO
          ENDIF
          READ(IUNIT1,94) (DSMURS(K,I,J), K=1,9)   ! DISTILLATE
          READ(IUNIT1,94) (DSMUTR(K,I,J), K=1,9)
          READ(IUNIT1,94) (JFMUTR(K,I,J), K=1,9)
          MGMUTR(MNUMCR,I,J)=0.0
          DSMUTR(MNUMCR,I,J)=0.0
          DSMURS(MNUMCR,I,J)=0.0
          JFMUTR(MNUMCR,I,J)=0.0
          DO K=1,DMDRGNS
            MGMUTR(MNUMCR,I,J)=MGMUTR(MNUMCR,I,J)+(MGMUTR(K,I,J)*QMGAS(K,I)/QMGAS(MNUMCR,I))
            DSMUTR(MNUMCR,I,J)=DSMUTR(MNUMCR,I,J)+(DSMUTR(K,I,J)*QDSTR(K,I)/QDSTR(MNUMCR,I))
            DSMURS(MNUMCR,I,J)=DSMURS(MNUMCR,I,J)+(DSMURS(K,I,J)*QDSRS(K,I)/QDSRS(MNUMCR,I))
            JFMUTR(MNUMCR,I,J)=JFMUTR(MNUMCR,I,J)+(JFMUTR(K,I,J)*QJFTR(K,I)/QJFTR(MNUMCR,I))
          ENDDO
        ENDDO
        IUNIT1=FILE_MGR('C',FNAME(J),NEW)            !  close file
      ENDDO
      ENDIF

      HISTDBUG=1                                 ! Set to 1 to print out history reads.

      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME(3),NEW)

! if LFadjsteoON=1, overwrite and phase out steo
      CALL PMM_NEXTDATA(IUNIT1)  !call 1
      READ(IUNIT1,*)  LFadjsteoON, HISTLYR, STEOLYR

      CALL PMM_NEXTDATA(IUNIT1)  !call 1
      READ(IUNIT1,99)HISTLYR_2

!     LFadjvolON=1, apply STEO volume constraints in LFMM
!     NumPhaseoutYrs = number of years to phase out volume adjustments
!     PhaseoutFactor = Percent allowed change from last STEO year per year/100
      CALL PMM_NEXTDATA(IUNIT1)  !call 1
      READ(IUNIT1,*)  LFadjvolON, NumPhaseoutYrs, PhaseoutFactor


! if LFadjhistON=1, overwrite and calibrate to hist
      CALL PMM_NEXTDATA(IUNIT1)  !call 1
      READ(IUNIT1,*)  LFadjhistON, HISTCALFYR, HISTCALLYR    ! first and last hist year used for calibration

      RLHISTYR=MIN(HISTLYR,2008-1989)
      WRITE(6,*)'HISTLYR ',HISTLYR,RLHISTYR,HISTCALFYR,HISTCALLYR,LFadjhistON,LFadjsteoON,LFadjvolON


      IF ((CURIYR .EQ. 1) .AND. (CURITR.EQ.1)) THEN
        CALL PMM_NEXTDATA(IUNIT1)  !call 1
        read(IUNIT1,985) (PASINadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSCMadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSELadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSINadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSRSadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSTRadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSTRHWYadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PJFTRadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PMGCMadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PMGINadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PMGTRadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PPFINadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRHELadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRLTRadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRHTRadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRLCMadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRLELadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRLINadjHist(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PETTRadjHist(I), I=1,MNUMCR-2)

        CALL PMM_NEXTDATA(IUNIT1)  !call 1
        read(IUNIT1,985) (PASINadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSCMadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSELadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSINadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSRSadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSTRadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PDSTRHWYadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PJFTRadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PMGCMadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PMGINadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PMGTRadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PPFINadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRHELadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRLTRadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRHTRadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRLCMadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRLELadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PRLINadjSTEO(I), I=1,MNUMCR-2)
        read(IUNIT1,985) (PETTRadjSTEO(I), I=1,MNUMCR-2)

        read(IUNIT1,985)  PETHMadjSTEO(MNUMCR)

!!!     write(6,*) 'PMGTRadjSTEO3: ', curiyr+1989, PMGTRadjSTEO(2)

      ELSE
        CALL PMM_NEXTDATA(IUNIT1)  !call 1
        CALL PMM_NEXTDATA(IUNIT1)  !call 1

      ENDIF      ! end conditional to  define historical calibration
 985  FORMAT(23X,9F8.3)
  
!!!   write(6,*) 'PMGTRadjSTEO4: ', curiyr+1989, PMGTRadjSTEO(2)

      IF (LFadjhistON .EQ. 0) THEN
          PASINadjHIST(:) = 0.0
          PDSCMadjHIST(:) = 0.0
          PDSELadjHIST(:) = 0.0
          PDSINadjHIST(:) = 0.0
          PDSRSadjHIST(:) = 0.0
          PDSTRadjHIST(:) = 0.0
          PDSTRHWYadjHIST(:) = 0.0
          PJFTRadjHIST(:) = 0.0

          PMGCMadjHIST(:) = 0.0
          PMGINadjHIST(:) = 0.0
          PMGTRadjHIST(:) = 0.0
          PPFINadjHIST(:) = 0.0

          PRHELadjHIST(:) = 0.0
          PRLTRadjHIST(:) = 0.0
          PRHTRadjHIST(:) = 0.0
          PRLCMadjHIST(:) = 0.0
          PRLELadjHIST(:) = 0.0
          PRLINadjHIST(:) = 0.0
          PETTRadjHIST(:) = 0.0
      ENDIF

      IF (LFadjsteoON .EQ. 0) THEN
          PASINadjSTEO(:) = 0.0
          PDSCMadjSTEO(:) = 0.0
          PDSELadjSTEO(:) = 0.0
          PDSINadjSTEO(:) = 0.0
          PDSRSadjSTEO(:) = 0.0
          PDSTRadjSTEO(:) = 0.0
          PDSTRHWYadjSTEO(:) = 0.0
          PJFTRadjSTEO(:) = 0.0

          PETHMadjSTEO(:) = 0.0

          PMGCMadjSTEO(:) = 0.0
          PMGINadjSTEO(:) = 0.0
          PMGTRadjSTEO(:) = 0.0
          PPFINadjSTEO(:) = 0.0

          PRHELadjSTEO(:) = 0.0
          PRLTRadjSTEO(:) = 0.0
          PRHTRadjSTEO(:) = 0.0
          PRLCMadjSTEO(:) = 0.0
          PRLELadjSTEO(:) = 0.0
          PRLINadjSTEO(:) = 0.0
          PETTRadjSTEO(:) = 0.0
      ENDIF

!!!   write(6,*) 'PMGTRadjSTEO2: ', curiyr+1989, PMGTRadjSTEO(2)

       
      CALL PMM_NEXTDATA(IUNIT1)  !call 1
      DO J=1,HISTLYR
        READ(IUNIT1,631) RFQEXCRD(MNUMPR,J) ! CRUDE EXPORTED
        DO I=1,PMMRGNS
          RFQEXCRD(I,J) = 0.0
        ENDDO
!        WRITE(6,*) 'RFQEXCRD ',J,RFQEXCRD(MNUMPR,J)
!mc6       RFQEXCRD(MNUMPR,J)=0.0
!mc6       READ(IUNIT1,25) (RFQEXCRD(I,J),I=1,PMMRGNS) ! CRUDE EXPORTED, by 5-PADD
!mc6       DO I=1,PMMRGNS
!mc6        RFQEXCRD(MNUMPR,J)=RFQEXCRD(MNUMPR,J)+RFQEXCRD(I,J)
!mc6       ENDDO
      ENDDO

      DO J=1,HISTLYR                             ! mvd fr above to be consistent w/ usage below
       DO I=1,MNUMPR                             ! ZERO DATA NOT IN
        RFETHMCT(I,J)=0.0                        ! MERCHANT ETHANOL CONSUMP
       ENDDO
       DO I=1,MNUMCR                             ! ZERO DATA NOT IN
        IF(J.LT.25)  CLLETHCD(I,J)=0.0           ! Unproduce historical cellulosic ethanol production
        CELLIMPFRAC(I,J)=0.0                     ! Unimport cellulosic ethanol
       ENDDO
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)  !call 2
      DO J=1,HISTLYR
       READ(IUNIT1,631) RFQICRD(MNUMPR,J) !  IMPORTED TOTAL CRUDE
       DO I=1,PMMRGNS
         RFQICRD(I,J) = 0.0
       ENDDO

!       WRITE(6,*) 'RFQICRD ',J,RFQICRD(MNUMPR,J)
!mc6       READ(IUNIT1,31) (RFQICRD(I,J),I=1,PMMRGNS) !  IMPORTED TOTAL CRUDE
!mc6       DO I=1,PMMRGNS
!mc6        RFQICRD(MNUMPR,J)= RFQICRD(MNUMPR,J) + RFQICRD(I,J)
!mc6       ENDDO
      ENDDO
!     IF (CURIYR.LT.PMMBSYR) THEN
!       DO J=HISTLYR+1,HISTLYR+2
!        RFQICRD(MNUMPR,J)= 0.0
!        READ(IUNIT1,31) (RFQICRD(I,J),I=1,PMMRGNS) !  IMPORTED TOTAL CRUDE
!        DO I=1,PMMRGNS
!         RFQICRD(MNUMPR,J)= RFQICRD(MNUMPR,J) + RFQICRD(I,J)
!        ENDDO
!       ENDDO
!     ENDIF
      DO I=1,MNUMPR
       DO J = 1,HISTLYR
        RFIMCR(I,J)=RFQICRD(I,J)-RFQEXCRD(I,J)/1000.0
       ENDDO
      ENDDO

!==   CALL PMM_NEXTDATA(IUNIT1)                  !call 3
!==   DO J=1,HISTLYR                             !  PRC/QUAN OF NGL BY
!==    RFPQNGL(MNUMPR,J,6,2)= 0.0
!==    READ(IUNIT1,31) (RFPQNGL(I,J,6,2), I=1,PMMRGNS)
!==    DO I=1,PMMRGNS
!==     RFPQNGL(MNUMPR,J,6,2)= RFPQNGL(MNUMPR,J,6,2) + RFPQNGL(I,J,6,2)
!==    ENDDO
!==   ENDDO

!== don't overwrite STEO, per JConti
      CALL PMM_NEXTDATA(IUNIT1)                  !call 4
      DO J=1,HISTLYR
       READ(IUNIT1,631) RFBDSTCAP(MNUMPR,J) !  BASE DST CAPACITY

       DO I=1,PMMRGNS
         RFBDSTCAP(I,J) = 0.0
       ENDDO

!       WRITE(6,*) 'RFBDSTCAP ',J,RFBDSTCAP(MNUMPR,J)
!mc6       READ(IUNIT1,27) (RFBDSTCAP(I,J),I=1,PMMRGNS+1) !  BASE DST CAPACITY
!mc6       IF (MNUMPR .NE. (PMMRGNS+1)) THEN
!mc6          RFBDSTCAP(MNUMPR,J)=RFBDSTCAP(PMMRGNS+1,J)
!mc6          RFBDSTCAP(PMMRGNS+1,J) = 0.0
!mc6       ENDIF
      ENDDO
                                                 ! set total Cap = Base Cap for hist yrs
                                                 ! set cap adds  = 0 for hist yrs
      DO J=1,HISTLYR
       DO I=1,MNUMPR
                                                 ! set ACU(1) cap net of CSU(53) cap
        REF_CAP(1,I,J) = RFBDSTCAP(I,J) * 1000. - REF_CAP(53,I,J)
        RFDSTCAP(I,J)=RFBDSTCAP(I,J)
       ENDDO
      ENDDO

! HARDCODED for ref2015 only
! add splitter capacity to RFBDSTCAP since STEO did not in 2015, 2016 (RR4 or Padd3)
! do NOT add to REF_CAP since only ACU
!==  IF (HISTLYR.GT.25) THEN
!==   RFDSTCAP(3,26) = RFDSTCAP(3,26) + 0.200
!==   RFDSTCAP(3,27) = RFDSTCAP(3,27) + 0.540
!==   RFDSTCAP(MNUMPR,26) = RFDSTCAP(MNUMPR,26) + 0.200
!==   RFDSTCAP(MNUMPR,27) = RFDSTCAP(MNUMPR,27) + 0.540
!==  ENDIF

!== don't overwrite STEO, per JConti
      CALL PMM_NEXTDATA(IUNIT1)                  !call 6
!     WRITE(6,*) 'RFDSTUTL...'
      DO J=1,HISTLYR
       READ(IUNIT1,631) RFDSTUTL(MNUMPR,J) ! CAPACITY UTILIZATION R
       DO I=1,PMMRGNS
         RFDSTUTL(I,J) = 0.0
       ENDDO

!       WRITE(6,*) 'RFDSTUTL ',J,RFDSTUTL(MNUMPR,J)
!mc6       READ(IUNIT1,32) (RFDSTUTL(I,J),I=1,PMMRGNS+1) ! CAPACITY UTILIZATION R
!mc6       RFDSTUTL(MNUMPR,J)=RFDSTUTL(PMMRGNS+1,J)
!mc6       IF (MNUMPR .NE. (PMMRGNS+1)) THEN
!mc6          RFDSTUTL(MNUMPR,J) = RFDSTUTL(PMMRGNS+1,J)
!mc6          RFDSTUTL(PMMRGNS+1,J) = 0.0
!mc6       ENDIF
!mc6       REF_UTL(1,:,J) = RFDSTUTL(:,J) / 100.
       REF_UTL(1,MNUMPR,J) = RFDSTUTL(MNUMPR,J) / 100.
!       WRITE(6,*) 'REF_UTL ',J,REF_UTL(1,MNUMPR,J)
       ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 7
!      WRITE(6,*) 'RFQEXPRDT...'
      DO J=1,HISTLYR
       READ(IUNIT1,631) RFQEXPRDT(MNUMPR,J) ! TOTAL PRODUCT EXPORTE
!       WRITE(6,*) '  ',RFQEXPRDT(MNUMPR,J)
       DO I=1,PMMRGNS
         RFQEXPRDT(I,J) = 0.0
       ENDDO
!mc6       RFQEXPRDT(MNUMPR,J)= 0.0
!mc6       READ(IUNIT1,31) (RFQEXPRDT(I,J),I=1,PMMRGNS) ! TOTAL PRODUCT EXPORTE
!mc6       DO I=1,PMMRGNS
!mc6        RFQEXPRDT(MNUMPR,J)= RFQEXPRDT(MNUMPR,J)+RFQEXPRDT(I,J)
!mc6       ENDDO
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 8
!      WRITE(6,*) 'RFPQIPRDT...'
      DO J=1,HISTLYR
       READ(IUNIT1,631) RFPQIPRDT(MNUMPR,J,2) ! TOTAL IMPORTED PRODUCT
!       WRITE(6,*) '  ',RFPQIPRDT(MNUMPR,J,2)
       DO I=1,PMMRGNS
         RFPQIPRDT(I,J,2) = 0.0
       ENDDO
!mc6       RFPQIPRDT(MNUMPR,J,2)= 0.0
!mc6       READ(IUNIT1,31) (RFPQIPRDT(I,J,2),I=1,PMMRGNS) ! TOTAL IMPORTED PRODUCT
!mc6       DO I=1,PMMRGNS
!mc6        RFPQIPRDT(MNUMPR,J,2)=RFPQIPRDT(MNUMPR,J,2)+RFPQIPRDT(I,J,2)
!mc6       ENDDO
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 21
!      WRITE(6,*) 'CRDUNACC...'
      DO J= 1,HISTLYR
       READ(IUNIT1,44) CRDUNACC(MNUMPR,J)
!       WRITE(6,*) '  ',CRDUNACC(MNUMPR,J)
      ENDDO
      DO J= HISTLYR+1,LASTYR
        CRDUNACC(MNUMPR,J) = 0.0                 !== hardcoded, 0 over projection period
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 22
!      WRITE(6,*) 'CRDSTWDR...'
      DO J= 1,HISTLYR
       READ(IUNIT1,44) CRDSTWDR(MNUMPR,J)
!       WRITE(6,*) '  ',CRDSTWDR(MNUMPR,J)
      ENDDO
      DO J= HISTLYR+1,LASTYR
        CRDSTWDR(MNUMPR,J) = 0.0                 !== hardcoded, 0 over projection period
      ENDDO

!?? add cond to not do for TESTING CALL IN PMMBSYR ???
      CALL PMM_NEXTDATA(IUNIT1)                  !call 24
!      WRITE(6,*) 'RFQPRCG...'
      DO J = 1,HISTLYR
       READ(IUNIT1,44) RFQPRCG(MNUMPR,J)         !  QUANT. OF PROCESSING GAIN
!       WRITE(6,*) '  ',RFQPRCG(MNUMPR,J)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 25
!      WRITE(6,*) 'BLDIMP...'
      DO J= 1,HISTLYR
       READ(IUNIT1,44) BLDIMP(MNUMPR,J)
!       WRITE(6,*) '  ',BLDIMP(MNUMPR,J)
       IF ((RFIPQRBOB(MNUMPR,J,2)+RFIPQCBOB(MNUMPR,J,2)) .NE. 0.0) &
       RFIPQRBOB(MNUMPR,J,2) =  BLDIMP(MNUMPR,J) * 1000. * RFIPQRBOB(MNUMPR,J,2)  / &
           (RFIPQRBOB(MNUMPR,J,2)+RFIPQCBOB(MNUMPR,J,2))
       RFIPQCBOB(MNUMPR,J,2) = BLDIMP(MNUMPR,J) * 1000. - RFIPQRBOB(MNUMPR,J,2)
      ENDDO



      CALL PMM_NEXTDATA(IUNIT1)                  !call 28c
!      WRITE(6,*) 'RFHCXH2IN...'
      DO J = 1,HISTLYR
       READ(IUNIT1,44) RFHCXH2IN(MNUMPR,J)            ! Hydrogen etc
!       WRITE(6,*) '  ',RFHCXH2IN(MNUMPR,J)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  ! new 1/26/2016
!     DO J = 1,HISTLYR_2
      DO J = 1,HISTLYR                           ! new, t-2 data copied to t-1,t, 8-16-19
       READ(IUNIT1,44) RFQNGPF(MNUMCR,J)         ! NG feedstock for Hydrogen produced at ref, tril BTU/yr
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 28e
!      WRITE(6,*) 'OTHPRDSP...'
       DO J = 1,HISTLYR
        READ(IUNIT1,44) OTHPRDSP(MNUMPR,J)        ! All negative product supplied
!        WRITE(6,*) '  ',OTHPRDSP(MNUMPR,J)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 29
!      WRITE(6,*) 'PRDSTKWDR...'
      DO J = 1,HISTLYR
       READ(IUNIT1,44) PRDSTKWDR(MNUMPR,J)            ! PROD STK CHG.
!       WRITE(6,*) '  ',PRDSTKWDR(MNUMPR,J)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 30
!      WRITE(6,*) 'OTHETHCD...'
      DO J = 1,HISTLYR
       READ(IUNIT1,44) OTHETHCD(MNUMCR,J)        ! Advancd ethanol production
!       WRITE(6,*) '  ',OTHETHCD(MNUMCR,J)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 31
!      WRITE(6,*) 'RFETHE85...'
      DO J = 1,HISTLYR
       READ(IUNIT1,44) RFETHE85(MNUMPR,J)
!       WRITE(6,*) '  ',RFETHE85(MNUMPR,J)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 34
!      WRITE(6,*) 'RFMTBI...'
      DO J = 1,HISTLYR
       READ(IUNIT1,44) RFMTBI(MNUMPR,J)
!       WRITE(6,*) '  ',RFMTBI(MNUMPR,J)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 35
!      WRITE(6,*) 'RFPQUFC...'
      DO J = 1,HISTLYR
       READ(IUNIT1,44) RFPQUFC(MNUMPR,J,2)            !Unf Oil imported
!       WRITE(6,*) '  ',RFPQUFC(MNUMPR,J,2)

                              ! make padds = total US
             TOT = 0.0
             DO K=1,DOMREFREG
               TOT = TOT + RFPQUFC(K,J,2)
             ENDDO
             DO K=1,DOMREFREG
               if (tot .ne. 0.0 .and. .not. isnan(tot)) then
                 RFPQUFC(K,J,2) = RFPQUFC(MNUMPR,J,2) * RFPQUFC(K,J,2) / TOT
               else
                 RFPQUFC(K,J,2) = 0.0
               endif
             ENDDO
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 36
!      WRITE(6,*) 'RFSPRFR...'
      DO J=1,HISTLYR
       READ(IUNIT1,44) RFSPRFR(J)                ! OTHER CRUDE INPUTS
!       WRITE(6,*) '  ',RFSPRFR(J)
      ENDDO
!     DO J= HISTLYR+1,LASTYR
!       RFSPRFR(J) = 0.0                         !== hardcoded, 0 over projection period
!     ENDDO

      DO J=1,HISTLYR
                                                 !== hardcoded, no change over projection period
       CRDOTHTOT(MNUMPR,J)=CRDUNACC(MNUMPR,J)+CRDSTWDR(MNUMPR,J)-RFSPRFR(J)
      ENDDO
      DO J=1,HISTLYR
                                                 !== hardcoded, no change over projection period
       RFCRDOTH(MNUMPR,J)=RFSPRFR(J)+CRDOTHTOT(MNUMPR,J)
      ENDDO

      DO I=1,MNUMPR
        DO J=1,HISTLYR
         RFIMTP(I,J)=RFPQIPRDT(I,J,2)-RFQEXPRDT(I,J)+RFPQUFC(I,J,2)+BLDIMP(I,J)
        ENDDO
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 37a
!      WRITE(6,*) 'TDIESEL...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) TDIESEL(MNUMCR,J)          !  PRODUCT SUPPLIED
!       TDIESEL(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 37
!      WRITE(6,*) 'RFQARO...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQARO(MNUMCR,J)          !  PRODUCT SUPPLIED
       RFQARO(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 38
!      WRITE(6,*) 'RFQPCK...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQPCK(MNUMCR,J)
       RFQPCK(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 39
!      WRITE(6,*) 'RFQJF...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQJF(MNUMCR,J)
       RFQJF(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 40
!      WRITE(6,*) 'RFQKS...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQKS(MNUMCR,J)
       RFQKS(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 41
!      WRITE(6,*) 'RFQLG...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQLG(MNUMCR,J)
       RFQLG(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 42
!      WRITE(6,*) 'RFQDS...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQDS(MNUMCR,J)
       RFQDS(1:(MNUMCR-1),J) = 0.0
      ENDDO

!== want to keep RFQKS separate from RFQDS, 4-8-15
!==   IF (J.LT.HISTLYR_2) THEN               !Add Kerosene to Distillate for History
!==     RFQDS(MNUMCR,J)=RFQDS(MNUMCR,J)+RFQKS(MNUMCR,J)
!==   ENDIF

      CALL PMM_NEXTDATA(IUNIT1)                  !call 43
!      WRITE(6,*) 'RFQRH...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQRH(MNUMCR,J)
       RFQRH(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 44
!      WRITE(6,*) 'RFQRL...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQRL(MNUMCR,J)
       RFQRL(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 45
!      WRITE(6,*) 'RFQOTH...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQOTH(MNUMCR,J)
       RFQOTH(MNUMCR,J)=RFQOTH(MNUMCR,J)+OTHPRDSP(MNUMPR,J)
       RFQOTH(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 46
!      WRITE(6,*) 'RFQPF...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQPF(MNUMCR,J)
       RFQPF(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 47
!      WRITE(6,*) 'RFQSTG...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQSTG(MNUMCR,J)
!      WRITE(6,*) '  ',RFQSTG(MNUMCR,J)
       RFQSTG(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 48
!      WRITE(6,*) 'RFQPRDT...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQPRDT(MNUMCR,J)         !  US PRODUCT SUPPLIED
       RFQSECT(J)= RFQPRDT(MNUMCR,J)/1000
       RFQPRDT(1:(MNUMCR-1),J) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                  !call 49
!      WRITE(6,*) 'RFQMG...'
      DO J= 1,HISTLYR
       READ(IUNIT1,46) RFQMG(MNUMCR,J)
       RFQMG(1:(MNUMCR-1),J) = 0.0
      ENDDO

      DO J=1,HISTLYR
         RFQARO(MNUMCR,J)=RFQARO(MNUMCR,J)/1000
         RFQPCK(MNUMCR,J)=RFQPCK(MNUMCR,J)/1000
         RFQJF(MNUMCR,J) =RFQJF(MNUMCR,J)/1000
         RFQKS(MNUMCR,J) =RFQKS(MNUMCR,J)/1000
         RFQLG(MNUMCR,J) =RFQLG(MNUMCR,J)/1000
         RFQDS(MNUMCR,J) =RFQDS(MNUMCR,J)/1000
         RFQRH(MNUMCR,J) =RFQRH(MNUMCR,J)/1000
         RFQRL(MNUMCR,J) =RFQRL(MNUMCR,J)/1000
         RFQOTH(MNUMCR,J)=RFQOTH(MNUMCR,J)/1000
         RFQPF(MNUMCR,J) =RFQPF(MNUMCR,J)/1000
         RFQSTG(MNUMCR,J)=RFQSTG(MNUMCR,J)/1000
         RFQPRDT(MNUMCR,J) = RFQPRDT(MNUMCR,J)/1000
         RFQMG(MNUMCR,J) =RFQMG(MNUMCR,J)/1000
      ENDDO


!em4  DO J = 1,HISTLYR                           ! Initialize Historical data added 11-25-03 WBR
      DO J = 1,HISTLYR                          !
        DO K =1,9
            QELETH(J,K) = 0                      ! tril BTU/yr
            QNGETH(J,K) = 0                      ! tril BTU/yr
            QCLETH(J,K) = 0                      ! tril BTU/yr
        ENDDO
      ENDDO

                                                 ! added 10-22-02 WBR
      CALL PMM_NEXTDATA(IUNIT1)
!      WRITE(6,*) 'QELETH...'
      DO J = 1,HISTLYR                          !
       READ(IUNIT1,45) QELETH(J,MNUMCR)              ! ELECT CONSUMED BY ETHANOL PLANT
       QELETH(J,1)=0.0
       QELETH(J,2)=0.0
       QELETH(J,3)=QELETH(J,MNUMCR) *.31             ! added 11-25-03 WBR
       QELETH(J,4)=QELETH(J,MNUMCR) *.69
       QELETH(J,5)=0.0
       QELETH(J,6)=0.0
       QELETH(J,7)=0.0
       QELETH(J,8)=0.0
       QELETH(J,9)=0.0
       QELETH(J,10)=0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)
!      WRITE(6,*) 'QNGETH...'
      DO J = 1,HISTLYR                          !
       READ(IUNIT1,45) QNGETH(J,MNUMCR)              ! NG CONSUMED BY ETHANOL PLANT
       QNGETH(J,1) = 0.0
       QNGETH(J,2) = 0.0
       QNGETH(J,3) = QNGETH(J,MNUMCR) * .30          ! added 11-25-03 WBR
       QNGETH(J,4) = QNGETH(J,MNUMCR) * .66
       QNGETH(J,5) = 0.0
       QNGETH(J,6) = QNGETH(J,MNUMCR) * .03
       QNGETH(J,7) = 0.0
       QNGETH(J,8) = QNGETH(J,MNUMCR) * .01
       QNGETH(J,9) = 0.0
       QNGETH(J,10) = 0.0
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)
!      WRITE(6,*) 'QCLETH...'
      DO J = 1,HISTLYR                          !
       READ(IUNIT1,45) QCLETH(J,MNUMCR)              ! COL2LQ CONSUMED BY ETHANOL PLANT
       QCLETH(J,1) = 0.0
       QCLETH(J,2) = 0.0
       QCLETH(J,3) = QCLETH(J,MNUMCR) * .28          ! added 11-25-03 WBR
       QCLETH(J,4) = QCLETH(J,MNUMCR) * .64
       QCLETH(J,5) = 0.0
       QCLETH(J,6) = QCLETH(J,MNUMCR) * .06
       QCLETH(J,7) = 0.0
       QCLETH(J,8) = QCLETH(J,MNUMCR) * .02
       QCLETH(J,9) = 0.0
       QCLETH(J,10) = 0.0
      ENDDO

      IF ( (CURIYR .EQ. 1) .and. (CURITR .EQ. 1) ) THEN
        modelPETHM(:,:) = 0
      ENDIF

      IF ( (CURIYR .GE. HISTCALFYR) .and. (CURIYR .LE. HISTLYR) ) THEN
        modelPETHM(:,CURIYR) = PETHM(:,CURIYR)
      ENDIF

      CALL PMM_NEXTDATA(IUNIT1)
!      WRITE(6,*) 'PETHM...'
      DO J = 1,HISTLYR
       READ(IUNIT1,45) PETHM(MNUMCR,J)               ! Read in Historical Ethanol Prices
      ENDDO                                      ! added 9-13-05 WBR

      CALL PMM_NEXTDATA(IUNIT1)
!===  DO J = 1,HISTLYR_2+1                      ! Let Crude Price Path set Brent in last STEO year (2015, AEO2015)
      DO J = 1,HISTLYR                          ! Let Crude Price Path set Brent in last STEO year (2015, AEO2015)
       READ(IUNIT1,45) BRENT_PRICE(J)           ! Read in Historical Brent Crude Prices, sh7 8-8-13
       BRENT_PRICE(J) = BRENT_PRICE(J)/MC_JPGDP(J)
       START_PRICE(J) = BRENT_PRICE(J)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)
!===  DO J = 1,HISTLYR_2+2                      ! Let Crude Price Path set WTI in last STEO year (2015, AEO2015)
      DO J = 1,HISTLYR                          ! Let Crude Price Path set WTI in last STEO year (2015, AEO2015)
       READ(IUNIT1,45) WTI_PRICE(J)             ! Read in Historical WTI Crude Prices,  sh7 8-8-13
       WTI_PRICE(J) = WTI_PRICE(J)/MC_JPGDP(J)
       RFIPQCLL(2,J,1)=WTI_PRICE(J)
       RFIPQCLL(MNUMPR,J,1)=WTI_PRICE(J)
      ENDDO                                      ! added 9-13-05 WBR

      CALL PMM_NEXTDATA(IUNIT1)
!      WRITE(6,*) 'ETHEXP...'
      DO J = 1,HISTLYR
       READ(IUNIT1,45) ETHEXP(MNUMCR,J)              ! Read in Historical Ethanol Exports
      ENDDO                                      ! added 10-02-07 SH7

      CALL PMM_NEXTDATA(IUNIT1)
        DO J = 1,HISTLYR
         READ(IUNIT1,45) PALMG(MNUMCR,J)               ! Read in Historical Gasoline Whls Price
         PALMG(MNUMCR,J) =PALMG(MNUMCR,J) *.42/MC_JPGDP(J)
        ENDDO                                      ! added 11-11-08 WBR

      CALL PMM_NEXTDATA(IUNIT1)
!      WRITE(6,*) 'PDSU...'
        DO J = 1,HISTLYR
         READ(IUNIT1,45)  PDSU(MNUMCR,J)               ! Read in Historical D Whls Price
         PDSU(MNUMCR,J)= PDSU(MNUMCR,J) *.42/MC_JPGDP(J)
        ENDDO                                      ! added 8-16-10 SH7

      CALL PMM_NEXTDATA(IUNIT1)
        DO J = 1,HISTLYR
         READ(IUNIT1,45) PDS(MNUMCR,J)                 ! Read in Historical #2 Whls Price
          PDS(MNUMCR,J) = (PDS(MNUMCR,J) *.42)/MC_JPGDP(J)
        ENDDO                                      ! added 11-11-08 WBR


      CALL PMM_NEXTDATA(IUNIT1)
!      WRITE(6,*) 'IT_WOP...'
        DO J = 1,HISTLYR
         READ(IUNIT1,45) IT_WOP(J,1)               ! Read in Historical Light Sweet Crude Prices
!        WRITE(6,*) '  ',IT_WOP(J,1)
         IT_WOP(J,1) = IT_WOP(J,1)/MC_JPGDP(J)
      ENDDO

      IF (L_OVWBSYR) THEN            ! added for BkCast
       BIMQTYCD(1,MNUMCR,19)=42.66           ! Mbbl/day projection per Mike Cole (mc6)
       BIMQTYCD(2,MNUMCR,19)=1.86            ! Mbbl/day projection per Mike Cole (mc6)
       BIMQTYCD(1,MNUMCR,20)=47.00           ! Mbbl/day estimate
       BIMQTYCD(2,MNUMCR,20)=1.90            ! Mbbl/day estimate
      ENDIF

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in STEO Biodiesel Production, sh7 9-09-09
        DO J = 1,HISTLYR
         READ(IUNIT1,45) rfBDPRPUS(J)
         rfBDPRPUS(J)=rfBDPRPUS(J)*1000
        ENDDO
        DO J =19,HISTLYR
          BIOPRD1(J)=BIMQTYCD(1,MNUMCR,J)/SUM(BIMQTYCD(1:4,MNUMCR,J))         ! Allocate total among different feedstock types
          BIOPRD2(J)=BIMQTYCD(2,MNUMCR,J)/SUM(BIMQTYCD(1:4,MNUMCR,J))
          BIOPRD3(J)=BIMQTYCD(3,MNUMCR,J)/SUM(BIMQTYCD(1:4,MNUMCR,J))
          BIOPRD4(J)=BIMQTYCD(4,MNUMCR,J)/SUM(BIMQTYCD(1:4,MNUMCR,J))
          BIMQTYCD(1,MNUMCR,J) = rfBDPRPUS(J) * BIOPRD1(J)
          BIMQTYCD(2,MNUMCR,J) = rfBDPRPUS(J) * BIOPRD2(J)
         !BIMQTYCD(3,MNUMCR,J) = rfBDPRPUS(J) * BIOPRD3(J)
         !BIMQTYCD(4,MNUMCR,J) = rfBDPRPUS(J) * BIOPRD4(J)
        ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in STEO Biodiesel Consumption, sh7 9-09-09
        DO J = 1,HISTLYR
        READ(IUNIT1,45) rfBDTCPUS(J)
         rfBDTCPUS(J)=rfBDTCPUS(J)*1000
        ENDDO
        DO J =19,HISTLYR                                              ! Allocate total among different feedstock types
          BIOCON1(J)= BIODCONCD(1,MNUMCR,J)/SUM(BIODCONCD(1:2,MNUMCR,J))
          BIOCON2(J)= BIODCONCD(2,MNUMCR,J)/SUM(BIODCONCD(1:2,MNUMCR,J))
          BIODCONCD(1,MNUMCR,J) = rfBDTCPUS(J) * BIOCON1(J)
          BIODCONCD(2,MNUMCR,J) = rfBDTCPUS(J) * BIOCON2(J)
        ENDDO


      CALL PMM_NEXTDATA(IUNIT1)                       !  Read in STEO Biodiesel Exports, sh7 9-02-10
        DO J = 1,HISTLYR
         READ(IUNIT1,45) BIODEXP(MNUMCR,J)
!         BIODEXP(MNUMCR,J) = (BIODEXP(MNUMCR,J)/365)*1000     ! Convert from Thousand Barrels to Barrels/Day
         BIODEXP(MNUMCR,J) = (BIODEXP(MNUMCR,J)/365)
      ENDDO


      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in STEO Biodiesel Imports, sh7 7-15-20
        DO J = 1,HISTLYR
         READ(IUNIT1,45) BIODIMP(MNUMCR,J)
!         BIODIMP(MNUMCR,J) = (BIODIMP(MNUMCR,J)/365)*1000     ! Convert from Thousand Barrels to Barrels/Day
         BIODIMP(MNUMCR,J) = (BIODIMP(MNUMCR,J)/365)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in STEO Renewable Diesel Imports, sh7 7-15-20
        DO J = 1,HISTLYR
         READ(IUNIT1,45) RENEWDIMP(MNUMCR,J)
         RENEWDIMP(MNUMCR,J) = (RENEWDIMP(MNUMCR,J)/365)
      ENDDO


      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in STEO Ethanol Production, sh7 9-09-09
      DO J = 1,HISTLYR                                ! Allocate total among different feedstock types
         READ(IUNIT1,45) rfEOPRPUS(J)
         rfEOPRPUS(J) = rfEOPRPUS(J) * 1000
         ETHPRD1(J) = CRNETHCD(MNUMCR,J) / (CRNETHCD(MNUMCR,J) + CLLETHCD(MNUMCR,J) + GRNETHCD(MNUMCR,J) + OTHETHCD(MNUMCR,J))
         ETHPRD2(J) = CLLETHCD(MNUMCR,J) / (CRNETHCD(MNUMCR,J) + CLLETHCD(MNUMCR,J) + GRNETHCD(MNUMCR,J) + OTHETHCD(MNUMCR,J))
         ETHPRD3(J) = GRNETHCD(MNUMCR,J) / (CRNETHCD(MNUMCR,J) + CLLETHCD(MNUMCR,J) + GRNETHCD(MNUMCR,J) + OTHETHCD(MNUMCR,J))
         ETHPRD4(J) = OTHETHCD(MNUMCR,J) / (CRNETHCD(MNUMCR,J) + CLLETHCD(MNUMCR,J) + GRNETHCD(MNUMCR,J) + OTHETHCD(MNUMCR,J))
         CRNETHCD(MNUMCR,J) = ETHPRD1(J) * rfEOPRPUS(J)
         CLLETHCD(MNUMCR,J) = ETHPRD2(J) * rfEOPRPUS(J)
         GRNETHCD(MNUMCR,J) = ETHPRD3(J) * rfEOPRPUS(J)
         OTHETHCD(MNUMCR,J) = ETHPRD4(J) * rfEOPRPUS(J)
      ENDDO


      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in STEO Ethanol Consumption, sh7 9-09-09
      DO J = 1,HISTLYR
         READ(IUNIT1,45) rfEOTCPUS(J)
!         WRITE(6,*) '  ',rfEOTCPUS(J)
         ETHTOTCD(MNUMCR,J) = rfEOTCPUS(J)*1000.
      ENDDO

                                                      ! Read in STEO Ethanol Net Imports, sh7 9-09-09
                                                      ! ETHIMPST is now    GROSS Imports, em4 8-13-19
      CALL PMM_NEXTDATA(IUNIT1)
      DO J=1,HISTLYR
        READ(IUNIT1,45) ETHIMPST(J)
      ENDDO
      DO J = 19,HISTLYR
         ETHIMP(MNUMCR,J) = ETHIMPST(J) 
!        ETHIMP(MNUMCR,J) = ETHIMPST(J) + ETHEXP(MNUMCR,J)
      ENDDO

!==   4-10-15 em4, set historical values
!==          note: position 2=AK, 58-63=offshore, all others=onshore
!!!   DO J = 1,HISTLYR_2
      DO J = 1,HISTLYR
         RFQNGPL(9,J,:)   = 0.0         ! RefReg9 (Eastern Canada/Caribbean)
         RFPQNGL(9,J,:,2) = 0.0         ! RefReg9 (Eastern Canada/Caribbean)
         DO K=1,DOMREFREG
            RFQNGPL(K,J,1) = sum( OGNGPLET(1:OGDIST,J)*OGDIST2REFREG(K,1:OGDIST) ) * 1000.
            RFQNGPL(K,J,2) = sum( OGNGPLPR(1:OGDIST,J)*OGDIST2REFREG(K,1:OGDIST) ) * 1000.
            RFQNGPL(K,J,3) = sum( OGNGPLBU(1:OGDIST,J)*OGDIST2REFREG(K,1:OGDIST) ) * 1000.
            RFQNGPL(K,J,4) = sum( OGNGPLIS(1:OGDIST,J)*OGDIST2REFREG(K,1:OGDIST) ) * 1000.
            RFQNGPL(K,J,5) = sum( OGNGPLPP(1:OGDIST,J)*OGDIST2REFREG(K,1:OGDIST) ) * 1000.
         ENDDO

         RFQNGPL(10,J,1) = sum(OGNGPLET(1:OGDIST,J)) * 1000.
         RFQNGPL(10,J,2) = sum(OGNGPLPR(1:OGDIST,J)) * 1000.
         RFQNGPL(10,J,3) = sum(OGNGPLBU(1:OGDIST,J)) * 1000.
         RFQNGPL(10,J,4) = sum(OGNGPLIS(1:OGDIST,J)) * 1000.
         RFQNGPL(10,J,5) = sum(OGNGPLPP(1:OGDIST,J)) * 1000.
         RFQNGPL(10,J,6) = sum(RFQNGPL(10,J,1:5))

         RFPQNGL(:,J,:,2)= RFQNGPL(:,J,:) / 1000.
      ENDDO

!==   DO J=1,HISTLYR                             !  PRC/QUAN OF NGL BY
!==    RFPQNGL(MNUMPR,J,6,2)= 0.0
!==    READ(IUNIT1,31) (RFPQNGL(I,J,6,2), I=1,PMMRGNS)
!==    DO I=1,PMMRGNS
!==     RFPQNGL(MNUMPR,J,6,2)= RFPQNGL(MNUMPR,J,6,2) + RFPQNGL(I,J,6,2)
!==    ENDDO

!==   ENDDO
!==   CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Production, ethane, sh7 8-13-13
!==   DO J = 1,HISTLYR_2
!==      READ(IUNIT1,45) RFQNGPL(10,J,1)
!==   ENDDO


!==   CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Production, propane, sh7 8-13-13
!==   DO J = 1,HISTLYR_2
!==      READ(IUNIT1,45) RFQNGPL(10,J,2)
!==   ENDDO


!==   CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Production, butane, sh7 8-13-13
!==   DO J = 1,HISTLYR_2
!==      READ(IUNIT1,45) RFQNGPL(10,J,3)
!==   ENDDO


!==   CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Production, isobutane, sh7 8-13-13
!==   DO J = 1,HISTLYR_2
!==      READ(IUNIT1,45) RFQNGPL(10,J,4)
!==   ENDDO


!==   CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Production, pentanes plus/c5, sh7 8-13-13
!==   DO J = 1,HISTLYR_2
!==      READ(IUNIT1,45) RFQNGPL(10,J,5)
!==   ENDDO
!==   DO J = 1,HISTLYR_2
!==      RFQNGPL(10,J,6) = sum(RFQNGPL(10,J,1:5))
!==   ENDDO



      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Imports, ethane, sh7 10-10-13
!      WRITE(6,*) 'RFIPQET...'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) RFIPQET(MNUMPR,J,2)
!         WRITE(6,*) '  ',RFIPQET(MNUMPR,J,2)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Imports, propane, sh7 10-10-13
!      WRITE(6,*) 'RFIPQPR...'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) RFIPQPR(MNUMPR,J,2)
!         WRITE(6,*) '  ',RFIPQPR(MNUMPR,J,2)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Imports, nbutane, sh7 10-10-13
!      WRITE(6,*) 'RFIPQBU...'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) RFIPQBU(MNUMPR,J,2)
!         WRITE(6,*) '  ',RFIPQBU(MNUMPR,J,2)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Imports, isobutane, sh7 10-10-13
!      WRITE(6,*) 'RFIPQIS...'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) RFIPQIS(MNUMPR,J,2)
!         WRITE(6,*) '  ',RFIPQIS(MNUMPR,J,2)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Imports, Pentanes Plus, sh7 10-10-13
!      WRITE(6,*) 'RFIPQPP...'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) RFIPQPP(MNUMPR,J,2)
!         WRITE(6,*) '  ',RFIPQPP(MNUMPR,J,2)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Imports, Propylene, sh7 10-10-13
!      WRITE(6,*) 'RFIPQPY...propylene'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) RFIPQPY(MNUMPR,J,2)
!         WRITE(6,*) '  ',RFIPQPY(MNUMPR,J,2)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Exports, ethane, sh7 10-10-13
!      WRITE(6,*) 'QPRDEX(27,J)...ethane'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) QPRDEX(27,J)
!         WRITE(6,*) '  ',J,QPRDEX(27,J)
         QPRDEX(27,J) = QPRDEX(27,J)/1000.
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Exports, propane, sh7 10-10-13
!      WRITE(6,*) 'QPRDEX(1,J)...propane'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) QPRDEX(1,J)
!         WRITE(6,*) '  ',J,QPRDEX(1,J)
         QPRDEX(1,J) = QPRDEX(1,J)/1000.
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Exports, nbutane, sh7 10-10-13
!      WRITE(6,*) 'QPRDEX(15,J)...butane'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) QPRDEX(15,J)
!         WRITE(6,*) '  ',J,QPRDEX(15,J)
         QPRDEX(15,J) = QPRDEX(15,J)/1000.
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Exports, isobutane, sh7 10-10-13
!      WRITE(6,*) 'QPRDEX(28,J)...isobutane'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) QPRDEX(28,J)
!         WRITE(6,*) '  ',J,QPRDEX(28,J)
         QPRDEX(28,J) = QPRDEX(28,J)/1000.
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Exports, Pentanes Plus, sh7 10-10-13
!      WRITE(6,*) 'QPRDEX(29,J)...pentanes plus'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) QPRDEX(29,J)
!         WRITE(6,*) '  ',J,QPRDEX(29,J)
         QPRDEX(29,J) = QPRDEX(29,J)/1000.
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)                       ! Read in NGPL Historic Exports, Propylene, sh7 10-10-13
!      WRITE(6,*) 'QPRDEX(14,J)...propylene'
      DO J = 1,HISTLYR_2
         READ(IUNIT1,45) QPRDEX(14,J)
!         WRITE(6,*) '  ',J,QPRDEX(14,J)
         QPRDEX(14,J) = QPRDEX(14,J)/1000.
      ENDDO
!      WRITE(6,*) 'QPRDEX(14)...complete'


      CALL PMM_NEXTDATA(IUNIT1)                       ! New read in statements for Historic Crude and Light/Heavy Product imports, sh7 10-10-13
!      WRITE(6,*) 'Historic imports 1...'
      DO J = 2008-1989,2014-1989
!         WRITE(6,*) '  ', J
         READ(IUNIT1,'(4x,13F12.6)') &
            ICOCanada(J),ICOMexico(J),ICONorthSea(J),ICOOPEC(J),ICOOPAmericas(J),         &
            ICOOPNoAfrica(J),ICOOPWestAfrica(J), ICOOPPersianGulf(J),ICOOtherMidEast(J),  &
            ICOOtherAmericas(J),ICOOtherAfrica(J),ICOOtherAsia(J),ICOTotal(J)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)
!      WRITE(6,*) 'Historic imports 2...'
      DO J = 2008-1989,2014-1989
         READ(IUNIT1,'(4x,12F12.6)') &
            ILPCanada(J),ILPNorthEurope(J),ILPSouthEurope(J),ILPOPEC(J),                  &
            ILPOPAmericas(J),ILPOPNoAfrica(J),ILPOPWestAfrica(J),                         &
            ILPOPPersianGulf(J),ILPCaribbean(J),ILPAsia(J),ILPOther(J),ILPTotal(J)
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)
!      WRITE(6,*) 'Historic imports 3...'
      DO J = 2008-1989,2014-1989
         READ(IUNIT1,'(4x,12F12.6)') &
            IHPCanada(J),IHPNorthEurope(J),IHPSouthEurope(J),IHPOPEC(J),IHPOPAmericas(J), &
            IHPOPNoAfrica(J),IHPOPWestAfrica(J), IHPOPPersianGulf(J),                     &
            IHPCaribbean(J),IHPAsia(J),IHPOther(J),IHPTotal(J)
      ENDDO



                                      ! added 11-11-08 WBR

!!!   CALL PMM_NEXTDATA(IUNIT1)
!!!   DO J = 1,HISTLYR
!!!     READ(IUNIT1,45) RFQEL(J)                 ! PRODUCT DEMAND
!!!     READ(IUNIT1,45) RFQIN(J)
!!!     READ(IUNIT1,45) RFQRC(J)
!!!     READ(IUNIT1,45) RFQTR(J)
!!!   ENDDO
!!!   DO J=1,HISTLYR
!!!     RFQSECT(J)=RFQRC(J)+RFQIN(J)+RFQTR(J)+RFQEL(J)
!!!   ENDDO
                                                 ! 4-21-03, replaces read above
      DO J = 1,HISTLYR
       RFQEL(J)    = 0.0                         ! PRODUCT DEMAND
       RFQIN(J)    = 0.0
       RFQRC(J)    = 0.0
       RFQTR(J)    = 0.0
      ENDDO


      CALL PMM_NEXTDATA(IUNIT1)
!      WRITE(6,*) 'Refinery fuel use...'
! 3/8/16 em4 read through STEO years for side case overwrites
      DO J =1,STEOLYR
       READ(IUNIT1,35) (QCLRF(I,J),I = 1,DMDRGNS)  ! REFINERY FUEL USE
       READ(IUNIT1,35) (QDSRF(I,J),I = 1,DMDRGNS)
       READ(IUNIT1,35) (QELRF(I,J),I = 1,DMDRGNS)
       READ(IUNIT1,35) (QLGRF(I,J),I = 1,DMDRGNS)
       READ(IUNIT1,35) (QNGRF(I,J),I = 1,DMDRGNS)
       READ(IUNIT1,35) (QOTRF(I,J),I = 1,DMDRGNS)
       READ(IUNIT1,35) (QCCRF(I,J),I = 1,DMDRGNS)
       READ(IUNIT1,35) (QRSRF(I,J),I = 1,DMDRGNS)
       READ(IUNIT1,35) (QSGRF(I,J),I = 1,DMDRGNS)
        QCLRF(MNUMCR,J) = 0.0
        QDSRF(MNUMCR,J) = 0.0
        QELRF(MNUMCR,J) = 0.0
        QLGRF(MNUMCR,J) = 0.0
        QNGRF(MNUMCR,J) = 0.0
        QOTRF(MNUMCR,J) = 0.0
        QCCRF(MNUMCR,J) = 0.0
        QRSRF(MNUMCR,J) = 0.0
        QSGRF(MNUMCR,J) = 0.0
       DO I =1,DMDRGNS
        QCLRF(MNUMCR,J) = QCLRF(MNUMCR,J) + QCLRF(I,J)
        QDSRF(MNUMCR,J) = QDSRF(MNUMCR,J) + QDSRF(I,J)
        QELRF(MNUMCR,J) = QELRF(MNUMCR,J) + QELRF(I,J)
        QLGRF(MNUMCR,J) = QLGRF(MNUMCR,J) + QLGRF(I,J)
        QNGRF(MNUMCR,J) = QNGRF(MNUMCR,J) + QNGRF(I,J)
        QOTRF(MNUMCR,J) = QOTRF(MNUMCR,J) + QOTRF(I,J)
        QCCRF(MNUMCR,J) = QCCRF(MNUMCR,J) + QCCRF(I,J)
        QRSRF(MNUMCR,J) = QRSRF(MNUMCR,J) + QRSRF(I,J)
        QSGRF(MNUMCR,J) = QSGRF(MNUMCR,J) + QSGRF(I,J)
       ENDDO
       DO I=1,MNUMCR
         QPCRF(I,J) = 0.0
       ENDDO

!! add NG feedstock for H2 production because
!! QNGRF should include fuel use at ref, for cogen, and for feedstock
!! must estimate addition of NG feedstock by CD since not available
!! (tril BTU/yr)

       QNGRF(MNUMCR,J) = QNGRF(MNUMCR,J) + RFQNGPF(MNUMCR,J)
       DO I=1,DMDRGNS
         QNGRF(I,J) = QNGRF(I,J) * ( QNGRF(MNUMCR,J) / (QNGRF(MNUMCR,J) - RFQNGPF(MNUMCR,J)) )
       ENDDO
!! will use model results for RFQNGPF in years after HISTLYR_2
!      IF (J.LE.HISTLYR_2) THEN
!        DO I=1,DMDRGNS
!          QNGRF(I,J) = QNGRF(I,J) * ( QNGRF(MNUMCR,J) / (QNGRF(MNUMCR,J) - RFQNGPF(MNUMCR,J)) )
!        ENDDO
!      ELSE
!        DO I=1,DMDRGNS
!          QNGRF(I,J) = QNGRF(I,J) + RFQNGPF(I,J)
!        ENDDO
!      ENDIF

      ENDDO      ! end year loop

!   SHOULD WE STILL BE USING QRLRF INSTEAD OF QRSRF???
      DO I=1,DMDRGNS                             ! Census Division loop
       DO J=1,STEOLYR
        QRLRF(I,J) = QRSRF(I,J)                  ! 9-27-01 em4, switched to RL=RS
       ENDDO
      ENDDO

! 3/8/16 em4 save STEO years for side case overwrites
      DO J=1,STEOLYR
       DO I =1,MNUMCR
        SIDE_QCLRF(I,J) = QCLRF(I,J)
        SIDE_QDSRF(I,J) = QDSRF(I,J)
        SIDE_QELRF(I,J) = QELRF(I,J)
        SIDE_QLGRF(I,J) = QLGRF(I,J)
        SIDE_QNGRF(I,J) = QNGRF(I,J)
        SIDE_QOTRF(I,J) = QOTRF(I,J)
        SIDE_QCCRF(I,J) = QCCRF(I,J)
        SIDE_QRSRF(I,J) = QRSRF(I,J)
        SIDE_QSGRF(I,J) = QSGRF(I,J)
        SIDE_QRLRF(I,J) = QRLRF(I,J)
       ENDDO
      ENDDO

! IF BENCHMARK YEARS, SAVE MODEL RESULTS AND BEFORE READING IN HISTORICAL END-USE PRICE DATA
! initialize to 0
      IF ( (CURIYR .EQ. 1) .and. (CURITR .EQ. 1) ) THEN
        modelPASIN(:,:) = 0
        modelPDSCM(:,:) = 0
        modelPDSEL(:,:) = 0
        modelPDSIN(:,:) = 0
        modelPDSRS(:,:) = 0
        modelPDSTR(:,:) = 0
        modelPDSTRHWY(:,:) = 0
        modelPJFTR(:,:) = 0
        modelPKSCM(:,:) = 0
        modelPKSIN(:,:) = 0
        modelPKSRS(:,:) = 0

!      Survey of LPG data discountinued in 2011, so bogus data in rfhist
!      will use new (Aug 2012) Baysian algorithm to define historical/STEO years
        modelPLGCM(:,:) = 0
        modelPLGIN(:,:) = 0
        modelPLGRS(:,:) = 0
        modelPLGTR(:,:) = 0

        modelPMGCM(:,:) = 0
        modelPMGIN(:,:) = 0
        modelPMGTR(:,:) = 0
        modelPPFIN(:,:) = 0
        modelPRHEL(:,:) = 0
        modelPRLTR(:,:) = 0
        modelPRHTR(:,:) = 0
        modelPRLCM(:,:) = 0
        modelPRLEL(:,:) = 0
        modelPRLIN(:,:) = 0
        modelPETTR(:,:) = 0
      ENDIF
! fill in model results (including STEO years, HISTLYR)
      IF ( (CURIYR .GE. HISTCALFYR) .and. (CURIYR .LE. HISTLYR) ) THEN
        modelPASIN(:,CURIYR) = PASIN(:,CURIYR)
        modelPDSCM(:,CURIYR) = PDSCM(:,CURIYR)
        modelPDSEL(:,CURIYR) = PDSEL(:,CURIYR)
        modelPDSIN(:,CURIYR) = PDSIN(:,CURIYR)
        modelPDSRS(:,CURIYR) = PDSRS(:,CURIYR)
        modelPDSTR(:,CURIYR) = PDSTR(:,CURIYR)
        modelPDSTRHWY(:,CURIYR) = PDSTRHWY(:,CURIYR)
        modelPJFTR(:,CURIYR) = PJFTR(:,CURIYR)
        modelPKSCM(:,CURIYR) = PKSCM(:,CURIYR)
        modelPKSIN(:,CURIYR) = PKSIN(:,CURIYR)
        modelPKSRS(:,CURIYR) = PKSRS(:,CURIYR)


!      Survey of LPG data discountinued in 2011, so bogus data in rfhist
!      will use new (Aug 2012) Baysian algorithm to define historical/STEO years
        modelPLGCM(:,CURIYR) = PLGCM(:,CURIYR)
        modelPLGIN(:,CURIYR) = PLGIN(:,CURIYR)
        modelPLGRS(:,CURIYR) = PLGRS(:,CURIYR)
        modelPLGTR(:,CURIYR) = PLGTR(:,CURIYR)

        modelPMGCM(:,CURIYR) = PMGCM(:,CURIYR)
        modelPMGIN(:,CURIYR) = PMGIN(:,CURIYR)
        modelPMGTR(:,CURIYR) = PMGTR(:,CURIYR)
        modelPPFIN(:,CURIYR) = PPFIN(:,CURIYR)
        modelPRHEL(:,CURIYR) = PRHEL(:,CURIYR)
        modelPRLTR(:,CURIYR) = PRLTR(:,CURIYR)
        modelPRHTR(:,CURIYR) = PRHTR(:,CURIYR)
        modelPRLCM(:,CURIYR) = PRLCM(:,CURIYR)
        modelPRLEL(:,CURIYR) = PRLEL(:,CURIYR)
        modelPRLIN(:,CURIYR) = PRLIN(:,CURIYR)
        modelPETTR(:,CURIYR) = PETTR(:,CURIYR)
      ENDIF

! READ IN HISTORICAL END-USE PRICE DATA
      CALL PMM_NEXTDATA(IUNIT1)
      DO J=1,HISTLYR
       READ(IUNIT1,36) (PASIN(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PDSCM(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PDSEL(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PDSIN(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PDSRS(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PDSTR(I,J),I=1,DMDRGNS)
         PDSTRHWY(MNUMCR,J)=0.0
         DO I=1,DMDRGNS
           PDSTRHWY(I,J)=PDSTR(I,J)              ! Temp, 6-23-04
           PDSTRHWY(MNUMCR,J)=PDSTRHWY(MNUMCR,J) +  &
             QDSTR(I,J)*PDSTRHWY(I,J)/QDSTR(MNUMCR,J)
         ENDDO
       READ(IUNIT1,36) (PJFTR(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PKSCM(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PKSIN(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PKSRS(I,J),I=1,DMDRGNS)

!      Survey of LPG data discountinued in 2011, so bogus data in rfhist
!      will use new (Aug 2012) Baysian algorithm to define historical/STEO years
       IF (J+1989.LT.2011) THEN
         READ(IUNIT1,36) (PLGCM(I,J),I=1,DMDRGNS)
         READ(IUNIT1,36) (PLGIN(I,J),I=1,DMDRGNS)
         READ(IUNIT1,36) (PLGRS(I,J),I=1,DMDRGNS)
         READ(IUNIT1,36) (PLGTR(I,J),I=1,DMDRGNS)

         ! fill in propane prices
         DO I=1,DMDRGNS
           PPRRS(I,J) = PLGRS(I,J)
           PPRCM(I,J) = PLGCM(I,J)
           PPRIN(I,J) = PLGIN(I,J)
           PPRTR(I,J) = PLGTR(I,J)
         ENDDO

       ELSE
         READ(IUNIT1,36) (tempPLG(I),I=1,DMDRGNS)
         READ(IUNIT1,36) (tempPLG(I),I=1,DMDRGNS)
         READ(IUNIT1,36) (tempPLG(I),I=1,DMDRGNS)
         READ(IUNIT1,36) (tempPLG(I),I=1,DMDRGNS)
       ENDIF

       READ(IUNIT1,36) (PMGCM(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PMGIN(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (tempPMGTR(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PPFIN(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PRHEL(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PRLTR(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PRHTR(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PRLCM(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PRLEL(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (PRLIN(I,J),I=1,DMDRGNS)
       READ(IUNIT1,36) (tempPETTR(I,J),I=1,DMDRGNS)
       IF (J+1989.LT.2016) THEN
         PETTR(:,J) = tempPETTR(:,J)
         PMGTR(:,J) = tempPMGTR(:,J)
       ELSEIF (FCRL .EQ. 1) THEN       ! only overwrite when fcrl=1
         PETTR(:,J) = tempPETTR(:,J)
         PMGTR(:,J) = tempPMGTR(:,J)
       ENDIF
      ENDDO

! define historical benchmark factor
! do not execute if LFadjhistON = 0 (e.g., side and price cases) because factors READ IN
      IF (LFadjhistON .EQ. 1) THEN
       IF (CURIYR .EQ. HISTCALLYR) THEN
        L = HISTCALLYR - HISTCALFYR +1    ! Number of calibration years
        ! initialize for all CDs
        PASINadjHist(:) = 0
        PDSCMadjHist(:) = 0
        PDSELadjHist(:) = 0
        PDSINadjHist(:) = 0
        PDSRSadjHist(:) = 0
        PDSTRadjHist(:) = 0
        PDSTRHWYadjHist(:) = 0
        PJFTRadjHist(:) = 0
        PMGCMadjHist(:) = 0
        PMGINadjHist(:) = 0
        PMGTRadjHist(:) = 0
        PPFINadjHist(:) = 0
        PRHELadjHist(:) = 0
        PRLTRadjHist(:) = 0
        PRHTRadjHist(:) = 0
        PRLCMadjHist(:) = 0
        PRLELadjHist(:) = 0
        PRLINadjHist(:) = 0
        PETTRadjHist(:) = 0

        DO I=1,DMDRGNS
          DO J=HISTCALFYR,HISTCALLYR    ! loop over calibration years

            PASINadjHist(I) = PASINadjHist(I) + (PASIN(I,J) / modelPASIN(I,J)) / L
            PDSCMadjHist(I) = PDSCMadjHist(I) + (PDSCM(I,J) / modelPDSCM(I,J)) / L
            PDSELadjHist(I) = PDSELadjHist(I) + (PDSEL(I,J) / modelPDSEL(I,J)) / L
            PDSINadjHist(I) = PDSINadjHist(I) + (PDSIN(I,J) / modelPDSIN(I,J)) / L
            PDSRSadjHist(I) = PDSRSadjHist(I) + (PDSRS(I,J) / modelPDSRS(I,J)) / L
            PDSTRadjHist(I) = PDSTRadjHist(I) + (PDSTR(I,J) / modelPDSTR(I,J)) / L
            PDSTRHWYadjHist(I) = PDSTRHWYadjHist(I) + (PDSTRHWY(I,J) / modelPDSTRHWY(I,J)) / L
            PJFTRadjHist(I) = PJFTRadjHist(I) + (PJFTR(I,J) / modelPJFTR(I,J)) / L
!             PKSCMadjHist(I) = PJFTRadjHist(I)
!             PKSINadjHist(I) = PJFTRadjHist(I)
!             PKSRSadjHist(I) = PJFTRadjHist(I)

            PMGCMadjHist(I) = PMGCMadjHist(I) + (PMGCM(I,J) / modelPMGCM(I,J)) / L
            PMGINadjHist(I) = PMGINadjHist(I) + (PMGIN(I,J) / modelPMGIN(I,J)) / L
            PMGTRadjHist(I) = PMGTRadjHist(I) + (PMGTR(I,J) / modelPMGTR(I,J)) / L
!NO         PPFINadjHist(I) = PPFINadjHist(I) + (PPFIN(I,J) / modelPPFIN(I,J)) / L
!NO         PRHELadjHist(I) = PRHELadjHist(I) + (PRHEL(I,J) / modelPRHEL(I,J)) / L
!NO         PRHTRadjHist(I) = PRHTRadjHist(I) + (PRHTR(I,J) / modelPRHTR(I,J)) / L
            PPFINadjHist(I) = 1.0
            PRHELadjHist(I) = 1.0
            PRLTRadjHist(I) = 1.0
            PRHTRadjHist(I) = 1.0
            PRLCMadjHist(I) = PRLCMadjHist(I) + (PRLCM(I,J) / modelPRLCM(I,J)) / L
            PRLELadjHist(I) = PRLELadjHist(I) + (PRLEL(I,J) / modelPRLEL(I,J)) / L
            PRLINadjHist(I) = PRLINadjHist(I) + (PRLIN(I,J) / modelPRLIN(I,J)) / L
!NO         PETTRadjHist(I) = PETTRadjHist(I) + (PETTR(I,J) / modelPETTR(I,J)) / L
            PETTRadjHist(I) = 1.0

!      Survey of LPG data discountinued in 2011, so bogus data in rfhist
!      will use new (Aug 2012) Baysian algorithm to define historical/STEO years
!      do not adjust
!           PLGCM(:,CURIYR)
!           PLGIN(:,CURIYR)
!           PLGRS(:,CURIYR)
!           PLGTR(:,CURIYR)

          ENDDO      ! end loop over historical calibration years
        ENDDO    ! end loop over Census Divisions for historical calibration

!!!!!   DO J=HISTCALFYR,HISTCALLYR    ! loop over calibration years
!!!!!     DO I=1,MNUMCR-2
!!!!!       write(6,*) 'HISTCALFYR  PRLEL: ',J+1989,I,PRLEL(I,J),PRLELadjHist(I)
!!!!!       write(6,*) 'HISTCALFYR  PRLEL: ',J+1989,I,modelPRLEL(I,J)
!!!!!     ENDDO
!!!!!     DO I=1,MNUMCR-2
!!!!!       write(6,*) 'HISTCALFYR  PDSTR: ',J+1989,I,PDSTR(I,J),PDSTRadjHist(I)
!!!!!       write(6,*) 'HISTCALFYR  PDSTR: ',J+1989,I, modelPDSTR(I,J)
!!!!!     ENDDO
!!!!!     DO I=1,MNUMCR-2
!!!!!       write(6,*) 'HISTCALFYR  PJFTR: ',J+1989,I,PJFTR(I,J),PJFTRadjHist(I)
!!!!!       write(6,*) 'HISTCALFYR  PJFTR: ',J+1989,I,modelPJFTR(I,J)
!!!!!     ENDDO
!!!!!     DO I=1,MNUMCR-2
!!!!!       write(6,*) 'HISTCALFYR  PMGTR: ',J+1989,I,PMGTR(I,J),PMGTRadjHist(I)
!!!!!       write(6,*) 'HISTCALFYR  PMGTR: ',J+1989,I,modelPMGTR(I,J)
!!!!!     ENDDO
!!!!!     DO I=1,MNUMCR-2
!!!!!       write(6,*) 'HISTCALFYR  PETTR: ',J+1989,I,PETTR(I,J),PETTRadjHist(I)
!!!!!       write(6,*) 'HISTCALFYR  PETTR: ',J+1989,I,modelPETTR(I,J)
!!!!!     ENDDO
!!!!!   ENDDO      ! end loop over historical calibration years

       ENDIF      ! end conditional to  define historical calibration
      ENDIF       ! end conditional to see if SIDE case





! define STEO benchmark factor
! do not execute if LFadjsteoON = 0 (e.g., side and price cases) because factors READ IN
      IF ((LFadjsteoON .EQ. 1) .AND. (HISTLYR .EQ. STEOLYR)) THEN   ! reference case, so set adjSTEO
       IF (CURIYR .EQ. STEOLYR) THEN
        ! initialize for all CDs
        PASINadjSTEO(:) = 0
        PDSCMadjSTEO(:) = 0
        PDSELadjSTEO(:) = 0
        PDSINadjSTEO(:) = 0
        PDSRSadjSTEO(:) = 0
        PDSTRadjSTEO(:) = 0
        PDSTRHWYadjSTEO(:) = 0
        PJFTRadjSTEO(:) = 0

        PETHMadjSTEO(:) = 0

        PMGCMadjSTEO(:) = 0
        PMGINadjSTEO(:) = 0
        PMGTRadjSTEO(:) = 0
        PPFINadjSTEO(:) = 0
        PRHELadjSTEO(:) = 0
        PRLTRadjSTEO(:) = 0
        PRHTRadjSTEO(:) = 0
        PRLCMadjSTEO(:) = 0
        PRLELadjSTEO(:) = 0
        PRLINadjSTEO(:) = 0
        PETTRadjSTEO(:) = 0

!aeo16  L = 2                             ! Number of STEO years
        L = 1                             ! Number of STEO years
        DO I=1,DMDRGNS
!STEO        J=STEOLYR              ! loop over STEO years
          DO J=STEOLYR-L+1,STEOLYR  ! loop over STEO years

            PASINadjSTEO(I) = PASINadjSTEO(I) +           &
                             (PASIN(I,J) - (modelPASIN(I,J) + PASINadjHist(I)) ) / L
            PDSCMadjSTEO(I) = PDSCMadjSTEO(I) +           &
                             (PDSCM(I,J) - (modelPDSCM(I,J) + PDSCMadjHist(I)) ) / L
            PDSELadjSTEO(I) = PDSELadjSTEO(I) +           &
                             (PDSEL(I,J) - (modelPDSEL(I,J) + PDSELadjHist(I)) ) / L
            PDSINadjSTEO(I) = PDSINadjSTEO(I) +           &
                             (PDSIN(I,J) - (modelPDSIN(I,J) + PDSINadjHist(I)) ) / L
            PDSRSadjSTEO(I) = PDSRSadjSTEO(I) +           &
                             (PDSRS(I,J) - (modelPDSRS(I,J) + PDSRSadjHist(I)) ) / L
            PDSTRadjSTEO(I) = PDSTRadjSTEO(I) +           &
                             (PDSTR(I,J) - (modelPDSTR(I,J) + PDSTRadjHist(I)) ) / L
            PDSTRHWYadjSTEO(I) = PDSTRHWYadjSTEO(I) +     &
                             (PDSTRHWY(I,J) - (modelPDSTRHWY(I,J) + PDSTRHWYadjHist(I)) ) / L
            PJFTRadjSTEO(I) = PJFTRadjSTEO(I) +           &
                             (PJFTR(I,J) - (modelPJFTR(I,J) + PJFTRadjHist(I)) ) / L
!             PKSCMadjSTEO(I) = PJFTRadjSTEO(I)
!             PKSINadjSTEO(I) = PJFTRadjSTEO(I)
!             PKSRSadjSTEO(I) = PJFTRadjSTEO(I)

            PMGCMadjSTEO(I) = PMGCMadjSTEO(I) +           &
                             (PMGCM(I,J) - (modelPMGCM(I,J) + PMGCMadjHist(I)) ) / L
            PMGINadjSTEO(I) = PMGINadjSTEO(I) +           &
                             (PMGIN(I,J) - (modelPMGIN(I,J) + PMGINadjHist(I)) ) / L
            PMGTRadjSTEO(I) = PMGTRadjSTEO(I) +           &
                             (PMGTR(I,J) - (modelPMGTR(I,J) + PMGTRadjHist(I)) ) / L
!NO         PPFINadjSTEO(I) = PPFINadjSTEO(I) +           &
!NO                          (PPFIN(I,J) - (modelPPFIN(I,J) + PPFINadjHist(I)) ) / L
!NO         PRHELadjSTEO(I) = PRHELadjSTEO(I) +           &
!NO                          (PRHEL(I,J) - (modelPRHEL(I,J) + PRHELadjHist(I)) ) / L
!NO         PRHTRadjSTEO(I) = PRHTRadjSTEO(I) +           &
!NO                          (PRHTR(I,J) - (modelPRHTR(I,J) + PRHTRadjHist(I)) ) / L
            PPFINadjSTEO(I) = 0.0
            PRHELadjSTEO(I) = 0.0
            PRLTRadjSTEO(I) = 0.0
            PRHTRadjSTEO(I) = 0.0
            PRLCMadjSTEO(I) = PRLCMadjSTEO(I) +           &
                             (PRLCM(I,J) - (modelPRLCM(I,J) + PRLCMadjHist(I)) ) / L
            PRLELadjSTEO(I) = PRLELadjSTEO(I) +           &
                             (PRLEL(I,J) - (modelPRLEL(I,J) + PRLELadjHist(I)) ) / L
            PRLINadjSTEO(I) = PRLINadjSTEO(I) +           &
                             (PRLIN(I,J) - (modelPRLIN(I,J) + PRLINadjHist(I)) ) / L
!NO         PETTRadjSTEO(I) = PETTRadjSTEO(I) +           &
!NO                          (PETTR(I,J) - (modelPETTR(I,J) + PETTRadjHist(I)) ) / L
            PETTRadjSTEO(I) = 0.0

!      Survey of LPG data discountinued in 2011, so bogus data in rfhist
!      will use new (Aug 2012) Baysian algorithm to define historical/STEO years
!      do not adjust
!           PLGCM(:,CURIYR)
!           PLGIN(:,CURIYR)
!           PLGRS(:,CURIYR)
!           PLGTR(:,CURIYR)

          ENDDO      ! end loop over STEO calibration years
        ENDDO    ! end loop over Census Divisions for STEO calibration

! PETHMadjSTEO only available for total US (not by CD)
!              and no HIST version

        DO J=STEOLYR-L+1,STEOLYR  ! loop over STEO years
          PETHMadjSTEO(MNUMCR) = PETHMadjSTEO(MNUMCR) +           &
                           (PETHM(MNUMCR,J) - modelPETHM(MNUMCR,J) ) / L
        ENDDO      ! end loop over STEO calibration years


! write historical calibration and STEO benchmark adders to output file
        IF ( (FCRL .EQ. 1) .AND. ((LFadjhistON .EQ. 1) .OR. (LFadjsteoON .EQ. 1)) )  THEN
          UNITrfadj=FILE_MGR('O','RFADJBFO',.true.)

          write(UNITrfadj,986) 'adjHISTCAL           : ', (I, I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PASIN   : ', (PASINadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PDSCM   : ', (PDSCMadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PDSEL   : ', (PDSELadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PDSIN   : ', (PDSINadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PDSRS   : ', (PDSRSadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PDSTR   : ', (PDSTRadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PDSTRHWY: ', (PDSTRHWYadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PJFTR   : ', (PJFTRadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PMGCM   : ', (PMGCMadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PMGIN   : ', (PMGINadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PMGTR   : ', (PMGTRadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PPFIN   : ', (PPFINadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PRHEL   : ', (PRHELadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PRLTR   : ', (PRLTRadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PRHTR   : ', (PRHTRadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PRLCM   : ', (PRLCMadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PRLEL   : ', (PRLELadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PRLIN   : ', (PRLINadjHist(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjHISTCAL   PETTR   : ', (PETTRadjHist(I), I=1,MNUMCR-2)

          write(UNITrfadj,986) 'adjSTEObench         : ', (I, I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PASIN   : ', (PASINadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PDSCM   : ', (PDSCMadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PDSEL   : ', (PDSELadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PDSIN   : ', (PDSINadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PDSRS   : ', (PDSRSadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PDSTR   : ', (PDSTRadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PDSTRHWY: ', (PDSTRHWYadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PJFTR   : ', (PJFTRadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PMGCM   : ', (PMGCMadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PMGIN   : ', (PMGINadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PMGTR   : ', (PMGTRadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PPFIN   : ', (PPFINadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PRHEL   : ', (PRHELadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PRLTR   : ', (PRLTRadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PRHTR   : ', (PRHTRadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PRLCM   : ', (PRLCMadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PRLEL   : ', (PRLELadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PRLIN   : ', (PRLINadjSTEO(I), I=1,MNUMCR-2)
          write(UNITrfadj,987) 'adjSTEObench PETTR   : ', (PETTRadjSTEO(I), I=1,MNUMCR-2)

          write(UNITrfadj,987) 'adjSTEObench PETHM   : ', PETHMadjSTEO(MNUMCR)

          UNITrfadj=FILE_MGR('C','RFADJBFO',.false.)    !  close file

 986      FORMAT(A23,11I8)
 987      FORMAT(A23,11F8.3)
        ENDIF

       ENDIF      ! end conditional that defines year to calc STEO bench
      ENDIF       ! end conditional to bench to STEO



      DO I=1,MNUMCR
       DO J=1,HISTLYR               !Correct Wholesale Price Calculation -sh7 8/9/10
            PJF(I,J) = ( PJFTR(I,J) - MUFTAX(J,3)/MC_JPGDP(J) &
                       - JFSTTX(I)- JFMUTR(I,J,1) )* CFJFK
       ENDDO
      ENDDO

      DO I=1,9
       DO J=1,HISTLYR                     !Calculate Regional Wholesale Prices -sh7 8/17/10

        IF (I.GT.3) THEN
         PDSU(I,J) = PDSU(MNUMCR,J)* (PDSTR(I,J)/PDSTR(7,J))
        ELSE
         PDSU(I,J) = PDSU(MNUMCR,J)* (PDSTR(I,J)/PDSTR(2,J))
        ENDIF
       ENDDO
      ENDDO

      DO I=1,9
       DO J=1,HISTLYR                     !Calculate Regional Heating Oil Wholesale Prices -sh7 11/02/10

        IF (I.GT.3) THEN
         PDS(I,J) = PDS(MNUMCR,J)* (PDSRS(I,J)/PDSRS(7,J))
        ELSE
         PDS(I,J) = PDS(MNUMCR,J)* (PDSRS(I,J)/PDSRS(2,J))
        ENDIF
       ENDDO
      ENDDO


      DO I=1,9
       DO J=1,HISTLYR                     !Calculate Regional MG Wholesale Prices -sh7 8/25/10

        IF (I.GT.3) THEN
         PALMG(I,J) = PALMG(MNUMCR,J)* (PMGTR(I,J)/PMGTR(MNUMCR,J))
        ELSE
         PALMG(I,J) = PALMG(MNUMCR,J)* (PMGTR(I,J)/PMGTR(2,J))
        ENDIF
       ENDDO
      ENDDO



! 3/8/16 em4 read through STEO years for side case overwrites
     DO I=1,MNUMCR
      DO J=1,STEOLYR
       DO L=1,MNUMCGF                          !cogen fuel type
        DO K=1,2
          CGREFGEN(I,J,L,K)=0.0
        ENDDO
        CGREFCAP(I,J,L)=0.0
        CGREFQ(I,J,L)=0.0
       ENDDO
      ENDDO
     ENDDO

      CALL PMM_NEXTDATA(IUNIT1)
! 3/8/16 em4 read through STEO years for side case overwrites
      DO J =1,STEOLYR
       DO K = 1,2
        DO L = 1,MNUMCGF
          IF (L .EQ.  1 .OR. L .EQ.  4 .OR. L .EQ.  5 .OR. L .EQ.  7 .OR. &
              L .EQ.  8 .OR. L .EQ. 10 .OR. L .EQ. 11 .OR. L .EQ. 12) CYCLE
          READ(IUNIT1,128) (CGREFGEN(I,J,L,K),I=1,DMDRGNS)
        ENDDO
       ENDDO
       DO L = 1,MNUMCGF
          IF (L .EQ.  1 .OR. L .EQ.  4 .OR. L .EQ.  5 .OR. L .EQ.  7 .OR. &
              L .EQ.  8 .OR. L .EQ. 10 .OR. L .EQ. 11 .OR. L .EQ. 12) CYCLE
          READ(IUNIT1,128) (CGREFCAP(I,J,L),I=1,DMDRGNS)
       ENDDO
       DO L = 1,MNUMCGF                         ! OG,PT,NG,OT, tril BTU/yr
          IF (L .EQ.  1 .OR. L .EQ.  4 .OR. L .EQ.  5 .OR. L .EQ.  7 .OR. &
              L .EQ.  8 .OR. L .EQ. 10 .OR. L .EQ. 11 .OR. L .EQ. 12) CYCLE
          READ(IUNIT1,128) (CGREFQ(I,J,L),I=1,DMDRGNS)

!! do NOT add fuel for cogen since QNGRF already is assumed to account for it
!!        IF(L.EQ.9) THEN                          ! OG into still Gas
!!         DO I=1,DMDRGNS
!!          QSGRF(I,J) = QSGRF(I,J) + CGREFQ(I,J,L)
!!         ENDDO
!!        ELSEIF (L.EQ.3) THEN                     ! NG into nat gas
!!         DO I=1,DMDRGNS
!!          QNGRF(I,J) = QNGRF(I,J) + CGREFQ(I,J,L)
!!         ENDDO
!!        ELSEIF (L.EQ.6) THEN
!! but do set QBMRF
          IF (L.EQ.6) THEN
           DO I=1,DMDRGNS                 ! no QMSRF variable, so put it in biomass
            QBMRF(I,J) = CGREFQ(I,J,L)    ! don't tack it on as SG, NG because history for QBMRF not read above and this would build over iterations
           ENDDO
          ENDIF

       ENDDO
      ENDDO

! 3/8/16 em4 read through STEO years for side case overwrites
      DO J=1,STEOLYR
       CGREFGEN(MNUMCR,J,:,:)= 0.0
       CGREFCAP(MNUMCR,J,:) =0.0
       CGREFQ(MNUMCR,J,:)= 0.0
       DO L =1,MNUMCGF                          ! fuel type consumed
        DO I=1,MNUMCR-2
         DO K=1,2                                   ! self, grid
          CGREFGEN(MNUMCR,J,L,K)=CGREFGEN(MNUMCR,J,L,K)+CGREFGEN(I,J,L,K)
         ENDDO
         CGREFCAP(MNUMCR,J,L)=CGREFCAP(MNUMCR,J,L)+CGREFCAP(I,J,L)
         CGREFQ(MNUMCR,J,L)=CGREFQ(MNUMCR,J,L)+CGREFQ(I,J,L)
        ENDDO
       ENDDO
      ENDDO

! 3/8/16 em4 save STEO years for side case overwrites
      DO J=1,STEOLYR
       DO L =1,MNUMCGF                          ! fuel type consumed
        DO I=1,MNUMCR
         DO K=1,2                                   ! self, grid
          SIDE_CGREFGEN(I,J,L,K) =CGREFGEN(I,J,L,K)
         ENDDO
         SIDE_CGREFCAP(I,J,L)    =CGREFCAP(I,J,L)
         SIDE_CGREFQ(I,J,L)      =CGREFQ(I,J,L)
        ENDDO
       ENDDO
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)
      DO J = 1,HISTLYR
       READ(IUNIT1,56) ETHSTKCHG(J)                  ! ethanol stock change
       ETHSTKCHG(J) = ETHSTKCHG(J) / 365. * (-1.)    ! convert to daily average and supply (withdraw)
       IF (MOD(J+1989,4) .EQ. 0) ETHSTKCHG(J) = ETHSTKCHG(J) * 365. / 366.
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)
      DO J = 1,HISTLYR
       READ(IUNIT1,56) BIODSTKCHG(J)                 ! biodiesel stock change
       BIODSTKCHG(J) = BIODSTKCHG(J) / 365. * (-1.)  ! convert to daily average and supply (withdraw)
       IF (MOD(J+1989,4) .EQ. 0) BIODSTKCHG(J) = BIODSTKCHG(J) * 365. / 366.
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)
      DO J = 1,HISTLYR
!       WRITE(6,*) 'RFPRDDIESEL...'
       READ(IUNIT1,57) (RFPRDDIESEL(I,J),I=1,MNUMCR)
       RFPRDDIESEL(:,J) = RFPRDDIESEL(:,J) / 365.
       IF (MOD(J+1989,4) .EQ. 0) RFPRDDIESEL(:,J) = RFPRDDIESEL(:,J) * 365. / 366.
      ENDDO

      CALL PMM_NEXTDATA(IUNIT1)
!!!   DO J = 1,HISTLYR
      DO J = 1,22                ! 1-13-16, hardcoded because only have data through 2011
       READ(IUNIT1,627) PROFIT_BBL(MNUMPR,J)              !data in 87$/gal
       PROFIT_BBL(MNUMPR,J) = PROFIT_BBL(MNUMPR,J) *42.   !want 87$/bbl
      ENDDO

! Special data:  soybean oil price; data in nom cents/lb, so need to convert to 87$/bbl
      CALL PMM_NEXTDATA(IUNIT1)
      READ(IUNIT1,680) SBO_lbgal, SBO_hyr1, SBO_hyr2, CDcol
      SBO_hyr1 = SBO_hyr1-1989   ! nems year (eg, 12 = 2001)
      SBO_hyr2 = SBO_hyr2-1989   ! nems year
      READ(IUNIT1,681) (CDindex(I), I=1,CDcol)
      DO J = SBO_hyr1, SBO_hyr2
        DO I=1,MNUMCR
          SBO_PRICE(I,J) = 0.0
        ENDDO
      ENDDO
      DO J = SBO_hyr1, SBO_hyr2
        READ(IUNIT1,682) SBO_hyr, (SBO_PRICE(CDindex(I),SBO_hyr), I=1,CDcol)
       !convert fr nom cents/lb to 87$/bbl
        DO I=1,CDcol
          SBO_PRICE(CDindex(I),SBO_hyr) = SBO_PRICE(CDindex(I),SBO_hyr) *          &
                                          SBO_lbgal *42 /MC_JPGDP(SBO_hyr) /100.
        ENDDO
      ENDDO

! Special data:  corn price; data in nom $/bushel, so need to convert to 87$/bushel
      CALL PMM_NEXTDATA(IUNIT1)
      READ(IUNIT1,680) CRN_dummy, CRN_hyr1, CRN_hyr2, CDcol
      CRN_hyr1 = CRN_hyr1-1989   ! nems year (eg, 12 = 2001)
      CRN_hyr2 = CRN_hyr2-1989   ! nems year
      READ(IUNIT1,681) (CDindex(I), I=1,CDcol)
      DO J = CRN_hyr1, CRN_hyr2
        DO I=1,MNUMCR
          CRNPRICE(I,J) = 0.0
        ENDDO
      ENDDO
      DO J = CRN_hyr1, CRN_hyr2
        READ(IUNIT1,682) CRN_hyr, (CRNPRICE(CDindex(I),CRN_hyr), I=1,CDcol)
       !convert fr nom cents/lb to 87$/bbl
        DO I=1,CDcol
          CRNPRICE(CDindex(I),CRN_hyr) = CRNPRICE(CDindex(I),CRN_hyr) /          &
                                         MC_JPGDP(CRN_hyr)
        ENDDO
      ENDDO

! Special data:  yellow grease price; data in nom cents/pound, so need to convert to 87$/bbl
      CALL PMM_NEXTDATA(IUNIT1)
      DO J= 1,21 ! initialize years 1990-2010 to 0.0 for all CDs
          YGR_PRICE(:,J) = 0.0
      ENDDO
      READ(IUNIT1,680) YGR_lbgal, YGR_hyr1, YGR_hyr2, CDcol
      YGR_hyr1 = YGR_hyr1-1989   ! nems year (eg, 12 = 2001)
      YGR_hyr2 = YGR_hyr2-1989   ! nems year
      READ(IUNIT1,681) (CDindex(I), I=1,CDcol)
      DO J = YGR_hyr1, YGR_hyr2
        DO I=1,MNUMCR
          YGR_PRICE(I,J) = 0.0
        ENDDO
      ENDDO
      DO J = YGR_hyr1, YGR_hyr2   
      READ(IUNIT1,682) YGR_hyr, (YGR_PRICE(CDindex(I),YGR_hyr), I=1,CDcol)
       !convert fr nom cents/lb to 87$/bbl
        DO I=1,CDcol
          YGR_PRICE(CDindex(I),YGR_hyr) = YGR_PRICE(CDindex(I),YGR_hyr) *          &
                                          YGR_lbgal *42 /MC_JPGDP(YGR_hyr) /100.
        ENDDO
      ENDDO

   680 FORMAT(F5.3, 2x,I4, 2x,I4, 2x,I2)
   681 FORMAT(7x, 11(6x,I3))
   682 FORMAT(5x,I2, 1X,11(F9.2))


      DO J=1,HISTLYR
       XRFQDCRD(MNUMOR,J)=0.0
       DO I=1,MNUMOR-1
        XDCRDWHP(I,J)=DCRDWHP(I,J)
        XRFQDCRD(I,J)=RFQDCRD(I,J)
        XRFQDCRD(MNUMOR,J)=XRFQDCRD(MNUMOR,J)+XRFQDCRD(I,J)
       ENDDO
      ENDDO

      CLOSE(IUNIT1)


! Write STEO_Hist.gdx so that the LFMM can use the benchmarking factors
      ok = gdxCreate(pgdxB,errMsg)

      IF (.not.ok) then
         write(6,'(a,a)') 'Error creating STEO_Hist.gdx file:  ', trim(errMsg)
         stop
      ENDIF

      ok2 = gdxOpenWrite(pgdxB,"STEO_Hist.gdx","refine.f",ErrNr)
      Write(6,*) 'Opening STEO_Hist.gdx'

      DO I = 1, MNUMYR
         write(CMNUMYR(i),*) I+1989
         CMNUMYR(i)=TRIM(ADJUSTL(CMNUMYR(i))) // '_MNUMYR'
      ENDDO
      DO I = 1, MNUMCR
         write(CMNUMCR(i),*) I
         CMNUMCR(i)='CenDiv' // TRIM(ADJUSTL(CMNUMCR(i)))
      ENDDO
      DO I = 1, MNUMPR
         write(CMNUMPR(i),*) I

         IF (I.LE.9) THEN
           CMNUMPR(I) = '0' // TRIM(ADJUSTL(CMNUMPR(I))) // '_MNUMPR'
         ELSE
           CMNUMPR(I) = TRIM(ADJUSTL(CMNUMPR(I))) // '_MNUMPR'
         ENDIF
      ENDDO

      MM = 2
      DO I = 1, MM
         write(C_MM(I),*) I
         C_MM(I)=TRIM(ADJUSTL(C_MM(I))) // '_M2'
      ENDDO

      call WriteGDXElement(pgdxB, 0, 'MNUMYR', 'Model years',       1, MNUMYR, 1, 1, 1, 1, CMNUMYR,j2,k2,l2,m2,'I')
      call WriteGDXElement(pgdxB, 0, 'MNUMPR', 'Model years',       1, MNUMPR, 1, 1, 1, 1, CMNUMPR,j2,k2,l2,m2,'I')
      call WriteGDXElement(pgdxB, 0, 'CenDiv', 'Census Divisions',  1, MNUMCR, 1, 1, 1, 1, CMNUMCR,j2,k2,l2,m2,'I')
      call WriteGDXElement(pgdxB, 0, 'M2', 'Index set',  1, MM, 1, 1, 1, 1, C_MM,j2,k2,l2,m2,'I')

      call WriteGDXElement(pgdxB, 1, 'STEO_CONIPUS', 'STEO Net Crude Imports', CONIPUS, MNUMYR, 1, 1, 1, 1, CMNUMYR,j2,k2,l2,m2,'R')
      call WriteGDXElement(pgdxB, 1, 'STEO_CORIPUS', 'STEO Crude Input to Refineries', CORIPUS, MNUMYR, 1, 1, 1, 1, CMNUMYR,j2,k2,l2,m2,'R')
      call WriteGDXElement(pgdxB, 1, 'STEO_UONIPUS', 'STEO Net UFO Imports', UONIPUS, MNUMYR, 1, 1, 1, 1, CMNUMYR,j2,k2,l2,m2,'R')
      call WriteGDXElement(pgdxB, 1, 'STEO_PANIPUS', 'STEO Net Product Imports', PANIPUS, MNUMYR, 1, 1, 1, 1, CMNUMYR,j2,k2,l2,m2,'R')
      call WriteGDXElement(pgdxB, 1, 'STEO_MBNIPUS', 'STEO Net BOB Imports', MBNIPUS, MNUMYR, 1, 1, 1, 1, CMNUMYR,j2,k2,l2,m2,'R')
      call WriteGDXElement(pgdxB, 1, 'STEO_OHNIPUS', 'STEO Net Other Hydrocarbon and Oxygenate Imports', OHNIPUS, MNUMYR, 1, 1, 1, 1, CMNUMYR,j2,k2,l2,m2,'R')

      call WriteGDXElement(pgdxB, 1, 'HIST_RFQEXCRD', 'STEO Gross Crude Exports', RFQEXCRD, MNUMPR, MNUMYR, 1, 1, 1, CMNUMPR,CMNUMYR,k2,l2,m2,'R')
      call WriteGDXElement(pgdxB, 1, 'HIST_RFQICRD', 'STEO Gross Crude Imports', RFQICRD, MNUMPR, MNUMYR, 1, 1, 1, CMNUMPR,CMNUMYR,k2,l2,m2,'R')
      call WriteGDXElement(pgdxB, 1, 'HIST_RFQEXPRDT', 'STEO Gross Product Exports', RFQEXPRDT, MNUMPR, MNUMYR, 1, 1, 1, CMNUMPR,CMNUMYR,k2,l2,m2,'R')
      call WriteGDXElement(pgdxB, 1, 'HIST_RFPQIPRDT', 'STEO Gross Product Imports', RFPQIPRDT, MNUMPR, MNUMYR, MM, 1, 1, CMNUMPR,CMNUMYR,C_MM,l2,m2,'R')
      call WriteGDXElement(pgdxB, 1, 'HIST_BLDIMP', 'STEO Net BOB Imports', BLDIMP, MNUMPR, MNUMYR, 1, 1, 1, CMNUMPR,CMNUMYR,k2,l2,m2,'R')
      call WriteGDXElement(pgdxB, 1, 'HIST_RFPQUFC', 'STEO Gross UFO Imports', RFPQUFC, MNUMPR, MNUMYR, MM, 1, 1, CMNUMPR,CMNUMYR,C_MM,l2,m2,'R')

      call WriteGDXElement(pgdxB, 1, 'STEOLYR', 'Last STEO year', STEOLYR, 1,1,1,1,1,  i2,j2,k2,l2,m2,'I')
      call WriteGDXElement(pgdxB, 1, 'HISTLYR', 'Last year of data read', HISTLYR, 1,1,1,1,1,  i2,j2,k2,l2,m2,'I')
      call WriteGDXElement(pgdxB, 1, 'LFadjvolON', 'Switch for LFMM STEO volume adjustments', LFadjvolON, 1,1,1,1,1,  i2,j2,k2,l2,m2,'I')
      call WriteGDXElement(pgdxB, 1, 'NumPhaseoutYrs', 'Number of STEO phase out years', NumPhaseoutYrs, 1,1,1,1,1,  i2,j2,k2,l2,m2,'I')
      call WriteGDXElement(pgdxB, 1, 'PhaseoutFactor', 'Phase out factor per year', PhaseoutFactor, 1,1,1,1,1,  i2,j2,k2,l2,m2,'R')

      Write(6,'(a)') 'Done with STEO and historical data.  Closing STEO_Hist.gdx'
      ok2 = gdxClose(pgdxB)





!   25 FORMAT(13X, <PMMRGNS>F10.0)
   625 FORMAT(13X, F10.0)
!  125 FORMAT(A1,12X, <PMMRGNS>F10.0)
!   26 FORMAT(15X, <PMMRGNS+1>F10.0)
!   27 FORMAT(15X, <PMMRGNS+1>F7.3)
   27 FORMAT(13X, <PMMRGNS+1>F10.3)
   627 FORMAT(13X, F10.3)
!   28 FORMAT(15X, 9F7.0)
  128 FORMAT(13X, 9F8.2)
!   29 FORMAT(12X, 12F7.3)
!   30 FORMAT(15X,  <PMMRGNS+1>F7.3)
   31 FORMAT(13X,<PMMRGNS>F10.3)
   631 FORMAT(13X,F10.3)
   32 FORMAT(13X,<PMMRGNS+1>F10.3)
   632 FORMAT(13X,F10.3)
!   33 FORMAT(13X,F9.0,<PMMRGNS>F10.0)
!   34 FORMAT(15X,F8.2,<PMMRGNS>F10.2)
   35 FORMAT(8X, 9F9.3)
   36 FORMAT(8X,9F7.2)
!   43 FORMAT(16X,F10.4)
   44 FORMAT(16X,F10.3)
   45 FORMAT(13X,F10.3)
   46 FORMAT(17X,F10.0)
!   55 FORMAT(13X, F9.0,<PMMRGNS>F10.0)
   56 FORMAT(13X,F10.0)
   57 FORMAT(13X, <MNUMCR>F8.0)
   90 FORMAT(A1)
   94 FORMAT(9(2X,F5.2))
   99 FORMAT(I2)

      RETURN
      END                                        !  END OF RFHIST1
