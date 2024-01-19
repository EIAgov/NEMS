! $Header: m:/default/source/RCS/intercv.f,v 1.63 2019/06/20 20:36:49 pkc Exp $
      PROGRAM INTERCV
      USE DFLIB

      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'cdsparms'
      include 'qblk'
      include 'mpblk'         ! NOTE:  UNADJUSTED PRICES--NO CARBON ALLOWANCE INCLUDED, except elec
      include 'ngtdmout'      ! NOTE:  UNADJUSTED PRICES--NO CARBON ALLOWANCE INCLUDED
      include 'ngtdmrep'
      include 'ogsmout'
      include 'uefdout'
      include 'coalemm'
      include 'coalprc'
      include 'macout'
      include 'intout'
      include 'pmmrpt'
      include 'pmmout'
      include 'emission'
      INTEGER MNOTH
      PARAMETER(MNOTH=65)
      include 'converge'
      include 'uso2grp'
      include 'wrenew'
      include 'continew'

      INTEGER IRESTART,FMT(2),FIRST_YEAR,LAST_YEAR,TOTAL_NOT_CONVERGED

      LOGICAL FILE_EXIST
      CHARACTER*100 DICTNAME,RESTART(2),CONVERGE_FILE, FILERNAME,VARLISTNAME

      INTEGER NUM_GAS
      PARAMETER(NUM_GAS=10)

! Variables holding values to be checked for convergence
      REAL Q(MNUMCR,MNUMYR,MNUMQ,3)  ! main quantity
      REAL GQ(NNGEM,MNUMYR,NUM_GAS,3)      ! gas/emm quantity
      REAL CQ(NDRGG,MNUMYR,NCLUT1,3) ! coal/emm quantity
      REAL OTHER(MNOTH,MNUMYR,3),PCAP(MX_NCOALS,MNUMYR,3),CL_CF(MX_UNITS,MNUMYR,3),CL_BTUs(MX_UNITS,MNUMYR,3)

! Variables holding adjusted price values to be checked for convergence
      REAL P(MNUMCR,MNUMYR,MNUMP,3)  ! main price
      REAL GP(NNGEM,MNUMYR,NUM_GAS,3)  ! gas/emm price
      REAL CP(2,NDRGG,MNUMYR,3)      ! coal price to emm
      common/cblk/P,GP,CP            ! in common to allow separate access to carbon-adjusted prices in 
                                     ! nonCONTAINed subroutine copy_to_local_adjusted
 
 ! Variables for relaxation assumptions
      REAL RLXQ(MNUMQ),RLXP(MNUMP),RLXGQ(NUM_GAS),RLXGP(NUM_GAS),RLXCQ(NCLUT1),RLXCP,RLXOTHER(MNOTH),RLXPCAP(3)
 
 
 ! Convergence criteria
      REAL MNQCNV(2,MNUMQ),MNPCNV(2,MNUMP)
      REAL MNGQCNV(2,NUM_GAS),MNGPCNV(2,NUM_GAS)
      REAL MNCQCNVS(2,NCLUT1),MNCPCNVS(2)
      REAL MNOCNV(2,MNOTH)
      REAL MNZCNV(2,3)

! Convergence identification variables
      CHARACTER*6 MPVARSL(MNUMP),MQVARSL(MNUMQ)
      CHARACTER*9 MGPVARSL(NUM_GAS),MGQVARSL(NUM_GAS)
      CHARACTER*7 MCQVARSLS(NCLUT1)
      CHARACTER*9 MCPVARSLS(2)
      CHARACTER*8 MZVARSL(3)

      character*3 n_envstr,nruns_envstr,doemall_envstr
      integer irun, nruns, doemall
      real*4 ave_score,min_score
      character*10 min_score_c  ! command line argument holding the minimum score
!================================      
      character*10 debug_flag   ! command line argument holding debug print flag.
      common/blk_dbg/debug_flag
!=================================      
      CHARACTER*8 PASSFAIL(0:1)/' *FAIL* ',' *PASS* '/
      CHARACTER*6 REASONMOD
      INTEGER PFGPA
! XCL_PCAP
      DATA MZVARSL/'XCL_PCAP','EM_CL_CF','E_CLBTUs'/
! Other:
!  MOVARS holds the variable names (approximately) for the additional convergence tests.  If more than
!  30 variables get tested, then increase the size of the array (MNOTH)

      CHARACTER*9  MOVARSL(MNOTH)
      DATA MOVARSL/'MC_GDPR','IT_WOP','RFQICRD','RFPQIPRDT', &                        ! 1- 4
         'EMETAX','EMELPSO2','ECP_PHG','OGWPRNG', &                                   ! 5- 8
         'OGCNPPRD1','OGCNPPRD2','PLNGIMP03','PLNGIMP04','PLNGIMP05','PLNGIMP06', &   ! 9-14
         'PLNGIMP07','PLNGIMP08','PLNGIMP09','PLNGIMP10','PLNGIMP11','PLNGIMP12', &   !15-20
         'PBMET01'  ,'PBMET02'  ,'PBMET03'  ,'PBMET04'  ,'PBMET05'  ,             &   !21-25
         'PBMET06'  ,'PBMET07'  ,'PBMET08'  ,'PBMET09'  ,'PBMET10'  ,             &   !26-30
         'QBMET01'  ,'QBMET02'  ,'QBMET03'  ,'QBMET04'  ,'QBMET05'  ,             &   !31-35
         'QBMET06'  ,'QBMET07'  ,'QBMET08'  ,'QBMET09'  ,'QBMET10'  ,             &   !36-40
         'Unused01 ','Unused02 ','GLBCRDDMD',                                     &   !41-43
         'PSO2_00_1','PSO2_01_1','PSO2_02_1','PSO2_03_1','PSO2_04_1','PSO2_05_1', &   !44-49
         'PSO2_06_1','PSO2_07_1','PSO2_08_1','PSO2_09_1','PSO2_10_1',             &   !50-54
         'PSO2_00_2','PSO2_01_2','PSO2_02_2','PSO2_03_2','PSO2_04_2','PSO2_05_2', &   !55-60
         'PSO2_06_2','PSO2_07_2','PSO2_08_2','PSO2_09_2','PSO2_10_2'/                 !61-65
                                                                                                                                                                                                                  
! +++ QUANTITY Variables:
      DATA MQVARSL/ &
       'QELRS','QELCM','QELTR','QELIN','QELRF','QELHM','QELAS', &
       'QGFRS','QGFCM','QGFTR','QGFIN','QGFRF','QGFEL','QGFHM','QGFAS', &
       'QGIRS','QGICM','QGITR','QGIIN','QGIRF','QGIEL','QGIHM','QGIAS', &
       'QNGRS','QNGCM','QNGTR','QNGIN','QNGRF','QNGEL','QNGHM','QNGAS', &
       'QGPTR','QLPIN', &
       'QCLRS','QCLCM','QCLIN','QCLRF','QCLEL','QCLSN','QCLHM','QCLAS', &
       'QMCIN','QMGCM','QMGTR','QMGIN','QMGAS','QJFTR', &
       'QDSRS','QDSCM','QDSTR','QDSIN','QDSRF','QDSEL','QDSAS', &
       'QKSRS','QKSCM','QKSIN','QKSAS', &
       'QLGRS','QLGCM','QLGTR','QLGIN','QLGRF','QLGAS', &
       'QRLCM','QRLTR','QRLIN','QRLRF','QRLEL','QRLAS', &
       'QRHTR','QRHEL','QRHAS', &
       'QRSCM','QRSTR','QRSIN','QRSRF','QRSEL','QRSAS', &
       'QPFIN','QSGIN','QSGRF','QPCIN','QPCRF','QPCEL','QPCAS','QASIN', &
       'QOTTR','QOTIN','QOTRF','QOTAS', &
       'QTPRS','QTPCM','QTPTR','QTPIN','QTPRF','QTPEL','QTPAS', &
       'QMETR','QETTR','QETHM','QHYTR','QUREL','QURHM', &
       'QHOIN','QHOEL','QHOAS','QGERS','QGEIN','QGEEL','QGEAS', &
       'QBMRS','QBMCM','QBMIN','QBMRF','QBMEL','QBMSN','QBMHM','QBMAS', &
       'QMSIN','QMSEL','QMSAS', &
       'QSTRS','QSTCM','QSTIN','QSTEL','QSTAS', &
       'QPVRS','QPVCM','QPVIN','QPVEL','QPVAS', &
       'QWIIN','QWIEL','QWIAS', &
       'QTRRS','QTRCM','QTRTR','QTRIN','QTREL','QTRSN','QTRHM','QTRAS', &
       'QEIEL','QCIIN', &
       'QTSRS','QTSCM','QTSTR','QTSIN','QTSRF','QTSEL','QTSSN','QTSHM','QTSAS', &
       'QH1TR','QH2TR','QH3TR'/

! +++ PRICE Variables:
      DATA MPVARSL/ &
       'PELRS','PELCM','PELTR','PELIN','PELAS', &
       'PGFRS','PGFCM','PGFTR','PGFIN','PGFEL','PGFAS', &
       'PGIRS','PGICM','PGITR','PGIIN','PGIEL','PGIAS', &
       'PNGRS','PNGCM','PNGTR','PNGIN','PNGEL','PNGAS', &
       'PGPTR','PLPIN', &
       'PCLRS','PCLCM','PCLIN','PCLEL','PCLSN','PCLAS','PMCIN', &
       'PMGCM','PMGTR','PMGIN','PMGAS','PJFTR', &
       'PDSRS','PDSCM','PDSTR','PDSIN','PDSEL','PDSAS', &
       'PKSRS','PKSCM','PKSIN','PKSAS', &
       'PLGRS','PLGCM','PLGTR','PLGIN','PLGAS', &
       'PRLCM','PRLTR','PRLIN','PRLEL','PRLAS', &
       'PRHTR','PRHEL','PRHAS','PRSCM','PRSTR','PRSIN','PRSEL','PRSAS', &
       'PPFIN','PASIN','POTTR','POTIN','POTAS', &
       'PTPRS','PTPCM','PTPTR','PTPIN','PTPRF','PTPEL','PTPAS', &
       'PMETR','PETTR','PHYTR','PUREL', &
       'PH1TR','PH2TR','PH3TR'/

! +++ QUANTITY variables for natural gas and coal from utilities:
      DATA MGQVARSL/'QGFELGR','QGIELGR','QGCELGR', &
              'QGFELGRS1','QGFELGRS2','QGIELGRS1','QGIELGRS2', &
              'SQNGELGR1','SQNGELGR2','SQNGELGR3'/
!  New coal
      DATA MCQVARSLS/ 'QCLB1NR', 'QCLB2NR', 'QCLB3NR', 'QCLB4NR', 'QCLB5NR', 'QCLB6NR', 'QCLB7NR', 'QCLB8NR', &
           'QCLC1NR', 'QCLC2NR', 'QCLC3NR', 'QCLC4NR', 'QCLC5NR', 'QCLC6NR', 'QCLC7NR', 'QCLC8NR', 'QCLC9NR', &
           'QCLCXNR', 'QCLCYNR', 'QCLCZNR', &
           'QCLH1NR', 'QCLH2NR', 'QCLH3NR', 'QCLH4NR', 'QCLH5NR', 'QCLH6NR', 'QCLH7NR', 'QCLH8NR', 'QCLH9NR', &
           'QCLHANR', 'QCLHBNR', 'QCLHCNR', &
           'QCLPCNR', 'QCLOCNR', 'QCLIGNR', 'QCLI2NR', 'QCLPQNR', 'QCLISNR'/
      DATA MCPVARSLS / 'PCLELCDR1' , 'PCLELCDR2' /

! +++ PRICE variables for natural gas and coal to utilities:
      DATA MGPVARSL/'PGFELGR','PGIELGR','PGCELGR', &
              'PGFELGRS1','PGFELGRS2','PGIELGRS1','PGIELGRS2', &
              'SPNGELGR1','SPNGELGR2','SPNGELGR3'/

! Check for command line argument for min_score to consider as convergence
      min_score=2.7
      CALL GETARG (1, min_score_c)

      if(len_trim(min_score_c).eq.0) then
        min_score=2.7
      else
        read(min_score_c,*,end=14,err=14) min_score
14      write(6,*) 'Min_Score from command line =',min_score
      endif
      !call getarg (2, debug_flag)
      debug_flag='debug'
      
! get environment strings n and nruns that are set in cycle.sh which usually runs this program.

      irun=3
      nruns=3
      n_envstr=' '
      nruns_envstr=' '
      doemall_envstr=' '
      call getenv('n',n_envstr)
      call getenv('nruns',nruns_envstr)
      call getenv('doemall',doemall_envstr)
      if(n_envstr.ne.' ') then
         read(n_envstr,*,err=16,end=16) irun
      endif
16 continue
      if(nruns_envstr.ne.' ') then
         read(nruns_envstr,*,err=17,end=17) nruns
      endif
17 continue
      if(doemall_envstr.ne.' ') then
         read(doemall_envstr,*,err=18,end=18) doemall
      endif
18 continue

! File containing the following reads from standard input, usually redirected (<) from intercvfiles.txt )
      read(5,'(11x,a)')restart(1)   ! output restart or current restart
      read(5,'(11x,i1)')fmt(1)
      read(5,'(11x,a)')restart(2)   ! input restart or old restart
      read(5,'(11x,i1)')fmt(2)
      read(5,'(11x,a)') converge_file
      read(5,'(11x,a)') dictname
      read(5,'(11x,a)') varlistname
      read(5,'(11x,a)') filername

      FIRST_YEAR=MSEDYR+2
      LAST_YEAR=MNUMYR
      CALL INITIALIZE_FILER
      CALL READ_CONVERGE
! read in old restart file, then current restart, and perform intercycle convergence test.
! This test includes writing out those items not meeting the convergence criteria. Then
! summarize the intercycle convergence test and give a score.

      IRESTART=2 ! old 
      CALL PROCESS_RESTART
      CALL COPY_TO_LOCAL_ADJUSTED(IRESTART)  ! overwrite local copy of prices with carbon-adjusted prices
      IRESTART=1 ! current or new 
      CALL PROCESS_RESTART
      CALL COPY_TO_LOCAL_ADJUSTED(IRESTART)  ! overwrite local copy of prices with carbon-adjusted prices
      CALL TEST_CONVERGE

      CALL SUM_CONVERGE(1)   ! Calc intercycle Convergence score at US level before relaxation
      CALL SUM_CONVERGE(2)   ! Summarize Regional Convergence and Create Summary of Convergence for Ftab Table before relax
      call output_restart(restart(1))
      
      IF (IRELAX .EQ. 2) THEN
! this requires reloading the two restart files in the original order (so most recent in
! memory), performing the relaxation, then writing out the restart file as restart.rlx.
! That file will be used as the next restart file if there is another cycle.  If this check
! is on the last cycle, or if the convergence check is passed, this relaxed restart file
! will not be used or affect results.
!   the Relaxation is performed with unadjusted prices (not carbon-adjusted).

          irestart=2 ! old
          call process_restart
          irestart=1 ! current or new
          call process_restart
          CALL RELAX      ! Apply relaxation between cycles.  
          CALL OUTPUT_RESTART('restart.rlx')
          write(6,*) 'Relaxed convergence variables written to restart.rlx'
      ENDIF

      REASONMOD = 'Other '
!  0 is "FAIL"; 1 is "PASS"
!  the last true IF takes precedence setting the reason to continue cycling, so order appropriately
      IF (CONTINH .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONH
         REASONMOD=SUBR_NAMES(13)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINW .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONW
         REASONMOD=SUBR_NAMES(1)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINN .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONN
         REASONMOD=SUBR_NAMES(12)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINR .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONR
         REASONMOD=SUBR_NAMES(3)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINK .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONK
         REASONMOD=SUBR_NAMES(4)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINT .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONT
         REASONMOD=SUBR_NAMES(6)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINI .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONI
         REASONMOD=SUBR_NAMES(5)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINL .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONL
         REASONMOD=SUBR_NAMES(9)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTING .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONG
         REASONMOD=SUBR_NAMES(10)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINC .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONC
         REASONMOD=SUBR_NAMES(8)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINM .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONM
         REASONMOD=SUBR_NAMES(2)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINO .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONO
         REASONMOD=SUBR_NAMES(11)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (CONTINE .EQ. 0) THEN              !  0 is fail
         REASONYES=0
         REASONCHR=REASONE
         REASONMOD=SUBR_NAMES(7)
         write(6,'("Pass/Fail:",I4,"  Reason: ",A20,"  Model: ",A6)') REASONYES,REASONCHR,REASONMOD
      ENDIF
      IF (AVE_SCORE .GT. MIN_SCORE) PFGPA = 1
      if(irun.lt.nruns) then
        if(pfgpa .eq. 1 .and. reasonyes .eq. 1) then
          write(6,'(a)') 'A passing GPA and no model events.   We can stop early.'
          stop 1   ! cycle.sh can pick this return code up and exit loop
        else if (reasonyes .eq. 0) then
          write (6,'(" We continue cycling.   GPA:",a8,a8,a20)') &
      PASSFAIL(PFGPA),trim(REASONMOD)//":",trim(REASONCHR)
        else
          write (6,'(" We continue cycling.   GPA:",a8,a8,a8)') &
      PASSFAIL(PFGPA),trim(REASONMOD)//":",PASSFAIL(REASONYES)
        endif
      else         ! if last cycle, including if only cycle
        if(pfgpa .eq. 1 .and. reasonyes .eq. 1) then
          write(6,'("Congratulations for passing our rigorous testing standards.   GPA:",a8,a20,a8)') &
      PASSFAIL(PFGPA),trim(REASONCHR)//":",PASSFAIL(REASONYES)
        else if (reasonyes .eq. 0) then
          write (6,'(" Warning:  additional run cycles may be needed.   GPA:",a8,a8,a20)') &
      PASSFAIL(PFGPA),trim(REASONMOD)//":",trim(REASONCHR)
        else
          write (6,'(" Warning:  additional run cycles may be needed.   GPA:",a8,a8,a8)') &
      PASSFAIL(PFGPA),trim(REASONMOD)//":",PASSFAIL(REASONYES)
        endif
      endif
      stop

      CONTAINS
!==============================================================================
      SUBROUTINE INITIALIZE_FILER
      IMPLICIT NONE

      INTEGER FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
      CHARACTER*100 FNAMEI,FNAMEO

! GET THE DATA FROM THE DATA BASE.  FIRST:  READ DICTIONARY (FRTYPE=3)

      FRTYPE=3
      FSOURC=1
      FUNITI=30
      FUNITO=40
      FNAMEI=DICTNAME
      FNAMEO=' '
      FRETCD=0
      FUNFMT=0
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

      RETURN
      END SUBROUTINE INITIALIZE_FILER
!==============================================================================
      SUBROUTINE PROCESS_RESTART
      IMPLICIT NONE

      INTEGER FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
      CHARACTER*100 FNAMEI,FNAMEO

! THEN:  READ DATA (FRTYPE=1) LOOPING OVER REQUESTED SCENARIOS

      FRTYPE=2
      FSOURC=1
      FUNITI=30+IRESTART
      FUNITO=40+IRESTART
      FNAMEI=RESTART(IRESTART)
      FNAMEO=' '
      FUNFMT=FMT(IRESTART)
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      CALL COPY_TO_LOCAL
      close(funiti)
      close(funito)

      RETURN
      END SUBROUTINE PROCESS_RESTART
!==============================================================================
      SUBROUTINE COPY_TO_LOCAL
      IMPLICIT NONE

! Basic PQ variables

      P(:,:,:,IRESTART) = MPRC(:,:,:)
      Q(:,:,:,IRESTART) = MQTY(:,:,:)

! Utility <---> Gas variables, two seasons

      GP(:,:,1:3,IRESTART) = MUPRC(:,:,1:3)
      GQ(:,:,1:3,IRESTART) = MUQTY(:,:,1:3)
      GP(:,:,4:7,IRESTART) = SMUPRC(:,:,1:4)
      GQ(:,:,4:7,IRESTART) = SMUQTY(:,:,1:4)
      GP(:,:,8:10,IRESTART) = SPNGELGR(:,:,1:3)
      GQ(:,:,8:10,IRESTART) = SQNGELGR(:,:,1:3)

! Utility <---> Coal variables

      CP(:,:,:,IRESTART) = PCLELCDR(:,:,:)
      CQ(:,:,:,IRESTART) = QCLCLNR(:,:,:)

! Other stuff

      OTHER(1,:,IRESTART) = MC_GDPR(:)
      OTHER(2,:,IRESTART) = IT_WOP(:,1)
      OTHER(3,:,IRESTART) = RFQICRD(MNUMPR,:)
      OTHER(4,:,IRESTART) = RFPQIPRDT(MNUMPR,:,2)
      OTHER(5,:,IRESTART) = EMETAX(1,:)
      OTHER(6,:,IRESTART) = EMELPSO2(:,1)
      OTHER(7,:,IRESTART) = ECP_PHG(1,:)
      OTHER(8,:,IRESTART) = OGWPRNG(MNUMOR,:)
      OTHER(9,:,IRESTART) = OGCNPPRD(1,:)
      OTHER(10,:,IRESTART) = OGCNPPRD(2,:)
      OTHER(21:29,:,IRESTART) = PBMET(1:9,:,1)
      OTHER(30,:,IRESTART) = PBMET(MNUMCR,:,1)
      OTHER(31:39,:,IRESTART) = QBMET(1:9,:,1)
      OTHER(40,:,IRESTART) = QBMET(MNUMCR,:,1)
      OTHER(43,:,IRESTART) = GLBCRDDMD(:)
      OTHER(44:54,:,IRESTART) = ECP_PSO2(0:10,:,1)
      OTHER(55:65,:,IRESTART) = ECP_PSO2(0:10,:,2)

! XCL_PCAP

      PCAP(:,:,IRESTART) = XCL_PCAP(:,:)

! EMM_CL_CF

      CL_CF(:,:,IRESTART) = EMM_CL_CF(:,:)

! EMM_CL_BTUs

      CL_BTUs(:,:,IRESTART) = EMM_CL_BTUs(:,:)

      LAST_YEAR=MIN(LAST_YEAR,LASTYR)

      RETURN
      END SUBROUTINE COPY_TO_LOCAL
!==============================================================================
      SUBROUTINE READ_CONVERGE
      IMPLICIT NONE

      CHARACTER CHECKQ*6,CHECKP*6,CHECKUQ*9,CHECKUP*9,CHECKO*9
      REAL FRCTOL, ABSTOL
      REAL MNCNVF,MNCNVA,RLXX(20),REDUCE
      CHARACTER*165 CONV_LINE
      CHARACTER*1 CONV_SET
      CHARACTER*9 CHECK
      INTEGER WHATTODO,OPEN_STATUS
      INTEGER IV,II
      LOGICAL KEEPGOING/.TRUE./
      LOGICAL MATCH
      DATA FRCTOL/0.05/
      DATA ABSTOL/20.0/

!  Initialize convergence arrays to FRCTOL and ABSTOL.

      DO IV=1,MNUMQ
        MNQCNV(1,IV) = FRCTOL
        MNQCNV(2,IV) = ABSTOL
        RLXQ(IV) = RLXPC
      ENDDO
      DO IV=1,MNUMP
        MNPCNV(1,IV) = FRCTOL
        MNPCNV(2,IV) = ABSTOL
        RLXP(IV) = RLXPC
      ENDDO
      DO IV=1,NUM_GAS
        MNGQCNV(1,IV) = FRCTOL
        MNGQCNV(2,IV) = ABSTOL
        RLXGQ(IV) = RLXPC
        MNGPCNV(1,IV) = FRCTOL
        MNGPCNV(2,IV) = ABSTOL
        RLXGP(IV) = RLXPC
      ENDDO
      DO IV=1,NCLUT1
        MNCQCNVS(1,IV) = FRCTOL
        MNCQCNVS(2,IV) = ABSTOL
        RLXCQ(IV) = RLXPC
      ENDDO
      MNCPCNVS(1) = FRCTOL
      MNCPCNVS(2) = ABSTOL
      RLXCP = RLXPC
      DO IV=1,MNOTH
        MNOCNV(1,IV) = FRCTOL
        MNOCNV(2,IV) = ABSTOL
        RLXOTHER(IV) = RLXPC
      ENDDO
      DO IV = 1 , 3
         MNZCNV(1,IV) = FRCTOL
         MNZCNV(2,IV) = ABSTOL
         RLXPCAP(IV) = RLXPC
      END DO

!   Open file containing convergence tolerances.

      OPEN(61,FILE=CONVERGE_FILE,status='OLD',IOSTAT=OPEN_STATUS)
      IF (OPEN_STATUS .NE. 0) THEN
        WRITE(6,*) ' Unable to open convergence file MNCNVRG.  Using global relaxation values.'
        RETURN
      ENDIF

!  Read in the convergence values - percent and absolute, relaxation %s,
!  and the variable name to check against M?VARS name array.  If there's
!  a non-match, don't transfer to the MN?CNV arrays.

      DO WHILE (KEEPGOING)
        READ (61,'(A165)',END=999) CONV_LINE
        IF (CONV_LINE(1:1) .EQ. '!') CYCLE
        IF (CONV_LINE(1:1) .EQ. 'Q' .OR. CONV_LINE(1:1) .EQ. 'P' .OR. &
            CONV_LINE(1:1) .EQ. 'G' .OR. CONV_LINE(1:1) .EQ. 'S' .OR. &
            CONV_LINE(1:1) .EQ. 'C' .OR. &
            CONV_LINE(1:1) .EQ. 'O' .OR. CONV_LINE(1:1) .EQ. 'Z') THEN
                CONV_SET=CONV_LINE(1:1)
                CYCLE
        ENDIF
        READ (CONV_LINE,'(I2)') WHATTODO
        IF (WHATTODO .EQ. 99) THEN
          READ (CONV_LINE,*) WHATTODO,MNCNVF,MNCNVA,RLXX(1),REDUCE,CHECK
        ELSE
          READ (CONV_LINE,*) II,MNCNVF,MNCNVA,(RLXX(IV),IV=1,WHATTODO),CHECK
          RLXX(1)=RLXX(WHATTODO)     !  use last iteration value (usually least relaxation)
        ENDIF
        MATCH = .FALSE.
        IF (CONV_SET .EQ. 'Q') THEN
          DO IV=1,MNUMQ
            IF (TRIM(CHECK) .EQ. TRIM(MQVARSL(IV))) THEN
              MNQCNV(1,IV) = MNCNVF
              MNQCNV(2,IV) = MNCNVA
              RLXQ(IV) = RLXX(1)
              MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'P') THEN
          DO IV=1,MNUMP
            IF (TRIM(CHECK) .EQ. TRIM(MPVARSL(IV))) THEN
              MNPCNV(1,IV) = MNCNVF
              MNPCNV(2,IV) = MNCNVA
              RLXP(IV) = RLXX(1)
              MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'G') THEN
          DO IV=1,3
            IF (TRIM(CHECK) .EQ. TRIM(MGQVARSL(IV))) THEN
              MNGQCNV(1,IV) = MNCNVF
              MNGQCNV(2,IV) = MNCNVA
              RLXGQ(IV) = RLXX(1)
! use GF (1) and GI (2) for GFS (4,5) and GIS (6,7)
              IF (IV .LT. 3) THEN
                MNGQCNV(1,2*IV+2) = MNCNVF
                MNGQCNV(2,2*IV+3) = MNCNVA
                RLXGQ(2*IV+2) = RLXX(1)
                RLXGQ(2*IV+3) = RLXX(1)
              ENDIF
              MATCH = .TRUE.
            ENDIF
            IF (TRIM(CHECK) .EQ. TRIM(MGPVARSL(IV))) THEN
              MNGPCNV(1,IV) = MNCNVF
              MNGPCNV(2,IV) = MNCNVA
              RLXGP(IV) = RLXX(1)
! use GF (1) and GI (2) for GFS (4,5) and GIS (6,7)
              IF (IV .LT. 3) THEN
                MNGPCNV(1,2*IV+2) = MNCNVF
                MNGPCNV(2,2*IV+3) = MNCNVA
                RLXGP(2*IV+2) = RLXX(1)
                RLXGP(2*IV+3) = RLXX(1)
              ENDIF
              MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'S') THEN                    ! seasonal natural gas, each season separate
          DO IV=1,3
            IF (TRIM(CHECK) .EQ. TRIM(MGQVARSL(IV+7))) THEN
               MNGQCNV(1,IV+7) = MNCNVF
               MNGQCNV(2,IV+7) = MNCNVA
               RLXGQ(IV+7) = RLXX(1)
               MATCH = .TRUE.
            ENDIF
            IF (TRIM(CHECK) .EQ. TRIM(MGPVARSL(IV+7))) THEN
               MNGPCNV(1,IV+7) = MNCNVF
               MNGPCNV(2,IV+7) = MNCNVA
               RLXGP(IV+7) = RLXX(1)
               MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'C') THEN
          DO IV=1,NCLUT1
            IF (TRIM(CHECK) .EQ. TRIM(MCQVARSLS(IV))) THEN
              MNCQCNVS(1,IV) = MNCNVF
              MNCQCNVS(2,IV) = MNCNVA
              RLXCQ(IV) = RLXX(1)
              MATCH = .TRUE.
            ENDIF
          ENDDO
          DO IV=1,2
            IF (TRIM(CHECK) .EQ. TRIM(MCPVARSLS(IV))) THEN
              MNCPCNVS(1) = MNCNVF
              MNCPCNVS(2) = MNCNVA
              RLXCP = RLXX(1)
              MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'O') THEN
          DO IV=1,MNOTH    
            IF (TRIM(CHECK) .EQ. TRIM(MOVARSL(IV))) THEN
              MNOCNV(1,IV) = MNCNVF
              MNOCNV(2,IV) = MNCNVA
              RLXOTHER(IV) = RLXX(1)
              MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'Z') THEN
          IF (TRIM(CHECK) .EQ. TRIM(MZVARSL(1))) THEN
            DO IV=1,MX_NCOALS
              MNZCNV(1,1) = MNCNVF
              MNZCNV(2,1) = MNCNVA
              RLXPCAP(1) = RLXX(1)
              MATCH = .TRUE.
            ENDDO
          ENDIF
          IF (TRIM(CHECK) .EQ. TRIM(MZVARSL(2))) THEN
            DO IV=1,MX_UNITS
              MNZCNV(1,2) = MNCNVF
              MNZCNV(2,2) = MNCNVA
              RLXPCAP(2) = RLXX(1)
              MATCH = .TRUE.
            ENDDO
          ENDIF
          IF (TRIM(CHECK) .EQ. TRIM(MZVARSL(3))) THEN
            DO IV=1,MX_UNITS
              MNZCNV(1,3) = MNCNVF
              MNZCNV(2,3) = MNCNVA
              RLXPCAP(3) = RLXX(1)
              MATCH = .TRUE.
            ENDDO
          ENDIF
        ELSE
          WRITE(6,*) ' Unknown convergence set type = ',CONV_SET
        ENDIF
        IF (MATCH .EQ. .FALSE.) WRITE(6,*) '  No match found for convergence variable ',CHECK
        CYCLE
999     KEEPGOING=.FALSE.
      ENDDO

!   Close the convergence values file.
      CLOSE(61)

      RETURN
      END SUBROUTINE READ_CONVERGE
!==============================================================================
      SUBROUTINE TEST_CONVERGE
      IMPLICIT NONE

      REAL FRCCHG                      ! Holds fractional change
      REAL ABSCHG                      ! Holds absolute change
      INTEGER IR                       ! Subscript index for REGION
      INTEGER IV                       ! Subscript index for VARIABLE
      INTEGER IQ                       ! Subscript index for Q VARIABLE
      INTEGER IY                       ! Subscript index for YEAR (IY=CURIYR)
      CHARACTER*9 VARNAM
      INTEGER NUMQAS,NUMQ_AS,NUMPAS,NUMP_AS,PQMATCH
      EXTERNAL NUMQ_AS,NUMP_AS,PQMATCH
      INTEGER MNDBGCV
      LOGICAL DONEYET/.FALSE./

      TOTAL_NOT_CONVERGED=0

      DO IY=FIRST_YEAR,LAST_YEAR
         DO IR=1,MNUMCR-2
            DO IV = 1,MNUMP
               NUMPAS=NUMP_AS(IV,MNUMP)
               IF(NUMPAS.EQ.0) THEN
                  CALL MNCHKCV(P(IR,IY,IV,1),P(IR,IY,IV,2),FRCCHG,ABSCHG)
                  IQ=PQMATCH(IV,MNUMQ,MNUMP)     ! Get matching QUANTITY product
                  IF(FRCCHG.GE.MNPCNV(1,IV).AND.ABSCHG.GE.MNPCNV(2,IV) .AND.   & ! CNV TEST
                     (Q(IR,IY,IQ,1).GE.MNQCNV(2,IQ)                 & ! CNV TEST
                     .OR. Q(IR,IY,IQ,2).GE.MNQCNV(2,IQ))) THEN       ! CNV TEST
                     VARNAM=MPVARSL(IV)//'   '
                     CALL MNPRTCV(IY,IR, &
                     P(IR,IY,IV,1),P(IR,IY,IV,2),FRCCHG,MNPCNV(1,IV), &
                       VARNAM,'&&&P')
                  ENDIF
               ENDIF
            ENDDO
            DO IV = 1,MNUMQ
               NUMQAS=NUMQ_AS(IV,MNUMQ)
               IF(NUMQAS.EQ.0) THEN
                  CALL MNCHKCV(Q(IR,IY,IV,1),Q(IR,IY,IV,2),FRCCHG,ABSCHG)
                  IF(FRCCHG.GE.MNQCNV(1,IV).AND.ABSCHG.GE.MNQCNV(2,IV))THEN ! CNV TEST
                     VARNAM=MQVARSL(IV)//'   '
                     CALL MNPRTCV(IY,IR, &
                        Q(IR,IY,IV,1),Q(IR,IY,IV,2),FRCCHG,MNQCNV(1,IV), &
                        VARNAM,'&&&Q')
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
  
         DO IR=1,NNGEM
            DO IV = 1,10
               CALL MNCHKCV(GP(IR,IY,IV,1),GP(IR,IY,IV,2),FRCCHG,ABSCHG)
               IF (FRCCHG.GE.MNGPCNV(1,IV).AND.ABSCHG.GE.MNGPCNV(2,IV)   & ! CNV TEST
                                       .AND.                          & ! CNV TEST
                  (GQ(IR,IY,IV,1).GE.MNGQCNV(2,IV)                      & ! CNV TEST
                  .OR.GQ(IR,IY,IV,2).GE.MNGQCNV(2,IV))       )THEN        ! CNV TEST
                  VARNAM=MGPVARSL(IV)
                  CALL MNPRTCV(IY,IR, &
                     GP(IR,IY,IV,1),GP(IR,IY,IV,2),FRCCHG,MNGPCNV(1,IV), &
                     VARNAM,'&&&P')
               ENDIF
            ENDDO

            DO IV = 1,10
               CALL MNCHKCV(GQ(IR,IY,IV,1),GQ(IR,IY,IV,2),FRCCHG,ABSCHG)
               IF (FRCCHG .GE. MNGQCNV(1,IV) .AND. ABSCHG .GE. MNGQCNV(2,IV)) THEN ! CNV TEST
                   VARNAM=MGQVARSL(IV)
                   CALL MNPRTCV(IY,IR, &
                   GQ(IR,IY,IV,1),GQ(IR,IY,IV,2),FRCCHG,MNGQCNV(1,IV), &
                   VARNAM,'&&&Q')
               ENDIF
            ENDDO
         ENDDO

!        Added check for new coal/utility prices and quantities
         DO IR=1,NDRGG
            DO IV = 1,2
               CALL MNCHKCV(CP(IV,IR,IY,1),CP(IV,IR,IY,2),FRCCHG,ABSCHG)
               IF (FRCCHG .GE. MNCPCNVS(1) .AND. ABSCHG .GE. MNCPCNVS(2)) THEN !  & ! CNV TEST
!   No corresponding Q variable for PCLELCDR!
!                                      .AND.                             & ! CNV TEST
!                  (CQ(IR,IY,IV,1) .GE. MNCQCNVS(2,IV)                      & ! CNV TEST
!                  .OR.CQ(IR,IY,IV,2) .GE. MNCQCNVS(2,IV))) THEN  ! CNV TEST
                   VARNAM=MCPVARSLS(IV)
                   CALL MNPRTCV(IY,IR, &
                      CP(IV,IR,IY,1),CP(IV,IR,IY,2),FRCCHG,MNCPCNVS(1), &
                      VARNAM,'&&&P')
               ENDIF
            ENDDO
            DO IV = 1,NCLUT1
               CALL MNCHKCV(CQ(IR,IY,IV,1),CQ(IR,IY,IV,2),FRCCHG,ABSCHG)
               IF (FRCCHG .GE. MNCQCNVS(1,IV) .AND. ABSCHG .GE. MNCQCNVS(2,IV)) THEN  ! CNV TEST
                   VARNAM=MCQVARSLS(IV)//'  '
                   CALL MNPRTCV(IY,IR, &
                      CQ(IR,IY,IV,1),CQ(IR,IY,IV,2),FRCCHG,MNCQCNVS(1,IV), &
                      VARNAM,'&&&Q')
               ENDIF
            ENDDO
         ENDDO

         IR=11
         DO IV = 1,MNOTH
            CALL MNCHKCV(OTHER(IV,IY,1),OTHER(IV,IY,2),FRCCHG,ABSCHG)
            IF (FRCCHG .GE. MNOCNV(1,IV) .AND. ABSCHG .GE. MNOCNV(2,IV)) THEN   ! CNV TEST
                VARNAM=MOVARSL(IV)
                CALL MNPRTCV(IY,IR, &
                OTHER(IV,IY,1),OTHER(IV,IY,2),FRCCHG,MNOCNV(1,IV), &
                VARNAM,'&&&O')
            ENDIF
         ENDDO

         IR=11
         DO IV = 1,MX_NCOALS
            CALL MNCHKCV(PCAP(IV,IY,1),PCAP(IV,IY,2),FRCCHG,ABSCHG)
            IF (FRCCHG .GE. MNZCNV(1,1) .AND. ABSCHG .GE. MNZCNV(2,1)) THEN   ! CNV TEST
                VARNAM=MZVARSL(1)
                CALL MNPRTCV(IY,IR,PCAP(IV,IY,1),PCAP(IV,IY,2),FRCCHG,MNZCNV(1,1),VARNAM,'&&&Z')
            ENDIF
         ENDDO
         DO IV = 1,MX_UNITS
            CALL MNCHKCV(CL_CF(IV,IY,1),CL_CF(IV,IY,2),FRCCHG,ABSCHG)
            IF (FRCCHG .GE. MNZCNV(1,2) .AND. ABSCHG .GE. MNZCNV(2,2)) THEN   ! CNV TEST
                VARNAM=MZVARSL(2)
                CALL MNPRTCV(IY,IR,CL_CF(IV,IY,1),CL_CF(IV,IY,2),FRCCHG,MNZCNV(1,2),VARNAM,'&&&Z')
            ENDIF
            CALL MNCHKCV(CL_BTUs(IV,IY,1),CL_BTUs(IV,IY,2),FRCCHG,ABSCHG)
            IF (FRCCHG .GE. MNZCNV(1,3) .AND. ABSCHG .GE. MNZCNV(2,3)) THEN   ! CNV TEST
                VARNAM=MZVARSL(3)
                CALL MNPRTCV(IY,IR,CL_BTUs(IV,IY,1),CL_BTUs(IV,IY,2),FRCCHG,MNZCNV(1,3),VARNAM,'&&&Z')
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END SUBROUTINE TEST_CONVERGE
!==============================================================================
      SUBROUTINE MNCHKCV(CUR,OLD,FRCCHG,ABSCHG)
      IMPLICIT NONE

      REAL CUR,OLD,FRCCHG,ABSCHG,DENOM

      FRCCHG=0.0
      ABSCHG=ABS(CUR-OLD)
      DENOM=(CUR+OLD)/2.
      IF (DENOM.NE.0.0) FRCCHG = ABSCHG/DENOM

      RETURN
      END SUBROUTINE MNCHKCV
!==============================================================================
      SUBROUTINE MNPRTCV(IY,IR,CUR,OLD,FRCCHG,LIMIT,VARNAM,TYPSTR)
      IMPLICIT NONE
!
! PRINTS MESSAGE FOR VARIABLES NOT MEETING CONVERGENCE CRITERIA.  THE
! MESSAGE DISPLAYS THE SUBROUTINE, VARIABLE NAME, CURRENT AND PRIOR VALUES,
! PERCENTAGE CHANGE, TOLERANCE,
! ITERATION COUNT, REGION, YEAR.  THE ROUTINE ALSO CALLS ROUTINE TO
! PUT THE ENTRY INTO THE SORTED SUMMARY LIST OF VARIABLES NOT CONVERGED.
!
! CALLED BY CVTEST
      INTEGER IY      ! NEMS YEAR INDEX
      INTEGER IR      ! REGION INDEX
      REAL    CUR     ! CURRENT VALUE
      REAL    OLD     ! PREVIOUS VALUE
      REAL    FRCCHG  ! FRACTIONAL RELATIVE CHANGE
      REAL    LIMIT   ! TOLERANCE LIMIT ON FRACTIONAL RELATIVE CHANGE
      CHARACTER*9 VARNAM  ! VARIABLE NAME
      CHARACTER*4 TYPSTR  ! SEARCH STRING ('&&&Q', '&&&P' OR '&&&O')

      IF(TYPSTR .NE. '&&&Z') WRITE(*,'(A,i4,I3,2F10.3,2F7.1)') ' ' // TYPSTR // ': ' // VARNAM, &
         IY+1989,IR,CUR,OLD,FRCCHG*100.,LIMIT*100.

      CALL SORTRPT(FRCCHG,IR,OLD,CUR,VARNAM)

      RETURN
      END SUBROUTINE MNPRTCV
!==============================================================================
      SUBROUTINE RELAX

      IMPLICIT NONE

      INTEGER IY,IR,IV,IX   ! SUBSCRIPTS FOR Year, REGION, Variable, x

! +++ Reset PRICES and QUANTITIES to points between previous and current iteration.
      DO IY=FIRST_YEAR,LAST_YEAR
         DO IR=1,MNUMCR-1
            DO IV = 1,MNUMP
               MPRC(IR,IY,IV)=P(IR,IY,IV,2)+ &
                   RLXP(IV)*(P(IR,IY,IV,1)-P(IR,IY,IV,2))
            ENDDO
            DO IV = 1,MNUMQ
               MQTY(IR,IY,IV)=Q(IR,IY,IV,2)+ &
                   RLXQ(IV)*(Q(IR,IY,IV,1)-Q(IR,IY,IV,2))
            ENDDO
         ENDDO
         DO IR=1,NNGEM
            DO IV = 1,3
               MUPRC(IR,IY,IV)=GP(IR,IY,IV,2)+ &
                   RLXGP(IV)*(GP(IR,IY,IV,1)-GP(IR,IY,IV,2))
               MUQTY(IR,IY,IV)=GQ(IR,IY,IV,2)+ &
                   RLXGQ(IV)*(GQ(IR,IY,IV,1)-GQ(IR,IY,IV,2))
               SPNGELGR(IR,IY,IV)=GP(IR,IY,IV+7,2)+ &
                   RLXGP(IV+7)*(GP(IR,IY,IV+7,1)-GP(IR,IY,IV+7,2))
               SQNGELGR(IR,IY,IV)=GQ(IR,IY,IV+7,2)+ &
                   RLXGQ(IV+7)*(GQ(IR,IY,IV+7,1)-GQ(IR,IY,IV+7,2))
            ENDDO
            DO IV = 1,4
               IF (IV.LE.2) THEN
                 IX = 1
               ELSE
                 IX = 2
               ENDIF
               SMUPRC(IR,IY,IV)=GP(IR,IY,IV+3,2)+ &
                   RLXGP(IX)*(GP(IR,IY,IV+3,1)-GP(IR,IY,IV+3,2))
               SMUQTY(IR,IY,IV)=GQ(IR,IY,IV+3,2)+ &
                   RLXGQ(IX)*(GQ(IR,IY,IV+3,1)-GQ(IR,IY,IV+3,2))
            ENDDO
         ENDDO

!        New coal regions
         DO IR=1,NDRGG
            PCLELCDR(1,IR,IY)=CP(1,IR,IY,2)+RLXCP*(CP(1,IR,IY,1)-CP(1,IR,IY,2))
            PCLELCDR(2,IR,IY)=CP(2,IR,IY,2)+RLXCP*(CP(2,IR,IY,1)-CP(2,IR,IY,2))
         ENDDO
         DO IR=1,NDRGG
            DO IV = 1,NCLUT1
               QCLCLNR(IR,IY,IV)=CQ(IR,IY,IV,2)+ &
                   RLXCQ(IV)*(CQ(IR,IY,IV,1)-CQ(IR,IY,IV,2))
            ENDDO
         ENDDO
         DO IV = 1,MNOTH
            OTHER(IV,IY,1)=OTHER(IV,IY,2)+ &
               RLXOTHER(IV)*(OTHER(IV,IY,1)-OTHER(IV,IY,2))
         ENDDO

         DO IV = 1,MX_NCOALS
            XCL_PCAP(IV,IY)=PCAP(IV,IY,2)+ &
               RLXPCAP(1)*(PCAP(IV,IY,1)-PCAP(IV,IY,2))
         ENDDO

         DO IV = 1,MX_UNITS
            EMM_CL_CF(IV,IY)=CL_CF(IV,IY,2)+ &
               RLXPCAP(2)*(CL_CF(IV,IY,1)-CL_CF(IV,IY,2))
            EMM_CL_BTUs(IV,IY)=CL_BTUs(IV,IY,2)+ &
               RLXPCAP(3)*(CL_BTUs(IV,IY,1)-CL_BTUs(IV,IY,2))
         ENDDO

      ENDDO

!     Need to copy back the relaxed other values

      DO IY=FIRST_YEAR,LAST_YEAR
        MC_GDPR(IY)       = OTHER(1,IY,1)
        IT_WOP(IY,1)      = OTHER(2,IY,1)
        RFQICRD(MNUMPR,IY)     = OTHER(3,IY,1)
        RFPQIPRDT(MNUMPR,IY,2) = OTHER(4,IY,1)
! for now, no relaxation of carbon tax
!       EMETAX(1,IY)      = OTHER(5,IY,1)
        EMELPSO2(IY,1)    = OTHER(6,IY,1)
        ECP_PHG(1,IY)     = OTHER(7,IY,1)
!       OGWPRNG(MNUMOR,IY)= OTHER(8,IY,1)    !  don't relax this as it is just the national average
        OGCNPPRD(1,IY)    = OTHER(9,IY,1)
        OGCNPPRD(2,IY)    = OTHER(10,IY,1)
        PBMET(1:9,IY,1)   = OTHER(21:29,IY,1)
        PBMET(MNUMCR,IY,1)= OTHER(30,IY,1)
        QBMET(1:9,IY,1)   = OTHER(31:39,IY,1)
        QBMET(MNUMCR,IY,1)= OTHER(40,IY,1)
        GLBCRDDMD(IY)     = OTHER(43,IY,1)
        ECP_PSO2(0:10,IY,1) = OTHER(44:54,IY,1)
        ECP_PSO2(0:10,IY,2) = OTHER(55:65,IY,1)

! Recalculate national totals for PBMET and QBMET to ensure they add up
      QBMET(MNUMCR,IY,1) = sum(QBMET(1:9,IY,1))
      PBMET(MNUMCR,IY,1) = 0.0
      DO IR=1,9
         IF (QBMET(MNUMCR,IY,1) .NE. 0.0) &
         PBMET(MNUMCR,IY,1) = PBMET(MNUMCR,IY,1) + QBMET(IR,IY,1) * PBMET(IR,IY,1) / QBMET(MNUMCR,IY,1)
      ENDDO
      ENDDO
      RETURN
      END SUBROUTINE RELAX
!==============================================================================
    SUBROUTINE SUM_CONVERGE(CLevel)
      IMPLICIT NONE
! This routine summarizes the convergence for a table in ftab.  The
! Variables selected for the table are indicated in CVTAB_MATCH (initialized below).
      integer CLevel ! 1 for US, 2 for Regional
      
!================================      
      character*10 debug_flag   ! command line argument holding debug print flag.
      common/blk_dbg/debug_flag
!=================================      
      
      REAL FRCCHG                      ! Holds fractional change
      REAL ABSCHG                      ! Holds absolute change
      INTEGER IR                       ! Subscript index for REGION
      INTEGER IV                       ! Subscript index for VARIABLE
      INTEGER IQ                       ! Subscript index for Q VARIABLE
      INTEGER IY                       ! Subscript index for YEAR (IY=CURIYR)
      CHARACTER*9 VARNAM
      INTEGER NUMQAS,NUMQ_AS,NUMPAS,NUMP_AS,PQMATCH
      EXTERNAL NUMQ_AS,NUMP_AS,PQMATCH
      real*4 sorted_score(mnumyr) ! ave_score defined globally above


! The following is in the converge include file:
   
!     REAL*4 CVTAB(MNUMCVTST,4,MNUMYR) ! Convergence Summary Table:  (number of items, {aft/bef/abs/pct} , year)
!     real*4 CVSCORE(MNUMYR) ! Convergence GPA score
!     real*4 CVSCORE_US(MNUMYR) ! Convergence GPA score, US Aggregates Only
!     real*4 CVSCOREHIST(MNUMYR,10) ! history of prior scores 
!     integer oscor ! number of oscillations
! CVTab_Match hold variable names of items to be summed and included in the summary report

      character*27 CVTab_Match(MNUMCVTST)/ &
       ' QELRS QELCM QELIN QELTR', & !   1 Electricity
       ' QNGRS QNGCM QNGIN QNGTR', & !   2 Natural Gas           End Use Sectors
       ' QLGRS QLGCM QLGTR QLGIN', & !   3 Liquid Petroleum Gas  End Use Sectors
       ' QDSRS QDSCM QDSTR QDSIN', & !   4 Distillate Fuel       End Use Sectors  
       ' QRSCM QRSTR QRSIN',       & !   5 Residual Fuel         End Use Sectors
       ' QMGCM QMGTR QMGIN',       & !   6 Motor Gasoline        End Use Sectors 
       ' QJFTR',                   & !   7 Jet Fuel              End Use Sectors  
       ' QCLRS QCLCM QCLIN',       & !   8 Steam Coal            End Use Sectors 
       ' QTSRS QTSCM QTSTR QTSIN', & !   9 Total Sector Fuel     End Use Sectors 
       ' QNGEL',                   & !  10 Natural Gas           Electricity
       ' QDSEL',                   & !  11 Distillate            Electricity
       ' QRSEL',                   & !  12 Residual Fuel         Electricity
       ' QCLEL',                   & !  13 Steam Coal      
       'QCLB1NR',                  & !  14 Coal in ECP Type B1
       'QCLB2NR',                  & !  15 Coal in ECP Type B2
       'QCLB3NR',                  & !  16 Coal in ECP Type B3
       'QCLB4NR',                  & !  17 Coal in ECP Type B4
       'QCLB5NR',                  & !  18 Coal in ECP Type B5
       'QCLB6NR',                  & !  19 Coal in ECP Type B6
       'QCLB7NR',                  & !  20 Coal in ECP Type B7
       'QCLB8NR',                  & !  21 Coal in ECP Type B8
       'QCLC1NR',                  & !  22 Coal in ECP Type C1
       'QCLC2NR',                  & !  23 Coal in ECP Type C2
       'QCLC3NR',                  & !  24 Coal in ECP Type C3
       'QCLC4NR',                  & !  25 Coal in ECP Type C4
       'QCLC5NR',                  & !  26 Coal in ECP Type C5
       'QCLC6NR',                  & !  27 Coal in ECP Type C6
       'QCLC7NR',                  & !  28 Coal in ECP Type C7
       'QCLC8NR',                  & !  29 Coal in ECP Type C8
       'QCLC9NR',                  & !  30 Coal in ECP Type C9
       'QCLCXNR',                  & !  31 Coal in ECP Type CY
       'QCLCYNR',                  & !  32 Coal in ECP Type CX
       'QCLCZNR',                  & !  33 Coal in ECP Type CZ
       'QCLH1NR',                  & !  34 Coal in ECP Type H1
       'QCLH2NR',                  & !  35 Coal in ECP Type H2
       'QCLH3NR',                  & !  36 Coal in ECP Type H3
       'QCLH4NR',                  & !  37 Coal in ECP Type H4
       'QCLH5NR',                  & !  38 Coal in ECP Type H5
       'QCLH6NR',                  & !  39 Coal in ECP Type H6
       'QCLH7NR',                  & !  40 Coal in ECP Type H7
       'QCLH8NR',                  & !  41 Coal in ECP Type H8
       'QCLH9NR',                  & !  42 Coal in ECP Type H9
       'QCLHANR',                  & !  43 Coal in ECP Type HA
       'QCLHBNR',                  & !  44 Coal in ECP Type HB
       'QCLHCNR',                  & !  45 Coal in ECP Type HC
       'QCLPCNR',                  & !  46 Coal in ECP Type PC
       'QCLIGNR',                  & !  47 Coal in ECP Type IG
       'QCLISNR',                  & !  48 Coal in ECP Type IS
       ' QUREL' ,                  & !  49 Uranium
       ' QTSEL' ,                  & !  50 Total Electricity Sector
       ' QTSAS' ,                  & !  51 Total Primary Fuel, all Sectors
       ' PELRS PELCM PELIN PELTR', & !  52 Electricity
       ' PNGRS PNGCM PNGIN PNGTR', & !  53 Natural Gas           End Use Sectors
       ' PLGRS PLGCM PLGTR PLGIN', & !  54 Liquid Petroleum Gas  End Use Sectors
       ' PDSRS PDSCM PDSTR PDSIN', & !  55 Distillate Fuel       End Use Sectors  
       ' PRSCM PRSTR PRSIN',       & !  56 Residual Fuel         End Use Sectors
       ' PMGCM PMGTR PMGIN',       & !  57 Motor Gasoline        End Use Sectors 
       ' PJFTR',                   & !  58 Jet Fuel              End Use Sectors  
       ' PCLRS PCLCM PCLIN',       & !  59 Steam Coal            End Use Sectors 
       ' PNGEL',                   & !  60 Natural Gas           Electricity
       ' PDSEL',                   & !  61 Distillate            Electricity
       ' PRSEL',                   & !  62 Residual Fuel         Electricity
       ' PCLEL',                   & !  63 Steam Coal      
       'PCLELCDR',                 & !  64 Coal in ECP Type B1
       'PCLB2NR',                  & !  65 Coal in ECP Type B2
       'PCLB3NR',                  & !  66 Coal in ECP Type B3
       'PCLB4NR',                  & !  67 Coal in ECP Type B4
       'PCLB5NR',                  & !  68 Coal in ECP Type B5
       'PCLB6NR',                  & !  69 Coal in ECP Type B6
       'PCLB7NR',                  & !  70 Coal in ECP Type B7
       'PCLB8NR',                  & !  71 Coal in ECP Type B8
       'PCLC1NR',                  & !  72 Coal in ECP Type C1
       'PCLC2NR',                  & !  73 Coal in ECP Type C2
       'PCLC3NR',                  & !  74 Coal in ECP Type C3
       'PCLC4NR',                  & !  75 Coal in ECP Type C4
       'PCLC5NR',                  & !  76 Coal in ECP Type C5
       'PCLC6NR',                  & !  77 Coal in ECP Type C6
       'PCLC7NR',                  & !  78 Coal in ECP Type C7
       'PCLC8NR',                  & !  79 Coal in ECP Type C8
       'PCLC9NR',                  & !  80 Coal in ECP Type C9
       'PCLCXNR',                  & !  81 Coal in ECP Type CY
       'PCLCYNR',                  & !  82 Coal in ECP Type CX
       'PCLCZNR',                  & !  83 Coal in ECP Type CZ
       'PCLH1NR',                  & !  84 Coal in ECP Type H1
       'PCLH2NR',                  & !  85 Coal in ECP Type H2
       'PCLH3NR',                  & !  86 Coal in ECP Type H3
       'PCLH4NR',                  & !  87 Coal in ECP Type H4
       'PCLH5NR',                  & !  88 Coal in ECP Type H5
       'PCLH6NR',                  & !  89 Coal in ECP Type H6
       'PCLH7NR',                  & !  90 Coal in ECP Type H7
       'PCLH8NR',                  & !  91 Coal in ECP Type H8
       'PCLH9NR',                  & !  92 Coal in ECP Type H9
       'PCLHANR',                  & !  93 Coal in ECP Type HA
       'PCLHBNR',                  & !  94 Coal in ECP Type HB
       'PCLHCNR',                  & !  95 Coal in ECP Type HC
       'PCLPCNR',                  & !  96 Coal in ECP Type PC
       'PCLIGNR',                  & !  97 Coal in ECP Type IG
       'PCLISNR',                  & !  98 Coal in ECP Type IS
       ' PUREL' ,                  & !  99 Uranium
       'EMETAX',                   & ! 100 CO2 ALLOWANCE
       'EMELPSO2',                 & ! 101 SO2 Allowance
       'ECP_PHG',                  & ! 102 Mercury Allowance
! future?       'OGWPRNG',                  & ! 103 Natural Gas Wellhead Price
       ' ',' ', ' ', ' '/

      character*27 tempa
! Assign a grade score to intercycle converge, ala GPA:  4:A, ..0:F
! Weight:                        7 Weight Classes
! End-Use Quantity:       24.5   1:first 9
! Electricity Quantity    24.5   2: next 4, skip 7, next 2
! End-Use Prices          24.5   3: next 8    
! Electricity Fuel Prices 24.5   4: next 4, skip 7, next 1
!  Subtotal:              98.0
! Emissions Allownaces (CO2 always 0 in this version that uses adjusted energy prices that include variation in CO2 price)
! Carbon Dioxide           0     5: emetax (100)
! SO2 ALlowance            1     6: EMELPSO2 (101)
! Mercury                  1     7: ecp_phg (102)   

       integer weight_cl(MNUMCVTST)/8*1,0,4*2,35*0,2,0,0,8*3,4*4,35*0,4,5,6,7,4*0/
       real*4 weight_start(7)/24.5,24.5,24.5,24.5,0.,1.,1./,weight(7)
       real*4 weight_sum(7),weight_count(7),score
     real*4 frcchg2,abschg2,rev1,rev2,ave
  integer i

! Maximum fractional deviation for scoring purposes
     real*4 maxdev/.15/
!
  integer itab,iclass,ir_start,ir_end
  write(6,*) 'CLevel=',CLevel
! Compile score at US level (CLevel=1) or at the regional level (CLevel=2).
  if(CLevel.eq.1) then
    ir_start=11       ! 11 holds US for census region variables
    ir_end=11
  else
    ir_start=1
    ir_end=MNUMCR-2   ! 9 census regions
  endif
  
    DO IY=1,LAST_YEAR
      CVTAB(:,:,IY)=0.  
      DO IV = 1,MNUMP
        VARNAM=MPVARSL(IV)//'   '
        CALL CVMATCH(VARNAM,CVTab_Match,ITAB)  ! see if variable "iv" is among those being summed
        IF(ITAB.gt.0) THEN
          IQ=PQMATCH(IV,MNUMQ,MNUMP)     ! Get matching QUANTITY product # IQ
          DO IR=ir_start,ir_end

            rev1=P(IR,IY,IV,1)*max(Q(IR,IY,IQ,1),Q(IR,IY,IQ,2))  ! weight price by size of market
            rev2=P(IR,IY,IV,2)*max(Q(IR,IY,IQ,1),Q(IR,IY,IQ,2)) 

            CALL MNCHKCV(rev1,rev2,FRCCHG,ABSCHG)
 
            CVTAB(ITAB,1,IY)=CVTAB(ITAB,1,IY) + rev1
            CVTAB(ITAB,2,IY)=CVTAB(ITAB,2,IY) + rev2
            CVTAB(ITAB,3,IY)=CVTAB(ITAB,3,IY) + ABSCHG

          ENDDO
        ENDIF
      ENDDO
      DO IV = 1,MNUMQ
        VARNAM=MQVARSL(IV)//'   '
        CALL CVMATCH(VARNAM,CVTab_Match,ITAB)  ! see if variable "iv" is among those being summed
        IF(ITAB.gt.0) THEN
          DO IR=ir_start,ir_end
            CALL MNCHKCV(Q(IR,IY,IV,1),Q(IR,IY,IV,2),FRCCHG,ABSCHG)
            CVTAB(ITAB,1,IY)=CVTAB(ITAB,1,IY) + Q(IR,IY,IV,1)
            CVTAB(ITAB,2,IY)=CVTAB(ITAB,2,IY) + Q(IR,IY,IV,2)
            CVTAB(ITAB,3,IY)=CVTAB(ITAB,3,IY) + ABSCHG
          ENDDO
        ENDIF
      ENDDO
! Test other regional variables subject to convergence, although these have no weight in the convergence scoring
      IF(CLEVEL.EQ.2) THEN  
        DO IV = 1,3
          VARNAM=MGPVARSL(IV)
          CALL CVMATCH(VARNAM,CVTab_Match,ITAB)  ! see if variable "iv" is among those being summed
          IF(ITAB.gt.0) THEN
            DO IR=1,NNGEM
              rev1=GP(IR,IY,IV,1)*max(GQ(IR,IY,IV,1),GQ(IR,IY,IV,2))  ! weight price by size of market
              rev2=GP(IR,IY,IV,2)*max(GQ(IR,IY,IV,1),GQ(IR,IY,IV,2)) 

              CALL MNCHKCV(rev1,rev2,FRCCHG,ABSCHG)

              CVTAB(ITAB,1,IY)=CVTAB(ITAB,1,IY) + rev1
              CVTAB(ITAB,2,IY)=CVTAB(ITAB,2,IY) + rev2
              CVTAB(ITAB,3,IY)=CVTAB(ITAB,3,IY) + ABSCHG

            ENDDO
          ENDIF
        ENDDO
        DO IV = 1,3
          VARNAM=MGQVARSL(IV)
          CALL CVMATCH(VARNAM,CVTab_Match,ITAB)  ! see if variable "iv" is among those being summed
          IF(ITAB.gt.0) THEN
            DO IR=1,NNGEM
              CALL MNCHKCV(GQ(IR,IY,IV,1),GQ(IR,IY,IV,2),FRCCHG,ABSCHG)
              CVTAB(ITAB,1,IY)=CVTAB(ITAB,1,IY) + GQ(IR,IY,IV,1)
              CVTAB(ITAB,2,IY)=CVTAB(ITAB,2,IY) + GQ(IR,IY,IV,2)
              CVTAB(ITAB,3,IY)=CVTAB(ITAB,3,IY) + ABSCHG
            ENDDO
          ENDIF
        ENDDO
 
!   Added check for new coal/utility prices and quantities
        DO IV = 1,2
          VARNAM=MCPVARSLS(IV)
          CALL CVMATCH(VARNAM,CVTab_Match,ITAB)  ! see if variable "iv" is among those being summed
          IF(ITAB.gt.0) THEN
            DO IR=1,NDRGG
              rev1=CP(IV,IR,IY,1) !*max(CQ(IR,IY,IV,1),CQ(IR,IY,IV,2))  ! weight price by size of market
              rev2=CP(IV,IR,IY,2) !*max(CQ(IR,IY,IV,1),CQ(IR,IY,IV,2)) 

              CALL MNCHKCV(rev1,rev2,FRCCHG,ABSCHG)

              CVTAB(ITAB,1,IY)=CVTAB(ITAB,1,IY) + rev1
              CVTAB(ITAB,2,IY)=CVTAB(ITAB,2,IY) + rev2
              CVTAB(ITAB,3,IY)=CVTAB(ITAB,3,IY) + ABSCHG

            ENDDO

         ENDIF
        ENDDO
        DO IV = 1,NCLUT1
          VARNAM=MCQVARSLS(IV)//'  '
          CALL CVMATCH(VARNAM,CVTab_Match,ITAB)  ! see if variable "iv" is among those being summed
          IF(ITAB.gt.0) THEN
            DO IR=1,NDRGG
              CALL MNCHKCV(CQ(IR,IY,IV,1),CQ(IR,IY,IV,2),FRCCHG,ABSCHG)
              CVTAB(ITAB,1,IY)=CVTAB(ITAB,1,IY) + CQ(IR,IY,IV,1)
              CVTAB(ITAB,2,IY)=CVTAB(ITAB,2,IY) + CQ(IR,IY,IV,2)
              CVTAB(ITAB,3,IY)=CVTAB(ITAB,3,IY) + ABSCHG
            ENDDO

          ENDIF
        ENDDO
      ENDIF  ! of (CLEVEL.EQ.2)
! Test National Level convergence variables
      IR=11
      DO IV = 1,MNOTH
        VARNAM=MOVARSL(IV)
        CALL CVMATCH(VARNAM,CVTab_Match,ITAB)  ! see if variable "iv" is among those being summed
        IF(ITAB.gt.0) THEN
          CALL MNCHKCV(OTHER(IV,IY,1),OTHER(IV,IY,2),FRCCHG,ABSCHG)
          CVTAB(ITAB,1,IY)=CVTAB(ITAB,1,IY) + OTHER(IV,IY,1)
          CVTAB(ITAB,2,IY)=CVTAB(ITAB,2,IY) + OTHER(IV,IY,2)
          CVTAB(ITAB,3,IY)=CVTAB(ITAB,3,IY) + ABSCHG
        ENDIF
      ENDDO
    ENDDO

    if(CLevel.eq.2) then
      if(curirun.eq.1) then
       CVScoreHist(:,:)=0.0   
       CVScore(:)=0.0
      endif
      do iv=10,2,-1
         CVScoreHist(1:last_year,iv)=CVScoreHist(1:last_year,iv-1)  ! push 10-cycle history of GPA scores down one to make room for latest
      enddo
      CVScoreHist(1:last_year,1)=CVScore(1:last_year)             ! store score from prior cycle in history
    endif
! Compute deviations
    write(6,*)' '
    do IY=1,LAST_YEAR
      do itab=1,MNUMCVTST
        CVTAB(itab,4,IY)=0.
        if(cvtab(itab,2,IY).ne.0.) then
          CVTAB(itab,4,IY)=100.*CVTAB(ITAB,3,IY)/abs(CVTAB(ITAB,2,IY))  ! deviation as % prior
        elseif(cvtab(itab,1,IY).ne.0.) then
          CVTAB(itab,4,IY)=100.*CVTAB(ITAB,3,IY)/abs(CVTAB(ITAB,1,IY))  ! deviation as % current
        endif
      enddo

      weight_sum(1:7)=0.
      weight_count(1:7)=0.

      do itab=1,MNUMCVTST
        iclass=weight_cl(itab)
        if(iclass.ne.0) then
          weight_sum(iclass)=weight_sum(iclass)+CVTAB(ITAB,1,IY)*cvtab(itab,4,iy)*.01 ! deviations in % so multiply by.01
          weight_count(iclass)=weight_count(iclass)+CVTAB(ITAB,1,IY)
        endif
      enddo
! zero weights for items with a zero total      
      weight(1:7)=0. 
      do iclass=1,7
        if(weight_count(iclass).ne.0.) weight(iclass)=weight_start(iclass)
      enddo      
      if(iy.eq.lastyr.or.(debug_flag.eq.'debug'.and.(iy+1989).gt.2010)) then
        write(6,'(a,i4)') 'Convergence Variable Summary for ',iy+1989
        write(6,'(a)')    ' #   Variables in Group         Current      Prior  Abs Chng     Dev(%) Class'
        do itab=1,MNUMCVTST
          tempa=CVTab_match(itab)
          call mlower(tempa)
          if(sum(cvtab(itab,1:2,iy))/2..gt.10.) then
           write(6,'(i3,1x,a,2f10.1,F10.3,F10.5,i5)') itab,tempa,(cvtab(itab,i,iy),i=1,4),weight_cl(itab)
          else
           write(6,'(i3,1x,a,2f10.4,F10.5,F10.5,i5)') itab,tempa,(cvtab(itab,i,iy),i=1,4),weight_cl(itab)
          endif
        enddo
        write(6,*) ' '
        write(6,*) 'Average Deviation before score-weighting',iy+1989
        write(6,*) 'Class     Weighted Sum     Sum of     Average   Dev for  Scoring'
        write(6,*) '  #       of Deviations  Cur Values  Deviation  Scoring   Weight'
        do iclass=1,7
         ave=0.
         if(weight_count(iclass).gt.0) ave=weight_sum(iclass)/weight_count(iclass)
         write(6,'(i3,2x,F15.4,F13.1,f12.5,f10.5,F9.1)') iclass,weight_sum(iclass),weight_count(iclass),ave,min(ave,maxdev),weight(iclass)
        enddo
      endif

! Weight the average deviations by their scoring weight and total

      do iclass=1,7
        if(weight_sum(iclass).ne.0.0 .and. sum(weight(1:7)).ne.0. .and.weight_count(iclass).ne.0.) then
          ave=min(weight_sum(iclass)/weight_count(iclass),maxdev)  ! assign maximum deviation so large change in one class' percentage doesn't outweigh everything else
          weight_sum(iclass)=ave*(weight(iclass)/sum(weight(1:7)))
        endif
      enddo

! Assign a grade score to intercycle converge, ala GPA:  4:A, ..0:F
      score=sum(weight_sum(1:7)) 
      
      if(iy.eq.lastyr.or.(debug_flag.eq.'debug'.and.(iy+1989).gt.2010)) then
      
        write(6,*) 'Deviations by class after score-weighting (fractional basis)',iy+1989
        write(6,*) 'Class  Weighted Score'
        do iclass=1,7
          write(6,'(i3,2x,F12.6)') iclass,weight_sum(iclass)
        enddo
        write(6,*)'---------------------'
        write(6,'(a5,f12.6)') 'Total ',score
        write(6,*)' '
      endif
      IF(CLEVEL.eq.2) THEN
        call GPA(score,cvscore(iy))
        write(6,'(a,i2,1x,a,i5,f8.3)') 'run #',irun, 'REG:  intercycle',iy+1989,cvscore(iy)
      ELSE
        call GPA(score,cvscore_US(iy))
        write(6,'(a,i2,1x,a,i5,f8.3)') 'run #',irun, 'US:   intercycle',iy+1989,cvscore_US(iy)
      ENDIF

    enddo ! of IY=1,LAST_YEAR

! copy scores and sort from highest to lowest
    IF(CLEVEL.eq.2) then
      sorted_score(:)=cvscore(:)
    else
      sorted_score(:)=cvscore_US(:)
    endif
    call sort_Real(sorted_score(FIRST_YEAR),  LASTYR - FIRST_YEAR + 1 )  ! sort only the scores in the projected period that starts 2 years after last SEDS year
    ave_score=sum(sorted_score(lastyr-2:lastyr))/3   ! take average of 3 worst (lowest) scores

    IF(CLEVEL.eq.2) THEN
       write(6,'(a,i2,1x,a,f5.3)')'run #',irun,  'REG:  intercycle convergence GPA=', &
          ave_score
    ELSE
       write(6,'(a,i2,1x,a,f5.3)')'run #',irun,  'US:   intercycle convergence GPA=', &
          ave_score
    ENDIF  
    
    
! 
! Recompute absolute value deviations as signed deviations for reporting, as some have been confused by
! the aggregation of absolute regional deviations in Table 150.

! Compute signed percentage changes for table 150
    do IY=1,LAST_YEAR
      do itab=1,MNUMCVTST
        CVTAB(itab,4,IY)=0.
        if(CVTAB(itab,2,IY).ne.0.) then
          CVTAB(itab,4,IY)=100.*(CVTAB(ITAB,1,IY)/CVTAB(ITAB,2,IY)-1.)  !  % change
        endif
      enddo
    enddo

!   
    write(6,*) ' Percentage deviations'
    write(6,'(a)')    ' #   Variables in Group         Current     Prior     Change (%)'
    iy=2040-1989
    do itab=1,MNUMCVTST
      tempa=CVTab_match(itab)
      call mlower(tempa)
      write(6,'(i3,1x,a,2f10.1,F10.5)') itab,tempa,cvtab(itab,1,iy),cvtab(itab,2,iy),cvtab(itab,4,iy)
    enddo
    
    RETURN
    END SUBROUTINE SUM_CONVERGE
!==============================================================================
    SUBROUTINE CVMATCH(VARNAM,CVTab_Match,ITAB) 
    IMPLICIT NONE
    integer itab,i
    character*9 varnam
    character*27 CVTab_Match(MNUMCVTST)
    
! see if variable VARNAM is among those being summed and identified in CVTab_Match
    itab=0
    do i=1,MNUMCVTST
      if(index(cvtab_Match(i),trim(varnam)).gt.0) then
         itab=i
         return
      endif
    enddo
    return
    END SUBROUTINE CVMATCH
!==============================================================================
SUBROUTINE GPA(score,grade)
implicit none
! Converts a score to a GPA-type grade (4.0 to 0.) based on a grading scale (scale_it)

 real score,grade,interpol
 integer i
 real scale_it(5)/0.005, & !   A (4.0)
                 0.02,   & !   B (3.0) 
                 0.05,   & !   C (2.0) 
                 0.10,   & !   D (1.0)
                 0.15/     !   F (0.0)
 real grade_it(5)/4.,3.,2.,1.,0.0001/

      grade=0.
      if(score.lt.scale_it(1)) then
         grade=grade_it(1)
      elseif(score.ge.scale_it(5)) then
         grade=grade_it(5)
      else
! find grading scale interval where score lies. interpolate to return a fractional grade.
        do i=2,5
          if(score.ge. scale_it(i-1).and.score.lt.scale_it(i)) then
            interpol=(score-scale_it(i-1))/(scale_it(i)-scale_it(i-1))
            grade=grade_it(i-1)-(grade_it(i-1)-grade_it(i))*interpol
            exit
          endif
        enddo
      endif 
 return
 end SUBROUTINE GPA
!==============================================================================
      SUBROUTINE OUTPUT_RESTART(restart_name)
      IMPLICIT NONE
      character*(*) restart_name
! +++ FILER Declarations:
      INTEGER FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
      CHARACTER*100 FNAMEI,FNAMEO
! +++ End FILER Declarations
      INTEGER OPEN_STATUS

      OPEN_STATUS=0
      FRETCD=0
      FSOURC=1          ! Get variable list from a file
      FRTYPE=1          ! Output request

      FUNITI=62  ! unit number for varlist.txt file name
      OPEN(UNIT=FUNITI,FILE=VARLISTNAME,STATUS='OLD',IOSTAT=OPEN_STATUS)
      IF (OPEN_STATUS .NE. 0) THEN
        WRITE(6,'(a,a,a)') ' Unable to open varlist file ',TRIM(VARLISTNAME),'.  No relaxation done.'
        RETURN
      ENDIF

      FUNITO=63
      fnameo=restart_name
      OPEN(UNIT=FUNITO,FILE=fnameo,STATUS='UNKNOWN',IOSTAT=OPEN_STATUS, &
           FORM='UNFORMATTED',ACCESS='SEQUENTIAL',CONVERT='BIG_ENDIAN')
      IF (OPEN_STATUS .NE. 0) THEN
        WRITE(6,'(a)') ' Unable to open RESTART output file '//trim(fnameo)//'convergence summary not updated& no relaxation done.'
        RETURN
      ENDIF
      FUNFMT=0
! +++ Write out in same format as brought in
      FUNFMT=fmt(1)

      FNAMEI=' '
      FNAMEO=' '
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      CLOSE(UNIT=62)
      REWIND(FUNITO)
      CLOSE(UNIT=63)
      RETURN
      END SUBROUTINE OUTPUT_RESTART
!==============================================================================
      END PROGRAM INTERCV
!==============================================================================
      FUNCTION NUMP_AS(IPRICE,MNUMP)
      IMPLICIT NONE


      INTEGER NUMP_AS
      INTEGER MNUMP,IPRICE
      INTEGER NP
      PARAMETER (NP=84)
      INTEGER NUMPAS(NP)

!  THE NUMPAS VARIABLE IS A KEY FOR BOTH CONVERGENCE TESTING AND
!     SUMMING OVER THE SECTORS INTO THE 'AS' VARIABLES AS FOLLOWS:
!
!           VALUE   EFFECT
!            -1     NO CONVERGENCE TESTING OR SUMMING
!             0     NO SUMMING
!             N(>0) SUM PREVIOUS N VARIABLES;  NO CONVERGENCE TESTING

      DATA NUMPAS/ &
           0, 0, 0, 0, 4,       & ! PELRS,PELCM,PELTR,PELIN,PELAS,
          -1,-1,-1, 0, 0, 5,    & ! PGFRS,PGFCM,PGFTR,PGFIN,PGFEL,PGFAS,
          -1,-1,-1, 0, 0, 5,    & ! PGIRS,PGICM,PGITR,PGIIN,PGIEL,PGIAS,
           0, 0, 0, 0, 0, 5,    & ! PNGRS,PNGCM,PNGTR,PNGIN,PNGEL,PNGAS,
           0, 0,                & ! PGPTR,PLPIN,
           0, 0, 0, 0, 0, 5,    & ! PCLRS,PCLCM,PCLIN,PCLEL,PCLSN,PCLAS,
           0,                   & ! PMCIN,
           0, 0, 0, 3,          & ! PMGCM,PMGTR,PMGIN,PMGAS,
           0,                   & ! PJFTR,
           0, 0, 0, 0, 0, 5,    & ! PDSRS,PDSCM,PDSTR,PDSIN,PDSEL,PDSAS,
           0, 0, 0, 3,          & ! PKSRS,PKSCM,PKSIN,PKSAS,
           0, 0, 0, 0, 4,       & ! PLGRS,PLGCM,PLGTR,PLGIN,PLGAS,
           0, 0, 0, 0, 4,       & ! PRLCM,PRLTR,PRLIN,PRLEL,PRLAS,
           0, 0, 2,             & ! PRHTR,PRHEL,PRHAS,
           0, 0, 0, 0, 4,       & ! PRSCM,PRSTR,PRSIN,PRSEL,PRSAS,
           0, 0,                & ! PPFIN,PASIN,
           0, 0, 2,             & ! POTIN,POTTR,POTAS,
          -1,-1,-1,-1,-1,-1, 6, & ! PTPRS,PTPCM,PTPTR,PTPIN,PTPRF,PTPEL,PTPAS
           0, 0, 0, 0,          & ! PMETR,PETTR,PHYTR,PUREL
          -1,-1,-1/               ! PH1TR,PH2TR,PH3TR

      IF (NP.NE.MNUMP) THEN
         WRITE(*,*) ' PARAMETER MNUMP (DIMENSION OF MAIN PRICE ARRAY' &
                  //' MPRC) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION NUMP_AS IN MAIN.'
      ENDIF

      NUMP_AS=NUMPAS(IPRICE)
      RETURN
      END FUNCTION NUMP_AS
!==============================================================================
      FUNCTION NUMQ_AS(IQUAN,MNUMQ)
      IMPLICIT NONE

      INTEGER NUMQ_AS
      INTEGER MNUMQ,IQUAN
      INTEGER NQ
      PARAMETER (NQ=157)
      INTEGER NUMQAS(NQ)

      DATA NUMQAS/ &
            0, 0, 0, 0, 0, 0, 6,        & ! QELRS,QELCM,QELTR,QELIN,QELRF,QELHM,QELAS,
           -1,-1,-1, 0,-1, 0,-1, 7,     & ! QGFRS,QGFCM,QGFTR,QGFIN,QGFRF,QGFEL,QGFHM,QGFAS,
           -1,-1,-1, 0,-1, 0,-1, 7,     & ! QGIRS,QGICM,QGITR,QGIIN,QGIRF,QGIEL,QGIHM,QGIAS,
            0, 0, 0, 0, 0, 0, 0, 7,     & ! QNGRS,QNGCM,QNGTR,QNGIN,QNGRF,QNGEL,QNGHM,QNGAS,
            0, 0,                       & ! QGPTR,QLPIN,
            0, 0, 0, 0, 0, 0, 0, 7,     & ! QCLRS,QCLCM,QCLIN,QCLRF,QCLEL,QCLSN,QCLHM,QCLAS,
            0,                          & ! QMCIN,
            0, 0, 0, 3,                 & ! QMGCM,QMGTR,QMGIN,QMGAS,
            0,                          & ! QJFTR,
            0, 0, 0, 0, 0, 0, 6,        & ! QDSRS,QDSCM,QDSTR,QDSIN,QDSRF,QDSEL,QDSAS,
            0, 0, 0, 3,                 & ! QKSRS,QKSCM,QKSIN,QKSAS,
            0, 0, 0, 0, 0, 5,           & ! QLGRS,QLGCM,QLGTR,QLGIN,QLGRF,QLGAS,
            0, 0, 0, 0, 0, 5,           & ! QRLCM,QRLTR,QRLIN,QRLRF,QRLEL,QRLAS,
            0, 0, 2,                    & ! QRHTR,QRHEL,QRHAS,
           -1,-1,-1,-1,-1, 5,           & ! QRSCM,QRSTR,QRSIN,QRSRF,QRSEL,QRSAS,
            0, 0, 0,                    & ! QPFIN,QSGIN,QSGRF,
            0, 0, 0, 3,                 & ! QPCIN,QPCRF,QPCEL,QPCAS,
            0,                          & ! QASIN,
            0, 0, 0, 3,                 & ! QOTTR,QOTIN,QOTRF,QOTAS,
           -1,-1,-1,-1,-1,-1, 6,        & ! QTPRS,QTPCM,QTPTR,QTPIN,QTPRF,QTPEL,QTPAS,
            0, 0, 0, 0, 0, 0,           & ! QMETR,QETTR,QETHM,QHYTR,QUREL,QURHM,
            0, 0, 2,                    & ! QHOIN,QHOEL,QHOAS,
            0, 0, 0, 3,                 & ! QGERS,QGEIN,QGEEL,QGEAS,
            0, 0, 0, 0, 0, 0, 0, 7,     & ! QBMRS,QBMCM,QBMIN,QBMRF,QBMEL,QBMSN,QBMHM,QBMAS,
            0, 0, 2,                    & ! QMSIN,QMSEL,QMSAS,
            0, 0, 0, 0, 4,              & ! QSTRS,QSTCM,QSTIN,QSTEL,QSTAS,
            0, 0, 0, 0, 4,              & ! QPVRS,QPVCM,QPVIN,QPVEL,QPVAS,
            0, 0, 2,                    & ! QWIIN,QWIEL,QWIAS,
           -1,-1,-1,-1,-1,-1,-1, 7,     & ! QTRRS,QTRCM,QTRTR,QTRIN,QTREL,QTRSN,QTRHM,QTRAS,
            0, 0,                       & ! QEIEL,QCIIN,
           -1,-1,-1,-1,-1,-1,-1,-1, 8,  & ! QTSRS,QTSCM,QTSTR,QTSIN,QTSRF,QTSEL,QTSSN,QTSHM,QTSAS
            0, 0, 0/                      ! QH1TR,QH2TR,QH3TR

      IF (NQ.NE.MNUMQ) THEN
         WRITE(*,*) ' PARAMETER MNUMQ (DIMENSION OF MAIN QUANTITY ARRAY' &
                  //' MPRC) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION NUMQ_AS IN MAIN.'
      ENDIF

      NUMQ_AS=NUMQAS(IQUAN)

      RETURN
      END FUNCTION NUMQ_AS
!==============================================================================
      FUNCTION PQMATCH(IPRICE,MNUMQ,MNUMP)
      IMPLICIT NONE

      INTEGER PQMATCH        ! Returns product code subscript for MQTY
      INTEGER NP             ! Dimension of map -- MUST MATCH MNUMP
      PARAMETER(NP=84)
      INTEGER MAP(NP)        ! Mapping of PRICE products to QUANTITY products
      INTEGER IPRICE         ! Product code subscript for MPRC
      INTEGER MNUMQ,MNUMP    ! Dimensions of MQTY and MPRC to insure against
                             ! changes.

      DATA MAP/ &
         1,  2,  3,  4,  7,          & ! PELRS,PELCM,PELTR,PELIN,PELAS,
         8,  9, 10, 11, 13, 15,      & ! PGFRS,PGFCM,PGFTR,PGFIN,PGFEL,PGFAS,
        16, 17, 18, 19, 21, 23,      & ! PGIRS,PGICM,PGITR,PGIIN,PGIEL,PGIAS,
        24, 25, 26, 27, 29, 31,      & ! PNGRS,PNGCM,PNGTR,PNGIN,PNGEL,PNGAS,
        32, 33,                      & ! PGPTR,PLPIN,
        34, 35, 36, 38, 39, 41, 42,  & ! PCLRS,PCLCM,PCLIN,PCLEL,PCLSN,PCLAS,PMCIN,
        43, 44, 45, 46, 47,          & ! PMGCM,PMGTR,PMGIN,PMGAS,PJFTR,
        48, 49, 50, 51, 53, 54,      & ! PDSRS,PDSCM,PDSTR,PDSIN,PDSEL,PDSAS,
        55, 56, 57, 58,              & ! PKSRS,PKSCM,PKSIN,PKSAS,
        59, 60, 61, 62, 64,          & ! PLGRS,PLGCM,PLGTR,PLGIN,PLGAS,
        65, 66, 67, 69, 70,          & ! PRLCM,PRLTR,PRLIN,PRLEL,PRLAS,
        71, 72, 73,                  & ! PRHTR,PRHEL,PRHAS,
        74, 75, 76, 78, 79,          & ! PRSCM,PRSTR,PRSIN,PRSEL,PRSAS,
        80, 87,                      & ! PPFIN,PASIN,
        88, 89, 91,                  & ! POTTR,POTIN,POTAS,
        92, 93, 94, 95, 96, 97, 98,  & ! PTPRS,PTPCM,PTPTR,PTPIN,PTPRF,PTPEL,PTPAS,
        99,100,102,103,              & ! PMETR,PETTR,PHYTR,PUREL
       155,156,157/                    ! PH1TR,PH2TR,PH3TR

      PQMATCH=MAP(IPRICE)
      IF (NP.NE.MNUMP) THEN
         WRITE(*,*) ' PARAMETER MNUMP (DIMENSION OF MAIN PRICE ARRAY' &
                  //' MPRC) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION PQMATCH IN MAIN.'
      ENDIF

      IF (PQMATCH.GT.MNUMQ) THEN
         WRITE(*,*) ' PARAMETER MNUMQ (DIMENSION OF MAIN QTY ARRAY' &
                  //' MQTY) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION PQMATCH IN MAIN.'
      ENDIF

      RETURN
      END FUNCTION PQMATCH
!==============================================================================
      SUBROUTINE SORTRPT(FRCCHG,IR,PREVV,MRC,MVARS)
      IMPLICIT NONE
      include 'parametr'

      INTEGER      MNDBGVARS   ! NUMBER OF CONVERGENCE VARIABLES HELD FOR RPT
      PARAMETER   (MNDBGVARS=250)
      REAL         DTBAC(MNDBGVARS,3) ! Before (PRIOR), After (CURRENT)
!                                     !  and fractional Change (B-A-C)
      INTEGER      DTREG(MNDBGVARS)   ! CENSUS REGION INDEX OF VARIABLE
      CHARACTER*9  DTVAR(MNDBGVARS)   ! NAME OF VARIABLE
 
      REAL PREVV,MRC,FRCCHG
      INTEGER IR
      CHARACTER*(*) MVARS
 
      INTEGER I,IDX
      I = 1
      IDX = 0
      DO WHILE ((I .LE. MNDBGVARS) .AND. (IDX .EQ. 0))
        IF (FRCCHG .GT. DTBAC(I,3)) IDX = I
        I = I + 1
      ENDDO
      IF (IDX .GT. 0) THEN
        DO I = (MNDBGVARS-1), IDX , -1
          DTBAC(I + 1,1) = DTBAC(I,1)
          DTBAC(I + 1,2) = DTBAC(I,2)
          DTBAC(I + 1,3) = DTBAC(I,3)
          DTREG(I + 1) = DTREG(I)
          DTVAR(I + 1) = DTVAR(I)
        ENDDO
        DTBAC(IDX,1) = PREVV
        DTBAC(IDX,2) = MRC
        DTBAC(IDX,3) = FRCCHG
        DTREG(IDX)   = IR
        DTVAR(IDX)   = MVARS
      ENDIF

      RETURN
      END SUBROUTINE SORTRPT
 
!=======================================================
    subroutine SORT_real(insort,array_len_in)
    USE DFPORT
    implicit none
    integer(4) array_len_in
    integer(SIZEOF_SIZE_T) array_len
    integer(2), external :: cmp_function
    real(4) insort(array_len_in)
 
    integer(SIZEOF_SIZE_T) array_size
    array_size = 4 ! size of each item in array in bytes
    array_len=array_len_in
    
    CALL qsort(insort,array_len,array_size,cmp_function)
    END subroutine sort_real
!=======================================================
    integer(2) function cmp_function(a1, a2)
    real(4) a1, a2
      
    if(a1.gt.a2) then
      cmp_function=-1
    elseif(a1.lt.a2) then
      cmp_function=1
    else
      cmp_function=0
    endif
    end function cmp_function
! ===============================================
      subroutine Mlower(a)
      implicit none
      character*(*) a
      integer L,j  ,i
      L=len(a)
      do i=1,L
        j=ichar(a(i:i))
        if(j.ge.65.and.j.le.90)then
          a(i:i)=char(j+32)
        endif
      enddo
      return
      end subroutine Mlower
!==============================================================================
      SUBROUTINE COPY_TO_LOCAL_ADJUSTED(IRESTART)
      IMPLICIT NONE

      include 'parametr'
      include 'ampblk'
      include 'angtdm'
      include 'acoalprc'
      include 'emmparm'
      include 'cdsparms'
      include 'coalemm'
!
!  Overwrite local copy of prices with adjusted prices for convergence scoring only. 
!  This shouldn't be called during relaxation loop.

! Variables holding adjusted price values to be checked for convergence
      REAL P(MNUMCR,MNUMYR,MNUMP,3)
      REAL GP(NNGEM,MNUMYR,7,3)
      REAL CP(2,NDRGG,MNUMYR,3)
      common/cblk/P,GP,CP
      
      integer IRESTART
      
      P(:,:,:,IRESTART) = MPRC(:,:,:)
      GP(:,:,1:3,IRESTART) = MUPRC(:,:,1:3)
      GP(:,:,4:7,IRESTART) = SMUPRC(:,:,1:4)
      CP(:,:,:,IRESTART) = PCLELCDR(:,:,:)

      RETURN
      END SUBROUTINE COPY_TO_LOCAL_ADJUSTED
