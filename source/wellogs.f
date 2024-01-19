! $Header: m:/default/source/RCS/wellogs.f,v 1.366 2020/10/14 20:40:57 DH5 Exp $
!      DEBUG SUBCHK,SUBTRACE
!      END DEBUG
      SUBROUTINE WELL
      IMPLICIT NONE
!%     PROGRAM OGSM
!%     IMPLICIT NONE
!===> MAIN MODULE START <===
!
!%     CALL WELL     ! INTERNAL CALL, DELETE THIS MAIN
!%     STOP          ! MODULE WHEN INTEGRATED INTO THE
!%     END           ! NEMS.
!
!===> MAIN MODULE END <===


!********************************************************************
!********************************************************************
!
!                   O G S M   B A S I C   F R A M E W O R K
!
!
!********************************************************************
!********************************************************************
!     SUBROUTINE WELL

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! ogsm lower 48 onshore variables
      include'ogsmoff'      ! ogsm lower 48 offshore variables
      include'ogsmak'       ! ogsm alaska variables
      include'ogsmlng'      ! foreign input variables
      include'ogsmout'      ! ogsm global output variables
      include'macout'       ! macro global output variables
      include'intout'       ! international global output variables
      include'pmmout'       ! lfmm global output variables
      include'lfmmout'      ! lfmm global output variables
      include'ngtdmrep'     ! ngmm global output variables
      include'mpblk'        ! NEMS price common block
      include'mxpblk'
      include'mxqblk'

      REAL OG_GROWTH
      REAL GDP87
      REAL BAA, T10YR, GDPCH, WACC, WACC_REAL
      real*4 lfmmprc(mnumpr+1,mnumyr)
      real*4 lfmmprd(mnumpr+1,mnumyr)
      REAL*4 TEMPSUM(mnumor), TEMPPRD(mnumor), TEMPOIL(mnumor)
      REAL*4 PRDTEMP(MNUMOR,MNCRUD,MNUMYR)
      INTEGER LOOPIT, DIST, IR
      INTEGER IYR           ! RUN YEAR (I.E., 1,2,3...)

!==========================> HSM Code Start <==========================
      LOGICAL hsm_dcf_first           /.true./
      LOGICAL hsm_offshore_first      /.true./
      LOGICAL hsm_discount_rate_first /.true./
      character(len=1024) :: hsm_filename
!===========================> HSM Code End <===========================

      LOOPIT = LASTYR      ! NUMBER OF YEARS TO LOOP OVER


!  OPEN THE DEBUG OUTPUT FILE
      IF (CURIYR.EQ.FIRSYR.AND. &
          CURITR.EQ.1) THEN
           OPEN(UNIT=BUGOUT,FILE='OGSMOUT.txt')
      ENDIF
      
!==========================> HSM Code Start <==========================
      if (hsm_restart_bool) call hsm_restart_output
      
      OPEN(unit=hsm_out     , file='hsm_out.log'     , action='write', position='append')
      if (hsm_dcf_bool) then
        
        write (hsm_filename, "(A12,I4,A4)") 'hsm_dcf_off_', curcalyr , '.log'
        OPEN(unit=hsm_dcf_off  , file=hsm_filename , action='write', position='append')
        
        write (hsm_filename, "(A11,I4,A4)") 'hsm_dcf_on_', curcalyr , '.log'
        OPEN(unit=hsm_dcf_on  , file=hsm_filename , action='write', position='append')
        
        OPEN(unit=hsm_dcf_ak  , file='hsm_dcf_ak.log'     , action='write', position='append')
        OPEN(unit=hsm_dcf_opts, file='hsm_dcf_opts.log'   , action='write', position='append')
        
        if (CURITR.EQ.1) then
          write(hsm_dcf_on   , '(*(G0.16,:,","))') 'array', (/0:39/)
          write(hsm_dcf_off  , '(*(G0.16,:,","))') 'function', 'iecon', 'array', (/0:60/)
        endif
        
        if (hsm_dcf_first) Then
          write(hsm_dcf_ak   , '(*(G0.16,:,","))') 'function', 'year', 'array', (/0:29/)
          write(hsm_dcf_opts , '(*(G0.16,:,","))') 'function', 'year', 'crude_price', 'natgas_price', 'crude_trans_price', &
              'natgas_trans_price', 'crude_tariff_price', 'natgas_tariff_price', 'royalty_rate', 'fed_tax_rate', & 
              'ogsm_test_npv', 'abandon_rate', 'state_abbreviation', 'exploratory_success_rate', 'development_success_rate', &
               'exploratory_cost', 'development_cost', 'exploratory_dry_cost', 'development_dry_cost', 'exp_tang_frac', &
               'dev_tang_frac', 'kap_tang_frac', 'intang_amor_frac', 'amor_schedule', 'deprec_schedule', 'discount_rate', &
               'evaluation_unit', 'field_size_class', 'oil_resources', 'predecline_prod_fraction_oil', 'gas_resources', & 
               'predecline_prod_fraction_gas', 'platform_cost', 'flowline_cost', 'flowlines', 'water_depth_ft', &
               'platform_type', 'platform_slots', 'drill_per_year', 'total_dev_wells', 'delineation_wells'
          hsm_dcf_first = .false.
        endif
      endif
      
      OPEN(unit=hsm_disc_rate  , file='hsm_disc_rate.log'     , action='write', position='append')
      
      if (hsm_offshore_bool) then
        
        OPEN(unit=hsm_offshore_disc   , file='hsm_offshore_disc.log'   , action='write', position='append')
        OPEN(unit=hsm_offshore_cumnfw , file='hsm_offshore_cumnfw.log' , action='write', position='append')
        OPEN(unit=hsm_offshore_prod   , file='hsm_offshore_prod.log'   , action='write', position='append')
        
        if (hsm_offshore_first) Then
          write(hsm_offshore_disc   , '(*(G0.16,:,","))') 'variable', 'evaluation_unit', 'year', (/2:17/)
          write(hsm_offshore_cumnfw , '(*(G0.16,:,","))') 'evaluation_unit', (/1:MNUMYR/)
          write(hsm_offshore_prod   , '(*(G0.16,:,","))') 'year', 'oil_or_gas', 'producing', 'announced', 'discovered'
          hsm_offshore_first = .false.
        endif
      endif
!===========================> HSM Code End <===========================

!  OPEN THE OGSM SYSTEM OUTPUT FILE
      IF (PRTDBGL.EQ.1.AND.CURIYR.EQ.FIRSYR.and.curitr.eq.1) THEN
         FNAME='WLDEBUG'
         NEW=.TRUE.
         SYSOUT = FILE_MGR('O',FNAME,NEW)

         FNAME='OGSMDBG'
         NEW=.TRUE.
         OGBUG1 = FILE_MGR('O',FNAME,NEW)

         FNAME='OGSMONEP'
         NEW=.TRUE.
         OFILE1 = FILE_MGR('O',FNAME,NEW)

         FNAME='OGSMONWL'
         NEW=.TRUE.
         OFILE2 = FILE_MGR('O',FNAME,NEW)

         FNAME='OGSMONRP'
         NEW=.TRUE.
         OFILE3 = FILE_MGR('O',FNAME,NEW)

      ENDIF

!  OPEN THE OGSM SYSTEM OUTPUT FILES
      IF (PRTDBGL.EQ.1.AND.CURIYR.EQ.FIRSYR.and.curitr.eq.1) THEN
         FNAME='WLUNOUT'
         NEW=.TRUE.
         UNOUT1 = FILE_MGR('O',FNAME,NEW)

         FNAME='OGUNOUT'
         NEW=.TRUE.
         DBOUT = FILE_MGR('O',FNAME,NEW)
      ENDIF

!  OPEN THE UGR OUTPUT FILE

      IF (CURIYR.EQ.FIRSYR.and.curitr.eq.1) THEN
         FNAME='OGSMEOR'
         NEW=.TRUE.
         VAROUT = FILE_MGR('O',FNAME,NEW)
      ENDIF

!  START TESTING LOOP
!%     DO CURIYR = 1,LOOPIT

!  TEST IF CURRENT INVOCATION OF SUBROUTINE WELL IS REPORTING STEP
      IF ((NCRL.EQ.1).AND.(CURIYR.LE.LOOPIT)) THEN
         do dist=1,ogdist
          write(ogbug1,*) 'NGMMinput', curiyr+1989, dist, ogrnagprd(dist,gastypes,curiyr), ogprcng(dist,curiyr)
         enddo

!  INVOKE FUNCTION TO INITIALIZE PRICES FOR CURRENT ITERATION
!  CHECK IF STAND-ALONE RUN FLAG IS SET & CALL THE RIGHT PRICE SUB.
         IF (OGRUNOP(1).EQ.1) THEN
           CALL OGINIT_PRICE2  ! USE OUR OWN INTERNAL PRICES
         ELSE
           CALL OGINIT_PRICE   ! USE NEMS COMPUTED PRICES
         ENDIF

!  IF CURRENT MODE IS STAND-ALONE RUN THEN OUTPUT SPECIAL HEADER
      IF (PRTDBGL.EQ.1) THEN
        IF (OGRUNOP(1).EQ.1) THEN
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ===            O G S M              ==='
         WRITE (SYSOUT,*) ' === >> INTERNAL PRICE & PROD RUN << ==='
         WRITE (SYSOUT,*) ' ===           ( I P P )             ==='
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ======================================='
        ELSEIF (OGRUNOP(1).EQ.2) THEN
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ===            O G S M              ==='
         WRITE (SYSOUT,*) ' === >> INTERNAL OIL&GAS PROD RUN << ==='
         WRITE (SYSOUT,*) ' ===           ( I O G P )           ==='
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ======================================='
        ELSEIF (OGRUNOP(1).EQ.3) THEN
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ===            O G S M              ==='
         WRITE (SYSOUT,*) ' ===   >> INTERNAL OIL PROD RUN <<   ==='
         WRITE (SYSOUT,*) ' ===           ( I O P )             ==='
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ======================================='
        ELSEIF (OGRUNOP(1).EQ.4) THEN
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ===            O G S M              ==='
         WRITE (SYSOUT,*) ' ===   >> INTERNAL GAS PROD RUN <<   ==='
         WRITE (SYSOUT,*) ' ===           ( I G P )             ==='
         WRITE (SYSOUT,*) ' ======================================='
         WRITE (SYSOUT,*) ' ======================================='
        ENDIF
      ENDIF

         IF (PRTDBGL.EQ.1.AND.CURIYR .EQ. FIRSYR) THEN
!  WRITE OUT THE RUNOPTIONS TO SUPPORT ANALYSIS OF THE RUN
!
 800  FORMAT(20(5X,I4,I5,/))
            WRITE (SYSOUT,*) ' * *  RUN OPTIONS'
            WRITE (SYSOUT,800) (I,OGRUNOP(I),I=1,20)
! WRITE OUT RESOURCE ADJUSTMENT FACTORS
            WRITE(SYSOUT,698) (INFFACL48(K),K=1,L48FUEL)
            WRITE(SYSOUT,699) (URRFACL48(K),K=1,L48FUEL)
! WRITE OUT DRILLING COST, DRILLING RESPONSE, & EXTRACTION RATE
! PARAMETERS AND TECHNOLOGY ADJ START YEAR - MDRADJUSTMENTS
            WRITE(SYSOUT,700) COSTADJ,DRLADJ,PRNEWADJ,TECHYR
  698 FORMAT(1X,'INFFACL48:',7F9.3)
  699 FORMAT(1X,'URRFACL48:',7F9.3)
  700 FORMAT(1X,'COSTADJ:',F5.2,3X,'DRLADJ:',F5.2,3X, &
                'PRNEWADJ:',F5.2,3X,'TECHYR:',I4)
         ENDIF

!  INVOKE LOWER 48 ONSHORE SUBROUTINES
         IF (CURIYR.GE.L48BYR) THEN
!          CALL OGFOR_L48
!          CALL OGMAIN_ON
           CALL OGOUT_L48
!          IF (curiyr.gt.l48hyr) then
             WRITE(BUGOUT,200) 'ngpls',curiyr+1989,sum(OGNGPLPRD(1:OGDIST,curiyr)), &
             sum(OGNGPLET(1:OGDIST,curiyr)), &
             sum(OGNGPLPR(1:OGDIST,curiyr)), &
             sum(OGNGPLBU(1:OGDIST,curiyr)), &
             sum(OGNGPLIS(1:OGDIST,curiyr)), &
             sum(OGNGPLPP(1:OGDIST,curiyr))
!          ENDIF
         ENDIF
  200 FORMAT(1X,A5,2x,I4,2x,6(F7.3))

!  INVOKE LOWER 48 OFFSHORE SUBROUTINES
         IF (CURIYR.GE.OFFBYR) THEN
!          CALL OGMAIN_OFF
!          CALL OGWELLS_OFF
           CALL OGOUT1_OFF
         ELSE
           CALL OGHIST_OFF(CURIYR)
           CALL OGOUT1_OFF
         ENDIF

!  INVOKE ALASKA REPORTING SUBROUTINE
         CALL OGREP_AK

!  INVOKE ROUTINE TO REPORT DATA TO CENTRAL DATABASE
         CALL OGREP_TABLES

!  INVOKE ROUTINES THAT GENERATE VARIOUS REPORTS
         CALL OGREP_OGS
         CALL OGREP_IER

!  INVOKE ROUTINE TO OUTPUT YEARS RESULTS
         CALL OGTAB_WRITE

!  INVOKE ROUTINE TO OUTPUT CO2 EOR REPORT
         if (curiyr.eq.lastyr) call rpt_co2

!  INVOKE ROUTINE TO OUTPUT REALIZED PRODUCTION REPORT
         if (curiyr.eq.lastyr) call write_rprod

! WRITE OUT SCALE FACTORS
      if (curiyr.eq.lastyr) then
      write(bugout,*) '************************************************'
      write(bugout,*) '************************************************'
      write(bugout,*) '*** STEO SCALE FACTORS ***'
      write(bugout,*) '*** SCALE FLAG = ', ogsteo
!     write(bugout,*) '*** sDCRDWHP'
!     write(bugout,11) '***','OGSM1','OGSM2','OGSM3','OGSM4','OGSM5','OGSM6','OGSM7','OGSM8','OGSM9', &
!                     'OGSM10','OGSM11','OGSM12','OGSM13'
!     do M = 1,3
!       write(bugout,12) l48hyr+M+baseyr-1, (sDCRDWHP(I,M),I=1,MNUMOR-1)
!     enddo
      write(bugout,*) '***'
      write(bugout,*) '*** sRFQTDCRD'
      write(bugout,11) '***','OGSM1','OGSM2','OGSM3','OGSM4','OGSM5','OGSM6','OGSM7','OGSM8','OGSM9', &
                      'OGSM10','OGSM11','OGSM12','OGSM13'
      do M = 1,3
        write(bugout,12) l48hyr+M+baseyr-1, (sRFQTDCRD(I,M),I=1,MNUMOR-1)
      enddo
      write(bugout,*) '***'
      write(bugout,13) '***  sNGPLPRD','sNGPLET','sNGPLPR','sNGPLBU','sNGPLIS','sNGPLPP'
      do M = 1,3
        write(bugout,14) l48hyr+M+baseyr-1,sNGPLPRD(M),sNGPLET(M),sNGPLPR(M),sNGPLBU(M),sNGPLIS(M),sNGPLPP(M)
      enddo
      write(bugout,*) '***'
      write(bugout,*) '*** EXPECTED U.S. NATURAL GAS PRODUCTION'
      write(bugout,*) '***         sOGENAGPRD           sOGADGPRD'
      write(bugout,15) '*** ',l48hyr+baseyr-1,l48hyr+baseyr,l48hyr+baseyr-1,l48hyr+baseyr
      do M = 1,OGDIST
        write(bugout,16) M,OGENAGPRD(m,gastypes,l48hyr),OGENAGPRD(m,gastypes,l48hyr+1),OGADGPRD(m,oiltypes,l48hyr),OGADGPRD(m,oiltypes,l48hyr+1)
      enddo
      write(bugout,*) '***'
      write(bugout,*) '*** EXPECTED CANADIAN NATURAL GAS PRODUCTION'
      write(bugout,*) '***        sCNADGPRD           sCNNAGPRD'
      write(bugout,17) '*** ','AD-East','AD-West','NA-East','NA-West'
      write(bugout,16) L48HYR+baseyr,(CNADGPRD(m,l48hyr+1),m=1,numcan),(CNENAGPRD(m,l48hyr+1),m=1,numcan)
      write(bugout,*) '************************************************'
      write(bugout,*) '************************************************'
      endif
 11 FORMAT(1x,A3,2x,<mnumor-1>(A7,1x))
 12 FORMAT(1x,I4,2x,<mnumor-1>(F6.3,2x))
 13 FORMAT(1x,A13,6(A10))
 14 FORMAT(1x,I4,3x,6(F6.3,4x))
 15 FORMAT(1x,A4,4(I10))
 16 FORMAT(1x,I4,4(F10.3))
 17 FORMAT(1x,A4,4(3x,A7))

      ELSE   ! NOT REPORTING STEP

!  INVOKE INPUT & INITIALIZE ROUTINES IF FIRST YEAR & FIRST ITTERATION
         IF (CURIYR .EQ. FIRSYR .AND. CURITR .EQ. 1) THEN

!  READ DATA
            CALL OGINIT_BFW   ! READ OGSM SYSTEM DATA

!************************************************************************************************************************
! TEMPORARY -- FILL IN VARIABLES FOR LOOKING AHEAD
!        DO M=1,MNUMYR
!          OGPRDNG(MNUMOR,M) = sum(OGRNAGPRD(1:OGDIST,gastypes,M)) + sum(OGADGPRD(1:OGDIST,oiltypes,M))
!          RFQTDCRD(MNUMOR+2,M) = sum(OGOILPRD(1:OGDIST,oiltypes,M))
!          RFQDCRD(MNUMOR+2,M) = sum(OGOILPRD(1:OGDIST,oiltypes,M))
!          RFQTDCRD(MNUMOR,M) = OGOILPRD(3,oiltypes,M)+ogoilprd(75,oiltypes,M)+ogoilprd(84,oiltypes,M)
!          RFQTDCRD(MNUMOR+1,M) = RFQTDCRD(MNUMOR+2,M) - RFQTDCRD(MNUMOR,M)
!        ENDDO
!! ASSIGN REGIONAL WELLHEAD PRICE USING DISTRICT LEVEL PRICES FROM NGMM
!        DO M=1,MNUMYR
!          TEMPPRD = 0.
!          TEMPSUM = 0.
!          TEMPOIL = 0.
!          DO DIST=1,ogdist
!            IR = distmap(dist,1)
!            IF (IR.NE.0.AND.IR.LE.L48RGN+OFFNEMSRGN) THEN   ! LOWER 48
!              TEMPSUM(IR) = TEMPSUM(IR) + OGPRCNG(dist,m)*(OGRNAGPRD(dist,gastypes,m)+OGADGPRD(dist,oiltypes,m))
!              TEMPPRD(IR) = TEMPPRD(IR) + OGRNAGPRD(dist,gastypes,m) + OGADGPRD(dist,oiltypes,m)
!              TEMPOIL(IR) = TEMPOIL(IR) + OGOILPRD(dist,oiltypes,m)
!              TEMPOIL(MNUMOR) = TEMPOIL(MNUMOR) + OGOILPRD(dist,oiltypes,m)
!            ENDIF
!            IF (DIST.EQ.3.OR.DIST.EQ.75.OR.DIST.EQ.84) THEN       ! ALASKA
!              TEMPSUM(L48RGN+OFFNEMSRGN+2) = TEMPSUM(L48RGN+OFFNEMSRGN+2) + OGPRCNG(dist,m)*(OGRNAGPRD(dist,gastypes,m)+OGADGPRD(dist,oiltypes,m))
!              TEMPPRD(L48RGN+OFFNEMSRGN+2) = TEMPPRD(L48RGN+OFFNEMSRGN+2) + OGRNAGPRD(dist,gastypes,m)+OGADGPRD(dist,oiltypes,m)
!              TEMPOIL(L48RGN+OFFNEMSRGN+2) = TEMPOIL(L48RGN+OFFNEMSRGN+2) + OGOILPRD(dist,oiltypes,m)
!            ENDIF
!            TEMPSUM(MNUMOR) = TEMPSUM(MNUMOR) + OGPRCNG(dist,m)*(OGRNAGPRD(dist,gastypes,m)+OGADGPRD(dist,oiltypes,m))
!            TEMPPRD(MNUMOR) = TEMPPRD(MNUMOR) + OGRNAGPRD(dist,gastypes,m)+OGADGPRD(dist,oiltypes,m)
!          ENDDO
!
!          DO R=1,MNUMOR
!            OGPRDNG(R,M) = TEMPPRD(R)
!            IF (TEMPPRD(R) > 0. .AND. TEMPSUM(R) > 0.) OGWPRNG(R,M) = TEMPSUM(R)/TEMPPRD(R)
!            if (r == l48rgn+offnemsrgn+1 .or. r == l48rgn+offnemsrgn+3) ogwprng(r,m) = ogwprng(l48rgn+offnemsrgn+2,m)
!          ENDDO
!          OGWPRNG(MNUMOR,M) = sum(TEMPSUM(1:L48RGN+OFFNEMSRGN))/sum(TEMPPRD(1:L48RGN+OFFNEMSRGN))  ! replace with lower 48 average (exclude AK)
!!         write(6,*) 'dh5gas',M+1989,OGPRDNG(MNUMOR,M), OGWPRNG(MNUMOR,M)
!! ASSIGN NEW LFMM VARIABLE RFCRUDEWHP(LFMMRGN,CRUDETYPE,YEAR) to OLD VARIABLE DCRDWHP(REGION,YEAR)
!          do r=1,MNUMPR
!            lfmmprc(r,m) = 0.
!            lfmmprd(r,m) = 0.
!            do t=1,mncrud
!               lfmmprc(r,m) = lfmmprc(r,m) + RFCRUDEWHP(r,t,m)*OGCRUDEREF(r,t,m)
!               lfmmprd(r,m) = lfmmprd(r,m) + OGCRUDEREF(r,t,m)
!            enddo
!            if (lfmmprd(r,m).gt.0.) lfmmprc(r,m) = lfmmprc(r,m)/lfmmprd(r,m)
!          enddo
!          do r=1,MNUMOR-1
!              DCRDWHP(r,M) = lfmmprc(lfmmmap(r),m)
!              RFQTDCRD(R,M) = TEMPOIL(R)
!              XRFQDCRD(R,M) = RFQTDCRD(R,M)
!!         write(6,*) 'dh5oil',M+1989, R, RFQTDCRD(R,M), DCRDWHP(R,M)
!          enddo
!          XRFQDCRD(MNUMOR,M) = RFQTDCRD(MNUMOR,M)
!          DCRDWHP(MNUMOR,M) = 0.
!          DO I =1,L48RGN+OFFNEMSRGN    ! CALCULATE LOWER 48 AVERAGE WELLHEAD PRICE
!            DCRDWHP(MNUMOR,M)=DCRDWHP(MNUMOR,M) + DCRDWHP(I,M)*RFQTDCRD(I,M)
!          ENDDO
!          DCRDWHP(MNUMOR,M)=DCRDWHP(MNUMOR,M)/RFQTDCRD(MNUMOR+1,M)
!          IF (M+1989.EQ.2016) DCRDWHP(MNUMOR,M) = DCRDWHP(MNUMOR,M)*1.10
!          IF (M+1989.EQ.2017) DCRDWHP(MNUMOR,M) = DCRDWHP(MNUMOR,M)*1.25
!          IF (M+1989.GT.2017) DCRDWHP(MNUMOR,M) = DCRDWHP(MNUMOR,M)*1.10
!!         write(6,*) 'dh5oil',M+1989, mnumor, RFQTDCRD(MNUMOR+2,M), DCRDWHP(MNUMOR,M)
!        ENDDO
!        DO M=MNUMYR+1,MNXYR
!          do r=1,MNUMOR
!            XOGWPRNG(R,M) = OGWPRNG(R,MNUMYR)*(1.01)**(M-MNUMYR)
!            XOGWPRNGPF(1,R,M) = OGWPRNG(R,MNUMYR)*(1.01)**(M-MNUMYR)
!            XOGWPRNGPF(2,R,M) = OGWPRNG(R,MNUMYR)*(1.01)**(M-MNUMYR)
!            XDCRDWHP(R,M) = DCRDWHP(R,MNUMYR)*(1.01)**(M-MNUMYR)
!            XRFQTDCRD(R,M) = RFQTDCRD(R,MNUMYR)*(1.01)**(M-MNUMYR)
!          ENDDO
!        ENDDO
!************************************************************************************************************************

            WRITE(BUGOUT,*) '***************************************'
            WRITE(BUGOUT,*) 'L48HYR = ', l48hyr+baseyr-1, '  TECHYR = ', techyr+baseyr-1
            WRITE(BUGOUT,*) '***************************************'
            CALL OGINIT_HIST  ! READ LOWER 48 HISTORICAL DATA
            CALL OGINIT_AK    ! READ ALASKA DATA
            CALL OGINIT_OFF   ! READ OFFSHORE DATA
!!!            CALL OGINIT_EOR   ! READ EOR DATA
!!!  added oginit_eor to end of oginit_bfw

!  RESET PRICES FOR ELASTICITY CHECK RUNNING STANDALONE
        IT_WOP(L48HYR+1:IJUMPYR,:)        = (1.0-ogrunop(6)/100.)*IT_WOP(L48HYR+1:IJUMPYR,:)
        BRENT_PRICE(L48HYR+1:IJUMPYR)     = (1.0-ogrunop(6)/100.)*BRENT_PRICE(L48HYR+1:IJUMPYR)
        WTI_PRICE(L48HYR+1:IJUMPYR)       = (1.0-ogrunop(6)/100.)*WTI_PRICE(L48HYR+1:IJUMPYR)
        RFCRUDEWHP(:,:,L48HYR+1:IJUMPYR)  = (1.0-ogrunop(6)/100.)*RFCRUDEWHP(:,:,L48HYR+1:IJUMPYR)
        DCRDWHP(:,L48HYR+1:IJUMPYR)       = (1.0-ogrunop(6)/100.)*DCRDWHP(:,L48HYR+1:IJUMPYR)
        PLGINPF(:,L48HYR+1:IJUMPYR)       = (1.0-ogrunop(6)/100.)*PLGINPF(:,L48HYR+1:IJUMPYR)
        OGPRCNG(:,L48HYR+1:IJUMPYR)       = (1.0-ogrunop(7)/100.)*OGPRCNG(:,L48HYR+1:IJUMPYR)
        OGWPRNG(:,L48HYR+1:IJUMPYR)       = (1.0-ogrunop(7)/100.)*OGWPRNG(:,L48HYR+1:IJUMPYR)
        OGHHPRNG(L48HYR+1:IJUMPYR)        = (1.0-ogrunop(7)/100.)*OGHHPRNG(L48HYR+1:IJUMPYR)
        IT_WOP(L48HYR+1:IJUMPYR,:)        = (1.0+ogrunop(8)/100.)*IT_WOP(L48HYR+1:IJUMPYR,:)
        BRENT_PRICE(L48HYR+1:IJUMPYR)     = (1.0+ogrunop(8)/100.)*BRENT_PRICE(L48HYR+1:IJUMPYR)
        WTI_PRICE(L48HYR+1:IJUMPYR)       = (1.0+ogrunop(8)/100.)*WTI_PRICE(L48HYR+1:IJUMPYR)
        RFCRUDEWHP(:,:,L48HYR+1:IJUMPYR)  = (1.0+ogrunop(8)/100.)*RFCRUDEWHP(:,:,L48HYR+1:IJUMPYR)
        DCRDWHP(:,L48HYR+1:IJUMPYR)       = (1.0+ogrunop(8)/100.)*DCRDWHP(:,L48HYR+1:IJUMPYR)
        PLGINPF(:,L48HYR+1:IJUMPYR)       = (1.0+ogrunop(8)/100.)*PLGINPF(:,L48HYR+1:IJUMPYR)
        OGPRCNG(:,L48HYR+1:IJUMPYR)       = (1.0+ogrunop(9)/100.)*OGPRCNG(:,L48HYR+1:IJUMPYR)
        OGWPRNG(:,L48HYR+1:IJUMPYR)       = (1.0+ogrunop(9)/100.)*OGWPRNG(:,L48HYR+1:IJUMPYR)
        OGHHPRNG(L48HYR+1:IJUMPYR)        = (1.0+ogrunop(9)/100.)*OGHHPRNG(L48HYR+1:IJUMPYR)

            IF (L48BYR.GT.1900) L48BYR = L48BYR - BASEYR + 1
            IF (OFFBYR.GT.1900) OFFBYR = OFFBYR - BASEYR + 1

!  DETERMINE MACRO VARIABLES
            BAA = 0.
            T10YR = 0.
            GDPCH = 0.
            DO M=techyr,mnumyr
              BAA = BAA + MC_RMCORPBAA(m)
              T10YR = T10YR + MC_RMTCM10Y(m)
              GDPCH = GDPCH + MC_JPGDP(m)/MC_JPGDP(m-1)
            WRITE(BUGOUT,*) 'mac_dcf', m+baseyr-1, MC_RMCORPBAA(m), MC_RMTCM10Y(m), MC_jpgdp(m)/mc_jpgdp(m-1)
            ENDDO
            BAA = BAA / (mnumyr-techyr+1)
            T10YR = T10YR / (mnumyr-techyr+1)
            GDPCH = GDPCH / (mnumyr-techyr+1)
            WACC = DEBTRATIO*BAA*(1-FEDTXR) + (1.-DEBTRATIO)*(t10yr+OGBETA*OGMRP)
            WACC_REAL = (1.+wacc/100.)/gdpch -1.
            WRITE(BUGOUT,*) 'mac_dcfavg(baa,t10yr,gdpch)', baa, t10yr, gdpch, wacc
            WRITE(BUGOUT,*) 'mac_dcfavg(wacc,wacc_real) ', wacc, wacc_real

!  INITIALIZE CRUDE OIL REPORT VARIABLE
            OGQCRREP = 0.
!  INITIALIZE PROVED RESERVES REPORT VARIABLE
            OGEOYRSV = 0.
!  INVOKE ROUTINE TO INITIALIZE RESERVES, P/R RATIOS, & ELASTICITIES
            CALL OGINIT_RES
!  INITIALIZE LAGGED WELLS
!           DO I = 1,L48WELL
!           DO R = 1,L48RGN
!           DO K = 1,L48FUEL
!             WELLAGL48(I,R,K) = &
!                 HISTWELL48(I,R,K,L48BYR-1)
!             WELLAG2L48(I,R,K) = &
!                 HISTWELL48(I,R,K,L48BYR-2)
!             WELLAG3L48(I,R,K) = &
!                 HISTWELL48(I,R,K,L48BYR-2)
!           ENDDO
!           ENDDO
!           ENDDO

!  INITIALIZE TECHNOLOGY ADJUSTMENT FACTORS FOR TECH SIDE CASES
            DO K = 1,L48FUEL                 ! ONSHORE CONVENTIONAL & UNCONVENTIONAL
             DO J = 1,5
              TECHADJ_ON(K,J) = 1.00
             ENDDO
            ENDDO
            DO K = 1,OFFFUEL                 ! OFFSHORE CONVENTIONAL
             DO J = 1,5
              TECHADJ_OFF(K,J) = 1.00
             ENDDO
            ENDDO
            DO K = 1,AKFUEL                  ! ALASKA
             DO J = 1,5
              TECHADJ_AK(K,J) = 1.00
             ENDDO
            ENDDO

            IF (OGRUNOP(2).EQ.0.OR.OGRUNOP(2).EQ.23) THEN   ! SLOW TECHNOLOGY
              DO K = 1,CONFUEL               ! ONSHORE CONVENTIONAL
               DO J = 1,5
                TECHADJ_ON(K,J) = 1.0 - ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = CONFUEL+1,L48FUEL       ! ONSHORE UNCONVENTIONAL
               DO J = 1,5
                TECHADJ_ON(K,J) = 1.0 - ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = 1,OFFFUEL               ! OFFSHORE CONVENTIONAL
               DO J = 1,5
                TECHADJ_OFF(K,J) = 1.0 - ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = 1,AKFUEL                ! ALASKA
               DO J = 1,5
                TECHADJ_AK(K,J) = 1.0 - ogrunop(11)/100.0
               ENDDO
              ENDDO
            ENDIF

            IF (OGRUNOP(2).EQ.2.OR.OGRUNOP(2).EQ.30) THEN   ! RAPID TECHNOLOGY
              DO K = 1,CONFUEL               ! ONSHORE CONVENTIONAL
               DO J = 1,5
                TECHADJ_ON(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = CONFUEL+1,L48FUEL       ! ONSHORE UNCONVENTIONAL
               DO J = 1,5
                TECHADJ_ON(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = 1,OFFFUEL               ! OFFSHORE CONVENTIONAL
               DO J = 1,5
                TECHADJ_OFF(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = 1,AKFUEL                ! ALASKA
               DO J = 1,5
                TECHADJ_AK(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
            ENDIF

            IF (OGRUNOP(2).EQ.3) THEN   ! ACCELERATED DEPLETION
              DO K = CONFUEL+1,L48FUEL       ! ONSHORE UNCONVENTIONAL
                TECHADJ_ON(K,3) = 1.0 - ogrunop(11)/100.0
                TECHADJ_ON(K,4) = 1.0 - ogrunop(11)/100.0
                TECHADJ_ON(K,5) = 1.0 - ogrunop(11)/100.0
              ENDDO
            ENDIF

            IF (OGRUNOP(2).EQ.4) THEN   ! ACCELERATED DEPLETION W/ SLOW TECHNOLOGY
              DO K = 1,CONFUEL               ! ONSHORE CONVENTIONAL
               DO J = 1,5
                TECHADJ_ON(K,J) = 1.0 - ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = CONFUEL+1,L48FUEL       ! ONSHORE UNCONVENTIONAL
               DO J = 1,5
                TECHADJ_ON(K,J) = 1.0 - ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = 1,OFFFUEL               ! OFFSHORE CONVENTIONAL
               DO J = 1,5
                TECHADJ_OFF(K,J) = 1.0 - ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = 1,AKFUEL                ! ALASKA
               DO J = 1,5
                TECHADJ_AK(K,J) = 1.0 - ogrunop(11)/100.0
               ENDDO
              ENDDO
            ENDIF

            IF (OGRUNOP(2).EQ.5) THEN   ! ACCELERATED DEPLETION W/ HIGH ACCESS
              DO K = CONFUEL+1,L48FUEL       ! ONSHORE UNCONVENTIONAL
                TECHADJ_ON(K,3) = 1.0 - ogrunop(11)/100.0
                TECHADJ_ON(K,4) = 1.0 + ogrunop(11)/100.0
                TECHADJ_ON(K,5) = 1.0 - ogrunop(11)/100.0
              ENDDO
            ENDIF

            IF (OGRUNOP(2).EQ.6) THEN   ! ACCELERATED DEPLETION W/ IMPROVED PRODUCTIVITY TECHNOLOGY
              DO K = 1,CONFUEL               ! ONSHORE CONVENTIONAL
                TECHADJ_ON(K,3) = 1.0 + ogrunop(11)/100.0
              ENDDO
              DO K = CONFUEL+1,L48FUEL       ! ONSHORE UNCONVENTIONAL
                TECHADJ_ON(K,3) = 1.0 + ogrunop(11)/100.0
                TECHADJ_ON(K,4) = 1.0 - ogrunop(11)/100.0
                TECHADJ_ON(K,5) = 1.00
              ENDDO
              DO K = 1,OFFFUEL               ! OFFSHORE CONVENTIONAL
                TECHADJ_OFF(K,3) = 1.0 + ogrunop(11)/100.0
              ENDDO
              DO K = 1,AKFUEL                ! ALASKA
                TECHADJ_AK(K,3) = 1.0 + ogrunop(11)/100.0
              ENDDO
            ENDIF

            IF (OGRUNOP(2).EQ.7) THEN   ! ACCELERATED DEPLETION W/ IMP PROD TECHNOLOGY & HIGH ACCESS
              DO K = 1,CONFUEL               ! ONSHORE CONVENTIONAL
                TECHADJ_ON(K,3) = 1.0 + ogrunop(11)/100.0
              ENDDO
              DO K = CONFUEL+1,L48FUEL       ! ONSHORE UNCONVENTIONAL
                TECHADJ_ON(K,3) = 1.0 + ogrunop(11)/100.0
                TECHADJ_ON(K,4) = 1.0 + ogrunop(11)/100.0
                TECHADJ_ON(K,5) = 1.00
              ENDDO
              DO K = 1,OFFFUEL               ! OFFSHORE CONVENTIONAL
                TECHADJ_OFF(K,3) = 1.0 + ogrunop(11)/100.0
              ENDDO
              DO K = 1,AKFUEL                ! ALASKA
                TECHADJ_AK(K,3) = 1.0 + ogrunop(11)/100.0
              ENDDO
            ENDIF

            IF (OGRUNOP(2).EQ.8) THEN   ! ACCELERATED DEPLETION W/ RAPID TECHNOLOGY & HIGH ACCESS
              DO K = 1,CONFUEL               ! ONSHORE CONVENTIONAL
               DO J = 1,5
                TECHADJ_ON(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = CONFUEL+1,L48FUEL       ! ONSHORE UNCONVENTIONAL
               DO J = 1,5
                TECHADJ_ON(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = 1,OFFFUEL               ! OFFSHORE CONVENTIONAL
               DO J = 1,5
                TECHADJ_OFF(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = 1,AKFUEL                ! ALASKA
               DO J = 1,5
                TECHADJ_AK(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
            ENDIF

            IF (OGRUNOP(2).EQ.9) THEN   ! ACCELERATED DEPLETION W/ RAPID TECHNOLOGY
              DO K = 1,CONFUEL               ! ONSHORE CONVENTIONAL
               DO J = 1,5
                TECHADJ_ON(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = CONFUEL+1,L48FUEL       ! ONSHORE UNCONVENTIONAL
               DO J = 1,5
                TECHADJ_ON(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
               TECHADJ_ON(K,4) = 1.0 - ogrunop(11)/100.0
              ENDDO
              DO K = 1,OFFFUEL               ! OFFSHORE CONVENTIONAL
               DO J = 1,5
                TECHADJ_OFF(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
              DO K = 1,AKFUEL                ! ALASKA
               DO J = 1,5
                TECHADJ_AK(K,J) = 1.0 + ogrunop(11)/100.0
               ENDDO
              ENDDO
            ENDIF

            IF (OGRUNOP(2).EQ.10) THEN   ! ACCELERATED DEPLETION W/ REDUCED PRODUCTIVITY TECHNOLOGY
              DO K = 1,CONFUEL               ! ONSHORE CONVENTIONAL
                TECHADJ_ON(K,3) = 1.0 - ogrunop(11)/100.0
              ENDDO
              DO K = CONFUEL+1,L48FUEL       ! ONSHORE UNCONVENTIONAL
                TECHADJ_ON(K,3) = 1.0 - ogrunop(11)/100.0
                TECHADJ_ON(K,4) = 1.0 - ogrunop(11)/100.0
                TECHADJ_ON(K,5) = 1.00
              ENDDO
              DO K = 1,OFFFUEL               ! OFFSHORE CONVENTIONAL
                TECHADJ_OFF(K,3) = 1.0 - ogrunop(11)/100.0
              ENDDO
              DO K = 1,AKFUEL                ! ALASKA
                TECHADJ_AK(K,3) = 1.0 - ogrunop(11)/100.0
              ENDDO
            ENDIF

!  INITIALIZE FLOW RATE ADJUSTMENT FACTORS
!           DO I=1,L48WELL
!             DO J=1,L48RGN
!               DO K=1,L48FUEL
!                 FRATL48(I,J,K) = 1.0
!               ENDDO
!             ENDDO
!           ENDDO
            DO I=1,OFFWELL
              DO J=1,OFFRGN
                DO K=1,OFFFUEL
                  FRATOFF(I,J,K) = 1.0
                ENDDO
              ENDDO
            ENDDO
!  INVOKE ROUTINE TO INITIALIZE EXPORTS

         ENDIF   ! END IF FIRST YEAR AND FIRST ITERATION

         IF (CURITR .EQ. 1) THEN
           IF (OGRUNOP(1).EQ.1) THEN
             CALL OGINIT_PRICE2  ! USE OUR OWN INTERNAL PRICES
           ELSE
             CALL OGINIT_PRICE   ! USE NEMS COMPUTED PRICES
           ENDIF
           CALL OGMAIN_ON
           IF (CURIYR.GE.OFFBYR) CALL OGMAIN_OFF
         ENDIF


! <<<<<<<<<<<CALL EVERY ITERATION >>>>>>>>>>
!  OVERWRITE DISCOUNT RATE WITH AA UTILITY BOND RATE FROM THE MACRO MODEL
         DISC = WACC_REAL + 0.05 ! cost of capital + required return over cost of capital

!==========================> HSM Code Start <==========================
         if (hsm_discount_rate_first) Then
            write(hsm_disc_rate   , '(*(G0.16,:,","))') 'corp_bond_rate', 'debt_ratio', 'fed_tax_rate', &
            'ten_year_treas_note_yield', 'market_risk_premium', 'industry_beta', 'expected_inflation_rate', &
            'return_over_capital_cost', 'discount_rate'
            write(hsm_disc_rate, '(*(G0.16,:,","))') BAA, debtratio, fedtxr, t10yr, ogmrp, ogbeta, gdpch, 0.05, disc
            hsm_discount_rate_first = .false.
         endif
!===========================> HSM Code End <===========================

         gdp87 = mc_jpgdp(curiyr)/mc_jpgdp(-2)
         dladj = mc_jpgdp(nom_year-baseyr+1)/mc_jpgdp(-2)
!  calculate inflation rate
         if (curiyr+baseyr-1.gt.1990) &
             infl = mc_jpgdp(curiyr)/mc_jpgdp(curiyr-1) - 1
         write (bugout,*) 'mac_inputs',curiyr+1989,gdp87,infl,disc,dladj
         IF (CURIYR .GE. AKBYR) THEN      ! em4 1/27/98
           CALL OGFOR_AK   ! (DH5 05/03/96) ALASKA PRODUCTION
         ENDIF
!        CALL OGOUT_EOR   ! REPORT CO2 EOR PRODUCTION
         IF (CURITR.EQ.1) CALL OGFOR_OS       ! DETERMINE OIL SHALE PRODUCTION
         CALL OG_OILPRD  ! (DH5 08/18/97) DOMESTIC CRUDE OIL PRODUCTION
         CALL OGCOMP_AD
         OGGROWFAC(CURIYR) = OG_GROWTH(CURIYR,-1)   ! SET FOR NGMM TO USE
         WRITE(BUGOUT,*) 'ugrprd', CURIYR+1989, PRDL48(5,5), PRDL48(5,6), PRDL48(5,7)

        DO I=1,OGDIST
          OGOILPRD(I,oiltypes,CURIYR) = sum(OGOILPRD(I,1:oiltypes-1,CURIYR)) 
          OGADGPRD(I,oiltypes,CURIYR) = sum(OGADGPRD(I,1:oiltypes-1,CURIYR)) 
          DO K=1,gastypes-1
            if(OGENAGPRD(I,K,CURIYR)<0.) OGENAGPRD(I,K,CURIYR) = 0.
          ENDDO
          OGENAGPRD(I,gastypes,CURIYR) = sum(OGENAGPRD(I,1:gastypes-1,CURIYR))
          OGNGPLPRD(I,curiyr) = OGNGPLET(I,curiyr) + OGNGPLPR(I,curiyr) +  &
               OGNGPLBU(I,curiyr) + OGNGPLIS(I,curiyr) + OGNGPLPP(I,curiyr)
        ENDDO
        CALL OG_GASPRD  ! SPLIT GAS PRODUCTION BY TYPE FOR PMM TO USE
!       WRITE(6,*) 'dh5out',curiyr+1989,curitr,sum(ogenagprd(1:OGDIST,gastypes,curiyr)),sum(ogadgprd(1:OGDIST,gastypes,curiyr))

      ENDIF  ! ENDIF FOR REPORTING STEP PROCESS

!%    ENDDO  ! END TESTING LOOP


      CALL OGCANADA        ! CALL THE CANADIAN SUPPLY MODULE


!  IF END OF THE LAST YEARS RUN & REPORT STEP THEN CLOSE OUTPUT FILE
      IF (CURIYR .EQ. LOOPIT .AND. NCRL .EQ. 1) THEN

!  MOVE PACIFIC REGION TO POSITION 7 and NORTHERN GREAT PLAINS TO POSITION 6 for FTAB
      DO M=1,MNUMYR
        DO R=1,MNUMOR
          DO K=1,MNCRUD
            PRDTEMP(R,K,M) = OGCRDPRD(R,K,M)
          ENDDO
        ENDDO
        DO K=1,MNCRUD
          OGCRDPRD(6,K,M) = PRDTEMP(7,K,M)
          OGCRDPRD(7,K,M) = PRDTEMP(6,K,M)
        ENDDO
      ENDDO

!  INVOKE SUBROUTINE TO GENERATE SUMMARY REPORTS
         CALL OGREP_DEMO

!  CLOSE DEBUG OUTPUT FILE IF IT WAS OPENED
         IF (PRTDBGL.EQ.1) THEN
            FNAME='WLDEBUG'
            NEW=.TRUE.
            SYSOUT = FILE_MGR('C',FNAME,NEW)
         ENDIF

!  CLOSE DEBUG OUTPUT FILE IF IT WAS OPENED
         IF (PRTDBGL.EQ.1) THEN
            FNAME='UGROUT'
            NEW=.TRUE.
            VAROUT = FILE_MGR('C',FNAME,NEW)
         ENDIF

!  CLOSE DEBUG OUTPUT FILE IF IT WAS OPENED
         IF (PRTDBGL.EQ.1) THEN
            FNAME='UGROUT'
            NEW=.TRUE.
            OGBUG1 = FILE_MGR('C',FNAME,NEW)
         ENDIF

!  CLOSE DEBUG OUTPUT FILE IF IT WAS OPENED
         IF (PRTDBGL.EQ.1) THEN
            FNAME='UGROUT'
            NEW=.TRUE.
            DBOUT = FILE_MGR('C',FNAME,NEW)
         ENDIF

!  CLOSE DEBUG OUTPUT FILE IF IT WAS OPENED
         IF (PRTDBGL.EQ.1) THEN
            FNAME='UGROUT'
            NEW=.TRUE.
            OFILE1 = FILE_MGR('C',FNAME,NEW)
         ENDIF

!  CLOSE DEBUG OUTPUT FILE IF IT WAS OPENED
         IF (PRTDBGL.EQ.1) THEN
            FNAME='UGROUT'
            NEW=.TRUE.
            OFILE2 = FILE_MGR('C',FNAME,NEW)
         ENDIF

!  CLOSE DEBUG OUTPUT FILE IF IT WAS OPENED
         IF (PRTDBGL.EQ.1) THEN
            FNAME='UGROUT'
            NEW=.TRUE.
            OFILE3 = FILE_MGR('C',FNAME,NEW)
         ENDIF
      ENDIF
      
!==========================> HSM Code Start <==========================
Close(unit=hsm_out      , status='keep')
if (hsm_dcf_bool) then
    Close(unit=hsm_dcf_off  , status='keep')
    Close(unit=hsm_dcf_on   , status='keep')
    Close(unit=hsm_dcf_ak   , status='keep')
    Close(unit=hsm_dcf_opts , status='keep')
    Close(unit=hsm_disc_rate , status='keep')
endif
if (hsm_offshore_bool) then
    Close(unit=hsm_offshore_disc  , status='keep')
endif
      if (hsm_restart_bool) call hsm_restart_results
!===========================> HSM Code End <===========================

      RETURN
      END

!********************************************************************
!      DEBUG SUBCHK,SUBTRACE
!      END DEBUG
      SUBROUTINE OGINIT_BFW
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! ogsm variables
      include'ogsmeor'      ! EOR variables
      include'cogen'        ! cogeneration
      include'ogsmout'      ! ogsm global output variables
      include'pmmout'       ! pmm global output variables
      include'macout'       ! macro global output variables
      include'steoblock'    ! steo global output variables
      include'ogsmcan'      ! canada variables

      INTEGER RTOVALUE      ! SYSTEM RUNTIME OPTION FUNCTION
      REAL TAXPREM(MNUMYR,2)    ! TAX PREMIUM (USE FOR READING IN ONLY)
      REAL OG_GROWTH
      character*750 cline
      character*46 dummy
      CHARACTER*14 CDUM
      character*8 DUMMY8
      character*40 DUMMY40
      character*49 DUMMY50
      INTEGER IDUM,TECHREADYR

      INTEGER*4 GR              ! INDEX
      INTEGER*4 FORYR_G         ! FIRST FORECAST YEAR FOR COGEN GAS CONS.
      INTEGER*4 FORYR           ! FIRST FORECAST YEAR FOR COGEN GENERATION (EMM SETS HISTORY)
      INTEGER*4 FORYR_C         ! FIRST FORECAST YEAR FOR COGEN CAPACITY
      INTEGER*4 NREAD


      INTEGER*4 CO2MAP(13)      !
      REAl*4 CGOGQ_CD9
      REAL*4 CGOGQ_CD9_S
      REAl*4 CGOGGEN_CD9_1
      REAL*4 CGOGGEN_CD9_2
      REAL*4 CGOGGEN_CD9_2S
      REAL*4 TIME, RATIO, EORADJ, EORCSTADJ
      REAL*4 UTIL_COGEN
      REAL*4 CONV               ! generation unit conversion betw input and output
      REAL*4 COAL(MNUMCR-2)     ! placeholder for coal cons, gen, or cap
      REAL*4 OTHER(MNUMCR-2)    ! placeholder for other cons, gen, or cap
      REAL*4 OIL(MNUMCR-2)      ! placeholder for oil cons, gen, or cap
      REAL*4 GAS(MNUMCR-2)      ! placeholder for gas cons, gen, or cap

!  INVOKE FILE_MGR TO OPEN INPUT FILE
!     FNAME='WLPDS(BASICFW)'
      FNAME='WLBASIC'
      NEW=.FALSE.
      IFILE3 = FILE_MGR('O',FNAME,NEW)

      TECHREADYR = IJUMPYR - 15 ! DATA BEGINS IN YEAR 2005

!  READ THE OGSM RUN OPTION FLAG VECTOR
      do M = 1,20
         call ognxtdat(ifile3,cline)
         READ (cline,*) OGRUNOP(M)
      enddo
!  SET RUNTIME OPTIONS
      OGRUNOP(1) = RTOVALUE('OGIPPR  ',OGRUNOP(1))
      OGRUNOP(2) = RTOVALUE('OGTECH  ',OGRUNOP(2))
      OGRUNOP(3) = RTOVALUE('OGHIST  ',OGRUNOP(3))
      OGRUNOP(4) = RTOVALUE('OGFORE  ',OGRUNOP(4))
      OGRUNOP(5) = RTOVALUE('AIMMSNG ',OGRUNOP(5))
      OGRUNOP(10) = RTOVALUE('OGMEXICO',OGRUNOP(10))
      OGRUNOP(11) = RTOVALUE('OGTECADJ',OGRUNOP(11))
      OGRUNOP(16) = RTOVALUE('OGACCESS',OGRUNOP(16))
      OGRUNOP(13) = RTOVALUE('OGCONADJ',OGRUNOP(13))
      OGRUNOP(14) = RTOVALUE('OGUNCADJ',OGRUNOP(14))
      OGRUNOP(17) = RTOVALUE('CCOST   ',OGRUNOP(17))
      OGRUNOP(18) = RTOVALUE('OGOILOPT',OGRUNOP(18))
      OGRUNOP(21) =RTOVALUE('LEGIRA  ', 1) ! IRA FLAG


!  READ THE OGSM REPORT FLAG VECTOR
      do M = 1,30
         call ognxtdat(ifile3,cline)
         READ (cline,*) OGREPORT(M)
      enddo

!  READ TECHNOLOGY YEAR
      call ognxtdat(ifile3,cline)
      READ (cline,*) TECHYR
      IF (TECHYR.GT.1900) TECHYR = TECHYR - BASEYR + 1
      IF (TECHYR.LE.L48HYR) TECHYR = L48HYR + 1

!  READ FEDERAL TAX RATE
      call ognxtdat(ifile3,cline)
      READ (cline,*) FEDTXR

!  READ ROYALTY RATE
      call ognxtdat(ifile3,cline)
      READ (cline,*) ROYRT
      IF (OGRUNOP(21).GE.1) ROYRT = 0.16667


!  READ DISCOUNT RATE - NOT USED
!  OVERWRITEN WITH THE AA UTILITY BOND RATE FROM THE MACRO MODEL
      call ognxtdat(ifile3,cline)
      READ (cline,*) DISC

!  READ INFLATION RATE
      call ognxtdat(ifile3,cline)
      READ (cline,*) INFL

!  READ LONG-TERM DEBT RATIO
      call ognxtdat(ifile3,cline)
      READ (cline,*) DEBTRATIO

!  READ INDUSTRY BETA
      call ognxtdat(ifile3,cline)
      READ (cline,*) OGBETA

!  READ MARKET RISK PREMIUM
      call ognxtdat(ifile3,cline)
      READ (cline,*) OGMRP

!  READ FACTORS FOR SETTING EXPECTED PRICES OFF OF LAST YEAR'S PRICE
      do i = 1,IJUMPYR
         call ognxtdat(ifile3,cline)
         READ (cline,*)J,OGPRCEXP(I)
         OGGROWFAC(I) = OG_GROWTH(I,-1)
      enddo

!  READ TAX PREMIUM, ASSIGN TO GLOBAL VARIABLE OGTAXPREM
      do M = 1,IJUMPYR
         call ognxtdat(ifile3,cline)
         READ (cline,*)   (TAXPREM(M,K),K=1,2)
      enddo
       DO M=1,IJUMPYR
       DO K=1,2
         OGTAXPREM(K,M) = TAXPREM(M,K)
       ENDDO
       ENDDO

!  READ AD GAS ECONOMETRIC PARAMETERS
!      do R = 1,9
!         call ognxtdat(ifile3,cline)
!         READ (cline,*) alpha_ad(r),beta_ad(r)
!      enddo

!  READ VARIABLES FOR ROCKY MOUNTAIN ACCESS STATUS
      call ognxtdat(ifile3,cline)
      READ(cline,*) ACCESS_YR
      DO I = 1,L48WELL
        DO K = 1,CONFUEL
          call ognxtdat(ifile3,cline)
          READ(cline,*) NAC_CONV(I,K),LST_CONV(I,K),STL_CONV(I,K)
        ENDDO
      ENDDO
!  SET ACCESS
      ACCESS = OGRUNOP(16)

!  READ ONSHORE BASE YEAR
      call ognxtdat(ifile3,cline)
      READ (cline,*) L48BYR

!  READ MAPPING(PADD->MNUMOR-1)
     do R = 1,MNUMOR-1
        call ognxtdat(ifile3,cline)
        READ (cline,*) LFMMMAP(R)
     enddo

!***************************************************************
!from OGINIT_EOR.F
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!  PURPOSE:  CALLED BY WELLOGS TO CALCULATE EOR RELATED VALUES
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


!  READ HISTORICAL VALUES

!  Read Equations' parameters

      call ognxtdat(ifile3,cline)
      READ(CLINE,*) CGOGQ_CD9, CGOGQ_CD9_S, CGOGGEN_CD9_1, CGOGGEN_CD9_2, CGOGGEN_CD9_2S

!  Read average utilization rate for electric generation

      call ognxtdat(ifile3,cline)
      READ(CLINE,*) UTIL_COGEN


!  CONSUMPTION

      call ognxtdat(ifile3,cline)
      READ(CLINE,*) FORYR_G
      FORYR_G = FORYR_G - BASEYR + 1

      CGOGSQ = 0.0
      DO I=1,4
        DO J=1,FORYR_G-1
          call ognxtdat(ifile3,cline)
          READ(cline,*) K,N,(CGOGSQ(L,J,I),L=1,MNUMCR-2)
          K = K - BASEYR + 1
          IF (K.NE.J .OR. N.NE.I) WRITE(6,*) ' CONS READ PROBLEM IN EOR HIST'
        ENDDO
      ENDDO

!   GENERATION

      call ognxtdat(ifile3,cline)
      READ(CLINE,*) FORYR
      FORYR = FORYR - BASEYR + 1

      CGOGSGEN = 0.0
      DO GR=1,2
        DO I=1,4
          DO J=1,FORYR-1
            call ognxtdat(ifile3,cline)
            READ(cline,*) K,N,(CGOGSGEN(L,J,I,GR),L=1,MNUMCR-2)
            K = K - BASEYR + 1
            IF (K.NE.J .OR. N.NE.I) WRITE(6,*) ' GEN READ PROBLEM IN EOR HIST',K,J,N,I
          ENDDO
        ENDDO
      ENDDO

! CAPACITY

      call ognxtdat(ifile3,cline)
      READ(CLINE,*) FORYR_C
      FORYR_C = FORYR_C - BASEYR + 1

      CGOGSCAP = 0.0
      DO I=1,4
        DO J=1,FORYR_C-1
          call ognxtdat(ifile3,cline)
          READ(cline,*) K,N,(CGOGSCAP(L,J,I),L=1,MNUMCR-2)
          K = K - BASEYR + 1
          IF (K.NE.J .OR. N.NE.I) WRITE(6,*) ' CAP READ PROBLEM IN EOR HIST'
        ENDDO
      ENDDO
      
!***************************************************************
!
!Reading in parameters for oil shale submodule (OGFOR_OS)
!  
!***************************************************************  
      
!  READ IN DATA from wlbasic.txt
      call OGSM_NEXTDATA2(ifile3)	! read until @ in second column
      
      read(ifile3,*) OS_GALLON_TON	! Oil shale per ton	
      read(ifile3,*) OS_PRJ_SIZE	! plant size
      read(ifile3,*) OS_CONV_EFF	! conversion efficiency
      read(ifile3,*) OS_MINE_CST_TON	! underground mining costs
      read(ifile3,*) OS_PLANT_INVEST	! retorting and upgrading plant costs
      read(ifile3,*) OS_CAP_FACTOR	! plant capacity factor
      read(ifile3,*) OS_PLANT_OPER_CST	! plant operating costs
      read(ifile3,*) OS_ROYAL_RATE	! royalty rate
      IF (OGRUNOP(21).GE.1) OS_ROYAL_RATE = 0.1667
      read(ifile3,*) OS_CORP_TAX_RATE	! corp tax rate
      read(ifile3,*) OS_EQUITY_SHARE	! equity % of total capital structure
  
      call OGSM_NEXTDATA2(ifile3)	! read until @ in second column
      
      read(ifile3,*) OS_EQUITY_VOL	! equity volatility beta
      
      call OGSM_NEXTDATA2(ifile3)	! read until @ in second column

      read(ifile3,*) OS_EQUITY_PREMIUM	! equity market risk premium
      
      call OGSM_NEXTDATA2(ifile3)	! read until @ in second column
      
      read(ifile3,*) OS_DEBT_PREMIUM	! debt risk premium
      read(ifile3,*) OS_GAS_PROD	! natural gas price for production
      read(ifile3,*) OS_ELEC_CONSUMP	! electricity consumption/yr
      read(ifile3,*) OS_CO2EMISS	! CO2 emissions
      read(ifile3,*) OS_MAX_PROD	! max production
      read(ifile3,*) OS_MAX_BLD	! max # plants built
      read(ifile3,*) OS_PRJ_LIFE	! project life
      
      call OGSM_NEXTDATA2(ifile3)	! read until @ in second column
     
      read(ifile3,*) OS_PRJ_CONST	! project construction period
      read(ifile3,*) OS_START_YR	! earliest calendar yr oil shale project can be built
      read(ifile3,*) OS_PENETRATE_YR	! max # yrs to reach full penetration

!  READ ETHANE REJECTION
      call OGSM_NEXTDATA2(ifile3)	! read until @ in second column
      do i = 1,IJUMPYR
         READ (ifile3,*) idum,(etrejfac(r,i),r=1,MNL48N), (ethanemax(r,i),r=1,MNL48N)
      enddo

!  READ ELASTICITY VALUES BY REGION/FUEL
      do R = 1,OGDIST
         call ognxtdat(ifile3,cline)
         READ (cline,*) idum, dummy8, (DISTMAP(R,K),K=1,7)
      enddo

!  READ STEO CRUDE OIL WELLHEAD PRICES AND PRODUCTION
      do M = l48hyr+1,l48hyr+3
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, (DCRDWHP(I,M),I=1,MNUMOR)
        if(wtipuus(m) <= 0.) WTIPUUS(M) = DCRDWHP(MNUMOR,M)*mc_jpgdp(1987-baseyr+1)/mc_jpgdp(M)  ! convert from nominal dollars to 87 dollars
      enddo
      do M = l48hyr+1,l48hyr+3
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, (RFQTDCRD(I,M),I=1,MNUMOR+1)  ! FED_GOM is saved in position mnumor (=13) and other L48 in position 14
        IF (PAPRPAK(M) <= 0..or.m.eq.l48hyr+3) PAPRPAK(M)=RFQTDCRD(mnumor-1,M)+RFQTDCRD(mnumor-2,M)
        IF (PAPRPGLF(M) <= 0..or.m.eq.l48hyr+3) PAPRPGLF(M)=RFQTDCRD(mnumor,M)
        IF (PAPR48NGOM(M) <= 0..or.m.eq.l48hyr+3) PAPR48NGOM(M)=RFQTDCRD(mnumor+1,M)
      enddo

!   READ PLAY-LEVEL PRODUCTION
      tOGQSHLOIL = 0.
      tOGQSHLGAS = 0.
      do M = 1,3
        call ognxtdat(ifile3,cline)
        READ (cline,*) dummy, (tOGQSHLOIL(I,M),I=1,10),tOGQSHLOIL(15,M)
      enddo
      do M = 1,3
        call ognxtdat(ifile3,cline)
        READ (cline,*) dummy, (tOGQSHLGAS(I,M),I=1,9),tOGQSHLGAS(15,M)
      enddo

!   READ STEO NGPL COMPOSITION (MBD)
      do M = l48hyr+1,l48hyr+3   ! total ngpls
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, (ngpl16(I,M),I=1,16)
        IF (NLPRPUS(M) <= 0.) NLPRPUS(M)=NGPL16(1,M)/1000.
      enddo
      do M = l48hyr+1,l48hyr+3   ! total ethane
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, (ethane16(I,M),I=1,16)
        IF (ETFPPUS(M) <= 0.) ETFPPUS(M)=ETHANE16(1,M)/1000.
      enddo
      do M = l48hyr+1,l48hyr+3   ! total propane
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, (propane16(I,M),I=1,16)
        IF (PRFPPUS(M) <= 0.) PRFPPUS(M)=PROPANE16(1,M)/1000.
      enddo
      do M = l48hyr+1,l48hyr+3   ! total butane
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, (butane16(I,M),I=1,16)
      enddo
      do M = l48hyr+1,l48hyr+3   ! total isobutane
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, (isobutane16(I,M),I=1,16)
        IF (C4FPPUS(M) <= 0.) C4FPPUS(M)=(BUTANE16(1,M)+ISOBUTANE16(1,M))/1000.
      enddo
      do M = l48hyr+1,l48hyr+3   ! pentanes plus
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, (pentanes16(I,M),I=1,16)
        IF (PPFPPUS(M) <= 0.) PPFPPUS(M)=PENTANES16(1,M)/1000.
      enddo

!  USE GLOBAL STEO VARIABLES TO ADJUST STEO OVERWRITES
      call ognxtdat(ifile3,cline)
      READ (cline,*) STEOYRS
      call ognxtdat(ifile3,cline)
      READ (cline,*) ogSTEO
      do M = 1,3
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, (sDCRDWHP(I,M),I=1,MNUMOR-1)
      enddo
      do M = 1,3
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, (sRFQTDCRD(I,M),I=1,MNUMOR-1)
      enddo
      do M = 1,3
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, sNGPLPRD(M),sNGPLET(M),sNGPLPR(M),sNGPLBU(M),sNGPLIS(M),sNGPLPP(M)
      enddo
      do M = 1,OGDIST
        call ognxtdat(ifile3,cline)
        READ (cline,*) idum, sOGENAGPRD(M,1),sOGENAGPRD(M,2),sOGADGPRD(M,1),sOGADGPRD(M,2)
      enddo
      call ognxtdat(ifile3,cline)
      READ (cline,*) idum, (sCNADGPRD(M),M=1,NUMCAN),(sCNNAGPRD(M),M=1,NUMCAN)

      if (ogSTEO.eq.0) then
        sDCRDWHP = 0.0
        sRFQTDCRD= 0.0
        sNGPLPRD=0.0
        sNGPLET= 0.0
        sNGPLPR= 0.0
        sNGPLBU= 0.0
        sNGPLIS= 0.0
        sNGPLPP= 0.0
      endif
        do M = l48hyr+1,l48hyr+steoyrs 
          N = N+1
          DO I=1,MNUMOR-1
            DCRDWHP(I,M)=DCRDWHP(I,M)*WTIPUUS(M)/(DCRDWHP(mnumor,M)*mc_jpgdp(1987-baseyr+1)/mc_jpgdp(M)) ! convert from nominal dollars to 87 dollars
          ENDDO
          RFQTDCRD(mnumor-2,M)=(PAPRPAK(M)-RFQTDCRD(mnumor-1,M))
          RFQTDCRD(L48RGN+2,M)=RFQTDCRD(L48RGN+2,M) + (PAPRPGLF(M)-RFQTDCRD(mnumor,M))
          DO I=1,L48RGN
            RFQTDCRD(I,M)=RFQTDCRD(I,M)*PAPR48NGOM(M)/RFQTDCRD(mnumor+1,M)
          ENDDO
          DO I=1,16
            NGPL16(I,M) = NGPL16(I,M)*NLPRPUS(M)*1000./NGPL16(1,M)
            ETHANE16(I,M) = ETHANE16(I,M)*ETFPPUS(M)*1000./ETHANE16(1,M)
            PROPANE16(I,M) = PROPANE16(I,M)*PRFPPUS(M)*1000./PROPANE16(1,M)
            BUTANE16(I,M) = BUTANE16(I,M)*C4FPPUS(M)*1000./(BUTANE16(1,M)+ISOBUTANE16(1,M))
            ISOBUTANE16(I,M) = ISOBUTANE16(I,M)*C4FPPUS(M)*1000./(BUTANE16(1,M)+ISOBUTANE16(1,M))
            PENTANES16(I,M) = PENTANES16(I,M)*PPFPPUS(M)*1000./PENTANES16(1,M)
          ENDDO
        enddo

!   ADJUST HISTORICAL LOWER 48 ONSHORE PRODUCTION TO EXCLUDE EOR
      do M = l48hyr+1,l48hyr+steoyrs
        DO I=1,MNUMOR
          DCRDWHP(I,M)=DCRDWHP(I,M)*mc_jpgdp(1987-baseyr+1)/mc_jpgdp(M) ! convert from nominal dollars to 87 dollars
          XDCRDWHP(I,M)=DCRDWHP(I,M)
        ENDDO
        RFQDCRD(:,M) = RFQTDCRD(:,M)
        DO I=1,l48rgn
          IF(RFQTDCRD(I,L48HYR).GT.0.) RFQDCRD(I,M) = RFQTDCRD(I,M)*RFQDCRD(I,L48HYR)/RFQTDCRD(I,L48HYR)
        ENDDO
        DO I=1,l48rgn
          OGQEORPR(I,M) = (RFQTDCRD(I,M) - RFQDCRD(I,M))*365*1000
	  OGEORPRD(I,4,M) = OGEORPRD(I,4,M-1) * 1.02
        ENDDO
      enddo

!  CLOSE INPUT FILE IFILE3
      IFILE3 = FILE_MGR('C',FNAME,NEW)

! ---------------------------------------------------------------------------------------

! FORECAST EOR GAS CONS., ELECTRIC GENERATION, & ELECTRIC CAPACITY

! Forecast EOR natural gas consumption by cogen

      DO L=1,MNUMCR-2
        COAL(L) = (CGOGSQ(L,9,1) + CGOGSQ(L,10,1) &       ! coal & other to avg 98-00
                                + CGOGSQ(L,11,1))/3.0
        OTHER(L) = (CGOGSQ(L,9,4) + CGOGSQ(L,10,4) &
                                 + CGOGSQ(L,11,4))/3.0
        OIL(L) = CGOGSQ(L,11,2)                             ! oil to 2000
        GAS(L) = (CGOGSQ(L,10,3) + CGOGSQ(L,11,3)) / 2.0  ! gas to 00 & 01 avg (except reg9)
           IF (CGOGSQ(L,FORYR_G-1,1).EQ.0.0) COAL(L) = 0.0
           IF (CGOGSQ(L,FORYR_G-1,2).EQ.0.0) OIL(L) = 0.0
           IF (CGOGSQ(L,FORYR_G-1,3).EQ.0.0) GAS(L) = 0.0
           IF (CGOGSQ(L,FORYR_G-1,4).EQ.0.0) OTHER(L) = 0.0

      ENDDO

      DO J=FORYR_G,IJUMPYR  ! Year Index
        TIME = REAL(J+1)    ! Time starts from 14 (=2002) to 37
        DO L=1,MNUMCR-2     ! Census Division Index
          CGOGSQ(L,J,1) = COAL(L)
          CGOGSQ(L,J,2) = OIL(L)
          CGOGSQ(L,J,4) = OTHER(L)
          IF (L.EQ.9) THEN
            CGOGSQ(L,J,3) = CGOGQ_CD9 + CGOGQ_CD9_S * TIME
          ELSE
            CGOGSQ(L,J,3) = GAS(L)
          ENDIF
        ENDDO
      ENDDO

! Forecast EOR electric generation

      DO GR=1,2               ! 1 is grid; 2 is own use
        DO L=1,MNUMCR-2
          COAL(L) = (CGOGSGEN(L,9,1,GR) + CGOGSGEN(L,10,1,GR) &      ! coal & other to avg 98-00
                                  + CGOGSGEN(L,11,1,GR))/3.0
          OTHER(L) = (CGOGSGEN(L,9,4,GR) + CGOGSGEN(L,10,4,GR) &
                                   + CGOGSGEN(L,11,4,GR))/3.0
          OIL(L) = CGOGSGEN(L,11,2,GR)                               ! oil to 2000
          GAS(L) = (CGOGSGEN(L,10,3,GR) + CGOGSGEN(L,11,3,GR)) / 2.0 ! gas to 00 & 01 avg (except reg9)
           IF (CGOGSGEN(L,FORYR-1,1,GR).EQ.0.0) COAL(L) = 0.0
           IF (CGOGSGEN(L,FORYR-1,2,GR).EQ.0.0) OIL(L) = 0.0
           IF (CGOGSGEN(L,FORYR-1,3,GR).EQ.0.0) GAS(L) = 0.0
           IF (CGOGSGEN(L,FORYR-1,4,GR).EQ.0.0) OTHER(L) = 0.0

        ENDDO

        DO J=FORYR,IJUMPYR    ! Year Index
          TIME = REAL(J+1)    ! Time starts from 14 (=2002) to 37
          DO L=1,MNUMCR-2     ! Census Division Index
            CGOGSGEN(L,J,1,GR) = COAL(L)
            CGOGSGEN(L,J,2,GR) = OIL(L)
            CGOGSGEN(L,J,4,GR) = OTHER(L)
            IF (L.EQ.9) THEN
              IF (GR.EQ.1) CGOGSGEN(L,J,3,GR) = CGOGGEN_CD9_1
              IF (GR.EQ.2) CGOGSGEN(L,J,3,GR) = CGOGGEN_CD9_2 + &
                                  CGOGGEN_CD9_2S * TIME
            ELSE
              CGOGSGEN(L,J,3,GR) = GAS(L)
            ENDIF
          ENDDO
        ENDDO
      ENDDO

! Forecast EOR electric capacity

      DO J=FORYR_C,IJUMPYR   ! Year Index
        DO I =1,4            ! Fuel Index
          DO L=1,MNUMCR-2    ! Census Division Index
            CGOGSCAP(L,J,I) = CGOGSCAP(L,FORYR_C-1,I)
          ENDDO
        ENDDO
        CGOGSCAP(9,J,3) = (CGOGSGEN(9,J,3,1) + CGOGSGEN(9,J,3,2)) &
                                    / (365.*24.*UTIL_COGEN)
          if (J.GE.12.AND.J.LE.14) then
              CGOGSCAP(9,J,3) = CGOGSCAP(9,FORYR_C-1,3)
          endif
      ENDDO

! Convert units and calc national totals for output to NEMS

      DO I=1,4
        DO J=1,IJUMPYR
          DO L=1,MNUMCR-2
            CGOGSQ(L,J,I) = CGOGSQ(L,J,I) / 1000000.           ! Convert to Trillion Btu
            CGOGSQ(MNUMCR,J,I) = CGOGSQ(MNUMCR,J,I) + CGOGSQ(L,J,I)
            CGOGSCAP(L,J,I) = CGOGSCAP(L,J,I) / 1000.      ! Convert to megawatts
            CGOGSCAP(MNUMCR,J,I) = CGOGSCAP(MNUMCR,J,I) + CGOGSCAP(L,J,I)
            DO GR =1,2
              CGOGSGEN(L,J,I,GR) = CGOGSGEN(L,J,I,GR) / 1000000.  ! Convert to Gigawatt-hours
              CGOGSGEN(MNUMCR,J,I,GR) = CGOGSGEN(MNUMCR,J,I,GR) + CGOGSGEN(L,J,I,GR)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

!********************************************************************
!      DEBUG SUBCHK,SUBTRACE
!      END DEBUG
      SUBROUTINE OGINIT_PRICE
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! ogsm system variables
      include'ogsmout'      ! ogsm global output variables
      include'pmmout'       ! lfmm global output variables
      include'lfmmout'      ! lfmm global output variables
      include'ngtdmrep'     ! ngmm global output variables
      include'mxpblk'       ! expectation variables

      logical once
      real pricecap(2)
      real OGPRCL48L(L48RGN,L48FUEL)
      real OGPRCOFFL(OFFRGN,OFFFUEL)
      real decline_rate, growth_rate
      real og_growth
      real prcexp
      real prcadj
      real prctmp
      real*4 NGPRCFAC(MNUMYR)
      real*4 og_avgoprc, og_avgoprc2
      real*4 og_avgprc
      real*4 gathering
      real*4 paddprc(mnumpr,mnumyr)
      REAL*4 TEMPSUM(mnumor), TEMPPRD(mnumor)
      INTEGER RGN(8) ! HOLDS REGION NUMBERS
      INTEGER YEAR   ! HOLDS YEAR
      INTEGER IR, DIST, IYR
      INTEGER*4 LOOKYR

      PARAMETER(LOOKYR=0)

      DATA RGN/1,2,3,4,5,6,7,8/
       data pricecap/0.30,0.30/
       data once/.false./
	DATA gathering/0.00/  !$0.25 gathering charge  ($0.15 in 87 dollars)

!==========================> HSM Code Start <==========================
    logical hsm_first  /.true./
!===========================> HSM Code End <===========================


        IF (.NOT.ONCE) THEN
          DO I=1,IJUMPYR
            NGPRCFAC(I) = OG_GROWTH(I,-1)**2
            WRITE(BUGOUT,*) 'P0fac',I+1989,OGPRCEXP(I),NGPRCFAC(I),  &
                                              OGPRCEXP(I)*NGPRCFAC(I)
          ENDDO
          ONCE = .TRUE.
        ENDIF

! ASSIGN REGIONAL WELLHEAD PRICE USING DISTRICT LEVEL PRICES FROM NGMM
        TEMPPRD = 0.
        TEMPSUM = 0.
        IYR = CURIYR
        IF (ogrunop(5) == 0 .AND. CURITR == 1 .AND. CURIYR > L48HYR) IYR = CURIYR-1
        DO DIST=1,ogdist
          IR = distmap(dist,1)
          IF (ogrunop(5) == 0) THEN
            IF (IR.NE.0.AND.IR.LE.L48RGN+OFFNEMSRGN) OGPRCNG(dist,curiyr) = OGWPRNG(IR,CURIYR)      ! LOWER 48
            IF (DIST.EQ.3.OR.DIST.EQ.75.OR.DIST.EQ.84) OGPRCNG(dist,curiyr) = OGWPRNG(mnumor-1,CURIYR)    ! ALASKA
          ENDIF
          IF (IR.NE.0.AND.IR.LE.L48RGN+OFFNEMSRGN) THEN   ! LOWER 48
            TEMPSUM(IR) = TEMPSUM(IR) + OGPRCNG(dist,curiyr)*OGRNAGPRD(dist,gastypes,iyr)
            TEMPPRD(IR) = TEMPPRD(IR) + OGRNAGPRD(dist,gastypes,iyr)
          ENDIF
          IF (DIST.EQ.3.OR.DIST.EQ.75.OR.DIST.EQ.84) THEN       ! ALASKA
            TEMPSUM(L48RGN+OFFNEMSRGN+2) = TEMPSUM(L48RGN+OFFNEMSRGN+2) + OGPRCNG(dist,curiyr)*OGRNAGPRD(dist,gastypes,iyr)
            TEMPPRD(L48RGN+OFFNEMSRGN+2) = TEMPPRD(L48RGN+OFFNEMSRGN+2) + OGRNAGPRD(dist,gastypes,iyr)
          ENDIF
          TEMPSUM(MNUMOR) = TEMPSUM(MNUMOR) + OGPRCNG(dist,curiyr)*OGRNAGPRD(dist,gastypes,iyr)
          TEMPPRD(MNUMOR) = TEMPPRD(MNUMOR) + OGRNAGPRD(dist,gastypes,iyr)
        ENDDO

        IF (ogrunop(5) == 1) THEN
          DO R=1,MNUMOR
            IF (TEMPPRD(R) > 0. .AND. TEMPSUM(R) > 0.) OGWPRNG(R,CURIYR) = TEMPSUM(R)/TEMPPRD(R)
            if (r == l48rgn+offnemsrgn+1 .or. r == l48rgn+offnemsrgn+3) ogwprng(r,curiyr) = ogwprng(l48rgn+offnemsrgn+2,curiyr)
          ENDDO
        ENDIF

! ASSIGN NEW LFMM VARIABLE RFCRUDEWHP(PADD,CRUDETYPE,YEAR) to OLD VARIABLE DCRDWHP(REGION,YEAR)
        if (curiyr.ge.l48hyr) then
          do r=1,MNUMPR
            paddprc(r,curiyr) = 0.
            do t=1,mncrud
               paddprc(r,curiyr) = paddprc(r,curiyr) + RFCRUDEWHP(r,t,curiyr)*OGCRUDEREF(r,t,curiyr)
            enddo
            if (sum(OGCRUDEREF(r,1:mncrud,curiyr)).gt.0.) paddprc(r,curiyr) = paddprc(r,curiyr)/sum(OGCRUDEREF(r,1:mncrud,curiyr))
          enddo
          if (CURIYR.gt.L48HYR+1) then
            do r=1,MNUMOR-1
              DCRDWHP(R,CURIYR) = paddprc(LFMMMAP(R),curiyr)
            enddo
          endif
          DO I =1,L48RGN+OFFNEMSRGN    ! CALCULATE LOWER 48 AVERAGE WELLHEAD PRICE
           DCRDWHP(mnumor,CURIYR)=DCRDWHP(mnumor,CURIYR)+ &
                              RFQTDCRD(I,CURIYR)*DCRDWHP(I,CURIYR)
          ENDDO
          DCRDWHP(mnumor,CURIYR)=DCRDWHP(mnumor,CURIYR)/RFQTDCRD(mnumor+1,CURIYR)
        endif

!  INITIALIZE PRICE VECTORS WITH CURRENT YEAR VALUE FROM PMM & NGMM
        DO R=1,AKRGN
!         OGPRCAK(R,1,1) = DCRDWHP(R+L48RGN+OFFNEMSRGN,CURIYR) ! AK CRUDE PRICE
          OGPRCAK(R,1,1) = amin1(OG_AVGOPRC(CURIYR,-3,R+L48RGN+OFFNEMSRGN),DCRDWHP(R+L48RGN+OFFNEMSRGN,CURIYR))
          OGPRCAK(R,2,1) = OGWPRNG(R+L48RGN+OFFNEMSRGN,CURIYR) - gathering
        ENDDO

!  SET LOWER48 ONSHORE PRICE VECTOR
        prcadj = og_growth(curiyr,lookyr)
        write(bugout,*) 'prcadj',curiyr+1989,prcadj
        DO R=1,L48RGN
        DO K=1,L48FUEL
          IF (K.LE.2) THEN
            OGPRCL48(R,K,1) = amin1(OG_AVGOPRC(CURIYR,-3,R),DCRDWHP(R,CURIYR))
          ELSE
            OGPRCL48(R,K,1) = amin1(OG_AVGPRC(CURIYR,-3,R)*prcadj,OGWPRNG(R,CURIYR)) - gathering
            if (curiyr.le.l48hyr+1) OGPRCL48(R,K,1) = OG_AVGPRC(CURIYR,-3,R)*prcadj - gathering
!           OGPRCL48(R,K,1) = OG_AVGPRC(CURIYR,-3,3)*prcadj - gathering
            if (k.eq.4) write(bugout,*) 'gasprc',curiyr+1989,r,ogprcl48(r,k,1),ogwprng(r,curiyr)
          ENDIF
        ENDDO
        ENDDO

        DO M=1,30
          DO R=1,MNUMPR
          DO K=1,MNCRUD
           OGPRCREF(r,k,m) = amin1(OG_AVGOPRC2(CURIYR,-3,r,k),RFCRUDEWHP(r,k,curiyr))
          ENDDO
          ENDDO
        ENDDO

!  SET OFFSHORE PRICE VECTOR
        OGPRCOFF(1,1,1) = amin1(OG_AVGOPRC(CURIYR,-3,L48RGN+1),DCRDWHP(L48RGN+1,CURIYR))
        OGPRCOFF(2,1,1) = amin1(OG_AVGOPRC(CURIYR,-3,L48RGN+3),DCRDWHP(L48RGN+3,CURIYR))
        OGPRCOFF(1,2,1) = amin1(OG_AVGPRC(CURIYR,-3,L48RGN+1)*prcadj,OGWPRNG(L48RGN+1,CURIYR)) - gathering
        OGPRCOFF(2,2,1) = amin1(OG_AVGPRC(CURIYR,-3,L48RGN+3)*prcadj,OGWPRNG(L48RGN+3,CURIYR)) - gathering
        if (curiyr.le.l48hyr+1) OGPRCOFF(1,2,1) = OG_AVGPRC(CURIYR,-3,L48RGN+1)*prcadj - gathering
        if (curiyr.le.l48hyr+1) OGPRCOFF(2,2,1) = OG_AVGPRC(CURIYR,-3,L48RGN+3)*prcadj - gathering
        DO R=3,OFFRGN
          OGPRCOFF(R,1,1) = amin1(OG_AVGOPRC(CURIYR,-3,L48RGN+2),DCRDWHP(L48RGN+2,CURIYR))
          OGPRCOFF(R,2,1) = amin1(OG_AVGPRC(CURIYR,-3,L48RGN+2)*prcadj,OGWPRNG(L48RGN+2,CURIYR)) - gathering
          if (curiyr.le.l48hyr+1) OGPRCOFF(R,2,1) = OG_AVGPRC(CURIYR,-3,L48RGN+2)*prcadj - gathering
        ENDDO

!  AFTER THE STEO YEARS, CAP THE DCF PRICE TO
!    SOME PERCENTAGE OF THE PREVIOUS YEARS PRICE
        IF (CURIYR.GT.L48HYR+1.AND.CURIYR .GT. TECHYR) THEN
          DO R=1,L48RGN
          DO K=3,L48FUEL
            PRCTMP = OGPRCL48(R,K,1)
            OGPRCL48(R,K,1) = amin1(ogprcl48l(r,K)*(1+pricecap(2)),OGPRCL48(R,K,1))
            OGPRCL48(R,K,1) = amax1(ogprcl48l(r,K)*(1-pricecap(2)),OGPRCL48(R,K,1))
            IF (ABS(PRCTMP-OGPRCL48(R,K,1)).gt.0.00001)  &
               write(bugout,*) 'attn: price cap l48on',curiyr+baseyr-1, &
                 r,ogprcl48(r,k,1),ogwprng(r,curiyr)
          ENDDO
          ENDDO
          DO R=1,OFFRGN
          DO K=1,OFFFUEL
            PRCTMP = OGPRCOFF(R,K,1)
            OGPRCOFF(R,K,1) = amin1(ogprcoffl(r,K)*(1+pricecap(K)),OGPRCOFF(R,K,1))
            OGPRCOFF(R,K,1) = amax1(ogprcoffl(r,K)*(1-pricecap(K)),OGPRCOFF(R,K,1))
            IF (ABS(PRCTMP-OGPRCOFF(R,K,1)).gt.0.00001.and.k.eq.2.and.r.eq.1)  &
               write(bugout,*) 'attn: price cap offng',curiyr+baseyr-1, &
                 r,ogprcoff(r,k,1),ogwprng(L48RGN+1,curiyr)
            IF (ABS(PRCTMP-OGPRCOFF(R,K,1)).gt.0.00001.and.k.eq.2.and.r.eq.2)  &
               write(bugout,*) 'attn: price cap offng',curiyr+baseyr-1, &
                 r,ogprcoff(r,k,1),ogwprng(L48RGN+3,curiyr)
            IF (ABS(PRCTMP-OGPRCOFF(R,K,1)).gt.0.00001.and.k.eq.2.and.r.gt.2)  &
               write(bugout,*) 'attn: price cap offng',curiyr+baseyr-1, &
                 r,ogprcoff(r,k,1),ogwprng(L48RGN+2,curiyr)
          ENDDO
          ENDDO
        ENDIF

!  INITIALIZE PRICE VECTORS WITH CURRENT YEAR VALUE FROM PMM & NGMM
      DO M=2,30
        DO R=1,AKRGN
          OGPRCAK(R,1,M) = OGPRCAK(R,1,M-1) ! AK CRUDE PRICE
          OGPRCAK(R,2,M) = OGPRCAK(R,2,M-1)
        ENDDO

        DO R=1,L48RGN
!  SET LOWER48 ONSHORE PRICE VECTOR
          OGPRCL48(R,1,M) = OGPRCL48(R,1,M-1)
          OGPRCL48(R,2,M) = OGPRCL48(R,2,M-1)
          DO K=3,L48FUEL
            OGPRCL48(R,K,M) = OGPRCL48(R,K,M-1)
          ENDDO
        ENDDO

!  SET OFFSHORE PRICE VECTOR
        DO R=1,OFFRGN
          OGPRCOFF(R,1,M) = OGPRCOFF(R,1,M-1)
          OGPRCOFF(R,2,M) = OGPRCOFF(R,2,M-1)
        ENDDO
      ENDDO

!  RETAIN LAGGED VALUES FOR USE IN NEXT ITERATION

      do k=1,l48fuel
        DO R=1,L48RGN

!  SET LOWER48 ONSHORE PRICE VECTOR
          OGPRCL48L(R,k) =  OGPRCL48(R,k,1)
        ENDDO
      ENDDO

!  SET OFFSHORE PRICE VECTORS

      OGPRCOFFL(1,1) =    OGPRCOFF(1,1,1)
      OGPRCOFFL(2,1) =    OGPRCOFF(2,1,1)
      OGPRCOFFL(1,2) =    OGPRCOFF(1,2,1)
      OGPRCOFFL(2,2) =    OGPRCOFF(2,2,1)
      DO R=3,OFFRGN
        OGPRCOFFL(R,1) = OGPRCOFF(R,1,1)
        OGPRCOFFL(R,2) = OGPRCOFF(R,2,1)
      ENDDO

!  SETUP MARKET PRICES BASED UPON SYSTEM FORESIGHT
!  I4SCNT=1 USE SYSTEM GENERATED PRICES ELSE USE OUR DEFAULTS
!
      IF ((I4SCNT .EQ. 1 .or. ogrunop(4).eq.1)) THEN
!
!  INITIALIZE PRICE VECTORS WITH THE NEMS SYSTEM PRICES
        DO M=2,30
          DO R=1,AKRGN
            OGPRCAK(R,1,M) = XDCRDWHP(R+9,M+CURIYR-1) !CRUDE PRICE
            OGPRCAK(R,2,M) = XOGWPRNG(R+9,M+CURIYR-1) - gathering ! NG PRICE
          ENDDO

          DO R=1,L48RGN
            OGPRCL48(R,1,M) = XDCRDWHP(R,M+CURIYR-1) ! CRUDE PRICE
            OGPRCL48(R,2,M) = XDCRDWHP(R,M+CURIYR-1) ! CRUDE PRICE
            do k=3,l48fuel
              OGPRCL48(R,k,M) = XOGWPRNG(R,M+CURIYR-1) - gathering ! NG PRICE
            enddo
          ENDDO

          OGPRCOFF(1,1,M) = XDCRDWHP(L48RGN+1,M+CURIYR-1) ! CRUDE PRICE
          OGPRCOFF(1,2,M) = XOGWPRNG(L48RGN+1,M+CURIYR-1) - gathering ! NG PRICE
          OGPRCOFF(2,1,M) = XDCRDWHP(L48RGN+3,M+CURIYR-1) ! CRUDE PRICE
          OGPRCOFF(2,2,M) = XOGWPRNG(L48RGN+3,M+CURIYR-1) - gathering ! NG PRICE
          DO R=3,OFFRGN
            OGPRCOFF(R,1,M) = XDCRDWHP(L48RGN+2,M+CURIYR-1) ! CRUDE PRICE
            OGPRCOFF(R,2,M) = XOGWPRNG(L48RGN+2,M+CURIYR-1) - gathering ! NG PRICE
          ENDDO
        ENDDO

      ENDIF   ! END PERFECT FORESIGHT


      do r=1,l48rgn
      do m=1,1
        write(bugout,*) 'ogprcl48',curiyr+1989,r,m,ogprcl48(r,1,m), ogprcl48(r,3,m)
      enddo
      enddo


!  ASSIGN REPORTING PRICE ARRAYS
      DO R=1,L48RGN
        DO K=1,L48FUEL
          REPPRCL48(R,K,CURIYR) = OGPRCL48(R,K,1)
        ENDDO
      ENDDO

      DO R=1,OFFRGN
        REPPRCOFF(R,1,CURIYR) = OGPRCOFF(R,1,1)
        REPPRCOFF(R,2,CURIYR) = OGPRCOFF(R,2,1)
      ENDDO

      DO R=1,AKRGN
        REPPRCAK(R,1,CURIYR) = OGPRCAK(R,1,1)
        REPPRCAK(R,2,CURIYR) = OGPRCAK(R,2,1)
      ENDDO
      
!==========================> HSM Code Start <==========================
if (hsm_first.AND.(CURCALYR.eq.2025)) Then
    OPEN(unit=hsm_prices, file='hsm_prices.log', action='write', position='append')
    
    ! OGPRCNG(OGDIST,MNUMYR)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'district_number', (/1990:(1990 + MNUMYR - 1)/)
    do m = 1, OGDIST
        write(hsm_prices, '(*(G0.16,:,","))') 'ogprcng', m, ((OGPRCNG(m,r)), r=1,MNUMYR)
    enddo
    
    ! OGRNAGPRD(OGDIST,GASTYPES,MNUMYR)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'district_number', 'gas_type_number', (/1990:(1990 + MNUMYR - 1)/)
    do m = 1, OGDIST
        do i = 1, gastypes
            write(hsm_prices, '(*(G0.16,:,","))') 'ogrnagprd', m, i, ((OGRNAGPRD(m,i,r)), r=1,MNUMYR)
        enddo
    enddo
    
    ! OGWPRNG(MNUMOR,MNUMYR)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'region_number', (/1990:(1990 + MNUMYR - 1)/)
    do m = 1, MNUMOR
        write(hsm_prices, '(*(G0.16,:,","))') 'ogprcng', m, ((OGWPRNG(m,r)), r=1,MNUMYR)
    enddo
    
    ! paddprc(mnumpr,mnumyr)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'lfmm_region_number', (/1990:(1990 + MNUMYR - 1)/)
    do m = 1, mnumpr
        write(hsm_prices, '(*(G0.16,:,","))') 'paddprc', m, ((paddprc(m,r)), r=1,MNUMYR)
    enddo
    
    ! RFCRUDEWHP(MNUMPR,MNCRUD,MNUMYR)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'lfmm_region_number', 'crude_type_number', (/1990:(1990 + MNUMYR - 1)/)
    do m = 1, mnumpr
        do i = 1, mncrud
            write(hsm_prices, '(*(G0.16,:,","))') 'rfcrudewhp', m, i, ((rfcrudewhp(m,i,r)), r=1,MNUMYR)
        enddo
    enddo
    
    ! OGCRUDEREF(MNUMPR,MNCRUD,MNUMYR)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'lfmm_region_number', 'crude_type_number', (/1990:(1990 + MNUMYR - 1)/)
    do m = 1, mnumpr
        do i = 1, mncrud
            write(hsm_prices, '(*(G0.16,:,","))') 'ogcruderef', m, i, ((OGCRUDEREF(m,i,r)), r=1,MNUMYR)
        enddo
    enddo
    
    ! DCRDWHP(MNUMOR,MNUMYR)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'region_number', (/1990:(1990 + MNUMYR - 1)/)
    do m = 1, MNUMOR
        write(hsm_prices, '(*(G0.16,:,","))') 'dcrdwhp', m, ((DCRDWHP(m,r)), r=1,MNUMYR)
    enddo
    
    ! RFQTDCRD(MNUMOR+2,MNUMYR)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'region_number', (/1990:(1990 + MNUMYR - 1)/)
    do m = 1, MNUMOR+2
        write(hsm_prices, '(*(G0.16,:,","))') 'rfqtdcrd', m, ((RFQTDCRD(m,r)), r=1,MNUMYR)
    enddo
    
    ! OGPRCAK(AKRGN,AKFUEL,30)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'alaska_region_number', 'fuel_number', (/0:(30 -1)/)
    do m = 1, AKRGN
        do i = 1, AKFUEL
            write(hsm_prices, '(*(G0.16,:,","))') 'ogprcak', m, i, ((OGPRCAK(m,i,r)), r=1,30)
        enddo
    enddo
    
    ! OGPRCL48(L48RGN,L48FUEL,30)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'lower48_region_number', 'fuel_number', (/0:(30 -1)/)
    do m = 1, L48RGN
        do i = 1, L48FUEL
            write(hsm_prices, '(*(G0.16,:,","))') 'ogprcl48', m, i, ((OGPRCL48(m,i,r)), r=1,30)
        enddo
    enddo
    
    ! OGPRCOFF(OFFRGN,OFFFUEL,30)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'offshore_region_number', 'fuel_number', (/0:(30 -1)/)
    do m = 1, OFFRGN
        do i = 1, OFFFUEL
            write(hsm_prices, '(*(G0.16,:,","))') 'ogprcoff', m, i, ((OGPRCOFF(m,i,r)), r=1,30)
        enddo
    enddo
    
    ! XDCRDWHP(MNUMOR,MNXYR)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'region_number', (/1990:(1990 + MNXYR - 1)/)
    do m = 1, MNUMOR
        write(hsm_prices, '(*(G0.16,:,","))') 'xdcrdwhp', m, ((XDCRDWHP(m,r)), r=1,MNXYR)
    enddo
    
    ! XOGWPRNG(MNUMOR,MNXYR)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'region_number', (/1990:(1990 + MNXYR - 1)/)
    do m = 1, MNUMOR
        write(hsm_prices, '(*(G0.16,:,","))') 'xogwprng', m, ((XOGWPRNG(m,r)), r=1,MNXYR)
    enddo
    
    ! OGCRUDEPRD(OGDIST,MNCRUD,MNUMYR)
    write(hsm_prices, '(*(G0.16,:,","))') 'variable', 'district_number', 'crude_type_number', (/1990:(1990 + MNUMYR - 1)/)
    do m = 1, OGDIST
        do i = 1, mncrud
            write(hsm_prices, '(*(G0.16,:,","))') 'ogcrudeprd', m, i, ((OGCRUDEPRD(m,i,r)), r=1,MNUMYR)
        enddo
    enddo
    
    hsm_first = .false.
    close(unit=hsm_prices   , status='keep')
endif
!===========================> HSM Code End <===========================

      RETURN
      END


!********************************************************************
!      DEBUG SUBCHK,SUBTRACE
!      END DEBUG
      SUBROUTINE OGINIT_PRICE2
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsmout'      ! ogsm global output variables

      REAL OILPRICE(MNUMYR)       ! PRICE VECTOR
      REAL OILLOW(MNUMYR)         ! AEO94 LOW WOP PRICES
      REAL OILBASE(MNUMYR)        ! AEO94 BASE PRICES
      REAL OILHIGH(MNUMYR)        ! AEO94 HIGH WOP PRICES
      REAL GASPRICE(MNUMYR)       ! PRICE VECTOR
      REAL GASLOW(MNUMYR)         ! AEO94 LOW WOP PRICES
      REAL GASBASE(MNUMYR)        ! AEO94 BASE PRICES
      REAL GASHIGH(MNUMYR)        ! AEO94 HIGH WOP PRICES
      REAL NGPFACT     ! GAS PRICE ADJUSTMENT FACTOR
      REAL CRPFACT     ! CRUDE PRICE ADJUSTMENT FACTOR
      REAL GDPFLTR      ! GDP DEFLATOR

!  ALL PRICES IN 1993 DOLLARS
!  ALL CASES WERE TAKEN FROM THE AEO95 HIGH/REF/LOW WOP CASES
!  THESE PRICES ARE CONVERTED TO 1987 DOLLARS IN THE CODE
      DATA OILHIGH/23.81,19.67,18.70,16.12,14.90,17.27,18.49, &
                    19.38,19.81,20.44,21.15,21.85,22.60,23.31, &
                    23.95,24.55,25.25,26.01,26.95,28.02,28.99, &
                    20*28.99,20*28.99/
      DATA OILBASE/23.81,19.67,18.70,16.12,14.90,16.41,16.90, &
                    17.45,18.00,18.53,19.13,19.65,20.16,20.63, &
                    21.08,21.50,21.98,22.44,22.94,23.50,24.12, &
                    20*24.12,20*24.12/
      DATA OILLOW/23.81,19.67,18.70,16.12,14.90,14.19,13.45, &
                    13.38,13.39,13.49,13.52,13.61,13.66,13.81, &
                    14.00,14.25,14.41,14.53,14.58,14.60,14.65, &
                    20*14.65,20*14.65/
      DATA GASHIGH/1.90,1.73,1.80,2.02,1.86,1.90,1.82,1.88, &
                    1.91,2.05,2.16,2.35,2.50,2.71,2.88,3.07, &
                    3.17,3.28,3.34,3.44,21*3.51,20*3.51/
      DATA GASBASE/1.90,1.73,1.80,2.02,1.85,1.90,1.81,1.87, &
                    1.90,2.06,2.14,2.34,2.47,2.69,2.83,3.02, &
                    3.12,3.21,3.30,3.35,21*3.39,20*3.39/
      DATA GASLOW/1.90,1.73,1.80,2.02,1.85,1.87,1.79,1.80, &
                    1.86,1.95,2.03,2.19,2.26,2.41,2.48,2.62, &
                    2.68,2.78,2.85,2.90,21*2.88,20*2.88/


!  ASSIGN PRICE VECTOR ACCORDING TO CASE
!  CONVERT FROM 1993 DOLLARS TO 1987 DOLLARS
      GDPFLTR = 1.242

      IF (WWOP.EQ.3) THEN             ! HIGH WOP **** ONLY USED IN STANDALONE MODE (OGIPPR=1) ***
        OILPRICE(CURIYR) = OILHIGH(CURIYR)/GDPFLTR
        GASPRICE(CURIYR) = GASHIGH(CURIYR)/GDPFLTR
      ELSE IF (WWOP.EQ.2) THEN        ! BASE **** ONLY USED IN STANDALONE MODE (OGIPPR=1) ***
        OILPRICE(CURIYR) = OILBASE(CURIYR)/GDPFLTR
        GASPRICE(CURIYR) = GASBASE(CURIYR)/GDPFLTR
      ELSE IF (WWOP.EQ.1) THEN        ! LOW WOP **** ONLY USED IN STANDALONE MODE (OGIPPR=1) ***
        OILPRICE(CURIYR) = OILLOW(CURIYR)/GDPFLTR
        GASPRICE(CURIYR) = GASLOW(CURIYR)/GDPFLTR
      ENDIF
!  CONDITIONAL ADDED FOR POLICY PRICE TEST
      OILPRICE(CURIYR) = OILPRICE(CURIYR)+OGTAXPREM(1,CURIYR)
      GASPRICE(CURIYR) = GASPRICE(CURIYR)+OGTAXPREM(2,CURIYR)

      DO M=1,30
         OGPRCAK(1,1,M) = 0.543669*(OILPRICE(CURIYR)-0.50)**1.04167
         OGPRCAK(2,1,M) = 0.543669*(OILPRICE(CURIYR)-0.50)**1.04147
         OGPRCAK(3,1,M) = 1.2383439*(OILPRICE(CURIYR)-0.50)**0.8909
         DO R=1,AKRGN
            OGPRCAK(R,2,M) = GASPRICE(CURIYR)
         ENDDO

         DO R=1,L48RGN
            IF (R.EQ.1) THEN
               NGPFACT = 1.5712
               CRPFACT = 1.0683
            OGPRCL48(R,1,M)=1.350529*(OILPRICE(CURIYR)-0.50)**.908884
            OGPRCL48(R,2,M)=1.350529*(OILPRICE(CURIYR)-0.50)**.908884
            ELSE IF (R.EQ.2) THEN
               NGPFACT = 1.0239
               CRPFACT = 1.0503
            OGPRCL48(R,1,M)=1.365345*(OILPRICE(CURIYR)-0.50)**.908102
            OGPRCL48(R,2,M)=1.365345*(OILPRICE(CURIYR)-0.50)**.908102
            ELSE IF (R.EQ.3) THEN
               NGPFACT = 0.9298
               CRPFACT = 1.0596
            OGPRCL48(R,1,M)=1.285400*(OILPRICE(CURIYR)-0.50)**.923486
            OGPRCL48(R,2,M)=1.285400*(OILPRICE(CURIYR)-0.50)**.923486
            ELSE IF (R.EQ.4) THEN
               NGPFACT = 0.8709
               CRPFACT = 1.0239
            OGPRCL48(R,1,M)=1.304659*(OILPRICE(CURIYR)-0.50)**.916972
            OGPRCL48(R,2,M)=1.304659*(OILPRICE(CURIYR)-0.50)**.916972
            ELSE IF (R.EQ.5) THEN
               NGPFACT = 0.8533
               CRPFACT = 0.9999
            OGPRCL48(R,1,M)=1.155351*(OILPRICE(CURIYR)-0.50)**.941837
            OGPRCL48(R,2,M)=1.155351*(OILPRICE(CURIYR)-0.50)**.941837
            ELSE IF (R.EQ.6) THEN
               NGPFACT = 1.3828
               CRPFACT = 0.8229
            OGPRCL48(R,1,M)=1.048539*(OILPRICE(CURIYR)-0.50)**.911031
            OGPRCL48(R,2,M)=1.048539*(OILPRICE(CURIYR)-0.50)**.911031
            ENDIF
            OGPRCL48(R,3,M) = GASPRICE(CURIYR) * NGPFACT
            OGPRCL48(R,4,M) = GASPRICE(CURIYR) * NGPFACT
            OGPRCL48(R,5,M) = GASPRICE(CURIYR) * NGPFACT
            OGPRCL48(R,6,M) = GASPRICE(CURIYR) * NGPFACT
            OGPRCL48(R,7,M) = GASPRICE(CURIYR) * NGPFACT
         ENDDO

         DO R=1,OFFRGN
            IF (R.EQ.1) THEN
               NGPFACT = 1.0
               CRPFACT = 1.0
            OGPRCOFF(R,1,M)=1.556533*(OILPRICE(CURIYR))**.874847
            ELSE IF (R.EQ.2) THEN
               NGPFACT = 1.3828
               CRPFACT = 0.7504
            OGPRCOFF(R,1,M)=0.805447*(OILPRICE(CURIYR))**.974081
            ELSE IF (R.GE.3) THEN
               NGPFACT = 1.0945
               CRPFACT = 1.0198
            OGPRCOFF(R,1,M)=1.556533*(OILPRICE(CURIYR))**.874847
            ENDIF
!           OGPRCOFF(R,1,M) = OILPRICE(CURIYR) * CRPFACT
            OGPRCOFF(R,2,M) = GASPRICE(CURIYR) * NGPFACT
         ENDDO

      ENDDO

!  ASSIGN REPORTING PRICE ARRAYS
         DO R=1,L48RGN
            DO K=1,L48FUEL
               REPPRCL48(R,K,CURIYR) = OGPRCL48(R,K,1)
            ENDDO
         ENDDO

         DO R=1,OFFRGN
            REPPRCOFF(R,1,CURIYR) = OGPRCOFF(R,1,1)
            REPPRCOFF(R,2,CURIYR) = OGPRCOFF(R,2,1)
         ENDDO

         DO R=1,AKRGN
            REPPRCAK(R,1,CURIYR) = OGPRCAK(R,1,1)
            REPPRCAK(R,2,CURIYR) = OGPRCAK(R,2,1)
         ENDDO

      RETURN
      END

!********************************************************************
!      DEBUG SUBCHK,SUBTRACE
!      END DEBUG
      SUBROUTINE OGINIT_RES
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! lower 48 input variables
      include'ogsmoff'      ! offshore input variables
      include'ogsmout'      ! ogsm global output variables

      REAL TOTPRD1(OFFFUEL)
      REAL TOTRSVS
      REAL TOTPRD
      REAL TOTRESAD
      REAL TOTRSV1(OFFFUEL)
      REAL TOTRSVGOM(OFFFUEL)
      REAL AGGPRROFF(OFFFUEL)


!  ASSIGN LOWER 48 GAS NEMS OUTPUT VARIABLES
      RESBOYL48 = 0.
      DO M = 1,L48BYR

!  ASSIGN LOWER 48 CRUDE NEMS OUTPUT VARIABLES
        DO R=1,L48RGN
           OGRESCO(R,M) = HISTRESL48(R,1,M) + HISTRESL48(R,2,M)
           OGPRRCO(R,M) = (HISTPRRL48(R,1,M)*HISTRESL48(R,1,M)+ &
                           HISTPRRL48(R,2,M)*HISTRESL48(R,2,M))/ &
                          (HISTRESL48(R,1,M) + HISTRESL48(R,2,M))
           OGELSCO(R,M) = ELASTL48(R,1)
        ENDDO


!  COMPUTE OFFSHORE TOTAL OIL & GAS RESERVES IN GOM TO REPORT TO NEMS
        DO K=1,OFFFUEL
           TOTRSVGOM(K) = 0.0  ! INITIALIZE TO 0
           DO R=3,OFFRGN       ! PROCESS GOM REGIONS ONLY
              TOTRSVGOM(K) = TOTRSVGOM(K) + HISTRESOFF(R,K,M)
           ENDDO
        ENDDO

!  COMPUTE OFFSHORE AGGREGATED P/R RATIO FOR GOM
        DO K=1,OFFFUEL
           TOTPRD1(K) = 0.0
           TOTRSV1(K) = 0.0
           DO R=3,OFFRGN       ! PROCESS GOM REGIONS ONLY
              TOTPRD1(K) = TOTPRD1(K) + (HISTRESOFF(R,K,M) * &
                           HISTPRROFF(R,K,M))
              TOTRSV1(K) = TOTRSV1(K) + HISTRESOFF(R,K,M)
           ENDDO
           AGGPRROFF(K) = TOTPRD1(K) / TOTRSV1(K)
        ENDDO

!  REPORT OFF SHORE CRUDE VALUES TO NEMS
        OGRESCO(L48RGN+1,M) = HISTRESOFF(1,1,M)
        OGRESCO(L48RGN+3,M) = HISTRESOFF(2,1,M)
        OGRESCO(L48RGN+2,M) = TOTRSVGOM(1)
        OGPRRCO(L48RGN+1,M) = HISTPRROFF(1,1,M)
        OGPRRCO(L48RGN+3,M) = HISTPRROFF(2,1,M)
        OGPRRCO(L48RGN+2,M) = AGGPRROFF(1)

!  REPORT OFFSHORE GAS VALUES TO NEMS
!  OGSM RGN 2 => NGMM/PMM RGN 3;  OGSM RGN 3-8 => NGMM/PMM RNG 2
        OGRESNGOF(1,M) = HISTRESOFF(1,2,M)
        OGRESNGOF(3,M) = HISTRESOFF(2,2,M)
        OGRESNGOF(2,M) = TOTRSVGOM(2)
        OGPRRNGOF(1,M) = HISTPRROFF(1,2,M)
        OGPRRNGOF(3,M) = HISTPRROFF(2,2,M)
        OGPRRNGOF(2,M) = AGGPRROFF(2)

      ENDDO

!  ASSIGN CURRENT RESERVES AND PR RATIOS
      DO K = 1,L48FUEL
        DO R = 1,L48RGN
          CURRESL48(R,K) = HISTRESL48(R,K,L48BYR)
          CURPRRL48(R,K) = HISTPRRL48(R,K,L48BYR)
        ENDDO
      ENDDO
      DO K = 1,OFFFUEL
        DO R = 1,OFFRGN
          CURRESOFF(R,K) = HISTRESOFF(R,K,L48BYR)
          CURPRROFF(R,K) = HISTPRROFF(R,K,L48BYR)
        ENDDO
      ENDDO

      RETURN
      END

!********************************************************************
!********************************************************************
!      DEBUG SUBCHK,SUBTRACE
!      END DEBUG
      SUBROUTINE OGINIT_HIST
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! ogsm variables
      include'ogsmoff'      ! ogsm variables
      include'ogsmout'      ! ogsm global output variables
      include'macout'       ! macro global output variables
      include'pmmout'       ! lfmm global output variables

      character*750 cline
      character*8 dummy
      character*6 cdum
      character*21 dummy2
      INTEGER IDUM
      real*8 tmpngpl(50,MNUMYR), tmpshr(ogdist,mnumyr)
      real*8 tmpngpl2(16,MNUMYR), tmpsum(16,mnumyr)
      real*4 ussum(mnumyr), shrsum(16)
      real*8 hoilres(ogdist,mnumyr), hgasres(ogdist,mnumyr), grestemp, orestemp
      real*8 tottemp
      real*4 tempdiff
      real*4 tOGPRDL48(l48rgn,l48fuel,mnumyr)
      real*4 togoilprd(ogdist,mnumyr)
      real*8 HEORPRD(15,MNUMYR)


!  INVOKE FILE_MGR TO OPEN INPUT FILE
      FNAME='WLHIST'
      NEW=.FALSE.
      IFILE2 = FILE_MGR('O',FNAME,NEW)

      OFFnHYR = L48HYR

!   READ HISTORICAL NON-TIGHT OIL WELLS DRILLED
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGOGWELLS(I,1,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL TIGHT OIL WELLS DRILLED
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGOGWELLS(I,2,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL CONVENTIONAL GAS WELLS DRILLED
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGOGWELLS(I,3,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL TIGHT GAS WELLS DRILLED
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGOGWELLS(I,4,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL SHALE GAS WELLS DRILLED
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGOGWELLS(I,5,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL CBM WELLS DRILLED
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGOGWELLS(I,6,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL DRY HOLES DRILLED
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGOGWELLS(I,7,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL NON-TIGHT CRUDE OIL PRODUCTION
      ogoilprd = 0.
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGOILPRD(I,1,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL TIGHT CRUDE OIL PRODUCTION
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGOILPRD(I,2,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL AD GAS PRODUCTION
      ogadgprd = 0.
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGADGPRD(I,1,M),m=1,l48hyr)
      enddo
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGADGPRD(I,2,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL NA CONVENTIONAL GAS PRODUCTION
      ogenagprd = 0.
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGENAGPRD(I,1,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL NA TIGHT GAS PRODUCTION
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGENAGPRD(I,2,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL NA SHALE GAS PRODUCTION
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGENAGPRD(I,3,M),m=1,l48hyr)
      enddo

!   READ HISTORICAL NA COALBED METHANE PRODUCTION
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (OGENAGPRD(I,4,M),m=1,l48hyr)

      enddo

!   READ HISTORICAL BEGINNING-OF-YEAR CRUDE OIL PROVED RESERVES
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (HOILRES(I,M),m=1,l48hyr+1)

      enddo

!   READ HISTORICAL BEGINNING-OF-YEAR NATURAL GAS PROVED RESERVES
      do I = 1,ogdist
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, cdum, (HGASRES(I,M),m=1,l48hyr+1)
      enddo

!  ASSIGN DISTRICT-LEVEL VARIABLES TO REGION-LEVEL
     HISTPRDl48 = 0.
     HISTRESl48 = 0.
     HISTPRRl48 = 0.
     HISTPRDOFF = 0.
     HISTRESOFF = 0.
     HISTPRROFF = 0.
     HISTADOFF = 0.
     HISTWELL48 = 0.
     HISTWELOFF = 0.
     HISTSRL48(1,:,:,:) = 0.80
     HISTSRL48(2,:,:,:) = 0.98
     HISTSROFF(1,:,:,:) = 0.30
     HISTSROFF(2,:,:,:) = 0.80
     OGPRDAD = 0.
     DO M=1,L48HYR
       DO I=1,OGDIST
         OGADGPRD(I,oiltypes,M) = sum(OGADGPRD(I,1:oiltypes-1,M))    ! determine total ad gas from OGSM
         OGENAGPRD(I,gastypes,M) = sum(OGENAGPRD(I,1:gastypes-1,M))
         OGRNAGPRD(I,:,M) = OGENAGPRD(I,:,M)
         DO K=1,gastypes-1
           if(sum(OGRNAGPRD(I,1:gastypes-1,m)).gt.0.) grestemp = HGASRES(I,M)*OGRNAGPRD(I,K,M)/sum(OGRNAGPRD(I,1:gastypes-1,M))
           if (OGRNAGPRD(I,K,M).gt.0.) grestemp = max(grestemp,ogrnagprd(i,k,m)/0.2)     ! set maximum PR ratio
           if(sum(OGOILPRD(I,1:4,m)).gt.0.) orestemp = HOILRES(I,M)*OGOILPRD(I,K,M)/sum(OGOILPRD(I,1:4,M))
           if (OGOILPRD(I,K,M).gt.0.) orestemp = max(orestemp,ogoilprd(i,k,m)/0.2)     ! set maximum PR ratio
           IF (DISTMAP(I,1).NE.0.AND.DISTMAP(I,1).LE.L48RGN) THEN
             HISTPRDL48(DISTMAP(I,1),K+3,M) = HISTPRDL48(DISTMAP(I,1),K+3,M) + OGRNAGPRD(I,K,M)
             if(sum(OGRNAGPRD(I,1:4,m)).gt.0.)HISTRESL48(DISTMAP(I,1),K+3,M) = HISTRESL48(DISTMAP(I,1),K+3,M) + grestemp
             HISTPRDL48(DISTMAP(I,1),1,M) = HISTPRDL48(DISTMAP(I,1),1,M) + OGOILPRD(I,K,M)
             HISTRESL48(DISTMAP(I,1),1,M) = HISTRESL48(DISTMAP(I,1),1,M) + orestemp
             OGPRDAD(DISTMAP(I,1),M) = OGPRDAD(DISTMAP(I,1),M) + OGADGPRD(I,K,M)
           ENDIF
           IF (DISTMAP(I,1).NE.0.AND.DISTMAP(I,1).LE.L48RGN+OFFNEMSRGN) THEN
             HISTADPRD(DISTMAP(I,1),M) = HISTADPRD(DISTMAP(I,1),M) + OGADGPRD(I,K,M)
           ENDIF
           IF (DISTMAP(I,1).GE.L48RGN+1.AND.DISTMAP(I,1).LE.L48RGN+3) THEN
               IF (DISTMAP(I,1).eq.L48RGN+1) THEN
                 HISTPRDOFF(1,1,M) = HISTPRDOFF(1,1,M) + OGOILPRD(I,K,M)
                 HISTPRDOFF(1,2,M) = HISTPRDOFF(1,2,M) + OGRNAGPRD(I,K,M)
                 HISTADOFF(1,M) = HISTADOFF(1,M) + OGADGPRD(I,K,M)
                 OGPRDAD(L48RGN+1,M) = OGPRDAD(L48RGN+1,M) + OGADGPRD(I,K,M)
                 IF (K.EQ.1) HISTRESOFF(1,K,M) = HISTRESOFF(1,K,M) + HOILRES(I,M)
                 IF (K.EQ.2) HISTRESOFF(1,K,M) = HISTRESOFF(1,K,M) + HGASRES(I,M)
               ENDIF
               IF (DISTMAP(I,1).eq.L48RGN+2) THEN
                 HISTPRDOFF(3,1,M) = HISTPRDOFF(3,1,M) + 0.20*OGOILPRD(I,K,M)
                 HISTPRDOFF(4,1,M) = HISTPRDOFF(4,1,M) + 0.80*OGOILPRD(I,K,M)
                 HISTPRDOFF(3,2,M) = HISTPRDOFF(3,2,M) + 0.40*OGRNAGPRD(I,K,M)
                 HISTPRDOFF(4,2,M) = HISTPRDOFF(4,2,M) + 0.60*OGRNAGPRD(I,K,M)
                 HISTADOFF(4,M) = HISTADOFF(4,M) + OGADGPRD(I,K,M)
                 OGPRDAD(L48RGN+2,M) = OGPRDAD(L48RGN+2,M) + OGADGPRD(I,K,M)
                 IF (K.EQ.1) THEN
                   HISTRESOFF(3,K,M) = HISTRESOFF(3,K,M) + 0.16*HOILRES(I,M)
                   HISTRESOFF(4,K,M) = HISTRESOFF(4,K,M) + 0.84*HOILRES(I,M)
                 ENDIF
                 IF (K.EQ.2) THEN
                   HISTRESOFF(3,K,M) = HISTRESOFF(3,K,M) + 0.50*HGASRES(I,M)
                   HISTRESOFF(4,K,M) = HISTRESOFF(4,K,M) + 0.50*HGASRES(I,M)
                 ENDIF
               ENDIF
               IF (DISTMAP(I,1).eq.L48RGN+3) THEN
                 HISTPRDOFF(2,1,M) = HISTPRDOFF(2,1,M) + OGOILPRD(I,K,M)
                 HISTPRDOFF(2,2,M) = HISTPRDOFF(2,2,M) + OGRNAGPRD(I,K,M)
                 HISTADOFF(2,M) = HISTADOFF(2,M) + OGADGPRD(I,K,M)
                 OGPRDAD(L48RGN+3,M) = OGPRDAD(L48RGN+3,M) + OGADGPRD(I,K,M)
                 IF (K.EQ.1) HISTRESOFF(2,K,M) = HISTRESOFF(2,K,M) + HOILRES(I,M)
                 IF (K.EQ.2) HISTRESOFF(2,K,M) = HISTRESOFF(2,K,M) + HGASRES(I,M)
               ENDIF
               if(HISTRESOFF(K,1,M).gt.0) HISTPRROFF(K,1,M) = HISTPRDOFF(K,1,M)/HISTRESOFF(K,1,M)
               if(HISTRESOFF(K,2,M).gt.0) HISTPRROFF(K,2,M) = HISTPRDOFF(K,2,M)/HISTRESOFF(K,2,M)
           ENDIF
           OGDNGPRD(I,K,M) = OGRNAGPRD(I,K,M)
           OGOILPRD(I,K,M) = OGOILPRD(I,K,M) / 365. ! convert to million barrels per day
         ENDDO
         OGDNGPRD(I,1,M) = OGDNGPRD(I,1,M) + sum(OGADGPRD(I,1:oiltypes-1,M)) - OGADGPRD(I,2,M)
         OGDNGPRD(I,3,M) = OGDNGPRD(I,3,M) + OGADGPRD(I,2,M)
         OGOILPRD(I,oiltypes,M) = sum(OGOILPRD(I,1:oiltypes-1,M))
         PMMDG(I,3,M) = OGRNAGPRD(I,1,M) + OGADGPRD(I,1,M) + OGADGPRD(I,3,M) + OGADGPRD(I,4,M)
         PMMDG(I,5,M) = OGRNAGPRD(I,2,M)
         PMMDG(I,6,M) = OGRNAGPRD(I,3,M) + OGADGPRD(I,2,M)
         PMMDG(I,7,M) = OGRNAGPRD(I,4,M)
         IF (DISTMAP(I,1).NE.0.AND.DISTMAP(I,1).LE.L48RGN) THEN
           HISTWELL48(2,DISTMAP(I,1),1,M) = HISTWELL48(2,DISTMAP(I,1),1,M) + OGOGWELLS(I,1,M) + OGOGWELLS(I,2,M) + 0.5*OGOGWELLS(I,7,M)
           HISTWELL48(2,DISTMAP(I,1),3,M) = HISTWELL48(2,DISTMAP(I,1),3,M) + OGOGWELLS(I,3,M) + 0.5*OGOGWELLS(I,7,M)
           HISTWELL48(2,DISTMAP(I,1),5,M) = HISTWELL48(2,DISTMAP(I,1),5,M) + OGOGWELLS(I,4,M)
           HISTWELL48(2,DISTMAP(I,1),6,M) = HISTWELL48(2,DISTMAP(I,1),6,M) + OGOGWELLS(I,5,M)
           HISTWELL48(2,DISTMAP(I,1),7,M) = HISTWELL48(2,DISTMAP(I,1),7,M) + OGOGWELLS(I,6,M)
         ENDIF
         IF (DISTMAP(I,1).EQ.L48RGN+1) HISTWELOFF(2,1,1,M) = HISTWELOFF(2,1,1,M) + OGOGWELLS(I,1,M) + 0.5*OGOGWELLS(I,7,M)
         IF (DISTMAP(I,1).EQ.L48RGN+1) HISTWELOFF(2,1,2,M) = HISTWELOFF(2,1,2,M) + OGOGWELLS(I,3,M) + 0.5*OGOGWELLS(I,7,M)
         IF (DISTMAP(I,1).EQ.L48RGN+2) HISTWELOFF(2,3,1,M) = HISTWELOFF(2,3,1,M) + OGOGWELLS(I,1,M) + 0.5*OGOGWELLS(I,7,M)
         IF (DISTMAP(I,1).EQ.L48RGN+2) HISTWELOFF(2,3,2,M) = HISTWELOFF(2,3,2,M) + OGOGWELLS(I,3,M) + 0.5*OGOGWELLS(I,7,M)
         IF (DISTMAP(I,1).EQ.L48RGN+3) HISTWELOFF(2,2,1,M) = HISTWELOFF(2,2,1,M) + OGOGWELLS(I,1,M) + 0.5*OGOGWELLS(I,7,M)
         IF (DISTMAP(I,1).EQ.L48RGN+3) HISTWELOFF(2,2,2,M) = HISTWELOFF(2,2,2,M) + OGOGWELLS(I,3,M) + 0.5*OGOGWELLS(I,7,M)

       ENDDO
       DO R=1,L48RGN
         DO K=1,L48FUEL
             if(HISTRESL48(R,K,M).GT.0) HISTPRRL48(R,K,M) = HISTPRDL48(R,K,M)/HISTRESL48(R,K,M)
         ENDDO
       ENDDO 
       OGPRDADOF(1,M) = ogprdad(l48rgn+1,m)
       OGPRDADOF(2,M) = ogprdad(l48rgn+2,m)
       OGPRDADOF(3,M) = ogprdad(l48rgn+3,m)
     ENDDO

!   READ HISTORICAL TIGHT OIL PRODUCTION FOR SELECT PLAYS
      do M = 1,l48hyr
        call ognxtdat(ifile2,cline)
        READ (cline,*) dummy, (ogqshloil(I,M),I=1,10),ogqshloil(15,M)
      enddo

!   READ HISTORICAL SHALE GAS PRODUCTION FOR SELECT PLAYS
      do M = 1,l48hyr 
        call ognxtdat(ifile2,cline)
        READ (cline,*) dummy, (ogqshlgas(I,M),I=1,9),ogqshlgas(15,M)

      enddo

!   READ HISTORICAL CRUDE OIL WELLHEAD PRICES AND PRODUCTION 
      do M = 1,l48hyr
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, (DCRDWHP(I,M),I=1,MNUMOR-1)
        DO I=1,MNUMOR-1
          DCRDWHP(I,M)=DCRDWHP(I,M)*mc_jpgdp(1987-baseyr+1)/mc_jpgdp(M) ! convert from nominal dollars to 87 dollars
          XDCRDWHP(I,M)=DCRDWHP(I,M)
        ENDDO
      enddo

      do M = 1,l48hyr
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, (RFQTDCRD(I,M),I=1,MNUMOR-1)
      enddo

!   READ HISTORICAL NGPL PRODUCTION (MMB) - to share out sub-state values
      do I = 1,50
        call ognxtdat(ifile2,cline)
        READ (cline,*) (tmpngpl(I,M),M=1,l48hyr)

      enddo
      do M = 1,l48hyr
        tmpngpl(8,m) = tmpngpl(8,m) + tmpngpl(7,m) + tmpngpl(6,m)   ! put all CA onshore in position 8
        tmpngpl(7,m) = 0.
        tmpngpl(6,m) = 0.
        tmpngpl(43,m) = tmpngpl(43,m) + tmpngpl(49,m)               ! put old UTAH & WYOMING category in WY
        tmpngpl(49,m) = 0.
        tmpngpl(47,m) = max(tmpngpl(47,m),0.1)                      ! current have 0 for 2009-2011, need share to be 1 not 0 so put small volume
        tmpngpl(50,m) = tmpngpl(42,m)*.7                            ! put some volumes in PA
        tmpngpl(42,m) = tmpngpl(42,m) - tmpngpl(50,m)               ! put some volumes in PA
      enddo

!     determine shares to apply to offcial NGPL production below
      tmpsum = 0.
      do I = 1,OGDIST
      do M = 1,l48hyr
        if(distmap(I,6) .ne. 99) tmpsum(distmap(I,6),m) = tmpsum(distmap(I,6),m) + tmpngpl(distmap(I,7),M)
      enddo
      enddo
      do I = 1,OGDIST
        do M = 1,l48hyr
          tmpshr(i,m) = 0.
          if(distmap(I,6).ne.99) then
            if(tmpsum(distmap(i,6),m).gt.0.) tmpshr(I,m) = tmpngpl(distmap(I,7),m)/tmpsum(distmap(I,6),m)
             if(m.eq.l48hyr.and.i.eq.3) tmpshr(I,m) = 0.45
             if(m.eq.l48hyr.and.i.eq.6) tmpshr(I,m) = 0.55
          endif
        enddo
        write(ogbug1,*) 'ngpl_shr',i,tmpshr(i,l48hyr-1),tmpshr(i,l48hyr)
      enddo

!   READ OFFICIAL HISTORICAL NGPL COMPOSITION (MBD)
!   STEO years are read in from wlbasic.txt but processed here
      do M = 1,l48hyr   ! total ngpls
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, (ngpl16(I,M),I=1,16)
      enddo
      do M = 1,l48hyr+steoyrs   ! total ngpls
        tmpngpl2(:,m) = ngpl16(:,m)
        if (m.le.3) then  ! share out padd totals for 1990-1992 using 1993 shares
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(3,m) = tmpngpl2(2,m)*tmpngpl2(3,4)/tmpngpl2(2,4)
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(4,m) = tmpngpl2(2,m)*tmpngpl2(4,4)/tmpngpl2(2,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(6,m) = tmpngpl2(5,m)*tmpngpl2(6,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(7,m) = tmpngpl2(5,m)*tmpngpl2(7,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(8,m) = tmpngpl2(5,m)*tmpngpl2(8,4)/tmpngpl2(5,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(10,m) = tmpngpl2(9,m)*tmpngpl2(10,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(11,m) = tmpngpl2(9,m)*tmpngpl2(11,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(12,m) = tmpngpl2(9,m)*tmpngpl2(12,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(13,m) = tmpngpl2(9,m)*tmpngpl2(13,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(14,m) = tmpngpl2(9,m)*tmpngpl2(14,4)/tmpngpl2(9,4)
        endif
        ussum(m) = (tmpngpl2(3,m) + tmpngpl2(4,m) + tmpngpl2(6,m) + tmpngpl2(7,m) + tmpngpl2(8,m)  &
                 + tmpngpl2(10,m) + tmpngpl2(11,m) + tmpngpl2(12,m) + tmpngpl2(13,m) + tmpngpl2(14,m)  &
                 + tmpngpl2(15,m) + tmpngpl2(16,m)) / 1000.
      enddo
!       write(6,*) 'dh5tot1', ogdist+15, (ussum(m),m=2008-1989,2011-1989)

      do I = 1,OGDIST   ! Assign to districts and convert to mmbd
        do M = 1,l48hyr+steoyrs
          ogngplprd(i,m) = 0.
          if(distmap(i,6).ne.99) then
            if (m.le.l48hyr) ogngplprd(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,m) / 1000.
            if (m.gt.l48hyr) ogngplprd(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,l48hyr) / 1000.
          endif
        enddo
      enddo
!      write(6,*) 'dh5tot2', ogdist+15, sum(ogngplprd(1:ogdist,2008-1989)),sum(ogngplprd(1:ogdist,2009-1989)), &
!        sum(ogngplprd(1:ogdist,2010-1989)),sum(ogngplprd(1:ogdist,2011-1989))

      do M = 1,l48hyr   ! ethane
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, (ethane16(I,M),I=1,16)
      enddo
      do M = 1,l48hyr+steoyrs   ! ethane
        tmpngpl2(:,m) = ethane16(:,m)
        if (m.le.3) then  ! share out padd totals for 1990-1992 using 1993 shares
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(3,m) = tmpngpl2(2,m)*tmpngpl2(3,4)/tmpngpl2(2,4)
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(4,m) = tmpngpl2(2,m)*tmpngpl2(4,4)/tmpngpl2(2,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(6,m) = tmpngpl2(5,m)*tmpngpl2(6,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(7,m) = tmpngpl2(5,m)*tmpngpl2(7,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(8,m) = tmpngpl2(5,m)*tmpngpl2(8,4)/tmpngpl2(5,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(10,m) = tmpngpl2(9,m)*tmpngpl2(10,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(11,m) = tmpngpl2(9,m)*tmpngpl2(11,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(12,m) = tmpngpl2(9,m)*tmpngpl2(12,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(13,m) = tmpngpl2(9,m)*tmpngpl2(13,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(14,m) = tmpngpl2(9,m)*tmpngpl2(14,4)/tmpngpl2(9,4)
        endif
        ussum(m) = (tmpngpl2(3,m) + tmpngpl2(4,m) + tmpngpl2(6,m) + tmpngpl2(7,m) + tmpngpl2(8,m)  &
                 + tmpngpl2(10,m) + tmpngpl2(11,m) + tmpngpl2(12,m) + tmpngpl2(13,m) + tmpngpl2(14,m)  &
                 + tmpngpl2(15,m) + tmpngpl2(16,m)) / 1000.
      enddo
!       write(6,*) 'dh5et1', ogdist+15, (ussum(m),m=l48hyr,l48hyr+steoyrs)

      do I = 1,OGDIST   ! Assign to districts and convert to mmbd
        do M = 1,l48hyr+steoyrs
          ogngplet(i,m) = 0.
          if(distmap(i,6).ne.99) then
            if (m.le.l48hyr) ogngplet(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,m) / 1000.
            if (m.gt.l48hyr) ogngplet(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,l48hyr) / 1000.
          endif
        enddo
      enddo
!       write(6,*) 'dh5et2', ogdist+15, sum(ogngplet(1:ogdist,l48hyr)),sum(ogngplet(1:ogdist,l48hyr+1)), &
!         sum(ogngplet(1:ogdist,l48hyr+2)),sum(ogngplet(1:ogdist,l48hyr+steoyrs))

      do M = 1,l48hyr   ! propane
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, (propane16(I,M),I=1,16)
      enddo
      do M = 1,l48hyr+steoyrs   ! propane
        tmpngpl2(:,m) = propane16(:,m)
        if (m.le.3) then  ! share out padd totals for 1990-1992 using 1993 shares
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(3,m) = tmpngpl2(2,m)*tmpngpl2(3,4)/tmpngpl2(2,4)
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(4,m) = tmpngpl2(2,m)*tmpngpl2(4,4)/tmpngpl2(2,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(6,m) = tmpngpl2(5,m)*tmpngpl2(6,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(7,m) = tmpngpl2(5,m)*tmpngpl2(7,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(8,m) = tmpngpl2(5,m)*tmpngpl2(8,4)/tmpngpl2(5,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(10,m) = tmpngpl2(9,m)*tmpngpl2(10,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(11,m) = tmpngpl2(9,m)*tmpngpl2(11,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(12,m) = tmpngpl2(9,m)*tmpngpl2(12,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(13,m) = tmpngpl2(9,m)*tmpngpl2(13,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(14,m) = tmpngpl2(9,m)*tmpngpl2(14,4)/tmpngpl2(9,4)
        endif
        ussum(m) = (tmpngpl2(3,m) + tmpngpl2(4,m) + tmpngpl2(6,m) + tmpngpl2(7,m) + tmpngpl2(8,m)  &
                 + tmpngpl2(10,m) + tmpngpl2(11,m) + tmpngpl2(12,m) + tmpngpl2(13,m) + tmpngpl2(14,m)  &
                 + tmpngpl2(15,m) + tmpngpl2(16,m)) / 1000.
      enddo
!       write(6,*) 'dh5pr1', ogdist+15, (ussum(m),m=l48hyr,l48hyr+steoyrs)

      do I = 1,OGDIST   ! Assign to districts and convert to mmbd
        do M = 1,l48hyr+steoyrs
          ogngplpr(i,m) = 0.
          if(distmap(i,6).ne.99) then
!           if (m.lt.l48hyr) ogngplpr(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,m) / 1000.
!           if (m.ge.l48hyr) ogngplpr(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,l48hyr-1) / 1000.
            if (m.le.l48hyr) ogngplpr(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,m) / 1000.
            if (m.gt.l48hyr) ogngplpr(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,l48hyr) / 1000.
          endif
        enddo
      enddo
!       write(6,*) 'dh5pr2', ogdist+15, sum(ogngplpr(1:ogdist,l48hyr)),sum(ogngplpr(1:ogdist,l48hyr+1)), &
!         sum(ogngplpr(1:ogdist,l48hyr+2)),sum(ogngplpr(1:ogdist,l48hyr+steoyrs))

      do M = 1,l48hyr   ! butane
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, (butane16(I,M),I=1,16)
      enddo
      do M = 1,l48hyr+steoyrs   ! butane
        tmpngpl2(:,m) = butane16(:,m)
        if (m.le.3) then  ! share out padd totals for 1990-1992 using 1993 shares
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(3,m) = tmpngpl2(2,m)*tmpngpl2(3,4)/tmpngpl2(2,4)
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(4,m) = tmpngpl2(2,m)*tmpngpl2(4,4)/tmpngpl2(2,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(6,m) = tmpngpl2(5,m)*tmpngpl2(6,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(7,m) = tmpngpl2(5,m)*tmpngpl2(7,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(8,m) = tmpngpl2(5,m)*tmpngpl2(8,4)/tmpngpl2(5,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(10,m) = tmpngpl2(9,m)*tmpngpl2(10,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(11,m) = tmpngpl2(9,m)*tmpngpl2(11,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(12,m) = tmpngpl2(9,m)*tmpngpl2(12,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(13,m) = tmpngpl2(9,m)*tmpngpl2(13,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(14,m) = tmpngpl2(9,m)*tmpngpl2(14,4)/tmpngpl2(9,4)
        endif
        ussum(m) = (tmpngpl2(3,m) + tmpngpl2(4,m) + tmpngpl2(6,m) + tmpngpl2(7,m) + tmpngpl2(8,m)  &
                 + tmpngpl2(10,m) + tmpngpl2(11,m) + tmpngpl2(12,m) + tmpngpl2(13,m) + tmpngpl2(14,m)  &
                 + tmpngpl2(15,m) + tmpngpl2(16,m)) / 1000.
      enddo
!       write(6,*) 'dh5bu1', ogdist+15, (ussum(m),m=l48hyr,l48hyr+steoyrs)

      do I = 1,OGDIST   ! Assign to districts and convert to mmbd
        do M = 1,l48hyr+steoyrs
          ogngplbu(i,m) = 0.
          if(distmap(i,6).ne.99) then
            if (m.le.l48hyr) ogngplbu(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,m) / 1000.
            if (m.gt.l48hyr) ogngplbu(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,l48hyr) / 1000.
          endif
        enddo
      enddo
!       write(6,*) 'dh5bu2', ogdist+15, sum(ogngplbu(1:ogdist,l48hyr)),sum(ogngplbu(1:ogdist,l48hyr+1)), &
!         sum(ogngplbu(1:ogdist,l48hyr+2)),sum(ogngplbu(1:ogdist,l48hyr+steoyrs))

      do M = 1,l48hyr   ! isobutane
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, (isobutane16(I,M),I=1,16)
      enddo
      do M = 1,l48hyr+steoyrs   ! isobutane
        tmpngpl2(:,m) = isobutane16(:,m)
        if (m.le.3) then  ! share out padd totals for 1990-1992 using 1993 shares
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(3,m) = tmpngpl2(2,m)*tmpngpl2(3,4)/tmpngpl2(2,4)
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(4,m) = tmpngpl2(2,m)*tmpngpl2(4,4)/tmpngpl2(2,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(6,m) = tmpngpl2(5,m)*tmpngpl2(6,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(7,m) = tmpngpl2(5,m)*tmpngpl2(7,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(8,m) = tmpngpl2(5,m)*tmpngpl2(8,4)/tmpngpl2(5,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(10,m) = tmpngpl2(9,m)*tmpngpl2(10,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(11,m) = tmpngpl2(9,m)*tmpngpl2(11,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(12,m) = tmpngpl2(9,m)*tmpngpl2(12,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(13,m) = tmpngpl2(9,m)*tmpngpl2(13,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(14,m) = tmpngpl2(9,m)*tmpngpl2(14,4)/tmpngpl2(9,4)
        endif
        ussum(m) = (tmpngpl2(3,m) + tmpngpl2(4,m) + tmpngpl2(6,m) + tmpngpl2(7,m) + tmpngpl2(8,m)  &
                 + tmpngpl2(10,m) + tmpngpl2(11,m) + tmpngpl2(12,m) + tmpngpl2(13,m) + tmpngpl2(14,m)  &
                 + tmpngpl2(15,m) + tmpngpl2(16,m)) / 1000.
      enddo
!       write(6,*) 'dh5is1', ogdist+15, (ussum(m),m=l48hyr,l48hyr+steoyrs)

      do I = 1,OGDIST   ! Assign to districts and convert to mmbd
        do M = 1,l48hyr+steoyrs
          ogngplis(i,m) = 0.
          if(distmap(i,6).ne.99) then
            if (m.le.l48hyr) ogngplis(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,m) / 1000.
            if (m.gt.l48hyr) ogngplis(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,l48hyr) / 1000.
          endif
        enddo
      enddo
!       write(6,*) 'dh5is2', ogdist+15, sum(ogngplis(1:ogdist,l48hyr)),sum(ogngplis(1:ogdist,l48hyr+1)), &
!         sum(ogngplis(1:ogdist,l48hyr+2)),sum(ogngplis(1:ogdist,l48hyr+steoyrs))

      do M = 1,l48hyr   ! pentanes plus
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, (pentanes16(I,M),I=1,16)
      enddo
      do M = 1,l48hyr+steoyrs   ! pentanes plus
        tmpngpl2(:,m) = pentanes16(:,m)
        if (m.le.3) then  ! share out padd totals for 1990-1992 using 1993 shares
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(3,m) = tmpngpl2(2,m)*tmpngpl2(3,4)/tmpngpl2(2,4)
          if(tmpngpl2(2,4).gt.0.) tmpngpl2(4,m) = tmpngpl2(2,m)*tmpngpl2(4,4)/tmpngpl2(2,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(6,m) = tmpngpl2(5,m)*tmpngpl2(6,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(7,m) = tmpngpl2(5,m)*tmpngpl2(7,4)/tmpngpl2(5,4)
          if(tmpngpl2(5,4).gt.0.) tmpngpl2(8,m) = tmpngpl2(5,m)*tmpngpl2(8,4)/tmpngpl2(5,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(10,m) = tmpngpl2(9,m)*tmpngpl2(10,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(11,m) = tmpngpl2(9,m)*tmpngpl2(11,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(12,m) = tmpngpl2(9,m)*tmpngpl2(12,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(13,m) = tmpngpl2(9,m)*tmpngpl2(13,4)/tmpngpl2(9,4)
          if(tmpngpl2(9,4).gt.0.) tmpngpl2(14,m) = tmpngpl2(9,m)*tmpngpl2(14,4)/tmpngpl2(9,4)
        endif
        ussum(m) = (tmpngpl2(3,m) + tmpngpl2(4,m) + tmpngpl2(6,m) + tmpngpl2(7,m) + tmpngpl2(8,m)  &
                 + tmpngpl2(10,m) + tmpngpl2(11,m) + tmpngpl2(12,m) + tmpngpl2(13,m) + tmpngpl2(14,m)  &
                 + tmpngpl2(15,m) + tmpngpl2(16,m)) / 1000.
      enddo
!       write(6,*) 'dh5pp1', ogdist+15, (ussum(m),m=l48hyr,l48hyr+steoyrs)

      do I = 1,OGDIST   ! Assign to districts and convert to mmbd
        do M = 1,l48hyr+steoyrs
          ogngplpp(i,m) = 0.
          if(distmap(i,6).ne.99) then
            if (m.le.l48hyr) ogngplpp(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,m) / 1000.
            if (m.gt.l48hyr) ogngplpp(i,m) = tmpngpl2(distmap(i,6),m)*tmpshr(i,l48hyr) / 1000.
          endif
        enddo
      enddo
!       write(6,*) 'dh5pp2', ogdist+15, sum(ogngplpp(1:ogdist,l48hyr)),sum(ogngplpp(1:ogdist,l48hyr)), &
!         sum(ogngplpp(1:ogdist,l48hyr+2)),sum(ogngplpp(1:ogdist,l48hyr+steoyrs))


!  HISTORICAL CRUDE OIL PRODUCTION BY TYPE AND LFMM REGION - L48 ONSHORE
      LF_crdprd_padd(:,:,:) = 0.
      do r = 1,pmmrgn-1   ! 
        do i = 1,6   ! 
          call ognxtdat(ifile2,cline)
          READ (cline,*) idum, idum, (LF_crdprd_padd(r,i,M),m=2009-baseyr+1,l48hyr)
        enddo
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, idum, (LF_crdprd_padd(r,10,M),m=2009-baseyr+1,l48hyr)
        call ognxtdat(ifile2,cline)
        READ (cline,*) idum, idum, (LF_crdprd_padd(r,11,M),m=2009-baseyr+1,l48hyr)
      enddo
      call ognxtdat(ifile2,cline)
      READ (cline,*) idum, idum, (LF_crdprd_padd(7,7,M),m=2009-baseyr+1,l48hyr)
      call ognxtdat(ifile2,cline)
      READ (cline,*) idum, idum, (LF_crdprd_padd(8,3,M),m=2009-baseyr+1,l48hyr)
      do m = 2009-baseyr+1,l48hyr   ! 
        do r = 1,pmmrgn-1   ! 
          do i = 1,6   ! 
            LF_crdprd_padd(r,i,m) = LF_crdprd_padd(r,i,m)*.365
          enddo
          LF_crdprd_padd(r,10,m) = LF_crdprd_padd(r,10,m)*.365
          LF_crdprd_padd(r,11,m) = LF_crdprd_padd(r,11,m)*.365
        enddo
        LF_crdprd_padd(7,7,m) = LF_crdprd_padd(7,7,m)*.365
        LF_crdprd_padd(8,3,m) = LF_crdprd_padd(8,3,m)*.365
        OGCRUDEREF(:,:,m) = LF_CRDPRD_PADD(:,:,m) ! new PADD variable
      enddo

!  SET DEFAULT AVERAGE API BY CRUDE TYPE
     DO M=1,MNCRUD
       LF_APIVOL(M,:) = 35.0
       IF (M.eq.1) LF_APIVOL(m,:) = 39.7
       IF (M.eq.2) LF_APIVOL(m,:) = 37.3
       IF (M.eq.3) LF_APIVOL(m,:) = 32.7
       IF (M.eq.4) LF_APIVOL(m,:) = 30.9
       IF (M.eq.5) LF_APIVOL(m,:) = 22.5
       IF (M.eq.6) LF_APIVOL(m,:) = 19.9
       IF (M.eq.7) LF_APIVOL(m,:) = 17.4
       IF (M.eq.8) LF_APIVOL(m,:) = 31.7
       IF (M.eq.9) LF_APIVOL(m,:) = 20.6
       IF (M.eq.10) LF_APIVOL(m,:) = 45.0
       IF (M.eq.11) LF_APIVOL(m,:) = 55.0
     ENDDO

!  HISTORICAL EOR PRODUCTION BASED ON OGJ EOR SURVEY
      do I = 1,15
        call ognxtdat(ifile2,cline)
        READ (cline,*) dummy, dummy2, (HEORPRD(I,M),m=1,l48hyr)
      enddo

!  ADJUST HISTORICAL LOWER 48 ONSHORE PRODUCTION TO EXCLUDE EOR
      do M = 1,l48hyr
!       RFQDCRD(1,M) = RFQTDCRD(1,M)*1.00
!       RFQDCRD(2,M) = RFQTDCRD(2,M)*0.96
!       RFQDCRD(3,M) = RFQTDCRD(3,M)*0.92
!       RFQDCRD(4,M) = RFQTDCRD(4,M)*0.81
!       RFQDCRD(5,M) = RFQTDCRD(5,M)*0.84
!       RFQDCRD(6,M) = RFQTDCRD(6,M)*0.55
        RFQDCRD(1,M) = RFQTDCRD(1,M)-sum(HEORPRD(1:15,M))/1000000.*0.0009
        RFQDCRD(2,M) = RFQTDCRD(2,M)-sum(HEORPRD(1:15,M))/1000000.*0.064
        RFQDCRD(3,M) = RFQTDCRD(3,M)-sum(HEORPRD(1:15,M))/1000000.*0.019
        RFQDCRD(4,M) = RFQTDCRD(4,M)-sum(HEORPRD(1:15,M))/1000000.*0.365
        RFQDCRD(5,M) = RFQTDCRD(5,M)-sum(HEORPRD(1:15,M))/1000000.*0.096
        RFQDCRD(6,M) = RFQTDCRD(6,M)-sum(HEORPRD(1:15,M))/1000000.*0.456
        RFQDCRD(7,M) = RFQTDCRD(7,M)-sum(HEORPRD(1:15,M))/1000000.*0.000
        DO I=1,l48rgn
          OGQEORPR(I,M) = (RFQTDCRD(I,M) - RFQDCRD(I,M))*365*1000
!         OGEORPRD(I,4,M) = OGQEORPR(I,M)
        ENDDO
        OGEORPRD(1,4,M) = (HEORPRD(9,M)+HEORPRD(10,M))/1000.*365*0.00
        OGEORPRD(2,4,M) = (HEORPRD(9,M)+HEORPRD(10,M))/1000.*365*0.17
        OGEORPRD(3,4,M) = (HEORPRD(9,M)+HEORPRD(10,M))/1000.*365*0.09
        OGEORPRD(4,4,M) = (HEORPRD(9,M)+HEORPRD(10,M))/1000.*365*0.63
        OGEORPRD(5,4,M) = (HEORPRD(9,M)+HEORPRD(10,M))/1000.*365*0.11
        OGEORPRD(6,4,M) = (HEORPRD(9,M)+HEORPRD(10,M))/1000.*365*0.0
        OGEORPRD(7,4,M) = (HEORPRD(9,M)+HEORPRD(10,M))/1000.*365*0.0
        OGEORPRD(8,4,M) = sum(OGEORPRD(1:7,4,M))
!       if (m.le.2005-baseyr+1) OGEORPRD(:,:,m) = 0.
      enddo
!     OGEORPRD(1,4,2013-baseyr+1) = 133.
!     OGEORPRD(2,4,2013-baseyr+1) = 14472.
!     OGEORPRD(3,4,2013-baseyr+1) = 7527.
!     OGEORPRD(4,4,2013-baseyr+1) = 65441.
!     OGEORPRD(5,4,2013-baseyr+1) = 14459.
!     OGEORPRD(6,4,2013-baseyr+1) = 0.
!     OGEORPRD(7,4,2013-baseyr+1) = 0.
!     OGEORPRD(8,4,2013-baseyr+1) = sum(OGEORPRD(1:7,4,2013-baseyr+1))

!     OGEORPRD(1,4,2012-baseyr+1) = 133.
!     OGEORPRD(2,4,2012-baseyr+1) = 14472.
!     OGEORPRD(3,4,2012-baseyr+1) = 7527.
!     OGEORPRD(4,4,2012-baseyr+1) = 65441.
!     OGEORPRD(5,4,2012-baseyr+1) = 14459.
!     OGEORPRD(6,4,2012-baseyr+1) = 0.
!     OGEORPRD(7,4,2012-baseyr+1) = 0.
!     OGEORPRD(8,4,2012-baseyr+1) = sum(OGEORPRD(1:7,4,2012-baseyr+1))

!     OGEORPRD(1,4,2011-baseyr+1) = 133.
!     OGEORPRD(2,4,2011-baseyr+1) = 14472.
!     OGEORPRD(3,4,2011-baseyr+1) = 7527.
!     OGEORPRD(4,4,2011-baseyr+1) = 65441.
!     OGEORPRD(5,4,2011-baseyr+1) = 14761.
!     OGEORPRD(6,4,2011-baseyr+1) = 0.
!     OGEORPRD(7,4,2011-baseyr+1) = 0.
!     OGEORPRD(8,4,2011-baseyr+1) = sum(OGEORPRD(1:7,4,2011-baseyr+1))

!     OGEORPRD(1,4,2010-baseyr+1) = 133.
!     OGEORPRD(2,4,2010-baseyr+1) = 14472.
!     OGEORPRD(3,4,2010-baseyr+1) = 7527.
!     OGEORPRD(4,4,2010-baseyr+1) = 65441.
!     OGEORPRD(5,4,2010-baseyr+1) = 15062.
!     OGEORPRD(6,4,2010-baseyr+1) = 0.
!     OGEORPRD(7,4,2010-baseyr+1) = 0.
!     OGEORPRD(8,4,2010-baseyr+1) = sum(OGEORPRD(1:7,4,2010-baseyr+1))

!     OGEORPRD(1,4,2009-baseyr+1) = 152.
!     OGEORPRD(2,4,2009-baseyr+1) = 5270.
!     OGEORPRD(3,4,2009-baseyr+1) = 9796.
!     OGEORPRD(4,4,2009-baseyr+1) = 66324.
!     OGEORPRD(5,4,2009-baseyr+1) = 14358.
!     OGEORPRD(6,4,2009-baseyr+1) = 955.
!     OGEORPRD(7,4,2009-baseyr+1) = 0.
!     OGEORPRD(8,4,2009-baseyr+1) = sum(OGEORPRD(1:7,4,2009-baseyr+1))

!     OGEORPRD(8,4,2008-baseyr+1) = 91980.
!     OGEORPRD(8,4,2007-baseyr+1) = 89060.
!     OGEORPRD(8,4,2006-baseyr+1) = 86870.
!     OGEORPRD(8,4,2005-baseyr+1) = 80665.

!  FUEL 1=CONV OIL&GAS, 2=TIGHT OIL, 3=EOR, 4=TIGHT GAS, 5=SHALE GAS, 6=CBM
      DO K = 1,L48FUEL-1
        DO M = 1,IJUMPYR
          OGTECHON(1,K,M) = 0.0
          OGTECHON(2,K,M) = 0.0
          OGTECHON(3,K,M) = 0.0
        ENDDO
      ENDDO

!  CLOSE INPUT FILE IFILE2
      IFILE2 = FILE_MGR('C',FNAME,NEW)

      RETURN
      END


!******************************************************************
!      DEBUG SUBCHK,SUBTRACE
!      END DEBUG
      SUBROUTINE OGOUT_L48
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! lower 48 variables
      include'ogsmoff'      ! offshore variables
      include'ogsmout'      ! ogsm global output variables
      include'pmmout'       ! lfmm global output variables
      include'ngtdmout'     ! ngmm global output variables

      INTEGER RGN(8),IR,DIST        ! HOLDS REGION NUMBERS
      REAL REMAIN1       ! HOLDS REMAINING RESOURCE LEVELS
      REAL REMAIN2
      REAL REMAIN3
      REAL TOTRSVS
      REAL TOTPRRAT
      REAL TOTRESAD
      REAL OGPRX  ! VARIABLE TO HOLD LAGGED P/R RATIO-OGSM CAT
      REAL EXTOTOIL(L48RGN)  ! TOTAL EXPECTED PRODUCTION BY REGION
      REAL EXTOTGAS(L48RGN)  ! TOTAL EXPECTED PRODUCTION BY REGION
      real ogsmres(l48rgn,l48fuel)
      real ogsmprd(l48rgn,l48fuel)
      real ogsmprr(l48rgn,l48fuel)
      real srtmp(l48well,l48rgn,confuel)
      real nfw_shr(l48rgn,confuel)
      real lagnfw_shr(l48rgn,confuel)
      real l_sr(l48well,l48rgn,confuel),xlog
      real l_srlag(l48well,l48rgn,confuel)
      real dcftot(l48rgn,confuel)
      real lagdcftot(l48rgn,confuel)
      real logprice(l48rgn,confuel)
      real laglogprc(l48rgn,confuel)
      real tot_sr(l48well,l48rgn)
      real totwls(l48well,l48rgn),cum_suc(l48well,l48rgn,confuel)
      real lagtotwls(l48well,l48rgn),lagcum_suc(l48well,l48rgn,confuel)
      real sr_beta3_int(l48well,confuel)
      real sr_beta3_end(l48well,confuel)
      real tech_dec_sr(l48well)
      real cnst_adj_sr(l48well,l48rgn,confuel)
      real pradj
      real remr(l48rgn,confuel), lagremr(l48rgn,confuel)
      REAL*8 RESSUM, PRDSUM, PRRATOIL(l48rgn)
      REAL*4 totprd,totugr
      REAL*4 PRRATTST(l48fuel,l48rgn)
      REAL*8 logistic(l48rgn), logisticlag(l48rgn)
      REAL*8 a_pr(l48rgn,2), b0_pr(2), b1_pr(2), b2_pr(l48rgn,2), rho_pr(2), b3_pr(2)
      REAL*8 RA_RATIO(l48rgn,l48fuel,MNUMYR)
!
! L48RGN  1    1    1    2    5    1    1    2    2    3    4    5    6    2    4    5    6
!                   xr1   dr1   xr2   dr2   xr3   dr3   xr4   dr4   xr5   dr5     xr6   dr6

!  SET T TO BE THE NUMBER OF YEARS FROM THE BASE YEAR (CURIYR - 1)
      T = CURIYR - L48BYR

!  COMPUTE EXPECTED PRODUCTION
      EXPRDL48 = 0.
      EXTOTOIL = 0.
      EXTOTGAS = 0.
      DO DIST=1,OGDIST
        IR = DISTMAP(DIST,1)
        IF (IR > 0 .AND. IR <= L48RGN) THEN
          OGOILPRD(DIST,oiltypes,CURIYR) = sum(OGOILPRD(DIST,1:oiltypes-1,CURIYR))
          OGENAGPRD(DIST,gastypes,CURIYR) = sum(OGENAGPRD(DIST,1:gastypes-1,CURIYR))
          DO K=1,oiltypes-1
            IF (K.eq.3.OR.K.eq.4) THEN
              EXPRDL48(IR,2) = EXPRDL48(IR,2) + OGOILPRD(DIST,K,CURIYR)*365.
              EXTOTOIL(IR) = EXTOTOIL(IR) + OGOILPRD(DIST,K,CURIYR)*365.
            ELSE
              EXPRDL48(IR,1) = EXPRDL48(IR,1) + OGOILPRD(DIST,K,CURIYR)*365.
              EXTOTOIL(IR) = EXTOTOIL(IR) + OGOILPRD(DIST,K,CURIYR)*365.
            ENDIF
          ENDDO
          DO K=1,gastypes-1
              EXPRDL48(IR,K+3) = EXPRDL48(IR,K+3) + OGENAGPRD(DIST,K,CURIYR)
              EXTOTGAS(IR) = EXTOTGAS(IR) + OGENAGPRD(DIST,K,CURIYR)
          ENDDO
        ENDIF
      ENDDO

!  ------------ COMPUTE LOWER 48 PRODUCTION ---------------
!  IF NOT STAND-ALONE (OGRUNOP(1)=0) THEN USE COMPUTED PRODUCTION
      IF (OGRUNOP(1) .EQ. 0) THEN
!  COMPUTE PRODUCTION AT THE OGSM REGION LEVEL FROM NGMM
!  SUM TO THE OGSM REGIONS
         PRDOGSM = 0.
         DO DIST=1,OGDIST
           IF (DISTMAP(DIST,1) > 0 .AND. DISTMAP(DIST,1) <= L48RGN) THEN
             PRDOGSM(DISTMAP(DIST,1)) = PRDOGSM(DISTMAP(DIST,1)) + OGRNAGPRD(DIST,GASTYPES,CURIYR)
           ENDIF
         ENDDO


         DO R = 1,L48RGN
            DO K = 1,L48FUEL
               IF (K.LE.2) THEN
!  CONVERT OIL PRODUCTION FROM MMB/DAY TO MMB/YEAR
                  IF (EXTOTOIL(R).GT.0.0) THEN
                    IF (r.eq.5) then
                    PRDL48(R,K) = (RFQTDCRD(R,CURIYR)*365.0-ogqcrrep(1,curiyr)) *  &   ! subtract oil shale
                                  (EXPRDL48(R,K)/EXTOTOIL(R))
                    else
                    PRDL48(R,K) = RFQTDCRD(R,CURIYR) * 365.0 * &
                                  (EXPRDL48(R,K)/EXTOTOIL(R))
                    endif
                  ELSE
                    PRDL48(R,K) = 0.0
                  ENDIF
               ELSE IF (EXTOTGAS(R).GT.0.0) THEN
                  PRDL48(R,K) = PRDOGSM(R) * (EXPRDL48(R,K)/ &
                                EXTOTGAS(R))
               ELSE
                  PRDL48(R,K) = 0.0
               ENDIF
            ENDDO
         ENDDO

      ELSEIF (OGRUNOP(1).EQ.1.OR.OGRUNOP(1).EQ.2) THEN  ! EXP. PROD.

         DO R=1,L48RGN
            DO K=1,L48FUEL
              PRDL48(R,K) = EXPRDL48(R,K)
            ENDDO
         ENDDO

!  SET NGMM RGN LEVEL PROD VARIABLE THE EXPECTED PRODUCTION
         DO DIST=1,OGDIST
           OGRNAGPRD(DIST,gastypes,CURIYR) = sum(OGENAGPRD(DIST,1:gastypes-1,CURIYR))
         ENDDO

      ELSEIF (OGRUNOP(1).EQ.3) THEN   !  USE EXP. OIL PROD.

         PRDOGSM = 0.
         DO DIST=1,OGDIST
           IF (DISTMAP(DIST,1) > 0 .AND. DISTMAP(DIST,1) <= L48RGN) THEN
             PRDOGSM(DISTMAP(DIST,1)) = PRDOGSM(DISTMAP(DIST,1)) + OGRNAGPRD(DIST,GASTYPES,CURIYR)
           ENDIF
         ENDDO

         DO R = 1,L48RGN
            DO K = 1,L48FUEL
               IF (K.LE.2) THEN
                  PRDL48(R,K) = EXPRDL48(R,K)
               ELSE IF (EXTOTGAS(R).GT.0.0) THEN
                  PRDL48(R,K) = PRDOGSM(R) * (EXPRDL48(R,K)/ &
                                EXTOTGAS(R))
               ELSE
                  PRDL48(R,K) = 0.0
               ENDIF
            ENDDO
         ENDDO

      ELSEIF (OGRUNOP(1).EQ.4) THEN   !  USE EXP. GAS PROD.

         PRDOGSM = 0.
         DO R=1,L48RGN
            DO K=1,L48FUEL
               IF (K.LE.2) THEN
!  CONVERT OIL PRODUCTION FROM MMB/DAY TO MMB/YEAR
                  IF (EXTOTOIL(R).GT.0.0) THEN
                    PRDL48(R,K) = RFQTDCRD(R,CURIYR) * 365.0 * &
                                  (EXPRDL48(R,K)/EXTOTOIL(R))
                  ELSE
                    PRDL48(R,K) = 0.0
                  ENDIF
               ELSE
                 PRDL48(R,K) = EXPRDL48(R,K)
                 PRDOGSM(R) = PRDOGSM(R) + EXPRDL48(R,K)
               ENDIF
            ENDDO
         ENDDO

         DO DIST=1,OGDIST
           DO K=1,GASTYPES
             OGRNAGPRD(DIST,K,CURIYR) = OGENAGPRD(DIST,K,CURIYR)
           ENDDO
         ENDDO

      ENDIF    ! END IF THIS IS A STAND-ALONE RUN OF OGSM

      DO DIST=1,OGDIST
        IR = DISTMAP(DIST,1)
        OGENAGPRD(DIST,gastypes,CURIYR) = sum(OGENAGPRD(DIST,1:gastypes-1,CURIYR))
        IF (DISTMAP(DIST,1) > 0 .AND. DISTMAP(DIST,1) <= L48RGN) THEN
          DO K=1,gastypes-1
!  ONLY ADJUST SHALE GAS TO MATCH REALIZED PRODUCTION
              if(OGENAGPRD(DIST,3,CURIYR).GT.0..AND.  &
                 (OGENAGPRD(DIST,3,CURIYR)+OGRNAGPRD(DIST,GASTYPES,CURIYR)-OGENAGPRD(DIST,GASTYPES,CURIYR)).GT.0.) THEN
                if (K.eq.3) OGRNAGPRD(DIST,K,CURIYR) = OGENAGPRD(DIST,K,CURIYR) + (OGRNAGPRD(DIST,GASTYPES,CURIYR)-OGENAGPRD(DIST,GASTYPES,CURIYR))
                if (K.ne.3) OGRNAGPRD(DIST,K,CURIYR) = OGENAGPRD(DIST,K,CURIYR)
              ELSE
                if (OGENAGPRD(DIST,gastypes,CURIYR).GT.0.) OGRNAGPRD(DIST,K,CURIYR) = OGRNAGPRD(DIST,GASTYPES,CURIYR) &
                                 * OGENAGPRD(DIST,K,CURIYR)/OGENAGPRD(DIST,gastypes,CURIYR)
              ENDIF
          ENDDO
          OGRNAGPRD(DIST,gastypes,CURIYR) = sum(OGRNAGPRD(DIST,1:gastypes-1,CURIYR))
          write(ogbug1,*) 'NGMMprd',curiyr+1989,dist,OGRNAGPRD(DIST,gastypes,curiyr),OGENAGPRD(DIST,gastypes,curiyr)
        ENDIF
      ENDDO

!  COMPUTE REALIZED PR RATIO FOR EACH REGION/FUEL
      DO R = 1,L48RGN
         DO K = 1,L48FUEL
            IF(CURRESL48(R,K).GT.0.0)THEN
              PRRATL48(R,K) = PRDL48(R,K)/CURRESL48(R,K)
            ELSE
              PRRATL48(R,K) = 0.0
            ENDIF
! ASSIGN OGPRX TO BASE (LAST YEAR'S) P/R RATIO
            OGPRX = CURPRRL48(R,K)

            OGSMPRD(R,K) = 0.0
            OGSMRES(R,K) = 0.0

         ENDDO
      ENDDO

!  CALCULATE PR RATIO AT THE OGSM REGIONS
      DO R=1,L48RGN
      DO K=3,L48FUEL
        OGSMPRR(R,K) = PRRATL48(R,K)
        RESBOYL48(R,K) = OGSMRES(R,K)
        IF (OGSMRES(R,K).GT.0.0) THEN
          PRRATL48(R,K) = OGSMPRD(R,K)/OGSMRES(R,K)
        ELSE
          PRRATL48(R,K) = 0.0
        ENDIF
        IF (K.EQ.3) WRITE(BUGOUT,*) 'prratio',curiyr+1989,r,prratl48(r,k), &
                          ogsmprr(r,k)
      ENDDO
      ENDDO

      CALL AGG_ADJ  ! adjust drilling based on realized production

!  COMPUTE BEGINING-OF-YEAR (CURIYR+1) RESERVES FOR EACH REGION/FUEL
      DO R = 1,L48RGN
         DO K = 1,L48FUEL
            RESADL48(R,K) = REPPRDL48(R,K,CURIYR)
            IF (CURIYR.LE.L48Hyr) RESADL48(R,K) = histresl48(r,k,curiyr+1)-histresl48(r,K,curiyr)-prdl48(r,K)
            RESBOYL48(R,K) = CURRESL48(R,K) - PRDL48(R,K) + &
                             RESADL48(R,K)
            IF(RESBOYL48(R,K).LT.0.0) RESBOYL48(R,K)=0.0
            IF (RESBOYL48(R,K).gt.0.and.curiyr.lt.mnumyr) PRRATL48(R,K) = REPPRDL48(R,K,CURIYR+1)/RESBOYL48(R,K)
!         if(curiyr.ge.l48hyr) write(6,*) 'dh5out8', curiyr,r,k,repprdl48(r,k,curiyr), resboyl48(r,k), prratl48(r,K)*resboyl48(r,k)
         ENDDO
      ENDDO

    DO R = 1,L48RGN
!      OGEOYRSV(13,1,CURIYR) = OGEOYRSV(13,1,CURIYR) + OGEOYRSV(R,1,CURIYR)
!      OGEOYRSV(13,2,CURIYR) = OGEOYRSV(13,2,CURIYR) + OGEOYRSV(R,2,CURIYR)
    ENDDO

!  OVERWRITE WITH HISTORY
      IF (CURIYR.LE.L48HYR) THEN
        DO K=1,L48FUEL
          DO R=1,L48RGN
            RESBOYL48(R,K) = HISTRESL48(R,K,CURIYR+1)
            if (curiyr.lt.l48hyr) PRRATL48(R,K) = HISTPRRL48(R,K,CURIYR+1)
            DO I=1,L48WELL
              wellsl48(i,r,k) = HISTWELL48(I,R,K,CURIYR)
              SRL48(I,R,K) = HISTSRL48(I,R,K,CURIYR)
            ENDDO
            ogwellsl48(r,k,curiyr) = wellsl48(1,r,k) + wellsl48(2,r,k)
            if (ogwellsl48(r,k,curiyr).gt.0) then
               ogsrl48(r,k,curiyr) = (wellsl48(1,r,k)*srl48(1,r,k)+wellsl48(2,r,k)*srl48(2,r,k))/ogwellsl48(r,k,curiyr)
            else
               ogsrl48(r,k,curiyr) = 0.
            endif
          ENDDO
        ENDDO
      ENDIF

      DO K=1,L48FUEL
        DO R=1,L48RGN
          RESADL48(R,K) = REPPRDL48(R,K,CURIYR)
          DO I=1,L48WELL
            SUCWELLL48(I,R,K) = SRL48(I,R,K) * WELLSL48(I,R,K)
            DRYWELLL48(I,R,K) = (1-SRL48(I,R,K)) * WELLSL48(I,R,K)
            IF (WELLSL48(I,R,K).lt.1.0) THEN
              SUCWELLL48(I,R,K) = 0.
              DRYWELLL48(I,R,K) = 0.
            ENDIF
          ENDDO
          DEVL48(R,K) = SUCWELLL48(2,R,K)
          OEXPL48(R,K) = SUCWELLL48(1,R,K) - NFWL48(R,K)
          IF(OEXPL48(R,K).LE.0.0) THEN
            OEXPL48(R,K) = 0.
          ENDIF
        ENDDO
      ENDDO

!  TOTAL ONSHORE WELLS DRILLED
      DO I=1,L48WELL
         DO K=1,L48FUEL
            DRILLTOTL48(I,K,CURIYR) = 0.0
            DO R=1,L48RGN
              DRILLTOTL48(I,K,CURIYR) = DRILLTOTL48(I,K,CURIYR) + &
                                        SUCWELLL48(I,R,K)
            ENDDO
         ENDDO
      ENDDO

!  ASSIGN CRUDE NEMS OUTPUT VARIABLES
      IF (CURIYR.LT.IJUMPYR) THEN
        DO R=1,L48RGN
         OGRESCO(R,CURIYR+1) = RESBOYL48(R,1)+RESBOYL48(R,2)
        ENDDO
      ENDIF

      DO R = 1,L48RGN
        DO K = 1,L48FUEL
!  ASSIGN VARIABLES THAT ARE USED FOR SUMMARY REPORTING
            REPPRDL48(R,K,CURIYR) = PRDL48(R,K)
            REPRADL48(R,K,CURIYR) = RESADL48(R,K)
            REPRSVL48(R,K,CURIYR) = CURRESL48(R,K)

!  ASSIGN THE LAG LOWER 48 RESERVE VARIABLES
            CURRESL48(R,K) = RESBOYL48(R,K)
        ENDDO
      ENDDO

      RETURN
      END


!********************************************************************
      subroutine ognxtdat(unitnum,cline)

      implicit none

      integer*4 unitnum,j,jj
      character*(*) cline

!
! read past all lines with @,!,*,+, or & in column 1

      read(unitnum,'(a)') cline
      do while ((cline(1:1) .eq. '@').or. &
                (cline(1:1) .eq. '!').or. &
                (cline(1:1) .eq. '*').or. &
                (cline(1:1) .eq. '+').or. &
                (cline(1:1) .eq. '&'))
      read(unitnum,'(a)') cline
      enddo

!
! don't read anything after a ! character
!

      do 100 j = 1,132
         if (cline(j:j).eq.'!') then
            do 99 jj = j,132
               cline(jj:jj)=' '
99          continue
            go to 101
         end if
100   continue
101   continue

      return
      end

!********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     DPRDPMM: SUBROUTINE TO CALCULATE CONVENTIONAL DOMESTIC CRUDE AND ADD
!               EOR CRUDE TO GET TOTAL CRUDE. ALASKA CRUDE IS OBTAINED FROM
!               OGSM.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE OG_OILPRD            ! BEGIN OG_OILPRD SUBROUTINE
         IMPLICIT NONE                !
                                      !
      include 'parametr'              !
      include 'ncntrl'                !
      include 'ogsmparm'              !
      include 'ogsmbfw'               !
      include 'ogsml48'               !
      include'ogsmout'      ! ogsm global output variables
      include'pmmout'       ! lfmm global output variables
      include'mxpblk'       ! expectation variables

      REAL*4  POIL(MNUMOR-1,MNUMYR)        !
      REAL*4  POILLAG(MNUMOR-1,MNUMYR)     !
      REAL*4  XPOIL(MNUMOR-1,MNUMYR)       !
      REAL*4  X                       !
      REAL    GRAV
      REAL    NATCRDPRD(MNCRUD,MNUMYR)
      REAL    NATCRDPRD_PADD(MNCRUD,MNUMYR)   ! New variable for PADD summations of crude type
      REAL CRDDIFF(l48rgn,mncrud), CRDPRD0(l48rgn,mncrud)

      RFQDCRD(MNUMOR+1,CURIYR)=0            !
      RFQDCRD(MNUMOR+2,CURIYR)=0            !
      RFQTDCRD(MNUMOR+1,CURIYR)=0           !
      RFQTDCRD(MNUMOR+2,CURIYR)=0           !
      DCRDWHP(MNUMOR,CURIYR)=0            !

      do t=1,mncrud
        natcrdprd(t,curiyr)=0.0
        natcrdprd_PADD(t,curiyr)=0.0  ! initalize new PADD varaible to zero
      enddo
      J = CURIYR

      DO R=1,MNUMOR-1
        POIL(R,J)=DCRDWHP(R,J)
        if (j.gt.1) POILLAG(R,J)=DCRDWHP(R,J-1)
        XPOIL(R,J)=XDCRDWHP(R,J)
      ENDDO
!     write (bugout,*) 'pacific-poil',j,poil(l48rgn+offnemsrgn,j),poillag(l48rgn+offnemsrgn,j)

      DO I = 1,OGDIST
        OGOILPRD(I,5,J) = sum(OGOILPRD(I,1:oiltypes-1,J))
      ENDDO                                     !   AEO97 SUPPLY CURVES END

!  CALCULATE DOMESTIC CRUDE OIL PRODUCTION (UNITS: MMBD)
!  CURRENTLY USING PMM'S HISTORICAL INPUT FILE
      IF(CURIYR.LE.L48HYR+1) THEN                ! HISTORICAL YEARS

         DO R = 1,L48RGN+OFFNEMSRGN                !  THE LAST ADJUSTMENT IS
           IF (OGPRRCO(R,J)*OGRESCO(R,J).GT.0.0) THEN    !  APPLIED TO
           ENDIF
         ENDDO
      ELSEIF(CURIYR.LE.L48HYR+STEOYRS) THEN      ! STEO YEARS
        tRFQTDCRD(:,J) = 0.
        DO I = 1,OGDIST
         IF(DISTMAP(I,1) > 0) THEN
           tRFQTDCRD(DISTMAP(I,1),J) = tRFQTDCRD(DISTMAP(I,1),J) + OGOILPRD(I,5,J)
         ENDIF
        ENDDO                                     !   AEO97 SUPPLY CURVES END
        DO I = 1,L48RGN+OFFNEMSRGN                           !   USE PRODUCTION IN WLBASIC.TXT
!       write(6,*) 'dh5out',j+1989,i,tRFQTDCRD(I,J),RFQTDCRD(I,J),(OGPRRCO(I,J)*OGRESCO(I,J))/365.
!         tRFQTDCRD(I,J) = (OGPRRCO(I,J)*OGRESCO(I,J))/365
          if (ogSTEO.eq.0) then
            if(tRFQTDCRD(I,J).GT.0.) sRFQTDCRD(I,curiyr-l48hyr) = RFQTDCRD(I,J)-tRFQTDCRD(I,J)
          endif
          RFQTDCRD(I,J) = tRFQTDCRD(I,J) + sRFQTDCRD(I,curiyr-l48hyr)
          XRFQDCRD(I,J) = RFQTDCRD(I,J)
        ENDDO                                     !   AEO97 SUPPLY CURVES END
      ELSE                                       ! PROJECTION PERIOD

       DO I = 1,L48RGN+OFFNEMSRGN 
         RFQTDCRD(I,J) = 0.
         XRFQDCRD(I,J) = 0.
       ENDDO
       DO I = 1,OGDIST                           !   AEO97 SUPPLY CURVES START
         IF(DISTMAP(I,1) > 0) THEN
           RFQTDCRD(DISTMAP(I,1),J) = RFQTDCRD(DISTMAP(I,1),J) + OGOILPRD(I,OILTYPES,J)
           XRFQDCRD(DISTMAP(I,1),J) = XRFQDCRD(DISTMAP(I,1),J) + OGOILPRD(I,OILTYPES,J)
         ENDIF
       ENDDO                                     !   AEO97 SUPPLY CURVES END
       DO I = 1,L48RGN+OFFNEMSRGN 
         if (OGRESCO(I,J) .ne. 0.0) &
         OGPRRCO(I,J) = RFQTDCRD(I,J)*365./OGRESCO(I,J)
       ENDDO

      ENDIF                                      ! END SHORT-RUN SUPPLY CURVES

      DO I=1,MNUMOR-1                            !
       IF (I.LE.L48RGN) THEN
        RFQDCRD(I,CURIYR)=RFQTDCRD(I,CURIYR)-OGQEORPR(I,CURIYR)/365/1000
       ELSE
        RFQDCRD(I,CURIYR)=RFQTDCRD(I,CURIYR)
       ENDIF
       XRFQDCRD(I,CURIYR)=RFQDCRD(I,CURIYR)
      ENDDO
      RFQTDCRD(5,CURIYR)=RFQTDCRD(5,CURIYR)+OGQCRREP(1,CURIYR)/365.  ! ADD OIL SHALE INTO ROCKY MOUNTAIN REGION

      DO I =1,MNUMOR-1
       RFQDCRD(MNUMOR+2,CURIYR)=RFQDCRD(MNUMOR+2,CURIYR)+RFQDCRD(I,CURIYR)
       RFQTDCRD(MNUMOR+2,CURIYR)=RFQTDCRD(MNUMOR+2,CURIYR)+RFQTDCRD(I,CURIYR)
      ENDDO

      DO I =1,L48RGN+OFFNEMSRGN
       RFQDCRD(MNUMOR+1,CURIYR)=RFQDCRD(MNUMOR+1,CURIYR)+RFQDCRD(I,CURIYR)
       RFQTDCRD(MNUMOR+1,CURIYR)=RFQTDCRD(MNUMOR+1,CURIYR)+RFQTDCRD(I,CURIYR)
       DCRDWHP(MNUMOR,CURIYR)=DCRDWHP(MNUMOR,CURIYR)+ &
                          RFQTDCRD(I,CURIYR)*DCRDWHP(I,CURIYR)
      ENDDO
      DCRDWHP(MNUMOR,CURIYR)=DCRDWHP(MNUMOR,CURIYR)/RFQTDCRD(MNUMOR+1,CURIYR)
      RFQDCRD(MNUMOR,CURIYR)=RFQDCRD(MNUMOR-3,CURIYR)+RFQDCRD(MNUMOR-2,CURIYR)+ &
                         RFQDCRD(MNUMOR-1,CURIYR)
      RFQTDCRD(MNUMOR,CURIYR)=RFQTDCRD(MNUMOR-3,CURIYR)+RFQTDCRD(MNUMOR-2,CURIYR)+ &
                         RFQTDCRD(MNUMOR-1,CURIYR)

! DETERMINE AVERAGE HEAT CONTENT for LFMM
      DO R=1,L48RGN
        TOTCRDPRD(R,CURIYR) = 0.
        DO t=1,mncrud
           OGCRDPRD(R,t,CURIYR) = LF_CRDPRD(R,t,CURIYR)
           if(r.eq.5.and.t.eq.3) OGCRDPRD(5,3,CURIYR) = OGCRDPRD(5,3,CURIYR) + OGQCRREP(1,CURIYR)  ! OIL SHALE medium medium sour
           TOTCRDPRD(R,CURIYR) = TOTCRDPRD(R,CURIYR) + OGCRDPRD(R,t,CURIYR)/365.
           NATCRDPRD(t,CURIYR) = NATCRDPRD(t,CURIYR) + OGCRDPRD(R,t,CURIYR)
        ENDDO
      ENDDO
      
      DO PP = 1 , MNUMPR - 2 
              
            TOTCRDPRD_PADD(PP,CURIYR) = 0.   !Initialize new variable for PADDs to zero
      
            DO t = 1 , mncrud		! loop through crude types
      
              OGCRUDEREF(PP,t,CURIYR) = LF_CRDPRD_PADD(PP,t,CURIYR) ! new PADD variable
      
      	      if(PP.eq.6.and.t.eq.3) OGCRUDEREF(6,3,CURIYR) = OGCRUDEREF(6,3,CURIYR) + OGQCRREP(1,CURIYR)  
                 
              TOTCRDPRD_PADD(PP,CURIYR) = TOTCRDPRD_PADD(PP,CURIYR) + OGCRUDEREF(PP,t,CURIYR)/365.
      
              NATCRDPRD_PADD(t,CURIYR) = NATCRDPRD_PADD(t,CURIYR) + OGCRUDEREF(PP,t,CURIYR)
      
            ENDDO
      
      ENDDO

      DO t = 1,mncrud
         grav = 141.5/(LF_APIVOL(t,curiyr)+131.5)   ! set default value
         if (natcrdprd_padd(t,curiyr).gt.0.) grav = LF_CRDVOL(t,curiyr)/NATCRDPRD_padd(t,curiyr)
         OGCRDHEAT(t,curiyr) = grav * (7.801769-1.3213*grav**2)
         if (curiyr .ge. l48hyr-1) write(bugout,'(A,I4,I6,2F12.4,F10.4)') 'type yr SG VOL HHV ',t,curiyr+1989, &
                                  grav,natcrdprd(t,curiyr),OGCRDHEAT(t,curiyr)
      ENDDO

! ASSIGN OFFSHORE, ALASKA to crude types for LFMM
      OGCRDPRD(L48RGN+1,3,CURIYR) = RFQTDCRD(L48RGN+1,curiyr)*365. ! ATLANTIC medium medium sour
      OGCRDPRD(L48RGN+2,3,CURIYR) = RFQTDCRD(L48RGN+2,curiyr)*365. ! GOM medium medium sour
      OGCRDPRD(L48RGN+3,7,CURIYR) = RFQTDCRD(L48RGN+3,curiyr)*365. ! california
      OGCRDPRD(L48RGN+OFFNEMSRGN+1,3,CURIYR) = RFQTDCRD(L48RGN+OFFNEMSRGN+1,curiyr)*365. ! AK medium medium sour
      OGCRDPRD(L48RGN+OFFNEMSRGN+2,3,CURIYR) = RFQTDCRD(L48RGN+OFFNEMSRGN+2,curiyr)*365. ! AK medium medium sour
      OGCRDPRD(L48RGN+OFFNEMSRGN+3,3,CURIYR) = RFQTDCRD(L48RGN+OFFNEMSRGN+3,curiyr)*365. ! AK medium medium sour
      
      if(curiyr.gt.l48hyr) then  ! offshore volumes in historical years are already included in the OGCRUDEREF read from wlhist.txt
        OGCRUDEREF(1,3,CURIYR) = OGCRUDEREF(1,3,CURIYR) + RFQTDCRD(L48RGN+1,curiyr)*365. ! ATLANTIC medium medium sour
        OGCRUDEREF(4,3,CURIYR) = OGCRUDEREF(4,3,CURIYR) + RFQTDCRD(L48RGN+2,curiyr)*365. ! GOM medium medium sour
        OGCRUDEREF(7,7,CURIYR) = OGCRUDEREF(7,7,CURIYR) + RFQTDCRD(L48RGN+3,curiyr)*365. ! california
      endif
      OGCRUDEREF(8,3,CURIYR) = OGCRUDEREF(8,3,CURIYR) + RFQTDCRD(L48RGN+OFFNEMSRGN+1,curiyr)*365. ! AK medium medium sour
      OGCRUDEREF(8,3,CURIYR) = OGCRUDEREF(8,3,CURIYR) + RFQTDCRD(L48RGN+OFFNEMSRGN+2,curiyr)*365. ! AK medium medium sour
      OGCRUDEREF(8,3,CURIYR) = OGCRUDEREF(8,3,CURIYR) + RFQTDCRD(L48RGN+OFFNEMSRGN+3,curiyr)*365. ! AK medium medium sour

!     OGOILPRD(76,1,CURIYR) = RFQTDCRD(L48RGN+1,curiyr)  ! Atlantic
!     OGOILPRD(80,1,CURIYR) = RFQTDCRD(L48RGN+2,curiyr)  ! GOM
!     OGOILPRD(82,1,CURIYR) = RFQTDCRD(L48RGN+3,curiyr)  ! CA
      OGOILPRD(3,1,CURIYR) = RFQTDCRD(L48RGN+OFFNEMSRGN+2,curiyr)  ! AK
      OGOILPRD(75,1,CURIYR) = RFQTDCRD(L48RGN+OFFNEMSRGN+1,curiyr)+RFQTDCRD(L48RGN+OFFNEMSRGN+3,curiyr)  ! AK

      do i = 1,ogdist
        OGOILPRD(i,5,CURIYR) = sum(OGOILPRD(i,1:4,curiyr))
      enddo

! CALIBRATE TO REPORTED VOLUMES
!     if (curiyr.le.l48hyr+3) then
        DO R=1,L48RGN
          DO t=1,mncrud
             CRDPRD0(R,t) = OGCRDPRD(R,t,CURIYR)
             if (totcrdprd(r,curiyr).gt.0.) OGCRDPRD(R,t,CURIYR) = OGCRDPRD(R,t,CURIYR) * RFQTDCRD(R,CURIYR)/TOTCRDPRD(R,CURIYR)
             crddiff(r,t) = OGCRDPRD(R,t,curiyr)-crdprd0(r,t)
          ENDDO
        ENDDO
        DO t=1,mncrud
          OGCRUDEREF(1,t,curiyr) = OGCRUDEREF(1,t,CURIYR) + crddiff(1,t)
          OGCRUDEREF(2,t,curiyr) = OGCRUDEREF(2,t,CURIYR) + crddiff(3,t) + crddiff(7,t)
          OGCRUDEREF(4,t,curiyr) = OGCRUDEREF(4,t,CURIYR) + crddiff(2,t)
          OGCRUDEREF(5,t,curiyr) = OGCRUDEREF(5,t,CURIYR) + crddiff(4,t)
          OGCRUDEREF(6,t,curiyr) = OGCRUDEREF(6,t,CURIYR) + crddiff(5,t)
          OGCRUDEREF(7,t,curiyr) = OGCRUDEREF(7,t,CURIYR) + crddiff(6,t)
        ENDDO
        DO r=1,mnumpr
        DO t=1,mncrud
          if(OGCRUDEREF(r,t,curiyr).lt.0.)  OGCRUDEREF(r,t,CURIYR) = 0.
        ENDDO
        ENDDO
!     ENDIF

      IF (curiyr+1989.ge.2009.and.curitr.eq.1) THEN
        DO r=1,12
          write(bugout,'(A,I6,I4,<mncrud+1>F7.4)') 'ogcrdprd  ',curiyr+1989,r,(OGCRDPRD(r,t,curiyr)/365.,t=1,mncrud),  &
                      sum(ogcrdprd(r,1:mncrud,curiyr))/365.
        ENDDO
          write(bugout,'(A,I6,I4,<mncrud+1>F7.4)') 'ogcrdprd  ',curiyr+1989,15,(sum(OGCRDPRD(1:mnumor-1,t,curiyr))/365.,t=1,mncrud),  &
                      sum(ogcrdprd(1:mnumor-1,1:mncrud,curiyr))/365.
      ENDIF

      If (curiyr+1989.ge.2009.and.curitr.eq.1) then
        Do PP = 1, MNUMPR - 2
          write(bugout,'(A,I6,I4,<mncrud+1>F7.4)') 'PADDCRDPRD  ',curiyr+1989,PP,(OGCRUDEREF(PP,t,curiyr)/365.,t=1,mncrud),  &
               sum(ogcruderef(PP,1:mncrud,curiyr))/365.
         ENDDO

	 write(bugout,'(A,I6,I4,<mncrud+1>F7.4)') 'PADDCRDPRD  	',curiyr+1989,MNUMOR+2,(sum(OGCRUDEREF(1:MNUMPR-2,t,curiyr))/365.,t=1,mncrud),  &
               sum(ogcruderef(1: MNUMPR-2,1:mncrud,curiyr))/365.

      ENDIF

!     DO I =1,MNUMOR-1
!      DO K =CURIYR,CURIYR+30
!       XDCRDWHP(I,K)=OLALPHA(I)*((XIT_WOP(K,1)-0.50)**OLBETA(I))
!      ENDDO
!     ENDDO

!  WRITE OUT PRODUCTION
!     write(bugout,*) 'onrefine-rfqdcrd',curiyr,curitr, &
!                     (rfqdcrd(i,curiyr),i=1,l48rgn)
!     write(bugout,*) 'onrefine-rfqtdcrd',curiyr,curitr, &
!                     (rfqtdcrd(i,curiyr),i=1,l48rgn)
!     write(bugout,*) 'onrefine-ogresco',curiyr,curitr, &
!                     (ogresco(i,curiyr),i=1,l48rgn)
!     write(bugout,*) 'onrefine-ogprrco',curiyr,curitr, &
!                     (ogprrco(i,curiyr),i=1,l48rgn)
      write(bugout,*) 'offrefine-rfqdcrd',curiyr+1989,curitr, &
                      (rfqdcrd(i,curiyr),i=l48rgn+2,l48rgn+3)
      write(bugout,*) 'offrefine-ogresco',curiyr+1989,curitr, &
                      (ogresco(i,curiyr),i=l48rgn+2,l48rgn+3)
      write(bugout,*) 'offrefine-ogprrco',curiyr+1989,curitr, &
                      (ogprrco(i,curiyr),i=l48rgn+2,l48rgn+3)

      RETURN                                     !
      END                                        ! END OG_OILPRD SUBROUTINE

!  ******************************************************************
      REAL FUNCTION OG_GROWTH(IYR,LOOKYR)

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'ogsmparm'
      include'ogsmbfw'
      include'qblk'

      INTEGER*4 LOOKYR
      INTEGER*4 IYR
      REAL*4 GASCON(-MNUMYR:MNUMYR)    ! NATURAL GAS CONSUMPTION FOR LOOKAHEAD YEARS
      REAL*4 FACTOR                    ! RESPONSE ADJUSTMENT FACTOR
      REAL*4 AVGCON               ! AVERAGE NATURAL GAS CONSUMPTION
      REAL*4 MAXGRO               ! MAXIMUM GROWTH RATE
      REAL*4 LAGGRO               ! LAGGED GROWTH RATE
      INTEGER*4 ISTART,IEND       ! INDICES

      DATA FACTOR/1.0/
      DATA MAXGRO/1.0/
!
!  ANTICIPATE ANNUAL PRICE INCREASES, IN RESPONSE TO
!  ANNUAL INCREASES IN gas DEMAND, FOR PURPOSES OF THE DCF CALCULATION
!
          AVGCON = 0.0
          IF (LOOKYR.GE.0) THEN
            ISTART = 1
            IEND = LOOKYR+1
          ELSE
            ISTART = LOOKYR+1
            IEND = 1
          ENDIF
          DO I=ISTART,IEND
              IF (IYR-1+I.LE.IJUMPYR.AND.IYR-1+I.GE.1) THEN
                GASCON(I) = QNGAS(11,IYR-1+I)
              ELSEIF (IYR-1+I.LT.1) THEN
                GASCON(I) = QNGAS(11,1)
              ELSE
                GASCON(I) = GASCON(I-1)
              ENDIF
              AVGCON = AVGCON + GASCON(I)
          ENDDO
          AVGCON = AVGCON/(IEND-ISTART+1)
          MAXGRO = 1.
          IF (LOOKYR.GE.0.AND.GASCON(1).GT.0.) MAXGRO = (AVGCON / GASCON(1))
          IF (LOOKYR.LT.0.AND.AVGCON.GT.0.) MAXGRO = (GASCON(1) / AVGCON)
          OG_GROWTH = (MAXGRO) ** FACTOR
          IF (MAXGRO.LT.1.03) OG_GROWTH = 1.0
          IF (IYR+1989.LE.L48HYR+1) OG_GROWTH = 1.0

!         WRITE(BUGOUT,*) 'OG_GROWTH',IYR+1989,LOOKYR,MAXGRO, &
!            OG_GROWTH

      RETURN
      END
!  ******************************************************************
      REAL FUNCTION OG_AVGPRC(IYR,LOOKYR,REG)

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'ogsmparm'
      include'ogsmbfw'
      include'ogsml48'
      include'ogsmout'
      include'ngtdmout'
      include'ngtdmrep'

      INTEGER*4 LOOKYR
      INTEGER*4 IYR,REG,DIST
      REAL*4 PRDGAS(mnumor,MNUMYR)    ! NATURAL GAS PRODUCTION FOR LOOKAHEAD YEARS
      REAL*4 GASPRD(mnumor,-MNUMYR:MNUMYR)    ! NATURAL GAS PRODUCTION FOR LOOKAHEAD YEARS
      REAL*4 GASWPR(mnumor,-MNUMYR:MNUMYR)    ! NATURAL GAS PRICE FOR LOOKAHEAD YEARS
      REAL*4 AVGPRD(mnumor)               ! AVERAGE NATURAL GAS PRODUCTION
      REAL*4 AVGWPR(mnumor)               ! AVERAGE NATURAL GAS PRICE
      INTEGER*4 ISTART,IEND               ! INDICES

!
!  CALCULATE HISTORICAL AVERGE GAS WELLHEAD PRICE
!  FOR PURPOSES OF THE DCF CALCULATION
!
       DO I=1,IJUMPYR
         PRDGAS(:,I) = 0
         DO DIST=1,OGDIST
           IF (DISTMAP(DIST,1) > 0 .AND. DISTMAP(DIST,1) <= L48RGN+OFFNEMSRGN) THEN
             PRDGAS(DISTMAP(DIST,1),I) = PRDGAS(DISTMAP(DIST,1),I) + OGRNAGPRD(DIST,GASTYPES,I)
           ENDIF
         ENDDO
         DO J=1,L48RGN+OFFNEMSRGN
           PRDGAS(MNUMOR,I) = PRDGAS(MNUMOR,I) + PRDGAS(J,I)
         ENDDO
       ENDDO

          AVGPRD = 0.0
          AVGWPR = 0.0
          IF (LOOKYR.GE.0) THEN
            ISTART = 1
            IEND = LOOKYR+1
          ELSE     ! do not include current year price when using historical look back
            ISTART = LOOKYR+1
            IEND = 0
          ENDIF
          DO I=ISTART,IEND
              IF (IYR-1+I.LE.IJUMPYR.AND.IYR-1+I.GE.1) THEN
                GASPRD(REG,I) = PRDGAS(REG,IYR-1+I)
                GASWPR(REG,I) = PRDGAS(REG,IYR-1+I)*OGWPRNG(REG,iyr-1+I)
              ELSEIF (IYR-1+I.LT.1) THEN
                GASPRD(REG,I) = PRDGAS(REG,1)
                GASWPR(REG,I) = PRDGAS(REG,1)*OGWPRNG(REG,1)
              ELSE
                GASPRD(REG,I) = GASPRD(REG,I-1)
                GASWPR(REG,I) = GASWPR(REG,I-1)
              ENDIF
              AVGPRD(REG) = AVGPRD(REG) + GASPRD(REG,I)
              AVGWPR(REG) = AVGWPR(REG) + GASWPR(REG,I)
          ENDDO
          IF (AVGPRD(REG).GT.0.0) THEN
            OG_AVGPRC = AVGWPR(REG)/AVGPRD(REG)
          ELSE
            OG_AVGPRC = OGWPRNG(REG,iyr)
          ENDIF

!         WRITE(BUGOUT,*) 'AVGWPR',IYR+1989,LOOKYR,REG,OG_AVGPRC

      RETURN
      END
!  ******************************************************************
      REAL FUNCTION OG_AVGOPRC(IYR,LOOKYR,REG)

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'ogsmparm'
      include'ogsmbfw'
      include'pmmout'

      INTEGER*4 LOOKYR
      INTEGER*4 IYR,REG
      REAL*4 OILPRD(mnumor,-MNUMYR:MNUMYR)    ! CRUDE OIL PRODUCTION FOR LOOKAHEAD YEARS
      REAL*4 OILWPR(mnumor,-MNUMYR:MNUMYR)    ! CRUDE OIL PRICE FOR LOOKAHEAD YEARS
      REAL*4 AVGPRD(mnumor)               ! AVERAGE CRUDE OIL PRODUCTION
      REAL*4 AVGWPR(mnumor)               ! AVERAGE CRUDE OIL PRICE
      INTEGER*4 ISTART,IEND               ! INDICES

!
!  CALCULATE HISTORICAL AVERAGE OIL WELLHEAD PRICE BY OGSM REGION
!  FOR PURPOSES OF THE DCF CALCULATION
!

          AVGPRD = 0.0
          AVGWPR = 0.0
          IF (LOOKYR.GE.0) THEN
            ISTART = 1
            IEND = LOOKYR+1
          ELSE
            ISTART = LOOKYR+1
            IEND = 1
          ENDIF
          DO I=ISTART,IEND
              IF (IYR-1+I.LE.IJUMPYR.AND.IYR-1+I.GE.1) THEN
                OILPRD(REG,I) = RFQTDCRD(REG,IYR-1+I)
                OILWPR(REG,I) = RFQTDCRD(REG,IYR-1+I)*DCRDWHP(REG,iyr-1+I)
              ELSEIF (IYR-1+I.LT.1) THEN
                OILPRD(REG,I) = RFQTDCRD(REG,1)
                OILWPR(REG,I) = RFQTDCRD(REG,1)*DCRDWHP(REG,1)
              ELSE
                OILPRD(REG,I) = OILPRD(REG,I-1)
                OILWPR(REG,I) = OILWPR(REG,I-1)
              ENDIF
              AVGPRD(REG) = AVGPRD(REG) + OILPRD(REG,I)
              AVGWPR(REG) = AVGWPR(REG) + OILWPR(REG,I)
          ENDDO
          IF (AVGPRD(REG).GT.0.0) THEN
            OG_AVGOPRC = AVGWPR(REG)/AVGPRD(REG)
          ELSE
            OG_AVGOPRC = DCRDWHP(REG,iyr)
          ENDIF

          WRITE(BUGOUT,*) 'AVGOWPR',IYR+1989,LOOKYR,REG,OG_AVGOPRC,DCRDWHP(REG,curiyr)

      RETURN
      END
!*******************************************************************************
!  ******************************************************************
      REAL FUNCTION OG_AVGOPRC2(IYR,LOOKYR,REG,CTYPE)

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'ogsmparm'
      include'ogsmbfw'
      include'ogsmout'
      include'lfmmout'

      INTEGER*4 LOOKYR
      INTEGER*4 IYR,REG,CTYPE
      REAL*4 OILPRD(mnumor,mncrud,-MNUMYR:MNUMYR)    ! CRUDE OIL PRODUCTION FOR LOOKAHEAD YEARS
      REAL*4 OILWPR(mnumor,mncrud,-MNUMYR:MNUMYR)    ! CRUDE OIL PRICE FOR LOOKAHEAD YEARS
      REAL*4 AVGPRD(mnumor,mncrud)               ! AVERAGE CRUDE OIL PRODUCTION
      REAL*4 AVGWPR(mnumor,mncrud)               ! AVERAGE CRUDE OIL PRICE
      INTEGER*4 ISTART,IEND               ! INDICES

!
!  CALCULATE HISTORICAL AVERAGE OIL WELLHEAD PRICE BY PADD and CRUDE TYPE
!  FOR PURPOSES OF THE DCF CALCULATION
!

          AVGPRD = 0.0
          AVGWPR = 0.0
          IF (LOOKYR.GE.0) THEN
            ISTART = 1
            IEND = LOOKYR+1
          ELSE
            ISTART = LOOKYR+1
            IEND = 1
          ENDIF
          DO I=ISTART,IEND
              IF (IYR-1+I.LE.IJUMPYR.AND.IYR-1+I.GE.1) THEN
                OILPRD(REG,CTYPE,I) = OGCRUDEREF(REG,CTYPE,IYR-1+I)
                OILWPR(REG,CTYPE,I) = OGCRUDEREF(REG,CTYPE,IYR-1+I)*RFCRUDEWHP(REG,CTYPE,iyr-1+I)
              ELSEIF (IYR-1+I.LT.1) THEN
                OILPRD(REG,CTYPE,I) = OGCRUDEREF(REG,CTYPE,1)
                OILWPR(REG,CTYPE,I) = OGCRUDEREF(REG,CTYPE,1)*RFCRUDEWHP(REG,CTYPE,1)
              ELSE
                OILPRD(REG,CTYPE,I) = OILPRD(REG,CTYPE,I-1)
                OILWPR(REG,CTYPE,I) = OILWPR(REG,CTYPE,I-1)
              ENDIF
              AVGPRD(REG,CTYPE) = AVGPRD(REG,CTYPE) + OILPRD(REG,CTYPE,I)
              AVGWPR(REG,CTYPE) = AVGWPR(REG,CTYPE) + OILWPR(REG,CTYPE,I)
          ENDDO
          IF (AVGPRD(REG,CTYPE).GT.0.0.AND.CURIYR+BASEYR-1+LOOKYR.GE.2012) THEN
            OG_AVGOPRC2 = AVGWPR(REG,CTYPE)/AVGPRD(REG,CTYPE)
          ELSE
            OG_AVGOPRC2 = RFCRUDEWHP(REG,CTYPE,iyr)*0.75
          ENDIF

          if(curitr.eq.1) WRITE(BUGOUT,'(A,2X,I4,I6,I4,I4,2F12.5)') 'AVGOWPR2',IYR+1989,LOOKYR,REG,CTYPE,OG_AVGOPRC2,RFCRUDEWHP(REG,CTYPE,curiyr)

      RETURN
      END
! ****************************************************************************
! ****************************************************************************
                               SUBROUTINE OGFOR_OS
! ****************************************************************************
! ****************************************************************************
!
!
!                            Oil Shale Supply Module
!                            Last Updated August 2010
!
! Updated 8/30/05 - 2nd plant built 5 years after 1st plant,  (commented out, see 9/9 notes)
!                   3rd plant built 3 years after 2nd plant,  (commented out, see 9/9 notes)
!                   4rd plant built 2 years after 3rd plant,  (commented out, see 9/9 notes)
!                   5th and subsequent plants built 1 year after prior plant
! Updated 9/7/05 -  Annual Plant Capacity Factor Inserted in Production and NPV
!                   Discount Factor now based on Macro variables & debt/equity ratio
!                   Net Present Value Calc incorporates Electricity Consumption and Prices
!                            & Gas Production and Prices
! Updated 9/9/05 -  Natural Gas Production & Electricity Consumption assigned to
!                            global variables for use elsewhere in NEMS,
!                   Plant construction period reduced 1 year to 5 years
!                   1 new plant can be built each forecast year
! uPDATED 9/12/05 - Corrected DCF calculation
!                   Reverted to 8/30/05 plant building schedule
! Updated 11/16/07 - Changed Earliest Commercialization Year from 2010 to 2017
!                    Changed Project Life from 20 to 25 Years to account for the extended production ramp-up
!                    Changed Production Ramp-Up Period from 2 to 5 Years
! Updated 10-2-08   Increased oil shale capital costs by 50 percent to reflect higher capital cost environment.
! Updated 12-30-09 - Included a carbon dioxide emissions charge in DCF calculation.
! Updated July 2010 - Reduced standard plant size from 100,000 bbl/day down to 50,000 bbl/day
!                   - Reduced construction time from 5 years down to 3.
!                   - Doubled maximum potential domestic oil shale production from 1 MMB/D to 2 MMB/D
!                   - Created a market penetration algorithm based on the project's relative profitability vis-a-vis
!                         oil prices; used linear penetration curve (a'la OLOGSS), based on 40-year penetration rate timeframe.
!                   - Market penetration algorithm allows multiple projects to be built in the same year.
! Updated 8-26-10   - Adjusted market penetration algorithm so that new plant calculation takes into account the expected
!                         plant capacity factor of 90 percent per year.
!                   - Equity share reduced from 70 percent to 60 percent
!                   - Market risk premium for equity reduced from 6.75% to 6.5%
!                   - Equity volatility beta increased from 1.75 to 1.8
!                   - Project life reduced from 25 to 23 years to reflect shorter construction period, operating life 20 years.
! Updated 11-19-10  - Added EXTRARISK variable to cost of capital and cost of equity to take into account possible GHG legislation
!
! Updated 11-18-11  - Set OS_START_YR = 2100 so that oil shale supplysubmodule is deactivated for AEO2012 projections
! Updated 7/17/14	- Moved variable declarations to ogsml48
!			- Defined parameters in wlbasic.txt
!
! ****************************************************************************
! ****************************************************************************

  IMPLICIT NONE

  INCLUDE 'parametr'                        ! nems parameters
  INCLUDE 'ncntrl'                          ! nems control variables
  INCLUDE 'ogsmparm'                        ! ogsm parameters
  INCLUDE 'ogsml48'                         ! ogsm variables
  INCLUDE 'ogsmbfw'                         ! ogsm variables
  INCLUDE 'emmparm'                         ! electric utility parameter file that includes some emissions varbiables
  INCLUDE 'emission'                        ! emissions allowance variables
  INCLUDE 'macout'                          ! Macro variables
  INCLUDE 'pmmout'                          ! PMM variables
  INCLUDE 'ogsmout'                         ! OGSM variables
  INCLUDE 'mpblk'                           ! NEMS price variables
  
  REAL wlbasic
  REAL WOP				    ! World oil price


  ! **********************************************************************
  ! Note: Parameters read from input file in OGINIT_BFW subroutine 
  ! **********************************************************************

    

  TM1 = CURIYR                              ! Current year variable TM1 = 1,2,3,etc.
! LASTYR                                      Last year in projection, used for looping through time, yr 2030 = "41"



! Initialize current year variables

  OS_WRITE = 1                              ! Intermediate variables report writer (ON = 1, OFF = "NOT" 1)
  OS_OPROD = 0.0                            ! Initialize current year oil shale syncrude production, across all operating plants.
  OS_GPROD = 0.0                            ! Initialize current year natural gas production, across all operating plants.
  OS_ELCONS = 0.0                           ! Initialize current year electricity consumption, across all operating plants.
  OS_TECH_RATE = 0.01                       ! Oil Shale Project Technological Progress Rate (percent/yr cost reduction)
  MAX_PROD = 0.0                            ! Temporary variable for OS_MAX_PROD and modulated by OS_PROFIT (July 2010)
  MAX_PROD1 = 0.0                           ! Temporary variable for MAX_PROD (July 2010)
  OS_PLANTS_NEW = 0                         ! Number of New Plants being built in single year (July 2010)
  OS_EXTRARISK = EXTRARISK(TM1+1989)        ! EXTRARISK matrix indices start with BASEYR (1990) and end with ENDYR (2050)



  ! Convert tech rate for technology cases
      IF (CURIYR.GE.TECHYR) OS_TECH_RATE = OS_TECH_RATE * TECHADJ_ON(1,2)

  ! Convert Shale oil price [OS_WOP(TM1)] from 1987$ into 2004$
      WOP = OS_WOP(TM1) * MC_JPGDP(15)                                    ! OS_WOP(T), global Shale oil price variable 

  ! Convert natural gas prices from 1987$ into 2004$
      OS_GAS_PRICE = OGPRCL48(5,3,1) * ( 1.083 / .732 )                   ! OGPRCL48(Region, Fuel, Current Yr), wellhead gas price

  ! Convert carbon price per kilogram (1987 $) into carbon dioxide price per metric ton (2004 $)
      OS_EMETAX = EMETAX(1,TM1) * 1000.0 * ( 12.0 / 44.0 ) * ( 1.083 / .732 ) ! 12/44 is the relative atomic weight of carbon and CO2

  ! Convert oil shale electricity cost into $/kwh and 2004$
      OS_ELEC_PRICE = PELIN(8,TM1) * ( 1.083 / .732 ) * 0.003412         ! Convert $/MMBtu into $/kwh, and 1987$ into 2004$
                                                                          !    1 kwh = 3412 Btu

  OS_MINE_CST = OS_MINE_CST_TON * (42.0 / (OS_GALLON_TON * OS_CONV_EFF))  ! Calculation of the mining cost per oil shale bbl

  ! ****************************************************************************************************************
  ! Calculate Discount Rate based on Tom Lee's formulas; global variables in fractional rather than percentage terms
  ! ****************************************************************************************************************

       OS_DISCOUNT_RATE = (((1 - OS_EQUITY_SHARE) * (MC_RMCORPBAA(TM1)/100. + OS_DEBT_PREMIUM)) * (1 - OS_CORP_TAX_RATE)) + &
                          (OS_EQUITY_SHARE * ((OS_EQUITY_PREMIUM * OS_EQUITY_VOL) + MC_RMTCM10Y(TM1)/100. ))
       OS_DISCOUNT_RATE = ((1.+OS_DISCOUNT_RATE+OS_EXTRARISK)/(1.+INFL)) - 1  ! CONVERT FROM NOMINAL TO REAL

       ! Print out discount rate calc valuables if OS_WRITE = 1

       IF(OS_WRITE.EQ.1)THEN
           WRITE(BUGOUT,*) ' OSDISR PARAMETER REPORT FOR YEAR =        ',(1989 + TM1)
           WRITE(BUGOUT,*) ' OSDISR BBB Corp. Bond Rate =              ',MC_RMCORPBAA(TM1)/100.
           WRITE(BUGOUT,*) ' OSDISR 10yr Treasury Bond Rate =          ',MC_RMTCM10Y(TM1)/100.
       END IF

  ! ***********End Discount Rate Section****************

  IF ((TM1+1989).EQ.OS_START_YR) THEN                                   ! Initialize OS_PLANT_CONST and OS_NEXT_CONST_YR
          OS_PLANTS = 0                                                 ! Zero plants in operation and under construction (July 2010)
          OS_PLANTS_CONST = 0                                           ! Zero plants built initially
  END IF


  ! *************************************************************************************************************************
  ! Initilize Future oil shale costs for plants considered in year "T". Based on the expected rate of technological progress.
  ! *************************************************************************************************************************

  IF(TM1.EQ.1) THEN                                                     ! Initialization Section

     DO ROW=1,(LASTYR-16)                                               ! 1 row for each year spanning 2005 thru 2030
       DO COL=1,(MNUMYR+5),1                                            ! 1 column for each year from 2005 through 2055
         OS_PROJ_OPROD(ROW,COL)=0.0                                     ! Initialize oil shale production martrix
         OS_PROJ_GPROD(ROW,COL)=0.0                                     ! Initialize natural gas production matrix
         OS_PROJ_ELCONS(ROW,COL)=0.0                                    ! Initialize electricity consumption matrix
       END DO
     END DO

     DO ROW=1,MNUMYR+5                                                  ! Initialize OS_YEARS for report writers
       OS_YEARS(ROW) = 1989 + ROW                                       !    OS_YEARS extents to 2055
     END DO

     DO TM2=1,LASTYR                                                    ! Initialize oil shale costs matrix at OS_START_YR
         IF(TM2.LT.(OS_START_YR - 1989)) THEN                           ! Costs are set to zero prior to OS_START_YR. (July 2010)
            OS_MINE_CST_BBL(TM2) = 0.0
            OS_INVEST(TM2) = 0.0
            OS_OPER_CST(TM2) = 0.0
         ELSE
                  IF(TM2.EQ.(OS_START_YR - 1989)) THEN                  ! In OS_START_YR, initialize costs. (July 2010)
                     OS_MINE_CST_BBL(TM2) = OS_MINE_CST
                     OS_INVEST(TM2) = OS_PLANT_INVEST
                     OS_OPER_CST(TM2) = OS_PLANT_OPER_CST
                  ELSE                                                  ! After OS_START_YR, reduce costs by tech rate. (July 2010)
                     OS_MINE_CST_BBL(TM2) = OS_MINE_CST_BBL(TM2-1) * (1 - OS_TECH_RATE)
                     OS_INVEST(TM2) = OS_INVEST(TM2-1) * (1 - OS_TECH_RATE)
                     OS_OPER_CST(TM2) = OS_OPER_CST(TM2-1) * (1 - OS_TECH_RATE)
                  END IF
         END IF
     END DO
  END IF                                                         ! Close initialization routine

  ! ********************************************************************
  ! Plants cannot be built prior to OS_START_YR
  ! ********************************************************************

  IF ((TM1+1989).GE.OS_START_YR) THEN                            ! Does not permit plants to be built prior to OS_START_YR

  ! ********************************************************************
  ! Determine discounted cash flow of projects considered at time "TM1"
  ! ********************************************************************

     DCF = OS_DCF(OS_INVEST(TM1), OS_MINE_CST_BBL(TM1), OS_OPER_CST(TM1), OS_CAP_FACTOR, OS_PRJ_SIZE, OS_PRJ_LIFE, OS_PRJ_CONST, WOP, &
                          OS_ROYAL_RATE, OS_CORP_TAX_RATE, OS_DISCOUNT_RATE, OS_GAS_PRICE, OS_GAS_PROD, OS_ELEC_PRICE, OS_ELEC_CONSUMP, &
                          OS_EMETAX, OS_CO2EMISS, OS_WRITE, TM1)

     OS_PROFIT = DCF / OS_PLANT_INVEST                     ! Relative profitability determines the rate of market pennetration (July 2010)

  ! Temporary write statement.  (July 2010)

     IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (CURIYR.EQ.1) WRITE(bugout,1666)
        WRITE(BUGOUT,1667) CURIYR+1989,OS_PROFIT,DCF,OS_PLANT_INVEST
     ENDIF

     1667 FORMAT('OSPROFX ',I4,3F17.2)
     1666 FORMAT('OSPROFX YEAR','        OS_PROFIT','              DCF','  OS_PLANT_INVEST')

  ! ********************************************************************
  ! Determine whether to build plant
  ! ********************************************************************

  ! Oil Shale Penetration Rate Algorithm - Relative profitability regulates the maximum oil shale production level
  !     Based on OLOGSS linear penetration algorithm as of July 2010

     IF (OS_PROFIT.GT.0) THEN
        MAX_PROD1 = OS_MAX_PROD * (OS_PROFIT / (1 + OS_PROFIT))                     ! Insures that multiplier ranges between 0 and 1                  
        MAX_PROD = MAX_PROD1 * ((TM1 - (OS_START_YR - 1989)) / OS_PENETRATE_YR )    ! Linear penetration curve

        ! Rounds off to the lowest number of new plants. (Aug. 2010)
        OS_PLANTS_NEW = INT((MAX_PROD - (OS_PLANTS * OS_PRJ_SIZE * OS_CAP_FACTOR)) / (OS_PRJ_SIZE * OS_CAP_FACTOR))   
           IF (OS_PLANTS_NEW.GT.0.AND.OS_PLANTS.LT.OS_MAX_BLD)  THEN
              OS_PLANTS = OS_PLANTS + OS_PLANTS_NEW                                ! Increment plant counter
           ELSE 
              OS_PLANTS_NEW = 0
           END IF
      END IF

! Temporary write statement.  (July 2010)

     IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (CURIYR.EQ.1) WRITE(bugout,2666)
        WRITE(BUGOUT,2667) CURIYR+1989,MAX_PROD1,MAX_PROD,OS_PLANTS,OS_PLANTS_NEW
     ENDIF

     2666 FORMAT('OSPENX YEAR','  MAX_PROD1','   MAX_PROD','  OS_PLANTS','  OS_PL_NEW')
     2667 FORMAT('OSPENX ',I4,F11.2,F11.2,2I11)
    

!*********************************************************
!   Load Oil/Gas Production Matrices for New Plant(s)
!*********************************************************

! Prior formulation only permitted one new plant to be built at a time; 
!      new formulation permits multiple plant starts in the same year (July 2010)

             ROW = TM1-15                                                ! The TM1-15 is to align matrix that starts in 2005
             
             ! First year of operation at 1/2 production, 1000 bbl/day; for new plants (July 2010)
             OS_PROJ_OPROD(ROW,TM1-15+OS_PRJ_CONST) = OS_PLANTS_NEW * ((OS_PRJ_SIZE * OS_CAP_FACTOR) / (2.0 * 1000.0))
             OS_PROJ_GPROD(ROW,TM1-15+OS_PRJ_CONST) = OS_PLANTS_NEW * (OS_GAS_PROD * OS_CAP_FACTOR) / 2.0
             OS_PROJ_ELCONS(ROW,TM1-15+OS_PRJ_CONST) = OS_PLANTS_NEW * (OS_ELEC_CONSUMP *  OS_CAP_FACTOR) / 2.0


             ! Second year with operation full production, 1000 bbl/day; for new plants (July 2010)
             DO COL = (TM1-15+OS_PRJ_CONST+1),MNUMYR+5,1
                OS_PROJ_OPROD(ROW,COL) = OS_PLANTS_NEW * ((OS_PRJ_SIZE * OS_CAP_FACTOR) / 1000.0)
                OS_PROJ_GPROD(ROW,COL) =   OS_PLANTS_NEW * OS_GAS_PROD * OS_CAP_FACTOR
                OS_PROJ_ELCONS(ROW,COL) =  OS_PLANTS_NEW * OS_ELEC_CONSUMP *  OS_CAP_FACTOR
             END DO

  ! **********************************************************************************************************************

    ! Calculate current year oil and gas production, and electricity consumption across all existing oil shale plants
    DO ROW=1,IJUMPYR-10,1
       OS_OPROD = OS_OPROD + OS_PROJ_OPROD(ROW,TM1-15)      ! Add oil shale production in current yr across all plants(BBL/Day).
       OS_GPROD = OS_GPROD + OS_PROJ_GPROD(ROW,TM1-15)      ! Add natural gas production in current yr across all plants (Mcf/yr)
       OS_ELCONS = OS_ELCONS + OS_PROJ_ELCONS(ROW,TM1-15)   ! Add electricity consumption in current yr across all plants (kwh/yr)
    END DO

  END IF                                                    ! Close statement: IF ((TM1+1989).GE.OS_START_YR)

    ! Pass results to rest of model
    OGQCRREP(1,TM1) = OS_OPROD * 0.365                     ! Pass current yr oil shale production to rest of model,
                                                            !     expressed in million barrels per year
    OGSHALENG(TM1)  = OS_GPROD / 1000000.                  ! Pass current year natural gas production to rest of model,
                                                            !     expressed in billion cubic feet per year
    OGELSHALE(TM1)  = (OS_ELCONS * 3412.) / 1.0E12         ! Pass current year electricity consumption to rest of model,
                                                            !     expressed in trillion Btus per year

  ! ********************WRITE STATEMENTS*********************************************************************************

  IF (OS_WRITE.EQ.1) THEN

      IF (TM1.EQ.1)  THEN                                             ! Print header in first year
          WRITE(BUGOUT,102) (OS_YEARS(TM2), TM2=16,61,1)
          WRITE(BUGOUT,104) (OS_YEARS(TM2), TM2=16,61,1)
          WRITE(BUGOUT,106) (OS_YEARS(TM2), TM2=16,61,1)
      END IF

      IF((TM1 + 1989).GE.2005) THEN
          WRITE(BUGOUT,103) (TM1+1989), (OS_PROJ_OPROD((TM1-15),TM2),TM2=1,46), OS_OPROD                 ! print oil production profile in 1,000 BBL/day
          WRITE(BUGOUT,105) (TM1+1989), ((OS_PROJ_GPROD((TM1-15),TM2)/1E6),TM2=1,46), (OS_GPROD / 1E6)   ! print nat gas production profile in Bcf/yr
          WRITE(BUGOUT,107) (TM1+1989), ((OS_PROJ_ELCONS((TM1-15),TM2)/1E6),TM2=1,46), (OS_ELCONS / 1E6) ! print electricity consumption in Million kwh per year
      END IF

  END IF

      102 FORMAT ('OSOPROD    ','YEAR      ', 46I10)                    ! Oil Production Formats
      103 FORMAT ('OSOPROD    ', I10, 47F10.1)                          ! Oil Production Formats

      104 FORMAT ('OSGPROD    ','YEAR      ', 46I10)                    ! Nat Gas Production Formats
      105 FORMAT ('OSGPROD    ', I10, 47F10.1)                          ! Nat Gas Production Formats

      106 FORMAT ('OSECON     ','YEAR      ', 46I10)                    ! Electricity Consumption Formats
      107 FORMAT ('OSECON     ', I10, 47F10.1)                          ! Electricity Consumption Formats

RETURN
END SUBROUTINE
!  ******************************************************************************************************************
!  ***********************************End of Oil Shale Subroutine****************************************************
!  ******************************************************************************************************************


!  **************************************************************************
!  ************************* Oil Shale DCF Function *************************
!  **************************************************************************

REAL FUNCTION OS_DCF(INVEST, MINE_CST, OPER_CST, PRJ_CAP_FAC, PRJ_SIZE, PRJ_LIFE, PRJ_CONST, WOPP, ROYAL_RATE, &
             TAX_RATE, DISC_RATE, GAS_PRICE, GAS_PROD, ELEC_PRICE, ELEC_CONS, CO2_TAX, CO2_EMISS, WRITER, TM)

     IMPLICIT NONE

     include'parametr'     ! nems dimension parameters
     include'ncntrl'       ! nems control variables
     include'ogsmparm'     ! ogsm parameter file
     include'ogsmbfw'      ! ogsm system variables

!  FUNCTION PARAMETER DECLARATIONS
     INTEGER TM                                  ! CURIYR variable passed from OGFOR_OS
     INTEGER PRJ_LIFE                            ! Project operating lifetime (years)
     INTEGER PRJ_CONST                           ! Project construction period (years)
     INTEGER WRITER                              ! Turns DCF report write on or off, ON=1, OFF=0
     REAL INVEST                                 ! Size of investment
     REAL MINE_CST                               ! Mining cost per barrel of oil
     REAL OPER_CST                               ! Annual retort/upgrade plant operating cost
     REAL PRJ_CAP_FAC                            ! Annual capacity factor (fraction per year)
     REAL PRJ_SIZE                               ! Oil shale production (barrels per day)
     REAL WOPP                                   ! World Oil Price
     REAL ROYAL_RATE                             ! Royalty rate
     REAL TAX_RATE                               ! Corporate tax rate
     REAL DISC_RATE                              ! Real discount rate
     REAL GAS_PRICE                              ! Gas Price $/Mcf, 2004$
     REAL GAS_PROD                               ! Gas Production per year at full plant capacity (100,000 bpd)
     REAL ELEC_PRICE                             ! Electricity Price $/kwh, 2004$
     REAL ELEC_CONS                              ! Electricity Consumption per year at full capacity (100,000 bpd)
     REAL CO2_TAX                                ! Carbon Dioxide emissions tax (2004 dollars per metric tonne of CO2)
     REAL CO2_EMISS                              ! Carbon Dioxide emissions per day (Metric tonnes per day at 100,000 bpd)

!  FUNCTION LOCAL VARIABLES
     REAL CASH_FLOW(PRJ_LIFE + PRJ_CONST)        ! Cash Flow per yr throughout project life; 
                                                 !           dimensioned by proj lifetime plus proj construction period
     REAL OIL_PROD                               ! Total annual oil shale production
     REAL TOT_REVENUE                            ! Total annual project revenues
     REAL OIL_REVENUE                            ! Total annual oil revenues
     REAL GAS_REVENUE                            ! Natural gas revenues per year
     REAL ELEC_COST                              ! Total electricity cost per year
     REAL CO2_COST                               ! Total carbon dioxide emissions cost per year
     REAL TOT_COST                               ! Total annual cost
     REAL DEPREC                                 ! Project annual depreciation cost
     REAL ROYAL                                  ! Annual production royalty cost
     REAL PRJ_MINE_CST                           ! Project mining costs
     REAL DEPREC_TAX_CREDIT                      ! Depreciation Tax Credits
     REAL PRETAX_CASH_FLOW                       ! Revenues minus Expenses
     REAL NET_CASH_FLOW                          ! Net present value of the discounted cash flow
     REAL DISCOUNT_FACTOR                        ! Annual factor used to discount cash flows
     INTEGER TM2                                           ! Time counter

     DEPREC = INVEST / PRJ_LIFE                  ! Annual depreciation charge, straight line depreciation
     DEPREC_TAX_CREDIT = TAX_RATE * DEPREC       ! Annual depreciation income tax credits


     ! ******************************************************************************************************
     ! Determine annual cash flows for each year of the project's life (construction time plus plant life)
     ! ******************************************************************************************************

     DO TM2 = 1,(PRJ_LIFE + PRJ_CONST),1                         ! Initialize Cash Flow Vector
        CASH_FLOW(TM2) = 0.0
     END DO


     ! Revenues per year for an entire year, at the project capacity factor
       OIL_PROD = PRJ_SIZE * PRJ_CAP_FAC * 365                   ! Project oil production at full capacity
       OIL_REVENUE = OIL_PROD * WOPP                             ! Oil Production multiplied by price
       GAS_REVENUE = GAS_PROD * GAS_PRICE * PRJ_CAP_FAC          ! Gas production per yr at full capacity multiplied price and cap fac

       TOT_REVENUE = OIL_REVENUE + GAS_REVENUE

     ! Costs per year for an entire year, at the project capacity factor
       ROYAL = TOT_REVENUE * ROYAL_RATE                          ! Royalties collected from oil and gas revenues
       PRJ_MINE_CST = MINE_CST * PRJ_SIZE * 365 * PRJ_CAP_FAC    ! Per bbl mining cost x annual production
       ELEC_COST = ELEC_CONS * ELEC_PRICE * PRJ_CAP_FAC          ! Electricity Cost per year
       CO2_COST = CO2_TAX * CO2_EMISS * 365 * PRJ_CAP_FAC        ! Carbon dioxide emissions cost per year

       TOT_COST = OPER_CST + ROYAL + PRJ_MINE_CST +  ELEC_COST + CO2_COST

       PRETAX_CASH_FLOW = TOT_REVENUE - TOT_COST                 ! Expected cash flow in each year

     DO TM2 = 1,(PRJ_LIFE + PRJ_CONST),1

            IF(TM2.LE.PRJ_CONST) THEN                                      ! During construction only negative cash flow
                CASH_FLOW(TM2)= -1 * (INVEST / PRJ_CONST)        ! Investment prorated equally over construction period
            ELSE
                IF(TM2.EQ.(PRJ_CONST + 1)) THEN
                       ! Project operates at half capacity in the 1st yr, so cashflow is half the value
                       
                       IF(PRETAX_CASH_FLOW.GT.0) THEN
                             CASH_FLOW(TM2) = ((0.5 * PRETAX_CASH_FLOW * (1 - TAX_RATE))) + DEPREC_TAX_CREDIT
                       ELSE
                             CASH_FLOW(TM2) = (0.5 * PRETAX_CASH_FLOW + DEPREC_TAX_CREDIT)
                       END IF
                ELSE
                       ! Project operates at full capacity in subsequent years
                       IF(PRETAX_CASH_FLOW.GT.0) THEN
                             CASH_FLOW(TM2) = (PRETAX_CASH_FLOW * (1 - TAX_RATE)) + DEPREC_TAX_CREDIT
                       ELSE
                             CASH_FLOW(TM2) = PRETAX_CASH_FLOW + DEPREC_TAX_CREDIT
                       END IF
                END IF
            END IF
     END DO
     ! *********************************************************************************************************
     ! Now discount revenue stream and sum the across time to calculate the net present value of the cash flow.
     ! *********************************************************************************************************

     NET_CASH_FLOW = 0.0                             ! Initialize the net cash flow variable to 0.
     DISCOUNT_FACTOR = 1.0                           ! Initialize discount factor.

     DO TM2 = 1, (PRJ_LIFE + PRJ_CONST), 1
           DISCOUNT_FACTOR = DISCOUNT_FACTOR * ( 1 / (1 + DISC_RATE))
           NET_CASH_FLOW = NET_CASH_FLOW + (DISCOUNT_FACTOR * CASH_FLOW(TM2))
     END DO

     ! ********************************************************************************************************

     OS_DCF = NET_CASH_FLOW                          ! Return DCF value to main oil shale program

     ! ********************************************************************************************************
     ! Write DCF Parameter Report and Results for time "TM" (time counter passed to Function)
     ! ********************************************************************************************************

     IF (WRITER.EQ.1) THEN
         WRITE(BUGOUT,*) ' OSDCF PARAMETER REPORT FOR YEAR =       ',(1989 + TM)
         WRITE(BUGOUT,*) ' OSDCF PLANT INVESTMENT ($1E6)           ',INVEST/1E6
         WRITE(BUGOUT,*) ' OSDCF MINING COST PER BARREL ($/bbl)    ',MINE_CST
         WRITE(BUGOUT,*) ' OSDCF ANNUAL MINING COST ($1E6)         ',PRJ_MINE_CST / 1E6
         WRITE(BUGOUT,*) ' OSDCF PLANT OPERATING COST ($1E6)       ',OPER_CST / 1E6
         WRITE(BUGOUT,*) ' OSDCF PROJECT SIZE                      ',PRJ_SIZE
         WRITE(BUGOUT,*) ' OSDCF WORLD OIL PRICE                   ',WOPP
         WRITE(BUGOUT,*) ' OSDCF NATURAL GAS PRICE                 ',GAS_PRICE
         WRITE(BUGOUT,*) ' OSDCF ELECTRICITY PRICE                 ',ELEC_PRICE
         WRITE(BUGOUT,*) ' OSDCF CO2 EMISSIONS TAX ($/metric ton)  ',CO2_TAX
         WRITE(BUGOUT,*) ' OSDCF PROJECT LIFE                      ',PRJ_LIFE
         WRITE(BUGOUT,*) ' OSDCF PROJECT CONSTRUCT TIME            ',PRJ_CONST
         WRITE(BUGOUT,*) ' OSDCF TOTAL ANNUAL REVEUNES             ',TOT_REVENUE / 1E6
         WRITE(BUGOUT,*) ' OSDCF ANNUAL OIL REVENUE ($1E6)         ',OIL_REVENUE / 1E6
         WRITE(BUGOUT,*) ' OSDCF ANNUAL GAS REVENUE ($1E6)         ',GAS_REVENUE / 1E6
         WRITE(BUGOUT,*) ' OSDCF TOTAL ANNUAL COST                 ',TOT_COST / 1E6
         WRITE(BUGOUT,*) ' OSDCF ANNUAL ELECTRICITY CST ($1E6)     ',ELEC_COST / 1E6
         WRITE(BUGOUT,*) ' OSDCF ANNUAL CO2 EMISSIONS COSTS ($1E6) ',CO2_COST/ 1E6
         WRITE(BUGOUT,*) ' OSDCF ANNUAL DEPRECIATION               ',DEPREC
         WRITE(BUGOUT,*) ' OSDCF DEPREC TAX CREDIT ($1E6)          ',DEPREC_TAX_CREDIT / 1E6
         WRITE(BUGOUT,*) ' OSDCF OIL ROYALTY RATE                  ',ROYAL_RATE
         WRITE(BUGOUT,*) ' OSDCF ANNUAL ROYALTY COST($1E6)         ',ROYAL/1E6
         WRITE(BUGOUT,*) ' OSDCF COPORATE TAX RATE                 ',TAX_RATE
         WRITE(BUGOUT,*) ' OSDCF REAL DISCOUNT RATE                ',DISC_RATE
         WRITE(BUGOUT,*) ' OSDCF NET CASH FLOW ($1E6)              ',NET_CASH_FLOW /1E6
         WRITE(BUGOUT,*) ' OSDCF <<<<<<<<<<<<<<<CASH FLOW FOR EACH YEAR>>>>>>>>>>>>>>>>>>>>>>>> '
         WRITE(BUGOUT,100)
         WRITE(BUGOUT,101) ((CASH_FLOW(TM2)/1E6), TM2=1,(PRJ_LIFE + PRJ_CONST))  ! Reported in $1E6

         100 FORMAT ('OSDCF    ','YEAR=1    ','YEAR=2    ','YEAR=3    ','YEAR=4    ','YEAR=5    ','YEAR=6    ', &
                      'YEAR=7    ','YEAR=8    ','YEAR=9    ','YEAR=10   ','YEAR=11   ','YEAR=12   ', &
                      'YEAR=13   ','YEAR=14   ','YEAR=15   ','YEAR=16   ','YEAR=17   ','YEAR=18   ', &
                      'YEAR=19   ','YEAR=20   ','YEAR=21   ','YEAR=22   ','YEAR=23   ','YEAR=24   ', &
                      'YEAR=25   ','YEAR=26   ','YEAR=27   ','YEAR=28   ','YEAR=29   ','YEAR=30   ')
         101 FORMAT ('OSDCF   ', 30F10.1)
     END IF

RETURN
END FUNCTION
!************************************************************************************************************
!************************************End Oil Shale DCF function**********************************************
!************************************************************************************************************


!**********************************************************************
!  THIS SUBROUTINE SPLITS GAS PRODUCTION FROM THE NGMM INTO FUEL TYPE
!**********************************************************************
      SUBROUTINE OG_GASPRD
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! lower 48 variables
      include'ogsmak'       ! alaska variables
      include'ogsmout'      ! ogsm global output variables
      include'ngtdmout'     ! ngmm global output variables

      REAL EXTOTGAS(L48RGN)  ! TOTAL EXPECTED PRODUCTION BY REGION
      REAL PRDTMP(L48RGN,L48FUEL)  ! TOTAL EXPECTED PRODUCTION BY REGION
      REAL EPRDL48(L48RGN,GASTYPES-1)  ! TOTAL EXPECTED PRODUCTION BY REGION
      REAL EPRDOFF(3)  ! TOTAL EXPECTED PRODUCTION BY REGION
      REAL EPRDADOFF(3)  ! TOTAL EXPECTED PRODUCTION BY REGION
      INTEGER DIST, IR, CNT
      REAL DISTSHR(4,OGDIST)     ! DISTRICTS SHARE OF REGIONAL PRODUCTION BY FUEL TYPE
      REAL DISTSHR0(5,OGDIST)     ! DISTRICTS SHARE OF REGIONAL PRODUCTION BY FUEL TYPE
      REAL DISTSHR1(4,OGDIST)     ! DISTRICTS SHARE OF REGIONAL PRODUCTION BY FUEL TYPE
      REAL TEMPSUM(L48RGN,4)     !
      REAL TEMPTOT, TEMPOFF


      DATA DISTSHR0/0.0054,0.1005,0.0000,0.9938,0.0000,  &     !  1. Alabama - North
                    0.0054,0.1005,0.0000,0.9938,0.0000,  &     !  2. Alabama - South
                    1.0000,0.0000,0.0000,0.0000,0.0000,  &     !  3. Alaska
                    0.0001,0.0000,0.0000,0.0000,0.0,  &     !  4. Arizona
                    0.0085,0.0017,0.2866,0.0000,0.0,  &     !  5. Arkansas
                    0.9989,1.0000,0.0000,0.0000,0.0,  &     !  6. California
                    0.1036,0.2391,0.1369,0.7870,0.0,  &     !  7. Colorado
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     !  8. Connecticut
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     !  9. Delaware
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 10. Washington DC
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 11. Florida
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 12. Georgia
                    1.0000,0.0000,0.0000,0.0000,0.0,  &     ! 13. Hawaii 
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 14. Idaho
                    0.0000,0.0000,0.0029,0.0000,0.0,  &     ! 15. Illinois
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 16. Indiana
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 17. Iowa
                    0.1018,0.0000,0.0251,0.0000,0.0,  &     ! 18. Kansas
                    0.0005,0.0000,0.0328,0.0000,0.0,  &     ! 19. Kentucky
                    0.3458,0.1268,1.0000,0.0011,0.0,  &     ! 20. Louisiana - North
                    0.3458,0.1268,1.0000,0.0011,0.0,  &     ! 21. Louisiana - South
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 22. Maine
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 23. Maryland
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 24. Massachusetts
                    0.9942,0.0000,0.7467,0.0000,0.0,  &     ! 25. Michigan
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 26. Minnesota
                    0.0026,0.0004,0.0000,0.0016,0.0,  &     ! 27. Mississippi - North
                    0.0026,0.0004,0.0000,0.0016,0.0,  &     ! 28. Mississippi - South
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 29. Missouri
                    0.0862,0.0034,0.0000,0.0071,0.0,  &     ! 30. Montana
                    0.0005,0.0000,0.0001,0.0000,0.0,  &     ! 31. Nebraska
                    0.0001,0.0000,0.0000,0.0000,0.0,  &     ! 32. Nevada
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 33. New Hampshire
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 34. New Jersey
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 35. New Mexico East
                    0.0888,0.0043,0.0000,0.0000,0.0,  &     ! 36. New Mexico West
                    0.0003,0.0000,0.0000,0.0000,0.0,  &     ! 37. New York
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 38. North Carolina
                    0.0071,0.0000,0.0000,0.0000,0.0,  &     ! 39. North Dakota
                    0.0009,1.0000,0.0000,0.0000,0.0,  &     ! 40. Ohio
                    0.8892,0.9983,0.6882,1.0000,0.0,  &     ! 41. Oklahoma
                    0.0006,0.0000,0.0000,0.0000,0.0,  &     ! 42. Oregon
                    0.0034,0.0000,0.2177,1.0000,0.0,  &     ! 43. Pennsylvania
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 44. Rhode Island
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 45. South Carolina
                    0.0000,0.0000,0.0649,0.0000,0.0,  &     ! 46. South Dakota
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 47. Tennessee
                    0.0041,0.0204,0.0000,0.0000,0.250,  &     ! 48. Texas RRC 1
                    0.0781,0.0027,0.0000,0.0000,0.250,  &     ! 49. Texas RRC 2
                    0.1108,0.0002,0.0000,0.0003,0.250,  &     ! 50. Texas RRC 3
                    0.3269,0.2502,0.0000,0.0006,0.250,  &     ! 51. Texas RRC 4
                    0.0330,0.1594,0.0000,0.0026,0.0,  &     ! 52. Texas RRC 5
                    0.0933,0.3395,0.0000,0.0000,0.0,  &     ! 53. Texas RRC 6
                    0.0468,0.0003,0.0000,0.0405,0.0,  &     ! 54. Texas RRC 7B
                    0.1198,0.4985,0.0011,0.0000,0.0,  &     ! 55. Texas RRC 7C
                    0.3468,0.4302,0.2098,0.7382,0.0,  &     ! 56. Texas RRC 8
                    0.0022,0.0000,0.0000,0.2107,0.0,  &     ! 57. Texas RRC 8A
                    0.0523,0.0001,0.7892,0.0000,0.0,  &     ! 58. Texas RRC 9
                    0.4321,0.0709,0.0000,0.0106,0.0,  &     ! 59. Texas RRC 10
                    0.0434,0.1681,0.0000,0.1259,0.0,  &     ! 60. Utah
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 61. Vermont
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 62. Virginia
                    0.0006,0.0000,0.0000,1.0000,0.0,  &     ! 63. Washington
                    0.0002,0.0000,0.0000,0.0000,0.0,  &     ! 64. West Virginia
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 65. Wisconsin
                    0.6707,0.5850,0.7982,0.0800,0.0,  &     ! 66. Wyoming
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 67. North Atlantic State Offshore
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 68. Mid Atlantic State Offshore
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 69. South Atlantic State Offshore
                    0.1000,0.0000,0.0000,0.0000,0.0,  &     ! 70. Alabama State Offshore
                    0.1000,0.0000,0.0000,0.0000,0.0,  &     ! 71. Louisiana State Offshore
                    0.1000,0.0000,0.0000,0.0000,0.0,  &     ! 72. Texas State Offshore
                    0.0500,0.0000,0.0000,0.0000,0.0,  &     ! 73. California State Offshore
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 74. Northern Pacific State Offshore
                    0.0000,0.0000,0.0000,0.0000,0.0,  &     ! 75. Alaska State Offshore
                    1.0000,0.0000,0.0000,0.0000,0.0,  &     ! 76. North Atlantic Federal Offshore
                    1.0000,0.0000,0.0000,0.0000,0.0,  &     ! 77. Mid Atlantic Federal Offshore
                    1.0000,0.0000,0.0000,0.0000,0.0,  &     ! 78. South Atlantic Federal Offshore
                    0.9000,0.0000,0.0000,0.0000,0.0,  &     ! 79. Eastern GOM Federal Offshore
                    0.9000,0.0000,0.0000,0.0000,0.0,  &     ! 80. Central GOM Federal Offshore
                    0.9000,0.0000,0.0000,0.0000,0.0,  &     ! 81. Western GOM Federal Offshore
                    0.9500,0.0000,0.0000,0.0000,0.0,  &     ! 82. California Federal Offshore
                    1.0000,0.0000,0.0000,0.0000,0.0,  &     ! 83. Northern Pacific Federal Offshore
                    1.0000,0.0000,0.0000,0.0000,0.0000/    ! 84. Alaska Federal Offshore

!  BENCHMARK ESPECTED PRODUCTION TO STEO - CURTAIL NATURAL GAS VOLUMES
      if(curitr.eq.1.and.(curiyr >= l48hyr+2.and.curiyr <= l48hyr+steoyrs)) then   ! adjust volumes using the volumes specified in wlbasic
          OGENAGPRD(52,3,curiyr) = OGENAGPRD(52,3,curiyr)+tOGQSHLGAS(1,curiyr-l48hyr)*1000.
          if(OGENAGPRD(52,3,curiyr).lt.0.) then
            OGENAGPRD(52,3,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN TX5'
          endif
          OGENAGPRD(20,3,curiyr) = OGENAGPRD(20,3,curiyr)+tOGQSHLGAS(2,curiyr-l48hyr)*1000.
          if(OGENAGPRD(20,3,curiyr).lt.0.) then
            OGENAGPRD(20,3,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN LAN'
          endif
          OGENAGPRD(5,3,curiyr) = OGENAGPRD(5,3,curiyr)+tOGQSHLGAS(3,curiyr-l48hyr)*1000.
          if(OGENAGPRD(5,3,curiyr).lt.0.) then
            OGENAGPRD(5,3,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN AR'
          endif
          OGENAGPRD(41,3,curiyr) = OGENAGPRD(41,3,curiyr)+tOGQSHLGAS(4,curiyr-l48hyr)*1000.
          if(OGENAGPRD(41,3,curiyr).lt.0.) then
            OGENAGPRD(41,3,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN OK'
          endif
          OGENAGPRD(48,3,curiyr) = OGENAGPRD(48,3,curiyr)+tOGQSHLGAS(5,curiyr-l48hyr)*1000.
          if(OGENAGPRD(48,3,curiyr).lt.0.) then
            OGENAGPRD(48,3,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN TX1'
          endif
          OGENAGPRD(25,3,curiyr) = OGENAGPRD(25,3,curiyr)+tOGQSHLGAS(6,curiyr-l48hyr)*1000.
          if(OGENAGPRD(25,3,curiyr).lt.0.) then
            OGENAGPRD(25,3,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN MI'
          endif
          OGENAGPRD(43,3,curiyr) = OGENAGPRD(43,3,curiyr)+tOGQSHLGAS(7,curiyr-l48hyr)*1000.
          if(OGENAGPRD(43,3,curiyr).lt.0.) then
            OGENAGPRD(43,3,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN PA'
          endif
          OGADGPRD(39,2,curiyr) = OGADGPRD(39,2,curiyr)+tOGQSHLGAS(8,curiyr-l48hyr)*1000.
          if(OGADGPRD(39,2,curiyr).lt.0.) then
            OGADGPRD(39,2,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN ND'
          endif
          OGENAGPRD(40,3,curiyr) = OGENAGPRD(40,3,curiyr)+tOGQSHLGAS(9,curiyr-l48hyr)*1000.
          if(OGENAGPRD(40,3,curiyr).lt.0.) then
            OGENAGPRD(40,3,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN OH'
          endif
          if(curiyr-l48hyr == 2) OGENAGPRD(7,3,curiyr) = OGENAGPRD(7,3,curiyr)+tOGQSHLGAS(15,curiyr-l48hyr)*1000.
          if(curiyr-l48hyr == 3) OGADGPRD(56,2,curiyr) = OGADGPRD(56,2,curiyr)+tOGQSHLGAS(15,curiyr-l48hyr)*1000.
          if(OGENAGPRD(7,3,curiyr).lt.0.) then
            OGENAGPRD(7,3,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN CO'
          endif
          if(OGADGPRD(56,2,curiyr).lt.0.) then
            OGADGPRD(56,2,curiyr) = 0.
            write(bugout,*) 'WARNING - CURTAILED PRODUCTION IS MORE THAN AVAILABLE PRODUCTION IN TX8'
          endif
          OGENAGPRD(52,gastypes,curiyr) = sum(OGENAGPRD(52,1:gastypes-1,curiyr))
          OGENAGPRD(20,gastypes,curiyr) = sum(OGENAGPRD(20,1:gastypes-1,curiyr))
          OGENAGPRD(5,gastypes,curiyr) = sum(OGENAGPRD(5,1:gastypes-1,curiyr))
          OGENAGPRD(41,gastypes,curiyr) = sum(OGENAGPRD(41,1:gastypes-1,curiyr))
          OGENAGPRD(48,gastypes,curiyr) = sum(OGENAGPRD(48,1:gastypes-1,curiyr))
          OGENAGPRD(25,gastypes,curiyr) = sum(OGENAGPRD(25,1:gastypes-1,curiyr))
          OGENAGPRD(43,gastypes,curiyr) = sum(OGENAGPRD(43,1:gastypes-1,curiyr))
          OGADGPRD(39,oiltypes,curiyr) = sum(OGADGPRD(39,1:oiltypes-1,curiyr))
          OGENAGPRD(40,gastypes,curiyr) = sum(OGENAGPRD(40,1:gastypes-1,curiyr))
          OGENAGPRD(7,gastypes,curiyr) = sum(OGENAGPRD(7,1:gastypes-1,curiyr))
          OGADGPRD(56,oiltypes,curiyr) = sum(OGADGPRD(56,1:oiltypes-1,curiyr))
      endif
 
!  COMPUTE EXPECTED PRODUCTION
      EPRDL48 = 0.
      EPRDOFF = 0.
      EPRDADOFF = 0.
      DO DIST=1,OGDIST
        DO K=1,GASTYPES-1
          IF (DISTMAP(DIST,1) > 0 .AND. DISTMAP(DIST,1) <= L48RGN) THEN
            EPRDL48(DISTMAP(DIST,1),K) = EPRDL48(DISTMAP(DIST,1),K) + OGENAGPRD(DIST,K,CURIYR)
          ENDIF
        ENDDO
        IF (DISTMAP(DIST,1) > L48RGN .AND. DISTMAP(DIST,1) <= L48RGN+3) THEN
          EPRDOFF(DISTMAP(DIST,1)-L48RGN) = EPRDOFF(DISTMAP(DIST,1)-L48RGN) + sum(OGENAGPRD(DIST,1:gastypes-1,CURIYR))
          EPRDADOFF(DISTMAP(DIST,1)-L48RGN) = EPRDADOFF(DISTMAP(DIST,1)-L48RGN) + sum(OGADGPRD(DIST,1:oiltypes-1,CURIYR))
        ENDIF
      ENDDO

!  COMPUTE PORTION AT THE DISTRICT REGION LEVEL FROM NGMM
      DO DIST=1,OGDIST
        IR = DISTMAP(DIST,1)
        IF (IR > 0 .AND. IR <= L48RGN) THEN
          DO K=1,gastypes-1
            if (OGENAGPRD(DIST,gastypes,CURIYR).GT.0.) &
              OGRNAGPRD(DIST,K,CURIYR) = OGRNAGPRD(DIST,GASTYPES,CURIYR) * OGENAGPRD(DIST,K,CURIYR)/SUM(OGENAGPRD(DIST,1:4,CURIYR))
          ENDDO
        ENDIF
      ENDDO

!  IF FUEL TYPE = 5,6,7 (tight gas, shale gas, CBM)
      OGPRDUGR(:,:,CURIYR) = 0.
      DO DIST=1,OGDIST
        IR = DISTMAP(DIST,1)
        IF (IR > 0 .AND. IR <= L48RGN) THEN
          DO K=5,L48FUEL
            OGPRDUGR(IR,K-4,CURIYR) = OGPRDUGR(IR,K-4,CURIYR) + OGRNAGPRD(DIST,K-3,CURIYR)
          ENDDO
        ENDIF
      ENDDO

!  BENCHMARK NGPL PRODUCTION TO STEO
         if (curiyr.ge.l48hyr+2.and.curiyr.le.l48hyr+steoyrs) then
           if (ogSTEO.eq.0.and.curitr.eq.1) then
             if(sum(NGPLPRD(1:66,curiyr)).gt.0.) sNGPLPRD(curiyr-l48hyr) = ((sum(OGNGPLPRD(1:66,curiyr))-OGNGPLPRD(3,curiyr))-(sum(NGPLPRD(1:66,curiyr))-NGPLPRD(3,curiyr)))
             if(sum(NGPLPRDET(1:66,curiyr)).gt.0.) sNGPLET(curiyr-l48hyr) = ((sum(OGNGPLET(1:66,curiyr))-OGNGPLET(3,curiyr))-(sum(NGPLPRDET(1:66,curiyr))-NGPLPRDET(3,curiyr)))
             if(sum(NGPLPRDPR(1:66,curiyr)).gt.0.) sNGPLPR(curiyr-l48hyr) = ((sum(OGNGPLPR(1:66,curiyr))-OGNGPLPR(3,curiyr))-(sum(NGPLPRDPR(1:66,curiyr))-NGPLPRDPR(3,curiyr)))
             if(sum(NGPLPRDIS(1:66,curiyr)).gt.0.) sNGPLIS(curiyr-l48hyr) = ((sum(OGNGPLIS(1:66,curiyr))-OGNGPLIS(3,curiyr))-(sum(NGPLPRDIS(1:66,curiyr))-NGPLPRDIS(3,curiyr)))
             if(sum(NGPLPRDBU(1:66,curiyr)).gt.0.) sNGPLBU(curiyr-l48hyr) = ((sum(OGNGPLBU(1:66,curiyr))-OGNGPLBU(3,curiyr))-(sum(NGPLPRDBU(1:66,curiyr))-NGPLPRDBU(3,curiyr)))
             if(sum(NGPLPRDPP(1:66,curiyr)).gt.0.) sNGPLPP(curiyr-l48hyr) = ((sum(OGNGPLPP(1:66,curiyr))-OGNGPLPP(3,curiyr))-(sum(NGPLPRDPP(1:66,curiyr))-NGPLPRDPP(3,curiyr)))
           endif
         endif

         DO R = 1,66
           IF (DISTMAP(R,1).NE.0.and.curiyr.gt.l48hyr) THEN
             OGDNGPRD(R,1,curiyr) = OGRNAGPRD(R,1,curiyr) + OGADGPRD(R,1,curiyr) + OGADGPRD(R,3,curiyr) + OGADGPRD(R,4,curiyr)
             OGDNGPRD(R,2,curiyr) = OGRNAGPRD(R,2,curiyr)                           ! TIGHT GAS
             OGDNGPRD(R,3,curiyr) = OGRNAGPRD(R,3,curiyr) + OGADGPRD(R,2,curiyr)    ! SHALE
             OGDNGPRD(R,4,curiyr) = OGRNAGPRD(R,4,curiyr)                           ! CBM
             if (curiyr.ge.l48hyr+2.and.curiyr.le.l48hyr+steoyrs) then
!              write(6,*) 'dh5ngpl',r,OGNGPLPRD(R,curiyr),NGPLPRD(R,curiyr),OGNGPLPRD(R,curiyr)-NGPLPRD(R,curiyr)
!              write(6,*) 'dh5eth',r,OGNGPLET(R,curiyr),NGPLPRDET(R,curiyr),OGNGPLET(R,curiyr)-NGPLPRDET(R,curiyr)
!              write(6,*) 'dh5pro',r,OGNGPLPR(R,curiyr),NGPLPRDPR(R,curiyr),OGNGPLPR(R,curiyr)-NGPLPRDPR(R,curiyr)
!              write(6,*) 'dh5isb',r,OGNGPLIS(R,curiyr),NGPLPRDIS(R,curiyr),OGNGPLIS(R,curiyr)-NGPLPRDIS(R,curiyr)
!              write(6,*) 'dh5but',r,OGNGPLBU(R,curiyr),NGPLPRDBU(R,curiyr),OGNGPLBU(R,curiyr)-NGPLPRDBU(R,curiyr)
!              write(6,*) 'dh5pen',r,OGNGPLPP(R,curiyr),NGPLPRDPP(R,curiyr),OGNGPLPP(R,curiyr)-NGPLPRDPP(R,curiyr)
               OGNGPLPRD(R,curiyr) = NGPLPRD(R,curiyr)+sNGPLPRD(curiyr-l48hyr)*NGPLPRD(R,curiyr)/sum(NGPLPRD(1:66,curiyr))
               OGNGPLET(R,curiyr) = NGPLPRDET(R,curiyr)+sNGPLET(curiyr-l48hyr)*NGPLPRDET(R,curiyr)/sum(NGPLPRDET(1:66,curiyr))
               OGNGPLPR(R,curiyr) = NGPLPRDPR(R,curiyr)+sNGPLPR(curiyr-l48hyr)*NGPLPRDPR(R,curiyr)/sum(NGPLPRDPR(1:66,curiyr))
               OGNGPLIS(R,curiyr) = NGPLPRDIS(R,curiyr)+sNGPLIS(curiyr-l48hyr)*NGPLPRDIS(R,curiyr)/sum(NGPLPRDIS(1:66,curiyr))
               OGNGPLBU(R,curiyr) = NGPLPRDBU(R,curiyr)+sNGPLBU(curiyr-l48hyr)*NGPLPRDBU(R,curiyr)/sum(NGPLPRDBU(1:66,curiyr))
               OGNGPLPP(R,curiyr) = NGPLPRDPP(R,curiyr)+sNGPLPP(curiyr-l48hyr)*NGPLPRDPP(R,curiyr)/sum(NGPLPRDPP(1:66,curiyr))
             ENDIF
             if (curiyr.gt.l48hyr+steoyrs) then
               OGNGPLPRD(R,curiyr) = NGPLPRD(R,curiyr)
               OGNGPLET(R,curiyr) = NGPLPRDET(R,curiyr)
               OGNGPLPR(R,curiyr) = NGPLPRDPR(R,curiyr)
               OGNGPLIS(R,curiyr) = NGPLPRDIS(R,curiyr)
               OGNGPLBU(R,curiyr) = NGPLPRDBU(R,curiyr)
               OGNGPLPP(R,curiyr) = NGPLPRDPP(R,curiyr)
             ENDIF
           ENDIF
         ENDDO

! Alaska NGPL

         OGDNGPRD(3,1,curiyr) = OGPRNGAK(3,CURIYR) + OGPRNGAK(2,CURIYR)
         OGDNGPRD(75,1,curiyr) = OGPRNGAK(1,CURIYR) * 0.96
         OGDNGPRD(84,1,curiyr) = OGPRNGAK(1,CURIYR) * 0.04
         if (curiyr.gt.l48hyr+steoyrs) then
           OGNGPLPRD(3,curiyr) = ogdngprd(3,1,curiyr)*0.0/365/1000. + ognglak(curiyr)/1000.
           OGNGPLET(3,curiyr) = 0.0000*OGNGPLPRD(3,curiyr)
           OGNGPLPR(3,curiyr) = 0.1967*OGNGPLPRD(3,curiyr)
           OGNGPLBU(3,curiyr) = 0.2739*OGNGPLPRD(3,curiyr)
           OGNGPLIS(3,curiyr) = 0.0703*OGNGPLPRD(3,curiyr)
           OGNGPLPP(3,curiyr) = 0.4591*OGNGPLPRD(3,curiyr)
         endif


! Offshore NGPL

         DO DIST = 67, 84
           IF (DISTMAP(DIST,1) > L48RGN .AND. DISTMAP(DIST,1) <= L48RGN+3) THEN
             R = DISTMAP(DIST,1) - L48RGN
               OGDNGPRD(DIST,1,curiyr) = sum(OGRNAGPRD(DIST,1:gastypes-1,curiyr))+sum(OGADGPRD(DIST,1:oiltypes-1,curiyr))
               OGDNGPRD(DIST,2,curiyr) = 0.
               OGDNGPRD(DIST,3,curiyr) = 0.
               OGDNGPRD(DIST,4,curiyr) = 0.
           ENDIF
         ENDDO

         if (curiyr.gt.l48hyr+steoyrs) then
         tempoff = 0.
         DO R = 67,84
           if(r.eq.75.or.r.eq.84) CYCLE
           tempoff = tempoff + ogdngprd(r,1,curiyr)
           OGNGPLPRD(r,curiyr) = ogdngprd(r,1,curiyr)*20.0/365/1000.
           OGNGPLET(r,curiyr) = 0.0000*OGNGPLPRD(r,curiyr)
           OGNGPLPR(r,curiyr) = 0.1967*OGNGPLPRD(r,curiyr)
           OGNGPLBU(r,curiyr) = 0.0739*OGNGPLPRD(r,curiyr)
           OGNGPLIS(r,curiyr) = 0.2703*OGNGPLPRD(r,curiyr)
           OGNGPLPP(r,curiyr) = 0.4591*OGNGPLPRD(r,curiyr)
           if (r.eq.80) then
             OGNGPLPRD(r,curiyr) = (ogdngprd(r,1,curiyr)*ogngprdgom(1,curiyr)/(ogngprdgom(1,curiyr)+ogngprdgom(2,curiyr))*60.  &
                + ogdngprd(r,1,curiyr)*ogngprdgom(2,curiyr)/(ogngprdgom(1,curiyr)+ogngprdgom(2,curiyr))*100.)/365./1000.
             OGNGPLET(r,curiyr) = 0.48*OGNGPLPRD(r,curiyr)
             OGNGPLPR(r,curiyr) = 0.27*OGNGPLPRD(r,curiyr)
             OGNGPLBU(r,curiyr) = 0.02*OGNGPLPRD(r,curiyr)
             OGNGPLIS(r,curiyr) = 0.16*OGNGPLPRD(r,curiyr)
             OGNGPLPP(r,curiyr) = 0.06*OGNGPLPRD(r,curiyr)
!            OGNGPLBU(r,curiyr) = OGNGPLBU(r,curiyr) - 0.055
!            OGNGPLET(r,curiyr) = OGNGPLET(r,curiyr) + 0.030
!            OGNGPLPR(r,curiyr) = OGNGPLPR(r,curiyr) + 0.025
           endif
         ENDDO
         endif

         temptot = 0.
         tempsum = 0.
         DO R = 1,OGDIST
         DO K = 1,4
           if(distmap(r,1) > 0 .and. distmap(r,1) <= L48rgn) tempsum(distmap(r,1),K) = tempsum(distmap(r,1),K) + ogdngprd(r,K,curiyr)
           temptot = temptot + ogdngprd(r,K,curiyr)
         ENDDO
         ENDDO

         DO R = 1,L48RGN
           WRITE(BUGOUT,*) 'PMMDNG',curiyr+1989,r,(prdtmp(r,i),i=1,4)
           WRITE(BUGOUT,*) 'PMMDNG',curiyr+1989,r,(tempsum(r,i),i=1,4)
         ENDDO
         WRITE(BUGOUT,*) 'PMMDNG',curiyr+1989,temptot, tempoff

         IF (curiyr.ge.l48hyr) then
           DO R = 1,OGDIST
             WRITE(BUGOUT,*) 'ngplprd',curiyr+1989,r,sum(ogdngprd(r,1:4,curiyr)),OGNGPLPRD(r,curiyr)*1000., &
                            OGNGPLPRD(r,curiyr)*365.

           ENDDO
           WRITE(BUGOUT,*) 'ngpltot',curiyr+1989,sum(OGNGPLPRD(1:OGDIST,curiyr))*1000., &
                            sum(OGNGPLPRD(1:OGDIST,curiyr))*365.
         ENDIF
         IF (curiyr+1989.eq.2018) then
           DO R = 1,OGDIST
             WRITE(BUGOUT,*) 'ngpltest',r,OGNGPLPRD(r,curiyr-2)*365.,OGNGPLPRD(r,curiyr-1)*365.,OGNGPLPRD(r,curiyr)*365.
           ENDDO
         ENDIF

      RETURN
      END
!===================================================================
      SUBROUTINE OGSM_NEXTDATA2(UNITNUM)

      IMPLICIT NONE

      INTEGER*4 UNITNUM,I
      CHARACTER*2 CH
        CHARACTER*1 DUM

! READ UNTIL @ IN 2nd COLUMN

      CH = 'AA'
      DO WHILE (CH .NE. ' @')
        READ(UNITNUM,10) CH
      ENDDO

  10  FORMAT(A2)

      RETURN
      END
!********************************************************************

      SUBROUTINE OGCOMP_AD
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! ogsm lower 48 onshore variables
      include'ogsmoff'      ! ogsm lower 48 offshore variables
      include'ogsmout'      ! ogsm global output variables
      include'pmmout'       ! lfmm global output variables

      REAL OGPRODGOM        ! TOTAL OFFSHORE GOM PRODUCTION
      REAL CUMPROD
      REAL X       ! TEMP HOLDING VARIABLES
      REAL OILPR(L48RGN,MNUMYR)     ! OIL PR
      REAL ADGASPR(L48RGN,MNUMYR)   ! AD GAS PR
      REAL rho,b4              ! estimated parameters
      REAL rho2,b1 ! estimated parameters
      REAL BENCHAD(L48RGN+OFFNEMSRGN)  ! AD PROD BENCH FACTORS

      DATA rho/0.387437/
      DATA b4/4.92347/
!
! parameters for Kevin reest using oil prod

      DATA rho2/0.509492/, b1/0.589867/

      IF (CURIYR.LE.L48HYR) THEN
        DO R=1,L48RGN+OFFNEMSRGN
          OGPRDAD(R,CURIYR) = HISTADPRD(R,CURIYR)
        ENDDO
        DO R = 1,L48RGN
          IF (RESBOYL48(R,1).GT.0.) OILPR(R,CURIYR) = RFQTDCRD(R,CURIYR)*365/(RESBOYL48(R,1)+RESBOYL48(R,2))
          IF (CURIYR.GT.1) THEN
            IF (EOYADRES(R,CURIYR-1).GT.0.) ADGASPR(R,CURIYR) = OGPRDAD(R,CURIYR)/EOYADRES(R,CURIYR-1)
          ENDIF
        ENDDO

      ELSE

! ONSHORE

!    Tony's estimations (June 2007)
         DO R = 1,L48RGN
           IF (EOYADRES(R,CURIYR-1).GT.0.) THEN
             IF (CURIYR.GT.1) ADGASPR(R,CURIYR) = HISTADPRD(R,CURIYR)/EOYADRES(R,CURIYR-1)
!          ELSE
!            HISTADPRD(R,CURIYR) = 0.
           ENDIF
           OGPRDAD(R,CURIYR) = HISTADPRD(R,CURIYR)
         ENDDO
        WRITE(BUGOUT,101) CURIYR+1989,(OGPRDAD(R,CURIYR),R=1,L48RGN),(eoyadres(R,curiyr-1),R=1,L48RGN),(RFQDCRD(R,CURIYR),R=1,L48RGN)
 101      FORMAT('adgtest',I5,<L48RGN>F8.1,'  |',<L48RGN>F8.1,'  |',<L48RGN>F8.3)


      ENDIF
      IF (CURIYR.GT.1.AND.FCRL.EQ.1) WRITE(BUGOUT,100) CURIYR+1989, &
          (OGPRDAD(R,CURIYR),R=1,L48RGN),(ADGASPR(R,CURIYR),R=1,L48RGN),(EOYADRES(R,CURIYR-1),R=1,L48RGN)
 100  FORMAT('adprd',I5,<L48RGN>F8.1,'  |',<L48RGN>F8.4,'  |',<L48RGN>F8.1)

!  OFFSHORE CA, GOM (& ATL=0)
        call calc_ags(curiyr)

      ogprdadof(1,curiyr) = ogprdad(l48rgn+1,curiyr)
      ogprdadof(2,curiyr) = ogprdad(l48rgn+2,curiyr)
      ogprdadof(3,curiyr) = ogprdad(l48rgn+3,curiyr)

! write ad gas production to WLDEBUG
      if (prtdbgl.eq.1.and.curiyr.eq.lastyr.and.fcrl.eq.1) then
        write(sysout,*) 'Onshore & Offshore AD gas production by OGSM regions'
        do r=1,l48rgn+offnemsrgn
        do m=1,LASTYR
          write(sysout,*) m+1989,r,ogprdad(r,m)
        enddo
        enddo
      endif

      return
      end

!==========================> HSM Code Start <==========================
SUBROUTINE hsm_restart_output
IMPLICIT NONE

include'parametr'
include'ncntrl'

integer*4 FRTYPE
integer*4 FSOURC
integer*4 FUNITI
integer*4 FUNITO
character*100 FNAMEI
character*100 FNAMEO
integer*4 FRETCD
integer*4 FUNFMT
character*60 putFileName
logical once  /.true./
logical new_file

integer file_mgr
external file_mgr

FRTYPE = 1   ! write out variables listed in output request file
FSOURC = 1   ! obtain list of variables from file
FNAMEI = ' ' ! blank when the file is opened outside of filer
FNAMEO = ' ' ! blank when the file is opened outside of filer
FUNFMT = 7   ! text data transfer via AIMMS composite tables

if (once) then
    call unitunopened(100,999,FUNITI)
    OPEN(unit=FUNITI, file='hsmputvars.txt', action='write', position='append')
    ! ogsm inputs
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=CONVFACT,VAR=CFLGQ'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=EMISSION,VAR=EMETAX'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=EMISSION,VAR=EXTRARISK'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=INTOUT,VAR=BRENT_PRICE'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=INTOUT,VAR=IT_WOP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=INTOUT,VAR=REPORT'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=INTOUT,VAR=START_PRICE'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=INTOUT,VAR=WTI_PRICE'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=LFMMOUT,VAR=RFCRUDEWHP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=MACOUT,VAR=MC_JPGDP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=MACOUT,VAR=MC_RMCORPBAA'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=MACOUT,VAR=MC_RMTCM10Y'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=MPBLK,VAR=PELIN'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=MXPBLK,VAR=XOGWPRNG'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=CURCALYR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=CURITR'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=CURIYR' ! shouldn't be needed for hsm
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=FCRL'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=FIRSYR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=I4SCNT'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=IJUMPCALYR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=IJUMPYR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=LASTYR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=NCRL'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=PRTDBGL'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NCNTRL,VAR=WWOP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NGTDMREP,VAR=NGEXPVOL'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NGTDMREP,VAR=OGHHPRNG'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NGTDMREP,VAR=OGPRCNG'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NGTDMREP,VAR=OGPRDNG'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NGTDMREP,VAR=OGPRSUP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=NGTDMREP,VAR=OGWPRNG'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCNPPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PLF'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2QEM'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2QLF'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGRNAGPRD' ! Shared with NGMM
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=CNRNAGPRD' ! Shared with NGMM
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=QBLK,VAR=QNGAS'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=TCS45Q,VAR=CCS_EOR_45Q'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=TCS45Q,VAR=I_45Q_DURATION'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=TCS45Q,VAR=I_45Q_LYR_RET'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=TCS45Q,VAR=I_45Q_SYR'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=UEFDOUT,VAR=CO2_CCS' ! Huge table
    ! ogsm outputs
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=COGEN,VAR=CGOGSCAP'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=COGEN,VAR=CGOGSGEN'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=COGEN,VAR=CGOGSQ'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=CNADGPRD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=CNENAGPRD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=DVDRYFT'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=DVFTAGE'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=DVGASFT'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=DVOILFT'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=DVSPEND'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=EXDRYFT'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=EXFTAGE'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=EXGASFT'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=EXOILFT'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=EXSPEND'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=NS_START'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGADGPRD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO245Q'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2AVL'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2AVLs'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2INJ'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PEM'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PRC'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PRCs'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PUR'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PUR2'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2REC'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2TAR'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCOPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCOPRDGOM'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCORSV'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCOWHP'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCRDHEAT'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCRDPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCRUDEREF'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGDNGPRD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGELSHALE'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGENAGPRD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEORPRD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYAD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYINF'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYRSV'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYRSVON'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYUGR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYURR'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGGROWFAC'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGJOBS'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGLAK'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLBU'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLET'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLIS'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLPP'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLPR'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLPRD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPRDGOM'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGRSV'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGWHP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNOWELL'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGOGWELLS'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGOILPRD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPCRWHP'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPNGWHP'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRCEXP'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRCOAK'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRDAD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRDADOF'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRDOFF'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRDUGR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRRNGOF'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQCRREP'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQCRRSV'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQNGREP'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQNGRSV'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQSHLGAS'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQSHLOIL'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGREGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGRESNGOF'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGSHALENG'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGSRL48'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGTAXPREM'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGTECHON'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGWELLSL48'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=PMMOUT,VAR=DCRDWHP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=PMMOUT,VAR=RFQDCRD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=PMMOUT,VAR=RFQTDCRD'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=PMMOUT,VAR=XDCRDWHP'
    ! write(FUNITI, '(*(G0.16,:,","))') ' LABEL=PMMOUT,VAR=XRFQDCRD'
    ! added to HSM for cost equation development
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=MACOUT,VAR=MC_WPI10'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=MACOUT,VAR=MC_WPI11'
    
    Close(unit=FUNITI, status='keep')
    once = .false.
endif

new_file=.false.
call unitunopened(100,999,FUNITI)
OPEN(unit=FUNITI, file='hsmputvars.txt', action='READ', position='append')
rewind funiti    ! in case it is already open, position at beginning

write(putFileName,'(a,i4,a,i2.2,a)') 'GlobalDataToPython_',curcalyr,'_',curitr,'.txt'
call unitunopened(100,999,FUNITO)

! FNAMEO = putFileName
open(funito, file=putFileName, status='unknown', buffered='YES')
rewind funito

CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
close(funiti)
close(funito)
end subroutine

SUBROUTINE hsm_restart_results
IMPLICIT NONE

include'parametr'
include'ncntrl'

integer*4 FRTYPE
integer*4 FSOURC
integer*4 FUNITI
integer*4 FUNITO
character*100 FNAMEI
character*100 FNAMEO
integer*4 FRETCD
integer*4 FUNFMT
character*60 putFileName
logical once  /.true./
logical new_file

integer file_mgr
external file_mgr

FRTYPE = 1   ! write out variables listed in output request file
FSOURC = 1   ! obtain list of variables from file
FNAMEI = ' ' ! blank when the file is opened outside of filer
FNAMEO = ' ' ! blank when the file is opened outside of filer
FUNFMT = 7   ! text data transfer via AIMMS composite tables

if (once) then
    call unitunopened(100,999,FUNITI)
    OPEN(unit=FUNITI, file='hsmgetvars.txt', action='write', position='append')
    ! ogsm outputs
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=CONVFACT,VAR=CFLGQ'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=COGEN,VAR=CGOGSCAP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=COGEN,VAR=CGOGSGEN'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=COGEN,VAR=CGOGSQ'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=CNADGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=CNENAGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=CNRNAGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=DVDRYFT'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=DVFTAGE'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=DVGASFT'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=DVOILFT'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=DVSPEND'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=EXDRYFT'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=EXFTAGE'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=EXGASFT'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=EXOILFT'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=EXSPEND'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=NS_START'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGADGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO245Q'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2AVL'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2AVLs'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2INJ'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PEM'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PRC'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PRCs'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PUR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2PUR2'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2REC'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCO2TAR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCOPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCOPRDGOM'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCORSV'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCOWHP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCRDHEAT'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCRDPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGCRUDEREF'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGDNGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGELSHALE'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGENAGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEORPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYAD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYINF'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYRSV'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYRSVON'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYUGR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGEOYURR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGGROWFAC'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGJOBS'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGLAK'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLBU'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLET'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLIS'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLPP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLPR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPLPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGPRDGOM'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGRSV'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNGWHP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGNOWELL'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGOGWELLS'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGOILPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPCRWHP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPNGWHP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRCEXP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRCOAK'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRDAD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRDADOF'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRDOFF'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRDUGR'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGPRRNGOF'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQCRREP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQCRRSV'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQNGREP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQNGRSV'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQSHLGAS'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGQSHLOIL'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGREGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGRESNGOF'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGRNAGPRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGSHALENG'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGSRL48'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGTAXPREM'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGTECHON'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=OGSMOUT,VAR=OGWELLSL48'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=PMMOUT,VAR=DCRDWHP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=PMMOUT,VAR=RFQDCRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=PMMOUT,VAR=RFQTDCRD'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=PMMOUT,VAR=XDCRDWHP'
    write(FUNITI, '(*(G0.16,:,","))') ' LABEL=PMMOUT,VAR=XRFQDCRD'

    Close(unit=FUNITI, status='keep')
    once = .false.
endif

new_file=.false.
call unitunopened(100,999,FUNITI)
OPEN(unit=FUNITI, file='hsmgetvars.txt', action='READ', position='append')
rewind funiti    ! in case it is already open, position at beginning

write(putFileName,'(a,i4,a,i2.2,a)') 'GlobalDataToNEMS_',curcalyr,'_',curitr,'.txt'
call unitunopened(100,999,FUNITO)

! FNAMEO = putFileName
open(funito, file=putFileName, status='unknown', buffered='YES')
rewind funito

CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
close(funiti)
close(funito)
end subroutine
!===========================> HSM Code End <===========================
