! $Header: m:/default/source/RCS/welldcf.f,v 1.163 2020/11/10 21:09:20 DH5 Exp $
!********************************************************************
!
!
!               O G S M     S Y S T E M     R O U T I N E S
!
!
!********************************************************************
!********************************************************************

!********************************************************************
      REAL FUNCTION OG_DCF(OGSMMOD,CLASS,REGION,FUEL,PRJ,RECPRD1, &
         RECPRD2,PRICE1,PRICE2,PRODPRIM,PRODCOP,DECLRT,DISCRT,INFLRT, &
         ROYALRT,EXPSCH,DEVSCH,OTHRSCH,TRANPRIM,TRANCOP,EXPSR,DEVSR, &
         KAP,EXPDRILL,DEVDRILL,OTHRCOST,EXDRYCST,DVDRYCST,LEASE, &
         OPERATE,STTAXRT,FEDTAXRT,PRIMSEV,COPSEV,ADVRT,KAPRT, &
         XDCKAPAKRT,taxcred,itcrt,wptrt)
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables

!  SUBROUTINE PARAMETER DECLARATIONS
      INTEGER OGSMMOD       ! MODULE FLAG (1-L48ON,2-AK,3-OFF)
      INTEGER CLASS         ! WELL CLASS INDEX
      INTEGER REGION        ! SPECIFIC MODULE REGION INDEX
      INTEGER FUEL          ! FUEL TYPE INDEX
      INTEGER PRJ           ! PROJECT LIFE
      INTEGER RECPRD1       ! RECOVERY PERIOD FOR IDC
      INTEGER RECPRD2       ! RECOVERY PERIOD FOR TDC
      INTEGER YEARVAL       ! YEAR VALUE FOR DCF REPORT
      REAL PRICE1(30)       ! DEFAULT PRICE OF PRIMARY FUEL
      REAL PRICE2(30)       ! DEFAULT PRICE OF COPRODUCT
      REAL PRODPRIM(30)     ! PRODUCTION OF PRIMARY FUEL
      REAL PRODCOP(30)      ! PRODUCTION OF COPRODUCT FUEL
      REAL DECLRT           ! DECLINE RATE
      REAL DISCRT           ! DISCOUNT RATE
      REAL INFLRT           ! INFLATION RATE
      REAL ROYALRT          ! ROYALTY RATE
      REAL EXPSCH(2,30)  ! EXPLORATORY DRILLING SCHEDULE (TOT WLS)
      REAL DEVSCH(2,30)  ! DEVELOPMENTAL DRILLING SCHED (TOT WLS)
      REAL OTHRSCH(2,30) ! SCHEDULE FOR NFW
      REAL OTHRCOST         ! COST DRILLING THE NFW
      REAL TRANPRIM         ! TRANSPORTATION COST PRIMARY FUEL
      REAL TRANCOP          ! TRANSPORTATION COST OF COPRODUCT
      REAL EXPSR            ! SUCCESS RATES
      REAL DEVSR            ! SUCCESS RATES
      REAL KAP(30)          ! OTHER EXPECTED CAP. COSTS
      REAL EXPDRILL         ! EXPLORATORY DRILL COST
      REAL DEVDRILL         ! DEVELOPMENTAL DRILL COST
      REAL EXDRYCST         ! DRY HOLE COST
      REAL DVDRYCST         ! DRY HOLE COST
      REAL LEASE            ! LEASE EQUIPMENT COST
      REAL OPERATE          ! OPERATING COST
      REAL STTAXRT          ! STATE TAX RATE
      REAL FEDTAXRT         ! FEDERAL TAX RATE
      REAL PRIMSEV          ! SEVERANCE TAX RATE PRIMARY FUEL
      REAL COPSEV           ! SEVERANCE TAX RATE COPRODUCT
      REAL ADVRT            ! AD VALOREM TAX RATE
      REAL KAPRT(2)         ! FRACT DRILL COST TANGIBLE & DEPRECIATED
      REAL XDCKAPAKRT       ! FRACT INTANG DRILL COST DEPRECIATED
      REAL taxcred(3)       ! unconventional fuel tax credit
      REAL itcrt            ! investment tax credit rate
      REAL wptrt            ! windfall profit tax rate
      REAL wptbase          ! windfall profit tax base

!  MISCELANIOUS LOCAL VARIABLES
      LOGICAL BETAFLG        ! SIGNALS FIRST 13 YEARS OF A PROJECT
      LOGICAL CONTINUE       ! SIGNALS END OF PROJECTS PROFITABILITY
      INTEGER BETA           ! NUMBER OF YEARS FOR CAPITAL RECOVERY
      REAL TOTOTHR           ! TOTAL OTHER CAPITAL EXPENDITURES
      REAL CUMSUC            ! TOTAL CUMULATIVE SUCCESSFUL WELLS
      REAL SUCEXP            ! NUMBER OF SUCCESSFUL EXPLOR WELLS
      REAL SUCNFW            ! NUMBER OF SUCCESSFUL NEW FLD WILDCATS
      REAL SUCDEV            ! NUMBER OF SUCCESSFUL DEVELOP WELLS
      REAL DRYEXP            ! NUMBER OF DRY EXPLOR WELLS
      REAL DRYNFW            ! NUMBER OF DRY NEW FLD WILDCATS
      REAL DRYDEV            ! NUMBER OF DRY DEVELOP WELLS
      REAL FRACREC           ! FRACTION OF EXPECTED RECOVERY
      REAL PV                ! PRESENT VALUE FACTOR
      REAL PVL               ! PV FACTOR FOR DEPRECIATION YEAR L
      REAL DEP               ! DEPRECIATION VALUE FORM MACRS OR SLM
      REAL PVINFL            ! PV OF INFLATION IN YEAR L
      REAL OG_TAX_CALC_IDC   ! INTANG COST TAX FUNCTION DECLARATION
      REAL OG_TAX_CALC_TDC   ! TANG COST TAX FUNCTION DECLARATION
      REAL OGSEVR_AK         ! ALASKA SEV TAX FUNCTION DECLARATION
      REAL OGOPER_AK         ! ALASKA OPER COST FUNCTION DECLARATION
      REAL DELTADCF          ! PERCENT DIFF IN CURRENT VS LAG YEAR DCF
      REAL DELTASR(2)         ! PERCENT DIFF IN CURRENT YEAR VS BASE YR SUCCESS RATE
      REAL BASERATE

!  COMPUTED VARIABLES
      REAL*8 REV(30)
      REAL*8 ROY(30)
      REAL*8 PRODTAX(30)
      REAL*8 DRILL(30)
      REAL*8 EQUIP(30)
      REAL*8 OPER(30)
      REAL*8 ABANDON(30)
      REAL*8 OTHERCAP(30)
      REAL*8 XIDC_S(30)
      REAL*8 XIDC_F(30)
      REAL*8 AIDC_S(30)
      REAL*8 AIDC_F(30)
      REAL*8 DEPREC_S(30)
      REAL*8 DEPREC_F(30)
      REAL*8 DHC(30)
      REAL*8 TAXBASE_S(30)
      REAL*8 TAXBASE_F(30)
      REAL*8 FCREDIT(30)
      REAL*8 ICREDIT(30)
      REAL*8 WINDFALL(30)
      REAL*8 STTAX(30)
      REAL*8 FEDTAX(30)
      REAL*8 CF(30)
      REAL*8 DCFTEMP(30)
      REAL*8 DCFTOT
      REAL*8 PVSUM(18)
!  LEASE EQUIPMENT COST
      REAL LEASOFFPRJ(2)     ! TEMP DECLARATION MOVE TO OGSMOFF
      LOGICAL ONCE
!  DATA STATEMENTS
      DATA LEASOFFPRJ/9300266,5225864/
      DATA ONCE/.FALSE./
      
!==========================> HSM Code Start <==========================
      real*8 hsm_oper(30)
      real*8 hsm_exp_drill(30)
      real*8 hsm_dev_drill(30)
      real*8 hsm_exp_dry_drill(30)
      real*8 hsm_dev_dry_drill(30)
      hsm_oper = 0.0
      hsm_exp_drill = 0.0
      hsm_dev_drill = 0.0
      hsm_exp_dry_drill = 0.0
      hsm_dev_dry_drill = 0.0
!===========================> HSM Code End <===========================

!  INITIALIZE FOR CURRENT INVOCATION
      DCFTOT = 0
      CUMSUC = 0
      DO M=1,30
         AIDC_S(M) = 0
         AIDC_F(M) = 0
         DEPREC_S(M) = 0
         DEPREC_F(M) = 0
         REV(M) = 0
         ROY(M) = 0
         PRODTAX(M) = 0
         DRILL(M) = 0
         EQUIP(M) = 0
         OPER(M) = 0
         ABANDON(M) = 0
         OTHERCAP(M) = 0
         XIDC_S(M) = 0
         XIDC_F(M) = 0
         DHC(M) = 0
         TAXBASE_S(M) = 0
         TAXBASE_F(M) = 0
         FCREDIT(M) = 0
         ICREDIT(M) = 0
         STTAX(M) = 0
         FEDTAX(M) = 0
         CF(M) = 0
         DCFTEMP(M) = 0
         IF (M.LE.18) PVSUM(M) = 0.0
      ENDDO

      CONTINUE = .TRUE.
      M = 0
      TOTOTHR = 0.0
      DO WHILE ((M .LT. PRJ).AND.(CONTINUE))
         M = M + 1
!        IF (OGSMMOD.EQ.1) THEN
!  COMPUTE NUMBER OF SUCCESSFUL EXPLORATORY & DEVELOP. WELLS
!  FROM THE TOTAL WELL SCHEDULE  (WT1 -10/20/93)
!          SUCEXP = EXPSR * EXPSCH(CLASS,M)
!          SUCDEV = DEVSR * DEVSCH(CLASS,M)
!  TEST IF SUCCESS RATE = 0; IF IT IS RESET THE WELL COUNTS TO 0
!          IF (EXPSR.EQ.0.0) SUCEXP = 0
!          IF (DEVSR.EQ.0.0) SUCDEV = 0

!  TOTAL ALL WELLS DRILLED FOR OPERATING COSTS CALCULATION
!          CUMSUC = CUMSUC + SUCEXP + SUCDEV

!  COMPUTE NUMBER OF DRY EXPLORATORY & DEVELOP. WELLS
!          IF (EXPSR .NE. 0.0) THEN
!             DRYEXP = EXPSCH(CLASS,M) * (1-EXPSR)
!          ELSE
!             DRYEXP = 0.0
!          ENDIF
!
!          IF (DEVSR .NE. 0.0) THEN
!             DRYDEV = DEVSCH(CLASS,M) * (1-DEVSR)
!             DRYDEV = 0.0
!          ENDIF
!        ELSE
!  THE WELL SCHEDULES CONTAIN SUCCESSFUL WELLS ONLY
           SUCEXP = EXPSCH(CLASS,M)
           SUCNFW = OTHRSCH(CLASS,M)
           SUCDEV = DEVSCH(CLASS,M)
           IF (OGSMMOD.EQ.3) SUCEXP = 0
!  TEST IF SUCCESS RATE = 0; IF IT IS RESET THE WELL COUNTS TO 0
           IF (EXPSR.EQ.0.0) SUCEXP = 0
           IF (EXPSR.EQ.0.0) SUCNFW = 0
           IF (DEVSR.EQ.0.0) SUCDEV = 0

!  TOTAL ALL WELLS DRILLED FOR OPERATING COSTS CALCULATION
           CUMSUC = CUMSUC + SUCEXP + SUCDEV

!  COMPUTE NUMBER OF DRY EXPLORATORY & DEVELOP. WELLS
           IF (EXPSR .NE. 0.0) THEN
              DRYEXP = EXPSCH(CLASS,M) * ((1-EXPSR)/EXPSR)
              DRYNFW = OTHRSCH(CLASS,M) * ((1-EXPSR)/EXPSR)
           ELSE
              DRYEXP = 0.0
           ENDIF

           IF (OGSMMOD.EQ.3) DRYEXP = EXPSCH(CLASS,M)

           IF (DEVSR .NE. 0.0) THEN
              DRYDEV = DEVSCH(CLASS,M) * ((1-DEVSR)/DEVSR)
           ELSE
              DRYDEV = 0.0
           ENDIF
!        ENDIF

!  COMPUTE THE PRESENT VALUE FACTOR IN YEAR M
         PV = (1/(1+DISCRT))**(M-1)

!  COMPUTE REVENUE
         REV(M) = (PRODPRIM(M) * (PRICE1(M)-TRANPRIM)) + (PRODCOP(M) &
                  * (PRICE2(M) - TRANCOP))

!  COMPUTE ROYALTY
         ROY(M) = REV(M) * ROYALRT

!  COMPUTE PRODUCTION TAX
         IF (OGSMMOD.EQ.2) THEN
            PRIMSEV = OGSEVR_AK(M,PRODPRIM(M),CUMSUC)
            PRODTAX(M) = PRODPRIM(M)*(1-ROYALRT)*PRIMSEV
         ELSE
            PRODTAX(M) = ((PRODPRIM(M)*(PRICE1(M)-TRANPRIM)*PRIMSEV)+ &
                          (PRODCOP(M)*(PRICE2(M)-TRANCOP)*COPSEV)) * &
                          (1-ROYALRT) + ADVRT*(REV(M)*(1-ROYALRT))
         ENDIF

!  COMPUTE DRILL COSTS
         DRILL(M) = (EXPDRILL * SUCEXP) + (DEVDRILL * SUCDEV) + &
                    (EXDRYCST * DRYEXP) + (DVDRYCST * DRYDEV) + &
                    (OTHRCOST * SUCNFW) + (EXDRYCST * DRYNFW)
                    
!==========================> HSM Code Start <==========================
         hsm_exp_drill(M)       = (EXPDRILL * SUCEXP)
         hsm_dev_drill(M)       = (DEVDRILL * SUCDEV)
         hsm_exp_dry_drill(M)   = (EXDRYCST * DRYEXP)
         hsm_dev_dry_drill(M)   = (DVDRYCST * DRYDEV)
!===========================> HSM Code End <===========================


!  COMPUTE LEASE EQUIP COSTS
         EQUIP(M) = LEASE * (SUCEXP + SUCDEV)

!  COMPUTE OPERATING COSTS
         IF (OGSMMOD.EQ.2) THEN
           OPER(M) = OGOPER_AK(PRODPRIM(M))
           
!==========================> HSM Code Start <==========================
           hsm_oper(M) = OPER(M)
!===========================> HSM Code End <===========================
           
         ELSE
           OPER(M) = OPERATE * CUMSUC
         ENDIF

!  COMPUTE OTHER CAPITAL EXPENSES
!  IF SUCCESS RATE = 0 THAN DO NOT ASSIGN ANY CAPITAL COSTS
         IF (EXPSR.NE.0.0 .OR. DEVSR.NE.0.0) THEN
            IF (OGSMMOD.EQ.3 .AND. M.EQ.3 .AND. CLASS.EQ.1) THEN
               OTHERCAP(M) = KAP(M) + LEASOFFPRJ(FUEL)
            ELSE
               OTHERCAP(M) = KAP(M)
            ENDIF
         ELSE
            OTHERCAP(M) = 0.0
         ENDIF

!  COMPUTE ABANDONMENT COSTS
         TOTOTHR = TOTOTHR + KAP(M)

!  COMPUTE EXPECTED EXPENSED DRILL COSTS
       XIDC_S(M) = (EXPDRILL * (1-KAPRT(1)) * (1-XDCKAPAKRT) * SUCEXP)+ &
                   (DEVDRILL * (1-KAPRT(2)) * (1-XDCKAPAKRT) * SUCDEV)+ &
                   (OTHRCOST * SUCNFW)
       XIDC_F(M) = ((EXPDRILL * (1-KAPRT(1)) * (1-XDCKAPAKRT) * SUCEXP)+ &
                   (DEVDRILL * (1-KAPRT(2)) * (1-XDCKAPAKRT) * SUCDEV)+ &
                   (OTHRCOST * SUCNFW)) * (1-ITCRT)

!  COMPUTE DEPRECIATED INTANGIBLE DRILLING COSTS (IDC)
         IF (M.LE.(PRJ-RECPRD1+1)) THEN
            BETA = RECPRD1
            BETAFLG = .TRUE.
         ELSE
            BETA = PRJ-M+1
            BETAFLG = .FALSE.
         ENDIF

         DO L=1,BETA
!  COMPUTE THE PRESENT VALUE OF INFLATION IN YEAR L
            PVINFL = (1/(1+INFLRT))**(L-1)
            DEP = OG_TAX_CALC_IDC(BETAFLG,BETA,L,PRJ,M)
            N = M + L - 1
!  COMPUTE PV OF EXPECTED AMORTIZED INTANGIBLE DRILL COSTS
          AIDC_S(N) = AIDC_S(N) + (((EXPDRILL * (1-KAPRT(1)) * &
                    XDCKAPAKRT * SUCEXP) + (DEVDRILL * (1-KAPRT(2)) &
                    * XDCKAPAKRT * SUCDEV) * DEP) * PVINFL)
          AIDC_F(N) = AIDC_F(N) + (((EXPDRILL * (1-KAPRT(1)) * &
                    XDCKAPAKRT * SUCEXP) + (DEVDRILL * (1-KAPRT(2)) &
                    * XDCKAPAKRT * SUCDEV) * DEP) * PVINFL) * (1-ITCRT)
         ENDDO

!  COMPUTE DEPRECIATED TANGIBLE DRILL COSTS (TDC)
         IF (M.LE.(PRJ-RECPRD2+1)) THEN
            BETA = RECPRD2
            BETAFLG = .TRUE.
         ELSE
            BETA = PRJ-M+1
            BETAFLG = .FALSE.
         ENDIF

         DO L=1,BETA
!  COMPUTE THE PRESENT VALUE OF INFLATION IN YEAR L
            PVINFL = (1/(1+INFLRT))**(L-1)
            DEP = OG_TAX_CALC_TDC(BETAFLG,BETA,L,PRJ,M)
            N = M + L - 1

!  COMPUTE PV EXPEC TANG. DRILL,LEASE EQUIP,OTHER CAP. EXPND COST
          DEPREC_S(N) = DEPREC_S(N) + (((EXPDRILL * KAPRT(1) + LEASE) * &
                      SUCEXP) + ((DEVDRILL * KAPRT(2) + LEASE) * &
                      SUCDEV) + OTHERCAP(M)) * DEP * PVINFL
          DEPREC_F(N) = DEPREC_F(N) + (((EXPDRILL * KAPRT(1) + LEASE) * &
                      SUCEXP) + ((DEVDRILL * KAPRT(2) + LEASE) * &
                      SUCDEV) + OTHERCAP(M)) * DEP * PVINFL * (1-ITCRT)
         ENDDO

!  COMPUTE DRY HOLE COST
         DHC(M) = (EXDRYCST*(DRYEXP+DRYNFW)) + (DVDRYCST*DRYDEV)

         TAXBASE_S(M) = REV(M) - ROY(M) - PRODTAX(M) - OPER(M) - &
               ABANDON(M) - XIDC_S(M) - AIDC_S(M) - DEPREC_S(M) - DHC(M)
         TAXBASE_F(M) = REV(M) - ROY(M) - PRODTAX(M) - OPER(M) - &
               ABANDON(M) - XIDC_F(M) - AIDC_F(M) - DEPREC_F(M) - DHC(M)

!  compute investment tax credit
         icredit(m) = itcrt * (expdrill*sucexp + devdrill*sucdev + &
                      equip(m))

!  compute windfall profit tax
         if ((curiyr.ge.1980-baseyr+1).and. &
             (curiyr.le.1984-baseyr+1)) then

           if (m.eq.1) then
             if (curiyr.eq.1980-baseyr+1) wptbase = 25.68
             if (curiyr.eq.1981-baseyr+1) wptbase = 26.11
             if (curiyr.eq.1982-baseyr+1) wptbase = 26.68
             if (curiyr.eq.1983-baseyr+1) wptbase = 27.18
             if (curiyr.eq.1984-baseyr+1) wptbase = 27.59
           else
             wptbase = wptbase * (1.0202)
           endif

           if (m.le.14) then
             if (fuel.le.2) then
               windfall(m) = (1-primsev) * prodprim(m) * &
                             (price1(m)-wptbase) * wptrt
             else
               windfall(m) = (1-copsev) * prodcop(m) * &
                             (price2(m)-wptbase) * wptrt
             endif
             if (windfall(m).lt.0.0) windfall(m) = 0.0
           else
             windfall(m) = 0.0
           endif

         else
           windfall(m) = 0.0
         endif

!  COMPUTE UNCONVENTIONAL FUEL TAX CREDIT
            FCREDIT(M) = 0.0
            IF (CURIYR.LE.1992-baseyr+1.AND. &
                M.LE.2002-baseyr+1-CURIYR+1) THEN
               IF (FUEL.EQ.5) FCREDIT(M) = taxcred(1) * PRODPRIM(M)
               IF (FUEL.EQ.6) FCREDIT(M) = taxcred(2) * PRODPRIM(M)
               IF (FUEL.EQ.7) FCREDIT(M) = taxcred(3) * PRODPRIM(M)
      	    ENDIF

!  COMPUTE STATE INCOME TAXES
         STTAX(M) = TAXBASE_S(M) * STTAXRT

!  COMPUTE FEDERAL TAXES
         FEDTAX(M) = (TAXBASE_F(M)-STTAX(M)-WINDFALL(M))*FEDTAXRT - &
                     FCREDIT(M) - ICREDIT(M)

!  COMPUTE CASH FLOW
         CF(M) = REV(M) - ROY(M) - PRODTAX(M) - DRILL(M) - EQUIP(M) - &
          OTHERCAP(M) - OPER(M) - ABANDON(M) - STTAX(M) - FEDTAX(M) - &
          windfall(m)

!  CALC ABANDON COSTS IF REV-ROY-PRODTAX<OPER & PRJ YEAR > 6
         IF ((REV(M)+FCREDIT(M)-ROY(M)-PRODTAX(M)).LT.OPER(M) &
              .AND.M.GT.6) THEN
!  SET ABANDON TO 10% OF TOTAL OTHER CAPITAL COSTS
            ABANDON(M) = TOTOTHR * 0.1
!  COMPUTING AFTER TAX ABANDONMENT COSTS (PJ3 - 6/14/94)
            CF(M) = -(1-FEDTAXRT)*(1-STTAXRT)*ABANDON(M)
            CONTINUE = .FALSE.    ! EXIT OUT OF LOOP
            REV(M) = 0.0
            ROY(M) = 0.0
            PRODTAX(M) = 0.0
            DRILL(M) = 0.0
            EQUIP(M) = 0.0
            OPER(M) = 0.0
            OTHERCAP(M) = 0.0
            STTAX(M) = 0.0
            FEDTAX(M) = 0.0
         	    TAXBASE_S(M)=0.0
         	    TAXBASE_F(M)=0.0
        ELSE
            ABANDON(M) = 0.0
          ENDIF

!  COMPUTE DISCOUNTED CASH FLOW
         DCFTEMP(M) = CF(M) * PV
! COMPUTE PRESENT VALUES FOR REPORT WRITER
         PVSUM(1) = PVSUM(1) + REV(M) * PV
         PVSUM(2) = PVSUM(2) + ROY(M) * PV
         PVSUM(3) = PVSUM(3) + PRODTAX(M) * PV
         PVSUM(4) = PVSUM(4) + DRILL(M) * PV
         PVSUM(5) = PVSUM(5) + EQUIP(M) * PV
         PVSUM(6) = PVSUM(6) + OPER(M) * PV
         PVSUM(7) = PVSUM(7) + ABANDON(M) * PV
         PVSUM(8) = PVSUM(8) + OTHERCAP(M) * PV
         PVSUM(9) = PVSUM(9) + XIDC_F(M) * PV
         PVSUM(10) = PVSUM(10) + AIDC_F(M) * PV
         PVSUM(11) = PVSUM(11) + DEPREC_F(M) * PV
         PVSUM(12) = PVSUM(12) + DHC(M) * PV
         PVSUM(13) = PVSUM(13) + TAXBASE_F(M) * PV
         PVSUM(14) = PVSUM(14) + ICREDIT(M) * PV
         PVSUM(15) = PVSUM(15) + FCREDIT(M) * PV
         PVSUM(16) = PVSUM(16) + WINDFALL(M) * PV
         PVSUM(17) = PVSUM(17) + STTAX(M) * PV
         PVSUM(18) = PVSUM(18) + FEDTAX(M) * PV

!  COMPUTE TOTAL DCF
         DCFTOT = DCFTOT + DCFTEMP(M)
      ENDDO
      
!==========================> HSM Code Start <==========================
if (hsm_dcf_bool) then
    ! hsm log file, ignore gas (fuel != 1)
    if (fuel.eq.1) then
        ! 'function', 'year', 'crude_price', 'natgas_price', 'crude_trans_price', &
        ! 'natgas_trans_price', 'crude_tariff_price', 'natgas_tariff_price', 'royalty_rate', 'fed_tax_rate', &
        ! 'net_present_value', 'abandon_rate', 'state_abbreviation', 'exploratory_success_rate', 'development_success_rate', &
        ! 'exploratory_cost', 'development_cost', 'exploratory_dry_cost', 'development_dry_cost', 'exp_tang_frac', &
        ! 'dev_tang_frac', 'kap_tang_frac', 'intang_amor_frac'
        ! hsm inputs
        write(hsm_dcf_opts, '(*(G0.16,:,","))') 'OG_DCF', curcalyr, PRICE1(1), PRICE2(1), 0.0, 0.0, TRANPRIM, TRANCOP, & 
              ROYALRT, FEDTAXRT, DCFTOT, TOTOTHR * 0.1, 'AK', EXPSR, DEVSR, EXPDRILL, DEVDRILL, EXDRYCST, DVDRYCST, KAPRT(1), &
              KAPRT(2), 1.0, XDCKAPAKRT, 'slm_6_half' , 'macrs_7', DISCRT
        ! hsm inputs
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'crude_production'      , PRODPRIM
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'natgas_production'     , PRODCOP
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'operating_cost'        , hsm_oper
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'fed_credit'            , FCREDIT
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'exp_drill_cost'        , hsm_exp_drill
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'dev_drill_cost'        , hsm_dev_drill
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'exp_dry_cost'          , hsm_exp_dry_drill
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'dev_dry_cost'          , hsm_dev_dry_drill
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'kap_cost'              , OTHERCAP
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'equip_cost'            , EQUIP
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'dry_hole_cost'         , DHC
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'invest_credit'         , ICREDIT
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'abandon_cost'          , ABANDON
        ! hsm checks
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'revenue'               , REV
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'royalty'               , ROY
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'severance_tax'         , PRODTAX
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'drill_cost'            , DRILL
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'expn_intang_cost'      , XIDC_S
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'amor_intang_cost'      , AIDC_S
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'deprec_tang_cost'      , DEPREC_S
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'state_tax'             , STTAX - STTAXRT * ABANDON ! Abandonment is nagative?
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'fed_tax'               , FEDTAX - (ABANDON - STTAXRT * ABANDON) * FEDTAXRT
        write(hsm_dcf_ak  , '(*(G0.16,:,","))') 'OG_DCF', curcalyr, 'cash_flow'             , CF
    endif
endif
!===========================> HSM Code End <===========================

!  ADJUST ALL DCFs UPWARD TO ELIMINATE THE CHANCE OF A NEGATIVE DCF
!     if (CLASS.NE.1) THEN
!       DCFTOT = DCFTOT + 15000000
!       IF (OGSMMOD.EQ.3) DCFTOT = DCFTOT + 55000000
!     ENDIF

      IF (CLASS.EQ.1) THEN
        OG_DCF = DCFTOT * EXPSR   ! ADJUST EXPLORATORY WELLS ONLY
      ELSE
        OG_DCF = DCFTOT * DEVSR
      ENDIF
!     OG_DCF = DCFTOT

!  TEST THE DCF REPORTING FLAG FOR EACH OF THE SUB MODULES THAT
!  CALL THIS FUNCTION ALONG WITH THE CORRESPONDING MOD FLAG.
      IF (PRTDBGL.EQ.1 .AND. &
         ((OGREPORT(14).GT.0.AND.OGSMMOD.EQ.3) .OR. &
          (OGREPORT(16).GT.0.AND.OGSMMOD.EQ.2))) THEN
         YEARVAL = CURIYR + BASEYR - 1
!        WRITE(BUGOUT,*) '=============================='
!        WRITE(BUGOUT,*) 'CURRENT YEAR  ',YEARVAL
!        WRITE(BUGOUT,*) 'MOD FLAG=',OGSMMOD,'  WELL CLASS=',CLASS
!        WRITE(BUGOUT,*) 'REGION=',REGION,'  FUEL=',FUEL
!        WRITE(BUGOUT,*) '=============================='
         if (YEARVAL .eq. 2003)then
         WRITE(BUGOUT,*) '1'
         WRITE(BUGOUT,*) ' **********  DCF PARAMETER REPORT ********'
         WRITE(BUGOUT,*) ' *****************************************'
         WRITE(BUGOUT,*) ' MOD FLAG (1-L48ON,2-AK,3-OFF)',OGSMMOD
         WRITE(BUGOUT,*) ' WELL CLASS INDEX             ',CLASS
         WRITE(BUGOUT,*) ' REGION INDEX                 ',REGION
         WRITE(BUGOUT,*) ' FUEL TYPE INDEX              ',FUEL
         WRITE(BUGOUT,*) ' PROJECT LIFE                 ',PRJ
         WRITE(BUGOUT,*) ' RECOVERY PERIOD FOR IDC      ',RECPRD1
         WRITE(BUGOUT,*) ' RECOVERY PERIOD FOR TDC      ',RECPRD2
!        WRITE(BUGOUT,*) ' PRICE OF PRIM FUEL (YEAR 1)  ',PRICE1(M)
!        WRITE(BUGOUT,*) ' PRICE OF COPRODUCT (YEAR 1)  ',PRICE2(M)
         WRITE(BUGOUT,*) ' DECLINE RATE                 ',DECLRT
         WRITE(BUGOUT,*) ' DISCOUNT RATE                ',DISCRT
         WRITE(BUGOUT,*) ' INFLATION RATE               ',INFLRT
         WRITE(BUGOUT,*) ' ROYALTY RATE                 ',ROYALRT
         WRITE(BUGOUT,*) ' TRANSPORTATION COST PRIMARY  ',TRANPRIM
         WRITE(BUGOUT,*) ' TRANSPORTATION COST COPRODUCT',TRANCOP
         WRITE(BUGOUT,*) ' SUCCESS RATES                ',EXPSR
         WRITE(BUGOUT,*) ' SUCCESS RATES                ',DEVSR
!        WRITE(BUGOUT,*) ' OTHER EXPECTED CAP. COSTS    ',KAP(M)
         WRITE(BUGOUT,*) ' EXPLORATORY DRILL COST       ',EXPDRILL
         WRITE(BUGOUT,*) ' DEVELOPMENTAL DRILL COST     ',DEVDRILL
         WRITE(BUGOUT,*) ' DRY HOLE COST                ',EXDRYCST
         WRITE(BUGOUT,*) ' DRY HOLE COST                ',DVDRYCST
         WRITE(BUGOUT,*) ' LEASE EQUIPMENT COST         ',LEASE
         WRITE(BUGOUT,*) ' OPERATING COST               ',OPERATE
         WRITE(BUGOUT,*) ' NEW FIELD WILDCAT COST       ',OTHRCOST
         WRITE(BUGOUT,*) ' STATE TAX RATE               ',STTAXRT
         WRITE(BUGOUT,*) ' FEDERAL TAX RATE             ',FEDTAXRT
         WRITE(BUGOUT,*) ' SEVERANCE RATE PRIMARY FUEL  ',PRIMSEV
         WRITE(BUGOUT,*) ' SEVERANCE RATE COPRODUCT     ',COPSEV
         WRITE(BUGOUT,*) ' AD VALOREM TAX RATE          ',ADVRT
         WRITE(BUGOUT,*) ' EXP TANG DRILL COST DEP      ',KAPRT(1)
         WRITE(BUGOUT,*) ' DEV TANG DRILL COST DEP      ',KAPRT(2)
         WRITE(BUGOUT,*) ' FRACT INTANG DRILL COST DEP  ',XDCKAPAKRT

         WRITE(BUGOUT,*) '1'
         WRITE(BUGOUT,*) ' ***********  DCF COST REPORT  ***********'
         WRITE(BUGOUT,500)
         WRITE(BUGOUT,501)
         DO M=1,PRJ
            WRITE(BUGOUT,505) M,PRODPRIM(M),PRODCOP(M), &
                   EXPSCH(CLASS,M),DEVSCH(CLASS,M),OTHRSCH(CLASS,M), &
                   REV(M),ROY(M),PRODTAX(M),DRILL(M),EQUIP(M), &
                   OPER(M),ABANDON(M),OTHERCAP(M)
         ENDDO
         WRITE(BUGOUT,*)
         WRITE(BUGOUT,510) (PVSUM(M),M=1,8)
         WRITE(BUGOUT,*)

         WRITE(BUGOUT,*) '1'
         WRITE(BUGOUT,*) ' **********  DCF TAXES REPORT **********'
         WRITE(BUGOUT,502)
         WRITE(BUGOUT,503)
         WRITE(BUGOUT,504)
         DO M=1,PRJ
            WRITE(BUGOUT,506) M,PRICE1(M),PRICE2(M),XIDC_F(M),AIDC_F(M), &
                DEPREC_F(M),DHC(M),TAXBASE_F(M),ICREDIT(M),FCREDIT(M), &
                WINDFALL(M),STTAX(M),FEDTAX(M),CF(M),DCFTEMP(M)
         ENDDO
         WRITE(BUGOUT,*)
         WRITE(BUGOUT,512) (PVSUM(M),M=9,18)
         WRITE(BUGOUT,*)

         WRITE(BUGOUT,*)
         WRITE(BUGOUT,*) ' TOTAL DCF VALUE ====>',DCFTOT
       endif
      ENDIF

  500 FORMAT(2X,7X,'PRIM    COPROD  -- WELLS --   ',20X, &
             '  PROD       DRILL      EQUIP      OPER      ', &
             ' ABAND    OTHER')
  501 FORMAT(2X,'YR     PROD     PROD  EXP DEV NFW      ', &
             ' REV       ROY      TAX        COST       ', &
             'COST       COST       COST    CAP COST')
  502 FORMAT(20X,'EXPENS    DEPREC    DEPREC',24x,'INVEST', &
             4X,'UNCONV')
  503 FORMAT(9X,'PRICE      INTANG    INTANG     TANG       DRY', &
          '      TAX-       TAX       TAX     WINDFALL    STATE', &
          '    FEDERAL    CASH')
  504 FORMAT(2X,'YR   PRIM  COPRD   COST      COST      COST', &
          '       COST     BASE     CREDIT    CREDIT       TAX', &
          '       TAX       TAX      FLOW      DCF')
  505 FORMAT(2X,I2,2F9.0,3F4.1,6F11.1,2F10.1)
  506 FORMAT(2X,I2,2F7.2,12F10.0)
  510 FORMAT(2X,'PRESENT VALUE',19X,6F11.1,2F10.1)
  512 FORMAT(2X,'PRESENT VALUE',3X,10F10.0)

      RETURN
      END

!********************************************************************
      REAL FUNCTION OG_DCF2(OGSMMOD,CLASS,WOP)

!  THIS FUNCTION COMPUTES AN AVERAGE DCF VALUE FOR THE FOLLOWING
!  CATEGORIES:
!    - LOWER48 ONSHORE EXPLORATORY
!    - LOWER48 ONSHORE DEVELOPMENTAL
!    - OFFSHORE EXPLORATORY
!    - OFFSHORE DEVELOPMENTAL
!  THIS CALCULATION IS MADE BASED UPON THE REGRESSION OF THE WORLD
!  OIL PRICE AND NATIONAL EXPENDITURES FOR EACH OF THE ABOVE CATS.

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables

      INTEGER OGSMMOD      ! MODULE FLAG (1-L48ON,2-AK,3-OFF)
      INTEGER CLASS        ! WELL CLASS INDEX (EXPLOR OR DEVEL)
      REAL WOP             ! WORLD OIL PRICE

      IF (OGSMMOD .EQ. 1) THEN  ! L48 ONSHORE
         IF (CLASS .EQ. 1) THEN   ! L48 ONSHORE EXPLORATORY
            OG_DCF2 = 175986.3 * (WOP**0.814209)
         ELSE  ! L48 ONSHORE DEVELOPMENTAL
            OG_DCF2 = 607.9 * (WOP**2.230746)
         ENDIF
      ELSE IF (OGSMMOD .EQ. 3) THEN   ! OFFSHORE
         IF (CLASS .EQ. 1) THEN  ! OFFSHORE EXPLORATORY
            OG_DCF2 = 2557.644 * (WOP**3.125110)
         ELSE  ! OFFSHORE DEVELOPMENTAL
            OG_DCF2 = 161234.2 * (WOP**1.100815)
         ENDIF
      ENDIF

      RETURN
      END

!********************************************************************
      REAL FUNCTION OG_TAX_CALC_IDC(BETAFLG,BETA,L,PRJ,M)

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables

      LOGICAL BETAFLG  ! SIGNALS THE FIRST 13 YEARS OF A PROJECT
      INTEGER PRJ  ! PROJECT LIFE
      INTEGER BETA     ! NO. YEARS FOR CAPITAL RECOVERY

!  COMPUTE THE DEPRECIATION FACTOR FOR THE INTANGIBLE DRILLING COSTS
      IF (BETAFLG) THEN
         IF (L.EQ.1.OR.L.EQ.BETA) THEN
            OG_TAX_CALC_IDC = (1.0/(BETA-1.0))/2.0
         ELSE
            OG_TAX_CALC_IDC = 1.0/(BETA-1.0)
         ENDIF
      ELSE
         OG_TAX_CALC_IDC = 1 / (PRJ-M+1)
      ENDIF

      RETURN
      END

!********************************************************************
      REAL FUNCTION OG_TAX_CALC_TDC(BETAFLG,BETA,L,PRJ,M)

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables

      LOGICAL BETAFLG  ! SIGNALS THE FIRST 13 YEARS OF A PROJECT
      INTEGER PRJ  ! PROJECT LIFE
      INTEGER BETA     ! NO. YEARS FOR CAPITAL RECOVERY

      REAL SLM(10)
      REAL ACRS(5)
      REAL MACRS3(4)
      REAL MACRS5(6)
      REAL MACRS7(8)
      REAL MACRS10(11)
      REAL MACRS15(16)
      REAL MACRS20(21)

!  MACRS LOOKUP TABLES FOR EACH RECOVERY PERIOD (PERCENT)
      data slm/18.18,16.36,14.54,12.72,10.91,9.09,7.27,5.45,3.64,1.82/
      data acrs/15.00,22.00,21.00,21.00,21.00/
      DATA MACRS3/33.33,44.45,14.81,7.41/
      DATA MACRS5/20.00,32.00,19.20,11.52,11.52,5.76/
      DATA MACRS7/14.29,24.49,17.49,12.49,8.93,8.92,8.92,4.46/
      DATA MACRS10/10.00,18.00,14.40,11.52,9.22,7.37,6.55,6.55,6.56, &
                   6.55,3.28/
      DATA MACRS15/5.00,9.50,8.55,7.70,6.93,6.23,5.90,5.90,5.91,5.90, &
                   5.91,5.90,5.91,5.90,5.91,2.95/
      DATA MACRS20/3.750,7.219,6.677,6.177,5.713,5.285,4.888,4.522, &
                   4.462,4.461,4.462,4.461,4.462,4.461,4.462,4.461, &
                   4.462,4.461,4.462,4.461,2.231/

!  COMPUTE THE DEPFACTOR FOR THE TANGIBLE DRILLING COSTS
      IF (BETAFLG) THEN
!  USE LOOKUP TABLES
         IF (BETA .EQ. 4) THEN
            OG_TAX_CALC_TDC = MACRS3(L) / 100.0
         ELSE IF (BETA .EQ. 6) THEN
            OG_TAX_CALC_TDC = MACRS5(L) / 100.0
         ELSE IF (BETA .EQ. 8) THEN
            OG_TAX_CALC_TDC = MACRS7(L) / 100.0
         ELSE IF (BETA .EQ. 11) THEN
            OG_TAX_CALC_TDC = MACRS10(L) / 100.0
         ELSE IF (BETA .EQ. 16) THEN
            OG_TAX_CALC_TDC = MACRS15(L) / 100.0
         ELSE IF (BETA .EQ. 21) THEN
            OG_TAX_CALC_TDC = MACRS20(L) / 100.0
         ELSE IF (BETA .EQ. 5) THEN
            OG_TAX_CALC_TDC = ACRS(L) / 100.0
         ELSE IF (BETA .EQ. 10) THEN
            OG_TAX_CALC_TDC = SLM(L) / 100.0
         ENDIF
      ELSE
         OG_TAX_CALC_TDC = 1 / (PRJ-M+1)
      ENDIF

      RETURN
      END
!***************************************************************
!from ECON_MAIN.FOR
!     Last change:  MC    6 May 2009   10:11 am
       SUBROUTINE ECONOMICS(ires,iy,ccode,ctype,cmode,ir)

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmbfw'
       include 'ogsmugr'
       include 'ogsml48'
       include 'ogsmout'
       include 'ngtdmrep'


       INTEGER ires,itech,iyr,nyrsi,inorm,nyrsi2
       INTEGER ist,io,ic,ioilcount,ishift
       INTEGER itend,itenda,iii,im,iy,ir
       INTEGER ccode,ctype                                               !ccode = 0: industrial/natural co2 sources considered
                                                                         !ctype = CO2 source number
       INTEGER cmode                                                     !cmode = 1: prescreen, cmode = 0: economcis

!
! VAIABLES WITH THE X2 DESIGNATION ARE USED AS TEMPORARY VARIABLES
! EITHER TO SUM THE BASE AND ADVANCED CASES OR FOR PRELIMINARY
! CALCULATIONS BEFORE BEING TRANSFERED TO THE FINAL VARIABLES
! THAT ARE SENT TO THE TIMING MODEL.
!
       REAL OILPRODX2(MAX_YR)    !OIL PRODUCTION
       REAL GASPRODX2(MAX_YR)    !GAS PRODUCTION
       REAL NGLPRODX2(MAX_YR)    !NGL PRODUCTION
       REAL OAMX2(MAX_YR)        !VARIABLE OPERATING COST
       REAL AOAMX2(MAX_YR)       !FIXED ANNUAL OPERATING COST
       REAL FACCOSTX2(MAX_YR)    !FACILITIES COSTS
       REAL DRL_CSTX2(MAX_YR)    !DRILLING COSTS
       REAL DRL_CST2X2(MAX_YR)   !DRILLING COSTS
       REAL PATNX2(MAX_YR)
       REAL SUMPX2(MAX_YR)
       REAL FOAMX2(MAX_YR)
       REAL XPATNX2(MAX_YR)      !ACTIVE PATTERNS
       REAL WATPRODX2(MAX_YR)    !WATER PRODUCTION
       REAL REMRESX2(MAX_YR)     !REMAINING RESERVES (Mbbl)
       REAL grEMRESX2(MAX_YR)     !REMAINING RESERVES (MMcf)
       real iremresx2(max_yr)     !inferred reserves (MBbl)         !mc change ver 7
       real igremresx2(max_yr)    !inferred reserves (MMcf)         !mc change ver 7
       REAL WATINJX2(MAX_YR)     !WATER INJECTED
       REAL TOTINJX2(MAX_YR)     !TOTAL INJECTED
       REAL TORECYX2(MAX_YR)     !TOTAL RECYCLED
       REAL INJX2(MAX_YR)        !INJECTED
       REAL EXIST_EOAMX2(MAX_YR) !ENVIRONMENTAL COSTS EXISTING
       REAL NEW_EOAMX2(MAX_YR)   !NEW ENVIRONMENTAL COSTS
       REAL EXIST_ECAPX2(MAX_YR)
       REAL TOTGASPROD(MAX_YR)  !TOTAL GAS PRODUCTION
       REAL STIMX2(MAX_YR)
       REAL COMPX2(MAX_YR)
       REAL PROC_OAMX2(MAX_YR)
       real fraccstx2(max_yr)

       REAL effinv,eff2, cadj
! VARIABLE TO GET NPV FOR PROJECT
       REAL CUMMDATCF
       REAL CUM_TI,CUM_II,CUM_ATCF,CUM_DATCF
       REAL CUM_O, CUM_G, CUM_N, CUM_P, CUM_C
! VARIABLES USED TO FIND THE NORMALIZED BASE,ADVANCED PATTERNS
       REAL PAT_YR(MAX_YR)
       REAL NORM_BAS(MAX_YR)     !% OF PATERNS BASE
       REAL NORM_ADV(MAX_YR)     !% OF PATERNS ADVANCED
       COMMON / NORM_PAT / NORM_BAS, NORM_ADV,pat_yr
!
! VARIABLES USED TO TRANSFER THE # OF WELLS DRILLED
       REAL EINJDRX2(MAX_YR)
       REAL EPRODDRX2(MAX_YR)
       REAL EPRODWELLX2(MAX_YR)
       REAL EINJWELLX2(MAX_YR)
       REAL ESHUTINX2(MAX_YR)
       REAL edryholex2(max_yr)      !4.1.09
       COMMON / NEWWELLS / EINJDRX2, EPRODDRX2, EPRODWELLX2, &
                           EINJWELLX2, ESHUTINX2,edryholex2
!
! INITIALIZATION
       ITECH=1
       ITEND  = 0
       ITENDA = 0
       NYRSI2 = 1
       CUMMDATCF = 0.0
       ioilcount = 0
       DO I=1,MAX_YR
         PAT_YR(I) = 0.0
       END DO


       STARTPR = 1
!  PATTERN DEVELOPMENT FOR THE PROJECT
!            find the maximum number of patterns to develop in that year
       ishift = 0
       if (apcode(ires).eq.10.or.apcode(ires).eq.16) ishift=disclag

         call patt_development(ires,1+ishift,302)

       iii = 0
       DO I=1,max_yr-1
          IF (iii.eq.0) THEN
            IF ((PATDEV(IRES,I,1)+PATDEV(IRES,I,2)).GT.0.0) THEN
             STARTPR = I
             iii = iii+1
!             PRINT*, aresid(ires),patdev(ires,i,1),patdev(ires,i,2),
!     &        startpr,iyr,i
            END IF
          END IF
       END DO

       DO IYR=1,MAX_YR
        IF (OGRUNOP(1).EQ.5) THEN
! IF OIL/GAS PRICE IS ZERO RESET TO PREVIOUS YEARS PRICE
          IF(OILPRICEC(IYR).LE.0.0) OILPRICEC(IYR)=OILPRICEO(IYR)
          IF(GASPRICEC(IYR).LE.0.0) GASPRICEC(IYR)=GASPRICEO(IYR)
          OILPRICE2(IYR) = OILPRICEC(IYR)
          GASPRICE2(IYR) = GASPRICEC(IYR)
        END IF
       ENDDO !END OF PRICE SETTING LOOP
!       pause

!       PRINT*, aresid(ires),itimeyr
!       pause

       IF (OGRUNOP(1).EQ.5) THEN
!!!       IF (ITIMEYR.GT.1) THEN                                             !mc change ver 4 - name change 'itimeyr' -> 'iy'
       if (iy.gt.1)      then                                                !mc change ver 4 - name change 'itimeyr' -> 'iy'

!    THIS IS TO SHIFT THE PRICE TRACK FORWARD TO MATCH THE BEGINNING OF
!    THE PROJECT INITITATION
!    SHIFT PRICES TO NEW ARRAY AND ZERO THE OLD PRICE ARRAY
           DO I=1,MAX_YR
             OILPRICE2(I) = OILPRICEC(I)
             GASPRICE2(I) = GASPRICEC(I)
             OILPRICEC(I)  = 0.0
             GASPRICEC(I)  = 0.0
           END DO
           DO I=1,MAX_YR
!             IF ((I+ITIMEYR-1).LE.MAX_YR) THEN                              !mc change ver 4 - name change 'itimeyr' -> 'iy'
!                OILPRICEC(I) = OILPRICE2(I+ITIMEYR-1)                       !mc change ver 4 - name change 'itimeyr' -> 'iy'
!                GASPRICEC(I) = GASPRICE2(I+ITIMEYR-1)                       !mc change ver 4 - name change 'itimeyr' -> 'iy'

             IF ((I+iy-1).LE.MAX_YR) THEN                                    !mc change ver 4 - name change 'itimeyr' -> 'iy'
                OILPRICEC(I) = OILPRICE2(I+iy-1)                             !mc change ver 4 - name change 'itimeyr' -> 'iy'
                GASPRICEC(I) = GASPRICE2(I+iy-1)                             !mc change ver 4 - name change 'itimeyr' -> 'iy'
             ELSE
                OILPRICEC(I) = OILPRICE2(MAX_YR)
                GASPRICEC(I) = GASPRICE2(MAX_YR)
             END IF
             IF(OILPRICEC(I).LE.0.0) OILPRICEC(I) = OILPRICEC(I-1)
             IF(GASPRICEC(I).LE.0.0) GASPRICEC(I) = GASPRICEC(I-1)
           END DO
       END IF
       END IF

!       print*, oilpricec(1), 'first year price ', aresid(ires), iy
!       pause

!  SET THE OIL PRICE TO THE FIXED PRESCREENING PRICE FOR INDUSTRIAL CO2
       if (cmode.eq.1) then
         do i = 1,max_yr
            oilpriceo(i) = oilco2
            gaspriceo(i) = oilco2/15.
         end do
!      else
!        oilco2 = oilpriceo(1)
       end if

!      if (cmode.ne.1) &
!       write (987,1987) aresid(ires),itimeyr, aregion(ires), (oilpricec(i),i=1,5),oilco2            !mc change ver2
   1987 format (a11,3x,i2,3x,i2,2x,6(f10.2,3x))                                            !mc change ver2

!        do i = 1,40
!           PRINT*, oilpricec(i)
!        end do
!        pause

!  START THE BASE AND ADVANCED TECHNOLOGY LOOP
       do itech = 1,2
!  DETERMINE THE DRY HOLE RATE FOR THE PROJECT

!  shift the variables from ecocom to resprod.  Resprod is used in the itech loop
       call arrayshift(1,ITECH,IRES,1,1)

!  calculate regional success rates
!  apply the drilling success levers at this point
       do im = 1,7!max_reg
                                                                                     !THIS SECTION REVISED 4.16.09
         REGDRYUE(IM) = (SUCEXP(IM)/100.0)*(1.0 - DRILL_FAC(ITECH)) *                 & !4.16.09
          EXPLR_FAC(ITECH)                                                           !4.16.09
         REGDRYUD(IM) = (SUCEXPD(IM)/100.0)*(1.0 - DRILL_FAC(ITECH))                 !4.16.09
         REGDRYKD(IM) = (SUCDEVE(IM)/100.0)*(1.0 - DRILL_FAC(ITECH))                 !4.16.09

         IF(PROCDE.EQ.8.OR.PROCDE.EQ.9) THEN                                         !4.16.09
           REGDRYKD(IM) = (SUCCHDEV/100.0)*(1.0 - DRILL_FAC(ITECH))                  !4.16.09
         END IF                                                                      !4.16.09

         IF(PROCDE.GE.21.AND.PROCDE.LE.23) THEN                                      !4.16.09
           REGDRYUD(IM) = AHEATVAL(IRES)*(1.0 - DRILL_FAC(ITECH))                   !4.16.09
         END IF                                                                      !4.16.09
                                                                                     !END REVISIONS 4.16.09
       end do

!  assign process specific depreciation schedule
       idepryr = depr_yrs(procde+1)
       do im=1,max_qdyr
          deprschl(im) = depr_sch(procde+1,im)
!          PRINT*, resid,procde,idepryr,deprschl(im)
       end do
!       pause


!
! FIND TOTAL BASE AND ADVANCED PATTERNS INITIATED, AND NORMALIZE THEM
! THIS WILL BE THE FACTOR OF BASE AND ADVANCED TECHNOLOGY
!
       IF (STARTPR.GT.1) THEN
! SHIFT THE PRICE, AND DEVELOPMENT SCHEDULE, WRITE ZEROES AT THE END
! OF THE PATTERN DEVELOPMENT SCHEDULE
           DO I=1,MAX_YR-STARTPR
             PATTERNS(I) = PATN1(I+STARTPR-1)
           END DO
       ELSE
           DO I=1,MAX_YR
             PATTERNS(I) = 0.0
             PATTERNS(I) = PATN1(I)
           END DO
       ENDIF !END OF PROJECT SHIFT IF STATEMENT
!

       IF(PROCDE.EQ.0.OR.PROCDE.EQ.11.OR.PROCDE.EQ.12.OR. &
               PROCDE.EQ.13.OR.PROCDE.EQ.14.OR.PROCDE.EQ.15)THEN      ! DECLINE CURVE MODELS
!         PRINT*, aresid(ires),totpat
!         pause
            PATTERNS(1) = TOTPAT
            DO IYR=2,max_yr
               PATTERNS(IYR) = 0.0
            END DO
            IF (ITECH.EQ.2) THEN
               DO IYR=1,max_yr
                  PATTERNS(IYR) = 0.0
               END DO
            END IF
       ENDIF

!       do IYR = 1,max_yr
!         PRINT*, patterns(iyr)
!       end do
!       pause

!       DO INORM = 1 , 40
!         PAT_YR(INORM) = PAT_YR(INORM) + PATTERNS(INORM)
!         PRINT*, aresid(ires),inorm,pat_yr(inorm),patterns(inorm),'st'
!       END DO
!       pause
!
!       IF (ITECH.EQ.2) THEN
!         DO INORM = 1 , 40
!           IF (PAT_YR(INORM).GT.0.0) THEN
!             NORM_ADV(INORM) = PATTERNS(INORM) / PAT_YR(INORM)
!             NORM_BAS(INORM) = 1 - NORM_ADV(INORM)
!           ELSEIF (PAT_YR(INORM).LE.0.0) THEN
!             NORM_ADV(INORM) = 0.0
!             NORM_BAS(INORM) = 0.0
!           END IF
!           IF(aresid(ires).eq.'UGUT2203616') PRINT*, inorm,
!     &      pat_yr(inorm),patterns(inorm)
!         END DO
!           IF(aresid(ires).eq.'UGUT2203616') pause
!       END IF

!  DETERMINE THE Costs for the project
!    IF PROJECT IS OIL RESOURCE
       if (apcode(ires).le.10.or.apcode(ires).eq.17) then
         io = 301
         CALL COST_OIL(ires,itech,ccode,ctype,ir)
       end if
!    END IF

!    IF PROJECT IS GAS RESOURCE
       if (apcode(ires).ge.11.and.apcode(ires).ne.17) then
         io = 302
         CALL COST_GAS(ires,itech)
       end if
!
!     GET THE PATTERN LIFE FOR ITECH = 1
!     PATTERN LIFE FOR ITECH = 2 WILL BE THE LAST ILIFE SHOWN
!

       DO INORM = 1 , max_yr-1
         PAT_YR(INORM) = PAT_YR(INORM) + PATTERNS(INORM)
!         PRINT*, aresid(ires),inorm,pat_yr(inorm),patterns(inorm),'st'
       END DO

       IF (ITECH.EQ.2) THEN
         DO INORM = 1 , max_yr-1
           IF (PAT_YR(INORM).GT.0.0) THEN
             NORM_ADV(INORM) = PATTERNS(INORM) / PAT_YR(INORM)
             NORM_BAS(INORM) = 1 - NORM_ADV(INORM)
           ELSEIF (PAT_YR(INORM).LE.0.0) THEN
             NORM_ADV(INORM) = 0.0
             NORM_BAS(INORM) = 0.0
           END IF
!           IF(aresid(ires).eq.'UGUT2203616') PRINT*, inorm,
!     &      pat_yr(inorm),patterns(inorm),'main'
         END DO
!           IF(aresid(ires).eq.'UGUT2203616') pause
       END IF


       IF (ITECH.EQ.1) THEN
         ILIFEX2=ILIFE
       END IF
!    END IF
       DO IYR=1,max_yr-1
          IF (ITECH.EQ.1) THEN
             OILPRODX2(IYR)    = 0.0
             GASPRODX2(IYR)    = 0.0
             NGLPRODX2(IYR)    = 0.0
             WATINJX2(IYR)     = 0.0
             TOTINJX2(IYR)     = 0.0
             TORECYX2(IYR)     = 0.0
             INJX2(IYR)        = 0.0
             OAMX2(IYR)        = 0.0
             AOAMX2(IYR)       = 0.0
             STIMX2(IYR)       = 0.0
             COMPX2(IYR)       = 0.0
             PROC_OAMX2(IYR)   = 0.0

             FACCOSTX2(IYR)    = 0.0
             fraccstx2(iyr)    = 0.0
             DRL_CSTX2(IYR)    = 0.0
             DRL_CST2X2(IYR)   = 0.0
             PATNX2(IYR)       = 0.0
             SUMPX2(IYR)       = 0.0
             FOAMX2(IYR)       = 0.0
             XPATNX2(IYR)      = 0.0
             WATPRODX2(IYR)    = 0.0
             REMRESX2(IYR)     = 0.0
             gremresx2(iyr)    = 0.0
             iremresx2(iyr)    = 0.0                  !mc change ver 7
             igremresx2(iyr)   = 0.0                  !mc change ver 7
             EXIST_EOAMX2(IYR) = 0.0
             NEW_EOAMX2(IYR)   = 0.0
             EXIST_ECAPX2(IYR) = 0.0
             GG(IYR)           = 0.0
             TORECY_CST(IYR)   = 0.0
          END IF
!  INTEGRATE BASE AND ADVANCED TECHNOLOGY VARIABLES INTO SINGLE STREAM
! SUM THE NEEDED CASHFLOW VALUES FOR BASE AND ADVANCED CASES
          OILPRODX2(IYR)    = OILPRODX2(IYR)     + OILPROD(IYR)
          GASPRODX2(IYR)    = GASPRODX2(IYR)     + GASPROD(IYR)
          NGLPRODX2(IYR)    = NGLPRODX2(IYR)     + NGLPROD(IYR)
          WATPRODX2(IYR)    = WATPRODX2(IYR)     + WATPROD(IYR)
          WATINJX2(IYR)     = WATINJX2(IYR)      + WATINJ(IYR)
          TOTINJX2(IYR)     = TOTINJX2(IYR)      + TOTINJ(IYR)
          TORECYX2(IYR)     = TORECYX2(IYR)      + TORECY(IYR)
          INJX2(IYR)        = INJX2(IYR)         + INJ(IYR)
          EXIST_EOAMX2(IYR) = EXIST_EOAMX2(IYR)  + EXIST_EOAM(IYR)
          NEW_EOAMX2(IYR)   = NEW_EOAMX2(IYR)    + NEW_EOAM(IYR)
          EXIST_ECAPX2(IYR) = EXIST_ECAPX2(IYR)  + EXIST_ECAP(IYR)
          AOAMX2(IYR)       = AOAMX2(IYR)        + AOAM(IYR)
          STIMX2(IYR)       = STIMX2(IYR)        + STIM(IYR)
          COMPX2(IYR)       = COMPX2(IYR)        + COMP(IYR)
          PROC_OAMX2(IYR)   = PROC_OAMX2(IYR)    + PROC_OAM(IYR)
          FACCOSTX2(IYR)    = FACCOSTX2(IYR)     + FACCOST(IYR)
          fraccstx2(iyr)    = fraccstx2(iyr)     + fraccst(iyr)
          DRL_CST2X2(IYR)   = DRL_CST2X2(IYR)    + DRL_CST2(IYR)
          PATNX2(IYR)       = PATNX2(IYR)        + PATN(IYR)
          XPATNX2(IYR)      = XPATNX2(IYR)       + XPATN(IYR)
          SUMPX2(IYR)       = SUMPX2(IYR)        + SUMP(IYR)
          FOAMX2(IYR)       = FOAMX2(IYR)        + FOAM(IYR)
          REMRESX2(IYR)     = REMRESX2(IYR)      + REMRES(IYR)
          gREMRESX2(IYR)    = gREMRESX2(IYR)     + gREMRES(IYR)
          iremresx2(iyr)    = iremresx2(iyr)     + iremres(iyr)         !mc change ver 7
          igremresx2(iyr)   = igremresx2(iyr)    + igremres(iyr)        !mc change ver 7
          OAMX2(IYR)        = OAMX2(IYR)         + OAM(IYR)

! RESET SUM TO VARIABLE EXPECTED BY THE CASHFLOW ROUTINE
          OILPROD(IYR)    = OILPRODX2(IYR)
          GASPROD(IYR)    = GASPRODX2(IYR)
          NGLPROD(IYR)    = NGLPRODX2(IYR)
          WATPROD(IYR)    = WATPRODX2(IYR)
          WATINJ(IYR)     = WATINJX2(IYR)
          TOTINJ(IYR)     = TOTINJX2(IYR)
          TORECY(IYR)     = TORECYX2(IYR)
          INJ(IYR)        = INJX2(IYR)
          PROC_OAM(IYR)   = PROC_OAMX2(IYR)
          OAM(IYR)        = OAMX2(IYR)
          AOAM(IYR)       = AOAMX2(IYR)
          STIM(IYR)       = STIMX2(IYR)
          COMP(IYR)       = COMPX2(IYR)
          FACCOST(IYR)    = FACCOSTX2(IYR)
          fraccst(iyr)    = fraccstx2(iyr)                               !mc change B
!          DRL_CST(IYR)    = DRL_CSTX2(IYR)
          DRL_CST2(IYR)   = DRL_CST2X2(IYR)
          PATN(IYR)       = PATNX2(IYR)
          XPATN(IYR)      = XPATNX2(IYR)
          SUMP(IYR)       = SUMPX2(IYR)
          FOAM(IYR)       = FOAMX2(IYR)
          REMRES(IYR)     = REMRESX2(IYR)
          gREMRES(IYR)    = gREMRESX2(IYR)
          iremres(iyr)    = iremresx2(iyr)                               !mc change ver 7
          igremres(iyr)   = igremresx2(iyr)                              !mc change ver 7
          EXIST_EOAM(IYR) = EXIST_EOAMX2(IYR)
          NEW_EOAM(IYR)   = NEW_EOAMX2(IYR)
          EXIST_ECAP(IYR) = EXIST_ECAPX2(IYR)
          DRL_CST(IYR)    = DRL_CST2(IYR)
          LEASCST(IRES,IYR) = FACCOST(IYR)
          DRILLCST(IRES,IYR) = DRL_CST(IYR)
          DRYCST(IRES,IYR) = DRY_CST(IYR) 
          OPCOST(IRES,IYR) = AOAM(IYR)

!  FORCE DEVELOPMENT OF PROJECTS CURRENTLY BEING DEVELOPED


           !Oil
           if (((apcode(ires).eq.17))) then
             if (itimeyr.eq.1.and.(anwellinj(ires)).ge.1) then
               cadj = 1.0
               if(anwellinj(ires).gt.5) cadj=0.7
               if(anwellinj(ires).gt.50) cadj=0.5
               drl_cst(iyr) = drl_cst(iyr)*cadj
             endif
           endif


           !GAS
           if (((apcode(ires).eq.21.or.apcode(ires).eq.22))) then
             if (itimeyr.eq.1.and.(anwellinj(ires)).ge.1) then
               cadj = 0.9
               if(anwellinj(ires).gt.5) cadj=0.5
               if(anwellinj(ires).gt.50) cadj=0.3
               drl_cst(iyr) = drl_cst(iyr)*cadj
             endif
           endif


         ENDDO

          elyroil(ires) = lyroil
          elyrgas(ires) = lyrgas

!         PRINT*, aresid(ires),itech,lyroil,elyroil(ires),ires
!         pause
! FIND THE GG FACTOR FOR THE COMBINED CASE, SUM OF BASE AND ADVANCED
         IF(PROCDE.EQ.0)THEN
               GG(1) = GG(1) + DRL_CST(1) * TANG_M(1) * GG_FAC
         ELSE
               GG(1) = 0.0
         ENDIF

!  END THE BASE AND ADVANCED TECHNOLOGY LOOP
       end do !(itech loop)

       call capital_cost(ires)

 1101  format (i2,4(3x,f12.3))

!  RUN THE CASHFLOW
       CALL CASHFLOW(nyrsi, ires, aresid(ires))

!  CALCULATE THE DCFROR
       CALL DCFROR(NYRSI,EFFINV)

       ENYRSI(IRES) = NYRSI
       CUM_TI = 0.0
       CUM_II = 0.0
       CUM_ATCF= 0.0
       CUM_DATCF=0.0
          CUM_O=0.0
          CUM_G=0.0
          CUM_N=0.0
          CUM_P=0.0
          CUM_C=0.0

       DO I=1,NYRSI
          CUM_II = CUM_II + ICST(I) / ((1.0+0.15)**(I-0.5))
          CUM_TI = CUM_TI + TI(I) / ((1.0+0.15)**(I-0.5))
          CUM_ATCF=CUM_ATCF + ATCF(I)
          CUM_DATCF=CUM_DATCF + DATCF(I)
          CUM_O=CUM_O + OILPROD(I)
          CUM_G=CUM_G + GASPROD(I)
          CUM_N=CUM_N + NGLPROD(I)
          CUM_P=CUM_P + PATN(I)
          CUM_C=CUM_C + TOTINJ(I)-TORECY(I)
       END DO

       PATNDCF(IRES,I) = DATCF(I)        !!!* PATN(I)
!
!  SET RANKING CRITERIA
!      use either the investment efficiency or the rate of return

       EFF2 = EFFINV
       IF ((CUM_TI+CUM_II).GT.0.0) THEN
          EFFINV = CUM_DATCF / (CUM_TI+CUM_II)
       ELSE
          EFFINV = 99000.0
       END IF
       IF (CUM_DATCF.LE.0.0) EFFINV = 0.0

!***********************************************************************
!
!     THIS SECTION OF CODE ADDED TO CALCULATE THE NUMBER OF
!     WELLS DRILLED BY YEAR AND THE NUMBER OF ACTIVE WELLS
!     BY YEAR.
!             EINJDR    NEW INJECTION WELLS DRILLED PER YEAR
!             EPRODDR   NEW PRODUCTION WELLS DRILLED PER YEAR
!             ESHUTIN   SHUTIN WELLS
!             EPRODWELL ACTIVE PRODUCING WELLS PER YEAR
!             EINJWELL  ACTIVE INJECTION WELLS PER YEAR
!             XPP1      TOTAL NUMBER OF PRODUCERS
!             XPP2      TOTAL NUMBER OF INJECTORS
!             XPP3      PRODUCERS TO INJECTORS
!             XPP4      PRODUCERS CONVERTED TO SECONDARY PRODUCERS
!
!
        CALL WELLCOUNTS(IRES,PAT_YR)

!***********************************************************************
!
!     RESET THE CASHFLOW ECONOMICS OUTPUT TO THE VARIABLES DEFINED IN
!     THE TIMING/EXPLORATION MODELS.  THEN DO THE PROJECT SHIFT TO
!     THE PROJECT START YEAR.
!
!***********************************************************************
!
       CALL INIT_OUTARRAY(IRES)

!       IF(aresid(ires).eq.'DOLA6053904'.or.
!     &  aresid(ires).eq.'DOCA6920204') then
!         WRITE(777,*) 'after initialization'
!         do IYR = 1,40
!          WRITE(777,1777) aresid(ires),itimeyr,gasprod(iyr),
!     &     eprodgas(ires,iyr)
!         end do
!         WRITE(777,*)
!       END if
! 1777  format (a11,2x,i2,2x,f10.3,2x,f10.3)

! IF PATTERN LIFE OR PROJECT LIFE IS 1 CONSIDER IT UNECONOMIC AND RETURN
! ZEROS.  BECAUSE OF THE TWO TECHNOLOGIES USE ILIFEX2+ILIFE/2.  IF
! BASE AND ADVANCED HAVE PATTERN LIVES OF 1 YEAR THEN TELL TIMING MODEL
! WITH ONEYEAR SWITCH NOT TO TIME IN.
!
       ONEYEAR(IRES) = .FALSE.
         IF (NYRSI.LE.1 .OR. (FLOAT(ILIFE+ILIFEX2)/2.0).LE.1.0) THEN
           ONEYEAR(IRES) = .TRUE.
         END IF

!         PRINT*, aresid(ires),ilife,ilifex2,FLOAT(ILIFE+ILIFEX2)/2.0
!     &     ,oneyear(ires),nyrsi,'c'
!         PRINT*, ""
!         pause




!
! FIND LAST YEAR OF ACTUAL ECONOMIC PRODUCTION, IF THERE
! IS NO PRODUCTION PRIOR TO END OF PROJECT LIFE SET PROJECT
! LIFE TO YEAR AFTER LAST YEAR OF PRODUCTION, IF A PROJECT GOES
! UNECONOMIC AND THERE IS PRODUCTION IN THE FOLLOWING YEAR
! REPORT THAT AS THE LAST YEAR.
!
!  IN 1ST YEAR UNECONOMIC
!   IF THERE IS OIL PRODUCTION THEN SHOW THAT YEAR
!   IF THERE IS NO OIL PRODUCTION THEN DON'T SHOW
!

!       PRINT*, aresid(ires),nyrsi,'  a'



       DO IC=max_yr-1,1,-1
!-OLD         IF (OILPROD(IC).GT.0.0) THEN
         IF ((OILPROD(IC)+GASPROD(IC)).GT.0.0) THEN
            NYRSI = MIN(IC,NYRSI)
            IF (NYRSI.LT.max_yr-1) THEN
!-OLD               IF (OILPROD(NYRSI+1).GT.0.0) THEN
               IF ((OILPROD(NYRSI+1)+gasPROD(NYRSI+1)).GT.0.0) THEN
                  NYRSI = NYRSI +1
               END IF
            END IF
            GOTO 652
         END IF
       END DO
652    CONTINUE

!       PRINT*, aresid(ires),nyrsi,'  b'
!       pause


!
! IF A DECLINE RESERVOIR NEVER PRODUCES THEN SET ALL WELLS TO SHUTIN
! AND REPORT ZEROS FOR THE ECONOMICS
!
      IF(PROCDE.EQ.0)THEN
         IOILCOUNT = 0
         DO IC=1,max_yr-1
          IF (OILPROD(IC)+GASPROD(IC).LE.0.0) THEN
            IOILCOUNT=IOILCOUNT+1
          END IF
         END DO
         IF (IOILCOUNT.GE.MAX_yr-1) THEN
          GOTO 697
         ENDIF
      END IF
      IF(PROCDE.EQ.11.OR.PROCDE.EQ.13.OR.PROCDE.EQ.12.OR. &
         PROCDE.EQ.15.OR.PROCDE.EQ.14)THEN
         IOILCOUNT = 0
         DO IC=1,max_yr-1
          IF (OILPROD(IC)+GASPROD(IC).LE.0.0) THEN
            IOILCOUNT=IOILCOUNT+1
          END IF
         END DO
         IF (IOILCOUNT.GE.max_yr-1) THEN
          GOTO 697
         ENDIF
      END IF
!
! USE ITEND TO REPORT ONLY THE FIRST YEARS OF ECONOMIC LIFE
! DO NOT REPORT THE LAST YEAR OF BEING UNECONOMIC, USE ITEND
! SO AS NOT TO OVER RUN THE ARRAY BOUNDS
!
! DO NOT ALLOW TIGHT/SHALE PROJECTS TO SHUTDOWN EARLY
!       if ((procde.eq.17.or.procde.eq.21.or.procde.eq.22).and.nyrsi.gt.1) then
        if ((procde.eq.13.or.procde.eq.14.or.procde.eq.15.or.procde.eq.21.or.procde.eq.22).and.nyrsi.gt.1) then
          if (OILPROD(nyrsi).GT.1500. .or. GASPROD(nyrsi).GT.5000.) then
            nyrsi = max_yr
          Endif
        Endif
       IF ((STARTPR+NYRSI).GT.max_yr-1) THEN
         ITEND = max_yr-1
       ELSEIF ((STARTPR+NYRSI).EQ.MAX_yr-1) THEN
         ITEND = STARTPR + NYRSI - 1
       ELSE
         ITEND = STARTPR + NYRSI - 1
       END IF
       DO IC=1,NYRSI
           CUMMDATCF = CUMMDATCF + DATCF(IC)
       END DO
!
! HERE WE DO THE SHIFT TO THE ORIGINAL START YEAR
!
       CALL ARRAYSHIFT(2,ITECH,IRES,STARTPR,ITEND)
       IF ((STARTPR+NYRSI).GT.max_yr-1) THEN
          CALL STORE_26(IRES,STARTPR,NYRSI)
       END IF

!       IF(aresid(ires).eq.'DOLA6053904'.or.
!     &  aresid(ires).eq.'DOCA6920204') then
!         WRITE(777,*) 'after arrayshift'
!         do IYR = 1,40
!          WRITE(777,1777) aresid(ires),itimeyr,gasprod(iyr),
!     &     eprodgas(ires,iyr)
!         end do
!         WRITE(777,*)
!       END if

!
! HERE WE RESET THE WELL COUNTS TO THE ARRAYS EXPECTED BY THE TIMING MODEL
!
! USE ITENDA SO IT DOES NOT OVERRUN THE ARRAY, ARRAY IS DIMENSIONED TO 45 YEARS
!
       IF ((STARTPR+NYRSI).GT.max_yr-1) THEN
         ITENDA = max_yr-1
       ELSEIF ((STARTPR+NYRSI).EQ.max_yr-1) THEN
         ITENDA = STARTPR + NYRSI - 1
       ELSE
         ITENDA = STARTPR + NYRSI - 1
       END IF
!
        DO IC=STARTPR,ITENDA
           EPRODWELL(IRES,IC) = EPRODWELLX2(IC-STARTPR+1)
           EPRODDR(IRES,IC)   = EPRODDRX2(IC-STARTPR+1)
           EINJDR(IRES,IC)    = EINJDRX2(IC-STARTPR+1)
           EINJWELL(IRES,IC)  = EINJWELLX2(IC-STARTPR+1)
        END DO
!
        DO IC=STARTPR,ITENDA
            ESHUTIN(IRES,IC) = ESHUTINX2(IC-STARTPR+1)
        END DO

        DO IC=STARTPR,ITENDA
            Edryhole(IRES,IC) = EdryholeX2(IC-STARTPR+1)
        END DO


!
        DO IC = ITENDA,max_yr-1                                        !mc change 5.5.09 39->max_yr-1
            ESHUTIN(IRES,IC+1) = ESHUTIN(IRES,ITENDA) &
                             + EPRODWELL(IRES,ITENDA) &
                             + EINJWELL(IRES,ITENDA)
        END DO
!
697     CONTINUE

!        PRINT*, aresid(ires),ioilcount,startpr,itend,itenda,
!     &   oneyear(ires)
!        do ic = 1,40
!           PRINT*, eprodwell(ires,ic),eproddr(ires,ic),einjdr(ires,ic),
!     *       einjwell(ires,ic),eshutin(ires,ic),ic,'a'
!        end do
!        PRINT*, ""
!        pause


!        IF((PROCDE.EQ.0.OR.PROCDE.EQ.11.OR.PROCDE.EQ.13.OR.                   !MC CHANGE 4.27.09
!     &         PROCDE.EQ.15.OR.PROCDE.EQ.12.OR.                               !MC CHANGE 4.27.09
!     &         PROCDE.EQ.14).AND.IOILCOUNT.EQ.max_yr-1) THEN                  !MC CHANGE 4.27.09
!           DO IC=1,max_yr-1                                                   !MC CHANGE 4.27.09
!             ESHUTIN(IRES,IC)   = 0.0                                         !MC CHANGE 4.27.09
!             ESHUTIN(IRES,IC)   = SUMP(IC)!-XPATN(I)                          !MC CHANGE 4.27.09
!C             EPRODWELL(I) = (XPATN(I))
!             EPRODDR(IRES,IC)   = 0.0                                         !MC CHANGE 4.27.09
!             EINJDR(IRES,IC)    = 0.0                                         !MC CHANGE 4.27.09
!             EINJWELL(IRES,IC)  = 0.0                                         !MC CHANGE 4.27.09
!           END DO                                                             !MC CHANGE 4.27.09
!        END IF                                                                !MC CHANGE 4.27.09
!
897     CONTINUE   !END POINT IF PATTERN or project LIFE IS LE TO 1

!        IF((PROCDE.EQ.0.OR.PROCDE.EQ.11.OR.PROCDE.EQ.13.OR.                   !MC CHANGE 4.27.09
!     &         PROCDE.EQ.15.OR.PROCDE.EQ.12.OR.                               !MC CHANGE 4.27.09
!     &         PROCDE.EQ.14).AND.ONEYEAR(IRES)) THEN                          !MC CHANGE 4.27.09
!           DO IC=1,max_yr-1                                                   !MC CHANGE 4.27.09
!             ESHUTIN(IRES,IC)   = 0.0                                         !MC CHANGE 4.27.09
!             ESHUTIN(IRES,IC)   = SUMP(IC)!-XPATN(I)                          !MC CHANGE 4.27.09
!C             EPRODWELL(I) = (XPATN(I))
!             EPRODDR(IRES,IC)   = 0.0                                         !MC CHANGE 4.27.09
!             EINJDR(IRES,IC)    = 0.0                                         !MC CHANGE 4.27.09
!             EINJWELL(IRES,IC)  = 0.0                                         !MC CHANGE 4.27.09
!           END DO                                                             !MC CHANGE 4.27.09
!        END IF                                                                !MC CHANGE 4.27.09

!        do ic = 1,40
!           PRINT*, eprodwell(ires,ic),eproddr(ires,ic),einjdr(ires,ic),
!     *       einjwell(ires,ic),eshutin(ires,ic),ic,'b'
!        end do
!        PRINT*, ""
!        pause

        EROR(IRES)    = ROR
        EINVEFF(IRES) = EFFINV
        ENPV(IRES)    = CUMMDATCF
        PROJECT_NPV(IRES) = CUMMDATCF
        IF (apcode(ires).ge.16.and.aprov(ires).eq.50) THEN  ! REMOVE NATURAL GAS FIELDS IN FLORIDA
          EROR(IRES)    = -1.
          EINVEFF(IRES) = -1.
          ENPV(IRES)    = -1.
          PROJECT_NPV(IRES) = -1.
        ENDIF
!
!       DETRMINE WHICH RANKING CRITERIA WILL BE USED AND SAVE APPROPRIATE VALUE
!
!        PRINT*, ires, aresid(ires),ror,effinv,cummdatcf,rnkval
        IF (RNKVAL.EQ.'NPV') THEN
           RANKING_VAL(IRES) = CUMMDATCF
        ELSEIF (RNKVAL.EQ.'ROR') THEN
           RANKING_VAL(IRES) = ROR
        ELSEIF (RNKVAL.EQ.'INV') THEN
           RANKING_VAL(IRES) = EFFINV
        ELSE
           PRINT*,'INVALID VALUE FOR RANKING CRITERIA'
           PRINT*, RNKVAL,' RANKING VALUE - ECONOMICS'
           STOP
        END IF

!        pause

!  Write the proforma
!!!       IF(opt_dbg2) &
!         IF(aresid(ires).eq."COTX4403817") &
!          call write_proforma(ires,NYRSI,819,EFFINV,CUMMDATCF,ROR)

!      if((procde.eq.21.or.procde.eq.20.or.((procde.eq.14.or.procde.eq.15).and.aresflag(ires).ge.3)).and.  &
!         cmode.eq.0) then
!         call write_dcfsum(ires,NYRSI,ogbug1,EFFINV,CUMMDATCF,ROR,itimeyr)                 !mc change ver 7
!      end if
       if((procde.ge.11.and.procde.ne.17).and.cmode.eq.0) then
          call write_dcfsum(ires,NYRSI,ogbug1,EFFINV,CUMMDATCF,ROR,itimeyr)                 !mc change ver 7
       end if

       END SUBROUTINE
!***************************************************************
!from patt_dev.FOR
!     Last change:  MC   27 Apr 2009    4:00 pm
       subroutine patt_development(ires,iyr2,io)
       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmbfw'
       include 'ogsmugr'
       include 'ogsml48'
       include 'ogsmout'
       include 'ngtdmrep'
       include 'pmmout'
       include 'lfmmout'
       include 'intout'


       INTEGER ires,iyr2,iyr,io

       REAL DUM_DEV     ! DEVELOPMENT FRACTION FROM PATT_DEV.DAT
       REAL DUM_DEV0    ! DEVELOPMENT FRACTION FROM PATT_DEV.DAT
       REAL DUM_DEV_MAX ! MAXIMUM NUMBER OF WELLS THAT CAN BE DEVELOPED IN A YEAR
       REAL DUM_DEV_MIN ! MINIMUM NUMBER OF WELLS THAT CAN BE DEVELOPED IN A YEAR
       REAL DUM_AVAIL   ! NUMBER OF AVAILABLE PATTERNS
       REAL DUM_AVAIL0  ! NUMBER OF AVAILABLE PATTERNS
       REAL A, A0
       REAL B
       REAL C
       REAL TECH,MINSIZE
       REAL PATT(100)
       REAL PRCADJ, MAX_AVAIL, WLSDRILLED, tGOR, GASADJ
       CHARACTER*1 DEV_ID
       INTEGER IR, II
       INTEGER RAMPYRS, COUNT, DRL_COMPLETE 
       REAL TEST1, TEST2
       REAL OPRCGR,xOWHP
!
! STORE THE PERCENT DEVELOPMENT FOR THIS RESERVIOR
!
       DEV_ID = ''
       READ(aresid(ires)(1:1),'(a1)') dev_id
       IR = AREGION(ires)
       DUM_AVAIL = 0.0
       DUM_DEV   = 0.0
       DUM_DEV_MAX = 0.0
       DUM_DEV_MIN = 0.0
!
       if (apcode(ires).eq.0) then
       DUM_DEV     = PATT_DEV(1) / 100.0
       DUM_DEV_MAX = PATT_DEV_MAX(1)
       DUM_DEV_MIN = PATT_DEV_MIN(1)
       else
       DUM_DEV     = PATT_DEV(APCODE(IRES)+1) / 100.0
       DUM_DEV_MAX = PATT_DEV_MAX(APCODE(IRES)+1)
       DUM_DEV_MIN = PATT_DEV_MIN(APCODE(IRES)+1)
       end if

! Determine maximum wells based on minimum cell size
       wlsdrilled = atotacres(ires)/apatsiz(ires)-atotpat(ires,1)  ! wells already drilled
       minsize = max(alatlen(ires,1)/3000.*40.,80.)
       max_avail = max(atotacres(ires)/minsize - wlsdrilled,0.)
!      if(apatsiz(ires).gt.minsize) max_avail = max(atotacres(ires)/minsize - wlsdrilled,0.)
!      if(apatsiz(ires).le.minsize) max_avail = atotpat(ires,1)
       if(atotpat(ires,1).lt.1.) max_avail = 0.

       IF ((ATOTPAT(IRES,1).GT.0.0).AND.(ATOTPAT(IRES,1).LT.10.0)) THEN
          ATOTPAT(IRES,1) = 0.0
       END IF
       DUM_AVAIL   = NINT(ATOTPAT(IRES,1))
       DUM_AVAIL0  = NINT(ATOTPAT(IRES,1))
       if(apcode(ires) == 17 .or. apcode(ires) == 21 .or. apcode(ires) == 22) then
         DUM_AVAIL   = NINT(MAX_AVAIL)
         DUM_AVAIL0  = NINT(MAX_AVAIL)
       endif
       dum_dev0 = dum_dev
!      basegas = ogwprng(IR,l48hyr)*dladj

       RAMPYRS = 5

       !OIL DUM_DEV_MAX
       if (apcode(ires).eq.21.or.apcode(ires).eq.22.or.(apcode(ires).eq.17.and.aresflag(ires).eq.9)) then
         dum_dev_max = atotacres(ires)/640*0.08
         IF (itimeyr.eq.1.and.dum_avail0.gt.0.) then
           if (anwellinj(ires).gt.0.) then
             dum_dev = max(dum_dev0,dum_dev,dum_dev_min/dum_avail0)
             IF (apcode(ires).eq.21) dum_dev = max(dum_dev,ANWELLINJ(ires)/dum_avail0)
             IF (apcode(ires).eq.22) dum_dev = max(dum_dev,ANWELLINJ(ires)/dum_avail0)
             IF (apcode(ires).eq.17.and.aresflag(ires).eq.9) dum_dev = max(dum_dev,ANWELLINJ(ires)/dum_avail0)
             dum_dev_max = max(dum_dev_max,ANWELLINJ(ires)*1.3)
             if (apcode(ires).eq.17.and.(wwop.eq.1)) dum_dev_max = dum_dev_max    ! remove control in low oil price case
             if(dum_dev.eq.anwellinj(ires)/dum_avail0) rampyrs = 1
             if(dum_dev.gt.0.3) dum_dev = 0.3
           ENDIF
         ENDIF
         if (itimeyr.eq.1) write(ogbug1,*) 'dh5drl',curiyr+1989,aplay_cde(ires),aresid(ires),dum_dev,dum_dev_max,anwellinj(ires)
       endif

       !GAS DUM_DEV_MAX
       if (apcode(ires).eq.21.or.apcode(ires).eq.22) then
         dum_dev_max = atotacres(ires)/640*0.09
         IF (itimeyr.eq.1.and.dum_avail0.gt.0.) then
           if (anwellinj(ires).gt.0.) then
             dum_dev = max(dum_dev0,dum_dev,dum_dev_min/dum_avail0)
             IF (apcode(ires).eq.21) dum_dev = max(dum_dev,ANWELLINJ(ires)/dum_avail0)
             IF (apcode(ires).eq.22) dum_dev = max(dum_dev,ANWELLINJ(ires)/dum_avail0)
             IF (apcode(ires).eq.17.and.aresflag(ires).eq.9) dum_dev = max(dum_dev,ANWELLINJ(ires)/dum_avail0)
             dum_dev_max = max(dum_dev_max,ANWELLINJ(ires)*1.3)
             if (apcode(ires).eq.17.and.(wwop.eq.1)) dum_dev_max = dum_dev_max    ! remove control in low oil price case
             if(dum_dev.eq.anwellinj(ires)/dum_avail0) rampyrs = 1
             if(dum_dev.gt.0.3) dum_dev = 0.3
           ENDIF
         ENDIF
         if (itimeyr.eq.1) write(ogbug1,*) 'dh5drl',curiyr+1989,aplay_cde(ires),aresid(ires),dum_dev,dum_dev_max,anwellinj(ires)
       endif
    
       DO I=1,100
          PATT(I) = 0
       END DO
!
!    PATTERN DEVELOPMENT SCHEDULE
!
       A0 = INT(DUM_AVAIL0*DUM_DEV)     ! FRACTION OF WELLS TO DEVELOP
       oprcgr = (START_PRICE(MNUMYR)/START_PRICE(curiyr-1))**(1./(mnumyr-(curiyr-1)+1))
       if(ogrunop(2).eq.30) oprcgr = ((START_PRICE(MNUMYR)*0.85)/START_PRICE(curiyr-1))**(1./(mnumyr-(curiyr-1)+1))   !HIGH OGS case
       if(ogrunop(2).eq.23) oprcgr = ((START_PRICE(MNUMYR)*1.15)/START_PRICE(curiyr-1))**(1./(mnumyr-(curiyr-1)+1))   !LOW OGS case
!      write(6,*) 'dh5oil',curiyr+1989,aplay_cde(ires),oprcgr
       count = 0
       DO I=IYR2,max_yr-1

         II = curiyr+i-1
         if (ii.gt.mnumyr) ii = mnumyr

!
! FIND THE MAXIMUM NUMBER OF PATTERNS TO DEVELOP IN A YEAR
!
          IF (DUM_AVAIL.LE.DUM_DEV_MIN) THEN
             PATT(I) = DUM_AVAIL
             GOTO 100
          ELSE

         test1=0.0
         test2=0.0
         if (dum_avail0 .ne. 0.0) test1 = dum_avail/dum_avail0
         if (alatlen(ires,1) .ne. 0.0) test2 = FLOAT(NINT(atotacres(ires)/(alatlen(ires,1)*870./43560.)))*0.5
         if (I.LE.rampyrs) THEN
           A = INT(A0*(i*1.0/rampyrs))
         elseif ((test1 .gt. 0.3 .and. dum_dev .lt. 0.3) .and. &
                 (sum(patt(iyr2:i))+wlsdrilled .lt. test2)) then
           A = A0
           count = count + 1
         else
           A = INT(A0*(1.0-dum_dev*1.)**(i-count))
         endif
!        if(aplay_cde(ires).eq.4774) write(6,*) 'dh5out',curiyr+1989,aresid(ires),i,a,dum_avail


             B = MIN(A,DUM_DEV_MAX)    ! MAX THAT CAN BE DEVELOPED
             C = MAX(B,DUM_DEV_MIN)    ! NOT LESS THAN THE MINIMUM

             IF(aresacc(ires).le.3) c = 0.                             !no access
             IF(aresacc(ires).ge.5.and.aresacc(ires).le.7) &
                 C=INT(0.5*C)                                          !if the reservoir is development constrained,
                                                                       !develop only half of the 'normal' number of patterns

! adjust drilling based on prices
             prcadj = 1.0
             tGOR = 20000.
             if(aprodoil(ires,1,1) > 0.) tGOR = aprodgas(ires,1,1)*5.6*1000./aprodoil(ires,1,1)
             IF (OGRUNOP(1).NE.5) THEN
               if ((apcode(ires).ge.11.and.apcode(ires).le.15).or.  &
                   (apcode(ires).ge.21)) then
                   prcadj = ((ogwprng(IR,curiyr-1)*dladj/basegas))
                   if ((apcode(ires).eq.21.or.apcode(ires).eq.22).and.aprodgas(ires,1,1).gt.0..and.adepth(ires).lt.10000.) then
!                  if ((apcode(ires).eq.21.or.apcode(ires).eq.22).and.aprodgas(ires,1,1).gt.0.) then
                     if (prcadj.lt.1.) then
                       prcadj = min(prcadj**(1/(aprodgas(ires,1,1)/1000.)),0.98)
                     else 
                       prcadj = max(prcadj**((aprodgas(ires,1,1)/1000.)),1.02)
                     endif
                   else
                     prcadj = prcadj**0.5
                   endif
!            if (i.eq.1) write(6,*) 'dh5gas',curiyr+1989,aresid(ires),prcadj,ogwprng(IR,curiyr-1)*dladj
               else
                  gasadj = ((ogwprng(IR,curiyr-1)*dladj/basegas))
                  xOWHP = ANINT(RFCRUDEWHP(apadd(ires),acrdtype(ires),curiyr-1)*(oprcgr)**(i))
                  if(curiyr.le.l48hyr+1) xOWHP = ANINT(RFCRUDEWHP(apadd(ires),acrdtype(ires),l48hyr)*(oprcgr)**(i))
                  if (ii.le.l48hyr+2) xOWHP = ANINT(RFCRUDEWHP(apadd(ires),acrdtype(ires),ii))  ! STEO years
                  prcadj = (xOWHP*dladj/baseoil)
                  if (apcode(ires).eq.17.and.aprodoil(ires,1,1).gt.0.) then   
                    if (prcadj.lt.1.) then   
                      prcadj = min(prcadj**(1/(aprodoil(ires,1,1)/100.)),0.98)
                    else
                      prcadj = max(prcadj**((aprodoil(ires,1,1)/100.)),1.02)
                    endif
                    if (tGOR > 6000. .and. gasadj > 1.) prcadj = prcadj**gasadj
                  else
                     prcadj = prcadj**0.5
                  endif
                  if (ii.le.l48hyr+4) then            ! reduce response to increasing prices during COVID-19 recovery
                    if (prcadj > 1.) prcadj = prcadj**0.25  
                    if (prcadj < 1.) prcadj = prcadj**1.75  
                  endif
                  if (wwop.eq.1.and.(ii.gt.l48hyr+4)) then            ! keep responsiveness low in the low oil price case
                    if (prcadj < 1.) prcadj = prcadj**1.25  
                  endif
!            if (i.eq.IYR2) write(6,*) 'dh5oil',curiyr+1989,aplay_cde(ires),apadd(ires),acrdtype(ires)
!            write(6,*) 'dh5oil',ii+1989,i,xOWHP,rfcrudewhp(apadd(ires),acrdtype(ires),curiyr-1),rfcrudewhp(apadd(ires),acrdtype(ires),ii)
               endif
             ENDIF
             if(prcadj.gt.2.) prcadj = 2.0
             C = INT(MAX(C*prcadj,DUM_DEV_MIN))

! SET DRILLING TO HISTORICAL LEVELS AND BENCHMARK TO STEO YEARS
             if (itimeyr+i-1.eq.1) then
               if (apcode(ires).eq.21) c = ANWELLINJ(ires)
               if (apcode(ires).eq.22) c = ANWELLINJ(ires)
               if (apcode(ires).eq.17.and.aresflag(ires).eq.9) c = ANWELLINJ(ires)
             endif
             !if (itimeyr+i-1.eq.2) then   ! benchmark to STEO
             !    if (apcode(ires).eq.17.and.aresflag(ires).eq.9.and.ANWELLINJ(ires).gt.0.) c = min(c,ANWELLINJ(ires)*0.5)
             !    if (apcode(ires).eq.21.and.adepth(ires).ge.10000.) c = c*0.7
             !endif
             !if (itimeyr+i-1.eq.3) then   ! benchmark to STEO
             !    if (apcode(ires).eq.17.and.aresflag(ires).eq.9.and.ANWELLINJ(ires).gt.0.) c = min(c,ANWELLINJ(ires)*0.80)
             !    if (wwop.eq.1.and.apcode(ires).eq.17.and.aresflag(ires).eq.9.and.ANWELLINJ(ires).gt.0.) c = min(c,ANWELLINJ(ires)*0.25)
             !    if (wwop.eq.3.and.apcode(ires).eq.17.and.aresflag(ires).eq.9.and.ANWELLINJ(ires).gt.0.) c = min(c,ANWELLINJ(ires)*0.90)
             !endif
             !if (itimeyr+i-1.eq.3.or.itimeyr+i-1.eq.4) then   ! benchmark to expectation of Bakken production from Lynn Helms (11/10/20 conversation)
             !    if (aplay_cde(ires).ge.3110.and.aplay_cde(ires).le.3115) c = c*0.75
             !endif
             !if (itimeyr+i-1.eq.3.or.itimeyr+i-1.eq.4) then   ! increase drilling response in the Wolfcamp
             !    if (aplay_cde(ires).eq.4401) c = c*1.10
             !endif
             !if (itimeyr+i-1.eq.4.or.itimeyr+i-1.eq.5) then   ! slow responsiveness of drilling in deep formations as a result of very high prices in the STEO years
             !    if (apcode(ires).eq.21.and.adepth(ires).ge.10000.) c = c*0.90
             !endif

             if (c.gt.dum_avail) c = dum_avail
             if (aresid(ires).eq.'DOTX6041303'.or.aresid(ires).eq.'DOTX6064503') co2lag = 0
             if (apcode(ires).eq.3.and.i.le.co2lag) c = 0

             if (ogrunop(2).eq.13) then                   ! ELIMINATING ALL UGR DRILLING AFTER 2012 - HYDRAULIC FRACTURING CASE
               if (ii.ge.2012-baseyr+1) then         
                 if (apcode(ires).ge.16.and.apcode(ires).ne.17) then
                   c=0
                 endif
               endif
             endif
             if (ogrunop(2).eq.14) then                   ! ELIMINATING ALL TIGHT AND SHALE DRILLING AFTER 2012 
               if (ii.ge.2012-baseyr+1) then         
                 if (apcode(ires).eq.18.or.apcode(ires).eq.22.or.   &
                    apcode(ires).eq.20.or.apcode(ires).eq.21) then
                   c=0
                 endif
               endif
             endif
             if (ogrunop(2).eq.17) then                   ! ELIMINATING SHALE DRILLING AFTER 2012 
               if (ii.ge.2012-baseyr+1) then         
                 if (apcode(ires).eq.20.or.apcode(ires).eq.21) then
                   c=0
                 endif
               endif
             endif

             if(I.gt.rampyrs) then
		if(patt(i-1).gt.0.and.c.gt.patt(i-1)*2.) c=patt(i-1)*2.
             endif
             PATT(I) = int(C)
!            MAX_AVAIL = MAX_AVAIL - C
             DUM_AVAIL = DUM_AVAIL - C
!            IF(apcode(ires).eq.21) dum_avail = NINT(min(max_avail,dum_avail*(1.0+togtechon(5,5)*techadj_on(1,1))))
!            IF(apcode(ires).eq.22) dum_avail = NINT(min(max_avail,dum_avail*(1.0+togtechon(5,4)*techadj_on(1,1))))
!            IF(apcode(ires).eq.17) dum_avail = NINT(min(max_avail,dum_avail*(1.0+togtechon(5,2)*techadj_on(1,1))))
!            if(apcode(ires).eq.17) write(6,*) 'dh5out',curiyr+1989,i,aresid(ires),max_avail,dum_avail
!           if(aresid(ires).eq."EGLA4714521") write(6,*) 'dh5gas1',curiyr+1989,i,prcadj,ogwprng(IR,curiyr-1)*dladj,c
!           if(aresid(ires).eq."EGLA4714521") write(6,*) 'dh5gas2',curiyr+1989,i,patt(i),sum(patt(1:I))
!           if(aresid(ires).eq."EGLA4714521") write(6,*) 'dh5gas3',curiyr+1989,i,atotacres(ires),dum_avail0,atotacres(ires)/(dum_avail0-dum_avail)

          END IF
       END DO


100    CONTINUE
!       pause
!     DO I=IYR2,IYR2+39
      DO I=IYR2,max_yr-1
        if(frac(ires)) advanced(i) = hfp_app                 !mc change B - overwrite techpen
        IF (I.LE.MAX_YR) TECH=ADVANCED(I)
        IF (I.GT.MAX_YR) TECH=ADVANCED(MAX_YR)
        PATDEV(IRES,I,2)=TECH*PATT(I)
        PATDEV(IRES,I,1)=PATT(I)-PATDEV(IRES,I,2)
        IF (PATDEV(IRES,I,1).LT.0.0) PATDEV(IRES,I,1) = 0.0
        IF (PATDEV(IRES,I,2).LT.0.0) PATDEV(IRES,I,2) = 0.0
      END DO


! MC CHANGE 4.27.09 - START - OVERWRITE OF DECLINE CURVE DRILLING PATTERNS
!  REDRILLING OF EXISTING DECLINE WELLS
       if (apcode(ires).EQ.0) THEN
         iyr = 1
         do i = iyr2,max_yr-1
           II = curiyr+i-1
           if (ii.gt.lastyr) ii = lastyr
           if(II.gt.l48hyr+1) ainjinj(ires,iyr,1) = ainjinj(ires,iyr,1)*1.03
                   prcadj = (dcrdwhp(IR,ii)/dcrdwhp(IR,l48b4yr-baseyr+1))
                   if (prcadj.lt.1.0) then
                     prcadj = prcadj**0.25
                   else
                     prcadj = prcadj**0.25
                   endif
                   prcadj = 1.0
           ainjinj(ires,iyr,1) = ainjinj(ires,iyr,1)*prcadj
           patt(i)          = ainjinj(ires,iyr,1)
           PATDEV(ires,i,1) = AINJINJ(IRES,IYR,1)
           patdev(ires,i,2) = ainjinj(ires,iyr,1)-patdev(ires,i,1)
           iyr = iyr+1
         end do
       ELSEIF(APCODE(IRES).GE.11.AND.APCODE(IRES).LE.15) THEN
         iyr = 1
         do i = iyr2,max_yr-1
           II = curiyr+i-1
           if (ii.gt.lastyr) ii = lastyr
           if(II.ge.l48hyr+1) ainjinj(ires,iyr,1) = ainjinj(ires,iyr,1)*1.1
                   prcadj = (ogwprng(IR,ii)/ogwprng(IR,l48b4yr-baseyr+1))
                   if (prcadj.lt.1.) then
                     prcadj = prcadj**0.25
                   else
                     prcadj = prcadj**0.25
                   endif
                   prcadj = 0.0
           ainjinj(ires,iyr,1) = ainjinj(ires,iyr,1)*prcadj
           patt(i)          = ainjinj(ires,iyr,1)
           PATDEV(ires,i,1) = AINJINJ(IRES,IYR,1)
           patdev(ires,i,2) = ainjinj(ires,iyr,1)-patdev(ires,i,1)
           iyr = iyr+1
         end do
       end if
! MC CHANGE 4.27.09 - END - OVERWRITE OF DECLINE CURVE DRILLING PATTERNS




!        write (io,*)
!        write (io,*) aresid(ires),iyr2
!        write (io,1) 'patterns    ',(patt(iyr),iyr=1,max_yr-1)
!        write (io,1) 'advanced    ',(advanced(iyr),iyr=1,max_yr-1)
!        write (io,1) 'base pattern',(patdev(ires,i,1),i=1,max_yr-1)
!        write (io,1) 'adv pattern',(patdev(ires,i,2),i=1,max_yr-1)

!  1     format (a20,(3x,f12.2))

       end subroutine

!***************************************************************
!from econ_array.FOR
!     Last change:  MC    4 May 2009    2:39 pm
       subroutine arrayshift(SHIFT,ITCH,IRS,STRT,IEND)
!        FIRST CALL   CALL ARRAY_SHIFT(1,ITECH,IRES,      1 ,   1)
!        SECOND       CALL ARRAY_SHIFT(2,ITECH,IRES,STARTPR,ITEND)
!
! THIS SUBROUTINE WILL BE USED TO SHIFT THE ARRAYS FROM
! TIMING YEAR, AND TIMING VARIABLES TO AND FROM THE ARRAYS
! USED BY THE ECONOMICS ROUTINE
!

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmbfw'
       INCLUDE 'ogsmugr'
       include 'ogsml48'
!
! LOCAL VARIABLES - CONTROLS
!
       INTEGER SHIFT
       INTEGER ITCH
       INTEGER IRS
       INTEGER STRT
       INTEGER IEND
       INTEGER IC, II, IT, AC
       INTEGER INFILL, IY,icde
       integer imc
       REAL T1
       REAL FEDROYALTY(MAX_YR)
       REAL TRESOIL,TRESGAS                                                ! MC CHANGE 6.23.09
       real tresprod                                                        ! mc change ver 2
       real tresvwello,tresvwellg                                          ! mc change ver 2
       real oprod0(max_yr),gprod0(max_yr),nprod0(max_yr),wprod0(max_yr)
       integer igen(MAX_YR)
       LOGICAL ONCE
       common/newcash/igen
       COMMON/AGG/FEDROYALTY

!
! IF THE ARRAYS ARE COMING INTO THE ECONOMICS
!
        IF (SHIFT.EQ.1) THEN
!
! INITIALIZE THE VARIABLES COMING IN
!
            CALL INITINP()
!
            RESID     = ARESID(IRS)
            READ(resid(10:11),'(i2)') apcode(irs)
            PROCDE    = APCODE(IRS)
!            PRINT*, procde
!            pause
            if (procde.le.10.or.procde.eq.17) then    !use process code to determine the resource type
               resource = 'O'
            else
               resource = 'G'
            end if
!  GET THE STATE CODE

        state = ''
        READ(aresid(irs)(3:4),'(a2)') state
        astate(irs) = state

        call state_match(state,ist)
        stnum = ist

        apresin(irs) = 15+(0.433*adepth(irs))
        T1 = 0.0
        if (state.eq.'CA') then
          T1 = 0.01
        ELSEIF(STATE.EQ.'KY'.OR.STATE.EQ.'OK'.OR.STATE.EQ.'UT'.OR. &
         STATE.EQ.'WV') THEN
          T1 = 0.014
        ELSEIF(STATE.EQ.'MT'.OR.STATE.EQ.'PA'.OR.STATE.EQ.'WY') THEN
          T1 = 0.016
        ELSEIF(STATE.EQ.'CO'.OR.STATE.EQ.'KS'.OR.STATE.EQ.'NE'.OR. &
         STATE.EQ.'SD'.OR.STATE.EQ.'IL') THEN
          T1 = 0.018
        ELSE
          T1 = 0.017
        end if
        atemp(irs) = 60+(T1*adepth(irs))

!        PRINT*, 'aray',aresid(irs),state,T1,adepth(irs),
!     &   atemp(irs)
!        pause

!cccc            RESOURCE  = ARES_TYPE(IRS)     ! RESOURCE TYPE (O)IL/(G)AS

            ECON_REGION= AREGION(IRS)
            iregion   = aregion(irs)
            play      = aplay_cde(irs)
            resflag   = aresflag(irs)
            RRC       = ARRC(IRS)
            SOI       = ASOI(IRS)
            gas_grav  = agas_grav(irs)
            btu       = abtu(irs)
            nwelloil  = anwelloil(irs)
            nwellgas  = anwellgas(irs)
            nwellinj  = anwellinj(irs)

!???????????????????????????????????????????????
!            if (resource.eq.'O') then
!               atotprod(irs,itch) = anwelloil(irs)  !for oil processes, use number of oil wells
!            else
!               atotprod(irs,itch) = anwellgas(irs)  !for gas processes, use number of gas wells
!            end if
!            atotprod(irs,itch) = 1.0
!???????????????????????????????????????????????

            presin    = apresin(irs)
!            totpat    = atotprod(irs,itch)

            totprod   = atotprod(irs,itch)
            tempr      = atemp(irs)
            heatval   = aheatval(irs)
            phi       = aphi(irs)
            soc       = asoc(irs)
            wor       = awor(irs)
            boi       = aboi(irs)
            perm      = aperm(irs)
            oilvis    = aoilvis(irs)
            vdp       = avdp(irs)
            sulfoil   = asulfoil(irs)
            orgooip   = aooip(irs)
            choilprod = achoilprod(irs)
            chgasprod = achgasprod(irs)

            SOR       = ASOR(IRS)
            PAY       = APAY(IRS)
            DEPTH     = ADEPTH(IRS)
            API       = AAPI(IRS)
            GORL48    = AGOR(IRS)
            POR       = APHI(IRS)
            AREA      = ATOTACRES(IRS)
            PATSZE    = APATSIZ(IRS)
            TOTPAT    = ATOTPAT(IRS,ITCH)
            AC        = APCODE(IRS)

            lyroil    = alyroil(irs)
            lyrgas    = alyrgas(irs)

            IF(apcode(irs).eq.0.or.apcode(irs).eq.17) totpat =            & !oil decline - existing wells
             anwelloil(irs)
            IF(apcode(irs).ge.11.and.apcode(irs).ne.16) totpat            & !gas decline - existing wells
             = anwellgas(irs)

            WPP1      = ATOTPROD(IRS,ITCH)
            WPP2      = ATOTINJ(IRS,ITCH)
            WPP3      = ATOTCONV(IRS,ITCH)
            WPP4      = ATOTPS(IRS,ITCH)
            NLAT      = ALATNUM(IRS,ITCH)
            LATLEN    = ALATLEN(IRS,ITCH)
            XOOIP     = AOOIP(IRS)
            FRAC_CO2  = ACO2CONT(IRS)
            FRAC_N2   = AN2CONT(IRS)
            FRAC_H2S  = AH2SCONT(IRS)
!            FRAC_CO2 = FRAC_CO2/100.0  ! CONVERT TO FRACTION
!            FRAC_N2  = FRAC_N2 /100.0  ! CONVERT TO FRACTION
!            FRAC_H2S = FRAC_H2S/100.0  ! CONVERT TO FRACTION
            FRAC_NGL  = ANGL(IRS)
            ONCE = .FALSE.
            DO IYR=1,MAX_YR
              OPROD(IYR) = APRODOIL(IRS,IYR,ITCH)
              GPROD(IYR) = APRODGAS(IRS,IYR,ITCH)
              NPROD(IYR) = APRODNGL(IRS,IYR,ITCH)
              WPROD(IYR) = APRODWAT(IRS,IYR,ITCH)
              OINJ(IYR)  = AINJINJ(IRS,IYR,ITCH)
              WINJ(IYR)  = AWATINJ(IRS,IYR,ITCH)
              ORECY(IYR) = AINJRECY(IRS,IYR,ITCH)
             ! DO NOT CHANGE EUR IN START YEAR FOR HIGH/LOW SHALE/TIGHT CASES
             II = curiyr+iyr-1
             if ((ii.eq.l48hyr)) then
               if (apcode(irs).eq.21) then
                 if (ogrunop(2).eq.18.or.ogrunop(2).eq.20) then
                   OPROD(IYR) = APRODOIL(IRS,IYR,ITCH)/(1+ogrunop(11)/100.)
                   GPROD(IYR) = APRODGAS(IRS,IYR,ITCH)/(1+ogrunop(11)/100.)
                   NPROD(IYR) = APRODNGL(IRS,IYR,ITCH)/(1+ogrunop(11)/100.)
                   WPROD(IYR) = APRODWAT(IRS,IYR,ITCH)/(1+ogrunop(11)/100.)
                 endif
                 if (ogrunop(2).eq.19.or.ogrunop(2).eq.21) then
                   OPROD(IYR) = APRODOIL(IRS,IYR,ITCH)/(1-ogrunop(11)/100.)
                   GPROD(IYR) = APRODGAS(IRS,IYR,ITCH)/(1-ogrunop(11)/100.)
                   NPROD(IYR) = APRODNGL(IRS,IYR,ITCH)/(1-ogrunop(11)/100.)
                   WPROD(IYR) = APRODWAT(IRS,IYR,ITCH)/(1-ogrunop(11)/100.)
                 endif
               endif
               if (apcode(irs).eq.17) then
                 if (ogrunop(2).eq.20) then
                   OPROD(IYR) = APRODOIL(IRS,IYR,ITCH)/(1+ogrunop(11)/100.)
                   GPROD(IYR) = APRODGAS(IRS,IYR,ITCH)/(1+ogrunop(11)/100.)
                   NPROD(IYR) = APRODNGL(IRS,IYR,ITCH)/(1+ogrunop(11)/100.)
                   WPROD(IYR) = APRODWAT(IRS,IYR,ITCH)/(1+ogrunop(11)/100.)
                 endif
                 if (ogrunop(2).eq.21) then
                   OPROD(IYR) = APRODOIL(IRS,IYR,ITCH)/(1-ogrunop(11)/100.)
                   GPROD(IYR) = APRODGAS(IRS,IYR,ITCH)/(1-ogrunop(11)/100.)
                   NPROD(IYR) = APRODNGL(IRS,IYR,ITCH)/(1-ogrunop(11)/100.)
                   WPROD(IYR) = APRODWAT(IRS,IYR,ITCH)/(1-ogrunop(11)/100.)
                 endif
               endif
               IF (apcode(irs).eq.17.or.apcode(irs).eq.18.or.apcode(irs).eq.20.or.  &
                   apcode(irs).eq.21.or.apcode(irs).eq.22) then
                 IF (ogrunop(2).eq.22.or.ogrunop(2).eq.30) then   ! high trr
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1+ogrunop(13)/100.)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1+ogrunop(13)/100.)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1+ogrunop(13)/100.)
                 ENDIF
                 IF (ogrunop(2).eq.23) then   ! low EUR
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1-ogrunop(13)/100.)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1-ogrunop(13)/100.)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1-ogrunop(13)/100.)
                 ENDIF
                 IF (ogrunop(2).eq.24) then   ! high EUR
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1+ogrunop(11)/100.)
                 ENDIF
               ENDIF
               IF (apcode(irs).ge.16.or.(apcode(irs).ge.2.and.apcode(irs).le.10)) then
                 IF (ogrunop(2).eq.28) then   ! high trr
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1+ogrunop(11)/100.)
                 ENDIF
               ENDIF
               IF (ogrunop(2).eq.29) then   ! high trr
                 IF (apcode(irs).eq.17.or.apcode(irs).eq.18.or.apcode(irs).eq.20.or.  &
                     apcode(irs).eq.21.or.apcode(irs).eq.22) then
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1+ogrunop(11)/100.)
                 ENDIF
                 IF(apcode(irs).eq.3) then   
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1+ogrunop(11)/100.*0.5)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1+ogrunop(11)/100.*0.5)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1+ogrunop(11)/100.*0.5)
                 ENDIF
               endif
             endif
             ! DO NOT CHANGE EUR IN STEO YEAR FOR HIGH/LOW SHALE/TIGHT CASES
             if ((ii.eq.l48hyr+1)) then
               IF (apcode(irs).eq.17.or.apcode(irs).eq.18.or.apcode(irs).eq.20.or.  &
                   apcode(irs).eq.21.or.apcode(irs).eq.22) then
                 IF (ogrunop(2).eq.22.or.ogrunop(2).eq.30) then   ! high trr
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1+ogrunop(13)/100.)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1+ogrunop(13)/100.)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1+ogrunop(13)/100.)
                 ENDIF
                 IF (ogrunop(2).eq.23) then   ! low EUR
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1-ogrunop(13)/100.)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1-ogrunop(13)/100.)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1-ogrunop(13)/100.)
                 ENDIF
                 IF (ogrunop(2).eq.24) then   ! high EUR
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1+ogrunop(11)/100.)
                 ENDIF
               ENDIF
               IF (apcode(irs).eq.16.or.apcode(irs).ge.18.or.(apcode(irs).ge.2.and.apcode(irs).le.10)) then
                 IF (ogrunop(2).eq.28) then   ! high trr
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1+ogrunop(11)/100.)
                 ENDIF
               ENDIF
               IF (ogrunop(2).eq.29) then   ! high trr
                 IF (apcode(irs).eq.17.or.apcode(irs).eq.18.or.apcode(irs).eq.20.or.  &
                     apcode(irs).eq.21.or.apcode(irs).eq.22) then
                     oprod(iyr) = aprodoil(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     gprod(iyr) = aprodgas(irs,iyr,itch)/(1+ogrunop(11)/100.)
                     nprod(iyr) = aprodngl(irs,iyr,itch)/(1+ogrunop(11)/100.)
                 ENDIF
               ENDIF
             endif
              PATN1(IYR) = PATDEV(IRS,IYR,ITCH)
              IF (RESOURCE.EQ.'G') THEN
                 API = 40.0
              ENDIF
            END DO


        ELSEIF (SHIFT.EQ.2) THEN
!
! SECTION FOR PUTTING THE VALUES INTO THE ARRAYS NEEDED
! FOR THE TIMING MODEL
!
!ccc               if (aresid(irs).eq.'DOAL6003107') then
!ccc                 PRINT*, strt,IEND, 'start and end points'
!ccc                 pause
!ccc               end if
            AECON_LIFE(IRS) = ECON_LIFE

        TRESOIL = 0.0
        TRESGAS = 0.0
        tresproc = 0.0                                              !mc change ver 2
        tresvwello = 0.0                                            !mc change ver 2
        tresvwellg = 0.0                                            !mc change ver 2

        DO IC = STRT,IEND                                           !MC CHANGE 6.23.09
           TRESOIL = TRESOIL + OILPROD(IC-STRT+1)                   !MC CHANGE 6.23.09
           TRESGAS = TRESGAS + GASPROD(IC-STRT+1)                   !MC CHANGE 6.23.09
           tresprod = tresprod + eproddr(irs,ic-strt+1)                 !mc change ver 2
        END DO

        if(tresprod.gt.0.0) tresvwello = tresoil/tresprod
        if(tresprod.gt.0.0) tresvwellg = tresgas/tresprod

!!!        if(apcode(irs).eq.0.or.(apcode(irs).ge.11.and.apcode(irs).le.15)) then
!!!          write (556,1556) aresid(irs),tresoil,tresgas,tresprod,tresvwello,tresvwellg
!!! 1556     format (a11,5(3x,f13.3))
!!!        end if

            DO IC=STRT, IEND
               EPRODOIL(IRS,IC) = OILPROD(IC-STRT+1)
               IF(apcode(irs).ne.3) &
               EPRODGAS(IRS,IC) = GASPROD(IC-STRT+1)
               EPRODWAT(IRS,IC) = WATPROD(IC-STRT+1)
               EWATINJ(IRS,IC)  = WATINJ(IC-STRT+1)
               ECO2INJ(IRS,IC)  = TOTINJ(IC-STRT+1)
               ECO2RCY(IRS,IC)  = TORECY(IC-STRT+1)
               ECO2POL(IRS,IC)  = INJ(IC-STRT+1)
               ESURFVOL(IRS,IC) = TOTINJ(IC-STRT+1)
               EROY(IC)     = ROYALTY(IC-STRT+1)
               EFEDROY(IC)  = FEDROYALTY(IC-STRT+1)
               ESEV(IC)     = SEV_TAX(IC-STRT+1)
!
! DO NOT REPORT NEGATIVE STATE OR FEDERAL TAXES
!
               ESTTAX(IC)   = STATE_TAX(IC-STRT+1)
               EFEDTAX(IC)   = FEDTAX(IC-STRT+1)
               ETAXES(IRS,IC)   = EROY(IC) + EFEDROY(IC) + ESEV(IC) +  &
                                  ESTTAX(IC) + EFEDTAX(IC)
               IF (ESTTAX(IC).LE.0.0) ESTTAX(IC)   = 0.0
               IF (EFEDTAX(IC).LE.0.0) EFEDTAX(IC) = 0.0
               EFEDCR(IC)    = FEDTAX_CR(IC-STRT+1)
               EGROSSREV(IC) = GROSS_REV(IC-STRT+1)
               EGRAVADJ(IC)  = GRAVPEN(IC-STRT+1)
               EGAEXP(IC)    = GA_EXP(IC-STRT+1)
               EGACAP(IC)    = GA_CAP(IC-STRT+1)
               EII(IRS,IC)       = ICST(IC-STRT+1)
               EIIDRL(IRS,IC)    = II_DRL(IC-STRT+1)
               EICAP2(IC)    = ICAP(IC-STRT+1)
               EINTCAP(IC)   = INTCAP(IC-STRT+1)
               ETI(IRS,IC)       = TI(IC-STRT+1)
               ETIDRL(IRS,IC)    = TI_DRL(IC-STRT+1)
               EOTC(IC)      = OTC(IC-STRT+1)
               ETCI(IC)      = TCI(IC-STRT+1)
               EDEPR(IC)     = DEPR(IC-STRT+1)
               EAMOR(IC)     = AMOR(IC-STRT+1)
               ENETREV(IC)   = NET_REV(IC-STRT+1)
               ETOC(IRS,IC)      = TOC(IC-STRT+1)
               EDEPLET(IC)   = DEPLET(IC-STRT+1)
               ETAXINC(IC)   = NIBTA(IC-STRT+1)
               EOAM(IC)      = OAM(IC-STRT+1)
               EAOAM(IC)     = AOAM(IC-STRT+1)
               ELA(IRS,IC)       = LA(IC-STRT+1)*PLAC
               EGG(IC)       = GG(IC-STRT+1)*PGGC
               EEGGLA(IC)    = EGGLA(IC-STRT+1)  !LA(IC)*(1-PLAC)
               EATCF(IC)     = ATCF(IC-STRT+1)
               EDATCF(IC)    = DATCF(IC-STRT+1)

               emarg(irs,ic)     = imarg(ic-strt+1)   !store the years which the well has marginal status

               esump(irs,ic)     = sump(ic-strt+1)
               expatn(irs,ic)    = xpatn(ic-strt+1)
               eremres(irs,ic)   = remres(ic-strt+1)
               egremres(irs,ic)  = gremres(ic-strt+1)
               eiremres(irs,ic)  = iremres(ic-strt+1)         !mc change version 7
               eigremres(irs,ic) = igremres(ic-strt+1)        !mc change version 7

               IF(APCODE(IRS).NE.0.AND.APCODE(IRS).NE.11.AND.APCODE(IRS)  &
               .NE.12.AND.APCODE(IRS).NE.13.AND.APCODE(IRS).NE.14.AND.    &
               APCODE(IRS).NE.15) THEN         !MC CHANGE 6.25.09
                 IF(OIL_SALES(APCODE(IRS)+1)) THEN                     !MC CHANGE 6.25.09 - EOR/ASR & UNDISCOVERED
                    eremres(irs,ic)   = remres(ic-strt+1)                !MC CHANGE 6.25.09
                    eiremres(irs,ic)  = iremres(ic-strt+1)               !MC CHANGE ver7
                 END IF                                                  !MC CHANGE 6.25.09
                                                                         !MC CHANGE 6.25.09
                 IF(GAS_SALES(APCODE(IRS)+1)) THEN                     !MC CHANGE 6.25.09
                    egremres(irs,ic)  = gremres(ic-strt+1)               !MC CHANGE 6.25.09
                    eigremres(irs,ic) = igremres(ic-strt+1)              !MC CHANGE ver7
                 END if                                                  !MC CHANGE 6.25.09
               ELSE                                                      !MC CHANGE 6.25.09
                 IF(OIL_SALES(APCODE(IRS)+1)) THEN                     !MC CHANGE 6.25.09
!                 IF(AECON_LIFE(IRS).GT.0) EREMRES(irs,IC)  = TRESOIL/AECON_LIFE(IRS)
                  IF(AECON_LIFE(IRS).GT.0) EREMRES(irs,IC)  = TRESOIL/40
                 END IF                                                  !MC CHANGE 6.25.09
                                                                         !MC CHANGE 6.25.09
                 IF(GAS_SALES(APCODE(IRS)+1)) THEN                     !MC CHANGE 6.25.09
!                 IF(AECON_LIFE(IRS).GT.0) EGREMRES(irs,IC)  = TRESGAS/AECON_LIFE(IRS)
                  IF(AECON_LIFE(IRS).GT.0) EGREMRES(irs,IC)  = TRESGAS/40
                 END IF                                                  !MC CHANGE 6.25.09

! aggregate the inferred reserves for non-associated gas (decline gas)
  !mc change ver 7
                 !convert to ogsm regions
                 imc = aregion(irs)
  !mc change ver 7
                 if(imc.eq.7) imc = 5
  !mc change ver 7

                 if(apcode(irs).ge.11.and.apcode(irs).le.15) then
  !mc change ver 7
                     if(apcode(irs).eq.11.or.apcode(irs).eq.12) then
  !mc change ver 7
                       if(adepth(irs).lt.10000.0) then
  !mc change ver 7
                          infresv(3,imc,ic) = infresv(3,imc,ic) + (egremres(irs,ic)/1000.0)
  !mc change ver 7
                       else
  !mc change ver 7
                          infresv(4,imc,ic) = infresv(4,imc,ic) + (egremres(irs,ic)/1000.0)
  !mc change ver 7
                       end if
  !mc change ver 7
                     elseif(apcode(irs).eq.13) then
  !mc change ver 7
                       infresv(5,imc,ic) = infresv(5,imc,ic) + (egremres(irs,ic)/1000.0)
  !mc change ver 7
                     elseif(apcode(irs).eq.14) then
  !mc change ver 7
                       if(aresflag(irs).le.2) then
  !mc change ver 7
                         infresv(7,imc,ic) = infresv(7,imc,ic) + (egremres(irs,ic)/1000.0)  !wet coal
  !mc change ver 7
                       else
  !mc change ver 7
                         infresv(6,imc,ic) = infresv(6,imc,ic) + (egremres(irs,ic)/1000.0)  !wet shale
  !mc change ver 7
                       end if
  !mc change ver 7
                     elseif(apcode(irs).eq.15) then
  !mc change ver 7
                       if(aresflag(irs).le.2) then
  !mc change ver 7
                         infresv(7,imc,ic) = infresv(7,imc,ic) + (egremres(irs,ic)/1000.0)  !dry coal
  !mc change ver 7
                       else
  !mc change ver 7
                         infresv(6,imc,ic) = infresv(6,imc,ic) + (egremres(irs,ic)/1000.0)  !dry shale
  !mc change ver 7
                       end if
  !mc change ver 7
                     end if
  !mc change ver 7
                 end if
  !mc change ver 7
! end aggregation
  !mc change ver 7
               ENDIF

               eadjgross(ic) = adjgross(ic-strt+1)
               einjcost(ic)  = inj(ic-strt+1)
               eco2cost(ic)  = co2cost
               etorecy(irs,ic)   = torecy(ic-strt+1)
               etorecy_cst(ic)=torecy_cst(ic-strt+1)
               efoam(ic)     = foam(ic-strt+1)
               eigen(ic)     = float(igen(ic-strt+1))
               eproc_oam(ic) = proc_oam(ic-strt+1)
               estim(ic)     = stim(ic-strt+1)
               eexist_eoam(ic)=exist_eoam(ic-strt+1)
               enew_eoam(ic) = new_eoam(ic-strt+1)
               eexist_ecap(ic)=exist_ecap(ic-strt+1)
               enew_ecap(ic) = new_ecap(ic-strt+1)
               ecomp(ic)     = comp(ic-strt+1)
               etciadj(ic)   = tciadj(ic-strt+1)
               ecap_base(ic) = cap_base(ic-strt+1)
               edepggla(ic)  = LA(ic-strt+1)*PLAC+GG(ic-strt+1)*PGGC
               edep_crd(ic)  = dep_crd(ic-strt+1)
               eadggla(ic)   = adggla(ic-strt+1)
               edggla(ic)    = dggla(ic-strt+1)
               eeortca(ic)   = eortca(ic-strt+1)
               eintadd(ic)   = intadd(ic-strt+1)
               egglaadd(ic)  = GGLA(ic-strt+1)
               enibt(ic)     = nibt(ic-strt+1)
               eamint(ic)    = amint(ic-strt+1)
               eniat(ic)     = niat(ic-strt+1)
               etcadd(ic)    = EORTCA(ic-strt+1)+INTADD(ic-strt+1)+ &
                                   GGLA(ic-strt+1)
               ecatcf(ic)	 = catcf(ic-strt+1)
			  
             IF(apcode(irs).le.16) then
               adoilprice(ic)= OILPRICEo(ic-strt+1)+FACT
               eoilprice2(ic)= oilpriceo(ic-strt+1)
               egasprice2(ic)= gaspriceo(ic-strt+1)
             END if
            END DO






         ELSE
!
! CHECK TO BE SURE OF INPUT CONTROL
!
          PRINT *,'ERROR - SPECIFICATION OF SHIFT - ARRAY_SHIFT - SHIFT'
         STOP
        END IF
!  GET THE STATE CODE

        state = ''
        READ(aresid(irs)(3:4),'(a2)') state

        call state_match(state,ist)
        stnum = ist

       RETURN
       END SUBROUTINE
!
!***********************************************************************
!
!***********************************************************************
!
!***********************************************************************
!
!***********************************************************************
!
        SUBROUTINE INITINP()

        implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsml48'

       INTEGER iyr
           RESID  = "  "
           STate   = "  "
           iregion   = 0
           PROCDE = 0.0
           RRC    = 0.0
           SOI    = 0.0
           SOR    = 0.0
           PAY    = 0.0
           DEPTH  = 0.0
           tempr = 0.0
           presin = 0.0
           API    = 0.0
           GORL48 = 0.0
           ECON_REGION = 0
           POR    = 0.0
           AREA   = 0.0
           PATSZE = 0.0
           TOTPAT = 0.0
           WPP1   = 0.0
           WPP2   = 0.0
           WPP3   = 0.0
           WPP4   = 0.0
           IPUMPYR = 0.0
           NLAT   = 0.0
           LATLEN = 0.0
           XOOIP = 0.0
           FRAC_CO2  = 0.0
           FRAC_N2   = 0.0
           FRAC_H2S  = 0.0
           FRAC_NGL  = 0.0
           ECON_OPTYPE = 0
           ECON_LIFE = 0
           DO IYR=1,MAX_YR
              OPROD(IYR) = 0.0
              GPROD(IYR) = 0.0
              NPROD(IYR) = 0.0
              WPROD(IYR) = 0.0
              OINJ(IYR)  = 0.0
              WINJ(IYR)  = 0.0
              ORECY(IYR) = 0.0
           ENDDO

        RETURN
        END
!
!***********************************************************************
!
!***********************************************************************
!
!***********************************************************************
!
!***********************************************************************
!
       SUBROUTINE STORE_26(IRS,STRT,NYRI)
!
       IMPLICIT NONE
!
        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       INCLUDE 'ogsmugr'
       include 'ogsml48'

!
! LOCAL VARIABLES - CONTROLS
!
       INTEGER IRS
       INTEGER STRT
       INTEGER IEND
       INTEGER IC
       INTEGER NYRI
       REAL FEDROYALTY(MAX_YR)
       COMMON/AGG/FEDROYALTY
!
!
       IF ((STRT+NYRI).GT.max_yr-1) THEN
         IEND = max_yr-1
       ELSEIF ((STRT+NYRI).EQ.max_yr-1) THEN
         IEND = STRT + NYRI - 1
       ELSE
         IEND = STRT + NYRI - 1
       END IF
       IEND = iend-strt
!
! STORE ALL VALUES YEAR max_yr FORWARD; TO THE YEAR max_yr
!
            DO IC=IEND+2, NYRI
               IF (OILPROD(IC).GT.0.0) THEN
               EPRODOIL(IRS,max_yr) = EPRODOIL(IRS,max_yr) + OILPROD(IC)
               EPRODGAS(IRS,max_yr) = EPRODGAS(IRS,max_yr) + GASPROD(IC)
               EPRODWAT(IRS,max_yr) = EPRODWAT(IRS,max_yr) + WATPROD(IC)
               EWATINJ(IRS,max_yr)  = EWATINJ(IRS,max_yr)  + WATINJ(IC)
               ECO2INJ(IRS,max_yr)  = ECO2INJ(IRS,max_yr)  + TOTINJ(IC)
               ECO2RCY(IRS,max_yr)  = ECO2RCY(IRS,max_yr)  + TORECY(IC)
               ECO2POL(IRS,max_yr)  = ECO2POL(IRS,max_yr)  + INJ(IC)
               ESURFVOL(IRS,max_yr) = ESURFVOL(IRS,max_yr) + TOTINJ(IC)
               EROY(max_yr)     = EROY(max_yr)     + ROYALTY(IC)
               EFEDROY(max_yr)  = EFEDROY(max_yr)  + &
                FEDROYALTY(IC)
               ESEV(max_yr)     = ESEV(max_yr)     + SEV_TAX(IC)
!
! DO NOT REPORT NEGATIVE STATE OR FEDERAL TAXES
!
               ESTTAX(max_yr)   = ESTTAX(max_yr) + STATE_TAX(IC)
                IF (ESTTAX(max_yr).LE.0.0) ESTTAX(IC)   = 0.0
               EFEDTAX(max_yr)  = EFEDTAX(max_yr) + FEDTAX(IC)
                IF (EFEDTAX(max_yr).LE.0.0) EFEDTAX(IC) = 0.0

               EFEDCR(max_yr)    = EFEDCR(max_yr)    + &
                FEDTAX_CR(IC)
               EGROSSREV(max_yr)= EGROSSREV(max_yr) + &
                GROSS_REV(IC)
              EGRAVADJ(max_yr)  = EGRAVADJ(max_yr)  +GRAVPEN(IC)
               EGAEXP(max_yr)    = EGAEXP(max_yr)    +GA_EXP(IC)
               EGACAP(max_yr)    = EGACAP(max_yr)    +GA_CAP(IC)
               EII(IRS,max_yr)       = EII(IRS,max_yr)       +ICST(IC)
               EIIDRL(IRS,max_yr)    = EIIDRL(IRS,max_yr)    +II_DRL(IC)
               EICAP2(max_yr)    = EICAP2(max_yr)    +ICAP(IC)
               EINTCAP(max_yr)   = EINTCAP(max_yr)   +INTCAP(IC)
               ETI(IRS,max_yr)       = ETI(IRS,max_yr)       +TI(IC)
               ETIDRL(IRS,max_yr)    = ETIDRL(IRS,max_yr)    +TI_DRL(IC)
               EOTC(max_yr)      = EOTC(max_yr)      +OTC(IC)
               ETCI(max_yr)      = ETCI(max_yr)      +TCI(IC)
               EDEPR(max_yr)     = EDEPR(max_yr)     +DEPR(IC)
               EAMOR(max_yr)     = EAMOR(max_yr)     +AMOR(IC)
              ENETREV(max_yr)   = ENETREV(max_yr)   +NET_REV(IC)
               ETOC(IRS,max_yr)      = ETOC(IRS,max_yr)      +TOC(IC)
               EDEPLET(max_yr)   = EDEPLET(max_yr)   +DEPLET(IC)
               ETAXINC(max_yr)   = ETAXINC(max_yr)   +NIBTA(IC)
               EOAM(max_yr)      = EOAM(max_yr)      +OAM(IC)
               EAOAM(max_yr)     = EAOAM(max_yr)     +AOAM(IC)
              ELA(IRS,max_yr)       = ELA(IRS,max_yr)       +LA(IC)*PLAC
              EGG(max_yr)       = EGG(max_yr)       +GG(IC)*PGGC
               EEGGLA(max_yr)    = EEGGLA(max_yr)    +EGGLA(IC)  !LA(IC)*(1-PLAC)
               EATCF(max_yr)     = EATCF(max_yr)     +ATCF(IC)
               EDATCF(max_yr)    = EDATCF(max_yr)    +DATCF(IC)
               END IF
            END DO

       RETURN
       END SUBROUTINE



!**********************************************************************************
!**********************************************************************************
!**********************************************************************************
!**********************************************************************************
       subroutine arraytrans(ires)

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmugr'
       include 'ogsml48'

       INTEGER ires
       INTEGER iyr

       call initcash
       do IYR = 1,max_yr
         oilprod(iyr) = 0.0
         gasprod(iyr) = 0.0
         nglprod(iyr) = 0.0
         watprod(iyr) = 0.0
          totinj(iyr) = 0.0
          watinj(iyr) = 0.0
          torecy(iyr) = 0.0
            sump(iyr) = 0.0
           xpatn(iyr) = 0.0
          remres(iyr) = 0.0
          gremres(iyr) = 0.0
          adoilprice(iyr) = 0.0
          inj(iyr) =0.0
          aoam(iyr) =0.0
          adggla(iyr) =0.0
          patdev(ires,iyr,1) =0.0
          patdev(ires,iyr,2) =0.0
       end do

       do IYR = 1,max_yr-disclag
         oilprod(iyr+disclag)       = aprodoil(ires,iyr,1)
         gasprod(iyr+disclag)       = aprodgas(ires,iyr,1)
         nglprod(iyr+disclag)       = aprodngl(ires,iyr,1)
         watprod(iyr+disclag)       = aprodwat(ires,iyr,1)
         totinj(iyr+disclag)        = ainjinj(ires,iyr,1)
         watinj(iyr+disclag)        = awatinj(ires,iyr,1)
         torecy(iyr+disclag)        = ainjrecy(ires,iyr,1)
         sump(iyr+disclag)          = atotpat(ires,1)
         patdev(ires,iyr+disclag,1) = atotpat(ires,1)
       end do

       end subroutine


!***************************************************************
!from patcash.FOR
!     Last change:  MC   10 Dec 2008    9:26 am
          subroutine patcash(patlife,io)
!----------------------------------------------------------------------
! This subroutine performs the detail cashflow analysis of a single
! project. The results of the cashflow analysis will be written
! in the *.pro file.
!----------------------------------------------------------------------
        implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include 'ogsml48'

! local variables
        real stc,sic,psum
!        real*4 tinfl
        real sevtax,cumprod,facapi
        REAL maxnoi,minnoi,maxyr,minyr,lowyr
!       real*4 alb
        integer patlife,iyr,io
! Structure of the cashflow analysis
!       if(ires.lt.3)then
!       call cashflo2(ires,nyrsi)
!       return
!       endif
! Turning switch on and Off for Federal tax Credit
!
!cccc       xroy = 0.125!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        xroy variable in 'taxes.h'

       patlife=1

! EORTC IS NOT BEING USED IN THIS ROUTINE
! LATER CAN BE ADDED BUT USE VALUE FROM SCENARIO NOT HARDWIRE HERE.
! 3/3/2004
!        eortc=.false.
!       print* , eortc
!        if(procde.ge.10.and.procde.ne.28)then
!          if(procde.ne.12)then
!             eortc=.true.
!          endif
!        endif
!       nyrsi = 1.0
!       print* , eortc
!       pause


! Initialize variables for the cashflow

!ccc        call initcash
        pattoc_shut = 0.0

! Find Start year of production
        maxnoi = 0.0
        minnoi = 0.0
        maxyr = 0
        minyr = 0
        lowyr=0
        psum = 0.0
        fact = 0.0
! gravity adjustment factor --- NOT NEEDED BECAUSE RECEIVING PRICES BY API GRAVITY CATEGORY FROM THE LFMM
!       fact = facapi(api)
        fact = 0.0
!        print*, fact
!        fact = 1.0
!        print*, fact

!
        stc = 0.0
        sic = 0.0
        cumprod = 0.0
        do iyr = 1,max_yr-1
           cumprod = cumprod + oilprod(iyr)
        enddo

!-----Loop to do the annual cash flow--------------------------------
        do iyr = 1,max_yr-1

! calculate gross revenue
        gross_rev(iyr)= oilprod(iyr)*(oilpriceo(iyr)) + &
                      gasprod(iyr)*gaspriceo(iyr)
! calculate gravity and transportation adjustments
!        gravpen(iyr) = oilprod(iyr)*(oilpriceo(iyr)+fact)
        gravpen(iyr) = oilprod(iyr)*fact
        adjgross(iyr) = gross_rev(iyr) + gravpen(iyr)
! calculate royality
        royalty(iyr) = adjgross(iyr)* xroy

! calculate net revenue (Gross less gravpen less royalty)
        net_rev(iyr) = adjgross(iyr) - royalty(iyr)



!         PRINT*, gross_rev(iyr),oilpricec(iyr),gaspricec(iyr)
!         PRINT*, gravpen(iyr)
!         PRINT*, adjgross(iyr)
!         PRINT*, royalty(iyr)
!         PRINT*, net_rev(iyr)
!
! calculate sevtax using the function in toris
! For the purpose of the severence tax calculation the routine sevtax used
! in TORIS is modified for this program to take care of actual rates.
!
        stc = ti(iyr)
        sic = 0.0
        if(iyr.gt.1) sic=catcf(iyr-1)


        sevtax = net_rev(iyr)*0.05                                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!        call sev_tax2(ist,oilprod(iyr),gasprod(iyr)
!     &              ,patn(iyr),psum, roy,(oilpricec(iyr)+fact)
!     &              , gaspricec(iyr),sic,stc,sevtax,
!     &               procde,iyr)
        sev_tax(iyr) = sevtax
! calculate O & M costs
!        PRINT*, inj(iyr),oam(iyr),aoam(iyr),foam(iyr)


        toc(iyr) = inj(iyr) + oam(iyr) + aoam(iyr) + foam(iyr) + &
                   stim(iyr) + COMP(IYR) + PROC_OAM(IYR)

        toc(iyr) = toc(iyr) + fraccst(iyr)                            !mc change B - added fracturing cost to O&M

        IF(procde.ne.0 &
         .and.procde.ne.1.and.procde.ne.11)Then
          toc(iyr) = toc(iyr)*1.2
        Endif

 & ! following added to amor_base
!                   + ga_cap(iyr)


! this gets me operating income
        noi(iyr) = net_rev(iyr) - sev_tax(iyr) - toc(iyr)

!        cnoi(iyr) = cnoi(iyr) + noi(iyr)

!        PRINT*, sev_tax(iyr)
!        PRINT*, toc(iyr)
!        PRINT*, noi(iyr),iyr
!        pause
!       IF(iyr.eq.1) then
!        WRITE (io,*)'PATCASH ROUTINE'
!        WRITE (io,599)
!599     FORMAT (3X,'YR',6x,'GROSS REV  ',6x,'GRAV PEN   ',6x,
!     &    ' ADJ GROSS ',6x, ' ROYALTY   ',6x,' NET REV   ',6x,
!     &          'SEV TAX   ',6x,' TOT OPCOST',
!     &          6x,' NOI       ')
!       END if
!           WRITE (io,600)Iyr,GROSS_REV(iyr),GRAVPEN(iyr),ADJGROSS(iyr),
!     &       ROYALTY(iyr),NET_REV(iyr),SEV_TAX(iyr),TOC(iyr),NOI(iyr)
! 600        FORMAT (3X,I2,8(6x,F11.2))

       IF(noi(iyr).ge.0.0.and.oilprod(iyr).gt.0.0)patlife = iyr
       IF(noi(iyr).ge.0.0.and.GASprod(iyr).gt.0.0)patlife = iyr
!
! following added for pattern shutin operating cost for Timing Model
!
       IF(noi(iyr).ge.0.0.and.oilprod(iyr).gt.0.0)pattoc_shut=toc(iyr)

        if(procde.eq.7.and.pattoc_shut.le.0)pattoc_shut=999999.0

      enddo
!        PRINT *,'PRECOST : ',(NOI(ITY),ITY=1,5)

       RETURN
       END SUBROUTINE


!     Last change:  ASH  23 Mar 2004   11:18 am
       FUNCTION FACAPI(API)

! THIS FUNCTION TAKES THE API GRAVITY AND ADJUSTS THE PRICE
! USING THE MMS RSVP MODEL PRICE CORRECTION

        REAL API
        REAL PRICEADJ

        IF (API.GE.65.0) THEN
           PRICEADJ= -2.13

        ELSEIF ((API.LT.65.0).AND.(API.GE.45.0)) THEN
               PRICEADJ=((API-45.0)/(65.0-45.0))*(-3.0)+0.87

        ELSEIF ((API.LT.45.0).AND.(API.GE.41.0)) THEN
               PRICEADJ=((API-41.0)/(45.0-41.0))*(0)+0.87

        ELSEIF ((API.LT.41.0).AND.(API.GE.35.0)) THEN
               PRICEADJ=((API-35.0)/(41.0-35.0))*(0.12)+0.75

        ELSEIF ((API.LT.35.0).AND.(API.GE.30.0)) THEN
               PRICEADJ=((API-30.0)/(35.0-30.0))*(.75)+0.0

        ELSEIF ((API.LT.30.0).AND.(API.GE.0.0)) THEN
               PRICEADJ=((API-0.0)/(30.0-0.0))*(4.5)-4.5

        ELSEIF (API.LT.0.0) THEN
               PRICEADJ=-4.5

        END IF

        FACAPI = PRICEADJ
       
       RETURN
       END FUNCTION

!***************************************************************
!from ECON_CASHFLOW.FOR
!     Last change:  MC   22 Mar 2009    1:57 pm
          subroutine cashflow(nyrsi, ires, aresid_ires)
!----------------------------------------------------------------------
! This subroutine performs the detail cashflow analysis of a single
! project. The results of the cashflow analysis will be written
! in the *.pro file.
!----------------------------------------------------------------------
!   This subroutine conducts cashflow analysis on the project.  Code will be drawn from 
!   the COGAM Cashflow.

        implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include 'ogsml48'
        
        INTEGER, INTENT(IN) :: ires
        CHARACTER*11, INTENT(IN) :: aresid_ires
! local variables
        INTEGER nyrsi,maxyr,minyr,iyr,lowyr,i,ist,iyr1,iyr2,ii1,iyrtc
        REAL DAYPROD(MAX_YR)
        real dayprodg(max_yr)                                           !mc change A   daily gas production
        real dayprodw(max_yr)                                           !mc change A   daily water production
        real stc,sic,psum
        real tinfl, term, tprod,aset,bset,sttax,disctemp,temp
        real sevtax,facapi
!       real alb
        REAL XROY2
        REAL DISC_RATE2
        REAL eortcr2            !temporary EOR tax credit rate
        real credit


! VARIABLES USED TO FIND THE NORMALIZED BASE,ADVANCED PATTERNS
       REAL PAT_YR(MAX_YR)
       REAL NORM_BAS(MAX_YR)     !% OF PATERNS BASE
       REAL NORM_ADV(MAX_YR)     !% OF PATERNS ADVANCED
       COMMON / NORM_PAT / NORM_BAS, NORM_ADV,pat_yr
       REAL    cumprod,   cumgprod, total_patterns_c
       
!==========================> HSM Code Start <==========================
    character*2 state_id(52)
    DATA (state_id(i),i=1,6)/'AL','AZ','AR','CA','CO','CT'/
    DATA (state_id(i),i=7,12)/'DE','DC','FL','GA','ID','IL'/
    DATA (state_id(i),i=13,18)/'IN','IA','KS','KY','LA','ME'/
    DATA (state_id(i),i=19,24)/'MD','MA','MI','MN','MS','MO'/
    DATA (state_id(i),i=25,30)/'MT','NE','NV','NH','NJ','NM'/
    DATA (state_id(i),i=31,36)/'NY','NC','ND','OH','OK','OR'/
    DATA (state_id(i),i=37,42)/'PA','RI','SC','SD','TN','TX'/
    DATA (state_id(i),i=43,48)/'UT','VT','VA','WA','WV','WI'/
    DATA (state_id(i),i=49,52)/'WY','AK','HI','PO'/
!===========================> HSM Code End <===========================
       
!       REAL norm_bas(MAX_YR)
!       REAL norm_adv(MAX_YR)
!       COMMON / norm_pat / norm_bas, norm_adv

!***********************************************************************
!
!     APPLY THE ROYALTY RATE CHANGE FROM THE SCENARIO DEFINITION
!
!***********************************************************************
!

        ist = stnum
        XROY2 = XROY
!
!
!     ADDED HARDCODE TO HANDLE 40% TANGIBLE RATE FOR GAS
!                              30% TANGIBLE RATE FOR OIL
!
        IF (RESOURCE.EQ.'O') THEN
           TANG_RATE = 0.3
        ELSEIF (RESOURCE.EQ.'G') THEN
           TANG_RATE = 0.4
        END IF
!
!       SPECIFY TAX CODE ACCORDING TO OPERATOR TYPE
!
        IF (ECON_OPTYPE.EQ.1) THEN
!          MAJOR
           pdr = 0.0                ! Depletion Rate
           piic= 0.3                 ! Intan. Inv. to Capitalize
        ELSEIF (ECON_OPTYPE.EQ.2) THEN
!          LARGE INDEPENDENT
           pdr = 0.15                ! Depletion Rate
           piic= 0.0                 ! Intan. Inv. to Capitalize
        ELSEIF (ECON_OPTYPE.EQ.3) THEN
!          SMALL INDEPENDENT
           pdr = 0.0                ! Depletion Rate
           piic= 0.0                 ! Intan. Inv. to Capitalize
        ELSE
!          UNSPECIFIED USE MAJOR
           pdr = 0.0                ! Depletion Rate
           piic= 0.3                 ! Intan. Inv. to Capitalize
        END IF
!
!
!     END OF ADDITION - CONFIRM BEFORE IMPLEMENTING AS VARIABLE
!

! Initialize variables for the cashflow

        call initcash
!        do iyr=1,MAX_YR
!            drl_cst(iyr) = 0.0
!            drl_cst(iyr) =drl_cst2(iyr)*factdrl
!        enddo
! Find Start year of production

        maxctcf = 0.0
        minctcf = 0.0
        maxyr = 0
        minyr = 0
        lowyr=0

        psum = 0.0
        fact = 0.0
! gravity adjustment factor --- NOT NEEDED BECAUSE RECEIVING PRICES BY API GRAVITY CATEGORY FROM THE LFMM
!       fact = facapi(api)
        fact = 0.0

        do i=1,max_yr-1
          finding(i) = 0.0
        enddo

!   Step 1: Calculate cumulative production
        stc = 0.0
        sic = 0.0
        cumprod = 0.0
        cumgprod = 0.0
        total_patterns_c = 0.0
        do iyr = 1,max_yr-1
           cumprod = cumprod + oilprod(iyr)
           cumgprod = cumgprod + gasprod(iyr)
           total_patterns_c = total_patterns_c + pat_yr(iyr)
        enddo

!ccc        IF (MARGINAL) THEN
!ccc           FP = 0.0
!ccc           PLOW = 100.0
!ccc           DO IYR=1,MAX_YR
!ccc             WATCRED(IYR) = 0.0
!ccc             DAYPROD(IYR) = 0.0
!ccc             IMARG(IYR)   = .FALSE.
!ccc           ENDDO
!ccc           DO IYR = 1,40
!ccc               IF (XPATN(IYR).GT.0.0) THEN
!ccc                  DAYPROD(IYR) = OILPROD(IYR)/XPATN(IYR)*1000.0/365.0
!ccc               ELSE
!ccc                  DAYPROD(IYR) = 0.0
!ccc               END IF
!ccc               IF(DAYPROD(IYR).LE.MAXBBL) IMARG(IYR) = .TRUE.
!               PRINT* ,IYR,OILPROD(IYR),DAYPROD(IYR),IMARG(IYR)
!     &         ,WATCRED(IYR),XPATN(IYR)
!               IF ( IMARG(IYR) ) PAUSE
!ccc           ENDDO
!ccc        END IF

! Marginal well check
!   oil:
!    1) if oil production is less than 15 Bbl/day
!    2) if oil production is less than 25 Bbl/day and watercut is at least 95%
!    3) if majority of oil production is from heavy oil (api<20)
!
!   gas:
!    1) if gas production is less than 90 Mcf/day
     if (marginal) then
       do iyr = 1,max_yr
          imarg(iyr) = .false.
          watcred(iyr) = 0.0
          dayprod(iyr) = 0.0
          dayprodg(iyr) = 0.0
          dayprodw(iyr) = 0.0
          if(xpatn(iyr).gt.0.0) then
            dayprod(iyr) = oilprod(iyr)/xpatn(iyr)*1000.0/365.0
            dayprodg(iyr) = gasprod(iyr)/xpatn(iyr)*1000.0/365.0
            dayprodw(iyr) = watprod(iyr)/xpatn(iyr)*1000.0/365.0
          end if
       end do
       if(procde.eq.0) then                                      !mature oil
         do iyr = 1,max_yr
           if(dayprod(iyr).lt.m_oil1) imarg(iyr) = .true.        !(1)
           if(dayprod(iyr).lt.m_oil2) then
             if(dayprod(iyr).gt.0.0.and.(dayprodw(iyr)/dayprod(iyr).gt.m_wat/100.0)) imarg(iyr) = .true.
           end if

           if(api.lt.m_api) imarg(iyr) = .true.
         end do
       elseif(procde.ge.11.and.procde.le.15) then                !mature gas
         do iyr = 1,max_yr
            if(dayprodg(iyr).lt.m_gas1) imarg(iyr) = .true.
         end do
       end if

!       write (799,1799) resid
!       write (799,2799) (oilprod(iyr),iyr=1,max_yr-1)
!       write (799,2799) (gasprod(iyr),iyr=1,max_yr-1)
!       write (799,2799) (watprod(iyr),iyr=1,max_yr-1)
!       write (799,2799) (dayprod(iyr),iyr=1,max_yr-1)
!       write (799,2799) (dayprodg(iyr),iyr=1,max_yr-1)
!       write (799,2799) (dayprodw(iyr),iyr=1,max_yr-1)
!       write (799,3799) (imarg(iyr),iyr=1,max_yr-1)
 1799  format (a11)
 2799  format (45(f12.3,3x))
 3799  format (45(l12,3x))

     end if

!
! END OF MARGINAL RATE CHECK
!

!   Step 2: Calculate the lease bonus & Geological and geophysical costs
       IF(procde.eq.1.or. &
        procde.eq.10)then
!-----Process; Horizontal Profile, or Primary Predictive
!         if(iagg.eq.0)then
! Lease Bonus
           la(1)=lbc_frac*cumprod*oilpriceo(1)*(1-XROY2)
! define gg as fraction of EWC
!           gg(1)=drl_cst(1)*tang_m(1)*gg_fac
!         else
! Lease Bonus
!           la(styr)=lbc_frac*cumprod*Oilpriceo(styr)*(1-XROY2)
! define gg as fraction of EWC
!           gg(styr)=drl_cst(styr)*tang_m(styr)*gg_fac
!         endif
       ELSEIF (procde.ge.18.and.procde.le.20) then
           la(1) = lbc_frac*cumgprod*gaspriceo(1)*(1-xroy2)
       Else
           la(1) = 0.0
           gg(1) = 0.0
       Endif

!         finding(1) = cumprod*5.0 + cumgprod*0.75
       do iyr= 1,max_yr-1
         if(procde.eq.1.or.procde.eq.10)then  !undiscovered conventional oil
           finding(iyr)=cumprod*7.62*pat_yr(iyr) &
           /total_patterns_c
         ELSEIF(procde.eq.16)then  !undiscovered conventional gas
           term = 0.0
           TERM = 1+0.7*(gaspriceo(IYR) - 4.0) / 4.0
           if(term.gt.0.97)term = 0.97
             finding(iyr)=cumgprod*7.62*0.178*term*pat_yr(iyr) &
             /total_patterns_c
         ELSEIF(procde.eq.18.or.procde.eq.19.or.procde.eq.20)then  !undiscovered Shale,CBM
           term = 0.0
           TERM = 1+0.4*(gaspriceo(IYR) - 3.54 ) / 3.54
           if(term.gt.1.0)term = 1.0
             finding(iyr)=cumgprod*7.62*0.178*0.45*term*pat_yr(iyr) &
            /total_patterns_c
         endif
         la(iyr) = la(iyr) + finding(iyr)
       end do
!
       if (procde.eq.0) then
         DISC_RATE2 = discount_rt(1)
       else
       DISC_RATE2 = DISCOUNT_RT(PROCDE+1)
       end if

!-----Process Decline curve
        if(procde.eq.0)then
           la(1) = 0.0
           gg(1) = 0.0
        endif

!   Step 3: Start Year Loop : do iyr = 1,maxyr
        do iyr = 1,max_yr-1 !!!! start of main cashflow loop

!  Added MC 9.9.08 - check the oil price.  If the oil price is
!  greater than the EOR cutoff price, the tax credit is not
!  applied.  At direction of HM.  Additional change: eortcr -> eortcr2
         if (oilpriceo(iyr).gt.eortcp) then
            eortcr2 = 0.0
         else
            eortcr2 = eortcr
         end if
!  end oil price & EOR tax credit check

         if (procde.eq.0) then
           IF ((.NOT.OIL_SALES(1)).AND.(GAS_SALES(1))) THEN                             !MC temp - process number
               OILPRICEo(IYR) = 0.0
           ELSEIF((OIL_SALES(1)).AND.(.NOT.GAS_SALES(1))) THEN
               GASPRICEo(IYR) = 0.0
           ENDIF
         else
         IF ((.NOT.OIL_SALES(PROCDE+1)).AND.(GAS_SALES(PROCDE+1))) THEN
               OILPRICEo(IYR) = 0.0
         ELSEIF((OIL_SALES(PROCDE+1)).AND. &
            (.NOT.GAS_SALES(PROCDE+1))) THEN
               GASPRICEo(IYR) = 0.0
           ENDIF
         end if

         psum = psum + patn(iyr)

!           A. Calculate the tangible drilling costs
         ti_drl(iyr) = drl_cst(iyr) * tang_rate * tang_m(iyr)

!           B. Calculate the capitalized costs

!           C. Calculate other tangible costs
         otc(iyr)=faccost(iyr)*tang_fac_rate*fac_m(iyr)

!           ADD COMPRESSION COSTS TO OTHER TANGIBLE CAPITAL
         otc(iyr)= OTC(IYR) + comp(iyr) * TANG_M(IYR)

!           D. Calculate total tangible investments
         ti(iyr) = ti_drl(iyr) + otc(iyr)

!           E. Calculate intangible investments
         ii_drl(iyr) = drl_cst(iyr)*(1-tang_rate)*tang_m(iyr)
!           F. Calculate intangibles to be capitalized
         icap(iyr)=faccost(iyr)*(1-tang_fac_rate)*fac_m(iyr)

         ICST(iyr) = ii_drl(iyr) + icap(iyr) &
          + exist_ecap(iyr) + new_ecap(iyr)

         if(cidc) intcap(iyr) = intcap(iyr) + piic*ii_drl(iyr)
         if(coi) intcap(iyr)  = intcap(iyr) + piic*icap(iyr)
         tci(iyr)=ti(iyr)+intcap(iyr)

!           G. Apply tax credits on tangible and intangible investments
! Adjustment for Federal Tax Credits (tciadj)
! if enhanced oil recovery tax credit then
!    credit for total capital investment
! endif
! if tax credit on tangibles then
!   if within eligible years then
!      credit tangible (tcap) and environmental (etcap) capital
!   endif
! elseif no tax credit on tangibles then
!   if tax credit on development tangibles then
!      credit for tangible capital
!   endif
!   if environmentals not expensed and environmental tax credit then
!      credit for environmental tangible capital
!   endif
! endif

        if(eortc) then

         tciadj(iyr)=tciadj(iyr)+eortcr2* &
         (ICST(iyr)+ti(iyr))

        endif
! Tax Credit on Tangible Investments
         if(tcoti) then
          if(yr1.ge.iyr) then
           tciadj(iyr)=tciadj(iyr) + tdtcr*ti_drl(iyr)
          endif
         else

! Tax credit in tangible devlopment cost
          if(tdtc) then
           tciadj(iyr)=tciadj(iyr) + tdtcr*ti_drl(iyr)
          endif
         endif
!           H. Calculate gross revenue
        gross_rev(iyr)= oilprod(iyr)*(oilpriceo(iyr)) + &
                      gasprod(iyr)*gaspriceo(iyr)

!            PRINT*, resid,iyr,gross_rev(iyr),gasprod(iyr)

!           I. Adjust gross revenue for API gravity
                      
!==========================> HSM Code Start <==========================
        fact = 0.0 ! This is handled by LFMM
!===========================> HSM Code End <===========================
        
        gravpen(iyr) = oilprod(iyr) * fact
        adjgross(iyr) = gross_rev(iyr) + gravpen(iyr)

!           J. Calculate royalty
        royalty(iyr) = adjgross(iyr)* XROY2

!           K. Calculate net revenue
        net_rev(iyr) = adjgross(iyr) - royalty(iyr)

!
!***********************************************************************
!
! WATKINS CREDIT TO BE INTRODUCED HERE - tax credit on existing marginal wells
!
!***********************************************************************
!
!ccc        IF (MARGINAL) THEN
!ccc           IF( IMARG(IYR) )THEN
!
! DETERMINE CREDIT AMOUNT BASED ON FLUID PURCHASE PRICE (FPP)
!
!ccc             IF(FPP(IYR).LT.MAR_MIN) CREDIT = MARGAMT
!ccc
!ccc             IF(FPP(IYR).GE.MAR_MIN.AND.FPP(IYR).LE.MAR_MAX)
!ccc     &         CREDIT= MAR_CM + MAR_DM*FPP(IYR)
!ccc
!ccc             IF(FPP(IYR).GT.MAR_MAX) CREDIT=0.0
!
! CALCULATE HOW MUCH CREDIT TO APPLY THIS YEAR BASED ON PRODUCTION
!
!ccc             IF(FP.EQ.1.0.AND.DAYPROD(IYR).GE.0.0)THEN
!ccc                   WATCRED(IYR) = BBLCRD*CREDIT*XPATN(IYR)*0.365
!ccc
!ccc             ELSEIF(FP.EQ.0.0.AND.DAYPROD(IYR).GE.BBLCRD)THEN
!ccc                   WATCRED(IYR) = BBLCRD*CREDIT*XPATN(IYR)*0.365
!ccc
!ccc             ELSEIF(FP.EQ.0.0.AND.DAYPROD(IYR).LT.BBLCRD)THEN
!ccc                   WATCRED(IYR) = DAYPROD(IYR)*CREDIT*XPATN(IYR)*0.365
!ccc
!ccc             ENDIF
!ccc           ELSE
!ccc             WATCRED(IYR) = 0.0
!ccc
!ccc           ENDIF
!ccc        ENDIF

       if(marginal) then
! determine the credit to be applied to the production
! three cases:
!  1.  price < min applicable price                           => full credit applied
!  2.  min applicable price < price < max applicable price    => credit is prorated
!  3.  price > max applicable price                           => no credit applied

!  determining the value of the credit for the project
!  two cases:
!      production is less than the maximum daily credit allotment - apply to production
!      production is more than the maximum daily credit allotment - apply to maximum allotment


        if(imarg(iyr)) then                                                 !only for years when credit can be applied
         credit = 0.0
         MAR_CM = 0.0
         MAR_DM = 0.0
         if(procde.eq.0) then                                                !mature oil
           if(oilpriceo(iyr).lt.marg_oil_min) credit = m_amt_oil             !(1)

           IF (M_amt_oil.GT.0.0)THEN                                         !(2)
             MAR_DM = M_amt_oil / ( MARg_oil_MIN - MARg_oil_MAX )
             MAR_CM = - ( MAR_DM * MARg_oil_MAX )
           ENDIF

           if(oilpriceo(iyr).ge.marg_oil_min.and.oilpriceo(iyr).le.marg_oil_max) then
             credit = mar_cm + mar_dm*oilpriceo(iyr)
           end if

           if(oilpriceo(iyr).gt.marg_oil_max) credit = 0.0                   !(3)

           if(dayprod(iyr).gt.m_vol_oil) then
             watcred(iyr) = credit*m_vol_oil*xpatn(iyr)*0.365
           else
             watcred(iyr) = credit*dayprod(iyr)*xpatn(iyr)*0.365
           end if
         elseif(procde.ge.11.and.procde.le.15) then                          !mature gas
           if(gaspriceo(iyr).lt.marg_gas_min) credit = m_amt_gas             !(1)

           IF (M_amt_gas.GT.0.0)THEN                                         !(2)
             MAR_DM = M_amt_gas / ( MARg_gas_MIN - MARg_gas_MAX )
             MAR_CM = - ( MAR_DM * MARg_gas_MAX )
           ENDIF

           if(gaspriceo(iyr).ge.marg_gas_min.and.gaspriceo(iyr).le.marg_gas_max) then
             credit = mar_cm + mar_dm*gaspriceo(iyr)
           end if

           if(gaspriceo(iyr).gt.marg_gas_max) credit = 0.0                   !(3)

           if(dayprodg(iyr).gt.m_vol_gas) then
             watcred(iyr) = credit*m_vol_gas*xpatn(iyr)*0.365
           else
             watcred(iyr) = credit*dayprodg(iyr)*xpatn(iyr)*0.365
           end if
         end if
        end if
       end if

!           L. Calculate severance tax
!           Call sev_tax(stnum,sevtax)                       !!!! this is a temporary severance tax subroutine.
!                                                            !!!! it assumes a flat rate - state level
        stc = ti(iyr)
        sic = 0.0
        if(iyr.gt.1)sic=catcf(iyr-1)

        call sev_tax2(ist,oilprod(iyr),gasprod(iyr) &
                    ,patn(iyr),psum, XROY2,(oilpriceo(iyr)+fact) &
                    , gaspriceo(iyr),sic,stc,sevtax, &
                     procde,iyr)

        sev_tax(iyr) = sevtax!!!!!!!!!!!!!!!! * SD_ST_CHG         FIX THIS - MC

!           M. Calculate G & A

       if (norm_bas(iyr).eq.0.0.and.norm_adv(iyr).eq.0.0) then
           if (iyr.eq.1) then
             norm_bas(iyr) = 0.0
             norm_adv(iyr) = 0.0
           else
             norm_bas(iyr) = norm_bas(iyr-1)
             norm_adv(iyr) = norm_adv(iyr-1)
           end if
       end if
       IF(procde.eq.9.or.procde.eq.1.or.procde.eq.10.or. &
         procde.eq.17)Then

         ga_cap(iyr) = gna_cap(2)* norm_adv(iyr)*(ICST(iyr) + ti(iyr)) + &
                       gna_cap(1)* norm_bas(iyr)*(ICST(iyr) + ti(iyr))

           ga_exp(iyr) = gna_exp(2) * norm_adv(iyr) * (inj(iyr) + &
                           oam(iyr)+aoam(iyr)) + &
                         gna_exp(1) * norm_bas(iyr) * &
                           (inj(iyr)+oam(iyr)+aoam(iyr))

       ELSE
           ga_cap(iyr) = gna_cap(2) * norm_adv(iyr) * &
                             (ICST(iyr) + ti(iyr)) + &
                         gna_cap(1) * norm_bas(iyr) * &
                             (ICST(iyr) + ti(iyr))

         ga_exp(iyr) = gna_exp(2)*norm_adv(iyr)*(oam(iyr) + aoam(iyr)) + &
                       gna_exp(1)*norm_bas(iyr)*(oam(iyr) + aoam(iyr))

       Endif

!      add reservoir characterization to the G&A in the first year                   !mc change ver 11
!      if(iyr.eq.1.and.(lyrgas.le.0..or.lyroil.le.0.).and.itimeyr.gt.1)   &
       if(iyr.eq.1.and.(lyrgas.le.0..and.lyroil.le.0.))   &
                    ga_exp(iyr) = ga_exp(iyr)+res_chr_fac(1)+res_chr_fac(2)          !mc change ver 11

!           N. Calculate total Operating and Maintainance costs
        STIM(IYR) = STIM(IYR) * INTANG_M(IYR)
        toc(iyr) = inj(iyr) + oam(iyr) + aoam(iyr) + ga_exp(iyr) &
                    + foam(iyr) &
       + exist_eoam(iyr) + new_eoam(iyr) + STIM(IYR) + PROC_OAM(IYR)

        toc(iyr) = toc(iyr) + fraccst(iyr)                            !mc change B - added fracturing cost to O&M
        

!           O. Calculate net operating income
        noi(iyr) = net_rev(iyr) - sev_tax(iyr) - toc(iyr)

!           P. Calculate depreciation & amortization
         cap_base(iyr) = tci(iyr) -tciadj(iyr)
!         cap_base(iyr) = ti(iyr)
        IF(iyr.eq.1)xcapbase(iyr) = cap_base(iyr)
        if(iyr.gt.1)xcapbase(iyr) = xcapbase(iyr-1) + cap_base(iyr)

        amor_base(iyr) = intcap(iyr) + ga_cap(iyr)

       if(cap_base(iyr).gt.0)then
          tinfl=1.0
          do iyr1=0,idepryr-1
           if(iyr+iyr1 .le. MAX_YR)then
           tinfl=tinfl*(1+rinfl(iyr+iyr1))
           if(iyr1.eq.0)tinfl=1.0
           temp = 0.0
           temp =depr(iyr+iyr1)+cap_base(iyr)*deprschl(iyr1+1)
            depr(iyr+iyr1)=depr(iyr+iyr1)+cap_base(iyr)*deprschl(iyr1+1) &
           /tinfl


!           rem_base(iyr+iyr1) = rem_base(iyr + iyr1) -


!           if(iyr+iyr1.eq.iyr)then
!            rem_base(iyr + iyr1) = cap_base(iyr)-temp+rem_base(iyr+iyr1)
!           else
!             rem_base(iyr+iyr1) =  xcapbase(iyr1) - temp
!            rem_base(iyr+iyr1) =  rem_base(iyr+iyr1-1) - temp
!     &        + cap_base(iyr)
!           endif

           endif
          enddo
       endif

       if(amor_base(iyr).gt.0)then
        tinfl=1.0
        do iyr2=0,iamoryr
         if(iyr+iyr2 .le. MAX_YR)then
           tinfl=tinfl*(1+rinfl(iyr+iyr1))
           if(iyr2.eq.0)tinfl=1.0
           temp =amor(iyr+iyr2)+amor_base(iyr)*amorschl(iyr2+1)
             amor(iyr+iyr2)=amor(iyr+iyr2)+amor_base(iyr)* &
             amorschl(iyr2+1)/tinfl
             if(iyr+iyr2.eq.1)then
               rem_amor(iyr + iyr2) = amor_base(iyr) - temp
              else
               rem_amor(iyr + iyr2) = rem_amor(iyr+iyr2-1) - temp
             endif
         endif
        enddo  !iyr2
       endif

!           Q. Calculate expensed G&G and Lease aquisition costs
       eggla(iyr)=la(iyr)*(1-plac) + gg(iyr)*(1-pggc)

!           R. Calculate G&G and lease aquisition cost depletion

       adggla(iyr)=  gg(iyr)*pggc + la(iyr)*plac
       dggla(iyr)=dggla(iyr)+gg(iyr)*pggc+la(iyr)*plac

       if(dggla(iyr).ne.0 .AND.(remres(iyr)+gremres(iyr)/5.65) &
        +oilprod(iyr).ne.0) then
!        do iyr1=iyr,MAX_YR

         tprod=oilprod(iyr)+gasprod(iyr)/5.642
         if(tprod.gt.0.)then
          tgglcd(iyr)= dggla(iyr)*tprod/(tprod+ &
           (remres(iyr)+gremres(iyr)/5.65))

 & !         dggla(iyr+1)=(dggla(iyr)-
!          dggla(iyr)*tprod/(tprod+remres(iyr)))/(1+rinfl(iyr))
         endif
!        enddo
       else
           tgglcd(iyr)=0.0
       endif

       nilb(iyr)=net_rev(iyr)-sev_tax(iyr)-toc(iyr)- &
         ICST(iyr)+intcap(iyr)-eggla(iyr)-depr(iyr)

       if(nil) then
        if(nilb(iyr).gt.0) then
         apd(iyr)=min(nilb(iyr)*nill,net_rev(iyr)*pdr)
        else
         apd(iyr)=0.
        endif
       else
        apd(iyr)=net_rev(iyr)*pdr
       endif
!           S. Calculate depletion
       deplet(iyr)=max(tgglcd(iyr),apd(iyr))
          dggla(iyr+1)=max((dggla(iyr)- deplet(iyr)),0.0)

!           T. Calculate net taxable income before addback
        nibta(iyr) = noi(iyr) -ICST(iyr) + intcap(iyr) &
        -eggla(iyr) - depr(iyr) - deplet(iyr) - amor(iyr)

!           U. Tax Credit Addback
       if(tcoii) then
        if(yr2.ge.iyr) then
         if(cidc) then
          idca(iyr)=idca(iyr)+(1-piic)*ii_drl(iyr)*idctcr*idctcab
           ii1=0
          do iyr2=iyr,iyr+iamoryr
           ii1=ii1+1
           idca(iyr2)=idca(iyr2)+piic*ii_drl(iyr)*idctcr*idctcab &
           *amorschl(ii1)
          enddo
         else
          idca(iyr)=idca(iyr)+ii_drl(iyr) * idctcr*idctcab
         endif
        else
         idca(iyr)=0
        endif
       else
        if(idctc) then
         if(cidc) then
          idca(iyr)=idca(iyr)+(1-piic)*ii_drl(iyr)*idctcr*idctcab
           ii1=0
          do iyr2=iyr,iyr+iamoryr
           ii1=ii1+1
           tinfl=tinfl*(1+rinfl(iyr2))
           if(ii1.eq.1)tinfl=1.0
           idca(iyr2)=idca(iyr2)+piic*ii_drl(iyr)*idctcr*idctcab &
           *amorschl(ii1)/tinfl
          enddo
         else
          idca(iyr)=ii_drl(iyr)*idctcr*idctcab
         endif
        else
         idca(iyr)=0
        endif
       endif
!  G&G and Lease Acquisition Addback
       if(ggctc) then
!        dggla(iyr)=dggla(iyr)-gg(iyr)*pggc*ggctcr

        if(gg(iyr)*pggc.gt.0.0.AND.(remres(iyr)+gremres(iyr)/5.65) &
         +oilprod(iyr).gt.0.) then
            do iyr1=0,MAX_YR
             if(iyr+iyr1.lt.MAX_YR)then
              tprod=oilprod(iyr+iyr1)+gasprod(iyr+iyr1)/5.642
              ggla(iyr+iyr1)=ggla(iyr+iyr1)+ &
              gg(iyr)*pggc*tprod/(tprod+(remres(iyr+iyr1)+ &
              gremres(iyr+iyr1)/5.65))*ggctcr*ggctcab
             endif
!             PRINT*, ggla(iyr)
            enddo
!            pause
        endif
       endif

       if(lactc) then
!        dggla(iyr)=dggla(iyr)-la(iyr)*plac*lactcr
        if(la(iyr)*plac.gt.0.0.AND.(remres(iyr)+gremres(iyr)/5.65) &
         +oilprod(iyr).gt.0.) then
            do iyr1=0,MAX_YR
             if(iyr+iyr1.lt.MAX_YR)then
              tprod=oilprod(iyr+iyr1)+gasprod(iyr+iyr1)/5.642
              ggla(iyr+iyr1)=ggla(iyr+iyr1)+ &
              la(iyr)*plac*tprod/ &
        (tprod+(remres(iyr+iyr1)+gremres(iyr+iyr1)/5.65))*lactcr*lactcab
             endif
            enddo
        endif
       endif

!           V. Other intangible addbacks
       if(tcoii) then
        if(yr2.ge.iyr) then
         if(coi) then
          oia(iyr)=oia(iyr)+(1-piic)*icap(iyr)*oitcr*oitcab

          do iyr1=0,iamoryr-1
           if(iyr+iyr1.le.MAX_YR)then
            oia(iyr1+iyr)=oia(iyr1+iyr)+piic*icap(iyr)*oitcr*oitcab &
           *amorschl(iyr1+1)
           endif
          enddo
         else
          oia(iyr)=icap(iyr)*oitcr*oitcab
         endif
        else
         oia(iyr)=0
        endif
       elseif(oitc) then
         if(coi) then
          oia(iyr)=oia(iyr)+(1-piic)*icap(iyr)*oitcr*oitcab

          do iyr1=0,iamoryr-1
           if(iyr+iyr1.le.MAX_YR)then

            oia(iyr1+iyr)=oia(iyr1+iyr)+piic*icap(iyr)*oitcr*oitcab &
            *amorschl(iyr1+1)
           endif
          enddo
         else
          oia(iyr)=icap(iyr)*oitcr*oitcab
         endif
       else
         oia(iyr)=0
       endif

! calculate intangible environmental addback
!      if(eitc) then
!       if(ce) then
!        eia(iyr)=eia(iyr)+(1-piic)*eicap(iyr)*eitcr*eitcab
!
!          ii1=0
!         do iyr2=iyr,iyr+iamoryr
!          if(iyr2.le.MAX_YR)then
!           ii1=ii1+1
!           eia(iyr2)=eia(iyr2)+piic*eicap(iyr)*eitcr*eitcab
!    $      *amorschl(ii1)
!          endif
!         enddo
!
!       else
!        eia(iyr)=eia(iyr)+eitcap(iyr)*eitcr*eitcab
!       endif
!      else
!       eia(iyr)=0
!      endif

! calculate environmental operating cost addback
!      if(eoctc) then
!       eoca(iyr)=eoam(iyr)*eoctcr*eoctcab
!      else
!       eoca(iyr)=0
!      endif

!           W. G&G lease aquisition addback
       if(ggetc) then
         ggla(iyr)=ggla(iyr)+ggetcr*gg(iyr)*(1-pggc)*ggetcab

       endif
       if(laetc) then
         ggla(iyr)=ggla(iyr)+laetcr*la(iyr)*(1-plac)*laetcab
       endif
! EOR tax credit addback

       if(eortc)then
          eortca(iyr)=eortca(iyr)+ &
          eortcr2*((1-piic)*ICST(iyr))*eortcab
         tinfl=1.0
         do iyr1=0,iamoryr-1
          if(iyr+iyr1.le.MAX_YR)then
          tinfl=tinfl*(1+rinfl(iyr+iyr1))
          if(iyr1.eq.0)tinfl=1.0
           eortca(iyr1+iyr)=eortca(iyr1+iyr)+piic*ICST(iyr) &
             *eortcr2*eortcab &
             *amorschl(iyr1+1)/tinfl
          endif
         enddo
         tinfl=1.0
         do iyr1=0,idepryr-1
          if(iyr+iyr1.le.MAX_YR)then
          tinfl=tinfl*(1+rinfl(iyr+iyr1))
          if(iyr1.eq.0)tinfl=1.0
           eortca(iyr1+iyr)=eortca(iyr1+iyr)+ti(iyr) &
             *eortcr2*eortcab &
             *deprschl(iyr1+1)/tinfl
          endif
         enddo
      endif
!
       intadd(iyr)=idca(iyr)+oia(iyr)

!           X. Calculate net income before taxes
       nibt(iyr)=nibta(iyr)+eortca(iyr)+intadd(iyr)+ggla(iyr) &
                 +oia(iyr)

!           Y. Calculate state taxes
        aset = adjgross(iyr)
        bset = nibta(iyr)
        state_tax(iyr) = sttax(ist,bset,aset)

!           Z. Calculate AMT
        amint(iyr) = amtrate* (nibt(iyr)-state_tax(iyr))
        fit(iyr) = fedrate * (nibt(iyr)-state_tax(iyr))
        if(amt)then
            sfit(iyr) = max(fit(iyr),amint(iyr))
        else
            sfit(iyr) = fit(iyr)
        endif
!           AA.Calculate federal taxes
        fedtax(iyr) = sfit(iyr)

!           AB.Calculate federal tax credits
! Injectant cost added to taxcredit
        if(eortc) then
         tinfl=1.
         do iyrtc=0,14
         if (iyr+iyrtc.le.max_yr) then  !added MC 6.9.08 - to avoid exceeding the array size for fedtax_cr
          fedtax_cr(iyr+iyrtc) =fedtax_cr(iyr+iyrtc)+ &
          (ti(iyr) + ICST(iyr) &
           +inj(iyr) &
            )*eortcr2*eortcrp(iyrtc+1) &
          /tinfl
          tinfl=tinfl*(1.+rinfl(iyr+iyrtc+1))
         end if                         !added MC 6.9.08 - to avoid exceeding the array size for fedtax_cr
         enddo
        endif
        if(tcoti) then
          if(yr1.ge.iyr) then
           fedtax_cr(iyr)=fedtax_cr(iyr)+ &
             ggctcr*gg(iyr)*pggc
            fedtax_cr(iyr)=fedtax_cr(iyr)+ &
             lactcr*la(iyr)*plac
            fedtax_cr(iyr)=fedtax_cr(iyr)+ &
             tdtcr*ti_drl(iyr)
!           fedtax_cr(iyr)=fedtax_cr(iyr)+
!     &      ettcr*etcap(iyr)
          endif
        else

             if(ggctc) fedtax_cr(iyr)=fedtax_cr(iyr)+ &
              ggctcr*gg(iyr)*pggc
             if(ggetc) fedtax_cr(iyr)=fedtax_cr(iyr)+ &
              ggetcr*gg(iyr)*(1-pggc)
             if(lactc) fedtax_cr(iyr)=fedtax_cr(iyr)+ &
              lactcr*la(iyr)*plac
             if(laetc) fedtax_cr(iyr)=fedtax_cr(iyr)+ &
              laetcr*la(iyr)*(1-plac)
             if(tdtc) fedtax_cr(iyr)=fedtax_cr(iyr)+ &
              tdtcr*ti_drl(iyr)
!             if(ettc) fedtax_cr(iyr)=fedtax_cr(iyr)+
!          &      ettcr*etcap(iyr)
        endif

              if(tcoii) then
               if(yr2.ge.iyr) then
                fedtax_cr(iyr)=fedtax_cr(iyr)+ &
                   idctcr*ii_drl(iyr)
                fedtax_cr(iyr)=fedtax_cr(iyr)+ &
                 oitcr*icap(iyr)
               endif
              else
               if(idctc) fedtax_cr(iyr)=fedtax_cr(iyr)+ &
                idctcr*ii_drl(iyr)
               if(oitc) fedtax_cr(iyr)=fedtax_cr(iyr)+ &
                 oitcr*icap(iyr)
              endif

!       if(eitc) fedtax_cr(iyr)=fedtax_cr(iyr)+
!     &   eitcr*eicap(iyr)
!       if(eoctc) fedtax_cr(iyr)=fedtax_cr(iyr)+
!     &   eoctcr*eoam(iyr)

!***********************************************************************
!
! APPLY THE MARGINAL WELL TAX CREDIT
!
!***********************************************************************

        IF (MARGINAL) THEN                                                         !mc change A
           IF ( IMARG(IYR) ) THEN                                                  !mc change A
              fedtax_cr(IYR) = fedtax_cr(IYR) + WATCRED(IYR)                       !mc change A
           ENDIF                                                                   !mc change A
        ENDIF                                                                      !mc change A


        if(procde.eq.0)then
              fedtax(iyr) = 0.0
              state_tax(iyr) = 0.0
        endif
!           AC.Calculate net income after taxes
        niat(iyr) = nibt(iyr) - state_tax(iyr) - fedtax(iyr)

!           AD.Calculate annual cashflow after taxes
         atcf(iyr) = niat(iyr) + depr(iyr) + deplet(iyr) -adggla(iyr) &
        - intcap(iyr) -ti(iyr) -eortca(iyr) -intadd(iyr) -ggla(iyr) &
        + amor(iyr) &
         + fedtax_cr(iyr)
!           AE.Discount cashflow after taxes
        temp = 0.0
        disctemp = 0.0
        disctemp = ((1+disc_rate2)**(iyr-0.5))
        datcf(iyr) = atcf(iyr)/((1+disc_rate2)**(iyr-0.5))
		
        if(iyr.eq.1)catcf(iyr) = datcf(iyr)
        if(iyr.gt.1)then
			if(datcf(iyr).eq.0.0) then
			catcf(iyr) = catcf(iyr-1)
			else
			catcf(iyr) = catcf(iyr-1) + datcf(iyr)
			endif
		endif
	
	
! other discounted items for timing model
        disc_drl(iyr)=(ti_drl(iyr)+ii_drl(iyr))/disctemp
        disc_ndrl(iyr)= (otc(iyr)+icap(iyr)+adggla(iyr))/disctemp
        disc_oam(iyr) = toc(iyr)/disctemp
        disc_inv(iyr) = disc_drl(iyr) + disc_ndrl(iyr)
        disc_fed(iyr) = (fedtax(iyr)-fedtax_cr(iyr))/disctemp
        disc_st(iyr)  = (state_tax(iyr)+sev_tax(iyr))/disctemp
        disc_roy(iyr) = royalty(iyr)/disctemp
        disc_oil(iyr) = oilprod(iyr)*(oilpriceo(iyr)+fact)/disctemp
        disc_gas(iyr) = gasprod(iyr)*gaspriceo(iyr)/disctemp

!           AF.Calculate cumulative after tax cashflow
        if(procde.ne.0)then
             if(iyr.eq.1.or.iyr.le.1)then
               maxctcf = catcf(iyr)
               minctcf = catcf(iyr)
               go to 941
             endif

             if(catcf(iyr).gt.maxctcf)maxctcf=catcf(iyr)
             if(catcf(iyr).gt.maxctcf)maxyr=iyr
             lowyr = iyr-1
             if(catcf(iyr).lt.minctcf)minctcf=catcf(iyr)
             if(catcf(iyr).lt.minctcf)minyr=iyr

          if(maxctcf.gt.0.0.and.atcf(iyr).lt.0.0)then
                 nyrsi = iyr
                 lowyr = iyr-1
!            if (resid.eq."DGWY5304515") nyrsi = 43
                 go to 942
!            ELSEIF(maxctcf.gt.0.0.AND.iyr.eq.25)then
          elseif((maxctcf.gt.0.0.or.atcf(iyr).gt.0.0).and. &
           iyr.eq.max_yr-5)then
               nyrsi = max_yr-1
!              PRINT *,maxctcf,atcf(iyr),iyr
               GO TO 989

          endif
             nyrsi = maxyr
             if (procde.eq.21.or.procde.eq.17) nyrsi = max_yr-1
             lowyr = nyrsi
             if(nyrsi.eq.0)nyrsi=1
        else
             if(iyr.eq.1)then
               maxctcf = catcf(1)
               minctcf = catcf(1)
               go to 941
             endif
             if(catcf(iyr).gt.maxctcf)maxctcf=catcf(iyr)
             if(catcf(iyr).gt.maxctcf)maxyr=iyr

             if(catcf(iyr).lt.minctcf)minctcf=catcf(iyr)
             if(catcf(iyr).lt.minctcf)minyr=iyr

          if(maxctcf.gt.0.0.and.atcf(iyr).lt.0.0)then
                     nyrsi = iyr
                     lowyr = iyr-1
                     go to 989
          elseif((maxctcf.gt.0.0.or.atcf(iyr).gt.0.0).and.iyr &
           .eq.max_yr-1)then
                  nyrsi  = max_yr-1
                  go to 989
          endif

             nyrsi = maxyr
             if(nyrsi.eq.0)nyrsi=1
         endif
941     continue

        
!   End Step 3: End do
        end do !end of main cashflow loop  *****
942     continue

!		do i = 1, max_yr
!		if(datcf(i).eq.0.0) catcf(i) = catcf(i-1)
!		enddo

!   Step 4: Calculate end of economic life using cumulative after tax cashflow (3:AF)
          if(nyrsi.le.0)nyrsi= 1

        if(maxctcf.le.0.0)then
          nyrsi = 1
          go to 999
        endif

         iyr = lowyr
      IF(iyr.gt.1)then
        If(catcf(iyr).lt.catcf(iyr-1))then
              nyrsi=iyr
              nibta(nyrsi) = nibta(nyrsi) + depr(nyrsi) + deplet(nyrsi) &
              + amor(nyrsi)

!   Step 5: Recalculate depreciation and amortization
              do iyr1 = nyrsi+1, MAX_YR
               depr(nyrsi) = depr(nyrsi) + depr(iyr1)
               amor(nyrsi) = amor(nyrsi) + amor(iyr1)
               deplet(nyrsi) = deplet(nyrsi) + deplet(iyr1)
              enddo
              tinfl = 1.0
              do iyr1 = 1, nyrsi-1
               tinfl=tinfl*(1+rinfl(iyr1))
              enddo
!              depr(nyrsi) = depr(nyrsi) + rem_base(nyrsi)/tinfl
!              amor(nyrsi) = amor(nyrsi) + rem_amor(nyrsi)/tinfl
             if(pdr.le.0.0)then
                temp = 0.0
               do iyr1 = 1,nyrsi-1
                 temp = temp + deplet(iyr1)
               enddo

                deplet(nyrsi) = dggla(1) - temp
                if(deplet(nyrsi).le.0.0)deplet(nyrsi) = 0.0
             else
               deplet(nyrsi) = deplet(nyrsi)
             endif

              nibta(nyrsi) = nibta(nyrsi) - depr(nyrsi) - deplet(nyrsi) &
              - amor(nyrsi)
              nibt(nyrsi)=nibta(nyrsi)+eortca(nyrsi)+intadd(nyrsi) &
               +ggla(nyrsi) +oia(nyrsi)

!   Step 6: Recalculate state taxes
            aset = adjgross(nyrsi)
            bset = nibta(nyrsi)
            state_tax(nyrsi) = sttax(ist,bset,aset)

!   Step 7: Recalculate AMT
            amint(nyrsi) = amtrate* (nibt(nyrsi)-state_tax(nyrsi))
            fit(nyrsi) = fedrate * (nibt(nyrsi)-state_tax(nyrsi))
            if(amt)then
               sfit(nyrsi) = max(fit(nyrsi),amint(nyrsi))
            else
               sfit(nyrsi) = fit(nyrsi)
            endif

!   Step 8: Recalculate federal taxes
            fedtax(nyrsi) = sfit(nyrsi)

!   Step 9: Recalculate net income after taxes
            niat(nyrsi) = nibt(nyrsi) - state_tax(nyrsi) - fedtax(nyrsi)

!   Step 10:Recalculate annual cashflow after taxes
            atcf(nyrsi) = niat(nyrsi) + depr(nyrsi) + deplet(nyrsi) &
            - adggla(nyrsi) - intcap(nyrsi) -ti(nyrsi) -eortca(nyrsi) &
            -intadd(nyrsi)-ggla(nyrsi) + amor(nyrsi) + fedtax_cr(nyrsi)

!   Step 11:Recalculate discounted cashflow after taxes
            datcf(nyrsi) = atcf(nyrsi)/((1+disc_rate2)**(nyrsi-0.5))
            if(nyrsi.eq.1)catcf(nyrsi) = datcf(nyrsi)
            if(nyrsi.gt.1)catcf(nyrsi) =catcf(nyrsi-1) + datcf(nyrsi)

         go to 999
        Endif
       ENDIF   !iyr gt.1

989     continue
        IF(nyrsi.gt.2)nyrsi=nyrsi-1

999     continue
!==========================> HSM Code Start <==========================
if (curcalyr.le.2025) then
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, procde,'ga_cap', gna_cap, norm_adv(1), norm_bas(1), ICST(1), ti(1), ga_cap
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, procde,'eggla' , la(1) , plac, gg(1), pggc, eggla
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, procde,'adggla', la(1) , plac, gg(1), pggc, adggla
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, 'faccost', faccost
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, 'fac_m'  , fac_m
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, 'comp'   , comp
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, 'tang_m' , tang_m
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, DRL_CST(1), TANG_M(1), GG_FAC, 'gg' , gg
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, DRL_CST(1), TANG_M(1), GG_FAC, 'gg' , gg
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, DRL_CST(1), TANG_M(1), GG_FAC, 'gg' , gg
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, 'ggctcr*gg(iyr)*pggc', ggctcr*gg(iyr)*pggc
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, 'WATCRED', WATCRED
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, 'eortcr', (ti(1) + ICST(1) +inj(1))*eortcr2*eortcrp(11)/tinfl, ti(1), ICST(1), inj(1), eortcr2, eortcrp, tinfl
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, aresid_ires, ires, procde, tang_rate, tang_fac_rate, disc_rate2, fedrate, oilpriceo(1), gaspriceo(1)
endif
!===========================> HSM Code End <===========================

!==========================> HSM Code Start <==========================
if (hsm_dcf_bool) then
    ! 'function', 'year', 'crude_price', 'natgas_price', 'crude_trans_price', &
    ! 'natgas_trans_price', 'crude_tariff_price', 'natgas_tariff_price', 'royalty_rate', 'fed_tax_rate', &
    ! 'net_present_value', 'abandon_rate', 'state_abbreviation', 'exploratory_success_rate', 'development_success_rate', &
    ! 'exploratory_cost', 'development_cost', 'exploratory_dry_cost', 'development_dry_cost', 'exp_tang_frac', &
    ! 'dev_tang_frac', 'kap_tang_frac', 'intang_amor_frac'
    ! hsm inputs
    write(hsm_dcf_opts, '(*(G0.16,:,","))') 'cashflow', curcalyr, oilpriceo(1), gaspriceo(1), 0.0, 0.0, 0.0, 0.0, XROY2, &
                                            fedrate, sum(datcf), 0.0, state_id(ist), 0.0, 0.0, 0.0 ,0.0, 0.0, 0.0, tang_rate, &
                                            tang_rate, tang_fac_rate, 0.0, 'slm_6' , 'macrs_7', disc_rate2
    ! hsm inputs
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'crude_production'      , oilprod * (atcf/atcf)
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'natgas_production'     , gasprod * (atcf/atcf)
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'dev_drill_cost'        , drl_cst*tang_m * (atcf/atcf) ! note if atcf = 0.0, makes nan
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'kap_cost'              , (faccost*fac_m +comp * TANG_M) * (atcf/atcf) ! note if atcf = 0.0, makes nan
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'general_admin_cost'    , ga_cap * (atcf/atcf) ! note if atcf = 0.0, makes nan, seems to be an error that doesn't impact OGSM but will impact HSM
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'tangible_adjustment'   , tciadj * (atcf/atcf)
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'depletion'             , deplet
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'operating_cost'        , toc
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'geo_geo_and_lease_aq'  , (eggla + adggla) * (atcf/atcf) ! note if atcf = 0.0, makes nan, seems to be an error that doesn't impact OGSM but will impact HSM
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'eor_tax_credit'        , eortca * (atcf/atcf)
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'fed_credit'            , fedtax_cr * (atcf/atcf)
    ! hsm checks
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'revenue'           , adjgross
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'royalty'           , royalty
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'severance_tax'     , sev_tax ! severance tax very involved, save for later
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'drill_cost'        , drl_cst*tang_m * (atcf/atcf) ! note if atcf = 0.0, makes nan
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'expn_intang_cost'  , ICST
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'deprec_tang_cost'  , depr * (atcf/atcf) ! note if atcf = 0.0, makes nan
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'amor_intang_cost'  , amor * (atcf/atcf) ! note if atcf = 0.0, makes nan
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'tax_base_state'    , nibta
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'tax_base_fed'      , (nibt - state_tax)
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'fed_tax'           , (fedtax - fedtax_cr)
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'state_tax'         , state_tax
    write(hsm_dcf_on  , '(*(G0.16,:,","))') 'cash_flow'         , atcf
endif
!===========================> HSM Code End <===========================

        END subroutine

!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************

        subroutine initcash

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsml48'

        integer iyr

        do iyr =1,MAX_YR

!          igen(iyr) = 0.0
          ti_drl(iyr)   = 0.0
          otc(iyr)      = 0.0
          ti(iyr)       = 0.0
          ii_drl(iyr)   = 0.0
          icap(iyr)     = 0.0
          ICST(iyr)       = 0.0
          intcap(iyr)   = 0.0
          eicap(iyr)    = 0.0
          gross_rev(iyr)= 0.0
          gravpen(iyr)  = 0.0
          adjgross(iyr) = 0.0
          royalty(iyr)  = 0.0
          net_rev(iyr)  = 0.0
          sev_tax(iyr)  = 0.0
          ga_exp(iyr)   = 0.0
          ga_cap(iyr)   = 0.0
          toc(iyr)      = 0.0
          fedtax_cr(iyr) = 0.0
          cap_base(iyr) = 0.0
          rem_base(iyr) = 0.0    ! added
          rem_amor(iyr)  = 0.0   ! added
          amor_base(iyr) = 0.0   ! added
          amor(iyr)     = 0.0        ! added
          depr(iyr)     = 0.0
          noi(iyr)      = 0.0
          nibta(iyr)    = 0.0
          nibt(iyr)     = 0.0
          state_tax(iyr)= 0.0
          amint(iyr)    = 0.0
          sfit(iyr)     = 0.0
          fit(iyr)      = 0.0
          fedtax(iyr)   = 0.0
          fedtax_cr(iyr)= 0.0  ! added
          atcf(iyr)     = 0.0
          datcf(iyr)    = 0.0
          catcf(iyr)    = 0.0
          tci(iyr)      = 0.0
          tciadj(iyr)   = 0.0
          la(iyr)       = 0.0
!          gg(iyr)       = 0.0
          eggla(iyr)    = 0.0
          dggla(iyr)    = 0.0
          dep_crd(iyr)  = 0.0
          tgglcd(iyr)   = 0.0
          nilb(iyr)     = 0.0
          apd(iyr)      = 0.0
          deplet(iyr)   = 0.0
          idca(iyr)     = 0.0
          oia(iyr)      = 0.0
          ggla(iyr)     = 0.0
          eortca(iyr)   = 0.0
          intadd(iyr)   = 0.0
          niat(iyr)     = 0.0
          eia(iyr)      = 0.0
          eoca(iyr)     = 0.0
          disc_inv(iyr) = 0.0
          disc_drl(iyr) = 0.0
          disc_ndrl(iyr) = 0.0
          disc_oam(iyr) = 0.0
          disc_fed(iyr) = 0.0
          disc_st(iyr) = 0.0
          disc_roy(iyr) = 0.0
          disc_oil(iyr) = 0.0
          disc_gas(iyr) = 0.0
          xcapbase(iyr) = 0.0
          enddo
          return
          end
!***************************************************************
!from ECON_DCFROR.FOR
!     Last change:  MC   20 May 2008    0:58 am
       SUBROUTINE DCFROR(NYRSI,EFFINV)
!
!-------------------------------------------------------------------------------
!     COMMON STATEMENTS.
!-------------------------------------------------------------------------------
!
! ADD ROR INFO HERE
!
        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        INCLUDE 'ogsml48'
!
        REAL MUCT(MAX_YR),MUCI(MAX_YR)
!!!        REAL TI(MAX_YR),II(MAX_YR),ATCF(MAX_YR)
        REAL ATPR(MAX_YR)
        INTEGER NYRSI
        INTEGER MYR
        INTEGER ANYCHK,TECH1,TECH2
        REAL KORW,KORO
        REAL MAXNCF
        REAL EFFINV
!
        INTEGER IDISC   ! METHOD OF DISCOUNTING
                        ! IDISC .EQ. 0 - END YEAR DISCOUNTING
                        ! IDISC .GT. 0 - MID YEAR DISCOUNTING
!
!-----------------------------------------------------------------------
!     ***** COMPUTE DCF RATE OF RETURN
!-----------------------------------------------------------------------
!
        INTEGER YR,COUNT
        LOGICAL MARK,RECAL
        REAL SUMPCF,SUMNCF,T1,T2,T3,DR1,DR2,XBAR, &
        PV1,PV2,PVX,DELTA,CCF
!
! INITITIALIZE AND SET VARIABLES
!
        IDISC = 1
        DO I=1,MAX_YR
           ATPR(I) = 0.0
           MUCT(I) = 0.0
           MUCI(I) = 0.0
        ENDDO
        MYR = NYRSI
        DO I=1,MYR
           ATPR(I) = ATCF(I)
           MUCT(I) = TI(I)
           MUCI(I) = ICST(I)
        ENDDO
        COSTRT = 0.0
        XINF   = 0.0
        EFFINV = 0.0

!     ***** ITERATE ON DISCOUNT RATE TO FIND ZERO DCF
       ITMAX = 100
       XINVEST = MUCT(1) + MUCI(1) + COSTRT
!
! CHECK WHETHER CASH FLOWS HAVE ANY SIGN CHANGES; IF NOT, THEN NO ROR EXISTS
!
      NSCHG = 0
      IF(MYR .LE. 1) GO TO 4
! FIND FIRST NON-ZERO CASH FLOW
      DO 1 I=1,MYR
         IF(ATPR(I) .NE. 0.0) GO TO 2
    1 CONTINUE
! ALL CASH FLOWS ARE ZERO, ROR DOES NOT EXIST
      ROR = 0.
      EFFINV = 0.
      RETURN
!
!
! FIND A CHANGE IN SIGN FROM YEAR I FOREWARD.
!
!
    2 J = I
      NEGYR = I
    3 IF(J .EQ. MYR) GO TO 4
      J = J + 1
      IF(ATPR(J) .EQ. 0.) GO TO 3
      IF( ATPR(I) / ABS(ATPR(I)) .EQ. ATPR(J) / ABS(ATPR(J)) ) GO TO 3
      IF(NSCHG.EQ.0 .AND. ATPR(J) .GT. 0.) NEGYR = J - 1
      NSCHG = NSCHG + 1
      I=J
      GO TO 3
   4  IF(NSCHG .NE. 0) GO TO 44
      IF(ATPR(MYR).LE.0.) GO TO 42
! ALL CASH FLOWS ARE POSITIVE
41    ROR = 1000.
!     EFFINV = 1000.
      GO TO 401
!     RETURN


!     GO TO 301
!
  42  CONTINUE
! ALL CASH FLOWS ARE NEGATIVE
      ROR    = 0.0
      EFFINV = 0.0
      RETURN


!     GO TO 301
!
44    CONTINUE
!
!      INITIALIZE ROR AT USER DISCOUNT RATE
      XDR = 0.18 ! ADDED
      ROR = XDR



!      ROR IS "REAL" DISCOUNT RATE AFTER INFLATION
      XROR = (1. + XINF) * (1. + ROR)
      XDRS = ROR
      XDRN = XROR
      MUSPV = ATPR(J)
!
!      SEARCH FOR ROR, MAXIMUM OF ITMAX TIMES
      DO 5 IT = 1, ITMAX
!      DCFATT IS THE AFTER TAX PRESENT VALUE
!      DCFATP IS THE FIRST DERIVATIVE OF DCFAT WRT ROR
      DCFATT = ATPR(MYR)
!      PRINT *,XDR,IDISC
      IF(IDISC .EQ. 0) DCFATP = FLOAT(MYR) * ATPR(MYR)
      IF(IDISC .GT. 0) DCFATP = (FLOAT(MYR) - 0.5) * ATPR(MYR)
      DO 50 I = 2,MYR
      DCFATT = DCFATT / XROR + ATPR(MYR - I + 1)
      IF(IDISC .EQ. 0) &
         DCFATP = DCFATP / XROR + ATPR(MYR - I + 1) &
                  * FLOAT(MYR - I + 1)
   50 IF(IDISC .GT. 0) &
         DCFATP = DCFATP / XROR + ATPR(MYR - I + 1) &
                  * (FLOAT(MYR - I + 1) - 0.5)
!
!      SUBTRACT PROJECT START-UP COST FROM  PRESENT VALUE
!     DCFATT = DCFATT - COSTRT
      IF(IDISC .GT. 0) &
         DCFATT = DCFATT / XROR ** 0.5
!     DCFATT = DCFATT - XINVEST
      DCFATP = -DCFATP / XROR
      IF(IDISC .GT. 0) &
         DCFATP = DCFATP * XROR ** 0.5
!     DCFATP = DCFATP + XINVEST
!
!      TRY TOP CLOSE NET PRESENT VALUE TO ZERO
      IF(ABS(DCFATT) .LT. 0.09) GO TO 6
!      SAVE OLD RATE
      XDRS = XROR
!
!      USE NEWTON-RAPSON TO ADJUST ROR
      XROR = XROR - DCFATT/DCFATP
!      PREVENT NEGATIVE OR ZERO ROR
      IF(XROR .LT. 1.0) XROR = 1.0
!      SAVE NEW PRESENT VALUE
      MUSPV = DCFATT
!      SAVE NEW RATE
      XDRN = XROR
!

    5 CONTINUE
!
!     IF HERE, DID NOT CONVERGE ON ROR
!
      ROR = -1.0
      GO TO 401
!     EFFINV = -1.0
!     GO TO 301
!
!      SOLVED FOR ROR
    6 CONTINUE
      ROR = (XROR / (1. + XINF) - 1.) * 100.
!
!     SOLVE FOR INVESTMENT EFFICIENCY,EFFINV
!  COMPUTES INVESTMENT EFFICIENCY AT USERS SET DISCOUNT RATE
!
!  XDR : DISCOUNT RATE FOR RUN
!  ATPR(YR) : UNDISCOUNTED AFTER TAX CAHSH FLOW IN YEAR 'YR'
!  MYR : LIFE OF PROJECT
!  SUMPCF : SUM OF DISCOUNTED POSITIVE CASH FLOWS
!  SUMNCF : ABSOLUTE VALUE OF SUM OF NEGATIVE CASH FLOWS
!  DMUL : DISCOUNTING FACTOR
!
 401  CONTINUE
       CCF=0.
       SUMPCF=0.
       MARK=.FALSE.
      XINVEST = MUCT(1) + MUCI(1) + COSTRT
!
      SUMNCF = XINVEST
!
      MAXNCF = XINVEST
!
      ATPR(1) = ATPR(1) + XINVEST
      DO 201 I=2,MYR
      XX = MUCT(I) + MUCI(I)
      ATPR(I-1)=ATPR(I-1) - XX
 201  ATPR(I) = ATPR(I) + XX
!
        DO 3000 YR=1,MYR
!  DON'T DISCOUNT YEAR ONE
!    1-10-84 WE DO DISCOUNT IN YR 1 SINCE INVESTMENTS ARE REMOVED
!        IF(YR.EQ.1)DMUL=1.0
!
!  DISCOUNT ALL OTHER YEARS ON THE HALF YEAR
        T1=FLOAT(YR)-0.5
!       IF(YR.GT.1)DMUL=(1.0+XDR)**T1
       XDR = 0.15                                                                                      !!!!!MC - TEMP

      DMUL = (1.0 + XDR)**T1
!
!  T2 : DISCOUNTED CASH FLOW IN YEAR
!
         T2=ATPR(YR)/DMUL
!  CCF : CUMULATIVE CASH FLOW
       CCF=CCF+T2
       IF(CCF.GE.0.)MARK=.TRUE.
!  CEASE ACCUMULATING NEGATIVES ONCE CUMULATIVE CASH FLOW GOES POSITIVE
         IF(T2.GE.0.)SUMPCF=SUMPCF+T2
       IF(MARK)GO TO 3000
         IF(T2.LT.0.)SUMNCF=SUMNCF+T2*(-1.)
!
!  CALCULATE MAXIMUM NEGATIVE CASH FLOW - MAXNCF
!
      IF(SUMNCF .GT. MAXNCF)MAXNCF = SUMNCF
!
3000     CONTINUE
!
!   COMPUTE INVESTMENT EFFICIENCY AS THE RATIO OF POSITIVE TO
!   NEGATIVE CASH FLOW SUMS
!
!  CASE WHERE THERE ARE NO NEGATIVE CASH FLOWS
       IF(SUMNCF.LE.0.)EFFINV=1.0E03
!  ALL OTHER CASES
!
        IF(MAXNCF.GT.0.)EFFINV=SUMPCF/MAXNCF
!       PRINT *,SUMPCF, MAXNCF, EFFINV
!
!  THIS COMPLETES COMPUTATION OF INVESTMENT EFFICIENCY
 301  CONTINUE
      ATPR(1) = ATPR(1) - XINVEST
      DO 302 I=2,MYR
      XX = MUCT(I) + MUCI(I)
      ATPR(I-1) = ATPR(I-1) + XX
      CONTINUE
 302  ATPR(I) = ATPR(I) - XX
      RETURN
       END SUBROUTINE
!***************************************************************
!from econ_wellcount.FOR
!     Last change:  MC   27 Apr 2009    4:55 pm
       SUBROUTINE WELLCOUNTS(IRS,PATYR)
!
! THIS SUBROUTINE WILL BE USED TO DETERMINE NEW
! WELLS DRILLED, CONVERTED, ETC.
!
!-----These includes are for the timing/exploration models
        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmugr'
       include 'ogsml48'

! Variables used to transfer the # of wells drilled
       real einjdrx2(MAX_YR)
       real eproddrx2(MAX_YR)
       real eprodwellx2(MAX_YR)
       real einjwellx2(MAX_YR)
       real eshutinx2(MAX_YR)
       REAL edryholex2(max_yr)                             !4.1.09
       COMMON / NEWWELLS / einjdrx2, eproddrx2, eprodwellx2, &
                           einjwellx2, eshutinx2,edryholex2

       INTEGER IC
       INTEGER IRS
       REAL PATYR(MAX_YR)
!***********************************************************************
!
!     This section of code added to calculate the number of
!     wells drilled by year and the number of active wells
!     by year.
!             einjdr    New injection wells drilled per year
!             eproddr   New production wells drilled per year
!             eshutin   Shutin wells
!             eprodwell Active producing wells per year
!             einjwell  Active injection wells per year
!             xpp1      Total number of producers
!             xpp2      Total number of injectors
!             xpp3      producers to injectors
!             xpp4      producers converted to secondary producers
!
!***********************************************************************
!
         do ic=1,max_yr-1
           einjdrx2(ic)      = 0.0
           eproddrx2(ic)     = 0.0
           eshutinx2(ic)     = 0.0
           eprodwellx2(ic)   = 0.0
           einjwellx2(ic)    = 0.0
           edryholex2(ic)    = 0.0
         end do
!
 1625   format (a11,4(2x,f5.2),<max_yr-1>(2x,f10.2))
!
        IF(PROCDE.EQ.0.OR.PROCDE.EQ.11.OR.PROCDE.EQ.13.OR.    & !decline curve
               PROCDE.EQ.12.OR.procde.eq.14.or. &
               PROCDE.EQ.15) THEN
           do i=1,max_yr-1
             eshutinx2(i)   = 0.0
             eshutinx2(i)   = (sump(i)-xpatn(i))
             eprodwellx2(i) = (xpatn(i))
!             eproddrx2(i)   = 0.0                                       !MC CHANGE 4.27.09
             EPRODDRX2(I) = (PATDEV(IRS,I,1)+PATDEV(IRS,I,2))*XPP1     !MC CHANGE 4.27.09
             einjdrx2(i)    = 0.0
             einjwellx2(i)  = 0.0
!             edryholex2(i)  = 0.0                                       !MC CHANGE 4.27.09
             EDRYHOLEX2(I)  = (PATDEV(IRS,I,1)+PATDEV(IRS,I,2))*XPP1*     & !MC CHANGE 4.27.09
                REGDRYKD(AREGION(IRS))                                 !MC CHANGE 4.27.09
           end do
!
         ELSEIF (procde.eq.1.or.procde.eq.10.or.          & !Primary processes only new drilling
                 PROCDE.EQ.17.OR.PROCDE.EQ.21.OR.PROCDE.EQ.22.OR.PROCDE &
                 .EQ.23.or.procde.eq.16.or.procde.eq.17.or. &
                 procde.eq.18.or.procde.eq.19.or.procde.eq.20) then

           do i=1,max_yr-1

             einjdrx2(i)    = (patyr(i) * (xpp2))        !New injectors drilled
             eproddrx2(i)   = (PATYR(i) * (xpp1))        !New producers drilled
             eprodwellx2(i) = (xpatn(i) * (xpp1))        !new active producers
             einjwellx2(i)  = (xpatn(i) * (xpp2+xpp3))   !new active injectors
             eshutinx2(i)   = (sump(i)-xpatn(i)) * xpp1   & !Shutin new producers
                            + (sump(i)-xpatn(i)) * xpp2   & !shutin new injectors
                            + (sump(i)-xpatn(i)) * xpp3  !shutin converted
!             IF(procde.lt.21) then                                                                  !4.16.09

!           IF(procde.eq.10.or.procde.eq.17) then                          !oil exploration          !4.15.09
            IF(procde.eq.10) then                                          !oil exploration          !4.15.09
               IF(i.eq.1) then  !first year - exploration and development                            !4.15.09
                 edryholex2(i) = (xpp1+xpp2)*                                                         & !4.15.09
                  regdryUE(aregion(irs))                                                           !4.16.09
                 edryholex2(i) = edryholex2(i) + (patyr(i)-1)*                                        & !4.15.09
                  (xpp1+xpp2)*regdryUD(aregion(irs))                                               !4.16.09
               else             !other years - development                                           !4.15.09
                 edryholex2(i) = patyr(i)*(xpp1+xpp2)*                                                & !4.15.09
                  regdryUD(aregion(irs))                                                           !4.16.09
               END if                                                                                !4.15.09
            ELSEIF(procde.eq.16.or.procde.eq.18.or.procde.eq.19.or.         & !gas exploration          !4.15.09
               procde.eq.20) then                                                                    !4.15.09
               IF(i.eq.1) then  !first year - exploration and development                            !4.15.09
                 edryholex2(i) = (xpp1+xpp2)*                                                         & !4.15.09
                  regdryUE(aregion(irs))                                                           !4.16.09
                 edryholex2(i) = edryholex2(i) + (patyr(i)-1)*                                        & !4.15.09
                  (xpp1+xpp2)*regdryUD(aregion(irs))                                               !4.16.09
               else             !other years - development                                           !4.15.09
                 edryholex2(i) = patyr(i)*(xpp1+xpp2)*                                                & !4.15.09
                  regdryUD(aregion(irs))                                                           !4.16.09
               END if                                                                                !4.15.09
            ELSEIF(procde.eq.17.or.procde.eq.21.or.procde.eq.22.or.procde.eq.23) THEN              !4.15.09
            edryholex2(i) = patyr(i)* (xpp1+xpp2)* &
                            regdryuD(aregion(irs))   !non-exploration                              !4.16.09
            else
            edryholex2(i) = patyr(i)* (xpp1+xpp2)* &
                            regdryKD(aregion(irs))   !non-exploration                              !4.16.09
            END if
!             else                                                                  !4.13.09         !4.16.09

!             END if                                                                                 !4.16.09
           end do
!
         ELSEIF (procde.eq.6) then
           do i=1,max_yr-1
             einjdrx2(i)    = (PATYR(i) * (xpp2))              !New injectors drilled
             eproddrx2(i)   = (PATYR(i) * (xpp1))              !New producers drilled
             eprodwellx2(i) = (xpatn(i) * (xpp1))              !new active producers
!     &                      + !(xpatn(i) * (1))                 !existing active producers
!     &                      - !(xpatn(i) * (xpp3))              !Less conversions to injectors
             einjwellx2(i)  = (xpatn(i) * (xpp2))               & !new active injectors
                            + (xpatn(i) * (xpp3))              !NEW converted injectors
!     &                      + (xpatn(i) * (1-xpp2))            !existing active injectors
             eshutinx2(i)   = (sump(i)-xpatn(i)) * (xpp1) &     !new producers SI
                            + (sump(i)-xpatn(i)) * (xpp2) &     !new injectors SI
!     &                      + (sump(i)-xpatn(i)) * (xpp3)     !Converted     SI
                            + (sump(i)-xpatn(i)) * (1-xpp2)    !existing injectors SI
!     &                      + (sump(i)-xpatn(i)) * (1)
            edryholex2(i) = patyr(i)* (xpp1+xpp2)* &
                            regdryKD(aregion(irs))                                  !4.16.09 REGDRY -> REGDRYKD
           end do
!
         ELSEIF (procde.eq.8) then
!            For Horizontal continuity use XPP1 = 2.0
!                                          XPP2 = 0.0
!                                          XPP3 = 2.0
!                                          XPP4 = 0.0
!            Assumes all producers at start of project
           do i=1,max_yr-1
             einjdrx2(i)    = (PATYR(i) * (0.0))               !New injectors drilled
             eproddrx2(i)   = (PATYR(i) * (2.0))               !New producers drilled
             eprodwellx2(i) = (xpatn(i) * (2.0))                & !new active producers
                            + (xpatn(i) * (2))                  & !existing active producers
                            - (xpatn(i) * (2.0))               !Less conversions to injectors
             einjwellx2(i)  = (xpatn(i) * (0))                  & !new active injectors
                            + (xpatn(i) * (2))                 !NEW converted injectors
!     &                      + (xpatn(i) * (1-xpp2))           !existing active injectors
             eshutinx2(i)   = (sump(i)-xpatn(i)) * (2)   &      !new producers SI
!     &                      + (sump(i)-xpatn(i)) * (xpp2)     !new injectors SI
!     &                      + (sump(i)-xpatn(i)) * (2)        !Converted     SI
!     &                      + (sump(i)-xpatn(i)) * (1-xpp2)   !existing injectors SI
                            + (sump(i)-xpatn(i)) * (2)
            edryholex2(i) = patyr(i)* (xpp1+xpp2)* &
                            regdryKD(aregion(irs))           !4.16.09 REGDRY -> REGDRYKD
           end do
!
!         ELSEIF (procde.eq.2.or.procde.eq.13) then            !4.16.09
         ELSEIF (PROCDE.EQ.2) THEN                             !4.16.09
             if (xpp2.eq.1.0) then      !Drill new injectors
                  do i=1,max_yr-1
                    einjdrx2(i)    = (PATYR(i) * (xpp2))         !New injectors drilled
                    eproddrx2(i)   = (PATYR(i) * (xpp1))         !New producers drilled
                    eprodwellx2(i) = (xpatn(i) * (1-xpp1))       !existing producers
                    einjwellx2(i)  = (xpatn(i) * (xpp2))         !new active injectors
!     &                             + (xpatn(i) * (1-xpp2))       !existing active injectors
                    eshutinx2(i)   = (sump(i)-xpatn(i)) * (xpp1)  & !new producers SI
                                   + (sump(i)-xpatn(i)) * (xpp2)  & !new injectors SI
                                   + (sump(i)-xpatn(i)) * (xpp3) !Converted     SI
                  end do
!
             ELSEIF (xpp3.eq.1.0) THEN  !convert existing producers to injectors
                  do i=1,max_yr-1
                    einjdrx2(i)    = (PATYR(i) * (xpp2))           !New injectors drilled
                    eproddrx2(i)   = (PATYR(i) * (xpp1))           !New producers drilled
                    eprodwellx2(i) = (xpatn(i) * (xpp1))            & !new active producers Drilled
                                   + (xpatn(i) * (1-xpp1))         !existing active producers
                    einjwellx2(i)  = (xpatn(i) * (xpp2+xpp3))      !new active injectors
                    eshutinx2(i)   = (sump(i)-xpatn(i)) * (xpp1)    & !new producers SI
                                   + (sump(i)-xpatn(i)) * (xpp2)    & !new injectors SI
                                   + (sump(i)-xpatn(i)) * (xpp3)    & !Converted     SI
                                   + (sump(i)-xpatn(i)) * (1-xpp1) !existing producers SI
                  end do
             end if
!
         else
           do i=1,MAX_yr-1
             einjdrx2(i)    = (PATYR(i) * (xpp2))           !New injectors drilled
             eproddrx2(i)   = (PATYR(i) * (xpp1))           !New producers drilled
             eprodwellx2(i) = (xpatn(i) * (xpp1))            & !new active producers
                            + (xpatn(i) * (1-xpp1))         !existing active producers
             einjwellx2(i)  = (xpatn(i) * (xpp2))            & !new active injectors Drilled
                            + (xpatn(i) * (xpp3))            & !new active injectors Converted
                            + (xpatn(i) * (1-xpp2-xpp3))    !existing active injectors
             eshutinx2(i)   = (sump(i)-xpatn(i)) * xpp1      & !new producers SI
                            + (sump(i)-xpatn(i)) * xpp2      & !new injectors SI
                            + (sump(i)-xpatn(i)) * xpp3     !conversions SI
!     &                      + (sump(i)-xpatn(i)) * (1-xpp2) !existing injectors SI
!     &                      + (sump(i)-xpatn(i)) * (1-xpp1) !existing producers SI
            edryholex2(i) = patyr(i)* (xpp1+xpp2)* &
                            regdryKD(aregion(irs))        !4.16.09 REGDRY -> REGDRYKD
           end do
         end if
!        IF (opt_Dbg) THEN
!         write (502,1502) resid
!         write (502,1503) 'NEW INJECTORS',(einjdrx2(i),i=1,max_yr-1)
!         WRITE (502,1503) 'NEW PRODUCERS',(eproddrx2(i),i=1,max_yr-1)
!         WRITE (502,1503) 'ACT PRODUCERS',(eprodwellx2(i),i=1,max_yr-1)
!         WRITE (502,1503) 'ACT INJECTORS',(einjwellx2(i),i=1,max_yr-1)
!         WRITE (502,1503) '    SHUTINS  ',(eshutinx2(i),i=1,max_yr-1)
!         write (502,*)
! 1502    format (a13)
! 1503    format (a13,<max_yr-1>(3x,f10.2))
!        END if



       RETURN
       END SUBROUTINE

!***************************************************************
!from ECON_INITOUT.FOR
!     Last change:  MC   18 Mar 2008    1:42 pm
        SUBROUTINE INIT_OUTARRAY(IRS)

        implicit none

!-----These includes are for the timing/exploration models
        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
       INCLUDE 'ogsmparm'
       include 'ogsmugr'

        INTEGER IC
        INTEGER IRS

        PROJECT_NPV(IRS) = 0.0
        RANKING_VAL(IRS) = 0.0
        EINVEFF(irs)     = 0.0

        do ic=1,max_yr
           eprodoil(IRS,ic) = 0.0
           eprodgas(IRS,ic) = 0.0
           eprodwat(IRS,ic) = 0.0
           ewatinj(IRS,ic)  = 0.0
           eco2pol(IRS,ic)  = 0.0
           esurfvol(IRS,ic) = 0.0
           eroy(ic)     = 0.0
           efedroy(ic)  = 0.0
           esev(ic)     = 0.0
           esttax(ic)   = 0.0
           efedtax(ic)  = 0.0
           efedcr(ic)   = 0.0
           egrossrev(ic)= 0.0
           egravadj(ic) = 0.0
           egaexp(ic)   = 0.0
           egacap(ic)   = 0.0
           eii(IRS,ic)      = 0.0
           eiidrl(IRS,ic)   = 0.0
           eicap2(ic)   = 0.0
           eintcap(ic)  = 0.0
           eti(IRS,ic)      = 0.0
           etidrl(IRS,ic)   = 0.0
           eotc(ic)     = 0.0
           etci(ic)     = 0.0
           edepr(ic)    = 0.0
           eamor(ic)    = 0.0
           enetrev(ic)  = 0.0
           etoc(IRS,ic)     = 0.0
           edeplet(ic)  = 0.0
           etaxinc(ic)  = 0.0
           eoam(ic)     = 0.0
           eaoam(ic)    = 0.0
           ela(IRS,ic)      = 0.0
           egg(ic)      = 0.0
           eeggla(ic)   = 0.0
           eatcf(ic)    = 0.0
           eDatcf(ic)    = 0.0
!           oilprice2(IC)     = 0.0
!           gasprice2(IC)     = 0.0
			
			ecatcf(ic) = 0.0
			estim(ic) = 0.0
			enibt(ic) = 0.0
			eamint(ic) = 0.0
			eniat(ic) = 0.0
			eadjgross(ic) = 0.0
			
		   eshutin(IRS,ic)   = 0.0
           eprodwell(IRS,ic) = 0.0
           eproddr(IRS,ic)   = 0.0
           einjdr(IRS,ic)    = 0.0
           einjwell(IRS,ic)  = 0.0
           eco2inj(IRS,ic)   = 0.0
           eco2rcy(IRS,ic)   = 0.0
           emarg(irs,ic)      = .false.
         end do

        END SUBROUTINE
!***************************************************************
!from econ_process.FOR
!     Last change:  MC   15 May 2008    2:23 pm
!**************************************************************************
! Program process.for

      SUBROUTINE process(n2,co2,h2s,ngl,totnet,gsamid,IR)
! ****************************************************************************
!    This program will calculate minimum costs for gas processing/treatment
!    based on various related inputs.  These costs will include revenue from
!    associated byproducts and will be expressed in $/MCF.
!
! References:
! (1) ``Business Characteristics of the Natural Gas Conditioning
!       Industry'', Topical Report (May 1993), Prepared by : C.C.
!       Tannehill and C. Galvin, Purvin & Gertz, Inc.,Prepared for:
!       The M. W. Kellogg Company
!       Gas Research Institute, Task 40,GRI Contract No. 5088-221-1753,
!
! (2)``Natural Gas Processing Costs: Cost Equations for Various Technologies,''
!      Reference: gascost.doc
! ****************************************************************************
!
      IMPLICIT NONE

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        INCLUDE 'ogsml48'
        INCLUDE 'ogsmout'
        INCLUDE 'mpblk'
        INCLUDE 'convfact'


      integer resnum,PREG,i,lhgh
      INTEGER IR

! gas impurities variables
      real n2,co2,h2o,h2s,ngl,denom,nglnumber,totnet98,totnet00,totnet15
      real n2_lim,co2_lim,h2o_lim,h2s_lim,ngl_lim
      real acid,by,co2cap,co2op,first,second
      real canusfactor,acidgas

! Explanation of capacity and thruput variables
!
! capsulf,capsu()  = capacity (long tons/day) for Claus Sulfur Recovery and
!                    Direct Conversion of H2S to Sulfur (Chelated Iron Process)
!
! capstrt,capst( ) = capacity (MMCFD) for NGL Straight Refrigeration
! capcryo,capcr( ) = capacity (MMCFD) for NGL Cryogenic Expander Plant
! captexas,capte( )= capacity (MMCFD) for all other plant types based on the
!                     averages for Texas (representative of the U.S.)
! thruput,thru()     = ngl thruput (used for all impurities)

      real capsulf, capsu(MAX_REG)
      real capstrt, capst(MAX_REG)
      real capcryo, capcr(MAX_REG)
      real captexas,capte(MAX_REG)
      real thruput,thru(MAX_REG)
      character*7 imp(5)

! plant and financial and regional variables
      real plt_lfe,dis_rate
      real op_factor,op_fac(MAX_REG)
      character*2     region(MAX_REG)
      character*11 gsamid

!     capital cost ($/MCF)
      real clccost,diccost,stccost,crccost
      real n2_ccst
      real co2_ccst
      real h2o_ccst
      real ngl_ccst

!     operating cost ($/MCF)
      real diocost,stocost,crocost
      real n2_ocst
      real co2_ocst
      real h2o_ocst
      real ngl_ocst

!     byproduct revenues ($/MCF)
      real byrevs(5), nglprice2

!     total values
      real totnet,totnet1,n2_cst,co2_cst,h2o_cst,ngl_cst

!     debugging variable
      character*4 type

      lhgh = 55
      totnet1 = 0.0
      totnet  = 0.0

! ******************* START OF MAIN PROGRAM **************************

!-ASHc capsulf,capsu()  = capacity (long tons/day) for Claus Sulfur Recovery and
!-ASHc                    Direct Conversion of H2S to Sulfur (Chelated Iron Process)
!-ASHc
!-ASHc capstrt,capst( ) = capacity (MMCFD) for NGL Straight Refrigeration
!-ASHc capcryo,capcr( ) = capacity (MMCFD) for NGL Cryogenic Expander Plant
!-ASHc captexas,capte( )= capacity (MMCFD) for all other plant types based on the
!-ASHc                     averages for Texas (representative of the U.S.)
!-ASH
!-ASHc nglnumber is fraction calculated based on ngl (bbl/MMCF)
!-ASH

       PREG = IR

       CAPST(PREG) = IMP_CAPST(PREG)
       CAPCR(PREG) = IMP_CAPCR(PREG)
       capte(PREG) = IMP_CAPTE(PREG)
       capsu(PREG) = IMP_CAPSU(PREG)
       thru(PREG)  = IMP_THRU(PREG)
       op_fac(PREG)= IMP_OP_FAC(PREG)


      nglnumber = (ngl*1512.0)/(10**6 + ngl*1512.0)
      denom = 1.0 - n2 - co2 - h2s - nglnumber

      if (denom.gt.0.0) then
         capst(PREG) = capst(PREG)/denom
         capcr(PREG) = capcr(PREG)/denom
         capte(PREG) = capte(PREG)/denom
         capsu(PREG) = capsu(PREG)/denom
      else
!!!        write(lhgh,34) gsamid,n2,co2,h2s,nglnumber
!!! 34     format(a11,2x,4(f7.3,1x),2x,'High Impurity')
      endif

! read in discount rate and plant life in years
! and the impurity limits  (see Ed Hardy for details on these values)

       dis_rate = imp_dis_rate
       plt_lfe  = imp_plt_lfe
       n2_lim   = imp_n2_lim
       co2_lim  = imp_co2_lim
       h2o_lim  = imp_h2o_lim
       h2s_lim  = imp_h2s_lim
       ngl_lim  = imp_ngl_lim



!       write(14,*)' '
!       write(14,*)'discount rate =',dis_rate*100,' %'
!       write(14,*)'plant life =', plt_lfe,' years'
!       write(14,*)'-----Impurity % Limits -----'
!       write(14,*)'n2_lim =',n2_lim*100,' %'
!       write(14,*)'co2_lim=',co2_lim*100,' %'
!       write(14,*)'h2o_lim=',h2o_lim*100,' %'
!       write(14,*)'h2s_lim=',h2s_lim*100,' %'
!       write(14,*)'ngl_lim=',ngl_lim,' Bbl/Mmcf'

      h2o= 0.00

!     if (h2s.gt.0.0) then
!      write(14,*)'----- Impurity percentages -----'
!      write(14,*)'gsamid =',gsamid
!      write(14,*)'n2 =',n2*100,' %'
!      write(14,*)'co2=',co2*100,' %'
!      write(14,*)'h2o=',h2o*100,' %'
!      write(14,*)'h2s=',h2s*100,' %'
!      write(14,*)'ngl=',ngl,' Bbl/Mmcf'
!     endif

!     write(*,*)'Processing GSAMID ',gsamid

          capstrt  = capst(PREG)
          capcryo  = capcr(PREG)
          captexas = capte(PREG)
          capsulf  = capsu(PREG)
          thruput  = thru(PREG)
          op_factor = op_fac(PREG)

! calculate the acid gas concentration (%)
      acid = h2s+co2


! calculate total gas process cost
! where gas process cost= captial cost + operating cost - byproduct revenues


! ucc = unitized capital costs ($/MCF)

! the capcosts,opcost, and byrevs vectors are as follows:
! element #1 = n2
! element #2 = co2_+_h2s
! element #3 = h2o
! element #4 = ngl

! capcosts = capital costs ($/MCF)
      call cap_cost(capstrt,capcryo,captexas,capsulf,acid, &
                    n2,n2_lim, &
                    co2,co2_lim,h2s,h2s_lim, &
                    h2o,h2o_lim, &
                    ngl,ngl_lim, &
                    dis_rate,plt_lfe,op_factor, &
                    thruput, &
                    clccost,diccost,stccost,crccost, &
                    n2_ccst,co2_ccst,h2o_ccst,ngl_ccst)

! operating costs ($/MCF)
      call op_cost(capstrt,capcryo,captexas,capsulf,thruput, &
                   acid, &
                   n2,n2_lim, &
                   co2,co2_lim,h2s,h2s_lim, &
                   h2o,h2o_lim, &
                   ngl,ngl_lim, &
                   diocost,stocost,crocost, &
                   n2_ocst,co2_ocst,ngl_ocst,h2o_ocst)
!

! byrevs = byproduct revenues ($/MCF)
!     nglprice2 = nglprice * (oit_wop(curiyr,1)/45.)**(0.25)
!     nglprice2 = (nglprice * oit_wop(curiyr,1)*mc_jpgdp(20)/mc_jpgdp(-2) + 5.)/42.  ! convert to $/gal 
      nglprice2 = (plginpf(11,curiyr)*cflgq(curiyr))/42.   ! convert to $/gal 
!     write(6,*) 'dh5out', curiyr+1989, oit_wop(curiyr,1), nglprice2, ngl
      call by_rev(byrevs,h2s,co2,ngl,co2price,h2sprice,nglprice2,n2price)

      imp(1) ='n2'
      imp(2) ='co2+h2s'
      imp(3) ='h2o'
      imp(4) ='ngl'
      imp(5) ='TOTAL'

      n2_cst = n2_ocst/denom+n2_ccst

      co2_cst= co2_ccst+co2_ocst/denom
      if ((co2.le.co2_lim).and.(h2s.gt.h2s_lim)) then
        first = co2_ccst+co2_ocst/denom
        second = diccost+diocost/denom
        if (first.le.second) then
         co2_cst = first
         co2cap = co2_ccst
         co2op = co2_ocst/denom
        else
         co2_cst = second
         co2cap = diccost
         co2op = diocost/denom
        endif

          if(co2_cst.eq.diccost+diocost/denom) then
                co2_ocst = diocost/denom
                co2_ccst = diccost
          end if

      end if
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      h2o_cst = h2o_ocst+h2o_ccst

!     if (ngl.gt.ngl_lim) then
!        ngl_cst = amin1(stccost+stocost,crccost+crocost)
!     end if

! ***********************************************
          if ((stccost+stocost).gt.(crccost+crocost)) then
            ngl_ocst = crocost
            ngl_ccst = crccost
            type = 'cryo'
          else
            ngl_ocst = stocost
            ngl_ccst = stccost
            type = 'strt'
          end if
          ngl_cst = ngl_ocst/denom+ngl_ccst
!***********************************************

!  if byrevs(5) :: Right now it is just NGL revenue, :: is greater than
!  1.3*ngl_cst then byrevs(5) = 1.3*ngl_cst

!     if (byrevs(5).gt.1.3*ngl_cst) byrevs(5) = 1.3*ngl_cst
      by = byrevs(5)

!**************************************************************************
! processing cost calculation procedure

          totnet = n2_cst+co2_cst+h2o_cst+ngl_cst-byrevs(5)
          totnet1 = totnet

 900  format(a2,t12,f7.3,t20,f7.3,t28,f7.3,t36,f7.3,t44,f7.3,t52,f4.2)
 901  format(a2,t12,f5.3,t18,f5.3,t24,f5.3,t30,f5.3,t36,f5.3)
 902  format(f4.2,1x,f4.1,1x,f4.2,1x,f4.2,1x,f4.2,1x,f9.7,1x,f5.3)
 903  format(1x,'gsamid: ',a11,1x,f10.3)
 905  format(a8,t8, a11,t22,a11,t35,a13,t55,a3)
 910  format(a7,t10,f10.4,t20,f10.4,t35,f10.4,t50,f10.4)

      RETURN
      END
!
!***********************************************************************
!***********************************************************************
!***********************************************************************
!
      SUBROUTINE by_rev(byrevs,h2s,co2,ngl,co2p,h2sp,nglp,n2p)
      IMPLICIT NONE
      real byrevs(5),byp_rev,h2s,co2,ngl
      real co2p,h2sp,nglp,n2p,oilp
      real co2_rev,h2s_rev,h2o_rev,ngl_rev,n2_rev

! By-product revenues were obtained from Bob Baron who looked them
! up in a Purvin & Gertz Topical Report

! By-product revenues:
! co2:  $0.65/MCF
! h2s:  $130/metric ton
! h2o:  $0/MCF
! ngl:  $0.30/gallon
!  formerly set at: n2 :  $1.30/MCF
! n2 :  $0/MCF

! conversion formulae for sulfur (h2s by-product)
! 1 metric ton = 1 long ton*1.016
!
! 1 long ton = 0.0246 MMCF Based on the following
! 1 mole of H2S at Std. Cond. Occupies 379.4 SCF
! Molecular Weight of H2S = 34
!
! 1 long ton = 2200 lb
! 2200 lb = 2200/34 mole of H2S = 64.71
! So, 64.71 moles of H2S (or 1 long ton) is 379.4*64.71/10**6 = 0.0246 MMCF
!
! $/MCF =[$/metric ton]*[metric ton/MCF]
!       =[$/(long ton*1.016)]*[1.016*long ton/MCF]
!       =[$/long ton]*[long ton/MCF]
!       =[$/long ton]*[0.040650]

! conversion formulae for ngl
!   0.30 $/Gallon :: price for NGL
!   ngl value in Bbl/MMCF
!   1 Bbl = 42.0 Gallon
!
!   $/MCF   = [$/Gallon] [Bbl/MMCF] [Gallon/Bbl] [MMCF/MCF]
!   ngl_rev =   0.30       ngl         42         1/1000
! need to  multiply by the fraction that n2 and h2s represent for the entire
! inlet gas

!     co2_rev = co2p*co2
!     h2s_rev = h2sp*0.04065000*h2s
      co2_rev = 0.000
      h2s_rev = 0.000
      h2o_rev = 0.00

! cap the ngl value at 85 bbl/MMCF per request of B. Baron & A. Becker 3-20-96
!     if (ngl.gt.85.0) then
!       ngl_rev = nglp*85.0*42.0/1000.00
!     else
        ngl_rev = nglp*ngl*42.0/1000.00
!     end if

      n2_rev  = 0.00

! compute the total by-product revenue in $/MCF
      byp_rev = n2_rev+co2_rev+h2s_rev+h2o_rev+ngl_rev

! update byrevs
      byrevs(1) = n2_rev
      byrevs(2) = co2_rev+h2s_rev
      byrevs(3) = h2o_rev
      byrevs(4) = ngl_rev
      byrevs(5) = byp_rev

      return
      end

! *********************************************************************
      SUBROUTINE calc_ucc(cost,capacity,dis_rate,plt_lfe, &
                          op_factor)

      IMPLICIT NONE
      real million, thousand
      parameter (million=1000000.00,thousand=1000.00)

      real cost,capacity,dis_rate,plt_lfe,op_factor
      real ac,avt

! input cost = capital costs ($10**6)
! ac   = annual capital charge ($10**6/year)
! output cost = unitized capital cost ($/MCF)
! avt  = average volume thoughput ($10**6 cubic feet/day) (calendar basis)
! Note: capacity is expressed in MMCF/day)

      ac  = cost*dis_rate/(1.00-exp(-dis_rate*plt_lfe))
      avt = capacity*365.00*op_factor
      cost = ac*(million/thousand)/(avt)
      return
      end

! *********************************************************************
      SUBROUTINE cap_cost(capstrt,capcryo,captexas,capsulf,acid, &
                          n2,n2_lim, &
                          co2,co2_lim,h2s,h2s_lim, &
                          h2o,h2o_lim, &
                          ngl,ngl_lim, &
                         dis_rate,plt_lfe,op_factor, &
                         thruput, &
                         clcost,dicost,stcost,crcost, &
                         n2_cst,co2_cst,h2o_cst,ngl_cst)

      IMPLICIT NONE

      real capstrt,capcryo,captexas,capsulf
      real acid,clcost,dicost,stcost,crcost
      real n2,n2_lim,n2_cst
      real co2,co2_lim,h2s,h2s_lim,co2_cst
      real h2o,h2o_lim,h2o_cst
      real ngl,ngl_lim,ngl_cst,thruput,capacity
      real dis_rate,op_factor,plt_lfe

! calculate costs in 10**6 dollars (see reference (2))
! then convert to $/MCF via subroutine calc_ucc
! which assumes that the capacity is in MMCF/day

! costs for each impurity will be generated only if impurity %'s are above
! associated limits

! initialize costs
      n2_cst  = 0.00
      co2_cst = 0.00
      h2o_cst = 0.00
      ngl_cst = 0.00
      dicost  = 0.0
      stcost  = 0.0
      crcost  = 0.0

! *** N2 ***
! Nitrogen Rejection Plant: Capital Costs
      if (n2.gt.n2_lim) then
        call nitrocc(n2_cst,captexas)
        call calc_ucc(n2_cst,captexas,dis_rate,plt_lfe,op_factor)
!       write(14,1000)n2_cst
 1000   format(' n2_cst in $/MCF',f10.5)
      end if

! *** CO2 + H2S ***
! cost for both is stored in co2_cst
! subroutine `deatrtcc' is DEA Treating Plant: Capital Costs
! subroutine `clauscc' is Claus Sulfur Recovery Plant: Capital Costs
! subroutine `directcc' is Direct Conversion of H_2S to Sulfur
! (Chelated Iron Process): Capital Costs

           if ((co2.gt.co2_lim).and.(h2s.le.h2s_lim)) then
             call deatrtcc(acid,captexas,co2_cst)
             call calc_ucc(co2_cst,captexas,dis_rate,plt_lfe,op_factor)
             if (h2s.gt.0.000) then
!            write(14,1001) co2_cst
! 1001        format (' deatrt cap cost in $/MCF',f10.5)
             endif
      else if ((co2.gt.co2_lim).and.(h2s.gt.h2s_lim)) then
             call deatrtcc(acid,captexas,co2_cst)
             call calc_ucc(co2_cst,captexas,dis_rate,plt_lfe,op_factor)
             if (h2s.gt.0.000) then
!            write(14,1001) co2_cst
             endif
             call clauscc(capsulf,clcost)
! first convert capsulf from long tons to MMCF
! we have 1 long ton of sulfur = 0.0246 MMCF
! also want capacity for the inlet gas, not just h2s so we must
! divide by the h2s %

             capacity = (capsulf/h2s)*0.024600
             call calc_ucc(clcost,capacity,dis_rate,plt_lfe,op_factor)
             if (h2s.gt.0.0) then
!            write(14,1002) clcost
             endif
 1002        format(' claus cap cost in $/MCF',f10.5)
             co2_cst = co2_cst + clcost
!            if (h2s.gt.0.000)
!    $       print *, 'After Adding Direct and Claus', co2_cst

      else if ((co2.le.co2_lim).and.(h2s.gt.h2s_lim)) then
             call deatrtcc(acid,captexas,co2_cst)
             call calc_ucc(co2_cst,captexas,dis_rate,plt_lfe,op_factor)
 & !            if (h2s.gt.0.000)
!            write(14,1001) co2_cst
             call clauscc(capsulf,clcost)
             capacity = (capsulf/h2s)*0.0246000
             call calc_ucc(clcost,capacity,dis_rate,plt_lfe,op_factor)
 & !            if (h2s.gt.0.000)
!            write(14,1002) clcost
             co2_cst = co2_cst + clcost

             call directcc(capsulf,dicost)
             capacity = (capsulf/h2s)*0.0246000
!            if (h2s.gt.0.000)
!    $       write(14,*)'direct capacity just before ucc=',capacity
             call calc_ucc(dicost,capacity,dis_rate,plt_lfe,op_factor)

!            if (h2s.gt.0.000)
!    $       write(14,1003)dicost
 1003        format(' direct cost in $/MCF',f10.5)

      else
! both co2 and h2s are under the prescribed limits
             co2_cst = 0.00
      end if



! *** H2O ***
! Glycol Dehydration: Capital Costs
      if (h2o.gt.h2o_lim) then
        call glycolcc(h2o_cst,captexas)
        call calc_ucc(h2o_cst,captexas,dis_rate,plt_lfe,op_factor)
!       write(14,1004)h2o_cst
 1004   format(' glycol cost in $/MCF',f10.5)

      end if

! *** NGL ***
! subroutine `strtcc' is Straight Refrigeration Plant: Capital Costs
! subroutine `cryocc' is Cryogenic Expander Plant: Capital Costs
      if (ngl.gt.ngl_lim) then
        call strtcc(capstrt,stcost,ngl,thruput)
        call calc_ucc(stcost,capstrt,dis_rate,plt_lfe,op_factor)
!       write(14,1005) stcost
        call cryocc(capcryo,crcost)
        call calc_ucc(crcost,capcryo,dis_rate,plt_lfe,op_factor)
!       write(14,1006)crcost
 1005   format(' straight cost in $/MCF', f10.5)
 1006   format(' cryogenic cost in $/MCF', f10.5)
      end if



      return
      end

! *********************************************************************
      SUBROUTINE check1(I)

      IMPLICIT NONE

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
      INCLUDE 'ogsml48'
      INTEGER I

      if (IMP_capst(I).lt.0.00) then
       write(*,*)'Inlet capacity for region ',IMP_REG(I),' is incorrect'
        write(*,*)'Program halted'
        stop
      end if

      if (IMP_capcr(I).lt.0.00) then
       write(*,*)'Inlet capacity for region ',IMP_REG(I),' is incorrect'
        write(*,*)'Program halted'
        stop
      end if

      if (IMP_capte(I).lt.0.00) then
       write(*,*)'Inlet capacity for region ',IMP_REG(I),' is incorrect'
        write(*,*)'Program halted'
        stop
      end if

      if (IMP_capsu(I).lt.0.00) then
       write(*,*)'Inlet capacity for region ',IMP_REG(I),' is incorrect'
        write(*,*)'Program halted'
        stop
      end if

      if (IMP_thru(I).lt.0.00) then
        write(*,*)'Gas thruput for region ',IMP_REG(I),' is incorrect'
        write(*,*)'Program halted'
        stop
      end if

      if ((IMP_op_fac(I).lt.0.00).or.(IMP_op_fac(I).gt.1.00)) then
      write(*,*)'Operating factor for region ',IMP_REG(I),'is incorrect'
        write(*,*)'Program halted'
        stop
      end if

      return
      end
! *********************************************************************
      SUBROUTINE check2(resnum,n2,co2,h2o,h2s,ngl)
      IMPLICIT NONE
      integer resnum
      real n2,co2,h2o,h2s,ngl


      if ((n2.gt.1.00).or.(n2.lt.0.00)) then
        write(*,*)'Incorrect % for N_2 value in reservoir # ',resnum
        write(*,*)'Program halted'
        stop
      end if

      if ((co2.gt.1.00).or.(co2.lt.0.00)) then
        write(*,*)'Incorrect % for CO_2 value in reservoir # ',resnum
        write(*,*)'Program halted'
        stop
      end if


      if ((h2o.gt.1.00).or.(h2o.lt.0.00)) then
        write(*,*)'Incorrect % for H_2O value in reservoir # ',resnum
        write(*,*)'Program halted'
        stop
      end if


      if ((h2s.gt.1.00).or.(h2s.lt.0.00)) then
        write(*,*)'Incorrect % for H_2S value in reservoir # ',resnum
        write(*,*)'Program halted'
        stop
      end if


      if (ngl.lt.0.00) then
        write(*,*)'Incorrect % for NGL value in reservoir # ',resnum
        write(*,*)'Program halted'
        stop
      end if

      return
      end
! *********************************************************************
      SUBROUTINE check3(dis_rate,plt_lfe,n2_lim,co2_lim,h2o_lim, &
                        h2s_lim,ngl_lim)
      IMPLICIT NONE
      real plt_lfe
      real dis_rate,n2_lim,co2_lim,h2o_lim,h2s_lim,ngl_lim

      if ((dis_rate.gt.1.00).or.(dis_rate.le.0.00)) then
        write(*,*)'Discount rate =', dis_rate, 'incorrect data'
        write(*,*)'Program halted'
        stop
      end if


      if ((plt_lfe.lt.0.00)) then
        write(*,*)'Plant life =', plt_lfe, 'incorrect data'
        write(*,*)'Program halted'
        stop
      end if


      if ((n2_lim.gt.1.00).or.(n2_lim.le.0.00)) then
        write(*,*)'N2 limit =', n2_lim, 'incorrect data'
        write(*,*)'Program halted'
        stop
      end if


      if ((co2_lim.gt.1.00).or.(co2_lim.le.0.00)) then
        write(*,*)'CO2 limit =', co2_lim, 'incorrect data'
        write(*,*)'Program halted'
        stop
      end if

      if ((h2o_lim.gt.1.00).or.(h2o_lim.le.0.00)) then
        write(*,*)'H2O limit =', h2o_lim, 'incorrect data'
        write(*,*)'Program halted'
        stop
      end if

      if ((h2s_lim.gt.1.00).or.(h2s_lim.le.0.00)) then
        write(*,*)'H2S limit =', h2s_lim, 'incorrect data'
        write(*,*)'Program halted'
        stop
      end if

      if (ngl_lim.lt.0.00) then
        write(*,*)'NGL limit =', ngl_lim, 'incorrect data'
        write(*,*)'Program halted'
        stop
      end if


      return
      end

! *********************************************************************
      SUBROUTINE clauscc(caplong,clcost)
      IMPLICIT NONE
      real caplong,clcost

! to minimize costs,
! if caplong .le. 30 long tons/day choose 2 stage with CBA
! o/w                              choose 3 stage with Scot Tail Gas Unit

! Note: the equations below were updated and shown on the
! gascost.doc printed for 1/16/96
      if (caplong.le.30.00) then
        clcost = exp(0.572400*alog(caplong)-0.836800)
      else
        clcost = exp(0.754200*alog(caplong)-1.3122000)
      end if


!       write(14,*)' '
!       write(14,*)'Claus capital costs******'
!       write(14,*)'costs in $10**6                  =',clcost
!       write(14,*)'capacity                         =',caplong
!       write(14,*)' '
!
      return
      end

! *********************************************************************
      SUBROUTINE clausoc(caplong,clcost,h2s)
      IMPLICIT NONE
      real caplong,clcost,h2s

        clcost = exp(6.021000*exp(-0.165600*alog(caplong)))

!       write(14,*)' '
!       write(14,*)'***Claus operating costs***'
!       write(14,*)'costs in $/long ton             =',clcost
!       write(14,*)'capacity                        =',caplong
!       write(14,*)' '

! costs are in $/Long Ton, we will need to convert to $/MCF
!
!  2240 lbs/LongTon
!  2.205 lbs/kg
!  1000 g/kg
!  34.076 g/mol H2S
!  22.4 L/mol @stp
!  28.317 L/cu.ft.
!  1000 cu.ft./MCF
!  h2s/(1-h2s) converts from h2s basis to ch4 basis

      clcost = clcost/2240*2.205*34.076/22.4*28.3169*h2s/(1-h2s)
!     write(14,*)'claus op cost $/MCF=',clcost
      return
      end

! *********************************************************************
      SUBROUTINE cryocc(capinlet,crcost)
      real capinlet, crcost

! Note:
! According to Bob Baron, the costs for capinlet < 3
! can be taken to be the same as those in the range (3,100)
! Also, for above 1000, the costs will be the same for those
! in the range (100,1000)

      if (capinlet.lt.100.00) then
        crcost = exp(0.484200*alog(capinlet)-0.096200)
      else
        crcost = exp(0.989300*alog(capinlet)-2.376900)
      end if



!       write(14,*)' '
!       write(14,*)'***Cryogenic Exp. capital costs***'
!       write(*,*)'Cryo Cap costs in $10**6                  =',crcost
!       write(14,*)'capacity                         =',capinlet
!       write(14,*)' '


      return
      end

! *********************************************************************
      SUBROUTINE cryooc(capinlet,crcost)
      IMPLICIT NONE
      real capinlet, crcost


! costs are in cents/MCF, need to convert to $/MCF
! to minimize costs, perform the following calculations
           if (capinlet.lt.30.00) then
             crcost = exp(2.738000*exp(-0.253400*alog(capinlet)))
           else
             crcost = exp(4.341700*exp(-0.246300*alog(capinlet)))
           end if


!       write(14,*)' '
!       write(14,*)'***Cryogenic Exp. operating costs***'
!       write(*,*)'Cryo Op Costs in cents/MCF           =',crcost
!       write(14,*)'capacity                     =',capinlet
!       write(14,*)' '


! convert to $/MCF
      crcost = crcost/100.00
      return
      end

! *********************************************************************
      SUBROUTINE deatrtcc(acid,capinlet,co2_cst)
      IMPLICIT NONE
      real acid,capinlet,co2_cst,weightb
      real cost1,cost2p5,cost5,cost10,cost20


! acid values
! 1%, 2.5%, 5%, 10%, 20%
! costs are really alog(costs)
! note that costs are calculated in dlog-dlog space and then interpolated
! before exponentiating
! this is because in log-log space the relationships are linear
! and the weighting scheme makes sense there

        cost1   = (0.643000*alog(capinlet)-2.690000)
        cost2p5 = (0.600900*alog(capinlet)-1.843700)
        cost5   = (0.635000*alog(capinlet)-1.645800)
        cost10  = (0.632500*alog(capinlet)-1.169000)
        cost20  = (0.624400*alog(capinlet)-0.739600)

! interpolate between these values to recover the appropriate costs

             if (acid.le.0.0100)  then
               weightb = acid/0.0100
               call wt_cst(weightb,0.00,cost1,co2_cst)

        else if (acid.le.0.02500) then
               weightb = (acid-0.0100)/(0.02500-0.0100)
               call wt_cst(weightb,cost1,cost2p5,co2_cst)

        else if (acid.le.0.0500)  then
               weightb = (acid-0.02500)/(0.0500-0.02500)
               call wt_cst(weightb,cost2p5,cost5,co2_cst)

        else if (acid.le.0.1000)  then
               weightb = (acid-0.0500)/(0.1000-0.0500)
               call wt_cst(weightb,cost5,cost10,co2_cst)

        else if (acid.le.0.2000)  then
               weightb = (acid-0.1000)/(0.2000-0.1000)
               call wt_cst(weightb,cost10,cost20,co2_cst)

        else
! According to Bob Baron, if acid > 20%, just take the cost for the 20% level.
               co2_cst = cost20
        end if

! exponentiate costs to get them in `normal' (not log-log) space
        co2_cst =exp(co2_cst)

!       write(14,*)' '
!       write(14,*)'***DEA treatment, capital cost check***'
!       write(14,*)'capital costs in $10**6=',co2_cst
!       write(14,*)'acid percentage        =',acid*100.0
!       write(14,*)'capacity               =',capinlet
!       write(14,*)' '

      return
      end


! *********************************************************************
      SUBROUTINE deatrtoc(acid,thru,co2_cst)
      IMPLICIT NONE
      real acid,thru,co2_cst,weightb
      real cost1,cost2p5,cost5,cost10,cost20


! costs in $/MCF

! calculate the various costs
! acid values
! 1%,  5%, 10%, 20%
! costs are really alog(costs)
! do the weighting in log-log space since the relationships are
! linear there, then exponentiate afterwards

        cost1   = (-0.354400*alog(thru)-2.483400)
        cost5   = (-0.312600*alog(thru)-2.397100)
        cost10  = (-0.274500*alog(thru)-2.242500)
        cost20  = (-0.176000*alog(thru)-2.114100)

! interpolate between these values to recover the appropriate costs

             if (acid.le.0.0100)  then
               weightb = acid/0.0100
               call wt_cst(weightb,0.00,cost1,co2_cst)

        else if (acid.le.0.0500)  then
               weightb = (acid-0.0100)/(0.0500-0.0100)
               call wt_cst(weightb,cost1,cost5,co2_cst)

        else if (acid.le.0.1000)  then
               weightb = (acid-0.0500)/(0.1000-0.0500)
               call wt_cst(weightb,cost5,cost10,co2_cst)

        else if (acid.le.0.2000)  then
               weightb = (acid-0.1000)/(0.2000-0.1000)
               call wt_cst(weightb,cost10,cost20,co2_cst)

        else
               co2_cst = cost20
        end if
        co2_cst = exp(co2_cst)


!       write(14,*)' '
!       write(14,*)'***DEA treatment, operating costs check***'
!       write(14,*)'costs in $/MCF inlet gas         =',co2_cst
!       write(14,*)'acid percentage                  =',acid*100.0
!       write(14,*)'thruput                          =',thru
!       write(14,*)' '
!

      return
      end

! *********************************************************************
      SUBROUTINE directcc(caplong,dicost)
      IMPLICIT NONE
      real caplong,dicost

      dicost = exp(1.170700*alog(caplong)-1.199900)
!      write(*,*)'inside directcc,caplong,dicost=',caplong,dicost

!       write(14,*)' '
!       write(14,*)'***Direct Conversion of h2s capital costs***'
!       write(14,*)'costs in $10**6                  =',dicost
!       write(14,*)'capacity                         =',caplong
!       write(14,*)' '
!

      return
      end


! *********************************************************************
      SUBROUTINE directoc(caplong,dicost,h2s)
      IMPLICIT NONE
      real caplong,dicost,h2s

! the equations were updated and shown on the document
! gascost.doc dated 1/16/96

       dicost = exp(-0.315600*alog(caplong)+6.302400)


!       write(14,*)' '
!       write(14,*)'*** Direct conversion of h2s operating costs***'
!       write(14,*)'costs in $/long ton              =',dicost
!       write(14,*)'capacity                         =',caplong
!       write(14,*)' '


! costs are in $/Long Ton, we will need to convert to $/MCF
!
!
      dicost = dicost/2240*2.205*34.076/22.4*28.3169*h2s/(1-h2s)
!      write(*,*)'inside directoc,caplong,dicost=',caplong,dicost
!     write(14,*)'direct costs in $/MCF in directoc.f:',dicost
      return
      end

! *********************************************************************
      SUBROUTINE op_cost(capstrt,capcryo,captexas,capsulf,thruput, &
                         acid, &
                         n2,n2_lim, &
                         co2,co2_lim,h2s,h2s_lim, &
                         h2o,h2o_lim, &
                         ngl,ngl_lim, &
                         dicost,stcost,crcost, &
                         n2_cst,co2_cst,ngl_cst,h2o_cst)



      IMPLICIT NONE

      real capstrt,capcryo,captexas,capsulf
      real thruput,acid
      real n2,n2_lim,n2_cst
      real co2,co2_lim,h2s,h2s_lim,co2_cst
      real h2o,h2o_lim,h2o_cst
      real ngl,ngl_lim,ngl_cst
      real clcost,dicost,stcost,crcost

! calculate operating costs in $/MCF (these are the units unless
! specified otherwise)

! if impurity is above acceptable limit, calculate costs using the
! suggested technology as given in reference (2)

      n2_cst  = 0.00
      co2_cst = 0.00
      h2o_cst = 0.00
      ngl_cst = 0.00
      stcost  = 0.0
      crcost  = 0.0

!       write(14,*)'initialize co2_cst, value=',co2_cst
! *** N2 ***
! Nitrogen Rejection Plant: Operating Costs
! costs are in cents/MCF
! equation updated on 1/16/96

      if (n2.gt.n2_lim) then
        call nitrooc(n2_cst,captexas)
!       convert to $/MCF
        n2_cst = n2_cst/100.00

! take into account fuel costs (approx. equal to operating costs)
        n2_cst = n2_cst*2.0
      end if

! *** CO2 + H2S ***
! cost for both is stored in co2_cst
! subroutine `deatrtoc' is DEA Treating Plant: Operating Costs
! subroutine `clausoc' is Claus Sulfur Recovery Plant: Operating Costs
! subroutine `directoc' is Direct Conversion of H_2S to Sulfur
! (Chelated Iron Process): Operating Costs

           if ((co2.gt.co2_lim).and.(h2s.le.h2s_lim)) then
             call deatrtoc(acid,thruput,co2_cst)

! take into account fuel costs (approx. equal to operating costs)
             co2_cst = co2_cst*2.0

      else if ((co2.gt.co2_lim).and.(h2s.gt.h2s_lim)) then
             call deatrtoc(acid,thruput,co2_cst)
             call clausoc(capsulf,clcost,h2s)
             co2_cst = co2_cst + clcost

! take into account fuel costs (approx. equal to operating costs)
             co2_cst = co2_cst*2.0

      else if ((co2.le.co2_lim).and.(h2s.gt.h2s_lim)) then
             call deatrtoc(acid,thruput,co2_cst)
             call clausoc(capsulf,clcost,h2s)
             co2_cst = co2_cst + clcost
             call directoc(capsulf,dicost,h2s)

! take into account fuel costs (approx. equal to operating costs)
             co2_cst = co2_cst*2.0
             dicost  = dicost*2.0
      else
! both co2 and h2s are under the prescribed limits
             co2_cst = 0.00
      end if

! *** H2O ***
! Glycol Dehydration: Operating Costs
! According to Bob Baron, use the 800 psia functional relationship
      if (h2o.gt.h2o_lim) then
        call glycoloc(h2o_cst,captexas)
! take into account fuel costs (approx. equal to operating costs)
        h2o_cst = h2o_cst*2.0
      end if

! *** NGL ***
! subroutine `strtoc' is Straight Refrigeration Plant: Operating Costs
! subroutine `cryooc' is Cryogenic Expander Plant: Operating Costs
      if (ngl.gt.ngl_lim) then
        call strtoc(capstrt,stcost,ngl,thruput)
        call cryooc(capcryo,crcost)

! take into account fuel costs (approx. equal to operating costs)
        stcost = stcost * 2.0
        crcost = crcost * 2.0
      end if
      return
      end
! *********************************************************************
      SUBROUTINE strtcc(capinlet,stcost,ngl,thruput)
      IMPLICIT NONE
      real capinlet,stcost,stcost6,stcost3,stcost1,stcost12,stcost24, &
        stcost48,stcost96,stcost192
      real ngl,thruput,gpm,weightb

! use costs in log-log space as opposed to normal space
!       then convert back later
! Cost eqns for 12,24,48,96,192 gpm were developed by using the
!       difference in the natural logs of the capital costs
!       from 3 to 6 gpm and applying this difference to create the
!       12,24,48,96,192 gpm cases
!   The difference was adjusted between each case by the same
!       ratio that the difference changed from between the 1.5
!       minus 3.0 vs. the 6.0 minus 3.0 case

! 192.0 GPM C3+
      stcost192 = (1.4396*alog(capinlet)-1.0789)

! 96.0 GPM C3+
      stcost96 = (1.194*alog(capinlet)-0.8611)

! 48.0 GPM C3+
      stcost48 = (0.9868*alog(capinlet)-0.7031)

! 24.0 GPM C3+
      stcost24 = (0.8139*alog(capinlet)-0.6011)

! 12.0 GPM C3+
      stcost12 = (0.6724*alog(capinlet)-0.5536)

! 6.0 GPM C3+  (Rich)
      stcost6 = (0.561200*alog(capinlet)-0.562800)

! 3.0 GPM C3+ (Medium)
      stcost3 = (0.481300*alog(capinlet)-0.636600)

! 1.5 GPM C3+ (Lean)
      stcost1 = (0.437900*alog(capinlet)-0.792900)

!cbs  write(*,*)'stcost1   = ',stcost1
! calculate the gallons per minute (GPM) and interpolate between the three
! costs given above
! for NGL (natural gas liquids) we have:
!
! GPM = thruput*ngl*42.0/24.00/60.00
!
! where thruput = gas thruput (MMCFD)
!       ngl    = ngl in BBl/MMCF


! cap ngl at 85 bbl/MMCF per request of B. Baron and A. Becker
!     if (ngl.gt.85.0) then
!       gpm = thruput*85.0*42.0/24.00/60.00
!     else
        gpm = thruput*ngl*42.0/24.00/60.00
!     end if
!cbs****************************
!cbs    write(*,*)'gpm    =',gpm
!*************************************

! interpolate the costs based on the gpm value
           if (gpm.le.1.500) then
              weightb = gpm/1.500
              call wt_cst(weightb,0.00,stcost1,stcost)
      else if (gpm.le.3.00) then
              weightb =  (gpm-1.500)/(3.00-1.500)
              call wt_cst(weightb,stcost1,stcost3,stcost)
      else if (gpm.le.6.00) then
              weightb = (gpm-3.00)/(6.00-3.00)
              call wt_cst(weightb,stcost3,stcost6,stcost)
      else if (gpm.le.12.00) then
              weightb = (gpm-6.00)/(12.00-6.00)
              call wt_cst(weightb,stcost6,stcost12,stcost)
      else if (gpm.le.24.00) then
              weightb = (gpm-12.00)/(24.00-12.00)
              call wt_cst(weightb,stcost12,stcost24,stcost)
      else if (gpm.le.48.00) then
              weightb = (gpm-24.00)/(48.00-24.00)
              call wt_cst(weightb,stcost24,stcost48,stcost)
      else if (gpm.le.96.00) then
              weightb = (gpm-48.00)/(96.00-48.00)
              call wt_cst(weightb,stcost48,stcost96,stcost)
      else if (gpm.le.192.00) then
              weightb = (gpm-96.00)/(192.00-96.00)
              call wt_cst(weightb,stcost96,stcost192,stcost)
      else
              stcost = stcost192
      end if

! exponentiate cost
      stcost = exp(stcost)


!       write(14,*)' '
!       write(14,*)'*** Straight Ref. Capital Costs***'
!       write(14,*)'stcost  is                  =',stcost
!       write(14,*)'GPM of flow                      =',gpm
!       write(14,*)'capacity                         =',capinlet
!       write(14,*)' '


      return
      end


! *********************************************************************
      SUBROUTINE strtoc(capinlet,stcost,ngl,thruput)
      IMPLICIT NONE
      real capinlet, stcost,stcost6,stcost3,stcost1,stcost12,stcost24, &
        stcost48,stcost96,stcost192
      real ngl,thruput,gpm,weightb

! 192 GPM C3+
        if (capinlet.le.6.00) then
        stcost192 = 0.3667*capinlet**(-.5809)
        else
        stcost192 = 0.2317*capinlet**(-0.3116)
        end if
! 96 GPM C3+
        if (capinlet.le.6.00) then
        stcost96 = 0.3406*capinlet**(-0.5761)
        else
        stcost96 = 0.2177*capinlet**(-0.3134)

        end if
! 48 GPM C3+
        if (capinlet.le.6.00) then
        stcost48 = 0.3144*capinlet**(-0.5706)
        else
        stcost48 = 0.2038*capinlet**(-0.3155)
        end if

! 24 GPM C3+
        if (capinlet.le.6.00) then
        stcost24 = 0.2883*capinlet**(-0.5642)
        else
        stcost24 = 0.1898*capinlet**(-0.3179)
        end if

! 12 GPM C3+
        if (capinlet.le.6.00) then
        stcost12 = 0.2622*capinlet**(-0.5566)
        else
        stcost12 = 0.1759*capinlet**(-0.3207)
        end if

! 6.0 GPM C3+  (Rich)
        if (capinlet.le.6.00) then
        stcost6 = 0.2361*capinlet**(-0.5475)
        else
        stcost6 = 0.162*capinlet**(-0.3241)
        end if

! 3.0 GPM C3+ (Medium)
        if (capinlet.le.6.00) then
        stcost3 = 0.21*capinlet**(-0.5363)
        else
        stcost3 = 0.1481*capinlet**(-0.3282)
        end if

! 1.5 GPM C3+ (Lean)
        if (capinlet.le.6.00) then
        stcost1 = 0.2035*capinlet**(-0.5782)
        else
        stcost1 = 0.1293*capinlet**(-0.3159)
        end if

! calculate the gallons per minute (GPM) and interpolate between the three
! costs given above
! for NGL (natural gas liquids) we have:
!
! GPM = thruput*ngl*42.0/24.00/60.00
!
! where thruput = gas thruput (MMCFD)
!       ngl    = ngl is Bbl/MMCF

! cap ngl at 85 bbl/MMCF per request of B. Baron and A. Becker
!     if (ngl.gt.85.0) then
!       gpm = thruput*85.0*42.0/24.00/60.00
!     else
        gpm = thruput*ngl*42.0/24.00/60.00
!     end if

! interpolate the costs based on the gpm value
           if (gpm.le.1.500) then
              weightb = gpm/1.500
              call wt_cst(weightb,0.00,stcost1,stcost)
        else if (gpm.le.3.00) then
                weightb =  (gpm-1.500)/(3.00-1.500)
                call wt_cst(weightb,stcost1,stcost3,stcost)
        else if (gpm.le.6.00) then
                 weightb = (gpm-3.00)/(6.00-3.00)
                 call wt_cst(weightb,stcost3,stcost6,stcost)
        else if (gpm.le.12.00) then
                 weightb = (gpm-6.00)/(12.00-6.00)
                 call wt_cst(weightb,stcost6,stcost12,stcost)
        else if (gpm.le.24.00) then
                 weightb = (gpm-12.00)/(24.00-12.00)
                 call wt_cst(weightb,stcost12,stcost24,stcost)
        else if (gpm.le.48.00) then
                 weightb = (gpm-24.00)/(48.00-24.00)
                 call wt_cst(weightb,stcost24,stcost48,stcost)
        else if (gpm.le.96.00) then
                 weightb = (gpm-48.00)/(96.00-48.00)
                 call wt_cst(weightb,stcost48,stcost96,stcost)
        else if (gpm.le.192.00) then
                 weightb = (gpm-96.00)/(192.00-96.00)
                 call wt_cst(weightb,stcost96,stcost192,stcost)
      else
              stcost = stcost192
      end if



!       write(14,*)' '
!        write(14,*)'***Straight Ref. operating costs***'
!        write(14,*)'stroc costs in $/MCF               =',stcost
!       write(14,*)'6.0 costs in $/MCF           =',stcost6
!       write(14,*)'3.0 costs in $/MCF           =',stcost3
!       write(14,*)'1.5 costs in $/MCF           =',stcost1
!        write(14,*)'GPM of flow                      =',gpm
!       write(14,*)'capacity                         =',capinlet
!       write(14,*)' '


      return
      end



! *********************************************************************
      SUBROUTINE wt_cst(weightb,costa,costb,cost)
      IMPLICIT NONE
      real weighta,weightb,costa,costb,cost

      if ((weightb.gt.1.00).or.(weightb.lt.0.00)) then
!       write(14,*)'incorrect data in subroutine wt_cst'
!       write(14,*)'weightb=',weightb
        stop
      end if
      weighta = 1.00-weightb
      cost    = weighta*costa+weightb*costb

      return
      end


! *********************************************************************
      SUBROUTINE glycolcc(h2o_cst,captexas)
      IMPLICIT NONE
      real h2o_cst,captexas

      h2o_cst = exp(0.625700*log(captexas)-3.635300)

!     write(14,*)' '
!     write(14,*)'***Glycol Dehydration, capital costs check***'
!     write(14,*)'costs in $10**6                  =',h2o_cst
!     write(14,*)'capacity                         =',captexas
!     write(14,*)' '
      return
      end
! *********************************************************************
      SUBROUTINE glycoloc(h2o_cst,captexas)
      IMPLICIT NONE
      real h2o_cst,captexas
!     write(14,*)'just inside glycoloc'
!     write(14,*)'h2o_cst,captexas=',h2o_cst,captexas
      h2o_cst = exp(-0.387000*alog(captexas)-2.727600)*0.93300

!     write(14,*)' '
!     write(14,*)'***Glycol Dehydration operating costs***'
!     write(14,*)'costs in $/MCF inlet gas         =',h2o_cst
!     write(14,*)'captexas                         =',captexas
!     write(14,*)' '
      return
      end
! *********************************************************************
      SUBROUTINE nitrocc(n2_cst,captexas)
      IMPLICIT NONE
      real n2_cst,captexas

      n2_cst=exp(0.457500*log(captexas)+0.359700)
!     n2_cst=exp(0.457500*log(captexas)+0.359700)/2.000

!     write(14,*)' '
!     write(14,*)'***Nitrogen Rejection capital costs***'
!     write(14,*)'costs in $10**6                  =',n2_cst
!     write(14,*)'capacity                         =',captexas
!     write(14,*)' '
      return
      end
! *********************************************************************
      SUBROUTINE nitrooc(n2_cst,captexas)
      IMPLICIT NONE
      real n2_cst,captexas
      n2_cst=exp(3.433900*exp(-0.129100*log(captexas)))
!     n2_cst=exp(3.433900*exp(-0.129100*log(captexas)))/2.000

!       this is in cents/MCF

!       write(14,*)' '
!       write(14,*)'****Nitrogen Rejection operating costs**'
!       write(14,*)'costs in cents/MCF inlet gas     =',n2_cst
!       write(14,*)'capacity                         =',captexas
!       write(14,*)' '


      return
      enD
!***************************************************************
!from Econ_wtrpro.for
!     Last change:  MC   22 Mar 2009    1:14 pm

        subroutine write_proforma(ires,nyrsi,i0,effinv,NPVW,RORW,iyear)

        implicit none

        INTEGER ires
        INTEGER nyrsi
        integer iyear



!  This submodule writes the detailed economic proforma for a reservoir.
!  It is only run if the 'wrtpro', read by 'read_options' is true.

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
      include 'ogsmugr'
      include 'ogsml48'

! local variables
      integer iyr1,iyr2,i0,ipage,iyr
      integer igen(MAX_YR)              ! no of generators required for stea
      common/newcash/igen
      REAL effinv, NPVW, RORW
      character*80 line80
      CHARACTER*51 PROCCODE(MAX_PROC+1)
      INTEGER bsn

      PROCCODE(1) ='0 : DECLINE CURVE                                  '
      PROCCODE(2) ='1 : PRIMARY                                        '
      PROCCODE(3) ='2 : WATER FLOODING                                 '
      PROCCODE(4) ='3 : CO2 FLOODING                                   '
      PROCCODE(5) ='4 : STEAM FLOODING                                 '
      PROCCODE(6) ='5 : POLYMER FLOODING                               '
      PROCCODE(7) ='6 : INFILL DRILLING                                '
      PROCCODE(8) ='7 : PROFILE MODIFICATION                           '
      PROCCODE(9) ='8 : HORIZONTAL CONTINUITY                          '
      PROCCODE(10)='9 : HORIZONTAL PROFILE                             '
      PROCCODE(11)='10: UNDISCOVERED CONVENTIONAL OIL                  '
      PROCCODE(12)='11: CONVENTIONAL RADIAL FLOW GAS                   '
      PROCCODE(13)='12: WATER DRIVE                                    '
      PROCCODE(14)='13: TIGHT SANDS                                    '
      PROCCODE(15)='14: WET COAL/SHALE GAS                             '
      PROCCODE(16)='15: DRY COAL/SHALE GAS                             '
      PROCCODE(17)='16: UNDISCOVERED CONVENTIONAL GAS                  '
      PROCCODE(18)='17: UNDISCOVERED UNCONVENTIONAL OIL                '
      PROCCODE(19)='18: UNDISCOVERED TIGHT GAS                         '
      PROCCODE(20)='19: UNDISCOVERED COAL BED METHANE                  '
      PROCCODE(21)='20: UNDISCOVERED SHALE GAS                         '
      PROCCODE(22)='21: DEVELOPING SHALE GAS                           '
      PROCCODE(23)='22: DEVELOPING TIGHT GAS                           '
      PROCCODE(24)='23: DEVELOPING COALBED METHANE                     '

       write (i0,*)
       write (i0,*)
       WRITE (i0,*)'+++++++++++++++++++++++++++++++++++++++++++++++++'
       write (i0,*)'New Reservoir '
       write (i0,1102) procde
       WRITE (I0,*) PROCCODE(PROCDE+1)

       write (i0,1103) resid
       write (i0,1101) effinv
       write (i0,3101) RORW
       write (i0,3102) NPVW
       write (i0,1104) ilifex2
       write (i0,1105) ilife
       WRITE (I0,1106) NYRSI

1102   FORMAT(1X,'PROCESS           :: ',I3)
1103   FORMAT(1X,'FOR RESERVOIR     :: ',A23)
1101   FORMAT(1x,'Investment Efficiency (DCFROR.FOR) ::' ,f12.3)
3101   FORMAT(1x,'RATE OF RETURN ROR    (DCFROR.FOR) ::' ,f12.3)
3102   FORMAT(1x,'NET PRESENT VALUE                  ::' ,f12.3)
1104   FORMAT(1X,'BASE PATTERN LIFE :: ',I2)
1105   FORMAT(1X,'ADV  PATTERN LIFE :: ',I2)
1106   FORMAT(1X,'PROJECT LIFE      :: ',I2)

       IYR1 = 1
       IYR2 = max_yr-1

!       WRITE(I0,*)ILIFE,DWC_W,PSI_W,PSW_W
!       WRITE(I0,*)RESID, NYRSI

!YEAR  RESID  API  DEPTH  SULFUR  CO2SOURCE  BASIN  PLAY  REGION
!I4, 2X, A11, 2X, F10.2, 2X, F10.2, 2X, I2, 2X, F10.2, 2X, I2, 2X, I2, 2X, I8, 2X, I2

       WRITE (I0,*)
       WRITE (I0,*)
       WRITE (I0,*)

!       WRITE (I0,1009) '2008',RESID,API,DEPTH,SULFOIL,'00','65',PLAY,
!     &  REGNUM
 1009  FORMAT (A4,2X,A11,2X,F10.2,2X,F10.2,2X,F10.2,2X,A2,2X,A2,2X,I8, &
        2X,I2)

       WRITE(I0,1011) 'YEAR',(IYR+(iyear-1)+(L48B4YR-1),IYR=IYR1,IYR2)
       WRITE(I0,2000) ('==========',IYR=IYR1,IYR2)

       WRITE(I0,1013) 'NO OF PATTERNS       ', &
         (SUMP(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'NO OF ACTIVE PATTERNS  ', &
         (XPATN(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'PATTERNS INITIATED     ', &
         (PATN(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'OIL PRODUCTION (MBO)', &
         (OILPROD(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'GAS PRODUCTION (MMCF)', &
         (GASPROD(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'NGL PRODUCTION (MB)', &
         (NGLPROD(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'OIL PRICE   ($/BBL)', &
         (OILPRICEO(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'GAS PRICE   ($/MCF)', &
         (GASPRICEO(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'REMAINING PROVEN RESRV(MBOE)', &           !mc change ver 7
         (REMRES(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'REMAINING PROVEN RESRV(MMcf)', &           !mc change ver 7
         (gREMRES(IYR),IYR=IYR1,IYR2)

       write(i0,1013) 'INFERRED RESERVES (MBBL)', &               !mc change ver 7
         (IREMRES(IYR),IYR=IYR1,IYR2)                             !mc change ver 7
       WRITE(I0,1013) 'INFERRED RESERVES (MMCF)', &               !mc change ver 7
         (IGREMRES(IYR),IYR=IYR1,IYR2)                            !mc change ver 7

       WRITE(I0,101) 'GROSS REVENUES (M$)', &
         (GROSS_REV(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'ADJUSTED OIL PRICE ($/BBL)', &
         (OILPRICEO(IYR)+FACT,IYR=IYR1,IYR2)
       WRITE(I0,102) 'GRAVITY/TRANS. COST ADJ.', &
        (GRAVPEN(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'ADJUSTED REVENUES',(ADJGROSS(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'ROYALTIES', &
          (ROYALTY(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'NET SALES',(NET_REV(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'TOTAL OPERATING COST',(TOC(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'G&A ON EXPENSED ITEMS', &
         (GA_EXP(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'G&A ON CAPITALIZED ITEMS', &
         (GA_CAP(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'CO2/POLYMER COST       ', &
         (INJ(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'WATER  PRODUCTION VOLUME', &
         (WATPROD(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'WATER INJ VOL/FUEL VOLUME', &
         (WATINJ(IYR),IYR=IYR1,IYR2)

       WRITE(I0,1022) 'CO2 PURCHASE PRICE', &
         (CO2COST,IYR=IYR1,IYR2)

       WRITE(I0,102) 'CO2/SURF/STEAM INJECTED VOL', &
         (TOTINJ(IYR),IYR=IYR1,IYR2)

      WRITE(I0,102) 'CO2/SURF/STEAM RECYCLED VOL', &
         (TORECY(IYR),IYR=IYR1,IYR2)
      WRITE(I0,102) 'CO2  RECY/INJECTED COST', &
         (TORECY_CST(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'CO2 FOAM COST', &
         (FOAM(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'NO OF STEAM GENERATORS', &
         (FLOAT(IGEN(IYR)),IYR=IYR1,IYR2)

       WRITE(I0,102) 'GAS PROCESSING' ,(PROC_OAM(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'GENERAL O&M',(OAM(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'GENERAL ANNUAL O&M',(AOAM(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'STIMULATION' ,(STIM(IYR),IYR=IYR1,IYR2)
       write(i0,102) 'FRACTURING',(fraccst(iyr),iyr=iyr1,iyr2)                                !mc change B

       write (i0,102) 'MARGINAL WELL TAX CREDIT',(WATCRED(IYR),IYR=IYR1,IYR2)                 !MC CHANGE A

       WRITE(I0,102) 'ENVIRONMENTAL EXISTING O&M', &
        (EXIST_EOAM(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'ENVIRONMENTAL NEW O&M', &
        (NEW_EOAM(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'ENVIRONMENTAL EXISTING CAPITAL', &
        (EXIST_ECAP(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'ENVIRONMENTAL NEW CAPITAL', &
        (NEW_ECAP(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'TOTAL INVESTMENTS', &
         (TI(IYR)+ICST(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'INTANGIBLE INVESTMENT', &
         (ICST(IYR),IYR=IYR1,IYR2)
        WRITE(I0,102) 'INTANG. DRILLING COSTS   ', &
           (II_DRL(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'OTHER INTANGIBLE COSTS', &
         (ICAP(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'PORTION OF INTANGIBLES TO CAPITALIZE', &
         (INTCAP(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'TANGIBLE INVESTMENTS', &
         (TI(IYR),IYR=IYR1,IYR2)
        WRITE(I0,102) 'TANG. DRILLING COST   ', &
         (TI_DRL(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'OTHER TANGIBLE CAPITAL', &
         (OTC(IYR),IYR=IYR1,IYR2)

       WRITE(I0,102) ' COMPRESSOR TANGIBLE CAPITAL', &
         (COMP(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'DEPRECIABLE/CAPITALIZED INVESTMENTS', &
         (TCI(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'PORTION OF INTANGIBLES TO CAPITALIZE', &
         (INTCAP(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'ADJ. FOR FEDERAL TAX CREDITS', &
         (TCIADJ(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'DEPRECIABLE/CAPITALIZE BASE', &
         (CAP_BASE(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'DEPRECIATION ON TANGIBLES', &
         (DEPR(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'DEPRECIATION-CAPITALIZED INTANGIBLES', &
         (AMOR(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'DEPLETABLE G&G/LEASE COSTS', &
         (LA(IYR)*PLAC+GG(IYR)*PGGC,IYR=IYR1,IYR2)
       WRITE(I0,102) 'DEPLETABLE LEASE ACQ. COST', &
         (LA(IYR)*PLAC,IYR=IYR1,IYR2)
       WRITE(I0,102) 'DEPLETABLE G&G COSTS', &
         (GG(IYR)*PGGC,IYR=IYR1,IYR2)
       WRITE(I0,102) 'ADJUSTMENTS FOR FEDERAL TAX CREDITS', &
         (DEP_CRD(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'ADDITIONS TO DEPLETION BASE', &
         (ADGGLA(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'DEPLETION BASE', &
         (DGGLA(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'EXPENSED G&G/LEASE COSTS', &
         (EGGLA(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'EXP. LEASE PURCHASE COST', &
         (LA(IYR)*(1-PLAC),IYR=IYR1,IYR2)
       WRITE(I0,102) 'EXP. G&G COSTS', &
         (GG(IYR)*(1-PGGC),IYR=IYR1,IYR2)

       WRITE(I0,101) 'NET REVENUES', &
         (NET_REV(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'OPERATOR SEVERANCE TAXES', &
         (SEV_TAX(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'OPERATING COSTS', &
         (TOC(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'EXPENSED INT.,G&G, AND LEASE ACQ.', &
         (ICST(IYR)-INTCAP(IYR)+EGGLA(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'DEPRECIATION TOTAL', &
         (DEPR(IYR)+AMOR(IYR),IYR=IYR1,IYR2)
!TO CALCULATE AND INSERT HERE
       WRITE(I0,102) 'DEPLETION ALLOWANCE', &
         (DEPLET(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'TAXABLE INCOME', &
         (NIBTA(IYR),IYR=IYR1,IYR2)

!TO CALCULATE ADDBACKS
       WRITE(I0,102) 'TAX CREDIT ADDBACK', &
         (EORTCA(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'INTANGIBLE ADDBACK', &
         (INTADD(IYR),IYR=IYR1,IYR2)

       WRITE(I0,102) 'G&G/LEASE ADDBACK', &
         (GGLA(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'NET INCOME BEFORE TAXES', &
         (NIBT(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'STATE INCOME TAXES', &
         (STATE_TAX(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'AMT                ', &
         (AMINT(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'FEDERAL INCOME TAX', &
         (FEDTAX(IYR),IYR=IYR1,IYR2)

       WRITE(I0,102) 'FEDERAL TAX CREDITS', &
         (FEDTAX_CR(IYR),IYR=IYR1,IYR2)
        WRITE(I0,101) 'NET INCOME AFTER TAXES', &
         (NIAT(IYR),IYR=IYR1,IYR2)
        WRITE(I0,102) 'PLUS TOTAL DEPRECIATION', &
         (DEPR(IYR)+AMOR(IYR),IYR=IYR1,IYR2)
        WRITE(I0,102) 'PLUS DEPLETION', &
         (DEPLET(IYR),IYR=IYR1,IYR2)
        WRITE(I0,102) 'LESS ADDITIONAL DEPLETABLE ITEMS', &
         (ADGGLA(IYR),IYR=IYR1,IYR2)
        WRITE(I0,102) 'LESS DEPRECIABLE/CAPITALIZED ITEMS', &
         (INTCAP(IYR)+TI(IYR),IYR=IYR1,IYR2)
        WRITE(I0,102) 'LESS TAX CREDIT ADDBACKS', &
         (EORTCA(IYR)+INTADD(IYR)+GGLA(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'ANNUAL AFTER TAX CASH FLOW', &
         (ATCF(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'DISCOUNTED AFTER TAX CASH FLOW', &
         (DATCF(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'CUMULATIVE DISCOUNTED AFTER TAX CASH FLOW', &
         (CATCF(IYR),IYR=IYR1,IYR2)

       WRITE (I0,994) 'NEW INJECTION WELLS', &
                         (EINJDR(IRES,IYR)   ,IYR=IYR1,IYR2)
       WRITE (I0,994) 'NEW PRODUCTION WELLS', &
                         (EPRODDR(IRES,IYR)  ,IYR=IYR1,IYR2)
       WRITE (I0,994) 'ACTIVE PRODUCING WELLS', &
                         (EPRODWELL(IRES,IYR),IYR=IYR1,IYR2)
       WRITE (I0,994) 'ACTIVE INJECTION WELLS', &
                         (EINJWELL(IRES,IYR) ,IYR=IYR1,IYR2)
       WRITE (I0,994) 'SHUT IN WELLS', &
                         (ESHUTIN(IRES,IYR)  ,IYR=IYR1,IYR2)
       WRITE (I0,994) 'PATTERNS DEVELOPED / BAS', &
                         (PATDEV(IRES,IYR,1) ,IYR=IYR1,IYR2)
       WRITE (I0,994) 'PATTERNS DEVELOPED / ADV', &
                         (PATDEV(IRES,IYR,2) ,IYR=IYR1,IYR2)


994    FORMAT(T1,A,T50,<max_yr-1>(1X,F12.2,1X))
!      enddo !ipage
 1011 format(t40,a,t49,<max_yr-1>(10x,i4))
 1013 format(t1,a,t50,<max_yr-1>(1x,f12.3,1x))
 101  format(t1,a,t50,<max_yr-1>(1x,f12.2,1x))
 102  format(t2,a,t50,<max_yr-1>(1x,f12.2,1x))
 103  format(t3,a,t50,<max_yr-1>(1x,f12.0,1x))
 104  format(t4,a,t50,<max_yr-1>(1x,f12.0,1x))
 105  format(t5,a,t50,<max_yr-1>(1x,f12.0,1x))
 2000 format(t50,<max_yr-1>(3x,a10,1x))
 1022 format(t2,a,t50,<max_yr-1>(1x,f12.3,1x))

      return

      end subroutine!***************************************************************
!from ECON_SEVTAX.FOR
!     Last change:  MC    2 Jun 2008    5:39 pm
       SUBROUTINE SEV_TAX2(ISC,PRPRO,PRPRG,PATI,NPAT,ROYL, &
       PRICEO,PRICEG,CUMINC,CUMTAN,sevtax, &
       iprocc,isevyr)
!
!  PURPOSE: TO CALCULATE THE TOTAL OF TAXES THAT COME
!          OUT OF THE FRONT END OF THE PROJECT USING
!          A REALISTIC REPRESENTATION OF STATE TAX LAWS
!
!
!417DJR02  START (6/26/93)
!
!   THIS MODIFICATION WAS DONE IN ORDER TO ACCOUNT FOR
!   NEW SEVERANCE TAX RATES IN SEVERAL STATES. THESE
!   NEW RATES WILL ONLY BE GRANTED WHEN RUNNING A BASE
!   CASE RUN. A "STATE INCENTIVE " RUN WILL USE THE
!   ORIGINAL SEVERANCE TAX RATES.
!
!417DJR02  END
        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include 'ogsml48'


        INTEGER ISC,ifbcase2,isevyr,iprocc
        REAL SEVTAX,PRPRO,PRPRG,NPAT,ROYL,PRICEO, &
       PRICEG,PATI,CUMINC,CUMTAN,T1,T2,T3,T4,T5,T6, &
       checkcf
!
! Table lookup  for ifbcase2 (file read in readsev.for)
! file: sevtaxr.dat     sevt.h (variables)
!
        ifbcase2=0
        do ic=1,totstate
             if(isc.eq.stl(ic))then
                ifbcase2= sev_proc(ic,iprocc)
             endif
        enddo

        checkcf = cuminc
        if(isevyr.eq.1) checkcf = -1.0
        sevprice = priceo
        t111=0.0
        t211=0.0



!417DJR02   START
!
!      COMMON /SEVDATA/ IFBCASE2,CHECKCF,ISEVYR,SEVPRICE,DEPTH
!
!417DJR02   END
!
! DEFINITION OF CALL ARGUMENTS
!
! ISC: STATE CODE BASED ON POSTAL SYSTEM
! PRPRO: TOTAL PRODUCTION OF OIL FROM THE PROJECT (BBL/YR)
! PRPRG: TOTAL PRODUCTION OF ASSOCIATED GAS FROM THE
!        PROJECT(MCF/YR)
! NPAT: NUMBER OF ACTIVE PATTERNS IN THE PROJECT
! PATI: NUMBER OF PATTERNS INITIATED DURING THE YEAR
! ROYL: ROYALTY RATE
! PRICEO: OIL PRICE IN EFFECT (POSTED PRICE LESS GRAVITY
!         PENALTY AND TRANSPORTATION DEDUCTIONS)
! PRICEG: GAS PRICE
! CUMTAN: CUMULATIVE TANGIBLE INVESTMENT (FOR STATES WITH
!         FRONT-END PROPERTY TAXES)
! CUMINC: CUMULATIVE AFTERTAX INCOME (FOR LOUISIANA)
!      *** NOTE: THE FOLLOWING ARE SUBSIDIARY OUTPUTS
! T1: OIL SEVERANCE TAX
! T2: GAS SEVERANCE TAX
! T3: CONSERVATION TAX
! T4: FRONT-END PROPERTY TAX
! T5: LOCAL TAXES
! T6: DRILLING TAXES
!
!
! COMPUTATION OF QUANTITIES USED IN THE STATE-BY-STATE
!         CALCULATIONS
!
!  ZERO OUT SUBSIDIARY TAX HOLDERS
         T1=0.
         T2=0.
         T3=0.
         T4=0.
         T5=0.
         T6=0.
          PRPRO = prpro
          prprg = prprg
!
! WIFRAC:  WORKING INTEREST FRACTION
          WIFRAC=1.0-ROYL
! PRBOD: PROJECT-WITDE OIL RECOVERY EXPRESSED IN BOPD
          PRBOD= PRPRO/365.25
! PRGD: PROJECT-WIDE GAS RECOVERY EXPRESSED IN MCF/DAY
          PRGD=PRPRG/365.25
! WLPRO: ANNUAL PRODUCTION FROM A TYPICAL WELL
!
!   *** IF INPUT VALUE FOR NUMBER OF PATTERNS IS
!        ZERO, THE PROGRAM REJECTS THE INPUTS
!        AND RETURNS A ZERO VALUE FOR SEVERANCE TAX
           IF(NPAT.GT.0.) GO TO 50
              SEVTAX=0.
!         IF(PRPRO.GT.0) WRITE(46,51)
!51    FORMAT(1X,'*** SEVTAX ERROR: NONZERO PRODUCTION',
!    A1X,'WITH ZERO ACTIVE PATTERNS')
              RETURN
 50     CONTINUE
!    *** AT THIS POINT, WE KNOW THAT 'NPAT' IS NOT ZERO
!        AND WE MAY DIVIDE BY IT WITH IMPUNITY ****
!
!
       IF((PRPRO.LE.0.).AND.(PRPRG.LE.0.)) SEVTAX=0.
       IF((PRPRO.LE.0.).AND.(PRPRG.LE.0.))RETURN
           WLPRO=PRPRO/NPAT
! WLPRG: ANNUAL GAS PRODUCTION FROM A TYPICAL WELL (MCF/YR)
           WLPRG=PRPRG/NPAT
! WLPROD: OIL PRODUCTION FROM A TYPICAL WELL,BOPD
           WLPROD= PRBOD/NPAT
! WLPRGD: GAS PRODUCTION FROM A TYPICAL WELL,MCF/DAY
           WLPRGD=PRGD/NPAT
! VALOIL: WORKING INTEREST VALUE OF OIL PRODUCED BY
!         THE PROJECT IN ANY YEAR
          VALOIL=PRICEO*WIFRAC*PRPRO
! VALGAS: WORKING INTEREST VALUE OF GAS PRODUCED BY THE
!         PROJECT IN ANY GIVEN YEAR
          VALGAS=PRICEG*WIFRAC*PRPRG
! VALOW: WORKING INTERESTVALUE OF OIL PRODUCED PER WELL PER YEAR
          VALOW=VALOIL/NPAT
! VALGW: WORKING INTEREST VALUE OF GAS PRODUCED PER WELL PER YEAR
          VALGW=VALGAS/NPAT
!
! THE NEXT SET OF 'IF' STATEMENTS SENDS THE DATA TO THE
!     CORRECT BLOCK OF CODE FOR PROCESSING
!
!************  BEGIN MODIFICATIONS -- RJH  **********************
!
!   The two major sources for the modifications to Tax structures
!  are taken from "Summary of USA Oil & Gas Production Taxes", by
!  the Society of Petroleum Evaluation Engineers, Houston, Texas.
!  (Revised December 31, 1993)  One of their sources and also our
!  other source is the "State Tax Handbook", by Commerce Clearing
!  House, Inc., 1994, Chicago, IL. (CCH, Inc.)
!    A source for 10 states was the Center for Legislative Energy
!  and Environmental Research (CLEER), "Production=Jobs & Revenues",
!  Presented December 1993.  CLEER is in Dallas, TX.
!   RJH -- 12/15/94
!       -- Subroutine structure is modified for 27 states using the
!        IF .. THEN, ELSE IF .. THEN, and ELSE statements ending with ENDIF .
!          These states are placed in order of their code numbers,
!          except for Alaska (AK).
!      The states are:
!          AL, AK, AR, CA, CO, FL, IL, IN, KS, KY,
!          LA, MI, MS, MT, NE, NM, NY, ND, OH, OK,
!          PA, SD, TN, TX, UT, WV, WY
!      The codes are :
!           1, 50,  3,  4,  5,  9, 12, 13, 15 ,16,
!          17, 21, 23, 25, 26, 30, 31, 33, 34, 35,
!          37, 40, 41, 42, 43, 47, 49
!
!    Also the following 7 states have been added to the list because
!  a severance tax has been found in the tax information literature.
!      The states are:
!          AZ, GA, ID, NV, OR, VA, WI
!      The codes are :
!           2, 10, 11, 27, 36  45, 48
         IF(ISC.EQ.1) THEN
!**************************ALABAMA*******************************
! 100       CONTINUE
!      TAX CALCULATIONS FOR ALABAMA
! *** RECOGNIZE DIFFERENTIAL FOR STRIPPER WELLS
!
!   RJH -- Wed Jan 18, 1995
!  All the tax rates and exceptions are the same as before except
!  that there is no known state drilling fee.
!
!       Also, onshore discovery wells have severance rates reduced
!  to 6% for 5 years from date of first production and development
!  wells are 6% if commenced within 4 years from discovery wells
!  completion date with depths of 6000 feet or less, and for 2 years
!  from date with depths greater than 6000 feet.
!  (Discovery dates are not available in the reservoir data base)
!
!  Strippers still are at 25 BOPD and 200 MCFD
!
        IF(WLPROD.GT.25.0) T1=0.08*VALOIL
        IF(WLPROD.LE.25.0)T1=0.04*VALOIL
!417DJR02   START
        IF (IFBCASE2.EQ.1) T1=0.04*VALOIL
!417DJR02   END
!
        IF(WLPRGD.GT.200.0) T2=0.08*VALGAS
        IF(WLPRGD.LE.200.0) T2=0.04*VALGAS
! CONSERVATION TAX
         T3=0.02*(VALOIL+VALGAS)
! DRILLING PERMIT FEE IS $250
!        T6=NPAT*250.  Wed Jan 18, 1995
         SEVTAX=T1+T2+T3+T6
         RETURN
!
         ELSE IF(ISC.EQ.50) THEN
!**************************ALASKA********************************
! 110     CONTINUE
! TAX COMPUTATIONS FOR ALASKA
!
!  -- RJH -- Wed Jan 18, 1995
!   The field economic limit factor is the point where the total
!  costs including operating costs, royalties and production taxes
!  will exceed gross revenue and the field becomes unprofitable.
!    The ELF is one minus the ratio of the monthly production rate
!  at the economic limit to the production during the month.
!  The field figures used are:  300 BOPD and 3000 MCFD
!  (Roger Marks, Income & Excise Tax Div, Anchorage, AK confirmed
!  that 300 BOPD and 3000 MCFD are still in effect; his phone number
!  is (907) 269-6620; also 5 cents per barrel clean-up tax is added
!   the fund reaches $50 million; he said it never reaches this ammout)
!
! *** CALCULATION OF THE ECONOMIC LIMIT FACTOR
!     OIL ECONOMIC LIMIT FACTOR
!   WD: WELL DAYS PER MONTH (NO INTERRUPTION)
         WD=365.25/12.
! PEL: MONTHLY OIL PRODUCTION RATE AT STATUTORY LIMIT
         PEL=300.0*WD
! TP: TOTAL MONTHLY PRODUCTION (ACTUAL)
         TP=WLPROD*WD
! OELFAC: OIL ECONOMIC LIMIT FACTOR (FROM STATUTE)
      IF(TP.LE.0.)OELFAC=0.
!  -- RJH -- Fri Jan 27, 1995
!  According to Roger Marks (see above) the ELF can never exceed 1.
!  The value of EXP(460.0*WD/PEL) was always 4.6336 which made the
!  ELF factor greater than 1 when WLPROD exceeded 382.563 BOPD.
!  Also PEL/TP=300/WLPROD
!
!HDG 4-11-95: The code below reflects the latest information from
!             Roger Marks concerning the calculation of the ELF.
!
! New: OELFAC=(1.-300./WLPROD)**(150000./PRBOD)**(460./300.)
!
       IF ( TP .GT. 0. ) THEN
           A1 = 1. - 300./WLPROD
           IF ( A1 .LT. 0. ) A1 = 0.
           B1 = 150000./PRBOD
           C1 = 460./300.
           OELFAC = A1**B1**C1
       END IF
       IF ( OELFAC .LT. 0. ) OELFAC = 0.
       IF ( OELFAC .GT. 1. ) OELFAC = 1.
!
!HDG 4-11-95: The code below reflects the former method of
!             calculating the ELF.
!
!      IF (TP.GT.0.)
!     X  OELFAC=(1.0-(PEL/TP))*EXP(460.0*WD/PEL)
!      IF(OELFAC.LE.0.) OELFAC=0.
!
!HDG 4-11-95: End former method.
!
!     GAS ECONOMIC LIMIT FACTOR
! GELFAC: GAS ECONOMIC LIMIT FACTOR (FROM STATUTE)
         IF(WLPRGD.LE.0.)GELFAC=0.
!  -- RJH -- Fri Jan 27, 1995
!  Cut out unecessary multiplication to save time.
         IF(WLPRGD.GT.0.) &
        GELFAC=( 1.0 - ( 3000./WLPRGD ) )
!     A  GELFAC=(1.0-(3000*WD/(WLPRGD*WD)))
!
         IF(GELFAC.LE.0) GELFAC=0.
! COMPUTATION OF OIL PRODUCTION TAX
         T11=0.15*VALOIL*OELFAC
         T12=0.80*PRPRO*OELFAC
         T1=T11
         IF(T12.GT.T11)T1=T12
! COMPUTATION OF GAS PRODUCTION TAX
         T21=0.10*VALGAS*GELFAC
         T22=0.064*PRPRG*GELFAC
          T2=T21
         IF(T22.GT.T21)T2=T22
! COMPUTATION OF OIL CONSERVATION TAX
! -- RJH --  Wed Jan 18, 1995
!  A conservation tax of 4 mills per barrel oil and
!  4 mills per 50 MCF gas is added.
!  A clean-up tax of $0.05 per barrel oil is added depending on
!  whether revenues in state funds exceed $50 million, then only
!  $0.03 per barrel is applied.
!         T3=0.00125*PRPRO*WIFRAC
!    the surcharge is :
         T3=(0.054*PRPRO+0.004*PRPRG/50)*WIFRAC
! COMPUTATION OF PROPERTY TAX
!  -- RJH -- Wed Jan 18, 1995
!   Current oil & gas property tax is 20 mills of full and true value.
!  Only CLEER supports this property tax idea.  Other sources do not.
!         T4=0.02*CUMTAN
!
         SEVTAX=T1+T2+T3+T4
         RETURN
!
!
!  -- RJH -- Mon Jan 23, 1995
         ELSE IF(ISC.EQ.2) THEN
!**************************ARIZONA*******************************
!
! Computation of ARIZONA taxes is a new addition
!   Oil severance tax base rate is 3.125%
!   Gas severance tax base rate is 3.125%
!
       T1=0.03125*VALOIL
       T2=0.03125*VALGAS
       SEVTAX=T1+T2
       RETURN

         ELSE IF(ISC.EQ.3) THEN
!**************************ARKANSAS******************************
!
! 130      CONTINUE
!  CALCULATION OF TAXES FOR ARKANSAS
!
! OIL SEVERANCE TAX HAS A STRIPPER WELL EXCLUSION
!  -- RJH -- Wed Jan 18, 1995
! The second tax added is now 2 cents per barrel.
       IF(WLPROD.GT.10.0)T1=0.05*VALOIL + 0.02*PRPRO*WIFRAC
       IF(WLPROD.LE.10.0)T1=0.04*VALOIL + 0.02*PRPRO*WIFRAC
!
!       IF(WLPROD.GT.10.0)T1=0.05*VALOIL + 0.045*PRPRO*WIFRAC
!       IF(WLPROD.LE.10.0)T1=0.04*VALOIL + 0.045*PRPRO*WIFRAC
! OIL COMPONENT HAS BOTH AN AD VALOREM AND PER UNIT COMPONENT
!
! GAS SEVERANCE TAX
       T2=0.003*PRPRG*WIFRAC
! CONSERVATION TAX
        T3=0.025*PRPRO*WIFRAC + 0.005*PRPRG*WIFRAC
! COMPUTATION OF DRILLING TAX
!       T6=300.00*PATI only in year 1994
        SEVTAX=T1+T2+T3+T6
        RETURN
!
         ELSE IF(ISC.EQ.4) THEN
!**************************CALIFORNIA****************************
!
! COMPUTATION OF TAXES FOR CALIFORNIA
!
! *** NOTE: BASED ON LAST YEAR REVENUE VS OIL VALUE
!           THE KERN COUNTY LOCAL TAX RATE HAS BEEN
!           ESTIMATED AT 2.7% OF GROSS OIL AND GAS VALUE
!           THIS VALUE IS USED FOR THIS STUDY,WHICH
!           DOES NOT HAVE COUNTY OR MUNICIPALITY
!           INFORMATION IN THE DATA BASE
!
! COMPUTATION OF STATE LEVEL TAXES
!   RJH -- 12/15/94
!          T1=0.015223*PRPRO*WIFRAC
!          T2=0.015223*WIFRAC*PRPRG/10.
!
!   Although a California phone source said the tax was $0.03186/bbl oil
!   and $0.03186 per 10 MCF, one source (SPEE) puts the severance tax at
!   $0.025028/bbl oil and $2.5028/MMCF of gas (which comes to $.025028
!   per 10 MCF of gas), and another source (CCH) states the tax is
!   $0.025221 per barrel oil or per cubic feet gas (which probably means
!   $0.025221 per 10 MCF).
!
!          T1=0.025221*PRPRO*WIFRAC
!          T2=0.025221*WIFRAC*PRPRG/10.

!  MC 6.2.08 - 2006 State tax data
           T1 = 0.0508980*PRPRO
           T2 = 0.0508980*WIFRAC*PRPRG/10.0




!
! COMPUTATION OF LOCAL TAXES
           T5=0.027*(VALOIL+VALGAS)
           SEVTAX=T1+T2+T5
           RETURN
!
         ELSE IF(ISC.EQ.5) THEN
!**************************COLORADO******************************
!
! COMPUTATION OF TAXES FOR COLORADO
! COMPUTATION OF OIL TAXES ON A PER WELL BASIS
!      THERE IS A STRIPPER WELL EXEMPTION
!        IF(WLPROD.LE.10.0) T1=0.
!        IF(WLPROD.LE.10.0) GO TO 141
!          IF(VALOW.GT.0.)R1=0.02
!          IF(VALOW.GT.25000.)R1=0.03
!          IF(VALOW.GT.100000.)R1=0.04
!          IF(VALOW.GE.300000.)R1=0.05
!         T1=R1*VALOW*NPAT
! 141     CONTINUE
!
! COMPUTATION OF GAS TAXES, PER WELL BASIS
!  THERE IS NO STRIPPER EXEMPTION
!
!          IF(VALGW.GT.0.)R2=0.02
!          IF(VALGW.GT.25000.)R2=0.03
!          IF(VALGW.GT.100000.)R2=0.04
!          IF(VALGW.GE.300000.)R2=0.05
!           T2=R2*VALGW*NPAT
! COMPUTATION OF CONSERVATION TAX
!           T3=0.0006*(VALOIL + VALGAS)
!  -- RJH -- 12/15/94
!
!           T1=BRACKET RATE*VALOW*NPAT
!
!   Oil severance/production tax is 2.0% on the First $25,000,
!   3.0% on the Next $75,000, 4.0% on the Next $200,000, and
!   5.0% on greater than $300,000.  (CLEER, �ec. 1993)
!
        valow = valow*1000.0
        valgw = valgw*1000.0
      IF(WLPROD.LT.10.0 .AND. VALOW .LE. 0) THEN
        T1=0.
      ELSE
        IF(VALOW .GT. 25000) THEN
          T1=0.02*25000*NPAT
          IF(VALOW .GT. 100000) THEN
            T1=T1+0.03*75000*NPAT
            IF(VALOW .GT. 300000) THEN
              T1=T1+0.04*200000*NPAT+0.05*(VALOW-300000)*NPAT
            ELSE
              T1=T1+0.04*(VALOW-100000)*NPAT
            ENDIF
          ELSE
            T1=T1+0.03*(VALOW-25000)*NPAT
          ENDIF
        ELSE
          T1=0.02*VALOW*NPAT
        ENDIF
      ENDIF

!
! COMPUTATION OF GAS TAXES, PER WELL BASIS
!  THERE IS NO STRIPPER EXEMPTION
!           T2=BRACKET RATE*VALGW*NPAT
!
!   Gas severance/production tax is 2.0% on the First $25,000,
!   3.0% on the Next $75,000, 4.0% on the Next $200,000, and
!   5.0% on greater than $300,000.  (CLEER, �ec. 1993)
!
      IF(VALGW .LE. 0) THEN
        T2=0.
      ELSE
        IF(VALGW .GT. 25000) THEN
          T2=0.02*25000*NPAT
          IF(VALGW .GT. 100000) THEN
            T2=T2+0.03*75000*NPAT
            IF(VALGW .GT. 300000) THEN
              T2=T2+0.04*200000*NPAT+0.05*(VALGW-300000)*NPAT
            ELSE
              T2=T2+0.04*(VALGW-100000)*NPAT
            ENDIF
          ELSE
            T2=T2+0.03*(VALGW-25000)*NPAT
          ENDIF
        ELSE
          T2=0.02*VALGW*NPAT
        ENDIF
      ENDIF
!
! COMPUTATION OF CONSERVATION TAX
!
!  Oil & Gas are both 1.5 mills per Dollar of Value at
!   the Well.  (CLEER, Dec. 1993)
!
           T3=0.0015*(VALOIL + VALGAS)
!
!  State Sales and Use Taxes which are not implemented are :
!         State                     3.0%
!         County, City, MTA, etc    4.0%
!
!     General property tax: well equipment is valued at 29% of
!  replacement cost minus first year depreciation times local
!  levy.  Oil & gas production is valued at 87.5% of selling
!  price of previous year's production excluding Government royalties
!  times local levy.
!     Credit is allowed against severance tax for
!  87.5% of all Ad Valorem Taxes paid based on production,
!  excluding stripper production.
!       ( CLEER, Dec. 1993)
!
!     This could come to $75 per well or this can be called a
!    drilling tax.
!
!        T6=75*NPAT
          SEVTAX=T1+T2+T3+T6
          sevtax= sevtax/1000.0
!          PRINT*, sevtax/1000.
!          pause
          RETURN
!
         ELSE IF(ISC.EQ.9) THEN
!**************************FLORIDA*******************************
! 150     CONTINUE
!    TAX CALCULATIONS FOR FLORIDA
!  -- RJH -- Wed Jan 18, 1995
!  Base oil severance tax is 8%.
!  Tertiary oil and stripper wells at 10 BOPD are taxed at 5%.
!  Gas severance is $0.123 per MCF (annually calculated)
!   There is no known drilling fee per well.
!
         T1=0.08*VALOIL
         IF( WLPROD .LE. 10) T1=0.05*VALOIL
         IF(IFBCASE2 .EQ. 1) T1=0.05*VALOIL
         T2=0.123*PRPRG*WIFRAC
!
!         T1=0.05*VALOIL
!         T2=0.05*VALGAS
!         T6= 50.00*PATI
         SEVTAX=T1+T2+T6
         RETURN
!
!  -- RJH -- Mon Jan 23, 1995
         ELSE IF(ISC.EQ.10) THEN
!**************************GEORGIA*******************************
!
! Computation of GEORGIA taxes is a new addition to prevent the 5% default.
!   No oil severance tax.
!   No gas severance tax.
!
       SEVTAX=0.0
       RETURN
!
!  -- RJH -- Mon Jan 23, 1995
         ELSE IF(ISC.EQ.11) THEN
!***************************IDAHO********************************
!
! Computation of IDAHO taxes is a new addition
!   Oil severance tax base rate is 2%
!   Gas severance tax base rate is 2%
!   Conservation tax is 5 mill per barrel of oil and 5 mills per 50 MCF gas.
!
       T1=0.02*VALOIL
       T2=0.02*VALGAS
       T3=0.005*(PRPRO+PRPRG/50)*WIFRAC
       SEVTAX=T1+T2+T3
       RETURN
         ELSE IF(ISC.EQ.12) THEN
!**************************ILLINOIS******************************
! 160      CONTINUE
! TAX CALCULATIONS FOR ILLINOIS
!
! *** NOTE ILLINOIS HAS NO SEVERANCE TAX
          SEVTAX=0.
          RETURN
!
         ELSE IF(ISC.EQ.13) THEN
!**************************INDIANA*******************************
! 170     CONTINUE
! TAX CALCULATIONS FOR INDIANA
        T1=0.01*VALOIL
        T2=0.01*VALGAS
! -- RJH -- Thu Jan 19, 1995
!  A minimum  tax of $0.24 per barrel oil, and $0.03 per MCF gas.
!   ( Source -- SPEE )
      T11=0.24*PRPRO*WIFRAC
      T21=0.03*PRPRG*WIFRAC
      IF( T1 .LT. T11) T1=T11
      IF( T2 .LT. T21) T2=T21
!
         SEVTAX=T1+T2
         RETURN
!
         ELSE IF(ISC.EQ.15) THEN
!**************************KANSAS********************************
! 180       CONTINUE
! TAX CALCULATIONS FOR KANSAS
           R1=0.08
           IF(WLPROD.LT.2.0)R1=0.
!417DJR02   START
           IF (IFBCASE2.EQ.1) R1=0.
!417DJR02   END
! -- RJH -- Thu Jan 19, 1995
!      All oil produced from 2000 feet or greater is exempt if:
!  the oil price per barrel is $30 or less for strippers of 4 BOPD or less;
!  or is $24/bbl for 5 BOPD or less; or $16/bbl for 6 BOPD or less;
!  or $10/bbl for 7 BOPD or less.  Add 1 BOPD if lease is water flooded.
!  Oil price is average of July through December of previous year.
!    An environmental tax of $0.002 per barrel oil and $0.00006 per MCF gas.
!
       IF(DEPTH .GE. 2000) THEN
         IF(PRICEO .LE. 10) THEN
           IF(WLPROD .LE. 7) R1=0.0
         ELSE IF(PRICEO .LE. 16) THEN
           IF(WLPROD .LE. 6) R1=0.0
         ELSE IF(PRICEO .LE. 24) THEN
           IF(WLPROD .LE. 5) R1=0.0
         ELSE IF(PRICEO .LE. 30) THEN
           IF(WLPROD .LE. 4) R1=0.0
         ENDIF
       ENDIF
!******
           T1=R1*VALOIL
           R2=0.08
           IF(VALGW.LT.81.0)R2=0.0
           T2=R2*VALGAS
!         T3=0.0225*PRPRO*WIFRAC + 0.00336*PRPRG*WIFRAC
!  -- RJH -- Thu Jan 19, 1995
          T3=0.002*PRPRO*WIFRAC + 0.00006*PRPRG*WIFRAC
         SEVTAX =T1+T2+T3
         RETURN
!
         ELSE IF(ISC.EQ.16) THEN
!**************************KENTUCKY******************************
! 190         CONTINUE
! TAX CALCULATIONS FOR KENTUCKY
      T1=0.045*VALOIL
      T2=0.045*VALGAS
        SEVTAX=T1+T2
        RETURN
!
         ELSE IF(ISC.EQ.17) THEN
!**************************LOUISIANA*****************************
! 200      CONTINUE
! TAX CALCULATIONS FOR LOUISIANA
!
! ***NOTE: LOUISIANA COLLECTS NO SEVERANCE TAXES
!          UNTIL THE TERTIARY RECOVERY INVESTMENT
!          IS REPAID FROM TERTIARY PRODUCTION REVENUES
       R1=0.125
!  -- RJH -- Fri Jan 20, 1995
!  SPEE uses $0.075/MCF; but CCH, Inc. uses $0.070
       R2=0.075
!  Includes 10 & 25 BOPD productions.
        IF(WLPROD.LE.10.0)R1=0.03125
!  Water cut must be 50% or more for 6.25%
        IF(WLPROD.LE.25.0)R1=0.0625
!        IF(WLPROD.LT.25.0)R1=0.0625
!        IF(WLPROD.LT.10.0)R1=0.03125
!
!  Tertiary project are still exempt until payout. (CLEER)
!       IF(CUMINC.LE.CUMTAN)R1=0.
        if(ifbcase2.eq.1.AND.checkcf.lt.0.0)r1=0.0
         T1=R1*VALOIL
!  Casing head pressure must be 50 psi or less.
       R2=0.03
!  R2 was intended, not R1.
!           IF(WLPRGD.LT.250.)R1=0.013
           IF(WLPRGD.LT.250.)R2=0.013
!
!  Tertiary project are still exempt until payout. (CLEER)
!       IF(CUMINC.LE.CUMTAN)R2=0.
        if(ifbcase2.eq.1.AND.checkcf.lt.0.0)r2=0.0
         T2=R2*PRPRG*WIFRAC
!  A site restoration fee of $0.01 per barrel of oil; and $0.002 per
!  MCF gas could be imposed in proportion to reduced severance rates,
!  but may be suspended depending on revenues.
!  FACTOR = reduced severance factor
!   T3=(0.01*PRPRO+0.002*PRPRG)*WIFRAC*FACTOR
!  ******************************************
         SEVTAX=T1+T2
         RETURN
!
         ELSE IF(ISC.EQ.21) THEN
!**************************MICHIGAN******************************
! 210      CONTINUE
! TAX COMPUTATIONS FOR MICHIGAN
!
! ***NOTE: THERE IS A STRIPPER WELL EXEMPTION
        R1=0.066
!  -- RJH -- Fri Jan 20, 1995
!    A stripper oil severance tax of 4% for oil production of 10 BOPD
!  or less for wells with depths below 2000 feet; or 20 BOPD for depths
!  between 2000 and 4000 feet; or 25 BOPD between 4000 and 6000 feet;
!  or 30 BOPD between 6000 and 8000 feet; or for 35 BOPD or less produced
!  at 8000 feet or deeper.
!
      IF((DEPTH.LT.2000.AND.WLPROD.LE.10).OR. &
         (DEPTH.GE.2000.AND.DEPTH.LT.4000 &
          .AND.WLPROD.LE.20).OR. &
         (DEPTH.GE.4000.AND.DEPTH.LT.8000 &
          .AND.WLPROD.LE.25).OR. &
         (DEPTH.GE.8000.AND.WLPROD.LE.35)) R1=0.04
       IF(WLPROD.LE.5.0) R1=0.04

        T1=R1*VALOIL
        T2=0.05*VALGAS
        T3=0.01*(VALOIL+VALGAS)
        SEVTAX=T1+T2+T3
        RETURN
!
         ELSE IF(ISC.EQ.23) THEN
!**************************MISSISSIPPI***************************
! 220     CONTINUE
! TAX COMPUTATIONS FOR MISSISSIPPI
         T1=0.06*VALOIL +0.02*PRPRO*WIFRAC
        if(ifbcase2.eq.1)T1=0.03*VALOIL +0.02*PRPRO*WIFRAC


!  -- RJH -- Fri Jan 20, 1995
!  Oil severance Tax is reduced to 3% for EOR by carbon dioxide.
!  For CO2 predictive model use:
!        T1=0.03*VALOIL +0.02*PRPRO*WIFRAC
!  all other predictive models use 6%.
!
         T2=0.06*VALGAS + 0.002*PRPRG
         SEVTAX=T1+T2
         RETURN
!
         ELSE IF(ISC.EQ.25) THEN
!***************************MONTANA******************************
!
! COMPUTATION OF MONTANA SEVERANCE TAX
!
! -- RJH -- 12/15/94
!     The oil net proceeds tax is 5% of taxable value of incremental
!  production from secondary recovery projects begun after 12/31/93,
!  of 3.3% of value of tertiary recovery projects, and of 7% of the
!  total gross value for all other.
!
!     The gas net proceeds tax is 12% of value.
!
!    For tertiary recovery (All predictive models, except Infill,etc.)
!      T4=0.033*VALOIL

!    For secondary recovery ( for Infill drilling, etc.)
!      T4=0.05*VALOIL

!    For all other production
        T4=0.07*VALOIL

        if(ifbcase2.eq.1.and.iprocc.ge.9)t4=0.033*valoil
        if(ifbcase2.eq.1.and.iprocc.le.8)t4=0.05*valoil


!    For gas proceeds add 12% of gas value to oil proceeds tax.
      T4=T4+0.12*VALGAS
!   After notification to the Department, new production is exempt from
!   tax for the first 12 months from date of first production.
      IF( ISEVYR .LE. 1) THEN
        T4=0
      ENDIF
!    A Local Government Severance Tax (L.G.S.T.), T51,includes any well
!   which began production before 7/1/85.  Tertiary recovery is 3.3%
!   of incremental oil value, secondary recovery is 5% incremental oil
!   value, all other is 8.4% of total gross value, and 5% for qualified
!   strippers.
!       ( L.G.S.T. is IMPLEMENTED HERE)
!
!     Qualified stripper production of 10 BOPD at 5% Oil Value
!      IF(WLPROD .LE. 10) T51=0.05*VALOIL
!    Tertiary recovery projects at 3.3% Oil Value
!      T51=0.033*VALOIL +
!
!    Secondary recover projects ( Infill model) at 5% Oil Value
!     T51=0.05*VALOIL
!
!    All other production at 8.4% Oil Value
!     T51=0.084*VALOIL
!
!   The gas severance tax is 2.65% of regular production over 60 MCF/day,
!   and 1.59% of stripper production which is 60 MCF/day or less.
!
!      T52=0.1525*VALGAS
!      IF(WLPRGD .LE. 60) T52=0.10*VALGAS
!  T5 is the sum of the oil and gas L.G.S.T.
!     T5=T51+T52
!
!   The oil severance tax is 3% for secondary recovery commenced
!   after 12/13/93, 2% for tertiary recovery, and 5% for all
!   regular production.  No exemptions allowed.  Also an Oil Privilege
!   tax and License fee of 0.2% and an Oil Resource Indemnity Trust Tax
!   of 0.5% is added.  No exemptions.
!
!
       IF(WLPRGD .LE. 60) THEN
         T2=0.0159*( PRICEG*(WLPRG-30)*WIFRAC*NPAT )
         IF(T2 .LE. 0) T2=0
       ELSE
         T2=0.0265*VALGAS
       ENDIF
!
      T3=(0.002+ 0.005)*(VALOIL+VALGAS)
!      Regular production
!           T1=0.05*VALOIL
!      Tertiary 2%
!          T1=0.02*VALOIL
!      Secondary 3%
!           T1=0.03*VALOIL
! Default T1
            T1=0.05*VALOIL
        if(ifbcase2.eq.1.and.iprocc.ge.9)t1=0.02*valoil
        if(ifbcase2.eq.1.and.iprocc.le.8)t1=0.03*valoil

!
!   The old code is below: -- RJH
!
!           T1=0.05*VALOIL
!           T2=0.0265*VALGAS
!           T31=0.0008*(VALOIL+VALGAS)
!           T32=(VALOIL+VALGAS-5000.)
!           IF(T32.LE.0.)T32=0.
!           T3=T31+0.005*T32+25
!           SEVTAX=T1+T2+T3
!
!
!           SEVTAX=T1+T2+T3+T4+T5
           SEVTAX=T1+T2+T3+T4
         RETURN
!
         ELSE IF(ISC.EQ.26) THEN
!**************************NEBRASKA******************************
! 240     CONTINUE
! TAX COMPUTATIONS FOR NEBRASKA
! ***NOTE: THERE IS A STRIPPER WELL EXEMPTION
        R1=0.03
        IF(WLPROD.LT.10.0) R1=0.02
        T1=R1*VALOIL
            R2=0.03
!  -- RJH -- Mon Jan 23, 1995
!   Gas severance reduction to 2% is assumed to be the same with
!   a 60 MCF cut off (no information is available on this).
            IF(WLPRGD.LT.60.)R2=0.02
            T2=R2*VALGAS
!  -- RJH -- Mon Jan 23, 1995
!      A conservation tax of 0.0035% is set by Oil and Gas Conservation
! Commission (SPEE), not to exceed 4 mills per dollar of value at well (CCH).
!  Actual rate used here is 0.35% because of probable typo error with SPEE.
!        T3=0.0010*(VALOIL+VALGAS)
         T3=0.0035*(VALOIL+VALGAS)
        SEVTAX=T1+T2+T3
        RETURN
!
!  -- RJH -- Mon Jan 23, 1995
         ELSE IF(ISC.EQ.27) THEN
!**************************NEVADA********************************
!
! COMPUTATION OF NEVADA TAXES IS A NEW ADDITION -- RJH --Mon Jan 23, 1995
!    Conservation tax of 50 mills per barrel of oil or 50 MCF of gas (CCH),
!    not to exceed 5% of the net proceeds (SPEE).  Also permit fee to drill
!    well is $50.
      T3 = ( 0.050*PRPRO + 0.001*PRPRG )*WIFRAC
      IF(T3 .GT. 0.05*(VALOIL+VALGAS) ) T3=0.05*(VALOIL+VALGAS)
!     T6=50*PATI
      SEVTAX = T3+T6
      RETURN
         ELSE IF(ISC.EQ.30) THEN
!*************************NEW MEXICO*****************************
!
! COMPUTATION OF NEW MEXICO TAXES
!
!  -- RJH -- 12/15/94
!    Oil severance/production tax is 3.75% of Value plus
!    a Privilege Tax of 3.15% of Value.  (CLEER, Dec. 1993)
!
         T1=0.0375*VALOIL
!
!     Rate of 1.875% for Qualified Enhanced Oil Recovery
!     Projects when Texas Crude Price is below $28 per
!     barrel.  (CLEER, Dec. 1993)
!
!417DJR02   START
!        IF ((SEVPRICE.LT.28.10).AND.(IFBCASE2.EQ.1))
! -- RJH -- 12/15/94
!     Use $28.01 as the Texas crude price because of possible round off error.
        IF ((SEVPRICE.LE.28.01).AND.(IFBCASE2.EQ.1)) &
       T1=0.01875*VALOIL
!417DJR02   END
! ADD IN EMERGENCY SCHOOL TAX
         T1=T1+ 0.0315*VALOIL
!  -- RJH -- Dec 15, 1995
!    Gas severance/production tax is 3.75% of Value plus
!    a Privilege Tax of 4% of Value.  (CLEER, Dec. 1993)
!  The old gas severance tax was $0.152 per MCF for the total associated
!  gas production from the project during the year.
!         T2=0.152*PRPRG*WIFRAC

         T2=0.0375*VALGAS
! ADD IN EMERGENCY SCHOOL TAX
!         T2=T2 + 0.0315*VALGAS
! -- RJH -- 12/15/94
         T2=T2 + 0.04*VALGAS
! CONSERVATION TAX, Fund over $1,000,000
          T3=0.0018*(VALOIL+VALGAS)
! CONSERVATION TAX
!          T3=0.0019*(VALOIL+VALGAS)
!     LOCAL NEW MEXICO TAX
!     THE ASSESSED VALUE IS 150% OF THE NET
!         OIL AND GAS REVENUES
!      THE BASIS FOR THE TAX IS 50% OF ASSESSED VALUE
!     THE TAX RATE IS 1.5 %
!       T5=0.015*0.50*1.50*(VALOIL+VALGAS)
! -- RJH -- 12/15/94
!             (Local levy of 1.5% assumed)
       T5=0.015*0.333*1.50*(VALOIL+VALGAS)
!
!     Equipment is assessed at 33.3% of 27% of prior year's
!      value less exempt royalties x local levy
!              (CLEER, Dec. 1993)
!
! -- RJH -- End of changes
      SEVTAX = T1 + T2 + T3 + T5
      RETURN
!
         ELSE IF(ISC.EQ.31) THEN
!**************************NEW YORK******************************
! 260  CONTINUE
!     COMPUTATION OF TAXATION FOR NEW YORK
!      NEW YORK HAS NO SEVERANCE TAX
!  -- RJH -- Mon Jan 23, 1995
!  Crude oil is exempt from tax.  Gas sometimes is subject to a
!  supplemental tax.  Example, $0.005118/MCF during July to Aug. 1991.
!
        SEVTAX = 0.
        RETURN
!
         ELSE IF(ISC.EQ.33) THEN
!*************************NORTH DAKOTA***************************
!
!     COMPUTATION OF SEVERANCE TAXATION FOR NORTH DAKOTA
!
! -- RJH -- 12/15/94
!      T1 = 0.05 * VALOIL
!   The oil extraction tax rate is 6 1/2 % of the gross value at the well of
!   crude oil.  However, the rate is reduced to 4% for oil produced from
!   incremental oil from a qualifying secondary or tertiary recovery project,
!   after the 5-year or 10-year exemption expires
      T1= 0.065 * VALOIL
      IF (IFBCASE2.EQ.1) T1=0.04 * VALOIL
!
!417DJR02  START
!     IF ((IFBCASE2.EQ.1).AND.(ISEVYR.LE.10)) T1=0.0
      IF ((IFBCASE2.EQ.1).AND.(ISEVYR.LE.10).and.iprocc.ge.9)T1=0.0
      IF ((IFBCASE2.EQ.1).AND.(ISEVYR.LE.5).and.iprocc.le.8)T1=0.0

!417DJR02  END
!  -- RJH -- 12/15/94
!   Severance exemption for tertiary oil for 10 years
!             for secondary recovery oil for  5 years
!    Exemptions for stripper wells are defined as:
!    10 BOPD for depth � 6000'
!    15 BOPD for depth > 6000' & � 10000'
!    20 BOPD for depth > 10000'
!
      IF( ( WLPROD .LE. 10 .AND. DEPTH .LE. 6000) .OR. &
         (WLPROD .LE. 15 .AND. &
         (DEPTH .LE. 10000 .AND. DEPTH .GT. 6000)) .OR. &
         (WLPROD .LE. 20 .AND. DEPTH .GT. 10000) ) T1=0
!
!   Royalty interest in oil from an Federal Land holding is
!    exempt from the oil tax.
!
!     EXEMPTION FROM INITIATIVE NO.6 SURTAX FOR STRIPPER WELLS
!      IF(WLPROD .GT. 5) T1 = T1 + 0.065 * VALOIL
      T2 = 0.05 * VALGAS
! -- RJH -- 12/15/94
!   Royalty interest in gas from an Federal Land holding is
!    exempt from the gas tax.
!
!   A gross production tax is 5% is lieu of property tax
!     This applied to gross except for Indian land and Govt. land
!
      T4=.05*VALOIL
! -- RJH -- 12/15/94
!      SEVTAX = T1 + T2
      SEVTAX = T1 + T2 + T4
      RETURN
!
         ELSE IF(ISC.EQ.34) THEN
!**************************OHIO**********************************
! 280  CONTINUE
!      COMPUTATION OF SEVERANCE TAXATION FOR OHIO
      T1 = 0.10 * PRPRO * WIFRAC
      T2 = 0.025 * PRPRG * WIFRAC
      SEVTAX = T1 + T2
      RETURN
!
         ELSE IF(ISC.EQ.35) THEN
!******************************OKLAHOMA**************************
!
!     COMPUTATION OF SEVERANCE TAXATION FOR OKLAHOMA
! -- RJH -- 12/15/94
       T1=(0.07 + 0.00095)*VALOIL
!       T1=(0.07 + 0.00085)*VALOIL
!417DJR02  START
      IF ((IFBCASE2.EQ.1).AND.(ISEVYR.LE.3).AND.(CHECKCF.LE.0.)) T1=0.
!417DJR02  END
! -- RJH -- 12/15/94
       T2=(0.07 + 0.00095)*VALGAS
!       T2=(0.07 + 0.00085)*VALGAS
       T31=(VALGAS/WIFRAC)/3.
      T32 = 0.07 * PRPRG - 0.07 * VALGAS
      T3 = T32
      IF(T32 .LE. 0.0) T3 = 0.
      IF(T32 .GT. T31) T3 = T31
      SEVTAX = T1 + T2 + T3
      RETURN
!
! -- RJH -- Mon Jan 23, 1995
!  The state of Oregon has been added to SEVTAX.
         ELSE IF(ISC.EQ.36) THEN
!*****************************OREGON*****************************
!  Oil and gas severance base tax rates are each 6%.
!  First $3000.00 Gross sales of each quarter is exempt.
!
!   Exemption of first $3000.00 value
!   First check for zero values
      IF((VALOIL+VALGAS).GT.0.0) THEN
!   Now calculate the exemption ratios for oil and gas
        R1=VALOIL/(VALOIL+VALGAS)
        R2=VALGAS/(VALOIL+VALGAS)
!   Subtract exemption from quarterly gross in proportion to oil or gas.
        T11 = VALOIL/4 - 3000.0*R1
        T21 = VALGAS/4 - 3000.0*R2
!  Where T11 is the oil gross income after exemption and T21 is
!  the gas gross income after exemption for the quarter.
!
        IF(T11 .LE. 0.0) T11 = 0.0
        IF(T21 .LE. 0.0) T21 = 0.0
      ELSE
        T11=0
        T21=0
      ENDIF
!  For 4 quarters in the year the severance is reduced.
      T1=0.06*T11*4
      T2=0.06*T21*4
      SEVTAX=T1+T2
      RETURN
         ELSE IF(ISC.EQ.37) THEN
!**************************PENNSYLVANIA**************************
! 300   CONTINUE
!     COMPUTATION OF SEVERANCE TAXATION FOR PENNSYLVANIA
!     PENNSYLVANIA HAS NO SEVERANCE TAX
!  However, an additional 110 mills per dollar of petroleum revenue
!  is added to the current corporate tax for oil companies.  This is
!  an 11% addition.  (This has not been implemented as yet)
         SEVTAX = 0.0
         RETURN
         ELSE IF(ISC.EQ.40) THEN
!**************************SOUTH DAKOTA**************************
! 310  CONTINUE
!     COMPUTATION OF SEVERANCE TAXATION FOR SOUTH DAKOTA
      T1 = 0.045 * VALOIL
      T2 = 0.045 * VALGAS
      T3 = 0.00240 * (VALOIL + VALGAS)
!  -- RJH --Mon Jan 23, 1995
!  The severance tax is still 4.5% plus 2.4 mills of taxable value.
!   No drilling tax per well pattern is mentioned in any tax source.
!      T6 = 100. * PATI
      SEVTAX = T1 + T2 + T3 + T6
      RETURN
         ELSE IF(ISC.EQ.41) THEN
!**************************TENNESSEE*****************************
! 320  CONTINUE
!     COMPUTATION OF SEVERANCE TAXATION FOR TENNESSEE
      T1 = 0.03 * VALOIL
      T2 = 0.03 * VALGAS
!  -- RJH -- Mon Jan 23, 1995
!  The severance tax is the same, however, no mention is made of a
!  drilling tax.
!      T6 = 50. * PATI
      SEVTAX = T1 + T2 + T6
      RETURN
         ELSE IF(ISC.EQ.42) THEN
!**************************TEXAS*********************************
! 330  CONTINUE
!     COMPUTATION OF SEVERANCE TAXATION FOR TEXAS
      IF(PRICEO .LT. 1.0) T1 = 0.046 * PRPRO * WIFRAC
      IF(PRICEO .GE. 1.0) T1 = 0.046 * VALOIL
!  -- RJH -- Mon Jan 23, 1995
!      An enhanced oil recovery reduced rate of 2.3% severance
!  tax for 10 years that begin after 12/31/89 but before 1/1/98
!      An enhanced oil recovery reduced rate of 2.3% severance
!  tax for 10 years on incremental production from EOR projects
!  that begin after 9/1/89 and expanded after 8/31/91.
!
!417DJR02   START
      IF ((IFBCASE2.EQ.1).AND.(ISEVYR.LE.10)) T1=0.023*VALOIL
!417DJR02   END
      T21 = (0.01 * 121./1500.) * PRPRG *WIFRAC
      T2 = 0.075 * VALGAS
      IF(T2 .LT. T21) T2 = T21
!  -- RJH -- Mon Jan 23, 1995
!  A new oil Clean-Up tax 5/16 of 1 cent ($0.003125) per barrel oil is added.
!      T3 = (0.01 * 3./16.) * PRPRO * WIFRAC
!  A new gas Clean-Up tax 0.003 cents ($0.00003) per MCF gas is added.
      T3 = (0.01*PRPRO + 0.00003*PRPRG) * WIFRAC
      SEVTAX = T1 + T2 + T3
      RETURN
         ELSE IF(ISC.EQ.43) THEN
!******************************UTAH******************************
!
!     COMPUTATION OF SEVERANCE TAXATION FOR UTAH
!     EXEMPTION OF FIRST 50000 VALUE
!     FIRST CHECK FOR ZERO VALUES
      IF((VALOIL+VALGAS).LE.0.0)GO TO 341
!     NOW CALCULATE EXEMPTION
           R1=VALOIL/(VALOIL+VALGAS)
           R2=VALGAS/(VALOIL+VALGAS)
           T11=(VALOIL+VALGAS-50000.)*R1
           T21=(VALOIL+VALGAS-50000.)*R2
      IF(T11 .LE. 0.0) T11 = 0.0
      IF(T21 .LE. 0.0) T21 = 0.
!     EXEMPTION OF FIRST 6 MONTH'S PRODUCTION FOR
!     NEWLY DRILLED WELLS
      IF(ISEVYR.LE.1) THEN
        T111 = WLPRO * PATI * (6./12.) * PRICEO * WIFRAC
!   -- RJH -- 12/15/94
!   WLPRGD is a daily production of gas from a well, MCF/D
!     where it should be WLPRG or an annual production per well
!      T211 = WLPRGD * PATI * (6./12.) * PRICEG * WIFRAC
        T211 = WLPRG * PATI * (6./12.) * PRICEG * WIFRAC
      ENDIF
!
      T11 = T11 - T111
      IF(T11 .LE. 0.0) T11 = 0.
      T21 = T21 - T211
      IF(T21 .LE. 0.0) T21 = 0.0
!     EXEMPTION FOR STRIPPER WELLS
      IF(WLPROD .LE. 20.0) T11 = 0.
      IF(WLPRGD.LE. 60.0) T21 = 0.
!     APPLICATION OF SEVERANCE TAX RATES
!      T1 = 0.05 * T11
!      T2 = 0.05 * T21
!
!  Severance is 3% of the value up to the first $13 per barrel
!  for oil and $1.50 per MCF for gas, and 5% of the value from
!  $13.01 and over per barrel for oil and $1.51 and over per MCF
!  for gas.
!
      IF(PRICEO .LE. 13.0) THEN
        T1 = 0.03*T11
      ELSE
        T1 = (0.03*13.00*T11/PRICEO) &
           + (0.05*(PRICEO-13.00)*T11/PRICEO)
      ENDIF
      IF(PRICEG .LE. 1.50) THEN
        T2 = 0.03*T21
      ELSE
        T2 = (0.03*1.50*T21/PRICEG) &
           + (0.05*(PRICEG-1.50)*T21/PRICEG)
      ENDIF
 341  CONTINUE
!     CONSERVATION TAX
! -- RJH -- 1/16/95 - T3 COULD BE NEGATIVE HERE IF PRPRO OR PRPRG IS NEGATIVE
      T3 = 0.002 * (VALOIL + VALGAS)
      SEVTAX = T1 + T2 + T3
      RETURN
!
! -- RJH -- Mon Jan 23, 1995
!  The state of Virginia has been added to SEVTAX.
         ELSE IF(ISC.EQ.45) THEN
!****************************VIRGINIA****************************
!
!  Oil severance tax base rate is 0.5%
!  Gas severance tax base rate is 2%
!  Local road improvement tax is 1% on oil and gas.
      T1=0.015*VALOIL
      T2=0.03*VALGAS
      SEVTAX=T1+T2
      RETURN
         ELSE IF(ISC.EQ.47) THEN
!**************************WEST VIRGINIA*************************
! 350  CONTINUE
!     COMPUTATION OF SEVERANCE TAXATION FOR W. VIRGINIA
! -- RJH -- Mon Jan 23, 1995
!     The oil severance tax base rate is 5% of Value.
!       The severance tax on other natural resources (gas)
!   base rate is 5% of Value.
!
!      T1 = 0.0434 * VALOIL
      T1 = 0.05*VALOIL
!  There is no first $5000 exemption on gas values listed.
!      T21 = VALGAS - 5000.
!      IF(T21 .LE. 0.0) T21 = 0.
!      T2 = 0.0863 * T21
      T2= 0.05*VALGAS
!
!     CONSERVATION TAX IS BASED ON ACREAGE
!       COMPUTED AS 20 AC. PER PATTERN
! -- RJH -- Mon Jan 23, 1995
!   The conservation tax is no longer mentioned.
!      T3 = 0.03 * 20. * NPAT
      SEVTAX = T1 + T2 + T3
      RETURN
! -- RJH -- Mon Jan 23, 1995
!  The state of Wisconsin has been added to SEVTAX.
         ELSE IF(ISC.EQ.48) THEN
!***************************WISCONSIN****************************
!
!  Oil severance tax base rate is 7%
!  Gas severance tax base rate is 7%
!
      T1=0.07*VALOIL
      T2=0.07*VALGAS
      SEVTAX=T1+T2
      RETURN
         ELSE IF(ISC.EQ.49) THEN
!*****************************WYOMING****************************
!
!     COMPUTATION OF SEVERANCE TAXATION FOR WYOMING
!         2% TAX ON "VALUABLE RESOURCE"
!             WHICH IS EXEMPTED FOR STRIPPER WELLS
!         2% OIL AND GAS SEVERANCE TAX
!         2% GROSS VALUE TAX
!   THIS IS A TOTAL OF 6% (OR 4% FOR STRIPPER WELLS)
!      IF(WLPROD.LE.10.0) T1 = 0.055 * VALOIL
!      IF(WLPROD.GT.10.0) T1 = 0.075 * VALOIL
!      T2 = 0.075 * VALGAS
!
!   -- RJH -- 12/15/94
!     New wells drilled between 7/1/93 and 12/31/96 only have
!     a 2 % severance on the first 40 BOPD or of the first
!     240 MCFD for 24 months, then back to full 6% thereafter.
!
!     For workovers and recompletions between 7/1/93 and 12/31/96, the
!     incremental production is taxed at 2% for the first 24 months
!     and then back to full 6%
!
!   Stripper Well Exemption -- Reduced severance tax rate to 4% for 5 years,
!    rather than 6%.  Oil only.
!
      IF(WLPROD.LE.10.0 .AND. ISEVYR .LE. 5) T1 = 0.04 * VALOIL
      IF(WLPROD.GT.10.0) T1 = 0.06 * VALOIL
!
!     TERTIARY RECOVERY PROJECTS -- OIL ONLY
!
      IF((IFBCASE2.EQ.1).AND.(ISEVYR.LE.5).AND.(CHECKCF.LE.0)) &
         T1=0.04*VALOIL
!
      T2 = 0.06 * VALGAS

!     CONSERVATION TAX
!      T3 = 0.0002 * (VALOIL + VALGAS)
!   -- RJH -- 12/15/94
      T3 = 0.0006 * (VALOIL + VALGAS)
! THE CONSERVATION TAX WILL BE RAISED TO 0.0008 WHEN
!    THE LEGISLATURE AUTHORIZES A NEW "OIL AND GAS BUILDING"
!     T6=25.*NPAT
      SEVTAX = T1 + T2 + T3 + T6
      RETURN
!
      ELSE
!   The state is not included in the 27 older states and the 7 newer ones.
!
! *** DEFAULT CALCULATION IN CASE ANY STATES THAT
!      ARE NOT ON THE LIST CREEP IN
!
       SEVTAX=0.05*(VALOIL+VALGAS)
        RETURN
        ENDIF
!
!

       END SUBROUTINE
!***************************************************************
!from ECON_STATE.FOR
!     Last change:  MC   17 Mar 2008    3:13 pm
       SUBROUTINE STATE_MATCH(ST,IST)

!  This subroutine matches the state abbreviation, from the resid, to an integer value.
!------------------------------------------------------------------------
! This subroutine converts a two-character state postal code
! to an integer code for cost/tax calculations
!------------------------------------------------------------------------
      implicit none

      CHARACTER*1 ST1(2)
      character*2 st,ST2
      character*2 STATE(52)
      INTEGER IST,i,j,k,ipos
      DATA (STATE(K),K=1,6)/'AL','AZ','AR','CA','CO','CT'/
      DATA (STATE(K),K=7,12)/'DE','DC','FL','GA','ID','IL'/
      DATA (STATE(K),K=13,18)/'IN','IA','KS','KY','LA','ME'/
      DATA (STATE(K),K=19,24)/'MD','MA','MI','MN','MS','MO'/
      DATA (STATE(K),K=25,30)/'MT','NE','NV','NH','NJ','NM'/
      DATA (STATE(K),K=31,36)/'NY','NC','ND','OH','OK','OR'/
      DATA (STATE(K),K=37,42)/'PA','RI','SC','SD','TN','TX'/
      DATA (STATE(K),K=43,48)/'UT','VT','VA','WA','WV','WI'/
      DATA (STATE(K),K=49,52)/'WY','AK','HI','PO'/
      EQUIVALENCE (ST1,ST2)
!
      ST2 = ST
!
! For the purposes of the table lookup
! convert the state postal code to upper case.
!
      DO I=1,2
        IPOS = ICHAR(ST1(I))
        IF ( IPOS .GT. 90 ) THEN
          IPOS = IPOS - 32
          ST1(I) = CHAR(IPOS)
        END IF
      END DO
!
! Now convert the state postal code.
!
      DO IST=1,52
        IF( ST2 .EQ. STATE(IST) ) RETURN
      END do

      IST = 0
      RETURN

       END SUBROUTINE
       
!***************************************************************
!from ECON_STTAX.for
!     Last change:  MC    9 Jun 2008    1:49 pm
!  Tue Jan 26, 1995
!   sttax94.f  1.1 10/17/91
!  formerly Newsttax.f
!     STTAX.FTN
!     ROUTINE TO CALCULATE THE STATE TAX FOR EACH YEAR INPUT
!
!     CALL IS AS FOLLOWS (IN SUBROUTINE ECON):
!          ASTX(I)=STTAX(ISTATE,ANOI(I),AREV(I))
!
!
      FUNCTION STTAX(ISTATE,XNOI,XGROSS)
      REAL SRATE(52)
!
! -- RJH -- 12/15/94  Addition  of a new function to calculate
!  the state corporate tax when a flat fixed rate is not present.
!
!  The function GRADTAX returns the corporate income tax for
!  any state that has different rates for individual tax
!  brackets.  The RST is the rate, and BST is the upper level
!  of the bracket.  The last rate applies to all income above
!  the previous bracket.
!   To use:   STAX = GRADTAX(XNOI,R,B)
!
       REAL RAK(12),BAK(12),  RAR(12),BAR(12)
       REAL RKS(12),BKS(12),  RKY(12),BKY(12)
       REAL RLA(12),BLA(12),  RMI(12),BMI(12)
       REAL RMS(12),BMS(12),  RNE(12),BNE(12)
       REAL RNM(12),BNM(12),  RND(12),BND(12)
       REAL ROH(12),BOH(12),  RSD(12),BSD(12)
!
!   where R and B are:
!
!  Alaska graded state corporate tax structure.
       DATA (RAK(I),I=1,12) /1,2,3,4,5,6,7,8,9,9.4,0,0/
       DATA (BAK(I),I=1,12) / 10000,20000,30000,40000, &
                             50000,60000,70000,80000, &
                             90000,    0,    0,    0/
!  Arkansa graded state corporate tax structure.
!  The $50,000 to $100,000 bracket is assumed to be 6%.
       DATA (RAR(I),I=1,12) /1,2,3,5,6,6,6.5,0,0,0,0,0/
       DATA (BAR(I),I=1,12) / 3000, 6000,11000,25000, &
                              50000,100000, 0,0,0,0,0,0/
!  Kansas graded state corporate tax structure.
       DATA (RKS(I),I=1,12) /4,7.35,0,0,0,0,0,0,0,0,0,0/
       DATA (BKS(I),I=1,12) /50000, 0, 0, 0, 0, 0, &
                                  0, 0, 0, 0, 0, 0/
!  Kentucky graded state corporate tax structure.
       DATA (RKY(I),I=1,12) /4,5,6,7,8.25,0,0,0,0,0,0,0/
       DATA (BKY(I),I=1,12) /25000, 50000, 100000, 250000, 0, 0, &
                                  0, 0, 0, 0, 0, 0/
!  Louisiana graded state corporate tax structure.
       DATA (RLA(I),I=1,12) /4,5,6,7,8,0,0,0,0,0,0,0/
       DATA (BLA(I),I=1,12) /25000,50000,100000,200000, &
                               0, 0, 0, 0,   0, 0, 0, 0/
!  Michigan graded state corporate tax structure.
       DATA (RMI(I),I=1,12) /0,2.35,0,0,0,0,0,0,0,0,0,0/
       DATA (BMI(I),I=1,12) /45000, 0, 0, 0, 0, 0, &
                                  0, 0, 0, 0, 0, 0/
!  Mississippi graded state corporate tax structure.
       DATA (RMS(I),I=1,12) /3,4,5,0,0,0,0,0,0,0,0,0/
       DATA (BMS(I),I=1,12) /5000, 10000, 0, 0, 0, 0, &
                                  0, 0, 0, 0, 0, 0/
!  Nebraska graded state corporate tax structure.
       DATA (RNE(I),I=1,12) /5.58,7.81,0,0,0,0,0,0,0,0,0,0/
       DATA (BNE(I),I=1,12) /50000, 0, 0, 0, 0, 0, &
                                  0, 0, 0, 0, 0, 0/
!  New Mexico graded state corporate tax structure.
       DATA (RNM(I),I=1,12) /4.8,6.4,7.6,0,0,0,0,0,0,0,0,0/
       DATA (BNM(I),I=1,12) / 500000,1000000,0,0,0,0, &
                                    0,      0,0,0,0,0/
!  North Dakato graded state corporate tax structure.
       DATA (RND(I),I=1,12) /2.67,4.1,5.6,6.4,7,0,0,0,0,0,0,0/
       DATA (BND(I),I=1,12) / 3000,8000,20000,30000,0,0, &
                                  0,   0,    0,    0,    0,0/
!  Ohio graded state corporate tax structure.
       DATA (ROH(I),I=1,12) /5.51,8.9,0,0,0,0,0,0,0,0,0,0/
       DATA (BOH(I),I=1,12) /50000, 0, 0, 0, 0, 0, &
                                  0, 0, 0, 0, 0, 0/
!  South Dakota graded state corporate tax structure.
       DATA (RSD(I),I=1,12) /6,5,4,3,2,1,0.5,0.25,0,0,0,0/
       DATA (BSD(I),I=1,12) /400000, 425000, 450000, &
                              475000, 500000,   600000, &
                                1200000, 0, 0,   0, 0, 0/
!  -- RJH --  Jan 19, 1995   The new 1994 corporate rates.
      DATA (SRATE(K),K=1,10)/5.0,9.0,6.0,9.3,5.0,11.5,8.7,9.975,5.5, &
                             6.0/
      DATA (SRATE(K),K=11,20)/8.0,7.2,7.9,12.0,6.75,6.0,8.0,8.93,7.0, &
                              9.5/
      DATA (SRATE(K),K=21,30)/2.3,9.8,4.0,5.0,6.75,6.65,0.0,8.0,9.0, &
                              7.2/
      DATA (SRATE(K),K=31,40)/9.0,8.75,7.5,8.9,6.0,6.6,11.99,9.0,5.,6./
      DATA (SRATE(K),K=41,51)/6.0,0.,5.0,8.25,6.0,0.,9.0,7.9,0.,9.4,0./
!
!  -- RJH -- Old rates in variable array SRATE is shown below:
!      DATA (SRATE(K),K=1,10)/5.0,10.5,6.0,9.6,5.0,11.5,8.7,5.0,6.0,
!     A     6.435/
!      DATA (SRATE(K),K=11,20)/7.7,6.5,7.0,12.0,6.75,6.0,8.0,8.93,7.0,
!     A     9.496/
!      DATA (SRATE(K),K=21,30)/2.35,12.0,4.0,5.0,6.75,6.65,0.0,8.0,9.4,
!     A       7.2/
!      DATA (SRATE(K),K=31,40)/10.0,6.,7.5,9.2,4.0,7.5,10.5,8.0,6.,6./
!      DATA (SRATE(K),K=41,51)/6.0,0.,5.0,9.0,6.0,0.,7.0,7.9,0.,9.4,0./
!
      STAX = 0.
      GO TO 555
      IF ( ISTATE .EQ. 50 ) THEN
!**************************** ALASKA *****************************
!  -- RJH -- Wed Jan 18, 1995
!   Graded Rates for Corporate State Tax of Alaska
!
        STAX=GRADTAX(XNOI,RAK,BAK)
!
      ELSE IF ( ISTATE .EQ. 3 ) THEN
!*************************ARKANSAS********************************
!  -- RJH -- Thu Jan 19, 1995
!   Graded Rates for Corporate State Tax of Arkansas
!
        STAX=GRADTAX(XNOI,RAR,BAR)
!
      ELSE IF ( ISTATE .EQ. 15 ) THEN
!**************************KANSAS*********************************
!  -- RJH -- Thu Jan 19, 1995
!   Graded Rates for Corporate State Tax of Kansas
!
        STAX=GRADTAX(XNOI,RKS,BKS)
!
      ELSE IF ( ISTATE .EQ. 16 ) THEN
!*************************KENTUCKY********************************
!  -- RJH -- Thu Jan 19, 1995
!   Graded Rates for Corporate State Tax of Kentucky
!
        STAX=GRADTAX(XNOI,RKY,BKY)
!
      ELSE IF ( ISTATE .EQ. 17 ) THEN
!*************************LOUISIANA*******************************
!  -- RJH -- Thu Jan 19, 1995
!   Graded Rates for Corporate State Tax of Louisiana
!
        STAX=GRADTAX(XNOI,RLA,BLA)
!
      ELSE IF ( ISTATE .EQ. 21 ) THEN
!*************************MICHIGAN********************************
!  -- RJH -- Thu Jan 19, 1995
!   Graded Rates for Corporate State Tax of Michigan
!
        STAX=GRADTAX(XNOI,RMI,BMI)
!
      ELSE IF ( ISTATE .EQ. 23 ) THEN
!*************************MISSISSIPPI*****************************
! -- RJH -- Thu Jan 19, 1995
!   Graded Rates for Corporate State Tax of Mississippi
!
        STAX=GRADTAX(XNOI,RMS,BMS)
!
      ELSE IF ( ISTATE .EQ. 26 ) THEN
!*************************NEBRASKA********************************
! -- RJH -- Thu Jan 19, 1995
!   Graded Rates for Corporate State Tax of Nebraska
!
        STAX=GRADTAX(XNOI,RNE,BNE)
!
      ELSE IF ( ISTATE .EQ. 30 ) THEN
!*************************NEW MEXICO******************************
! -- RJH -- 12/15/94
!     NEW MEXICO STATE TAX CALCULATED HERE
!
!   Income tax is 4.8% on the first $500,000, 6.4% on the next $500,000,
!    and 7.6% on all amounts over $1,000,000.
!
        STAX=GRADTAX(XNOI,RNM,BNM)
!
!**************************NEW YORK*******************************
      ELSE IF ( ISTATE .EQ. 31 ) THEN
!
!     NEW YORK STATE TAX CALCULATED HERE
!
!  -- RJH -- 1/26/95
!   The principle corporate state tax is 9% with a 5% surcharge after
!   June 30, 1995 and eliminated after June 30, 1996.
!   Additional 0.9 mill per $1 tax on allocated subsidiary capital.
!        STAX = (0.1*XNOI) + (0.0075*XGROSS)
        STAX = (0.09*XNOI) + (0.009*XGROSS)
!
      ELSE IF ( ISTATE .EQ. 33 ) THEN
!*************************NORTH DAKOTA****************************
! -- RJH -- 12/15/94
!     NORTH DAKOTA STATE CORPORATE TAX CALCULATED HERE
!
!  -- UPDATED MC 6.02.08
!
!      ADDITIONAL STATE TAXES: $78 FOR INCOME BETWEEN 3,001 AND 8,000
!                             $245 FOR INCOME BETWEEN 8,001 AND 20,000
!                             $995 FOR INCOME BETWEEN 20,001 AND 30,000
!                           $1,595 FOR INCOME ABOVE   30,000

        STAX=GRADTAX(XNOI,RND,BND)

        IF(xnoi.gt.3000.and.xnoi.le.8000) STAX = STAX + 0.078
        IF(xnoi.gt.8000.and.xnoi.le.20000) STAX = STAX + 0.245
        IF(xnoi.gt.20000.and.xnoi.le.30000) STAX = STAX + 0.995
        IF(xnoi.gt.30000) STAX = STAX + 1.595

      ELSE IF ( ISTATE .EQ. 34 ) THEN
!****************************OHIO*********************************
! -- RJH -- Thu Jan 19, 1995
!   Graded Rates for Corporate State Tax of Ohio
!
        STAX=GRADTAX(XNOI,ROH,BOH)
!
      ELSE IF ( ISTATE .EQ. 40 ) THEN
!*************************SOUTH DAKOTA*****************************
! -- RJH -- Thu Jan 19, 1995
!   Graded Rates for Corporate State Tax of South Dakota
!
        STAX=GRADTAX(XNOI,RSD,BSD)
!
      ELSE
!*****************************************************************
!     ALL STATES WITH FIXED RATE CALCULATED HERE
         STAX = XNOI * SRATE(ISTATE )/100.0
!
!  This is remainder of the old code with the exception of the last 2 lines.
!
 & !     GO TO (10,20,30,10,10,530,530,530,10,530,530,10,10,
 & !     530,150,160,170,530,530,530,10,530,230,530,10,260,530,530,
 & !     530,300,310,530,330,340,10,530,10,530,530,530,10,530,10,
!     530,530,530,470,530,530,500,530,530) ISTATE
!     ALL STATES WITH FIXED RATE CALCULATED HERE
!10   STAX = XNOI * SRATE(ISTATE)
!     GO TO 530
!     ARIZONA STATE TAX CALCULATED HERE
!20   IF(XNOI.LE.1000.0) STAX = STAX + 0.025 * XNOI
!     IF(XNOI.GT.1000.0) STAX = 25.00
 & !     IF(XNOI.GT.1000.0 .AND. XNOI.LE.2000.)
!       STAX = STAX + 0.04*(XNOI-1000.)
!     IF(XNOI.GT.2000.0) STAX = STAX + 40.0
 & !     IF(XNOI.GT.2000.0 .AND. XNOI.LE.3000.)
!       STAX = STAX + 0.05*(XNOI-2000.)
!     IF(XNOI.GT.3000.) STAX = STAX + 50.0
 & !     IF(XNOI.GT.3000.0 .AND. XNOI.LE.4000.0)
!       STAX = STAX + 0.065*(XNOI-3000.)
!     IF(XNOI.GT.4000.0) STAX = STAX + 65.00
 & !     IF(XNOI.GT.4000.0 .AND. XNOI.LE.5000.0)
!       STAX = STAX + 0.08*(XNOI-4000.0)
!     IF(XNOI.GT.5000.0) STAX = STAX + 80.0
 & !     IF(XNOI.GT.5000.0 .AND. XNOI.LE.6000.0)
!        STAX = STAX + 0.09*(XNOI-5000.0)
!     IF(XNOI.GT.6000.0) STAX = STAX + 90.0 + 0.105*(XNOI-6000.0)
!     GO TO 530
!     ARKANSAS STATE TAX CALCULATED HERE
!30   IF(XNOI.LE.3000.0) STAX = STAX + 0.01*XNOI
!     IF(XNOI.GT.3000.0) STAX = STAX + 30.0
 & !     IF(XNOI.GT.3000.0 .AND. XNOI .LE. 6000.0)
!        STAX = STAX + 0.02*(XNOI-3000.0)
!     IF(XNOI.GT.6000.0) STAX = STAX + 60.0
 & !     IF(XNOI.GT.6000.0 .AND. XNOI.LE.11000.0)
!        STAX = STAX + 0.03*(XNOI-6000.0)
!     IF(XNOI.GT.11000.0) STAX = STAX + 150.00
 & !     IF(XNOI.GT.11000.0 .AND. XNOI.LE.25000.0)
!        STAX = STAX + 0.05*(XNOI-11000.0)
!     IF(XNOI.GT.25000.0) STAX = STAX + 700.00 + 0.06*(XNOI-25000.0)
!     GO TO 530
!     KANSAS STATE TAX CALCULATED HERE
!150  STAX = STAX + 0.045*XNOI
!     IF(XNOI.GE.25000.0) STAX = STAX + 0.0225*(XNOI-25000.0)
!     GO TO 530
!     KENTUCKY STATE TAX CALCULATED HERE
!160  IF(XNOI.LE.25000.0) STAX = 0.03*XNOI
!     IF(XNOI.GT.25000.0)STAX = 750.00
 & !     IF(XNOI.GT.25000.0 .AND. XNOI.LE.50000.0)
!        STAX = STAX + 0.04*(XNOI-25000.)
!     IF(XNOI.GT.50000.0) STAX = STAX + 1000.00
 & !     IF(XNOI.GT.50000.0 .AND. XNOI.LE.100000.0)
!        STAX = STAX + 0.05*(XNOI-50000.0)
!     IF(XNOI.GT.100000.0) STAX=STAX+2500.00+0.06*(XNOI-100000.0)
!     GO TO 530
!     LOUSISANA STATE TAX CALCULATED HERE
!170  IF(XNOI.LE.25000.0) STAX=0.04*(XNOI)
!     IF(XNOI.GT.25000.0) STAX=1000.00
 & !     IF(XNOI.GT.25000.0 .AND. XNOI.LE.50000.0)
!        STAX=STAX+0.05*(XNOI-25000.0)
!     IF(XNOI.GT.50000.0) STAX=STAX+1250.00
 & !     IF(XNOI.GT.50000.0 .AND. XNOI.LE.100000.0)
!        STAX=STAX+0.06*(XNOI-50000.0)
!     IF(XNOI.GT.100000.0) STAX = STAX + 3000.0
 & !     IF(XNOI.GT.100000.0 .AND. XNOI.LE.200000.0)
!        STAX = STAX + 0.07*(XNOI-100000.0)
!     IF(XNOI.GT.200000.0) STAX=STAX + 7000.00+0.08*(XNOI-200000.0)
!     GO TO 530
!     MISSISSIPPI STATE TAX CALCULATED HERE
!230  IF(XNOI.LE.5000.0) STAX=0.03*XNOI
!     IF(XNOI.GT.5000.0) STAX=150.00
 & !     IF(XNOI.GT.5000.0 .AND. XNOI.LE.10000.0)
!        STAX = STAX + 0.04*(XNOI-5000.)
!     IF(XNOI.GT.10000.0) STAX=STAX+200.00+0.05*(XNOI-10000.0)
!     GO TO 530
!     NEBRASKA STATE TAX CALCULATED HERE
!260        CONTINUE
!     IF(XNOI.LE.50000.0) STAX = 0.0475*XNOI
!     IF(XNOI.GT.50000.0) STAX=2375.00+0.0665*(XNOI-50000.0)
!     GO TO 530
!     NEW MEXICO STATE TAX CALCULATED HERE
!300  IF(XNOI.LE.1000000.0)STAX=0.048*XNOI
!     IF(XNOI.GT.1000000.0) STAX=48000.00
 & !     IF(XNOI.GT.1000000.0 .AND. XNOI.LE.2000000.0)
!        STAX=STAX+0.06*(XNOI-1000000.0)
!     IF(XNOI.GT.2000000.0) STAX=STAX+60000.00+0.072*(XNOI-2000000.0)
!     GO TO 530
!     NEW YORK STATE TAX CALCULATED HERE
! 310  STAX = (0.1*XNOI) + (0.0075*XGROSS)
!      GO TO 530
!     NORTH DAKOTA STATE TAX CALCULATED HERE
!330  IF(XNOI.LE.3000.0) STAX=0.03*XNOI
!     IF(XNOI.GT.3000.0) STAX=90.00
 & !     IF(XNOI.GT.3000.0 .AND. XNOI.LE.8000.0)
!        STAX=STAX + 0.045*(XNOI-3000.0)
!     IF(XNOI.GT.8000.0) STAX=STAX + 225.00
 & !     IF(XNOI.GT.8000.0 .AND. XNOI.LE.20000.0)
!        STAX=STAX + 0.06*(XNOI-8000.0)
!     IF(XNOI.GT.20000.0) STAX=STAX + 720.00
 & !     IF(XNOI.GT.20000.0 .AND. XNOI.LE.30000.0)
!        STAX=STAX + 0.075*(XNOI-20000.0)
!     IF(XNOI.GT.30000.0) STAX= STAX + 750.00
 & !     IF(XNOI.GT.30000.0 .AND. XNOI.LE.50000.0)
!        STAX = STAX + 0.09*(XNOI-30000.0)
!     IF(XNOI.GT.50000.0) STAX=STAX+1800.00+0.105*(XNOI-50000.0)
!     GO TO 530
!     OHIO STATE TAX CALCULATED HERE
!340  IF(XNOI.LE.25000.0) STAX=0.051*XNOI
!     IF(XNOI.GT.25000.0)STAX=1275.00+0.092*(XNOI-25000.0)
!     STAX = STAX + 0.054*(XNOI)
!     GO TO 530
!     WEST VIRGINIA STATE TAX CALCULATED HERE
!470  IF(XNOI.LE.50000.0) STAX=0.06*XNOI
!     IF(XNOI.GT.50000.0)STAX=3000.0+0.07*(XNOI-50000.0)
!     GO TO 530
!     ALASKA STATE TAX CALCULATED HERE
!500  IF(XNOI.LT.10000.0) STAX=0.01*XNOI
 & !     IF(XNOI.GE.10000.0 .AND. XNOI.LT.20000.0)
!        STAX = 0.02*XNOI
 & !     IF(XNOI.GE.20000.0 .AND. XNOI.LT.30000.0)
!        STAX=0.03*XNOI
 & !     IF(XNOI.GE.30000.0 .AND. XNOI.LT.40000.0)
!        STAX=0.04*XNOI
 & !     IF(XNOI.GE.40000.0 .AND. XNOI.LT.50000.0)
!        STAX = 0.05*XNOI
 & !     IF(XNOI.GE.50000.0 .AND. XNOI.LT.60000.0)
!        STAX=0.06*XNOI
 & !     IF(XNOI.GE.60000.0 .AND. XNOI.LT.70000.)
!        STAX=0.07*XNOI
 & !     IF(XNOI.GE.70000.0 .AND. XNOI.LT.80000.0)
!        STAX=0.08*XNOI
 & !     IF(XNOI.GE.80000.0 .AND. XNOI.LT.90000.0)
!        STAX=0.09*XNOI
!     IF(XNOI.GE.90000.0) STAX=0.094*XNOIC
!530   CONTINUE
      ENDIF
555     continue
!555   WRITE(*,*) istate,srate(istate),xnoi

      STAX = XNOI * SRATE(ISTATE )/100.0
      STTAX = STAX

      RETURN
      END

!      GRADTAX.F
!
! -- RJH -- 12/15/94
!  The function GRADTAX returns the corporate income tax for
!  any state that has different rates for individual tax
!  brackets.  The array R is the tax rate, and B is the upper
!  level of the tax bracket.  The last rate applies to all
!  income above the previous bracket.
!   To use:   STAX = GRADTAX(XNOI,R,B)
!
      FUNCTION GRADTAX(INCOME,R,B)
      REAL R(12),B(12),INCOME
!
      IF ( INCOME .LE. 0. ) THEN
          GRADTAX = 0.
          RETURN
      ENDIF
!
      BASE=0.0
       DO K=1,12
         RATE=R(K)/100
         IF(INCOME .GT. B(K) ) THEN
           IF(K .EQ. 1) THEN
               BASE=BASE+RATE*B(K)
           ELSE
             IF(B(K) .GT. 0) THEN
               BASE=BASE+RATE*(B(K)-B(K-1))
              ELSE
               GRADTAX=BASE+RATE*(INCOME-B(K-1))
               RETURN
              ENDIF
           ENDIF
         ELSE
        IF(K.EQ.1) THEN
           GRADTAX=BASE+RATE*INCOME
           RETURN
        ELSE
              GRADTAX=BASE+RATE*(INCOME-B(K-1))
            RETURN
        ENDIF
         ENDIF
       END DO
      RETURN
      END       

!***************************************************************

        subroutine write_dcfsum(ires,nyrsi,i0,effinv,NPVW,RORW,iyear)

        implicit none

        INTEGER ires
        INTEGER nyrsi
        integer iyear



!  This submodule writes the detailed economic proforma for a reservoir.
!  It is only run if the 'wrtpro', read by 'read_options' is true.

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
      include 'ogsmugr'
      include 'ogsml48'

! local variables
      integer iyr1,iyr2,i0,ipage,iyr
      integer igen(MAX_YR)              ! no of generators required for stea
      common/newcash/igen
      REAL effinv, NPVW, RORW
      REAL capex(max_yr), opex(max_yr), taxes(max_yr), tprod(max_yr)
      REAL c_capex, c_opex, c_taxes, c_prod, disctemp
      REAL EUR, TRR, c_gprod
      character*80 line80
      CHARACTER*51 PROCCODE(MAX_PROC+1)
      INTEGER bsn
      INTEGER dbon

      dbon = 1

      PROCCODE(1) ='0 : DECLINE CURVE                                  '
      PROCCODE(2) ='1 : PRIMARY                                        '
      PROCCODE(3) ='2 : WATER FLOODING                                 '
      PROCCODE(4) ='3 : CO2 FLOODING                                   '
      PROCCODE(5) ='4 : STEAM FLOODING                                 '
      PROCCODE(6) ='5 : POLYMER FLOODING                               '
      PROCCODE(7) ='6 : INFILL DRILLING                                '
      PROCCODE(8) ='7 : PROFILE MODIFICATION                           '
      PROCCODE(9) ='8 : HORIZONTAL CONTINUITY                          '
      PROCCODE(10)='9 : HORIZONTAL PROFILE                             '
      PROCCODE(11)='10: UNDISCOVERED CONVENTIONAL OIL                  '
      PROCCODE(12)='11: CONVENTIONAL RADIAL FLOW GAS                   '
      PROCCODE(13)='12: WATER DRIVE                                    '
      PROCCODE(14)='13: TIGHT SANDS                                    '
      PROCCODE(15)='14: WET COAL/SHALE GAS                             '
      PROCCODE(16)='15: DRY COAL/SHALE GAS                             '
      PROCCODE(17)='16: UNDISCOVERED CONVENTIONAL GAS                  '
      PROCCODE(18)='17: UNDISCOVERED UNCONVENTIONAL OIL                '
      PROCCODE(19)='18: UNDISCOVERED TIGHT GAS                         '
      PROCCODE(20)='19: UNDISCOVERED COAL BED METHANE                  '
      PROCCODE(21)='20: UNDISCOVERED SHALE GAS                         '
      PROCCODE(22)='21: DEVELOPING SHALE GAS                           '
      PROCCODE(23)='22: DEVELOPING TIGHT GAS                           '
      PROCCODE(24)='23: DEVELOPING COALBED METHANE                     '


       IYR1 = 1
       IYR2 = max_yr-1

       capex = 0.
       opex = 0.
       taxes = 0.
       tprod = 0.
       c_capex = 0.
       c_opex = 0.
       c_taxes = 0.
       c_prod = 0.
       c_gprod = 0.
       EUR= 0.

       do IYR =1,max_yr-1
          EUR = EUR + aprodgas(ires,iyr,1)
       ENDDO
       if (procde.ge.20) then
         TRR = EUR * ATOTPAT(ires,1)/1000000.
       else
         TRR = EUR/1000000.
       endif

       do iyr = iyr1,iyr2
         tprod(iyr) = oilprod(iyr)*5.8 + gasprod(iyr)
! not used         trate = (fedrate+state_tax(iyr)/nibta(iyr))
         if (tprod(iyr).gt.0.) then
           capex(iyr) = (ti(iyr)+icst(iyr))/tprod(iyr)
           opex(iyr) = (toc(iyr))/tprod(iyr)
           taxes(iyr) = (royalty(iyr)+sev_tax(iyr)+state_tax(iyr)+  &
                         fedtax(iyr)-fedtax_cr(iyr))/tprod(iyr)
         endif
         if (iyr.le.nyrsi) then
!          disctemp = (1+discount_rt(procde+1))**(iyr-0.5)
           disctemp = 1.
           c_capex = c_capex + (ti(iyr)+icst(iyr))/disctemp
           c_opex = c_opex + (toc(iyr))/disctemp
           c_taxes = c_taxes + (royalty(iyr)+sev_tax(iyr)+state_tax(iyr)+  &
                     fedtax(iyr)-fedtax_cr(iyr))/disctemp
           c_prod = c_prod + (tprod(iyr))/disctemp
           c_gprod = c_gprod + (gasprod(iyr))/1000000.
         endif

       enddo

      IF (dbon.eq.1.and.aresid(ires).eq.'COND3102117') THEN
       write (i0,*)
       write (i0,*)
       WRITE (i0,*)'+++++++++++++++++++++++++++++++++++++++++++++++++'
       write (i0,*)'New Reservoir '
       write (i0,1102) procde
       WRITE (I0,*) PROCCODE(PROCDE+1)

       write (i0,2102) play
       write (i0,1103) resid
       write (i0,1101) effinv
       write (i0,3101) RORW
       write (i0,3102) NPVW
       write (i0,1104) ilifex2
       write (i0,1105) ilife
       WRITE (I0,1106) NYRSI
      ENDIF

1102   FORMAT(1X,'PROCESS           :: ',I3)
2102   FORMAT(1X,'PLAY              :: ',I6)
1103   FORMAT(1X,'FOR RESERVOIR     :: ',A23)
1101   FORMAT(1x,'Investment Efficiency (DCFROR.FOR) ::' ,f12.3)
3101   FORMAT(1x,'RATE OF RETURN ROR    (DCFROR.FOR) ::' ,f12.3)
3102   FORMAT(1x,'NET PRESENT VALUE                  ::' ,f12.3)
1104   FORMAT(1X,'BASE PATTERN LIFE :: ',I2)
1105   FORMAT(1X,'ADV  PATTERN LIFE :: ',I2)
1106   FORMAT(1X,'PROJECT LIFE      :: ',I2)

!      IF (c_prod.gt.0.0005) then
!      write (I0,3103) curiyr+baseyr-1, procde, aresflag(ires),play,   &
!                      c_capex/c_prod,c_opex/c_prod,c_taxes/c_prod,  &
!                      (c_capex+c_opex+c_taxes)/c_prod, TRR, c_prod, totpat, atotpat(ires,1), npvw, rorw
!      endif
3103   FORMAT(1x,'AVERAGE COST PER MCF  ::' ,I6,2I4,I6,6f12.3,2f12.0,2f12.3)

      IF (dbon.eq.1.and.aresid(ires).eq.'EGLA4714521') THEN
       WRITE (I0,*)
       WRITE(I0,1011) 'YEAR',(IYR+(iyear-1)+(L48B4YR-1),IYR=IYR1,IYR2)
       WRITE(I0,2000) ('==========',IYR=IYR1,IYR2)

       WRITE(I0,1013) 'NO OF PATTERNS       ', (SUMP(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'NO OF ACTIVE PATTERNS  ', (XPATN(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'PATTERNS INITIATED     ', (PATN(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'OIL PRODUCTION (MBO)', (OILPROD(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'GAS PRODUCTION (MMCF)', (GASPROD(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'NGL PRODUCTION (MB)', (NGLPROD(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'OIL PRICE   ($/BBL)', (OILPRICEO(IYR),IYR=IYR1,IYR2)
       WRITE(I0,1013) 'GAS PRICE   ($/MCF)', (GASPRICEO(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'GROSS REVENUES (M$)', (GROSS_REV(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'ROYALTIES', (ROYALTY(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'NET SALES',(NET_REV(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'TOTAL OPERATING COST', (TOC(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'SEVERANCE TAX       ', (SEV_TAX(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'TOTAL INVESTMENTS', (TI(IYR)+ICST(IYR),IYR=IYR1,IYR2)


       WRITE(I0,101) 'INTANGIBLE INVESTMENT', (ICST(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'TANGIBLE INVESTMENTS', (TI(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'NET REVENUES', (NET_REV(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'TAXABLE INCOME', (NIBTA(IYR),IYR=IYR1,IYR2)

       WRITE(I0,102) 'TAX CREDIT ADDBACK', (EORTCA(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'INTANGIBLE ADDBACK', (INTADD(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'G&G/LEASE ADDBACK', (GGLA(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'NET INCOME BEFORE TAXES', (NIBT(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'STATE INCOME TAXES', (STATE_TAX(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'AMT                ', (AMINT(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'FEDERAL INCOME TAX', (FEDTAX(IYR),IYR=IYR1,IYR2)
       WRITE(I0,102) 'FEDERAL TAX CREDITS', (FEDTAX_CR(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'NET INCOME AFTER TAXES', (NIAT(IYR),IYR=IYR1,IYR2)

       WRITE(I0,101) 'ANNUAL AFTER TAX CASH FLOW', (ATCF(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'DISCOUNTED AFTER TAX CASH FLOW', (DATCF(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'CUMULATIVE DISCOUNTED AFTER TAX CASH FLOW', (CATCF(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'capital expenditures per mcf', (capex(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'operating expenditures per mcf', (opex(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'taxes & royalties per mcf', (taxes(IYR),IYR=IYR1,IYR2)
       WRITE(I0,101) 'total costs per mcf', ((capex(iyr)+opex(iyr)+taxes(IYR)),IYR=IYR1,IYR2)
       WRITE(I0,101) 'capital expenditures', ((capex(IYR)*tprod(iyr)),IYR=IYR1,IYR2)
       WRITE(I0,101) 'operating expenditures', ((opex(IYR)*tprod(iyr)),IYR=IYR1,IYR2)
       WRITE(I0,101) 'taxes & royalties', ((taxes(IYR)*tprod(iyr)),IYR=IYR1,IYR2)
       WRITE(I0,101) 'total costs', (((capex(iyr)+opex(iyr)+taxes(IYR))*tprod(iyr)),IYR=IYR1,IYR2)

       WRITE (I0,994) 'NEW INJECTION WELLS', (EINJDR(IRES,IYR)   ,IYR=IYR1,IYR2)
       WRITE (I0,994) 'NEW PRODUCTION WELLS', (EPRODDR(IRES,IYR)  ,IYR=IYR1,IYR2)
       WRITE (I0,994) 'ACTIVE PRODUCING WELLS', (EPRODWELL(IRES,IYR),IYR=IYR1,IYR2)
       WRITE (I0,994) 'ACTIVE INJECTION WELLS', (EINJWELL(IRES,IYR) ,IYR=IYR1,IYR2)
       WRITE (I0,994) 'SHUT IN WELLS', (ESHUTIN(IRES,IYR)  ,IYR=IYR1,IYR2)
       WRITE (I0,994) 'PATTERNS DEVELOPED / BAS', (PATDEV(IRES,IYR,1) ,IYR=IYR1,IYR2)
       WRITE (I0,994) 'PATTERNS DEVELOPED / ADV', (PATDEV(IRES,IYR,2) ,IYR=IYR1,IYR2)
      ENDIF


 994  FORMAT(T1,A,T50,<max_yr-1>(1X,F12.2,1X))
 1011 format(t40,a,t49,<max_yr-1>(10x,i4))
 1013 format(t1,a,t50,<max_yr-1>(1x,f12.3,1x))
 101  format(t1,a,t50,<max_yr-1>(1x,f12.2,1x))
 102  format(t2,a,t50,<max_yr-1>(1x,f12.2,1x))
 103  format(t3,a,t50,<max_yr-1>(1x,f12.0,1x))
 104  format(t4,a,t50,<max_yr-1>(1x,f12.0,1x))
 105  format(t5,a,t50,<max_yr-1>(1x,f12.0,1x))
 2000 format(t50,<max_yr-1>(3x,a10,1x))
 1022 format(t2,a,t50,<max_yr-1>(1x,f12.3,1x))

      return

      end subroutine!***************************************************************
