!==============================================================================
! NEMS Residential Demand Module (RDM)
!
! A component of the U.S. Energy Information Administration's
!  National Energy Modeling System (NEMS)
!
! LANGUAGE:      FORTRAN
! CALLED BY:     PROGRAM NEMS (Integrating Module)
! ANALYSIS:      AEO2026
! CASE:          Reference
! DATE:          January 28, 2025
!
!==============================================================================
! AEO2026 CHANGES 
! -Move RSSWITCH fuel/technology switching costs (RPINSCOST) to new tab in
!   RSMESS.xlsx input file; RTFCBIAS moved to RSHTSHR.txt
! -Move read-in of RTFCBIAS values to RSHTSHR.txt
! -Add output of TotKW to RDM_DGENOUT.txt (!CapacityOutput)
! -Remove unused kerosene placeholders from input files since combining
!   kerosene with distillate fuel oil (!NoKero)
! -Remove unused NHtoUse variable for 2012 secondary heating/ direct heating
!   equipment standard; predates 2020 RECS base year
! -Add missing declarations for LTlCapInvest and EAEQCN
! -Add read-in of battery energy storage system technology inputs from
!   RSGENTK.txt for future modeling purposes (!BESSmodel)
! -Cleaned up various code comments and formatting
!******************************************************************************
! PAST CHANGES THAT REQUIRE OCCASIONAL UPDATES, REVIEW, OR VERIFICATION (ESPECIALLY DURING RECS UPDATE)
! -Revise hard-coded EQT values that correspond to xlRTEQTYPE values from RSMEQP
!   tab of RSMESS.xlsx; specifically affects clothes washer, water heater, refrigerator,
!   and freezer equipment (!techupdate)
! -Update BASEMEF using top-load clothes washer base-year efficiency from latest major end-use technology report
! -Update space cooling equipment types (xlRTEQTYPE in RSMEQP) used to differentiate between cooling classes (!CoolTypes)
! -Update water heater equipment types (xlRTEQTYPE in RSMEQP) referenced for 2015 water heater standard (!WHStandard)
! -Add parameters for hard-coded end-use class and technology count numbers, with values calculated in the different tabs
!   of RSMESS.xlsx (!EqpParam)
! -Adjust cost for ASHP compared with central AC (!ACcost)
! -Update average natural gas water heating UEC (XWATERHTGMMBTU)
! -Update share of homes with propane grills (LPGGRILL)
! -Update shares of natural gas water-heated homes that also have natural gas cooking ranges by housing type (NGNGFACT)
! -Update penetration rate of diswashers into new construction (!DISHNEWpen)
! -Calibrate ZIP-code PV penetration model to historical exogenous PV capacity from RSGENTK (ExogPVMistie; !PVzipcalib);
!   can be turned off in RGENTK.txt
! -Update calculation of solar PV generation based on PVWatts 5 (!PVgen)
! -NOTE: there are many instances where mNumCR 10 is used to denote national total;
!   per PARAMETR includes file, 10 is California and 11 should be national; no fix yet implemented
! -TODO - review uses of T=CurCalYr versus Y=CurCalYr across subroutines

MODULE R_

INCLUDE 'parametr'
INCLUDE 'ncntrl'
INCLUDE 'apq'
INCLUDE 'resdrep'
INCLUDE 'rtek'
INCLUDE 'bldglrn'
INCLUDE 'emmparm'
INCLUDE 'emission'
INCLUDE 'eusprc'
INCLUDE 'emablk'
INCLUDE 'macout'
INCLUDE 'rscon'
INCLUDE 'rseff'
INCLUDE 'qsblk'
INCLUDE 'cogen'
INCLUDE 'uefpout' !electricity price for grid sales
INCLUDE 'uecpout' !contains RPS credit price in 1987 mills/kWh - EPRPSPR(CurIYr)
INCLUDE 'e111d'
INCLUDE 'steoblock' !common STEO inputs

!Array location for aggregate of census division data
INTEGER, PARAMETER :: NationalPtr = 11

!Parameters for RSCLASS, RSMEQP, and RSMSHL input file RSMESS.xlsx; calculated at the bottom of their respective tabs and manually input here	!TODO - consider reading parameter values directly from RSMESS.xlsx
!Max number of end-use classes in RSCLASS  !EqpParam
INTEGER, PARAMETER :: nHeatClasses = 10  !NoKero
INTEGER, PARAMETER :: nCoolClasses = 5
INTEGER, PARAMETER :: nClWashClasses = 1
INTEGER, PARAMETER :: nDishClasses = 1
INTEGER, PARAMETER :: nWatHtClasses = 5
INTEGER, PARAMETER :: nCookClasses = 3
INTEGER, PARAMETER :: nClDryClasses = 2
INTEGER, PARAMETER :: nRefrClasses = 1
INTEGER, PARAMETER :: nFrezClasses = 1

!Max number of end-use equipment types in RSMEQP  !EqpParam
INTEGER, PARAMETER :: nHeatTypes = 30
INTEGER, PARAMETER :: nCoolTypes = 16
INTEGER, PARAMETER :: nClWashTypes = 6
INTEGER, PARAMETER :: nDishTypes = 3
INTEGER, PARAMETER :: nWatHtTypes = 15
INTEGER, PARAMETER :: nCookTypes = 7
INTEGER, PARAMETER :: nClDryTypes = 6
INTEGER, PARAMETER :: nRefrTypes = 12
INTEGER, PARAMETER :: nFrezTypes = 7

!Max number of shell types in RSMSHL
INTEGER, PARAMETER :: nShellTypes = 5

!Parameters for RSMLGT lighting menu and arrays
INTEGER, PARAMETER :: NLRec = 100   !Number of lighting records in the technology database
INTEGER, PARAMETER :: MaxApps = 4   !Maximum number of applications
INTEGER, PARAMETER :: MaxTypes = 4  !Maximum number of bulb types within an application
INTEGER, PARAMETER :: MaxBins = 6   !Maximum number of hours-per-day usage bins per applications

!Parameters for Residential Price-Induced Technical Change (RSPITC)
! These parameters allow first years of availability to be advanced when energy price increases are large.
! The idea is that the menu years are based on business-as-usual and would not account for research and development in the event of large energy price increases.
! Setting IFMAX = 0 turns this feature off.
INTEGER, PARAMETER :: IFMAX = 0 !Maximum forward effect

COMMON/BASE111D/BASELINEBKWH(mNumCR,mNumYr) !111(d)
COMMON/EFFDriver/Driver(mNumYr,mNumFuel,mNumCR-2,mNumBldg),Driver2(mNumYr,mNumCR-2,mNumBldg)  !NoKero
COMMON/EQCES/EQCESE(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR)
COMMON/EQCRP/EQCRP90(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
COMMON/EQCSR/EQCSR90(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
COMMON/EQADD/EQCADD(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
COMMON/EQREP/EQCREP(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
COMMON/EQSUR/EQCSUR(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
COMMON/RFCON/NHTRFL,NCLFL,NWHFL,NSTVFL,NDRYFL,NREFFL,NFRZFL, &
             FHTRCON(mNumFuel),FCLCON(mNumFuel),FWHCON(mNumFuel),FSTVCON(mNumFuel), &
             FDRYCON(mNumFuel),FREFCON(mNumFuel),FFRZCON(mNumFuel), &
             FCSWCON(mNumFuel),FDSWCON(mNumFuel),NCSWFL,NDSWFL,NSHTRFL
COMMON/LFE/HDRfy,HDRly,HDR(mNumBldg),HDQ((BaseYr-BaseYr+2):(EndYr-BaseYr+1),mNumCR,mNumBldg),HDi((BaseYr-BaseYr+2):(EndYr-BaseYr+1),mNumCR,mNumBldg), &	!TODO - Verify use of "BaseYr-BaseYr" (should refer to EndYr, RECSyear, HDRfy, or HDRly?)
           HDiAve(mNumCR,mNumBldg),HCDshr((BaseYr-BaseYr+2):(EndYr-BaseYr+1),mNumCR,mNumBldg),HCDshrAve(mNumCR,mNumBldg),ResDiscountRate,Tenure	!TODO - Verify use of "BaseYr-BaseYr" (should refer to EndYr, RECSyear, HDRfy, or HDRly?)
COMMON/EXHS/EH(RECSYear:EndYr,mNumBldg,mNumCR-2)
COMMON/CWSHR/TCW_SHR,FCW_SHR
COMMON/FRZSHR/TMF_SHR,SMF_SHR,BMF_SHR,CH_SHR,UP_SHR
COMMON/SAT/RACSAT(mNumBldg,mNumCR),RACUnits(mNumBldg,mNumCR),CACSAT(mNumBldg,mNumCR),CACPR(mNumCR), &
           FRZSAT(mNumBldg,mNumCR),ELDRYPR(mNumBldg,mNumCR),REFSAT(mNumBldg,mNumCR)
COMMON/SHTR/SHTSHR(mNumBldg,mNumCR,mNumFuel),NSHTSHR(mNumBldg,mNumCR,mNumFuel) !NG, ELEC, DFO/KER, PROPANE, WOOD  !NoKero
COMMON/SHELL/EHSHELL(RECSYear:EndYr+1,mNumFuel,mNumCR,mNumBldg),ECSHELL(RECSYear:EndYr+1,mNumCR,mNumBldg), &
             NHSHELL(RECSYear:EndYr+1,mNumFuel,mNumCR,mNumBldg),NCSHELL(RECSYear:EndYr+1,mNumCR,mNumBldg), &
             AHSHELL(RECSYear:EndYr+1,mNumFuel,mNumCR,mNumBldg),ACSHELL(RECSYear:EndYr+1,mNumCR,mNumBldg), &
             TECHG(RECSYear:EndYr+1,mNumCR-2,mNumBldg),LIMIT, &
             WTHRZTN(RECSYear:EndYr,2,mNumCR-2,mNumBldg)  !2 = space heating and space cooling end uses
COMMON/WTHRADJ/HDDYEAR(BaseYr:NUMHDDYR),CDDYEAR(BaseYr:NUMCDDYR)
COMMON/APPLSAT/NEWDRYSAT(RECSYear+1:EndYr,2,mNumBldg,mNumCR-2), &
               DISHNEW(RECSYear+1:EndYr,mNumBldg,mNumCR-2),WASHNEW(RECSYear+1:EndYr,mNumBldg,mNumCR-2)
COMMON/ALLHOUSE/OLDHSES(RECSYear:EndYr),NEWHSES(RECSYear:EndYr)
COMMON/INSCOST/RPINSCOST(mNumRTCl,mNumRTCl)
COMMON/NEWMISC/MELsIncomeEffect(30),&	!TODO - 30 is an arbitrary value meant to exceed the current number of end uses; replace with parameter
       TVSPEN(RECSYear:EndYr),TVSEFF(RECSYear:EndYr),STBPEN(RECSYear:EndYr),STBEFF(RECSYear:EndYr),&
       HTSPEN(RECSYear:EndYr),HTSEFF(RECSYear:EndYr),OTTPEN(RECSYear:EndYr),OTTEFF(RECSYear:EndYr),&
       VGCPEN(RECSYear:EndYr),VGCEFF(RECSYear:EndYr),&
       DPCPEN(RECSYear:EndYr),DPCEFF(RECSYear:EndYr),LPCPEN(RECSYear:EndYr),LPCEFF(RECSYear:EndYr),&
       MONPEN(RECSYear:EndYr),MONEFF(RECSYear:EndYr),NETPEN(RECSYear:EndYr),NETEFF(RECSYear:EndYr),&
       BATPEN(RECSYear:EndYr),BATEFF(RECSYear:EndYr),CFNPEN(RECSYear:EndYr),CFNEFF(RECSYear:EndYr),&
       COFPEN(RECSYear:EndYr),COFEFF(RECSYear:EndYr),DEHPEN(RECSYear:EndYr),DEHEFF(RECSYear:EndYr),&
       MCOPEN(RECSYear:EndYr),MCOEFF(RECSYear:EndYr),PLPPEN(RECSYear:EndYr),PLPEFF(RECSYear:EndYr),&
       PLHPEN(RECSYear:EndYr),PLHEFF(RECSYear:EndYr),&
       SECPEN(RECSYear:EndYr),SECEFF(RECSYear:EndYr),SPAPEN(RECSYear:EndYr),SPAEFF(RECSYear:EndYr),&
       WCLPEN(RECSYear:EndYr),WCLEFF(RECSYear:EndYr),&
       SPKPEN(RECSYear:EndYr),SPKEFF(RECSYear:EndYr),PHNPEN(RECSYear:EndYr),PHNEFF(RECSYear:EndYr),&
       TABPEN(RECSYear:EndYr),TABEFF(RECSYear:EndYr),KITPEN(RECSYear:EndYr),KITEFF(RECSYear:EndYr)
COMMON/APLSHARES/NEWHEATUEC(nHeatClasses,mNumBldg,mNumCR-2),NEWCOOLUEC(mNumBldg,mNumCR-2),BASELOAD(nHeatClasses+nCoolClasses)
COMMON/RETIRE/EQCRET(RECSYear:EndYr,mNumRTCl)
COMMON/SQRFOOT/SQNEW(RECSYear:EndYr,mNumBldg,mNumCR-2),EXSQFTADJ(RECSYear:EndYr,mNumBldg,mNumCR-2,5)	!TODO - replace 5 with parameter?
COMMON/SQRFLTS/ELASTIC(5,mNumCR-2)	!TODO - replace 5 with parameter?
COMMON/SQFTDATA/SQRFOOT(RECSYear:EndYr,mNumBldg,mNumCR-2),EXSQRFOOT(RECSYear:EndYr,mNumBldg,mNumCR-2),STOCKSQRFOOT(RECSYear:EndYr,mNumBldg,mNumCR-2)
COMMON/PRI/PRICES(mNumFuel,mNumCR,BaseYr:EndYr)
COMMON/DRYSA/DRYSAT(mNumBldg,mNumCR)
COMMON/HOTWATER/HOTWATQ(RECSYear:EndYr,mNumBldg,mNumCR-2),CWLOAD(RECSYear),NCWLOAD(RECSYear:EndYr,mNumCR-2,mNumBldg),ECWLOAD(RECSYear:EndYr,mNumCR-2,mNumBldg), &
                DWPR(mNumBldg,mNumCR)
COMMON/EFFIC/EQCEFF(RECSYear:EndYr,mNumRTCl)
COMMON/STEFFIC/STKEFF(RECSYear:EndYr,mNumRTCl)
COMMON/EUECS/EQCUEC(mNumCR,mNumRTCl,mNumBldg),FANUEC(mNumCR,mNumBldg) &
        ,TVSUEC(mNumCR,mNumBldg),STBUEC(mNumCR,mNumBldg),HTSUEC(mNumCR,mNumBldg) &
        ,OTTUEC(mNumCR,mNumBldg),VGCUEC(mNumCR,mNumBldg),DPCUEC(mNumCR,mNumBldg) &
        ,LPCUEC(mNumCR,mNumBldg),MONUEC(mNumCR,mNumBldg),NETUEC(mNumCR,mNumBldg) &
        ,BATUEC(mNumCR,mNumBldg),CFNUEC(mNumCR,mNumBldg),COFUEC(mNumCR,mNumBldg) &
        ,DEHUEC(mNumCR,mNumBldg),MCOUEC(mNumCR,mNumBldg),PLPUEC(mNumCR,mNumBldg),PLHUEC(mNumCR,mNumBldg) &
        ,SECUEC(mNumCR,mNumBldg),SPAUEC(mNumCR,mNumBldg),WCLUEC(mNumCR,mNumBldg) &
        ,SPKUEC(mNumCR,mNumBldg),PHNUEC(mNumCR,mNumBldg),TABUEC(mNumCR,mNumBldg),KITUEC(mNumCR,mNumBldg) &
        ,EAUEC(mNumCR,mNumBldg),FANIUEC(mNumCR,mNumBldg),SHTUEC(mNumCR,mNumFuel,mNumBldg),APPUEC(mNumCR,3,mNumBldg)  !NoKero 	!TODO - replace 3 with parameter?
COMMON/EQCUEC/EQCNUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCAUEC(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCAHVUEC(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCRUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCSUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCHVUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCNIUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCRIUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCSIUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCHVIUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
COMMON/SLC/SLCON(RECSYear-BaseYr+1:mNumYr+1,mNumCR)
COMMON/APLC/APLCON(RECSYear-BaseYr+1:mNumYr,3,mNumCR-2) !3 Fuels	!TODO - replace 3 with parameter?
COMMON/RSFC/RSFLCN(RECSYear-BaseYr+1:mNumYr,5,mNumCR-1)  !NoKero	!TODO - replace 5 with parameter? represents aggregated consumption across end uses
COMMON/NHOUSES/NH(RECSYear:EndYr,mNumBldg,mNumCR-2),HSEADD(RECSYear:EndYr,mNumBldg,mNumCR-2), &
               HHSTOCKBYDIV(RECSYear:EndYr,mNumCR-2)	!TODO - remove?
COMMON/ALLNEW/ALLNEW(RECSYear:EndYr,mNumCR-2)		!TODO - remove?
COMMON/LIFE/LFCY(MNUMRTTY,mNumBldg,mNumCR,3)
COMMON/EQCRP90/EQCRP90RP(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
COMMON/OEQCRP/OEQCRP90(RECSYear:EndYr,mNumRTCl,1,mNumCR)
COMMON/OEQCRPR/OEQCRP90R(RECSYear:EndYr,mNumRTCl,1,mNumCR)
COMMON/OEQREP/OEQCREP(RECSYear:EndYr,mNumRTCl,1,mNumCR)
COMMON/SWITCH/EQCSW90(RECSYear:EndYr,mNumRTCl,mNumRTCl,1,mNumCR) &
        ,EQCSW90R(RECSYear:EndYr,mNumRTCl,mNumRTCl,1,mNumCR)
COMMON/SW/SWITCHES(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,SWITCHESR(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,SWITCHTO(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,SWITCHTOR(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)&
        ,SWTOTAL(RECSYear:EndYr,mNumRTCl,mNumCR-2) &
        ,SWFTOTAL(RECSYear:EndYr,mNumRTCl,mNumCR-2)
COMMON/NWHTR/HSYSSHR(RECSYear:EndYr+1,nHeatClasses,mNumBldg,mNumCR)
COMMON/HTSHRYR/HTSHRYR
COMMON/ESTARHISTYR/ESTARHISTYR
COMMON/GASCUST/WATERTOT,COOKTOT,DRYERTOT

REAL*4,ALLOCATABLE::NEQTSHR(:,:,:,:)
REAL*4,ALLOCATABLE::REQTSHR(:,:,:,:)

COMMON/WEQCEF/ WTEQCEFFN(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,WTEQCEFFR(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,WTEQCEFFA(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,WTEQCEFFHV(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,WTEQCSQFHV(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR)
COMMON/HURDLE/HRDRATE,ELIGBLE,ALPHA1,HRDADJ
COMMON/DISCRATE/BETA1DR(MNUMRTTY)
COMMON/EQCND/EQCND90(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)       !LOCAL
COMMON/HEATOT/HEATOT(RECSYear:EndYr+1,nHeatClasses,mNumBldg,mNumCR)

REAL*4,ALLOCATABLE::EQR90FUT  (:,:,:,:,:)
REAL*4,ALLOCATABLE::EQREPFUT  (:,:,:,:,:)
REAL*4,ALLOCATABLE::EQADDFUT  (:,:,:,:,:)
REAL*4,ALLOCATABLE::EQR90RPFUT(:,:,:,:,:)
REAL*4,ALLOCATABLE::EQCESEFUT (:,:,:,:,:)

COMMON/EQCEQ/EQCEQCN(RECSYear-BaseYr:mNumYr,mNumRTCl,mNumBldg,mNumCR)
COMMON/SLEQ/SLEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR) !"SLEQ" was "GOEQ"
COMMON/FANEQ/FANEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2)
COMMON/NH2O/NH2OSH(RECSYear:EndYr+1,mNumFuel,mNumBldg,mNumCR)
COMMON/NWCK/NCKSH(RECSYear:EndYr+1,mNumFuel,mNumBldg,mNumCR)
COMMON/LTEQ/LTEQCN(RECSYear-BaseYr:mNumYr,4,mNumBldg,mNumCR-2)	!TODO - replace 4 with parameter?
COMMON/EAEQ/EAEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2)
COMMON/OTUSES/TVSEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2)&
        ,STBEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),HTSEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,OTTEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),VGCEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,DPCEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),LPCEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,MONEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),NETEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,BATEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),CFNEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,COFEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),DEHEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,MCOEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),PLPEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,SECEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),SPAEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,WCLEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),PLHEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,SPKEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),PHNEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,TABEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),KITEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2)
COMMON/SHEQ/SHEQCN(RECSYear-BaseYr:mNumYr,mNumFuel,mNumBldg,mNumCR-2)  !NoKero
COMMON/APEQ/APEQCN(RECSYear-BaseYr:mNumYr,3,mNumBldg,mNumCR-2),APLEQP(RECSYear:EndYr,mNumBldg,mNumCR-2,3)
COMMON/RBENCH/BNCHFCT(RECSYear-BaseYr+1:mNumYr,mNumFuel-1,mNumCR-2), BNCHFCTAVG(mNumFuel-1,mNumCR-2)  !mNumFuel-1 = major fuels (natural gas, electricity, distillate fuel oil/kerosene, propane)  !NoKero
COMMON/RSDgOut/Units(mNumYr,mNumCR,nTek), Cap(mNumYr,mNumCR,nTek), Trills(mNumYr,mNumCR,nTek) &
        ,TrillsOwnUse(mNumYr,mNumCR,nTek), GasUsage(mNumYr,mNumCR,nTek) &
        ,HwBtu(mNumYr,mNumCR,nTek), Invest(mNumYr,mNumCR,nTek)   &
        ,x111dRenSub(mNumYr,mNumCR,nTek),iGenCapCostYr
COMMON/COOLVAC/ACICOST(MNUMRTTY,RECSYear:EndYr,mNumCR-2),ACEFF(MNUMRTTY,RECSYear:EndYr,mNumCR-2)

REAL*4,ALLOCATABLE::RSNEFDB1(:,:,:,:)
REAL*4,ALLOCATABLE::RSEEFDB1(:,:,:,:)
REAL*4,ALLOCATABLE::HTSHELLEFFWT(:,:,:,:,:)
REAL*4,ALLOCATABLE::HTSHELLWT(:,:,:,:,:)
REAL*4,ALLOCATABLE::HSHELL(:,:,:,:)
REAL*4,ALLOCATABLE::CSHELL(:,:,:,:)
REAL*4,ALLOCATABLE::SHELLBUILDS(:,:,:,:,:)
REAL*4,ALLOCATABLE::SHELLINVEST (:,:,:,:,:)
REAL*4,ALLOCATABLE::SHELLSUBSIDY(:,:,:,:,:)
REAL*4,ALLOCATABLE::SHELLSUBSIDY111D(:,:,:,:,:)
REAL*4,ALLOCATABLE::CLSHELLWT(:,:,:,:)
REAL*4,ALLOCATABLE::SHLEVELH(:,:,:,:,:)

COMMON/OTHEREQP/FANEQP(RECSYear:EndYr,mNumBldg,mNumCR-2), &
        EAEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),APPEQP(RECSYear:EndYr,mNumBldg,mNumCR-2,3),&	!TODO - replace 3 with parameter?
        SHTEQP(RECSYear:EndYr,mNumBldg,mNumCR-2,mNumFuel),&  !NoKero
        TVSEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),STBEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        HTSEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),OTTEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        VGCEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        DPCEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),LPCEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        MONEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),NETEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        BATEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),CFNEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        COFEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),DEHEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        MCOEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),PLPEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        SECEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),SPAEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        WCLEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),PLHEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        SPKEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),PHNEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        TABEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),KITEQP(RECSYear:EndYr,mNumBldg,mNumCR-2)
COMMON/DISPINC/INCOME(mNumCR-2,RECSYear:EndYr,30) !DISPOSABLE INCOME VARIABLE - 30 is an arbitrary value meant to exceed the current number of MELs end uses (aligns with MELsIncomeEffect)	!TODO - replace 30 with parameter?

REAL*4,ALLOCATABLE::HVEQSHR(:,:,:,:)
REAL*4,ALLOCATABLE::HEATINGTYPEPURCH(:,:,:,:,:)
REAL*4,ALLOCATABLE::NEQTSHRC(:,:,:,:)
REAL*4,ALLOCATABLE::LEARNFACT(:,:)

COMMON/LTUEC/LTUEC(MaxApps,mNumCR-2,mNumBldg),LTEQP(MaxApps,RECSYear:EndYr,mNumBldg,mNumCR-2), &
        LTNUEC(MaxApps,RECSYear:EndYr,mNumCR-2,mNumBldg), LTNUECly(MaxApps,RECSYear:EndYr,mNumCR-2,mNumBldg)

!Common block for lighting variables !NLRec is the maximum number of lighting records in the rsmlgt.txt cost
! and performance section (see above for parameter setting)     !111(D) adding subsidy with division dimension and division to capital cost
COMMON/NewLightingVars/BulbCost(NLRec),BulbEESub(NLRec,mNumCR-2),BulbSub(NLRec,mNumCR-2),LPW(NLRec),BulbWatts(NLRec),LifeHours(NLRec),BulbCRI(NLRec), &
         BaseWattsBulbs(MaxApps,MaxTypes),BaseWattBins(MaxApps,MaxBins),AnnualBulbCost(MaxApps,MaxTypes,MaxBins), &
         WattsCY(MaxTypes),Beta1,Beta2,AppBinHours(MaxApps,MaxBins), BulbBinLife(MaxApps,MaxBins), &
         BulbsPerHH(MaxApps,mNumBldg),BulbBinShares(MaxApps,MaxTypes,MaxBins),BinShares(MaxApps,MaxBins), &
         BulbBinEnergy(MaxApps,MaxTypes,MaxBins),CRIBulb(MaxApps),LTlbeta1(NLRec), LTlbeta2(NLRec), watts(MaxTypes), &
         LTlCap(MaxTypes,mNumCR-2,MaxBins),LTLsub(MaxTypes,mNumCR-2),LTlCapInvest(MaxTypes),LTLIFE(MaxTypes,MaxBins),LTBinShare(MaxApps,MaxBins), &
         NumApps,NumTypes(MaxApps),AppIndex(MaxApps),NumAppBins(MaxApps),FirstYear(NLRec),LastYear(NLRec),BulbDiv(nlrec),&
         AppID(MaxApps),LightingApp(NLRec),BulbType(NLRec),LightDiag,RLGTDOLLARYR

COMMON/LTDATABASE/LTInvest(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,2),  &
                  LTsubsidy(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,2), &
                  LTREPbyAPP(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2),&
                  LTNEEDEDbyAPP(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2), &
                  WTLEFFbyAPP(MaxApps,RECSYear:EndYr,mNumBldg,mNumCR-2), &
                  appbulbname(MaxApps,MaxTypes),LTSTOCK(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins)
REAL*4 LTinvest,LTsubsidy,LTREPbyAPP,LTNEEDEDbyAPP,WTLEFFbyAPP,LTSTOCK
CHARACTER*3 appbulbname
REAL*4 BulbCost, BulbEESub, BulbSub, LPW, BulbWatts, LifeHours, BulbCRI,BaseWattsBulbs,BaseWattBins,AnnualBulbCost, &
        WattsCY,Beta1,Beta2,AppBinHours,BulbBinLife,BulbsPerHH,BulbBinShares,BinShares,BulbBinEnergy,CRIBulb, &
        LTlbeta1, LTlbeta2, watts, LTlcap, LTlCapInvest, LTLsub, LTlife, LTBinShare
INTEGER NumApps, NumTypes, appindex, NumAppBins, FirstYear, LastYear, BulbDiv
INTEGER LightDiag, RLGTDOLLARYR
CHARACTER*3 LightingApp, BulbType, AppID
REAL*4 LTUEC, LTNUEC, LTEQP, LTNUECly
REAL*4 BASELINEBKWH
REAL*4 Driver,Driver2
REAL*4 WTHRZTN
REAL*4 INCOME
REAL*4 FANEQP,EAEQP,APPEQP,SHTEQP
REAL*4 TVSEQP,STBEQP,HTSEQP,OTTEQP,VGCEQP
REAL*4 DPCEQP,LPCEQP,MONEQP,NETEQP
REAL*4 BATEQP,CFNEQP,COFEQP,DEHEQP,MCOEQP,PLPEQP,PLHEQP,SECEQP,SPAEQP,WCLEQP
REAL*4 SPKEQP,PHNEQP,TABEQP,KITEQP
REAL*4 FANPEN,EAPEN,APPPEN,SHTPEN
INTEGER NumMELs,MELsIncomeEffect
REAL*4 TVSPEN,STBPEN,HTSPEN,OTTPEN,VGCPEN
REAL*4 DPCPEN,LPCPEN,MONPEN,NETPEN
REAL*4 BATPEN,CFNPEN,COFPEN,DEHPEN,MCOPEN,PLPPEN,PLHPEN,SECPEN,SPAPEN,WCLPEN
REAL*4 SPKPEN,PHNPEN,TABPEN,KITPEN
REAL*4 ACICOST,ACEFF
REAL*8 Units, Cap, Trills, TrillsOwnUse, GasUsage, HwBtu, Invest
REAL*8 x111dRenSub
INTEGER iGenCapCostYr
REAL*4 BNCHFCT, BNCHFCTAVG
REAL*4 APEQCN,APLEQP
REAL*4 SHEQCN
REAL*4 TVSEQCN,STBEQCN,HTSEQCN,OTTEQCN,VGCEQCN
REAL*4 DPCEQCN,LPCEQCN,MONEQCN,NETEQCN
REAL*4 BATEQCN,CFNEQCN,COFEQCN,DEHEQCN,MCOEQCN,PLPEQCN,PLHEQCN,SECEQCN,SPAEQCN,WCLEQCN
REAL*4 SPKEQCN,PHNEQCN,TABEQCN,KITEQCN,EAEQCN
REAL*4 LTEQCN
REAL*4 NCKSH
REAL*4 NH2OSH
REAL*4 EQCEQCN
REAL*4 SLEQCN
REAL*4 FANEQCN
REAL*4 EQCRP90RP,EQCRET
REAL*4 BETA1DR
REAL*4 EQCND90
REAL*4 HEATOT
REAL*4 HRDRATE,ELIGBLE,ALPHA1,HRDADJ
INTEGER HTSHRYR
INTEGER ESTARHISTYR
REAL*4,ALLOCATABLE::HVBETA1(:,:,:,:),HVBETA2(:,:,:,:) ! LOGIT PARAMETER 1 (INSTALLED COST) and 2 (OPERATING COST)
REAL*4 HSYSSHR
REAL*4 WTEQCEFFN,WTEQCEFFR,WTEQCEFFA,WTEQCEFFHV,WTEQCSQFHV
REAL*4 SWITCHES,SWITCHESR,SWITCHTO,SWITCHTOR,SWTOTAL,SWFTOTAL
REAL*4 EQCSW90,EQCSW90R
REAL*4 LFCY,OEQCRP90,OEQCREP,OEQCRP90R
REAL*4 ALLNEW
REAL*4 NH,HSEADD,HHSTOCKBYDIV	!TODO - remove HHSTOCKBYDIV?
REAL*4 SLCON,SHTSHR,NSHTSHR,APLCON,RSFLCN	!TODO - Why isn't APLCON declared in RSCON include like other consumption variables?
REAL*4 EQCUEC,EAUEC,FANUEC,FANIUEC,SHTUEC,APPUEC
REAL*4 TVSUEC,STBUEC,HTSUEC,OTTUEC,VGCUEC
REAL*4 DPCUEC,LPCUEC,MONUEC,NETUEC
REAL*4 BATUEC,CFNUEC,COFUEC,DEHUEC,MCOUEC,PLPUEC,PLHUEC,SECUEC,SPAUEC,WCLUEC
REAL*4 SPKUEC,PHNUEC,TABUEC,KITUEC
REAL*4 EQCNUEC,EQCAUEC,EQCRUEC,EQCSUEC,EQCHVUEC,EQCAHVUEC
REAL*4 EQCNIUEC,EQCRIUEC,EQCSIUEC,EQCHVIUEC
REAL*4 STKEFF
REAL*4 EQCEFF
REAL*4 HOTWATQ,CWLOAD,NCWLOAD,ECWLOAD,DWPR
REAL*4 DRYSAT
REAL*4 PRICES ! 1=Distillate Fuel Oil 2=Propane/LPG 3=Natural Gas 4=Electricity 5=Wood
REAL*4 SQFTADJ,SQRFOOT,SQNEW,STOCKSQRFOOT,EXSQRFOOT,EXSQFTADJ,ELASTIC
REAL*4 NEWHEATUEC,NEWCOOLUEC,BASELOAD
REAL*4 TVSEFF,STBEFF,HTSEFF,OTTEFF,VGCEFF
REAL*4 DPCEFF,LPCEFF,MONEFF,NETEFF
REAL*4 BATEFF,CFNEFF,COFEFF,DEHEFF,MCOEFF,PLPEFF,PLHEFF,SECEFF,SPAEFF,WCLEFF
REAL*4 SPKEFF,PHNEFF,TABEFF,KITEFF
REAL*4 DISHNEW,WASHNEW
REAL*4 OLDHSES,NEWHSES
INTEGER HDDYEAR,CDDYEAR,RPINSCOST
REAL*4 HDR,HDQ,HDi,HDiAve,HCDshr,HCDshrAve,ResDiscountRate,Tenure,LEAPYR
INTEGER HDRfy,HDRly
REAL*4 EH
REAL*4 TCW_SHR,FCW_SHR
REAL*4 TMF_SHR,SMF_SHR,BMF_SHR,CH_SHR,UP_SHR
REAL*4 RACSAT,RACUNITS,CACSAT,CACPR,FRZSAT,ELDRYPR,REFSAT
REAL*4 RENSHR
REAL*4 NEWDRYSAT
REAL*4 EHSHELL,ECSHELL,NHSHELL,NCSHELL,AHSHELL,ACSHELL,TECHG,LIMIT
INTEGER NHTRFL,NCLFL,NWHFL,NSTVFL,NDRYFL,NREFFL,NFRZFL,FHTRCON,FCLCON,FWHCON,FSTVCON,FDRYCON,FREFCON,FFRZCON,FCSWCON,FDSWCON,NCSWFL,NDSWFL,NSHTRFL
REAL*4 EQCESE,EQCRP90,EQCSR90,EQCADD,EQCREP,EQCSUR
INTEGER RSYR,PREVYR,EU,RTOVALUE,STEOBM,EPA111D
EXTERNAL RTOVALUE
REAL*4 WATERTOT(RECSYear:EndYr,mNumCR-2),COOKTOT(RECSYear:EndYr,mNumCR-2),DRYERTOT(RECSYear:EndYr,mNumCR-2)

END MODULE R_


!====================================================================
!     RESD SUBROUTINE
!====================================================================
SUBROUTINE RESD
USE R_
IMPLICIT NONE

!Dynamically assigns array dimensions only once in a NEMS run (RDM base year and first iteration)
IF (CurCalYr.EQ.RECSYear.AND.CURITR.EQ.1) THEN
  ALLOCATE(NEQTSHR(RECSYear:EndYr+1,MNUMRTTY,mNumBldg,mNumCR))
  ALLOCATE(REQTSHR(RECSYear:EndYr+1,MNUMRTTY,mNumBldg,mNumCR))
  ALLOCATE(EQR90FUT(RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2))
  ALLOCATE(EQREPFUT(RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2))
  ALLOCATE(EQADDFUT(RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2))
  ALLOCATE(EQR90RPFUT(RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2))
  ALLOCATE(EQCESEFUT(RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2))
  ALLOCATE(RSNEFDB1(mNumYr,MNUMRTTY,mNumBldg,mNumCR-2))
  ALLOCATE(RSEEFDB1(mNumYr,MNUMRTTY,mNumBldg,mNumCR-2))
  ALLOCATE(HTSHELLEFFWT(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR-2))
  ALLOCATE(HTSHELLWT(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR-2))
  ALLOCATE(HSHELL(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR-2))
  ALLOCATE(CSHELL(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR-2))
  ALLOCATE(SHELLBUILDS(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR-2))
  ALLOCATE(SHELLINVEST(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR))
  ALLOCATE(SHELLSUBSIDY(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR))
  ALLOCATE(SHELLSUBSIDY111D(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR))
  ALLOCATE(CLSHELLWT(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR-2))
  ALLOCATE(SHLEVELH(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR-2))
  ALLOCATE(HVEQSHR(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR-2))
  ALLOCATE(HEATINGTYPEPURCH(RECSYear:EndYr,MNUMRTTY,mNumBldg,mNumCR-2,2))
  ALLOCATE(NEQTSHRC(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR))
  ALLOCATE(LEARNFACT(mNumBldg,mNumCR-2))

  !Initialize allocated arrays
  NEQTSHR=0.0
  REQTSHR=0.0
  EQR90FUT=0.0
  EQREPFUT=0.0
  EQADDFUT=0.0
  EQR90RPFUT=0.0
  EQCESEFUT=0.0
  RSNEFDB1=0.0
  RSEEFDB1=0.0
  HTSHELLEFFWT=0.0
  HTSHELLWT=0.0
  HSHELL=0.0
  CSHELL=0.0
  SHELLBUILDS=0.0
  SHELLINVEST=0.0
  SHELLSUBSIDY=0.0
  SHELLSUBSIDY111D=0.0
  CLSHELLWT=0.0
  SHLEVELH=0.0
  HVEQSHR=0.0
  HEATINGTYPEPURCH=0.0
  NEQTSHRC=0.0
  LEARNFACT=0.0
ENDIF

RSYR=CurIYr+BaseYr-1
PREVYR=CurIYr-1
STEOBM=RTOVALUE("STEOBM  ",0)
EPA111D=RTOVALUE("EPA111D ",0)

!********************************************************************
!READ INPUT DATA IN RDM BASE YEAR AND FIRST ITERATION
!********************************************************************
IF (CurCalYr.LT.RECSYear) RETURN

IF (CurCalYr.EQ.RECSYear.AND.CURITR.EQ.1) THEN
  OPEN(9,FILE='RDM_OUT.TXT',FORM='FORMATTED')  !open before input files so it can be used, as needed, for output writes
  CALL RTEKREAD
  IF (IFMAX.NE.0) CALL PITCINIT
  CALL RDSQFOOT
  CALL DEGDAYREAD
  CALL BLDBASEREAD
  CALL RSUECSHLREAD
  CALL RSMELSREAD
  CALL RSMISCREAD
  CALL RSMLGTREAD
  CALL RDRET
  CALL INTEQT
  CALL RDHTREQC
  CALL RDEFF
  CALL RDSTEFF
  CALL RDESTARHOMES
  CALL RDUECS
  CALL RCONSFL
  CALL RDISTGEN
ENDIF

!********************************************************************
!STORE PRICES FOR EACH YEAR
!********************************************************************
IF (CurCalYr.GE.RECSYear) CALL RDPR

!********************************************************************
!BEGIN CALLING OTHER SUBROUTINES
!********************************************************************
IF (CurCalYr.EQ.RECSYear) THEN
  CALL EXCONS
  CALL RSBENCH
  CALL NEMSCN
  CALL RESDRP
ELSE
  IF (MOD(CurCalYr,4).EQ.0.AND.CurCalYr.LE.LastSTEOYr) THEN  !MOD(A,P) computes the remainder of the division of A by P
    LEAPYR= 366.0/365.0
  ELSE
    LEAPYR= 365.0/365.0
  ENDIF

  IF (IFMAX.NE.0) CALL RSPITC(IFMAX, LastSTEOYr)

  CALL NEWHSE

  !SPACE HEATING EQUIPMENT SUBROUTINES
  CALL SQFTCALC         
  CALL RSHVAC
  CALL RHTRTEC
  CALL RHTRADD

  !DISTRIBUTED GENERATION SUBROUTINE
  CALL RDISTGEN

  !SPACE COOLING EQUIPMENT SUBROUTINES
  CALL RCLTEC
  CALL RCLADD

  !CLOTHES WASHING EQUIPMENT SUBROUTINES
  CALL RCWTEC
  CALL RCWADD

  !DISHWASHING EQUIPMENT SUBROUTINES
  CALL RDWTEC
  CALL RDWADD

  !WATER HEATING EQUIPMENT SUBROUTINES
  EU = 5  !EU = 5 is water heating; this is used by REUADD subroutine
  CALL RWHTEC
  CALL REUADD

  !COOKING EQUIPMENT SUBROUTINES
  EU = 6  !EU = 6 is cooking; this is used by REUADD subroutine
  CALL RSTVTEC
  CALL REUADD

  !CLOTHES DRYING EQUIPMENT SUBROUTINES
  CALL RDRYTEC
  CALL RDRYADD

  !REFRIGERATION EQUIPMENT SUBROUTINES
  CALL RREFTEC
  CALL RREFADD

  !FREEZING EQUIPMENT SUBROUTINES
  CALL RFRZTEC
  CALL RFRZADD

  !CONSUMPTION SUBROUTINES
  CALL RHTRCON
  CALL RCLCON
  CALL RCWCON
  CALL RDWCON
  CALL RWHCON
  CALL RSTOVCON
  CALL RDRYCON
  CALL RREFCON
  CALL RFRZCON

  !LIGHTING, OTHER ELECTRICITY (MELs), SECONDARY HEATING, & SMALL APPLIANCE SUBROUTINES
  CALL LTCNS
  CALL APCNS
  CALL SHTCNS
  CALL APPCNS

  !NEMS CONSUMPTION SUBROUTINE
  CALL FUELCN
  CALL RSBENCH
  CALL NEMSCN
  CALL RESDRP
  CALL CALC111D  !111(D) RESCINDED
ENDIF  !check CurCalYr >= RECSYear

!*******************************************************************
!REPORTING SUBROUTINES
!*******************************************************************
IF ((CurCalYr-BaseYr+1.EQ.LastYr).AND.(FCRL.EQ.1)) THEN
  CALL NHTSHR
  CALL RESDRP2
  CALL RESDBOUT
ENDIF

CONTAINS


!==============================================================================
! RTEKREAD READS THE RESIDENTIAL MENUS FOR EQUIPMENT, SHELLS, AND SUBSIDIES
!  RSMESS.XLSX: RSCLASS, RSMEQP, RSSWITCH, and RSMSHL
!==============================================================================
SUBROUTINE RTEKREAD
IMPLICIT NONE

!These are for printing out the read-in named range data for debugging purposes
COMMON/NEMSWK1/XMLOUT

INTEGER FILE_MGR                  ! File manager
INTEGER*4 INFILE,               & ! File handle
 I,xlI,NewDiv,NewRowI,J,K,Type, & ! General indices
 Y,D,R,T,B, &
 LASTEU,LASTCLAS,LASTTYPE

INTEGER XMLOUT

INTEGER*4 ClsRecords,EqpRecords,NewEqpRecords,ShlRecords !number/count of records in the RSCLASS, RSMEQP, and RSMSHL tabs of input file, calculated and read-in from input file

INTEGER RECCL,RECCLSW !equipment class indexes for RSSWITCH array

INTEGER*2, ALLOCATABLE :: xlRTCLENDU(:),xlRTCLEQCL(:),xlRTCLTYPT(:),xlRTCLPNTR(:),xlRTCLREPL(:),xlRTFUEL(:),xlRTFFAN(:),                             & !RSCLASS
                          xlRTTYENDU(:),xlRTTYEQCL(:),xlRTEQTYPE(:),xlRTINITYR(:),xlRTLASTYR(:),xlRTCENDIV(:),xlHVACPNTR(:),xlRTTYPNTR(:),           & !RSMEQP
                          xlRPINSCOST(:,:),                                                                                                          & !RSSWITCH
                          xlRSCENDIV(:),xlRSBTYPE(:),xlHVHTEQCL(:),xlHVHTEQTY(:),xlHVCLEQCL(:),xlHVCLEQTY(:),xlHVFYEAR(:),xlHVLYEAR(:),xlHVPACKG(:)    !RSMSHL

CHARACTER*10, ALLOCATABLE :: xlRTCLNAME(:), & !RSCLASS
                             xlRTMATURE(:), & !RSMEQP
                             xlRTTYNAME(:)    !RSMEQP

CHARACTER*14, ALLOCATABLE :: xlHVPGNAME(:)    !RSMSHL

REAL*4, ALLOCATABLE :: xlRTALPHA(:),xlRTBASEFF(:),xlRTK(:),xlRTLAMBDA(:),xlRTFCBETA(:),xlRTSWFACT(:),xlRTSWBETA(:),xlRTSWBIAS(:),                             & !RSCLASS
                       xlCWMEF(:),xlLOADADJ(:),xlRTEQEFF(:),xlRTEQCOST(:),xlRTRECOST(:),xlRTEQSUB(:),xlRTRESUB(:),xlRTEQSUBN(:),xlRTRESUBN(:),                & !RSMEQP
                        xlRTEQSUB111D(:),xlRTRESUB111D(:),xlRTCOSTP1(:),xlRTCOSTP2(:),xlRTCOSTP3(:),xlRTECBTA1(:),xlRTECBTA2(:),xlRTECBTA3(:),xlRTECBIAS(:),  & !RSMEQP
                       xlHVHEATFACTOR(:),xlHVCOOLFACTOR(:),xlHTSHEFF(:),xlCLSHEFF(:),xlHTSHBASE(:),xlCLSHBASE(:),xlSHELCOST(:),xlSHELSUB(:),xlSHELSUB111D(:)    !RSMSHL

ALLOCATE (xlRTCLENDU(mNumRTCl),xlRTCLEQCL(mNumRTCl),xlRTCLTYPT(mNumRTCl),xlRTCLPNTR(mNumRTCl),xlRTCLREPL(mNumRTCl),xlRTFUEL(mNumRTCl),xlRTFFAN(mNumRTCl),          & !RSCLASS
           xlRTCLNAME(mNumRTCl),xlRTALPHA(mNumRTCl),xlRTBASEFF(mNumRTCl),xlRTK(mNumRTCl),xlRTLAMBDA(mNumRTCl),xlRTFCBETA(mNumRTCl),xlRTSWFACT(mNumRTCl),           & !RSCLASS
           xlRTSWBETA(mNumRTCl),xlRTSWBIAS(mNumRTCl),                                                                                                              & !RSCLASS
          xlRTTYENDU(MNUMRTTY),xlRTTYEQCL(MNUMRTTY),xlRTEQTYPE(MNUMRTTY),xlRTINITYR(MNUMRTTY),xlRTLASTYR(MNUMRTTY),xlRTCENDIV(MNUMRTTY),xlHVACPNTR(MNUMRTTY)       & !RSMEQP
           ,xlRTTYPNTR(MNUMRTTY),xlRTTYNAME(MNUMRTTY),xlRTMATURE(MNUMRTTY),xlCWMEF(MNUMRTTY),xlLOADADJ(MNUMRTTY),xlRTEQEFF(MNUMRTTY),xlRTEQCOST(MNUMRTTY),         & !RSMEQP
           xlRTRECOST(MNUMRTTY),xlRTEQSUB(MNUMRTTY),xlRTRESUB(MNUMRTTY),xlRTEQSUBN(MNUMRTTY),xlRTRESUBN(MNUMRTTY),xlRTEQSUB111D(MNUMRTTY),xlRTRESUB111D(MNUMRTTY), & !RSMEQP
           xlRTCOSTP1(MNUMRTTY),xlRTCOSTP2(MNUMRTTY),xlRTCOSTP3(MNUMRTTY),xlRTECBTA1(MNUMRTTY),xlRTECBTA2(MNUMRTTY),xlRTECBTA3(MNUMRTTY),xlRTECBIAS(MNUMRTTY),     & !RSMEQP
          xlRSCENDIV(MNUMHVAC),xlRSBTYPE(MNUMHVAC),xlHVHTEQCL(MNUMHVAC),xlHVHTEQTY(MNUMHVAC),xlHVCLEQCL(MNUMHVAC),xlHVCLEQTY(MNUMHVAC),xlHVFYEAR(MNUMHVAC),        & !RSMSHL
           xlHVLYEAR(MNUMHVAC),xlHVPACKG(MNUMHVAC),xlHVPGNAME(MNUMHVAC),xlHVHEATFACTOR(MNUMHVAC),xlHVCOOLFACTOR(MNUMHVAC),xlHTSHEFF(MNUMHVAC),xlCLSHEFF(MNUMHVAC), & !RSMSHL
           xlHTSHBASE(MNUMHVAC),xlCLSHBASE(MNUMHVAC),xlSHELCOST(MNUMHVAC),xlSHELSUB(MNUMHVAC),xlSHELSUB111D(MNUMHVAC),xlRPINSCOST(mNumRTCl,mNumRTCl))                !RSMSHL, RSSWITCH


!Initialize arrays
xlRTCLENDU=0; xlRTCLEQCL=0; xlRTCLTYPT=0; xlRTCLPNTR=0; xlRTCLREPL=0; xlRTFUEL=0; xlRTFFAN=0 !RSCLASS
 xlRTCLNAME=""; xlRTALPHA=0; xlRTBASEFF=0; xlRTK=0; xlRTLAMBDA=0; xlRTFCBETA=0; xlRTSWFACT=0; xlRTSWBETA=0; xlRTSWBIAS=0 !RSCLASS
xlRTTYENDU=0; xlRTTYEQCL=0; xlRTEQTYPE=0; xlRTINITYR=0; xlRTLASTYR=0; xlRTCENDIV=0; xlHVACPNTR=0; xlRTTYPNTR=0; xlRTTYNAME=""; xlRTMATURE=""  !RSMEQP
 xlCWMEF=0; xlLOADADJ=0; xlRTEQEFF=0; xlRTEQCOST=0; xlRTRECOST=0; xlRTEQSUB=0; xlRTRESUB=0; xlRTEQSUBN=0; xlRTRESUBN=0              !RSMEQP
 xlRTEQSUB111D=0; xlRTRESUB111D=0; xlRTCOSTP1=0; xlRTCOSTP2=0; xlRTCOSTP3=0; xlRTECBTA1=0; xlRTECBTA2=0; xlRTECBTA3=0; xlRTECBIAS=0 !RSMEQP
xlRSCENDIV=0; xlRSBTYPE=0; xlHVHTEQCL=0; xlHVHTEQTY=0; xlHVCLEQCL=0; xlHVCLEQTY=0; xlHVFYEAR=0; xlHVLYEAR=0; xlHVPACKG=0; xlHVPGNAME="" !RSMSHL
 xlHVHEATFACTOR=0; xlHVCOOLFACTOR=0; xlHTSHEFF=0; xlCLSHEFF=0; xlHTSHBASE=0; xlCLSHBASE=0; xlSHELCOST=0; xlSHELSUB=0; xlSHELSUB111D=0;  !RSMSHL
xlRPINSCOST=0                                                                                                                       !RSSWITCH

DO I=1,MNUMENDU
  RTCLEUPT(I) = 0 ! Last record # in RSCLASS for each end use
  RTTYEUPT(I) = 0 ! Last record # in RSMEQP for each end use
  RTTYPECT(I) = 0 ! Last type # in type arrays for each end use
ENDDO

LASTCLAS=-1
LASTEU  =-1
LASTTYPE=-1

RTCLCNT = 0
RTEUCNT = 0
RTTYCNT = 0

NewDiv = 0  !Used to increment expanded RSMEQP CD from 11 to 1-9
NewRowI = 0  !Counts number of "new rows" endogenously inserted into

!Read technology input data for RSCLASS, RSMEQP, and RSMSHL from RSMESS.xlsx Excel workbook
INFILE= FILE_MGR ('O','RSMESS',.FALSE.)

!Turn on debugging write-out of read-in named ranges to RDM_XLSDBG.txt output file if scedes value PRTDBGR=1
IF (PRTDBGR.EQ.1) THEN
  XMLOUT = FILE_MGR('O','RSXLSDBG',.TRUE.)
ENDIF

!Call subroutine to read all defined ranges from worksheets in workbook
! This stores the ranges in a temporary data area that can get overwritten by the next model if they use it.
! All ranges have to be extracted from the temporary area immediately.
CALL ReadRngXLSX(infile,'RSCLASS') !read worksheet named 'RSCLASS' in Excel workbook RSMESS.xlsx

!*****************************************************************************************************
!Copy each range from worksheet data area to variables using nemswk1.f subroutines
! Example: ('Variable_Description',VariableName,#rows_of_data,#columns_of_data,#groups)
!
! GETRNGI: Copies an INTEGER*2 variable from the worksheet data area into the variable.
!          -The variable dimensions are passed as the 3rd, 4th, and 5th arguments, (e.g., ... 1,1,1).
!          -A variable with dimesions of 1,1,1 is a scalar (e.g., ClsRecords).
!          -A variable with dimensions of n,1,1 is a one-dimensional array with n elements
! GETRNGR: Copies a REAL variable from the worksheet data area into the variable.
! GETRNGC: Copies a CHARACTER variable from the worksheet data area into the variable.
!          Max string length is set in wk1block includes file (i.e., MaxStrings=30000).
!*****************************************************************************************************

!Read in record count from spreadsheet calculation
CALL GETRNGI('ClsRecords        ',ClsRecords,1,1,1)          !RSCLASS

!Read the values of RSCLASS, later to be parsed into NEMS residential variables
! Dimensioned by technology, vintage, etc.
CALL GETRNGI('xlRTCLENDU        ',xlRTCLENDU,ClsRecords,1,1) !RSCLASS
CALL GETRNGI('xlRTCLEQCL        ',xlRTCLEQCL,ClsRecords,1,1) !RSCLASS
CALL GETRNGI('xlRTCLTYPT        ',xlRTCLTYPT,ClsRecords,1,1) !RSCLASS
CALL GETRNGI('xlRTCLPNTR        ',xlRTCLPNTR,ClsRecords,1,1) !RSCLASS
CALL GETRNGI('xlRTCLREPL        ',xlRTCLREPL,ClsRecords,1,1) !RSCLASS
CALL GETRNGI('xlRTFUEL          ',xlRTFUEL  ,ClsRecords,1,1) !RSCLASS
CALL GETRNGI('xlRTFFAN          ',xlRTFFAN  ,ClsRecords,1,1) !RSCLASS
CALL GETRNGR('xlRTBASEFF        ',xlRTBASEFF,ClsRecords,1,1) !RSCLASS
CALL GETRNGR('xlRTALPHA         ',xlRTALPHA ,ClsRecords,1,1) !RSCLASS
CALL GETRNGR('xlRTK             ',xlRTK     ,ClsRecords,1,1) !RSCLASS
CALL GETRNGR('xlRTLAMBDA        ',xlRTLAMBDA,ClsRecords,1,1) !RSCLASS
CALL GETRNGR('xlRTFCBETA        ',xlRTFCBETA,ClsRecords,1,1) !RSCLASS
CALL GETRNGR('xlRTSWFACT        ',xlRTSWFACT,ClsRecords,1,1) !RSCLASS
CALL GETRNGR('xlRTSWBETA        ',xlRTSWBETA,ClsRecords,1,1) !RSCLASS
CALL GETRNGR('xlRTSWBIAS        ',xlRTSWBIAS,ClsRecords,1,1) !RSCLASS
CALL GETRNGC('xlRTCLNAME        ',xlRTCLNAME,ClsRecords,1,1) !RSCLASS

!Convert Excel named ranges to NEMS variables
DO I = 1,ClsRecords           !RSCLASS
  RTCLENDU(I) = xlRTCLENDU(I) !RSCLASS
  RTCLEQCL(I) = xlRTCLEQCL(I) !RSCLASS
  RTCLTYPT(I) = xlRTCLTYPT(I) !RSCLASS
  RTCLPNTR(I) = xlRTCLPNTR(I) !RSCLASS
  RTCLREPL(I) = xlRTCLREPL(I) !RSCLASS
  RTFUEL(I) = xlRTFUEL(I)     !RSCLASS
  RTFFAN(I) = xlRTFFAN(I)     !RSCLASS
  RTBASEFF(RECSYear,I) = xlRTBASEFF(I) !RSCLASS
  RTALPHA(I) = xlRTALPHA(I)   !RSCLASS
  RTK(I) = xlRTK(I)           !RSCLASS
  RTLAMBDA(I) = xlRTLAMBDA(I) !RSCLASS
  RTFCBETA(I) = xlRTFCBETA(I) !RSCLASS
  RTSWFACT(I) = xlRTSWFACT(I) !RSCLASS
  RTSWBETA(I) = xlRTSWBETA(I) !RSCLASS
  RTSWBIAS(I) = xlRTSWBIAS(I) !RSCLASS
  RTCLNAME(I) = xlRTCLNAME(I) !RSCLASS

  !Processing RSCLASS data after being read in

  !Un-commenting this code turns off switching for a quick test of how much actually goes on.
  !RTSWFACT(I) = 0.

  RTCLCNT=RTCLCNT+1
  J=RTCLENDU(I)
  IF (J.NE.LASTEU) RTEUCNT=RTEUCNT+1

  !Collect the raw data to compute the RSCLASS end-use pointers
  RTCLEUPT(J+1)=RTCLEUPT(J+1)+1 ! RTCLEUPT(I) is the last record # in RSCLASS for each end use
  LASTEU=J
ENDDO !ClsRecords

!Use raw data to assign pointers
DO J=1,RTEUCNT
  RTCLEUPT(J+1)=RTCLEUPT(J+1)+RTCLEUPT(J)
ENDDO

!If RSCLASS read was successful, print summary information to unit 6 (nohup.out)
WRITE(6,*) 'RESDMSG SUB_RTEKREAD: RSMESS.xls/RSCLASS EOF REACHED OK; COUNT = ',ClsRecords

CALL ReadRngXLSX(infile,'RSMEQP')  !read worksheet named 'RSMEQP' in Excel workbook RSMESS.xlsx

LASTEU=-1 ! Resets counter for value of last end use

!Read in record count from spreadsheet calculation
CALL GETRNGI('EqpRecords        ',EqpRecords,1,1,1)          !RSMEQP

!Read the values of RSMEQP, later to be parsed into NEMS residential variables
! Dimensioned by technology, vintage, etc.
CALL GETRNGI('RTEKDOLLARYR      ',RTEKDOLLARYR,1,1,1)        !RSMEQP
CALL GETRNGI('xlRTTYENDU        ',xlRTTYENDU,EqpRecords,1,1) !RSMEQP
CALL GETRNGI('xlRTTYEQCL        ',xlRTTYEQCL,EqpRecords,1,1) !RSMEQP
CALL GETRNGI('xlRTEQTYPE        ',xlRTEQTYPE,EqpRecords,1,1) !RSMEQP
CALL GETRNGI('xlRTINITYR        ',xlRTINITYR,EqpRecords,1,1) !RSMEQP
CALL GETRNGI('xlRTLASTYR        ',xlRTLASTYR,EqpRecords,1,1) !RSMEQP
CALL GETRNGI('xlRTCENDIV        ',xlRTCENDIV,EqpRecords,1,1) !RSMEQP
CALL GETRNGI('xlHVACPNTR        ',xlHVACPNTR,EqpRecords,1,1) !RSMEQP
CALL GETRNGI('xlRTTYPNTR        ',xlRTTYPNTR,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlCWMEF           ',xlCWMEF   ,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlLOADADJ         ',xlLOADADJ ,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTEQEFF         ',xlRTEQEFF ,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTEQCOST        ',xlRTEQCOST,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTRECOST        ',xlRTRECOST,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTEQSUB         ',xlRTEQSUB ,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTRESUB         ',xlRTRESUB ,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTEQSUBN        ',xlRTEQSUBN,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTRESUBN        ',xlRTRESUBN,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTEQSUB111D     ',xlRTEQSUB111D,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTRESUB111D     ',xlRTRESUB111D,EqpRecords,1,1) !RSMEQP
CALL GETRNGC('xlRTMATURE        ',xlRTMATURE,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTCOSTP1        ',xlRTCOSTP1,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTCOSTP2        ',xlRTCOSTP2,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTCOSTP3        ',xlRTCOSTP3,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTECBTA1        ',xlRTECBTA1,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTECBTA2        ',xlRTECBTA2,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTECBTA3        ',xlRTECBTA3,EqpRecords,1,1) !RSMEQP
CALL GETRNGR('xlRTECBIAS        ',xlRTECBIAS,EqpRecords,1,1) !RSMEQP
CALL GETRNGC('xlRTTYNAME        ',xlRTTYNAME,EqpRecords,1,1) !RSMEQP

!Convert Excel named ranges to NEMS variables and expand all instances where a single RSMEQP input row represents CD 1-9 (i.e., CD=11)
! NOTE: The xl input arrays only go up to EqpRecords but are ALLOCATED up to MNUMRTTY from RTEK (so MNUMRTTY could be 9*EqpRecords from RSMEQP to generously ensure proper array size)

NewEqpRecords = EqpRecords  !Used to increment expanded RSMEQP CD from 11 to 1-9

DO xlI = 1,EqpRecords  !RSMEQP
  I=xlI+NewRowI        !RSMEQP  !Used to skip over the newly expanded CD data when pulling next data from Excel named range
  IF (xlRTCENDIV(xlI) .EQ. 11) THEN !RSMEQP  !If CD=11, the row will be "copied" eight more times and each of the census divisions will be renumbered 1-9
    DO I = I,I+8       !RSMEQP  !Adding 8 represents the 8 new "rows" that are being endogenously added to expand from one CD row to nine CD rows in RSMEQP
      NewDiv=NewDiv+1  !RSMEQP  !Used to increment expanded CD from 11 to 1-9
      RTTYENDU(I) = xlRTTYENDU(xlI) !RSMEQP
      RTTYEQCL(I) = xlRTTYEQCL(xlI) !RSMEQP
      RTEQTYPE(I) = xlRTEQTYPE(xlI) !RSMEQP
      RTINITYR(I) = xlRTINITYR(xlI) !RSMEQP
      RTLASTYR(I) = xlRTLASTYR(xlI) !RSMEQP
      RTCENDIV(I) = NewDiv          !RSMEQP  !Used to increment expanded CD from 11 to 1-9
      HVACPNTR(I) = xlHVACPNTR(xlI) !RSMEQP
      RTTYPNTR(I) = xlRTTYPNTR(xlI) !RSMEQP
      CWMEF(I) = xlCWMEF(xlI)       !RSMEQP
      LOADADJ(I) = xlLOADADJ(xlI)   !RSMEQP
      RTEQEFF(I) = xlRTEQEFF(xlI)   !RSMEQP
      RTEQCOST(I) = xlRTEQCOST(xlI) !RSMEQP
      RTRECOST(I) = xlRTRECOST(xlI) !RSMEQP
      RTEQSUB(I) = xlRTEQSUB(xlI)   !RSMEQP
      RTRESUB(I) = xlRTRESUB(xlI)   !RSMEQP
      RTEQSUBN(I) = xlRTEQSUBN(xlI) !RSMEQP
      RTRESUBN(I) = xlRTRESUBN(xlI) !RSMEQP
      RTEQSUB111D(I) = xlRTEQSUB111D(xlI) !RSMEQP
      RTRESUB111D(I) = xlRTRESUB111D(xlI) !RSMEQP
      RTMATURE(I) = xlRTMATURE(xlI) !RSMEQP
      RTCOSTP1(I) = xlRTCOSTP1(xlI) !RSMEQP
      RTCOSTP2(I) = xlRTCOSTP2(xlI) !RSMEQP
      RTCOSTP3(I) = xlRTCOSTP3(xlI) !RSMEQP
      RTECBTA1(I) = xlRTECBTA1(xlI) !RSMEQP
      RTECBTA2(I) = xlRTECBTA2(xlI) !RSMEQP
      RTECBTA3(I) = xlRTECBTA3(xlI) !RSMEQP
      RTECBIAS(I) = xlRTECBIAS(xlI) !RSMEQP
      RTTYNAME(I) = xlRTTYNAME(xlI) !RSMEQP
    ENDDO !I  !RSMEQP  !Adding 8 represents the 8 new "rows" that are being endogenously "inserted" to expand from 1 CD row to 9 CD rows in RSMEQP
    NewDiv=0  !RSMEQP  !Used to reset increment for expanding CD from 11 to 1-9
    NewRowI=NewRowI+8  !RSMEQP  !Used to skip over the newly expanded CD data when pulling next data from Excel named range
    NewEqpRecords=NewEqpRecords+8  !Adding 8 represents the 8 new "rows" that are being endogenously "inserted" each time a "row" is endogenously "expanded" from 1 CD row to 9 CD rows in RSMEQP
  ELSEIF (xlRTCENDIV(xlI) .NE. 11) THEN  !If not a national row, THEN use the row from RSMEQP as entered into RSMESS.xlsx
    RTTYENDU(I) = xlRTTYENDU(xlI) !RSMEQP
    RTTYEQCL(I) = xlRTTYEQCL(xlI) !RSMEQP
    RTEQTYPE(I) = xlRTEQTYPE(xlI) !RSMEQP
    RTINITYR(I) = xlRTINITYR(xlI) !RSMEQP
    RTLASTYR(I) = xlRTLASTYR(xlI) !RSMEQP
    RTCENDIV(I) = xlRTCENDIV(xlI) !RSMEQP
    HVACPNTR(I) = xlHVACPNTR(xlI) !RSMEQP
    RTTYPNTR(I) = xlRTTYPNTR(xlI) !RSMEQP
    CWMEF(I) = xlCWMEF(xlI)       !RSMEQP
    LOADADJ(I) = xlLOADADJ(xlI)   !RSMEQP
    RTEQEFF(I) = xlRTEQEFF(xlI)   !RSMEQP
    RTEQCOST(I) = xlRTEQCOST(xlI) !RSMEQP
    RTRECOST(I) = xlRTRECOST(xlI) !RSMEQP
    RTEQSUB(I) = xlRTEQSUB(xlI)   !RSMEQP
    RTRESUB(I) = xlRTRESUB(xlI)   !RSMEQP
    RTEQSUBN(I) = xlRTEQSUBN(xlI) !RSMEQP
    RTRESUBN(I) = xlRTRESUBN(xlI) !RSMEQP
    RTEQSUB111D(I) = xlRTEQSUB111D(xlI) !RSMEQP
    RTRESUB111D(I) = xlRTRESUB111D(xlI) !RSMEQP
    RTMATURE(I) = xlRTMATURE(xlI) !RSMEQP
    RTCOSTP1(I) = xlRTCOSTP1(xlI) !RSMEQP
    RTCOSTP2(I) = xlRTCOSTP2(xlI) !RSMEQP
    RTCOSTP3(I) = xlRTCOSTP3(xlI) !RSMEQP
    RTECBTA1(I) = xlRTECBTA1(xlI) !RSMEQP
    RTECBTA2(I) = xlRTECBTA2(xlI) !RSMEQP
    RTECBTA3(I) = xlRTECBTA3(xlI) !RSMEQP
    RTECBIAS(I) = xlRTECBIAS(xlI) !RSMEQP
    RTTYNAME(I) = xlRTTYNAME(xlI) !RSMEQP
  ENDIF !xlRTCENDIV
ENDDO !xlI

!Write out expanded RSMEQP inputs to same debug file (RDM_XLSDBG.txt) that Excel named ranges are written into.
IF (PRTDBGR.EQ.1) THEN
  WRITE(XMLOUT,9998) 'I ', 'RTTYENDU(I) ', 'RTTYEQCL(I) ', 'RTEQTYPE(I) ', 'RTINITYR(I) ', 'RTLASTYR(I) ', 'RTCENDIV(I) ', 'HVACPNTR(I) ', 'RTTYPNTR(I) ', 'CWMEF(I) ', 'LOADADJ(I) ', 'RTEQEFF(I) ', 'RTEQCOST(I) ', &
   'RTRECOST(I) ', 'RTEQSUB(I) ', 'RTRESUB(I) ', 'RTEQSUBN(I) ', 'RTRESUBN(I) ', 'RTEQSUB111D(I) ', 'RTRESUB111D(I) ', 'RTMATURE(I) ', 'RTCOSTP1(I) ', 'RTCOSTP2(I) ', 'RTCOSTP3(I) ', 'RTECBTA1(I) ', 'RTECBTA2(I) ', &
   'RTECBTA3(I) ', 'RTECBIAS(I) ', 'RTTYNAME(I) '
  DO I = 1,NewEqpRecords
    WRITE(XMLOUT,9999) I, RTTYENDU(I), RTTYEQCL(I), RTEQTYPE(I), RTINITYR(I), RTLASTYR(I), RTCENDIV(I), HVACPNTR(I), RTTYPNTR(I), CWMEF(I), LOADADJ(I), RTEQEFF(I), RTEQCOST(I), &
     RTRECOST(I), RTEQSUB(I), RTRESUB(I), RTEQSUBN(I), RTRESUBN(I), RTEQSUB111D(I), RTRESUB111D(I), RTMATURE(I), RTCOSTP1(I), RTCOSTP2(I), RTCOSTP3(I), RTECBTA1(I), RTECBTA2(I), &
     RTECBTA3(I), RTECBIAS(I), RTTYNAME(I)
  ENDDO  !I
  9998 FORMAT(29A)
  9999 FORMAT(9(I4,' '),11(F8.3,' '),A,' ',7(F8.3,' '),A)
ENDIF

DO I = 1,NewEqpRecords
  !Immediately subtract subsidy value from equipment costs
  RTEQCOST(I)=RTEQCOST(I)-RTEQSUB(I)-RTEQSUBN(I)-FLOAT(EPA111D)*RTEQSUB111D(I)
  RTRECOST(I)=RTRECOST(I)-RTRESUB(I)-RTRESUBN(I)-FLOAT(EPA111D)*RTRESUB111D(I)

  !Create variables for HVAC subroutine
  DO Y=RECSYear,EndYr
    !HVACPNTR counts equipment types across heating and cooling.
    !Note that the first cooling type is numbered nHeatTypes+1 because space heating technologies are listed before space cooling
    ! for space heating HVACPNTR = RTEQTYPE
    ! for space cooling HVACPNTR = RTEQTYPE + nHeatTypes (current number of heating system types)
    IF (HVACPNTR(I).GT.nHeatTypes) THEN
      Type=HVACPNTR(I)-nHeatTypes
      D=RTCENDIV(I)
      IF (Y.GE.RTINITYR(I).AND.Y.LE.RTLASTYR(I)) THEN
        ACEFF(Type,Y,D)=RTEQEFF(I)
        ACICOST(Type,Y,D)=RTEQCOST(I)
      ENDIF !Y
    ENDIF !HVACPNTR
  ENDDO !Y

  RTTYCNT=RTTYCNT+1
  J=RTTYENDU(I)

  !Collect the raw data to compute the RSMEQP end-use pointers
  RTTYEUPT(J+1)=RTTYEUPT(J+1)+1 ! RTTYEUPT(I) is the last record # in RSMEQP for each end use
  K=RTEQTYPE(I)
  IF (J.NE.LASTEU.OR.K.NE.LASTTYPE)RTTYPECT(J+1)=RTTYPECT(J+1)+1 ! RTTYPECT(I) is the last type # in type arrays for each end use
  LASTEU=J
  LASTTYPE=K
ENDDO !NewEqpRecords

!Use raw data to assign pointers
DO J=1,RTEUCNT
  RTTYEUPT(J+1)=RTTYEUPT(J+1)+RTTYEUPT(J)
  RTTYPECT(J+1)=RTTYPECT(J+1)+RTTYPECT(J)
ENDDO

!If RSMEQP read was successful, print summary information to unit 6 (nohup.out)
WRITE(6,*) 'RESDMSG SUB_RTEKREAD: RSMESS.xls/RSMEQP EOF REACHED OK; COUNT = ',EqpRecords  !reports number of records read-in from RSMEQP rather than counting the expanded CD rows created endogenously

CALL ReadRngXLSX(infile,'RSSWITCH')  !read worksheet named 'RSSWITCH' in Excel workbook RSMESS.xlsx

!Read the values of RSSWITCH, later to be parsed into NEMS residential variables
! Dimensioned by switch-from (RECCL) and switch-to (RECCLSW) equipment class
CALL GETRNGI('xlRPINSCOST       ',xlRPINSCOST(1:ClsRecords-2,1:ClsRecords-2),ClsRecords-2,ClsRecords-2,1)  !RSSWITCH

DO RECCL=1,RTCLCNT-2 !RTCLCNT-2 RECORDS BECAUSE NO SWITCHING FOR REFRIGERTION OR FREEZING EQUIPMENT CLASSES
  DO RECCLSW=1,RTCLCNT-2
    RPINSCOST(RECCL,RECCLSW) = xlRPINSCOST(RECCLSW,RECCL)  !Transposes GETRNGI read-in to be consistent with RPINSCOST use
    IF (PRTDBGR.EQ.1) THEN	!TODO - remove write code after verifying
      WRITE (XMLOUT,'("RECCLSW,RECCL,xlRPINSCOST(RECCLSW,RECCL),RPINSCOST(RECCL,RECCLSW) ",5i6)') RECCLSW, RECCL, xlRPINSCOST(RECCLSW,RECCL), RPINSCOST(RECCL,RECCLSW)	!TODO
    ENDIF
  ENDDO
ENDDO

!If RSCLASS read was successful, print summary information to unit 6 (nohup.out)
 WRITE(6,*) 'RESDMSG SUB_RTEKREAD: RSMESS.xls/RSSWITCH EOF REACHED OK; COUNT = ',(ClsRecords-2)*(ClsRecords-2)

CALL ReadRngXLSX(infile,'RSMSHL')  !read worksheet named 'RSMSHL' in Excel workbook RSMESS.xlsx

!Read in record count from spreadsheet calculation
CALL GETRNGI('ShlRecords      ',ShlRecords,1,1,1)             !RSMSHL

!Read the values of RSMSHL, later to be parsed into NEMS residential variables
! Dimensioned by technology, vintage, etc.
CALL GETRNGI('RSHLdollarYR      ',RSHLdollarYR,1,1,1)         !RSMSHL
CALL GETRNGI('xlRSCENDIV        ',xlRSCENDIV,ShlRecords,1,1)  !RSMSHL
CALL GETRNGI('xlRSBTYPE         ',xlRSBTYPE ,ShlRecords,1,1)  !RSMSHL
CALL GETRNGI('xlHVHTEQCL        ',xlHVHTEQCL,ShlRecords,1,1)  !RSMSHL
CALL GETRNGI('xlHVHTEQTY        ',xlHVHTEQTY,ShlRecords,1,1)  !RSMSHL
CALL GETRNGI('xlHVCLEQCL        ',xlHVCLEQCL,ShlRecords,1,1)  !RSMSHL
CALL GETRNGI('xlHVCLEQTY        ',xlHVCLEQTY,ShlRecords,1,1)  !RSMSHL
CALL GETRNGI('xlHVFYEAR         ',xlHVFYEAR ,ShlRecords,1,1)  !RSMSHL
CALL GETRNGI('xlHVLYEAR         ',xlHVLYEAR ,ShlRecords,1,1)  !RSMSHL
CALL GETRNGR('xlHVHEATFACTOR    ',xlHVHEATFACTOR,ShlRecords,1,1) !RSMSHL
CALL GETRNGR('xlHVCOOLFACTOR    ',xlHVCOOLFACTOR,ShlRecords,1,1) !RSMSHL
CALL GETRNGR('xlHTSHEFF         ',xlHTSHEFF ,ShlRecords,1,1)  !RSMSHL
CALL GETRNGR('xlCLSHEFF         ',xlCLSHEFF ,ShlRecords,1,1)  !RSMSHL
CALL GETRNGR('xlHTSHBASE        ',xlHTSHBASE ,ShlRecords,1,1) !RSMSHL
CALL GETRNGR('xlCLSHBASE        ',xlCLSHBASE ,ShlRecords,1,1) !RSMSHL
CALL GETRNGR('xlSHELCOST        ',xlSHELCOST ,ShlRecords,1,1) !RSMSHL
CALL GETRNGR('xlSHELSUB         ',xlSHELSUB, ShlRecords,1,1)  !RSMSHL
CALL GETRNGR('xlSHELSUB111D     ',xlSHELSUB111D,ShlRecords,1,1) !RSMSHL
CALL GETRNGI('xlHVPACKG         ',xlHVPACKG ,ShlRecords,1,1)  !RSMSHL
CALL GETRNGC('xlHVPGNAME        ',xlHVPGNAME,ShlRecords,1,1)  !RSMSHL

!Convert Excel named ranges to NEMS variables
DO I = 1,ShlRecords           !RSMSHL
  RSCENDIV(I) = xlRSCENDIV(I) !RSMSHL
  RSBTYPE(I) = xlRSBTYPE(I)   !RSMSHL
  HVHTEQCL(I) = xlHVHTEQCL(I) !RSMSHL
  HVHTEQTY(I) = xlHVHTEQTY(I) !RSMSHL
  HVCLEQCL(I) = xlHVCLEQCL(I) !RSMSHL
  HVCLEQTY(I) = xlHVCLEQTY(I) !RSMSHL
  HVFYEAR(I) = xlHVFYEAR(I)   !RSMSHL
  HVLYEAR(I) = xlHVLYEAR(I)   !RSMSHL
  HVHEATFACTOR(I) = xlHVHEATFACTOR(I) !RSMSHL
  HVCOOLFACTOR(I) = xlHVCOOLFACTOR(I) !RSMSHL
  HTSHEFF(I) = xlHTSHEFF(I)   !RSMSHL
  CLSHEFF(I) = xlCLSHEFF(I)   !RSMSHL
  HTSHBASE(I) = xlHTSHBASE(I) !RSMSHL
  CLSHBASE(I) = xlCLSHBASE(I) !RSMSHL
  SHELCOST(I) = xlSHELCOST(I) !RSMSHL
  SHELSUB(I) = xlSHELSUB(I)   !RSMSHL
  SHELSUB111D(I) = xlSHELSUB111D(I) !RSMSHL
  HVPACKG(I) = xlHVPACKG(I)   !RSMSHL
  HVPGNAME(I) = xlHVPGNAME(I) !RSMSHL

  !Processing RSMSHL data after being read in
  !Convert shell costs to RSMEQP dollar-year costs (if different)
  SHELCOST(I)    = SHELCOST(I)    *MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(rshldollaryr-BaseYr+1)
  SHELSUB(I)     = SHELSUB(I)     *MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(rshldollaryr-BaseYr+1)
  SHELSUB111D(I) = SHELSUB111D(I) *MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(rshldollaryr-BaseYr+1)

  !Immediately subtract subsidies from shell cost
  SHELCOST(I) = (SHELCOST(I)-SHELSUB(I)-SHELSUB111D(I)*FLOAT(EPA111D))
ENDDO !ShlRecords

!If RSMSHL read was successful, print summary information to unit 6 (nohup.out)
WRITE(6,*) 'RESDMSG SUB_RTEKREAD: RSMESS.xls/RSMSHL EOF REACHED OK; COUNT = ',ShlRecords

!Close input workbook
INFILE= FILE_MGR ('C','RSMESS',.FALSE.)

!Turn off debugging write-out of read-in named ranges to RDM_XLSDBG.txt output file
IF (PRTDBGR.EQ.1) THEN
  XMLOUT = FILE_MGR('C','RSXLSDBG',.TRUE.) ! Comment out this line of code to turn off
  XMLOUT = 0
ENDIF

DEALLOCATE (xlRTCLENDU,xlRTCLEQCL,xlRTCLTYPT,xlRTCLPNTR,xlRTCLREPL,xlRTFUEL,xlRTFFAN,                       & !RSCLASS
             xlRTCLNAME,xlRTALPHA,xlRTBASEFF,xlRTK,xlRTLAMBDA,xlRTFCBETA,xlRTSWFACT,xlRTSWBETA,xlRTSWBIAS,  & !RSCLASS
            xlRTTYENDU,xlRTTYEQCL,xlRTEQTYPE,xlRTINITYR,xlRTLASTYR,xlRTCENDIV,xlHVACPNTR,xlRTTYPNTR,      & !RSMEQP
             xlRTTYNAME,xlRTMATURE,xlCWMEF,xlLOADADJ,xlRTEQEFF,xlRTEQCOST,xlRTRECOST,xlRTEQSUB,xlRTRESUB, & !RSMEQP
             xlRTEQSUBN,xlRTRESUBN,xlRTEQSUB111D,xlRTRESUB111D,xlRTCOSTP1,xlRTCOSTP2,                     & !RSMEQP
             xlRTCOSTP3,xlRTECBTA1,xlRTECBTA2,xlRTECBTA3,xlRTECBIAS,                                      & !RSMEQP
            xlRSCENDIV,xlRSBTYPE,xlHVHTEQCL,xlHVHTEQTY,xlHVCLEQCL,xlHVCLEQTY,xlHVFYEAR,xlHVLYEAR, & !RSMSHL
             xlHVPACKG,xlHVPGNAME,xlHVHEATFACTOR,xlHVCOOLFACTOR,xlHTSHEFF,xlCLSHEFF,xlHTSHBASE,   & !RSMSHL
             xlCLSHBASE,xlSHELCOST,xlSHELSUB,xlSHELSUB111D,xlRPINSCOST)                             !RSMSHL, RSSWITCH

END SUBROUTINE RTEKREAD


!==============================================================================
! READ HEATING AND COOLING DEGREE DAY (HDD/CDD) DATA
!  KDEGDAY.TXT
!==============================================================================
SUBROUTINE DEGDAYREAD
IMPLICIT NONE

INTEGER FILE_MGR        !FILE MANAGER
INTEGER*4 INFILE,     & !FILE HANDLE
          IOS,        & !READ ERROR NUMBER
          YEAR,DIV,D,Y  !GENERAL INDICES

!Initialize HDD and CDD variables as zero
HDDADJ(BaseYr:IJUMPCALYR,1:mNumCR) = 0.0
CDDADJ(BaseYr:IJUMPCALYR,1:mNumCR) = 0.0

!OPEN AND READ THE DATA FILE
INFILE=FILE_MGR('O','KDEGDAY',.FALSE.)

READ(INFILE,'(99(/))')                ! SKIP 100 LINE HEADER PER CDM CONVENTION

DO YEAR=BaseYr,IJUMPCALYR
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) HDDYEAR(YEAR), (HDDADJ(YEAR,D),D=1,mNumCR-2)
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) CDDYEAR(YEAR), (CDDADJ(YEAR,D),D=1,mNumCR-2)

  !Create national population-weighted degree days
  DO D=1,mNumCR-2
    HDDADJ(YEAR,NationalPtr) = HDDADJ(YEAR,NationalPtr) + HDDADJ(YEAR,D) * MC_NP(D,YEAR-BaseYr+1)/MC_NP(NationalPtr,YEAR-BaseYr+1)
    CDDADJ(YEAR,NationalPtr) = CDDADJ(YEAR,NationalPtr) + CDDADJ(YEAR,D) * MC_NP(D,YEAR-BaseYr+1)/MC_NP(NationalPtr,YEAR-BaseYr+1)
  ENDDO
ENDDO !YEAR

!If KDEGDAY read was successful, print summary information to unit 6 (nohup.out)
WRITE(6,*) 'RESDMSG SUB_DEGDAYREAD: KDEGDAY.txt data set read successfully'
INFILE=FILE_MGR('C','KDEGDAY',.FALSE.)
RETURN !successful

!READ ERROR OCCURRED
! CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
10 CONTINUE
INFILE=FILE_MGR('C','KDEGDAY',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_DEGDAYREAD: KDEGDAY.txt read error number ',IOS
RETURN

!END OF FILE REACHED BEFORE DATA COMPLETELY READ IN
! CLOSE THE FILE, PRINT ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
95 CONTINUE
INFILE=FILE_MGR('C','KDEGDAY',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_DEGDAYREAD: Error - KDEGDAY.txt EOF reached before all data read in.'
WRITE(6,*) 'RESDMSG SUB_DEGDAYREAD: Error - Correct KDEGDAY.txt and resubmit job.'

RETURN

END SUBROUTINE DEGDAYREAD


!==============================================================================
! READ BASELINEKWH FOR EPA111D ANALYSIS; USED BY RDM AND CDM
!  BLDBASE.TXT
!==============================================================================
SUBROUTINE BLDBASEREAD
IMPLICIT NONE

INTEGER FILE_MGR        !FILE MANAGER
INTEGER*4 INFILE,     & !FILE HANDLE
          IOS,        & !READ ERROR NUMBER
          YEAR,DIV,D,Y  !GENERAL INDICES
REAL TEMP               !PLACEHOLDER TO PULL IN QELCM AND DISCARD

!OPEN AND READ THE DATA FILE
INFILE=FILE_MGR('O','BLDBASE',.FALSE.) ! OPEN THE DEGDAY DATA SET

READ(INFILE,'(99(/))')  !SKIP 100 LINE HEADER PER CDM CONVENTION + SKIP YEARS BEFORE RECS YEAR

!Write baseline electricity consumption to unit 9 (RDM_OUT.txt)
!WRITE(9,*) 'division, divcheck, year, yearcheck, baseline Trills converted to bkWh'

DO D=1,mNumCR-2
  DO y=1,mNumYr
    READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) Div, Year, baselinebkwh(d,y), TEMP !upon read, these data are in Trills
    BASELINEBKWH(d,y)=(BASELINEBKWH(d,y)/3412.)*10**3  !convert quads (trills?) to bkWh
    !WRITE(9,5) D,Div,Y,Year,BASELINEBKWH(D,Y)
  ENDDO
ENDDO
5 FORMAT(' BASELINE CHECK',4I5,F12.5)

!FILE SUCCESSFULLY READ; CLOSE FILE AND RETURN
WRITE(6,*) 'RESDMSG SUB_BLDBASEREAD: BLDBASE.txt data set read successfully'
INFILE=FILE_MGR('C','BLDBASE',.FALSE.)
RETURN

!READ ERROR OCCURRED
! CLOSE THE FILE, PRINT ERROR MESSAGE, AND RETURN
10 CONTINUE
INFILE=FILE_MGR('C','BLDBASE',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_BLDBASEREAD: BLDBASE.txt read error number ',IOS
RETURN

!END OF FILE REACHED BEFORE DATA COMPLETELY READ IN
! CLOSE THE FILE, PRINT ERROR MESSAGE, AND RETURN
95 CONTINUE
INFILE=FILE_MGR('C','BLDBASE',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_BLDBASEREAD: Error - BLDBASE.txt EOF reached before all data read in.'
WRITE(6,*) 'RESDMSG SUB_BLDBASEREAD: Error - Correct BLDBASE.txt and resubmit job.'
RETURN
END SUBROUTINE BLDBASEREAD


!==============================================================================
! READ NEW AND EXISTING HOME SHELL UNIT ENERGY CONSUMPTION DATA
!  RSUECSHL.TXT
!==============================================================================
SUBROUTINE RSUECSHLREAD
IMPLICIT NONE

INTEGER FILE_MGR     !FILE MANAGER
INTEGER*4 INFILE,  & !FILE HANDLE
          IOS,     & !READ ERROR NUMBER
          D,E,B,F    !GENERAL INDICES

!OPEN AND READ THE DATA FILE
INFILE=FILE_MGR('O','RSUECSHL',.FALSE.)

READ(INFILE,'(19(/))')  !SKIP 20 LINE HEADER

DO D=1,mNumCR-2
  DO E=1,nHeatClasses
    READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NEWHEATUEC(E,B,D),B=1,mNumBldg)
  ENDDO
ENDDO

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NEWCOOLUEC(B,D),B=1,mNumBldg)
ENDDO

READ(INFILE,'(1(/))')  !SKIP 2 LINE HEADER

DO E=1,(nHeatClasses+nCoolClasses)  !NoKero
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) BASELOAD(E)
ENDDO

!Initialize existing shell space heating and space cooling shell variables
EHSHELL(RECSYear,1:mNumFuel,1:mNumCR-2,1:mNumBldg) = 1.0
ECSHELL(RECSYear,1:mNumCR-2,1:mNumBldg) = 1.0

INFILE=FILE_MGR('C','RSUECSHL',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSUECSHLREAD: RSUECSHL.txt data set read successfully'
RETURN

!READ ERROR OCCURRED
! CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
10 CONTINUE
INFILE=FILE_MGR('C','RSUECSHL',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSUECSHLREAD: RSUECSHL.txt read error number ',IOS
RETURN

!END OF FILE REACHED BEFORE DATA COMPLETELY READ IN
! CLOSE THE FILE, PRINT ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
95 CONTINUE
INFILE=FILE_MGR('C','RSUECSHL',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSUECSHLREAD: Error - RSUECSHL.txt EOF reached before all data read in.'
WRITE(6,*) 'RESDMSG SUB_RSUECSHLREAD: Error - correct RSUECSHL.txt and resubmit job.'
RETURN

END SUBROUTINE RSUECSHLREAD


!==============================================================================
! READ UEC AND STOCK INDICES FOR MISCELLANEOUS ELECTRIC LOAD (MELs)
!  RSMELS.TXT
!==============================================================================
SUBROUTINE RSMELSREAD
IMPLICIT NONE

INTEGER FILE_MGR     !FILE MANAGER
INTEGER*4 INFILE,  & !FILE HANDLE
          IOS,     & !READ ERROR NUMBER
          B,D,F,Y,YEAR,EQC,E,V,L,I  !GENERAL INDICES

!OPEN AND READ THE DATA FILE
INFILE=FILE_MGR('O','RSMELS',.FALSE.)

READ(INFILE,'(19(/))')  !SKIP 20 LINE HEADER

!Read in counter for number of MELs end uses in RSMELS.txt	!TODO - will be used more once MELs read-in/calculations are streamlined
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) NumMELs

READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER


!*******************************************************************
!READ STOCK INDEXES
!*******************************************************************
!Televisions and related equipment
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(TVSPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(STBPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(HTSPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(OTTPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(VGCPEN(Y), Y=RECSYear+1,ijumpcalyr)

!Computers and related equipment
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DPCPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(LPCPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MONPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NETPEN(Y), Y=RECSYear+1,ijumpcalyr)

!Other specified miscellaneous electric loads
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(BATPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(CFNPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(COFPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DEHPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MCOPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PLPPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PLHPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SECPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SPAPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(WCLPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SPKPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PHNPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(TABPEN(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(KITPEN(Y), Y=RECSYear+1,ijumpcalyr)

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

!*******************************************************************
!READ UNIT ENERGY CONSUMPTION (UEC) INDEXES
!*******************************************************************
!Televisions and related equipment
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(TVSEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(STBEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(HTSEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(OTTEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(VGCEFF(Y), Y=RECSYear+1,ijumpcalyr)

!Computers and related equipment
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DPCEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(LPCEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MONEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NETEFF(Y), Y=RECSYear+1,ijumpcalyr)

!Other specified miscellaneous electric loads
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(BATEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(CFNEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(COFEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DEHEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MCOEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PLPEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PLHEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SECEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SPAEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(WCLEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SPKEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PHNEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(TABEFF(Y), Y=RECSYear+1,ijumpcalyr)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(KITEFF(Y), Y=RECSYear+1,ijumpcalyr)

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

!Read in switch to apply/not apply income effect to each of the specific MELs (1=yes, 0=no)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MELsIncomeEffect(I), I=1,NumMELs)

INFILE=FILE_MGR('C','RSMELS',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSMELSREAD: RSMELS.txt data set read successfully'
RETURN

!READ ERROR OCCURRED
! CLOSE THE FILE, PRINT ERROR MESSAGE, AND RETURN
10 CONTINUE
INFILE=FILE_MGR('C','RSMELS',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSMELSREAD: RSMELS.txt read error number ',IOS
RETURN

!END OF FILE REACHED BEFORE DATA COMPLETELY READ IN
! CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
95 CONTINUE
INFILE=FILE_MGR('C','RSMELS',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSMELSREAD: Error - RSMELS.txt EOF reached before all data read in.'
WRITE(6,*) 'RESDMSG SUB_RSMELSREAD: Error - correct RSMELS.txt and resubmit job.'
RETURN

END SUBROUTINE RSMELSREAD


!==============================================================================
! READ MISCELLANEOUS RESIDENTIAL INPUTS
!  RSMISC.TXT
!==============================================================================
SUBROUTINE RSMISCREAD
IMPLICIT NONE

!DECLARE LOCAL VARIABLES
INTEGER FILE_MGR                    ! FILE MANAGER
INTEGER*4 INFILE,                 & ! FILE HANDLE
          IOS,                    & ! READ ERROR NUMBER
          B,D,F,Y,YEAR,EQC,E,V,L, & ! GENERAL INDICES
          DummyNum,NUMCL,EU,RECCL,RECCLSW,BIN,Y1
CHARACTER*3 DummyTxt
REAL*4 ELfactor, NGfactor, DSfactor, LGfactor
INTEGER*2 modyear, endmodyear, s

!OPEN AND READ THE DATA FILE
INFILE=FILE_MGR('O','RSMISC',.FALSE.) ! OPEN THE RSMISC DATA SET

READ(INFILE,'(19(/))')  !SKIP 20 LINE HEADER

READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(HDR(B),B=1,mNumBldg)

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)HDRfy,HDRly

IF (HDRfy.LE.BaseYr) THEN
  HDRfy=BaseYr+1 !Because HDR calculations require a previous year (i.e., Y-1), the first year must be later than the base year
  WRITE(9,'("Warning: HDRfy modified from RSMISC.txt value")')
ENDIF

IF (HDRly.GT.EndYr) THEN
  HDRly=EndYr !Prevents last year from being outside of range of available housing stock/start data from MAM
  WRITE(9,'("Warning: HDRly modified from RSMISC.txt value")')
ENDIF

!Write input HDR years to unit 9 (RDM_OUT.txt) to compare with input years
WRITE(9,'(a)') 'Housing demolition/decay rate (HDR) years to compare with RSMISC.txt'
WRITE(9,'("HDRfy ",i4)') HDRfy
WRITE(9,'("HDRly ",i4)') HDRly

!Write input HDRs to unit 9 (RDM_OUT.txt) to compare with calculated HDRs
WRITE(9,'(a)') 'Input housing demolition/decay rates (HDR) from RSMISC.txt'
WRITE(9,'("HDR_input_SF ",f6.4)') HDR(1)
WRITE(9,'("HDR_input_MF ",f6.4)') HDR(2)
WRITE(9,'("HDR_input_MH ",f6.4)') HDR(3)

!For standalone RDM runs, use HDR values as input into RSMISC.txt (above); otherwise, overwrite with calculated values based on integrated NEMS Macroeconomic Activity Module (MAM) outputs for housing starts/stocks
!!!If input HDR values differ from integrated run-calculated values (below), RDM outputs will differ between standalone and integrated test runs!!!

  !MAM VARIABLES (FOR REFERENCE):
  !MC_HUSPS1(D,CurIYr) !single-family (B=1) housing starts (millions of housing units)
  !MC_HUSPS2A(D,CurIYr) !multifamily (B=2) housing starts (millions of housing units)
  !MC_HUSMFG(D,CurIYr) !mobile home (B=3) shipments (millions of housing units)

  !MC_KHUPS1(D,CurIYr) !single-family (B=1) housing stocks (millions of housing units)
  !MC_KHUPS2A(D,CurIYr) !multifamily (B=2) housing stocks (millions of housing units)
  !MC_KHUMFG(D,CurIYr) !mobile home (B=3) stocks (millions of housing units)

IF (EXM.EQ.1) THEN !calculate HDR endogenously
  !Initialize variables (especially important for aggregated values used for averages)
  HDR(1:mNumBldg)=0.0
  HDQ((BaseYr-BaseYr+2):(EndYr-BaseYr+1),1:mNumCR,1:mNumBldg)=0.0
  HDi((BaseYr-BaseYr+2):(EndYr-BaseYr+1),1:mNumCR,1:mNumBldg)=0.0
  HDiAve(1:mNumCR,1:mNumBldg)=0.0
  HCDshr((BaseYr-BaseYr+2):(EndYr-BaseYr+1),1:mNumCR,1:mNumBldg)=0.0
  HCDshrAve(1:mNumCR,1:mNumBldg)=0.0

  DO Y=(HDRfy-BaseYr+1),(HDRly-BaseYr+1) !Convert calendar years to index years
    DO D=1,mNumCR-2
      !Number of households demolished in a given year (millions of housing units)
      HDQ(Y,D,1)=MC_KHUPS1(D,Y-1)-MC_KHUPS1(D,Y)+MC_HUSPS1(D,Y-1)
      HDQ(Y,D,2)=MC_KHUPS2A(D,Y-1)-MC_KHUPS2A(D,Y)+MC_HUSPS2A(D,Y-1)
      HDQ(Y,D,3)=MC_KHUMFG(D,Y-1)-MC_KHUMFG(D,Y)+MC_HUSMFG(D,Y-1)
      IF (PRTDBGR.EQ.1) THEN
        !Write MAM housing stocks/starts to unit 9 (RDM_OUT.txt) to verify
        WRITE(9,*) 'SF_stock,',Y+BaseYr-1,',',D,',',MC_KHUPS1(D,Y),',',Y+BaseYr-2,',',MC_KHUPS1(D,Y-1)
        WRITE(9,*) 'SF_starts,',Y+BaseYr-1,',',D,',',MC_HUSPS1(D,Y)
        WRITE(9,*) 'MF_stock,',Y+BaseYr-1,',',D,',',MC_KHUPS2A(D,Y),',',Y+BaseYr-2,',',MC_KHUPS2A(D,Y-1)
        WRITE(9,*) 'MF_starts,',Y+BaseYr-1,',',D,',',MC_HUSPS2A(D,Y)
        WRITE(9,*) 'MH_stock,',Y+BaseYr-1,',',D,',',MC_KHUMFG(D,Y),',',Y+BaseYr-2,',',MC_KHUMFG(D,Y-1)
        WRITE(9,*) 'MH_ships,',Y+BaseYr-1,',',D,',',MC_HUSMFG(D,Y)
        WRITE(9,*) 'HDQ,',Y+BaseYr-1,',',D,',',HDQ(Y,D,1),',',HDQ(Y,D,2),',',HDQ(Y,D,3)
      ENDIF

      !Annual index of households demoslished
      HDi(Y,D,1)=1-(HDQ(Y,D,1)/MC_KHUPS1(D,Y))
      HDi(Y,D,2)=1-(HDQ(Y,D,2)/MC_KHUPS2A(D,Y))
      HDi(Y,D,3)=1-(HDQ(Y,D,3)/MC_KHUMFG(D,Y))
      IF (PRTDBGR.EQ.1) THEN
        !Write to unit 9 (RDM_OUT.txt) to verify
        WRITE(9,*) 'HDi,',Y+BaseYr-1,',',D,',',HDi(Y,D,1),',',HDi(Y,D,2),',',HDi(Y,D,3)
      ENDIF

      !Aggregate annual index of households demoslished to create an average value
      HDiAve(D,1)=HDiAve(D,1)+HDi(Y,D,1)
      HDiAve(D,2)=HDiAve(D,2)+HDi(Y,D,2)
      HDiAve(D,3)=HDiAve(D,3)+HDi(Y,D,3)
      IF (PRTDBGR.EQ.1) THEN
        !Write to unit 9 (RDM_OUT.txt) to verify
        IF (Y.EQ.HDRly-BaseYr+1) THEN
          WRITE(9,*) 'HDiAveTot,',Y+BaseYr-1,',',D,',',HDiAve(D,1),',',HDiAve(D,2),',',HDiAve(D,3)
        ENDIF
      ENDIF

      IF (PRTDBGR.EQ.1) THEN
        !Write to unit 9 (RDM_OUT.txt) to verify
        IF (D.EQ.mNumCR-2) THEN
          WRITE(9,*) 'Total_Housing_Units,',Y+BaseYr-1,',',D,',',MC_KHUPS1(11,Y),',',MC_KHUPS2A(11,Y),',',MC_KHUMFG(11,Y)
        ENDIF
      ENDIF

      !Census division share of U.S. total households by housing type
      HCDshr(Y,D,1)=MC_KHUPS1(D,Y)/MC_KHUPS1(11,Y)
      HCDshr(Y,D,2)=MC_KHUPS2A(D,Y)/MC_KHUPS2A(11,Y)
      HCDshr(Y,D,3)=MC_KHUMFG(D,Y)/MC_KHUMFG(11,Y)
      IF (PRTDBGR.EQ.1) THEN
        !Write to unit 9 (RDM_OUT.txt) to verify
        WRITE(9,*) 'HCDshr,',Y+BaseYr-1,',',D,',',HCDshr(Y,D,1),',',HCDshr(Y,D,2),',',HCDshr(Y,D,3)
      ENDIF

      !Aggregate census division share of U.S. total households by housing type to create an average value
      HCDshrAve(D,1)=HCDshrAve(D,1)+HCDshr(Y,D,1)
      HCDshrAve(D,2)=HCDshrAve(D,2)+HCDshr(Y,D,2)
      HCDshrAve(D,3)=HCDshrAve(D,3)+HCDshr(Y,D,3)
      IF (PRTDBGR.EQ.1) THEN
        !Write to unit 9 (RDM_OUT.txt) to verify
        IF (Y.EQ.HDRly-BaseYr+1) THEN
          WRITE(9,*) 'HCDshrAveTot,',Y+BaseYr-1,',',D,',',HCDshrAve(D,1),',',HCDshrAve(D,2),',',HCDshrAve(D,3)
        ENDIF
      ENDIF
    ENDDO !D
  ENDDO !Y

  DO D=1,mNumCR-2
    !Average index of households demolished over specified period
    HDiAve(D,1)=HDiAve(D,1)/(HDRly-HDRfy+1)
    HDiAve(D,2)=HDiAve(D,2)/(HDRly-HDRfy+1)
    HDiAve(D,3)=HDiAve(D,3)/(HDRly-HDRfy+1)
    IF (PRTDBGR.EQ.1) THEN
      !Write to unit 9 (RDM_OUT.txt) to verify
      WRITE(9,*) 'HDiAve,',D,',',HDiAve(D,1),',',HDiAve(D,2),',',HDiAve(D,3)
    ENDIF

    !Average census division shares of U.S. total households by housing type over specified period
    HCDshrAve(D,1)=HCDshrAve(D,1)/(HDRly-HDRfy+1)
    HCDshrAve(D,2)=HCDshrAve(D,2)/(HDRly-HDRfy+1)
    HCDshrAve(D,3)=HCDshrAve(D,3)/(HDRly-HDRfy+1)
    IF (PRTDBGR.EQ.1) THEN
      !Write to unit 9 (RDM_OUT.txt) to verify
      WRITE(9,*) 'HCDshrAve,',D,',',HCDshrAve(D,1),',',HCDshrAve(D,2),',',HCDshrAve(D,3)
    ENDIF
  ENDDO !D

  DO B=1,mNumBldg
    DO D=1,mNumCR-2
      HDR(B)=HDR(B)+(HDiAve(D,B)*HCDshrAve(D,B))
    ENDDO !D
  ENDDO !B

  !Write calculated HDRs to unit 9 (RDM_OUT.txt) to compare with input HDRs
  !!!If input HDR values in RSMISC.txt differ from integrated run-calculated values, RDM outputs will differ between standalone and integrated test runs!!!
  !!!Can use these values written to RDM_OUT.txt as inputs to RSMISC.txt for standalone testing, though values may change in side cases (e.g., economic growth cases or oil/fuel prices affecting mobile home shipments)!!!!
  WRITE(9,'(a)') 'Calculated housing demolition/decay rates (HDR) for integrated runs'
  WRITE(9,'("HDR_calculated_SF ",f6.4)') HDR(1)
  WRITE(9,'("HDR_calculated_MF ",f6.4)') HDR(2)
  WRITE(9,'("HDR_calculated_MH ",f6.4)') HDR(3)

  DO B=1,mNumBldg
    IF (HDR(B).GT.1.0) THEN
      HDR(B)=1.0	!TODO - should 1.0 max be reconsidered? Set to value less than 1.0 (0.9999?) to ensure some removal of housing stock?
      !Write warning to both NOHUP.OUT (unit 6) and RDM_OUT.txt (unit 9) that HDR has been overwritten
      !HDR=1.0 means that housing stock does not decrease (no demolitions, or conversions into residential housing units exceed demolitions)
      WRITE(6,'("Warning: calculated residential HDR value greater than 1.0000; overwritten to 1.0000 for housing type ",i2)') B
      WRITE(9,'("Warning: calculated residential HDR value greater than 1.0000; overwritten to 1.0000 for housing type ",i2)') B
    ENDIF
  ENDDO !B
ENDIF !calculate HDR endogenously

READ(INFILE,'(1(/))')  !SKIP 2 LINE HEADER

READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(ResDiscountRate)

READ(INFILE,'(1(/))')  !SKIP 2 LINE HEADER

READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(Tenure)

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(EH(RECSYear,B,D),B=1,mNumBldg)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(RACSAT(B,D),B=1,mNumBldg)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(RACUnits(B,D),B=1,mNumBldg)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(CACSAT(B,D),B=1,mNumBldg)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,CACPR(D)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(DWPR(B,D),B=1,mNumBldg)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(ELDRYPR(B,D),B=1,mNumBldg)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)TCW_SHR,FCW_SHR

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)TMF_SHR,SMF_SHR,BMF_SHR

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)CH_SHR,UP_SHR

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(FRZSAT(B,D),B=1,mNumBldg)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(REFSAT(B,D),B=1,mNumBldg)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(DISHNEW(RECSYear+1,B,D), B=1,mNumBldg)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(WASHNEW(RECSYear+1,B,D), B=1,mNumBldg)
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO E=1,nClDryClasses
  DO D=1,mNumCR-2
    READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,DummyTxt,(NEWDRYSAT(RECSYear+1,E,B,D),B=1,mNumBldg)
  ENDDO
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO B=1,mNumBldg
  DO D=1,mNumCR-2
    READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,DummyTxt,(SHTSHR(B,D,F),F=1,5) !NG, ELEC, DFO/KER, PROPANE, WOOD  !NoKero	!TODO - replace 5 with NSHTRFL? would need to be declared in different module/subroutine
  ENDDO
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO B=1,mNumBldg
  DO D=1,mNumCR-2
    READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,DummyTxt,(NSHTSHR(B,D,F),F=1,5) !NG, ELEC, DFO/KER, PROPANE, WOOD  !NoKero	!TODO - replace 5 with NSHTRFL? would need to be declared in different module/subroutine
  ENDDO
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO F=1,2  !Heating = 1; Cooling = 2
  DO B=1,mNumBldg
    DO D=1,mNumCR-2
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyTxt,DummyTxt,DummyNum,(WTHRZTN(Y,F,D,B),Y=RECSyear,EndYr)
    ENDDO
  ENDDO
ENDDO

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

DO D=1,mNumCR-2
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(ELASTIC(S,D),S=1,5) !FOSSIL HEATING, ELECTRIC HEATING, CENTRAL AC, HEAT PUMP AC, FURNACE FAN
ENDDO

!Overwrite prices in the common block for elasticity runs
READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER

READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) &
 MODYEAR, EndModYear, ELFACTOR, NGFACTOR, DSFACTOR, LGFACTOR

! NOTE FOR ELECTRICITY, PRICES ARE BY END USE AS FOLLOWS
!    PELRSOUT(...,EU)      RTEK EU #
!    1 = Space Heating     1
!    2 = Space Cooling     2
!    3 = Water Heating     5
!    4 = Cooking           6
!    5 = Clothes Drying    7; also 3 (clothes washing)
!    6 = Refrigeration     8
!    7 = Freezing          9
!    8 = Lighting          not in RTEK
!    9 = Appliances/Other  4 Dishwashers; 14 Other electric appliances
!    10= Secondary Space Heating not in RTEK
!    11= Electric Vehicle Charging  not in RTEK

DO y=ModYear,EndModYear
  DO d= 1, mNumCR-2
    PELRS (d,y) =  PELRS (d,y) * ELFACTOR
    DO s=1,10	!TODO - S=1-10 for EMM end-use services; replace 10 with parameter (MNEURSGRP-1?)
      PELRSOUT(d,y,s)=PELRSOUT(d,y,s)*ELFACTOR
    ENDDO !s
    PNGRS (d,y) =  PNGRS (d,y) * NGfactor
    PDSRS (d,y) =  PDSRS (d,y) * DSfactor
    PLGRS (d,y) =  PLGRS (d,y) * LGfactor
  ENDDO  !divisions
ENDDO  !years

!READ SUCCESSFUL; CLOSE THE FILE
INFILE=FILE_MGR('C','RSMISC',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSMISCREAD: RSMISC.txt data set read successfully'

DO Y=RECSYear+1,EndYr
  DO B=1,mNumBldg
    DO D=1,mNumCR-2
      EH(Y,B,D)=((EH(Y-1,B,D)*HDR(B)))
    ENDDO  !D
  ENDDO  !B
ENDDO  !Y

OLDHSES(RECSYear)=0.0
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    OLDHSES(RECSYear)=OLDHSES(RECSYear)+EH(RECSYear,B,D)
  ENDDO  !B
ENDDO  !D

LIMIT=0.30  !Maximum shell efficiency index of 0.3 (i.e., maximum shell efficiency is limited to a 70% improvement on the base-year value); applies to existing, new, heating, cooling	!TODO - Update?

DO Y1=RECSYear+1,LastYr+BaseYr-1  !represents the annual increase in existing shell efficiency due to technology improvements by census division(?); subtracted from existing shell index	!TODO - identify source
  DO B=1,mNumBldg
    TECHG(Y1,1,B)=TECHG(Y1-1,1,B)+0.005
    TECHG(Y1,2,B)=TECHG(Y1-1,2,B)+0.003
    TECHG(Y1,3,B)=TECHG(Y1-1,3,B)+0.006
    TECHG(Y1,4,B)=TECHG(Y1-1,4,B)+0.003
    TECHG(Y1,5,B)=TECHG(Y1-1,5,B)+0.006
    TECHG(Y1,6,B)=TECHG(Y1-1,6,B)+0.016
    TECHG(Y1,7,B)=TECHG(Y1-1,7,B)+0.003
    TECHG(Y1,8,B)=TECHG(Y1-1,8,B)+0.003
    TECHG(Y1,9,B)=TECHG(Y1-1,9,B)+0.011
  ENDDO
ENDDO

RETURN  !Return if successful

!READ ERROR OCCURRED
! CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
10 CONTINUE
INFILE=FILE_MGR('C','RSMISC',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSMISCREAD: RSMISC.txt read error number ',IOS
RETURN

!END OF FILE REACHED BEFORE DATA COMPLETELY READ IN
! CLOSE THE FILE, PRINT ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
95 CONTINUE
INFILE=FILE_MGR('C','RSMISC',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSMISCREAD: Error - RSMISC.txt EOF reached before all data read in.'
WRITE(6,*) 'RESDMSG SUB_RSMISCREAD: Error - correct RSMISC.txt and resubmit job.'
RETURN

END SUBROUTINE RSMISCREAD


!==============================================================================
! READ RESIDENTIAL LIGHTING TECHNOLOGY AND USAGE DATA
!  RSMLGT.TXT
!==============================================================================
SUBROUTINE RSMLGTREAD
IMPLICIT NONE

!DECLARE LOCAL VARIABLES
INTEGER FILE_MGR              ! FILE MANAGER
INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,              & ! READ ERROR NUMBER
          D,B,E,BIN,Y1,I,app

INFILE=FILE_MGR('O','RSMLGT',.FALSE.) ! OPEN THE RSMLGT DATA SET

READ(INFILE,'(19(/))')  !SKIP 20 LINE HEADER

!CONTROL VARIABLES

!Switch for writing lighting diagnostic outputs/ testing read-in
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) LightDiag

!Dollar year for lighting technology costs
READ(INFILE,'((/))')  !SKIP 2 LINE HEADER
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) RLGTDOLLARYR
IF (LightDiag .NE. 0) WRITE(9,*) 'Read of Lighting Data and Test Print '
IF (LightDiag .NE. 0) WRITE(9,*) 'RLGTDOLLARYR ', RLGTDOLLARYR

!Number of lighting application types (currently 4 types: general service lamps, reflectors, linear fluorescent, and exterior)
READ(INFILE,'((/))')  !SKIP 2 LINE HEADER
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) NumApps
IF (LightDiag .NE. 0) WRITE(9,*) 'Number of Lighting Apps (NumApps) ', NumApps

!Application IDs (three-character codes and order of index which map to the lighting technology data later)
READ(INFILE,'((/))')  !SKIP 2 LINE HEADER
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (AppID(i),i=1,NumApps)
IF (LightDiag .NE. 0) WRITE(9,'(5a5)') (AppID(i),i=1,NumApps)
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (AppIndex(i),i=1,NumApps)
IF (LightDiag .NE. 0) WRITE(9,'(5i5)') (AppIndex(i),i=1,NumApps)

!Number of bulbs per application types
READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (NumTypes(i),i=1,NumApps)
IF (LightDiag .NE. 0) WRITE(9,'(5i5)') (NumTypes(i),i=1,NumApps)

!Number of bins (i.e., hours used) by application
READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (NumAppBins(i),i=1,NumApps)
IF (LightDiag .NE. 0) WRITE(9,'(5i5)') (NumAppBins(i),i=1,NumApps)

READ(INFILE,'(3(/))')  !SKIP 4 LINE HEADER

!Technology Data - read until a first year of 9999 is found
DO I=1,NLRec
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) FirstYear(i),LastYear(i),BulbCost(i),(BulbEESub(i,d),d=1,mNumCR-2),(BulbSub(i,d),d=1,mNumCR-2),LPW(i), &
   BulbWatts(i),LifeHours(i),BulbCRI(i),LightingApp(i),BulbType(i),LTLBeta1(i),LTLBeta2(i)
  IF (LightDiag .NE. 0) WRITE(9,3)FirstYear(i),LastYear(i),BulbCost(i),(BulbEESub(i,d),d=1,mNumCR-2),(BulbSub(i,d),d=1,mNumCR-2),LPW(i), &
   BulbWatts(i),LifeHours(i),BulbCRI(i),LightingApp(i),BulbType(i),LTLBeta1(i),LTLBeta2(i)
  ! Convert Lighting Equipment Costs to RTEKDOLLARYR (consistent with prices)
  BulbCost(i)=BulbCost(i)*MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(RLGTDOLLARYR-BaseYr+1)
  IF (FirstYear(i).EQ.9999) EXIT
ENDDO
3 FORMAT(2i6,19f8.2,4f7.1,2a5,2f7.1)

READ(INFILE,'(2(/))')  !SKIP 3 LINE HEADER
DO app=1,NumApps
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BulbsPerHH(app,B),B=1,3)
  IF (LightDiag .NE. 0) WRITE(9,'("BulbsPerHH",i5,3f8.2)') app,(BulbsPerHH(app,B),B=1,3)
  READ(INFILE,'(1x)')
ENDDO

DO app=1,NumApps !maximum 5 lighting applications, currently 4
  READ(INFILE,'((/))')  !SKIP 2 LINE HEADER
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (AppBinHours(app,BIN),BIN=1,NumAppBins(app))
  IF (LightDiag .NE. 0) WRITE(9,*) 'APPBINHOURS'
  IF (LightDiag .NE. 0) WRITE(9,'(6f10.0)') (AppBinHours(app,BIN),BIN=1,NumAppBins(app))

  !General service bin shares each of 3 lighting types by 6 bins
  READ(INFILE,'(1x)')
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BinShares(app,BIN),BIN=1,NumAppBins(app))
  IF (LightDiag .NE. 0) WRITE(9,*) 'LTBINSHARES'
  IF (LightDiag .NE. 0) WRITE(9,'(i4,6f10.2)') app,(BinShares(app,BIN),BIN=1,NumAppBins(app))

  READ(INFILE,'(1x)')
  IF (LightDiag .NE. 0) WRITE(9,*) 'app, type, BULBBINSHARES (by type)'
  DO e=1,NumTypes(app)
    READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BulbBinShares(app,e,BIN),BIN=1,NumAppBins(app))
    IF (LightDiag .NE. 0) WRITE(9,'(2I4,6f10.1)') app,e,(BulbBinSHAREs(app,e,BIN),BIN=1,NumAppBins(app))
  ENDDO !e

  READ(INFILE,'(1x)')
  READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BaseWattsBulbs(app,e),e=1,NumTypes(app))
  IF (LightDiag .NE. 0) WRITE(9,'(i4,6f10.1)') app,(BaseWattsBulbs(app,e),e=1,NumTypes(app))
  IF (app .NE. NumApps) READ(INFILE,'(1x)')
ENDDO

!Compute RECS-year base watts per bulb weight averaged across bulb types

!Initialize values
DO app=1,NumApps
  DO bin=1,NumAppBins(app)
    basewattbins(app,bin)=0.
  ENDDO
ENDDO

DO app=1,NumApps
  DO bin=1,NumAppBins(app)
    DO e=1,NumTypes(app)
      basewattbins(app,bin)=basewattbins(app,bin) + bulbbinshares(app,e,bin)*basewattsbulbs(app,e)
    ENDDO
  ENDDO
ENDDO

DO app=1,NumApps
  IF (LightDiag .NE. 0) WRITE(9,9) AppID(app),(basewattbins(app,bin),BIN=1,NumAppBins(app))
ENDDO
9 FORMAT(' weighted base watts by bin for app: ',a5, 6f10.2)

WRITE(6,*) 'RESDMSG SUB_RSMLGTREAD: RSMLGT.txt data set read successfully'

INFILE=FILE_MGR('C','RSMLGT',.FALSE.)
RETURN !successful

!READ ERROR OCCURRED
! CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT) AND UNIT 9 (RDM_OUT.txt), AND RETURN
10 CONTINUE
INFILE=FILE_MGR('C','RSMLGT',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSMLGTREAD: RSMLGT.txt read error number ',IOS
RETURN

!END OF FILE REACHED BEFORE DATA COMPLETELY READ IN
! CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT) AND UNIT 9 (RDM_OUT.txt), AND RETURN
95 CONTINUE
INFILE=FILE_MGR('C','RSMLGT',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RSMLGTREAD: Error - RSMLGT.txt EOF reached before all data read in.'
WRITE(6,*) 'RESDMSG SUB_RSMLGTREAD: Error - correct RSMLGT.txt and resubmit job.'
RETURN

END SUBROUTINE RSMLGTREAD


!==============================================================================
! READ RETIRING PERCENTAGES FOR DECAY OF RECS-YEAR EQUIPMENT
!  RSRET01.TXT
!==============================================================================
SUBROUTINE RDRET
IMPLICIT NONE

INTEGER  Y, RECCL
LOGICAL NEW
CHARACTER*18 FNAME
INTEGER FILE_MGR
INTEGER*4 IOS
EXTERNAL FILE_MGR
INTEGER  IUNIT1

FNAME='RSRET01'
NEW=.FALSE.
IUNIT1=FILE_MGR('O',FNAME,NEW)

READ(IUNIT1,'(19(/))')  !SKIP 20 LINE HEADER

READ(IUNIT1, FMT=*, ERR=10, END=95, IOSTAT=IOS) ((EQCRET(Y,RECCL), Y=RECSYear+1,ijumpcalyr), RECCL=1,RTCLCNT)

IUNIT1=FILE_MGR('C',FNAME,NEW)
WRITE(6,*) 'RESDMSG SUB_RDRET: RSRET01.txt data set read successfully'
RETURN

!READ ERROR OCCURRED
! CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
10 CONTINUE
IUNIT1=FILE_MGR('C','FNAME',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RDRET: RSRET01.txt read error number ',IOS
RETURN

!END OF FILE REACHED BEFORE DATA COMPLETELY READ IN
! CLOSE THE FILE, PRINT ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
95 CONTINUE
IUNIT1=FILE_MGR('C','FNAME',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RDRET: Error - RSRET01.txt EOF reached before all data read in.'
WRITE(6,*) 'RESDMSG SUB_RDRET: Error - Correct RSRET01.txt and resubmit job.'
RETURN

END SUBROUTINE RDRET


!==============================================================================
! SPACE HEATING EQUIPMENT MARKET SHARE INITIALIZATION SUBROUTINE
!  From RECSYear to last year of Census SOC (formerly C25) data in RSHTSHR.txt
!==============================================================================
SUBROUTINE INTEQT
IMPLICIT NONE

INCLUDE 'rtek'
EXTERNAL FILE_MGR

LOGICAL NEW
CHARACTER*18 FNAME
INTEGER FILE_MGR, IUNIT1, B, E ,D, Y, EU, RECCL

EU = 1  !space heating

FNAME='RSHTSHR'
NEW=.FALSE.
IUNIT1=FILE_MGR('O',FNAME,NEW)

READ(IUNIT1,'(19(/))')
READ(IUNIT1,*) HTSHRYR

READ(IUNIT1,'(3(/))')

!Shares of new space heating equipmbent by class, housing type, and census division based on U.S. Census Bureau Characteristics of New Construction
DO Y=RECSYear,HTSHRYR
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      READ(IUNIT1,*) (HSYSSHR(Y,RECCL,B,D),B=1,mNumBldg)
    ENDDO
  ENDDO
ENDDO

READ(IUNIT1, '(/)')       ! SKIP LINE

DO B=1,mNumBldg
  DO E=1,nHeatClasses
    READ(IUNIT1,*) (RTFCBIAS(E,B,D),D=1,mNumCR-2)
  ENDDO
ENDDO

IUNIT1=FILE_MGR('C',FNAME,NEW)

END SUBROUTINE INTEQT


!==============================================================================
! FUEL NUMBERING SYSTEMS MAPPING SUBROUTINE
!==============================================================================
SUBROUTINE RCONSFL
IMPLICIT NONE

! NOTE FOR ELECTRICITY, PRICES ARE BY END USE AS FOLLOWS
!    PELRSOUT(...,EU)      RTEK EU #
!    1 = Space Heating     1
!    2 = Space Cooling     2
!    3 = Water Heating     5
!    4 = Cooking           6
!    5 = Clothes Drying    7; also 3 (clothes washing)
!    6 = Refrigeration     8
!    7 = Freezing          9
!    8 = Lighting          not in RTEK
!    9 = Appliances/Other  4 Dishwashers; 14 Other electric appliances
!    10= Secondary Space Heating not in RTEK
!    11= Electric Vehicle Charging  not in RTEK

!    MAP RTEK FUEL NUMBERS INTO HTRCON FUEL NUMBERS  !NoKero
!                   HTRCON    RTEK
!                   FUEL #   FUEL #
!    FUEL NAME       FCON       F(RTFUEL)
!    NATURAL GAS       1        3
!    ELECTRICITY       2        4
!    DFO+KEROSENE      3        1 (DFO=Distillate Fuel Oil)
!    PROPANE           4        2
!    WOOD              5        5 (Priced to distillate fuel oil, 1)  !NoKero

!    MAP RTEK FUEL NUMBERS INTO CLCON FUEL NUMBERS
!                   CLCON     RTEK
!                   FUEL #    FUEL #
!    FUEL NAME       FCON       F
!    ELECTRICITY       1        4
!    NATURAL GAS       2        3	!TODO - If eliminating NG_HP, remove

!Space Heating (EU = 1)
NHTRFL=5 !number of space heating fuels  !NoKero
FHTRCON(1)=3 !natural gas - NG_FA, NG_RAD, NG_HP
FHTRCON(2)=4 !electricity - ELEC_RAD, ELEC_HP, GEO_HP
FHTRCON(3)=1 !distillate fuel oil - DIST_FA, DIST_RAD
FHTRCON(4)=2 !propane - LPG_FA
FHTRCON(5)=5 !wood - WOOD_HT  !NoKero

!Secondary Space Heating (combined with space heating)
NSHTRFL=5 !number of secondary space heating fuels (natural gas, electricity, distillate fuel oil/kerosene, propane, wood)

!Space Cooling (EU = 2)
NCLFL=2 !number of space cooling (air conditioning) fuels	!TODO - If eliminating NG_HP, reduce to 1?
FCLCON(4)=1 !electricity - ROOM_AIR, CENT_AIR, ELEC_HP, GEO_HP
FCLCON(3)=2 !natural gas - NG_HP	!TODO - If eliminating NG_HP, remove

!Clothes Washing (EU = 3)
NCSWFL=1 !number of clothes washer fuels (i.e., electricity)	!TODO - still used?

!Dishwashing (EU = 4)
NDSWFL=1 !number of dishwasher fuels (i.e., electricity)	!TODO - still used?

!Water Heating (EU = 5)
NWHFL=5 !number of water heating fuels
FWHCON(1)=3 !natural gas - NG_WH
FWHCON(2)=4 !electricity - ELEC_WH
FWHCON(3)=1 !distillate fuel oil/kerosene - DIST_WH
FWHCON(4)=2 !propane - LPG_WH
FWHCON(5)=5 !solar - SOLAR_WH  !NoKero	!TODO - should RTFUEL=5 (on right) for solar or =4 for electricity as in RSCLASS?

!Cooking (EU = 6)
NSTVFL=3 !number of cooking fuels
FSTVCON(3)=1 !natural gas - NG_STV
FSTVCON(2)=2 !propane - LPG_STV
FSTVCON(4)=3 !electricity - ELEC_STV

!Clothes Drying (EU = 7)
NDRYFL=2 !number of clothes dryer fuels
FDRYCON(3)=1 !natural gas - NG_DRY
FDRYCON(4)=2 !electricity - ELEC_DRY

!Refrigeration (EU = 8)
NREFFL=1 !number of refrigeration fuels (i.e., electricity)	!TODO - still used?

!Freezing (EU = 9)
NFRZFL=1 !number of standalone freezer fuels (i.e., electricity)	!TODO - still used?

RETURN

END SUBROUTINE RCONSFL


!==============================================================================
! READ AVERAGE SQUARE FOOTAGE OF NEW CONSTRUCTION
!  RSSQFT.TXT
!==============================================================================
SUBROUTINE RDSQFOOT
IMPLICIT NONE

INTEGER D, Y, B,S
LOGICAL NEW
CHARACTER*18 FNAME
INTEGER FILE_MGR
INTEGER*4 IOS
EXTERNAL FILE_MGR
INTEGER  IUNIT1

FNAME='RSSQFT'
NEW=.FALSE.
IUNIT1=FILE_MGR('O',FNAME,NEW)

READ(IUNIT1,'(19(/))')  !SKIP 20 LINE HEADER

READ(IUNIT1, FMT=*, ERR=10, END=95, IOSTAT=IOS) (((SQRFOOT(Y,B,D), Y=RECSYear,ijumpcalyr), D=1,mNumCR-2), B=1,mNumBldg)

IUNIT1=FILE_MGR('C',FNAME,NEW)
WRITE(6,*) 'RESDMSG SUB_RDSQFOOT: RSSQFT.txt data set read successfully'
RETURN

!READ ERROR OCCURRED
! CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
10 CONTINUE
IUNIT1=FILE_MGR('C','FNAME',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RDSQFOOT: RSSQFT.txt read error number ',IOS
RETURN

!END OF FILE REACHED BEFORE DATA COMPLETELY READ IN
! CLOSE THE FILE, PRINT ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
95 CONTINUE
IUNIT1=FILE_MGR('C','FNAME',.FALSE.)
WRITE(6,*) 'RESDMSG SUB_RDSQFOOT: Error - RSSQFT.txt EOF reached before all data read in.'
WRITE(6,*) 'RESDMSG SUB_RDSQFOOT: Error - Correct RSSQFT.txt and resubmit job.'
RETURN

END SUBROUTINE RDSQFOOT


!==============================================================================
! INITIALIZE PRICES FROM NEMS AND INFLATE
!==============================================================================
SUBROUTINE RDPR
IMPLICIT NONE

INTEGER D, Y, Y1

IF (CurCalYr.EQ.RECSYear) THEN
  !MAP ALL PRICES FOR YEARS PRIOR TO RECSYear AND ALL FUTURE YEARS FROM RESTART FILE
  ! NOT IMPLEMENTED, BUT COULD BE USED FOR EXPECTATIONS
  DO D=1,mNumCR-2
    DO Y=1,mNumYr
      Y1=Y+BaseYr-1
      PRICES(1,D,Y1)=PDSRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      PRICES(2,D,Y1)=PLGRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      PRICES(3,D,Y1)=PNGRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      PRICES(4,D,Y1)=PELRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      PRICES(5,D,Y1)=PDSRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2)) !wood priced to distillate fuel oil/kerosene  !NoKero
    ENDDO
  ENDDO
ELSE
  !OVERWRITE PREVIOUS PRICES WITH CURRENT PRICES
  DO D=1,mNumCR-2
    PRICES(1,D,CurCalYr)=PDSRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
    PRICES(2,D,CurCalYr)=PLGRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
    PRICES(3,D,CurCalYr)=PNGRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
    PRICES(4,D,CurCalYr)=PELRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
    !WOOD PRICE (5) IS LINKED TO GAS PRICE LESS ANY CARBON TAX	!TODO - verify why wood was priced differently in projections versus RECSyear (above)
    PRICES(5,D,CurCalYr)=(PNGRS(D,CurIYr)-JNGRS(CurIYr))*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))  !NoKero
    !PRICES(5,D,CurCalYr)=PDSRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2)) !wood priced to distillate fuel oil/kerosene	!TODO - this is consistent with RECSyear above, but not how wood has been priced since AEO2013(?)

    !Check for zero prices and report to unit 6 (nohup.out)
    IF (PRICES(1,D,CurCalYr) .LE. 0.0) THEN
      PRICES(1,D,CurCalYr)=PRICES(1,D,CurCalYr-1)
      WRITE(6,*) 'RESDMSG: Warning - non-positive price for residential distillate fuel oil in census division=',D
    ENDIF
    IF (PRICES(2,D,CurCalYr).LE. 0.0) THEN
      PRICES(2,D,CurCalYr)=PRICES(2,D,CurCalYr-1)
      WRITE(6,*) 'RESDMSG: Warning - non-positive price for residential propane in census division=',D
    ENDIF
    IF (PRICES(3,D,CurCalYr) .LE. 0.0) THEN
      PRICES(3,D,CurCalYr)=PRICES(3,D,CurCalYr-1)
      WRITE(6,*) 'RESDMSG: Warning - non-positive price for residential natural gas in census division=',D
    ENDIF
    IF (PRICES(4,D,CurCalYr) .LE. 0.0) THEN
      PRICES(4,D,CurCalYr)=PRICES(4,D,CurCalYr-1)
      WRITE(6,*) 'RESDMSG: Warning - non-positive price for residential electricity in census division=',D
    ENDIF
    IF (PRICES(5,D,CurCalYr) .LE. 0.0) THEN
      PRICES(5,D,CurCalYr)=PRICES(5,D,CurCalYr-1)
      WRITE(6,*) 'RESDMSG: Warning - non-positive price for residential wood (priced to distillate fuel oil) in census division=',D  !NoKero
    ENDIF
  ENDDO
ENDIF

END SUBROUTINE RDPR


!==============================================================================
! READ RECS-YEAR VINTAGE EQUIPMENT SUBROUTINE
!  RSSTK.TXT
!  (Refer to RSCLASS tab of RSMESS.xlsx to see the numbers that correspond to equipment classes)
!==============================================================================
SUBROUTINE RDHTREQC
IMPLICIT NONE

LOGICAL NEW
CHARACTER*18 FNAME
INTEGER FILE_MGR
EXTERNAL FILE_MGR
INTEGER  B, D, Y, EU, RECCL, IUNIT1,EQC,RECCLHP,E

FNAME='RSSTK'
NEW=.FALSE.
IUNIT1=FILE_MGR('O',FNAME,NEW)

READ(IUNIT1,'(19(/))')

!All major end-uses
DO EU = 1, 9	!TODO - replace 9 with parameter?
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    DO D=1,mNumCR-2
      READ(IUNIT1,*) (EQCESE(RECSYear,RECCL,B,D), B=1,mNumBldg)
    ENDDO
  ENDDO
ENDDO

!Miscellaneous Electric Loads (MELs; no end use number)
READ(IUNIT1, *) ((FANEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! furnace fans
READ(IUNIT1, *) ((TVSEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! televisions
READ(IUNIT1, *) ((STBEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! set-top boxes
READ(IUNIT1, *) ((HTSEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! home theater systems
READ(IUNIT1, *) ((OTTEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! over-the-top (OTT) streaming devices
READ(IUNIT1, *) ((VGCEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! video game consoles
READ(IUNIT1, *) ((DPCEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! desktop personal computers
READ(IUNIT1, *) ((LPCEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! laptop personal computers
READ(IUNIT1, *) ((MONEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! computer monitors
READ(IUNIT1, *) ((NETEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! computer networking equipment
READ(IUNIT1, *) ((BATEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! rechargeable devices
READ(IUNIT1, *) ((CFNEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! ceiling fans
READ(IUNIT1, *) ((COFEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! coffee makers
READ(IUNIT1, *) ((DEHEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! dehumidifiers
READ(IUNIT1, *) ((MCOEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! microwave ovens
READ(IUNIT1, *) ((PLPEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! pool pumps
READ(IUNIT1, *) ((PLHEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! pool heaters
READ(IUNIT1, *) ((SECEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! home security systems
READ(IUNIT1, *) ((SPAEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! portable electric spas
READ(IUNIT1, *) ((WCLEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! wine coolers
READ(IUNIT1, *) ((SPKEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! smart speakers
READ(IUNIT1, *) ((PHNEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! smartphones
READ(IUNIT1, *) ((TABEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! tablets
READ(IUNIT1, *) ((KITEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! small kitchen appliances
READ(IUNIT1, *) ((EAEQP(RECSYear,B,D), B = 1, mNumBldg), D = 1, mNumCR - 2) ! electric other equipment

!Secondary space heating equipment (natural gas, electricity, propane, distillate fuel oil, wood)
READ(IUNIT1, *) (((SHTEQP(RECSYear,B,D,E), B = 1, mNumBldg), D = 1, mNumCR - 2), E = 1, 5)  !NoKero	!TODO - replace 5 with NSHTRFL? would need to be declared in different module/subroutine

!Other appliances (natural gas, propane, distillate fuel oil)
READ(IUNIT1, *) (((APPEQP(RECSYear,B,D,E), B = 1, mNumBldg), D = 1, mNumCR - 2), E = 1, 3)	!TODO - replace 3 with parameter?

!****************************************************************************************
!  CALCULATE TOTAL WATER HEATERS IN RDM BASE YEAR TO SHARE OUT CLOTHES WASHERS AND DISWASHERS
!****************************************************************************************
EU=5
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    HOTWATQ(RECSYear,B,D)=0.0
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      HOTWATQ(RECSYear,B,D)=HOTWATQ(RECSYear,B,D)+EQCESE(RECSYear,RECCL,B,D)
    ENDDO
  ENDDO
ENDDO

!****************************************************************************************
! PROJECT EXISTING EQUIPMENT BY APPLYING EQUIPMENT RETIREMENT/REPLACEMENT RATE
!****************************************************************************************
DO EU = 1, 9	!TODO - replace 9 with parameter?
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      DO B=1,mNumBldg
        DO Y=RECSYear+1,EndYr
          !Test for space cooling heat pumps so they align with space heating equipment
          EQCESE(Y,RECCL,B,D)=(EQCESE(RECSYear,RECCL,B,D)*(HDR(B)**(Y-RECSYear))*(1.0-EQCRET(Y,RECCL)))
          IF (EU.EQ.2) THEN
            IF (RTCLPNTR(RECCL).NE.0) THEN
              RECCLHP=RTCLPNTR(RECCL)
              EQCESE(Y,RECCL,B,D)=EQCESE(Y,RECCLHP,B,D)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

IUNIT1=FILE_MGR('C',FNAME,NEW)
END SUBROUTINE RDHTREQC


!==============================================================================
! READ RETIRING EQUIPMENT EFFICIENCIES
!  RSEFF01.TXT
!==============================================================================
SUBROUTINE RDEFF
!IMPLICIT NONE	!TODO - this was missing?

INTEGER  Y, RECCL
LOGICAL NEW
CHARACTER*18 FNAME
INTEGER FILE_MGR
EXTERNAL FILE_MGR
INTEGER  IUNIT1

FNAME='RSEFF01'
NEW=.FALSE.
IUNIT1=FILE_MGR('O',FNAME,NEW)
READ(IUNIT1,'(19(/))')
DO RECCL=1,RTCLCNT
  READ(IUNIT1,*) (EQCEFF(Y,RECCL), Y=RECSYear+1,ijumpcalyr)
ENDDO
IUNIT1=FILE_MGR('C',FNAME,NEW)

END SUBROUTINE RDEFF


!==============================================================================
! READ RECS-YEAR STOCK EFFICIENCIES
!  RSSTKEFF.TXT
!==============================================================================
SUBROUTINE RDSTEFF
!IMPLICIT NONE	!TODO - this was missing?

INTEGER  Y, RECCL
LOGICAL NEW
CHARACTER*18 FNAME
INTEGER FILE_MGR
EXTERNAL FILE_MGR
INTEGER  IUNIT1

FNAME='RSSTKEFF'
NEW=.FALSE.
IUNIT1=FILE_MGR('O',FNAME,NEW)
READ(IUNIT1,'(19(/))')
DO RECCL=1,RTCLCNT
  READ(IUNIT1,*) (STKEFF(Y,RECCL), Y=RECSYear,ijumpcalyr)
ENDDO
IUNIT1=FILE_MGR('C',FNAME,NEW)

END SUBROUTINE RDSTEFF


!==============================================================================
! READ ENERGY STAR LEARNING FACTORS AND SHELL BETAS TO BENCHMARK ENERGY STAR HOME SHARES
!  RSESTAR.TXT
!==============================================================================
SUBROUTINE RDESTARHOMES
IMPLICIT NONE

INTEGER  B, D, E, S, Y
!Input variables declared in RTEK includes file
LOGICAL NEW
CHARACTER*18 FNAME
INTEGER FILE_MGR
EXTERNAL FILE_MGR
INTEGER IUNIT1

FNAME='RSESTAR'
NEW=.FALSE.
IUNIT1=FILE_MGR('O',FNAME,NEW)
READ(IUNIT1,'(19(/))')  !SKIP 20-LINE HEADER
READ(IUNIT1,*) ESTARHISTYR
!WRITE(9,*)  'ESTARHISTYR', ESTARHISTYR

ALLOCATE(HVBETA1(RECSYear+1:ESTARHISTYR,mNumBldg,nShellTypes,mNumCR))
ALLOCATE(HVBETA2(RECSYear+1:ESTARHISTYR,mNumBldg,nShellTypes,mNumCR))
HVBETA1=0.0
HVBETA2=0.0

READ(IUNIT1,'(//)')  !SKIP 3-LINE HEADER
DO B=1,mNumBldg
  DO S=1,nShellTypes
    DO D=1,mNumCR-2
      READ(IUNIT1,*) (HVBETA1(Y,B,S,D), Y=RECSYear+1,ESTARHISTYR)
    ENDDO !D loop for census division
  ENDDO !S loop for building shell levels
ENDDO !B loop for building type

READ(IUNIT1,'(//)')  !SKIP 3-LINE HEADER
DO B=1,mNumBldg
  DO S=1,nShellTypes
    DO D=1,mNumCR-2
      READ(IUNIT1,*) (HVBETA2(Y,B,S,D), Y=RECSYear+1,ESTARHISTYR)
    ENDDO !D loop for census division
  ENDDO !S loop for building shell levels
ENDDO !B loop for building type

READ(IUNIT1,'(//)')  !SKIP 3-LINE HEADER
DO D=1,mNumCR-2
  READ(IUNIT1,*)(LEARNFACT(B,D),B=1,mNumBldg)
ENDDO

IUNIT1=FILE_MGR('C',FNAME,NEW)

END SUBROUTINE RDESTARHOMES


!==============================================================================
! READ INITIAL BASE-YEAR UNIT ENERGY CONSUMPTIONS (UECs)
!  RSUEC.TXT
!  (Refer to RSCLASS tab of RSMESS.xlsx to see the numbers that correspond to equipment classes)
!==============================================================================
SUBROUTINE RDUECS
IMPLICIT NONE

INTEGER B, D, E, F, EU, RECCL, RECCL1
LOGICAL NEW
CHARACTER*18 FNAME
INTEGER FILE_MGR
EXTERNAL FILE_MGR
INTEGER IUNIT1

FNAME='RSUEC'
NEW=.FALSE.
IUNIT1=FILE_MGR('O',FNAME,NEW)
READ(IUNIT1,'(19(/))')

!*******************************************************************
!    RECCL          = RECORD NUMBER IN RSCLASS
!    RTCLEUPT(EU)   = LAST RECORD # IN SPACE HEATING (EU=1)
!    RTCLEUPT(EU+1) = LAST RECORD # IN SPACE COOLING (EU=2)
!    EQC            = EQUIPMENT CLASS # FOR COOLING
!*******************************************************************

!All major end-uses
DO EU = 1, 9	!TODO - replace 9 with parameter?
  DO RECCL = RTCLEUPT(EU) + 1, RTCLEUPT(EU + 1)
    DO D = 1, mNumCR - 2
      READ(IUNIT1, *) (EQCUEC(D, RECCL, B), B = 1, mNumBldg)
    ENDDO
  ENDDO
ENDDO

!For electric-only end uses:
! clothes washing (EU=3), dishwashing (EU=4), refrigeration (EU=8), and freezing (EU=9)
DO EU = 1, 9	!TODO - replace 9 with parameter?
  IF (EU == 3 .OR. EU == 4 .OR. EU == 8 .OR. EU == 9) THEN
    DO RECCL = RTCLEUPT(EU) + 1, RTCLEUPT(EU + 1)
      RECCL1 = RECCL
	  DO D = 1, mNumCR - 2
        DO B = 1, mNumBldg
          EQCUEC(D, RECCL1, B) = EQCUEC(D, RECCL, B)
        ENDDO
      ENDDO
    ENDDO
  ENDIF
ENDDO

!Miscellaneous Electric Loads (MELs; no end use number)
READ(IUNIT1, *) ((FANUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! furnace fans
READ(IUNIT1, *) ((TVSUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! televisions
READ(IUNIT1, *) ((STBUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! set-top boxes
READ(IUNIT1, *) ((HTSUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! home theater systems
READ(IUNIT1, *) ((OTTUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! over-the-top (OTT) streaming devices
READ(IUNIT1, *) ((VGCUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! video game consoles
READ(IUNIT1, *) ((DPCUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! desktop personal computers
READ(IUNIT1, *) ((LPCUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! laptop personal computers
READ(IUNIT1, *) ((MONUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! computer monitors
READ(IUNIT1, *) ((NETUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! computer networking equipment
READ(IUNIT1, *) ((BATUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! rechargeable devices
READ(IUNIT1, *) ((CFNUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! ceiling fans
READ(IUNIT1, *) ((COFUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! coffee makers
READ(IUNIT1, *) ((DEHUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! dehumidifiers
READ(IUNIT1, *) ((MCOUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! microwave ovens
READ(IUNIT1, *) ((PLPUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! pool pumps
READ(IUNIT1, *) ((PLHUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! pool heaters
READ(IUNIT1, *) ((SECUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! home security systems
READ(IUNIT1, *) ((SPAUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! portable electric spas
READ(IUNIT1, *) ((WCLUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! wine coolers
READ(IUNIT1, *) ((SPKUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! smart speakers
READ(IUNIT1, *) ((PHNUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! smartphones
READ(IUNIT1, *) ((TABUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! tablets
READ(IUNIT1, *) ((KITUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! small kitchen appliances
READ(IUNIT1, *) ((EAUEC(D, B), B = 1, mNumBldg), D = 1, mNumCR - 2) ! electric other equipment

!Secondary space heating equipment (natural gas, electricity, propane, distillate fuel oil, wood)
READ(IUNIT1, *) (((SHTUEC(D, E, B), B = 1, mNumBldg), D = 1, mNumCR - 2), E = 1, 5)  !NoKero	!TODO - replace 5 with NSHTRFL? would need to be declared in different module/subroutine

!Other appliances (natural gas, propane, distillate fuel oil)
READ(IUNIT1, *) (((APPUEC(D, E, B), B = 1, mNumBldg), D = 1, mNumCR - 2), E = 1, 3)	!TODO - replace 3 with parameter?

IUNIT1=FILE_MGR('C',FNAME,NEW)

END SUBROUTINE RDUECS


!==============================================================================
! CALCULATE CONSUMPTION IN RECS YEAR
!  CALLED ONLY FOR CurIYr = RECSYear
!==============================================================================
SUBROUTINE EXCONS
IMPLICIT NONE

INCLUDE 'tranrep'

REAL*4  TVSCON(mNumYr,mNumCR-2),STBCON(mNumYr,mNumCR-2),HTSCON(mNumYr,mNumCR-2),OTTCON(mNumYr,mNumCR-2),&
        VGCCON(mNumYr,mNumCR-2)
REAL*4  DPCCON(mNumYr,mNumCR-2),LPCCON(mNumYr,mNumCR-2),MONCON(mNumYr,mNumCR-2),NETCON(mNumYr,mNumCR-2)
REAL*4  BATCON(mNumYr,mNumCR-2),CFNCON(mNumYr,mNumCR-2),COFCON(mNumYr,mNumCR-2),DEHCON(mNumYr,mNumCR-2),&
        MCOCON(mNumYr,mNumCR-2),PLPCON(mNumYr,mNumCR-2),SPACON(mNumYr,mNumCR-2),WCLCON(mNumYr,mNumCR-2),&
        SPKCON(mNumYr,mNumCR-2),PHNCON(mNumYr,mNumCR-2),TABCON(mNumYr,mNumCR-2),KITCON(mNumYr,mNumCR-2),&
        PLHCON(mNumYr,mNumCR-2),SECCON(mNumYr,mNumCR-2),EACON(mNumYr,mNumCR-2)

REAL*4  TVSCONUS(mNumYr),STBCONUS(mNumYr),HTSCONUS(mNumYr),OTTCONUS(mNumYr),VGCCONUS(mNumYr)
REAL*4  DPCCONUS(mNumYr),LPCCONUS(mNumYr),MONCONUS(mNumYr),NETCONUS(mNumYr)
REAL*4  BATCONUS(mNumYr),CFNCONUS(mNumYr),COFCONUS(mNumYr),DEHCONUS(mNumYr),&
        MCOCONUS(mNumYr),PLPCONUS(mNumYr),SPACONUS(mNumYr),WCLCONUS(mNumYr),&
        SPKCONUS(mNumYr),PHNCONUS(mNumYr),TABCONUS(mNumYr),KITCONUS(mNumYr),&
        PLHCONUS(mNumYr),SECCONUS(mNumYr),EACONUS(mNumYr)

INTEGER D,E,F,B,FCON,EU,EQC,RECCL,EQCGHP,EQCEHP,EQCEWH,EQCSWH,y
INTEGER RECCLGHP,RECCLEHP,RECCLEWH,RECCLSWH

!111(d) - initialize sales based on input restart file, against which to track savings (written to RDM_OUT.txt)
WRITE(9,*) 'RESTART FILE baseline electricity data (Trills) d, y, QELRS(d,y), QELCM(d,y)'
DO D=1,mNumCR-2
  DO y=1,mNumYr
    !QELRS,QELCM in Trills	!TODO - QELRS and QELCM are normally in quads, but these aren't multiplied by 1000 here. This is legacy EPA 111D code, so not really used by the RDM, but is still used by CDM for AB32 calculations
    WRITE(9,5) D,Y,QELRS(d,y),QELCM(d,y)
  ENDDO
ENDDO
5 FORMAT(2i5,2F12.5)

!SET HOT WATER LOAD ADDITIONS FOR CLOTHES WASHERS AND DISHWASHERS	!TODO -  verify/update/move closer to point of use: load adjustment of clothes washers with respect to water heating load in RECSyear
CWLOAD(RECSYear)=0.2047

!********************************************************************
!CALCULATE SPACE HEATING CONSUMPTION
!********************************************************************
EU = 1

!Initialize main and secondary space heating consumption variables
HTRCON(CurIYr,1:NHTRFL,1:mNumCR-2)=0.0
SHTCON(CurIYr,1:NHTRFL,1:mNumCR-2)=0.0

!CALCULATE HEATING CONSUMPTION FOR THE IDENTIFIED FUELS IN RSCLASS
! AT THE SAME TIME, IDENTIFY ELECTRIC AIR-SOURCE HEAT PUMPS AND GROUND-SOURCE HEAT PUMPS FOR LATER USE	!TODO - is this still necessary to do?
! LOOP OVER ALL HEATING EQUIPMNENT TYPES
DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !ALL RECORDS IN RSCLASS
  EQC=RTCLEQCL(RECCL)
  F=RTFUEL(RECCL)
  !MAP RSCLASS FUEL NUMBERS INTO HTRCON FUEL NUMBERS
  FCON=FHTRCON(F)
  !ALSO FIND INDICES FOR THE AIR-SOURCE AND GROUND-SOURCE HEAT PUMPS.
  IF (RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
    EQCEHP=RTCLEQCL(RECCL)
    RECCLEHP=EQCEHP
  ELSEIF (RTCLNAME(RECCL).EQ.'GEO_HP') THEN	!TODO - is this still necessary to do?
    EQCGHP=RTCLEQCL(RECCL)
    RECCLGHP=EQCGHP
  ENDIF

  !NOW THAT FUEL IS MAPPED, AGGREGATE BY D AND B
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      HTRCON(CurIYr,FCON,D)=HTRCON(CurIYr,FCON,D)+ &
       (EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
    ENDDO !BUILDING TYPES, B
  ENDDO !CENSUS DIVISIONS, D
ENDDO !HEATING EQUIPMENT TYPES, RECCL

!********************************************************************
!CALCULATE SPACE COOLING CONSUMPTION
!********************************************************************
EU = 2  !space cooling

!Initialize space cooling consumption variable
COOLCN(CurIYr,1:NCLFL,1:mNumCR-2)=0.0

! CALCULATE COOLING CONSUMPTION FOR THE IDENTIFIED FUELS IN RSCLASS
DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !ALL RECORDS IN RSCLASS
  EQC=RTCLEQCL(RECCL)
  F=RTFUEL(RECCL)
  !MAP RSCLASS FUEL NUMBERS INTO CLCON FUEL NUMBERS
  FCON=FCLCON(F)
  !ALSO FIND INDICES FOR AIR-SOURCE AND GROUND-SOURCE HEAT PUMPS AS FOR HEATING
  IF (RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
    EQCEHP=RTCLEQCL(RECCL)
    !NOTE START NUMBERING AFTER HEATERS -- NEED TO INCREMENT EQCEHP BY HEATERS
    RECCLEHP=EQCEHP+RTCLEUPT(EU)
  ELSEIF (RTCLNAME(RECCL).EQ.'GEO_HP') THEN
    EQCGHP=RTCLEQCL(RECCL)
    !NOTE START NUMBERING AFTER HEATERS -- NEED TO INCREMENT EQCGHP BY HEATERS
    RECCLGHP=EQCGHP+RTCLEUPT(EU)
  ENDIF

  !NOW THAT FUEL IS MAPPED, AGGREGATE BY D AND B
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      COOLCN(CurIYr,FCON,D)=COOLCN(CurIYr,FCON,D)+ &
       (EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
    ENDDO !BUILDING TYPES, B
  ENDDO !CENSUS DIVISIONS, D
ENDDO !COOLING EQUIPMENT TYPES, RECCL

!********************************************************************
!CALCULATE CLOTHES WASHING CONSUMPTION
!********************************************************************
EU = 3

DO D=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    !Initialize space cooling consumption variable
	CSWCON(CurIYr,D)=0.0
    DO B=1,mNumBldg
      CSWCON(CurIYr,D)=CSWCON(CurIYr,D)+(EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE DISHWASHING CONSUMPTION
!********************************************************************
EU = 4

DO D=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    !Initialize dishwashing consumption variable
    DSWCON(CurIYr,D)=0.0
    DO B=1,mNumBldg
      DSWCON(CurIYr,D)=DSWCON(CurIYr,D)+(EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE WATER HEATING CONSUMPTION
! ALSO CALCULATE SOLAR CONSUMPTION - USES EL UEC (55 PERCENT)	!TODO - verify "55 PERCENT" comment
!********************************************************************
EU = 5

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  !FIND INDICES FOR THE ELECTRIC AND SOLAR WATER HEATERS; USED TO COMPUTE H2OCON FOR SOLAR FUEL (FCON=5)
  IF (RTCLNAME(RECCL).EQ.'ELEC_WH') THEN
    EQCEWH=RTCLEQCL(RECCL)
    !AS FOR COOLING, INCREMENT THE EQUIPMENT CLASS BY THE SUM OF CLASSES BEFORE IT
    RECCLEWH=EQCEWH+RTCLEUPT(EU)
  ELSEIF (RTCLNAME(RECCL).EQ.'SOLAR_WH') THEN
    EQCSWH=RTCLEQCL(RECCL)
    !AS FOR COOLING, INCREMENT THE EQUIPMENT CLASS BY THE SUM OF CLASSES BEFORE IT
    RECCLSWH=EQCSWH+RTCLEUPT(EU)
  ENDIF
ENDDO

DO D=1,mNumCR-2
  SLCON(CurIYr,D)=0.0
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    F=RTFUEL(RECCL)
    FCON=FWHCON(F)
    H2OCON(CurIYr,FCON,D)=0.0
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      F=RTFUEL(RECCL)
      FCON=FWHCON(F)
      H2OCON(CurIYr,FCON,D)=H2OCON(CurIYr,FCON,D)+(EQCUEC(D,RECCL,B)*EQCESE(RECSYear,RECCL,B,D))
    ENDDO

    !SOLAR IS COMPUTED DIFFERENTLY (RTCLEQCL=5 for SOLAR_WH in RSCLASS)
    H2OCON(CurIYr,5,D)=H2OCON(CurIYr,5,D)+ &  !NoKero
     (EQCESE(RECSYear,RECCLSWH,B,D)* &
     (EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412.)
    SLCON(CurIYr,D)=SLCON(CurIYr,D)+ &
     (EQCESE(RECSYear,RECCLSWH,B,D)* &
     (EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412.)
    SLEQCN(CurIYr,1,B,D)=(EQCESE(RECSYear,RECCLSWH,B,D)* &
     (EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412.)
  ENDDO
ENDDO

!********************************************************************
!CALCULATE COOKING CONSUMPTION
!********************************************************************
EU = 6

DO D=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    CKCON(CurIYr,EQC,D)=0.0
    DO B=1,mNumBldg
      CKCON(CurIYr,EQC,D)=CKCON(CurIYr,EQC,D)+(EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE CLOTHES DRYER CONSUMPTION
!********************************************************************
EU = 7

DO D=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    DRYCON(CurIYr,EQC,D)=0.0
    DO B=1,mNumBldg
      DRYCON(CurIYr,EQC,D)=DRYCON(CurIYr,EQC,D)+(EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE REFRIGERATION CONSUMPTION
!********************************************************************
EU = 8

DO D=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    REFCON(CurIYr,D)=0.0
    DO B=1,mNumBldg
      REFCON(CurIYr,D)=REFCON(CurIYr,D)+(EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE FREEZING CONSUMPTION
!********************************************************************
EU = 9

DO D=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    FRZCON(CurIYr,D)=0.0
    DO B=1,mNumBldg
      FRZCON(CurIYr,D)=FRZCON(CurIYr,D)+(EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE FURNACE FAN CONSUMPTION
!********************************************************************

!Initialize fan consumption
FANCON(CurIYr,1:mNumCR-2)=0.0

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    FANEQCN(CurIYr,1,b,d)=(FANEQP(RECSYear,B,D)*FANUEC(D,B))
    FANCON(CurIYr,D)=FANCON(CurIYr,D)+(FANEQP(RECSYear,B,D)*FANUEC(D,B))
   ENDDO
ENDDO

!*******************************************************************
!CALCULATE MISCELLANEOUS ELECTRIC LOADS (MELs) CONSUMPTION:
! TELEVISIONS, SET-TOP BOXES, HOME THEATER SYSTEMS, OVER-THE-TOP (OTT) STREAMING DEVICES, VIDEO GAME CONSOLES,
! DESKTOP PCs, LAPTOP PCs, MONITORS, NETWORKING EQUIPMENT, NON-PC RECHARGEABLES,CEILING FANS, COFFEE MAKERS, DEHUMIDIFIERS,
! MICROWAVES, POOL PUMPS, POOL HEATERS, SECURITY SYSTEMS, PORTABLE ELECTRIC SPAS, PORTABLE REFRIGERATION EQUIPMENT (WINE COOLERS),
! SMART SPEAKERS, SMARTPHONES, TABLETS, SMALL KITCHEN APPLIANCES, OTHER ELECTRIC APPLIANCES
!********************************************************************

!Initialize consumption variables
TVSCON(CurIYr,1:mNumCR-2)=0.0
STBCON(CurIYr,1:mNumCR-2)=0.0
HTSCON(CurIYr,1:mNumCR-2)=0.0
OTTCON(CurIYr,1:mNumCR-2)=0.0
VGCCON(CurIYr,1:mNumCR-2)=0.0
DPCCON(CurIYr,1:mNumCR-2)=0.0
LPCCON(CurIYr,1:mNumCR-2)=0.0
MONCON(CurIYr,1:mNumCR-2)=0.0
NETCON(CurIYr,1:mNumCR-2)=0.0
BATCON(CurIYr,1:mNumCR-2)=0.0
CFNCON(CurIYr,1:mNumCR-2)=0.0
COFCON(CurIYr,1:mNumCR-2)=0.0
DEHCON(CurIYr,1:mNumCR-2)=0.0
MCOCON(CurIYr,1:mNumCR-2)=0.0
PLPCON(CurIYr,1:mNumCR-2)=0.0
PLHCON(CurIYr,1:mNumCR-2)=0.0
SECCON(CurIYr,1:mNumCR-2)=0.0
SPACON(CurIYr,1:mNumCR-2)=0.0
WCLCON(CurIYr,1:mNumCR-2)=0.0
SPKCON(CurIYr,1:mNumCR-2)=0.0
PHNCON(CurIYr,1:mNumCR-2)=0.0
TABCON(CurIYr,1:mNumCR-2)=0.0
KITCON(CurIYr,1:mNumCR-2)=0.0
EACON(CurIYr,1:mNumCR-2)=0.0

TVSCONUS(CurIYr)=0.0
STBCONUS(CurIYr)=0.0
HTSCONUS(CurIYr)=0.0
OTTCONUS(CurIYr)=0.0
VGCCONUS(CurIYr)=0.0
DPCCONUS(CurIYr)=0.0
LPCCONUS(CurIYr)=0.0
MONCONUS(CurIYr)=0.0
NETCONUS(CurIYr)=0.0
BATCONUS(CurIYr)=0.0
CFNCONUS(CurIYr)=0.0
COFCONUS(CurIYr)=0.0
DEHCONUS(CurIYr)=0.0
MCOCONUS(CurIYr)=0.0
PLPCONUS(CurIYr)=0.0
PLHCONUS(CurIYr)=0.0
SPACONUS(CurIYr)=0.0
WCLCONUS(CurIYr)=0.0
SPKCONUS(CurIYr)=0.0
PHNCONUS(CurIYr)=0.0
TABCONUS(CurIYr)=0.0
KITCONUS(CurIYr)=0.0
SECCONUS(CurIYr)=0.0
EACONUS(CurIYr)=0.0

DO D=1,mNumCR-2		
  DO B=1,mNumBldg
    TVSCON(CurIYr,D)=TVSCON(CurIYr,D)+(TVSEQP(RECSYear,B,D)*TVSUEC(D,B))
    STBCON(CurIYr,D)=STBCON(CurIYr,D)+(STBEQP(RECSYear,B,D)*STBUEC(D,B))
    HTSCON(CurIYr,D)=HTSCON(CurIYr,D)+(HTSEQP(RECSYear,B,D)*HTSUEC(D,B))
    OTTCON(CurIYr,D)=OTTCON(CurIYr,D)+(OTTEQP(RECSYear,B,D)*OTTUEC(D,B))
    VGCCON(CurIYr,D)=VGCCON(CurIYr,D)+(VGCEQP(RECSYear,B,D)*VGCUEC(D,B))
    DPCCON(CurIYr,D)=DPCCON(CurIYr,D)+(DPCEQP(RECSYear,B,D)*DPCUEC(D,B))
    LPCCON(CurIYr,D)=LPCCON(CurIYr,D)+(LPCEQP(RECSYear,B,D)*LPCUEC(D,B))
    MONCON(CurIYr,D)=MONCON(CurIYr,D)+(MONEQP(RECSYear,B,D)*MONUEC(D,B))
    NETCON(CurIYr,D)=NETCON(CurIYr,D)+(NETEQP(RECSYear,B,D)*NETUEC(D,B))
    BATCON(CurIYr,D)=BATCON(CurIYr,D)+(BATEQP(RECSYear,B,D)*BATUEC(D,B))
    CFNCON(CurIYr,D)=CFNCON(CurIYr,D)+(CFNEQP(RECSYear,B,D)*CFNUEC(D,B))
    COFCON(CurIYr,D)=COFCON(CurIYr,D)+(COFEQP(RECSYear,B,D)*COFUEC(D,B))
    DEHCON(CurIYr,D)=DEHCON(CurIYr,D)+(DEHEQP(RECSYear,B,D)*DEHUEC(D,B))
    MCOCON(CurIYr,D)=MCOCON(CurIYr,D)+(MCOEQP(RECSYear,B,D)*MCOUEC(D,B))
    PLPCON(CurIYr,D)=PLPCON(CurIYr,D)+(PLPEQP(RECSYear,B,D)*PLPUEC(D,B))
    PLHCON(CurIYr,D)=PLHCON(CurIYr,D)+(PLHEQP(RECSYear,B,D)*PLHUEC(D,B))
    SECCON(CurIYr,D)=SECCON(CurIYr,D)+(SECEQP(RECSYear,B,D)*SECUEC(D,B))
    SPACON(CurIYr,D)=SPACON(CurIYr,D)+(SPAEQP(RECSYear,B,D)*SPAUEC(D,B))
    WCLCON(CurIYr,D)=WCLCON(CurIYr,D)+(WCLEQP(RECSYear,B,D)*WCLUEC(D,B))
    SPKCON(CurIYr,D)=SPKCON(CurIYr,D)+(SPKEQP(RECSYear,B,D)*SPKUEC(D,B))
    PHNCON(CurIYr,D)=PHNCON(CurIYr,D)+(PHNEQP(RECSYear,B,D)*PHNUEC(D,B))
    TABCON(CurIYr,D)=TABCON(CurIYr,D)+(TABEQP(RECSYear,B,D)*TABUEC(D,B))
    KITCON(CurIYr,D)=KITCON(CurIYr,D)+(KITEQP(RECSYear,B,D)*KITUEC(D,B))
    EACON(CurIYr,D)=EACON(CurIYr,D)+(EH(RECSYear,B,D)*EAUEC(D,B))
    EAEQCN(CurIYr,1,B,D)=EH(RECSYear,B,D)*EAUEC(D,B)
  ENDDO
ENDDO

DO D=1,mNumCR-2
  TVSCONUS(CurIYr)=TVSCONUS(CurIYr)+TVSCON(CurIYr,D)
  STBCONUS(CurIYr)=STBCONUS(CurIYr)+STBCON(CurIYr,D)
  HTSCONUS(CurIYr)=HTSCONUS(CurIYr)+HTSCON(CurIYr,D)
  OTTCONUS(CurIYr)=OTTCONUS(CurIYr)+OTTCON(CurIYr,D)
  VGCCONUS(CurIYr)=VGCCONUS(CurIYr)+VGCCON(CurIYr,D)
  DPCCONUS(CurIYr)=DPCCONUS(CurIYr)+DPCCON(CurIYr,D)
  LPCCONUS(CurIYr)=LPCCONUS(CurIYr)+LPCCON(CurIYr,D)
  MONCONUS(CurIYr)=MONCONUS(CurIYr)+MONCON(CurIYr,D)
  NETCONUS(CurIYr)=NETCONUS(CurIYr)+NETCON(CurIYr,D)
  BATCONUS(CurIYr)=BATCONUS(CurIYr)+BATCON(CurIYr,D)
  CFNCONUS(CurIYr)=CFNCONUS(CurIYr)+CFNCON(CurIYr,D)
  COFCONUS(CurIYr)=COFCONUS(CurIYr)+COFCON(CurIYr,D)
  DEHCONUS(CurIYr)=DEHCONUS(CurIYr)+DEHCON(CurIYr,D)
  MCOCONUS(CurIYr)=MCOCONUS(CurIYr)+MCOCON(CurIYr,D)
  PLPCONUS(CurIYr)=PLPCONUS(CurIYr)+PLPCON(CurIYr,D)
  PLHCONUS(CurIYr)=PLHCONUS(CurIYr)+PLHCON(CurIYr,D)
  SECCONUS(CurIYr)=SECCONUS(CurIYr)+SECCON(CurIYr,D)
  SPACONUS(CurIYr)=SPACONUS(CurIYr)+SPACON(CurIYr,D)
  WCLCONUS(CurIYr)=WCLCONUS(CurIYr)+WCLCON(CurIYr,D)
  SPKCONUS(CurIYr)=SPKCONUS(CurIYr)+SPKCON(CurIYr,D)
  PHNCONUS(CurIYr)=PHNCONUS(CurIYr)+PHNCON(CurIYr,D)
  TABCONUS(CurIYr)=TABCONUS(CurIYr)+TABCON(CurIYr,D)
  KITCONUS(CurIYr)=KITCONUS(CurIYr)+KITCON(CurIYr,D)
  EACONUS(CurIYr)=EACONUS(CurIYr)+EACON(CurIYr,D)
ENDDO

!********************************************************************
!AGGREGATE TELEVISIONS AND RELATED EQUIPMENT CONSUMPTION
!********************************************************************
DO D=1,mNumCR-2
  TVRCON(CurIYr,D)=0.0
  DO B=1,mNumBldg
    TVRCON(CurIYr,D)=TVSCON(CurIYr,D)+STBCON(CurIYr,D)+HTSCON(CurIYr,D)+OTTCON(CurIYr,D)+VGCCON(CurIYr,D)
  ENDDO
ENDDO

!********************************************************************
!AGGREGATE PERSONAL COMPUTER AND RELATED EQUIPMENT CONSUMPTION
!********************************************************************
DO D=1,mNumCR-2
  PCRCON(CurIYr,D)=0.0
  DO B=1,mNumBldg
    PCRCON(CurIYr,D)=DPCCON(CurIYr,D)+LPCCON(CurIYr,D)+MONCON(CurIYr,D)+NETCON(CurIYr,D)
  ENDDO
ENDDO

!********************************************************************
!AGGREGATE MELs CONSUMPTION
!********************************************************************
DO D=1,mNumCR-2
  APCON(CurIYr,D)=0.0
  DO B=1,mNumBldg
    APCON(CurIYr,D)=BATCON(CurIYr,D)+CFNCON(CurIYr,D)+&
     COFCON(CurIYr,D)+DEHCON(CurIYr,D)+MCOCON(CurIYr,D)+&
     PLPCON(CurIYr,D)+SECCON(CurIYr,D)+SPACON(CurIYr,D)+&
     WCLCON(CurIYr,D)+PLHCON(CurIYr,D)+EACON(CurIYr,D)+&
     SPKCON(CurIYr,D)+PHNCON(CurIYr,D)+TABCON(CurIYr,D)+KITCON(CurIYr,D)
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE NON-ELECTRIC APPLIANCES CONSUMPTION
!********************************************************************
DO D=1,mNumCR-2
  DO F=1,3  !Natural Gas, Propane, and Distillate Fuel Oil/Kerosene	!TODO - replace 3 with parameter
    APLCON(CurIYr,F,D)=0.0
    DO B=1,mNumBldg
      APLCON(CurIYr,F,D)=APLCON(CurIYr,F,D)+(APPUEC(D,F,B)*APPEQP(RECSYear,B,D,F))
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE SECONDARY SPACE HEATING CONSUMPTION
!********************************************************************
DO D=1,mNumCR-2
  DO F=1,NSHTRFL  !NoKero
    SHTCON(CurIYr,F,D)=0.0
    DO B=1,mNumBldg
      SHEQCN(CurIYr,F,B,D)=SHTEQP(RECSYear,B,D,F)*SHTUEC(D,F,B)
      SHTCON(CurIYr,F,D)=SHTCON(CurIYr,F,D)+(SHTEQP(RECSYear,B,D,F)*SHTUEC(D,F,B))
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!INITIALIZE RECS-YEAR NEMS DATA
!********************************************************************
QBMRS(NATIONALPTR,CurIYr)=0.0 !biomass (wood)
QSTRS(NATIONALPTR,CurIYr)=0.0 !solar thermal (water heaters)
QPVRS(NATIONALPTR,CurIYr)=0.0 !solar photovoltaic
QGERS(NATIONALPTR,CurIYr)=0.0 !geothermal	!TODO - Geothermal not modeled; remove?

!********************************************************************
!CALCULATE CENSUS DIVISION FUEL CONSUMPTION
!********************************************************************
DO D=1,mNumCR-2
  !NATURAL GAS
  RSFLCN(CurIYr,1,D)= &
   (HTRCON(CurIYr,1,D)+H2OCON(CurIYr,1,D)+APLCON(CurIYr,1,D)+COOLCN(CurIYr,2,D)+ &	!TODO - If removing NG_HP, remove COOLCN(CurIYr,2,D)
   CKCON(CurIYr,1,D)+DRYCON(CurIYr,1,D)+SHTCON(CurIYr,1,D))/1000000.
  QNGRS(D,CurIYr)=RSFLCN(CurIYr,1,D)
  QGFRS(D,CurIYr)=RSFLCN(CurIYr,1,D)*1.0
  QGIRS(D,CurIYr)=RSFLCN(CurIYr,1,D)*0.0

  !ELECTRICITY
  RSFLCN(CurIYr,2,D)= &
   (HTRCON(CurIYr,2,D)+COOLCN(CurIYr,1,D)+H2OCON(CurIYr,2,D)+REFCON(CurIYr,D)+ &
   FRZCON(CurIYr,D)+LTCON(CurIYr,D)+APCON(CurIYr,D)+CKCON(CurIYr,3,D)+ &
   DRYCON(CurIYr,2,D)+SHTCON(CurIYr,2,D)+PCRCON(CurIYr,D)+TVRCON(CurIYr,D)+ &
   CSWCON(CurIYr,D)+DSWCON(CurIYr,D)+FANCON(CurIYr,D))/1000000.
  QELRS(D,CurIYr)=RSFLCN(CurIYr,2,D)+TRQ_ELEC(1,D,CurIYr)

  !DISTILLATE FUEL OIL
  RSFLCN(CurIYr,3,D)= &
   (HTRCON(CurIYr,3,D)+H2OCON(CurIYr,3,D)+APLCON(CurIYr,3,D)+SHTCON(CurIYr,3,D))/1000000.
  QDSRS(D,CurIYr)=RSFLCN(CurIYr,3,D)

  !PROPANE
  RSFLCN(CurIYr,4,D)= &
   (HTRCON(CurIYr,4,D)+H2OCON(CurIYr,4,D)+APLCON(CurIYr,2,D)+ &
   CKCON(CurIYr,2,D)+SHTCON(CurIYr,4,D))/1000000.
  QLGRS(D,CurIYr)=RSFLCN(CurIYr,4,D)
  QPRRS(D,CurIYr)=QLGRS(D,CurIYr)

  !BIOMASS (WOOD)
  RSFLCN(CurIYr,5,D)=(HTRCON(CurIYr,5,D)+SHTCON(CurIYr,5,D))/1000000.  !NoKero
  QBMRS(D,CurIYr)=RSFLCN(CurIYr,5,D)  !NoKero

  !SOLAR ENERGY - ADDED SOLAR WATER HEATERS (SLCON, SOLAR_WH)
  SLCON(CurIYr,10)=SLCON(CurIYr,10)+H2OCON(CurIYr,5,D)	!TODO - mNumCR=10 for California; should be MNUMCR=11 for national total instead?
  QSTRS(D,CurIYr)=H2OCON(CurIYr,5,D)/1000000.
  QSTRS(NATIONALPTR,CurIYr)=QSTRS(NATIONALPTR,CurIYr)+H2OCON(CurIYr,5,D)/1000000.
  QPVRS(D,CurIYr)=QPVRS(NATIONALPTR,CurIYr)

  !NO LONGER MODELED	!TODO - Remove these?
  !KEROSENE
  QKSRS(D,CurIYr)=0.0 !NoKero
  !GEOTHERMAL
  QGERS(D,CurIYr)=0.0
ENDDO

!********************************************************************
!CALCULATE US (DIVISION 10) FUEL CONSUMPTION IN QUADRILLION BTU	!TODO - D=10 is technically reserved for CA per PARAMETR include file; US/National should otherwise be 11; verify use of D=10 versus D=11 across RDM
!********************************************************************
DO F=1,mNumFuel  !NoKero
  RSFLCN(CurIYr,F,MNUMCR-1)=0.0
  TRQ_ELEC(1,MNUMCR,CurIYr)=0.0
  DO D=1,mNumCR-2
    RSFLCN(CurIYr,F,MNUMCR-1)=RSFLCN(CurIYr,F,MNUMCR-1)+RSFLCN(CurIYr,F,D)
    TRQ_ELEC(1,MNUMCR,CurIYr) = TRQ_ELEC(1,MNUMCR,CurIYr) + TRQ_ELEC(1,D,CurIYr)
  ENDDO
ENDDO

!Organized by FCON; natural gas, electricity, distillate fuel oil/kerosene, propane, wood
QNGRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,1,MNUMCR-1)
QGFRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,1,MNUMCR-1)*1.0  !FIRM GAS	!TODO - still used?
QGIRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,1,MNUMCR-1)*0.0  !INTERRUPTIBLE GAS	!TODO - still used?
QELRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,2,MNUMCR-1) + TRQ_ELEC(1,MNUMCR,CurIYr)
QDSRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,3,MNUMCR-1)
QLGRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,4,MNUMCR-1)
QPRRS(NATIONALPTR,CurIYr)=QLGRS(NATIONALPTR,CurIYr)
QBMRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,5,MNUMCR-1)  !NoKero

!NO LONGER MODELED	!TODO - Remove these?
QKSRS(NATIONALPTR,CurIYr)=0.0  !NoKero
QGERS(NATIONALPTR,CurIYr)=0.0

END SUBROUTINE EXCONS


!==============================================================================
! CALCULATE NEW HOUSING FOR RSYR = RECSYear+1:LastSTEOYrAvail
!==============================================================================
SUBROUTINE NEWHSE
IMPLICIT NONE

INTEGER  Y, B, D, E, F, IUNIT1,Y1

Y=CurCalYr
Y1=CurIYr

DO D=1,mNumCR-2
  IF (Y.EQ.RECSYear+1) NH(RECSYear,1,D)=0.0
  IF (Y.EQ.RECSYear+1) NH(RECSYear,2,D)=0.0
  IF (Y.EQ.RECSYear+1) NH(RECSYear,3,D)=0.0

  HSEADD(Y,1,D) = 1000000.0 * MC_HUSPS1(D,Y1)
  NH(Y,1,D) = 1000000.0 * MC_HUSPS1(D,Y1) + (NH(Y-1,1,D)*HDR(1))
  HSEADD(Y,2,D) = 1000000.0 * MC_HUSPS2A(D,Y1)
  NH(Y,2,D) = 1000000.0 * MC_HUSPS2A(D,Y1) + (NH(Y-1,2,D)*HDR(2))
  HSEADD(Y,3,D) = 1000000.0 * MC_HUSMFG(D,Y1)
  NH(Y,3,D) = 1000000.0 * MC_HUSMFG(D,Y1) + (NH(Y-1,3,D)*HDR(3))
ENDDO

!DEVELOP SUBTOTALS FOR REPORTING
DO D=1,mNumCR-2		!TODO - optimize/remove DO loop?
  ALLNEW(Y,D)=0.0
  DO B=1,mNumBldg
    ALLNEW(Y,D)=NH(Y,B,D)
  ENDDO
ENDDO

DO D=1,mNumCR-2		!TODO - optimize/remove DO loop?
  HHSTOCKBYDIV(CurCalYr,D)=0.0
  DO B=1,mNumBldg
    HHSTOCKBYDIV(CurCalYr,D)=(HHSTOCKBYDIV(CurCalYr,D)+NH(CurCalYr,B,D)+EH(CurCalYr,B,D))/1000000.
  ENDDO
ENDDO

END SUBROUTINE NEWHSE


!==============================================================================
! CALCULATE AVERAGE SQUARE FOOTAGE OF HOUSING FOR RSYR = RECSYear+1:EndYr
!  Note: not called in the RECS year, so if this is the first year after RECS,
!   some initial RECS-year calculations are also done.
!==============================================================================
SUBROUTINE SQFTCALC
IMPLICIT NONE

REAL*4 SQFTTOT(RECSYear:EndYr), RENOVATE
INTEGER Y, B, D, T, Y1

Y=CurCalYr  !calendar year
Y1=CurIYr   !NEMS index for calendar year

RENOVATE=7.18 !THIS IS DERIVED FROM THE % OF HOMES ADDING A ROOM X THE SIZE ADDED (1.2% X 1/3 FLOOR AREA).	!TODO - Update

!Aggrgate new and existing housing stocks
OLDHSES(Y)=0.0
NEWHSES(Y)=0.0

DO B=1,mNumBldg
  DO D=1,mNumCR-2
    OLDHSES(Y)=OLDHSES(Y)+EH(Y,B,D)
    NEWHSES(Y)=NEWHSES(Y)+NH(Y,B,D)
  ENDDO
ENDDO

!Initialize RECS-year values
! Process RECS-year calculations if this is the first call to this routine
IF (Y.EQ.RECSYear+1) THEN
  SQFTTOT(RECSYear)=0.0
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      !calculate total square footage in the RECS stock
      SQFTTOT(RECSYear)=SQFTTOT(RECSYear)+ SQRFOOT(RECSYear,B,D)*EH(RECSYear,B,D)
      EXSQRFOOT(RECSYear,B,D)=SQRFOOT(RECSYear,B,D)
    ENDDO !B
  ENDDO !D
  SQFTAVG(Y1-1)=SQFTTOT(RECSYear)/OLDHSES(RECSYear)
ENDIF !End of RECS-year calculations

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    !For projected renovation activity, allow the square footage of existing (RECS-year) houses to increase over time
    EXSQRFOOT(Y,B,D)=SQRFOOT(RECSYear,B,D)+(RENOVATE*(Y-RECSYear))
  ENDDO !B
ENDDO !D

!CALCULATE AVERAGE SQUARE FOOT IN EACH YEAR
DO B=1,mNumBldg
  DO D=1,mNumCR-2
    IF (Y.EQ.RECSYear+1) THEN
      SQNEW(RECSYear,B,D)=0.
    ELSE
      SQNEW(Y-1,B,D)=0.0
      DO T=RECSYear+1,Y-1
        SQNEW(Y-1,B,D)= SQNEW(Y-1,B,D) + &
         ((HSEADD(T,B,D)*HDR(B)**(Y-1-T))*SQRFOOT(T,B,D))/NH(Y-1,B,D)
      ENDDO
    ENDIF
  ENDDO
ENDDO

!CALCULATE STOCK AVERAGE SQUARE FOOTAGE FOR OUTPUT TO RDM_DBOUT.TXT
DO B=1,mNumBldg
  DO D=1,mNumCR-2
    IF (Y.EQ.RECSYear+1) THEN
      STOCKSQRFOOT(CurCalYr-1,B,D)=EXSQRFOOT(CurCalYr-1,B,D)
      STOCKSQRFOOT(CurCalYr,B,D)=(EH(Y,B,D)*EXSQRFOOT(CurCalYr,B,D)+HSEADD(Y,B,D)*SQRFOOT(Y,B,D))/(EH(Y,B,D)+HSEADD(Y,B,D))
    ELSE
      STOCKSQRFOOT(CurCalYr,B,D)=(EH(Y,B,D)*EXSQRFOOT(CurCalYr,B,D)+HSEADD(Y,B,D)*SQRFOOT(Y,B,D)+SQNEW(Y-1,B,D)*NH(Y-1,B,D))/(EH(Y,B,D)+NH(Y,B,D))
    ENDIF
  ENDDO
ENDDO

SQFTTOT(Y)=0.0
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    SQFTTOT(Y)=SQFTTOT(Y)+ ( SQRFOOT(Y,B,D)*HSEADD(Y,B,D) + SQNEW(Y-1,B,D)*NH(Y-1,B,D)+ EXSQRFOOT(CurCalYr,B,D)*EH(Y,B,D) )
  ENDDO
ENDDO

SQFTAVG(Y1)= SQFTTOT(Y) / ( OLDHSES(Y)+  NEWHSES(Y) )

END SUBROUTINE SQFTCALC


!==============================================================================
! CALCULATE REPLACEMENT EQUIPMENT TYPE
!==============================================================================
SUBROUTINE REPLACE(EU,R,B,RECCL,FLAG)
IMPLICIT NONE

REAL*4 EQCOST,CAPITAL,RETAIL,RPSHARE(mNumRTCl)
REAL*4 TOTSH,RETIRED,RETIREDR,EQC
INTEGER EU,EQCSW,RECCL,RECCLSW,EQTSW,RECTYSW,B,R
INTEGER I,Y,FLAG

TOTSH = 0.0

IF (FLAG.EQ.1) THEN
  !OEQCREP is old value of EQCREP with no technology switching
  RETIRED = OEQCREP(CurCalYr,RECCL,1,R)
ELSE
  !OEQCRP90 is old value of EQCRP90 with only switching to NG_FA
  RETIRED = OEQCRP90(CurCalYr,RECCL,1,R)
  RETIREDR= OEQCRP90R(CurCalYr,RECCL,1,R)
ENDIF

DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !process all classes in this end use
  EQCSW = RTCLEQCL(RECCLSW)
  EQTSW = RTCLTYPT(RECCLSW)
  RECTYSW = 0

  DO I = RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
    IF (RTEQTYPE(I).EQ.EQTSW.AND. &
     (CurCalYr.GE.RTINITYR(I).AND.CurCalYr.LE.RTLASTYR(I))) THEN
      RECTYSW = I
      EXIT ! Once RECTYSW found, get out of loop
    ENDIF
  ENDDO !I

  IF (RECTYSW.EQ.0) THEN
    !Report error to unit 6 (nohup.out)
    WRITE(6,*) 'RESDMSG SUB_REPLACE: Warning - no representative equipment type in RSCLASS for end use = ',EU, &
     ' eq class = ',EQCSW,' eq type = ',EQTSW,' CurCalYr = ',CurCalYr
    RETURN
  ENDIF

  !WHEN RECTYSW IS FOUND, CONTINUE FROM HERE
  !If COSTTRSW = 1, use function EQCOST to compute capital and retail cost of new equipment.
  IF (COSTTRSW.EQ.1) THEN
    CAPITAL = EQCOST(RECTYSW,CurCalYr,"CAP")
    RETAIL = EQCOST(RECTYSW,CurCalYr,"RET")
  !If COSTTRSW = 0, use constant value from RSMEQP file for capital and retail cost of new equipment.
  ELSE
    CAPITAL = RTEQCOST(RECTYSW)
    RETAIL = RTRECOST(RECTYSW)
  ENDIF

  !Compute shares for this equipment class
  RPSHARE(EQCSW) = EXP(RTSWBIAS(RECCLSW)+RTSWBETA(RECCLSW) * (LFCY(EQTSW,B,R,1)+RPINSCOST(RECCL,RECCLSW)))	!TODO - are RECCL and RECCLSW switched here, or is this intentional?

  !TOTSH = TOTAL SHARES FOR ALL EQCSW FOR THIS EQUIPMENT CLASS
  TOTSH = TOTSH + RPSHARE(EQCSW)
ENDDO !RECCLSW

!NORMALIZE SHARES FOR THOSE WHO SWITCH TECHNOLOGIES
DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  EQCSW = RTCLEQCL(RECCLSW)
  IF (TOTSH.GT.0.0) THEN
    RPSHARE(EQCSW)=RPSHARE(EQCSW)/TOTSH
  ELSE
    RPSHARE(EQCSW) = 0.0
  ENDIF

  !SHARE OUT REPLACEMENTS FOR THOSE WHO SWITCH TECHNOLOGIES
  ! RTSWFACT(RECCL)=SWITCHING FACTOR
  IF (FLAG.EQ.1) THEN  !calculate replacements for post-RECSyear homes
    EQCREP(CurCalYr,RECCLSW,B,R) = (EQCREP(CurCalYr,RECCLSW,B,R) + (RETIRED * RPSHARE(EQCSW) * RTSWFACT(RECCL)))
  ELSE  !calculate replacements pre-RECSyear+1 homes
    EQCSW90(CurCalYr,RECCL,RECCLSW,B,R) = (RETIRED * RPSHARE(EQCSW) * RTSWFACT(RECCL))
    EQCSW90R(CurCalYr,RECCL,RECCLSW,B,R) = (RETIREDR * RPSHARE(EQCSW) * RTSWFACT(RECCL))
  ENDIF
ENDDO

!SUM OVER ALL TYPES FOR TOTAL SWITCHES FROM EACH TECHNOLOGY
IF (FLAG.NE.1) THEN
  SWITCHES(CurCalYr,RECCL,B,R)=0.0
  SWITCHESR(CurCalYr,RECCL,B,R)=0.0
  DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    IF (RECCLSW.NE.RECCL) THEN
      SWITCHES(CurCalYr,RECCL,B,R)=SWITCHES(CurCalYr,RECCL,B,R) + EQCSW90(CurCalYr,RECCL,RECCLSW,B,R)
      SWITCHESR(CurCalYr,RECCL,B,R)=SWITCHESR(CurCalYr,RECCL,B,R) + EQCSW90R(CurCalYr,RECCL,RECCLSW,B,R)
    ENDIF
  ENDDO
ENDIF

!REPLACEMENTS FOR THOSE WHO DON'T SWITCH TECHNOLOGIES
IF (FLAG.EQ.1) THEN ! Flag = 1 calculate replacements for post-RECS-year homes
  EQCREP(CurCalYr,RECCL,B,R) = EQCREP(CurCalYr,RECCL,B,R) + (RETIRED * (1-RTSWFACT(RECCL)))
ENDIF

IF (CurCalYr.EQ.2021 .AND. EU.EQ.1 .AND. B.EQ.1 .AND. R.EQ.1) THEN	!TODO - remove after verifying
  DO RECCL=1,27
    DO RECCLSW=1,27
      WRITE(9,*) 'SUBRT_REPLACE-RPINSCOST(RECCL,RECCLSW) ',RECCL,RECCLSW,RPINSCOST(RECCL,RECCLSW)
    ENDDO
  ENDDO
ENDIF

END SUBROUTINE REPLACE


!==============================================================================
! HVAC CHOICE SUBROUTINE
!==============================================================================
SUBROUTINE RSHVAC
IMPLICIT NONE

COMMON/ESTARHOMES/HVEQWTN(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR)
COMMON/SQFTSHELL/HTSQRFOOTFAC(RECSYear:EndYr,nHeatTypes,mNumCR-2,mNumBldg),CLSQRFOOTFAC(RECSYear:EndYr,nHeatTypes,mNumCR-2,mNumBldg)

!LOCAL VARIABLES
REAL*4 HTSQRFOOTFAC,CLSQRFOOTFAC
REAL*4 ACRECOST,rlearncost
REAL*4 EQCOST,CAPITAL,RETAIL,CAPITALX
REAL*4 HDDFACT(mNumCR),CDDFACT(mNumCR),HVOPCOST,HVLFCY
REAL*4 RTEFFAC(3),DECAY,ECTEMP,DENOM,SUM,SUM1,E
REAL*4 HTSHELLFAC(RECSYear:EndYr,MNUMCENDIV,mNumBldg),&
       CLSHELLFAC(RECSYear:EndYr,MNUMCENDIV,mNumBldg),SQFTWEIGHTC(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR),&
       EFFWEIGHTC(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR),EQWTNCA(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR)
REAL*4 COOLSHWT(nCoolTypes,mNumBldg,mNumCR)
REAL*4 TOTEWTN(nHeatClasses,mNumBldg,mNumCR),HVEQWTN,HVBETA2A(MNUMHVAC)
REAL*4 WTDEFF(nHeatClasses),EFFWEIGHT(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR),SQFTWEIGHT(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR)	!TODO - WTDEFF not used (set to 0 later but that's it); remove?
REAL*4 TOTEWTNC(RECSYear:EndYr,nCoolClasses,mNumBldg,mNumCR)  !EqpParam	!TODO - why is TOTEWTNC annualized but TOTEWTN isn't?
REAL*4 EQFSHRNC(nCoolTypes),SHLLEARN(RECSYear:EndYr,mNumBldg,mNumCR-2)
REAL*4 EQFSHRN(nHeatTypes),EFFWT(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR),EQPEFF(nHeatTypes)  !EqpParam	!TODO - EFFWT not used?
REAL*4 WeightTot(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR),TOTEFFWT(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR)	!TODO - WeightTot and TOTEFFWT not used?
REAL*4 EPRICE(mNumCR-2,RECSYear:EndYr),ESHR(RECSYear:EndYr)
!These variables are involved in the efficiency choice calculation	!TODO - clarify comment?
! RECAR and EQTAR are dimensioned for the number of choices across efficiency types in any single year	!TODO - clarify comment?
INTEGER EU,EUPR,RECTY,RECCL,R,B,F,T,EQT,EQC,TYPE,COUNT,L,HCNT,Y,Y1,HC,RECCL1
INTEGER RECAR(nHeatTypes),EQTAR(nHeatTypes),S,HVRCTY,HVC,HVT,FS,HVCC,HVCT,HVTYCNT,HE,HS,CS  !EqpParam

!*******************************************************************
! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES
!*******************************************************************

EU = 1  !space heating
EUPR=1
ALPHA1 = -0.50

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    IF (CurCalYr.GT.RECSYear+1) THEN	!TODO - (RECSYear+1) should be changed to ESTARHISTYR once betas in RSESTAR.txt are readjusted for historical ENERGY STAR housing start shares
      SHLLEARN(CurCalYr,B,R)=SHLLEARN(CurCalYr-1,B,R)*(LEARNFACT(B,R)**(CurCalYr-(RECSYear+1)))	!TODO - (RECSYear+1) should be changed to ESTARHISTYR once betas in RSESTAR.txt are readjusted for historical ENERGY STAR housing start shares
    ELSE
      SHLLEARN(CurCalYr,B,R)=1.0
    ENDIF
  ENDDO
ENDDO

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  !Heating price (EU=1,EUPR=1)
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
  !Cooling Price (EU=2,EUPR=2)
  EPRICE(R,CurCalYr)=PELRSOUT(R,CurIYr,2)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!COMPUTE WEATHER ADJUSTMENT FACTORS
DO R=1,mNumCR-2
  HDDFACT(R)=(HDDADJ(CurCalYr,R)/HDDADJ(RECSYear,R))**2.00 !A 10% increase in HDD would increase space heating consumption by 21% (e.g., 1.10^2.00=1.21)
  CDDFACT(R)=(CDDADJ(CurCalYr,R)/CDDADJ(RECSYear,R))**1.50 !A 10% increase in CDD would increase space cooling consumption by 15% (e.g., 1.10^1.50=1.15)
ENDDO

!COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
IF (CURITR.EQ.1) THEN
  DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
ENDIF

!INITIALIZE ARRAYS
TOTEWTN(1:nHeatClasses,1:mNumBldg,1:mNumCR-2)=0.0
WTDEFF(1:nHeatClasses)=0.0
HSHELL(CurCalYr,1:nHeatClasses,1:mNumBldg,1:mNumCR-2)=0.0

TOTEWTNC(CurCalYr,1:nCoolClasses,1:mNumBldg,1:mNumCR-2)=0.0
CSHELL(CurCalYr,1:nCoolClasses,1:mNumBldg,1:mNumCR-2)=0.0

EQWTNCA(CurCalYr,1:nCoolTypes,1:mNumBldg,1:mNumCR-2)=0.0
COOLSHWT(1:nCoolTypes,1:mNumBldg,1:mNumCR-2)=0.0
EQFSHRNC(1:nCoolTypes)=0.0

DO R=1,mNumCR-2   !Major loops to end of share calculations
  DO B=1,mNumBldg

    !VARIABLES USED THIS SECTION:
    !RSMEQP and RSCLASS Variables
    ! RTTYEUPT(EU)   = 0 FOR EU=1 (BEFORE BEGINNING OF FILE)
    ! RTTYEUPT(EU+1) = LAST RECORD # IN SPACE HEATING (EU=1)
    ! RECTY          = RECORD # FROM RSMEQP FILE
    ! EQT            = EQUIPMENT TYPE NUMBER
    ! EQC            = EQUIPMENT CLASS NUMBER
    ! RECCL          = RECORD # FROM RSCLASS FILE
    ! F              = FUEL #

    DO HVRCTY=1,MNUMHVAC   ! for all records in the shell file
      IF (RSCENDIV(HVRCTY).EQ.R .AND. RSBTYPE(HVRCTY).EQ.B) THEN
        ! Heating and cooling types and classes assigned by the building envelope/ shell input file RSMSHL
        HVT=HVHTEQTY(HVRCTY)   ! Number of heating types calculated in RSMESS-- defines efficiency of equipment
        HVC=HVHTEQCL(HVRCTY)   ! Number of heating classes calculated in RSMESS
        HVCT=HVCLEQTY(HVRCTY)  ! Number of cooling types calculated in RSMESS -- defines efficiency of equipment
        HVCC=HVCLEQCL(HVRCTY)  ! Number of cooling classes calculated in RSMESS
        S=HVPACKG(HVRCTY)     ! S = 1 to 5, 1=NoCode, 2=IECC, 3=ENERGY STAR, 4=IECC+40%, 5=PATH; ENERGY STAR-qualified = 3 + 4 + 5
        HS=HTSHEFF(HVRCTY)     ! Heating shell efficiency
        CS=CLSHEFF(HVRCTY)     ! Cooling shell efficiency
        !Filter Shell File for Calendar Year Availability
        IF (CurCalYr.GE.HVFYEAR(HVRCTY).AND.CurCalYr.LE.HVLYEAR(HVRCTY)) THEN
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            !Filter RSMEQP for equipment availability
            IF (CurCalYr.GE.RTINITYR(RECTY).AND. &
             CurCalYr.LE.RTLASTYR(RECTY).AND.RTCENDIV(RECTY).EQ.R) THEN
              EQT=RTEQTYPE(RECTY) !nHeatTypes = # of specific heating types
              EQC=RTTYEQCL(RECTY) !nHeatClasses = # of heating classes
              RECCL=RTCLEUPT(EU)+EQC  !points to record number of heating equipment class in RSCLASS
              F=RTFUEL(RECCL) !fuel for equipment class
              FS=RTFUEL(HVC)   !fuel for shell class

              !RECCL1 maps the space heating equipment class to the appropriate space cooling class
              ! e.g., RECCL=2 (ELEC_HP) maps to RECCL1=13 (ELEC_HP in space cooling section of RSCLASS),
              ! adding number of heating classes(nHeatClasses) to the cooling class for HPs(3), etc.	!TODO - instead of mapping here, read RECCL1 or other pointers/mapping from RSCLASS tab of RSMESS.xlsx
              IF (RECCL.EQ.2) THEN
                RECCL1=13   !nHeatClasses + RTCLEQCL(ELEC_HP) = 10 + 3 = 13  !NoKero
              ELSEIF (RECCL.EQ.9) THEN  !NoKero
                RECCL1=14   !nHeatClasses + RTCLEQCL(GEO_HP) = 10 + 4 = 14  !NoKero
              ELSEIF (RECCL.EQ.10) THEN  !NoKero
                RECCL1=15   !nHeatClasses + RTCLEQCL(NG_HP) = 10 + 5 = 15  !NoKero	!TODO - update if repurposing NG_HP inputs for MS_HP
              ELSE
                RECCL1=12   !central AC (CENT_AIR, RECCL1=12) for all other; room air conditioner (ROOM_AIR, RECCL1=11) not considered here  !NoKero
              ENDIF  !RECCL

              IF (EQT.EQ.HVT) THEN
                !COMPUTE EFFICIENCY FACTORS USED IN COMPUTING OPERATING COST
                ! RTEFFAC(2) is used for the heating component of the shell package
                ! RTEFFAC(3) is used for the cooling component of the shell package
                SHLEVELH(CurCalYr,HVC,S,B,R)=0.0
                EQPEFF(HVT)=RTEQEFF(RECTY)
                IF (RTEQEFF(RECTY) .NE. 0.0) THEN
                  RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
                ELSE
                  RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
                ENDIF
                IF (ACEFF(HVCT,CurCalYr,R) .NE. 0.0) THEN
                  RTEFFAC(3)=RTBASEFF(RECSYear,HVCC)/ACEFF(HVCT,CurCalYr,R)
                ELSE
                  RTEFFAC(3)=RTBASEFF(RECSYear,HVCC)
                ENDIF

                !COMPUTE EFFICIENCY SHELL EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
                HTSHELLFAC(CurCalYr,R,B)=HTSHEFF(HVRCTY)/HTSHBASE(HVRCTY)
                CLSHELLFAC(CurCalYr,R,B)=CLSHEFF(HVRCTY)/CLSHBASE(HVRCTY)

                !COMPUTE SQUARE FOOTAGE EFFECT FOR NEW CONSTRUCTION (CHANGES FROM INITIAL NEW CONTRUCTION VALUE)
                ! These factors are estimated from building simulations for 10% increases in floor area.	!TODO - revisit 10% assumption
                ! The inputs are total effects and thus must be divided by 1.10 to produce a percentage	!TODO - revisit 10% assumption
                ! change in HVAC use per percentage change in floor area.
                HTSQRFOOTFAC(CurCalYr,HVT,R,B)=1.+(((SQRFOOT(CurCalYr,B,R)/SQRFOOT(RECSYear+1,B,R))-1.)*(HVHEATFACTOR(HVRCTY)/1.10))	!TODO - revisit 10% assumption
                CLSQRFOOTFAC(CurCalYr,HVCT,R,B)=1.+(((SQRFOOT(CurCalYr,B,R)/SQRFOOT(RECSYear+1,B,R))-1.)*(HVCOOLFACTOR(HVRCTY)/1.10))	!TODO - revisit 10% assumption

                !COST TREND CALCULATIONS
                ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
                ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
                IF (COSTTRSW.EQ.1) THEN
                  IF (EQT.EQ.HVT) THEN
                    CAPITAL=(EQCOST(RECTY,CurCalYr,"CAP")+SHELCOST(HVRCTY)+ACICOST(HVCT,CurCalYr,R))
                    SHELLSUBSIDY(CurCalYr,RECCL,S,B,R)=SHELSUB(HVRCTY)+SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
                    SHELLSUBSIDY111D(CurCalYr,RECCL,S,B,R)=SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
                  ENDIF
                ELSE
                  IF (EQT.EQ.HVT) THEN
                    CAPITAL=(RTEQCOST(RECTY)+SHELCOST(HVRCTY)+ACICOST(HVCT,CurCalYr,R))
                    SHELLSUBSIDY(CurCalYr,RECCL,S,B,R)=SHELSUB(HVRCTY)+SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
                    SHELLSUBSIDY111D(CurCalYr,RECCL,S,B,R)=SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
                  ENDIF
                ENDIF !Cost Trend Calculations

                !COMPUTE THE PART OF THE EQUIMENT CHOICE CALC NOT DEPENDENT ON REGION OR BUILDING TYPE
                SHELLInvest(CurCalYr,RECCL,S,B,R)=CAPITAL-RTEQCOST(RECTY)-ACICOST(HVCT,CurCalYr,R)

                !CALCULATE OPERATING COST
                HVOPCOST=PRICES(F,R,CurCalYr)*NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL)* &
                 RTEFFAC(2)*HDDFACT(R)*HTSQRFOOTFAC(CurCalYr,HVT,R,B)*HTSHELLFAC(CurCalYr,R,B)+  &
                 EPRICE(R,CurCalYr)*NEWCOOLUEC(B,R)/BASELOAD(RECCL1)*              &
                 RTEFFAC(3)*CDDFACT(R)*CLSQRFOOTFAC(CurCalYr,HVT,R,B)*CLSHELLFAC(CurCalYr,R,B)

                !CALCULATE LIFE CYCLE COSTS
                HVLFCY=CAPITAL+HVOPCOST*DECAY

                !Shell learning for ENERGY STAR-qualified shells (s>2)
                IF (S.GT.2) THEN
                  !ENERGY STAR-qualified (ENERGY STAR, IECC+40%, and PATH)
                  !Allow further shell improvements
                  HVBETA2A(HVRCTY)=HVBETA2(MIN(CurCalYr,ESTARHISTYR),B,S,R)*SHLLEARN(CurCalYr,B,R)
                ELSE
                  !Code homes, non-ENERGY STAR
                  !Code homes DO not get further learned shell improvements
                  HVBETA2A(HVRCTY)=HVBETA2(MIN(CurCalYr,ESTARHISTYR),B,S,R)
                ENDIF  !S>2

                !Benchmark ENERGY STAR shares for each of the nHeatTypes (HVT)
                IF (B.NE.1) THEN
                  !For multifamily and mobile homes, compute shares here
                  HVEQWTN(CurCalYr,HVT,S,B,R)=EXP(HVBETA2A(HVRCTY)+(HVBETA1(MIN(CurCalYr,ESTARHISTYR),B,S,R)*HVLFCY))
                ELSEIF ((B.EQ.1).AND.(CurCalYr.GT.RECSYear)) THEN            ! HVAC Historical	!TODO - Should RECSYear be ESTARHISTYR?
                  !If beyond the historical ENERGY STAR housing share benchmarking period also compute shares here
                  HVEQWTN(CurCalYr,HVT,S,B,R)=EXP(HVBETA2A(HVRCTY)+(HVBETA1(MIN(CurCalYr,ESTARHISTYR),B,S,R)*HVLFCY))
                ENDIF !B<>1

                TOTEWTN(HVC,B,R)=TOTEWTN(HVC,B,R)+HVEQWTN(CurCalYr,HVT,S,B,R)
                EQWTNCA(CurCalYr,HVCT,B,R)=EQWTNCA(CurCalYr,HVCT,B,R)+HVEQWTN(CurCalYr,HVT,S,B,R)
                COOLSHWT(HVCT,B,R)=COOLSHWT(HVCT,B,R)+HVEQWTN(CurCalYr,HVT,S,B,R)*CLSHEFF(HVRCTY)
                TOTEWTNC(CurCalYr,HVCC,B,R)=TOTEWTNC(CurCalYr,HVCC,B,R)+HVEQWTN(CurCalYr,HVT,S,B,R)

              ENDIF  !Calculations for EQT = HVT
            ENDIF  !Filter all RSMEQP records (RECTY) for Current Year and Division
          ENDDO   !DO all RSMEQP records
        ENDIF  !Filter shells for year
      ENDIF  !Filter shells for census division and building type
    ENDDO  !For all shell file records

    !Now that the first pass through the data has been made, raw accumulations
    ! of logit exponents are available for share calculations
    DO HE=1,nHeatTypes      !For all specific heating equipment types
      EQFSHRN(HE)=0.0
      HVEQSHR(CurCalYr,HE,B,R)=0.0
      DO HVRCTY=1,MNUMHVAC  !All shell file records (max 5000)
        IF (RSCENDIV(HVRCTY).EQ.R .AND. RSBTYPE(HVRCTY).EQ.B) THEN
          HVC=HVHTEQCL(HVRCTY)  !nHeatClasses = # of heating classes
          HVCC=HVCLEQCL(HVRCTY) !nCoolClasses = # of cooling classes
          HVT=HVHTEQTY(HVRCTY)  !nHeatTypes = # of specific heating types
          HVCT=HVCLEQTY(HVRCTY) !nCoolTypes = # of specific cooling types
          S= HVPACKG(HVRCTY)    !nShellTypes = # of building envelope/ shell options

          !Filter for year availability
          IF (CurCalYr.GE.HVFYEAR(HVRCTY).AND. CurCalYr.LE.HVLYEAR(HVRCTY) ) THEN
            !Filter for equipment match
            IF (HE.EQ.HVT) THEN  !Ignore records for other specific equipment types
              !SET EQUIPMENT FUEL SHARE (AND NEQTSHR FOR WATER HEATING)
              IF (TOTEWTN(HVC,B,R).GT.0.0) THEN
                HTSHELLWT(CurCalYr,HVT,S,B,R)=HVEQWTN(CurCalYr,HVT,S,B,R)/TOTEWTN(HVC,B,R)
                !Weighted heating shell efficiency for this specific equipment type
                HTSHELLEFFWT(CurCalYr,HVT,S,B,R)= HTSHELLWT(CurCalYr,HVT,S,B,R)*HTSHEFF(HVRCTY)
              ELSE
                HTSHELLEFFWT(CurCalYr,HVT,S,B,R)=0.0
                HTSHELLWT(CurCalYr,HVT,S,B,R)=0.0
              ENDIF

              !Calculate shell efficiency by heating class and accumulate shares
              HSHELL(CurCalYr,HVC,B,R)=HSHELL(CurCalYr,HVC,B,R)+HTSHELLEFFWT(CurCalYr,HVT,S,B,R)
              SHLEVELH(CurCalYr,HVC,S,B,R)=SHLEVELH(CurCalYr,HVC,S,B,R)+HVEQWTN(CurCalYr,HVT,S,B,R)/TOTEWTN(HVC,B,R)

              !For equipment classes with non-zero shares accumulate shares for specific equipment used by shell
              IF (TOTEWTN(HVC,B,R).GT.0.0) THEN
                EQFSHRN(HE)=EQFSHRN(HE)+HVEQWTN(CurCalYr,HVT,S,B,R)/TOTEWTN(HVC,B,R)
              ELSE
                EQFSHRN(HE)=0.0
              ENDIF
              HVEQSHR(CurCalYr,HE,B,R)=EQFSHRN(HE) !Map specific equipment share into the HV array
            ENDIF !Filter for specific equipment match (HVT = HE)
          ENDIF !Filter for current year validity
        ENDIF !Filter for Census Division and Building Type
      ENDDO !All shell file records
    ENDDO !HE, All nHeatTypes for specific heating equipment types

    !CALCULATE WEIGHTED EFFICIENCY AND WEIGHTED SQUARE FOOTAGE FACTOR FOR EACH HEATING EQUIPMENT CLASS FOR USE BELOW
    DO HVT=1,nHeatTypes
      IF (EQPEFF(HVT).GT.0.) EFFWEIGHT(CurCalYr,HVT,B,R)=HVEQSHR(CurCalYr,HVT,B,R)/EQPEFF(HVT)
      !EFFWEIGHT(CurCalYr,HVT,B,R)=HVEQSHR(CurCalYr,HVT,B,R)*EQPEFF(HVT)
      SQFTWEIGHT(CurCalYr,HVT,B,R)=HVEQSHR(CurCalYr,HVT,B,R)*HTSQRFOOTFAC(CurCalYr,HVT,R,B)
      !IF (PRTDBGR.EQ.1) THEN !debug write to unit 9 (RDM_OUT.txt)
        !WRITE(9,'("efficiency_calc: CurCalYr HVT B R HVEQSHR EQPEFF ",4i5,2f12.4)') CurCalYr, HVT, B, R, HVEQSHR(CurCalYr,HVT,B,R), EQPEFF(HVT)
      !ENDIF
    ENDDO
  ENDDO
ENDDO !End census division and building type loop for share calculations

!CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT SPACE HEATING EQUIPMENT
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DENOM =0
      COUNT =0
      !TYPE = INDEX FOR ARRAYS NEQTSHR AND REQTSHR
      ! INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
      ! AND THEN COUNT VALID TYPES IN CURRENT END USE
      !Loop through all equipment records for this end use
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (CurCalYr.GE.RTINITYR(RECTY) &
         .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            IF (RTTYEQCL(RECTY).EQ.EQC) THEN
              !Count valid efficiency levels for this type
              COUNT=COUNT+1
              EQT=RTEQTYPE(RECTY)  !nHeatTypes = # of specific heating types
              RECAR(COUNT)=RECTY   !RSMEQP record number
              EQTAR(COUNT)=EQT     !specific type
              DENOM=DENOM+HVEQSHR(CurCalYr,EQT,B,R)
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFHV(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
        WTEQCSQFHV(CurCalYr,RECCL,B,R)=1.0
      ELSE
        SUM=0.0
        SUM1=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+EFFWEIGHT(CurCalYr,TYPE,B,R)
          SUM1=SUM1+SQFTWEIGHT(CurCalYr,TYPE,B,R)
        ENDDO
        !SYSTEM-WEIGHTED SQUARE FOOTAGE UEC ADJUSTMENT FACTORS
        ! (DERIVED FROM RSMSHL HEATING ADJUSTMENT FACTORS)
        WTEQCSQFHV(CurCalYr,RECCL,B,R)=SUM1/DENOM
        !SYSTEM-WEIGHTED CLASS EFFICIENCY
        WTEQCEFFHV(CurCalYr,RECCL,B,R)=SUM/DENOM
      ENDIF
    ENDDO
  ENDDO
ENDDO

!DONE WITH SPACE HEATING; SWITCH TO SPACE COOLING CALCULATIONS
EU=2  !space cooling

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    DO HC=4,nCoolTypes !skips over room air conditioners (ROOM_AIR); HC numbers may need to change if xlRTEQTYPE changes in RSMEQP tab of RSMESS.xlsx  !CoolTypes !techupdate - update hard-coded value	!TODO - relate HC tests to RTCLTYPT from RSCLASS?
      EQFSHRNC(HC)=0.0
      NEQTSHRC(CurCalYr,HC,B,R)=0.0
      IF (HC.LT.8)               HVCC=2 !CENT_AIR; HC numbers may need to change if xlRTEQTYPE changes in RSMEQP tab of RSMESS.xlsx  !CoolTypes !techupdate - update hard-coded value
      IF (HC.GT.7.AND.HC.LT.12)  HVCC=3 !ELEC_HP; HC numbers may need to change if xlRTEQTYPE changes in RSMEQP tab of RSMESS.xlsx  !CoolTypes !techupdate - update hard-coded value
      IF (HC.GT.11.AND.HC.LT.16) HVCC=4 !GEO_HP; HC numbers may need to change if xlRTEQTYPE changes in RSMEQP tab of RSMESS.xlsx  !CoolTypes !techupdate - update hard-coded value
      IF (HC.EQ.16)              HVCC=5 !NG_HP; HC numbers may need to change if xlRTEQTYPE changes in RSMEQP tab of RSMESS.xlsx  !CoolTypes !techupdate - update hard-coded value
      IF (TOTEWTNC(CurCalYr,HVCC,B,R).GT.0.0) THEN
        CLSHELLWT(CurCalYr,HC,B,R)=COOLSHWT(HC,B,R)/TOTEWTNC(CurCalYr,HVCC,B,R)
      ELSE
        CLSHELLWT(CurCalYr,HC,B,R)=0.0
      ENDIF
      CSHELL(CurCalYr,HVCC,B,R)=CSHELL(CurCalYr,HVCC,B,R)+CLSHELLWT(CurCalYr,HC,B,R)
      IF (TOTEWTNC(CurCalYr,HVCC,B,R).GT.0.0) THEN
        EQFSHRNC(HC)=EQWTNCA(CurCalYr,HC,B,R)/TOTEWTNC(CurCalYr,HVCC,B,R)
      ELSE
        EQFSHRNC(HC)=0.0
      ENDIF
      NEQTSHRC(CurCalYr,HC,B,R)=EQFSHRNC(HC)
    ENDDO !HC

    DO HVCT=4,nCoolTypes !skips over room air conditioners (ROOM_AIR); HC numbers may need to change if xlRTEQTYPE changes in RSMEQP tab of RSMESS.xlsx  !CoolTypes !techupdate - update hard-coded EQT value
      IF (ACEFF(HVCT,CurCalYr,R).GT.0.) EFFWEIGHTC(CurCalYr,HVCT,B,R)=NEQTSHRC(CurCalYr,HVCT,B,R)/ACEFF(HVCT,CurCalYr,R)
      SQFTWEIGHTC(CurCalYr,HVCT,B,R)=NEQTSHRC(CurCalYr,HVCT,B,R)*CLSQRFOOTFAC(CurCalYr,HVCT,R,B)
    ENDDO

    !CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT SPACE COOLING EQUIPMENT
    DO RECCL=RTCLEUPT(EU)+2,RTCLEUPT(EU+1) !process space cooling classes, skipping first RECCL for ROOM_AIR (hence +2)
      EQC = RTCLEQCL(RECCL)
      DENOM = 0
      COUNT = 0

      !TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
      ! INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
      ! AND THEN COUNT VALID TYPES IN CURRENT END USE
      TYPE = RTTYPECT(EU)
      !All records for space cooling in RSMEQP except the first block for ROOM_AIR
      DO RECTY=RTTYEUPT(EU),RTTYEUPT(EU+1) !all records in RSMEQP
        IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            IF (RTTYEQCL(RECTY).EQ.EQC) THEN
              COUNT=COUNT+1
              EQT=RTEQTYPE(RECTY)
              RECAR(COUNT)=RECTY
              EQTAR(COUNT)=EQT
              DENOM=DENOM+NEQTSHRC(CurCalYr,EQT,B,R)
            ENDIF !equipment is a member of this class
          ENDIF !census division filter
        ENDIF !Year availability filter
      ENDDO !All records from RSMEQP for this end use

      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFHV(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
        WTEQCSQFHV(CurCalYr,RECCL,B,R)=1.0
      ELSE
        SUM=0.0
        SUM1=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+EFFWEIGHTC(CurCalYr,TYPE,B,R)
          SUM1=SUM1+SQFTWEIGHTC(CurCalYr,TYPE,B,R)
        ENDDO
        WTEQCSQFHV(CurCalYr,RECCL,B,R)=SUM1/DENOM
        WTEQCEFFHV(CurCalYr,RECCL,B,R)=SUM/DENOM
      ENDIF

      IF (WTEQCEFFHV(CurCalYr,RECCL,B,R).EQ.0.0) THEN
        !SHOULDN'T BE HERE!	!TODO - remove?
        WRITE(9,'("NOTE: should not see this msg!",3i5)') CurCalYr,RECCL,eqt
        WTEQCEFFHV(CurCalYr,RECCL,B,R)= 1/RTBASEFF(RECSYear,RECCL)
      ENDIF
    ENDDO !all classes of equipment for this end use
  ENDDO
ENDDO

END SUBROUTINE RSHVAC


!==============================================================================
! SPACE HEATING CHOICE SUBROUTINE
!  CALLED FOR CurIYr
!==============================================================================
SUBROUTINE RHTRTEC
IMPLICIT NONE

COMMON/TESTHT/HTYSSHR(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR)

!LOCAL VARIABLES
REAL*4 TOTEWTN(nHeatClasses,mNumBldg,mNumCR),TOTEWTR(nHeatClasses,mNumBldg,mNumCR)
REAL*4 EQWTN(nHeatTypes,mNumBldg,mNumCR),EQWTR(nHeatTypes,mNumBldg,mNumCR)
REAL*4 HEATSYS(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR-2),SYSTOT
REAL*4 EQCOST,CAPITAL,RETAIL,CAPITAL1
REAL*4 HDDFACT(mNumCR)
REAL*4 EQFSHRR,EQFSHRN,OPCOST(3),BLDRWT !OPCOST(3) represents 1)replacement equipment for housing unit existing in RECS year, 2)new equipment in post-RECS-built housing unit, and 3)replacement equipment in post-RECS-built housing unit	!TODO - should EQFSHRR and EQFSHRN be declared with(nHeatTypes), or is that no longer necessary because they've already been populated once?
REAL*4 RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2,e
REAL*4 HTYSSHR,HSYSTOT,LAGFACTOR,tmplogit
!These variables are involved in the efficiency choice calculation	!TODO - clarify comment?
! RECAR and EQTAR are dimensioned for the number of choices across efficiency types in any single year	!TODO - clarify comment?
INTEGER EU,EUPR,RECTY,RECCL,R,B,F,EQT,EQC,TYPE,COUNT,L
INTEGER RECAR(nHeatTypes),EQTAR(nHeatTypes)  !EqpParam

!********************************************************************
! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES	!TODO - combine/optimize similar code
!********************************************************************

EU=1  !space heating
EUPR=1
ALPHA1=-0.50
BLDRWT=6.0 !FACTOR TO DISCOUNT FUEL PRICE IMPACT IN BUILDERS' FUEL CHOICE DECISION	!TODO - Revise? Source?

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!COMPUTE HDDFACT
DO R=1,mNumCR-2
  HDDFACT(R)=(HDDADJ(CurCalYr,R)/HDDADJ(RECSYear,R))**2.00 !A 10% increase in HDD would increase space heating consumption by 21% (e.g., 1.10^2.00=1.21)
ENDDO

!COMPUTE DECAY RATE USED TO COMPUTE LIFE-CYCLE COST (FIRST ITERATION ONLY)
IF (CURITR.EQ.1) THEN
  DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
ENDIF

IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.EQ.RECSYear+1)) THEN  !AppBetaData File Initialization/header
  OPEN(unit = 661, file = "AppBetaData.txt") !creates the file
    WRITE(661,61) 'Betacodes, EU,EQC,EQT,B,R,CurCalYr,RECTY,TYPE,RECCL,EQWTN(EQT;B;R),EQWTR(EQT;B;R),ECTEMP,BETA1DR(RECTY),CAPITAL,RTECBIAS(RECTY),OPCOST(1),OPCOST(2),OPCOST(3),RTECBTA2(RECTY),RTECBTA3(RECTY),LFCY(EQT;B;R;1),LFCY(EQT;B;R;2),TOTEWTR(EQC;B;R),TOTEWTN(EQC;B;R),NEQTSHR(CurCalYr;TYPE;B;R),EQFSHRR,REQTSHR(CurCalYr;TYPE;B;R),EQCADD(Y;RECCL;B;r),HEATINGTYPEPURCH(Y;TYPE;B;R;1),HEATINGTYPEPURCH(Y;TYPE;B;R;2),EQCREP(Y;RECCL;B;r),EQCRP90RP(Y;RECCL;B;r),EQCRP90(Y;RECCL;B;r),TCWSHR(B;R),FCWSHR(B;R),FCW_SHR,TCW_SHR,UPSHR(B;R)'  !RAD writes the header; BetaCodeSection = radref
    61 FORMAT(a) !formats the header
  CLOSE(661) !closing it, so that it can be reopened with an "append" function, hoping that each time this iterates, as new variables are calculated it adds to the bottom instead of replacing
ENDIF !Appbetadata file initialization complete

!IF (CURITR .eq. 1) THEN	!TODO - is this block of code still needed?
!  OPEN(unit = 667, file = "pv_hh.txt",position = "append") !creates the file
!    WRITE(667,93) "if", ",", "year", ",", "cd", ",", "xExogPen", ",", "exogpvmistie", ",", "cap", ",", "units", ",", "xcalckw", ",", "xinvest", ",", "x111d", ",", "trills"
!    93 FORMAT(       A,   A,      A,   A,    A,   A,  A,A,            A,   A,     A,   A,       A,   A,         A,   A,         A,   A,       A,   A, A)
!  CLOSE(667) !closing it, so that it can be reopened with an "append" function, hoping that each time this iterates, as new variables are calculated it adds to the bottom instead of replacing
!ENDIF

DO R=1,mNumCR-2
  DO B=1,mNumBldg

    !VARIABLES USED THIS SECTION:
    ! RSMEQP and RSCLASS Variables
    ! RTTYEUPT(EU)   = 0 FOR EU=1 (BEFORE BEGINNING OF FILE)
    ! RTTYEUPT(EU+1) = LAST RECORD # IN END USE 1 (SPACE HEATING)
    ! RECTY          = RECORD # FROM RSMEQP.TXT FILE
    ! EQT            = EQUIPMENT TYPE NUMBER FROM RSMEQP.TXT FILE
    ! EQC            = EQUIPMENT CLASS NUMBER FROM RSMEQP.TXT FILE
    ! RECCL          = RECORD # FROM RSCLASS.TXT FILE
    ! F              = FUEL # FROM RSCLASS.TXT FILE
    ! RTEQEFF(RECTY) = SPECIFIC EQUIPMENT EFFICIENCY FROM RSMEQP.TXT FILE
    ! EQCEFF(Y,RECCL)= FORECAST RETIRING EFFICIENCY FROM RSEFF01.TXT (computed in vintaging workbook)
    ! RTBASEFF(RECSYear,RECCL) = AVERAGE STOCK EFFICIENCY FROM RSCLASS.TXT FILE
    ! BASELOAD (RECCL) = STANDARD LEVEL EFFICIENCY FOR HVAC (THROUGH RECCL=nHeatClasses+nCoolClasses=16) FROM RSUECSHL.TXT

    !INITIALIZE ARRAYS
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      TOTEWTR(RECCL,B,R)=0.0
      TOTEWTN(RECCL,B,R)=0.0
    ENDDO

    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1) !for all RSMEQP records this end use
      !Filter for year availability
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        !Filter for census division
        IF (RTCENDIV(RECTY).EQ.R) THEN
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          F=RTFUEL(RECCL)

          !COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
          IF (RTEQEFF(RECTY).NE.0.0) THEN
            !RTEFFAC(1) is used to adjust UECs for replacements from the original stock of equipment from RECSYear
            RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)  !eqceff is retiring stock efficiency
            !RTEFFAC(2) is used to adjust RECSYear UECs for new construction decisions
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)  !rtbaseff is stock efficiency at RECSYear
          ELSE !message to RDM_OUT.txt (unit 9)
            WRITE(9,'("WARNING:shouldnt_see",3i5,3e15.4)') CurCalYr,RECCL,eqt,eqceff(CurCalYr,RECCL),rteqeff(recty),rtbaseff(RECSYear,RECCL)
            RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
          ENDIF

          !SET CAPITAL COSTS
          ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
          ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
          IF (COSTTRSW.EQ.1) THEN
            CAPITAL =  EQCOST(RECTY,CurCalYr,"CAP")
            IF ((EQC.NE.2).AND.(EQC.NE.9).AND.(EQC.NE.10)) THEN !ELEC_HP, GEO_HP, NG_HP  !NoKero !EqpParam	!TODO - use RTTYNAME in RSCLASS instead, or add variable to indicate what techs are heat pumps?
              !If not a HP technology, then add typical retail cost for central air conditioning
              CAPITAL1= RTRECOST(RECTY)+2760.  !updated using March 2023 tech report (using Central AC South 2020 installed base retail equipment cost in RTEKDOLLARYR dollars)  !ACcost
            ELSE
              CAPITAL1= RTRECOST(RECTY)
            ENDIF
          ELSE
            CAPITAL =  RTEQCOST(RECTY)
            IF ((EQC.NE.2).AND.(EQC.NE.9).AND.(EQC.NE.10)) THEN !ELEC_HP, GEO_HP, NG_HP  !NoKero !EqpParam	!TODO - use RTTYNAME in RSCLASS instead, or add variable to indicate what techs are heat pumps?
              !If not a HP technology, then add typical retail cost for central air conditioning
              CAPITAL1= RTRECOST(RECTY)+2760.  !updated using March 2023 tech report (using Central AC South 2020 installed base retail equipment cost in RTEKDOLLARYR dollars)  !ACcost
            ELSE
              CAPITAL1= RTRECOST(RECTY)
            ENDIF
          ENDIF

          !CHANGE BETA1 TO REFLECT PRICE-INDUCED BEHAVIOR CHANGES
          ! i.e., reduce implicit discount rates as real prices increase
          IF ((CurCalYr.GT.2008).AND. &	!TODO - 2008 marks last year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)? Remove legacy energy bill code as necessary
           (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
            HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
            ELIGBLE=HRDRATE - 0.07	!TODO - source of 7% assumption? Revise?
            IF (ELIGBLE.GT.0.0) THEN
              HRDADJ= ELIGBLE * &
               ((PRICES(F,R,CurCalYr)/PRICES(F,R,RECSYear))**ALPHA1)
              BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
            ELSE
              BETA1DR(RECTY)=RTECBTA1(RECTY)
            ENDIF
          ELSE
            BETA1DR(RECTY)=RTECBTA1(RECTY)
          ENDIF

          !COMPUTE THE PART OF THE EQUIMENT CHOICE WEIGHT NOT DEPENDENT ON REGION AND BUILDING TYPE
          ECTEMP = RTECBIAS(RECTY) + (BETA1DR(RECTY)*CAPITAL)	!TODO - reincorporate into EQWTN and EQWTR equations below (similar to other end uses)? There are a few EUs that use this ECTEMP variable, the math ends up being the same, so it would be easy to incorporate back 

          !CALCULATE OPERATING COST FOR THREE DECISION TYPES:
          ! UECS: EQCUEC = RECSYear UEC FROM RSUEC.TXT
          ! NEWHEATUEC = NEW UECS READ IN FROM RSHLUEC.TXT NOT YEAR DEPENDENT, SO ADJUST BELOW
          ! EQCAHVUEC = AVERAGE? !TODO - review

          IF (CurCalYr.EQ.RECSYear+1) THEN
            !prices x original RECS UEC x efficiency adjustment x hddadj
            OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)*HDDFACT(R)

            !FOR BUILDER CHOICE IN NEW CONSTRUCTION: DILUTE OPCOST USING BLDRWT
            !prices x new construction UEC from RSUECSHL.txt / standard efficiency x basestock efficiency / specific equipment efficiency x hddadj / adj for builder
            OPCOST(2)=PRICES(F,R,CurCalYr)*(NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL))*RTEFFAC(2)*HDDFACT(R)/BLDRWT
            OPCOST(3)=PRICES(F,R,CurCalYr)*NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL)*RTEFFAC(2)*HDDFACT(R)
          ELSE
            OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)*HDDFACT(R)*(EHSHELL(CurCalYr-1,F,R,B)/EHSHELL(RECSYear,F,R,B))
            OPCOST(2)=PRICES(F,R,CurCalYr)*(NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL))*RTEFFAC(2)*HDDFACT(R)/BLDRWT
            OPCOST(3)=PRICES(F,R,CurCalYr)*EQCAHVUEC(CurCalYr-1,RECCL,b,r)*(AHSHELL(CurCalYr-1,F,R,B)/EHSHELL(RECSYear,F,R,B))*RTEFFAC(2)*HDDFACT(R)
          ENDIF

          !CALCULATE LIFE-CYCLE COSTS
          LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1)*DECAY)   !Replacement choice (EQWTR) for homeowner of house existing in RECS year (equipment costs + installation costs)
          LFCY(EQT,B,R,2)=CAPITAL1 + (OPCOST(2)*DECAY)  !New construction choice for builder, counting only a fraction of operating costs in the decision,
                                                        ! thus favoring a choice toward equipment with lower first costs (also builder does incur different
                                                        ! installation costs than the homeowner does)
          LFCY(EQT,B,R,3)=CAPITAL1 + (OPCOST(3)*DECAY)  !Replacement choice (EQWTN) for homeowner in post-RECS added house

          !COMPUTE WEIGHTS FOR REPLACEMENT EQUIPMENT TYPES
          EQWTR(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(1)) + (RTECBTA3(RECTY)*LFCY(EQT,B,R,1)))
          TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

          !COMPUTE WEIGHTS FOR POST-RECS YEAR REPLACEMENT(?) EQUIPMENT TYPES
          !OPCOST(3) represents 1)replacement equipment for housing unit existing in RECS year, 2)new equipment in post-RECS-built housing unit, and 3)replacement equipment in post-RECS-built housing unit
          EQWTN(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(3)) + (RTECBTA3(RECTY)*LFCY(EQT,B,R,3)))	!TODO - Clarify? Other major end uses (except refrigeration, freezing, and lighting) use OPCOST(2) rather than OPCOST(3)
          TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 1
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,62)'1,',EU, ',',EQC, ',','X,',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,',EQWTN(EQT,B,R),',',EQWTR(EQT,B,R),',',ECTEMP,',',BETA1DR(RECTY), ',',CAPITAL,',','X,',OPCOST(1),',','X,',OPCOST(3),',',RTECBTA2(RECTY),',',RTECBTA3(RECTY),',',LFCY(EQT,B,R,1),',','X,',TOTEWTR(EQC,B,R),',',TOTEWTN(EQC,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              62 FORMAT(a,I,a,I,a,a,I,a,I,a,I,a,I,a,a,a,f,a,f,a,f,a,f,a,  f,a,  a,  f,a,a,  f,a,f,a,f,a,f,a,a,  f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF
        ENDIF !filter census division
      ENDIF !filter year availability
    ENDDO !for all RSMEQP records this end use

    !*******************************************************************
    !CALCULATE NEW AND REPLACEMENT MARKET SHARES
    !*******************************************************************
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !Filter for year availability
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        !Filter for census division
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          !SET EQUIPMENT CLASS (EQC) & EQUIPMENT TYPE (EQT)
          EQC=RTTYEQCL(RECTY)
          EQT=RTEQTYPE(RECTY)

          !SET NEW EQUIPMENT FUEL SHARES (AND NEQTSHR FOR WATER HEATING)
          IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
            EQFSHRN=EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R)
          ELSE
            EQFSHRN=0.0
          ENDIF
          NEQTSHR(CurCalYr,TYPE,B,R)=EQFSHRN

          !SET REPLACEMENT EQUIPMENT FUEL SHARES (AND NEQTSHR FOR WATER HEATING)
          IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
            EQFSHRR=EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R)
          ELSE
            EQFSHRR=0.0
          ENDIF

          REQTSHR(CurCalYr,TYPE,B,R)=EQFSHRR

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 2
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,63) '2,',EU, ',',EQC, ',','X,',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',',EQFSHRR,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              63 FORMAT(a,I,a,I,a,a,  I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    !CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT SPACE HEATING EQUIPMENT
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DENOM=0
      DENOM2=0
      COUNT =0
      TYPE = RTTYPECT(EU) !initialize to last equipment record, previous end use
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            TYPE=TYPE+1      !counting valid types
            IF (RTTYEQCL(RECTY).EQ.EQC) THEN
              COUNT=COUNT+1
              EQT=RTEQTYPE(RECTY)
              RECAR(COUNT)=RECTY
              EQTAR(COUNT)=TYPE
              DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
              DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
      ENDIF

      !Based on IF statement above should never be here!	!TODO - verify
      IF (WTEQCEFFN(CurCalYr,RECCL,B,R).EQ.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ENDIF

      !COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
      IF (DENOM2.LE.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
      ENDIF
      IF (WTEQCEFFR(CurCalYr,RECCL,B,R).EQ.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ENDIF
    ENDDO

    !COMPUTE LOGIT VALUES FOR EACH SPACE HEATING SYSTEM AND SUM OVER TYPE
    SYSTOT=0.0
    LAGFACTOR=0.9	!TODO - Update? Source?

    !COMPUTE PERCENT ELIGIBLE FOR FUEL CHOICE SIMULATION
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQT=RTCLTYPT(RECCL)
      TYPE = RTTYPECT(EU) + EQT	!TODO - not used here
      tmplogit=EXP( RTFCBIAS(RECCL,B,R)+ RTFCBETA(RECCL)*LFCY(EQT,B,R,2) ) !same as second half of HEATSYS equation below; lifecycle cost for new construction
      HEATSYS(CurCalYr,EQT,B,R)=LAGFACTOR*HEATSYS(CurCalYr-1,EQT,B,R) &
       +(1-LAGFACTOR)*EXP( RTFCBIAS(RECCL,B,R)+ RTFCBETA(RECCL)*LFCY(EQT,B,R,2) )
      SYSTOT=SYSTOT+HEATSYS(CurCalYr,EQT,B,R)

      !Diagnostics only: !Write to unit 9 (RDM_OUT.txt) to verify
      IF ((PRTDBGR.EQ.1) .AND. (CurCalYr.GT.RECSYear) .AND. (B.EQ.1)) THEN	!TODO - delete this entire section diagnostic?
        !WRITE(9,'("HTSHR_parm_checks(SF): CurCalYr B R EQC EQT HEATSYS(y-1) tmplogit HEATSYS HSYSSHR ",5i5,4e15.4)') CurCalYr, B, R, EQC, EQT, HEATSYS(CurCalYr-1,EQT,B,R), tmplogit, HEATSYS(CurCalYr,EQT,B,R), HSYSSHR(CurCalYr,EQC,B,R)
        !WRITE(9,'("tmplogit_parm_checks(SF): CurCalYr B R EQC EQT RTFCBIAS RTFCBETA LFCY ",5i5,3e15.4)') CurCalYr, B, R, EQC, EQT, RTFCBIAS(RECCL,B,R), RTFCBETA(RECCL), LFCY(EQT,B,R,2)
      ENDIF
    ENDDO

    !COMPUTE NORMALIZED SHARES FOR EACH FUEL SYSTEM CHOICE
    HSYSTOT=0.0
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQT=RTCLTYPT(RECCL)
      IF (SYSTOT.NE.0.0) THEN
        HTYSSHR(CurCalYr,EQC,B,R)=HEATSYS(CurCalYr,EQT,B,R)/SYSTOT
      ELSE
        HTYSSHR(CurCalYr,EQC,B,R)=0.0
      ENDIF

      !Use space heating equipment shares in historical years as shown in RSHTSHR.txt input file; otherwise, use calculated value
      IF (CurCalYr.GT.HTSHRYR) HSYSSHR(CurCalYr,EQC,B,R)=HTYSSHR(CurCalYr,EQC,B,R)

      !Diagnostics only: !Write to unit 9 (RDM_OUT.txt) to verify
      IF ((PRTDBGR.EQ.1) .AND. (CurCalYr.GT.HTSHRYR) .AND. (B.EQ.1)) THEN
        !WRITE(9,'("HTSHR_system_shares(SF): CurCalYr B R EQC EQT HTYSSHR HTYSSHR(y-1) HTYSSHR(y-2) ",5i5,3e15.4)') CurCalYr, B, R, EQC, EQT, HTYSSHR(CurCalYr,EQC,B,R), HTYSSHR(CurCalYr-1,EQC,B,R), HTYSSHR(CurCalYr-2,EQC,B,R)
      ENDIF
    ENDDO

  ENDDO  !B
ENDDO  !R

END SUBROUTINE RHTRTEC


!==============================================================================
! SPACE HEATING ADDED/REPLACED SUBROUTINE
!==============================================================================
SUBROUTINE RHTRADD
IMPLICIT NONE

REAL*4 SHARESN(nHeatTypes,mNumBldg,mNumCR)
REAL*4 SWT(RECSYear:EndYr),SWF(RECSYear:EndYr),SA, HSR, ESR, SVRTE
REAL*4 NEWSHELLWT(RECSYear:EndYr,6,mNumBldg,mNumCR),NEWADDWT(RECSYear:EndYr,6,mNumBldg,mNumCR) !dimensioned by the number of fuels reflected in RSCLASS	!TODO - review
INTEGER EQT,RECTY,TYPE,COUNT,L,EQCAR(10),RECCLSW,V	!TODO - replace 10 with parameter?
INTEGER EU,EQC,RECCL,Y,R,B,TEMP,F,FSW,NUMEQT,T,S,Y1,E,D

EU = 1 !space heating

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    !WRITE(6,'("TESTING:CurCalYr_R_B_HSEADD ",3i4,f10.2)') CurCalYr, B, R, HSEADD(CurCalYr,B,R)	!TODO - remove
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      !Initialize arrays
      EQC=RTCLEQCL(RECCL)
      SHARESN(EQC,B,R)=0.0
      EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
      EQCSR90(CurCalYr,RECCL,B,R)=0.0
      EQCSUR(CurCalYr,RECCL,B,R)=0.0
      IF (B.EQ.1) EQCREP(CurCalYr,RECCL,B,R) = 0.0
    ENDDO

    !Aggregate heating equipment shares by the nHeatClasses # of general heating classes in RSCLASS
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          EQC=RTTYEQCL(RECTY)
          EQT=RTEQTYPE(RECTY)
          SHARESN(EQC,B,R)=HSYSSHR(CurCalYr,EQC,B,R)
        ENDIF
      ENDIF
    ENDDO

    !CALCULATE SPACE HEATING EQUIPMENT ADDED IN PREVIOUS YEAR (CurIYr-1)
    ! CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS YEAR VINTAGE PRIOR TO PREVYR
    !CUMULATE SURVIVING NEW HEATERS ADDED PRIOR TO PREVYR TO ESTIMATE NH
    ! SA REPRESENTS NH IN PREVYR-1
    ! CUMULATE SURVIVING NEW HEATERS ADDED & REPLACED PRIOR TO PREVYR
    ! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) -  SURV.EQUIP(EQCSUR)
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !process all classes this end use
      EQC=RTCLEQCL(RECCL)
      EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*SHARESN(EQC,B,R))
      SA=0.0

      !Calculate replacement equipment from original RECS-year stock
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
         *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-RECSYear)))
      ELSE
        EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
         EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-RECSYear)))
      ENDIF

      !COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
           -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
        ENDDO
      ENDIF

     !CALCULATE SURVIVING REPLACEMENTS
     IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1 !loop previous years
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
          HSR=HDR(B)**(TEMP)
          SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
          EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
           (((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR))))
        ENDDO !loop previous years
      ENDIF

      !************************************************************
      ! CALCULATE REPLACEMENT SPACE HEATERS FOR NEW VINTAGE IN CurIYr-1
      !  NOTE: REPLACES LIKE-FOR-LIKE IN MULTIFAMILY AND MOBILE HOMES
      !  NOTE: FOR NEW HOUSES (NH) - CurIYr-1 IS THE LAGGED VALUE
      !************************************************************

      !SUBROUTINE 'REPLACE' DISTRIBUTES REPLACEMENTS IN POST-RECS-YEAR SINGLE-FAMILY HOMES WHEN LAST ARGUEMENT = 1
      IF (B.EQ.1) THEN  !single-family homes only
        !First, store what replacements would have been if no switching allowed
        OEQCREP(CurCalYr,RECCL,1,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
        !Call REPLACE to distribute replacements
        CALL REPLACE(EU,R,B,RECCL,1)
      ELSE
        !No fuel/technology switching allowed in multifamily or mobile homes
        EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
      ENDIF

    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

!SUBROUTINE 'REPLACE' DISTRIBUTES REPLACEMENTS IN EXISTING SINGLE-FAMILY HOMES WHEN LAST ARGUMENT = 2
B = 1
DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    OEQCRP90(CurCalYr,RECCL,B,R) = EQCRP90(CurCalYr,RECCL,1,R)
    OEQCRP90R(CurCalYr,RECCL,B,R) = EQCRP90RP(CurCalYr,RECCL,1,R)
    CALL REPLACE(EU,R,B,RECCL,2)
  ENDDO
ENDDO

DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    SWITCHTO(CurCalYr,RECCL,B,R)=0.0
    SWITCHTOR(CurCalYr,RECCL,B,R)=0.0
    DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      IF (RECCLSW.NE.RECCL) THEN
        SWITCHTO(CurCalYr,RECCL,B,R) = SWITCHTO(CurCalYr,RECCL,B,R) + EQCSW90(CurCalYr,RECCLSW,RECCL,B,R)
        SWITCHTOR(CurCalYr,RECCL,B,R) = SWITCHTOR(CurCalYr,RECCL,B,R) + EQCSW90R(CurCalYr,RECCLSW,RECCL,B,R)
      ENDIF
    ENDDO
  ENDDO
ENDDO

DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQCRP90(CurCalYr,RECCL,B,R) = EQCRP90(CurCalYr,RECCL,B,R) - SWITCHES(CurCalYr,RECCL,B,R)
    EQCRP90RP(CurCalYr,RECCL,B,R) = EQCRP90RP(CurCalYr,RECCL,B,R) - SWITCHESR(CurCalYr,RECCL,B,R) + &
     SWITCHTOR(CurCalYr,RECCL,B,R) + SWITCHTO(CurCalYr,RECCL,B,R)
  ENDDO
ENDDO

SWF(CurCalYr)=0.0
SWT(CurCalYr)=0.0

DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    SWT(CurCalYr)=SWT(CurCalYr)+SWITCHTO(CurCalYr,RECCL,B,R)+SWITCHTOR(CurCalYr,RECCL,B,R)
    SWF(CurCalYr)=SWF(CurCalYr)+SWITCHES(CurCalYr,RECCL,B,R)+SWITCHESR(CurCalYr,RECCL,B,R)
    SWTOTAL(CurCalYr,RECCL,R)= SWITCHTO(CurCalYr,RECCL,B,R)+SWITCHTOR(CurCalYr,RECCL,B,R)
    SWFTOTAL(CurCalYr,RECCL,R)=SWITCHES(CurCalYr,RECCL,B,R)+SWITCHESR(CurCalYr,RECCL,B,R)
  ENDDO
ENDDO

DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) ! Sums equipment in existing RECS-year households
    EQCND90(CurCalYr,RECCL,B,R) = EQCRP90(CurCalYr,RECCL,B,R) + EQCESE(CurCalYr,RECCL,B,R) + &
     EQCSR90(CurCalYr,RECCL,B,R)+ EQCRP90RP(CurCalYr,RECCL,B,R) 
  ENDDO
ENDDO

!Initialize new shell variables
NHSHELL(CurCalYr, 1:NHTRFL, 1:(mNumCR-2), 1:mNumBldg) = 0.0
NEWSHELLWT(CurCalYr, 1:NHTRFL, 1:mNumBldg, 1:(mNumCR-2)) = 0.0
NEWADDWT(CurCalYr, 1:NHTRFL, 1:mNumBldg, 1:(mNumCR-2)) = 0.0

!Compute Shell Investment
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      IF (RECCL.LE.2.OR.RECCL.EQ.9) F=4 !ELEC_RAD, ELEC_HP, and GEO_HP  !NoKero	!TODO - instead of mapping like this, use F=RTFUEL from RSCLASS; otherwise, move RECCL.EQ.10 here when replacing NG_HP with MS_HP
      IF (RECCL.EQ.3.OR.RECCL.EQ.4.OR.RECCL.EQ.10) F=3 !NG_FA, NG_RAD, and NG_HP  !NoKero
      IF (RECCL.EQ.5) F=2 !LPG_FA  !NoKero
      IF (RECCL.EQ.6.OR.RECCL.EQ.7) F=1 !DIST_FA and DIST_RAD  !NoKero
      IF (RECCL.EQ.8) F=5 !WOOD_HT  !NoKero

      EQC=RTCLEQCL(RECCL)

      HEATOT(CurCalYr,EQC,B,R)=EQCESE(CurCalYr,RECCL,B,R)+ &
       EQCRP90(CurCalYr,RECCL,B,R)+EQCSR90(CurCalYr,RECCL,B,R)+ &
       EQCADD(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R)+ &
       EQCSUR(CurCalYr,RECCL,B,R)+EQCRP90RP(CurCalYr,RECCL,B,R)

      NEWSHELLWT(CurCalYr,F,B,R)=NEWSHELLWT(CurCalYr,F,B,R)+ &
       EQCADD(CurCalYr,RECCL,B,R)*HSHELL(CurCalYr,RECCL,B,R)
      NEWADDWT(CurCalYr,F,B,R)=NEWADDWT(CurCalYr,F,B,R)+EQCADD(CurCalYr,RECCL,B,R)
    ENDDO
  ENDDO
ENDDO

!COMPUTE SHELL AVERAGE FOR EACH FUEL TYPE
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO F=1,NHTRFL  !distillate fuel oil, propane, natural gas, electricity, wood  !NoKero
      IF (NEWADDWT(CurCalYr,F,B,R).GT.0.0) THEN
        NHSHELL(CurCalYr,F,R,B)=NEWSHELLWT(CurCalYr,F,B,R)/NEWADDWT(CurCalYr,F,B,R)
      ELSE
        NHSHELL(CurCalYr,F,R,B)=1.0
      ENDIF
      NHSHELL(RECSYear,F,R,B)=NHSHELL(RECSYear+1,F,R,B)
    ENDDO
  ENDDO
ENDDO

!Adjustments to house size
! Cooling adjustments in positions 3 & 4 done in RCLADD	!TODO - replace with parameters? 
! Values 1, 2, and 5 refer to column indexes for ELASTIC in RSMISC.txt	!TODO - replace with parameters? 
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    !fossil fuel heating
    EXSQFTADJ(CurCalYr,B,D,1)=(ELASTIC(1,D)*((EXSQRFOOT(CurCalYr,B,D)-EXSQRFOOT(RECSYear,B,D))/ &
     EXSQRFOOT(RECSYear,B,D))*NHSHELL(CurCalYr,3,D,B))+1
    !electric heating
    EXSQFTADJ(CurCalYr,B,D,2)=(ELASTIC(2,D)*((EXSQRFOOT(CurCalYr,B,D)-EXSQRFOOT(RECSYear,B,D))/ &
     EXSQRFOOT(RECSYear,B,D))*NHSHELL(CurCalYr,4,D,B))+1
    !furnace fans
    EXSQFTADJ(CurCalYr,B,D,5)=(ELASTIC(5,D)*((EXSQRFOOT(CurCalYr,B,D)-EXSQRFOOT(RECSYear,B,D))/ &
     EXSQRFOOT(RECSYear,B,D))*NHSHELL(CurCalYr,3,D,B))+1
  ENDDO
ENDDO

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DO Y=CurCalYr,EndYr
        !VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
        TEMP=Y-CurCalYr
        HSR=HDR(B)**(TEMP)
        ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
        EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!AGGREGATE SPACE HEATING SYSTEMS FOR INVESTMENT ANALYSIS
!********************************************************************
T=CurCalYr
Y=CurIYr
NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

DO B=1,mNumBldg
  DO r=1,mNumCR-2
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1  !INDEX to count the 'TYPE' records in RSMEQP
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          HEATINGTYPEPURCH(T,EQT,B,R,1)=(HVEQSHR(T,EQT,B,r)*EQCADD(T,RECCL,B,r))
          HEATINGTYPEPURCH(T,EQT,B,R,2)=(NEQTSHR(T,EQT,B,r)*(EQCREP(T,RECCL,B,r) + &
           EQCRP90RP(T,RECCL,B,r)) + REQTSHR(T,EQT,B,r)*EQCRP90(T,RECCL,B,r) )
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 8
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,69) '8,',EU, ',',T,',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',HVEQSHR(T,EQT,B,r), ',',EQCADD(T,RECCL,B,r), ',',HEATINGTYPEPURCH(T,EQT,B,R,1), ',', HEATINGTYPEPURCH(T,EQT,B,R,2), ',',EQCREP(T,RECCL,B,r), ',',EQCRP90RP(T,RECCL,B,r), ',',EQCRP90(T,RECCL,B,r), ',','X,','X,','X,','X,',REQTSHR(T,EQT,B,r)
              69 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  f)
            CLOSE(661)
          ENDIF
          DO S=1,nShellTypes
            SHELLBUILDS(T,EQT,S,B,R)=HTSHELLWT(T,EQT,S,B,R)*EQCADD(T,RECCL,B,r)
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!Natural gas space heating equipment defined in RSCLASS (e.g., furnaces, boilers, heat pumps) are proxy for number of natural gas customers	!TODO - Remove ",heat pumps" if replacing NG_HP inputs with MS_HP
! RSGASCUST tracks the number of natural gas customers by looking across end uses (i.e., if number of natural gas water heaters, cooking ranges,
!  or clothes dryers exceeds number of space heaters, that value becomes the number of natural gas customers for that year/census division/building type
! Note: this is not a constraint on hookups...

!Initialize values
RSGASCUST(CurCalYr,1:mNumCR-2)=0.0

IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
  DO R=1,mNumCR-2
    RSGASCUST(RECSYear,R)=0.0
    DO B=1,mNumBldg
      RSGASCUST(RECSYear,R)=RSGASCUST(RECSYear,R)+EQCESE(RECSYear,3,B,R)+EQCESE(RECSYear,4,B,R)+EQCESE(RECSYear,10,B,R) !NG_FA + NG_RAD + NG_HP  !NoKero	!TODO - Remove +EQCESE(RECSYear,10,B,R) if replacing NG_HP inputs with MS_HP
    ENDDO
  ENDDO
ELSE
  DO R=1,mNumCR-2
    DO B=1,mNumBldg
      RSGASCUST(CurCalYr,R)=RSGASCUST(CurCalYr,R)+HEATOT(CurCalYr,3,B,R)+HEATOT(CurCalYr,4,B,R)+HEATOT(CurCalYr,10,B,R) !NG_FA + NG_RAD + NG_HP  !NoKero	!TODO - Remove +HEATOT(CurCalYr,10,B,R) if replacing NG_HP inputs with MS_HP
    ENDDO
  ENDDO
ENDIF

END SUBROUTINE RHTRADD


!==============================================================================
! SPACE HEATING CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE RHTRCON
IMPLICIT NONE

REAL*4 NFANUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),EFANUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 ENUMER(mNumCR,mNumBldg), EDENOM(mNumCR,mNumBldg), &
       NDENOM(mNumCR,mNumBldg),ANUM(mNumFuel,mNumCR,mNumBldg), &
       ADEN(mNumFuel,mNumCR,mNumBldg), NNUMER(mNumCR,mNumBldg)
REAL*4 HDDFACT(mNumCR),TEMP,TEMP1,TEMP2, NFANIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 FANEFF(RECSYear:EndYr),NFANEFF(RECSYear:EndYr),ALPHA,ef1,ef2,ef3,alpha2,rbn,rbr,rba,AFANUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
INTEGER F, F2, FCON, Q, V, YEAR,FAN,E,T,y1
INTEGER EU,EUPR,EQC,RECCL,Y,R,B,EQCEHP,EQCGHP,D,S
INTEGER RECCLEHP, RECCLGHP

!PRICES 1=Distillate Fuel Oil 2=Propane 3=Natural Gas 4=Electricity
!SHELL 1=Distillate Fuel Oil/Wood 2=Propane 3=Natural Gas 4=Electricity
!********************************************************************
! F    = FUEL NUMBER FROM RSCLASS FILE
! FCON = FUEL NUMBER FOR CONSUMPTION (AS FOLLOWS):
!        1=Natural Gas 2=Electricity 3=Distillate Fuel Oil + Kerosene 4=Propane 5=Wood
!********************************************************************

EU = 1  !space heating
EUPR = 1
ALPHA = -.15; ef1 = .5; ef2 = .35; ef3 = .15 !own-price elasticity and distribution
ALPHA2 = -.05  !heating shell adjustment

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!Apply weather elasticity factor for space heating
DO R=1,mNumCR-2
  IF (CurCalYr.LE.IJUMPCALYR) THEN
    HDDFACT(R)=(HDDADJ(CurCalYr,R)/HDDADJ(RECSYear,R))**2.00 !A 10% increase in HDD would increase space heating consumption by 21% (e.g., 1.10^2.00=1.21)	!TODO - review adjustment factor (**2.00)
  ENDIF
ENDDO

!Furnace fan standard effective 2019	!TODO - in historical years prior to 2020 RECS base year; update/remove
IF (CurCalYr.EQ.RECSYear+1) THEN
  DO Y=RECSyear,EndYr
    IF (Y.LE.2018) THEN  !FurnFanStandard
      NFANEFF(Y)=1.0
      FANEFF(Y)=1.0
    ELSE
      NFANEFF(Y)=0.75 !Furance fans expected to be 25% more efficient due to 2019 standard?  !FurnFanStandard
      FANEFF(Y)=1.0
    ENDIF
  ENDDO
ENDIF

IF (CURITR.EQ.1) THEN
  DO R=1,mNumCR-2
    DO B=1,mNumBldg
      EFANUEC(CurCalYr,R,B)=FANUEC(R,B)*FANEFF(CurCalYr)*EXSQFTADJ(CurCalYr,B,R,5)
      FANIUEC(R,B)=FANUEC(R,B)*FANEFF(CurCalYr)
    ENDDO
  ENDDO
ENDIF

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    NFANUEC(CurCalYr,R,B)=FANUEC(R,B)*NFANEFF(CurCalYr)*exSQFTADJ(CurCalYr,B,R,5)
    NFANIUEC(CurCalYr,R,B)=FANUEC(R,B)*NFANEFF(CurCalYr)  ! INTENSITY UEC FOR INDEX
  ENDDO
ENDDO

!********************************************************************
!COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!********************************************************************
IF (CurCalYr.GE.RECSYear+1) THEN
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
  ENDDO
ENDIF

!******************************************************************************
!CALCULATE EXISTING (RECS-YEAR) SPACE HEATING & COOLING SHELL INDEXES (BEFORE WEATHERIZATION)
!******************************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO F=1,mNumFuel  !NoKero
      EHSHELL(CurCalYr,F,R,B)=0.0
      IF (F.NE.5) THEN  !No price response for wood  !NoKero
        EHSHELL(CurCalYr,F,R,B)=EHSHELL(RECSYear,F,R,B)*RSELAST(F,R,ALPHA2,EF1,EF2,EF3,RECSYear,EUPR)-TECHG(CurCalYr,R,B)
      ELSE  !Wood-heated
        EHSHELL(CurCalYr,F,R,B)=EHSHELL(RECSYear,F,R,B)-TECHG(CurCalYr,R,B)
      ENDIF
    ENDDO

    !Compute composite cooling shell by R & B based on 3 fuels	!TODO - verify source of shares
    ECSHELL(CurCalYr,R,B)=ECSHELL(RECSYear,R,B)- &
     ((EHSHELL(RECSYear,1,R,B)-EHSHELL(CurCalYr,1,R,B))*0.1 + &  !Distillate Fuel Oil
     (EHSHELL(RECSYear,3,R,B)-EHSHELL(CurCalYr,3,R,B))*0.6 + &  !Natural Gas
     (EHSHELL(RECSYear,4,R,B)-EHSHELL(CurCalYr,4,R,B))*0.3 )*0.38  !Electricity
  ENDDO
ENDDO

!******************************************************************************
!APPLY WEATHERIZATION EFFECTS TO EXISTING (RECS-YEAR) SPACE HEATING & COOLING SHELL INDEXES
!******************************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO F=1,mNumFuel  !NoKero
      !INITIALIZE ARRAYS
      ANUM(F,R,B)=0.0
      ADEN(F,R,B)=0.0
      EHSHELL(CurCalYr,F,R,B)=EHSHELL(CurCalYr,F,R,B)+(WTHRZTN(CurCalYr,1,R,B))
      IF (EHSHELL(CurCalYr,F,R,B) .GT. EHSHELL(CurCalYr-1,F,R,B)) &
       EHSHELL(CurCalYr,F,R,B)=EHSHELL(CurCalYr-1,F,R,B)
      IF (EHSHELL(CurCalYr,F,R,B).LT.LIMIT) EHSHELL(CurCalYr,F,R,B)=LIMIT
    ENDDO
    ECSHELL(CurCalYr,R,B)=ECSHELL(CurCalYr,R,B)+(WTHRZTN(CurCalYr,2,R,B))
    IF (ECSHELL(CurCalYr,R,B) .GT. ECSHELL(CurCalYr-1,R,B)) &
     ECSHELL(CurCalYr,R,B)=ECSHELL(CurCalYr-1,R,B)
    IF (ECSHELL(CurCalYr,R,B).LT.LIMIT) ECSHELL(CurCalYr,R,B)=LIMIT
  ENDDO
ENDDO

!********************************************************************
!CALCULATE NEW (ADDED IN CURRENT YEAR) SPACE HEATING SHELL INDEX
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      F=RTFUEL(RECCL)
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          ANUM(F,R,B)=ANUM(F,R,B)+(NHSHELL(Y,F,R,B)*EQCADD(Y,RECCL,B,R))
          ADEN(F,R,B)=ADEN(F,R,B)+(EQCADD(Y,RECCL,B,R))
        ENDDO
      ENDIF
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE NEW (POST-RECS-YEAR ADDTIONS EXCEPT CURRENT YEAR) SPACE HEATING SHELL INDEX
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO F=1,mNumFuel  !NoKero
      IF (ADEN(F,R,B).LE.0) THEN
        AHSHELL(CurCalYr,F,R,B)=AHSHELL(CurCalYr-1,F,R,B)
      ELSE
        AHSHELL(CurCalYr,F,R,B)=ANUM(F,R,B)/ADEN(F,R,B)
      ENDIF
      IF (AHSHELL(CurCalYr,F,R,B).LT.LIMIT) AHSHELL(CurCalYr,F,R,B)=LIMIT
      IF (CurCalYr.LE.(RECSYear+1)) THEN
        AHSHELL(CurCalYr,F,R,B)=NHSHELL(CurCalYr,F,R,B)
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE NEW, REPLACEMENT, AND AVERAGE UECS
!*******************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      FAN=RTFFAN(RECCL)
      F=RTFUEL(RECCL)

      !S IS USED IN THE EXSQFTADJ CALCULATION AND DIFFERS DEPENDING ON
      ! WHETHER THE FUEL FOR HEATING IS ELECTRIC (2) OR FOSSIL (1)
      ! SEE RSMISC.TXT FOR "ELASTIC" INPUTS THAT MODIFY THE SQFT ADJUSTMENTS
      ! BY TYPE OF FUEL (THERE ARE ALSO SEPARATE ADJUSTMENTS FOR
      ! CENTRAL AC (3), HP COOLING (4), AND FURNACE FANS (5)

      !F=4 for electricity
      IF (F.EQ.4) S=2
      IF (F.NE.4) S=1

      !RTBASEFF(CurCalYr,RECCL) IS PROJECTED STOCK EFFICIENCY OF THE SURVIVING RECSYear STOCK
      ! READ FROM RSSTKEFF.TXT WHICH WAS DEVELOPED BY EXOGENOUS VINTAGING PROGRAM

      !THERE ARE TWO SETS OF UECS DEVELOPED BELOW:
      ! THE "I" IN MIDDLE OF THE UECS NAMES DENOTE INTENSITIES FOR THE RESIDENTIAL EFFICIENCY INDEX IN REPORT WRITER,
      ! LEAVING OFF SQUARE FOOTAGE ADJUSTMENT (INCREASE) TO RECOGNIZE THAT GREATER USAGE FOR EXPANDED
      ! LIVING SPACE IS NOT AN EFFICIENCY LOSS, BUT INSTEAD SERVES INCREASED SERVICE DEMAND

      !EQCSUEC APPLIES TO THE SURVIVING RECSYear STOCK
      EQCSUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)*RTBASEFF(RECSYear,RECCL)/RTBASEFF(CurCalYr,RECCL) &
       *EXSQFTADJ(CurCalYr,B,R,S)
      EQCSIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)*RTBASEFF(RECSYear,RECCL)/RTBASEFF(CurCalYr,RECCL)

      !EQCNUEC APPLIES TO NEW REPLACEMENT EQUIPMENT IN NEW CONSTRUCTION (I.E. ADDED AFTER THE RECSYear)
      IF (WTEQCEFFN(CurCalYr,RECCL,B,R).GT.0.0) THEN
        EQCNUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)*WTEQCEFFN(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)
        EQCNIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)*WTEQCEFFN(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)
      ELSE
        EQCNUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)
        EQCNIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)
      ENDIF

      !EQCHVUEC APPLIES TO NEW CONSTRUCTION THIS YEAR
      IF (WTEQCEFFHV(CurCalYr,RECCL,B,R).GT.0.0) THEN
        EQCHVUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)*WTEQCEFFHV(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)*WTEQCSQFHV(CurCalYr,RECCL,B,R)
        EQCHVIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)*WTEQCEFFHV(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)
      ELSE
        EQCHVUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)*WTEQCSQFHV(CurCalYr,RECCL,B,R)
        EQCHVIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)    !INTENSITY UEC FOR INDEX
      ENDIF

      !EQCRUEC APPLIES TO REPLACEMENTS OF EQUIPMENT THIS YEAR FROM HOUSING EXISTING IN RECSYear
      IF (WTEQCEFFR(CurCalYr,RECCL,B,R) .GT. 0.0) THEN
        EQCRUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)*WTEQCEFFR(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)*EXSQFTADJ(CurCalYr,B,R,S)
        EQCRIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)*WTEQCEFFR(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)
      ELSE
        EQCRUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)*EXSQFTADJ(CurCalYr,B,R,S)
        EQCRIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)
      ENDIF

      !EQCAUEC IS THE AVERAGE UEC FOR EQUIPMENT IN RECSYear HOUSING STOCK THAT HAS BEEN REPLACED (ONCE)
      ! AND THAT BOTH THE EQUIPMENT AND HOUSE HAVE SURVIVED TO THIS YEAR.
      !EQCAHVUEC IS THE AVERAGE UEC FOR EQUIPMENT IN POST-RECSYear HOUSINGS STOCK.
      IF (CurCalYr-1.EQ.RECSYear) THEN
        EQCAUEC(CurCalYr,RECCL,B,R)=EQCNUEC(CurCalYr,RECCL,B,R)
        EQCAHVUEC(CurCalYr,RECCL,B,R)=EQCHVUEC(CurCalYr,RECCL,B,R)
        AFANUEC(RECSYear+1,R,B)=NFANUEC(RECSYear+1,R,B)
      ELSE
        !SUM ALL OF THE SURVIVING / VINTAGED EQUIPMENT FROM RECSYear TO YEAR PRIOR TO THIS YEAR
        ! (EQUIPMENT STOCK NAMES WITH "FUT" APPENDED FOR "SURVIVING IN A FUTURE YEAR")
        TEMP=0.0
        TEMP1=0.0
        TEMP2=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,R)+EQR90RPFUT(Y,CurCalYr,RECCL,B,R)
          TEMP1=TEMP1+(EQR90FUT(Y,CurCalYr,RECCL,B,R)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,R)+EQREPFUT(Y,CurCalYr,RECCL,B,R)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,R))*FAN
          TEMP2=TEMP2+EQADDFUT(Y,CurCalYr,RECCL,B,R)+EQREPFUT(Y,CurCalYr,RECCL,B,R)
        ENDDO

        IF (TEMP.LE.0.0) THEN
          EQCAUEC(CurCalYr,RECCL,B,R)=EQCNUEC(CurCalYr,RECCL,B,R)
        ELSE
          EQCAUEC(CurCalYr,RECCL,B,R)=0.0
          DO Y=RECSYear,CurCalYr-1
            EQCAUEC(CurCalYr,RECCL,B,R)=EQCAUEC(CurCalYr,RECCL,B,R)+ &
             (EQR90FUT(Y,CurCalYr,RECCL,B,R)*EQCRUEC(Y,RECCL,B,R)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,R)*EQCNIUEC(Y,RECCL,B,R)) &
             /TEMP
          ENDDO
        ENDIF

        IF (TEMP1.LE.0.0) THEN
          AFANUEC(CurCalYr,R,B)=NFANUEC(CurCalYr,R,B)
        ELSE
          AFANUEC(CurCalYr,R,B)=0.0
          DO Y=RECSYear,CurCalYr-1
            AFANUEC(CurCalYr,R,B)=AFANUEC(CurCalYr,R,B)+ &
             (EQR90FUT(Y,CurCalYr,RECCL,B,R)*EFANUEC(Y,R,B)*FAN+ &
             EQADDFUT(Y,CurCalYr,RECCL,B,R)*NFANUEC(Y,R,B)*FAN+&
             EQREPFUT(Y,CurCalYr,RECCL,B,R)*NFANUEC(Y,R,B)*FAN+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,R)*NFANUEC(Y,R,B)*FAN) &   !Change from EUEC to NUEC in fan calculation
             /TEMP1
          ENDDO
        ENDIF !TEMP1 <=0.

        IF (TEMP2.LE.0.0) THEN
          EQCAHVUEC(CurCalYr,RECCL,B,R)=EQCHVUEC(CurCalYr,RECCL,B,R)
        ELSE
          EQCAHVUEC(CurCalYr,RECCL,B,R)=0.0
          DO Y=RECSYear,CurCalYr-1
            EQCAHVUEC(CurCalYr,RECCL,B,R)=EQCAHVUEC(CurCalYr,RECCL,B,R)+ &
             (EQADDFUT(Y,CurCalYr,RECCL,B,R)*EQCHVUEC(Y,RECCL,B,R)+ &
             EQREPFUT(Y,CurCalYr,RECCL,B,R)*EQCNUEC(Y,RECCL,B,R)) &
             /TEMP2
          ENDDO
        ENDIF !TEMP2 <=0.
      ENDIF  !CurCalYr = RECSYear
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

!********************************************************************
!CALCULATE AVERAGE EQUIPMENT EFFICIENCY
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      FAN=RTFFAN(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        WTEQCEFFA(RECSYear+1,RECCL,B,R)=WTEQCEFFN(RECSYear+1,RECCL,B,R)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,R)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,R)+EQREPFUT(Y,CurCalYr,RECCL,B,R)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,R)
        ENDDO
        IF (TEMP.GT. 0.0) THEN
          WTEQCEFFA(CurCalYr,RECCL,B,R)=0.0
          DO Y=RECSYear,CurCalYr-1
            WTEQCEFFA(CurCalYr,RECCL,B,R)=WTEQCEFFA(CurCalYr,RECCL,B,R)+ &
             ((EQR90FUT(Y,CurCalYr,RECCL,B,R)*WTEQCEFFR(Y,RECCL,B,R))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,R)*WTEQCEFFHV(Y,RECCL,B,R))+ &
             (EQREPFUT(Y,CurCalYr,RECCL,B,R)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,R))*WTEQCEFFN(Y,RECCL,B,R)))/TEMP
          ENDDO
        ELSE
          WTEQCEFFA(CurCalYr,RECCL,B,R)=WTEQCEFFN(CurCalYr,RECCL,B,R)
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!INITIALIZE SPACE HEATING CONSUMPTION VARIABLES
!FOR NEMS REPORTING
HTRCON(CurIYr,1:NHTRFL,1:mNumCR-2) = 0.0
FANCON(CurIYr,1:mNumCR-2) = 0.0

!FOR REPORT WRITER EFFICIENCY CALCULATION
Driver(CurIYr,1:NHTRFL,1:mNumCR-2, 1:mNumBldg) = 0.0
Driver2(CurIYr,1:mNumCR-2,1:mNumBldg) = 0.0
HTRCONWT(CurIYr,1:NHTRFL,1:mNumCR-2,1:mNumBldg) = 0.0
HTRCONIN(CurIYr,1:NHTRFL,1:mNumCR-2,1:mNumBldg) = 0.0
FANCONWT(CurIYr,1:mNumCR-2,1:mNumBldg) = 0.0
FANCONIN(CurIYr,1:mNumCR-2,1:mNumBldg) = 0.0

!********************************************************************
!CALCULATE SPACE HEATING CONSUMPTION
!********************************************************************
DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  !FIND EQUIPMENT CLASS FOR AIR-SOURCE AND GROUND-SOURCE HEAT PUMPS	!TODO - still needed if no longer estimating geothermal energy consumption?
  IF (RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
    EQCEHP=RTCLEQCL(RECCL)
    RECCLEHP=EQCEHP
  ELSEIF (RTCLNAME(RECCL).EQ.'GEO_HP') THEN
    EQCGHP=RTCLEQCL(RECCL)
    RECCLGHP=EQCGHP
  ENDIF
ENDDO

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    FANEQCN(CurIYr,1,B,R)=0.0
    FANEQP(CurCalYr,B,R)=0.0
    !F = FUEL NUMBER FROM RSCLASS FILE, F=1,2,3,4,5(WOOD)  !NoKero
    !FHTRCON(F) = HTRCON FUEL NUMBER
    ! MAP RSCLASS FUEL NUMBERS INTO FHTRCON FUEL NUMBERS
    !                FHTRCON   RSCLASS(RTEK)
    ! FUEL             FCON       F
    !  NATURAL GAS       1        3
    !  ELECTRICITY       2        4
    !  DFO+KEROSENE      3        1 (DFO=Distillate Fuel Oil)
    !  PROPANE           4        2
    !  WOOD              5        1 (Priced to distillate fuel oil)  !NoKero

    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      F=RTFUEL(RECCL)
      FAN=RTFFAN(RECCL)
      !F=4 for electricity
      !S IS USED IN THE EXSQFTADJ CALCULATION AND DIFFERS DEPENDING ON
      ! WHETHER THE FUEL FOR HEATING IS ELECTRIC (2) OR FOSSIL (1)
      ! SEE RSMISC.TXT FOR "ELASTIC" INPUTS THAT MODIFY THE SQFT ADJUSTMENTS
      ! BY TYPE OF FUEL (THERE ARE ALSO SEPARATE ADJUSTMENTS FOR
      ! CENTRAL AC (3), HP COOLING (4) AND FURNACE FANS (5)
      IF (F.EQ.4) S=2
      IF (F.NE.4) S=1
      !MAP RTEK FUEL NUMBERS INTO NEMS FUEL NUMBERS
      FCON=FHTRCON(F)

      IF (F.EQ.4) THEN
        ALPHA=-0.30  !was -0.15 prior to American Recovery and Reinvestment Act of 2009 (ARRA) stimulus; permanently affects price elasticity (but not rebound) based on the smart grid concept
      ELSE
        ALPHA=-0.15
      ENDIF

      !Code to streamline wood treatment
      F2=F
      IF (F.EQ.5) ALPHA=0.50  !NoKero
      IF (F.EQ.5) F2=1  !NoKero

      !Efficiency Rebound Effects
      IF (CurCalYr.GT.RECSYear+1) THEN
        RBA=(RTBASEFF(RECSYear,RECCL)*WTEQCEFFA(CurCalYr,RECCL,B,R))**ALPHA
        RBR=(RTBASEFF(RECSYear,RECCL)*WTEQCEFFR(CurCalYr,RECCL,B,R))**ALPHA
        RBN=(RTBASEFF(RECSYear,RECCL)*WTEQCEFFN(CurCalYr,RECCL,B,R))**ALPHA
      ELSE
        RBA=1.0
        RBR=1.0
        RBN=1.0
      ENDIF

      !CONSUMPTION FOR NEMS OUTPUT TABLES
      HTRCON(CurIYr,FCON,R)=HTRCON(CurIYr,FCON,R)+HDDFACT(R)*LEAPYR*(( &
       (EQCESE(CurCalYr,RECCL,B,R)*EQCSUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2)) + &
       (EQCADD(CurCalYr,RECCL,B,R)*EQCHVUEC(CurCalYr,RECCL,B,R)* &
       (NHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
       (EQCRP90(CurCalYr,RECCL,B,R)*EQCRUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBR + &
       (EQCRP90RP(CurCalYr,RECCL,B,R)*EQCNIUEC(CurCalYr,RECCL,B,R)*EXSQFTADJ(CurCalYr,B,R,S)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
       (EQCSR90(CurCalYr,RECCL,B,R)*EQCAUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA + &
       (EQCREP(CurCalYr,RECCL,B,R) *EQCNUEC(CurCalYr,RECCL,B,R)* &
       (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
       (EQCSUR(CurCalYr,RECCL,B,R) *EQCAHVUEC(CurCalYr,RECCL,B,R)* &
       (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA)*( &
       RSELAST(F2,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

      !WEIGHT, INTENSITY AND "DRIVER" VARIABLES FOR THE REPORT WRITER EFFICIENCY CALCULATION	!TODO - aside from B and the WT in name, this equation is the same as above for HTRCON; combine common calculations or create function to use across end uses?
      HTRCONWT(CurIYr,FCON,R,B)=HTRCONWT(CurIYr,FCON,R,B)+HDDFACT(R)*LEAPYR*(( &
       (EQCESE(CurCalYr,RECCL,B,R)*EQCSUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2)) + &
       (EQCADD(CurCalYr,RECCL,B,R)*EQCHVUEC(CurCalYr,RECCL,B,R)* &
       (NHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
       (EQCRP90(CurCalYr,RECCL,B,R)*EQCRUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBR + &
       (EQCRP90RP(CurCalYr,RECCL,B,R)*EQCNIUEC(CurCalYr,RECCL,B,R)*EXSQFTADJ(CurCalYr,B,R,S)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
       (EQCSR90(CurCalYr,RECCL,B,R)*EQCAUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA + &
       (EQCREP(CurCalYr,RECCL,B,R) *EQCNUEC(CurCalYr,RECCL,B,R)* &
       (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
       (EQCSUR(CurCalYr,RECCL,B,R) *EQCAHVUEC(CurCalYr,RECCL,B,R)* &
       (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA)*( &
       RSELAST(F2,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

      HTRCONIN(CurIYr,FCON,R,B)=HTRCONIN(CurIYr,FCON,R,B)+((( &
       (EQCESE(CurCalYr,RECCL,B,R)*EQCSIUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
       (EQCADD(CurCalYr,RECCL,B,R)*EQCHVIUEC(CurCalYr,RECCL,B,R)* &
       (NHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
       (EQCRP90(CurCalYr,RECCL,B,R)*EQCRIUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
       (EQCRP90RP(CurCalYr,RECCL,B,R)*EQCNIUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
       (EQCSR90(CurCalYr,RECCL,B,R)*EQCAUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
       (EQCREP(CurCalYr,RECCL,B,R) *EQCNIUEC(CurCalYr,RECCL,B,R)* &
       (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
       (EQCSUR(CurCalYr,RECCL,B,R) *EQCAHVUEC(CurCalYr,RECCL,B,R)* &
       (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))))))

      Driver(CurIYr,FCON,R,B)=Driver(CurIYr,FCON,R,B)+ &
       (EQCESE(CurCalYr,RECCL,B,R)+EQCADD(CurCalYr,RECCL,B,R)+ &
       EQCRP90RP(CurCalYr,RECCL,B,R)+EQCRP90(CurCalYr,RECCL,B,R)+ &
       EQCSR90(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R)+ &
       EQCSUR(CurCalYr,RECCL,B,R))

      !CALCULATION FOR EQUIPMENT-SPECIFIC ENERGY CONSUMPTION DATABASE
      EQCEQCN(CurIYr,RECCL,B,R)= HDDFACT(R)*LEAPYR*(( &
       (EQCESE(CurCalYr,RECCL,B,R)*EQCSUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2)) + &
       (EQCADD(CurCalYr,RECCL,B,R)*EQCHVUEC(CurCalYr,RECCL,B,R)* &
       (NHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
       (EQCRP90(CurCalYr,RECCL,B,R)*EQCRUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBR + &
       (EQCRP90RP(CurCalYr,RECCL,B,R)*EQCNIUEC(CurCalYr,RECCL,B,R)*EXSQFTADJ(CurCalYr,B,R,S)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
       (EQCSR90(CurCalYr,RECCL,B,R)*EQCAUEC(CurCalYr,RECCL,B,R)* &
       (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA + &
       (EQCREP(CurCalYr,RECCL,B,R)*EQCNUEC(CurCalYr,RECCL,B,R)* &
       (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
       (EQCSUR(CurCalYr,RECCL,B,R)*EQCAHVUEC(CurCalYr,RECCL,B,R)* &
       (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA)* &
       RSELAST(F2,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

      IF (F.NE.5) THEN   !ASSUMING NO FANS OR FAN CONSUMPTION FOR WOOD_HT  !NoKero
        FANEQCN(CurIYr,1,B,R)=FANEQCN(CurIYr,1,B,R)+HDDFACT(R)*LEAPYR* &
         (EQCESE(CurCalYr,RECCL,B,R)*FAN*EFANUEC(CurCalYr,R,B)+ &
         (EQCRP90(CurCalYr,RECCL,B,R)+EQCRP90RP(CurCalYr,RECCL,B,R))* &
         FAN*NFANUEC(CurCalYr,R,B)+ &
         (EQCSR90(CurCalYr,RECCL,B,R))*FAN*AFANUEC(CurCalYr,R,B)+ &
         (EQCADD(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R))* &
         FAN*NFANUEC(CurCalYr,R,B) + &
         EQCSUR(CurCalYr,RECCL,B,R)*FAN*AFANUEC(CurCalYr,R,B))* &
         RSELAST(F,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

        FANCON(CurIYr,R)=FANCON(CurIYr,R)+HDDFACT(R)*LEAPYR* &
         (EQCESE(CurCalYr,RECCL,B,R)*EFANUEC(CurCalYr,R,B)*FAN+ &
         (EQCRP90(CurCalYr,RECCL,B,R)+EQCRP90RP(CurCalYr,RECCL,B,R))* &
         FAN*NFANUEC(CurCalYr,R,B)+ &
         (EQCSR90(CurCalYr,RECCL,B,R))*FAN*AFANUEC(CurCalYr,R,B)+ &
         (EQCADD(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R))* &
         FAN*NFANUEC(CurCalYr,R,B) + &
         EQCSUR(CurCalYr,RECCL,B,R)*FAN*AFANUEC(CurCalYr,R,B))* &
         RSELAST(F,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

        FANCONWT(CurIYr,R,B)=FANCONWT(CurIYr,R,B)+HDDFACT(R)*LEAPYR* &	!TODO - aside from B and the WT in name, this equation is the same as above for FANCON; combine common calculations or create function to use across end uses?
         (EQCESE(CurCalYr,RECCL,B,R)*EFANUEC(CurCalYr,R,B)*FAN+ &
         (EQCRP90(CurCalYr,RECCL,B,R)+EQCRP90RP(CurCalYr,RECCL,B,R))* &
         FAN*NFANUEC(CurCalYr,R,B)+ &
         (EQCSR90(CurCalYr,RECCL,B,R))*FAN*AFANUEC(CurCalYr,R,B)+ &
         (EQCADD(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R))* &
         FAN*NFANUEC(CurCalYr,R,B) + &
         EQCSUR(CurCalYr,RECCL,B,R)*FAN*AFANUEC(CurCalYr,R,B))* &
         RSELAST(F,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

        FANEQP(CurCalYr,B,R)=FANEQP(CurCalYr,B,R)+ &
         (EQCESE(CurCalYr,RECCL,B,R)+EQCRP90(CurCalYr,RECCL,B,R)+ &
         EQCSR90(CurCalYr,RECCL,B,R)+EQCSUR(CurCalYr,RECCL,B,R)+ &
         EQCRP90RP(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R)+& !Added EQCREP
         EQCADD(CurCalYr,RECCL,B,R))*FAN

        FANCONIN(CurIYr,R,B)=FANCONIN(CurIYr,R,B)+((&
         (EQCESE(CurCalYr,RECCL,B,R))*FAN*FANIUEC(R,B)+ &
         (EQCADD(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R)+ &
         EQCRP90(CurCalYr,RECCL,B,R)+EQCRP90RP(CurCalYr,RECCL,B,R))* &
         FAN*NFANIUEC(CurCalYr,R,B)+ &
         (EQCSUR(CurCalYr,RECCL,B,R)+EQCSR90(CurCalYr,RECCL,B,R))* &
         FAN*AFANUEC(CurCalYr,R,B)))

        Driver2(CurIYr,R,B)=Driver2(CurIYr,R,B)+ &
         EQCESE(CurCalYr,RECCL,B,R)+EQCADD(CurCalYr,RECCL,B,R)+ &
         EQCRP90RP(CurCalYr,RECCL,B,R)+EQCRP90(CurCalYr,RECCL,B,R)+ &
         EQCSR90(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R)+ &
         EQCSUR(CurCalYr,RECCL,B,R)
      ENDIF ! F - NO FAN CONSUMPTION FOR WOOD
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

!CALCULATE INTENSITY VARIABLE FOR REPORT WRITER, ADJUSTING FOR DRIVER IN DENOMINATOR
DO R=1,mNumCR-2
  DO FCON=1,NHTRFL
    DO B=1,mNumBldg
      IF (Driver(CurIYr,FCON,R,B).GT.0) &
       HTRCONIN(CurIYr,FCON,R,B)=HTRCONIN(CurIYr,FCON,R,B)/Driver(CurIYr,FCON,R,B)
      IF (Driver2(CurIYr,R,B).GT.0) &
       FANCONIN(CurIYr,R,B)=FANCONIN(CurIYr,R,B)/Driver2(CurIYr,R,B)
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE RHTRCON


!==============================================================================
! SPACE COOLING CHOICE SUBROUTINE
!==============================================================================
SUBROUTINE RCLTEC
IMPLICIT NONE

REAL*4 TOTEWTN(nCoolClasses,mNumBldg,mNumCR-2),TOTEWTR(nCoolClasses,mNumBldg,mNumCR-2)  !EqpParam
REAL*4 OPCOST(2),CDDFACT(mNumCR-2)
REAL*4 EQWTN(nCoolTypes,mNumBldg,mNumCR),EQWTR(nCoolTypes,mNumBldg,mNumCR)
REAL*4 RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2
REAL*4 EQCOST,CAPITAL,RETAIL
INTEGER EU,EUPR,EUHT,RECTY,RECTYHT,RECCL,R,B,F,EQT,EQC,TYPE,TYPEHT,RECCLHHP,COUNT,CNT,L,IND
INTEGER RECAR(nCoolTypes),EQTAR(nCoolTypes)  !EqpParam

!********************************************************************
! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES	!TODO - combine/optimize similar code
!********************************************************************

EU=2  !space cooling
EUPR=2
ALPHA1=-0.50

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!COMPUTE CDDFACT
DO R=1,mNumCR-2
  CDDFACT(R)=(CDDADJ(CurCalYr,R)/CDDADJ(RECSYear,R))**1.50 !A 10% increase in CDD would increase space cooling consumption by 15% (e.g., 1.10^1.50=1.15)
ENDDO

!COMPUTE DECAY RATE USED TO COMPUTE LIFE-CYCLE COST (FIRST ITERATION ONLY)
IF (CURITR.EQ.1) THEN
  DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
ENDIF

DO R=1,mNumCR-2
  DO B=1,mNumBldg

    !VARIABLES USED THIS SECTION (see additional notes in RHTRTEC):
    ! RSMEQP and RSCLASS Variables
    ! RECCL          = RECORD NUMBER IN RSCLASS
    ! RTCLEUPT(EU)   = LAST RECORD # IN SPACE HEATING (EU=1)
    ! RTCLEUPT(EU+1) = LAST RECORD # IN SPACE COOLING (EU=2)
    ! EQC            = EQUIPMENT CLASS # FOR SPACE COOLING

    !INITIALIZE ARRAYS
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      TOTEWTN(EQC,B,R)=0.0
      TOTEWTR(EQC,B,R)=0.0
    ENDDO

    !RECTY          = RECORD NUMBER IN RSMEQP
    !RTTYEUPT(EU)   = LAST RECORD # IN SPACE HEATING (EU=1)
    !RTTYEUPT(EU+1) = LAST RECORD # IN SPACE COOLING (EU=2)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT
          ! TYPE (EQT), REC # FOR RECTY FILE (RECCL), AND FUEL TYPE (F)
          EQC=RTTYEQCL(RECTY)
          EQT=RTEQTYPE(RECTY)
          RECCL=RTCLEUPT(EU)+EQC

          !ONLY CONTINUE CALCULATIONS IF THIS IS NOT A HEAT PUMP (ELEC_HP, GEO_HP, NG_HP)
          ! (HEAT PUMPS USE THE SAME DATA CALCULATED FOR HEATING)
          ! (If RTTYPNTR is not zero, then it points back to the heating equipment type)
          IF (RTTYPNTR(RECTY).LE.0) THEN
            !FUEL NUMBER FOR THE CURRENT EQUIPMENT CLASS
            F = RTFUEL(RECCL)

            !COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
            IF (RTEQEFF(RECTY).NE.0.0) THEN
              !RTEFFAC(1) is used to adjust UECs for replacements from the original stock of equipment from RECSYear
              RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)  !eqceff is retiring stock efficiency
              !RTEFFAC(2) is used to adjust RECSYear UECs for new construction decisions
              RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)  !rtbaseff is stock efficiency at RECSYear
            ELSE
              RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
              RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
            ENDIF

            !SET CAPITAL COSTS
            ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
            ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
            IF (COSTTRSW.EQ.1) THEN
              CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
            ELSE
              CAPITAL = RTEQCOST(RECTY)
            ENDIF

            !CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
            ! i.e., reduce implicit discount rates as real prices increase
            IF ((CurCalYr.GT.2008).AND. &	!TODO - 2008 marks last year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)? Remove/modify legacy energy bill code as necessary
             (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
              HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
              ELIGBLE=HRDRATE - 0.07
              IF (ELIGBLE.GT.0.0) THEN
                HRDADJ= ELIGBLE * &
                 ((PRICES(F,R,CurCalYr)/PRICES(F,R,RECSYear))**ALPHA1 )
                BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
              ELSE
                BETA1DR(RECTY)=RTECBTA1(RECTY)
              ENDIF
            ELSE
              BETA1DR(RECTY)=RTECBTA1(RECTY)
            ENDIF

            !COMPUTE THE PART OF THE EQUIMENT CHOICE WEIGHT NOT DEPENDENT ON REGION AND BUILDING TYPE
            ECTEMP = RTECBIAS(RECTY) + (BETA1DR(RECTY)*CAPITAL)	!TODO - reincorporate into EQWTN and EQWTR equations below (similar to other end uses)? There are a few EUs that use this ECTEMP variable, the math ends up being the same, so it would be easy to incorporate back 
		    
            !CALCULATE OPERATING COST
            IF (CurCalYr.EQ.RECSYear+1) THEN
              OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)*CDDFACT(R)
              OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(2)*CDDFACT(R)
            ELSE
              OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)*CDDFACT(R) &
               *(ECSHELL(CurCalYr-1,R,B)/ECSHELL(RECSYear,R,B))
              OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(2)*CDDFACT(R) &
               *(NCSHELL(CurCalYr-1,R,B)/ECSHELL(RECSYear,R,B))
            ENDIF
	        
            !CALCULATE LIFE-CYCLE COSTS
            LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
            LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)
	        
            !COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
            EQWTN(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(2)) + ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
            TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
            EQWTR(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(1)) + ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
            TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
            IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 3
              OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
                WRITE(661,64) '3,',EU, ',',EQC, ',','X,',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,',EQWTN(EQT,B,R),',',EQWTR(EQT,B,R),',',ECTEMP,',','X,',CAPITAL,',','X,',OPCOST(1),',',OPCOST(2),',','X,',RTECBTA2(RECTY),',',RTECBTA3(RECTY),',',LFCY(EQT,B,R,1),',','X,',TOTEWTR(EQC,B,R),',',TOTEWTN(EQC,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
                64 FORMAT( a,I,a,I,a,a,  I,a,I,a,I,a,I,a,a,  a,  f,a,f,a,f,a,a,  f,a,  a,  f,a,f,a,a,  f,a,f,a,f,a,a,  f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
              CLOSE(661)
            ENDIF
          ENDIF !filter census division
        ENDIF !filter census division
      ENDIF !filter year availability
    ENDDO !for all RSMEQP records this end use

    !*******************************************************************
    !CALCULATE NEW AND REPLACEMENT MARKET SHARES
    !*******************************************************************
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !Filter for year availability
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT TYPE (EQT), REC # (RECCL) IN RSCLASS
          EQC=RTTYEQCL(RECTY)
          EQT=RTEQTYPE(RECTY)
          RECCL=RTCLEUPT(EU)+EQC

          !IF THIS IS A HEAT PUMP, ASSIGN CORRESPONDING HEATER SHARES TO HEAT PUMP SPACE COOLING
          IF (RTTYPNTR(RECTY).GT.0) THEN
            TYPEHT=RTTYPNTR(RECTY)
            NEQTSHR(CurCalYr,TYPE,B,R)=NEQTSHR(CurCalYr,TYPEHT,B,R)
            REQTSHR(CurCalYr,TYPE,B,R)=REQTSHR(CurCalYr,TYPEHT,B,R)

          !IF NOT A HEAT PUMP, COMPUTE SHARES
          ELSE
            IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
              NEQTSHR(CurCalYr,TYPE,B,R)=EQWTN(EQT,B,R) / TOTEWTN(EQC,B,R)
            ELSE
              NEQTSHR(CurCalYr,TYPE,B,R)=0.0
            ENDIF

            IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
              REQTSHR(CurCalYr,TYPE,B,R)=EQWTR(EQT,B,R) / TOTEWTR(EQC,B,R)
            ELSE
              REQTSHR(CurCalYr,TYPE,B,R)=0.0
            ENDIF
          ENDIF
        ENDIF
      ENDIF 

      IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 4
        OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
          WRITE(661,65) '4,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
          65 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
        CLOSE(661)
      ENDIF
    ENDDO

    !CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT SPACE COOLING EQUIPMENT
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC   =RTCLEQCL(RECCL)
      DENOM =0
      DENOM2=0
      COUNT =0

      TYPE = RTTYPECT(EU)
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            TYPE=TYPE+1
            IF (RTTYEQCL(RECTY).EQ.EQC) THEN
              COUNT=COUNT+1
              RECAR(COUNT)=RECTY
              EQTAR(COUNT)=TYPE
              DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
              DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
      ENDIF
     
      !Based on IF statement above should never be here!	!TODO - verify
      IF (WTEQCEFFN(CurCalYr,RECCL,B,R).EQ.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ENDIF
     
      !COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
      IF (DENOM2.LE.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
      ENDIF

      !COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
      IF (WTEQCEFFR(CurCalYr,RECCL,B,R).EQ.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ENDIF

    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

END SUBROUTINE RCLTEC


!==============================================================================
! SPACE COOLING ADDED SUBROUTINE
!==============================================================================
SUBROUTINE RCLADD
IMPLICIT NONE

REAL*4 EQCPN90(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
REAL*4 SA(8,mNumBldg,mNumCR-2), HSR, ESR, SVRTE	!TODO - replace 8 with parameter (nCoolClasses?)
REAL*4 SUMHP,CACFACT,max_cenac_pen,x
REAL*4 RPTOT(mNumYr),EXTOT(mNumYr),ADDTOT(mNumYr),RPRPTOT(mNumYr),ANUMC(mNumCR-2,mNumBldg),ADENC(mNumCR-2,mNumBldg)
REAL*4 NEWSHELLWTC(RECSYear:EndYr,mNumBldg,mNumCR),NEWADDWTC(RECSYear:EndYr,mNumBldg,mNumCR)
INTEGER EQC,RECCL,RECCLHHP,RECCLCAC,RECCLEHP,EU
INTEGER Y, R, B, TYPE, NUMEQT, EQT, RECTY, T, TEMP, V, E, D

EU =  2  !space cooling
CACFACT = .1	!TODO - add note/source
max_cenac_pen = 0.90  !Max 90% central AC penetration into remaining RECSYear housing stock (analyst judgment)

!Initialize arrays
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCSR90(CurCalYr,RECCL,B,R)=0.0
      EQCSUR(CurCalYr,RECCL,B,R)=0.0
      SA(EQC,B,R)=0.0
      EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
    ENDDO
  ENDDO
ENDDO

!CALCULATE SPACE COOLING EQUIPMENT ADDED IN PREVIOUS YEAR (CurIYr-1)
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    SUMHP=0.0
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)

      !RECCLCAC = EQUIPMENT CLASS NUMBER FOR CENTRAL AC
      !RECCLEHP = EQUIPMENT CLASS NUMBER FOR ELECTRIC HP
      IF (RTCLNAME(RECCL).EQ.'ROOM_AIR') THEN
        EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*RACSAT(B,R)*RACUnits(B,R))
      ELSEIF (RTCLNAME(RECCL).EQ.'CENT_AIR') THEN
        RECCLCAC=EQC+RTCLEUPT(EU)
        EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*CACSAT(B,R))
      ELSE
        !IF NOT ROOM_AIR OR CENT_AIR, MUST BE HEAT PUMP.
        ! ADJUST FOR HEAT PUMPS CALCULATED IN HEATING SUBROUTINE
        ! RECCLHHP = HEAT PUMP RECORD NUMBER FROM THE HEATING DATA
        ! RECCLEHP = ELEC HEAT PUMP RECORD NUMBER FROM COOLING
        IF (RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
          RECCLEHP=EQC+RTCLEUPT(EU)
        ENDIF
        RECCLHHP=RTCLPNTR(RECCL)
        EQCADD(CurCalYr,RECCL,B,R)=(EQCADD(CurCalYr,RECCLHHP,B,R))
        SUMHP=(SUMHP+EQCADD(CurCalYr,RECCL,B,R))
      ENDIF
    ENDDO

    !UPDATE CENTRAL AC ADDITIONS BASED ON TOTAL HP ADDITIONS
    IF (EQCADD(CurCalYr,RECCLCAC,B,R).LE.SUMHP) THEN
      EQCADD(CurCalYr,RECCLCAC,B,R)=(EQCADD(CurCalYr,RECCLEHP,B,R)*CACFACT)
    ELSE
      EQCADD(CurCalYr,RECCLCAC,B,R)=(EQCADD(CurCalYr,RECCLCAC,B,R)-SUMHP)
    ENDIF
  ENDDO
ENDDO

!********************************************************************
! CALCULATE SURVIVING EQUIPMENT REPLACED FOR RECS-YEAR VINTAGE PRIOR TO CurIYr-1
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (RTCLPNTR(RECCL).GT.0) THEN
        !HEAT PUMP SPACE COOLING USES HEAT PUMP SPACE HEATING DATA
        RECCLHHP=RTCLPNTR(RECCL)
        EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCLHHP,B,R))
        EQCSUR(CurCalYr,RECCL,B,R)=(EQCSUR(CurCalYr,RECCLHHP,B,R))
      ELSE
        !CALCULATE DATA FOR NON-HEAT PUMPS
        IF (CurCalYr.EQ.RECSYear+1) THEN
          EQCND90(CurCalYr,RECCL,B,R)=(EQCESE(RECSYear,RECCL,B,R)*HDR(B))
        ELSE
          EQCND90(CurCalYr,RECCL,B,R)=(EQCND90(CurCalYr-1,RECCL,B,R)*HDR(B))
        ENDIF

        IF ((B.EQ.1).AND.(RTCLNAME(RECCL).EQ.'CENT_AIR')) THEN
          IF ((EQCPN90(CurCalYr,RECCL,B,R)/EH(CurCalYr,B,R)).GE.max_cenac_pen) THEN
            EQCPN90(CurCalYr,RECCL,B,R)=(EQCND90(CurCalYr,RECCL,B,R)-EQCND90(CurCalYr,RECCL,B,R))
          ELSE
            EQCPN90(CurCalYr,RECCL,B,R)=(EQCND90(CurCalYr,RECCL,B,R)*(1.+CACPR(R))-EQCND90(CurCalYr,RECCL,B,R))
          ENDIF !90% central AC penetration into remaining RECSYear housing stock
        ELSE
          EQCPN90(CurCalYr,RECCL,B,R)=0.0
        ENDIF

        IF (CurCalYr.EQ.RECSYear+1) THEN
          EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
           *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
        ELSE
          EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
           EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
        ENDIF

        !COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
        IF (CurCalYr.GT.RECSYear+1) THEN
          DO Y=RECSYear+1,(CurCalYr-1)
            TEMP=CurCalYr-Y
            ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
             -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
            EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
             (EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
             EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
             EQCPN90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
          ENDDO
        ELSE
          EQCRP90RP(CurCalYr,RECCL,B,R)=EQCPN90(CurCalYr,RECCL,B,R)
        ENDIF

        IF (CurCalYr.GT.RECSYear+1) THEN
          EQCRP90RP(CurCalYr,RECCL,B,R)=EQCRP90RP(CurCalYr,RECCL,B,R)+EQCPN90(CurCalYr,RECCL,B,R)
        ENDIF

        IF (CurCalYr.GT.RECSYear+1) THEN
          DO Y=RECSYear+1,(CurCalYr-1)
            HSR=HDR(B)**(CurCalYr-Y)
            ESR=SVRTE(RTALPHA(RECCL),CurCalYr-Y,RTK(RECCL),RTLAMBDA(RECCL))
            EQCSR90(CurCalYr,RECCL,B,R)= (&
             EQCSR90(CurCalYr,RECCL,B,R)+EQCRP90(Y,RECCL,B,R)* &
             ESR*HSR+EQCRP90RP(Y,RECCL,B,R)*ESR*HSR &
             + EQCPN90(Y,RECCL,B,R)*ESR*HSR)

            !*******************************************************************
            !CALCULATE SURVIVING NEW COOLING EQUIPMENT ADDED PRIOR TO CurIYr-1 TO ESTIMATE NH
            ! SA REPRESENTS NH AT PREVYR-1
            ! CUMULATE SURVIVING NEW CACS ADDED & REPLACED PRIOR TO CurIYr-1
            ! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) -  SURV.EQUIP(CACSUR)
            !*******************************************************************
            SA(EQC,B,R) = (SA(EQC,B,R) + EQCADD(Y,RECCL,B,R)*HSR)
            EQCSUR(CurCalYr,RECCL,B,R) = ( EQCSUR(CurCalYr,RECCL,B,R) + &
             ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR)))
          ENDDO
        ENDIF
      ENDIF  !RTCLPNTR.GT.0
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

!*******************************************************************
! CALCULATE REPLACEMENTS FOR RECS-YEAR VINTAGE IN CurIYr-1
!*******************************************************************
!CALCULATE REPLACEMENT COOLING EQUIPMENT FOR NEW VINTAGE IN CurIYr-1
! NOTE: REPLACES WITH LIKE
! NOTE: FOR NEW HOUSES (NH) - PREVYR REPRESENTS THE LAGGED VALUE
!*******************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (RTCLPNTR(RECCL).GT.0) THEN
        !HEAT PUMP SPACE COOLING USES HEAT PUMP SPACE HEATING DATA
        RECCLHHP=RTCLPNTR(RECCL)
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRP90(CurCalYr,RECCLHHP,B,R))
        EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCLHHP,B,R))
        IF (B.EQ.1) OEQCRP90(CurCalYr,RECCL,B,R)=(EQCRP90(CurCalYr,RECCLHHP,B,R))
        IF (B.EQ.1) OEQCRP90R(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCLHHP,B,R))
        EQCREP(CurCalYr,RECCL,B,R)=(EQCREP(CurCalYr,RECCLHHP,B,R))
        IF (B.EQ.1) OEQCREP(CurCalYr,RECCL,B,R)=(EQCREP(CurCalYr,RECCLHHP,B,R))
      ELSE
        !CALCULATE DATA FOR NON-HEAT PUMPS
        ! Calculate replacement equipment from original base-year stock
        IF (B.EQ.1) OEQCRP90(CurCalYr,RECCL,B,R)=EQCRP90(CurCalYr,RECCL,B,R)
        IF (B.EQ.1) OEQCRP90R(CurCalYr,RECCL,B,R)=EQCRP90RP(CurCalYr,RECCL,B,R)
        EQCREP(CurCalYr,RECCL,B,R)=(SA(EQC,B,R)-EQCSUR(CurCalYr,RECCL,B,R))
        IF (B.EQ.1) OEQCREP(CurCalYr,RECCL,B,R)=(SA(EQC,B,R)-EQCSUR(CurCalYr,RECCL,B,R))
      ENDIF
    ENDDO
  ENDDO  !B
ENDDO  !R

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DO Y=CurCalYr,EndYr
        !VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
        TEMP=Y-CurCalYr
        HSR=HDR(B)**(TEMP)
        ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
        EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!Initialize new shell variables
NCSHELL(CurCalYr,1:mNumCR-2,1:mNumBldg)=0.0
NEWSHELLWTC(CurCalYr,1:mNumBldg,1:mNumCR-2)=0.0
NEWADDWTC(CurCalYr,1:mNumBldg,1:mNumCR-2)=0.0
ANUMC(1:mNumCR-2,1:mNumBldg)=0.0
ADENC(1:mNumCR-2,1:mNumBldg)=0.0

!ADD TOTAL COOLING EQUIPMENT AND COMPUTE NEW SHELL EFFICIENCY
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+2,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      NEWSHELLWTC(CurCalYr,B,R)=NEWSHELLWTC(CurCalYr,B,R)+ &
       EQCADD(CurCalYr,RECCL,B,R)*CSHELL(CurCalYr,EQC,B,R)
      NEWADDWTC(CurCalYr,B,R)=NEWADDWTC(CurCalYr,B,R)+EQCADD(CurCalYr,RECCL,B,R)
    ENDDO
  ENDDO
ENDDO

!COMPUTE SHELL AVERAGE FOR EACH FUEL TYPE
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    IF (NEWADDWTC(CurCalYr,B,R).GT.0.0) THEN
      NCSHELL(CurCalYr,R,B)=NEWSHELLWTC(CurCalYr,B,R)/NEWADDWTC(CurCalYr,B,R)
    ELSE
      NCSHELL(CurCalYr,R,B)=1.0
    ENDIF
  ENDDO
ENDDO

!Adjustments to house size
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    ! cooling non-HP
    EXSQFTADJ(CurCalYr,B,D,3)=(ELASTIC(3,D)*((EXSQRFOOT(CurCalYr,B,D)-EXSQRFOOT(RECSYear,B,D))/ &
     EXSQRFOOT(RECSYear,B,D))*NCSHELL(CurCalYr,D,B))+1
    ! cooling HP
    EXSQFTADJ(CurCalYr,B,D,4)=(ELASTIC(4,D)*((EXSQRFOOT(CurCalYr,B,D)-EXSQRFOOT(RECSYear,B,D))/ &
     EXSQRFOOT(RECSYear,B,D))*NCSHELL(CurCalYr,D,B))+1
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE AVERAGE NEW COOLING SHELL INDEX
!*******************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          ANUMC(R,B)=ANUMC(R,B)+(NCSHELL(Y,R,B)* &
          EQCADD(Y,RECCL,B,R))
          ADENC(R,B)=ADENC(R,B)+(EQCADD(Y,RECCL,B,R))
        ENDDO
      ENDIF
    ENDDO
  ENDDO
ENDDO

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    IF (ADENC(R,B).LE.0) THEN
      ACSHELL(CurCalYr,R,B)=ACSHELL(CurCalYr-1,R,B)
    ELSE
      ACSHELL(CurCalYr,R,B)=ANUMC(R,B)/ADENC(R,B)
    ENDIF
    IF (ACSHELL(CurCalYr,R,B).LT.LIMIT) ACSHELL(CurCalYr,R,B)=LIMIT
    IF (CurCalYr.LE.RECSYear+1) THEN
      ACSHELL(CurCalYr,R,B)=NCSHELL(CurCalYr,R,B)
    ENDIF
  ENDDO
ENDDO

!*******************************************************************
!AGGREGATE COOLING SYSTEMS FOR INVESTMENT ANALYSIS
!*******************************************************************
Y=CurCalYr
NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)
DO B=1,mNumBldg
  DO r=1,mNumCR-2
    TYPE=RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          X=1.0
          IF (RECCL.EQ.11) THEN !nHeatClasses + RTCLEQCL(ROOM_AIR) = 10 + 1 = 11  !NoKero	!TODO - replace 11 with parameter (nHeatClasses + 1)
            X=1.0	!TODO - Is this necessary?
            HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
            HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r))
          ELSE
            HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHRC(Y,EQT,B,r)*EQCADD(Y,RECCL,B,r))
            HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r))
          ENDIF
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 5
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,66)'5,',EU, ',',EQC, ',','X,',B,',',R,',',CurCalYr,',',RECTY,',',TYPE,',',RECCL,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',',EQCADD(Y,RECCL,B,r),',',HEATINGTYPEPURCH(Y,TYPE,B,R,1),',',HEATINGTYPEPURCH(Y,TYPE,B,R,2),',',EQCREP(Y,RECCL,B,r),',',EQCRP90RP(Y,RECCL,B,r),',',EQCRP90(Y,RECCL,B,r),',','X,','X,','X,','X,','X'
              66 FORMAT(a,I,a,I,a,a,  I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF  
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE RCLADD


!==============================================================================
! SPACE COOLING CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE RCLCON
IMPLICIT NONE

REAL*4 ALPHA,ALPHA2,ef1,ef2,ef3,TEMP,TEMP1,TEMP2,CDDFACT(mNumCR)
REAL*4 rba,rbr,rbn
INTEGER B, D, F, Y, YEAR,CYEAR,S,R
INTEGER EU, EUPR, EQC, RECCL, FCON, FDIM, EQCGHP, EQCEHP, V
INTEGER RECCLGHP, RECCLEHP

!*******************************************************************
!EQUIPMENT 1=ROOM_AIR 2=CENT_AIR 3=ELEC_HP 4=GEO_HP 5=NG_HP - Y,E,B,D
!CONSUMPTION FUEL 1=ELECTRICITY 2=NATURAL GAS
!*******************************************************************

EU = 2  !space cooling
EUPR = 2
ALPHA = -.15; ef1 = .5; ef2 = .35; ef3 = .15 !own-price elasticity and distribution
ALPHA2 = -.15  !cooling shell adjustment

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!Apply weather elasticity factor for space cooling	!TODO - HDDFACT tests for CurCalYr.LE.IJUMPCALYR; should this?
DO D=1,mNumCR-2
  CDDFACT(D)=(CDDADJ(CurCalYr,D)/CDDADJ(RECSYear,D))**1.50 !A 10% increase in CDD would increase space cooling consumption by 15% (e.g., 1.10^1.50=1.15)
ENDDO

!********************************************************************
!COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!********************************************************************
IF (CurCalYr.GE.RECSYear+1) THEN
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    RTBASEFF(CurCalYr,RECCL) = STKEFF(CurCalYr,RECCL)
  ENDDO
ENDIF

!********************************************************************
!CALCULATE NEW AND AVERAGE UECS
!********************************************************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    !SEARCH THE SPACE COOLING SECTION OF THE DATA (EU=2)
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)

      !S IS USED IN THE EXSQFTADJ CALCULATION AND DIFFERS DEPENDING ON WHETHER THE FUEL FOR HEATING IS ELECTRIC (2) OR FOSSIL (1)
      ! SEE RSMISC.TXT FOR "ELASTIC" INPUTS THAT MODIFY THE SQFT ADJUSTMENTS BY TYPE OF FUEL (THERE ARE ALSO SEPARATE ADJUSTMENTS FOR
      ! CENTRAL AC (3), HP COOLING (4), AND FURNACE FANS (5)
      IF (EQC.LT.3) THEN !ROOM_AIR and CENT_AIR
        S=3
      ELSE !ELEC_HP, GEO_HP, NG_HP
        S=4
      ENDIF

      EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)*EXSQFTADJ(CurCalYr,B,D,S)* &
       ( RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )
      EQCSIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       ( RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )
      IF (WTEQCEFFN(CurCalYr,RECCL,B,D) .GT. 0.0) THEN
        EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
         WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL) ! &
        EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
         WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
      ELSE
        EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
        EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
      ENDIF

      IF (WTEQCEFFHV(CurCalYr,RECCL,B,D) .GT. 0.0) THEN
        EQCHVUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) &
         *WTEQCEFFHV(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL) &
         *WTEQCSQFHV(CurCalYr,RECCL,B,D)
        EQCHVIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) &
         *WTEQCEFFHV(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
      ELSE
        EQCHVUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)*WTEQCSQFHV(CurCalYr,RECCL,B,D)
        EQCHVIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
      ENDIF

      IF (WTEQCEFFR(CurCalYr,RECCL,B,D) .GT. 0.0) THEN
        EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
         WTEQCEFFR(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)*EXSQFTADJ(CurCalYr,B,D,S)
        EQCRIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
         WTEQCEFFR(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
      ELSE
        EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
        EQCRIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
      ENDIF

      IF (CurCalYr .EQ. RECSYear+1) THEN
        EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
        EQCAHVUEC(CurCalYr,RECCL,B,D)=EQCHVUEC(CurCalYr,RECCL,B,D)
      ELSE
        TEMP=0.0
        TEMP1=0.0
        TEMP2=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
          TEMP1=TEMP1+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)
          TEMP2=TEMP2+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D) + EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (EQC.EQ.1) THEN !ROOM_AIR
          IF (TEMP2.LE.0.0) THEN
            EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
          ELSE
            EQCAUEC(CurCalYr,RECCL,B,D)=0.0
            DO Y=RECSYear,CurCalYr-1
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+ &
               ((EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
               (EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)+ &
               EQR90RPFUT(Y,CurCalYr,RECCL,B,D)*EQCNIUEC(Y,RECCL,B,D))/TEMP2
            ENDDO
          ENDIF
        ELSE
          IF (TEMP.LE.0.0) THEN
            EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
          ELSE
            EQCAUEC(CurCalYr,RECCL,B,D)=0.0
            DO Y=RECSYear,CurCalYr-1
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+ &
               (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D)+ &
               EQR90RPFUT(Y,CurCalYr,RECCL,B,D)*EQCNIUEC(Y,RECCL,B,D))/TEMP
            ENDDO
          ENDIF
        ENDIF

        IF (EQC.GT.1) THEN
          IF (TEMP1.LE.0.0) THEN
            EQCAHVUEC(CurCalYr,RECCL,B,D)=EQCHVUEC(CurCalYr,RECCL,B,D)
          ELSE
            EQCAHVUEC(CurCalYr,RECCL,B,D)=0.0
            DO Y=RECSYear,CurCalYr-1
              EQCAHVUEC(CurCalYr,RECCL,B,D)=EQCAHVUEC(CurCalYr,RECCL,B,D)+ &
               (EQADDFUT(Y,CurCalYr,RECCL,B,D)*EQCHVUEC(Y,RECCL,B,D)+ &
               EQREPFUT(Y,CurCalYr,RECCL,B,D)*EQCNUEC(Y,RECCL,B,D))/TEMP1
            ENDDO
          ENDIF
        ENDIF

      ENDIF  !CurCalYr
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !D

!*******************************************************************
!CALCULATE AVERAGE EQUIPMENT EFFICIENCY
!*******************************************************************
DO B=1,mNumBldg	!TODO - why B first?
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP .GT. 0.0) THEN
          WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            IF (EQC.EQ.1) THEN !ROOM_AIR
              WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
               (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
               ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
               EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
            ELSE
              WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
               EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D)+ &
               EQADDFUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFHV(Y,RECCL,B,D)+ &
               (EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
               EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))/TEMP
            ENDIF
          ENDDO
        ELSE
          WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFN(CurCalYr,RECCL,B,D)
        ENDIF
      ENDIF
    ENDDO
  ENDDO  !D
ENDDO  !B

!INITIALIZE SPACE COOLING CONSUMPTION VARIABLES
Driver(CurIYr,1:NCLFL,1:mNumCR-2,1:mNumBldg) = 0.0
COOLCN(CurIYr,1:NCLFL,1:mNumCR-2) = 0.0
COOLCNWT(CurIYr,1:NCLFL,1:mNumCR-2,1:mNumBldg) = 0.0
COOLCNIN(CurIYr,1:NCLFL,1:mNumCR-2,1:mNumBldg) = 0.0

!FIND EQUIPMENT CLASS FOR AIR-SOURCE AND GROUND-SOURCE HEAT PUMPS	!TODO - still needed if no longer estimating geothermal energy consumption?
DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  IF (RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
    EQCEHP=RTCLEQCL(RECCL)
    RECCLEHP=EQCEHP+RTCLEUPT(EU)
  ELSEIF (RTCLNAME(RECCL).EQ.'GEO_HP') THEN
    EQCGHP=RTCLEQCL(RECCL)
    RECCLGHP=EQCGHP+RTCLEUPT(EU)
  ENDIF
ENDDO

!*******************************************************************
!CALCULATE SPACE COOLING CONSUMPTION
!*******************************************************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC = RTCLEQCL(RECCL)
      F=RTFUEL(RECCL) !F = RTEK FUEL NUMBER
      FCON=FCLCON(F) !FCON = CONSUMPTION FUEL NUMBER FOR COOLING
      EQCEQCN(CurIYr,RECCL,B,D)=0.0
      IF (CurCalYr.EQ.RECSYear+1) THEN
        IF (EQC.EQ.1) THEN !ROOM_AIR
          COOLCN(CurIYr,FCON,D)=COOLCN(CurIYr,FCON,D)+ CDDFACT(D)*( &
           ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)* &
           (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
           EQCNUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

          COOLCNWT(CurIYr,FCON,D,B)=COOLCNWT(CurIYr,FCON,D,B)+CDDFACT(D)*( &
           ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)* &
           (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
           EQCNUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

          COOLCNIN(CurIYr,FCON,D,B)=COOLCNIN(CurIYr,FCON,D,B)+( &
           ((EQCESE(CurCalYr,RECCL,B,D)*EQCSIUEC(CurCalYr,RECCL,B,D)* &
           (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
           EQCNUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRIUEC(CurCalYr,RECCL,B,D) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))))

          Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
           (EQCESE(CurCalYr,RECCL,B,d)+EQCADD(CurCalYr,RECCL,B,d)+ &
           EQCRP90RP(CurCalYr,RECCL,B,d)+EQCRP90(CurCalYr,RECCL,B,d))

          EQCEQCN(CurIYr,RECCL,B,D)=CDDFACT(D)*(((EQCESE(CurCalYr,RECCL,B,D)* &
           EQCSUEC(CurCalYr,RECCL,B,D)* &
           (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))) + &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)* &
           (NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCRP90(CurCalYr,RECCL,B,D)* &
           EQCRUEC(CurCalYr,RECCL,B,D)*(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))
        ELSE ! EQC <> 1
          COOLCN(CurIYr,FCON,D)=COOLCN(CurIYr,FCON,D)+ CDDFACT(D)*( &
           ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)* &
           (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
           EQCHVUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

          COOLCNWT(CurIYr,FCON,D,B)=COOLCNWT(CurIYr,FCON,D,B)+CDDFACT(D)*( &
           ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)* &
           (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
           EQCHVUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

          COOLCNIN(CurIYr,FCON,D,B)=COOLCNIN(CurIYr,FCON,D,B)+( &
           ((EQCESE(CurCalYr,RECCL,B,D)*EQCSIUEC(CurCalYr,RECCL,B,D)* &
           (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
           EQCHVIUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRIUEC(CurCalYr,RECCL,B,D) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))))

          Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+ &
           (EQCESE(CurCalYr,RECCL,B,d)+EQCADD(CurCalYr,RECCL,B,d)+ &
           EQCRP90RP(CurCalYr,RECCL,B,d)+EQCRP90(CurCalYr,RECCL,B,d))

          EQCEQCN(CurIYr,RECCL,B,D)=CDDFACT(D)*(((EQCESE(CurCalYr,RECCL,B,D)* &
           EQCSUEC(CurCalYr,RECCL,B,D)* &
           (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))) + &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
           *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
           (EQCADD(CurCalYr,RECCL,B,D)*EQCHVUEC(CurCalYr,RECCL,B,D)* &
           (NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCRP90(CurCalYr,RECCL,B,D)* &
           EQCRUEC(CurCalYr,RECCL,B,D)*(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))
        ENDIF ! EQC
      ELSE ! CurCalYr <> RECSYear+1
        IF (F.EQ.4) THEN
          ALPHA=-0.30  !was -0.15 prior to American Recovery and Reinvestment Act of 2009 (ARRA) stimulus; permanently affects price elasticity (but not rebound) based on the smart grid concept
        ELSE
          ALPHA=-0.15
        ENDIF

        rba=(rtbaseff(RECSYear,RECCL)*wteqceffa(CurCalYr,RECCL,b,d))**alpha2
        rbr=(rtbaseff(RECSYear,RECCL)*wteqceffr(CurCalYr,RECCL,b,d))**alpha2
        rbn=(rtbaseff(RECSYear,RECCL)*wteqceffn(CurCalYr,RECCL,b,d))**alpha2

        IF (EQC.EQ.1) THEN !ROOM_AIR
          COOLCN(CurIYr,fcon,d)=COOLCN(CurIYr,fcon,d)+CDDFACT(D)* (( &
           (EQCESE(CurCalYr,RECCL,b,d)*eqcsuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2)) + &
           (eqcadd(CurCalYr,RECCL,b,d)*eqcnuec(CurCalYr,RECCL,b,d)* &
           (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr + &
           (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
           *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba + &
           (EQCREP(CurCalYr,RECCL,b,d) *eqcnuec(CurCalYr,RECCL,b,d)* &
           (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (EQCSUR(CurCalYr,RECCL,b,d) *eqcauec(CurCalYr,RECCL,b,d) &
           *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

          COOLCNWT(CurIYr,fcon,d,b)=COOLCNWT(CurIYr,fcon,d,b)+CDDFACT(D)*(( &
           (EQCESE(CurCalYr,RECCL,b,d)*eqcsiuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2)) + &
           (eqcadd(CurCalYr,RECCL,b,d)*eqcnuec(CurCalYr,RECCL,b,d)* &
           (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr + &
           (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
           *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba + &
           (EQCREP(CurCalYr,RECCL,b,d) *eqcnuec(CurCalYr,RECCL,b,d)* &
           (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (EQCSUR(CurCalYr,RECCL,b,d) *eqcauec(CurCalYr,RECCL,b,d) &
           *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

          COOLCNIN(CurIYr,fcon,d,b)=COOLCNIN(CurIYr,fcon,d,b)+(( &
           (EQCESE(CurCalYr,RECCL,b,d)*eqcsiuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
           (eqcadd(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)* &
           (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
           (eqcrp90(CurCalYr,RECCL,b,d)*eqcriuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
           (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
           (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
           *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B)))+ &
           (EQCREP(CurCalYr,RECCL,b,d) *eqcniuec(CurCalYr,RECCL,b,d)* &
           (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
           (EQCSUR(CurCalYr,RECCL,b,d) *eqcauec(CurCalYr,RECCL,b,d) &
           *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B)))))

          Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+ &
           (EQCESE(CurCalYr,RECCL,B,d)+EQCADD(CurCalYr,RECCL,B,d)+ &
           EQCRP90RP(CurCalYr,RECCL,B,d)+EQCRP90(CurCalYr,RECCL,B,d)+ &
           EQCSR90(CurCalYr,RECCL,b,d)+EQCREP(CurCalYr,RECCL,b,d)+ &
           EQCSUR(CurCalYr,RECCL,b,d))

          EQCEQCN(CurIYr,RECCL,b,d)=CDDFACT(D)*( &
           ((EQCESE(CurCalYr,RECCL,b,d)*eqcsuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))+ &
           (eqcadd(CurCalYr,RECCL,b,d)*eqcnuec(CurCalYr,RECCL,b,d)* &
           (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn  + &
           (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr+ &
           (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn+ &
           (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba  + &
           (EQCREP(CurCalYr,RECCL,b,d)*eqcnuec(CurCalYr,RECCL,b,d)* &
           (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn+ &
           (EQCSUR(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d)* &
           (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))
        ELSE
          COOLCN(CurIYr,fcon,d)=COOLCN(CurIYr,fcon,d)+CDDFACT(D)* (( &
           (EQCESE(CurCalYr,RECCL,b,d)*eqcsuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2)) + &
           (eqcadd(CurCalYr,RECCL,b,d)*eqchvuec(CurCalYr,RECCL,b,d)* &
           (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr + &
           (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
           *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba + &
           (EQCREP(CurCalYr,RECCL,b,d) *eqcnuec(CurCalYr,RECCL,b,d)* &
           (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (EQCSUR(CurCalYr,RECCL,b,d) *eqcahvuec(CurCalYr,RECCL,b,d) &
           *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

          coolcnwt(CurIYr,fcon,d,b)=coolcnwt(CurIYr,fcon,d,b)+CDDFACT(D)*(( &
           (EQCESE(CurCalYr,RECCL,b,d)*eqcsuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2)) + &
           (eqcadd(CurCalYr,RECCL,b,d)*eqchvuec(CurCalYr,RECCL,b,d)* &
           (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr + &
           (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
           *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba + &
           (EQCREP(CurCalYr,RECCL,b,d) *eqcnuec(CurCalYr,RECCL,b,d)* &
           (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
           (EQCSUR(CurCalYr,RECCL,b,d) *eqcahvuec(CurCalYr,RECCL,b,d) &
           *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

          coolcnin(CurIYr,fcon,d,b)=coolcnin(CurIYr,fcon,d,b)+(( &
           (EQCESE(CurCalYr,RECCL,b,d)*eqcsiuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
           (eqcadd(CurCalYr,RECCL,b,d)*eqchviuec(CurCalYr,RECCL,b,d)* &
           (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
           (eqcrp90(CurCalYr,RECCL,b,d)*eqcriuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
           (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
           (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
           *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B)))+ &
           (EQCREP(CurCalYr,RECCL,b,d) *eqcniuec(CurCalYr,RECCL,b,d)* &
           (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
           (EQCSUR(CurCalYr,RECCL,b,d) *eqcahvuec(CurCalYr,RECCL,b,d) &
           *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B)))))

          Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+ &
           (EQCESE(CurCalYr,RECCL,B,d)+EQCADD(CurCalYr,RECCL,B,d)+ &
           EQCRP90RP(CurCalYr,RECCL,B,d)+EQCRP90(CurCalYr,RECCL,B,d)+ &
           EQCSR90(CurCalYr,RECCL,b,d)+EQCREP(CurCalYr,RECCL,b,d)+ &
           EQCSUR(CurCalYr,RECCL,b,d))

          eqceqcn(CurIYr,RECCL,b,d)=CDDFACT(D)*( &
           ((EQCESE(CurCalYr,RECCL,b,d)*eqcsuec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))+ &
           (eqcadd(CurCalYr,RECCL,b,d)*eqchvuec(CurCalYr,RECCL,b,d)* &
           (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn  + &
           (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr+ &
           (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn+ &
           (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d)* &
           (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba  + &
           (EQCREP(CurCalYr,RECCL,b,d)*eqcnuec(CurCalYr,RECCL,b,d)* &
           (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn+ &
           (EQCSUR(CurCalYr,RECCL,b,d)*eqcahvuec(CurCalYr,RECCL,b,d)* &
           (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
           RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

        ENDIF  !EQC
      ENDIF  !CurCalYr
    ENDDO  !RECCL

  ENDDO  !B
ENDDO  !D

DO R=1,mNumCR-2
  DO FCON=1,NCLFL
    DO B=1,mNumBldg
      IF (Driver(CurIYr,FCON,R,B).GT.0) &
       COOLCNIN(CurIYr,FCON,R,B)=COOLCNIN(CurIYr,FCON,R,B)/Driver(CurIYr,FCON,R,B)
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE RCLCON


!==============================================================================
! CLOTHES WASHER CHOICE SUBROUTINE
!==============================================================================
SUBROUTINE RCWTEC
IMPLICIT NONE

REAL*4 TCWSHR(mNumBldg,mNumCR-2)  !share of top-load clothes washers
REAL*4 FCWSHR(mNumBldg,mNumCR-2)  !share of front-load clothes washers
REAL*4 RTEFFAC(2)
REAL*4 DECAY,OPCOST(2)
REAL*4 EQWTN(nClWashTypes,mNumBldg,mNumCR),EQWTR(nClWashTypes,mNumBldg,mNumCR), &  !EqpParam
 TOTEWTN(nClWashClasses,mNumBldg,mNumCR),TOTEWTR(nClWashClasses,mNumBldg,mNumCR)  !EqpParam
REAL*4 DENOM, DENOM2, SUM,SUM1
REAL*4 EQCOST,CAPITAL,RETAIL,BASEUSE,BASEMEF
INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
INTEGER RECAR(nClWashTypes),EQTAR(nClWashTypes)  !EqpParam

!********************************************************************
! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES	!TODO - combine/optimize similar code
!********************************************************************
EU=3  !clothes washing
EUPR=5

ALPHA1=-0.50
BASEMEF=1.57 !RECS base-year stock-average efficiency [clothes washer (Integrated) Modified Energy Factor, or MEF]; 1.57 for top-loading (72% TCW_SHR in 2020)or 2.76 for front-loading (28% FCW_SHR in 2020)->1.90 weighted average; RSEFF01.txt and RSSTKEFF.txt use top-loading clothes washer machine energy, so using TCW installed base efficiency here
BASEUSE=4.0125 !Annual energy use (MMBtu) of clothes washers (+ clothes dryers?) in RECS base year, or just average UEC of a single clothes washer?	!TODO - update; used since AEO2008

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!COMPUTE DECAY RATE USED TO COMPUTE LIFE-CYCLE COST (FIRST ITERATION ONLY)
IF (CURITR.EQ.1) THEN
  DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
ENDIF

!Initialize arrays
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    NCWLOAD(CurCalYr,R,B)=0.0
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      TOTEWTN(EQC,B,R)=0.0
      TOTEWTR(EQC,B,R)=0.0
      WTEQCEFFN(CurCalYr,RECCL,B,R)=0.0
      WTEQCEFFR(CurCalYr,RECCL,B,R)=0.0
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE OPERATING COSTS, LIFE-CYCLE COSTS, EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!********************************************************************
!VARIABLES USED THIS SECTION:
!RECTY          = RECORD NUMBER IN RSMEQP FILE
!RTTYEUPT(EU)   = LAST RECORD IN SPACE COOLING (EU=2)
!RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES WASHING (EU=3)

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          EQT = RTEQTYPE(RECTY)
          EQC = RTTYEQCL(RECTY)
          RECCL = RTCLEUPT(EU)+EQC
          F = RTFUEL(RECCL)

          !COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
          IF (RTEQEFF(RECTY).NE.0.0) THEN
            RTEFFAC(1)=RTEQEFF(RECTY)/EQCEFF(CurCalYr,RECCL)
            RTEFFAC(2)=RTEQEFF(RECTY)/RTBASEFF(RECSYear,RECCL)
          ELSE
            RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
          ENDIF

          !SET CAPITAL COSTS
          ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
          ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
          IF (COSTTRSW.EQ.1) THEN
            CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
          ELSE
            CAPITAL = RTEQCOST(RECTY)
          ENDIF

          !CALCULATE OPERATING COSTS
          OPCOST(1)=PRICES(F,R,CurCalYr)*(BASEUSE*(BASEMEF/CWMEF(RECTY))) !new	!TODO - Is this the same as equation B-67 in 2022 RDM documentation? If so, why hard-coded BASEFF and not EQCUEC[r,eg,b] values from RSUEC.txt input file?
          OPCOST(2)=PRICES(F,R,CurCalYr)*(BASEUSE*(BASEMEF/CWMEF(RECTY))) !existing

          !CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
          IF ((CurCalYr.GT.2008).AND. &	!TODO - 2008 marks last year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)? Remove legacy energy bill code as necessary
           (PRICES(4,R,CurCalYr).GT.PRICES(4,R,RECSYear))) THEN
            HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
            ELIGBLE=HRDRATE - 0.07
            IF (ELIGBLE.GT.0.0) THEN
              HRDADJ= ELIGBLE * ((PRICES(4,R,CurCalYr)/PRICES(4,R,RECSYear))**ALPHA1 )
              BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
            ELSE
              BETA1DR(RECTY)=RTECBTA1(RECTY)
            ENDIF
          ELSE
            BETA1DR(RECTY)=RTECBTA1(RECTY)
          ENDIF

          !CALCULATE LIFE-CYCLE COSTS
          LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
          LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)

          !COMPUTE WEIGHTS FOR NEW EQUIPMENT TYPES
          EQWTN(EQT,B,R)=EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)*CAPITAL)+ &
           (RTECBTA2(RECTY)*OPCOST(2))+(RTECBTA3(RECTY)*LFCY(EQT,B,R,2)))
          TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)

          !COMPUTE WEIGHTS FOR REPLACEMENT EQUIPMENT TYPES
          EQWTR(EQT,B,R)=EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)*CAPITAL)+ &
           (RTECBTA2(RECTY)*OPCOST(1))+(RTECBTA3(RECTY)*LFCY(EQT,B,R,1)))
          TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 6  
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
               WRITE(661,67)'6,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,',EQWTN(EQT,B,R),',',EQWTR(EQT,B,R),',','X,',BETA1DR(RECTY),',',CAPITAL,',',RTECBIAS(RECTY),',',OPCOST(1),',',OPCOST(2),',','X,',RTECBTA2(RECTY),',',RTECBTA3(RECTY),',',LFCY(EQT,B,R,1),',',LFCY(EQT,B,R,2),',',TOTEWTR(EQC,B,R),',',TOTEWTN(EQC,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
               67 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  f,a,f,a,a,  f,a,f,a,f,a,f,a,f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF

        ENDIF  !RTCENDIV
      ENDIF  !CurCalYr
    ENDDO  !RECTY
  ENDDO  !B
ENDDO  !R

!********************************************************************
!CALCULATE NEW AND REPLACEMENT MARKET SHARES
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    TCWSHR(B,R)=0.
    FCWSHR(B,R)=0.
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          IF (EQT.LE.3) THEN  !refers to instances of CL_WASH_T in RSMEQP !techupdate - update hard-coded EQT value
            TCWSHR(B,R) = TCWSHR(B,R) + EQWTN(EQT,B,R)
          ELSE  !refers to instances of CL_WASH_F in RSMEQP
            FCWSHR(B,R) = FCWSHR(B,R) + EQWTN(EQT,B,R)
          ENDIF
        ENDIF !RTCENDIV
      ENDIF !CurCalYr
    ENDDO !RECTY
  ENDDO !B
ENDDO !R

!********************************************************************
!CALCULATE NEW AND REPLACEMENT MARKET SHARES	!TODO - why is this different from section above?
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          IF (EQT.LE.3) THEN  !refers to instances of CL_WASH_T in RSMEQP !techupdate - update hard-coded EQT value
            NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/TCWSHR(B,R))*TCW_SHR
            REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) ! choices the same for CL_WASH
          ELSE  !refers to instances of CL_WASH_F in RSMEQP
            NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/FCWSHR(B,R))*FCW_SHR
            REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) ! choices the same for CL_WASH
          ENDIF !EQT
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 7
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,68)'7,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',',TYPE,',','X,',EQWTN(EQT,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,',TCWSHR(B,R),',',FCWSHR(B,R),',',FCW_SHR,',',TCW_SHR,',','X'
              68 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  a,  a,  a,  a,  a,  f,a,f,a,f,a,f,a,a)
            CLOSE(661)
          ENDIF
        ENDIF !RTCENDIV
      ENDIF !CurCalYr
    ENDDO !RECTY

    !CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT CLOTHES WASHING EQUIPMENT
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DENOM =0
      DENOM2=0
      COUNT =0

      !TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
      ! INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE AND THEN COUNT VALID TYPES IN CURRENT END USE
      TYPE = RTTYPECT(EU)
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            TYPE=TYPE+1
            IF (RTTYEQCL(RECTY).EQ.EQC) THEN
              COUNT=COUNT+1
              EQT=RTEQTYPE(RECTY)
              RECAR(COUNT)=RECTY
              EQTAR(COUNT)=TYPE
              DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
              DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        SUM1=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
          SUM1=SUM1+(NEQTSHR(CurCalYr,TYPE,B,R)*LOADADJ(RECTY))
        ENDDO
        WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
        NCWLOAD(CurCalYr,R,B)=SUM1/DENOM
      ENDIF

      !COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
      IF (DENOM2.LE.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
      ENDIF

    ENDDO  !RECCL
  ENDDO !B
ENDDO !R

END SUBROUTINE RCWTEC


!==============================================================================
! CLOTHES WASHERS ADDED SUBROUTINE
!==============================================================================
SUBROUTINE RCWADD
IMPLICIT NONE

REAL*4 SA, HSR, ESR, SVRTE
REAL*4 LOADTOT(RECSYear:EndYr,mNumRTCl,mNumCR,mNumBldg)
INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EQT,NUMEQT,TYPE,RECTY,V

EU = 3  !clothes washing

!********************************************************************
!CALCULATE CLOTHES WASHERS ADDED IN CurCalYr (CurCalYr-1)
! CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS-YEAR VINTAGE PRIOR TO CurCalYr
!********************************************************************
!CUMULATE SURVIVING NEW CLOTHES WASHERS ADDED PRIOR TO CurCalYr TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW WASHERS ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-CLOTHES WASHERS)
!********************************************************************

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      !Initialize arrays
      EQC=RTCLEQCL(RECCL)
      EQCSR90(CurCalYr,RECCL,B,R)=0.0
      EQCSUR(CurCalYr,RECCL,B,R)=0.0
      EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
      ECWLOAD(CurCalYr,R,B) = 0.0
    ENDDO

    IF (CurCalYr.GT.RECSYear+1) THEN
      WASHNEW(CurCalYr,B,R)=WASHNEW(CurCalYr-1,B,R)*1.0000 !Average annual penetration rate of clothes washers into new homes (based on newest homes in RECS); penetration rate not increasing based on latest RECS, so set to 1.0000
    ENDIF
    
    IF (WASHNEW(CurCalYr,B,R).GT.1.0000) THEN
      WASHNEW(CurCalYr,B,R)=1.0000  !Prevents penetration of clothes washers into new homes since RECS year from exceeding 100%
    ENDIF
    
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*WASHNEW(CurCalYr,B,R))
      SA=0.0
      !******************************************************************
      !Calculate replacement equipment from original base-year stock
      !******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL)*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
        EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
        EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF
	  
      !COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
           -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
        ENDDO
      ENDIF
	  
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
          HSR=HDR(B)**(TEMP)
          SA=(SA+EQCADD(Y,RECCL,B,R)*HSR)
          EQCSUR(CurCalYr,RECCL,B,R)=(EQCSUR(CurCalYr,RECCL,B,R) + &
           (((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR))))
        ENDDO
      ENDIF
	  
      !*******************************************************************
      !CALCULATE REPLACEMENT CLOTHES WASHERS FOR NEW VINTAGE IN CurCalYr-1
      ! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
      ! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
      !*******************************************************************
      EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)

    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DO Y=CurCalYr,EndYr
        !VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
        TEMP=Y-CurCalYr
        HSR=HDR(B)**(TEMP)
        ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
        EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQCESEFUT(CurCalYr,Y,RECCL,B,R)=(EQCESE(CurCalYr,RECCL,B,R)*ESR*HSR)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!AGGREGATE CLOTHES WASHERS FOR INVESTMENT ANALYSIS
!********************************************************************
Y=CurCalYr
NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

DO B=1,mNumBldg
  DO r=1,mNumCR-2
    TYPE=RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
          HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
           REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 9
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,69)'9,',EU, ',',EQC, ',','X,',B,',',R,',',Y,',','X,',TYPE,',',RECCL,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',EQCADD(Y,RECCL,B,r),',','X,','X,',EQCREP(Y,RECCL,B,r),',',EQCRP90RP(Y,RECCL,B,r),',',EQCRP90(Y,RECCL,B,r),',','X,','X,','X,','X,','X'
              69 FORMAT(a,I,a,I,a,a,  I,a,I,a,I,a,a,  I,a,  I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  a,  f,a,f,a,f,a,a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE RCWADD


!==============================================================================
! CLOTHES WASHER CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE RCWCON
IMPLICIT NONE

REAL*4 ALPHA,ef1,ef2,ef3,TEMP,TEMP1
INTEGER B,E,D,EU,EUPR,RECCL,EQC,F,Y,R

EU = 3  !clothes washing
EUPR = 5  !no end-use price for clothes washers; map to clothes dryer price (assume usage pattern is similar)
alpha = 0.0; ef1 = .5; ef2 = .35; ef3 = .15

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!********************************************************************
!COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!********************************************************************
IF (CurCalYr.GE.RECSYear+1) THEN
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
  ENDDO
ENDIF

!********************************************************************
!CALCULATE NEW, REPLACEMENT, AND AVERAGE UECS
!********************************************************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (RTBASEFF(CurCalYr,RECCL)/RTBASEFF(RECSYear,RECCL))
      EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (WTEQCEFFN(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL))
      EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (WTEQCEFFR(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL))
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
        ECWLOAD(CurCalYr,D,B)=(EQCESE(CurCalYr,RECCL,B,D)+ &
         ((EQCADD(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D))* &
         NCWLOAD(CurCalYr,D,B)))/(EQCESE(CurCalYr,RECCL,B,D)+ &
         EQCADD(CurCalYr,RECCL,B,D)+ EQCRP90(CurCalYr,RECCL,B,D))
      ELSE
        TEMP=0.0
        TEMP1=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
          TEMP1=TEMP1+ EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)+EQCESEFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP.LE.0.0) THEN
          EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
        ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+ &
             ((EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
            ECWLOAD(CurCalYr,D,B)=ECWLOAD(CurCalYr,D,B)+ &
             (EQCESEFUT(Y,CurCalYr,RECCL,B,D)+((EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
             EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*NCWLOAD(Y,D,B)))/TEMP1
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE AVERAGE EQUIPMENT EFFICIENCY
!*******************************************************************
DO B=1,mNumBldg
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP .GT. 0.0) THEN
          WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+ &
             ((EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
          ENDDO
        ELSE
          WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFN(CurCalYr,RECCL,B,D)
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE CLOTHES WASHING CONSUMPTION
!*******************************************************************

!Initialize arrays
CSWCON(CurIYr,1:mNumCR-2)=0.0
CSWCONIN(CurIYr,1:mNumCR-2,1:mNumBldg)=0.
Driver2(CurIYr,1:mNumCR-2,1:mNumBldg)=0.
CSWCONWT(CurIYr,1:mNumCR-2,1:mNumBldg)=0.


DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      F=RTFUEL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        CSWCON(CurIYr,D)=CSWCON(CurIYr,D)+LEAPYR* &
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

        CSWCONWT(CurIYr,D,B)=CSWCONWT(CurIYr,D,B)+LEAPYR* &	!TODO - aside from the WT in name, this equation is the same as above for HTRCON; set equal?
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
          CSWCONIN(CurIYr,D,B)=CSWCONIN(CurIYr,D,B)+ &
           ((((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
           (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))))

          Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+ &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D))
        ENDIF

        EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR* &
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

      ELSE
        CSWCON(CurIYr,D)=CSWCON(CurIYr,D)+ LEAPYR* &
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
         (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

        CSWCONWT(CurIYr,D,B)=CSWCONWT(CurIYr,D,B)+LEAPYR* &
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
         (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

         IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
          EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
          EQCSR90(CurCalYr,RECCL,B,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
          EQCSUR(CurCalYr,RECCL,B,D).GT.0.) THEN
           CSWCONIN(CurIYr,D,B)=CSWCONIN(CurIYr,D,B)+ &
            ((((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
            (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
            (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
            (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
            (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
            (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
            (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))))

           Driver2(CurIYr,D,B)=Driver2(CurIYr,d,B)+ &
            (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
            EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
            EQCSR90(CurCalYr,RECCL,B,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
            EQCSUR(CurCalYr,RECCL,B,D))
         ENDIF

         EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR* &
          (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
          (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
          (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
          (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
          (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
          (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
          (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
          RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

      ENDIF  !CurCalYr
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !D

!CALCULATE INTENSITY VARIABLE FOR REPORT WRITER
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    IF (Driver2(CurIYr,R,B).GT.0) CSWCONIN(CurIYr,R,B)=CSWCONIN(CurIYr,R,B)/Driver2(CurIYr,R,B)
  ENDDO
ENDDO

END SUBROUTINE RCWCON


!==============================================================================
! DISHWASHING CHOICE SUBROUTINE
!==============================================================================
SUBROUTINE RDWTEC
IMPLICIT NONE

REAL*4 RTEFFAC(2)
REAL*4 DECAY,OPCOST(2)
REAL*4 EQWTN(nDishTypes,mNumBldg,mNumCR),EQWTR(nDishTypes,mNumBldg,mNumCR), &  !EqpParam
 TOTEWTN(nDishClasses,mNumBldg,mNumCR),TOTEWTR(nDishClasses,mNumBldg,mNumCR)  !EqpParam
REAL*4 DENOM, DENOM2, SUM,SUM1
REAL*4 EQCOST,CAPITAL,RETAIL
INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
INTEGER RECAR(nDishTypes),EQTAR(nDishTypes)  !EqpParam

!********************************************************************
! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES	!TODO - combine/optimize similar code
!********************************************************************

EU=4  !dishwashing
EUPR=9
ALPHA1=-0.50

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!COMPUTE DECAY RATE USED TO COMPUTE LIFE-CYCLE COST (FIRST ITERATION ONLY)
IF (CURITR.EQ.1) THEN
  DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
ENDIF

!INITIALIZE ARRAYS
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      TOTEWTN(EQC,B,R)=0.0
      TOTEWTR(EQC,B,R)=0.0
      WTEQCEFFN(CurCalYr,RECCL,B,R)=0.0
      WTEQCEFFR(CurCalYr,RECCL,B,R)=0.0
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE OPERATING COSTS, LIFE-CYCLE COSTS, EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!********************************************************************
!RECTY          = RECORD NUMBER IN RSMEQP FILE
!RTTYEUPT(EU)   = LAST RECORD IN CLOTHES WASHING (EU=3)
!RTTYEUPT(EU+1) = LAST RECORD IN DISHWASHING (EU=4)

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          F=RTFUEL(RECCL)

          !COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
          IF (RTEQEFF(RECTY).NE.0.0) THEN
            RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
          ELSE
            RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
          ENDIF
		  
          !SET CAPITAL COSTS
          ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
          ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
          IF (COSTTRSW.EQ.1) THEN
            CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
          ELSE
            CAPITAL = RTEQCOST(RECTY)
          ENDIF
	      
          !CALCULATE OPERATING COST
          OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)
          OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(2)
          
          !CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
          IF ((CurCalYr.GT.2008).AND. &	!TODO - 2008 marks last year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)? Remove legacy energy bill code as necessary
           (PRICES(4,R,CurCalYr).GT.PRICES(4,R,RECSYear))) THEN
            HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
            ELIGBLE=HRDRATE - 0.07
            IF (ELIGBLE.GT.0.0) THEN
              HRDADJ= ELIGBLE * ((PRICES(4,R,CurCalYr)/PRICES(4,R,RECSYear))**ALPHA1 )
              BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
            ELSE
              BETA1DR(RECTY)=RTECBTA1(RECTY)
            ENDIF
          ELSE
            BETA1DR(RECTY)=RTECBTA1(RECTY)
          ENDIF
		  
          !CALCULATE LIFE CYCLE COSTS
          LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
          LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)
		  
          EQWTN(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
           (BETA1DR(RECTY)*CAPITAL)+ &
           (RTECBTA2(RECTY)*OPCOST(2))+ &
           (RTECBTA3(RECTY)*LFCY(EQT,B,R,2)))
          TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
          EQWTR(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
           (BETA1DR(RECTY)*CAPITAL)+ &
           (RTECBTA2(RECTY)*OPCOST(1))+ &
           (RTECBTA3(RECTY)*LFCY(EQT,B,R,1)))
          TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
		  
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 10
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,75)'10,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,',EQWTN(EQT,B,R),',',EQWTR(EQT,B,R),',','X,',BETA1DR(RECTY),',',CAPITAL,',',RTECBIAS(RECTY),',',OPCOST(1),',',OPCOST(2),',','X,',RTECBTA2(RECTY),',',RTECBTA3(RECTY),',',LFCY(EQT,B,R,1),',',LFCY(EQT,B,R,2),',',TOTEWTR(EQC,B,R),',',TOTEWTN(EQC,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              75 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  f,a,f,a,a,  f,a,f,a,f,a,f,a,f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF

        ENDIF  !RTCENDIV
      ENDIF  !CurCalYr
    ENDDO  !RECTY
  ENDDO  !B
ENDDO  !R

!********************************************************************
!CALCULATE NEW AND REPLACEMENT MARKET SHARES
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY))THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC

          NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R))
          REQTSHR(CurCalYr,TYPE,B,R)=(EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R))

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 11
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,76)'11,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              76 FORMAT( a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    !CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT DISHWASHING EQUIPMENT
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DENOM=0
      DENOM2=0
      COUNT=0
    
      !TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
      ! INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE AND THEN COUNT VALID TYPES IN CURRENT END USE
      TYPE = RTTYPECT(EU)
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            TYPE=TYPE+1
            IF (RTTYEQCL(RECTY).EQ.EQC) THEN
              COUNT=COUNT+1
              EQT=RTEQTYPE(RECTY)
              RECAR(COUNT)=RECTY
              EQTAR(COUNT)=TYPE
              DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
              DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    
      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        SUM1=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
           SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
          SUM1=SUM1+(NEQTSHR(CurCalYr,TYPE,B,R)*LOADADJ(RECTY))
        ENDDO
        WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
      ENDIF
    
      !COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
      IF (DENOM2.LE.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
      ENDIF

    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

END SUBROUTINE RDWTEC


!==============================================================================
! DISHWASHERS ADDED SUBROUTINE
!==============================================================================
SUBROUTINE RDWADD
IMPLICIT NONE

REAL*4 SA, HSR, ESR, SVRTE
REAL*4 new_dw_pen, max_dw_pen
INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EQT,NUMEQT,TYPE,RECTY,V

EU = 4  !dishwashing
new_dw_pen = 1.0048  !Average annual penetration rate of dishwashers into new homes (based on newest homes in RECS)  !DISHNEWpen
max_dw_pen = 0.90  !Max 90% dishwasher penetration into remaining RECSYear housing stock (analyst judgment)

!********************************************************************
!CALCULATE CLOTHES WASHERS ADDED IN CurCalYr (CurCalYr-1)
! CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS-YEAR VINTAGE PRIOR TO CurCalYr
!********************************************************************
!CUMULATE SURVIVING NEW CLOTHES WASHERS ADDED PRIOR TO CurCalYr TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW WASHERS ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-CLOTHES WASHERS)
!********************************************************************

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      !Initialize arrays
      EQC=RTCLEQCL(RECCL)
      EQCSR90(CurCalYr,RECCL,B,R)=0.0
      EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
      EQCSUR(CurCalYr,RECCL,B,R)=0.0
    ENDDO

    IF (CurCalYr.GT.RECSYear+1) THEN
      DISHNEW(CurCalYr,B,R)=DISHNEW(CurCalYr-1,B,R)*new_dw_pen
    ENDIF

    IF (DISHNEW(CurCalYr,B,R).GT.1.0000) THEN
      DISHNEW(CurCalYr,B,R)=1.0000  !Prevents penetration of dishwashers into new homes since RECS year from exceeding 100%
    ENDIF

    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*DISHNEW(CurCalYr,B,R))
      SA=0.0
      IF ((EQCND90(CurCalYr,RECCL,B,R)/EH(CurCalYr,B,R)).GE.max_dw_pen) THEN
        EQCND90(CurCalYr,RECCL,B,R)=(EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))- &
         EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear)))
      ELSE
        EQCND90(CurCalYr,RECCL,B,R)=(EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))*(1.+DWPR(B,R))- &
         EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear)))
      ENDIF

      !******************************************************************
      !Calculate replacement equipment from original base-year stock
      !******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL)*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
        EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
        EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF

      !COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
           -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+&
           EQCND90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
        ENDDO
      ENDIF

      EQCRP90RP(CurCalYr,RECCL,B,R)=EQCRP90RP(CurCalYr,RECCL,B,R) + &
       EQCND90(CurCalYr,RECCL,B,R)

      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
           EQCND90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
          HSR=HDR(B)**(TEMP)
          SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
          EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
           (((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR))))
        ENDDO
      ENDIF

      !*******************************************************************
      !CALCULATE REPLACEMENT DISHWASHERS FOR NEW VINTAGE IN CurCalYr-1
      ! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
      ! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
      !*******************************************************************
      EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)

    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DO Y=CurCalYr,EndYr
        !VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
        TEMP=Y-CurCalYr
        HSR=HDR(B)**(TEMP)
        ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
        EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!AGGREGATE DISHWASHERS FOR INVESTMENT ANALYSIS
!********************************************************************
Y=CurCalYr
NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

DO B=1,mNumBldg
  DO r=1,mNumCR-2
  TYPE=RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
          HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
           REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 12
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,77)'12,',EU, ',',EQC, ',','X,',B,',',R,',',CurCalYr,',',RECTY,',',TYPE,',',RECCL,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',',EQCADD(Y,RECCL,B,r),',',HEATINGTYPEPURCH(Y,TYPE,B,R,1),',',HEATINGTYPEPURCH(Y,TYPE,B,R,2),',',EQCREP(Y,RECCL,B,r),',',EQCRP90RP(Y,RECCL,B,r),',',EQCRP90(Y,RECCL,B,r),',','X,','X,','X,','X,','X'
              77 FORMAT(a,I,a,I,a,a,  I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF   
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE RDWADD


!==============================================================================
! DISHWASHER CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE RDWCON
IMPLICIT NONE

REAL*4 ALPHA,ef1,ef2,ef3,TEMP
INTEGER B, E, D,EU,EUPR,RECCL,EQC,F,Y,R

EU = 4  !dishwashing
EUPR = 9
alpha = 0.0; ef1 = .5; ef2 = .35; ef3 = .15

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!*******************************************************************
!COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
IF (CurCalYr.GE.RECSYear+1) THEN
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
  ENDDO
ENDIF

!********************************************************************
!CALCULATE NEW, REPLACEMENT, AND AVERAGE UECS
!********************************************************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )
      EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (WTEQCEFFN(CurCalYr,RECCL,B,D)* RTBASEFF(RECSYear,RECCL))
      EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (WTEQCEFFR(CurCalYr,RECCL,B,D)* RTBASEFF(RECSYear,RECCL))
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
          EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
          EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP.LE.0.0) THEN
          EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
        ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+( &
             (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
DO B=1,mNumBldg
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP .GT. 0.0) THEN
          WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
             (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
              EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
          ENDDO
        ELSE
          WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFN(CurCalYr,RECCL,B,D)
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE DISHWASHING CONSUMPTION
!*******************************************************************

!Initialize arrays
DSWCON(CurIYr,1:mNumCR-2)=0.0
DSWCONIN(CurIYr,1:mNumCR-2,1:mNumBldg)=0.
Driver2(CurIYr,1:mNumCR-2,1:mNumBldg)=0.
DSWCONWT(CurIYr,1:mNumCR-2,1:mNumBldg)=0.

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      F  =RTFUEL(RECCL)

      IF (CurCalYr.EQ.RECSYear+1) THEN
        DSWCON(CurIYr,D)=DSWCON(CurIYr,D)+LEAPYR* ( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
         +(EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
         +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
         *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

        DSWCONWT(CurIYr,D,B)=DSWCONWT(CurIYr,D,B)+LEAPYR*( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
         +(EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
         +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
         *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

      IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
       EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
        DSWCONIN(CurIYr,D,B)=DSWCONIN(CurIYr,D,B)+(  (&
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
         +(EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
         +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))) )

        Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+ &
         (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D))
      ENDIF

      EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*( &
       ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
       (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
       +(EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
       +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
       *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

      ELSE
        DSWCON(CurIYr,D)=DSWCON(CurIYr,D)+ LEAPYR*( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
         (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

        DSWCONWT(CurIYr,D,B)=DSWCONWT(CurIYr,D,B)+LEAPYR*( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
         (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
         EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
         EQCSUR(CurCalYr,RECCL,b,D).GT.0.) THEN
          DSWCONIN(CurIYr,D,B)=DSWCONIN(CurIYr,D,B)+(  (&
           ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
           (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
           (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
           (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))))   )

          Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+ &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
           EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
           EQCSUR(CurCalYr,RECCL,b,D))
        ENDIF

        EQCEQCN(CurIYr,RECCL,B,D)=LEAPYR* ( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
         +(EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))) &
         *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  )

      ENDIF  !CurCalYr
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !D

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    IF (Driver2(CurIYr,R,B).GT.0) &
     DSWCONIN(CurIYr,R,B)=DSWCONIN(CurIYr,R,B)/Driver2(CurIYr,R,B)
  ENDDO
ENDDO

END SUBROUTINE RDWCON


!==============================================================================
! WATER HEATING CHOICE SUBROUTINE
!  CALLED FOR CurIYr
!==============================================================================
SUBROUTINE RWHTEC
IMPLICIT NONE

REAL*4 TOTN(mNumBldg,mNumCR)
REAL*4 EQFSHRN(nWatHtTypes,mNumBldg,mNumCR),EQFSHRR(nWatHtTypes,mNumBldg,mNumCR)
REAL*4 EQWTN(nWatHtTypes,mNumBldg,mNumCR),EQWTR(nWatHtTypes,mNumCR,mNumCR)
REAL*4 Temp
REAL*4 OPCOST(2)
REAL*4 TOTEWTN(nWatHtClasses,mNumBldg,mNumCR),TOTEWTR(nWatHtClasses,mNumBldg,mNumCR)
REAL*4 RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2
REAL*4 EQCOST,CAPITAL,RETAIL
INTEGER EU,EUPR,EUHT,RECTY,RECCL,RECCLHT,R,B,F,EQT,EQC,EQCHT,TYPE,COUNT,L
INTEGER RECAR(nWatHtTypes),EQTAR(nWatHtTypes)

!********************************************************************
! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES	!TODO - combine/optimize similar code
!********************************************************************

EU = 5  !water heating
EUPR = 3
EUHT = 1
ALPHA1 = -0.50

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!COMPUTE DECAY RATE USED TO COMPUTE LIFE-CYCLE COST (FIRST ITERATION ONLY)
IF (CURITR.EQ.1) THEN
  DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
ENDIF

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    !INITIALIZE ARRAYS
    TOTN(B,R)=0.0
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      TOTEWTN(EQC,B,R)=0.0
      TOTEWTR(EQC,B,R)=0.0
      NH2OSH(CurCalYr,EQC,B,R)=0.0
    ENDDO

    !SUM THE HEATER SHARES OVER ALL HEATER CLASSES
    DO RECCL=RTCLEUPT(EUHT)+1,RTCLEUPT(EUHT+1)
      EQCHT=RTCLEQCL(RECCL)
      TOTN(B,R)=TOTN(B,R)+HSYSSHR(CurCalYr,EQCHT,B,R)
    ENDDO
  ENDDO
ENDDO

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    !CALCULATE WATER HEATER SHARES BY FUEL FOR NEW DISTRIBUTION
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DO RECCLHT=RTCLEUPT(EUHT)+1,RTCLEUPT(EUHT+1)
        !ASSUME SAME SHARES FOR WATER HEATERS AS SPACE HEATERS WHOSE RTCLPNTR POINTER
        ! POINTS FROM THE SPACE HEATER CLASS TO THE WATER HEATER CLASS
        IF (RTCLPNTR(RECCLHT).EQ.EQC) THEN
          EQCHT=RTCLEQCL(RECCLHT)
          IF (TOTN(B,R).GT.0.0) THEN
            NH2OSH(CurCalYr,EQC,B,R)=NH2OSH(CurCalYr,EQC,B,R)+(HSYSSHR(CurCalYr,EQCHT,B,R)/TOTN(B,R))
          ELSE
            NH2OSH(CurCalYr,EQC,B,R)=0.0
          ENDIF
        ENDIF
      ENDDO
    ENDDO

    !VARIABLES USED THIS SECTION:
    ! RSMEQP and RSCLASS Variables
    ! RECTY          = RECORD NUMBER IN RSMEQP FILE
    ! RTTYEUPT(EU)   = LAST RECORD IN DISHWASHING (EU=4)
    ! RTTYEUPT(EU+1) = LAST RECORD IN WATER HEATING (EU=5)

    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT
          ! TYPE (EQT), REC # FOR RECCL FILE (RECCL), AND FUEL TYPE (F)
          EQC = RTTYEQCL(RECTY)
          EQT = RTEQTYPE(RECTY)
          RECCL = RTCLEUPT(EU)+EQC
          F = RTFUEL(RECCL)

          !COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
          IF (RTEQEFF(RECTY).NE.0.0) THEN
            RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
          ELSE
            RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
          ENDIF

          !SET CAPITAL COSTS
          ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
          ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
          IF (COSTTRSW.EQ.1) THEN
            CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
          ELSE
            CAPITAL = RTEQCOST(RECTY)
          ENDIF

          !CALCULATE OPERATING COST
          OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B) * RTEFFAC(1)
          OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B) * RTEFFAC(2)

          !CALCULATE LIFE CYCLE COSTS
          LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) * DECAY)
          LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) * DECAY)

          !CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
          IF ((CurCalYr.GT.2008).AND.(PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
            HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
            ELIGBLE=HRDRATE - 0.07
            IF ((ELIGBLE.GT.0.0).AND.(PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
              HRDADJ= ELIGBLE * ((PRICES(F,R,CurCalYr)/PRICES(F,R,RECSYear))**ALPHA1 )
              BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
            ELSE
              BETA1DR(RECTY)=RTECBTA1(RECTY)
            ENDIF
          ELSE
            BETA1DR(RECTY)=RTECBTA1(RECTY)
          ENDIF

          !COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
          EQWTN(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
           CAPITAL)+(RTECBTA2(RECTY)*OPCOST(2)) + &
           (RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
          TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
          EQWTR(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
           CAPITAL)+(RTECBTA2(RECTY)*OPCOST(1)) + &
           (RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
          TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 13
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,78)'13,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,',EQWTN(EQT,B,R),',',EQWTR(EQT,B,R),',','X,',BETA1DR(RECTY),',',CAPITAL,',',RTECBIAS(RECTY),',',OPCOST(1),',',OPCOST(2),',','X,',RTECBTA2(RECTY),',',RTECBTA3(RECTY),',',LFCY(EQT,B,R,1),',',LFCY(EQT,B,R,2),',',TOTEWTR(EQC,B,R),',',TOTEWTN(EQC,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              78 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  f,a,f,a,a,  f,a,f,a,f,a,f,a,f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)   
          ENDIF

        ENDIF !filter census division
      ENDIF !filter year availability
    ENDDO !for all RSMEQP records this end use

    !*******************************************************************
    !CALCULATE NEW AND REPLACEMENT MARKET SHARES
    !*******************************************************************
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT TYPE (EQT)
          EQC=RTTYEQCL(RECTY)
          EQT=RTEQTYPE(RECTY)
    
          !SET EQUIPMENT FUEL SHARE (AND NEQTSHR FOR WATER HEATING)
          IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
            EQFSHRN(EQT,B,R)=EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R)
          ELSE
            EQFSHRN(EQT,B,R)=0.0
          ENDIF
    
          IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
            EQFSHRR(EQT,B,R)=EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R)
          ELSE
            EQFSHRR(EQT,B,R)=0.0
          ENDIF
    
          NEQTSHR(CurCalYr,TYPE,B,R)=EQFSHRN(EQT,B,R)
          REQTSHR(CurCalYr,TYPE,B,R)=EQFSHRR(EQT,B,R)
    
          !Diagnostics only:	!TODO - still needed?
          !IF ((CurCalYr.GE.2015) .AND. (B.EQ.1)) THEN	!TODO - revise if updated !WHStandard
            !WRITE(9,'("Water heater new and replacement shares, before revision for 2015 water heater standard",4i5,2e15.4)') CurCalYr, EQC, EQT, TYPE, EQFSHRN(EQT,B,R),EQFSHRR(EQT,B,R)
          !ENDIF
    
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 14
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,79)'14,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              79 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    !Revise weights for 2015 water heater standard  !WHStandard
    IF (CurCalYr.GE.2015) THEN	!TODO - check for updated standard
      !Adjustments for Natural Gas Water Heating (EQC=1); xlRTCLEQCL in RSCLASS
      !The standard requires a condensing natural gas water heater for capacities greater than 56 gallons, which comprise
      ! approximately 4% of the existing market; if purchased share is less than 4%, revise shares.	!TODO - identify source of 4% share (TSD)?
      EQC=1

      IF (EQFSHRN(3,B,R) .LT. .04) THEN  !Values 1-3 represent the current NG_WH xlRTEQTYPE values in RSMEQP, with 3 being the highest efficiency available  !WHStandard !techupdate !EqpParam	!TODO - update hard-coded EQT value
        Temp=(1+(EQFSHRN(3,B,R)-.04)/(1.-EQFSHRN(3,B,R))) !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRN(1,B,R)=EQFSHRN(1,B,R)*temp
        EQFSHRN(2,B,R)=EQFSHRN(2,B,R)*temp
        EQFSHRN(3,B,R)=.04 !techupdate - update hard-coded EQT value
      ENDIF

      IF (EQFSHRR(3,B,R) .LT. .04) THEN  !Values 1-3 represent the current NG_WH xlRTEQTYPE values in RSMEQP, with 3 being the highest efficiency available  !WHStandard !techupdate !EqpParam	!TODO - update hard-coded EQT value
        Temp=(1+(EQFSHRR(3,B,R)-.04)/(1.-EQFSHRR(3,B,R))) !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRR(1,B,R)=EQFSHRR(1,B,R)*temp
        EQFSHRR(2,B,R)=EQFSHRR(2,B,R)*temp
        EQFSHRR(3,B,R)=.04 !techupdate !EqpParam	!TODO - update hard-coded EQT value
      ENDIF

      !Adjustments for Electric Water Heating (EQC=2); xlRTCLEQCL in RSCLASS	!TODO - revise if updated standard
      !The standard requires a heat pump water heater for capacities greater than 56 gallons which comprise
      ! approximately 9% of the existing market; if purchased share is less than 9%, revise shares.	!TODO - source of 9% share? TSD?
      EQC=2

      IF (EQFSHRN(6,B,R)+EQFSHRN(7,B,R)+EQFSHRN(8,B,R) .LT. .09) THEN  !Values 4-8 represent the current ELEC_WH and HP_WH xlRTEQTYPE values in RSMEQP, with 6-8 being the heat pump water heaters  !WHStandard !techupdate !EqpParam	!TODO - update hard-coded EQT value
        Temp=(1+(EQFSHRN(6,B,R)+EQFSHRN(7,B,R)+EQFSHRN(8,B,R)-.09)/(1.-EQFSHRN(6,B,R)-EQFSHRN(7,B,R)-EQFSHRN(8,B,R))) !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRN(4,B,R)=EQFSHRN(4,B,R)*Temp !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRN(5,B,R)=EQFSHRN(5,B,R)*Temp !techupdate !EqpParam	!TODO - update hard-coded EQT value
        Temp=.09/(EQFSHRN(6,B,R)+EQFSHRN(7,B,R)+EQFSHRN(8,B,R)) !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRN(6,B,R)=EQFSHRN(6,B,R)*Temp !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRN(7,B,R)=EQFSHRN(7,B,R)*Temp !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRN(8,B,R)=EQFSHRN(8,B,R)*Temp !techupdate !EqpParam	!TODO - update hard-coded EQT value
      ENDIF

      IF (EQFSHRR(6,B,R)+EQFSHRR(7,B,R)+EQFSHRR(8,B,R) .LT. .09) THEN  !Values 4-8 represent the current ELEC_WH and HP_WH xlRTEQTYPE values in RSMEQP, with 6-8 being the heat pump water heaters !techupdate !EqpParam	!TODO - update hard-coded EQT value
        Temp=(1+(EQFSHRR(6,B,R)+EQFSHRR(7,B,R)+EQFSHRR(8,B,R)-.09)/(1.-EQFSHRR(6,B,R)-EQFSHRR(7,B,R)-EQFSHRR(8,B,R))) !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRR(4,B,R)=EQFSHRR(4,B,R)*Temp
        EQFSHRR(5,B,R)=EQFSHRR(5,B,R)*Temp !techupdate !EqpParam	!TODO - update hard-coded EQT value
        Temp=.09/(EQFSHRR(6,B,R)+EQFSHRR(7,B,R)+EQFSHRR(8,B,R)) !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRR(6,B,R)=EQFSHRR(6,B,R)*Temp !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRR(7,B,R)=EQFSHRR(7,B,R)*Temp !techupdate !EqpParam	!TODO - update hard-coded EQT value
        EQFSHRR(8,B,R)=EQFSHRR(8,B,R)*Temp !techupdate !EqpParam	!TODO - update hard-coded EQT value
      ENDIF

      ! Reset Shares
      TYPE = RTTYPECT(EU)
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            TYPE=TYPE+1
            EQC=RTTYEQCL(RECTY)
            EQT=RTEQTYPE(RECTY)
            NEQTSHR(CurCalYr,TYPE,B,R)=EQFSHRN(EQT,B,R)
            REQTSHR(CurCalYr,TYPE,B,R)=EQFSHRR(EQT,B,R)

            !Diagnostics only:	!TODO - still needed?
            !IF ((CurCalYr.GE.2015) .AND. (B.EQ.1) .AND. (R.EQ.1)) THEN	!TODO - revise if updated standard !WHStandard
            !  WRITE(9,'("Water heater new and replacement shares, revised for 2015 water heater standard",4i5,2e15.4)') CurCalYr, EQC, EQT, TYPE, EQFSHRN(EQT,B,R),EQFSHRR(EQT,B,R)
            !ENDIF

            IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 15
              OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
                WRITE(661,83)'15,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
                83 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
              CLOSE(661)
            ENDIF
          ENDIF
        ENDIF
      ENDDO

    ENDIF !Revised WH for 2015 standard  !WHStandard

    !********************************************************************
    !CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT SPACE HEATING EQUIPMENT
    !********************************************************************
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC   =RTCLEQCL(RECCL)
      DENOM =0
      DENOM2=0
      COUNT =0
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (RTTYEQCL(RECTY).EQ.EQC.AND.CurCalYr.GE.RTINITYR(RECTY) &
         .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            COUNT=COUNT+1
            EQT=RTEQTYPE(RECTY)
            RECAR(COUNT)=RECTY
            EQTAR(COUNT)=EQT
            DENOM =DENOM +EQFSHRN(EQT,B,R)
            DENOM2=DENOM2+EQFSHRR(EQT,B,R)
          ENDIF
        ENDIF
      ENDDO

      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          EQT=EQTAR(L)
          SUM=SUM+(EQFSHRN(EQT,B,R)/RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
        IF (WTEQCEFFN(CurCalYr,RECCL,B,R).EQ.0.0) THEN
          WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
        ENDIF
      ENDIF

      !COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
      IF (DENOM2.LE.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          EQT=EQTAR(L)
          SUM=SUM+(EQFSHRR(EQT,B,R)/RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
      ENDIF
      IF (WTEQCEFFR(CurCalYr,RECCL,B,R).EQ.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ENDIF

    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

END SUBROUTINE RWHTEC


!==============================================================================
! WATER HEATING AND COOKING EQUIPMENT ADDED SUBROUTINE
!==============================================================================
SUBROUTINE REUADD
IMPLICIT NONE

REAL*4 SWT(RECSYear:EndYr),SWF(RECSYear:EndYr)
REAL*4 SA, HSR, ESR, SVRTE, SHARE  ,SWFT
REAL*4 EQSRT(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR-2),EQSR90T(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR-2)
INTEGER EQC,RECCL,TEMP,RECCLSW
INTEGER Y,Y1,R,D, B,TYPE,RECTY,NUMEQT,EQT,V

!EU # SET WHEN CALLING REUADD

!*******************************************************************
!CALCULATE EQUIPMENT ADDED IN CurCalYr
!*******************************************************************
!CUMULATE SURVIVING NEW EQUIPMENT ADDED PRIOR TO CurCalYr TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW EQUIPMENT ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) -  SURV.EQUIP(EQCSUR)
!*******************************************************************

!Initialize arrays
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCSR90(CurCalYr,RECCL,B,R)=0.0
      EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
      EQCSUR(CurCalYr,RECCL,B,R)=0.0
      IF (B.EQ.1) EQCREP(CurCalYr,RECCL,B,R) = 0.0
      SHARE = 1.0
    ENDDO
  ENDDO
ENDDO

!INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE AND THEN COUNT VALID TYPES IN CURRENT END USE
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (EU.EQ.5) SHARE = NH2OSH(CurCalYr,EQC,B,R)  !water heating share
      IF (EU.EQ.6) SHARE = NCKSH(CurCalYr,EQC,B,R)  !cooking share
      EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*SHARE)
      SA = 0.0
	  
      !******************************************************************
      !Calculate replacement equipment from original base year stock
      !******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) * &
         EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
        EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL) - &
         EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF
	  
      !COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
           -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
        ENDDO
      ENDIF
	  
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
          HSR=HDR(B)**(TEMP)
          SA=(SA+EQCADD(Y,RECCL,B,R)*HSR)
          EQCSUR(CurCalYr,RECCL,B,R)=(EQCSUR(CurCalYr,RECCL,B,R) + &
           (((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR))))
        ENDDO
      ENDIF
	  
      !*******************************************************************
      !CALCULATE REPLACEMENT WATER HEATERS FOR NEW VINTAGE IN CurCalYr-1
      ! NOTE: REPLACES WITH LIKE IN MULTIFAMILY AND MOBILE HOMES
      ! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
      !*******************************************************************

      !SUBROUTINE 'REPLACE' DISTRIBUTES REPLACEMENTS IN POST-RECS-YEAR SINGLE-FAMILY HOMES WHEN LAST ARGUMENT = 1
      IF (B.EQ.1) THEN
        !First, store what replacements would have been if no switching allowed
        OEQCREP(CurCalYr,RECCL,1,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
        !Call REPLACE to distribute replacements
        CALL REPLACE(EU,R,B,RECCL,1)
      ELSE
        ! No switching allowed in multifamily or mobile homes.
        EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
      ENDIF
	  
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

!SUBROUTINE 'REPLACE' DISTRIBUTES REPLACEMENTS IN EXISTING SINGLE-FAMILY HOMES WHEN LAST ARGUMENT = 2
B = 1
DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    OEQCRP90(CurCalYr,RECCL,B,R) = EQCRP90(CurCalYr,RECCL,1,R)
    OEQCRP90R(CurCalYr,RECCL,B,R) = EQCRP90RP(CurCalYr,RECCL,1,R)
    CALL REPLACE(EU,R,B,RECCL,2)
  ENDDO
ENDDO

DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    SWITCHTO(CurCalYr,RECCL,B,R)=0.0
    SWITCHTOR(CurCalYr,RECCL,B,R)=0.0
    DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      IF (RECCLSW.NE.RECCL) THEN
        SWITCHTO(CurCalYr,RECCL,B,R)=SWITCHTO(CurCalYr,RECCL,B,R) + EQCSW90(CurCalYr,RECCLSW,RECCL,B,R)
        SWITCHTOR(CurCalYr,RECCL,B,R)=SWITCHTOR(CurCalYr,RECCL,B,R) + EQCSW90R(CurCalYr,RECCLSW,RECCL,B,R)
      ENDIF
    ENDDO
  ENDDO
ENDDO

DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQCRP90(CurCalYr,RECCL,B,R)= EQCRP90(CurCalYr,RECCL,B,R) - SWITCHES(CurCalYr,RECCL,B,R)
    EQCRP90RP(CurCalYr,RECCL,B,R)= EQCRP90RP(CurCalYr,RECCL,B,R) - &
     SWITCHESR(CurCalYr,RECCL,B,R) + SWITCHTOR(CurCalYr,RECCL,B,R) + SWITCHTO(CurCalYr,RECCL,B,R)
  ENDDO
ENDDO

SWF(CurCalYr)=0.0
SWT(CurCalYr)=0.0

DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    SWT(CurCalYr)=SWT(CurCalYr)+SWITCHTO(CurCalYr,RECCL,B,R) + SWITCHTOR(CurCalYr,RECCL,B,R)
    SWF(CurCalYr)=SWF(CurCalYr)+SWITCHES(CurCalYr,RECCL,B,R) + SWITCHESR(CurCalYr,RECCL,B,R)
    !TODO - space heating calculates SWTOTAL and SWFTOTAL here; should that be mimicked for water heating (and possibly replace SWFT below)?
  ENDDO
ENDDO

IF (CurCalYr.EQ.EndYr) THEN
  SWFT=0.0
  DO Y=RECSYear+1,EndYr
    SWFT=SWFT+SWF(Y)
  ENDDO
ENDIF

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DO Y=CurCalYr,EndYr       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
        TEMP=Y-CurCalYr
        HSR=HDR(B)**(TEMP)
        ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
        EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!AGGREGATE WATER HEATING SYSTEMS FOR INVESTMENT ANALYSIS
!********************************************************************
Y=CurCalYr
NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    TYPE=RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
    ! CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY) .AND. CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1  !INDEX to count the 'TYPE' records in RSMEQP
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
          HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + &
           EQCRP90RP(Y,RECCL,B,r)) + REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 16
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,84)'16,',EU, ',',EQC, ',','X,',B,',',R,',',CurCalYr,',',RECTY,',',TYPE,',',RECCL,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',',EQCADD(Y,RECCL,B,r),',',HEATINGTYPEPURCH(Y,TYPE,B,R,1),',',HEATINGTYPEPURCH(Y,TYPE,B,R,2),',',EQCREP(Y,RECCL,B,r),',',EQCRP90RP(Y,RECCL,B,r),',',EQCRP90(Y,RECCL,B,r),',','X,','X,','X,','X,','X'
              84 FORMAT(a,I,a,I,a,a,  I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

IF (EU.EQ.5) THEN
  IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
    DO R=1,mNumCR-2
     WATERTOT(RECSYear,R)=0.0
      DO B=1,mNumBldg
       WATERTOT(RECSYear,R)=WATERTOT(RECSYear,R)+EQCESE(RECSYear,18,B,R) !NG_WH  !EqpParam  !NoKero	!TODO - replace 18 with parameter read from RSCLASS
      ENDDO
    ENDDO
  ELSE
    DO R=1,mNumCR-2
     WATERTOT(CurCalYr,R)=0.0
      DO B=1,mNumBldg
       WATERTOT(CurCalYr,R)=WATERTOT(CurCalYr,R)+EQCESE(CurCalYr,18,B,R)+EQCADD(CurCalYr,18,B,R)+& !NG_WH  !EqpParam  !NoKero	!TODO - replace 18 with parameter read from RSCLASS
         EQCRP90(CurCalYr,18,B,R)+EQCRP90RP(CurCalYr,18,B,R)+EQCSUR(CurCalYr,18,B,R)+EQCREP(CurCalYr,18,B,R)+& !NG_WH  !EqpParam  !NoKero	!TODO - replace 18 with parameter read from RSCLASS
         EQCSR90(CurCalYr,18,B,R) !NG_WH  !EqpParam  !NoKero	!TODO - replace 18 with parameter read from RSCLASS
      ENDDO
    ENDDO
  ENDIF
ELSE
  IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
    DO R=1,mNumCR-2
      COOKTOT(RECSYear,R)=0.0
      DO B=1,mNumBldg
        COOKTOT(RECSYear,R)=COOKTOT(RECSYear,R)+EQCESE(RECSYear,23,B,R) !NG_STV !EqpParam  !NoKero	!TODO - replace 23 with parameter read from RSCLASS
      ENDDO
    ENDDO
  ELSE
    DO R=1,mNumCR-2
     COOKTOT(CurCalYr,R)=0.0
      DO B=1,mNumBldg
       COOKTOT(CurCalYr,R)=COOKTOT(CurCalYr,R)+EQCESE(CurCalYr,23,B,R)+EQCADD(CurCalYr,23,B,R)+& !NG_STV !EqpParam  !NoKero	!TODO - replace 23 with parameter read from RSCLASS
         EQCRP90(CurCalYr,23,B,R)+EQCRP90RP(CurCalYr,23,B,R)+EQCSUR(CurCalYr,23,B,R)+EQCREP(CurCalYr,23,B,R)+& !NG_STV !EqpParam  !NoKero	!TODO - replace 23 with parameter read from RSCLASS
         EQCSR90(CurCalYr,23,B,R) !NG_STV !EqpParam  !NoKero	!TODO - replace 23 with parameter read from RSCLASS
      ENDDO
    ENDDO
  ENDIF
ENDIF

!Natural gas space heating equipment defined in RSCLASS (e.g., furnaces, boilers, heat pumps) are proxy for number of natural gas customers	!TODO - Remove ",heat pumps" if replacing NG_HP inputs with MS_HP
! RSGASCUST tracks the number of natural gas customers by looking across end uses (i.e., if number of natural gas water heaters, cooking ranges,
!  or clothes dryers exceeds number of space heaters, that value becomes the number of natural gas customers for that year/census division/building type
! Note: this is not a constraint on hookups...	!TODO - could optimize this section of code using MAX function?
DO R=1,mNumCR-2
  IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
    IF (EU.EQ.5) THEN
      IF (WATERTOT(RECSYear,R).GT.RSGASCUST(RECSYear,R)) THEN
        RSGASCUST(RECSYear,R)=WATERTOT(RECSYear,R)
      ENDIF
    ELSE
      IF (COOKTOT(RECSYear,R).GT.RSGASCUST(RECSYear,R)) THEN
        RSGASCUST(RECSYear,R)=COOKTOT(RECSYear,R)
      ENDIF
    ENDIF
  ELSE
    IF (EU.EQ.5) THEN
      IF (WATERTOT(CurCalYr,R).GT.RSGASCUST(CurCalYr,R)) THEN
        RSGASCUST(CurCalYr,R)=WATERTOT(CurCalYr,R)
      ENDIF
    ELSE
      IF (COOKTOT(CurCalYr,R).GT.RSGASCUST(CurCalYr,R)) THEN
        RSGASCUST(CurCalYr,R)=COOKTOT(CurCalYr,R)
      ENDIF
    ENDIF
  ENDIF
ENDDO

END SUBROUTINE REUADD


!==============================================================================
! WATER HEATING CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE RWHCON
IMPLICIT NONE

REAL*4 HOUSES(RECSYear:EndYr,mNumCR)
REAL*4 ALPHA,ef1,ef2,ef3,TEMP,HHSELAS,HHSIZE(RECSYear:EndYr,mNumCR-2)
REAL*4 CHECK(RECSYear:EndYr,4),DCHECK(RECSYear:EndYr,4)	!TODO - variables not used?
INTEGER B, D, EU, EUPR, EQC, RECCL, V, F, FCON,Y,FD,R
INTEGER RECCLSWH, RECCLEWH,EQCSWH, EQCEWH,EQCCW

!PRICES 1=Distillate Fuel Oil 2=Propane 3=Natural Gas 4=Electricity
!********************************************************************
! F    = FUEL NUMBER FROM RSCLASS FILE
! FCON = FUEL NUMBER FOR CONSUMPTION (AS FOLLOWS):
!        1=Natural Gas 2=Electricity 3=Distillate Fuel Oil + Kerosene 4=Propane 5=Solar
!********************************************************************

EU = 5  !water heating
EUPR = 3
ALPHA = -.15; ef1 = .5; ef2 = .35; ef3 = .15	!TODO - revise?
HHSELAS=.315  !People per house elasticity for hot water use (lbl)	!TODO - Revise?

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!********************************************************************
!COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!********************************************************************
IF (CurCalYr.GE.RECSYear+1) THEN
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
  ENDDO
ENDIF

!********************************************************************
!CALCULATE AVERAGE OCCUPANCY PER HOUSING UNIT
!********************************************************************

!Initialize variables
HOUSES(CurCalYr,1:mNumCR-2) = 0.0

!Calculate housing stock and people per housing unit in RECSYear
IF (CurCalYr.EQ.RECSYear+1) THEN
  DO D=1,mNumCR-2
    HOUSES(RECSYear,D)=0.0
    DO B=1,mNumBldg
      HOUSES(RECSYear,D)=HOUSES(RECSYear,D)+EH(RECSYear,B,D)
    ENDDO
    HHSIZE(RECSYear,D)=MC_NP16A(D,RECSYear-BaseYr+1) / HOUSES(RECSYear,D)
  ENDDO
ENDIF

!Calculate housing stock and people per housing unit in projection years
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    HOUSES(CurCalYr,D)=HOUSES(CurCalYr,D)+EH(CurCalYr,B,D)+NH(CurCalYr,B,D)
  ENDDO
  HHSIZE(CurCalYr,D)=MC_NP16A(D,CurIYr) / HOUSES(CurCalYr,D)
ENDDO

!********************************************************************
!CALCULATE NEW, REPLACEMENT, AND AVERAGE UECS
!********************************************************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
       ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS)* &
       (RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL))
      EQCSIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
       (RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL))

      IF (CurCalYr.EQ.RECSYear+1) THEN
        IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS) * &
           WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
          EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
         ELSE
           EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
            ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS)
           EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
         ENDIF
      ELSE
        IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))** HHSELAS)* &
           WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
          EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
        ELSE
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS)
          EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
        ENDIF
      ENDIF

      IF (WTEQCEFFR(CurCalYr,RECCL,B,D).GT.0.0) THEN
        EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
         ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS)* &
         WTEQCEFFR(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
        EQCRIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
         WTEQCEFFR(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
      ELSE
        EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
         ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS)
        EQCRIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
      ENDIF

      IF (CurCalYr .EQ. RECSYear+1) THEN
        EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP.LE.0.0) THEN
          EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
        ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear+1,CurCalYr-1
            EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+( &
             (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
          ENDDO
        ENDIF
      ENDIF

    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !D

!********************************************************************
!CALCULATE AVERAGE EFFICIENCY
!********************************************************************
DO B=1,mNumBldg
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO

        IF (TEMP .GT. 0.0) THEN
          WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear+1,CurCalYr-1
            WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
             (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
          ENDDO
        ELSE
          WTEQCEFFA(CurCalYr,RECCL,B,D)= &
            WTEQCEFFN(CurCalYr,RECCL,B,D)
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!Calculate Water Heating Shares of Dishwashers and Clothes Washers
!********************************************************************
DO D=1,mNumCR-2
   DO B=1,mNumBldg
      HOTWATQ(CurCalYr,B,D)=0.0
     DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC = RTCLEQCL(RECCL)
       HOTWATQ(CurCalYr,B,D)=HOTWATQ(CurCalYr,B,D)+ &
        EQCESE(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D) + &
        EQCRP90RP(CurCalYr,RECCL,B,D)+EQCREP(CurCalYr,RECCL,B,D) + &
        EQCSUR(CurCalYr,RECCL,B,D)+ EQCSR90(CurCalYr,RECCL,B,D)
   ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE WATER HEATING CONSUMPTION
!********************************************************************

!Initialize variables
H2OCONWT(CurIYr,1:5,1:mNumCR-2,1:mNumBldg) = 0.	!TODO - replace 5 with NWHFL? would need to be declared in different module/subroutine
H2OCONIN(CurIYr,1:5,1:mNumCR-2,1:mNumBldg) = 0.	!TODO - replace 5 with NWHFL? would need to be declared in different module/subroutine
Driver(CurIYr,1:5,1:mNumCR-2,1:mNumBldg) = 0.	!TODO - replace 5 with NWHFL? would need to be declared in different module/subroutine

DO D=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    H2OCON(CurIYr,EQC,D)=0.0
  ENDDO
ENDDO

!********************************************************************
!FIND INDICES FOR THE ELECTRIC AND SOLAR WATER HEATERS USED TO COMPUTE H2OCON FOR SOLAR FUEL (FCON=5)
!********************************************************************
DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  IF (RTCLNAME(RECCL).EQ.'ELEC_WH') THEN
    EQCEWH=RTCLEQCL(RECCL)
    RECCLEWH=EQCEWH+RTCLEUPT(EU)
  ELSEIF (RTCLNAME(RECCL).EQ.'SOLAR_WH') THEN
    EQCSWH=RTCLEQCL(RECCL)
    RECCLSWH=EQCSWH+RTCLEUPT(EU)
  ENDIF
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC  = RTCLEQCL(RECCL)
      F    = RTFUEL(RECCL)
      FCON = FWHCON(F)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        H2OCON(CurIYr,FCON,D)=H2OCON(CurIYr,FCON,D)+ LEAPYR* &
         (((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+ &
         (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
         ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+ &
         (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)* &
         NCWLOAD(CurCalYr,D,B)))*EQCADD(CurCalYr,RECCL,B,D) + &
         ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+ &
         (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)* &
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         H2OCONWT(CurIYr,FCON,D,B)=H2OCONWT(CurIYr,FCON,D,B)+LEAPYR* &
          (((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+ &
          (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
          ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D)+ &
          ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+ &
          (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)* &
          NCWLOAD(CurCalYr,D,B)))*EQCADD(CurCalYr,RECCL,B,D)+ &
          ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+ &
          (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)* &
          NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D))* &
          RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
          H2OCONIN(CurIYr,FCON,D,B)=H2OCONIN(CurIYr,FCON,D,B)+( &
           ((1.-CWLOAD(RECSYear))*EQCSIUEC(CurCalYr,RECCL,B,D)+&
           (EQCSIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
           ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
           ((1.-CWLOAD(RECSYear))*EQCNIUEC(CurCalYr,RECCL,B,D)+&
           (EQCNIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
           NCWLOAD(CurCalYr,D,B)))*EQCADD(CurCalYr,RECCL,B,D) + &
           ((1.-CWLOAD(RECSYear))*EQCRIUEC(CurCalYr,RECCL,B,D)+&
           (EQCRIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
           NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D))

          Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+ &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D))
        ENDIF

        EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*( &
         ((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+&
         (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
         ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+&
         (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCADD(CurCalYr,RECCL,B,D) + &
         ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+&
         (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

      ELSE
        IF (F.EQ.4) THEN
          ALPHA=-0.30  !was -0.15 prior to American Recovery and Reinvestment Act of 2009 (ARRA) stimulus; permanently affects price elasticity (but not rebound) based on the smart grid concept
        ELSE
          ALPHA=-0.15
        ENDIF

        H2OCON(CurIYr,FCON,D)=H2OCON(CurIYr,FCON,D)+ LEAPYR*( &
         ((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+&
         (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
         ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+&
         (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*(EQCADD(CurCalYr,RECCL,B,D)+EQCRP90RP(CurCalYr,RECCL,B,D)) + &
         ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+&
         (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D)+ &
         ((1.-CWLOAD(RECSYear))*EQCAUEC(CurCalYr,RECCL,B,D)+&
         (EQCAUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*(EQCREP(CurCalYr,RECCL,B,D)+EQCSUR(CurCalYr,RECCL,B,D)+EQCSR90(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

        H2OCONWT(CurIYr,FCON,D,B)=H2OCONWT(CurIYr,FCON,D,B)+LEAPYR*( &
         ((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+&
         (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
         ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+&
         (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*(EQCADD(CurCalYr,RECCL,B,D)+EQCRP90RP(CurCalYr,RECCL,B,D)) + &
         ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+&
         (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D)+ &
         ((1.-CWLOAD(RECSYear))*EQCAUEC(CurCalYr,RECCL,B,D)+&
         (EQCAUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*(EQCREP(CurCalYr,RECCL,B,D)+EQCSUR(CurCalYr,RECCL,B,D)+EQCSR90(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
         EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
         EQCSUR(CurCalYr,RECCL,b,D).GT.0.) THEN
          H2OCONIN(CurIYr,FCON,D,B)=H2OCONIN(CurIYr,FCON,D,B)+(  &
           ((1.-CWLOAD(RECSYear))*EQCSIUEC(CurCalYr,RECCL,B,D)+&
           (EQCSIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
           ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
           ((1.-CWLOAD(RECSYear))*EQCNIUEC(CurCalYr,RECCL,B,D)+&
           (EQCNIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
           NCWLOAD(CurCalYr,D,B)))*(EQCADD(CurCalYr,RECCL,B,D)+EQCRP90RP(CurCalYr,RECCL,B,D)) + &
           ((1.-CWLOAD(RECSYear))*EQCRIUEC(CurCalYr,RECCL,B,D)+&
           (EQCRIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
           NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D)+ &
           ((1.-CWLOAD(RECSYear))*EQCAUEC(CurCalYr,RECCL,B,D)+&
           (EQCAUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
           ECWLOAD(CurCalYr,D,B)))*(EQCREP(CurCalYr,RECCL,B,D)+EQCSUR(CurCalYr,RECCL,B,D)+EQCSR90(CurCalYr,RECCL,B,D)))

          Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
           EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
           EQCSUR(CurCalYr,RECCL,b,D))
        ENDIF

        EQCEQCN(CurIYr,RECCL,B,D) = LEAPYR* ( &
         ((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+&
         (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
         ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+&
         (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*(EQCADD(CurCalYr,RECCL,B,D)+EQCRP90RP(CurCalYr,RECCL,B,D)) + &
         ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+&
         (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D)+ &
         ((1.-CWLOAD(RECSYear))*EQCAUEC(CurCalYr,RECCL,B,D)+&
         (EQCAUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*(EQCREP(CurCalYr,RECCL,B,D)+EQCSUR(CurCalYr,RECCL,B,D)+EQCSR90(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
      ENDIF

      !SOLAR IS COMPUTED DIFFERENTLY
      FCON = FWHCON(5) !FWHCON(5)=5 !solar - SOLAR_WH  !NoKero	!TODO - FCON is not referenced in the following equations, but hard-coded 5 is

      H2OCON(CurIYr,5,D)=H2OCON(CurIYr,5,D)+LEAPYR* &
       (EQCESE(CurCalYr,RECCLSWH,B,D)* &
       ((EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412. )+ &
       EQCRP90(CurCalYr,RECCLSWH,B,D)* &
       ((EQCUEC(D,RECCLEWH,B)-EQCRUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &
       + EQCADD(CurCalYr,RECCLSWH,B,D)* &
       ((EQCUEC(D,RECCLEWH,B)-EQCHVUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) + &
       (EQCREP(CurCalYr,RECCLSWH,B,D)+EQCRP90RP(CurCalYr,RECCLSWH,B,D))* &
       ((EQCUEC(D,RECCLEWH,B)-EQCNUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &
       +(EQCSR90(CurCalYr,RECCLSWH,B,D)+ &
       EQCSUR(CurCalYr,RECCLSWH,B,D))* &
       ((EQCUEC(D,RECCLEWH,B)-EQCAUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.))

      SLEQCN(CurIYr,1,B,D)=LEAPYR*&
       (EQCESE(CurCalYr,RECCLSWH,B,D)* &
       ((EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412. )+ &
       EQCRP90(CurCalYr,RECCLSWH,B,D)* &
       ((EQCUEC(D,RECCLEWH,B)-EQCRUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &
       + EQCADD(CurCalYr,RECCLSWH,B,D)* &
       ((EQCUEC(D,RECCLEWH,B)-EQCHVUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) + &
       (EQCREP(CurCalYr,RECCLSWH,B,D)+EQCRP90RP(CurCalYr,RECCLSWH,B,D))* &
       ((EQCUEC(D,RECCLEWH,B)-EQCNUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &
       +(EQCSR90(CurCalYr,RECCLSWH,B,D)+ &
       EQCSUR(CurCalYr,RECCLSWH,B,D))* &
       ((EQCUEC(D,RECCLEWH,B)-EQCAUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.))

    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !D

!********************************************************************
!Calculate Solar Water Heating Consumption
!********************************************************************
DO D=1,mNumCR-2
  SLCON(CurIYr,D)=0.0
  DO B=1,mNumBldg
    SLCON(CurIYr,D)=SLCON(CurIYr,D)+LEAPYR*&
     (EQCESE(CurCalYr,RECCLSWH,B,D)* &
     ((EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412. )+ &
     EQCRP90(CurCalYr,RECCLSWH,B,D)* &
     ((EQCUEC(D,RECCLEWH,B)-EQCRUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &
     + EQCADD(CurCalYr,RECCLSWH,B,D)* &
     ((EQCUEC(D,RECCLEWH,B)-EQCHVUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) + &
     (EQCREP(CurCalYr,RECCLSWH,B,D)+EQCRP90RP(CurCalYr,RECCLSWH,B,D))* &
     ((EQCUEC(D,RECCLEWH,B)-EQCNUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &
     +(EQCSR90(CurCalYr,RECCLSWH,B,D)+ &
     EQCSUR(CurCalYr,RECCLSWH,B,D))* &
     ((EQCUEC(D,RECCLEWH,B)-EQCAUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.))
  ENDDO
ENDDO

DO R=1,mNumCR-2
  DO FCON=1,5  !Natural gas, electricity, distillate fuel oil, propane, solar	!TODO - replace 5 with NWHFL? would need to be declared in different module/subroutine
    DO B=1,mNumBldg
      IF (Driver(CurIYr,FCON,R,B).GT.0) &
       H2OCONIN(CurIYr,FCON,R,B)=H2OCONIN(CurIYr,FCON,R,B)/Driver(CurIYr,FCON,R,B)
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE RWHCON


!==============================================================================
! COOKING CHOICE SUBROUTINE
!==============================================================================
SUBROUTINE RSTVTEC
IMPLICIT NONE

REAL*4 NEQTSHRD(RECSYear:EndYr+1,MNUMRTTY,mNumBldg,mNumCR), &
       REQTSHRD(RECSYear:EndYr+1,MNUMRTTY,mNumBldg,mNumCR)
REAL*4 OPCOST(2)
REAL*4 NGNGFACT(mNumBldg)
REAL*4 RTEFFAC(2),DECAY,DENOM,SUM,DENOM2,SUM2
REAL*4 EQFSHRN(nCookTypes,mNumBldg,mNumCR),EQFSHRR(nCookTypes,mNumBldg,mNumCR)  !EqpParam
REAL*4 EQWTN(nCookTypes,mNumBldg,mNumCR),EQWTR(nCookTypes,mNumCR,mNumCR)  !EqpParam
REAL*4 TOTEWTN(nCookClasses,mNumBldg,mNumCR),TOTEWTR(nCookClasses,mNumBldg,mNumCR)  !EqpParam
REAL*4 EQCOST,CAPITAL,RETAIL
INTEGER EU,EUPR,EUHW,RECTY,RECCL,RECCLHW,R,B,F,EQT,EQC,EQCHW, &
        TYPE,COUNT,RECAR(nCookTypes),EQTAR(nCookTypes),L  !EqpParam

!********************************************************************
! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES	!TODO - combine/optimize similar code
!********************************************************************
EU=6  !cooking
EUHW=5  !links cooking choice to water heating fuel
EUPR=4
ALPHA1=-0.50

!Set share of homes with natural gas water heaters that also have natural gas cooking ranges (based on RECS)	!TODO - revise anytime RECS base year is updated
NGNGFACT(1)= 0.46
NGNGFACT(2)= 0.46
NGNGFACT(3)= 0.64

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
IF (CURITR.EQ.1) THEN
  DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
ENDIF

!Initialize arrays
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      NCKSH(CurCalYr,EQC,B,R)=0.0
      TOTEWTN(EQC,B,R)=0.0
      TOTEWTR(EQC,B,R)=0.0
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE NEW COOKING SHARE (NCKSH) FOR NEW DISTRIBUTION
!********************************************************************
!VARIABLES USED THIS SECTION:
! RECTY          = RECORD NUMBER IN RSMEQP FILE
! RTTYEUPT(EU)   = LAST RECORD IN WATER HEATING (EU=5)
! RTTYEUPT(EU+1) = LAST RECORD IN COOKING       (EU=6)

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DO RECCLHW=RTCLEUPT(EUHW)+1,RTCLEUPT(EUHW+1)
        IF (RTCLPNTR(RECCLHW).EQ.EQC.OR.RTCLREPL(RECCLHW).EQ.EQC) THEN
          EQCHW=RTCLEQCL(RECCLHW)
          IF (RTCLNAME(RECCLHW).EQ.'NG_WH'.AND.RTCLNAME(RECCL).EQ.'NG_STV') THEN
            NCKSH(CurCalYr,EQC,B,R)=NCKSH(CurCalYr,EQC,B,R)+NH2OSH(CurCalYr,EQCHW,B,R)*NGNGFACT(B)
          ELSEIF (RTCLNAME(RECCLHW).EQ.'NG_WH'.AND.RTCLNAME(RECCL).EQ.'ELEC_STV') THEN
            NCKSH(CurCalYr,EQC,B,R)=NCKSH(CurCalYr,EQC,B,R)+NH2OSH(CurCalYr,EQCHW,B,R)*(1-NGNGFACT(B))
          ELSE
            NCKSH(CurCalYr,EQC,B,R)=NCKSH(CurCalYr,EQC,B,R)+NH2OSH(CurCalYr,EQCHW,B,R)
          ENDIF
        ENDIF
      ENDDO
    ENDDO

    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          EQC = RTTYEQCL(RECTY)
          EQT = RTEQTYPE(RECTY)
          RECCL = RTCLEUPT(EU)+EQC
          F = RTFUEL(RECCL)

          !COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
          IF (RTEQEFF(RECTY).NE.0.0) THEN
            RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
          ELSE
            RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
          ENDIF

          !SET CAPITAL COSTS
          ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
          ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
          IF (COSTTRSW.EQ.1) THEN
            CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
          ELSE
            CAPITAL = RTEQCOST(RECTY)
          ENDIF

          !CALCULATE OPERATING COST
          OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)
          OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(2)

          !CHANGE BETA1 TO REFLECT PRICE-INDUCED BEHAVIOR CHANGES
          IF ((CurCalYr.GT.2008).AND. &	!TODO - 2008 marks last year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)? Remove legacy energy bill code as necessary
           (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
            HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
            ELIGBLE=HRDRATE - 0.07
            IF (ELIGBLE.GT.0.0) THEN
              HRDADJ= ELIGBLE * &
               ((PRICES(F,R,CurCalYr)/PRICES(F,R,RECSYear))**ALPHA1 )
              BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
            ELSE
              BETA1DR(RECTY)=RTECBTA1(RECTY)
            ENDIF
          ELSE
            BETA1DR(RECTY)=RTECBTA1(RECTY)
          ENDIF

          !CALCULATE LIFE-CYCLE COSTS
          LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
          LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)

          !********************************************************************
          !COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
          !********************************************************************
          EQWTN(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
           CAPITAL)+(RTECBTA2(RECTY)*OPCOST(2)) + &
           ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
          TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
          EQWTR(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
           CAPITAL)+(RTECBTA2(RECTY)*OPCOST(1)) + &
           ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
          TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 17
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,85)'17,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,',EQWTN(EQT,B,R),',',EQWTR(EQT,B,R),',','X,',BETA1DR(RECTY),',',CAPITAL,',',RTECBIAS(RECTY),',',OPCOST(1),',',OPCOST(2),',','X,',RTECBTA2(RECTY),',',RTECBTA3(RECTY),',',LFCY(EQT,B,R,1),',',LFCY(EQT,B,R,2),',',TOTEWTR(EQC,B,R),',',TOTEWTN(EQC,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              85 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  f,a,f,a,a,  f,a,f,a,f,a,f,a,f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF  

        ENDIF  !RTCENDIV
      ENDIF  !CurCalYr
    ENDDO  !RECTY

    !********************************************************************
    !CALCULATE NEW AND REPLACEMENT MARKET SHARES
    !********************************************************************
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT TYPE (EQT)
          EQC=RTTYEQCL(RECTY)
          EQT=RTEQTYPE(RECTY)

          !SET EQUIPMENT FUEL SHARE (AND NEQTSHR FOR WATER HEATING)
          IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
            EQFSHRN(EQT,B,R)=EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R)
          ELSE
            EQFSHRN(EQT,B,R)=0.0
          ENDIF

          IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
            EQFSHRR(EQT,B,R)=EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R)
          ELSE
            EQFSHRR(EQT,B,R)=0.0
          ENDIF

          NEQTSHR(CurCalYr,TYPE,B,R)=EQFSHRN(EQT,B,R)
          REQTSHR(CurCalYr,TYPE,B,R)=EQFSHRR(EQT,B,R)

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 18
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,86)'18,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              86 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF  
        ENDIF
      ENDIF
    ENDDO

    !********************************************************************
    !CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT EQUIPMENT
    !********************************************************************
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DENOM=0
      DENOM2=0
      COUNT =0
      TYPE = RTTYPECT(EU)
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (RTTYEQCL(RECTY).EQ.EQC.AND.CurCalYr.GE.RTINITYR(RECTY) &
         .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            COUNT=COUNT+1
            EQT=RTEQTYPE(RECTY)
            RECAR(COUNT)=RECTY
            EQTAR(COUNT)=EQT
            DENOM =DENOM +EQFSHRN(EQT,B,R)
            DENOM2=DENOM2+EQFSHRR(EQT,B,R)
          ENDIF
        ENDIF
      ENDDO

      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          EQT=EQTAR(L)
          SUM=SUM+(EQFSHRN(EQT,B,R)*RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
      ENDIF

      !COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
      IF (DENOM2.LE.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          EQT=EQTAR(L)
          SUM=SUM+(EQFSHRR(EQT,B,R)*RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
      ENDIF
    ENDDO

  ENDDO !B
ENDDO !R

END SUBROUTINE RSTVTEC


!==============================================================================
! COOKING ADDED SUBROUTINE HANDLED BY SUBROUTINE REUADD(EU)
!==============================================================================


!==============================================================================
! COOKING CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE RSTOVCON
IMPLICIT NONE

REAL*4 ALPHA,ef1,ef2,ef3,TEMP
INTEGER B, E, D, FCON, RECCL, EU, EUPR, V, F,TYPE,RECTY,EQT,NUMEQT,EQC,R,Y

EU = 6  !cooking
EUPR = 4
alpha = 0.0; ef1 = .5; ef2 = .35; ef3 = .15

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!********************************************************************
!COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!********************************************************************
IF (CurCalYr.GE.RECSYear+1) THEN
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
  ENDDO
ENDIF

!********************************************************************
!CALCULATE NEW, AND AVERAGE UECS
!********************************************************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )
      IF (CurCalYr.EQ.RECSYear+1) THEN
        IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           (RTBASEFF(RECSYear,RECCL)/WTEQCEFFN(CurCalYr,RECCL,B,D))
        ELSE
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
        ENDIF
      ELSE
        IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           (RTBASEFF(RECSYear,RECCL)/WTEQCEFFN(CurCalYr,RECCL,B,D))
        ELSE
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
        ENDIF
      ENDIF
      IF (WTEQCEFFR(CurCalYr,RECCL,B,D).GT.0.0) THEN
        EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
         (RTBASEFF(RECSYear,RECCL)/WTEQCEFFR(CurCalYr,RECCL,B,D))
      ELSE
        EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
      ENDIF
      IF (CurCalYr .EQ. RECSYear+1) THEN
        EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP.LE.0.0) THEN
          EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
        ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+( &
             (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE AVERAGE EQUIPMENT EFFICIENCY
!*******************************************************************
DO B=1,mNumBldg
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP .GT. 0.0) THEN
          WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
             (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
          ENDDO
        ELSE
          WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFN(CurCalYr,RECCL,B,D)
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE COOKING CONSUMPTION
!*******************************************************************

!Initialize arrays
CKCON(CurIYr,1:NSTVFL,1:mNumCR-2)=0.0
CKCONWT(CurIYr,1:NSTVFL,1:mNumCR-2,1:mNumBldg)=0.
Driver(CurIYr,1:NSTVFL,1:mNumCR-2,1:mNumBldg)=0.
CKCONIN(CurIYr,1:NSTVFL,1:mNumCR-2,1:mNumBldg)=0.

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      E=RTCLEQCL(RECCL)
      F=RTFUEL(RECCL)
      FCON=FSTVCON(F)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        CKCON(CurIYr,FCON,D)=CKCON(CurIYr,FCON,D)+LEAPYR* ( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

        CKCONWT(CurIYr,FCON,D,B)=CKCONWT(CurIYr,FCON,D,B)+ LEAPYR*   ( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
          CKCONIN(CurIYr,FCON,D,B)=CKCONIN(CurIYr,FCON,D,B)+(       (&
           ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
           (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))) )

          Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D) )
        ENDIF
        EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*&
        ( EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
          EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
          EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)) &
        * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
      ELSE
        CKCON(CurIYr,FCON,D)=CKCON(CurIYr,FCON,D)+LEAPYR* &
         (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)+ &
         EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
         EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
         EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)+ &
         EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
         EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
	  
        CKCONWT(CurIYr,FCON,D,B)= CKCONWT(CurIYr,FCON,D,B) + LEAPYR*&
         (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)+ &
         EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
         EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
         EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)+ &
         EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
         EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
         EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
         EQCSUR(CurCalYr,RECCL,b,D).GT.0.) THEN
          CKCONIN(CurIYr,FCON,D,B)= CKCONIN(CurIYr,FCON,D,B) + ( &
           (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)+ &
           EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
           EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
           EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)+ &
           EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
           EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))

          Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+ &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
           EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
           EQCSUR(CurCalYr,RECCL,b,D))
        ENDIF

        EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*&
         (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)+ &
         EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
         EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
         EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)+ &
         EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
         EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D) ) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
	  
      ENDIF  !CurCalYr
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !D

!CALCULATE INTENSITY VARIABLE FOR REPORT WRITER
DO R=1,mNumCR-2
  DO FCON=1,NSTVFL
    DO B=1,mNumBldg
      IF (Driver(CurIYr,FCON,R,B).GT.0) CKCONIN(CurIYr,FCON,R,B)=CKCONIN(CurIYr,FCON,R,B)/Driver(CurIYr,FCON,R,B)
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE RSTOVCON


!==============================================================================
! CLOTHES DRYING CHOICE SUBROUTINE
!==============================================================================
SUBROUTINE RDRYTEC
IMPLICIT NONE

REAL*4 EQWTN(nClDryTypes,mNumBldg,mNumCR),EQWTR(nClDryTypes,mNumBldg,mNumCR)  !EqpParam
REAL*4 TOTEWTN(nClDryClasses,mNumBldg,mNumCR),TOTEWTR(nClDryClasses,mNumBldg,mNumCR)  !EqpParam
REAL*4 OPCOST(2)
REAL*4 RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2,SUM2
REAL*4 EQCOST,CAPITAL,RETAIL
INTEGER EU,RECTY,RECCL,R,B,F,EQT,EQC,REQT,TYPE,COUNT,L
INTEGER RECAR(nClDryTypes),EQTAR(nClDryTypes)  !EqpParam

!********************************************************************
! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES	!TODO - combine/optimize similar code
!********************************************************************

EU = 7  !clothes drying
ALPHA1 = -0.50

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,5)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!COMPUTE DECAY RATE USED TO COMPUTE LIFE-CYCLE COST (FIRST ITERATION ONLY)
IF (CURITR.EQ.1) THEN
  DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
ENDIF

!INITIALIZE ARRAYS
DO B=1,mNumBldg
  DO R=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      TOTEWTN(EQC,B,R)=0.0
      TOTEWTR(EQC,B,R)=0.0
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE OPERATING COSTS, LIFE-CYCLE COSTS, EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!********************************************************************
!RTTYEUPT(EU)   = LAST RECORD IN COOKING        (EU=6)
!RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES DRYING (EU=7)
!RECTY          = RECORD NUMBER IN RSMEQP FILE

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT TYPE (EQT), RECORD # FOR TECH MENU, AND FUEL TYPE (F)
          EQC=RTTYEQCL(RECTY)
          EQT=RTEQTYPE(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          F=RTFUEL(RECCL)

          !COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
          IF (RTEQEFF(RECTY).NE.0.0) THEN
            RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
          ELSE
            RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
            RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
          ENDIF

          !SET CAPITAL COSTS
          ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
          ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
          IF (COSTTRSW.EQ.1) THEN
            CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
          ELSE
            CAPITAL = RTEQCOST(RECTY)
          ENDIF

          !CALCULATE OPERATING COST
          OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)
          OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(2)

          !CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
          IF ((CurCalYr.GT.2008).AND. &	!TODO - 2008 marks last year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)? Remove legacy energy bill code as necessary
           (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
            HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
            ELIGBLE=HRDRATE - 0.07
            IF (ELIGBLE.GT.0.0) THEN
              HRDADJ= ELIGBLE * ((PRICES(F,R,CurCalYr)/PRICES(F,R,RECSYear))**ALPHA1 )
              BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
            ELSE
              BETA1DR(RECTY)=RTECBTA1(RECTY)
            ENDIF
          ELSE
            BETA1DR(RECTY)=RTECBTA1(RECTY)
          ENDIF

          !CALCULATE LIFE CYCLE COSTS
          LFCY(EQT,B,R,1)=RTEQCOST(RECTY) + (OPCOST(1) *DECAY)
          LFCY(EQT,B,R,2)=RTEQCOST(RECTY) + (OPCOST(2) *DECAY)

          !COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
          ECTEMP = RTECBIAS(RECTY) + BETA1DR(RECTY)*CAPITAL

          EQWTN(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(2)) + ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
          TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
          EQWTR(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(1)) + ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
          TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 19
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,87)'19,',EU, ',',EQC, ',','X,',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,',EQWTN(EQT,B,R),',',EQWTR(EQT,B,R),',',ECTEMP,',',BETA1DR(RECTY),',',CAPITAL,',',RTECBIAS(RECTY),',',OPCOST(1),',',OPCOST(2),',','X,',RTECBTA2(RECTY),',',RTECBTA3(RECTY),',',LFCY(EQT,B,R,1),',','X,',TOTEWTR(EQC,B,R),',',TOTEWTN(EQC,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              87 FORMAT(a,I,a,I,a,a,  I,a,I,a,I,a,I,a,a,  a,  f,a,f,a,f,a,f,a,f,a,f,a,f,a,f,a,a,  f,a,f,a,f,a,a,  f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF
		 
        ENDIF  !RTCENDIV
      ENDIF  !CurCalYr
    ENDDO  !RECTY
  ENDDO  !B
ENDDO  !R

!********************************************************************
!CALCULATE NEW AND REPLACEMENT MARKET SHARES
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    TYPE=RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1

          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT TYPE (EQT)
          EQC=RTTYEQCL(RECTY)
          EQT=RTEQTYPE(RECTY)
		  
          !SET NEW EQUIPMENT SHARE
          IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
            RECCL=RTCLEUPT(EU)+EQC
            NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R))
            REQTSHR(CurCalYr,TYPE,B,R)=(EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R))
          ELSE
            NEQTSHR(CurCalYr,TYPE,B,R)=0.0
            REQTSHR(CurCalYr,TYPE,B,R)=0.0
          ENDIF
		  
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 20
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,88)'20,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              88 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!CALCULATE WEIGHTED EFFICIENCY
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC   =RTCLEQCL(RECCL)
      DENOM =0
      DENOM2=0
      COUNT =0
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (RTTYEQCL(RECTY).EQ.EQC.AND.CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            COUNT=COUNT+1
            EQT=RTEQTYPE(RECTY)
            RECAR(COUNT)=RECTY
            EQTAR(COUNT)=EQT
            DENOM =DENOM+EQWTN(EQT,B,R)
            DENOM2=DENOM2+EQWTR(EQT,B,R)
          ENDIF
        ENDIF
      ENDDO

      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          EQT=EQTAR(L)
          SUM=SUM+(EQWTN(EQT,B,R)/RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
      ENDIF

      !COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
      IF (DENOM2.LE.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM2=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          EQT=EQTAR(L)
          SUM2=SUM2+(EQWTR(EQT,B,R)/RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM2/DENOM2
      ENDIF

    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

END SUBROUTINE RDRYTEC


!==============================================================================
! CLOTHES DRYERS ADDED SUBROUTINE
!==============================================================================
SUBROUTINE RDRYADD
IMPLICIT NONE

REAL*4 SWT(RECSYear:EndYr),SWF(RECSYear:EndYr)
REAL*4 SA, HSR, ESR, SVRTE, P, max_eldry_pen
INTEGER NUMEQT,RECCLSW
INTEGER EU,EQC,EQT,RECCL,RECTY,TYPE,Y,R,B,TEMP,V,Y1

EU = 7  !clothes drying
max_eldry_pen = 0.90  !Max 90% electric clothes dryer penetration into remaining RECSYear housing stock (analyst judgment)

!****************************************************************************************************************
!  DRYER Shares - 1=NG_DRY1, 2=NG_DRY2, 3=NG_DRY3, 4=NG_DRY4, 5=ELEC_DRY1, 6=ELEC_DRY2, 7=ELEC_DRY3, 8=ELEC_DRY4
!  DRYERS - 1=NG_DRY 2=ELEC_DRY
!****************************************************************************************************************

!Initialize arrays
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCADD(CurCalYr,RECCL,B,R)=0.0
      EQCSR90(CurCalYr,RECCL,B,R)=0.0
      EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
      EQCSUR(CurCalYr,RECCL,B,R)=0.0
      EQCREP(CurCalYr,RECCL,B,R) = 0.0
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE CLOTHES DRYING EQUIPMENT ADDED IN CurCalYr
!********************************************************************
!CUMULATE SURVIVING NEW EQUIPMENT ADDED PRIOR TO CurCalYr TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW EQUIPMENT ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) -  SURV.EQUIP(EQCSUR)
!*******************************************************************
!Calculate clothes dryers added in CurCalYr (CurCalYr-1)
! RTTYEUPT(EU)   = LAST RECORD IN COOKING        (EU=6)
! RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES DRYING (EU=7)
! RECTY          = RECORD NUMBER IN RSMEQP FILE
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    TYPE=RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          EQC=RTTYEQCL(RECTY)
          EQT=RTEQTYPE(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          IF (CurCalYr.GT.RECSYear+1) THEN
            NEWDRYSAT(CurCalYr,2,B,R)=NEWDRYSAT(CurCalYr-1,2,B,R)*1.0064  !Average annual penetration rate of electric clothes dryers (E=2) into new homes
            NEWDRYSAT(CurCalYr,1,B,R)=NEWDRYSAT(CurCalYr-1,1,B,R)  !Average annual penetration rate of natural gas clothes dryers (E=1) into new homes; natural gas dryer penetration not assumed to increase over time like electric
          ENDIF
          IF ((NEWDRYSAT(CurCalYr,1,B,R)+NEWDRYSAT(CurCalYr,2,B,R)).GT.1.0000) THEN
            NEWDRYSAT(CurCalYr,2,B,R)=1.0000  !Prevents penetration of clothes dryers into new homes since RECS year from exceeding 100%
          ENDIF
          EQCADD(CurCalYr,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R) + (NEQTSHR(CurCalYr,TYPE,B,R) &
           *HSEADD(CurCalYr,B,R)*NEWDRYSAT(CurCalYr,EQC,B,R)))
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE AND THEN COUNT VALID TYPES IN CURRENT END USE
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      SA = 0.0
      IF (RTCLNAME(RECCL).EQ.'ELEC_DRY') THEN
        IF ((EQCND90(CurCalYr,RECCL,B,R)/EH(CurCalYr,B,R)).GE.max_eldry_pen) THEN
          EQCND90(CurCalYr,RECCL,B,R)=EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))- &
           EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))
        ELSE
          EQCND90(CurCalYr,RECCL,B,R)=EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))*(1.+ELDRYPR(B,R))- &
           EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))
        ENDIF !90% electric dryer penetration into remaining RECSYear housing stock
      ELSE
        EQCND90(CurCalYr,RECCL,B,R)=0.0
      ENDIF ! RTCLNAME='ELEC_DRY'
      !Calculate replacement equipment from original base year stock
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
         *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
        EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
         EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF
      !COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
           -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
           EQCND90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
        ENDDO
      ENDIF
      EQCRP90RP(CurCalYr,RECCL,B,R)=EQCRP90RP(CurCalYr,RECCL,B,R) + EQCND90(CurCalYr,RECCL,B,R)
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
           EQCND90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP)) ))
          HSR=HDR(B)**(TEMP)
          SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
          EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
           (((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR)) ))
        ENDDO
      ENDIF

      !*******************************************************************
      !CALCULATE REPLACEMENT CLOTHES DRYERS FOR NEW VINTAGE IN CurCalYr-1
      ! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
      ! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
      !*******************************************************************
      ! SUBROUTINE REPLACE DISTRIBUTES REPLACEMENTS IN POST-RECS-YEAR
      !  SINGLE-FAMILY HOMES WHEN LAST ARGUEMENT = 1
      IF (B.EQ.1) THEN
        !First, store what replacements would have been if no switching allowed.
        OEQCREP(CurCalYr,RECCL,1,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
        !Call REPLACE to distribute replacements.
        CALL REPLACE(EU,R,B,RECCL,1)
      ELSE
        !No switching allowed in multifamily or mobile homes.
        EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
      ENDIF

    ENDDO
  ENDDO  !B
ENDDO  !R

!The following call to REPLACE with final argument = 2 distributes replacements in existing single-family homes
B = 1
DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    OEQCRP90(CurCalYr,RECCL,B,R) = EQCRP90(CurCalYr,RECCL,1,R)
    OEQCRP90R(CurCalYr,RECCL,B,R) = EQCRP90RP(CurCalYr,RECCL,1,R)
    CALL REPLACE(EU,R,B,RECCL,2)
  ENDDO
ENDDO

B = 1
DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    SWITCHTO(CurCalYr,RECCL,B,R)=0.0
    SWITCHTOR(CurCalYr,RECCL,B,R)=0.0
    DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      IF (RECCLSW.NE.RECCL) THEN
        SWITCHTO(CurCalYr,RECCL,B,R)=SWITCHTO(CurCalYr,RECCL,B,R)+EQCSW90(CurCalYr,RECCLSW,RECCL,B,R)
        SWITCHTOR(CurCalYr,RECCL,B,R)=SWITCHTOR(CurCalYr,RECCL,B,R)+EQCSW90R(CurCalYr,RECCLSW,RECCL,B,R)
      ENDIF
    ENDDO
  ENDDO
ENDDO

B = 1
DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQCRP90(CurCalYr,RECCL,B,R)= EQCRP90(CurCalYr,RECCL,B,R)-SWITCHES(CurCalYr,RECCL,B,R)
    EQCRP90RP(CurCalYr,RECCL,B,R)= EQCRP90RP(CurCalYr,RECCL,B,R)-SWITCHESR(CurCalYr,RECCL,B,R)+ &
    SWITCHTOR(CurCalYr,RECCL,B,R)+SWITCHTO(CurCalYr,RECCL,B,R)
  ENDDO
ENDDO

B=1
SWF(CurCalYr)=0.0
SWT(CurCalYr)=0.0
DO  R=1,mNumCR-2
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    SWT(CurCalYr)=SWT(CurCalYr)+SWITCHTO(CurCalYr,RECCL,B,R)+SWITCHTOR(CurCalYr,RECCL,B,R)
    SWF(CurCalYr)=SWF(CurCalYr)+SWITCHES(CurCalYr,RECCL,B,R)+SWITCHESR(CurCalYr,RECCL,B,R)
  ENDDO
ENDDO

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DO Y=CurCalYr,EndYr  !VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
        TEMP=Y-CurCalYr
        HSR=HDR(B)**(TEMP)
        ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
        EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!AGGREGATE CLOTHES DRYERS FOR INVESTMENT ANALYSIS
!*******************************************************************
Y=CurCalYr
NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

DO B=1,mNumBldg
  DO r=1,mNumCR-2
    TYPE=RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1  ! INDEX FOR 'TYPE' VARIABLES
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
          HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
           REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r)  )

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 21
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,89)'21,',EU, ',',EQC, ',','X,',B,',',R,',',Y,',',RECTY,',',TYPE,',',RECCL,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(Y,TYPE,B,r),',','X,',REQTSHR(Y,TYPE,B,r),',',EQCADD(Y,RECCL,B,r),',',HEATINGTYPEPURCH(Y,TYPE,B,R,1),',',HEATINGTYPEPURCH(Y,TYPE,B,R,2),',',EQCREP(Y,RECCL,B,r),',',EQCRP90RP(Y,RECCL,B,r),',',EQCRP90(Y,RECCL,B,r),',','X,','X,','X,','X,','X'
              89 FORMAT(a,I,a,I,a,a,  I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF   
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
  DO R=1,mNumCR-2
    DRYERTOT(RECSYear,R)=0.0
    DO B=1,mNumBldg
      DRYERTOT(RECSYear,R)=DRYERTOT(RECSYear,R)+EQCESE(RECSYear,26,B,R) !NG_DRY  !EqpParam  !NoKero	!TODO - replace 26 with parameter read from RSCLASS
    ENDDO
  ENDDO
ELSE
  DO R=1,mNumCR-2
    DRYERTOT(CurCalYr,R)=0.0
    DO B=1,mNumBldg
      DRYERTOT(CurCalYr,R)=DRYERTOT(CurCalYr,R)+EQCESE(CurCalYr,26,B,R)+EQCADD(CurCalYr,26,B,R)+& !NG_DRY  !EqpParam  !NoKero	!TODO - replace 26 with parameter read from RSCLASS
       EQCRP90(CurCalYr,26,B,R)+EQCRP90RP(CurCalYr,26,B,R)+EQCSUR(CurCalYr,26,B,R)+EQCREP(CurCalYr,26,B,R)+& !NG_DRY  !EqpParam  !NoKero	!TODO - replace 26 with parameter read from RSCLASS
       EQCSR90(CurCalYr,26,B,R) !NG_DRY  !EqpParam  !NoKero	!TODO - replace 26 with parameter read from RSCLASS
    ENDDO
  ENDDO
ENDIF

!Proxy for gas customers is gas water heating for CDs 1,2,7, and 9	!TODO - verify statement about specific census divisions; no special treatment actually applied at CD-level?
DO R=1,mNumCR-2
 IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
    IF (DRYERTOT(RECSYear,R).GT.RSGASCUST(RECSYear,R)) THEN
      RSGASCUST(RECSYear,R)=DRYERTOT(RECSYear,R)
    ENDIF
 ELSE
    IF (DRYERTOT(CurCalYr,R).GT.RSGASCUST(CurCalYr,R)) THEN
      RSGASCUST(CurCalYr,R)=DRYERTOT(CurCalYr,R)
    ENDIF
 ENDIF
ENDDO

END SUBROUTINE RDRYADD


!==============================================================================
! CLOTHES DRYING CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE RDRYCON
IMPLICIT NONE

REAL*4 TEMP,ALPHA,ef1,ef2,ef3
INTEGER B, E, D, F, EQC, RECCL, EU, EUPR,FCON,Y,R

EU = 7  !clothes drying
EUPR=5
ALPHA = -.15; ef1 = .5; ef2 = .35; ef3 = .15

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!*******************************************************************
!COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
IF (CurCalYr.GE.RECSYear+1) THEN
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
  ENDDO
ENDIF

!********************************************************************
!CALCULATE NEW AND AVERAGE UECS
!********************************************************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       ( RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )
      IF (CurCalYr.EQ.RECSYear+1) THEN
        IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
        ELSE
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
        ENDIF
      ELSE
        IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
        ELSE
          EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
        ENDIF
      ENDIF

      IF (WTEQCEFFR(CurCalYr,RECCL,B,D).GT.0.0) THEN
        EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
         WTEQCEFFR(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
      ELSE
        EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
      ENDIF

      IF (CurCalYr .EQ. RECSYear+1) THEN
        EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP.LE.0.0) THEN
          EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
        ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+( &
             (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
DO B=1,mNumBldg
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP .GT. 0.0) THEN
          WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
             (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
          ENDDO
        ELSE
          WTEQCEFFA(CurCalYr,RECCL,B,D)= &
          WTEQCEFFN(CurCalYr,RECCL,B,D)
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE CLOTHES DRYING CONSUMPTION
!*******************************************************************

!Initialize arrays
DRYCON(CurIYr,1:NDRYFL,1:mNumCR-2)=0.0
DRYCONIN(CurIYr,1:NDRYFL,1:mNumCR-2,1:mNumBldg)=0.
Driver(CurIYr,1:NDRYFL,1:mNumCR-2,1:mNumBldg)=0.
DRYCONWT(CurIYr,1:NDRYFL,1:mNumCR-2,1:mNumBldg)=0.

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      E=RTCLEQCL(RECCL)
      F=RTFUEL(RECCL)
      FCON=FDRYCON(F)

      IF (CurCalYr.EQ.RECSYear+1) THEN
        DRYCON(CurIYr,FCON,D)=DRYCON(CurIYr,FCON,D)+LEAPYR* ( &
         (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)) + &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
        DRYCONWT(CurIYr,FCON,D,B)=DRYCONWT(CurIYr,FCON,D,B)+ LEAPYR* ( &
         (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)) + &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
	  
        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
          DRYCONIN(CurIYr,FCON,D,B)=DRYCONIN(CurIYr,FCON,D,B)+((&
           (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)) + &
           (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) )
          Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+ &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D) )
        ENDIF
	  
        EQCEQCN(CurIYr,RECCL,B,D)=LEAPYR* ( &
         (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
	  
      ELSE
        IF (F.EQ.4) THEN
          ALPHA=-0.30  !was -0.15 prior to American Recovery and Reinvestment Act of 2009 (ARRA) stimulus; permanently affects price elasticity (but not rebound) based on the smart grid concept
        ELSE
          ALPHA=-0.15
        ENDIF
	  
        DRYCON(CurIYr,FCON,D)=DRYCON(CurIYr,FCON,D)+LEAPYR*( &
         (EQCESE(CurCalYr,RECCL,B,D) *EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
         (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) ) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
	   
        DRYCONWT(CurIYr,FCON,D,B)=DRYCONWT(CurIYr,FCON,D,B)+ LEAPYR*( &
         (EQCESE(CurCalYr,RECCL,B,D) *EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
         (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) ) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
	  
        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
         EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
         EQCSUR(CurCalYr,RECCL,b,D).GT.0.) THEN
          DRYCONIN(CurIYr,FCON,D,B)=DRYCONIN(CurIYr,FCON,D,B)+ (( &
           (EQCESE(CurCalYr,RECCL,B,D) *EQCSUEC(CurCalYr,RECCL,B,D))+ &
           (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
           (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) ) )
	  
          Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+ &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
           EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
           EQCSUR(CurCalYr,RECCL,b,D))
        ENDIF
	  
        EQCEQCN(CurIYr,RECCL,B,D)=LEAPYR* ( &
         (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
         (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) ) &
         * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

      ENDIF  !CurCalYr
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !D

DO R=1,mNumCR-2
  DO FCON=1,NDRYFL
    DO B=1,mNumBldg
      IF (Driver(CurIYr,FCON,R,B).GT.0) &
       DRYCONIN(CurIYr,FCON,R,B)=DRYCONIN(CurIYr,FCON,R,B)/Driver(CurIYr,FCON,R,B)
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE RDRYCON


!==============================================================================
! REFRIGERATOR CHOICE SUBROUTINE
!==============================================================================
SUBROUTINE RREFTEC
IMPLICIT NONE

REAL*4 TMFSHR(mNumBldg,mNumCR-2)   !share for top-mounted freezers
REAL*4 SMFSHR(mNumBldg,mNumCR-2)   !share for side-mounted freezers
REAL*4 BMFSHR(mNumBldg,mNumCR-2)   !share for bottom-mounted freezers
REAL*4 DECAY,OPCOST1,LFCYCLE,FACTOR,UEC(MNUMRTTY)
REAL*4 EQWTN(nRefrTypes,mNumBldg,mNumCR),EQWTR(nRefrTypes,mNumBldg,mNumCR), &
 TOTEWTN(nRefrClasses,mNumBldg,mNumCR),TOTEWTR(nRefrClasses,mNumBldg,mNumCR)
REAL*4 DENOM, DENOM2, SUM
REAL*4 EQCOST,CAPITAL,RETAIL
INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
INTEGER RECAR(nRefrTypes),EQTAR(nRefrTypes)  !EqpParam

!********************************************************************
! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES	!TODO - combine/optimize similar code
!********************************************************************

EU = 8  !refrigeration
EUPR = 6
ALPHA1 = -0.50
FACTOR = .003412  !FACTOR TO CONVERT FROM REFRIGERATOR EFFICIENCY TO UEC

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO
      
!COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
IF (CURITR.EQ.1) THEN
  DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
ENDIF

!INITIALIZE ARRAYS
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      TOTEWTN(EQC,B,R)=0.0
      TOTEWTR(EQC,B,R)=0.0
      WTEQCEFFN(CurCalYr,RECCL,B,R)=0.0
      WTEQCEFFR(CurCalYr,RECCL,B,R)=0.0
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE OPERATING COSTS, LIFE-CYCLE COSTS, EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!********************************************************************
!RTTYEUPT(EU)   = LAST RECORD IN CLOTHES DRYING (EU=7)
!RTTYEUPT(EU+1) = LAST RECORD IN REFRIGERATION (EU=8)
!RECTY          = RECORD NUMBER IN RSMEQP FILE

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT TYPE (EQT), RECORD # FOR TECH MENU, AND FUEL TYPE (F)
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          F=RTFUEL(RECCL)
          UEC(RECTY)=RTEQEFF(RECTY)*FACTOR

          !SET CAPITAL COSTS
		  ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
          ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
          IF (COSTTRSW.EQ.1) THEN
            CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
          ELSE
            CAPITAL = RTEQCOST(RECTY)
          ENDIF

          !CALCULATE OPERATING COST
          OPCOST1=PRICES(F,R,CurCalYr)*UEC(RECTY)

          !CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
          IF ((CurCalYr.GT.2008).AND. &	!TODO - 2008 marks last year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)? Remove legacy energy bill code as necessary
           (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
            HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
            ELIGBLE=HRDRATE - 0.07	!TODO - verify source/ update 7%?
            IF (ELIGBLE.GT.0.0) THEN
              HRDADJ= ELIGBLE * ((PRICES(4,R,CurCalYr)/PRICES(4,R,RECSYear))**ALPHA1 )
              BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
            ELSE
              BETA1DR(RECTY)=RTECBTA1(RECTY)
            ENDIF  !eligible .GT.0
          ELSE
            BETA1DR(RECTY)=RTECBTA1(RECTY)
          ENDIF    !CurCalYr .GT. 2008

          !CALCULATE LIFE CYCLE COSTS
          LFCYCLE= CAPITAL+(OPCOST1*DECAY)
          EQWTN(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
           (BETA1DR(RECTY)*CAPITAL)+ &
           (RTECBTA2(RECTY)*OPCOST1)+ &
           (RTECBTA3(RECTY)*LFCYCLE))
          TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
          EQWTR(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
           (BETA1DR(RECTY)*CAPITAL)+ &
           (RTECBTA2(RECTY)*OPCOST1)+ &
           (RTECBTA3(RECTY)*LFCYCLE))
          TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 22
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,91)'22,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',','X,','X,','X,',EQWTN(EQT,B,R),',',EQWTR(EQT,B,R),',','X,',BETA1DR(RECTY),',',CAPITAL,',',RTECBIAS(RECTY),',',OPCOST1,',','X,','X,',RTECBTA2(RECTY),',',RTECBTA3(RECTY),',',LFCY(EQT,B,R,1),',','X,',TOTEWTR(EQC,B,R),',',TOTEWTN(EQC,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              91FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  f,a,f,a,a,  f,a,f,a,f,a,f,a,a,  a,  f,a,f,a,f,a,a,  f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF

        ENDIF  !RTCENDIV
      ENDIF  !CurCalYr
    ENDDO  !RECTY
  ENDDO  !B
ENDDO  !R

!********************************************************************
!CALCULATE NEW MARKET SHARES BY REFRIGERATOR CONFIGURATION
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    TMFSHR(B,R) = 0.
    BMFSHR(B,R) = 0.
    SMFSHR(B,R) = 0.
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1

          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT TYPE (EQT)
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC

          !SET NEW EQUIPMENT SHARE
          IF (EQT.LE.4) THEN  !refers to xlRTEQTYPE instances of REFR_TF in RSMEQP	!TODO - replace value with parameter?
            TMFSHR(B,R) = TMFSHR(B,R) + EQWTN(EQT,B,R)
          ELSEIF (EQT.GE.5 .AND. EQT.LE.8) THEN  !refers to xlRTEQTYPE instances of REFR_SF in RSMEQP	!TODO - replace value with parameter?
            SMFSHR(B,R) = SMFSHR(B,R) + EQWTN(EQT,B,R)
          ELSE  !refers to xlRTEQTYPE instances of REFR_BF in RSMEQP
            BMFSHR(B,R) = BMFSHR(B,R) + EQWTN(EQT,B,R)
          ENDIF !EQT
        ENDIF !RTCENDIV
      ENDIF !CurCalYr
    ENDDO !RECTY
  ENDDO !B
ENDDO !R

!********************************************************************
!CALCULATE NEW AND REPLACEMENT MARKET SHARES
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1

          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT TYPE (EQT)
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC

          !SET NEW EQUIPMENT SHARE
          IF (EQT.LE.4) THEN  !refers to xlRTEQTYPE instances of REFR_TF in RSMEQP
            NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/TMFSHR(B,R))*TMF_SHR
            REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) !choices the same for refrigerators
            IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 23  
              OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
                WRITE(661,92)'23,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',',TMF_SHR,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
                92 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,f,a,a,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
              CLOSE(661)
            ENDIF

            ELSEIF (EQT.GE.5 .AND. EQT.LE.8) THEN  !refers to xlRTEQTYPE instances of REFR_SF in RSMEQP  
              NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/SMFSHR(B,R))*SMF_SHR
              REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) !choices the same for refrigerators
            IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 23  
              OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
                WRITE(661,92)'23,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',',SMF_SHR,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
                CLOSE(661)
            ENDIF 

            ELSE  !refers to instances xlRTEQTYPE of REFR_BF in RSMEQP
              NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/BMFSHR(B,R))*BMF_SHR
              REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) !choices the same for refrigerators
              IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 23  
                OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
                WRITE(661,92)'23,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',',BMF_SHR,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              CLOSE(661)
            ENDIF 
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    !********************************************************************
    ! CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT EQUIPMENT
    !********************************************************************
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC   =RTCLEQCL(RECCL)
      DENOM =0
      DENOM2=0
      COUNT =0
    
      !TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
      ! INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE AND THEN COUNT VALID TYPES IN CURRENT END USE
      TYPE = RTTYPECT(EU)
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            TYPE=TYPE+1
            IF (RTTYEQCL(RECTY).EQ.EQC) THEN
              COUNT=COUNT+1
              EQT=RTEQTYPE(RECTY)
              RECAR(COUNT)=RECTY
              EQTAR(COUNT)=TYPE
              DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
              DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
            ENDIF  ! rttyeqcl=eqc filter refrigerators
          ENDIF   ! rtcendiv=r   filter regions
        ENDIF    ! filter years
      ENDDO      ! process RSMEQP records
    
      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
      ENDIF
    
      !COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
      IF (DENOM2.LE.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
        ENDDO
       WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
      ENDIF

    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

END SUBROUTINE RREFTEC


!==============================================================================
! REFRIGERATORS ADDED SUBROUTINE
!==============================================================================
SUBROUTINE RREFADD
IMPLICIT NONE

REAL*4 SA, HSR, ESR, SVRTE
INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EQT,NUMEQT,TYPE,RECTY,V

EU = 8  !refrigeration

!*******************************************************************
!CALCULATE REFRIGERATORS ADDED IN CurCalYr (CurCalYr-1)
! CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS-YEAR VINTAGE PRIOR TO CurCalYr
!*******************************************************************
!CUMULATE SURVIVING NEW REFRIGERATORS ADDED PRIOR TO CurCalYr TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW REFRIGERATORS ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-REFRIGERATORS)
!*******************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*REFSAT(B,R))

      !Initialize arrays
      EQCSR90(CurCalYr,RECCL,B,R)=0.0
      EQCSUR(CurCalYr,RECCL,B,R)=0.0
      EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
      SA=0.0

      !******************************************************************
      !Calculate replacement equipment from original base-year stock
      !******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
         *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
        EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
         EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF

      !COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
           -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
        ENDDO
      ENDIF

      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
          HSR=HDR(B)**(TEMP)
          SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
          EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
           (((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR)) ))
        ENDDO
      ENDIF

      !*******************************************************************
      !CALCULATE REPLACEMENT REFRIGERATORS FOR NEW VINTAGE IN CurCalYr-1
      ! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
      ! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
      !*******************************************************************
      EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)

    ENDDO !RECCL
  ENDDO !R
ENDDO !B

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DO Y=CurCalYr,EndYr  !VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
        TEMP=Y-CurCalYr
        HSR=HDR(B)**(TEMP)
        ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
        EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!AGGREGATE REFRIGERATORS FOR INVESTMENT ANALYSIS
!********************************************************************
Y=CurCalYr
NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    TYPE=RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1  ! INDEX FOR 'TYPE' VARIABLES
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
          HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
           REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 24
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,93)'24,',EU, ',',EQC, ',','X,',B,',',R,',',CurCalYr,',',RECTY,',',TYPE,',',RECCL,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',',EQCADD(Y,RECCL,B,r),',',HEATINGTYPEPURCH(Y,TYPE,B,R,1),',',HEATINGTYPEPURCH(Y,TYPE,B,R,2),',',EQCREP(Y,RECCL,B,r),',',EQCRP90RP(Y,RECCL,B,r),',',EQCRP90(Y,RECCL,B,r),',','X,','X,','X,','X,','X'
              93 FORMAT(a,I,a,I,a,a,  I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF
        ENDIF !RTCENDIV
      ENDIF !CurCalYr
    ENDDO !RECTY
  ENDDO !R
ENDDO !B

END SUBROUTINE RREFADD


!==============================================================================
! REFRIGERATION CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE RREFCON
IMPLICIT NONE

REAL*4 ALPHA,ef1,ef2,ef3,TEMP
INTEGER B, E, EUPR, D,EU,RECCL,EQC,F,Y,R

EU = 8  !refrigeration
EUPR=6
alpha = 0.0; ef1 = .5; ef2 = .35; ef3 = .15

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!*******************************************************************
!COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
IF (CurCalYr.GE.RECSYear+1) THEN
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
  ENDDO
ENDIF

!*******************************************************************
!CALCULATE NEW, REPLACEMENT, AND AVERAGE UECS
!*******************************************************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       ( RTBASEFF(CurCalYr,RECCL) / RTBASEFF(RECSYear,RECCL) )
      EQCNUEC(CurCalYr,RECCL,B,D)=(EQCUEC(D,RECCL,B)* &
       (WTEQCEFFN(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL)))*(1.0/REFSAT(B,D))+ &
       EQCUEC(D,RECCL,B)*(1.0-(1.0/REFSAT(B,D)))
      EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (WTEQCEFFR(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL))
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP.LE.0.0) THEN
          EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
        ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+( &
             (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
DO B=1,mNumBldg
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP .GT. 0.0) THEN
          WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
             (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
          ENDDO
        ELSE
          WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFN(CurCalYr,RECCL,B,D)
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE REFRIGERATION CONSUMPTION
!*******************************************************************

!Initialize arrays
REFCON(CurIYr,1:mNumCR-2)=0.0
REFCONWT(CurIYr,1:mNumCR-2,1:mNumBldg)=0.
Driver2(CurIYr,1:mNumCR-2,1:mNumBldg)=0.
REFCONIN(CurIYr,1:mNumCR-2,1:mNumBldg)=0.

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      F  =RTFUEL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        REFCON(CurIYr,D)=REFCON(CurIYr,D)+ LEAPYR*( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
         +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
         *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))
        REFCONWT(CurIYr,D,B)=REFCONWT(CurIYr,D,B)+LEAPYR*( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
         +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
         *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))
        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
          REFCONIN(CurIYr,D,B)=REFCONIN(CurIYr,D,B)+(  (&
          ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
          (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
          (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
          +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))))
          Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+ &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D) )
        ENDIF
        EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
         +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
         *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))
      ELSE
        REFCON(CurIYr,D)=REFCON(CurIYr,D)+ LEAPYR*( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
         +(EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))) &
         *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR) )
        REFCONWT(CurIYr,D,B)=REFCONWT(CurIYr,D,B)+ LEAPYR* ( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
         +(EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))) &
         *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR) )
        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
         EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
         EQCSUR(CurCalYr,RECCL,b,D).GT.0.) THEN
          REFCONIN(CurIYr,D,B)= REFCONIN(CurIYr,D,B)+( (&
           ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
           (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
           +(EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
           (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))))   )
          Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+ &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
           EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
           EQCSUR(CurCalYr,RECCL,b,D))
        ENDIF
        EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*( &
         ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
         +(EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))) &
         *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

      ENDIF  !CurCalYr
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !D

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    IF (Driver2(CurIYr,R,B).GT.0) &
     REFCONIN(CurIYr,R,B)=REFCONIN(CurIYr,R,B)/Driver2(CurIYr,R,B)
  ENDDO
ENDDO

END SUBROUTINE RREFCON


!==============================================================================
! STANDALONE FREEZER CHOICE SUBROUTINE
!==============================================================================
SUBROUTINE RFRZTEC
IMPLICIT NONE

REAL*4 CHSHR(mNumBldg,mNumCR-2)  !share for chest freezers
REAL*4 UPSHR(mNumBldg,mNumCR-2)  !share for upright freezers
REAL*4 DECAY,OPCOST2,LFCYCLE1,FACTOR,UEC(MNUMRTTY)
REAL*4 EQWTN(nFrezTypes,mNumBldg,mNumCR),EQWTR(nFrezTypes,mNumBldg,mNumCR), &
 TOTEWTN(nFrezClasses,mNumBldg,mNumCR),TOTEWTR(nFrezClasses,mNumBldg,mNumCR)
REAL*4 DENOM,DENOM2,SUM
REAL*4 EQCOST,CAPITAL,RETAIL
INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
INTEGER RECAR(nFrezTypes),EQTAR(nFrezTypes)

!********************************************************************
! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES	!TODO - combine/optimize similar code
!********************************************************************

EU = 9  !standalone freezing
EUPR = 7
ALPHA1 = -0.50
FACTOR = .003412  !FACTOR TO CONVERT FROM FREEZING EFFICIENCY TO UEC

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
IF (CURITR.EQ.1) THEN
  DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
ENDIF

!INITIALIZE ARRAYS
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      TOTEWTN(EQC,B,R)=0.0
      TOTEWTR(EQC,B,R)=0.0
      WTEQCEFFN(CurCalYr,RECCL,B,R)=0.0
      WTEQCEFFR(CurCalYr,RECCL,B,R)=0.0
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!CALCULATE OPERATING COSTS, LIFE-CYCLE COSTS, EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!********************************************************************
! RECTY          = RECORD NUMBER IN RSMEQP FILE
! RTTYEUPT(EU)   = LAST RECORD IN REFRIGERATION (EU=8)
! RTTYEUPT(EU+1) = LAST RECORD IN STANDALONE FREEZERS (EU=9)
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          EQT = RTEQTYPE(RECTY)
          EQC = RTTYEQCL(RECTY)
          RECCL = RTCLEUPT(EU)+EQC
          F = RTFUEL(RECCL)
          UEC(RECTY) = RTEQEFF(RECTY)*FACTOR

          !SET CAPITAL COSTS
		  ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
          ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
          IF (COSTTRSW.EQ.1) THEN
            CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
          ELSE
            CAPITAL = RTEQCOST(RECTY)
          ENDIF

          !CALCULATE OPERATING COST
          OPCOST2=PRICES(F,R,CurCalYr)*UEC(RECTY)

          !CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
          IF ((CurCalYr.GT.2008).AND. &
           (PRICES(4,R,CurCalYr).GT.PRICES(4,R,RECSYear))) THEN
            HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
            ELIGBLE=HRDRATE - 0.07	!TODO - source for 0.07?
            IF (ELIGBLE.GT.0.0) THEN
              HRDADJ= ELIGBLE *((PRICES(4,R,CurCalYr)/PRICES(4,R,RECSYear))**ALPHA1 )
              BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)	!TODO - source for 0.07?
            ELSE
              BETA1DR(RECTY)=RTECBTA1(RECTY)
            ENDIF
          ELSE
            BETA1DR(RECTY)=RTECBTA1(RECTY)
          ENDIF

          !CALCULATE LIFE CYCLE COSTS
          LFCYCLE1= CAPITAL+(OPCOST2*DECAY)

          EQWTN(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
           (BETA1DR(RECTY)*CAPITAL)+ &
           (RTECBTA2(RECTY)*OPCOST2)+ &
           (RTECBTA3(RECTY)*LFCYCLE1))
          TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
          EQWTR(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
           (BETA1DR(RECTY)*CAPITAL)+ &
           (RTECBTA2(RECTY)*OPCOST2)+ &
           (RTECBTA3(RECTY)*LFCYCLE1))
          TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 25
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,94)'25,',EU, ',',EQC, ',',EQT,',',B,',',R,',',CurCalYr,',',RECTY,',','X,','X,',EQWTN(EQT,B,R),',',EQWTR(EQT,B,R),',','X,',BETA1DR(RECTY),',',CAPITAL,',',RTECBIAS(RECTY),',','X,',OPCOST2,',','X,',RTECBTA2(RECTY),',',RTECBTA3(RECTY),',',LFCY(EQT,B,R,1),',','X,',TOTEWTR(EQC,B,R),',',TOTEWTN(EQC,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X'
              94 FORMAT(a,I,a,I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  f,a,f,a,a,  f,a,f,a,f,a,a,  f,a,a,  f,a,f,a,f,a,a,  f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF

        ENDIF  !RTCENDIV
      ENDIF  !CurCalYr
    ENDDO  !RECTY
  ENDDO  !B
ENDDO  !R

!********************************************************************
!CALCULATE NEW MARKET SHARES BY FREEZER ORIENTATION
!********************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    CHSHR(B,R)=0.
    UPSHR(B,R)=0.
    TYPE = RTTYPECT(EU)

    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          IF (EQT.LE.3) THEN  !refers to xlRTEQTYPE instances of FREZ_C in RSMEQP !techupdate - update hard-coded EQT value
            CHSHR(B,R) = CHSHR(B,R) + EQWTN(EQT,B,R)
          ELSE  !refers to xlRTEQTYPE instances of FREZ_U in RSMEQP
            UPSHR(B,R) = UPSHR(B,R) + EQWTN(EQT,B,R)
          ENDIF
        ENDIF !RTCENDIV
      ENDIF !CurCalYr
    ENDDO !RECTY
  ENDDO !B
ENDDO !R

!*******************************************************************
!CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    TYPE = RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      IF (CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1

          !FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT TYPE (EQT)
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC

          !SET NEW EQUIPMENT SHARE
          IF (EQT.LE.3) THEN  !refers to xlRTEQTYPE instances of FREZ_C in RSMEQP !techupdate - update hard-coded EQT value
            NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/CHSHR(B,R))*CH_SHR
            REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) !choices the same for freezers
            IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 26
              OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
                WRITE(661,95)'26,',EU, ',','X,','X,',B,',',R,',',CurCalYr,',',RECTY,',',TYPE,',','X,',EQWTN(EQT,B,R),',',UP_SHR,',',CHSHR(B,R),',',CH_SHR,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',UPSHR(B,R)
                95 FORMAT(a,I,a,a,a,I,a,I, a, I,a, I,a,I,a, a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f)
              CLOSE(661)  
            ENDIF

            ELSE  !refers to xlRTEQTYPE instances of FREZ_U in RSMEQP
              NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/UPSHR(B,R))*UP_SHR
              REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) !choices the same for freezers
            IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 26
              OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
                WRITE(661,95)'26,',EU, ',','X,','X,',B,',',R,',',CurCalYr,',',RECTY,',',TYPE,',','X,',EQWTN(EQT,B,R),',',UP_SHR,',',CHSHR(B,R),',',CH_SHR,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',UPSHR(B,R)
                CLOSE(661)
            ENDIF
          ENDIF !EQT
        ENDIF !RTCENDIV
      ENDIF !CurCalYr
    ENDDO !RECTY

    !********************************************************************
    ! CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT EQUIPMENT
    !********************************************************************
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC   =RTCLEQCL(RECCL)
      DENOM =0
      DENOM2=0
      COUNT =0

      !TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
      ! INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE AND THEN COUNT VALID TYPES IN CURRENT END USE
      TYPE = RTTYPECT(EU)
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        IF (CurCalYr.GE.RTINITYR(RECTY) &
         .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN
            TYPE=TYPE+1
            IF (RTTYEQCL(RECTY).EQ.EQC) THEN
              COUNT=COUNT+1
              EQT=RTEQTYPE(RECTY)
              RECAR(COUNT)=RECTY
              EQTAR(COUNT)=TYPE
              DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
              DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      !COMPLETE CALCULATION FOR NEW EQUIPMENT
      IF (DENOM.LE.0.0) THEN
        WTEQCEFFN(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
      ENDIF

      !COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
      IF (DENOM2.LE.0.0) THEN
        WTEQCEFFR(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
      ELSE
        SUM=0.0
        DO L=1,COUNT
          RECTY=RECAR(L)
          TYPE=EQTAR(L)
          SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
        ENDDO
        WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
      ENDIF
	  
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !R

END SUBROUTINE RFRZTEC


!==============================================================================
! STANDALONE FREEZERS ADDED SUBROUTINE
!==============================================================================
SUBROUTINE RFRZADD
IMPLICIT NONE

REAL*4 SA, HSR, ESR, SVRTE
INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EQT,NUMEQT,TYPE,RECTY,V

EU = 9  !standalone freezing

!*******************************************************************
!CALCULATE FREEZERS ADDED IN CurCalYr (CurCalYr-1)
! CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS-YEAR VINTAGE PRIOR TO CurCalYr
!*******************************************************************
!CUMULATE SURVIVING NEW FREEZERS ADDED PRIOR TO CurCalYr TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW FREEZERS ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-FREEZERS)
!*******************************************************************
DO R=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*FRZSAT(B,R))
	  
      !Initialize arrays
      EQCSR90(CurCalYr,RECCL,B,R)=0.0
      EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
      EQCSUR(CurCalYr,RECCL,B,R)=0.0
      SA=0.0

      !******************************************************************
      !Calculate replacement equipment from original base-year stock
      !******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
         *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
        EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
         EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF

      !COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
           -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
           EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
        ENDDO
      ENDIF

      IF (CurCalYr.GT.RECSYear+1) THEN
        DO Y=RECSYear+1,CurCalYr-1
          TEMP=CurCalYr-Y
          ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
          EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
           (EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
          EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
          HSR=HDR(B)**(TEMP)
          SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
          EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
           (((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR)) ))
        ENDDO
      ENDIF

      !*******************************************************************
      !CALCULATE REPLACEMENT STANDALONE FREEZERS FOR NEW VINTAGE IN CurCalYr-1
      ! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
      ! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
      !*******************************************************************
      EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)

    ENDDO !RECCL
  ENDDO !R
ENDDO !B

DO B=1,mNumBldg
  DO R=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      DO Y=CurCalYr,EndYr  !VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
        TEMP=Y-CurCalYr
        HSR=HDR(B)**(TEMP)
        ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
        EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
        EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!AGGREGATE STANDALONE FREEZERS FOR INVESTMENT ANALYSIS
!********************************************************************
Y=CurCalYr
NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

DO B=1,mNumBldg
  DO r=1,mNumCR-2
    TYPE=RTTYPECT(EU)
    DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
      !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
      IF (CurCalYr.GE.RTINITYR(RECTY).AND. &
        CurCalYr.LE.RTLASTYR(RECTY)) THEN
        IF (RTCENDIV(RECTY).EQ.R) THEN
          TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
          EQT=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
          HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
           REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r)  )
          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 27
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,96) '27,',EU, ',',EQC, ',','X,',B,',',R,',',CurCalYr,',',RECTY,',',TYPE,',',RECCL,',','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,','X,',NEQTSHR(CurCalYr,TYPE,B,R),',','X,',REQTSHR(CurCalYr,TYPE,B,R),',',EQCADD(Y,RECCL,B,r),',',HEATINGTYPEPURCH(Y,TYPE,B,R,1),',',HEATINGTYPEPURCH(Y,TYPE,B,R,2),',',EQCREP(Y,RECCL,B,r),',',EQCRP90RP(Y,RECCL,B,r),',',EQCRP90(Y,RECCL,B,r),',','X,','X,','X,','X,','X'
              96 FORMAT(a,I,a,I,a,a,  I,a,I,a,I,a,I,a,I,a,I,a,a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  a,  f,a,a,  f,a,f,a,f,a,f,a,f,a,f,a,f,a,a,  a,  a,  a,  a)
            CLOSE(661)
          ENDIF
        ENDIF !RTCENDIV
      ENDIF !CurCalYr
    ENDDO !RECTY
  ENDDO !R
ENDDO !B

END SUBROUTINE RFRZADD


!==============================================================================
! STANDALONE FREEZER CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE RFRZCON
IMPLICIT NONE

REAL*4 ALPHA,ef1,ef2,ef3,TEMP
INTEGER B,E,EUPR,D,EU,RECCL,EQC,F,Y,R

EU=9  !standalone freezing
EUPR=7
alpha = 0.0; ef1 = .5; ef2 = .35; ef3 = .15

!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
DO R=1,mNumCR-2
  PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!*******************************************************************
!COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
IF (CurCalYr.GE.RECSYear+1) THEN
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
  ENDDO
ENDIF

!*******************************************************************
!CALCULATE NEW, REPLACEMENT, AND AVERAGE UECS
!*******************************************************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (RTBASEFF(CurCalYr,RECCL)/RTBASEFF(RECSYear,RECCL))
      EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (WTEQCEFFN(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL))
      EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       (WTEQCEFFR(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL))
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
          EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP.LE.0.0) THEN
          EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
        ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+ &
             ((EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
DO B=1,mNumBldg
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
      ELSE
        TEMP=0.0
        DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
           EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
           EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
        ENDDO
        IF (TEMP .GT. 0.0) THEN
          WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
          DO Y=RECSYear,CurCalYr-1
            WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+ &
             ((EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
             ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
             EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
          ENDDO
        ELSE
          WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFN(CurCalYr,RECCL,B,D)
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE STANDALONE FREEZER CONSUMPTION
!*******************************************************************

!Initialize arrays
FRZCON(CurIYr,1:mNumCR-2)=0.0
FRZCONIN(CurIYr,1:mNumCR-2,1:mNumBldg)=0.
Driver2(CurIYr,1:mNumCR-2,1:mNumBldg)=0.
FRZCONWT(CurIYr,1:mNumCR-2,1:mNumBldg)=0.

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      EQC=RTCLEQCL(RECCL)
      F =RTFUEL(RECCL)
      IF (CurCalYr.EQ.RECSYear+1) THEN
        FRZCON(CurIYr,D)=FRZCON(CurIYr,D)+LEAPYR* &
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

        FRZCONWT(CurIYr,D,B)=FRZCONWT(CurIYr,D,B)+LEAPYR* &
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
          FRZCONIN(CurIYr,D,B)=FRZCONIN(CurIYr,D,B)+ &
          ((((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
          (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
          (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
          (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))))

          Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+ &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D))
        ENDIF

        EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR* &
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

      ELSE
        FRZCON(CurIYr,D)=FRZCON(CurIYr,D)+ LEAPYR* &
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
         (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

        FRZCONWT(CurIYr,D,B)=FRZCONWT(CurIYr,D,B)+LEAPYR* &
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
         (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

        IF (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
         EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
         EQCSR90(CurCalYr,RECCL,B,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
         EQCSUR(CurCalYr,RECCL,B,D).GT.0.) THEN
          FRZCONIN(CurIYr,D,B)=FRZCONIN(CurIYr,D,B)+ &
           ((((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
           (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
           (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
           (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
           (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))))

          Driver2(CurIYr,D,B)=Driver2(CurIYr,d,B)+ &
           (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
           EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
           EQCSR90(CurCalYr,RECCL,B,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
           EQCSUR(CurCalYr,RECCL,B,D))
        ENDIF

        EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR* &
         (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
         (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
         (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
         (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
         (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
         RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

      ENDIF  !CurCalYr
    ENDDO  !RECCL
  ENDDO  !B
ENDDO  !D

DO R=1,mNumCR-2
  DO B=1,mNumBldg
    IF (Driver2(CurIYr,R,B).GT.0) &
     FRZCONIN(CurIYr,R,B)=FRZCONIN(CurIYr,R,B)/Driver2(CurIYr,R,B)
  ENDDO
ENDDO

END SUBROUTINE RFRZCON


!==============================================================================
! LIGHTING CHOICE, STOCK, AND CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE LTCNS
IMPLICIT NONE

REAL*4  LTMSHR(RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins),opcost(MaxTypes), &
        WTLEFF(MaxApps,RECSYear:EndYr,mNumBldg,mNumCR-2,MaxBins), &
        TOTEWTN(mNumBldg,mNumCR-2,MaxBins), &
        EQWTN(MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTREPTOT(MaxApps,RECSYear:EndYr,mNumBldg,mNumCR-2,MaxBins), &
        LTREP(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTTOTSTOCK(MaxApps,RECSYear:EndYr,mNumBldg,mNumCR-2,MaxBins), &
        LTSTOCKEX(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTNEEDED(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTNEEDEDFUTly(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTREPFUTly(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTrepconsly(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTrepstkly(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTREPFUT(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTNEEDEDFUT(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTREPstk(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTREPcons(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LTstockexcons(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LightBeta1(MaxApps,RECSYear:EndYr,MaxTypes), &
        LightBeta2(MaxApps,RECSYear:EndYr,MaxTypes), &
        LightCalcA(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LightCalcB(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
        LightCalcC(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins)

REAL*4 temp, rep, cumrep, maxrep, annrep, ef1,ef2,ef3,FACTOR,LTLBeta1DR,ALPHA,alpha1, LHRATE,ELIGL,LHRDADJ,LGTBeta1,LGTBeta2,LGTBETA1DR
INTEGER Y,B,D,EUPR,Y1,R,E,T,BIN,y2,diagnostics,i,indx,app,ilife,iLastYr

!Initialize variables in pre-RECS and RECS years (all iterations)
LTCON(CurCalYr-BaseYr+1,1:mNumCR-2) = 0.0
LTCONwt(CurCalYr-BaseYr+1,1:mNumCR-2,1:mNumBldg) = 0.0
LTCONin(CurCalYr-BaseYr+1,1:mNumCR-2,1:mNumBldg) = 0.0

!Initialize variables and calculate consumption in first post-RECS year (first iteration)
IF (CurCalYr.EQ.RECSYear+1.AND.CURITR.EQ.1) THEN
  LTCON(RECSYear-BaseYr+1,1:mNumCR-2) = 0.0
  LTCONwt(RECSYear-BaseYr+1,1:mNumCR-2,1:mNumBldg) = 0.0
  LTCONin(RECSYear-BaseYr+1,1:mNumCR-2,1:mNumBldg) = 0.0

  DO app=1,NumApps
    DO y=RECSYear,LastYr+BaseYr-1
      LTNUEC(app,y,1:mNumCR-2,1:mNumBldg) = 0.0
    ENDDO

    !Filter Lighting Database for Current Year Values
    e=1 !initialize number of bulb types and add to it below
    DO i=1,NLRec
      IF (lightingapp(i).NE.AppID(app)) CYCLE
        !Application found, check years
        IF (CurCalYr-1.GE.firstyear(i).AND.CurCalYr-1.LE.lastyear(i)) THEN
          watts(e)=bulbwatts(i)
          DO bin=1,NumAppBins(app)
            bulbbinlife(e,bin)=lifehours(i)/(365.*appbinhours(app,bin))
          ENDDO
          LightBeta1(app,CurCalYr,e)=LTLBeta1(i)
          LightBeta2(app,CurCalYr,e)=LTLBeta2(i)
          IF (LightDiag .NE. 0) WRITE(9,*) 'Betas (i,app,CurCalYr,e,beta1,beta2): ',i,app,CurCalYr,e,&
          LightBeta1(app,CurCalYr,e),LightBeta2(app,CurCalYr,e)
          e=e+1
        ENDIF
    ENDDO !i

    IF (e-1 .NE. numtypes(app)) &
     WRITE(9,*) 'RESDMSG SUB_LTCNS: Lamps not lining up for application ', app

    DO bin=1,NumAppBins(app)
      LTBinShare(app,bin)=0.
    ENDDO

    !Compute equipment stocks, consumption per HH by application (LTUEC), and bin shares of energy use (LTBinShare)
    DO d=1,mNumCR-2
      DO B=1,mNumBldg
        LTEQP(app,RECSYear,B,d)=bulbsperhh(app,b)*EH(RECSYear,B,d)
        LTuec(app,d,b)=0.
        DO bin=1,NumAppBins(app)
          DO e=1,numtypes(app)
            LTUEC(app,d,B)=ltuec(app,d,b)+appbinhours(app,bin)*365.*binshares(app,bin)*bulbbinshares(app,e,bin) &
             *basewattsbulbs(app,e)*3.412/10**6
            LTBinShare(app,bin)=LTBinShare(app,bin)  +appbinhours(app,bin)*365.*binshares(app,bin)*bulbbinshares(app,e,bin) &
             *basewattsbulbs(app,e)*3.412/10**6
          ENDDO !e
        ENDDO !bin
      ENDDO
    ENDDO

    !LTBinShare contains total energy, now convert LTBinShare into shares of energy use
    temp=0.
    DO bin=1,NumAppBins(app)
      temp=temp+LTBinShare(app,bin)
    ENDDO
    DO bin=1,NumAppBins(app)
      LTBinShare(app,bin)=LTBinShare(app,bin)/temp
    ENDDO

    !Calculate Lighting Consumption
    DO d=1,mNumCR-2
      DO B=1,mNumBldg
        !Added next line to feed RECS year consumption into output database file
        lteqcn(RECSYear-BaseYr+1,app,b,d)=LTEQP(app,RECSYear,B,d)*LTUEC(app,d,B)
        LTCON(RECSYear-BaseYr+1,d)=LTCON(RECSYear-BaseYr+1,d)+(LTEQP(app,RECSYear,B,d)*LTUEC(app,d,B))
      ENDDO
    ENDDO

    DO d=1,mNumCR-2
      DO B=1,mNumBldg
        DO BIN=1,NumAppBins(APP)
          LTTOTSTOCK(app,RECSYear,B,d,BIN)=0.0
          DO E=1,numtypes(APP)
            LTSTOCK(app,RECSYear,E,B,d,BIN)=(LTEQP(app,RECSYear,B,d)*binshares(app,bin)*bulbbinshares(app,e,BIN))
            LTSTOCKEX(app,RECSYear,E,B,d,BIN)=(LTEQP(app,RECSYear,B,d)*binshares(app,bin)*bulbbinshares(app,e,BIN))
            LTstockexcons(app,RECSYear,E,B,d,BIN)=LTstockex(app,RECSYear,E,B,d,BIN) &
             *appbinhours(app,bin)*365.*basewattsbulbs(app,e)*3.412/10**6
            LTNEEDED(app,RECSYear,E,B,d,BIN)=0.0
            LTrepfut(app,RECSYear,E,B,d,BIN)=0.0
            LTREP(app,RECSYear,E,B,d,BIN)=0.0
            LTTOTSTOCK(app,RECSYear,B,d,BIN)=LTTOTSTOCK(app,RECSYear,B,d,BIN)+LTSTOCK(app,RECSYear,E,B,d,BIN)
          ENDDO
        ENDDO
      ENDDO
    ENDDO

    !This is the remaining RECSYear stock by bin projected into the future.
    ! Will become zero at some point for all bulb types depending on
    !  bin hours and bulb lives (see calculation of bulbbinlife(e, bin))
    DO d=1,mNumCR-2
      DO B=1,mNumBldg
        DO BIN=1,NumAppBins(app)
          DO Y1=RECSYear+1,LastYr+BaseYr-1
            DO E=1,numtypes(app)
              LTStockEx(app,Y1,E,B,d,BIN)= &
               max(0.,LTStockEx(app,Y1-1,E,B,d,BIN)-LTStockEx(app,RECSYear,E,B,d,BIN)*HDR(B)**(y1-RECSYear)/bulbbinlife(E,BIN))
              LTstockexcons(app,Y1,E,B,d,BIN)= LTStockEx(app,Y1,E,B,d,BIN) &
               *appbinhours(app,bin)*365.*basewattsbulbs(app,e)*3.412/10**6
            ENDDO !e
          ENDDO !y1
        ENDDO !bin
      ENDDO !b
    ENDDO !r
  ENDDO !NumApps
ENDIF !RECSYear processing on first iteration

!****************************
!PROCESS PROJECTION YEARS
!****************************

!Map Electricity Price into technology menu dollar year
! Prices in constant dollars, $/MMBtu
EUPR=8   !End use price for lighting

DO d=1,mNumCR-2
  PRICES(4,d,CurCalYr)=PELRSOUT(d,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!Price elasticities and distribution and behavioral elasticity adjustment factor(alpha1)
ALPHA = -.15; ef1 = .50; ef2 = .35; ef3 = .15; ALPHA1 = -0.01

!CONVERSION FACTOR FROM WattHours TO MMBTU
FACTOR = 3.412/10**6   !Btu/Watt to millions of Btu

!Loop through all applications
DO app=1,NumApps

  !Filter Lighting Database for Current Year Values
  e=1
  DO i=1,NLRec
    IF (lightingapp(i).NE.AppID(app)) CYCLE
      !Application found in data, now check years
      IF (CurCalYr.GE.firstyear(i).AND.CurCalYr.LE.lastyear(i)) THEN
        watts(e)=bulbwatts(i)
        DO bin=1,NumAppBins(app)
          bulbbinlife(e,bin)=lifehours(i)/(365.*appbinhours(app,bin))
        ENDDO
        LightBeta1(app,CurCalYr,e)=LTLBeta1(i)
        LightBeta2(app,CurCalYr,e)=LTLBeta2(i)
        DO d =1,mNumCR-2
          LTLCap(e,d,1)=bulbcost(i)-bulbEEsub(i,d)-bulbsub(i,d)*FLOAT(EPA111D) !put bulb cost in bin 1 for now
          ltlsub(e,d)=bulbEEsub(i,d)+bulbsub(i,d)*FLOAT(EPA111D)
          LTlCapInvest(e)=bulbcost(i)
        ENDDO

        cribulb(e)=bulbcri(i)
        appbulbname(app,e)=bulbtype(i)
        e=e+1
      ENDIF
  ENDDO

  IF (e-1 .NE. numtypes(app)) & !write to unit 6 (nohup.out)
   WRITE(6,*) 'RESDMSG SUB_LTCNS: Error - ', AppID(app),' lamps not lining up'
  IF (e-1 .NE. numtypes(app) .AND. LightDiag.EQ.1) & !write to unit 9 (RDM_OUT.txt)
   WRITE(9,*) 'RESDMSG SUB_LTCNS: Error - ', AppID(app),' lamps not lining up'

  !Further processing of LTLCAP - adjust for CRI and multiple replacements per year
  DO d=1,mNumCR-2
    DO e=1,NUMTYPES(APP)
      temp=LTLCap(e,d,1) !bulb cost DOesn't vary by bin until bin hours are accounted for below
      DO bin=1,NumAppBins(app)
        LTLCap(e,d,bin)= temp/(cribulb(e)/100.)**2
        !If bulb lasts less than a year in this bin, then increase capital costs based on number of replacements per year
        IF (bulbbinlife(e,bin) .LT. 1.) LTLCap(e,d,bin)=(temp/bulbbinlife(e,bin))/(cribulb(e)/100.)**2
      ENDDO
    ENDDO
  ENDDO

  !Operating Cost and Logit Shares
  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      !lighting diagnostics	!TODO - check significance of years below; otherwise, comment out?
      diagnostics=0
      IF (d.EQ.1 .AND. b.EQ.1 .AND. CurCalYr.EQ.2020) diagnostics=1
      IF (d.EQ.1 .AND. b.EQ.1 .AND. CurCalYr.EQ.2021) diagnostics=1
      IF (diagnostics.EQ.1 .AND. LightDiag.EQ.1) &
       WRITE(9,*) 'year, app, bin, e, LTLBeta1dr, LTLCAP, bulbbinlife(e,bin), LightBeta2(app,CurCalYr,e), opcost(e), prices, watts, factor, LTHOURS(e), logit calc(e) '
      diagnostics=0

      DO BIN=1,NumAppBins(app)
        TOTEWTN(B,d,BIN)=0.0
        DO E=1,numtypes(app)
          !CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES	!TODO - still needed?
          ! Note: to temporarily disable discount rate adjustment, uncomment the following line and comment the one below
          !IF ((CurCalYr.GT.EndYr).AND. &
          IF ((CurCalYr.GT.2008).AND. &
           (PRICES(4,d,CurCalYr).GT.PRICES(4,d,RECSYear))) THEN
            LHRATE=LightBeta1(app,CurCalYr,e)/LightBeta2(app,CurCalYr,e)
            ELIGL=LHRATE - 0.07	!TODO - 0.07 reference?
            IF (ELIGL.GT.0.0) THEN
              LHRDADJ= ELIGL * &
               ((PRICES(4,d,CurCalYr)/PRICES(4,d,RECSYear))**ALPHA1 )
              LTLBeta1DR = (LHRDADJ+0.07) * LightBeta2(app,CurCalYr,e)	!TODO - 0.07 reference?
            ELSE
              LTLBeta1DR=LightBeta1(app,CurCalYr,e)
            ENDIF
          ELSE
            LTLBeta1DR=LightBeta1(app,CurCalYr,e)
          ENDIF

          OPCOST(E)=PRICES(4,d,CurCalYr)*WATTS(E)*FACTOR*appbinHOURS(app,BIN)*365.
          EQWTN(E,B,d,BIN)=exp(LTLBeta1dr*LTLCap(e,d,bin)+LightBeta2(app,CurCalYr,e)*opcost(e))
          TOTEWTN(B,d,BIN)=TOTEWTN(B,d,BIN)+EQWTN(E,B,d,BIN)

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 30
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,81)'30,', OPCOST(E), ',', E, ',',  EQWTN(E,B,d,BIN), ',', B, ',', d, ',', BIN, ',', LTLBeta1dr, ',',LTLCap(e,d,bin), ',',LightBeta2(app,CurCalYr,e), ',', opcost(e), ',', TOTEWTN(B,d,BIN), ',', CurCalYr, ',', app
              81 FORMAT(a,f,a,i,a,f,a,i,a,i,a,i,a,f,a,f,a,f,a,f,a,f,a,i,a,i)
            CLOSE(661)
          ENDIF !lighting Beta writeout complete

          !Lighting diagnostics	!TODO - check significance of years below; otherwise, comment out? is this redundant code?
          diagnostics=0
          IF (d.EQ.1 .AND. b.EQ.1 .AND. CurCalYr.EQ.2020) diagnostics=1
          IF (d.EQ.1 .AND. b.EQ.1 .AND. CurCalYr.EQ.2021) diagnostics=1
          IF (diagnostics.EQ.1 .AND. LightDiag.EQ.1) &
           WRITE(9,443) CurCalYr, app, bin, e, LTLBeta1dr, LTLCap(e,d,bin), bulbbinlife(e,bin),  &
           LightBeta2(app,CurCalYr,e), opcost(e),PRICES(4,d,CurCalYr),watts(e),factor,appbinhours(app,bin),eqwtn(e,b,d,bin)
          diagnostics=0
        ENDDO !e
      ENDDO !bin
    ENDDO !B
  ENDDO !d

  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      DO E=1,NumTypes(app)
        DO BIN=1,6  !6 usage bins for lighting technologies (based on hours of use)	!TODO - replace 6 with a parameter?
          IF (TOTEWTN(B,d,BIN).NE.0.0) THEN
            LTMSHR(CurCalYr,E,B,d,BIN)=(EQWTN(E,B,d,BIN)/TOTEWTN(B,d,BIN))
          ELSE
            LTMSHR(CurCalYr,E,B,d,BIN)=0.0
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  !Diagnostics
  IF (CurCalYr.GT.RECSYear .OR. CurCalYr.LE.2025) THEN	!TODO - why 2025? AEO2005 was the last one to project through 2025, though even AEO2006 resd.f didn't include this code
    DO BIN=1,NumAppBins(app)
      IF (LightDiag.EQ.1) WRITE (18,442) 'SHR',CurCalYr,app,bin,(LTMSHR(CurCalYr,E,1,1,BIN),E=1,NumTypes(app))
    ENDDO
  ENDIF

  !Initialize future replacements for iteration control
  DO BIN=1,NumAppBins(app)
    DO d=1,mNumCR-2
      DO B=1,mNumBldg
        DO E=1,NumTypes(app)
          IF (CURITR.EQ.1) THEN  !first iteration
            DO Y1=CurCalYr,LastYr+BaseYr-1
              LTNEEDEDFUTly(app,y1,E,B,d,BIN)=LTNEEDEDFUT(app,y1,E,B,d,BIN)
              LTREPFUTly(app,y1,E,B,d,BIN)=LTREPFUT(app,y1,E,B,d,BIN)
              LTrepconsly(app,y1,e,B,d,BIN)=LTrepcons(app,y1,e,B,d,BIN)
              LTrepstkly(app,y1,e,B,d,BIN)=LTrepstk(app,y1,e,B,d,BIN)
              LTNUECly(app,Y1,d,b)=LTNUEC(app,Y1,d,b)
            ENDDO
          ELSE  !set subsequent iterations to be same as first iteration
            DO Y1=CurCalYr,LastYr+BaseYr-1
              LTNEEDEDFUT(app,y1,E,B,d,BIN)=LTNEEDEDFUTly(app,y1,E,B,d,BIN)
              LTREPFUT(app,y1,E,B,d,BIN)=LTREPFUTly(app,y1,E,B,d,BIN)
              LTrepcons(app,y1,e,B,d,BIN)=LTrepconsly(app,y1,e,B,d,BIN)
              LTrepstk(app,y1,e,B,d,BIN)=LTrepstkly(app,y1,e,B,d,BIN)
              LTNUEC(app,Y1,d,b)=LTNUECly(app,Y1,d,b)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  !Additional stock bulbs needed for this year's new construction and for newly added floorspace this year
  ! in existing homes that remain from the original RECSYear stock of homes.
  ! This is allocated by purchases and represents current year requirements only.
  DO BIN=1,NumAppBins(app)
    DO d=1,mNumCR-2
      DO B=1,mNumBldg
        DO E=1,NumTypes(app)
          LTNEEDED(app,CurCalYr,E,B,d,BIN)=(HSEADD(CurCalYr,B,d)*(SQRFOOT(CurCalYr,B,d)/SQRFOOT(RECSYear,B,d)) &
           *bulbsperhh(app,b)*LTMSHR(CurCalYr,E,B,d,BIN)*binshares(app,BIN) &
           +EH(CurCalYr,B,d)*((EXSQRFOOT(CurCalYr,B,d)/EXSQRFOOT(CurCalYr-1,B,d))-1.0) &
           *bulbsperhh(app,b)*LTMSHR(CurCalYr,E,B,d,BIN)*binshares(app,BIN))

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 31
            LightCalcA(app,CurCalYr,E,B,d,BIN) = 0
            LightCalcB(app,CurCalYr,E,B,d,BIN) = 0
            LightCalcA(app,CurCalYr,E,B,d,BIN) = HSEADD(CurCalYr,B,d)*(SQRFOOT(CurCalYr,B,d)/SQRFOOT(RECSYear,B,d))*bulbsperhh(app,b)*binshares(app,BIN)  !multiplying the independant variables so that we get to Ax+Bx to simplify calculations later
            LightCalcB(app,CurCalYr,E,B,d,BIN) = EH(CurCalYr,B,d)*((EXSQRFOOT(CurCalYr,B,d)/EXSQRFOOT(CurCalYr-1,B,d))-1.0)*bulbsperhh(app,b)*binshares(app,BIN)
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,82)'31,', LTNEEDED(app,CurCalYr,E,B,d,BIN), ',' , app, ',', CurCalYr, ',', E, ',', B, ',', d, ',', BIN, ',',LightCalcA(app,CurCalYr,E,B,d,BIN), ',', LightCalcB(app,CurCalYr,E,B,d,BIN), ',', LTMSHR(CurCalYr,E,B,d,BIN)
              82 FORMAT(a,f,a,i,a,i,a,i,a,i,a,i,a,i,a,f,a,f,a,f) 
            CLOSE(661)
          ENDIF !lighting Beta writeout complete
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  !Bulbs needed this year for all reasons
  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      DO BIN=1,NumAppBins(app)
        LTREPTOT(app,CurCalYr,B,d,BIN)=0.
        DO e=1,NumTypes(app)
          !This year's purchases equal replacements for original RECS stock plus replacements from past purchases needed this year
          ! plus new bulbs added due to new construction & floorspace additions in existing homes
          ! NOTE that LTrepfut(app,CurCalYr) is finalized in CurCalYr-1; we update the future stream for this year's purchases below

          LTREPTOT(app,CurCalYr,B,d,BIN) = LTREPTOT(app,CurCalYr,B,d,BIN) &
           + LTSTOCKEX(app,CurCalYr-1,E,B,d,BIN) * hdr(b) - LTSTOCKEX(app,CurCalYr,E,B,d,BIN) &
           + LTREPFUT(app,CurCalYr,E,B,d,BIN) + LTneeded(app,CurCalYr,e,b,d,bin)

          IF ((ApplianceBetaSwitch.EQ.1).AND.(CurCalYr.GT.RECSYear).AND.(CurCalYr.LE.ApplianceBetaEndYr)) THEN !Appbetadata writeout, betacode = 32
            LightCalcC(app,CurCalYr,E,B,d,BIN) = 0
            LightCalcC(app,CurCalYr,E,B,d,BIN) = LTSTOCKEX(app,CurCalYr-1,E,B,d,BIN)*hdr(b)-LTSTOCKEX(app,CurCalYr,E,B,d,BIN)+ LTREPFUT(app,CurCalYr,E,B,d,BIN)    !adding the independant variables so that we get to C+x to simplify calculations later
            OPEN(unit = 661, file = "AppBetaData.txt", action="write", position="append") !reopen the file, with append
              WRITE(661,83)'32,', LTNEEDED(app,CurCalYr,E,B,d,BIN), ',' , app, ',', CurCalYr, ',', E, ',', B, ',', d, ',', BIN, ',',LightCalcC(app,CurCalYr,E,B,d,BIN), ',',  LTREPTOT(app,CurCalYr,B,d,BIN)
              83 FORMAT(a,f,a,i,a,i,a,i,a,i,a,i,a,i,a,f,a,f) 
            CLOSE(661)
          ENDIF !lighting Beta writeout complete
        ENDDO
      ENDDO
    ENDDO
  ENDDO


  !Distribute purchases to bulb types based on purchase shares (LTMSHR), accumulate stocks and consumption
  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      DO BIN=1,NumAppBins(app)
        DO e=1,NumTypes(app)
          LTREP(app,CurCalYr,e,B,d,BIN)=LTREPTOT(app,CurCalYr,B,d,BIN)*LTMSHR(CurCalYr,E,B,d,BIN)
          LTREPstk(app,CurCalYr,E,B,d,BIN)=LTREPstk(app,CurCalYr,E,B,d,BIN)+LTREP(app,CurCalYr,e,B,d,BIN)
          LTrepcons(app,CurCalYr,e,B,d,BIN)=LTrepcons(app,CurCalYr,e,B,d,BIN)+LTrep(app,CurCalYr,e,B,d,BIN)*365.*appbinhours(app,bin)*watts(e)*3.412/10**6
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  !Extend this year's bulb purchases (LTREP) into the future replacement purchase requirements;
  ! extend this year's purchased bulbs into future purchased-bulb remaining stocks;
  ! compute the energy consumed for this year's purchases and the energy requirements
  ! for future remaining stocks from this year's purchases
  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      DO BIN=1,NumAppBins(app)
        DO E=1,NumTypes(app)
          cumrep=0.  !cumulative replacements
          rep=LTREP(app,CurCalYr,E,B,d,BIN)/max(1.,bulbbinlife(E,BIN)) !bulbs per year decaying from LTREP

          !restricting the looping will save some amount of execution time
          ilife=nint(bulbbinlife(E,BIN)+.5)+1 !round up bulblife as an INTEGER and add a year for looping	!TODO - what is .5?
          iLastYr=min(LastYr+BaseYr-1,CurCalYr+ilife)
          DO Y1=CurCalYr+1,iLastYr
            maxrep=LTREP(app,CurCalYr,E,B,d,BIN)
            maxrep=maxrep*hdr(b)
            annrep=max(0.,min(maxrep-cumrep,rep))

            IF (annrep .GT. 0.) THEN
              LTREPFUT(app,Y1,E,B,d,BIN)=LTREPFUT(app,Y1,E,B,d,BIN) + annrep*HDR(B)**(y1-CurCalYr)
              cumrep=cumrep+annrep
              LTREPstk(app,y1,E,B,d,BIN)=LTREPstk(app,y1,E,B,d,BIN)+ maxrep-cumrep
              LTrepcons(app,y1,E,B,d,BIN)=LTrepcons(app,y1,E,B,d,BIN)+(maxrep-cumrep)*365.*appbinhours(app,bin) &
               *watts(e)*3.412/10**6
            ENDIF
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  !LTrepstk is prior replacements remaining stock, so total stock equals RECS stock remaining
  ! plus this year's purchases plus prior year replacements still left in stock
  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      DO BIN=1,NumAppBins(app)
        DO E=1,NumTypes(app)
          LTSTOCK(app,CurCalYr,E,B,d,BIN)=LTSTOCKEX(app,CurCalYr,E,B,d,BIN)+LTrepstk(app,CurCalYr,E,B,d,BIN)

        ENDDO
      ENDDO
    ENDDO
  ENDDO

  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      LTEQP(app,CurCalYr,B,d)=0.0
      DO BIN =1,NumAppBins(app)
        LTTOTSTOCK(app,CurCalYr,B,d,BIN)=0.0
        DO E=1,NumTypes(app)
          LTTOTSTOCK(app,CurCalYr,B,d,BIN)=LTTOTSTOCK(app,CurCalYr,B,d,BIN)+LTSTOCK(app,CurCalYr,E,B,d,BIN)
          LTEQP(app,CurCalYr,B,d)=LTEQP(app,CurCalYr,B,d)+LTSTOCK(app,CurCalYr,E,B,d,BIN)
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      DO E=1,NumTypes(app)
        LTInvest(app,CurCalYr,E,B,d,1)=0.0
        LTInvest(app,CurCalYr,E,B,d,2)=0.0
        LTsubsidy(app,CurCalYr,E,B,d,1)=0.0
        LTsubsidy(app,CurCalYr,E,B,d,2)=0.0
        LTNEEDEDbyAPP(app,CurCalYr,E,B,d)=0.0
        LTREPbyAPP(app,CurCalYr,E,B,d)=0.0
        DO BIN=1,NumAppBins(app)
          LTNEEDEDbyAPP(app,CurCalYr,E,B,d)=LTNEEDEDbyAPP(app,CurCalYr,E,B,d)+LTNEEDED(app,CurCalYr,E,B,d,BIN)
          LTREPbyAPP(app,CurCalYr,E,B,d)=LTREPbyAPP(app,CurCalYr,E,B,d)+LTREP(app,CurCalYr,E,B,d,BIN)
          !adjust investment spending for bulbs lasting < 1 year by dividing by life in years
          temp=LTlCapInvest(E)-LTLSUB(e,d)
          IF (bulbbinlife(e,bin) .LT. 1.) temp=temp/bulbbinlife(e,bin)
          LTInvest(app,CurCalYr,E,B,d,1)=LTInvest(app,CurCalYr,E,B,d,1)+LTNEEDED(app,CurCalYr,E,B,d,BIN)*temp
          LTInvest(app,CurCalYr,E,B,d,2)=LTInvest(app,CurCalYr,E,B,d,2)+LTREP(app,CurCalYr,E,B,d,BIN)*temp
          temp=LTLSUB(E,d)  !111(D) -- not likely to subsidize short lived bulbs, but just in case...
          IF (bulbbinlife(e,bin) .LT. 1.) temp=temp/bulbbinlife(e,bin)
          LTsubsidy(app,CurCalYr,E,B,d,1)=LTsubsidy(app,CurCalYr,E,B,d,1)+LTNEEDED(app,CurCalYr,E,B,d,BIN)*temp
          LTsubsidy(app,CurCalYr,E,B,d,2)=LTsubsidy(app,CurCalYr,E,B,d,2)+LTREP(app,CurCalYr,E,B,d,BIN)*temp
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  !************************************
  !CALCULATE WEIGHTED EFFICIENCY
  !************************************
  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      DO BIN=1,NumAppBins(app)
        WTLEFF(app,CurCalYr,B,d,BIN)=0.0
        DO E=1,NumTypes(app)
          !First calculate total watts for this years purchases and surviving purchases from past years,
          ! then calculate watts per bulb in the next loop
          WTLEFF(app,CurCalYr,B,d,BIN)=WTLEFF(app,CurCalYr,B,d,BIN) &
           +LTrepcons(app,CurCalYr,E,B,d,BIN)/(365.*appbinhours(app,bin)*3.412/10**6)&
           +LTstockexcons(app,CurCalYr,e,B,d,BIN)/(365.*appbinhours(app,bin)*3.412/10**6)
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      DO BIN=1,NumAppBins(app)
        !now divide by the total stock to get average watts per bulb by bin
        WTLEFF(app,CurCalYr,B,d,BIN)=WTLEFF(app,CurCalYr,B,d,BIN)/LTTOTSTOCK(app,CurCalYr,B,d,BIN)
      ENDDO
    ENDDO
  ENDDO

  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      WTLEFFbyAPP(app,CurCalYr,B,d)=0.0
      DO BIN=1,NumAppBins(app)
        WTLEFFbyAPP(app,CurCalYr,B,d)=WTLEFFbyAPP(app,CurCalYr,B,d)+((WTLEFF(app,CurCalYr,B,d,BIN)*LTTOTSTOCK(app,CurCalYr,B,d,BIN))/LTEQP(app,CurCalYr,B,d))
      ENDDO
    ENDDO
  ENDDO

  !**************************
  !LIGHTING UEC
  !**************************
  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      DO BIN=1,NumAppBins(app)
        !This calculation adjusts UECs for efficiency and rebound effect, price elasticity in next loop
        LTNUEC(app,CurCalYr,d,b)= LTNUEC(app,CurCalYr,d,b) &
         + LTBinShare(app,BIN)*LTUEC(app,d,b)*(WTLEFF(app,CurCalYr,b,d,BIN)/basewattbins(app,bin))**(1+alpha)
         ! this might be preferred        + LTBinShare(app,BIN)*LTUEC(app,d,b)*(WTLEFF(app,CurCalYr,b,d,BIN)/WTLEFF(app,RECSYear+1,b,d,BIN))**(1+alpha)
         ! 0613 switch from basewatts     + LTBinShare(app,BIN)*LTUEC(app,d,b)*(WTLEFF(app,CurCalYr,b,d,BIN)/basewattbins(app,bin))**(1+alpha)
         !  this caused an unexpected jump in RECSYear+1 consumption	!TODO - need to investigate further or remove
      ENDDO
    ENDDO
  ENDDO

  !**************************
  !LIGHTING UEC AND CONSUMPTION
  !**************************
  y=CurIYr
  ALPHA=-0.30  !was -0.15 prior to American Recovery and Reinvestment Act of 2009 (ARRA) stimulus; permanently affects price elasticity (but not rebound) based on the smart grid concept

  DO d=1,mNumCR-2
    DO B=1,mNumBldg
      lteqcn(y,app,b,d)=leapyr*lteqp(app,CurCalYr,b,d)*ltnuec(app,CurCalYr,d,b)*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

      !OUTPUTS TO NEMS TABLE 4 (ltcon)
      ltcon(y,d) = ltcon(y,d) + lteqcn(y,app,b,d)
      ltconwt(y,d,b) = ltconwt(y,d,b)+lteqcn(y,app,b,d)

      !OUTPUTS TO NEMS TABLE 31 (ltconwt, ltconin)
      ltconin(y,d,b)= ltconin(y,d,b)+(ltconwt(y,d,b)/RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))/&
       (EH(CurCalYr,B,D)*EXSQRFOOT(CurCalYr,B,D)+HSEADD(CurCalYr,B,D)*SQRFOOT(CurCalYr,B,D)+ &
       SQNEW(CurCalYr-1,B,D)*NH(CurCalYr-1,B,D))
    ENDDO
  ENDDO

  !**************************
  !Lighting diagnostics
  !**************************
  IF (LightDiag.EQ.1) THEN
    ! WRITE(9,*) 'year, app, bin, e, LTLBeta1dr, LTLCAP, bulbbinlife(e,bin), LightBeta2(app,CurCalYr,e), opcost(e) prices, watts,factor, LTHOURS(e), logit calc(e) '

    IF (CurCalYr .EQ. EndYr) THEN
      b=1
      d=1

      WRITE(9,*) ' Printing Lighting Diagnostics for Division ',d,' Building Type ',b
      WRITE(9,*) 'EH EXSQRFOOT'
      WRITE(9,445) bin, e,(EH(y1,1,1)/10.**6,Y1=RECSyear,EndYr)
      WRITE(9,445) bin, e,(EXSQRFOOT(y1,1,1)/10.**3,Y1=RECSyear,EndYr)

      WRITE(9,*) 'HSEADD SQRFOOT'
      WRITE(9,445) bin, e,(HSEADD(y1,1,1)/10.**6,Y1=RECSyear,EndYr)
      WRITE(9,445) bin, e,(SQRFOOT(y1,1,1)/10.**3,Y1=RECSyear,EndYr)

      WRITE(9,*) ' '
      WRITE(9,*) 'LTrep Bins&Type '
      DO BIN=1,NumAppBins(app)
        DO E=1,NumTypes(app)
          !Divide by 10^6 for most detail
          WRITE(9,445)   bin, e,(LTrep(app,y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)
        ENDDO !e
      ENDDO !bin

      WRITE(9,*) ' '
      WRITE(9,*) 'LTRepTot Bins '
      DO BIN=1,NumAppBins(app)
        !Divide by 10^6 for most detail
        WRITE(9,445) bin, e,(LTreptot(app,Y1,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)
      ENDDO !bin

      WRITE(9,*) ' '
      WRITE(9,*) 'LTRepFut Bins&Type '
      DO BIN=1,NumAppBins(app)
        DO E=1,NumTypes(app)
          !Divide by 10^6 for most detail
          WRITE(9,445) bin, e,(LTrepFUT(app,Y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)
        ENDDO !e
      ENDDO !bin

      WRITE(9,*) ' '
      WRITE(9,*) 'LTStockEx Bins&Type '
      DO BIN=1,NumAppBins(app)
        DO E=1,NumTypes(app)
          !Divide by 10^6 for most detail
          WRITE(9,445) bin, e,(LTSTOCKEX(app,Y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)
        ENDDO !e
      ENDDO !bin

      WRITE(9,*) ' '
      WRITE(9,*) 'LTStockExCons Bins&Type '
      DO BIN=1,NumAppBins(app)
        DO E=1,NumTypes(app)
          !Divide by 10^6 for most detail
          WRITE(9,445) bin, e,(LTstockexcons(app,Y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)
        ENDDO !e
      ENDDO !bin

      WRITE(9,*) ' '
      WRITE(9,*) 'LTrepstk Bins&Type '
      DO BIN=1,NumAppBins(app)
        DO E=1,NumTypes(app)
          !Divide by 10^6 for most detail
          WRITE(9,445) bin, e,(LTrepstk(app,Y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)
        ENDDO !e
      ENDDO !bin

      WRITE(9,*) ' '
      WRITE(9,*) 'LTrepCons Bins&Type '
      DO BIN=1,NumAppBins(app)
        DO E=1,NumTypes(app)
          !Divide by 10^6 for most detail
          WRITE(9,445)   bin, e,(LTrepCons(app,y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)
        ENDDO !e
      ENDDO !bin

      WRITE(9,*) ' '
      WRITE(9,*) 'LTneeded Bins&Type '
      DO BIN=1,NumAppBins(app)
        DO E=1,NumTypes(app)
          !Divide by 10^6 for most detail
          WRITE(9,445)  bin, e,(LTneeded(app,y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)
        ENDDO !e
      ENDDO !bin

      WRITE(9,*) ' '
      WRITE(9,*) 'LTStock Bins&Type '
      DO BIN=1,NumAppBins(app)
        DO E=1,NumTypes(app)
          !Divide by 10^6 for most detail
          WRITE(9,445) bin,e,(LTSTOCK(app,Y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)
        ENDDO !e
      ENDDO !bin

      WRITE(9,*) ' '
      WRITE(9,*) 'WTLEFF Bins '
      e=0
      DO BIN=1,NumAppBins(app)
        WRITE(9,445) bin,e,(WTLEFF(app,y1,B,d,BIN),Y1=RECSyear,EndYr)
      ENDDO !bin

      WRITE(9,*) ' '
      WRITE(9,*) 'ltcon/10**9 '
      WRITE(9,444) (ltcon(y1,1)/10**9,Y1=RECSYear-BaseYr+1,LastYr)

      WRITE(9,*) 'ltnuec '
      WRITE(9,444) (LTNUEC(app,y1,1,1),Y1=RECSyear,EndYr)

      WRITE(9,*) 'ltnuec*lteqp/10**9'
      WRITE(9,444) (LTNUEC(app,y1,1,1)*LTEQP(app,y1,1,1)/10**9,Y1=RECSyear,EndYr)

    ENDIF !current year is EndYr

  ENDIF !LightDiag.EQ.1

ENDDO !NumApps

442 FORMAT(a4,i5,2i2,4e11.3)
443 FORMAT(i5,3i2,10e11.3)
444 FORMAT(36(1x,f7.4))
445 FORMAT(2i3,36(1x,f7.3))

END SUBROUTINE LTCNS


!==============================================================================
! MISCELLANEOUS ELECTRIC LOADS (MELs) AND OTHER ELECTRIC APPLIANCE CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE APCNS
IMPLICIT NONE

REAL*4 TVSNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),TVSNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 STBNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),STBNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 HTSNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),HTSNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 OTTNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),OTTNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 VGCNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),VGCNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 DPCNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),DPCNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 LPCNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),LPCNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 MONNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),MONNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 NETNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),NETNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 BATNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),BATNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 CFNNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),CFNNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 COFNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),COFNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 DEHNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),DEHNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 MCONIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),MCONUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 PLPNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),PLPNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 PLHNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),PLHNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 SECNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),SECNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 SPANIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),SPANUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 WCLNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),WCLNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 SPKNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),SPKNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 PHNNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),PHNNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 TABNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),TABNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 KITNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),KITNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
REAL*4 EANIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),EANUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)

REAL*4 TVSCONWT(mNumYr,mNumCR-2,mNumBldg),TVSCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 STBCONWT(mNumYr,mNumCR-2,mNumBldg),STBCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 HTSCONWT(mNumYr,mNumCR-2,mNumBldg),HTSCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 OTTCONWT(mNumYr,mNumCR-2,mNumBldg),OTTCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 VGCCONWT(mNumYr,mNumCR-2,mNumBldg),VGCCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 DPCCONWT(mNumYr,mNumCR-2,mNumBldg),DPCCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 LPCCONWT(mNumYr,mNumCR-2,mNumBldg),LPCCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 MONCONWT(mNumYr,mNumCR-2,mNumBldg),MONCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 NETCONWT(mNumYr,mNumCR-2,mNumBldg),NETCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 BATCONWT(mNumYr,mNumCR-2,mNumBldg),BATCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 CFNCONWT(mNumYr,mNumCR-2,mNumBldg),CFNCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 COFCONWT(mNumYr,mNumCR-2,mNumBldg),COFCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 DEHCONWT(mNumYr,mNumCR-2,mNumBldg),DEHCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 MCOCONWT(mNumYr,mNumCR-2,mNumBldg),MCOCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 PLPCONWT(mNumYr,mNumCR-2,mNumBldg),PLPCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 PLHCONWT(mNumYr,mNumCR-2,mNumBldg),PLHCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 SECCONWT(mNumYr,mNumCR-2,mNumBldg),SECCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 SPACONWT(mNumYr,mNumCR-2,mNumBldg),SPACONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 WCLCONWT(mNumYr,mNumCR-2,mNumBldg),WCLCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 SPKCONWT(mNumYr,mNumCR-2,mNumBldg),SPKCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 PHNCONWT(mNumYr,mNumCR-2,mNumBldg),PHNCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 TABCONWT(mNumYr,mNumCR-2,mNumBldg),TABCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 KITCONWT(mNumYr,mNumCR-2,mNumBldg),KITCONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 EACONWT(mNumYr,mNumCR-2,mNumBldg),EACONIN(mNumYr,mNumCR-2,mNumBldg)
REAL*4 ALPHA,ef1,ef2,ef3,ELOTPEN(RECSYear:EndYr,mNumCR-2)

INTEGER Y,B,D,EUPR,F,Y1,Y3,I

Y=CurCalYr-BaseYr+1

ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15

EUPR=9

!********************************************************************
!MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
!********************************************************************
DO D=1,mNumCR-2
  PRICES(4,D,CurCalYr)=PELRSOUT(D,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
ENDDO

!********************************************************************
!CALCULATE INCOME EFFECT FOR MISCELLANEOUS ELECTRIC LOADS (MELs)
! Assumes that higher disposable income means more annual usage of MELs
!********************************************************************
DO I=1,NumMELs
  DO D=1,mNumCR-2
    IF (MELsIncomeEffect(I).LT.1) THEN
      INCOME(D,CurCalYr,I)=1.
    ELSE
      INCOME(D,CurCalYr,I)=(MC_YPDR(D,CurIYr)/MC_YPDR(D,RECSYear-BaseYr+1))**.05
    ENDIF
  ENDDO
ENDDO

!********************************************************************
!SET PENETRATION RATES FOR OTHER ELECTRIC/ UNSPECIFIED MISCELLANEOUS ELECTRIC LOADS (MELs)
! Penetration of electric other changes at the rate of disposable income per person over age 16
!*******************************************************************
DO D=1,mNumCR-2
  ELOTPEN(RECSYear,D)=1.
  ELOTPEN(CurCalYr,D)=ELOTPEN(RECSYear,D)*((MC_YPDR(D,Y)/MC_NP16A(D,Y))/(MC_YPDR(D,RECSYear-BaseYr+1)/MC_NP16A(D,RECSYear-BaseYr+1)))
ENDDO

!********************************************************************
!MELs Unit Energy Consumption (UEC)
!********************************************************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    TVSNUEC(CurCalYr,D,B)=  TVSUEC(D,B)*TVSEFF(CurCalYr)*INCOME(D,CurCalYr,1)
    TVSNIUEC(CurCalYr,D,B)= TVSUEC(D,B)*TVSEFF(CurCalYr)
    STBNUEC(CurCalYr,D,B)=  STBUEC(D,B)*STBEFF(CurCalYr)*INCOME(D,CurCalYr,2)
    STBNIUEC(CurCalYr,D,B)= STBUEC(D,B)*STBEFF(CurCalYr)
    HTSNUEC(CurCalYr,D,B)=  HTSUEC(D,B)*HTSEFF(CurCalYr)*INCOME(D,CurCalYr,3)
    HTSNIUEC(CurCalYr,D,B)= HTSUEC(D,B)*HTSEFF(CurCalYr)
    OTTNUEC(CurCalYr,D,B)=  OTTUEC(D,B)*OTTEFF(CurCalYr)*INCOME(D,CurCalYr,4)
    OTTNIUEC(CurCalYr,D,B)= OTTUEC(D,B)*OTTEFF(CurCalYr)
    VGCNUEC(CurCalYr,D,B)=  VGCUEC(D,B)*VGCEFF(CurCalYr)*INCOME(D,CurCalYr,5)
    VGCNIUEC(CurCalYr,D,B)= VGCUEC(D,B)*VGCEFF(CurCalYr)
    DPCNUEC(CurCalYr,D,B)=  DPCUEC(D,B)*DPCEFF(CurCalYr)*INCOME(D,CurCalYr,6)
    DPCNIUEC(CurCalYr,D,B)= DPCUEC(D,B)*DPCEFF(CurCalYr)
    LPCNUEC(CurCalYr,D,B)=  LPCUEC(D,B)*LPCEFF(CurCalYr)*INCOME(D,CurCalYr,7)
    LPCNIUEC(CurCalYr,D,B)= LPCUEC(D,B)*LPCEFF(CurCalYr)
    MONNUEC(CurCalYr,D,B)=  MONUEC(D,B)*MONEFF(CurCalYr)*INCOME(D,CurCalYr,8)
    MONNIUEC(CurCalYr,D,B)= MONUEC(D,B)*MONEFF(CurCalYr)
    NETNUEC(CurCalYr,D,B)=  NETUEC(D,B)*NETEFF(CurCalYr)*INCOME(D,CurCalYr,9)
    NETNIUEC(CurCalYr,D,B)= NETUEC(D,B)*NETEFF(CurCalYr)
    BATNUEC(CurCalYr,D,B)=  BATUEC(D,B)*BATEFF(CurCalYr)*INCOME(D,CurCalYr,10)
    BATNIUEC(CurCalYr,D,B)= BATUEC(D,B)*BATEFF(CurCalYr)
    CFNNUEC(CurCalYr,D,B)=  CFNUEC(D,B)*CFNEFF(CurCalYr)*(CDDADJ(CurCalYr,D)/CDDADJ(RECSYear,D))**(2.0)*INCOME(D,CurCalYr,11)	!TODO - only including for consistency; need to differentiate ceiling fans from other income-affected MELs
    CFNNIUEC(CurCalYr,D,B)= CFNUEC(D,B)*CFNEFF(CurCalYr)
    COFNUEC(CurCalYr,D,B)=  COFUEC(D,B)*COFEFF(CurCalYr)*INCOME(D,CurCalYr,12)
    COFNIUEC(CurCalYr,D,B)= COFUEC(D,B)*COFEFF(CurCalYr)
    DEHNUEC(CurCalYr,D,B)=  DEHUEC(D,B)*DEHEFF(CurCalYr)*INCOME(D,CurCalYr,13)
    DEHNIUEC(CurCalYr,D,B)= DEHUEC(D,B)*DEHEFF(CurCalYr)
    MCONUEC(CurCalYr,D,B)=  MCOUEC(D,B)*MCOEFF(CurCalYr)*INCOME(D,CurCalYr,14)
    MCONIUEC(CurCalYr,D,B)= MCOUEC(D,B)*MCOEFF(CurCalYr)
    PLPNUEC(CurCalYr,D,B)=  PLPUEC(D,B)*PLPEFF(CurCalYr)*INCOME(D,CurCalYr,15)
    PLPNIUEC(CurCalYr,D,B)= PLPUEC(D,B)*PLPEFF(CurCalYr)
    PLHNUEC(CurCalYr,D,B)=  PLHUEC(D,B)*PLHEFF(CurCalYr)*INCOME(D,CurCalYr,16)
    PLHNIUEC(CurCalYr,D,B)= PLHUEC(D,B)*PLHEFF(CurCalYr)
    SECNUEC(CurCalYr,D,B)=  SECUEC(D,B)*SECEFF(CurCalYr)*INCOME(D,CurCalYr,17)
    SECNIUEC(CurCalYr,D,B)= SECUEC(D,B)*SECEFF(CurCalYr)
    SPANUEC(CurCalYr,D,B)=  SPAUEC(D,B)*SPAEFF(CurCalYr)*INCOME(D,CurCalYr,18)
    SPANIUEC(CurCalYr,D,B)= SPAUEC(D,B)*SPAEFF(CurCalYr)
    WCLNUEC(CurCalYr,D,B)=  WCLUEC(D,B)*WCLEFF(CurCalYr)*INCOME(D,CurCalYr,19)
    WCLNIUEC(CurCalYr,D,B)= WCLUEC(D,B)*WCLEFF(CurCalYr)
    SPKNUEC(CurCalYr,D,B)=  SPKUEC(D,B)*SPKEFF(CurCalYr)*INCOME(D,CurCalYr,20)
    SPKNIUEC(CurCalYr,D,B)= SPKUEC(D,B)*SPKEFF(CurCalYr)
    PHNNUEC(CurCalYr,D,B)=  PHNUEC(D,B)*PHNEFF(CurCalYr)*INCOME(D,CurCalYr,21)
    PHNNIUEC(CurCalYr,D,B)= PHNUEC(D,B)*PHNEFF(CurCalYr)
    TABNUEC(CurCalYr,D,B)=  TABUEC(D,B)*TABEFF(CurCalYr)*INCOME(D,CurCalYr,22)
    TABNIUEC(CurCalYr,D,B)= TABUEC(D,B)*TABEFF(CurCalYr)
    KITNUEC(CurCalYr,D,B)=  KITUEC(D,B)*KITEFF(CurCalYr)*INCOME(D,CurCalYr,23)
    KITNIUEC(CurCalYr,D,B)= KITUEC(D,B)*KITEFF(CurCalYr)

    EANUEC(CurCalYr,D,B)=  EAUEC(D,B)* ELOTPEN(CurCalYr,D)
    EANIUEC(CurCalYr,D,B)= EAUEC(D,B)* ELOTPEN(CurCalYr,D)
  ENDDO
ENDDO

!****************************
!MELs Equipment Stocks
!****************************
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    TVSEQP(CurCalYr,B,D)=((TVSEQP(RECSYear,B,D)/EH(RECSYear,B,D))*TVSPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    STBEQP(CurCalYr,B,D)=((STBEQP(RECSYear,B,D)/EH(RECSYear,B,D))*STBPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    HTSEQP(CurCalYr,B,D)=((HTSEQP(RECSYear,B,D)/EH(RECSYear,B,D))*HTSPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    OTTEQP(CurCalYr,B,D)=((OTTEQP(RECSYear,B,D)/EH(RECSYear,B,D))*OTTPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    VGCEQP(CurCalYr,B,D)=((VGCEQP(RECSYear,B,D)/EH(RECSYear,B,D))*VGCPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    DPCEQP(CurCalYr,B,D)=((DPCEQP(RECSYear,B,D)/EH(RECSYear,B,D))*DPCPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    LPCEQP(CurCalYr,B,D)=((LPCEQP(RECSYear,B,D)/EH(RECSYear,B,D))*LPCPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    MONEQP(CurCalYr,B,D)=((MONEQP(RECSYear,B,D)/EH(RECSYear,B,D))*MONPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    NETEQP(CurCalYr,B,D)=((NETEQP(RECSYear,B,D)/EH(RECSYear,B,D))*NETPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    BATEQP(CurCalYr,B,D)=((BATEQP(RECSYear,B,D)/EH(RECSYear,B,D))*BATPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    CFNEQP(CurCalYr,B,D)=((CFNEQP(RECSYear,B,D)/EH(RECSYear,B,D))*CFNPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    COFEQP(CurCalYr,B,D)=((COFEQP(RECSYear,B,D)/EH(RECSYear,B,D))*COFPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    DEHEQP(CurCalYr,B,D)=((DEHEQP(RECSYear,B,D)/EH(RECSYear,B,D))*DEHPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    MCOEQP(CurCalYr,B,D)=((MCOEQP(RECSYear,B,D)/EH(RECSYear,B,D))*MCOPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    PLPEQP(CurCalYr,B,D)=((PLPEQP(RECSYear,B,D)/EH(RECSYear,B,D))*PLPPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    PLHEQP(CurCalYr,B,D)=((PLHEQP(RECSYear,B,D)/EH(RECSYear,B,D))*PLHPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    SECEQP(CurCalYr,B,D)=((SECEQP(RECSYear,B,D)/EH(RECSYear,B,D))*SECPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    SPAEQP(CurCalYr,B,D)=((SPAEQP(RECSYear,B,D)/EH(RECSYear,B,D))*SPAPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    WCLEQP(CurCalYr,B,D)=((WCLEQP(RECSYear,B,D)/EH(RECSYear,B,D))*WCLPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    SPKEQP(CurCalYr,B,D)=((SPKEQP(RECSYear,B,D)/EH(RECSYear,B,D))*SPKPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    PHNEQP(CurCalYr,B,D)=((PHNEQP(RECSYear,B,D)/EH(RECSYear,B,D))*PHNPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    TABEQP(CurCalYr,B,D)=((TABEQP(RECSYear,B,D)/EH(RECSYear,B,D))*TABPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    KITEQP(CurCalYr,B,D)=((KITEQP(RECSYear,B,D)/EH(RECSYear,B,D))*KITPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
    EAEQP(CurCalYr,B,D)= EAEQP(RECSYear,B,D)+NH(CurCalYr,B,D)
  ENDDO
ENDDO

!********************************************************************
! CALCULATE CONSUMPTION FOR MISCELLANEOUS ELECTRIC LOADS (MELs) AND OTHER ELECTRIC APPLIANCES
!********************************************************************
ALPHA=-0.30  !was -0.15 prior to American Recovery and Reinvestment Act of 2009 (ARRA) stimulus; permanently affects price elasticity (but not rebound) based on the smart grid concept

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    !TELEVISIONS (TVS)
    TVSCONWT(Y,D,B)=LEAPYR*(TVSEQP(CurCalYr,B,D)*TVSNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    TVSCONIN(Y,D,B)= 0.
    IF (TVSEQP(CurCalYr,B,D).GT.0.) TVSCONIN(Y,D,B)=(TVSEQP(CurCalYr,B,D)*TVSNIUEC(CurCalYr,D,B)) / TVSEQP(CurCalYr,B,D)
    TVSEQCN(Y,1,B,D)=LEAPYR*(TVSEQP(CurCalYr,B,D)*TVSNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !SET-TOP BOXES (STB)
    STBCONWT(Y,D,B)=LEAPYR*(STBEQP(CurCalYr,B,D)*STBNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    STBCONIN(Y,D,B)= 0.
    IF (STBEQP(CurCalYr,B,D).GT.0.) STBCONIN(Y,D,B)=(STBEQP(CurCalYr,B,D)*STBNIUEC(CurCalYr,D,B)) / STBEQP(CurCalYr,B,D)
    STBEQCN(Y,1,B,D)=LEAPYR*(STBEQP(CurCalYr,B,D)*STBNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !HOME THEATER SYSTEMS (HTS)
    HTSCONWT(Y,D,B)=LEAPYR*(HTSEQP(CurCalYr,B,D)*HTSNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    HTSCONIN(Y,D,B)= 0.
    IF (HTSEQP(CurCalYr,B,D).GT.0.) HTSCONIN(Y,D,B)=(HTSEQP(CurCalYr,B,D)*HTSNIUEC(CurCalYr,D,B)) / HTSEQP(CurCalYr,B,D)
    HTSEQCN(Y,1,B,D)=LEAPYR*(HTSEQP(CurCalYr,B,D)*HTSNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !OVER-THE-TOP STREAMING DEVICES (OTT)
    OTTCONWT(Y,D,B)=LEAPYR*(OTTEQP(CurCalYr,B,D)*OTTNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    OTTCONIN(Y,D,B)= 0.
    IF (OTTEQP(CurCalYr,B,D).GT.0.) OTTCONIN(Y,D,B)=(OTTEQP(CurCalYr,B,D)*OTTNIUEC(CurCalYr,D,B)) / OTTEQP(CurCalYr,B,D)
    OTTEQCN(Y,1,B,D)=LEAPYR*(OTTEQP(CurCalYr,B,D)*OTTNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !VIDEO GAME CONSOLES (VGC)
    VGCCONWT(Y,D,B)=LEAPYR*(VGCEQP(CurCalYr,B,D)*VGCNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    VGCCONIN(Y,D,B)= 0.
    IF (VGCEQP(CurCalYr,B,D).GT.0.) VGCCONIN(Y,D,B)=(VGCEQP(CurCalYr,B,D)*VGCNIUEC(CurCalYr,D,B)) / VGCEQP(CurCalYr,B,D)
    VGCEQCN(Y,1,B,D)=LEAPYR*(VGCEQP(CurCalYr,B,D)*VGCNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !DESKTOP PCS (DPC)
    DPCCONWT(Y,D,B)=LEAPYR*(DPCEQP(CurCalYr,B,D)*DPCNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    DPCCONIN(Y,D,B)= 0.
    IF (DPCEQP(CurCalYr,B,D).GT.0.) DPCCONIN(Y,D,B)=(DPCEQP(CurCalYr,B,D)*DPCNIUEC(CurCalYr,D,B)) / DPCEQP(CurCalYr,B,D)
    DPCEQCN(Y,1,B,D)=LEAPYR*(DPCEQP(CurCalYr,B,D)*DPCNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !LAPTOP PCS (LPC)
    LPCCONWT(Y,D,B)=LEAPYR*(LPCEQP(CurCalYr,B,D)*LPCNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    LPCCONIN(Y,D,B)= 0.
    IF (LPCEQP(CurCalYr,B,D).GT.0.) LPCCONIN(Y,D,B)=(LPCEQP(CurCalYr,B,D)*LPCNIUEC(CurCalYr,D,B)) / LPCEQP(CurCalYr,B,D)
    LPCEQCN(Y,1,B,D)=LEAPYR*(LPCEQP(CurCalYr,B,D)*LPCNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !MONITORS (MON)
    MONCONWT(Y,D,B)=LEAPYR*(MONEQP(CurCalYr,B,D)*MONNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    MONCONIN(Y,D,B)= 0.
    IF (MONEQP(CurCalYr,B,D).GT.0.) MONCONIN(Y,D,B)=(MONEQP(CurCalYr,B,D)*MONNIUEC(CurCalYr,D,B)) / MONEQP(CurCalYr,B,D)
    MONEQCN(Y,1,B,D)=LEAPYR*(MONEQP(CurCalYr,B,D)*MONNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !NETWORKING EQUIPMENT (NET)
    NETCONWT(Y,D,B)=LEAPYR*(NETEQP(CurCalYr,B,D)*NETNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    NETCONIN(Y,D,B)= 0.
    IF (NETEQP(CurCalYr,B,D).GT.0.) NETCONIN(Y,D,B)=(NETEQP(CurCalYr,B,D)*NETNIUEC(CurCalYr,D,B)) / NETEQP(CurCalYr,B,D)
    NETEQCN(Y,1,B,D)=LEAPYR*(NETEQP(CurCalYr,B,D)*NETNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !NON-PC RECHARGEABLES (BAT)
    BATCONWT(Y,D,B)=LEAPYR*(BATEQP(CurCalYr,B,D)*BATNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    BATCONIN(Y,D,B)= 0.
    IF (BATEQP(CurCalYr,B,D).GT.0.) BATCONIN(Y,D,B)=(BATEQP(CurCalYr,B,D)*BATNIUEC(CurCalYr,D,B)) / BATEQP(CurCalYr,B,D)
    BATEQCN(Y,1,B,D)=LEAPYR*(BATEQP(CurCalYr,B,D)*BATNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !CEILING FANS (CFN)
    CFNCONWT(Y,D,B)=LEAPYR*(CFNEQP(CurCalYr,B,D)*CFNNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    CFNCONIN(Y,D,B)= 0.
    IF (CFNEQP(CurCalYr,B,D).GT.0.) CFNCONIN(Y,D,B)=(CFNEQP(CurCalYr,B,D)*CFNNIUEC(CurCalYr,D,B)) / CFNEQP(CurCalYr,B,D)
    CFNEQCN(Y,1,B,D)=LEAPYR*(CFNEQP(CurCalYr,B,D)*CFNNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !COFFEE MACHINES (COF)
    COFCONWT(Y,D,B)=LEAPYR*(COFEQP(CurCalYr,B,D)*COFNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    COFCONIN(Y,D,B)= 0.
    IF (COFEQP(CurCalYr,B,D).GT.0.) COFCONIN(Y,D,B)=(COFEQP(CurCalYr,B,D)*COFNIUEC(CurCalYr,D,B)) / COFEQP(CurCalYr,B,D)
    COFEQCN(Y,1,B,D)=LEAPYR*(COFEQP(CurCalYr,B,D)*COFNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !DEHUMIDIFIERS (DEH)
    DEHCONWT(Y,D,B)=LEAPYR*(DEHEQP(CurCalYr,B,D)*DEHNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    DEHCONIN(Y,D,B)= 0.
    IF (DEHEQP(CurCalYr,B,D).GT.0.) DEHCONIN(Y,D,B)=(DEHEQP(CurCalYr,B,D)*DEHNIUEC(CurCalYr,D,B)) / DEHEQP(CurCalYr,B,D)
    DEHEQCN(Y,1,B,D)=LEAPYR*(DEHEQP(CurCalYr,B,D)*DEHNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !MICROWAVE OVENS (MCO)
    MCOCONWT(Y,D,B)=LEAPYR*(MCOEQP(CurCalYr,B,D)*MCONUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    MCOCONIN(Y,D,B)= 0.
    IF (MCOEQP(CurCalYr,B,D).GT.0.) MCOCONIN(Y,D,B)=(MCOEQP(CurCalYr,B,D)*MCONIUEC(CurCalYr,D,B)) / MCOEQP(CurCalYr,B,D)
    MCOEQCN(Y,1,B,D)=LEAPYR*(MCOEQP(CurCalYr,B,D)*MCONUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !POOL PUMPS (PLP)
    PLPCONWT(Y,D,B)=LEAPYR*(PLPEQP(CurCalYr,B,D)*PLPNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    PLPCONIN(Y,D,B)= 0.
    IF (PLPEQP(CurCalYr,B,D).GT.0.) PLPCONIN(Y,D,B)=(PLPEQP(CurCalYr,B,D)*PLPNIUEC(CurCalYr,D,B)) / PLPEQP(CurCalYr,B,D)
    PLPEQCN(Y,1,B,D)=LEAPYR*(PLPEQP(CurCalYr,B,D)*PLPNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !POOL HEATERS (PLH)
    PLHCONWT(Y,D,B)=LEAPYR*(PLHEQP(CurCalYr,B,D)*PLHNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    PLHCONIN(Y,D,B)= 0.
    IF (PLHEQP(CurCalYr,B,D).GT.0.) PLHCONIN(Y,D,B)=(PLHEQP(CurCalYr,B,D)*PLHNIUEC(CurCalYr,D,B)) / PLHEQP(CurCalYr,B,D)
    PLHEQCN(Y,1,B,D)=LEAPYR*(PLHEQP(CurCalYr,B,D)*PLHNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !SECURITY SYSTEMS (SEC)
    SECCONWT(Y,D,B)=LEAPYR*(SECEQP(CurCalYr,B,D)*SECNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    SECCONIN(Y,D,B)= 0.
    IF (SECEQP(CurCalYr,B,D).GT.0.) SECCONIN(Y,D,B)=(SECEQP(CurCalYr,B,D)*SECNIUEC(CurCalYr,D,B)) / SECEQP(CurCalYr,B,D)
    SECEQCN(Y,1,B,D)=LEAPYR*(SECEQP(CurCalYr,B,D)*SECNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !SPAS (SPA)
    SPACONWT(Y,D,B)=LEAPYR*(SPAEQP(CurCalYr,B,D)*SPANUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    SPACONIN(Y,D,B)= 0.
    IF (SPAEQP(CurCalYr,B,D).GT.0.) SPACONIN(Y,D,B)=(SPAEQP(CurCalYr,B,D)*SPANIUEC(CurCalYr,D,B)) / SPAEQP(CurCalYr,B,D)
    SPAEQCN(Y,1,B,D)=LEAPYR*(SPAEQP(CurCalYr,B,D)*SPANUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !WINE COOLERS (WCL)
    WCLCONWT(Y,D,B)=LEAPYR*(WCLEQP(CurCalYr,B,D)*WCLNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    WCLCONIN(Y,D,B)= 0.
    IF (WCLEQP(CurCalYr,B,D).GT.0.) WCLCONIN(Y,D,B)=(WCLEQP(CurCalYr,B,D)*WCLNIUEC(CurCalYr,D,B)) / WCLEQP(CurCalYr,B,D)
    WCLEQCN(Y,1,B,D)=LEAPYR*(WCLEQP(CurCalYr,B,D)*WCLNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !SMART SPEAKERS (SPK)
    SPKCONWT(Y,D,B)=LEAPYR*(SPKEQP(CurCalYr,B,D)*SPKNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    SPKCONIN(Y,D,B)= 0.
    IF (SPKEQP(CurCalYr,B,D).GT.0.) SPKCONIN(Y,D,B)=(SPKEQP(CurCalYr,B,D)*SPKNIUEC(CurCalYr,D,B)) / SPKEQP(CurCalYr,B,D)
    SPKEQCN(Y,1,B,D)=LEAPYR*(SPKEQP(CurCalYr,B,D)*SPKNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !SMARTPHONES (PHN)
    PHNCONWT(Y,D,B)=LEAPYR*(PHNEQP(CurCalYr,B,D)*PHNNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    PHNCONIN(Y,D,B)= 0.
    IF (PHNEQP(CurCalYr,B,D).GT.0.) PHNCONIN(Y,D,B)=(PHNEQP(CurCalYr,B,D)*PHNNIUEC(CurCalYr,D,B)) / PHNEQP(CurCalYr,B,D)
    PHNEQCN(Y,1,B,D)=LEAPYR*(PHNEQP(CurCalYr,B,D)*PHNNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !TABLETS (TAB)
    TABCONWT(Y,D,B)=LEAPYR*(TABEQP(CurCalYr,B,D)*TABNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    TABCONIN(Y,D,B)= 0.
    IF (TABEQP(CurCalYr,B,D).GT.0.) TABCONIN(Y,D,B)=(TABEQP(CurCalYr,B,D)*TABNIUEC(CurCalYr,D,B)) / TABEQP(CurCalYr,B,D)
    TABEQCN(Y,1,B,D)=LEAPYR*(TABEQP(CurCalYr,B,D)*TABNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !SMALL KITCHEN APPLIANCES (KIT)
    KITCONWT(Y,D,B)=LEAPYR*(KITEQP(CurCalYr,B,D)*KITNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    KITCONIN(Y,D,B)= 0.
    IF (KITEQP(CurCalYr,B,D).GT.0.) KITCONIN(Y,D,B)=(KITEQP(CurCalYr,B,D)*KITNIUEC(CurCalYr,D,B)) / KITEQP(CurCalYr,B,D)
    KITEQCN(Y,1,B,D)=LEAPYR*(KITEQP(CurCalYr,B,D)*KITNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    !ELECTRIC OTHER APPLIANCES (EOA)
    EACONWT(y,d,b)=LEAPYR*(EAEQP(CurCalYr,B,D)*EANUEC(CurCalYr,d,b))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    EACONIN(y,d,b)=0.
    IF (EAEQP(CurCalYr,B,D).GT.0.) EACONIN(y,d,b)=(EAEQP(CurCalYr,B,D)*EANIUEC(CurCalYr,d,b)) / EAEQP(CurCalYr,B,D)
    EAEQCN(y,1,b,d)=LEAPYR*(EAEQP(CurCalYr,b,d)*EAnuec(CurCalYr,d,b))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
  ENDDO !B
ENDDO !D

!********************************************************************
!Aggregate consumption for TV-related, PC-related, and all other appliances/MELs
!********************************************************************
DO D=1,mNumCR-2
  TVRCON(Y,D)=0.0
  PCRCON(Y,D)=0.0
  APCON(Y,D)=0.0
  DO B=1,mNumBldg
    TVRCON(Y,D)=TVRCON(Y,D)+TVSEQCN(Y,1,B,D)+STBEQCN(Y,1,B,D)+HTSEQCN(Y,1,B,D)+OTTEQCN(Y,1,B,D)+VGCEQCN(Y,1,B,D)
    TVRCONWT(Y,D,b)=TVSCONWT(Y,D,B)+STBCONWT(Y,D,B)+HTSCONWT(Y,D,B)+OTTCONWT(Y,D,B)+VGCCONWT(Y,D,B)
    PCRCON(Y,D)=PCRCON(Y,D)+ DPCEQCN(Y,1,B,D)+LPCEQCN(Y,1,B,D)+MONEQCN(Y,1,B,D)+NETEQCN(Y,1,B,D)
    PCRCONWT(Y,D,b)=DPCCONWT(Y,D,B)+LPCCONWT(Y,D,B)+MONCONWT(Y,D,B)+NETCONWT(Y,D,B)
    APCON(Y,D)=APCON(Y,D)+ BATEQCN(Y,1,B,D)+CFNEQCN(Y,1,B,D)+COFEQCN(Y,1,B,D)+DEHEQCN(Y,1,B,D)+ &
     MCOEQCN(Y,1,B,D)+PLPEQCN(Y,1,B,D)+SECEQCN(Y,1,B,D)+SPAEQCN(Y,1,B,D)+ &
     WCLEQCN(Y,1,B,D)+PLHEQCN(Y,1,B,D)+EAEQCN(Y,1,B,D)+ &
     SPKEQCN(Y,1,B,D)+PHNEQCN(Y,1,B,D)+TABEQCN(Y,1,B,D)+KITEQCN(Y,1,B,D)
    APCONWT(Y,D,b)=BATCONWT(Y,D,B)+CFNCONWT(Y,D,B)+COFCONWT(Y,D,B)+DEHCONWT(Y,D,B)+ &
     MCOCONWT(Y,D,B)+PLPCONWT(Y,D,B)+SECCONWT(Y,D,B)+SPACONWT(Y,D,B)+ &
     WCLCONWT(Y,D,B)+PLHCONWT(Y,D,B)+EACONWT(Y,D,B)+ &
     SPKCONWT(Y,D,B)+PHNCONWT(Y,D,B)+TABCONWT(Y,D,B)+KITCONWT(Y,D,B)
  ENDDO
ENDDO

!Compute other electric appliance efficiency based on weighted average equipment intensities
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    TVRCONIN(Y,D,b)=(TVSCONIN(Y,D,B)*tvsconwt(y,d,b)+STBCONIN(Y,D,B)*stbconwt(y,d,b)+HTSCONIN(Y,D,B)*htsconwt(y,d,b) + &
     OTTCONIN(Y,D,B)*OTTconwt(y,d,b)+VGCCONIN(Y,D,B)*vgcconwt(y,d,b)) / tvrconwt(y,d,b)
    PCRCONIN(Y,D,b)=(DPCCONIN(Y,D,B)*dpcconwt(y,d,b)+LPCCONIN(Y,D,B)*lpcconwt(y,d,b)+MONCONIN(Y,D,B)*monconwt(y,d,b) + &
     NETCONIN(Y,D,B)*netconwt(y,d,b)) / pcrconwt(y,d,b)
    APCONIN(Y,D,b)= (BATCONIN(Y,D,B)*batconwt(y,d,b)+CFNCONIN(Y,D,B)*cfnconwt(y,d,b)+COFCONIN(Y,D,B)*cofconwt(y,d,b) + &
     DEHCONIN(Y,D,B)*dehconwt(y,d,b)+MCOCONIN(Y,D,B)*mcoconwt(y,d,b)+PLPCONIN(Y,D,B)*PLPconwt(y,d,b) + &
     SECCONIN(Y,D,B)*secconwt(y,d,b)+SPACONIN(Y,D,B)*spaconwt(y,d,b)+WCLCONIN(Y,D,B)*wclconwt(y,d,b) + &
     SPKCONIN(Y,D,B)*SPKconwt(y,d,b)+PHNCONIN(Y,D,B)*PHNconwt(y,d,b)+TABCONIN(Y,D,B)*TABconwt(y,d,b)+KITCONIN(Y,D,B)*KITconwt(y,d,b) + &
     EACONIN(Y,D,B)*eaconwt(y,d,b)+PLHCONIN(Y,D,B)*PLHconwt(y,d,b)) / apconwt(y,d,b)
  ENDDO
ENDDO

END SUBROUTINE APCNS


!==============================================================================
! SECONDARY HEATING CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE SHTCNS
IMPLICIT NONE

REAL*4 ALPHA,ef1,ef2,ef3
REAL*4 HDDFACT(mNumCR), SHTEFF
INTEGER Y, B, D, F, F2, FShell, EUPR

Y=CurCalYr-BaseYr+1
ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
EUPR=10

!INITIALIZE	!TODO - replace 5 with NSHTRFL? Would need to be declared in different module/subroutine
SHTCON(Y, 1:5, 1:mNumCR-2) = 0.0
SHEQCN(Y, 1:5, 1:mNumBldg, 1:mNumCR-2) = 0.0
SHTCONIN(Y, 1:5, 1:mNumCR-2, 1:mNumBldg) = 0.0
SHTCONWT(Y, 1:5, 1:mNumCR-2, 1:mNumBldg) = 0.0

!COMPUTE HDDFACT
DO D=1,mNumCR-2
  HDDFACT(D)=(HDDADJ(CurCalYr,D)/HDDADJ(RECSYear,D))
ENDDO

!Begin Looping for Secondary Heating Calculations
DO F=1,5  !NoKero	!TODO - replace 5 with NSHTRFL? would need to be declared in different module/subroutine
  !Fuels are matched with prices below for the RSELAST function calls
  ! F is the secondary heating numbering scheme
  ! "F" -> FCON: 1=Natural Gas, 2=Electricity, 3=Distillate Fuel Oil, 4=Propane, 5=Wood  !NoKero
  ! F2 is the fuel for the elasticity calculation:
  ! Prices "F2" -> RTFUEL: 1=Distillate Fuel Oil 2=Propane 3=Natural Gas 4=Electricity 5=Wood  !NoKero

  IF (F.EQ.4) THEN
    ALPHA=-0.30  !was -0.15 prior to American Recovery and Reinvestment Act of 2009 (ARRA) stimulus; permanently affects price elasticity (but not rebound) based on the smart grid concept
  ELSE
    ALPHA=-0.15
  ENDIF

  FShell=F  !FShell is the heating shell to use, none for wood, so set wood below  !NoKero
  IF (F.EQ.1) F2=3 !natural gas
  IF (F.EQ.2) F2=4 !electricity
  IF (F.EQ.3) F2=1 !distillate fuel oil
  IF (F.EQ.4) F2=2 !propane
  IF (F.EQ.5) THEN !wood, priced to distillate fuel oil  !NoKero
    F2=1
    FShell=3 !There is no shell for wood, so use distillate fuel oil	!TODO - RSHTSHR has wood in multifamily and mobile home new construction; none in single-family
    ALPHA=0.50 !No ARRA effect for wood; has positive elasticity with respect to distillate fuel oil
  ENDIF

  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      SHTEQP(CurCalYr,B,D,F)=(SHTSHR(B,D,F)*EH(CurCalYr,B,D)+NSHTSHR(B,D,F)*NH(CurCalYr,B,D))

      SHTCON(Y,F,D)=SHTCON(Y,F,D)+LEAPYR*(SHTSHR(B,D,F)* &
       EH(CurCalYr,B,D)*SHTUEC(D,F,B)* &
       HDDFACT(D)*(EHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)) + &
       NSHTSHR(B,D,F)*NH(CurCalYr,B,D)*SHTUEC(D,F,B)*HDDFACT(D)* &
       (AHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)))* &
       RSELAST(F2,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

      SHTCONWT(Y,F,D,B)=SHTCONWT(Y,F,D,B)+LEAPYR*(SHTSHR(B,D,F)* &
       EH(CurCalYr,B,D)*SHTUEC(D,F,B)* &
       HDDFACT(D)*(EHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)) + &
       NSHTSHR(B,D,F)*NH(CurCalYr,B,D)*SHTUEC(D,F,B)*HDDFACT(D)* &
       (AHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)))* &
       RSELAST(F2,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

      IF ((EH(CurCalYr,B,D)+NH(CurCalYr,B,D)).GT.0. .AND. ehshell(RECSYear,fshell,d,b).GT.0. .AND. &
       SHTSHR(B,D,F).GT.0. .AND. ahshell(CurCalYr,fshell,d,b).GT.0. .AND. NSHTSHR(B,D,F).GT.0. ) THEN
        SHTCONIN(Y,F,D,B)=SHTCONIN(Y,F,D,B)+( (SHTSHR(B,D,F)* &
        EH(CurCalYr,B,D)*SHTUEC(D,F,B)* &
        (EHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)) + &
        NSHTSHR(B,D,F)*NH(CurCalYr,B,D)*SHTUEC(D,F,B)*HDDFACT(D)* &
        (AHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B))) )&
        / (EH(CurCalYr,B,D)+NH(CurCalYr,B,D))
      ENDIF

      SHEQCN(Y,F,B,D)=LEAPYR*(SHTSHR(B,D,F)* &
       EH(CurCalYr,B,D)*SHTUEC(D,F,B)* &
       HDDFACT(D)*(EHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)) + &
       NSHTSHR(B,D,F)*NH(CurCalYr,B,D)*SHTUEC(D,F,B)*HDDFACT(D)* &
       (AHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)))* &
       RSELAST(F2,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

    ENDDO !B
  ENDDO !D
ENDDO !F

END SUBROUTINE SHTCNS


!==============================================================================
! UNSPECIFIED/OTHER FUEL APPLIANCE CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE APPCNS
IMPLICIT NONE

REAL*4 ALPHA,ef1,ef2,ef3,LPGGRILL(RECSYear:EndYr)
INTEGER Y, B, D,F,F1,Y1,EUPR

Y=CurCalYr-BaseYr+1
ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
EUPR=10

LPGGRILL(RECSYear)=0.34  !share of homes with propane grills in the RECS year
LPGGRILL(ijumpcalyr)=0.60  !share of homes with propane grills by end of projection	!TODO - verify source/ revise assumption?

IF (CurCalYr.EQ.(RECSYear+1)) THEN
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      DO F=1,3	!TODO - replace 3 with parameter?
        APLEQP(RECSYear,B,D,F)=APPEQP(RECSYear,B,D,F)
      ENDDO
    ENDDO
  ENDDO
ENDIF

IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
  DO Y1=RECSYear+1,ijumpcalyr
    LPGGRILL(Y1)=LPGGRILL(Y1-1)+((LPGGRILL(ijumpcalyr)-LPGGRILL(RECSYear))/(ijumpcalyr-RECSYear))  !calculates average annual penetration rate of propane grills into residential households
  ENDDO
ENDIF

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO F=1,3	!TODO - replace 3 with parameter?
      IF (F.EQ.2) THEN !propane
        APLEQP(CurCalYr,B,D,F)=((APPEQP(RECSYear,B,D,F)/EH(RECSYear,B,D))*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D))&
         *(LPGGRILL(CurCalYr)/LPGGRILL(RECSYear)) )
      ELSE
        APLEQP(CurCalYr,B,D,F)=((APPEQP(RECSYear,B,D,F)/EH(RECSYear,B,D))*EH(CurCalYr,B,D))
      ENDIF
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO F=1,3	!TODO - replace 3 with parameter
    IF (F.EQ.1) F1=3
    IF (F.EQ.2) F1=2
    IF (F.EQ.3) F1=1
    APLCON(Y,F,D)=0.0
    DO B=1,mNumBldg
      APLCON(Y,F,D)= APLCON(Y,F,D)+ LEAPYR*APLEQP(CurCalYr,B,D,F)*APPUEC(D,F,B) * RSELAST(F1,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
      APLCONWT(Y,F,D,B)= LEAPYR*APLEQP(CurCalYr,B,D,F)*APPUEC(D,F,B) * RSELAST(F1,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
      IF (EH(CurCalYr,B,D)+NH(CurCalYr,B,D).GT.0.) THEN
        APLCONIN(Y,F,D,B)= (APLEQP(CurCalYr,B,D,F)*APPUEC(D,F,B)) / (EH(CurCalYr,B,D)+NH(CurCalYr,B,D))
      ENDIF
      APEQCN(Y,F,B,D)=LEAPYR*APLEQP(CurCalYr,B,D,F)*APPUEC(D,F,B) * RSELAST(F1,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE APPCNS


!==============================================================================
! AGGREGATED FUEL CONSUMPTION SUBROUTINE
!==============================================================================
SUBROUTINE FUELCN
IMPLICIT NONE

INTEGER Y,D,F

Y=CurCalYr-BaseYr+1

!CALCULATE DIVISIONAL FUEL CONSUMPTION
DO D=1,mNumCR-2
  !NATURAL GAS
  RSFLCN(Y,1,D)= (HTRCON(Y,1,D)+H2OCON(Y,1,D)+CKCON(Y,1,D)+DRYCON(Y,1,D)+COOLCN(Y,2,D)+SHTCON(Y,1,D)+APLCON(Y,1,D))/1000000.	!TODO - remove COOLCN if switching NG_HP to MS_HP

  !ELECTRICITY
  RSFLCN(Y,2,D)= (HTRCON(Y,2,D)+COOLCN(Y,1,D)+H2OCON(Y,2,D)+REFCON(Y,D)+CKCON(Y,3,D)+DRYCON(Y,2,D)+SHTCON(Y,2,D)+TVRCON(Y,D)+ &
          FRZCON(Y,D)+LTCON(Y,D)+APCON(Y,D)+PCRCON(Y,D)+FANCON(Y,D)+CSWCON(Y,D)+DSWCON(Y,D))/1000000.

  !DISTILLATE FUEL OIL
  RSFLCN(Y,3,D)= (APLCON(Y,3,D)+HTRCON(Y,3,D)+H2OCON(Y,3,D)+SHTCON(Y,3,D))/1000000.

  !PROPANE
  RSFLCN(Y,4,D)= (SHTCON(Y,4,D)+APLCON(Y,2,D)+HTRCON(Y,4,D)+H2OCON(Y,4,D)+CKCON(Y,2,D))/1000000.

  !WOOD
  RSFLCN(Y,5,D)=(HTRCON(Y,5,D)+SHTCON(Y,5,D))/1000000.  !NoKero
ENDDO

!CALCULATE NATIONAL (DIVISION 10) FUEL CONSUMPTION	!TODO - aggreggate to mNumCR=11 instead of 10; 10 is intended as a placeholder for California, not national total
DO F=1,mNumFuel  !NoKero
  RSFLCN(Y,F,10)=0.0
  DO D=1,mNumCR-2
    RSFLCN(Y,F,10)=RSFLCN(Y,F,10)+RSFLCN(Y,F,D)
  ENDDO
ENDDO

END SUBROUTINE FUELCN


!==============================================================================
!  NEMS CONSUMPTION OUTPUT SUBROUTINE
!==============================================================================
SUBROUTINE NEMSCN
IMPLICIT NONE

INCLUDE 'tranrep'

INTEGER Y,D,F

Y=CurCalYr-BaseYr+1
SLCON(Y,10)=0.0	!TODO - replace 10 with mNumCR?
QPVRS(NationalPtr,y)=0.0

!CALCULATE DIVISIONAL FUEL CONSUMPTION
DO D=1,mNumCR-2
  !NATURAL GAS
  QNGRS(D,Y)=RSFLCN(Y,1,D)
  QGFRS(D,Y)=QNGRS(D,Y)*1.0	!TODO - is this variable/calculation still necessary?
  QGIRS(D,Y)=QNGRS(D,Y)*0.0	!TODO - is this still necessary if multiplied by zero?

  !ELECTRICITY
  QELRS(D,Y)=RSFLCN(Y,2,D)-(TrillsOwnUse(Y,D,1)+TrillsOwnUse(Y,D,2)+TrillsOwnUse(Y,D,3)+TrillsOwnUse(Y,D,4))/1000. + TRQ_ELEC(1,D,Y)  !Subtract all onsite own-use generation; QELRS = quads purchased electricity from grid  !BESSmodel

  !DISTILLATE FUEL OIL + KEROSENE
  IF (RSFLCN(Y,3,D).LT.0.) THEN
    QDSRS(D,Y)=0.
  ELSE
    QDSRS(D,Y)=RSFLCN(Y,3,D)
  ENDIF

  !PROPANE
  QLGRS(D,Y)=RSFLCN(Y,4,D)
  QPRRS(D,Y)=QLGRS(D,Y)

  !WOOD
  QBMRS(D,Y)=RSFLCN(Y,5,D)

  !SOLAR PHOTOVOLTAIC
  IF (CurIYr.LT.RECSYear-BaseYr+1) THEN
    QPVRS(D,Y)=QPVRS(D,Y-1)
  ELSE
    QPVRS(D,Y)=Trills(Y,D,1)
  ENDIF

  QPVRS(NationalPtr,y)=QPVRS(NationalPtr,y)+QPVRS(d,y)  !Aggregate solar PV to national total

  !SOLAR THERMAL WATER HEATING (SOLAR_WH)
  SLCON(Y,10)=SLCON(Y,10)+SLCON(Y,D)	!TODO - mNumCR=10 for California; should be MNUMCR=11 for national total instead?
  QSTRS(D,Y)=SLCON(Y,D)/1000000.

  !KEROSENE
  QKSRS(D,Y)=0.0  !NoKero	!TODO - no longer modeled; remove this?
  !GEOTHERMAL
  QGERS(D,Y)=0.0	!TODO - no longer modeled; remove this?
ENDDO

END SUBROUTINE NEMSCN


!==============================================================================
!  RESIDENTIAL REPORTING VARIABLE OUTPUT SUBROUTINE (REPORTER TABLE 4)
!==============================================================================
SUBROUTINE RESDRP
IMPLICIT NONE

INTEGER Y, B, D, F, F2,Y1

Y=CurIYr

!AGGREGATE EXISTING HOUSES, NEW HOUSES, HOUSING STARTS
RSEH(Y,1:mNumBldg)=0.0
RSNH(Y,1:mNumBldg)=0.0
RSHSEADD(Y,1:mNumBldg)=0.0
DO B=1,mNumBldg
  DO D=1,mNumCR-2
    RSEH(Y,B)=RSEH(Y,B)+EH(CurCalYr,B,D)
    RSNH(Y,B)=RSNH(Y,B)+NH(CurCalYr,B,D)
    RSHSEADD(Y,B)=RSHSEADD(Y,B)+HSEADD(CurCalYr,B,D)
  ENDDO
ENDDO

!AGGREGATE EXISTING HOUSES, NEW HOUSES & HOUSING STOCKS
RSHOUSES(Y,1:mNumCR-2)=0.0
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    RSHOUSES(Y,D)=RSHOUSES(Y,D)+(EH(CurCalYr,B,D)+NH(CurCalYr,B,D))
    TotalHouses(y,b,d)=(EH(CurCalYr,B,D)+NH(CurCalYr,B,D))
  ENDDO
ENDDO

!AGGREGATE SPACE HEATING CONSUMPTION
RSHTRCON(Y,1:NHTRFL)=0.0
DO F=1,NHTRFL  !NoKero - 5 space heating fuels reported to Table 4
  DO D=1,mNumCR-2
    !Aggregate both primary and secondary space heating consumption
    RSHTRCON(Y,F)=RSHTRCON(Y,F)+HTRCON(Y,F,D)+SHTCON(Y,F,D)  !NoKero
  ENDDO
ENDDO

!AGGREGATE SPACE COOLING CONSUMPTION
RSCOOLCN(Y,1:NCLFL)=0.0
DO D=1,mNumCR-2
  DO F=1,NCLFL
    RSCOOLCN(Y,F)=RSCOOLCN(Y,F)+COOLCN(Y,F,D)
  ENDDO
ENDDO

!AGGREGATE WATER HEATING CONSUMPTION
RSH2OCON(Y,1:NWHFL)=0.0
RSH2OCON(Y,5)=SLCON(Y,10)	!TODO - mNumCR=10 for California; should be MNUMCR=11 for national total instead?
DO F=1,NWHFL-1  !excludes solar water heating consumption from previous line  !DG report - general cleanup
  DO D=1,mNumCR-2
    RSH2OCON(Y,F)=RSH2OCON(Y,F)+H2OCON(Y,F,D)
  ENDDO
ENDDO

!AGGREGATE COOKING CONSUMPTION
RSCKCON(Y,1:NSTVFL)=0.0
DO D=1,mNumCR-2
  RSCKCON(Y,1)=RSCKCON(Y,1)+CKCON(Y,1,D)
  RSCKCON(Y,2)=RSCKCON(Y,2)+CKCON(Y,2,D)
  RSCKCON(Y,3)=RSCKCON(Y,3)+CKCON(Y,3,D)
ENDDO

!AGGREGATE CLOTHES DRYING CONSUMPTION
RSDRYCON(Y,1:NDRYFL)=0.0
DO F=1,NDRYFL
  DO D=1,mNumCR-2
    RSDRYCON(Y,F)=RSDRYCON(Y,F)+DRYCON(Y,F,D)
  ENDDO
ENDDO

!AGGREGATE OTHER UNSPECIFIED APPLIANCE CONSUMPTION
RSAPCON(Y,1:4)=0.0	!TODO - replace 4 with parameter
DO D=1,mNumCR-2
  RSAPCON(Y,1)=RSAPCON(Y,1)+APLCON(Y,1,D) !Natural Gas
  RSAPCON(Y,2)=RSAPCON(Y,2)+APCON(Y,D)    !Electricity
  RSAPCON(Y,3)=RSAPCON(Y,3)+APLCON(Y,3,D) !Distillate Fuel Oil/Kerosene
  RSAPCON(Y,4)=RSAPCON(Y,4)+APLCON(Y,2,D) !Propane
ENDDO

!AGGREGATE CLOTHES WASHING, DISHWASHING, REFRIGERATION, FREEZING, LIGHTING, AND MISCELLANEOUS ELECTRIC LOAD (MELs) CONSUMPTION
RSCSWCON(Y)=0.0
RSDSWCON(Y)=0.0
RSREFCON(Y)=0.0
RSFRZCON(Y)=0.0
RSLTCON(Y)=0.0
RSLTCON(RECSYear-BaseYr+1)=0.0
RSTVRCON(Y)=0.0
RSPCRCON(Y)=0.0
RSFANCON(Y)=0.0
DO D=1,mNumCR-2
  RSCSWCON(Y)=RSCSWCON(Y)+CSWCON(Y,D)
  RSDSWCON(Y)=RSDSWCON(Y)+DSWCON(Y,D)
  RSREFCON(Y)=RSREFCON(Y)+REFCON(Y,D)
  RSFRZCON(Y)=RSFRZCON(Y)+FRZCON(Y,D)
  RSLTCON(RECSYear-BaseYr+1)= RSLTCON(RECSYear-BaseYr+1)+LTCON(RECSYear-BaseYr+1,D)	!TODO - verify
  RSLTCON(Y)= RSLTCON(Y)+LTCON(Y,D)
  RSTVRCON(Y)=RSTVRCON(Y)+TVRCON(Y,D)
  RSFANCON(Y)=RSFANCON(Y)+FANCON(Y,D)
  RSPCRCON(Y)=RSPCRCON(Y)+PCRCON(Y,D)
ENDDO

END SUBROUTINE RESDRP


!==============================================================================
! RESIDENTIAL EQUIPMENT, SHELL, AND DISTRIBUTED GENERATION REPORT SUBROUTINE (REPORTER TABLE 30)	!TODO - reorganize/optimize table output so most/all of this subroutine can use CASE or similar structure for simplicity
!==============================================================================
SUBROUTINE RESDRP2
IMPLICIT NONE

REAL*4 EHANDNH(RECSYear:EndYr,mNumBldg,mNumCR-2)
REAL*4 RACUnits(mNumBldg,mNumCR-2),X,TEMP
REAL*4 NUME(RECSYear:EndYr+1,15),DEN(RECSYear:EndYr+1,15),NUME1(RECSYear:EndYr+1,15,mNumBldg,mNumCR-2),DEN1(RECSYear:EndYr+1,15,mNumBldg,mNumCR-2)	!TODO - replace 15 with parameter (nHeatClasses+nCoolClasses? or just nHeatClasses because it's the EU with most equipment classes?)
REAL*4 RSCLUSR(RECSYear:EndYr+1)
REAL*4 RSCLUSC(RECSYear:EndYr+1)
REAL*4 HSHINDE(RECSYear:EndYr,mNumFuel,mNumCR-2,mNumBldg),HSHINDA(RECSYear:EndYr,mNumFuel,mNumCR-2,mNumBldg),HSHINDN(RECSYear:EndYr,mNumFuel,mNumCR-2,mNumBldg), &
       HSHELLE(RECSYear:EndYr),HEATOTE(RECSYear:EndYr), &
       HSHELLN(RECSYear:EndYr),HEATOTN(RECSYear:EndYr), &
       HSHELLA(RECSYear:EndYr),HEATOTA(RECSYear:EndYr)
REAL*4 CSHINDE(RECSYear:EndYr,mNumCR-2,mNumBldg),CSHINDA(RECSYear:EndYr,mNumCR-2,mNumBldg),CSHINDN(RECSYear:EndYr,mNumCR-2,mNumBldg), &
       CSHELLE(RECSYear:EndYr),COLTOTE(RECSYear:EndYr), &
       CSHELLN(RECSYear:EndYr),COLTOTN(RECSYear:EndYr), &
       CSHELLA(RECSYear:EndYr),COLTOTA(RECSYear:EndYr)
INTEGER Y,D,B,E,E2,E3,F,T,V,Y1
INTEGER EU,RECCL,EQC,NUMEQC,RECTY,TYPE,EQT,NUMEQT,OTYPE
INTEGER EUHT,RECCLHHP,RECTYHT,TYPEHT
CHARACTER*15  EUPRNAMES(10) !electricity end-use price categories	!TODO - replace 10 with parameter
DATA EUPRNAMES/'Space Heating','Space Cooling','Water Heating','Cooking', &
               'Clothes Drying','Refrigeration','Freezing','Lighting', &
               'Appliances','Secondary Heat'/
CHARACTER*18 HEN(8),CEN(5),WEN(5), CKEN(3), DRYEN(2)	!TODO - verify use; appear to be related to RESDREP and Table 30
DATA HEN/'Electric HP','Other Electric','Gas HP','Gas Other','Distillate','Propane','Wood Stoves','Geothermal HP'/
DATA CEN/'Room AC','Central Air','Heat Pump','GSHP','GHP'/	!TODO - these are mapped in a different order than RSCOOLERS/ Table 30; verify use
DATA WEN/'Natural Gas','Electric','Distillate','Propane','Solar'/	!TODO - verify use
DATA CKEN/'Natural Gas','Propane','Electric'/	!TODO - verify use
DATA DRYEN/'Natural Gas','Electric'/	!TODO - verify use

!********************************************************************
!AGGREGATE SPACE HEATING SYSTEMS
! SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!********************************************************************
EU = 1

!Initialize array
RSHTRS(RECSYear-BaseYr+1:LastYr,1:8) = 0.0 !main space heaters reported in Table 30; currently 8 rows  !NoKero	!TODO - NG_FA and DIST_FA not currently reported here; have ALL equipment classes represented? replace 8 with parameter?

DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    !EQC values same as RTCLEQCL in RSCLASS; E2 is the order in which technologies appear in Table 30
    IF (EQC.EQ.1)  THEN  !ELEC_RAD
      E2=2
    ELSEIF (EQC.EQ.2) THEN  !ELEC_HP
      E2=1
    ELSEIF (EQC.LE.4) THEN  !NG_FA and NG_RAD
      E2=4
    ELSEIF (EQC.EQ.5) THEN  !LPG_FA  !NoKero
      E2=6
    ELSEIF (EQC.LE.7) THEN  !DIST_FA and DIST_RAD  !NoKero
      E2=5
    ELSEIF (EQC.EQ.8) THEN  !WOOD_HT  !NoKero
      E2=7  !NoKero
    ELSEIF (EQC.EQ.9) THEN  !GEO_HP  !NoKero
      E2=8  !NoKero
    ELSEIF (EQC.EQ.10) THEN  !NG_HP  !NoKero
      E2=3
    ENDIF
    DO B=1,mNumBldg
      DO D=1,mNumCR-2
        IF (Y.EQ.RECSYear) THEN
          RSHTRS(Y1,E2)=RSHTRS(Y1,E2) + EQCESE(Y,RECCL,B,D)
        ELSE
          RSHTRS(Y1,E2)=RSHTRS(Y1,E2) + EQCESE(Y,RECCL,B,D) + &
           EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D) + &
           EQCADD(Y,RECCL,B,D) + EQCREP(Y,RECCL,B,D) + &
           EQCSUR(Y,RECCL,B,D) + EQCRP90RP(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!INITIALIZE HEATING SHELL INDEX ARRAYS
HSHINDE(RECSYear:LastYr+BaseYr-1,1:mNumFuel,1:mNumCR-2,1:mNumBldg)=0.0
HSHINDA(RECSYear:LastYr+BaseYr-1,1:mNumFuel,1:mNumCR-2,1:mNumBldg)=0.0
HSHINDN(RECSYear:LastYr+BaseYr-1,1:mNumFuel,1:mNumCR-2,1:mNumBldg)=0.0

!AGGREGATE HEATING SYSTEMS FOR COMPUTING AGGREGATE SHELL
DO Y=RECSYear,LastYr+BaseYr-1
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      E=RTCLEQCL(RECCL)
      F=RTFUEL(RECCL)
      DO B=1,mNumBldg
        IF (Y.EQ.RECSYear) THEN
          HSHINDE(Y,F,D,B)=HSHINDE(Y,F,D,B)+EQCESE(Y,RECCL,B,D)
          HSHINDN(Y,F,D,B)=0.0
          HSHINDA(Y,F,D,B)=0.0
        ELSE
          HSHINDE(Y,F,D,B)=HSHINDE(Y,F,D,B)+EQCESE(Y,RECCL,B,D) &
           + EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D) &
           + EQCRP90RP(Y,RECCL,B,D)
          HSHINDN(Y,F,D,B)=HSHINDN(Y,F,D,B) + EQCADD(Y,RECCL,B,D)
          HSHINDA(Y,F,D,B)=HSHINDA(Y,F,D,B) + EQCREP(Y,RECCL,B,D) + EQCSUR(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!CALCULATE HEATING SHELL INDICES FOR REPORT
DO Y=RECSYear,LastYr+BaseYr-1
  HSHELLE(Y)= 0.0
  HSHELLN(Y)= 0.0
  HSHELLA(Y)= 0.0
  DO F=1,mNumFuel  !NoKero
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        IF (Y.EQ.RECSYear) THEN
          HSHELLE(Y)=HSHELLE(Y)+(HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
          HSHELLN(Y)=HSHELLN(Y)+(HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
          HSHELLA(Y)=HSHELLA(Y)+(HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
        ELSE
          HSHELLE(Y)=HSHELLE(Y) + (HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
          HSHELLN(Y)=HSHELLN(Y) + (HSHINDN(Y,F,D,B)*NHSHELL(Y,F,D,B))
          HSHELLA(Y)=HSHELLA(Y) + (HSHINDA(Y,F,D,B)*AHSHELL(Y,F,D,B) &
           + HSHINDN(Y,F,D,B)*NHSHELL(Y,F,D,B) + HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear,LastYr+BaseYr-1
  HEATOTE(Y)=0.0
  HEATOTN(Y)=0.0
  HEATOTA(Y)=0.0
  DO F=1,mNumFuel  !NoKero
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        HEATOTE(Y)=HEATOTE(Y)+HSHINDE(Y,F,D,B)
        HEATOTN(Y)=HEATOTN(Y)+HSHINDN(Y,F,D,B)
        HEATOTA(Y)=HEATOTA(Y)+HSHINDN(Y,F,D,B)+HSHINDE(Y,F,D,B)+HSHINDA(Y,F,D,B)
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  IF (HEATOTE(Y).GT.0.0) THEN
    HSHELL1(Y1)=HSHELLE(Y)/HEATOTE(Y)
  ELSE
    HSHELL1(Y1)=1.0
  ENDIF
  IF (HEATOTN(Y).GT.0.0) THEN
    HSHELL2(Y1)=HSHELLN(Y)/HEATOTN(Y)
  ELSE
    HSHELL2(Y1)=1.0
  ENDIF
  IF (HEATOTA(Y).GT.0.0) THEN
    HSHELL3(Y1)=HSHELLA(Y)/HEATOTA(Y)
  ELSE
    HSHELL3(Y1)=1.0
  ENDIF
ENDDO

!WRITE OUT VARIABLES OF SPECIAL INTEREST TO UNIT 9 (RDM_OUT.TXT)	!TODO - add switch to turn these on?
IF (PRTDBGR.EQ.1) THEN
  WRITE(9,*) 'Estimated number of natural gas customers based on equipment count by end use (maximum of space heating->water heating->cooking->clothes drying); used by NGMM'
  WRITE(9,*) 'YEAR  CD  RSGASCUST    WATERTOT    COOKTOT    DRYERTOT'
  DO Y=RECSYear,LastYr+BaseYr-1
    DO D=1,mNumCR-2
      WRITE(9,222) Y,D,RSGASCUST(Y,D),WATERTOT(Y,D),COOKTOT(Y,D),DRYERTOT(Y,D)
    ENDDO
  ENDDO
ENDIF
222 FORMAT(I4,I2,4(1X,F14.0))

WRITE(9,*) 'single-family replacement UECs - space heating'
DO d=1,mNumCR-2
  WRITE(9,*) 'D=',D
  WRITE(9,1140)(RTCLNAME(E),E=1,nHeatClasses)
  DO Y=RECSYear+1,LastYr+BaseYr-1
    WRITE(9,1141) Y,(EQCRUEC(Y,E,1,D),E=1,nHeatClasses)
  ENDDO
ENDDO
1140 FORMAT(6X,11(1X,a9))
1141 FORMAT(I6,11(1X,F9.4))

WRITE(9,*) 'single-family average UECs - space heating'
DO d=1,mNumCR-2
  WRITE(9,*) 'D=',D
  WRITE(9,1140)(RTCLNAME(E),E=1,nHeatClasses)
  DO Y=RECSYear+1,LastYr+BaseYr-1
    WRITE(9,1141) Y,(EQCAUEC(Y,E,1,D),E=1,nHeatClasses)
  ENDDO
ENDDO

WRITE(9,*) 'single-family new UECs - space heating'
DO d=1,mNumCR-2
  WRITE(9,*) 'D=',D
  WRITE(9,1140)(RTCLNAME(E),E=1,nHeatClasses)
  DO Y=RECSYear+1,LastYr+BaseYr-1
    WRITE(9,1141) Y,(EQCNUEC(Y,E,1,D),E=1,nHeatClasses)
  ENDDO
ENDDO

!BNCHFCT adjustment for RECS through first two years after hard-bench STEO year (last two years of factors should be same and carried through projection)
WRITE(9,*) 'BNCHFCT(trills?)'	!TODO - is this actually in quads? output values in RDM_OUT.txt seem much too high to be quads, but the variables used to calculate BNCHFCT appear to be in quads
DO Y=(RECSYear-BaseYr+1),(LASTSTEOYR-BaseYr+3)
  WRITE(9,*) Y+BaseYr-1
  DO D=1,mNumCR-2
    WRITE(9,114) D,(BNCHFCT(Y,F,D),F=1,mNumFuel-1)  !major fuels (natural gas, electricity, distillate fuel oil/kerosene, propane)  !NoKero
  ENDDO
ENDDO
114 FORMAT(2X,I1,4(1X,F9.4))

!AGGREGATE SPACE COOLING SYSTEMS
! SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
EU = 2

DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    E=RTCLEQCL(RECCL)
    IF (E.EQ.1) THEN  !ROOM_AIR
      E2=5
    ELSEIF (E.EQ.2) THEN  !CENT_AIR
      E2=4
    ELSEIF (E.EQ.3) THEN  !ELEC_HP
      E2=1
    ELSEIF (E.EQ.4) THEN  !GEO_HP
      E2=3
    ELSEIF (E.EQ.5) THEN  !NG_HP
      E2=2
    ENDIF
    RSCOOLERS(Y1,E2)=0.0
    DO B=1,mNumBldg
      DO D=1,mNumCR-2
        X=1.0
        IF (RTCLNAME(RECCL).EQ.'ROOM_AIR') X=1.0 ! RACUnits(B,D)
          IF (Y.EQ.RECSYear) THEN
            RSCOOLERS(Y1,E2)=RSCOOLERS(Y1,E2)+EQCESE(Y,RECCL,B,D)*X
          ELSE
            RSCOOLERS(Y1,E2)=RSCOOLERS(Y1,E2)+ (  EQCESE(Y,RECCL,B,D) + &
            EQCRP90(Y,RECCL,B,D)+ &
            EQCSR90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D) + &
            EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) + &
            EQCSUR(Y,RECCL,B,D))*X
          ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!INITIALIZE COOLING SHELL INDEX ARRAYS
CSHINDE(RECSYear:LastYr+BaseYr-1,1:mNumCR-2,1:mNumBldg)=0.0
CSHINDA(RECSYear:LastYr+BaseYr-1,1:mNumCR-2,1:mNumBldg)=0.0
CSHINDN(RECSYear:LastYr+BaseYr-1,1:mNumCR-2,1:mNumBldg)=0.0

!AGGREGATE SPACE COOLING SYSTEMS FOR COMPUTING AGGREGATE SHELL
DO Y=RECSYear,LastYr+BaseYr-1
  DO D=1,mNumCR-2
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      E=RTCLEQCL(RECCL)
      DO B=1,mNumBldg
        IF (Y.EQ.RECSYear) THEN
          CSHINDE(Y,D,B)=CSHINDE(Y,D,B)+EQCESE(Y,RECCL,B,D)
        ELSE
          CSHINDE(Y,D,B) = CSHINDE(Y,D,B) + EQCESE(Y,RECCL,B,D) + EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D) + EQCRP90RP(Y,RECCL,B,D)
          CSHINDN(Y,D,B) = CSHINDN(Y,D,B) + EQCADD(Y,RECCL,B,D)
          CSHINDA(Y,D,B) = CSHINDA(Y,D,B) + EQCREP(Y,RECCL,B,D) + EQCSUR(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!CALCULATE COOLING SHELL INDICES FOR REPORT
DO Y=RECSYear,LastYr+BaseYr-1
  CSHELLE(Y)= 0.0
  CSHELLN(Y)= 0.0
  CSHELLA(Y)= 0.0
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      IF (Y.EQ.RECSYear) THEN
        CSHELLE(Y) = CSHELLE(Y) + (CSHINDE(Y,D,B) * ECSHELL(Y,D,B))
        CSHELLN(Y) = CSHELLN(Y) + (CSHINDE(Y,D,B) * ECSHELL(Y,D,B))
        CSHELLA(Y) = CSHELLA(Y) + (CSHINDE(Y,D,B) * ECSHELL(Y,D,B))
      ELSE
        CSHELLE(Y) = CSHELLE(Y) + (CSHINDE(Y,D,B) * ECSHELL(Y,D,B))
        CSHELLN(Y) = CSHELLN(Y) + (CSHINDN(Y,D,B) * NCSHELL(Y,D,B))
        CSHELLA(Y) = CSHELLA(Y) + (CSHINDE(Y,D,B) * ECSHELL(Y,D,B) + CSHINDN(Y,D,B) * NCSHELL(Y,D,B) + CSHINDA(Y,D,B) * ACSHELL(Y,D,B))
      ENDIF
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear,LastYr+BaseYr-1
  COLTOTE(Y) = 0.0
  COLTOTN(Y) = 0.0
  COLTOTA(Y) = 0.0
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      COLTOTE(Y) = COLTOTE(Y) + CSHINDE(Y,D,B)
      COLTOTN(Y) = COLTOTN(Y) + CSHINDN(Y,D,B)
      COLTOTA(Y) = COLTOTA(Y) + CSHINDE(Y,D,B) + CSHINDN(Y,D,B) + CSHINDA(Y,D,B)
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  IF (COLTOTE(Y).GT.0.0) THEN
    CSHELL1(Y1)=CSHELLE(Y)/COLTOTE(Y)
  ELSE
    CSHELL1(Y1)=1.0
  ENDIF
  IF (COLTOTN(Y).GT.0.0) THEN
    CSHELL2(Y1)=CSHELLN(Y)/COLTOTN(Y)
  ELSE
    CSHELL2(Y1)=1.0
  ENDIF
  IF (COLTOTA(Y).GT.0.0) THEN
    CSHELL3(Y1)=CSHELLA(Y)/COLTOTA(Y)
  ELSE
    CSHELL3(Y1)=1.0
  ENDIF
ENDDO

!TODO - EU = 3 and EU = 4 not currently included in Table 30, but could (should?) be

!AGGREGATE WATER HEATING SYSTEMS
! SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
EU = 5

!INITIALIZE WATER HEATERS ARRAY AS ZERO	!TODO - Optimize?
DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO E=1,nWatHtClasses  !Natural gas, electricity, distillate fuel oil, propane, solar
    RSWATER(Y1,E)=0.0
  ENDDO
ENDDO	
	
DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (Y.EQ.RECSYear) THEN
          RSWATER(Y1,EQC)=RSWATER(Y1,EQC)+EQCESE(Y,RECCL,B,D)
        ELSE
          RSWATER(Y1,EQC) = RSWATER(Y1,EQC) + EQCESE(Y,RECCL,B,D) + &
           EQCRP90(Y,RECCL,B,D) + EQCADD(Y,RECCL,B,D) + &
           EQCSR90(Y,RECCL,B,D) + EQCRP90RP(Y,RECCL,B,D) + &
           EQCREP(Y,RECCL,B,D) + EQCSUR(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!WRITE OUT WATER HEATING MARKET SHARES TO RDM_OUT.TXT (UNIT 9)	!TODO - is this still needed? not shares; quantity of equipment
WRITE(9,*) 'rswater shares?'
DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  WRITE(9,3773) Y,(RSWATER(Y1,E3),E3=1,5)  !Natural gas, electricity, distillate fuel oil, propane, solar	!TODO - is this still needed? If so, replace 5 with NWHFL? would need to be declared in different module/subroutine
ENDDO
3773 FORMAT(4X,I4,5(1X,F14.3))

!AGGREGATE COOKING SYSTEMS
! SET EU = 6 TO SEARCH THE COOKING SECTION OF THE DATA
EU = 6

DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    RSCOOK(Y1,EQC) = 0.0
    DO B=1,mNumBldg
      DO D=1,mNumCR-2
        IF (Y.EQ.RECSYear) THEN
          RSCOOK(Y1,EQC) = RSCOOK(Y1,EQC) + EQCESE(Y,RECCL,B,D)
        ELSE
          RSCOOK(Y1,EQC) = RSCOOK(Y1,EQC) + EQCESE(Y,RECCL,B,D) + &
           EQCRP90(Y,RECCL,B,D) + EQCADD(Y,RECCL,B,D) + &
           EQCSR90(Y,RECCL,B,D) + EQCRP90RP(Y,RECCL,B,D) + &
           EQCREP(Y,RECCL,B,D) + EQCSUR(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!AGGREGATE COOKING SYSTEMS FOR MARKET SHARE ANALYSIS
NUMEQT = RTTYPECT(EU+1) - RTTYPECT(EU)

!WRITE OUT COOKING MARKET SHARES TO RDM_OUT.TXT (UNIT 9)	!TODO - is this still needed? not shares; quantity of equipment
WRITE(9,*) ' '
WRITE(9,*) 'rscook'
NUMEQC=RTCLEUPT(EU+1)-RTCLEUPT(EU)
DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  WRITE(9,3117) Y,(RSCOOK(Y1,EQC),EQC=1,NUMEQC)
ENDDO
3117 FORMAT(4X,I7,3(1X,F12.1))

!AGGREGATE CLOTHES DRYING SYSTEMS
! SET EU = 7 TO SEARCH THE CLOTHES DRYER SECTION OF THE DATA
EU = 7

DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    RSDRY(Y1,EQC)=0.0
    DO B=1,mNumBldg
      DO D=1,mNumCR-2
        IF (Y.EQ.RECSYear) THEN
          RSDRY(Y1,EQC) = RSDRY(Y1,EQC) + EQCESE(Y,RECCL,B,D)
        ELSE
          RSDRY(Y1,EQC) = RSDRY(Y1,EQC) + EQCESE(Y,RECCL,B,D) + &
           EQCRP90(Y,RECCL,B,D) + EQCADD(Y,RECCL,B,D) + &
           EQCSR90(Y,RECCL,B,D) + EQCRP90RP(Y,RECCL,B,D) + &
           EQCREP(Y,RECCL,B,D) + EQCSUR(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!WRITE OUT DRYER MARKET SHARES TO RDM_OUT.TXT (UNIT 9)	!TODO - is this still needed? not shares; quantity of equipment
NUMEQC=RTCLEUPT(EU+1)-RTCLEUPT(EU)
WRITE(9,*) 'rsdry'
DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  WRITE(9,4117) Y,(RSDRY(Y1,EQC),EQC=1,NUMEQC)
ENDDO
4117 FORMAT(4X,I7,2(1X,F12.2))

!AGGREGATE FOOD REFRIGERATION SYSTEMS
! SET EU = 8 TO SEARCH THE FOOD REFRIG SECTION OF THE DATA
EU = 8

DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    RSREF(Y1)=0.0
    DO B=1,mNumBldg
      DO D=1,mNumCR-2
        IF (Y.EQ.RECSYear) THEN
          RSREF(Y1) = RSREF(Y1) + EQCESE(Y,RECCL,B,D)
        ELSE
          RSREF(Y1) = RSREF(Y1) + EQCESE(Y,RECCL,B,D) + &
           EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D) + &
           EQCADD(Y,RECCL,B,D) + EQCRP90RP(Y,RECCL,B,D) + &
           EQCREP(Y,RECCL,B,D) + EQCSUR(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!AGGREGATE STANDALONE FREEZING SYSTEMS
! SET EU = 9 TO SEARCH THE STANDALONE FREEZING SECTION OF THE DATA
EU = 9

DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    RSFRZ(Y1) = 0.0
    DO B=1,mNumBldg
      DO D=1,mNumCR-2
        IF (Y.EQ.RECSYear) THEN
          RSFRZ(Y1)=RSFRZ(Y1)+EQCESE(Y,RECCL,B,D)
        ELSE
          RSFRZ(Y1)=RSFRZ(Y1) + EQCESE(Y,RECCL,B,D) + &
           EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D) + &
           EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) + &
           EQCSUR(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!********************************************************************
!PRELIMINARY CALCULATION OF WEIGHTED NEW SPACE HEATER EFFICIENCIES
! SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!********************************************************************
EU = 1

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    NUME(Y,EQC)=0.0
    DEN(Y,EQC)=0.0
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        IF ((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
         (WTEQCEFFR(Y,RECCL,B,D).GT.0.0).AND. &
         (WTEQCEFFHV(Y,RECCL,B,D).GT.0.0)) THEN
          NUME(Y,EQC)=NUME(Y,EQC) +EQCADD(Y,RECCL,B,D)  &
           * (1/WTEQCEFFHV(Y,RECCL,B,D)) &
           + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
           * (1/WTEQCEFFN(Y,RECCL,B,D)) &
           + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
          NUME1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)  &
           * (1/WTEQCEFFHV(Y,RECCL,B,D)) &
           + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
           * (1/WTEQCEFFN(Y,RECCL,B,D)) &
           + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
          DEN(Y,EQC)=DEN(Y,EQC)+ EQCADD(Y,RECCL,B,D)  + &
           EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
          DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)  + &
           EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    IF (RTCLNAME(RECCL).EQ.'ELEC_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFHT(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)   ! ELEC_HP
    ELSEIF (RTCLNAME(RECCL).EQ.'NG_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFHT(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_HP
    ELSEIF (RTCLNAME(RECCL).EQ.'GEO_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFHT(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)   ! GEO_HP
    ELSEIF (RTCLNAME(RECCL).EQ.'NG_FA'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFHT(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_FA
    ELSEIF (RTCLNAME(RECCL).EQ.'DIST_FA'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFHT(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)   ! DIST_FA
    ENDIF
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
          IF (EQC.NE.2) THEN
            RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
          ELSE
            RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)*3.412
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED NEW COOLER EFFICIENCIES
! SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
EU = 2

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    E=RTCLEQCL(RECCL)
    NUME(Y,E)=0.0
    DEN(Y,E)=0.0
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        IF (E.NE.1) THEN
          IF ((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
           (WTEQCEFFR(Y,RECCL,B,D).GT.0.0).AND. &
           (WTEQCEFFHV(Y,RECCL,B,D).GT.0.0)) THEN
            NUME(Y,E)=NUME(Y,E) + EQCADD(Y,RECCL,B,D)  * &
             (1/WTEQCEFFHV(Y,RECCL,B,D))  &
             + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
             * (1/WTEQCEFFN(Y,RECCL,B,D)) &
             + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
            DEN(Y,E)=DEN(Y,E)+ EQCADD(Y,RECCL,B,D)  + &
             EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
            NUME1(Y,E,B,D)= EQCADD(Y,RECCL,B,D)  * &
             (1/WTEQCEFFHV(Y,RECCL,B,D))  &
             + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
             * (1/WTEQCEFFN(Y,RECCL,B,D)) &
             + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
            DEN1(Y,E,B,D)=EQCADD(Y,RECCL,B,D)  + &
             EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
          ENDIF
        ELSE
          IF ((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
           (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
            NUME(Y,E)=NUME(Y,E) +EQCADD(Y,RECCL,B,D)* &
             (1/WTEQCEFFN(Y,RECCL,B,D)) &
             + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
             * (1/WTEQCEFFN(Y,RECCL,B,D)) &
             + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
            DEN(Y,E)=DEN(Y,E)+ EQCADD(Y,RECCL,B,D)+ &
             EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
            NUME1(Y,E,B,D)=(EQCADD(Y,RECCL,B,D)+&
             + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
             * (1/WTEQCEFFN(Y,RECCL,B,D)) &
             + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
            DEN1(Y,E,B,D)=EQCADD(Y,RECCL,B,D)+ &
             EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    IF (RTCLNAME(RECCL).EQ.'ELEC_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFCL(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)   ! ELEC_HP
    ELSEIF (RTCLNAME(RECCL).EQ.'NG_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFCL(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_HP
    ELSEIF (RTCLNAME(RECCL).EQ.'GEO_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFCL(Y1,3)=(NUME(Y,EQC)/DEN(Y,EQC))/3.412   ! GEO_HP
    ELSEIF (RTCLNAME(RECCL).EQ.'CENT_AIR'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFCL(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)   ! CENT_AIR
    ELSEIF (RTCLNAME(RECCL).EQ.'ROOM_AIR'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFCL(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)   ! ROOM_AIR
    ENDIF
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
          IF (EQC.LE.3) THEN
            RSNEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))*3.412
          ELSE
            RSNEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED NEW CLOTHES WASHER EFFICIENCIES
! SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
EU = 3

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
         + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
         * (WTEQCEFFN(Y,RECCL,B,D)) &
         + EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D)))
        DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED NEW CLOTHES WASHER EFFICIENCIES
! SET EU = 4 TO SEARCH THE DISHWASHER SECTION OF THE DATA
EU = 4

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        IF ((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
         (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
          NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
           + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
           * (1/WTEQCEFFN(Y,RECCL,B,D)) &
           + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
          DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
           EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED NEW H2O EFFICIENCIES
! SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
EU = 5

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    NUME(Y,EQC)=0.0
    DEN(Y,EQC)=0.0
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        IF ((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
         (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
          NUME(Y,EQC)=NUME(Y,EQC)+((EQCADD(Y,RECCL,B,D) &
           + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
           * (1/WTEQCEFFN(Y,RECCL,B,D)) &
           + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
          DEN(Y,EQC)=DEN(Y,EQC)+EQCADD(Y,RECCL,B,D)+ &
           EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
          NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
           + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
           * (1/WTEQCEFFN(Y,RECCL,B,D)) &
           + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
          DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
           EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    IF (RTCLNAME(RECCL).EQ.'ELEC_WH'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFHW(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)   ! ELEC_WH
    ELSEIF (RTCLNAME(RECCL).EQ.'NG_WH'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFHW(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_WH
    ELSEIF (RTCLNAME(RECCL).EQ.'DIST_WH'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFHW(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)   ! DIST_WH
    ELSEIF (RTCLNAME(RECCL).EQ.'LPG_WH'.AND.DEN(Y,EQC).GT.0.0) THEN
      RSNEFHW(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)   ! LPG_WH
    ENDIF
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED NEW COOKING EFFICIENCIES
! SET EU = 6 TO SEARCH THE COOKING SECTION OF THE DATA
EU = 6

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        IF ((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
         (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
          NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
           + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
           * (WTEQCEFFN(Y,RECCL,B,D)) &
           + EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D)))
          DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
           EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED NEW CLOTHES DRYER EFFICIENCIES
! SET EU = 7 TO SEARCH THE CLOTHES DRYER SECTION OF THE DATA
EU = 7

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        IF ((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
         (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
          NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
           + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
           * (1/WTEQCEFFN(Y,RECCL,B,D)) &
           + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
          DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
           EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED NEW REFRIGERATOR EFFICIENCIES
! SET EU = 8 TO SEARCH THE FOOD REFRIGERATION SECTION OF THE DATA
EU = 8

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    NUME(Y,EQC)=0.0
    DEN(Y,EQC)=0.0
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME(Y,EQC)=NUME(Y,EQC)+((EQCADD(Y,RECCL,B,D) &
         + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
         * WTEQCEFFN(Y,RECCL,B,D) &
         + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
        DEN(Y,EQC)=DEN(Y,EQC)+EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
        NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
         + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
         * WTEQCEFFN(Y,RECCL,B,D) &
         + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
        DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  IF (DEN(Y,1).GT.0.0) RSNEFRF(Y1)=NUME(Y,1)/DEN(Y,1)
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        Y1=Y-BaseYr+1
        IF (DEN1(Y,1,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED NEW STANDALONE FREEZER EFFICIENCIES
! SET EU = 9 TO SEARCH THE STANDALONE FREEZER SECTION OF THE DATA
EU = 9

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    NUME(Y,EQC)=0.0
    DEN(Y,EQC)=0.0
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME(Y,EQC)=NUME(Y,EQC)+((EQCADD(Y,RECCL,B,D) &
         + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
         * WTEQCEFFN(Y,RECCL,B,D) &
         + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
        DEN(Y,EQC)=DEN(Y,EQC)+EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
        NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
         + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
         * WTEQCEFFN(Y,RECCL,B,D) &
         + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
        DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  IF (DEN(Y,1).GT.0.0) RSNEFFZ(Y1)=NUME(Y,1)/DEN(Y,1)   !FREZ
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        Y1=Y-BaseYr+1
        IF (DEN1(Y,1,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)   !FREZ
      ENDDO
    ENDDO
  ENDDO
ENDDO

!WRITE OUT NEW WEIGHTED EFFICIENCIES TO RDM_OUT.TXT (UNIT 9)	!TODO - is this still needed? not shares; quantity of equipment; vectorize write statments and add PRTDBGR switch?
WRITE(9,*) 'rsnefht'
DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  WRITE(9,6667) Y1+BaseYr-1,(RSNEFHT(Y1,E3),E3=1,5)	!TODO - replace 5 with parameter?
  6667 FORMAT(4X,I4,5(1X,F6.2))
ENDDO

WRITE(9,*) 'rsnefcl'
DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  WRITE(9,6667) Y1+BaseYr-1,(RSNEFCL(Y1,E3)*3.412,E3=1,5)	!TODO - replace 5 with parameter?
ENDDO

WRITE(9,*) 'rsnefhw'
DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  WRITE(9,6668) Y1+BaseYr-1,(RSNEFHW(Y1,E3),E3=1,4)	!TODO - replace 4 with parameter?
  6668 FORMAT(4X,I4,4(1X,F7.4))
ENDDO

WRITE(9,*) 'rsnefrf'
DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  WRITE(9,6669) Y1+BaseYr-1, RSNEFRF(Y1)
  6669 FORMAT(4X,I4,1X,F7.2)
ENDDO

WRITE(9,*) 'rsneffz'
DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  WRITE(9,6669) Y1+BaseYr-1, RSNEFFZ(Y1)
ENDDO

!********************************************************************
!PRELIMINARY CALCULATION OF WEIGHTED EXISTING HEATER EFFICIENCIES
! SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!********************************************************************
EU = 1

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    NUME(Y,EQC)=0.0
    DEN(Y,EQC)=0.0
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME(Y,EQC)=NUME(Y,EQC)+ &
         (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
         (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
         (1/WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D))+ &
         EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (1/WTEQCEFFA(Y,RECCL,B,D)) )

        DEN(Y,EQC)=DEN(Y,EQC)+ ( EQCESE(Y,RECCL,B,D)+ &
         EQCRP90RP(Y,RECCL,B,D)+ EQCRP90(Y,RECCL,B,D) + &
         EQCREP(Y,RECCL,B,D) + EQCADD(Y,RECCL,B,D)+&
         EQCSUR(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D) )

        NUME1(Y,EQC,B,D)= &
         (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
         (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
         (1/WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D))+ &
         EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (1/WTEQCEFFA(Y,RECCL,B,D)) )

        DEN1(Y,EQC,B,D)= ( EQCESE(Y,RECCL,B,D)+ &
         EQCRP90RP(Y,RECCL,B,D)+ EQCRP90(Y,RECCL,B,D) + &
         EQCREP(Y,RECCL,B,D) + EQCADD(Y,RECCL,B,D)+&
         EQCSUR(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D) )
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  IF (RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
    RSEEFHT(RECSYear-BaseYr+1,1)=RTBASEFF(RECSYear,RECCL)  !ELEC_HP
  ELSEIF (RTCLNAME(RECCL).EQ.'NG_HP') THEN
    RSEEFHT(RECSYear-BaseYr+1,2)=RTBASEFF(RECSYear,RECCL)  !NG_HP
  ELSEIF (RTCLNAME(RECCL).EQ.'GEO_HP') THEN
    RSEEFHT(RECSYear-BaseYr+1,3)=RTBASEFF(RECSYear,RECCL)  !GEO_HP
  ELSEIF (RTCLNAME(RECCL).EQ.'NG_FA') THEN
    RSEEFHT(RECSYear-BaseYr+1,4)=RTBASEFF(RECSYear,RECCL)  !NG_FA
  ELSEIF (RTCLNAME(RECCL).EQ.'DIST_FA') THEN
    RSEEFHT(RECSYear-BaseYr+1,5)=RTBASEFF(RECSYear,RECCL)  !DIST_FA
  ENDIF

  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    IF (RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFHT(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFHT(Y1,1)=RSEEFHT(RECSYear-BaseYr+1,1)
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'NG_HP') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFHT(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFHT(Y1,2)=RSEEFHT(RECSYear-BaseYr+1,2)
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'GEO_HP') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFHT(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFHT(Y1,3)=RSEEFHT(RECSYear-BaseYr+1,3)
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'NG_FA') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFHT(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFHT(Y1,4)=RSEEFHT(RECSYear-BaseYr+1,4)
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'DIST_FA') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFHT(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFHT(Y1,5)=RSEEFHT(RECSYear-BaseYr+1,5)
      ENDIF
    ENDIF
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
          IF (EQC.NE.2) THEN
            RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
          ELSE
            RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)*3.412  !Multiply by 3.412 to convert ELEC_HP from COP to HSPF
          ENDIF
        ELSE
          IF (EQC.NE.2) THEN
            RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
          ELSE
            RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)*3.412  !Multiply by 3.412 to convert ELEC_HP from COP to HSPF
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED EXISTING SPACE COOLING EFFICIENCIES
! SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
EU = 2

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    E=RTCLEQCL(RECCL)
    NUME(Y,E)=0.0
    DEN(Y,E)=0.0
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        IF (E.EQ.1) THEN
          NUME(Y,E)=NUME(Y,E)+  &
           (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
           EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFN(Y,RECCL,B,D))+ &
           (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
           (1/WTEQCEFFN(Y,RECCL,B,D))+ &
           EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
           (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
           (1/WTEQCEFFA(Y,RECCL,B,D)))
          NUME1(Y,E,B,D)=  &
           (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
           EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFN(Y,RECCL,B,D))+ &
           (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
           (1/WTEQCEFFN(Y,RECCL,B,D))+ &
           EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
           (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
           (1/WTEQCEFFA(Y,RECCL,B,D)))
        ELSE
          NUME(Y,E)=NUME(Y,E)+  &
           (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
           EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D)) + &
           (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
           (1/WTEQCEFFN(Y,RECCL,B,D))+ &
           EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
           (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
           (1/WTEQCEFFA(Y,RECCL,B,D)))
          NUME1(Y,E,B,D)=  &
           (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
           EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D)) + &
           (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
           (1/WTEQCEFFN(Y,RECCL,B,D))+ &
           EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
           (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
           (1/WTEQCEFFA(Y,RECCL,B,D)))
        ENDIF

        DEN(Y,E)=DEN(Y,E)+  EQCESE(Y,RECCL,B,D) + &
         EQCRP90(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) + &
         EQCADD(Y,RECCL,B,D) +EQCSR90(Y,RECCL,B,D)+ &
         EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D)
        DEN1(Y,E,B,D)=  EQCESE(Y,RECCL,B,D)  + &
         EQCRP90(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) + &
         EQCADD(Y,RECCL,B,D) +EQCSR90(Y,RECCL,B,D)+ &
         EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D)
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  IF (RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
    RSEEFCL(RECSYear-BaseYr+1,1)=RTBASEFF(RECSYear,RECCL)  !ELEC_HP
  ELSEIF (RTCLNAME(RECCL).EQ.'NG_HP') THEN
    RSEEFCL(RECSYear-BaseYr+1,2)=RTBASEFF(RECSYear,RECCL)  !NG_HP
  ELSEIF (RTCLNAME(RECCL).EQ.'GEO_HP') THEN
    RSEEFCL(RECSYear-BaseYr+1,3)=RTBASEFF(RECSYear,RECCL)  !GEO_HP
  ELSEIF (RTCLNAME(RECCL).EQ.'CENT_AIR') THEN
    RSEEFCL(RECSYear-BaseYr+1,4)=RTBASEFF(RECSYear,RECCL)  !CENT_AIR
  ELSEIF (RTCLNAME(RECCL).EQ.'ROOM_AIR') THEN
    RSEEFCL(RECSYear-BaseYr+1,5)=RTBASEFF(RECSYear,RECCL)  !ROOM_AIR
  ENDIF

  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    IF (RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFCL(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFCL(Y1,1)=RSEEFCL(RECSYear-BaseYr+1,1)
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'NG_HP') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFCL(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFCL(Y1,2)=RSEEFCL(RECSYear-BaseYr+1,2)
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'GEO_HP') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFCL(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFCL(Y1,3)=RSEEFCL(RECSYear-BaseYr+1,3)
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'CENT_AIR') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFCL(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFCL(Y1,4)=RSEEFCL(RECSYear-BaseYr+1,4)
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'ROOM_AIR') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFCL(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFCL(Y1,5)=RSEEFCL(RECSYear-BaseYr+1,5)
      ENDIF
    ENDIF
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
          IF (EQC.LE.3) THEN
            RSEEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))*3.412  !Multiply by 3.412 to convert ROOM_AIR from COP to EER/ CENT_AIR and ELEC_HP from COP to SEER
          ELSE
            RSEEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))
          ENDIF
        ELSE
          IF (EQC.LE.3) THEN
            RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)*3.412  !Multiply by 3.412 to convert ROOM_AIR from COP to EER/ CENT_AIR and ELEC_HP from COP to SEER
          ELSE
            RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED EXISTING CLOTHES WASHER EFFICIENCIES
! SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
EU = 3

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME1(Y,EQC,B,D)= &
         (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
         (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (WTEQCEFFA(Y,RECCL,B,D)))
  
        DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
         EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
         EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
          RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
          RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO


!PRELIMINARY CALCULATION OF WEIGHTED EXISTING DISHWASHER EFFICIENCIES
! SET EU = 4 TO SEARCH THE DISHWASHER SECTION OF THE DATA
EU = 4

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME1(Y,EQC,B,D)= &
         (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL))+ &
         (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (1/WTEQCEFFA(Y,RECCL,B,D)))

        DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
         EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
         EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
          RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
          RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED EXISTING WATER HEATER EFFICIENCIES
! SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
EU = 5

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    E=RTCLEQCL(RECCL)
    NUME(Y,E)=0.0
    DEN(Y,E)=0.0
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME(Y,E)=NUME(Y,E)+ &
         (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
         (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (1/WTEQCEFFA(Y,RECCL,B,D)))

        DEN(Y,E)=DEN(Y,E)+(EQCESE(Y,RECCL,B,D)+ &
         EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
         EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
         EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))

        NUME1(Y,E,B,D)= &
         (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
         (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (1/WTEQCEFFA(Y,RECCL,B,D)))

        DEN1(Y,E,B,D)=(EQCESE(Y,RECCL,B,D)+ &
         EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
         EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
         EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  IF (RTCLNAME(RECCL).EQ.'ELEC_WH') THEN
    RSEEFHW(RECSYear-BaseYr+1,1)=RTBASEFF(RECSYear,RECCL)  !ELEC_WH
  ELSEIF (RTCLNAME(RECCL).EQ.'NG_WH') THEN
    RSEEFHW(RECSYear-BaseYr+1,2)=RTBASEFF(RECSYear,RECCL)  !NG_WH
  ELSEIF (RTCLNAME(RECCL).EQ.'DIST_WH') THEN
    RSEEFHW(RECSYear-BaseYr+1,3)=RTBASEFF(RECSYear,RECCL)  !DIST_WH
  ELSEIF (RTCLNAME(RECCL).EQ.'LPG_WH') THEN
    RSEEFHW(RECSYear-BaseYr+1,4)=RTBASEFF(RECSYear,RECCL)  !LPG_WH
  ENDIF

  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    IF (RTCLNAME(RECCL).EQ.'ELEC_WH') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFHW(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFHW(Y1,1)=RSEEFHW(RECSYear-BaseYr+1,1)  !ELEC_WH
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'NG_WH') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFHW(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFHW(Y1,2)=RSEEFHW(RECSYear-BaseYr+1,2)  !NG_WH
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'DIST_WH') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFHW(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFHW(Y1,3)=RSEEFHW(RECSYear-BaseYr+1,3)  !DIST_WH
      ENDIF
    ELSEIF (RTCLNAME(RECCL).EQ.'LPG_WH') THEN
      IF (DEN(Y,EQC).GT.0.0) THEN
        RSEEFHW(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)
      ELSE
        RSEEFHW(Y1,4)=RSEEFHW(RECSYear-BaseYr+1,4)  !LPG_WH
      ENDIF
    ENDIF
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
          RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
        ELSE
          RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED EXISTING COOKING EFFICIENCIES
! SET EU = 6 TO SEARCH THE COOKING SECTION OF THE DATA
EU = 6

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    E=RTCLEQCL(RECCL)
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME1(Y,E,B,D)= &
         (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
         (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (WTEQCEFFA(Y,RECCL,B,D)))
        DEN1(Y,E,B,D)=(EQCESE(Y,RECCL,B,D)+ &
         EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
         EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
         EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO


DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
          RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
        ELSE
          RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED EXISTING CLOTHES DRYER EFFICIENCIES
! SET EU = 7 TO SEARCH THE CLOTHES DRYER SECTION OF THE DATA
EU = 7

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    E=RTCLEQCL(RECCL)
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME1(Y,E,B,D)= &
         (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
         (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (1/WTEQCEFFA(Y,RECCL,B,D)))

        DEN1(Y,E,B,D)=(EQCESE(Y,RECCL,B,D)+ &
         EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
         EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
         EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
          RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
        ELSE
          RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED EXISTING REFRIGERATOR EFFICIENCIES
! SET EU = 8 TO SEARCH THE FOOD REFRIGERATION SECTION OF THE DATA
EU = 8

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  RSEEFRF(RECSYear-BaseYr+1)=RTBASEFF(RECSYear,RECCL)
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    NUME(Y,EQC)=0.0
    DEN(Y,EQC)=0.0
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME(Y,EQC)=NUME(Y,EQC)+ &
         (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
         (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (WTEQCEFFA(Y,RECCL,B,D)))

        DEN(Y,EQC)=DEN(Y,EQC)+(EQCRP90RP(Y,RECCL,B,D)+ &
         EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
         EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))

        NUME1(Y,EQC,B,D)=&
         (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
         (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (WTEQCEFFA(Y,RECCL,B,D)))

        DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
         EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
         EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  IF (DEN(Y,1).GT.0.0) RSEEFRF(Y1)=NUME(Y,1)/DEN(Y,1)
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
          RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
          RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!PRELIMINARY CALCULATION OF WEIGHTED EXISTING STANDALONE FREEZER EFFICIENCIES
! SET EU = 9 TO SEARCH THE STANDALONE FREEZING SECTION OF THE DATA
EU = 9

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  RSEEFFZ(RECSYear-BaseYr+1)=RTBASEFF(RECSYear,RECCL)
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
    EQC=RTCLEQCL(RECCL)
    NUME(Y,EQC)=0.0
    DEN(Y,EQC)=0.0
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        NUME(Y,EQC)=NUME(Y,EQC)+ &
         (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
         (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D))* &
         (WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (WTEQCEFFA(Y,RECCL,B,D)))

        DEN(Y,EQC)=DEN(Y,EQC)+(EQCRP90RP(Y,RECCL,B,D)+ &
         EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
         EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))

        NUME1(Y,EQC,B,D)=&
         (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
         (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
         EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
         (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
         (WTEQCEFFA(Y,RECCL,B,D)))

        DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
         EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
         EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
         EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear,LastYr+BaseYr-1
  Y1=Y-BaseYr+1
  IF (DEN(Y,1).GT.0.0) RSEEFFZ(Y1)=NUME(Y,1)/DEN(Y,1)
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
          RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
          RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

WRITE(9,*) 'END-USE ELECTRICITY PRICES BY CD (PELRSOUT_1987$)'	!TODO - still needed? add switch to turn on?
DO EU=1,10	!TODO - replace 10 with parameter (MNEURSGRP-1?)
  WRITE(9,*) 'EU= ', EU,' ', EUPRNAMES(EU)
  DO Y=RECSYear-BaseYr+1,LastYr
    WRITE(9,191) Y+BaseYr-1,(PELRSOUT(D,Y,EU),D=1,mNumCR-2)	!TODO - RDM EU numbers are not the same as EUGRP numbers from EMM; review or remove price write-out
  ENDDO
ENDDO
191 FORMAT (I4,9(1X,F5.2))

WRITE(9,*) 'AVERAGE ELECTRICITY PRICES BY CD (PELRS_1987$)'	!TODO - still needed? add switch to turn on?
DO Y=RECSYear-BaseYr+1,LastYr
  WRITE(9,191) Y+BaseYr-1,(PELRS(d,y),D=1,mNumCR-2)
ENDDO

END SUBROUTINE RESDRP2


!*******************************************************************
!NEW HOME HEATING SYSTEM REPORT
!*******************************************************************
SUBROUTINE NHTSHR

IMPLICIT NONE
COMMON/TESTHT/HTYSSHR(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR)
INTEGER Y, D, B, E, E2, EU, RECCL, EQC, NUMEQC
REAL*4 HTYSSHR, NWEQHTSH(RECSYear:EndYr,nHeatClasses), HEATCAL(RECSYear:EndYr,nHeatClasses), TOTALSUM(RECSYear:EndYr)

EU = 1 !space heating

NUMEQC=RTCLEUPT(EU+1)-RTCLEUPT(EU)

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO E=1,NUMEQC
    HEATCAL(Y,E)=0.0
    DO B=1,mNumBldg
      DO D=1,mNumCR-2
        HEATCAL(Y,E)= HEATCAL(Y,E) + (HSYSSHR(Y,E,B,D)*HSEADD(Y,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  TOTALSUM(Y)=0.0
    DO B=1,mNumBldg
      DO D=1,mNumCR-2
        TOTALSUM(Y)=TOTALSUM(Y)+HSEADD(Y,B,D)
    ENDDO
  ENDDO
ENDDO

DO Y=RECSYear+1,LastYr+BaseYr-1
  DO E=1,NUMEQC
    IF (TOTALSUM(Y).GT.0.0) THEN
      NWEQHTSH(Y,E)=HEATCAL(Y,E) / TOTALSUM(Y)
    ELSE
      NWEQHTSH(Y,E)=0.0
    ENDIF
  ENDDO
ENDDO

!WRITE OUT VARIABLES OF SPECIAL INTEREST TO UNIT 9 (RDM_OUT.TXT)	!TODO - add switch to turn these on?
IF (PRTDBGR.EQ.1) THEN
  WRITE(9,*) ''
  WRITE(9,*) ''
  WRITE(9,*) 'Residential New Heating Report'
  WRITE(9,*) 'New Home Heating System Shares (percent)'
  WRITE(9,*) ''
  WRITE(9,6)(RTCLNAME(E),E=1,NUMEQC)
  6 FORMAT(6x,11(1X,A7))

  DO Y=RECSYear+1,LastYr+BaseYr-1
    WRITE(9,2) Y,(NWEQHTSH(Y,E)*100.,E=1,NUMEQC)
  ENDDO
  2 FORMAT(1X,I4,11(1X,F7.2))
ENDIF

END SUBROUTINE NHTSHR


!==============================================================================
! RESIDENTIAL SEDS/MER/STEO CONSUMPTION BENCHMARKING SUBROUTINE
!==============================================================================
SUBROUTINE RSBENCH
IMPLICIT NONE

INCLUDE 'steoblock' !common STEO inputs

INTEGER iLastSTEOYr ! allows the last STEO year parameter to be reset when turning off STEO benchmarking
COMMON/STEO/STEOCN(RECSYear+1:LastSTEOYrAvail,9,mNumCR-2),STEObenchNG,STEObenchEL	!TODO - replace 9 with parameter (mNumFuel-1 for major fuels? 1=NG, 2=EL; no other fuels benchmarked with this variable?)
REAL*4 STEOCN,STEObenchNG,STEObenchEL
LOGICAL NEW
CHARACTER*18 FNAME
INTEGER FILE_MGR
EXTERNAL FILE_MGR
INTEGER IUNIT1
INTEGER Y,Y1,y4,D,F,B

Y=CurIYr !for brevity below

!First year processing to read RSSTEO input file
IF (CurCalYr.EQ.RECSYear) THEN
  FNAME='RSSTEO'
  NEW=.FALSE.
  IUNIT1=FILE_MGR('O',FNAME,NEW)
  READ(IUNIT1,'(19(/))')
  READ(IUNIT1,*) STEObenchNG
  READ(IUNIT1,'(1(/))')
  READ(IUNIT1,*) STEObenchEL
  READ(IUNIT1,'(1(/))')
  DO F=3,4 !Natural gas and electricity (F=1,2) are pulled directly from common steoblock below	!TODO - replace "3,4" with parameters?
    READ(IUNIT1,*)  !skip header
    DO D=1,mNumCR-2
      READ(IUNIT1,*) (STEOCN(Y1,F,D),Y1=RECSYear+1,LastSTEOYrAvail)
      !WRITE(9,*) (STEOCN(Y1,F,D),Y1=RECSYear+1,LastSTEOYrAvail)  !Write RSSTEO.txt inputs to RDM_OUT.txt
    ENDDO
  ENDDO

IUNIT1=FILE_MGR('C',FNAME,NEW)
!WRITE(9,'("rsbench,steobm,iLastSTEOYr,LastSTEOYr,LastSEDSyr,msedyr,GLOBALBENCHON,BENCHALLYRS")')
ENDIF

!If STEOBM .EQ. 0, then turn off STEO benchmarking by setting the last STEO year to the MER year
iLastSTEOYr=LastSTEOYr
IF (STEOBM.EQ.0) iLastSTEOYr=LastSEDSyr+1  !LastSEDSyr=BaseYr-1+MSEDYR (declared in resdrep include file)

!WRITE(9,'("rsbench",6i5,e15.4)') steobm,iLastSTEOYr,LastSTEOYr,LastSEDSyr,msedyr,GLOBALBENCHON,BENCHALLYRS

!Residential benchmarking uses STEO variables from common steoblock (consistent with MER through latest historical year), converted to trillion Btu.
! STEO doesn't have sector-level liquids variables. MER values for these fuels still needed in rsteo.txt.
! Regional natural gas values in STEO may also be problematic.

DO Y1= RECSYear+1, LastSTEOYrAvail  ! Get MER/STEO data from common block
  !Natural gas
  ! Use regional natural gas shares applied to national STEO total to calculate CD consumption
  STEOCN(Y1,1,1) = NGRCP_NEC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
  STEOCN(Y1,1,2) = NGRCP_MAC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
  STEOCN(Y1,1,3) = NGRCP_ENC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
  STEOCN(Y1,1,4) = NGRCP_WNC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
  STEOCN(Y1,1,5) = NGRCP_SAC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
  STEOCN(Y1,1,6) = NGRCP_ESC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
  STEOCN(Y1,1,7) = NGRCP_WSC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
  STEOCN(Y1,1,8) = NGRCP_MTN(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
  STEOCN(Y1,1,9) = NGRCP_PAC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.

  !Electricity (converted from bkWh to trillion Btu)
  STEOCN(Y1,2,1) = EXRCP_NEC(y) * 3.412
  STEOCN(Y1,2,2) = EXRCP_MAC(y) * 3.412
  STEOCN(Y1,2,3) = EXRCP_ENC(y) * 3.412
  STEOCN(Y1,2,4) = EXRCP_WNC(y) * 3.412
  STEOCN(Y1,2,5) = EXRCP_SAC(y) * 3.412
  STEOCN(Y1,2,6) = EXRCP_ESC(y) * 3.412
  STEOCN(Y1,2,7) = EXRCP_WSC(y) * 3.412
  STEOCN(Y1,2,8) = EXRCP_MTN(y) * 3.412
  STEOCN(Y1,2,9) = (EXRCP_PAC(y)+EXRCP_HAK(y)) * 3.412
ENDDO !y

!BEFORE BENCHMARKING, MAKE HIGH-LEVEL ADJUSTMENTS TO TOTALS FOR NATURAL GAS AND ELECTRICITY
DO D=1,mNumCR-2
  !DISTRIBUTED GENERATION
  ! ADD NATURAL GAS USAGE FOR FUEL CELLS AND DEDUCT SELF-GENERATED ELECTRICITY
  ! GasUsage(Y,D,2) IS FOR FUEL CELLS (DG TECHNOLOGY NUMBER 2)
  RSFLCN(Y,1,D)=RSFLCN(Y,1,D)+GasUsage(Y,D,2)-HWBTU(Y,D,2)
  !DISTRIBUTED GENERATION, ELECTRICITY GENERATION OFFSETS FROM GRID PURCHASES
  RSFLCN(Y,2,D)=RSFLCN(Y,2,D)-TrillsOwnUse(Y,D,1)-TrillsOwnUse(Y,D,2)-TrillsOwnUse(Y,D,3)-TrillsOwnUse(Y,D,4)  !BESSmodel
ENDDO

!TEST YEAR TO DETERMINE BENCHMARKING TREATMENTS
IF (Y.LE.MSEDYR) THEN
  !SEDS Historical Benchmarking Years [Data from NEMS Global (QSBLK) in trillion Btu]
  DO D=1,mNumCR-2
    BNCHFCT(Y,1,D) = QSNGRS(D,Y)-RSFLCN(Y,1,D) !natural gas
    BNCHFCT(Y,2,D) = QSELRS(D,Y)-RSFLCN(Y,2,D) !electricity
    BNCHFCT(Y,3,D) = (QSDSRS(D,Y)+QSKSRS(D,Y))-RSFLCN(Y,3,D) !distillate fuel oil & kerosene  !NoKero
    BNCHFCT(Y,4,D) = QSLGRS(D,Y)-RSFLCN(Y,4,D) !propane
  ENDDO

!STEO Benchmarking Years (includes MSEDYR+1 MER Historical Data)
ELSEIF ((Y.GT.MSEDYR).AND.(CurCalYr.LE.iLastSTEOYr)) THEN
  DO F=1,mNumFuel-1  !major fuels (natural gas, electricity, distillate fuel oil/kerosene, propane)  !NoKero
    DO D=1,mNumCR-2
      BNCHFCT(Y,F,D) = STEOCN(CURCALYR,F,D)-RSFLCN(Y,F,D)
    ENDDO
  ENDDO

!Post-STEO Period Factors
ELSEIF (CurCalYr.GT.iLastSTEOYr) THEN
  DO F=1,mNumFuel-1  !major fuels (natural gas, electricity, distillate fuel oil/kerosene, propane)
    DO D=1,mNumCR-2
      IF (F.LE.2) THEN
        !Post-STEO period benchmarking option for natural gas (F=1) and electricity (F=2)
        ! Setting BENCHALLYRS=0. turns off (and assumes UECs were adjusted for SEDS/MER)
        ! Setting BENCHALLYRS=1. turns on and keeps benchmarking factor for the remainder of the projection years
        BNCHFCT(Y,F,D)= BNCHFCTAVG(F,D)*BENCHALLYRS
        !Temporarily freeze benchmarking to MER year, with ability to tweak final totals from RSSTEO.txt for final benchmarking purposes
        IF (F.EQ.1 .AND. STEObenchNG.NE.1.0) BNCHFCT(Y,F,D) = BNCHFCT(LastSEDSyr+1-BaseYr+1,F,D)*STEObenchNG*BENCHALLYRS !natural gas
        IF (F.EQ.2 .AND. STEObenchEL.NE.1.0) BNCHFCT(Y,F,D) = BNCHFCT(LastSEDSyr+1-BaseYr+1,F,D)*STEObenchEL*BENCHALLYRS !electricity
      ELSE
        !Maintain STEO bench factors for other fuels
        BNCHFCT(Y,F,D)= BNCHFCTAVG(F,D)
      ENDIF !F.LE.2
    ENDDO !D
  ENDDO !F
ENDIF !CurCalYr (and Y)

!TAKE AVERAGE OF LAST 5 YEARS OF BNCHFCT FOR FIRST YEAR AFTER HISTORICAL DATA
! (OR THE NUMBER OF YEARS BETWEEN RECSYear AND MER YEAR)
IF (Y .EQ. MSEDYR+2) THEN  !MSED+2 is first year after MER
  DO D=1,mNumCR-2
    BNCHFCTAVG(1:(mNumFuel-1),D)= 0.0  !NoKero

    DO y4= 1, MIN(MSEDYR+1-(RECSYear-BaseYr+1),4)
      BNCHFCTAVG(1,D)=  BNCHFCTAVG(1,D) + BNCHFCT(Y-y4,1,D) !natural gas
      BNCHFCTAVG(2,D)=  BNCHFCTAVG(2,D) + BNCHFCT(Y-y4,2,D) !electricity
      BNCHFCTAVG(3,D)=  BNCHFCTAVG(3,D) + BNCHFCT(Y-y4,3,D) !distillat fuel oil & kerosene
      BNCHFCTAVG(4,D)=  BNCHFCTAVG(4,D) + BNCHFCT(Y-y4,4,D) !propane
    ENDDO

    BNCHFCTAVG(1,D)=  BNCHFCTAVG(1,D) / MIN((MSEDYR+1)-(RECSYear-BaseYr+1),5) !natural gas
    BNCHFCTAVG(2,D)=  BNCHFCTAVG(2,D) / MIN((MSEDYR+1)-(RECSYear-BaseYr+1),5) !electricity
    BNCHFCTAVG(3,D)=  BNCHFCTAVG(3,D) / MIN((MSEDYR+1)-(RECSYear-BaseYr+1),5) !distillat fuel oil & kerosene
    BNCHFCTAVG(4,D)=  BNCHFCTAVG(4,D) / MIN((MSEDYR+1)-(RECSYear-BaseYr+1),5) !propane
  ENDDO !D
ENDIF !Y

!END TEST YEAR TO DETERMINE BENCHMARKING TREATMENTS ^^^^

!Turn all post-RECS benchmarking off for testing purposes only
! This option maintains only the RECS benchmarking factors by setting all post-RECS years to those of the RECS year
IF ((GLOBALBENCHON.EQ.0) .AND. (CurCalYr.GT.RECSYear) .AND. (PRTDBGR.EQ.1)) THEN
  DO D=1,mNumCR-2
    DO F=1,mNumFuel-1  !major fuels (natural gas, electricity, distillate fuel oil/kerosene, propane)  !NoKero
      BNCHFCT(Y,F,D)= BNCHFCT(Y-1,F,D)
      WRITE(9,'("rsNObench Y F D ",3i5,f7.1)') y,f,d,bnchfct(y,f,d)
    ENDDO
  ENDDO
ENDIF

!Print benchmarking to RDM_OUT.txt (unit 9) for testing purposes (UNCOMMENT CODE BELOW TO ENABLE)
!IF ((GLOBALBENCHON.EQ.1) .AND. (PRTDBGR.EQ.1)) THEN
!  DO D=1,mNumCR-2
!    DO F=1,mNumFuel-1  !major fuels (natural gas, electricity, distillate fuel oil/kerosene, propane)  !NoKero
!      WRITE(9,'("rsbenchfactors Y F D ",3i5,f12.2)') y,f,d,bnchfct(y,f,d)
!    ENDDO
!  ENDDO
!ENDIF

!CALCULATE RSFLCN
DO D=1,mNumCR-2
  DO F=1,mNumFuel-1  !major fuels (natural gas, electricity, distillate fuel oil/kerosene, propane)  !NoKero
    !Additive benchmarking for Natural Gas, Electricity, Distillate Fuel Oil + Kerosene, and Propane
    RSFLCN(Y,F,D)=RSFLCN(Y,F,D)+BNCHFCT(Y,F,D)
  ENDDO
ENDDO

!BENCHMARK NATURAL GAS (F=1), DISTILLATE FUEL OIL + KEROSENE (F=3), AND PROPANE (F=4) USING SINGLE-FAMILY SPACE HEATING
!Natural gas
F=1
B=1 !Single-family
DO D=1,mNumCR-2
  HTRCON(Y,F,D)=HTRCON(Y,F,D)+BNCHFCT(Y,F,D)*1000000.
  HTRCONWT(Y,F,D,B)=HTRCONWT(Y,F,D,B)+BNCHFCT(Y,F,D)*1000000.
ENDDO

!Distillate fuel oil/kerosene (3) and propane (4)
B=1 !Single-family
DO D=1,mNumCR-2
  DO F=3,4	!TODO - replace 3,4 with parameters?
    HTRCON(Y,F,D)=HTRCON(Y,F,D)+BNCHFCT(Y,F,D)*1000000.
    !Don't allow weights to become negative (consumption could have also been capped, but is checked manually)
    IF (HTRCONWT(Y,F,D,B)+BNCHFCT(Y,F,D)*1000000. .GT. 0. .AND. HTRCONWT(Y,F,D,B) .GT. 0.) THEN
      HTRCONIN(Y,F,D,B)=(BNCHFCT(Y,F,D)*1000000.+HTRCONWT(Y,F,D,B))/HTRCONWT(Y,F,D,B)*HTRCONIN(Y,F,D,B)
      HTRCONWT(Y,F,D,B)=HTRCONWT(Y,F,D,B)+BNCHFCT(Y,F,D)*1000000.
    ELSE
      HTRCONIN(Y,F,D,B)= 0.
      HTRCONWT(Y,F,D,B)= 0.
    ENDIF
  ENDDO
ENDDO

!BENCHMARK ALL CENSUS DIVISIONS USING OTHER APPLIANCES
DO D=1,mNumCR-2
  B=1 !Single-family
  APCON(Y,D)=APCON(Y,D)+BNCHFCT(Y,2,D)*1000000.
  APCONWT(Y,D,B)=APCONWT(Y,D,B)+BNCHFCT(Y,2,D)*1000000.
  EAEQCN(Y,1,B,D)=EAEQCN(Y,1,B,D)+BNCHFCT(Y,2,D)*1000000.
ENDDO

!ADJUST NATURAL GAS USAGE FOR DISTRIBUTED GENERATION (FUEL CELLS)
! (ASSUMING FUEL CELLS PENETRATE WHERE GAS IS AVAILABLE USING WATER HEATING)
DO D=1,mNumCR-2
  H2OCON(Y,1,D)=H2OCON(Y,1,D)-HWBTU(Y,D,2)*1000000.+(GasUsage(Y,D,2)+GasUsage(Y,D,3))*1000000.0
  DO B=1,mNumBldg
    IF (B.EQ.1) THEN
      H2OCONWT(Y,1,D,B)=H2OCONWT(Y,1,D,B)-(HWBTU(Y,D,2)+GasUsage(Y,D,2))*1000000.0
      EQCEQCN(Y,18,B,D)=EQCEQCN(Y,18,B,D)-(HWBTU(Y,D,2)+GasUsage(Y,D,2))*1000000.0 !NG_WH  !EqpParam  !NoKero	!TODO - replace 18 with parameter read from RSCLASS
    ENDIF
  ENDDO
ENDDO

END SUBROUTINE RSBENCH


!==============================================================================
! RESIDENTIAL DETALED DATABASE OUTPUT SUBROUTINE
!==============================================================================
SUBROUTINE RESDBOUT
IMPLICIT NONE

!FOR REFERENCE; SET IN MODULE R_ AT START OF CODE
!INTEGER MaxApps, MaxTypes, MaxBins! These are used in looping of report variables
!PARAMETER (MaxApps=4)  !Maximum number of applications
!PARAMETER (MaxTypes=4) !Maximum number of bulb types within an application
!PARAMETER (MaxBins=6)  !Maximum number of bulb bins within an application

INTEGER app, bin
REAL*4 temp
REAL*4 bb
REAL*4 HEATERS(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR-2)
REAL*4 COOLERS(RECSYear:EndYr,nCoolClasses,mNumBldg,mNumCR-2)
REAL*4 WATERS(RECSYear:EndYr,nWatHtClasses,mNumBldg,mNumCR-2)
REAL*4 COOKS(RECSYear:EndYr,nCookClasses,mNumBldg,mNumCR-2)
REAL*4 DRYERS(RECSYear:EndYr,nClDryClasses,mNumBldg,mNumCR-2)
REAL*4 FRIGS(RECSYear:EndYr,mNumBldg,mNumCR-2)
REAL*4 FREEZE(RECSYear:EndYr,mNumBldg,mNumCR-2)
REAL*4 CLOTHE(RECSYear:EndYr,mNumBldg,mNumCR-2)
REAL*4 DISHW(RECSYear:EndYr,mNumBldg,mNumCR-2)

INTEGER W, Y, B, E, D, F, P, E2, Z, R,Y1
INTEGER ff(mNumFuel)  !pointers to benchmarking fuels from rtfuel (mNumFuel=5 in RTEK include file)  !EqpParam  !NoKero
INTEGER EU,RECCL,EQC,EQTYPE,TYPE,RECTY,S
INTEGER EQCGHP,EQCEHP,RECCLGHP,RECCLEHP,EQCSWH,EQCEWH,RECCLSWH,RECCLEWH
INTEGER FILE_MGR  !FILE MANAGER
INTEGER*4 OUTFILE  !FILE HANDLE
CHARACTER*18 FNAME
CHARACTER*3 FL
CHARACTER*4 SR  !end use and other output label
CHARACTER*8 EUNAME(9)	!TODO - replace 9 with parameter?
INTEGER DUM, zero
DATA ff/3,4,1,2,5/   ! pointers to benchmarking fuels from rtfuel; natural gas, electricity, distillate fuel oil/kerosene, propane, wood  !NoKero	!TODO - replace values with parameters?
DATA EUNAME/'HEAT','COOL','CWASH','DWASH','HOTWATER','COOK','DRYERS','FRIDG','FREEZE'/	!TODO - for some reason, these all print out to RESDEQP as 4 spaces and 4 characters despite being CHARACTER*8 and formatted as A8 in the write statement

zero=0

!****************************
!AGGREGATE EQUIPMENT
!****************************
!Set RECS-year equipment stocks equal to values from RSSTK.txt
DO D = 1, mNumCR - 2
  DO B = 1, mNumBldg
    DO EU = 1, 9	!TODO - replace 9 with parameter?
      DO RECCL = RTCLEUPT(EU) + 1, RTCLEUPT(EU + 1)
        EQC = RTCLEQCL(RECCL)
        SELECT CASE (EU)
            CASE (1)
              HEATERS(RECSYear, EQC, B, D) = EQCESE(RECSYear, RECCL, B, D)
            CASE (2)
              COOLERS(RECSYear, EQC, B, D) = EQCESE(RECSYear, RECCL, B, D)
            CASE (3)
              CLOTHE(RECSYear, B, D) = EQCESE(RECSYear, RECCL, B, D)
            CASE (4)
              DISHW(RECSYear, B, D) = EQCESE(RECSYear, RECCL, B, D)
            CASE (5)
              WATERS(RECSYear, EQC, B, D) = EQCESE(RECSYear, RECCL, B, D)
            CASE (6)
              COOKS(RECSYear, EQC, B, D) = EQCESE(RECSYear, RECCL, B, D)
            CASE (7)
              DRYERS(RECSYear, EQC, B, D) = EQCESE(RECSYear, RECCL, B, D)
            CASE (8)
              FRIGS(RECSYear, B, D) = EQCESE(RECSYear, RECCL, B, D)
            CASE (9)
              FREEZE(RECSYear, B, D) = EQCESE(RECSYear, RECCL, B, D)
        END SELECT
      ENDDO
    ENDDO
  ENDDO
ENDDO

!Aggreggate equipment stocks in projection years
DO Y = RECSYear + 1, EndYr
  DO D = 1, mNumCR - 2
    DO B = 1, mNumBldg
      DO EU = 1, 9	!TODO - replace 9 with parameter?
        DO RECCL = RTCLEUPT(EU) + 1, RTCLEUPT(EU + 1)
          EQC = RTCLEQCL(RECCL)

          !Sum all equipment vintages and temporarily store
          temp = EQCESE(Y, RECCL, B, D) + EQCRP90(Y, RECCL, B, D) + &
                 EQCRP90RP(Y, RECCL, B, D) + EQCSR90(Y, RECCL, B, D) + &
                 EQCADD(Y, RECCL, B, D) + EQCREP(Y, RECCL, B, D) + &
                 EQCSUR(Y, RECCL, B, D)

          !Assign the temporary calculated sum to the appropriate end use number
          SELECT CASE (EU)
            CASE (1)
              HEATERS(Y, EQC, B, D) = temp
            CASE (2)
              COOLERS(Y, EQC, B, D) = temp
            CASE (3)
              CLOTHE(Y, B, D) = temp
            CASE (4)
              DISHW(Y, B, D) = temp
            CASE (5)
              WATERS(Y, EQC, B, D) = temp
            CASE (6)
              COOKS(Y, EQC, B, D) = temp
            CASE (7)
              DRYERS(Y, EQC, B, D) = temp
            CASE (8)
              FRIGS(Y, B, D) = temp
            CASE (9)
              FREEZE(Y, B, D) = temp
          END SELECT
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

!*******************************************************************
!CALCULATE EQUIPMENT CONSUMPTION FOR RECSYear
!*******************************************************************
!Initialize heat pump class markers
RECCLGHP = 0
RECCLEHP = 0
EQCGHP = 0
EQCEHP = 0

!Search for space heating (EU=1) heat pump records
EU = 1
DO RECCL = RTCLEUPT(EU) + 1, RTCLEUPT(EU+1)
  IF (TRIM(RTCLNAME(RECCL)) == 'GEO_HP') RECCLGHP = RECCL
  IF (TRIM(RTCLNAME(RECCL)) == 'ELEC_HP') RECCLEHP = RECCL
ENDDO

!Search for space cooling (EU=2) heat pump records
EU = 2
DO RECCL = RTCLEUPT(EU) + 1, RTCLEUPT(EU+1)
  IF (TRIM(RTCLNAME(RECCL)) == 'GEO_HP') EQCGHP = RECCL
  IF (TRIM(RTCLNAME(RECCL)) == 'ELEC_HP') EQCEHP = RECCL
ENDDO

!Checks to ensure heat pump class markers were found
IF (RECCLGHP == 0 .OR. RECCLEHP == 0 .OR. EQCGHP == 0 .OR. EQCEHP == 0) THEN
  WRITE(9, '("RES_OUT.txt WARNING: Could not find required heat pump record numbers.", 2i5)') EU, RECCL
ENDIF

!Major end-use equipment consumption
DO D = 1, mNumCR - 2
  DO B = 1, mNumBldg
    DO EU = 1, 9	!TODO - replace 9 with parameter?
      DO RECCL = RTCLEUPT(EU) + 1, RTCLEUPT(EU + 1)
        EQCEQCN(RECSYear-BaseYr+1, RECCL, B, D) = EQCESE(RECSYear, RECCL, B, D) * EQCUEC(D, RECCL, B)
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    !Other appliances (natural gas, propane, distillate fuel oil & kerosene)
    DO E=1,3	!TODO - replace 3 with parameter?
      APEQCN(RECSYear-BaseYr+1,E,B,D)=APPEQP(RECSYear,B,D,E)*APPUEC(D,E,B)
    ENDDO

    !MELs consumption	!TODO - when streamlining MELs inputs, revise variables so arrays are in the same order
    TVSEQCN(RECSYear-BaseYr+1,1,B,D)=TVSEQP(RECSYear,B,D)*TVSUEC(D,B)
    STBEQCN(RECSYear-BaseYr+1,1,B,D)=STBEQP(RECSYear,B,D)*STBUEC(D,B)
    HTSEQCN(RECSYear-BaseYr+1,1,B,D)=HTSEQP(RECSYear,B,D)*HTSUEC(D,B)
    OTTEQCN(RECSYear-BaseYr+1,1,B,D)=OTTEQP(RECSYear,B,D)*OTTUEC(D,B)
    VGCEQCN(RECSYear-BaseYr+1,1,B,D)=VGCEQP(RECSYear,B,D)*VGCUEC(D,B)
    DPCEQCN(RECSYear-BaseYr+1,1,B,D)=DPCEQP(RECSYear,B,D)*DPCUEC(D,B)
    LPCEQCN(RECSYear-BaseYr+1,1,B,D)=LPCEQP(RECSYear,B,D)*LPCUEC(D,B)
    MONEQCN(RECSYear-BaseYr+1,1,B,D)=MONEQP(RECSYear,B,D)*MONUEC(D,B)
    NETEQCN(RECSYear-BaseYr+1,1,B,D)=NETEQP(RECSYear,B,D)*NETUEC(D,B)
    BATEQCN(RECSYear-BaseYr+1,1,B,D)=BATEQP(RECSYear,B,D)*BATUEC(D,B)
    CFNEQCN(RECSYear-BaseYr+1,1,B,D)=CFNEQP(RECSYear,B,D)*CFNUEC(D,B)
    COFEQCN(RECSYear-BaseYr+1,1,B,D)=COFEQP(RECSYear,B,D)*COFUEC(D,B)
    DEHEQCN(RECSYear-BaseYr+1,1,B,D)=DEHEQP(RECSYear,B,D)*DEHUEC(D,B)
    MCOEQCN(RECSYear-BaseYr+1,1,B,D)=MCOEQP(RECSYear,B,D)*MCOUEC(D,B)
    PLPEQCN(RECSYear-BaseYr+1,1,B,D)=PLPEQP(RECSYear,B,D)*PLPUEC(D,B)
    PLHEQCN(RECSYear-BaseYr+1,1,B,D)=PLHEQP(RECSYear,B,D)*PLHUEC(D,B)
    SECEQCN(RECSYear-BaseYr+1,1,B,D)=SECEQP(RECSYear,B,D)*SECUEC(D,B)
    SPAEQCN(RECSYear-BaseYr+1,1,B,D)=SPAEQP(RECSYear,B,D)*SPAUEC(D,B)
    WCLEQCN(RECSYear-BaseYr+1,1,B,D)=WCLEQP(RECSYear,B,D)*WCLUEC(D,B)
    SPKEQCN(RECSYear-BaseYr+1,1,B,D)=SPKEQP(RECSYear,B,D)*SPKUEC(D,B)
    PHNEQCN(RECSYear-BaseYr+1,1,B,D)=PHNEQP(RECSYear,B,D)*PHNUEC(D,B)
    TABEQCN(RECSYear-BaseYr+1,1,B,D)=TABEQP(RECSYear,B,D)*TABUEC(D,B)
    KITEQCN(RECSYear-BaseYr+1,1,B,D)=KITEQP(RECSYear,B,D)*KITUEC(D,B)
  ENDDO !B
ENDDO !D

!TODO - move format statements closer to their use
984  FORMAT(A4,',',I2,',',I2,',',A2,',',A8,',',I4,',',F12.0,',',I13,',',I3,',',A5)
985  FORMAT(A7,A5,A5,A5,A9,A5,A8,A12,A11,A8)
986  FORMAT(A7,A5,A5,A5,A9,A8,A5,A7,A6,A12,A12,6A10,A12,A13,A11,A13,A9,10a13)
987  FORMAT(A,',',I4,',',I4,',',A2,',',A8,',',A10,',',I4,2(',',F9.4),4(',',F12.0),19(',',I11))
988  FORMAT(1X,A8,1X,I4,1X,I4,1X,A2,1X,A8,1X,A8,1X,I4,2(1X,I11))
989  FORMAT(A4,',',I2,',',I2,',',A2,',',A8,',',I4,',',F12.0,',',I13,',',A5)
990  FORMAT(A8,',',I4,',',I4,',',A2,',',A8,',',A8,',',I4,2(',',F9.4),11(',',f11.0))
991  FORMAT(A4,',',I2,',',I2,',',A2,',',A8,',',I4,',',F12.0,',',I13,',',I13)
992  FORMAT(A4, ',', 4(I2,','), I4, ',', F12.8, ',', I2, ',', I2)
998  FORMAT(A4,',',I2,',',A2,',',A2,',',A8,',',I4,',',F12.2,',',I13)
999  FORMAT(1X,A2,1X,I4,1X,A2,1X,A3,1X,I4,6(1X,F10.3))	!TODO - unused?

!********************************************************************
!WRITE RESIDENTIAL EQUIPMENT PURCHASES, EFFICIENCY, SUBSIDIES, AND SHELL OUTPUTS TO RDM_EQPOUT.TXT
!********************************************************************
OUTFILE=FILE_MGR('O','RESDEQP',.TRUE.)

WRITE(OUTFILE,986) 'ENDUSE,','CDIV,','BLDG,','FUEL,','EQPCLASS,','EQPTYPE,', &
      'YEAR,','NEWEFF,','EXEFF,','NEWPURCHASE,','REPPURCHASE,',&
      'NEWINVEST,','REPINVEST,','NEWFEDSUB,','REPFEDSUB,','NEWNFDSUB,','REPNFDSUB,', &
      'NON_IECC09,','IECC2009,','ESTAR,','IECC+40%,','BEST,', &
      'I_NON_IECC09,','I_IECC2009,','I_ESTAR,','I_IECC+40%,','I_BEST,', &
      'S_NON_IECC09,','S_IECC2009,','S_ESTAR,','S_IECC+40%,','S_BEST'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      DO EU = 1,9	!TODO - replace 9 with parameter (adjust mNumEndU in RTEK?)
        SR=EUNAME(EU)
        TYPE = RTTYPECT(EU)
        DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
          !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
          IF ((Y.GE.RTINITYR(RECTY).AND. Y.LE.RTLASTYR(RECTY)).AND.(RTCENDIV(RECTY).EQ.D)) THEN
            TYPE = TYPE + 1
            EQTYPE=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
            F=RTFUEL(RECCL)
            E=RTCLEQCL(RECCL)
            Y1=Y-BaseYr+1

            !FIND THE RECORD NUMBER FOR THE GROUND-SOURCE HEAT PUMP FOR HEATING
            IF (RTCLNAME(RECCL).EQ.'GEO_HP') RECCLGHP=RECCL

            !DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
            IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN	!TODO - base on RTFUEL instead?
              FL='WD'
            ELSEIF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ELSEIF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSEIF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSEIF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ENDIF

            IF (EU.EQ.1) THEN !Calculate shell investment and subsidies with heating system records only
              WRITE(OUTFILE,987) SR,D,B,FL,RTCLNAME(RECCL),RTTYNAME(RECTY),Y , RSNEFDB1(Y1,RECCL,B,D), RSEEFDB1(Y1,RECCL,B,D),&
               HEATINGTYPEPURCH(Y,TYPE,B,D,1),HEATINGTYPEPURCH(Y,TYPE,B,D,2),&
               HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtrecost(recty)+rtresub(recty)+rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty)),   & !Investment excludes installation costs, so use rtresub for investment only
               HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rtrecost(recty)+rtresub(recty)+rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty)),   & !Investment excludes installation costs for retrofit equipment because of way heat pump costs are split between heating & cooling (and replacement versus new)
               INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*rtresub(recty)), &  ! New federal subsidy
               INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*rteqsub(recty)), &  ! Replacement federal subsidy
               INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))), & !New non-federal subsidy
               INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rteqsubn(recty)+FLOAT(EPA111D)*RTEQSUB111D(recty))), & !Replacement non-federal subsidy
               (INT(SHELLBUILDS(Y,EQTYPE,S,B,D)),S=1,nShellTypes), &
               (INT((shellinvest(y,RECCL,s,b,d)+shellsubsidy (y,RECCL,s,b,d))*nint(shellbuilds(Y,EQTYPE,S,B,D))),S=1,nShellTypes), &  !add the subsidy back in for investment includes 111D subsidies
               (INT(shellsubsidy(y,RECCL,s,b,d)*nint(shellbuilds(Y,EQTYPE,S,B,D))),S=1,nShellTypes)
            ELSE  !other equipment
              WRITE(OUTFILE,987) SR,D,B,FL,RTCLNAME(RECCL),RTTYNAME(RECTY),Y , RSNEFDB1(Y1,RECCL,B,D), RSEEFDB1(Y1,RECCL,B,D),&
               HEATINGTYPEPURCH(Y,TYPE,B,D,1),HEATINGTYPEPURCH(Y,TYPE,B,D,2),&
               HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtrecost(recty)+rtresub(recty)+rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty)),   & !Add back in the subsidy, which was taken out earlier ((..1) is new construction (..2) replacements)
               HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rtrecost(recty)+rtresub(recty)+rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty)),   & !Investment excludes installation costs for retrofit equipment because of way heat pump costs are split between heating & cooling (and replacement versus new)
               INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*rtresub(recty)),&                    !New federal subsidy. For subsidies, account for all components - here new construction, just capital subsidies
               INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*rteqsub(recty)),&                    !Replacement federal subsidy. For replacement purchases, include rteqsub which includes subsidies for total installed costs (including labor)
               INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))),&  !New non-federal subsidy
               INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rteqsubn(recty)+FLOAT(EPA111D)*RTEQSUB111D(recty))),&  !Replacement non-federal subsidy
               0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0 !column placeholders in output file
            ENDIF
          ENDIF !Year and census division filter
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO app=1,NumApps
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      DO E=1,numtypes(app)
        DO Y=RECSYear+1,LastYr+BaseYr-1
          WRITE(OUTFILE,990) 'LIGHTING',D,B,'EL',AppID(app),appbulbname(app,e),Y,WTLEFFbyAPP(app,Y,B,D),WTLEFFbyAPP(app,Y,B,D),(LTNEEDEDbyAPP(app,Y,E,B,D)), &
           (LTREPbyAPP(app,Y,E,B,D)),LTInvest(app,Y,E,B,D,1),LTInvest(app,Y,E,B,D,2),0,0,LTsubsidy(app,Y,E,B,D,1),LTsubsidy(app,Y,E,B,D,2),0,0,0
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

OUTFILE=FILE_MGR('C','RESDEQP',.FALSE.) !Close the RDM_EQPOUT.txt output file

!********************************************************************
!WRITE RESIDENTIAL END-USE CONSUMPTION, EQUIPMENT STOCK, HOUSEHOLD,
! FUEL SWITCHING, SHELL EFFICIENCY, AND FUEL PRICE OUTPUTS TO RDM_DBOUT.TXT
!********************************************************************
FNAME='RDM_DBOUT.TXT' !Unit 23

OPEN(23,FILE=FNAME,FORM='FORMATTED')

!SPACE HEATING
EU = 1
SR='HT'

WRITE(23,985) 'ENDUSE,','CDIV,','BLDG,','FUEL,','EQPCLASS,','YEAR,','EQSTOCK,','CONSUMPTION,','HOUSEHOLDS,','BULBTYPE'
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      E=RTCLEQCL(RECCL)
      !FIND EQUIPMENT CLASS FOR GROUND-SOURCE HEAT PUMPS	!TODO - still needed if no longer estimating geothermal energy consumption?
      IF (RTCLNAME(RECCL).EQ.'GEO_HP') RECCLGHP=RECCL
        !DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
        IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN	!TODO - why isn't this based on "RTFUEL(RECCL).EQ.5" similar to other fuels? Because wood isn't used for all end uses?
          FL='WD'
        ELSEIF (RTFUEL(RECCL).EQ.1) THEN
          FL='DS'
        ELSEIF (RTFUEL(RECCL).EQ.2) THEN
          FL='LG'
        ELSEIF (RTFUEL(RECCL).EQ.3) THEN
          FL='GS'
        ELSEIF (RTFUEL(RECCL).EQ.4) THEN
          FL='EL'
      ENDIF

      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        bb=0.
        IF (RECCL.EQ.1) THEN  !Writes number of households when processing first space heating equipment class (ELEC_RAD)
          WRITE(23,991) SR,D,B,FL,RTCLNAME(RECCL),Y,HEATERS(Y,E,B,D),INT(EQCEQCN(Y1,RECCL,B,D)),INT(EH(Y,B,D)+NH(Y,B,D))
        ELSE
          IF ((RECCL.EQ.3).AND.(B.EQ.1)) bb=BNCHFCT(Y1,1,D)*1000000.  !NG_FA
          IF ((RECCL.EQ.5).AND.(B.EQ.1)) bb=BNCHFCT(Y1,4,D)*1000000.  !LPG_FA  !NoKero
          IF ((RECCL.EQ.6).AND.(B.EQ.1)) bb=BNCHFCT(Y1,3,D)*1000000.  !DIST_FA  !NoKero
          IF (bb+EQCEQCN(Y1,RECCL,B,D).LE.0.) THEN !Set negative consumption equal to zero
            bb=0.
            EQCEQCN(Y1,RECCL,B,D)=0.
          ENDIF
          WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,HEATERS(Y,E,B,D),INT(bb+EQCEQCN(Y1,RECCL,B,D))
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

!SPACE COOLING
EU = 2
SR='CL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      E=RTCLEQCL(RECCL)
      !FIND EQUIPMENT CLASS FOR GROUND-SOURCE HEAT PUMPS	!TODO - still needed if no longer estimating geothermal energy consumption?
      IF (RTCLNAME(RECCL).EQ.'GEO_HP') THEN
        EQCGHP=RTCLEQCL(RECCL)
        RECCLGHP=EQCGHP+RTCLEUPT(EU)
      ENDIF

      !DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
      IF (RTFUEL(RECCL).EQ.1) THEN
        FL='DS'
      ELSEIF (RTFUEL(RECCL).EQ.2) THEN
        FL='LG'
      ELSEIF (RTFUEL(RECCL).EQ.3) THEN
        FL='GS'
      ELSEIF (RTFUEL(RECCL).EQ.4) THEN
        FL='EL'
      ELSEIF (RTFUEL(RECCL).EQ.5) THEN
        FL='WD'
      ENDIF

      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,COOLERS(Y,E,B,D),INT(EQCEQCN(Y1,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

!CLOTHES WASHING
EU = 3
SR='CW'
FL='EL'

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,CLOTHE(Y,B,D),INT(EQCEQCN(Y1,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

!DISHWASHING
EU = 4
SR='DW'
FL='EL'

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      DO Y=RECSYear,LastYr+BaseYr-1	!TODO - verify; LastYr+BaseYr-1 had been EndYr
        Y1=Y-BaseYr+1
        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,DISHW(Y,B,D),INT(EQCEQCN(Y1,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

!WATER HEATING
EU = 5
SR='HW'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      E=RTCLEQCL(RECCL)
      IF (RTFUEL(RECCL).EQ.1) THEN
        FL='DS'
      ELSEIF (RTFUEL(RECCL).EQ.2) THEN
        FL='LG'
      ELSEIF (RTFUEL(RECCL).EQ.3) THEN
        FL='GS'
      ELSEIF (RTFUEL(RECCL).EQ.4) THEN
        FL='EL'
      ELSEIF (RTFUEL(RECCL).EQ.5) THEN	!TODO - RTFUEL(RECCL) is wood for other end uses
        FL='SL'
      ENDIF

      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,WATERS(Y,E,B,D),INT(EQCEQCN(Y1,RECCL,B,D))
      ENDDO
    ENDDO

    FL='SL'
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'SOL',Y,WATERS(Y,5,B,D),INT(SLEQCN(Y1,1,B,D))  !Thermal energy used by SOLAR_WH	!TODO - incorporate as an IF in WRITE row above so FL='SL' and SLEQCN=EQCEQCN
    ENDDO
  ENDDO
ENDDO

!COOKING
EU = 6
SR='CK'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      E=RTCLEQCL(RECCL)
      IF (RTFUEL(RECCL).EQ.1) THEN	!TODO - Cooking only includes NG_STV, LPG_STV, and ELEC_STV; remove FL='DS'?
        FL='DS'
      ELSEIF (RTFUEL(RECCL).EQ.2) THEN
        FL='LG'
      ELSEIF (RTFUEL(RECCL).EQ.3) THEN
        FL='GS'
      ELSEIF (RTFUEL(RECCL).EQ.4) THEN
        FL='EL'
      ENDIF

      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,COOKS(Y,E,B,D),int(EQCEQCN(Y1,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

!CLOTHES DRYING
EU = 7
SR='DR'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      E=RTCLEQCL(RECCL)
      IF (RTFUEL(RECCL).EQ.1) THEN	!TODO - Clothes drying only includes NG_DRY and ELEC_DRY; remove FL='DS','LG'?
        FL='DS'
      ELSEIF (RTFUEL(RECCL).EQ.2) THEN
        FL='LG'
      ELSEIF (RTFUEL(RECCL).EQ.3) THEN
        FL='GS'
      ELSEIF (RTFUEL(RECCL).EQ.4) THEN
        FL='EL'
      ENDIF

      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,DRYERS(Y,E,B,D),int(EQCEQCN(Y1,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

!REFRIGERATION
EU = 8
SR='RF'
FL='EL'

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,FRIGS(Y,B,D),int(EQCEQCN(Y1,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

!FREEZING
EU = 9
SR='FZ'
FL='EL'

DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,FREEZE(Y,B,D),int(EQCEQCN(Y1,RECCL,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

!LIGHTING (all applications)
SR='LT'
FL='EL'

DO app =1,NumApps
  !Adding this loop across types splits lighting applications into bulb types for stocks
  !Also need to accumulate into temp below to split by types
  DO type =1,numtypes(app)
    DO D=1,mNumCR-2
      DO B=1,mNumBldg
        DO Y=RECSYear,LastYr+BaseYr-1
          Y1=Y-BaseYr+1
          TEMP=0.
          DO BIN=1,MAXBINS
            TEMP=TEMP+ltstock(app,y,type,B,D,BIN)
          ENDDO
          IF (type.EQ.1) THEN
            WRITE(23,984) SR,D,B,FL,appid(app),Y,TEMP,int(LTEQCN(Y1,app,B,D)),0,appbulbname(app,type)
          ELSE
            WRITE(23,984) SR,D,B,FL,appid(app),Y,TEMP,0,0,appbulbname(app,type)
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

!FURNACE FANS & BOILER PUMPS
SR='FF'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,SR,Y,FANEQP(Y,B,D),INT(FANEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!TELEVISIONS
SR='TVS'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'TV&R',Y,TVSEQP(Y,B,D),INT(TVSEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!SET-TOP BOXES
SR='STB'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'TV&R',Y,STBEQP(Y,B,D),INT(STBEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!HOME THEATER SYSTEMS
SR='HTS'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'TV&R',Y,HTSEQP(Y,B,D),INT(HTSEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!OVER-THE-TOP STREAMING DEVICES
SR='OTT'
FL='EL'
DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'TV&R',Y,OTTEQP(Y,B,D),INT(OTTEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!VIDEO GAME CONSOLES
SR='VGC'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'TV&R',Y,VGCEQP(Y,B,D),INT(VGCEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!DESKTOP PCs
SR='DPC'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'PC&R',Y,DPCEQP(Y,B,D),INT(DPCEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!LAPTOP PCs
SR='LPC'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'PC&R',Y,LPCEQP(Y,B,D),INT(LPCEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!COMPUTER MONITORS
SR='MON'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'PC&R',Y,MONEQP(Y,B,D),INT(MONEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!NETWORKING EQUIPMENT
SR='NET'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'PC&R',Y,NETEQP(Y,B,D),INT(NETEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!NON-PC RECHARGEABLES
SR='BAT'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, BATEQP(Y,B,D),INT(BATEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!CEILING FANS
SR='CFN'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, CFNEQP(Y,B,D),INT(CFNEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!COFFEE MAKERS
SR='COF'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, COFEQP(Y,B,D),INT(COFEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!DEHUMIDIFIERS
SR='DEH'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, DEHEQP(Y,B,D),INT(DEHEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!MICROWAVES
SR='MCO'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, MCOEQP(Y,B,D),INT(MCOEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!POOL PUMPS
SR='PLP'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, PLPEQP(Y,B,D),INT(PLPEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!POOL HEATERS
SR='PLH'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, PLHEQP(Y,B,D),INT(PLHEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!SECURITY SYSTEMS
SR='SEC'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, SECEQP(Y,B,D),INT(SECEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!PORTABLE ELECTRIC SPAS
SR='SPA'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, SPAEQP(Y,B,D),INT(SPAEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!WINE COOLERS
SR='WCL'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, WCLEQP(Y,B,D),INT(WCLEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!SMART SPEAKERS
SR='SPK'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, SPKEQP(Y,B,D),INT(SPKEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!SMARTPHONES
SR='PHN'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, PHNEQP(Y,B,D),INT(PHNEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!TABLETS
SR='TAB'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, TABEQP(Y,B,D),INT(TABEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!SMALL KITCHEN APPLIANCES
SR='KIT'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      WRITE(23,989) SR,D,B,FL,'MEL',Y, KITEQP(Y,B,D),INT(KITEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!OTHER ELECTRIC APPLIANCES NOT EXPLICITLY MODELED
SR='EO'
FL='EL'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      Y1=Y-BaseYr+1
      bb=0.0
      WRITE(23,989) SR,D,B,FL,'MEL',Y,EAEQP(Y,B,D),INT(EAEQCN(Y1,1,B,D))
    ENDDO
  ENDDO
ENDDO

!OTHER APPLIANCES IN FUELS OTHER THAN ELECTRICITY
SR='OA'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO E=1,3	!TODO - replace 3 with parameter?
      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        IF (E .EQ. 1) THEN
          FL='GS'
          f=1
          bb=0.0
        ELSEIF (E .EQ. 2) THEN
          FL='LG'
          f=4
          bb=0.0
        ELSEIF (E .EQ. 3) THEN
          FL='DS'
          f=3
          bb=0.0
        ENDIF
        WRITE(23,989) SR,D,B,FL,FL,Y,APLEQP(Y,B,D,E),INT(bb+APEQCN(Y1,E,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

!SECONDARY SPACE HEATING
SR='SH'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO E=1,3	!TODO - verify that this split in equipment types is correct/necessary; wood is the only minor fuel in secondary space heating
      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        IF (E .EQ. 1) THEN
          FL='GS'
          bb=0.0 ! bnchfct(y1,e,d)
        ELSEIF (E .EQ. 2) THEN
          FL='EL'
          bb=0.0 ! bnchfct(y1,e,d)
        ELSEIF (E .EQ. 3) THEN
          FL='DS'
          bb=0.0
        ENDIF
        WRITE(23,989) SR,D,B,FL,FL,Y,SHTEQP(Y,B,D,E),INT(bb+sheQCN(Y1,E,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO E=4,5	!TODO - verify that this split in equipment types is correct/necessary; wood is the only minor fuel in secondary space heating
      DO Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        IF (E .EQ. 4) THEN
          FL='LG'
          bb=1.0 ! bnchfct(y1,e,d)
        ELSEIF (E .EQ. 5) THEN  !NoKero
          FL='WD'
          bb=1.0
        ENDIF
        WRITE(23,989) SR,D,B,FL,FL,Y,SHTEQP(Y,B,D,E),INT(bb*sheQCN(Y1,E,B,D))
      ENDDO
    ENDDO
  ENDDO
ENDDO

!HOUSING STARTS
SR='HS'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear+1,LastYr+BaseYr-1
      WRITE(23,989) SR,D,B,'0','0',Y,HSEADD(Y,B,D),zero
    ENDDO
  ENDDO
ENDDO

!SQUARE FOOTAGE
! adjust database output
SR='SQ'

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO Y=RECSYear,LastYr+BaseYr-1
      WRITE(23,991) SR,D,B,'0','0',Y,STOCKSQRFOOT(Y,B,D),INT(SQRFOOT(Y,B,D)),INT(((EH(Y,B,D)+NH(Y,B,D))*STOCKSQRFOOT(Y,B,D))/10**6)
    ENDDO
  ENDDO
ENDDO

!FUEL SWITCHING TO TECHNOLOGIES
EU = 1
SR='ST'

DO D=1,mNumCR-2
  DO B=1,1	!TODO - does this need to have a range of 1,1?
    DO Y=RECSYear+1,LastYr+BaseYr-1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        E=RTCLEQCL(RECCL)
        !DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
        IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN	!TODO - test for RTFUEL instead?
          FL='WD'
        ELSEIF (RTFUEL(RECCL).EQ.1) THEN
          FL='DS'
        ELSEIF (RTFUEL(RECCL).EQ.2) THEN
          FL='LG'
        ELSEIF (RTFUEL(RECCL).EQ.3) THEN
          FL='GS'
        ELSEIF (RTFUEL(RECCL).EQ.4) THEN
          FL='EL'
        ENDIF
        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,SWTOTAL(Y,E,D),zero
      ENDDO
    ENDDO
  ENDDO
ENDDO

!FUEL SWITCHING FROM TECHNOLOGIES
EU = 1
SR='SF'

DO D=1,mNumCR-2
  DO B=1,1	!TODO - does this need to have a range of 1,1?
    DO Y=RECSYear+1,LastYr+BaseYr-1
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        E=RTCLEQCL(RECCL)
        !DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
        IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN	!TODO - test for RTFUEL instead?
          FL='WD'
        ELSEIF (RTFUEL(RECCL).EQ.1) THEN
          FL='DS'
        ELSEIF (RTFUEL(RECCL).EQ.2) THEN
          FL='LG'
        ELSEIF (RTFUEL(RECCL).EQ.3) THEN
          FL='GS'
        ELSEIF (RTFUEL(RECCL).EQ.4) THEN
          FL='EL'
        ENDIF
        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,SWFTOTAL(Y,E,D),zero
      ENDDO
    ENDDO
  ENDDO
ENDDO

!FUEL PRICES
! WRITE NEMS PRICES IN INTERNAL NEMS DOLLAR YEAR (1987$/MMBTU); VALUES FOR mNumCR=10 (CALIFORNIA PLACEHOLDER) CAN BE IGNORED
SR='FP'

DO D=1,mNumCR
  DO Y=1,LastYr
    WRITE(23,998) SR,D,'1','DS','0',Y+BaseYr-1,PDSRS(D,Y),zero
    WRITE(23,998) SR,D,'1','LG','0',Y+BaseYr-1,PLGRS(D,Y),zero
    WRITE(23,998) SR,D,'1','NG','0',Y+BaseYr-1,PNGRS(D,Y),zero
    WRITE(23,998) SR,D,'1','EL','0',Y+BaseYr-1,PELRS(D,Y),zero
  ENDDO
ENDDO

!RESIDENTIAL SHELL INDICES
!space heating indices
DO Y=RECSYear-BaseYr+1,LastYr
  WRITE(23,992) 'HSHE',11,1,0,0,Y+BaseYr-1,HSHELL1(Y),0,0  !existing
  WRITE(23,992) 'HSHN',11,1,0,0,Y+BaseYr-1,HSHELL2(Y),0,0  !new construction
  WRITE(23,992) 'HSHA',11,1,0,0,Y+BaseYr-1,HSHELL3(Y),0,0  !average
ENDDO

!space cooling indices
DO Y=RECSYear-BaseYr+1,LastYr
  WRITE(23,992) 'CSHE',11,1,0,0,Y+BaseYr-1,CSHELL1(Y),0,0  !existing
  WRITE(23,992) 'CSHN',11,1,0,0,Y+BaseYr-1,CSHELL2(Y),0,0  !new construction
  WRITE(23,992) 'CSHA',11,1,0,0,Y+BaseYr-1,CSHELL3(Y),0,0  !average
ENDDO

CLOSE(23) !Unit 23 = RDM_DBOUT.TXT

END SUBROUTINE RESDBOUT


!==============================================================================
! EPA 111D ENERGY EFFICIENCY REBATES SUBSIDY CALCULATION SUBROUTINE - DEPRECATED
!==============================================================================
SUBROUTINE CALC111D
IMPLICIT NONE

!EPA111D=1 is scedes switch to turn national utility energy efficiency subsidies and modeling on (primarily for modeling EPA's Clean Power Plan)
!AB32SW=1 is scedes switch to turn CA energy efficiency modeling on

!INTEGER MaxApps, MaxTypes, MaxBins! These are used in looping of report variables - These are set near top of code
!PARAMETER (MaxApps=4)  !Maximum number of applications
!PARAMETER (MaxTypes=4) !Maximum number of bulb types within an application
INTEGER app
INTEGER Y, B, E, D, F, Y1, NT
INTEGER EU,RECCL,EQC,EQTYPE,TYPE,RECTY,S

!QELRS in trillion Btu (rather than quadrillion Btu), BASELINEBKWH in billions kWh
!BASELINEBKWH(d,y)=(QELRS(D,Y)/3412.)*10**3

Y=CurCalYr
!Y1=Y-BaseYr+1
Y1=CurIYr

IF (CurCalYr .LT. 2017) THEN  !comment this block out out so savings can be calculated in years prior to 2017 for EE runs?	!TODO - should the 2017 be changed?
  DO D=1,mNumCR-2
    SAVE111RES(D,Y1)=0. !Residential sector savings in billions of kilowatthours
    COST111RES(D,Y1)=0. !Residential costs billions of 1987$
  ENDDO
  RETURN  !DON'T RETURN DURING TESTING MODE
ENDIF

!Calculate savings and initialize cost to zero
DO D=1,mNumCR-2
  SAVE111RES(D,Y1)=BASELINEBKWH(D,Y1)-((RSFLCN(Y1,2,D)-(TrillsOwnUse(Y1,D,1)+TrillsOwnUse(Y1,D,2)+TrillsOwnUse(Y1,D,3)+TrillsOwnUse(Y1,D,4))/1000.)/3412.)*10**3  !Subtract all onsite own-use generation; BASELINEBKWH converted from QELRS (quads purchased electricity from grid)  !BESSmodel
  COST111RES(D,Y1)=0.
ENDDO

DO D=1,mNumCR-2
  DO B=1,mNumBldg
    DO EU = 1,9	!TODO - replace 9 with parameter (adjust mNumEndU in RTEK?)
      TYPE = RTTYPECT(EU)
      DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
        !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
        IF ((Y.GE.RTINITYR(RECTY).AND. &
         Y.LE.RTLASTYR(RECTY)).AND.(RTCENDIV(RECTY).EQ.D)) THEN
          TYPE = TYPE + 1
          EQTYPE=RTEQTYPE(RECTY)
          EQC=RTTYEQCL(RECTY)
          RECCL=RTCLEUPT(EU)+EQC
          IF (RTFUEL(RECCL).EQ.4) THEN  !electricity subsidies only
            IF (EU.EQ.1) THEN !Calculate shell investment and subsidies with heating system records only
              COST111RES(D,Y1)=COST111RES(D,Y1) + FLOAT(EPA111D) * &          !costs only accumulate in CPP runs
               ((HEATINGTYPEPURCH(Y,TYPE,B,D,1)*rtresub111D(recty)) + &       !new construction subsidies (..1) are from equipment only
               (HEATINGTYPEPURCH(Y,TYPE,B,D,2)*rteqsub111D(recty)))         !replacement purchases (..2) include subsidy for installation costs
              DO S=1,nShellTypes
                COST111RES(D,Y1)=COST111RES(D,Y1) + FLOAT(EPA111D)*&   !costs only accumulate in CPP runs
                shellsubsidy111D(y,RECCL,s,b,d)*(shellbuilds(Y,TYPE,S,B,D))
              ENDDO
            ELSE  !other equipment
              COST111RES(D,Y1)=COST111RES(D,Y1) + FLOAT(EPA111D) * &          !costs only accumulate in CPP runs
               ((HEATINGTYPEPURCH(Y,TYPE,B,D,1)*rtresub111D(recty)) + &
               (HEATINGTYPEPURCH(Y,TYPE,B,D,2)*rteqsub111D(recty)))
            ENDIF
          ENDIF  ! fuel is electricity
        ENDIF !Year and Census division filter
      ENDDO
	ENDDO
  ENDDO
ENDDO

!Add Lighting Subsidies (note, lighting bulb costs are converted into RTEKDOLLARYR based on RLGTDOLLARYR)
DO app=1,NumApps
  DO D=1,mNumCR-2
    DO B=1,mNumBldg
      DO E=1,numtypes(app)
        COST111RES(D,Y1)=COST111RES(D,Y1)+FLOAT(EPA111D)*(LTsubsidy(app,Y,E,B,d,1)+LTsubsidy(app,Y,E,B,d,2))  !costs only accumulate in CPP runs
      ENDDO
	ENDDO
  ENDDO
ENDDO

!Before exit convert to billions of 1987$ & multiply incentive payments for approximate direct and indirect costs (slightly higher costs than EIA-861 for 2012)
DO D=1,mNumCR-2
  COST111RES(D,Y1)= 1.5*(COST111RES(D,Y1)*MC_JPGDP(-2)/MC_JPGDP(RTEKDOLLARYR-BaseYr+1) )/10**9  !scale the incentive payment costs here to account for program administration	!TODO - revist use; update or remove as necessary
ENDDO

!Add subsidies for distributed generation and deflate from iGenCapCostYr (generally not the same as rsmeqp) as well as convert from $mill to $bill
DO D=1,mNumCR-2
  DO NT=1, nTek
    COST111RES(D,Y1)=COST111RES(D,Y1)+FLOAT(EPA111D)*x111dRenSub(y1,d,nt)*( MC_JPGDP(-2)/MC_JPGDP(iGenCapCostYr-BaseYr+1) )/1000.  !costs only accumulate in CPP runs
  ENDDO
ENDDO

IF ((PRTDBGR.EQ.1).AND.(EPA111D.EQ.1)) THEN
  WRITE(9,*)'111d year, division, cost (billion 1987$), savings (bkWh)'
  !Print exactly what gets passed on for integrated debugging only
  DO D=1,mNumCR-2
    WRITE(9,160) Y, D, COST111RES(D,Y1), SAVE111RES(D,Y1)
  ENDDO
ENDIF
160 FORMAT(2I5,2f12.5)

END SUBROUTINE CALC111D


!==============================================================================
! DISTRIBUTED GENERATION ADOPTION SUBROUTINE
!
!  INCLUDES USER SPECIFIED NUMBER OF GENERAL TECHNOLOGIES:
!  -CAN MODEL NON-OVERLAPPING VINTAGE RANGES OR INDIVIDUAL YEARS
!  -CAN INCLUDE A USER-SPECIFIED NUMBER OF TECHNOLOGIES
!==============================================================================
SUBROUTINE RDISTGEN
IMPLICIT NONE

!NEMS VARIABLES FROM RESD AND LDSM/EMM
INTEGER MaxNiche
PARAMETER (MaxNiche=26) !Maximum number of distributed generation niches per census division (i.e., maximum iNiche) from RSGENTK.txt	!TODO - create variable in RSGENTK.txt
REAL*4 xExistPen

!********************************************************************
!LOCAL VARIABLES AND PARAMETERS INTERNAL TO RDISTGEN
!********************************************************************
!Distributed generation (DGrate) variables for switching between retail space cooling end-use electricity rates and weighted marginal/retail electricity rates for solar PV calculations
LOGICAL DGrateBlend  !switch to turn on blending of retail and wholesale electricity rate
INTEGER DGrateYr  !first year of electricity rate change
REAL*4 DGmargWt(MNUMCR)  !CD-level weight for marginal/ wholesale electricity rate
REAL*4 DGretWt(MNUMCR)  !CD-level weight for retail electricity rate

!DATA FOR (30-YEAR) PAYBACK COMPUTATION
INTEGER iPayback(30)
REAL*8 xMaxPen,xSimplePayback,xPen,xTemp,xTempHH,xTest

!DATA FOR INTERCONNECTION LIMITATION
REAL*4 XINX(mNumCR-2),xInxDecay(mNumCR-2,mNumYr)
INTEGER xInxFY, xInxLY

!DATA FOR 30-YEAR CASH-FLOW MODEL CALCULATION
REAL*4 XOUTLAY(30), XINTAMT(30), XPRIN(30)
REAL*4 XLOANBAL(30), XDEPR(30)
REAL*4 XFUELCOST(30), XTAXDEDUCT(30)
REAL*4 XMAINTCOST(30), XTOTALCOST(30)
REAL*4 XVALESAVE(30),xKWH(30)
REAL*4 XNETCASHFLOW(30), XPAYMENT, XDOWNPAY
REAL*4 XCUMNETFLOW(35) !30+5 positions for "look ahead"
REAL*4 XINTRATE, XTERM, XINFLATION, XLIFE, XDOWNPAYPCT
REAL*4 XTAXCREDIT(30), XTAXRATE
REAL*4 xEqCost, xTaxCreditPct, XTAXCREDITMAxKW, XTAXCREDITMAX, XBaseYrFUELCOST
REAL*4 XMAINTCOSTBASE, XVALESAVEBASE, xDegradation
REAL*4 xAnnualKWh,XGASINPUT,XWATERHTGMMBTU
REAL*4 XBTUWASTEHEAT,XEXCESSKWH,xElecAvgUEC
REAL*4 xSalestoGridPR, xRetailElecPR, xSizefromTaxOptim
REAL*8 xUnits, xTrills, xCapacity, xTrillsOwnUse, xfuelusage, xhwbtu, xInvest

!FOR RENEWABLE PORTFOLIO STANDARD (RPS) MODELING
REAL*4 xRetailElecPRnoRPS, xSalestoGridPrnoRPS, xRPS(nTek,mNumYr)
REAL*4 xRetailElecPRadjRPS, xSalestoGridPradjRPS !credit adjustment features for American Clean Energy and Security Act of 2009
INTEGER iRPSStartYear, iRPSPhaseOutYear, iRPSGrandFatherYear, iCALYR
INTEGER iNumRPSCreditYrs(nTek,mNumYr), iNumYrsatRPSBaseRate(nTek,mNumYr), iTemp1, iTemp2
!For calculating credit factors to pass to Electric Generation Model	!TODO - review; these and related calculations only used by RDM and CDM?
REAL*4 xPVGenAdded(mNumCR,mNumYr),xWindGenAdded(mNumCR,mNumYr)
REAL*4 xCompCredit, xCompGen, xCredit, xBonus
REAL*4 XVALESAVEBASEnoRPS, XVALESAVEBASEadjRPS

!TECHNOLOGY-SPECIFIC DATA FROM RSGENTK.TXT INPUT FILE
REAL*4 xDegrad(nTek,mNumYr),xElEff(nTek,mNumYr)
REAL*4 xEqLife(nTek,mNumYr),xWhRecovery(nTek,mNumYr)
REAL*4 xInstCost(nTek,mNumYr),xCapCost(nTek,mNumYr)
REAL*4 xMaintCst(nTek,mNumYr),xAvail(nTek,mNumYr)
REAL*4 xTxCrPct(nTek,mNumYr),xTXCrMaxPerKW(nTek,mNumYr),xTXCrMaxPerSys(nTek,mNumYr)
REAL*4 xBESSkWh(nTek,mNumYr)  !BESSmodel
REAL*4 xTxCrPct_Div(mNumYr,mNumCR,nTek)
REAL*4 xKW(nTek,mNumYr),xOperHours(nTek),xLossFac(nTek,mNumYr)
REAL*4 xIntervalCst(nTek,mNumYr)
REAL*8 xAlpha,xPenParm,xExogPen(mNumYr,mNumCR,nTek),ExogPVMistie(mNumYr,mNumCR)    !PVzipcalib
INTEGER iFirstYr(nTek,mNumYr),iLastYr(nTek,mNumYr), iIntervalYrs(nTek,mNumYr)
INTEGER iFuelType(nTek),NumTechs,NumYears,NumDiv, iIntervalYrstoUse
INTEGER iExogHistYr(nTek) !Last year of historical exogenous capacity data
CHARACTER*22 aEquipName(nTek,mNumYr) !BESSmodel

!Learning-related variables
REAL *4 xAdjCost,rlearncost,cumship,xbeta(nTek),xc0(nTek)
LOGICAL GlobalLearn
INTEGER NVINT

!Internal niche variables
REAL*4 QRSDGSG(mNumYr,mNumCR) !Grid electricity sales in trillion Btu
INTEGER iNiche, iRateLevel
INTEGER NumPVNiche(mNumCR)

!Solar PV niche variables
REAL*4 xSolarInsolation(mNumCR,MaxNiche,3)     !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
REAL*4 xHHShare(mNumCR,MaxNiche,3)             !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
REAL*4 xRateScalar(mNumCR,MaxNiche,3)          !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
REAL*4 xAvgKWH(mNumCR,MaxNiche,3)              !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
REAL*4 xRoofAreaPerHH(mNumCR,MaxNiche,3)       !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
REAL*4 xRuralPctHH(mNumCR,MaxNiche,3)          !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
REAL*8 xSizefromRoofArea,xSizefromAnnualKWH,xSizeMax,xSizeMin,xCalcKW,xCalcEqCost,xSolarIns    !PVgen
REAL*4 SolarPVTechPotentialMW(mNumYr,mNumCR)
REAL*4 SolarPVAvailRoofArea(mNumYr,mNumCR)
REAL*8 SolarPVInstalledMW(mNumYr,mNumCR)
REAL*4 SolarPVUsedRoofArea(mNumYr,mNumCR)
REAL*4 xSqftPerKW
REAL*4 xpctPVSuitable             !Percentage of households with a suitable south-facing roof (technical potential)
REAL*4 xpctWindSuitable           !Assumed percentage of HH for which wind could be appropriate	!TODO - not used

!Distributed Wind Niche Variables
REAL*4 xWindSpeed(mNumCR,MaxNiche,3)           !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
REAL*4 WindAvailHH(mNumYr,mNumCR)
REAL*4 WindTechPotentialMW(mNumYr,mNumCR)
REAL*4 WindInstalledMW(mNumYr,mNumCR)
REAL*4 xMpS                                    !meters per second temp variable

!OTHER LOCAL VARIABLES
INTEGER IYR,iCurIYr,NV,NT,iDiv,ilife,r,i,F
LOGICAL LPRINT ,LPRINT2  ! LPRINT, AND LPRINT2 CONTROL OUTPUT DETAIL
INTEGER FILE_MGR         ! FILE MANAGER
INTEGER*4 INFILE         ! FILE HANDLE FOR INPUT
INTEGER*4 DGDAT          ! FILE HANDLE FOR OUTPUT
REAL XValue

!SOLAR PV HURDLE MODEL VARIABLES (use dynamic allocations due to ZIP code dimension for arrays, ~30,000 ZIPs)
!Static and Dynamic Variables for Econometric PV Penetration Model
INTEGER NumZIPs                                 ! Read from input file, number of ZIP code records to read
Logical UseZipModel                             ! Read from input file
Logical PVzipcalib                              ! Read from input file  !PVzipcalib
INTEGER EstYear                                 ! Estimation year for the econometric model, also first year used in projections
INTEGER*4, ALLOCATABLE:: ZipCode(:)             ! ZIP code for diagnostics only
CHARACTER*2, ALLOCATABLE:: State(:)             ! State code
INTEGER, ALLOCATABLE:: CenDiv(:)                ! Census division of ZIP Code
REAL*4, ALLOCATABLE:: Income(:)                 ! Income per capita in ZIP
REAL*4, ALLOCATABLE:: Households(:)             ! Households in ZIP
REAL*4, ALLOCATABLE:: PopDensity (:)            ! PopDensity in ZIP
REAL*4, ALLOCATABLE:: ElecRate(:)               ! Electric Rate in ZIP
REAL*4, ALLOCATABLE:: Income_L(:)               ! Initial Value for Iteration Control
REAL*8, ALLOCATABLE:: Households_L(:)           ! Initial Value for Iteration Control
REAL*4, ALLOCATABLE:: PopDensity_L(:)           ! Initial Value for Iteration Control
REAL*4, ALLOCATABLE:: ElecRate_L(:)             ! Initial Value for Iteration Control
REAL*4, ALLOCATABLE:: Insol(:)                  !
REAL*4, ALLOCATABLE:: LagCDD(:)                 !
REAL*4 IntRate                                  ! National level variable
REAL*4 PVPrice                                  ! National level variable
REAL*4 InputPVPrice                             ! National level variable; used to store unmodified PV price as input from RGENTK.txt
REAL*4 MonthlyPayment                           ! National level variable
REAL*8, ALLOCATABLE:: Lag1Installs(:)           ! Initial Lag1 Installs from Input File
REAL*8, ALLOCATABLE:: Lag2Installs(:)           ! Initial Lag2 Installs from Input File
REAL*8, ALLOCATABLE:: ProjectedInstalls(:)      ! Contains model projections, used to set lagged installs for subsequent projection years
INTEGER, ALLOCATABLE:: PureHurdle(:)            ! PureHurdle in combination with RuralZip determine model coefficient values
INTEGER, ALLOCATABLE:: RuralZip(:)              ! Density less than 10 HH per square mile
REAL*8, ALLOCATABLE:: ModelInstalls(:)          ! For verification that results = R Code values in First Year, for Last Year's Projections Subsequently
REAL*8, ALLOCATABLE:: CumUnits(:)               ! For constraining penetration
REAL*8, ALLOCATABLE:: CumUnits_L(:)             ! For constraining penetration; Initial Value for Iteration Control
REAL*4 CINT(2,3),CHH(2,3),CPD(2,3),CINC(2,3),CINS(2,3), &
       CER(2,3),CCDD(2,3),CPMT(2,3),CIR(2,3),CLAG1(2,3),CLAG2(2,3),CPVP(2,3)  !Sets of model coefficients
REAL*16 xLogit, xNegBinom                       ! Temporary variables
INTEGER j                                       ! Index variable for model selection
REAL*8 factor0, factor, factor1, factor2, factor3, factor4, factor5, factor6, factor7
INTEGER*2 NumYearsPV

!********************************************************************
!TEST FOR TRIGGER TO READ FILE AND BEGIN CALCULATIONS
!********************************************************************
DGDAT=23  !Unit 23 is RDM_DGENOUT.txt
!TODO - Unit 23 is also used for the RDM_DBOUT and CDM_RPTOUT files and is closed when RDM_DGENOUT is written

!NO CALCULATIONS PRIOR TO RECSYear+1
IF (CurCalYr.LT.RECSYear) RETURN
IF (RSYR.EQ.RECSYear) OPEN(DGDAT,FILE='RDM_DGENOUT.txt',FORM='FORMATTED')
iCurIYr=RSYR-BaseYr+1
IF (CurCalYr.NE.RECSYear.OR.CURITR.NE.1) GOTO 95

!********************************************************************
!Read solar PV ZIP code econometric hurdle model inputs (RGENTK.txt)
! Data from EstYear onward where projections are based on an econometric logit/ hurdle model formulation
!********************************************************************
INFILE=FILE_MGR('O','RGENTK',.FALSE.)
  READ(INFILE,'(19(/))')
  READ(INFILE,*)UseZipModel !if true the ZIP code model will be used for EstYear and beyond
  !IF (LPRINT) WRITE(DGDAT,*) 'UseZipModel= ', UseZipModel
  READ(INFILE,*)PVzipcalib !if true the ZIP code model will calibrate to exogenous PV capacity in EstYear and earlier
  !IF (LPRINT) WRITE(DGDAT,*) 'PVzipcalib= ', PVzipcalib
  READ(INFILE,*)EstYear  !this is the first year that the ZIP code model will project
  !IF (LPRINT) WRITE(DGDAT,*) 'Estimation Year= ', EstYear
  READ(INFILE,'(2(/))')

  DO i=1,3	!TODO - replace 3 with parameter?
    READ(INFILE,*)CINT(1,i),CINT(2,i)
    READ(INFILE,*)CHH(1,i),CHH(2,i)
    READ(INFILE,*)CPD(1,i),CPD(2,i)
    READ(INFILE,*)CINC(1,i),CINC(2,i)
    READ(INFILE,*)CINS(1,i),CINS(2,i)
    READ(INFILE,*)CER(1,i),CER(2,i)
    READ(INFILE,*)CCDD(1,i),CCDD(2,i)
    READ(INFILE,*)CPMT(1,i),CPMT(2,i)
    READ(INFILE,*)CIR(1,i),CIR(2,i)
    READ(INFILE,*)CLAG1(1,i),CLAG1(2,i)
    READ(INFILE,*)CLAG2(1,i),CLAG2(2,i)
    READ(INFILE,*)CPVP(1,i),CPVP(2,i)
    READ(INFILE,*) !skip model title
  ENDDO
  !IF (LPRINT) WRITE(DGDAT,*) CLAG1(1,1),CLAG2(1,1)

  READ(INFILE,*)NumZIPs
  !IF (LPRINT) WRITE(DGDAT,*) 'NumZIPs= ', NumZIPs
  READ(INFILE,'((/))')

  !Beginning of Dynamic Array Allocations for new Econometric Model
  IF (ALLOCATED(ZipCode)) DEALLOCATE(ZipCode); ALLOCATE(ZipCode(NumZIPs))
  IF (ALLOCATED(State)) DEALLOCATE(State); ALLOCATE(State(NumZIPs))
  IF (ALLOCATED(CenDiv)) DEALLOCATE(CenDiv); ALLOCATE(CenDiv(NumZIPs))
  IF (ALLOCATED(Income)) DEALLOCATE(Income); ALLOCATE(Income(NumZIPs))
  IF (ALLOCATED(Households)) DEALLOCATE(HouseHolds); ALLOCATE(Households(NumZIPs))
  IF (ALLOCATED(PopDensity)) DEALLOCATE(PopDensity); ALLOCATE(PopDensity(NumZIPs))
  IF (ALLOCATED(ElecRate)) DEALLOCATE(ElecRate); ALLOCATE(ElecRate(NumZIPs))
  IF (ALLOCATED(Income_L)) DEALLOCATE(Income_L); ALLOCATE(Income_L(NumZIPs))
  IF (ALLOCATED(Households_L)) DEALLOCATE(HouseHolds_L); ALLOCATE(Households_L(NumZIPs))
  IF (ALLOCATED(PopDensity_L)) DEALLOCATE(PopDensity_L); ALLOCATE(PopDensity_L(NumZIPs))
  IF (ALLOCATED(ElecRate_L)) DEALLOCATE(ElecRate_L); ALLOCATE(ElecRate_L(NumZIPs))
  IF (ALLOCATED(LagCDD)) DEALLOCATE(LagCDD); ALLOCATE(LagCDD(NumZIPs))
  IF (ALLOCATED(Insol)) DEALLOCATE(Insol); ALLOCATE(Insol(NumZIPs))
  IF (ALLOCATED(Lag1Installs)) DEALLOCATE(Lag1Installs); ALLOCATE(Lag1Installs(NumZIPs))
  IF (ALLOCATED(Lag2Installs)) DEALLOCATE(Lag2Installs); ALLOCATE(Lag2Installs(NumZIPs))
  IF (ALLOCATED(ProjectedInstalls)) DEALLOCATE(ProjectedInstalls); ALLOCATE(ProjectedInstalls(NumZIPs))
  IF (ALLOCATED(PureHurdle)) DEALLOCATE(PureHurdle); ALLOCATE(PureHurdle(NumZIPs))
  IF (ALLOCATED(RuralZip)) DEALLOCATE(RuralZip); ALLOCATE(RuralZip(NumZIPs))
  IF (ALLOCATED(ModelInstalls)) DEALLOCATE(ModelInstalls); ALLOCATE(ModelInstalls(NumZIPs))
  IF (ALLOCATED(CumUnits)) DEALLOCATE(CumUnits); ALLOCATE(CumUnits(NumZIPs))
  IF (ALLOCATED(CumUnits_L)) DEALLOCATE(CumUnits_L); ALLOCATE(CumUnits_L(numzips))

  ZipCode(:)=0
  State(:)=" "
  CenDiv(:)=0
  Income(:)=0
  Households(:)=0
  PopDensity(:)=0
  ElecRate(:)=0
  Income_L(:)=0
  Households_L(:)=0
  PopDensity_L(:)=0
  ElecRate_L(:)=0
  LagCDD(:)=0
  Insol(:)=0
  Lag1Installs(:)=0
  Lag2Installs(:)=0
  ProjectedInstalls(:)=0
  PureHurdle(:)=0
  RuralZip(:)=0
  ModelInstalls(:)=0
  CumUnits(:)=0
  CumUnits_L(:)=0
  IntRate=0
  PVPrice=0
  MonthlyPayment=0

  DO i=1,NumZIPs
    READ(INFILE,*) ZipCode(i),State(i),CenDiv(i),Income(i),Households(i),PopDensity(i),  &
     Insol(i),ElecRate(i),LagCDD(i),IntRate,PVPrice,MonthlyPayment,Lag1Installs(i),  &
     Lag2Installs(i),PureHurdle(i),RuralZip(i),ModelInstalls(i)
    Income_L(i)=Income(i)
    Households_L(i)=Households(i)
    PopDensity_L(i)=PopDensity(i)
    ElecRate_L(i)=ElecRate(i)
    InputPVPrice=PVPrice  !preserves original PVPrice as input
    !IF (UseZipModel) WRITE(DGDAT,29) 'ZipCode,State,CenDiv,ElecRate(2018$),PELME,PELRSOUT(cooling) ',ZipCode(i),State(i),CenDiv(i),ElecRate(i),PELME(CenDiv(i),EstYear-BaseYr+1),PELRSOUT(CenDiv(i),EstYear-BaseYr+1,2)  !Compare ZIP code-level retail electricity rate with census division-level retail and wholesale rates
    !29 FORMAT(A,I5,A4,I2,1X,F5.2,1X,F5.2,1X,F5.2)
  ENDDO
  !IF (UseZipModel) WRITE(DGDAT,*) 'Completed Read of ZIP Code Data'
  !IF (UseZipModel) WRITE(DGDAT,*) 'First Value', ZipCode(1)
  !IF (UseZipModel) WRITE(DGDAT,*) 'Last Value', ModelInstalls(NumZIPs)
INFILE=FILE_MGR('C','RGENTK',.FALSE.)
!End of processing of RGENTK for econometric PV penetration model

!********************************************************************
!Read distributed generation technology menu and related inputs (RSGENTK.txt)
!********************************************************************
INFILE=FILE_MGR('O','RSGENTK',.FALSE.) !OPEN THE DISTRIBUTED GENERATION TECHNOLOGY MENU

!SKIP 20-LINE HEADER AND READ GENERAL CONTROL PARAMETERS AND INPUTS
READ(INFILE,'(19(/))')
!LPRINT TURNS ON TRACING OF EXECUTION; LPRINT2 PROVIDES DETAILS OF THE CASH-FLOW CALCULATIONS FOR "VINTAGE" YEARS
READ(INFILE,*)LPRINT, LPRINT2
IF (LPRINT) WRITE(DGDAT,*) 'MODEL YEAR ',RSYR, 'ITERATION ',CURITR

READ(INFILE, '(/)')
!NUMBER OF TECHNOLOGIES, NUMBER OF MODEL YEARS, AND NUMBER OF MODELED CENSUS DIVISIONS
READ(INFILE,*) NumTechs,NumYears,NumDiv	!TODO - NumYears and NumDiv could/should be same as other NEMS variables and not read in here
IF (LPRINT) WRITE(DGDAT,*) NumTechs,NumYears,NumDiv

READ(INFILE,'(/)')
READ(INFILE,*) iGenCapCostYr
IF (LPRINT) WRITE(DGDAT,*) iGenCapCostYr


READ(INFILE, '(/)')
READ(INFILE,*) xAlpha, xPenParm
!xAlpha AND xPenParm CONTROL THE MAGNITUDE AND SHAPE OF THE PENETRATION OF DISTRIBUTED GENERATION TECHNOLOGIES
IF (LPRINT) WRITE(DGDAT,*) xAlpha,xPenParm

READ(INFILE, '(/)')
READ(INFILE,*) (xOperHours(NT), NT=1,NumTechs)
IF (LPRINT) WRITE(DGDAT,'(7F9.2)') (xOperHours(NT), NT=1,NumTechs)
!FOR EACH OF THE TECHNOLOGIES, SYSTEM SIZE IN KW(DC), AND ANNUAL OPERATING HOURS (RELEVENT ONLY FOR FUEL-CONSUMING GENERATION TECHNOLOGIES)

READ(INFILE, '(/)')
READ(INFILE,*) GLOBALLEARN
IF (LPRINT) WRITE(DGDAT,*) "Global Learning = ",GLOBALLEARN

READ(INFILE, '(/)')
!FOR EACH OF THE TECHNOLOGIES, Learning Betas (Doubling parameter, 0=no learning) and c0's (initial costs)
READ(INFILE,*) (XBETA(NT), NT=1,NumTechs)
IF (LPRINT) WRITE(DGDAT,'(3F9.2)') (XBETA(NT), NT=1,NumTechs)
READ(INFILE,*) (XC0(NT), NT=1,NumTechs)
IF (LPRINT) WRITE(DGDAT,'(3F9.2)') (XC0(NT), NT=1,NumTechs)

READ(INFILE, '(/)')
READ(INFILE,*) iRPSStartYear, iRPSPhaseOutYear, iRPSGrandFatherYear
IF (LPRINT) WRITE(DGDAT,'(3I8)') iRPSStartYear, iRPSPhaseOutYear, iRPSGrandFatherYear
!The above are scalars for multiplying the RPS credit price

READ(INFILE, '(/)')
!scalars for limiting the penetration based on interconnection index
READ(INFILE,*) (XINX(i),i=1,NumDiv)
IF (LPRINT) WRITE(DGDAT,'(9F7.3)') (XINX(i),i=1,NumDiv)
!first and last years of the interconnection limits
READ(INFILE,*) xInxFY, xInxLY
IF (LPRINT) WRITE(DGDAT,'(3I8)') xInxFY, xInxLY

READ(INFILE, '(/)')
READ(INFILE,*)XTAXRATE,XDOWNPAYPCT,XTERM,XINFLATION
IF (LPRINT) WRITE(DGDAT,'(4F7.2)') XTAXRATE, XDOWNPAYPCT, XTERM, XINFLATION
!NOTES:
! -XTAXRATE IS THE COMBINED FEDERAL AND STATE INCOME TAX RATE WHICH IS USED
!   IN THE CASH-FLOW CALCULATIONS FOR INTEREST PAID ON EQUIPMENT LOANS
!   (ASSUMED TO BE "ROLLED IN" WITH THE MORTGAGE)
! -XDOWNPAYPCT IS THE MORTGAGE DOWN PAYMENT PERCENTAGE
! -XINTRATE IS THE MORTGAGE ANNUAL LOAN RATE
! -XTERM IS THE NUMBER OF YEARS FOR THE LOAN (ASSUMING 20 YEARS SIMPLIFIES
!   EQUIPMENT ACCCOUNTING (REPLACEMENT OF ORIGINAL EQUIPMENT IS NOT
!   PART OF THE CASH-FLOW BECAUSE IT DIES AFTER 20 YEARS)
! -XINFLATION IS THE INFLATION FOR THE CASH-FLOW CALCULATIONS (IN REAL DOLLARS)
!   TO DISCOUNT LOAN PAYMENTS (IN NOMINAL DOLLARS)

READ(INFILE, '(//)')
READ(INFILE,*) NVINT  !number of technology records in file
IF (LPRINT) WRITE(DGDAT,*) NVINT
!Initialize technology variable to be read in next
aEquipName = "                      " !BESSmodel
iFuelType = 0.0
iFirstYr = 0.0
iLastYr = 0.0
xKW = 0.0
xBESSkWh = 0.0 !BESSmodel
xElEff = 0.0
xLossFac = 0.0
xDegrad = 0.0
xEqLife = 0.0
xWhRecovery = 0.0
xInstCost = 0.0
xCapCost = 0.0
xMaintCst = 0.0
xIntervalCst = 0.0
iIntervalYrs = 0.0
xAvail = 0.0
xTxCrPct = 0.0
xTXCrMaxPerKW = 0.0
xTXCrMaxPerSys = 0.0
!xTxCrPct_div = 0.0 !BESSmodelTODO
!xTemp = 0.0 !BESSmodelTODO
!iTemp1 = 0 !BESSmodelTODO
!iTemp2 = 0 !BESSmodelTODO

!POPULATE TECHNOLOGY ARRAY
DO NT=1,NumTechs
  DO NV=1,NVINT
    !THE ORIGINAL TECHNOLOGY FILE ASSUMES:
    ! NT=1 IS SOLAR PHOTOVOLTAIC (PV) TECHNOLOGY
    ! NT=2 IS NATURAL GAS FUEL CELL TECHNOLOGY
    ! NT=3 IS SMALL WIND TURBINE TECHNOLOGY
    ! NT=4 IS BATTERY ENERGY STORAGE TECHNOLOGY  !BESSmodel
    !THE VINTAGES APPLY TO DIFFERENT NON-OVERLAPPING TIME PERIODS AND MAY EMBODY TECH PROGRESS

    !Vary DG capacity by year -- add read of xKW capacity by vintage here
    READ(INFILE,*,END=99) aEquipName(NT,NV), iFuelType(NT), &
     iFirstYr(NT,NV),   iLastYr(NT,NV), &
     xKW(NT,NV), xBESSkWh(NT,NV), xElEff(NT,NV), &  !BESSmodel
     xLossFac(NT,NV),  xDegrad(NT,NV), &
     xEqLife(NT,NV),  xWhRecovery(NT,NV), &
     xInstCost(NT,NV),  xCapCost(NT,NV), &
     xMaintCst(NT,NV),  xIntervalCst(NT,NV), &
     iIntervalYrs(NT,NV),  xAvail(NT,NV), &
     xTxCrPct(NT,NV),  xTXCrMaxPerKW(NT,NV), xTXCrMaxPerSys(NT,NV), &
     (xTxCrPct_Div(nv,iDiv,nt),iDiv=1,mNumCR-2), &
     xTemp, iTemp1, iTemp2

    !NOTES:
    ! -aEquipName IS THE TECHNOLOGY/EQUIPMENT NAME FOR REPORTING PURPOSES
    ! -iFuelType IS THE FUEL USED BY THE TECHNOLOGY (0 FOR SOLAR OR WIND)
    !   THIS FUEL TYPE MUST COINCIDE WITH THE MAIN MODEL DEFINITION OF FUELS
    ! -iFirstYr IS THE FIRST YEAR A TECHNOLOGY CAN BE PURCHASED
    ! -iLastYr IS THE LAST YEAR A TECHNOLOGY CAN BE PURCHASED (DON'T
    !   DON'T ALLOW TECHNOLOGIES TO "OVERLAP" OR "GAP" (E.G., VINTAGE 1 2020-2029; VINTAGE 2 2030-2039; VINTAGE 3 2040-2050)
    ! -xKW IS THE AVERAGE SYSTEM CAPACITY IN KW-DC (SOLAR PV & BATTERY ENERGY STORAGE SYSTEMS) OR KW-AC (NATURAL GAS FUEL CELLS & SMALL WIND)  !BESSmodel
    ! -xBESSkWh IS THE AVERAGE STORAGE CAPACITY OF BATTERY ENERGY STORAGE SYSTEMS IN KWH-DC; ZERO FOR ALL OTHER TECHNOLOGIES  !BESSmodel
    ! -xElEff IS THE ELECTRICAL CONVERSION EFFICIENCY OF THE TECHNOLOGY
    ! -xLossFac IS THE LOSS FACTOR FROM GENERATION TO END USE; INCLUDES LINE LOSS, INVERTER LOSSES, ETC.
    !   EQUIVALENT TO THE INVERSE OF THE INVERTER LOADING RATIO
    ! -xDegrad IS THE ANNUAL DEGRADATION IN THE CONVERSION EFFICIENCY OF SOLAR PV
    ! -xEqLife IS THE EQUIPMENT LIFE IN YEARS
    ! -xWhRecovery IS THE PERCENTAGE OF WASTE HEAT (FOR FUEL-USING TECHNOLOGIES) THAT CAN BE RECOVERED FOR WATER HEATING
    !   ANYTHING IN EXCESS OF AVERAGE WATER HEATING REQUIREMENTS IS ASSUMED WASTED
    ! -xInstCost IS THE INSTALLATION COST PER KW IN iGenCapCostYr-DOLLARS
    ! -xCapCost IS THE CAPITAL COST PER KW IN iGenCapCostYr-DOLLARS
    ! -xMaintCst IS THE ANNUAL MAINTENANCE COST IN iGenCapCostYr-DOLLARS
    ! -xIntervalCst IS THE DC-TO-AC INVERTER COST IN iGenCapCostYr-DOLLARS; FOR SOLAR PV ONLY
    ! -iIntervalYrs IS THE INVERTER LIFE IN YEARS
    ! -xAvail IS PERCENTAGE OF TIME AVAILABLE (1.0-FORCED OUTAGE RATE-PLANNED OUTAGE RATE) APPLIED TO TYPICAL OPERATING HOURS
    ! -xTxCrPct IS THE TAX FEDERAL CREDIT PERCENTAGE AS A PERCENTAGE OF INSTALLED COST
    ! -xTXCrMaxPerKW IS THE MAXIMUM DOLLAR AMOUNT (PER KW) OF THE TAX CREDIT (IF ZERO, NO CAP)
    ! -xTXCrMaxPerSys IS THE MAXIMUM DOLLAR AMOUNT (PER SYSTEM) OF THE TAX CREDIT (IF ZERO, NO CAP)
    ! -xTxCrPct_Div IS THE NON-FEDERAL/UTILITY SUBSIDY/REBATE PERCENTAGE AS A PERCENTAGE OF INSTALLED COST
    ! -xTemp IS THE RPS CREDIT AVAILABILITY, ANNUALIZED USING xRPS variable
    ! -iTemp1 IS THE NUMBER OF YEARS BEFORE RPS RATE ADJUSTMENT; ANNUALIZED USING iNumRPSCreditYrs variable
    ! -iTemp2 IS THE TOTAL NUMBER OF RPS CREDIT YEARS; ANNUALIZED USING iNumRPSCreditYrs VARIABLE

    IF (LPRINT) WRITE(DGDAT,30) aEquipName(NT,NV), iFuelType(NT), iFirstYr(NT,NV), iLastYr(NT,NV), &
     xKW(NT,NV), xBESSkWh(NT,NV), xElEff(NT,NV), xLossFac(NT,NV), xDegrad(NT,NV), xEqLife(NT,NV), &  !BESSmodel
     xWhRecovery(NT,NV), xInstCost(NT,NV), xCapCost(NT,NV), xMaintCst(NT,NV), xIntervalCst(NT,NV), &
     iIntervalYrs(NT,NV), xAvail(NT,NV), xTxCrPct(NT,NV), xTXCrMaxPerKW(NT,NV), xTXCrMaxPerSys(NT,NV), &
     (xTxCrPct_Div(nv,iDiv,nt),iDiv=1,mNumCR-2), xTemp, iTemp1, iTemp2

    !Immediately populate the RPS variable mapping vintages to iCurIYr	!TODO - why use the temp variable names at all when annualized RPS values are read-in from file?
    DO iyr=iFirstYr(nt,nv),iLastYr(nt,nv)
      xRPS(nt,iyr-BaseYr+1)=xTemp
      iNumYrsatRPSBaseRate(nt,iyr-BaseYr+1)=iTemp1
      iNumRPSCreditYrs(nt,iyr-BaseYr+1)=iTemp2
    ENDDO

  ENDDO  !NumTechs
ENDDO  !NVINT
30 FORMAT(1X,A10,3I6,2F9.1,3F9.2,2F9.1,F9.2,3F9.0,I6,2F9.2,2F9.1,9F9.2,F5.1,2I5)  !BESSmodel

!Debugging to see what we ended up with:
!DO iyr=1,mNumYr
!  WRITE(DGDAT,*)xRPS(3,iyr),iNumYrsatRPSBaseRate(3,iyr),iNumRPSCreditYrs(3,iyr)
!ENDDO

!PV NICHES:
! Add an arbitrary number of insolation niches customized to each census division (based on bins of solar insolation levels rounded to nearest 0.25 kWh/m^2/day)
! Add 3 rate levels: High, Mid, and Low average electricity rates

!READ SOLAR INSOLATION, SQFT SHARES, AVERAGE ELECTRICITY RATES RELATIVE TO CENSUS DIVISION, ROOF TO SQFT RATIOS, WIND SPEED FOR NICHES.
! VALUES ARE ESTIMATED FROM RECS MICRODATA (BOTH PUBLIC AND UN-PUBLISHED)USING ZIP CODE-LEVEL PV SOLAR RADIATION/INSOLATION (LATITUDED TILT)
! AND NREL 30M ANNUAL AVERAGE WIND SPEEDS BY STATE.

READ(INFILE,'(///)')
DO I=1,NumDiv
  READ(INFILE,*) iDiv, iNiche
  IF (LPRINT) WRITE(DGDAT,*) "iDiv, iNiche", iDiv, iNiche
  NumPVNiche(iDiv)= iNiche
  DO iNiche=1,NumPVNiche(iDiv)
    DO iRateLevel=1,3 !i.e., high, mid/average, and low average electricity rate levels	!TODO - replace 3 with parameter?
      READ(INFILE,*) xSolarInsolation(iDiv,iNiche,iRateLevel),xHHShare(iDiv,iNiche,iRateLevel), &
       xRateScalar(iDiv,iNiche,iRateLevel), xAvgKWH(iDiv,iNiche,iRateLevel), &
       xRoofAreaPerHH(iDiv,iNiche,iRateLevel), xWindSpeed(iDiv,iNiche,iRateLevel),xRuralPctHH(iDiv,iNiche,iRateLevel)
      IF (LPRINT) WRITE(DGDAT,87) iDiv,xSolarInsolation(iDiv,iNiche,iRateLevel),xHHShare(iDiv,iNiche,iRateLevel), &
       xRateScalar(iDiv,iNiche,iRateLevel), xAvgKWH(iDiv,iNiche,iRateLevel), &
       xRoofAreaPerHH(iDiv,iNiche,iRateLevel), xWindSpeed(iDiv,iNiche,iRateLevel),xRuralPctHH(iDiv,iNiche,iRateLevel)
    ENDDO !iRateLevel
  ENDDO !iNiche
ENDDO !NumDiv
87 FORMAT(1x,"Div= ",I3,3F10.4,2F12.4,2F10.4)

!Last year of historical exogenous capacity data; model builds begin after this
READ(INFILE, '(/)')
READ(INFILE,*,END=99) (iExogHistYr(NT),NT=1,NumTechs)

!Read exogenous distributed generation capacity
READ(INFILE, '(/)')
DO NT=1,NumTechs
  DO iDiv=1,NumDiv
    READ(INFILE,*,END=99) (xExogPen(IYR,iDiv,NT),IYR=RECSYear-BaseYr+1,NumYears)

    IF (LPRINT) WRITE(DGDAT,*) 'EXOGENOUS DG CAPACITY (KW-DC) FROM RSGENTK (RECSYR-ONWARD)'
    IF (LPRINT) WRITE(DGDAT,*) 'TECHNOLOGY ',NT,'  DIVISION ',iDiv
    IF (LPRINT) WRITE(DGDAT,97) (xExogPen(IYR,iDiv,NT),IYR=RECSYear-BaseYr+1,NumYears)
  ENDDO !NumDiv
ENDDO !NumTechs
97 FORMAT(5F10.0)

!Read switch to combine marginal (wholesale) electricity rates and retail electricity rates for PV calculations
READ(INFILE,'(/)')
READ(INFILE,*) DGrateBlend  !switch to turn on blending of retail and wholesale electricity rate
IF (LPRINT) WRITE(DGDAT,*) 'DGrateBlend ',DGrateBlend

!Read first year chosen to combine marginal (wholesale) electricity rates and retail electricity rates for PV calculations
READ(INFILE,'(/)')
READ(INFILE,*) DGrateYr
IF (LPRINT) WRITE(DGDAT,*) 'DGrateYr ',DGrateYr

!Read weight values to combine marginal (wholesale) electricity rates and retail electricity rates for PV calculations
READ(INFILE, '(//)')
DO iDiv=1,NumDiv
  READ(INFILE,*,END=99) DGmargWt(iDiv),DGretWt(iDiv)
ENDDO !NumDiv

IF (LPRINT) THEN
  WRITE(DGDAT,*) 'DGmargWt ','DGretWt ','CD'
  DO iDiv=1,NumDiv
    WRITE(DGDAT,*) DGmargWt(iDiv),' ',DGretWt(iDiv),' ',iDiv
  ENDDO !NumDiv
ENDIF

READ(INFILE, * ,END=96) XTEST
WRITE(DGDAT,*) &
 'INPUT ERROR ON RESIDENTIAL DISTRIBUTED GENERATION TECHNOLOGY DATA FILE'
WRITE(DGDAT,*) &
 'EXTRA DATA ENCOUNTERED -- MOST LIKELY A SEVERE PROBLEM'

GOTO 96

99 WRITE(DGDAT,*) &
 'INPUT ERROR ON RESIDENTIAL DISTRIBUTED GENERATION TECHNOLOGY DATA FILE'
WRITE(DGDAT,*) &
 'TOO FEW DATA ENCOUNTERED -- A SEVERE PROBLEM'

96 INFILE=FILE_MGR('C','RSGENTK',.FALSE.)

!Initialize arrays
CGCPVRES(1:NumDiv,iCurIYr)=1.
CGCWNRES(1:NumDiv,iCurIYr)=1.
!xExogPen(1:NumYears,1:NumDiv,1:NumTechs)=0.0 !used for testing / removing exogenous DG installations
Units(1:NumYears,1:NumDiv,1:NumTechs)=0.
Cap(1:NumYears,1:NumDiv,1:NumTechs)=0.
Trills(1:NumYears,1:NumDiv,1:NumTechs)=0.
HWBTU(1:NumYears,1:NumDiv,1:NumTechs)=0.
GasUsage(1:NumYears,1:NumDiv,1:NumTechs)=0.
TrillsOwnUse(1:NumYears,1:NumDiv,1:NumTechs)=0.

!Assumptions for developing technical potential for PV	!TODO - review/update
! Based on orientation alone, approximately 50% of HH would have a suitable southwest- to southeast-facing roof surface.
! Next, assume that, of the suitably oriented single-family households, only 50% of the roof area is facing south.
! Also assume that 40% of this area is unavailable due to shading and other issues like roof impediments.
xpctPVSuitable= .5*.5*(1.-.4)

!END OF READ AND INITIALIZE

!********************************************************************
!BEGIN ECONOMIC PENETRATION MODELING FOR PROJECTED DG BUILDS
!********************************************************************
95 CONTINUE

!Initialize accumulators to allow for multiple NEMS iterations
QRSDGSG(iCurIYr,1:NumDiv)=0.
SolarPVTechPotentialMW(iCurIYr,1:NumDiv)=0.
SolarPVInstalledMW(iCurIYr,1:NumDiv)=0.
SolarPVAvailRoofArea(iCurIYr,1:NumDiv)=0.
SolarPVUsedRoofArea(iCurIYr,1:NumDiv)=0.
WindAvailHH(iCurIYr,1:NumDiv)=0.
WindTechPotentialMW(iCurIYr,1:NumDiv)=0.
WindInstalledMW(iCurIYr,1:NumDiv)=0.
x111dRenSub(iCurIYr,1:NumDiv,1:NumTechs)=0.
xInxDecay(1:NumDiv,iCurIYr)=1.

!Set interconnection limit variable used for all technologies, by census division
DO iDiv=1,NumDiv
  IF (CurCalYr.EQ.xInxFY) THEN
    xInxDecay(iDiv,iCurIYr)=XINX(iDiv)
  ELSE
    xInxDecay(iDiv,iCurIYr)=MIN(1.,(XINX(iDiv)+((1.-XINX(iDiv))*(FLOAT(CurCalYr-xInxFY)/FLOAT(xInxLY-xInxFY)))))
  ENDIF
  !IF (LPRINT) WRITE(DGDAT,*) 'Interconnection Limit ',CurCalYr, iDiv, xInxDecay(iDiv,iCurIYr)
ENDDO

DO NT=1,NumTechs-1 !BESSmodel - Do not process battery energy storage systems
  !NT=1 IS SOLAR PHOTOVOLTAIC TECHNOLOGY
  !NT=2 IS FUEL CELL TECHNOLOGY
  !NT=3 IS DISTRIBUTED WIND TURBINE TECHNOLOGY
  !NT=4 IS BATTERY ENERGY STORAGE TECHNOLOGY  !BESSmodel - Not currently processed, hence "NumTechs-1" above
  !FILTER FOR "VINTAGE" APPROPRIATE FOR THIS MODEL YEAR
  DO NV=1,NVINT
    IF (iFirstYr(NT,NV).GT.CurCalYr) GOTO 66  !SKIP OUT-OF-SCOPE VINTAGES
    IF (iLastYr (NT,NV).LT.CurCalYr) GOTO 66  !SKIP OUT-OF-SCOPE VINTAGES
    IF (LPRINT) THEN
      IF (iFuelType(NT).EQ.0) THEN
        WRITE(DGDAT,*)'BEGINNING CASH-FLOW CALCULATIONS FOR SOLAR PV, WIND SYSTEMS, OR BATTERY ENERGY STORAGE SYSTEMS' !BESSmodel
      ELSE
        WRITE(DGDAT,*)'BEGINNING CASH-FLOW CALCULATIONS FOR FUEL CELL / OTHER SYSTEMS'
      ENDIF
    ENDIF

    !INITIALIZE VALUES FOR OPERATING COST CALCULATIONS
    ! Update "Learned" Costs for Solar PV, Fuel Cells, Wind, and Batttery Energy Storage Systems
    IF (aEquipName(nt,nv) .EQ. "Fuel_Cell" ) THEN
      cumship=CFuelCell_MW(iCurIYr-1)+ RFuelCell_MW(iCurIYr-1) !buildings shipments only
      !IF (globallearn.EQ.1)cumship=CFuelCell_MW(iCurIYr-1)+ RFuelCell_MW(iCurIYr-1) +UFuelcell_MW(iCurIYr-1)+ IntnlFuelCell_MW(iCurIYr-1)+ IFuelCell_MW(iCurIYr-1)	!TODO - not enabled; verify variables
      xAdjCost = rLearnCost (xCapCost(nt,nv), xbeta(nt), xc0(nt), cumship, 23)  !Unit 23 = CDM_RPTOUT.txt	!TODO - set 23 as a parameter once so it doesn't need to be repeated
      IF (LPRINT) WRITE(DGDAT,*)'current year ',iCurIYr+BaseYr-1
      IF (LPRINT) WRITE(DGDAT,*)'learning beta ',xbeta(nt)
      IF (LPRINT) WRITE(DGDAT,*)'initial cost ',xc0(nt)
      IF (LPRINT) WRITE(DGDAT,*)'cumulative shipments ',CumShip
      IF (LPRINT) WRITE(DGDAT,*)'RSGENTK default cost ',xCapCost(nt,nv)
      IF (LPRINT) WRITE(DGDAT,*)'learning-adjusted cost ',xAdjCost
    ELSEIF (aEquipName(nt,nv) .EQ. "Solar_PV" ) THEN
      IF (globallearn) THEN
        !globallearn=True indicates to include utility-scale PV installs in the learning calculations for buildings
        cumship=CPV_MW(iCurIYr-1) +RPV_MW(iCurIYr-1)+ UPV_MW(iCurIYr-1)
      ELSE
        cumship=CPV_MW(iCurIYr-1) +RPV_MW(iCurIYr-1)
      ENDIF
      !IF (globallearn.EQ.1) cumship=CPV_MW(iCurIYr-1) +RPV_MW(iCurIYr-1)+ UPV_MW(iCurIYr-1) + IntnlPV_MW(iCurIYr-1)+IPV_MW(iCurIYr-1)  !not including international PV capacity for learning calculations	!TODO - not enabled; verify variables
      xAdjCost = rLearnCost (xCapCost(nt,nv), xbeta(nt), xc0(nt), cumship, 23)  !Unit 23 = CDM_RPTOUT.txt	!TODO - set 23 as a parameter once so it doesn't need to be repeated
      IF (LPRINT) WRITE(DGDAT,*)'current year ',iCurIYr+BaseYr-1
      IF (LPRINT) WRITE(DGDAT,*)'learning beta ',xbeta(nt)
      IF (LPRINT) WRITE(DGDAT,*)'initial cost ',xc0(nt)
      IF (LPRINT) WRITE(DGDAT,*)'cumulative shipments ',CumShip
      IF (LPRINT) WRITE(DGDAT,*)'RSGENTK default cost ',xCapCost(nt,nv)
      IF (LPRINT) WRITE(DGDAT,*)'learning-adjusted cost ',xAdjCost
    ELSEIF (aEquipName(nt,nv) .EQ. "Wind" ) THEN
      cumship=CWind_MW(iCurIYr-1) +RWind_MW(iCurIYr-1)
      !IF (globallearn.EQ.1) cumship=CWind_MW(iCurIYr-1) +RWind_MW(iCurIYr-1)+ UWind_MW(iCurIYr-1) + IntnlWind_MW(iCurIYr-1) + IWind_MW(iCurIYr-1)	!TODO - not enabled; verify variables
      xAdjCost = rLearnCost (xCapCost(nt,nv), xbeta(nt), xc0(nt), cumship, 23)  !Unit 23 = CDM_RPTOUT.txt	!TODO - set 23 as a parameter once so it doesn't need to be repeated
      IF (LPRINT) WRITE(DGDAT,*)'current year ',iCurIYr+BaseYr-1
      IF (LPRINT) WRITE(DGDAT,*)'learning beta ',xbeta(nt)
      IF (LPRINT) WRITE(DGDAT,*)'initial cost ',xc0(nt)
      IF (LPRINT) WRITE(DGDAT,*)'cumulative shipments ',CumShip
      IF (LPRINT) WRITE(DGDAT,*)'RSGENTK default cost ',xCapCost(nt,nv)
      IF (LPRINT) WRITE(DGDAT,*)'learning-adjusted cost ',xAdjCost
    ELSE
      xAdjCost=xCapCost(nt,nv)
    ENDIF
    xTaxCreditPct=xTxCrPct(NT,NV)
    XTAXCREDITMAxKW=xTXCrMaxPerKW(NT,NV)
    XTAXCREDITMAX=xTXCrMaxPerSys(NT,NV)
    XLIFE=xEqLife(NT,NV)
    xEqCost=(xAdjCost+xInstCost(NT,NV)) * xKW(NT,NV) !Vary DG capacity by year
    xDegradation=xDegrad(NT,NV)
    !IF (LPRINT) WRITE (DGDAT,'(4F12.2)') xInstCost(NT,NV),xCapCost(NT,NV), xEqCost, xDegradation

    DO iDiv=1,NumDiv
      !Initialization to output accumulating variables
      xUnits=0.
      xTrills=0.
      xCapacity=0.
      !xCalcKW=0. !BESSmodel	!TODO
      !xTemp=0. !BESSmodel	!TODO
      xTrillsOwnUse=0.
      xfuelusage=0.
      xhwbtu=0.
      xInvest=0.
      !CGCPVRES(iDiv,iCurIYr)=1. !RPS Variable for Electricity Module
      !CGCWNRES(iDiv,iCurIYr)=1. !RPS Variable for Electricity Module
      IF (NT.EQ.1 .AND. UseZipModel .AND. CurCalYr.GE.EstYear) GOTO 26  !FOR PV FOR THE ESTIMATION YEAR AND BEYOND USE CD MODEL
      !WRITE (DGDAT,*) 'MODEL YEAR = ',CurCalYr, 'CENSUS DIVISION ', iDiv
      !IF (LPRINT) WRITE(DGDAT,*) 'VINTAGE YEAR',CurCalYr
      !WRITE(DGDAT,*) '   COMPUTING OPERATING COSTS AND VALUE OF ENERGY SAVINGS',iDiv

      IF (LPRINT.AND.LPRINT2) WRITE(DGDAT,*) 'FUEL TYPE ',iFuelType(NT),'TECHNOLOGY ',NT

      ! Calculate Grid Sales Price
      ! Units are in $/kWh in year dollars of capital costs
      ! Assumed not to be "niche" related
      xSalestoGridPR=PELME(iDiv,iCurIYr)*.003412*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)	!TODO - need to update PELME?

      DO iNiche=1,NumPVNiche(iDiv)  !Add Insolation and Wind Niches
        DO iRateLevel=1,3           !Add Niches for High=1, Mid=2 and Low=3 Average Rates	!TODO - replace 3 with paramete?
          IF (xHHShare(iDiv,iNiche,iRateLevel).EQ.0.) GOTO 25  !next niche -- skip those that have 0 HH share
          !Set average consumption
          xElecAvgUEC=xAvgKWH(iDiv,iNiche,iRateLevel)

          !Skip to other distributed generation techology sections
          IF (NT.EQ.3) GOTO 100  ! Wind
          IF (NT.EQ.2) GOTO 150  ! Fuel Cells

          !CALCULATION OF KWH SUPPLIED FOR Solar
          ! The quantity "77.*(.14/xElEff)*xKW" represents the estimated module square footage.	!TODO - review/update values?
          ! Thus the kWh supplied is:
          !  annualkwh=eff*insolation*sqftperkw*systemkw*lossadj
          ! where lossadj represents average non-optimality factor (orientation effects, etc.)
          ! Notes: Future efficiency gains will result in a smaller collector footprint for a given kW capacity.
          !        Solar insolation is in kWh/m^2/day convert to annual per square foot (365.25/10.8)	!TODO - replace 365.25 with reference to LEAPYR?
          xSqftperKW=77.*.14/xElEff(NT,NV)	!TODO - review/update values?

          !Optimize capacity: set maximums based on 80% of the optimally oriented roof surface area is available (e.g., non-shaded)        !PVgen
          ! Assume only 40% of roof area is suitable for PV to allow for non-optimal orientation and/or complex roof angles.               !PVgen
          ! Also, assume most homeowners cover only 75% of the potential max area and assume a global maximum and minimum of 10 and 1 kW.  !PVgen
          IF (CurCalYr>=RECSYear) THEN                                                                                                     !PVgen
            xSolarIns=xSolarInsolation(iDiv,iNiche,IRateLevel)                                                                             !PVgen
            !transform solar insolation to account for unknown orientations                                                                !PVgen
            xSolarIns= -1.0533 + 1.4325*xSolarIns - 0.0652*xSolarIns**2                                                                    !PVgen	!TODO - review/update values?
            xSizefromRoofArea= (xRoofAreaPerHH(iDiv,iNiche,iRateLevel)*0.8*0.4*0.75/xSqftperKW)                                            !PVgen	!TODO - review/update values?
            xSizefromAnnualKWH=xElecAvgUEC/(xElEff(nt,nv)*xSolarIns*365.25/10.8*xSqftperKW*xLossFac(NT,NV))                                !PVgen	!TODO - replace 365.25 with reference to LEAPYR?

            !Also optimize to maximize the after tax cost per kW(dc) based on credits
            xSizefromTaxOptim=xSizeMax !set to max size if no cap on tax credit
            !Else compute largest size that fully utilizes the credit
            ! first reset the tax credit percentage to potentially something less if the max credit per kW(dc) is capped
            IF (xTaxCreditPct>0. .AND. xtaxcreditmaxKW>0.) THEN
              xTaxCreditPct=min(xTaxCreditPct,xtaxcreditmaxKW/(xAdjCost+xInstCost(NT,NV)))
            ENDIF
            IF (xTaxCreditPct>0. .AND. xtaxcreditmax>0.) xSizefromTaxOptim = (xtaxcreditmax/xTaxCreditPct)/(xAdjCost+xInstCost(NT,NV))
            xSizeMax=10.  !set absolute maximum size
            xSizeMin=1.  !set absolute minimum size
            xCalcKW=FLOAT(nint(max(min(xSizefromRoofArea,xSizefromTaxOptim,xSizeMax),xSizeMin)))    !removed RECS average generation constraint
          ELSE
            xCalcKW=xKW(NT,NV)  !set size to menu capacity
          ENDIF

          xAnnualKWh=xElEff(NT,NV)*xSolarIns*365.25/10.8*xSqftperKW*xCalcKW*xLossFac(NT,NV)    !PVgen	!TODO - replace 365.25 with reference to LEAPYR?

          !The internal NEMS energy prices are converted to "current year" dollars (the dollar year for the DG capacity costs) from the internal NEMS year of 1987 dollars.
          !Note that MC_JPGDP(-2) is the deflator for 1987.
          !For PV, use the space cooling end-use electricity price as the implicit value for own-use generation, due to the high "coincidence factor" between PV output and air conditioning loads.
          ! This usage of the space cooling end-use electricity price is to reflect average summer prices, when PV output is at its highest.

          !THIS CODE IS USED TO SWITCH FROM RETAIL SPACE COOLING ELECTRICITY RATES (PELRSOUT(iDiv,iCurIYr,2)) TO WEIGHTED MARGINAL/WHOLESALE (PELME(iDiv,ICURIYR)) AND RETAIL RATE BLEND
          IF (DGrateBlend) THEN  !If set to TRUE in RSGENTK.txt, then use blended electricity rate starting in DGrateYr
            IF (CURCALYR.LT.DGrateYr) THEN
              xRetailElecPR=( PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
               +xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)     !own-use including scaled RPS credit
              xRetailElecPRadjRPS=( PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
               +1.0*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)     !own-use including scaled RPS credit
              xRetailElecPRnoRPS= PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
               *MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)                                            !own-use no RPS credit
            ELSEIF (CURCALYR.GE.DGrateYr) THEN
              xRetailElecPR=( ((PELME(iDiv,ICURIYR)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,ICURIYR,2)*DGretWt(iDiv)))*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
               +xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)     !own-use including scaled RPS credit
              xRetailElecPRadjRPS=( ((PELME(iDiv,ICURIYR)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,ICURIYR,2)*DGretWt(iDiv)))*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
               +1.0*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)     !own-use including scaled RPS credit
              xRetailElecPRnoRPS= ((PELME(iDiv,ICURIYR)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,ICURIYR,2)*DGretWt(iDiv)))*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
               *MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)                                            !own-use no RPS credit
            ENDIF
          ELSE
            xRetailElecPR=( PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
             +xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)     !own-use including scaled RPS credit
            xRetailElecPRadjRPS=( PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
             +1.0*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)     !own-use including scaled RPS credit
            xRetailElecPRnoRPS= PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
             *MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)                                            !own-use no RPS credit
          ENDIF

          !Compare annual PV generation to building use; value own-use generation at the retail price and grid sales at the grid price
          XEXCESSKWH=xAnnualKWh-xElecAvgUEC
          IF (XEXCESSKWH.LT.0.) THEN
            XVALESAVEBASE=xAnnualKWh*xRetailElecPr  ! own-use
          ELSE
            XVALESAVEBASE= &
             XEXCESSKWH*( xSalestoGridPR+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000.*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2) ) & !adds scaled RPS credit
             +xElecAvgUEC*xRetailElecPR             !plus own-use
          ENDIF
          !Recompute value of energy savings with RPS credit of 1 for years where credit can switch (per provisions of American Clean Energy and Security Act of 2009)
          IF (XEXCESSKWH.LT.0.) THEN
            XVALESAVEBASEadjRPS=xAnnualKWh*xRetailElecPradjRPS
          ELSE
            XVALESAVEBASE= xElecAvgUEC*xRetailElecPR   &                                                                   !own-use
             +XEXCESSKWH*( xSalestoGridPR+1.0*EPRPSPR(iCurIYr)/1000.*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2) ) !grid sales with scaled RPS credit
          ENDIF
          !Recompute value of energy savings without RPS credit for potential phase outs
          IF (XEXCESSKWH.LT.0.) THEN
            XVALESAVEBASEnoRPS=xAnnualKWh*xRetailElecPrnoRPS  ! own-use only
          ELSE
            XVALESAVEBASEnoRPS= XEXCESSKWH*xSalestoGridPR +xElecAvgUEC*xRetailElecPRnoRPS
          ENDIF

          !In case RPS credits come through in years before they should be credited to residential:
          IF (CurCalYr .LT. iRPSStartYear) THEN
            XVALESAVEBASE=XVALESAVEBASEnoRPS
          ENDIF

          !Zero out unused variables
          XBaseYrFUELCOST=0.
          XGASINPUT=0.
          XWATERHTGMMBTU=0.
          XBTUWASTEHEAT=0.
          XMAINTCOSTBASE=xMaintCst(NT,NV) * xCalcKW
          IF (LPRINT) WRITE (DGDAT,*) &
           "AC Price R&C", PELRSOUT(iDiv,iCurIYr,2), PELCMOUT(iDiv,iCurIYr,2), &
           "GridSalesPrice ", PELME(iDiv,iCurIYr), &
           "Deflators ", MC_JPGDP(iGenCapCostYr-BaseYr+1),MC_JPGDP(-2), &
           "RPS Credit (mills)", EPRPSPR(iCurIYr)
          !IF (LPRINT) WRITE(DGDAT,*) 'using old niche model', CurCalYr, iDiv, iNiche

          GOTO 200  !jump to cash flow model

          !****************************
          !SETUP FOR WIND CALCULATIONS
          !****************************
          100 CONTINUE

          !Wind generation is valued at the average residential electricity price.
          !Convert to same year dollars as generation capital costs and then use
          ! inflation to maintain nominal dollars. $/kWh in year dollars of capital cost data (iGenCapCostYr)
          xRetailElecPR=( PELRS(iDiv,iCurIYr)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
           +xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)     !own-use including scaled RPS credit
          xRetailElecPRadjRPS=( PELRS(iDiv,iCurIYr)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
           +1.0*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)     !own-use including scaled RPS credit
          xRetailElecPRnoRPS= PELRS(iDiv,iCurIYr)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
           *MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)     !own-use no RPS credit

          !Account for total households with potential for wind penetration
          ! Assume X% of households have lots of 0.5 acre or above and are appropriate
          WindAvailHH(iCurIYr,iDiv)= WindAvailHH(iCurIYr,iDiv) + &
           (HSEADD(CurCalYr,1,iDiv)+EH(CurCalYr,1,iDiv)) * xHHShare(iDiv,iNiche,iRateLevel) &
           *xRuralPctHH(iDiv,iNiche,iRateLevel)                                                    !Assumes rural households are suitable for wind

          !CALCULATION OF KWH SUPPLIED FOR WIND
          ! Wind speed is in m/s
          ! xElEff represents relative efficiency of future technologies relative to today's models
          ! xMpS is the wind speed in meters per second and capacity factor is a cubic function of
          !  wind speed (.0645 -0.0670*xMpS +.0210*xMpS**2 -.0011*xMpS**3).
          xMpS=xWindSpeed(iDiv,iNiche,iRateLevel)
          xAnnualKWh=xElEff(NT,NV)/xElEff(nt,1)* &
           (.0645 -0.0670*xMpS +.0210*xMpS**2 -.0011*xMpS**3)*xKW(NT,NV)*8760.*xLossFac(NT,NV)	!TODO - review/update values?

          !Compare annual Wind generation to building use, value own-use at the retail price and grid sales at the grid price
          XEXCESSKWH=xAnnualKWh-xElecAvgUEC
          IF (XEXCESSKWH.LT.0.) THEN
            XVALESAVEBASE=xAnnualKWh*xRetailElecPr  !own-use only
          ELSE
            XVALESAVEBASE= xElecAvgUEC*xRetailElecPR   &  !own-use
             +XEXCESSKWH*( xSalestoGridPR+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000.*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2) ) !grid sales w/ scaled RPS credit
          ENDIF

          !Recompute value of energy savings with RPS credit of 1 for years where credit can switch (per provisions of American Clean Energy and Security Act of 2009)
          IF (XEXCESSKWH.LT.0.) THEN
            XVALESAVEBASEadjRPS=xAnnualKWh*xRetailElecPradjRPS
          ELSE
            XVALESAVEBASE= xElecAvgUEC*xRetailElecPR   &  !own-use
             +XEXCESSKWH*( xSalestoGridPR+1.0*EPRPSPR(iCurIYr)/1000.*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2) ) !grid sales w/ scaled RPS credit
          ENDIF

          !Recompute value of energy savings without RPS credit for potential phaseouts
          IF (XEXCESSKWH.LT.0.) THEN
            XVALESAVEBASEnoRPS=xAnnualKWh*xRetailElecPrnoRPS  !own-use only
          ELSE
            XVALESAVEBASEnoRPS= XEXCESSKWH*xSalestoGridPR +xElecAvgUEC*xRetailElecPRnoRPS
          ENDIF

          !In case RPS credits come through in years before they should be credited to residential:
          IF (CurCalYr .LT. iRPSStartYear) THEN
            XVALESAVEBASE=XVALESAVEBASEnoRPS
          ENDIF

          !Zero out variables not relevant to wind
          XBaseYrFUELCOST=0.
          XGASINPUT=0.
          XWATERHTGMMBTU=0.
          XBTUWASTEHEAT=0.
          xCalcKW=xKW(NT,NV)
          XMAINTCOSTBASE=xMaintCst(NT,NV) * xCalcKW

          !End setup for wind calculations
          GOTO 200 !jump to cash flow model

          !****************************
          !SETUP FOR FUEL CELL CALCULATIONS
          !****************************
          150 CONTINUE

          !Fuel Cell generation is valued at the average residential electricity price.
          !Convert to same year dollars as generation capital costs and then use
          ! inflation to maintain nominal dollars. $/kWh in year dollars of capital cost data (iGenCapCostYr)
          xRetailElecPR=PELRS(iDiv,iCurIYr)*xRateScalar(iDiv,iNiche,iRateLevel) &
           *.003412*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)

          !COMPUTE ANNUAL KWH GENERATION
          xAnnualKWh=xOperHours(NT) * xAvail(NT,NV) * xKW(NT,NV) * xLossFac(NT,NV)
          !Average UEC for natural gas water heating (main & secondary) from RECS (Annual MMBtu per household)
          XWATERHTGMMBTU=17.5	!TODO - update?

          !COMPUTE FUEL INPUT IN MMBTU
          XGASINPUT=.003412 * xKW(NT,NV)/xElEff(NT,NV) * xOperHours(NT) * xAvail(NT,NV)	!TODO - update?

          !COMPUTE HEAT AVAILABLE FOR WATER HEATING IN MMBTU
          XBTUWASTEHEAT= (XGASINPUT-.003412 * xAnnualKWh)* xWhRecovery(NT,NV)
          IF (XBTUWASTEHEAT .LT. XWATERHTGMMBTU) XWATERHTGMMBTU=XBTUWASTEHEAT

          !COMPUTE ANNUAL FUEL COST FOR FUEL CELL -- NET OF IMPUTED WATERHEATING COSTS
          XBaseYrFUELCOST = (XGASINPUT-XWATERHTGMMBTU) &
           *PNGRS(iDiv,iCurIYr)*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)

          !Compare annual Fuel Cell generation to building use, value own-use generation at the retail price and grid sales at the grid price
          XEXCESSKWH=xAnnualKWh-xElecAvgUEC
          IF (XEXCESSKWH.LT.0.) THEN
            XVALESAVEBASE=xAnnualKWh*xRetailElecPr !own-use
          ELSE
            XVALESAVEBASE= XEXCESSKWH*xSalestoGridPR & !grid sales
             +xElecAvgUEC*xRetailElecPR !own-use
          ENDIF
          xCalcKW=xKW(NT,NV)
          XMAINTCOSTBASE=xMaintCst(NT,NV) * xCalcKW

          200 CONTINUE

          !************************************************
          !CALCULATE PAYBACKS BY AVAILABLE TECHNOLOGY TYPE
          !************************************************

          !IF (LPRINT) WRITE(DGDAT,*) ' CALCULATING SIMPLE PAYBACK'
          xCalcEqCost = xEqCost*xCalcKW/xKW(NT,NV)

          !CALCULATE ANNUAL LEVELIZED PAYMENT
          XINTRATE=MC_RMMTG30CON(iCurIYr)/100.
          XDOWNPAY=XDOWNPAYPCT*xCalcEqCost
          XPAYMENT=XINTRATE/(1.-(1.+XINTRATE)**(-1.*XTERM))*(xCalcEqCost-XDOWNPAY)
          IF (LPRINT) WRITE(DGDAT,*) ' PAYMENT',XPAYMENT,' Int Rate',XINTRATE

          !INITIALIZE CASH FLOW STARTING VALUES
          XCUMNETFLOW(1:35)=0.  !30+5 positions for "look ahead"
          XOUTLAY(1)=XDOWNPAYPCT*xCalcEqCost
          XFUELCOST(1)=0.
          XFUELCOST(2)=XBaseYrFUELCOST
          XMAINTCOST(1)=0.
          XMAINTCOST(2)=XMAINTCOSTBASE
          XLOANBAL(1)=xCalcEqCost*(1.-XDOWNPAYPCT)
          XTAXCREDIT(1)=0.
          XTAXCREDIT(2)=0.
          XTAXCREDIT(3)=xCalcEqCost*xTaxCreditPct

          !Apply cap if there is one
          IF (xtaxcreditmaxKW .GT. 0.) XTAXCREDIT(3)=min(XTAXCREDIT(3),XTAXCREDITMAxKW*xCalcKW)
          IF (xtaxcreditmax .GT. 0.) XTAXCREDIT(3)=min(XTAXCREDIT(3),XTAXCREDITMAX)

          !Add census division subsidy (if any) for renewable technologies
          XTAXCREDIT(3)=XTAXCREDIT(3)+xCalcEqCost*xTxCrPct_Div(NV,iDiv,NT)
          XNETCASHFLOW(1)=-XOUTLAY(1)
          XCUMNETFLOW(1)=-XOUTLAY(1)
          iIntervalYrstoUse=iIntervalYrs(NT,NV)

          DO IYR=2,30
            XOUTLAY(IYR)=0.0
            XVALESAVE(IYR)=0.
            xKWH(IYR)=0.
            IF (FLOAT(IYR).LE.XTERM+1.)XOUTLAY(IYR)=XPAYMENT
            XINTAMT(IYR)=XLOANBAL(IYR-1)*XINTRATE
            XPRIN(IYR)=0.0
            IF (FLOAT(IYR).LE.XTERM+1.)XPRIN(IYR)=XPAYMENT-XINTAMT(IYR)
            XLOANBAL(IYR)=XLOANBAL(IYR-1)-XPRIN(IYR)
            !CURRENTLY NO DEPRECIATION ALLOWANCE FOR RESIDENTIAL TAXES; KEEP FOR GENERALITY
            !XDEPR(IYR)=xCalcEqCost/XLIFE  !STRAIGHT-LINE DEPRECIATION
            XDEPR(IYR)=0.
            XTAXDEDUCT(IYR)=XTAXRATE*(XINTAMT(IYR-1)+XDEPR(IYR-1))+XTAXCREDIT(IYR)
            IF (IYR.GT.2) XFUELCOST(IYR)=0.
            IF (FLOAT(IYR).LE.(XLIFE+1.)) &
             XFUELCOST(IYR)=XFUELCOST(2)*(1.+XINFLATION)**(IYR-2)
            XMAINTCOST(IYR)=0.

            !INVERTERS:
            ! Calculate both annual and discrete maintenance costs. Initially designed to accommodate
            !  discrete solar PV inverter replacements; this is also used for wind.
            IF (iyr.GT.2 .AND. IMod(Iyr-2,iIntervalYrstoUse).EQ.0 .AND. Iyr.NE.29 .AND. Iyr.NE.30) THEN
              IF (FLOAT(IYR).LE.(XLIFE+1.)) &
                XMAINTCOST(IYR)=(XMAINTCOSTBASE+xIntervalCst(NT,NV)* xCalcKW)*(1.+XINFLATION)**(IYR-2)
                !Adjust the interval years for subsequent (if needed) discrete replacement
                !The use of IYR reflects "average" progress in extending inverter lives for subsequent replacements
                iIntervalYrstoUse=2*iIntervalYrstoUse+IYR
                !IF (lprint3) WRITE(dgdat,*) 'interval adjustment***  orig ', iyr, 'new ',iIntervalYrstoUse  !diagnostic only
              ELSE
                IF (FLOAT(IYR).LE.(XLIFE+1.)) &
                 XMAINTCOST(IYR)=XMAINTCOSTBASE*(1.+XINFLATION)**(IYR-2)
              ENDIF

              IF (FLOAT(IYR).LE.(XLIFE+1.)) THEN
                XVALESAVE(IYR)=XVALESAVEBASE * (1.+XINFLATION)**(IYR-2) * (1.-xDegradation)**(IYR-2)
                !Sunset the RPS if applicable
                IF (NT.EQ.1 .OR. NT.EQ.3) THEN
                  IF (CurCalYr+iyr-1 .GT. iRPSPhaseOutYear) &
                   XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-xDegradation)**(IYR-2)
                  !Adjust
                  IF (CurCalYr .GE. iRPSStartYear .AND. iyr .GT. iNumYrsatRPSBaseRate(nt,iCurIYr)) &
                   XVALESAVE(IYR)=XVALESAVEBASEadjRPS * (1.+XINFLATION)**(IYR-2) * (1.-xDegradation)**(IYR-2)
                  !Remove Credits if number of years < 30
                  IF (CurCalYr .GE. iRPSStartYear .AND. iyr .GT. iNumRPSCreditYrs(nt,iCurIYr)) &
                   XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-xDegradation)**(IYR-2)
                  !Remove Credits if CurCalYr is before start of RPS credits
                  IF (CurCalYr .LT. iRPSStartYear) &
                   XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-xDegradation)**(IYR-2)
                ENDIF
              ENDIF

              IF (FLOAT(IYR).LE.(XLIFE+1.)) xKWH(IYR)=xAnnualKWh * (1.-xDegradation)**(IYR-2)
              XNETCASHFLOW(IYR)=-XOUTLAY(IYR)-XFUELCOST(IYR)-XMAINTCOST(IYR) &
               +XTAXDEDUCT(IYR)+XVALESAVE(IYR)
              XCUMNETFLOW(IYR)=XCUMNETFLOW(IYR-1)+XNETCASHFLOW(IYR)
          ENDDO  !cash flow year loop (iyr)

          !Print Switch for Detailed Cash Flow Model Results
          IF (LPRINT.AND.LPRINT2.AND.iDiv.EQ.1.AND.iNiche.EQ.1.AND.iratelevel.EQ.1) &
           WRITE(DGDAT,*) 'YEAR    OUTLAY TAXDEDUCT  FUELCOST MAINTCOST    ESAVE  NETCASHFL   CUM   Annual kWh', aEquipName(NT,NV)

          iPayback(1:30)=1

          !SEARCH FOR POSITIVE CASH FLOW, PERSISTING FOR SEVERAL YEARS
          ! USE "0" TO INDICATE POSITIVE CASH FLOW MEETING PERSISTENCE CRITERIA
          DO IYR=1,20 !Stop at 20 because of look-ahead; assume that greater than 20-year paybacks are too long
            IF (XCUMNETFLOW(iyr).GE.0. .AND. XCUMNETFLOW(iyr+1).GE.0. .AND. &
             xcumnetflow(iyr+2).GE.0. .AND. xcumnetflow(iyr+3).GE.0. .AND. &
             xcumnetflow(iyr+4).GE.0. .AND. xcumnetflow(iyr+5).GE.0. .AND. &
             xcumnetflow(iyr+6).GE.0. .AND. xcumnetflow(iyr+7).GE.0. .AND. &
             xcumnetflow(iyr+8).GE.0.) iPayback(IYR)=0
            !SWITCHABLE DIAGNOSTICS:
            IF (LPRINT.AND.LPRINT2.AND.iDiv.EQ.1.AND.iNiche.EQ.1.AND.iratelevel.EQ.1) &
              WRITE(DGDAT,10)IYR-1,XOUTLAY(IYR),XTAXDEDUCT(IYR),XFUELCOST(IYR), &
               XMAINTCOST(IYR), XVALESAVE(IYR), XNETCASHFLOW(IYR), &
               XCUMNETFLOW(IYR),xKWH(IYR),iPayback(iyr),xAnnualKWh
          ENDDO
          10 FORMAT(1X,I4,8F10.2,i4,f10.2)


          xSimplePayback=29.
          ilife=nint(xlife)
          DO IYR=1,30
            IF (iPayback(iyr).EQ.0) THEN
              xSimplePayback=FLOAT(IYR-1)  !Allow 1-year and less simple paybacks
              !Get the first year of a positive cumulative cash flow and compute how long at that year's net cash flow it would take to build the cumulative balance in the first positive year.
              ! The interpolated years to positive cash flow are then equal to the simple payback years minus ending cumulative cash flow balance / net cash flow in "iYr".
              IF (xSimplePayback.LT.1.) xSimplePayback=1.
              IF (iyr.LT.15) xSimplePayback= xSimplePayback - XCUMNETFLOW(IYR)/XNETCASHFLOW(IYR) !relax distributed generation cap in new
              !Diagnostic warning
              IF (xSimplePayback .LT. 0.) THEN
                !WRITE(DGDAT,*) '**Negative Payback** CurCalYr,NT,NV,iDiv',CurCalYr,NT,NV,iDiv,'EQ CLASS ', & !Turned off for DGrate testing
                ! aEquipName(NT,NV), '1ST YEAR ', iFirstYr(NT,NV), 'PAYBACK=', xSimplePayback, 'xPen= ',xPen !Turned off for DGrate testing
              ENDIF
              GOTO 11  !CONTINUE once payback is identified
            ENDIF
            !If here, investment never achieves positive cash flow; payback is set to 29 years
          ENDDO

          !END OF CALCULATE PAYBACK

              !********************************************
              !CALCULATE PENETRATION-BASED PAYBACK PERIOD
              !********************************************
              11 CONTINUE

              xMaxPen=xPenParm/xSimplePayback
              !Maximum penetration into new construction capped at 75%.	!TODO - review/update values?
              ! The cap would affect projects with paybacks of less than approximately 5 months.
              XValue=FLOAT(CurCalYr-(RECSYear+1))
              IF (XValue.GT.25.0) XValue=25.0  !currently limit penetration beyond 2030	!TODO - hasn't been updated since 2005 was base year; 25 needs to be changed, or this line needs to be removed
              xPen=min(0.75,xMaxPen-xMaxPen/(1.+xMaxPen*EXP(xAlpha*(XValue-xSimplePayback))))	!TODO - review/update values?

              IF (LPRINT) WRITE(DGDAT,*) 'CurCalYr,NT,NV,iDiv',CurCalYr,NT,NV,iDiv,'EQ CLASS ', & !Turned off for DGrate testing
               aEquipName(NT,NV),'1ST YEAR ',iFirstYr(NT,NV),'PAYBACK=',xSimplePayback,'xPen= ',xPen !Turned off for DGrate testing

              !CODE TO PRINT 20 YEARS OF PENETRATION DATA FOR TESTING
              !DO IYR=CurCalYr,CurCalYr+20
              !  xPen2=xMaxPen-xMaxPen/(1.+xMaxPen*EXP(xAlpha*((iyr-(RECSYear+1))-xSimplePayback)))
              !  IF (LPRINT) WRITE(DGDAT,'(1X,4I8,4e16.3)') CurCalYr,RECSYear,IYR,NV,xMaxPen,xAlpha,xSimplePayback,xPen2
              !  IF (LPRINT) WRITE(DGDAT,*) 'xPen2',CurCalYr,RECSYear,IYR,NV,xAlpha,xMaxPen,xSimplePayback,xPen2
              !ENDDO

              !Turn off endogenous builds to avoid double-counting of historical data
              ! (Increasing LastSTEOYear in RESDREP include file for STEO benchmarking prior to updating
              ! exogenous capacity will cause PV to nearly flatten out between SEDS year and MER year)

              !Solved issue by creating variable set in RSGENTK.txt input file with last year of historical data (historical capacity is input as exogenous capacity)
              IF (CurCalYr.LE.iExogHistYr(NT)) xPen=0.0  !No endogenous model builds for any DG technology in years with historical data
			  
              !Account for penetration into existing housing units
              ! Penetration into existing is based on penetration into new construction with an assumed upper bound
              xExistPen=min(xPen/40.0,0.005)  !Penetration cap for existing	!TODO - review/update values?
			  
              xTemp=xPen*xInxDecay(iDiv,iCurIYr)*HSEADD(CurCalYr,1,iDiv) &          !Penetration into New Construction !INXLIMIT
               +(xExogPen(iCurIYr,iDiv,NT)-xExogPen(iCurIYr-1,iDiv,NT))/xCalcKW  &  !Add Current Year Exogenous Units  !convert to kW
               +xExistPen*xInxDecay(iDiv,iCurIYr)*(EH(CurCalYr,1,iDiv)-Units(iCurIYr-1,iDiv,NT))  !Existing Construction
              xTemp=xTemp*xHHShare(iDiv,iNiche,iRateLevel)  !Scale down to suitable HH for niche share of HH
              xTemp = FLOAT(nint(xTemp*100.+.5))/100.  !Eliminate fractional units < 0.01
              xTempHH=(HSEADD(CurCalYr,1,iDiv)+EH(CurCalYr,1,iDiv))*xHHShare(iDiv,iNiche,iRateLevel)  !HH in niche
              !END OF CALCULATE PENETRATION-BASED PAYBACK PERIOD
			  
              !Accumulators for Technical Potential and Other Summary Statistics
              ! Note xSqftperKW varies by year and is calculated above
              IF (NT.EQ.1) THEN
                SolarPVTechPotentialMW(iCurIYr,iDiv)=SolarPVTechPotentialMW(iCurIYr,iDiv)+ &
                 (xTempHH*xpctPVSuitable*xRoofAreaperHH(iDiv,iNiche,iRateLevel)/(xSqftperKW*1000.))
                SolarPVInstalledMW(iCurIYr,iDiv)=SolarPVInstalledMW(iCurIYr,iDiv)+xTemp*xCalcKW/1000.
                SolarPVAvailRoofArea(iCurIYr,iDiv)= SolarPVAvailRoofArea(iCurIYr,iDiv) + &
                 (xTempHH*xpctPVSuitable*xRoofAreaperHH(iDiv,iNiche,iRateLevel)/10.**6)
                SolarPVUsedRoofArea(iCurIYr,iDiv)=SolarPVUsedRoofArea(iCurIYr,iDiv)+(xTemp*xCalcKW*xSqftperKW/10**6)
              ENDIF
              IF (NT.EQ.3) THEN
                WindTechPotentialMW(iCurIYr,iDiv)=WindTechPotentialMW(iCurIYr,iDiv)+xTempHH*xCalcKW/1000.*xRuralPctHH(iDiv,iNiche,iRateLevel)
                WindInstalledMW(iCurIYr,iDiv)=WindInstalledMW(iCurIYr,iDiv)+xTemp*xCalcKW/1000.
                WindAvailHH(iCurIYr,iDiv)=WindAvailHH(iCurIYr,iDiv)+xTempHH*xRuralPctHH(iDiv,iNiche,iRateLevel)
              ENDIF
              !END ACCUMULATORS AND OTHER SUMMARY STATISTICS
			  
              !Now that new units are determined, calculate associated estimates of generation, capacity,
              ! own-use generation, fuel usage, offsets to energy consumption for hot water & space heating and investment.
              xUnits = xUnits + xTemp
              xTrills = xTrills + xTemp*xAnnualKWh*3412./10.**12  !trillion Btu
              xCapacity = xCapacity + xTemp*xCalcKW  !in kW(dc) for now
              IF (xAnnualKWh.GT.xElecAvgUEC) THEN
                xTrillsOwnUse = xTrillsOwnUse + xTemp*xElecAvgUEC*3412./10.**12
              ELSE
                !BUILDING CONSUMES ALL OF ITS OWN GENERATION
                xTrillsOwnUse = xTrillsOwnUse + xTemp*xAnnualKWh*3412./10.**12
              ENDIF

              xfuelusage = xfuelusage + xTemp*xgasinput/10.**6  !trillion Btu
              xhwbtu = xhwbtu +xTemp*XWATERHTGMMBTU/10.**6  !trillion Btu
              xInvest = xInvest + xTemp*xCalcEqCost/10.**6  !$million

          !Label 25 for skipping unpopulated niches
          25 CONTINUE

        ENDDO   !iRateLevel
      ENDDO  !iNiche

      GOTO 81  !SKIP OVER SOLAR PV HURDLE MODEL CODE IF HERE

      26 CONTINUE

      !**************************************************************
      !ZIP CODE-BASED SOLAR PV HURDLE MODEL; USEZIPMODEL IF HERE
      !**************************************************************
      !THIS IS JUST OUTSIDE OF THE RATE LEVEL, CLIMATE ZONE NICHE LOOPS
      ! ALL RESULTS HERE ARE FOR CENSUS DIVISIONS
      xSqftperKW=77.*.14/xElEff(NT,NV)

      !National and census division-level input variables
      IF (CurCalYr.GT.EstYear) THEN
        INTRATE=(MC_RMMTG30CON(iCurIYr)/100.)  !convert to a decimal fraction

        !Add the divisional tax credit into the calculation of PVPRICE in the line of code below
        ! PVPrice equals RSGENTK installed equipment cost, net of any tax credit, times an adjustment factor to scale to the econometric ZIP code model's cost level [PVPrice in RGENTK]
        PVPrice=(xadjcost+xInstCost(NT,NV))*(1.-xtaxcreditpct-xTxCrPct_Div(nv,iDiv,nt) )*InputPVPrice/(xCapCost(1,EstYear-RECSYear+1)/1000*(1-xTxCrPct(1,EstYear-RECSYear+1)))
        !IF (LPRINT) WRITE(DGDAT,*) 'PVmultiplier_calc', InputPVPrice/(xCapCost(1,EstYear-RECSYear+1)/1000*(1-xTxCrPct(1,EstYear-RECSYear+1))), 'EstYear-RECSYear+1', EstYear-RECSYear+1, &
        !'xCapCost(1,EstYear-RECSYear+1)', xCapCost(1,EstYear-RECSYear+1), 'xTxCrPct(1,EstYear-RECSYear+1)', xTxCrPct(1,EstYear-RECSYear+1)

        !PVPrice=(xAdjCost+xInstCost(NT,NV))*(1.-xTaxCreditPct-xTxCrPct_Div(nv,iDiv,nt) )*InputPVPrice/((xCapCost(1,EstYear-RECSYear+1)+xInstCost(1,EstYear-RECSYear+1))/1000*(1-xTxCrPct(1,EstYear-RECSYear+1)))	!TODO - The line above should use total installed cost, not just capital cost. Test this code further, but verify costs to sum)
        !IF (LPRINT) WRITE(DGDAT,*) 'PVmultiplier_calc', InputPVPrice/((xCapCost(1,EstYear-RECSYear+1)+xInstCost(1,EstYear-RECSYear+1))/1000*(1-xTxCrPct(1,EstYear-RECSYear+1))), 'EstYear-RECSYear+1', EstYear-RECSYear+1, &    !PVmultiplier	!TODO - The line above should use total installed cost, not just capital cost. Test this code further, but verify costs to sum)
        !'xCapCost(1,EstYear-RECSYear+1)', xCapCost(1,EstYear-RECSYear+1), 'xInstCost(1,EstYear-RECSYear+1)', xInstCost(1,EstYear-RECSYear+1), 'xTxCrPct(1,EstYear-RECSYear+1)', xTxCrPct(1,EstYear-RECSYear+1)	!TODO - The line above should use total installed cost, not just capital cost. Test this code further, but verify costs to sum)

        MonthlyPayment= ( PVPRICE * (INTRATE/12.) / (1.-(1.+INTRATE/12.)**(-360.)) ) !assume a 360 month mortgage
        !IF (LPRINT) WRITE(DGDAT,*) 'USING NEW MODEL National Level Variables ', IntRate, PVPrice, MonthlyPayment
      ENDIF

      !Process ZIP codes
      DO i=1,NumZIPs
        IF (CenDiv(i) .NE. iDiv) CYCLE
        xSolarIns=INSOL(i)                                                 !PVgen
        !Transform solar insolation to account for unknown orientations    !PVgen
        xSolarIns= -1.0533 + 1.4325*xSolarIns - 0.0652*xSolarIns**2        !PVgen	!TODO - update values using PVwatts?

        xCalcKW=xKW(NT,NV)  !set size to menu capacity

        xAnnualKWh=xElEff(NT,NV)*xSolarIns*365.25/10.8*xSqftperKW*xCalcKW*xLossFac(NT,NV)	!TODO - replace 365.25 with reference to LEAPYR?

        !j is the model switch based on the 3 sets of coefficients
        !  if PureHurdle is zero, THEN j = 1 using the first subscript position for coefficients
        !  if PureHurdle is 1, THEN j = 2 using the 2nd subscript position for coefficients (urban model)
        !  if PureHurdle is 0, and ruralzip is 1, THEN j = 3 using the 3rd subscript position for coefficients (rural model)
        j = PureHurdle(i)+1
        IF (ruralzip(i) .EQ. 1) j=3
        !The first array element of the coefficient is for the logit model, the 2nd for the negative binomial.
        ! Not all variables for specific model variants are non-zero, but this is programmed to allow flexible model evolution.

        !Set inputs to equations
        IF (curitr.EQ.1 .AND. CurCalYr.GT.EstYear) THEN  !Subsequent to the estimation year update lag variables on first iteration
          Income_L(i)=Income(i)
          Households_L(i)=Households(i)
          ElecRate_L(i)=ElecRate(i)
          PopDensity_L(i)=PopDensity(i)
          Lag2Installs(i)=Lag1Installs(i)
          Lag1Installs(i)=ProjectedInstalls(i)
          CumUnits_L(i)=CumUnits(i)
        ENDIF !curitr=1 for years after econometric model estimation year

        IF (CurCalYr.GT.EstYear) THEN  !Subsequent to the estimation year update ZIP code level variables
          Income(i)=Income_L(i)*(MC_YPDR(CenDiv(i),iCurIYr)/MC_YPDR(CenDiv(i),iCurIYr-1)) / &
           ( ( EH(CurCalYr,1,iDiv)+NH(CurCalYr,1,iDiv)+EH(CurCalYr,2,iDiv)+NH(CurCalYr,2,iDiv)+EH(CurCalYr,3,iDiv)+NH(CurCalYr,3,iDiv) ) &
           / ( EH(CurCalYr-1,1,iDiv)+NH(CurCalYr-1,1,iDiv)+EH(CurCalYr-1,2,iDiv)+NH(CurCalYr-1,2,iDiv)+EH(CurCalYr-1,3,iDiv)+NH(CurCalYr-1,3,iDiv) ) )
          Households(i)=Households_L(i) * (EH(CurCalYr,1,iDiv)+  NH(CurCalYr,1,iDiv)+  EH(CurCalYr,2,iDiv)+  NH(CurCalYr,2,iDiv)+  EH(CurCalYr,3,iDiv)+  NH(CurCalYr,3,iDiv)) &
           /(EH(CurCalYr-1,1,iDiv)+NH(CurCalYr-1,1,iDiv)+EH(CurCalYr-1,2,iDiv)+NH(CurCalYr-1,2,iDiv)+EH(CurCalYr-1,3,iDiv)+NH(CurCalYr-1,3,iDiv))
          PopDensity(i)=PopDensity_L(i)*MC_NP65A(iCurIYr)/MC_NP65A(iCurIYr-1)

          !THIS CODE IS USED TO SWITCH FROM RETAIL SPACE COOLING ELECTRICITY RATES (PELRSOUT(iDiv,iCurIYr,2)) TO WEIGHTED MARGINAL/WHOLESALE (PELME(iDiv,ICURIYR)) AND RETAIL RATE BLEND
          IF (DGrateBlend) THEN  !If set to TRUE in RSGENTK.txt, then use blended electricity rate starting in DGrateYr
            IF (CURCALYR.LT.DGrateYr) THEN
              ElecRate(i)=ElecRate_L(i)*( (PELRSOUT(CenDiv(iDiv),iCurIYr,2)*.003412+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. ) ) / &    !own-use including scaled RPS credit
               ( (PELRSOUT(CenDiv(iDiv),iCurIYr-1,2)*.003412+xRPS(nt,iCurIYr-1)*EPRPSPR(iCurIYr-1)/1000. ) )
            ELSEIF (CURCALYR.EQ.DGrateYr) THEN  !This is a transition where where the prior year uses 100% retail space cooling electricity rate [PELRSOUT(D,Y,2)] and the current year uses weighted retail/marginal electricity rate
              ElecRate(i)=ElecRate_L(i)*( (((PELME(iDiv,iCurIYr)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,iCurIYr,2)*DGretWt(iDiv)))*.003412+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. ) ) / &    !own-use including scaled RPS credit
               ( (PELRSOUT(CenDiv(iDiv),iCurIYr-1,2)*.003412+xRPS(nt,iCurIYr-1)*EPRPSPR(iCurIYr-1)/1000. ) )
            ELSEIF (CURCALYR.GT.DGrateYr) THEN
              ElecRate(i)=ElecRate_L(i)*( (((PELME(iDiv,iCurIYr)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,iCurIYr,2)*DGretWt(iDiv)))*.003412+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. ) ) / &    !own-use including scaled RPS credit
              ( (((PELME(iDiv,iCurIYr-1)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,iCurIYr-1,2)*DGretWt(iDiv)))*.003412+xRPS(nt,iCurIYr-1)*EPRPSPR(iCurIYr-1)/1000. ) )
            ENDIF
          ELSE
            ElecRate(i)=ElecRate_L(i)*( (PELRSOUT(CenDiv(iDiv),iCurIYr,2)*.003412+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. ) ) / &    !own-use including scaled RPS credit
             ( (PELRSOUT(CenDiv(iDiv),iCurIYr-1,2)*.003412+xRPS(nt,iCurIYr-1)*EPRPSPR(iCurIYr-1)/1000. ) )
          ENDIF
        ENDIF

        !PV CONTAGION EFFECT
        ! Here we calculate adjustment factors for the lag coefficients clag1 and clag2.
        ! The factors will be decreasing functions of the number of years between the projection year 
        ! (CurCalYr) and the estimation year for the econometric model (EstYear).  
        ! The factors will represent the decay in the social or “contagion effect” of solar PV.

        !Calculate factors; not all factors used, but all included here to illustrate different options
        ! NOTE: declared as REAL*8 above
        IF (CurCalYr.GT.EstYear) THEN  !to avoid dividing by zero
          NumYearsPV = CurCalYr-EstYear
          factor0 = 1                      !Used to turn off solar PV contagion effect decay
          factor1 = 1/(NumYearsPV**0.050)
          factor2 = 1/(NumYearsPV**0.055)
          factor3 = 1/(NumYearsPV**0.060)
          factor4 = 1/(NumYearsPV**0.090)  !Meant to align with Stanford DeepSolar data incorporation
          factor5 = 1/(NumYearsPV**0.12)   !Added to curb growth after the ITC incentive expiration
          factor6 = 1/(NumYearsPV**0.18)
          factor7 = 1/(NumYearsPV**0.5)
          !factor5 = exp(-NumYearsPV)  !compiler doesn't appear to like negative exponential argument
          !factor6 = exp(2*(-NumYearsPV))  !compiler doesn't appear to like negative exponential argument
        ENDIF

        !For each individual test, we set factor equal to one of the test values
        IF (xTxCrPct(1,CurCalYr - RECSYear + 1) .GT. 0.0) THEN !For all years before ITC phaseout
          factor = factor4
        ELSE  !For after ITC phaseout/expiration
          factor = factor5
        ENDIF

        !Calculate numerator and denominator components for projections (these are REAL*8 to prevent overflows)
        xLogit= cint (1,j) + &
         chh (1,j)*HouseHolds(i) +           &
         cpd (1,j)*PopDensity(i) +           &
         factor*cinc (1,j)*Income(i) +       & 
         factor*cer (1,j)*ElecRate(i) +      &
         ccdd (1,j)*LagCDD(i) +              &
         cpmt (1,j)*MonthlyPayment +         &
         cir (1,j)*INTRATE +                 &
         factor*clag1(1,j)*Lag1Installs(i) + &
         factor*clag2(1,j)*lag2Installs(i) + &  
         cpvp(1,j)*PVPRICE +                 &
         cins (1,j)*Insol(i)
        xNegBinom= cint (2,j) + &
         chh (2,j)*HouseHolds(i) + &
         cpd  (2,j)*PopDensity(i)   + &
         factor*cinc (2,j)*Income(i) + &  
         factor*cer (2,j)*ElecRate(i)   + &
         ccdd (2,j)*LagCDD(i)       + &
         cpmt (2,j)*MonthlyPayment  + & 
         cir (2,j)*INTRATE       + &
         factor*clag1(2,j)*Lag1Installs(i) + &
         factor*clag2(2,j)*lag2Installs(i) + &  
         cpvp(2,j)*PVPRICE       + &
         cins (2,j)*Insol(i)

        !Note that Logit(18) = 1; we use this to prevent overflow when we exponentiate	!TODO(EES) - Verify significance of 18 
        IF (xLogit .GE. 18.) THEN 
          xLogit = 1.
        ELSE
          xLogit = exp(xLogit)/(1.+exp(xLogit))
        ENDIF

        !This is "enforcing the hurdle," where we say that, if there is a ZIP code with a low probability
        ! of install, we set the expectation to be zero. This helps curtail explosion.	!TODO(EES) - What is meant by "explosion?" Model crashes? Snowballing capacity growth?
        IF (xLogit .LT. 0.25) xLogit = 0	!TODO(EES) - What is the significance of 0.25?

        !Compute the predicted number of installs
        IF (xNegBinom .GE. 50.) THEN	!TODO(EES) - What is the significance of 50?
          xTemp = REAL(xLogit * exp(50.))
        ELSE
          xTemp = REAL(xLogit * exp(xNegBinom))
        ENDIF

        !Final null check, just in case 
        IF (CurCalYr .GT. EstYear) xTemp= xTemp*xInxDecay(iDiv,iCurIYr)
        IF (ISNaN(xTemp)) xTemp = 0.

        !Cap at 80% of households 
        IF ((CumUnits_L(i) + xTemp)/(HouseHolds(i)) .GE. 0.80) THEN 
          xTemp = 0.80*HouseHolds(i) - CumUnits_L(i)
          IF (xTemp .LE. 0) xTemp = 1.
        ENDIF

        !If a household has achieved near 80% of households, set to 80% and keep it that way for the rest of the model
        IF (CumUnits_L(i) .GE. .78 * HouseHolds_L(i)) THEN
          CumUnits(i) = .8 * HouseHolds(i)
          ProjectedInstalls(i) = HouseHolds(i)
          xTemp = CumUnits(i) - CumUnits_L(i)
          IF (xTemp .LE. 0) xTemp = 1.
        ELSE
          ProjectedInstalls(i) = xTemp
          CumUnits(i) = CumUnits_L(i) + xTemp
        ENDIF
        xUnits = xUnits + xTemp

        !Add tax credit for penetration calculation of payment (?)
        xTrills= xTrills + xTemp*xAnnualKWh*3412./10.**12  !beware of mixed mode: bad results if coded as "/10**12"

        xElecAvgUEC= 0.
        xElecAvgUEC= ((RSFLCN(iCurIYr,2,iDiv) * 10.**12) / 3412.) / (EH(CurCalYr,1,iDiv) + NH(CurCalYr,1,iDiv) + EH(CurCalYr,2,iDiv) +  &  !consumption includes onsite own-use generation
         NH(CurCalYr,2,iDiv) + EH(CurCalYr,3,iDiv) + NH(CurCalYr,3,iDiv))  !Average electricity consumption (kWh) per household

        IF (xAnnualKWh.GT.xElecAvgUEC) THEN  !If PV generation exceeds average consumption per household...
          xTrillsOwnUse = xTrillsOwnUse + xTemp*xElecAvgUEC*3412./10.**12
        ELSE
          !BUILDING CONSUMES ALL OF ITS OWN GENERATION
          xTrillsOwnUse = xTrillsOwnUse + xTemp*xAnnualKWh*3412./10.**12
        ENDIF

        IF (xTrillsOwnUse .GT. xTrills) THEN
         !Prevents negative sales to grid
         xTrillsOwnUse= xTrills
        ENDIF

        IF (xTrillsOwnUse .LT. 0.) THEN
         !Prevents negative generation for own use
         xTrillsOwnUse= 0.
        ENDIF

      ENDDO !Process ZIP codes

      !Print DGrate variables to RDM_DGENOUT.txt
      IF (LPRINT .AND. CURITR.EQ.1 .AND. IDIV.EQ.1) THEN  !write header only during first iteration and before first census division
        WRITE(DGDAT,*) 'CD ICURIYR Cal_Year Blended_ElecRate PELRSOUT PELME'
      ENDIF

      IF (LPRINT .AND. CURITR.EQ.1) THEN  !write header only during first iteration
        WRITE(DGDAT,909) IDIV,ICURIYR,CurCalYr,((PELME(IDIV,ICURIYR)*DGmargWt(IDIV))+(PELRSOUT(IDIV,ICURIYR,2)*DGretWt(IDIV))),PELRSOUT(IDIV,ICURIYR,2),PELME(IDIV,icuriyr)
      ENDIF
      909 FORMAT(I2,1X,I2,1X,I4,4F8.4)

      !CREATE CENSUS DIVISION RESULTS FOR PV (NT.EQ.1) HERE
      !IF (LPRINT) WRITE(DGDAT,*) 'Using solar PV hurdle model', CurCalYr, iDiv, xUnits, xTrills
      Units(iCurIYr,iDiv,NT)=Units(iCurIYr-1,iDiv,NT)+ xUnits
      xCapacity=xUnits*xCalcKW  !in kW(dc)
      Cap(iCurIYr,iDiv,NT)=Cap(iCurIYr-1,iDiv,NT)+ xCapacity

      !xTrills= xUnits*xAnnualKWh*3412./10.**12  !insert NREL equation; this is a per-kW calculation so multiply by above and convert to trillion Btu
      Trills(iCurIYr,iDiv,NT)=Trills(iCurIYr-1,iDiv,NT)+ xTrills
      !xTrillsOwnUse=xTrills   !placeholder; base this on RECS by census division  !This placeholder was never filled.  Calculations added above

      TrillsOwnUse(iCurIYr,iDiv,NT)= TrillsOwnUse(iCurIYr-1,iDiv,NT) +xTrillsOwnUse
      xInvest=xUnits*xEqCost/10.**6
      x111dRenSub(iCurIYr,iDiv,nt)=xInvest*xTxCrPct_Div(nv,iDiv,nt)

      Invest(iCurIYr,iDiv,NT)=xInvest  !($mill)

      IF (iCurIYr .LE. EstYear-BaseYr+2 .AND. PVzipcalib .AND. NT .EQ. 1) THEN    !Used to calibrate to historical exogenous PV capacity in EstYear and the following year    !PVzipcalib	!TODO - should the +2 be set to <PVzipcalibyear, or ExogPVlastDataYear/ExogPVhistYear>-ESTYEAR?
        ExogPVMistie(iCurIYr,iDiv)= 0.                                                                                                                        !PVzipcalib
        !xExogPen is exogenous PV capacity (NT=1) in kW from RSGENTK in index year (EstYear-BaseYr+1; 2020=31)                                                !PVzipcalib
        ExogPVMistie(iCurIYr,iDiv)= xExogPen(iCurIYr,iDiv,1) - Cap(iCurIYr,iDiv,1)                                                                            !PVzipcalib
        !WRITE(DGDAT,*)  'ExogPVMistie_Test_1', iCurIYr, iDiv, xExogPen(iCurIYr,iDiv,1), Cap(iCurIYr,iDiv,1), ExogPVMistie(iCurIYr,iDiv), xUnits, &   !Test-write ExogPVMistie data to RDM_DGENOUT.txt to verify            !PVzipcalib
        ! xCalcKW, xAnnualKWh, xElecAvgUEC, ( EH(CurCalYr,1,iDiv) + NH(CurCalYr,1,iDiv) + EH(CurCalYr,2,iDiv) + NH(CurCalYr,2,iDiv) + EH(CurCalYr,3,iDiv) + &  !PVzipcalib
        ! NH(CurCalYr,3,iDiv)), QELRS(iDiv,iCurIYr), Trills(iCurIYr,iDiv,1), TrillsOwnUse(iCurIYr,iDiv,1)                                                      !PVzipcalib
        !Cap(iCurIYr,iDiv,1)= Cap(iCurIYr,iDiv,1) + ExogPVMistie(iCurIYr,iDiv)                                                                                  !PVzipcalib
        Cap(iCurIYr,iDiv,1) = xExogPen(iCurIYr,iDiv,1) ! These two are identical, but this is more clear about what is going on
        !Adjust units based on what we want the capacity to be
        Units(iCurIYr,iDiv,1)= Units(iCurIYr,iDiv,1) + (ExogPVMistie(iCurIYr,iDiv) / xCalcKW)                                                                 !PVzipcalib
       
        !Adjust trills as well 
        xUnits=Units(iCurIYr,iDiv,1)-Units(iCurIYr-1,iDiv,1)
        xCapacity=xUnits*xCalcKW                                                                                                                              !PVzipcalib
        xInvest=xUnits*xEqCost/10.**6
        x111dRenSub(iCurIYr,iDiv,1)=xInvest*xTxCrPct_Div(nv,iDiv,1)                                                                                           !PVzipcalib
        Trills(iCurIYr,iDiv,1)= Trills(iCurIYr,iDiv,1) + (ExogPVMistie(iCurIYr,iDiv) / xCalcKW) *xAnnualKWh*3412./10.**12                                     !PVzipcalib

        !Test for negative generation after exogenous PV calibration                                                                                          !PVzipcalib
        IF (Trills(iCurIYr,iDiv,1) .LT. 0.) Trills(iCurIYr,iDiv,1)=0.                                                                                         !PVzipcalib

        !Share out generation of ExogPVMistie to own-use generation rather than putting it all into both Trills and TrillsOwnUse                              !PVzipcalib
        TrillsOwnUse(iCurIYr,iDiv,1)= TrillsOwnUse(iCurIYr,iDiv,1) + (TrillsOwnUse(iCurIYr,iDiv,1)/Trills(iCurIYr,iDiv,1)) * &                                !PVzipcalib
         (ExogPVMistie(iCurIYr,iDiv) / xCalcKW) *xAnnualKWh*3412./10.**12                                                                                     !PVzipcalib

        !Test for negative own-use generation after exogenous PV calibration                                                                                  !PVzipcalib
        IF (TrillsOwnUse(iCurIYr,iDiv,1) .LT. 0.) TrillsOwnUse(iCurIYr,iDiv,1)=0.                          !PVzipcalib

        Invest(iCurIYr,iDiv,NT)=xInvest  !($million)
        !IF (CURITR .EQ. 1) THEN
        !  OPEN(unit = 667, file = "pv_hh.txt", action="write", position="append") !reopen the file, with append
        !    WRITE(667,94) 1, ",", iCurIYr, ",", iDiv, ",", xExogPen(iCurIYr,iDiv,1), ",", ExogPVMistie(iCurIYr, iDiv), ",", Cap(iCurIYr,iDiv,1), ",", Units(iCurIYr,iDiv,1), ",", xCalcKW, ",", xInvest, ",", x111dRenSub(iCurIYr,iDiv,1), ",", Trills(iCurIYr,iDiv,1)
        !    94 FORMAT(    I,   A,       I,  A,     I,   A,                 F,A,          F,   A,                   F,   A,                     F,   A,       F,   A,       F,   A,                           F,   A,  F)
        !  CLOSE(667)
        !ENDIF !PVzipcalib

      ENDIF !PVzipcalib
       
      !The PV model is additive, so once it has been benchmarked to historical capacity through iExogHistYr, we don't need to continue
      ! to adjust the output to exogenous capacity (it has already been bumped up or down enough)
      
      GOTO 82  !DON'T REDO DIVISION-LEVEL CALCULATIONS FOR PV IF USING ECONOMETRIC MODEL
      !END OF ZIP CODE-BASED SOLAR PV HURDLE MODEL

      81 CONTINUE  !END OF SOLAR PV HURDLE MODEL CODE

      !DIVISION-LEVEL CALCS IF USING THE NICHE MODEL INSTEAD OF THE ECONOMETRIC MODEL
      !IF (LPRINT) WRITE(DGDAT,*) 'KWH ',xAnnualKWh
      !IF (LPRINT) WRITE(DGDAT,*) 'xTemp ', xTemp, 'xPen ',xPen
      !IF (LPRINT) WRITE(DGDAT,*) 'HSEADD ',HSEADD(CurCalYr,1,iDiv)
      !IF (LPRINT) WRITE(DGDAT,*) 'EXOG PEN ' , xExogPen(iCurIYr,iDiv,NT)

      Units(iCurIYr,iDiv,NT)=Units(iCurIYr-1,iDiv,NT)+ xUnits
      Cap(iCurIYr,iDiv,NT)=Cap(iCurIYr-1,iDiv,NT)+ xCapacity
      Trills(iCurIYr,iDiv,NT)=Trills(iCurIYr-1,iDiv,NT)+ xTrills
      TrillsOwnUse(iCurIYr,iDiv,NT)= TrillsOwnUse(iCurIYr-1,iDiv,NT) +xTrillsOwnUse
      GasUsage(iCurIYr,iDiv,NT)=GasUsage(iCurIYr-1,iDiv,NT)+ xfuelusage
      HWBTU(iCurIYr,iDiv,NT)=HWBTU(iCurIYr-1,iDiv,NT)+ xhwbtu
      Invest(iCurIYr,iDiv,NT)=xInvest  !($mill)
      x111dRenSub(iCurIYr,iDiv,nt)=xInvest*xTxCrPct_Div(nv,iDiv,nt)

      82 CONTINUE
      !END OF PENETRATION CALCULATIONS

      !*************************************************************************************************
      ! More RPS Calculations to transfer the composite bonus credits to the Electricity Market Module	!TODO - optimize this code?
      !*************************************************************************************************
      IF (NT.EQ.1) THEN  !solar PV
        xPVGenAdded(iDiv,iCurIYr)= xTrills
        xCompCredit=0.
        xCompGen=0.
        !Accumulate Credits
        xCredit=1.0 !Minimum credit multiplier is 1.
        DO iyr=RECSYear-BaseYr+1,iCurIYr
          iCalYR=iyr+BaseYr-1
          xBonus=0.
          xCompGen=xCompGen+xPVGenAdded(iDiv,iyr)
          IF (CurCalYr .LT. iRPSStartYear .AND. iCalYR .GE. iRPSGrandFatherYear) THEN  !no bonus for equipment in service before grandfather year
            xbonus=xRPS(NT,iCurIYr)-1.0  !give the current year credit to renewable DG placed in service before the legislation
          ELSE
            IF (iCurIYr .LT. iyr+iNumYrsatRPSBaseRate(NT,iyr) .AND.  CurCalYr .LT. iRPSPhaseOutYear) THEN
              xBonus=xRPS(NT,iyr)-1.0  !give the base year credit for capacity added in iyr for iNumYrsatRPSBaseRate of years
              !WRITE (DGDAT,*) 'test iCurIYr iyr inumyrs at rpsbase', iCurIYr, iyr, inumyrsatrpsbaserate(nt,iyr)
            ELSE
            IF (iCurIYr .LT. iyr+iNumRPSCreditYrs(NT,iyr) .AND.  CurCalYr .LT. iRPSPhaseOutYear) &
              xBonus=xRPS(NT,iCurIYr)-1.0 !Give the current year credit if beyond the number of base credit years
            ENDIF
          ENDIF
          xCompCredit=xCompCredit+xPVGenAdded(iDiv,iyr)*(xCredit+xBonus)
          !IF (LPRINT .AND. (iDiv < 2) WRITE (DGDAT,*) 'RPS Calcs PV', iyr, xPVGenAdded(iDiv,iyr), xcredit+xbonus  !for debugging
        ENDDO
        IF (xCompGen .GT. 0.) CGCPVRES(iDiv,iCurIYr)=xCompCredit/xCompGen
        IF (LPRINT .AND. iDiv < 10) WRITE (DGDAT,'("RPS Calcs PV",3i6,2F12.5)') iCurIYr, CurCalYr, iDiv, CGCPVRes(iDiv,iCurIYr), EPRPSPR(iCurIYr)
      ENDIF !NT=1

      IF (NT.EQ.3) THEN  !wind
        xWindGenAdded(iDiv,iCurIYr)= xTrills
        xCompCredit=0.
        xCompGen=0.
        !Accumulate Credits
        xCredit=1.0 !Minimum credit multiplier is 1.
        DO iyr=RECSYear-BaseYr+1,iCurIYr
          iCalYR=iyr+BaseYr-1
          xBonus=0.
          xCompGen=xCompGen+xWindGenAdded(iDiv,iyr)
          IF (CurCalYr .LT. iRPSStartYear .AND. iCALYR .GE. iRPSGrandFatherYear) THEN  !no bonus for equipment in service before grandfather year
            xbonus=xRPS(NT,iCurIYr)-1.0  !give the current year credit to renewable DG placed in service before the legislation
          ELSE
            IF (iCurIYr .LT. iyr+iNumYrsatRPSBaseRate(NT,iyr) .AND.  CurCalYr .LT. iRPSPhaseOutYear) THEN
              xBonus=xRPS(NT,iyr)-1.0  !give the base year credit for capacity added in iyr for iNumYrsatRPSBaseRate of years
              !WRITE (DGDAT,*) 'test iCurIYr iyr inumyrs at rpsbase', iCurIYr, iyr, inumyrsatrpsbaserate(nt,iyr)
            ELSE
              IF (iCurIYr .LT. iyr+iNumRPSCreditYrs(NT,iyr) .AND.  CurCalYr .LT. iRPSPhaseOutYear) &
               xBonus=xRPS(NT,iCurIYr)-1.0 !Give the current year credit if beyond the number of base credit years
            ENDIF
          ENDIF
        xCompCredit=xCompCredit+xWindGenAdded(iDiv,iyr)*(xCredit+xBonus)
        !Debugging Use IF (LPrint .AND. iDiv < 2) WRITE (DGDAT,*) 'RPS Calcs Wind', iyr, xWindGenAdded(iDiv,iyr), xcredit+xbonus
        ENDDO
        IF (xCompGen .GT. 0.) CGCWNRES(iDiv,iCurIYr)=xCompCredit/xCompGen
        IF (LPRINT .AND. iDiv < 10) WRITE (DGDAT,'("RPS Calcs WN",3i6,2F12.5)') iCurIYr, CurCalYr, iDiv, CGCWNRes(iDiv,iCurIYr), EPRPSPR(iCurIYr)
      ENDIF !NT=3

      !********************************
      !CHECK TECHNICAL POTENTIAL
      !********************************
      !Accumulators for Technical Potential and Other Summary Statistics
      ! Note xSqftperKW varies by year and is calculated above
      IF (NT.EQ.1) THEN
        SolarPVInstalledMW(iCurIYr,iDiv)=SolarPVInstalledMW(iCurIYr,iDiv)+SolarPVInstalledMW(iCurIYr-1,iDiv)
        SolarPVUsedRoofArea(iCurIYr,iDiv)=SolarPVUsedRoofArea(iCurIYr,iDiv)+SolarPVUsedRoofArea(iCurIYr-1,iDiv)
      ENDIF

      IF (NT.EQ.3) THEN
        WindInstalledMW(iCurIYr,iDiv)=WindInstalledMW(iCurIYr,iDiv)+WindInstalledMW(iCurIYr-1,iDiv)
      ENDIF

      ENDDO  !END CENSUS DIVISION LOOP iDiv

    66 CONTINUE  !SKIP OUT-OF-SCOPE VINTAGES

  ENDDO  !END TECHNOLOGY VINTAGE LOOP NV
ENDDO  !END TECHNOLOGY TYPE LOOP NT

!MORE DIAGNOSTIC PRINTING
DO iDiv=1,NumDiv
  DO NT=1,NumTechs
    QRSDGSG(iCurIYr,iDiv)=QRSDGSG(iCurIYr,iDiv)+Trills(iCurIYr,iDiv,NT)-TrillsOwnUse(iCurIYr,iDiv,NT) !Grid electricity sales in trillion Btu
    !IF (LPRINT.AND.LPRINT2) WRITE(DGDAT,*)QRSDGSG(iCurIYr,iDiv),Trills(iCurIYr,iDiv,NT),TrillsOwnUse(iCurIYr,iDiv,NT)
  ENDDO  ! TECHNOLOGIES
  IF (LPRINT.AND.LPRINT2) THEN
    WRITE(DGDAT,*) ' DIV       GRID SALES'
    WRITE(DGDAT,67) iDiv,QRSDGSG(iCurIYr,iDiv)
  ENDIF
ENDDO  ! DIVISIONS

!**************************************
!CALCULATE OUTPUTS TO NEMS AND RESD
!**************************************
!Load arrays for passing data to the Electric Utility Module
! Calculate base-year cogeneration capacity by census division, building type, and fuel;
! Populate common block variables for DG learning

!Initialize learning variables
rFuelCell_MW(iCurIYr)=0.
rPV_MW(iCurIYr)=0.
rMicroTur_MW(iCurIYr)=0.	!TODO - no residential microturbines modeled in RDM
rWind_MW(iCurIYr)=0.
CGRESQ(1:NumDiv,iCurIYr,1:MNUMCGF)= 0.0  !Cogeneration Fuel Consumption in Trills  !BESSmodel
CGRESGEN(1:NumDiv,iCurIYr,1:MNUMCGF,1:2)= 0.0  !Generation in GWh by Capacity Type and Grid Sales(1) versus Own Use(2)  !BESSmodel
CGRESCAP(1:NumDiv,iCurIYr,1:MNUMCGF)=0.  !Capacity in MW(dc)  !BESSmodel

DO r=1,NumDiv  !census division loop for populating arrays for utility module
  !Initialize Output Arrays for Linking to the Utility Module
  ! The index f(1:13) is the cogeneration fuel numbering scheme for the Utility Module Link Array (based on MNUMCGF in parametr includes):
  !    f=3, natural gas corresponds to NT=2, fuel cells
  !    f=8, solar corresponds to NT=1
  !    f=11, wind corresponds to NT=3
  !    f=13, battery energy storage corresponds to NT=4  !BESSmodel
  ! The remaining fuels are unused here:
  !    f=1, coal
  !    f=2, distillate/ residual fuel oil + kerosene
  !    f=4, hydro
  !    f=5, geothermal
  !    f=6, MSW
  !    f=7, biomass
  !    f=9, other gaseous
  !    f=10, other
  !    f=12, solar thermal

  !Cogen Capacity MW; maps MNUMCGF to NTEK values from RSGENTK.txt
  CGRESCAP (r,iCurIYr,3)= CGRESCAP (r,iCurIYr,3) + Cap(iCurIYr,r,2)/1000. !natural gas-fired fuel cells
  CGRESCAP (r,iCurIYr,8)= CGRESCAP (r,iCurIYr,8) + Cap(iCurIYr,r,1)/1000. !solar PV
  CGRESCAP (r,iCurIYr,11)= CGRESCAP (r,iCurIYr,11) + Cap(iCurIYr,r,3)/1000. !wind
  CGRESCAP (r,iCurIYr,13)= CGRESCAP (r,iCurIYr,13) + Cap(iCurIYr,r,4)/1000. !battery energy storage  !BESSmodel

  !Populate learning common block variables in MW
  rPV_MW(iCurIYr)=      rPV_MW(iCurIYr)       + Cap(iCurIYr,r,1)/1000.
  rFuelCell_MW(iCurIYr)=rFuelCell_MW(iCurIYr) + Cap(iCurIYr,r,2)/1000.
  rWind_MW(iCurIYr)=    rWind_MW(iCurIYr)     + Cap(iCurIYr,r,3)/1000.

  !Cogen Electricity Generated in GWh (1=grid sales, 2=own-use) -- UNUGS.f uses both CGRESGEN(ICN,IYR,JFL,1) and CGRESGEN(ICN,IYR,JFL,2)

  !Grid Sales:
  CGRESGEN (r,iCurIYr,3,1)= CGRESGEN (r,iCurIYr,3,1)     &     ! Natural Gas Fuel Cell
         + (Trills(iCurIYr,r,2) - TrillsOwnUse(iCurIYr,r,2)) & ! Grid Sales = total generation - own use
         * (1000./3.412)                                       ! GWh/trill conversion

  CGRESGEN (r,iCurIYr,8,1)= CGRESGEN (r,iCurIYr,8,1)     &     ! Solar PV
         + (Trills(iCurIYr,r,1) - TrillsOwnUse(iCurIYr,r,1)) & ! Grid Sales = total generation - own use
         * (1000./3.412)                                       ! GWh/trill conversion

  CGRESGEN (r,iCurIYr,11,1)= CGRESGEN (r,iCurIYr,11,1)     &   ! Wind
         + (Trills(iCurIYr,r,3) - TrillsOwnUse(iCurIYr,r,3)) & ! Grid Sales = total generation - own use
         * (1000./3.412)                                       ! GWh/trill conversion

  CGRESGEN (r,iCurIYr,13,1)= CGRESGEN (r,iCurIYr,13,1)     &   ! Battery Energy Storage  !BESSmodel
         + (Trills(iCurIYr,r,4) - TrillsOwnUse(iCurIYr,r,4)) & ! Grid Sales = total generation - own use  !BESSmodel
         * (1000./3.412)                                       ! GWh/trill conversion  !BESSmodel

  !Own-use generation:
  CGRESGEN (r,iCurIYr,3,2)= CGRESGEN (r,iCurIYr,3,2)      &    ! Natural Gas Fuel Cell
         + TrillsOwnUse(iCurIYr,r,2) &
         * (1000./3.412)                                       ! GWh/trill conversion

  CGRESGEN (r,iCurIYr,8,2)= CGRESGEN (r,iCurIYr,8,2)      &    ! Solar PV
         + TrillsOwnUse(iCurIYr,r,1) &
         * (1000./3.412)                                       ! GWh/trill conversion

  CGRESGEN (r,iCurIYr,11,2)= CGRESGEN (r,iCurIYr,11,2)    &    ! Wind
         + TrillsOwnUse(iCurIYr,r,3) &
         * (1000./3.412)                                       ! GWh/trill conversion

  CGRESGEN (r,iCurIYr,13,2)= CGRESGEN (r,iCurIYr,13,2)    &    ! Battery Energy Storage  !BESSmodel
         + TrillsOwnUse(iCurIYr,r,4) &                         !BESSmodel
         * (1000./3.412)                                       ! GWh/trill conversion

  !Fuel consumption for cogeneration (trillion Btu)
  CGRESQ (r,iCurIYr,3)= CGRESQ(r,iCurIYr,3)               &    ! Natural Gas Fuel Cell
         + GasUsage(iCurIYr,r,2)

  CGRESQ (r,iCurIYr,8)= CGRESQ(r,iCurIYr,8)               & ! Solar PV
         + Trills(iCurIYr,r,1)                              ! report "fuel usage" as generation in Trills

  CGRESQ (r,iCurIYr,11)= CGRESQ(r,iCurIYr,11)             & ! Wind
         + Trills(iCurIYr,r,3)                              ! report "fuel usage" as generation in Trills

  CGRESQ (r,iCurIYr,13)= CGRESQ(r,iCurIYr,13)             & ! Battery Energy Storage  !BESSmodel
         + Trills(iCurIYr,r,4)  !BESSmodel

ENDDO !NumDiv

!**********************************************************
!AGGREGATE NATIONAL COGEN RESULTS FOR UTILITY MODULE
!**********************************************************
!Initialize arrays  !BESSmodel
CGRESGEN(11,iCurIYr,1:MNUMCGF,1)=0.0   !GRID SALES (GWh)
CGRESGEN(11,iCurIYr,1:MNUMCGF,2)=0.0   !OWN-USE (GWh)
CGRESQ(11,iCurIYr,1:MNUMCGF)=0.0       !COGEN FUEL CONS (Trills)
CGRESCap(11,iCurIYr,1:MNUMCGF)=0.0     !CAPACITY (MW)

!Aggregate to census division 11 (national total)
CGRESGEN(11, iCurIYr, 1:MNUMCGF, 1) = CGRESGEN(11, iCurIYr, 1:MNUMCGF, 1) + SUM(CGRESGEN(1:NumDiv, iCurIYr, 1:MNUMCGF, 1), DIM=1)
CGRESGEN(11, iCurIYr, 1:MNUMCGF, 2) = CGRESGEN(11, iCurIYr, 1:MNUMCGF, 2) + SUM(CGRESGEN(1:NumDiv, iCurIYr, 1:MNUMCGF, 2), DIM=1)
CGRESQ(11, iCurIYr, 1:MNUMCGF) = CGRESQ(11, iCurIYr, 1:MNUMCGF) + SUM(CGRESQ(1:NumDiv, iCurIYr, 1:MNUMCGF), DIM=1)
CGRESCap(11, iCurIYr, 1:MNUMCGF) = CGRESCap(11, iCurIYr, 1:MNUMCGF) + SUM(CGRESCap(1:NumDiv, iCurIYr, 1:MNUMCGF), DIM=1)

TrillsOwnUse(iCurIYr, 11, 1:nTek) = TrillsOwnUse(iCurIYr, 11, 1:nTek) + SUM(TrillsOwnUse(iCurIYr, 1:NumDiv, 1:nTek), DIM=2)

!**********************************************************
!SUMMARY PRINTING TO THE OUTPUT DATABASE FILE FOR THE YEAR
!**********************************************************
IF (LPRINT) THEN
  DO NT=1,NumTechs
    WRITE(DGDAT,*) 'TECHNOLOGY CLASS:  ',aEquipName(NT,1)
    WRITE(DGDAT,*) ' DIV   UNITS       INVESTMENT'
    WRITE(DGDAT,68) (iDiv, Units(iCurIYr,iDiv,NT), Invest(iCurIYr,iDiv,NT), iDiv=1,NumDiv)

    WRITE(DGDAT,*) ' DIV       Trills       TrillsOwnUse'
    WRITE(DGDAT,68) (iDiv, Trills(iCurIYr,iDiv,NT), TrillsOwnUse(iCurIYr,iDiv,NT), iDiv=1,NumDiv)

    WRITE(DGDAT,*) ' DIV       GasUsage       HWSAVINGS'
    WRITE(DGDAT,68) (iDiv, GasUsage(iCurIYr,iDiv,NT), HWBTU(iCurIYr,iDiv,NT), iDiv=1,NumDiv)
  ENDDO

  WRITE(DGDAT,*)' Technical Potentials and Penetration ', CurCalYr
  WRITE(DGDAT,*)' Div  PV_Potential_MW           PV_Installed_MW'
  WRITE(DGDAT,73) (iDiv, SolarPVTechPotentialMW(iCurIYr,iDiv), SolarPVInstalledMW(iCurIYr,iDiv),iDiv=1,NumDiv)
  WRITE(DGDAT,*)' Div  PV_Available_Roof_Area    PV_Used_Roof_Area '
  WRITE(DGDAT,73) (iDiv, SolarPVAvailRoofArea(iCurIYr,iDiv), SolarPVUsedRoofArea(iCurIYr,iDiv), iDiv=1,NumDiv)
  WRITE(DGDAT,*)' Div  Wind_Potential_MW         Wind_Installed_MW'
  WRITE(DGDAT,73) (iDiv, WindTechPotentialMW(iCurIYr,iDiv), WindInstalledMW(iCurIYr,iDiv), iDiv=1,NumDiv)
ENDIF
67 FORMAT(1X,I4,F15.0)
68 FORMAT(1X,I4,2F15.1)
73 FORMAT(1X,I4,F15.1,10X,F15.1)

!**********************************************************
!PRINT THE DISTRIBUTED GENERATION DATABASE (RDM_DGENOUT.TXT)
!**********************************************************
IF (iCurIYr.EQ.LastYr .AND. FCRL.EQ.1) THEN
  WRITE(DGDAT,69)
  DO NT=1,NumTechs
    DO IYR=RECSYear-BaseYr+1,LastYr !BESSmodel
      DO iDiv=1,NumDiv
        xTrills=Trills(IYR,iDiv,NT)-TrillsOwnUse(IYR,iDiv,NT)  !Grid Sales = Trills - TrillsOwnUse
        xUnits=Units(IYR,iDiv,NT)-Units(IYR-1,iDiv,NT)  !Number of units (systems) added in a given year (in case units don't change from one year to the next)
        IF (xUnits>0.) THEN
          xCalcKW=(Cap(IYR,iDiv,NT)-Cap(IYR-1,iDiv,NT))/xUnits !calculated average system size  !BESSmodel
        ELSE
          xCalcKW=xKW(NT,IYR) !average system size from RSGENTK.txt !BESSmodel
        ENDIF
        WRITE(DGDAT,70) aEquipName(nt,1),IYR+BaseYr-1,iDiv, & !BldgType added in format below
         Units(IYR,iDiv,NT), xCalcKW,       &
         xCalcKW*xUnits,                    &
         Cap(IYR,iDiv,NT),                  &  !CapacityOutput
         Trills(IYR,iDiv,NT), xTrills,      &
         TrillsOwnUse(IYR,iDiv,NT),         &
         HWBTU(IYR,iDiv,NT),GasUsage(IYR,iDiv,NT), &
         Invest(IYR,iDiv,NT)
      ENDDO
    ENDDO
  ENDDO
ENDIF !Check for final convergence
69 FORMAT(1X,'Tech,Year,Division,BldgType,#Units,AvgKWCap,TotKWAdded,TotKW,GEN(tBtu),GridSales,OwnUse,HWOut,SHOut,FuelInp,Invest($mill)')  !CapacityOutput
70 FORMAT(1X,A22,',',I5,',',I5,',','SF',3(',',F12.3),',',F14.5,4(',',F12.5),', 0.',2(',',F12.5))  !BESSmodel  !CapacityOutput

IF ((CurCalYr .EQ. EstYear+2) .AND. (PRTDBGR.EQ.1)) THEN  !Test-write ExogPVMistie data to RDM_OUT.txt to verify  !PVzipcalib
  WRITE(9,*)  'ExogPVMistie_Test_2'
  WRITE(9,*)  'IYR  ', 'CalYr  ', 'CD  ', 'xExogPen  ', 'Cap  ', 'ExogPVMistie  ', 'Units  ', 'Trills  ', 'TrillsOwnUse  '
  DO IYR= (RECSYear-BaseYr+1),(EstYear-BaseYr+2)  !Prints through year after DGrateYr (hence +2); ExogPVMistie should be 0.0 in years before EstYear
    DO iDiv= 1,mNumCR-2
      WRITE(9,919)  IYR, IYR+BaseYr-1, iDiv, xExogPen(IYR,iDiv,1), Cap(IYR,iDiv,1), ExogPVMistie(IYR,iDiv), Units(IYR,iDiv,1), Trills(IYR,iDiv,1), TrillsOwnUse(IYR,iDiv,1)
    ENDDO
  ENDDO
ENDIF
919 FORMAT(I2,1X,I4,1X,I1,1X,F20.0,5(1X,F20.4))

RETURN  !SEND CONTROL BACK TO RESD

END SUBROUTINE RDISTGEN


!==============================================================================
! PITCINIT: INITIALIZES THE PRICE-DRIVEN TECHNOLOGY ADVANCEMENT
!  VARIABLES AND ARCHIVES THE RTINITYR FROM RSMEQP
!==============================================================================
SUBROUTINE PITCINIT
IMPLICIT NONE

INTEGER I,F
COMMON/PITCVARS/XTINITYR(MNUMRTTY),IFORWARD(4),IFWDPREVYR(4)
INTEGER XTINITYR     !STORAGE FOR INITIAL YEARS FROM RSMEQP
INTEGER IFORWARD     !CALCULATION OF PRICE EFFECTS ON TECH MENU
INTEGER IFWDPREVYR   !STORAGE OF PREVIOUS YEAR'S VALUES
INTEGER IFMAX        !MAXIMUM FORWARD EFFECT
INTEGER ILastSTEOYr  !CALENDAR YEAR FOR LAST STEO BENCH YEAR
INTEGER IFWD         !TEMP VARIABLE

DO F=1,4	!TODO - replace 4 with parameter?
  IFORWARD(F)=0.0
  IFWDPREVYR(F)=0.0
ENDDO

DO I=1,RTTYCNT
  XTINITYR(I)=RTINITYR(I)  ! ARCHIVE INITIAL START YEAR FOR REPORTING
  !WRITE(DGDAT,*) "READ DATA " , CurCalYr, RTTYNAME(I),XTINITYR(I), RTINITYR(I)
ENDDO

RETURN

END SUBROUTINE PITCINIT


!==============================================================================
! RSPITC: COMPUTES AND STORES TECHNOLOGY MENU ADVANCEMENTS ANNUALLY
!==============================================================================
SUBROUTINE RSPITC(IFMAX,ILastSTEOYr)
IMPLICIT NONE

INTEGER I,Y,F,RECNO,EU,EQCLNO,EQC
COMMON/PITCVARS/XTINITYR(MNUMRTTY),IFORWARD(4),IFWDPREVYR(4)
INTEGER XTINITYR  !STORAGE FOR INITIAL YEARS FROM RSMEQP
INTEGER IFORWARD  !CALCULATION OF PRICE EFFECTS ON TECH MENU
INTEGER IFWDPREVYR    !STORAGE OF PREVIOUS YEAR'S VALUES
INTEGER IFMAX         !MAXIMUM FORWARD EFFECT
INTEGER iLastSTEOYr   !CALENDAR YEAR FOR LAST STEO BENCH YEAR
INTEGER IFWD          !TEMP VARIABLE
REAL*4 PRICEDELTA(4)  !PRICE CHANGE - 3YR AVERAGE

iLastSTEOYr=LastSTEOYr
IF (CurCalYr.LE.iLastSTEOYr)RETURN
!IFMAX=-10 !READ FROM RMISC EVENTUALLY SET TO ZERO TO TURN OFF RSPITC	!TODO - remove?

!MAP IFORWARD TO RTEK FUEL NUMBERING SYSTEM

!ON FIRST ITERATION, STORE PREVIOUS YEAR'S ADVANCEMENT INTO IFWDPREVYR
IF (CURITR.EQ.1) THEN
  DO F=1,4	!TODO - replace 4 with parameter?
    IFWDPREVYR(F)=IFORWARD(F)!SET TO LAST YEAR'S  ! RSPITC
  ENDDO
ENDIF

!NEXT COMPUTE THREE-YEAR AVERAGE PRICE INDEX RELATIVE TO BASE YEAR
Y=CurIYr
PRICEDELTA(1)=.33333*(PDSRS(11,Y)+ PDSRS(11,Y-1)+ PDSRS(11,Y-2)) / PDSRS(11,RECSYear-BaseYr+1)
PRICEDELTA(2)=.33333*(PLGRS(11,Y)+ PLGRS(11,Y-1)+ PLGRS(11,Y-2)) / PLGRS(11,RECSYear-BaseYr+1)
PRICEDELTA(3)=.33333*(PNGRS(11,Y)+ PNGRS(11,Y-1)+ PNGRS(11,Y-2)) / PNGRS(11,RECSYear-BaseYr+1)
PRICEDELTA(4)=.33333*(PELRS(11,Y)+ PELRS(11,Y-1)+ PELRS(11,Y-2)) / PELRS(11,RECSYear-BaseYr+1)

!SET ADVANCMENT YEARS BY FUEL FOR PRICE-INDUCED TECHNICAL CHANGE
DO F=1,4	!TODO - replace 4 with parameter?
  !SET TO LAST YEAR'S ADVANCEMENT (IE ONCE SHIFTED FORWARD, THEY REMAIN ADVANCED
  IFWD=IFWDPREVYR(F)

  !SET MINIMUM SHIFT TO WHAT IT WAS LAST YEAR OR TO A GREATER SHIFT
  IFORWARD(F) = MIN(IFWD,-nint(((PRICEDELTA(F)-1.0)/.10)))

  !SET MAXIMUM SHIFT TO IFMAX FROM RSMISC? FILE
  IFORWARD(F) = MAX(IFMAX,IFORWARD(F))
ENDDO

!APPLY SHIFTS TO INDIVIDUAL TECHNOLOGIES BASED ON NEARNESS TO LAST BENCHMARKING YEAR

!THIS DO LOOP INDEX AND LIMITS MAY BE INCORRECT	!TODO - review
DO 10 RECNO=1,RTTYCNT  ! DO FOR ALL RTEK RECORDS
  EQC= RTTYEQCL(RECNO) ! EQUIPMENT CLASS NUMBER FROM RSMEQP
  EU = RTTYENDU(RECNO) ! END USE NUMBER FROM RSMEQP

  DO EQCLNO=1,RTCLCNT  !NEXT MATCH RSMEQP RECORD TO RSCLASS RECORD TO FIND FUEL TYPE
    IF (RTCLENDU(EQCLNO).EQ.EU) THEN ! END USE MATCHED, NOW CHECK FOR EQUIPMENT CLASS
      IF (RTCLEQCL(EQCLNO).EQ.EQC) THEN
        F=RTFUEL(EQCLNO)  ! MATCH FOUND, SET FUEL POINTER AND PROCEED
        IF (F.GT.(mNumFuel-1)) F=0   ! SET TO ZERO IF NOT ONE OF THE 4 MAJOR FUELS (natural gas, electricity, distillate fuel oil/kerosene, propane)
          GOTO 5
        ELSE
          CONTINUE
        ENDIF
        ELSE
          CONTINUE
        ENDIF
  ENDDO

!SET ADVANCEMENTS:
5  IF (F.EQ.0) GOTO 10  !SKIP IF NOT ONE OF THE 4 MAJOR FUELS	!TODO - replace GOTO with different convention?
IFWD=IFORWARD(F)  !FIRST, SET MAXIMUM ADVANCEMENT BASED ON FUEL PRICES

!NOW CHECK FOR "CLOSE-IN" TECHNOLOGIES AND TRIM ADVANCMENT YEARS
! PERFORM CHECKING ON UN-ADVANCED INITIAL YEARS
IF (XTINITYR(RECNO).LE.iLastSTEOYr+50) IFWD=IFORWARD(F)
IF (XTINITYR(RECNO).LE.iLastSTEOYr+10) IFWD=MAX(-5,IFORWARD(F))
IF (XTINITYR(RECNO).LE.iLastSTEOYr+ 5) IFWD=MAX(-3,IFORWARD(F))
IF (XTINITYR(RECNO).LE.iLastSTEOYr   ) IFWD=0

RTINITYR(RECNO)=XTINITYR(RECNO)+IFWD  ! SHIFT INITIAL YEAR AND STORE IN WORKING ARRAY

10 CONTINUE  !NEXT RSMEQP RECORD	!TODO - convert to ENDDO?

RETURN  !ALL RECORDS PROCESSED

END SUBROUTINE RSPITC


!====================================================================
!  DISTRIBUTED SHORT-RUN ELASTICITY CALCULATION FUNCTION
!====================================================================
REAL FUNCTION RSELAST (F,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
IMPLICIT NONE

REAL*4 EF1,EF2,EF3
REAL*4 ALPHA
INTEGER F,R,RECSYear,EUPR
REAL*4 FAC1,FAC2,FAC3

!Set no elasticity adjustment if no fuel is specified, then return
! Allows more general use of this function and streamlines code
IF (F .EQ. 0.) THEN
  RSELAST=1.
  RETURN
ENDIF

!NOTE EF1+EF2+EF3 SHOULD SUM TO 1.0 -- THEY ARE DISTRIBUTIONAL SHARES FOR THE SHORT-RUN ELASTICITY EFFECTS
FAC1=1.  ;  FAC2=1.  ;  FAC3=1.   !INITIALIZE

IF (F.EQ.4) THEN
  !END USE PRICING FOR ELECTRICITY (no need to deflate to a particular year because it would appear in numerator and denominator)
  IF (CurCalYr>=RECSYear+1)FAC1=(PELRSOUT(R,CurIYr,  EUPR)/PELRSOUT(R,RECSYear-BaseYr+1,EUPR))**(ALPHA*EF1)
  IF (CurCalYr>=RECSYear+2)FAC2=(PELRSOUT(R,CurIYr-1,EUPR)/PELRSOUT(R,RECSYear-BaseYr+1,EUPR))**(ALPHA*EF2)
  IF (CurCalYr>=RECSYear+3)FAC3=(PELRSOUT(R,CurIYr-2,EUPR)/PELRSOUT(R,RECSYear-BaseYr+1,EUPR))**(ALPHA*EF3)
  ELSE
  IF (CurCalYr>=RECSYear+1)FAC1=(PRICES(F,R,CurCalYr  )/PRICES(F,R,RECSYear))**(ALPHA*EF1)
  IF (CurCalYr>=RECSYear+2)FAC2=(PRICES(F,R,CurCalYr-1)/PRICES(F,R,RECSYear))**(ALPHA*EF2)
  IF (CurCalYr>=RECSYear+3)FAC3=(PRICES(F,R,CurCalYr-2)/PRICES(F,R,RECSYear))**(ALPHA*EF3)
ENDIF

RSELAST=FAC1*FAC2*FAC3

!WRITE(DGDAT,*) "rselast=(i),rselast,CurCalYr,PRICES(F,R,CurCalYr),RECSYear,prices(f,r,RECSYear)!produces copious output in RDM_DGENOUT

RETURN

END FUNCTION RSELAST

END SUBROUTINE RESD  !closes the contains structure


!==============================================================================
! EQUIPMENT SURVIVAL RATE FUNCTION
!  (SURVIVING FRACTION OF EQUIPMENT STOCK IN YEAR Y)
!==============================================================================
REAL FUNCTION SVRTE(ALPHA,Y,K,LAMBDA)

INTEGER Y
REAL*4 ALPHA
REAL*4 LAMBDA
REAL*4 KLAMBDA1
REAL*4 KLAMBDA2
REAL*4 K

IF (FLOAT(Y)-ALPHA.LT.0) THEN  ! Y minus ALPHA is to the left of Weibull curve, so no equipment expected to retire yet
  SVRTE=1.0
ELSE
  KLAMBDA1=(FLOAT(Y)-ALPHA)/LAMBDA
  KLAMBDA2=KLAMBDA1**K
  SVRTE=EXP(-KLAMBDA2)
ENDIF

RETURN

END FUNCTION SVRTE


!==============================================================================
! COST TREND FUNCTION
!==============================================================================
! This function returns the projected cost of equipment identified in the RSMEQP file by technology RECTY, for the calendar
!  year CurCalYr, where CTYPE indicates whether the requested cost type is the total installed cost (equip. + installation)
!  or only the equipment cost. Several required parameters, such as the trend type (MATURE, ADOLESCENT, INFANT), logistic
!  shape parameters, years of availability, etc., are obtained from the RTEK common block rather than passed as arguments.

REAL FUNCTION EQCOST*4 (RECTY,CurCalYr,CTYPE)
IMPLICIT NONE

INCLUDE 'parametr'
INCLUDE 'rtek'

INTEGER*4 RECTY     ! Technology index
INTEGER*4 CurCalYr  ! Price forecast calendar year
CHARACTER*3 CTYPE   ! Cost type requested ('CAP' or 'RET')

REAL*4 y0           ! Year of inflection on logistic cost curve
REAL*4 y1           ! Starting year of logistic cost curve
REAL*4 d            ! Proportional decline in equipment cost
REAL*4 gamma        ! Logistic cost curve shape parameter
REAL*4 RSYR2

!In case of any error that might occur below, the cost returned will be huge:
EQCOST= 10.0**9
RSYR2=FLOAT(CurCalYr)

!Project the equipment cost based on the type of cost trend appropriate for the maturity level of this technology:

!Mature technology:
IF (RTMATURE(RECTY) .EQ. "MATURE") THEN
  ! Current implementation calls for costs to continue unchanged from the initial costs specified in RSMEQP.
  IF (CTYPE .EQ. "CAP") THEN
    !Total installed cost of equipment
    EQCOST= RTEQCOST(RECTY)
    RETURN
  ELSE
    IF (CTYPE .EQ. "RET") THEN
      !Equipment only cost (Total installed cost less installation costs)
      EQCOST= RTRECOST(RECTY)
      RETURN
    ELSE
      RETURN
    ENDIF !Retail cost test
  ENDIF   !Capital cost test
ENDIF     !MATURE technology test

!Adolescent technology
IF (RTMATURE(RECTY) .EQ. "ADOLESCENT") THEN
  !Current implementation calls for a logistic functional form, with the base year coinciding with the inflection point (the code
  ! actually uses the first year of availability as specified in RSMEQP). The remaining proportional cost decline is specified
  ! (RTCOSTP3), as is a 'representative' year of introduction (RTCOSTP1), and shape parameter (RTCOSTP2), in the RSMEQP input file:
  y1= RTCOSTP1(RECTY) !representative year cost decline began
  y0= FLOAT(RTINITYR(RECTY)) !year of inflection of cost trend
  d= RTCOSTP3(RECTY) !total possible proportional decline in equipment cost from y0 onward
  gamma= RTCOSTP2(RECTY) !logistic curve shape parameter

  IF (CTYPE .EQ. "CAP") THEN
    EQCOST= RTEQCOST(RECTY) * 2.0 * d / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) + ( 1.0 - d ) * RTEQCOST(RECTY)
    RETURN
  ELSE
    IF (CTYPE .EQ. "RET") THEN
      EQCOST= RTRECOST(RECTY) * 2.0 * d / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) + ( 1.0 - d ) * RTRECOST(RECTY)
      RETURN
    ELSE
      RETURN
    ENDIF !Retail cost test
  ENDIF   !Capital cost test
ENDIF     !ADOLSECENT technology test

!Infant technology
IF (RTMATURE(RECTY) .EQ. "INFANT") THEN
  !Current implementation calls for a logistic functional form for the cost trend:
  y1= FLOAT(RTINITYR(RECTY)) !year cost decline begins
  y0= RTCOSTP1(RECTY) !year of inflection of cost trend
  d= RTCOSTP3(RECTY) !total possible proportional decline in equipment cost from y1 onward
  gamma= RTCOSTP2(RECTY) !logistic curve shape parameter

  IF (CTYPE .EQ. "CAP") THEN
    EQCOST= RTEQCOST(RECTY) * d / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) + ( 1.0 - d ) * RTEQCOST(RECTY)
    RETURN
  ELSE
    IF (CTYPE .EQ. "RET") THEN
      EQCOST= RTRECOST(RECTY) * d / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) + ( 1.0 - d ) * RTRECOST(RECTY)
      RETURN
    ELSE
      RETURN
    ENDIF !Retail cost test
  ENDIF   !Capital cost test
ENDIF     !INFANT technology test

RETURN

END FUNCTION EQCOST


!==============================================================================
! LEARNING COST FUNCTION
!==============================================================================
! This function returns the projected cost of equipment based on cumulative shipment estimates (from the prior year)

REAL FUNCTION rLearnCost*4 (MaxCost,Beta,c0,CumShip,report)
IMPLICIT NONE

REAL*4 MaxCost     ! maximum cost set equal to default projections
REAL*4 Beta        ! the learning cost function shape parameter
REAL*4 c0          ! first unit cost
REAL*4 CumShip     ! cumulative shipments through the previous year

INTEGER*4 report   ! link to CDM_RPTOUT.txt (Unit 23) output file	!TODO - set 23 as a parameter once so it doesn't need to be repeated

IF (cumship .LE. 1.) rLearnCost=MaxCost
IF (cumship .LE. 1.) RETURN
IF (c0 .EQ. 0.) rLearnCost=MaxCost
IF (c0 .EQ. 0.) RETURN

rLearnCost=min( MaxCost, exp( log(c0) - Beta*log(CumShip) ) )

RETURN

END FUNCTION rLearnCost


!========You've reached the end of the Residential Demand Module!========