! $Header: M:/default/source/RCS/tran.f,v 1.398 2020/11/18 21:05:38 JMA Exp $

! ... *******************************************************************************
! ... **                                                                           **
! ... **  NEMS Transportation model                                                **
! ... **                                                                           **
! ... *******************************************************************************

module T_

! ... Include files of the NEMS Global Data Structure (Variables in RESTART file)

INCLUDE 'PARAMETR'
INCLUDE 'APQ'
INCLUDE 'APONROAD'
INCLUDE 'NCNTRL'
INCLUDE 'QSBLK'
INCLUDE 'MACOUT'
INCLUDE 'CDSPARMS'
INCLUDE 'COALOUT' 
INCLUDE 'CONVFACT'
INCLUDE 'TRANREP'
INCLUDE 'TRANMAIN'
INCLUDE 'INTOUT'

! ... Global Declaration Section    
! ... Switches for transportation scenarios
INTEGER      RTOVALUE, FILE_MGR
EXTERNAL     RTOVALUE, FILE_MGR

INTEGER      H2UNIT                                                 ! unit for writing hydrogen output to TRNH2OUT.txt
INTEGER      LEGIRA,   &											! "1" turns on 2022 IRA tax credits for PHEV and eV
             TRANEFF                                                ! "1" turns on FRZNEFF
                                                                    ! "2" turns on HIGHEFF
                                                                    ! "3" turns on AEO2025 No new policy case
INTEGER      IBank                                                  ! "1" turns on CAFE banking
INTEGER      CAFEMEET                                               ! "1" calls subroutine CAFETEST: ensures CAFE compliance by increasing
INTEGER      TRANAB32                                               ! Switch for California AB32 off/on
																	!     sales of hybrid and diesel vehicles
INTEGER      FRTEFF                                                 ! technology switch for the heavy vehicles
                                                                    !     "1" turns on high technology
                                                                    !     "2" turns on frozen technology
REAL         FRZNEFF,  &                                            ! "1" frozen technology scenario
             HIGHEFF,  &                                            ! "1" high technology scenario
			 IRA_STIM   											! "1" switch for IRA PHEV/EV tax credit
             
! ... Global definitions
integer      iy                                                     ! year index corresponding to first year of inputs
integer      num_to_read                                            ! number of years in input ranges
integer      First_Read_Year                                        ! = 1995
INTEGER      YRS                                                    ! actual model year (1989+curiyr)
INTEGER      N                                                      ! tran variable for curiyr
INTEGER      LASTID                                                 ! parameter for index describing number of technologies
INTEGER      MAXGROUP,MAXVTYP,MAXCLASS,MAXTECH,MAXLDV               ! various
INTEGER      MAXNOTE,MAXAGE,MAXLTAGE,MAXFLEET,MAXNMLM               ! parameters,
INTEGER      MAXFUEL,GAS,FCLO,FCHI,BASE,PREV,CURRENT                ! see
INTEGER      BYR,LYR, XYR,IRAYR,AGEGRP,MF   		                ! definitions below
INTEGER      CARGRP,LTKGRP,MAXHAV,numstkyrs,STOCKYR,MAXCHRG
INTEGER		 PHEVTYPE								

PARAMETER    (MAXGROUP    = 11,          &                          ! number of light duty vehicle groups  (5-car and 6-light truck)
              MAXCHRG     = 3,           &                          ! number of charger types (dc, l1, l2)
              CARGRP      = 5,           &                          ! number of light duty vehicle groups that are car mfgs
			  PHEVTYPE	  = 2,			 &							! 1 = PHEV20, 2 = PHEV50
              LTKGRP      = 6,           &                          ! number of light duty vehicle groups that are light truck mfgs
              MAXVTYP     = 2,           &                          ! number of light duty vehicle types (car/truck)
              MAXCLASS    = 8,           &                          ! number of vehicle classes in each light duty group
              MAXTECH     = 100,         &                          ! number of light duty vehicle technologies
              MAXLDV      = 16,          &                          ! number of light duty vehicle fueling configurations
              MAXNOTE     = 400,         &                          ! number of light duty vehicle engineering notes
              MAXAGE      = 25,          &                          ! number of light duty vehicle vintages
              MAXLTAGE    = 20,          &                          ! number of commercial light truck vintages
			  MAXOWNER    = 5,           &                          ! number of owner types (household/business/government/utility/taxi)
              MAXFLEET    = 4,           &                          ! number of light duty fleet types (business/government/utility/taxi)
              MAXNMLM     = 13,          &                          ! number of coefficients for the nested multinomial logit model
              MAXFUEL     = 8,           &                          ! number of distinct fueling station types
              GAS         = 1,           &                          ! vehicle fueling configuration index value for gasoline
              FCLO        = 13,          &                          ! vehicle fueling configuration minimum fuel cell index value
              FCHI        = 15,          &                          ! vehicle fueling configuration maximum fuel cell index value
              BASE        = 0,           &                          ! FEM attribute index value for the base year
              PREV        = 1,           &                          ! FEM attribute index value for the previous year
              CURRENT     = 2,           &                          ! FEM attribute index value for the current year
              BYR         = BASEYR,      &                          ! base year for TRAN (1990)
              XYR         = 2022,        &                          ! base year for FEM
			  IRAYR       = 2023,		 &							! first IRA year
              LYR         = ENDYR,       &                          ! last projection year
              AGEGRP	  = 5,           &		                    ! population age groupings
              MF          = 2,           &		                    ! male =1, female =2
			  MAXHAV      = 4,           &                          ! number of levels of highly automated LDV (see ihav definition)
			  STOCKYR     = 2023,        &                          ! last year of census division level stock history years
			  NUMSTKYRS   = stockyr-1994,&							! number of census division level stock history years
			  MG_HHV	  = 5253/42)                                ! Energy content (high heating value) of gasoline, in thousand btu per gallon

! ... Indices
integer      GrpMap(MAXGROUP)                                       ! Map for how groups are defined back to vehicle types: cars and light trucks
integer      GrpDIMap(MAXGROUP)                                     ! Map for how groups are defined back to domestic (=1) and import (=2)
INTEGER      IGP,     &                                             ! vehicle group (MAXGROUP)
			 XGP,     &
			 INMLM,   &												! consumer choice model (maxnmlm)
             IVTYP,   &                                             ! vehicle type (MAXTYPE)
			 IOWN,    &                                             ! owner type (MAXOWNER)
             ICL,     &                                             ! vehicle size class (MAXCLASS) 
             ITECH,   &                                             ! technology type (MAXTECH)
             NUMTECH, &                                             ! actual number of input technologies                                   
             ILDV,    &                                             ! fuel engine technology (MAXLDV)
			 IPHEV,	  &												! (phevtype)
			 IZEV
																	! for inmlm (maxnmlm)
																	!    1 = level 2, vehicle price
																	!    2 = level 2, fuel cost
																	!    3 = level 2, range
																	!    4 = level 2, battery replacement
																	!    5 = level 2, acceleration
																	!    6 = level 2, EV home refueling
																	!    7 = level 2, maintenance cost
																	!    8 = level 2, luggage space
																	!    9 = level 2, fuel availability 1
																	!   10 = level 2, fuel availability 2
																	!   11 = level 2, make/model availability
																	!   12 = level 1, tech set general cost
																	!   13 = level 3, multi-fuel general cost
                                                                    ! for ildv (maxldv)
																	!    1 = gasoline
                                                                    !    2 = tdi diesel
                                                                    !    3 = ethanol flex
                                                                    !    4 = ev - 100
                                                                    !    5 = PHEV20 gasoline
                                                                    !    6 = PHEV50 gasoline
                                                                    !    7 = ev - 200
                                                                    !    8 = diesel hybrid
                                                                    !    9 = cng bifuel
                                                                    !   10 = lpg bifuel
                                                                    !   11 = cng
                                                                    !   12 = lpg
                                                                    !   13 = fuel cell methanol
                                                                    !   14 = fuel cell hydrogen
                                                                    !   15 = ev - 300
                                                                    !   16 = gasoline hybrid
INTEGER      INOTE,     &                                           ! engineering note (MAXNOTE)
             IAGE,      &                                           ! vehicle vintages 1-25 (MAXAGE)
             IFLEET,    &                                           ! ldv fleet types (MAXFLEET)
             IFUEL,     &                                           ! light duty vehicle fuel type indices varies by subroutine
             IFUELX,    &                                           ! generic fuel indices
             IYR,       &                                           ! year indices
             IREGN,     &                                           ! region indices
             IRAILREGN, &                                           ! rail region indices
             I,J,K,     &                                           ! miscellaneous indices
             SIGN,      &                                           ! positive or negative indicator
             ichrg                                                  ! index charger type (MAXCHRG)
																	! age groupings
INTEGER      IAGR                   								!   1  = 16-19
																	!   2  = 20-34
																	!   3  = 35-54
																	!   4  = 55-64
																	!   5  = 65+
INTEGER      IMF													!   1  = male
																	!   2  = female
INTEGER      IHAV                                                   ! 1 = L0 - non-HAV (SAE levels 0-3)
                                                                    ! 2 = L4a - low speed HAV (SAE level 4)  !!!!
																	! 3 = L4b - high speed HAV (SAE level 4)
																	! 4 = L5 - fully automated vehicle (SAE level 5)
                                   
REAL         ROUNDOFF_ERROR                                         ! roundoff error buffer

! ... Transportation specific macro variables - subroutine TMAC
REAL         FUELTAX(MNUMYR)                                        ! incremental petroleum fuel tax - nominal $/million Btu
REAL         FUELTAX87(MNUMYR)                                      ! incremental petroleum fuel tax - in 1987$
REAL         LIC_TREND(AGEGRP,MF,MNUMCR-2)                          ! growth trend in licensing rates
REAL         LIC_ELAS(MNUMCR-2,AGEGRP)                              ! change in licensing rate realtive to change in employment
REAL         LIC_MAX(AGEGRP,MF,MNUMCR-2)                            ! maximum licensing rate
REAL         LICRATE_M(AGEGRP,MNUMYR,MNUMCR-2)                      ! regional male drivers licensing rate by age group
REAL         LICRATE_F(AGEGRP,MNUMYR,MNUMCR-2)                      ! regional female drivers licensing rate by age group
INTEGER      LicRHistYr                                             ! last historic data year for LicRate
REAL         LICDRIVER(AGEGRP,MF,MNUMCR,MNUMYR)                     ! licensed drivers by region, male/female and age group
REAL         INC00_D_NPT(MNUMCR,MNUMYR)                               ! disposable income per capita (1000's OF 2000$) - transit rail module
REAL         INC00_D_16(MNUMCR,MNUMYR)                                ! disposable income per capita 16+ (2000$) - travel module and car/LT split
REAL         INC90_D_NP(MNUMCR,BYR:LYR)                               ! disposable income per capita (1990 $) - fuel economy module

! ... Total new light duty vehicle sales - subroutine NEWLDV
REAL		 NEWLDVs(maxvtyp,mnumcr,mnumyr)							! New ldv sales
REAL		 CarTrkSplit(mnumcr,maxvtyp,mnumyr)						! Car/LT split by region

! ... New light duty vehicle fuel economy - subroutine TMPGNEW
! ...... historic values LDV attribute values beyond xyr for model calibration - subroutine READNHTSA
INTEGER  EPAYR                                                		! first year of EPA/NHTSA data
INTEGER  EPALYR														! last year of EPA/NHTSA data 
REAL(4), allocatable :: EPAMPG(:,:,:,:)                             ! EPAMPG(MAXGROUP,MAXCLASS,EPAYR:LYR,MAXLDV) new ldv tested fuel economy
REAL(4), allocatable :: EPAHP(:,:,:,:) 								! EPAHP(MAXGROUP,MAXCLASS,EPAYR:LYR,MAXLDV) new ldv horsepower 
REAL(4), allocatable :: EPAPRI(:,:,:,:) 							! EPAPRI(MAXGROUP,MAXCLASS,EPAYR:LYR,MAXLDV) new ldv low volume price
REAL(4), allocatable :: EPAWGT(:,:,:,:) 							! EPAWGT(MAXGROUP,MAXCLASS,EPAYR:LYR,MAXLDV) new ldv curb weight 
REAL(4), allocatable :: EPARNG(:,:,:,:) 							! EPARNG(MAXGROUP,MAXCLASS,EPAYR:LYR,MAXLDV) new ldv driving range 
REAL(4), allocatable :: EPATSZ(:,:,:,:) 							! EPATSZ(MAXGROUP,MAXCLASS,EPAYR:LYR,MAXLDV) new ldv tank size    
REAL(4), allocatable :: EPALUG(:,:,:,:) 							! EPALUG(MAXGROUP,MAXCLASS,EPAYR:LYR,MAXLDV) new ldv trunk size/storage space   
REAL(4), allocatable :: OWN_SALES(:,:,:,:,:,:)						! OWN_SALES(MAXOWNER,MAXGROUP,MAXCLASS,MAXLDV,MNUMCR-2,2019:LYR)
! ...... historic values for the fuel economy module - subroutine READHIST 
REAL(4), allocatable :: FEMMPG(:,:,:,:)                             ! FEMMPG(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV) new ldv tested fuel economy
REAL(4), allocatable :: FEMHP(:,:,:,:) 								! FEMHP(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV) new ldv horsepower 
REAL(4), allocatable :: FEMPRI(:,:,:,:) 							! FEMPRI(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV) new ldv low volume price
REAL(4), allocatable :: FEMWGT(:,:,:,:) 							! FEMWGT(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV) new ldv curb weight 
REAL(4), allocatable :: FEMRNG(:,:,:,:) 							! FEMRNG(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV) new ldv driving range 
REAL(4), allocatable :: FEMTSZ(:,:,:,:) 							! FEMTSZ(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV) new ldv tank size       
REAL(4), allocatable :: FEMPEN(:,:,:,:,:)                           ! FEMPEN(MAXGROUP,MAXCLASS,MAXTECH,BYR:LYR,MAXLDV) new ldv technology penetration 
REAL(4), allocatable :: CAFESALES(:,:,:,:)							! CAFESALES(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV) new ldv sales
REAL(4), allocatable :: MPGCOMP(:,:,:,:)							! MPGCOMP(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV) new ldv compliance fuel economy (w/credits)
REAL(4), allocatable :: MPGADJ(:,:,:,:)								! MPGADJ(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV) new ldv adjusted fuel economy (on-road)
REAL(4), allocatable :: PHEV_EVMT(:,:,:,:)							! EPA VMT factor
REAL(4), allocatable :: PHEVMPG_S(:,:,:,:)							! PHEV charge sustaining fuel economy 
REAL(4), allocatable :: PHEVMPG_D(:,:,:,:)							! PHEV charge depleting fuel economy
REAL(4), allocatable :: NAMEPLATE(:,:,:,:)							! Number of nameplates represented
REAL(4), allocatable :: EV_RNG(:,:,:,:) 							! EV_RNG(MAXGROUP,MAXCLASS,byr:lyr,MAXLDV) electric vehicle range - EV & PHEV
REAL(4), allocatable :: BatPackSize(:,:,:,:) 						! BatPackSize(BYR:LYR,MAXCLASS,MAXGROUP,MAXLDV) 
REAL(4), allocatable :: FPRT(:,:,:,:)	 							! FPRT(MAXGROUP,MAXCLASS,byr:lyr,MAXLDV) vehicle footprint

!...attribute adjustment values for AFVs
REAL         AFVADJHP(MAXLDV,MAXVTYP)                				! ATV horsepower differential (ratio)
REAL         AFVADJFE(MAXLDV,MAXVTYP)                				! ATV fuel economy differential (ratio)
REAL         AFVADJWT(MAXLDV,MAXVTYP)               				! ATV weight differential (ratio)
REAL         AFVADJPR(MAXLDV,MAXVTYP)                    			! ATV price differential (1990$)
REAL		 EVMPG_ADJ(MAXGROUP,MAXCLASS,MAXLDV)					! ev mpg adjustment factors
! ...... input values for base year vehicles
REAL         FE(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)              ! vehicle class base fuel economy
REAL         WEIGHT(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)          ! vehicle class base curb weight
REAL         PRICE(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)           ! vehicle class base price (low volume)
REAL         HP(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)              ! vehicle class base horsepower
real         vhp_adj(MAXCLASS,MAXGROUP,PREV:CURRENT,MAXLDV)         ! weight based hp adjustment
REAL         VALUEPERF(MAXCLASS,MAXGROUP)                           ! vehicle class base performance value
REAL         PERFFACT(MAXCLASS,MAXGROUP)                            ! vehicle class base performance factor
REAL         TANKSIZE(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)        ! vehicle class base fuel tank size
LOGICAL*1    CLASSFLAG(MAXCLASS,MAXGROUP,MAXLDV)		            ! AFV vehicle class applicability flag  
INTEGER*2    GRPFLAG(MAXLDV,MAXCLASS,MAXGROUP)                      ! ATV introduction year by manufacture group by size class
REAL         SALES_PER_MODEL(MAXCLASS,MAXGROUP)                     ! Sales per nameplate, used to introduce new nameplates in projection

! ...... variable used in subroutine FEMCALC
INTEGER      PAYBACK                                                ! payback period
REAL         DISCOUNT                                               ! discount rate
REAL         PMGTR90_D_(MNUMCR,BYR:LYR)                               ! national gasoline price in 1990 dollars
REAL         CFE                                                    ! mpg used to calculate effectiveness = FE(ICL,IGP,PREV,ILDV)
REAL         PRICE_EX(12)                                           ! expected fuel price used in cost effectiveness calculation
REAL         PSLOPE                                                 ! expected rate of change in future fuel price
LOGICAL*1    CAFEPASS(MAXGROUP)                                     ! indicates manufacturer has passed CAFE standard
LOGICAL*1    first_time_cafetest
REAL         PERFCAP(MAXCLASS,MAXGROUP)                             ! vehicle class performance cap
REAL         USEDCAP(MAXCLASS,MAXGROUP)                             ! fraction of vehicle class performance cap used
REAL         MKT_PEN(MAXCLASS,MAXGROUP,MAXTECH,BASE:CURRENT,MAXLDV) ! vehicle systems technology market share
REAL         ACTUAL_MKT(MAXTECH)                                    ! technology market share = MKT_PEN 
REAL         MKT_MAX(MAXCLASS,MAXGROUP,MAXTECH,MAXLDV)              ! maximum technology market share = TECHMKTSHARE
REAL         MMAX(MAXTECH)                                          ! maximum technology market share = MKT_MAX
REAL         MAX_SHARE                                              ! maximum technology market share = MMAX
REAL         TOT_MKT                                                ! total market share of subsystem technology
CHARACTER*30 TECHLABEL(MAXTECH,MAXVTYP)                             ! technology label
INTEGER*2    TECHID(MAXTECH*MAXVTYP)                                ! technology identification number
CHARACTER*15 SYS_AFFECT(MAXTECH,MAXVTYP)                            ! vehicle system affected
REAL         DEL_FE(MAXTECH,MAXVTYP)                                ! incremental fractional change in fuel economy
REAL         DEL_COSTABS(MAXTECH,MAXVTYP)                           ! absolute incremental change in cost ($)
REAL         DEL_COSTWGT(MAXTECH,MAXVTYP)                           ! relative incremental change in cost ($/lb)
REAL         DEL_WGTABS(MAXTECH,MAXVTYP)                            ! absolute incremental change in weight (lb)
REAL         DEL_WGTWGT(MAXTECH,MAXVTYP)                            ! relative incremental change in weight (lb/base vehicle lb)
INTEGER*2    FRSTYEAR(MAXTECH,MAXGROUP)                             ! first year of technology introduction
REAL         DEL_HP(MAXTECH,MAXVTYP)                                ! incremental fractional change in horsepower
REAL         COEFF_LEARN(MAXTECH,MAXVTYP)                           ! coefficient for technology learning curve
REAL         COEFF_LRN1(MAXTECH,MAXVTYP)                            ! coefficient for learning curve trigger for most technologies
REAL         COEFF_LRN2(MAXTECH,MAXVTYP)                            ! coefficient for learning curve trigger for micro hybrids, mild hybrids, tires II
LOGICAL*1    TECH_APPLIC(MAXTECH,MAXVTYP,MAXLDV)                    ! fueling type applicability indicator
INTEGER      YEARS_MKTD                                             ! matches learning cost curve rate to proper introduction year array
REAL         LEARN_COST_MULTIPLIER(4)                               ! learning curve parameter
REAL         VMT(mnumcr,12)                                        ! annual vmt by vintage for each region

! ... STEO Benchmarking
REAL         MER_tran(4)                                            ! Last two historic MER years for gasoline, jet fuel, distillate, and residual by region
INTEGER      ymer                                                   ! Last MER year
INTEGER      ysteo                                                  ! Last STEO year
REAL         TMGTCBUS(MNUMYR)
REAL         TJFTCBUS(MNUMYR)
REAL         TDSTCPUS(MNUMYR)
REAL         TRFTCBUS(MNUMYR)
REAL         BEN_MG 
REAL         BEN_JF 
REAL         BEN_DS 
REAL         BEN_RS
REAL         BTQLDV(9,MNUMCR)
REAL         BTQRAILR(4,MNUMCR)                                     ! benchmarked energy demand for freight rail
                                                                    !   1) diesel
                                                                    !   2) residual
                                                                    !   3) CNG
                                                                    !   4) LNG
REAL         BTQISHIPR(4,MNUMCR)                                    ! benchmarket international waterborne energy demand
REAL         BTQDSHIPR(4,MNUMCR)                                    ! benchmarket domistic waterborne energy demand
REAL         BQJETR(MNUMCR)
REAL         BQAGR(MNUMCR)
REAL         BQMILTR(4,MNUMCR)
REAL         BQMODR(7,MNUMCR)
REAL         BQRECR(MNUMCR)
REAL         BQLUBR(MNUMCR)
REAL         BFLTFUELBTU(MNUMCR,MAXFUEL,MNUMYR)                     ! fleet
REAL         BTQFREIRSC(3,7,MNUMCR)                                 ! heavy truck energy demand by size class, fuel, region
REAL         BFVMTECHSC(2,12,mnumcr)								! 
REAL         BVMTECH(MAXLDV,MNUMCR)                                 ! benchmarked regional household VMT by fuel type (VMTHH)
REAL         BFLTVMTECH(MAXVTYP,MAXFLEET,MAXLDV)            	    ! benchmarked fleet vmt in billion miles							
REAL		 FLTVMTHAV(MAXVTYP,MAXFLEET,MAXLDV,MAXHAV,BYR:LYR)		! Benchmarked fleet vmt (billion miles); used to track vmt by ihav
REAL         BASMDEMD(2)
CHARACTER*15 GROUPLABEL(MAXGROUP)                                   ! vehicle group labels
CHARACTER*30 CLASSLABEL(MAXCLASS,MAXGROUP)                          ! vehicle class labels
INTEGER*2    SUPERSEDES(10,MAXNOTE),TECH_CNT(MAXNOTE)               ! supersedes engineering note parameters
INTEGER*2    REQUIRES(2,MAXNOTE)                                    ! required engineering note parameters
INTEGER*2    SYNERGY(2,MAXNOTE)                                     ! synergy engineering note parameters
REAL         SYNR_DEL(MAXNOTE)                                      ! synergy engineering note parameters
INTEGER*2    MANDYEAR(3,MAXNOTE)                                    ! mandatory engineering note parameters
REAL         MANDMKSH(MAXNOTE)                                      ! mandatory engineering note parameters
LOGICAL*1    MAND_ORIDE(MAXNOTE)                                    ! mandatory engineering note parameters
INTEGER      NUM_REQ,NUM_SUP,NUM_MAN,NUM_SYN                        ! engineering note counters
REAL         REG_COST                                               ! CAFE non-compliance fine

!...Coporate Average Fuel economy (CAFE) standards for light duty vehicles 
REAL         CAFE_STAND(MAXGROUP,BYR:LYR)                           ! Single CAFE standards for cars and light trucks (pre-2011)
!...Variables for Footprint CAFE.
REAL         FPrint(MAXCLASS,MAXGROUP,MNUMYR)                       ! vehicle footprint values
REAL         CAFEMpgGrp(MAXGROUP,mnumyr)                            ! the mpg of the group for cafe purposes (with credits)
REAL         TrueMpgGrp(MAXGROUP,mnumyr)                            ! the true mpg of the group (without credits)
REAL         EPAghgGrp(MAXGROUP,mnumyr)                             ! gCO2/mile of the group for EPA reg purposes (based on test mpg)
REAL         MgGhgGrp(MAXGROUP,mnumyr)                              ! Total Mg credits/debits by group for EPA reg purposes
REAL         AVSales_Old(MAXGROUP,MAXCLASS,MaxLdv,mnumyr)           ! initial vehicle sales
REAL         AVSales_New(MAXGROUP,MAXCLASS,MaxLdv,mnumyr)           ! revised vehicle sales

!...EISA07 NHTSA CAFE footprint parameters for light trucks
REAL         TFCoefA(MNUMYR)                                        ! max fuel economy target
REAL         TFCoefB(MNUMYR)                                        ! min fuel eocnomy target 
REAL         TFCoefC(MNUMYR)                                        ! footprint midway between
REAL         TFCoefD(MNUMYR)                                        ! rate of change parameter
!...EISA07 NHTSA CAFE footprint parameters for cars
REAL         CFCoefA(MNUMYR)                                        ! max fuel economy target
REAL         CFCoefB(MNUMYR)                                        ! min fuel eocnomy target
REAL         CFCoefC(MNUMYR)                                        ! footprint midway between
REAL         CFCoefD(MNUMYR)                                        ! rate of change parameter
!...NHTSA GHG CAFE parameters for light trucks
REAL         TFCoefA2(MNUMYR)                                       ! max fuel economy target
REAL         TFCoefB2(MNUMYR)                                       ! min fuel eocnomy target
REAL         TFCoefC2(MNUMYR)                                       ! rate of change
REAL         TFCoefD2(MNUMYR)                                       ! constant
REAL         TFCoefE2(MNUMYR)                                       ! max fuel economy target -- 74 sqft (MY2017-2025 reg)		
REAL         TFCoefF2(MNUMYR)                                       ! min fuel eocnomy target -- 74 sqft (MY2017-2025 reg)		
REAL         TFCoefG2(MNUMYR)                                       ! rate of change -- 74 sqft (MY2017-2025 reg)				
REAL         TFCoefH2(MNUMYR)                                       ! constant -- 74 sqft (MY2017-2025 reg)						
REAL         TFCoefEPAA2(MNUMYR)                                    ! min CO2 target											
REAL         TFCoefEPAB2(MNUMYR)                                    ! max CO2 target											
REAL         TFCoefEPAC2(MNUMYR)                                    ! rate of change											
REAL         TFCoefEPAD2(MNUMYR)                                    ! constant													
REAL         TFCoefEPAE2(MNUMYR)                                    ! max fuel economy target -- 74 sqft (MY2017-2025 reg)		
REAL         TFCoefEPAF2(MNUMYR)                                    ! min fuel eocnomy target -- 74 sqft (MY2017-2025 reg)		
REAL         TFCoefEPAG2(MNUMYR)                                    ! rate of change -- 74 sqft (MY2017-2025 reg)				
REAL         TFCoefEPAH2(MNUMYR)                                    ! constant -- 74 sqft (MY2017-2025 reg)						

!...NHTSA GHG CAFE parameters for cars
REAL         CFCoefA2(MNUMYR)                                       ! max fuel economy target
REAL         CFCoefB2(MNUMYR)                                       ! min fuel eocnomy target
REAL         CFCoefC2(MNUMYR)                                       ! rate of change
REAL         CFCoefD2(MNUMYR)                                       ! constant
REAL         CFCoefEPAA2(MNUMYR)                                    ! min CO2 target
REAL         CFCoefEPAB2(MNUMYR)                                    ! max CO2 target
REAL         CFCoefEPAC2(MNUMYR)                                    ! rate of change
REAL         CFCoefEPAD2(MNUMYR)                                    ! constant

!...NHTSA CAFE PEF multipliers
REAL		 CAFEPEFMULT(MAXLDV,MNUMYR)								! Fuel economy multipliers for NHTSA CAFE mpgs (from DOE PEF reg)	
REAL		 EPAALTMULT(MAXLDV,MNUMYR)								! Fuel economy multipliers for EPA GHG sales weights	
INTEGER		 RUN_EPA												! Switch to use EPA GHG instead of NHTSA CAFE		
INTEGER		 CAFEMY27_SWITCH										    ! Switch to run AEO2025 No CAFE side case (CAFE/GHG freeze at MY2026 level)	
REAL		 AC_CO2_OFFSET(MAXGROUP,MNUMYR)						    ! A/C leakage and alt refrigerant credits; subtract off EPA GHG reg before converting to mpg						

REAL         LDV_MPG_CL(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)            ! fuel economy by size class by fuel type
REAL         FPghg(MAXCLASS,MAXGROUP,MNUMYR)                        ! EPA GHG (g/mi) by size class
REAL         FPghgGrp(MAXGROUP,MNUMYR)                             ! EPA GHG (g/mi) by manufacturer
REAL         FPMpg(MAXCLASS,MAXGROUP,MNUMYR)                        ! CAFE by size class
REAL         FPMpgGrp(MAXGROUP,MNUMYR)                              ! CAFE by manufacturer
REAL         Cafe_Used(MAXGROUP,byr:lyr)                            ! CAFE standard used for light trucks
REAL         REF_MPG(MaxVTYP,6:MNUMYR)                              ! reference case tested fuel economy for cafe cases (update annually)
REAL         FUEL__D_(MNUMYR)                                       ! fuel price for CAFE case
REAL         SAVED__D_(MaxVTYP,MNUMYR)                              ! fuel saved from CAFE case
REAL		 AC_OC_CREDIT(MAXGROUP,MNUMYR)                          ! AC and off cycle CAFE credits 
INTEGER      FUEL_N

! various transportation variables
REAL         CLASS_SHARE(MNUMCR,MAXCLASS,MAXGROUP,BYR:LYR)          ! vehicle class market shares (within vehicle groups)
REAL         COEF_A(MAXCLASS,MAXGROUP)                              ! ATV Y-intercept or alpha coefficient
REAL         COEF_B(MAXCLASS,MAXGROUP)                              ! ATV fuel price elasticities
REAL         COEF_C(MAXCLASS,MAXGROUP)                              ! ATV income elasticities
REAL         COEF_P(MAXCLASS,MAXGROUP)                              ! ATV vehicle price elasticities

!...Plug-in Electric Vehicle modeling
REAL		 BatPackWgt(BYR:LYR,MAXCLASS,MAXGROUP,MAXLDV)			! Battery weight (full pack) for HEV,PHEV,BEV,and FCVs
REAL         ElecSysIncCost(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)  ! Cost of on-board electricity systems and storage
! 	Battery model parameters
INTEGER		 FIRST_BAT_YR											! First year LIONCOSTCALC is called (first battery price projection year)
REAL         NiMH_Cost(BYR:LYR)                                     ! Nickel metal hydride battery cost ($/kWhr)
REAL         PACK_A(MAXLDV)                                         ! Cumulative Li-ion battery pack initial cost parameter
REAL         PACK_B(MAXLDV)                                         ! Cumulative Li-ion battery pack learning rate parameter
REAL         MAT_A(MAXLDV)                                          ! Cumulative Li-ion materials parameter
REAL         MAT_B(MAXLDV)                                          ! Cumulative Li-ion materials learning rate parameter
REAL		 MAT_MARKUP(MNUMYR)										! Price adjustment to account for critical mineral supply constraints
REAL         EV_range(MAXCLASS,MAXGROUP,maxldv,byr:lyr)             ! EV all electric range for EV100, EV200, EV300, PHEV20, PHEV50

REAL 		 EV_range_m(MAXLDV)										! Range slope based on EV battery size
REAL 		 EV_range_b(MAXLDV)										! Range constant based on EV battery size
REAL 		 LION_LB_perkWh(MAXLDV)									! Lithium-ion weight (lbs) per kWh battery capacity
REAL 		 LIONkWh_perLb(maxclass,maxgroup,maxldv)				! Battery sizing factor (kWh) based on vehicle weight
REAL         PHEV_DOD(BYR:LYR)                                      ! depth of discharge percentage for PHEV battery
REAL         EV_DOD(BYR:LYR)                                        ! depth of discharge percentage for EV battery
! 	Non-battery incremental costs
REAL		 ElecNonBattCst(MAXCLASS,BYR:LYR,MAXVTYP,MAXLDV)		! Non-battery electric (HEV, PHEV, BEV) incremental costs
REAL		 CSRATIO(MAXCLASS,MAXGROUP,PHEVTYPE)					! phev mpg ratio to non-hybrid gasoline vehilce
REAL         PctPHEV20(MNUMYR)                               		! Percent of PHEV20 energy use that is electric
REAL         PctPHEV50(MNUMYR)                               		! Percent of PHEV50 energy use that is electric

REAL         FuelCell_D_kW(BYR:LYR,FCLO:FCHI)                       ! fuel cell cost ($/kW)
REAL         FUELCELL(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)        ! incremental fuel cell cost 
REAL 		 STATE_CRED(MNUMCR-2,2023:LYR,3)						! EV, PHEV and HEV state tax credits sales weighted to CD
REAL		 IRA_BAT_CRED											! IRA EV/PHEV battery tax credit
REAL		 IRA_VEH_CRED											! IRA EV/PHEV vehicle tax credit
REAL		 IRA_BAT_SHR(2,IRAYR:LYR,2)								! Share of qualifying batteries {1 = EV, 2 = PHEV} {1:Reference, 2: No CAFE side case (AEO2025)}
REAL		 IRA_VEH_SHR(2,IRAYR:LYR,2)								! Share of gualifying vehicles {1 = EV, 2 = PHEV}  {1:Reference, 2: No CAFE side case (AEO2025)}
REAL         TEC_ORNL(MAXCLASS,MAXGROUP,MAXTECH,MAXLDV)             ! tech cost
REAL         MKT_PENF(MAXGROUP,MAXTECH,MAXLDV)                      ! tech penetration agg over class
REAL         AVCOST(MAXGROUP,MAXTECH,MAXLDV)                        ! tech cost agg over class
REAL         MICROPEN(MAXVTYP,MAXLDV,MNUMYR)                        ! market penetration of micro hybrids

! consumer choice model coefficents
REAL		MMAVAIL(MAXGROUP,MAXCLASS,MAXLDV,MNUMCR-2,byr:lyr)      ! ATV make/model availability
REAL		X210(MAXGROUP,MAXCLASS,MAXLDV,MNUMCR-2)                 ! ATV calibration coefficients
REAL		NMLMCO(MAXNMLM,MAXCLASS,MAXGROUP)						! nmlm variables used in consumer choice model
REAL		NMLMCOCAR(MAXNMLM,MAXCLASS,CARGRP)						! CAR coefficients by census division from trnldvx
REAL		NMLMCOTRK(MAXNMLM,MAXCLASS,LTKGRP)						! TRK coefficients by census division from trnldvx
REAL 		ATVCOCAR1(MAXLDV,MNUMCR-2,MAXCLASS)						! CAR1 calibration coefficients 
REAL 		ATVCOCAR2(MAXLDV,MNUMCR-2,MAXCLASS)						! CAR2 calibration coefficients
REAL 		ATVCOCAR3(MAXLDV,MNUMCR-2,MAXCLASS)						! CAR3 calibration coefficients
REAL 		ATVCOCAR4(MAXLDV,MNUMCR-2,MAXCLASS)						! CAR4 calibration coefficients
REAL 		ATVCOCAR5(MAXLDV,MNUMCR-2,MAXCLASS)						! CAR5 calibration coefficients
REAL 		ATVCOTRK1(MAXLDV,MNUMCR-2,MAXCLASS)						! TRK1 calibration coefficients
REAL 		ATVCOTRK2(MAXLDV,MNUMCR-2,MAXCLASS)						! TRK2 calibration coefficients
REAL 		ATVCOTRK3(MAXLDV,MNUMCR-2,MAXCLASS)						! TRK3 calibration coefficients
REAL 		ATVCOTRK4(MAXLDV,MNUMCR-2,MAXCLASS)						! TRK4 calibration coefficients
REAL 		ATVCOTRK5(MAXLDV,MNUMCR-2,MAXCLASS)						! TRK5 calibration coefficients
REAL 		ATVCOTRK6(MAXLDV,MNUMCR-2,MAXCLASS)						! TRK6 calibration coefficients
REAL        ATVCOEF_CALIB(MAXLDV,MAXVTYP)                           ! Alt specific coefficient calibration to align first projection year with prelim sales data

REAL         FAVAIL(maxfuel,MNUMYR,MNUMCR-2)                        ! fuel availability by fuel, region, year
REAL         INITSTA(maxfuel,MNUMYR,MNUMCR-2)                       ! initial refueling stations by fuel, yr, region
REAL         STA_RAT(maxfuel)                                       ! refuel stations per vehicle stock
REAL         PRT_CNT(MAXCHRG,MNUMYR,MNUMCR-2)                       ! ev charging ports count by type, region and year (2016:2032)
REAL         PRT_CNT_nc(MAXCHRG,MNUMYR,MNUMCR-2)                    ! No Cafe - ev charging ports count by type, region and year (2016:2032)
REAL         chg_dist(MNUMCR,3,MNUMYR)                              ! Distribution of BEV charging consumption by type {1:DCFC, 2:Home, 3:L2}
REAL         PRT_RT(MAXCHRG)                                        ! ev time to refuel by type
real         CHGCSTMULT(MAXCHRG)                                    ! Markup on comm'l electricity cost (represents what folks actually pay to charge)
REAL         PRT_VAR(MAXCHRG,5,MNUMCR-2)                            ! ev charging s curve variables for projection
INTEGER      CHR_STR_YR                                             ! first year charger data
INTEGER      CHR_LST_YR                                             ! last year charger data
REAL         MAINTGRP(MAXLDV,MAXCLASS,MAXVTYP)                      ! vehicle maintenance cost by tech, size class & type
REAL         WGT(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)                   ! light duty vehicle weight
REAL         PSPR(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,BYR:LYR)        ! vehicle price
REAL		 VRNG(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,BYR:LYR)          ! vehicle range
REAL         LDV_PRI(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)               ! vehice price by tech, class
REAL         LDV_RNG(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)               ! vehice range by tech, class
REAL         FPRICE(MAXLDV,MNUMCR,BYR:LYR)                          ! fuel price by region
REAL         FLCOST(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR,BYR:LYR)        ! fuel cost per mile 
REAL         BRCOST25(MAXGROUP,MAXLDV,MAXCLASS,BYR:LYR)             ! battery replacement cost - currently set to zero
REAL         ACCL(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,BYR:LYR)        ! vehicle acceleration - 0 to 60 mph
REAL         HFUEL(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,BYR:LYR)       ! home refueling 
REAL         MAINT(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,BYR:LYR)       ! vehicle maintenance cost
REAL         LUGG(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2)                ! vehilce luggage space	 
REAL		 LUGGAVG(MAXGROUP,MAXCLASS)
REAL         TrueMPG_regn(mnumcr,2,mnumyr)                          ! Regional true MPG variable

! ... zev mandate 
REAL         VSALES_T(MNUMCR-2,MAXLDV,MNUMYR)                       ! total cd sales for ZEV mandate calculations
REAL         Covered_Sales(MNUMCR-2)                                ! number of vehicles under ZEV regulation
REAL         ZEV_covered_sales(MNUMCR-2)                            ! number of vehicles under ZEV regulation only within ZEV states !nce
REAL         Credit_Transfer_Rate(MNUMCR-2)                         ! ZEV credit transfer from california to other states (CDs)
REAL         zev_credit_req(MNUMCR-2,MAXZEV,MNUMYR)                 ! required sales by zev category (ATPZEV, TZEV, ZEV)
REAL         s177_traveling_factor(MNUMCR-2)                        ! credit traveling factor/rate for traveling S177 credits to California
REAL         ZEV_State_Alloc(MNUMCR-2,MNUMYR)                       ! share of sales of 177 states by CD
REAL         ZEV_Requirement(MAXZEV,MNUMYR)                         ! Basic ZEV compliance pathway sales percentages by ATPZEV, TZEV, ZEV 
REAL		 ZEV_Requirement_optional(MAXZEV,MNUMYR)				! Optional S177 states compliance pathway sales percentages by ATPZEV, TZEV, ZEV
REAL		 zev_basebank(MNUMCR-2,4)
REAL         ZEV_Req_CA_CO2(MNUMYR)                                 ! ZEV CA CO2 emissions reduction "requirement" for ZEVs (SB32)
REAL         TZEV_REQ_CA_CO2(MNUMYR)                                ! TZEV CA CO2 emissions reduction "requirement" for TZEVs (SB32)
REAL         VMT_CA_CO2(mnumyr)                                     ! CA CO2
REAL         ZEV_Credit_LDV(MNUMCR-2,MAXLDV,MNUMYR)                 ! zev creidts earned by ldv type by CD
REAL         California_Credit(MNUMCR-2,MAXLDV)                     ! california zev credits allocated to to 177 states
REAL         ZEV_Credit_Earn(MNUMCR-2,MAXZEV,MNUMYR)                ! credits earned by zev type
REAL         ZEV_Multiplier(MAXLDV,MNUMYR)                          ! credits earned per zev type
REAL         Sales_Delta(MAXVTYP,MAXCLASS,MNUMCR-2,MAXLDV)
REAL         Adjusted_ZEV_Credit(MNUMCR-2,MAXLDV)                   ! sum of creidt adjustments
REAL		 CA_shr_of9                                       		! California's share of new sales for CD9
REAL		 CO_shr_of8                                       		! Coloradp's share of new sales for CD8
REAL		 VA_shr_of5                                       		! Virginia's share of new sales for CD5
REAL		 MN_shr_of4                                       		! Minnesota's share of new sales for CD4
REAL         Bank_buffer(MNUMYR)									! Manufacturer risk mitigation strategy to keep buffer in bank of expected compliance requirement  
REAL         bank_spending_rate(MNUMCR-2,MNUMYR)					! ZEV mandate credit bank allowed spending rate
REAL         ZEV_sales_dist(MAXLDV)                                 ! Distriubtion of sales by type towards meeting ZEV compliance with sales "push"
REAL         TZEV_sales_dist(MAXLDV)                                ! Distriubtion of sales by type towards TZEV allowance with sales "push"

! ... Light Duty Vehicle Fleet Module
REAL         OLDFSTKT(MNUMCR,MAXVTYP,MAXLDV,MAXAGE)
REAL         SURVFLT(MAXFLEET,MAXAGE,MAXVTYP)                       ! LDV survival rate by fleet type
REAL         FLTVMTYR(MAXFLEET,MNUMYR,MAXVTYP)                      ! annual miles of travel per vehicle
REAL		 FLT_COVID(MAXFLEET,MNUMYR)								! covid impact on fleet vehicle travel
REAL		 FLTTRANS(MAXFLEET,MAXAGE,MAXVTYP)  					! fraction of ldvs transfering from a given fleet to households
REAL         FLTTOTMPG(MAXVTYP)                                     ! total fleet mpg
REAL         TOTFLTCAR(MAXVTYP)                                     ! total fleet car
REAL         FLTMPGTOT2(MAXVTYP)                                    ! total fleet mpg
REAL         FLTMPGNEW(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MNUMYR)   	! fleet MPG by vehicle type, fleet type, powertrain
REAL 		 FLTMPGSTK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXAGE,MNUMYR)
REAL         MPGFLTSTK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,mnumyr)              ! fleet
REAL         FLTFUELBTU(MNUMCR,MAXFUEL,MNUMYR)                      ! fleet
REAL         OLDFSTK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXAGE)            	! fleet vehicles transfered to HH stock
REAL         FLTECHSAL(MNUMCR,MAXVTYP,MAXFLEET,MAXCLASS,MAXLDV,MAXHAV) 	! fleet sales
REAL         FLTECHSTK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXHAV)          	! fleet stock
REAL         FLTECHVMT(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXHAV)          	! fleet vmt 
REAL         FLTECHGGE(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MNUMYR)			! fleet equivalent gasoline gallons 
REAL         FLTECHBTU(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MNUMYR)			! fleet Btu 
REAL		 FLTLDVBTU(MNUMCR,MAXVTYP,MAXLDV,MNUMYR)
REAL		 FLTLDVBTUT(MNUMCR,MAXLDV,MNUMYR)
REAL		 FLTGRPSAL(MNUMCR,MAXFLEET,MAXGROUP,MAXCLASS,MAXLDV) 	! fleet sales by group
REAL(4), allocatable :: FLT_STOCK(:,:,:,:,:,:,:)					! FLT_STOCK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXAGE,MAXHAV,MNUMYR) Fleet Stock

! ... Light Duty Vehicle Stock Module - LDV Stock Accounting Model
REAL         SURV25(MNUMCR,MAXAGE,MAXVTYP)                          ! 25 vintage survival rates for cars and light trucks
REAL         SURV_ADJ(MNUMYR)                                       ! low macro survival curve adjustment
REAL         SSURV25(MNUMCR,MAXAGE,MAXVTYP)                         ! 25 vintage survival rates for cars and light trucks
REAL         PVMT(MAXAGE,MNUMYR,MNUMCR,MAXLDV)                      ! car household vmt per vintage by region
REAL         LVMT(MAXAGE,MNUMYR,MNUMCR,MAXLDV)                      ! light household truck vmt per vintage by region
REAL		 VMT_SCHED_PARAM(MAXLDV,2)								! Parameters defining the convergence of EV VMT schedules with that of ICEs
REAL 		 DEGFACGRP(MAXGROUP,MAXCLASS,MAXLDV,MNUMYR)			  	! tested mpg on-road adjustment factors
REAL		 DEGFAC(MAXVTYP,MAXLDV,MNUMYR)					  		! average tested on-road adjustment factors   
REAL(4), allocatable :: LDV_STOCK(:,:,:,:,:,:,:)					! LDV_STOCK(mnumcr,maxvtyp,maxowner,maxldv,maxage,maxhav,mnumyr) total LDV stock
REAL         CMPGSTKGAS95(MAXVTYP,MAXAGE)                           ! on road mpg 1990 gasoline
REAL         STKAVGWGT(MAXVTYP,MAXAGE)                              ! stock average weight by vintage
REAL         TRWTCAR_HIST(MNUMYR)                                   ! historic average vehicle weight of car stock
REAL         TRWTTRK_HIST(MNUMYR)                                   ! historic average vehicle weight of light truck stock

! ... Light Duty Vehicle Stock Module - VMT Model
REAL         M_CD_AGE_DIST(AGEGRP,MNUMYR,MNUMCR)                    ! regional population distribution by age for male - ref case
REAL         F_CD_AGE_DIST(AGEGRP,MNUMYR,MNUMCR)                    ! regional population distribution by age for female - ref case
REAL         M_CD_AGE_DIST_L(AGEGRP,MNUMYR,MNUMCR)                  ! regional population distribution by age for male - low macro case
REAL         F_CD_AGE_DIST_L(AGEGRP,MNUMYR,MNUMCR)                  ! regional population distribution by age for female - low macro Case
REAL         M_CD_AGE_DIST_H(AGEGRP,MNUMYR,MNUMCR)                  ! regional population distribution by age for male - high macro case
REAL         F_CD_AGE_DIST_H(AGEGRP,MNUMYR,MNUMCR)                  ! regional population distribution by age for female - high macro case
REAL         AGE_ADJ(MF,MNUMYR)										! age cohort 5 vmt adjustment for aging population - ref case 
REAL		 AGE_ADJ_L(MF,MNUMYR)									! age cohort 5 vmt adjustment for aging population - low macro case
REAL		 AGE_ADJ_H(MF,MNUMYR)									! age cohort 5 vmt adjustment for aging population - high macro case
REAL         TMC_NP15A(AGEGRP,MF,MNUMCR,MNUMYR)                     ! population    
REAL         EMP_RATE_LD(MNUMYR)                                    ! national employment rate for licensing rate equation
REAL         EMP_RATE_VMT(MNUMYR)                                   ! national employment rate for vmt equation
REAL         BETACOST(MF,AGEGRP)                                    ! coefficient price
REAL         BETAINC(MF,AGEGRP)                                     ! coefficient income
REAL         BETAVMT(MF,AGEGRP)                                     ! coefficient lag vmt
REAL         BETAVPLD(MF,AGEGRP)                                    ! coefficient vehicle per licensed driver
REAL         BETAEMP(MF,AGEGRP)                                     ! coefficient unemployment
REAL         ALPHA(MF,AGEGRP)                                       ! constant for ldv vmt equation
REAL         VMTLD(AGEGRP,MNUMYR,MF)                                ! VMT per licensed driver (1000,s)
REAL         VPLD(MNUMYR)                                           ! light duty vehicles per licensed driver
INTEGER      VMTLDHISTYR                                            ! last historic data year for VMTLD
REAL         VMTLDV(AGEGRP,MNUMYR,MF,MNUMCR)                 		! total ldv (<8,500 lbs. gvwr) household and fleet vmt
REAL         COSTMI(MNUMYR)                                         ! fuel cost of driving 1 mile (2004 cents per gallon)

! ... Rail Freight Module
REAL         TQRAILR(4,MNUMCR,MNUMYR)                               ! regional freight rail energy demand by fuel type
REAL         RAIL_FUEL(4)                                           ! historic rail fuel share 1)diesel 2)residual 3)CNG 4)LNG
REAL         LNG_MAXPEN(40)                                         ! LNG new/rebuild locomotives as share of total motive stock
REAL         LOCOM_LIFE(30)                                         ! locomotive lifecycle utilization rate
INTEGER      RailHistyr                                             ! last historic year
REAL         RHIST_NCTONMI(MNUMYR,MNUMCR-2)                         ! historic total freight rail non-coal ton-miles
REAL         RHIST_CTONMI(MNUMYR,MNUMCR-2)                          ! historic total freight rail coal ton-miles
REAL		 RPROJ_NCTONMI(MNUMYR,MNUMCR,16)  						! non-coal ton-miles for projected year
REAL		 RPROJ_CTONMI(MNUMYR,MNUMCR)       						! coal ton-miles for projected year
REAL         RAIL_TONMILE(MNUMYR,MNUMCR)                            ! freight rail ton-miles travelled (billion)
REAL         RTM_OUTPUT(MNUMCR-2,16)                                ! freight rail ton-miles per $ output
REAL         RTM_SHARES(MNUMCR-2,16)                                ! Commodity distribution ofnotepad rail ton-miles for each region, in latest FAF year
REAL         FREFF(MNUMYR)                                          ! freight rail efficiency (1000 Btu/ton-mile)
REAL         HTFREFF(MNUMYR)                                        ! high tech case freight rail efficiency (1000 Btu/ton-mile)
REAL         TQFRAILT(MNUMYR,MNUMCR)                                ! freight rail energy demand
REAL         BRTMTT(MNUMYR,MNUMCR)                                  ! benchmarked freight rail ton-miles (billion)
REAL         RTMTT(MNUMYR,MNUMCR)                                   ! freight rail ton-miles (billion)
INTEGER      NGYEAR						    						! first year of LNG locomotive availability
REAL         CIDISCOUNT						    					! discount applied to Class I Railroad diesel fuel price
REAL         LOCOMBTU						    					! average annual Btu by Class I Railroad locomotive
REAL         DISCRT                                                 ! discount rate applied by freight railroads
INTEGER      PAYBK						    						! payback period demanded by freight railroads (must be <31 years)
REAL         RLNG_INCCOST                                           ! LNG locomotive incremental cost (2012$)
REAL         RLNG_LEARN                                             ! (1-learning rate) applied to LNG locomotive incremental cost

! ... Waterborne Freight Module
! ... Domestic Waterborne 
REAL         DSHIP_TONMILE(MNUMYR,MNUMCR)                           ! domestic marine ton-miles travelled (billion)
REAL         TQDSHIP(4,MNUMCR-2,MNUMYR)                             ! domestic marine energy demand by fuel type
                                                                    ! 1) diesel 2) residual 3) CNG 4) LNG
! ... Military Energy Demand
REAL         MFD(4,MNUMYR)                                          ! total domestic military use by fuel type
                                                                    !   1: distillate
                                                                    !   2: jet fuel naphtha
                                                                    !   3: residual
                                                                    !   4: jet fuel (kerosene)
REAL         MFDH(4,MNUMYR)                                         ! historic military fuel use by fuel type
INTEGER      MILTHISTYR                                             ! index year for last historic record
REAL         MILTRSHR90(4,MNUMCR-2)                                 ! military regional consumption shares by fuel & region
REAL         QMILTR(4,MNUMCR,MNUMYR)                                ! military energy demand by fuel by region

! ... Transit Rail 
INTEGER      TRHISTYEAR                                             ! last year of historical data
REAL		 TR_COEF(MNUMCR-2,4)                                    ! tranist rail travel coefs 1-constant, 2-gdp/cap, 3-gasoline price, 4-COVID
REAL      TRCOVID(MNUMCR-2,MNUMYR)                                ! transit rail COVID impact
REAL		 TRRPMPC(MNUMCR-2,MNUMYR)								! transit rail travel by non-farm employee
REAL         TRRPM(MNUMCR-2,MNUMYR)                                 ! transit rail passenger miles traveled
REAL         TRRPMHIST(MNUMCR-2,MNUMYR)                             ! historic transit rail passenger miles traveled
REAL         TREFF(MNUMCR-2,MNUMYR)                                 ! transit rail efficiengy (Btu/passenger mile)
REAL         TREFFHIST(MNUMCR-2,MNUMYR)                             ! historic transit rail efficiency
REAL         TRED(MNUMCR,MNUMYR)                                    ! transit rail energy demand by CD
REAL         TREDHIST(MNUMCR-2,MNUMYR)                              ! historic transit rail energy use
REAL         TR_CAV_ADJ(MNUMYR)                                     ! pmt adjustment due to CAV growth

! ... Commuter Rail
INTEGER      CRHISTYEAR                                             ! last year of historical data
REAL		 CR_COEF(MNUMCR-2,4)                                    ! commuter rail travel coefs 1-constant, 2-gdp/cap, 3-gasoline price, 4-COVID
REAL		 CRCOVID(MNUMCR-2,MNUMYR)										! commuter rail COVID impact
REAL         CRCON(MNUMCR-2)                                        ! travel demand constant term
REAL         CRINC(MNUMCR-2)                                        ! travel demand log of income
REAL         CRFC(MNUMCR-2)                                         ! travel demand fuel cost 2004$
REAL         CRDUM(MNUMCR-2)                                        ! travel demand dummy
REAL         CRRPM(MNUMCR-2,MNUMYR)                                 ! commuter rail passenger miles traveled
REAL         CRRPMHIST(MNUMCR-2,MNUMYR)                             ! historic commuter rail passenger miles traveled
REAL         CREFF(MNUMCR-2,MNUMYR)                                 ! commuter rail efficiengy (Btu/passenger mile)
REAL         CREFFHIST(MNUMCR-2,MNUMYR)                             ! historic commuter rail efficiency
REAL         CRED(MNUMCR,MNUMYR)                                    ! commuter rail energy demand by CD
REAL         CREDD(MNUMCR,MNUMYR)                                   ! commuter rail diesel demand by CD
REAL         CREDE(MNUMCR,MNUMYR)                                   ! commuter rail electricty demand by CD
REAL         CREDDHIST(MNUMCR-2,MNUMYR)                             ! historic commuter rail diesel demand
REAL         CREDEHIST(MNUMCR-2,MNUMYR)                             ! historic commuter rail electricity demand
REAL         CREDDSHR(MNUMCR-2)                                     ! share of commuter rail energy demand that is diesel
REAL		 CR_CAV_ADJ(MNUMYR)                                     ! pmt adjustment due to CAV growth

! ... Intercity Rail
INTEGER      IRHISTYEAR                                             ! last year of historical data
REAL         IRCON                                                  ! travel demand constant term
REAL         IRINC                                                  ! travel demand log of income
REAL         IRPMCL                                                 ! lag of travel demand per capita
REAL         IRFC                                                   ! travel demand fuel cost 2004$
REAL         IRPMPC(MNUMYR)                                         ! passenger miles per capita (16+)
REAL         IRPMPCHIST(MNUMYR)                                     ! historic passenger miles per capita (16+)
REAL         IRRPM(MNUMYR)                                          ! intercity rail passenger miles traveled
REAL         IRRPMHIST(MNUMYR)                                      ! historic intercity rail passenger miles traveled
REAL         IREFF(MNUMYR)                                          ! intercity rail efficiengy (Btu/passenger mile)
REAL         IREFFHIST(MNUMYR)                                      ! historic intercity rail efficiency
REAL         IRED(MNUMYR)                                           ! intercity rail energy demand
REAL         IREDD(MNUMYR)                                          ! intercity rail diesel demand
REAL         IREDE(MNUMYR)                                          ! intercity rail electricty demand
REAL         IREDDSHR                                               ! diesel share of total demand
REAL         IRREGSHR(MNUMCR-2)                                     ! fuel shares by region
REAL         IREDDR(MNUMCR,MNUMYR)                                  ! intercity rail diesel demand by CD
REAL         IREDER(MNUMCR,MNUMYR)                                  ! intercity rail electricty demand by CD
REAL         IREDDHIST(MNUMYR)                                      ! historic intercity rail diesel demand
REAL         IREDEHIST(MNUMYR)                                      ! historic intercity rail electricity demand

! ... Total Passenger Rail
REAL         QMTRR(4,MNUMCR,MNUMYR)                                 ! passenger rail energy demand by fuel by region
                                                                    !   1) diesel
                                                                    !   2) electricity
                                                                    !   3) CNG
                                                                    !   4) LNG
!...Bus Model
REAL    QMTBR(3,8,MNUMCR,MNUMYR)                                  	! bus energy demand by fuel by region (all types)
!...  Transit Bus
INTEGER TBHISTYEAR                                                	! last year of historical data
REAL    TBPMT(MNUMCR,MNUMYR)                                      	! passenger miles traveled
REAL    TBPMTHIST(MNUMCR-2,MNUMYR)                                	! historic passenger miles traveled
REAL    TBCOVID(MNUMCR-2,MNUMYR)												! transit bus COVID travel impact
REAL	TB_COEF(MNUMCR-2,4)                                         ! tranist bus travel coefs 1-constant, 2-gdp/cap, 3-gasoline price, 4-COVID
REAL    TBPMTPC(MNUMCR-2,MNUMYR)                                  	! average passenger miles traveled per region
REAL    TBPMTPC08(MNUMCR-2)                                      	! last history year travel per capita
REAL    TBPMTPCGR(MNUMCR-2)                                       	! TBPMTPC average historical growth rate
REAL    TBFSHR(MNUMCR-2,8,MNUMYR)                                 	! fuel share by Census Division
REAL    TBFSHRHIST(8,MNUMYR,MNUMCR-2)                             	! historic fuel share by Census Division 
REAL    TBBTUPM(MNUMCR-2,MNUMYR)                                  	! Btu per passenger mile
REAL    TBBTUPMHIST(MNUMCR-2,MNUMYR)                              	! historic Btu per passenger mile
REAL    TBSYSEFF(MNUMCR-2)                                        	! historic improvement in Btu per passenger mile
REAL	TB_CAV_ADJ(MNUMYR)                                        	! pmt adjustment due to CAV growth
!...  Intercity and school bus
INTEGER IBSBHISTYEAR                                              	! last year of historical data
REAL    TMODINIT(2,MNUMYR)                                          ! historic bus passenger miles
REAL    TMCOVID(2,MNUMYR)											! COVID travel impact for intercity and school bus
REAL    TMPASMIL(2)                                               	! passenger miles per capita by bus type
REAL    TMOD(2,MNUMYR)												! intercity/school bus passenger miles
REAL	TMOD_R(2,8,MNUMCR-2,MNUMYR)									! regional intercity/school bus passenger miles
REAL    TMEFFINIT(2,MNUMYR)                                         ! historic bus efficiency (btu/passenger mile)
REAL    TMEFF(2,MNUMYR)                                           	! bus efficiency (btu/passenger mile)
REAL	TMEFF_F(2,8,MNUMYR)											! bus efficiency by fuel type by bus type
REAL	EFF_Adj(8)													! school bus fuel efficiency adjustment
REAL    QMODFSHR(2,8,MNUMYR)                                      	! intercity/school bus fuel shares
REAL    QMODFSHRH(8,mnumyr,2)                                       ! historic bus fuel shares !cclnfort
REAL	SchBus_Shr(MNUMCR-2)										! share of school bus vmt by region
REAL	SchBus_PMT_Shr(8,MNUMCR-2,MNUMYR)							! share of school bus pmt by region by by fuel type
REAL	SchBus_EV_Shr(MNUMCR-2,MNUMYR)								! share of school bus vmt by electric
REAL	SchBus_Adj(MNUMCR-2,MNUMYR)									! used to normalize bus vmt shares

! ... Recreational Boating
REAL         RECFD(2,MNUMYR)                                        ! energy demand by fuel type (gasoline, diesel)
REAL         RECFDH(2,MNUMYR)                                       ! historic energy demand by fuel type
INTEGER      RBHISTYR                                               ! index year for last historic record
REAL         RBCON(2)                                               ! energy demand constant term
REAL         RBINC(2)                                               ! energy demand log of income 
REAL         RBFC(2)                                                ! energy demand fuel cost 2004$
REAL         RBEDPC(2,MNUMYR)                                       ! energy demand per capita by fuel type
REAL         QRECR(2,MNUMCR,MNUMYR)                                 ! recreational boat energy demand by region

! ... Transportation Lubricant Demand
REAL         LUBFD(MNUMYR)                                          ! transportation lubricant demand
REAL         LUBFDH(MNUMYR)                                         ! historic transportation lubricant demand
INTEGER      LUBHISTYR                                              ! index year for last historic record 

REAL         Cyc_RPM(MNUMYR)                                        ! motorcycle/3-wheel passenger miles
INTEGER      CycHistYR                                              ! index year for last historic record
REAL         TNCSALE(MNUMCR,MNUMYR)
REAL         TNTSALE(MNUMCR,MNUMYR)

! ... Car light truck sales shares
REAL         DUMM(MNUMYR)                                           ! car share dummy
REAL         CCONSTANT(mnumcr-2)                                    ! constant for use in determining new car sales
REAL         CRHO(mnumcr-2)                                         ! 
REAL         CINC(mnumcr-2)                                         ! disposable income coefficient
REAL         CFUEL(mnumcr-2)                                        ! fuel cost coefficient
REAL         CHP(mnumcr-2)                                          ! HP coefficient
REAL         CWGT(mnumcr-2)                                         ! vehicle weight coefficient
REAL         CMPG(mnumcr-2)                                         ! fuel economy coefficient
REAL         CDUMM(mnumcr-2)                                        ! dummy variable
REAL         TCONSTANT(mnumcr-2)                                    ! constant coefficent for determining new truck sales
REAL         TRHO(mnumcr-2)                                         ! 
REAL         TINC(mnumcr-2)                                         ! disposable income coefficient
REAL         TFUEL(mnumcr-2)                                        ! fuel cost coefficient
REAL         THP(mnumcr-2)                                          ! HP coefficient
REAL         TWGT(mnumcr-2)                                         ! vehicle wieght coefficient
REAL         TMPG(mnumcr-2)                                         ! fuel economy coefficient
REAL         TDUMM(mnumcr-2)                                        ! dummy variable
REAL         NEWLDVPERLD(MNUMCR-2,MNUMYR)
CHARACTER*15 FTYPELABEL(MAXLDV) 
REAL         GRPSHARE(MNUMCR,MAXGROUP,MNUMYR)	                    ! car and light truck sales shares by group
REAL		 OWNSALESSHR(MAXOWNER,MAXGROUP,MAXCLASS,MAXLDV,MNUMCR-2,MNUMYR)	! car and light truck sales shares by owner type
REAL		 ownsaletemp(maxowner,maxgroup,maxclass,mnumcr-2,mnumyr)!
REAL         RATIO_BYR                                              ! used to determine size class shares 
REAL         RATIO_LN                                               ! used to determine size class shares
REAL         RATIO                                                  ! used to determine size class shares
REAL         GROUPSUM(MAXGROUP)                                     ! sum of class shares by manufacturer
REAL         AHPCAR(MNUMCR,MNUMYR)                                  ! average car horsepower
REAL         AHPTRUCK(MNUMCR,MNUMYR)                                ! average light truck horsepower
REAL         AWTCAR(MNUMCR,MNUMYR)                                  ! average car weight
REAL         AWTTRUCK(MNUMCR,MNUMYR)                                ! average light truck weight
INTEGER*2    PASS,pass2
INTEGER*2    RETURN_STAT                                            ! technology supersedes check
REAL         SUM_MKS                                                ! diagnostic used to check forecasted vs. actual mpg
REAL         SUM_MKS_FE                                             ! diagnostic used to check forecasted vs. actual mpg
REAL         REGCOST(MAXGROUP)                                      ! CAFE Fine
real         GBInc                                                  ! the increment of the HP give back that is to be used
REAL         FUELSAVE(MAXTECH)                                      ! expected fuel savings of advanced subsystem technology
REAL         TECHCOST(MAXTECH)                                      ! first cost of subsystem technology - cost adjustments 
                                                                    ! (economies of scale, etc) made to this value
REAL         COSTEF_FUEL(MAXTECH)                                   ! cost effectiveness of mpg improvement of a subsystem technology
REAL         COSTEF_PERF(MAXTECH)                                   ! cost effectiveness of performance improvement of a subsystem technology
REAL         VAL_PERF(MAXTECH)                                      ! value of performance improvement to consumer 
REAL         OLD_PMAX(MAXCLASS,MAXGROUP,MAXTECH,MAXLDV,2)           ! used to determine incremental increase in subsystem market penetration
REAL         MKT_FUEL(MAXTECH)                                      ! subsystem technology market share based on efficiency cost effectiveness
REAL         MKT_PERF(MAXTECH)                                      ! subsystem technology market share based on performance cost effectiveness
REAL         ADJFE                                                  ! fuel economy adjustment associated with performance adjustment
REAL         DELTA_MKT                                              ! incremental increase in subsystem technology market share
REAL         REQ_MKT                                                ! required market share - see engineering notes
REAL         RANGE(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)           ! vehicle driving range
real         MAXHISTRNG(MAXCLASS,MAXGROUP,MAXLDV)                   ! max historical range
LOGICAL*1    REQUIRED                                               ! indicates required subsystem technology
REAL         GRPCLSHR(MAXGROUP,MAXCLASS,MNUMYR)                     ! manufacture share of sales by size class
REAL         MPGHH(MNUMYR)											! household stock fuel economy
REAL         PE(MNUMYR)                                             ! vmt price elasticity
REAL         IE(MNUMYR)                                             ! vmt income elasticity
REAL         DE(MNUMYR)
REAL         MPGT(MAXLDV,MNUMCR,MNUMYR)
REAL         MPGC(MAXLDV,MNUMCR,MNUMYR)
REAL         TLDVMPG(3,MNUMYR)                                      ! on-road stock fuel economy for all cars, light trucks, & total
REAL         TOTLEV(MNUMCR)
REAL         EPACTOT                                                ! Total EPAct92 fleet vehicle sales
REAL 		 NLDVTECH(MNUMCR,MAXCLASS,MAXLDV,MNUMYR)			  
REAL 		 NLDVGRPSALES(MNUMCR,MAXGROUP,MAXCLASS,MAXLDV,MAXOWNER,MNUMYR)
REAL         TQLDV(9,MNUMCR,MNUMYR)
REAL         FAVL(MAXLDV,MNUMCR-2,BYR:LYR)
REAL		 LDV_SALES(MAXGROUP,MAXCLASS,MAXLDV,MNUMCR,MNUMYR)    ! consumer choice model total ldv sales (household and fleet)
REAL 		 HHGRPSAL(MNUMCR,MAXGROUP,MAXCLASS,MAXLDV,MNUMYR)	  ! household vehicle sales by mfr group 
REAL 		 HHTECHSAL(MNUMCR,MAXVTYP,MAXCLASS,MAXLDV,MNUMYR)	  ! household vehicle sales by vehicle type
REAL		 HHMPGNEW(MNUMCR,MAXVTYP,MAXLDV,MNUMYR)			  	  ! household new vehicle fuel economy
REAL		 HHMPGSTK(MNUMCR,MAXVTYP,MAXLDV,MAXAGE,MNUMYR)
REAL 		 HHTECHGGE(MNUMCR,MAXVTYP,MAXLDV,MAXAGE,MNUMYR)
REAL 		 HHTECHBTU(MNUMCR,MAXVTYP,MAXLDV,MNUMYR)
REAL 		 LDVMPGNEW(MNUMCR,MAXVTYP,MAXLDV,MNUMYR)              ! average new ldv mpg 
REAL         APSHR55(MAXVTYP,MAXCLASS,MNUMCR,MAXLDV)              ! adjusted shares afer ZEV mandate
REAL		 APSHRGRP(MAXGROUP,MAXCLASS,MNUMCR,MAXLDV,MNUMYR)	  ! consumer choice by maxgroup
REAL         MPG_ACTUAL(MAXVTYP,MNUMYR)
REAL         AVSALES(maxvtyp,MAXCLASS,MNUMCR,MAXLDV)
REAL         AVSALEST(maxvtyp,MAXCLASS,MNUMCR)
REAL         REG_SHARE_ADJ(MNUMCR-2,MNUMYR)
REAL         EXPENDVEH(2,MNUMYR)
REAL         MPGTECH(MAXLDV,MNUMYR)									!household stock fuel economy by fuel type
REAL         AFVFE(MAXVTYP,MAXCLASS,MNUMYR)	 						! average AFV mpg by class for table 52
REAL         AFVFETOT(MAXVTYP,MNUMYR)								! average AFV mpg for table 52
REAL         SCMPG(MNUMYR)                                  		! national on-road stock mpg household cars
REAL         STMPG(MNUMYR)                                  		! national on-road stock mpg household light trucks
REAL         CMPG_IT(MAXLDV,MNUMYR)									! household car stock fuel economy by fuel type
REAL         TMPG_IT(MAXLDV,MNUMYR)									! household LT stock fuel economy by fuel type
REAL         VMT_STK_HH(maxvtyp,MAXLDV,MAXAGE,MAXHAV,mnumcr)      	! hh vehicle miles traveled car by region derived from stocks
REAL 		 HHTECHVMT(MNUMCR,MAXVTYP,MAXLDV,MAXAGE)
REAL         BVMT_STK_HH(maxvtyp,MAXLDV,MAXAGE,MAXHAV,mnumcr-2)     ! benchmarked hh vehicle miles traveled car by region
REAL         RSHR(MNUMCR,MNUMYR)									! regional share of total VMT (calculated from VMTLDV)
REAL         LDVSTK(MAXLDV,MNUMYR)                          		! total light duty vehicle stock by ILDV
REAL         STKCAR(MNUMYR)                                 		! total non-fleet car stock
REAL         STKTR(MNUMYR)                                  		! total non-fleet light truck stock
REAL         VSTK(maxvtyp,MAXLDV)                           		! total light duty vehicle stock 
integer      PassNo               									! Controls two passes for high and low volume sales.
REAL         MFR_SALES(MNUMCR,MAXGROUP,MAXCLASS,MNUMYR)

! ... Variables for CAFE banking.
real CafeBankA(MAXGROUP),CafeBank(5,MAXGROUP),BankBal(MAXGROUP,byr:lyr),CafeWork(5,MAXGROUP),CafeNeed
common/BankVars/CafeBankA,CafeBank,CafeWork,CafeNeed

common/FPVars/FPrint,TFCoefA,TFCoefB,TFCoefC,TFCoefD,FPMpg, LDV_MPG_CL, FPMpgGrp,Cafe_Used,AltCafe,IBank,&
             CFCoefA,CFCoefB,CFCoefC,CFCoefD,TFCoefA2,TFCoefB2,TFCoefC2,TFCoefD2,TFCoefE2,TFCoefF2,TFCoefG2,TFCoefH2,&			
             CFCoefA2,CFCoefB2,CFCoefC2,CFCoefD2,CAFEPEFMULT,EPAALTMULT,RUN_EPA,CAFEMY27_SWITCH,AC_CO2_OFFSET,&									
			 CFCoefEPAA2,CFCoefEPAB2,CFCoefEPAC2,CFCoefEPAD2,TFCoefEPAA2,TFCoefEPAB2,TFCoefEPAC2,TFCoefEPAD2,TFCoefEPAE2,&		
			 TFCoefEPAF2,TFCoefEPAG2,TFCoefEPAH2																				

!=======================================================================================================
REAL         TOTALSALSC(maxvtyp,MAXCLASS,MAXLDV,MNUMYR)

REAL         CarSales,TrkSales,CarSales2,TrkSales2
!===================================================================================
! ... to accomodate view variables in the Visual Studio, the variables are
! ... added to common blocks here.  Then the USE statement for module T_ must go in every 
! ... subroutine.

common/trancomreal/INC00_D_16, ROUNDOFF_ERROR, DEL_FE, DEL_COSTABS, DEL_COSTWGT, DEL_WGTABS, DEL_WGTWGT, DEL_HP, 	&
	COEFF_LEARN, COEFF_LRN1, COEFF_LRN2, YEARS_MKTD, LEARN_COST_MULTIPLIER, VMT, BEN_MG,BEN_JF,BEN_DS,BEN_RS, FE,	&
	WEIGHT, PRICE, HP, VALUEPERF, PERFFACT, TANKSIZE, PERFCAP, USEDCAP, MKT_PEN, MKT_MAX, SYNR_DEL, MANDMKSH, 		&
	DISCOUNT, REG_COST, CAFE_STAND, REF_MPG,CLASS_SHARE, COEF_A, COEF_B, COEF_C, COEF_P, COEF_E, AFVADJHP,&
	AFVADJFE, AFVADJWT, AFVADJPR, NiMH_Cost, PHEV_DOD, EV_DOD, BATTERY, ElecSysIncCost, FuelCell_D_kW, 				&
	ElecNonBattCst, PctPHEV20, PctPHEV50, BatPackWgt, IRA_Bat_CRed, IRA_Veh_cred, 		&
	FUELCELL, TEC_ORNL, MKT_PENF, AVCOST, MMAVAIL, X210, FAVAIL, INITSTA, STA_RAT, PRT_CNT, PRT_RT,CHGCSTMULT, 		&
	MAINTGRP, VSALES_T, Covered_Sales, ZEV_covered_sales, Credit_Transfer_Rate, zev_credit_req, 			&
	ZEV_State_Alloc, VMT_CA_CO2, ZEV_Requirement, ZEV_Requirement_optional, ZEV_Req_CA_CO2, ZEV_Credit_LDV, 		&
	California_Credit, ZEV_Credit_Earn, ZEV_Multiplier, s177_traveling_factor, Adjusted_ZEV_Credit, TZEV_REQ_CA_CO2,&
	CA_shr_of9, CO_shr_of8, VA_shr_of5, MN_shr_of4, Bank_buffer, zev_basebank, SURVFLT,FLTVMTYR, FLTTOTMPG, 		&
	TOTFLTCAR, FLTMPGTOT2, SURVP, SURVLT, PVMT, LVMT, VMT_SCHED_PARAM, bank_spending_rate, ZEV_sales_dist, & 
	FLTTRANS, CMPGSTKGAS95, STKAVGWGT, TZEV_sales_dist, M_CD_AGE_DIST, F_CD_AGE_DIST, M_CD_AGE_DIST_L, 				&
	F_CD_AGE_DIST_L, M_CD_AGE_DIST_H, F_CD_AGE_DIST_H, AGE_ADJ, AGE_ADJ_L, AGE_ADJ_H, BETACOST, BETAINC, BETAVMT, 	&
	BETAVPLD, BETAEMP, ALPHA, FREFF, HTFREFF, RPROJ_NCTONMI, RPROJ_CTONMI, 	&
	NEWCLS2B, RHIST_NCTONMI, RHIST_CTONMI, RAIL_TONMILE, DSHIP_TONMILE, RTM_OUTPUT, NEWLDVs, CarTrkSplit,  &
	MILTRSHR90, TMODINIT, TMCOVID, TMPASMIL, TMEFFINIT, QMODFSHR, QMODFSHRH, CARLTSHR, NEWLDVPERLD, 	& 
	DUMM, FUELTAX, FUELTAX87, TQDSHIP, GRPSHARE, AHPCAR, AHPTRUCK, AWTCAR, AWTTRUCK, PMGTR90_D_, INC90_D_NP, 		&
	REGCOST, FUELSAVE, TECHCOST, COSTEF_FUEL, COSTEF_PERF, MMAX, ACTUAL_MKT, MKT_FUEL, MKT_PERF, VAL_PERF, ADJFE, 	&
	DELTA_MKT, REQ_MKT, CFE, PRICE_EX, PSLOPE, RATIO_LN, RATIO, GROUPSUM, TOT_MKT, MAX_SHARE, RATIO_BYR, RANGE,	&
	MAXHISTRNG, OLD_PMAX, SUM_MKS, SUM_MKS_FE, FRZNEFF, HIGHEFF,	GRPCLSHR, WGT, PSPR, LDV_PRI, LDV_RNG, 	&
	FPRICE, FLCOST, BRCOST25, ACCL, HFUEL, MAINT, LUGG, MPGHH, PE, IE, VMTLD, VPLD, LDVCOVID, VMTLDHISTYR, 			&
	LICDRIVER, LIC_TREND, LIC_ELAS, LIC_MAX, LICRATE_M, LICRATE_F, LicRHistYr, DE,  	&
	FLTMPGNEW, TLDVMPG, TOTLEV, EPACTOT, TQLDV, BTQLDV, BTQISHIPR, BTQDSHIPR, BTQRAILR, BQJETR, BQAGR, BQMILTR, BQMODR,&
	BQRECR, BQLUBR, BFLTFUELBTU, BTQFREIRSC, BFVMTECHSC, BVMTECH, BFLTVMTECH, BASMDEMD, BRTMTT, FAVL, APSHRGRP, 	&
	APSHR55, MPG_ACTUAL, AVSALES, AVSALEST, MPGFLTSTK, FLTFUELBTU, OLDFSTK, FLTECHSTK, FLTECHSAL, EXPENDVEH, TQFRAILT, & 
	SSURV25, QMILTR, QMTRR, QMTBR, QRECR, MFD, TMFD, RECFD, LUBFD, TMOD, TMEFF, MPGTECH, AFVFE, AFVFETOT, SCMPG, &
	STMPG, CMPG_IT, TMPG_IT, VMT_STK_HH, BVMT_STK_HH, RTMTT, TQRAILR, RSHR, LDVSTK, & 
	STKCAR, STKTR, VSTK, VMTLDV, COSTMI, TOTALSALSC, CarSales,TrkSales,CarSales2,TrkSales2,  & 
	MER_tran, carltshr_regn, CCONSTANT, CRHO, CINC, CFUEL, 	&
	CHP, CWGT, CMPG, CDUMM, TCONSTANT, TRHO, TINC, TFUEL, THP, TWGT, TMPG, TDUMM, truempg_regn, LDVNEWMPG, DEGFAC, DEGFACGRP, OWNSALESSHR

common/tran_easy/tmgtcbus,tdstcpus,tjftcbus,trftcbus

common/trancomint4/iy, num_to_read, First_Read_Year, FRTEFF,TRANEFF,CAFEMEET,TRANAB32, ymer, ysteo

common/trancomint2/YRS,N, LASTID, ICL,IGP,IVTYP,ITECH, INOTE, IYR, ILDV, ifuel, IFLEET, IREGN, IRAILREGN,&
	IAGE, I, J, K, SIGN, NUMTECH, SYNERGY, MANDYEAR, NUM_REQ, NUM_SUP, NUM_MAN, NUM_SYN, PAYBACK, PASS, IHAV, 		&
	RETURN_STAT, EPAYR 

common/trancomlog/TECH_APPLIC, CLASSFLAG, MAND_ORIDE, CAFEPASS, REQUIRED

common/trancomchar/TECHLABEL, SYS_AFFECT, GROUPLABEL, CLASSLABEL !, FF 

common/morevars/PassNo

common/JAHVar/mfr_sales

!...Map for how groups are defined back to vehicle types: cars (1) and light trucks (2)
    data GrpMap/1,1,1,1,1,2,2,2,2,2,2/
!...Map for how groups are defined back to domestic (=1) and import (=2) (CAVs are considered domestic) 
    data GrpDIMap/1,2,2,2,1,1,1,1,2,2,1/

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . HAV model variables . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

PARAMETER (MAXLIDAR=2)                            		!...Number of lidar systems (1: low-speed, 2: high-speed)
PARAMETER (MAXPRODPHASE=4)                        		!...Number of lidar production phases
!                                                                   1 = R&D / Revolutionary
!                                                                   2 = Evolutionary
!                                                                   3 = Mature
!                                                                   4 = High volume (MAXPRODPHASE)

REAL :: LIDAR_COEF_A(MAXPRODPHASE,MAXLIDAR)				!...Cost model coefficient (2015$)
REAL :: LIDAR_COEF_B(MAXPRODPHASE,MAXLIDAR)				!...Cost model coefficient
INTEGER*2 :: first_lidar_year(MAXLIDAR) 				!...First year of commercial availability
REAL :: LIDAR_PROD_THRSH(MAXPRODPHASE,MAXLIDAR) 		!...Production threshold to reach current phase [lidar_prod(1)=0; lidar_prod(2)=pre-defined; others=0]
integer :: iphase										!...Production phase index
INTEGER :: lidar_phase(maxlidar, BYR:LYR)				!...Lidar production phase; FOR DEBUG WRITES ONLY
REAL :: lidarsales(maxlidar,BYR:LYR)=0.0				!...Annual lidar sales by type
REAL :: cumul_lidar_prod(maxlidar, BYR:LYR)				!...cumulative LiDAR production by system type and year
REAL :: LIDAR_RND_PROD(MAXLIDAR,BYR:LYR)				!...R&D lidar system production; indexed with calendar year
REAL :: lidar_cost(MAXHAV, BYR:LYR)=0.0 				!...Lidar system cost by year
REAL :: HAV_battery_kWh(maxhav-1)						!...HAV battery size for 4a [1], 4b [2], and 5 [3]

! ... HAV inputs for vehicle system without battery or lidar
INTEGER*2 :: hav_lidar_map(maxhav-1)					!...lidar system id (1:maxlidar) used by HAV vehicles 4a [1], 4b [2], and 5 [3]
REAL :: HAV_sys_lrn(maxhav-1, BYR:LYR)					!...HAV system time-based cost reductions for 4a [1], 4b [2], and 5 [3]
REAL :: hav_sys_cost(maxhav, BYR:LYR)=0.0				!...HAV system costs for 4a [1], 4b [2], and 5 [3]
REAL :: hav_techmap(maxhav-1)							!...Maps HAV levels to tech matrix indices (e.g. 4a:90, 4b:91, 5:92)

!...Fleet HAV adoption variables (FLTHAV)
REAL :: taxi_rev_params(maxhav,6)			! fleet adoption model parameter block:	
REAL :: taxi_mi_life(maxhav)				!	taxi lifetime miles
REAL :: taxi_idle_gph(maxhav)				!	taxi idle fuel rate, gallons/hr
REAL :: taxi_maint_cost(maxhav)				!	taxi maintenance costs, monthly component, 2015$
REAL :: taxi_data_fee(maxhav)				!	HAV data fee per month, 2015$
REAL :: taxi_insur(maxhav)					!	taxi insurance fee per month, 2015$
REAL :: hav_mpgdeg(maxhav, BYR:LYR)			!   MPG multiplier for HAVs; applied degradation factors 

REAL :: taxi_mi_ann(mnumcr-2, maxhav)		! taxi annual mileage
REAL :: taxi_idle_hrs(mnumcr-2, maxhav)		! taxi idle hours per month
REAL :: taxi_live_frac(mnumcr-2, maxhav)	! fraction of taxi miles that are live (generate revenue)
REAL :: taxi_maint_mi(mnumcr-2,maxhav)		! taxi maintenance cost; mileage component, 2015$
REAL :: taxi_rev_permi(mnumcr-2, maxhav)	! taxi revenue per mile, 2015$
REAL :: taxi_shifts(mnumcr-2, maxhav)		! average number of paid shifts per taxi per day
REAL :: hav_oper_limit(mnumcr-2, maxhav)	! hav operational limit attribute for adoption decision
REAL :: taxi_disc_r							! discount rate for taxi NPV calculations
REAL :: taxi_salary							! taxi driver salary
REAL :: taxi_rev_coef						! taxi net lifetime revenue coefficient for adoption decision
REAL :: hav_newtech_lim(maxhav)				! hav new technology limit attribute for adoption decision
REAL :: taxi_newtech_pd						! hav time based new tech function (Weibull) parameter, adjusts period / years
REAL :: taxi_newtech_r						! hav time based new tech function (Weibull) parameter, adjusts rate / slope
INTEGER*2 :: hav_newtech_lag				! number of years from introduction before newtech function starts to decline
	
REAL :: flt_hav_shares(mnumcr, maxvtyp, MAXCLASS, maxldv, BYR:LYR,maxhav)		! shares of taxi HAVs within cr, type, class, and ILDV.
REAL :: taxi_sales(mnumcr,maxvtyp,MAXCLASS,maxldv,BYR:LYR,maxhav)				! taxi sales by cr, type, class, ILDV, and ihav
	
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . End HAV model variables . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

!======================================================================================================      
      
end module T_

!======================================================================================================
  SUBROUTINE TRAN
  use T_
  IMPLICIT NONE
    INCLUDE 'NGTDMOUT'
    integer do_once/0/


    ROUNDOFF_ERROR=0.002                    ! roundoff error buffer

    FTYPELABEL(1:MAXLDV)=(/'Gasoline       ',  &
                           'Turbo DI Diesel',  &
                           'Ethanol Flex   ',  &
                           'Electric - 100 ',  &
                           'PHEV20 Gas     ',  &
                           'PHEV50 Gas     ',  &
                           'Electric - 200 ',  &
                           'Dsl/Elec Hybrid',  &
                           'CNG Bifuel     ',  &
                           'LPG Bifuel     ',  &
                           'CNG Dedicated  ',  &
                           'LPG Dedicated  ',  &
                           'F.Cell Methanol',  &
                           'F.Cell Hydrogen',  &
                           'Electric - 300 ',  &
                           'Gas/Elec Hybrid'/) 

    First_Read_Year = 1995
    IY = First_read_Year - BASEYR + 1
    Num_to_read = IJUMPYR -(First_Read_Year - BASEYR)
    
	if(curcalyr.lt.first_read_year) return

!...IBank is a switch for testing new CAFE banking. IBank=0 do not do banking, else do banking.
    IBank=1

!...TRANEFF switches on alternate scenarios  
    TRANEFF    = RTOVALUE('TRANEFF ',0)
    TRANAB32   = RTOVALUE('AB32SW  ',1)
    FRZNEFF    = 0.0
    HIGHEFF    = 0.0
    FRTEFF     = 0.0
	IRA_STIM   = 0.0

!...CAFEMEET (=1) Natural Gas Vehicle side cases
    CAFEMEET  = RTOVALUE('CAFEMEET',0)

    if(cafemeet.eq.1) frteff=3    !...expanded natural gas market, permanent natural gas incremental cost tax credit, new logistic function, 1990$
    if(cafemeet.eq.2) frteff=4    !...expanded natural gas market, temporary natural gas incremental cost tax credit, new logistic function, 1990$  

!...2022 IRA tax credits
    LEGIRA = RTOVALUE('LEGIRA  ',0)
    if(LEGIRA.gt.0)then
      IRA_STIM = 1.0
    endif	
    
!...Switch to turn off(0)/on(1) for frozen technology scenario
    if(TRANEFF.eq.1) FRZNEFF = 1.0

!...Switch to turn off(0)/on(1) for high tech/efficiency scenario
    if(TRANEFF.eq.2)then 
      HIGHEFF = 1.0
    endif
!...Switch to turn off(0)/on(1) for high efficiency scenario /fe(2) for frozen technology
    if(HIGHEFF.eq.1) FRTEFF = 1
    if(FRZNEFF.eq.1) FRTEFF = 2

    YRS = CURCALYR
    N = CURIYR

    OPEN(21,FILE='TDM_TRNOUT.TXT')

	if(.not. allocated(FEMMPG)) allocate(FEMMPG(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV))
	if(.not. allocated(FEMHP)) allocate(FEMHP(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)) 
	if(.not. allocated(FEMPRI)) allocate(FEMPRI(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV))
	if(.not. allocated(FEMWGT)) allocate(FEMWGT(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)) 
	if(.not. allocated(FEMRNG)) allocate(FEMRNG(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)) 
	if(.not. allocated(FEMTSZ)) allocate(FEMTSZ(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV))     
    IF(.not. allocated(FEMPEN)) allocate(FEMPEN(MAXGROUP,MAXCLASS,MAXTECH,BYR:LYR,MAXLDV))
	if(.not. allocated(BatPackSize)) allocate(BatPackSize(BYR:LYR,MAXCLASS,MAXGROUP,MAXLDV))
	if(.not. allocated(EV_RNG)) allocate(EV_RNG(MAXGROUP,MAXCLASS,byr:lyr,MAXLDV))	
	if(.not. allocated(FPRT)) allocate(FPRT(MAXGROUP,MAXCLASS,byr:lyr,MAXLDV))	
	if(.not. allocated(CAFESALES)) allocate(CAFESALES(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV))
	if(.not. allocated(MPGCOMP)) allocate(MPGCOMP(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV))
	if(.not. allocated(MPGADJ)) allocate(MPGADJ(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV))
	if(.not. allocated(PHEV_EVMT)) allocate(PHEV_EVMT(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV))
	if(.not. allocated(PHEVMPG_S)) allocate(PHEVMPG_S(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV))
	if(.not. allocated(PHEVMPG_D)) allocate(PHEVMPG_D(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV))
	if(.not. allocated(NAMEPLATE)) allocate(NAMEPLATE(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV))	
	if(.not. allocated(EPAMPG)) allocate(EPAMPG(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV))
	if(.not. allocated(EPAHP)) allocate(EPAHP(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV)) 
	if(.not. allocated(EPAPRI)) allocate(EPAPRI(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV))
	if(.not. allocated(EPAWGT)) allocate(EPAWGT(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV)) 
	if(.not. allocated(EPARNG)) allocate(EPARNG(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV)) 
	if(.not. allocated(EPATSZ)) allocate(EPATSZ(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV)) 
	if(.not. allocated(EPALUG)) allocate(EPALUG(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV)) 	
	if(.not. allocated(OWN_SALES)) allocate(OWN_SALES(MAXOWNER,MAXGROUP,MAXCLASS,MAXLDV,MNUMCR-2,2019:LYR))
    IF(.not. allocated(FLT_STOCK)) allocate(FLT_STOCK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXAGE,MAXHAV,MNUMYR))
    IF(.not. allocated(LDV_STOCK)) allocate(LDV_STOCK(mnumcr,maxvtyp,maxowner,maxldv,maxage,maxhav,mnumyr))
    
    if(DO_ONCE.eq.0) then
      DO_ONCE=1
      CALL READLDV
	  CALL READSTOCK
    endif
    CALL TMAC
    CALL NEWLDV
    CALL TMPGNEW
    if (yrs.ge.MINVAL(first_lidar_year)) CALL FLTHAV 	! if lidar is available, determine taxi / ride-hail fleet adoption of HAVs
    CALL TFLTVMTS
    CALL TSMOD
    CALL TMPGSTK
    CALL TCURB
    CALL TFLTMPGS
    CALL TFLTCONS
	CALL TVMT
    CALL TRANFRT(FRTEFF,0)
    CALL TRANFRT(0,1)    ! in TRANFRT.F, 1 indicates a reporting call to TFRTRPT
    CALL TMPGAG
    CALL TRAIL
    CALL TSHIP
    CALL TRANAIR
    CALL TMISC
    CALL TCONS
    CALL TINTEG
    CALL TBENCHMARK
    CALL TREPORT

!...Calculate total highway vehicle CNG demand    
!...QGFTR: total transportation natural gas demand
!...QGFTRFV: CNG central refueling
!...QGFTRPV: CNG retail purchase
!...QGLTRFV: LNG central refueling
!...QGLTRPV: LNG retail refueling

    do iregn=1,mnumcr
      QGFTR(iregn,n)   = QNGTR(iregn,n)
!...  Calculate retail CNG demand...heavy-duty truck demand populated in tranfrt.f
	  QGFTRPV(iregn,n) = QGFTRPV(iregn,n) + TQLDV(4,iregn,n) * BENNG(iregn,n)	

!...Calculate total fleet vehicle (light and heavy central refueling) NG demand
!...Add H2 consumption to NG consumption (no HMM Right now)
		QGFTRFV(iregn,n) = QGFTRFV(iregn,n) + (FLTFUELBTU(iregn,4,n) + &
                           QMTBR(1,5,IREGN,N) +       & ! transit bus   
                           QMTBR(2,5,IREGN,N) +       & ! intercity bus  
                           QMTBR(3,5,IREGN,N))        & ! school bus 
                           * BENNG(iregn,n)
      QRHTR(iregn,n)   = QRSTR(iregn,n) - max(0.0,benrs(iregn,n)*tqishipr(5,iregn,n)) - max(0.0,benrs(iregn,n)*TQRAILR(2,iregn,n))
      QRLTR(iregn,n)   = max(0.0,benrs(iregn,n)*tqishipr(5,iregn,n)) + max(0.0,benrs(iregn,n)*TQRAILR(2,iregn,n))
    enddo

!...Calc average price of cars, trucks and vehicles
    AVG_PRC_CAR(curiyr) = 0.0
    AVG_PRC_TRK(curiyr) = 0.0
    Avg_PRC_VEH(curiyr) = 0.0

    CARSALES  = 0.0
    TRKSALES  = 0.0
    CARSALES2 = 0.0
    TRKSALES2 = 0.0

    do ICL=1,MAXCLASS
      do ILDV=1,MAXLDV
        if(LDV_PRI(1,ILDV,ICL,yrs).gt.0.0) then
          Avg_Prc_Car(curiyr)=Avg_Prc_Car(curiyr)+LDV_PRI(1,ILDV,ICL,yrs)*TotalSalsc(1,ICL,ILDV,n)
          CarSales=CarSales+TotalSalsc(1,ICL,ILDV,n)
        endif
        CarSales2=CarSales2+TotalSalsc(1,ICL,ILDV,n)
        if(LDV_PRI(2,ILDV,ICL,yrs).gt.0.0) then
         Avg_Prc_Trk(curiyr)=Avg_Prc_Trk(curiyr)+LDV_PRI(2,ILDV,ICL,yrs)*TotalSalsc(2,ICL,ILDV,n)
         TrkSales=TrkSales+TotalSalsc(2,ICL,ILDV,n)
        endif
        TrkSales2=TrkSales2+TotalSalsc(2,ICL,ILDV,n)
      enddo
    enddo
    
    Avg_Prc_Veh(curiyr)=((Avg_Prc_Car(curiyr)+Avg_Prc_Trk(curiyr))/(CarSales+TrkSales))/1000.0
    Avg_Prc_Car(curiyr)=(Avg_Prc_Car(curiyr)/CarSales)/1000.0
    Avg_Prc_Trk(curiyr)=(Avg_Prc_Trk(curiyr)/TrkSales)/1000.0

    Full_Prc_Veh(curiyr)=Avg_Prc_Veh(curiyr)
    Full_Prc_Car(curiyr)=Avg_Prc_Car(curiyr)
    Full_Prc_Trk(curiyr)=Avg_Prc_Trk(curiyr)

    do iregn=1,mnumcr 
	  ! Rail CNG
      QGFTRRAIL(1,iregn,n) = BTQRAILR(3,iregn)  ! freight rail
      QGFTRRAIL(2,iregn,n) = 0.0                ! intercity rail
      QGFTRRAIL(3,iregn,n) = 0.0                ! tranist rail
      QGFTRRAIL(4,iregn,n) = 0.0                ! commuter rail
      ! Rail LNG
      QGLTRRAIL(1,iregn,n) = BTQRAILR(4,iregn)  ! freight rail
      QGLTRRAIL(2,iregn,n) = 0.0                ! intercity rail
      QGLTRRAIL(3,iregn,n) = 0.0                ! tranist rail
      QGLTRRAIL(4,iregn,n) = 0.0                ! commuter rail        
      ! Ship CNG       
      QGFTRSHIP(1,iregn,n) = BTQDSHIPR(3,iregn) ! domestic ship
      QGFTRSHIP(2,iregn,n) = BTQISHIPR(3,iregn) ! international ship
      QGFTRSHIP(3,iregn,n) = 0.0                ! recreational boat
      ! Ship LNG
      QGLTRSHIP(1,iregn,n) = BTQDSHIPR(4,iregn) ! domestic ship
      QGLTRSHIP(2,iregn,n) = BTQISHIPR(4,iregn) ! international ship
      QGLTRSHIP(3,iregn,n) = 0.0                ! recreational boat        
    enddo

    if(n.eq.lastyr.and.fcrl.eq.1) Call VReport
    
! ==========================================================================================================
! ...
! ... TRAN contains each of the following subprograms, so that all parameters
! ... defined within TRAN are available to each subprogram
! ...
! ==========================================================================================================

  end subroutine tran

! ==========================================================================================================
  subroutine VReport
  use T_
  implicit none
    integer nmap(20),ymap(20),tmap,it,ix
	real ttot
	real tshr(maxhav)
    character*9 ClsLab(MAXCLASS,maxvtyp),GrpLab(MAXGROUP),TypLab(maxvtyp)
    data ClsLab/'Mini','SubCmpt','Compact','Midsize','Large','2-Seat','SmPV CUV','LgPV CUV', &
                'Cmpt PTrk','Std PTrk','Cmpt Van','Std Van','Cmpt SUV','Std SUV','SmLT CUV','LgLT CUV'/
    data GrpLab/'Dom Car','Asa Car','Eur Car','Spt Car','CAV Car','Trk 1','Trk 2','Trk 3','Trk 4','Trk 5','CAV Trk 6'/
    data TypLab/'Cars','Trucks'/
! ... Maps for 2015 through 2030, and 2035, 2040, 2045, 2050.
    data nmap/26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,46,51,56,61/
    data ymap/2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030,2035,2040,2045,2050/
    tmap=20

!   ********************************************************************************************
!   ****************************** LIDARCOSTCALC and FLTHAV DEBUG ******************************
!	********************************************************************************************
!    write(H2UNIT, '(/,a)') 'LIDARCOSTCALC output - Lidar cost reduction over time:'
!    write(H2UNIT,'(2x,a,36f8.4)') lidar_cost(:, :)
!	write(21,*) 'lidar phase:'
!    do i=1995, 2050
!        write(21,*) i, lidar_phase(1,i), lidar_phase(2,i)
!    enddo
!
!    do i=1995, 2050
!        write(21,*) i, ' cost:', lidar_cost(1,i), lidar_cost(2,i), lidar_cost(3,i), lidar_cost(4,i)
!    enddo
!	
!	write(21, *) 'Low and High speed production:'
!	do i=1995, 2050
!        write(21,*) i, ': ', cumul_lidar_prod(1,i),cumul_lidar_prod(2,i)
!    enddo
!
!	write(21, *) 'HAV System Cost:'
!    do i=1995, 2050
!        write(21, '(i4, 4F8.0)') i, hav_sys_cost(:,i)
!    enddo
!	
!   flt_hav_shares(mnumcr, maxvtyp, MAXCLASS, maxldv, BYR:LYR,,maxhav)
!   taxi_sales(mnumcr,maxvtyp,MAXCLASS,maxldv,BYR:LYR,maxhav)
!	write (21,*) 'Fleet HAV adoption in taxis for all CD, type, class, and ILDV:'
!	write (21,*) 'Total national sales of HAV levels, by year'
	do i=2020, 2050
		! National sales by HAV level (all CD, type, class, ILDV summed)
		do ihav=1, maxhav
			tshr(ihav) = sum(taxi_sales(mnumcr,:,:,:,i,ihav))				  ! National sales by HAV level
		enddo
!		write (21,'(i4, 2x, 4f8.0)') i, tshr(:)
	enddo
!	write (21,*) 'Total national shares of HAV levels, by year'
	do i=2020, 2050
		ttot = sum(taxi_sales(mnumcr,:,:,:,i,:))							  ! National sales, all HAV levels summed
	!...HAV national sales share
		do ihav=1, maxhav
			tshr(ihav) = sum(taxi_sales(mnumcr,:,:,:,i,ihav))/ttot				  ! National sales share by HAV level
		enddo
!		write (21,'(i4, 2x, 4f8.4)') i, tshr(:)
	enddo
!	
!	write (21,*) 'Fleet HAV adoption for cars (IVTYP=1), class 4, gasoline (ILDV=1):'
!	write (21,*) 'Shares of HAV levels, by year'
!	do iregn = 1, mnumcr-2
!		write (21,*) '>> CD ', iregn
!		write (21,*) '    shares'
!		do i=1995, 2050
!			write (21,'(i4, 5f10.4)') i, flt_hav_shares(iregn,1,4,1,i,:), sum(flt_hav_shares(iregn,1,4,1,i,:))
!		enddo
!		write (21,*) '    sales'
!		do i=1995, 2050
!			write (21,'(i4, 5f10.0)') i, taxi_sales(iregn,1,4,1,i,:), sum(taxi_sales(iregn,1,4,1,i,:))
!		enddo
!	enddo
!	
	write (21,*) 'Fleet HAV total VMT for all vtypes and ildvs:'
	do i=1995, 2050
	    write (21,'(i4, 4f13.0, 4f13.0, 4f13.0, 4f13.0)') i, sum(FLTVMTHAV(:,4,:,1,i)), sum(FLTVMTHAV(:,4,:,2,i)), sum(FLTVMTHAV(:,4,:,3,i)), sum(FLTVMTHAV(:,4,:,4,i))				!FLTVMTHAV(IVTYP,ifleet,ILDV,ihav,yrs)
	enddo
	
	write (21,*) 'Fleet HAV total stock for cars and trucks, taxis (fleet=4), all fuels, all ages (1:maxage) each HAV level:'
	do i=1995, 2050
		write (21,'(i4, 4f8.0, 4f8.0, 4f8.0, 4f8.0)') i, sum(Flt_Stock(:,:,4,:,:,1,i-1989)), sum(Flt_Stock(:,:,4,:,:,2,i-1989)), sum(Flt_Stock(:,:,4,:,:,3,i-1989)), sum(Flt_Stock(:,:,4,:,:,4,i-1989))
	enddo
	
	write (21,*) 'Fleet HAV total sales for cars (IVTYP=1), taxis (fleet=4), gasoline (ILDV=1), all AV levels (0-3, 4a, 4b, 5):'
	write (21,'(i4, 4f8.0)') i, sum(FLTECHSAL(mnumcr,1,4,:,1,:))

!   ********************************************************************************************

!    write(H2UNIT,'(/,a)') 'Class Shares by Manufacturer Group (Class_Share)'
!    write(H2UNIT,'(11x,36i8)') (ymap(ix),ix=1,tmap)
!    do IGP=1,MAXGROUP
!      it=GrpMap(IGP)
!      write(H2UNIT,'(a)') GrpLab(IGP)
!        do ICL=1,MAXCLASS
!          write(H2UNIT,'(2x,a,36f8.4)') ClsLab(ICL,it),(class_share(mnumcr,ICL,IGP,ymap(ix)),ix=1,tmap)
!        end do
!    end do

!    write(H2UNIT,'(/,a)') 'Group Shares Within Types by Vehicle Class (Grpclshr)'
!    write(H2UNIT,'(11x,36i8)') (ymap(ix),ix=1,tmap)
!    do IGP=1,MAXGROUP
!      it=GrpMap(IGP)
!      write(H2UNIT,'(a)') GrpLab(IGP)
!      do ICL=1,MAXCLASS
!        write(H2UNIT,'(2x,a,36f8.4)') ClsLab(ICL,it),(Grpclshr(IGP,ICL,nmap(ix)),ix=1,tmap)
!      end do
!   end do

!   write(H2UNIT,'(/,a)') 'Group Shares Within Types Over All Vehicle Classes (GrpShare)'
!   write(H2UNIT,'(11x,36i8)') (ymap(ix),ix=1,tmap)
!   do IGP=1,MAXGROUP
!     it=GrpMap(IGP)
!     write(H2UNIT,'(2x,a,36f8.4)') GrpLab(IGP),(GrpShare(mnumcr,IGP,nmap(ix)),ix=1,tmap)
!   end do

!   write(H2UNIT,'(/,a)') 'projection Sales of Non-Fleet (Personal) Vehicles by Manufacturer Group and Vehicle Class (mfr_sales)'
!   write(H2UNIT,'(11x,36i8)') (ymap(ix),ix=1,tmap)
!   do IGP=1,MAXGROUP
!     it=GrpMap(IGP)
!     write(H2UNIT,'(a)') GrpLab(IGP)
!     do ICL=1,MAXCLASS
!       write(H2UNIT,'(2x,a,36f8.1)') ClsLab(ICL,it),(mfr_sales(mnumcr,IGP,ICL,nmap(ix))*1000.0,ix=1,tmap)
!     end do
!   end do

   return
   end subroutine VReport

! ==========================================================================================================
! ... Subroutine READLDV reads the spreadsheet input file TRNLDV.XML
! ==========================================================================================================
   SUBROUTINE READLDV
   USE T_
   IMPLICIT NONE

!...Declare local parameters

LOGICAL       NEW/.FALSE./
CHARACTER*18  INAME,JNAME
INTEGER       WKUNIT,IFL,IR,IYEARS
INTEGER*2     INPUT_ERROR

INTEGER*2     TECHGROUP(MAXTECH*MAXVTYP)           ! vehicle group classfication: car(1), light truck(2)
CHARACTER*30  TECHLABIN(MAXTECH*MAXVTYP)           ! technology label
CHARACTER*15  TECHSYSTEM(MAXTECH*MAXVTYP)          ! vehicle system affected by technology
REAL          TECHFE(MAXTECH*MAXVTYP)              ! incremental change in fuel economy (percent)
REAL          TECHCOSTA(MAXTECH*MAXVTYP)           ! absolute incremental change in cost ($)
REAL          TECHCOSTR(MAXTECH*MAXVTYP)           ! relative incremental change in cost ($/lb)
REAL          TECHWGTA(MAXTECH*MAXVTYP)            ! absolute incremental change in weight (lb)
REAL          TECHWGTR(MAXTECH*MAXVTYP)            ! relative incremental change in weight (lb/base vehicle lb)
INTEGER*2     TECHFYR(MAXTECH*MAXVTYP)             ! first year of technology introduction
REAL          TECHHP(MAXTECH*MAXVTYP)              ! incremental change in horsepower (percent)
REAL          TECHLEARN(MAXTECH*MAXVTYP)           ! coefficient for technology learning curve
REAL          TECHLRN1(MAXTECH*MAXVTYP)            ! coefficient for learning curve trigger for most technologies
REAL          TECHLRN2(MAXTECH*MAXVTYP)            ! coefficient for learning curve trigger for mild hybrids, micro hybrids, tires II
CHARACTER*3   TECHAPPLY(MAXTECH*MAXVTYP,MAXLDV)    ! fueling type applicability indicator
CHARACTER*15  VEHGROUPLABEL(MAXGROUP*MAXCLASS)     ! vehicle group label
INTEGER*2     VEHGROUP(MAXGROUP*MAXCLASS)          ! vehicle group number
CHARACTER*30  VEHCLASSLABEL(MAXGROUP*MAXCLASS)     ! vehicle class label
INTEGER*2     VEHCLASS(MAXGROUP*MAXCLASS)          ! vehicle class number

REAL          VEHPERFVAL(MAXGROUP*MAXCLASS)        ! vehicle class base performance value
REAL          VEHPERFFAC(MAXGROUP*MAXCLASS)        ! vehicle class base performance factor
REAL          VEHPERFCAP(MAXGROUP*MAXCLASS)        ! vehicle class performance cap
REAL          VEHUSEDCAP(MAXGROUP*MAXCLASS)        ! fraction of vehicle class performance cap used since 1990
INTEGER*2     MKTGROUP(MAXGROUP*MAXTECH)                  ! vehicle group number
INTEGER*2     MKTTECH(MAXGROUP*MAXTECH)                   ! technology number
REAL          TECHMKTSHARE(MAXGROUP*MAXTECH,MAXCLASS*2)   ! technology base and maximum market shares
INTEGER*2     ENGNOTETYPE(MAXNOTE)                 ! engineering note type
INTEGER*2     ENGNOTETECH(MAXNOTE,10)              ! engineering note technology ID #1-10
INTEGER*2     ENGNOTEYEAR(MAXNOTE)                 ! engineering note year
REAL          ENGNOTEPCT(MAXNOTE)                  ! engineering note percent affected

REAL          TFCOEFNRG(4,MNUMYR)                  	! 4 coefficients for EISA07 lt footprint CAFE
REAL          CFCOEFNRG(4,MNUMYR)                  	! 4 coefficients for EISA07 car footprint CAFE
!REAL          TFCOEFGHG(4,MNUMYR)                  ! 4 coefficients for GHG lt footprint CAFE
!REAL          CFCOEFGHG(4,MNUMYR)                  ! 4 coefficients for GHG car footprint CAFE
REAL          TFCOEFCAFE(8,MNUMYR)                  ! 8 coefficients for CAFE lt footprint		
REAL          CFCOEFCAFE(4,MNUMYR)                  ! 4 coefficients for CAFE car footprint		
REAL          TFCOEFEPA(8,MNUMYR)                  	! 8 coefficients for EPA GHG lt footprint	
REAL          CFCOEFEPA(4,MNUMYR)                  	! 4 coefficients for EPA GHG car footprint	
INTEGER       PRNTFET                              	! print fem results
INTEGER       PRNTECHT                             	! print market share results
INTEGER       PRNTDGT                              	! print diagnostic results
INTEGER*2     FRSTYEARX(MAXTECH,MAXGROUP)          	! Temp placeholder for first year of technology introduction
INTEGER*2     CAFEYEAR(MNUMYR)                     	! CAFE year
REAL          CAFE_STD(MAXVTYP,6:MNUMYR)           	! CAFE standards

!...Battery and hybrid systems
!...reference case 
REAL          NIMHCOST(MNUMYR)                    	! Nickel metal hydride battery cost ($/kWh)
REAL          LIONCOST(MNUMYR)                    	! Lithium-ion battery cost ($/kWh)
REAL          LIONCOST_PHEV(MNUMYR)                 ! Lithium-ion battery cost ($/kWh) 
REAL          HEVSYSCST(MAXCLASS,MNUMYR,MAXVTYP)  	! HEV non-battery system cost       
REAL          PHEV_DOD_FAC(MNUMYR)                	! depth of discharge percentage for PHEV battery
REAL          EV_DOD_FAC(MNUMYR)                  	! depth of discharge percentage for EV battery
REAL          PHEV20SYSCST(MAXCLASS,MNUMYR,MAXVTYP) 	! PHEV non-battery system cost
REAL          PHEV50SYSCST(MAXCLASS,MNUMYR,MAXVTYP) 	! PHEV non-battery system cost
REAL          EVSYSCST(MAXCLASS,MNUMYR,MAXVTYP)   	! EV non-batterysystem cost
REAL          PACKA(MAXLDV)                       	! Cumulative Li-ion battery pack initial cost parameter
REAL          PACKLR(MAXLDV)                      	! Cumulative Li-ion battery pack learning rate
REAL          MATA(MAXLDV)                        	! Cumulative Li-ion materials parameter
REAL          MATLR(MAXLDV)                       	! Cumulative Li-ion materials learning rate
REAL          CELLMCST(MNUMYR)                     ! methanol fuel cell cost learning curve
REAL          CELLHCST(MNUMYR)                     ! hydrogen fuel cell cost learning curve

integer       ix
 
! debug output of trnldvx input: each variable written to unit xmlout, (trnldvx.txt in this case) when xmlout<>0
     common/nemswk1/xmlout
     integer xmlout
     call unitunopened(300,999,xmlout)  ! get unused unit number
     open(xmlout,file='TDM_trnldvx.txt',status='unknown')
     rewind xmlout
! end of debug output set up

!...Store data ranges from xml file
      
      INAME = 'TRNH2OUT'
      H2UNIT = FILE_MGR('O',INAME,.TRUE.)
      INAME = 'TRNLDVX'
      WKUNIT = FILE_MGR('O',INAME,NEW)   !open trnldvx.xlsx input file
      CALL ReadRngXLSX(WKUNIT,'trnldv')   !read range names & coerresponding data from worksheet "trnldv"
      WKUNIT = FILE_MGR('C',INAME,NEW)   !close .xlsx input file

!...*******************************************************************************
!...* Light Duty Vehicle Module input variables                                   *
!...*******************************************************************************
!...* Fuel Economy Model                                                          *
!...*******************************************************************************

!...Defer input of technology matrix, learning curve parameters, base and maximum technology
!...market penetration, and engineering note data until end of input to determine if data should
!...be from "best estimate" (TRNINPUT) or "optimistic estimate" (copy from HITECHIN) input files.  This
!...avoids the need to maintain redundant input and validation routines.  Regardless of technology
!...input, all non source-sensitive inputs from TRNINPUT must be read in before the high
!...efficiency inputs are read in.

!...Read in, validate, and store for global reference the LDV/LDT base year attributes
      CALL GETRNGC('VEHGROUPLABEL   ',VEHGROUPLABEL,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Group Label
      CALL GETRNGI('VEHGROUP        ',VEHGROUP     ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Group Number
      CALL GETRNGC('VEHCLASSLABEL   ',VEHCLASSLABEL,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Label
      CALL GETRNGI('VEHCLASS        ',VEHCLASS     ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Number
      CALL GETRNGR('VEHPERFVAL      ',VEHPERFVAL   ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Base Performance Value
      CALL GETRNGR('VEHPERFFAC      ',VEHPERFFAC   ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Base Performance Factor
      CALL GETRNGR('VEHPERFCAP      ',VEHPERFCAP   ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Performance Cap
      CALL GETRNGR('VEHUSEDCAP      ',VEHUSEDCAP   ,1,MAXGROUP*MAXCLASS,1)    ! Fraction of Vehicle Class Perf Cap Used Since 1990

      DO IGP=1,MAXGROUP
        DO ICL=1,MAXCLASS
          K = ((IGP-1)*MAXCLASS) + ICL
          IF (VEHGROUP(K) .NE. IGP) STOP 110
          IF (ICL .EQ. 1) GROUPLABEL(IGP) = VEHGROUPLABEL(K)
          IF (VEHCLASS(K) .NE. ICL) STOP 111
          CLASSLABEL(ICL,IGP)        = VEHCLASSLABEL(K) ! jma hp cap?
          VALUEPERF(ICL,IGP)         = VEHPERFVAL(K)
          PERFFACT(ICL,IGP)          = VEHPERFFAC(K)
          PERFCAP(ICL,IGP)           = VEHPERFCAP(K)
          USEDCAP(ICL,IGP)           = VEHUSEDCAP(K)
        ENDDO;
      ENDDO;

! ... Read in and store for global reference various miscellaneous parameters

      CALL GETRNGI('PBACKT          ',PAYBACK ,1,1,1)    !Payback period 
      CALL GETRNGR('DRATET          ',DISCOUNT,1,1,1)    !Discount rate 

      DISCOUNT = DISCOUNT * 0.01

      CALL GETRNGR('FINET           ',REG_COST,1,1,1)     ! CAFE fine

! ... Read in and store for global reference CAFE standards and actual CAFE acheived
   
      CALL GETRNGI('CAFEYEAR        ',CAFEYEAR(iy:IJUMPYR),num_to_read,1,1)
      CALL GETRNGR('CAFE_STD        ',CAFE_STD(1:maxvtyp,iy:IJUMPYR),MAXVTYP,num_to_read,1)
      CALL GETRNGR('REF_MPG         ',REF_MPG(1:maxvtyp,iy:ijumpyr),maxvtyp,num_to_read,1)
 
! ... Copy the two vehicle types that were input to all the vehicle groups.

      DO I=IY,IJUMPYR
        IYR = CAFEYEAR(I) 
        do IGP=1,MAXGROUP
          ix=GrpMap(IGP)
          IF(IYR .GE. BYR .AND. IYR .LE. LYR) THEN
            CAFE_STAND(IGP,IYR) = CAFE_STD(ix,I)
          ENDIF
        end do
      ENDDO

!...Read in and store the vehicle footprints and the coefficients for the footprint-based CAFE standards
    CALL GETRNGR('TFCOEFNRG       ',TFCOEFNRG(1:4,19:IJUMPYR),4,IJUMPYR-18,1)
    CALL GETRNGR('CFCOEFNRG       ',CFCOEFNRG(1:4,19:IJUMPYR),4,IJUMPYR-18,1)
    CALL GETRNGR('TFCOEFCAFE      ',TFCOEFCAFE(1:8,23:IJUMPYR),8,IJUMPYR-22,1)		
    CALL GETRNGR('CFCOEFCAFE      ',CFCOEFCAFE(1:4,23:IJUMPYR),4,IJUMPYR-22,1)		
    CALL GETRNGR('TFCOEFEPA       ',TFCOEFEPA(1:8,23:IJUMPYR),8,IJUMPYR-22,1)		
    CALL GETRNGR('CFCOEFEPA       ',CFCOEFEPA(1:4,23:IJUMPYR),4,IJUMPYR-22,1)		
	CALL GETRNGR('CAFEPEFMULT     ',CAFEPEFMULT(1:maxldv,22:IJUMPYR),maxldv,IJUMPYR-21,1)
	CALL GETRNGR('EPAALTMULT      ',EPAALTMULT(1:maxldv,22:IJUMPYR),maxldv,IJUMPYR-21,1)
	CALL GETRNGR('AC_CO2_OFFSET   ',AC_CO2_OFFSET(1:maxgroup,23:IJUMPYR),maxgroup,IJUMPYR-22,1)
	CALL GETRNGI('RUN_EPA         ',RUN_EPA,1,1,1)
	CALL GETRNGI('CAFEMY27_SWITCH ',CAFEMY27_SWITCH,1,1,1)
	CALL GETRNGR('AC_OC_CREDIT    ',AC_OC_CREDIT(1:MAXGROUP,iy:ijumpyr),MAXGROUP,num_to_read,1)

!...Scedes switch overrides trnldvx setting for MY27+ standards
    if (TRANEFF.eq.3) CAFEMY27_SWITCH = 0

!...Freeze standards at MY2026 if CAFEMY27_SWITCH turned off
!   Change ac/oc credits since maximum no longer phases out
    if (CAFEMY27_SWITCH.eq.0) then
      CALL GETRNGR('AC_OC_CREDIT_NC ',AC_OC_CREDIT(1:MAXGROUP,iy:ijumpyr),MAXGROUP,num_to_read,1)
      do igp=1,maxgroup
        AC_CO2_OFFSET(igp,2027-1989:MNUMYR) = AC_CO2_OFFSET(igp,2026-1989)
      enddo
      do i=1,8
        if (i.le.4) then
          TFCOEFNRG(i,2027-1989:MNUMYR) = TFCOEFNRG(i,2026-1989)
          CFCOEFNRG(i,2027-1989:MNUMYR) = CFCOEFNRG(i,2026-1989)
          CFCOEFCAFE(i,2027-1989:MNUMYR) = CFCOEFCAFE(i,2026-1989)
          CFCOEFEPA(i,2027-1989:MNUMYR) = CFCOEFEPA(i,2026-1989)
        endif
        TFCOEFCAFE(i,2027-1989:MNUMYR) = TFCOEFCAFE(i,2026-1989)
        TFCOEFEPA(i,2027-1989:MNUMYR) = TFCOEFEPA(i,2026-1989)
      enddo
!      do ildv = 1,maxldv
!        CAFEPEFMULT(ildv,2027-1989:mnumyr) = CAFEPEFMULT(ildv,2026-1989)
!      enddo
    endif


!...Copy foot print coefficients (EISA) into separate coefficient variables and FPRNT into groups by classes
    do i=19,ijumpyr ! 2009+
      CFCoefA(i)=CFCOEFNRG(1,i)
      CFCoefB(i)=CFCOEFNRG(2,i)
      CFCoefC(i)=CFCOEFNRG(3,i)
      CFCoefD(i)=CFCOEFNRG(4,i)
      TFCoefA(i)=TFCOEFNRG(1,i)
      TFCoefB(i)=TFCOEFNRG(2,i)
      TFCoefC(i)=TFCOEFNRG(3,i)
      TFCoefD(i)=TFCOEFNRG(4,i)
    enddo

!...Copy foot print coefficients into separate coefficient variables for GHG CAFE standards
    do i=23,ijumpyr ! 2012+ 
!	  NHTSA CAFE
      CFCoefA2(i)=CFCOEFCAFE(1,i)
      CFCoefB2(i)=CFCOEFCAFE(2,i)
      CFCoefC2(i)=CFCOEFCAFE(3,i)
      CFCoefD2(i)=CFCOEFCAFE(4,i)
      TFCoefA2(i)=TFCOEFCAFE(1,i)
      TFCoefB2(i)=TFCOEFCAFE(2,i)
      TFCoefC2(i)=TFCOEFCAFE(3,i)
      TFCoefD2(i)=TFCOEFCAFE(4,i)
      TFCoefE2(i)=TFCOEFCAFE(5,i)	
      TFCoefF2(i)=TFCOEFCAFE(6,i)	
      TFCoefG2(i)=TFCOEFCAFE(7,i)	
      TFCoefH2(i)=TFCOEFCAFE(8,i)	
!	  EPA GHG
      CFCoefEPAA2(i)=CFCOEFEPA(1,i)	
      CFCoefEPAB2(i)=CFCOEFEPA(2,i)	
      CFCoefEPAC2(i)=CFCOEFEPA(3,i)	
      CFCoefEPAD2(i)=CFCOEFEPA(4,i)	
      TFCoefEPAA2(i)=TFCOEFEPA(1,i)	
      TFCoefEPAB2(i)=TFCOEFEPA(2,i)	
      TFCoefEPAC2(i)=TFCOEFEPA(3,i)	
      TFCoefEPAD2(i)=TFCOEFEPA(4,i)	
      TFCoefEPAE2(i)=TFCOEFEPA(5,i)	
      TFCoefEPAF2(i)=TFCOEFEPA(6,i)	
      TFCoefEPAG2(i)=TFCOEFEPA(7,i)	
      TFCoefEPAH2(i)=TFCOEFEPA(8,i)	
    enddo

! ... Read in and store for global reference basic Advanced Technology Vehicle parameters
      CALL GETRNGR('COEF_A          ',COEF_A,MAXCLASS,MAXGROUP,1)     ! elasticity for time by veh group
      CALL GETRNGR('COEF_B          ',COEF_B,MAXCLASS,MAXGROUP,1)     ! elasticity for fuel price by veh group
      CALL GETRNGR('COEF_C          ',COEF_C,MAXCLASS,MAXGROUP,1)     ! elasticity for income by veh group
      CALL GETRNGR('COEF_P          ',COEF_P,MAXCLASS,MAXGROUP,1)     ! elasticity for veh price by veh group
	  
	  CALL GETRNGR('AFVADJHP        ',AFVADJHP,MAXLDV,MAXVTYP,1) 
	  CALL GETRNGR('AFVADJFE        ',AFVADJFE,MAXLDV,MAXVTYP,1)     
	  CALL GETRNGR('AFVADJWT        ',AFVADJWT,MAXLDV,MAXVTYP,1)
	  CALL GETRNGR('AFVADJPR        ',AFVADJPR,MAXLDV,MAXVTYP,1)

! ... The following parameters indicate the availablity of advanced technology vehicles by group and class, and the number of sales per nameplate
!     (required to estimate the introduction of new nameplates in the projection)
      CALL GETRNGI('GRPFLAG         ',GRPFLAG(1:MAXLDV,1:MAXCLASS,1:MAXGROUP),MAXLDV,MAXCLASS,MAXGROUP)
      CALL GETRNGR('SALES_PER_MODEL ',SALES_PER_MODEL,MAXCLASS,MAXGROUP,1)

!   If not applying MY2027-My2032 CAFE/GHG standards, eliminate manufacturer-announced BEV offerings and allow model
!   to determine new nameplate introductions endogenously with sales_per_model
    if (CAFEMY27_SWITCH.eq.0) then
      CALL GETRNGR('SALES_PER_MDL_NC',SALES_PER_MODEL,MAXCLASS,MAXGROUP,1)
      do igp=1,maxgroup
        do icl=1,maxclass
          do ildv=1,maxldv
            if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then
              if(GRPFLAG(ildv,icl,igp).ne.epalyr+1) GRPFLAG(ildv,icl,igp) = 0
!           In a case without MY27+ regulations, which require BEVs for compliance, add HEVs wherever there are PHEVs introduced
            elseif(ildv.eq.5.or.ildv.eq.6) then
              if(GRPFLAG(16,icl,igp).eq.0.and.GRPFLAG(ildv,icl,igp).gt.0) GRPFLAG(16,icl,igp) = GRPFLAG(ildv,icl,igp)
            endif
          enddo
        enddo
      enddo
    endif

! ... Read in and store for global reference learning cost curves for batteries and fuel cells
      CALL GETRNGR('NIMHCOST        ',NIMHCOST(iy:IJUMPYR),Num_to_Read,1,1) 
      CALL GETRNGR('LIONCOST        ',LIONCOST(iy:IJUMPYR),Num_to_read,1,1)							!generic li-ion cost curve 
      CALL GETRNGR('LIONCOST_PHEV   ',LIONCOST_PHEV(iy:IJUMPYR),Num_to_read,1,1)					!generic li-ion cost curve 
      CALL GETRNGR('PHEV_DOD_FAC    ',PHEV_DOD_FAC(iy:IJUMPYR),Num_to_read,1,1)
	  CALL GETRNGR('PCTPHEV20       ',PCTPHEV20(iy:IJUMPYR),Num_to_read,1,1)
	  CALL GETRNGR('PCTPHEV50       ',PCTPHEV50(iy:IJUMPYR),Num_to_read,1,1)	  
      CALL GETRNGR('EV_DOD_FAC      ',EV_DOD_FAC(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('CELLMCST        ',CELLMCST(iy:IJUMPYR),Num_to_Read,1,1) 
      CALL GETRNGR('CELLHCST        ',CELLHCST(iy:IJUMPYR),Num_to_Read,1,1) 
      CALL GETRNGR('HEVSYSCST       ',HEVSYSCST(1:MAXCLASS,iy:IJUMPYR,1:maxvtyp),MAXCLASS,Num_to_Read,MAXVTYP)       
      CALL GETRNGR('PHEV20SYSCST    ',PHEV20SYSCST(1:MAXCLASS,iy:IJUMPYR,1:maxvtyp),MAXCLASS,Num_to_Read,MAXVTYP) 
      CALL GETRNGR('PHEV50SYSCST    ',PHEV50SYSCST(1:MAXCLASS,iy:IJUMPYR,1:maxvtyp),MAXCLASS,Num_to_Read,MAXVTYP) 
      CALL GETRNGR('EVSYSCST        ',EVSYSCST(1:MAXCLASS,iy:IJUMPYR,1:maxvtyp),MAXCLASS,Num_to_Read,MAXVTYP) 
	  CALL GETRNGI('FIRST_BAT_YR    ',FIRST_BAT_YR,1,1,1)			! first battery price projection year
      CALL GETRNGR('PACKA           ',PACKA,1,MAXLDV,1)				! Li-ion battery pack initial cost of production
      CALL GETRNGR('PACKLR          ',PACKLR,1,MAXLDV,1)			! Li-ion battery pack learning rate
      CALL GETRNGR('MATA            ',MATA,1,MAXLDV,1)				! Li-ion materials input cost or production
      CALL GETRNGR('MATLR           ',MATLR,1,MAXLDV,1)				! Li-ion materials learning rate
	  CALL GETRNGR('MAT_MARKUP      ',MAT_MARKUP(33:ijumpyr),ijumpyr-32,1,1)
	  CALL GETRNGR('LIONkWh_perLb   ',LIONkWh_perLb,maxclass,maxgroup,maxldv)		! Battery sizing factor (kWh) based on equivalent non-HEV gas vehicle weight
	  CALL GETRNGR('LION_LB_perkWh  ',LION_LB_perkWh,1,MAXLDV,1)	! Lithium-ion weight (lbs) per kWh battery capacity
	  CALL GETRNGR('EV_range_b      ',EV_range_b,1,MAXLDV,1)		! Range constant based on EV battery size
	  CALL GETRNGR('EV_range_m      ',EV_range_m,1,MAXLDV,1)		! Range slope based on EV battery size

!     fill pre-2023 MAT_MARKUP with 1.0	  
      do iyr = first_read_year,2021
		mat_markup(iyr-1989) = 1.0
	  enddo
	  
      do iyr = first_read_year,IJUMPCALYR
        FuelCell_D_kW(IYR,13)   = CELLMCST(IYR-1989)
        FuelCell_D_kW(IYR,14)   = CELLHCST(IYR-1989)
        NiMH_Cost(iyr)        = NIMHCOST(IYR-1989) / MC_JPGDP(11) * MC_JPGDP(1)      ! convert from 2000$ to 1990$
        PHEV_DOD(iyr)         = PHEV_DOD_FAC(IYR-1989)
        EV_DOD(iyr)           = EV_DOD_FAC(IYR-1989)
        do ILDV=1,maxldv
          Li_ion_Cost(ILDV,iyr)  =  LIONCOST(IYR-1989) / MC_JPGDP(31) * MC_JPGDP(1)  ! convert from 2020$ to 1990$
		  if (ILDV.eq.5.OR.ILDV.eq.6.OR.ILDV.eq.16) Li_ion_Cost(ILDV,iyr)  =  LIONCOST_PHEV(IYR-1989) / MC_JPGDP(31) * MC_JPGDP(1)  ! convert from 2020$ to 1990$
        enddo
		
        do ICL=1,MAXCLASS
          do IVTYP=1,maxvtyp
		    ElecNonBattCst(icl,iyr,ivtyp,4)  = EVSYSCST(ICL,IYR-1989,IVTYP) / MC_JPGDP(11) * MC_JPGDP(1)     	! convert from 2000$ to 1990$
			ElecNonBattCst(icl,iyr,ivtyp,5)  = PHEV20SYSCST(ICL,IYR-1989,IVTYP) / MC_JPGDP(11) * MC_JPGDP(1)    ! convert from 2000$ to 1990$
			ElecNonBattCst(icl,iyr,ivtyp,6)  = PHEV50SYSCST(ICL,IYR-1989,IVTYP) / MC_JPGDP(11) * MC_JPGDP(1)    ! convert from 2000$ to 1990$
		    ElecNonBattCst(icl,iyr,ivtyp,7)  = EVSYSCST(ICL,IYR-1989,IVTYP) / MC_JPGDP(11) * MC_JPGDP(1)     	! convert from 2000$ to 1990$
		    ElecNonBattCst(icl,iyr,ivtyp,15) = EVSYSCST(ICL,IYR-1989,IVTYP) / MC_JPGDP(11) * MC_JPGDP(1)     	! convert from 2000$ to 1990$
			ElecNonBattCst(icl,iyr,ivtyp,16) = HEVSYSCST(ICL,IYR-1989,IVTYP) / MC_JPGDP(11) * MC_JPGDP(1)     	! convert from 2000$ to 1990$
          enddo
        enddo  
      enddo

!....Calculate Li-ion cost learning curve parameters
      do ILDV=1,maxldv
        if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) then
          pack_a(ILDV) = packa(ILDV) / MC_JPGDP(31) * MC_JPGDP(1)	  	! convert from 2020$ to 1990$
          mat_a(ILDV) = mata(ILDV) / MC_JPGDP(31) * MC_JPGDP(1)	  		! convert from 2020$ to 1990$
          pack_b(ILDV) = -LOG(1.0-packlr(ILDV)) / LOG(2.0)
          mat_b(ILDV) = -LOG(1.0-matlr(ILDV)) / LOG(2.0)
        endif
      enddo

! ... Read and load variables for plug-in hybrids.
	  CALL GETRNGR('STATE_CRED      ',STATE_CRED(1:MNUMCR-2,2023:LYR,1:3),MNUMCR-2,LYR-2023+1,3)
	  CALL GETRNGR('IRA_BAT_SHR     ',IRA_BAT_SHR(1:2,irayr:lyr,1:2),2,lyr-irayr+1,2)
	  CALL GETRNGR('IRA_VEH_SHR     ',IRA_VEH_SHR(1:2,irayr:lyr,1:2),2,lyr-irayr+1,2)
	  CALL GETRNGR('CSRATIO         ',CSRATIO(1:maxclass,1:maxgroup,1:phevtype),maxclass,maxgroup,phevtype)

! ... Read and load nested multinomial logit model coefficients
	  CALL GETRNGR('NMLMCOCAR       ',NMLMCOCAR(1:MAXNMLM,1:MAXCLASS,1:CARGRP),MAXNMLM,MAXCLASS,CARGRP) !consumer choice model coefficients
	  CALL GETRNGR('NMLMCOTRK       ',NMLMCOTRK(1:MAXNMLM,1:MAXCLASS,1:LTKGRP),MAXNMLM,MAXCLASS,LTKGRP) !consumer choice model coefficients
	  CALL GETRNGR('ATVCOCAR1       ',ATVCOCAR1(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS) 
	  CALL GETRNGR('ATVCOCAR2       ',ATVCOCAR2(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS)
	  CALL GETRNGR('ATVCOCAR3       ',ATVCOCAR3(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS)
	  CALL GETRNGR('ATVCOCAR4       ',ATVCOCAR4(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS)
	  CALL GETRNGR('ATVCOCAR5       ',ATVCOCAR5(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS)
	  CALL GETRNGR('ATVCOTRK1       ',ATVCOTRK1(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS)
	  CALL GETRNGR('ATVCOTRK2       ',ATVCOTRK2(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS)
	  CALL GETRNGR('ATVCOTRK3       ',ATVCOTRK3(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS)
	  CALL GETRNGR('ATVCOTRK4       ',ATVCOTRK4(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS)
	  CALL GETRNGR('ATVCOTRK5       ',ATVCOTRK5(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS)
	  CALL GETRNGR('ATVCOTRK6       ',ATVCOTRK6(1:MAXLDV,1:MNUMCR-2,1:MAXCLASS),MAXLDV,MNUMCR-2,MAXCLASS)
	  CALL GETRNGR('ATVCOEF_CALIB   ',ATVCOEF_CALIB(1:MAXLDV,1:MAXVTYP),MAXLDV,MAXVTYP,1)
	  
! ... Read in and store for global reference various Advanced Technology Vehicle parameters
! ... read initial light duty vehicle attributes for consumer choice model

      CALL GETRNGR('INITSTA         ',INITSTA(1:maxfuel,iy:IJUMPYR,1:mnumcr-2), maxfuel,Num_to_Read,MNUMCR-2)  ! refueling stations by fuel type and reg
      CALL GETRNGR('STA_RAT         ',STA_RAT, maxfuel,1,1)                 ! ratio of fueling stations to vehicle stock
      CALL GETRNGI('CHR_STR_YR      ',CHR_STR_YR,1,1,1)                     ! first year charger data
      CALL GETRNGI('CHR_LST_YR      ',CHR_LST_YR,1,1,1)                     ! last year charger data
      CALL GETRNGR('PRT_CNT         ',PRT_CNT(1:MAXCHRG,CHR_STR_YR:CHR_LST_YR, 1:MNUMCR-2), MAXCHRG,CHR_LST_YR-CHR_STR_YR+1,MNUMCR-2)
      CALL GETRNGR('PRT_CNT_nc      ',PRT_CNT_nc(1:MAXCHRG,CHR_STR_YR:CHR_LST_YR, 1:MNUMCR-2), MAXCHRG,CHR_LST_YR-CHR_STR_YR+1,MNUMCR-2)
      CALL GETRNGR('PRT_RT          ',PRT_RT,  MAXCHRG, 1, 1)               ! ev time to refuel by type
      CALL GETRNGR('CHGCSTMULT      ',CHGCSTMULT,  MAXCHRG, 1, 1)           ! Markup on comm'l electricity cost (represents what folks actually pay to charge)
      CALL GETRNGR('CHG_DIST        ',CHG_DIST(1:mnumcr-2,1:MAXCHRG,2023-1989),  MNUMCR-2, MAXCHRG, 1)             ! Distribution of charging by location/speed
      CALL GETRNGR('PRT_VAR         ',PRT_VAR, MAXCHRG, 5, MNUMCR-2)     !ev charger s curve variables
      CALL GETRNGR('MAINTGRP        ',MAINTGRP(1:MAXLDV,1:MAXCLASS,1:maxvtyp),maxldv,maxclass,maxvtyp)
      
      if (CAFEMY27_SWITCH.eq.0) PRT_CNT = PRT_CNT_nc
      
      do iregn=1,mnumcr-2
        do ichrg=1,maxchrg
          chg_dist(iregn,ichrg,1:2022-1989) = chg_dist(iregn,ichrg,2023-1989)
        enddo
      enddo
      
! ... Read in ZEV mandates

      CALL GETRNGR('ZEV_State_Alloc ',ZEV_State_Alloc(1:9,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)  
	  CALl GETRNGR('Reg_Share_Adj   ',Reg_Share_Adj(1:9,iy:ijumpyr),MNUMCR-2,Num_to_Read,1)
	  CALL GETRNGR('ZEV_Requirement ',ZEV_Requirement(1:3,iy:IJUMPYR),MAXZEV,Num_to_Read,1)
	  CALL GETRNGR('ZEV_Requirement_optional ',ZEV_Requirement_optional(1:3,iy:IJUMPYR),MAXZEV,Num_to_Read,1)
	  CALL GETRNGR('ZEV_Req_CA_CO2  ',ZEV_Req_CA_CO2(iy:IJUMPYR),Num_to_Read,1,1)
	  CALL GETRNGR('TZEV_Req_CA_CO2 ',TZEV_Req_CA_CO2(iy:IJUMPYR),Num_to_Read,1,1)
	  CALL GETRNGR('VMT_CA_CO2      ',VMT_CA_CO2(iy:IJUMPYR),Num_to_Read,1,1)
	  CALL GETRNGR('ZEV_Multiplier  ',ZEV_Multiplier(1:maxldv,iy:IJUMPYR),MAXLDV,Num_to_read,1) 
	  CALL GETRNGR('zev_basebank    ',zev_basebank(1:mnumcr-2,1:4),MNUMCR-2,4,1)
	  CALL GETRNGR('CA_shr_of9      ',CA_shr_of9,1,1,1)
	  CALL GETRNGR('CO_shr_of8      ',CO_shr_of8,1,1,1) 
	  CALL GETRNGR('VA_shr_of5      ',VA_shr_of5,1,1,1) 
	  CALL GETRNGR('MN_shr_of4      ',MN_shr_of4,1,1,1) 
	  CALL GETRNGR('Bank_buffer     ',Bank_buffer(iy:IJUMPYR),Num_to_Read,1,1) 
	  CALL GETRNGR('ZEV_sales_dist  ',ZEV_sales_dist,maxldv,1,1) 
	  CALL GETRNGR('TZEV_sales_dist ',TZEV_sales_dist,maxldv,1,1)   
      CALL GETRNGR('s177_traveling_factor ',s177_traveling_factor(1:mnumcr-2),MNUMCR-2,1,1)

! ... read light duty vehicle fleet input variables
! ...   Note: indexed by fleet type: 1) business  2) government  3) utility 4) taxi  
! ...         and vehicle type:      1) car       2) light truck              
      CALL GETRNGR('FLTTRANSPC      ',FLTTRANS(1:MAXFLEET,1:MAXAGE,1),MAXFLEET,MAXAGE,1)    ! fraction of fleet passenger cars transfered to households
      CALL GETRNGR('FLTTRANSLT      ',FLTTRANS(1:MAXFLEET,1:MAXAGE,2),MAXFLEET,MAXAGE,1)    ! fraction of fleet passenger cars transfered to households
	  CALL GETRNGR('SURVFLT         ',SURVFLT,MAXFLEET,MaxAge,MAXVTYP)
      CALL GETRNGR('FLTVMTYR        ',FLTVMTYR(1:MAXFLEET,iy:IJUMPYR,1:maxvtyp),MAXFLEET, Num_to_Read,MAXVTYP)  ! VMT per vehicle by fleet type
	  CALL GETRNGR('FLT_COVID       ',FLT_COVID(1:MAXFLEET,iy:IJUMPYR),MAXFLEET,Num_to_Read,1)
	  
! ... *******************************************************************************
! ... * Light Duty Vehicle Stock Module input variables                             *
! ... *******************************************************************************
! ... * LDV Stock Accounting Model                                                  *
! ... *******************************************************************************

      CALL GETRNGR('SURV25          ',SURV25,      MNUMCR,MaxAge,MaxVtyp)
	  CALL GETRNGR('SURV_ADJ        ',SURV_ADJ    (iy:IJUMPYR),Num_to_Read,1,1)

!...  Calculate 25 vintage survival curves for cars and light trucks
!	  Convert cumulative survival (from input file) into annual survival rates
	  do iregn=1,mnumcr
        do IVTYP=1,maxvtyp
          SSURV25(iregn,1,IVTYP) = SURV25(iregn,1,IVTYP)
          do iage=2,maxage
            SSURV25(iregn,iage,IVTYP) = SURV25(iregn,iage,IVTYP)/SURV25(iregn,iage-1,IVTYP)
          enddo
	    enddo
      enddo
	  
      CALL GETRNGR('PVMT_ICE        ',PVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,1) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('PVMT_DSL        ',PVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,2) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('PVMT_EV100      ',PVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,4) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('PVMT_EV200      ',PVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,7) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('PVMT_EV300      ',PVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,15),MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('PVMT_PHEV20     ',PVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,5) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('PVMT_PHEV50     ',PVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,6) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('PVMT_HEV        ',PVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,16),MAXAGE,STOCKYR-2016+1,mnumcr-2)
      
      CALL GETRNGR('LVMT_ICE        ',LVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,1) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('LVMT_DSL        ',LVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,2) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('LVMT_EV100      ',LVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,4) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('LVMT_EV200      ',LVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,7) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('LVMT_EV300      ',LVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,15),MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('LVMT_PHEV20     ',LVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,5) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('LVMT_PHEV50     ',LVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,6) ,MAXAGE,STOCKYR-2016+1,mnumcr-2)
      CALL GETRNGR('LVMT_HEV        ',LVMT(1:maxage,27:STOCKYR-1989,1:mnumcr-2,16),MAXAGE,STOCKYR-2016+1,mnumcr-2)

      CALL GETRNGR('CMPGSTKGAS95    ',CMPGSTKGAS95,MAXVTYP,MAXAGE,1)         
      CALL GETRNGR('STKAVGWGT       ',STKAVGWGT,   MAXVTYP,MAXAGE,1)
      CALL GETRNGR('TRWTCAR_HIST    ',TRWTCAR_HIST(IY:IJUMPYR),Num_to_Read,1,1)
      CALL GETRNGR('TRWTTRK_HIST    ',TRWTTRK_HIST(IY:IJUMPYR),Num_to_Read,1,1)

	  CALL GETRNGR('VMT_SCHED_PARAM ',VMT_SCHED_PARAM(1:MAXLDV,1:2),MAXLDV,2,1)

!	  Fill in VMT for powertrains that are assumed equal to gasoline ICE
	  DO icl = 27, STOCKYR-1989
	    PVMT(:,icl,:,3)  = PVMT(:,icl,:,1)
		PVMT(:,icl,:,8)  = PVMT(:,icl,:,1)
		PVMT(:,icl,:,9)  = PVMT(:,icl,:,1)
		PVMT(:,icl,:,10) = PVMT(:,icl,:,1)
		PVMT(:,icl,:,11) = PVMT(:,icl,:,1)
		PVMT(:,icl,:,12) = PVMT(:,icl,:,1)
		PVMT(:,icl,:,13) = PVMT(:,icl,:,1)
		PVMT(:,icl,:,14) = PVMT(:,icl,:,1)
		LVMT(:,icl,:,3)  = LVMT(:,icl,:,1)
		LVMT(:,icl,:,8)  = LVMT(:,icl,:,1)
		LVMT(:,icl,:,9)  = LVMT(:,icl,:,1)
		LVMT(:,icl,:,10) = LVMT(:,icl,:,1)
		LVMT(:,icl,:,11) = LVMT(:,icl,:,1)
		LVMT(:,icl,:,12) = LVMT(:,icl,:,1)
		LVMT(:,icl,:,13) = LVMT(:,icl,:,1)
		LVMT(:,icl,:,14) = LVMT(:,icl,:,1)
	  ENDDO
!	  Populate pre-2016 (pre-Polk VMT data) with 2016 values
	  DO icl = iy, 2015-1989
	    PVMT(:,icl,:,:) = PVMT(:,2016-1989,:,:)
	    LVMT(:,icl,:,:) = LVMT(:,2016-1989,:,:)
	  ENDDO
!	  Populate post-STOCKYR (post-Polk VMT data) with STOCKYR values (projected VMT schedule = last historical)
!	  Ramp up EV (ildv=15) VMT schedule to converge with a given share of the gasoline ICE schedule (VMT_MAXSHR) 
!	  by the year specified in trnldvx.xlsx (VMT_ENDYR).
	  DO icl = STOCKYR-1989, IJUMPYR
	    DO ildv = 1, maxldv
		  IF (VMT_SCHED_PARAM(ildv,1).eq.0.0) THEN
	        PVMT(:,icl,:,ildv) = PVMT(:,STOCKYR-1989,:,ildv)
	        LVMT(:,icl,:,ildv) = LVMT(:,STOCKYR-1989,:,ildv)
		  ELSE
		    DO iregn = 1, mnumcr-2
			  DO iage = 1, maxage
		        PVMT(iage,icl,iregn,ildv) = MIN((PVMT(iage,STOCKYR-1989,iregn,1)*VMT_SCHED_PARAM(ildv,1)-PVMT(iage,STOCKYR-1989,iregn,ildv))* &
									          (icl+1989-STOCKYR)/(VMT_SCHED_PARAM(ildv,2)-STOCKYR) + PVMT(iage,STOCKYR-1989,iregn,ildv), &
									         PVMT(iage,STOCKYR-1989,iregn,1)*VMT_SCHED_PARAM(ildv,1))
			    LVMT(iage,icl,iregn,ildv) = MIN((LVMT(iage,STOCKYR-1989,iregn,1)*VMT_SCHED_PARAM(ildv,1)-LVMT(iage,STOCKYR-1989,iregn,ildv))* &
									          (icl+1989-STOCKYR)/(VMT_SCHED_PARAM(ildv,2)-STOCKYR) + LVMT(iage,STOCKYR-1989,iregn,ildv), &
									         LVMT(iage,STOCKYR-1989,iregn,1)*VMT_SCHED_PARAM(ildv,1))
			  ENDDO
			ENDDO
		  ENDIF
		ENDDO
	  ENDDO

!...calculate US household average PVMT and LVMT to prevent 0.0 values where no stock exists
!...stock weighted average calculated later
	do ix=iy,ijumpyr
	  do iage=1,maxage 
		do icl=1,maxclass 
		  do ildv=1,maxldv
			pvmt(iage,ix,mnumcr,ildv) = sum(pvmt(iage,ix,1:mnumcr-2,ildv))/9.0
			lvmt(iage,ix,mnumcr,ildv) = sum(lvmt(iage,ix,1:mnumcr-2,ildv))/9.0
		  enddo 
	    enddo 
	  enddo
	enddo

! ... *******************************************************************************
! ... * Vehicle Miles Traveled Model                                                *
! ... *******************************************************************************

      CALL GETRNGR('M_CD_AGE_DIST   ',M_CD_AGE_DIST   (1:agegrp,iy:IJUMPYR,1:mnumcr-2),AGEGRP,Num_to_Read,MNUMCR-2)
      CALL GETRNGR('F_CD_AGE_DIST   ',F_CD_AGE_DIST   (1:agegrp,iy:IJUMPYR,1:mnumcr-2),AGEGRP,Num_to_Read,MNUMCR-2)
      CALL GETRNGR('M_CD_AGE_DIST_L ',M_CD_AGE_DIST_L (1:agegrp,iy:IJUMPYR,1:mnumcr-2),AGEGRP,Num_to_Read,MNUMCR-2)
      CALL GETRNGR('F_CD_AGE_DIST_L ',F_CD_AGE_DIST_L (1:agegrp,iy:IJUMPYR,1:mnumcr-2),AGEGRP,Num_to_Read,MNUMCR-2)	  
      CALL GETRNGR('M_CD_AGE_DIST_H ',M_CD_AGE_DIST_H (1:agegrp,iy:IJUMPYR,1:mnumcr-2),AGEGRP,Num_to_Read,MNUMCR-2)
      CALL GETRNGR('F_CD_AGE_DIST_H ',F_CD_AGE_DIST_H (1:agegrp,iy:IJUMPYR,1:mnumcr-2),AGEGRP,Num_to_Read,MNUMCR-2)	 
      CALL GETRNGR('AGE_ADJ         ',AGE_ADJ         (1:mf,30:61),mf,32,1)
      CALL GETRNGR('AGE_ADJ_L       ',AGE_ADJ_L       (1:mf,30:61),mf,32,1)
      CALL GETRNGR('AGE_ADJ_H       ',AGE_ADJ_H       (1:mf,30:61),mf,32,1)	  
      CALL GETRNGR('BETACOST        ',BETACOST,        MF,AGEGRP,1)
      CALL GETRNGR('BETAINC         ',BETAINC,         MF,AGEGRP,1)
      CALL GETRNGR('BETAVMT         ',BETAVMT,         MF,AGEGRP,1)
      CALL GETRNGR('BETAVPLD        ',BETAVPLD,        MF,AGEGRP,1)
      CALL GETRNGR('BETAEMP         ',BETAEMP,         MF,AGEGRP,1)
      CALL GETRNGR('ALPHA           ',ALPHA,           MF,AGEGRP,1)
      CALL GETRNGR('LICRATE_M       ',LICRATE_M       (1:agegrp,iy:IJUMPYR,1:mnumcr-2),AGEGRP,Num_to_Read,MNUMCR-2)
      CALL GETRNGR('LICRATE_F       ',LICRATE_F       (1:agegrp,iy:IJUMPYR,1:mnumcr-2),AGEGRP,Num_to_Read,MNUMCR-2)
	  CALL GETRNGR('LIC_TREND       ',LIC_TREND,       AGEGRP,MF,MNUMCR-2)
	  CALL GETRNGR('LIC_MAX         ',LIC_MAX,         AGEGRP,MF,MNUMCR-2)
	  CALL GETRNGR('LIC_ELAS        ',LIC_ELAS,        MNUMCR-2,AGEGRP,1)
      CALL GETRNGI('LICRHISTYR      ',LICRHISTYR,      1,1,1)
      CALL GETRNGR('VMTLD           ',VMTLD           (1:agegrp,iy:IJUMPYR,1:mf),AGEGRP,Num_to_Read,MF)       
      CALL GETRNGI('VMTLDHISTYR     ',VMTLDHISTYR,     1,1,1)

! ... *******************************************************************************
! ... * Freight Transport Module input variables                                    *
! ... *******************************************************************************
! ... * Rail Freight Model                                                          *
! ... *******************************************************************************

      CALL GETRNGR('RAIL_FUEL       ',RAIL_FUEL,4,1,1)
      CALL GETRNGR('LNG_MAXPEN      ',LNG_MAXPEN,1,40,1)
      CALL GETRNGR('LOCOM_LIFE      ',LOCOM_LIFE,1,30,1)
      CALL GETRNGI('NGYEAR          ',NGYEAR,1,1,1)
      CALL GETRNGR('CIDISCOUNT      ',CIDISCOUNT,1,1,1)      
      CALL GETRNGR('LOCOMBTU        ',LOCOMBTU,1,1,1)
      CALL GETRNGR('DISCRT          ',DISCRT,1,1,1)
      CALL GETRNGI('PAYBK           ',PAYBK,1,1,1)
      CALL GETRNGR('RLNG_INCCOST    ',RLNG_INCCOST,1,1,1)
      CALL GETRNGR('RLNG_LEARN      ',RLNG_LEARN,1,1,1)
      CALL GETRNGI('RAILHISTYR      ',RAILHISTYR,1,1,1)
      CALL GETRNGR('RHIST_NCTONMI   ',RHIST_NCTONMI(iy:RAILHISTYR,1:mnumcr-2),RAILHISTYR-iy+1,MNUMCR-2,1)
      CALL GETRNGR('RHIST_CTONMI    ',RHIST_CTONMI(iy:RAILHISTYR,1:mnumcr-2),RAILHISTYR-iy+1,MNUMCR-2,1)
      CALL GETRNGR('RTM_SHARES      ',RTM_SHARES (1:mnumcr-2,1:16),MNUMCR-2,16,1)
      CALL GETRNGR('FREFF           ',FREFF(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('HTFREFF         ',HTFREFF(iy:IJUMPYR),Num_to_read,1,1)

! ... *******************************************************************************
! ... * Miscellaneous Transportation Energy Demand Module input variables           *
! ... *******************************************************************************

      CALL GETRNGR('MFDH            ',MFDH      (1:4,iy:IJUMPYR),4,Num_to_Read,1)
      CALL GETRNGI('MILTHISTYR      ',MILTHISTYR,1,1,1)
      CALL GETRNGR('MILTRSHR90      ',MILTRSHR90,4,MNUMCR-2,1)
      CALL GETRNGI('TRHISTYEAR      ',TRHISTYEAR,1,1,1)
      CALL GETRNGR('TR_COEF         ',TR_COEF   (1:MNUMCR-2,1:4),MNUMCR-2,4,1)
      CALL GETRNGR('TRCOVID         ',TRCOVID   (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TRRPMHIST       ',TRRPMHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TREFFHIST       ',TREFFHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TREDHIST        ',TREDHIST  (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TR_CAV_ADJ      ',TR_CAV_ADJ(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGI('CRHISTYEAR      ',CRHISTYEAR,1,1,1)
      CALL GETRNGR('CR_COEF         ',CR_COEF   (1:MNUMCR-2,1:4),MNUMCR-2,4,1)
      CALL GETRNGR('CRCOVID         ',CRCOVID   (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_read,1)
      CALL GETRNGR('CRCON           ',CRCON,     MNUMCR-2,1,1)
      CALL GETRNGR('CRINC           ',CRINC,     MNUMCR-2,1,1)
      CALL GETRNGR('CRFC            ',CRFC,      MNUMCR-2,1,1)
      CALL GETRNGR('CRDUM           ',CRDUM,     MNUMCR-2,1,1)
      CALL GETRNGR('CREDDSHR        ',CREDDSHR,  MNUMCR-2,1,1)
      CALL GETRNGR('CRRPMHIST       ',CRRPMHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('CREFFHIST       ',CREFFHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('CREDDHIST       ',CREDDHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('CREDEHIST       ',CREDEHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('CR_CAV_ADJ      ',CR_CAV_ADJ(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGI('IRHISTYEAR      ',IRHISTYEAR,1,1,1)
      CALL GETRNGR('IRCON           ',IRCON,     1,1,1)
      CALL GETRNGR('IRINC           ',IRINC,     1,1,1)
      CALL GETRNGR('IRPMCL          ',IRPMCL,    1,1,1)
      CALL GETRNGR('IRFC            ',IRFC,      1,1,1)
      CALL GETRNGR('IRRPMHIST       ',IRRPMHIST (iy:IJUMPYR),Num_to_Read,1,1)
      CALL GETRNGR('IRPMPCHIST      ',IRPMPCHIST(iy:IJUMPYR),Num_to_Read,1,1)
      CALL GETRNGR('IREFFHIST       ',IREFFHIST (iy:IJUMPYR),Num_to_Read,1,1)
      CALL GETRNGR('IREDDHIST       ',IREDDHIST (iy:IJUMPYR),Num_to_Read,1,1)
      CALL GETRNGR('IREDEHIST       ',IREDEHIST (iy:IJUMPYR),Num_to_Read,1,1)
      CALL GETRNGR('IRREGSHR        ',IRREGSHR,  MNUMCR-2,1,1)
      CALL GETRNGR('RECFDH          ',RECFDH    (1:2,IY:IJUMPYR),2,Num_to_read,1)
      CALL GETRNGI('RBHISTYR        ',RBHISTYR,  1,1,1)
      CALL GETRNGR('RBCON           ',RBCON,     2,1,1)
      CALL GETRNGR('RBINC           ',RBINC,     2,1,1)
      CALL GETRNGR('RBFC            ',RBFC,      2,1,1)
      CALL GETRNGR('LUBFDH          ',LUBFDH    (iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGI('LUBHISTYR       ',LUBHISTYR, 1,1,1)
      CALL GETRNGR('Cyc_RPM         ',Cyc_RPM    (iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGI('CycHistYR       ',CycHistYR, 1,1,1)
      CALL GETRNGR('TBPMTHIST       ',TBPMTHIST (1:MNUMCR-2,IY:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TBCOVID         ',TBCOVID   (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TB_COEF         ',TB_COEF   (1:MNUMCR-2,1:4),MNUMCR-2,4,1)
      CALL GETRNGI('TBHISTYEAR      ',TBHISTYEAR,1,1,1)
      CALL GETRNGR('TBPMTPC08       ',TBPMTPC08, 9,1,1)
      CALL GETRNGR('TBPMTPCGR       ',TBPMTPCGR, 9,1,1)
      CALL GETRNGR('TBBTUPMHIST     ',TBBTUPMHIST(1:MNUMCR-2,IY:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TBSYSEFF        ',TBSYSEFF,  9,1,1)
      CALL GETRNGR('TB_CAV_ADJ      ',TB_CAV_ADJ(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('TBFSHRHIST      ',TBFSHRHIST(1:8,IY:IJUMPYR,1:MNUMCR-2),8,Num_to_Read,MNUMCR-2) 
      CALL GETRNGR('TMODINIT        ',TMODINIT  (1:2,IY:IJUMPYR),2,Num_to_Read,1) !ccl
      CALL GETRNGR('TMCOVID         ',TMCOVID   (1:2,IY:IJUMPYR),2,Num_to_Read,1)
      CALL GETRNGR('TMPASMIL        ',TMPASMIL,  2,1,1)
      CALL GETRNGR('TMEFFINIT       ',TMEFFINIT (1:2,IY:IJUMPYR),2,Num_to_Read, 1)  !ccl
      CALL GETRNGR('QMODFSHRH       ',QMODFSHRH (1:8,IY:IJUMPYR,1:2),8,Num_to_Read, 2) !ccl 
      CALL GETRNGR('SCHBUS_SHR      ',SCHBUS_SHR,MNUMCR-2,1,1)
      CALL GETRNGR('EFF_ADJ         ',EFF_ADJ,   8,1,1)
      CALL GETRNGR('SCHBUS_EV_SHR   ',SCHBUS_EV_SHR(1:MNUMCR-2,IY:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGI('IBSBHISTYEAR    ',IBSBHISTYEAR,1,1,1)  
      CALL GETRNGR('LTSPLIT         ',LTSPLIT       (iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('DUMM            ',DUMM          (iy:IJUMPYR),Num_to_read,1,1) ! car share dummy
      CALL GETRNGR('CCONSTANT       ',CCONSTANT,    MNUMCR-2,1,1)         ! Coefficient variables to determine
      CALL GETRNGR('CRHO            ',CRHO,         MNUMCR-2,1,1)         ! the car/light truck split
      CALL GETRNGR('CINC            ',CINC,         MNUMCR-2,1,1)
      CALL GETRNGR('CFUEL           ',CFUEL,        MNUMCR-2,1,1)
      CALL GETRNGR('CHP             ',CHP,          MNUMCR-2,1,1)
      CALL GETRNGR('CWGT            ',CWGT,         MNUMCR-2,1,1)
      CALL GETRNGR('CMPG            ',CMPG,         MNUMCR-2,1,1)
      CALL GETRNGR('CDUMM           ',CDUMM,        MNUMCR-2,1,1)
      CALL GETRNGR('TCONSTANT       ',TCONSTANT,    MNUMCR-2,1,1)
      CALL GETRNGR('TRHO            ',TRHO,         MNUMCR-2,1,1)
      CALL GETRNGR('TINC            ',TINC,         MNUMCR-2,1,1)
      CALL GETRNGR('TFUEL           ',TFUEL,        MNUMCR-2,1,1)
      CALL GETRNGR('THP             ',THP,          MNUMCR-2,1,1)
      CALL GETRNGR('TWGT            ',TWGT,         MNUMCR-2,1,1)
      CALL GETRNGR('TMPG            ',TMPG,         MNUMCR-2,1,1)
      CALL GETRNGR('TDUMM           ',TDUMM,        MNUMCR-2,1,1)

!     If running NOCAFE case (no Phase 3 EPA HD GHG reg), read in lower electrification transit and school bus shares
      IF (TRANEFF.eq.3) then
        CALL GETRNGR('TBFSHRHISTNC    ',TBFSHRHIST(1:8,IY:IJUMPYR,1:MNUMCR-2),8,Num_to_Read,MNUMCR-2)
        CALL GETRNGR('SCHBUS_EV_SHRNC ',SCHBUS_EV_SHR(1:MNUMCR-2,IY:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      ENDIF

! ... *******************************************************************************
! ... * STEO History shared to regions with SEDS, used to benchmark fuel use by census region
! ... *******************************************************************************
      CALL GETRNGR('MER_tran        ',MER_tran,  4,1,1)					   ! MER by fuel - gasoline, jet fuel, diesel, residual fuel
      CALL GETRNGI('ymer            ',ymer, 1,1,1)                         ! Last historic MER year
      CALL GETRNGI('ysteo           ',ysteo, 1,1,1)                        ! Last STEO projection year	  

      CALL GETRNGR('FUELTAX         ',FUELTAX(iy:IJUMPYR),Num_to_read,1,1)  ! Incremental petroleum fuel tax

! ... Read in, validate, and store for global reference the LDV/LDT technology matrix

      CALL GETRNGI('TECHGROUP       ',TECHGROUP ,1,MAXTECH*MAXVTYP,1)         ! Car/Truck Index
      CALL GETRNGI('TECHID          ',TECHID    ,1,MAXTECH*MAXVTYP,1)         ! Technology Index
      CALL GETRNGC('TECHLABIN       ',TECHLABIN ,1,MAXTECH*MAXVTYP,1)         ! Technology Label
      CALL GETRNGC('TECHSYSTEM      ',TECHSYSTEM,1,MAXTECH*MAXVTYP,1)         ! Vehicle System
      CALL GETRNGR('TECHFE          ',TECHFE    ,1,MAXTECH*MAXVTYP,1)         ! Percent Change in MPG
      CALL GETRNGR('TECHCOSTA       ',TECHCOSTA ,1,MAXTECH*MAXVTYP,1)         ! Absolute Incremental Cost
      CALL GETRNGR('TECHCOSTR       ',TECHCOSTR ,1,MAXTECH*MAXVTYP,1)         ! Weight-Based Percent Change in Cost
      CALL GETRNGR('TECHWGTA        ',TECHWGTA  ,1,MAXTECH*MAXVTYP,1)         ! Absolute Change in Weight
      CALL GETRNGR('TECHWGTR        ',TECHWGTR  ,1,MAXTECH*MAXVTYP,1)         ! Percent Change in Weight
      CALL GETRNGI('TECHFYR         ',TECHFYR   ,1,MAXTECH*MAXVTYP,1)         ! First Year Tech is Available
      CALL GETRNGR('TECHHP          ',TECHHP    ,1,MAXTECH*MAXVTYP,1)         ! Percent Change in Horsepower
      CALL GETRNGR('TECHLEARN       ',TECHLEARN ,1,MAXTECH*MAXVTYP,1)         ! Coefficient for Technology Learning Curve
      CALL GETRNGR('TECHLRN1        ',TECHLRN1  ,1,MAXTECH*MAXVTYP,1)         ! Coefficient for Learning Curve Trigger most techs
      CALL GETRNGR('TECHLRN2        ',TECHLRN2  ,1,MAXTECH*MAXVTYP,1)         ! Coefficient for Learning Curve Trigger mild, micro hyb, tires II
      CALL GETRNGC('TECHAPPLY       ',TECHAPPLY ,MAXTECH*MAXVTYP,MAXLDV,1)    ! Fueling Type Applicability Indicator

      NUMTECH = 0

      DO IVTYP=1,MAXVTYP
        LASTID = 0
        DO ITECH=1,MAXTECH
          K = ((IVTYP-1)*MAXTECH) + ITECH
          IF (TECHGROUP(K) .NE. IVTYP) STOP 101
          IF (TECHID(K) .EQ. 0) CYCLE
          IF (TECHID(K) .NE. LASTID+1) STOP 102
          LASTID = TECHID(K)
          TECHLABEL(TECHID(K),IVTYP)   = TECHLABIN(K)
          SYS_AFFECT(TECHID(K),IVTYP)  = TECHSYSTEM(K)
          DEL_FE(TECHID(K),IVTYP)      = TECHFE(K)*0.01
          DEL_COSTABS(TECHID(K),IVTYP) = TECHCOSTA(K) / MC_JPGDP(32) * MC_JPGDP(1)  ! convert from 2021$ to 1990$
          DEL_COSTWGT(TECHID(K),IVTYP) = TECHCOSTR(K) / MC_JPGDP(32) * MC_JPGDP(1)  ! convert from 2021$ to 1990$
          DEL_WGTABS(TECHID(K),IVTYP)  = TECHWGTA(K)
          DEL_WGTWGT(TECHID(K),IVTYP)  = TECHWGTR(K)*0.01
          FRSTYEARX(TECHID(K),IVTYP)   = TECHFYR(K)
          DEL_HP(TECHID(K),IVTYP)      = TECHHP(K)*0.01
          COEFF_LEARN(TECHID(K),IVTYP) = TECHLEARN(K)
          COEFF_LRN1(TECHID(K),IVTYP)  = TECHLRN1(K)
          COEFF_LRN2(TECHID(K),IVTYP)  = TECHLRN2(K)
          DO ILDV=1,MAXLDV
            CALL UPPERCASE (TECHAPPLY(K,ILDV))
            TECH_APPLIC(TECHID(K),IVTYP,ILDV) = .FALSE.
            IF (TECHAPPLY(K,ILDV) .EQ. 'YES') TECH_APPLIC(TECHID(K),IVTYP,ILDV) = .TRUE.
          ENDDO
        ENDDO
        IF (IVTYP .EQ. 1) NUMTECH = LASTID
        IF (LASTID .NE. NUMTECH) STOP 103
      ENDDO

! ... Expand technology introduction year (FRSTYEAR) from domestic and import to all groups since
! ... some techs have non-zero base penetrations prior to the overall introduction year
! ... (in effect, the intro year can delay introduction for either domestics or
! ... imports while the same tech can already be in the market for the other through
! ... the input of a non-zero base year penetration).
      do itech=1,numtech
       do IGP=1,MAXGROUP
        ix=GrpMap(IGP)
        FRSTYEAR(itech,IGP)=FRSTYEARX(itech,ix)
       end do
      end do

! ... Read in, validate, and store for global reference technology market penetration data

      CALL GETRNGI('MKTGROUP        ',MKTGROUP     ,1,MAXGROUP*MAXTECH,1)             ! Vehicle Group Number
      CALL GETRNGI('MKTTECH         ',MKTTECH      ,1,MAXGROUP*MAXTECH,1)             ! Technology Number
      CALL GETRNGR('TECHMKTSHARE    ',TECHMKTSHARE ,MAXGROUP*MAXTECH,MAXCLASS*2,1)    ! Technology Base and Max Market Shares

! ... Because a limited number of techs can have non-zero base shares prior to the
! ... overall introduction year for the technology, an allowance for this situation
! ... must exist.  In effect, the intro year can be domestic or import rather than
! ... just car/truck specific by setting the base year share for the "quicker" intro
! ... group to a non-zero value.  FEM will then adjust the intro year for this
! ... tech/group combination to the base year.  Control is maintained over this intro
! ... year shifting by requiring the affected technology to be explicitly listed in
! ... the following array ALT_FRSTYEAR to indicate an "okay to shift" status.
! ... Nonlisted techs will be held to the input FRSTYEAR and subjected to normal error
! ... detection and processing as appropriate (i.e., base year share must be zero if
! ... FRSTYEAR is later than the base year).

      DO IGP=1,MAXGROUP
        IVTYP=grpmap(IGP)
        DO ITECH=1,MAXTECH
          K = ((IGP-1)*MAXTECH) + ITECH
          IF (MKTGROUP(K) .NE. IGP) STOP 120
          IF (ITECH .GT. NUMTECH) CYCLE
          IF (MKTTECH(K) .NE. ITECH) STOP 121
          DO ICL=1,MAXCLASS
            MKT_PEN(ICL,IGP,ITECH,BASE,GAS) = TECHMKTSHARE(K,2*ICL-1) * 0.01
            MKT_MAX(ICL,IGP,ITECH,GAS)      = TECHMKTSHARE(K,2*ICL)   * 0.01

            INPUT_ERROR = 0

            IF (MKT_PEN(ICL,IGP,ITECH,BASE,GAS) .LT. 0.0-ROUNDOFF_ERROR) THEN
              INPUT_ERROR = 1
            ENDIF

            IF (INPUT_ERROR .EQ. 0 .AND. MKT_MAX(ICL,IGP,ITECH,GAS) .GT. 1.0+ROUNDOFF_ERROR) THEN
              INPUT_ERROR = 3
            ENDIF

            IF (INPUT_ERROR .EQ. 0 .AND. MKT_MAX(ICL,IGP,ITECH,GAS) .LT. MKT_PEN(ICL,IGP,ITECH,BASE,GAS)) THEN
              INPUT_ERROR = 4
            ENDIF

            IF (INPUT_ERROR .NE. 0) THEN
              WRITE (*,*)
              WRITE (*,*) '======================================'
              WRITE (*,*)
              IF (INPUT_ERROR .EQ. 1) THEN
                WRITE (*,*) 'Base Tech Pen is less than Zero'
              ELSEIF (INPUT_ERROR .EQ. 2) THEN
                WRITE (*,*) 'Base Tech Pen is Non-Zero Prior to'
                WRITE (*,*) '            Tech Introduction Year'
              ELSEIF (INPUT_ERROR .EQ. 3) THEN
                WRITE (*,*) 'Max Tech Pen is greater than One'
              ELSEIF (INPUT_ERROR .EQ. 4) THEN
                WRITE (*,*) 'Max Tech Pen is less than Base Pen'
              ENDIF
              WRITE (*,*)
              WRITE (*,*) 'Vehicle Group    = ',GROUPLABEL(IGP)
              WRITE (*,*) 'Vehicle Class    = ',CLASSLABEL(ICL,IGP)
              WRITE (*,*) 'Technology ID    = ',TECHLABEL(ITECH,IVTYP)
              WRITE (*,*) 'Base Penetration = ',MKT_PEN(ICL,IGP,ITECH,BASE,GAS)
              WRITE (*,*) 'Max Penetration  = ',MKT_MAX(ICL,IGP,ITECH,GAS)
              IF (INPUT_ERROR .EQ. 2) WRITE (6,*) 'Tech Intro Year  = ',FRSTYEAR(ITECH,IGP)
              WRITE (*,*)
              WRITE (*,*) '     ***** Run ABORTED *****'
              WRITE (*,*)
              WRITE (*,*) 'Fix Tech Market Share Matrix and Rerun'
              WRITE (*,*)
              WRITE (*,*) '======================================'
              STOP
            ENDIF

          ENDDO
        ENDDO
      ENDDO

! ... Read in, validate, and store for global reference technology engineering note data

      CALL GETRNGI('ENGNOTETYPE     ',ENGNOTETYPE      ,1,400,1)      ! Engineering Note Type
      CALL GETRNGI('ENGNOTETECH01   ',ENGNOTETECH(1,1) ,1,400,1)      ! Engineering Note Technology ID #1
      CALL GETRNGI('ENGNOTETECH02   ',ENGNOTETECH(1,2) ,1,400,1)      ! Engineering Note Technology ID #2
      CALL GETRNGI('ENGNOTEYEAR     ',ENGNOTEYEAR      ,1,400,1)      ! Engineering Note Year
      CALL GETRNGR('ENGNOTEPCT      ',ENGNOTEPCT       ,1,400,1)      ! Engineering Note Percent Affected
      CALL GETRNGI('ENGNOTETECH03   ',ENGNOTETECH(1,3) ,1,400,1)      ! Engineering Note Technology ID #3
      CALL GETRNGI('ENGNOTETECH04   ',ENGNOTETECH(1,4) ,1,400,1)      ! Engineering Note Technology ID #4
      CALL GETRNGI('ENGNOTETECH05   ',ENGNOTETECH(1,5) ,1,400,1)      ! Engineering Note Technology ID #5
      CALL GETRNGI('ENGNOTETECH06   ',ENGNOTETECH(1,6) ,1,400,1)      ! Engineering Note Technology ID #6
      CALL GETRNGI('ENGNOTETECH07   ',ENGNOTETECH(1,7) ,1,400,1)      ! Engineering Note Technology ID #7
      CALL GETRNGI('ENGNOTETECH08   ',ENGNOTETECH(1,8) ,1,400,1)      ! Engineering Note Technology ID #8
      CALL GETRNGI('ENGNOTETECH09   ',ENGNOTETECH(1,9) ,1,400,1)      ! Engineering Note Technology ID #9
      CALL GETRNGI('ENGNOTETECH10   ',ENGNOTETECH(1,10),1,400,1)      ! Engineering Note Technology ID #10

      NUM_SUP = 0
      NUM_REQ = 0
      NUM_SYN = 0
      NUM_MAN = 0

      DO I=1,MAXNOTE
        IF (ENGNOTETYPE(I) .LT. 0 .OR. ENGNOTETYPE(I) .GT. 4) STOP 130
        IF (ENGNOTETECH(I,1)  .LT. 0 .OR. ENGNOTETECH(I,1)  .GT. NUMTECH) STOP 131
        IF (ENGNOTETYPE(I) .NE. 4) THEN
          IF (ENGNOTETECH(I,2) .LT. 0 .OR. ENGNOTETECH(I,2) .GT. NUMTECH) STOP 132
        ELSE
          IF (ABS(ENGNOTETECH(I,2)) .GT. (IJUMPCALYR-BYR)+1) STOP 133
        ENDIF
        IF (ENGNOTETECH(I,3)  .LT. 0 .OR. ENGNOTETECH(I,3)  .GT. NUMTECH) STOP 134
        IF (ENGNOTETECH(I,4)  .LT. 0 .OR. ENGNOTETECH(I,4)  .GT. NUMTECH) STOP 135
        IF (ENGNOTETECH(I,5)  .LT. 0 .OR. ENGNOTETECH(I,5)  .GT. NUMTECH) STOP 136
        IF (ENGNOTETECH(I,6)  .LT. 0 .OR. ENGNOTETECH(I,6)  .GT. NUMTECH) STOP 137
        IF (ENGNOTETECH(I,7)  .LT. 0 .OR. ENGNOTETECH(I,7)  .GT. NUMTECH) STOP 138
        IF (ENGNOTETECH(I,8)  .LT. 0 .OR. ENGNOTETECH(I,8)  .GT. NUMTECH) STOP 139
        IF (ENGNOTETECH(I,9)  .LT. 0 .OR. ENGNOTETECH(I,9)  .GT. NUMTECH) STOP 140
        IF (ENGNOTETECH(I,10) .LT. 0 .OR. ENGNOTETECH(I,10) .GT. NUMTECH) STOP 141
        IF (ENGNOTEYEAR(I) .LT. 0) STOP 142
        IF (ABS(ENGNOTEPCT(I)) .GT. 100.0) STOP 143

        IF (ENGNOTETYPE(I) .EQ. 0) CYCLE

        IF (ENGNOTETYPE(I) .EQ. 1) THEN               ! Setup parameters for SUPERSEDES notes
          NUM_SUP = NUM_SUP + 1
          SUPERSEDES(1,NUM_SUP) = ENGNOTETECH(I,1)
          SUPERSEDES(2,NUM_SUP) = ENGNOTETECH(I,2)
          TECH_CNT(NUM_SUP) = 2
          DO J = 3,10
            IF (ENGNOTETECH(I,J) .EQ. 0) CYCLE
            TECH_CNT(NUM_SUP) = TECH_CNT(NUM_SUP) + 1
            SUPERSEDES(TECH_CNT(NUM_SUP),NUM_SUP) = ENGNOTETECH(I,J)
          ENDDO

        ELSEIF (ENGNOTETYPE(I) .EQ. 2) THEN           ! Setup parameters for REQUIRED notes
          NUM_REQ = NUM_REQ + 1
          REQUIRES(1,NUM_REQ) = ENGNOTETECH(I,1)
          REQUIRES(2,NUM_REQ) = ENGNOTETECH(I,2)

        ELSEIF (ENGNOTETYPE(I) .EQ. 3) THEN           ! Setup parameters for SYNERGY notes
          NUM_SYN = NUM_SYN + 1
          SYNERGY(1,NUM_SYN) = ENGNOTETECH(I,1)
          SYNERGY(2,NUM_SYN) = ENGNOTETECH(I,2)
          SYNR_DEL(NUM_SYN)  = ENGNOTEPCT(I) * 0.01

        ELSE                                          ! Setup parameters for MANDATORY notes

! ... Note, the mandatory override flag (MAND_ORIDE), activated by negating the
! ... number of phase-in years on input, signifies a technology that has NO
! ... econometric value.  The override flag will force any econometrically driven
! ... market share for the applicable technology back to zero after completion of
! ... the econometric processing loop in FEMCALC.

          NUM_MAN = NUM_MAN + 1
          IF (ENGNOTETECH(I,2) .LT. 0) THEN
            SIGN = -1
            MAND_ORIDE(NUM_MAN) = .TRUE.
          ELSE
            SIGN = 1
            MAND_ORIDE(NUM_MAN) = .FALSE.
          ENDIF
          MANDYEAR(1,NUM_MAN) = ENGNOTETECH(I,1)
          MANDYEAR(2,NUM_MAN) = ENGNOTEYEAR(I)
          MANDYEAR(3,NUM_MAN) = ENGNOTETECH(I,2) * SIGN
          MANDMKSH(NUM_MAN)   = ENGNOTEPCT(I) * 0.01

        ENDIF
      ENDDO

      Requires(:,:)=0
      mand_oride(:)=.false.


! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . HAV variable read-in . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!         lidar learning curve parameters; curves are defined for multiple (maxlidar) systems defined by performance requirements
      CALL GETRNGR('LIDAR_PROD_THRSH',LIDAR_PROD_THRSH(1:MAXPRODPHASE,1:MAXLIDAR), MAXPRODPHASE,1,MAXLIDAR)		!...cumulative lidar production at threshold of production phases
      CALL GETRNGR('LIDAR_COEF_A    ',LIDAR_COEF_A(1:MAXPRODPHASE,1:MAXLIDAR), MAXPRODPHASE,1,MAXLIDAR)			!...lidar cost reduction equation coefficients
      CALL GETRNGR('LIDAR_COEF_B    ',LIDAR_COEF_B(1:MAXPRODPHASE,1:MAXLIDAR), MAXPRODPHASE,1,MAXLIDAR)			!...lidar cost reduction equation coefficients
      CALL GETRNGI('FIRST_LIDAR_YEAR',FIRST_LIDAR_YEAR,1,MAXLIDAR,1)											!...first year lidar is available
      CALL GETRNGR('LIDAR_RND_PROD  ',LIDAR_RND_PROD(1:MAXLIDAR,first_read_year:lyr), MAXLIDAR,Num_to_Read,1)

	  LIDAR_COEF_A = LIDAR_COEF_A / MC_JPGDP(26) * MC_JPGDP(1)													!...convert coef A ("zero" year cost) from 2015$ to 1990$

      CALL GETRNGI('hav_lidar_map   ',hav_lidar_map,1,maxhav-1,1)						    					!...lidar (1, 2) used by each HAV - 4a [1], 4b [2], and 5 [3]
      CALL GETRNGR('hav_sys_lrn     ',hav_sys_lrn(1:maxhav-1,first_read_year:lyr),maxhav-1,num_to_read,1)    	!...HAV system cost (no lidar or battery) reductions for 4a [1], 4b [2], and 5 [3]
      CALL GETRNGR('HAV_battery_kWh ',HAV_battery_kWh,maxhav-1,1,1)    											!...HAV battery capacity for 4a [1], 4b [2], and 5 [3]
	  CALL GETRNGR('hav_techmap     ',hav_techmap,maxhav-1,1,1)						    						!...Maps HAV levels to tech matrix indices (e.g. 4a:90, 4b:91, 5:92)	

!...Fleet HAV adoption variable inputs
      CALL GETRNGR('taxi_rev_params ',taxi_rev_params,maxhav,6,1)	! parameters for taxi revenue calculation
	  do ihav = 1, maxhav
         taxi_mi_life(ihav) = taxi_rev_params(ihav,1)									! taxi lifetime miles
         taxi_idle_gph(ihav) = taxi_rev_params(ihav,2)									! taxi idle fuel rate, gallons/hr
         taxi_maint_cost(ihav) = taxi_rev_params(ihav,3) / MC_JPGDP(26) * MC_JPGDP(1)	! taxi maintenance costs per month (independent of mileage), input in 2015$; converted to 1990$
         taxi_data_fee(ihav) = taxi_rev_params(ihav,4) / MC_JPGDP(26) * MC_JPGDP(1)		! HAV data fee per month, input in 2015$; converted to 1990$
         taxi_insur(ihav) = taxi_rev_params(ihav,5) / MC_JPGDP(26) * MC_JPGDP(1)		! taxi insurance fee per month, input in 2015$; converted to 1990$
	  enddo

      CALL GETRNGR('taxi_mi_ann     ',taxi_mi_ann,mnumcr-2,maxhav,1)			! taxi annual mileage
      CALL GETRNGR('taxi_shifts     ',taxi_shifts,mnumcr-2,maxhav,1)			! average number of paid shifts per taxi per day; applied to salary
      CALL GETRNGR('taxi_idle_hrs   ',taxi_idle_hrs,mnumcr-2,maxhav,1)			! taxi idle hours
      CALL GETRNGR('taxi_live_frac  ',taxi_live_frac,mnumcr-2,maxhav,1)			! taxi live mile fraction
	  CALL GETRNGR('taxi_maint_mi   ',taxi_maint_mi,mnumcr-2,maxhav,1)			! taxi maintenance cost; mileage component, 2015$
      CALL GETRNGR('taxi_rev_permi  ',taxi_rev_permi,mnumcr-2,maxhav,1)			! taxi revenue per mile, 2015$
	  CALL GETRNGR('hav_oper_limit  ',hav_oper_limit,mnumcr-2,maxhav,1)			! HAV operational domain limit attribute for adoption decision
	  
      taxi_rev_permi(:,:) = taxi_rev_permi(:,:) / MC_JPGDP(26) * MC_JPGDP(1)	! convert revenue and per mile maintenance cost from 2015$ to 1990$
	  taxi_maint_mi(:,:) = taxi_maint_mi(:,:) / MC_JPGDP(26) * MC_JPGDP(1)

      CALL GETRNGR('taxi_disc_r     ',taxi_disc_r,1,1,1)				! discount rate for taxi NPV calculations (fleet owner's next best investment)
      CALL GETRNGR('taxi_salary     ',taxi_salary,1,1,1)				! taxi driver salary
      CALL GETRNGR('hav_newtech_lim ',hav_newtech_lim,maxhav,1,1)		! HAV new technology limit attribute for adoption decision
	  CALL GETRNGR('taxi_rev_coef   ',taxi_rev_coef,1,1,1)				! taxi net lifetime revenue coefficient for adoption decision
	  CALL GETRNGR('taxi_newtech_pd ',taxi_newtech_pd,1,1,1)			! HAV time-based new tech function parameter (Weibull), years
	  CALL GETRNGR('taxi_newtech_r  ',taxi_newtech_r,1,1,1)				! HAV time-based new tech function parameter (Weibull), slope
	  CALL GETRNGI('hav_newtech_lag ',hav_newtech_lag,1,1,1)			! HAV time-based new tech function parameter (Weibull), slope
	
      taxi_salary = taxi_salary / MC_JPGDP(26) * MC_JPGDP(1)			! convert salary from 2015$ to 1990$

	  CALL GETRNGR('HAV_MPGDEG      ',hav_mpgdeg(1:maxhav,first_read_year:lyr),maxhav,num_to_read,1)	!...HAV MPG degradation multiplier; pre-calculated based on set HAV intro years. 
	  
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . End HAV variable read-in . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
	  
	  
! close xmlout (trnldvx.txt unit) and set to 0 to end debug output
      close(xmlout)
      xmlout=0
      RETURN
      END SUBROUTINE READLDV

	  
! ==========================================================================================================
!...Subroutine HAVCALC
!   Description:
!       Calculates HAV total system incremental cost for level 4a, 4b, and 5 systems, including:
!			1) a light-detection and ranging (LiDAR) array, 
!			2) a lithium-ion battery that powers the system, and 
!			3) the remaining HAV system (sensors, processors, communication system, wiring, mounting hardware, etc.).
!		Uses output from LIDARCOSTCALC (1) and LIONCOSTCALC (2) which apply production based learning. Uses 
!		input initial cost and time-based reduction curve for the remaining system components (3) which are considered 
!		more mature.
!	Output:
!		hav_sys_cost(maxhav, BYR:LYR)	...HAV system total incremental cost, by HAV level, including lidar and battery
! ==========================================================================================================
  SUBROUTINE HAVCALC
  USE T_
  IMPLICIT NONE

!...Local variable definitions
	INTEGER :: minyear									!...first year that any lidar system or hav vehicle is available


!...Determine first year lidar or hav is available and check that current analysis year is after available date
	minyear = MIN(MINVAL(first_lidar_year),MINVAL(frstyear(hav_techmap,:)))
!    if (yrs.lt.minyear) then
!        write(21,*) "HAV cost subroutine called in year ", yrs, " prior to lowest lidar and hav first year."
!        RETURN 
!    endif

!...Get current year LIDARCOST (lidar_cost(maxlidar, yrs))
    CALL LIDARCOSTCALC          ! returns lidar_cost(maxhav,byr:lyr)

!...Calculate total HAV incremental cost
	do ihav = 2, maxhav
        if (yrs.ge.MINVAL(frstyear(hav_techmap(ihav-1),:))) then
			hav_sys_cost(ihav, yrs) = hav_sys_lrn(ihav-1, yrs)*DEL_COSTABS(hav_techmap(ihav-1),1) + Li_ion_Cost(16, yrs)*hav_battery_kWh(ihav-1) + lidar_cost(ihav,yrs)
		endif
    enddo
		
    RETURN
    END SUBROUTINE HAVCALC
	  
! ==========================================================================================================
!...Subroutine LIDARCOSTCALC
!   Description:
!       Calculates LiDAR system cost ($/system) for highly automated vehicles. 
!		LiDAR cost is modeled at the package level, assuming that cost and functionality would be technology-independent
!		and similar whether the manufacturers implemented a single high-resolution, 360 degree field of view LiDAR unit,
!		or multiple LiDAR units with limited fields of view (choice based on least cost as solid state and new approaches mature). 
!		Costs are estimated for two (maxlidar) different LiDAR systems based on functional requirements: 
!			high-resolution - capable of bothhigh- and low-speed operation, and 
!			low-resolution - capable of low-speed operation only. 
!		Level 4a uses low-resolution LiDAR, while Levels 4b and 5 use high-resolution LiDAR.
!		Cost reductions are calculated using classic experience curve based on cumulative production. Learning is calculated 
!		separately for each of the two systems (assumed to use different technology approaches). Each of the two cost curves 
!		has five different production phases with production thresholds specified in trnldv: 
!			R&D, revolutionary, evolutionary, mature, and high-volume. 
!		These phases are characterized by different learning rates, with faster learning taking place during the revolutionary and 
!		evolutionary periods, and slower learning during the mature and high-volume phases. An assumed input R&D production,  
!		in conjunction with the R&D phase of the cost curve, ensures that LiDAR system cost reduction continues even if HAVs do 
!		not penetrate the market, thereby accounting for HAV testing and other uses (e.g. defense, science, drones, agriculture). 
!		First run of LIDARCOSTCALC occurs at n = first_lidar_year
!	Output:
!		lidar_cost(MAXHAV, yrs)
! ==========================================================================================================
  SUBROUTINE LIDARCOSTCALC
  USE T_
  IMPLICIT NONE

! Local variables
    INTEGER :: ic										!...temporary looping counter for calendar year
    INTEGER :: ilidar									!...temporary looping counter for lidar system type index (1:maxlidar)
    REAL :: lidar_cost_temp(maxlidar, BYR:LYR)=0.0		!...lidar cost dimensioned by lidar type (low-res v. high-res); only used within this subroutine

!... subroutine execution shouldn't occur if n is less than BOTH first_lidar_year values
!    if (yrs.lt.first_lidar_year(1).and.yrs.lt.first_lidar_year(2)) then
!        write(21,*) "Lidar cost subroutine called in year ", yrs, " prior to lidar first year."
!        RETURN 
!    endif
	
	lidarsales(:,yrs-1) = 0.0		! initialize annual sales
	
    do ilidar = 1, MAXLIDAR
        !...Calculate cumulative production for each LiDAR system (1: low-speed, 2: high-speed) based on vehicle sales.
        if (yrs.ge.first_lidar_year(ilidar)) then

			!...first populate annual lidar sales by lidar type using map to vehicle HAV level
			do ihav = 2, maxhav
				if (hav_lidar_map(ihav-1).eq.ilidar) then			! determine which ihav sales have ilidar system and sum across ihav
					lidarsales(ilidar, yrs-1) = lidarsales(ilidar, yrs-1) + sum(taxi_sales(:,:,:,:,yrs-1,ihav))		! annual sales by lidar type
				endif
			enddo

            if (yrs.eq.first_lidar_year(ilidar))then
                !...Accumulate existing R&D curve (lidar_rnd_prod) up to the year before first_lidar_year
                cumul_lidar_prod(ilidar, First_Read_Year) = 0
                do ic = First_Read_Year+1, first_lidar_year(ilidar)-1
                    cumul_lidar_prod(ilidar, ic) = cumul_lidar_prod(ilidar, ic-1) + lidar_rnd_prod(ilidar, ic)
                enddo

            elseif (yrs.gt.first_lidar_year(ilidar))then
                !...First check to make sure historical data is populated; if not write warning
!                if (cumul_lidar_prod(ilidar, yrs-2).eq.0) then 
!                    write(21,*) "Historical cumulative lidar production for lidar system ", ilidar, "not populated in year ", yrs
!                endif
                !...Append last year's LiDAR system production. Only populating previous year's cumul sales which are used for this year's cost
                !...Use R&D production if sales have not reached R&D levels yet
                if (lidarsales(ilidar, yrs-1).le.lidar_rnd_prod(ilidar, yrs-1)) then
                    cumul_lidar_prod(ilidar, yrs-1) = cumul_lidar_prod(ilidar,yrs-2) + lidar_rnd_prod(ilidar, yrs-1)
                else
                    cumul_lidar_prod(ilidar, yrs-1) = cumul_lidar_prod(ilidar,yrs-2) + lidarsales(ilidar, yrs-1)
                endif
				
            endif

			!...determine phase and calculate cost
            if (cumul_lidar_prod(ilidar, yrs-1).gt.LIDAR_PROD_THRSH(maxprodphase, ilidar)) then
				lidar_cost_temp(ilidar, yrs) = LIDAR_COEF_A(maxprodphase, ilidar)*cumul_lidar_prod(ilidar, yrs-1)**(-LIDAR_COEF_B(maxprodphase, ilidar))
				lidar_phase(ilidar,yrs)=maxprodphase	! For writes only
            else		
                do iphase=1,MAXPRODPHASE-1
                    !...If cumulative production (as of previous year) is less than the next phase's threshold, use the current phase's curve to calculate cost
                    if (cumul_lidar_prod(ilidar, yrs-1).le.LIDAR_PROD_THRSH(iphase+1, ilidar)) then
                        lidar_cost_temp(ilidar, yrs) = LIDAR_COEF_A(iphase, ilidar)*cumul_lidar_prod(ilidar, yrs-1)**(-LIDAR_COEF_B(iphase, ilidar))
						lidar_phase(ilidar,yrs)=iphase	! For writes only
						exit
                    endif
                enddo
            endif
        endif
    enddo

	!...now populate cost at ihav level using map
	do ihav=2, maxhav
		do ilidar = 1, maxlidar
		    if (hav_lidar_map(ihav-1).eq.ilidar) then
			    lidar_cost(ihav,yrs) = lidar_cost_temp(ilidar,yrs)
		    endif
		enddo
	enddo

	RETURN
  END SUBROUTINE LIDARCOSTCALC
!
  SUBROUTINE FLTHAV
  USE T_
  IMPLICIT NONE

! ==========================================================================================================
!...Subroutine FLTHAV 
!   Description:
!       Calculates adoption of HAVs by taxi / ride-hailing fleets as a function of lifetime net revenue, operation 
!		and maintenance costs; new technology limitations; and operational domain limitations.
!
!		HAVs are modeled with three automation levels: Level 4a (L4a), Level 4b (L4b), and Level 5 (L5). L4a 
!		autonomous operation is restricted to low-speed (<35 mph) in limited geofenced areas such as urban centers. 
!		Low-speed-only operation requires a less sophisticated, lower-resolution, and lower-cost HAV system. L4b 
!		autonomous operation is restricted to limited geofenced areas, but includes any speed roads, and includes 
!		controlled environments such as limited-access highways. Highway speed operation requires a more sophisticated, 
!		higher-resolution, and more expensive HAV control system to accurately sense and react to its environment at 
!		longer range. It also requires faster computational speed because of the shorter response times needed at 
!		higher speeds. L5 vehicles can operate autonomously on all roads and road types and at all (legal) road speed 
!		limits and have no operational domains limitations. The L5 HAV system is marginally more expensive than the L4b 
!		system because of the need for a more capable and expensive processor and controller.
!
!		FLTHAV uses a logit choice equation to estimate sales shares of L4a, L4b, and L5 HAVs within taxi / ride-hailing
!		fleets. A time-dependent new technology variable f_wb is included to represent factors that limit adoption of new 
!		technologies, large capital requirements, limited model availability, production capacity restrictions, and 
!		other potential limitations. This variable is modeled using a Weibull function that allows these limitations to
!		erode toward zero over time. Additionally, the utility of HAV Levels 4a and 4b includes a parameter characterizing 
!		the disutility of their operational domain (speed, geography, weather) limitations. The present value of 
!		lifetime taxi net revenue is calculated from up-front vehicle cost, trip revenue, driver salary (if applicable), 
!		and operating costs that include fuel, maintenance, insurance, and data fees.
!
!	Uses technote and attribute data:
!		tech_applic(itech,ivtype,ILDV))
!		mkt_max(ICL,IGP,itech,ILDV)
!		! FEMPRI(IGP,ICL,YRS,ILDV,ihav) << need ihav dimension; currently using temp variable to get around
!		! FEMMPG(IGP,ICL,YRS,ILDV,ihav)
!		FRSTYEAR (itech, IGP)
!		DEL_COSTABS(itech, IVTYP)
!	*** this version operates at vtyp level instead of manufacturer since this is consumer adoption (cav manuf. do not make ihav=1) ***
!     the following attributes are accessed AFTER TLDV  
!		LDV_PRI(IVTYP,ILDV,ICL,yrs)
!		LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)
!		FPRICE(ILDV,iregn,yrs) -- from TLDV consumer adoption, in cents/gal
!	Output:
!		flt_hav_shares(mnumcr, maxvtyp, MAXCLASS, maxldv, BYR:LYR,,maxhav)   -- shares of taxi fleet within:
!           type, class, ldv, cr of each ihav.  Adds to 1 accross ihav.
!       taxi_sales(mnumcr,maxvtyp,MAXCLASS,maxldv,BYR:LYR,maxhav)            -- sales of taxis
! ==========================================================================================================
 
!
! Needs base (level 0) vehicle price, and HAV systsem incremental cost including lidar_cost(ihav, yrs), and MPG. 
! ...Access after TLDV to get prices after adjustment for production volume:
!       FEMPRI(IGP,ICL,YRS,ILDV,ihav), FEMMPG(IGP,ICL,YRS,ILDV,ihav)     <<<<<< assume added ihav. *************** need to remove L0-3 penetrations from price!!
!       LDV_PRI(IVTYP,ILDV,ICL,yrs), LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)
! ...This also allows use of fuel prices calculated for consumer adoption:
!       FPRICE(ILDV,iregn,yrs) -- in cents/gal

!...Local variable definitions 
	LOGICAL*1 :: havatvflag(maxvtyp,MAXCLASS,maxldv)=.false.		! local variable for GRPFLAG - year ILDV is available in class
	REAL :: newtech_firstyr											! first year for new tech function
    REAL :: f_wb													! Weibull s-curve as function of years since intro (newtech_firstyr); = 1 at 0, approaching 0 at ~2*taxi_newtech_pd
	INTEGER :: taxi_life(mnumcr-2, maxhav)							! taxi lifetime in months
	INTEGER :: ipv													! looping variable for PV calculations, months
	INTEGER, PARAMETER :: iavg = 3									! number of years over which to calculate the average fuel price for price expectations
	REAL :: avgfuelprice(mnumcr-2,2)								! average fuel price (1) and lagged average fuel price (2)
	REAL :: del_fuelprice(mnumcr-2)									! expected monthly change in fuel price based on avg price and lagged avg price
	REAL :: fuelpriceproj											! temporary fuel price projection variable for NPV calculation
	REAL :: taxi_npv(mnumcr-2, maxhav)								! net present value of lifetime revenue and cost stream for adoption decision
	REAL :: taxi_fuel(mnumcr-2, maxhav)								! taxi fuel consumption in gallons / mo
	REAL :: taxi_mpg(maxhav)										! taxi fuel economy, including HAV degradation
	REAL :: taxi_mo_cost(mnumcr-2, maxhav)							! monthly costs except fuel (which changes per projected fuel price)
	REAL :: taxi_mo_rev(mnumcr-2, maxhav)							! monthly taxi revenue
	REAL :: taxi_util(mnumcr-2, maxhav)								! calculated utility for each ihav option
	REAL :: sum_exp_util											! sum of exp(utility) for each ihav option
	LOGICAL :: bTaxiUtilErr(maxvtyp,MAXCLASS,maxldv,mnumcr) = .false.	! error flag indicating a negative or zero utility for the conventional taxi; used to limit error messages
	LOGICAL :: bDoLogit = .true.									! error flag - if false, error occurred, skip logit and leave HAV shares at 0
	INTEGER :: iyearHAV
! ****************** temporary local variables for vehicle attributes - remove after ihav redimensioned variables are available
	REAL :: Vehprice(maxvtyp,MAXCLASS,BYR:LYR,maxldv,maxhav)
	REAL :: mpgtemp(maxvtyp,MAXCLASS,BYR:LYR,maxldv,maxhav)
!	

!...Initialize shares for this year. If this subroutine is aborted due to any error, HAV shares will remain at 0%
	flt_hav_shares(:,:,:,:,yrs,:) = 0.0
	flt_hav_shares(:,:,:,:,yrs,1) = 1.0

!...Set availability flag for this year
!...first set ILDV available year within class	
    havatvflag(:,:,:) = .false.
	do ILDV = 1, maxldv
		do ICL = 1, MAXCLASS
            if(ildv.eq.1.or.ildv.eq.15.or.ildv.eq.16) then
              havatvflag(1,ICL,ILDV) = ANY(CLASSFLAG(ICL,1:cargrp,ILDV))
			  havatvflag(2,ICL,ILDV) = ANY(CLASSFLAG(ICL,ltkgrp:maxgroup,ILDV))
            endif
		enddo
	enddo

!...make sure that hav is available
!    if (yrs.lt.MINVAL(frstyear(hav_techmap,:))) then
!        write(21,*) '***** WARNING: FLTHAV adoption subroutine called in year ', yrs, ' prior to first year any hav is available: ', MINVAL(frstyear(hav_techmap,:))
!        RETURN 
!    endif
!
!...calculate utility of HAV levels (including ihav = 1: L0-3) within type, class, and powertrain
	do IVTYP = 1, maxvtyp
		do ICL = 1, MAXCLASS
			do ILDV = 1, maxldv
			!...If no HAV is available in this class, vehicle type, and ILDV, exit ILDV loop and check next
			    if ((.not.(havatvflag(IVTYP,ICL,ILDV))).or.(.not.(any(tech_applic(hav_techmap,IVTYP,ILDV))))) then
				    taxi_sales(mnumcr,IVTYP,ICL,ILDV,yrs,1) = fltechsal(mnumcr,ivtyp,4,icl,ildv,1)
					CYCLE
				endif
			! get HAV incremental attributes: cost (eventually weight)
				CALL HAVCALC

			! ******************** until get variables with ihav, generate a local variable for price, mpg *********************
				do ihav = 1, maxhav
					Vehprice(IVTYP,ICL,yrs,ILDV,ihav) = LDV_PRI(IVTYP,ILDV,ICL,yrs) + hav_sys_cost(ihav, yrs)
					mpgtemp(IVTYP,ICL,yrs,ILDV,ihav) = LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)*hav_mpgdeg(ihav, yrs)
				enddo
				
			!...calculate new technology function - represents consumer knowledge and risk aversion as well as producer capacity limitations
			!... Weibull function begins with maximum value of 1.0 in yrs <= newtech_firstyr and erodes toward 0
				newtech_firstyr = MINVAL(frstyear(hav_techmap,:)) + hav_newtech_lag
				f_wb = 1.0
				if (yrs.gt.newtech_firstyr) then
					f_wb = exp(-((yrs - newtech_firstyr)/taxi_newtech_pd)**taxi_newtech_r)
				end if

				do iregn = 1, mnumcr-2
					sum_exp_util = 0.0			! initialize summed exp of util for this iregn
					do ihav = 1, maxhav			! this loop needs to include the L0-3 cav; make sure that ihav is the inner loop so that logit is calculated outside of this loop

			!...Is HAV ihav available?
						if (ihav.gt.1) then
							if (yrs.lt.MINVAL(frstyear(hav_techmap(ihav-1),:))) then
								taxi_util(iregn,ihav) = -1000000.0						! ensure no share before available
								exit													! skip this ihav and move to next
							endif
						endif
					
			!...Calculate an average fuel price and price expectation similar to FEMCALC but using ILDV specific fuel prices from TLDV.
			!...	Note that FEMCALC uses a 5-year average from 3-years ago (due to design cycles?).
			!...    Here we're using an average over iavg years, beginning with last year, and the change in 
			!...    this average based on a one-year lag. 
			!...    avgfuelprice(mnumcr-2,2) - recent avg in col 1 and lagged value in col 2.

						avgfuelprice(iregn,:) = 0.0
						do iyr = (yrs-iavg-1),(yrs-1)
							avgfuelprice(iregn,1) = avgfuelprice(iregn,1) + fprice(ILDV,iregn,iyr)/100			! converting fuel price from cents/gal to $/gal
							avgfuelprice(iregn,2) = avgfuelprice(iregn,2) + fprice(ILDV,iregn,iyr-1)/100
						enddo
						avgfuelprice(iregn,:) = avgfuelprice(iregn,:) / iavg
						del_fuelprice(iregn) = MAX(0.0,avgfuelprice(iregn,1)-avgfuelprice(iregn,2))/12.0		! apply delta monthly for cash flow; never allow to decrease

			!...monthly operating costs without fuel since fuel costs change each month
						taxi_mo_cost(iregn,ihav) = taxi_maint_cost(ihav) + taxi_data_fee(ihav) + taxi_insur(ihav) + taxi_shifts(iregn,ihav)*taxi_salary/12.0 + (taxi_mi_ann(iregn,ihav)/12.0)*taxi_maint_mi(iregn,ihav)

			!...monthly fuel consumption, including idling
						taxi_fuel(iregn, ihav) = (taxi_mi_ann(iregn,ihav)/12.0)/mpgtemp(IVTYP,ICL,yrs,ILDV,ihav) + taxi_idle_gph(ihav)*taxi_idle_hrs(iregn,ihav)

			!...monthly revenue
						taxi_mo_rev(iregn, ihav) = taxi_live_frac(iregn,ihav)*(taxi_mi_ann(iregn,ihav)/12.0)*taxi_rev_permi(iregn,ihav)
						
			!...calculate expected lifetime in months (based on lifetime and annual miles) for NPV function
						taxi_life(iregn, ihav) = CEILING(12.0*taxi_mi_life(ihav)/taxi_mi_ann(iregn,ihav))
						
			!...now calculate expected lifetime NPV (revenue - operating cost - capital cost) based on monthly cash flow						
						taxi_npv(iregn,ihav) = - Vehprice(IVTYP,ICL,yrs,ILDV,ihav)										! using temp price for now...
						fuelpriceproj = 0
						do ipv = 1, taxi_life(iregn,ihav)
							fuelpriceproj = avgfuelprice(iregn,1) + ipv*del_fuelprice(iregn)
							taxi_npv(iregn,ihav) = taxi_npv(iregn,ihav) + ((1.+taxi_disc_r/12.0)**(-ipv))*(taxi_mo_rev(iregn,ihav) - taxi_mo_cost(iregn,ihav) - fuelpriceproj*taxi_fuel(iregn,ihav))
						enddo
			!...determine utility
						taxi_util(iregn, ihav) = f_Wb*hav_newtech_lim(ihav) + hav_oper_limit(iregn, ihav) + taxi_rev_coef*taxi_npv(iregn, ihav)/1000.0
			!...sum exp(util) over ihav; since we skipped out of ihav loop if not yet available, these won't be included in the total sum
						sum_exp_util = sum_exp_util + exp(taxi_util(iregn,ihav))

					enddo ! next ihav

			!...Determine sales shares		

			!......First check for errors in utility calcs
					bDoLogit = .true.		! reset flag first
					if (taxi_util(iregn,1).le.0.0) then 				! utility of L0-3 taxi <= 0 & therefore has zero or negative net lifetime revenue
						write(21,*) '****** WARNING: conventional taxi utility is less than or equal to zero for:'
						write(21,'(A,I5,4(A,I3))') '  year  :', yrs, ',  class :', ICL, ',  ILDV  :', ILDV, ',  region:', iregn
						if (not(bTaxiUtilErr(IVTYP,ICL,ILDV,iregn))) then		! if this is the first year this occurred, write out detailed diagnostics
							bTaxiUtilErr(IVTYP,ICL,ILDV,iregn) = .true.			! set error flag so this is done only once per type / class / ILDV / region combo
							write(21,'(A,4F9.0)') '    Veh price :', Vehprice(IVTYP,ICL,yrs,ILDV,:)
							write(21,'(A,4F9.3)') '    Fuel price:', avgfuelprice(iregn,:)
							write(21,'(A,4F9.3)') '    Fuel cost :', fuelpriceproj
							write(21,'(A,4F9.3)') '    Delta_fuel:', del_fuelprice(iregn)
							write(21,'(A,4I9)')   '    Taxi life :', taxi_life(iregn,:)
							write(21,'(A,4F9.1)') '    Fuel      :', taxi_fuel(iregn,:)
							write(21,'(A,4F9.1)') '    MPG       :', mpgtemp(IVTYP,ICL,yrs,ILDV,:)
							write(21,'(A,4F9.0)') '    mo cost   :', taxi_mo_cost(iregn,:)
							write(21,'(A,4F9.0)') '    mo rev    :', taxi_mo_rev(iregn,:)
							write(21,'(A,4F9.0)') '    NPV       :', taxi_npv(iregn,:)
							write(21,'(A,4F9.6)') '    utility   :', taxi_util(iregn,:)
						endif
						if(maxval(taxi_util(iregn,1:maxhav)).le.0.0) then
							write(21,*) '****** All HAVs have zero or negative utility. HAV shares set to zero.'
							bDoLogit = .false.			! no need for logit calculations; leave ihav=1 share at 100%
						else
							write(21,*) '****** At least one HAV has positive utility. Negative utilities retained and HAV shares calculated using logit.'
						endif
					endif
					if (bDoLogit) then		! if bDoLogit = false - skip logit; all ihav have zero or negative utility; all HAV shares left at zero
						do ihav = 1, maxhav
							flt_hav_shares(iregn,IVTYP,ICL,ILDV,yrs,ihav) = exp(taxi_util(iregn,ihav))/sum_exp_util
							if (ihav.gt.1) then				! zero out shares for havs prior to first year available (should be tiny)
								if (yrs.lt.MINVAL(frstyear(hav_techmap(ihav-1),:))) then
									flt_hav_shares(iregn,IVTYP,ICL,ILDV,yrs,ihav) = 0.0
								endif
							endif
                        enddo
                        !...Normalize to remove rounding errors by adjusting ihav = 1: L0-3
						flt_hav_shares(iregn,IVTYP,ICL,ILDV,yrs,1) = 1. - sum(flt_hav_shares(iregn,IVTYP,ICL,ILDV,yrs,2:maxhav))
					endif
					! Apply HAV shares to total TAXI (ifleet=4) sales (FLTECHSAL). Prior to calling FLTHAV, all taxi sales are in ihav = 1.
				    taxi_sales(iregn,IVTYP,ICL,ILDV,yrs,:) = flt_hav_shares(iregn,IVTYP,ICL,ILDV,yrs,:)*fltechsal(iregn,ivtyp,4,icl,ildv,1)
					! Populate other fleet model sales/stock variables (JUST a redistribution of existing vehicles into ihav bins -- no changes to the totals)
					do ihav=1,maxhav
					  fltechsal(iregn,ivtyp,4,icl,ildv,ihav) = taxi_sales(iregn,IVTYP,ICL,ILDV,yrs,ihav)
					  Flt_Stock(iregn,IVTYP,4,ILDV,1,ihav,n)= sum(FLTECHSAL(iregn,ivtyp,4,1:maxclass,ildv,ihav))  
					enddo
				enddo	! next iregn 
				! Populate national totals for taxi_sales (written out for debug later) and fleet variables (used in fleet subroutines later)
				do ihav = 1, maxhav
					taxi_sales(mnumcr,IVTYP,ICL,ILDV,yrs,ihav) = sum(taxi_sales(1:mnumcr-2,IVTYP,ICL,ILDV,yrs,ihav))
					fltechsal(mnumcr,IVTYP,4,ICL,ILDV,ihav) = sum(fltechsal(1:mnumcr-2,ivtyp,4,icl,ildv,ihav))
					Flt_Stock(mnumcr,IVTYP,4,ILDV,1,ihav,n) = sum(FLTECHSAL(mnumcr,ivtyp,4,1:maxclass,ildv,ihav))  
					FLTECHSTK(mnumcr,ivtyp,4,ildv,ihav)=sum(Flt_Stock(mnumcr,ivtyp,4,ildv,1:maxage,ihav,n))
				enddo
			enddo   ! next ILDV
		enddo  ! next class
	enddo 	! next type
	
    RETURN
	END SUBROUTINE FLTHAV
!	  	  

! ==========================================================================================================
! ... Subroutine READSTOCK reads the spreadsheet input file TRNSTOCK.XML
! ==========================================================================================================
   SUBROUTINE READSTOCK
   USE T_
   IMPLICIT NONE

!...Declare local parameters

LOGICAL       NEW/.FALSE./
CHARACTER*18  INAME
INTEGER       WKUNIT,out78
INTEGER       numhhstk, numfltstk
PARAMETER     (numhhstk = maxldv*maxage*(mnumcr-2))                 ! Number of rows in household stock (1995 - STOCKYR) CARSTKNFREGN, LTSTKNFREGN
PARAMETER     (numfltstk = 11232)                                   ! Number of rows in fleet stock data (1995 - STOCKYR) CARSTKTFREGN, LTSTKTFREGN
                                                                    ! Census division * 16 LDV powertrains * # of vintages (by household or fleet type) * fleet type
REAL          CARSTKNFREGN(numhhstk,numstkyrs)		                ! Historic non-fleet car stock by fuel type, vintage, census division
REAL          CARSTKTFREGN(numfltstk,numstkyrs)		                ! Historic total fleet car stock by fuel type, vintage, census division
REAL          LTSTKNFREGN(numhhstk,numstkyrs)		                ! Historic non-fleet light truck stock by fuel type, vintage, census division
REAL          LTSTKTFREGN(numfltstk,numstkyrs)		                ! Historic total fleet light truck stock by fuel type, vintage, census division
Integer*2     LDV_CENSUS(numhhstk), LDV_FUEL(numhhstk), LDV_VINTAGE(numhhstk)
Integer*2     LDV_fleet(numfltstk), Fleet_census(numfltstk), Fleet_fuel(numfltstk), Fleet_vintage(numfltstk)
INTEGER*2     m2, r2, a2, f2, fl2, j2, y2

!...Output dataset
      INAME = 'TRNH2OUT'
      out78 = FILE_MGR('O',INAME,.TRUE.)

!...Store data ranges from xlsx file
      INAME = 'TRNSTOCKX'
      WKUNIT = FILE_MGR('O',INAME,NEW)   ! open trnstockx.xlsx input file
      CALL ReadRngXLSX(WKUNIT,'LDV')     ! read range names & corresponding data from worksheet "LDV"

! ... *******************************************************************************
! ... * Light Duty Vehicle Stock Module input variables                             *
! ... *******************************************************************************
! ... * LDV Stock Accounting Model                                                  *
! ... *******************************************************************************

!... Pulling in 2012 - onward data (regional data)	  
    ! Households
	  CALL GETRNGR('CARSTKNFREGN    ',CARSTKNFREGN, numhhstk,numstkyrs,1)
	  CALL GETRNGR('LTSTKNFREGN     ',LTSTKNFREGN,  numhhstk,numstkyrs,1)	  
	  CALL GETRNGI('LDV_CENSUS      ',LDV_CENSUS,  1,numhhstk,1)
	  CALL GETRNGI('LDV_FUEL        ',LDV_FUEL,    1,numhhstk,1)
	  CALL GETRNGI('LDV_VINTAGE     ',LDV_VINTAGE, 1,numhhstk,1)	  

    ! Fleets	  
	  CALL GETRNGR('CARSTKTFREGN    ',CARSTKTFREGN, numfltstk,numstkyrs,1)
	  CALL GETRNGR('LTSTKTFREGN     ',LTSTKTFREGN,  numfltstk,numstkyrs,1)
	  CALL GETRNGI('FLEET_CENSUS    ',FLEET_CENSUS, 1,numfltstk,1)
	  CALL GETRNGI('FLEET_FUEL      ',FLEET_FUEL,   1,numfltstk,1)
	  CALL GETRNGI('FLEET_VINTAGE   ',FLEET_VINTAGE,1,numfltstk,1)	  
	  CALL GETRNGI('LDV_FLEET       ',LDV_FLEET,    1,numfltstk,1)
	  	  

!...Redistributing LDV (Car and Class 1-2a Light Truck) stock data
!   All historical stocks assumed to be non-automated (ihav = 1)
!   LDV_STOCK(mnumcr,maxvtyp,maxowner,maxldv,maxage,maxhav,mnumyr)
    LDV_STOCK(:,:,:,:,:,:,:) = 0.0
!...Household stocks
    do m2 = 1,numhhstk
      do j2 = 1,numstkyrs 
	    y2 = j2 + 5
        a2 = LDV_VINTAGE(m2)
	    f2 = LDV_FUEL(m2)
	    r2 = LDV_CENSUS(m2)
	    LDV_STOCK(r2,1,1,f2,a2,1,y2) = CARSTKNFREGN(m2,j2)/1000000.0
	    LDV_STOCK(r2,2,1,f2,a2,1,y2) = LTSTKNFREGN(m2,j2)/1000000.0
	  enddo
    enddo
!...Fleet stocks
    do m2 = 1,numfltstk
      do j2 = 1,numstkyrs 
	    y2 = j2 + 5
        a2 = fleet_VINTAGE(m2)
	    f2 = fleet_FUEL(m2)
	    r2 = fleet_CENSUS(m2)
	    fl2 = LDV_fleet(m2) + 1
	    LDV_STOCK(r2,1,fl2,f2,a2,1,y2) = CARSTKTFREGN(m2,j2)/1000000.0
	    LDV_STOCK(r2,2,fl2,f2,a2,1,y2) = LTSTKTFREGN(m2,j2)/1000000.0 
      enddo
    enddo

!	Estimate CNG and LPG vehicle stocks (overwrites data from trnstockx.xlsx -- Polk does not track these vehicles)
    do j2 = 28, stockyr-1989
	  do iregn = 1,mnumcr-2
        do ILDV=9,12
!	      do ihav = 1,maxhav
		    do IVTYP = 1,maxvtyp
			  do iown=1,maxowner
			    if (iown.eq.1) then 
                  do iage=2,maxage-1
		            LDV_STOCK(iregn,IVTYP,iown,ILDV,iage,1,j2) = LDV_STOCK(iregn,IVTYP,iown,ILDV,iage-1,1,j2-1)*SSURV25(iregn,iage-1,IVTYP)	  
                  enddo
		          LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage,1,j2) = LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage-1,1,j2-1)*SSURV25(iregn,maxage-1,IVTYP) + &
			                                                     LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage,1,j2-1)*SSURV25(iregn,maxage,IVTYP)
				else
				  do iage=2,maxage-1
		            LDV_STOCK(iregn,IVTYP,iown,ILDV,iage,1,j2) = LDV_STOCK(iregn,IVTYP,iown,ILDV,iage-1,1,j2-1)*SURVFLT(iown-1,iage-1,IVTYP)	  
                  enddo
		          LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage,1,j2) = LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage-1,1,j2-1)*SURVFLT(iown-1,iage-1,IVTYP) + &
			                                                     LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage,1,j2-1)*SURVFLT(iown-1,iage-1,IVTYP)
				endif
			  enddo
		    enddo
!		  enddo
		enddo
	  enddo
    enddo
	
!...Filling in the national stock number
    do IVTYP = 1,maxvtyp
	  do iown = 1,maxowner    ! owner type
	    do ILDV = 1,maxldv
		  do iage = 1,maxage
			do j2 = 1,numstkyrs 
			  y2 = j2 + 5
		      LDV_STOCK(mnumcr,IVTYP,iown,ILDV,iage,1,y2) = sum(LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,iage,1,y2))
            enddo
		  enddo
		enddo
	  enddo
	enddo

    RETURN
    END SUBROUTINE READSTOCK	  	  
	  
! ==========================================================================================================
! ... Subroutine UPPERCASE converts all lower case characters in a string to upper case
! ==========================================================================================================
    SUBROUTINE UPPERCASE (TEXT)
    USE T_
    IMPLICIT NONE

      CHARACTER*(*) TEXT
      INTEGER       INDEX

      IF (LEN(TEXT) .EQ. 0) RETURN

      DO INDEX = 1,LEN(TEXT)
        IF (ICHAR(TEXT(INDEX:INDEX)) .GE. Z'61' .AND. &
            ICHAR(TEXT(INDEX:INDEX)) .LE. Z'7A') TEXT(INDEX:INDEX) = CHAR(ICHAR(TEXT(INDEX:INDEX))-Z'20')
      ENDDO

    RETURN
    END SUBROUTINE UPPERCASE

! ==========================================================================================================
! ... Subroutine TMAC reassigns MACRO data to TRAN model local variables
! ==========================================================================================================
    SUBROUTINE TMAC
    USE T_
    IMPLICIT NONE

! ... Add an incremental petroleum fuel tax to highway fuel costs.  The tax is read in as
! ... nominal $/million Btu, converted to 1987$, then applied to the fuel prices obtained from PMM.
      FUELTAX87(N) = FUELTAX(N) / MC_JPGDP(N)

! ... Multiplying FUELTAX87 by 0.901 adjusts for the higher Btu content of diesel fuel, thus
! ... maintaining an equivalent per gallon tax.
      DO IREGN=1,MNUMCR
        HWYPDSTR(IREGN,N) = PDSTRHWY(IREGN,N) !+ (FUELTAX87(N) * 0.901)
      ENDDO
	  
!...  IRA credits for PHEV and EV
	  ira_veh_cred = 0.0
	  ira_bat_cred = 0.0 
	  if(ira_stim.eq.1.0)then
	    ira_veh_cred = 3750.0/ MC_JPGDP(N) * MC_JPGDP(1)
		ira_bat_cred = 3750.0/ MC_JPGDP(N) * MC_JPGDP(1)		
	  endif

      DO I=1,MNUMCR
        IF (I .EQ. 10) CYCLE
!...    adjust price index when dollar year changes (from macro)
        INC00_D_NPT(:,:)  = MC_YPDR(:,:)/MC_NP(:,:)/(MC_JPGDP(23)/MC_JPGDP(11))
        INC00_D_NP(:,:)   = (MC_YPDR(:,:)/MC_NP(:,:))*1000.0 /(MC_JPGDP(23)/MC_JPGDP(11))
        INC00_D_16(:,:)   = (MC_YPDR(:,:)/MC_NP16A(:,:)) *1000.0 /(MC_JPGDP(23)/MC_JPGDP(11))
      ENDDO

! ... National average per capita income (1990 $) 
      do iregn = 1,mnumcr
          if(MC_NP(iregn,N).gt.0.0) INC90_D_NP(iregn,YRS) = (MC_YPDR(iregn,N)/MC_NP(iregn,N))*MC_JPGDP(1)*1000.0
      enddo
      
!...Start new population model
!...Populate regional age groups by gender
    DO iagr = 1,AGEGRP
      DO imf = 1,MF
!		TMC_NP15A(iagr,imf,11,n)=MC_NP(11,n)*POP_DIST(iagr,n,imf)
!        IF (MMAC.eq.4) TMC_NP15A(iagr,imf,11,n)=MC_NP(11,n)*POP_DIST_LOW(iagr,n,imf)
!        IF (MMAC.eq.5) TMC_NP15A(iagr,imf,11,n)=MC_NP(11,n)*POP_DIST_HIGH(iagr,n,imf)
        DO iregn = 1,MNUMCR-2
!		  reference case population distribution
 		  if(imf.eq.1) TMC_NP15A(iagr,imf,iregn,n)=MC_NP16A(iregn,n)*M_CD_AGE_DIST(iagr,n,iregn)
          if(imf.eq.2) TMC_NP15A(iagr,imf,iregn,n)=MC_NP16A(iregn,n)*F_CD_AGE_DIST(iagr,n,iregn)
!		  low macro case population distribution
          if(MMAC.eq.4) then
 		    if(imf.eq.1) TMC_NP15A(iagr,imf,iregn,n)=MC_NP16A(iregn,n)*M_CD_AGE_DIST_L(iagr,n,iregn)
            if(imf.eq.2) TMC_NP15A(iagr,imf,iregn,n)=MC_NP16A(iregn,n)*F_CD_AGE_DIST_L(iagr,n,iregn)	
		  endif
!		  high macro case population distribution		  
          if(MMAC.eq.5) then
 		    if(imf.eq.1) TMC_NP15A(iagr,imf,iregn,n)=MC_NP16A(iregn,n)*M_CD_AGE_DIST_H(iagr,n,iregn)
            if(imf.eq.2) TMC_NP15A(iagr,imf,iregn,n)=MC_NP16A(iregn,n)*F_CD_AGE_DIST_H(iagr,n,iregn)
		  endif
	    ENDDO
      ENDDO
    ENDDO

!...calculate employment rate for licensing rate
    EMP_RATE_LD(n) = MC_EEA(n)/MC_NP16A(11,n) !sum(TMC_NP15A(1:agegrp,1:mf,1:mnumcr-2,n)) 

!...calculate licensing rate for projection years
    if(curcalyr.gt.licrhistyr) then
      do iagr=1,agegrp
        do imf=1,mf
          do iregn=1,mnumcr-2
            if(imf.eq.1) then 
		      LICRATE_M(iagr,n,iregn) = LICRATE_M(iagr,n-1,iregn)+LIC_TREND(iagr,imf,iregn)+LIC_ELAS(iregn,iagr)*((EMP_RATE_LD(n)/EMP_RATE_LD(n-1))-1)
			  LICRATE_M(iagr,n,iregn) = MIN(LICRATE_M(iagr,n,iregn),LIC_MAX(iagr,mf,iregn)) 
    	    elseif(imf.eq.2) then
              LICRATE_F(iagr,n,iregn) = LICRATE_F(iagr,n-1,iregn)+LIC_TREND(iagr,imf,iregn)+LIC_ELAS(iregn,iagr)*((EMP_RATE_LD(n)/EMP_RATE_LD(n-1))-1)
			  LICRATE_F(iagr,n,iregn) = MIN(LICRATE_F(iagr,n,iregn),LIC_MAX(iagr,mf,iregn))
		    endif
          enddo
        enddo
      enddo
    endif
    
!...fill regional drivers (millions) assuming licrates across census division  
    do iagr=1,agegrp
      do imf=1,mf
        do iregn=1,mnumcr-2
          if(imf.eq.1) LICDRIVER(iagr,imf,iregn,n)=TMC_NP15A(iagr,imf,iregn,n)*LICRATE_M(iagr,n,iregn)
          if(imf.eq.2) LICDRIVER(iagr,imf,iregn,n)=TMC_NP15A(iagr,imf,iregn,n)*LICRATE_F(iagr,n,iregn)
        enddo
      enddo
    enddo
      
!...sum regional drivers to national levels new
    do iagr=1,agegrp
      do imf=1,mf
        LICDRIVER(iagr,imf,11,n)=sum(LICDRIVER(iagr,imf,1:mnumcr-2,n))
      enddo
    enddo
      
   RETURN
   END SUBROUTINE TMAC

! ==========================================================================================================
! ... Subroutine NEWLDV segments new light vehicle sales by car, light truck <8,500 GVW, and light truck 
! ... >8,500 GVW to <10,000 GVW.    
! ==========================================================================================================                        
  SUBROUTINE NEWLDV
  USE T_
  IMPLICIT NONE

    REAL            TEMPCLS12A(MNUMYR), TEMPLDVSALES(MNUMCR,N)
	REAL            PMGTR00_D_C(MNUMYR),PMGTR00_D_C_regn(mnumcr,mnumyr)
	REAL            CARSHRT_regn(mnumcr,mnumyr), TRKSHRT_regn(mnumcr,mnumyr), regn_shr(mnumcr,mnumyr)

!...Calculate gasoline price in 2000$ per gallon
    PMGTR00_D_C(N) = PMGTR(11,N)* MG_HHV/1000.0 * MC_JPGDP(11)*100.0
	do iregn = 1,mnumcr-2
	    PMGTR00_D_C_regn(iregn,N) = PMGTR(iregn,N)* MG_HHV/1000.0 * MC_JPGDP(11)*100.0
        IF (PMGTR(iregn,N).le.0.0.or.PMGTR(iregn,N).ne.PMGTR(iregn,N)) then
          WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
          WRITE(21,'(a,3(i4,","),11(f12.5,","))')'ERROR: PMGTR busted', curcalyr,curitr,iregn,PMGTR(:,N)
          STOP
        ENDIF
	enddo

    TEMPCLS12A(n)=(MC_Vehicles(1,n)+MC_Vehicles(2,n))*LTSplit(n)
    
!...Populate historic total sales for car and class 1-2a (millions of vehicles) by IVTYP, ILDV and region
!...and calculate needed car/truck shares for later use in the model.
    if(curcalyr.le.STOCKYR)then
!...  fill historic new car and light truck sales arrays
	  do ivtyp=1,Maxvtyp
        do iregn=1,mnumcr-2
	      NEWLDVs(ivtyp,iregn,n) = sum(LDV_STOCK(iregn,ivtyp,1:maxowner,1:maxldv,1,1,n))
	    enddo
	    NEWLDVs(ivtyp,mnumcr,n) = sum(LDV_STOCK(mnumcr,ivtyp,1:maxowner,1:maxldv,1,1,n)) 
      enddo	    
	  
!...  Calculate sales distribution of cars and light trucks across regions
      do ivtyp=1,maxvtyp
	    do iregn=1,mnumcr-2
		  CarTrkSplit(iregn,ivtyp,N) = sum(LDV_STOCK(iregn,ivtyp,1:maxowner,1:maxldv,1,1,n))/sum(LDV_STOCK(iregn,1:maxvtyp,1:maxowner,1:maxldv,1,1,n))
		enddo
	  enddo
	  CarTrkSplit(mnumcr,1,N) = sum(LDV_STOCK(mnumcr,1,1:maxowner,1:maxldv,1,1:maxhav,n))/sum(LDV_STOCK(mnumcr,1:maxvtyp,1:maxowner,1:maxldv,1,1,n))
	  CarTrkSplit(mnumcr,2,N) = 1.0 - CarTrkSplit(mnumcr,1,N)

!...  Calculate new vehicle sales per licensed driver by region	
	  do iregn=1,mnumcr-2
		NewLDVPerLD(iregn,n) = sum(NEWLDVs(1:maxvtyp,iregn,n))/sum(LicDriver(1:agegrp,1:mf,iregn,n))
	  enddo
	endif ! <= stockyr
	
!...Project percent of total light vehicles <8,500 GVW that are cars and light trucks by region
    if(curcalyr.gt.stockyr) then
!...  Determine new LDV sales shares by region post STOCKYR based on licensed drivers
      do iregn=1,mnumcr-2
	    TEMPLDVSALES(iregn,n) = NewLDVPerLD(iregn,stockyr-1989)*sum(LICDriver(1:agegrp,1:mf,iregn,n))
	  enddo
	  do iregn=1,mnumcr-2
	    regn_shr(iregn,n) = TEMPLDVSALES(iregn,n)/sum(TEMPLDVSALES(1:mnumcr-2,n))
	  enddo
!...  regional shares
	  do iregn = 1,mnumcr-2
        CARSHRT_regn(iregn,N) = EXP(CCONSTANT(iregn)*(1-CRHO(iregn))+(CRHO(iregn)*LOG(CarTrkSplit(iregn,1,n-1))) +          &
               CINC(iregn) *(LOG(INC00_D_16(iregn,n))- (CRHO(iregn)*LOG(INC00_D_16(iregn,n-1)))) + &
               CFUEL(iregn)*(LOG(PMGTR00_D_C_regn(iregn,n))  - (CRHO(iregn)*LOG(PMGTR00_D_C_regn(iregn,n-1)))) +   &
               CHP(iregn)  *(LOG(AHPCAR(iregn,n-1))   - (CRHO(iregn)*LOG(AHPCAR(iregn,n-2)))) +    &
               CWGT(iregn) *(LOG(AWTCAR(iregn,n-1))   - (CRHO(iregn)*LOG(AWTCAR(iregn,n-2)))) +    &
               CMPG(iregn) *(LOG(TRUEMPG_regn(iregn,1,n-1)) - (CRHO(iregn)*LOG(TRUEMPG_regn(iregn,1,n-2)))) +    &
               CDUMM(iregn)*(log(DUMM(n)) - (CRHO(iregn)*log(DUMM(n-1)))))

        TRKSHRT_regn(iregn,N) = EXP(TCONSTANT(iregn)*(1-TRHO(iregn))+(TRHO(iregn)*LOG(CarTrkSplit(iregn,1,n-1))) +          &
               TINC(iregn) *(LOG(INC00_D_16(iregn,n))- (TRHO(iregn)*LOG(INC00_D_16(iregn,n-1)))) + &
               TFUEL(iregn)*(LOG(PMGTR00_D_C_regn(iregn,n))  - (TRHO(iregn)*LOG(PMGTR00_D_C_regn(iregn,n-1)))) +   &
               THP(iregn)  *(LOG(AHPTruck(iregn,n-1)) - (TRHO(iregn)*LOG(AHPTruck(iregn,n-2)))) +    &
               TWGT(iregn) *(LOG(AWTTruck(iregn,n-1)) - (TRHO(iregn)*LOG(AWTTruck(iregn,n-2)))) +    &
               TMPG(iregn) *(LOG(TRUEMPG_regn(iregn,2,n-1)) - (TRHO(iregn)*LOG(TRUEMPG_regn(iregn,2,n-2)))) +    &
               TDUMM(iregn)*(log(DUMM(n)) - (TRHO(iregn)*log(DUMM(n-1)))))	
        
		CarTrkSplit(iregn,1,N) = CARSHRT_regn(iregn,n)/(CARSHRT_regn(iregn,n) + TRKSHRT_regn(iregn,n))
		CarTrkSplit(iregn,2,N) = 1 - CarTrkSplit(iregn,1,N)
! debug		
!		IF (CARSHRT_regn(iregn,N).ne.CARSHRT_regn(iregn,N)) then
!		  WRITE(21,'(a,i5,i3,8f8.4)')'ERROR CARSHRT_regn NaN ',n+1989,iregn,CCONSTANT(iregn),&
!					CRHO(iregn),CINC(iregn),INC00_D_16(iregn,n),PMGTR00_D_C_regn(iregn,n),AHPCAR(iregn,n-1),AWTCAR(iregn,n-1),TRUEMPG_regn(iregn,1,n-1)
!		ENDIF
!		IF (TRKSHRT_regn(iregn,N).ne.TRKSHRT_regn(iregn,N)) then
!		  WRITE(21,'(a,i5,i3,8f8.4)')'ERROR TRKSHRT_regn NaN ',n+1989,iregn,TCONSTANT(iregn),&
!					TRHO(iregn),TINC(iregn),INC00_D_16(iregn,n),PMGTR00_D_C_regn(iregn,n),AHPTruck(iregn,n-1),AWTTruck(iregn,n-1),TRUEMPG_regn(iregn,1,n-1)
!		ENDIF
!        write(21,'(a,",",3(i4,","),13(f14.4,","))')'test_mac1', curcalyr, curitr, iregn, CarTrkSplit(iregn,:,N), INC00_D_16(iregn,n), PMGTR00_D_C_regn(iregn,n), AHPCAR(iregn,n-1), AHPCAR(iregn,n-2),&
!                                                  AWTCAR(iregn,n-1),AWTCAR(iregn,n-2),TRUEMPG_regn(iregn,1,n-1),TRUEMPG_regn(iregn,1,n-2),DUMM(n)
      enddo

!...New car and light truck sales projection by census division
	  do ivtyp=1,maxvtyp
	    do iregn=1,mnumcr-2
	      NEWLDVs(ivtyp,iregn,n) = (MC_SUVA(N) + TEMPCLS12A(N)) * regn_shr(iregn,n) * CarTrkSplit(iregn,ivtyp,N)
!          write(21,'(a,",",4(i4,","),5(f14.4,","))')'test_mac2', curcalyr, curitr, ivtyp, iregn, NEWLDVs(ivtyp,iregn,n), MC_SUVA(N), TEMPCLS12A(N),regn_shr(iregn,n),CarTrkSplit(iregn,ivtyp,N)
		enddo
		NEWLDVs(ivtyp,mnumcr,n) = sum(NEWLDVs(ivtyp,1:mnumcr-2,n))
		if (ivtyp.eq.2) Then
		  CarTrkSplit(mnumcr,1,N) = NEWLDVs(1,mnumcr,n)/sum(NEWLDVs(1:maxvtyp,mnumcr,n))
		  CarTrkSplit(mnumcr,2,N) = 1 - CarTrkSplit(mnumcr,1,N)
		endif
	  enddo
	endif
	
    RETURN
    END SUBROUTINE NEWLDV

! ==========================================================================================================
! ... Subroutine TMPGNEW starts the fuel economy model, AFV model, and loads data inputs. After completion, 
! ... compute avg price of vehicles
! ==========================================================================================================           
    SUBROUTINE TMPGNEW
    USE T_
    IMPLICIT NONE

    integer i2,first_time/0/
    integer it,pass3,npass2,npass3,passall
	real	num,num1,num2,PHEV50_EVMT(MAXVTYP),PHEV20_EVMT(MAXVTYP),sales_per_nameplate,nameplate_sensitivity(ildv)
!...Load all historic data files
    if(first_time.eq.0) then
      first_time=1
      CALL READHIST
      CALL READNHTSA
      write(*,'(a)') 'Finished reading ReadHist'
    endif

!...calculate % of PHEV vmt in all electric mode.  
	if(curcalyr.eq.epalyr) then	
!...  cars
	  phev20_evmt(1) = 0.0 
	  phev50_evmt(1) = 0.0	
	  num1=0.0
	  num2=0.0	  
	  do igp=1,cargrp 
	  	do ildv=5,6 ! phev20, phev50
		  do icl=1,maxclass 
			if(femmpg(igp,icl,yrs,ildv).gt.0.0) then 
			  if(ildv.eq.5) num1 = num1 + phev_evmt(igp,icl,yrs,ildv) * cafesales(igp,icl,yrs,ildv)
			  if(ildv.eq.6) num2 = num2 + phev_evmt(igp,icl,yrs,ildv) * cafesales(igp,icl,yrs,ildv)
			endif 
		  enddo 
		enddo
	  enddo
	  phev20_evmt(1) = num1/sum(cafesales(1:cargrp,:,yrs,5)) 
	  phev50_evmt(1) = num2/sum(cafesales(1:cargrp,:,yrs,6))
!...  light trucks
	  phev20_evmt(2) = 0.0 
	  phev50_evmt(2) = 0.0	
	  num1=0.0
	  num2=0.0	  
	  do igp=ltkgrp,maxgroup 
	  	do ildv=5,6 ! phev20, phev50
		  do icl=1,maxclass
			if(femmpg(igp,icl,yrs,ildv).gt.0.0) then 
			  if(ildv.eq.5) num1 = num1 + phev_evmt(igp,icl,yrs,ildv) * cafesales(igp,icl,yrs,ildv)
			  if(ildv.eq.6) num2 = num2 + phev_evmt(igp,icl,yrs,ildv) * cafesales(igp,icl,yrs,ildv)
			endif 
		  enddo 
		enddo
	  enddo		  
	  phev20_evmt(2) = num1/sum(cafesales(ltkgrp:maxgroup,:,yrs,5)) 
	  phev50_evmt(2) = num2/sum(cafesales(ltkgrp:maxgroup,:,yrs,6))		  
	  do igp=1,maxgroup 
	    ivtyp = grpmap(igp)
	    do icl=1,maxclass 
		  do ildv=5,6
		    if(phev_evmt(igp,icl,yrs,ildv).eq.0.0) then
			  if(ildv.eq.5) phev_evmt(igp,icl,yrs,ildv) = phev20_evmt(ivtyp)
			  if(ildv.eq.6) phev_evmt(igp,icl,yrs,ildv) = phev50_evmt(ivtyp)
		    endif
		  enddo
		enddo 
	  enddo
	endif
!...fill values for phev_evmt projection years
	if(curcalyr.gt.epalyr) then 
	  do ildv=5,6 ! phev20, phev50
		do icl=1,maxclass 
		  do igp=1,maxgroup 
			phev_evmt(igp,icl,yrs,ildv) = phev_evmt(igp,icl,yrs-1,ildv)
		  enddo 
		enddo 
	  enddo 
	endif

!...In setting ATV car and truck flags, user inputs are overridden if the corresponding gasoline vehicle does not exist.
!...In other words, an ATV group/class combination is not allowed unless the same group/class combination is allowed for 
!...gasoline vehicles.  This is necessary since initial ATV attributes are expressed relative to gasoline. Note, the 
!...CLASSFLAG is set for all years after initial penetration year, since there is a time dimension. 
    
!...determine xyr and epalyr AFV availability 
	if(curcalyr.ge.xyr.and.curcalyr.le.epalyr) then     ! AEO2025: if 2022 or 2023
	  do ildv=1,maxldv
        do icl=1,maxclass
          do igp=1,maxgroup
            classflag(icl,igp,ildv)= .false.
!           Vehicle exists in 2022
	        if(curcalyr.eq.xyr.and.femmpg(igp,icl,yrs,ildv).ne.0.0) then
		      classflag(icl,igp,ildv) = .true.
!           Vehicle introduced in 2023
			elseif(.not.classflag(icl,igp,ildv).and.curcalyr.eq.epalyr.and.epampg(igp,icl,yrs,ildv).ne.0.0) then 
			  classflag(icl,igp,ildv) = .true.
			  do i=base,current
		        FE(icl,igp,i,ildv)       = epampg(igp,icl,yrs,ildv)
                WEIGHT(icl,igp,i,ildv)   = epawgt(igp,icl,yrs,ildv)
                PRICE(icl,igp,i,ildv)    = epapri(igp,icl,yrs,ildv)
                HP(icl,igp,i,ildv)       = epahp(igp,icl,yrs,ildv)
                TANKSIZE(icl,igp,i,ildv) = epatsz(igp,icl,yrs,ildv)
		        RANGE(icl,igp,i,ildv)	 = eparng(igp,icl,yrs,ildv)	
			    do itech=1,maxtech
			      MKT_PEN(icl,igp,itech,i,ildv) = MKT_PEN(icl,igp,itech,i,gas)
			    enddo
              enddo
			  FEMMPG(igp,icl,epalyr,ildv) = epampg(igp,icl,yrs,ildv)
			  FEMWGT(igp,icl,epalyr,ildv) = epawgt(igp,icl,yrs,ildv)
			  FEMPRI(igp,icl,epalyr,ildv) = epapri(igp,icl,yrs,ildv)
			  FEMHP(igp,icl,epalyr,ildv)	= epahp(igp,icl,yrs,ildv)
			  FEMTSZ(igp,icl,epalyr,ildv)	= epatsz(igp,icl,yrs,ildv)
			  FEMRNG(igp,icl,epalyr,ildv)	= eparng(igp,icl,yrs,ildv)
			endif
			if(classflag(icl,igp,ildv).and.epampg(igp,icl,yrs,ildv).eq.0.0) then 
!...		  ildv discontinued in epalyr
			  classflag(icl,igp,ildv) = .false.
			  do i=base,current
		        FE(icl,igp,i,ildv)       = 0.0
                WEIGHT(icl,igp,i,ildv)   = 0.0
                PRICE(icl,igp,i,ildv)    = 0.0
                HP(icl,igp,i,ildv)       = 0.0
                TANKSIZE(icl,igp,i,ildv) = 0.0
		        RANGE(icl,igp,i,ildv)	 = 0.0
			  enddo
			  FEMMPG(igp,icl,epalyr,ildv) = 0.0
			  FEMWGT(igp,icl,epalyr,ildv) = 0.0
			  FEMPRI(igp,icl,epalyr,ildv) = 0.0
			  FEMHP(igp,icl,epalyr,ildv)  = 0.0
			  FEMTSZ(igp,icl,epalyr,ildv) = 0.0
			  FEMRNG(igp,icl,epalyr,ildv) = 0.0			  
			endif
          enddo
        enddo
      enddo	
	endif

	
!...calculate lithium ion battery cost ($/kwh).		
!...Needs to happen before the first battery year, to populate historical cumulative_gwh for freight model
!   Also needs to be called BEFORE AFVADJ is called -- otherwise the vehicles introduced by GRPFLAG will be extremely expensive
	IF(YRS.GE.FIRST_BAT_YR+1989-1) CALL LIONCOSTCALC !2024

!   Set the sensitivity of the model to introducing new nameplates. Larger values mean the model is less sensitive (i.e., OEMs wait for more sales before introducing a new nameplate).
!   If the model exceeds the exogenous (trnldvx) average sales per nameplate, multiplied by the sensitivity below, it will introduce a new
!   nameplate in that group, size class, and powertrain.    
    if (curcalyr.ge.epalyr+2.and.curcalyr.le.2032.and.CAFEMY27_SWITCH.eq.1) then
      nameplate_sensitivity(:) = 1.0
    elseif (CAFEMY27_SWITCH.eq.1) then      ! Manufacturers less likely to introduce new alt-fuel nameplates when no regs apply
      nameplate_sensitivity(:) = 2.0
    else                                    ! Manufacturers even less likely to introduce new BEVs nameplates when future regs don't require them
      nameplate_sensitivity(:) = 2.0
      nameplate_sensitivity([4,7,15]) = 3.0
    endif
	 
!...determine ildv availability post epalyr
	if(curcalyr.ge.epalyr) then
!     Calculate nameplate counts for the projection (historical counts [epalyr] are populated from trnnhtsa.xlsx, in subroutine READNHTSA
	  if(curcalyr.gt.epalyr) then
        do ildv=1,maxldv
	 	  do icl=1,maxclass
	 	    do igp=1,maxgroup
!             Incorporate manufacturer-announced new non-gasoline vehicles (EXOGENOUS)
	 	 	  if(grpflag(ildv,icl,igp).eq.yrs.and.ildv.gt.1) then
!               If models already exist, add one
                if(classflag(icl,igp,ildv)) then
                  nameplate(igp,icl,yrs,ildv) = nameplate(igp,icl,yrs-1,ildv) + 1.0
!               If this nameplate is the first of it's kind (igp/icl/ildv), make it
	 	 	    elseif(.not.classflag(icl,igp,ildv)) then 
	 	 	      if (classflag(icl,igp,GAS)) then
	 	 	        classflag(icl,igp,ildv) = .true.
	 	 	        CALL AFVADJ (YRS)
	 	 	        nameplate(igp,icl,yrs,ildv) = 1.0
                  endif
	 	 	    endif
!             If no new announced nameplates, determine whether the market has grown enough to warrant additional nameplates (ENDOGENOUS)
              else
                if (nameplate(igp,icl,yrs-1,ildv).gt.0.0) then
                  sales_per_nameplate = cafesales(igp,icl,yrs-1,ildv) / nameplate(igp,icl,yrs-1,ildv)
                  if (curcalyr.gt.epalyr+2.and.sales_per_nameplate.ge.(SALES_PER_MODEL(icl,igp)*nameplate_sensitivity(ildv))) then
                    nameplate(igp,icl,yrs,ildv) = nameplate(igp,icl,yrs-1,ildv) + 1.0
                  else
                    nameplate(igp,icl,yrs,ildv) = nameplate(igp,icl,yrs-1,ildv)
                  endif
                endif
              endif
	 	    enddo 
	 	  enddo
	    enddo
      endif
!     Calculate make/model availability based on above nameplate counts (projection) and nameplate counts from READNHTSA (history)
	  do igp=1,maxgroup
	    do icl=1,maxclass
		  do ildv=1,maxldv
            do iregn = 1, mnumcr-2
              mmavail(igp,icl,ildv,iregn,yrs) = 0.0
!             Last historical year: if there were sales, calculate mmavail
              if (curcalyr.eq.epalyr.and.ldv_sales(igp,icl,ildv,iregn,n).gt.0.0) then
                if (ildv.eq.1) then
                  mmavail(igp,icl,ildv,iregn,yrs) = 1.0
                else
                  mmavail(igp,icl,ildv,iregn,yrs) = nameplate(igp,icl,epalyr,ildv)/sum(nameplate(igp,icl,epalyr,1:maxldv))
                endif
!             All projection years: calculate mmavail (no sales available yet, haven't run choice model)
              elseif (curcalyr.gt.epalyr) then
                if (ildv.eq.1.and.nameplate(igp,icl,yrs,ildv).gt.0.0) then
                  mmavail(igp,icl,ildv,iregn,yrs) = 1.0
                else
                  if (sum(nameplate(igp,icl,yrs,1:maxldv)).gt.0.0) then
                    mmavail(igp,icl,ildv,iregn,yrs) = nameplate(igp,icl,yrs,ildv)/sum(nameplate(igp,icl,yrs,1:maxldv))
		          endif
                endif
              endif
!              if (curcalyr.ge.2023.and.curcalyr.le.2024) WRITE(21,'(a,6(i4,","),2(f12.4,","))')'mmavail_debug',curcalyr,curitr,igp,icl,ildv,iregn,mmavail(igp,icl,ildv,iregn,yrs),nameplate(igp,icl,epalyr,ildv)
            enddo
          enddo
		enddo
	  enddo
	endif

!...calculate vehicle foot print
	if(curcalyr.ge.2010.and.curcalyr.le.epalyr) then
	  do igp=1,maxgroup
		do icl=1,maxclass
		  fprint(icl,igp,n) = 0.0
		  num = 0.0		  
		  do ildv=1,maxldv 
			if(FemMpg(igp,icl,yrs,ildv).ne.0.0) then 
			  num = num + fprt(igp,icl,yrs,ildv) * cafesales(igp,icl,yrs,ildv)
			endif 
		  enddo
		  if(sum(cafesales(igp,icl,yrs,1:maxldv)).ne.0.0) then
		    fprint(icl,igp,n) = num/sum(cafesales(igp,icl,yrs,1:maxldv)) 
		  endif
		enddo 
	  enddo 
	endif
!...hold last year fprint constant for projection years
	if(curcalyr.gt.epalyr) then
	  do igp=1,maxgroup
		do icl=1,maxclass
		  fprint(icl,igp,n) = fprint(icl,igp,n-1) 
		enddo 
	  enddo 	
	endif 
	
! ******************************** TESTING ONLY ********************************
!     IF (CURITR.EQ.1) THEN
!      DO ILDV = 1,16
!       WRITE(6,2999)YRS,ILDV,(CLASSFLAG(ICL,1,ILDV),ICL=1,6),(CLASSFLAG(ICL,3,ILDV),ICL=1,6)
!      ENDDO
!2999 FORMAT(1x,2I4,'CAR=',6A7,' TRK=',6A7)
!     ENDIF
! ******************************** TESTING ONLY ********************************

! ... FEM is now a three pass module in instances where CAFE is not met.
! ... The second pass has not changed and will still try to adopt technology
! ... sufficient to meet CAFE demand while also maintaining econometric and
! ... tech-driven HP increases.  If CAFE is still not met after the second
! ... pass, then the HP increases will be "backed out" and converted to
! ... equivalent FE on the third pass (in effect, assuming manufacturers
! ... will minimize their costs by complying with CAFE to the maximum extent
! ... possible before pushing additional HP increases "out the door."  This
! ... also means that calls to the NHTSA calibration routine are not performed
! ... until after the third pass.

      do IGP=1,MAXGROUP
       CafePass(IGP)= .false.
      end do

!...  Set fuel cost and income.  Lagged fuel costs (up to 7 years) and income (up to 1 year) are used in FEM calcs, so these parameters must
!...  be set for years leading up to the FEM base year.  Simply setting fuel cost and income for each year from the TRAN base year on provides 
!...  all the lag data necessary.  
      do iregn = 1,mnumcr
        PMGTR90_D_(iregn,YRS) = (PMGTR(iregn,N)*MG_HHV/1000.0) * MC_JPGDP(1)
	  enddo

      pass=1
        do IGP=1,MAXGROUP
         RegCost(IGP)=0.0
        end do
        if(curcalyr.gt.xyr) CALL FEMCALC
        CALL CGSHARE
        CALL TREG
        CALL TLDV
        CALL CAFECALC(0)
      pass=2
! ... And now through every one of the increments...
! ... Implement the fine incrementally on this pass.
       npass2=10
       do pass2=1,npass2
         do IGP=1,MAXGROUP
          RegCost(IGP)=Reg_Cost*(float(pass2)/float(npass2))
         end do
         if(curcalyr.gt.xyr) CALL FEMCALC
         CALL CGSHARE
         CALL TREG
         CALL TLDV
         CALL CAFECALC(0)
       end do
      pass=3
! ... In this pass, the value of RegCost is what it was from the previous pass. But we need
! ... to calculate an increment that goes with the HP giveback.
! ... Do the HP giveback incrementally. Determine the fraction for each increment.
       npass3=10
       do pass3=1,npass3
        if(yrs.ge.2022.and.FRZNEFF.eq.1)cycle
        GBInc=float(pass3)/float(npass3)
        do IGP=1,MAXGROUP
         RegCost(IGP)=Reg_Cost
        end do
        if(curcalyr.gt.xyr) CALL FEMCALC
        CALL CGSHARE
        CALL TREG
        CALL TLDV
        CALL CAFECALC(0)
       end do

!...call cafetest if the market is still out of compliance with CAFE and/or EPA GHG 
	if(curcalyr.gt.epalyr+1) then
      first_time_cafetest = .true.
      cafepass(:) = .true.
	  do igp=1,maxgroup
        ivtyp = GrpMap(igp)
        
!       If enforcing CAFE and EPA GHG
        if (RUN_EPA.eq.1) then 
!         If the aggregate market is out of compliance
          if (sum(MgGhgGrp(:,n)).lt.0.0.or.NewMPG(3,n).lt.cafestd(3,n)) then
!           Determine whether it's aggregate car or truck that is out of compliance (or both), and flag the individual groups that are out
            if (ivtyp.eq.1.and.(NewMpg(ivtyp,n).lt.cafestd(ivtyp,n).or.sum(MgGhgGrp(1:cargrp,n)).lt.0.0)) then
              if (MgGhgGrp(igp,n).lt.0.0.or.CafeMpgGrp(igp,n).lt.Cafe_Used(igp,yrs)) then
                cafepass(igp) = .false.
              endif
            elseif (ivtyp.eq.2.and.(NewMpg(ivtyp,n).lt.cafestd(ivtyp,n).or.sum(MgGhgGrp(ltkgrp:maxgroup,n)).lt.0.0)) then
              if (MgGhgGrp(igp,n).lt.0.0.or.CafeMpgGrp(igp,n).lt.Cafe_Used(igp,yrs)) then
                cafepass(igp) = .false.
              endif
            endif
!         If the aggregate market is in compliance, but said compliance would require transferring more than the maximum allowed mpg
!         b/w car/truck (2mpg), flag all the groups that are out of compliance          
          elseif ((NewMPG(ivtyp,n)-cafestd(ivtyp,n)).lt.-2.0.and.CafeMpgGrp(igp,n).lt.Cafe_Used(igp,yrs)) then
            cafepass(igp) = .false.
          endif
!       If not enforcing EPA (CAFE only)
        else
!         If the aggregate market is out of compliance
          if (NewMPG(3,n).lt.cafestd(3,n)) then
!           Determine whether it's aggregate car or truck that is out of compliance (or both), and flag the individual groups that are out
            if (ivtyp.eq.1.and.(NewMpg(ivtyp,n).lt.cafestd(ivtyp,n))) then
              if (CafeMpgGrp(igp,n).lt.Cafe_Used(igp,yrs)) then
                cafepass(igp) = .false.
              endif
            elseif (ivtyp.eq.2.and.(NewMpg(ivtyp,n).lt.cafestd(ivtyp,n))) then
              if (CafeMpgGrp(igp,n).lt.Cafe_Used(igp,yrs)) then
                cafepass(igp) = .false.
              endif
            endif
!         If the aggregate market is in compliance, but said compliance would require transferring more than the maximum allowed mpg
!         b/w car/truck (2mpg), flag all the groups that are out of compliance          
          elseif ((NewMPG(ivtyp,n)-cafestd(ivtyp,n)).lt.-2.0.and.CafeMpgGrp(igp,n).lt.Cafe_Used(igp,yrs)) then
            cafepass(igp) = .false.
          endif
        endif

!       Exotics don't need to comply; non-compliance is priced in        
        if(igp.eq.5) cafepass(igp) = .true.
        
      enddo
      
      do igp=maxgroup,1,-1
        if (.not.cafepass(IGP)) then
          if(fcrl.eq.1) WRITE(21,'(a,i2,a,i4)')'Entering CAFETEST for group ',igp,' in ',curcalyr
          call CAFETEST
        endif
      enddo

	endif

!   Detailed outputs -- final shares by regn, grp, icl, ildv
!    if (n.eq.mnumyr.and.fcrl.eq.1) then
!      WRITE(21,*)'choice_model_output'    
!      WRITE(21,*)'var,year,regn,grp,icl,ildv,val'
!      do i2 = 34, mnumyr
!        do iregn = 1, mnumcr
!          if (iregn.ge.10) CYCLE
!          do igp=1,maxgroup
!            do icl=1,maxclass
!              do ildv = 1,maxldv
!                if (ildv.ge.8.and.ildv.le.13) CYCLE
!                if (mmavail(igp,icl,ildv,iregn,i2+1989).eq.0.0) CYCLE
!                WRITE(21,'(a,",",5(i4,","),f10.3)')'mmavail',i2+1989,iregn,igp,icl,ildv,mmavail(igp,icl,ildv,iregn,i2+1989)
!                WRITE(21,'(a,",",5(i4,","),f10.1)')'batt_kwh',i2+1989,iregn,igp,icl,ildv,BatPackSize(i2+1989,icl,igp,ildv)
!                WRITE(21,'(a,",",5(i4,","),f10.6)')'fuel_cost',i2+1989,iregn,igp,icl,ildv,FLCOST(igp,ildv,icl,iregn,i2+1989)
!                WRITE(21,'(a,",",5(i4,","),f10.2)')'mpg',i2+1989,iregn,igp,icl,ildv,femmpg(igp,icl,i2+1989,ildv)
!                WRITE(21,'(a,",",5(i4,","),f10.6)')'hpwgt',i2+1989,iregn,igp,icl,ildv,ACCL(igp,ildv,icl,iregn,i2+1989)
!                WRITE(21,'(a,",",5(i4,","),f10.2)')'msrp',i2+1989,iregn,igp,icl,ildv,PSPR(igp,ildv,icl,iregn,i2+1989)
!                WRITE(21,'(a,",",5(i4,","),f10.6)')'mkt_share',i2+1989,iregn,igp,icl,ildv,APShrGrp(igp,icl,iregn,ildv,i2)
!                WRITE(21,'(a,",",5(i4,","),f10.3)')'sales_thou',i2+1989,iregn,igp,icl,ildv,ldv_sales(igp,icl,ildv,iregn,i2)*1000
!                WRITE(21,'(a,",",5(i4,","),f10.1)')'range',i2+1989,iregn,igp,icl,ildv,vrng(igp,ildv,icl,iregn,i2+1989)
!                WRITE(21,'(a,",",5(i4,","),f10.6)')'luggage',i2+1989,iregn,igp,icl,ildv,LUGG(igp,ildv,icl,iregn)
!                WRITE(21,'(a,",",5(i4,","),f10.6)')'fuel_avail',i2+1989,iregn,igp,icl,ildv,FAVL(ildv,iregn,i2+1989)
!                WRITE(21,'(a,",",5(i4,","),f10.4)')'atvcoef',i2+1989,iregn,igp,icl,ildv,x210(igp,icl,ildv,iregn)
!                WRITE(21,'(a,",",5(i4,","),f10.1)')'nameplate',i2+1989,iregn,igp,icl,ildv,nameplate(igp,icl,i2+1989,ildv)
!                WRITE(21,'(a,",",5(i4,","),i10)')'grpflag',i2+1989,iregn,igp,icl,ildv,GRPFLAG(ILDV,ICL,IGP)
!              enddo
!            enddo
!          enddo
!        enddo
!      enddo
!      
!    endif

    RETURN
    END SUBROUTINE TMPGNEW

! ==========================================================================================================
! ... Subroutine FEMCALC determines the cost effective market shares of technologies for each vehicle class 
! ... and then calculates the resulting fuel economy, weight, horsepower, and price.
! ========================================================================================================== 
    SUBROUTINE FEMCALC
    USE T_
    IMPLICIT NONE

      REAL          FIVEYR_FUELCOST(2)
      REAL          SYNERGY_LOSS(MAXTECH)
      LOGICAL*1     MIN_ADJ
      REAL          TECH_ADJHP,PERF_ADJHP,TTL_ADJHP,PERF_COEFF,DEMAND_USED
      REAL          TEMP_HP,HP_WGT,HP_WGT_MIN,HP_WGT_BASE
      REAL          EXCESS_ADJHP,HP_GIVEBACK
      REAL          MIN_ADJHP,NEED_ADJHP
      REAL          PHASEIN,PHASESHR,CLASSSHR,UPPER_BUFFER,LOWER_BUFFER
      REAL          MKT_COEFF,TECH_MAX_ADJHP,VCW_ADJ
      REAL          MAX_TAKE_EXP
      REAL          LEARNYEAR(maxtech,MAXGROUP)                            !...year when learning curve is available for each technology


! ... Sumcheck holds the IDs of techs that together should not sum to more than one.
! ... It is formatted to allow up to 15 IDs for a single check, with up to 20 checks
! ... total.  Additional checks can be accomodated by either adding new IDs to unused
! ... array elements or increasing the second array dimension (and the subsequent loop
! ... index parameter accordingly.  Values of -9 indicate "No Data."

      INTEGER       SUMCHECK(15,20)
      REAL          CHECKSUM
      REAL FUNCMAX
      EXTERNAL FUNCMAX
	  DATA SUMCHECK / 1,  2,  3,  4,  5, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 01 IDs (material substitution)
                      6,  7,  8,  9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 02 IDs (aero/drag reduction)
                     10, 11, 12, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 03 IDs (tires)
                     13, 14, 15, 16, 17, 18, 19, 20, 21, 22, -9, -9, -9, -9, -9, &   !Check 04 IDs (non-DCT transmissions)
                     13, 14, 17, 18, 19, 20, 21, 22, -9, -9, -9, -9, -9, -9, -9, &   !Check 05 IDs (auto transmission)
                     15, 16, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 06 IDs (CVT transmission)
                     23, 24, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 07 IDs (DCT transmission)
                     25, 26, 27, 28, 29, 30, 31, 32, 33, -9, -9, -9, -9, -9, -9, &   !Check 08 IDs (SOHC)
                     34, 35, 36, 37, 38, 39, 40, 41, 42, -9, -9, -9, -9, -9, -9, &   !Check 09 IDs (DOHC)
                     34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, &   !Check 10 IDs (DOHC & turbo)
                     43, 44, 45, 46, 47, 48, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 11 IDs (turbo)
                     49, 50, 51, 52, 53, 54, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 12 IDs (turbo 1&2)
                     43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, -9, -9, -9, &   !Check 13 IDs (all turbo)
                     55, 56, 57, 58, 59, 60, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 14 IDs (HCR)
					 61, 62, 63, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 15 IDs (ADEAC) 
					 55, 56, 57, 58, 59, 60, 61, 62, 63, -9, -9, -9, -9, -9, -9, &   !Check 16 IDs (HCR & ADEAC)
                     49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, &   !Check 17 IDs (turbo 1&2 & HCR)
                     49, 50, 51, 52, 53, 54, 61, 62, 63, -9, -9, -9, -9, -9, -9, &   !Check 18 IDs (turbo 1&2 & ADEAC)
                     64, 65, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 19 IDs (SS12V & BISG)
                     66, 67, 68, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9/     !Check 20 IDs (CAV 1-3)	  
                     
      IF (PASS .EQ. 1) TEC_ORNL = 0.0

!...  If this is the FEM base year or earlier, jump over the core FEM calculations.  However, there are several periperhal calculations that should 
!...  be implemented even for pre-FEM estimate years.  Such calculations generally involve setting up lagged parameters and validating historic year
!...  inputs.  For example, the calculation of vehicle range and other NEMS statistics requires the definition of FEM "current year" data regardless of 
!...  whether or not the current year is before or after the FEM base year.  For years up to the FEM base year, these data must first be extracted from
!...  the report writer or base year arrays since years through the FEM base year (XYR) are never actually processed through the main portion of FEMCALC.

!... On the first pass of the first TRAN iteration for ALL years, all "current year" data (which are actually data from the final pass/iteration for the
!... previous year for all years after the FEM base year) must be copied into the "previous year" arrays so that they are available as the basis for the
!... new evaluation year updates.  This assignment is required even for years prior to the FEM base year to provide for cross-iteration stability (even 
!... pre-base year data can be altered through calibration).

      IF(CURITR .EQ. 1 .AND. PASS .EQ. 1) THEN
!...    years > xyr
		if(yrs.gt.xyr) then
!...	  set previous year data only once for each evaluation year	to provide necessary cross-iteration stability	
          DO ILDV=1,MAXLDV 
            DO IGP=1,MAXGROUP                          
              DO ICL=1,MAXCLASS
			    if(classflag(icl,igp,ildv)) then
                  FE(ICL,IGP,PREV,ILDV)       = FE(ICL,IGP,CURRENT,ILDV)
                  WEIGHT(ICL,IGP,PREV,ILDV)   = WEIGHT(ICL,IGP,CURRENT,ILDV)
                  PRICE(ICL,IGP,PREV,ILDV)    = PRICE(ICL,IGP,CURRENT,ILDV)
                  HP(ICL,IGP,PREV,ILDV)       = HP(ICL,IGP,CURRENT,ILDV)
                  TANKSIZE(ICL,IGP,PREV,ILDV) = TANKSIZE(ICL,IGP,CURRENT,ILDV)
                  RANGE(ICL,IGP,PREV,ILDV)    = RANGE(ICL,IGP,CURRENT,ILDV)

                  if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) ElecSysIncCost(ICL,IGP,PREV,ILDV) = ElecSysIncCost(ICL,IGP,CURRENT,ILDV)

                  DO ITECH=1,NUMTECH
                    MKT_PEN(ICL,IGP,ITECH,PREV,ILDV) = MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV)
                  ENDDO
				endif
              ENDDO
            ENDDO
          ENDDO
		endif
      ENDIF
  
!...  For each technology, the expected fuel savings associated with incremental fuel economy impacts is calculated.  This calculation occurs
!...  below within a technology evaluation loop, but the fuel costs on which the calculation is dependent are fixed annually so that continual 
!...  recalculation within the technology loop is redundant.  Accordingly, the basic fuel cost calculations are included here.  Nominally, fuel
!...  costs three years ago and the annual rate of fuel price change are used to estimate expected dollar savings.  However, since prices can 
!...  spike and since manufacturing decisions will not be based on one-year spikes, the "three year ago" and "rate of change" prices used for this
!...  calculation are actually the "five year running average price" and the "difference between the three year ago five year average price and 
!...  the four year ago five year average price."  Thus, the effect of short term transients is buffered.
      IF(YRS .GT. XYR) THEN              ! lagged parameters are not needed until after the FEM base year
        FIVEYR_FUELCOST = 0.0
        DO IYR=(YRS-8),(YRS-4)
          FIVEYR_FUELCOST(2) = FIVEYR_FUELCOST(2) + PMGTR90_D_(11,IYR)
        ENDDO
        FIVEYR_FUELCOST(1) = FIVEYR_FUELCOST(2) - PMGTR90_D_(11,YRS-8) + PMGTR90_D_(11,YRS-3)
        FIVEYR_FUELCOST(1) = FIVEYR_FUELCOST(1) / 5.0
        FIVEYR_FUELCOST(2) = FIVEYR_FUELCOST(2) / 5.0
        PSLOPE = MAX(0.0,FIVEYR_FUELCOST(1)-FIVEYR_FUELCOST(2))
      ENDIF

!...  Initiate FEM processing loop (loop through each fuel type, vehicle group, vehicle class, and fuel economy technology).
      DO ILDV=1,MAXLDV
        DO IGP=1,MAXGROUP

!...    In the 1st call to FEMCALC, CAFEPASS(IGP) should always be FALSE (i.e., none of the nine CAFE groups have demonstrated compliance).  In 
!...	subsequent calls values may be TRUE or FALSE depending on the compliance status of each CAFE group.
!...      For GHG CAFE

          if(CAFEMY27_SWITCH.eq.1) then
            PAYBACK = 8 ! 6.5
            DISCOUNT = 0.05 !0.075
          else
            PAYBACK = 3
            DISCOUNT = 0.1
          endif

          IF(pass.ge.2.and.CAFEPASS(IGP)) CYCLE

          DO ICL=1,MAXCLASS
            IF(.NOT. CLASSFLAG(ICL,IGP,ILDV)) CYCLE    ! skip loop if no vehicles in the class
!...        Set vehicle type index for correct reference to arrays that are type, rather than group specific.
            IVTYP=GrpMap(IGP)
!...        Set the initial estimates for each years fuel efficiency, weight, price,
!...        and fuel tank size equal to the previous year's values before
!...        considering new technologies.

            FE(ICL,IGP,CURRENT,ILDV)       = FE(ICL,IGP,PREV,ILDV)
            WEIGHT(ICL,IGP,CURRENT,ILDV)   = WEIGHT(ICL,IGP,PREV,ILDV)
            PRICE(ICL,IGP,CURRENT,ILDV)    = PRICE(ICL,IGP,PREV,ILDV)
            HP(ICL,IGP,CURRENT,ILDV)       = HP(ICL,IGP,PREV,ILDV)
            TANKSIZE(ICL,IGP,CURRENT,ILDV) = TANKSIZE(ICL,IGP,PREV,ILDV)

!...		Jump over core FEM processing if this is FEM base year or earlier, but perform market share sum checks for related
!...		technologies to ensure reliable input data.  Note also that this jump could be performed prior to the attribute 
!...		resets (i.e. CURRENT = PREV) above, but the resets are run first to make data in and out of FEM "perfectly" stable 
!...		across iterations.  Output would be stable either way (as calibration factors "go to one" for 2nd and later iterations),
!... 		but this approach makes debugging easier/cleaner.
            IF(YRS .LE. XYR) GO TO 1100  

!...		For electric power train vehicles subtract last year's electric storage cost from vehicle price.
            if((ILDV.ge.4.and.ILDV.le.8).or.ILDV.ge.13) then
			  if(price(icl,igp,current,ildv).ne.0.0) then
                price(ICL,IGP,CURRENT,ILDV) = price(ICL,IGP,CURRENT,ILDV)-ElecSysIncCost(ICL,IGP,PREV,ILDV)
!                if (ElecSysIncCost(ICL,IGP,PREV,ILDV).le.0.0) WRITE(21,'(a,5(i4,","),f12.2)')'check: ElecSysIncCost',curcalyr,curitr,icl,igp,ildv,ElecSysIncCost(ICL,IGP,PREV,ILDV)
              endif
            endif

!...		Calculate possible market share in the absence of any engineering notes

!... 		Initialize the value of PERF_COEFF, the parameter used to constrain the incremental value of additional vehicle performance.  
!...		This parameter, which is independent of technology and can thus be set prior to beginning the main technology evaluation loop,
!...		increases as performance increases so that the incremental value of additional performance declines.  Since the value of
!...		performance is based on 1990 data, the consumer performance demand function also uses a base of 1990.  However, since the base 
!...		data year is XYR, the demand that has already accrued must be accounted for through the use of parameter USEDCAP.
            IF(WEIGHT(ICL,IGP,BASE,ILDV) .NE. 0.0) THEN
              HP_WGT_BASE = HP(ICL,IGP,BASE,ILDV) / WEIGHT(ICL,IGP,BASE,ILDV)
            ELSE
              WRITE(*,*) 
              WRITE(*,*) ' Divisor in FEMCALC PERF_COEFF calc equals zero - RUN ABORTED.'
              WRITE(*,*)
              WRITE(*,*) ' --- At ABORT, index parameters were:'
              WRITE(*,*)
              WRITE(*,*) '   YRS   = ',YRS
              WRITE(*,*) '   ICL   = ',ICL
              WRITE(*,*) '   IGP   = ',IGP
              WRITE(*,*) '   ILDV = ',ILDV
              WRITE(*,*)
              WRITE(*,*) ' --- the offending denominator was:'
              WRITE(*,*)
              WRITE(*,*) '   WEIGHT(ICL,IGP,BASE,ILDV) = ',WEIGHT(ICL,IGP,BASE,ILDV)
              WRITE(*,*)
              WRITE(*,*) ' --- and the associated numerator was:'
              WRITE(*,*)
              WRITE(*,*) '   HP(ICL,IGP,BASE,ILDV)     = ',HP(ICL,IGP,BASE,ILDV)
			  WRITE(*,*) '   ClassFlag(ICL,IGP,ILDV)   = ',classflag(icl,igp,ildv)
              STOP 505
            ENDIF

            IF(WEIGHT(ICL,IGP,CURRENT,ILDV) .NE. 0.0) THEN
              HP_WGT = HP(ICL,IGP,CURRENT,ILDV) / WEIGHT(ICL,IGP,CURRENT,ILDV)
            ELSE
              WRITE(*,*) 
              WRITE(*,*) ' Divisor in FEMCALC PERF_COEFF calc equals zero - RUN ABORTED.'
              WRITE(*,*)
              WRITE(*,*) ' --- At ABORT, index parameters were:'
              WRITE(*,*)
              WRITE(*,*) '   YRS   = ',YRS
              WRITE(*,*) '   ICL   = ',ICL
              WRITE(*,*) '   IGP   = ',IGP
              WRITE(*,*) '   ILDV = ',ILDV
              WRITE(*,*)
              WRITE(*,*) ' --- the offending denominator was:'
              WRITE(*,*)
              WRITE(*,*) '   WEIGHT(ICL,IGP,CURRENT,ILDV) = ',WEIGHT(ICL,IGP,CURRENT,ILDV)
              WRITE(*,*)
              WRITE(*,*) ' --- and the associated numerator was:'
              WRITE(*,*)
              WRITE(*,*) '   HP(ICL,IGP,CURRENT,ILDV)     = ',HP(ICL,IGP,CURRENT,ILDV)
              STOP 506
            ENDIF

            IF(USEDCAP(ICL,IGP) .GE. 0.0 .AND. USEDCAP(ICL,IGP) .LT. 1.0) THEN
              DEMAND_USED = (PERFCAP(ICL,IGP) - HP_WGT_BASE) * (USEDCAP(ICL,IGP)/(1.0-USEDCAP(ICL,IGP)))
            ELSE
              WRITE(*,*) 
              WRITE(*,*) ' Divisor in FEMCALC PERF_COEFF calc is out-of-range - RUN ABORTED.'
              WRITE(*,*)
              WRITE(*,*) ' --- At ABORT, index parameters were:'
              WRITE(*,*)
              WRITE(*,*) '   YRS   = ',YRS
              WRITE(*,*) '   ICL   = ',ICL
              WRITE(*,*) '   IGP   = ',IGP
              WRITE(*,*) '   ILDV = ',ILDV
              WRITE(*,*)
              WRITE(*,*) ' --- the offending parameter (USEDCAP), which must be greater'
              WRITE(*,*) '     than or equal to zero and less than one, was:'
              WRITE(*,*)
              WRITE(*,*) '   USEDCAP(ICL,IGP) = ',USEDCAP(ICL,IGP)
              STOP 508
            ENDIF

            IF(PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED .NE. 0.0) THEN
              PERF_COEFF  = 1.0 -((HP_WGT-HP_WGT_BASE+DEMAND_USED)/(PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED))
              PERF_COEFF  = MIN(1.0,PERF_COEFF)
              PERF_COEFF  = MAX(0.0,PERF_COEFF)
            ELSE
              WRITE(*,*) 
              WRITE(*,*) ' Divisor in FEMCALC PERF_COEFF calc equals zero.'
              WRITE(*,*)
              WRITE(*,*) ' --- Index parameters at the time of this disconcerting occurrence were:'
              WRITE(*,*)
              WRITE(*,*) '   YRS   = ',YRS
              WRITE(*,*) '   ICL   = ',ICL
              WRITE(*,*) '   IGP   = ',IGP
              WRITE(*,*) '   ILDV = ',ILDV
              WRITE(*,*)
              WRITE(*,*) ' --- the offending denominator was:'
              WRITE(*,*)
              WRITE(*,*) '   PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED = ',PERFCAP(ICL,IGP) - HP_WGT_BASE  + DEMAND_USED
              WRITE(*,*)
              WRITE(*,*) ' --- and the associated numerator was:'
              WRITE(*,*)
              WRITE(*,*) '   HP_WGT-HP_WGT_BASE+DEMAND_USED           = ',HP_WGT - HP_WGT_BASE + DEMAND_USED

              IF(CURIYR .GT. 1) THEN  ! Post 2030 keep the model running
                 IF (PERFCAP(ICL,IGP) .EQ. HP_WGT_BASE) THEN
                    PERF_COEFF = 0.01
                    PERFCAP(ICL,IGP) = PERFCAP(ICL,IGP) + 0.01
                    WRITE(*,*) 
                    WRITE(*,*) ' Divisor in FEMCALC PERF_COEFF calc equals zero -- RUN NOT ABORTED.'
                    WRITE(*,*) ' (Rather than make a big scene maybe i can get something to work.)' 
                    WRITE(*,*)
                    
                 ENDIF
              ELSE
                 WRITE(*,*) 
                 WRITE(*,*) ' Divisor in FEMCALC PERF_COEFF calc equals zero -- RUN ABORTED.'
                 WRITE(*,*)
                 STOP 507
              ENDIF
            ENDIF

!...		The following initialization statements are used to define several parameters that are later referenced in determining price, 
!...		fuel economy, hp, etc.  Since some technologies are skipped under certain conditions, the affected parameters might otherwise
!...		be undefined (or worse, retain values from previous iterations).
            DO ITECH=1,NUMTECH
              MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) = MKT_PEN(ICL,IGP,ITECH,PREV,ILDV)
              ACTUAL_MKT(ITECH) = MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV)
              MMAX(ITECH) = MKT_MAX(ICL,IGP,ITECH,ILDV)
              MKT_PERF(ITECH) = 0.0
              MKT_FUEL(ITECH) = 0.0
              ACTUAL_MKT(ITECH) = 0.0

!...		  Skip non-applicable technologies.
              IF(.NOT. TECH_APPLIC(ITECH,IVTYP,ILDV)) CYCLE

!...		  Set tech penetration limiting parameter (OLD_PMAX).  Penetration curve should be accelerated if CAFE is not met, therefore OLD_PMAX
!...		  should be set (updated) on both the first and second passes through FEM.  No further acceleration is appropriate on pass three, 
!...		  which simply "trades" performance gains for fuel economy.  Pass three constraints are handled further on in the code by restricting
!...		  updates to OLD_PMAX.  OLD_PMAX must also be reset for each iteration, so OLD_PMAX(...,1) is used to hold the first iteration/first pass
!...		  value for each year so that it can be restored as necessary.  Within year processing is controlled via OLD_PMAX(...,2).
              IF(CURITR .EQ. 1 .AND. PASS .EQ. 1) THEN    ! 1st iter/1st pass -- store reset value (last value for year-1)
                OLD_PMAX(ICL,IGP,ITECH,ILDV,1) = OLD_PMAX(ICL,IGP,ITECH,ILDV,2)
              ELSEIF(PASS .EQ. 1) THEN                    ! subsequent iter/1st pass -- restore 1st iter/1st pass value
                OLD_PMAX(ICL,IGP,ITECH,ILDV,2) = OLD_PMAX(ICL,IGP,ITECH,ILDV,1)
              ENDIF

              IF(YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE

! ************************************ TESTING ONLY ******************************
!   IF (CURITR.EQ.1.AND.N.GE.16.AND.ILDV.EQ.1.AND.ICL.EQ.3.AND.IGP.LE.2.AND.PASS.GE.1.AND.ITECH.EQ.2)&
!    WRITE(6,*)'PASS-MKT-PMAX1-PMAX2',PASS, MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV), &
!           OLD_PMAX(ICL,IGP,ITECH,ILDV,1), OLD_PMAX(ICL,IGP,ITECH,ILDV,2)
! ************************************ TESTING ONLY ******************************

!... 		  Calculate expected fuel savings associated with incremental fuel economy.
              FUELSAVE(ITECH) = 0.0
              CFE = FE(ICL,IGP,PREV,ILDV)

!...		  Use the last VMT schedule available (STOCKYR). Assume gas ICE VMT (need consistent assumption)
              DO I=1,PAYBACK
                if(IGP.le.cargrp) VMT(I,mnumcr)=PVMT(I,STOCKYR-1989,mnumcr,1)	
                if(IGP.ge.ltkgrp) VMT(I,mnumcr)=LVMT(I,STOCKYR-1989,mnumcr,1)
				
                PRICE_EX(I) = PSLOPE * (I+2) + FIVEYR_FUELCOST(1)
                IF(CFE .NE. 0.0) FUELSAVE(ITECH) = FUELSAVE(ITECH)+VMT(I,mnumcr) *          &
                                                   (1/CFE - (1/((1+DEL_FE(ITECH,IVTYP))*CFE))) * &
                                                    PRICE_EX(I) * (1+DISCOUNT)**(-I)	
              ENDDO

!...          calculate incremental technology cost of specific technology starting in base year technology
!...          update: this year flag must be updated if technology base year attributes are set to a year other than 2010
!...          absolute technology cost
              TECHCOST(ITECH) = DEL_COSTABS(ITECH,IVTYP)            
!...          absolute weight-based technology cost
              SIGN = 1
              if(DEL_WGTABS(ITECH,IVTYP) .lt. 0.0) SIGN = -1       
              TECHCOST(ITECH) = TECHCOST(ITECH) +  &
                                (DEL_COSTWGT(ITECH,IVTYP) * DEL_WGTABS(ITECH,IVTYP) * SIGN)
!...          weight-based technology cost
              SIGN = 1
              if(DEL_WGTWGT(ITECH,IVTYP) .lt. 0.0) SIGN = -1       
              TECHCOST(ITECH) = TECHCOST(ITECH) +  &
                                (DEL_COSTWGT(ITECH,IVTYP) *  &
                                 DEL_WGTWGT(ITECH,IVTYP) * SIGN * WEIGHT(ICL,IGP,CURRENT,ILDV))              
                
!...          skip if technology cost is 0 
              if(TECHCOST(ITECH) .lt. 0.0) GO TO 500
              
!...          apply time-based learning to technology cost
              do i=1,4
                LEARN_COST_MULTIPLIER(i) = 1.0
              enddo 
!...          determine where technology is along learning % reduction path
              if(frstyear(itech,IGP).lt.2011)then
                learnyear(itech,IGP)=2011  ! UPDATE? JMA
              else
                learnyear(itech,IGP)=frstyear(itech,IGP)
              endif
!...          set array position
              YEARS_MKTD = YRS - learnyear(ITECH,IGP) + 1
!...          apply technology learning
              if(YEARS_MKTD.ge.1)then    
                LEARN_COST_MULTIPLIER(2)= 1.0 + (COEFF_LRN1(ITECH,IVTYP)*COEFF_LEARN(years_mktd,1))
                LEARN_COST_MULTIPLIER(3)= 1.0 + (COEFF_LRN2(ITECH,IVTYP)*COEFF_LEARN(years_mktd,2))
              endif
 
              if(TECHCOST(ITECH) .gt. 0.0) TECHCOST(ITECH) =  TECHCOST(ITECH) * &
                                                              LEARN_COST_MULTIPLIER(1) * &
                                                              LEARN_COST_MULTIPLIER(2) * &
                                                              LEARN_COST_MULTIPLIER(3) * &
                                                              LEARN_COST_MULTIPLIER(4)                

!... 		  Save technology cost for use in subroutine TLEGIS.
  500         TEC_ORNL(ICL,IGP,ITECH,ILDV) = TECHCOST(ITECH)

!...		  Estimate the value of performance associated with the technology.  Scale the the value of performance 
!...		  downward (using PERF_COEFF) as HP/WGT increases to reflect the decreasing incremental value of more performance.
              VAL_PERF(ITECH) = 0.0
              IF(CFE .NE. 0.0) VAL_PERF(ITECH) = VALUEPERF(ICL,IGP)*PERF_COEFF*INC90_D_NP(11,YRS)/INC90_D_NP(11,YRS-1) *       &
                                                (1+DEL_FE(ITECH,IVTYP))*PMGTR90_D_(11,YRS-1)/PMGTR90_D_(11,YRS)*DEL_HP(ITECH,IVTYP)
!... 		  Calculate the cost effectiveness based on fuel savings and performance.
              IF(TECHCOST(ITECH) .GT. 0.0) THEN     ! if tech costs money, estimate cost effectiveness
                COSTEF_FUEL(ITECH) = (FUELSAVE(ITECH)-TECHCOST(ITECH)+REGCOST(IGP) * CFE * &
									  DEL_FE(ITECH,IVTYP))/TECHCOST(ITECH)

                COSTEF_PERF(ITECH) = -80.0

                IF (VAL_PERF(ITECH) .NE. 0.0) COSTEF_PERF(ITECH) = (VAL_PERF(ITECH)-TECHCOST(ITECH))/TECHCOST(ITECH)

                COSTEF_FUEL(ITECH) = MAX(-80.0,COSTEF_FUEL(ITECH))
                COSTEF_PERF(ITECH) = MAX(-80.0,COSTEF_PERF(ITECH))

              ELSE ! if tech has zero or negative cost, either 100% effective or ineffective based on savings/performance
                COSTEF_FUEL(ITECH) = -80.0
                IF (FUELSAVE(ITECH) .GT. 0.0) COSTEF_FUEL(ITECH) = 80.0
                COSTEF_PERF(ITECH) = -80.0
                IF (VAL_PERF(ITECH) .GT. 0.0) COSTEF_PERF(ITECH) = 80.0
              ENDIF

              MMAX(ITECH) = MKT_MAX(ICL,IGP,ITECH,ILDV)

              IF(PASS.EQ.1 .OR. (PASS.EQ.2 .AND. PASS2.EQ.1)) THEN
                OLD_PMAX(ICL,IGP,ITECH,ILDV,2) = &
                FUNCMAX(MKT_PEN(ICL,IGP,ITECH,PREV,ILDV),OLD_PMAX(ICL,IGP,ITECH,ILDV,2))
              ENDIF

!...		  Calculate the economic market share for fuel saving technology/performance. The cost effectiveness coefficient in the fuel
!...		  market penetration equation varies between -2 and -4 depending on whether cost effectiveness is greater than or less than zero.
!...		  Originally a value of -2 was set under both conditions, but at this value, even techs producing NO benefits derive a market 
!...		  penetration of 12 percent, REGARDLESS OF COST.  With a coefficient -4, this "no benefit" penetration drops to a more reasonable 
!...		  2 percent. Ideally, some measure of the absolute cost differential should be introduced into the algorithm since a consumer is
!...		  more likely to accept a cheap technology with poor payback than an expensive one (i.e., getting $10 back on a $20 investment is
!...		  more palatable than getting $1500 back on $3000 invested). To some extent, the implemented approach accomplishes this since fuel 
!...		  savings vary less than tech costs, making the high cost techs more likely candidates for lower relative cost effectiveness estimates.
              MKT_COEFF = -2.0
              IF(COSTEF_FUEL(ITECH) .LT. 0.0) MKT_COEFF = -4.0

              MKT_FUEL(ITECH) = 1/(1+EXP(MKT_COEFF*COSTEF_FUEL(ITECH)))

              MKT_COEFF = -2.0
              IF(COSTEF_PERF(ITECH) .LT. 0.0) MKT_COEFF = -4.0

              MAX_TAKE_EXP = MIN(MKT_COEFF*COSTEF_PERF(ITECH),75.0)
              MKT_PERF(ITECH) = 1/(1+EXP(MAX_TAKE_EXP))

!...		  Calculate the actual economic market share.  PMAX defines the fraction of vehicle makes and models on which the technology is available
!...		  and MKT_FUEL and MKT_PERF define the percentage of those model buyers who desire the technology from a cost effectiveness standpoint.
!...		  So the actual estimated market share is the product of these two influences, constrained by the maximum market share and "no backsliding."
              ACTUAL_MKT(ITECH) = OLD_PMAX(ICL,IGP,ITECH,ILDV,2) * MAX(MKT_FUEL(ITECH),MKT_PERF(ITECH))
              ACTUAL_MKT(ITECH) = MAX(MKT_PEN(ICL,IGP,ITECH,PREV,ILDV),ACTUAL_MKT(ITECH))
              ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),MMAX(ITECH),1.0)
            ENDDO   ! end technology (ITECH) loop

!... 		Apply mandatory and supersedes engineering notes.
            DO ITECH=1,NUMTECH
              IF(YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE
              DO INOTE=1,NUM_MAN      ! loop through mandatory notes
                IF(MANDYEAR(1,INOTE) .EQ. ITECH .AND. YRS .GE. MANDYEAR(2,INOTE)) THEN

!...			  If a non-econometric technology (i.e., MAND_ORIDE is TRUE), zero any econometrically calculated market share.
                  IF(MAND_ORIDE(INOTE)) ACTUAL_MKT(ITECH) = 0.0
!...			  If the number of phase-in years is between 0 and 1, adopt the full market share immediately.  Since the maximum market penetration
!...			  allowance can vary by vehicle class, the actual market share logic must consider the mandatory share, not in isolation, but in 
!...			  conjunction with the maximum allowable share for the vehicle class.
                  IF(MANDYEAR(3,INOTE) .LE. 1) THEN
                    ACTUAL_MKT(ITECH) = MAX(MANDMKSH(INOTE),ACTUAL_MKT(ITECH))
                    ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),MKT_MAX(ICL,IGP,ITECH,ILDV))
                  ELSE
!...			  If the number of phase-in years is greater than 1, adopt a proportional share of the total mandatory share each year.  Since both the
!...			  base and maximum market penetrations can vary by vehicle class, the actual market share logic must adopt annual shares in proportion
!...			  to the allowable market share spread for each vehicle class, with the minimum market share defined by the base share for the class.
                    PHASEIN  = MIN(1.0,(REAL(YRS-MANDYEAR(2,INOTE)))/REAL(MANDYEAR(3,INOTE)))
                    PHASESHR = MANDMKSH(INOTE) * PHASEIN
                    CLASSSHR = MKT_PEN(ICL,IGP,ITECH,BASE,ILDV) +     &
                              (PHASESHR*(MKT_MAX(ICL,IGP,ITECH,ILDV)- &
                                         MKT_PEN(ICL,IGP,ITECH,BASE,ILDV)))
                    ACTUAL_MKT(ITECH) = MAX(ACTUAL_MKT(ITECH),CLASSSHR)
                    ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),MKT_MAX(ICL,IGP,ITECH,ILDV))
                  ENDIF
                ENDIF
              ENDDO                   ! end mandatory note loop
		
              CALL NOTE_SUPER         ! process supersedes notes

              IF(RETURN_STAT .LT. 0) THEN
                RETURN_STAT = RETURN_STAT * (-1)
                I = MOD(RETURN_STAT,100)
                INOTE = RETURN_STAT/100
                WRITE (*,*)
                WRITE (*,*) '======================================'
                WRITE (*,*)
                WRITE (*,*) 'Logic Error in Supersedes Algorithm,'
                WRITE (*,*) 'Market Penetration is Less Than Zero !!!'
                WRITE (*,*)
                WRITE (*,*) 'Year             = ',YRS
                WRITE (*,*) 'Vehicle Group    = ',GROUPLABEL(IGP)
                WRITE (*,*) 'Vehicle Class    = ',CLASSLABEL(ICL,IGP)
                WRITE (*,*) 'Technology ID    = ',TECHLABEL(ITECH,IVTYP)
                WRITE (*,*) 'Mkt Penetration  = ',ACTUAL_MKT(ITECH)
                write (*,*) 'max mkt          = ',mmax(itech)
                write (*,*) 'mkt_max          = ',MKT_MAX(ICL,IGP,ITECH,ILDV)
                WRITE (*,*)
                WRITE (*,*) 'Superseded Market Penetrations are as follows:'
                WRITE (*,*) '(after algorithm short-circuit at tech def ',I,')'
                WRITE (*,*)
                DO J = 1,TECH_CNT(INOTE)
                WRITE (*,*) 'Technology ID    = ',TECHLABEL(SUPERSEDES(J,INOTE),IVTYP)
                WRITE (*,*) 'Mkt Penetration  = ',ACTUAL_MKT(SUPERSEDES(J,INOTE))
                WRITE (*,*)
                ENDDO
                WRITE (*,*) '  ***** Run ABORTED *****'
                WRITE (*,*)
                WRITE (*,*) 'Fix Program Logic and Rerun'
                WRITE (*,*)
                WRITE (*,*) '======================================'
                STOP
              ENDIF
            ENDDO   ! end technology (ITECH) loop for mandatory and supersedes notes

!...		loop through and apply required engineering notes
            DO ITECH=1,NUMTECH        ! loop through and apply required engineering notes
              IF(YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE
              REQUIRED = .FALSE.
              REQ_MKT  = 0.0
              DO INOTE=1,NUM_REQ
                IF(REQUIRES(1,INOTE) .EQ. ITECH) THEN
                  REQUIRED = .TRUE.
                  REQ_MKT  = REQ_MKT + MKT_PEN(ICL,IGP,REQUIRES(2,INOTE),CURRENT,ILDV)
                ENDIF
              ENDDO
              IF(REQUIRED) THEN
                REQ_MKT = MIN(REQ_MKT,1.0)
                ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),REQ_MKT)
              ENDIF
!...		  Note, this loop also very surreptitiously assigns the MKT_PEN value for all technologies.  It's highly important, but easy to miss
!...		  "hidden" within a loop otherwise "advertised" as processing required engineering notes.
              MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) = ACTUAL_MKT(ITECH)
            ENDDO   ! end technology (ITECH) loop for required notes

!...		Loop through and apply the synergy engineering notes
            DO ITECH=1,NUMTECH        
              IF(YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE
              SYNERGY_LOSS(ITECH) = 0.0
!...		  Market share affected by synergy effects between two technologies is estimated as the probabilistic overlap between the market shares
!...		  of the two technologies. Mathematically, this market share is expressed as the product of the market shares of the two technologies.  
!...		  The incremental market share overlap for a single year is equal to the cumulative estimated overlap (based on cumulative estimated market 
!...		  penetrations) for the current year minus the cumulative estimated overlap for the previous year.  Note also, that the input value of 
!...		  is negative so that the estimated synergy loss will also be negative and should be treated as an additive parameter.
              DO INOTE=1,NUM_SYN
                IF(SYNERGY(1,INOTE) .EQ. ITECH) THEN
                  DELTA_MKT = (MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) *             &
                               MKT_PEN(ICL,IGP,SYNERGY(2,INOTE),CURRENT,ILDV)) - &
                              (MKT_PEN(ICL,IGP,ITECH,PREV,ILDV) *                &
                               MKT_PEN(ICL,IGP,SYNERGY(2,INOTE),PREV,ILDV))
                
                  IF (DEL_FE(ITECH,IVTYP) .LT. ABS(SYNR_DEL(INOTE))) THEN
                    WRITE (*,*)
                    WRITE (*,*) '==============================================================='
                    WRITE (*,*)
                    WRITE (*,*) 'Logic Error in Synergy Algorithm,'
                    WRITE (*,*) 'Synergy Loss is Greater than Unadjusted Fuel Economy Impact !!!'
                    WRITE (*,*)
                    WRITE (*,*) 'Year             = ',YRS
                    WRITE (*,*) 'Vehicle Group    = ',GROUPLABEL(IGP)
                    WRITE (*,*) 'Vehicle Class    = ',CLASSLABEL(ICL,IGP)
                    WRITE (*,*) 'Technology ID 1  = ',TECHLABEL(ITECH,IVTYP)
                    WRITE (*,*) 'Technology ID 2  = ',TECHLABEL(SYNERGY(2,INOTE),IVTYP)
                    WRITE (*,*) 'DELTA_MKT        = ',DELTA_MKT
                    WRITE (*,*) 'DEL_FE           = ',DEL_FE(ITECH,IVTYP)
                    WRITE (*,*) 'SYNR_DEL         = ',SYNR_DEL(INOTE)
                    WRITE (*,*) 'Net FE Effect    = ',DEL_FE(ITECH,IVTYP) + SYNR_DEL(INOTE)
                    WRITE (*,*)
                    WRITE (*,*) '  ***** Run ABORTED *****'
                    WRITE (*,*)
                    WRITE (*,*)'Fix Program Logic and Rerun'
                    WRITE (*,*)
                    WRITE (*,*) '==============================================================='
                    STOP 291
                  ENDIF

                  SYNERGY_LOSS(ITECH) = SYNERGY_LOSS(ITECH) + (DELTA_MKT * SYNR_DEL(INOTE))

                ENDIF
              ENDDO
            ENDDO   ! end technology (ITECH) loop for synergy notes

!...		Repeat the technology loop one last time to aggregate the impacts of changes in technology in market share.
            TECH_ADJHP  = 0.0
            HP_GIVEBACK = 0.0

            DO ITECH=1,NUMTECH
!...		  skip technology not applicable to vehicle type (by fuel type)
              IF(.NOT. TECH_APPLIC(ITECH,IVTYP,ILDV)) CYCLE
              IF (YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE
              DELTA_MKT = MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) - &
                          MKT_PEN(ICL,IGP,ITECH,PREV,ILDV)
!...		  For pure non-econometric consumer driven techs (e.g., forced size/weight increase), set DELTA_MKT to zero if this is a CAFE pass 
!...		  through FEMCALC (i.e., PASS > 1).  Do not change the actual MKT_PEN values since this will screw up the tech phase-in for subsequent
!...		  years.  Setting DELTA_MKT will nullify both the FE and WGT impacts of these techs for this year without creating compensating changes
!...		  in subsequent years.
              FE(ICL,IGP,CURRENT,ILDV) = FE(ICL,IGP,CURRENT,ILDV)+FE(ICL,IGP,PREV,ILDV)*((DELTA_MKT * DEL_FE(ITECH,IVTYP))+SYNERGY_LOSS(ITECH))
              WEIGHT(ICL,IGP,CURRENT,ILDV) = WEIGHT(ICL,IGP,CURRENT,ILDV) + (DELTA_MKT * (DEL_WGTABS(ITECH,IVTYP) +       &
                                             (WEIGHT(ICL,IGP,CURRENT,ILDV) * DEL_WGTWGT(ITECH,IVTYP))))

              PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) + (DELTA_MKT * TECHCOST(ITECH))

!...		  Calculate annual horsepower adjustment due to technology introduction alone. This is only part of overall horsepower adjustment,
!...		  so final horsepower is calculated below, outside the technology loop.
              TECH_ADJHP = TECH_ADJHP + (DELTA_MKT * DEL_HP(ITECH,IVTYP))
            ENDDO   ! end market share impact loop

            TECH_MAX_ADJHP = TECH_ADJHP

!...		Run checks on the total market penetration of related technologies to ensure that it does not exceed 100 percent.
!...		skip to here if <= xyr
 1100       DO I=1,20
              CHECKSUM = 0.0
              DO J=1,15
                IF (SUMCHECK(J,I) .EQ. -9) GO TO 1200
                IF (SUMCHECK(J,I) .GT. MAXTECH) THEN
                  WRITE (*,*)
                  WRITE (*,*) '============================================'
                  WRITE (*,*)
                  WRITE (*,*) 'Checksum Tech ID is Out of Range'
                  WRITE (*,*)
                  WRITE (*,*) 'Maximum Tech ID     = ',MAXTECH
                  WRITE (*,*) 'Encountered Tech ID = ',SUMCHECK(J,I)
                  WRITE (*,*) 'Sumcheck array row  = ',I
                  WRITE (*,*) 'Sumcheck array col  = ',J
                  WRITE (*,*)
                  WRITE (*,*) '         ***** Run ABORTED *****'
                  WRITE (*,*)
                  WRITE (*,*) 'Fix Sumcheck Array Definitions in Subroutine'
                  WRITE (*,*) 'FEMCALC (of the TRAN module) and Rerun'
                  WRITE (*,*)
                  WRITE (*,*) '============================================'
                  STOP 701
                ENDIF
                CHECKSUM = CHECKSUM + MKT_PEN(ICL,IGP,SUMCHECK(J,I),CURRENT,ILDV)
              ENDDO
              J = 16
 1200         J = J - 1
              IF (CHECKSUM .GT. 1.0+ROUNDOFF_ERROR) THEN                    
                WRITE (*,*)
                WRITE (*,*) '============================================'
                WRITE (*,*)
                WRITE (*,*) 'Related Tech Pen Sum is greater than One'
                WRITE (*,*)
                WRITE (*,*) 'Year             = ',YRS
                WRITE (*,*) 'Vehicle Group    = ',GROUPLABEL(IGP)
                WRITE (*,*) 'Vehicle Class    = ',CLASSLABEL(ICL,IGP)
                WRITE (*,*) 'Vehicle Type     = ',FTYPELABEL(ILDV)
                WRITE (*,*)
                WRITE (*,*) 'Checksum         = ',CHECKSUM
                WRITE (*,*)
                WRITE (*,*) 'Individual Tech Penetrations are as follows:'
                WRITE (*,*)
                DO K = 1,J
                WRITE (*,*) 'Technology ID    = ',TECHLABEL(SUMCHECK(k,I),IVTYP)
                WRITE (*,*) 'Market Share     = ',MKT_PEN(ICL,IGP,SUMCHECK(k,i),CURRENT,ILDV)
                write (*,*) 'max mkt          = ',mmax(sumcheck(k,i))
                write (*,*) 'mkt_max          = ',MKT_MAX(ICL,IGP,sumcheck(k,i),ILDV)               
                WRITE (*,*)
                ENDDO
                WRITE (*,*) '    ***** Run ABORTED *****'
                WRITE (*,*)
                WRITE (*,*) 'Fix Tech Market Share Matrix or'
                WRITE (*,*) 'Engineering Notes and Rerun'
                WRITE (*,*)
                WRITE (*,*) '============================================'
                STOP 702
              ENDIF
            ENDDO

!...		Jump over remainder of FEM processing if this is FEM base year or earlier.
            IF (YRS .LE. XYR) CYCLE

 2000       CONTINUE

!...		Electric drive vehicles have an additional price adjustments to account for battery and fuel cell cost.
			if(price(icl,igp,current,ildv).ne.0.0) then
              if(ILDV.eq.4.OR.ILDV.EQ.7.OR.ILDV.EQ.15) call EVCALC (yrs)
              if(ILDV .EQ. 8 .OR. ILDV .EQ. 16) CALL HEVCALC (yrs)
			  if(ILDV.EQ.5.OR.ILDV.EQ.6) CALL PHEVCALC (yrs)
			  if(ILDV.GE.13 .AND. ILDV .LE. 14) CALL FCCALC (yrs)
			endif
			
!...		Electric power train vehicles do NOT have HP adjustments.
            if(ILDV.eq.4.or.ILDV.eq.7.or.(ILDV.ge.13.and.ILDV.le.15)) then  ! revisit this jma
                if(WEIGHT(ICL,IGP,PREV,ILDV).ne.0.0) then
!...check this jma
                  HP(ICL,IGP,CURRENT,ILDV) = (HP(ICL,IGP,PREV,ILDV)/WEIGHT(ICL,IGP,PREV,ILDV)) * WEIGHT(ICL,IGP,CURRENT,ILDV)
			    else
				  HP(ICL,IGP,CURRENT,ILDV) = 0.0
				endif
              CYCLE
            endif

!...		Initially set horsepower for constant performance level based on last year's power to weight ratio.
            IF(WEIGHT(ICL,IGP,PREV,ILDV) .NE. 0.0) THEN
!...check this
              HP(ICL,IGP,CURRENT,ILDV) = (HP(ICL,IGP,PREV,ILDV)/WEIGHT(ICL,IGP,PREV,ILDV)) * WEIGHT(ICL,IGP,CURRENT,ILDV)
            ELSE
              HP(ICL,IGP,CURRENT,ILDV) = 0.0
            ENDIF

!...		Estimate annual horsepower adjustment due to consumer performance demand. Consumer performance demand is adjusted downward as HP/WGT
!...		ratio increases so that performance gains cannot continue indefinitely.  Initial demand coefficients are controlled via user input 
!...		parameter PERFFACT (VEHPERFFAC in TRNLDV.XML) and demand caps via user input parameter PERFCAP (VEHPERFCAP in TRLDV.XML).
            if(ILDV.lt.4.or.(ILDV.ge.9.and.ILDV.le.12).or.ildv.eq.16)then
              IF(INC90_D_NP(11,YRS-1)                     .NE. 0.0 .AND. &
                PRICE(ICL,IGP,CURRENT,ILDV)               .NE. 0.0 .AND. &
                FE(ICL,IGP,PREV,ILDV)                     .NE. 0.0 .AND. &
                PMGTR90_D_(11,YRS)                        .NE. 0.0 .AND. &
                WEIGHT(ICL,IGP,BASE,ILDV)                 .NE. 0.0 .AND. &
                WEIGHT(ICL,IGP,CURRENT,ILDV)              .NE. 0.0 .AND. &
                PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED  .NE. 0.0) THEN

                PERF_ADJHP = (((INC90_D_NP(11,YRS)/INC90_D_NP(11,YRS-1)) ** 0.9) * &
                             ((PRICE(ICL,IGP,PREV,ILDV)/PRICE(ICL,IGP,CURRENT,ILDV)) ** 0.9) * &
                             ((FE(ICL,IGP,CURRENT,ILDV)/FE(ICL,IGP,PREV,ILDV)) ** 0.2) * &
                             ((PMGTR90_D_(11,YRS-1)/PMGTR90_D_(11,YRS)) ** 0.2)) - 1.0
		  
                if(n.ge.XYR+1) perf_adjhp = 0.0 

                if(weight(ICL,IGP,PREV,ILDV).NE. 0.0) then
                  vcw_adj = weight(ICL,IGP,CURRENT,ILDV)/weight(ICL,IGP,PREV,ILDV)-1.0
                else
                  vcw_adj = 0.0
                endif

                if(vcw_adj.lt.0.0) then 
                  vhp_adj(ICL,IGP,CURRENT,ILDV) = 1.0+(vcw_adj*0.8)
                  vhp_adj(ICL,IGP,PREV,ILDV) = vhp_adj(ICL,IGP,CURRENT,ILDV)
                else
                  if(vhp_adj(ICL,IGP,PREV,ILDV).eq.0.0)then
                    vhp_adj(ICL,IGP,CURRENT,ILDV) = 1.0
                  else
                    vhp_adj(ICL,IGP,CURRENT,ILDV) = vhp_adj(ICL,IGP,PREV,ILDV)
                  endif
                endif

                hp(ICL,IGP,current,ILDV) = hp(ICL,IGP,current,ILDV)*vhp_adj(ICL,IGP,current,ILDV) 
              
                HP_WGT = 0.0			  
                if(WEIGHT(ICL,IGP,CURRENT,ILDV).ne.0.0) HP_WGT = HP(ICL,IGP,CURRENT,ILDV) / WEIGHT(ICL,IGP,CURRENT,ILDV)
                PERF_COEFF  = 1.0 - ((HP_WGT- HP_WGT_BASE + DEMAND_USED) / (PERFCAP(ICL,IGP) - HP_WGT_BASE + DEMAND_USED))

!...		    Print warning message if PERF_COEFF takes on a value above one or below zero. Since WEIGHT is already adjusted, but horsepower is
!...		    not, allow a small buffer above one in the early years of the projection and a small buffer below zero in the latter years of the 
!...		    projection to avoid warnings due solely to "noise" type changes in the initial HP/WGT ratio estimate for each projection year.  
!...		    However,do not maintain this buffer in the actual PERF_ADJHP scaling algorithm.  Instead, reset ANY value above one or below zero.
                UPPER_BUFFER = 1.0 + (MAX(XYR+1 - YRS,0) * 0.01)
                LOWER_BUFFER = 0.0 - (MAX(YRS - XYR+11,0) * 0.01)
              
                IF(PERF_COEFF .GT. UPPER_BUFFER .OR. PERF_COEFF .LT. LOWER_BUFFER) THEN
!                 WRITE (21,*)
!                 WRITE (21,*) 'Consumer Performance Coefficient is'
!                 WRITE (21,*) 'less than Zero or greater than One.'
!                 WRITE (21,*)
!                 WRITE (21,*) '                   Year = ',YRS
!                 WRITE (21,*) '         NEMS Iteration = ',CURITR
!                 WRITE (21,*) '               FEM Pass = ',PASS
!                 WRITE (21,*) '          Vehicle Group = ',IGP
!                 WRITE (21,*) '          Vehicle Class = ',ICL
!                 WRITE (21,*) '              Fuel Type = ',ILDV
!                 WRITE (21,*)
!                 WRITE (21,*) 'Value before Adjustmemt = ',PERF_COEFF
!                 WRITE (21,*)
!                 WRITE (21,*) 'Coefficient has been Reset (to 0 or 1).'
!                 WRITE (21,*)
                ENDIF

                PERF_COEFF  = MIN(1.0,PERF_COEFF)
                PERF_COEFF  = MAX(0.0,PERF_COEFF)
                PERF_ADJHP  = PERF_ADJHP * PERFFACT(ICL,IGP) * PERF_COEFF

!...		    If this is an ultra CAFE pass (i.e., PASS = 3), zero any consumer HP demand at this point, but wait until after the remaining HP/WGT 
!...		    consistency checks are performed to restrict tech-driven HP.  Note, some consumer demand may be readded below if the required minimum 
!...		    HP/WGT ratio is not maintained.  Since a minimum HP/WGT ratio is considered essential for driveability, a minimum HP requirement will
!...		    not be waived even under a third pass CAFE scenario.
                IF (PASS .EQ. 3) PERF_ADJHP = MIN(PERF_ADJHP,0.0)
              ELSE
				WRITE(*,*)
				WRITE(*,*) ' Divisor in FEMCALC PERF_ADJHP calc equals zero - RUN ABORTED.'
				WRITE(*,*)
				WRITE(*,*) ' --- At ABORT, index parameters were:'
				WRITE(*,*)
				WRITE(*,*) '   YRS   = ',YRS
				WRITE(*,*) '   ICL   = ',ICL
				WRITE(*,*) '   IGP   = ',IGP
				WRITE(*,*) '   ILDV  = ',ILDV
				WRITE(*,*)
				WRITE(*,*) ' --- the various denominators were:'
				WRITE(*,*)
				WRITE(*,*) '   INC90_D_NP(11,YRS-1)                     = ',INC90_D_NP(11,YRS-1)
				WRITE(*,*) '   PRICE(ICL,IGP,CURRENT,ILDV)              = ',PRICE(ICL,IGP,CURRENT,ILDV)
				WRITE(*,*) '   FE(ICL,IGP,PREV,ILDV)                    = ',FE(ICL,IGP,PREV,ILDV)
				WRITE(*,*) '   PMGTR90_D_(11,YRS)                       = ',PMGTR90_D_(11,YRS)
				WRITE(*,*) '   WEIGHT(ICL,IGP,BASE,ILDV)                = ',WEIGHT(ICL,IGP,BASE,ILDV)
				WRITE(*,*) '   WEIGHT(ICL,IGP,CURRENT,ILDV)             = ',WEIGHT(ICL,IGP,CURRENT,ILDV)
				WRITE(*,*) '   PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED = ',PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED
				WRITE(*,*)
				WRITE(*,*) ' --- and associated numerators were:'
				WRITE(*,*)
				WRITE(*,*) '   INC90_D_NP(11,YRS)                       = ',INC90_D_NP(11,YRS)
				WRITE(*,*) '   PRICE(ICL,IGP,PREV,ILDV)                 = ',PRICE(ICL,IGP,PREV,ILDV)
				WRITE(*,*) '   FE(ICL,IGP,CURRENT,ILDV)                 = ',FE(ICL,IGP,CURRENT,ILDV)
				WRITE(*,*) '   PMGTR90_D_(11,YRS-1)                     = ',PMGTR90_D_(11,YRS-1)
				WRITE(*,*) '   HP(ICL,IGP,BASE,ILDV)                    = ',HP(ICL,IGP,BASE,ILDV)
				WRITE(*,*) '   HP(ICL,IGP,CURRENT,ILDV)                 = ',HP(ICL,IGP,CURRENT,ILDV)
				WRITE(*,*) '   HP_WGT-HP_WGT_BASE+DEMAND_USED           = ',HP(ICL,IGP,CURRENT,ILDV)/WEIGHT(ICL,IGP,CURRENT,ILDV)-&
                                                                            HP_WGT_BASE+DEMAND_USED
				STOP 509
              ENDIF

!...		  Calculate the total horsepower adjustment for this year (i.e., technology-driven plus consumer demand-driven adjustments).
			  TTL_ADJHP = TECH_ADJHP + PERF_ADJHP

!...		  Limit total horsepower adjustment in any given year to 10%.  Take back consumer demand first since that fuel economy effect is not yet 
!...		  considered.  Take any additionally needed horsepower demand back from the technology side, and track this "giveback" since it must be 
!...		  converted back into its fuel economy equivalent.
              HP_GIVEBACK = 0.0
              IF(TTL_ADJHP .GT. 0.1) THEN
!               WRITE (21,*)
!               WRITE (21,*) 'Total HP adjustment constrained to 10%'
!               WRITE (21,*)
!               WRITE (21,*) '                   Year = ',YRS
!               WRITE (21,*) '         NEMS Iteration = ',CURITR
!               WRITE (21,*) '               FEM Pass = ',PASS
!               WRITE (21,*) '          Vehicle Group = ',IGP
!               WRITE (21,*) '          Vehicle Class = ',ICL
!               WRITE (21,*) '              Fuel Type = ',ILDV
!               WRITE (21,*)
!               WRITE (21,*) 'Value before constraint = ',TTL_ADJHP
!               WRITE (21,*) '        Tech adjustment = ',TECH_ADJHP
!               WRITE (21,*) '        Perf adjustment = ',PERF_ADJHP

                HP_GIVEBACK = TTL_ADJHP - 0.1
                IF(PERF_ADJHP .GT. 0.0) THEN
                  PERF_ADJHP = PERF_ADJHP - HP_GIVEBACK
                  IF(PERF_ADJHP .GE. 0.0) THEN
                    HP_GIVEBACK = 0.0
                  ELSE
                    HP_GIVEBACK = 0.0 - PERF_ADJHP
                    PERF_ADJHP  = 0.0
                  ENDIF
                ENDIF
                TECH_ADJHP = TECH_ADJHP - HP_GIVEBACK
                TTL_ADJHP  = TECH_ADJHP + PERF_ADJHP
!               WRITE (21,*)
!               WRITE (21,*) 'Value after constraint  = ',TTL_ADJHP
!               WRITE (21,*) '       Tech adjustment  = ',TECH_ADJHP
!               WRITE (21,*) '       Tech giveback    = ',HP_GIVEBACK
!               WRITE (21,*) '       Perf adjustment  = ',PERF_ADJHP
              ENDIF

!...		  Also impose a maximum limit on HP/WGT ratio so that performance characteristics do not become unreasonable. Take back any consumer 
!...		  demand first since that fuel economy effect is not yet considered.  However, at this point it is likely that any consumer demand is
!...		  small already due to the performance demand constraints imposed above on HP/WGT ratio increases.  Take any additional required horsepower
!...		  demand back from the technology side, and track this "giveback" since it must be converted back into its fuel economy equivalent.
              IF(WEIGHT(ICL,IGP,CURRENT,ILDV) .NE. 0.0) THEN
                TEMP_HP = HP(ICL,IGP,CURRENT,ILDV) * (1.0 + TTL_ADJHP)
                HP_WGT  = TEMP_HP / WEIGHT(ICL,IGP,CURRENT,ILDV)
                IF(HP_WGT .GT. PERFCAP(ICL,IGP)) THEN
!                 WRITE (21,*)
!                 WRITE (21,*) 'Total HP adjustment exceeds HP/WGT max'
!                 WRITE (21,*)
!                 WRITE (21,*) '                   Year = ',YRS
!                 WRITE (21,*) '         NEMS Iteration = ',CURITR
!                 WRITE (21,*) '               FEM Pass = ',PASS
!                 WRITE (21,*) '          Vehicle Group = ',IGP
!                 WRITE (21,*) '          Vehicle Class = ',ICL
!                 WRITE (21,*) '              Fuel Type = ',ILDV
!                 WRITE (21,*)
!                 WRITE (21,*) 'Value before constraint = ',TTL_ADJHP
!                 WRITE (21,*) '        Tech adjustment = ',TECH_ADJHP
!                 WRITE (21,*) '        Perf adjustment = ',PERF_ADJHP
!                 WRITE (21,*) '             HP/WGT Cap = ',PERFCAP(ICL,IGP)
!                 WRITE (21,*) '       Tentative HP/WGT = ',HP_WGT

                  EXCESS_ADJHP = TTL_ADJHP
                  if(hp_wgt.ne.0.0) TTL_ADJHP = ((1.0 + TTL_ADJHP)*(PERFCAP(ICL,IGP)/HP_WGT)) - 1.0
                  EXCESS_ADJHP = EXCESS_ADJHP - TTL_ADJHP
                  IF(PERF_ADJHP .GT. 0.0) THEN
                    PERF_ADJHP = PERF_ADJHP - EXCESS_ADJHP
                    IF(PERF_ADJHP .GE. 0.0) THEN
                      EXCESS_ADJHP = 0.0
                    ELSE
                      EXCESS_ADJHP = 0.0 - PERF_ADJHP
                      PERF_ADJHP   = 0.0
                    ENDIF
                  ENDIF
                  IF(EXCESS_ADJHP .GT. TECH_ADJHP) EXCESS_ADJHP = TECH_ADJHP
                  TECH_ADJHP   = TECH_ADJHP - EXCESS_ADJHP
                  TTL_ADJHP    = TECH_ADJHP + PERF_ADJHP
                  HP_GIVEBACK  = HP_GIVEBACK + EXCESS_ADJHP

                  TEMP_HP = HP(ICL,IGP,CURRENT,ILDV) * (1.0 + TTL_ADJHP)
                  HP_WGT  = TEMP_HP / WEIGHT(ICL,IGP,CURRENT,ILDV)

!                 WRITE (21,*)
!                 WRITE (21,*) ' Value after constraint = ',TTL_ADJHP
!                 WRITE (21,*) '        Tech adjustment = ',TECH_ADJHP
!                 WRITE (21,*) '          Tech giveback = ',HP_GIVEBACK
!                 WRITE (21,*) '        Perf adjustment = ',PERF_ADJHP
!                 WRITE (21,*) '           Final HP/WGT = ',HP_WGT

!                 IF(HP_WGT .GT. (PERFCAP(ICL,IGP)+ROUNDOFF_ERROR)) THEN
!                   WRITE (21,*)
!                   WRITE (21,*) ' Constraint Limited by Available Tech Adjustment'
!                 ENDIF
                ENDIF
              ELSE
                WRITE(*,*)
                WRITE(*,*) ' Divisor in FEMCALC EXCESS_ADJHP calc'
                WRITE(*,*) ' equals zero - RUN ABORTED.'
                WRITE(*,*)
                STOP
              ENDIF

!...		  Finally, make sure HP/WGT ratio stays above that required for driveability (95% of base year value or 0.04 for two seaters, 0.033 otherwise;
!...		  whichever is lower). In this case, add additional required demand to consumer performance demand side since all "standardly" available 
!...		  technology performance impacts will already be considered on the tech side.  Additional demand need not be specially tracked since it is
!...		  reflected in PERF_ADJHP, which is automatically converted into fuel economy equivalent impacts in the algorithms that follow.
              IF(WEIGHT(ICL,IGP,CURRENT,ILDV).NE.0.0.AND.WEIGHT(ICL,IGP,BASE,ILDV).NE.0.0.AND.HP(ICL,IGP,CURRENT,ILDV).NE.0.0) THEN
!               HP_WGT_MIN = 0.95 * (HP(ICL,IGP,BASE,ILDV)/WEIGHT(ICL,IGP,BASE,ILDV))
                HP_WGT_MIN = 0.9 * (HP(ICL,IGP,BASE,ILDV)/WEIGHT(ICL,IGP,BASE,ILDV))
                IF(ICL .EQ. 6 .AND. IGP .LE. 4) THEN
                  HP_WGT_MIN = MIN(HP_WGT_MIN,1.0/25.0)
                ELSE
                  HP_WGT_MIN = MIN(HP_WGT_MIN,1.0/30.0)
                ENDIF
                TEMP_HP = HP(ICL,IGP,CURRENT,ILDV) * (1.0 + TTL_ADJHP)
                HP_WGT = TEMP_HP / WEIGHT(ICL,IGP,CURRENT,ILDV)
                MIN_ADJHP = ((HP_WGT_MIN * WEIGHT(ICL,IGP,CURRENT,ILDV))/HP(ICL,IGP,CURRENT,ILDV)) - 1.0
                MIN_ADJ = .FALSE.
              ELSE
                WRITE(*,*)
                WRITE(*,*) ' Divisor in FEMCALC HP/WGT min calc'
                WRITE(*,*) ' equals zero - RUN ABORTED.'
                WRITE(*,*)
                STOP
              ENDIF

              IF(HP_WGT .LT. HP_WGT_MIN) THEN
                MIN_ADJ = .TRUE.
!               WRITE (21,*)
!               WRITE (21,*) 'Total HP adjustment below HP/WGT min'
!               WRITE (21,*)
!               WRITE (21,*) '                   Year = ',YRS
!               WRITE (21,*) '         NEMS Iteration = ',CURITR
!               WRITE (21,*) '               FEM Pass = ',PASS
!               WRITE (21,*) '          Vehicle Group = ',IGP
!               WRITE (21,*) '          Vehicle Class = ',ICL
!               WRITE (21,*) '              Fuel Type = ',ILDV
!               WRITE (21,*) '       Tentative HP/WGT = ',HP_WGT
!               WRITE (21,*) '         Minimum HP/WGT = ',HP_WGT_MIN
!               WRITE (21,*)
!               WRITE (21,*) 'Value before constraint = ',TTL_ADJHP
!               WRITE (21,*) '        Tech adjustment = ',TECH_ADJHP
!               WRITE (21,*) '        Perf adjustment = ',PERF_ADJHP
!...		    Calculate the horsepower demand required to maintain a minimum HP/WGT ratio.
                NEED_ADJHP  = MIN_ADJHP - TTL_ADJHP
                PERF_ADJHP  = PERF_ADJHP + NEED_ADJHP
                TTL_ADJHP   = TECH_ADJHP + PERF_ADJHP
!               WRITE (21,*)
!               WRITE (21,*) 'Value after constraint  = ',TTL_ADJHP
!               WRITE (21,*) '       Tech adjustment  = ',TECH_ADJHP
!               WRITE (21,*) '       Perf adjustment  = ',PERF_ADJHP
              ENDIF

!...	      Finally, if this is a third (i.e., ultra CAFE) pass, take back all the tech driven HP demand except that required
!...	      to maintain the HP/WGT minimum.
              if(ILDV.lt.4.or.(ILDV.ge.9.and.ILDV.le.12).or.ILDV.eq.16)then
                if(igp.ne.5.and.igp.ne.11) then ! luxury vehicles
                  if(PASS.eq.3) then
                    EXCESS_ADJHP = TTL_ADJHP - MAX(MIN_ADJHP,0.0)
                    EXCESS_ADJHP = MAX(EXCESS_ADJHP,0.0)
!...                This new algorithm takes back only a user specified increment in each step.
                    Excess_AdjHP = Excess_AdjHP*GBInc
                    if(TECH_ADJHP.gt.Roundoff_Error.and.(MIN_ADJ.and.EXCESS_ADJHP.gt.TECH_ADJHP)) then
                      WRITE(*,*)
                      WRITE(*,*) ' Error in Pass 3 HP Adjustment Logic'
                      WRITE(*,*)
                      WRITE(*,*) '             Year = ',YRS
                      WRITE(*,*) '   NEMS Iteration = ',CURITR
                      WRITE(*,*) '         FEM Pass = ',PASS
                      WRITE(*,*) '    Vehicle Group = ',IGP
                      WRITE(*,*) '    Vehicle Class = ',ICL
                      WRITE(*,*) '        Fuel Type = ',ILDV
                      WRITE(*,*) '        Ttl_Adjhp = ',TTL_ADJHP
                      WRITE(*,*) '       Tech_Adjhp = ',TECH_ADJHP
                      WRITE(*,*) '       Perf_Adjhp = ',PERF_ADJHP
                      WRITE(*,*) '        Min_Adjhp = ',MIN_ADJHP
					  WRITE(*,*) '     Excess_Adjhp = ',EXCESS_ADJHP
					  WRITE(*,*)
					  WRITE(*,*) '***** RUN ABORTED *****'
					  WRITE(*,*)
					  WRITE(*,*) ' Correct Logic and Rerun.'
					  WRITE(*,*)
					  STOP
                    else
                      EXCESS_ADJHP = MIN(MAX(TECH_ADJHP,0.0),max(EXCESS_ADJHP,0.0))
					  TECH_ADJHP   = TECH_ADJHP - EXCESS_ADJHP
					  TTL_ADJHP    = TECH_ADJHP + PERF_ADJHP
					  HP_GIVEBACK  = HP_GIVEBACK + EXCESS_ADJHP
					endif
                  else
                    if(TTL_ADJHP.eq.0.0) HP_Giveback = 0.0
                  endif
                else
                  if(TTL_ADJHP.eq.0.0) HP_Giveback = 0.0
                endif
          
!...		  	Finally, check to make sure horsepower giveback does not exceed maximum available tech-driven HP increase.  
!...		  	If so, something's wrong in the HP adjustment logic.  Now there's a stretch!
                if(igp.ne.5.and.igp.ne.11) then ! luxury vehicles
                  if((TECH_MAX_ADJHP.gt.0.0.and.HP_GIVEBACK.gt.(TECH_MAX_ADJHP+ROUNDOFF_ERROR)).or. &
                     (TECH_MAX_ADJHP.le.0.0.and.ABS(HP_GIVEBACK).gt.(ABS(TECH_MAX_ADJHP)+roundoff_error))) then
                    WRITE(*,*)
                    WRITE(*,*) ' Error in HP Adjustment Logic'
                    WRITE(*,*) ' HP Giveback Exceeds Tech Max'
                    WRITE(*,*)
                    WRITE(*,*) '             Year = ',YRS
                    WRITE(*,*) '   NEMS Iteration = ',CURITR
                    WRITE(*,*) '         FEM Pass = ',PASS
                    WRITE(*,*) '    Vehicle Group = ',IGP
                    WRITE(*,*) '    Vehicle Class = ',ICL
                    WRITE(*,*) '        Fuel Type = ',ILDV
                    WRITE(*,*) '        Ttl_Adjhp = ',TTL_ADJHP
                    WRITE(*,*) '       Tech_Adjhp = ',TECH_ADJHP
                    WRITE(*,*) '       Perf_Adjhp = ',PERF_ADJHP
                    WRITE(*,*) '   Tech_Max_Adjhp = ',TECH_MAX_ADJHP
                    WRITE(*,*) '      HP_Giveback = ',HP_GIVEBACK
                    WRITE(*,*)
                    WRITE(*,*) '***** RUN ABORTED *****'
                    WRITE(*,*)
                    WRITE(*,*) ' Correct Logic and Rerun.'
                    WRITE(*,*)
                    STOP
                  endif
                endif
              endif

!...	      Now ready to adjust fuel economy up or down in accordance with the sum of consumer driven horsepower adjustment and any horsepower giveback.
!...	      Horsepower giveback is HP demand already considered in FE estimates, so FE estimates need to be adjusted upward for any giveback.  Tech driven
!...	      affects are already accounted for in the tech incremental fuel economy values.  Note that the consumer and giveback estimates are aggregated
!...	      into the consumer parameter PERF_ADJHP to facilitate the series of ensuing FE and PRICE algortihms, recognizing of course that giveback is 
!...	      negative demand.
              PERF_ADJHP = PERF_ADJHP - HP_GIVEBACK
              SIGN = 1
              IF(PERF_ADJHP .LT. 0.0) SIGN = -1
              ADJFE = (-0.220 * PERF_ADJHP) - (+0.560 * SIGN * PERF_ADJHP * PERF_ADJHP)

              HP(ICL,IGP,CURRENT,ILDV) = HP(ICL,IGP,CURRENT,ILDV)*(1+TTL_ADJHP)
              FE(ICL,IGP,CURRENT,ILDV) = FE(ICL,IGP,CURRENT,ILDV)*(1+ADJFE)

              PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) + PERF_ADJHP * VALUEPERF(ICL,IGP)
            
            endif
		  ENDDO   ! end vehicle class (ICL) loop
        ENDDO   ! end vehicle group (IGP) loop
      ENDDO   ! end vehicle fuel type (ILDV) loop

 5000 CALL FEMRANGE

! ... Assign FEM parms to report writer arrays
	  if(curcalyr.gt.xyr) then
        DO ILDV=1,MAXLDV
          DO IGP=1,MAXGROUP
			DO ICL=1,MAXCLASS
              FEMMPG(IGP,ICL,YRS,ILDV)  = FE(ICL,IGP,CURRENT,ILDV)
              FEMWGT(IGP,ICL,YRS,ILDV)  = WEIGHT(ICL,IGP,CURRENT,ILDV)
              FEMPRI(IGP,ICL,YRS,ILDV)  = PRICE(ICL,IGP,CURRENT,ILDV)
              FEMHP(IGP,ICL,YRS,ILDV)   = HP(ICL,IGP,CURRENT,ILDV)
              FEMTSZ(IGP,ICL,YRS,ILDV)  = TANKSIZE(ICL,IGP,CURRENT,ILDV)
              FEMRNG(IGP,ICL,YRS,ILDV)  = RANGE(ICL,IGP,CURRENT,ILDV)
              DO ITECH=1,NUMTECH
                FEMPEN(IGP,ICL,ITECH,YRS,ILDV) = MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV)
              ENDDO
            ENDDO
          ENDDO
        ENDDO

        IF(PASS .EQ. 3) THEN
		  CALL CALIBNHTSA
!...      Copy calibrated data back into "current year" arrays for use with next evaluation year
          DO ILDV=1,MAXLDV
            DO IGP=1,MAXGROUP
              DO ICL=1,MAXCLASS
                FE(ICL,IGP,CURRENT,ILDV)      = FEMMPG(IGP,ICL,YRS,ILDV)
                WEIGHT(ICL,IGP,CURRENT,ILDV)  = FEMWGT(IGP,ICL,YRS,ILDV)
                HP(ICL,IGP,CURRENT,ILDV)      = FEMHP(IGP,ICL,YRS,ILDV)
			    PRICE(ICL,IGP,CURRENT,ILDV)   = FEMPRI(IGP,ICL,YRS,ILDV)
			    TANKSIZE(ICL,IGP,CURRENT,ILDV)= FEMTSZ(IGP,ICL,YRS,ILDV) 
              ENDDO
            ENDDO
          ENDDO
		endif
	
!...    Rerun range calculation using calibrated fuel economy
        CALL FEMRANGE
        
      ENDIF
      
    RETURN
    END SUBROUTINE FEMCALC

! ==========================================================================================================
! ... Function FUNCMAX returns the maximum possible market share given previous period values.  Intended to 
! ... reflect institutional factors leading to production lags.
! 
! ... Parameters: OLDMKSH - previous market share                              
! ...             IGP     - vehicle group (domestic/import - car/truck)        
! ...             OLDPMAX - previous value returned by FUNCMAX used to ensure  
! ...                       that FUNCMAX never goes down                       
! ... Returned Values: Returns maximum possible market share
! ==========================================================================================================                  
    REAL FUNCTION FUNCMAX(OLDMKSH,OLDPMAX)
    USE T_

      REAL OLDMKSH,OLDPMAX,TPMAX,RETD(0:30),RETI(0:30)

      DATA RETD/0.000, 0.048, 0.096, 0.145, 0.193, 0.241, 0.289, 0.337, 0.385, 0.434, &
                0.482, 0.530, 0.578, 0.626, 0.674, 0.722, 0.771, 0.819, 0.869, 0.907, &
                0.936, 0.957, 0.973, 0.984, 0.992, 0.997, 1.000, 1.000, 1.000, 1.000, 1.000/
      DATA RETI/0.000, 0.048, 0.096, 0.145, 0.193, 0.241, 0.289, 0.337, 0.385, 0.434, &
                0.482, 0.530, 0.578, 0.626, 0.674, 0.722, 0.771, 0.819, 0.869, 0.907, &
                0.936, 0.957, 0.973, 0.984, 0.992, 0.997, 1.000, 1.000, 1.000, 1.000, 1.000/

      IF (OLDPMAX .EQ. 1.0) THEN                  ! return 100% if full market was previously allowed
        FUNCMAX = 1.0
        RETURN
      ENDIF

      TPMAX = MIN(OLDPMAX,1.0)

      IF (GrpDIMap(IGP).eq.1) THEN        ! domestic cars and light trucks
        DO I=1,30
          IF (OLDMKSH .LE. RETD(I-1) .AND. TPMAX .LT. RETD(I)) THEN
            FUNCMAX = RETD(I)
            RETURN
          ENDIF
        ENDDO
        FUNCMAX = MAX(0.0,TPMAX)

      ELSEIF (GrpDIMap(IGP).eq.2) THEN    ! import cars and light trucks
        DO I=1,30
          IF (OLDMKSH .LE. RETI(I-1) .AND. TPMAX .LT. RETI(I)) THEN
            FUNCMAX = RETI(I)
            RETURN
          ENDIF
        ENDDO
        FUNCMAX = MAX(0.0,TPMAX)

      ELSE                                        ! bad argument returns zero
        FUNCMAX = 0
      ENDIF

    RETURN
    END FUNCTION FUNCMAX

! ==========================================================================================================
! ... Subroutine NOTE_SUPER makes superseding technology adjustments to the FEM estimates and ensures that 
! ... related technologies do not exceed a specific CUMULATIVE penetration.  Although inidividual tech 
! ... penetrations are controlled via the basic allowable maximum penetrations, the combined penetrations of
! ... two or more techs are controlled here.  Accordingly, this subroutine will never ADD market penetration, 
! ... but can SUBTRACT excess penetration initially allocated to a superseded technology.  The maximum 
! ... allowable market penetration for a related technology chain is taken as the MAXIMUM of the maximum 
! ... penetrations for each component technology and can thus be adjusted externally through the maximum 
! ... market penetration matrix in the TRNLDV.XML file.  Even though the maximum penetration for the chain 
! ... may exceed that of an individual tech, no problems arise since the penetration of that individual tech 
! ... is constrained by its specific maximum in the individual tech market penetration algorithms.
! ==========================================================================================================
    SUBROUTINE NOTE_SUPER
    USE T_
    IMPLICIT NONE

      DO INOTE = 1,NUM_SUP
        IF (SUPERSEDES(1,INOTE) .EQ. ITECH) THEN
!...      Set initial market share and market share maximum.
          TOT_MKT   = ACTUAL_MKT(ITECH)
          MAX_SHARE = MMAX(ITECH)
!...      Find maximum allowable tech chain penetration.
          DO I = 2,TECH_CNT(INOTE)
            MAX_SHARE = MAX(MAX_SHARE,MMAX(SUPERSEDES(I,INOTE)))
          ENDDO
!... 	  Find and adjust any EXCESS penetration downward.
          DO I = 2,TECH_CNT(INOTE)
            TOT_MKT = TOT_MKT + ACTUAL_MKT(SUPERSEDES(I,INOTE))
            IF (TOT_MKT .GT. MAX_SHARE) THEN
              ACTUAL_MKT(SUPERSEDES(I,INOTE)) = ACTUAL_MKT(SUPERSEDES(I,INOTE)) - &
                                               (TOT_MKT - MAX_SHARE)
              TOT_MKT = MAX_SHARE
!...		  Must leave some margin for round-off error in less than zero check.
              IF (ACTUAL_MKT(SUPERSEDES(I,INOTE)) .LT. 0.0-ROUNDOFF_ERROR) THEN
                RETURN_STAT = (-100 * INOTE) -  I
                RETURN
!... 		  But go ahead and reset non-zero values due to round-off to zero.
              ELSEIF (ACTUAL_MKT(SUPERSEDES(I,INOTE)) .LT. 0.0) THEN
                ACTUAL_MKT(SUPERSEDES(I,INOTE)) = 0.0
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO

      RETURN_STAT = 0
    RETURN
    END SUBROUTINE NOTE_SUPER

! ==========================================================================================================
! ... Subroutine CGSHARE combines manufacturer group data for cars and light trucks and calculates light 
! ... vehicle size class shares and average mpg, horsepower, range and weight for cars and light trucks                          
!
! ... For years through the last historic data year, use manufacture sales data from CAFE compliance to 
! ... aggregate up to car and light truck averages. For other years, use sales fractions from the last CAFE 
! ... compliance year.  
!
! ... For size class shares, project sales shares beyond last CAFE compliance data year.
! ==========================================================================================================
    SUBROUTINE CGSHARE
    USE T_
    IMPLICIT NONE

    integer it
    real	DiffLn,SumTyp(maxvtyp),NUM1,DEN1,DEN2

!...Calculate the shares of manufacturer groups and size class within each vehicle type (cars/trucks) from trnfemx.xml data
    Grpclshr=0.0
	sumtyp=0.0
!...Sum up the sales of manufacturer groups within each vehicle type (cars/trucks)
!...Include only cases where FEM data has an MPG>0.
    do icl=1,maxclass
	  sumtyp(1) = sum(cafesales(1:cargrp,icl,yrs,1:maxldv))
	  sumtyp(2) = sum(cafesales(ltkgrp:maxgroup,icl,yrs,1:maxldv))
!...  Then divide sales of each manufacturer group and size class by the total of vehicle type
      do igp=1,maxgroup
        it=GrpMap(igp)
		if(SumTyp(it).ne.0.0) Grpclshr(igp,icl,n) = sum(cafesales(igp,icl,yrs,1:maxldv))/SumTyp(it)
      enddo
	enddo

!...fill historical national ldv_sales sales with cafesales
	if(curcalyr.le.epalyr) then 
	  do igp=1,maxgroup
		do icl=1,maxclass 
		  do ildv=1,maxldv 
		    ldv_sales(igp,icl,ildv,mnumcr,n) = cafesales(igp,icl,yrs,ildv) 
		  enddo 
		enddo
	  enddo
	endif

!...calculate national ldvs sales precentages by manufacturing group
	if(curcalyr.le.epalyr) then
      do igp=1,maxgroup
		if(igp.le.cargrp) then
          GrpShare(mnumcr,igp,n) = sum(cafesales(igp,1:maxclass,yrs,1:maxldv))/sum(cafesales(1:cargrp,1:maxclass,yrs,1:maxldv))
		else 
		  GrpShare(mnumcr,igp,n) = sum(cafesales(igp,1:maxclass,yrs,1:maxldv))/sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv))
		endif 
	  enddo
	endif
!...fill regional grpshare values with US values through 2018
	if(curcalyr.le.2018) then
	  do iregn=1,mnumcr-2
		do igp=1,maxgroup
		  GrpShare(iregn,igp,n) = GrpShare(mnumcr,igp,n)
		enddo
	  enddo
	endif
!...calculate manufacturing group shares by region begining 2019	
	if(curcalyr.ge.2019.and.curcalyr.le.epalyr) then
	  do iregn=1,mnumcr-2
		do igp=1,maxgroup
		  if(igp.le.cargrp) then
	  	    GrpShare(iregn,igp,n) = sum(ldv_sales(igp,1:maxclass,1:maxldv,iregn,n))/sum(ldv_sales(1:cargrp,1:maxclass,1:maxldv,iregn,n))
		  else 
			GrpShare(iregn,igp,n) = sum(ldv_sales(igp,1:maxclass,1:maxldv,iregn,n))/sum(ldv_sales(ltkgrp:maxgroup,1:maxclass,1:maxldv,iregn,n))
		  endif
	    enddo
	  enddo 
	endif
	if(curcalyr.gt.epalyr) then
	  do iregn=1,mnumcr 
		do igp=1,maxgroup 
		  grpshare(iregn,igp,n) = grpshare(iregn,igp,n-1) 
		enddo 
	  enddo 
    endif	  
!...Go through each group. If the year is less than or equal to epalyr, then use the historical data
!...and calculate a class share. 
    do igp=1,maxgroup
!...  Use the historical cafe data.
      groupsum(igp) = sum(cafesales(igp,1:maxclass,yrs,1:maxldv))
      do icl=1,maxclass
	    class_share(mnumcr,icl,igp,yrs) = 0.0
	    if(groupsum(igp).ne.0.0) class_share(mnumcr,icl,igp,yrs)=sum(cafesales(igp,icl,yrs,1:maxldv))/groupsum(igp)
	  enddo
	enddo
	  
!...fill regional values with US values through 2018
	if(curcalyr.le.2018) then
	  do iregn = 1,mnumcr-2
	    do icl=1,maxclass 
		  do igp=1,maxgroup 
		    class_share(iregn,icl,igp,yrs) = class_share(mnumcr,icl,igp,yrs)    
	      enddo
		enddo 
	  enddo 
	endif
!...calculate class shares by group by region
	if(curcalyr.ge.2019.and.curcalyr.le.epalyr) then
	  do iregn=1,mnumcr-2
		do igp=1,maxgroup	  
		  groupsum(igp) = sum(ldv_sales(igp,1:maxclass,1:maxldv,iregn,n)) 
		  do icl=1,maxclass
		    class_share(iregn,icl,igp,yrs) = 0.0
		    if(groupsum(igp).ne.0.0) class_share(iregn,icl,igp,yrs) = sum(ldv_sales(igp,icl,1:maxldv,iregn,n))/groupsum(igp)
		  enddo
	    enddo
	  enddo
	endif

!...values for subsequent projection years are the same as the last historical year, calculations made in READHIST
    if(curcalyr.gt.stockyr) then
!...  owner sales shares remain constant through projection period 
	  do igp=1,maxgroup
	    do iown=1,maxowner 
	      do icl=1,maxclass 
			do ildv=1,maxldv
			  do iregn=1,mnumcr-2
			    ownsalesshr(iown,igp,icl,ildv,iregn,n) = ownsalesshr(iown,igp,icl,ildv,iregn,n-1)
			  enddo
			enddo 
		  enddo 
		enddo 
	  enddo
!...  temp owner shares	  
	  do igp=1,maxgroup
 	    do iown=1,maxowner 
	      do icl=1,maxclass 
			do iregn=1,mnumcr-2
			  ownsaletemp(iown,igp,icl,iregn,n) = ownsaletemp(iown,igp,icl,iregn,n-1)
			enddo
		  enddo 
		enddo 
	  enddo
	endif 

!...Use the econometric projection for yrs > stockyr
    if(curcalyr.gt.stockyr) then
	  ratio_byr=yrs-epalyr		!dst should be updated
	  do igp=1,maxgroup
	    groupsum(igp) = 0.0
        do icl=1,maxclass
		  do iregn = 1,mnumcr
            if (iregn.eq.10) CYCLE
!           If gasoline vehicles existed in this group and size class in BOTH the current and previous years
            if(pmgtr90_D_(iregn,yrs-1).ge.0.0.and.inc90_D_np(iregn,yrs-1).gt.13000.0.and.price(ICL,IGP,prev,gas).gt.0.0.and.price(ICL,IGP,current,gas).gt.0.0) then
			
			  diffln = coef_a(ICL,IGP)*log(ratio_byr) + coef_b(ICL,IGP)*log(pmgtr90_D_(iregn,yrs)/pmgtr90_D_(iregn,yrs-1)) + &
					   coef_c(ICL,IGP)*log((inc90_D_np(iregn,yrs)-13000.0)/(inc90_D_np(iregn,yrs-1)-13000.0)) + &
                       coef_p(ICL,IGP)*log(price(ICL,IGP,current,gas)/price(ICL,IGP,prev,gas))

              ratio_ln = diffln + log(max(class_share(iregn,ICL,IGP,epalyr),0.000001)/(1.0-max(class_share(iregn,ICL,IGP,epalyr),0.000001)))					
              ratio = exp(ratio_ln)
              class_share(iregn,icl,igp,yrs) = ratio/(1.0+ratio)
!           If there are ANY vehicles available in the current year, take the previous year's share
			elseif (ANY(price(ICL,IGP,current,:).gt.0.0)) then
              class_share(iregn,ICL,IGP,yrs) = class_share(iregn,ICL,IGP,yrs-1)
!           No vehicles, so no share
            else
              class_share(iregn,ICL,IGP,yrs) = 0.0
            endif
		  enddo
        enddo
!...    Normalize the shares
        do ICL=1,MAXCLASS
		  do iregn = 1,mnumcr
            if (iregn.eq.10) CYCLE
			if(sum(class_share(iregn,1:MAXCLASS,IGP,yrs)).gt.0.0) class_share(iregn,ICL,IGP,yrs) = class_share(iregn,ICL,IGP,yrs)/sum(class_share(iregn,1:MAXCLASS,IGP,yrs))
		  enddo	          
        enddo
      enddo

!...calculate manufacturer sales by group and class
      do icl=1,maxclass
		do igp=1,maxgroup
          it=GrpMap(igp)
		  do iregn=1,mnumcr
			if(iregn.eq.10) CYCLE
            mfr_sales(iregn,igp,icl,n) = newldvs(it,iregn,n)*GrpShare(iregn,igp,n)*class_share(iregn,icl,igp,yrs)
		  enddo
		enddo
      enddo
	endif ! > stockyr
	
    RETURN
    END SUBROUTINE CGSHARE

! ==========================================================================================================
! ... Subroutine TREG estimates the regional values for fuel demand, fuel cost, VMT demand, VMT shares, and 
! ... sales of non-fleet vehicles
! ==========================================================================================================
  SUBROUTINE TREG
  USE T_
  IMPLICIT NONE

    INTEGER       IR,IS
    REAL          inc_growth

!...Calculate regional shares of fuel demand
    IF (curiyr.LE.msedyr) THEN
      DO IR=1,MNUMCR-2
        IF (QSELTR(11,N) .NE. 0.0) SEDSHREL(IR,N)=QSELTR(IR,N)/QSELTR(11,N)
        IF (QSMGTR(11,N) .NE. 0.0) SEDSHRMG(IR,N)=QSMGTR(IR,N)/QSMGTR(11,N)
        IF (QSJFTR(11,N) .NE. 0.0) SEDSHRJF(IR,N)=QSJFTR(IR,N)/QSJFTR(11,N)
        IF (QSDSTR(11,N) .NE. 0.0) SEDSHRDS(IR,N)=QSDSTR(IR,N)/QSDSTR(11,N)
        IF (QSLGTR(11,N) .NE. 0.0) SEDSHRLG(IR,N)=QSLGTR(IR,N)/QSLGTR(11,N)
        IF (QSRSTR(11,N) .NE. 0.0) SEDSHRRS(IR,N)=QSRSTR(IR,N)/QSRSTR(11,N)
        IF (QSOTTR(11,N) .NE. 0.0) SEDSHROT(IR,N)=QSOTTR(IR,N)/QSOTTR(11,N)
!...give variables with 0. values in SEDS a 1/9th Census Share for now
        SEDSHRNG(IR,N)=0.1111
        SEDSHRME(IR,N)=0.1111
        SEDSHRET(IR,N)=0.1111
        SEDSHRHY(IR,N)=0.1111
      ENDDO
    ELSE
      DO IR=1,MNUMCR-2
        Inc_Growth=MC_YPDR(IR,N)/MC_YPDR(IR,N-1)
        IF (SEDSHREL(IR,N-1).LT.0.0001) SEDSHREL(IR,N-1)=0.0001
        IF (SEDSHRNG(IR,N-1).LT.0.0001) SEDSHRNG(IR,N-1)=0.0001
        IF (SEDSHRMG(IR,N-1).LT.0.0001) SEDSHRMG(IR,N-1)=0.0001
        IF (SEDSHRJF(IR,N-1).LT.0.0001) SEDSHRJF(IR,N-1)=0.0001
        IF (SEDSHRDS(IR,N-1).LT.0.0001) SEDSHRDS(IR,N-1)=0.0001
        IF (SEDSHRLG(IR,N-1).LT.0.0001) SEDSHRLG(IR,N-1)=0.0001
        IF (SEDSHRRS(IR,N-1).LT.0.0001) SEDSHRRS(IR,N-1)=0.0001
        IF (SEDSHROT(IR,N-1).LT.0.0001) SEDSHROT(IR,N-1)=0.0001
        IF (SEDSHRME(IR,N-1).LT.0.0001) SEDSHRME(IR,N-1)=0.0001
        IF (SEDSHRET(IR,N-1).LT.0.0001) SEDSHRET(IR,N-1)=0.0001
        IF (SEDSHRHY(IR,N-1).LT.0.0001) SEDSHRHY(IR,N-1)=0.0001

        SEDSHREL(IR,N)=SEDSHREL(IR,N-1)*Inc_Growth
        SEDSHRNG(IR,N)=SEDSHRNG(IR,N-1)*Inc_Growth
        SEDSHRMG(IR,N)=SEDSHRMG(IR,N-1)*Inc_Growth
        SEDSHRJF(IR,N)=SEDSHRJF(IR,N-1)*Inc_Growth
        SEDSHRDS(IR,N)=SEDSHRDS(IR,N-1)*Inc_Growth
        SEDSHRLG(IR,N)=SEDSHRLG(IR,N-1)*Inc_Growth
        SEDSHRRS(IR,N)=SEDSHRRS(IR,N-1)*Inc_Growth
        SEDSHROT(IR,N)=SEDSHROT(IR,N-1)*Inc_Growth
        SEDSHRME(IR,N)=SEDSHRME(IR,N-1)*Inc_Growth
        SEDSHRET(IR,N)=SEDSHRET(IR,N-1)*Inc_Growth
        SEDSHRHY(IR,N)=SEDSHRHY(IR,N-1)*Inc_Growth
      ENDDO
    ENDIF

!...normalize
    SEDSHREL(mnumcr,N)=sum(SEDSHREL(1:mnumcr-2,N))
    SEDSHRNG(mnumcr,N)=sum(SEDSHRNG(1:mnumcr-2,N))
    SEDSHRMG(mnumcr,N)=sum(SEDSHRMG(1:mnumcr-2,N))
    SEDSHRJF(mnumcr,N)=sum(SEDSHRJF(1:mnumcr-2,N))
    SEDSHRDS(mnumcr,N)=sum(SEDSHRDS(1:mnumcr-2,N))
    SEDSHRLG(mnumcr,N)=sum(SEDSHRLG(1:mnumcr-2,N))
    SEDSHRRS(mnumcr,N)=sum(SEDSHRRS(1:mnumcr-2,N))
    SEDSHROT(mnumcr,N)=sum(SEDSHROT(1:mnumcr-2,N))
    SEDSHRME(mnumcr,N)=sum(SEDSHRME(1:mnumcr-2,N))
    SEDSHRET(mnumcr,N)=sum(SEDSHRET(1:mnumcr-2,N))
    SEDSHRHY(mnumcr,N)=sum(SEDSHRHY(1:mnumcr-2,N))

    DO IR=1,MNUMCR
      SEDSHREL(IR,N)=SEDSHREL(IR,N)/SEDSHREL(mnumcr,N)
      SEDSHRNG(IR,N)=SEDSHRNG(IR,N)/SEDSHRNG(mnumcr,N)
      SEDSHRMG(IR,N)=SEDSHRMG(IR,N)/SEDSHRMG(mnumcr,N)
      SEDSHRJF(IR,N)=SEDSHRJF(IR,N)/SEDSHRJF(mnumcr,N)
      SEDSHRDS(IR,N)=SEDSHRDS(IR,N)/SEDSHRDS(mnumcr,N)
      SEDSHRLG(IR,N)=SEDSHRLG(IR,N)/SEDSHRLG(mnumcr,N)
      SEDSHRRS(IR,N)=SEDSHRRS(IR,N)/SEDSHRRS(mnumcr,N)
      SEDSHROT(IR,N)=SEDSHROT(IR,N)/SEDSHROT(mnumcr,N)
      SEDSHRME(IR,N)=SEDSHRME(IR,N)/SEDSHRME(mnumcr,N)
      SEDSHRET(IR,N)=SEDSHRET(IR,N)/SEDSHRET(mnumcr,N)
      SEDSHRHY(IR,N)=SEDSHRHY(IR,N)/SEDSHRHY(mnumcr,N)
    ENDDO


!...calculate regional driving demand (household) VMTLDV, assuming all in gasoline for the purpose of this calculation, 
!...which is only done here to calculate regional travel shares (RSHR), correct fuel shares for VMTLDV are calculated 
!...in subroutine TVMT
    do iregn=1,mnumcr-2
      if(curcalyr.le.VMTLDHISTYR)then
        do imf=1,MF
          do iagr=1,agegrp 
			VMTLDV(iagr,n,imf,iregn) = VMTLD(iagr,n,imf) * LICDRIVER(iagr,imf,iregn,n)		
          enddo
        enddo
      else
        do imf=1,MF
          do iagr=1,agegrp 
			VMTLDV(iagr,n,imf,iregn) = VMTLD(iagr,n-1,imf) * LICDRIVER(iagr,imf,iregn,n)
          enddo
        enddo
      endif
    enddo
	
    IF (TRANAB32 .NE. 0) then
	  do imf=1,MF
	    do iagr=1,agegrp	
			VMTLDV(iagr,n,imf,mnumcr-2) = VMTLDV(iagr,n,imf,mnumcr-2)*vmt_ca_co2(n) 
		enddo
	  enddo
	endif

!...calculate national vmt derived from licensed drivers
	do imf=1,MF
	  do iagr=1,agegrp
		VMTLDV(iagr,n,imf,mnumcr) = sum(VMTLDV(iagr,n,imf,1:mnumcr-2))
	  enddo
	enddo
	
!...calculate regional VMT shares (RSHR)
	do iregn=1,mnumcr-2
	  RSHR(iregn,n) = sum(VMTLDV(1:agegrp,n,1:MF,iregn))/sum(VMTLDV(1:agegrp,n,1:MF,mnumcr))
	enddo
	RSHR(mnumcr,n)=1.0
	  
  RETURN
  END SUBROUTINE TREG

! ==========================================================================================================
! ... Subroutine TLDV initiates the vehicle choice routine. 
! ==========================================================================================================
    SUBROUTINE TLDV
    USE T_
    IMPLICIT NONE

      PassNo=1
      CALL TATTRIB
      CALL TALT2
	  if(curcalyr.gt.stockyr) CALL TALT2X
      CALL TFLTSTKS
    !  CALL TLEGIS

      PassNo=2
      CALL TATTRIB
      CALL TALT2
	  if(curcalyr.gt.stockyr) CALL TALT2X
      CALL TFLTSTKS
    !  CALL TLEGIS

    RETURN
    END SUBROUTINE TLDV

! ==========================================================================================================                                                   
! ... Subroutine TATTRIB adjusts the LDV attributes so they can be used throughout the model (MPG, price, 
! ... range, and horsepower)
! ========================================================================================================== 
  SUBROUTINE TATTRIB
  USE T_
  IMPLICIT NONE

    INCLUDE 'ANGTDM'
    INCLUDE 'AEUSPRC'

    INTEGER       ZYR 
    REAL          ATVSALES,ESTPRICE,NUM1,NUM2,temp_phevevmt(2)
    REAL          SLOPE,INTERCEPT,IRA_Credit(MAXLDV)
    integer 	  it,xldv

!...vehicle attribute calculations for the consumer choice model - TALT2X
    DO IREGN=1,MNUMCR-2
       IF (PETTR(IREGN,N) .EQ. 0.0) PETTR(IREGN,N) = PMGTR(IREGN,N)*1.18
    ENDDO

!...FLEXSHR calculates VMT shares for flex- and bi-fuel vehicles, PctAF. And more recently PctPHEV20.
    CALL FLEXSHR

!...Fuel cost (unit = nominal cents/mile) >xyr jma do loop for phev by group
    DO IREGN=1,MNUMCR
      if(iregn.eq.10) CYCLE
      FPRICE(1,IREGN,YRS)  =  PMGTR(IREGN,N)
      FPRICE(2,IREGN,YRS)  =  HWYPDSTR(IREGN,N)
      FPRICE(3,IREGN,YRS)  =  MIN(PETTR(IREGN,N),PMGTR(IREGN,N))
      FPRICE(4,IREGN,YRS)  =  chg_dist(iregn,3,2023-1989)*(PELP2CM(IREGN,N) * CHGCSTMULT(3)) + chg_dist(iregn,1,2023-1989)*(PELPFCM(IREGN,N)*CHGCSTMULT(1)) + chg_dist(iregn,2,2023-1989)*PELVHRS(IREGN,N)
      FPRICE(5,IREGN,YRS)  = (PctPHEV20(N)*PELVHRS(IREGN,N)) + ((1.0-PctPHEV20(N))*PMGTR(IREGN,N))
      FPRICE(6,IREGN,YRS)  = (PctPHEV50(N)*PELVHRS(IREGN,N)) + ((1.0-PctPHEV50(N))*PMGTR(IREGN,N))
      FPRICE(7,IREGN,YRS)  =  chg_dist(iregn,3,2023-1989)*(PELP2CM(IREGN,N) * CHGCSTMULT(3)) + chg_dist(iregn,1,2023-1989)*(PELPFCM(IREGN,N)*CHGCSTMULT(1)) + chg_dist(iregn,2,2023-1989)*PELVHRS(IREGN,N)
      FPRICE(8,IREGN,YRS)  =  HWYPDSTR(IREGN,N)
      FPRICE(9,IREGN,YRS)  = (PCTAF(3,IREGN,N)*PGFTRPV(IREGN,N)) + ((1.0-PCTAF(3,IREGN,N))*PMGTR(IREGN,N))
      FPRICE(10,IREGN,YRS) = (PCTAF(4,IREGN,N)*PLGTR(IREGN,N)) + ((1.0-PCTAF(4,IREGN,N))*PMGTR(IREGN,N))
      FPRICE(11,IREGN,YRS) =  PGFTRPV(IREGN,N)
      FPRICE(12,IREGN,YRS) =  PLGTR(IREGN,N)
      FPRICE(13,IREGN,YRS) =  PMETR(IREGN,N)
      FPRICE(14,IREGN,YRS) =  PH2TR(IREGN,N)
      FPRICE(15,IREGN,YRS) =  chg_dist(iregn,3,2023-1989)*(PELP2CM(IREGN,N) * CHGCSTMULT(3)) + chg_dist(iregn,1,2023-1989)*(PELPFCM(IREGN,N)*CHGCSTMULT(1)) + chg_dist(iregn,2,2023-1989)*PELVHRS(IREGN,N)
      FPRICE(16,IREGN,YRS) =  PMGTR(IREGN,N)

!     Decrease the share of charging that occus at home, and redistribute to public L2/DCFC, as fuel availability (driven by BEV stocks) grows       
      if(iregn.lt.10.and.curcalyr.gt.2023) then

        chg_dist(iregn,2,n) = chg_dist(iregn,2,2023-1989) - FAVL(7,iregn,yrs-1)/1.4                                           ! Home
        chg_dist(iregn,1,n) = chg_dist(iregn,1,2023-1989)/sum(chg_dist(iregn,[1,3],2023-1989)) * (1-chg_dist(iregn,2,n))      ! DCFC
        chg_dist(iregn,3,n) = 1 - sum(chg_dist(iregn,1:2,n))                                                                ! L2 public
        
        FPRICE([4,7,15],IREGN,YRS)  =  chg_dist(iregn,1,n)*(PELP2CM(IREGN,N) * CHGCSTMULT(1)) + chg_dist(iregn,3,n)*(PELPFCM(IREGN,N) * CHGCSTMULT(3)) +chg_dist(iregn,2,n)*PELVHRS(IREGN,N)
        
!        if (curcalyr.gt.epalyr.and.fcrl.eq.1) WRITE(21,'(a,",",2(i4,","),6(f12.3,","))')'check_elec',curcalyr,iregn,chg_dist(iregn,:,n),FPRICE([4,7,15],IREGN,YRS)
      endif
      

!     If past last historical year, use modeled phev eVMT ratio and changing distribution of PHEVs by manufacturer group to estimate combined PHEV fuel prices
!      if (yrs.gt.epalyr) then
!        NUM1 = 0.0
!        NUM2 = 0.0
!        do igp=1,maxgroup
!          do icl=1,maxclass
!            NUM1 = NUM1 + phev_evmt(igp,icl,yrs-1,5)*cafesales(igp,icl,yrs-1,5)
!            NUM2 = NUM2 + phev_evmt(igp,icl,yrs-1,6)*cafesales(igp,icl,yrs-1,6)
!          enddo
!        enddo
!
!        if(sum(cafesales(1:maxgroup,1:maxclass,yrs-1,5)).gt.0.0) temp_phevevmt(1) = NUM1 / sum(cafesales(1:maxgroup,1:maxclass,yrs-1,5))
!        if(sum(cafesales(1:maxgroup,1:maxclass,yrs-1,5)).gt.0.0) temp_phevevmt(2) = NUM1 / sum(cafesales(1:maxgroup,1:maxclass,yrs-1,6))
!        
!        if(temp_phevevmt(1).gt.0.0) FPRICE(5,IREGN,YRS) = (temp_phevevmt(1)*PELVHRS(IREGN,N)) + ((1.0-temp_phevevmt(1))*PMGTR(IREGN,N))
!        if(temp_phevevmt(2).gt.0.0) FPRICE(6,IREGN,YRS) = (temp_phevevmt(2)*PELVHRS(IREGN,N)) + ((1.0-temp_phevevmt(2))*PMGTR(IREGN,N))
!
!      endif
      
    ENDDO

!...vehicle attribute calculations for the consumer choice model - TALT2X

!... convert fuel price in 1987$/mmBtu to 1990$ cents/gallon
    if(curcalyr.ge.xyr) FPRICE(:,:,YRS) = FPRICE(:,:,YRS) * MC_JPGDP(1) * 100.0 * MG_HHV/1000.0

!...initialize vehicle attributes to 0.0
    do igp=1,maxgroup
      do icl=1,maxclass
        do ildv=1,maxldv	
		  do iregn=1,mnumcr-2
            FLCOST(igp,ildv,icl,iregn,yrs) = 0.0	
			HFUEL(igp,ildv,icl,iregn,yrs) = 0.0
			ACCL(igp,ildv,icl,iregn,yrs) = 0.0
			MAINT(igp,ildv,icl,iregn,yrs) =0.0	
			LUGG(igp,ildv,icl,iregn) = 0.0
			VRNG(igp,ildv,icl,iregn,yrs) = 0.0
		  enddo 
		enddo 
	  enddo
	enddo
	
	if(curcalyr.ge.epalyr) then
	  do igp=1,maxgroup
		ivtyp = grpmap(igp) ! for maintenance cost
        do icl=1,maxclass
          do ildv=1,maxldv
		    do iregn=1,mnumcr-2
			  if(mmavail(igp,icl,ildv,iregn,yrs).gt.0.0) then
!...			calculate fuel cost per mile			  
				if(femmpg(igp,icl,yrs,ildv).ne.0.0) FLCOST(igp,ildv,icl,iregn,yrs) = FPRICE(ildv,iregn,yrs)/femmpg(igp,icl,yrs,ildv) 
!...		    assign home fueling dummy to electric vehicles, currently excluding PHEVs
			    if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) HFUEL(igp,ildv,icl,iregn,yrs) = 1.0
!...			calcualte horsepower to weight ratio
				if(femwgt(igp,icl,yrs,ildv).ne.0.0) ACCL(igp,ildv,icl,iregn,yrs) = femhp(igp,icl,yrs,ildv)/femwgt(igp,icl,yrs,ildv)
!...			maintenance cost (unit = nominal $).  Convert cost in 1996$ back to 1987$ and then to nominal $
				MAINT(igp,ildv,icl,iregn,yrs) = MAINTGRP(ildv,icl,ivtyp) 
!...			luggage space (unit = cu. ft.). Calculate ratio to gas vehicle, or if no gas vehicle, ind avg ratio to ind. avg gas veh.
!               Pickups don't have a luggage space (for choice model purposes)
				if(igp.ge.ltkgrp.and.icl.le.2) then
                  LUGG(igp,ildv,icl,iregn) = 0.0
!               Use historical data
                elseif(epalug(igp,icl,epalyr,ildv).gt.0.0.and.epalug(igp,icl,epalyr,gas).gt.0.0) then
				  LUGG(igp,ildv,icl,iregn) = epalug(igp,icl,epalyr,ildv)/epalug(igp,icl,epalyr,gas)
!               Use industry average luggage space ratio
				else 
				  LUGG(igp,ildv,icl,iregn) = luggavg(igp,icl)				  
				endif
!...			vehicle range
				vrng(igp,ildv,icl,iregn,yrs) = femrng(igp,icl,yrs,ildv)
			  endif
			enddo 
		  enddo 
		enddo 
	  enddo
	  
!...  calculate IRA PHEV and EV tax credit
      IRA_CREDIT = 0.0
      if(ira_stim.eq.1.0.and.yrs.ge.irayr) then
        do ildv=1,maxldv
		  ira_credit(ildv) = 0.0
          if(ildv.eq.4.or.ildv.le.7.or.ildv.eq.15.or.ildv.eq.14) then
		    if (CAFEMY27_SWITCH.eq.0) IRA_Credit(ildv) = (ira_veh_cred * ira_veh_shr(1,yrs,2)) + (ira_bat_cred * ira_bat_shr(1,yrs,2))
		    if (CAFEMY27_SWITCH.eq.1) IRA_Credit(ildv) = (ira_veh_cred * ira_veh_shr(1,yrs,1)) + (ira_bat_cred * ira_bat_shr(1,yrs,1))
		  endif
          if(ILDV.eq.5.or.ILDV.eq.6.or.ildv.eq.14) then
		    if (CAFEMY27_SWITCH.eq.0) IRA_Credit(ildv) = (ira_veh_cred * ira_veh_shr(2,yrs,2)) + (ira_bat_cred * ira_bat_shr(2,yrs,2))
		    if (CAFEMY27_SWITCH.eq.1) IRA_Credit(ildv) = (ira_veh_cred * ira_veh_shr(2,yrs,1)) + (ira_bat_cred * ira_bat_shr(2,yrs,1))
		  endif
	    enddo
	  endif
!...  vehicle purchase price: fempri less ira and state vehicle tax credits (1990$).
      do ildv=1,maxldv
        do igp=1,maxgroup
          do icl=1,maxclass
			do iregn=1,mnumcr-2
			  pspr(igp,ildv,icl,iregn,yrs) = 0.0
			  if(mmavail(igp,icl,ildv,iregn,yrs).gt.0.0) then
				pspr(igp,ildv,icl,iregn,yrs) = fempri(igp,icl,yrs,ildv)
!...			electric vehicles
				if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.14) & 
                  pspr(igp,ildv,icl,iregn,yrs) = fempri(igp,icl,yrs,ildv) - ira_credit(ildv) - state_cred(iregn,yrs,1) 
!...			plug-in hybrid vehilces
				if(ildv.eq.5.or.ildv.eq.6.or.ildv.eq.14) &
				  pspr(igp,ildv,icl,iregn,yrs) = fempri(igp,icl,yrs,ildv) - ira_credit(ildv) - state_cred(iregn,yrs,2)
!...			hybrid vehicles
				if(ildv.eq.16) pspr(igp,ildv,icl,iregn,yrs) = fempri(igp,icl,yrs,ildv) - state_cred(iregn,yrs,3)
              endif
            enddo
          enddo
        enddo 
      enddo
	endif

!debug jma
!    if(curcalyr.ge.epalyr.and.PassNo.eq.2.and.fcrl.eq.1) then
!	  write(21,'(6(a4,","),11(a12,","))')'attr','year','iregn','igp','icl','ildv','clsflg','grpflg','mmapply','mmavail','nameplate','pspr','fempri','ira_credit','state_ev','state_phev','state_hev'
!	  do iregn=1,mnumcr-2 
!	    do igp=1,maxgroup
!		  do icl=1,maxclass 
!		    do ildv=1,maxldv
!              WRITE(21,'(a,",",5(i4,","),L12,",",2(i12,","),9(f12.2,","))')'attr', curcalyr,iregn,igp,icl,ildv,classflag(icl,igp,ildv),grpflag(ildv,icl,igp),mmapply(igp,icl,ildv,iregn),mmavail(igp,icl,ildv,iregn,yrs),&
!                                                                           nameplate(igp,icl,yrs,ildv),pspr(igp,ildv,icl,iregn,yrs),fempri(igp,icl,yrs,ildv),ira_credit(ildv),state_cred(iregn,yrs,:)
!		  	enddo 
!		  enddo 
!	    enddo 
!	  enddo
!	endif

  RETURN
  END SUBROUTINE TATTRIB

! ==========================================================================================================
! ... Subroutine TALT2 calculates regional fuel availability for highway fuels  
! ...   Indices for endogenous calculation for fuel availability FAVAIL: 
! ...     ifuel=1 => gasoline     
! ...     ifuel=2 => diesel       
! ...     ifuel=3 => ethanol      
! ...     ifuel=4 => methanol     
! ...     ifuel=5 => cng          
! ...     ifuel=6 => lpg         
! ...     ifuel=7 => electricity 
! ...     ifuel=8 => hydrogen    
! ==========================================================================================================
  SUBROUTINE TALT2
  USE T_
  IMPLICIT NONE
    
  INCLUDE 'NGTDMREP'

    REAL    ALTSTA(MNUMCR-2,maxfuel,MNUMYR)
    REAL    ALTSTAT(maxfuel,MNUMYR)
    REAL    PREDSTK(maxfuel,MNUMYR)
    REAL    AFVSHREG(MNUMCR-2,maxfuel,MNUMYR)
    REAL    FUELVSAL(MNUMCR-2,maxfuel,MNUMYR)
    REAL    FUELVSALT(maxfuel,MNUMYR)
    REAL    sta_rat_base(maxfuel)
    REAL    gas_tput_hr(MNUMCR-2)
    REAL    port_time_tot(MNUMCR-2)
    REAL    port_time(maxchrg,MNUMCR-2)
    REAL    LDV_EV(MNUMYR,MNUMCR-2)
    REAL    s_curve(maxchrg,MNUMYR,MNUMCR-2)
    REAL    PRT_proj(maxchrg,MNUMYR,MNUMCR-2) 
      
    CHARACTER*3 iflab(8)         
    DATA iflab/'Gas','Dsl','Eth','Mth','CNG','LPG','Elc','Hyd'/      

    PARAMETER gas_stat = 9.615 ! gas pumps/station 

!...Re-assign initial number of refueling stations according to NREL data by Census Division
    if(curcalyr.le.2012)then
      do iregn=1, mnumcr-2
        do ifuel=1,maxfuel
          if(curcalyr.le.1995)then
            ALTSTA(iregn,ifuel,n) = INITSTA(ifuel,6,iregn)
          else
            ALTSTA(iregn,ifuel,n) = INITSTA(ifuel,n,iregn)
          endif
        enddo
      enddo  
!...  calculate total U.S. stations
      do ifuel=1,maxfuel
        ALTSTAT(ifuel,N) = sum(ALTSTA(1:mnumcr-2,ifuel,n))
      enddo
        
!...  initialize CNG fuel availability
      do iregn=1,mnumcr-2 
        CNGAVAIL(iregn,n) = ALTSTA(iregn,5,n)/ALTSTA(iregn,1,n)
      enddo    
      if(curcalyr.eq.2012) then 
        PREDSTK(1,N) =  (ldvstk( 1,n-1)+(ldvstk( 3,n-1)+ldvstk( 5,n-1)+ldvstk( 6,n-1)+ldvstk( 9,n-1)+ldvstk(10,n-1))*.75 &
                      + ldvstk(16,n-1)) * 1000000. 
        PREDSTK(2,N) =  (LDVSTK( 2,n-1)+ LDVSTK( 8,n-1)) * 1000000.
        PREDSTK(3,N) =  (LDVSTK( 3,n-1)*0.25) * 1000000.
        PREDSTK(4,N) =   LDVSTK(13,n-1)* 1000000.
        PREDSTK(5,N) = ((LDVSTK( 9,n-1)*0.25)+LDVSTK(11,N-1)) * 1000000.
        PREDSTK(6,N) = ((LDVSTK(10,n-1)*0.25)+LDVSTK(12,N-1)) * 1000000.
        PREDSTK(7,N) = ((ldvstk( 5,n-1)*0.1 +ldvstk( 6,n-1))*0.25+ldvstk( 4,n-1)+ldvstk( 7,n-1)+ldvstk(15,n-1)) * 1000000.
        PREDSTK(8,N) =   LDVSTK(14,n-1) * 1000000.
	  endif
	  
	  do ifuel=1,maxfuel
	    sta_rat_base(ifuel) = sta_rat(ifuel)
	  enddo
    else
!...  Estimate the vehicle stocks used to calculate the number of refueling stations
!...  by weighting flex and bi-fuel at 25% 
      PREDSTK(1,N) =  (ldvstk( 1,n-1)+(ldvstk( 3,n-1)+ldvstk( 5,n-1)+ldvstk( 6,n-1)+ldvstk( 9,n-1)+ldvstk(10,n-1))*.75 &
                      + ldvstk(16,n-1)) * 1000000. 
      PREDSTK(2,N) =  (LDVSTK( 2,n-1)+ LDVSTK( 8,n-1)) * 1000000.
      PREDSTK(3,N) =  (LDVSTK( 3,n-1)*0.25) * 1000000.
      PREDSTK(4,N) =   LDVSTK(13,n-1)* 1000000.
      PREDSTK(5,N) = ((LDVSTK( 9,n-1)*0.25)+LDVSTK(11,N-1)) * 1000000.
      PREDSTK(6,N) = ((LDVSTK(10,n-1)*0.25)+LDVSTK(12,N-1)) * 1000000.
      PREDSTK(7,N) = ((ldvstk( 5,n-1)*0.1 +ldvstk( 6,n-1))*0.25+ldvstk( 4,n-1)+ldvstk( 7,n-1)+ldvstk(15,n-1)) * 1000000.
      PREDSTK(8,N) =   LDVSTK(14,n-1) * 1000000.

!      if(curcalyr.ge.2020) then
	  if(curcalyr.le.stockyr) then
        do ifuel=2,maxfuel
          Sta_rat(ifuel) = sta_rat_base(ifuel) + (predstk(ifuel,n)/predstk(1,n))*sta_rat(1)
        enddo
      endif
		  
!...  Calculate the total number of refueling stations needed based on an historic
!...  ratio of vehicle stock per refueling station
      do ifuel=1,maxfuel
        ALTSTAT(ifuel,N) = ALTSTAT(ifuel,N-1)+((PREDSTK(ifuel,n)-PREDSTK(ifuel,n-1))/STA_RAT(ifuel))
      enddo
      
!...  Regionalize the predicted stations by regional vehicle sales [ldv_sales(maxgroup,maxclass,maxldv,mnumcr,mnumyr)]
      do iregn=1,mnumcr-2 
!...    Gasoline
        FUELVSAL(iregn,1,n) = sum(ldv_sales(:,:,[1,16],iregn,n-1))
!...    Diesel
        FUELVSAL(iregn,2,n) = sum(ldv_sales(:,:,[2,8],iregn,n-1))
!...    Ethanol
        FUELVSAL(iregn,3,n) = sum(ldv_sales(:,:,3,iregn,n-1))   
!...    Methanol
        FUELVSAL(iregn,4,n) = sum(ldv_sales(:,:,13,iregn,n-1)) 
!...    CNG
        FUELVSAL(iregn,5,n) = sum(ldv_sales(:,:,[9,11],iregn,n-1)) 
!...    LPG
        FUELVSAL(iregn,6,n) = sum(ldv_sales(:,:,[10,12],iregn,n-1))
!...    Electric
        FUELVSAL(iregn,7,n) = sum(ldv_sales(:,:,[4,5,6,7,15],iregn,n-1))
!...    Hydrogen
        FUELVSAL(iregn,8,n) = sum(ldv_sales(:,:,14,iregn,n-1))
      enddo

!...  calculate total U.S. sales
      do ifuel=1,maxfuel
        FUELVSALT(ifuel,n) = sum(FUELVSAL(1:mnumcr-2,ifuel,n))
      enddo

      do ifuel=1,maxfuel
        do iregn=1,mnumcr-2
          AFVSHREG(iregn,ifuel,n)=0.0
          if(FUELVSALT(ifuel,n).ne. 0.0) AFVSHREG(iregn,ifuel,n) = FUELVSAL(iregn,ifuel,n)/FUELVSALT(ifuel,n)
        enddo
      enddo

      do iregn=1,mnumcr-2
        do ifuel=1,maxfuel
            ALTSTA(iregn,ifuel,n) = ALTSTAT(ifuel,n) * AFVSHREG(iregn,ifuel,n)
        enddo
      enddo		
    endif

!...Estimate fuel availability
    do iregn=1,mnumcr-2 
      do ifuel=1,maxfuel
!        LDV_EV(n,iregn) = (sum(LDV_STOCK(iregn,1:maxvtyp,1:maxowner,4,1:maxage,1:maxhav,n-1)) + & !ev100
!                           sum(LDV_STOCK(iregn,1:maxvtyp,1:maxowner,7,1:maxage,1:maxhav,n-1)) + & !ev200
!                           sum(LDV_STOCK(iregn,1:maxvtyp,1:maxowner,15,1:maxage,1:maxhav,n-1))) !ev300                                                                       

!        if (n.ge.CHR_STR_YR.and.n.le.CHR_LST_YR) then
!          do ichrg=1, MAXCHRG 
!            PRT_proj(ichrg,n,iregn) =  PRT_CNT(ichrg,n,iregn)
!          enddo
!        endif
            
!        if (n.gt.CHR_LST_YR) then  
!          do ichrg=1, MAXCHRG    !ichrg 1 = DC, 2 = L1, 3 = L2                       
!            if (ichrg.eq.1.or.ichrg.eq.3) then
!!...            For DC and L2 chargers - S-curve = max - 1/(1/(MAX-MIN)) + exp(C+R*LDV_E*(a/min2))
!                s_curve(ichrg,n,iregn) = PRT_VAR(ichrg,2,iregn) - 1/((1/(PRT_VAR(ichrg,2,iregn)-PRT_VAR(ichrg,1,iregn))) + &
!                exp(PRT_VAR(ichrg,4,iregn)+PRT_VAR(ichrg,3,iregn)*LDV_EV(n,iregn)/PRT_VAR(ichrg,5,iregn))) 
!            else 
!!...            For L1 chargers
!                if (n.eq.CHR_LST_YR+1) s_curve(ichrg,n,iregn) = PRT_VAR(ichrg,1,iregn)*PRT_VAR(ichrg,3,iregn)
!                if (n.gt.CHR_LST_YR+1) s_curve(ichrg,n,iregn) = s_curve(ichrg,n-1,iregn)*PRT_VAR(ichrg,3,iregn)                            
!            endif              
!!...        Historic + Projected station count 
!            PRT_proj(ichrg,n,iregn) = LDV_EV(n,iregn)*1000000.0*s_curve(ichrg,n,iregn)
!          enddo
!        endif
!      enddo
        
        if (ifuel.eq.7) then
          if (n.ge.CHR_STR_YR.and.n.le.CHR_LST_YR) then
!           If doing NOCAFE side case, and we're beyond pseudo-history (calibrated to Ward's YTD, shouldn't change across side cases), use the endogenous station build estimate (based on stock growth)
            if (CAFEMY27_SWITCH.eq.0.and.n.gt.epalyr+1) then
              favail(ifuel,n,iregn) = favail(ifuel,n-1,iregn) * &
                                      (((sum(LDV_STOCK(iregn,:,:,[4,7,15],:,:,n-1))/sum(LDV_STOCK(iregn,:,:,:,:,:,n-1))) / (sum(LDV_STOCK(iregn,:,:,[4,7,15],:,:,n-2))/sum(LDV_STOCK(iregn,:,:,:,:,:,n-2)))-1) &
                                       /2 + 1)
!           Otherwise, stick with the exogenous station build estimates
            else
!...          Gasoline refueling capacity/throughput
              gas_tput_hr(iregn) = (INITSTA(1,n,iregn)*gas_stat)*6.
!...          EV refueling capacity/throughput (EVs charged per hour)
              do ichrg=1, MAXCHRG 
                port_time(ichrg,iregn) = (PRT_CNT(ichrg,n,iregn)/PRT_RT(ichrg))
              enddo
              port_time_tot(iregn) = sum(port_time(:,iregn))
              
!...          Calculate fuel availability
              favail(ifuel,n,iregn) = port_time_tot(iregn)/gas_tput_hr(iregn)
            endif
!...      Grow fuel availability proportionally to BEV share of total on-road stock (post-2032)
          else
            favail(ifuel,n,iregn) = favail(ifuel,n-1,iregn) * &
                                    (((sum(LDV_STOCK(iregn,:,:,[4,7,15],:,:,n-1))/sum(LDV_STOCK(iregn,:,:,:,:,:,n-1))) / (sum(LDV_STOCK(iregn,:,:,[4,7,15],:,:,n-2))/sum(LDV_STOCK(iregn,:,:,:,:,:,n-2)))-1) &
                                     /2 + 1)
!            WRITE(21,*) IREGN, N,'FAVAIL=', FAVAIL(7,N,IREGN)                           
          endif
        else 
          if(ALTSTA(iregn,1,n).gt.0.0) then
            FAVAIL(ifuel,n,iregn) = ALTSTA(iregn,ifuel,n)/ALTSTA(iregn,1,n)
            if (ifuel.eq.8) FAVAIL(ifuel,n,iregn) = MIN(FAVAIL(ifuel,n,iregn),FAVAIL(ifuel,n-1,iregn)*1.1)  ! Limit H2 infra growth sans policy 
          endif
          if(curcalyr.gt.2012) FAVAIL(ifuel,n,iregn) = MAX(FAVAIL(ifuel,n,iregn),FAVAIL(ifuel,n-1,iregn))
        endif

!...    Set the availability of CNG from the value given by ngtdm         
        if(curcalyr.gt.2012.and.ifuel.eq.5)then
          if(CNGAVAIL(iregn,n).gt.FAVAIL(ifuel,n,iregn))then
            FAVAIL(ifuel,n,iregn) = CNGAVAIL(iregn,n)
          else
            CNGAVAIL(iregn,n) = FAVAIL(ifuel,n,iregn)
          endif
        endif
                                                                         
!...    Beginning in 2012 set hydrogen infrastructure equal to CNG if the HMM is off
        if(curcalyr.ge.2012.and.ifuel.eq.8.and.exh.eq.0) FAVAIL(8,N,IREGN) = FAVAIL(5,N,IREGN)	! MDRAEO2025 -- CHANGE WHEN HMM IN

!...    Do not allow any fuel availability to be larger than gasoline (100%)
        FAVAIL(ifuel,n,iregn) = min(FAVAIL(ifuel,n,iregn),FAVAIL(1,n,iregn))
      enddo
    enddo

!...Re-align indices (ILDV=1-16) for fuel availability
    do iregn=1,mnumcr-2 
      FAVL( 1,iregn,yrs) = FAVAIL(1,n,iregn)
      FAVL( 2,iregn,yrs) = FAVAIL(2,n,iregn)
      FAVL( 3,iregn,yrs) = max(FAVAIL(3,n,iregn),FAVAIL(1,n,iregn))
      FAVL( 4,iregn,yrs) = FAVAIL(7,n,iregn) 
      FAVL( 5,iregn,yrs) = max(FAVAIL(7,n,iregn),FAVAIL(1,n,iregn))
      FAVL( 6,iregn,yrs) = max(FAVAIL(7,n,iregn),FAVAIL(1,n,iregn))
      FAVL( 7,iregn,yrs) = FAVAIL(7,n,iregn)
      FAVL( 8,iregn,yrs) = max(FAVAIL(7,n,iregn),FAVAIL(2,n,iregn))
      FAVL( 9,iregn,yrs) = max(FAVAIL(5,n,iregn),FAVAIL(1,n,iregn))
      FAVL(10,iregn,yrs) = max(FAVAIL(6,n,iregn),FAVAIL(1,n,iregn))
      FAVL(11,iregn,yrs) = FAVAIL(5,n,iregn)
      FAVL(12,iregn,yrs) = FAVAIL(6,n,iregn)
      FAVL(13,iregn,yrs) = FAVAIL(4,n,iregn)
      FAVL(14,iregn,yrs) = FAVAIL(8,n,iregn)
      FAVL(15,iregn,yrs) = FAVAIL(7,n,iregn) 
      FAVL(16,iregn,yrs) = FAVAIL(1,n,iregn)
    enddo
       
  RETURN
  END SUBROUTINE TALT2

! ==========================================================================================================
! ... Subroutine TALT2X calculates Level 1 and Level 2 light vehicle market penetration estimates in the AFV 
! ... model.
! ==========================================================================================================
    SUBROUTINE TALT2X
    USE T_
    IMPLICIT NONE

      REAL*8        ETOT(5),UISUM(6),ESUM(6),XSHARE(5,6),GCOST(5),GENCOST, YSHARE(5)

      INTEGER       JG,JT
      INTEGER       L2GROUP(5,6),L2GRPTOT(5)
      real cof(12),var(12),rst(13),TmpShr,TmpPlug,TmpPlug1,TmpPlug2
      integer DoBug
      character*20 TAltLabel(16)
      data TAltLabel/'Gasoline','ADV Diesel','Ethanol Flex','EV100','PHEV20','PHEV50', &
       'EV200','Diesel Hybrid','CNG Bi-Fuel','LPG Bi-Fuel','CNG','LPG','FC Methanol', &
       'FC Hydrogen','EV300','Gasoline Hybrid'/

      DATA (L2GROUP(1,JT),JT=1,6) / 1, 2, 3, 9,10,16/  ! conventional (level 1 logit)
      DATA (L2GROUP(2,JT),JT=1,6) / 5, 6, 8, 0, 0, 0/  ! elec. hybrid (level 1)
      DATA (L2GROUP(3,JT),JT=1,6) /11,12, 0, 0, 0, 0/  ! dedicated gaseous(level 1) 
      DATA (L2GROUP(4,JT),JT=1,6) /13,14, 0, 0, 0, 0/  ! fuel cell (level 1) 
      DATA (L2GROUP(5,JT),JT=1,6) / 4, 7,15, 0, 0, 0/  ! electric vehicle (level 1) 
      DATA (L2GRPTOT(JG) ,JG=1,5) / 6, 3, 2, 2, 3/     ! lookup table index for size of group

      REAL          XGS(MAXLDV),XCOST
      CHARACTER*15  XT(MAXLDV)

! ... Calculate ORNL NMLM utility for AFV (ILDV=1-16)
      ifuelx = 1
      DO JG=1,5
        DO JT=1,L2GRPTOT(JG)
          XT(ifuelx) = FTYPELABEL(L2GROUP(JG,JT))
          ifuelx = ifuelx + 1
        ENDDO
      ENDDO

! ... load consumer choice model coefficients
	  do inmlm=1,maxnmlm
	    do icl=1,maxclass 
		  do igp=1,cargrp
		    nmlmco(inmlm,icl,igp) = nmlmcocar(inmlm,icl,igp)
		  enddo
		  do igp=ltkgrp,maxgroup   
		    nmlmco(inmlm,icl,igp) = nmlmcotrk(inmlm,icl,igp-cargrp)			   
		  enddo
		enddo 
	  enddo

	  do icl=1,maxclass
		do ildv=1,maxldv 
		  do iregn=1,mnumcr-2   
            X210(1,icl,ildv,iregn)  = atvcocar1(ildv,iregn,icl)
			X210(2,icl,ildv,iregn)  = atvcocar2(ildv,iregn,icl)
			X210(3,icl,ildv,iregn)  = atvcocar3(ildv,iregn,icl)
			X210(4,icl,ildv,iregn)  = atvcocar4(ildv,iregn,icl)
			X210(5,icl,ildv,iregn)  = atvcocar5(ildv,iregn,icl)
			X210(6,icl,ildv,iregn)  = atvcotrk1(ildv,iregn,icl)
			X210(7,icl,ildv,iregn)  = atvcotrk2(ildv,iregn,icl)  
			X210(8,icl,ildv,iregn)  = atvcotrk3(ildv,iregn,icl)  
			X210(9,icl,ildv,iregn)  = atvcotrk4(ildv,iregn,icl)
			X210(10,icl,ildv,iregn) = atvcotrk5(ildv,iregn,icl)
			X210(11,icl,ildv,iregn) = atvcotrk6(ildv,iregn,icl)
            
!           Calibrate pseudo-historical year (AEO2025: 2024)
            if (curcalyr.ge.epalyr+1) then
              do igp=1,maxgroup
                ivtyp = GrpMap(igp)
                if(X210(igp,icl,ildv,iregn).gt.0.0) then
                  X210(igp,icl,ildv,iregn) = X210(igp,icl,ildv,iregn) * ATVCOEF_CALIB(ildv,ivtyp)
                else
                  X210(igp,icl,ildv,iregn) = X210(igp,icl,ildv,iregn) * (1-ATVCOEF_CALIB(ildv,ivtyp)+1)
                endif
              enddo
            endif

!           Calibrate 2025 sales (avoid HEV sales dropout, and maintain momentum), maintain calibration factor through projection
!           Lean manufacturers/consumers further and further toward hybrids versus gasoline vehicles due to decline in consumer resistance to hybridization
            do igp = 1,maxgroup
              if (ildv.eq.16.and.mmavail(igp,icl,16,iregn,yrs).gt.0.0) then
                if (curcalyr.ge.epalyr+2) X210(igp,icl,ildv,iregn) = X210(igp,icl,ildv,iregn) + 0.2
                if (curcalyr.gt.epalyr+2) X210(igp,icl,ildv,iregn) = X210(igp,icl,ildv,iregn) + 0.75*(curcalyr-(epalyr+2.0))/(mnumyr+1989.0-(epalyr+2.0))
              endif
            enddo
          
          enddo
        enddo
      enddo

! ... Temporarily set battery replacement cost to zero to nullify its impact on
! ... market penetrations.  If/when battery replacement costs are extracted from
! ... overall maintenance costs, this initialization should be removed in favor
! ... of a more appropriate alternative.
      DO IGP=1,MAXGROUP
        DO ICL=1,MAXCLASS
          DO ILDV=1,MAXLDV
            BRCOST25(IGP,ILDV,ICL,YRS) = 0.0
          ENDDO
        ENDDO
      ENDDO

      DO IREGN=1,(MNUMCR-2)
        DO IGP=1,MAXGROUP
		  IVTYP=grpmap(IGP)
          DO ICL=1,MAXCLASS
            DO JG=1,5 ! level 1 
              ETOT(JG) = 0.0
              DO JT=1,L2GRPTOT(JG)        !tech within group
                ILDV = L2GROUP(JG,JT)
                UISUM(JT) = 0.0
!                IF (PSPR(igp,ildv,icl,iregn,yrs) .LT. 0.0) STOP 450

! ... Calculate value functions.  Because of the formulation, FEMRNG must > 0
! ... The importance of this should not be underestimated, it is more than a simple divide
! ... by zero precaution.  The requirement for a non-zero range estimate actually allows
! ... the ATV penetration model to properly bypass non-existant vehicle classes (assuming,
! ... of course, that the ranges for these classes are set to zero).

                if(mmavail(igp,icl,ildv,iregn,yrs).gt.0.0) then

                  IF (PSPR(igp,ildv,icl,iregn,yrs) .LT. 0.0) then
                    WRITE(21,'(a,",",5(a4,","),4(a12,","))')'ERROR: negative price','year','iter','grp','icl','ildv','pspr','fempri','pspr_prev','fempri_prev'
                    WRITE(21,'(a,",",5(i4,","),4(f12.2,","))')'ERROR: negative price', curcalyr,curitr,igp,icl,ildv,PSPR(igp,ildv,icl,iregn,yrs),fempri(igp,icl,yrs,ildv),&
                                                                                       PSPR(igp,ildv,icl,iregn,yrs-1),fempri(igp,icl,yrs-1,ildv)
                    WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
                    STOP 450
                  endif


                  !Gather data for the debug writes.
				  cof(1)  = nmlmco(1,icl,igp);  var(1)=pspr(igp,ildv,icl,iregn,yrs); rst(1)=cof(1)*var(1)
				  cof(2)  = nmlmco(2,icl,igp);  var(2)=FlCost(igp,ildv,icl,iregn,yrs); rst(2)=cof(2)*var(2)	
				  cof(3)  = nmlmco(3,icl,igp);  var(3)=vrng(igp,ildv,icl,iregn,yrs); rst(3)=cof(3)*(1.0/var(3))	
				  cof(4)  = nmlmco(4,icl,igp);  var(4)=brcost25(igp,ildv,icl,yrs); rst(4)=cof(4)*var(4)
				  cof(5)  = nmlmco(5,icl,igp);  var(5)=accl(igp,ildv,icl,iregn,yrs); rst(5)=cof(5)*var(5)
				  cof(6)  = nmlmco(6,icl,igp);  var(6)=hfuel(igp,ildv,icl,iregn,yrs); rst(6)=cof(6)*var(6)			
				  cof(7)  = nmlmco(7,icl,igp);  var(7)=maint(igp,ildv,icl,iregn,yrs); rst(7)=cof(7)*var(7)
				  cof(8)  = nmlmco(8,icl,igp);  var(8)=lugg(igp,ildv,icl,iregn); rst(8)=cof(8)*var(8)
				  cof(9)  = nmlmco(10,icl,igp); var(9)=FAvl(ildv,iregn,yrs); rst(9)=0.0
				  cof(10) = nmlmco(9,icl,igp);  var(10)=0.0; rst(10)=cof(10)*exp(cof(9)*var(9))
				  cof(11) = nmlmco(11,icl,igp); var(11)=mmavail(igp,icl,ildv,iregn,yrs); rst(11)=cof(11)*alog(var(11))
                  cof(12) = x210(igp,icl,ildv,iregn); var(12)=0.0; rst(12)=cof(12)

                  if(ILDV.ne.9.and.ILDV.ne.10) then

! ... Value function for all technologies except group 1 (conventional) FFV's and bi-fuel's

                    UISUM(JT) = nmlmco(1,icl,igp) * PSPR(igp,ildv,icl,iregn,yrs) +   				& 
                                nmlmco(2,icl,igp) * FlCost(igp,ildv,icl,iregn,yrs) + 				& 
                                nmlmco(3,icl,igp) * (1/vrng(igp,ildv,icl,iregn,yrs)) + 			 	& 
                                nmlmco(4,icl,igp) * BRCOST25(igp,ildv,icl,yrs) +     				&
                                nmlmco(5,icl,igp) * ACCL(igp,ildv,icl,iregn,yrs) +         			& 
                                nmlmco(6,icl,igp) * HFUEL(igp,ildv,icl,iregn,yrs) +        			&
                                nmlmco(7,icl,igp) * MAINT(igp,ildv,icl,iregn,yrs) +        			&
                                nmlmco(8,icl,igp) * LUGG(igp,ildv,icl,iregn) +             			&
                                nmlmco(9,icl,igp) * EXP(nmlmco(10,icl,igp)*FAvl(ildv,iregn,yrs)) +  &
                                nmlmco(11,icl,igp) * ALOG(MMAVAIL(igp,icl,ildv,iregn,yrs)) +  	 	&
                                X210(igp,icl,ildv,iregn)

                  ELSE
! ... Value function for FFV's and bi-fuels.  Note: fuel availability and range are
! ... calculated in call statements w/case(3), ..., case(6) and are in the generalized
! ... cost calculation, gencost.
                    XCOST = -10.0
                    select case (ILDV)
                      CASE(9) ! CNG bifuel
                        CALL TALT315(XCOST,nmlmco(13,icl,igp), &
                                           nmlmco( 2,icl,igp), &
                                           nmlmco( 3,icl,igp), &
                                           nmlmco( 9,icl,igp), &
                                           nmlmco(10,icl,igp))
                      CASE(10) ! LPG bifuel
                        CALL TALT316(XCOST,nmlmco(13,icl,igp), &
                                           nmlmco( 2,icl,igp), &
                                           nmlmco( 3,icl,igp), &
                                           nmlmco( 9,icl,igp), &
                                           nmlmco(10,icl,igp))
                      CASE DEFAULT
                        STOP 440
                    END SELECT

                    UISUM(JT) = nmlmco(1,icl,igp)  * PSPR(igp,ildv,icl,iregn,yrs) +          & 
                                nmlmco(2,icl,igp)  * XCOST +                                 & 
                                nmlmco(4,icl,igp)  * BRCOST25(igp,ildv,icl,yrs) +            & 
                                nmlmco(5,icl,igp)  * ACCL(igp,ildv,icl,iregn,yrs) +          & 
                                nmlmco(6,icl,igp)  * HFUEL(igp,ildv,icl,iregn,yrs) +         & 
                                nmlmco(7,icl,igp)  * MAINT(igp,ildv,icl,iregn,yrs) +         &
                                nmlmco(8,icl,igp)  * LUGG(igp,ildv,icl,iregn) +              &
                                nmlmco(11,icl,igp) * ALOG(MMAVAIL(igp,icl,ildv,iregn,yrs)) + &
                                X210(igp,icl,ildv,iregn)	

                    !Overwrite some data gathered for the debug writes.
					cof(2)=nmlmco(2,icl,igp); var(2)=xcost; rst(2)=cof(2)*var(2)
                    rst(3)=0.0
                    rst(10)=0.0
                  ENDIF

                  ESUM(JT) = 0.0
                  IF (UISUM(JT) .NE. 0.0) ESUM(JT) = DEXP(UISUM(JT))

                ELSE
                  ESUM(JT) = 0.0
                ENDIF

                ETOT(JG) = ETOT(JG) + ESUM(JT)

                !Sum up all the utilities (should be the same sum as uisum).
                rst(13)=rst(1)+rst(2)+rst(3)+rst(4)+rst(5)+rst(6)+rst(7)+rst(8)+rst(9)+rst(10)+rst(11)+rst(12)
                !Create an overall decision variable for the debugs (comprises all the decisions in one variable).
                DoBug=0
!                if(n.eq.(ijumpyr-10).or.n.eq.ijumpyr) then
!                 if(fcrl.eq.1.and.pass.eq.2.and.passno.eq.2.and.iregn.eq.5) then
!                  if(igp.eq.1.and.ICL.eq.5) then
!                   DoBug=1
!                  endif
!                 endif
!                endif
				if(curcalyr.gt.2023.and.curcalyr.lt.2026) then !Dobug=1			JMA LOGIT DEBUG
                !Write out details for each technology in each group.
                if(DoBug.eq.1) then
                 if(jt.eq.1) write(H2UNIT,'(/,a,i3,a,4i5,a)') 'Logit Nest: ',jg,'  (year, region, grp, class: ',n+1989,iregn,igp,ICL,')'
                 if(ESum(jt).eq.0.0) then
                  write(H2UNIT,'(a,a,i3,a)') taltlabel(ILDV),' (tech: ',ILDV,') Tech excluded because VRng=0'
                 else
                  write(H2UNIT,'(a,a,i3,a)') taltlabel(ILDV),' (tech: ',ILDV,')'
                  write(H2UNIT,'(a,a)') '      PSPR    FlCost      VRng  BrCost25      Accl     HFuel     ', &
                   'Maint      Lugg     FAvl1     FAvl2   MMAvail     Const'
                  write(H2UNIT,'(f10.5,f10.4,f10.2,f10.5,2f10.4,f10.5,2f10.4,f10.3,2f10.4)') (cof(i),i=1,12)
                  write(H2UNIT,'(f10.1,f10.3,f10.1,f10.5,f10.3,f10.5,f10.2,2f10.3,f10.2,f10.4,f10.2)') (var(i),i=1,12)
                  write(H2UNIT,'(12f10.2)') (rst(i),i=1,12)
                  write(H2UNIT,'(a,f12.3,a,f12.3,a)') '  Total utility: ',rst(13),'  (Check total: ',uisum(jt),')'
                 end if
               end if
               endif

              ENDDO !JT 

! ... Level 2 shares
              DO JT=1,L2GRPTOT(JG)
                XSHARE(JG,JT) = 0.0
                IF (ETOT(JG) .NE. 0.0) XSHARE(JG,JT) = ESUM(JT)/ETOT(JG) * 100.0
! ... Write out details for the technology shares in the group.
                if(DoBug.eq.1.and.curcalyr.gt.2023.and.curcalyr.lt.2026) then
                 if(ESum(jt).eq.0.0) then
                  write(H2UNIT,'(a,3i3,2x,a,10x,a,f12.5)') '  Group Share: ',jg,jt,l2group(jg,jt),taltlabel(l2group(jg,jt)),'na',xshare(jg,jt)/100.0
                 else
                  write(H2UNIT,'(a,3i3,2x,a,f12.3,f12.5)') '  Group Share: ',jg,jt,l2group(jg,jt),taltlabel(l2group(jg,jt)),UISum(jt),xshare(jg,jt)/100.0
                 end if
               end if
              ENDDO

              GCOST(JG) = 0.0
              IF (ETOT(JG) .NE. 0.0) GCOST(JG) = (1/nmlmco(1,icl,igp)) * DLOG(ETOT(JG))

            ENDDO !JG

! ... Level 1 shares
            ETOT(1) = 0.0

            DO JG=1,5
              UISUM(JG) = 0.0
			  UISUM(JG) = nmlmco(12,icl,igp) * GCOST(JG)
              ESUM(JG) = 0.0
              IF (UISUM(JG) .NE. 0.0) ESUM(JG) = DEXP(UISUM(JG))
              ETOT(1) = ETOT(1) + ESUM(JG)
            ENDDO

            ETOT(2) = 0.0

            DO JG=1,5
              YSHARE(JG) = 0.0
              IF (ETOT(1) .NE. 0.0) YSHARE(JG) = ESUM(JG)/ETOT(1) * 100.0
              ETOT(2) = ETOT(2) + YSHARE(JG)
! ... Write out details for the overall shares for each of the groups.
              if(DoBug.eq.1.and.curcalyr.gt.2023.and.curcalyr.lt.2026) then
               if(jg.eq.1) then
                write(H2UNIT,'(/,a,4i5,a)') 'Overall Group Shares:  (year, region, grp, class: ',n+1989,iregn,igp,ICL,')'
                write(H2UNIT,'(2x,a,2f10.6)') 'x21 and x11:   ',nmlmco(1,icl,igp),nmlmco(12,icl,igp)
                write(H2UNIT,'(7x,a)') '     GCost     TUtil     Share'
               end if
               write(H2UNIT,'(2x,i3,2x,f10.1,f10.3,f10.5)') jg,gcost(jg),uisum(jg),yshare(jg)/100.0
              end if
            ENDDO

            GENCOST = 0.0
!            IF (ETOT(1) .NE. 0.0) GENCOST = (1/X11(ICL,igp)) * DLOG(ETOT(1))
            IF (ETOT(1) .NE. 0.0) GENCOST = (1/nmlmco(12,icl,igp)) * DLOG(ETOT(1))
            IFUELX = 1

            DO JG=1,5
              DO JT=1,L2GRPTOT(JG)
                XGS(ifuelx) = XSHARE(JG,JT) * YSHARE(JG) / 100.0
                APShrGrp(igp,ICL,iregn,L2Group(jg,jt),n)=xgs(ifuelx)/100 
                ifuelx = ifuelx + 1
			  ENDDO
            ENDDO
          ENDDO
        ENDDO

!...	if stringent cafe standards then over write consumer choice with powertrain needed for compliance 
		if(pass.ge.2) then
		if(curcalyr.ge.2033.and.CAFEMY27_SWITCH.eq.1) then 
		  do igp=1,maxgroup 
			do icl=1,maxclass
		      do ildv=2,maxldv
				if(apshrgrp(igp,icl,iregn,ildv,n-1).gt.apshrgrp(igp,icl,iregn,ildv,n)) then
				  apshrgrp(igp,icl,iregn,ildv,n) = max(apshrgrp(igp,icl,iregn,ildv,n-1),apshrgrp(igp,icl,iregn,ildv,n)) 
				endif
			  enddo
			  if(apshrgrp(igp,icl,iregn,1,n).ne.0.0) then
			    apshrgrp(igp,icl,iregn,1,n) = 1.0 - sum(apshrgrp(igp,icl,iregn,2:maxldv,n))
			  else
			    do ildv=2,maxldv
				  if(sum(apshrgrp(igp,icl,iregn,2:maxldv,n)).gt.0.0) then
				    apshrgrp(igp,icl,iregn,ildv,n) = apshrgrp(igp,icl,iregn,ildv,n)/sum(apshrgrp(igp,icl,iregn,1:maxldv,n))
				  endif
				enddo
			  endif
			enddo
		  enddo
		endif
		endif



!...	calculate regional sales by Group to calculate CAFE
	    do igp=1,maxgroup
		  do icl=1,maxclass 
		    do ildv=1,maxldv 
			  ldv_sales(igp,icl,ildv,iregn,n) = APShrGrp(igp,icl,iregn,ildv,n) * mfr_sales(iregn,igp,icl,n)
!              WRITE(21,'(a,",",4(i4,","),4(f12.3,","),f12.5)')'sales_check',curcalyr,igp,icl,ildv,ldv_sales(igp,icl,ildv,iregn,34),mfr_sales(iregn,igp,icl,34),ldv_sales(igp,icl,ildv,iregn,n),mfr_sales(iregn,igp,icl,n),APShrGrp(igp,icl,iregn,ildv,n)
              if (ldv_sales(igp,icl,ildv,iregn,n).ne.ldv_sales(igp,icl,ildv,iregn,n)) then
                write(21,'(a,",",6(a4,","),5(a12,","))')'ERROR: ldv_sales NaN','year','itr','regn','igp','icl','ildv','ldv_sales','apshrgrp','mfr_sales','class_share','GrpShare'
                write(21,'(a,",",6(i4,","),5(f12.4,","))')'ERROR: ldv_sales NaN',curcalyr,curitr,iregn,igp,icl,ildv,ldv_sales(igp,icl,ildv,iregn,n),apshrgrp(igp,icl,iregn,ildv,n),mfr_sales(iregn,igp,icl,n),&
                                                  class_share(iregn,icl,igp,yrs),GrpShare(iregn,igp,n)
                WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
                STOP
              endif
              if (ildv.eq.maxldv.and.icl.eq.maxclass.and.igp.eq.maxgroup) then
                if (SUM(ldv_sales(1:5,:,:,iregn,n)).eq.0.0.or.SUM(ldv_sales(6:11,:,:,iregn,n)).eq.0.0) then
                  WRITE(21,'(a,",",6(i4,","),4(f12.4,","))')'ERROR: ldv_sales zero',curcalyr,curitr,iregn,igp,icl,ildv,SUM(ldv_sales(1:5,:,:,iregn,n)),SUM(ldv_sales(6:11,:,:,iregn,n)),&
                                                            SUM(mfr_sales(iregn,1:5,:,n)),SUM(mfr_sales(iregn,6:11,:,n))
                  WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
                  STOP
                endif
              endif
            enddo
		  enddo
		enddo		
	  enddo ! mnumcr-2
	  
!debug jma	  
	  do iregn=1,mnumcr-2 
		do igp=1,maxgroup 
		  do icl=1,maxclass
		    do ildv=1,maxldv 
			  if(apshrgrp(igp,icl,iregn,ildv,n).gt.0.0.and.ownsaletemp(1,igp,icl,iregn,n).eq.0.0) then
			    write(21,'(a,5(i4,","),3(f12.5,","))')'ownsaletemp error',curcalyr,iregn,igp,icl,ildv,apshrgrp(igp,icl,iregn,ildv,n),ldv_sales(igp,icl,ildv,iregn,n),mfr_sales(iregn,igp,icl,n)
			  endif 
			enddo 
		  enddo 
		enddo 
	  enddo
	  
!...  calculate total US sales
	  do igp=1,maxgroup 
	    do icl=1,maxclass 
		  do ildv=1,maxldv
			ldv_sales(igp,icl,ildv,mnumcr,n) = sum(ldv_Sales(igp,icl,ildv,1:mnumcr-2,n))
		  enddo 
		enddo 
	  enddo

!...  fill cafesales
	  do igp=1,maxgroup 
	    do icl=1,maxclass 
		  do ildv=1, maxldv 
		    cafesales(igp,icl,yrs,ildv) = ldv_sales(igp,icl,ildv,mnumcr,n) 
		  enddo 
		enddo 
	  enddo 
	  
!!...  fill ttltechsal
!	  do iregn=1,mnumcr
!		do icl=1,maxclass 
!		  do ildv=1,maxldv 
!		    if(iregn.ne.10) then 
!			  ttltechsal(iregn,1,icl,ildv,n) = sum(ldv_sales(1:cargrp,icl,ildv,iregn,n))
!			  ttltechsal(iregn,2,icl,ildv,n) = sum(ldv_sales(ltkgrp:maxgroup,icl,ildv,iregn,n))
!			endif 
!		  enddo 
!		enddo 
!	  enddo
	  
!...  fill household vehicle sales by mfr group 
	  do igp=1,maxgroup 
		do icl=1,maxclass 
		  do ildv=1,maxldv 
			do iregn=1,mnumcr-2
!			  hhgrpsal(iregn,igp,icl,ildv,n) = ldv_sales(igp,icl,ildv,iregn,n) * ownsalesshr(1,igp,icl,ildv,iregn,n)   !jma
			  hhgrpsal(iregn,igp,icl,ildv,n) = ldv_sales(igp,icl,ildv,iregn,n) * ownsaletemp(1,igp,icl,iregn,n)
			enddo 
!...	    US sales 
		    hhgrpsal(mnumcr,igp,icl,ildv,n) = sum(hhgrpsal(1:mnumcr-2,igp,icl,ildv,n)) 
		  enddo 
		enddo 
	  enddo
	  
!...  fill household sales by vehicle type	  
	  do icl=1,maxclass 
	    do ildv=1,maxldv 
		  do iregn=1,mnumcr-2 
		    hhtechsal(iregn,1,icl,ildv,n) = sum(hhgrpsal(iregn,1:cargrp,icl,ildv,n))
		    hhtechsal(iregn,2,icl,ildv,n) = sum(hhgrpsal(iregn,ltkgrp:maxgroup,icl,ildv,n))			
		  enddo 
		  do ivtyp=1,maxvtyp 
		    hhtechsal(mnumcr,ivtyp,icl,ildv,n) = sum(hhtechsal(1:mnumcr-2,ivtyp,icl,ildv,n)) 
		  enddo 
		enddo 
	  enddo

!...  calculate national sales shares
      do igp=1,maxgroup
		do icl=1,maxclass 
		  do ildv=1,maxldv 	  
			APShrGrp(igp,icl,mnumcr,ildv,n) = ldv_sales(igp,icl,ildv,mnumcr,n)/sum(ldv_Sales(igp,icl,1:maxldv,mnumcr,n))
		  enddo
		enddo 
	  enddo
  
    RETURN
    END SUBROUTINE TALT2X

! ==========================================================================================================
! ... Subroutine TALT313 calculates fuel cost, vehicle range, and fuel availability for methanol flex 
! ... vehicles
! ...                                                               
! ...   NOTE: LEVEL 3 FOR LEVEL 2 GROUP 1, TECH 3                   
! ...         GROUP 1 : Conventional Liquid Fuel Capable Vehicles   
! ...         TECH 3 : FFV MeOH                                     
! ==========================================================================================================

!...CURRENTLY NOT USED

    SUBROUTINE TALT313 (GENCOST,X3131,XFC,XR,XB,XB2)
    USE T_
    IMPLICIT NONE

      INTEGER       LVL3IT
      PARAMETER    (LVL3IT = 2)

      REAL          X3131               ! coefficient for level 3, group 1, tech 3, fuel cost
      REAL          X3132               ! coefficient for level 3, group 1, tech 3, range
      REAL          BETAFA313           ! coefficient for level 3, group 1, tech 3, parameter fuel availability
      REAL          BETAFA2313          ! coefficient for level 3, group 1, tech 3, parameter fuel availability

      REAL          FLCOST313(LVL3IT)   ! fuel cost for level 3, group 1, tech 3
      REAL          VRANG313(LVL3IT)    ! range for level 3, group 1, tech 3
      REAL          FAVAL313(LVL3IT)    ! fuel availability for level 3, group 1, tech 3

      REAL          UISUM(LVL3IT),ESUM(LVL3IT),ETOT,GENCOST

      REAL          XFC                 ! coefficient from level 2, fuel cost
      REAL          XR                  ! coefficient from level 2, range
      REAL          XB                  ! coefficient from level 2, parameter fuel availability
      REAL          XB2                 ! coefficient from level 2, parameter fuel availability

      X3132 = X3131 * XR / XFC
      BETAFA313 = X3131 * XB / XFC
      BETAFA2313 = XB2

! ... VL3IT = 1 - Gasoline
      FLCOST313(1) = FLCOST(igp,1,ICL,IREGN,YRS)	
      VRANG313(1) =  1 / vrng(igp,gas,icl,iregn,yrs)		
      FAVAL313(1) = FAVL(1,IREGN,YRS)

! ... VL3IT = undefined
      FLCOST313(2) = FLCOST(igp,6,ICL,IREGN,YRS)	
      VRANG313(2) =  1 / vrng(igp,6,icl,iregn,yrs)		
      FAVAL313(2) = FAVL(1,IREGN,YRS)

      ETOT = 0

      DO K=1,LVL3IT
        UISUM(K) = 0.0
        UISUM(K) = X3131 * FLCOST313(K) + X3132 * VRANG313(K) + &
                   BETAFA313 * EXP(BETAFA2313*FAVAL313(K))
        ESUM(K) = EXP(UISUM(K))
        ETOT = ETOT + ESUM(K)
      ENDDO

      GENCOST = (1/X3131) * ALOG(ETOT)

    RETURN
    END SUBROUTINE TALT313

! ==========================================================================================================
! ... Subroutine TALT314 calculates fuel cost, vehicle range, and fuel availability for ethanol flex 
! ... vehicles
! ... 
! ...   NOTE: LEVEL 3 FOR LEVEL 2 GROUP 1, TECH 4                  
! ...         GROUP 1 : Conventional Liquid Fuel Capable Vehicles  
! ...          TECH 4 : FFV Ethanol                                
! ==========================================================================================================
    SUBROUTINE TALT314 (GENCOST,X3141,XFC,XR,XB,XB2)
    USE T_
    IMPLICIT NONE

      INTEGER       LVL4IT
      PARAMETER    (LVL4IT = 2)

      REAL          X3141               ! coefficient for level 3, group 1, tech 4, fuel cost
      REAL          X3142               ! coefficient for level 3, group 1, tech 4, range
      REAL          BETAFA314           ! coefficient for level 3, group 1, tech 4, parameter fuel availability
      REAL          BETAFA2314          ! coefficient for level 3, group 1, tech 4, parameter fuel availability

      REAL          FLCOST314(LVL4IT)   ! fuel cost for level 3, group 1, tech 4
      REAL          VRANG314(LVL4IT)    ! range for level 3, group 1, tech 4
      REAL          FAVAL314(LVL4IT)    ! fuel availability for level 3, group 1, tech 4

      REAL          UISUM(LVL4IT),ESUM(LVL4IT),ETOT,GENCOST

      REAL          XFC                 ! coefficient from level 2, fuel cost
      REAL          XR                  ! coefficient from level 2, range
      REAL          XB                  ! coefficient from level 2, parameter fuel availability
      REAL          XB2                 ! coefficient from level 2, parameter fuel availability

      X3142 = X3141 * XR / XFC
      BETAFA314 = X3141 * XB / XFC
      BETAFA2314 = XB2

! ... VL4IT = 1 - Gasoline
      FLCOST314(1) = FLCOST(igp,1,ICL,IREGN,YRS)	
      VRANG314(1) =  1 / vrng(igp,gas,icl,iregn,yrs)	
      FAVAL314(1) = FAVL(1,IREGN,YRS)

! ... VL4IT = 2 - E85
      FLCOST314(2) = FLCOST(igp,3,ICL,IREGN,YRS)	
      VRANG314(2) =  1 / vrng(igp,3,icl,iregn,yrs)		
      FAVAL314(2) = FAVL(4,IREGN,YRS)

      ETOT = 0.0

      DO K=1,LVL4IT
        UISUM(K) = 0.0
        UISUM(K) = X3141 * FLCOST314(K) + X3142 * VRANG314(K) + &
                   BETAFA314 * EXP(BETAFA2314*FAVAL314(K))
        ESUM(K) = EXP(UISUM(K))
        ETOT = ETOT + ESUM(K)
      ENDDO

      GENCOST = (1/X3141) * ALOG(ETOT)

    RETURN
    END SUBROUTINE TALT314

! ==========================================================================================================
! ... Subroutine TALT315 calculates fuel cost, vehicle range, and fuel availability for CNG bi-fuel vehicles
! ... 
! ...   NOTE: LEVEL 3 FOR LEVEL 2 GROUP 1, TECH 5                 
! ...         GROUP 1 : Conventional Liquid Fuel Capable Vehicles 
! ...          TECH 5 : CNG Bi-fuel                               
! ==========================================================================================================
    SUBROUTINE TALT315(GENCOST,X3151,XFC,XR,XB,XB2)
    USE T_
    IMPLICIT NONE

      INTEGER       LVL5IT
      PARAMETER    (LVL5IT = 2)

      REAL          X3151               ! coefficient for level 3, group 1, tech 5, fuel cost
      REAL          X3152               ! coefficient for level 3, group 1, tech 5, range
      REAL          BETAFA315           ! coefficient for level 3, group 1, tech 5, parameter fuel availability
      REAL          BETAFA2315          ! coefficient for level 3, group 1, tech 5, parameter fuel availability

      REAL          FLCOST315(LVL5IT)   ! fuel cost for level 3, group 1, tech 5
      REAL          VRANG315(LVL5IT)    ! range for level 3, group 1, tech 5
      REAL          FAVAL315(LVL5IT)    ! fuel availability for level 3, group 1, tech 5

      REAL          UISUM(LVL5IT),ESUM(LVL5IT),ETOT,GENCOST

      REAL          XFC                 ! coefficient from level 2, fuel cost
      REAL          XR                  ! coefficient from level 2, range
      REAL          XB                  ! coefficient from level 2, parameter fuel availability
      REAL          XB2                 ! coefficient from level 2, parameter fuel availability

      X3152 = X3151 * XR / XFC
      BETAFA315 = X3151 * XB / XFC
      BETAFA2315 = XB2

! ... VL5IT = 1 - Gasoline
      FLCOST315(1) = FLCOST(igp,1,ICL,IREGN,YRS)	
      VRANG315(1) =  1 / vrng(igp,gas,icl,iregn,yrs)	
      FAVAL315(1) = FAVL(1,IREGN,YRS)

! ... VL5IT = 2 - CNG
      FLCOST315(2) = FLCOST(igp,11,ICL,IREGN,YRS)	
      VRANG315(2) =  1 / vrng(igp,11,icl,iregn,yrs)	
      FAVAL315(2) = FAVL(9,IREGN,YRS)

      ETOT = 0.0

      DO K=1,LVL5IT
        UISUM(K) = 0.0
        UISUM(K) = X3151 * FLCOST315(K) + X3152 * VRANG315(K) + &
                   BETAFA315 * EXP(BETAFA2315*FAVAL315(K))
        ESUM(K) = EXP(UISUM(K))
        ETOT = ETOT + ESUM(K)
      ENDDO

      GENCOST = (1/X3151)*ALOG(ETOT)

    RETURN
    END SUBROUTINE TALT315

! ==========================================================================================================
! ... Subroutine TALT316 calculates fuel cost, vehicle range, and fuel availability for LPG bi-fuel vehicles 
! ... 
! ...   NOTE: LEVEL 3 FOR LEVEL 2 GROUP 1, TECH 6                
! ...         GROUP 1 : Conventional Liquid Fuel Capable Vehicles
! ...          TECH 6 : LPG Bi-fuel                              
! ==========================================================================================================
    SUBROUTINE TALT316(GENCOST,X3161,XFC,XR,XB,XB2)
    USE T_
    IMPLICIT NONE

      INTEGER       LVL6IT
      PARAMETER    (LVL6IT = 2)

      REAL          X3161               ! coefficient for level 3, group 1, tech 6, fuel cost
      REAL          X3162               ! coefficient for level 3, group 1, tech 6, range
      REAL          BETAFA316           ! coefficient for level 3, group 1, tech 6, parameter fuel availability
      REAL          BETAFA2316          ! coefficient for level 3, group 1, tech 6, parameter fuel availability

      REAL          FLCOST316(LVL6IT)   ! fuel cost for level 3, group 1, tech 6
      REAL          VRANG316(LVL6IT)    ! range for level 3, group 1, tech 6
      REAL          FAVAL316(LVL6IT)    ! fuel availability for level 3, group 1, tech 6

      REAL          UISUM(LVL6IT),ESUM(LVL6it),ETOT,GENCOST

      REAL          XFC                 ! coefficient from level 2, fuel cost
      REAL          XR                  ! coefficient from level 2, range
      REAL          XB                  ! coefficient from level 2, parameter fuel availability
      REAL          XB2                 ! coefficient from level 2, parameter fuel availability

      X3162 = X3161 * XR / XFC
      BETAFA316 = X3161 * XB / XFC
      BETAFA2316 = XB2

! ... VL6IT = 1 - Gasoline
      FLCOST316(1) = FLCOST(igp,1,ICL,IREGN,YRS)	
      VRANG316(1) =  1 / vrng(igp,gas,icl,iregn,yrs)	
      FAVAL316(1) = FAVL(1,IREGN,YRS)

! ... VL6IT = 2 - LPG
      FLCOST316(2) = FLCOST(igp,12,ICL,IREGN,YRS)	
      VRANG316(2) =  1 / vrng(igp,12,icl,iregn,yrs)	
      FAVAL316(2) = FAVL(10,IREGN,YRS)

      ETOT = 0.0

      DO K=1,LVL6IT
        UISUM(K) = 0.0
        UISUM(K) = X3161 * FLCOST316(K) + X3162 * VRANG316(K) + &
                   BETAFA316 * EXP(BETAFA2316*FAVAL316(K))
        ESUM(K) = EXP(UISUM(K))
        ETOT = ETOT + ESUM(K)
      ENDDO

      GENCOST = (1/X3161) * ALOG(ETOT)

    RETURN
    END SUBROUTINE TALT316

! ==========================================================================================================
! ... Subroutine TFLTSTKS calculates sales and stocks of fleet vehicles
! ...   NOTE: The IFLEET index: IFLEET = 1    BUSINESS    
! ...                           IFLEET = 2    GOVERNMENT  
! ...                           IFLEET = 3    UTILITY     
! ...							IFLEET = 4    TAXI
! ==========================================================================================================
  SUBROUTINE TFLTSTKS
  USE T_
  IMPLICIT NONE

!...for 1995-stockyr read in fleet stock totals by fleet type, tech, and vintage
	if(curcalyr.le.stockyr) then
	  do ivtyp=1,maxvtyp
		do ifleet=1,maxfleet
		  do ildv=1,maxldv
		  	do iage=1,maxage
		      do iregn=1,mnumcr-2
				Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,1,n) = LDV_STOCK(iregn,ivtyp,ifleet+1,ildv,iage,1,n)*1000000.0
			  enddo
			  Flt_Stock(mnumcr,ivtyp,ifleet,ildv,iage,1,n) = sum(flt_stock(1:mnumcr-2,ivtyp,ifleet,ildv,iage,1,n))
			enddo
		  enddo
	    enddo
	  enddo
	endif 

!...fill region fleet sales (2019-stockyr)
	if(curcalyr.ge.2019.and.curcalyr.le.stockyr) then
	  do iregn=1,mnumcr-2 
		do ifleet=1,maxfleet 
		  do icl=1,maxclass 
		    do ildv=1,maxldv 
			  do igp=1,maxgroup 
 				fltgrpsal(iregn,ifleet,igp,icl,ildv) = own_sales(ifleet+1,igp,icl,ildv,iregn,yrs) 
			  enddo
			  fltechsal(iregn,1,ifleet,icl,ildv,1) = sum(fltgrpsal(iregn,ifleet,1:cargrp,icl,ildv))
			  fltechsal(iregn,2,ifleet,icl,ildv,1) = sum(fltgrpsal(iregn,ifleet,ltkgrp:maxgroup,icl,ildv))
			enddo 
		  enddo 
		enddo 
	  enddo 
	endif 

!...fleet vehicle sales projection by size class by group 
	if(curcalyr.gt.stockyr)then
 	  do iregn=1,mnumcr-2
		do ifleet=1,maxfleet
		  do igp=1,maxgroup
		    do icl=1,maxclass
			  do ildv=1,maxldv
!		      	fltgrpsal(iregn,ifleet,igp,icl,ildv) = ldv_sales(igp,icl,ildv,iregn,n) * ownsalesshr(ifleet+1,igp,icl,ildv,iregn,n)*1000000.0  !jma
				fltgrpsal(iregn,ifleet,igp,icl,ildv) = ldv_sales(igp,icl,ildv,iregn,n) * ownsaletemp(ifleet+1,igp,icl,iregn,n)*1000000.0
			  enddo
			enddo
          enddo
        enddo
      enddo
!...  fleet vehicle sales projection summed by group
	  do iregn=1,mnumcr-2
		do ifleet=1,maxfleet
		  do icl=1,maxclass
			do ildv=1,maxldv
		      fltechsal(iregn,1,ifleet,icl,ildv,1) = sum(fltgrpsal(iregn,ifleet,1:cargrp,icl,ildv))
		      fltechsal(iregn,2,ifleet,icl,ildv,1) = sum(fltgrpsal(iregn,ifleet,ltkgrp:maxgroup,icl,ildv))			  
			enddo
		  enddo
        enddo
      enddo
	endif
	
!...sum national sales	  
    if(curcalyr.ge.2019) then
	  do ifleet=1,maxfleet
	    do icl=1,maxclass
		  do ildv=1,maxldv
	        do ivtyp=1,maxvtyp
	  	      fltechSal(mnumcr,ivtyp,ifleet,icl,ildv,1) = sum(FltechSal(1:mnumcr-2,ivtyp,ifleet,icl,ildv,1))
			enddo
			do igp=1,maxgroup 
			  fltgrpsal(mnumcr,ifleet,igp,icl,ildv) = sum(fltgrpsal(1:mnumcr-2,ifleet,igp,icl,ildv))
			enddo
		  enddo
		enddo
	  enddo
	endif 

!...project fleet stocks	
	if(curcalyr.gt.stockyr) then
      do ivtyp=1,maxvtyp
		do ifleet=1,maxfleet
		  do ildv=1,maxldv
			do ihav = 1,maxhav
			  do iregn=1,mnumcr-2
				flt_stock(iregn,ivtyp,ifleet,ildv,1,ihav,n) = sum(fltechsal(iregn,ivtyp,ifleet,1:maxclass,ildv,ihav))
			    do iage=2,maxage-1
				  Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,ihav,n)=Flt_Stock(iregn,ivtyp,ifleet,ildv,iage-1,ihav,n-1) * SURVFLT(ifleet,iage-1,ivtyp)
			    enddo ! maxage-1
				Flt_Stock(iregn,ivtyp,ifleet,ildv,maxage,ihav,n) = Flt_Stock(iregn,ivtyp,ifleet,ildv,maxage-1,ihav,n-1) * SURVFLT(ifleet,maxage-1,ivtyp)+ &
																   Flt_Stock(iregn,ivtyp,ifleet,ildv,maxage,ihav,n-1) * SURVFLT(ifleet,maxage,ivtyp)
			  enddo ! mnumcr-2
			  flt_stock(mnumcr,ivtyp,ifleet,ildv,iage,ihav,n) = sum(flt_stock(1:mnumcr-2,ivtyp,ifleet,ildv,iage,ihav,n))
			enddo ! maxhav
		  enddo ! maxldv
		enddo ! maxfleet
	  enddo ! maxtype
	
!...  assign fleet vehicles to household vehicles no HAVs transfer to household vehicles
!...  for years greater than stockyr, use fleet transfer rates based on 2012-2016 data by fleet type 
	  OLDFSTK=0.0
	  do iregn=1,mnumcr-2
	    do ifleet=1,maxfleet
		  do ildv=1,maxldv
		    if(ildv.lt.9.or.ildv.gt.12)then	! Keep natural gas and propane vehicles in the fleet
		      do iage=1,maxage
			    do ivtyp=1,MAXVTYP
				  OLDFSTK(iregn,ivtyp,ifleet,ildv,iage)=Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,1,n) * FLTTRANS(ifleet,iage,ivtyp)
				  Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,1,n)=Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,1,n) - OLDFSTK(iregn,ivtyp,ifleet,ildv,iage)
				enddo
		      enddo
		    endif
		  enddo
		enddo
	  enddo

!...  fill ldv_stock array
	  do iregn=1,mnumcr-2
        do ivtyp=1,maxvtyp
	      do ifleet=1,maxfleet
		    do ildv=1,maxldv
		      do iage=1,maxage
			    do ihav=1,maxhav
			      LDV_Stock(iregn,ivtyp,ifleet+1,ildv,iage,ihav,n) = Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,ihav,n)/1000000.0
				enddo
			  enddo
			enddo
	      enddo
		enddo
	  enddo
	  
!...  sum national 
      do ivtyp=1,maxvtyp
	    do ifleet=1,maxfleet
		  do ildv=1,maxldv
		    do iage=1,maxage
			  do ihav=1,maxhav
	            Flt_Stock(mnumcr,ivtyp,ifleet,ildv,iage,ihav,n) = sum(Flt_Stock(1:mnumcr-2,ivtyp,ifleet,ildv,iage,ihav,n))
			    LDV_Stock(mnumcr,IVTYP,ifleet+1,ildv,iage,ihav,n) = sum(LDV_Stock(1:mnumcr-2,ivtyp,ifleet+1,ildv,iage,ihav,n))
			  enddo
			enddo
		  enddo
		enddo
	  enddo
	endif ! >stockyr

!...Calculate total surviving vehicles, by vehicle, fleet type, and vehicle technology (FLTECHSTK)
    do ivtyp=1,maxvtyp
      do ifleet=1,maxfleet
        do ildv=1,maxldv
		  do ihav = 1,maxhav
		    do iregn=1,mnumcr-2
              FLTECHSTK(iregn,ivtyp,ifleet,ILDV,ihav) = sum(Flt_Stock(iregn,ivtyp,ifleet,ildv,1:maxage,ihav,n))			
			enddo
            FLTECHSTK(mnumcr,ivtyp,ifleet,ILDV,ihav) = sum(Flt_Stock(mnumcr,ivtyp,ifleet,ildv,1:maxage,ihav,n))
		  enddo
        enddo
      enddo
    enddo

    do ivtyp=1,maxvtyp
      TOTFLTCAR(ivtyp) = SUM(FLTECHSTK(mnumcr,ivtyp,1:maxfleet,1:maxldv,1:maxhav))/1000000.0
    enddo

!...EPACT legislative alternative vehicle sales, for table 48
    epactot = sum(fltechsal(mnumcr,1:2,2:3,1:6,3:16,1:4))/1000000.0

  RETURN
  END SUBROUTINE TFLTSTKS

! ==========================================================================================================
! ... Subroutine TLEGIS adjusts vehicle sales and market shares to reflect legislative mandates on sales of 
! ... ZEVs and ULEVs 
! ==========================================================================================================
    SUBROUTINE TLEGIS  ! currently turned off
    USE T_
    IMPLICIT NONE
	
!...Local variable dictionary
!...real variables
	REAL :: VSALES(maxvtyp,MAXCLASS,MNUMCR-2,MAXLDV)        !...starting vehicle sales
	REAL :: NORM_VSALES(MAXVTYP,MAXCLASS,MNUMCR-2,MAXLDV)   !...regionaly adjusted/normalized vehicle sales
	REAL :: ADJ_VSALES(MAXVTYP,MAXCLASS,MNUMCR-2,MAXLDV)    !...sales adjustments made
	REAL :: ORIG_SHARE(MNUMCR-2,MAXVTYP,MAXLDV)             !...initial regional sales from history
	REAL :: NORM_SHARE(MNUMCR,MAXVTYP,MAXLDV)               !...CVCM sales shares calibrated to history
    REAL :: hold_zev_credit_ldv(MNUMCR-2)                   !   holding of credits for comuptation purposes !nce by msi
    REAL :: traveling_CA_credits(MNUMCR-2,MAXLDV,MNUMYR)	!...temporary traveling California credits available
    REAL :: traveling_s177_credits(MAXLDV)                  !   credits traveling from S177 to California
	REAL :: CA_credits(MAXLDV,MNUMYR)						!...temporary California ZEV credits earned
	REAL :: annual_credit_balance(MNUMCR-2,MAXZEV,MNUMYR)	!...annual ZEV credit balance (earn-requirement)
	REAL :: credit_bank_switch(MNUMCR-2)					!...2018 credit transition from ATPZEV and PZEV to TZEV
	REAL :: credit_shortfall(MNUMCR-2,MAXZEV,MNUMYR)		!...annual ZEV mandate credit shortfall (requirement-earn)
	REAL :: bank_credit_available(MNUMCR-2,MAXZEV,MNUMYR)	!...annual ZEV bank credits available for use
	REAL :: postbank_shortfall(MNUMCR-2,MAXZEV,MNUMYR)		!...ZEV mandate shortfall after bank credit use
	REAL :: bank_draw(MNUMCR-2,MAXZEV,MNUMYR)				!...annual ZEV bank credit drawdown
	REAL :: adj_zev_credit_earn(MNUMCR-2,MAXZEV,MNUMYR)		!...annual ZEV credit adjustment after sales increase
	REAL :: credit_shortfall_buff(MNUMCR-2,MAXZEV,MNUMYR)	!...annual ZEV mandate credit shortfall(requirement*bankbuffer-earn)
	REAL :: a_bank(MNUMCR-2,MAXZEV)							!...quadratic factor for bank spending rate 
	REAL :: b_bank(MNUMCR-2,MAXZEV)							!...quadratic factor for bank spending rate 
	REAL :: c_bank(MNUMCR-2,MAXZEV)							!...quadratic factor for bank spending rate 	
	REAL :: sales_adj_ratio(MAXVTYP,MAXCLASS,MNUMCR-2,MAXLDV)	!...sales adjustment ratio for ZEV sales push
	REAL :: Sales_Adjustment								! adjustment factor used to increase sales by zev by CD
	
!...integer variables
	INTEGER it,xy,iiregn
	INTEGER zev_lasthistyr									!...year ZEV_basebank was calculated for
	INTEGER drawdown_yr										!...goal year that ZEV credit bank use ends 
	INTEGER draw_first_yr									!...year that ZEV credit bank starts 


	zev_lasthistyr = 2021			!ZEV align with polk stock year in subroutine call

!...Calculate regional vehicle sales, by technology, within 8 size classes
!...Start by applying the national powertrain distribution to each region's sales
!...This assumes that ALL regions have the same distribution of sales across powertrain (ildv) [CORRECTED below]

!	if(curcalyr.eq.2023.or.curcalyr.eq.2024) then
!	  write(21,*)'tlegis 1'
!	  write(21,*)'year          =',curcalyr
!	  write(21,*)'propane sales =',ttltechsal(5,1,5,12,n)
!	  write(21,*)'lpg share(44) =',apshr44(1,5,5,12,n)
!	  do igp=1,cargrp
!	    write(21,*)'  APShrGrp1 =',apshrgrp(1,5,5,12,n),'GrpShare=',GrpShare(5,1,n)
!		write(21,*)'  APShrGrp2 =',apshrgrp(2,5,5,12,n),'GrpShare=',GrpShare(5,2,n)
!		write(21,*)'  APShrGrp3 =',apshrgrp(3,5,5,12,n),'GrpShare=',GrpShare(5,3,n)
!		write(21,*)'  APShrGrp4 =',apshrgrp(4,5,5,12,n),'GrpShare=',GrpShare(5,4,n)
!		write(21,*)'  APShrGrp5 =',apshrgrp(5,5,5,12,n),'GrpShare=',GrpShare(5,5,n)
! 	  enddo
!	endif
	norm_share = 0.
    DO IREGN=1,MNUMCR-2
	  do ivtyp=1,maxvtyp
        DO ILDV=1,MAXLDV
          DO ICL=1,MAXCLASS
			vsales(1,icl,iregn,ildv) = sum(ldv_sales(1:cargrp,icl,ildv,iregn,n))
            vsales(2,icl,iregn,ildv) = sum(ldv_sales(LTKGRP:maxgroup,icl,ildv,iregn,n))
		  ENDDO
        ENDDO
	  enddo

!...  Adjust powertrain distribution within each region using regional sales data for HEV, PHEV, EV and FCV
!...  Apply the last historical sales data, along with a correction to account for increasing ZEV requirements (reg_share_adj), for the projection
!...  calculate historic regional share
	  if(curcalyr.ge.stockyr) then
	    do ivtyp=1,maxvtyp
	      do ildv=1,maxldv
!...        calculate initial sales shares across region (vintages 1 and 2)
		    if(curcalyr.eq.stockyr) then
		      if(sum(LDV_Stock(mnumcr,ivtyp,1,ildv,1:2,1,n)).gt.0.) &
			    orig_share(iregn,ivtyp,ildv) = sum(LDV_Stock(iregn,ivtyp,1,ildv,1:2,1,n))/sum(LDV_Stock(mnumcr,ivtyp,1,ildv,1:2,1,n))   
		    endif
!...        account for increases in ZEV requirements for EVs
		    if(curcalyr.gt.stockyr) then
              if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then
                norm_share(iregn,ivtyp,ildv) = orig_share(iregn,ivtyp,ildv) + reg_share_adj(iregn,n)
		      endif ! ildv
            endif ! > stockyr
	      enddo ! maxldv
	    enddo ! maxvtyp
      endif 
    enddo ! mnumcr
	
    if(curcalyr.ge.stockyr) then
	  do ivtyp=1,maxvtyp
	    do ildv=1,maxldv
	      norm_share(mnumcr,ivtyp,ildv) = sum(norm_share(1:mnumcr-2,ivtyp,ildv))
	    enddo
	  enddo
	endif
!...  apply new regional powertrain sales shares from above to national CVCM output for HEV, PHEV and EV	  
    if(curcalyr.gt.stockyr) then
	  do iregn=1,mnumcr-2
	    do ivtyp=1,maxvtyp
	      do ildv=1,maxldv
!...	    normalize shares to 1
	        if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then
		      norm_share(iregn,ivtyp,ildv) = norm_share(iregn,ivtyp,ildv)/norm_share(mnumcr,ivtyp,ildv)
		    endif
            do icl=1,maxclass
!...          for EV100, EV200 and EV300			
	          if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then 	
!...            new vehicle sales across region
		        norm_vsales(ivtyp,icl,iregn,ildv) = sum(vsales(ivtyp,icl,1:mnumcr-2,ildv)) * norm_share(iregn,ivtyp,ildv)
                adj_vsales(ivtyp,icl,iregn,ildv) = norm_vsales(ivtyp,icl,iregn,ildv) - vsales(ivtyp,icl,iregn,ildv)
!...		    adjust sales to account for revisions  
				vsales(ivtyp,icl,iregn,ildv) = norm_vsales(ivtyp,icl,iregn,ildv)
				vsales(ivtyp,icl,iregn,1) = vsales(ivtyp,icl,iregn,1) - adj_vsales(ivtyp,icl,iregn,ildv)
			  endif
!...          for HEV, PHEV and FCV 
!...          Note: FCV goes primarily to CD 9, for high FCV sales cases this might need to be altered
			  if(ildv.eq.5.or.ildv.eq.6.or.ildv.eq.14.or.ildv.eq.16) then
!...            new vehicle sales across region			  
			    norm_vsales(ivtyp,icl,iregn,ildv) = sum(vsales(ivtyp,icl,1:mnumcr-2,ildv)) * orig_share(iregn,ivtyp,ildv)
! jma check this once 2021 polk data is in.  No lt PHEV50 in history.
			    if(ivtyp.eq.2.and.ildv.eq.6) then
				  norm_vsales(ivtyp,icl,iregn,ildv) = sum(vsales(ivtyp,icl,1:mnumcr-2,ildv)) * orig_share(iregn,ivtyp,5)
				endif
				adj_vsales(ivtyp,icl,iregn,ildv) = norm_vsales(ivtyp,icl,iregn,ildv) - vsales(ivtyp,icl,iregn,ildv)
!...			adjust ildv sales to account for revisions  
				vsales(ivtyp,icl,iregn,ildv) = norm_vsales(ivtyp,icl,iregn,ildv)
				vsales(ivtyp,icl,iregn,1) = vsales(ivtyp,icl,iregn,1) - adj_vsales(ivtyp,icl,iregn,ildv)					  
 			  endif ! conditional ldv 
			enddo ! maxclass
		  enddo ! maxldv
		enddo ! maxvtyp
	  enddo ! mnumcr-2
	endif ! stockyr

!	JMA - Debug sales regionalization	
!	if(curcalyr.gt.stockyr)then
!	  do ivtyp=1,maxvtyp
!	    do ildv=1,maxldv
!	      write(21,*)'check total norm'
!	       write(21,*)'reg =',iregn,'ivtyp =', ivtyp,'ildv =', ildv, 'norm share =',sum(norm_share(1:mnumcr-2,ivtyp,ildv))
!	    enddo
!		do ildv=1,maxldv
!	      write(21,*)'check total orig'
!	      write(21,*)'reg =',iregn,'ivtyp =', ivtyp,'ildv =', ildv, 'orig share =',sum(orig_share(1:mnumcr-2,ivtyp,ildv))		  
!	    enddo
!	  enddo
!	endif
	
!...populate adjusted vehicle sales	
    do iregn=1,mnumcr-2  
	  do IVTYP=1,maxvtyp
	    do ICL=1,MAXCLASS
          do ILDV=1,maxldv
            avsales(IVTYP,ICL,iregn,ILDV) = vsales(IVTYP,ICL,iregn,ILDV)
          enddo ! maxldv
        enddo ! maxclass
	  enddo ! maxvtyp
	enddo ! mnumcr-2

!...Calculate mandated sales of ZEVs by participating states: 
!	CD1 = Connecticut, Massachusetts, Maine, Rhode Island, Vermont
!	CD2 = New York, New Jersey
!	CD4 = Minnesota (MY2025+)
!	CD5 = Maryland, Virginia (MY2025+)
!	CD8 = Colorado (MY2023+), Nevada (MY2025+), New Mexico (MY2026+)
!	CD9 = California, Oregon, Washington (2025+)

!...Note: California LEVP Mandates are not separate by car and l.t., so car and l.t. are   
!...combined to meet sales constraint

!...Determine vehicle sales for calculating ZEV requirements
!	From CA code 13 CCR 1962.2(b)(1)(B) "For 2018 and subsequent model years, a manufacturer's production volume for the given model year will be based on the three-year 
!	average of the manufacturer's volume of PCs and LDTs, produced and delivered for sale in California in the prior second, third, and fourth model year [for example, 
!	2019 model year ZEV requirements will be based on California production volume average of PCs and LDTs for the 2015 to 2017 model years]"
    do iregn=1,mnumcr-2
      do ILDV=1,maxldv
        vsales_t(iregn,ILDV,n) = sum(vsales(1:2,1:MAXCLASS,iregn,ILDV))
      enddo
      ZEV_covered_sales(iregn) = (sum(vsales_t(iregn,1:16,n-4:n-2)) / 3.) * zev_state_alloc(iregn,n)
    enddo	

!...Calculate "traveling provision" transfer rate from sales in California towards compliance in other S.177 states
!	Manufacturers can count CA sales -- and the corresponding credits generated -- in both CA AND in other ZEVR states
!	pre-2018: CCR 1962.1(d)(5)(E) -- all ZEV credits eligible
!	2018+:    CCR 1962.2(d)(5)(E) -- ONLY FCVs eligible
    do iregn=1,mnumcr-2
	  if(ZEV_covered_sales(iregn).ne.0.0)then
	    if(iregn.le.8)then
	      credit_transfer_rate(iregn) = ZEV_covered_sales(iregn) / (ZEV_covered_sales(9)*(zev_state_alloc(9,n)/CA_shr_of9))
	    else !...calculate non-CA CD9 (OR, WA) reception of California travelling credits
         credit_transfer_rate(iregn) = (ZEV_covered_sales(iregn)*(zev_state_alloc(9,n)-CA_shr_of9)) / (ZEV_covered_sales(9)*CA_shr_of9)   		
        endif
      endif
	enddo

!...Determine ZEV credits earned by Census Division by mandate category (izev index)
!	including both actual sales (zev_credit_ldv) and traveling credits (traveling_CA_credits and traveling_S177_credits)
!	1. Calculate credits earned in each region, based on sales in that region
    zev_credit_ldv(:,:,n)=0.0
    do iregn=1,mnumcr-2
      do ILDV=1,maxldv
        zev_credit_ldv(iregn,ILDV,n)=vsales_t(iregn,ILDV,n)*zev_state_alloc(iregn,n)*zev_multiplier(ILDV,n)
      enddo
    enddo
!	2. Calculate ZEV credits earned and traveling both East S177-to-California and California-to-S177 credits
    do iregn=1,mnumcr-2
      do ILDV=1,maxldv
!	    Estimate number of CA-earned credits that travel to other Section 177 states (traveling_CA_credits)
!		and number of non-CA-earned credits that travel to CA (traveling_S177_credits)
        if(curcalyr.ge.zev_lasthistyr.and.zev_state_alloc(iregn,n).ne.0.0)then
          if(ILDV.eq.14)then		! Only FCV credits can travel in 2018+
            if(iregn.eq.1)then
              hold_zev_credit_ldv(:)=zev_credit_ldv(:,ILDV,n)
              traveling_S177_credits(ILDV)=dot_product(s177_traveling_factor,hold_zev_credit_ldv) ! East S177 credits travel to CA
              zev_credit_ldv(1:mnumcr-2,ILDV,n)=(1.-s177_traveling_factor(1:mnumcr-2))*zev_credit_ldv(1:mnumcr-2,ILDV,n) ! eliminate the share of originating credits from East S177 because travel to CA
            endif
            CA_credits(ILDV,n)=zev_credit_ldv(9,ILDV,n)*(CA_shr_of9/zev_state_alloc(9,n))+traveling_s177_credits(ILDV)
          endif
        endif ! >= zev_lasthistyr
        if(credit_transfer_rate(iregn).ne.0.0)then
          traveling_CA_credits(iregn,ILDV,n) = CA_credits(ILDV,n) * credit_transfer_rate(iregn)
        endif
!		Combine ZEV credits earned and traveling East S177 and California credits
!		zev_credit_earn(mnumcr,maxzev,mnumyr), where maxzev = {1:ATPZEV, 2: TZEV, 3: ZEV}
        if(iregn.le.8)then
          zev_credit_earn(iregn,1,n)=zev_credit_ldv(iregn,8,n) + traveling_CA_credits(iregn,8,n) +& !HEV diesel
                                     zev_credit_ldv(iregn,11,n)+ traveling_CA_credits(iregn,11,n)+& !CNG dedicated
                                     zev_credit_ldv(iregn,16,n)+ traveling_CA_credits(iregn,16,n)   !HEV gasoline
          zev_credit_earn(iregn,2,n)=zev_credit_ldv(iregn,5,n) + traveling_CA_credits(iregn,5,n) +& !PHEV20
                                     zev_credit_ldv(iregn,6,n) + traveling_CA_credits(iregn,6,n)    !PHEV50
          zev_credit_earn(iregn,3,n)=zev_credit_ldv(iregn,4,n) + traveling_CA_credits(iregn,4,n) +& !EV100
                                     zev_credit_ldv(iregn,7,n) + traveling_CA_credits(iregn,7,n) +& !EV200
                                     zev_credit_ldv(iregn,13,n)+ traveling_CA_credits(iregn,13,n)+& !FCV methanol
                                     zev_credit_ldv(iregn,14,n)+ traveling_CA_credits(iregn,14,n)+& !FCV hydrogen
                                     zev_credit_ldv(iregn,15,n)+ traveling_CA_credits(iregn,15,n)   !EV300
        else
          zev_credit_earn(iregn,1,n)=zev_credit_ldv(iregn,8,n) + traveling_CA_credits(iregn,8,n) +& !HEV diesel
                                     zev_credit_ldv(iregn,11,n)+ traveling_CA_credits(iregn,11,n)+& !CNG dedicated
                                     zev_credit_ldv(iregn,16,n)+ traveling_CA_credits(iregn,16,n)+& !HEV gasoline
                                     traveling_s177_credits(8) +                                  & !S177 traveled HEV diesel
                                     traveling_s177_credits(11)+                                  & !S177 traveled CNG dedicated
                                     traveling_s177_credits(16)                                     !S177 traveled HEV gasoline
          zev_credit_earn(iregn,2,n)=zev_credit_ldv(iregn,5,n) + traveling_CA_credits(iregn,5,n) +& !PHEV20
                                     zev_credit_ldv(iregn,6,n) + traveling_CA_credits(iregn,6,n) +& !PHEV50
                                     traveling_s177_credits(5) +                                  & !S177 traveled PHEV20
                                     traveling_s177_credits(6)                                      !S177 traveled PHEV50
          zev_credit_earn(iregn,3,n)=zev_credit_ldv(iregn,4,n) + traveling_CA_credits(iregn,4,n) +& !EV100
                                     zev_credit_ldv(iregn,7,n) + traveling_CA_credits(iregn,7,n) +& !EV200
                                     zev_credit_ldv(iregn,13,n)+ traveling_CA_credits(iregn,13,n)+& !FCV methanol
                                     zev_credit_ldv(iregn,14,n)+ traveling_CA_credits(iregn,14,n)+& !FCV hydrogen
                                     zev_credit_ldv(iregn,15,n)+ traveling_CA_credits(iregn,15,n)+& !EV300
                                     traveling_s177_credits(4) +                                  & !S177 traveled EV100
                                     traveling_s177_credits(7) +                                  & !S177 traveled EV200
                                     SUM(traveling_s177_credits(13:15))								!S177 traveled FCV methanol,FCV hydrogen,EV300

        endif
      enddo
    enddo

!...determine ZEV credit requirements and allowances by Census Division and mandate category (izev)
!	includes California AB32 regulation
	do iregn=1,mnumcr-2
	  if(curcalyr.gt.zev_lasthistyr.and.zev_state_alloc(iregn,n).gt.0.0)then
        do izev=1,maxzev 
	      if(iregn.le.8)then
		    zev_credit_req(iregn,izev,n) = ZEV_covered_sales(iregn) * ZEV_Requirement_optional(izev,n)
		  elseif(iregn.eq.9)then
		    zev_credit_req(iregn,izev,n) = ZEV_covered_sales(iregn) * ZEV_Requirement(izev,n)
			if(TRANAB32.ne.0.and.izev.eq.2)then
			  zev_credit_req(iregn,izev,n) = ZEV_covered_sales(iregn) * tzev_req_ca_co2(n)
			elseif(TRANAB32.ne.0.and.izev.eq.3)then
			  zev_credit_req(iregn,izev,n) = ZEV_covered_sales(iregn) * zev_req_ca_co2(n)
			endif
		  endif
		enddo
	  endif ! > zev_lasthistyr
	enddo

	if(curcalyr.eq.zev_lasthistyr) then
	  zev_credit_bank(:,:,:) = 0.0	! initialize
	  credit_shortfall(:,:,:) = 0.0
	  DO iregn=1,mnumcr-2
	    zev_credit_bank(iregn,1,n) = SUM(zev_basebank(iregn,3:4))	! ATPZEV, PZEV, NEV+		
	    zev_credit_bank(iregn,2,n) = zev_basebank(iregn,2)			! TZEV
	    zev_credit_bank(iregn,3,n) = zev_basebank(iregn,1)			! ZEV (FCV + BEV + BEV+)
	  ENDDO
!	  Certain states' regs indicate that manufacturers will receive a credit deposit in the first MY
!	  of the regulation (proportional to CA credits/sales). There is often a "max percent of ZEV requirement allowed to be met with these
!	  credits" stipulation attached, but we don't model sales by state. So the initial credit deposits 
!	  are completed below, but the use of those credits isn't limited later on.
!     Colorado (5 CCR 1001-24)
	ELSEIF(curcalyr.eq.2023) THEN
	  DO izev=1,maxzev
	    zev_credit_bank(8,izev,n) = (CO_shr_of8*(sum(vsales_t(8,1:16,n-4:n-2)) / 3.)) / &
								    (CA_shr_of9*(sum(vsales_t(9,1:16,n-4:n-2)) / 3.)) * &
									 zev_credit_bank(9,izev,n) + zev_credit_bank(8,izev,n)
	  ENDDO
!     Virginia (10.1-1307.04) and Minnesota (OAH 71-9003-36416)
	ELSEIF(curcalyr.eq.2025) THEN
	  DO izev=1,maxzev
	    zev_credit_bank(5,izev,n) = (VA_shr_of5*(sum(vsales_t(5,1:16,n-4:n-2)) / 3.)) / &
									(CA_shr_of9*(sum(vsales_t(9,1:16,n-4:n-2)) / 3.)) * &
									 zev_credit_bank(9,izev,n) + zev_credit_bank(5,izev,n)
		zev_credit_bank(4,izev,n) = (MN_shr_of4*(sum(vsales_t(4,1:16,n-4:n-2)) / 3.)) / &
									(CA_shr_of9*(sum(vsales_t(9,1:16,n-4:n-2)) / 3.)) * &
									 zev_credit_bank(9,izev,n) + zev_credit_bank(4,izev,n)
	  ENDDO
	ENDIF ! = yr condition

!...verify Census Divisions are in compliance with ZEV requirement in 2018+ using credits earned and banking  
!...calculate ZEV compliance 1st, highest order constraint
	do iregn=1,mnumcr-2 !region
	  if(curcalyr.gt.zev_lasthistyr.and.zev_state_alloc(iregn,n).gt.0.0)then !year
		if(zev_credit_earn(iregn,3,n).ge.(zev_credit_req(iregn,3,n)*(1.0+Bank_buffer(n))))then  !met all reqs?
		  annual_credit_balance(iregn,3,n) = zev_credit_earn(iregn,3,n) - zev_credit_req(iregn,3,n)
		  zev_credit_bank(iregn,3,n) = zev_credit_bank(iregn,3,n-1) + annual_credit_balance(iregn,3,n)	
		else
		  credit_shortfall(iregn,3,n)      =  zev_credit_req(iregn,3,n) - zev_credit_earn(iregn,3,n) ! req only
		  credit_shortfall_buff(iregn,3,n) = (zev_credit_req(iregn,3,n)*(1.0+Bank_buffer(n))) - zev_credit_earn(iregn,3,n) ! req + buffer
!...      determine bank spending rate: quadratic spending curve based on bank entering 2018, and targeted end year
!...	  first year of credit shortfall
		  if(credit_shortfall(iregn,3,n-1).le.0.001.and.credit_shortfall(iregn,3,n).gt.0.001)then
			draw_first_yr = n
			drawdown_yr = 41 - draw_first_yr		!draw down year 2030 is indexed from first year with shortfall
			c_bank(iregn,3) = 0.3 * credit_shortfall(iregn,3,n)		!set intercept 30% of first year shortfall
			b_bank(iregn,3) = 6 * (zev_credit_bank(iregn,3,n-1)-((2/3)*drawdown_yr*c_bank(iregn,3))) / (drawdown_yr**2)
			a_bank(iregn,3) = -((drawdown_yr*b_bank(iregn,3)) + c_bank(iregn,3)) / (drawdown_yr**2)
		  endif
!...	  spending rate curve: calculated spending quadratic divided by shortfall
		  bank_spending_rate(iregn,n) = (a_bank(iregn,3)*((n - draw_first_yr)**2) + &
										 b_bank(iregn,3)*(n-draw_first_yr) + c_bank(iregn,3))/credit_shortfall(iregn,3,n) 
		  bank_spending_rate(iregn,n) = MAX(0.,(MIN(1.,bank_spending_rate(iregn,n))))
		  if(credit_shortfall(iregn,3,n).lt.zev_credit_bank(iregn,3,n-1))then  ! req shortfall < bank
			bank_draw(iregn,3,n) = credit_shortfall(iregn,3,n) * bank_spending_rate(iregn,n) ! bank draw 
		  else
			bank_draw(iregn,3,n) = zev_credit_bank(iregn,3,n-1) * bank_spending_rate(iregn,n)  ! req shortfall > bank
		  endif  
		  credit_shortfall(iregn,3,n) = credit_shortfall(iregn,3,n)-bank_draw(iregn,3,n)
		  if(bank_draw(iregn,3,n).lt.zev_credit_bank(iregn,3,n-1))then ! bank draw < credit bank
			zev_credit_bank(iregn,3,n) = zev_credit_bank(iregn,3,n-1) - bank_draw(iregn,3,n) ! adjust bank
		  else
			bank_draw(iregn,3,n) = zev_credit_bank(iregn,3,n-1)		
			zev_credit_bank(iregn,3,n) = 0.0
		  endif
		  if(zev_credit_bank(iregn,3,n).ge.0.0)then
			if(credit_shortfall(iregn,3,n).gt.0.0)then ! did not meet req
			  if((zev_credit_bank(iregn,3,n)/2.0).gt.credit_shortfall(iregn,3,n) + (zev_credit_req(iregn,3,n)*bank_buffer(n)))then
				postbank_shortfall(iregn,3,n) = credit_shortfall(iregn,3,n)  ! huge bank bal just meet req need 
			  else  ! low bank replenish with buffer
				postbank_shortfall(iregn,3,n) = credit_shortfall(iregn,3,n) + (zev_credit_req(iregn,3,n)*Bank_buffer(n)) 
			  endif
			else ! exceeded req, but not buffer
			  if((zev_credit_bank(iregn,3,n)/2.0).gt.(zev_credit_req(iregn,3,n)*bank_buffer(n)))then
				postbank_shortfall(iregn,3,n) = 0.0
			  else
				postbank_shortfall(iregn,3,n) = credit_shortfall_buff(iregn,3,n) 
			  endif
			endif
		  endif
		endif !met all regs?
		if(postbank_shortfall(iregn,3,n).gt.0.0)then	!shortfall	
		  sales_delta = 0.0
		  sales_adjustment = (zev_credit_req(iregn,3,n) *(1.0+Bank_buffer(n))) / (zev_credit_earn(iregn,3,n) + bank_draw(iregn,3,n))  
		  do IVTYP=1,maxvtyp
			do ICL=1,MAXCLASS
			  do ILDV=1,maxldv
				if(ILDV.eq.4.or.ILDV.eq.7.or.ILDV.eq.14.or.ILDV.eq.15)then
				  sales_adj_ratio(IVTYP,ICL,iregn,ILDV) = ((sales_adjustment-1.0)*zev_state_alloc(iregn,n))+1.0
				  avsales(IVTYP,ICL,iregn,ILDV) = vsales(IVTYP,ICL,iregn,ILDV) * sales_adj_ratio(IVTYP,ICL,iregn,ILDV)
				  sales_delta(IVTYP,ICL,iregn,ILDV) = avsales(IVTYP,ICL,iregn,ILDV) - vsales(IVTYP,ICL,iregn,ILDV)
				  avsales(IVTYP,ICL,iregn,1) = avsales(IVTYP,ICL,iregn,1) - sales_delta(IVTYP,ICL,iregn,ILDV)
				endif
			  enddo
			enddo
		  enddo 
		  adjusted_zev_credit = 0.0   
		  do ILDV=1,maxldv
			if(ILDV.eq.4.or.ILDV.eq.7.or.ILDV.eq.14.or.ILDV.eq.15)then		   
			  adjusted_zev_credit(iregn,ILDV)=sum(sales_delta(1:maxvtyp,1:MAXCLASS,iregn,ILDV)) * zev_multiplier(ILDV,n)	
			endif
		  enddo
		  adj_zev_credit_earn(iregn,3,n)=sum(adjusted_zev_credit(iregn,1:maxldv))
		  if(adj_zev_credit_earn(iregn,3,n).gt.postbank_shortfall(iregn,3,n))then
			zev_credit_bank(iregn,3,n) = zev_credit_bank(iregn,3,n) + adj_zev_credit_earn(iregn,3,n) - postbank_shortfall(iregn,3,n)
		  endif  
		endif !shortfall
	  endif ! > zev_lasthistyr
	enddo !region	

!...Calculate TZEV compliance allowance for 2018+ using credits earned and banking
	do iregn=1,mnumcr-2 !region
	  if(curcalyr.gt.zev_lasthistyr.and.zev_state_alloc(iregn,n).gt.0.0)then !year
	    zev_credit_bank(iregn,1,n) = zev_credit_bank(iregn,1,n-1)		! carry over previous year's ATPZEV/PZEV/NEV+ credits
!		Can cover 25% of the TZEV req with previously earned ATPZEV/PZEV/NEV+ credits
!		Assume OEMs want to to spend down these older credits ASAP (expire 2025), so they use them before using current-year-TZEV credits
!		13 CCR 1962(g)(6)(A)
		if(zev_credit_bank(iregn,1,n).gt.0.001.and.curcalyr.le.2025) then
		  bank_draw(iregn,1,n) = MIN(zev_credit_req(iregn,2,n)*0.25,zev_credit_bank(iregn,1,n))
		  zev_credit_bank(iregn,1,n)= zev_credit_bank(iregn,1,n)-bank_draw(iregn,1,n)
		  zev_credit_req(iregn,2,n) = zev_credit_req(iregn,2,n)-bank_draw(iregn,1,n)
		endif
	    if(zev_credit_earn(iregn,2,n).ge.zev_credit_req(iregn,2,n))then  !met all reqs?
	      annual_credit_balance(iregn,2,n) = zev_credit_earn(iregn,2,n) - zev_credit_req(iregn,2,n)
	      zev_credit_bank(iregn,2,n) = zev_credit_bank(iregn,2,n-1) + annual_credit_balance(iregn,2,n)
	    else
	      credit_shortfall(iregn,2,n)      =  zev_credit_req(iregn,2,n) - zev_credit_earn(iregn,2,n) ! req only
!		  determine bank spending rate: quadratic spending curve based on bank entering 2018, and targeted end year
!		  first year of credit shortfall
		  if(credit_shortfall(iregn,2,n-1).le.0.001.and.credit_shortfall(iregn,2,n).gt.0.001)then
		    draw_first_yr = n
		    drawdown_yr = 41 - draw_first_yr		!draw down year 2030 "n=41" is indexed from first year with shortfall
		    c_bank(iregn,2) = 0.3 * credit_shortfall(iregn,2,n)		!set intercept 30% of first year shortfall
		    b_bank(iregn,2) = 6 * (zev_credit_bank(iregn,2,n-1)-((2/3)*drawdown_yr*c_bank(iregn,2))) / (drawdown_yr**2)
		    a_bank(iregn,2) = -((drawdown_yr*b_bank(iregn,2)) + c_bank(iregn,2)) / (drawdown_yr**2)
		  endif
!...      spending rate curve: calculated spending quadratic divided by shortfall
		  bank_spending_rate(iregn,n) = (a_bank(iregn,2)*((n - draw_first_yr)**2) + &
		  	 						     b_bank(iregn,2)*(n-draw_first_yr) + c_bank(iregn,2))/credit_shortfall(iregn,2,n) 
		  bank_spending_rate(iregn,n) = MAX(0.,(MIN(1.,bank_spending_rate(iregn,n))))
		  if(credit_shortfall(iregn,2,n).lt.zev_credit_bank(iregn,2,n-1))then  ! req shortfall < bank
		    bank_draw(iregn,2,n) = credit_shortfall(iregn,2,n) * bank_spending_rate(iregn,n) ! bank draw 
		  else
		    bank_draw(iregn,2,n) = zev_credit_bank(iregn,2,n-1) * bank_spending_rate(iregn,n)  ! req shortfall > bank
		  endif  
		  credit_shortfall(iregn,2,n) = credit_shortfall(iregn,2,n)-bank_draw(iregn,2,n)
		  if(bank_draw(iregn,2,n).lt.zev_credit_bank(iregn,2,n-1))then ! bank draw < credit bank
		    zev_credit_bank(iregn,2,n) = zev_credit_bank(iregn,2,n-1) - bank_draw(iregn,2,n) ! adjust bank
		  else
		    bank_draw(iregn,2,n) = zev_credit_bank(iregn,2,n-1)		
		    zev_credit_bank(iregn,2,n) = 0.0
		  endif
		  if(zev_credit_bank(iregn,2,n).ge.0.0)then
		    if(credit_shortfall(iregn,2,n).gt.0.0)then ! did not meet req
		  	  postbank_shortfall(iregn,2,n) = credit_shortfall(iregn,2,n) 
		  	endif
		  endif
		endif !met all regs?
		if(postbank_shortfall(iregn,2,n).gt.0.0)then	!shortfall	
		  sales_delta = 0.0
		  sales_adjustment = zev_credit_req(iregn,2,n) / (zev_credit_earn(iregn,2,n) + bank_draw(iregn,2,n))
		  do IVTYP=1,maxvtyp
		    do ICL=1,MAXCLASS
		      do ILDV=1,maxldv
		    	if(ILDV.eq.5.or.ILDV.eq.6)then
		    	  sales_adj_ratio(IVTYP,ICL,iregn,ILDV) = ((sales_adjustment-1.0)*zev_state_alloc(iregn,n))+1.0
		    	  avsales(IVTYP,ICL,iregn,ILDV) = vsales(IVTYP,ICL,iregn,ILDV) * sales_adj_ratio(IVTYP,ICL,iregn,ILDV)
		    	  sales_delta(IVTYP,ICL,iregn,ILDV) = avsales(IVTYP,ICL,iregn,ILDV) - vsales(IVTYP,ICL,iregn,ILDV)
		    	  avsales(IVTYP,ICL,iregn,1) = avsales(IVTYP,ICL,iregn,1) - sales_delta(IVTYP,ICL,iregn,ILDV)
		    	endif
		      enddo
		    enddo
		  enddo 
		  adjusted_zev_credit = 0.0   
		  do ILDV=1,maxldv
		    if(ILDV.eq.5.or.ILDV.eq.6)then		   
		  	  adjusted_zev_credit(iregn,ILDV)=sum(sales_delta(1:maxvtyp,1:MAXCLASS,iregn,ILDV)) * zev_multiplier(ILDV,n)	
		  	endif
		  enddo
		  adj_zev_credit_earn(iregn,2,n)=sum(adjusted_zev_credit(iregn,1:maxldv))
		  if(adj_zev_credit_earn(iregn,2,n).gt.postbank_shortfall(iregn,2,n))then
		    zev_credit_bank(iregn,2,n) = zev_credit_bank(iregn,2,n) + adj_zev_credit_earn(iregn,2,n) - postbank_shortfall(iregn,2,n)
		  endif  
		endif ! shortfall
	  endif ! > zev_lasthistyr 
	enddo	

!	ZEV Mandate debug writes
!	IF (N.eq.mnumyr.and.PassNo.eq.2) THEN
!	  WRITE(21,*)'ZEV_mandate_check'
!	  WRITE(21,*)'year,region,zev_type,earned,required,banked,withdrawn,shortfall'
!	  DO icl=25,mnumyr		! commandeer class param for temp year index
!	    DO iregn=1,mnumcr-2
!		  DO izev=1,maxzev
!	  	  WRITE(21,'(i5,",",i3,",",i3,5(", ",f14.5))') icl+1989,iregn,izev,zev_credit_earn(iregn,izev,icl)*1000000,zev_credit_req(iregn,izev,icl)*1000000,&
!													   zev_credit_bank(iregn,izev,icl-1)*1000000,bank_draw(iregn,izev,icl)*1000000, postbank_shortfall(iregn,izev,icl)*1000000
!	      ENDDO
!		ENDDO
!	  ENDDO
!	ENDIF

!...Total adjusted vehicle sales
    DO IVTYP=1,MAXVTYP
      DO ICL=1,MAXCLASS
        DO ILDV=1,MAXLDV
	      avsales(ivtyp,icl,mnumcr,ildv) = 0.0
		  AVSALES(IVTYP,ICL,MNUMCR,ILDV) = sum(AVSALES(IVTYP,ICL,1:MNUMCR-2,ILDV))
        ENDDO
      ENDDO
    ENDDO
    avsalest = 0.
    DO IVTYP=1,MAXVTYP
      DO IREGN=1,MNUMCR
        DO ICL=1,MAXCLASS
          AVSALEST(IVTYP,ICL,IREGN) = sum(AVSALES(IVTYP,ICL,IREGN,1:maxldv))
        ENDDO
      ENDDO
    ENDDO

!...calculate new absolute market shares for each vehicle technology            
!...Note: APSHR55 is necessary to divide total vehicle sales back into car and  
!...l.t. separately after the total vehicle sales have been adjusted to the     
!...mandates                                                                    
    APSHR55 = 0.0
    DO IVTYP=1,MAXVTYP
      DO ICL=1,MAXCLASS
        DO IREGN=1,MNUMCR
          DO ILDV=1,MAXLDV
            IF (AVSALEST(IVTYP,ICL,IREGN) .NE. 0.0) &
              APSHR55(IVTYP,ICL,IREGN,ILDV) = AVSALES(IVTYP,ICL,IREGN,ILDV) / AVSALEST(IVTYP,ICL,IREGN)
          ENDDO
        ENDDO
      ENDDO
    ENDDO

!...Calculate total personal vehicle sales and fleet vehicle sales by size class and fueling technology
    do ivtyp=1,maxvtyp
      do icl=1,maxclass
        do ildv=1,maxldv
		  TOTALSALSC(IVTYP,ICL,ILDV,n) = sum(avsales(ivtyp,icl,1:mnumcr-2,ildv))+(sum(FLTECHSAL(mnumcr,ivtyp,1:maxfleet,icl,ildv,1:maxhav))/1000000.0)
        enddo
      enddo
    enddo

!...Calculate total expenditures on vehicle purchases
    DO IVTYP=1,MAXVTYP
      EXPENDVEH(IVTYP,N) = 0.0
      DO ICL=1,MAXCLASS
        DO ILDV=1,MAXLDV
          EXPENDVEH(IVTYP,N) = EXPENDVEH(IVTYP,N) + (TOTALSALSC(IVTYP,ICL,ILDV,N) * LDV_PRI(IVTYP,ILDV,ICL,YRS))
        ENDDO
      ENDDO
    ENDDO
  
  RETURN
  END SUBROUTINE TLEGIS  ! currently turned off

! ==========================================================================================================
! ... Subroutine TFLTVMTS calculates VMT for fleets
! ==========================================================================================================
  SUBROUTINE TFLTVMTS
  USE T_
  IMPLICIT NONE

!...Total VMT by vehicle type and technology
    do ivtyp=1,maxvtyp
      do ifleet=1,maxfleet
        do ildv=1,maxldv
		  do ihav=1, maxhav
			do iregn=1,mnumcr 
			  if(iregn.ne.10) &
                fltechvmt(iregn,ivtyp,ifleet,ildv,ihav) = fltechstk(iregn,ivtyp,ifleet,ildv,ihav)*fltvmtyr(ifleet,n,ivtyp)*(1.-flt_covid(ifleet,n))
			enddo
            fltvmtech(ivtyp,ifleet,ildv,ihav) = fltechstk(mnumcr,ivtyp,ifleet,ildv,ihav)*fltvmtyr(ifleet,n,ivtyp)*(1.-flt_covid(ifleet,n))
		  enddo
        enddo
      enddo
    enddo
	
  RETURN
  END SUBROUTINE TFLTVMTS

! ==========================================================================================================
! ... Subroutine CAFECALC checks whether fuel economy of all vehicles meets CAFE standards
!     Note that CAFECALC uses a different group dimension index -- jgp instead of igp -- since it can be called inside of an igp loop (in CAFETEST)
! ==========================================================================================================
  SUBROUTINE CAFECALC(cafetestcall)
  USE T_
  IMPLICIT NONE

    integer   it,L,jgp
    real      CafeNeedX(MAXGROUP,mnumyr)
	real 	  num1, num2, den1, den2, mpgadjldv(maxvtyp,maxldv,mnumyr)
    INTEGER   cafetestcall                                                  ! If 1, CAFECALC is called from CAFETEST; CAFECALC only called to re-calculate agg attribs/mpgs (not for cafepass)
    INTEGER, PARAMETER :: CAFEGHG_DEBUG = 0                                 ! If 1, write out detailed cafe/ghg compliance information

!...Calculate EPA GHG standard
    FPghgGrp(:,n) = 0.0
    MgGhgGrp(:,n) = 0.0
    EPAghgGrp(:,n) = 0.0
    
    if(curcalyr.ge.2012) then
      do jgp=1,maxgroup
!       By size class
        do icl=1,maxclass		
		  FPghg(icl,jgp,n) = 0.0
          if(sum(cafesales(jgp,icl,yrs,1:maxldv)).gt.0.0) then
            if(jgp.le.cargrp)then
              FPghg(icl,jgp,n) = min(CFCoefEPAB2(n),max(CFCoefEPAA2(n),CFCoefEPAC2(n)*FPrint(ICL,jgp,n)+CFCoefEPAD2(n)))
            else
              FPghg(icl,jgp,n) = min((min(TFCoefEPAB2(n),max(TFCoefEPAA2(n),TFCoefEPAC2(n)*FPrint(icl,jgp,n)+TFCoefEPAD2(n)))), &
											        (min(TFCoefEPAF2(n),max(TFCoefEPAE2(n),TFCoefEPAG2(n)*FPrint(icl,jgp,n)+TFCoefEPAH2(n)))))
            endif
          endif
        enddo
!       For the whole group        
        num1 = 0.0
        do icl=1,maxclass
          num1 = num1 + FPghg(icl,jgp,n)*sum(cafesales(jgp,icl,yrs,1:maxldv))
        enddo
        FPghgGrp(jgp,n) = num1 / sum(cafesales(jgp,1:maxclass,yrs,1:maxldv))
      enddo
!   Calculate compliance (CO2 credits or debits) using 2-cycle tested mpg
	  do jgp=1,maxgroup
	    NUM1 = 0.0
	    NUM2 = 0.0    
		do ildv=1,maxldv 
		  do icl=1,maxclass
		    if(femmpg(jgp,icl,yrs,ildv).ne.0.0) then
			  if(jgp.le.cargrp) then
				if (CAFEMY27_SWITCH.eq.1.and.curcalyr.ge.2027.and.(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15)) then         ! Zero g/mi off-cycle and AC efficiency for BEVs in MY2027+
                  CYCLE
                elseif (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then
                  NUM1 = NUM1 + cafesales(jgp,icl,yrs,ildv) * (-ac_oc_credit(jgp,n)*8887)
                elseif (ildv.eq.5.or.ildv.eq.6) then
                  NUM1 = NUM1 + cafesales(jgp,icl,yrs,ildv) * (1-phev_evmt(jgp,icl,yrs,ildv)) * &
                         (1/PHEVMPG_S(jgp,icl,yrs,ildv)*8887 - ac_oc_credit(jgp,n)*8887)
                else
                  NUM1 = NUM1 + cafesales(jgp,icl,yrs,ildv) * (1/femmpg(jgp,icl,yrs,ildv)*8887 - ac_oc_credit(jgp,n)*8887)
                endif
              else  ! trucks
				if (CAFEMY27_SWITCH.eq.1.and.curcalyr.ge.2027.and.(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15)) then         ! Zero g/mi off-cycle and AC efficiency for BEVs in MY2027+
                  CYCLE
                elseif (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.14) then
                  NUM2 = NUM2 + cafesales(jgp,icl,yrs,ildv) * (-ac_oc_credit(jgp,n)*8887)
                elseif (ildv.eq.5.or.ildv.eq.6) then    
                  NUM2 = NUM2 + cafesales(jgp,icl,yrs,ildv) * (1-phev_evmt(jgp,icl,yrs,ildv)) * &
                         (1/(PHEVMPG_S(jgp,icl,yrs,ildv))*8887 - ac_oc_credit(jgp,n)*8887)
                else
                  NUM2 = NUM2 + cafesales(jgp,icl,yrs,ildv) * (1/femmpg(jgp,icl,yrs,ildv)*8887 - ac_oc_credit(jgp,n)*8887)
                endif
			  endif
			endif
		  enddo
	    enddo

!       Calculate extra zero-emission sales to throw in the denominator (Advanced Technology Multipliers)
        DEN1 = 0.0
        do ildv = 1, maxldv
          if (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.14) then 
            DEN1 = DEN1 + sum(cafesales(jgp,1:maxclass,yrs,ildv)) * (EPAALTMULT(ildv,n) - 1)
          endif
        enddo

!       Calculate production-weighted g/mi by group
!       Incorporate advanced tech multipliers for BEVs and FCVs (vehicles count for >1 sale)
!       All vehicles can claim AC leakage         
        if(jgp.le.cargrp) then
          EPAghgGrp(jgp,n) = NUM1/(sum(cafesales(jgp,1:maxclass,yrs,1:maxldv)) +  DEN1) - AC_CO2_OFFSET(jgp,n)
        else
          EPAghgGrp(jgp,n) = NUM2/(sum(cafesales(jgp,1:maxclass,yrs,1:maxldv)) +  DEN1) - AC_CO2_OFFSET(jgp,n)
        endif

      enddo

!     Calculate total MgCO2 credits/debits by group; note that cafesales is in million units, so no need to divide by 1M
      do jgp=1,maxgroup
        if (jgp.le.cargrp) then
          MgGhgGrp(jgp,n) = (FPghgGrp(jgp,n) - EPAghgGrp(jgp,n))*sum(cafesales(jgp,1:maxclass,yrs,1:maxldv)) * 195264
        else
          MgGhgGrp(jgp,n) = (FPghgGrp(jgp,n) - EPAghgGrp(jgp,n))*sum(cafesales(jgp,1:maxclass,yrs,1:maxldv)) * 225865
        endif
      enddo      
    endif
        
!...Calculate NHTSA CAFE standards
!...If the year is 2010 or later, calculate the alternative CAFE standard for each group of
!...cars and light trucks based upon the footprint of each of the classes in that group
    if(curcalyr.ge.2010) then 
      do jgp=1,maxgroup
        do icl=1,maxclass		
		  FPMpg(icl,jgp,n) = 0.0
		  if(fprint(icl,jgp,n).ne.0.0)  then 
!...        Determine the mpg standard for each class in each group based upon the footprint.
!...        cars
            if(jgp.le.cargrp)then
              FPMpg(icl,jgp,n)=1.0/((1.0/CFCoefA(n))+(1.0/CFCoefB(n)-1.0/CFCoefA(n))*(exp((FPrint(icl,jgp,n)-CFCoefC(n))/CFCoefD(n))/ &
                                    (1.0+exp((FPrint(icl,jgp,n)-CFCoefC(n))/CFCoefD(n)))))
						   
              if(curcalyr.ge.2012) then
                FPMpg(icl,jgp,n)=1.0/(min(max(((CFCoefC2(n)*FPrint(icl,jgp,n))+CFCoefD2(n)),1.0/CFCoefA2(n)),1.0/CFCoefB2(n)))
              endif
!...        light trucks
            else 
              FPMpg(icl,jgp,n)=1.0/((1.0/TFCoefA(n))+(1.0/TFCoefB(n)-1.0/TFCoefA(n))*(exp((FPrint(icl,jgp,n)-TFCoefC(n))/TFCoefD(n))/ &
                                    (1.0+exp((FPrint(icl,jgp,n)-TFCoefC(n))/TFCoefD(n)))))
              if(curcalyr.ge.2012)then
				FPMpg(icl,jgp,n)=MAX(1.0/(min(max(((TFCoefC2(n)*FPrint(icl,jgp,n))+TFCoefD2(n)),1.0/TFCoefA2(n)),1.0/TFCoefB2(n))),	&	
									 1.0/(min(max(((TFCoefG2(n)*FPrint(icl,jgp,n))+TFCoefH2(n)),1.0/TFCoefE2(n)),1.0/TFCoefF2(n))))
			  endif
		    endif
          endif
		enddo 
!...    Calculate the mpg standard for the group, weighted up over the classes.
		den1 = 0.0
		FPMpgGrp(jgp,n) = 0.0		
		do icl=1,maxclass
          if(FPMpg(ICL,jgp,n).gt.0.0) then 
		    den1 = den1 + sum(cafesales(jgp,icl,yrs,1:maxldv))/FPMpg(ICL,jgp,n)
          endif 
		enddo
		if(den1.ne.0.0) FPMpgGrp(jgp,n) = sum(cafesales(jgp,1:maxclass,yrs,1:maxldv))/den1
      enddo
    endif

!...Use the traditional standard before 2011 and the foot print standard after.
    do jgp=1,maxgroup
      if(yrs.le.2010) Cafe_Used(jgp,yrs)=Cafe_Stand(jgp,yrs)
      if(yrs.ge.2011) Cafe_Used(jgp,yrs)=FPMpgGrp(jgp,n)
    enddo

!...calculate cafe standard for fleet
	cafestd(1:maxvtyp,n) = 0.0
    do jgp = 1,maxgroup
      ivtyp = GrpMap(jgp)
	  if(cafe_used(jgp,yrs).ne.0) cafestd(ivtyp,n) = cafestd(ivtyp,n) + GrpShare(mnumcr,jgp,n)/cafe_used(jgp,yrs)
    enddo

	if(cafestd(1,n).gt.0.0) cafestd(1,n) = 1.0/cafestd(1,n)
	if(cafestd(2,n).gt.0.0) cafestd(2,n) = 1.0/cafestd(2,n)	
	if(cafestd(3,n).gt.0.0) cafestd(3,n) = 1/((cartrksplit(mnumcr,1,n)/cafestd(1,n))+(cartrksplit(mnumcr,2,n)/cafestd(2,n)))

!...calculate cafe, tested, and on-road mpg vaules compute the average new car and light truck mpg    
!...collapse new car and light truck mpg from 11 group and 8 size classes to 1 - for table 7 
!...femmpg benchmarked to historical data after xyr	

!...compliance mpg (with credits) for ftab Table 7
	if(curcalyr.le.epalyr) then
	  do jgp=1,maxgroup	 
	    DEN1 = 0.0
		do ildv=1,maxldv 
		  do icl=1,maxclass
!		    if(mpgcomp(jgp,icl,yrs,ildv).ne.0.0) DEN1 = DEN1 + (cafesales(jgp,icl,yrs,ildv)/mpgcomp(jgp,icl,yrs,ildv))
		    if(mpgcomp(jgp,icl,yrs,ildv).ne.0.0) DEN1 = DEN1 + cafesales(jgp,icl,yrs,ildv)/ ( (1/mpgcomp(jgp,icl,yrs,ildv) - ac_oc_credit(jgp,n))**-1 )
		  enddo
	    enddo
		if(DEN1.ne.0.0) CafeMpgGrp(jgp,n) = sum(cafesales(jgp,1:maxclass,yrs,1:maxldv))/DEN1
	  enddo 
	elseif(curcalyr.gt.epalyr) then
	  do jgp=1,maxgroup
        ivtyp = GrpMap(jgp)
	    DEN1 = 0.0
	    DEN2 = 0.0
		do ildv=1,maxldv 
		  do icl=1,maxclass
		    if(femmpg(jgp,icl,yrs,ildv).gt.0.0) then
!             BEVs don't qualify for ac/oc credits in MY2027+ in MY2027-MY2032 CAFE/GHG reg
              if (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then
                if (CAFEMY27_SWITCH.eq.1.and.curcalyr.ge.2027) then
                  if(ivtyp.eq.1) DEN1 = DEN1 + cafesales(jgp,icl,yrs,ildv)/(femmpg(jgp,icl,yrs,ildv)*cafepefmult(ildv,n))
                  if(ivtyp.eq.2) DEN2 = DEN2 + cafesales(jgp,icl,yrs,ildv)/(femmpg(jgp,icl,yrs,ildv)*cafepefmult(ildv,n))
                else
                  if(ivtyp.eq.1) DEN1 = DEN1 + cafesales(jgp,icl,yrs,ildv)/((1/(femmpg(jgp,icl,yrs,ildv)*cafepefmult(ildv,n)) - ac_oc_credit(jgp,n))**-1)
                  if(ivtyp.eq.2) DEN2 = DEN2 + cafesales(jgp,icl,yrs,ildv)/((1/(femmpg(jgp,icl,yrs,ildv)*cafepefmult(ildv,n)) - ac_oc_credit(jgp,n))**-1)
                endif
              else
                if(ivtyp.eq.1) DEN1 = DEN1 + cafesales(jgp,icl,yrs,ildv)/((1/(femmpg(jgp,icl,yrs,ildv)*cafepefmult(ildv,n)) - ac_oc_credit(jgp,n))**-1)
                if(ivtyp.eq.2) DEN2 = DEN2 + cafesales(jgp,icl,yrs,ildv)/((1/(femmpg(jgp,icl,yrs,ildv)*cafepefmult(ildv,n)) - ac_oc_credit(jgp,n))**-1)
              endif
			endif
		  enddo
	    enddo
		if(ivtyp.eq.1.and.DEN1.ne.0.0) CafeMpgGrp(jgp,n) = sum(cafesales(jgp,1:maxclass,yrs,1:maxldv))/DEN1
		if(ivtyp.eq.2.and.DEN2.ne.0.0) CafeMpgGrp(jgp,n) = sum(cafesales(jgp,1:maxclass,yrs,1:maxldv))/DEN2
	  enddo ! jgp
	endif

!...for Table 7	
	if(curcalyr.le.epalyr) then 
      NewMPG(1,n) = 0.0 
	  NewMPG(2,n) = 0.0
      do jgp=1,cargrp
	    if(cafempggrp(jgp,n).ne.0) NewMPG(1,n) = NewMPG(1,n) + sum(cafesales(jgp,:,yrs,:)) /cafempggrp(jgp,n)
      enddo
	  do jgp=ltkgrp,maxgroup
	    if(cafempggrp(jgp,n).ne.0) NewMPG(2,n) = NewMPG(2,n) + sum(cafesales(jgp,:,yrs,:)) /cafempggrp(jgp,n)
	  enddo
	  NewMPG(1,n) = sum(cafesales(1:cargrp,:,yrs,:))/NewMPG(1,n)
	  NewMPG(2,n) = sum(cafesales(ltkgrp:maxgroup,:,yrs,:))/NewMPG(2,n)
	  NewMPG(3,n) = sum(cafesales(:,:,yrs,:))/((sum(cafesales(1:cargrp,:,yrs,:))/NewMPG(1,n))+sum(cafesales(ltkgrp:maxgroup,:,yrs,:))/NewMPG(2,n))
	endif	

!...Check individual group CAFE against the standard.
!   Do the same for EPA tailpipe GHG, if RUN_EPA option is selected
    if(cafetestcall.eq.0) then 
      do jgp=1,MAXGROUP
        ivtyp = GrpMap(jgp)
        cafepass(jgp)= .true.
        if(CafeMpgGrp(jgp,n).lt.Cafe_Used(jgp,yrs)) cafepass(jgp)= .false.
        if(RUN_EPA.eq.1.and.MgGhgGrp(jgp,n).lt.0.0) cafepass(jgp)= .false.

        if (CAFEGHG_DEBUG.eq.1) then
          if(curcalyr.gt.2022.and.fcrl.eq.1.and.jgp.eq.1) WRITE(21,'(a14,",",2(a4,","),a12,",",8(a12,","))')'cafepass','year','grp','pass?','compliance','standard','comp_car','stndrd_car','comp_trk','stndrd_trk','comp_all','stndrd_all'
          if(curcalyr.gt.2022.and.fcrl.eq.1.and.pass.eq.3) WRITE(21,'(a14,",",2(i4,","),l12,",",8(f12.1,","))')'cafepass_nhtsa',curcalyr,jgp,cafepass(jgp),CafeMpgGrp(jgp,n),Cafe_Used(jgp,yrs), &
                                                                                        NewMPG(1,n),cafestd(1,n),NewMPG(2,n),cafestd(2,n),NewMPG(3,n),cafestd(3,n)
          if(curcalyr.gt.2022.and.fcrl.eq.1.and.pass.eq.3) WRITE(21,'(a14,",",2(i4,","),l12,",",8(f12.1,","))')'cafepass_epa',curcalyr,jgp,cafepass(jgp),MgGhgGrp(jgp,n),0.0, &
                                                                                        sum(MgGhgGrp(1:cargrp,n)),0.0,sum(MgGhgGrp(6:11,n)),0.0,sum(MgGhgGrp(1:11,n)),0.0
        endif
      end do
    endif

!...tested mpg (without credits) for ftab table 7
	do ivtyp=1,maxvtyp
	  DEN1 = 0.0
	  DEN2 = 0.0
	  do ildv=1,maxldv 
		do jgp=1,maxgroup
		  do icl=1,maxclass
			if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
			  if(jgp.le.cargrp) then
				DEN1 = DEN1 + (cafesales(jgp,icl,yrs,ildv)/FemMpg(jgp,icl,yrs,ildv))
			  else
				DEN2 = DEN2 + (cafesales(jgp,icl,yrs,ildv)/FemMpg(jgp,icl,yrs,ildv))
			  endif
		    endif
		  enddo
		enddo
	  enddo
	  if(ivtyp.eq.1) then
	    if(DEN1.ne.0.0) TrueMPG(ivtyp,n) = sum(cafesales(1:cargrp,1:maxclass,yrs,1:maxldv))/DEN1
	  else
		if(DEN2.ne.0.0) TrueMPG(ivtyp,n) = sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv))/DEN2
	  endif 
	enddo
	TrueMpg(3,n) = 1/(cartrksplit(mnumcr,1,n)/truempg(1,n)+(cartrksplit(mnumcr,2,n))/truempg(2,n))	

!...calculate mpgadj after xyr 
	if(curcalyr.gt.epalyr) then
	  do jgp=1,maxgroup 
		do icl=1,maxclass 
		  do ildv=1,maxldv 
		    mpgadj(jgp,icl,yrs,ildv) = femmpg(jgp,icl,yrs,ildv)*degfacgrp(jgp,icl,ildv,n)
		  enddo 
		enddo 
	  enddo 
	endif

!...on-road mpg (adjusted tested) for ftab table 7
	do ivtyp=1,maxvtyp
	  DEN1 = 0.0
	  DEN2 = 0.0
	  do ildv=1,maxldv 
		do jgp=1,maxgroup
		  do icl=1,maxclass
			if(MpgAdj(jgp,icl,yrs,ildv).ne.0.0) then
			  if(jgp.le.cargrp) then
				DEN1 = DEN1 + (cafesales(jgp,icl,yrs,ildv)/mpgadj(jgp,icl,yrs,ildv))
			  else
				DEN2 = DEN2 + (cafesales(jgp,icl,yrs,ildv)/mpgadj(jgp,icl,yrs,ildv))
			  endif
			endif
		  enddo
		enddo
	  enddo
	  if(ivtyp.eq.1) then
		if(DEN1.ne.0.0) AdjMpg(ivtyp,n) = sum(cafesales(1:cargrp,1:maxclass,yrs,1:maxldv))/DEN1
	  else
		if(DEN2.ne.0.0) AdjMpg(ivtyp,n) = sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv))/DEN2
	  endif 
	enddo
	AdjMpg(3,n) = 1/(cartrksplit(mnumcr,1,n)/adjmpg(1,n)+(cartrksplit(mnumcr,2,n))/adjmpg(2,n))

!...calculate US new ldv fuel economy by ildv for ftab table 50
	ldvmpgnew(:,:,:,n) = 0.0
    do ildv=1,maxldv
	  DEN1 = 0.0
	  DEN2 = 0.0
	  do jgp=1,maxgroup
		do icl=1,maxclass 
		  if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
			if(jgp.le.cargrp) then		    
			  DEN1 = DEN1 + (cafesales(jgp,icl,yrs,ildv)/FemMpg(jgp,icl,yrs,ildv))
			else
			  DEN2 = DEN2 + (cafesales(jgp,icl,yrs,ildv)/FemMpg(jgp,icl,yrs,ildv))
			endif
		  endif
		enddo
	  enddo
	  if(DEN1.ne.0.0) ldvmpgnew(mnumcr,1,ildv,n) = sum(cafesales(1:cargrp,1:maxclass,yrs,ildv))/DEN1
	  if(DEN2.ne.0.0) ldvmpgnew(mnumcr,2,ildv,n) = sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,ildv))/DEN2
	enddo 
	
!...fill regional mpg values through 2018, regional = US
	if(curcalyr.le.2018) then
	  do iregn=1,mnumcr-2
		do ivtyp=1,maxvtyp 
		  do ildv=1,maxldv
		  	ldvmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,ildv,n)
		  enddo
		enddo 
	  enddo 
	endif ! <2018

!...calculate regional mpg >= 2019
	if(curcalyr.ge.2019) then
	  do iregn=1,mnumcr-2 
	    do ivtyp=1,maxvtyp 
		  do ildv=1,maxldv 
		    ldvmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,ildv,n)
		  enddo 
		enddo 
	  enddo
	  do iregn=1,mnumcr-2
	    do ildv=1,maxldv
		  DEN1 = 0.0
		  DEN2 = 0.0
		  do jgp=1,maxgroup
		    do icl=1,maxclass 
			  if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
				if(jgp.le.cargrp) then		    
				  DEN1 = DEN1 + (ldv_sales(jgp,icl,ildv,iregn,n)/FemMpg(jgp,icl,yrs,ildv))
				else
				  DEN2 = DEN2 + (ldv_sales(jgp,icl,ildv,iregn,n)/FemMpg(jgp,icl,yrs,ildv))
				endif
			  endif
			enddo
		  enddo
		  if(DEN1.ne.0.0) ldvmpgnew(iregn,1,ildv,n) = sum(ldv_sales(1:cargrp,1:maxclass,ildv,iregn,n))/DEN1
		  if(DEN2.ne.0.0) ldvmpgnew(iregn,2,ildv,n) = sum(ldv_sales(ltkgrp:maxgroup,1:maxclass,ildv,iregn,n))/DEN2
		enddo
	  enddo
!...  calculate average car and light truck fuel economy by region for size class model	   
	  do iregn=1,mnumcr-2 
		do ivtyp=1,maxvtyp 
		  den1 = 0.0
		  den2 = 0.0
		  do ildv=1,maxldv
			if(ldvmpgnew(iregn,ivtyp,ildv,n).ne.0.0) then 
			  if(ivtyp.eq.1) then 
				den1 = den1 + sum(ldv_sales(1:cargrp,1:maxclass,ildv,iregn,n))/ldvmpgnew(iregn,ivtyp,ildv,n)
			  else 
			    den2 = den2 + sum(ldv_sales(ltkgrp:maxgroup,1:maxclass,ildv,iregn,n))/ldvmpgnew(iregn,ivtyp,ildv,n) 
			  endif 
			endif 
		  enddo 
		  if(den1.ne.0.0) truempg_regn(iregn,1,n) = sum(ldv_sales(1:cargrp,1:maxclass,1:maxldv,iregn,n))/den1 
		  if(den2.ne.0.0) truempg_regn(iregn,2,n) = sum(ldv_sales(ltkgrp:maxgroup,1:maxclass,1:maxldv,iregn,n))/den2
		enddo 
	  enddo 
	endif ! >= 2019
	
!...calculate mpg on-road adjustment factors by ildv
	if(curcalyr.le.epalyr) then
	  do ivtyp=1,maxvtyp
		do ildv=1,maxldv
		  DEN1 = 0.0
		  DEN2 = 0.0
		  do jgp=1,maxgroup
			do icl=1,maxclass 
			  if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
			    if(jgp.le.cargrp) then		    
			      DEN1 = DEN1 + (cafesales(jgp,icl,yrs,ildv)/mpgadj(jgp,icl,yrs,ildv))
				else
			  	  DEN2 = DEN2 + (cafesales(jgp,icl,yrs,ildv)/mpgadj(jgp,icl,yrs,ildv))
				endif
			  endif
		    enddo
		  enddo
		  if(ivtyp.eq.1) then
			if(DEN1.ne.0.0) mpgadjldv(ivtyp,ildv,n) = sum(cafesales(1:cargrp,1:maxclass,yrs,ildv))/DEN1
		  else
			if(DEN2.ne.0.0) mpgadjldv(ivtyp,ildv,n) = sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,ildv))/DEN2
		  endif
		enddo	  
!...    calculate mpg degredation factors
	  	degrpt(ivtyp,n) = adjmpg(ivtyp,n)/truempg(ivtyp,n)
!...    calculate average mgp degredation factor by ildv
		do ildv=1,maxldv
          degfac(ivtyp,ildv,n) = 0.0
		  if(ldvmpgnew(mnumcr,ivtyp,ildv,n).ne.0.0) then
		    degfac(ivtyp,ildv,n) = mpgadjldv(ivtyp,ildv,n)/ldvmpgnew(mnumcr,ivtyp,ildv,n) 
		  endif 
		  if(degfac(ivtyp,ildv,n-1).gt.0.0.and.degfac(ivtyp,ildv,n).eq.0.0) degfac(ivtyp,ildv,n) = degfac(ivtyp,ildv,n-1) 
		  if(degfac(ivtyp,ildv,n).eq.0.0) degfac(ivtyp,ildv,n) = degrpt(ivtyp,n)  
		enddo
	  enddo
	else ! > epalyr
	  do ivtyp=1,maxvtyp 
		degrpt(ivtyp,n) = adjmpg(ivtyp,n)/truempg(ivtyp,n)
        do ildv=1,maxldv 
		  degfac(ivtyp,ildv,n) = degfac(ivtyp,ildv,n-1) 
		enddo 
	  enddo 
	endif
	
!...calculate mpg degredation factor by jgp, icl, ildv
	if(curcalyr.le.epalyr) then
	  do jgp=1,maxgroup 
		do icl=1,maxclass 
		  do ildv=1,maxldv 
		    degfacgrp(jgp,icl,ildv,n) = 0.0
		    if(femmpg(jgp,icl,yrs,ildv).ne.0.0) then
			  degfacgrp(jgp,icl,ildv,n) = mpgadj(jgp,icl,yrs,ildv)/femmpg(jgp,icl,yrs,ildv)
			endif
			if(degfacgrp(jgp,icl,ildv,n).eq.0.0) then 
			  if(jgp.le.cargrp) then 
			    degfacgrp(jgp,icl,ildv,n) = degfac(1,ildv,n) 
			  else 
			    degfacgrp(jgp,icl,ildv,n) = degfac(2,ildv,n)
			  endif			  
			endif 
		  enddo 
		enddo 
	  enddo
	else 
	  do jgp=1,maxgroup 
	    do icl=1,maxclass 
		  do ildv=1,maxldv 
			degfacgrp(jgp,icl,ildv,n) = degfacgrp(jgp,icl,ildv,n-1)
		  enddo 
		enddo 
	  enddo 
	endif

!...Code for doing the CAFE banking. For current testing IBank=0 does not do banking, IBank=1 does banking.
    if(IBank.eq.1.and.curcalyr.ge.epalyr) then
!...Go through each of the manufacturing groups.
      do jgp=1,MAXGROUP
!...    On first pass and first iteration, establish the starting values for this xyr from from previous year's
!...    working values and from last year's new bank.
		if(curcalyr.eq.epalyr) then
		  do i=1,4
		    CafeWork(i,jgp) = 0.0
		  enddo
		  CafeBankA(jgp) = 0.0
!...      xyr bank
          if(CafeMpgGrp(jgp,n).gt.Cafe_Used(jgp,yrs)) CafeBankA(jgp) = CafeMpgGrp(jgp,n)-Cafe_Used(jgp,yrs)
!...	  previous year's banks
		  do i=1,4
		    if(CafeMpgGrp(jgp,n-i).gt.Cafe_Used(jgp,yrs-i)) CafeWork(i,jgp) = CafeMpgGrp(jgp,n-i)-Cafe_Used(jgp,yrs-i)
		  enddo 
		endif
        if(pass.eq.1.and.curitr.eq.1) then
          CafeBank(5,jgp) = CafeWork(4,jgp)
          CafeBank(4,jgp) = CafeWork(3,jgp)
          CafeBank(3,jgp) = CafeWork(2,jgp)
          CafeBank(2,jgp) = CafeWork(1,jgp)
          CafeBank(1,jgp) = CafeBankA(jgp)
          CafeBankA(jgp)  = 0.0
!         write(H2UNIT,'(a,4i5,5f8.3)') '**BnkInt ',curiyr+1989,curitr,pass,jgp,CafeBank(1,jgp),CafeBank(2,jgp),CafeBank(3,jgp),CafeBank(4,jgp),CafeBank(5,jgp)
        endif
!...    On first pass and every iteration, put the saved values into working values. This is necessary
!...    so that we have the fresh bank values at the start of each new iteration.
        if(pass.eq.1) then
          CafeWork(5,jgp) = CafeBank(5,jgp)
          CafeWork(4,jgp) = CafeBank(4,jgp)
          CafeWork(3,jgp) = CafeBank(3,jgp)
          CafeWork(2,jgp) = CafeBank(2,jgp)
          CafeWork(1,jgp) = CafeBank(1,jgp)
!...    On the first pass, if the group passed, then bank its excess MPG. Otherwise, pull values out of the bank.
          if(CafePass(jgp).eq. .true.) then
            CafeBankA(jgp) = CafeMpgGrp(jgp,n)-Cafe_Used(jgp,yrs)
!            write(H2UNIT,'(a,4i5,f8.3)') '**BnkPas ',curiyr+1989,curitr,pass,jgp,CafeBankA(jgp)
          else
!...    Get the total amount by which the group did not pass the cafe standard.
            CafeNeed = Cafe_Used(jgp,yrs)-CafeMpgGrp(jgp,n)
            CafeNeedX(jgp,n) = CafeNeed
!...    Work backwards through the bank and see if we can make up the difference.
            do i=5,1,-1
              if(CafeNeed.gt.0.0) then
                if(CafeNeed.le.CafeWork(i,jgp)) then
                  CafeWork(i,jgp) = CafeWork(i,jgp)-CafeNeed
                  CafeNeed = 0.0
                  CafePass(jgp) = .true.
                else
                  CafeNeed = CafeNeed-CafeWork(i,jgp)
                  CafeWork(i,jgp) = 0.0
                endif
              endif
            enddo
            if(CafeNeed.eq.0.0)then
              CafePass(jgp)=.true.
              CafeMpgGrp(jgp,n) = Cafe_Used(jgp,yrs)
            endif
!            write(H2UNIT,'(a,4i5,7f8.3,i8)') '**BnkFal ',curiyr+1989,curitr,pass,jgp,CafeNeedX(jgp,n),CafeWork(1,jgp),CafeWork(2,jgp),CafeWork(3,jgp),&
!                                                         CafeWork(4,jgp),CafeWork(5,jgp),CafeNeed,CafePass(jgp)
          endif
        endif
      enddo
    endif
    
    do jgp=1,MAXGROUP
      bankbal(jgp,yrs)=sum(CafeBank(1:5,jgp))
    enddo

!...calculate average new tested mpg with AFV credits and banking
	if(curcalyr.gt.epalyr) then 
      NewMPG(1,n) = 0.0 
	  NewMPG(2,n) = 0.0
      do jgp=1,cargrp
	    if(cafempggrp(jgp,n).ne.0) NewMPG(1,n) = NewMPG(1,n) + sum(cafesales(jgp,:,yrs,:)) /cafempggrp(jgp,n)
      enddo
	  do jgp=ltkgrp,maxgroup
	    if(cafempggrp(jgp,n).ne.0) NewMPG(2,n) = NewMPG(2,n) + sum(cafesales(jgp,:,yrs,:)) /cafempggrp(jgp,n)
	  enddo
	  NewMPG(1,n) = sum(cafesales(1:cargrp,:,yrs,:))/NewMPG(1,n)
	  NewMPG(2,n) = sum(cafesales(ltkgrp:maxgroup,:,yrs,:))/NewMPG(2,n)
	  NewMPG(3,n) = sum(cafesales(:,:,yrs,:))/((sum(cafesales(1:cargrp,:,yrs,:))/NewMPG(1,n))+sum(cafesales(ltkgrp:maxgroup,:,yrs,:))/NewMPG(2,n))
	endif	

!...calculate average AFV mpg by size class for ftab table 52
!   AFVs = anything that isn't non-HEV gasoline or diesel
	do ivtyp=1,maxvtyp
      do icl=1,maxclass
		DEN1 = 0.0
		DEN2 = 0.0	    
		do ildv=3,maxldv
          do jgp=1,maxgroup
			if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
			  if(jgp.le.cargrp) then
				DEN1 = DEN1 + cafesales(jgp,icl,yrs,ildv)/FemMpg(jgp,icl,yrs,ildv)
			  else
				DEN2 = DEN2 + cafesales(jgp,icl,yrs,ildv)/FemMpg(jgp,icl,yrs,ildv)
			  endif
			endif
          enddo
		enddo
		if(ivtyp.eq.1) then
          if(DEN1.ne.0.0) AFVFE(ivtyp,icl,n) = sum(cafesales(1:cargrp,icl,yrs,3:maxldv))/DEN1
		else
		  if(DEN2.ne.0.0) AFVFE(ivtyp,icl,n) = sum(cafesales(ltkgrp:maxgroup,icl,yrs,3:maxldv))/DEN2
		endif 
	  enddo
	enddo

!...calculate average mpg for AFVs for ftab table 52
!   AFVs = anything that isn't non-HEV gasoline or diesel
	do ivtyp=1,maxvtyp
	  DEN1 = 0.0
	  DEN2 = 0.0	
	  do jgp=1,maxgroup
		do icl=1,maxclass 
	      do ildv=3,maxldv
			if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
			  if(jgp.le.cargrp) then
				DEN1 = DEN1 + cafesales(jgp,icl,yrs,ildv)/FemMpg(jgp,icl,yrs,ildv)
			  else 
				DEN2 = DEN2 + cafesales(jgp,icl,yrs,ildv)/FemMpg(jgp,icl,yrs,ildv)
			  endif
		    endif
		  enddo
	    enddo
	  enddo
      if(ivtyp.eq.1) then
		if(DEN1.ne.0.0) AFVFETOT(ivtyp,n) = sum(cafesales(1:cargrp,1:maxclass,yrs,3:maxldv))/DEN1
	  else
		if(DEN2.ne.0.0) AFVFETOT(ivtyp,n) = sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,3:maxldv))/DEN2	
	  endif 
	enddo

!...Calculate vehicle group-average technology penetration rate (%) and cost, % for ftab Table 60 
!...calculate historical values from input data, then projected values 
	If(curcalyr.ge.xyr)then 
      DO jgp=1,MAXGROUP
        DO ITECH=1,NUMTECH
          DO ILDV=1,MAXLDV
            MKT_PENF(jgp,ITECH,ILDV) = 0.0
            AVCOST(jgp,ITECH,ILDV) = 0.0
            DO ICL=1,MAXCLASS
              MKT_PENF(jgp,ITECH,ILDV) = MKT_PENF(jgp,ITECH,ILDV) + MKT_PEN(ICL,jgp,ITECH,CURRENT,ILDV) * CLASS_SHARE(mnumcr,ICL,jgp,YRS) * 100.0
              AVCOST(jgp,ITECH,ILDV) = AVCOST(jgp,ITECH,ILDV) + TEC_ORNL(ICL,jgp,ITECH,ILDV) * CLASS_SHARE(mnumcr,ICL,jgp,YRS)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

!...Sum over the manufacturer groups to produce a market penetration rate (%) and average
!...cost (90_) tables, but only for gasoline vehicles.
      do itech=1,numtech
        do it=1,maxvtyp+1
          MKT_D_P(it,itech,yrs)=0.0
          AvgCost(it,itech,yrs)=0.0
        enddo
        do jgp=1,MAXGROUP
          it=GrpMap(jgp)
          MKT_D_P(it,itech,yrs)=MKT_D_P(it,itech,yrs)+(Mkt_Penf(jgp,itech,gas)*GrpShare(mnumcr,jgp,n))
          AvgCost(it,itech,yrs)=AvgCost(it,itech,yrs)+(AvCost(jgp,itech,gas)*GrpShare(mnumcr,jgp,n))
        enddo
        MKT_D_P(3,itech,yrs)=MKT_D_P(1,itech,yrs)*CarTrkSplit(mnumcr,1,N)+MKT_D_P(2,itech,yrs)*CarTrkSplit(mnumcr,2,N)
        AvgCost(3,itech,yrs)=AvgCost(1,itech,yrs)*CarTrkSplit(mnumcr,1,N)+AvgCost(2,itech,yrs)*CarTrkSplit(mnumcr,2,N)
      end do

!...sum micro hybrid vehicle sales shares - for ftab Table 48
!...update this equation when tech list is updated (64 = 12V micro hybrid, 65 = BISG) UPDATE THIS WHEN TECH MENU IS UPDATED
      do ILDV=1,maxldv
        do it=1,maxvtyp
          micropen(it,ILDV,n)=0.0
        enddo
        do jgp=1,MAXGROUP
          it=GrpMap(jgp)
          micropen(it,ILDV,n)=micropen(it,ILDV,n)+(Mkt_Penf(jgp,64,ILDV)/100*GrpShare(mnumcr,jgp,n))+(Mkt_Penf(jgp,65,ILDV)/100*GrpShare(mnumcr,jgp,n)) 
        enddo
      enddo
	endif ! >= xyr year

!...calculate fuel economy, hp, price, and curb weight by vtyp, icl and ildv
	do icl=1,maxclass
	  do ildv=1,maxldv
!...	fuel economy (Table 113)
		den1 = 0.0
		den2 = 0.0
        do jgp=1,maxgroup
		  if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
		    if(jgp.le.cargrp) then
			  den1 = den1 + cafesales(jgp,icl,yrs,ildv)/FemMpg(jgp,icl,yrs,ildv)
			else
			  den2 = den2 + cafesales(jgp,icl,yrs,ildv)/FemMpg(jgp,icl,yrs,ildv)
			endif
		  endif
		enddo
		do ivtyp=1,maxvtyp 
		  ldv_mpg_cl(ivtyp,ildv,icl,yrs) = 0.0
		  if(ivtyp.eq.1) then
		    if(den1.ne.0.0) LDV_MPG_CL(ivtyp,ildv,icl,yrs) = sum(cafesales(1:cargrp,icl,yrs,ildv))/DEN1
		  else
		    if(den2.ne.0.0) LDV_MPG_CL(ivtyp,ildv,icl,yrs) = sum(cafesales(ltkgrp:maxgroup,icl,yrs,ildv))/DEN2
		  endif 
		enddo
!...	horsepower (Table 52)
		num1 = 0.0
		num2 = 0.0 
		den1 = 0.0
		den2 = 0.0
		do jgp=1,maxgroup	
		  if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
		    if(jgp.le.cargrp) then
			  num1 = num1 + cafesales(jgp,icl,yrs,ildv) * femhp(jgp,icl,yrs,ildv)
			  den1 = den1 + cafesales(jgp,icl,yrs,ildv) 
			else
			  num2 = num2 + cafesales(jgp,icl,yrs,ildv) * femhp(jgp,icl,yrs,ildv)
			  den2 = den2 + cafesales(jgp,icl,yrs,ildv) 
			endif
		  endif
		enddo 	
		do ivtyp=1,maxvtyp
		  LDVHPW(ivtyp,ildv,icl,yrs) = 0.0
		  if(ivtyp.eq.1) then 
		    if(den1.ne.0.0) LDVHPW(ivtyp,ildv,icl,yrs) = num1/den1	
		  else
		    if(den2.ne.0.0) LDVHPW(ivtyp,ildv,icl,yrs) = num2/den2	
		  endif 
		enddo
!...	MSRP, vehicle price (Table 114)
		num1 = 0.0
		num2 = 0.0 
		den1 = 0.0
		den2 = 0.0
		do jgp=1,maxgroup	
		  if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
		    if(jgp.le.cargrp) then
			  num1 = num1 + cafesales(jgp,icl,yrs,ildv) * fempri(jgp,icl,yrs,ildv)
			  den1 = den1 + cafesales(jgp,icl,yrs,ildv) 
			else
			  num2 = num2 + cafesales(jgp,icl,yrs,ildv) * fempri(jgp,icl,yrs,ildv)
			  den2 = den2 + cafesales(jgp,icl,yrs,ildv) 
			endif
		  endif
		enddo 
		do ivtyp=1,maxvtyp
		  LDV_PRI(ivtyp,ildv,icl,yrs) = 0.0		
		  if(ivtyp.eq.1) then
		    if(den1.ne.0.0) LDV_PRI(ivtyp,ildv,icl,yrs) = num1/den1	
		  else
		    if(den2.ne.0.0) LDV_PRI(ivtyp,ildv,icl,yrs) = num2/den2
		  endif
		enddo
!...	Driving range (Table 115)
		num1 = 0.0
		num2 = 0.0 
		den1 = 0.0
		den2 = 0.0
		do jgp=1,maxgroup	
		  if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
		    if(jgp.le.cargrp) then
			  num1 = num1 + cafesales(jgp,icl,yrs,ildv) * femrng(jgp,icl,yrs,ildv)
			  den1 = den1 + cafesales(jgp,icl,yrs,ildv) 
			else
			  num2 = num2 + cafesales(jgp,icl,yrs,ildv) * femrng(jgp,icl,yrs,ildv)
			  den2 = den2 + cafesales(jgp,icl,yrs,ildv) 
			endif
		  endif
		enddo 
		do ivtyp=1,maxvtyp 
		  LDV_RNG(ivtyp,ildv,icl,yrs) = 0.0		
		  if(ivtyp.eq.1) then
		    if(den1.ne.0.0) LDV_RNG(ivtyp,ildv,icl,yrs) = num1/den1	
		  else
			if(den2.ne.0.0) LDV_RNG(ivtyp,ildv,icl,yrs) = num2/den2
		  endif
		enddo
!...	Vehicle weight (Table 52)
		num1 = 0.0
		num2 = 0.0 
		den1 = 0.0
		den2 = 0.0
		do jgp=1,maxgroup	
		  if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
		    if(jgp.le.cargrp) then
			  num1 = num1 + cafesales(jgp,icl,yrs,ildv) * femwgt(jgp,icl,yrs,ildv)
			  den1 = den1 + cafesales(jgp,icl,yrs,ildv) 
			else
			  num2 = num2 + cafesales(jgp,icl,yrs,ildv) * femwgt(jgp,icl,yrs,ildv)
			  den2 = den2 + cafesales(jgp,icl,yrs,ildv) 
			endif
		  endif
		enddo 	
		do ivtyp=1,maxvtyp
		  WGT(ivtyp,ildv,icl,yrs) = 0.0
		  if(ivtyp.eq.1) then 
			if(den1.ne.0.0) WGT(ivtyp,ildv,icl,yrs) = num1/den1	
		  else
			if(den2.ne.0.0) WGT(ivtyp,ildv,icl,yrs) = num2/den2		
		  endif 
		enddo
	  enddo
	enddo

!...Calculate average horsepower and weight for new gasoline cars and light trucks for Table 52
!...and size class choice model
!...weight
	num1 = 0.0
	num2 = 0.0 
	den1 = 0.0
	den2 = 0.0
	do jgp=1,maxgroup	
	  do icl=1,maxclass
		do ildv=1,maxldv
          if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
		    if(jgp.le.cargrp) then
		      num1 = num1 + cafesales(jgp,icl,yrs,ildv) * femwgt(jgp,icl,yrs,ildv)
		  	  den1 = den1 + cafesales(jgp,icl,yrs,ildv) 
		    else
		  	  num2 = num2 + cafesales(jgp,icl,yrs,ildv) * femwgt(jgp,icl,yrs,ildv)
		  	  den2 = den2 + cafesales(jgp,icl,yrs,ildv) 
		    endif
		  endif
        enddo
	  enddo
	enddo
	if(den1.ne.0.0) AWTCAR(mnumcr,n) = num1/den1	
	if(den2.ne.0.0) AWTTRUCK(mnumcr,n) = num2/den2	
!...horsepower 
	num1 = 0.0
	num2 = 0.0 
	den1 = 0.0
	den2 = 0.0
	do jgp=1,maxgroup	
	  do icl=1,maxclass
		do ildv=1,maxldv
          if(FemMpg(jgp,icl,yrs,ildv).ne.0.0) then
            if(jgp.le.cargrp) then
		      num1 = num1 + cafesales(jgp,icl,yrs,ildv) * femhp(jgp,icl,yrs,ildv)
		  	  den1 = den1 + cafesales(jgp,icl,yrs,ildv) 
		    else
		  	  num2 = num2 + cafesales(jgp,icl,yrs,ildv) * femhp(jgp,icl,yrs,ildv)
		  	  den2 = den2 + cafesales(jgp,icl,yrs,ildv) 
		    endif
		  endif
        enddo
	  enddo
	enddo
	if(den1.ne.0.0) AHPCAR(mnumcr,n) = num1/den1	
	if(den2.ne.0.0) AHPTRUCK(mnumcr,n) = num2/den2
!...fill regional with US pre 2019
	if(curcalyr.lt.2019) then 
	  do iregn=1,mnumcr-2 
        AHPCAR(iregn,n)   = AHPCAR(mnumcr,n) 
        AHPTRUCK(iregn,n) = AHPTRUCK(mnumcr,n) 
        AWTCAR(iregn,n)   = AWTCAR(mnumcr,n)    
        AWTTRUCK(iregn,n) = AWTTRUCK(mnumcr,n)	    
	  enddo 
	endif
!...calculate regional values post 2019
	do iregn=1,mnumcr-2
	  num1 = 0.0
	  num2 = 0.0 
	  den1 = 0.0
	  den2 = 0.0
	  do jgp=1,maxgroup	
		do icl=1,maxclass
		  do ildv=1,maxldv
            if(FemMpg(jgp,icl,yrs,ildv).ne.0.0.and.ldv_sales(jgp,icl,ildv,iregn,n).ne.0.0) then
!		      if(femwgt(jgp,icl,yrs,gas).ne.femwgt(jgp,icl,yrs,gas).or.femwgt(jgp,icl,yrs,gas).eq.0.0) then
!                WRITE(21,'(a,",",4(i4,","),2(f12.4,","))')'ERROR: LDV no HP',curcalyr,curitr,jgp,icl,femwgt(jgp,icl,yrs,gas),cafesales(jgp,icl,yrs,gas)
!                WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
!                STOP
!              endif
		 	  if(jgp.le.cargrp) then
		        num1 = num1 + ldv_sales(jgp,icl,ildv,iregn,n) * femwgt(jgp,icl,yrs,ildv)
		 	    den1 = den1 + ldv_sales(jgp,icl,ildv,iregn,n) 
		 	  else
		 	    num2 = num2 + ldv_sales(jgp,icl,ildv,iregn,n) * femwgt(jgp,icl,yrs,ildv)
		 	    den2 = den2 + ldv_sales(jgp,icl,ildv,iregn,n) 
		      endif
		    endif
          enddo
		enddo
	  enddo
	  if(den1.ne.0.0) AWTCAR(iregn,n) = num1/den1	
	  if(den2.ne.0.0) AWTTRUCK(iregn,n) = num2/den2	
!...horsepower 
	  num1 = 0.0
	  num2 = 0.0 
	  den1 = 0.0
	  den2 = 0.0
	  do jgp=1,maxgroup	
	    do icl=1,maxclass
		  do ildv=1,maxldv
            if(FemMpg(jgp,icl,yrs,ildv).ne.0.0.and.ldv_sales(jgp,icl,ildv,iregn,n).ne.0.0) then
!		    if(femhp(jgp,icl,yrs,gas).ne.femhp(jgp,icl,yrs,gas).or.femhp(jgp,icl,yrs,gas).eq.0.0) then
!              WRITE(21,'(a,",",4(i4,","),2(f12.4,","))')'ERROR: LDV no HP',curcalyr,curitr,jgp,icl,femhp(jgp,icl,yrs,gas),cafesales(jgp,icl,yrs,gas)
!              WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
!              STOP
!            endif
		      if(jgp.le.cargrp) then
		        num1 = num1 + ldv_sales(jgp,icl,ildv,iregn,n) * femhp(jgp,icl,yrs,ildv)
			    den1 = den1 + ldv_sales(jgp,icl,ildv,iregn,n) 
		      else
			    num2 = num2 + ldv_sales(jgp,icl,ildv,iregn,n) * femhp(jgp,icl,yrs,ildv)
			    den2 = den2 + ldv_sales(jgp,icl,ildv,iregn,n) 
		      endif
		    endif
!          if (curcalyr.gt.2022) WRITE(21,'(a,",",5(i4,","),6(f12.1,","))')'AHP_check',curcalyr,curitr,iregn,jgp,icl,ldv_sales(jgp,icl,gas,iregn,n),femhp(jgp,icl,yrs,gas),num1,den1,num2,den2
	      enddo
        enddo
	  enddo
	  if(den1.ne.0.0) AHPCAR(iregn,n) = num1/den1	
	  if(den2.ne.0.0) AHPTRUCK(iregn,n) = num2/den2		
    enddo
	
!...calculate total ldv sales by size class and fueling technology (Table 52) 
	do icl=1,maxclass
      do ildv=1,maxldv
	    TOTALSALSC(1,icl,ildv,n) = sum(cafesales(1:cargrp,icl,yrs,ildv))
	    TOTALSALSC(2,icl,ildv,n) = sum(cafesales(ltkgrp:maxgroup,icl,yrs,ildv))		
	  enddo
    enddo
    
!...recalculate total expenditures on vehicle purchases
    DO IVTYP=1,MAXVTYP
      EXPENDVEH(IVTYP,N) = 0.0
      DO ICL=1,MAXCLASS
        DO ILDV=1,MAXLDV
		  EXPENDVEH(IVTYP,N) = EXPENDVEH(IVTYP,N) + (TOTALSALSC(IVTYP,ICL,ILDV,N) * LDV_PRI(IVTYP,ILDV,ICL,YRS))
        ENDDO
      ENDDO
    ENDDO

  RETURN
  END SUBROUTINE CAFECALC

! ==========================================================================================================
! ... Subroutine CAFETEST determines the least cost, alternatively fueled vehicle that could meet CAFE   
! ... standards by size class and incrementally increases sales of these vehicles to meet CAFE shortfall
! ==========================================================================================================
  SUBROUTINE CAFETEST
  USE T_
  IMPLICIT NONE

  INTEGER :: l,xldv,xcl,ycl,xpass,stop_gas,jgp			    !...local looping, array, and conditional variables			
  INTEGER :: salesflag(maxgroup,maxldv,maxclass,mnumyr)		!...indicates vehicle is available 
  INTEGER cafeveh(5) /5,6,7,15,16/					        !...ILDV fuel types available for CAFETEST compliance
  INTEGER carcls(8) /7,8,4,3,2,5,6,1/						!...passenger car size classes
  INTEGER trkcls(8) /7,8,2,5,6,4,3,1/						!...light-duty truck size classes 
  REAL :: delta_fuel__D_(maxgroup,maxldv,maxclass,mnumyr)	!...fuel savings by fuel type compared to conventional gasoline
  REAL :: delta_price(maxgroup,maxldv,maxclass,mnumyr)		!...incremental ILDV vehicle price compared to convetional gasoline
  REAL :: factor__D_(maxgroup,maxldv,maxclass,mnumyr)		!...fuel economics cost effectiveness (fuel savings-incremental vehicle cost)
  REAL :: asort(5)                                          !...sort by cost effectiveness
  REAL :: isort(5)                                          !...sort by order
  REAL :: xsort(5,maxgroup,maxclass,mnumyr)					!...saved sort order 
  REAL :: delta(maxgroup,maxclass)							!...maximum sales change per pass(loop)
  REAL :: apshr55_reg(maxgroup,maxclass,mnumcr,maxldv)		!...sales shares by region
  REAL :: CafeMpgGrp2(maxgroup,mnumyr)
  REAL :: EPAghgGrp2(maxgroup,mnumyr)
  REAL :: NUM1,DEN1,DEN2,DEN3,TEMPMPG
  REAL :: MAXADJ
  REAL :: temp_tot(maxgroup,maxclass),temp_tot2(maxgroup)
  LOGICAL*1 :: CafePass2(maxgroup)
  INTEGER, PARAMETER :: MAXVEH = 5							!...ILDV fuel types availble for CAFETEST compliance
  INTEGER, PARAMETER :: PAYB = 5							!...years of payback for fuel economics calculation
  INTEGER, PARAMETER :: MAXPASS = 30						!...number of loop passes for adding vehicle sales
!  REAL, PARAMETER    :: MAXADJ = 1.5                       !...Maximum number of vehicle sales that can be shifted b/w powertrains in a given year (across entire market), millions

    MAXADJ = 1
!   If IRA Section 30D still available, allow CAFETEST to force more BEVs/PHEVs/HEVs in during the 2 years following expiration of the credit
    if(LEGIRA.gt.0.and.CAFEMY27_SWITCH.eq.1.and.(curcalyr.ge.2033.and.curcalyr.le.2040)) MAXADJ = 1.5

!...set maximum allowable increase in alternatively fueled vehicle sales by manufacturer
!   Share out the total (MAXADJ) by the distribution of total gasoline sales across group and class (proxy for compliance shortfall)
!   Only calculated once per year. Units: million vehicles
	if (first_time_cafetest) then
      temp_tot(:,:) = 0.0
      delta(:,:) = 0.0
      do jgp=1,maxgroup
        do icl=1,maxclass
          if(sum(femmpg(jgp,icl,yrs,[4,5,6,7,15,16])).gt.0.0.and..not.cafepass(jgp)) then
            temp_tot(jgp,icl) = temp_tot(jgp,icl) + cafesales(jgp,icl,yrs,1)
          endif
	    enddo
      enddo
      
      do jgp=1,maxgroup
        if(cafepass(jgp)) CYCLE
        do icl=1,maxclass
          delta(jgp,icl) = 1./REAL(MAXPASS)*MAXADJ * temp_tot(jgp,icl)/sum(temp_tot(:,:))
        enddo
      enddo

!     If applying EPA GHG, distribute compliance burden across groups using actual compliance shortfall (negative Mgs); then
!     distribute across size classes using dist. of gasoline vehicle sales
      if (RUN_EPA.eq.1) then
        delta(:,:) = 0.0
        temp_tot2(:) = 0.0
        
        do jgp=1,maxgroup
          if(.not.cafepass(jgp)) then
            temp_tot2(jgp) = temp_tot2(jgp) + MgGhgGrp(jgp,n)
          endif
        enddo
!       
        do jgp=1,maxgroup
          if(cafepass(jgp)) CYCLE
          do icl=1,maxclass
            delta(jgp,icl) = 1./REAL(MAXPASS)*MAXADJ * temp_tot2(jgp)/sum(temp_tot2(:)) * temp_tot(jgp,icl)/sum(temp_tot(jgp,:))
          enddo
        enddo
      endif
      
      first_time_cafetest = .false.
    endif

!...save initial vehicle sales by manufacturer
	do icl=1,maxclass
	  do ildv=1,maxldv
		avsales_old(igp,icl,ildv,n) = cafesales(igp,icl,yrs,ildv)
!...    fill revised sales array for later calculations
		avsales_new(igp,icl,ildv,n)= cafesales(igp,icl,yrs,ildv)
	  enddo
	enddo

!...calculate cost effectiveness of advanced technology vehicles relative to conventional gasoline
!...calculate discounted fuel savings
	delta_fuel__D_=0.
	do l=1,maxveh
	  xldv=cafeveh(l)
	  do icl=1,maxclass
		salesflag(igp,xldv,icl,n) = 0
		factor__D_(igp,xldv,ICL,n)=-50000.
		if(femmpg(igp,icl,yrs,xldv).ne.0.) then
		  salesflag(igp,xldv,icl,n)=1
		  do i=1,payb
			if(igp.le.cargrp) VMT(i,mnumcr)=PVMT(i,STOCKYR-1989,mnumcr,1)
			if(igp.ge.ltkgrp) VMT(i,mnumcr)=LVMT(i,STOCKYR-1989,mnumcr,1)
			  fuel_n = n+i-1
			  if(fuel_n.gt.61) fuel_n=61
			  Fuel__D_(i) = FPRICE(1,mnumcr,YRS)
			  delta_fuel__D_(igp,xldv,icl,n) = delta_fuel__D_(igp,xldv,icl,n) + ((VMT(i,mnumcr)*(1/femmpg(igp,icl,yrs,gas) - &
												 1./femmpg(igp,icl,yrs,xldv))*Fuel__D_(i)) * ((1.07)**(-i)))															 

		  enddo
		endif
!...    calculate incremental vehicle price
		delta_price(igp,xldv,icl,n) = fempri(igp,icl,yrs,xldv)-fempri(igp,icl,yrs,gas)
!...    calculate fuel economics cost effectiveness factor
		factor__D_(igp,xldv,ICL,n) = delta_fuel__D_(igp,xldv,icl,n) - delta_price(igp,xldv,icl,n)

!       If MY2027+ CAFE/GHG, and IRA Section 30D, then allow the model to adopt more EVs in the years after Section 30D phaseout
!       (assume OEMs will price vehicles to prevent BEV factory capacity utilization from falling off a cliff)        
!        if (xldv.eq.15.or.xldv.eq.7.or.xldv.eq.5.or.xldv.eq.6.or.xldv.eq.16) then 
!          if (curcalyr.ge.2033.and.femmpg(igp,icl,yrs,xldv).ne.0..and.CAFEMY27_SWITCH.eq.1) then
!            delta_price(igp,xldv,icl,n) = MIN(delta_price(igp,xldv,icl,n),delta_price(igp,xldv,icl,2032-1989)*1.1)
!            factor__D_(igp,xldv,ICL,n) = delta_fuel__D_(igp,xldv,icl,n) - delta_price(igp,xldv,icl,n)
!          endif
!        endif

	  enddo
	enddo

!...sort factor_$ by most cost effective
	do icl=1,maxclass    
	  do l=1,maxveh
		xldv=cafeveh(l)
		asort(l) = -1.0 * factor__D_(igp,xldv,icl,n)
		isort(l)=cafeveh(l)
	  enddo
	  call RSORT(asort,isort,maxveh,maxveh)
	  do l=1,maxveh
		xsort(l,igp,icl,n)= isort(l)
!        if(curcalyr.gt.2040) xsort(l,igp,icl,n) = xsort(l,igp,icl,n-1)
	  enddo
	enddo

!...increase sales of alternative fuel vehicles in order of cost effectiveness to meet CAFE standard
    CafeMpgGrp2(igp,n) = 0.0
    EPAghgGrp2(igp,n)   = 0.0
	outer: do xcl=1,maxclass
	  if(cafepass(IGP)) exit	  
	  if(igp.le.cargrp) ycl=carcls(xcl)
	  if(igp.ge.ltkgrp) ycl=trkcls(xcl)
	  if(avsales_old(igp,ycl,gas,n).gt.0.) then
		inner: do l=1,maxveh
		  if(CafePass2(igp)) exit
		  xldv=xsort(l,igp,ycl,n)
!...      nsure alternative fuel vehicle and class sales exist for manufacturer and class
		  if(salesflag(igp,xldv,ycl,n).eq.1) then
!...        add alternative fuel vehicle sales until CAFE is met
			stop_gas=0
!...        incrementally add alternative fuel vehicle sales to meet CAFE
			do xpass=1,maxpass
!...          subtract delta alternative vehicle sales from initial gasoline vehicle sales
!...          add delta alternative vehicle sales to inital advanced vehicle sales                
			  if(stop_gas.eq.0)then
				avsales_new(igp,ycl,1,n) = avsales_new(igp,ycl,1,n) - delta(igp,ycl)
				avsales_new(igp,ycl,xldv,n) = avsales_new(igp,ycl,xldv,n) + delta(igp,ycl)
!                if(fcrl.eq.1) WRITE(21,'(a,",",5(i4,","),11(f12.1,","))')'cafetest_check',curcalyr,xcl,xldv,xpass,stop_gas,avsales_new(igp,ycl,1,n)*1000000,&
!                                                    avsales_new(igp,ycl,xldv,n)*1000000,delta(igp,ycl)*1000000,CafeMpgGrp2(igp,n),EPAghgGrp2(igp,n),&
!                                                    cafe_used(igp,yrs),FPghgGrp(igp,n),MgGhgGrp(igp,n),sum(MgGhgGrp(1:cargrp,n)),sum(MgGhgGrp(LTKGRP:maxgroup,n)),sum(MgGhgGrp(:,n))
!...            stop if conventional gasoline vehicle sales go to zero or negative                
				if(avsales_new(igp,ycl,1,n).le.0.)then
				  if(stop_gas.eq.0)then
					avsales_new(igp,ycl,1,n) = avsales_new(igp,ycl,1,n) + delta(igp,ycl)
					avsales_new(igp,ycl,xldv,n) = avsales_new(igp,ycl,xldv,n) - delta(igp,ycl)
				  endif
				  stop_gas=1
				endif
			  endif
              
!...		  For each xpass calculate new fleet average mpg and g/mi.  Exit if standard(s) met.
!             First, fill global sales variable with latest adjusted sales 
              do ildv=1,maxldv
                do icl=1,maxclass
                  cafesales(igp,icl,yrs,ildv) = avsales_new(igp,ICL,ILDV,n)
                enddo
              enddo
!             Second, calculate the new group average mpg, g/mi, and MgGHG
			  NUM1 = 0.0
              DEN1 = 0.0
			  DEN2 = 0.0
              DEN3 = 0.0
			  do ildv=1,maxldv 
				do icl=1,maxclass
				  if (femmpg(igp,icl,yrs,ildv).gt.0.0) then
                    if (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then
                      if (CAFEMY27_SWITCH.eq.1.and.curcalyr.ge.2027) then
                        TEMPMPG = femmpg(igp,icl,yrs,ildv)*cafepefmult(ildv,n)
                      else
                        TEMPMPG = (1/(femmpg(igp,icl,yrs,ildv)*cafepefmult(ildv,n)) - ac_oc_credit(igp,n))**-1
                      endif
                    else
                      TEMPMPG = (1/(femmpg(igp,icl,yrs,ildv)*cafepefmult(ildv,n)) - ac_oc_credit(igp,n))**-1
                    endif
                    DEN1 = DEN1 + avsales_new(igp,icl,ildv,n)/TEMPMPG


		      	 	if (CAFEMY27_SWITCH.eq.1.and.curcalyr.ge.2027.and.(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15)) then         ! Zero g/mi off-cycle and AC efficiency for BEVs in MY2027+
                      CYCLE
                    elseif (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then
                      NUM1 = NUM1 + avsales_new(igp,icl,ildv,n) * (-ac_oc_credit(igp,n)*8887)
                    elseif (ildv.eq.5.or.ildv.eq.6) then
                      NUM1 = NUM1 + avsales_new(igp,icl,ildv,n) * (1-phev_evmt(igp,icl,yrs,ildv)) * &
                               (1/PHEVMPG_S(igp,icl,yrs,ildv)*8887 - ac_oc_credit(igp,n)*8887)
                    else
                      NUM1 = NUM1 + avsales_new(igp,icl,ildv,n) * (1/femmpg(igp,icl,yrs,ildv)*8887 - ac_oc_credit(igp,n)*8887)
                    endif
                  endif
		        enddo
!               Calculate extra zero-emission sales to throw in the denominator (Advanced Technology Multipliers)
                if (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.14) then 
                  DEN3 = DEN3 + sum(avsales_new(igp,1:maxclass,ildv,n)) * (EPAALTMULT(ildv,n) - 1)
                endif
              enddo

			  CafeMpgGrp2(igp,n) = sum(avsales_new(igp,1:maxclass,1:maxldv,n))/DEN1
              EPAghgGrp2(igp,n) = NUM1/(sum(avsales_new(igp,1:maxclass,1:maxldv,n)) +  DEN3) - AC_CO2_OFFSET(igp,n)

              if (igp.le.cargrp) then
                MgGhgGrp(igp,n) = (FPghgGrp(igp,n) - EPAghgGrp2(igp,n))*sum(avsales_new(igp,1:maxclass,1:maxldv,n)) * 195264
              else
                MgGhgGrp(igp,n) = (FPghgGrp(igp,n) - EPAghgGrp2(igp,n))*sum(avsales_new(igp,1:maxclass,1:maxldv,n)) * 225865
              endif

!             Third and finally, calculate total fleet average mpg (car, truck, and total) for CAFE
              CafeMpgGrp(igp,n) = CafeMpgGrp2(igp,n)
              if(curcalyr.gt.epalyr) then 
                NewMPG(1,n) = 0.0 
                NewMPG(2,n) = 0.0
                do jgp=1,cargrp
                  if(cafempggrp(jgp,n).ne.0) NewMPG(1,n) = NewMPG(1,n) + sum(cafesales(jgp,:,yrs,:)) /cafempggrp(jgp,n)
                enddo
                do jgp=ltkgrp,maxgroup
                    if(cafempggrp(jgp,n).ne.0) NewMPG(2,n) = NewMPG(2,n) + sum(cafesales(jgp,:,yrs,:)) /cafempggrp(jgp,n)
                enddo
                NewMPG(1,n) = sum(cafesales(1:cargrp,:,yrs,:))/NewMPG(1,n)
                NewMPG(2,n) = sum(cafesales(ltkgrp:maxgroup,:,yrs,:))/NewMPG(2,n)
                NewMPG(3,n) = sum(cafesales(:,:,yrs,:))/((sum(cafesales(1:cargrp,:,yrs,:))/NewMPG(1,n))+sum(cafesales(ltkgrp:maxgroup,:,yrs,:))/NewMPG(2,n))
              endif
                            
!...		  is standard met?
!             Does the current group meet CAFE . . OR . . does the whole market meet CAFE 
			  if(CafeMpgGrp2(igp,n).ge.cafe_used(igp,yrs).or. &                                                                         ! One group meets CAFE
                 (NewMPG(3,n).gt.cafestd(3,n).and.((NewMPG(1,n)-cafestd(1,n)).gt.-2.0).and.((NewMPG(2,n)-cafestd(2,n)).gt.-2.0))) then  ! Whole market meets CAFE
!               If not enforcing EPA tailpipe GHG reg, meeting CAFE means this group is good to go
!               Note that if the whole market meets, CAFETEST will catch that later on and stop stepping through other mfr groups
                if (RUN_EPA.eq.0) then
                  cafepass(IGP) = .true.
!                  CafeMpgGrp(igp,n) = CafeMpgGrp2(igp,n)
                  EPAghgGrp(igp,n) = EPAghgGrp2(igp,n)
                  exit outer
!               If enforcing EPA tailpipe GHG reg, does the whole market meet?
                elseif(MgGhgGrp(igp,n).gt.0.0.or.sum(MgGhgGrp(:,n)).gt.0.0) then   ! Whole market meets EPA GHG
                  if(fcrl.eq.1) WRITE(21,'(a,",",5(i4,","),a)')'cafetest_check',curcalyr,xcl,xldv,xpass,igp,'PASSED CAFE/GHG ONE GROUP'
                  cafepass(IGP) = .true.
!                  CafeMpgGrp(igp,n) = CafeMpgGrp2(igp,n)
                  EPAghgGrp(igp,n) = EPAghgGrp2(igp,n)
                  exit outer
!               If the whole market does not meet EPA
                else
!                 (if current group is a car group) does all of car? If so, done with this group and set all car groups to PASS
                  if (GrpMap(igp).eq.1.and.sum(MgGhgGrp(1:CARGRP,n)).ge.0.0) then   ! Car meets EPA GHG
                    if(fcrl.eq.1) WRITE(21,'(a,",",5(i4,","),a)')'cafetest_check',curcalyr,xcl,xldv,xpass,igp,'PASSED CAFE/GHG ALL CAR'
                    cafepass(1:CARGRP) = .true.
!                    CafeMpgGrp(igp,n) = CafeMpgGrp2(igp,n)
                    EPAghgGrp(igp,n) = EPAghgGrp2(igp,n)
                    exit outer
!                 (if current group is a light truck group) does all of light truck? If so, done with this group and set all light truck groups to PASS
                  elseif (GrpMap(igp).eq.2.and.sum(MgGhgGrp(ltkgrp:maxgroup,n)).ge.0.0) then    ! Truck meets EPA GHG
                    if(fcrl.eq.1) WRITE(21,'(a,",",5(i4,","),a)')'cafetest_check',curcalyr,xcl,xldv,xpass,igp,'PASSED CAFE/GHG ALL TRUCK'
                    cafepass(ltkgrp:maxgroup) = .true.
!                    CafeMpgGrp(igp,n) = CafeMpgGrp2(igp,n)
                    EPAghgGrp(igp,n) = EPAghgGrp2(igp,n)
                    exit outer
                  endif
                endif                
              endif
!...          if no gasoline to displace then exit and jump to the next size class
			  if(stop_gas.eq.1) exit inner
			enddo
		  endif
	    enddo inner
	  endif
	enddo outer
      
    
!...calculate new sales totals for light duty vehicles
!   First national
    do ildv=1,maxldv
      do icl=1,maxclass
        cafesales(igp,icl,yrs,ildv) = avsales_new(igp,ICL,ILDV,n)
      enddo
    enddo

!!   Then regionalize
    apshr55_reg = 0.
    do iregn=1,mnumcr-2
      do ildv=1,maxldv
        do icl=1,maxclass
          if(sum(ldv_sales(igp,icl,ildv,1:mnumcr-2,n)).ne.0.0) then
		    apshr55_reg(igp,icl,iregn,ILDV)=ldv_sales(igp,icl,ildv,iregn,n)/sum(ldv_sales(igp,icl,ildv,1:mnumcr-2,n))
          endif
        enddo
      enddo
    enddo    
    do iregn=1,mnumcr-2
      do ildv=1,maxldv
        do icl=1,maxclass
          ldv_sales(igp,icl,ildv,iregn,n) = cafesales(igp,icl,yrs,ildv) * apshr55_reg(igp,icl,iregn,ILDV)
          if (iregn.eq.mnumcr-2) ldv_sales(igp,icl,ildv,mnumcr,n) = sum(ldv_sales(igp,icl,ildv,1:mnumcr-2,n))
        enddo
      enddo
    enddo

!   From TALT2X -- redistribute sales to households and fleets (same share as used pre-CAFETEST)
!...  fill household/fleet vehicle sales by mfr group 
!	  do igp=1,maxgroup 
		do icl=1,maxclass 
		  do ildv=1,maxldv 
			do iregn=1,mnumcr-2
			  hhgrpsal(iregn,igp,icl,ildv,n) = ldv_sales(igp,icl,ildv,iregn,n) * ownsaletemp(1,igp,icl,iregn,n)
			  do ifleet = 1, maxfleet
                fltgrpsal(iregn,ifleet,igp,icl,ildv) = ldv_sales(igp,icl,ildv,iregn,n) * ownsaletemp(ifleet+1,igp,icl,iregn,n)*1000000.0
              enddo
            enddo 
!...	    US sales 
            hhgrpsal(mnumcr,igp,icl,ildv,n) = sum(hhgrpsal(1:mnumcr-2,igp,icl,ildv,n))
            do ifleet = 1,maxfleet 
              fltgrpsal(mnumcr,ifleet,igp,icl,ildv) = sum(fltgrpsal(1:mnumcr-2,ifleet,igp,icl,ildv))
            enddo
          enddo 
		enddo 
!	  enddo
	  
!...  fill household/fleet sales by vehicle type	  
	  do icl=1,maxclass 
	    do ildv=1,maxldv 
		  do iregn=1,mnumcr-2 
		    hhtechsal(iregn,1,icl,ildv,n) = sum(hhgrpsal(iregn,1:cargrp,icl,ildv,n))
		    hhtechsal(iregn,2,icl,ildv,n) = sum(hhgrpsal(iregn,ltkgrp:maxgroup,icl,ildv,n))			
		    do ifleet = 1, maxfleet
              fltechsal(iregn,1,ifleet,icl,ildv,1) = sum(fltgrpsal(iregn,ifleet,1:cargrp,icl,ildv))
		      fltechsal(iregn,2,ifleet,icl,ildv,1) = sum(fltgrpsal(iregn,ifleet,ltkgrp:maxgroup,icl,ildv))	
            enddo
          enddo 
		  do ivtyp=1,maxvtyp 
		    hhtechsal(mnumcr,ivtyp,icl,ildv,n) = sum(hhtechsal(1:mnumcr-2,ivtyp,icl,ildv,n))
            do ifleet = 1,maxfleet
              fltechSal(mnumcr,ivtyp,ifleet,icl,ildv,1) = sum(FltechSal(1:mnumcr-2,ivtyp,ifleet,icl,ildv,1))
		    enddo
          enddo 
		enddo 
	  enddo

!...  calculate national sales shares
	  do icl=1,maxclass 
	    do ildv=1,maxldv 	  
	  	  APShrGrp(igp,icl,mnumcr,ildv,n) = ldv_sales(igp,icl,ildv,mnumcr,n)/sum(ldv_Sales(igp,icl,1:maxldv,mnumcr,n))
	    enddo
	  enddo 

    CALL CAFECALC(1)

!   If, after re-estimating all of the compliance metrics, the entire market meets CAFE/GHG, set all groups to pass to prevent further CAFETEST runs
    if (NewMPG(3,n).gt.cafestd(3,n).and.((NewMPG(1,n)-cafestd(1,n)).gt.-2.0).and.((NewMPG(2,n)-cafestd(2,n)).gt.-2.0)) then
      if (RUN_EPA.eq.1) then
        if (sum(MgGhgGrp(:,n)).gt.0.0) then
          cafepass(:) = .true.
          WRITE(21,'(a,",",2(i4,","),a)')'cafetest_check',curcalyr,igp,'PASSED CAFE/GHG -- WHOLE MARKET'
        endif
      else
        cafepass(:) = .true.
      endif
    endif

!    if(curcalyr.gt.2022.and.fcrl.eq.1) WRITE(21,'(a14,",",2(i4,","),l12,",",8(f12.1,","))')'cafepass_nhtsa',curcalyr,igp,cafepass(igp),CafeMpgGrp(igp,n),Cafe_Used(igp,yrs), &
!                                                                                  NewMPG(1,n),cafestd(1,n),NewMPG(2,n),cafestd(2,n),NewMPG(3,n),cafestd(3,n)
!    if(curcalyr.gt.2022.and.fcrl.eq.1) WRITE(21,'(a14,",",2(i4,","),l12,",",8(f12.1,","))')'cafepass_epa',curcalyr,igp,cafepass(igp),MgGhgGrp(igp,n),0.0, &
!                                                                                  sum(MgGhgGrp(1:cargrp,n)),0.0,sum(MgGhgGrp(6:11,n)),0.0,sum(MgGhgGrp(1:11,n)),0.0


  RETURN
  END SUBROUTINE CAFETEST

! ==========================================================================================================
!...Subroutine RSORT sorts first Q elements of real array A.  It also sorts an indexing arry, IDX, which
!...can by used to map the sort order to other arrays.  IDX should be passed with the series 1, 2,...Q.  
!...The sort methodology is called quicksort and is considered an efficieny approach for sorting large
!...arrays.
! ==========================================================================================================

  SUBROUTINE RSORT(A,IDX,Q,QMAX)
  USE T_
  IMPLICIT NONE

    INTEGER*4 QMAX,Q
    real*4    A(QMAX),X,W
    INTEGER*4 IDX(QMAX)
    INTEGER*4 STACK(16,2),L,R,S,II,ITEMP
    S=1
    STACK(1,1)=1
    STACK(1,2)=Q
10  CONTINUE
    L=STACK(S,1)
    R=STACK(S,2)
    S=S-1
20  CONTINUE
      I=L
      J=R
      II=(L+R)/2
      X=A(II)
30    CONTINUE
40      CONTINUE
          IF(A(I).LT.X) THEN
             I=I+1
             GO TO 40
          ENDIF
50      CONTINUE
          IF(X.LT.A(J)) THEN
            J=J-1
            GO TO 50
          ENDIF
        IF(I.LE.J) THEN
          W=A(I)
          A(I)=A(J)
          A(J)=W
          ITEMP=IDX(I)
          IDX(I)=IDX(J)
          IDX(J)=ITEMP
          I=I+1
          J=J-1
        ENDIF
        IF(I.LE.J) GO TO 30
        IF((J-L).LT.(R-I)) THEN
          IF(I.LT.R) THEN
            S=S+1
            STACK(S,1)=I
            STACK(S,2)=R
          ENDIF
          R=J
        ELSE
          IF(L.LT.J) THEN
            S=S+1
            STACK(S,1)=L
            STACK(S,2)=J
          ENDIF
          L=I
        ENDIF
        IF(L.LT.R) GO TO 20
      IF(S.GT.0) GO TO 10
    RETURN
    END

! ==========================================================================================================
!...Subroutine TSMOD calculates light vehicle stocks by technology type 
! ==========================================================================================================
    SUBROUTINE TSMOD
    USE T_
    IMPLICIT NONE
	
	REAL age_wgt(mnumyr,maxvtyp,mnumcr)		! For writing out average age of fleet
	REAL avg_age(mnumyr,maxvtyp,mnumcr)		! For writing out average age of fleet
	
!-----------------------------------------------------------------------------------------------------------------------------------
!...This section of code is implemented to populate reporting tables with historical data for light duty vehicles.  The 
!...1995-2020 vehicle stock values come from - Polk w/adjustments for vehicle stocks assigned to fleet.
!...The stock variables are disaggregated by fuel type, census division, and by fleet

!...if low macro case adjust survival curve to account for declining sales 
    if(MMAC.eq.4.and.curcalyr.gt.STOCKYR+1.and.curitr.eq.1)then
	  if(curcalyr.le.STOCKYR+2)then
	    do iregn=1,mnumcr
		  do IVTYP=1,maxvtyp
		    do iage=4,23
              SSURV25(iregn,iage,IVTYP) = SSURV25(iregn,iage,IVTYP)*1.002
			  IF(curcalyr.eq.2023) SSURV25(iregn,iage,IVTYP) = SSURV25(iregn,iage,IVTYP)*1.003
			enddo
		  enddo
		enddo
	  endif
	endif

!...fill household vehicle sales 2019-stockyr 
	if(curcalyr.ge.2019.and.curcalyr.le.stockyr) then
      do iregn=1,mnumcr-2
		do icl=1,maxclass 
		  do ildv=1,maxldv 
			do igp=1,maxgroup 
			  hhgrpsal(iregn,igp,icl,ildv,n) = own_sales(1,igp,icl,ildv,iregn,yrs)/1000000.0
			enddo 
			hhtechsal(iregn,1,icl,ildv,n) = sum(hhgrpsal(iregn,1:cargrp,icl,ildv,n))
			hhtechsal(iregn,2,icl,ildv,n) = sum(hhgrpsal(iregn,ltkgrp:maxgroup,icl,ildv,n))
		  enddo 
		enddo 
	  enddo 
	  do icl=1,maxclass
	    do ildv=1,maxldv
		  do ivtyp=1,maxvtyp 
		    hhtechsal(mnumcr,ivtyp,icl,ildv,n) = sum(hhtechsal(1:mnumcr-2,ivtyp,icl,ildv,n))
		  enddo 
		  do igp=1,maxgroup
		    hhgrpsal(mnumcr,igp,icl,ildv,n) = sum(hhgrpsal(1:mnumcr-2,igp,icl,ildv,n)) 
		  enddo
		enddo 
	  enddo
	endif
	
!...sum active/retired fleet for transfer/subtraction to/from non-fleet - HAVs do not transfer
    OLDFSTKT=0.0
	if(curcalyr.gt.stockyr)then
	  do iregn=1,mnumcr-2
        do ivtyp=1,maxvtyp
          do ildv=1,maxldv
		    if(ildv.lt.9.and.ildv.gt.12) then
              do iage=1,maxage
                oldfstkt(iregn,IVTYP,ILDV,iage)=sum(oldfstk(iregn,IVTYP,1:maxfleet,ILDV,iage))/1000000.0
              enddo
		    endif
          enddo
        enddo
	  enddo
!... calculate ldv stock post STOCKYR - by region
	  do iregn=1,mnumcr-2
		do ivtyp=1,maxvtyp
          do ildv=1,maxldv
			LDV_STOCK(iregn,ivtyp,1,ildv,1,1,n) = sum(hhtechsal(iregn,ivtyp,1:maxclass,ildv,n)) 
            do iage=2,maxage-1
		      LDV_STOCK(iregn,ivtyp,1,ildv,iage,1,n) = LDV_STOCK(iregn,ivtyp,1,ildv,iage-1,1,n-1)*SSURV25(iregn,iage-1,ivtyp)	  
            enddo
		    LDV_STOCK(iregn,ivtyp,1,ildv,maxage,1,n) = LDV_STOCK(iregn,ivtyp,1,ildv,maxage-1,1,n-1)*SSURV25(iregn,maxage-1,ivtyp) + &
			                                           LDV_STOCK(iregn,ivtyp,1,ildv,maxage,1,n-1)*SSURV25(iregn,maxage,ivtyp) 
!...    	transfer retired fleet stock to non-fleet stock (taxis are not transfered to HH stock) 
		    do iage=1,maxage
			  LDV_STOCK(iregn,ivtyp,1,ildv,iage,1,n) = LDV_STOCK(iregn,ivtyp,1,ildv,iage,1,n) + OLDFSTKT(iregn,ivtyp,ildv,iage)
			enddo
          enddo
	    enddo
      enddo
!...  sum across regions to determine national stock quantities
	  do ivtyp = 1,maxvtyp
		do iown = 1,maxowner
          do ildv=1,maxldv
			do iage = 1,maxage
			  LDV_STOCK(mnumcr,ivtyp,iown,ildv,iage,1,n) = sum(LDV_STOCK(1:mnumcr-2,ivtyp,iown,ildv,iage,1,n))
		    enddo
		  enddo	  
        enddo
	  enddo
    endif ! year > stockyr

!	Write out average vehicle age by size class and region
!	IF(N.eq.MNUMYR.and.FCRL.eq.1) THEN
!	  avg_age(:,:,:) = 0.0
!	  age_wgt(:,:,:) = 0.0
!	  do iown = 22, mnumyr ! (2010-2050)		! Commandeer iown for the write statement
!	    do iregn = 1, mnumcr
!		 if (iregn.eq.10) CYCLE
!		  do IVTYP = 1, maxvtyp
!		    do iage = 1, maxage
!		      age_wgt(iown,ivtyp,iregn) = age_wgt(iown,ivtyp,iregn) + iage * sum(LDV_STOCK(iregn,IVTYP,:,:,iage,:,iown))
!		    enddo
!		    avg_age(iown,ivtyp,iregn) = age_wgt(iown,ivtyp,iregn) / sum(LDV_STOCK(iregn,IVTYP,:,:,:,:,iown))
!		  enddo
!		enddo
!	  enddo
!
!	  WRITE(21,*)'Average LDV age'
!	  WRITE(21,*)'year,ivtyp,cd1,cd2,cd3,cd4,cd5,cd6,cd7,cd8,cd9,national'
!	  do iown = 24, mnumyr ! (2012-2050)
!	    do ivtyp = 1, maxvtyp
!	      
!	      WRITE(21,'(I4,",",I2,10(",",F10.2))') iown+1989, ivtyp, avg_age(iown,ivtyp,1),avg_age(iown,ivtyp,2),&
!	      										avg_age(iown,ivtyp,3),avg_age(iown,ivtyp,4),avg_age(iown,ivtyp,5),&
!	      										avg_age(iown,ivtyp,6),avg_age(iown,ivtyp,7),avg_age(iown,ivtyp,8),&
!	      										avg_age(iown,ivtyp,9),avg_age(iown,ivtyp,11)
!		enddo
!	  enddo
!	endif

!-----------------------------------------------------------------------------------------------------------------------------------
!...Calculate total ldv vehicle stock by fuel types (for table 49)
    do ildv=1,maxldv
	  do ivtyp=1,maxvtyp
	   VSTK(ivtyp,ildv) = sum(LDV_STOCK(mnumcr,ivtyp,1:maxowner,ildv,1:maxage,1:maxhav,n)) 
	  enddo
    enddo
    
!...calculate total non-fleet stocks
    STKCAR(n) = sum(LDV_STOCK(mnumcr,1,1,1:maxldv,1:maxage,1,n)) 
    STKTR(n)  = sum(LDV_STOCK(mnumcr,2,1,1:maxldv,1:maxage,1,n)) 

!...calculate light duty vehicles per licensed driver
    VPLD(n) = sum(vstk(1:maxvtyp,1:maxldv))/SUM(LicDriver(1:AGEGRP,1:MF,1:MNUMCR-2,n)) 

!...calculate total LDV stocks to determine alt fuel availability
    do ildv=1,maxldv
      LDVSTK(ildv,n) = sum(vstk(1:maxvtyp,ildv)) !..jma check this
    enddo
  RETURN
  END SUBROUTINE TSMOD

! ==========================================================================================================
! ... Subroutine TMPGSTK calculates household light vehicle stock mpg by technology
! ==========================================================================================================
  SUBROUTINE TMPGSTK
  USE T_
  IMPLICIT NONE

    REAL     CMPGT(MNUMYR,mnumcr), TMPGT(MNUMYR,mnumcr), NUM1,DEN1,NUM2,DEN2


!...Calculate stock mpg for household cars and lt. trucks                          
!...populate hhmpgstk for older vintages (1995 and previous model years)
    if(curcalyr.eq.first_read_year) then 
	  do iregn=1,mnumcr
	    if(iregn.ne.10) then
		  do ivtyp=1,maxvtyp
		    do ildv=1,maxldv
			  do iage=2,maxage 
			    if(ildv.eq.1) hhmpgstk(iregn,ivtyp,ildv,iage,n) = cmpgstkgas95(ivtyp,iage)*degfac(ivtyp,ildv,n)
				if(ildv.eq.2) hhmpgstk(iregn,ivtyp,ildv,iage,n) = cmpgstkgas95(ivtyp,iage)*degfac(ivtyp,ildv,n)*afvadjfe(ildv,ivtyp)
				if(ildv.eq.3) hhmpgstk(iregn,ivtyp,ildv,iage,n) = cmpgstkgas95(ivtyp,iage)*degfac(ivtyp,ildv,n)
				if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) hhmpgstk(iregn,ivtyp,ildv,iage,n) = cmpgstkgas95(ivtyp,iage)*degfac(ivtyp,ildv,n)*4.0				
				if(ildv.ge.9.and.ildv.le.12) hhmpgstk(iregn,ivtyp,ildv,iage,n) = cmpgstkgas95(ivtyp,iage)*degfac(ivtyp,ildv,n)
			  enddo
		    enddo 
		  enddo
		endif
	  enddo 
	endif

!...calculate new household fuel economy by region, vtyp, and ldv <= 2018
	if(curcalyr.le.2018) then
      do iregn=1,mnumcr
	    if(iregn.ne.10) then
		  do ivtyp=1,maxvtyp 	    
			do ildv=1,maxldv
			  hhmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,ildv,n)
!...          assign mpg if no new vehicle in FEM data (conversion/demonstration/small mfr) 			  
	          if(hhmpgnew(iregn,ivtyp,ildv,n).eq.0.0) then
				if(ildv.eq.2) hhmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n) * afvadjfe(ildv,ivtyp)
				if(ildv.eq.3) hhmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n)
				if(ildv.ge.9.and.ildv.le.12) hhmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n)
				if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then
				  if(ivtyp.eq.1) hhmpgnew(iregn,ivtyp,ildv,n) = 105.0
				  if(ivtyp.eq.2) hhmpgnew(iregn,ivtyp,ildv,n) = 85.0	
			    endif			
			  endif
		    enddo 
		  enddo
		endif
	  enddo 
	endif !<=2018

!...household mpg regional detail  
	if(curcalyr.ge.2019) then
!...  ensure all new household ldvs have a mpg in case older ldvs appear in region in later years
	  do iregn=1,mnumcr 
	    if(iregn.ne.10) then
		  do ivtyp=1,maxvtyp 
		    do ildv=1,maxldv
			  hhmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,ildv,n)
			  if(iregn.le.9) then
			    if(ldvmpgnew(iregn,ivtyp,ildv,n).ne.0.0) hhmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(iregn,ivtyp,ildv,n) 
			  endif
		    enddo 
		  enddo
		endif
	  enddo
!...  calculate region specific household fuel economy
	  do iregn=1,mnumcr-2
	    if(iregn.ne.10) then
		  do ildv=1,maxldv 		
			DEN1 = 0.0
			DEN2 = 0.0
			do icl=1,maxclass
	          do igp=1,maxgroup	
!...			calculate regional mpg if ildv present			  
				if(hhgrpsal(iregn,igp,icl,ildv,n).ne.0.0.and.femmpg(igp,icl,yrs,ildv).ne.0.0) then
				  if(igp.le.cargrp) then
					DEN1 = DEN1 + hhgrpsal(iregn,igp,icl,ildv,n)/femmpg(igp,icl,yrs,ildv)
				  else
					DEN2 = DEN2 + hhgrpsal(iregn,igp,icl,ildv,n)/femmpg(igp,icl,yrs,ildv)	
				  endif 
			    endif
			  enddo 
		    enddo 
		    if(DEN1.ne.0.0) hhmpgnew(iregn,1,ildv,n) = sum(hhgrpsal(iregn,1:cargrp,1:maxclass,ildv,n))/DEN1
			if(DEN2.ne.0.0) hhmpgnew(iregn,2,ildv,n) = sum(hhgrpsal(iregn,ltkgrp:maxgroup,1:maxclass,ildv,n))/DEN2
		  enddo
		endif
	  enddo 
!...  calculate US household mpg
	  do ivtyp=1,maxvtyp 
		do ildv=1,maxldv
		  DEN1 = 0.0
		  do iregn=1,mnumcr-2
		    if(sum(hhtechsal(iregn,ivtyp,1:maxclass,ildv,n)).ne.0.0.and.hhmpgnew(iregn,ivtyp,ildv,n).ne.0.0) then
			  DEN1 = sum(hhtechsal(iregn,ivtyp,1:maxclass,ildv,n))/hhmpgnew(iregn,ivtyp,ildv,n) 
			endif
		  enddo
		  if(DEN1.ne.0.0) hhmpgnew(mnumcr,ivtyp,ildv,n) = sum(hhtechsal(1:mnumcr-2,ivtyp,1:maxclass,ildv,n))/DEN1
		enddo 
	  enddo 
	endif ! years >= 2019

!...Advance vintage array of household stock mpg 1 year and account for new additions
!...fill vinage 1 hhmpgstk 
	do iregn=1,mnumcr 
	  do ivtyp=1,maxvtyp 
	    do ildv=1,maxldv
		  hhmpgstk(iregn,ivtyp,ildv,1,n) = hhmpgnew(iregn,ivtyp,ildv,n) * degfac(ivtyp,ildv,n)
		enddo 
	  enddo 
	enddo
!...fill current vintage mpg with previous year mpg
	if(curcalyr.gt.first_read_year) then 
	  do iregn=1,mnumcr 
	    do ivtyp=1,maxvtyp 
		  do ildv=1,maxldv
			do iage=2,maxage
			  hhmpgstk(iregn,ivtyp,ildv,iage,n) = hhmpgstk(iregn,ivtyp,ildv,iage-1,n-1)
			enddo
!...        fill maxage mpg
			num1 = 0.0
			den1 = 0.0
			if(ldv_stock(iregn,ivtyp,1,ildv,maxage-1,1,n-1).ne.0.0.and.hhmpgstk(iregn,ivtyp,ildv,maxage-1,n-1).ne.0.0) then 
			  num1 = ldv_stock(iregn,ivtyp,1,ildv,maxage-1,1,n-1)
			  den1 = ldv_stock(iregn,ivtyp,1,ildv,maxage-1,1,n-1)/hhmpgstk(iregn,ivtyp,ildv,maxage-1,n-1)
			elseif(ldv_stock(iregn,ivtyp,1,ildv,maxage,1,n-1).ne.0.0.and.hhmpgstk(iregn,ivtyp,ildv,maxage,n-1).ne.0.0) then 
			  num1 = num1 + ldv_stock(iregn,ivtyp,1,ildv,maxage,1,n-1)
			  den1 = den1 + ldv_stock(iregn,ivtyp,1,ildv,maxage,1,n-1)/hhmpgstk(iregn,ivtyp,ildv,maxage,n-1)
			endif
			if(den1.ne.0.0) hhmpgstk(iregn,ivtyp,ildv,maxage,n) = num1/den1
		  enddo 
		enddo 
	  enddo	
	endif ! > first_read_year

! mpg debug jma
    if(curcalyr.le.2026) then
	  do iregn=1,mnumcr
	    if(iregn.ne.10) then
		do ivtyp=1,maxvtyp
		  do ildv=1,maxldv
			do iage=1,maxage
              if(ldv_stock(iregn,ivtyp,1,ildv,iage,1,n).ne.0.0.and.hhmpgstk(iregn,ivtyp,ildv,iage,n).eq.0.0) then 
			    write(21,*)'household regional mpg error, iregn',iregn,curcalyr
			    write(21,*)'ivtyp',ivtyp,'ildv',ildv,'iage',iage
				write(21,*)'hhmpgstk/ldvmpgnew',hhmpgstk(iregn,ivtyp,ildv,iage,n),ldvmpgnew(mnumcr,ivtyp,ildv,n-iage+1)
                write(21,*)'ldv_stock',ldv_stock(iregn,ivtyp,1,ildv,iage,1,n)
			  endif
		    enddo
		  enddo
		enddo 
		endif
	  enddo
	endif
    	
!...household travel (million miles)	
	VMT_STK_HH(:,:,:,:,:) = 0.0
    do ILDV=1,maxldv
      do iage=1,maxage
		do iregn=1,mnumcr-2
		  VMT_STK_HH(1,ildv,iage,1,iregn) = LDV_STOCK(iregn,1,1,ildv,iage,1,n)*PVMT(iage,n,iregn,ildv)
		  VMT_STK_HH(2,ildv,iage,1,iregn) = LDV_STOCK(iregn,2,1,ildv,iage,1,n)*LVMT(iage,n,iregn,ildv)
		enddo
		do IVTYP = 1,maxvtyp
		  VMT_STK_HH(ivtyp,ildv,iage,1,mnumcr)=sum(VMT_STK_HH(ivtyp,ildv,iage,1,1:mnumcr-2))
		enddo	  
      enddo
    enddo

	do iregn=1,mnumcr
      CMPGT(n,iregn) = 0.0	!car hh consumption (denominator for FE)
      TMPGT(n,iregn) = 0.0	!LT hh consumption (denominator for FE)
	enddo

    do iregn=1,mnumcr-2
      do ildv=1,maxldv
	    do iage=1,maxage  
		  if(hhmpgstk(iregn,1,ildv,iage,n).ne.0.0) CMPGT(n,iregn) = CMPGT(n,iregn)+(VMT_STK_HH(1,ildv,iage,1,iregn) /  &
																				   (hhmpgstk(iregn,1,ildv,iage,n)))
		  if(hhmpgstk(iregn,2,ildv,iage,n).ne.0.0) TMPGT(n,iregn) = TMPGT(n,iregn)+(VMT_STK_HH(2,ildv,iage,1,iregn) /  &
																				   (hhmpgstk(iregn,2,ildv,iage,n)))
		enddo
      enddo
    enddo
!...calculate national consumption (gallons of gasoline equivalent) 
	CMPGT(n,mnumcr) = sum(CMPGT(n,1:mnumcr-2))	!hh car
	TMPGT(n,mnumcr) = sum(TMPGT(n,1:mnumcr-2))	!hh truck	

!...Calculate average mpg of household light duty vehicles	
!...national household car and truck stock fuel economies	
	SCMPG(N) = sum(VMT_STK_HH(1,1:maxldv,1:maxage,1,1:mnumcr-2))/CMPGT(n,mnumcr)
	STMPG(N) = sum(VMT_STK_HH(2,1:maxldv,1:maxage,1,1:mnumcr-2))/TMPGT(n,mnumcr)
	
!...national household combined stock fuel economy
    MPGHH(n) = sum(VMT_STK_HH(1:maxvtyp,1:maxldv,1:maxage,1,1:mnumcr-2)) / (CMPGT(n,mnumcr)+TMPGT(n,mnumcr))
	
!...Calculate average vehicle mpg by technology (combined car and LT) for IEOVARS
    do ildv=1,maxldv
      NUM1 = 0.0
      DEN1 = 0.0
	  do ivtyp=1,maxvtyp
		do iage=1,maxage
		  do iregn=1,mnumcr-2 
		    if(hhmpgstk(iregn,ivtyp,ildv,iage,n).ne.0.0) then											
              NUM1 = NUM1 + VMT_STK_HH(ivtyp,ILDV,iage,1,iregn)
              DEN1 = DEN1 + (VMT_STK_HH(ivtyp,ILDV,iage,1,iregn) / hhmpgstk(iregn,ivtyp,ildv,iage,n)) 
		    endif
		  enddo !end iregn loop
		enddo !end iage loop
	  enddo
      MPGTECH(ILDV,n) = 0.0
      if(DEN1.ne.0.0) MPGTECH(ILDV,n) = NUM1/DEN1
	enddo

!...calculate average car and light truck mpg by technology 
    do ildv=1,maxldv
      CMPG_IT(ildv,n) = 0.0
      TMPG_IT(ildv,n) = 0.0
	  do ivtyp=1,maxvtyp
        NUM1 = 0.0
        DEN1 = 0.0
		NUM2 = 0.0
		DEN2 = 0.0
		do iage=1,maxage
		  do iregn=1,mnumcr-2 
			if(hhmpgstk(iregn,ivtyp,ildv,iage,n).ne.0.0) then
			  if(ivtyp.eq.1) then			
                NUM1 = NUM1 + VMT_STK_HH(ivtyp,ildv,iage,1,iregn)
                DEN1 = DEN1 + (VMT_STK_HH(ivtyp,ildv,iage,1,iregn) / hhmpgstk(iregn,ivtyp,ildv,iage,n))  
			  else 
		        NUM2 = NUM2 + VMT_STK_HH(ivtyp,ildv,iage,1,iregn)
                DEN2 = DEN2 + (VMT_STK_HH(ivtyp,ildv,iage,1,iregn) / hhmpgstk(iregn,ivtyp,ildv,iage,n)) 
			  endif			
		    endif
		  enddo !end iregn loop
		enddo !end iage loop
		if(ivtyp.eq.1) then 
		  if(DEN1.ne.0.0) CMPG_IT(ildv,n) = NUM1/DEN1
		else 
		  if(DEN2.ne.0.0) TMPG_IT(ildv,n) = NUM2/DEN2	
		endif
	  enddo ! ivtyp
    enddo ! ildv 

  RETURN
  END SUBROUTINE TMPGSTK

! ==========================================================================================================
! ... Subroutine TCURB the stock average weight (by vintage) of cars and light trucks  
! ... This subroutine only considers the weight of non-highly automated vehicles 
! ==========================================================================================================
  SUBROUTINE TCURB
  USE T_
  IMPLICIT NONE

    IF (CURCALYR .LT. 1995) RETURN

    IF (CURITR .EQ. 1 .AND. CURCALYR .GT. 2006) THEN
      do IVTYP = 1,maxvtyp 
        STKAVGWGT(IVTYP,MAXAGE) = (STKAVGWGT(IVTYP,MAXAGE)*  SUM(LDV_STOCK(mnumcr,IVTYP,1,1:MAXLDV,MAXAGE,1,N)) +    &
                                   STKAVGWGT(IVTYP,MAXAGE-1) * SUM(LDV_STOCK(mnumcr,IVTYP,1,1:MAXLDV,MAXAGE-1,1,N))) / &
                                   SUM(LDV_STOCK(mnumcr,IVTYP,1,1:MAXLDV,MAXAGE-1:MAXAGE,1,N))	
      enddo												  												 
      DO IAGE=MAXAGE-1,2,-1
        STKAVGWGT(1:2,IAGE) = STKAVGWGT(1:2,IAGE-1)     ! shift vintages one year
      ENDDO
    ENDIF

    STKAVGWGT(1,1) = AWTCAR(11,N)
    STKAVGWGT(2,1) = AWTTRUCK(11,N)

!...Calculate average weight of cars and light trucks over vintage
    TRWTCAR_STOCK(N) = 0.0
    TRWTTRK_STOCK(N) = 0.0
    DO IAGE=1,MAXAGE
      TRWTCAR_STOCK(N) = TRWTCAR_STOCK(N) + STKAVGWGT(1,IAGE)*SUM(LDV_STOCK(mnumcr,1,1,1:maxldv,iage,1,n))  
      TRWTTRK_STOCK(N) = TRWTTRK_STOCK(N) + STKAVGWGT(2,IAGE)*SUM(LDV_STOCK(mnumcr,2,1,1:maxldv,iage,1,n)) 
    ENDDO

    TRWTCAR_STOCK(N) = TRWTCAR_STOCK(N) / SUM(LDV_STOCK(mnumcr,1,1,1:MAXLDV,1:MAXAGE,1,n)) 
    TRWTTRK_STOCK(N) = TRWTTRK_STOCK(N) / SUM(LDV_STOCK(mnumcr,2,1,1:MAXLDV,1:MAXAGE,1,n))

!...Fill reporting variable with historic data
    if(n.le.16)then
      trwtcar_stock(n) = trwtcar_hist(n)
      trwttrk_stock(n) = trwttrk_hist(n)
    endif

  RETURN
  END SUBROUTINE TCURB

! ==========================================================================================================
! ... Subroutine TFLTMPGS calculates MPG for the fleet stock
! ==========================================================================================================
  SUBROUTINE TFLTMPGS
  USE T_
  IMPLICIT NONE

  REAL     MPGFSTK(MNUMCR-2,MAXVTYP,MAXFLEET,MAXLDV,MAXAGE,MNUMYR)
  REAL     NUM,DEN,NUM1,DEN1,DEN2    

!...populate 1995 fltmpgstk values for all vintages 
    if(curcalyr.eq.first_read_year) then 
	  do iregn=1,mnumcr
		do ivtyp=1,maxvtyp
		  do ifleet=1,maxfleet
			do ildv=1,maxldv
			  do iage=2,maxage
				if(ildv.eq.1) fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n) = cmpgstkgas95(ivtyp,iage)*degfac(ivtyp,ildv,n)
				if(ildv.eq.2) fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n) = cmpgstkgas95(ivtyp,iage)*degfac(ivtyp,ildv,n)*afvadjfe(ildv,ivtyp) 
				if(ildv.eq.3) fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n) = cmpgstkgas95(ivtyp,iage)*degfac(ivtyp,ildv,n)
				if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n) = cmpgstkgas95(ivtyp,iage)*degfac(ivtyp,ildv,n)*4.0				
				if(ildv.ge.9.and.ildv.le.12) fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n) = cmpgstkgas95(ivtyp,iage)*degfac(ivtyp,ildv,n)
			  enddo
			enddo 
		  enddo 
		enddo 
	  enddo 
	endif

!...calculate new fleet fuel economy by region, fleet type, vtyp, and ldv <= 2018
	if(curcalyr.le.2018) then
      do iregn=1,mnumcr
		if(iregn.ne.10) then
		  do ivtyp=1,maxvtyp 	    
			do ifleet=1,maxfleet
		      do ildv=1,maxldv
				fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = ldvmpgnew(mnumcr,ivtyp,ildv,n)
!...          	fill other new fleet vehicle ldvs with FEM data (conversion/demonstration/small mfr) 			  
				if(fltmpgnew(iregn,ivtyp,ifleet,ildv,n).eq.0.0) then
				  if(ildv.eq.2) fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n) * afvadjfe(ildv,ivtyp)
				  if(ildv.eq.3) fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n)
				  if(ildv.ge.9.and.ildv.le.12) fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n)
				  if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then
					if(ivtyp.eq.1) fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = 105.0
					if(ivtyp.eq.2) fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = 85.0	
				  endif			
				endif
			  enddo 
			enddo 
		  enddo
		endif
	  enddo 
	endif !<=2018

!...fleet mpg regional detail  
	if(curcalyr.ge.2019) then
!...  ensure all new fleet ldvs have a mpg in case older ldvs appear in fleet/region in later years 
	  do iregn=1,mnumcr 
	    do ivtyp=1,maxvtyp 
	      do ifleet=1,maxfleet 
		    do ildv=1,maxldv	    
		      fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = ldvmpgnew(mnumcr,ivtyp,ildv,n)
			  if(iregn.le.9) then
			    if(ldvmpgnew(iregn,ivtyp,ildv,n).ne.0.0) fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = ldvmpgnew(iregn,ivtyp,ildv,n)
			  endif
			enddo 
		  enddo 
		enddo 
	  enddo
!...  calculate region specific fleet new vehicle mpg
	  do iregn=1,mnumcr-2
		do ifleet=1,maxfleet
		  do ildv=1,maxldv 		
			DEN1 = 0.0
			DEN2 = 0.0
			do icl=1,maxclass
	          do igp=1,maxgroup				
				if(fltgrpsal(iregn,ifleet,igp,icl,ildv).ne.0.0.and.femmpg(igp,icl,yrs,ildv).ne.0.0) then
				  if(igp.le.cargrp) then			  
				    DEN1 = DEN1 + fltgrpsal(iregn,ifleet,igp,icl,ildv)/femmpg(igp,icl,yrs,ildv)
				  else 
				    DEN2 = DEN2 + fltgrpsal(iregn,ifleet,igp,icl,ildv)/femmpg(igp,icl,yrs,ildv)	
				  endif 
				endif
			  enddo
			enddo 
			if(DEN1.ne.0.0) fltmpgnew(iregn,1,ifleet,ildv,n) = sum(fltgrpsal(iregn,ifleet,1:cargrp,1:maxclass,ildv))/DEN1
			if(DEN2.ne.0.0) fltmpgnew(iregn,2,ifleet,ildv,n) = sum(fltgrpsal(iregn,ifleet,ltkgrp:maxgroup,1:maxclass,ildv))/DEN2		
		  enddo 
		enddo 
	  enddo 
!...  calculate US fleet mpg
	  do ivtyp=1,maxvtyp 
	    do ifleet=1,maxfleet 
		  do ildv=1,maxldv
			DEN = 0.0
			do iregn=1,mnumcr-2
			  if(sum(fltechsal(iregn,ivtyp,ifleet,1:maxclass,ildv,:)).ne.0.0.and.fltmpgnew(iregn,ivtyp,ifleet,ildv,n).ne.0.0) then
				DEN = DEN + sum(fltechsal(iregn,ivtyp,ifleet,1:maxclass,ildv,:))/fltmpgnew(iregn,ivtyp,ifleet,ildv,n) 
			  endif
			enddo
			if(DEN.ne.0.0) fltmpgnew(mnumcr,ivtyp,ifleet,ildv,n) = sum(fltechsal(1:mnumcr-2,ivtyp,ifleet,1:maxclass,ildv,:))/DEN
		  enddo 
		enddo 
	  enddo
	endif ! years >= 2019

!debug for fltmpgnew jma
	if(curcalyr.le.stockyr) then 
	  do iregn=1,mnumcr
	    do ivtyp=1,maxvtyp 
		  do ifleet=1,maxfleet
		    do ildv=1,maxldv 
			  if(flt_stock(iregn,ivtyp,ifleet,ildv,1,1,n).ne.0.0.and.fltmpgnew(iregn,ivtyp,ifleet,ildv,n).eq.0.0) then 
			    write(21,*)'fleet regional mpg error, iregn',iregn,curcalyr
				write(21,*)'ivtyp,ifleet,ildv',ivtyp,ifleet,ildv
				write(21,*)'flt_stock',flt_stock(iregn,ivtyp,ifleet,ildv,1,1,n)
			    write(21,*)'fltmpgnew',fltmpgnew(iregn,ivtyp,ifleet,ildv,n)
				write(21,*)'ldvmpgnew',ldvmpgnew(mnumcr,ivtyp,ildv,n)
			  endif
			enddo
		  enddo 
		enddo 
	  enddo
	endif         
!...Calculate new average fleet mpg for cars and lt 
	do ivtyp=1,maxvtyp  
	  NUM = 0.0
	  DEN = 0.0
	  do ifleet=1,maxfleet
		do ildv=1,maxldv
		  if(fltmpgnew(mnumcr,ivtyp,ifleet,ildv,n).ne.0.0) then
			NUM = NUM + sum(flt_stock(mnumcr,ivtyp,ifleet,ildv,1,:,n))
			DEN = DEN + sum(flt_stock(mnumcr,ivtyp,ifleet,ildv,1,:,n))/fltmpgnew(mnumcr,ivtyp,ifleet,ildv,n)
		  endif
		enddo
	  enddo
	  FLTMPGTOT2(IVTYP) = 0.0
	  if(DEN.ne.0.0) FLTMPGTOT2(IVTYP) = NUM/DEN
	enddo	  

!...Advance vintage array of fleet stock mpg 1 year and account for new additions       ! MDRMDR put inside of an ihav loop if flthav working correctly
	do iregn=1,mnumcr
	  if(iregn.ne.10) then
		do ivtyp=1,maxvtyp
		  do ifleet=1,maxfleet
!...        fill vintage 1
			do ildv=1,maxldv ! add ihav loop jma
			  fltmpgstk(iregn,ivtyp,ifleet,ildv,1,n) = fltmpgnew(iregn,ivtyp,ifleet,ildv,n) * degfac(ivtyp,ildv,n)
			enddo
			if(curcalyr.gt.first_read_year) then				
			  do ildv=1,maxldv 
!...		    advance vintages 2-maxage one year	
				do iage=2,maxage
				  fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n) = fltmpgstk(iregn,ivtyp,ifleet,ildv,iage-1,n-1)		
				enddo
!...			calculate last vintage (25)
				num1 = 0
				den1 = 0
				if(flt_stock(iregn,ivtyp,ifleet,ildv,maxage-1,1,n-1).ne.0.0.and.fltmpgstk(iregn,ivtyp,ifleet,ildv,maxage-1,n-1).ne.0.0) then 
			      num1 = flt_stock(iregn,ivtyp,ifleet,ildv,maxage-1,1,n-1)
			      den1 = flt_stock(iregn,ivtyp,ifleet,ildv,maxage-1,1,n-1)/fltmpgstk(iregn,ivtyp,ifleet,ildv,maxage-1,n-1)
			    elseif(flt_stock(iregn,ivtyp,ifleet,ildv,maxage,1,n-1).ne.0.0.and.fltmpgstk(iregn,ivtyp,ifleet,ildv,maxage,n-1).ne.0.0) then 
				  num1 = num1 + flt_stock(iregn,ivtyp,ifleet,ildv,maxage,1,n-1)
				  den1 = den1 + flt_stock(iregn,ivtyp,ifleet,ildv,maxage,1,n-1)/fltmpgstk(iregn,ivtyp,ifleet,ildv,maxage,n-1)
				endif
			    if(den1.ne.0.0) fltmpgstk(iregn,ivtyp,ifleet,ildv,maxage,n) = num1/den1	
		      enddo
			endif ! > first_read_year
	      enddo
		enddo
	  endif
	enddo

! debug to check sales input consistency between input files jma
    if(curcalyr.le.2026) then
	do iregn=1,mnumcr-2
	  do ivtyp=1,maxvtyp
		do ifleet=1,maxfleet
		  do ildv=1,maxldv
			do iage=1,maxage
              if(Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,1,n).ne.0.0.and.fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n).eq.0.0) then 
			    write(21,*)'error between stocks and mpg',curcalyr
			    write(21,*)'fltmpgstk, iregn',iregn,curcalyr
			    write(21,'(4(A,I6))')'ivtyp',ivtyp,'ifleet',ifleet,'ildv',ildv,'iage',iage
				write(21,*)'fleet stock',Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,1,n)
				write(21,*)'fleet mpg',fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n)
			  endif
		    enddo
		  enddo
		enddo
	  enddo 
	enddo
	endif
	
!...Calculate average mpg by vehicle and fleet type                             
!...Note: Weight MPG vintages by their stock (and not VMT since we assumed all  
!...vintages are driven the same annual VMT) and also apply the degradation factors    
    do ivtyp=1,maxvtyp
      do ifleet=1,maxfleet
        do ildv=1,maxldv
		  do iregn=1,mnumcr
			mpgfltstk(iregn,ivtyp,ifleet,ildv,n) = 0.0
            NUM = 0.0
            DEN = 0.0
		    if(iregn.ne.10) then
			  do iage=1,maxage
				do ihav = 1,maxhav
			      if(ihav.eq.1) then
					if(fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n).ne.0.0) then
			          NUM = NUM +  Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,ihav,n)
                      DEN = DEN + (Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,ihav,n)/fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n))
					endif 
				  else !automated vehicles
					if(curcalyr.ge.first_lidar_year(1)) then
					  if(fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n).ne.0.0) then
			            NUM = NUM +  Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,ihav,n)
                        DEN = DEN + (Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,ihav,n)/(fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n)*hav_mpgdeg(ihav,yrs)))
					  endif
					endif 				
				  endif
				enddo
			  enddo
              if(DEN.ne.0.0) MPGFLTSTK(iregn,ivtyp,ifleet,ildv,n) = NUM/DEN			
			endif
		  enddo ! iregn
        enddo
      enddo
    enddo

!...Calculate overall fleet average mpg by fuel technology (FLTTOTMPG)
    do ivtyp=1,maxvtyp
      NUM = 0.0
      DEN = 0.0
      do ifleet=1,maxfleet
        do ildv=1,maxldv
		  do ihav = 1,maxhav
            if(MPGFLTSTK(mnumcr,ivtyp,ifleet,ildv,n).ne.0.0) then
              NUM = NUM +  FLTECHSTK(mnumcr,ivtyp,ifleet,ildv,ihav)
              DEN = DEN + (FLTECHSTK(mnumcr,ivtyp,ifleet,ildv,ihav)/MPGFLTSTK(mnumcr,ivtyp,ifleet,ildv,n))
            endif
		  enddo
        enddo
      enddo
      FLTTOTMPG(IVTYP) = 0.0
      if(DEN.ne.0.0) FLTTOTMPG(ivtyp) = NUM/DEN
    enddo

  RETURN
  END SUBROUTINE TFLTMPGS

! ==========================================================================================================
! ... Subroutine TFLTCONS calculates fuel consumption of fleet vehicles
! ==========================================================================================================
  SUBROUTINE TFLTCONS
  USE T_
  IMPLICIT NONE

!...Calculate fuel consumption (gallons and btu) by ldv type
	fltechgge=0.0
    do ivtyp=1,maxvtyp
      do ifleet=1,maxfleet
        do ildv=1,maxldv
		  do iregn=1,mnumcr
		      if(iregn.ne.10) then
              if(MPGFLTSTK(iregn,ivtyp,ifleet,ildv,n).ne.0.0) then
			    fltechgge(iregn,ivtyp,ifleet,ildv,n) = (sum(fltechvmt(iregn,ivtyp,ifleet,ildv,1:maxhav))/1000000.0)/MPGFLTSTK(iregn,ivtyp,ifleet,ildv,n)
			    fltechbtu(iregn,ivtyp,ifleet,ildv,n) = fltechgge(iregn,ivtyp,ifleet,ildv,n) * MG_HHV/1000.0
			  endif
			endif
		  enddo
        enddo
      enddo
    enddo
	
!...fleet btu by ldv 
	do iregn=1,mnumcr 
	  if(iregn.ne.10) then 
	    do ildv=1,maxldv 	  
		  do ivtyp=1,maxvtyp
			fltldvbtu(iregn,ivtyp,ildv,n) = sum(fltechbtu(iregn,ivtyp,1:maxfleet,ildv,n)) 
		  enddo
		  fltldvbtut(iregn,ildv,n) = sum(fltldvbtu(iregn,1:maxvtyp,ildv,n))
		enddo
	  endif 
	enddo

!...Calculate total fleet consumption by fuel type by region 
    do ifuel=1,maxfuel
      do iregn=1,mnumcr-2
!...    gasoline
        if(IFUEL.eq.1) then
          FLTFUELBTU(iregn,1,n)= (fltldvbtut(iregn, 1,n) + &                           ! Gasoline
                                 (fltldvbtut(iregn, 3,n)*(1.0-PctAF(2,iregn,n))) + &   ! Ethanol FFV
                                 (fltldvbtut(iregn, 5,n)*(1.0-PctPHEV20(n))) + & 	   ! PHEV20
                                 (fltldvbtut(iregn, 6,n)*(1.0-PctPHEV50(n))) + & 	   ! PHEV50 
                                 (fltldvbtut(iregn, 9,n)*(1.0-PctAF(3,iregn,n))) + &   ! CNG Bifuel
                                 (fltldvbtut(iregn,10,n)*(1.0-PctAF(4,iregn,n))) + &   ! LPG Bifuel
                                  fltldvbtut(iregn,16,n))             				   ! Hybrid Gasoline
!...    methanol
        elseif(IFUEL.eq.2) then
          FLTFUELBTU(iregn,2,n)=  fltldvbtut(iregn,13,n)                				! FCV Methanol
!...    ethanol consumption
        elseif(IFUEL.eq.3) then
          FLTFUELBTU(iregn,3,n)= (fltldvbtut(iregn, 3,n)*PctAF(2,iregn,n))   			! Ethanol FFV
                                 
!...    CNG
        elseif(IFUEL.eq.4) then
          FLTFUELBTU(iregn,4,n)=((fltldvbtut(iregn, 9,n)*(PctAF(3,iregn,n))) + &     	! CNG Bifuel
                                  fltldvbtut(iregn,11,n))  								! CNG Dedicated 
!...    LPG
        elseif(IFUEL.eq.5) then
          FLTFUELBTU(iregn,5,n)=((fltldvbtut(iregn,10,n)*(PctAF(4,iregn,n))) + &     	! LPG Bifuel
                                  fltldvbtut(iregn,12,n)) 				            	! LPG Dedicated  
!...    electricity
        elseif(IFUEL.eq.6) then
          FLTFUELBTU(iregn,6,n)=((fltldvbtut(iregn, 5,n)* PctPHEV20(n)) + &    			! PHEV20
                                 (fltldvbtut(iregn, 6,n)* PctPHEV50(n)) + &    			! PHEV50 
                                  fltldvbtut(iregn, 7,n)+ &
								  fltldvbtut(iregn, 4,n)+ &
								  fltldvbtut(iregn,15,n)) 								! EV 
!...    hydrogen
        elseif(IFUEL.eq.7) then
          FLTFUELBTU(iregn,7,n)=  fltldvbtut(iregn,14,n)  						 		! FCV Hydrogen

!...    diesel consumption
        elseif(IFUEL.eq.8) then
          FLTFUELBTU(iregn,8,n)=( fltldvbtut(iregn, 2,n) + &                         	! Diesel
                                  fltldvbtut(iregn, 8,n)) 			                	! Hybrid Diesel
        endif
      enddo
    enddo

    do ifuel=1,maxfuel
      FLTFUELBTU(11,ifuel,n) = sum(FLTFUELBTU(1:MNUMCR-2,ifuel,n))
    enddo

  RETURN
  END SUBROUTINE TFLTCONS

! ==========================================================================================================
! ... Subroutine TVMT calculates total household light vehicle VMT
! ==========================================================================================================
    SUBROUTINE TVMT
    USE T_
    IMPLICIT NONE

    REAL    VMTEXP(MF,AGEGRP), CA_ADJ, VMTLD_1(AGEGRP,MNUMYR,MF), NUM1, NUM2
	REAL 	vmtldvhh(mnumcr-2), hhvmtadj(mnumcr-2)

!...Calculate cost of driving per mile (COSTMI)
    COSTMI(n) = ((PMGTR(11,n)*CFMGQ(n)/42.0)/trldmpgf(3,n))*100.0 * MC_JPGDP(11)
	
!...Calculate employment rate for vmt growth
    EMP_RATE_VMT(n) = MC_EEA(n)/MC_NP16A(11,n)
	  
!...Calculate vmt per licensed driver (VMTLD) for years greater than last historic year of data (1000's of miles)
    IF(curcalyr.gt.VMTLDHistYr) THEN
	  VMTLD(:,n-1,:) = VMTLD_1(:,n-1,:)	    
      DO IMF=1,MF
        DO iagr=1,agegrp 
		  VMTEXP(imf,iagr) = (ALPHA(imf,iagr) + BETAVMT(imf,iagr)*LOG(VMTLD(iagr,n-1,imf))+BETAINC(imf,iagr)*LOG(INC00_D_16(11,n))   + &
							  BETACOST(imf,iagr)*LOG(COSTMI(n))+BETAVPLD(imf,iagr)*LOG(VPLD(n))+BETAEMP(imf,iagr)*LOG(EMP_RATE_VMT(n)))
		  VMTLD(iagr,n,imf) = EXP(VMTEXP(imf,iagr))
		ENDDO
	  ENDDO
	ENDIF
	  
	VMTLD_1 = VMTLD 
	  
! ... Adjust vmt per licensed driver 65+ to account for aging of this cohort
      if(curcalyr.gt.VMTLDHistYr+1) then
	    Do imf=1,mf
!         reference case adjustment	  
	      vmtld(5,n,imf)=vmtld(5,n,imf)*age_adj(imf,n)
!		  low macro adjustment
		  if(MMAC.eq.4) vmtld(5,n,imf)=vmtld(5,n,imf)*age_adj_l(imf,n)
!		  high macro adjustment
		  if(MMAC.eq.5) vmtld(5,n,imf)=vmtld(5,n,imf)*age_adj_h(imf,n)
	    enddo
	  endif
	  
!...recalculate regional total driving demand VMTLDV (billion miles) with projected VMT/licensed driver
!...set all to gasoline and redistribute to ILDV lower down
    do imf=1,mf
      do iagr=1,agegrp 
		do iregn=1,mnumcr-2
			VMTLDV(iagr,n,imf,iregn) = VMTLD(iagr,n,imf) * LICDRIVER(iagr,imf,iregn,n)
        enddo
      enddo
	enddo
 
!...adjust region 9 vmt for california GHG vmt reduction policy
    if(TRANAB32 .NE. 0) then
	  do imf=1,MF
	    do iagr=1,agegrp	
		  VMTLDV(iagr,n,imf,9) = VMTLDV(iagr,n,imf,9)*vmt_ca_co2(n) 
		enddo
	  enddo
	endif

!...calculate national vmt 
	do imf=1,MF
	  do iagr=1,agegrp
		VMTLDV(iagr,n,imf,mnumcr) = sum(VMTLDV(iagr,n,imf,1:mnumcr-2))
	  enddo
	enddo

!...calculate US household vmt (millions) by vintage from regional estimates weighted by vehicle stocks
	do iage=1,maxage
	  NUM1 = 0.0
	  NUM2 = 0.0		
	  do iregn=1,mnumcr-2
!...	sum each region's vmt/vintage weighted by their stock
		do ildv=1,maxldv
		  NUM1 = NUM1 + (PVMT(iage,n,iregn,ildv)*LDV_STOCK(iregn,1,1,ildv,iage,1,n))
		  NUM2 = NUM2 + (LVMT(iage,n,iregn,ildv)*LDV_STOCK(iregn,2,1,ildv,iage,1,n))
		enddo
	  enddo
!...  divide sum by total stock to finish weighted average calculation
	  do ildv=1,maxldv
	    if(ldv_stock(mnumcr,1,1,ildv,iage,1,n).ne.0.0) then
	      PVMT(iage,n,mnumcr,ildv)=NUM1/LDV_STOCK(mnumcr,1,1,ildv,iage,1,n)
	      LVMT(iage,n,mnumcr,ildv)=NUM2/LDV_STOCK(mnumcr,2,1,ildv,iage,1,n)		
		endif
	  enddo
	enddo

!...Calculate total household miles driven by each type of vehicle by vintage (million miles)
	hhtechvmt = 0.0
    do ildv=1,maxldv 
	  do iage=1,maxage 
	    do iregn=1,mnumcr-2		    
		  hhtechvmt(iregn,1,ildv,iage) = LDV_STOCK(iregn,1,1,ildv,iage,1,n)*PVMT(iage,n,iregn,ildv)
		  hhtechvmt(iregn,2,ildv,iage) = LDV_STOCK(iregn,2,1,ildv,iage,1,n)*LVMT(iage,n,iregn,ildv)
		enddo
		do ivtyp=1,maxvtyp		
		  hhtechvmt(mnumcr,ivtyp,ildv,iage) = sum(hhtechvmt(1:mnumcr-2,ivtyp,ildv,iage))
        enddo
	  enddo	  
    enddo

!...adjust hhtechvmt to align with vmtldv
!...calculate driver based household vmt
    do iregn=1,mnumcr-2
	  vmtldvhh(iregn) = sum(vmtldv(1:agegrp,n,1:mf,iregn)) - sum(fltechvmt(iregn,1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav))/1000000000.0
	enddo
!...calculate household regional vmt adjustment factor
	do iregn=1,mnumcr-2
      hhvmtadj(iregn) = vmtldvhh(iregn)/(sum(hhtechvmt(iregn,1:maxvtyp,1:maxldv,1:maxage))/1000.0)
	enddo
	
!...apply vmt adjustment factor to hhtechvmt 
	do ivtyp=1,maxvtyp 
	  do ildv=1,maxldv
		do iage=1,maxage 
		  do iregn=1,mnumcr-2 
		    hhtechvmt(iregn,ivtyp,ildv,iage) = hhtechvmt(iregn,ivtyp,ildv,iage) * hhvmtadj(iregn) 
		  enddo
		  hhtechvmt(mnumcr,ivtyp,ildv,iage) = sum(hhtechvmt(1:mnumcr-2,ivtyp,ildv,iage))
		enddo 
	  enddo 
	enddo

!...total household vmt for reporting (billion miles)	
	do iregn=1,mnumcr 
	  do ildv=1,maxldv 
	    do ivtyp=1,maxvtyp 
		  vmthh(n,iregn,ildv,ivtyp) = sum(hhtechvmt(iregn,ivtyp,ildv,1:maxage))/1000.0
		enddo 
	  enddo 
	enddo

    PE(N) = 1 !BETACOST * (COSTMI(N) / VMTLD(11,N))
    IE(N) = 1 !BETAINC  * (INC00_D_16(11,N) / VMTLD(11,N))
    DE(N) = 1 !BETAVMT  * (VMTLD(11,N) / VMTLD(11,N))

    RETURN
    END SUBROUTINE TVMT

! ==========================================================================================================
! ... Subroutine TMPGAG summarizes (personal and fleet) light vehicle sales and mpg by technology
! ==========================================================================================================
  SUBROUTINE TMPGAG
  USE T_
  IMPLICIT NONE

    REAL     SUMLDV1(maxvtyp),SUMLDV2(maxvtyp)

! ... Calculate fleet average stock car & light truck mpg (Table 50)
      SUMLDV1(1) = 0.0
      SUMLDV1(2) = 0.0
!...hh car and truck stock fuel economy to be weighted
!.. by VMT instead of stocks					 
      IF (SCMPG(N) .NE. 0.0 .AND. FLTTOTMPG(1) .NE. 0.0) &
        SUMLDV1(1) = (sum(VMT_STK_HH(1,1:maxldv,1:maxage,1,1:mnumcr-2))/(SCMPG(N)*(CFMGQ(N)/CFMGQ(18)))) + &
                     (TOTFLTCAR(1)/(FLTTOTMPG(1)*(CFMGQ(N)/CFMGQ(18))))
      IF (STMPG(N) .NE. 0.0 .AND. FLTTOTMPG(2) .NE. 0.0) &
        SUMLDV1(2) = (sum(VMT_STK_HH(2,1:maxldv,1:maxage,1,1:mnumcr-2))/(STMPG(N)* (CFMGQ(N)/CFMGQ(18)))) + &
                     (TOTFLTCAR(2)/(FLTTOTMPG(2)*(CFMGQ(N)/CFMGQ(18))))
      SUMLDV2(1) = STKCAR(N) + TOTFLTCAR(1)
      SUMLDV2(2) = STKTR(N)  + TOTFLTCAR(2)

      DO IVTYP=1,MAXVTYP
        TLDVMPG(IVTYP,N) = 0.0
        IF (SUMLDV1(IVTYP) .NE. 0.0 .AND. SUMLDV2(IVTYP) .NE. 0.0) &
          TLDVMPG(IVTYP,N) = 1.0 / (SUMLDV1(IVTYP)/SUMLDV2(IVTYP))
      ENDDO

! ... Calculate fleet average stock vehicle mpg (Table 50)
      TLDVMPG(3,N) = 0.0
      IF (SUMLDV1(1)+SUMLDV1(2) .NE. 0.0 .AND. SUMLDV2(1)+SUMLDV2(2) .NE. 0.0) &
        TLDVMPG(3,N) = 1.0 / ((SUMLDV1(1) + SUMLDV1(2)) / (SUMLDV2(1) + SUMLDV2(2)))

    RETURN
    END SUBROUTINE TMPGAG

! ==========================================================================================================
!...Subroutine TRAIL 
!   Description:
!   	Projects freight rail energy consumption by estimting ton-miles, energy efficiency, and fuel choice
! ==========================================================================================================
    SUBROUTINE TRAIL
    USE T_
    IMPLICIT NONE
    INCLUDE 'NGTDMOUT'

!...Local variable dictionary
    INTEGER :: ISIC		       	              !...industrial sector subscript
    INTEGER, PARAMETER :: SIC = 16            !...industrial sectors that move domestic marine commodities:
                                              !...1) basic chemicals                 2) primary metals
                                              !...3) processed food                  4) paper products
                                              !...5) petroleum products              6) stone, clay, glass, and concrete
                                              !...7) metal durables, less computers  8) other manufacturing
                                              !...9) agriculture                     10) mining
                                              !...11) alcohol and tobacco            12) pharma
                                              !   13) fertilizers                    14) rubber and plastics
                                              !   15) computers
                                              !   16) furniture
    INTEGER, PARAMETER :: US = 11             !...national census division level
    REAL :: COAL_TMT(MNUMYR)                  !...sum of coal ton-miles travelled
    REAL :: COAL_GROWTH(MNUMYR)               !...coal growth rate of ton-miles traveled
    REAL :: FUEL_PRICE(2,MNUMYR)              !...Class I Railroad fuel price paid for: 1=diesel , 2=LNG
    REAL :: ANN_FUEL_SAVINGS(30)              !...annual fuel savings for LNG compared to diesel fuel per locomotive
    REAL :: NPV_LNG(MNUMYR)                   !...net present value of LNG fuel savings for locomotives
    INTEGER :: Y                              !...local accounting variable #1
    REAL :: LNG_LOCOM_INCCOST(MNUMYR)         !...incremental cost (2012$) of LNG locomotive and tender
    REAL :: LNG_SHR(MNUMYR)                   !...LNG market share
    INTEGER once/.false./                     !...logical switch
    REAL :: rail_fuel_shr(4,mnumyr)           !...rail fuel share by fuel type (1=diesel,2=resid,3=CNG,4=LNG)
    INTEGER :: T                              !...local accounting variable
    INTEGER :: adoptlng(mnumyr)               !...local accounting variable
  
RPROJ_NCTONMI(n,:,:)= 0.
RPROJ_CTONMI(n,:)	= 0.

!...calculate freight rail freight parameter for latest FAF year
    if(n.eq.iFAFyear) then
	  do iregn=1,mnumcr-2
        do isic=1,sic
          if(TSIC(isic,iregn,iFAFyear).gt.0.) then
            RTM_OUTPUT(iregn,isic)=RHIST_NCTONMI(n,iregn)*RTM_SHARES(iregn,isic)/TSIC(isic,iregn,iFAFyear)
		   else						
             RTM_OUTPUT(iregn,isic)=0.
           endif
        enddo
      enddo
    endif
	
!...project census division history (between iFAFyear and RAILHISTYR) to compute census division shares
    do iregn=1,mnumcr-2
      do isic=1,sic
        if((n.ge.iFAFyear).and.(n.le.RAILHISTYR))then
          RPROJ_NCTONMI(n,iregn,isic)=TSIC(isic,iregn,n)*RTM_OUTPUT(iregn,isic)
        endif
      enddo
    enddo

!...calculate freight rail ton-miles
    if(n.ge.RAILHISTYR)then
      COAL_TMT(n)=TTONMILE(n)
    endif
    do iregn=1,mnumcr-2
      if(n.le.RAILHISTYR)then
        RTMTT(n,iregn)=0.
        if((n.ge.iFAFyear).and.(sum(RPROJ_NCTONMI(n,1:mnumcr-2,1:sic)).gt.0.))then
          RTMTT(n,iregn)=(sum(RHIST_NCTONMI(n,1:mnumcr-2))*   &     !  non-coal history by census division
          (sum(RPROJ_NCTONMI(n,iregn,1:sic))/       &
          sum(RPROJ_NCTONMI(n,1:mnumcr-2,1:sic))))+  &
          RHIST_CTONMI(n,iregn)                          !  coal history by census division
        else
          RTMTT(n,iregn)=RHIST_NCTONMI(n,iregn)+RHIST_CTONMI(n,iregn)
        endif
      else
        do isic=1,sic
          RPROJ_NCTONMI(n,iregn,isic)=TSIC(isic,iregn,n)*RTM_OUTPUT(iregn,isic)
        enddo
        COAL_GROWTH(n)=(COAL_TMT(n)-COAL_TMT(n-1))/COAL_TMT(n-1)
        if(n.eq.RAILHISTYR+1)then
            RPROJ_CTONMI(n,iregn)=RHIST_CTONMI(n-1,iregn)*(1.+ COAL_GROWTH(n))
        else
            RPROJ_CTONMI(n,iregn)=RPROJ_CTONMI(n-1,iregn)*(1.+ COAL_GROWTH(n))
        endif
        RTMTT(n,iregn)=sum(RPROJ_NCTONMI(n,iregn,1:SIC)) + RPROJ_CTONMI(n,iregn)
      endif
    enddo
    RTMTT(n,11)=sum(RTMTT(n,1:mnumcr-2)) 

!...calculate freight rail energy consumption
    if(n.gt.RAILHISTYR)then
      if(FRZNEFF.eq.1) FREFF(n)=FREFF(RAILHISTYR)  !...freezes freight rail efficiency at last historic year
      if(HIGHEFF.eq.1) FREFF(n)=HTFREFF(n)
    endif
    do iregn=1,mnumcr
      TQFRAILT(n,iregn)=RTMTT(n,iregn)*FREFF(n)
    enddo

    if(PAYBK.gt.30) PAYBK = 30

!...calculate freight rail fuel consumption
    if(curcalyr.lt.ngyear)then
      do ifuelx=1,4
        rail_fuel_shr(ifuelx,n)=rail_fuel(ifuelx)
      enddo
    else
      FUEL_PRICE(1,n)=PDSTR(US,n) * 1.115 * CIDISCOUNT  !...diesel fuel price (1990$/mmbtu)
      FUEL_PRICE(2,n)=PGLTRRAIL(1,US,n) * 1.115 * 2        !...LNG fuel price (1990$/mmbtu)     ! MDRAEO2025 -- added 2x multiplier (like frt trk) until NGMM corrects LNG price
!...  calculate annual fuel saving from switching to LNG
      do y=1,paybk
        ANN_FUEL_SAVINGS(y) = (((LOCOMBTU*LOCOM_LIFE(y)*FREFF(n))/1000.)*FUEL_PRICE(1,n)) - &
                              (((LOCOMBTU*LOCOM_LIFE(y)*FREFF(n))/1000.)*FUEL_PRICE(2,n))
      enddo
!...  sum of discounted annual LNG fuel savings
      NPV_LNG(n)=ANN_FUEL_SAVINGS(1)/(1.0+DISCRT)
      do Y=2,PAYBK
        NPV_LNG(n)=NPV_LNG(n)+ANN_FUEL_SAVINGS(y)/(1.0+DISCRT)**(Y)
      enddo
      if(curcalyr.eq.ngyear)then 
        LNG_LOCOM_INCCOST(n)=RLNG_INCCOST/mc_jpgdp(24) * mc_jpgdp(1)
      else
        LNG_LOCOM_INCCOST(n)=LNG_LOCOM_INCCOST(n-1)*RLNG_LEARN
      endif 
      if(curcalyr.eq.ngyear) adoptlng(n-1)=1
      do ifuelx=4,1,-1
        if(ifuelx.eq.4)then
          if(LNG_LOCOM_INCCOST(n).gt.NPV_LNG(n))then
            adoptlng(n)=adoptlng(n-1)
            T=adoptlng(n)
            rail_fuel_shr(4,n)=rail_fuel_shr(4,n-1)
          else
            adoptlng(n)=adoptlng(n-1)+1
            T=adoptlng(n)
            rail_fuel_shr(4,n)=rail_fuel_shr(4,n-1)+LNG_MAXPEN(T)*rail_fuel_shr(1,n-1)
          endif
        endif  
        rail_fuel_shr(1,n)=1.0-rail_fuel_shr(4,n)
        rail_fuel_shr(2,n)=rail_fuel(2)
        rail_fuel_shr(3,n)=rail_fuel(3)
      enddo
    endif

!...regionalize fuel consumption   
    do iregn=1,mnumcr
      do ifuelx=1,4
        TQRAILR(ifuelx,iregn,n)=TQFRAILT(n,iregn) * rail_fuel_shr(ifuelx,n)
      enddo
    enddo 

    RETURN
    END SUBROUTINE TRAIL


! ==========================================================================================================
! ... Subroutine TMISC calculates miscellaneous transportation energy use from the military, mass transit 
! ... (buses and rail), recreational boating, and lubricants
! ==========================================================================================================
    SUBROUTINE TMISC
    USE T_
    IMPLICIT NONE

    INCLUDE 'ANGTDM'

      INTEGER   IM,IN
      REAL      HYWAY(MNUMYR),FTVMT(MNUMYR), SHR_TRRPM(MNUMCR-2),SHR_CRRPM(MNUMCR-2),SHR_TBPMT(MNUMCR-2),&
                MILTARGR(MNUMYR),MILTRSHR(4,MNUMCR-2,MNUMYR), CRRPMPCEXP(MNUMCR-2,MNUMYR),CRRPMPC(MNUMCR-2,MNUMYR),&
                TMODRSHR(MNUMCR-2,MNUMYR),LUBRSHR(MNUMCR-2,MNUMYR),TRRPMPCEXP(MNUMCR-2,MNUMYR), IRPMPC1(MNUMYR), &
                BETALUB,BUSSYSEF(3),BOATS,NONFARMEMP(MNUMCR-2,MNUMYR),PMGTR19_D_(MNUMYR),TBPMTPCEXP(MNUMCR-2,MNUMYR), &
				CREFF_ADJ(mnumcr-2), TREFF_ADJ(mnumcr-1), TBBTUPM_ADJ(mnumcr-1)
      REAL      TEMP

! ... Calculate military energy use                                               
! ... Data source:                                                                
! ...      - distillate and residual consumption ('000 gal) are from EIA 'Fuel Oil
! ...        and Kerosene Sales xxxx'                                             
! ...      - residual consumption = residuals + local purchase (DESC-Net Sales)  
! ...      - jet fuel (naphtha) consumption are from Petroleum Supply Annual,    
! ...        Table 2 column "Product Supplied"
! ...      - jet fuel (kerosene JP-5 & JP-8) consumption are from Defense Fuel   
! ...        Supply Center Fact Book, Defense Logistics Agency, Table "Net Sales 
! ...        by Category".                                                       
! ...                                                                            
! ... Note: IFUELX  = 1 :distillates                                              
! ...               = 2 :jet fuel (naphtha)                                      
! ...               = 3 :residual                                                
! ...               = 4 :jet fuel (kerosene: JP-5 + JP-8)                        

! ... Calculate military budget growth rate
! ... Note: MC_GFMLR macro variable for federal government defense purchases
      if(curcalyr.gt.MiltHistYr) MILTARGR(N) = MC_GFMLR(N)/MC_GFMLR(N-1)

! ... Calculate military fuel demand
      do ifuelx=1,4
! ... read historic data
        if(n.lt.6) mfd(ifuelx,n) = 0.0
        if(n.ge.6.and.curcalyr.le.MiltHistYr) mfd(ifuelx,n) = mfdh(ifuelx,n)
! ... project demand growth
        if(curcalyr.gt.MiltHistYr) mfd(ifuelx,n) = mfd(ifuelx,n-1)*MiltARGR(n)
      enddo

! ... Regionalize military fuel use
      DO IFUELX=1,4
        DO IREGN=1,MNUMCR-2
          MILTRSHR(IFUELX,IREGN,N) = MILTRSHR90(IFUELX,IREGN)
          QMILTR(IFUELX,IREGN,N) = MFD(IFUELX,N) * MILTRSHR(IFUELX,IREGN,N)
        ENDDO
      ENDDO

! ... Sum military fuel use by type by region
	DO IFUELX=1,4
          QMILTR(IFUELX,mnumcr,N) = sum(QMILTR(IFUELX,1:mnumcr-2,N))
	ENDDO

!...sum non-farm employs by CD
    nonfarmemp=0.0
    Do iregn=1,mnumcr-2
	   nonfarmemp(iregn,n) = sum(mc_empna(iregn,1:39,n))-sum(mc_empna(iregn,20:21,n))
	enddo   
!...gasoline price 2019$
    PMGTR19_D_(N) = PMGTR(11,N)*MG_HHV/1000.0 * MC_JPGDP(30)   
	  
! ... Transit rail 
    DO IREGN=1,MNUMCR-2
      if(curcalyr.le.TRHISTYEAR) then 
		TRRPM(iregn,n) = TRRPMHIST(iregn,n)
        TREFF(iregn,n) = TREFFHIST(iregn,n)
        TRED(iregn,n)  = TREDHIST(iregn,n)
      else
		if(tred(iregn,n-1).gt.0.0) then
          TRRPMPCEXP(iregn,n) = tr_coef(iregn,1)+(log(MC_GDPR(n)/MC_NP16A(11,n))*tr_coef(iregn,2)+log(pmgtr19_D_(n))*tr_coef(iregn,3)+trcovid(IREGN, n)*tr_coef(iregn,4))
		  TRRPMPC(iregn,n) = exp(TRRPMPCEXP(iregn,n))
		! calculate transit rail efficiency -> assumes efficiency improves to histric value as pmt recovers
          ! Needs to updated with current in yr 
	      TRRPM(iregn,n)=TRRPMPC(iregn,n)*nonfarmemp(iregn,n)
		  TREFF_ADJ(iregn) = treffhist(iregn,33) - treffhist(iregn,30) ! MMA - TREFF_ADJ(iregn) = treffhist(iregn,trhistyear-1989) - treffhist(iregn,trhistyear-1990) 
		  if(trrpm(iregn,n).lt.trrpm(iregn,30)) then
            TREFF(IREGN,N) = TREFFHIST(IREGN,33) - (treff_adj(iregn) * (trrpm(iregn,n)/trrpm(iregn,30))) 
		  else
		    treff(iregn,n) = treff(iregn,30)
		  endif
		else
		  trrpmpc(iregn,n) = 0.0		  
		endif
      endif
	enddo          

!...adjust transit rail rpm to account for growth in CAV travel
    do iregn=1,mnumcr-2
		SHR_TRRPM(iregn) = TRRPM(iregn,n)/sum(TRRPM(1:mnumcr-2,n))
	enddo
	do iregn=1,mnumcr-2
	  if(shr_trrpm(iregn).gt.0.0) TRRPM(iregn,n) = TRRPM(iregn,n)-(sum(FLTVMTECH(1:maxvtyp,4,1:maxldv,2:maxhav))*TR_CAV_ADJ(n)*SHR_TRRPM(iregn)/1000000.0)
	  if(trrpm(iregn,n).gt.0.0) then
	    TRED(iregn,n) = TRRPM(iregn,n)*TREFF(iregn,n)/1000000.0 
	  else
		TRED(iregn,n) = 0.0
	  endif
    enddo
	TRED(11,n)=sum(TRED(1:mnumcr-2,n))
	
!...Commuter rail 
	DO IREGN=1,MNUMCR-2
        IF (curcalyr.le.CRHISTYEAR) THEN 
          CRRPM(IREGN,N) = CRRPMHIST(IREGN,N)
          CREFF(IREGN,N) = CREFFHIST(IREGN,N)     
          CREDD(IREGN,N) = CREDDHIST(IREGN,N)
          CREDE(IREGN,N) = CREDEHIST(IREGN,N)
        ELSE
          CRRPMPCEXP(iregn,n) = cr_coef(iregn,1)+(log(MC_GDPR(n)/MC_NP16A(11,n))*cr_coef(iregn,2)+log(pmgtr19_D_(n))*cr_coef(iregn,3)+crcovid(IREGN,n)*cr_coef(iregn,4))
		  CRRPMPC(iregn,n) = exp(CRRPMPCEXP(iregn,n))
	      CRRPM(iregn,n)=CRRPMPC(iregn,n)*nonfarmemp(iregn,n)
            ! Commuter rail efficiency, assumes efficiency improves to histric value as pmt recovers
            ! Needs to updated with current in yr 
		  CREFF_ADJ(iregn) = creffhist(iregn,33) - creffhist(iregn,30) 
		  if(crrpm(iregn,n).lt.crrpm(iregn,30)) then
            CREFF(IREGN,N) = CREFFHIST(IREGN,33) - (creff_adj(iregn) * (crrpm(iregn,n)/crrpm(iregn,30))) 
		  else
		    creff(iregn,n) = creff(iregn,30)
		  endif
        ENDIF
	ENDDO
	  
!...adjust commuter rail rpm to account for growth in CAV travel	
	do iregn=1,mnumcr-2
		SHR_CRRPM(iregn)=CRRPM(iregn,n)/sum(CRRPM(1:mnumcr-2,n)) 
	enddo
	IF(curcalyr.gt.CRHISTYEAR) THEN 
	  do iregn=1,mnumcr-2
		CRRPM(iregn,n)=CRRPM(iregn,n)+(sum(FLTVMTECH(1:maxvtyp,4,1:maxldv, 2:maxhav))*CR_CAV_ADJ(n)*SHR_CRRPM(iregn)/1000000.0)
		CRED(IREGN,N)   = CRRPM(IREGN,N)*CREFF(IREGN,N)/1000000.0  
		CREDD(IREGN,N)  = CRED(IREGN,N)*CREDDSHR(IREGN)
		CREDE(IREGN,N)  = CRED(IREGN,N)-CREDD(IREGN,N)
	  enddo
	endif
	CREDD(mnumcr,n)=sum(CREDD(1:mnumcr-2,n))
	CREDE(mnumcr,n)=sum(CREDE(1:mnumcr-2,n))
	  
!...Intercity rail

	!fraction of diesel fuel share
      IREDDSHR = IREDDHIST(IRHISTYEAR-1989)/(IREDDHIST(IRHISTYEAR-1989)+IREDEHIST(IRHISTYEAR-1989))

      IF (curcalyr.le.IRHISTYEAR) THEN
        IRRPM(N)  = IRRPMHIST(N)
        IRPMPC(N) = IRPMPCHIST(N)
		IRPMPC1(n)= IRPMPCHIST(n)
        IREFF(N)  = IREFFHIST(N)
        IREDD(N)  = IREDDHIST(N)
        IREDE(N)  = IREDEHIST(N)
      ELSE 
        IRPMPC1(n)= IRPMPC1(n-1)*1.002
        do iregn=1,mnumcr-2
		IRPMPC(n) = IRPMPC1(n)*(1-(0.5*crcovid(IREGN, n)))
        enddo
        IREFF(N)  = IREFFHIST(IRHISTYEAR-1989)  !assumes efficiency stays constant throughout projection 
        IRRPM(N)  = IRPMPC(N)*MC_NP16A(11,N)
        IRED(N)   = IRRPM(N)*IREFF(N)/1000000.0
        IREDD(N)  = IRED(N)*IREDDSHR
        IREDE(N)  = IRED(N)-IREDD(N)
      ENDIF

      IREDDR(11,N) = 0.0
      IREDER(11,N) = 0.0
      DO IREGN=1,MNUMCR-2
        IREDDR(IREGN,N) = IREDD(N)*IRREGSHR(IREGN)
        IREDER(IREGN,N) = IREDE(N)*IRREGSHR(IREGN)
        IREDDR(11,N)    = IREDDR(11,N)+IREDDR(IREGN,N)
        IREDER(11,N)    = IREDER(11,N)+IREDER(IREGN,N)
      ENDDO
       
!...Sum mass transit rail by fuel by mode
    DO IREGN=1,MNUMCR 
      DO IFUELX=1,2
        IF(IFUELX.EQ.1) THEN    ! diesel
          QMTRR(IFUELX,IREGN,N) = CREDD(IREGN,N)+IREDDR(IREGN,N)
        ELSE                    ! electricity
          QMTRR(IFUELX,IREGN,N) = TRED(IREGN,N)+CREDE(IREGN,N)+IREDER(IREGN,N)
        ENDIF
      ENDDO
      qmtrr(3,iregn,n) = 0.0 ! CNG
      qmtrr(4,iregn,n) = 0.0 ! LNG  
    ENDDO
                     
!...Calculate bus segments
!... bus type 
!...  im=1: transit buses (not currently used- calculated seperately)
!...  im=2: intercity buses
!...  im=3: school buses
!... fuel type:             
!...  ifuelx=1: gasoline 
!...  ifuelx=2: diesel 
!...  ifuelx=3: ethanol 
!...  ifuelx=4: methanol (not used)
!...  ifuelx=5: CNG 
!...  ifuelx=6: LPG 
!...  ifuelx=7: electricity 
!...  ifuelx=8: hydrogen 

!...transit bus passenger miles traveled by Census Division
!...read in historic travel
    do iregn=1,mnumcr-2
      if(curcalyr.le.TBHISTYEAR) TBPMT(iregn,n)=TBPMTHIST(iregn,n)
      if(curcalyr.eq.TBHISTYEAR) TBPMTPC(iregn,n)=TBPMTPC08(iregn)
!...  project travel
      if(curcalyr.gt.TBHISTYEAR) then
        TBPMTPCEXP(iregn,n) =(tb_coef(iregn,1)+(log(MC_GDPR(n)/MC_NP16A(11,n))*tb_coef(iregn,2)+log(pmgtr19_D_(n))*tb_coef(iregn,3)+tbcovid(iregn, n)*tb_coef(iregn,4)))
		TBPMTPC(iregn,n) = exp(TBPMTPCEXP(iregn,n))
	    TBPMT(iregn,n)=TBPMTPC(iregn,n)*nonfarmemp(iregn,n)
      endif
    enddo
	
!...adjust transit bus pmt to account for growth in CAV travel 
	do iregn=1,mnumcr-2
		SHR_TBPMT(iregn) = TBPMT(iregn,n)/sum(TBPMT(1:mnumcr-2,n))
	enddo
	do iregn=1,mnumcr-2
		TBPMT(iregn,n) = TBPMT(iregn,n)-(sum(FLTVMTECH(1:maxvtyp,4,1:maxldv,2:maxhav))*TB_CAV_ADJ(n)*SHR_TBPMT(iregn)/1000000.0)
	enddo
    TBPMT(11,n)=sum(TBPMT(1:mnumcr-2,n))
	
!...transit bus share of fuel demand
    do iregn=1,mnumcr-2
      do ifuelx=1,8 
        TBFSHR(iregn,ifuelx,n) = TBFSHRHIST(ifuelx,n,iregn)
      enddo
    enddo
		
!...check validity of parens on TBBTUPM equation - JDM
!...transit bus efficiency Btu/passenger mile traveled
!...Needs to updated with current in yr 
    do iregn=1,mnumcr-2
      if(curcalyr.le.TBHISTYEAR)then
        TBBTUPM(iregn,n)=TBBTUPMHIST(iregn,n)
      else
		TBBTUPM_ADJ(iregn) = tbbtupm(iregn,33) - tbbtupm(iregn,30) !MMA - TBBTUPM_ADJ(iregn) = tbbtupm(iregn,tbhistyear-1989) - tbbtupm(iregn,tbhistyear-1990)
		if(tbpmt(iregn,n).lt.tbpmt(iregn,30)) then
          Tbbtupm(IREGN,N) = Tbbtupmhist(IREGN,33) - (tbbtupm_adj(iregn) * (tbpmt(iregn,n)/tbpmt(iregn,30))) 
		else
		  TBBTUPM(iregn,n)=TBBTUPM(iregn,30)*TBSYSEFF(iregn)* &
                         1-((1-(TFR_FTMPG_S(IY-1,3,2)/TFR_FTMPG_S(IY,3,2)) * TBFSHR(iregn,2,n))) * & 
                         1+((TBFSHR(iregn,5,n)-TBFSHR(iregn,5,n-1))*0.25)   
        endif						 
      endif
    enddo
    
!...transit bus energy demand
    do ifuelx=1,8 !
      do iregn=1,mnumcr-2
        QMTBR(1,ifuelx,iregn,n) = ((TBPMT(iregn,n)*TBBTUPM(iregn,n))/1000000.0)*TBFSHR(iregn,ifuelx,n)
      enddo
      QMTBR(1,ifuelx,11,n) = sum(QMTBR(1,ifuelx,1:mnumcr-2,n))
  
    enddo
!...annual intercity and school bus passenger miles (millions)
    do im=1,2
      if(curcalyr.le.IBSBHISTYEAR)then                    
        TMOD(im,n) = TMODINIT(im,n)       
      else
	    if(im.eq.1) TMOD(im,n) = (365.7535+(0.2519 * (MC_GDPR(n)/MC_NP16A(11,n)))) * MC_NP16A(11,n) * (1.-(0.375 * TMCOVID(im,n))) !intercity grows with adult population
		if(im.eq.2) TMOD(im,n) = (18653.33+(curcalyr*-8.57)) * (MC_NP(11,n)-MC_NP16A(11,n)) * (1.-(0.65 * TMCOVID(im,n)))  !school bus grows with child population
      endif
    enddo

!...share of travel demand by intercity and school bus
!...  1) Gasoline 2) Diesel 3) E85      4) Methanol 
!...  5) CNG      6) LPG    7) Electric 8) Hydrogen
    do im=1,2
      do ifuelx=1,8
        if(curcalyr.le.IBSBHISTYEAR) then 
          QMODFSHR(im,ifuelx,n) = QMODFSHRH(ifuelx,n,im)
        else
!...      maintain fuel shares at historic levels
          QMODFSHR(im,ifuelx,n) = QMODFSHR(im,ifuelx,n-1)
        endif
      enddo
    enddo

!...historic school bus shares by region
	do iregn=1, mnumcr-2
	  do ifuelx=1,8
	    schbus_pmt_shr(ifuelx,iregn,n) = QMODFSHR(2,ifuelx,n)	    
	  enddo
	enddo
    
!...project pmt shares for school buses: maintain LPG and gasoline shares at historic levels
!...allow CNG share to increase based on ratio of diesel to CNG fuel price. 
!...if increase in CNG share reduces diesel below 5 percent, then maintain diesel share at 5 percent
!...increase in CNG share
    if(curcalyr.gt.IBSBHISTYEAR)then   
      QMODFSHR(2,5,n) = QMODFSHR(2,5,n-1) + ((HWYPDSTR(11,n)/PGFTRFV(11,n)-1.0)*0.0005) !CNG
      QMODFSHR(2,8,n) = QMODFSHR(2,8,n-1) !H2
   	endif

!...regional share by fuel 
    if(curcalyr.ge.2014)then ! account for EV adjustment
	  do iregn=1,mnumcr-2
        TEMP = SUM(schbus_pmt_shr([1,2,6],iregn,n))     ! grab MG/DS/LPG fuel shares so BEV/CNG can be subtracted off proportionally
	    do ifuelx=1,8
	      if(ifuelx.eq.7) schbus_pmt_shr(ifuelx,iregn,n) = schbus_ev_shr(iregn,n)
		enddo  
!...    normalize shares to 1: subtract cng and ev from diesel
		if(curcalyr.le.IBSBHISTYEAR) then
		  schbus_pmt_shr(1,iregn,n) = schbus_pmt_shr(1,iregn,n)-schbus_pmt_shr(7,iregn,n)*schbus_pmt_shr(1,iregn,n)/TEMP        ! Gasoline
          schbus_pmt_shr(2,iregn,n) = schbus_pmt_shr(2,iregn,n)-schbus_pmt_shr(7,iregn,n)*schbus_pmt_shr(2,iregn,n)/TEMP        ! Diesel
		  schbus_pmt_shr(6,iregn,n) = schbus_pmt_shr(6,iregn,n)-schbus_pmt_shr(7,iregn,n)*schbus_pmt_shr(6,iregn,n)/TEMP        ! LPG
		else
		  schbus_pmt_shr(1,iregn,n) = schbus_pmt_shr(1,iregn,n)-(schbus_pmt_shr(7,iregn,n)+schbus_pmt_shr(5,iregn,n))*schbus_pmt_shr(1,iregn,n)/TEMP        ! Gasoline
          schbus_pmt_shr(2,iregn,n) = schbus_pmt_shr(2,iregn,n)-(schbus_pmt_shr(7,iregn,n)+schbus_pmt_shr(5,iregn,n))*schbus_pmt_shr(2,iregn,n)/TEMP        ! Diesel
		  schbus_pmt_shr(6,iregn,n) = schbus_pmt_shr(6,iregn,n)-(schbus_pmt_shr(7,iregn,n)+schbus_pmt_shr(5,iregn,n))*schbus_pmt_shr(6,iregn,n)/TEMP        ! LPG
    	endif
!...    check diesel share
	    if(schbus_pmt_shr(2,iregn,n).lt.0.01) then
		  schbus_adj(iregn,n) = schbus_pmt_shr(2,iregn,n)+schbus_pmt_shr(7,iregn,n)+schbus_pmt_shr(5,iregn,n)   ! total of shares that are changing
		  schbus_pmt_shr(2,iregn,n) = 0.01                                                                      ! reset diesel share
		  schbus_pmt_shr(5,iregn,n) = schbus_adj(iregn,n)-schbus_pmt_shr(7,iregn,n)-schbus_pmt_shr(2,iregn,n)   ! recalculate CNG share
		endif
	  enddo
	endif

!...Regionalize intercity/commuter bus travel demand by regional population
    do iregn=1,mnumcr-2
      TMODRSHR(iregn,n) = MC_NP(iregn,n) / MC_NP(11,n)
    enddo		

!...Regionalize pass-miles to account for regional vmt shares and fuel shares
	do ifuelx=1,8
      do iregn=1,mnumcr-2
	    TMOD_R(1,ifuelx,iregn,n) = TMOD(1,n)*TMODRSHR(iregn,n)*QMODFSHR(1,ifuelx,n)            ! intercity
	    TMOD_R(2,ifuelx,iregn,n) = TMOD(2,n)*SchBus_Shr(iregn)*schbus_pmt_shr(ifuelx,iregn,n)  !school bus
	  enddo
	enddo    
	
!...Calculate intercity and school bus efficiency in BTU/Passenger Miles:
    BUSSYSEF(1) = 1.007
    BUSSYSEF(2) = 1.001
    TMEFF_F = 0.0
    do im=1,2
      if(curcalyr.le.IBSBHISTYEAR)then
        TMEFF(im,n) = TMEFFINIT(im,n)
      else
	    TMEFF(im,n) = TMEFF(im,n-1) / bussysef(im)
	  endif
	  do ifuelx=1,8
	    if(im.eq.1.and.ifuelx.eq.2) TMEFF_F(im,ifuelx,n) = TMEFF(im,n)/1000000      ! diesel only for intercity
		if(im.eq.2) TMEFF_F(im,ifuelx,n) = (TMEFF(im,n) * eff_adj(ifuelx))/1000000  ! adjust efficiency by fuel type 
      enddo
    enddo
	
!...Calculate intercity and school bus energy demand by region and fuel
	do im=1,2
	  do iregn=1,MNUMCR-2
	    do ifuelx=1,8
		  QMTBR(im+1,ifuelx,iregn,n) = TMOD_R(im,ifuelx,iregn,n)*TMEFF_F(im,ifuelx,n)
		enddo
	  enddo
	enddo
!...national total
	do im=1,2
	  do ifuelx=1,8
	    QMTBR(im+1,ifuelx,mnumcr,n) = sum(QMTBR(im+1,ifuelx,1:mnumcr-2,n))
	  enddo
	enddo
 
! ... Calculate recreational boating fuel use 
! ...    1 = gasoline
! ...    2 = diesel
! ... Historic energy demand 
      DO IFUELX=1,2
        IF(curcalyr.LE.RBHistYr) THEN
          RBEDPC(IFUELX,N) = RECFDH(IFUELX,N)
        ELSE
! ... Calculate demand growth
		  boats = (MC_GDPR(n)/MC_NP16A(11,n)) * (240.42*exp(-0.012*n))
          IF(IFUELX .EQ.1) THEN
			RBEDPC(ifuelx,n) = boats * (0.0181*exp(-0.0083*n))  
          ELSE
			RBEDPC(ifuelx,n) = boats * 0.0033
          ENDIF
        ENDIF
        RECFD(IFUELX,N) = RBEDPC(IFUELX,N)
      ENDDO

! ... Regionalize recreational boat demand by population
      DO IFUELX=1,2
      QRECR(IFUELX,11,N) = 0.0
        DO IREGN=1,MNUMCR-2
          QRECR(IFUELX,IREGN,N) = RECFD(IFUELX,N) * TMODRSHR(IREGN,N)
          QRECR(IFUELX,11,N) = QRECR(IFUELX,11,N) + QRECR(IFUELX,IREGN,N)
        ENDDO
      ENDDO

! ... Sum freight VMT across 3 size classes
      FTVMT(N) = VMT_TR(N)
	  HYWAY(N) = sum(VMTHH(n,mnumcr,1:maxldv,1:maxvtyp)) + FTVMT(N) + sum(FLTVMTECH(1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav))

! ... Calculate lubricant demand
      BETALUB = 0.25
! ... Read historic values
      if(curcalyr.le.LubHistYr)then
        lubfd(n)= lubfdh(n)
      else
! ... Project growth based on total VMT 
        LUBFD(N) = (LUBFD(N-1) * ((HYWAY(N)/HYWAY(N-1))**BETALUB))
      endif

! ... Regionalize lubrication demand by summing VMT shares for freight and light duty vehicles
      QLUTR(11,N) = 0.0
      DO IREGN=1,MNUMCR-2
		LUBRSHR(IREGN,N) = (sum(VMTHH(N,mnumcr,1:maxldv,1:maxvtyp))  * SEDSHRMG(IREGN,N) +  &
                            sum(FLTVMTECH(1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav)) * SEDSHRMG(IREGN,N) +  &
                            FTVMT(N)  * SEDSHRDS(IREGN,N)) / &
                            HYWAY(N)
        QLUTR(IREGN,N) = LUBFD(N) * LUBRSHR(IREGN,N)
        QLUTR(11,N) = QLUTR(11,N) + QLUTR(IREGN,N)
      ENDDO

    RETURN
    END SUBROUTINE TMISC

! ==========================================================================================================
! Subroutine TCONS combines VMT and efficiencies by technology to estimate fuel consumption for light 
! duty vehicles by fuel type (household)
! 
! Note: indices for IFUELX
!       IFUELX=1 :M85 FLEX  
!       IFUELX=2 :E85 FLEX   
!       IFUELX=3 :CNG BIFUEL 
!       IFUELX=4 :LPG BIFUEL 
! Indicies for MPGTECH = ILDV
! ==========================================================================================================
    SUBROUTINE TCONS
    USE T_
    IMPLICIT NONE
!... will need to regionalize mpgtech for fuel economy estimation
      DO ILDV=1,MAXLDV
        IF (MPGTECH(ILDV,N) .EQ. 0.0) then
          if (ildv.ge.9.and.ildv.le.12) then
            MPGTECH(ILDV,N) = MPGTECH(1,N) * 0.95
          else
            MPGTECH(ILDV,N) = 1.0
          endif
        endif
      ENDDO

!...calculate household gge consumption and btu	
	hhtechgge=0.0
    do ivtyp=1,maxvtyp
      do ildv=1,maxldv
		do iregn=1,mnumcr-2
		  if(iregn.ne.10) then
			do iage=1,maxage
              if(hhmpgstk(iregn,ivtyp,ildv,iage,n).ne.0.0) then
			    hhtechgge(iregn,ivtyp,ildv,iage,n) = hhtechvmt(iregn,ivtyp,ildv,iage)/hhmpgstk(iregn,ivtyp,ildv,iage,n)
			  endif 
			enddo
			hhtechbtu(iregn,ivtyp,ildv,n) = sum(hhtechgge(iregn,ivtyp,ildv,1:maxage,n)) * MG_HHV/1000.0
		  endif
		enddo
		hhtechbtu(mnumcr,ivtyp,ildv,n) = sum(hhtechbtu(1:mnumcr-2,ivtyp,ildv,n))
      enddo
    enddo

!...Calculate consumption by fuel
    DO IFUELX=1,8
      DO IREGN=1,MNUMCR
!...    Gasoline
        IF(IFUELX .EQ. 1) THEN
          TQLDV(1,IREGN,N) =   sum(hhtechbtu(iregn,1:maxvtyp, 1,n)) + &
							  (sum(hhtechbtu(iregn,1:maxvtyp, 5,n)) * (1.0-PctPHEV20(n))) + &
							  (sum(hhtechbtu(iregn,1:maxvtyp, 6,n)) * (1.0-PctPHEV50(N))) + &
							  (sum(hhtechbtu(iregn,1:maxvtyp, 2,n)) * (1.0-PCTAF(2,IREGN,N)))   + &
							  (sum(hhtechbtu(iregn,1:maxvtyp,10,n)) * (1.0-PCTAF(4,IREGN,N)))   + &
							  (sum(hhtechbtu(iregn,1:maxvtyp, 9,n)) * (1.0-PCTAF(3,IREGN,N)))   + &
							   sum(hhtechbtu(iregn,1:maxvtyp,16,n)) 
!...	Methanol
        ELSEIF(IFUELX .EQ. 2) THEN
          TQLDV(2,IREGN,N) =   sum(hhtechbtu(iregn,1:maxvtyp,13,n)) 
!...  	Ethanol
        ELSEIF(IFUELX .EQ. 3) THEN
          TQLDV(3,IREGN,N) =   sum(hhtechbtu(iregn,1:maxvtyp, 3,n)) * PCTAF(2,IREGN,N)
!...	CNG
        ELSEIF(IFUELX .EQ. 4) THEN
          TQLDV(4,IREGN,N) =   sum(hhtechbtu(iregn,1:maxvtyp,11,n)) + &
                              (sum(hhtechbtu(iregn,1:maxvtyp, 9,n)) * PCTAF(3,IREGN,N))
!...  	LPG
        ELSEIF(IFUELX .EQ. 5) THEN
          TQLDV(5,IREGN,N) =   sum(hhtechbtu(iregn,1:maxvtyp,12,n)) + &
                              (sum(hhtechbtu(iregn,1:maxvtyp,10,n)) * PCTAF(4,IREGN,N))
!...	Electricity
        ELSEIF(IFUELX .EQ. 6) THEN
          TQLDV(6,IREGN,N) =   sum(hhtechbtu(iregn,1:maxvtyp, 4,n)) + & 
							   sum(hhtechbtu(iregn,1:maxvtyp, 7,n)) + &
							   sum(hhtechbtu(iregn,1:maxvtyp,15,n)) + &
							  (sum(hhtechbtu(iregn,1:maxvtyp, 5,n)) * PctPHEV20(n)) + &
							  (sum(hhtechbtu(iregn,1:maxvtyp, 6,n)) * PctPHEV50(n))
!...  	Hydrogen
        ELSEIF(IFUELX .EQ. 7) THEN
          TQLDV(7,IREGN,N) =   sum(hhtechbtu(iregn,1:maxvtyp,14,n)) 

!...	Diesel
        ELSEIF(IFUELX .EQ. 8) THEN
          TQLDV(8,IREGN,N) =   sum(hhtechbtu(iregn,1:maxvtyp, 2,n)) + &
							   sum(hhtechbtu(iregn,1:maxvtyp, 8,n)) 

        ENDIF
      ENDDO
    ENDDO

! ... Sum total consumption of all fuels into TQLDV(9)
      DO IREGN=1,MNUMCR
        TQLDV(9,IREGN,N) = SUM(TQLDV(1:8,IREGN,N))
      ENDDO

    RETURN
    END SUBROUTINE TCONS

! ==========================================================================================================
! ... Subroutine FLEXSHR calculates the VMT shares for flex- and bi-fuel vehicles. 
! ... 
! ...   Note:IFUELX indices
! ...        1=M85 flex
! ...        2=E85 flex
! ...        3=CNG bifuel
! ...        4=LPG bifuel
! ==========================================================================================================
    SUBROUTINE FLEXSHR
    USE T_
    IMPLICIT NONE

      INCLUDE 'ANGTDM'

      REAL     PRIRATIO(4,MNUMCR),PCTFLOOR(4),PCTCEILING(4),BETAP(4),GAMMAP, &
               PCTAFL(4,MNUMCR,MNUMYR),total_mkt20,total_mkt50,     &
               PCTFUEL_D_(4,MNUMCR,MNUMYR),PCTFAVL(4,MNUMCR,MNUMYR),ETHPRR
      REAL*8   ALTPRR(4,MNUMCR),GASPRR(MNUMCR),DOUBLE_PCTAFL
               
! ... Set parameters for minimum alternative fuel use in flex and bi fuel vehicles
      PCTFLOOR(1)   =  0.00000001
      PCTFLOOR(2)   =  0.001
      PCTFLOOR(3)   =  0.20
      PCTFLOOR(4)   =  0.20

      PCTCEILING(1) =  0.00000001
      PCTCEILING(2) =  0.70
      PCTCEILING(3) =  0.70
      PCTCEILING(4) =  0.30

      GAMMAP  = -7.0

      IF (N .LE. 13) THEN
        BETAP(1)   =  0.10
        BETAP(2)   =  0.01
        BETAP(3)   =  0.50
        BETAP(4)   =  0.30
      ELSE
        BETAP(2)   =  0.01
        BETAP(3)   =  0.70
        BETAP(4)   =  0.30
      ENDIF

! ... Calculate an arithmetic average methanol price for $0.00 price regions
      IF(PMETR(MNUMCR,N).EQ.0) THEN
        IF(QMETR(MNUMCR,N).GT.0.) THEN
          DO IREGN=1,MNUMCR-2
            PMETR(MNUMCR,N)=QMETR(IREGN,N)*PMETR(IREGN,N)
          ENDDO
          PMETR(MNUMCR,N)=PMETR(MNUMCR,N)/QMETR(MNUMCR,N)
        ELSE
          PMETR(MNUMCR,N)=SUM(PMETR(1:MNUMCR-2,N))/(MNUMCR-2)   
        ENDIF
      ENDIF

! ... Fill alternative fuel price array
      DO IFUELX=1,4
        DO IREGN=1,MNUMCR-2
          IF(IFUELX .EQ. 1) THEN
            PCTFUEL_D_(IFUELX,IREGN,N) = PMETR(IREGN,N)  ! methanol
          ELSEIF(IFUELX .EQ. 2)THEN
            PCTFUEL_D_(IFUELX,IREGN,N) = PETTR(IREGN,N)  ! ethanol
          ELSEIF(IFUELX .EQ. 3) THEN
            PCTFUEL_D_(IFUELX,IREGN,N) = PGFTRPV(IREGN,N)! CNG non-central fuel price
          ELSEIF(IFUELX .EQ. 4) THEN
            PCTFUEL_D_(IFUELX,IREGN,N) = PLGTR(IREGN,N)  ! LPG
          ENDIF
        ENDDO
      ENDDO

! ... Calculate regional price ratio for minimum alternative fuel use
      DO IFUELX=1,4
        DO IREGN=1,MNUMCR-2
          PRIRATIO(IFUELX,IREGN) = (PCTFUEL_D_(IFUELX,IREGN,N)**GAMMAP) / &
                                   ((PCTFUEL_D_(IFUELX,IREGN,N)**GAMMAP) + (PMGTR(IREGN,N)**GAMMAP))
        ENDDO
      ENDDO

! ... Calculate minimum alternative use
      DO IFUELX=1,4
        DO IREGN=1,MNUMCR-2
          IF (PRIRATIO(IFUELX,IREGN) .GT. 0.50) THEN 
              PRIRATIO(IFUELX,IREGN) = PCTCEILING(IFUELX)
          ENDIF     
          PCTAF(IFUELX,IREGN,N) = PCTFLOOR(IFUELX) + (BETAP(IFUELX)*PRIRATIO(IFUELX,IREGN))
          IF (PCTAF(IFUELX,IREGN,N) .GT. PCTCEILING(IFUELX)) THEN
              PCTAF(IFUELX,IREGN,N) = PCTCEILING(IFUELX)
          ENDIF
        ENDDO
      ENDDO

! ... Fill alternative fuel station availability array
      DO IFUELX=1,4
        DO IREGN=1,MNUMCR-2
          IF(IFUELX.EQ.1) PCTFAVL(IFUELX,IREGN,N) = 0.0000001
          IF(IFUELX .EQ. 2) THEN
            IF (curcalyr.le.2012) THEN
              IF(curcalyr.le.1995) THEN
                PCTFAVL(IFUELX,IREGN,N) = INITSTA(3,6,iregn)/INITSTA(1,6,iregn)  ! ethanol
              ELSE
!                PCTFAVL(IFUELX,IREGN,N) = INITSTA(3,N-5,iregn)/INITSTA(1,N-5,iregn)  ! ethanol
                PCTFAVL(IFUELX,IREGN,N) = INITSTA(3,N,iregn)/INITSTA(1,N,iregn)  ! ethanol
              ENDIF
              E85AVAIL(IREGN,N) = PCTFAVL(IFUELX,IREGN,N)
            ELSE
              PCTFAVL(IFUELX,IREGN,N) = E85AVAIL(IREGN,N) ! ethanol
            END IF
          ELSEIF(IFUELX .EQ. 3) THEN
            PCTFAVL(IFUELX,IREGN,N) = FAVL(5,IREGN,YRS)  ! CNG
          ELSEIF(IFUELX .EQ. 4) THEN
            PCTFAVL(IFUELX,IREGN,N) = FAVL(6,IREGN,YRS)  ! LPG
          ENDIF
        ENDDO
      ENDDO

! ... Pass regional E-85 fuel availability to PMM
      E85AVAIL(11,N) = 0.0
      DO IREGN=1, MNUMCR-2
        E85AVAIL(11,N)    = E85AVAIL(11,N) + (E85AVAIL(IREGN,N) * RSHR(IREGN,N))
      ENDDO

! ... Alternative fuel choice logit

      FCLogit0(1) = -1.2501 !-1.385
      FCLogit0(2) = -1.6998 !-1.938
      FCLogit0(3) = -2.1780 !-2.352
      FCLogit0(4) = -2.0684 !-2.006
      FCLogit0(5) = -0.8035 !-0.693
      FCLogit0(6) = -0.7942 !-0.735
      FCLogit0(7) = -0.9550 !-0.983
      FCLogit0(8) = -1.3789 !-1.319
      FCLogit0(9) = -1.1009 !-1.148

      FCLOGIT1 = -2.2540  !-2.142
      FCLOGIT2 =  3.4444  ! 2.816
      FCLOGIT3 = -18.830  !-15.0
      FCLogit4 = -2.4921  !-2.398
     

      if(curcalyr.ge.2006)then
       DO IFUELX=1,4
         DO IREGN=1,MNUMCR-2
           ALTPRR(IFUELX,IREGN) = DEXP(DBLE(FClogit0(iregn))+(DBLE(PCTFUEL_D_(IFUELX,IREGN,N)* MG_HHV/1000.0)*DBLE(FCLOGIT1))- &
                          DBLE(FCLOGIT2)*(DEXP(DBLE(PCTFAVL(IFUELX,IREGN,N))*DBLE(FCLOGIT3))))
           GASPRR(IREGN) = DEXP(DBLE(PMGTR(IREGN,N)* MG_HHV/1000.0)*DBLE(FCLogit4))          
           DOUBLE_PCTAFL = ALTPRR(IFUELX,IREGN)/(ALTPRR(IFUELX,IREGN) + GASPRR(IREGN))
           PCTAFL(IFUELX,IREGN,N) = SNGL(DOUBLE_PCTAFL)
         ENDDO
       ENDDO
      endif

! ... Set max penetration greater of minimum or logit
       DO IFUELX=1,4
         DO IREGN=1,MNUMCR-2
           PCTAF(IFUELX,IREGN,N) = MAX (PCTAF(IFUELX,IREGN,N),PCTAFL(IFUELX,IREGN,N))
           IF(IFUELX .EQ. 1) PCTAF(IFUELX,IREGN,N) = 0.00000001
          ENDDO
        ENDDO

! ... National average alternative fuel use percentage for flex and bi fuel vehicles
      DO IFUELX=1,4
        PCTAF(IFUELX,11,N) = 0.0
        DO IREGN=1,MNUMCR-2
           PCTAF(IFUELX,11,N) = PCTAF(IFUELX,11,N) + (PCTAF(IFUELX,IREGN,N)*RSHR(IREGN,N))
		   !dst this is the first usage of RSHR - to calculate vmt weighted national fuel use percentage. should be ultimately removed
		   !with model regionalization (sales and stocks)
        ENDDO
      ENDDO
	  
    RETURN
    END SUBROUTINE FLEXSHR

! ==========================================================================================================
! ... Subroutine TINTEG calculates total transportation energy use by fuel type  
! ==========================================================================================================
    SUBROUTINE TINTEG
    USE T_
    IMPLICIT NONE

! ... Calculate total consumption from all modes
      DO IREGN=1,MNUMCR
! ...   gasoline
        QMGTR(IREGN,N) = TQLDV(1,IREGN,N)      + &                         ! hhldv
                         FLTFUELBTU(iregn,1,n) + &                         ! ldv fleet 
                         cltfbtu(n,1,iregn)    + &                         ! commercial light truck						 
                         SUM(TFRBTU_F_T(N,1:3,2,IREGN)) + &                ! heavy truck (gasoline and gasoline HEV)
                         QMTBR(1,1,IREGN,N)    + &                         ! transit bus
                         QMTBR(2,1,IREGN,N)    + &                         ! intercity bus
                         QMTBR(3,1,IREGN,N)    + &                         ! school bus
                         QRECR(1,IREGN,N)                                  ! recreational boating
! ...   methanol
        QMETR(IREGN,N) = TQLDV(2,IREGN,N)      + &                         ! ldv
                         FLTFUELBTU(iregn,2,n)                             ! ldv fleet 
! ...   ethanol
        QETTR(IREGN,N) = TQLDV(3,IREGN,N)      + &                         ! ldv
                         FLTFUELBTU(iregn,3,n) + &                         ! ldv fleet
                         QMTBR(1,3,IREGN,N)    + &                         ! transit bus					 
                         cltfbtu(n,5,iregn)    + &                         ! commercial light truck
						 sum(TFRBTU_F_T(n,1:3,5,iregn))                    ! heavy truck
! ...   cng
        QNGTR(IREGN,N) = TQLDV(4,IREGN,N)      + &                         ! ldv
                         FLTFUELBTU(iregn,4,n) + &                         ! ldv fleet
						 cltfbtu(n,4,iregn)    + &                         ! commercial light truck						 
                         QMTBR(1,5,IREGN,N)    + &                         ! transit bus
                         QMTBR(2,5,IREGN,N)    + &                         ! intercity bus
                         QMTBR(3,5,IREGN,N)    + &                         ! school bus
                         TQISHIPR(3,IREGN,N)   + &                         ! international marine CNG                         
                         TQDSHIPR(3,IREGN,N)   + &                         ! domestic marine CNG       
                         TQRAILR(3,IREGN,N)    + &                         ! freight rail CNG         
                         TQISHIPR(4,IREGN,N)   + &                         ! international marine LNG                         
                         TQDSHIPR(4,IREGN,N)   + &                         ! domestic marine LNG       
                         TQRAILR(4,IREGN,N)    + &                         ! freight rail LNG          
                         QMTRR(3,IREGN,N)      + &                         ! passenger rail CNG       
                         QMTRR(4,IREGN,N)      + &                         ! passenger rail LNG       
                         SUM(TFRBTU_F_T(N,1:3,4,IREGN))                    ! heavy truck
! ... lpg
        QLGTR(IREGN,N) = TQLDV(5,IREGN,N)      + &                         ! ldv
                         FLTFUELBTU(iregn,5,n) + &                         ! ldv fleet
						 cltfbtu(n,3,iregn)    + &                         ! commercial light truck
                         QMTBR(1,6,IREGN,N)    + &                         ! transit bus
                         QMTBR(2,6,IREGN,N)    + &                         ! intercity bus
                         QMTBR(3,6,IREGN,N)    + &                         ! school bus
                         SUM(TFRBTU_F_T(N,1:3,3,IREGN))                    ! heavy truck
        QPRTR(IREGN,N) = QLGTR(IREGN,N)
        
! ... electricity
        QELTR(IREGN,N) = QMTRR(2,IREGN,N)                                  ! passenger rail
        
! ... hydrogen
        QH2TR(IREGN,N) = TQLDV(7,IREGN,N)      + &                         ! ldv
                         FLTFUELBTU(iregn,7,n) + &                         ! ldv fleet    
                         cltfbtu(n,7,iregn)    + &                         ! commercial light truck
                         QMTBR(1,8,IREGN,N)    + &                         ! transit bus
                         SUM(TFRBTU_F_T(N,1:3,7,IREGN))                    ! heavy truck
! ... diesel
        QDSTR(IREGN,N) = TQLDV(8,IREGN,N)      + &                         ! ldv
                         FLTFUELBTU(iregn,8,n) + &                         ! ldv fleet
                         cltfbtu(n,2,iregn)    + &                         ! commercial light truck
                         TQISHIPR(1,IREGN,N)   + &                         ! international marine
                         TQDSHIPR(1,IREGN,N)   + &                         ! domestic marine
                         QRECR(2,IREGN,N)      + &                         ! recreational boating
                         TQRAILR(1,IREGN,N)    + &                         ! freight rail
                         QMILTR(1,IREGN,N)     + &                         ! military
                         QMTBR(1,2,IREGN,N)    + &                         ! transit bus
                         QMTBR(2,2,IREGN,N)    + &                         ! intercity bus
                         QMTBR(3,2,IREGN,N)    + &                         ! school bus
                         QMTRR(1,IREGN,N)      + &                         ! passenger rail
                         SUM(TFRBTU_F_T(N,1:3,1,IREGN))                    ! heavy truck
! ... residual
        QRSTR(IREGN,N) = TQISHIPR(2,IREGN,N)   + &                         ! international marine
                         TQISHIPR(5,IREGN,N)   + &                         ! international marine (low sulfur)
                         TQDSHIPR(2,IREGN,N)   + &                         ! domestic marine
                         TQRAILR(2,IREGN,N)    + &                         ! freight rail
                         QMILTR(3,IREGN,N)                                 ! military
! ... jet fuel
        QJFTR(IREGN,N) = QJETR(IREGN,N)        + &                         ! commercial jet
                         QMILTR(2,IREGN,N)     + &                         ! military naphtha
                         QMILTR(4,IREGN,N)                                 ! military kerosene							 
! ... other
        QOTTR(IREGN,N) = QAGTR(IREGN,N)        + &                         ! avaition gasoline
                         QLUTR(IREGN,N)                                    ! lubricants


! ... Calculate percent of motor gasoline demand that is E10 only (MY2001 and earlier)
      if(iregn.ne.10) E10SHARE(iregn,n) = (QRECR(1,iregn,n)/(MG_HHV*1000.0)*1000000.0)  & ! recreational boats
                                         /((QMGTR(iregn,n)/(MG_HHV*1000.0))*1000000.0)     ! total transportation motor gasoline

      ENDDO

    RETURN
    END SUBROUTINE TINTEG


! ==========================================================================================================
! ... Subroutine TRANEASY For STEO easy button
! ==========================================================================================================
  SUBROUTINE TRANEASY
    IMPLICIT NONE 
    INCLUDE 'PARAMETR'
    INCLUDE 'STEOBLOCK'
    INCLUDE 'CONVFACT'
    
    common/tran_easy/tmgtcbus,tdstcpus,tjftcbus,trftcbus
	
    real tmgtcbus(mnumyr),tjftcbus(mnumyr),tdstcpus(mnumyr),trftcbus(mnumyr)

    tmgtcbus=mgtcbus
    tjftcbus=jftcbus/jftczus*cfjfq
    tdstcpus=datcpus-dfepdel
    trftcbus=rftcbus

  RETURN
  END SUBROUTINE TRANEASY
	
! ==========================================================================================================
! ... Subroutine TBENCHMARK benchmarks short term projections (2 year) to SEDS and the STEO 
! ==========================================================================================================
  SUBROUTINE TBENCHMARK
  USE T_
  IMPLICIT NONE

    INTEGER STEOBM,icheck,yseds
    REAL   SUMCAR,SUMLTT,BMFAC
    REAL   bm_mer(11,4) ! Benchmark factors for last history year from the MER.
    REAL   bm_steo(11,4) ! Benchmark factors for STEO year
	REAL   QELTR_B(mnumcr,mnumyr), QNGTR_B(mnumcr,mnumyr), QMGTR_B(mnumcr,mnumyr), QJFTR_B(mnumcr,mnumyr), &
           QDSTR_B(mnumcr,mnumyr), QLGTR_B(mnumcr,mnumyr), QPRTR_B(mnumcr,mnumyr), QRSTR_B(mnumcr,mnumyr), &
           QOTTR_B(mnumcr,mnumyr), QMETR_B(mnumcr,mnumyr), QETTR_B(mnumcr,mnumyr), QHYTR_B(mnumcr,mnumyr), &
           QAGTR_B(mnumcr,mnumyr), QLUTR_B(mnumcr,mnumyr)
	REAL   BEN_ME, BEN_ET, BEN_NG, BEN_LG, BEN_EL, BEN_HY
    REAL   MER_STEO_regn(MNUMCR,4)
	REAL   STMGTR_new(mnumcr,mnumyr), STJFTR_new(mnumcr,mnumyr), STDSTR_new(mnumcr,mnumyr), STRSTR_new(mnumcr,mnumyr)
    REAL   BCLTBTU(12,MNUMCR,MNUMYR)
	
!...Benchmark factors are calculated using SEDS data for         
    yseds=baseyr+msedyr-1  ! calendar year for last SEDS data

    STEOBM = RTOVALUE('STEOBM  ',0)
    call traneasy
    
!   Initialize all benchmark factors to 1.0    
    BENEL(:,n)=1.0;BENNG(:,n)=1.0;BENMG(:,n)=1.0;BENJF(:,n)=1.0;BENDS(:,n)=1.0;BENRS(:,n)=1.0;&
    BENLG(:,N)=1.0;BENOT(:,n)=1.0;BENME(:,n)=1.0;BENET(:,n)=1.0;BENHY(:,n)=1.0
    BEN_MG=1.0;BEN_JF=1.0;BEN_DS=1.0;BEN_RS=1.0;BEN_ME=1.0;BEN_ET=1.0;BEN_NG=1.0;BEN_LG=1.0;BEN_EL=1.0;BEN_HY=1.0

!   Initialize Balance Sector variables
    QMGBS(:,n)=0.0
    QDSBS(:,n)=0.0
    QJFBS(:,n)=0.0

!    Debug SEDS val read-in
!    if(curcalyr.eq.yseds) then
!      WRITE(21,'(a,",",i4,11(",",f12.1))')'QSMGTR',curcalyr,QSMGTR(:,n)
!      WRITE(21,'(a,",",i4,11(",",f12.1))')'QSJFTR',curcalyr,QSJFTR(:,n)
!      WRITE(21,'(a,",",i4,11(",",f12.1))')'QSDSTR',curcalyr,QSDSTR(:,n)
!      WRITE(21,'(a,",",i4,11(",",f12.1))')'QSRSTR',curcalyr,QSRSTR(:,n)
!    endif

    do iregn=1,mnumcr

!...  Benchmark to SEDS 
      if(CURCALYR.le.yseds)then 
        IF (QMGTR(iregn,n) .NE. 0.0) BENMG(iregn,n) = QSMGTR(iregn,n) / QMGTR(iregn,n)
        IF (QJFTR(iregn,n) .NE. 0.0) BENJF(iregn,n) = QSJFTR(iregn,n) / QJFTR(iregn,n) 
        IF (QDSTR(iregn,n) .NE. 0.0) BENDS(iregn,n) = QSDSTR(iregn,n) / QDSTR(iregn,n)
        IF (QRSTR(iregn,n) .NE. 0.0) BENRS(iregn,n) = QSRSTR(iregn,n) / QRSTR(iregn,n)	

!       Determining SEDS consumption by region and fuel based on MER annual values		
		if(curcalyr.eq.yseds) then
          MER_STEO_regn(iregn,1) = QSMGTR(iregn,n) / QSMGTR(mnumcr,n) * MER_tran(1)     ! Gasoline
          MER_STEO_regn(iregn,2) = QSJFTR(iregn,n) / QSJFTR(mnumcr,n) * MER_tran(2)     ! Jet fuel
          MER_STEO_regn(iregn,3) = QSDSTR(iregn,n) / QSDSTR(mnumcr,n) * MER_tran(3)     ! Diesel
          MER_STEO_regn(iregn,4) = QSRSTR(iregn,n) / QSRSTR(mnumcr,n) * MER_tran(4)     ! Resid
!          WRITE(21,'(a11,2(",",i4),9(",",f12.1))')'mer_steo_1',curcalyr,curitr,MER_STEO_regn(iregn,:),MER_tran(:),QSMGTR(iregn,n)
		endif

!...  Benchmark to MER, STEO
      elseif(CURCALYR.ge.ymer.and.CURCALYR.le.ysteo)then
!       MER
        IF (curcalyr.eq.ymer) THEN
          STMGTR_new(iregn,n) = MER_STEO_regn(iregn,1)
		  STJFTR_new(iregn,n) = MER_STEO_regn(iregn,2)
		  STDSTR_new(iregn,n) = MER_STEO_regn(iregn,3)
		  STRSTR_new(iregn,n) = MER_STEO_regn(iregn,4)

!...    STEO
        ELSE
	      STMGTR_new(iregn,n) = STMGTR_new(iregn,n-1)*(TMGTCBUS(n)/TMGTCBUS(n-1))
		  STJFTR_new(iregn,n) = STJFTR_new(iregn,n-1)*(TJFTCBUS(n)/TJFTCBUS(n-1))
		  STDSTR_new(iregn,n) = STDSTR_new(iregn,n-1)*(TDSTCPUS(n)/TDSTCPUS(n-1))
		  STRSTR_new(iregn,n) = STRSTR_new(iregn,n-1)*(TRFTCBUS(n)/TRFTCBUS(n-1))			
		endif		  

        IF (QMGTR(iregn,n) .NE. 0.0) BENMG(iregn,n) = STMGTR_new(iregn,n) / QMGTR(iregn,n)
        IF (QJFTR(iregn,n) .NE. 0.0) BENJF(iregn,n) = STJFTR_new(iregn,n) / QJFTR(iregn,n) 
        IF (QDSTR(iregn,n) .NE. 0.0) BENDS(iregn,n) = STDSTR_new(iregn,n) / QDSTR(iregn,n)
        IF (QRSTR(iregn,n) .NE. 0.0) BENRS(iregn,n) = STRSTR_new(iregn,n) / QRSTR(iregn,n)	
          
        if(curcalyr.eq.ymer) then
          bm_mer(iregn,1)=BENMG(iregn,n)
          bm_mer(iregn,2)=BENJF(iregn,n)
          bm_mer(iregn,3)=BENDS(iregn,n)
          bm_mer(iregn,4)=BENRS(iregn,n)
        endif

        if(curcalyr.eq.ysteo) then
          bm_STEO(iregn,1)=BENMG(iregn,n)
          bm_STEO(iregn,2)=BENJF(iregn,n)
          bm_STEO(iregn,3)=BENDS(iregn,n)
          bm_STEO(iregn,4)=BENRS(iregn,n)
        endif

!...  phase out of STEO benchmark to MER benchmark
      elseif(CURCALYR.eq.ysteo+1) then
        if (abs(1.0-BM_MER(iregn,1)).lt.abs(1.0-BM_STEO(iregn,1))) then
          BENMG(iregn,n) = (BM_STEO(iregn,1)+BM_MER(iregn,1))/2.0
        else
          BENMG(iregn,n) = BM_STEO(iregn,1)
        endif
        if (abs(1.0-BM_MER(iregn,2)).lt.abs(1.0-BM_STEO(iregn,2))) then
          BENJF(iregn,n) = (BM_STEO(iregn,2)+BM_MER(iregn,2))/2.0   
        else
          BENJF(iregn,n) = BM_STEO(iregn,2)
        endif
        if (abs(1.0-BM_MER(iregn,3)).lt.abs(1.0-BM_STEO(iregn,3))) then
          BENDS(iregn,n) = (BM_STEO(iregn,3)+BM_MER(iregn,3))/2.0 
        else
          BENDS(iregn,n) = BM_STEO(iregn,3)
        endif
        if (abs(1.0-BM_MER(iregn,4)).lt.abs(1.0-BM_STEO(iregn,4))) then
          BENRS(iregn,n) = (BM_STEO(iregn,4)+BM_MER(iregn,4))/2.0
        else
          BENRS(iregn,n) = BM_STEO(iregn,4)
        endif
        
!...  Benchmark factors after STEO phaseout based on final MER benchmark
      elseif(curcalyr.ge.ysteo+2) then
        if (abs(1.0-BM_MER(iregn,1)).lt.abs(1.0-BM_STEO(iregn,1))) then
          BENMG(iregn,n) = BM_MER(iregn,1)
        else
          BENMG(iregn,n) = BM_STEO(iregn,1)
        endif
        if (abs(1.0-BM_MER(iregn,2)).lt.abs(1.0-BM_STEO(iregn,2))) then
          BENJF(iregn,n) = BM_MER(iregn,2)
        else
          BENJF(iregn,n) = BM_STEO(iregn,2)
        endif
        if (abs(1.0-BM_MER(iregn,3)).lt.abs(1.0-BM_STEO(iregn,3))) then
          BENDS(iregn,n) = BM_MER(iregn,3)
        else
          BENDS(iregn,n) = BM_STEO(iregn,3)
        endif
        if (abs(1.0-BM_MER(iregn,4)).lt.abs(1.0-BM_STEO(iregn,4))) then
          BENRS(iregn,n) = BM_MER(iregn,4)
        else
          BENRS(iregn,n) = BM_STEO(iregn,4)
        endif
      endif	!end year control
    enddo	!end regional loop
	 
!...Benchmarking transportation specific consumption variables
    DO IREGN=1,MNUMCR	  
      QELTR(IREGN,N) = QELTR(IREGN,N) * BENEL(IREGN,N)      ! electricity
      QNGTR(IREGN,N) = QNGTR(IREGN,N) * BENNG(IREGN,N)      ! cng
      QMGTR(IREGN,N) = QMGTR(IREGN,N) * BENMG(IREGN,N)      ! motor gasoline
      QJFTR(IREGN,N) = QJFTR(IREGN,N) * BENJF(IREGN,N)      ! jet fuel
      QDSTR(IREGN,N) = QDSTR(IREGN,N) * BENDS(IREGN,N)      ! distillate
      QLGTR(IREGN,N) = QLGTR(IREGN,N) * BENLG(IREGN,N)      ! lpg
      QPRTR(IREGN,N) = QLGTR(IREGN,N)
      QRSTR(IREGN,N) = QRSTR(IREGN,N) * BENRS(IREGN,N)      ! residual
      QOTTR(IREGN,N) = QOTTR(IREGN,N) * BENOT(IREGN,N)      ! aviation + lubricant
      QMETR(IREGN,N) = QMETR(IREGN,N) * BENME(IREGN,N)      ! methanol
      QETTR(IREGN,N) = QETTR(IREGN,N) * BENET(IREGN,N)      ! ethanol
      QH2TR(IREGN,N) = QH2TR(IREGN,N) * BENHY(IREGN,N)      ! liquid hydrogen
      QAGTR(IREGN,N) = QAGTR(IREGN,N) * BENOT(IREGN,N)      ! aviation gasoline
      QLUTR(IREGN,N) = QLUTR(IREGN,N) * BENOT(IREGN,N)      ! lubricants
!      WRITE(21,'(a11,3(",",i4),",",f12.1,3(",",f5.2))')'qmgtr_test',curcalyr,curitr,iregn,QMGTR(IREGN,N),BENMG(IREGN,N),BM_MER(iregn,1),BM_STEO(iregn,1)
    ENDDO
	 
!...Make sure the national benchmark factor is consistent with the regions
    IF (CURCALYR .GT. yseds) THEN
      BEN_MG = sum(BENMG(1:mnumcr-2,N)*QMGTR(1:mnumcr-2,N))/sum(QMGTR(1:mnumcr-2,N))
      BEN_JF = sum(BENJF(1:mnumcr-2,N)*QJFTR(1:mnumcr-2,N))/sum(QJFTR(1:mnumcr-2,N))
      BEN_DS = sum(BENDS(1:mnumcr-2,N)*QDSTR(1:mnumcr-2,N))/sum(QDSTR(1:mnumcr-2,N))
      BEN_RS = sum(BENRS(1:mnumcr-2,N)*QRSTR(1:mnumcr-2,N))/sum(QRSTR(1:mnumcr-2,N))	
    ELSE
      BEN_MG = BENMG(mnumcr,N)
      BEN_JF = BENJF(mnumcr,N)
      BEN_DS = BENDS(mnumcr,N)
	  BEN_RS = BENRS(mnumcr,N)
      BEN_ME = BENME(mnumcr,N)
      BEN_ET = BENET(mnumcr,N)
      BEN_NG = BENNG(mnumcr,N)
      BEN_LG = BENLG(mnumcr,N)
      BEN_EL = BENEL(mnumcr,N)
      BEN_HY = BENHY(mnumcr,N)
    ENDIF

!   Error catching
    DO iregn = 1, mnumcr
      IF (iregn.eq.10) CYCLE
!     Zeros
      IF (QMGTR(iregn,n).eq.0.0.or.QJFTR(iregn,n).eq.0.0.or.QDSTR(iregn,n).eq.0.0) THEN
        WRITE(*,*)'ERROR - TDM'
        WRITE(*,*)'See p1/TRNOUT.txt'
        WRITE(21,'(a5,3(",",a4),",",a5,8(",",a12))')'var','year','iter','regn','bench','STEOr_cur','MERr_prev','Qregn',&
                                                    'STEOnat_cur','STEOnat_prev','MER_STEO_reg','QSEDS','price'
        IF (QMGTR(iregn,n).eq.0.0) THEN
          WRITE(*,*)'QMGTR = 0.0 in ',curcalyr,' in region ',iregn,' in iteration ',curitr
          WRITE(21,'(a5,3(",",i4),",",f5.3,8(",",f12.1))')'QMGTR',curcalyr,curitr,iregn,BENMG(iregn,n),&
                                                           STMGTR_new(iregn,n),STMGTR_new(iregn,n-1),QMGTR(iregn,n),&
                                                           TMGTCBUS(n),TMGTCBUS(n-1),MER_STEO_regn(iregn,1),QSMGTR(iregn,yseds-1989),&
                                                           PMGTR(iregn,n)
        ENDIF
        IF (QJFTR(iregn,n).eq.0.0) THEN
          WRITE(*,*)'QJFTR = 0.0 in ',curcalyr,' in region ',iregn,' in iteration ',curitr
          WRITE(21,'(a5,3(",",i4),",",f5.3,8(",",f12.1))')'QJFTR',curcalyr,curitr,iregn,BENJF(iregn,n),&
                                                           STJFTR_new(iregn,n),STJFTR_new(iregn,n-1),QJFTR(iregn,n),&
                                                           TJFTCBUS(n),TJFTCBUS(n-1),MER_STEO_regn(iregn,2),QSJFTR(iregn,yseds-1989),&
                                                           PDSTR(iregn,n)
        ENDIF
        IF (QDSTR(iregn,n).eq.0.0) THEN
          WRITE(*,*)'QDSTR = 0.0 in ',curcalyr,' in region ',iregn,' in iteration ',curitr
          WRITE(21,'(a5,3(",",i4),",",f5.3,7(",",f12.1))')'QDSTR',curcalyr,curitr,iregn,BENDS(iregn,n),&
                                                           STDSTR_new(iregn,n),STDSTR_new(iregn,n-1),QDSTR(iregn,n),&
                                                           TDSTCPUS(n),TDSTCPUS(n-1),MER_STEO_regn(iregn,3),QSDSTR(iregn,yseds-1989)
        ENDIF        
      ENDIF
!     NaNs
      IF ((QMGTR(iregn,n)+QJFTR(iregn,n)+QDSTR(iregn,n)+QRSTR(iregn,n)).ne.(QMGTR(iregn,n)+QJFTR(iregn,n)+QDSTR(iregn,n)+QRSTR(iregn,n))) THEN
        WRITE(*,*)'ERROR - TDM'
        WRITE(*,*)'See p1/TRNOUT.txt'
        WRITE(21,'(a5,3(",",a4),",",a5,8(",",a12))')'var','year','iter','regn','bench','STEOr_cur','MERr_prev','Qregn',&
                                                    'STEOnat_cur','STEOnat_prev','MER_STEO_reg','QSEDS','price'
        IF (QMGTR(iregn,n).ne.QMGTR(iregn,n)) THEN
          WRITE(*,*)'QMGTR = NaN in ',curcalyr,' in region ',iregn,' in iteration ',curitr
          WRITE(21,'(a5,3(",",i4),",",f5.3,8(",",f12.1))')'QMGTR',curcalyr,curitr,iregn,BENMG(iregn,n),&
                                                           STMGTR_new(iregn,n),STMGTR_new(iregn,n-1),QMGTR(iregn,n),&
                                                           TMGTCBUS(n),TMGTCBUS(n-1),MER_STEO_regn(iregn,1),QSMGTR(iregn,yseds-1989),&
                                                           PMGTR(iregn,n)
          WRITE(21,'(a9,8(",",a12))')'QMGTR_det','LDVall','LDVflt','CLT','frt_trk','bus_transit','bus_intercity','bus_school','recboat'
          WRITE(21,'(a9,8(",",f12.2))')'QMGTR_det',TQLDV(1,IREGN,N),FLTFUELBTU(iregn,1,n),cltfbtu(n,1,iregn),&
                                                   SUM(TFRBTU_F_T(N,1:3,2,IREGN)),QMTBR(:,1,IREGN,N),QRECR(1,IREGN,N)
        ENDIF
        IF (QJFTR(iregn,n).ne.QJFTR(iregn,n)) THEN
          WRITE(*,*)'QJFTR = NaN in ',curcalyr,' in region ',iregn,' in iteration ',curitr
          WRITE(21,'(a5,3(",",i4),",",f5.3,7(",",f12.1))')'QJFTR',curcalyr,curitr,iregn,BENJF(iregn,n),&
                                                           STJFTR_new(iregn,n),STJFTR_new(iregn,n-1),QJFTR(iregn,n),&
                                                           TJFTCBUS(n),TJFTCBUS(n-1),MER_STEO_regn(iregn,2),QSJFTR(iregn,yseds-1989)
        ENDIF
        IF (QDSTR(iregn,n).ne.QDSTR(iregn,n)) THEN
          WRITE(*,*)'QDSTR = NaN in ',curcalyr,' in region ',iregn,' in iteration ',curitr
          WRITE(21,'(a5,3(",",i4),",",f5.3,8(",",f12.1))')'QDSTR',curcalyr,curitr,iregn,BENDS(iregn,n),&
                                                           STDSTR_new(iregn,n),STDSTR_new(iregn,n-1),QDSTR(iregn,n),&
                                                           TDSTCPUS(n),TDSTCPUS(n-1),MER_STEO_regn(iregn,3),QSDSTR(iregn,yseds-1989),&
                                                           PDSTR(iregn,n)
          WRITE(21,'(a9,11(",",a12))')'QDSTR_det','LDVall','LDVflt','CLT','frt_trk','bus_transit','bus_intercity','bus_school','recboat','int_mar','dom_mar','frt_rail'
          WRITE(21,'(a9,11(",",f12.2))')'QDSTR_det',TQLDV(8,IREGN,N),FLTFUELBTU(iregn,8,n),cltfbtu(n,2,iregn),&
                                                   SUM(TFRBTU_F_T(N,1:3,1,IREGN)),QMTBR(:,2,IREGN,N),QRECR(2,IREGN,N),TQISHIPR(1,IREGN,N),&
                                                   TQDSHIPR(1,IREGN,N),TQRAILR(1,IREGN,N)
        ENDIF        
        IF (QRSTR(iregn,n).ne.QRSTR(iregn,n)) THEN
          WRITE(*,*)'QRSTR = NaN in ',curcalyr,' in region ',iregn,' in iteration ',curitr
          WRITE(21,'(a5,3(",",i4),",",f5.3,7(",",f12.1))')'QRSTR',curcalyr,curitr,iregn,BENRS(iregn,n),&
                                                           STRSTR_new(iregn,n),STRSTR_new(iregn,n-1),QRSTR(iregn,n),&
                                                           TRFTCBUS(n),TRFTCBUS(n-1),MER_STEO_regn(iregn,4),QSRSTR(iregn,yseds-1989)
        ENDIF
        STOP
      ENDIF
    ENDDO

!   Fill the balance sector quantities that are substracted from associated rows in ftab Table 2.
!   Retain seds-based Census Division sharing in balance sector by factoring the US benchmark.
    if(curcalyr.gt.ymer) then
      QMGBS(1:mnumcr,N)=(QMGTR(mnumcr,N)-(QMGTR(mnumcr,N)/BENMG(mnumcr,N)))*(QMGTR(1:mnumcr,n)/qmgtr(mnumcr,n))
      QDSBS(1:mnumcr,N)=(QDSTR(mnumcr,N)-(QDSTR(mnumcr,N)/BENDS(mnumcr,N)))*(QDSTR(1:mnumcr,n)/QDSTR(mnumcr,n))
    endif   
    QJFBS(1:mnumcr,N)=(QJFTR(mnumcr,N)-(QJFTR(mnumcr,N)/BENJF(mnumcr,N)))*(QJFTR(1:mnumcr,n)/QJFTR(mnumcr,n))

! ... Write the benchmark factors and balance sector values to tranout.txt
    IF (N.eq.MNUMYR.and.FCRL.eq.1) THEN
	  WRITE(21,*)'============== BENCHMARK FACTORS =============='
	  WRITE(21,'(a4,4(",",a8))')'YEAR' , 'BENMG' , 'BENDS' , 'BENRS','BENEL'
	  DO icheck = 6, MNUMYR
	    WRITE(21,'(i4,4(",",f8.3))')icheck+1989,BENMG(11,icheck),BENDS(11,icheck),BENRS(11,icheck),BENEL(11,icheck)
	  ENDDO
	ENDIF

!   Some benchmark differences are accounted for in the fuel balance sector and will be
!   subtracted from the associated transportation consumption arrays in Table 2 of ftab.  So un-do
!   the benchmarking for these fuels as applied to the remaining mode consumption and travel arrays.
    if(curcalyr.gt.ymer) then
      BENMG(1:mnumcr,n)=1.0
      BENDS(1:mnumcr,n)=1.0
      BEN_MG=1.
      BEN_DS=1.
    endif
    BENJF(1:mnumcr,n)=1.0
    BEN_JF=1.

!...FFV total fuel demand by region passed to PMM
    do iregn=1,mnumcr
      QFFV(iregn,n) = (tqldv(3,iregn,n)+fltldvbtut(iregn,3,n))*(BEN_ET*PCTAF(2,iregn,n)+BEN_MG*(1.0-PCTAF(2,iregn,n)))					   

!.. Recalibrate QETTR based on benchmarked total FFV demand
      QETTR(IREGN,N) = QFFV(IREGN,N) * PCTAF(2,iregn,n)                 
    enddo

!... Benchmark commercial light truck consumption by powertrain
    do IREGN=1,MNUMCR-2
      BCLTBTU(1,IREGN,N)  = CLTBTUT(1,iregn,N) *BEN_MG
      BCLTBTU(2,IREGN,N)  = CLTBTUT(2,iregn,N) *BEN_DS
	  BCLTBTU(3,IREGN,N)  = CLTBTUT(3,iregn,N) *BEN_LG
	  BCLTBTU(4,IREGN,N)  = CLTBTUT(4,iregn,N) *BEN_NG
	  BCLTBTU(5,IREGN,N)  = CLTBTUT(5,iregn,N) *(BEN_ET*PCTAF(2,mnumcr,N)+BEN_MG*(1.0-PCTAF(2,mnumcr,N)))
	  BCLTBTU(6,IREGN,N)  = CLTBTUT(6,iregn,N) *BEN_EL
	  BCLTBTU(7,IREGN,N)  = CLTBTUT(7,iregn,N) *(BEN_EL*PctEVMT_PHEV(N,4,IREGN,1) + BEN_DS*(1.0 - PctEVMT_PHEV(N,4,IREGN,1)))
	  BCLTBTU(8,IREGN,N)  = CLTBTUT(8,iregn,N) *(BEN_EL*PctEVMT_PHEV(N,4,IREGN,2) + BEN_MG*(1.0 - PctEVMT_PHEV(N,4,IREGN,2))) 
	  BCLTBTU(9,IREGN,N)  = CLTBTUT(9,iregn,N) *BEN_HY
	  BCLTBTU(10,IREGN,N) = CLTBTUT(10,iregn,N)*BEN_HY
	  BCLTBTU(11,IREGN,N) = CLTBTUT(11,iregn,N)*BEN_MG
	  BCLTBTU(12,IREGN,N) = CLTBTUT(12,iregn,N)*BEN_HY
    enddo

    BCLTBTUT(1,N)  = sum(BCLTBTU(1,1:mnumcr-2,N))                      ! Gasoline
    BCLTBTUT(2,N)  = sum(BCLTBTU(2,1:mnumcr-2,N))                      ! Diesel
    BCLTBTUT(3,N)  = sum(BCLTBTU(3,1:mnumcr-2,N))                      ! LPG
    BCLTBTUT(4,N)  = sum(BCLTBTU(4,1:mnumcr-2,N))                      ! CNG
    BCLTBTUT(5,N)  = sum(BCLTBTU(5,1:mnumcr-2,N))                      ! Flex fuel
    BCLTBTUT(6,N)  = sum(BCLTBTU(6,1:mnumcr-2,N))                      ! Electric
    BCLTBTUT(7,N)  = sum(BCLTBTU(7,1:mnumcr-2,N))                      ! PHEV diesel
    BCLTBTUT(8,N)  = sum(BCLTBTU(8,1:mnumcr-2,N))                      ! PHEV gasoline
    BCLTBTUT(9,N)  = sum(BCLTBTU(9,1:mnumcr-2,N))                      ! FCEV
	BCLTBTUT(10,N) = sum(BCLTBTU(10,1:mnumcr-2,N))                    ! FCHEV
	BCLTBTUT(11,N) = sum(BCLTBTU(11,1:mnumcr-2,N))                    ! Gasoline HEV
	BCLTBTUT(12,N) = sum(BCLTBTU(12,1:mnumcr-2,N))                    ! H2 ICE

!...Benchmark commercial light truck consumption by fuel 
    CLTFUELBTU(1,n) = sum(cltfbtu(n,1,1:mnumcr-2))*BEN_MG             ! Gasoline
    CLTFUELBTU(2,n) = sum(cltfbtu(n,2,1:mnumcr-2))*BEN_DS             ! Diesel
	CLTFUELBTU(3,n) = sum(cltfbtu(n,3,1:mnumcr-2))*BEN_LG             ! LPG
	CLTFUELBTU(4,n) = sum(cltfbtu(n,4,1:mnumcr-2))*BEN_NG             ! CNG
	CLTFUELBTU(5,n) = sum(cltfbtu(n,5,1:mnumcr-2))*BEN_ET             ! Ethanol
	CLTFUELBTU(6,n) = sum(cltfbtu(n,6,1:mnumcr-2))*BEN_EL             ! Electric
	CLTFUELBTU(7,n) = sum(cltfbtu(n,7,1:mnumcr-2))*BEN_HY             ! Hydrogen

!...Benchmark consumption variables
    DO IREGN=1,MNUMCR
!...  Benchmark light duty vehicle consumption by fuel type
!     1=gasoline   2=methanol   3=ethanol          4=cng
!     5=lpg        6=electric   7=liquid hydrogen  8=distillate
      BTQLDV(1,IREGN) = TQLDV(1,IREGN,N) * BEN_MG
      BTQLDV(2,IREGN) = TQLDV(2,IREGN,N) * BENME(IREGN,N)
      BTQLDV(3,IREGN) = TQLDV(3,IREGN,N) * BENET(IREGN,N)
      BTQLDV(4,IREGN) = TQLDV(4,IREGN,N) * BENNG(IREGN,N)
      BTQLDV(5,IREGN) = TQLDV(5,IREGN,N) * BENLG(IREGN,N)
      BTQLDV(6,IREGN) = TQLDV(6,IREGN,N) * BENEL(IREGN,N)
      BTQLDV(7,IREGN) = TQLDV(7,IREGN,N) * BENHY(IREGN,N)
      BTQLDV(8,IREGN) = TQLDV(8,IREGN,N) * BEN_DS

!...  Benchmark freight truck consumption by fuel type and size class
!     Light heavy-duty trucks                                                                       
      BTQFREIRSC(1,1,IREGN) = TFRBTU_F_T(N,1,1,IREGN)*BENDS(iregn,n)        ! Diesel
      BTQFREIRSC(1,2,IREGN) = TFRBTU_F_T(N,1,2,IREGN)*BENMG(iregn,n)        ! Gasoline
      BTQFREIRSC(1,3,IREGN) = TFRBTU_F_T(N,1,3,IREGN)*BENLG(IREGN,N)        ! LPG
      BTQFREIRSC(1,4,IREGN) = TFRBTU_F_T(N,1,4,IREGN)*BENNG(IREGN,N)        ! Natural Gas
	  BTQFREIRSC(1,5,IREGN) = TFRBTU_F_T(N,1,5,IREGN)*BENET(IREGN,N)        ! Ethanol
	  BTQFREIRSC(1,6,IREGN) = TFRBTU_F_T(N,1,6,IREGN)*BENEL(IREGN,N)        ! Electric
	  BTQFREIRSC(1,7,IREGN) = TFRBTU_F_T(N,1,7,IREGN)*BENHY(IREGN,N)        ! Hydrogen		
!     Medium heavy-duty trucks
      BTQFREIRSC(2,1,IREGN) = TFRBTU_F_T(N,2,1,IREGN)*BENDS(iregn,n)        ! Diesel
      BTQFREIRSC(2,2,IREGN) = TFRBTU_F_T(N,2,2,IREGN)*BENMG(iregn,n)        ! Gasoline
      BTQFREIRSC(2,3,IREGN) = TFRBTU_F_T(N,2,3,IREGN)*BENLG(IREGN,N)        ! LPG
      BTQFREIRSC(2,4,IREGN) = TFRBTU_F_T(N,2,4,IREGN)*BENNG(IREGN,N)        ! Natural Gas
	  BTQFREIRSC(2,5,IREGN) = TFRBTU_F_T(N,2,5,IREGN)*BENET(IREGN,N)        ! Ethanol
	  BTQFREIRSC(2,6,IREGN) = TFRBTU_F_T(N,2,6,IREGN)*BENEL(IREGN,N)        ! Electric
	  BTQFREIRSC(2,7,IREGN) = TFRBTU_F_T(N,2,7,IREGN)*BENHY(IREGN,N)        ! Hydrogen						
!     Heavy heavy-duty trucks
      BTQFREIRSC(3,1,IREGN) = TFRBTU_F_T(N,3,1,IREGN)*BENDS(iregn,n)        ! Diesel
      BTQFREIRSC(3,2,IREGN) = TFRBTU_F_T(N,3,2,IREGN)*BENMG(iregn,n)        ! Gasoline
      BTQFREIRSC(3,3,IREGN) = TFRBTU_F_T(N,3,3,IREGN)*BENLG(IREGN,N)        ! LPG
      BTQFREIRSC(3,4,IREGN) = TFRBTU_F_T(N,3,4,IREGN)*BENNG(IREGN,N)        ! Natural Gas
	  BTQFREIRSC(3,5,IREGN) = TFRBTU_F_T(N,3,5,IREGN)*BENET(IREGN,N)        ! Ethanol
	  BTQFREIRSC(3,6,IREGN) = TFRBTU_F_T(N,3,6,IREGN)*BENEL(IREGN,N)        ! Electric
	  BTQFREIRSC(3,7,IREGN) = TFRBTU_F_T(N,3,7,IREGN)*BENHY(IREGN,N)        ! Hydrogen

!...  Benchmark domestic shipping consumption by fuel type
!     1=distillate   2=residual   3=cng  4=lng 
      BTQDSHIPR(1,IREGN) = TQDSHIPR(1,IREGN,N) * BENDS(iregn,n)
      BTQDSHIPR(2,IREGN) = TQDSHIPR(2,IREGN,N) * BENRS(iregn,n)
      BTQDSHIPR(3,IREGN) = TQDSHIPR(3,IREGN,N)  ! CNG 
      BTQDSHIPR(4,IREGN) = TQDSHIPR(4,IREGN,N)  ! LNG 

!...  Benchmark international shipping consumption by fuel type
!     1=distillate   2=residual  3=cng  4=lng  5=low sulfur fuel oil
      BTQISHIPR(1,IREGN) = TQISHIPR(1,IREGN,N) * BEN_DS
      BTQISHIPR(2,IREGN) = (TQISHIPR(2,IREGN,N) +TQISHIPR(5,IREGN,N))* BEN_RS
      BTQISHIPR(3,IREGN) = TQISHIPR(3,IREGN,N)  ! CNG 
      BTQISHIPR(4,IREGN) = TQISHIPR(4,IREGN,N)  ! LNG 
        
!...  Benchmark rail consumption by fuel type
!     1=distillate   2=residual   3=cng  4=lng
      BTQRAILR(1,IREGN) = TQRAILR(1,IREGN,N) * BENDS(iregn,n)
      BTQRAILR(2,IREGN) = TQRAILR(2,IREGN,N) * BENRS(iregn,n)     
      BTQRAILR(3,IREGN) = TQRAILR(3,IREGN,N)
      BTQRAILR(4,IREGN) = TQRAILR(4,IREGN,N)

!...  Benchmark military consumption by fuel type
!     1=distillate   2=jet fuel naphtha   3=residual   4=jet fuel kerosene
      BQMILTR(1,IREGN) = QMILTR(1,IREGN,N) * BEN_DS
      BQMILTR(2,IREGN) = QMILTR(2,IREGN,N) * BEN_JF
      BQMILTR(3,IREGN) = QMILTR(3,IREGN,N) * BEN_RS
      BQMILTR(4,IREGN) = QMILTR(4,IREGN,N) * BEN_JF

!...  Benchmark mass transit by mode
!     1=ldv    2=transit bus      3=intercity bus   4=school bus
!     5=intercity rail   6=transit rail    7=commuter rail
      BQMODR(2,IREGN) = QMTBR(1,1,IREGN,N)* BENMG(IREGN,N) + &
                        QMTBR(1,2,IREGN,N)* BENDS(IREGN,N) + &
                        QMTBR(1,3,IREGN,N)* BENET(IREGN,N) + &
                        QMTBR(1,4,IREGN,N)* BENME(IREGN,N) + &
                        QMTBR(1,5,IREGN,N)* BENNG(IREGN,N) + &
                        QMTBR(1,6,IREGN,N)* BENLG(IREGN,N) + &
                        QMTBR(1,7,IREGN,N)* BENEL(IREGN,N) + &
                        QMTBR(1,8,IREGN,N)* BENHY(IREGN,N) 

      BQMODR(3,IREGN) = QMTBR(2,1,IREGN,N)* BENMG(IREGN,N) + &
                        QMTBR(2,2,IREGN,N)* BENDS(IREGN,N) + &
                        QMTBR(2,3,IREGN,N)* BENET(IREGN,N) + &
                        QMTBR(2,4,IREGN,N)* BENME(IREGN,N) + &
                        QMTBR(2,5,IREGN,N)* BENNG(IREGN,N) + &
                        QMTBR(2,6,IREGN,N)* BENLG(IREGN,N) + &
                        QMTBR(2,7,IREGN,N)* BENEL(IREGN,N) + &
                        QMTBR(2,8,IREGN,N)* BENHY(IREGN,N) 
                        
      BQMODR(4,IREGN) = QMTBR(3,1,IREGN,N)* BENMG(IREGN,N) + &
                        QMTBR(3,2,IREGN,N)* BENDS(IREGN,N) + &
                        QMTBR(3,3,IREGN,N)* BENET(IREGN,N) + &
                        QMTBR(3,4,IREGN,N)* BENME(IREGN,N) + &
                        QMTBR(3,5,IREGN,N)* BENNG(IREGN,N) + &
                        QMTBR(3,6,IREGN,N)* BENLG(IREGN,N) + &
                        QMTBR(3,7,IREGN,N)* BENEL(IREGN,N) + &
                        QMTBR(3,8,IREGN,N)* BENHY(IREGN,N)  

      BQMODR(5,IREGN) = (IREDER(IREGN,N)* BENEL(IREGN,N)) + &
                        (IREDDR(IREGN,N)* BENDS(IREGN,N))

      BQMODR(6,IREGN) = TRED(IREGN,N)* BENEL(IREGN,N)

      BQMODR(7,IREGN) = (CREDE(IREGN,N)* BENEL(IREGN,N)) + &
                        (CREDD(IREGN,N)* BENDS(IREGN,N))

!...  Benchmark commercial fleet vehicle consumption by fuel type
      BFLTFUELBTU(iregn,1,n) = FLTFUELBTU(iregn,1,n) * BENMG(iregn,n)
      BFLTFUELBTU(iregn,2,n) = FLTFUELBTU(iregn,2,n) * BENME(iregn,n)
      BFLTFUELBTU(iregn,3,n) = FLTFUELBTU(iregn,3,n) * BENET(iregn,n)
      BFLTFUELBTU(iregn,4,n) = FLTFUELBTU(iregn,4,n) * BENNG(iregn,n) 
      BFLTFUELBTU(iregn,5,n) = FLTFUELBTU(iregn,5,n) * BENLG(iregn,n) 
      BFLTFUELBTU(iregn,6,n) = FLTFUELBTU(iregn,6,n) * BENEL(iregn,n)
      BFLTFUELBTU(iregn,7,n) = FLTFUELBTU(iregn,7,n) * BENHY(iregn,n)
      BFLTFUELBTU(iregn,8,n) = FLTFUELBTU(iregn,8,n) * BENDS(iregn,n)

      BQJETR(IREGN) = QJETR(IREGN,N) * BEN_JF             ! jet fuel
      BQAGR(IREGN)  = QAGTR(IREGN,N)                      ! aviation gasoline already benchmarked
      BQRECR(IREGN) = QRECR(1,IREGN,N) * BEN_MG + &       ! recreational boat gasoline
                      QRECR(2,IREGN,N) * BEN_DS           ! recreational boat diesel
      BQLUBR(IREGN) = QLUTR(IREGN,N)                      ! lubrication already benchmarked

      enddo

    do ifuel=1,maxfuel
      BFLTFUELBTU(11,ifuel,n) = sum(BFLTFUELBTU(1:MNUMCR-2,ifuel,n))
    enddo

!...Benchmark household VMT (billion miles) by technology
	do iregn=1,mnumcr-2
      BVMTECH( 1,iregn) = sum(VMTHH(N,iregn,1,1:maxvtyp)) *   BEN_MG
      BVMTECH( 2,iregn) = sum(VMTHH(N,iregn,2,1:maxvtyp)) *   BEN_DS
      BVMTECH( 3,iregn) = sum(VMTHH(N,iregn,3,1:maxvtyp)) *  (BEN_ET*PCTAF(2,iregn,N)+BEN_MG*(1.0-PCTAF(2,iregn,N)))
      BVMTECH( 4,iregn) = sum(VMTHH(N,iregn,4,1:maxvtyp)) *   BEN_EL
      BVMTECH( 5,iregn) = sum(VMTHH(N,iregn,5,1:maxvtyp)) *  (BEN_EL*PctPHEV20(N)+BEN_MG*(1.0-PctPHEV20(N)))
      BVMTECH( 6,iregn) = sum(VMTHH(N,iregn,6,1:maxvtyp)) *  (BEN_EL*PctPHEV50(N)+BEN_MG*(1.0-PctPHEV50(N)))
      BVMTECH( 7,iregn) = sum(VMTHH(N,iregn,7,1:maxvtyp)) *   BEN_EL
      BVMTECH( 8,iregn) = sum(VMTHH(N,iregn,8,1:maxvtyp)) *   BEN_DS
      BVMTECH( 9,iregn) = sum(VMTHH(N,iregn,9,1:maxvtyp)) *  (BEN_NG*PCTAF(3,iregn,N)+BEN_MG*(1.0-PCTAF(3,iregn,N)))
      BVMTECH(10,iregn) = sum(VMTHH(N,iregn,10,1:maxvtyp)) *  (BEN_LG*PCTAF(4,iregn,N)+BEN_MG*(1.0-PCTAF(4,iregn,N)))
      BVMTECH(11,iregn) = sum(VMTHH(N,iregn,11,1:maxvtyp)) *   BEN_NG
      BVMTECH(12,iregn) = sum(VMTHH(N,iregn,12,1:maxvtyp)) *   BEN_LG
      BVMTECH(13,iregn) = sum(VMTHH(N,iregn,13,1:maxvtyp)) *   BEN_ME
      BVMTECH(14,iregn) = sum(VMTHH(N,iregn,14,1:maxvtyp)) *   BEN_HY
      BVMTECH(15,iregn) = sum(VMTHH(N,iregn,15,1:maxvtyp)) *   BEN_EL 
      BVMTECH(16,iregn) = sum(VMTHH(N,iregn,16,1:maxvtyp)) *   BEN_MG
	enddo
	
	do ILDV=1,maxldv	!calculate national benched vmt
	  BVMTECH(ILDV,mnumcr) = sum(BVMTECH(ILDV,1:mnumcr-2))
	enddo

! ... Calculate total car and l.t. vmt by technology

    DO ILDV=1,MAXLDV
  	  BVMT_STK_HH(1:maxvtyp,ILDV,1:maxage,1,1:mnumcr-2) = VMT_STK_HH(1:maxvtyp,ILDV,1:maxage,1,1:mnumcr-2)
  	  !VMT_STK_HH is in millions, and BVMTECH is in billions, 
  	  !...so BMFAC as calculated is adjusted by a factor of 1000 to normalize to 1
      BMFAC = 1.0
  	IF (sum(VMT_STK_HH(1:maxvtyp,ILDV,1:maxage,1,1:mnumcr-2)) .NE. 0.0) then
  	  BMFAC = 1000.0 * BVMTECH(ILDV,mnumcr) / sum(VMT_STK_HH(1:maxvtyp,ILDV,1:maxage,1,1:mnumcr-2))
  	endif
	  do ivtyp=1,maxvtyp
  	    BVMT_STK_HH(ivtyp,ILDV,1:maxage,1,1:mnumcr-2) = BVMT_STK_HH(ivtyp,ILDV,1:maxage,1,1:mnumcr-2) * BMFAC
	  enddo
    ENDDO
	  
!...Benchmark VMT for light duty business fleet vehicles
    do ivtyp=1,maxvtyp
      do ifleet=1,maxfleet
	    do ihav=1,maxhav
			FLTVMTHAV(ivtyp,ifleet, 1,ihav,yrs)= FLTVMTECH(ivtyp,ifleet, 1,ihav)*  BEN_MG
			FLTVMTHAV(ivtyp,ifleet, 2,ihav,yrs)= FLTVMTECH(ivtyp,ifleet, 2,ihav)*  BEN_DS 
			FLTVMTHAV(ivtyp,ifleet, 3,ihav,yrs)= FLTVMTECH(ivtyp,ifleet, 3,ihav)*((BEN_ET*PCTAF(2,mnumcr,n)) + (BEN_MG*(1.0-PCTAF(2,mnumcr,n))))
			FLTVMTHAV(ivtyp,ifleet, 4,ihav,yrs)= FLTVMTECH(ivtyp,ifleet, 4,ihav)*  BEN_EL 
			FLTVMTHAV(ivtyp,ifleet, 5,ihav,yrs)= FLTVMTECH(ivtyp,ifleet, 5,ihav)*((BEN_EL*PctPHEV20(n)) + (BEN_MG*(1.0-PctPHEV20(n))))
			FLTVMTHAV(ivtyp,ifleet, 6,ihav,yrs)= FLTVMTECH(ivtyp,ifleet, 6,ihav)*((BEN_EL*PctPHEV50(n)) + (BEN_MG*(1.0-PctPHEV50(n))))
			FLTVMTHAV(ivtyp,ifleet, 7,ihav,yrs)= FLTVMTECH(ivtyp,ifleet, 7,ihav)*  BEN_EL
			FLTVMTHAV(ivtyp,ifleet, 8,ihav,yrs)= FLTVMTECH(ivtyp,ifleet, 8,ihav)*  BEN_DS
			FLTVMTHAV(ivtyp,ifleet, 9,ihav,yrs)= FLTVMTECH(ivtyp,ifleet, 9,ihav)*((BEN_NG*PCTAF(3,mnumcr,n)) + (BEN_MG*(1.0-PCTAF(3,mnumcr,n))))
			FLTVMTHAV(ivtyp,ifleet,10,ihav,yrs)= FLTVMTECH(ivtyp,ifleet,10,ihav)*((BEN_LG*PCTAF(4,mnumcr,n)) + (BEN_MG*(1.0-PCTAF(4,mnumcr,n)))) 
			FLTVMTHAV(ivtyp,ifleet,11,ihav,yrs)= FLTVMTECH(ivtyp,ifleet,11,ihav)*  BEN_NG
			FLTVMTHAV(ivtyp,ifleet,12,ihav,yrs)= FLTVMTECH(ivtyp,ifleet,12,ihav)*  BEN_LG
			FLTVMTHAV(ivtyp,ifleet,13,ihav,yrs)= FLTVMTECH(ivtyp,ifleet,13,ihav)*  BEN_ME
			FLTVMTHAV(ivtyp,ifleet,14,ihav,yrs)= FLTVMTECH(ivtyp,ifleet,14,ihav)*  BEN_HY
			FLTVMTHAV(ivtyp,ifleet,15,ihav,yrs)= FLTVMTECH(ivtyp,ifleet,15,ihav)*  BEN_EL 
			FLTVMTHAV(ivtyp,ifleet,16,ihav,yrs)= FLTVMTECH(ivtyp,ifleet,16,ihav)*  BEN_MG
		enddo
      enddo
    enddo
!...Convert benchmarked fleet vmt to billion miles
	do ivtyp=1,maxvtyp
	  do ifleet=1,maxfleet
	    do ildv=1,maxldv
			BFLTVMTECH(ivtyp,ifleet,ildv) = sum(FLTVMTHAV(ivtyp,ifleet,ildv,1:maxhav,yrs))/1000000000.0
		enddo
	  enddo
	enddo

!...Benchmark VMT for commercial light trucks by technology
    BCLTVMT(1,N)  = CLTVMTT(1,N)*BENMG(mnumcr,N)                                     ! Gasoline	
    BCLTVMT(2,N)  = CLTVMTT(2,N)*BENDS(mnumcr,N)                                     ! Diesel
    BCLTVMT(3,N)  = CLTVMTT(3,N)*BENLG(mnumcr,N)                                     ! LPG
	BCLTVMT(4,N)  = CLTVMTT(4,N)*BENNG(mnumcr,N)                                     ! CNG
	BCLTVMT(5,N)  = CLTVMTT(5,N)*(BENET(mnumcr,N)*PCTAF(2,mnumcr,n) &        		 ! Flex fuel
				 			   + BENMG(mnumcr,N)*(1.0-PCTAF(2,mnumcr,n)))
	BCLTVMT(6,N)  = CLTVMTT(6,N)*BENEL(mnumcr,N)                                     ! Electric
	BCLTVMT(7,N)  = CLTVMTT(7,N)*(BENEL(mnumcr,N)*PctEVMT_PHEV(N,4,mnumcr,1) &       ! PHEV Diesel
				                + BENDS(mnumcr,N)*(1.0-PctEVMT_PHEV(N,4,mnumcr,1)))
	BCLTVMT(8,N)  = CLTVMTT(8,N)*(BENEL(mnumcr,N)*PctEVMT_PHEV(N,4,mnumcr,2) &       ! PHEV Gasoline
				 			   + BENMG(mnumcr,N)*(1.0-PctEVMT_PHEV(N,4,mnumcr,2)))
	BCLTVMT(9,N)  = CLTVMTT(9,N)*BENHY(mnumcr,N)                                     ! FCEV
	BCLTVMT(10,N) = CLTVMTT(10,N)*BENHY(mnumcr,N)                                    ! FCHEV
	BCLTVMT(11,N) = CLTVMTT(11,N)*BENMG(mnumcr,N)                                    ! Gasoline HEV
	BCLTVMT(12,N) = CLTVMTT(12,N)*BENHY(mnumcr,N)                                    ! H2 ICE

!...Benchmark VMT for freight truck by technology
    do iregn=1,mnumcr  		
!...  Light and medium heavy-duty trucks	  
      BFVMTECHSC(1,1,iregn) = sum(VMTFLT_SAF_TR(1:2,1,iregn))*BENDS(iregn,N)                                                            ! Diesel
      BFVMTECHSC(1,2,iregn) = sum(VMTFLT_SAF_TR(1:2,2,iregn))*BENMG(iregn,N)                                                           ! Gasoline
      BFVMTECHSC(1,3,iregn) = sum(VMTFLT_SAF_TR(1:2,3,iregn))*BENLG(iregn,N)                                                            ! Lpg
      BFVMTECHSC(1,4,iregn) = sum(VMTFLT_SAF_TR(1:2,4,iregn))*BENNG(iregn,N)                                                            ! Cng
	  BFVMTECHSC(1,5,iregn) = sum(VMTFLT_SAF_TR(1:2,5,iregn))*(BENET(iregn,N)*PCTAF(2,iregn,n) + BENMG(iregn,N)*(1.0-PCTAF(2,iregn,n))) ! Flex fuel
	  BFVMTECHSC(1,6,iregn) = sum(VMTFLT_SAF_TR(1:2,6,iregn))*BENEL(iregn,n)                                                            ! Electric 
	  BFVMTECHSC(1,7,iregn) = VMTFLT_SAF_TR(1,7,iregn)*(BENEL(iregn,N)*PctEVMT_PHEV(N,1,iregn,1) + BENDS(iregn,N)*(1.0-PctEVMT_PHEV(N,1,iregn,1)))&		! PHEV Diesel, Class 3
	  						+ VMTFLT_SAF_TR(2,7,iregn)*(BENEL(iregn,N)*PctEVMT_PHEV(N,2,iregn,1) + BENDS(iregn,N)*(1.0-PctEVMT_PHEV(N,2,iregn,1)))		! PHEV Diesel, Class 4-6
	  BFVMTECHSC(1,8,iregn) = VMTFLT_SAF_TR(1,8,iregn)*(BENEL(iregn,N)*PctEVMT_PHEV(N,1,iregn,2) + BENDS(iregn,N)*(1.0-PctEVMT_PHEV(N,1,iregn,2)))&		! PHEV Gasoline, Class 3
	  						+ VMTFLT_SAF_TR(2,8,iregn)*(BENEL(iregn,N)*PctEVMT_PHEV(N,2,iregn,2) + BENDS(iregn,N)*(1.0-PctEVMT_PHEV(N,2,iregn,2)))		! PHEV Gasoline, Class 4-6
	  BFVMTECHSC(1,9,iregn) = sum(VMTFLT_SAF_TR(1:2,9,iregn))*BENHY(iregn,n)                                                            ! FCEV
	  BFVMTECHSC(1,10,iregn)= sum(VMTFLT_SAF_TR(1:2,10,iregn))*BENHY(iregn,N)															! FCHEV
	  BFVMTECHSC(1,11,iregn)= sum(VMTFLT_SAF_TR(1:2,11,iregn))*BENMG(iregn,N)															! Gasoline HEV
	  BFVMTECHSC(1,12,iregn)= sum(VMTFLT_SAF_TR(1:2,12,iregn))*BENHY(iregn,N)															! H2 ICE

!...  Heavy heavy-duty trucks		
      BFVMTECHSC(2,1,iregn) = VMTFLT_SAF_TR(3,1,iregn)*BENDS(iregn,N)                                                                   ! Diesel
      BFVMTECHSC(2,2,iregn) = VMTFLT_SAF_TR(3,2,iregn)*BENMG(iregn,N)                                                                   ! Gasoline
      BFVMTECHSC(2,3,iregn) = VMTFLT_SAF_TR(3,3,iregn)*BENLG(iregn,N)                                                                   ! Lpg
      BFVMTECHSC(2,4,iregn) = VMTFLT_SAF_TR(3,4,iregn)*BENNG(iregn,N)                                                                   ! Cng
	  BFVMTECHSC(2,5,iregn) = VMTFLT_SAF_TR(3,5,iregn)*(BENET(iregn,N)*PCTAF(2,iregn,n) + BENMG(iregn,N)*(1.0-PCTAF(2,iregn,n)))        ! Flex fuel
	  BFVMTECHSC(2,6,iregn) = VMTFLT_SAF_TR(3,6,iregn)*BENEL(iregn,n)                                                                   ! Electric 
	  BFVMTECHSC(2,7,iregn) = VMTFLT_SAF_TR(3,7,iregn)*(BEN_EL*PctEVMT_PHEV(N,3,iregn,1) + BEN_DS*(1.0-PctEVMT_PHEV(N,3,iregn,1)))    	! PHEV Diesel
	  BFVMTECHSC(2,8,iregn) = VMTFLT_SAF_TR(3,8,iregn)*(BEN_EL*PctEVMT_PHEV(N,3,iregn,2) + BEN_MG*(1.0-PctEVMT_PHEV(N,3,iregn,2)))    	! PHEV Gasoline
	  BFVMTECHSC(2,9,iregn) = VMTFLT_SAF_TR(3,9,iregn)*BENHY(iregn,n)  		                                                            ! FCEV 
	  BFVMTECHSC(2,10,iregn)= VMTFLT_SAF_TR(3,10,iregn)*BENHY(iregn,n) 																	! FCHEV
	  BFVMTECHSC(2,11,iregn)= VMTFLT_SAF_TR(3,11,iregn)*BENMG(iregn,n) 																	! Gasoline HEV
	  BFVMTECHSC(2,12,iregn)= VMTFLT_SAF_TR(3,12,iregn)*BENHY(iregn,n) 																	! H2 ICE
    enddo 

!...Benchmark seat-miles demanded for air
    IF (N .GE. 6) THEN
      BASMDEMD(1) = ASMDEMD(1,1,N)
      BASMDEMD(2) = ASMDEMD(1,2,N)
    ENDIF

!...Benchmark TMT for rail and ship
    do iregn=1,mnumcr
!...  rail
      if(n.le.RAILHISTYR) then
        BRTMTT(n,iregn) = RTMTT(n,iregn)
      else
        BRTMTT(n,iregn) = RTMTT(n,iregn) * BENDS(iregn,n)
      endif 
!...  ship
      if(n.le.shiphistyr) then
        BSTMTT(n,iregn) = STMTT(n,iregn)
      else
	    if (STMTT(n,iregn).gt.0.0) then
		  BSTMTT(n,iregn) = STMTT(n,iregn) * sum(BTQDSHIPR(1:4,iregn))/sum(TQDSHIPR(1:4,iregn,n))
		else
		  BSTMTT(n,iregn) = STMTT(n,iregn)
		endif
      endif     
    enddo

    RETURN
    END SUBROUTINE TBENCHMARK

! ==========================================================================================================
! ... Subroutine TREPORT generates the parameters used in the report writer
! ==========================================================================================================
    SUBROUTINE TREPORT
    USE T_
    IMPLICIT NONE


	  REAL   TRQLDT(MNUMYR),TRQHWY_TMP(2,MNUMYR),TRQHWY_SHR(2,MNUMYR), &
             FLTSALTOTC(MNUMYR),FLTSALTOTT(MNUMYR),FLTSALTOTV(MNUMYR), &
             FLTSTKTOTC(MNUMYR),FLTSTKTOTT(MNUMYR),FLTSTKTOTV(MNUMYR), &
			 VMT_STK_TOT(maxvtyp,maxldv)

      REAL   FLTECHRPT_REG(2,MNUMCR,MAXLDV,MNUMYR),&
             ANCSALE(MNUMCR,MNUMYR),ANTSALE(MNUMCR,MNUMYR), CQ(MNUMYR),LTQ(MNUMYR), ldvshr, &
             BEN(8,MNUMYR)

      REAL   NUM1,NUM2,NUM3,DEN1,DEN2,DEN3,FLTNUM1,FLTNUM2,FLTDEN1,FLTDEN2,                  &  
             cmpg_1(maxldv),tmpg_1(maxldv),lmpg_1(maxldv),cmpg_2(maxldv),tmpg_2(maxldv),     &
             lmpg_2(maxldv)	 
             
      REAL   TECHMPG_NUM(7)             ! Numerator for sales-weighted new vehicle fuel economy (TECHMPG) and VMT-weighted stock fuel economy (STKMPG)
      
      INTEGER isec,impg
      INTEGER mpgmap(maxldv),mpgmapCLT(12)
      
      data mpgmap/1,2,1,5,4,4,5,3,7,7,7,7,7,6,5,3/
      data mpgmapCLT/1,2,7,7,1,5,4,4,6,6,3,7/
             
! ... ***** TABLE 7 *******
! ... Total freight truck vmt
      DO IFUELX=1,12
        DO I=1,2
          TRVMTTRK(I,IFUELX,N) = sum(BFVMTECHSC(I,IFUELX,1:mnumcr-2)) / (1.0*1E9)
        ENDDO
      ENDDO
!   Passenger Travel (billion vehicle miles travelled) 		
!   2/3 Wheelers
	if(curcalyr.le.CycHistYR)	then
	  PAS_RPM(1,n) = Cyc_RPM(n)
	else
	  PAS_RPM(1,n) = (TRLDVMTE(1,N)/TRLDVMTE(1,n-1))*PAS_RPM(1,n-1)
	endif
!   Bus
	PAS_RPM(2,n)=(sum(tmod(1:2,n))+tbpmt(11,n))/1000.0	!transit 
!   Rail (passenger miles travelled)
	PAS_RPM(3,n)=(sum(trrpm(1:9,n))+sum(crrpm(1:9,n))+irrpm(n))/1000.0

!...FTAB Table 46 - total energy use by fuel type within light duty vehicle
    do ifuel=1,maxfuel
      do iregn=1,mnumcr
        TRQLDV(ifuel,iregn,n) = BTQLDV(ifuel,iregn) + BFLTFUELBTU(iregn,ifuel,n)
      enddo
    enddo

    TRQLDT(N) = TRQLDV(1,11,N) + TRQLDV(2,11,N) + TRQLDV(3,11,N) + &
                TRQLDV(4,11,N) + TRQLDV(5,11,N) + TRQLDV(6,11,N) + &
                TRQLDV(7,11,N) + TRQLDV(8,11,N)

! ... Calculate energy use by fuel type within freight truck
    TRQFTRK(1,N) = sum(BTQFREIRSC(1:3,2,1:mnumcr-2))                                   ! gasoline
    TRQFTRK(2,N) = sum(BTQFREIRSC(1:3,1,1:mnumcr-2))                                   ! diesel
    TRQFTRK(3,N) = sum(BTQFREIRSC(1:3,4,1:mnumcr-2))                                   ! cng
    TRQFTRK(4,N) = 0.0                                                                 ! unused
    TRQFTRK(5,N) = sum(BTQFREIRSC(1:3,3,1:mnumcr-2))                                   ! lpg
	TRQFTRK(6,N) = sum(BTQFREIRSC(1:3,5,1:mnumcr-2))                                   ! Ethanol
	TRQFTRK(7,N) = sum(BTQFREIRSC(1:3,6,1:mnumcr-2))                                   ! Electric
	TRQFTRK(8,N) = sum(BTQFREIRSC(1:3,7,1:mnumcr-2))                                   ! Hydrogen

  ! Light Duty Trucks		
	TRQFTRK_new(1,1,n) = sum(BTQFREIRSC(1,2,1:mnumcr-2)) + sum(BTQFREIRSC(1,5,1:mnumcr-2)) ! Motor Gasoline
	TRQFTRK_new(1,2,n) = sum(BTQFREIRSC(1,1,1:mnumcr-2))                                   ! Diesel
	TRQFTRK_new(1,3,n) = sum(BTQFREIRSC(1,3,1:mnumcr-2))                                   ! Liquid Petroleum Gas
	TRQFTRK_new(1,4,n) = sum(BTQFREIRSC(1,6,1:mnumcr-2))                                   ! Electric
	TRQFTRK_new(1,5,n) = sum(BTQFREIRSC(1,4,1:mnumcr-2))                                   ! Natural Gas	
	TRQFTRK_new(1,6,n) = sum(BTQFREIRSC(1,7,1:mnumcr-2))                                   ! Hydrogen
  ! Medium Trucks		
	TRQFTRK_new(2,1,n) = sum(BTQFREIRSC(2,2,1:mnumcr-2)) + sum(BTQFREIRSC(2,5,1:mnumcr-2)) ! Motor Gasoline
	TRQFTRK_new(2,2,n) = sum(BTQFREIRSC(2,1,1:mnumcr-2))                                   ! Diesel
	TRQFTRK_new(2,3,n) = sum(BTQFREIRSC(2,3,1:mnumcr-2))                                   ! Liquid Petroleum Gas
	TRQFTRK_new(2,4,n) = sum(BTQFREIRSC(2,6,1:mnumcr-2))                                   ! Electric
	TRQFTRK_new(2,5,n) = sum(BTQFREIRSC(2,4,1:mnumcr-2))                                   ! Natural Gas	
	TRQFTRK_new(2,6,n) = sum(BTQFREIRSC(2,7,1:mnumcr-2))                                   ! Hydrogen
  ! Heavy Trucks	
	TRQFTRK_new(3,1,n) = sum(BTQFREIRSC(3,2,1:mnumcr-2)) + sum(BTQFREIRSC(3,5,1:mnumcr-2)) ! Motor Gasoline
	TRQFTRK_new(3,2,n) = sum(BTQFREIRSC(3,1,1:mnumcr-2))                                   ! Diesel
	TRQFTRK_new(3,3,n) = sum(BTQFREIRSC(3,3,1:mnumcr-2))                                   ! Liquid Petroleum Gas
	TRQFTRK_new(3,4,n) = sum(BTQFREIRSC(3,6,1:mnumcr-2))                                   ! Electric
	TRQFTRK_new(3,5,n) = sum(BTQFREIRSC(3,4,1:mnumcr-2))                                   ! Natural Gas	
	TRQFTRK_new(3,6,n) = sum(BTQFREIRSC(3,7,1:mnumcr-2))                                   ! Hydrogen
	
  ! Total by Mode
	TRQHWY_new(5,N) = sum(BTQFREIRSC(1,1:7,1:mnumcr-2))                                    ! Light Truck
	TRQHWY_new(6,N) = sum(BTQFREIRSC(2,1:7,1:mnumcr-2))                                    ! Medium Truck


! ... Calculate total incremental consumer spending as a result of incremental
! ... petroleum fuel tax - billions of 1987 dollars
      HFUELTAX(N) = (((TRQLDV(1,11,N) + TRQFTRK(1,N)) / CFMGQ(N) * 0.042) +  &    ! total hwy gasoline gallons
                     ((TRQLDV(8,11,N) + TRQFTRK(2,N)) / CFDSTR(N)* 0.042)) * &    ! total hwy diesel gallons
                      (FUELTAX87(N)/8)                                            ! tax/gallon 1987$

! ... Calculate energy use by fuel type within air transportation
      TRQAIRT(1,N) = BQJETR(11)
      TRQAIRT(2,N) = BQAGR(11)

! ... ***** TABLE 45 *******
! ... Calculate energy use by freight trucks	
      TRQHWY(5,N) = sum(BTQFREIRSC(1:2,1:7,1:mnumcr-2))                                       ! medium
      TRQHWY(6,N) = sum(BTQFREIRSC(3,1:7,1:mnumcr-2))                                         ! large			  
					

! ... Calculate personal car and l.t. consumption
      CQ(N)  = 0.0
      LTQ(N) = 0.0
      DO ILDV=1,MAXLDV		
		IF (CMPG_IT(ILDV,N) .NE. 0.0) &
          CQ(N) = CQ(N) + (sum(BVMT_STK_HH(1,ILDV,1:maxage,1,1:mnumcr-2)) /CMPG_IT(ILDV,N)) * MG_HHV / 1000.0
        IF (TMPG_IT(ILDV,N) .NE. 0.0) &
          LTQ(N) = LTQ(N) + (sum(BVMT_STK_HH(2,ILDV,1:maxage,1,1:mnumcr-2))/TMPG_IT(ILDV,N)) * MG_HHV / 1000.0		  
      ENDDO

!...FTAB Table 53 - Fleet energy demand by vehicle type
!...Benchmark before writing to report writer
	do iregn=1,mnumcr 
	  if(iregn.ne.10)then
		do ivtyp=1,maxvtyp
		  FLTLDVBTU(iregn,ivtyp, 1,n) = FLTLDVBTU(iregn,ivtyp, 1,n) * BENMG(iregn,n)
		  FLTLDVBTU(iregn,ivtyp, 2,n) = FLTLDVBTU(iregn,ivtyp, 2,n) * BENDS(iregn,n)
		  FLTLDVBTU(iregn,ivtyp, 3,n) = FLTLDVBTU(iregn,ivtyp, 3,n) * (1.0-PCTAF(2,iregn,n))*BENMG(iregn,n)+&
										FLTLDVBTU(iregn,ivtyp, 3,n) * PCTAF(2,iregn,n)*BENET(iregn,n)
		  FLTLDVBTU(iregn,ivtyp, 4,n) = FLTLDVBTU(iregn,ivtyp, 4,n) * BENEL(iregn,n) 
		  FLTLDVBTU(iregn,ivtyp, 5,n) = FLTLDVBTU(iregn,ivtyp, 5,n) * (1.0-PctPHEV20(n))*BENMG(iregn,n)+&
										FLTLDVBTU(iregn,ivtyp, 5,n) * PctPHEV20(n)*BENEL(iregn,n)
		  FLTLDVBTU(iregn,ivtyp, 6,n) = FLTLDVBTU(iregn,ivtyp, 6,n) * (1.0-PctPHEV50(n))*BENMG(iregn,n)+&
										FLTLDVBTU(iregn,ivtyp, 6,n) * PctPHEV50(n)*BENEL(iregn,n)
		  FLTLDVBTU(iregn,ivtyp, 7,n) = FLTLDVBTU(iregn,ivtyp, 7,n) * BENEL(iregn,n)
		  FLTLDVBTU(iregn,ivtyp, 8,n) = FLTLDVBTU(iregn,ivtyp, 8,n) * BENDS(iregn,n)
		  FLTLDVBTU(iregn,ivtyp, 9,n) = FLTLDVBTU(iregn,ivtyp, 9,n) * (1.0-PCTAF(3,iregn,n))*BENMG(iregn,n)+&
										FLTLDVBTU(iregn,ivtyp, 9,n) * PCTAF(3,iregn,n)*BENMG(iregn,n)
		  FLTLDVBTU(iregn,ivtyp,10,n) = FLTLDVBTU(iregn,ivtyp,10,n) * (1.0-PCTAF(4,iregn,n))*BENMG(iregn,n)+&
										FLTLDVBTU(iregn,ivtyp,10,n) * PCTAF(4,iregn,n)*BENLG(iregn,n)
		  FLTLDVBTU(iregn,ivtyp,11,n) = FLTLDVBTU(iregn,ivtyp,11,n) * BENNG(iregn,n)
		  FLTLDVBTU(iregn,ivtyp,12,n) = FLTLDVBTU(iregn,ivtyp,12,n) * BENLG(iregn,n)
		  FLTLDVBTU(iregn,ivtyp,13,n) = FLTLDVBTU(iregn,ivtyp,13,n) * BENME(iregn,n)
		  FLTLDVBTU(iregn,ivtyp,14,n) = FLTLDVBTU(iregn,ivtyp,14,n) * BENHY(iregn,n)
		  FLTLDVBTU(iregn,ivtyp,15,n) = FLTLDVBTU(iregn,ivtyp,15,n) * BENEL(iregn,n) 
		  FLTLDVBTU(iregn,ivtyp,16,n) = FLTLDVBTU(iregn,ivtyp,16,n) * BENMG(iregn,n)
		enddo 
!...  regional/total fleet vehicle energy demand by light vehicle type
		do ildv=1,maxldv 
	      fltldvbtut(iregn,ildv,n) = sum(fltldvbtu(iregn,1:maxvtyp,ildv,n))
		enddo
	  endif
    enddo

!...for report writer 
	do ivtyp=1,maxvtyp 
	  do ildv=1,maxldv 
	    fltfcldvbtu(ivtyp,ildv,n) = fltldvbtu(mnumcr,ivtyp,ildv,n)
	  enddo 
	enddo

! ... Calculate energy use by light duty vehicles (cars, l.t., motorcycles)
      TRQHWY_TMP(1,N) = CQ(N) + sum(fltldvbtu(mnumcr,1,1:maxldv,n))

! ... Personal + fleet + freight for l.t.
      TRQHWY_TMP(2,N) = LTQ(N) + sum(fltldvbtu(mnumcr,2,1:maxldv,n))
      TRQHWY(3,N) = 0.00287 * TRQHWY_TMP(1,N)                ! Motorcycle

! ... Adjust the consumption so they match the total shown in tables 7 & 46
      TRQHWY_SHR(1,N) = TRQHWY_TMP(1,N) / (TRQHWY_TMP(1,N) + TRQHWY_TMP(2,N))
      TRQHWY_SHR(2,N) = TRQHWY_TMP(2,N) / (TRQHWY_TMP(1,N) + TRQHWY_TMP(2,N))

      TRQHWY(1,N) = (TRQLDT(N) - TRQHWY(3,N)) * TRQHWY_SHR(1,N)
      TRQHWY(2,N) = (TRQLDT(N) - TRQHWY(3,N)) * TRQHWY_SHR(2,N)

! ... Calculate energy use by buses (transit, intercity, school)
      TRQBUS(1,1,N) = QMTBR(1,1,11,N) * BENMG(11,N)      ! transit bus gasoline
      TRQBUS(1,2,N) = QMTBR(1,2,11,N) * BENDS(11,N)      ! transit bus diesel
      TRQBUS(1,3,N) = QMTBR(1,3,11,N) * BENET(11,N)      ! transit bus ethanol
      TRQBUS(1,4,N) = QMTBR(1,4,11,N) * BENME(11,N)      ! transit bus methanol 
      TRQBUS(1,5,N) = QMTBR(1,5,11,N) * BENNG(11,N)      ! transit bus CNG 	  
      TRQBUS(1,6,N) = QMTBR(1,6,11,N) * BENLG(11,N)      ! transit bus LPG 	  
      TRQBUS(1,7,N) = QMTBR(1,7,11,N) * BENEL(11,N)      ! transit bus Electricity	  
      TRQBUS(1,8,N) = QMTBR(1,8,11,N) * BENHY(11,N)      ! transit bus Hydrogen
      TRQBUS(2,1,N) = QMTBR(2,1,11,N) * BENMG(11,N)      ! intercity bus gasoline
      TRQBUS(2,2,N) = QMTBR(2,2,11,N) * BENDS(11,N)      ! intercity bus diesel
      TRQBUS(2,3,N) = QMTBR(2,3,11,N) * BENET(11,N)      ! intercity bus ethanol
      TRQBUS(2,4,N) = QMTBR(2,4,11,N) * BENME(11,N)      ! intercity bus methanol 
      TRQBUS(2,5,N) = QMTBR(2,5,11,N) * BENNG(11,N)      ! intercity bus CNG 	  
      TRQBUS(2,6,N) = QMTBR(2,6,11,N) * BENLG(11,N)      ! intercity bus LPG 	  
      TRQBUS(2,7,N) = QMTBR(2,7,11,N) * BENEL(11,N)      ! intercity bus Electricity	  
      TRQBUS(2,8,N) = QMTBR(2,8,11,N) * BENHY(11,N)      ! intercity bus Hydrogen 	  
      TRQBUS(3,1,N) = QMTBR(3,1,11,N) * BENMG(11,N)      ! school bus gasoline
      TRQBUS(3,2,N) = QMTBR(3,2,11,N) * BENDS(11,N)      ! school bus diesel
      TRQBUS(3,3,N) = QMTBR(3,3,11,N) * BENET(11,N)      ! school bus ethanol
      TRQBUS(3,4,N) = QMTBR(3,4,11,N) * BENME(11,N)      ! school bus methanol 
      TRQBUS(3,5,N) = QMTBR(3,5,11,N) * BENNG(11,N)      ! school bus CNG 	  
      TRQBUS(3,6,N) = QMTBR(3,6,11,N) * BENLG(11,N)      ! school bus LPG 	  
      TRQBUS(3,7,N) = QMTBR(3,7,11,N) * BENEL(11,N)      ! school bus Electricity	  
      TRQBUS(3,8,N) = QMTBR(3,8,11,N) * BENHY(11,N)      ! school bus Hydrogen 		  

! ... Calculate energy use by air
!	  QJETR_DI(maxwreg,domint,2,mnumyr)  [where third dimension is 1: pass, 2: freight]
!	  Assume general aviation accounts for 5% of total jet fuel consumption, in addition to avgas.
      TRQNHWY(1,N) = BQAGR(11) + (sum(QJETR_DI(1,:,:,N)) - sum(QJETR_DI(1,:,:,N))/1.05) * BEN_JF	! General aviation
      TRQNHWY(2,N) = (QJETR_DI(1,1,1,N)/1.05)* BEN_JF												! Passenger Domestic
      TRQNHWY(3,N) = (QJETR_DI(1,2,1,N)/1.05) * BEN_JF												! Passenger International
      TRQNHWY(4,N) = (sum(QJETR_DI(1,:,2,N))/1.05)* BEN_JF	  										! Freight (domestic and int'l)
						 
! ... Calculate energy use by water
      TRQDOMS(1,N) = sum(BTQDSHIPR(1,1:mnumcr-2))       ! diesel
      TRQDOMS(2,N) = sum(BTQDSHIPR(2,1:mnumcr-2))       ! residual
      TRQDOMS(3,N) = sum(BTQDSHIPR(3,1:mnumcr-2))       ! CNG 
      TRQDOMS(4,N) = sum(BTQDSHIPR(4,1:mnumcr-2))       ! LNG 

      TRQINTS(1,N) = BTQISHIPR(1,11)                    ! diesel
      TRQINTS(2,N) = BTQISHIPR(2,11)                    ! residual
      TRQINTS(3,N) = BTQISHIPR(3,11)                    ! CNG 
      TRQINTS(4,N) = BTQISHIPR(4,11)                    ! LNG 

      TRQBOAT(1,N) = QRECR(1,11,N) * BEN_MG             ! gasoline
      TRQBOAT(2,N) = QRECR(2,11,N) * BEN_DS             ! diesel

! ... Calculate energy use by rail (freight, passenger)
      TRQRRF(1,N) = sum(BTQRAILR(1,1:mnumcr-2))       ! diesel
      TRQRRF(2,N) = sum(BTQRAILR(2,1:mnumcr-2))       ! residual
      TRQRRF(3,N) = sum(BTQRAILR(3,1:mnumcr-2))       ! CNG 
      TRQRRF(4,N) = sum(BTQRAILR(4,1:mnumcr-2))       ! LNG 
      
      TRQRRP(1,N) = IREDE(N)                          ! intercity rail elec.
      TRQRRP(2,N) = IREDD(N)*BEN_DS                   ! intercity rail diesel
      TRQRRP(3,N) = 0.0                               ! intercity rail CNG
      TRQRRP(4,N) = 0.0                               ! intercity rail LNG
      TRQRRP(5,N) = sum(TRED(1:MNUMCR-2,N))           ! transit rail elec.
      TRQRRP(6,N) = sum(CREDE(1:MNUMCR-2,N))          ! commuter rail elec.
      TRQRRP(7,N) = sum(CREDD(1:MNUMCR-2,N))*BEN_DS   ! commuter rail diesel
      TRQRRP(8,N) = 0.0                               ! commuter rail CNG
      TRQRRP(9,N) = 0.0                               ! commuter rail LNG
      
! ... Calculate energy use by lubricants
      TRQLUB(N) = BQLUBR(11)

! ... Calculate energy use by military
      TRQMIL(1,N) = BQMILTR(4,11)     ! jet fuel kerosene
      TRQMIL(2,N) = BQMILTR(2,11)     ! jet fuel naphtha
      TRQMIL(3,N) = BQMILTR(3,11)     ! residual
      TRQMIL(4,N) = BQMILTR(1,11)     ! distillate

! ... Calculate energy use by type
      TRQENUSE(1,N)  = QMGTR(11,N)
      TRQENUSE(2,N)  = QDSTR(11,N)
      TRQENUSE(3,N)  = QJFTR(11,N)
      TRQENUSE(4,N)  = QRSTR(11,N)
      TRQENUSE(5,N)  = QAGTR(11,N) 
      TRQENUSE(6,N)  = QLGTR(11,N)
      TRQENUSE(7,N)  = QLUTR(11,N) 
      TRQENUSE(8,N)  = QMETR(11,N)
      TRQENUSE(9,N)  = QETTR(11,N)
      TRQENUSE(10,N) = QELTR(11,N)
      TRQENUSE(11,N) = QNGTR(11,N)
      TRQENUSE(12,N) = QH2TR(11,N)

!...Populate electricity consumption array for EMM
!	These values are all benchmarked.
	TRQ_ELEC(:,:,N) = 0.0
    DO IREGN = 1, MNUMCR-2
	  TRQ_ELEC(1,IREGN,N) = TRQLDV(6,IREGN,N)*chg_dist(iregn,2,n)				! LDV Home
	  TRQ_ELEC(2,IREGN,N) = TRQLDV(6,IREGN,N)*chg_dist(iregn,3,n)			    ! LDV public L2
	  TRQ_ELEC(3,IREGN,N) = TRQLDV(6,IREGN,N)-SUM(TRQ_ELEC(1:2,IREGN,N))        ! LDV public DCFC
	  TRQ_ELEC(4,IREGN,N) = QMTBR(3,7,IREGN,N) * BENEL(IREGN,N)			        ! Bus school
	  TRQ_ELEC(5,IREGN,N) = QMTBR(1,7,IREGN,N) * BENEL(IREGN,N)			        ! Bus transit
	  TRQ_ELEC(6,IREGN,N) = QMTBR(2,7,IREGN,N) * BENEL(IREGN,N)			        ! Bus intercity
	  TRQ_ELEC(7,IREGN,N) = cltfbtu(N,6,IREGN)* BENEL(IREGN,N)			        ! Commercial Light trucks (CLT)
	  TRQ_ELEC(8,IREGN,N) = SUM(BTQFREIRSC(1:3,6,IREGN))*TFRBTU_chgsplit(2,n)	! Freight trucks (light, medium, heavy) -- depot/fleet
	  TRQ_ELEC(9,IREGN,N) = SUM(BTQFREIRSC(1:3,6,IREGN))*TFRBTU_chgsplit(1,n)   ! Freight trucks (light, medium, heavy) -- non-fleet
	  TRQ_ELEC(10,IREGN,N)= TRQRAILR(1,IREGN,N)							        ! Passenger rail
        
!     national total
      DO isec = 1,10
        TRQ_ELEC(isec,MNUMCR,N) = TRQ_ELEC(isec,MNUMCR,N) + TRQ_ELEC(isec,IREGN,N)
      ENDDO
    ENDDO

!   2/3 Wheelers		
	TTHcons(1,n) = TRQHWY(3,N)                                   ! Motor Gasoline
	TTHcons(2,n) = 0.0                                           ! Diesel
	TTHcons(3,n) = 0.0                                           ! Liquid Petroleum Gas
	TTHcons(4,n) = 0.0                                           ! Electric
	TTHcons(5,n) = 0.0                                           ! Natural Gas	
	TTHcons(6,n) = 0.0                                           ! Hydrogen


!...FTAB Table 47 - LDV energy consumption by LDV type
    do ILDV=1,maxldv
      TRLDQTEK(ILDV,N) = 0.0  ! revisit this jma
    enddo

    do iregn=1,mnumcr-2
!...  Gas    
      TRLDQTEK(1,n) =  TRLDQTEK(1,n) + (sum(hhtechbtu(iregn,1:maxvtyp,1,n))*BENMG(iregn,n)) + FLTLDVBTUT(iregn,1,n)
!...  Diesel
      TRLDQTEK(2,n) =  TRLDQTEK(2,n) + (sum(hhtechbtu(iregn,1:maxvtyp,2,n))*BENDS(iregn,n)) + FLTLDVBTUT(iregn,2,n)
!...  FFV Ethanol
      TRLDQTEK(3,n) =  TRLDQTEK(3,n) + ((1.0-PCTAF(2,iregn,n))*sum(hhtechbtu(iregn,1:maxvtyp,3,n))*BENMG(iregn,n)) + &
                                        (PCTAF(2,iregn,n)*sum(hhtechbtu(iregn,1:maxvtyp,3,n))*BENET(iregn,n)) + &
                                         FLTLDVBTUT(iregn,3,n)
!...  Electric - 100 
      TRLDQTEK(4,n) =  TRLDQTEK(4,n) + (sum(hhtechbtu(iregn,1:maxvtyp,4,n))*BENEL(iregn,n)) + FLTLDVBTUT(iregn,7,n)
!...  PHEV20
      TRLDQTEK(5,n) =  TRLDQTEK(5,n) + ((1.0-PctPHEV20(n))*sum(hhtechbtu(iregn,1:maxvtyp,5,n))*BENMG(iregn,n)) + &
										(PctPHEV20(n)*sum(hhtechbtu(iregn,1:maxvtyp,5,n))*BENEL(iregn,n)) + &
										 FLTLDVBTUT(iregn,5,n)
!...  PHEV50
      TRLDQTEK(6,n) =  TRLDQTEK(6,n) + ((1.0-PctPHEV50(n))*sum(hhtechbtu(iregn,1:maxvtyp,6,n))*BENMG(iregn,n)) + &
										(PctPHEV50(n)*sum(hhtechbtu(iregn,1:maxvtyp,6,n))*BENEL(iregn,n)) + &
										 FLTLDVBTUT(iregn,6,n)
!...  Electric - 200 
      TRLDQTEK(7,n) =  TRLDQTEK(7,n) + (sum(hhtechbtu(iregn,1:maxvtyp,7,n))*BENEL(iregn,n)) + FLTLDVBTUT(iregn,7,n)
!...  HEV Diesel
      TRLDQTEK(8,n) =  TRLDQTEK(8,n) + (sum(hhtechbtu(iregn,1:maxvtyp,8,n))*BENDS(iregn,n)) + FLTLDVBTUT(iregn,8,n)
!...  Bi-Fuel CNG
      TRLDQTEK(9,n) =  TRLDQTEK(9,n) + ((1.0-PCTAF(3,iregn,n))*sum(hhtechbtu(iregn,1:maxvtyp,9,n))*BENMG(iregn,n)) + &
										(PCTAF(3,iregn,n)*sum(hhtechbtu(iregn,1:maxvtyp,9,n))*BENNG(iregn,n)) + &
										 FLTLDVBTUT(iregn,9,n)
!...  Bi-Fuel LPG
      TRLDQTEK(10,n) = TRLDQTEK(10,n) + ((1.0-PCTAF(4,iregn,n))*sum(hhtechbtu(iregn,1:maxvtyp,10,n))*BENMG(iregn,n)) + &
										 (PCTAF(4,iregn,n)*sum(hhtechbtu(iregn,1:maxvtyp,10,n))*BENLG(iregn,n)) + &
										  FLTLDVBTUT(iregn,10,n)
!...  AFV CNG
      TRLDQTEK(11,n) = TRLDQTEK(11,n) + (sum(hhtechbtu(iregn,1:maxvtyp,11,n))*BENNG(iregn,n)) + FLTLDVBTUT(iregn,11,n)
!...  AFV LPG
      TRLDQTEK(12,n) = TRLDQTEK(12,n) + (sum(hhtechbtu(iregn,1:maxvtyp,12,n))*BENLG(iregn,n)) + FLTLDVBTUT(iregn,12,n) 
!...  FCV Methanol
      TRLDQTEK(13,n) = TRLDQTEK(13,n) + (sum(hhtechbtu(iregn,1:maxvtyp,13,n))*BENME(iregn,n)) + FLTLDVBTUT(iregn,13,n)
!...  FCV Hydrogen
      TRLDQTEK(14,n) = TRLDQTEK(14,n) + (sum(hhtechbtu(iregn,1:maxvtyp,14,n))*BENHY(iregn,n)) + FLTLDVBTUT(iregn,14,n)
!...  Electric - 300  
      TRLDQTEK(15,n) = TRLDQTEK(15,n) + (sum(hhtechbtu(iregn,1:maxvtyp,15,n))*BENEL(iregn,n)) + FLTLDVBTUT(iregn,15,n)
!...  HEV Gasoline
      TRLDQTEK(16,n) = TRLDQTEK(16,n) + (sum(hhtechbtu(iregn,1:maxvtyp,16,n))*BENMG(iregn,n)) + FLTLDVBTUT(iregn,16,n)

    enddo

! ... ***** TABLE 52 *******

! ... Fuel efficiency - new conventional cars and light trucks
      TREFFCAR(:,N) = 0.0
      TREFFTRK(:,N) = 0.0
      DO icl=1,maxclass
        DO ildv=1,2
          if(LDV_MPG_CL(1,ildv,ICL,YRS).gt.0.0) TREFFCAR(ICL,N)        = TREFFCAR(ICL,N) + TOTALSALSC(1,icl,ildv,n)/LDV_MPG_CL(1,ildv,ICL,YRS)
          if(LDV_MPG_CL(2,ildv,ICL,YRS).gt.0.0) TREFFTRK(ICL,N)        = TREFFTRK(ICL,N) + TOTALSALSC(2,icl,ildv,n)/LDV_MPG_CL(2,ildv,ICL,YRS)
          if(LDV_MPG_CL(1,ildv,ICL,YRS).gt.0.0) TREFFCAR(MAXCLASS+1,N) = TREFFCAR(MAXCLASS+1,N) + TOTALSALSC(1,icl,ildv,n)/LDV_MPG_CL(1,ildv,ICL,YRS)
          if(LDV_MPG_CL(1,ildv,ICL,YRS).gt.0.0) TREFFCAR(MAXCLASS+2,N) = TREFFCAR(MAXCLASS+2,N) + TOTALSALSC(1,icl,ildv,n)/(LDV_MPG_CL(1,ildv,ICL,YRS)* degfac(1,ildv,n))
          if(LDV_MPG_CL(2,ildv,ICL,YRS).gt.0.0) TREFFTRK(MAXCLASS+1,N) = TREFFTRK(MAXCLASS+1,N) + TOTALSALSC(2,icl,ildv,n)/LDV_MPG_CL(2,ildv,ICL,YRS)
          if(LDV_MPG_CL(2,ildv,ICL,YRS).gt.0.0) TREFFTRK(MAXCLASS+2,N) = TREFFTRK(MAXCLASS+2,N) + TOTALSALSC(2,icl,ildv,n)/(LDV_MPG_CL(2,ildv,ICL,YRS)* degfac(2,ildv,n))
        ENDDO
        if(TREFFCAR(ICL,N).gt.0.0) TREFFCAR(ICL,N) = sum(TOTALSALSC(1,icl,[1,2],n))/TREFFCAR(ICL,N)
        if(TREFFTRK(ICL,N).gt.0.0) TREFFTRK(ICL,N) = sum(TOTALSALSC(2,icl,[1,2],n))/TREFFTRK(ICL,N)
      ENDDO
      
      if(TREFFCAR(MAXCLASS+1,N).gt.0.0) TREFFCAR(MAXCLASS+1,N) = sum(TOTALSALSC(1,:,[1,2],n))/TREFFCAR(MAXCLASS+1,N)
      if(TREFFCAR(MAXCLASS+2,N).gt.0.0) TREFFCAR(MAXCLASS+2,N) = sum(TOTALSALSC(1,:,[1,2],n))/TREFFCAR(MAXCLASS+2,N)
      if(TREFFTRK(MAXCLASS+1,N).gt.0.0) TREFFTRK(MAXCLASS+1,N) = sum(TOTALSALSC(2,:,[1,2],n))/TREFFTRK(MAXCLASS+1,N)
      if(TREFFTRK(MAXCLASS+2,N).gt.0.0) TREFFTRK(MAXCLASS+2,N) = sum(TOTALSALSC(2,:,[1,2],n))/TREFFTRK(MAXCLASS+2,N)

! ... Fuel efficiency - new AFVs (ildvs 3:16)
      DO ICL=1,MAXCLASS
        TREFFALTC(ICL,N) = AFVFE(1,icl,n)
        TREFFALTT(ICL,N) = AFVFE(2,icl,n)
      ENDDO

      TREFFALTC(MAXCLASS+1,N) = AFVFETOT(1,n)
      TREFFALTT(MAXCLASS+1,N) = AFVFETOT(2,n)
	  
      TREFFFLT(1,N) = FLTMPGTOT2(1)
      TREFFFLT(2,N) = FLTMPGTOT2(2)
      TREFFFLT(3,N) = FLTTOTMPG(1)
      TREFFFLT(4,N) = FLTTOTMPG(2)

! ... Size Class Sales shares	  
	  !output combined household and fleet class sales shares for all fuel types (aka all vehicles)
	  do icl=1,maxclass
		TRSLSHRC(icl,n) = sum(TOTALSALSC(1,icl,1:maxldv,n))/sum(TOTALSALSC(1,1:maxclass,1:maxldv,n))
		TRSLSHRT(icl,n) = sum(TOTALSALSC(2,icl,1:maxldv,n))/sum(TOTALSALSC(2,1:maxclass,1:maxldv,n))
	  enddo
	  
! ... Horsepower
      TRHPCAR(:,N) = 0.0
      TRHPTRK(:,N) = 0.0      
      do icl=1,maxclass
        do ildv = 1, maxldv
          TRHPCAR(ICL,N) = TRHPCAR(ICL,N) + TOTALSALSC(1,icl,ildv,n) * LDVHPW(1,ILDV,ICL,YRS)
          TRHPTRK(ICL,N) = TRHPTRK(ICL,N) + TOTALSALSC(2,icl,ildv,n) * LDVHPW(2,ILDV,ICL,YRS)
        enddo
        if (sum(TOTALSALSC(1,icl,:,n)).gt.0.0) TRHPCAR(ICL,N) = TRHPCAR(ICL,N) / sum(TOTALSALSC(1,icl,:,n))
        if (sum(TOTALSALSC(2,icl,:,n)).gt.0.0) TRHPTRK(ICL,N) = TRHPTRK(ICL,N) / sum(TOTALSALSC(2,icl,:,n))
      enddo
      
	  TRHPCAR(MAXCLASS+1,N) = AHPCAR(11,N)
	  TRHPTRK(MAXCLASS+1,N) = AHPTRUCK(11,N)
	  
! ... Weight
      TRWTCAR(:,N) = 0.0
      TRWTTRK(:,N) = 0.0      
      do icl=1,maxclass
        do ildv = 1, maxldv
          TRWTCAR(ICL,N) = TRWTCAR(ICL,N) + TOTALSALSC(1,icl,ildv,n) * WGT(1,ILDV,ICL,YRS)
          TRWTTRK(ICL,N) = TRWTTRK(ICL,N) + TOTALSALSC(2,icl,ildv,n) * WGT(2,ILDV,ICL,YRS)
        enddo
        if (sum(TOTALSALSC(1,icl,:,n)).gt.0.0) TRWTCAR(ICL,N) = TRWTCAR(ICL,N) / sum(TOTALSALSC(1,icl,:,n))
        if (sum(TOTALSALSC(2,icl,:,n)).gt.0.0) TRWTTRK(ICL,N) = TRWTTRK(ICL,N) / sum(TOTALSALSC(2,icl,:,n))
      enddo
      TRWTCAR(MAXCLASS+1,N) = AWTCAR(11,N)
	  TRWTTRK(MAXCLASS+1,N) = AWTTRUCK(11,N)
	  
!...FTAB Table 54 - Fleet vehicle sales ! jma
    do ivtyp=1,maxvtyp
      do ildv=1,maxldv
        FLTECHRPT(ivtyp,ildv,n) = sum(flt_stock(mnumcr,ivtyp,1:maxfleet,ildv,1,1:maxhav,n))/1000.0
      enddo
    enddo

!...Regionalize fleet sales for FTAB Table 48 (1,000s)
    do ivtyp=1,maxvtyp
      do ildv=1,maxldv
        do iregn=1,mnumcr-2
		  FLTECHRPT_REG(ivtyp,iregn,ildv,n) = sum(flt_stock(iregn,ivtyp,1:maxfleet,ildv,1,1:maxhav,n))/1000.0
        enddo
      enddo
    enddo
    FLTSALTOTC(n) = sum(fltechrpt(1,1:maxldv,n))
    FLTSALTOTT(n) = sum(fltechrpt(2,1:maxldv,n))
    FLTSALTOTV(n)=FLTSALTOTC(n)+FLTSALTOTT(n)

!...FTAB Table 55 - Fleet vehicle stocks (1,000s)
    do IVTYP=1,maxvtyp
      do ILDV=1,maxldv
        FLTECHSTKRPT(ivtyp,ildv,n)=sum(FLTECHSTK(mnumcr,ivtyp,1:maxfleet,ildv,1:maxhav))/1000.0
      enddo
    enddo
    FLTSTKTOTC(n) = sum(fltechstkrpt(1,1:maxldv,n)) 
    FLTSTKTOTT(n) = sum(fltechstkrpt(2,1:maxldv,n)) 
    FLTSTKTOTV(n)=FLTSTKTOTC(n)+FLTSTKTOTT(n)

!...FTAB Table 56 - Fleet vmt by ldv type (billion miles)
    do IVTYP=1,maxvtyp
      do ILDV=1,maxldv
        FLTECHVMTRPT(IVTYP,ILDV,n) = sum(BFLTVMTECH(ivtyp,1:maxfleet,ildv)) 	   
      enddo
    enddo

! ... ***** TABLE 58 *******

!...Freight Travel (billion ton miles travelled)	
    TRK_TMT(:,n) = 0.0
	if(curcalyr.gt.2012)then	
      ! Light Truck - class 3
      TRK_TMT(1,n)=sum(tfr_vmt_fas_t(n,1,1:12,2))*1.37
      ! Medium Truck - class 4-6
      TRK_TMT(2,n)=sum(tfr_vmt_fas_t(n,2,1:12,2))*3.58
      ! Heavy Truck
      TRK_TMT(3,n)=sum(tfr_vmt_fas_t(n,3,1:12,2))*10.7
    endif

! ... Railroad tmt & efficiency
      TRTMRR(1,N) = sum(BRTMTT(n,1:mnumcr-2))
      TRTMRR(2,N) = 1/FREFF(n)     

! ... Domestic shipping
      TRTMSHIP(1,N) = sum(BSTMTT(n,1:mnumcr-2))
      TRTMSHIP(2,N) = 1.0 / DSEFF(n)

! ... International shipping
      TRIMSHIP(N) = MC_MR(N)                  ! Real imports

! ... rail electricity demand passed to EMM
      do iregn=1,mnumcr-2      
        TRQRAILR(1,IREGN,N) = qmtrr(2,iregn,n)* benel(iregn,n)
        TRQRAILR(2,IREGN,N) = qmtrr(1,iregn,n)* bends(iregn,n)
      enddo

!...FTAB Table 48 new ldv sales by ldv type (millions)
!...Calculate total vehicle sales by tech and region  
!...regional ldv sales
	do iregn=1,mnumcr-2
	  do ildv=1,maxldv
	    TRLDSALC(ildv,iregn,n) = sum(LDV_STOCK(iregn,1,1:maxowner,ildv,1,1:maxhav,n))
	    TRLDSALT(ildv,iregn,n) = sum(LDV_STOCK(iregn,2,1:maxowner,ildv,1,1:maxhav,n))
	  enddo
	enddo
!...US ldv sales
	do ildv=1,maxldv
      TRLDSALC(ildv,11,n) = sum(TRLDSALC(ildv,1:mnumcr-2,n))
	  TRLDSALT(ildv,11,n) = sum(TRLDSALT(ildv,1:mnumcr-2,n))
	enddo

!...calculate total micro hybrid (ISG, BISG) vehicles sales jma update code to only count micros in gasoline, diesel and flexfuel vehicles
	if(curcalyr.eq.1995) trmicros(:,:,:,:) = 0.0
    if(curcalyr.ge.xyr)then 
	  do ivtyp=1,maxvtyp
        do ildv=1,maxldv
          do iregn=1,mnumcr
            if(ivtyp.eq.1) trmicros(ivtyp,ildv,iregn,n) = trldsalc(ildv,iregn,n)*micropen(ivtyp,ildv,n)
            if(ivtyp.eq.2) trmicros(ivtyp,ildv,iregn,n) = trldsalt(ildv,iregn,n)*micropen(ivtyp,ildv,n) 
            if(ildv.ge.4)  trmicros(ivtyp,ildv,iregn,n) = 0.0 
          enddo
        enddo
      enddo
	endif 

    DO IREGN=1,MNUMCR
      ANCSALE(IREGN,N) = 0.0
      ANTSALE(IREGN,N) = 0.0
      TNCSALE(IREGN,N) = 0.0
      TNTSALE(IREGN,N) = 0.0
      DO ILDV=3,MAXLDV
        ANCSALE(IREGN,N) = ANCSALE(IREGN,N) + TRLDSALC(ILDV,IREGN,N)
        ANTSALE(IREGN,N) = ANTSALE(IREGN,N) + TRLDSALT(ILDV,IREGN,N)
      ENDDO
      DO ILDV=1,MAXLDV
        TNCSALE(IREGN,N) = TNCSALE(IREGN,N) + TRLDSALC(ILDV,IREGN,N)
        TNTSALE(IREGN,N) = TNTSALE(IREGN,N) + TRLDSALT(ILDV,IREGN,N)
      ENDDO
    ENDDO

    DO IREGN=1,MNUMCR
      LEGALTSAL(1,IREGN,N) = 0.0
      IF (TNCSALE(IREGN,N) + TNTSALE(IREGN,N) .NE. 0.0) &
        LEGALTSAL(1,IREGN,N) = (ANCSALE(IREGN,N) + ANTSALE(IREGN,N)) / &
                               (TNCSALE(IREGN,N) + TNTSALE(IREGN,N))
      LEGALTSAL(3,IREGN,N) = TOTLEV(IREGN)
    ENDDO

    DO IREGN=1,MNUMCR-2
      LEGALTSAL(2,IREGN,N) = EPACTOT * RSHR(IREGN,N)
    ENDDO
    LEGALTSAL(2,11,N) = EPACTOT

!...FTAB Table 49 - LDV stock by ldv type
    do ILDV=1,maxldv
      TRLDSTKC(ILDV,n) = VSTK(1,ILDV)
      TRLDSTKT(ILDV,n) = VSTK(2,ILDV)
    enddo

!...FTAB Table 50 - LDV mpg by ldv type
    if (curcalyr.eq.1995) then
      TRLDMPGC(:,:) = 0.0
      TRLDMPGT(:,:) = 0.0
    endif
    
    TRLDMPGC(:,n) = 0.0
    TRLDMPGT(:,n) = 0.0
    do ildv=1,maxldv
      if(ldvmpgnew(mnumcr,1,ildv,n) .eq.ldvmpgnew(mnumcr,1,ildv,n)) TRLDMPGC(ILDV,n) = ldvmpgnew(mnumcr,1,ildv,n) 
      if(ldvmpgnew(mnumcr,2,ildv,n) .eq.ldvmpgnew(mnumcr,2,ildv,n)) TRLDMPGT(ILDV,n) = ldvmpgnew(mnumcr,2,ildv,n) 
    enddo
    TRLDMPGF(1,N) = TLDVMPG(1,N)
    TRLDMPGF(2,N) = TLDVMPG(2,N)
    TRLDMPGF(3,N) = TLDVMPG(3,N)
    
!   Table 50, sales-weighted new LDV mpg by powertrain, INCLUDING commercial light trucks {1:gas, 2:dsl, 3:hev, 4:phev, 5:bev, 6:fcv, 7:gaseous}
    TECHMPG(:,n) = 0.0
    TECHMPG_NUM(:) = 0.0
    do ildv = 1,maxldv
      impg = mpgmap(ildv)
      if (TRLDMPGC(ildv,n).gt.0.0) TECHMPG(impg,n) = TECHMPG(impg,n) + sum(cafesales(1:cargrp,:,yrs,ildv))/TRLDMPGC(ildv,n)
      if (TRLDMPGT(ildv,n).gt.0.0) TECHMPG(impg,n) = TECHMPG(impg,n) + sum(cafesales(ltkgrp:maxgroup,:,yrs,ildv))/TRLDMPGT(ildv,n)
      TECHMPG_NUM(impg) = TECHMPG_NUM(impg) + sum(cafesales(1:maxgroup,:,yrs,ildv))
    enddo
    
    do ildv = 1,12
      impg = mpgmapCLT(ildv)
      if (NCLTMPG(n,ildv).gt.0.0) TECHMPG(impg,n) = TECHMPG(impg,n) + CLTSALT(ildv,n)/1000000/NCLTMPG(n,ildv)
      if (NCLTMPG(n,ildv).gt.0.0) TECHMPG_NUM(impg) = TECHMPG_NUM(impg) + CLTSALT(ildv,n)/1000000
    enddo
    
    do impg = 1,7
      if(TECHMPG(impg,n).gt.0.0) TECHMPG(impg,n) = TECHMPG_NUM(impg)/TECHMPG(impg,n)
    enddo
    
!   Table 50, VMT-weighted stock LDV mpg by powertrain, INCLUDING commercial light trucks {1:gas, 2:dsl, 3:hev, 4:phev, 5:bev, 6:fcv}
    STKMPG(:,n) = 0.0
    TECHMPG_NUM(:) = 0.0
    do ildv = 1,maxldv
      impg = mpgmap(ildv)
      if (MPGTECH(ildv,n).gt.0.0) STKMPG(impg,n) = STKMPG(impg,n) + (sum(BVMT_STK_HH(1:2,ILDV,1:maxage,1,1:mnumcr-2)) + sum(BFLTVMTECH(1:2,:,ildv)))/MPGTECH(ILDV,n)
      if (MPGTECH(ildv,n).gt.0.0) TECHMPG_NUM(impg) = TECHMPG_NUM(impg) + sum(BVMT_STK_HH(1:2,ILDV,1:maxage,1,1:mnumcr-2)) + sum(BFLTVMTECH(1:2,:,ildv))
    enddo

    do ildv = 1,12
      impg = mpgmapCLT(ildv)
      if (CLTMPG(n,ildv).gt.0.0) STKMPG(impg,n) = STKMPG(impg,n) + BCLTVMT(ildv,n)/CLTMPG(n,ildv)
      if (CLTMPG(n,ildv).gt.0.0) TECHMPG_NUM(impg) = TECHMPG_NUM(impg) + BCLTVMT(ildv,n)
    enddo
    
    do impg = 1,7
      if(STKMPG(impg,n).gt.0.0) STKMPG(impg,n) = TECHMPG_NUM(impg)/STKMPG(impg,n)
    enddo
      
!...Calculate national stock-vmt weighted ldv fuel economy
	do ildv=1,maxldv 
		NUM1=0.0
		DEN1=0.0
		NUM2=0.0
		DEN2=0.0
		NUM3=0.0
		DEN3=0.0
	  do iage=1,maxage	!add all the ages and regions for hh
	    do ivtyp=1,maxvtyp
	      do iregn = 1,mnumcr-2
			if(ivtyp.eq.1)then
		      if(hhmpgstk(iregn,ivtyp,ildv,iage,n).ne.0.0) then
			    NUM1=NUM1 + VMT_STK_HH(1,ILDV,iage,1,iregn)
			    DEN1=DEN1 +(VMT_STK_HH(1,ILDV,iage,1,iregn)/hhmpgstk(iregn,ivtyp,ildv,iage,n))
		      endif
			else
		      if(hhmpgstk(iregn,ivtyp,ildv,iage,n).ne.0.0) then
		        NUM2=NUM2 + VMT_STK_HH(2,ILDV,iage,1,iregn)
			    DEN2=DEN2 +(VMT_STK_HH(2,ILDV,iage,1,iregn)/hhmpgstk(iregn,ivtyp,ildv,iage,n))
			  endif
			endif
		  enddo !end iregn loop
		enddo
	  enddo !end  iage loop

!...  sum fleet vmt and consumption by fleet type, which is already national
	  FLTNUM1=sum(fltvmtech(1,1:maxfleet,ILDV,1:maxhav))/1000000.0
      FLTNUM2=sum(fltvmtech(2,1:maxfleet,ILDV,1:maxhav))/1000000.0
      FLTDEN1=sum(fltechgge(mnumcr,1,1:maxfleet,ILDV,n))
      FLTDEN2=sum(fltechgge(mnumcr,2,1:maxfleet,ILDV,n))
 
!...  add fleet vmt and consumption to household vehicles (for each ILDV)
      NUM1=NUM1+FLTNUM1
      NUM2=NUM2+FLTNUM2
      DEN1=DEN1+FLTDEN1
      DEN2=DEN2+FLTDEN2
	  
!...  sum vmt and consumption for car and light truck
      NUM3=NUM1+NUM2
      DEN3=DEN1+DEN2
!...  calculate mpg by ILDV
      cmpg_1(ILDV)=0.0
      tmpg_1(ILDV)=0.0
      lmpg_1(ILDV)=0.0
      if(DEN1.ne.0.0) cmpg_1(ILDV)=NUM1/DEN1
      if(DEN2.ne.0.0) tmpg_1(ILDV)=NUM2/DEN2
      if(DEN3.ne.0.0) lmpg_1(ILDV)=NUM3/DEN3
!...  sum vmt by ILDV
	  do IVTYP = 1,maxvtyp
		VMT_STK_TOT(IVTYP,ILDV) = sum(VMT_STK_HH(IVTYP,ILDV,1:maxage,1,mnumcr))+(sum(fltvmtech(IVTYP,1:maxfleet,ILDV,1:maxhav))/1000000.0)
	  enddo
    enddo	!end ILDV loop
!...calculate vmt shares by ILDV
    do ILDV=1,maxldv
      if(cmpg_1(ILDV).ne.0.0) cmpg_2(ILDV)=(VMT_STK_TOT(1,ILDV)/sum(VMT_STK_TOT(1,1:maxldv)))/cmpg_1(ILDV)
      if(tmpg_1(ILDV).ne.0.0) tmpg_2(ILDV)=(VMT_STK_TOT(2,ILDV)/sum(VMT_STK_TOT(2,1:maxldv)))/tmpg_1(ILDV)
      if(lmpg_1(ILDV).ne.0.0) lmpg_2(ILDV)=(sum(VMT_STK_TOT(1:maxvtyp,ILDV))/(sum(VMT_STK_TOT(1,1:maxldv))+sum(VMT_STK_TOT(2,1:maxldv))))/lmpg_1(ILDV)
    enddo
!...vmt weighted stock fuel economy for table 7 and table 50
    trldmpgf(1,n)=sum(cmpg_2(1:maxldv))**-1.0
    trldmpgf(2,n)=sum(tmpg_2(1:maxldv))**-1.0
    trldmpgf(3,n)=sum(lmpg_2(1:maxldv))**-1.0

!...FTAB Table 51 - total ldv vmt by ldv type (billion miles)
    do ILDV=1,maxldv
	  TRLDVMT(ILDV,n) = BVMTECH(ILDV,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,ILDV))
    enddo
    
	TRLDVMTE(1,N)  = sum(BVMTECH(1:maxldv,mnumcr)) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,1:maxldv))	!dst vmt
    TRLDVMTE(2,N)  = trldvmte(1,n)/SUM(LicDriver(1:AGEGRP,1:MF,11,n))  
    TRLDVMTE(3,N)  = SUM(LicDriver(1:AGEGRP,1:MF,11,n)) 
    TRLDVMTE(4,N)  = PMGTR(11,N)
	TRLDVMTE(5,N)  = MPGHH(N)
    TRLDVMTE(6,N)  = COSTMI(N)
    TRLDVMTE(7,N)  = SUM(LicDriver(1:AGEGRP,1:MF,1:MNUMCR-2,n))/sum(TMC_NP15A(1:agegrp,1:mf,1:mnumcr-2,n)) 
    TRLDVMTE(8,N)  = MC_YPDR(11,N)/MC_NP16A(11,N)
    TRLDVMTE(9,N)  = IE(N)
    TRLDVMTE(10,N) = sum(licdriver(1:agegrp,2,11,n))/sum(licdriver(1:agegrp,1:mf,11,n)) 
    TRLDVMTE(11,N) = trldvmte(1,n)/sum(vstk(1:maxvtyp,1:maxldv))
    TRLDVMTE(12,N) = 1.0

!   Energy Efficiency Indicators (mpg) (weighted average for LDV and CLT)
	ldvshr=(tncsale(11,n)+tntsale(11,n))/(tncsale(11,n)+tntsale(11,n)+(SUM(cltsalt(1:12,n))*.000001))
	LDV_MPG(1,n)=(ldvshr/cafestd(3,n)+(1-ldvshr)/ncltmpgt(n))**-1  
	LDV_MPG(2,n)=(ldvshr/truempg(3,n)+(1-ldvshr)/ncltmpgt(n))**-1
	ldvshr=trldvmte(1,n)/(trldvmte(1,n)+sum(bcltvmt(1:12,n)))
	LDV_MPG(3,n)=(ldvshr/trldmpgf(3,n)+(1-ldvshr)/cltmpgt(n))**-1
    
!...FTAB TABLES 113 - 115 
!...fuel economy, vehicle price (MSRP), and range by ldv type and size class
	do IVTYP=1,MAXVTYP
	  do ILDV=1,MAXLDV
	    do ICL=1,MAXCLASS
		  LDVMPG(IVTYP,ILDV,ICL,yrs) = LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)
		  LDVPRI(IVTYP,ILDV,ICL,yrs) = LDV_PRI(IVTYP,ILDV,ICL,yrs)
		  LDVRNG(IVTYP,ILDV,ICL,yrs) = LDV_RNG(IVTYP,ILDV,ICL,yrs)
		enddo
	  enddo
	enddo
	
  RETURN
  END SUBROUTINE TREPORT

! ==========================================================================================================
! ... Subroutine FEMRANGE calculates vehicle range estimates
! ==========================================================================================================
    SUBROUTINE FEMRANGE
    USE T_
    IMPLICIT NONE

      DO IGP=1,MAXGROUP
	    ivtyp = grpmap(igp)
        DO ICL=1,MAXCLASS
          DO ILDV=1,MAXLDV
            IF (.NOT. CLASSFLAG(ICL,IGP,ILDV)) CYCLE
			RANGE(ICL,IGP,CURRENT,ILDV) = TANKSIZE(ICL,IGP,CURRENT,ildv) * FE(ICL,IGP,CURRENT,ildv) * degfacgrp(igp,icl,ildv,n)
!... 		Electrics have a range based on battery capacity calculated in EVCALC subroutine
            if(ILDV.eq.4) RANGE(ICL,IGP,current,ILDV) = EV_range(ICL,IGP,ILDV,yrs)
            if(ILDV.eq.7) RANGE(ICL,IGP,current,ILDV) = EV_range(ICL,IGP,ILDV,yrs)
            if(ILDV.eq.15) RANGE(ICL,IGP,current,ILDV) = EV_range(ICL,IGP,ILDV,yrs)	
          ENDDO
        ENDDO
      ENDDO

    RETURN
    END SUBROUTINE FEMRANGE

! ==========================================================================================================
! ... This subroutine calculates the base (and historic) year price, weight, fuel economy, and horsepower 
! ... for alternative fuel vehicles.  Most of these are set relative to the gasoline vehicle values.  Note 
! ... that all attributes are assigned on the basis of and stored in the "current year" elements of each 
! ... parameter, so it is critical that these paremeters be set correctly prior to the AFVADJ call and also 
! ... that the last call is for the actual FEM base year.
! ==========================================================================================================
    SUBROUTINE AFVADJ (ZYR)
    USE T_ 
    IMPLICIT NONE

    INTEGER     ZYR

!...calculate lithium ion battery cost ($/kwh).		
!...Needs to happen before the first battery year, to populate historical cumulative_gwh for freight model
!	IF(YRS.GE.FIRST_BAT_YR+1989-1) CALL LIONCOSTCALC !2024

		  ivtyp = grpmap(IGP)
!...		Calculate BASE values for vehicle introduction year greater than XYR
            IF(ZYR.GT.XYR.and.classflag(ICL,IGP,ILDV)) then
			  if(GRPFLAG(ILDV,ICL,IGP).EQ.ZYR) then  
                WEIGHT(ICL,IGP,BASE,ILDV)  = WEIGHT(ICL,IGP,CURRENT,GAS) * AFVADJWT(ILDV,ivtyp)
                FE(ICL,IGP,BASE,ILDV)      = FE(ICL,IGP,CURRENT,GAS)     * AFVADJFE(ILDV,ivtyp)
                HP(ICL,IGP,BASE,ILDV)      = HP(ICL,IGP,CURRENT,GAS)     * AFVADJHP(ILDV,ivtyp)
                TANKSIZE(ICL,IGP,BASE,ILDV)= TANKSIZE(ICL,IGP,CURRENT,GAS)
                PRICE(ICL,IGP,BASE,ILDV)   = PRICE(ICL,IGP,CURRENT,GAS)  + AFVADJPR(ILDV,ivtyp)
                DO ITECH=1,NUMTECH
                  MKT_PEN(ICL,IGP,ITECH,BASE,ILDV) = MKT_PEN(ICL,IGP,ITECH,BASE,GAS)
                ENDDO
			  endif
			ENDIF

!...        Calculate CURRENT values for vehicle introduction year
		    if(classflag(ICL,IGP,ILDV)) then
              IF(ZYR.gt.xyr.and.GRPFLAG(ILDV,ICL,IGP).eq.ZYR) then ! introduction year 
                PRICE(ICL,IGP,CURRENT,ILDV)    = PRICE(ICL,IGP,CURRENT,GAS)  + AFVADJPR(ILDV,ivtyp)
                FE(ICL,IGP,CURRENT,ILDV)       = FE(ICL,IGP,CURRENT,GAS)     * AFVADJFE(ILDV,ivtyp)
                WEIGHT(ICL,IGP,CURRENT,ILDV)   = WEIGHT(ICL,IGP,CURRENT,GAS) * AFVADJWT(ILDV,ivtyp)
                HP(ICL,IGP,CURRENT,ILDV)       = HP(ICL,IGP,CURRENT,GAS)     * AFVADJHP(ILDV,ivtyp)
                TANKSIZE(ICL,IGP,CURRENT,ILDV) = TANKSIZE(ICL,IGP,CURRENT,GAS)
!...			All electric drive vehicles have an additional battery and fuel cell price, horsepower, and weight adjustments
                if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) then
                  if(ILDV.eq.4.or.ILDV.eq.7.or.ILDV.eq.15) call EVCALC (zyr)
				  if(ILDV.eq.5.or.ILDV.eq.6) call PHEVCALC (zyr)
                  if(ILDV.eq.8.or.ILDV.eq.16) call HEVCALC (zyr)
                  if(ILDV.ge.13.and.ILDV.le.14) call FCCALC (zyr)
                endif
!... 			Assume base and historic technology penetrations for AFVs are the same as gasoline
                DO ITECH=1,NUMTECH
                  MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) = MKT_PEN(ICL,IGP,ITECH,CURRENT,GAS)
                  MKT_MAX(ICL,IGP,ITECH,ILDV)         = MKT_MAX(ICL,IGP,ITECH,GAS)
                ENDDO
			  endif
            ENDIF

    RETURN
    END SUBROUTINE AFVADJ

! ==========================================================================================================
!...Subroutine LIONCOSTCALC
!   Description:
!       Calculates lithium-ion battery cost ($/kWh) for battery electric vehicles
! ==========================================================================================================
  SUBROUTINE LIONCOSTCALC
  USE T_
  IMPLICIT NONE

!...Local variable dictionary
    INTEGER :: ic,it                                     !...temporary looping counter
	REAL :: annual_gwh(mnumyr)                           !...annual EV and PHEV li-ion battery production
                                                         !...cumulative_gwh(mnumyr) = cumulative EV and PHEV li-ion battery production (defined in tranmain)
	REAL :: avg_kwh_rpt(3,maxldv,mnumyr)			 	 !...avg batt kWh per vehicle by car/LT (and total) and PHEV/BEV type
	REAL :: NUM1,NUM2(maxvtyp)							 !...components of avg batt kWh calculation

!...Calculate cumulative production (GWh) of li-ion batteries in EVs and PHEVs
!   The first two years LIONCOSTCALC is called, sum up historical GWh (1995 to current)
!   Average kwh capacity calculated for vehicle classes that have capacity and 
!   multiplied by annual sales in each fuel type and vehicle type (car/light truck)
	if(n.le.first_bat_yr)then
	  annual_gwh(:) 	= 0.0
	  cumulative_gwh(:) = 0.0
	  do ic = 6,n
        do igp = 1, maxgroup
          do icl = 1, maxclass
            do ILDV=1,maxldv
              if (BatPackSize(ic+1989-1,ICL,IGP,ILDV).eq.0.0) CYCLE
	  		  if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.ge.15) &
                annual_gwh(ic) = annual_gwh(ic) + ldv_sales(igp,icl,ildv,mnumcr,ic-1)*BatPackSize(ic+1989-1,ICL,IGP,ILDV)
!	  	      WRITE(21,'(a,6(i4,","),3(f12.3,","))')'lioncost_debug1',curcalyr,curitr,ic,igp,icl,ildv,ldv_sales(igp,icl,ildv,mnumcr,ic-1)/1000,BatPackSize(ic+1989-1,ICL,IGP,ILDV),annual_gwh(ic)
            enddo ! maxldv
	  	  enddo
	    enddo
        cumulative_gwh(ic) = cumulative_gwh(ic-1) + annual_gwh(ic)
      enddo ! yr
!   In subsequent years, calculate previous year GWh and add to cumulative total to estimate current year cost
!   Average KWh capacity for each fuel type, class, and vehicle type (Car/light truck) multiplied by sales
!   for each fuel type, class, and vehicle type.
	else
	  annual_gwh(n) = 0.0
      do ildv=1,maxldv
	    do igp=1,maxgroup
	  	  do icl=1,maxclass
	  	    if (BatPackSize(ic+1989-1,ICL,IGP,ILDV).eq.0.0) CYCLE
            if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.ge.15) &
              annual_gwh(n) = annual_gwh(n) + ldv_sales(igp,icl,ildv,mnumcr,n-1)*BatPackSize(n+1989-1,ICL,IGP,ILDV)
!	  	      WRITE(21,'(a,",",6(i4,","),3(f12.3,","))')'lioncost_debug2',curcalyr,curitr,ic,igp,icl,ildv,ldv_sales(igp,icl,ildv,mnumcr,ic-1)/1000,BatPackSize(ic+1989-1,ICL,IGP,ILDV),annual_gwh(ic)
          enddo ! maxldv
	    enddo ! maxclass
	  enddo ! igp
	  cumulative_gwh(n) = cumulative_gwh(n-1) + annual_gwh(n)
	endif ! end check for first bat year
	
!...Calculation of cumulative production-based lithium-ion battery cost ($/kWh) by vehicle type
!	In the first projection year, the default learning rate (16.5%) is assumed. This prevents the need for re-calibration to the "pseuo-historical" year
!	(usually the first projection year sales are calibrated based on whatever we have YTD from Ward's).  After the first projection year, the model re-estimates 
!	the base price coefficient (pack_a) for the curve, using the learning rate provided in trnldvx.xlsx.
	if (n.ge.first_bat_yr) then
      do ILDV=1,maxldv
	    if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) then
!	  	  Align curve with historical data
	      if(n.eq.first_bat_yr) then
            pack_a(ILDV) = (li_ion_cost(ILDV,yrs)-mat_a(ILDV)*mat_markup(n)*cumulative_gwh(n-1)**(-(-LOG(1.0)/LOG(2.0))))/(cumulative_gwh(n-1)**(-(-LOG(1.0-0.165)/LOG(2.0))))	
	  	  elseif(n.eq.first_bat_yr+1) then		! Assume standard learning rate (16.5%) through first projection year (year that we calibrate sales to Ward's based on YTD values)
	  	    li_ion_cost(ILDV,yrs) = pack_a(ILDV)*cumulative_gwh(n)**(-(-LOG(1.0-0.165)/LOG(2.0))) + mat_a(ILDV)*mat_markup(n)*cumulative_gwh(n)**(-(-LOG(1.0)/LOG(2.0)))		
	      else								! If past the first projection year, use the user-defined learning rate (pack_b)
	      	if(n.eq.first_bat_yr+2) then		! Align curve coefficient -- pack_a -- so that the user-defined pack_b produces the same cost in first_bat_yr+2
	      	  pack_a(ILDV) = (li_ion_cost(ILDV,yrs-1)-mat_a(ILDV)*mat_markup(n)*cumulative_gwh(n-1)**(-mat_b(ILDV)))/(cumulative_gwh(n-1)**(-pack_b(ILDV)))
	      	endif
	      	li_ion_cost(ILDV,yrs) = pack_a(ILDV)*cumulative_gwh(n)**(-pack_b(ILDV)) + mat_a(ILDV)*mat_markup(n)*cumulative_gwh(n)**(-mat_b(ILDV))
	      endif
	    endif
	  enddo
    endif

!	Write out battery production, costs, and capacity per vehicle
	if(n.eq.MNUMYR.and.FCRL.eq.1)then
		write(21,*) "LIONCOSTCALC_debug_2020USD"
		write(21,'(a5,",",8(a12,","))') 'year','ann_gwh','cumul_gwh','ev100','phev20','phev50','ev200','ev300','hev'
		do ic=6,n
			write(21, '(I5,", ", 8(F12.1,", "))') ic+1989, annual_gwh(ic), cumulative_gwh(ic), li_ion_cost([4,5,6,7,15,16],ic+1989)/ MC_JPGDP(1) * MC_JPGDP(31)
		enddo
		write(21,*) "LIONCOSTCALC_debug_kWhPerVeh"
		write(21,*) "avg_kwh_rpt (ILDV=4:7 and 15:16)"
		
		do ic=26,mnumyr
		  do ildv=1,MAXLDV
		    NUM1    = 0.0
		    NUM2(:) = 0.0
            if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.ge.15) then
		      do igp = 1, maxgroup
			    it=GrpMap(igp)
		        do icl=1,maxclass
		          NUM2(it) = NUM2(it) + sum(ldv_sales(igp,icl,ildv,1:mnumcr-2,ic))*BatPackSize(ic+1989,ICL,IGP,ILDV)   !avg_kwh(it,ILDV,ICL,ic+1989)*TOTALSALSC(it,ICL,ILDV,ic)
				  NUM1     = NUM1     + sum(ldv_sales(igp,icl,ildv,1:mnumcr-2,ic))*BatPackSize(ic+1989,ICL,IGP,ILDV)   !avg_kwh(it,ILDV,ICL,ic+1989)*TOTALSALSC(it,ICL,ILDV,ic)
		  	    enddo
			  enddo
			  avg_kwh_rpt(1,ILDV,ic) = NUM2(1)/sum(ldv_sales([1:5],:,ildv,1:mnumcr-2,ic))
              avg_kwh_rpt(2,ILDV,ic) = NUM2(2)/sum(ldv_sales([6:11],:,ildv,1:mnumcr-2,ic))
              avg_kwh_rpt(3,ILDV,ic) = NUM1/sum(ldv_sales(:,:,ildv,1:mnumcr-2,ic))
			  if (avg_kwh_rpt(3,ILDV,ic).ne.avg_kwh_rpt(3,ILDV,ic)) &
                WRITE(21,'(a,2(i4,","),5(f12.2,","))')'ERROR', ILDV,ic+1989,NUM1, NUM2(:), sum(ldv_sales([1:5],:,ildv,1:mnumcr-2,ic)),sum(ldv_sales([6:11],:,ildv,1:mnumcr-2,ic))
			ENDIF
		  enddo
		enddo
		
        WRITE(21,'(a5,",",6(a8,","))')'year','EV100','PHEV20','PHEV50','EV200','EV300','HEV'
		WRITE(21,*)'batt_size_total'
		do ic=26,mnumyr
		  write(21, '(I5,", ", 6(F8.2,", "))') ic+1989, avg_kwh_rpt(3,[4,5,6,7,15,16],ic)
		enddo
		WRITE(21,*)'batt_size_car'
		do ic=26,mnumyr
		  write(21, '(I5,", ", 6(F8.2,", "))') ic+1989, avg_kwh_rpt(1,[4,5,6,7,15,16],ic)
		enddo
		WRITE(21,*)'batt_size_lt'
		do ic=26,mnumyr
		  write(21, '(I5,", ", 6(F8.2,", "))') ic+1989, avg_kwh_rpt(2,[4,5,6,7,15,16],ic)
		enddo
		
	endif

  RETURN
  END SUBROUTINE LIONCOSTCALC
  
! ==========================================================================================================
!...Subroutine HEVCALC 
!   Description:
!       Calculates hybrid electric vehicle battery size, price, weight, and horsepower adjustments
! ==========================================================================================================
  SUBROUTINE HEVCALC (dsyrs)
  USE T_
  IMPLICIT NONE

!...Local variable definitions
	integer       dsyrs
    REAL, PARAMETER :: NiMHweight_per_kWh = 53.42       		! NiMH weight (lbs) per kWh battery capacity
    REAL, PARAMETER :: NiMHkWh_per_pound  = 0.0004989   		! Battery sizing factor (kWh) based on vehicle weight
    REAL			:: Lii_kWhr(MAXCLASS,MAXGROUP,MAXLDV)		! HEV lithium ion battery pack size
	REAL			:: Nmh_KWhr(MAXCLASS,MAXGROUP,MAXLDV)       ! HEV nickel metal hydride battery pack size
	
!...Calculate required battery size based on vehicle weight and battery type
!   Incorporate improvements in DOD (LIONkWh_perlb was estimated based on pack sizing in EPALYR)
    lii_kWhr(ICL,IGP,ILDV) = weight(ICL,IGP,current,gas) * LIONkWh_perLb(ICL,IGP,ILDV) * (phev_dod(epalyr)/phev_dod(dsyrs))
    nmh_kWhr(ICL,IGP,ILDV) = weight(ICL,IGP,current,gas) * NiMHkWh_per_pound

    if(lii_kWhr(ICL,IGP,ILDV)*Li_ion_Cost(ILDV,dsyrs).lt.nmh_kWhr(ICL,IGP,ILDV)*nimh_cost(dsyrs)) then
	  BatPackSize(dsyrs,icl,igp,ildv) = lii_kWhr(ICL,IGP,ILDV)
      ElecSysIncCost(ICL,IGP,current,ILDV) = lii_kWhr(ICL,IGP,ILDV)*Li_ion_Cost(ILDV,dsyrs) + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)
	  BatPackWgt(dsyrs,ICL,IGP,ILDV) = LION_LB_perkWh(ILDV) * lii_kWhr(ICL,IGP,ILDV)
	else
	  BatPackSize(dsyrs,icl,igp,ildv) = nmh_kWhr(ICL,IGP,ILDV)
      ElecSysIncCost(ICL,IGP,current,ILDV) = nmh_kWhr(ICL,IGP,ILDV)*nimh_cost(dsyrs) + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)
	  BatPackWgt(dsyrs,ICL,IGP,ILDV) = NiMHweight_per_kWh * nmh_kWhr(ICL,IGP,ILDV) 
	endif

    if (ElecSysIncCost(ICL,IGP,prev,ILDV).eq.0.0) ElecSysIncCost(ICL,IGP,prev,ILDV) = ElecSysIncCost(ICL,IGP,current,ILDV)
	
!...Calculate total battery electric vehicle price
    PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)

!    WRITE(21,'(a,",",5(i4,","),4(f12.2,","))')'hev_price',curcalyr,curitr,igp,icl,ildv,PRICE(icl,igp,current,ildv),PRICE(icl,igp,prev,ildv),ElecSysIncCost(icl,igp,current,ildv),&
!                                                ElecNonBattCst(icl,dsyrs,ivtyp,ildv)


!...Calculate battery electric vehicle weight
	if(weight(icl,igp,prev,ildv).eq.0.0) WEIGHT(icl,igp,current,ildv) = weight(icl,igp,current,gas)+BatPackWgt(dsyrs,icl,igp,ildv)

!...Assume HEVs have equivalent performance to conventional gasoline vehicle
    if(weight(icl,igp,prev,ildv).eq.0.0) HP(icl,igp,current,ildv) = HP(icl,igp,current,gas) * (weight(icl,igp,current,ildv)/weight(icl,igp,current,gas))
	
!...fuel economy
	if(fe(icl,igp,prev,ildv).eq.0.0.or.(grpflag(ildv,icl,igp).eq.curcalyr.and.fempri(igp,icl,epalyr,ildv).eq.0.0)) then 
	  if(igp.le.cargrp) fe(icl,igp,current,ildv) = fe(icl,igp,current,gas) * 1.646
	  if(igp.ge.ltkgrp) fe(icl,igp,current,ildv) = fe(icl,igp,current,gas) * 1.45
	endif

!...tank size
	if(tanksize(icl,igp,prev,ildv).eq.0.0) then 
	  if(igp.le.cargrp) tanksize(icl,igp,current,ildv) = fe(icl,igp,current,ildv) / 5.44
	  if(igp.ge.ltkgrp) tanksize(icl,igp,current,ildv) = fe(icl,igp,current,ildv) / 3.92
	endif

!...fill base and previous values
	if(grpflag(ildv,icl,igp).eq.curcalyr.and.fempri(igp,icl,epalyr,ildv).eq.0.0) then
	  do i=base,prev
		weight(icl,igp,i,ildv) = weight(icl,igp,current,ildv)
		fe(icl,igp,i,ildv) = fe(icl,igp,current,ildv)
		hp(icl,igp,i,ildv) = hp(icl,igp,current,ildv)
		tanksize(icl,igp,i,ildv) = tanksize(icl,igp,current,ildv)
		price(icl,igp,i,ildv) = price(icl,igp,current,ildv)
	  enddo
	endif

  RETURN
  END SUBROUTINE HEVCALC
  
! ==========================================================================================================
!...Subroutine PHEVCALC
!   Description:
!      Calculates plug-in hybrid electric vehicle (35+ mile range) battery size, price, weight, and horsepower
!	   1) Battery size is estimated using the historical relationship between total battery kWh and vehicle weight.
!	   2) If the calculated battery size is close to the threshold for a tax credit, the size is increased.
!	   3) Weight is increased due to the addition of a battery; Horsepower/weight ratio is assumed equivalent to gasoline vehicles
!	   4) Range is estimated based on historical relationship between pack size and range
!	   5) Fuel economy is estimated based on electric/gas shares of PHEV VMT
! ==========================================================================================================
  SUBROUTINE PHEVCALC (dsyrs)
  USE T_
  IMPLICIT NONE
  
!...local variable definitions
	integer       dsyrs							

!...Calculate required battery size based on vehicle weight and depth of discharge improvement
!   If the vehicle existed last year -- shrink the pack by the improvement in DOD
    if(batpacksize(dsyrs-1,icl,igp,ildv).gt.0.0) then 
	  BatPackSize(dsyrs,icl,igp,ildv) = batpacksize(dsyrs-1,icl,igp,ildv) * (phev_dod(dsyrs-1)/phev_dod(dsyrs))
!   If the vehicle didn't exist last year -- estimate using kWh/lb parameter (which was developed from epalyr data, so need to adjust by improvements in DOD since then)
	else
	  BatPackSize(dsyrs,icl,igp,ildv) = weight(icl,igp,current,gas) * LIONkWh_perLb(ICL,IGP,ILDV) * (phev_dod(epalyr)/phev_dod(dsyrs))
	endif

    IF (BatPackSize(dsyrs,icl,igp,ildv).le.0.0.or.BatPackSize(dsyrs,icl,igp,ildv).ne.BatPackSize(dsyrs,icl,igp,ildv)) THEN
      WRITE(21,'(a,",",5(i4,","),4(f9.2,","))')'ERROR: PHEV pack size',curitr,curcalyr,igp,icl,ildv,pass,BatPackSize(dsyrs,icl,igp,ildv),LIONkWh_perLb(ICL,IGP,ILDV),phev_dod(epalyr),weight(icl,igp,current,gas)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF

!...Calculate total battery electric vehicle incremental cost
	ElecSysIncCost(icl,igp,current,ildv) = (BatPackSize(dsyrs,icl,igp,ildv)*Li_ion_Cost(ildv,dsyrs)) + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)
    if (ElecSysIncCost(ICL,IGP,prev,ILDV).eq.0.0) ElecSysIncCost(ICL,IGP,prev,ILDV) = ElecSysIncCost(ICL,IGP,current,ILDV)

!...Calculate battery electric vehicle price
    PRICE(icl,igp,current,ildv) = PRICE(icl,igp,current,ildv) + ElecSysIncCost(icl,igp,current,ildv)

!    WRITE(21,'(a,",",5(i4,","),4(f12.2,","))')'phev_price',curcalyr,curitr,igp,icl,ildv,PRICE(icl,igp,current,ildv),PRICE(icl,igp,prev,ildv),ElecSysIncCost(icl,igp,current,ildv),&
!                                        ElecNonBattCst(icl,dsyrs,ivtyp,ildv)

!...Calculate battery electric vehicle weight
	if(weight(icl,igp,prev,ildv).ne.0.0) then 
	  weight(icl,igp,current,ildv) = weight(icl,igp,prev,ildv)
	else
      weight(icl,igp,current,ildv) = weight(icl,igp,current,ildv)+(BatPackSize(dsyrs,icl,igp,ildv)*LION_LB_perkWh(ildv))  
	endif
	
!...Assume PHEVs have equivalent performance (HP/weight ratio) to conventional gasoline vehicle
	if(hp(icl,igp,prev,ildv).eq.0.0) then
      if(weight(icl,igp,current,gas).ne.0.0) HP(icl,igp,current,ildv) = HP(icl,igp,current,gas) * (weight(icl,igp,current,ildv)/weight(icl,igp,current,gas)) 
	endif

!...Estimate Electric range
    if(BatPackSize(dsyrs-1,icl,igp,ildv).gt.0.0) then 
      EV_Range(icl,igp,ildv,dsyrs) = EV_range(icl,igp,ildv,dsyrs-1)
    else
	  EV_Range(icl,igp,ildv,dsyrs) = EV_range_m(ildv) * (BatPackSize(dsyrs,icl,igp,ildv)) + EV_range_b(ildv)
	endif

    IF (EV_Range(icl,igp,ildv,dsyrs).le.0.0.or.EV_Range(icl,igp,ildv,dsyrs).ne.EV_Range(icl,igp,ildv,dsyrs)) THEN
      WRITE(21,'(a,",",5(i4,","),7(f9.2,","))')'ERROR: PHEV range',curitr,curcalyr,igp,icl,ildv,pass,EV_Range(icl,igp,ildv,dsyrs),RANGE(icl,igp,prev,ildv),EV_range_m(ildv),EV_range_b(ildv)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF

!...Calcualte EV fuel economy    
	FE(icl,igp,current,ildv) = (EV_Range(icl,igp,ildv,dsyrs)/(BatPackSize(dsyrs,icl,igp,ildv)*phev_dod(dsyrs))) * & 
	                            (MG_HHV*1000.0*(1.0/3412.0)) * evmpg_adj(igp,icl,ildv)
    
    PHEVMPG_D(igp,icl,yrs,ildv) = FE(icl,igp,current,ildv)
    
    if(ildv.eq.5) then
      if (FE(ICL,IGP,current,gas).gt.0.0) then
        PHEVMPG_S(igp,icl,yrs,ildv) = FE(ICL,IGP,current,gas)*csratio(icl,igp,1)
      else
        PHEVMPG_S(igp,icl,yrs,ildv) = (FE(ICL,IGP,current,ILDV)/2.464)
      endif
    elseif(ildv.eq.6) then
      if (FE(ICL,IGP,current,gas).gt.0.0) then
        PHEVMPG_S(igp,icl,yrs,ildv) = FE(ICL,IGP,current,gas)*csratio(icl,igp,2)
      else
        PHEVMPG_S(igp,icl,yrs,ildv) = (FE(ICL,IGP,current,ILDV)/1.7)
      endif
    endif

    FE(icl,igp,current,ildv) = 1.0/(phev_evmt(igp,icl,dsyrs,ildv)/PHEVMPG_D(igp,icl,yrs,ildv) + &              ! charge depleting mpg
								     (1.0-phev_evmt(igp,icl,dsyrs,ildv))/PHEVMPG_S(igp,icl,yrs,ildv))          ! charge sustaining mpg

    IF (FE(icl,igp,current,ildv).le.0.0.or.FE(icl,igp,current,ildv).ne.FE(icl,igp,current,ildv)) THEN
      WRITE(21,'(a,",",6(i4,","),7(f9.2,","))')'ERROR: PHEV mpg',curitr,curcalyr,igp,icl,ildv,pass,FE(icl,igp,current,ildv),BatPackSize(dsyrs,icl,igp,ildv),evmpg_adj(igp,icl,ildv),phev_dod(dsyrs),&
                                                                                                   phev_evmt(igp,icl,dsyrs,ildv),csratio(icl,igp,:)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF

!...calculate tanksize	
	if(tanksize(icl,igp,prev,ildv).eq.0.0) then
	  if(ildv.eq.5) then
	    if(igp.le.3) tanksize(icl,igp,current,ildv) = FE(icl,igp,current,ildv)/7.774
		if(igp.eq.4) tanksize(icl,igp,current,ildv) = FE(icl,igp,current,ildv)/4.572
		if(igp.eq.5) tanksize(icl,igp,current,ildv) = FE(icl,igp,current,ildv)/3.093
		if(igp.ge.6) tanksize(icl,igp,current,ildv) = FE(icl,igp,current,ildv)/3.381 
	  else 
	    if(igp.le.3) tanksize(icl,igp,current,ildv) = FE(icl,igp,current,ildv)/11.597
		if(igp.eq.4) tanksize(icl,igp,current,ildv) = FE(icl,igp,current,ildv)/13.886
		if(igp.eq.5) tanksize(icl,igp,current,ildv) = FE(icl,igp,current,ildv)/ 3.795
		if(igp.ge.6) tanksize(icl,igp,current,ildv) = FE(icl,igp,current,ildv)/ 5.350
      endif
	endif 
	
!...fill FEMCALC base year values for grpflag year	
	if(grpflag(ildv,icl,igp).eq.curcalyr.and.fempri(igp,icl,epalyr,ildv).eq.0.0) then
!...  fill base and previous values
	  do i=base,prev
	    weight(icl,igp,i,ildv) = weight(icl,igp,current,ildv)
		fe(icl,igp,i,ildv) = fe(icl,igp,current,ildv)
		hp(icl,igp,i,ildv) = hp(icl,igp,current,ildv)
		tanksize(icl,igp,i,ildv) = tanksize(icl,igp,current,ildv)
		price(icl,igp,i,ildv) = price(icl,igp,current,ildv)
	  enddo
	endif
	
  RETURN
  END SUBROUTINE PHEVCALC

! ==========================================================================================================
!...Subroutine EVCALC
!   Description:
!      Calculates electric vehicle (100,200, & 300 mile range) battery size, price, weight, and horsepower
! ==========================================================================================================
  SUBROUTINE EVCALC (dsyrs)
  USE T_
  IMPLICIT NONE

!...local variable definitions
	integer       dsyrs

!...Calculate required battery size based on vehicle weight and depth of discharge improvement
!   Battery size requirement stays constant -- DOD improvements go to increasing range
	if(batpacksize(dsyrs-1,icl,igp,ildv).gt.0.0) then 
      BatPackSize(dsyrs,icl,igp,ildv) = batpacksize(dsyrs-1,icl,igp,ildv) 
	else
      BatPackSize(dsyrs,icl,igp,ildv) = weight(icl,igp,current,gas) * LIONkWh_perLb(ICL,IGP,ILDV)
	endif

    IF (BatPackSize(dsyrs,icl,igp,ildv).le.0.0.or.BatPackSize(dsyrs,icl,igp,ildv).ne.BatPackSize(dsyrs,icl,igp,ildv)) THEN
      WRITE(21,'(a,",",6(i4,","),4(f9.2,","))')'ERROR: BEV pack size',curitr,curcalyr,igp,icl,ildv,pass,BatPackSize(dsyrs,icl,igp,ildv),LIONkWh_perLb(ICL,IGP,ILDV),ev_dod(epalyr),weight(icl,igp,current,gas)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF

!...Calculate total battery electric vehicle incremental cost
    ElecSysIncCost(ICL,IGP,current,ILDV) = BatPackSize(dsyrs,ICL,IGP,ILDV) * Li_ion_Cost(ILDV,dsyrs) + &
										   + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)

    if (ElecSysIncCost(ICL,IGP,prev,ILDV).eq.0.0) ElecSysIncCost(ICL,IGP,prev,ILDV) = ElecSysIncCost(ICL,IGP,current,ILDV)

!...Calculate battery electric vehicle price
    PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)

!...Calculate average battery weight (lbs) per kWh
	BatPackWgt(dsyrs,icl,igp,ildv) = BatPackSize(dsyrs,icl,igp,ildv) * LION_LB_perkWh(ildv)

!...Calculate vehicle weight based on battery size
	if(weight(icl,igp,prev,ildv).eq.0.0) WEIGHT(icl,igp,current,ildv) = WEIGHT(icl,igp,current,gas)-500.0 + BatPackWgt(dsyrs,icl,igp,ildv)

!...Assume EVs have equivalent performance (HP/weight ratio) to conventional gasoline vehicle
	if(hp(icl,igp,prev,ildv).eq.0.0) then
      if(weight(icl,igp,current,gas).ne.0.0) HP(icl,igp,current,ildv) = HP(icl,igp,current,gas) * (weight(icl,igp,current,ildv)/weight(icl,igp,current,gas)) 
	endif

!...Calculate vehicle range based on usable battery size
!   As noted in the pack size calcs above, DOD improvements go to increasing range.
    if(BatPackSize(dsyrs-1,icl,igp,ildv).gt.0.0) then 
      EV_Range(icl,igp,ildv,dsyrs) = EV_range(icl,igp,ildv,dsyrs-1) * (ev_dod(dsyrs)/ev_dod(dsyrs-1))
    else
      EV_Range(icl,igp,ildv,dsyrs) = (EV_range_m(ildv) * BatPackSize(dsyrs,icl,igp,ildv) + EV_range_b(ildv)) * (ev_dod(dsyrs)/ev_dod(epalyr))
    endif

    IF (EV_Range(icl,igp,ildv,dsyrs).le.0.0.or.EV_Range(icl,igp,ildv,dsyrs).ne.EV_Range(icl,igp,ildv,dsyrs)) THEN
      WRITE(21,'(a,",",6(i4,","),4(f9.2,","))')'ERROR: BEV range',curitr,curcalyr,igp,icl,ildv,pass,EV_Range(icl,igp,ildv,dsyrs),RANGE(icl,igp,prev,ildv),EV_range_m(ildv),EV_range_b(ildv)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF
	
!...Calculate EV fuel economy equivalency based on range and battery size.
	FE(icl,igp,current,ildv) = (EV_range(icl,igp,ildv,dsyrs)/(BatPackSize(dsyrs,icl,igp,ildv)*EV_DOD(dsyrs))* &
	                            MG_HHV*1000.0*(1.0/3412.0)) * evmpg_adj(igp,icl,ildv)

    IF (FE(icl,igp,current,ildv).le.0.0.or.FE(icl,igp,current,ildv).ne.FE(icl,igp,current,ildv)) THEN
      WRITE(21,'(a,",",6(i4,","),4(f9.2,","))')'ERROR: BEV mpg',curitr,curcalyr,igp,icl,ildv,pass,FE(icl,igp,current,ildv),BatPackSize(dsyrs,icl,igp,ildv),evmpg_adj(igp,icl,ildv),EV_DOD(dsyrs)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF

!...fill FEMCALC base year values for grpflag year	
	if(grpflag(ildv,icl,igp).eq.curcalyr.and.fempri(igp,icl,epalyr,ildv).eq.0.0) then
!...  fill base and previous values
      do i=base,prev
	    weight(icl,igp,i,ildv) = weight(icl,igp,current,ildv)
	    fe(icl,igp,i,ildv) = fe(icl,igp,current,ildv)
	    hp(icl,igp,i,ildv) = hp(icl,igp,current,ildv)
	    tanksize(icl,igp,i,ildv) = tanksize(icl,igp,current,ildv)
	    price(icl,igp,i,ildv) = price(icl,igp,current,ildv)
	  enddo
	endif

    RETURN
    END SUBROUTINE EVCALC

! ==========================================================================================================
! ... Subroutine FCCALC calculates battery costs and related quantities for fuel cell vehicles
! ==========================================================================================================
  SUBROUTINE FCCALC (dsyrs)
  USE T_
  IMPLICIT NONE

    REAL          BATTERY_WT,BATTERY_POWER
    REAL          TANKCOST(13:14)  /     0.0,  3500.0 /
    REAL          GALPERMILE(13:14)/ 0.00625, 0.00570 /  
	integer       dsyrs
	
!...Calculate base fuel cell cost based on a need of 0.028 kW per vehicle pound and input
!...fuel cell costs in $/kW.
    FUELCELL(ICL,IGP,CURRENT,ILDV) = WEIGHT(ICL,IGP,CURRENT,GAS) * 0.028 * FuelCell_D_kW(dsYRS,ILDV)

!...Calculate battery requirements for FCV 
    if(weight(ICL,IGP,current,gas).gt.0.0) BatPackSize(dsyrs,ICL,IGP,ILDV) = weight(ICL,IGP,current,gas) * LIONkWh_perLb(ICL,IGP,ILDV)

!...Calculate the total fuel cell, battery and hydrogen storage cost
    ElecSysIncCost(ICL,IGP,CURRENT,ILDV) = (FUELCELL(ICL,IGP,CURRENT,ILDV) + &
										BatPackSize(dsyrs,ICL,IGP,ILDV) * Li_ion_Cost(ILDV,dsyrs)  + &
                                        TANKCOST(ILDV))

!...Adjust vehicle price to include price of the fuel cell and battery.
    PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)
	
!...Estimate fuel cell vehicle fuel economy using estimates of gallons per mile per
!...1000 pounds of vehicle weight.
	if(fe(icl,igp,prev,ildv).eq.0.0) FE(ICL,IGP,CURRENT,ILDV) = 1 / (GALPERMILE(ILDV) * (WEIGHT(ICL,IGP,CURRENT,GAS)/1000.0))

  RETURN
  END SUBROUTINE FCCALC

! ==========================================================================================================
! ... Subroutine READHIST reads data for 1990 through the year prior to the FEM base year from the 
! ... historical data file.  These data are required to support output beginning in 1990.
! ==========================================================================================================
    SUBROUTINE READHIST
    USE T_
    IMPLICIT NONE

    LOGICAL*1     NEW/.FALSE./
    CHARACTER*18  INAME
    INTEGER       WKUNIT
    INTEGER       ICYR,IT

!...new detailed model year data
	INTEGER 	NUM_H
	PARAMETER 	(NUM_H = 3167) ! update this value when Base model year data are updated
	INTEGER*2	ICL_H(NUM_H), IGP_H(NUM_H), ILDV_H(NUM_H), YEAR_H(NUM_H)
	REAL 		PRICE_H(NUM_H), HRSPWR_H(NUM_H), TANKSZ_H(NUM_H), CURBWGT_H(NUM_H)
	REAL 		BATTKWH_H(NUM_H), FTPRNT_H(NUM_H), SALES_H(NUM_H), RANGE_H(NUM_H) 
	REAL		MPGTST_H(NUM_H), MPGADJ_H(NUM_H), MPGCOMP_H(NUM_H), PHEV_EVMT_H(NUM_H)
	REAL		EV_RANGE_H(NUM_H) 
	INTEGER*2	nm, gp, cl, ld, cy 
	
!...Read historic FEM data
    INAME = 'TRNFEMX'
    WKUNIT = FILE_MGR('O',INAME,NEW)   ! open trnfemx.xlsx input file
    CALL ReadRngXLSX(WKUNIT,'trnfem')   ! read range names & corresponding data from worksheet "trnfem"
    WKUNIT = FILE_MGR('C',INAME,NEW)   ! close xlsx input file
	
!...read 1990 TO XYR new ldv data 
	CALL GETRNGI('ICL_H           ',ICL_H,1,NUM_H,1)
	CALL GETRNGI('IGP_H           ',IGP_H,1,NUM_H,1)
	CALL GETRNGI('ILDV_H          ',ILDV_H,1,NUM_H,1)
	CALL GETRNGR('PRICE_H         ',PRICE_H,1,NUM_H,1)
	CALL GETRNGR('HRSPWR_H        ',HRSPWR_H,1,NUM_H,1)
	CALL GETRNGR('TANKSZ_H        ',TANKSZ_H,1,NUM_H,1)
	CALL GETRNGR('CURBWGT_H       ',CURBWGT_H,1,NUM_H,1)	
	CALL GETRNGR('BATTKWH_H       ',BATTKWH_H,1,NUM_H,1)
	CALL GETRNGR('FTPRNT_H        ',FTPRNT_H,1,NUM_H,1)
	CALL GETRNGR('SALES_H         ',SALES_H,1,NUM_H,1)
	CALL GETRNGR('RANGE_H         ',RANGE_H,1,NUM_H,1)	
	CALL GETRNGR('MPGTST_H        ',MPGTST_H,1,NUM_H,1)
	CALL GETRNGR('MPGADJ_H        ',MPGADJ_H,1,NUM_H,1)
	CALL GETRNGR('MPGCOMP_H       ',MPGCOMP_H,1,NUM_H,1)
	CALL GETRNGR('PHEV_EVMT_H     ',PHEV_EVMT_H,1,NUM_H,1)
	CALL GETRNGR('EV_RANGE_H      ',EV_RANGE_H,1,NUM_H,1)
	CALL GETRNGI('YEAR_H          ',YEAR_H,1,NUM_H,1)

!...read new detailed vehicle (maxldv) data into FEM variables
!...clear values
	do icyr=1990,lyr
	  do igp=1,maxgroup 
	    do icl=1,maxclass 
		  do ildv=1,maxldv 
		    femmpg(igp,icl,icyr,ildv)   = 0.0
			femtsz(igp,icl,icyr,ildv)   = 0.0
			fempri(igp,icl,icyr,ildv)   = 0.0
			femhp(igp,icl,icyr,ildv)    = 0.0
			femwgt(igp,icl,icyr,ildv)   = 0.0
			femrng(igp,icl,icyr,ildv)   = 0.0
			ev_rng(igp,icl,icyr,ildv)     = 0.0
            fprt(igp,icl,icyr,ildv)       = 0.0
            mpgcomp(igp,icl,icyr,ildv)  = 0.0
			mpgadj(igp,icl,icyr,ildv)   = 0.0
			phev_evmt(igp,icl,icyr,ildv) = 0.0			
			batpacksize(icyr,icl,igp,ildv) = 0.0
            cafesales(igp,icl,icyr,ildv) = 0.0
		  enddo 
		enddo 
	  enddo
	enddo
!...populate new values
	do nm=1,num_h
      gp = igp_h(nm)
	  cl = icl_h(nm)
	  ld = ildv_h(nm)
	  cy = year_h(nm)
	  femmpg(gp,cl,cy,ld) = mpgtst_h(nm)
	  femtsz(gp,cl,cy,ld) = tanksz_h(nm)
	  fempri(gp,cl,cy,ld) = price_h(nm) * MC_JPGDP(1)/MC_JPGDP(CY-1989)
	  femhp(gp,cl,cy,ld)  = hrspwr_h(nm)
	  femwgt(gp,cl,cy,ld) = curbwgt_h(nm)
	  femrng(gp,cl,cy,ld) = range_h(nm)
	  ev_rng(gp,cl,cy,ld) = ev_range_H(nm)
	  fprt(gp,cl,cy,ld)   = ftprnt_h(nm)
	  cafesales(gp,cl,cy,ld) = sales_h(nm)
	  mpgcomp(gp,cl,cy,ld) = mpgcomp_h(nm)
	  mpgadj(gp,cl,cy,ld) = mpgadj_h(nm)
	  BatPackSize(cy,cl,gp,ld) = battkwh_h(nm)
	  phev_evmt(gp,cl,cy,ld) = phev_evmt_h(nm)
	enddo

!...clear fem variables
	do icl=1,maxclass
      do igp=1,maxgroup
		do ildv=1,maxldv
		  do i=base,current
            FE(icl,igp,i,ildv)       = 0.0
            WEIGHT(icl,igp,i,ildv)   = 0.0
            PRICE(icl,igp,i,ildv)    = 0.0
            HP(icl,igp,i,ildv)       = 0.0
            TANKSIZE(icl,igp,i,ildv) = 0.0
		    RANGE(icl,igp,i,ildv)	 = 0.0
		  enddo 
		enddo 
	  enddo 
	enddo 
	
!...set FEM base-current year data (xyr)
	do icl=1,maxclass
      do igp=1,maxgroup
		do ildv=1,maxldv
		  do i=base,current
            FE(icl,igp,i,ildv)       = FEMMPG(igp,icl,xyr,ildv)
            WEIGHT(icl,igp,i,ildv)   = FEMWGT(igp,icl,xyr,ildv)
            PRICE(icl,igp,i,ildv)    = FEMPRI(igp,icl,xyr,ildv)
            HP(icl,igp,i,ildv)       = FEMHP(igp,icl,xyr,ildv)
            TANKSIZE(icl,igp,i,ildv) = FEMTSZ(igp,icl,xyr,ildv)
		    RANGE(icl,igp,i,ildv)	 = FEMRNG(igp,icl,xyr,ildv)
            do itech=1,numtech 
              MKT_PEN(icl,igp,itech,i,ildv) = MKT_PEN(icl,igp,itech,base,gas)
            enddo
            if((ILDV.ge.4.and.ildv.le.8).or.ildv.ge.13) then 
		      if(igp.le.cargrp) then
		        ElecSysIncCost(icl,igp,i,ildv) = (BatPackSize(xyr,icl,igp,ildv)*Li_ion_Cost(ildv,xyr)) + ElecNonBattCst(icl,xyr,1,ildv)
			  else 
			    ElecSysIncCost(icl,igp,i,ildv) = (BatPackSize(xyr,icl,igp,ildv)*Li_ion_Cost(ildv,xyr)) + ElecNonBattCst(icl,xyr,2,ildv)
			  endif 
		    endif
		  enddo
        enddo
      enddo
    enddo

    RETURN
    END SUBROUTINE READHIST

! ==========================================================================================================
! ... Subroutine READNHTSA reads the EPA/NHTSA historic data post xyr. 
! ==========================================================================================================
    SUBROUTINE READNHTSA
    USE T_
    IMPLICIT NONE

    LOGICAL*1     NEW/.FALSE./
    CHARACTER*18  INAME
    INTEGER       WKUNIT, JYR,IT

!...new detailed model year data
	INTEGER 	NUM_E, NUM_19, NUM_20, NUM_21, NUM_22, NUM_23, ICYR
	PARAMETER 	(NUM_E = 158) ! update this value when model year data are updated
	PARAMETER 	(NUM_19 = 4841, NUM_20 = 4380, NUM_21 = 4931, NUM_22 = 5166, NUM_23 = 5244) ! regional sales data
	INTEGER*2	ICL_E(NUM_E), IGP_E(NUM_E), ILDV_E(NUM_E), YEAR_E(NUM_E)
	INTEGER*2 	ICD_19(NUM_19), ICL_19(NUM_19), IGP_19(NUM_19), ILDV_19(NUM_19), IOWN_19(NUM_19)
	INTEGER*2 	ICD_20(NUM_20), ICL_20(NUM_20), IGP_20(NUM_20), ILDV_20(NUM_20), IOWN_20(NUM_20)
	INTEGER*2 	ICD_21(NUM_21), ICL_21(NUM_21), IGP_21(NUM_21), ILDV_21(NUM_21), IOWN_21(NUM_21)
	INTEGER*2 	ICD_22(NUM_22), ICL_22(NUM_22), IGP_22(NUM_22), ILDV_22(NUM_22), IOWN_22(NUM_22)
	INTEGER*2 	ICD_23(NUM_23), ICL_23(NUM_23), IGP_23(NUM_23), ILDV_23(NUM_23), IOWN_23(NUM_23)
	REAL 		PRICE_E(NUM_E), HRSPWR_E(NUM_E), TANKSZ_E(NUM_E), TRNKSZ_E(NUM_E), CURBWGT_E(NUM_E)
	REAL 		BATTKWH_E(NUM_E), FTPRNT_E(NUM_E), SALES_E(NUM_E), RANGE_E(NUM_E) 
	REAL		MPGTST_E(NUM_E), MPGADJ_E(NUM_E), MPGCOMP_E(NUM_E), PHEV_EVMT_E(NUM_E)
	REAL		EV_RANGE_E(NUM_E), PHEVMPG_D_E(NUM_E), PHEVMPG_S_E(NUM_E), NAMEPLATE_E(NUM_E)
	REAL 		SALES_19(NUM_19), SALES_20(NUM_20), SALES_21(NUM_21), SALES_22(NUM_22), SALES_23(NUM_23)
	REAL		DEN1, DEN2, own_sales_ttl(maxgroup,maxclass,maxldv,mnumcr-2), tempmpg(maxgroup,maxclass,maxldv)
	REAL		EVMPGADJ(maxclass,2,maxvtyp), luggage(maxvtyp,maxclass), num1, num2
	INTEGER*2	nm, gp, cl, ld, cy, ow, cd 

!...Read historical epa data for FEM
    INAME = 'TRNNHTSAX'                 ! trnnhtsaX.xlsx
    WKUNIT = FILE_MGR('O',INAME,NEW)    !open trnnhtsaX.xlsx input file
    CALL ReadRngXLSX(WKUNIT,'trnnhtsa') !read range names & corresponding data from worksheet "trnnhtsa"
    WKUNIT = FILE_MGR('C',INAME,NEW)    !close xlsx input file

	CALL GETRNGI('EPAYR           ',EPAYR,1,1,1)
	CALL GETRNGI('EPALYR          ',EPALYR,1,1,1)
	CALL GETRNGI('ICL_E           ',ICL_E,1,NUM_E,1)
	CALL GETRNGI('IGP_E           ',IGP_E,1,NUM_E,1)
	CALL GETRNGI('ILDV_E          ',ILDV_E,1,NUM_E,1)
	CALL GETRNGR('PRICE_E         ',PRICE_E,1,NUM_E,1)
	CALL GETRNGR('HRSPWR_E        ',HRSPWR_E,1,NUM_E,1)
	CALL GETRNGR('TANKSZ_E        ',TANKSZ_E,1,NUM_E,1)
	CALL GETRNGR('TRNKSZ_E        ',TRNKSZ_E,1,NUM_E,1)	
	CALL GETRNGR('CURBWGT_E       ',CURBWGT_E,1,NUM_E,1)	
	CALL GETRNGR('BATTKWH_E       ',BATTKWH_E,1,NUM_E,1)
	CALL GETRNGR('FTPRNT_E        ',FTPRNT_E,1,NUM_E,1)
	CALL GETRNGR('SALES_E         ',SALES_E,1,NUM_E,1)
	CALL GETRNGR('RANGE_E         ',RANGE_E,1,NUM_E,1)	
	CALL GETRNGR('MPGTST_E        ',MPGTST_E,1,NUM_E,1)
	CALL GETRNGR('MPGADJ_E        ',MPGADJ_E,1,NUM_E,1)
	CALL GETRNGR('MPGCOMP_E       ',MPGCOMP_E,1,NUM_E,1)
	CALL GETRNGR('PHEV_EVMT_E     ',PHEV_EVMT_E,1,NUM_E,1)
	CALL GETRNGR('PHEVMPG_S_E     ',PHEVMPG_S_E,1,NUM_E,1)
	CALL GETRNGR('PHEVMPG_D_E     ',PHEVMPG_D_E,1,NUM_E,1)
	CALL GETRNGR('NAMEPLATE_E     ',NAMEPLATE_E,1,NUM_E,1)	
	CALL GETRNGR('EV_RANGE_E      ',EV_RANGE_E,1,NUM_E,1)
	CALL GETRNGI('YEAR_E          ',YEAR_E,1,NUM_E,1)
	CALL GETRNGR('EVMPGADJ        ',EVMPGADJ(1:MAXCLASS,1:2,1:MAXVTYP),MAXCLASS,2,MAXVTYP)

!...Read historical polk/epa data on sales by region
!...2019
	CALL GETRNGI('ICD_19          ',ICD_19,1,NUM_19,1)
	CALL GETRNGI('ICL_19          ',ICL_19,1,NUM_19,1)
	CALL GETRNGI('IGP_19          ',IGP_19,1,NUM_19,1)
	CALL GETRNGI('ILDV_19         ',ILDV_19,1,NUM_19,1)
	CALL GETRNGI('IOWN_19         ',IOWN_19,1,NUM_19,1)
	CALL GETRNGR('SALES_19        ',SALES_19,1,NUM_19,1)
!...2020
	CALL GETRNGI('ICD_20          ',ICD_20,1,NUM_20,1)
	CALL GETRNGI('ICL_20          ',ICL_20,1,NUM_20,1)
	CALL GETRNGI('IGP_20          ',IGP_20,1,NUM_20,1)
	CALL GETRNGI('ILDV_20         ',ILDV_20,1,NUM_20,1)
	CALL GETRNGI('IOWN_20         ',IOWN_20,1,NUM_20,1)
	CALL GETRNGR('SALES_20        ',SALES_20,1,NUM_20,1)
!...2021
	CALL GETRNGI('ICD_21          ',ICD_21,1,NUM_21,1)
	CALL GETRNGI('ICL_21          ',ICL_21,1,NUM_21,1)
	CALL GETRNGI('IGP_21          ',IGP_21,1,NUM_21,1)
	CALL GETRNGI('ILDV_21         ',ILDV_21,1,NUM_21,1)
	CALL GETRNGI('IOWN_21         ',IOWN_21,1,NUM_21,1)
	CALL GETRNGR('SALES_21        ',SALES_21,1,NUM_21,1)
!...2022
	CALL GETRNGI('ICD_22          ',ICD_22,1,NUM_22,1)
	CALL GETRNGI('ICL_22          ',ICL_22,1,NUM_22,1)
	CALL GETRNGI('IGP_22          ',IGP_22,1,NUM_22,1)
	CALL GETRNGI('ILDV_22         ',ILDV_22,1,NUM_22,1)
	CALL GETRNGI('IOWN_22         ',IOWN_22,1,NUM_22,1)
	CALL GETRNGR('SALES_22        ',SALES_22,1,NUM_22,1)
!...2023
	CALL GETRNGI('ICD_23          ',ICD_23,1,NUM_23,1)
	CALL GETRNGI('ICL_23          ',ICL_23,1,NUM_23,1)
	CALL GETRNGI('IGP_23          ',IGP_23,1,NUM_23,1)
	CALL GETRNGI('ILDV_23         ',ILDV_23,1,NUM_23,1)
	CALL GETRNGI('IOWN_23         ',IOWN_23,1,NUM_23,1)
	CALL GETRNGR('SALES_23        ',SALES_23,1,NUM_23,1)

	if(epalyr.gt.xyr) then
!...  clear data arrays
	  do igp=1,maxgroup 
	    do icl=1,maxclass 
	      do ildv=1,maxldv
		    cy = epalyr ! change to do loop if multiple years are read in
		    epampg(igp,icl,cy:lyr,ildv) = 0.0
		    epatsz(igp,icl,cy:lyr,ildv) = 0.0
			epalug(igp,icl,cy:lyr,ildv) = 0.0
		    epapri(igp,icl,cy:lyr,ildv) = 0.0
		    epahp(igp,icl,cy:lyr,ildv)  = 0.0
		    epawgt(igp,icl,cy:lyr,ildv) = 0.0
		    eparng(igp,icl,cy:lyr,ildv) = 0.0
		    ev_rng(igp,icl,cy:lyr,ildv) = 0.0
		    fprt(igp,icl,cy:lyr,ildv)   = 0.0		
		    cafesales(igp,icl,cy:lyr,ildv) = 0.0
		    mpgcomp(igp,icl,cy:lyr,ildv) = 0.0
		    mpgadj(igp,icl,cy:lyr,ildv) = 0.0
		    BatPackSize(cy:lyr,icl,igp,ildv) = 0.0
		    phev_evmt(igp,icl,cy:lyr,ildv) = 0.0
			phevmpg_s(igp,icl,cy:lyr,ildv) = 0.0
			phevmpg_d(igp,icl,cy:lyr,ildv) = 0.0
			nameplate(igp,icl,cy:lyr,ildv) = 0.0
		  enddo 
		enddo 
	  enddo	
!...  populate LDV data arrays	  
      do nm=1,num_e
		gp = igp_e(nm)
		cl = icl_e(nm)
		ld = ildv_e(nm)
		cy = year_e(nm)
		epampg(gp,cl,cy,ld) = mpgtst_e(nm)
		epatsz(gp,cl,cy,ld) = tanksz_e(nm)
		epalug(gp,cl,cy,ld) = trnksz_e(nm)
		epapri(gp,cl,cy,ld) = price_e(nm) * MC_JPGDP(1)/MC_JPGDP(CY-1989)
		epahp(gp,cl,cy,ld)  = hrspwr_e(nm)
		epawgt(gp,cl,cy,ld) = curbwgt_e(nm)
		eparng(gp,cl,cy,ld) = range_e(nm)
		ev_rng(gp,cl,cy,ld) = ev_range_e(nm)
		fprt(gp,cl,cy,ld)   = ftprnt_e(nm)		
		cafesales(gp,cl,cy,ld) = sales_e(nm)
		mpgcomp(gp,cl,cy,ld) = mpgcomp_e(nm)
		mpgadj(gp,cl,cy,ld) = mpgadj_e(nm)
		BatPackSize(cy,cl,gp,ld) = battkwh_e(nm)
		phev_evmt(gp,cl,cy,ld) = phev_evmt_e(nm)
		phevmpg_s(gp,cl,cy,ld) = phevmpg_s_e(nm)
		phevmpg_d(gp,cl,cy,ld) = phevmpg_d_e(nm)
		nameplate(gp,cl,cy,ld) = nameplate_e(nm)		
	  enddo
	endif

!...EV average mpg adjustment factors for PHEV and EV 
	do igp=1,maxgroup
	  ivtyp=grpmap(igp)
	  do icl=1,maxclass 
	    do ildv=1,maxldv 
		  if(ildv.eq.5.or.ildv.eq.6) then ! phevs
			evmpg_adj(igp,icl,ildv) = evmpgadj(icl,1,ivtyp) 
		  elseif(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then ! evs
			evmpg_adj(igp,icl,ildv) = evmpgadj(icl,2,ivtyp)	
		  endif 
		enddo 
	  enddo 
	enddo 
!...populate actual EV mpg adjustments where data exists	
    do igp=1,maxgroup 
	  do icl=1,maxclass 
	    do ildv=1,maxldv
		  tempmpg(igp,icl,ildv) = 0.0
		  if(batpacksize(epalyr,icl,igp,ildv).ne.0.0) then
		    if(ildv.eq.5.or.ildv.eq.6) then ! phevs
		      tempmpg(igp,icl,ildv) = ev_rng(igp,icl,epalyr,ildv)/BatPackSize(epalyr,icl,igp,ildv)*MG_HHV*1000.0*(1.0/3412.0)  
			elseif(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then ! evs
		      tempmpg(igp,icl,ildv) = ev_rng(igp,icl,epalyr,ildv)/BatPackSize(epalyr,icl,igp,ildv)*MG_HHV*1000.0*(1.0/3412.0) 
			endif
		  endif
		  if(tempmpg(igp,icl,ildv).ne.0.0) then 
		    if(ildv.eq.5.or.ildv.eq.6) evmpg_adj(igp,icl,ildv) = phevmpg_d(igp,icl,epalyr,ildv)/tempmpg(igp,icl,ildv)
			if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) evmpg_adj(igp,icl,ildv) = epampg(igp,icl,epalyr,ildv)/tempmpg(igp,icl,ildv)
		  endif
		enddo 
	  enddo 
	enddo
			  			  
!...calculate average luggage space by size class for ildv introductions post epalyr
!...calculate group averages
	do icl=1,maxclass 
	  do igp=1,maxgroup	
	  	num1 = 0.0
	    den1 = 0.0
		luggavg(igp,icl) = 0.0
		do ildv=1,maxldv
		  if(epampg(igp,icl,epalyr,ildv).gt.0.0.and.epalug(igp,icl,epalyr,gas).gt.0.0) then
			num1 = num1 + cafesales(igp,icl,epalyr,ildv) * (epalug(igp,icl,epalyr,ildv)/epalug(igp,icl,epalyr,gas))
			den1 = den1 + cafesales(igp,icl,epalyr,ildv) 
		  endif
		enddo
		if(den1.ne.0.0) luggavg(igp,icl) = num1/den1
	  enddo
	enddo
!...calculate industry average by type and class
	do icl=1,maxclass 
	  num1 = 0.0
	  den1 = 0.0
	  num2 = 0.0
	  den2 = 0.0		
	  do igp=1,maxgroup	
	  	do ildv=1,maxldv
		  if(epampg(igp,icl,epalyr,ildv).gt.0.0.and.epampg(igp,icl,epalyr,gas).gt.0.0) then
		    if(igp.le.cargrp) then
			  num1 = num1 + cafesales(igp,icl,epalyr,ildv) * epalug(igp,icl,epalyr,ildv)/epalug(igp,icl,epalyr,gas)
			  den1 = den1 + cafesales(igp,icl,epalyr,ildv) 
			else 
			  num1 = num1 + cafesales(igp,icl,epalyr,ildv) * epalug(igp,icl,epalyr,ildv)/epalug(igp,icl,epalyr,gas)
			  den1 = den1 + cafesales(igp,icl,epalyr,ildv)	
			endif
		  endif
		enddo
	  enddo
	  if(den1.ne.0.0) luggage(1,icl) = num1/den1
	  if(den2.ne.0.0) luggage(2,icl) = num1/den1
!...  assign industry average if group/class luggage = 0.0
	  do igp=1,maxgroup 
		if(luggavg(igp,icl).eq.0.0) then 
		  if(igp.le.cargrp) then 
		    luggavg(igp,icl) = luggage(1,icl) 
		  else 
		    luggavg(igp,icl) = luggage(2,icl)
		  endif 
		endif 
	  enddo
	enddo

    do nm=1,num_19
	  ow = iown_19(nm)
	  gp = igp_19(nm)
	  cl = icl_19(nm)
	  ld = ildv_19(nm)
	  cd = icd_19(nm)
	  cy = 2019
	  own_sales(ow,gp,cl,ld,cd,cy) = sales_19(nm)
	enddo
!...2020
    do nm=1,num_20
	  ow = iown_20(nm)
	  gp = igp_20(nm)
	  cl = icl_20(nm)
	  ld = ildv_20(nm)
	  cd = icd_20(nm)
	  cy = 2020
	  own_sales(ow,gp,cl,ld,cd,cy) = sales_20(nm)
	enddo
!...2021
    do nm=1,num_21
	  ow = iown_21(nm)
	  gp = igp_21(nm)
	  cl = icl_21(nm)
	  ld = ildv_21(nm)
	  cd = icd_21(nm)
	  cy = 2021
	  own_sales(ow,gp,cl,ld,cd,cy) = sales_21(nm)
	enddo
!...2022
    do nm=1,num_22
	  ow = iown_22(nm)
	  gp = igp_22(nm)
	  cl = icl_22(nm)
	  ld = ildv_22(nm)
	  cd = icd_22(nm)
	  cy = 2022
	  own_sales(ow,gp,cl,ld,cd,cy) = sales_22(nm)
	enddo
!...2023
    do nm=1,num_23
	  ow = iown_23(nm)
	  gp = igp_23(nm)
	  cl = icl_23(nm)
	  ld = ildv_23(nm)
	  cd = icd_23(nm)
	  cy = 2023
	  own_sales(ow,gp,cl,ld,cd,cy) = sales_23(nm)
	enddo

	do icyr=2019,stockyr
!...  sum sales by owner type
	  do igp=1,maxgroup 
		do icl=1,maxclass 
		  do ildv=1,maxldv 
			do iregn=1,mnumcr-2 
			  ldv_sales(igp,icl,ildv,iregn,icyr-1989) = sum(own_sales(1:maxowner,igp,icl,ildv,iregn,icyr))
		    enddo 
			ldv_sales(igp,icl,ildv,mnumcr,icyr-1989) = sum(own_sales(1:maxowner,igp,icl,ildv,1:mnumcr-2,icyr))
		  enddo 
		enddo 
	  enddo
   enddo !2019-stockyr

!...calculate share of sales by fleet, group, class, ildv, and region 
!...first calculate sales total 
    do igp=1,maxgroup 
	  do icl=1,maxclass 
	    do ildv=1,maxldv 
		  do iregn=1,mnumcr-2 
		    own_sales_ttl(igp,icl,ildv,iregn) = sum(own_sales(1:maxowner,igp,icl,ildv,iregn,stockyr)) 
		  enddo 
		enddo 
	  enddo 
	enddo  

!...calculate owner sales shares at stockyr
	do iown=1,maxowner 
	  do igp=1,maxgroup 
	    do icl=1,maxclass 
		  do ildv=1,maxldv
	        do iregn=1,mnumcr-2
			  ownsalesshr(iown,igp,icl,ildv,iregn,stockyr-1989) = 0.0
			  if(own_sales_ttl(igp,icl,ildv,iregn).ne.0.0) then
			    ownsalesshr(iown,igp,icl,ildv,iregn,stockyr-1989) = own_sales(iown,igp,icl,ildv,iregn,stockyr)/own_sales_ttl(igp,icl,ildv,iregn)
			  endif
			enddo
		  enddo 
		enddo 
	  enddo 
	enddo 

! jma temp ownershare to get model working
	do iown=1,maxowner 
	  do igp=1,maxgroup 
	    do icl=1,maxclass 
	      do iregn=1,mnumcr-2
			ownsaletemp(iown,igp,icl,iregn,stockyr-1989) = 0.0
			if(sum(own_sales_ttl(igp,icl,1:maxldv,iregn)).ne.0.0) then
			  ownsaletemp(iown,igp,icl,iregn,stockyr-1989) = sum(own_sales(iown,igp,icl,1:maxldv,iregn,stockyr))/sum(own_sales_ttl(igp,icl,1:maxldv,iregn))
			endif
		  enddo 
		enddo 
	  enddo 
	enddo 
    
!   Fill EV_range variable used in EVCALC and PHEVCALC
	do igp=1,maxgroup 
	  do icl=1,maxclass
        do ildv = 1, maxldv
          if (ildv.eq.4.or.ildv.eq.5.or.ildv.eq.6.or.ildv.eq.7.or.ildv.eq.15) then
            EV_range(icl,igp,ildv,1990:epalyr) = ev_rng(igp,icl,1990:epalyr,ildv)
          endif
        enddo
      enddo
    enddo

    RETURN
    END SUBROUTINE READNHTSA

! ==========================================================================================================
! ... Subroutine CALIBNHTSA calibrates FEM results to NHTSA values.  
! ==========================================================================================================
    SUBROUTINE CALIBNHTSA
    USE T_
    IMPLICIT NONE

    REAL	CALRATIO_FE(MAXGROUP,MAXCLASS,MAXLDV)		! calibration factor for FE
    REAL	CALRATIO_HP(MAXGROUP,MAXCLASS,MAXLDV)		! calibration factor for HP
    REAL	CALRATIO_WGT(MAXGROUP,MAXCLASS,MAXLDV)		! calibration factor for WGHT
	REAL	CALRATIO_PRI(MAXGROUP,MAXCLASS,MAXLDV)		! calibration factor for price 	
	REAL	CALRATIO_TSZ(MAXGROUP,MAXCLASS,MAXLDV)		! calicration factor for tank size

!...Calibration factors will be based on historical EPA/NHTSA data after xyr and through the last available 
!...data year. Calibrated data for each year serve as input into FEM for the next year, in effect carrying 
!...the last year of calibration data through the projection.
    
    do igp=1,maxgroup
      do icl=1,maxclass
        do ildv=1,maxldv
		  calratio_fe(igp,icl,ildv) = 1.0
		  calratio_hp(igp,icl,ildv) = 1.0
		  calratio_wgt(igp,icl,ildv) = 1.0
		  calratio_pri(igp,icl,ildv) = 1.0	
		  calratio_tsz(igp,icl,ildv) = 1.0
		  if(femmpg(igp,icl,yrs,ildv).ne.0.0) then
			if(curcalyr.le.epalyr) then
			  calratio_fe(igp,icl,ildv) = epampg(igp,icl,yrs,ildv)/femmpg(igp,icl,yrs,ildv)
			  calratio_hp(igp,icl,ildv) = epahp(igp,icl,yrs,ildv)/femhp(igp,icl,yrs,ildv)
			  calratio_wgt(igp,icl,ildv) = epawgt(igp,icl,yrs,ildv)/femwgt(igp,icl,yrs,ildv)
			  calratio_pri(igp,icl,ildv) = epapri(igp,icl,yrs,ildv)/fempri(igp,icl,yrs,ildv)
			  if(femtsz(igp,icl,yrs,ildv).ne.0.0) &
			    calratio_tsz(igp,icl,ildv) = epatsz(igp,icl,yrs,ildv)/femtsz(igp,icl,yrs,ildv)
			endif
		    femmpg(igp,icl,yrs,ildv) = femmpg(igp,icl,yrs,ildv) * calratio_fe(igp,icl,ildv)
            femhp(igp,icl,yrs,ildv)  = femhp(igp,icl,yrs,ildv)  * calratio_hp(igp,icl,ildv) 
            femwgt(igp,icl,yrs,ildv) = femwgt(igp,icl,yrs,ildv) * calratio_wgt(igp,icl,ildv)
			fempri(igp,icl,yrs,ildv) = fempri(igp,icl,yrs,ildv) * calratio_pri(igp,icl,ildv)
			femtsz(igp,icl,yrs,ildv) = femtsz(igp,icl,yrs,ildv) * calratio_tsz(igp,icl,ildv)
		  endif
		  if(curcalyr.eq.epalyr) then 
		  	if(cafesales(igp,icl,yrs,ildv).ne.0.0.and.femmpg(igp,icl,yrs,ildv).eq.0.0) then
              write(21,*) ' fix group flag for the following:'
			  write(21,*) '   YRS   = ',yrs
              write(21,*) '   IGP   = ',igp
              write(21,*) '   ICL   = ',icl
              write(21,*) '   ILDV  = ',ildv
			  write(21,*) '  femmpg = ',femmpg(igp,icl,yrs,ildv) 
			  write(21,*) '  epampg = ',epampg(igp,icl,yrs,ildv)
            endif
		  endif
        enddo
      enddo
    enddo

    RETURN
    END SUBROUTINE CALIBNHTSA
