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

INTEGER      LEGIRA,   &											! "1" turns on 2022 IRA tax credits for PHEV and eV
             TRANEFF                                                ! "3" turns on Alternative Transportation case
INTEGER      IBank                                                  ! "1" turns on CAFE banking
INTEGER      TRANAB32                                               ! Switch for California AB32 off/on
																	!     sales of hybrid and diesel vehicles
REAL         IRA_STIM   											! "1" switch for IRA PHEV/EV tax credit

! ... Global definitions
integer      iy                                                     ! year index corresponding to first year of inputs
integer      num_to_read                                            ! number of years in input ranges
integer      First_Read_Year                                        ! = 1995
INTEGER      YRS                                                    ! actual model year (1989+curiyr)
INTEGER      N                                                      ! tran variable for curiyr
INTEGER      LASTID                                                 ! parameter for index describing number of technologies
INTEGER      MAXGROUP,MAXVTYP,MAXCLASS,MAXTECH,MAXLDV               ! various
INTEGER      MAXNOTE,MAXAGE,MAXFLEET,MAXNMLM                        ! parameters,
INTEGER      MAXFUEL,GAS,FCLO,FCHI,BASE,PREV,CURRENT                ! see
INTEGER      BYR,LYR, XYR,IRAYR,AGEGRP,MF   		                ! definitions below
INTEGER      CARGRP,LTKGRP,MAXHAV,numstkyrs,STOCKYR,VMTYR,MAXCHRG
INTEGER		 PHEVTYPE								

PARAMETER    (MAXGROUP    = 11,          &                          ! number of light duty vehicle groups  (5-car and 6-light truck)
              MAXCHRG     = 3,           &                          ! number of charger types (dc, l1, l2)
              CARGRP      = 5,           &                          ! number of light duty vehicle groups that are car mfgs
			  PHEVTYPE	  = 2,			 &							! 1 = PHEV20, 2 = PHEV50
              LTKGRP      = 6,           &                          ! number of light duty vehicle groups that are light truck mfgs
              MAXVTYP     = 2,           &                          ! number of light duty vehicle types (car/truck)
              MAXCLASS    = 8,           &                          ! number of vehicle classes in each light duty group
              MAXTECH     = 71,          &                          ! number of light duty vehicle technologies (resize based on TECHID arrays in trnldvx.xlsx)
              MAXLDV      = 16,          &                          ! number of light duty vehicle fueling configurations
              MAXNOTE     = 53,          &                          ! number of light duty vehicle engineering notes (resize based on ENGNOTETYPE arrays in trnldvx.xlsx)
              MAXAGE      = 25,          &                          ! number of light duty vehicle vintages
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
			  STOCKYR     = 2024,        &                          ! last year of census division level stock history years
			  NUMSTKYRS   = stockyr-1994,&							! number of census division level stock history years
			  VMTYR       = 2024)                                   ! last year of historical VMT schedule read-in (PVMT and LVMT)

! ... Indices
integer      GrpMap(MAXGROUP)                                       ! Map for how groups are defined back to vehicle types: cars and light trucks
INTEGER      IGP,     &                                             ! vehicle group (MAXGROUP)
			 INMLM,   &												! consumer choice model (maxnmlm)
             IVTYP,   &                                             ! vehicle type (MAXTYPE)
			 IOWN,    &                                             ! owner type (MAXOWNER)
             ICL,     &                                             ! vehicle size class (MAXCLASS) 
             ITECH,   &                                             ! technology type (MAXTECH)
             NUMTECH, &                                             ! actual number of input technologies                                   
             ILDV                                                   ! fuel engine technology (MAXLDV)
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
             SIGN_TDM,  &                                           ! positive or negative indicator
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
                                                                    ! 2 = L4a - low speed HAV (SAE level 4)
																	! 3 = L4b - high speed HAV (SAE level 4)
																	! 4 = L5 - fully automated vehicle (SAE level 5)
                                   
REAL         ROUNDOFF_ERROR                                         ! roundoff error buffer

CHARACTER*15 FTYPELABEL(MAXLDV) 

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
REAL         INC00_D_16(MNUMCR,MNUMYR)                                ! disposable income per capita 16+ (2000$) - travel module and car/LT split
REAL         INC90_D_NP(MNUMCR,BYR:LYR)                               ! disposable income per capita (1990 $) - fuel economy module
REAL         INC_GR_REGN(MNUMCR,BYR:LYR)                            ! Annual growth in disposable income; used in FEMCALC

! ... Total new light duty vehicle sales - subroutine NEWLDV
REAL		 NEWLDVs(maxvtyp,mnumcr,mnumyr)							! New ldv sales
REAL		 CarTrkSplit(mnumcr,maxvtyp,mnumyr)						! Car/LT split by region
REAL         NEWLDVPERLD(MNUMCR-2,MNUMYR)
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

! ... New light duty vehicle fuel economy - subroutine TMPGNEW
! ...... historic values LDV attribute values beyond xyr for model calibration - subroutine READNHTSA
INTEGER  EPALYR														! last year of EPA/NHTSA data 
REAL(4), allocatable :: EPAMPG(:,:,:,:)                             ! EPAMPG(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV) new ldv tested fuel economy
REAL(4), allocatable :: EPAHP(:,:,:,:) 								! EPAHP(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV) new ldv horsepower 
REAL(4), allocatable :: EPAPRI(:,:,:,:) 							! EPAPRI(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV) new ldv low volume price
REAL(4), allocatable :: EPAWGT(:,:,:,:) 							! EPAWGT(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV) new ldv curb weight 
REAL(4), allocatable :: EPARNG(:,:,:,:) 							! EPARNG(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV) new ldv driving range 
REAL(4), allocatable :: EPATSZ(:,:,:,:) 							! EPATSZ(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV) new ldv tank size    
REAL(4), allocatable :: EPALUG(:,:,:,:) 							! EPALUG(MAXGROUP,MAXCLASS,XYR:LYR,MAXLDV) new ldv trunk size/storage space   
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

INTEGER pass3,npass2,npass3                                         ! Counters for FEMCALC passes

!...attribute adjustment values for AFVs
REAL         AFVADJHP(MAXLDV,MAXVTYP)                				! ATV horsepower differential (ratio)
REAL         AFVADJFE(MAXLDV,MAXVTYP)                				! ATV fuel economy differential (ratio)
REAL         AFVADJWT(MAXLDV,MAXVTYP)               				! ATV weight differential (ratio)
REAL         AFVADJPR(MAXLDV,MAXVTYP)                    			! ATV price differential (1990$)
REAL		 EVMPG_ADJ(MAXGROUP,MAXCLASS,MAXLDV)					! ev mpg adjustment factors
! ...... input values for base year vehicles
REAL         FE(MAXGROUP,MAXCLASS,BASE:CURRENT,MAXLDV)              ! vehicle class base fuel economy
REAL         WEIGHT(MAXGROUP,MAXCLASS,BASE:CURRENT,MAXLDV)          ! vehicle class base curb weight
REAL         PRICE(MAXGROUP,MAXCLASS,BASE:CURRENT,MAXLDV)           ! vehicle class base price (low volume)
REAL         HP(MAXGROUP,MAXCLASS,BASE:CURRENT,MAXLDV)              ! vehicle class base horsepower
real         vhp_adj(MAXCLASS,MAXGROUP,PREV:CURRENT,MAXLDV)         ! weight based hp adjustment
REAL         VALUEPERF(MAXCLASS,MAXGROUP)                           ! vehicle class base performance value
REAL         PERFFACT(MAXCLASS,MAXGROUP)                            ! vehicle class base performance factor
REAL         TANKSIZE(MAXGROUP,MAXCLASS,BASE:CURRENT,MAXLDV)        ! vehicle class base fuel tank size
LOGICAL*1    CLASSFLAG(MAXGROUP,MAXCLASS,MAXLDV)		            ! AFV vehicle class applicability flag  
INTEGER*2    GRPFLAG(MAXLDV,MAXCLASS,MAXGROUP)                      ! ATV introduction year by manufacture group by size class
REAL         SALES_PER_MODEL(MAXCLASS,MAXGROUP)                     ! Sales per nameplate, used to introduce new nameplates in projection
! ...... adjustment factors for attributes, applied to those produced in FEMCALC/EVCALC/HEVCALC/PHEVCALC/FCCALC
REAL	     CALRATIO_FE(MAXGROUP,MAXCLASS,MAXLDV)		            ! calibration factor for FE
REAL	     CALRATIO_HP(MAXGROUP,MAXCLASS,MAXLDV)		            ! calibration factor for HP
REAL	     CALRATIO_WGT(MAXGROUP,MAXCLASS,MAXLDV)		            ! calibration factor for WGHT
REAL	     CALRATIO_PRI(MAXGROUP,MAXCLASS,MAXLDV)		            ! calibration factor for price 	
REAL	     CALRATIO_TSZ(MAXGROUP,MAXCLASS,MAXLDV)		            ! calicration factor for tank size

! ...... variable used in subroutine FEMCALC
INTEGER      PAYBACK                                                ! payback period
REAL         DISCOUNT                                               ! discount rate
REAL         PMGTR90_D_(MNUMCR,BYR:LYR)                             ! national gasoline price in 1990 dollars
REAL         PMGTR90_INVGR(MNUMCR,BYR:LYR)                          ! Inverse of gasoline price growth; used in FEMCALC
REAL         PRICE_EX(12)                                           ! expected fuel price used in cost effectiveness calculation
REAL         PSLOPE                                                 ! expected rate of change in future fuel price
LOGICAL*1    CAFEPASS(MAXGROUP)                                     ! indicates manufacturer has passed CAFE standard
LOGICAL*1    first_time_cafetest
REAL         PERFCAP(MAXCLASS,MAXGROUP)                             ! vehicle class performance cap
REAL         USEDCAP(MAXCLASS,MAXGROUP)                             ! fraction of vehicle class performance cap used
REAL         MKT_PEN(MAXGROUP,MAXCLASS,MAXTECH,BASE:CURRENT,MAXLDV) ! vehicle systems technology market share
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
REAL         VMT(mnumcr,12)                                         ! annual vmt by vintage for each region
real         GBInc                                                  ! the increment of the HP give back that is to be used
REAL         FUELSAVE(MAXTECH)                                      ! expected fuel savings of advanced subsystem technology
REAL         TECHCOST(MAXTECH)                                      ! first cost of subsystem technology - cost adjustments 
INTEGER*2    FEM_PASS,pass2
INTEGER*2    RETURN_STAT                                            ! technology supersedes check
REAL         REGCOST(MAXGROUP)                                      ! CAFE Fine
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

! ... STEO Benchmarking
REAL         MER_tran(4)                                            ! Last two historic MER years for gasoline, jet fuel, distillate, and residual by region
INTEGER      ymer                                                   ! Last MER year
INTEGER      ysteo                                                  ! Last STEO year
REAL         TMGTCBUS(MNUMYR)
REAL         TJFTCBUS(MNUMYR)
REAL         TDSTCPUS(MNUMYR)
REAL         TRFTCBUS(MNUMYR)
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
REAL         creds_avail                                            ! EPA GHG credits available for whole market aggregate compliance in EPALYR+2
REAL         creds_avail_grp(maxgroup)
REAL         AVSales_Old(MAXGROUP,MAXCLASS,MaxLdv,mnumyr)           ! initial vehicle sales
REAL         MAXADJ_CAFETEST                                         ! Maximum number of sales that can be converted to AFV in a single CAFEGHG_MEET cycle

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
REAL         CREDBAL_EPALYR
REAL         LDV_VEHLIFEMI(MAXVTYP)                                 ! Lifetime miles assumed for EPA GHG compliance calculation
INTEGER		 ENFORCE_EPA										    ! Switch to use EPA GHG instead of NHTSA CAFE		
INTEGER		 ENFORCE_MY27REGS										! Switch to run AEO2025 No CAFE side case (CAFE/GHG freeze at MY2026 level)	
INTEGER		 ENFORCE_CAFE										    ! Switch to freeze LDV NHTSA CAFE standards at MY2024 (i.e. don't enforce CAFE)
REAL		 AC_CO2_OFFSET(MAXGROUP,MNUMYR)						    ! A/C leakage and alt refrigerant credits; subtract off EPA GHG reg before converting to mpg						

REAL         FPghg(MAXCLASS,MAXGROUP,MNUMYR)                        ! EPA GHG (g/mi) by size class
REAL         FPghgGrp(MAXGROUP,MNUMYR)                             ! EPA GHG (g/mi) by manufacturer
REAL         FPMpg(MAXCLASS,MAXGROUP,MNUMYR)                        ! CAFE by size class
REAL         FPMpgGrp(MAXGROUP,MNUMYR)                              ! CAFE by manufacturer
REAL         Cafe_Used(MAXGROUP,byr:lyr)                            ! CAFE standard used for light trucks
REAL		 AC_OC_CREDIT(MAXGROUP,MNUMYR)                          ! AC and off cycle CAFE credits 
REAL         PU_CRED_ELIG(MAXGROUP)                                 ! Share of group standard pickup sales that are eligible for the 20g/mile pickup incentive in 2024
REAL         ac_oc_credit_expanded(MAXGROUP, MAXCLASS, MAXLDV,MNUMYR)      ! Re-dimensioned version to enable vectorization in harmonic mean functions
REAL         ac_oc_credit_expanded_adj(MAXGROUP, MAXCLASS, MAXLDV,MNUMYR)  ! Re-dimensioned version to enable vectorization in harmonic mean functions
REAL         cafepefmult_expanded(MAXGROUP, MAXCLASS, MAXLDV,MNUMYR)       ! Re-dimensioned version to enable vectorization in harmonic mean functions

!...Size class model (CGSHARE)
REAL         CLASS_SHARE(MNUMCR,MAXCLASS,MAXGROUP,BYR:LYR)          ! vehicle class market shares (within vehicle groups)
REAL         COEF_A(MAXCLASS,MAXGROUP)                              ! ATV Y-intercept or alpha coefficient
REAL         COEF_B(MAXCLASS,MAXGROUP)                              ! ATV fuel price elasticities
REAL         COEF_C(MAXCLASS,MAXGROUP)                              ! ATV income elasticities
REAL         COEF_P(MAXCLASS,MAXGROUP)                              ! ATV vehicle price elasticities
REAL         RATIO_BYR                                              ! used to determine size class shares 
REAL         RATIO_LN                                               ! used to determine size class shares
REAL         RATIO                                                  ! used to determine size class shares
REAL         GRPSHARE(MNUMCR,MAXGROUP,MNUMYR)	                    ! car and light truck sales shares by group
REAL		 OWNSALESSHR(MAXOWNER,MAXGROUP,MAXCLASS,MAXLDV,MNUMCR-2,MNUMYR)	! car and light truck sales shares by owner type
REAL		 ownsaletemp(maxowner,maxgroup,maxclass,mnumcr-2,mnumyr)
REAL         GROUPSUM(MAXGROUP)                                     ! sum of class shares by manufacturer

!...Plug-in Electric Vehicle modeling
REAL		 BatPackWgt(BYR:LYR,MAXCLASS,MAXGROUP,MAXLDV)			! Battery weight (full pack) for HEV,PHEV,BEV,and FCVs
REAL         ElecSysIncCost(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)  ! Cost of on-board electricity systems and storage
! 	Battery model parameters
REAL         NiMH_Cost(BYR:LYR)                                     ! Nickel metal hydride battery cost ($/kWhr)
REAL         PACK_A(MAXLDV)                                         ! Cumulative Li-ion battery pack initial cost parameter
REAL         PACK_B(MAXLDV)                                         ! Cumulative Li-ion battery pack learning rate parameter
REAL         MAT_A(MAXLDV)                                          ! Cumulative Li-ion materials parameter
REAL         MAT_B(MAXLDV)                                          ! Cumulative Li-ion materials learning rate parameter
REAL		 MAT_MARKUP(MNUMYR)										! Price adjustment to account for critical mineral supply constraints
REAL         EV_range(MAXGROUP,MAXCLASS,maxldv,byr:lyr)             ! EV all electric range for EV100, EV200, EV300, PHEV20, PHEV50

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

!   State and federal purchase incentives
INTEGER		 SwitchLDV_HR1										    ! {0: HR1 not enacted, 1: HR1 enacted}	
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
REAL		MMAVAIL(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,byr:lyr)      ! ATV make/model availability
REAL		X210(MAXGROUP,MAXCLASS,MAXLDV,MNUMCR-2,MNUMYR)          ! ATV calibration coefficients
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
REAL         GAS_PUMP_PER_STA                                       ! Gasoline pumps per station
REAL         GAS_HRLY_THRUPUT                                       ! Throughput per gasoline pump, in cars per hour
REAL         INITSTA(maxfuel,MNUMYR,MNUMCR-2)                       ! initial refueling stations by fuel, yr, region
REAL         STA_RAT(maxfuel)                                       ! refuel stations per vehicle stock
REAL         PRT_CNT(MAXCHRG,MNUMYR,MNUMCR-2)                       ! ev charging ports count by type, region and year (2016:2032)
REAL         PRT_CNT_nc(MAXCHRG,MNUMYR,MNUMCR-2)                    ! No Cafe - ev charging ports count by type, region and year (2016:2032)
REAL         chg_dist(MNUMCR,3,MNUMYR)                              ! Distribution of BEV charging consumption by type {1:DCFC, 2:Home, 3:L2}
REAL         PRT_RT(MAXCHRG)                                        ! ev time to refuel by type
real         CHGCSTMULT(MAXCHRG)                                    ! Markup on comm'l electricity cost (represents what folks actually pay to charge)
INTEGER      CHR_STR_YR                                             ! first year charger data
INTEGER      CHR_LST_YR                                             ! last year charger data
REAL         ELAS_FAVL                                              ! EV stock share elasticity of fuel availability (for endogenous EVSE growth)
REAL         MAINTGRP(MAXLDV,MAXCLASS,MAXVTYP)                      ! vehicle maintenance cost by tech, size class & type
REAL         WGT(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)                   ! light duty vehicle weight
REAL         PSPR(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,BYR:LYR)        ! vehicle price
REAL		 VRNG(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,BYR:LYR)          ! vehicle range
REAL         FPRICE(MAXLDV,MNUMCR,BYR:LYR)                          ! fuel price by region
REAL         FLCOST(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR,BYR:LYR)        ! fuel cost per mile 
REAL         BRCOST25(MAXGROUP,MAXLDV,MAXCLASS,BYR:LYR)             ! battery replacement cost - currently set to zero
REAL         ACCL(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,BYR:LYR)        ! vehicle acceleration - 0 to 60 mph
REAL         HFUEL(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,BYR:LYR)       ! home refueling 
REAL         MAINT(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2,BYR:LYR)       ! vehicle maintenance cost
REAL         LUGG(MAXGROUP,MAXLDV,MAXCLASS,MNUMCR-2)                ! vehilce luggage space	 
REAL		 LUGGAVG(MAXGROUP,MAXCLASS)
REAL         TrueMPG_regn(mnumcr,2,mnumyr)                          ! Regional true MPG variable

! ... Light Duty Vehicle Fleet Module
REAL         OLDFSTKT(MNUMCR,MAXVTYP,MAXLDV,MAXAGE)
REAL         SURVFLT(MAXFLEET,MAXAGE,MAXVTYP)                       ! LDV survival rate by fleet type
REAL         FLTVMTYR(MAXFLEET,MNUMYR,MAXVTYP)                      ! annual miles of travel per vehicle
REAL		 FLTTRANS(MAXFLEET,MAXAGE,MAXVTYP)  					! fraction of ldvs transfering from a given fleet to households
REAL         TOTFLTCAR(MAXVTYP)                                     ! total fleet car
REAL         FLTMPGNEW(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MNUMYR)   	! fleet MPG by vehicle type, fleet type, powertrain
REAL 		 FLTMPGSTK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXAGE,MNUMYR)
REAL         MPGFLTSTK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,mnumyr)              ! fleet
REAL         FLTFUELBTU(MNUMCR,MAXFUEL,MNUMYR)                      ! fleet
REAL         OLDFSTK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXAGE)            	! fleet vehicles transfered to HH stock
REAL         FLTECHSAL(MNUMCR,MAXVTYP,MAXFLEET,MAXCLASS,MAXLDV,MAXHAV) 	! fleet sales
REAL         FLTECHSTK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXHAV)          	! fleet stock
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
REAL         COSTMI(MNUMCR,MNUMYR)                                  ! fuel cost of driving 1 mile (2004 cents per gallon)

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
REAL         TRCOVID(MNUMCR-2,MNUMYR)                                ! transit rail COVID impact
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
REAL         IRPMPC(MNUMYR)                                         ! passenger miles per capita (16+)
REAL         IRRPM(MNUMYR)                                          ! intercity rail passenger miles traveled
REAL         IRRPMHIST(MNUMYR)                                      ! historic intercity rail passenger miles traveled
REAL         IREFF(MNUMYR)                                          ! intercity rail efficiengy (Btu/passenger mile)
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
REAL    TBFSHR(MNUMCR-2,8,MNUMYR)                                 	! fuel share by Census Division
REAL    TBFSHRHIST(8,MNUMYR,MNUMCR-2)                             	! historic fuel share by Census Division 
REAL    TBBTUPM(MNUMCR-2,MNUMYR)                                  	! Btu per passenger mile
REAL    TBBTUPMHIST(MNUMCR-2,MNUMYR)                              	! historic Btu per passenger mile
REAL    TBSYSEFF(MNUMCR-2)                                        	! historic improvement in Btu per passenger mile
REAL	TB_CAV_ADJ(MNUMYR)                                        	! pmt adjustment due to CAV growth
!...  Intercity and school bus
INTEGER IBSBHISTYEAR                                              	! last year of historical data
REAL    IBCOEFF(2)                                                  ! Intercity bus travel demand equation coefficients
REAL    TMODINIT(2,MNUMYR)                                          ! historic bus passenger miles
REAL    TMCOVID(2,MNUMYR)											! COVID travel impact for intercity and school bus
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
REAL    SBCOEFF(2)                                                  ! School bus travel demand equation coefficients

! ... Recreational Boating
REAL         RECFD(2,MNUMYR)                                        ! energy demand by fuel type (gasoline, diesel)
REAL         RECFDH(2,MNUMYR)                                       ! historic energy demand by fuel type
INTEGER      RBHISTYR                                               ! index year for last historic record
REAL         RBBOAT_COEF(2)                                         ! coefficients for relationship b/w rec boat count and gdp/cap
REAL         RBMG_COEF(2)                                           ! coefficients for annual gasoline consumption per boat
REAL         RBDS_COEF                                              ! coefficient for annual diesel consumption per boat
REAL         RBEDPC(2,MNUMYR)                                       ! energy demand per capita by fuel type
REAL         QRECR(2,MNUMCR,MNUMYR)                                 ! recreational boat energy demand by region

! ... Transportation Lubricant Demand
REAL         LUBFD(MNUMYR)                                          ! transportation lubricant demand
REAL         LUBFDH(MNUMYR)                                         ! historic transportation lubricant demand
INTEGER      LUBHISTYR                                              ! index year for last historic record 

REAL         Cyc_RPM(MNUMYR)                                        ! motorcycle passenger miles
REAL         Cyc_MPG(MNUMYR)                                        ! motorcycle fuel economy
INTEGER      CycHistYR                                              ! index year for last historic record
REAL         TNCSALE(MNUMCR,MNUMYR)
REAL         TNTSALE(MNUMCR,MNUMYR)

REAL         AHPCAR(MNUMCR,MNUMYR)                                  ! average car horsepower
REAL         AHPTRUCK(MNUMCR,MNUMYR)                                ! average light truck horsepower
REAL         AWTCAR(MNUMCR,MNUMYR)                                  ! average car weight
REAL         AWTTRUCK(MNUMCR,MNUMYR)                                ! average light truck weight
REAL         RANGE(MAXGROUP,MAXCLASS,BASE:CURRENT,MAXLDV)           ! vehicle driving range
LOGICAL*1    REQUIRED                                               ! indicates required subsystem technology
REAL         PE(MNUMYR)                                             ! vmt price elasticity
REAL         IE(MNUMYR)                                             ! vmt income elasticity
REAL         DE(MNUMYR)
REAL         TOTLEV(MNUMCR)
REAL         TQLDV(MAXFUEL,MNUMCR,MNUMYR)
REAL         FAVL(MAXLDV,MNUMCR-2,BYR:LYR)
REAL		 LDV_SALES(MAXGROUP,MAXCLASS,MAXLDV,MNUMCR,MNUMYR)    ! consumer choice model total ldv sales (household and fleet)
REAL 		 HHGRPSAL(MNUMCR,MAXGROUP,MAXCLASS,MAXLDV,MNUMYR)	  ! household vehicle sales by mfr group 
REAL 		 HHTECHSAL(MNUMCR,MAXVTYP,MAXCLASS,MAXLDV,MNUMYR)	  ! household vehicle sales by vehicle type
REAL		 HHMPGNEW(MNUMCR,MAXVTYP,MAXLDV,MNUMYR)			  	  ! household new vehicle fuel economy
REAL		 HHMPGSTK(MNUMCR,MAXVTYP,MAXLDV,MAXAGE,MNUMYR)
REAL 		 HHTECHGGE(MNUMCR,MAXVTYP,MAXLDV,MAXAGE,MNUMYR)
REAL 		 HHTECHBTU(MNUMCR,MAXVTYP,MAXLDV,MNUMYR)
REAL 		 LDVMPGNEW(MNUMCR,MAXVTYP,MAXLDV,MNUMYR)              ! average new ldv mpg 
REAL		 APSHRGRP(MAXGROUP,MAXCLASS,MAXLDV,MNUMCR,MNUMYR)	  ! Powertrain adoption shares from consumer choice model
REAL         EXPENDVEH(2,MNUMYR)                                    ! total expenditures on vehicle purchases (based on MSRP, not incl. incentives)
REAL         FLTMPGSTK_LDV(MAXLDV,MNUMYR)							! fleet stock fuel economy by fuel type
REAL         MPGHH(MNUMYR)											! household stock fuel economy
REAL         HHMPGSTK_LDV(MAXLDV,MNUMYR)							! household stock fuel economy by fuel type
REAL         HHMPGSTK_TYP(MAXVTYP,MNUMYR)                           ! household stock fuel economy
REAL         HHMPGSTK_TYPLDV(MAXLDV,MAXVTYP,MNUMYR)                 ! household stock fuel economy by powertrain
REAL         HHMPGSTK_TYPREG(MNUMCR,MAXVTYP,MNUMYR)                 ! household stock fuel economy by region
REAL         VMT_STK_HH(maxvtyp,MAXLDV,MAXAGE,MAXHAV,mnumcr)      	! hh vehicle miles traveled car by region derived from stocks
REAL 		 HHTECHVMT(MNUMCR,MAXVTYP,MAXLDV,MAXAGE)
REAL         BVMT_STK_HH(maxvtyp,MAXLDV,MAXAGE,MAXHAV,mnumcr-2)     ! benchmarked hh vehicle miles traveled car by region
REAL         LDVSTK(MAXLDV,MNUMYR)                          		! total light duty vehicle stock by ILDV
REAL         VSTK(maxvtyp,MAXLDV)                           		! total light duty vehicle stock 
REAL         MFR_SALES(MNUMCR,MAXGROUP,MAXCLASS,MNUMYR)

! ... Variables for CAFE banking.
real CafeBankA(MAXGROUP),CafeBank(5,MAXGROUP),BankBal(MAXGROUP,byr:lyr),CafeWork(5,MAXGROUP),CafeNeed
common/BankVars/CafeBankA,CafeBank,CafeWork,CafeNeed

common/FPVars/FPrint,TFCoefA,TFCoefB,TFCoefC,TFCoefD,FPMpg, FPMpgGrp,Cafe_Used,IBank,&
             CFCoefA,CFCoefB,CFCoefC,CFCoefD,TFCoefA2,TFCoefB2,TFCoefC2,TFCoefD2,TFCoefE2,TFCoefF2,TFCoefG2,TFCoefH2,&			
             CFCoefA2,CFCoefB2,CFCoefC2,CFCoefD2,CAFEPEFMULT,EPAALTMULT,ENFORCE_EPA,ENFORCE_MY27REGS,ENFORCE_CAFE,AC_CO2_OFFSET,&									
			 CFCoefEPAA2,CFCoefEPAB2,CFCoefEPAC2,CFCoefEPAD2,TFCoefEPAA2,TFCoefEPAB2,TFCoefEPAC2,TFCoefEPAD2,TFCoefEPAE2,&		
			 TFCoefEPAF2,TFCoefEPAG2,TFCoefEPAH2																				

!=======================================================================================================
REAL         TOTALSALSC(maxvtyp,MAXCLASS,MAXLDV,MNUMYR)
REAL         CarSales,TrkSales

!===================================================================================
! ... to accomodate view variables in the Visual Studio, the variables are
! ... added to common blocks here.  Then the USE statement for module T_ must go in every 
! ... subroutine.

common/trancomreal/INC00_D_16, ROUNDOFF_ERROR, DEL_FE, DEL_COSTABS, DEL_COSTWGT, DEL_WGTABS, DEL_WGTWGT, DEL_HP, 	&
	COEFF_LEARN, COEFF_LRN1, COEFF_LRN2, YEARS_MKTD, LEARN_COST_MULTIPLIER, VMT, FE,	&
	WEIGHT, PRICE, HP, VALUEPERF, PERFFACT, TANKSIZE, PERFCAP, USEDCAP, MKT_PEN, MKT_MAX, SYNR_DEL, MANDMKSH, 		&
	DISCOUNT, REG_COST, CAFE_STAND, REF_MPG,CLASS_SHARE, COEF_A, COEF_B, COEF_C, COEF_P, COEF_E, AFVADJHP,&
	AFVADJFE, AFVADJWT, AFVADJPR, NiMH_Cost, PHEV_DOD, EV_DOD, BATTERY, ElecSysIncCost, FuelCell_D_kW, 				&
	ElecNonBattCst, PctPHEV20, PctPHEV50, BatPackWgt, IRA_Bat_CRed, IRA_Veh_cred, 		&
	FUELCELL, TEC_ORNL, MKT_PENF, AVCOST, MMAVAIL, X210, FAVAIL, INITSTA, GAS_HRLY_THRUPUT,GAS_PUMP_PER_STA,STA_RAT, PRT_CNT, PRT_RT,CHGCSTMULT, 		&
	MAINTGRP,SURVFLT,FLTVMTYR, TOTFLTCAR, SURVP, SURVLT, PVMT, LVMT, VMT_SCHED_PARAM, &
	FLTTRANS, CMPGSTKGAS95, STKAVGWGT, M_CD_AGE_DIST, F_CD_AGE_DIST, M_CD_AGE_DIST_L, 				&
	F_CD_AGE_DIST_L, M_CD_AGE_DIST_H, F_CD_AGE_DIST_H, AGE_ADJ, AGE_ADJ_L, AGE_ADJ_H, BETACOST, BETAINC, BETAVMT, 	&
	BETAVPLD, BETAEMP, ALPHA, FREFF, HTFREFF, RPROJ_NCTONMI, RPROJ_CTONMI, 	&
	NEWCLS2B, RHIST_NCTONMI, RHIST_CTONMI, RAIL_TONMILE, DSHIP_TONMILE, RTM_OUTPUT, NEWLDVs, CarTrkSplit,  &
	MILTRSHR90, TMODINIT, TMCOVID, TMEFFINIT, QMODFSHR, QMODFSHRH, CARLTSHR, NEWLDVPERLD, 	& 
	DUMM, FUELTAX, FUELTAX87, TQDSHIP, GRPSHARE, AHPCAR, AHPTRUCK, AWTCAR, AWTTRUCK, PMGTR90_D_, INC90_D_NP, 		&
	REGCOST, FUELSAVE, TECHCOST, COSTEF_FUEL, COSTEF_PERF, MMAX, ACTUAL_MKT, MKT_FUEL, MKT_PERF, VAL_PERF, ADJFE, 	&
	DELTA_MKT, REQ_MKT, PRICE_EX, PSLOPE, RATIO_LN, RATIO, GROUPSUM, TOT_MKT, MAX_SHARE, RATIO_BYR, RANGE,	&
	OLD_PMAX, SUM_MKS, SUM_MKS_FE, WGT, PSPR, 	&
	FPRICE, FLCOST, BRCOST25, ACCL, HFUEL, MAINT, LUGG, MPGHH, PE, IE, VMTLD, VPLD, LDVCOVID, VMTLDHISTYR, 			&
	LICDRIVER, LIC_TREND, LIC_ELAS, LIC_MAX, LICRATE_M, LICRATE_F, LicRHistYr, DE,  	&
	FLTMPGNEW, TOTLEV, TQLDV, BTQLDV, BTQISHIPR, BTQDSHIPR, BTQRAILR, BQJETR, BQAGR, BQMILTR,&
	BQRECR, BQLUBR, BFLTFUELBTU, BTQFREIRSC, BFVMTECHSC, BVMTECH, BFLTVMTECH, BASMDEMD, BRTMTT, FAVL, APSHRGRP, 	&
	MPGFLTSTK, FLTFUELBTU, OLDFSTK, FLTECHSTK, FLTECHSAL, EXPENDVEH, TQFRAILT, & 
	SSURV25, QMILTR, QMTRR, QMTBR, QRECR, MFD, TMFD, RECFD, LUBFD, TMOD, TMEFF, HHMPGSTK_LDV, FLTMPGSTK_LDV, HHMPGSTK_TYP, &
	HHMPGSTK_TYPLDV, VMT_STK_HH, BVMT_STK_HH, RTMTT, TQRAILR, LDVSTK, & 
	VSTK, VMTLDV, COSTMI, TOTALSALSC, CarSales,TrkSales, mfr_sales, &
	MER_tran, carltshr_regn, CCONSTANT, CRHO, CINC, CFUEL, 	&
	CHP, CWGT, CMPG, CDUMM, TCONSTANT, TRHO, TINC, TFUEL, THP, TWGT, TMPG, TDUMM, truempg_regn, LDVNEWMPG, DEGFAC, DEGFACGRP, OWNSALESSHR

common/tran_easy/tmgtcbus,tdstcpus,tjftcbus,trftcbus

common/trancomint4/iy, num_to_read, First_Read_Year,TRANEFF,TRANAB32, ymer, ysteo, SYNERGY, MANDYEAR

common/trancomint2/YRS,N, LASTID, ICL,IGP,IVTYP,ITECH, INOTE, IYR, ILDV, ifuel, IFLEET, IREGN, IRAILREGN, IHAV,&
	IAGE, I, J, K, SIGN_TDM, NUMTECH, NUM_REQ, NUM_SUP, NUM_MAN, NUM_SYN, PAYBACK, FEM_PASS, 		&
	RETURN_STAT

common/trancomlog/TECH_APPLIC, CLASSFLAG, MAND_ORIDE, CAFEPASS, REQUIRED

common/trancomchar/TECHLABEL, SYS_AFFECT, GROUPLABEL, CLASSLABEL

!...Map for how groups are defined back to vehicle types: cars (1) and light trucks (2)
data GrpMap/1,1,1,1,1,2,2,2,2,2,2/

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
      
end module T_

! ==========================================================================================================
!...MEAN_FUNCS houses algorithms to calculate regular weighted and harmonic weighted means.
!   Note that the output arrays are initialized to zero in each of the functions contained.
!   Functions HARMONIC_MEAN_1D, 2D, and 3D calculate weighted harmonic averages of fuel economy (MPGS) using
!   associated input weights (WEIGHTS). This calculation is vectorized to fully utilize fortran's speed
!   in array computations. Each function is built to collapse 1, 2, or 3 dimensions in a given MPGS/WEIGHTS array pair.
!   2D and 3D implementation also has an optional input, CREDS, which should be in units of gallons per mile.

!   Inputs: MPGS, WEIGHTS, CREDS
!   Output: HM_1D/2D/3D

!   Functions WEIGHTED_MEAN_1D, 2D, and 3D calculate weighted averages of any attribute (VALUES) using associated
!   input weights (WEIGHTS).

!   Inputs: VALUES, WEIGHTS
!   Output: WM_1D/2D/3D
! ==========================================================================================================
MODULE MEAN_FUNCS
  USE, INTRINSIC :: IEEE_ARITHMETIC
  IMPLICIT NONE

CONTAINS

! WEIGHTED MEANS
  ! --- 1D Arrays ---
  FUNCTION WEIGHTED_MEAN_1D(VALUES, WEIGHTS,caller_id) RESULT(WM_1D)
    REAL, INTENT(IN) :: VALUES(:)
    REAL, INTENT(IN) :: WEIGHTS(:)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: caller_id
    REAL :: WM_1D

    REAL :: total_weights
    REAL :: weighted_sum_numerator
    CHARACTER(LEN=100) :: error_prefix
    
!   Optional caller_id to print in case of error
    IF (PRESENT(caller_id)) THEN
        error_prefix = " (" // TRIM(caller_id) // ") "
    ELSE
        error_prefix = ""
    END IF
    
    IF (ANY(IEEE_IS_NAN(VALUES))) THEN
        PRINT *, "ERROR TDM: (WEIGHTED_MEAN_1D", TRIM(error_prefix), "): Input VALUES array contains NaN."
        WM_1D = 0.0
        STOP
    END IF
    
    IF (SIZE(VALUES) /= SIZE(WEIGHTS)) THEN
        PRINT *, "ERROR TDM: (WEIGHTED_MEAN_1D", TRIM(error_prefix), "): VALUES and WEIGHTS must have the same size."
        WM_1D = 0.0
        STOP
    END IF

    total_weights = SUM(WEIGHTS)
    WM_1D = 0.0 ! Initialize result

    IF (total_weights == 0.0) THEN
      ! Cannot calculate weighted mean if total weights are zero.
      ! Returns 0.0 as initialized.
      RETURN
    END IF

    ! Calculate the sum of VALUES(I) * WEIGHTS(I)
    weighted_sum_numerator = SUM(VALUES * WEIGHTS)
    
    WM_1D = weighted_sum_numerator / total_weights

  END FUNCTION WEIGHTED_MEAN_1D

! --- 2D Arrays ---
  FUNCTION WEIGHTED_MEAN_2D(VALUES, WEIGHTS,caller_id) RESULT(WM_2D)
    REAL, INTENT(IN) :: VALUES(:,:)
    REAL, INTENT(IN) :: WEIGHTS(:,:)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: caller_id
    REAL :: WM_2D

    REAL :: total_weights
    REAL :: weighted_sum_numerator
    CHARACTER(LEN=100) :: error_prefix
    
!   Optional caller_id to print in case of error
    IF (PRESENT(caller_id)) THEN
        error_prefix = " (" // TRIM(caller_id) // ") "
    ELSE
        error_prefix = ""
    END IF
    
!   Check for NaNs and make sure array shapes are identical
    IF (ANY(IEEE_IS_NAN(VALUES))) THEN
        PRINT *, "ERROR TDM: (WEIGHTED_MEAN_2D", TRIM(error_prefix), "): Input VALUES array contains NaN."
        WM_2D = 0.0
        STOP
    END IF
    
    IF (ANY(SHAPE(VALUES) /= SHAPE(WEIGHTS))) THEN
        PRINT *, "ERROR TDM: (WEIGHTED_MEAN_2D", TRIM(error_prefix), "): VALUES and WEIGHTS must have the same shape."
        WM_2D = 0.0
        STOP
    END IF

    total_weights = SUM(WEIGHTS)
    WM_2D = 0.0 ! Initialize result

    IF (total_weights == 0.0) THEN
      RETURN
    END IF

    weighted_sum_numerator = SUM(VALUES * WEIGHTS)
    
    WM_2D = weighted_sum_numerator / total_weights

  END FUNCTION WEIGHTED_MEAN_2D

! --- 3D Arrays ---
  FUNCTION WEIGHTED_MEAN_3D(VALUES, WEIGHTS,caller_id) RESULT(WM_3D)
    REAL, INTENT(IN) :: VALUES(:,:,:)
    REAL, INTENT(IN) :: WEIGHTS(:,:,:)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: caller_id
    REAL :: WM_3D

    REAL :: total_weights
    REAL :: weighted_sum_numerator
    CHARACTER(LEN=100) :: error_prefix
    
!   Optional caller_id to print in case of error
    IF (PRESENT(caller_id)) THEN
        error_prefix = " (" // TRIM(caller_id) // ") "
    ELSE
        error_prefix = ""
    END IF    

!   Check for NaNs and make sure array shapes are identical
    IF (ANY(IEEE_IS_NAN(VALUES))) THEN
        PRINT *, "ERROR TDM: (WEIGHTED_MEAN_3D", TRIM(error_prefix), "): Input VALUES array contains NaN."
        WM_3D = 0.0
        STOP
    END IF
    
    IF (ANY(SHAPE(VALUES) /= SHAPE(WEIGHTS))) THEN
        PRINT *, "ERROR TDM: (WEIGHTED_MEAN_3D", TRIM(error_prefix), "): VALUES and WEIGHTS must have the same shape."
        WM_3D = 0.0
        STOP
    END IF

    total_weights = SUM(WEIGHTS)
    WM_3D = 0.0 ! Initialize result

    IF (total_weights == 0.0) THEN
      RETURN
    END IF

    weighted_sum_numerator = SUM(VALUES * WEIGHTS)
    
    WM_3D = weighted_sum_numerator / total_weights

  END FUNCTION WEIGHTED_MEAN_3D

! WEIGHTED HARMONIC MEANS
! --- Implementation for 1D Arrays ---
  FUNCTION HARMONIC_MEAN_1D(MPGS, WEIGHTS,caller_id) RESULT(HMEAN_1D)
    REAL, INTENT(IN) :: MPGS(:)
    REAL, INTENT(IN) :: WEIGHTS(:)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: caller_id
    REAL :: HMEAN_1D

    REAL :: total_weights
    REAL :: weighted_inverse_sum_numerator
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: valid_terms_mask
    REAL, ALLOCATABLE, DIMENSION(:) :: inverse_mpg_terms
    CHARACTER(LEN=100) :: error_prefix

!   Optional caller_id to print in case of error
    IF (PRESENT(caller_id)) THEN
        error_prefix = " (" // TRIM(caller_id) // ") "
    ELSE
        error_prefix = ""
    END IF

    IF (SIZE(MPGS) /= SIZE(WEIGHTS)) THEN
        PRINT *, "ERROR TDM (HARMONIC_MEAN_2D", TRIM(error_prefix), "): MPGS and WEIGHTS must have the same size."
        HMEAN_1D = 0.0
        STOP
    END IF

    total_weights = SUM(WEIGHTS)
    HMEAN_1D = 0.0

    IF (total_weights == 0.0) RETURN
    
    ! Allocate local arrays to match the shape of MPGS
    ALLOCATE(valid_terms_mask(SIZE(MPGS)), inverse_mpg_terms(SIZE(MPGS)))
    
    valid_terms_mask = (MPGS /= 0.0) .AND. (WEIGHTS /= 0.0)
    inverse_mpg_terms = 0.0
    WHERE (valid_terms_mask)
      inverse_mpg_terms = WEIGHTS * (1.0 / MPGS)
    END WHERE
    weighted_inverse_sum_numerator = SUM(inverse_mpg_terms)
    
    DEALLOCATE(valid_terms_mask, inverse_mpg_terms)
    
    IF (weighted_inverse_sum_numerator == 0.0) RETURN
    HMEAN_1D = total_weights / weighted_inverse_sum_numerator
  END FUNCTION HARMONIC_MEAN_1D


! --- Implementation for 2D Arrays ---
  FUNCTION HARMONIC_MEAN_2D(MPGS, WEIGHTS,CREDS,caller_id) RESULT(HMEAN_2D)
    REAL, INTENT(IN) :: MPGS(:,:)
    REAL, INTENT(IN) :: WEIGHTS(:,:)
    REAL, INTENT(IN),OPTIONAL :: CREDS(:,:)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: caller_id
    REAL :: HMEAN_2D

    REAL :: total_weights
    REAL :: weighted_inverse_sum_numerator
    LOGICAL, ALLOCATABLE, DIMENSION(:,:) :: valid_terms_mask
    REAL, ALLOCATABLE, DIMENSION(:,:) :: inverse_mpg_terms
    CHARACTER(LEN=100) :: error_prefix
    
    INTEGER, DIMENSION(2) :: current_shape_mpgs
    
!   Optional caller_id to print in case of error
    IF (PRESENT(caller_id)) THEN
        error_prefix = " (" // TRIM(caller_id) // ") "
    ELSE
        error_prefix = ""
    END IF
    
    IF (ANY(SHAPE(MPGS) /= SHAPE(WEIGHTS))) THEN
        PRINT *, "ERROR TDM (HARMONIC_MEAN_2D", TRIM(error_prefix), "): MPGS and WEIGHTS must have the same shape."
        HMEAN_2D = 0.0
        STOP
    END IF

    total_weights = SUM(WEIGHTS)
    HMEAN_2D = 0.0

    IF (total_weights == 0.0) RETURN
    
    current_shape_mpgs = SHAPE(MPGS)
    
    ! Allocate local arrays to match the shape of MPGS
    ALLOCATE(valid_terms_mask(current_shape_mpgs(1), current_shape_mpgs(2)), &
             inverse_mpg_terms(current_shape_mpgs(1), current_shape_mpgs(2)))

    valid_terms_mask = (MPGS /= 0.0) .AND. (WEIGHTS /= 0.0)
    inverse_mpg_terms = 0.0
    
    IF (PRESENT(CREDS)) THEN
      WHERE (valid_terms_mask)
        inverse_mpg_terms = WEIGHTS * (1.0 / MPGS - CREDS)
      END WHERE
    ELSE 
      WHERE (valid_terms_mask)
        inverse_mpg_terms = WEIGHTS * (1.0 / MPGS)
      END WHERE
    ENDIF
    
    weighted_inverse_sum_numerator = SUM(inverse_mpg_terms)
    
    DEALLOCATE(valid_terms_mask, inverse_mpg_terms)
    
    IF (weighted_inverse_sum_numerator == 0.0) RETURN
    HMEAN_2D = total_weights / weighted_inverse_sum_numerator
  END FUNCTION HARMONIC_MEAN_2D


! --- Implementation for 3D Arrays ---
  FUNCTION HARMONIC_MEAN_3D(MPGS, WEIGHTS, CREDS,caller_id) RESULT(HMEAN_3D)
    REAL, INTENT(IN) :: MPGS(:,:,:)
    REAL, INTENT(IN) :: WEIGHTS(:,:,:)
    REAL, INTENT(IN),OPTIONAL :: CREDS(:,:,:)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: caller_id
    REAL :: HMEAN_3D

    REAL :: total_weights
    REAL :: weighted_inverse_sum_numerator
    LOGICAL, ALLOCATABLE, DIMENSION(:,:,:) :: valid_terms_mask
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: inverse_mpg_terms
    CHARACTER(LEN=100) :: error_prefix
    
    ! Intermediate array to hold the shape
    INTEGER, DIMENSION(3) :: current_shape_mpgs
    
!   Optional caller_id to print in case of error
    IF (PRESENT(caller_id)) THEN
        error_prefix = " (" // TRIM(caller_id) // ") "
    ELSE
        error_prefix = ""
    END IF

    IF (ANY(SHAPE(MPGS) /= SHAPE(WEIGHTS))) THEN
        PRINT *, "ERROR TDM (HARMONIC_MEAN_3D", TRIM(error_prefix), "): MPGS and WEIGHTS must have the same shape."
        HMEAN_3D = 0.0
        STOP
    END IF

    total_weights = SUM(WEIGHTS)
    HMEAN_3D = 0.0

    IF (total_weights == 0.0) RETURN
    
    current_shape_mpgs = SHAPE(MPGS)
    ! Allocate local arrays to match the shape of MPGS
    ALLOCATE(valid_terms_mask(current_shape_mpgs(1), current_shape_mpgs(2), current_shape_mpgs(3)), &
             inverse_mpg_terms(current_shape_mpgs(1), current_shape_mpgs(2), current_shape_mpgs(3)))

    valid_terms_mask = (MPGS /= 0.0) .AND. (WEIGHTS /= 0.0)
    inverse_mpg_terms = 0.0
    
    IF (PRESENT(CREDS)) THEN
      WHERE (valid_terms_mask)
        inverse_mpg_terms = WEIGHTS * (1.0 / MPGS - CREDS)
      END WHERE
    ELSE 
      WHERE (valid_terms_mask)
        inverse_mpg_terms = WEIGHTS * (1.0 / MPGS)
      END WHERE
    ENDIF
    
    weighted_inverse_sum_numerator = SUM(inverse_mpg_terms)
    
    DEALLOCATE(valid_terms_mask, inverse_mpg_terms)
    
    IF (weighted_inverse_sum_numerator == 0.0) RETURN
    HMEAN_3D = total_weights / weighted_inverse_sum_numerator
  END FUNCTION HARMONIC_MEAN_3D

END MODULE MEAN_FUNCS

!======================================================================================================
  SUBROUTINE TRAN
  use T_
  USE MEAN_FUNCS
  IMPLICIT NONE
    INCLUDE 'NGTDMOUT'
    integer do_once/0/

!   Timers
    INTEGER(KIND=4) :: start_count, end_count, count_rate, count_max
    REAL(KIND=8)    :: elapsed_time_seconds,elapsed_time_seconds_TRANFRT, &
                       elapsed_time_seconds_TRANAIR,elapsed_time_seconds_TBENCHMARK, &
                       elapsed_time_seconds_TSHIP
    INTEGER         :: result


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
	IRA_STIM   = 0.0

!...2022 IRA tax credits
    LEGIRA = RTOVALUE('LEGIRA  ',0)
    if(LEGIRA.gt.0)then
      IRA_STIM = 1.0
    endif	

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
      
!...  Initialize allocated variables to zero      
      FEMMPG = 0.0
      FEMHP = 0.0
      FEMPRI = 0.0
      FEMWGT = 0.0
      FEMRNG = 0.0
      FEMTSZ = 0.0
      FEMPEN = 0.0
      BatPackSize = 0.0
      EV_RNG = 0.0
      FPRT = 0.0
      CAFESALES = 0.0
      MPGCOMP = 0.0
      MPGADJ = 0.0
      PHEV_EVMT = 0.0
      PHEVMPG_S = 0.0
      PHEVMPG_D = 0.0
      NAMEPLATE = 0.0
      EPAMPG = 0.0
      EPAHP = 0.0
      EPAPRI = 0.0
      EPAWGT = 0.0
      EPARNG = 0.0
      EPATSZ = 0.0
      EPALUG = 0.0
      OWN_SALES = 0.0
      FLT_STOCK = 0.0
      LDV_STOCK = 0.0
      
      CALL SYSTEM_CLOCK(COUNT=start_count, COUNT_RATE=count_rate, COUNT_MAX=count_max)
      CALL READLDV
      CALL READVMT
      CALL SYSTEM_CLOCK(COUNT=end_count)
      elapsed_time_seconds = REAL(end_count - start_count, KIND=8) / REAL(count_rate, KIND=8)
      WRITE(21,*) "Elapsed wall-clock time (READLDV): ", elapsed_time_seconds, " seconds"
      CALL SYSTEM_CLOCK(COUNT=start_count, COUNT_RATE=count_rate, COUNT_MAX=count_max)
	  CALL READSTOCK
      CALL SYSTEM_CLOCK(COUNT=end_count)
      elapsed_time_seconds = REAL(end_count - start_count, KIND=8) / REAL(count_rate, KIND=8)
      WRITE(21,*) "Elapsed wall-clock time (READSTOCK): ", elapsed_time_seconds, " seconds"
      CALL SYSTEM_CLOCK(COUNT=start_count, COUNT_RATE=count_rate, COUNT_MAX=count_max)
      CALL READHIST
      CALL SYSTEM_CLOCK(COUNT=end_count)
      elapsed_time_seconds = REAL(end_count - start_count, KIND=8) / REAL(count_rate, KIND=8)
      WRITE(21,*) "Elapsed wall-clock time (READHIST): ", elapsed_time_seconds, " seconds"
      CALL SYSTEM_CLOCK(COUNT=start_count, COUNT_RATE=count_rate, COUNT_MAX=count_max)
      CALL READNHTSA

    endif
    CALL TMAC
    CALL NEWLDV
    CALL TMPGNEW
    CALL TREG
    CALL TFLTSTKS
    if (yrs.ge.MINVAL(first_lidar_year)) CALL FLTHAV 	! if lidar is available, determine taxi / ride-hail fleet adoption of HAVs
    CALL TFLTVMTS
    CALL TSMOD
    CALL TMPGSTK
    CALL TCURB
    CALL TFLTMPGS
    CALL TFLTCONS
	CALL TVMT
      CALL SYSTEM_CLOCK(COUNT=start_count, COUNT_RATE=count_rate, COUNT_MAX=count_max)
    CALL TRANFRT(0)
    CALL TRANFRT(1)    ! in TRANFRT.F, 1 indicates a reporting call to TFRTRPT      ! MDRAEO2026 still use reporting call functionality?
      CALL SYSTEM_CLOCK(COUNT=end_count)
      elapsed_time_seconds_TRANFRT = elapsed_time_seconds_TRANFRT + REAL(end_count - start_count, KIND=8) / REAL(count_rate, KIND=8)
      if (n.eq.mnumyr.and.fcrl.eq.1) WRITE(21,*) "Elapsed wall-clock time (TRANFRT): ", elapsed_time_seconds_TRANFRT, " seconds"
    CALL TMPGAG
    CALL TRAIL
      CALL SYSTEM_CLOCK(COUNT=start_count, COUNT_RATE=count_rate, COUNT_MAX=count_max)
    CALL TSHIP
      CALL SYSTEM_CLOCK(COUNT=end_count)
      CALL SYSTEM_CLOCK(COUNT=start_count, COUNT_RATE=count_rate, COUNT_MAX=count_max)
      elapsed_time_seconds_TSHIP = elapsed_time_seconds_TSHIP + REAL(end_count - start_count, KIND=8) / REAL(count_rate, KIND=8)
      if (n.eq.mnumyr.and.fcrl.eq.1) WRITE(21,*) "Elapsed wall-clock time (TSHIP): ", elapsed_time_seconds_TSHIP, " seconds"
    CALL TRANAIR
      CALL SYSTEM_CLOCK(COUNT=end_count)
      elapsed_time_seconds_TRANAIR = elapsed_time_seconds_TRANAIR + REAL(end_count - start_count, KIND=8) / REAL(count_rate, KIND=8)
      if (n.eq.mnumyr.and.fcrl.eq.1) WRITE(21,*) "Elapsed wall-clock time (TRANAIR): ", elapsed_time_seconds_TRANAIR, " seconds"
    CALL TMISC
    CALL TCONS
    CALL TINTEG
      CALL SYSTEM_CLOCK(COUNT=start_count, COUNT_RATE=count_rate, COUNT_MAX=count_max)
    CALL TBENCHMARK
      CALL SYSTEM_CLOCK(COUNT=end_count)
      elapsed_time_seconds_TBENCHMARK = elapsed_time_seconds_TBENCHMARK + REAL(end_count - start_count, KIND=8) / REAL(count_rate, KIND=8)
      if (n.eq.mnumyr.and.fcrl.eq.1) WRITE(21,*) "Elapsed wall-clock time (TBENCHMARK): ", elapsed_time_seconds_TBENCHMARK, " seconds"
    CALL TREPORT

!...Calculate total highway vehicle CNG demand    
!...QGFTR: total transportation natural gas demand
!...QGFTRFV: CNG central refueling
!...QGFTRPV: CNG retail purchase
!...QGLTRFV: LNG central refueling
!...QGLTRPV: LNG retail refueling

    QGFTR(1:mnumcr,n)   = QNGTR(1:mnumcr,n)
!   Calculate retail CNG demand...heavy-duty truck demand populated in tranfrt.f
	QGFTRPV(1:mnumcr,n) = QGFTRPV(1:mnumcr,n) + TQLDV(4,1:mnumcr,n) * BENNG(1:mnumcr,n)	

!...Calculate total fleet vehicle (light and heavy central refueling) NG demand
!...Add H2 consumption to NG consumption (no HMM Right now)
	QGFTRFV(1:mnumcr,n) = QGFTRFV(1:mnumcr,n) + (FLTFUELBTU(1:mnumcr,4,n) + &
                          QMTBR(1,5,1:mnumcr,N) +       & ! transit bus   
                          QMTBR(2,5,1:mnumcr,N) +       & ! intercity bus  
                          QMTBR(3,5,1:mnumcr,N))        & ! school bus 
                           * BENNG(1:mnumcr,n)
    QRHTR(1:mnumcr,n)   = QRSTR(1:mnumcr,n) - max(0.0,benrs(1:mnumcr,n)*tqishipr(5,1:mnumcr,n)) - max(0.0,benrs(1:mnumcr,n)*TQRAILR(2,1:mnumcr,n))
    QRLTR(1:mnumcr,n)   = max(0.0,benrs(1:mnumcr,n)*tqishipr(5,1:mnumcr,n)) + max(0.0,benrs(1:mnumcr,n)*TQRAILR(2,1:mnumcr,n))

!...Calc average price of cars, trucks and vehicles
    AVG_PRC_CAR(curiyr) = 0.0
    AVG_PRC_TRK(curiyr) = 0.0
    Avg_PRC_VEH(curiyr) = 0.0
    
    CARSALES  = 0.0
    TRKSALES  = 0.0      
    
    do ICL=1,MAXCLASS
      do ILDV=1,MAXLDV
        if(LDVPRI(1,ILDV,ICL,yrs).gt.0.0) then
          Avg_Prc_Car(curiyr)=Avg_Prc_Car(curiyr)+LDVPRI(1,ILDV,ICL,yrs)*TotalSalsc(1,ICL,ILDV,n)
          CarSales=CarSales+TotalSalsc(1,ICL,ILDV,n)       
        endif
        if(LDVPRI(2,ILDV,ICL,yrs).gt.0.0) then
         Avg_Prc_Trk(curiyr)=Avg_Prc_Trk(curiyr)+LDVPRI(2,ILDV,ICL,yrs)*TotalSalsc(2,ICL,ILDV,n)
         TrkSales=TrkSales+TotalSalsc(2,ICL,ILDV,n)
        endif
      enddo
    enddo
    
    Avg_Prc_Veh(curiyr)=((Avg_Prc_Car(curiyr)+Avg_Prc_Trk(curiyr))/(CarSales+TrkSales))/1000.0
    Avg_Prc_Car(curiyr)=(Avg_Prc_Car(curiyr)/CarSales)/1000.0
    Avg_Prc_Trk(curiyr)=(Avg_Prc_Trk(curiyr)/TrkSales)/1000.0

!   These are alternate vehicle prices, which are fed into the macroeconomic module when scedes switch CAFE is flipped to 1.
    Full_Prc_Veh(curiyr)=Avg_Prc_Veh(curiyr)
    Full_Prc_Car(curiyr)=Avg_Prc_Car(curiyr)
    Full_Prc_Trk(curiyr)=Avg_Prc_Trk(curiyr)

	! Rail CNG
    QGFTRRAIL(1,1:mnumcr,n) = BTQRAILR(3,1:mnumcr)  ! freight rail
    QGFTRRAIL(2,1:mnumcr,n) = 0.0                ! intercity rail
    QGFTRRAIL(3,1:mnumcr,n) = 0.0                ! tranist rail
    QGFTRRAIL(4,1:mnumcr,n) = 0.0                ! commuter rail
    ! Rail LNG
    QGLTRRAIL(1,1:mnumcr,n) = BTQRAILR(4,1:mnumcr)  ! freight rail
    QGLTRRAIL(2,1:mnumcr,n) = 0.0                ! intercity rail
    QGLTRRAIL(3,1:mnumcr,n) = 0.0                ! tranist rail
    QGLTRRAIL(4,1:mnumcr,n) = 0.0                ! commuter rail        
    ! Ship CNG       
    QGFTRSHIP(1,1:mnumcr,n) = BTQDSHIPR(3,1:mnumcr) ! domestic ship
    QGFTRSHIP(2,1:mnumcr,n) = BTQISHIPR(3,1:mnumcr) ! international ship
    QGFTRSHIP(3,1:mnumcr,n) = 0.0                ! recreational boat
    ! Ship LNG
    QGLTRSHIP(1,1:mnumcr,n) = BTQDSHIPR(4,1:mnumcr) ! domestic ship
    QGLTRSHIP(2,1:mnumcr,n) = BTQISHIPR(4,1:mnumcr) ! international ship
    QGLTRSHIP(3,1:mnumcr,n) = 0.0                ! recreational boat        

  end subroutine tran


! ==========================================================================================================
! ... Subroutine READVMT reads in the annual VMT schedules aggregated from Polk odometer readings
!     Values are read in for ildv = [1,2,4,5,6,7,15,16]. The remaining ildvs are set equal to one
!     of these after read in.
! ==========================================================================================================
  SUBROUTINE READVMT
  USE T_
  IMPLICIT NONE
    
    LOGICAL        NEW/.FALSE./
    CHARACTER*18   INAME
    CHARACTER*18   FNAME
    INTEGER        WKUNIT
    
    INTEGER, PARAMETER      :: NUM_ROWS = 3600       ! Must set this manually based on input file trnldv_vmt.csv
    integer, PARAMETER      :: NUM_IND_COLS = 4
    INTEGER, PARAMETER      :: NUM_DATA_COLS = VMTYR - 2016 + 1
    INTEGER, PARAMETER      :: TOTAL_COLS = NUM_IND_COLS + NUM_DATA_COLS
    INTEGER, PARAMETER      :: MAX_LINE_BUFFER_LENGTH = 20 * TOTAL_COLS 

    INTEGER, DIMENSION(NUM_ROWS) :: col1_ivtyp
    INTEGER, DIMENSION(NUM_ROWS) :: col2_iregn
    INTEGER, DIMENSION(NUM_ROWS) :: col3_ildv
    INTEGER, DIMENSION(NUM_ROWS) :: col4_iage
    
    REAL, DIMENSION(NUM_ROWS,NUM_DATA_COLS) :: LDV_VMTSCHED

    REAL        y2_indices(NUM_DATA_COLS)
    INTEGER*2   m2, r2, a2, f2, v2

!   Parameters for csv read-in    
    CHARACTER(LEN=MAX_LINE_BUFFER_LENGTH) :: line_buffer
    INTEGER :: status, current_row_idx

!...Read in sales and attribute data (trnfem) from csv  

!   Open the file
    FNAME = 'TRNLDVVMT'
    WKUNIT = FILE_MGR('O',FNAME,NEW)
    
!   -- Read file line by line --
!   Read and drop the header
    READ(WKUNIT, '(A)', IOSTAT=status) line_buffer
    
!   Read the data    
    current_row_idx = 0
    DO i = 1, NUM_ROWS 
      current_row_idx = current_row_idx + 1
      
      ! Read an entire line into the buffer
      READ(WKUNIT, '(A)', IOSTAT=status) line_buffer
      IF (status /= 0) THEN
        PRINT *, "Error reading line ", current_row_idx, " or unexpected EOF. IOSTAT=", status
        STOP
      END IF
      
      ! Now parse the line_buffer using an internal read (assumes commas are delimiters)
      READ(line_buffer, *, IOSTAT=status) &
           col1_ivtyp(current_row_idx), &
           col2_iregn(current_row_idx), &
           col3_ildv(current_row_idx), &
           col4_iage(current_row_idx), &
           (LDV_VMTSCHED(current_row_idx,j), j=1, NUM_DATA_COLS)
          
      IF (status /= 0) THEN
        PRINT *, "Error parsing line ", current_row_idx, " with data: '", TRIM(line_buffer), "' IOSTAT=", status
        STOP
      END IF
      
    ENDDO ! NUM_ROWS

    WKUNIT = FILE_MGR('C',FNAME,NEW)
    
    WRITE(*,*) "Successfully read ", NUM_ROWS, " records from ", FNAME
    
    y2_indices = (/(i+26, i = 1, NUM_DATA_COLS)/)

!   Fill the VMT array with the input data from trnldv_vmt.csv
    do m2 = 1,NUM_ROWS
      a2 = col4_iage(m2)
	  f2 = col3_ildv(m2)
	  r2 = col2_iregn(m2)
      v2 = col1_ivtyp(m2)
	  if (v2.eq.1) then
        PVMT(a2,y2_indices,r2,f2) = LDV_VMTSCHED(m2,1:NUM_DATA_COLS)
      else
        LVMT(a2,y2_indices,r2,f2) = LDV_VMTSCHED(m2,1:NUM_DATA_COLS)
      endif
    enddo     

!	Fill in VMT for powertrains that are assumed equal to gasoline ICE
!   These include: E85, diesel hybrid, cng/lpg, methanol, and h2 fuel cell

    PVMT(:,27:VMTYR-1989,:,3)  = PVMT(:,27:VMTYR-1989,:,1)
    PVMT(:,27:VMTYR-1989,:,8)  = PVMT(:,27:VMTYR-1989,:,1)
    PVMT(:,27:VMTYR-1989,:,9)  = PVMT(:,27:VMTYR-1989,:,1)
    PVMT(:,27:VMTYR-1989,:,10) = PVMT(:,27:VMTYR-1989,:,1)
    PVMT(:,27:VMTYR-1989,:,11) = PVMT(:,27:VMTYR-1989,:,1)
    PVMT(:,27:VMTYR-1989,:,12) = PVMT(:,27:VMTYR-1989,:,1)
    PVMT(:,27:VMTYR-1989,:,13) = PVMT(:,27:VMTYR-1989,:,1)
    PVMT(:,27:VMTYR-1989,:,14) = PVMT(:,27:VMTYR-1989,:,1)
    LVMT(:,27:VMTYR-1989,:,3)  = LVMT(:,27:VMTYR-1989,:,1)
    LVMT(:,27:VMTYR-1989,:,8)  = LVMT(:,27:VMTYR-1989,:,1)
    LVMT(:,27:VMTYR-1989,:,9)  = LVMT(:,27:VMTYR-1989,:,1)
    LVMT(:,27:VMTYR-1989,:,10) = LVMT(:,27:VMTYR-1989,:,1)
    LVMT(:,27:VMTYR-1989,:,11) = LVMT(:,27:VMTYR-1989,:,1)
    LVMT(:,27:VMTYR-1989,:,12) = LVMT(:,27:VMTYR-1989,:,1)
    LVMT(:,27:VMTYR-1989,:,13) = LVMT(:,27:VMTYR-1989,:,1)
    LVMT(:,27:VMTYR-1989,:,14) = LVMT(:,27:VMTYR-1989,:,1)
      
!	  Populate pre-2016 (pre-Polk VMT data) with 2016 values
	  DO icl = iy, 2015-1989
	    PVMT(:,icl,:,:) = PVMT(:,2016-1989,:,:)
	    LVMT(:,icl,:,:) = LVMT(:,2016-1989,:,:)
	  ENDDO

!	  Populate post-VMTYR (post-Polk VMT data) with VMTYR values (projected VMT schedule = last historical)
!	  Ramp up EV (ildv=15) VMT schedule to converge with a given share of the gasoline ICE schedule (VMT_MAXSHR) 
!	  by the year specified in trnldvx.xlsx (VMT_ENDYR).
	  DO icl = VMTYR-1989, IJUMPYR
	    DO ildv = 1, maxldv
		  IF (VMT_SCHED_PARAM(ildv,1).eq.0.0) THEN
	        PVMT(:,icl,:,ildv) = PVMT(:,VMTYR-1989,:,ildv)
	        LVMT(:,icl,:,ildv) = LVMT(:,VMTYR-1989,:,ildv)
		  ELSEIF (icl.le.VMT_SCHED_PARAM(ildv,2)-1989) THEN
		    DO iregn = 1, mnumcr-2
			  DO iage = 1, maxage
		        if (PVMT(iage,icl,iregn,ildv).lt.PVMT(iage,icl,iregn,1)*VMT_SCHED_PARAM(ildv,1)) then 
                  PVMT(iage,icl,iregn,ildv) = (PVMT(iage,VMTYR-1989,iregn,1)*VMT_SCHED_PARAM(ildv,1)-PVMT(iage,VMTYR-1989,iregn,ildv))* &
									            (icl+1989-VMTYR)/(VMT_SCHED_PARAM(ildv,2)-VMTYR) + PVMT(iage,VMTYR-1989,iregn,ildv)
			    else
                  PVMT(iage,icl,iregn,ildv) = PVMT(iage,VMTYR-1989,iregn,1)*VMT_SCHED_PARAM(ildv,1)
                endif
                if (LVMT(iage,icl,iregn,ildv).lt.LVMT(iage,icl,iregn,1)*VMT_SCHED_PARAM(ildv,1)) then 
                  LVMT(iage,icl,iregn,ildv) = (LVMT(iage,VMTYR-1989,iregn,1)*VMT_SCHED_PARAM(ildv,1)-LVMT(iage,VMTYR-1989,iregn,ildv))* &
									            (icl+1989-VMTYR)/(VMT_SCHED_PARAM(ildv,2)-VMTYR) + LVMT(iage,VMTYR-1989,iregn,ildv)
			    else
                  LVMT(iage,icl,iregn,ildv) = LVMT(iage,VMTYR-1989,iregn,1)*VMT_SCHED_PARAM(ildv,1)
                endif
                
                if (PVMT(iage,icl,iregn,ildv).lt.0.0) then
                  WRITE(21,'(a,",",5(i4,","),4(f8.1,","))')'PVMT',icl+1989,iregn,ildv,icl,iage,PVMT(iage,icl,iregn,ildv),&
                                                    PVMT(iage,VMTYR-1989,iregn,1),VMT_SCHED_PARAM(ildv,1),&
                                                    VMT_SCHED_PARAM(ildv,2)
                endif
                if (LVMT(iage,icl,iregn,ildv).lt.0.0) then
                  WRITE(21,'(a,",",5(i4,","),4(f8.1,","))')'LVMT',icl+1989,iregn,ildv,icl,iage,LVMT(iage,icl,iregn,ildv),&
                                                    LVMT(iage,VMTYR-1989,iregn,1),VMT_SCHED_PARAM(ildv,1),&
                                                    VMT_SCHED_PARAM(ildv,2)
                endif
                
              ENDDO
			ENDDO
          ELSE
            PVMT(1:maxage,icl,1:mnumcr-2,ildv) = PVMT(1:maxage,icl-1,1:mnumcr-2,ildv)
            LVMT(1:maxage,icl,1:mnumcr-2,ildv) = LVMT(1:maxage,icl-1,1:mnumcr-2,ildv)
		  ENDIF
		ENDDO
	  ENDDO

!...calculate US household [unweighted] average PVMT and LVMT to prevent 0.0 values where no stock exists
!...stock weighted average calculated later
	pvmt(1:maxage,iy:ijumpyr,mnumcr,1:maxldv) = sum(pvmt(1:maxage,iy:ijumpyr,1:mnumcr-2,1:maxldv),dim=3)/9.0
    lvmt(1:maxage,iy:ijumpyr,mnumcr,1:maxldv) = sum(lvmt(1:maxage,iy:ijumpyr,1:mnumcr-2,1:maxldv),dim=3)/9.0
  
  RETURN
  END SUBROUTINE READVMT 

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
! Uncomment if wanting to debug input file (also uncomment later "close" xmlout statement)
!     common/nemswk1/xmlout
!     integer xmlout
!     call unitunopened(300,999,xmlout)  ! get unused unit number
!     open(xmlout,file='TDM_trnldvx.txt',status='unknown')
!     rewind xmlout
! end of debug output set up

!...Store data ranges from xml file

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
	CALL GETRNGI('ENFORCE_EPA     ',ENFORCE_EPA,1,1,1)
	CALL GETRNGI('ENFORCE_MY27REGS',ENFORCE_MY27REGS,1,1,1)
	CALL GETRNGI('ENFORCE_CAFE    ',ENFORCE_CAFE,1,1,1)
	CALL GETRNGR('CREDBAL_EPALYR  ',CREDBAL_EPALYR,1,1,1)
	CALL GETRNGR('LDV_VEHLIFEMI   ',LDV_VEHLIFEMI,1,maxvtyp,1)
	CALL GETRNGR('AC_OC_CREDIT    ',AC_OC_CREDIT(1:MAXGROUP,iy:ijumpyr),MAXGROUP,num_to_read,1)
	CALL GETRNGR('PU_CRED_ELIG    ',PU_CRED_ELIG,1,MAXGROUP,1)


!...Scedes switch overrides trnldvx setting for MY27+ standards
    if (TRANEFF.eq.3) ENFORCE_MY27REGS = 0

!...Freeze standards at MY2026 if ENFORCE_MY27REGS turned off
!   Change ac/oc credits since maximum no longer phases out
    if (ENFORCE_MY27REGS.eq.0) then
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
    endif

!   These array spreads allow fuel economy calculations to be vectorized in CAFECALC.
    ac_oc_credit_expanded = SPREAD( &
                              SPREAD(ac_oc_credit(1:MAXGROUP,1:MNUMYR), &
                                     DIM=2,                             &
                                     NCOPIES=MAXCLASS),                 &
                              DIM=3,                                    &
                              NCOPIES=MAXLDV)                            
                              
    ac_oc_credit_expanded_adj = ac_oc_credit_expanded
    ac_oc_credit_expanded_adj(1:maxgroup,1:maxclass,[4,7,15],1:MNUMYR) = 0.0
    cafepefmult_expanded  = SPREAD( &
                              SPREAD(cafepefmult(1:MAXLDV,1:MNUMYR),    &
                                     DIM=1,                             &
                                     NCOPIES=MAXGROUP),                 &
                              DIM=2,                                    &
                              NCOPIES=MAXCLASS)                          


!   Pre-2012 footprint curves
    CFCoefA(19:ijumpyr)=CFCOEFNRG(1,19:ijumpyr)
    CFCoefB(19:ijumpyr)=CFCOEFNRG(2,19:ijumpyr)
    CFCoefC(19:ijumpyr)=CFCOEFNRG(3,19:ijumpyr)
    CFCoefD(19:ijumpyr)=CFCOEFNRG(4,19:ijumpyr)
    TFCoefA(19:ijumpyr)=TFCOEFNRG(1,19:ijumpyr)
    TFCoefB(19:ijumpyr)=TFCOEFNRG(2,19:ijumpyr)
    TFCoefC(19:ijumpyr)=TFCOEFNRG(3,19:ijumpyr)
    TFCoefD(19:ijumpyr)=TFCOEFNRG(4,19:ijumpyr)

!   2012+ footprint curves
!   NHTSA CAFE
    CFCoefA2(23:ijumpyr)=CFCOEFCAFE(1,23:ijumpyr)
    CFCoefB2(23:ijumpyr)=CFCOEFCAFE(2,23:ijumpyr)
    CFCoefC2(23:ijumpyr)=CFCOEFCAFE(3,23:ijumpyr)
    CFCoefD2(23:ijumpyr)=CFCOEFCAFE(4,23:ijumpyr)
    TFCoefA2(23:ijumpyr)=TFCOEFCAFE(1,23:ijumpyr)
    TFCoefB2(23:ijumpyr)=TFCOEFCAFE(2,23:ijumpyr)
    TFCoefC2(23:ijumpyr)=TFCOEFCAFE(3,23:ijumpyr)
    TFCoefD2(23:ijumpyr)=TFCOEFCAFE(4,23:ijumpyr)
    TFCoefE2(23:ijumpyr)=TFCOEFCAFE(5,23:ijumpyr)	
    TFCoefF2(23:ijumpyr)=TFCOEFCAFE(6,23:ijumpyr)	
    TFCoefG2(23:ijumpyr)=TFCOEFCAFE(7,23:ijumpyr)	
    TFCoefH2(23:ijumpyr)=TFCOEFCAFE(8,23:ijumpyr)	
!   EPA GHG
    CFCoefEPAA2(23:ijumpyr)=CFCOEFEPA(1,23:ijumpyr)
    CFCoefEPAB2(23:ijumpyr)=CFCOEFEPA(2,23:ijumpyr)
    CFCoefEPAC2(23:ijumpyr)=CFCOEFEPA(3,23:ijumpyr)
    CFCoefEPAD2(23:ijumpyr)=CFCOEFEPA(4,23:ijumpyr)
    TFCoefEPAA2(23:ijumpyr)=TFCOEFEPA(1,23:ijumpyr)
    TFCoefEPAB2(23:ijumpyr)=TFCOEFEPA(2,23:ijumpyr)
    TFCoefEPAC2(23:ijumpyr)=TFCOEFEPA(3,23:ijumpyr)
    TFCoefEPAD2(23:ijumpyr)=TFCOEFEPA(4,23:ijumpyr)
    TFCoefEPAE2(23:ijumpyr)=TFCOEFEPA(5,23:ijumpyr)
    TFCoefEPAF2(23:ijumpyr)=TFCOEFEPA(6,23:ijumpyr)
    TFCoefEPAG2(23:ijumpyr)=TFCOEFEPA(7,23:ijumpyr)
    TFCoefEPAH2(23:ijumpyr)=TFCOEFEPA(8,23:ijumpyr)

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

!   If not applying MY2027-My2032 CAFE/GHG standards, read in alternate GRPFLAG and introduce
!   more hybrids (where PHEVs already exist)
    if (ENFORCE_MY27REGS.eq.0) then
      CALL GETRNGR('SALES_PER_MDL_NC',SALES_PER_MODEL,MAXCLASS,MAXGROUP,1)
      CALL GETRNGI('GRPFLAG_ALT     ',GRPFLAG(1:MAXLDV,1:MAXCLASS,1:MAXGROUP),MAXLDV,MAXCLASS,MAXGROUP) 
      do igp=1,maxgroup
        do icl=1,maxclass
          if(GRPFLAG(16,icl,igp).eq.0.and.ANY(GRPFLAG([5,6],icl,igp).gt.0)) GRPFLAG(16,icl,igp) = MINVAL(GRPFLAG([5,6],icl,igp))
        enddo
      enddo
    endif
        
! ... Read in and store for global reference learning cost curves for batteries and fuel cells
      CALL GETRNGR('NIMHCOST        ',NIMHCOST(iy:IJUMPYR),Num_to_Read,1,1) 
      CALL GETRNGR('LIONCOST        ',LIONCOST(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('LIONCOST_PHEV   ',LIONCOST_PHEV(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('batt_dmd_nUSLDV ',global_batt_prod(1,2,iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('batt_dmd_nUSnLDV',global_batt_prod(2,2,iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('batt_dmd_cons   ',global_batt_prod(6,2,iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('PHEV_DOD_FAC    ',PHEV_DOD_FAC(iy:IJUMPYR),Num_to_read,1,1)
	  CALL GETRNGR('CSRATIO         ',CSRATIO(1:maxclass,1:maxgroup,1:phevtype),maxclass,maxgroup,phevtype)      
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
      mat_markup(first_read_year-1989:2021-1989) = 1.0
	  
      FuelCell_D_kW(first_read_year:IJUMPCALYR,13)   = CELLMCST(first_read_year-1989:IJUMPCALYR-1989)
      FuelCell_D_kW(first_read_year:IJUMPCALYR,14)   = CELLHCST(first_read_year-1989:IJUMPCALYR-1989)
      NiMH_Cost(first_read_year:IJUMPCALYR)          = NIMHCOST(first_read_year-1989:IJUMPCALYR-1989) / MC_JPGDP(11) * MC_JPGDP(1)      ! convert from 2000$ to 1990$
      PHEV_DOD(first_read_year:IJUMPCALYR)           = PHEV_DOD_FAC(first_read_year-1989:IJUMPCALYR-1989)
      EV_DOD(first_read_year:IJUMPCALYR)             = EV_DOD_FAC(first_read_year-1989:IJUMPCALYR-1989)

      do iyr = first_read_year,IJUMPCALYR
        Li_ion_Cost(1:maxldv,iyr)  =  LIONCOST(IYR-1989) / MC_JPGDP(31) * MC_JPGDP(1)       ! convert from 2020$ to 1990$
		Li_ion_Cost([5,6,16],iyr)  =  LIONCOST_PHEV(IYR-1989) / MC_JPGDP(31) * MC_JPGDP(1)  ! convert from 2020$ to 1990$
		
        ElecNonBattCst(1:maxclass,iyr,1:maxvtyp,4)  = EVSYSCST(1:maxclass,IYR-1989,1:maxvtyp) / MC_JPGDP(11) * MC_JPGDP(1)     	! convert from 2000$ to 1990$
        ElecNonBattCst(1:maxclass,iyr,1:maxvtyp,5)  = PHEV20SYSCST(1:maxclass,IYR-1989,1:maxvtyp) / MC_JPGDP(11) * MC_JPGDP(1)    ! convert from 2000$ to 1990$
        ElecNonBattCst(1:maxclass,iyr,1:maxvtyp,6)  = PHEV50SYSCST(1:maxclass,IYR-1989,1:maxvtyp) / MC_JPGDP(11) * MC_JPGDP(1)    ! convert from 2000$ to 1990$
        ElecNonBattCst(1:maxclass,iyr,1:maxvtyp,7)  = EVSYSCST(1:maxclass,IYR-1989,1:maxvtyp) / MC_JPGDP(11) * MC_JPGDP(1)     	! convert from 2000$ to 1990$
        ElecNonBattCst(1:maxclass,iyr,1:maxvtyp,15) = EVSYSCST(1:maxclass,IYR-1989,1:maxvtyp) / MC_JPGDP(11) * MC_JPGDP(1)     	! convert from 2000$ to 1990$
        ElecNonBattCst(1:maxclass,iyr,1:maxvtyp,16) = HEVSYSCST(1:maxclass,IYR-1989,1:maxvtyp) / MC_JPGDP(11) * MC_JPGDP(1)     	! convert from 2000$ to 1990$  
      enddo

! ... Calculate Li-ion cost learning curve parameters (only for BEV, PHEV, HEV, and FCV)
      pack_a([4:8,13:maxldv]) = packa([4:8,13:maxldv]) / MC_JPGDP(31) * MC_JPGDP(1)	  	! convert from 2020$ to 1990$
      mat_a([4:8,13:maxldv])  = mata([4:8,13:maxldv]) / MC_JPGDP(31) * MC_JPGDP(1)	  		! convert from 2020$ to 1990$
      pack_b([4:8,13:maxldv]) = -LOG(1.0-packlr([4:8,13:maxldv])) / LOG(2.0)
      mat_b([4:8,13:maxldv])  = -LOG(1.0-matlr([4:8,13:maxldv])) / LOG(2.0)

! ... Read and load state and federal purchase incentives 
	  CALL GETRNGR('STATE_CRED      ',STATE_CRED(1:MNUMCR-2,2023:LYR,1:3),MNUMCR-2,LYR-2023+1,3)
	  CALL GETRNGR('IRA_BAT_SHR     ',IRA_BAT_SHR(1:2,irayr:lyr,1:2),2,lyr-irayr+1,2)
	  CALL GETRNGR('IRA_VEH_SHR     ',IRA_VEH_SHR(1:2,irayr:lyr,1:2),2,lyr-irayr+1,2)
	  CALL GETRNGI('SwitchLDV_HR1   ',SwitchLDV_HR1,1,1,1)

!...Scedes switch overrides trnldvx setting for HR1 and for enforcing CAFE (if HR1, then no CAFE enforcing due to no fines)
    if (TRANEFF.eq.4) SwitchLDV_HR1 = 0
    if(SwitchLDV_HR1.eq.0) ENFORCE_CAFE = 1

! ... Read and load nested multinomial logit model coefficients
	  CALL GETRNGR('NMLMCOCAR       ',nmlmco(1:MAXNMLM,1:MAXCLASS,1:CARGRP),MAXNMLM,MAXCLASS,CARGRP) !consumer choice model coefficients
	  CALL GETRNGR('NMLMCOTRK       ',nmlmco(1:MAXNMLM,1:MAXCLASS,LTKGRP:MAXGROUP),MAXNMLM,MAXCLASS,LTKGRP) !consumer choice model coefficients
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

! ... Set battery replacement cost to zero to nullify its impact on
! ... market penetrations.  If/when battery replacement costs are extracted from
! ... overall maintenance costs, this initialization should be removed in favor
! ... of a more appropriate alternative.
      BRCOST25(1:maxgroup, 1:maxldv, 1:maxclass, :) = 0.0
	  
! ... Read in and store for global reference various Advanced Technology Vehicle parameters
! ... read initial light duty vehicle attributes for consumer choice model

      CALL GETRNGR('GAS_PUMP_PER_STA',GAS_PUMP_PER_STA,1,1,1)
      CALL GETRNGR('GAS_HRLY_THRUPUT',GAS_HRLY_THRUPUT,1,1,1)
      CALL GETRNGR('INITSTA         ',INITSTA(1:maxfuel,iy:IJUMPYR,1:mnumcr-2), maxfuel,Num_to_Read,MNUMCR-2)  ! refueling stations by fuel type and reg
      CALL GETRNGR('STA_RAT         ',STA_RAT, maxfuel,1,1)                 ! ratio of fueling stations to vehicle stock
      CALL GETRNGI('CHR_STR_YR      ',CHR_STR_YR,1,1,1)                     ! first year charger data
      CALL GETRNGI('CHR_LST_YR      ',CHR_LST_YR,1,1,1)                     ! last year charger data
      CALL GETRNGR('ELAS_FAVL       ',ELAS_FAVL,1,1,1)                     ! last year charger data      
      CALL GETRNGR('PRT_CNT         ',PRT_CNT(1:MAXCHRG,CHR_STR_YR:CHR_LST_YR, 1:MNUMCR-2), MAXCHRG,CHR_LST_YR-CHR_STR_YR+1,MNUMCR-2)
      CALL GETRNGR('PRT_CNT_nc      ',PRT_CNT_nc(1:MAXCHRG,CHR_STR_YR:CHR_LST_YR, 1:MNUMCR-2), MAXCHRG,CHR_LST_YR-CHR_STR_YR+1,MNUMCR-2)
      CALL GETRNGR('PRT_RT          ',PRT_RT,  MAXCHRG, 1, 1)               ! ev time to refuel by type
      CALL GETRNGR('CHGCSTMULT      ',CHGCSTMULT,  MAXCHRG, 1, 1)           ! Markup on comm'l electricity cost (represents what folks actually pay to charge)
      CALL GETRNGR('CHG_DIST        ',CHG_DIST(1:mnumcr-2,1:MAXCHRG,2023-1989),  MNUMCR-2, MAXCHRG, 1)             ! Distribution of charging by location/speed
      CALL GETRNGR('MAINTGRP        ',MAINTGRP(1:MAXLDV,1:MAXCLASS,1:maxvtyp),maxldv,maxclass,maxvtyp)
      
      if (ENFORCE_MY27REGS.eq.0) PRT_CNT = PRT_CNT_nc
            
      do iregn=1,mnumcr-2
        do ichrg=1,maxchrg
          chg_dist(iregn,ichrg,1:2022-1989) = chg_dist(iregn,ichrg,2023-1989)
        enddo
      enddo

! ... read light duty vehicle fleet input variables
! ...   Note: indexed by fleet type: 1) business  2) government  3) utility 4) taxi  
! ...         and vehicle type:      1) car       2) light truck              
      CALL GETRNGR('FLTTRANSPC      ',FLTTRANS(1:MAXFLEET,1:MAXAGE,1),MAXFLEET,MAXAGE,1)    ! fraction of fleet passenger cars transfered to households
      CALL GETRNGR('FLTTRANSLT      ',FLTTRANS(1:MAXFLEET,1:MAXAGE,2),MAXFLEET,MAXAGE,1)    ! fraction of fleet passenger cars transfered to households
	  CALL GETRNGR('SURVFLT         ',SURVFLT,MAXFLEET,MaxAge,MAXVTYP)
      CALL GETRNGR('FLTVMTYR        ',FLTVMTYR(1:MAXFLEET,iy:IJUMPYR,1:maxvtyp),MAXFLEET, Num_to_Read,MAXVTYP)  ! VMT per vehicle by fleet type
	  
! ... *******************************************************************************
! ... * Light Duty Vehicle Stock Module input variables                             *
! ... *******************************************************************************
! ... * LDV Stock Accounting Model                                                  *
! ... *******************************************************************************

      CALL GETRNGR('SURV25          ',SURV25,      MNUMCR,MaxAge,MaxVtyp)
	  CALL GETRNGR('SURV_ADJ        ',SURV_ADJ    (iy:IJUMPYR),Num_to_Read,1,1)

!...  Calculate 25 vintage survival curves for cars and light trucks
!	  Convert cumulative survival (from input file) into annual survival rates
	  SSURV25(1:mnumcr,1,1:maxvtyp) = SURV25(1:mnumcr,1,1:maxvtyp)
      SSURV25(1:mnumcr,2:maxage,1:maxvtyp) = SURV25(1:mnumcr,2:maxage,1:maxvtyp)/SURV25(1:mnumcr,1:maxage-1,1:maxvtyp)
	
      CALL GETRNGR('CMPGSTKGAS95    ',CMPGSTKGAS95,MAXVTYP,MAXAGE,1)         
      CALL GETRNGR('STKAVGWGT       ',STKAVGWGT,   MAXVTYP,MAXAGE,1)
      CALL GETRNGR('TRWTCAR_HIST    ',TRWTCAR_HIST(IY:IJUMPYR),Num_to_Read,1,1)
      CALL GETRNGR('TRWTTRK_HIST    ',TRWTTRK_HIST(IY:IJUMPYR),Num_to_Read,1,1)
      
      trwtcar_stock(1:16) = trwtcar_hist(1:16)
      trwttrk_stock(1:16) = trwttrk_hist(1:16)
      
	  CALL GETRNGR('VMT_SCHED_PARAM ',VMT_SCHED_PARAM(1:MAXLDV,1:2),MAXLDV,2,1)

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
      CALL GETRNGR('CREDDSHR        ',CREDDSHR,  MNUMCR-2,1,1)
      CALL GETRNGR('CRRPMHIST       ',CRRPMHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('CREFFHIST       ',CREFFHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('CREDDHIST       ',CREDDHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('CREDEHIST       ',CREDEHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('CR_CAV_ADJ      ',CR_CAV_ADJ(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGI('IRHISTYEAR      ',IRHISTYEAR,1,1,1)
      CALL GETRNGR('IRRPMHIST       ',IRRPMHIST (iy:IJUMPYR),Num_to_Read,1,1)
      CALL GETRNGR('IREDDHIST       ',IREDDHIST (iy:IJUMPYR),Num_to_Read,1,1)
      CALL GETRNGR('IREDEHIST       ',IREDEHIST (iy:IJUMPYR),Num_to_Read,1,1)
      CALL GETRNGR('IRREGSHR        ',IRREGSHR,  MNUMCR-2,1,1)
      CALL GETRNGR('RECFDH          ',RECFDH    (1:2,IY:IJUMPYR),2,Num_to_read,1)
      CALL GETRNGI('RBHISTYR        ',RBHISTYR,  1,1,1)
      CALL GETRNGR('RBBOAT_COEF     ',RBBOAT_COEF,2,1,1)
      CALL GETRNGR('RBMG_COEF       ',RBMG_COEF,2,1,1)
      CALL GETRNGR('RBDS_COEF       ',RBDS_COEF,1,1,1)
      CALL GETRNGR('LUBFDH          ',LUBFDH    (iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGI('LUBHISTYR       ',LUBHISTYR, 1,1,1)
      CALL GETRNGR('Cyc_RPM         ',Cyc_RPM    (iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('Cyc_MPG         ',Cyc_MPG    (iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGI('CycHistYR       ',CycHistYR, 1,1,1)
      CALL GETRNGR('TBPMTHIST       ',TBPMTHIST (1:MNUMCR-2,IY:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TBCOVID         ',TBCOVID   (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TB_COEF         ',TB_COEF   (1:MNUMCR-2,1:4),MNUMCR-2,4,1)
      CALL GETRNGI('TBHISTYEAR      ',TBHISTYEAR,1,1,1)
      CALL GETRNGR('TBPMTPC08       ',TBPMTPC08, 9,1,1)
      CALL GETRNGR('TBBTUPMHIST     ',TBBTUPMHIST(1:MNUMCR-2,IY:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TBSYSEFF        ',TBSYSEFF,  9,1,1)
      CALL GETRNGR('TB_CAV_ADJ      ',TB_CAV_ADJ(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('TBFSHRHIST      ',TBFSHRHIST(1:8,IY:IJUMPYR,1:MNUMCR-2),8,Num_to_Read,MNUMCR-2) 
      CALL GETRNGR('TMODINIT        ',TMODINIT  (1:2,IY:IJUMPYR),2,Num_to_Read,1) !ccl
      CALL GETRNGR('TMCOVID         ',TMCOVID   (1:2,IY:IJUMPYR),2,Num_to_Read,1)
      CALL GETRNGR('TMEFFINIT       ',TMEFFINIT (1:2,IY:IJUMPYR),2,Num_to_Read, 1)  !ccl
      CALL GETRNGR('QMODFSHRH       ',QMODFSHRH (1:8,IY:IJUMPYR,1:2),8,Num_to_Read, 2) !ccl 
      CALL GETRNGR('SCHBUS_SHR      ',SCHBUS_SHR,MNUMCR-2,1,1)
      CALL GETRNGR('EFF_ADJ         ',EFF_ADJ,   8,1,1)
      CALL GETRNGR('SCHBUS_EV_SHR   ',SCHBUS_EV_SHR(1:MNUMCR-2,IY:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('SBCOEFF         ',SBCOEFF,2,1,1)                        ! School bus travel demand equation coefficients      
      CALL GETRNGI('IBSBHISTYEAR    ',IBSBHISTYEAR,1,1,1)  
      CALL GETRNGR('IBCOEFF         ',IBCOEFF,2,1,1)                        ! Intercity bus travel demand equation coefficients
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

!     If running AltTrnp case (no Phase 3 EPA HD GHG reg), read in lower electrification transit and school bus shares
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
            MKT_PEN(IGP,ICL,ITECH,BASE,GAS) = TECHMKTSHARE(K,2*ICL-1) * 0.01
            MKT_MAX(ICL,IGP,ITECH,GAS)      = TECHMKTSHARE(K,2*ICL)   * 0.01

            INPUT_ERROR = 0

            IF (MKT_PEN(IGP,ICL,ITECH,BASE,GAS) .LT. 0.0-ROUNDOFF_ERROR) THEN
              INPUT_ERROR = 1
            ENDIF

            IF (INPUT_ERROR .EQ. 0 .AND. MKT_MAX(ICL,IGP,ITECH,GAS) .GT. 1.0+ROUNDOFF_ERROR) THEN
              INPUT_ERROR = 3
            ENDIF

            IF (INPUT_ERROR .EQ. 0 .AND. MKT_MAX(ICL,IGP,ITECH,GAS) .LT. MKT_PEN(IGP,ICL,ITECH,BASE,GAS)) THEN
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
              WRITE (*,*) 'Base Penetration = ',MKT_PEN(IGP,ICL,ITECH,BASE,GAS)
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

      CALL GETRNGI('ENGNOTETYPE     ',ENGNOTETYPE      ,1,MAXNOTE,1)      ! Engineering Note Type
      CALL GETRNGI('ENGNOTETECH01   ',ENGNOTETECH(1,1) ,1,MAXNOTE,1)      ! Engineering Note Technology ID #1
      CALL GETRNGI('ENGNOTETECH02   ',ENGNOTETECH(1,2) ,1,MAXNOTE,1)      ! Engineering Note Technology ID #2
      CALL GETRNGI('ENGNOTEYEAR     ',ENGNOTEYEAR      ,1,MAXNOTE,1)      ! Engineering Note Year
      CALL GETRNGR('ENGNOTEPCT      ',ENGNOTEPCT       ,1,MAXNOTE,1)      ! Engineering Note Percent Affected
      CALL GETRNGI('ENGNOTETECH03   ',ENGNOTETECH(1,3) ,1,MAXNOTE,1)      ! Engineering Note Technology ID #3
      CALL GETRNGI('ENGNOTETECH04   ',ENGNOTETECH(1,4) ,1,MAXNOTE,1)      ! Engineering Note Technology ID #4
      CALL GETRNGI('ENGNOTETECH05   ',ENGNOTETECH(1,5) ,1,MAXNOTE,1)      ! Engineering Note Technology ID #5
      CALL GETRNGI('ENGNOTETECH06   ',ENGNOTETECH(1,6) ,1,MAXNOTE,1)      ! Engineering Note Technology ID #6
      CALL GETRNGI('ENGNOTETECH07   ',ENGNOTETECH(1,7) ,1,MAXNOTE,1)      ! Engineering Note Technology ID #7
      CALL GETRNGI('ENGNOTETECH08   ',ENGNOTETECH(1,8) ,1,MAXNOTE,1)      ! Engineering Note Technology ID #8
      CALL GETRNGI('ENGNOTETECH09   ',ENGNOTETECH(1,9) ,1,MAXNOTE,1)      ! Engineering Note Technology ID #9
      CALL GETRNGI('ENGNOTETECH10   ',ENGNOTETECH(1,10),1,MAXNOTE,1)      ! Engineering Note Technology ID #10

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
            SIGN_TDM = -1
            MAND_ORIDE(NUM_MAN) = .TRUE.
          ELSE
            SIGN_TDM = 1
            MAND_ORIDE(NUM_MAN) = .FALSE.
          ENDIF
          MANDYEAR(1,NUM_MAN) = ENGNOTETECH(I,1)
          MANDYEAR(2,NUM_MAN) = ENGNOTEYEAR(I)
          MANDYEAR(3,NUM_MAN) = ENGNOTETECH(I,2) * SIGN_TDM
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
      CALL GETRNGR('taxi_rev_params ',taxi_rev_params,maxhav,6,1)	                            ! parameters for taxi revenue calculation
      taxi_mi_life(1:maxhav)    = taxi_rev_params(1:maxhav,1)									! taxi lifetime miles
      taxi_idle_gph(1:maxhav)   = taxi_rev_params(1:maxhav,2)									! taxi idle fuel rate, gallons/hr
      taxi_maint_cost(1:maxhav) = taxi_rev_params(1:maxhav,3) / MC_JPGDP(26) * MC_JPGDP(1)	    ! taxi maintenance costs per month (independent of mileage), input in 2015$; converted to 1990$
      taxi_data_fee(1:maxhav)   = taxi_rev_params(1:maxhav,4) / MC_JPGDP(26) * MC_JPGDP(1)		! HAV data fee per month, input in 2015$; converted to 1990$
      taxi_insur(1:maxhav)      = taxi_rev_params(1:maxhav,5) / MC_JPGDP(26) * MC_JPGDP(1)		! taxi insurance fee per month, input in 2015$; converted to 1990$

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
!      close(xmlout)
!      xmlout=0
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
!		LDVPRI(IVTYP,ILDV,ICL,yrs)
!		LDVMPG(IVTYP,ILDV,ICL,yrs)
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
!       LDVPRI(IVTYP,ILDV,ICL,yrs), LDVMPG(IVTYP,ILDV,ICL,yrs)
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
!   temporary local variables for vehicle attributes - remove after ihav redimensioned variables are available
	REAL :: Vehprice(maxvtyp,MAXCLASS,BYR:LYR,maxldv,maxhav)
	REAL :: mpgtemp(maxvtyp,MAXCLASS,BYR:LYR,maxldv,maxhav)
    
!   For debug writes 
    LOGICAL :: hav_debug = .false.
    integer it,ix
	real ttot
	real tshr(maxhav)

!...Initialize shares for this year. If this subroutine is aborted due to any error, HAV shares will remain at 0%
	flt_hav_shares(:,:,:,:,yrs,:) = 0.0
	flt_hav_shares(:,:,:,:,yrs,1) = 1.0

!...Set availability flag for this year
!...first set ILDV available year within class	
    havatvflag(:,:,:) = .false.
	do ILDV = 1, maxldv
		do ICL = 1, MAXCLASS
            if(ildv.eq.1.or.ildv.eq.15.or.ildv.eq.16) then
              havatvflag(1,ICL,ILDV) = ANY(CLASSFLAG(1:cargrp,ICL,ILDV))
			  havatvflag(2,ICL,ILDV) = ANY(CLASSFLAG(ltkgrp:maxgroup,ICL,ILDV))
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
					Vehprice(IVTYP,ICL,yrs,ILDV,ihav) = LDVPRI(IVTYP,ILDV,ICL,yrs) + hav_sys_cost(ihav, yrs)
					mpgtemp(IVTYP,ICL,yrs,ILDV,ihav) = LDVMPG(IVTYP,ILDV,ICL,yrs)*hav_mpgdeg(ihav, yrs)
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


    if(n.eq.lastyr.and.fcrl.eq.1.and.hav_debug) then
      write(21, '(/,a)') 'LIDARCOSTCALC output - Lidar cost reduction over time:'
      write(21,'(2x,a,36f8.4)') lidar_cost(:, :)
	  write(21,*) 'lidar phase:'
      do i=1995, 2050
          write(21,*) i, lidar_phase(1,i), lidar_phase(2,i)
      enddo
      do i=1995, 2050
          write(21,*) i, ' cost:', lidar_cost(1,i), lidar_cost(2,i), lidar_cost(3,i), lidar_cost(4,i)
      enddo
	  
	  write(21, *) 'Low and High speed production:'
	  do i=1995, 2050
          write(21,*) i, ': ', cumul_lidar_prod(1,i),cumul_lidar_prod(2,i)
      enddo
      
	  write(21, *) 'HAV System Cost:'
      do i=1995, 2050
          write(21, '(i4, 4F8.0)') i, hav_sys_cost(:,i)
      enddo
!	  
!     flt_hav_shares(mnumcr, maxvtyp, MAXCLASS, maxldv, BYR:LYR,,maxhav)
!     taxi_sales(mnumcr,maxvtyp,MAXCLASS,maxldv,BYR:LYR,maxhav)
	  write (21,*) 'Fleet HAV adoption in taxis for all CD, type, class, and ILDV:'
	  write (21,*) 'Total national sales of HAV levels, by year'
	  do i=2020, 2050
	  	! National sales by HAV level (all CD, type, class, ILDV summed)
	  	do ihav=1, maxhav
	  		tshr(ihav) = sum(taxi_sales(mnumcr,:,:,:,i,ihav))				  ! National sales by HAV level
	  	enddo
	  	write (21,'(i4, 2x, 4f8.0)') i, tshr(:)
	  enddo
	  write (21,*) 'Total national shares of HAV levels, by year'
	  do i=2020, 2050
	  	ttot = sum(taxi_sales(mnumcr,:,:,:,i,:))							  ! National sales, all HAV levels summed
	  !...HAV national sales share
	  	do ihav=1, maxhav
	  		tshr(ihav) = sum(taxi_sales(mnumcr,:,:,:,i,ihav))/ttot				  ! National sales share by HAV level
	  	enddo
!	  	write (21,'(i4, 2x, 4f8.4)') i, tshr(:)
	  enddo
!	  
!	  write (21,*) 'Fleet HAV adoption for cars (IVTYP=1), class 4, gasoline (ILDV=1):'
!	  write (21,*) 'Shares of HAV levels, by year'
!	  do iregn = 1, mnumcr-2
!	  	write (21,*) '>> CD ', iregn
!	  	write (21,*) '    shares'
!	  	do i=1995, 2050
!	  		write (21,'(i4, 5f10.4)') i, flt_hav_shares(iregn,1,4,1,i,:), sum(flt_hav_shares(iregn,1,4,1,i,:))
!	  	enddo
!	  	write (21,*) '    sales'
!	  	do i=1995, 2050
!	  		write (21,'(i4, 5f10.0)') i, taxi_sales(iregn,1,4,1,i,:), sum(taxi_sales(iregn,1,4,1,i,:))
!	  	enddo
!	  enddo
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
    endif
    
    RETURN
	END SUBROUTINE FLTHAV
!	  	  

! ==========================================================================================================
! ... Subroutine READSTOCK reads the input file TRNSTOCK_LDV.CSV
! ==========================================================================================================
   SUBROUTINE READSTOCK
   USE T_
   IMPLICIT NONE

!...Declare local parameters

    LOGICAL       NEW/.FALSE./
    CHARACTER*18  INAME
    INTEGER       WKUNIT,out78
    INTEGER*2     m2, r2, a2, f2, fl2, j2, y2, v2
    REAL          y2_indices(numstkyrs)

    INTEGER, PARAMETER      :: NUM_ROWS = (MAXOWNER-1)*maxvtyp*maxldv*maxage*(mnumcr-2) + &
                               maxvtyp*maxldv*3*(mnumcr-2)                                       ! Note: Business fleet only has 3 vintages
    integer, PARAMETER      :: NUM_IND_COLS = 5
    INTEGER, PARAMETER      :: NUM_DATA_COLS = numstkyrs
    INTEGER, PARAMETER      :: TOTAL_COLS = NUM_IND_COLS + NUM_DATA_COLS
    INTEGER, PARAMETER      :: MAX_LINE_BUFFER_LENGTH = 20 * TOTAL_COLS 
    
    INTEGER, ALLOCATABLE, DIMENSION(:) :: col1_iown
    INTEGER, ALLOCATABLE, DIMENSION(:) :: col2_ivtyp
    INTEGER, ALLOCATABLE, DIMENSION(:) :: col3_iregn
    INTEGER, ALLOCATABLE, DIMENSION(:) :: col4_iage
    INTEGER, ALLOCATABLE, DIMENSION(:) :: col5_ildv
    REAL, ALLOCATABLE, DIMENSION(:,:)  :: LDVSTKNFREGN
    
    CHARACTER(LEN=MAX_LINE_BUFFER_LENGTH) :: line_buffer
    INTEGER :: status, current_row_idx

!...Read in LDV stock data from csv  
!   Allocate all temp data arrays
    ALLOCATE(col1_iown(NUM_ROWS), &
             col2_ivtyp(NUM_ROWS), &
             col3_iregn(NUM_ROWS), &
             col4_iage(NUM_ROWS), &
             col5_ildv(NUM_ROWS), &
             LDVSTKNFREGN(NUM_ROWS,NUM_DATA_COLS))

!   Open the file
    INAME = 'TRNSTOCKX_LDV'
    WKUNIT = FILE_MGR('O',INAME,NEW)
    
!   -- Read data line by line  --
!   Read and drop the header
    READ(WKUNIT, '(A)', IOSTAT=status) line_buffer

!   Read the data    
    current_row_idx = 0
    DO i = 1, NUM_ROWS 
      current_row_idx = current_row_idx + 1
      
      ! Read an entire line into the buffer
      READ(WKUNIT, '(A)', IOSTAT=status) line_buffer
      IF (status /= 0) THEN
        PRINT *, "Error reading line ", current_row_idx, " or unexpected EOF. IOSTAT=", status
        STOP
      END IF
      
      ! Now parse the line_buffer using an internal read (assumes commas are delimiters)
      READ(line_buffer, *, IOSTAT=status) &
           col1_iown(current_row_idx), &
           col2_ivtyp(current_row_idx), &
           col3_iregn(current_row_idx), &
           col4_iage(current_row_idx), &
           col5_ildv(current_row_idx), &
           (LDVSTKNFREGN(current_row_idx,j), j=1, NUM_DATA_COLS)
          
      IF (status /= 0) THEN
        PRINT *, "Error parsing line ", current_row_idx, " with data: '", TRIM(line_buffer), "' IOSTAT=", status
        STOP
      END IF
      
    ENDDO ! NUM_ROWS
    
    WKUNIT = FILE_MGR('C',INAME,NEW)
    
    WRITE(*,*) "Successfully read ", NUM_ROWS, " records from ", INAME

    LDV_STOCK(:,:,:,:,:,:,:) = 0.0
    y2_indices = (/(i + 5, i = 1, numstkyrs)/)
  
!...Household and Fleet LDV stocks
    do m2 = 1,NUM_ROWS
      a2 = col4_iage(m2)
	  f2 = col5_ildv(m2)
	  r2 = col3_iregn(m2)
      v2 = col2_ivtyp(m2)
      fl2 = col1_iown(m2)
	  LDV_STOCK(r2,v2,fl2,f2,a2,1,y2_indices) = LDVSTKNFREGN(m2,1:NUM_DATA_COLS)/1000000.0
    enddo  

    DEALLOCATE(LDVSTKNFREGN,col1_iown,col2_ivtyp,col3_iregn,col4_iage,col5_ildv)

! ... *******************************************************************************
! ... * Light Duty Vehicle Stock Module input variables                             *
! ... *******************************************************************************
! ... * LDV Stock Accounting Model                                                  *
! ... *******************************************************************************

!	Estimate CNG and LPG vehicle stocks (overwrites data from trnstockx.xlsx -- Polk does not track these vehicles)
    do j2 = 28, stockyr-1989
        do ILDV=9,12
		    do IVTYP = 1,maxvtyp
			  do iown=1,maxowner
			    if (iown.eq.1) then 
                  do iage=2,maxage-1
		            LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,iage,1,j2) = LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,iage-1,1,j2-1)*SSURV25(1:mnumcr-2,iage-1,IVTYP)
                  enddo
		          LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,maxage,1,j2) = LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,maxage-1,1,j2-1)*SSURV25(1:mnumcr-2,maxage-1,IVTYP) + &
                                                                    LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,maxage,1,j2-1)*SSURV25(1:mnumcr-2,maxage,IVTYP)
				else
				  do iage=2,maxage-1
		            LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,iage,1,j2) = LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,iage-1,1,j2-1)*SURVFLT(iown-1,iage-1,IVTYP)	  
                  enddo
		          LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,maxage,1,j2) = LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,maxage-1,1,j2-1)*SURVFLT(iown-1,iage-1,IVTYP) + &
                                                                    LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,maxage,1,j2-1)*SURVFLT(iown-1,iage-1,IVTYP)
				endif
			  enddo
		    enddo
		enddo
    enddo
	
!...Filling in the national stock number (sum of regional values)
    LDV_STOCK(mnumcr, 1:maxvtyp, 1:maxowner, 1:maxldv, 1:maxage, 1, y2_indices) = &
        sum(LDV_STOCK(1:mnumcr-2, 1:maxvtyp, 1:maxowner, 1:maxldv, 1:maxage, 1, y2_indices),DIM=1)

!...Read in fleet stock totals by fleet type, tech, and vintage
    Flt_Stock(1:mnumcr, 1:maxvtyp, 1:maxfleet, 1:maxldv, 1:maxage, 1, y2_indices) = &
        LDV_STOCK(1:mnumcr, 1:maxvtyp, [1:maxfleet]+1, 1:maxldv, 1:maxage, 1, y2_indices) * 1000000.0

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
    
      REAL EMP_GR

! ... Add an incremental petroleum fuel tax to highway fuel costs.  The tax is read in as
! ... nominal $/million Btu, converted to 1987$, then applied to the fuel prices obtained from PMM.
      FUELTAX87(N) = FUELTAX(N) / MC_JPGDP(N)

! ... Multiplying FUELTAX87 by 0.901 adjusts for the higher Btu content of diesel fuel, thus
! ... maintaining an equivalent per gallon tax.
      HWYPDSTR(1:mnumcr,N) = PDSTRHWY(1:mnumcr,N)
	  
!...  IRA credits for PHEV and EV
	  ira_veh_cred = 0.0
	  ira_bat_cred = 0.0 
	  if(ira_stim.eq.1.0)then
	    ira_veh_cred = 3750.0/ MC_JPGDP(N) * MC_JPGDP(1)
		ira_bat_cred = 3750.0/ MC_JPGDP(N) * MC_JPGDP(1)		
	  endif
      
      INC00_D_NP(1:mnumcr,n)   = (MC_YPDR(1:mnumcr,n)/MC_NP(1:mnumcr,n))*1000.0 /(MC_JPGDP(23)/MC_JPGDP(11))
      INC00_D_16(1:mnumcr,n)   = (MC_YPDR(1:mnumcr,n)/MC_NP16A(1:mnumcr,n)) *1000.0 /(MC_JPGDP(23)/MC_JPGDP(11))
      INC90_D_NP(1:mnumcr,YRS) = (MC_YPDR(1:mnumcr,N)/MC_NP(1:mnumcr,N))*MC_JPGDP(1)*1000.0
      INC_GR_REGN(1:mnumcr,YRS) = INC90_D_NP(1:mnumcr,YRS)/INC90_D_NP(1:mnumcr,YRS-1)
      
!...Start new population model
!...Populate regional age groups by gender
    DO iagr = 1,AGEGRP
      if(MMAC.eq.4) then     ! low macro
        TMC_NP15A(iagr,1,1:mnumcr-2,n)=MC_NP16A(1:mnumcr-2,n)*M_CD_AGE_DIST_L(iagr,n,1:mnumcr-2)
        TMC_NP15A(iagr,2,1:mnumcr-2,n)=MC_NP16A(1:mnumcr-2,n)*F_CD_AGE_DIST_L(iagr,n,1:mnumcr-2)
      elseif(MMAC.eq.5) then ! high macro
        TMC_NP15A(iagr,1,1:mnumcr-2,n)=MC_NP16A(1:mnumcr-2,n)*M_CD_AGE_DIST_H(iagr,n,1:mnumcr-2)
        TMC_NP15A(iagr,2,1:mnumcr-2,n)=MC_NP16A(1:mnumcr-2,n)*F_CD_AGE_DIST_H(iagr,n,1:mnumcr-2)
      else
        TMC_NP15A(iagr,1,1:mnumcr-2,n)=MC_NP16A(1:mnumcr-2,n)*M_CD_AGE_DIST(iagr,n,1:mnumcr-2)
        TMC_NP15A(iagr,2,1:mnumcr-2,n)=MC_NP16A(1:mnumcr-2,n)*F_CD_AGE_DIST(iagr,n,1:mnumcr-2)
      endif
    enddo

!...calculate employment rate for licensing rate
    EMP_RATE_LD(n) = MC_EEA(n)/MC_NP16A(11,n) !sum(TMC_NP15A(1:agegrp,1:mf,1:mnumcr-2,n))
    EMP_GR         = ((EMP_RATE_LD(n)/EMP_RATE_LD(n-1))-1)

!...calculate licensing rate for projection years
    if(curcalyr.gt.licrhistyr) then    
      LICRATE_M(1:agegrp,n,1:mnumcr-2) = LICRATE_M(1:agegrp,n-1,1:mnumcr-2)+LIC_TREND(1:agegrp,1,1:mnumcr-2)+TRANSPOSE(LIC_ELAS(1:mnumcr-2,1:agegrp))*EMP_GR
	  LICRATE_M(1:agegrp,n,1:mnumcr-2) = MIN(LICRATE_M(1:agegrp,n,1:mnumcr-2),LIC_MAX(1:agegrp,1,1:mnumcr-2)) 
      LICRATE_F(1:agegrp,n,1:mnumcr-2) = LICRATE_F(1:agegrp,n-1,1:mnumcr-2)+LIC_TREND(1:agegrp,2,1:mnumcr-2)+TRANSPOSE(LIC_ELAS(1:mnumcr-2,1:agegrp))*EMP_GR
	  LICRATE_F(1:agegrp,n,1:mnumcr-2) = MIN(LICRATE_F(1:agegrp,n,1:mnumcr-2),LIC_MAX(1:agegrp,2,1:mnumcr-2)) 
    endif
    
!...fill regional drivers (millions) assuming licrates across census division  
    LICDRIVER(1:agegrp,1,1:mnumcr-2,n)=TMC_NP15A(1:agegrp,1,1:mnumcr-2,n)*LICRATE_M(1:agegrp,n,1:mnumcr-2)
    LICDRIVER(1:agegrp,2,1:mnumcr-2,n)=TMC_NP15A(1:agegrp,2,1:mnumcr-2,n)*LICRATE_F(1:agegrp,n,1:mnumcr-2)
      
!...sum regional drivers to national levels new
    LICDRIVER(1:agegrp,1:mf,11,n)=sum(LICDRIVER(1:agegrp,1:mf,1:mnumcr-2,n), DIM=3)
      
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
	REAL            CARSHRT_regn(mnumcr,mnumyr), TRKSHRT_regn(mnumcr,mnumyr), &
                    regn_shr(mnumcr,mnumyr)
    
!...Calculate gasoline price in 2000$ per gallon
    PMGTR00_D_C(N) = PMGTR(11,N)* CFMGQ(n)/42.0 * MC_JPGDP(11)*100.0
    PMGTR00_D_C_regn(1:mnumcr-2,N) = PMGTR(1:mnumcr-2,N)* CFMGQ(n)/42.0 * MC_JPGDP(11)*100.0
	do iregn = 1,mnumcr-2
!	    PMGTR00_D_C_regn(iregn,N) = PMGTR(iregn,N)* CFMGQ(n)/42.0 * MC_JPGDP(11)*100.0
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
      NEWLDVs(1:maxvtyp,1:mnumcr-2,n) = TRANSPOSE(sum(sum(LDV_STOCK(1:mnumcr-2,1:maxvtyp,1:maxowner,1:maxldv,1,1,n),DIM=4),DIM=3))
      NEWLDVs(1:maxvtyp,mnumcr,n) = sum(sum(sum(LDV_STOCK(1:mnumcr-2,1:maxvtyp,1:maxowner,1:maxldv,1,1,n),DIM=4),DIM=3),DIM=1)    
	  
!...  Calculate sales distribution of cars and light trucks across regions
      CarTrkSplit(1:mnumcr-2,1:maxvtyp,N) = sum(sum(LDV_STOCK(1:mnumcr-2,1:maxvtyp,1:maxowner,1:maxldv,1,1,n),DIM=4),DIM=3) / &
                                            SPREAD(sum(sum(sum(LDV_STOCK(1:mnumcr-2,1:maxvtyp,1:maxowner,1:maxldv,1,1,n),DIM=4),DIM=3),DIM=2),DIM=2,NCOPIES=maxvtyp)

	  CarTrkSplit(mnumcr,1,N) = sum(LDV_STOCK(mnumcr,1,1:maxowner,1:maxldv,1,1:maxhav,n))/sum(LDV_STOCK(mnumcr,1:maxvtyp,1:maxowner,1:maxldv,1,1,n))
	  CarTrkSplit(mnumcr,2,N) = 1.0 - CarTrkSplit(mnumcr,1,N)

!...  Calculate new vehicle sales per licensed driver by region	
      NewLDVPerLD(1:mnumcr-2,n) = sum(NEWLDVs(1:maxvtyp,1:mnumcr-2,n),DIM=1) / &
                                  sum(sum(LicDriver(1:agegrp,1:mf,1:mnumcr-2,n),DIM=2),DIM=1)
	endif ! <= stockyr
	
!...Project percent of total light vehicles <8,500 GVW that are cars and light trucks by region
    if(curcalyr.gt.stockyr) then
!...  Determine new LDV sales shares by region post STOCKYR based on licensed drivers
      TEMPLDVSALES(1:mnumcr-2,n) = NewLDVPerLD(1:mnumcr-2,stockyr-1989)*sum(sum(LICDriver(1:agegrp,1:mf,1:mnumcr-2,n),DIM=2),DIM=1)

      regn_shr(1:mnumcr-2,n) = TEMPLDVSALES(1:mnumcr-2,n)/sum(TEMPLDVSALES(1:mnumcr-2,n))

!...  regional shares
      CARSHRT_regn(1:mnumcr-2,N) = EXP(CCONSTANT(1:mnumcr-2)*(1-CRHO(1:mnumcr-2))+(CRHO(1:mnumcr-2)*LOG(CarTrkSplit(1:mnumcr-2,1,n-1))) +          &
               CINC(1:mnumcr-2) *(LOG(INC00_D_16(1:mnumcr-2,n))- (CRHO(1:mnumcr-2)*LOG(INC00_D_16(1:mnumcr-2,n-1)))) + &
               CFUEL(1:mnumcr-2)*(LOG(PMGTR00_D_C_regn(1:mnumcr-2,n))  - (CRHO(1:mnumcr-2)*LOG(PMGTR00_D_C_regn(1:mnumcr-2,n-1)))) +   &
               CHP(1:mnumcr-2)  *(LOG(AHPCAR(1:mnumcr-2,n-1))   - (CRHO(1:mnumcr-2)*LOG(AHPCAR(1:mnumcr-2,n-2)))) +    &
               CWGT(1:mnumcr-2) *(LOG(AWTCAR(1:mnumcr-2,n-1))   - (CRHO(1:mnumcr-2)*LOG(AWTCAR(1:mnumcr-2,n-2)))) +    &
               CMPG(1:mnumcr-2) *(LOG(TRUEMPG_regn(1:mnumcr-2,1,n-1)) - (CRHO(1:mnumcr-2)*LOG(TRUEMPG_regn(1:mnumcr-2,1,n-2)))) +    &
               CDUMM(1:mnumcr-2)*(log(DUMM(n)) - (CRHO(1:mnumcr-2)*log(DUMM(n-1)))))

      TRKSHRT_regn(1:mnumcr-2,N) = EXP(TCONSTANT(1:mnumcr-2)*(1-TRHO(1:mnumcr-2))+(TRHO(1:mnumcr-2)*LOG(CarTrkSplit(1:mnumcr-2,2,n-1))) +          &
             TINC(1:mnumcr-2) *(LOG(INC00_D_16(1:mnumcr-2,n))- (TRHO(1:mnumcr-2)*LOG(INC00_D_16(1:mnumcr-2,n-1)))) + &
             TFUEL(1:mnumcr-2)*(LOG(PMGTR00_D_C_regn(1:mnumcr-2,n))  - (TRHO(1:mnumcr-2)*LOG(PMGTR00_D_C_regn(1:mnumcr-2,n-1)))) +   &
             THP(1:mnumcr-2)  *(LOG(AHPTruck(1:mnumcr-2,n-1)) - (TRHO(1:mnumcr-2)*LOG(AHPTruck(1:mnumcr-2,n-2)))) +    &
             TWGT(1:mnumcr-2) *(LOG(AWTTruck(1:mnumcr-2,n-1)) - (TRHO(1:mnumcr-2)*LOG(AWTTruck(1:mnumcr-2,n-2)))) +    &
             TMPG(1:mnumcr-2) *(LOG(TRUEMPG_regn(1:mnumcr-2,2,n-1)) - (TRHO(1:mnumcr-2)*LOG(TRUEMPG_regn(1:mnumcr-2,2,n-2)))) +    &
             TDUMM(1:mnumcr-2)*(log(DUMM(n)) - (TRHO(1:mnumcr-2)*log(DUMM(n-1)))))	
      
	  CarTrkSplit(1:mnumcr-2,1,N) = CARSHRT_regn(1:mnumcr-2,n)/(CARSHRT_regn(1:mnumcr-2,n) + TRKSHRT_regn(1:mnumcr-2,n))
	  CarTrkSplit(1:mnumcr-2,2,N) = 1.0 - CarTrkSplit(1:mnumcr-2,1,N)

!...New car and light truck sales projection by census division
	  do ivtyp=1,maxvtyp
	    NEWLDVs(ivtyp,1:mnumcr-2,n) = (MC_SUVA(N) + TEMPCLS12A(N)) * regn_shr(1:mnumcr-2,n) * CarTrkSplit(1:mnumcr-2,ivtyp,N)
		NEWLDVs(ivtyp,mnumcr,n) = sum(NEWLDVs(ivtyp,1:mnumcr-2,n))
		if (ivtyp.eq.2) Then
		  CarTrkSplit(mnumcr,1,N) = NEWLDVs(1,mnumcr,n)/sum(NEWLDVs(1:maxvtyp,mnumcr,n))
		  CarTrkSplit(mnumcr,2,N) = 1.0 - CarTrkSplit(mnumcr,1,N)
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
    USE MEAN_FUNCS
    IMPLICIT NONE

    integer i2,jgp,ipass
    integer it,start_grp,end_grp
	real	num,num1,num2,den,PHEV50_EVMT(MAXVTYP),PHEV20_EVMT(MAXVTYP),sales_per_nameplate,&
            nameplate_sensitivity(ildv),temp_cnt,sum_nameplate_epalyr,sum_nameplate_curyr, &
            sum_ldvsales_allLDV
    integer, parameter :: MAX_CAFETEST_PASS = 30
    REAL sum_deficit_grps
    
    logical ldv_comply_debug/.TRUE./

!   Values needed to write out equal-weighted-average LDV msrps and mpgs
    real    price_eqwgt(4,maxclass,maxldv,mnumyr),sales_temp(4,maxclass,maxldv,mnumyr),mpg_eqwgt(4,maxclass,maxldv,mnumyr)
    integer grp_to_luxPVLT(maxgroup)
    data    grp_to_luxPVLT/1,1,1,2,2,3,3,3,3,3,4/
    logical eq_wgt_write/.FALSE./
    logical eq_wgt_write_incl_exotics/.FALSE./
    
!   Switch to write out detailed inputs/outputs
    real    price_temp(maxgroup,maxclass,mnumyr)
    logical SizeClassWrite/.FALSE./
    logical choice_mdl_write/.FALSE./
    logical classflagwrite/.FALSE./
    logical choice_mdl_fprice_write/.FALSE./

!   Timers
    INTEGER(KIND=4) :: start_count, end_count, count_rate, count_max
    REAL(KIND=8)    :: elapsed_time_seconds
    REAL(KIND=8)    :: elapsed_time_CAFETEST = 0.0
    INTEGER         :: result
    
!...calculate % of PHEV vmt in all electric mode.  
!   Calculate aggregate averages (phev20_evmt and phev50_evmt) to fill in any zeros in the overall array (phev_evmt)
	if(curcalyr.eq.epalyr) then
      phev20_evmt(1) = WEIGHTED_MEAN_2D(phev_evmt(1:cargrp,1:maxclass,yrs,5), &
                                        cafesales(1:cargrp,1:maxclass,yrs,5),&
                                        caller_id = 'phev20_evmt(1)')
      phev20_evmt(2) = WEIGHTED_MEAN_2D(phev_evmt(ltkgrp:maxgroup,1:maxclass,yrs,5), &
                                        cafesales(ltkgrp:maxgroup,1:maxclass,yrs,5),&
                                        caller_id = 'phev20_evmt(1)')
      phev50_evmt(1) = WEIGHTED_MEAN_2D(phev_evmt(1:cargrp,1:maxclass,yrs,6), &
                                        cafesales(1:cargrp,1:maxclass,yrs,6),&
                                        caller_id = 'phev20_evmt(1)')
      phev50_evmt(2) = WEIGHTED_MEAN_2D(phev_evmt(ltkgrp:maxgroup,1:maxclass,yrs,6), &
                                        cafesales(ltkgrp:maxgroup,1:maxclass,yrs,6),&
                                        caller_id = 'phev20_evmt(1)')                                  	  
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
	  phev_evmt(1:maxgroup, 1:maxclass, yrs, 5:6) = phev_evmt(1:maxgroup, 1:maxclass, yrs-1, 5:6)
	endif

!...In setting ATV car and truck flags, user inputs are overridden if the corresponding gasoline vehicle does not exist.
!...In other words, an ATV group/class combination is not allowed unless the same group/class combination is allowed for 
!...gasoline vehicles.  This is necessary since initial ATV attributes are expressed relative to gasoline. Note, the 
!...CLASSFLAG is set for all years after initial penetration year, since there is a time dimension. 
    
!...determine AFV availability between XYR and EPALYR
	if(curcalyr.ge.xyr.and.curcalyr.le.epalyr) then
	  do ildv=1,maxldv
        do icl=1,maxclass
          do igp=1,maxgroup
            classflag(igp,icl,ildv)= .false.
!           Vehicle exists in 2022
	        if(curcalyr.eq.xyr.and.femmpg(igp,icl,yrs,ildv).ne.0.0) then
		      classflag(igp,icl,ildv) = .true.
!           Vehicle introduced after XYR
			elseif (.not.classflag(igp,icl,ildv).and.curcalyr.gt.xyr) then
              if(curcalyr.lt.epalyr.and.femmpg(igp,icl,yrs,ildv).ne.0.0) then 
			    classflag(igp,icl,ildv) = .true.
                do i=base,current       ! Fill attributes with values from trnfem (READHIST)
		          FE(igp,icl,i,ildv)       = femmpg(igp,icl,yrs,ildv)
                  WEIGHT(igp,icl,i,ildv)   = femwgt(igp,icl,yrs,ildv)
                  PRICE(igp,icl,i,ildv)    = fempri(igp,icl,yrs,ildv)
                  HP(igp,icl,i,ildv)       = femhp(igp,icl,yrs,ildv)
                  TANKSIZE(igp,icl,i,ildv) = femtsz(igp,icl,yrs,ildv)
		          RANGE(igp,icl,i,ildv)	 = femrng(igp,icl,yrs,ildv)	
                  MKT_PEN(igp,icl,1:maxtech,i,ildv) = MKT_PEN(igp,icl,1:maxtech,i,gas)
                enddo      

                epampg(igp,icl,yrs,ildv) = FEMMPG(igp,icl,yrs,ildv)
                epawgt(igp,icl,yrs,ildv) = FEMWGT(igp,icl,yrs,ildv)
                epapri(igp,icl,yrs,ildv) = FEMPRI(igp,icl,yrs,ildv)
                epahp(igp,icl,yrs,ildv)  = FEMHP(igp,icl,yrs,ildv)	
                epatsz(igp,icl,yrs,ildv) = FEMTSZ(igp,icl,yrs,ildv)
                eparng(igp,icl,yrs,ildv) = FEMRNG(igp,icl,yrs,ildv)
              
              elseif (curcalyr.eq.epalyr.and.epampg(igp,icl,yrs,ildv).ne.0.0) then  ! Fill attributes with values from trnnhtsa (READNHTSA)
                classflag(igp,icl,ildv) = .true.
                do i=base,current
		          FE(igp,icl,i,ildv)       = epampg(igp,icl,yrs,ildv)
                  WEIGHT(igp,icl,i,ildv)   = epawgt(igp,icl,yrs,ildv)
                  PRICE(igp,icl,i,ildv)    = epapri(igp,icl,yrs,ildv)
                  HP(igp,icl,i,ildv)       = epahp(igp,icl,yrs,ildv)
                  TANKSIZE(igp,icl,i,ildv) = epatsz(igp,icl,yrs,ildv)
		          RANGE(igp,icl,i,ildv)	 = eparng(igp,icl,yrs,ildv)	
                  MKT_PEN(igp,icl,1:maxtech,i,ildv) = MKT_PEN(igp,icl,1:maxtech,i,gas)
                enddo
			    FEMMPG(igp,icl,yrs,ildv) = epampg(igp,icl,yrs,ildv)
			    FEMWGT(igp,icl,yrs,ildv) = epawgt(igp,icl,yrs,ildv)
			    FEMPRI(igp,icl,yrs,ildv) = epapri(igp,icl,yrs,ildv)
			    FEMHP(igp,icl,yrs,ildv)	 = epahp(igp,icl,yrs,ildv)
			    FEMTSZ(igp,icl,yrs,ildv) = epatsz(igp,icl,yrs,ildv)
			    FEMRNG(igp,icl,yrs,ildv) = eparng(igp,icl,yrs,ildv)       
              endif
			endif
            
			if(classflag(igp,icl,ildv).and.FEMMPG(igp,icl,yrs,ildv).eq.0.0) then 
!...		  ildv discontinued after xyr
			  classflag(igp,icl,ildv) = .false.
!             Zero out attributes              
              FE(igp,icl,base:current,ildv)       = 0.0
              WEIGHT(igp,icl,base:current,ildv)   = 0.0
              PRICE(igp,icl,base:current,ildv)    = 0.0
              HP(igp,icl,base:current,ildv)       = 0.0
              TANKSIZE(igp,icl,base:current,ildv) = 0.0
              RANGE(igp,icl,base:current,ildv)    = 0.0
              
			  FEMMPG(igp,icl,yrs,ildv) = 0.0
			  FEMWGT(igp,icl,yrs,ildv) = 0.0
			  FEMPRI(igp,icl,yrs,ildv) = 0.0
			  FEMHP(igp,icl,yrs,ildv)  = 0.0
			  FEMTSZ(igp,icl,yrs,ildv) = 0.0
			  FEMRNG(igp,icl,yrs,ildv) = 0.0
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
    if (curcalyr.ge.epalyr+2.and.curcalyr.le.2032.and.ENFORCE_MY27REGS.eq.1) then
      nameplate_sensitivity(:) = 1.0
    elseif (ENFORCE_MY27REGS.eq.1) then      ! Manufacturers less likely to introduce new alt-fuel nameplates when regs aren't getting more stringent
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
                if(classflag(igp,icl,ildv)) then
                  nameplate(igp,icl,yrs,ildv) = nameplate(igp,icl,yrs-1,ildv) + 1.0
!               If this nameplate is the first of it's kind (igp/icl/ildv), make it
	 	 	    elseif(.not.classflag(igp,icl,ildv)) then 
	 	 	      if (classflag(igp,icl,GAS)) then
	 	 	        classflag(igp,icl,ildv) = .true.
	 	 	        CALL AFVADJ (YRS,igp)
	 	 	        nameplate(igp,icl,yrs,ildv) = 1.0
!                 If this manufacturer group doesn't have any gasoline vehicles in this size class,
!                   the model won't be able to build the alt-fuel powertrain option (in AFVADJ).
!                 Instead, we will introduce the new nameplate across all related (car or LT) manufacturer groups.
!                   That new nameplate would have extracted market share from the other groups' sales in that size class anyway.
!                 In other words: this takes the existing market for that size class and adds a new powertrain to it
                  else
                    IF (igp <= cargrp) THEN
                      start_grp = 1
                      end_grp = cargrp
                    ELSE  ! light truck
                      start_grp = ltkgrp
                      end_grp = maxgroup
                    ENDIF
                    
                    if (ANY(classflag(start_grp:end_grp,icl,GAS))) then
                      temp_cnt = REAL(COUNT(classflag(1:cargrp,icl,GAS)))
                      do jgp = start_grp,end_grp
                        if (classflag(jgp,icl,GAS)) then
                          if (classflag(jgp,icl,ildv)) then
                            nameplate(jgp,icl,yrs,ildv) = nameplate(jgp,icl,yrs,ildv) + 1/temp_cnt
                          else
                            classflag(jgp,icl,ildv) = .true.
                            GRPFLAG(ildv,icl,jgp) = yrs
                            CALL AFVADJ (YRS,jgp)
                            nameplate(jgp,icl,yrs,ildv) = 1/temp_cnt
                          endif
                        endif
                      enddo
                    endif
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
		  sum_nameplate_epalyr = sum(nameplate(igp,icl,epalyr,1:maxldv))
          sum_nameplate_curyr = sum(nameplate(igp,icl,yrs,1:maxldv))
          do ildv=1,maxldv
            do iregn = 1, mnumcr-2
              mmavail(igp,ildv,icl,iregn,yrs) = 0.0
!             Last historical year: if there were sales, calculate mmavail
              if (curcalyr.eq.epalyr.and.ldv_sales(igp,icl,ildv,iregn,n).gt.0.0) then
                if (ildv.eq.1) then
                  mmavail(igp,ildv,icl,iregn,yrs) = 1.0
                elseif(sum_nameplate_epalyr.ne.0.0) then
                  mmavail(igp,ildv,icl,iregn,yrs) = nameplate(igp,icl,epalyr,ildv)/sum_nameplate_epalyr
                else 
                  mmavail(igp,ildv,icl,iregn,yrs) = 0.0
                endif
!             All projection years: calculate mmavail (no sales available yet, haven't run choice model)
              elseif (curcalyr.gt.epalyr) then
                if (ildv.eq.1.and.nameplate(igp,icl,yrs,ildv).gt.0.0) then
                  mmavail(igp,ildv,icl,iregn,yrs) = 1.0
                else
!                 Note that mmavail is calculated as the ratio of current-year nameplate count divided by total nameplate
!                 count from the LAST HISTORICAL YEAR (EPALYR). Non-hybrid gasoline mmavail is always 1.0, so if new AFV
!                 nameplates are added, and the denominator increases (current year total nameplates), then non-hybrid
!                 gasoline will snag share from the AFVS that already existed every time a new AFV nameplate is added.. 
                  if (sum_nameplate_epalyr.gt.0.0) then
                    mmavail(igp,ildv,icl,iregn,yrs) = MIN(nameplate(igp,icl,yrs,ildv)/sum_nameplate_epalyr,1.0)
		          endif
                endif
              endif
!              if (curcalyr.ge.2023.and.curcalyr.le.2024) WRITE(21,'(a,6(i4,","),2(f12.4,","))')'mmavail_debug',curcalyr,curitr,igp,icl,ildv,iregn,mmavail(igp,ildv,icl,iregn,yrs),nameplate(igp,icl,epalyr,ildv)
            enddo
          enddo
		enddo
	  enddo
	endif

! ... FEM is a three pass module in instances where CAFE/GHG are not met.
! ... The second pass tries to adopt technology sufficient to meet CAFE demand while 
! ... maintaining econometric and tech-driven HP increases.
! ... If CAFE is still not met after the second pass, the HP increases will be
! ... "backed out" and converted to
! ... equivalent FE on the third pass (in effect, assuming manufacturers
! ... will minimize their costs by complying with CAFE to the maximum extent
! ... possible before pushing additional HP increases "out the door."  This
! ... also means that calls to the NHTSA calibration routine are not performed
! ... until after the third pass.

      cafepass(1:maxgroup) = .FALSE.

!...  Set fuel cost and income.  Lagged fuel costs (up to 7 years) and income (up to 1 year) are used in FEM calcs, so these parameters must
!...  be set for years leading up to the FEM base year.  Simply setting fuel cost and income for each year from the TRAN base year on provides 
!...  all the lag data necessary.  
      
      PMGTR90_D_(1:mnumcr,YRS) = (PMGTR(1:mnumcr,N)*CFMGQ(n)/42.0) * MC_JPGDP(1)
      PMGTR90_INVGR(1:mnumcr,YRS) = PMGTR90_D_(1:mnumcr,YRS-1)/PMGTR90_D_(1:mnumcr,YRS)

!     Check gasoline price before proceeding and stop NEMS if not good      
      do iregn = 1, mnumcr
        if (iregn.eq.10) CYCLE
        if (PMGTR(iregn,N).ne.PMGTR(iregn,N)) then
          WRITE(*,*)'ERROR: NaN gasoline price PMGTR in regn ',iregn,' in ',N+1989
          STOP 111
        elseif(PMGTR(iregn,N).eq.0.0) then
          WRITE(*,*)'ERROR: $0 gasoline price PMGTR in regn ',iregn,' in ',N+1989
          STOP 112
        endif
      enddo

! ... For the first pass, see what the market comes up with. 
      FEM_PASS=1
      RegCost(1:maxgroup)=0.0
      if(curcalyr.gt.xyr) CALL FEMCALC
      CALL CGSHARE
      CALL TATTRIB
      CALL TALT2
	  if(curcalyr.gt.stockyr) CALL TALT2X
      CALL CAFECALC(0)
      CALL CAFECALC(1)
      
! ... Second pass. If any group is out of compliance, 
! ... implement the fine incrementally.
      FEM_PASS=2
      npass2=10
!     In case with BEV-requiring standards, don't waste extra time
!     incrementing up the reg_cost in tiny steps
      if (ENFORCE_MY27REGS.eq.1.and.curcalyr.ge.2027) npass2=5
      do pass2=1,npass2
        RegCost(1:maxgroup)=Reg_Cost*(float(pass2)/float(npass2))
        if(curcalyr.gt.xyr) then
          CALL FEMCALC
          CALL CGSHARE
          CALL TATTRIB
          CALL TALT2
          if(curcalyr.gt.stockyr) CALL TALT2X
          CALL CAFECALC(0)
        endif
        if (ALL(cafepass(:))) THEN
          if(fcrl.eq.1.and.ldv_comply_debug) WRITE(21,'(i4,a,i2,a,i2)')curcalyr,' LDV: Passed CAFE/GHG in FEM_PASS ',FEM_PASS,' npass ',pass2
          CALL CAFECALC(1)
          EXIT
        endif
      end do
      
! ... Third pass. If any group is out of compliance, 
! ... start converting horsepower improvements to mpg ("HP giveback").      
      FEM_PASS=3
      npass3=10
!     In case with BEV-requiring standards, don't waste extra time
!     incrementing up the horsepower giveback in tiny steps
      if (ENFORCE_MY27REGS.eq.1.and.curcalyr.ge.2027) npass3=5
      RegCost(1:maxgroup)=Reg_Cost
      do pass3=1,npass3
        GBInc=float(pass3)/float(npass3)
        if(curcalyr.gt.xyr) then
          CALL FEMCALC
          CALL CGSHARE
          CALL TATTRIB
          CALL TALT2
          if(curcalyr.gt.stockyr) CALL TALT2X
          CALL CAFECALC(0)
        endif
        if (ALL(cafepass(:))) THEN
          if(fcrl.eq.1.and.ldv_comply_debug) WRITE(21,'(i4,a,i2,a,i2)')curcalyr,' LDV: Passed CAFE/GHG in FEM_PASS ',FEM_PASS,' npass ',pass3
          CALL CAFECALC(1)
          EXIT
        endif
      end do

!...call CAFEGHG_MEET if the market is still out of compliance with CAFE and/or EPA GHG
!   Recalculate compliance for the OVERALL market (implies credit trading)
	CALL SYSTEM_CLOCK(COUNT=start_count, COUNT_RATE=count_rate, COUNT_MAX=count_max)
	if(curcalyr.gt.epalyr+1.and.ANY(.not.cafepass(:))) then

!     In the years following the pseudo-historical year (EPALYR + 2:EPALYR + 4), use up the remaining GHG credit bank
      if (curcalyr.ge.epalyr+2.and.curcalyr.le.epalyr+3) then
!       Spend credits more quickly up front due to likely compliance struggle in the near-term
        if (curcalyr.eq.epalyr+2) creds_avail = CREDBAL_EPALYR*0.7
        if (curcalyr.eq.epalyr+3) creds_avail = CREDBAL_EPALYR*0.3
!       Sum up the total deficits (only mfr groups that are in the negative), sum_deficit_grps
        sum_deficit_grps = 0.0
        do igp = 1, maxgroup
          if (MgGhgGrp(igp,n).lt.0.0.and.igp.ne.5) sum_deficit_grps = sum_deficit_grps + MgGhgGrp(igp,n)
        enddo
        
        if(fcrl.eq.1.and.ldv_comply_debug)WRITE(21,'(i4,a,",",5(f12.0,","))')curcalyr,' LDV: Spent remaining GHG creds in EPALYR + 2', &
                                      SUM(MgGhGGrp(1:maxgroup,n)),SUM(MgGhGGrp(1:maxgroup,n-2:n)), CREDBAL_EPALYR, &
                                      creds_avail, sum_deficit_grps
        
!       Distribute the available credits across the groups that are in the negative proportionate to deficit size          
        do igp = 1, maxgroup
          if (MgGhgGrp(igp,n).lt.0.0.and.igp.ne.5) creds_avail_grp(igp) = creds_avail * MgGhgGrp(igp,n)/sum_deficit_grps
        enddo
        
        if(fcrl.eq.1.and.ldv_comply_debug)WRITE(21,'(i4,a,",",f12.0)')curcalyr,' LDV: Spent remaining GHG creds in EPALYR + 2', SUM(MgGhGGrp(1:maxgroup,n)) + sum(creds_avail_grp(1:maxgroup))

      ELSE
        creds_avail = 0.0
        creds_avail_grp(1:maxgroup) = 0.0
      endif

      first_time_cafetest = .true.
      cafepass(:) = .true.
	  do igp=1,maxgroup
        ivtyp = GrpMap(igp)

!       If enforcing CAFE and EPA GHG
        if (ENFORCE_EPA.eq.1) then 

!         If the aggregate market is out of compliance
          if (sum(MgGhgGrp(:,n))+creds_avail.lt.0.0.or.NewMPG(3,n).lt.cafestd(3,n)) then 
!           Determine whether it's aggregate car or truck that is out of compliance (or both), and flag the individual groups that are out
            if (MgGhgGrp(igp,n)+creds_avail_grp(igp).lt.0.0 .or. (CafeMpgGrp(igp,n).lt.Cafe_Used(igp,yrs).and.ENFORCE_CAFE.eq.1)) then
              cafepass(igp) = .false.
            endif
!         If the aggregate market is in compliance, but said compliance would require transferring more than the maximum allowed mpg
!         b/w car/truck (2mpg), flag all the groups that are out of compliance          
          elseif ((NewMPG(ivtyp,n)-cafestd(ivtyp,n)).lt.-2.0.and.CafeMpgGrp(igp,n).lt.Cafe_Used(igp,yrs).and.ENFORCE_CAFE.eq.1) then
            cafepass(igp) = .false.
          endif
!       If not enforcing EPA (CAFE only)
        else
!         If the aggregate market is out of compliance
          if (NewMPG(3,n).lt.cafestd(3,n).and.ENFORCE_CAFE.eq.1) then
!           Determine whether it's aggregate car or truck that is out of compliance (or both), and flag the individual groups that are out
            if (CafeMpgGrp(igp,n).lt.Cafe_Used(igp,yrs)) then
              cafepass(igp) = .false.
            endif
!         If the aggregate market is in compliance, but said compliance would require transferring more than the maximum allowed mpg
!         b/w car/truck (2mpg), flag all the groups that are out of compliance          
          elseif ((NewMPG(ivtyp,n)-cafestd(ivtyp,n)).lt.-2.0.and.CafeMpgGrp(igp,n).lt.Cafe_Used(igp,yrs)) then
            cafepass(igp) = .false.
          endif
        endif
      enddo
      
      if (ALL(cafepass(1:maxgroup))) THEN
        if(fcrl.eq.1.and.ldv_comply_debug) WRITE(21,'(i4,a)')curcalyr,' LDV: Passed CAFE/GHG after FEM without CAFEGHG_MEET due to aggregate market passing'
        CALL CAFECALC(1)
      else

!       Now entering CAFEGHG_MEET, which converts gasoline vehicle sales into BEV, PHEV, or HEV sales until the market
!       complies with NHTSA CAFE and EPA GHG (or both/neither, depending on inputs, e.g. ENFORCE_EPA, ENFORCE_CAFE). 
!       In it's first pass (first_time_cafetest = TRUE), CAFEGHG_MEET distributes the total
!       "compliance burden" across manufacturers based on either 1) the total MgCO2 deficit (if ENFORCE_EPA is on) or
!       2) gasoline vehicle sales in classes where BEVs, PHEVs, or HEVs are available (mmavail).
!       This "compliance burden" determines the share of vehicle sales, in that pass, which must be converted from gasoline.
!       It goes through up to 30 passes -- each pass converting up to 100,000 sales total across the manufacturer groups.
!       If compliance is achieved (CAFEPASS is true for all groups), no more passes are required.
        do ipass = 1, MAX_CAFETEST_PASS
          first_time_cafetest = .TRUE.
          MAXADJ_CAFETEST = ipass * 0.1
          do igp=maxgroup,1,-1
            if (.not.cafepass(IGP)) then
              if(fcrl.eq.1.and.ldv_comply_debug) WRITE(21,'(i4,a,i2,",",4(f12.0,","))')curcalyr,' LDV: Entering CAFEGHG_MEET for group ',igp, &
                                                                  MgGhgGrp(igp,n), sum(MgGhgGrp(:,n)), creds_avail_grp(igp), creds_avail
              call CAFEGHG_MEET
!              if(fcrl.eq.1) WRITE(21,'(f10.3,a,i4,a,i4)')sum(cafesales(igp,1:maxclass,yrs,[7,15])) - sum(avsales_old(igp,1:maxclass,[7,15],n)),' BEVs added to group ',igp,' in ',curcalyr
!              if(fcrl.eq.1) WRITE(21,'(f10.3,a,i4,a,i4)')sum(cafesales(igp,1:maxclass,yrs,[5,6])) - sum(avsales_old(igp,1:maxclass,[5,6],n)),' PHEVs added to group ',igp,' in ',curcalyr
            endif
          enddo
!         Call CAFECALC to re-calculate all aggregate average attributes (mpg, weight, horsepower, etc)
          if (ALL(CAFEPASS(1:maxgroup))) then
            if (fcrl.eq.1.and.ldv_comply_debug) WRITE(21,'(i4,a,f5.4,a,f5.4,a)')curcalyr,' LDV: Between ',(ipass-1) * 0.1,' and ',ipass * 0.1,' million sales converted to meet CAFE/GHG.'
            CALL CAFECALC(1)
            exit
          elseif (ipass.eq.MAX_CAFETEST_PASS) then
            CALL CAFECALC(1)
          endif
        enddo
        
!       Writes to track compliance progress over projection 
        if (ldv_comply_debug) then
          if ((NewMPG(3,n).gt.cafestd(3,n).and.((NewMPG(1,n)-cafestd(1,n)).gt.-2.0).and.((NewMPG(2,n)-cafestd(2,n)).gt.-2.0)) .or. ENFORCE_CAFE.eq.0 ) then
            if (ENFORCE_EPA.eq.1) then
              if (sum(MgGhgGrp(:,n))+ creds_avail.gt.0.0) then
                if(fcrl.eq.1)WRITE(21,'(i4,a,",",f10.0)')curcalyr,' LDV: PASSED CAFE/GHG -- WHOLE MARKET', sum(MgGhgGrp(:,n)) + creds_avail
              else
                if(fcrl.eq.1)WRITE(21,'(i4,a,",",f10.0)')curcalyr,' LDV: FAILED GHG PASSED CAFE -- WHOLE MARKET', sum(MgGhgGrp(:,n)) + creds_avail
              endif
            else
              if(fcrl.eq.1)WRITE(21,'(i4,a,",",f10.0)')curcalyr,' LDV: PASSED CAFE -- WHOLE MARKET -- EPA NOT ENFORCED',sum(MgGhgGrp(:,n)) + creds_avail
            endif
          else
            if (ENFORCE_EPA.eq.1) then
              if(fcrl.eq.1)WRITE(21,'(i4,a,",",f10.0)')curcalyr,' LDV: FAILED CAFE PASSED GHG -- WHOLE MARKET',sum(MgGhgGrp(:,n)) + creds_avail
            else
              if(fcrl.eq.1)WRITE(21,'(i4,a,",",f10.0)')curcalyr,' LDV: FAILED CAFE -- WHOLE MARKET -- EPA NOT ENFORCED',sum(MgGhgGrp(:,n)) + creds_avail
            endif
          endif
        endif
      endif ! ALL(cafepass(1:maxgroup))
    
    elseif(ALL(.not.cafepass(:))) then
      if(fcrl.eq.1)WRITE(21,'(i4,a)')curcalyr, ' LDV: FAILED CAFE/GHG -- WHOLE MARKET'
    endif
    
    CALL SYSTEM_CLOCK(COUNT=end_count)
    elapsed_time_seconds = REAL(end_count - start_count, KIND=8) / REAL(count_rate, KIND=8)
    elapsed_time_CAFETEST = elapsed_time_CAFETEST + elapsed_time_seconds
    if(n.eq.mnumyr.and.fcrl.eq.1) WRITE(21,*) "Elapsed wall-clock time (CAFETESTTOTAL): ", elapsed_time_CAFETEST, " seconds"


!   DEBUG STATEMENTS and detailed outputs
    if (n.eq.mnumyr.and.fcrl.eq.1) then
      if (choice_mdl_fprice_write) then
        do iregn=1,mnumcr-2
          WRITE(21,'(a,",",16(f12.6,","))')'fprice',FPRICE(1:maxldv,iregn,epalyr)
        ENDDO
      endif
      
      if (SizeClassWrite) then
        price_temp(:,:,:) = 0.0
        do i2 = 21,mnumyr
          do igp=1,maxgroup
            do icl=1,maxclass
              sum_ldvsales_allLDV = sum(ldv_sales(igp,icl,1:maxldv,mnumcr,i2))
              do ildv=1,maxldv
                if (ildv.le.2.or.ildv.eq.5.or.ildv.eq.6.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.16) then
                  if (fempri(igp,icl,i2+1989,ildv).gt.0.0) then 
                    price_temp(igp,icl,i2) = price_temp(igp,icl,i2) + ldv_sales(igp,icl,ildv,mnumcr,i2) * fempri(igp,icl,i2+1989,ildv)
                  endif
                endif
              enddo
              if (sum_ldvsales_allLDV.gt.0.0) price_temp(igp,icl,i2) = price_temp(igp,icl,i2)/sum_ldvsales_allLDV
            enddo
          enddo
        enddo
        
        WRITE(21,*)'LDV Size Class Model Inputs and Results (all prices in 1990USD)'
        WRITE(21,'(4(a4,","),5(a12,","),8(a10,","))')'year','regn','grp','cls','class_share','GasPriGrowth','IncGrowth','VehPriGrowth','VehSales','VehPri','VehPriLag1','Income','IncomeLag1','GasPri','GasPriLag1','VehPriAll','VehPriAllLag1'
        do it=2010-1989,mnumyr
          do iregn=1,mnumcr
            if(iregn.eq.10) CYCLE
            do igp=1,maxgroup
              do icl=1,maxclass
                WRITE(21,'(4(i4,","),4(f12.5,","),f12.0,",",8(f10.3,","))')it+1989,iregn,igp,icl,class_share(iregn,icl,igp,it+1989),pmgtr90_D_(iregn,it+1989)/pmgtr90_D_(iregn,it+1989-1),&
                                                          (inc90_D_np(iregn,it+1989)-13000.0)/(inc90_D_np(iregn,it+1989-1)-13000.0), &
                                                          FEMPRI(IGP,ICL,it+1989,1)/FEMPRI(IGP,ICL,it+1989-1,1),mfr_sales(iregn,igp,icl,it)*1000000,FEMPRI(IGP,ICL,it+1989,1),&
                                                          FEMPRI(IGP,ICL,it+1989-1,1),inc90_D_np(iregn,it+1989),inc90_D_np(iregn,it+1989-1),pmgtr90_D_(iregn,it+1989),pmgtr90_D_(iregn,it+1989-1),&
                                                          price_temp(igp,icl,it),price_temp(igp,icl,it-1)
              enddo
            enddo
          enddo
        enddo
      endif
            
      if (choice_mdl_write) then
        WRITE(21,*)'choice_model_output'    
        WRITE(21,*)'var,year,regn,grp,icl,ildv,val'
        do i2 = 34, mnumyr
          do iregn = 1, mnumcr
            if (iregn.ge.10) CYCLE
            do igp=1,maxgroup
              do icl=1,maxclass
                do ildv = 1,maxldv
                  if (ildv.ge.8.and.ildv.le.13) CYCLE
                  if (mmavail(igp,ildv,icl,iregn,i2+1989).eq.0.0) CYCLE
                  WRITE(21,'(a,",",5(i4,","),f10.3)')'mmavail',i2+1989,iregn,igp,icl,ildv,mmavail(igp,ildv,icl,iregn,i2+1989)
                  WRITE(21,'(a,",",5(i4,","),f10.1)')'batt_kwh',i2+1989,iregn,igp,icl,ildv,BatPackSize(i2+1989,icl,igp,ildv)
                  WRITE(21,'(a,",",5(i4,","),f10.6)')'fuel_cost',i2+1989,iregn,igp,icl,ildv,FLCOST(igp,ildv,icl,iregn,i2+1989)
                  WRITE(21,'(a,",",5(i4,","),f10.2)')'mpg',i2+1989,iregn,igp,icl,ildv,femmpg(igp,icl,i2+1989,ildv)
                  WRITE(21,'(a,",",5(i4,","),f10.6)')'hpwgt',i2+1989,iregn,igp,icl,ildv,ACCL(igp,ildv,icl,iregn,i2+1989)
                  WRITE(21,'(a,",",5(i4,","),f10.6)')'hp',i2+1989,iregn,igp,icl,ildv,femhp(igp,icl,i2+1989,ildv)
                  WRITE(21,'(a,",",5(i4,","),f10.2)')'wgt',i2+1989,iregn,igp,icl,ildv,femwgt(igp,icl,i2+1989,ildv)
                  WRITE(21,'(a,",",5(i4,","),f10.2)')'msrp',i2+1989,iregn,igp,icl,ildv,PSPR(igp,ildv,icl,iregn,i2+1989)
                  WRITE(21,'(a,",",5(i4,","),f10.6)')'mkt_share',i2+1989,iregn,igp,icl,ildv,APShrGrp(igp,icl,ildv,iregn,i2)
                  WRITE(21,'(a,",",5(i4,","),f10.3)')'sales_thou',i2+1989,iregn,igp,icl,ildv,ldv_sales(igp,icl,ildv,iregn,i2)*1000
                  WRITE(21,'(a,",",5(i4,","),f10.1)')'range',i2+1989,iregn,igp,icl,ildv,vrng(igp,ildv,icl,iregn,i2+1989)
                  WRITE(21,'(a,",",5(i4,","),f10.6)')'luggage',i2+1989,iregn,igp,icl,ildv,LUGG(igp,ildv,icl,iregn)
                  WRITE(21,'(a,",",5(i4,","),f10.6)')'fuel_avail',i2+1989,iregn,igp,icl,ildv,FAVL(ildv,iregn,i2+1989)
                  WRITE(21,'(a,",",5(i4,","),f10.4)')'atvcoef',i2+1989,iregn,igp,icl,ildv,x210(igp,icl,ildv,iregn,i2)
                  WRITE(21,'(a,",",5(i4,","),f10.1)')'nameplate',i2+1989,iregn,igp,icl,ildv,nameplate(igp,icl,i2+1989,ildv)
                  WRITE(21,'(a,",",5(i4,","),i10)')'grpflag',i2+1989,iregn,igp,icl,ildv,GRPFLAG(ILDV,ICL,IGP)
                enddo
              enddo
            enddo
          enddo
        enddo
      endif

!     Get equal-weighted (not sales-weighted) MSRPs and mpgs for luxury and non-luxury car and light truck 
!     I.e., assume an equal share of each powertrain is sold in each group (i.e., BEVs aren't more heavily-weighted to lux, HEVs not more heavily-weighted to mass-market).
!     This gives an idea of the average MSRP and mpg consumers SEE versus the average MSRP consumers BUY
!     Leave out exotics 
      if (eq_wgt_write) then
        price_eqwgt(:,:,:,:) = 0.0
        sales_temp(:,:,:,:) = 0.0
        sum_ldvsales_allLDV = 0.0
        do i2 = 34,mnumyr
          do ildv=1,maxldv
            if (ildv.le.2.or.ildv.eq.5.or.ildv.eq.6.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.16) then
              do igp=1,maxgroup
                if (eq_wgt_write_incl_exotics.and.igp.eq.5) CYCLE
                do icl=1,maxclass
                  if (fempri(igp,icl,i2+1989,ildv).gt.0.0) then 
                    sum_ldvsales_allLDV = sum(ldv_sales(igp,icl,1:maxldv,mnumcr,i2))
                    sales_temp(grp_to_luxPVLT(igp),icl,ildv,i2) = sales_temp(grp_to_luxPVLT(igp),icl,ildv,i2) + sum_ldvsales_allLDV
                    price_eqwgt(grp_to_luxPVLT(igp),icl,ildv,i2) = price_eqwgt(grp_to_luxPVLT(igp),icl,ildv,i2) + sum_ldvsales_allLDV * fempri(igp,icl,i2+1989,ildv)
                    if(sum(ldv_sales(igp,icl,:,mnumcr,i2)).gt.0.0) mpg_eqwgt(grp_to_luxPVLT(igp),icl,ildv,i2) = mpg_eqwgt(grp_to_luxPVLT(igp),icl,ildv,i2) + sum_ldvsales_allLDV/femmpg(igp,icl,i2+1989,ildv)
                  endif
                enddo
              enddo
            endif
          enddo
        enddo            
        
        WRITE(21,*)'Equal-weighted LDV MSRP (2024USD)'
        if (eq_wgt_write_incl_exotics) WRITE(21,*)'Exotics (group 5) included in car luxury (2)'
        if (.not.eq_wgt_write_incl_exotics) WRITE(21,*)'Exotics not included'
        do i2=34,mnumyr
          do igp=1,4
            do icl=1,maxclass
              do ildv=1,maxldv
                if (ildv.le.2.or.ildv.eq.5.or.ildv.eq.6.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.16) then
                  if (sales_temp(igp,icl,ildv,i2).gt.0.0) price_eqwgt(igp,icl,ildv,i2) = price_eqwgt(igp,icl,ildv,i2)/sales_temp(igp,icl,ildv,i2)
                endif
              enddo
              WRITE(21,'(3(i4,","),7(f9.2,","))')i2+1989,igp,icl,price_eqwgt(igp,icl,[1,2,5,6,7,15,16],i2)*mc_jpgdp(35)/mc_jpgdp(1)
            enddo
          enddo
        enddo
        
        WRITE(21,*)'Equal-weighted LDV Tested MPG not incl. exotics'
        if (eq_wgt_write_incl_exotics) WRITE(21,*)'Exotics (group 5) included in car luxury (2)'
        if (.not.eq_wgt_write_incl_exotics) WRITE(21,*)'Exotics not included'
        do i2=34,mnumyr
          do igp=1,4
            do icl=1,maxclass
              do ildv=1,maxldv
                if (ildv.le.2.or.ildv.eq.5.or.ildv.eq.6.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.16) then
                  if (sales_temp(igp,icl,ildv,i2).gt.0.0) mpg_eqwgt(igp,icl,ildv,i2) = sales_temp(igp,icl,ildv,i2)/mpg_eqwgt(igp,icl,ildv,i2)
                endif
              enddo
              WRITE(21,'(3(i4,","),7(f9.2,","))')i2+1989,igp,icl,mpg_eqwgt(igp,icl,[1,2,5,6,7,15,16],i2)
            enddo
          enddo
        enddo
      
        WRITE(21,*)'LDV sales by group, size class (thousands)'
        do i2=34,mnumyr
          do icl=1,maxclass
            WRITE(21,'(3(i4,","),7(f10.2,","))')i2+1989,1,icl,(ldv_sales(1,icl,[1,2,5,6,7,15,16],mnumcr,i2)+ldv_sales(2,icl,[1,2,5,6,7,15,16],mnumcr,i2)+ldv_sales(3,icl,[1,2,5,6,7,15,16],mnumcr,i2))*1000
            WRITE(21,'(3(i4,","),7(f10.2,","))')i2+1989,2,icl,ldv_sales(4,icl,[1,2,5,6,7,15,16],mnumcr,i2)*1000
            WRITE(21,'(3(i4,","),7(f10.2,","))')i2+1989,3,icl,(ldv_sales(6,icl,[1,2,5,6,7,15,16],mnumcr,i2)+ldv_sales(7,icl,[1,2,5,6,7,15,16],mnumcr,i2)+ldv_sales(8,icl,[1,2,5,6,7,15,16],mnumcr,i2) &
                                                               +ldv_sales(9,icl,[1,2,5,6,7,15,16],mnumcr,i2)+ldv_sales(10,icl,[1,2,5,6,7,15,16],mnumcr,i2))*1000
            WRITE(21,'(3(i4,","),7(f10.2,","))')i2+1989,4,icl,ldv_sales(11,icl,[1,2,5,6,7,15,16],mnumcr,i2)*1000
          enddo
        enddo 
      endif
    
      if (classflagwrite) then
        WRITE(21,*)'classflag -- vehicle availability in 2050'  
        WRITE(21,*)'grp,ildv,val'
        do igp=1,maxgroup
          do ildv=1,maxldv
            WRITE(21,'(2(i2,","),8(l4,","))')igp,ildv,classflag(igp,1:maxclass,ildv)
          enddo
        enddo
      endif
    
    endif

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
      REAL          current_vmt                                             ! Temporary annual vmt/vehicle used in payback calculation
      REAL          fuel_savings_per_mile                                   ! Temporary fuel savings per mile used in payback calculation
      REAL          CFE, CFE_INV                                            ! Temporary mpg and inverse to calculate fuel savings in payback calc
      
! ... Sumcheck holds the IDs of techs that together should not sum to more than one.
! ... It is formatted to allow up to 15 IDs for a single check, with up to 20 checks
! ... total.  Additional checks can be accomodated by either adding new IDs to unused
! ... array elements or increasing the second array dimension (and the subsequent loop
! ... index parameter accordingly.  Values of -9 indicate "No Data."

      INTEGER       SUMCHECK(15,20)
      REAL          CHECKSUM
      REAL FUNCMAX
      EXTERNAL FUNCMAX
	  DATA SUMCHECK / 1,  2,  3,  4,  5, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 01 IDs (material substitution)      ! MDRAEO2026 move to input file
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
                     
      IF (FEM_PASS .EQ. 1) TEC_ORNL = 0.0

!...  If this is the FEM base year or earlier, jump over the core FEM calculations.  However, there are several periperhal calculations that should 
!...  be implemented even for pre-FEM estimate years.  Such calculations generally involve setting up lagged parameters and validating historic year
!...  inputs.  For example, the calculation of vehicle range and other NEMS statistics requires the definition of FEM "current year" data regardless of 
!...  whether or not the current year is before or after the FEM base year.  For years up to the FEM base year, these data must first be extracted from
!...  the report writer or base year arrays since years through the FEM base year (XYR) are never actually processed through the main portion of FEMCALC.

!... On the first pass of the first TRAN iteration, all "current year" data (which are actually data from the final pass/iteration for the
!... previous year for all years after the FEM base year) must be copied into the "previous year" arrays so that they are available as the basis for the
!... new evaluation year updates.

      IF(CURITR .EQ. 1 .AND. FEM_PASS .EQ. 1 .AND. YRS .GT. XYR) THEN
        WHERE(classflag(1:MAXGROUP, 1:MAXCLASS, 1:MAXLDV))
          FE(1:MAXGROUP, 1:MAXCLASS, PREV, 1:MAXLDV)       = FE(1:MAXGROUP, 1:MAXCLASS, CURRENT, 1:MAXLDV)
          WEIGHT(1:MAXGROUP, 1:MAXCLASS, PREV, 1:MAXLDV)   = WEIGHT(1:MAXGROUP, 1:MAXCLASS, CURRENT, 1:MAXLDV)
          PRICE(1:MAXGROUP, 1:MAXCLASS, PREV, 1:MAXLDV)    = PRICE(1:MAXGROUP, 1:MAXCLASS, CURRENT, 1:MAXLDV)
          HP(1:MAXGROUP, 1:MAXCLASS, PREV, 1:MAXLDV)       = HP(1:MAXGROUP, 1:MAXCLASS, CURRENT, 1:MAXLDV)
          TANKSIZE(1:MAXGROUP, 1:MAXCLASS, PREV, 1:MAXLDV) = TANKSIZE(1:MAXGROUP, 1:MAXCLASS, CURRENT, 1:MAXLDV)
          RANGE(1:MAXGROUP, 1:MAXCLASS, PREV, 1:MAXLDV)    = RANGE(1:MAXGROUP, 1:MAXCLASS, CURRENT, 1:MAXLDV)
        END WHERE
        MKT_PEN(1:MAXGROUP, 1:MAXCLASS, 1:NUMTECH, PREV, 1:MAXLDV) = MKT_PEN(1:MAXGROUP, 1:MAXCLASS, 1:NUMTECH, CURRENT, 1:MAXLDV)
        
!...	set previous year data only once for each evaluation year	to provide necessary cross-iteration stability	
        DO ILDV=1,MAXLDV 
          if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) then
            DO IGP=1,MAXGROUP                          
              DO ICL=1,MAXCLASS
			    if(classflag(igp,icl,ildv)) ElecSysIncCost(ICL,IGP,PREV,ILDV) = ElecSysIncCost(ICL,IGP,CURRENT,ILDV)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDIF
  
!...  For each technology, the expected fuel savings associated with incremental fuel economy impacts is calculated.  This calculation occurs
!...  below within a technology evaluation loop, but the fuel costs on which the calculation is dependent are fixed annually so that continual 
!...  recalculation within the technology loop is redundant.  Accordingly, the basic fuel cost calculations are included here.  Nominally, fuel
!...  costs three years ago and the annual rate of fuel price change are used to estimate expected dollar savings.  However, since prices can 
!...  spike and since manufacturing decisions will not be based on one-year spikes, the "three year ago" and "rate of change" prices used for this
!...  calculation are actually the "five year running average price" and the "difference between the three year ago five year average price and 
!...  the four year ago five year average price."  Thus, the effect of short term transients is buffered.
      IF(YRS .GT. XYR) THEN              ! lagged parameters are not needed until after the FEM base year
        FIVEYR_FUELCOST    = 0.0
        FIVEYR_FUELCOST(1) = SUM(PMGTR90_D_(11,YRS-7:YRS-3))/5.0
        FIVEYR_FUELCOST(2) = SUM(PMGTR90_D_(11,YRS-8:YRS-4))/5.0
        PSLOPE = MAX(0.0, FIVEYR_FUELCOST(1) - FIVEYR_FUELCOST(2))
      ENDIF

!...  Set payback period and discount rates for technology adoption
      if(ENFORCE_MY27REGS.eq.1) then
        PAYBACK = 8 ! 6.5
        DISCOUNT = 0.05 !0.075
      else
        PAYBACK = 3
        DISCOUNT = 0.1
      endif

!...  Initiate FEM processing loop (loop through each fuel type, vehicle group, vehicle class, and fuel economy technology).
      DO ILDV=1,MAXLDV
        DO IGP=1,MAXGROUP

!...    In the 1st call to FEMCALC, CAFEPASS(IGP) should always be FALSE (i.e., none of the nine CAFE groups have demonstrated compliance).  In 
!...	subsequent calls values may be TRUE or FALSE depending on the compliance status of each CAFE group.

          IF(FEM_PASS.ge.2.and.CAFEPASS(IGP)) CYCLE

          DO ICL=1,MAXCLASS
            IF(.NOT. classflag(igp,icl,ILDV)) CYCLE    ! skip loop if no vehicles in the class
!...        Set vehicle type index for correct reference to arrays that are type, rather than group specific.
            IVTYP=GrpMap(IGP)
!...        Set the initial estimates for each years fuel efficiency, weight, price,
!...        and fuel tank size equal to the previous year's values before
!...        considering new technologies.

            FE(IGP,ICL,CURRENT,ILDV)       = FE(IGP,ICL,PREV,ILDV)
            WEIGHT(IGP,ICL,CURRENT,ILDV)   = WEIGHT(IGP,ICL,PREV,ILDV)
            PRICE(IGP,ICL,CURRENT,ILDV)    = PRICE(IGP,ICL,PREV,ILDV)
            HP(IGP,ICL,CURRENT,ILDV)       = HP(IGP,ICL,PREV,ILDV)
            TANKSIZE(IGP,ICL,CURRENT,ILDV) = TANKSIZE(IGP,ICL,PREV,ILDV)

!...		Jump over core FEM processing if this is FEM base year or earlier, but perform market share sum checks for related
!...		technologies to ensure reliable input data.  Note also that this jump could be performed prior to the attribute 
!...		resets (i.e. CURRENT = PREV) above, but the resets are run first to make data in and out of FEM "perfectly" stable 
!...		across iterations.  Output would be stable either way (as calibration factors "go to one" for 2nd and later iterations),
!... 		but this approach makes debugging easier/cleaner.
            IF(YRS .LE. XYR) GO TO 1100  

!...		For electric power train vehicles subtract last year's electric storage cost from vehicle price.
            if((ILDV.ge.4.and.ILDV.le.8).or.ILDV.ge.13) then
			  if(price(IGP,ICL,current,ildv).ne.0.0) then
                price(IGP,ICL,CURRENT,ILDV) = price(IGP,ICL,CURRENT,ILDV)-ElecSysIncCost(ICL,IGP,PREV,ILDV)
              endif
            endif

!...		Calculate possible market share in the absence of any engineering notes

!... 		Initialize the value of PERF_COEFF, the parameter used to constrain the incremental value of additional vehicle performance.  
!...		This parameter, which is independent of technology and can thus be set prior to beginning the main technology evaluation loop,
!...		increases as performance increases so that the incremental value of additional performance declines.  Since the value of
!...		performance is based on 1990 data, the consumer performance demand function also uses a base of 1990.  However, since the base 
!...		data year is XYR, the demand that has already accrued must be accounted for through the use of parameter USEDCAP.
            IF(WEIGHT(IGP,ICL,BASE,ILDV) .NE. 0.0) THEN
              HP_WGT_BASE = HP(IGP,ICL,BASE,ILDV) / WEIGHT(IGP,ICL,BASE,ILDV)
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
              WRITE(*,*) '   WEIGHT(IGP,ICL,BASE,ILDV) = ',WEIGHT(IGP,ICL,BASE,ILDV)
              WRITE(*,*)
              WRITE(*,*) ' --- and the associated numerator was:'
              WRITE(*,*)
              WRITE(*,*) '   HP(IGP,ICL,BASE,ILDV)     = ',HP(IGP,ICL,BASE,ILDV)
			  WRITE(*,*) '   classflag(igp,icl,ILDV)   = ',classflag(igp,icl,ildv)
              STOP 505
            ENDIF

            IF(WEIGHT(IGP,ICL,CURRENT,ILDV) .NE. 0.0) THEN
              HP_WGT = HP(IGP,ICL,CURRENT,ILDV) / WEIGHT(IGP,ICL,CURRENT,ILDV)
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
              WRITE(*,*) '   WEIGHT(IGP,ICL,CURRENT,ILDV) = ',WEIGHT(IGP,ICL,CURRENT,ILDV)
              WRITE(*,*)
              WRITE(*,*) ' --- and the associated numerator was:'
              WRITE(*,*)
              WRITE(*,*) '   HP(IGP,ICL,CURRENT,ILDV)     = ',HP(IGP,ICL,CURRENT,ILDV)
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
            MKT_PEN(IGP,ICL,1:NUMTECH,CURRENT,ILDV) = MKT_PEN(IGP,ICL,1:NUMTECH,PREV,ILDV)
            MMAX(1:NUMTECH) = MKT_MAX(ICL,IGP,1:NUMTECH,ILDV)
            MKT_PERF(1:NUMTECH) = 0.0
            MKT_FUEL(1:NUMTECH) = 0.0
            ACTUAL_MKT(1:NUMTECH) = 0.0
            FUELSAVE(1:NUMTECH) = 0.0

!           Calculate key parameters (that don't change by technology) used in the fuel savings equation            
            CFE = FE(IGP,ICL,PREV,ILDV)
            CFE_INV = 0.0
            if (CFE.gt.0.0) CFE_INV = 1/CFE
            
            DO ITECH=1,NUMTECH

!...		  Skip non-applicable technologies.
              IF(.NOT. TECH_APPLIC(ITECH,IVTYP,ILDV)) CYCLE

!...		  Set tech penetration limiting parameter (OLD_PMAX).  Penetration curve should be accelerated if CAFE is not met, therefore OLD_PMAX
!...		  should be set (updated) on both the first and second passes through FEM.  No further acceleration is appropriate on pass three, 
!...		  which simply "trades" performance gains for fuel economy.  Pass three constraints are handled further on in the code by restricting
!...		  updates to OLD_PMAX.  OLD_PMAX must also be reset for each iteration, so OLD_PMAX(...,1) is used to hold the first iteration/first pass
!...		  value for each year so that it can be restored as necessary.  Within year processing is controlled via OLD_PMAX(...,2).
              IF(CURITR .EQ. 1 .AND. FEM_PASS .EQ. 1) THEN    ! 1st iter/1st pass -- store reset value (last value for year-1)
                OLD_PMAX(ICL,IGP,ITECH,ILDV,1) = OLD_PMAX(ICL,IGP,ITECH,ILDV,2)
              ELSEIF(FEM_PASS .EQ. 1) THEN                    ! subsequent iter/1st pass -- restore 1st iter/1st pass value
                OLD_PMAX(ICL,IGP,ITECH,ILDV,2) = OLD_PMAX(ICL,IGP,ITECH,ILDV,1)
              ENDIF

              IF(YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE

!... 		  Calculate expected fuel savings associated with incremental fuel economy.
!...		  Use the last VMT schedule available (VMTYR). Assume gas ICE VMT (need consistent assumption)
              IF (CFE.gt.0.0) THEN
                fuel_savings_per_mile = CFE_INV - (1/((1+DEL_FE(ITECH,IVTYP))*CFE))
                DO I=1,PAYBACK
                  current_vmt = 0.0
                  if(IGP.le.cargrp) current_vmt = PVMT(I,VMTYR-1989,mnumcr,1)	
                  if(IGP.ge.ltkgrp) current_vmt = LVMT(I,VMTYR-1989,mnumcr,1)
			 	 
                  PRICE_EX(I) = PSLOPE * (REAL(I)+2.0) + FIVEYR_FUELCOST(1)
                  FUELSAVE(ITECH) = FUELSAVE(ITECH) + current_vmt * fuel_savings_per_mile * &
                                    PRICE_EX(I) * (1+DISCOUNT)**(-I)
                ENDDO
              ENDIF

!...          calculate incremental technology cost of specific technology starting in base year technology
!...          update: this year flag must be updated if technology base year attributes are set to a year other than 2010
!...          absolute technology cost
              TECHCOST(ITECH) = DEL_COSTABS(ITECH,IVTYP)

! ...         absolute weight-based technology cost
              TECHCOST(ITECH) = TECHCOST(ITECH) + &
                                (DEL_COSTWGT(ITECH,IVTYP) * DEL_WGTABS(ITECH,IVTYP) * SIGN(1.0, DEL_WGTABS(ITECH,IVTYP)))

! ...         weight-based technology cost
              TECHCOST(ITECH) = TECHCOST(ITECH) + &
                                (DEL_COSTWGT(ITECH,IVTYP) * DEL_WGTWGT(ITECH,IVTYP) * SIGN(1.0, DEL_WGTWGT(ITECH,IVTYP)) * WEIGHT(IGP,ICL,CURRENT,ILDV))

!...          skip if technology cost is 0 
              if(TECHCOST(ITECH) .lt. 0.0) GO TO 500
              
!...          apply time-based learning to technology cost
              LEARN_COST_MULTIPLIER(1:4) = 1.0

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

!... 		  Save technology cost for use in subroutine.
  500         TEC_ORNL(ICL,IGP,ITECH,ILDV) = TECHCOST(ITECH)

!...		  Estimate the value of performance associated with the technology.  Scale the the value of performance 
!...		  downward (using PERF_COEFF) as HP/WGT increases to reflect the decreasing incremental value of more performance.
              VAL_PERF(ITECH) = 0.0
              IF(CFE .NE. 0.0) VAL_PERF(ITECH) = VALUEPERF(ICL,IGP)*PERF_COEFF*INC_GR_REGN(11,YRS) *       &
                                                (1+DEL_FE(ITECH,IVTYP))*PMGTR90_INVGR(11,YRS)*DEL_HP(ITECH,IVTYP)
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

              IF(FEM_PASS.EQ.1 .OR. (FEM_PASS.EQ.2 .AND. PASS2.EQ.1)) THEN
                OLD_PMAX(ICL,IGP,ITECH,ILDV,2) = &
                FUNCMAX(MKT_PEN(IGP,ICL,ITECH,PREV,ILDV),OLD_PMAX(ICL,IGP,ITECH,ILDV,2))
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
              ACTUAL_MKT(ITECH) = MAX(MKT_PEN(IGP,ICL,ITECH,PREV,ILDV),ACTUAL_MKT(ITECH))
              ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),MMAX(ITECH),1.0)
            ENDDO   ! end technology (ITECH) loop

!... 		Apply mandatory and supersedes engineering notes.
            DO ITECH=1,NUMTECH
              IF(YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE
              IF (NUM_MAN.gt.0) THEN
                DO INOTE=1,NUM_MAN      ! loop through mandatory notes
                  IF(MANDYEAR(1,INOTE) .EQ. ITECH .AND. YRS .GE. MANDYEAR(2,INOTE)) THEN

!...			    If a non-econometric technology (i.e., MAND_ORIDE is TRUE), zero any econometrically calculated market share.
                    IF(MAND_ORIDE(INOTE)) ACTUAL_MKT(ITECH) = 0.0
!...			    If the number of phase-in years is between 0 and 1, adopt the full market share immediately.  Since the maximum market penetration
!...			    allowance can vary by vehicle class, the actual market share logic must consider the mandatory share, not in isolation, but in 
!...			    conjunction with the maximum allowable share for the vehicle class.
                    IF(MANDYEAR(3,INOTE) .LE. 1) THEN
                      ACTUAL_MKT(ITECH) = MAX(MANDMKSH(INOTE),ACTUAL_MKT(ITECH))
                      ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),MKT_MAX(ICL,IGP,ITECH,ILDV))
                    ELSE
!...			    If the number of phase-in years is greater than 1, adopt a proportional share of the total mandatory share each year.  Since both the
!...			    base and maximum market penetrations can vary by vehicle class, the actual market share logic must adopt annual shares in proportion
!...			    to the allowable market share spread for each vehicle class, with the minimum market share defined by the base share for the class.
                      PHASEIN  = MIN(1.0,(REAL(YRS-MANDYEAR(2,INOTE)))/REAL(MANDYEAR(3,INOTE)))
                      PHASESHR = MANDMKSH(INOTE) * PHASEIN
                      CLASSSHR = MKT_PEN(IGP,ICL,ITECH,BASE,ILDV) +     &
                                (PHASESHR*(MKT_MAX(ICL,IGP,ITECH,ILDV)- &
                                           MKT_PEN(IGP,ICL,ITECH,BASE,ILDV)))
                      ACTUAL_MKT(ITECH) = MAX(ACTUAL_MKT(ITECH),CLASSSHR)
                      ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),MKT_MAX(ICL,IGP,ITECH,ILDV))
                    ENDIF
                  ENDIF
                ENDDO                   ! end mandatory note loop
              ENDIF
		
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
            IF (NUM_REQ.gt.0) THEN
              DO ITECH=1,NUMTECH        ! loop through and apply required engineering notes
                IF(YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE
                REQUIRED = .FALSE.
                REQ_MKT  = 0.0
                DO INOTE=1,NUM_REQ
                  IF(REQUIRES(1,INOTE) .EQ. ITECH) THEN
                    REQUIRED = .TRUE.
                    REQ_MKT  = REQ_MKT + MKT_PEN(IGP,ICL,REQUIRES(2,INOTE),CURRENT,ILDV)
                  ENDIF
                ENDDO
                IF(REQUIRED) THEN
                  REQ_MKT = MIN(REQ_MKT,1.0)
                  ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),REQ_MKT)
                ENDIF
              ENDDO   ! end technology (ITECH) loop for required notes
            ENDIF
            
            MKT_PEN(IGP,ICL,1:NUMTECH,CURRENT,ILDV) = ACTUAL_MKT(1:NUMTECH)

!...		Loop through and apply the synergy engineering notes
            IF (NUM_SYN.gt.0) THEN
              DO ITECH=1,NUMTECH        
                IF(YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE
                SYNERGY_LOSS(ITECH) = 0.0
!...		    Market share affected by synergy effects between two technologies is estimated as the probabilistic overlap between the market shares
!...		    of the two technologies. Mathematically, this market share is expressed as the product of the market shares of the two technologies.  
!...		    The incremental market share overlap for a single year is equal to the cumulative estimated overlap (based on cumulative estimated market 
!...		    penetrations) for the current year minus the cumulative estimated overlap for the previous year.  Note also, that the input value of 
!...		    is negative so that the estimated synergy loss will also be negative and should be treated as an additive parameter.
                DO INOTE=1,NUM_SYN
                  IF(SYNERGY(1,INOTE) .EQ. ITECH) THEN
                    DELTA_MKT = (MKT_PEN(IGP,ICL,ITECH,CURRENT,ILDV) *             &
                                 MKT_PEN(IGP,ICL,SYNERGY(2,INOTE),CURRENT,ILDV)) - &
                                (MKT_PEN(IGP,ICL,ITECH,PREV,ILDV) *                &
                                 MKT_PEN(IGP,ICL,SYNERGY(2,INOTE),PREV,ILDV))
                  
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
            ENDIF

!...		Repeat the technology loop one last time to aggregate the impacts of changes in technology in market share.
            TECH_ADJHP  = 0.0
            HP_GIVEBACK = 0.0

            DO ITECH=1,NUMTECH
!...		  skip technology not applicable to vehicle type (by fuel type)
              IF(.NOT. TECH_APPLIC(ITECH,IVTYP,ILDV)) CYCLE
              IF (YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE
              DELTA_MKT = MKT_PEN(IGP,ICL,ITECH,CURRENT,ILDV) - &
                          MKT_PEN(IGP,ICL,ITECH,PREV,ILDV)
!...		  For pure non-econometric consumer driven techs (e.g., forced size/weight increase), set DELTA_MKT to zero if this is a CAFE pass 
!...		  through FEMCALC (i.e., FEM_PASS > 1).  Do not change the actual MKT_PEN values since this will screw up the tech phase-in for subsequent
!...		  years.  Setting DELTA_MKT will nullify both the FE and WGT impacts of these techs for this year without creating compensating changes
!...		  in subsequent years.
              FE(IGP,ICL,CURRENT,ILDV) = FE(IGP,ICL,CURRENT,ILDV)+FE(IGP,ICL,PREV,ILDV)*((DELTA_MKT * DEL_FE(ITECH,IVTYP))+SYNERGY_LOSS(ITECH))
              
              IF (DELTA_MKT.ne.0.0) THEN
                WEIGHT(IGP,ICL,CURRENT,ILDV) = WEIGHT(IGP,ICL,CURRENT,ILDV) + (DELTA_MKT * (DEL_WGTABS(ITECH,IVTYP) +       &
                                               (WEIGHT(IGP,ICL,CURRENT,ILDV) * DEL_WGTWGT(ITECH,IVTYP))))
                
                PRICE(IGP,ICL,CURRENT,ILDV) = PRICE(IGP,ICL,CURRENT,ILDV) + (DELTA_MKT * TECHCOST(ITECH))
                
!...		    Calculate annual horsepower adjustment due to technology introduction alone. This is only part of overall horsepower adjustment,
!...		    so final horsepower is calculated below, outside the technology loop.
                TECH_ADJHP = TECH_ADJHP + (DELTA_MKT * DEL_HP(ITECH,IVTYP))
              ENDIF
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
                CHECKSUM = CHECKSUM + MKT_PEN(IGP,ICL,SUMCHECK(J,I),CURRENT,ILDV)
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
                WRITE (*,*) 'Market Share     = ',MKT_PEN(IGP,ICL,SUMCHECK(k,i),CURRENT,ILDV)
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
			if(price(igp,icl,current,ildv).ne.0.0.and.classflag(igp,icl,ildv)) then
              SELECT CASE (ILDV)
                CASE (4, 7, 15)
                  CALL EVCALC (yrs,igp)
                CASE (8, 16)
                  CALL HEVCALC (yrs,igp)
                CASE (5, 6)
                  CALL PHEVCALC (yrs,igp)
                CASE (13, 14)
                  CALL FCCALC (yrs,igp)
              END SELECT
			endif

!...		Initially set horsepower for constant performance level based on last year's power to weight ratio.
            IF(WEIGHT(IGP,ICL,PREV,ILDV) .NE. 0.0) THEN
              HP(IGP,ICL,CURRENT,ILDV) = (HP(IGP,ICL,PREV,ILDV)/WEIGHT(IGP,ICL,PREV,ILDV)) * WEIGHT(IGP,ICL,CURRENT,ILDV)
            ELSE
              HP(IGP,ICL,CURRENT,ILDV) = 0.0
            ENDIF

!...		Estimate annual horsepower adjustment due to consumer performance demand. Consumer performance demand is adjusted downward as HP/WGT
!...		ratio increases so that performance gains cannot continue indefinitely.  Initial demand coefficients are controlled via user input 
!...		parameter PERFFACT (VEHPERFFAC in TRNLDV.XML) and demand caps via user input parameter PERFCAP (VEHPERFCAP in TRLDV.XML).
!...        Note that BEVs and PHEVs do not have horsepower adjustments due to consumer performance demand.
            if(ILDV.lt.4.or.(ILDV.ge.9.and.ILDV.le.12).or.ILDV.eq.16) then
              IF(INC90_D_NP(11,YRS-1)                     .NE. 0.0 .AND. &
                PRICE(IGP,ICL,CURRENT,ILDV)               .NE. 0.0 .AND. &
                FE(IGP,ICL,PREV,ILDV)                     .NE. 0.0 .AND. &
                WEIGHT(IGP,ICL,BASE,ILDV)                 .NE. 0.0 .AND. &
                WEIGHT(IGP,ICL,CURRENT,ILDV)              .NE. 0.0 .AND. &
                PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED  .NE. 0.0) THEN

!               Growth in consumer HP demand for HEVs is pinned to non-hybrid gasoline
                if (ILDV.eq.16) then
                  PERF_ADJHP = ((INC_GR_REGN(11,YRS) ** 0.9) * &
                               ((PRICE(IGP,ICL,PREV,1)/PRICE(IGP,ICL,CURRENT,1)) ** 0.9) * &
                               ((FE(IGP,ICL,CURRENT,1)/FE(IGP,ICL,PREV,1)) ** 0.2) * &
                               (PMGTR90_INVGR(11,YRS) ** 0.2)) - 1.0
                  hp(IGP,ICL,current,ILDV) = hp(IGP,ICL,current,ILDV)*vhp_adj(ICL,IGP,current,1) 

!               Otherwise, use the attributes of the current powertrain
                else
                  PERF_ADJHP = ((INC_GR_REGN(11,YRS) ** 0.9) * &
                               ((PRICE(IGP,ICL,PREV,ILDV)/PRICE(IGP,ICL,CURRENT,ILDV)) ** 0.9) * &
                               ((FE(IGP,ICL,CURRENT,ILDV)/FE(IGP,ICL,PREV,ILDV)) ** 0.2) * &
                               (PMGTR90_INVGR(11,YRS) ** 0.2)) - 1.0

                  if(weight(IGP,ICL,PREV,ILDV).NE. 0.0) then
                    vcw_adj = weight(IGP,ICL,CURRENT,ILDV)/weight(IGP,ICL,PREV,ILDV)-1.0
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

                  hp(IGP,ICL,current,ILDV) = hp(IGP,ICL,current,ILDV)*vhp_adj(ICL,IGP,current,ILDV) 

                endif

                HP_WGT = 0.0			  
                if(WEIGHT(IGP,ICL,CURRENT,ILDV).ne.0.0) HP_WGT = HP(IGP,ICL,CURRENT,ILDV) / WEIGHT(IGP,ICL,CURRENT,ILDV)
                PERF_COEFF  = 1.0 - ((HP_WGT- HP_WGT_BASE + DEMAND_USED) / (PERFCAP(ICL,IGP) - HP_WGT_BASE + DEMAND_USED))

!...		    Print warning message if PERF_COEFF takes on a value above one or below zero. Since WEIGHT is already adjusted, but horsepower is
!...		    not, allow a small buffer above one in the early years of the projection and a small buffer below zero in the latter years of the 
!...		    projection to avoid warnings due solely to "noise" type changes in the initial HP/WGT ratio estimate for each projection year.  
!...		    However,do not maintain this buffer in the actual PERF_ADJHP scaling algorithm.  Instead, reset ANY value above one or below zero.
                UPPER_BUFFER = 1.0 + (MAX(XYR+1 - YRS,0) * 0.01)
                LOWER_BUFFER = 0.0 - (MAX(YRS - XYR+11,0) * 0.01)
              
!                IF(PERF_COEFF .GT. UPPER_BUFFER .OR. PERF_COEFF .LT. LOWER_BUFFER) THEN
!                 WRITE (21,*)
!                 WRITE (21,*) 'Consumer Performance Coefficient is'
!                 WRITE (21,*) 'less than Zero or greater than One.'
!                 WRITE (21,*)
!                 WRITE (21,*) '                   Year = ',YRS
!                 WRITE (21,*) '         NEMS Iteration = ',CURITR
!                 WRITE (21,*) '               FEM Pass = ',FEM_PASS
!                 WRITE (21,*) '          Vehicle Group = ',IGP
!                 WRITE (21,*) '          Vehicle Class = ',ICL
!                 WRITE (21,*) '              Fuel Type = ',ILDV
!                 WRITE (21,*)
!                 WRITE (21,*) 'Value before Adjustmemt = ',PERF_COEFF
!                 WRITE (21,*)
!                 WRITE (21,*) 'Coefficient has been Reset (to 0 or 1).'
!                 WRITE (21,*)
!                ENDIF

                PERF_COEFF  = MIN(1.0,PERF_COEFF)
                PERF_COEFF  = MAX(0.0,PERF_COEFF)
                PERF_ADJHP  = PERF_ADJHP * PERFFACT(ICL,IGP) * PERF_COEFF

!...		    If this is an ultra CAFE pass (i.e., FEM_PASS = 3), zero any consumer HP demand at this point, but wait until after the remaining HP/WGT 
!...		    consistency checks are performed to restrict tech-driven HP.  Note, some consumer demand may be readded below if the required minimum 
!...		    HP/WGT ratio is not maintained.  Since a minimum HP/WGT ratio is considered essential for driveability, a minimum HP requirement will
!...		    not be waived even under a third pass CAFE scenario.
                IF (FEM_PASS .EQ. 3) PERF_ADJHP = MIN(PERF_ADJHP,0.0)
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
				WRITE(*,*) '   PRICE(igp,icl,CURRENT,ILDV)              = ',PRICE(igp,icl,CURRENT,ILDV)
				WRITE(*,*) '   FE(IGP,ICL,PREV,ILDV)                    = ',FE(IGP,ICL,PREV,ILDV)
				WRITE(*,*) '   PMGTR90_D_(11,YRS)                       = ',PMGTR90_D_(11,YRS)
				WRITE(*,*) '   WEIGHT(IGP,ICL,BASE,ILDV)                = ',WEIGHT(IGP,ICL,BASE,ILDV)
				WRITE(*,*) '   WEIGHT(IGP,ICL,CURRENT,ILDV)             = ',WEIGHT(IGP,ICL,CURRENT,ILDV)
				WRITE(*,*) '   PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED = ',PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED
				WRITE(*,*)
				WRITE(*,*) ' --- and associated numerators were:'
				WRITE(*,*)
				WRITE(*,*) '   INC90_D_NP(11,YRS)                       = ',INC90_D_NP(11,YRS)
				WRITE(*,*) '   PRICE(igp,icl,PREV,ILDV)                 = ',PRICE(igp,icl,PREV,ILDV)
				WRITE(*,*) '   FE(IGP,ICL,CURRENT,ILDV)                 = ',FE(IGP,ICL,CURRENT,ILDV)
				WRITE(*,*) '   PMGTR90_D_(11,YRS-1)                     = ',PMGTR90_D_(11,YRS-1)
				WRITE(*,*) '   HP(IGP,ICL,BASE,ILDV)                    = ',HP(IGP,ICL,BASE,ILDV)
				WRITE(*,*) '   HP(IGP,ICL,CURRENT,ILDV)                 = ',HP(IGP,ICL,CURRENT,ILDV)
				WRITE(*,*) '   HP_WGT-HP_WGT_BASE+DEMAND_USED           = ',HP(IGP,ICL,CURRENT,ILDV)/WEIGHT(IGP,ICL,CURRENT,ILDV)-&
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
!               WRITE (21,*) '               FEM Pass = ',FEM_PASS
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
              IF(WEIGHT(IGP,ICL,CURRENT,ILDV) .NE. 0.0) THEN
                TEMP_HP = HP(IGP,ICL,CURRENT,ILDV) * (1.0 + TTL_ADJHP)
                HP_WGT  = TEMP_HP / WEIGHT(IGP,ICL,CURRENT,ILDV)
                IF(HP_WGT .GT. PERFCAP(ICL,IGP)) THEN
!                 WRITE (21,*)
!                 WRITE (21,*) 'Total HP adjustment exceeds HP/WGT max'
!                 WRITE (21,*)
!                 WRITE (21,*) '                   Year = ',YRS
!                 WRITE (21,*) '         NEMS Iteration = ',CURITR
!                 WRITE (21,*) '               FEM Pass = ',FEM_PASS
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

                  TEMP_HP = HP(IGP,ICL,CURRENT,ILDV) * (1.0 + TTL_ADJHP)
                  HP_WGT  = TEMP_HP / WEIGHT(IGP,ICL,CURRENT,ILDV)

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
              IF(WEIGHT(IGP,ICL,CURRENT,ILDV).NE.0.0.AND.WEIGHT(IGP,ICL,BASE,ILDV).NE.0.0.AND.HP(IGP,ICL,CURRENT,ILDV).NE.0.0) THEN
!               HP_WGT_MIN = 0.95 * (HP(IGP,ICL,BASE,ILDV)/WEIGHT(IGP,ICL,BASE,ILDV))
                HP_WGT_MIN = 0.9 * (HP(IGP,ICL,BASE,ILDV)/WEIGHT(IGP,ICL,BASE,ILDV))
                IF(ICL .EQ. 6 .AND. IGP .LE. 4) THEN
                  HP_WGT_MIN = MIN(HP_WGT_MIN,1.0/25.0)
                ELSE
                  HP_WGT_MIN = MIN(HP_WGT_MIN,1.0/30.0)
                ENDIF
                TEMP_HP = HP(IGP,ICL,CURRENT,ILDV) * (1.0 + TTL_ADJHP)
                HP_WGT = TEMP_HP / WEIGHT(IGP,ICL,CURRENT,ILDV)
                MIN_ADJHP = ((HP_WGT_MIN * WEIGHT(IGP,ICL,CURRENT,ILDV))/HP(IGP,ICL,CURRENT,ILDV)) - 1.0
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
!               WRITE (21,*) '               FEM Pass = ',FEM_PASS
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

!...	      Finally, if this is a third pass, take back all the tech driven HP demand except that required
!...	      to maintain the HP/WGT minimum.
              if(ILDV.lt.4.or.(ILDV.ge.9.and.ILDV.le.12))then
                if(igp.ne.4.and.igp.ne.5.and.igp.ne.11) then ! luxury vehicles
                  if(FEM_PASS.eq.3) then
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
                      WRITE(*,*) '         FEM Pass = ',FEM_PASS
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
                if(igp.ne.4.and.igp.ne.5.and.igp.ne.11) then ! luxury vehicles
                  if((TECH_MAX_ADJHP.gt.0.0.and.HP_GIVEBACK.gt.(TECH_MAX_ADJHP+ROUNDOFF_ERROR)).or. &
                     (TECH_MAX_ADJHP.le.0.0.and.ABS(HP_GIVEBACK).gt.(ABS(TECH_MAX_ADJHP)+roundoff_error))) then
                    WRITE(*,*)
                    WRITE(*,*) ' Error in HP Adjustment Logic'
                    WRITE(*,*) ' HP Giveback Exceeds Tech Max'
                    WRITE(*,*)
                    WRITE(*,*) '             Year = ',YRS
                    WRITE(*,*) '   NEMS Iteration = ',CURITR
                    WRITE(*,*) '         FEM Pass = ',FEM_PASS
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
              SIGN_TDM = 1
              IF(PERF_ADJHP .LT. 0.0) SIGN_TDM = -1
              ADJFE = (-0.220 * PERF_ADJHP) - (+0.560 * SIGN_TDM * PERF_ADJHP * PERF_ADJHP)

              HP(IGP,ICL,CURRENT,ILDV) = HP(IGP,ICL,CURRENT,ILDV)*(1+TTL_ADJHP)
              FE(IGP,ICL,CURRENT,ILDV) = FE(IGP,ICL,CURRENT,ILDV)*(1+ADJFE)

              PRICE(IGP,ICL,CURRENT,ILDV) = PRICE(IGP,ICL,CURRENT,ILDV) + PERF_ADJHP * VALUEPERF(ICL,IGP)
            
            endif
		  ENDDO   ! end vehicle class (ICL) loop
        ENDDO   ! end vehicle group (IGP) loop
      ENDDO   ! end vehicle fuel type (ILDV) loop

 5000 CALL FEMRANGE

! ... Assign FEM parms to report writer arrays
	  if(curcalyr.gt.xyr) then
        FEMMPG(1:maxgroup,1:maxclass,YRS,1:maxldv)  = FE(1:maxgroup,1:maxclass,CURRENT,1:maxldv)
        FEMWGT(1:maxgroup,1:maxclass,YRS,1:maxldv)  = WEIGHT(1:maxgroup,1:maxclass,CURRENT,1:maxldv)
        FEMPRI(1:maxgroup,1:maxclass,YRS,1:maxldv)  = PRICE(1:maxgroup,1:maxclass,CURRENT,1:maxldv)
        FEMHP(1:maxgroup,1:maxclass,YRS,1:maxldv)   = HP(1:maxgroup,1:maxclass,CURRENT,1:maxldv)
        FEMTSZ(1:maxgroup,1:maxclass,YRS,1:maxldv)  = TANKSIZE(1:maxgroup,1:maxclass,CURRENT,1:maxldv)
        FEMRNG(1:maxgroup,1:maxclass,YRS,1:maxldv)  = RANGE(1:maxgroup,1:maxclass,CURRENT,1:maxldv)
        FEMPEN(1:maxgroup,1:maxclass,1:NUMTECH,YRS,1:maxldv) = MKT_PEN(1:maxgroup,1:maxclass,1:NUMTECH,CURRENT,1:maxldv)

        IF (curcalyr.le.EPALYR) then
          CALL CALIBNHTSA
!...      Copy calibrated data back into "current year" arrays for use with next evaluation year
          FE(1:maxgroup,1:maxclass,CURRENT,1:maxldv)      = FEMMPG(1:maxgroup,1:maxclass,YRS,1:maxldv)
          WEIGHT(1:maxgroup,1:maxclass,CURRENT,1:maxldv)  = FEMWGT(1:maxgroup,1:maxclass,YRS,1:maxldv)
          HP(1:maxgroup,1:maxclass,CURRENT,1:maxldv)      = FEMHP(1:maxgroup,1:maxclass,YRS,1:maxldv)
          PRICE(1:maxgroup,1:maxclass,CURRENT,1:maxldv)   = FEMPRI(1:maxgroup,1:maxclass,YRS,1:maxldv)
          TANKSIZE(1:maxgroup,1:maxclass,CURRENT,1:maxldv)= FEMTSZ(1:maxgroup,1:maxclass,YRS,1:maxldv)
	
!...      Rerun range calculation using calibrated fuel economy
          CALL FEMRANGE
        ENDIF
        
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
    IMPLICIT NONE

      REAL, INTENT(IN)  :: OLDMKSH,OLDPMAX
      REAL              :: TPMAX
      REAL, PARAMETER   :: RETD(0:30) = (/0.000, 0.048, 0.096, 0.145, 0.193, 0.241, 0.289, 0.337, 0.385, 0.434, &
                                          0.482, 0.530, 0.578, 0.626, 0.674, 0.722, 0.771, 0.819, 0.869, 0.907, &
                                          0.936, 0.957, 0.973, 0.984, 0.992, 0.997, 1.000, 1.000, 1.000, 1.000, 1.000/)

      IF (OLDPMAX .EQ. 1.0) THEN                  ! return 100% if full market was previously allowed
        FUNCMAX = 1.0
        RETURN
      ENDIF

      TPMAX = MIN(OLDPMAX,1.0)
      
      DO I=1,30
        IF (OLDMKSH .LE. RETD(I-1) .AND. TPMAX .LT. RETD(I)) THEN
          FUNCMAX = RETD(I)
          RETURN
        ENDIF
      ENDDO
      FUNCMAX = MAX(0.0,TPMAX)      

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
    real	DiffLn
    REAL    cpm_logdiff(mnumcr), inc_logdiff(mnumcr),pri_logdiff(maxgroup,maxclass)   
    
!   Temporary variables for calcs
    REAL    temp_cls_shr,NUM1,DEN1,DEN2,sales_all_trks(mnumcr),sales_all_cars(mnumcr)
    
    sales_all_cars(1:mnumcr) = SUM(SUM(SUM(ldv_sales(1:cargrp,1:maxclass,1:maxldv,1:mnumcr,n),DIM=3),DIM=2),DIM=1)
    sales_all_trks(1:mnumcr) = SUM(SUM(SUM(ldv_sales(ltkgrp:maxgroup,1:maxclass,1:maxldv,1:mnumcr,n),DIM=3),DIM=2),DIM=1)

!...calculate national ldvs sales precentages by manufacturing group
    if (curcalyr.le.epalyr) then
      GrpShare(mnumcr,1:cargrp,n)        = SUM(SUM(cafesales(1:cargrp,1:maxclass,yrs,1:maxldv), DIM=3), DIM=2) / &
                                           sales_all_cars(mnumcr)
      GrpShare(mnumcr,ltkgrp:maxgroup,n) = SUM(SUM(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv), DIM=3), DIM=2) / &
                                           sales_all_trks(mnumcr)

!     fill regional grpshare values with US values through 2018
	  if(curcalyr.le.2018) then
        GrpShare(1:mnumcr-2,1:maxgroup,n) = SPREAD(GrpShare(mnumcr,1:maxgroup,n), DIM=1, NCOPIES=mnumcr-2)
!     calculate manufacturing group shares by region beginning 2019	  
      else
        do iregn=1,mnumcr-2
          GrpShare(iregn,1:cargrp,n)        = SUM(SUM(ldv_sales(1:cargrp,1:maxclass,1:maxldv,iregn,n), DIM=3), DIM=2) / &
                                              sales_all_cars(iregn)
          GrpShare(iregn,ltkgrp:maxgroup,n) = SUM(SUM(ldv_sales(ltkgrp:maxgroup,1:maxclass,1:maxldv,iregn,n), DIM=3), DIM=2) / &
                                              sales_all_trks(iregn)
        enddo
      endif
    else
      grpshare(1:mnumcr, 1:maxgroup, n) = grpshare(1:mnumcr, 1:maxgroup, n-1)
    endif	  

!...Go through each group. If the year is less than or equal to epalyr, then use the historical data
!...and calculate a class share. 
    class_share(1:mnumcr,1:maxclass,1:maxgroup,yrs) = 0.0
    do igp=1,maxgroup
!...  Use the historical cafe data.
      groupsum(igp) = sum(cafesales(igp,1:maxclass,yrs,1:maxldv))
      do icl=1,maxclass
	    if(groupsum(igp).ne.0.0) class_share(mnumcr,icl,igp,yrs)=sum(cafesales(igp,icl,yrs,1:maxldv))/groupsum(igp)
	  enddo
	enddo
	  
!...fill regional values with US values through 2018
	if(curcalyr.le.2018) then
	  class_share(1:mnumcr-2, 1:maxclass, 1:maxgroup, yrs) = &
        SPREAD(class_share(mnumcr, 1:maxclass, 1:maxgroup, yrs), DIM=1, NCOPIES=mnumcr-2)

!...calculate class shares by group by region
	elseif(curcalyr.ge.2019.and.curcalyr.le.epalyr) then
	  do iregn=1,mnumcr-2
		do igp=1,maxgroup	  
		  groupsum(igp) = sum(ldv_sales(igp,1:maxclass,1:maxldv,iregn,n)) 
		  do icl=1,maxclass
		    if(groupsum(igp).ne.0.0) class_share(iregn,icl,igp,yrs) = sum(ldv_sales(igp,icl,1:maxldv,iregn,n))/groupsum(igp)
		  enddo
	    enddo
	  enddo
	endif

!...values for subsequent projection years are the same as the last historical year, calculations made in READHIST
    if(curcalyr.gt.stockyr) then
!...  owner sales shares remain constant through projection period
      ownsalesshr(1:maxowner, 1:maxgroup, 1:maxclass, 1:maxldv, 1:mnumcr-2, n) = &
        ownsalesshr(1:maxowner, 1:maxgroup, 1:maxclass, 1:maxldv, 1:mnumcr-2, n-1)

!...  temp owner shares	  
      ownsaletemp(1:maxowner, 1:maxgroup, 1:maxclass, 1:mnumcr-2, n) = &
        ownsaletemp(1:maxowner, 1:maxgroup, 1:maxclass, 1:mnumcr-2, n-1)
	endif 

!...Use the econometric projection for yrs > stockyr
    if(curcalyr.gt.stockyr) then
      cpm_logdiff(1:mnumcr) = log(COSTMI(1:mnumcr,n-1)/COSTMI(1:mnumcr,n-2))
      inc_logdiff(1:mnumcr) = log((inc90_D_np(1:mnumcr,yrs)-13000.0)/(inc90_D_np(1:mnumcr,yrs-1)-13000.0))
      pri_logdiff(1:maxgroup,1:maxclass) = log(price(1:maxgroup,1:maxclass,current,gas)/price(1:maxgroup,1:maxclass,prev,gas))
      
	  ratio_byr=log(REAL(yrs-epalyr))
	  do igp=1,maxgroup
        do icl=1,maxclass
		  do iregn = 1,mnumcr
            if (iregn.eq.10) CYCLE
!           If gasoline vehicles existed in this group and size class in BOTH the current and previous years
            if(COSTMI(iregn,n-1).ge.0.0.and.inc90_D_np(iregn,yrs-1).gt.13000.0.and.price(igp,icl,prev,gas).gt.0.0.and.price(igp,icl,current,gas).gt.0.0) then
			
			  diffln = coef_a(ICL,IGP)*ratio_byr + &
                       coef_b(ICL,IGP)*cpm_logdiff(iregn) + &
					   coef_c(ICL,IGP)*inc_logdiff(iregn) + &
                       coef_p(ICL,IGP)*pri_logdiff(igp,icl)

              ratio_ln = diffln + log(max(class_share(iregn,ICL,IGP,epalyr),0.000001)/(1.0-max(class_share(iregn,ICL,IGP,epalyr),0.000001)))					
              ratio = exp(ratio_ln)
              class_share(iregn,icl,igp,yrs) = ratio/(1.0+ratio)
!           If there are ANY vehicles available in the current year, take the previous year's share
			elseif (ANY(price(igp,icl,current,:).gt.0.0)) then
              class_share(iregn,ICL,IGP,yrs) = class_share(iregn,ICL,IGP,yrs-1)
!           No vehicles, so no share
            else
              class_share(iregn,ICL,IGP,yrs) = 0.0
            endif
		  enddo
        enddo
!...    Normalize the shares
        do iregn=1,mnumcr
          if (iregn.eq.10) CYCLE
          temp_cls_shr = sum(class_share(iregn,1:maxclass,igp,yrs))
          if(temp_cls_shr.gt.0.0) then
            class_share(iregn,1:maxclass,IGP,yrs) = class_share(iregn,1:maxclass,IGP,yrs)/temp_cls_shr
          endif	          
        enddo
      enddo
	endif ! > stockyr

!...calculate manufacturer sales by group and class
    do icl=1,maxclass
	  do igp=1,maxgroup
        it=GrpMap(igp)
        mfr_sales(1:mnumcr-2,igp,icl,n) = newldvs(it,1:mnumcr-2,n)*GrpShare(1:mnumcr-2,igp,n)*class_share(1:mnumcr-2,icl,igp,yrs)
	  enddo
    enddo
	
    RETURN
    END SUBROUTINE CGSHARE

! ==========================================================================================================
! ... Subroutine TREG estimates regional fuel demand shares
! ==========================================================================================================
  SUBROUTINE TREG
  USE T_
  IMPLICIT NONE

    INTEGER       IR,IS
    REAL          inc_growth

!...Calculate regional shares of fuel demand
!   In historical years use SEDS data
    IF (curiyr.LE.msedyr) THEN
      DO IR=1,MNUMCR-2
        IF (QSELTR(11,N) .NE. 0.0) SEDSHREL(IR,N)=QSELTR(IR,N)/QSELTR(11,N)         ! MDRAEO2026 vectorize
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
!   In projection years, vary shares based on variation in income growth across regions
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
      SEDSHREL(IR,N)=SEDSHREL(IR,N)/SEDSHREL(mnumcr,N)               ! MDRAEO2026 vectorize
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

  RETURN
  END SUBROUTINE TREG

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
    REAL          SLOPE,INTERCEPT,IRA_Credit(MAXLDV,MNUMYR)
    integer 	  it,xldv

!...vehicle attribute calculations for the consumer choice model
    WHERE (PETTR(1:mnumcr-2,N) .EQ. 0.0) 
      PETTR(1:mnumcr-2,N) = PMGTR(1:mnumcr-2,N)*1.18
    END WHERE

!...FLEXSHR calculates VMT shares for flex- and bi-fuel vehicles, PctAF. And more recently PctPHEV20.
    CALL FLEXSHR

!...Fuel cost (unit = nominal cents/mile) >xyr jma do loop for phev by group
    FPRICE(1,1:mnumcr,YRS)  =  PMGTR(1:mnumcr,N)
    FPRICE(2,1:mnumcr,YRS)  =  HWYPDSTR(1:mnumcr,N)
    FPRICE(3,1:mnumcr,YRS)  =  MIN(PETTR(1:mnumcr,N),PMGTR(1:mnumcr,N))
    FPRICE(4,1:mnumcr,YRS)  =  chg_dist(1:mnumcr,3,2023-1989)*(PELP2CM(1:mnumcr,N) * CHGCSTMULT(3)) + chg_dist(1:mnumcr,1,2023-1989)*(PELPFCM(1:mnumcr,N)*CHGCSTMULT(1)) + chg_dist(1:mnumcr,2,2023-1989)*PELVHRS(1:mnumcr,N)
    FPRICE(5,1:mnumcr,YRS)  = (PctPHEV20(N)*PELVHRS(1:mnumcr,N)) + ((1.0-PctPHEV20(N))*PMGTR(1:mnumcr,N))
    FPRICE(6,1:mnumcr,YRS)  = (PctPHEV50(N)*PELVHRS(1:mnumcr,N)) + ((1.0-PctPHEV50(N))*PMGTR(1:mnumcr,N))
    FPRICE(7,1:mnumcr,YRS)  =  chg_dist(1:mnumcr,3,2023-1989)*(PELP2CM(1:mnumcr,N) * CHGCSTMULT(3)) + chg_dist(1:mnumcr,1,2023-1989)*(PELPFCM(1:mnumcr,N)*CHGCSTMULT(1)) + chg_dist(1:mnumcr,2,2023-1989)*PELVHRS(1:mnumcr,N)
    FPRICE(8,1:mnumcr,YRS)  =  HWYPDSTR(1:mnumcr,N)
    FPRICE(9,1:mnumcr,YRS)  = (PCTAF(3,1:mnumcr,N)*PGFTRPV(1:mnumcr,N)) + ((1.0-PCTAF(3,1:mnumcr,N))*PMGTR(1:mnumcr,N))
    FPRICE(10,1:mnumcr,YRS) = (PCTAF(4,1:mnumcr,N)*PLGTR(1:mnumcr,N)) + ((1.0-PCTAF(4,1:mnumcr,N))*PMGTR(1:mnumcr,N))
    FPRICE(11,1:mnumcr,YRS) =  PGFTRPV(1:mnumcr,N)
    FPRICE(12,1:mnumcr,YRS) =  PLGTR(1:mnumcr,N)
    FPRICE(13,1:mnumcr,YRS) =  PMETR(1:mnumcr,N)
    FPRICE(14,1:mnumcr,YRS) =  PH2TR(1:mnumcr,N)
    FPRICE(15,1:mnumcr,YRS) =  chg_dist(1:mnumcr,3,2023-1989)*(PELP2CM(1:mnumcr,N) * CHGCSTMULT(3)) + chg_dist(1:mnumcr,1,2023-1989)*(PELPFCM(1:mnumcr,N)*CHGCSTMULT(1)) + chg_dist(1:mnumcr,2,2023-1989)*PELVHRS(1:mnumcr,N)
    FPRICE(16,1:mnumcr,YRS) =  PMGTR(1:mnumcr,N)

    if(curcalyr.gt.2023) then
!   Decrease the share of charging that occus at home, and redistribute to public L2/DCFC, as fuel availability (driven by BEV stocks) grows       
      chg_dist(1:mnumcr-2,2,n) = chg_dist(1:mnumcr-2,2,2023-1989) - FAVL(7,1:mnumcr-2,yrs-1)/1.4                                           ! Home
      chg_dist(1:mnumcr-2,1,n) = chg_dist(1:mnumcr-2,1,2023-1989)/sum(chg_dist(1:mnumcr-2,[1,3],2023-1989),DIM=2) * (1-chg_dist(1:mnumcr-2,2,n))      ! DCFC
      chg_dist(1:mnumcr-2,3,n) = 1 - sum(chg_dist(1:mnumcr-2,1:2,n),DIM=2)                                                                ! L2 public
      FPRICE(4,1:mnumcr-2,YRS)  =  chg_dist(1:mnumcr-2,1,n)*(PELP2CM(1:mnumcr-2,N) * CHGCSTMULT(1)) + chg_dist(1:mnumcr-2,3,n)*(PELPFCM(1:mnumcr-2,N) * CHGCSTMULT(3)) +chg_dist(1:mnumcr-2,2,n)*PELVHRS(1:mnumcr-2,N)
      FPRICE(7,1:mnumcr-2,YRS)  =  chg_dist(1:mnumcr-2,1,n)*(PELP2CM(1:mnumcr-2,N) * CHGCSTMULT(1)) + chg_dist(1:mnumcr-2,3,n)*(PELPFCM(1:mnumcr-2,N) * CHGCSTMULT(3)) +chg_dist(1:mnumcr-2,2,n)*PELVHRS(1:mnumcr-2,N)
      FPRICE(15,1:mnumcr-2,YRS)  =  chg_dist(1:mnumcr-2,1,n)*(PELP2CM(1:mnumcr-2,N) * CHGCSTMULT(1)) + chg_dist(1:mnumcr-2,3,n)*(PELPFCM(1:mnumcr-2,N) * CHGCSTMULT(3)) +chg_dist(1:mnumcr-2,2,n)*PELVHRS(1:mnumcr-2,N)
    endif
!
!!     If past last historical year, use modeled phev eVMT ratio and changing distribution of PHEVs by manufacturer group to estimate combined PHEV fuel prices
!!      if (yrs.gt.epalyr) then
!!        NUM1 = 0.0
!!        NUM2 = 0.0
!!        do igp=1,maxgroup
!!          do icl=1,maxclass
!!            NUM1 = NUM1 + phev_evmt(igp,icl,yrs-1,5)*cafesales(igp,icl,yrs-1,5)
!!            NUM2 = NUM2 + phev_evmt(igp,icl,yrs-1,6)*cafesales(igp,icl,yrs-1,6)
!!          enddo
!!        enddo
!!
!!        if(sum(cafesales(1:maxgroup,1:maxclass,yrs-1,5)).gt.0.0) temp_phevevmt(1) = NUM1 / sum(cafesales(1:maxgroup,1:maxclass,yrs-1,5))
!!        if(sum(cafesales(1:maxgroup,1:maxclass,yrs-1,5)).gt.0.0) temp_phevevmt(2) = NUM1 / sum(cafesales(1:maxgroup,1:maxclass,yrs-1,6))
!!        
!!        if(temp_phevevmt(1).gt.0.0) FPRICE(5,IREGN,YRS) = (temp_phevevmt(1)*PELVHRS(IREGN,N)) + ((1.0-temp_phevevmt(1))*PMGTR(IREGN,N))
!!        if(temp_phevevmt(2).gt.0.0) FPRICE(6,IREGN,YRS) = (temp_phevevmt(2)*PELVHRS(IREGN,N)) + ((1.0-temp_phevevmt(2))*PMGTR(IREGN,N))

    FPRICE(:,:,YRS) = FPRICE(:,:,YRS) * MC_JPGDP(1) * 100.0 * CFMGQ(n)/42.0

!...initialize vehicle attributes to 0.0
    
    FLCOST(1:maxgroup,1:maxldv,1:maxclass,1:mnumcr-2,yrs)   = 0.0
    HFUEL(1:maxgroup,1:maxldv,1:maxclass,1:mnumcr-2,yrs)    = 0.0
    ACCL(1:maxgroup,1:maxldv,1:maxclass,1:mnumcr-2,yrs)     = 0.0
    MAINT(1:maxgroup,1:maxldv,1:maxclass,1:mnumcr-2,yrs)    = 0.0	
    LUGG(1:maxgroup,1:maxldv,1:maxclass,1:mnumcr-2)         = 0.0
    VRNG(1:maxgroup,1:maxldv,1:maxclass,1:mnumcr-2,yrs)     = 0.0
	
	if(curcalyr.ge.epalyr) then
	  do igp=1,maxgroup
		ivtyp = grpmap(igp) ! for maintenance cost
        do icl=1,maxclass
          do ildv=1,maxldv
              if(classflag(igp,icl,ildv)) then
!...			calculate fuel cost per mile			  
				if(femmpg(igp,icl,yrs,ildv).ne.0.0) FLCOST(igp,ildv,icl,1:mnumcr-2,yrs) = FPRICE(ildv,1:mnumcr-2,yrs)/femmpg(igp,icl,yrs,ildv)
!...		    assign home fueling dummy to electric vehicles, currently excluding PHEVs
			    if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) HFUEL(igp,ildv,icl,1:mnumcr-2,yrs) = 1.0
!...			calculate horsepower to weight ratio
				if(femwgt(igp,icl,yrs,ildv).ne.0.0) ACCL(igp,ildv,icl,1:mnumcr-2,yrs) = femhp(igp,icl,yrs,ildv)/femwgt(igp,icl,yrs,ildv)
!...			maintenance cost (unit = nominal $).  Convert cost in 1996$ back to 1987$ and then to nominal $
				MAINT(igp,ildv,icl,1:mnumcr-2,yrs) = MAINTGRP(ildv,icl,ivtyp) 
!...			luggage space (unit = cu. ft.). Calculate ratio to gas vehicle, or if no gas vehicle, ind avg ratio to ind. avg gas veh.
!               Pickups don't have a luggage space (for choice model purposes)
				if(igp.ge.ltkgrp.and.icl.le.2) then
                  LUGG(igp,ildv,icl,1:mnumcr-2) = 0.0
!               Use historical data
                elseif(epalug(igp,icl,epalyr,ildv).gt.0.0.and.epalug(igp,icl,epalyr,gas).gt.0.0) then
				  LUGG(igp,ildv,icl,1:mnumcr-2) = epalug(igp,icl,epalyr,ildv)/epalug(igp,icl,epalyr,gas)
!               Use industry average luggage space ratio
				else 
				  LUGG(igp,ildv,icl,1:mnumcr-2) = luggavg(igp,icl)
				endif
!...			vehicle range
				vrng(igp,ildv,icl,1:mnumcr-2,yrs) = femrng(igp,icl,yrs,ildv)
			  endif
		  enddo 
		enddo 
	  enddo

!...  calculate IRA PHEV and EV tax credits
      if(curcalyr.eq.epalyr) then
        IRA_CREDIT = 0.0
        if(ira_stim.eq.1.0) then
          do ildv=1,maxldv
            if(ildv.eq.4.or.ildv.le.7.or.ildv.eq.15.or.ildv.eq.14) then
  		      if (SwitchLDV_HR1.eq.1) IRA_Credit(ildv,epalyr-1989:mnumyr) = (ira_veh_cred * ira_veh_shr(1,epalyr:mnumyr+1989,1)) + (ira_bat_cred * ira_bat_shr(1,epalyr:mnumyr+1989,1))
  		      if (SwitchLDV_HR1.eq.0) IRA_Credit(ildv,epalyr-1989:mnumyr) = (ira_veh_cred * ira_veh_shr(1,epalyr:mnumyr+1989,2)) + (ira_bat_cred * ira_bat_shr(1,epalyr:mnumyr+1989,2))
            elseif(ILDV.eq.5.or.ILDV.eq.6) then
  		      if (SwitchLDV_HR1.eq.1) IRA_Credit(ildv,epalyr-1989:mnumyr) = (ira_veh_cred * ira_veh_shr(2,epalyr:mnumyr+1989,1)) + (ira_bat_cred * ira_bat_shr(2,epalyr:mnumyr+1989,1))
  		      if (SwitchLDV_HR1.eq.0) IRA_Credit(ildv,epalyr-1989:mnumyr) = (ira_veh_cred * ira_veh_shr(2,epalyr:mnumyr+1989,2)) + (ira_bat_cred * ira_bat_shr(2,epalyr:mnumyr+1989,2))
  		    endif
  	      enddo
  	    endif
      endif
!...  vehicle purchase price: fempri less ira and state vehicle tax credits (1990$).
      do ildv=1,maxldv
        do igp=1,maxgroup
          do icl=1,maxclass
			do iregn=1,mnumcr-2
			  pspr(igp,ildv,icl,iregn,yrs) = 0.0
			  if(mmavail(igp,ildv,icl,iregn,yrs).gt.0.0) then
				pspr(igp,ildv,icl,iregn,yrs) = fempri(igp,icl,yrs,ildv)
!...			electric vehicles
				if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.14) & 
                  pspr(igp,ildv,icl,iregn,yrs) = fempri(igp,icl,yrs,ildv) - ira_credit(ildv,n) - state_cred(iregn,yrs,1) 
!...			plug-in hybrid vehilces
				if(ildv.eq.5.or.ildv.eq.6.or.ildv.eq.14) &
				  pspr(igp,ildv,icl,iregn,yrs) = fempri(igp,icl,yrs,ildv) - ira_credit(ildv,n) - state_cred(iregn,yrs,2)
!...			hybrid vehicles
				if(ildv.eq.16) pspr(igp,ildv,icl,iregn,yrs) = fempri(igp,icl,yrs,ildv) - state_cred(iregn,yrs,3)
              endif
            enddo
          enddo
        enddo 
      enddo
	endif

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

    REAL    ALTSTA(maxfuel,MNUMCR-2,MNUMYR)
    REAL    ALTSTAT(maxfuel,MNUMYR)
    REAL    PREDSTK(maxfuel,MNUMYR)
    REAL    AFVSHREG(maxfuel,MNUMCR-2,MNUMYR)
    REAL    FUELVSAL(maxfuel,MNUMCR-2,MNUMYR)
    REAL    FUELVSALT(maxfuel,MNUMYR)
    REAL    sta_rat_base(maxfuel)
    REAL    gas_tput_hr(MNUMCR-2)
    REAL    port_time_tot(MNUMCR-2)
    REAL    port_time(maxchrg,MNUMCR-2)

!...Re-assign initial number of refueling stations according to NREL data by Census Division
    if(curcalyr.le.1995)then
      ALTSTA(1:maxfuel,1:mnumcr-2,n) = INITSTA(1:maxfuel,6,1:mnumcr-2)
    elseif(curcalyr.le.2012)then
      ALTSTA(1:maxfuel,1:mnumcr-2,n) = INITSTA(1:maxfuel,n,1:mnumcr-2)

!...  calculate total U.S. stations
      ALTSTAT(1:maxfuel,N) = sum(ALTSTA(1:maxfuel,1:mnumcr-2,n), DIM=2)
        
!...  initialize CNG fuel availability
      CNGAVAIL(1:mnumcr-2,n) = ALTSTA(5,1:mnumcr-2,n)/ALTSTA(1,1:mnumcr-2,n)

      if(curcalyr.eq.2012) then 
        PREDSTK(1,N) =  (SUM(ldvstk([3,5,6,9,10,16],n-1))*.75 + SUM(ldvstk([1,16],n-1))) * 1000000.
        PREDSTK(2,N) =  SUM(LDVSTK([2,8],n-1)) * 1000000.
        PREDSTK(3,N) =  (LDVSTK( 3,n-1)*0.25) * 1000000.
        PREDSTK(4,N) =   LDVSTK(13,n-1)* 1000000.
        PREDSTK(5,N) = ((LDVSTK( 9,n-1)*0.25)+LDVSTK(11,N-1)) * 1000000.
        PREDSTK(6,N) = ((LDVSTK(10,n-1)*0.25)+LDVSTK(12,N-1)) * 1000000.
        PREDSTK(7,N) = ((ldvstk( 5,n-1)*0.1 +ldvstk( 6,n-1))*0.25+ldvstk( 4,n-1)+ldvstk( 7,n-1)+ldvstk(15,n-1)) * 1000000.
        PREDSTK(8,N) =   LDVSTK(14,n-1) * 1000000.
	  endif
	  
	  sta_rat_base(1:maxfuel) = sta_rat(1:maxfuel)

    else
!...  Estimate the vehicle stocks used to calculate the number of refueling stations
!...  by weighting flex and bi-fuel at 25% 
      PREDSTK(1,N) =  (SUM(ldvstk([3,5,6,9,10,16],n-1))*.75 + SUM(ldvstk([1,16],n-1))) * 1000000. 
      PREDSTK(2,N) =  SUM(LDVSTK([2,8],n-1)) * 1000000.
      PREDSTK(3,N) =  (LDVSTK( 3,n-1)*0.25) * 1000000.
      PREDSTK(4,N) =   LDVSTK(13,n-1)* 1000000.
      PREDSTK(5,N) = ((LDVSTK( 9,n-1)*0.25)+LDVSTK(11,N-1)) * 1000000.
      PREDSTK(6,N) = ((LDVSTK(10,n-1)*0.25)+LDVSTK(12,N-1)) * 1000000.
      PREDSTK(7,N) = ((ldvstk( 5,n-1)*0.1 +ldvstk( 6,n-1))*0.25+ldvstk( 4,n-1)+ldvstk( 7,n-1)+ldvstk(15,n-1)) * 1000000.
      PREDSTK(8,N) =   LDVSTK(14,n-1) * 1000000.

	  if(curcalyr.le.stockyr) Sta_rat(2:maxfuel) = sta_rat_base(2:maxfuel) + (predstk(2:maxfuel,n)/predstk(1,n))*sta_rat(1)
		  
!...  Calculate the total number of refueling stations needed based on an historic
!...  ratio of vehicle stock per refueling station
      ALTSTAT(1:maxfuel,N) = ALTSTAT(1:maxfuel,N-1)+((PREDSTK(1:maxfuel,n)-PREDSTK(1:maxfuel,n-1))/STA_RAT(1:maxfuel))
      
!...  Regionalize the predicted stations by regional vehicle sales [ldv_sales(maxgroup,maxclass,maxldv,mnumcr,mnumyr)]
!...    Gasoline
        FUELVSAL(1,1:mnumcr-2,n) = sum(sum(sum(ldv_sales(:,:,[1,16],1:mnumcr-2,n-1),DIM=3),DIM=2),DIM=1)
!...    Diesel
        FUELVSAL(2,1:mnumcr-2,n) = sum(sum(sum(ldv_sales(:,:,[2,8],1:mnumcr-2,n-1),DIM=3),DIM=2),DIM=1)
!...    Ethanol
        FUELVSAL(3,1:mnumcr-2,n) = sum(sum(ldv_sales(:,:,3,1:mnumcr-2,n-1),DIM=2),DIM=1)   
!...    Methanol
        FUELVSAL(4,1:mnumcr-2,n) = sum(sum(ldv_sales(:,:,13,1:mnumcr-2,n-1),DIM=2),DIM=1) 
!...    CNG
        FUELVSAL(5,1:mnumcr-2,n) = sum(sum(sum(ldv_sales(:,:,[9,11],1:mnumcr-2,n-1),DIM=3),DIM=2),DIM=1) 
!...    LPG
        FUELVSAL(6,1:mnumcr-2,n) = sum(sum(sum(ldv_sales(:,:,[10,12],1:mnumcr-2,n-1),DIM=3),DIM=2),DIM=1)
!...    Electric
        FUELVSAL(7,1:mnumcr-2,n) = sum(sum(sum(ldv_sales(:,:,[4,5,6,7,15],1:mnumcr-2,n-1),DIM=3),DIM=2),DIM=1)
!...    Hydrogen
        FUELVSAL(8,1:mnumcr-2,n) = sum(sum(ldv_sales(:,:,14,1:mnumcr-2,n-1),DIM=2),DIM=1)

!...  calculate total U.S. sales
      FUELVSALT(1:maxfuel,n) = sum(FUELVSAL(1:maxfuel,1:mnumcr-2,n),DIM=2)
      
      AFVSHREG(1:maxfuel,1:mnumcr-2,n)=0.0
      do ifuel=1,maxfuel
        if(FUELVSALT(ifuel,n).ne. 0.0) AFVSHREG(ifuel,1:mnumcr-2,n) = FUELVSAL(ifuel,1:mnumcr-2,n)/FUELVSALT(ifuel,n)
      enddo
      
      do iregn=1,mnumcr-2
        ALTSTA(1:maxfuel,iregn,n) = ALTSTAT(1:maxfuel,n) * AFVSHREG(1:maxfuel,iregn,n)
      enddo	
	
    endif

!...Estimate fuel availability
    do iregn=1,mnumcr-2 
      do ifuel=1,maxfuel
        if (ifuel.eq.7) then
          if (n.ge.CHR_STR_YR.and.n.le.CHR_LST_YR) then
!           If doing AltTrnp side case, and we're beyond pseudo-history (calibrated to Ward's YTD, shouldn't change across side cases), use the endogenous station build estimate (based on stock growth)
            if (ENFORCE_MY27REGS.eq.0.and.curcalyr.gt.epalyr+2) then
              favail(ifuel,n,iregn) = favail(ifuel,n-1,iregn) * &
                                      (( (sum(LDV_STOCK(iregn,:,:,[4,7,15],:,:,n-1))/sum(LDV_STOCK(iregn,:,:,:,:,:,n-1))) / &
                                         (sum(LDV_STOCK(iregn,:,:,[4,7,15],:,:,n-2))/sum(LDV_STOCK(iregn,:,:,:,:,:,n-2)))-1) * ELAS_FAVL &
                                       + 1)
!           Otherwise, stick with the exogenous station build estimates
            else
!...          Gasoline refueling capacity/throughput
              gas_tput_hr(iregn) = (INITSTA(1,n,iregn)*GAS_PUMP_PER_STA)*GAS_HRLY_THRUPUT
              
!...          EV refueling capacity/throughput (EVs charged per hour)
              port_time(1:maxchrg,iregn) = (PRT_CNT(1:maxchrg,n,iregn)/PRT_RT(1:maxchrg))
              
              port_time_tot(iregn) = sum(port_time(1:maxchrg,iregn))
              
!...          Calculate fuel availability
              favail(ifuel,n,iregn) = port_time_tot(iregn)/gas_tput_hr(iregn)
            endif
!...      Grow fuel availability proportionally to BEV share of total on-road stock (post-2032)
          else
            favail(ifuel,n,iregn) = favail(ifuel,n-1,iregn) * &
                                    (( (sum(LDV_STOCK(iregn,:,:,[4,7,15],:,:,n-1))/sum(LDV_STOCK(iregn,:,:,:,:,:,n-1))) / &
                                       (sum(LDV_STOCK(iregn,:,:,[4,7,15],:,:,n-2))/sum(LDV_STOCK(iregn,:,:,:,:,:,n-2)))-1) * ELAS_FAVL &
                                       + 1)
          endif
        else 
          if(ALTSTA(1,iregn,n).gt.0.0) then
            FAVAIL(ifuel,n,iregn) = ALTSTA(ifuel,iregn,n)/ALTSTA(1,iregn,n)
            if (ifuel.eq.8) FAVAIL(ifuel,n,iregn) = MIN(FAVAIL(ifuel,n,iregn),FAVAIL(ifuel,n-1,iregn)*1.1)  ! Limit H2 infra growth sans policy 
          endif
          if(curcalyr.gt.2012) FAVAIL(ifuel,n,iregn) = MAX(FAVAIL(ifuel,n,iregn),FAVAIL(ifuel,n-1,iregn))
        endif

!...    Set the availability of CNG from the value given by ngtdm         
        if(curcalyr.gt.2012.and.ifuel.eq.5) FAVAIL(ifuel,n,iregn) = MAX(FAVAIL(ifuel,n,iregn),CNGAVAIL(iregn,n))

!...    Do not allow any fuel availability to be larger than gasoline (100%)
        FAVAIL(ifuel,n,iregn) = min(FAVAIL(ifuel,n,iregn),FAVAIL(1,n,iregn))
      enddo
    enddo

!...Re-align indices (ILDV=1-16) for fuel availability
    FAVL( 1, 1:mnumcr-2, yrs) = FAVAIL(1, n, 1:mnumcr-2)
    FAVL( 2, 1:mnumcr-2, yrs) = FAVAIL(2, n, 1:mnumcr-2)
    FAVL( 3, 1:mnumcr-2, yrs) = MAX(FAVAIL(3, n, 1:mnumcr-2), FAVAIL(1, n, 1:mnumcr-2))
    FAVL( 4, 1:mnumcr-2, yrs) = FAVAIL(7, n, 1:mnumcr-2)
    FAVL( 5, 1:mnumcr-2, yrs) = MAX(FAVAIL(7, n, 1:mnumcr-2), FAVAIL(1, n, 1:mnumcr-2))
    FAVL( 6, 1:mnumcr-2, yrs) = MAX(FAVAIL(7, n, 1:mnumcr-2), FAVAIL(1, n, 1:mnumcr-2))
    FAVL( 7, 1:mnumcr-2, yrs) = FAVAIL(7, n, 1:mnumcr-2)
    FAVL( 8, 1:mnumcr-2, yrs) = MAX(FAVAIL(7, n, 1:mnumcr-2), FAVAIL(2, n, 1:mnumcr-2))
    FAVL( 9, 1:mnumcr-2, yrs) = MAX(FAVAIL(5, n, 1:mnumcr-2), FAVAIL(1, n, 1:mnumcr-2))
    FAVL(10, 1:mnumcr-2, yrs) = MAX(FAVAIL(6, n, 1:mnumcr-2), FAVAIL(1, n, 1:mnumcr-2))
    FAVL(11, 1:mnumcr-2, yrs) = FAVAIL(5, n, 1:mnumcr-2)
    FAVL(12, 1:mnumcr-2, yrs) = FAVAIL(6, n, 1:mnumcr-2)
    FAVL(13, 1:mnumcr-2, yrs) = FAVAIL(4, n, 1:mnumcr-2)
    FAVL(14, 1:mnumcr-2, yrs) = FAVAIL(8, n, 1:mnumcr-2)
    FAVL(15, 1:mnumcr-2, yrs) = FAVAIL(7, n, 1:mnumcr-2)
    FAVL(16, 1:mnumcr-2, yrs) = FAVAIL(1, n, 1:mnumcr-2)
       
  RETURN
  END SUBROUTINE TALT2

! ==========================================================================================================
! ... Subroutine TALT2X calculates Level 1 and Level 2 light vehicle market penetration estimates in the AFV 
! ... model.
! ==========================================================================================================
    SUBROUTINE TALT2X
    USE T_
    IMPLICIT NONE

      INTEGER, PARAMETER :: MAX_JG = 5      ! Number of nests
      INTEGER, PARAMETER :: MAX_JT = 6      ! Number of powertrain slots in each nest
      INTEGER, PARAMETER :: DoBug  = 0
      
      REAL*8        ETOT(MAX_JG),UISUM(MAX_JT),ESUM(MAX_JT),XSHARE(MAX_JG,MAX_JT),GCOST(MAX_JG),GENCOST, YSHARE(MAX_JG)
      
      REAL*8        nmlmco_1, nmlmco_2, nmlmco_3, nmlmco_4, nmlmco_5
      REAL*8        nmlmco_6, nmlmco_7, nmlmco_8, nmlmco_9, nmlmco_10, nmlmco_11, nmlmco_12
      
      REAL*8        MMAVAIL_LOG(MAXGROUP, MAXLDV, MAXCLASS, MNUMCR-2, MNUMYR)
      REAL*8        VRNG_INV(MAXGROUP, MAXLDV, MAXCLASS, MNUMCR-2, MNUMYR)
      
      INTEGER       JG,JT
      INTEGER       L2GROUP(MAX_JG,MAX_JT),L2GRPTOT(MAX_JG)
      real          cof(12),var(12),rst(13),TmpShr,TmpPlug,TmpPlug1,TmpPlug2, TmpApshrTot,ldv_sales_allLDV
      character*20  TAltLabel(MAXLDV)
      data          TAltLabel/'Gasoline','Diesel','Ethanol Flex','EV100','PHEV20','PHEV50', &
                              'EV200','Diesel Hybrid','CNG Bi-Fuel','LPG Bi-Fuel','CNG','LPG','FC Methanol', &
                              'FC Hydrogen','EV300','Gasoline Hybrid'/
      
      REAL*8        hev_shr_of_hev_and_gas, hev_shr_of_hev_and_gas_lag
      
      LOGICAL, DIMENSION(MAXGROUP, MAXCLASS, MAXLDV, MNUMCR-2) :: nan_error_mask
      LOGICAL, DIMENSION(MAXGROUP, MAXCLASS, MAXLDV, MNUMCR-2) :: negative_error_mask
      
      DATA (L2GROUP(1,JT),JT=1,MAX_JT) / 1, 2, 3, 9,10,16/  ! conventional (level 1 logit)
      DATA (L2GROUP(2,JT),JT=1,MAX_JT) / 5, 6, 8, 0, 0, 0/  ! elec. hybrid (level 1)
      DATA (L2GROUP(3,JT),JT=1,MAX_JT) /11,12, 0, 0, 0, 0/  ! dedicated gaseous(level 1) 
      DATA (L2GROUP(4,JT),JT=1,MAX_JT) /13,14, 0, 0, 0, 0/  ! fuel cell (level 1) 
      DATA (L2GROUP(5,JT),JT=1,MAX_JT) / 4, 7,15, 0, 0, 0/  ! electric vehicle (level 1) 
      DATA (L2GRPTOT(JG) ,JG=1,MAX_JG) / 6, 3, 2, 2, 3/     ! lookup table index for size of group

      REAL          XGS(MAXLDV)

!     Initialize sales shares to 0
      apshrgrp(1:maxgroup, 1:maxclass, 1:maxldv, 1:mnumcr, n) = 0.0
      
      WHERE (MMAVAIL(1:maxgroup,1:MAXLDV,1:maxclass,1:MNUMCR-2,yrs).gt.0.0)
        MMAVAIL_LOG(1:maxgroup,1:MAXLDV,1:maxclass,1:MNUMCR-2,N) = ALOG(MMAVAIL(1:maxgroup,1:MAXLDV,1:maxclass,1:MNUMCR-2,yrs))
        VRNG_INV(1:maxgroup,1:MAXLDV,1:maxclass,1:MNUMCR-2,N) = 1/VRNG(1:maxgroup,1:MAXLDV,1:maxclass,1:MNUMCR-2,yrs)
      ENDWHERE
      
      DO IREGN=1,MNUMCR-2
        DO ICL=1,MAXCLASS
          DO IGP=1,MAXGROUP
!           Pre-fetch nmlmco values for the current ICL, IGP for efficiency
            nmlmco_1  = nmlmco(1,icl,igp)
            nmlmco_2  = nmlmco(2,icl,igp)
            nmlmco_3  = nmlmco(3,icl,igp)
            nmlmco_4  = nmlmco(4,icl,igp)
            nmlmco_5  = nmlmco(5,icl,igp)
            nmlmco_6  = nmlmco(6,icl,igp)
            nmlmco_7  = nmlmco(7,icl,igp)
            nmlmco_8  = nmlmco(8,icl,igp)
            nmlmco_9  = nmlmco(9,icl,igp)
            nmlmco_10 = nmlmco(10,icl,igp)
            nmlmco_11 = nmlmco(11,icl,igp)
            nmlmco_12 = nmlmco(12,icl,igp)
            
            DO JG=1,MAX_JG ! level 1 
              ETOT(JG) = 0.0
              DO JT=1,L2GRPTOT(JG)        !tech within group
                ILDV = L2GROUP(JG,JT)
                UISUM(JT) = 0.0

! ... Calculate value functions.  Because of the formulation, FEMRNG must > 0
! ... The importance of this should not be underestimated, it is more than a simple divide
! ... by zero precaution.  The requirement for a non-zero range estimate actually allows
! ... the ATV penetration model to properly bypass non-existant vehicle classes (assuming,
! ... of course, that the ranges for these classes are set to zero).

                if(mmavail(igp,ildv,icl,iregn,yrs).gt.0.0) then

                  IF (PSPR(igp,ildv,icl,iregn,yrs) .LT. 0.0) then
                    WRITE(21,'(a,",",5(a4,","),4(a12,","))')'ERROR: negative price','year','iter','grp','icl','ildv','pspr','fempri','pspr_prev','fempri_prev'
                    WRITE(21,'(a,",",5(i4,","),4(f12.2,","))')'ERROR: negative price', curcalyr,curitr,igp,icl,ildv,PSPR(igp,ildv,icl,iregn,yrs),fempri(igp,icl,yrs,ildv),&
                                                                                       PSPR(igp,ildv,icl,iregn,yrs-1),fempri(igp,icl,yrs-1,ildv)
                    WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
                    STOP 450
                  endif

! ... Value function for all technologies
                  
                  UISUM(JT) = nmlmco_1 * PSPR(igp,ildv,icl,iregn,yrs) +   				& 
                              nmlmco_2 * FlCost(igp,ildv,icl,iregn,yrs) + 				& 
                              nmlmco_3 * VRNG_INV(igp,ildv,icl,iregn,N) +               &
                              nmlmco_4 * BRCOST25(igp,ildv,icl,yrs) +     				&
                              nmlmco_5 * ACCL(igp,ildv,icl,iregn,yrs) +         		& 
                              nmlmco_6 * HFUEL(igp,ildv,icl,iregn,yrs) +        		&
                              nmlmco_7 * MAINT(igp,ildv,icl,iregn,yrs) +        		&
                              nmlmco_8 * LUGG(igp,ildv,icl,iregn) +             		&
                              nmlmco_9 * EXP(nmlmco_10*FAvl(ildv,iregn,yrs)) +          &
                              nmlmco_11 * MMAVAIL_LOG(igp,ildv,icl,iregn,N) +           &
                              X210(igp,icl,ildv,iregn,n)

                  ESUM(JT) = 0.0
                  IF (UISUM(JT) .NE. 0.0) ESUM(JT) = DEXP(UISUM(JT))

                ELSE
                  ESUM(JT) = 0.0
                ENDIF

                ETOT(JG) = ETOT(JG) + ESUM(JT)

                ! Debug writes
				if(DoBug.eq.1) then ! LOGIT DEBUG
				  cof(1)  = nmlmco_1;  var(1)=pspr(igp,ildv,icl,iregn,yrs); rst(1)=cof(1)*var(1)
				  cof(2)  = nmlmco_2;  var(2)=FlCost(igp,ildv,icl,iregn,yrs); rst(2)=cof(2)*var(2)	
				  cof(3)  = nmlmco_3;  var(3)=vrng(igp,ildv,icl,iregn,yrs); rst(3)=cof(3)*(1.0/var(3))	
				  cof(4)  = nmlmco_4;  var(4)=brcost25(igp,ildv,icl,yrs); rst(4)=cof(4)*var(4)
				  cof(5)  = nmlmco_5;  var(5)=accl(igp,ildv,icl,iregn,yrs); rst(5)=cof(5)*var(5)
				  cof(6)  = nmlmco_6;  var(6)=hfuel(igp,ildv,icl,iregn,yrs); rst(6)=cof(6)*var(6)			
				  cof(7)  = nmlmco_7;  var(7)=maint(igp,ildv,icl,iregn,yrs); rst(7)=cof(7)*var(7)
				  cof(8)  = nmlmco_8;  var(8)=lugg(igp,ildv,icl,iregn); rst(8)=cof(8)*var(8)
				  cof(9)  = nmlmco_10; var(9)=FAvl(ildv,iregn,yrs); rst(9)=0.0
				  cof(10) = nmlmco_9;  var(10)=0.0; rst(10)=cof(10)*exp(cof(9)*var(9))
				  cof(11) = nmlmco_11; var(11)=mmavail(igp,ildv,icl,iregn,yrs); rst(11)=cof(11)*alog(var(11))
                  cof(12) = x210(igp,icl,ildv,iregn,n); var(12)=0.0; rst(12)=cof(12)

                  rst(13)=SUM(rst(1:12))

                  !Write out details for each technology in each group.
                  if(curcalyr.gt.2023.and.curcalyr.lt.2026) then
                    if(jt.eq.1) write(21,'(/,a,i3,a,4i5,a)') 'Logit Nest: ',jg,'  (year, region, grp, class: ',n+1989,iregn,igp,ICL,')'
                    if(ESum(jt).eq.0.0) then
                      write(21,'(a,a,i3,a)') taltlabel(ILDV),' (tech: ',ILDV,') Tech excluded because VRng=0'
                    else
                      write(21,'(a,a,i3,a)') taltlabel(ILDV),' (tech: ',ILDV,')'
                      write(21,'(a,a)') '      PSPR    FlCost      VRng  BrCost25      Accl     HFuel     ', &
                       'Maint      Lugg     FAvl1     FAvl2   MMAvail     Const'
                      write(21,'(f10.5,f10.4,f10.2,f10.5,2f10.4,f10.5,2f10.4,f10.3,2f10.4)') (cof(i),i=1,12)
                      write(21,'(f10.1,f10.3,f10.1,f10.5,f10.3,f10.5,f10.2,2f10.3,f10.2,f10.4,f10.2)') (var(i),i=1,12)
                      write(21,'(12f10.2)') (rst(i),i=1,12)
                      write(21,'(a,f12.3,a,f12.3,a)') '  Total utility: ',rst(13),'  (Check total: ',uisum(jt),')'
                    endif
                  endif
                endif

              ENDDO !JT 

! ...         Level 2 shares
              XSHARE(JG, 1:L2GRPTOT(JG)) = 0.0
              GCOST(JG) = 0.0

              IF (ETOT(JG) .NE. 0.0) THEN
                  XSHARE(JG, 1:L2GRPTOT(JG)) = (ESUM(1:L2GRPTOT(JG)) / ETOT(JG)) * 100.0
                  GCOST(JG) = (1.0 / nmlmco(1, icl, igp)) * DLOG(ETOT(JG))
              END IF

            ENDDO !JG

! ... Level 1 shares
            ETOT(1) = 0.0

            UISUM(1:MAX_JG) = nmlmco(12,icl,igp) * GCOST(1:MAX_JG)
            ESUM(1:MAX_JG) = 0.0
            
            WHERE (UISUM(1:MAX_JG) .NE. 0.0)
              ESUM(1:MAX_JG) = DEXP(UISUM(1:MAX_JG))
            END WHERE
            
            ETOT(1) = SUM(ESUM(1:MAX_JG))
            
            YSHARE(1:MAX_JG) = 0.0
            GENCOST = 0.0
            IF (ETOT(1) .NE. 0.0) THEN
              YSHARE(1:MAX_JG) = (ESUM(1:MAX_JG)/ETOT(1)) * 100.0
              GENCOST = (1.0/nmlmco(12,icl,igp)) * DLOG(ETOT(1))
            END IF
            ETOT(2) = SUM(YSHARE(1:MAX_JG))
            
            IFUELX = 1
            
            DO JG=1,MAX_JG
              DO JT=1,L2GRPTOT(JG)
                XGS(ifuelx) = XSHARE(JG,JT) * YSHARE(JG) / 100.0
                APShrGrp(igp,ICL,L2Group(jg,jt),iregn,n)=xgs(ifuelx)/100 
                ifuelx = ifuelx + 1
			  ENDDO
            ENDDO
          ENDDO
        ENDDO
        
!...	calculate regional sales by Group to calculate CAFE; perform some checks
        ldv_sales(1:maxgroup,1:maxclass,1:maxldv,iregn,n) = APShrGrp(1:maxgroup,1:maxclass,1:maxldv,iregn,n) * &
                                                            SPREAD(mfr_sales(iregn,1:maxgroup,1:maxclass,n),&
                                                                   DIM=3, NCOPIES = MAXLDV)
        
	  enddo ! mnumcr-2

	  nan_error_mask = (ldv_sales(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,n) .NE. ldv_sales(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,n))
      negative_error_mask = (ldv_sales(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,n) .LT. 0.0)
      
      if (ANY(nan_error_mask).or.ANY(negative_error_mask)) then
        do iregn = 1, MNUMCR-2
          do igp=1,maxgroup
            do icl=1,maxclass 
              do ildv=1,maxldv 
                if (nan_error_mask(igp,icl,ildv,iregn)) then
                  write(21,'(a,",",6(a4,","),5(a12,","))')'ERROR: ldv_sales NaN','year','itr','regn','igp','icl','ildv','ldv_sales','apshrgrp','mfr_sales','class_share','GrpShare'
                  write(21,'(a,",",6(i4,","),5(f12.4,","))')'ERROR: ldv_sales NaN',curcalyr,curitr,iregn,igp,icl,ildv,ldv_sales(igp,icl,ildv,iregn,n),apshrgrp(igp,icl,ildv,iregn,n),mfr_sales(iregn,igp,icl,n),&
                                                    class_share(iregn,icl,igp,yrs),GrpShare(iregn,igp,n)
                  WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
                  STOP
                endif
                if (negative_error_mask(igp,icl,ildv,iregn)) then
                  write(21,'(a,",",6(a4,","),5(a12,","))')'ERROR: ldv_sales negative','year','itr','regn','igp','icl','ildv','ldv_sales','apshrgrp','mfr_sales','class_share','GrpShare'
                  write(21,'(a,",",6(i4,","),5(f12.4,","))')'ERROR: ldv_sales negative',curcalyr,curitr,iregn,igp,icl,ildv,ldv_sales(igp,icl,ildv,iregn,n),apshrgrp(igp,icl,ildv,iregn,n),mfr_sales(iregn,igp,icl,n),&
                                                    class_share(iregn,icl,igp,yrs),GrpShare(iregn,igp,n)
                  WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
                  STOP
                endif
              enddo
            enddo
          enddo
        enddo
      endif
      
!...  calculate total US sales
	  ldv_sales(1:maxgroup,1:maxclass,1:maxldv,mnumcr,n) = sum(ldv_Sales(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,n),DIM=4)

!...  fill cafesales
      cafesales(1:maxgroup, 1:maxclass, yrs, 1:maxldv) = ldv_sales(1:maxgroup, 1:maxclass, 1:maxldv, mnumcr, n)

!...  calculate national sales shares (powertrain share within each group/class bin)
      APShrGrp(1:maxgroup, 1:maxclass, 1:maxldv, mnumcr, n) = ldv_sales(1:maxgroup, 1:maxclass, 1:maxldv, mnumcr, n) / &
                                                              SPREAD(sum(ldv_Sales(1:maxgroup, 1:maxclass,1:maxldv,mnumcr,n), DIM=3), &
                                                                     DIM=3, NCOPIES=maxldv)
  
    RETURN
    END SUBROUTINE TALT2X

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

!...fill region fleet sales (2019-stockyr)
	if(curcalyr.ge.2019.and.curcalyr.le.stockyr) then
		do ifleet=1,maxfleet 
		  do icl=1,maxclass 
		    do ildv=1,maxldv 
			  do igp=1,maxgroup 
 				fltgrpsal(1:mnumcr-2,ifleet,igp,icl,ildv) = own_sales(ifleet+1,igp,icl,ildv,1:mnumcr-2,yrs) 
			  enddo
			  fltechsal(1:mnumcr-2,1,ifleet,icl,ildv,1) = sum(fltgrpsal(1:mnumcr-2,ifleet,1:cargrp,icl,ildv),DIM=2)
			  fltechsal(1:mnumcr-2,2,ifleet,icl,ildv,1) = sum(fltgrpsal(1:mnumcr-2,ifleet,ltkgrp:maxgroup,icl,ildv),DIM=2)
			enddo 
		  enddo 
		enddo 
	endif 

!...fleet vehicle sales projection by size class by group 
	if(curcalyr.gt.stockyr)then
		do ifleet=1,maxfleet
		  do igp=1,maxgroup
		    do icl=1,maxclass
			  do ildv=1,maxldv
!		      	fltgrpsal(iregn,ifleet,igp,icl,ildv) = ldv_sales(igp,icl,ildv,iregn,n) * ownsalesshr(ifleet+1,igp,icl,ildv,iregn,n)*1000000.0  !jma
				fltgrpsal(1:mnumcr-2,ifleet,igp,icl,ildv) = ldv_sales(igp,icl,ildv,1:mnumcr-2,n) * ownsaletemp(ifleet+1,igp,icl,1:mnumcr-2,n)*1000000.0
			  enddo
			enddo
          enddo
        enddo
!...  fleet vehicle sales projection summed by type (car/LT)
      fltechsal(1:mnumcr-2, 1, 1:maxfleet, 1:maxclass, 1:maxldv, 1) = &
        SUM(fltgrpsal(1:mnumcr-2, 1:maxfleet, 1:cargrp, 1:maxclass, 1:maxldv), DIM=3)
      fltechsal(1:mnumcr-2, 2, 1:maxfleet, 1:maxclass, 1:maxldv, 1) = &
        SUM(fltgrpsal(1:mnumcr-2, 1:maxfleet, ltkgrp:maxgroup, 1:maxclass, 1:maxldv), DIM=3)
	endif
	
!...sum national sales by ivtyp (fltechsal) and igp (fltgrpsal)
    fltechSal(mnumcr, 1:maxvtyp, 1:maxfleet, 1:maxclass, 1:maxldv, 1) = &
        SUM(fltechSal(1:mnumcr-2, 1:maxvtyp, 1:maxfleet, 1:maxclass, 1:maxldv, 1), DIM=1)
    fltgrpsal(mnumcr, 1:maxfleet, 1:maxgroup, 1:maxclass, 1:maxldv) = &
        SUM(fltgrpsal(1:mnumcr-2, 1:maxfleet, 1:maxgroup, 1:maxclass, 1:maxldv), DIM=1)

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
			  do ivtyp=1,MAXVTYP
				OLDFSTK(iregn,ivtyp,ifleet,ildv,1:maxage)=Flt_Stock(iregn,ivtyp,ifleet,ildv,1:maxage,1,n) * FLTTRANS(ifleet,1:maxage,ivtyp)
				Flt_Stock(iregn,ivtyp,ifleet,ildv,1:maxage,1,n)=Flt_Stock(iregn,ivtyp,ifleet,ildv,1:maxage,1,n) - OLDFSTK(iregn,ivtyp,ifleet,ildv,1:maxage)
              enddo
		    endif
		  enddo
		enddo
	  enddo

!...  fill ldv_stock array
      LDV_Stock(1:mnumcr-2, 1:maxvtyp, [1:maxfleet]+1, 1:maxldv, 1:maxage, 1:maxhav, n) = &
        Flt_Stock(1:mnumcr-2, 1:maxvtyp, 1:maxfleet, 1:maxldv, 1:maxage, 1:maxhav, n) / 1000000.0
        
!...  sum national         
      Flt_Stock(mnumcr, 1:maxvtyp, 1:maxfleet, 1:maxldv, 1:maxage, 1:maxhav, n) = &
        SUM(Flt_Stock(1:mnumcr-2, 1:maxvtyp, 1:maxfleet, 1:maxldv, 1:maxage, 1:maxhav, n), DIM=1)
      LDV_Stock(mnumcr, 1:maxvtyp, [1:maxfleet]+1, 1:maxldv, 1:maxage, 1:maxhav, n) = &
        SUM(LDV_Stock(1:mnumcr-2, 1:maxvtyp, [1:maxfleet]+1, 1:maxldv, 1:maxage, 1:maxhav, n), DIM=1)

	endif ! >stockyr

!...Calculate total surviving vehicles, by vehicle, fleet type, and vehicle technology (FLTECHSTK)
    FLTECHSTK(1:mnumcr, 1:maxvtyp, 1:maxfleet, 1:maxldv, 1:maxhav) = &
        SUM(Flt_Stock(1:mnumcr, 1:maxvtyp, 1:maxfleet, 1:maxldv, 1:maxage, 1:maxhav, n), DIM=5)

    do ivtyp=1,maxvtyp
      TOTFLTCAR(ivtyp) = SUM(FLTECHSTK(mnumcr,ivtyp,1:maxfleet,1:maxldv,1:maxhav))/1000000.0
    enddo

!...EPACT legislative alternative vehicle sales, for table 48
    do iregn=1,mnumcr-2
      LEGALTSAL(2,IREGN,N) = sum(fltechsal(iregn,1:2,2:3,1:6,3:maxldv,1:4))/1000000.0
      if (iregn.eq.mnumcr-2) LEGALTSAL(2,MNUMCR,N) = sum(LEGALTSAL(2,1:mnumcr-2,N))
    enddo

  RETURN
  END SUBROUTINE TFLTSTKS

! ==========================================================================================================
! ... Subroutine TFLTVMTS calculates VMT for fleets
! ==========================================================================================================
  SUBROUTINE TFLTVMTS
  USE T_
  IMPLICIT NONE

!...Total VMT by vehicle type and technology
    do ivtyp=1,maxvtyp
      do ifleet=1,maxfleet
        fltechvmt(1:mnumcr,ivtyp,ifleet,1:maxldv,1:maxhav) = fltechstk(1:mnumcr,ivtyp,ifleet,1:maxldv,1:maxhav)*fltvmtyr(ifleet,n,ivtyp)
      enddo
    enddo
	
  RETURN
  END SUBROUTINE TFLTVMTS

! ==========================================================================================================
! ... Subroutine CAFECALC estimates the fuel economy and tailpipe g/mi standards, the fleet average fuel economy and
!     g/mi emissions for each manufacturer group, and the compliance status of each group.
!     It also calculates the fleet-average vehicle attributes for publication after the final sales distributions are estimated.
!     Note that CAFECALC uses a different group dimension index -- jgp instead of igp -- since it can be called inside of an igp loop (in CAFEGHG_MEET)
! ==========================================================================================================
  SUBROUTINE CAFECALC(cafetestcall)
  USE T_
  USE MEAN_FUNCS
  IMPLICIT NONE

    integer   it,L,jgp
    real      CafeNeedX(MAXGROUP,mnumyr)
	real 	  num1, num2, den1, den2, mpgadjldv(maxvtyp,maxldv,mnumyr)
    REAL      ac_oc_credit_gCo2
    INTEGER   cafetestcall                                                  ! If 1, CAFECALC is called post-market-compliance
    INTEGER, PARAMETER :: CAFEGHG_DEBUG = 0                                 ! If 1, write out detailed cafe/ghg compliance information

!...Initialize once
    FPghgGrp(1:maxgroup,n) = 0.0
    MgGhgGrp(1:maxgroup,n) = 0.0
    EPAghgGrp(1:maxgroup,n) = 0.0

!...Calculate EPA GHG standard    
    if(ENFORCE_EPA.eq.1) then
      if(curcalyr.ge.2012) then
        FPghg(1:maxclass,1:maxgroup,n) = 0.0
!       Cars by size class
        WHERE (TRANSPOSE(sum(cafesales(1:cargrp,1:maxclass,yrs,1:maxldv),DIM=3)).gt.0.0)
          FPghg(1:maxclass,1:cargrp,n) = min(CFCoefEPAB2(n),max(CFCoefEPAA2(n),CFCoefEPAC2(n)*FPrint(1:maxclass,1:cargrp,n)+CFCoefEPAD2(n)))
        END WHERE
!       Light trucks by size class
        WHERE (TRANSPOSE(sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv),DIM=3)).gt.0.0)
          FPghg(1:maxclass,ltkgrp:maxgroup,n) = min((min(TFCoefEPAB2(n),max(TFCoefEPAA2(n),TFCoefEPAC2(n)*FPrint(1:maxclass,ltkgrp:maxgroup,n)+TFCoefEPAD2(n)))), &
	  										        (min(TFCoefEPAF2(n),max(TFCoefEPAE2(n),TFCoefEPAG2(n)*FPrint(1:maxclass,ltkgrp:maxgroup,n)+TFCoefEPAH2(n)))))
        END WHERE

!       Cars and light trucks, collapse size class       
        do jgp = 1, maxgroup
          FPghgGrp(jgp,n) = WEIGHTED_MEAN_1D(FPghg(1:maxclass,jgp,n), &
                                             sum(cafesales(jgp,1:maxclass,yrs,1:maxldv), DIM=2), &
                                             caller_id = 'FPghgGrp(CAFECALC)')
        enddo
!     Calculate compliance (CO2 credits or debits) using 2-cycle tested mpg
	    do jgp=1,maxgroup
	      ac_oc_credit_gCo2 = ac_oc_credit(jgp,n)*8887.0
          NUM1 = 0.0
	      NUM2 = 0.0    
	  	  do ildv=1,maxldv 
	  	    do icl=1,maxclass
	  	      if(femmpg(jgp,icl,yrs,ildv).ne.0.0) then
	  		    if(jgp.le.cargrp) then
	  			  if (ENFORCE_MY27REGS.eq.1.and.curcalyr.ge.2027.and.(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15)) then         ! Zero g/mi off-cycle and AC efficiency for BEVs in MY2027+
                    CYCLE
                  elseif (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then                                                      ! No tailpipe GHG, but can earn AC/offcycle creds
                    NUM1 = NUM1 + cafesales(jgp,icl,yrs,ildv) * (-ac_oc_credit_gCo2)
                  elseif (ildv.eq.5.or.ildv.eq.6) then                                                                    ! For PHEV, only count emissions from gasoline-fueled miles
                    NUM1 = NUM1 + cafesales(jgp,icl,yrs,ildv) * (1-phev_evmt(jgp,icl,yrs,ildv)) * &
                           (1/PHEVMPG_S(jgp,icl,yrs,ildv)*8887 - ac_oc_credit_gCo2)
                  else
                    NUM1 = NUM1 + cafesales(jgp,icl,yrs,ildv) * (1/femmpg(jgp,icl,yrs,ildv)*8887 - ac_oc_credit_gCo2)
                  endif
                else  ! trucks
	  			  if (ENFORCE_MY27REGS.eq.1.and.curcalyr.ge.2027.and.(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15)) then         ! Zero g/mi off-cycle and AC efficiency for BEVs in MY2027+
                    CYCLE
                  elseif (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.14) then                                        ! No tailpipe GHG, but can earn AC/offcycle creds
                    NUM2 = NUM2 + cafesales(jgp,icl,yrs,ildv) * (-ac_oc_credit_gCo2)
                  elseif (ildv.eq.5.or.ildv.eq.6) then                                                                    ! For PHEV, only count emissions from gasoline-fueled miles
                    NUM2 = NUM2 + cafesales(jgp,icl,yrs,ildv) * (1-phev_evmt(jgp,icl,yrs,ildv)) * &
                           (1/(PHEVMPG_S(jgp,icl,yrs,ildv))*8887 - ac_oc_credit_gCo2)
                  else
!                   Add full-size pickup truck incentives (20g/mile for strong hybrids, where they were 10% of the nameplate sales)
!                   No nameplates were eligible pre-MY2024; no longer available starting MY2025
                    if (curcalyr.eq.2024.and.icl.eq.2.and.ildv.eq.16) then
                      NUM2 = NUM2 + cafesales(jgp,icl,yrs,ildv) * (1/femmpg(jgp,icl,yrs,ildv)*8887 - ac_oc_credit_gCo2 - PU_CRED_ELIG(jgp)*20.0)
                    else
                      NUM2 = NUM2 + cafesales(jgp,icl,yrs,ildv) * (1/femmpg(jgp,icl,yrs,ildv)*8887 - ac_oc_credit_gCo2)
                    endif
                  endif
	  		    endif
	  		  endif
	  	    enddo
	      enddo
      
!         Calculate extra zero-emission sales to throw in the denominator (Advanced Technology Multipliers)
          DEN1 = sum( sum(cafesales(jgp,1:maxclass,yrs,[4,7,15,14]),DIM=1) * (EPAALTMULT([4,7,15,14],n) - 1) )
      
!         Calculate production-weighted g/mi by group
!         Incorporate advanced tech multipliers for BEVs and FCVs (vehicles count for >1 sale)
!         All vehicles can claim AC leakage
          if(jgp.le.cargrp) then
            EPAghgGrp(jgp,n) = NUM1/(sum(cafesales(jgp,1:maxclass,yrs,1:maxldv)) +  DEN1) - AC_CO2_OFFSET(jgp,n)
          else
            EPAghgGrp(jgp,n) = NUM2/(sum(cafesales(jgp,1:maxclass,yrs,1:maxldv)) +  DEN1) - AC_CO2_OFFSET(jgp,n)
          endif
      
        enddo
      
!       Calculate total MgCO2 credits/debits by group
        MgGhgGrp(1:cargrp,n)        = (FPghgGrp(1:cargrp,n) - EPAghgGrp(1:cargrp,n)) * &
                                      sum(sum(cafesales(1:cargrp,1:maxclass,yrs,1:maxldv),DIM=3),DIM=2) * LDV_VEHLIFEMI(1)
        MgGhgGrp(ltkgrp:maxgroup,n) = (FPghgGrp(ltkgrp:maxgroup,n) - EPAghgGrp(ltkgrp:maxgroup,n)) * &
                                      sum(sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv),DIM=3),DIM=2) * LDV_VEHLIFEMI(2)
   
      endif
    endif  ! ENFORCE_EPA.eq.1
        
!...Calculate NHTSA CAFE standards
    if(curcalyr.ge.2010) then 
		  
      FPMpg(1:maxclass,1:maxgroup,n) = 0.0
      if (curcalyr.lt.2012) then
        WHERE (fprint(1:maxclass,1:cargrp,n).ne.0.0)    ! cars
          FPMpg(1:maxclass,1:cargrp,n) = 1.0/((1.0/CFCoefA(n))+(1.0/CFCoefB(n)-1.0/CFCoefA(n))*(exp((FPrint(1:maxclass,1:cargrp,n)-CFCoefC(n))/CFCoefD(n))/ &
                                         (1.0+exp((FPrint(1:maxclass,1:cargrp,n)-CFCoefC(n))/CFCoefD(n)))))
        END WHERE
        WHERE (fprint(1:maxclass,ltkgrp:maxgroup,n).ne.0.0) ! light trucks
          FPMpg(1:maxclass,ltkgrp:maxgroup,n) = 1.0/((1.0/TFCoefA(n))+(1.0/TFCoefB(n)-1.0/TFCoefA(n))*(exp((FPrint(1:maxclass,ltkgrp:maxgroup,n)-TFCoefC(n))/TFCoefD(n))/ &
                                                (1.0+exp((FPrint(1:maxclass,ltkgrp:maxgroup,n)-TFCoefC(n))/TFCoefD(n)))))
        END WHERE
      else
        WHERE (fprint(1:maxclass,1:cargrp,n).ne.0.0)    ! cars
          FPMpg(1:maxclass,1:cargrp,n) = 1.0/(min(max(((CFCoefC2(n)*FPrint(1:maxclass,1:cargrp,n))+CFCoefD2(n)),1.0/CFCoefA2(n)),1.0/CFCoefB2(n)))
        END WHERE
        WHERE (fprint(1:maxclass,ltkgrp:maxgroup,n).ne.0.0) ! light trucks
          FPMpg(1:maxclass,ltkgrp:maxgroup,n) = MAX(1.0/(min(max(((TFCoefC2(n)*FPrint(1:maxclass,ltkgrp:maxgroup,n))+TFCoefD2(n)),1.0/TFCoefA2(n)),1.0/TFCoefB2(n))),	&	
                                                1.0/(min(max(((TFCoefG2(n)*FPrint(1:maxclass,ltkgrp:maxgroup,n))+TFCoefH2(n)),1.0/TFCoefE2(n)),1.0/TFCoefF2(n))))
        END WHERE
      endif

!...  Calculate the mpg standard for the group, weighted up over the classes.
      do jgp = 1,maxgroup
		FPMpgGrp(jgp,n) = HARMONIC_MEAN_1D(FPMpg(1:maxclass,jgp,n), &
                                           sum(cafesales(jgp,1:maxclass,yrs,1:maxldv), DIM=2), &
                                           caller_id = 'FPMpgGrp')
      enddo
    endif

!...Use the traditional standard before 2011 and the foot print standard after.
    if(yrs.le.2010) Cafe_Used(1:maxgroup,yrs)=Cafe_Stand(1:maxgroup,yrs)
    if(yrs.ge.2011) Cafe_Used(1:maxgroup,yrs)=FPMpgGrp(1:maxgroup,n)

!...calculate cafe standard for fleet
	cafestd(1,n) = HARMONIC_MEAN_1D(cafe_used(1:cargrp,yrs), &
                                    GrpShare(mnumcr,1:cargrp,n), &
                                    caller_id = 'cafestd(1)')
    cafestd(2,n) = HARMONIC_MEAN_1D(cafe_used(ltkgrp:maxgroup,yrs), &
                                    GrpShare(mnumcr,ltkgrp:maxgroup,n), &
                                    caller_id = 'cafestd(2)')
    cafestd(3,n) = HARMONIC_MEAN_1D(cafestd(1:2,n), &
                                    cartrksplit(mnumcr,1:2,n), &
                                    caller_id = 'cafestd(3)')

!...calculate cafe, tested, and on-road mpg vaules compute the average new car and light truck mpg    
!...collapse new car and light truck mpg from 11 group and 8 size classes to 1 - for table 7 
!...femmpg benchmarked to historical data after xyr	

!...compliance mpg (with credits) for ftab Table 7
	if(curcalyr.le.epalyr) then
	  do jgp=1,maxgroup
        CafeMpgGrp(jgp,n) = HARMONIC_MEAN_2D(mpgcomp(jgp,1:maxclass,yrs,1:maxldv),&
                                             cafesales(jgp,1:maxclass,yrs,1:maxldv),&
                                             ac_oc_credit_expanded(jgp,1:maxclass,1:maxldv,n),&
                                             caller_id='CafeMpgGrp(<epalyr)')
      enddo 
	elseif(curcalyr.gt.epalyr) then
      do jgp=1,maxgroup
        if (ENFORCE_MY27REGS.eq.1.and.curcalyr.ge.2027) then
          CafeMpgGrp(jgp,n) = HARMONIC_MEAN_2D(femmpg(jgp,1:maxclass,yrs,1:maxldv)*cafepefmult_expanded(jgp,1:maxclass,1:maxldv,n),&
                                               cafesales(jgp,1:maxclass,yrs,1:maxldv),&
                                               ac_oc_credit_expanded_adj(jgp,1:maxclass,1:maxldv,n),&
                                               caller_id='CafeMpgGrp(>=epalyr)')
        else
          CafeMpgGrp(jgp,n) = HARMONIC_MEAN_2D(femmpg(jgp,1:maxclass,yrs,1:maxldv)*cafepefmult_expanded(jgp,1:maxclass,1:maxldv,n),&
                                               cafesales(jgp,1:maxclass,yrs,1:maxldv),&
                                               ac_oc_credit_expanded(jgp,1:maxclass,1:maxldv,n),&
                                               caller_id='CafeMpgGrp(>=epalyr)')
        endif
      enddo
	endif

!...Check individual group CAFE against the standard.
!   Do the same for EPA tailpipe GHG, if ENFORCE_EPA option is selected
    if(cafetestcall.eq.0) then 
      do jgp=1,MAXGROUP
        ivtyp = GrpMap(jgp)
        cafepass(jgp)= .true.
        if(CafeMpgGrp(jgp,n).lt.Cafe_Used(jgp,yrs)) cafepass(jgp)= .false.
        if(ENFORCE_EPA.eq.1.and.MgGhgGrp(jgp,n).lt.0.0) cafepass(jgp)= .false.

        if (CAFEGHG_DEBUG.eq.1) then
          if(curcalyr.gt.2022.and.fcrl.eq.1.and.jgp.eq.1) WRITE(21,'(a14,",",2(a4,","),a12,",",8(a12,","))')'cafepass','year','grp','pass?','compliance','standard','comp_car','stndrd_car','comp_trk','stndrd_trk','comp_all','stndrd_all'
          if(curcalyr.gt.2022.and.fcrl.eq.1.and.FEM_PASS.eq.3) WRITE(21,'(a14,",",2(i4,","),l12,",",5(f12.1,","))')'cafepass_nhtsa',curcalyr,jgp,cafepass(jgp),CafeMpgGrp(jgp,n),Cafe_Used(jgp,yrs),cafestd(3,n)
          if(curcalyr.gt.2022.and.fcrl.eq.1.and.FEM_PASS.eq.3) WRITE(21,'(a14,",",2(i4,","),l12,",",8(f12.1,","))')'cafepass_epa',curcalyr,jgp,cafepass(jgp),MgGhgGrp(jgp,n),0.0, &
                                                                                        sum(MgGhgGrp(1:cargrp,n)),0.0,sum(MgGhgGrp(6:11,n)),0.0,sum(MgGhgGrp(1:11,n)),0.0
        endif
      end do
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
        if(FEM_PASS.eq.1.and.curitr.eq.1) then
          CafeBank(5,jgp) = CafeWork(4,jgp)
          CafeBank(4,jgp) = CafeWork(3,jgp)
          CafeBank(3,jgp) = CafeWork(2,jgp)
          CafeBank(2,jgp) = CafeWork(1,jgp)
          CafeBank(1,jgp) = CafeBankA(jgp)
          CafeBankA(jgp)  = 0.0
        endif
!...    On first pass and every iteration, put the saved values into working values. This is necessary
!...    so that we have the fresh bank values at the start of each new iteration.
        if(FEM_PASS.eq.1) then
          CafeWork(5,jgp) = CafeBank(5,jgp)
          CafeWork(4,jgp) = CafeBank(4,jgp)
          CafeWork(3,jgp) = CafeBank(3,jgp)
          CafeWork(2,jgp) = CafeBank(2,jgp)
          CafeWork(1,jgp) = CafeBank(1,jgp)
!...    On the first pass, if the group passed, then bank its excess MPG. Otherwise, pull values out of the bank.
          if(CafePass(jgp).eq. .true.) then
            CafeBankA(jgp) = CafeMpgGrp(jgp,n)-Cafe_Used(jgp,yrs)
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
          endif
        endif
      enddo
    endif
    
    bankbal(1:maxgroup,yrs)=sum(CafeBank(1:5,1:maxgroup), DIM=1)

! Aggregate final vehicle attributes for reporting
! In the projection, this is only executed after CAFE/GHG regs have been met (or after the model has exhausted
! all possible options). No need to re-calculate reported values if they are about to be adjusted in FEMCALC or CAFEGHG_MEET
  IF (cafetestcall.eq.1.or.curcalyr.le.epalyr+1) THEN

!...tested mpg (without credits) for ftab table 7
    TrueMPG(1,n) = HARMONIC_MEAN_3D(FemMpg(1:cargrp,1:maxclass,yrs,1:maxldv), &
                                    cafesales(1:cargrp,1:maxclass,yrs,1:maxldv),&
                                    caller_id='TrueMPG(1)')
    TrueMPG(2,n) = HARMONIC_MEAN_3D(FemMpg(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv), &
                                    cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv),&
                                    caller_id='TrueMPG(2)')
    TrueMPG(3,n) = HARMONIC_MEAN_3D(FemMpg(1:maxgroup,1:maxclass,yrs,1:maxldv), &
                                    cafesales(1:maxgroup,1:maxclass,yrs,1:maxldv),&
                                    caller_id='TrueMPG(3)')

!...calculate mpgadj after xyr 
	if(curcalyr.gt.epalyr) then
	  mpgadj(1:maxgroup,1:maxclass,yrs,1:maxldv) = femmpg(1:maxgroup,1:maxclass,yrs,1:maxldv)*degfacgrp(1:maxgroup,1:maxclass,1:maxldv,n)
	endif

!...on-road mpg (adjusted tested) for ftab table 7
    AdjMpg(1,n) = HARMONIC_MEAN_3D(mpgadj(1:cargrp,1:maxclass,yrs,1:maxldv), &
                                    cafesales(1:cargrp,1:maxclass,yrs,1:maxldv),&
                                    caller_id='AdjMpg(1)')
    AdjMpg(2,n) = HARMONIC_MEAN_3D(mpgadj(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv), &
                                    cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv),&
                                    caller_id='AdjMpg(2)')
    AdjMpg(3,n) = HARMONIC_MEAN_3D(mpgadj(1:maxgroup,1:maxclass,yrs,1:maxldv), &
                                    cafesales(1:maxgroup,1:maxclass,yrs,1:maxldv),&
                                    caller_id='AdjMpg(3)')

!...calculate US new ldv fuel economy by ildv for ftab table 50
	ldvmpgnew(:,:,:,n) = 0.0
    do ildv=1,maxldv
      ldvmpgnew(mnumcr,1,ildv,n) = HARMONIC_MEAN_2D(FemMpg(1:cargrp,1:maxclass,yrs,ildv), &
                                    cafesales(1:cargrp,1:maxclass,yrs,ildv),&
                                    caller_id='ldvmpgnew(mnumcr,1)')
      ldvmpgnew(mnumcr,2,ildv,n) = HARMONIC_MEAN_2D(FemMpg(ltkgrp:maxgroup,1:maxclass,yrs,ildv), &
                                    cafesales(ltkgrp:maxgroup,1:maxclass,yrs,ildv),&
                                    caller_id='ldvmpgnew(mnumcr,2)')
    enddo
	
!...fill regional mpg values through 2018, regional = US
	if(curcalyr.le.2018) then
      ldvmpgnew(1:mnumcr-2, 1:maxvtyp, 1:maxldv, n) = SPREAD(ldvmpgnew(mnumcr, 1:maxvtyp, 1:maxldv, n), DIM=1, NCOPIES=mnumcr-2)
	endif ! <2018

!...calculate regional mpg >= 2019
	if(curcalyr.ge.2019) then
	  do iregn=1,mnumcr-2
	    do ildv=1,maxldv
          ldvmpgnew(iregn,1,ildv,n) = HARMONIC_MEAN_2D(FemMpg(1:cargrp,1:maxclass,yrs,ildv), &
                                                       ldv_sales(1:cargrp,1:maxclass,ildv,iregn,n),&
                                                       caller_id='ldvmpgnew(1)')
          ldvmpgnew(iregn,2,ildv,n) = HARMONIC_MEAN_2D(FemMpg(ltkgrp:maxgroup,1:maxclass,yrs,ildv), &
                                                       ldv_sales(ltkgrp:maxgroup,1:maxclass,ildv,iregn,n),&
                                                       caller_id='ldvmpgnew(2)')
        ENDDO
      
        truempg_regn(iregn,1,n) = HARMONIC_MEAN_1D(ldvmpgnew(iregn,1,1:maxldv,n), &
                                                   sum(sum(ldv_sales(1:cargrp,1:maxclass,1:maxldv,iregn,n),DIM=1),DIM=1),&
                                                   caller_id='truempg_regn(1)')
        truempg_regn(iregn,2,n) = HARMONIC_MEAN_1D(ldvmpgnew(iregn,2,1:maxldv,n), &
                                                   sum(sum(ldv_sales(ltkgrp:maxgroup,1:maxclass,1:maxldv,iregn,n),DIM=1),DIM=1),&
                                                   caller_id='truempg_regn(2)')
      
      ENDDO
	endif ! >= 2019
	
!...calculate mpg on-road adjustment factors by ildv
	if(curcalyr.le.epalyr) then
	  do ildv=1,maxldv
        mpgadjldv(1,ildv,n) = HARMONIC_MEAN_2D(mpgadj(1:cargrp,1:maxclass,yrs,ildv), &
                                                cafesales(1:cargrp,1:maxclass,yrs,ildv),caller_id='mpgadjldv(1)')
        mpgadjldv(2,ildv,n) = HARMONIC_MEAN_2D(mpgadj(ltkgrp:maxgroup,1:maxclass,yrs,ildv), &
                                                cafesales(ltkgrp:maxgroup,1:maxclass,yrs,ildv),caller_id='mpgadjldv(2)')
      enddo

	  do ivtyp=1,maxvtyp
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
	  degrpt(1:maxvtyp,n)          = adjmpg(1:maxvtyp,n)/truempg(1:maxvtyp,n)
      degfac(1:maxvtyp,1:maxldv,n) = degfac(1:maxvtyp,1:maxldv,n-1)
	endif
	
!...calculate mpg degredation factor by jgp, icl, ildv
	if(curcalyr.le.epalyr) then
	  WHERE (femmpg(1:maxgroup,1:maxclass,yrs,1:maxldv).ne.0.0)
        degfacgrp(1:maxgroup,1:maxclass,1:maxldv,n) = mpgadj(1:maxgroup,1:maxclass,yrs,1:maxldv)/femmpg(1:maxgroup,1:maxclass,yrs,1:maxldv)
      END WHERE
      
      DO ildv=1,maxldv
        WHERE(degfacgrp(1:cargrp,1:maxclass,ildv,n).eq.0.0)         ! cars
          degfacgrp(1:cargrp,1:maxclass,ildv,n) = degfac(1,ildv,n) 
        END WHERE
        WHERE(degfacgrp(ltkgrp:maxgroup,1:maxclass,ildv,n).eq.0.0)  ! light trucks
          degfacgrp(ltkgrp:maxgroup,1:maxclass,ildv,n) = degfac(2,ildv,n) 
        END WHERE
      ENDDO

	else  ! set it equal to previous year
	  degfacgrp(1:maxgroup,1:maxclass,1:maxldv,n) = degfacgrp(1:maxgroup,1:maxclass,1:maxldv,n-1)
	endif

!...Calculate average fuel economies for different tables.
!   Average new tested mpg with credits and banking
!...for Table 7	
    NewMPG(1,n) = HARMONIC_MEAN_1D(cafempggrp(1:cargrp,n), &
                                   sum(sum(cafesales(1:cargrp,1:maxclass,yrs,1:maxldv),DIM=2),DIM=2),&
                                   caller_id='NewMpg(1)')
    NewMPG(2,n) = HARMONIC_MEAN_1D(cafempggrp(ltkgrp:maxgroup,n), &
                                   sum(sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv),DIM=2),DIM=2),&
                                   caller_id='NewMpg(2)')
    NewMPG(3,n) = HARMONIC_MEAN_1D(cafempggrp(1:maxgroup,n), &
                                   sum(sum(cafesales(1:maxgroup,1:maxclass,yrs,1:maxldv),DIM=2),DIM=2),&
                                   caller_id='NewMpg(3)')

!   Average alternative-fuel vehicle mpg by size class (Table 52)
!   AFVs = anything that isn't non-HEV gasoline or diesel
	do icl=1,maxclass
      TREFFALTC(icl,n) = HARMONIC_MEAN_2D(FemMpg(1:cargrp,icl,yrs,3:maxldv),&
                                          cafesales(1:cargrp,icl,yrs,3:maxldv),&
                                          caller_id='TREFFALTC')
      TREFFALTT(icl,n) = HARMONIC_MEAN_2D(FemMpg(ltkgrp:maxgroup,icl,yrs,3:maxldv),&
                                          cafesales(ltkgrp:maxgroup,icl,yrs,3:maxldv),&
                                          caller_id='TREFFALTT')
    enddo

    TREFFALTC(maxclass+1,n) = HARMONIC_MEAN_3D(FemMpg(1:cargrp,1:maxclass,yrs,3:maxldv), &
                                               cafesales(1:cargrp,1:maxclass,yrs,3:maxldv),&
                                               caller_id='TREFFALTC')
    TREFFALTT(maxclass+1,n) = HARMONIC_MEAN_3D(FemMpg(ltkgrp:maxgroup,1:maxclass,yrs,3:maxldv), &
                                               cafesales(ltkgrp:maxgroup,1:maxclass,yrs,3:maxldv),&
                                               caller_id='TREFFALTT')

!...Calculate vehicle group-average technology penetration rate (%) and cost, % for ftab Table 60 
!...calculate historical values from input data, then projected values 
    If(curcalyr.ge.xyr)then 
      if(curcalyr.eq.xyr) then
        MKT_D_P(1:3,1:MAXTECH,:) = 0.0
        AvgCost(1:3,1:MAXTECH,:) = 0.0
      else
        MKT_D_P(1:3,1:MAXTECH,yrs) = 0.0
        AvgCost(1:3,1:MAXTECH,yrs) = 0.0
      endif

      DO ITECH=1,NUMTECH
        DO ILDV=1,MAXLDV
          MKT_PENF(1:maxgroup,ITECH,ILDV) = SUM(MKT_PEN(1:maxgroup,1:maxclass,ITECH,CURRENT,ILDV) * TRANSPOSE(CLASS_SHARE(mnumcr,1:maxclass,1:maxgroup,YRS)) * 100.0,DIM=2)
          AVCOST(1:maxgroup,ITECH,ILDV)   = SUM(TEC_ORNL(1:maxclass,1:maxgroup,ITECH,ILDV) * CLASS_SHARE(mnumcr,1:maxclass,1:maxgroup,YRS),DIM=1)
        ENDDO
      ENDDO

!...Sum over the manufacturer groups to produce a market penetration rate (%) and average
!...cost (90_) tables, but only for gasoline vehicles.
      do jgp = 1,MAXGROUP
        it = GrpMap(jgp)
        MKT_D_P(it,1:numtech,yrs) = MKT_D_P(it,1:numtech,yrs) + (Mkt_Penf(jgp,1:numtech,gas) * GrpShare(mnumcr,jgp,n))
        AvgCost(it,1:numtech,yrs) = AvgCost(it,1:numtech,yrs) + (AvCost(jgp,1:numtech,gas) * GrpShare(mnumcr,jgp,n))
      enddo
      MKT_D_P(3,1:numtech,yrs) = MKT_D_P(1,1:numtech,yrs) * CarTrkSplit(mnumcr,1,N) + MKT_D_P(2,1:numtech,yrs) * CarTrkSplit(mnumcr,2,N)
      AvgCost(3,1:numtech,yrs) = AvgCost(1,1:numtech,yrs) * CarTrkSplit(mnumcr,1,N) + AvgCost(2,1:numtech,yrs) * CarTrkSplit(mnumcr,2,N)

!...sum micro hybrid vehicle sales shares - for ftab Table 48
!...update this equation when tech list is updated (64 = 12V micro hybrid, 65 = BISG)
!      micropen(1:maxvtyp,1:maxldv,n) = 0.0
      do ildv = 1, maxldv
        micropen(1,ILDV,n) = SUM( SUM(Mkt_Penf(1:cargrp,[64,65],ILDV),DIM=2) / 100 * GrpShare(mnumcr,1:cargrp,n) )
        micropen(2,ILDV,n) = SUM( SUM(Mkt_Penf(ltkgrp:maxgroup,[64,65],ILDV),DIM=2) / 100 * GrpShare(mnumcr,ltkgrp:maxgroup,n) )
      enddo
      
	endif ! >= xyr year

!...Calculate fuel economy, hp, price, and curb weight by vtyp, icl and ildv
!   I.e., collapse the igp dimension into ivtyp
	do icl=1,maxclass
	  do ildv=1,maxldv
      
!       Fuel economy (Table 113)
        LDVMPG(1,ildv,icl,yrs) = HARMONIC_MEAN_1D(FemMpg(1:cargrp,icl,yrs,ildv), &
                                                  cafesales(1:cargrp,icl,yrs,ildv),caller_id='LDVMPG(1)')
        LDVMPG(2,ildv,icl,yrs) = HARMONIC_MEAN_1D(FemMpg(ltkgrp:maxgroup,icl,yrs,ildv), &
                                                  cafesales(ltkgrp:maxgroup,icl,yrs,ildv),caller_id='LDVMPG(2)')
!...	MSRP, vehicle price (Table 114)
        LDVPRI(1,ildv,icl,yrs) = WEIGHTED_MEAN_1D(fempri(1:cargrp,icl,yrs,ildv), &
                                                  cafesales(1:cargrp,icl,yrs,ildv),'LDVPRI(1)')
        LDVPRI(2,ildv,icl,yrs) = WEIGHTED_MEAN_1D(fempri(ltkgrp:maxgroup,icl,yrs,ildv), &
                                                  cafesales(ltkgrp:maxgroup,icl,yrs,ildv),'LDVPRI(2)')
!...	Driving range (Table 115)
        LDVRNG(1,ildv,icl,yrs) = WEIGHTED_MEAN_1D(femrng(1:cargrp,icl,yrs,ildv), &
                                                  cafesales(1:cargrp,icl,yrs,ildv),'LDVRNG(1)')
        LDVRNG(2,ildv,icl,yrs) = WEIGHTED_MEAN_1D(femrng(ltkgrp:maxgroup,icl,yrs,ildv), &
                                                  cafesales(ltkgrp:maxgroup,icl,yrs,ildv),'LDVRNG(2)')
!...	Vehicle weight (Table 52)
        WGT(1,ildv,icl,yrs) = WEIGHTED_MEAN_1D(femwgt(1:cargrp,icl,yrs,ildv), &
                                                  cafesales(1:cargrp,icl,yrs,ildv),'WGT(1)')
        WGT(2,ildv,icl,yrs) = WEIGHTED_MEAN_1D(femwgt(ltkgrp:maxgroup,icl,yrs,ildv), &
                                                  cafesales(ltkgrp:maxgroup,icl,yrs,ildv),'WGT(2)')
	  enddo
	enddo

!...Calculate average horsepower and weight for new cars and light trucks for Table 52
!   and size class choice model
!   National
    AWTCAR(mnumcr,n)    = WEIGHTED_MEAN_3D(femwgt(1:cargrp,1:maxclass,yrs,1:maxldv), &
                                           cafesales(1:cargrp,1:maxclass,yrs,1:maxldv),'AWTCAR')
    AWTTRUCK(mnumcr,n)  = WEIGHTED_MEAN_3D(femwgt(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv), &
                                           cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv),'AWTTRUCK')
    AHPCAR(mnumcr,n)    = WEIGHTED_MEAN_3D(femhp(1:cargrp,1:maxclass,yrs,1:maxldv), &
                                           cafesales(1:cargrp,1:maxclass,yrs,1:maxldv),'AHPCAR')
    AHPTRUCK(mnumcr,n)  = WEIGHTED_MEAN_3D(femhp(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv), &
                                           cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv),'AHPTRUCK')
!   Regional
    if (curcalyr.lt.2019) then  ! No detailed sales pre-2019
      AHPCAR(1:mnumcr-2,n)   = AHPCAR(mnumcr,n) 
      AHPTRUCK(1:mnumcr-2,n) = AHPTRUCK(mnumcr,n)
      AWTCAR(1:mnumcr-2,n)   = AWTCAR(mnumcr,n)  
      AWTTRUCK(1:mnumcr-2,n) = AWTTRUCK(mnumcr,n)
    else
      do iregn=1,mnumcr-2
        AWTCAR(iregn,n)  = WEIGHTED_MEAN_3D(femwgt(1:cargrp,1:maxclass,yrs,1:maxldv), &
                                             ldv_sales(1:cargrp,1:maxclass,1:maxldv,iregn,n),'AWTCAR_regn')
        AWTTRUCK(iregn,n)= WEIGHTED_MEAN_3D(femwgt(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv), &
                                             ldv_sales(ltkgrp:maxgroup,1:maxclass,1:maxldv,iregn,n),'AWTTRUCK_regn')
        AHPCAR(iregn,n)  = WEIGHTED_MEAN_3D(femhp(1:cargrp,1:maxclass,yrs,1:maxldv), &
                                             ldv_sales(1:cargrp,1:maxclass,1:maxldv,iregn,n),'AHPCAR_regn')
        AHPTRUCK(iregn,n)= WEIGHTED_MEAN_3D(femhp(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv), &
                                             ldv_sales(ltkgrp:maxgroup,1:maxclass,1:maxldv,iregn,n),'AHPTRUCK_regn')
      enddo
    endif

!...calculate total ldv sales by size class and fueling technology (Table 52) 
	TOTALSALSC(1,1:MAXCLASS,1:MAXLDV,n) = SUM(cafesales(1:cargrp,1:maxclass,yrs,1:maxldv), DIM=1)
	TOTALSALSC(2,1:MAXCLASS,1:MAXLDV,n) = SUM(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv), DIM=1)
    
!...recalculate total expenditures on vehicle purchases
    DO IVTYP=1,MAXVTYP
      EXPENDVEH(IVTYP,N) = SUM( TOTALSALSC(IVTYP,1:MAXCLASS,1:MAXLDV,N) * TRANSPOSE(LDVPRI(IVTYP,1:MAXLDV,1:MAXCLASS,YRS)) )
    ENDDO

  ENDIF !

  RETURN
  END SUBROUTINE CAFECALC

! =================================================================================================================
! ... Subroutine CAFEGHG_MEET determines the least cost, alternatively fueled vehicle    
!     by size class and incrementally increases sales of these vehicles to meet CAFE/GHG shortfall.
!     Given the latest market and regulatory developments (post-2020), the powertrain options for achieving 
!     compliance in this subroutine are limited to plug-ins, specifically PHEV50, EV200, and EV300.
! =================================================================================================================
  SUBROUTINE CAFEGHG_MEET
  USE T_
  USE MEAN_FUNCS
  IMPLICIT NONE

  INTEGER, PARAMETER :: MAXVEH = 3							!...ILDV fuel types availble for compliance

  INTEGER :: l,xldv,xcl,ycl,xpass,stop_gas,jgp			    !...local looping, array, and conditional variables			
  INTEGER cafeveh(MAXVEH) /6,7,15/					        !...ILDV fuel types available for compliance
  INTEGER carcls(8) /7,8,4,3,2,5,6,1/						!...passenger car size classes
  INTEGER trkcls(8) /7,8,2,5,6,4,3,1/						!...light-duty truck size classes 
  REAL :: delta_fuel__D_(maxgroup,maxldv,maxclass,mnumyr)	!...fuel savings by fuel type compared to conventional gasoline
  REAL :: delta_price(maxgroup,maxldv,maxclass,mnumyr)		!...incremental ILDV vehicle price compared to convetional gasoline
  REAL :: factor__D_(maxgroup,maxldv,maxclass,mnumyr)		!...fuel economics cost effectiveness (fuel savings-incremental vehicle cost)
  REAL :: asort(MAXVEH)                                     !...sort by cost effectiveness
  REAL :: isort(MAXVEH)                                     !...sort by order
  REAL :: xsort(MAXVEH,maxgroup,maxclass,mnumyr)			!...saved sort order 
  REAL :: delta(maxgroup,maxclass)							!...maximum sales change per pass(loop)
  REAL :: apshr55_reg(maxgroup,maxclass,mnumcr,maxldv)		!...sales shares by region
  REAL :: NUM1,NUM2,DEN1,DEN2,DEN3,TEMPMPG
  REAL :: ac_oc_credit_n_val
  REAL :: sales_by_grpcls(maxgroup,maxclass),Mg_deficit_by_grp(maxgroup), tot_by_class(maxgroup), &
          sales_tot,Mg_deficit_tot,sum_ldvsales_ldv,sum_ldvsales_natl        ! Temporary vars for calcs
  INTEGER, PARAMETER :: PAYB = 5							!...years of payback for fuel economics calculation
  INTEGER, PARAMETER :: MAXPASS = 15						!...number of loop passes for adding vehicle sales

!   Timers
    INTEGER(KIND=4) :: start_count2, end_count2, count_rate, count_max
    REAL(KIND=8)    :: elapsed_time_seconds2,elapsed_time_seconds_cumul
    INTEGER         :: result
 
!...set maximum allowable increase in alternatively fueled vehicle sales by manufacturer
!   Share out the total (MAXADJ) by the distribution of total gasoline sales across group and class (proxy for compliance shortfall)
!   Only calculated once per year. Units: million vehicles
	if (first_time_cafetest) then
      sales_by_grpcls(:,:) = 0.0
      delta(:,:) = 0.0
      do jgp=1,maxgroup
        do icl=1,maxclass
          if(sum(femmpg(jgp,icl,yrs,cafeveh(1:maxveh))).gt.0.0.and..not.cafepass(jgp)) then     ! Only where powertrains in cafeveh exist
            sales_by_grpcls(jgp,icl) = sales_by_grpcls(jgp,icl) + cafesales(jgp,icl,yrs,1)
          endif
	    enddo
      enddo
      
      sales_tot = sum(sales_by_grpcls(1:maxgroup,1:maxclass))
      
      do jgp=1,maxgroup
        if(cafepass(jgp)) CYCLE
        do icl=1,maxclass
          delta(jgp,icl) = 1./REAL(MAXPASS)*MAXADJ_CAFETEST * sales_by_grpcls(jgp,icl)/sales_tot
        enddo
      enddo

!     If applying EPA GHG, distribute compliance burden across groups using actual compliance shortfall (negative Mgs); then
!     distribute across size classes using dist. of gasoline vehicle sales
      if (ENFORCE_EPA.eq.1) then
        delta(:,:) = 0.0
        Mg_deficit_by_grp(:) = 0.0
        
        do jgp=1,maxgroup
          if(.not.cafepass(jgp)) then
            Mg_deficit_by_grp(jgp) = Mg_deficit_by_grp(jgp) + MgGhgGrp(jgp,n)
          endif
        enddo
        
        Mg_deficit_tot = sum(Mg_deficit_by_grp(1:maxgroup))
        tot_by_class(1:maxgroup) = sum(sales_by_grpcls(1:maxgroup,1:maxclass),DIM=2)
 
        do jgp=1,maxgroup
          if(cafepass(jgp)) CYCLE
          do icl=1,maxclass
            delta(jgp,icl) = 1./REAL(MAXPASS)*MAXADJ_CAFETEST * Mg_deficit_by_grp(jgp)/Mg_deficit_tot * sales_by_grpcls(jgp,icl)/tot_by_class(jgp)
          enddo
        enddo
      endif
      
      first_time_cafetest = .false.
    endif

!...save initial vehicle sales by manufacturer
	avsales_old(1:maxgroup,1:maxclass,1:maxldv,n) = cafesales(1:maxgroup,1:maxclass,yrs,1:maxldv)

!...calculate cost effectiveness of advanced technology vehicles relative to conventional gasoline
!...calculate discounted fuel savings (using gasoline price as "fuel price" for all powertrains)
	delta_fuel__D_=0.
    factor__D_(igp,cafeveh(:),1:maxclass,n)=-50000.
    if(igp.le.cargrp) VMT(1:payb,mnumcr)=PVMT(1:payb,VMTYR-1989,mnumcr,1)
    if(igp.ge.ltkgrp) VMT(1:payb,mnumcr)=LVMT(1:payb,VMTYR-1989,mnumcr,1)
    
	do l=1,maxveh
	  xldv=cafeveh(l)
	  do icl=1,maxclass
		if(femmpg(igp,icl,yrs,xldv).ne.0.) then
!         Estimate fuel savings over the time period "payb", using a 7% discount rate.
          delta_fuel__D_(igp, xldv, icl, n) = sum( VMT(1:payb, mnumcr) * &
                                                   (1.0 / femmpg(igp, icl, yrs, gas) - 1.0 / femmpg(igp, icl, yrs, xldv)) * &
                                                   FPRICE(1, mnumcr, YRS) * &
                                                   ((1.07)**-real( (/ (i, i=1, payb) /), kind=KIND(1.0D0))) &
                                                  )
!...      calculate incremental vehicle price
		  delta_price(igp,xldv,icl,n) = fempri(igp,icl,yrs,xldv)-fempri(igp,icl,yrs,gas)
!...      calculate fuel economics cost effectiveness factor
		  factor__D_(igp,xldv,ICL,n) = delta_fuel__D_(igp,xldv,icl,n) - delta_price(igp,xldv,icl,n)
		endif
	  enddo
	enddo

!...sort factor_$ by most cost effective
	do icl=1,maxclass    
	  do l=1,maxveh
		xldv=cafeveh(l)
		asort(l) = -1.0 * factor__D_(igp,xldv,icl,n)
		isort(l)=cafeveh(l)
	  enddo
!     Select best sorting algorithm, depending on number of powertrain options
      if (maxveh.gt.10) then
        call RSORT(asort,isort,maxveh,maxveh)
      else
	    call RSORT_SMALL(asort,isort,maxveh)
      endif
	  do l=1,maxveh
		xsort(l,igp,icl,n)= isort(l)
	  enddo
	enddo

!...increase sales of alternative fuel vehicles in order of cost effectiveness to meet CAFE standard
    MgGhgGrp(igp,n) = 0.0
    ac_oc_credit_n_val = ac_oc_credit(igp,n)*8887.0
	outer: do xcl=1,maxclass
	  if(cafepass(IGP)) exit	  
	  if(igp.le.cargrp) ycl=carcls(xcl)
	  if(igp.ge.ltkgrp) ycl=trkcls(xcl)
	  if(avsales_old(igp,ycl,gas,n).gt.0.) then
		inner: do l=1,maxveh
		  xldv=xsort(l,igp,ycl,n)
!...      Ensure alternative fuel vehicle and class sales exist for manufacturer and class
		  if(sum(mmavail(igp,xldv,ycl,1:mnumcr-2,yrs)).gt.0.0) then
!...        add alternative fuel vehicle sales until CAFE is met
			stop_gas=0
!...        incrementally add alternative fuel vehicle sales to meet CAFE
			do xpass=1,maxpass
!...          subtract delta alternative vehicle sales from initial gasoline vehicle sales
!...          add delta alternative vehicle sales to inital advanced vehicle sales                
			  if(stop_gas.eq.0)then
				cafesales(igp,ycl,yrs,1) = cafesales(igp,ycl,yrs,1) - delta(igp,ycl)
				cafesales(igp,ycl,yrs,xldv) = cafesales(igp,ycl,yrs,xldv) + delta(igp,ycl)
!...            stop if conventional gasoline vehicle sales go to zero or negative                
				if(cafesales(igp,ycl,yrs,1).le.0.)then
				  if(stop_gas.eq.0)then
					cafesales(igp,ycl,yrs,1) = cafesales(igp,ycl,yrs,1) + delta(igp,ycl)
					cafesales(igp,ycl,yrs,xldv) = cafesales(igp,ycl,yrs,xldv) - delta(igp,ycl)
				  endif
				  stop_gas=1
				endif
			  endif
              
!...		  For each xpass calculate new fleet average mpg and g/mi.  Exit if standard(s) met.
!             Calculate the new group average mpg, g/mi, and MgGHG
              if (ENFORCE_MY27REGS.eq.1.and.curcalyr.ge.2027) then
                CafeMpgGrp(igp,n) = HARMONIC_MEAN_2D(femmpg(igp,1:maxclass,yrs,1:maxldv)*cafepefmult_expanded(igp,1:maxclass,1:maxldv,n),&
                                                     cafesales(igp,1:maxclass,yrs,1:maxldv),&
                                                     ac_oc_credit_expanded_adj(igp,1:maxclass,1:maxldv,n),&
                                                     caller_id='CafeMpgGrp(>=epalyr)')
              else
                CafeMpgGrp(igp,n) = HARMONIC_MEAN_2D(femmpg(igp,1:maxclass,yrs,1:maxldv)*cafepefmult_expanded(igp,1:maxclass,1:maxldv,n),&
                                                     cafesales(igp,1:maxclass,yrs,1:maxldv),&
                                                     ac_oc_credit_expanded(igp,1:maxclass,1:maxldv,n),&
                                                     caller_id='CafeMpgGrp(>=epalyr)')
              endif             

!             Calculate compliance (CO2 credits or debits) using 2-cycle tested mpg
	          ac_oc_credit_n_val = ac_oc_credit(igp,n)*8887.0
              NUM1 = 0.0
	          NUM2 = 0.0
              DEN3 = 0.0
	  	      do ildv=1,maxldv 
	  	        do icl=1,maxclass
	  	          if(femmpg(igp,icl,yrs,ildv).ne.0.0) then
	  	    	    if(igp.le.cargrp) then
	  	    		  if (ENFORCE_MY27REGS.eq.1.and.curcalyr.ge.2027.and.(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15)) then         ! Zero g/mi off-cycle and AC efficiency for BEVs in MY2027+
                        CYCLE
                      elseif (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then                                                      ! No tailpipe GHG, but can earn AC/offcycle creds
                        NUM1 = NUM1 + cafesales(igp,icl,yrs,ildv) * (-ac_oc_credit_n_val)
                      elseif (ildv.eq.5.or.ildv.eq.6) then                                                                    ! For PHEV, only count emissions from gasoline-fueled miles
                        NUM1 = NUM1 + cafesales(igp,icl,yrs,ildv) * (1-phev_evmt(igp,icl,yrs,ildv)) * &
                               (1/PHEVMPG_S(igp,icl,yrs,ildv)*8887 - ac_oc_credit_n_val)
                      else
                        NUM1 = NUM1 + cafesales(igp,icl,yrs,ildv) * (1/femmpg(igp,icl,yrs,ildv)*8887 - ac_oc_credit_n_val)
                      endif
                    else  ! trucks
	  	    		  if (ENFORCE_MY27REGS.eq.1.and.curcalyr.ge.2027.and.(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15)) then         ! Zero g/mi off-cycle and AC efficiency for BEVs in MY2027+
                        CYCLE
                      elseif (ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15.or.ildv.eq.14) then                                        ! No tailpipe GHG, but can earn AC/offcycle creds
                        NUM2 = NUM2 + cafesales(igp,icl,yrs,ildv) * (-ac_oc_credit_n_val)
                      elseif (ildv.eq.5.or.ildv.eq.6) then                                                                    ! For PHEV, only count emissions from gasoline-fueled miles
                        NUM2 = NUM2 + cafesales(igp,icl,yrs,ildv) * (1-phev_evmt(igp,icl,yrs,ildv)) * &
                               (1/(PHEVMPG_S(igp,icl,yrs,ildv))*8887 - ac_oc_credit_n_val)
                      else
!                       Add full-size pickup truck incentives (20g/mile for strong hybrids, where they were 10% of the nameplate sales)
!                       No nameplates were eligible pre-MY2024; no longer available starting MY2025
                        if (curcalyr.eq.2024.and.icl.eq.2.and.ildv.eq.16) then
                          NUM2 = NUM2 + cafesales(igp,icl,yrs,ildv) * (1/femmpg(igp,icl,yrs,ildv)*8887 - ac_oc_credit_n_val - PU_CRED_ELIG(igp)*20.0)
                        else
                          NUM2 = NUM2 + cafesales(igp,icl,yrs,ildv) * (1/femmpg(igp,icl,yrs,ildv)*8887 - ac_oc_credit_n_val)
                        endif
                      endif
	  	    	    endif
	  	    	  endif
	  	        enddo
              enddo
              
!             Calculate extra zero-emission sales to throw in the denominator (Advanced Technology Multipliers)              
              DEN3 = sum( sum(cafesales(igp,1:maxclass,yrs,[4,7,15,14]),DIM=1) * (EPAALTMULT([4,7,15,14],n) - 1) )

              if(igp.le.cargrp) then
                EPAghgGrp(igp,n) = NUM1/(sum(cafesales(igp,1:maxclass,yrs,1:maxldv)) +  DEN3) - AC_CO2_OFFSET(igp,n)
              else
                EPAghgGrp(igp,n) = NUM2/(sum(cafesales(igp,1:maxclass,yrs,1:maxldv)) +  DEN3) - AC_CO2_OFFSET(igp,n)
              endif
              
!             Calculate total MgCO2 credits/debits by group
              MgGhgGrp(1:cargrp,n)        = (FPghgGrp(1:cargrp,n) - EPAghgGrp(1:cargrp,n)) * &
                                            sum(sum(cafesales(1:cargrp,1:maxclass,yrs,1:maxldv),DIM=3),DIM=2) * 195264.0
              MgGhgGrp(ltkgrp:maxgroup,n) = (FPghgGrp(ltkgrp:maxgroup,n) - EPAghgGrp(ltkgrp:maxgroup,n)) * &
                                            sum(sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv),DIM=3),DIM=2) * 225865.0

!             Calculate total fleet average mpg (car, truck, and total) for CAFE
              if(curcalyr.gt.epalyr) then 
                NewMPG(1,n) = HARMONIC_MEAN_1D(cafempggrp(1:cargrp,n), &
                                               sum(sum(cafesales(1:cargrp,1:maxclass,yrs,1:maxldv),DIM=2),DIM=2),&
                                               caller_id='NewMpg(1)')
                NewMPG(2,n) = HARMONIC_MEAN_1D(cafempggrp(ltkgrp:maxgroup,n), &
                                               sum(sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,1:maxldv),DIM=2),DIM=2),&
                                               caller_id='NewMpg(2)')
                NewMPG(3,n) = HARMONIC_MEAN_1D(cafempggrp(1:maxgroup,n), &
                                               sum(sum(cafesales(1:maxgroup,1:maxclass,yrs,1:maxldv),DIM=2),DIM=2),&
                                               caller_id='NewMpg(3)')
              endif

!...		  is standard met?
!             Does the current group meet CAFE . . OR . . does the whole market meet CAFE 
			  if(CafeMpgGrp(igp,n).ge.cafe_used(igp,yrs).or. &                                                                          ! One group meets CAFE
                 (NewMPG(3,n).gt.cafestd(3,n).and.((NewMPG(1,n)-cafestd(1,n)).gt.-2.0).and.((NewMPG(2,n)-cafestd(2,n)).gt.-2.0)).or.&   ! Whole market meets CAFE
                 ENFORCE_CAFE.eq.0) then                                                                                               ! Not enforcing CAFE
!               If not enforcing EPA tailpipe GHG reg, meeting CAFE means this group is good to go
                if (ENFORCE_EPA.eq.0) then
                  cafepass(IGP) = .true.
                  exit outer
!               If enforcing EPA tailpipe GHG reg, does the whole market meet?
!               If so, everyone passes and we're done
                elseif(sum(MgGhgGrp(1:MAXGROUP,n)).gt.0.0) then 
                  if(fcrl.eq.1) WRITE(21,'(i4,a,",",4(i4,","),a,",",f12.0)')curcalyr,' LDV: cafetest_check',xcl,xldv,xpass,igp,'PASSED CAFE/GHG WHOLE MARKET due to aggregate',sum(MgGhgGrp(1:MAXGROUP,n))
                  cafepass(1:MAXGROUP) = .true.
                  exit outer
!               How about if we include previous years carry-forward (only non-zero in EPALYR+2)?
                elseif(sum(MgGhgGrp(1:MAXGROUP,n))+creds_avail.gt.0.0) then 
                  if(fcrl.eq.1) WRITE(21,'(i4,a,",",4(i4,","),a,2(",",f12.0))')curcalyr,' LDV: cafetest_check',xcl,xldv,xpass,igp,'PASSED CAFE/GHG WHOLE MARKET due to aggregate W/ CREDS',sum(MgGhgGrp(1:MAXGROUP,n)),creds_avail
                  cafepass(1:MAXGROUP) = .true.
                  exit outer
!               If not the whole market, does this group?
                elseif(MgGhgGrp(igp,n).gt.0.0) then
                  if(fcrl.eq.1) WRITE(21,'(i4,a,",",4(i4,","),a,",",f12.0)')curcalyr,' LDV: cafetest_check',xcl,xldv,xpass,igp,'PASSED CAFE/GHG ONE GROUP', MgGhgGrp(igp,n)
                  cafepass(IGP) = .true.
                  exit outer
!               How about with credits (only non-zero in EPALYR+2)?
                elseif(MgGhgGrp(igp,n)+creds_avail_grp(igp).gt.0.0) then
                  if(fcrl.eq.1) WRITE(21,'(i4,a,",",4(i4,","),a,2(",",f12.0))')curcalyr,' LDV: cafetest_check',xcl,xldv,xpass,igp,'PASSED CAFE/GHG ONE GROUP W/ CREDS', MgGhgGrp(igp,n),creds_avail_grp(igp)
                  cafepass(IGP) = .true.
                  exit outer
                endif                
              endif ! if current group and total market do not meet CAFE, keep on converting vehicles 
!...          if no gasoline to displace then exit and jump to the next size class
			  if(stop_gas.eq.1) exit inner
			enddo
		  endif
	    enddo inner
	  endif
	enddo outer

!...Regionalize
!   Calculate regional shares
    apshr55_reg = 0.
    do icl = 1, maxclass
      do ildv = 1, maxldv
        sum_ldvsales_natl = sum(ldv_sales(igp, icl, ildv, 1:mnumcr-2, n))
    
        if (sum_ldvsales_natl /= 0.0) then
          apshr55_reg(igp, icl, 1:mnumcr-2, ildv) = ldv_sales(igp, icl, ildv, 1:mnumcr-2, n) / sum_ldvsales_natl
        endif
      enddo
    enddo
    
!   Apply regional shares to national sales total
    do iregn=1,mnumcr-2
      ldv_sales(igp,1:maxclass,1:maxldv,iregn,n) = cafesales(igp,1:maxclass,yrs,1:maxldv) * apshr55_reg(igp,1:maxclass,iregn,1:maxldv)
    enddo
    ldv_sales(igp,1:maxclass,1:maxldv,mnumcr,n) = sum(ldv_sales(igp,1:maxclass,1:maxldv,1:mnumcr-2,n),DIM=3)
    
!...calculate national sales shares
	APShrGrp(igp, 1:maxclass, 1:maxldv, mnumcr, n) = 0.0
    do iregn=1,mnumcr
      if (iregn.eq.10) CYCLE
      do icl=1,maxclass 
	    sum_ldvsales_ldv = sum(ldv_Sales(igp,icl,1:maxldv,iregn,n))
        if (sum_ldvsales_ldv /= 0.0) then
          APShrGrp(igp, icl, 1:maxldv, iregn, n) = ldv_Sales(igp, icl, 1:maxldv, iregn, n) / sum_ldvsales_ldv
        endif
	  enddo  
    enddo

  RETURN
  END SUBROUTINE CAFEGHG_MEET

! ==========================================================================================================
!...Subroutine RSORT_SMALL sorts first Q elements of real array A.  It also sorts an indexing arry, IDX, which
!   can by used to map the sort order to other arrays.  The sort methodology is called Insertion Sort and is 
!   considered an efficieny approach for sorting small arrays.
! ==========================================================================================================
  SUBROUTINE RSORT_SMALL(A,IDX,Q)
  USE T_
  IMPLICIT NONE
  
    INTEGER(KIND=4), INTENT(IN) :: Q
    REAL(KIND=4), INTENT(INOUT) :: A(Q)
    INTEGER(KIND=4), INTENT(INOUT) :: IDX(Q)
    
!   Internal variables for swapping
    REAL(KIND=4) :: temp_real
    INTEGER(KIND=4) :: temp_int
    
!   This is an Insertion Sort that will handle a small number of elements very efficiently.
    DO i = 2, Q
      temp_real = A(i)
      temp_int = IDX(i)
      j = i - 1
      ! Move elements of A(1:i-1) that are greater than temp_real
      ! to one position ahead of their current position
      DO WHILE (.TRUE.)
        IF (j < 1) EXIT
        IF (A(j) <= temp_real) EXIT
        ! Shift elements if conditions above are not met
        A(j + 1) = A(j)
        IDX(j + 1) = IDX(j)
        j = j - 1
      ENDDO
      A(j + 1) = temp_real
      IDX(j + 1) = temp_int
    ENDDO
    
    RETURN
    
  END SUBROUTINE RSORT_SMALL

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
	LOGICAL write_avg_veg_age/.false./
!-----------------------------------------------------------------------------------------------------------------------------------
!...This section of code is implemented to populate reporting tables with historical data for light duty vehicles.  The 
!...1995-2020 vehicle stock values come from - Polk w/adjustments for vehicle stocks assigned to fleet.
!...The stock variables are disaggregated by fuel type, census division, and by fleet

!...if low macro case adjust survival curve to account for declining sales 
    if(MMAC.eq.4.and.curcalyr.gt.STOCKYR+1.and.curitr.eq.1)then
	  if(curcalyr.le.STOCKYR+2)then
	    SSURV25(1:mnumcr, 1:23, 1:maxvtyp) = SSURV25(1:mnumcr, 1:23, 1:maxvtyp)*1.002
        IF(curcalyr.eq.stockyr) SSURV25(1:mnumcr, 1:23, 1:maxvtyp) = SSURV25(1:mnumcr, 1:23, 1:maxvtyp)*1.003
	  endif
    endif

!...fill household vehicle sales 2019-stockyr 
	if(curcalyr.ge.2019.and.curcalyr.le.stockyr) then      
      do iregn=1,mnumcr-2
		hhgrpsal(iregn,1:maxgroup,1:maxclass,1:maxldv,n) = own_sales(1,1:maxgroup,1:maxclass,1:maxldv,iregn,yrs)/1000000.0 
	  enddo 

!     Collapse the group dimension of hhgrpsal to vtype for hhtechsal
      hhtechsal(1:mnumcr-2,1,1:maxclass,1:maxldv,n) = sum(hhgrpsal(1:mnumcr-2,1:cargrp,1:maxclass,1:maxldv,n),DIM=2)
      hhtechsal(1:mnumcr-2,2,1:maxclass,1:maxldv,n) = sum(hhgrpsal(1:mnumcr-2,ltkgrp:maxgroup,1:maxclass,1:maxldv,n),DIM=2)
      
      hhtechsal(mnumcr,1:maxvtyp,1:maxclass,1:maxldv,n) = sum(hhtechsal(1:mnumcr-2,1:maxvtyp,1:maxclass,1:maxldv,n),DIM=1)
      hhgrpsal(mnumcr,1:maxgroup,1:maxclass,1:maxldv,n) = sum(hhgrpsal(1:mnumcr-2,1:maxgroup,1:maxclass,1:maxldv,n),DIM=1)
!	endif

!...fill household vehicle sales in projection (>stockyr) 
    elseif(curcalyr.gt.stockyr) then      
      do igp=1,maxgroup 
	    do icl=1,maxclass 
	      do ildv=1,maxldv
              hhgrpsal(1:mnumcr-2,igp,icl,ildv,n) = ldv_sales(igp,icl,ildv,1:mnumcr-2,n) * ownsaletemp(1,igp,icl,1:mnumcr-2,n)
!...        US sales 
	        hhgrpsal(mnumcr,igp,icl,ildv,n) = sum(hhgrpsal(1:mnumcr-2,igp,icl,ildv,n)) 
	      enddo 
	    enddo 
	  enddo
      
      hhtechsal(1:mnumcr,1,1:maxclass,1:maxldv,n) = sum(hhgrpsal(1:mnumcr,1:cargrp,1:maxclass,1:maxldv,n), DIM=2)
	  hhtechsal(1:mnumcr,2,1:maxclass,1:maxldv,n) = sum(hhgrpsal(1:mnumcr,ltkgrp:maxgroup,1:maxclass,1:maxldv,n), DIM=2)	

    endif

!...sum active/retired fleet for transfer/subtraction to/from non-fleet - CNG/LPG vehicles [ildv = 9,10,11,12] do not transfer
    OLDFSTKT=0.0
	if(curcalyr.gt.stockyr)then
      oldfstkt(1:mnumcr-2, 1:maxvtyp, [1:8,13:maxldv], 1:MAXAGE) = SUM(oldfstk(1:mnumcr-2, 1:maxvtyp, 1:MAXFLEET, [1:8,13:maxldv], 1:MAXAGE), DIM=3) / 1000000.0

!... calculate ldv stock post STOCKYR - by region
		do ivtyp=1,maxvtyp
          do ildv=1,maxldv
			LDV_STOCK(1:mnumcr-2,ivtyp,1,ildv,1,1,n) = sum(hhtechsal(1:mnumcr-2,ivtyp,1:maxclass,ildv,n),DIM=2) 
            
!           Calculate current year stock based on previous year's 1-year-younger count and survival curve (SSURV25)
            LDV_STOCK(1:mnumcr-2, ivtyp, 1, ildv, 2:MAXAGE - 1, 1, n) = &
              LDV_STOCK(1:mnumcr-2, ivtyp, 1, ildv, 1:MAXAGE - 2, 1, n - 1) * SSURV25(1:mnumcr-2, 1:MAXAGE - 2, ivtyp)

		    LDV_STOCK(1:mnumcr-2,ivtyp,1,ildv,maxage,1,n) = LDV_STOCK(1:mnumcr-2,ivtyp,1,ildv,maxage-1,1,n-1)*SSURV25(1:mnumcr-2,maxage-1,ivtyp) + &
			                                           LDV_STOCK(1:mnumcr-2,ivtyp,1,ildv,maxage,1,n-1)*SSURV25(1:mnumcr-2,maxage,ivtyp) 
!       	transfer retired fleet stock to non-fleet stock (taxis are not transfered to HH stock) 
		    LDV_STOCK(1:mnumcr-2, ivtyp, 1, ildv, 1:MAXAGE, 1, n) = &
              LDV_STOCK(1:mnumcr-2, ivtyp, 1, ildv, 1:MAXAGE, 1, n) + OLDFSTKT(1:mnumcr-2, ivtyp, ildv, 1:MAXAGE)

          enddo
	    enddo

!...  sum across regions to determine national stock quantities
	  LDV_STOCK(mnumcr, 1:maxvtyp, 1:maxowner, 1:maxldv, 1:maxage, 1, n) = sum(LDV_STOCK(1:mnumcr-2, 1:maxvtyp, 1:maxowner, 1:maxldv, 1:maxage, 1, n),DIM=1)

    endif ! year > stockyr

!	Write out average vehicle age by size class and region
	IF(N.eq.MNUMYR.and.FCRL.eq.1.and.write_avg_veg_age) THEN
	  avg_age(:,:,:) = 0.0
	  age_wgt(:,:,:) = 0.0
	  do iown = 22, mnumyr ! (2010-2050)		! Commandeer iown for the write statement
	    do iregn = 1, mnumcr
		 if (iregn.eq.10) CYCLE
		  do IVTYP = 1, maxvtyp
		    do iage = 1, maxage
		      age_wgt(iown,ivtyp,iregn) = age_wgt(iown,ivtyp,iregn) + iage * sum(LDV_STOCK(iregn,IVTYP,:,:,iage,:,iown))
		    enddo
		    avg_age(iown,ivtyp,iregn) = age_wgt(iown,ivtyp,iregn) / sum(LDV_STOCK(iregn,IVTYP,:,:,:,:,iown))
		  enddo
		enddo
	  enddo

	  WRITE(21,*)'Average LDV age'
	  WRITE(21,*)'year,ivtyp,cd1,cd2,cd3,cd4,cd5,cd6,cd7,cd8,cd9,national'
	  do iown = 24, mnumyr ! (2012-2050)
	    do ivtyp = 1, maxvtyp
	      
	      WRITE(21,'(I4,",",I2,10(",",F10.2))') iown+1989, ivtyp, avg_age(iown,ivtyp,1),avg_age(iown,ivtyp,2),&
	      										avg_age(iown,ivtyp,3),avg_age(iown,ivtyp,4),avg_age(iown,ivtyp,5),&
	      										avg_age(iown,ivtyp,6),avg_age(iown,ivtyp,7),avg_age(iown,ivtyp,8),&
	      										avg_age(iown,ivtyp,9),avg_age(iown,ivtyp,11)
		enddo
	  enddo
	endif

!-----------------------------------------------------------------------------------------------------------------------------------
!...Calculate total ldv vehicle stock by fuel types (for table 49). Collapse iowner, iage, ihav dims.
    VSTK(1:maxvtyp,1:maxldv) = sum(sum(sum(LDV_STOCK(mnumcr,1:maxvtyp,1:maxowner,1:maxldv,1:maxage,1:maxhav,n), DIM=5),DIM=4),DIM=2) 

!...calculate light duty vehicles per licensed driver
    VPLD(n) = sum(vstk(1:maxvtyp,1:maxldv))/SUM(LicDriver(1:AGEGRP,1:MF,1:MNUMCR-2,n)) 

!...calculate total LDV stocks to determine alt fuel availability
    LDVSTK(1:maxldv,n) = sum(vstk(1:maxvtyp,1:maxldv), DIM=1)

  RETURN
  END SUBROUTINE TSMOD

! ==========================================================================================================
! ... Subroutine TMPGSTK calculates household light vehicle stock mpg by technology
! ==========================================================================================================
  SUBROUTINE TMPGSTK
  USE T_
  USE MEAN_FUNCS
  IMPLICIT NONE

    REAL     NUM1,DEN1,NUM2,DEN2
    LOGICAL, DIMENSION(MNUMCR, MAXVTYP, MAXLDV, MAXAGE) :: error_condition_mask

!...Calculate stock mpg for household cars and lt. trucks                          
!...populate hhmpgstk for older vintages (1995 and previous model years)
    if(curcalyr.eq.first_read_year) then 
	  do iregn=1,mnumcr
	    if(iregn.ne.10) then
		  do ivtyp=1,maxvtyp
		    do ildv=1,maxldv
			  SELECT CASE (ILDV)
                CASE (1)
                  hhmpgstk(iregn,ivtyp,ildv,2:maxage,n) = cmpgstkgas95(ivtyp,2:maxage)*degfac(ivtyp,ildv,n)
                CASE (2)
                  hhmpgstk(iregn,ivtyp,ildv,2:maxage,n) = cmpgstkgas95(ivtyp,2:maxage)*degfac(ivtyp,ildv,n)*afvadjfe(ildv,ivtyp)
                CASE (3) 
                  hhmpgstk(iregn,ivtyp,ildv,2:maxage,n) = cmpgstkgas95(ivtyp,2:maxage)*degfac(ivtyp,ildv,n)
                CASE (4,7,15) 
                  hhmpgstk(iregn,ivtyp,ildv,2:maxage,n) = cmpgstkgas95(ivtyp,2:maxage)*degfac(ivtyp,ildv,n)*4.0				
                CASE (9,10,11,12)
                  hhmpgstk(iregn,ivtyp,ildv,2:maxage,n) = cmpgstkgas95(ivtyp,2:maxage)*degfac(ivtyp,ildv,n)
              END SELECT
		    enddo 
		  enddo
		endif
	  enddo 
	endif

!...calculate new household fuel economy by region, vtyp, and ldv <= 2018
	if(curcalyr.le.2018) then
!     Fill all regional values with national average
      hhmpgnew(1:mnumcr,1:maxvtyp,1:maxldv,n) = SPREAD(ldvmpgnew(mnumcr,1:maxvtyp,1:maxldv,n), &
                                                         DIM=1, NCOPIES = MNUMCR)
!     assign mpg if no new vehicle in FEM data (conversion/demonstration/small mfr) 
      do iregn=1,mnumcr
	    if(iregn.ne.10) then
		  do ivtyp=1,maxvtyp 	    
			do ildv=1,maxldv
!			  hhmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,ildv,n)
	          if(hhmpgnew(iregn,ivtyp,ildv,n).eq.0.0) then
				SELECT CASE (ILDV)
                  CASE (2)
                    hhmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n) * afvadjfe(ildv,ivtyp)
                  CASE (3)
                    hhmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n)
                  CASE (9,10,11,12)
                    hhmpgnew(iregn,ivtyp,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n)
                  CASE (4,7,15)
                    if(ivtyp.eq.1) then
                      hhmpgnew(iregn,ivtyp,ildv,n) = 105.0
                    else
                      hhmpgnew(iregn,ivtyp,ildv,n) = 85.0
                    endif
                END SELECT			
			  endif
		    enddo 
		  enddo
		endif
	  enddo 
	endif !<=2018

!...household mpg regional detail  
	if(curcalyr.ge.2019) then
!...  ensure all new household ldvs have a mpg in case older ldvs appear in region in later years
!     Fill all regional values with national
	  hhmpgnew(1:mnumcr,1:maxvtyp,1:maxldv,n) = SPREAD(ldvmpgnew(mnumcr,1:maxvtyp,1:maxldv,n), &
                                                         DIM=1, NCOPIES = MNUMCR)
!     Where regional values exist, overwrite the national
      WHERE (ldvmpgnew(1:mnumcr-2,1:maxvtyp,1:maxldv,n).ne.0.0)
        hhmpgnew(1:mnumcr-2,1:maxvtyp,1:maxldv,n) = ldvmpgnew(1:mnumcr-2,1:maxvtyp,1:maxldv,n)
      END WHERE

!...  calculate region specific household fuel economy
	  do iregn=1,mnumcr
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
!...fill vintage 1 hhmpgstk 
	hhmpgstk(1:mnumcr,1:maxvtyp,1:maxldv,1,n) = hhmpgnew(1:mnumcr,1:maxvtyp,1:maxldv,n) * &
                                                SPREAD(degfac(1:maxvtyp,1:maxldv,n),&
                                                       DIM=1,NCOPIES=MNUMCR)

!...fill current vintage mpg with previous year mpg
	if(curcalyr.gt.first_read_year) then 
	  do iregn=1,mnumcr 
	    do ivtyp=1,maxvtyp 
		  do ildv=1,maxldv
!           Fill current year mpgs with previous vintage from previous year
			hhmpgstk(iregn, ivtyp, ildv, 2:maxage, n) = hhmpgstk(iregn, ivtyp, ildv, 1:maxage-1, n-1)

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

!   Error checking household mpg
    if(curcalyr.le.epalyr+2) then
	  error_condition_mask = (ldv_stock(1:mnumcr,1:maxvtyp,1,1:maxldv,1:maxage,1,n) .NE. 0.0) .AND. &
                             (hhmpgstk(1:mnumcr,1:maxvtyp,1:maxldv,1:maxage,n) .EQ. 0.0)
      do iregn=1,mnumcr
	    if(iregn.ne.10) then
		  do ivtyp=1,maxvtyp
		    do ildv=1,maxldv
		  	  do iage=1,maxage
                if (error_condition_mask(iregn, ivtyp, ildv, iage)) then 
                  if (ivtyp.eq.1) then 
		  	        write(21,'(a,5(i4,","),4(f5.1,","),2(f8.5,"."))')'WARNING: household regional mpg error ',curcalyr,iregn,ivtyp,ildv,iage,hhmpgstk(iregn,ivtyp,ildv,iage,n),&
                                                hhmpgnew(mnumcr,ivtyp,ildv,n),degfac(ivtyp,ildv,n),ldvmpgnew(mnumcr,ivtyp,ildv,n-iage+1),&
                                                ldv_stock(iregn,ivtyp,1,ildv,iage,1,n),sum(cafesales(1:cargrp,1:maxclass,yrs,ildv))
                  else
		  	        write(21,'(a,5(i4,","),4(f5.1,","),2(f8.5,"."))')'WARNING: household regional mpg error ',curcalyr,iregn,ivtyp,ildv,iage,hhmpgstk(iregn,ivtyp,ildv,iage,n),&
                                                hhmpgnew(mnumcr,ivtyp,ildv,n),degfac(ivtyp,ildv,n),ldvmpgnew(mnumcr,ivtyp,ildv,n-iage+1),&
                                                ldv_stock(iregn,ivtyp,1,ildv,iage,1,n),sum(cafesales(ltkgrp:maxgroup,1:maxclass,yrs,ildv)) 
                  endif
                    if (iage.eq.1) then
                      do igp=1,maxgroup
                        write(21,'(a,",",8(f5.1,","))')'WARNING: household regional mpg error femmpg', FemMpg(igp,1:maxclass,yrs,ildv)
                      enddo
                    endif
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
	  do iregn=1,mnumcr-2
	    VMT_STK_HH(1,ildv,1:maxage,1,iregn) = LDV_STOCK(iregn,1,1,ildv,1:maxage,1,n)*PVMT(1:maxage,n,iregn,ildv)
	    VMT_STK_HH(2,ildv,1:maxage,1,iregn) = LDV_STOCK(iregn,2,1,ildv,1:maxage,1,n)*LVMT(1:maxage,n,iregn,ildv)
	  enddo
	  do IVTYP = 1,maxvtyp
	    VMT_STK_HH(ivtyp,ildv,1:maxage,1,mnumcr)=sum(VMT_STK_HH(ivtyp,ildv,1:maxage,1,1:mnumcr-2),DIM=2)
	  enddo	  
    enddo

    HHMPGSTK_TYPREG(1:mnumcr,1:2,n) = 0.0	!car/LT hh consumption (denominator for FE)

    do iregn=1,mnumcr-2
      do ildv=1,maxldv
	    do iage=1,maxage  
		  if(hhmpgstk(iregn,1,ildv,iage,n).ne.0.0) HHMPGSTK_TYPREG(iregn,1,n) = HHMPGSTK_TYPREG(iregn,1,n)+(VMT_STK_HH(1,ildv,iage,1,iregn) /  &
																				   (hhmpgstk(iregn,1,ildv,iage,n)))
		  if(hhmpgstk(iregn,2,ildv,iage,n).ne.0.0) HHMPGSTK_TYPREG(iregn,2,n) = HHMPGSTK_TYPREG(iregn,2,n)+(VMT_STK_HH(2,ildv,iage,1,iregn) /  &
																				   (hhmpgstk(iregn,2,ildv,iage,n)))
		enddo
      enddo
    enddo
    
!...calculate national consumption (gallons of gasoline equivalent) 
	HHMPGSTK_TYPREG(mnumcr,1,n) = sum(HHMPGSTK_TYPREG(1:mnumcr-2,1,n))	!hh car
	HHMPGSTK_TYPREG(mnumcr,2,n) = sum(HHMPGSTK_TYPREG(1:mnumcr-2,2,n))	!hh truck	

!...Calculate average mpg of household light duty vehicles	
!...national household car and truck stock fuel economies	
	HHMPGSTK_TYP(1,N) = sum(VMT_STK_HH(1,1:maxldv,1:maxage,1,1:mnumcr-2))/HHMPGSTK_TYPREG(mnumcr,1,n)
	HHMPGSTK_TYP(2,N) = sum(VMT_STK_HH(2,1:maxldv,1:maxage,1,1:mnumcr-2))/HHMPGSTK_TYPREG(mnumcr,2,n)
	
!...national household combined stock fuel economy
    MPGHH(n) = sum(VMT_STK_HH(1:maxvtyp,1:maxldv,1:maxage,1,1:mnumcr-2)) / (HHMPGSTK_TYPREG(mnumcr,1,n)+HHMPGSTK_TYPREG(mnumcr,2,n))

!...calculate average car and light truck mpg by technology 
    do ildv=1,maxldv
      HHMPGSTK_TYPLDV(ildv,:,n) = 0.0
      HHMPGSTK_LDV(ILDV,N) = 0.0
	  NUM1 = 0.0
      DEN1 = 0.0
      NUM2 = 0.0
      DEN2 = 0.0
      do ivtyp=1,maxvtyp
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
		  if(DEN1.ne.0.0) HHMPGSTK_TYPLDV(ildv,1,n) = NUM1/DEN1
		else 
		  if(DEN2.ne.0.0) HHMPGSTK_TYPLDV(ildv,2,n) = NUM2/DEN2	
		endif
	  enddo ! ivtyp
      
!...Calculate average household vehicle mpg by technology (combined car and LT) for reporting
      if((DEN1+DEN2).ne.0.0) HHMPGSTK_LDV(ILDV,n) = (NUM1+NUM2)/(DEN1+DEN2)
!     Fill in missing CNG/LPG mpgs; others with 1.0 to prevent NaNs in final agg calcs in TREPORT
      IF (HHMPGSTK_LDV(ILDV,N) .EQ. 0.0) then
        if (ildv.ge.9.and.ildv.le.12) then
          HHMPGSTK_LDV(ILDV,N) = HHMPGSTK_LDV(1,N) * 0.95
        else
          HHMPGSTK_LDV(ILDV,N) = 1.0
        endif
      endif
    enddo ! ildv 

  RETURN
  END SUBROUTINE TMPGSTK

! ==========================================================================================================
! ... Subroutine TCURB the stock average weight (by vintage) of cars and light trucks  
! ... This subroutine only considers the weight of non-highly automated vehicles 
! ==========================================================================================================
  SUBROUTINE TCURB
  USE T_
  USE MEAN_FUNCS
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
    TRWTCAR_STOCK(N) = WEIGHTED_MEAN_1D(STKAVGWGT(1,1:maxage), &
                                        SUM(LDV_STOCK(mnumcr,1,1,1:maxldv,1:maxage,1,n), DIM=1),&
                                        caller_id = 'TRWTCAR_STOCK')
    TRWTTRK_STOCK(N) = WEIGHTED_MEAN_1D(STKAVGWGT(2,1:maxage), &
                                        SUM(LDV_STOCK(mnumcr,2,1,1:maxldv,1:maxage,1,n), DIM=1),&
                                        caller_id = 'TRWTTRK_STOCK')

  RETURN
  END SUBROUTINE TCURB

! ==========================================================================================================
! ... Subroutine TFLTMPGS calculates MPG for the fleet stock
! ==========================================================================================================
  SUBROUTINE TFLTMPGS
  USE T_
  USE MEAN_FUNCS
  IMPLICIT NONE

  REAL     MPGFSTK(MNUMCR-2,MAXVTYP,MAXFLEET,MAXLDV,MAXAGE,MNUMYR)
  REAL     NUM,DEN,NUM1,DEN1,DEN2
  
  LOGICAL, DIMENSION(MNUMCR-2, MAXVTYP, MAXFLEET, MAXLDV, MAXAGE) :: error_condition_mask
  LOGICAL, DIMENSION(MNUMCR, MAXVTYP, MAXFLEET, MAXLDV) :: error_condition_mask_2

!...populate 1995 fltmpgstk values for all vintages 
    if(curcalyr.eq.first_read_year) then 
	  do iregn=1,mnumcr
		do ivtyp=1,maxvtyp
		  do ifleet=1,maxfleet
			do ildv=1,maxldv
              SELECT CASE (ILDV)
                CASE(1)
                  fltmpgstk(iregn,ivtyp,ifleet,ildv,2:maxage,n) = cmpgstkgas95(ivtyp,2:maxage)*degfac(ivtyp,ildv,n)
                CASE(2)
                  fltmpgstk(iregn,ivtyp,ifleet,ildv,2:maxage,n) = cmpgstkgas95(ivtyp,2:maxage)*degfac(ivtyp,ildv,n)*afvadjfe(ildv,ivtyp)
                CASE(3)
                  fltmpgstk(iregn,ivtyp,ifleet,ildv,2:maxage,n) = cmpgstkgas95(ivtyp,2:maxage)*degfac(ivtyp,ildv,n)
                CASE(4,7,15)
                  fltmpgstk(iregn,ivtyp,ifleet,ildv,2:maxage,n) = cmpgstkgas95(ivtyp,2:maxage)*degfac(ivtyp,ildv,n)*4.0
                CASE(9,10,11,12)
                  fltmpgstk(iregn,ivtyp,ifleet,ildv,2:maxage,n) = cmpgstkgas95(ivtyp,2:maxage)*degfac(ivtyp,ildv,n)
              END SELECT
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
				  SELECT CASE (ILDV)
                    CASE(2)
                      fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n) * afvadjfe(ildv,ivtyp)
                    CASE(3)
                      fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n)
                    CASE(9,10,11,12)
                      fltmpgnew(iregn,ivtyp,ifleet,ildv,n) = ldvmpgnew(mnumcr,ivtyp,gas,n)
                    CASE(4,7,15)
                      if(ivtyp.eq.1) then
                        fltmpgnew(iregn,1,ifleet,ildv,n) = 105.0
                      else
                        fltmpgnew(iregn,2,ifleet,ildv,n) = 85.0
                      endif
                  END SELECT
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
!     Fill all regional values with national
	  fltmpgnew(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,n) = SPREAD(SPREAD(ldvmpgnew(mnumcr,1:maxvtyp,1:maxldv,n), &
                                                                   DIM=2, NCOPIES = MAXFLEET), &
                                                                   DIM=1, NCOPIES = MNUMCR)

!     Where regional values exist, overwrite the national
      WHERE (SPREAD(ldvmpgnew(1:mnumcr-2,1:maxvtyp,1:maxldv,n),DIM=3, NCOPIES = MAXFLEET).ne.0.0)
        fltmpgnew(1:mnumcr-2,1:maxvtyp,1:maxfleet,1:maxldv,n) = SPREAD(ldvmpgnew(1:mnumcr-2,1:maxvtyp,1:maxldv,n), &
                                                                       DIM=3, NCOPIES = MAXFLEET)
      END WHERE
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

!   Error checking for fltmpgnew
	if(curcalyr.le.stockyr) then 
	  error_condition_mask_2 = (flt_stock(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,1,1,n) .NE. 0.0) .AND. &
                               (fltmpgnew(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,n) .EQ. 0.0)
      do iregn=1,mnumcr
	    do ivtyp=1,maxvtyp 
		  do ifleet=1,maxfleet
		    do ildv=1,maxldv 
			  if(error_condition_mask_2(iregn, ivtyp, ifleet, ildv)) then 
			    write(21,'(a,5(i4,","),2(f5.1,","),f6.5)')'fleet regional mpg error ',curcalyr,iregn,ivtyp,ildv,ifleet,fltmpgnew(iregn,ivtyp,ifleet,ildv,n),&
                                            ldvmpgnew(mnumcr,ivtyp,ildv,n),flt_stock(iregn,ivtyp,ifleet,ildv,1,1,n)
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
	  TREFFFLT(IVTYP,N) = 0.0
	  if(DEN.ne.0.0) TREFFFLT(IVTYP,N) = NUM/DEN
	enddo	  

!...Advance vintage array of fleet stock mpg 1 year and account for new additions       ! MDR put inside of an ihav loop if flthav working correctly
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

!   Error checking for sales input consistency between input files
    if(curcalyr.le.epalyr+2) then
	  error_condition_mask = (Flt_Stock(1:mnumcr-2,1:maxvtyp,1:maxfleet,1:maxldv,1:maxage,1,n) .NE. 0.0) .AND. &
                             (fltmpgstk(1:mnumcr-2,1:maxvtyp,1:maxfleet,1:maxldv,1:maxage,n) .EQ. 0.0)
      do iregn=1,mnumcr-2
	    do ivtyp=1,maxvtyp
	  	  do ifleet=1,maxfleet
	  	    do ildv=1,maxldv
	  		  do iage=1,maxage
                if(error_condition_mask(iregn,ivtyp,ifleet,ildv,iage)) then 
                  write(21,'(a,6(i4,","),f5.1,",",f8.5)')'error between fleet stocks and mpg ',curcalyr,iregn,ivtyp,ildv,ifleet,iage,fltmpgstk(iregn,ivtyp,ifleet,ildv,iage,n),&
                                              Flt_Stock(iregn,ivtyp,ifleet,ildv,iage,1,n)
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

!...Calculate overall fleet average mpg by fuel technology (FLTMPGSTK_LDV, used in TREPORT)
    do ildv=1,maxldv
      NUM = 0.0
      DEN = 0.0
      do ivtyp=1,maxvtyp
        do ifleet=1,maxfleet
		  do ihav = 1,maxhav
            if(MPGFLTSTK(mnumcr,ivtyp,ifleet,ildv,n).ne.0.0) then
              NUM = NUM +  FLTECHSTK(mnumcr,ivtyp,ifleet,ildv,ihav)
              DEN = DEN + (FLTECHSTK(mnumcr,ivtyp,ifleet,ildv,ihav)/MPGFLTSTK(mnumcr,ivtyp,ifleet,ildv,n))
            endif
		  enddo
        enddo
      enddo
      FLTMPGSTK_LDV(ILDV,n) = 0.0
      if(DEN.ne.0.0) FLTMPGSTK_LDV(ildv,n) = NUM/DEN
!     Fill in missing CNG/LPG mpgs; others with 1.0 to prevent NaNs in final agg calcs in TREPORT
      IF (FLTMPGSTK_LDV(ILDV,N) .EQ. 0.0) then
        if (ildv.ge.9.and.ildv.le.12) then
          FLTMPGSTK_LDV(ILDV,N) = FLTMPGSTK_LDV(1,N) * 0.95
        else
          FLTMPGSTK_LDV(ILDV,N) = 1.0
        endif
      endif
    enddo
    
!...Calculate overall fleet stock average mpg by car / light truck    
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
      TREFFFLT(IVTYP+2,n) = 0.0
      if(DEN.ne.0.0) TREFFFLT(ivtyp+2,n) = NUM/DEN
    enddo

  RETURN
  END SUBROUTINE TFLTMPGS

! ==========================================================================================================
! ... Subroutine TFLTCONS calculates fuel consumption of fleet vehicles
! ==========================================================================================================
  SUBROUTINE TFLTCONS
  USE T_
  IMPLICIT NONE
  
  REAL, DIMENSION(mnumcr,maxvtyp,maxfleet,maxldv) :: sum_allhav
  LOGICAL, DIMENSION(mnumcr,maxvtyp,maxfleet,maxldv) :: mask_nonzero_mpg

!...Calculate fuel consumption (gallons and btu) by ldv type
    fltechgge(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,n) = 0.0
    fltechbtu(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,n) = 0.0
    
    mask_nonzero_mpg(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv) = (MPGFLTSTK(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,n) /= 0.0)
    sum_allhav(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv) = SUM(fltechvmt(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav), DIM=5)
    
    WHERE (mask_nonzero_mpg(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv))
      fltechgge(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,n) = &
          (sum_allhav(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv) / 1000000.0) / &
          MPGFLTSTK(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,n)
    
      fltechbtu(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,n) = &
          fltechgge(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,n) * CFMGQ(n) / 42.0
    END WHERE
	
!...fleet btu by ldv
    fltldvbtu(1:mnumcr,1:maxvtyp,1:maxldv,n) = sum(fltechbtu(1:mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,n), DIM=3) 
	fltldvbtut(1:mnumcr,1:maxldv,n)          = sum(fltldvbtu(1:mnumcr,1:maxvtyp,1:maxldv,n), DIM=2)

!...Calculate total fleet consumption by fuel type by region 
    FLTFUELBTU(1:mnumcr-2,1:maxfuel,n) = 0.0
    do ifuel=1,maxfuel
      SELECT CASE (ifuel)
        CASE(1) ! gasoline
          FLTFUELBTU(1:mnumcr-2,1,n) = fltldvbtut(1:mnumcr-2, 1,n) + &                              ! Gasoline
                                       fltldvbtut(1:mnumcr-2, 3,n)*(1.0-PctAF(2,1:mnumcr-2,n)) + &  ! Ethanol FFV
                                       fltldvbtut(1:mnumcr-2, 5,n)*(1.0-PctPHEV20(n)) + & 	        ! PHEV20
                                       fltldvbtut(1:mnumcr-2, 6,n)*(1.0-PctPHEV50(n)) + & 	        ! PHEV50 
                                       fltldvbtut(1:mnumcr-2, 9,n)*(1.0-PctAF(3,1:mnumcr-2,n)) + &  ! CNG Bifuel
                                       fltldvbtut(1:mnumcr-2,10,n)*(1.0-PctAF(4,1:mnumcr-2,n)) + &  ! LPG Bifuel
                                       fltldvbtut(1:mnumcr-2,16,n)             				        ! Hybrid Gasoline
        CASE(2) ! methanol           
          FLTFUELBTU(1:mnumcr-2,2,n) = fltldvbtut(1:mnumcr-2,13,n)                				    ! FCV Methanol
        CASE(3) ! ethanol            
          FLTFUELBTU(1:mnumcr-2,3,n) = fltldvbtut(1:mnumcr-2, 3,n) * PctAF(2,1:mnumcr-2,n)   		! Ethanol FFV
        CASE(4) ! cng
          FLTFUELBTU(1:mnumcr-2,4,n) = fltldvbtut(1:mnumcr-2, 9,n) * PctAF(3,1:mnumcr-2,n) + &      ! CNG Bifuel
                                       fltldvbtut(1:mnumcr-2,11,n)  								! CNG Dedicated 
        CASE(5) ! LPG                
          FLTFUELBTU(1:mnumcr-2,5,n) = fltldvbtut(1:mnumcr-2,10,n) * PctAF(4,1:mnumcr-2,n) + &      ! LPG Bifuel
                                       fltldvbtut(1:mnumcr-2,12,n) 				            	    ! LPG Dedicated
        CASE(6) ! electricity        
          FLTFUELBTU(1:mnumcr-2,6,n) = fltldvbtut(1:mnumcr-2, 5,n)* PctPHEV20(n) + &    			! PHEV20
                                       fltldvbtut(1:mnumcr-2, 6,n)* PctPHEV50(n) + &    			! PHEV50 
                                       fltldvbtut(1:mnumcr-2, 7,n)+ &                                         
								       fltldvbtut(1:mnumcr-2, 4,n)+ &                                         
								       fltldvbtut(1:mnumcr-2,15,n) 								    ! EV 
        CASE(7) ! hydrogen                                                                           
          FLTFUELBTU(1:mnumcr-2,7,n) =  fltldvbtut(1:mnumcr-2,14,n)  						 		! FCV Hydrogen
        CASE(8) ! diesel
          FLTFUELBTU(1:mnumcr-2,8,n) = fltldvbtut(1:mnumcr-2, 2,n) + &                         	    ! Diesel
                                       fltldvbtut(1:mnumcr-2, 8,n) 			                	    ! Hybrid Diesel
      END SELECT
    enddo
    
    FLTFUELBTU(mnumcr,1:maxfuel,n) = sum(FLTFUELBTU(1:MNUMCR-2,1:maxfuel,n), DIM=1)

  RETURN
  END SUBROUTINE TFLTCONS

! ==========================================================================================================
! ... Subroutine TVMT calculates total household light vehicle VMT
! ==========================================================================================================
    SUBROUTINE TVMT
    USE T_
    USE MEAN_FUNCS
    IMPLICIT NONE

    REAL    VMTEXP(MF,AGEGRP), CA_ADJ, NUM1, NUM2
	REAL 	vmtldvhh(mnumcr-2), hhvmtadj(mnumcr-2)
    REAL    TEMP_MPG(ildv)

!...Calculate cost of driving per mile (COSTMI,cents in 2000USD)
!   Supporting calcs not available pre-2013, so continue using gasoline price only and overall stock average fuel economy
    if (curcalyr.lt.2013) then
      DO iregn = 1, mnumcr
        if (iregn.eq.10) CYCLE
        COSTMI(iregn,n) = ((PMGTR(iregn,n)*CFMGQ(n)/42.0)/trldmpgf(3,n))*100.0 * MC_JPGDP(11)
      ENDDO
!   Where calcs available, do average cost per mile for all powertrains/fuels instead of just gasoline price
    else
      TEMP_MPG(:) = 0.0
      DO ildv = 1,maxldv
        if (ildv.le.7.or.ildv.ge.15.and.HHMPGSTK_LDV(ILDV,n).gt.0.0.and.FLTMPGSTK_LDV(ILDV,N).gt.0.0) then
          TEMP_MPG(ildv) = (SUM(VMT_STK_HH(1:maxvtyp,ILDV,:,1:maxhav,mnumcr))+SUM(fltechvmt(mnumcr,1:maxvtyp,1:maxfleet,ILDV,1:maxhav))) / &
                           ( SUM(VMT_STK_HH(1:maxvtyp,ILDV,:,1:maxhav,mnumcr))/HHMPGSTK_LDV(ILDV,n) &
                            +SUM(fltechvmt(mnumcr,1:maxvtyp,1:maxfleet,ILDV,1:maxhav))/FLTMPGSTK_LDV(ILDV,N))
        endif
      enddo
      
      DO iregn = 1, mnumcr
        if (iregn.eq.10) CYCLE
        NUM1 = 0.0
        NUM2 = 0.0
        DO ildv = 1,maxldv
          if (ildv.le.7.or.ildv.ge.15.and.HHMPGSTK_LDV(ILDV,n).gt.0.0.and.FLTMPGSTK_LDV(ILDV,N).gt.0.0) then
            NUM1 = NUM1 + (FPRICE(ILDV,iregn,YRS) / TEMP_MPG(ildv)*(SUM(VMT_STK_HH(1:maxvtyp,ILDV,:,1:maxhav,iregn))+SUM(fltechvmt(iregn,1:maxvtyp,1:maxfleet,ILDV,1:maxhav))))
            NUM2 = NUM2 + SUM(VMT_STK_HH(1:maxvtyp,ILDV,:,1:maxhav,iregn))+SUM(fltechvmt(iregn,1:maxvtyp,1:maxfleet,ILDV,1:maxhav))
          endif
        enddo
!       FPRICE in 1990$ cents/gallon. COSTMI expected in 2000$ cents/mile        
        COSTMI(iregn,n) = NUM1/NUM2 * MC_JPGDP(11) / MC_JPGDP(1)
      ENDDO
    endif
    
!...Calculate employment rate for vmt growth
    EMP_RATE_VMT(n) = MC_EEA(n)/MC_NP16A(11,n)
	  
!...Calculate vmt per licensed driver (VMTLD) for years greater than last historic year of data (1000's of miles)
    IF(curcalyr.gt.VMTLDHistYr) THEN
      DO IMF=1,MF
        VMTLD(1:agegrp,n,imf) = (ALPHA(imf,1:agegrp) + BETAVMT(imf,1:agegrp)*LOG(VMTLD(1:agegrp,n-1,imf))+BETAINC(imf,1:agegrp)*LOG(INC00_D_16(11,n))   + &
							  BETACOST(imf,1:agegrp)*LOG(COSTMI(mnumcr,n))+BETAVPLD(imf,1:agegrp)*LOG(VPLD(n))+BETAEMP(imf,1:agegrp)*LOG(EMP_RATE_VMT(n)))  
	  ENDDO
	ENDIF

    !For use in re-estimating VMTLD equation coefficients
    !IF(n.eq.mnumyr.and.fcrl.eq.1) then
    !  do i = 7, mnumyr
    !    do imf = 1,mf
    !      do iagr = 1,agegrp
    !        WRITE(21,'(a,",",3(i4,","),5(f12.5,","))')'vmt_hist',i+1989,imf,iagr,VMTLD(iagr,i-1,imf),INC00_D_16(11,i),COSTMI(mnumcr,i),VPLD(i),EMP_RATE_VMT(i)
    !      enddo
    !    enddo
    !  enddo
    !endif
	  
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
		VMTLDV(iagr,n,imf,1:mnumcr-2) = VMTLD(iagr,n,imf) * LICDRIVER(iagr,imf,1:mnumcr-2,n)
      enddo
	enddo

!...calculate national vmt 
	VMTLDV(1:agegrp,n,1:MF,mnumcr) = sum(VMTLDV(1:agegrp,n,1:MF,1:mnumcr-2),DIM=3)

!...calculate US household vmt (millions) by vintage from regional estimates weighted by vehicle stocks
	do iage=1,maxage
	  do ildv=1,maxldv
	    PVMT(iage,n,mnumcr,ildv) = WEIGHTED_MEAN_1D(PVMT(iage,n,1:mnumcr-2,ildv), &
                                                    LDV_STOCK(1:mnumcr-2,1,1,ildv,iage,1,n), &  !LDV_STOCK(iregn,ivtyp,ifleet,ildv,iage,ihav,n)
                                                    caller_id = 'PVMT(mnumcr)')
        LVMT(iage,n,mnumcr,ildv) = WEIGHTED_MEAN_1D(LVMT(iage,n,1:mnumcr-2,ildv), &
                                                    LDV_STOCK(1:mnumcr-2,2,1,ildv,iage,1,n), &  !LDV_STOCK(iregn,ivtyp,ifleet,ildv,iage,ihav,n)
                                                    caller_id = 'PVMT(mnumcr)')
	  enddo
	enddo

!...Calculate total household miles driven by each type of vehicle by vintage (million miles)
	hhtechvmt = 0.0
    do ildv=1,maxldv 
	  do iregn=1,mnumcr-2		    
	    hhtechvmt(iregn,1,ildv,1:maxage) = LDV_STOCK(iregn,1,1,ildv,1:maxage,1,n)*PVMT(1:maxage,n,iregn,ildv)
	    hhtechvmt(iregn,2,ildv,1:maxage) = LDV_STOCK(iregn,2,1,ildv,1:maxage,1,n)*LVMT(1:maxage,n,iregn,ildv)
	  enddo
	
      do ivtyp=1,maxvtyp		
	    hhtechvmt(mnumcr,ivtyp,ildv,1:maxage) = sum(hhtechvmt(1:mnumcr-2,ivtyp,ildv,1:maxage),DIM=1)
      enddo
    enddo

!...adjust hhtechvmt to align with vmtldv
!...calculate driver based household vmt
    vmtldvhh(1:mnumcr-2) = SUM(SUM(vmtldv(1:agegrp,n,1:mf,1:mnumcr-2),DIM=2),DIM=1) &
                         - SUM(SUM(SUM(SUM(fltechvmt(1:mnumcr-2,1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav),DIM=5),DIM=4),DIM=3),DIM=2)/1000000000.0
                         
!...calculate household regional vmt adjustment factor
	hhvmtadj(1:mnumcr-2) = vmtldvhh(1:mnumcr-2)/(sum(sum(sum(hhtechvmt(1:mnumcr-2,1:maxvtyp,1:maxldv,1:maxage),DIM=4),DIM=3),DIM=2)/1000.0)
	
!...apply vmt adjustment factor to hhtechvmt 
	do ivtyp=1,maxvtyp 
	  do ildv=1,maxldv
		do iage=1,maxage 
          hhtechvmt(1:mnumcr-2,ivtyp,ildv,iage) = hhtechvmt(1:mnumcr-2,ivtyp,ildv,iage) * hhvmtadj(1:mnumcr-2) 
		  hhtechvmt(mnumcr,ivtyp,ildv,iage) = sum(hhtechvmt(1:mnumcr-2,ivtyp,ildv,iage))
		enddo 
	  enddo 
	enddo

!...total household vmt for reporting (billion miles)	
	do ivtyp=1,maxvtyp 
	  vmthh(n,1:mnumcr,1:maxldv,ivtyp) = sum(hhtechvmt(1:mnumcr,ivtyp,1:maxldv,1:maxage),DIM=3)/1000.0
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
      IF (HHMPGSTK_TYP(1,N) .NE. 0.0 .AND. TREFFFLT(3,n) .NE. 0.0) &
        SUMLDV1(1) = (sum(VMT_STK_HH(1,1:maxldv,1:maxage,1,1:mnumcr-2))/(HHMPGSTK_TYP(1,N)*(CFMGQ(N)/CFMGQ(18)))) + &
                     (TOTFLTCAR(1)/(TREFFFLT(3,n)*(CFMGQ(N)/CFMGQ(18))))
      IF (HHMPGSTK_TYP(2,N) .NE. 0.0 .AND. TREFFFLT(4,n) .NE. 0.0) &
        SUMLDV1(2) = (sum(VMT_STK_HH(2,1:maxldv,1:maxage,1,1:mnumcr-2))/(HHMPGSTK_TYP(2,N)* (CFMGQ(N)/CFMGQ(18)))) + &
                     (TOTFLTCAR(2)/(TREFFFLT(4,n)*(CFMGQ(N)/CFMGQ(18))))
      SUMLDV2(1) = sum(LDV_STOCK(mnumcr,1,1,1:maxldv,1:maxage,1,n)) + TOTFLTCAR(1)
      SUMLDV2(2) = sum(LDV_STOCK(mnumcr,2,1,1:maxldv,1:maxage,1,n))  + TOTFLTCAR(2)

      DO IVTYP=1,MAXVTYP
        TRLDMPGF(IVTYP,N) = 0.0
        IF (SUMLDV1(IVTYP) .NE. 0.0 .AND. SUMLDV2(IVTYP) .NE. 0.0) &
          TRLDMPGF(IVTYP,N) = 1.0 / (SUMLDV1(IVTYP)/SUMLDV2(IVTYP))
      ENDDO

! ... Calculate fleet average stock vehicle mpg (Table 50)
      TRLDMPGF(3,N) = 0.0
      IF (SUMLDV1(1)+SUMLDV1(2) .NE. 0.0 .AND. SUMLDV2(1)+SUMLDV2(2) .NE. 0.0) &
        TRLDMPGF(3,N) = 1.0 / ((SUMLDV1(1) + SUMLDV1(2)) / (SUMLDV2(1) + SUMLDV2(2)))

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
        COAL_GROWTH(n)=(TTONMILE(n)-TTONMILE(n-1))/TTONMILE(n-1)
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
      FUEL_PRICE(1,n)=PDSTR(US,n) * mc_jpgdp(1) * CIDISCOUNT  !...diesel fuel price (1990$/mmbtu)
      FUEL_PRICE(2,n)=PGLTRRAIL(1,US,n) * mc_jpgdp(1) * 2        !...LNG fuel price (1990$/mmbtu)
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

      INTEGER   IM
      REAL      HYWAY(MNUMYR),FTVMT(MNUMYR), SHR_TRRPM(MNUMCR-2),SHR_CRRPM(MNUMCR-2),SHR_TBPMT(MNUMCR-2),&
                MILTARGR(MNUMYR),MILTRSHR(4,MNUMCR-2,MNUMYR), CRRPMPCEXP(MNUMCR-2,MNUMYR),CRRPMPC(MNUMCR-2,MNUMYR),&
                TMODRSHR(MNUMCR-2,MNUMYR),LUBRSHR(MNUMCR-2,MNUMYR),TRRPMPCEXP(MNUMCR-2,MNUMYR), &
                BETALUB,BUSSYSEF(3),BOATS,NONFARMEMP(MNUMCR-2,MNUMYR),PMGTR19_D_(MNUMYR),TBPMTPCEXP(MNUMCR-2,MNUMYR), &
				CREFF_ADJ(mnumcr-2), TREFF_ADJ(mnumcr-1), TBBTUPM_ADJ(mnumcr-1)
      REAL      TEMP
      LOGICAL   write_transit_drivers/.FALSE./       ! If on, write out transit drivers needed for model estimation

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
    nonfarmemp(:,n)=0.0
    Do iregn=1,mnumcr-2
	   nonfarmemp(iregn,n) = sum(mc_empna(iregn,1:39,n))-sum(mc_empna(iregn,20:21,n))
	enddo   
!...gasoline price 2019$
    PMGTR19_D_(N) = PMGTR(11,N)* CFMGQ(n)/42.0 * MC_JPGDP(30)
	  
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
	  if(shr_trrpm(iregn).gt.0.0) TRRPM(iregn,n) = TRRPM(iregn,n)-(sum(fltechvmt(mnumcr,1:maxvtyp,4,1:maxldv,2:maxhav))*TR_CAV_ADJ(n)*SHR_TRRPM(iregn)/1000000.0)
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
		CRRPM(iregn,n)=CRRPM(iregn,n)+(sum(fltechvmt(mnumcr,1:maxvtyp,4,1:maxldv, 2:maxhav))*CR_CAV_ADJ(n)*SHR_CRRPM(iregn)/1000000.0)
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
        IRPMPC(N) = IRRPM(N)/MC_NP16A(11,N)
        IREDD(N)  = IREDDHIST(N)
        IREDE(N)  = IREDEHIST(N)
        IREFF(N)  = (IREDD(N)+IREDE(N)) / IRRPM(N) * 1000000
      ELSE
        IRPMPC(n) = IRPMPC(n-1)*1.002
        IREFF(N)  = IREFF(IRHISTYEAR-1989)          ! assumes efficiency stays constant throughout projection 
        IRRPM(N)  = IRPMPC(N)*MC_NP16A(11,N)        ! from RPM/cap to total RPM
        IRED(N)   = IRRPM(N)*IREFF(N)/1000000.0     ! from total RPM to energy consumed
        IREDD(N)  = IRED(N)*IREDDSHR                ! diesel consumed
        IREDE(N)  = IRED(N)-IREDD(N)                ! electricity consumed
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
		TBPMT(iregn,n) = TBPMT(iregn,n)-(sum(fltechvmt(mnumcr,1:maxvtyp,4,1:maxldv,2:maxhav))*TB_CAV_ADJ(n)*SHR_TBPMT(iregn)/1000000.0)
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
	    if(im.eq.1) TMOD(im,n) = (IBCOEFF(2)+(IBCOEFF(1) * LOG((MC_GDPR(n)/MC_NP16A(11,n))))) * MC_NP16A(11,n) * (1.-(TMCOVID(im,n)))
		if(im.eq.2) TMOD(im,n) = (SBCOEFF(2)+(SBCOEFF(1) * (MC_NP(11,n)-MC_NP16A(11,n))/MC_NP(11,n))) * (1.-(TMCOVID(im,n)))
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
		  boats = (MC_GDPR(n)/MC_NP16A(11,n)) * (RBBOAT_COEF(1)*exp(RBBOAT_COEF(2)*n))
          IF(IFUELX .EQ.1) THEN
			RBEDPC(ifuelx,n) = boats * (RBMG_COEF(1)*exp(RBMG_COEF(2)*n))  
          ELSE
			RBEDPC(ifuelx,n) = boats * RBDS_COEF
          ENDIF
        ENDIF
        RECFD(IFUELX,N) = RBEDPC(IFUELX,N)
      ENDDO

! ... Regionalize recreational boat demand by population
      DO IFUELX=1,2
      QRECR(IFUELX,MNUMCR,N) = 0.0
        DO IREGN=1,MNUMCR-2
          QRECR(IFUELX,IREGN,N) = RECFD(IFUELX,N) * TMODRSHR(IREGN,N)
          QRECR(IFUELX,MNUMCR,N) = QRECR(IFUELX,MNUMCR,N) + QRECR(IFUELX,IREGN,N)
        ENDDO
      ENDDO

! ... Total VMT
      FTVMT(N) = VMT_TR(N)
	  HYWAY(N) = sum(VMTHH(n,mnumcr,1:maxldv,1:maxvtyp)) + FTVMT(N) + sum(fltechvmt(mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav))

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
                            sum(fltechvmt(mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav)) * SEDSHRMG(IREGN,N) +  &
                            FTVMT(N)  * SEDSHRDS(IREGN,N)) / &
                            HYWAY(N)
        QLUTR(IREGN,N) = LUBFD(N) * LUBRSHR(IREGN,N)
        QLUTR(11,N) = QLUTR(11,N) + QLUTR(IREGN,N)
      ENDDO

    IF (n.eq.mnumyr.and.fcrl.eq.1.and.write_transit_drivers) then
      WRITE(21,*)'Transit model inputs'
      WRITE(21,'(2(a4,","),4(a12,","))')'year','regn','gdp/cap($2012)','pmgtr($2019)','coviddum','emp_nf_mil'
      do i=6,mnumyr
        do iregn=1,mnumcr-2
          WRITE(21,'(2(i4,","),4(f12.7,","))')i+1989,iregn,MC_GDPR(i)/MC_NP16A(11,i),pmgtr19_D_(i),trcovid(IREGN,i),nonfarmemp(iregn,i)
        enddo
      enddo
    endif

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
! Indicies for HHMPGSTK_LDV = ILDV
! ==========================================================================================================
    SUBROUTINE TCONS
    USE T_
    IMPLICIT NONE

!...calculate household gge consumption and btu	
	hhtechgge(1:mnumcr,1:maxvtyp,1:maxldv,1:maxage,n)=0.0
    
    WHERE (hhmpgstk(1:mnumcr-2,1:maxvtyp,1:maxldv,1:maxage,n).ne.0.0)
      hhtechgge(1:mnumcr-2,1:maxvtyp,1:maxldv,1:maxage,n) = hhtechvmt(1:mnumcr-2,1:maxvtyp,1:maxldv,1:maxage) &
                                                          / hhmpgstk(1:mnumcr-2,1:maxvtyp,1:maxldv,1:maxage,n)
    END WHERE
    
    hhtechgge(mnumcr,1:maxvtyp,1:maxldv,1:maxage,n) = SUM(hhtechgge(1:mnumcr-2,1:maxvtyp,1:maxldv,1:maxage,n),DIM=1)
    hhtechbtu(1:mnumcr,1:maxvtyp,1:maxldv,n) = sum(hhtechgge(1:mnumcr,1:maxvtyp,1:maxldv,1:maxage,n), DIM=4) * CFMGQ(n)/42.0

!...Calculate household consumption by fuel
!...Gasoline
    TQLDV(1,1:mnumcr,N) = sum(hhtechbtu(1:mnumcr,1:maxvtyp, 1,n),DIM=2) + &
				         (sum(hhtechbtu(1:mnumcr,1:maxvtyp, 5,n),DIM=2) * (1.0-PctPHEV20(n))) + &
				         (sum(hhtechbtu(1:mnumcr,1:maxvtyp, 6,n),DIM=2) * (1.0-PctPHEV50(N))) + &
				         (sum(hhtechbtu(1:mnumcr,1:maxvtyp, 3,n),DIM=2) * (1.0-PCTAF(2,1:mnumcr,N)))   + &
				         (sum(hhtechbtu(1:mnumcr,1:maxvtyp,10,n),DIM=2) * (1.0-PCTAF(4,1:mnumcr,N)))   + &
				         (sum(hhtechbtu(1:mnumcr,1:maxvtyp, 9,n),DIM=2) * (1.0-PCTAF(3,1:mnumcr,N)))   + &
				          sum(hhtechbtu(1:mnumcr,1:maxvtyp,16,n),DIM=2) 
!...Methanol
    TQLDV(2,1:mnumcr,N) = sum(hhtechbtu(1:mnumcr,1:maxvtyp,13,n),DIM=2) 
!...Ethanol
    TQLDV(3,1:mnumcr,N) = sum(hhtechbtu(1:mnumcr,1:maxvtyp, 3,n),DIM=2) * PCTAF(2,1:mnumcr,N)
!...CNG
    TQLDV(4,1:mnumcr,N) = sum(hhtechbtu(1:mnumcr,1:maxvtyp,11,n),DIM=2) + &
                         (sum(hhtechbtu(1:mnumcr,1:maxvtyp, 9,n),DIM=2) * PCTAF(3,1:mnumcr,N))
!...LPG
    TQLDV(5,1:mnumcr,N) = sum(hhtechbtu(1:mnumcr,1:maxvtyp,12,n),DIM=2) + &
                         (sum(hhtechbtu(1:mnumcr,1:maxvtyp,10,n),DIM=2) * PCTAF(4,1:mnumcr,N))
!...Electricity
    TQLDV(6,1:mnumcr,N) = sum(hhtechbtu(1:mnumcr,1:maxvtyp, 4,n),DIM=2) + & 
						  sum(hhtechbtu(1:mnumcr,1:maxvtyp, 7,n),DIM=2) + &
						  sum(hhtechbtu(1:mnumcr,1:maxvtyp,15,n),DIM=2) + &
						 (sum(hhtechbtu(1:mnumcr,1:maxvtyp, 5,n),DIM=2) * PctPHEV20(n)) + &
						 (sum(hhtechbtu(1:mnumcr,1:maxvtyp, 6,n),DIM=2) * PctPHEV50(n))
!...Hydrogen
    TQLDV(7,1:mnumcr,N) = sum(hhtechbtu(1:mnumcr,1:maxvtyp,14,n),DIM=2) 

!...Diesel
    TQLDV(8,1:mnumcr,N) = sum(hhtechbtu(1:mnumcr,1:maxvtyp, 2,n),DIM=2) + &
	  				      sum(hhtechbtu(1:mnumcr,1:maxvtyp, 8,n),DIM=2) 

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

      REAL     PRIRATIO(4,MNUMCR),BETAP(4),GAMMAP, PCTAFL(4,MNUMCR,MNUMYR),&
               PCTFUEL_D_(4,MNUMCR,MNUMYR),PCTFAVL(4,MNUMCR,MNUMYR), &
               LDV_STOCK_E85NATL,LDV_STOCK_E85NATL_LAG,LDV_STOCK_E85REGNL(mnumcr-2),LDV_STOCK_E85REGNL_LAG(mnumcr-2)
      REAL     ALTPRR(4,MNUMCR),GASPRR(MNUMCR)
      
      REAL     PCTFLOOR(1:4) /0.00000001, 0.001, 0.20, 0.20/
      REAL     PCTCEILING(1:4) / 0.00000001, 0.70, 0.70, 0.30 /
      
      FCLogit0(1:9) = (/-1.2501, -1.6998, -2.1780, -2.0684, -0.8035, &
                        -0.7942, -0.9550, -1.3789, -1.1009/)
                       
      FCLOGIT1 = -2.2540
      FCLOGIT2 =  3.4444
      FCLOGIT3 = -18.830
      FCLogit4 = -2.4921

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
      IF(PMETR(MNUMCR,N).EQ.0) PMETR(MNUMCR,N)=SUM(PMETR(1:MNUMCR-2,N))/(MNUMCR-2) 

! ... Fill alternative fuel price array
      PCTFUEL_D_(1,1:mnumcr-2,N) = PMETR(1:mnumcr-2,N)  ! methanol
      PCTFUEL_D_(2,1:mnumcr-2,N) = PETTR(1:mnumcr-2,N)  ! methanol
      PCTFUEL_D_(3,1:mnumcr-2,N) = PGFTRPV(1:mnumcr-2,N)! CNG non-central fuel price
      PCTFUEL_D_(4,1:mnumcr-2,N) = PLGTR(1:mnumcr-2,N)  ! LPG

! ... Calculate regional price ratio for minimum alternative fuel use
      DO IFUELX=1,4
        DO IREGN=1,MNUMCR-2
          PRIRATIO(IFUELX,IREGN) = (PCTFUEL_D_(IFUELX,IREGN,N)**GAMMAP) / &
                                   ((PCTFUEL_D_(IFUELX,IREGN,N)**GAMMAP) + (PMGTR(IREGN,N)**GAMMAP))            ! MDRAEO2026 vectorize iregn
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
        IF(IFUELX.EQ.1) PCTFAVL(IFUELX,1:mnumcr-2,N) = 0.0000001
        IF(IFUELX .EQ. 2) THEN
          IF (curcalyr.le.2012) THEN
            IF(curcalyr.le.1995) THEN
              PCTFAVL(IFUELX,1:mnumcr-2,N) = INITSTA(3,6,1:mnumcr-2)/INITSTA(1,6,1:mnumcr-2)  ! ethanol
            ELSE
              PCTFAVL(IFUELX,1:mnumcr-2,N) = INITSTA(3,N,1:mnumcr-2)/INITSTA(1,N,1:mnumcr-2)  ! ethanol
            ENDIF
            E85AVAIL(1:mnumcr-2,N) = PCTFAVL(IFUELX,1:mnumcr-2,N)
          ELSE
            PCTFAVL(IFUELX,1:mnumcr-2,N) = E85AVAIL(1:mnumcr-2,N) ! ethanol
          END IF
        ELSEIF(IFUELX .EQ. 3) THEN
          PCTFAVL(IFUELX,1:mnumcr-2,N) = FAVL(5,1:mnumcr-2,YRS)  ! CNG
        ELSEIF(IFUELX .EQ. 4) THEN
          PCTFAVL(IFUELX,1:mnumcr-2,N) = FAVL(6,1:mnumcr-2,YRS)  ! LPG
        ENDIF
      ENDDO

! ... Pass regional E-85 fuel availability to PMM
      E85AVAIL(11,N) = 0.0
      LDV_STOCK_E85NATL = sum(LDV_STOCK(mnumcr,1:maxvtyp,1:maxowner,3,1:maxage,1:maxhav,n))
      LDV_STOCK_E85NATL_LAG = sum(LDV_STOCK(mnumcr,1:maxvtyp,1:maxowner,3,1:maxage,1:maxhav,n-1))
      LDV_STOCK_E85REGNL(1:mnumcr-2) = sum(sum(sum(sum(LDV_STOCK(1:mnumcr-2,1:maxvtyp,1:maxowner,3,1:maxage,1:maxhav,n),DIM=5),DIM=4),DIM=3),DIM=2)
      LDV_STOCK_E85REGNL_LAG(1:mnumcr-2) = sum(sum(sum(sum(LDV_STOCK(1:mnumcr-2,1:maxvtyp,1:maxowner,3,1:maxage,1:maxhav,n-1),DIM=5),DIM=4),DIM=3),DIM=2)
      
      if (yrs.le.STOCKYR) then 
        E85AVAIL(11,N) = sum(E85AVAIL(1:mnumcr-2,N)*LDV_STOCK_E85REGNL(1:mnumcr-2))/LDV_STOCK_E85NATL
      else
        E85AVAIL(11,N) = sum(E85AVAIL(1:mnumcr-2,N)*LDV_STOCK_E85REGNL_LAG(1:mnumcr-2))/LDV_STOCK_E85NATL_LAG
      endif

! ... Alternative fuel choice logit
      if(curcalyr.ge.2006)then
       DO IFUELX=1,4
         DO IREGN=1,MNUMCR-2
           ALTPRR(IFUELX,IREGN) = DEXP(DBLE(FClogit0(iregn))+(DBLE(PCTFUEL_D_(IFUELX,IREGN,N)* CFMGQ(n)/42.0)*DBLE(FCLOGIT1))- &
                          DBLE(FCLOGIT2)*(DEXP(DBLE(PCTFAVL(IFUELX,IREGN,N))*DBLE(FCLOGIT3))))
           GASPRR(IREGN) = DEXP(DBLE(PMGTR(IREGN,N)* CFMGQ(n)/42.0)*DBLE(FCLogit4))          
           PCTAFL(IFUELX,IREGN,N) = SNGL(ALTPRR(IFUELX,IREGN)/(ALTPRR(IFUELX,IREGN) + GASPRR(IREGN)))
         ENDDO
       ENDDO
      endif

! ... Set max penetration greater of minimum or logit
       PCTAF(1:4, 1:MNUMCR-2, N) = MAX(PCTAF(1:4, 1:MNUMCR-2, N), PCTAFL(1:4, 1:MNUMCR-2, N))
       PCTAF(1, 1:MNUMCR-2, N) = 0.00000001

! ... National average alternative fuel use percentage for flex and bi fuel vehicles
      DO IFUELX = 1,4
        if (yrs.le.STOCKYR) then 
            PCTAF(IFUELX,11,N) = sum(PCTAF(IFUELX,1:mnumcr-2,N)*LDV_STOCK_E85REGNL(1:mnumcr-2))/LDV_STOCK_E85NATL
        else
          PCTAF(IFUELX,11,N) = sum(PCTAF(IFUELX,1:mnumcr-2,N)*LDV_STOCK_E85REGNL_LAG(1:mnumcr-2))/LDV_STOCK_E85NATL_LAG
        endif
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
      
        IF (iregn.eq.10) then
          QMGTR(IREGN,N) = 0.0
          QMETR(IREGN,N) = 0.0
          QETTR(IREGN,N) = 0.0
          QNGTR(IREGN,N) = 0.0
          QLGTR(IREGN,N) = 0.0
          QPRTR(IREGN,N) = 0.0
          QELTR(IREGN,N) = 0.0
          QH2TR(IREGN,N) = 0.0
          QDSTR(IREGN,N) = 0.0
          QRSTR(IREGN,N) = 0.0
          QJFTR(IREGN,N) = 0.0
          QOTTR(IREGN,N) = 0.0
          CYCLE
        ENDIF
        
! ...   gasoline
        QMGTR(IREGN,N) = TQLDV(1,IREGN,N)      + &                         ! hhldv
                         FLTFUELBTU(iregn,1,n) + &                         ! ldv fleet 
                         cltfbtu(n,1,iregn)    + &                         ! commercial light truck						 
                         SUM(TFRBTU_F_T(N,1:3,2,IREGN)) + &                ! heavy truck (gasoline and gasoline HEV)            ! MDRAEO2026 vectorize iregn
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
      if(iregn.ne.10) E10SHARE(iregn,n) = (QRECR(1,iregn,n)/(CFMGQ(n)*1000000.0/42.0)*1000000.0)  & ! recreational boats
                                         /((QMGTR(iregn,n)/(CFMGQ(n)*1000000.0/42.0))*1000000.0)     ! total transportation motor gasoline

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
    tjftcbus=jftcbus
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

    INTEGER STEOBM,icheck,yseds, ibus
    INTEGER, PARAMETER :: TFRT_FUEL = 12        ! Number of powertrains from TRANFRT
    REAL   SUMCAR,SUMLTT,BMFAC
    REAL   MER_STEO_regn(MNUMCR,4)
	REAL   STMGTR_new(mnumcr,mnumyr), STJFTR_new(mnumcr,mnumyr), STDSTR_new(mnumcr,mnumyr), STRSTR_new(mnumcr,mnumyr)
    REAL   BCLTBTU(TFRT_FUEL,MNUMCR,MNUMYR)
    REAL   BEN_LDV(MAXLDV,MNUMCR,MNUMYR), BEN_CLT(TFRT_FUEL,MNUMCR,MNUMYR)
	
!...Benchmark factors are calculated using SEDS data for         
    yseds=baseyr+msedyr-1  ! calendar year for last SEDS data

    STEOBM = RTOVALUE('STEOBM  ',0)
    call traneasy
    
!   Initialize all benchmark factors to 1.0    
    BENEL(:,n)=1.0;BENNG(:,n)=1.0;BENMG(:,n)=1.0;BENJF(:,n)=1.0;BENDS(:,n)=1.0;BENRS(:,n)=1.0;&
    BENLG(:,N)=1.0;BENOT(:,n)=1.0;BENME(:,n)=1.0;BENET(:,n)=1.0;BENHY(:,n)=1.0

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
    
      if (iregn.eq.10) CYCLE
      
!...  Benchmark to SEDS
      if (CURCALYR.le.yseds) then 
        IF (QMGTR(iregn,n) .NE. 0.0) BENMG(iregn,n) = QSMGTR(iregn,n) / QMGTR(iregn,n)
        IF (QDSTR(iregn,n) .NE. 0.0) BENDS(iregn,n) = QSDSTR(iregn,n) / QDSTR(iregn,n)
        IF (QRSTR(iregn,n) .NE. 0.0) BENRS(iregn,n) = QSRSTR(iregn,n) / QRSTR(iregn,n)	

!       Determining SEDS consumption by region and fuel based on MER annual values		
		if (curcalyr.eq.yseds) then
          MER_STEO_regn(iregn,1) = QSMGTR(iregn,n) / QSMGTR(mnumcr,n) * MER_tran(1)     ! Gasoline
          MER_STEO_regn(iregn,2) = QSJFTR(iregn,n) / QSJFTR(mnumcr,n) * MER_tran(2)     ! Jet fuel
          MER_STEO_regn(iregn,3) = QSDSTR(iregn,n) / QSDSTR(mnumcr,n) * MER_tran(3)     ! Diesel
          MER_STEO_regn(iregn,4) = QSRSTR(iregn,n) / QSRSTR(mnumcr,n) * MER_tran(4)     ! Resid
!          WRITE(21,'(a11,2(",",i4),9(",",f12.1))')'mer_steo_1',curcalyr,curitr,MER_STEO_regn(iregn,:),MER_tran(:),QSMGTR(iregn,n)
		endif

      elseif (CURCALYR.eq.ymer) then 
        BENMG(iregn,n) = MER_STEO_regn(iregn,1) / QMGTR(iregn,n)
        BENDS(iregn,n) = MER_STEO_regn(iregn,3) / QDSTR(iregn,n)
        BENRS(iregn,n) = 1.0
        if (QRSTR(iregn,n).gt.0.0) BENRS(iregn,n) = MER_STEO_regn(iregn,4) / QRSTR(iregn,n)
        
      else
        BENMG(iregn,n) = BENMG(iregn,n-1)
        BENDS(iregn,n) = BENDS(iregn,n-1)

!       Resid is benchmarked to STEO
        if (curcalyr.le.ysteo.and.STEOBM.eq.1) then 
          BENRS(iregn,n) = 1.0
          if (QRSTR(iregn,n).gt.0.0) BENRS(iregn,n) = QRSTR(mnumcr,n-1) * TRFTCBUS(n)/TRFTCBUS(n-1) / QRSTR(mnumcr,n)
        else
          BENRS(iregn,n) = BENRS(iregn,n-1)
        endif

      endif
    enddo	!end regional loop
	 
!...Benchmarking transportation specific consumption variables
    DO IREGN=1,MNUMCR
      QMGTR(IREGN,N) = QMGTR(IREGN,N) * BENMG(IREGN,N)      ! motor gasoline
      QDSTR(IREGN,N) = QDSTR(IREGN,N) * BENDS(IREGN,N)      ! distillate
      QRSTR(IREGN,N) = QRSTR(IREGN,N) * BENRS(IREGN,N)      ! residual
    ENDDO

!   Adjust regional benchmark factors to ensure sum matches national total, even as
!   TDM consumption across regions varies in the projection 
    if(curcalyr.ge.ymer) then
      BENMG(1:MNUMCR-2,N) = BENMG(1:MNUMCR-2,N) * QMGTR(MNUMCR,N)/SUM(QMGTR(1:MNUMCR-2,N))      ! motor gasoline
      BENDS(1:MNUMCR-2,N) = BENDS(1:MNUMCR-2,N) * QDSTR(MNUMCR,N)/SUM(QDSTR(1:MNUMCR-2,N))      ! distillate
      BENRS(1:MNUMCR-2,N) = BENRS(1:MNUMCR-2,N) * QRSTR(MNUMCR,N)/SUM(QRSTR(1:MNUMCR-2,N))      ! residual

      QMGTR(1:MNUMCR-2,N) = QMGTR(1:MNUMCR-2,N) * QMGTR(MNUMCR,N)/SUM(QMGTR(1:MNUMCR-2,N))      ! motor gasoline
      QDSTR(1:MNUMCR-2,N) = QDSTR(1:MNUMCR-2,N) * QDSTR(MNUMCR,N)/SUM(QDSTR(1:MNUMCR-2,N))      ! distillate
      QRSTR(1:MNUMCR-2,N) = QRSTR(1:MNUMCR-2,N) * QRSTR(MNUMCR,N)/SUM(QRSTR(1:MNUMCR-2,N))      ! residual
    endif

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

!...Fill the balance sector quantities that are subtracted from associated rows in ftab Table 2.
!   Jet fuel is not benchmarked at all, so any difference between TDM and SEDS/MER/STEO is BS
    if (curcalyr.le.yseds) then
      QJFBS(1:mnumcr,N) = QSJFTR(1:mnumcr,n) - QJFTR(1:mnumcr,n)
    elseif (curcalyr.eq.ymer) then
      QJFBS(1:mnumcr,N) = MER_STEO_regn(1:mnumcr,2) - QJFTR(1:mnumcr,n)
!   Balance sector is filled with the delta between STEO and TDM for gas, diesel, and jet    
    elseif(curcalyr.gt.ymer) then

      if (STEOBM.eq.1) then
!       If before last STEO year, difference between STEO and TDM goes into unspecified/balance sector
!       STEO side of this is assumed to grow across all regions at same rate
        if (curcalyr.le.ysteo) then
          QMGBS(1:mnumcr,N) =  QMGTR(1:mnumcr,N-1) * TMGTCBUS(n)/TMGTCBUS(n-1) - QMGTR(1:mnumcr,N)
          QDSBS(1:mnumcr,N) =  QDSTR(1:mnumcr,N-1) * tdstcpus(n)/tdstcpus(n-1) - QDSTR(1:mnumcr,N)
          QJFBS(1:mnumcr,N) =  QJFTR(1:mnumcr,N-1) * tjftcbus(n)/tjftcbus(n-1) - QJFTR(1:mnumcr,N)
!       Leave unspecified gasoline/diesel/jet at the same proportion of total gas/diesel/jet as the last steo year
        else
          QMGBS(1:mnumcr,N) = QMGBS(1:mnumcr,ysteo-1989)/QMGTR(1:mnumcr,ysteo-1989) * QMGTR(1:mnumcr,N)
          QDSBS(1:mnumcr,N) = QDSBS(1:mnumcr,ysteo-1989)/QDSTR(1:mnumcr,ysteo-1989) * QDSTR(1:mnumcr,N)
          QJFBS(1:mnumcr,N) = QJFBS(1:mnumcr,ysteo-1989)/QJFTR(1:mnumcr,ysteo-1989) * QJFTR(1:mnumcr,N)
        endif
      else  ! Use MER year BS proportion of TR consumption for rest of projection if not benching to STEO
        QMGBS(1:mnumcr,N) = QMGBS(1:mnumcr,ymer-1989)/QMGTR(1:mnumcr,ymer-1989) * QMGTR(1:mnumcr,N)
        QDSBS(1:mnumcr,N) = QDSBS(1:mnumcr,ymer-1989)/QDSTR(1:mnumcr,ymer-1989) * QDSTR(1:mnumcr,N)
        QJFBS(1:mnumcr,N) = QJFBS(1:mnumcr,ymer-1989)/QJFTR(1:mnumcr,ymer-1989) * QJFTR(1:mnumcr,N)
      endif
      
    endif

!   Zero out region 10 balance sector    
    QMGBS(10,N) = 0.0
    QDSBS(10,N) = 0.0
    QJFBS(10,N) = 0.0

!   Add balance sector back into transportation sector totals (otherwise won't be accounted for in other modules, only reported)
!   MDRAEO2027 -- need to split BS out separately and let LFMM/others add it in separately in their modules -- MDRAEO2027    
    QMGTR(1:mnumcr,n) = QMGTR(1:mnumcr,n) + QMGBS(1:mnumcr,N)
    QDSTR(1:mnumcr,n) = QDSTR(1:mnumcr,n) + QDSBS(1:mnumcr,N)
    QJFTR(1:mnumcr,n) = QJFTR(1:mnumcr,n) + QJFBS(1:mnumcr,N)
    
  ! ... Write the benchmark factors and balance sector values to tranout.txt
    IF (N.eq.MNUMYR.and.FCRL.eq.1) THEN
	  WRITE(21,*)'============== BENCHMARK FACTORS =============='
	  WRITE(21,'(a4,4(",",a8))')'YEAR' , 'BENMG' , 'BENDS' , 'BENRS','BENEL'
	  DO icheck = 6, MNUMYR
	    WRITE(21,'(i4,4(",",f8.3))')icheck+1989,BENMG(11,icheck),BENDS(11,icheck),BENRS(11,icheck),BENEL(11,icheck)
	  ENDDO
	ENDIF

!...FFV total fuel demand by region passed to PMM
    QFFV(1:mnumcr,n) = QETTR(1:mnumcr,N)        

    do iregn = 1,mnumcr
      if (iregn.eq.10) CYCLE
      BEN_CLT(1,iregn,n) = BENMG(iregn,n)
      BEN_CLT(2,iregn,n) = BENDS(iregn,n)
      BEN_CLT(3,iregn,n) = BENLG(iregn,n)
      BEN_CLT(4,iregn,n) = BENNG(iregn,n)
      BEN_CLT(5,iregn,n) = (BENET(iregn,n)*PCTAF(2,mnumcr,N)+BENMG(iregn,n)*(1.0-PCTAF(2,mnumcr,N)))
      BEN_CLT(6,iregn,n) = BENEL(iregn,n)
      BEN_CLT(7,iregn,n) = (BENEL(iregn,n)*PctEVMT_PHEV(N,4,IREGN,1) + BENDS(iregn,n)*(1.0 - PctEVMT_PHEV(N,4,IREGN,1)))
      BEN_CLT(8,iregn,n) = (BENEL(iregn,n)*PctEVMT_PHEV(N,4,IREGN,2) + BENMG(iregn,n)*(1.0 - PctEVMT_PHEV(N,4,IREGN,2)))
      BEN_CLT(9,iregn,n) = BENHY(iregn,n)
      BEN_CLT(10,iregn,n) = BENHY(iregn,n)
      BEN_CLT(11,iregn,n) = BENMG(iregn,n)
      BEN_CLT(12,iregn,n) = BENHY(iregn,n)

      BEN_LDV([1,16],iregn,n)     = BENMG(iregn,n)
      BEN_LDV([2,8],iregn,n)      = BENDS(iregn,n)
      BEN_LDV(3,iregn,n)          = (BENET(IREGN,N)*PCTAF(2,iregn,N)+BENMG(iregn,n)*(1.0-PCTAF(2,iregn,N)))
      BEN_LDV([4,7,15],iregn,n)   = BENEL(IREGN,N)
      BEN_LDV(5,iregn,n)          = (BENEL(IREGN,N)*PctPHEV20(N)+BENMG(iregn,n)*(1.0-PctPHEV20(N)))
      BEN_LDV(6,iregn,n)          = (BENEL(IREGN,N)*PctPHEV50(N)+BENMG(iregn,n)*(1.0-PctPHEV50(N)))
      BEN_LDV(9,iregn,n)          = (BENNG(IREGN,N)*PCTAF(3,iregn,N)+BENMG(iregn,n)*(1.0-PCTAF(3,iregn,N)))
      BEN_LDV(10,iregn,n)         = (BENLG(IREGN,N)*PCTAF(4,iregn,N)+BENMG(iregn,n)*(1.0-PCTAF(4,iregn,N)))
      BEN_LDV(11,iregn,n)         = BENNG(IREGN,N)
      BEN_LDV(12,iregn,n)         = BENLG(IREGN,N)
      BEN_LDV(13,iregn,n)         = BENME(IREGN,N)
      BEN_LDV(14,iregn,n)         = BENHY(IREGN,N)
    enddo

!...----- Benchmark consumption variables -----
!...  Light duty vehicles
!     Total
      BTQLDV(1,1:MNUMCR) = TQLDV(1,1:MNUMCR,N) * BENMG(MNUMCR,n)                   ! Gasoline
      BTQLDV(2,1:MNUMCR) = TQLDV(2,1:MNUMCR,N) * BENME(MNUMCR,N)                   ! Methanol
      BTQLDV(3,1:MNUMCR) = TQLDV(3,1:MNUMCR,N) * BENET(MNUMCR,N)                   ! Ethanol
      BTQLDV(4,1:MNUMCR) = TQLDV(4,1:MNUMCR,N) * BENNG(MNUMCR,N)                   ! CNG
      BTQLDV(5,1:MNUMCR) = TQLDV(5,1:MNUMCR,N) * BENLG(MNUMCR,N)                   ! LPG
      BTQLDV(6,1:MNUMCR) = TQLDV(6,1:MNUMCR,N) * BENEL(MNUMCR,N)                   ! Electricity
      BTQLDV(7,1:MNUMCR) = TQLDV(7,1:MNUMCR,N) * BENHY(MNUMCR,N)                   ! Hydrogen
      BTQLDV(8,1:MNUMCR) = TQLDV(8,1:MNUMCR,N) * BENDS(MNUMCR,n)                   ! Diesel
!     Fleet
!     by fuel
      BFLTFUELBTU(1:MNUMCR,1,n) = FLTFUELBTU(1:MNUMCR,1,n) * BENMG(MNUMCR,n)
      BFLTFUELBTU(1:MNUMCR,2,n) = FLTFUELBTU(1:MNUMCR,2,n) * BENME(MNUMCR,n)
      BFLTFUELBTU(1:MNUMCR,3,n) = FLTFUELBTU(1:MNUMCR,3,n) * BENET(MNUMCR,n)
      BFLTFUELBTU(1:MNUMCR,4,n) = FLTFUELBTU(1:MNUMCR,4,n) * BENNG(MNUMCR,n) 
      BFLTFUELBTU(1:MNUMCR,5,n) = FLTFUELBTU(1:MNUMCR,5,n) * BENLG(MNUMCR,n) 
      BFLTFUELBTU(1:MNUMCR,6,n) = FLTFUELBTU(1:MNUMCR,6,n) * BENEL(MNUMCR,n)
      BFLTFUELBTU(1:MNUMCR,7,n) = FLTFUELBTU(1:MNUMCR,7,n) * BENHY(MNUMCR,n)
      BFLTFUELBTU(1:MNUMCR,8,n) = FLTFUELBTU(1:MNUMCR,8,n) * BENDS(MNUMCR,n)
!     by powertrain
      do iregn = 1, MNUMCR
        do ivtyp=1,maxvtyp
          FLTLDVBTU(iregn,ivtyp,1:maxldv,n) = FLTLDVBTU(iregn,ivtyp,1:maxldv,n) * BEN_LDV(1:maxldv,iregn,n)
        enddo
        fltldvbtut(iregn,1:maxldv,n) = sum(FLTLDVBTU(iregn,1:maxvtyp,1:maxldv,n),DIM=1)   ! sum across ivtyp
      enddo
      
!...  Freight trucks
!     Light heavy-duty trucks                                                                       
      BTQFREIRSC(1,1,1:MNUMCR) = TFRBTU_F_T(N,1,1,1:MNUMCR)*BENDS(MNUMCR,n)        ! Diesel
      BTQFREIRSC(1,2,1:MNUMCR) = TFRBTU_F_T(N,1,2,1:MNUMCR)*BENMG(MNUMCR,n)        ! Gasoline
      BTQFREIRSC(1,3,1:MNUMCR) = TFRBTU_F_T(N,1,3,1:MNUMCR)*BENLG(MNUMCR,N)        ! LPG
      BTQFREIRSC(1,4,1:MNUMCR) = TFRBTU_F_T(N,1,4,1:MNUMCR)*BENNG(MNUMCR,N)        ! Natural Gas
	  BTQFREIRSC(1,5,1:MNUMCR) = TFRBTU_F_T(N,1,5,1:MNUMCR)*BENET(MNUMCR,N)        ! Ethanol
	  BTQFREIRSC(1,6,1:MNUMCR) = TFRBTU_F_T(N,1,6,1:MNUMCR)*BENEL(MNUMCR,N)        ! Electric
	  BTQFREIRSC(1,7,1:MNUMCR) = TFRBTU_F_T(N,1,7,1:MNUMCR)*BENHY(MNUMCR,N)        ! Hydrogen		
!     Medium heavy-duty trucks
      BTQFREIRSC(2,1,1:MNUMCR) = TFRBTU_F_T(N,2,1,1:MNUMCR)*BENDS(MNUMCR,n)        ! Diesel
      BTQFREIRSC(2,2,1:MNUMCR) = TFRBTU_F_T(N,2,2,1:MNUMCR)*BENMG(MNUMCR,n)        ! Gasoline
      BTQFREIRSC(2,3,1:MNUMCR) = TFRBTU_F_T(N,2,3,1:MNUMCR)*BENLG(MNUMCR,N)        ! LPG
      BTQFREIRSC(2,4,1:MNUMCR) = TFRBTU_F_T(N,2,4,1:MNUMCR)*BENNG(MNUMCR,N)        ! Natural Gas
	  BTQFREIRSC(2,5,1:MNUMCR) = TFRBTU_F_T(N,2,5,1:MNUMCR)*BENET(MNUMCR,N)        ! Ethanol
	  BTQFREIRSC(2,6,1:MNUMCR) = TFRBTU_F_T(N,2,6,1:MNUMCR)*BENEL(MNUMCR,N)        ! Electric
	  BTQFREIRSC(2,7,1:MNUMCR) = TFRBTU_F_T(N,2,7,1:MNUMCR)*BENHY(MNUMCR,N)        ! Hydrogen						
!     Heavy heavy-duty trucks
      BTQFREIRSC(3,1,1:MNUMCR) = TFRBTU_F_T(N,3,1,1:MNUMCR)*BENDS(MNUMCR,n)        ! Diesel
      BTQFREIRSC(3,2,1:MNUMCR) = TFRBTU_F_T(N,3,2,1:MNUMCR)*BENMG(MNUMCR,n)        ! Gasoline
      BTQFREIRSC(3,3,1:MNUMCR) = TFRBTU_F_T(N,3,3,1:MNUMCR)*BENLG(MNUMCR,N)        ! LPG
      BTQFREIRSC(3,4,1:MNUMCR) = TFRBTU_F_T(N,3,4,1:MNUMCR)*BENNG(MNUMCR,N)        ! Natural Gas
	  BTQFREIRSC(3,5,1:MNUMCR) = TFRBTU_F_T(N,3,5,1:MNUMCR)*BENET(MNUMCR,N)        ! Ethanol
	  BTQFREIRSC(3,6,1:MNUMCR) = TFRBTU_F_T(N,3,6,1:MNUMCR)*BENEL(MNUMCR,N)        ! Electric
	  BTQFREIRSC(3,7,1:MNUMCR) = TFRBTU_F_T(N,3,7,1:MNUMCR)*BENHY(MNUMCR,N)        ! Hydrogen

!...  Domestic shipping
      BTQDSHIPR(1,1:MNUMCR) = TQDSHIPR(1,1:MNUMCR,N) * BENDS(MNUMCR,n)             ! Diesel
      BTQDSHIPR(2,1:MNUMCR) = TQDSHIPR(2,1:MNUMCR,N) * BENRS(MNUMCR,n)             ! Residual
      BTQDSHIPR(3,1:MNUMCR) = TQDSHIPR(3,1:MNUMCR,N)                              ! CNG
      BTQDSHIPR(4,1:MNUMCR) = TQDSHIPR(4,1:MNUMCR,N)                              ! LNG 

!...  International shipping
!     1=distillate   2=residual  3=cng  4=lng  5=low sulfur fuel oil
      BTQISHIPR(1,1:MNUMCR) = TQISHIPR(1,1:MNUMCR,N) * BENDS(MNUMCR,n)
      BTQISHIPR(2,1:MNUMCR) = (TQISHIPR(2,1:MNUMCR,N) + TQISHIPR(5,1:MNUMCR,N))* BENRS(MNUMCR,n)
      BTQISHIPR(3,1:MNUMCR) = TQISHIPR(3,1:MNUMCR,N)  ! CNG 
      BTQISHIPR(4,1:MNUMCR) = TQISHIPR(4,1:MNUMCR,N)  ! LNG 
        
!...  Freight rail
      BTQRAILR(1,1:MNUMCR) = TQRAILR(1,1:MNUMCR,N) * BENDS(MNUMCR,n)             ! Diesel
      BTQRAILR(2,1:MNUMCR) = TQRAILR(2,1:MNUMCR,N) * BENRS(MNUMCR,n)             ! Residual     
      BTQRAILR(3,1:MNUMCR) = TQRAILR(3,1:MNUMCR,N)                              ! CNG
      BTQRAILR(4,1:MNUMCR) = TQRAILR(4,1:MNUMCR,N)                              ! LNG 

!...  Passenger rail
      TRQRAILR(1,1:MNUMCR,N) = qmtrr(2,1:MNUMCR,n) * benel(MNUMCR,n)
      TRQRAILR(2,1:MNUMCR,N) = qmtrr(1,1:MNUMCR,n) * bends(MNUMCR,n)

!...  Military
!     1=distillate   2=jet fuel naphtha   3=residual   4=jet fuel kerosene
      BQMILTR(1,1:MNUMCR) = QMILTR(1,1:MNUMCR,N) * BENDS(MNUMCR,n)
      BQMILTR(2,1:MNUMCR) = QMILTR(2,1:MNUMCR,N) * BENJF(MNUMCR,n)
      BQMILTR(3,1:MNUMCR) = QMILTR(3,1:MNUMCR,N) * BENRS(MNUMCR,n)
      BQMILTR(4,1:MNUMCR) = QMILTR(4,1:MNUMCR,N) * BENJF(MNUMCR,n)

!...  Commercial light trucks by powertrain
      BCLTBTU(1:TFRT_FUEL,1:MNUMCR,N)  = CLTBTUT(1:TFRT_FUEL,1:MNUMCR,N) * SPREAD(BEN_CLT(1:TFRT_FUEL,MNUMCR,n), DIM=2, NCOPIES=MNUMCR)
      
!...  Other modes
      BQJETR(1:MNUMCR) = QJETR(1:MNUMCR,N) * BENJF(MNUMCR,n)             ! jet fuel
      BQAGR(1:MNUMCR)  = QAGTR(1:MNUMCR,N)                               ! aviation gasoline already benchmarked
      BQRECR(1:MNUMCR) = QRECR(1,1:MNUMCR,N) * BENMG(MNUMCR,n) + &       ! recreational boat gasoline
                         QRECR(2,1:MNUMCR,N) * BENDS(MNUMCR,n)           ! recreational boat diesel
      BQLUBR(1:MNUMCR) = QLUTR(1:MNUMCR,N) * BENOT(MNUMCR,N)             ! lubrication

!...Some national totals 
!   Commercial light truck 
!   by powertrain
    BCLTBTUT(1:TFRT_FUEL,1:mnumcr) = BCLTBTU(1:TFRT_FUEL,1:mnumcr,N)
    
!   by fuel
    CLTFUELBTU(:,n) = 0.0
    CLTFUELBTU(1,n) = cltfbtu(n,1,mnumcr) * BENMG(MNUMCR,n)   ! Gasoline
    CLTFUELBTU(2,n) = cltfbtu(n,2,mnumcr) * BENDS(MNUMCR,n)   ! Diesel
	CLTFUELBTU(3,n) = cltfbtu(n,3,mnumcr) * BENLG(MNUMCR,n)   ! LPG
	CLTFUELBTU(4,n) = cltfbtu(n,4,mnumcr) * BENNG(MNUMCR,n)   ! CNG
	CLTFUELBTU(5,n) = cltfbtu(n,5,mnumcr) * BENET(MNUMCR,n)   ! Ethanol
	CLTFUELBTU(6,n) = cltfbtu(n,6,mnumcr) * BENEL(MNUMCR,n)   ! Electric
	CLTFUELBTU(7,n) = cltfbtu(n,7,mnumcr) * BENHY(MNUMCR,n)   ! Hydrogen
    
!   Mass transit {1: transit, 2: intercity, 3: school}
    do ibus = 1, 3
      TRQBUS(ibus,1,N) = SUM(QMTBR(ibus,1,1:MNUMCR-2,N)) * BENMG(MNUMCR,N)
      TRQBUS(ibus,2,N) = SUM(QMTBR(ibus,2,1:MNUMCR-2,N)) * BENDS(MNUMCR,N)
      TRQBUS(ibus,3,N) = SUM(QMTBR(ibus,3,1:MNUMCR-2,N)) * BENET(MNUMCR,N)
      TRQBUS(ibus,4,N) = SUM(QMTBR(ibus,4,1:MNUMCR-2,N)) * BENME(MNUMCR,N)
      TRQBUS(ibus,5,N) = SUM(QMTBR(ibus,5,1:MNUMCR-2,N)) * BENNG(MNUMCR,N)
      TRQBUS(ibus,6,N) = SUM(QMTBR(ibus,6,1:MNUMCR-2,N)) * BENLG(MNUMCR,N)
      TRQBUS(ibus,7,N) = SUM(QMTBR(ibus,7,1:MNUMCR-2,N)) * BENEL(MNUMCR,N)
      TRQBUS(ibus,8,N) = SUM(QMTBR(ibus,8,1:MNUMCR-2,N)) * BENHY(MNUMCR,N)
    enddo
      
!   ----- Benchmark VMT -----
!...Household VMT (billion miles) by technology
    do iregn=1,mnumcr-2
      BVMTECH(1:maxldv,iregn) = sum(VMTHH(N,iregn,1:maxldv,1:maxvtyp),DIM=2) * BEN_LDV(1:maxldv,mnumcr,n)
    enddo    
    BVMTECH(1:maxldv,mnumcr) = sum(BVMTECH(1:maxldv,1:mnumcr-2),DIM=2)

!...Calculate total car and l.t. vmt by technology
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
	  
!...Light-duty fleet vehicle VMT
    do ivtyp=1,maxvtyp
      do ifleet=1,maxfleet
        do ihav=1,maxhav
          FLTVMTHAV(ivtyp,ifleet,1:maxldv,ihav,yrs)= fltechvmt(mnumcr,ivtyp,ifleet,1:maxldv,ihav) * BEN_LDV(1:maxldv,mnumcr,n)
        enddo
      enddo
    enddo

!...Convert benchmarked fleet vmt to billion miles; collapse ihav dimension
	BFLTVMTECH(1:maxvtyp,1:maxfleet,1:maxldv) = sum(FLTVMTHAV(1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav,yrs),DIM=4)/1000000000.0
    
!...Commercial light truck VMT 
    BCLTVMT(1:TFRT_FUEL,N)  = CLTVMTT(1:TFRT_FUEL,N) * BEN_CLT(1:TFRT_FUEL,mnumcr,n)

!...Freight truck VMT
!   Used to populate TRVMTTRK for Table 7
    do iregn=1,mnumcr
!     Light and medium heavy-duty trucks	  
      BFVMTECHSC(1,1,iregn) = sum(VMTFLT_SAF_TR(1:2,1,iregn))*BENDS(mnumcr,N)                                                            ! Diesel
      BFVMTECHSC(1,2,iregn) = sum(VMTFLT_SAF_TR(1:2,2,iregn))*BENMG(mnumcr,N)                                                           ! Gasoline
      BFVMTECHSC(1,3,iregn) = sum(VMTFLT_SAF_TR(1:2,3,iregn))*BENLG(mnumcr,N)                                                            ! Lpg
      BFVMTECHSC(1,4,iregn) = sum(VMTFLT_SAF_TR(1:2,4,iregn))*BENNG(mnumcr,N)                                                            ! Cng
	  BFVMTECHSC(1,5,iregn) = sum(VMTFLT_SAF_TR(1:2,5,iregn))*(BENET(mnumcr,N)*PCTAF(2,iregn,n) + BENMG(mnumcr,N)*(1.0-PCTAF(2,iregn,n))) ! Flex fuel
	  BFVMTECHSC(1,6,iregn) = sum(VMTFLT_SAF_TR(1:2,6,iregn))*BENEL(mnumcr,n)                                                            ! Electric 
	  BFVMTECHSC(1,7,iregn) = VMTFLT_SAF_TR(1,7,iregn)*(BENEL(mnumcr,N)*PctEVMT_PHEV(N,1,iregn,1) + BENDS(mnumcr,N)*(1.0-PctEVMT_PHEV(N,1,iregn,1)))&		! PHEV Diesel, Class 3
	  						+ VMTFLT_SAF_TR(2,7,iregn)*(BENEL(mnumcr,N)*PctEVMT_PHEV(N,2,iregn,1) + BENDS(mnumcr,N)*(1.0-PctEVMT_PHEV(N,2,iregn,1)))		! PHEV Diesel, Class 4-6
	  BFVMTECHSC(1,8,iregn) = VMTFLT_SAF_TR(1,8,iregn)*(BENEL(mnumcr,N)*PctEVMT_PHEV(N,1,iregn,2) + BENMG(mnumcr,N)*(1.0-PctEVMT_PHEV(N,1,iregn,2)))&		! PHEV Gasoline, Class 3
	  						+ VMTFLT_SAF_TR(2,8,iregn)*(BENEL(mnumcr,N)*PctEVMT_PHEV(N,2,iregn,2) + BENMG(mnumcr,N)*(1.0-PctEVMT_PHEV(N,2,iregn,2)))		! PHEV Gasoline, Class 4-6
	  BFVMTECHSC(1,9,iregn) = sum(VMTFLT_SAF_TR(1:2,9,iregn))*BENHY(mnumcr,n)                                                            ! FCEV
	  BFVMTECHSC(1,10,iregn)= sum(VMTFLT_SAF_TR(1:2,10,iregn))*BENHY(mnumcr,N)															! FCHEV
	  BFVMTECHSC(1,11,iregn)= sum(VMTFLT_SAF_TR(1:2,11,iregn))*BENMG(mnumcr,N)															! Gasoline HEV
	  BFVMTECHSC(1,12,iregn)= sum(VMTFLT_SAF_TR(1:2,12,iregn))*BENHY(mnumcr,N)															! H2 ICE

!     Heavy heavy-duty trucks		
      BFVMTECHSC(2,1,iregn) = VMTFLT_SAF_TR(3,1,iregn)*BENDS(mnumcr,N)                                                                   ! Diesel
      BFVMTECHSC(2,2,iregn) = VMTFLT_SAF_TR(3,2,iregn)*BENMG(mnumcr,N)                                                                   ! Gasoline
      BFVMTECHSC(2,3,iregn) = VMTFLT_SAF_TR(3,3,iregn)*BENLG(mnumcr,N)                                                                   ! Lpg
      BFVMTECHSC(2,4,iregn) = VMTFLT_SAF_TR(3,4,iregn)*BENNG(mnumcr,N)                                                                   ! Cng
	  BFVMTECHSC(2,5,iregn) = VMTFLT_SAF_TR(3,5,iregn)*(BENET(mnumcr,N)*PCTAF(2,iregn,n) + BENMG(mnumcr,N)*(1.0-PCTAF(2,iregn,n)))        ! Flex fuel
	  BFVMTECHSC(2,6,iregn) = VMTFLT_SAF_TR(3,6,iregn)*BENEL(mnumcr,n)                                                                   ! Electric 
	  BFVMTECHSC(2,7,iregn) = VMTFLT_SAF_TR(3,7,iregn)*(BENEL(mnumcr,N)*PctEVMT_PHEV(N,3,iregn,1) + BENDS(mnumcr,N)  *(1.0-PctEVMT_PHEV(N,3,iregn,1)))    	! PHEV Diesel
	  BFVMTECHSC(2,8,iregn) = VMTFLT_SAF_TR(3,8,iregn)*(BENEL(mnumcr,N)*PctEVMT_PHEV(N,3,iregn,2) + BENMG(mnumcr,N)*(1.0-PctEVMT_PHEV(N,3,iregn,2)))    	! PHEV Gasoline
	  BFVMTECHSC(2,9,iregn) = VMTFLT_SAF_TR(3,9,iregn)*BENHY(mnumcr,n)  		                                                            ! FCEV 
	  BFVMTECHSC(2,10,iregn)= VMTFLT_SAF_TR(3,10,iregn)*BENHY(mnumcr,n) 																	! FCHEV
	  BFVMTECHSC(2,11,iregn)= VMTFLT_SAF_TR(3,11,iregn)*BENMG(mnumcr,n) 																	! Gasoline HEV
	  BFVMTECHSC(2,12,iregn)= VMTFLT_SAF_TR(3,12,iregn)*BENHY(mnumcr,n) 																	! H2 ICE
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
        BRTMTT(n,iregn) = RTMTT(n,iregn) * BENDS(mnumcr,n)
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
    USE MEAN_FUNCS
    IMPLICIT NONE


	  REAL   TRQLDT(MNUMYR),TRQHWY_TMP(2,MNUMYR),TRQHWY_SHR(2,MNUMYR), &
			 VMT_STK_TOT(maxvtyp,maxldv)

      REAL   FLTECHRPT_REG(2,MNUMCR,MAXLDV,MNUMYR),&
             ANCSALE(MNUMCR,MNUMYR),ANTSALE(MNUMCR,MNUMYR), CQ(MNUMYR),LTQ(MNUMYR), ldvshr

      REAL   NUM1,NUM2,NUM3,DEN1,DEN2,DEN3,FLTNUM1,FLTNUM2,FLTDEN1,FLTDEN2,                  &  
             cmpg_1(maxldv),tmpg_1(maxldv),lmpg_1(maxldv),cmpg_2(maxldv),tmpg_2(maxldv),     &
             lmpg_2(maxldv)
             
      REAL   TECHMPG_NUM(7)             ! Numerator for sales-weighted new vehicle fuel economy (TECHMPG) and VMT-weighted stock fuel economy (STKMPG)
      
      INTEGER isec,impg
      INTEGER mpgmap(maxldv),mpgmapCLT(12)
      
      data mpgmap/1,2,1,5,4,4,5,3,7,7,7,7,7,6,5,3/
      data mpgmapCLT/1,2,7,7,1,5,4,4,6,6,3,7/
    
    
!   Initialize
    TTHcons(:,n)        = 0.0
    TRQHWY(:,N)         = 0.0
    TRVMTTRK(:,:,N)     = 0.0
    PAS_RPM(:,n)        = 0.0
    TRQLDV(:,:,n)       = 0.0
    TRQLDT(N)           = 0.0
    TRQFTRK(:,N)        = 0.0
    TRQFTRK_new(:,:,n)  = 0.0
    TRQHWY_new(:,N)     = 0.0
    
! ... ***** TABLE 7 *******
! ... Total freight truck vmt
      TRVMTTRK(1:2,1:12,N) = sum(BFVMTECHSC(1:2,1:12,1:mnumcr-2),DIM=3) / (1.0*1E9)

!   Passenger Travel (billion vehicle miles travelled) 		
!   Motorcycles - pin future growth to LDV travel growth
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
      TRQLDV(ifuel,1:mnumcr,n) = BTQLDV(ifuel,1:mnumcr) + BFLTFUELBTU(1:mnumcr,ifuel,n)
    enddo

    TRQLDT(N) = SUM(TRQLDV([1:maxfuel],11,N))

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

!...for report writer 
	fltfcldvbtu(1:maxvtyp,1:maxldv,n) = fltldvbtu(mnumcr,1:maxvtyp,1:maxldv,n)

! ... Calculate personal car and l.t. consumption
      CQ(N)  = 0.0
      LTQ(N) = 0.0
      DO ILDV=1,MAXLDV		
		IF (HHMPGSTK_TYPLDV(ILDV,1,N) .NE. 0.0) &
          CQ(N) = CQ(N) + (sum(BVMT_STK_HH(1,ILDV,1:maxage,1,1:mnumcr-2)) /HHMPGSTK_TYPLDV(ILDV,1,N)) * CFMGQ(n)/42.0
        IF (HHMPGSTK_TYPLDV(ILDV,2,N) .NE. 0.0) &
          LTQ(N) = LTQ(N) + (sum(BVMT_STK_HH(2,ILDV,1:maxage,1,1:mnumcr-2))/HHMPGSTK_TYPLDV(ILDV,2,N)) * CFMGQ(n)/42.0 
      ENDDO

! ... Calculate energy use by light duty vehicles (cars, l.t., motorcycles)
      TRQHWY_TMP(1,N) = CQ(N) + sum(fltldvbtu(mnumcr,1,1:maxldv,n))     ! Cars
      TRQHWY_TMP(2,N) = LTQ(N) + sum(fltldvbtu(mnumcr,2,1:maxldv,n))    ! Light trucks
      TRQHWY(3,N) = PAS_RPM(1,n) / Cyc_MPG(n) * CFMGQ(n)/42 *1000       ! Motorcycles

! ... Adjust the consumption so they match the total shown in tables 7 & 46
      TRQHWY_SHR(1,N) = TRQHWY_TMP(1,N) / (TRQHWY_TMP(1,N) + TRQHWY_TMP(2,N))
      TRQHWY_SHR(2,N) = TRQHWY_TMP(2,N) / (TRQHWY_TMP(1,N) + TRQHWY_TMP(2,N))

      TRQHWY(1,N) = (TRQLDT(N) - TRQHWY(3,N)) * TRQHWY_SHR(1,N)
      TRQHWY(2,N) = (TRQLDT(N) - TRQHWY(3,N)) * TRQHWY_SHR(2,N)

! ... Calculate energy use by air
!	  QJETR_DI(maxwreg,domint,2,mnumyr)  [where third dimension is 1: pass, 2: freight]
!	  Assume general aviation accounts for 5% of total jet fuel consumption, in addition to avgas.
      TRQNHWY(1,N) = BQAGR(11) + (sum(QJETR_DI(1,:,:,N)) - sum(QJETR_DI(1,:,:,N))/1.05) * BENJF(MNUMCR,N)	! General aviation
      TRQNHWY(2,N) = (QJETR_DI(1,1,1,N)/1.05)* BENJF(MNUMCR,N)												! Passenger Domestic
      TRQNHWY(3,N) = (QJETR_DI(1,2,1,N)/1.05) * BENJF(MNUMCR,N)												! Passenger International
      TRQNHWY(4,N) = (sum(QJETR_DI(1,:,2,N))/1.05)* BENJF(MNUMCR,N)	  										! Freight (domestic and int'l)
						 
! ... Calculate energy use by water
      TRQDOMS(1,N) = BTQDSHIPR(1,mnumcr)       ! diesel
      TRQDOMS(2,N) = BTQDSHIPR(2,mnumcr)       ! residual
      TRQDOMS(3,N) = BTQDSHIPR(3,mnumcr)       ! CNG 
      TRQDOMS(4,N) = BTQDSHIPR(4,mnumcr)       ! LNG 

      TRQINTS(1,N) = BTQISHIPR(1,mnumcr)                    ! diesel
      TRQINTS(2,N) = BTQISHIPR(2,mnumcr)                    ! residual
      TRQINTS(3,N) = BTQISHIPR(3,mnumcr)                    ! CNG 
      TRQINTS(4,N) = BTQISHIPR(4,mnumcr)                    ! LNG 
      
      TRQBOAT(1,N) = SUM(QRECR(1,1:mnumcr-2,N)) * BENMG(mnumcr,n)             ! gasoline
      TRQBOAT(2,N) = SUM(QRECR(2,1:mnumcr-2,N)) * BENDS(mnumcr,n)             ! diesel

! ... Calculate energy use by rail (freight, passenger)
      TRQRRF(1,N) = BTQRAILR(1,mnumcr)       ! diesel
      TRQRRF(2,N) = BTQRAILR(2,mnumcr)       ! residual
      TRQRRF(3,N) = BTQRAILR(3,mnumcr)       ! CNG 
      TRQRRF(4,N) = BTQRAILR(4,mnumcr)       ! LNG 
      
      TRQRRP(1,N) = IREDE(N)                                       ! intercity rail elec.
      TRQRRP(2,N) = IREDD(N)*BENDS(MNUMCR,N)                       ! intercity rail diesel
      TRQRRP(3,N) = 0.0                                            ! intercity rail CNG
      TRQRRP(4,N) = 0.0                                            ! intercity rail LNG
      TRQRRP(5,N) = TRED(MNUMCR,N)                                 ! transit rail elec.
      TRQRRP(6,N) = CREDE(MNUMCR,N)                                ! commuter rail elec.
      TRQRRP(7,N) = CREDD(MNUMCR,N)*BENDS(mnumcr,N)                ! commuter rail diesel
      TRQRRP(8,N) = 0.0                                            ! commuter rail CNG
      TRQRRP(9,N) = 0.0                                            ! commuter rail LNG
      
! ... Calculate energy use by lubricants
      TRQLUB(N) = BQLUBR(mnumcr)

! ... Calculate energy use by military
      TRQMIL(1,N) = BQMILTR(4,mnumcr)     ! jet fuel kerosene
      TRQMIL(2,N) = BQMILTR(2,mnumcr)     ! jet fuel naphtha
      TRQMIL(3,N) = BQMILTR(3,mnumcr)     ! residual
      TRQMIL(4,N) = BQMILTR(1,mnumcr)     ! distillate

! ... Calculate energy use by type
      TRQENUSE(1,N)  = QMGTR(mnumcr,N)
      TRQENUSE(2,N)  = QDSTR(mnumcr,N)
      TRQENUSE(3,N)  = QJFTR(mnumcr,N)
      TRQENUSE(4,N)  = QRSTR(mnumcr,N)
      TRQENUSE(5,N)  = QAGTR(mnumcr,N) 
      TRQENUSE(6,N)  = QLGTR(mnumcr,N)
      TRQENUSE(7,N)  = QLUTR(mnumcr,N) 
      TRQENUSE(8,N)  = QMETR(mnumcr,N)
      TRQENUSE(9,N)  = QETTR(mnumcr,N)
      TRQENUSE(10,N) = QELTR(mnumcr,N)
      TRQENUSE(11,N) = QNGTR(mnumcr,N)
      TRQENUSE(12,N) = QH2TR(mnumcr,N)

!...Populate electricity consumption array for EMM
!	These values are all benchmarked.
	TRQ_ELEC(:,:,N) = 0.0
	TRQ_ELEC(1,1:mnumcr-2,N) = TRQLDV(6,1:mnumcr-2,N)*chg_dist(1:mnumcr-2,2,n)				! LDV Home
	TRQ_ELEC(2,1:mnumcr-2,N) = TRQLDV(6,1:mnumcr-2,N)*chg_dist(1:mnumcr-2,3,n)			    ! LDV public L2
	TRQ_ELEC(3,1:mnumcr-2,N) = TRQLDV(6,1:mnumcr-2,N)-SUM(TRQ_ELEC(1:2,1:mnumcr-2,N),DIM=1) ! LDV public DCFC
	TRQ_ELEC(4,1:mnumcr-2,N) = QMTBR(3,7,1:mnumcr-2,N) * BENEL(1:mnumcr-2,N)			    ! Bus school
	TRQ_ELEC(5,1:mnumcr-2,N) = QMTBR(1,7,1:mnumcr-2,N) * BENEL(1:mnumcr-2,N)			    ! Bus transit
	TRQ_ELEC(6,1:mnumcr-2,N) = QMTBR(2,7,1:mnumcr-2,N) * BENEL(1:mnumcr-2,N)			    ! Bus intercity
	TRQ_ELEC(7,1:mnumcr-2,N) = cltfbtu(N,6,1:mnumcr-2)* BENEL(1:mnumcr-2,N)			        ! Commercial Light trucks (CLT)
	TRQ_ELEC(8,1:mnumcr-2,N) = SUM(BTQFREIRSC(1:3,6,1:mnumcr-2),DIM=1)*TFRBTU_chgsplit(2,n)	! Freight trucks (light, medium, heavy) -- depot/fleet
	TRQ_ELEC(9,1:mnumcr-2,N) = SUM(BTQFREIRSC(1:3,6,1:mnumcr-2),DIM=1)*TFRBTU_chgsplit(1,n) ! Freight trucks (light, medium, heavy) -- non-fleet
	TRQ_ELEC(10,1:mnumcr-2,N)= TRQRAILR(1,1:mnumcr-2,N)							            ! Passenger rail
    
    TRQ_ELEC(1:10,MNUMCR,N) = SUM(TRQ_ELEC(1:10,1:mnumcr-2,N),DIM=2)

!   2/3 Wheelers		
	TTHcons(1,n) = TRQHWY(3,N)                                   ! Motor Gasoline
	TTHcons(2,n) = 0.0                                           ! Diesel
	TTHcons(3,n) = 0.0                                           ! Liquid Petroleum Gas
	TTHcons(4,n) = 0.0                                           ! Electric
	TTHcons(5,n) = 0.0                                           ! Natural Gas	
	TTHcons(6,n) = 0.0                                           ! Hydrogen

!...FTAB Table 47 - LDV energy consumption by LDV type
!   Sums up consumption across all census divisions, for each powertrain type (ildv)
    TRLDQTEK(1:maxldv,N) = 0.0  ! revisit this jma
    
    TRLDQTEK( 1,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,1,n),DIM=2)*BENMG(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,1,n) )
    TRLDQTEK( 2,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,2,n),DIM=2)*BENDS(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,2,n) )
    TRLDQTEK( 3,n) = SUM( (1.0-PCTAF(2,1:mnumcr-2,n))*sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,3,n),DIM=2)*BENMG(mnumcr,n) + &
                          PCTAF(2,1:mnumcr-2,n)*sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,3,n),DIM=2)*BENET(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,3,n) )
    TRLDQTEK( 4,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,4,n),DIM=2)*BENEL(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,7,n) )
    TRLDQTEK( 5,n) = SUM( (1.0-PctPHEV20(n))*sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,5,n),DIM=2)*BENMG(mnumcr,n) + &
					      PctPHEV20(n)*sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,5,n),DIM=2)*BENEL(mnumcr,n) + &
					      FLTLDVBTUT(1:mnumcr-2,5,n) )
    TRLDQTEK( 6,n) = SUM( (1.0-PctPHEV50(n))*sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,6,n),DIM=2)*BENMG(mnumcr,n) + &
                          PctPHEV50(n)*sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,6,n),DIM=2)*BENEL(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,6,n) )
    TRLDQTEK( 7,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,7,n),DIM=2)*BENEL(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,7,n) )
    TRLDQTEK( 8,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,8,n),DIM=2)*BENDS(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,8,n) )
    TRLDQTEK( 9,n) = SUM( (1.0-PCTAF(3,1:mnumcr-2,n))*sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,9,n),DIM=2)*BENMG(mnumcr,n) + &
                          PCTAF(3,1:mnumcr-2,n)*sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,9,n),DIM=2)*BENNG(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,9,n) )
    TRLDQTEK(10,n) = SUM( (1.0-PCTAF(4,1:mnumcr-2,n))*sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,10,n),DIM=2)*BENMG(mnumcr,n) + &
                          PCTAF(4,1:mnumcr-2,n)*sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,10,n),DIM=2)*BENLG(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,10,n) )
    TRLDQTEK(11,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,11,n),DIM=2)*BENNG(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,11,n) )
    TRLDQTEK(12,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,12,n),DIM=2)*BENLG(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,12,n)  )
    TRLDQTEK(13,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,13,n),DIM=2)*BENME(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,13,n) )
    TRLDQTEK(14,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,14,n),DIM=2)*BENHY(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,14,n) )
    TRLDQTEK(15,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,15,n),DIM=2)*BENEL(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,15,n) )
    TRLDQTEK(16,n) = SUM( sum(hhtechbtu(1:mnumcr-2,1:maxvtyp,16,n),DIM=2)*BENMG(mnumcr,n) + &
                          FLTLDVBTUT(1:mnumcr-2,16,n) )

! ... ***** TABLE 52 *******

! ... Fuel efficiency - new conventional cars and light trucks
      TREFFCAR(:,N) = 0.0
      TREFFTRK(:,N) = 0.0
      DO icl=1,maxclass
        DO ildv=1,2
          if(LDVMPG(1,ildv,ICL,YRS).gt.0.0) TREFFCAR(ICL,N)        = TREFFCAR(ICL,N) + TOTALSALSC(1,icl,ildv,n)/LDVMPG(1,ildv,ICL,YRS)
          if(LDVMPG(2,ildv,ICL,YRS).gt.0.0) TREFFTRK(ICL,N)        = TREFFTRK(ICL,N) + TOTALSALSC(2,icl,ildv,n)/LDVMPG(2,ildv,ICL,YRS)
          if(LDVMPG(1,ildv,ICL,YRS).gt.0.0) TREFFCAR(MAXCLASS+1,N) = TREFFCAR(MAXCLASS+1,N) + TOTALSALSC(1,icl,ildv,n)/LDVMPG(1,ildv,ICL,YRS)
          if(LDVMPG(1,ildv,ICL,YRS).gt.0.0) TREFFCAR(MAXCLASS+2,N) = TREFFCAR(MAXCLASS+2,N) + TOTALSALSC(1,icl,ildv,n)/(LDVMPG(1,ildv,ICL,YRS)* degfac(1,ildv,n))
          if(LDVMPG(2,ildv,ICL,YRS).gt.0.0) TREFFTRK(MAXCLASS+1,N) = TREFFTRK(MAXCLASS+1,N) + TOTALSALSC(2,icl,ildv,n)/LDVMPG(2,ildv,ICL,YRS)
          if(LDVMPG(2,ildv,ICL,YRS).gt.0.0) TREFFTRK(MAXCLASS+2,N) = TREFFTRK(MAXCLASS+2,N) + TOTALSALSC(2,icl,ildv,n)/(LDVMPG(2,ildv,ICL,YRS)* degfac(2,ildv,n))
        ENDDO
        if(TREFFCAR(ICL,N).gt.0.0) TREFFCAR(ICL,N) = sum(TOTALSALSC(1,icl,[1,2],n))/TREFFCAR(ICL,N)
        if(TREFFTRK(ICL,N).gt.0.0) TREFFTRK(ICL,N) = sum(TOTALSALSC(2,icl,[1,2],n))/TREFFTRK(ICL,N)
      ENDDO
      
      if(TREFFCAR(MAXCLASS+1,N).gt.0.0) TREFFCAR(MAXCLASS+1,N) = sum(TOTALSALSC(1,:,[1,2],n))/TREFFCAR(MAXCLASS+1,N)
      if(TREFFCAR(MAXCLASS+2,N).gt.0.0) TREFFCAR(MAXCLASS+2,N) = sum(TOTALSALSC(1,:,[1,2],n))/TREFFCAR(MAXCLASS+2,N)
      if(TREFFTRK(MAXCLASS+1,N).gt.0.0) TREFFTRK(MAXCLASS+1,N) = sum(TOTALSALSC(2,:,[1,2],n))/TREFFTRK(MAXCLASS+1,N)
      if(TREFFTRK(MAXCLASS+2,N).gt.0.0) TREFFTRK(MAXCLASS+2,N) = sum(TOTALSALSC(2,:,[1,2],n))/TREFFTRK(MAXCLASS+2,N)

! ... Size Class Sales shares	  
	  !output combined household and fleet class sales shares for all fuel types (aka all vehicles)
	  do icl=1,maxclass
		TRSLSHRC(icl,n) = sum(TOTALSALSC(1,icl,1:maxldv,n))/sum(TOTALSALSC(:,1:maxclass,1:maxldv,n))
		TRSLSHRT(icl,n) = sum(TOTALSALSC(2,icl,1:maxldv,n))/sum(TOTALSALSC(:,1:maxclass,1:maxldv,n))
	  enddo
	  
! ... Horsepower
      do icl=1,maxclass
        do ildv=1,maxldv
          LDVHPW(1,ildv,icl,yrs) = WEIGHTED_MEAN_1D(femhp(1:cargrp,icl,yrs,ildv), &
                                                    cafesales(1:cargrp,icl,yrs,ildv),'LDVHPW(1)')
          LDVHPW(2,ildv,icl,yrs) = WEIGHTED_MEAN_1D(femhp(ltkgrp:maxgroup,icl,yrs,ildv), &
                                                    cafesales(ltkgrp:maxgroup,icl,yrs,ildv),'LDVHPW(2)')
        enddo
      
        TRHPCAR(ICL,N) = WEIGHTED_MEAN_1D(LDVHPW(1,1:maxldv,icl,yrs), &
                                          TOTALSALSC(1,icl,1:maxldv,n), caller_id = 'TRHPCAR')
        TRHPTRK(ICL,N) = WEIGHTED_MEAN_1D(LDVHPW(2,1:maxldv,icl,yrs), &
                                          TOTALSALSC(2,icl,1:maxldv,n), caller_id = 'TRHPCAR')
                                          
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
	  
!...Populate fleet LDV sales (T54 and T48), stocks (T55), VMT (T56) (collapse ifleet and ihav dimensions)
    FLTECHRPT(1:maxvtyp,1:maxldv,n)     = sum(sum(flt_stock(mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,1,1:maxhav,n),DIM=4),DIM=2)/1000.0
    FLTECHSTKRPT(1:maxvtyp,1:maxldv,n)  = sum(sum(FLTECHSTK(mnumcr,1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav),DIM=4),DIM=2)/1000.0
    FLTECHVMTRPT(1:maxvtyp,1:maxldv,n)  = sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,1:maxldv),DIM=2)
    do iregn=1,mnumcr-2
      FLTECHRPT_REG(1:maxvtyp,iregn,1:maxldv,n) = sum(sum(flt_stock(iregn,1:maxvtyp,1:maxfleet,1:maxldv,1,1:maxhav,n),DIM=4),DIM=2)/1000.0
    enddo

! ... ***** TABLE 58 *******
! ... Railroad tmt & efficiency
      TRTMRR(1,N) = sum(BRTMTT(n,1:mnumcr-2))
      TRTMRR(2,N) = 1/FREFF(n)     

! ... Domestic shipping
      TRTMSHIP(1,N) = sum(BSTMTT(n,1:mnumcr-2))
      TRTMSHIP(2,N) = 1.0 / DSEFF(n)

! ... International shipping
      TRIMSHIP(N) = MC_MR(N)                  ! Real imports

!...FTAB Table 48 new ldv sales by ldv type (millions)
!...Calculate total vehicle sales by tech and region  
!...regional ldv sales (collapse ihav and iowner dimensions)
	TRLDSALC(1:maxldv,1:mnumcr-2,n) = TRANSPOSE(sum(sum(LDV_STOCK(1:mnumcr-2,1,1:maxowner,1:maxldv,1,1:maxhav,n),DIM=4),DIM=2))
	TRLDSALT(1:maxldv,1:mnumcr-2,n) = TRANSPOSE(sum(sum(LDV_STOCK(1:mnumcr-2,2,1:maxowner,1:maxldv,1,1:maxhav,n),DIM=4),DIM=2))

!...US ldv sales
    TRLDSALC(1:maxldv,11,n) = sum(TRLDSALC(1:maxldv,1:mnumcr-2,n),DIM=2)
	TRLDSALT(1:maxldv,11,n) = sum(TRLDSALT(1:maxldv,1:mnumcr-2,n),DIM=2)

!...calculate total micro hybrid (ISG, BISG) vehicles sales
	if(curcalyr.eq.1995) trmicros(:,:,:,:) = 0.0
    if(curcalyr.ge.xyr)then
      trmicros(1,1:maxldv,1:mnumcr,n) = trldsalc(1:maxldv,1:mnumcr,n) * &
                                        SPREAD(micropen(1,1:maxldv,n),DIM=2,NCOPIES=mnumcr)
      trmicros(2,1:maxldv,1:mnumcr,n) = trldsalt(1:maxldv,1:mnumcr,n) * &
                                        SPREAD(micropen(2,1:maxldv,n),DIM=2,NCOPIES=mnumcr)
      trmicros(1:2,4:maxldv,1:mnumcr,n) = 0.0 
	endif 

!   Calculate total alternative ldv sales (ANC/T) and total ldv sales (TNC/T) for Table 48
    ANCSALE(1:mnumcr,N) = SUM(TRLDSALC([3:MAXLDV],1:mnumcr,N),DIM=1)
    ANTSALE(1:mnumcr,N) = SUM(TRLDSALT([3:MAXLDV],1:mnumcr,N),DIM=1)
    TNCSALE(1:mnumcr,N) = SUM(TRLDSALC(:,1:mnumcr,N),DIM=1)
    TNTSALE(1:mnumcr,N) = SUM(TRLDSALT(:,1:mnumcr,N),DIM=1)
    
    LEGALTSAL(1,1:mnumcr,N) = 0.0
    WHERE (TNCSALE(1:mnumcr,N) + TNTSALE(1:mnumcr,N) .NE. 0.0)
      LEGALTSAL(1,1:mnumcr,N) = (ANCSALE(1:mnumcr,N) + ANTSALE(1:mnumcr,N)) / &
                               (TNCSALE(1:mnumcr,N) + TNTSALE(1:mnumcr,N))
    END WHERE
    LEGALTSAL(3,1:mnumcr,N) = TOTLEV(1:mnumcr)

!...FTAB Table 49 - LDV stock by ldv type
    TRLDSTKC(1:maxldv,n) = VSTK(1,1:maxldv)
    TRLDSTKT(1:maxldv,n) = VSTK(2,1:maxldv)

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
    
!...Table 50, VMT-weighted stock LDV mpg by powertrain, INCLUDING commercial light trucks {1:gas, 2:dsl, 3:hev, 4:phev, 5:bev, 6:fcv}
    STKMPG(:,n) = 0.0           ! Summed VMT/MPG (also final write var at end)
    TECHMPG_NUM(:) = 0.0        ! Summed VMT

!   Light duty vehicle
    do ildv = 1,maxldv
      impg = mpgmap(ildv)
      if (HHMPGSTK_LDV(ildv,n).gt.0.0) then      ! household
        STKMPG(impg,n) = STKMPG(impg,n) + sum(BVMT_STK_HH(1:2,ILDV,1:maxage,1,1:mnumcr-2))/HHMPGSTK_LDV(ILDV,n)
        TECHMPG_NUM(impg) = TECHMPG_NUM(impg) + sum(BVMT_STK_HH(1:2,ILDV,1:maxage,1,1:mnumcr-2))
      endif
      if (FLTMPGSTK_LDV(ildv,n).gt.0.0) then   ! fleet
        STKMPG(impg,n) = STKMPG(impg,n) + sum(BFLTVMTECH(1:2,:,ildv))/FLTMPGSTK_LDV(ILDV,n)
        TECHMPG_NUM(impg) = TECHMPG_NUM(impg) + sum(BFLTVMTECH(1:2,:,ildv))
      endif
    enddo

!   Commercial light truck
    do ildv = 1,12
      impg = mpgmapCLT(ildv)
      if (CLTMPG(n,ildv).gt.0.0) STKMPG(impg,n) = STKMPG(impg,n) + BCLTVMT(ildv,n)/CLTMPG(n,ildv)
      if (CLTMPG(n,ildv).gt.0.0) TECHMPG_NUM(impg) = TECHMPG_NUM(impg) + BCLTVMT(ildv,n)
    enddo

!   Combined
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
	  FLTNUM1=sum(fltechvmt(mnumcr,1,1:maxfleet,ILDV,1:maxhav))/1000000.0
      FLTNUM2=sum(fltechvmt(mnumcr,2,1:maxfleet,ILDV,1:maxhav))/1000000.0
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
		VMT_STK_TOT(IVTYP,ILDV) = sum(VMT_STK_HH(IVTYP,ILDV,1:maxage,1,mnumcr))+(sum(fltechvmt(mnumcr,IVTYP,1:maxfleet,ILDV,1:maxhav))/1000000.0)
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
      if (TRLDVMT(ILDV,n).lt.0.0) then
        WRITE(21,'(a,2(i4,","),2(f15.0,","))')'TRLDVMT',n+1989,ildv,BVMTECH(ILDV,mnumcr),sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,ILDV))
      endif
    enddo
    
	TRLDVMTE(1,N)  = sum(BVMTECH(1:maxldv,mnumcr)) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,1:maxldv))
    TRLDVMTE(2,N)  = trldvmte(1,n)/SUM(LicDriver(1:AGEGRP,1:MF,11,n))  
    TRLDVMTE(3,N)  = SUM(LicDriver(1:AGEGRP,1:MF,11,n)) 
    TRLDVMTE(4,N)  = PMGTR(11,N)
	TRLDVMTE(5,N)  = MPGHH(N)
    TRLDVMTE(6,N)  = COSTMI(MNUMCR,N)
    TRLDVMTE(7,N)  = SUM(LicDriver(1:AGEGRP,1:MF,1:MNUMCR-2,n))/sum(TMC_NP15A(1:agegrp,1:mf,1:mnumcr-2,n)) 
    TRLDVMTE(8,N)  = MC_YPDR(11,N)/MC_NP16A(11,N)
    TRLDVMTE(9,N)  = IE(N)
    TRLDVMTE(10,N) = sum(licdriver(1:agegrp,2,11,n))/sum(licdriver(1:agegrp,1:mf,11,n)) 
    TRLDVMTE(11,N) = trldvmte(1,n)/sum(vstk(1:maxvtyp,1:maxldv))
    TRLDVMTE(12,N) = 1.0

!   Energy Efficiency Indicators (mpg) (weighted average for LDV and CLT)
	ldvshr       = (tncsale(11,n) + tntsale(11,n)) / &
                   (tncsale(11,n) + tntsale(11,n) + (SUM(cltsalt(1:12,n))*.000001))
	LDV_MPG(1,n) = (ldvshr/cafestd(3,n) + (1-ldvshr)/ncltmpgt(n))**-1  
	LDV_MPG(2,n) = (ldvshr/truempg(3,n) + (1-ldvshr)/ncltmpgt(n))**-1
	ldvshr       = trldvmte(1,n)/(trldvmte(1,n)+sum(bcltvmt(1:12,n)))
	LDV_MPG(3,n) = (ldvshr/trldmpgf(3,n) + (1-ldvshr)/cltmpgt(n))**-1

!   Detailed outputs for buildings team (regional EV consumption)
!    if (n.eq.mnumyr.and.fcrl.eq.1) then
!      WRITE(21,*)'EV charging by residential/commercial, tBtu'
!      do icl=34,mnumyr
!        WRITE(21,'(a,",",i4,",",9(f9.3,","))')'rs_ev',icl+1989,TRQ_ELEC(1,1:mnumcr-2,icl)
!        WRITE(21,'(a,",",i4,",",9(f9.3,","))')'cm_ev',icl+1989,TRQ_ELEC(2,1:mnumcr-2,icl)+TRQ_ELEC(3,1:mnumcr-2,icl)+TRQ_ELEC(4,1:mnumcr-2,icl)+TRQ_ELEC(5,1:mnumcr-2,icl) &
!                                                              +TRQ_ELEC(6,1:mnumcr-2,icl)+TRQ_ELEC(7,1:mnumcr-2,icl)+TRQ_ELEC(8,1:mnumcr-2,icl)+TRQ_ELEC(9,1:mnumcr-2,icl)
!      enddo
!    endif

  RETURN
  END SUBROUTINE TREPORT

! ==========================================================================================================
! ... Subroutine FEMRANGE calculates vehicle range estimates. Ranges are based on adjusted (5-cycle, 
!     on-road fuel economy).
! ==========================================================================================================
    SUBROUTINE FEMRANGE
    USE T_
    IMPLICIT NONE

      RANGE(1:maxgroup,1:maxclass,CURRENT,1:maxldv) = TANKSIZE(1:maxgroup,1:maxclass,CURRENT,1:maxldv) * &
                                                      FE(1:maxgroup,1:maxclass,CURRENT,1:maxldv) * &
                                                      degfacgrp(1:maxgroup,1:maxclass,1:maxldv,n)
      
      RANGE(1:maxgroup,1:maxclass,current,[5,6])    = tanksize(1:maxgroup,1:maxclass,current,[5,6]) * PHEVMPG_S(1:maxgroup,1:maxclass,yrs,[5,6]) * &
                                                      degfacgrp(1:maxgroup,1:maxclass,[5,6],n) + &
                                                      EV_range(1:maxgroup,1:maxclass,[5,6],yrs)
      
      RANGE(1:maxgroup,1:maxclass,CURRENT,[4,7,15]) = EV_range(1:maxgroup,1:maxclass,[4,7,15],yrs)

!     Zero out range values where vehicles don't exist
      WHERE (.not.classflag(1:maxgroup,1:maxclass,1:maxldv))
        RANGE(1:maxgroup,1:maxclass,CURRENT,1:maxldv) = 0.0
      END WHERE

    RETURN
    END SUBROUTINE FEMRANGE

! ==========================================================================================================
! ... This subroutine calculates the base (and historic) year price, weight, fuel economy, and horsepower 
! ... for alternative fuel vehicles that are introduced after the base year (xyr).  Most of these are set relative to the gasoline vehicle values.  Note 
! ... that all attributes are assigned on the basis of and stored in the "current year" elements of each 
! ... parameter, so it is critical that these paremeters be set correctly prior to the AFVADJ call and also 
! ... that the last call is for the actual FEM base year.
! ==========================================================================================================
    SUBROUTINE AFVADJ (ZYR,igp_tmp)
    USE T_ 
    IMPLICIT NONE

    INTEGER     ZYR
    INTEGER     igp_tmp

		  ivtyp = grpmap(igp_tmp)
!...		Calculate BASE values for vehicle introduction year greater than XYR
            IF(ZYR.GT.XYR.and.classflag(igp_tmp,ICL,ILDV)) then
			  if(GRPFLAG(ILDV,ICL,igp_tmp).EQ.ZYR) then  
                WEIGHT(igp_tmp,icl,BASE,ILDV)  = WEIGHT(igp_tmp,icl,CURRENT,GAS) * AFVADJWT(ILDV,ivtyp)
                FE(igp_tmp,icl,BASE,ILDV)      = FE(igp_tmp,icl,CURRENT,GAS)     * AFVADJFE(ILDV,ivtyp)
                HP(igp_tmp,icl,BASE,ILDV)      = HP(igp_tmp,icl,CURRENT,GAS)     * AFVADJHP(ILDV,ivtyp)
                TANKSIZE(igp_tmp,icl,BASE,ILDV)= TANKSIZE(igp_tmp,icl,CURRENT,GAS)
                PRICE(igp_tmp,icl,BASE,ILDV)   = PRICE(igp_tmp,icl,CURRENT,GAS)  + AFVADJPR(ILDV,ivtyp)
                MKT_PEN(igp_tmp,icl,1:NUMTECH,BASE,ILDV) = MKT_PEN(igp_tmp,icl,1:NUMTECH,BASE,GAS)
			  endif
			ENDIF

!...        Calculate CURRENT values for vehicle introduction year
		    if(classflag(igp_tmp,ICL,ILDV)) then
              IF(ZYR.gt.xyr.and.GRPFLAG(ILDV,ICL,igp_tmp).eq.ZYR) then ! introduction year 
                PRICE(igp_tmp,icl,CURRENT,ILDV)    = PRICE(igp_tmp,icl,CURRENT,GAS)  + AFVADJPR(ILDV,ivtyp)
                FE(igp_tmp,icl,CURRENT,ILDV)       = FE(igp_tmp,icl,CURRENT,GAS)     * AFVADJFE(ILDV,ivtyp)
                WEIGHT(igp_tmp,icl,CURRENT,ILDV)   = WEIGHT(igp_tmp,icl,CURRENT,GAS) * AFVADJWT(ILDV,ivtyp)
                HP(igp_tmp,icl,CURRENT,ILDV)       = HP(igp_tmp,icl,CURRENT,GAS)     * AFVADJHP(ILDV,ivtyp)
                TANKSIZE(igp_tmp,icl,CURRENT,ILDV) = TANKSIZE(igp_tmp,icl,CURRENT,GAS)
!...			All electric drive vehicles have an additional battery and fuel cell price, horsepower, and weight adjustments
                if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) then
                  if(ILDV.eq.4.or.ILDV.eq.7.or.ILDV.eq.15) call EVCALC (zyr,igp_tmp)
				  if(ILDV.eq.5.or.ILDV.eq.6) call PHEVCALC (zyr,igp_tmp)
                  if(ILDV.eq.8.or.ILDV.eq.16) call HEVCALC (zyr,igp_tmp)
                  if(ILDV.ge.13.and.ILDV.le.14) call FCCALC (zyr,igp_tmp)
                endif
!... 			Assume base and historic technology penetrations for AFVs are the same as gasoline
                MKT_PEN(igp_tmp,icl,1:NUMTECH,CURRENT,ILDV) = MKT_PEN(igp_tmp,icl,1:NUMTECH,CURRENT,GAS)
                MKT_MAX(ICL,igp_tmp,1:NUMTECH,ILDV)         = MKT_MAX(ICL,igp_tmp,1:NUMTECH,GAS)
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
	REAL :: avg_kwh_rpt(3,maxldv,mnumyr)			 	 !...avg batt kWh per vehicle by car/LT (and total) and PHEV/BEV type
	REAL :: NUM1,NUM2(maxvtyp)							 !...components of avg batt kWh calculation
    REAL :: ldv_sales_current_slice_sum
    
!...Calculate cumulative production (GWh) of li-ion batteries in EVs and PHEVs
!   The first two years LIONCOSTCALC is called, sum up historical GWh (1995 to current)
!   Average kwh capacity calculated for vehicle classes that have capacity and 
!   multiplied by annual sales in each fuel type and vehicle type (car/light truck)
	if(n.le.first_bat_yr)then
	  annual_gwh(:) 	= 0.0
      global_batt_prod(1:2,1,:) = 0.0     ! LDV/truck, US
	  do ic = 6,n
        do igp = 1, maxgroup
          do icl = 1, maxclass
            do ILDV=1,maxldv
              if (BatPackSize(ic+1989-1,ICL,IGP,ILDV).eq.0.0) CYCLE
	  		  if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.ge.15) &
                annual_gwh(ic) = annual_gwh(ic) + ldv_sales(igp,icl,ildv,mnumcr,ic-1)*BatPackSize(ic+1989-1,ICL,IGP,ILDV)
!	  	      WRITE(21,'(a,",",5(i4,","),3(f12.3,","))')'lioncost_debug1',curitr,ic+1989,igp,icl,ildv,ldv_sales(igp,icl,ildv,mnumcr,ic-1),BatPackSize(ic+1989-1,ICL,IGP,ILDV),annual_gwh(ic)
            enddo ! maxldv
	  	  enddo
	    enddo
        global_batt_prod(1,1,ic) = global_batt_prod(1,1,ic-1) + annual_gwh(ic)
      enddo ! yr
!   In subsequent years, calculate previous year GWh and add to cumulative total to estimate current year cost
!   Average KWh capacity for each fuel type, class, and vehicle type (Car/light truck) multiplied by sales
!   for each fuel type, class, and vehicle type.
	else
	  annual_gwh(n) = 0.0
      do ildv=1,maxldv
	    do igp=1,maxgroup
	  	  do icl=1,maxclass
	  	    if (BatPackSize(n+1989-1,ICL,IGP,ILDV).eq.0.0) CYCLE
            if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.ge.15) &
              annual_gwh(n) = annual_gwh(n) + ldv_sales(igp,icl,ildv,mnumcr,n-1)*BatPackSize(n+1989-1,ICL,IGP,ILDV)
!	  	      WRITE(21,'(a,",",6(i4,","),3(f12.3,","))')'lioncost_debug2',curcalyr,curitr,n,igp,icl,ildv,ldv_sales(igp,icl,ildv,mnumcr,n-1)/1000,BatPackSize(n+1989-1,ICL,IGP,ILDV),annual_gwh(n)
          enddo ! maxldv
	    enddo ! maxclass
	  enddo ! igp
      global_batt_prod(1,1,n) = global_batt_prod(1,1,n-1) + annual_gwh(n)
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
            pack_a(ILDV) = (li_ion_cost(ILDV,yrs)-mat_a(ILDV)*mat_markup(n)*(sum(global_batt_prod(:,:,n-1)))**(-(-LOG(1.0)/LOG(2.0))))/(sum(global_batt_prod(:,:,n-1))**(-(-LOG(1.0-0.165)/LOG(2.0))))	
	  	  elseif(n.eq.first_bat_yr+1) then		! Assume standard learning rate (16.5%) through first projection year (year that we calibrate sales to Ward's based on YTD values)
	  	    li_ion_cost(ILDV,yrs) = pack_a(ILDV)*(sum(global_batt_prod(:,:,n)))**(-(-LOG(1.0-0.165)/LOG(2.0))) + mat_a(ILDV)*mat_markup(n)*(sum(global_batt_prod(:,:,n)))**(-(-LOG(1.0)/LOG(2.0)))		
	      else								! If past the first projection year, use the user-defined learning rate (pack_b)
	      	if(n.eq.first_bat_yr+2) then		! Align curve coefficient -- pack_a -- so that the user-defined pack_b produces the same cost in first_bat_yr+2
	      	  pack_a(ILDV) = (li_ion_cost(ILDV,yrs-1)-mat_a(ILDV)*mat_markup(n)*(sum(global_batt_prod(:,:,n-1)))**(-mat_b(ILDV)))/(sum(global_batt_prod(:,:,n-1))**(-pack_b(ILDV)))
	      	endif
	      	li_ion_cost(ILDV,yrs) = pack_a(ILDV)*(sum(global_batt_prod(:,:,n)))**(-pack_b(ILDV)) + mat_a(ILDV)*mat_markup(n)*(sum(global_batt_prod(:,:,n)))**(-mat_b(ILDV))
	      endif
	    endif
	  enddo
    endif

!	Write out battery production, costs, and capacity per vehicle
	if(n.eq.MNUMYR.and.FCRL.eq.1)then
		write(21,*) "LIONCOSTCALC_debug_2020USD"
		write(21,'(a5,",",9(a12,","))') 'year','ann_gwh','cumulgwh_ldv','cumulgwh_oth','ev100','phev20','phev50','ev200','ev300','hev'
		do ic=6,n
			write(21, '(I5,", ", 9(F12.1,", "))') ic+1989, annual_gwh(ic), global_batt_prod(1,1,ic), sum(global_batt_prod(2:6,1:2,ic)), li_ion_cost([4,5,6,7,15,16],ic+1989)/ MC_JPGDP(1) * MC_JPGDP(31)    
		enddo
        
		WRITE(21,*) "Global Battery Demand (GWh)"
        WRITE(21,'(a5,5(",",a12))')'year','ldv_us','oth_tdm_us','ldv_nus','oth_tdm_nus','cons_elec'
        do ic=6,n
			write(21, '(I5,", ", 5(F12.3,", "))') ic+1989, global_batt_prod(1,1,ic), global_batt_prod(2,1,ic), global_batt_prod(1,2,ic), global_batt_prod(2,2,ic),global_batt_prod(6,2,ic)
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
		          ldv_sales_current_slice_sum = sum(ldv_sales(igp,icl,ildv,1:mnumcr-2,ic))
                  NUM2(it) = NUM2(it) + ldv_sales_current_slice_sum*BatPackSize(ic+1989,ICL,IGP,ILDV)   !avg_kwh(it,ILDV,ICL,ic+1989)*TOTALSALSC(it,ICL,ILDV,ic)
				  NUM1     = NUM1     + ldv_sales_current_slice_sum*BatPackSize(ic+1989,ICL,IGP,ILDV)   !avg_kwh(it,ILDV,ICL,ic+1989)*TOTALSALSC(it,ICL,ILDV,ic)
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
  SUBROUTINE HEVCALC (dsyrs,igp_tmp)
  USE T_
  IMPLICIT NONE

!...Local variable definitions
	integer       dsyrs
    REAL, PARAMETER :: NiMHweight_per_kWh = 53.42       		! NiMH weight (lbs) per kWh battery capacity
    REAL, PARAMETER :: NiMHkWh_per_pound  = 0.0004989   		! Battery sizing factor (kWh) based on vehicle weight
    REAL			:: Lii_kWhr(MAXCLASS,MAXGROUP,MAXLDV)		! HEV lithium ion battery pack size
	REAL			:: Nmh_KWhr(MAXCLASS,MAXGROUP,MAXLDV)       ! HEV nickel metal hydride battery pack size
    INTEGER       igp_tmp
	
!...Calculate required battery size based on vehicle weight and battery type
!   Incorporate improvements in DOD (LIONkWh_perlb was estimated based on pack sizing in EPALYR)
    lii_kWhr(ICL,igp_tmp,ILDV) = weight(igp_tmp,ICL,current,gas) * LIONkWh_perLb(ICL,igp_tmp,ILDV) * (phev_dod(epalyr)/phev_dod(dsyrs))
    nmh_kWhr(ICL,igp_tmp,ILDV) = weight(igp_tmp,ICL,current,gas) * NiMHkWh_per_pound

    if(lii_kWhr(ICL,igp_tmp,ILDV)*Li_ion_Cost(ILDV,dsyrs).lt.nmh_kWhr(ICL,igp_tmp,ILDV)*nimh_cost(dsyrs)) then
	  BatPackSize(dsyrs,icl,igp_tmp,ildv) = lii_kWhr(ICL,igp_tmp,ILDV)
      ElecSysIncCost(ICL,igp_tmp,current,ILDV) = lii_kWhr(ICL,igp_tmp,ILDV)*Li_ion_Cost(ILDV,dsyrs) + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)
	  BatPackWgt(dsyrs,ICL,igp_tmp,ILDV) = LION_LB_perkWh(ILDV) * lii_kWhr(ICL,igp_tmp,ILDV)
	else
	  BatPackSize(dsyrs,icl,igp_tmp,ildv) = nmh_kWhr(ICL,igp_tmp,ILDV)
      ElecSysIncCost(ICL,igp_tmp,current,ILDV) = nmh_kWhr(ICL,igp_tmp,ILDV)*nimh_cost(dsyrs) + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)
	  BatPackWgt(dsyrs,ICL,igp_tmp,ILDV) = NiMHweight_per_kWh * nmh_kWhr(ICL,igp_tmp,ILDV) 
	endif

    if (ElecSysIncCost(ICL,igp_tmp,prev,ILDV).eq.0.0) ElecSysIncCost(ICL,igp_tmp,prev,ILDV) = ElecSysIncCost(ICL,igp_tmp,current,ILDV)
	
!...Calculate hybrid electric vehicle price
!   Note that because of the way this works (building off previous years' prices)
!   we cannot use CALIB_NHTSA to calibrate price every year. If this methodology changes,
!   the calibratio_pri in CALIB_NHTSA must be re-applied (currently backed out).
    PRICE(igp_tmp,ICL,CURRENT,ILDV) = PRICE(igp_tmp,ICL,CURRENT,ILDV) + ElecSysIncCost(ICL,igp_tmp,CURRENT,ILDV)

!    WRITE(21,'(a,",",5(i4,","),4(f12.2,","))')'hev_price',curcalyr,curitr,igp_tmp,icl,ildv,PRICE(igp_tmp,ICL,current,ildv),PRICE(igp_tmp,ICL,prev,ildv),ElecSysIncCost(icl,igp_tmp,current,ildv),&
!                                                ElecNonBattCst(icl,dsyrs,ivtyp,ildv)


!...Calculate battery electric vehicle weight
	if(weight(igp_tmp,ICL,prev,ildv).eq.0.0) WEIGHT(igp_tmp,ICL,current,ildv) = weight(igp_tmp,ICL,current,gas)+BatPackWgt(dsyrs,icl,igp_tmp,ildv)

!...Assume HEVs have equivalent performance to conventional gasoline vehicle
    if(weight(igp_tmp,ICL,prev,ildv).eq.0.0) HP(igp_tmp,ICL,current,ildv) = HP(igp_tmp,ICL,current,gas) * (weight(igp_tmp,ICL,current,ildv)/weight(igp_tmp,ICL,current,gas))
	
!...fuel economy for vehicles introduced in the projection
!   hard coded values are rough estimates of fuel economy improvement over non-HEV gasoline;
!   note these are calibrated to actual based on historical data (using CALRATIO_FE from CALIBNHTSA)
	if(fe(igp_tmp,icl,prev,ildv).eq.0.0.or.(grpflag(ildv,icl,igp_tmp).eq.curcalyr.and.fempri(igp_tmp,icl,epalyr,ildv).eq.0.0)) then 
	  if(igp_tmp.le.cargrp) fe(igp_tmp,icl,current,ildv) = fe(igp_tmp,icl,current,gas) * 1.646
	  if(igp_tmp.ge.ltkgrp) fe(igp_tmp,icl,current,ildv) = fe(igp_tmp,icl,current,gas) * 1.45
      if(curcalyr.gt.EPALYR)fe(igp_tmp,icl,current,ildv) = fe(igp_tmp,icl,current,ildv) * CALRATIO_FE(igp_tmp,icl,ildv)
    endif

!...tank size for vehicles introduced in the projection
!   hard coded value is rough estimate of tanksize decrease v. equivalent non-HEV gasoline vehicle;
!   note these are calibrated to actual based on historical data (using CALRATIO_TSZ from CALIBNHTSA)
	if(tanksize(igp_tmp,icl,prev,ildv).eq.0.0) then
      tanksize(igp_tmp,icl,current,ildv) = tanksize(igp_tmp,icl,current,gas) * 0.90
      if(curcalyr.gt.EPALYR)tanksize(igp_tmp,icl,current,ildv) = tanksize(igp_tmp,icl,current,ildv) * CALRATIO_TSZ(igp_tmp,icl,ildv)
    endif

!...fill base and previous values
	if(grpflag(ildv,icl,igp_tmp).eq.curcalyr.and.fempri(igp_tmp,icl,epalyr,ildv).eq.0.0) then
      weight(igp_tmp,icl,base:prev,ildv) = weight(igp_tmp,icl,current,ildv)
	  fe(igp_tmp,icl,base:prev,ildv) = fe(igp_tmp,icl,current,ildv)
	  hp(igp_tmp,icl,base:prev,ildv) = hp(igp_tmp,icl,current,ildv)
	  tanksize(igp_tmp,icl,base:prev,ildv) = tanksize(igp_tmp,icl,current,ildv)
	  price(igp_tmp,icl,base:prev,ildv) = price(igp_tmp,icl,current,ildv)
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
  SUBROUTINE PHEVCALC (dsyrs,igp_tmp)
  USE T_
  IMPLICIT NONE
  
!...local variable definitions
	integer       dsyrs							
    INTEGER       igp_tmp

!...Calculate required battery size based on vehicle weight and depth of discharge improvement
!   If the vehicle existed last year -- shrink the pack by the improvement in DOD
    if(batpacksize(dsyrs-1,icl,igp_tmp,ildv).gt.0.0) then 
	  BatPackSize(dsyrs,icl,igp_tmp,ildv) = batpacksize(dsyrs-1,icl,igp_tmp,ildv) * (phev_dod(dsyrs-1)/phev_dod(dsyrs))
!   If the vehicle didn't exist last year -- estimate using kWh/lb parameter (which was developed from epalyr data, so need to adjust by improvements in DOD since then)
	else
	  BatPackSize(dsyrs,icl,igp_tmp,ildv) = weight(igp_tmp,ICL,current,gas) * LIONkWh_perLb(ICL,igp_tmp,ILDV) * (phev_dod(epalyr)/phev_dod(dsyrs))
	endif

    IF (BatPackSize(dsyrs,icl,igp_tmp,ildv).le.0.0.or.BatPackSize(dsyrs,icl,igp_tmp,ildv).ne.BatPackSize(dsyrs,icl,igp_tmp,ildv)) THEN
      WRITE(21,'(a,",",5(i4,","),4(f9.2,","))')'ERROR: PHEV pack size',curitr,curcalyr,igp_tmp,icl,ildv,FEM_PASS,&
                                                BatPackSize(dsyrs,icl,igp_tmp,ildv),LIONkWh_perLb(ICL,igp_tmp,ILDV),phev_dod(epalyr),weight(igp_tmp,ICL,current,gas)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF

!...Calculate total battery electric vehicle incremental cost
	ElecSysIncCost(icl,igp_tmp,current,ildv) = (BatPackSize(dsyrs,icl,igp_tmp,ildv)*Li_ion_Cost(ildv,dsyrs)) + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)
    if (ElecSysIncCost(ICL,igp_tmp,prev,ILDV).eq.0.0) ElecSysIncCost(ICL,igp_tmp,prev,ILDV) = ElecSysIncCost(ICL,igp_tmp,current,ILDV)

!...Calculate plug-in hybrid electric vehicle price
!   Note that because of the way this works (building off previous years' prices)
!   we cannot use CALIB_NHTSA to calibrate price every year. If this methodology changes,
!   the calibratio_pri in CALIB_NHTSA must be re-applied (currently backed out).
    PRICE(igp_tmp,ICL,current,ildv) = PRICE(igp_tmp,ICL,current,ildv) + ElecSysIncCost(icl,igp_tmp,current,ildv)

!    WRITE(21,'(a,",",5(i4,","),4(f12.2,","))')'phev_price',curcalyr,curitr,igp_tmp,icl,ildv,PRICE(igp_tmp,ICL,current,ildv),PRICE(igp_tmp,ICL,prev,ildv),ElecSysIncCost(icl,igp_tmp,current,ildv),&
!                                        ElecNonBattCst(icl,dsyrs,ivtyp,ildv)

!...Calculate battery electric vehicle weight
	if(weight(igp_tmp,ICL,prev,ildv).ne.0.0) then 
	  weight(igp_tmp,ICL,current,ildv) = weight(igp_tmp,ICL,prev,ildv)
	else
      weight(igp_tmp,ICL,current,ildv) = weight(igp_tmp,ICL,current,ildv)+(BatPackSize(dsyrs,icl,igp_tmp,ildv)*LION_LB_perkWh(ildv))  
	endif
	
!...Assume PHEVs have equivalent performance (HP/weight ratio) to conventional gasoline vehicle
	if(hp(igp_tmp,ICL,prev,ildv).eq.0.0) then
      if(weight(igp_tmp,ICL,current,gas).ne.0.0) HP(igp_tmp,ICL,current,ildv) = HP(igp_tmp,ICL,current,gas) * (weight(igp_tmp,ICL,current,ildv)/weight(igp_tmp,ICL,current,gas)) 
	endif

!...Estimate Electric range
!   Coefficients are estimated based on EV ranges adjusted for degradation
    if(BatPackSize(dsyrs-1,icl,igp_tmp,ildv).gt.0.0) then 
      EV_range(igp_tmp,icl,ildv,dsyrs) = EV_range(igp_tmp,icl,ildv,dsyrs-1)
    else
	  EV_range(igp_tmp,icl,ildv,dsyrs) = EV_range_m(ildv) * (BatPackSize(dsyrs,icl,igp_tmp,ildv)) + EV_range_b(ildv)
	endif

    IF (EV_range(igp_tmp,icl,ildv,dsyrs).le.0.0.or.EV_range(igp_tmp,icl,ildv,dsyrs).ne.EV_range(igp_tmp,icl,ildv,dsyrs)) THEN
      WRITE(21,'(a,",",5(i4,","),7(f9.2,","))')'ERROR: PHEV range',curitr,curcalyr,igp_tmp,icl,ildv,FEM_PASS,EV_range(igp_tmp,icl,ildv,dsyrs),RANGE(igp_tmp,ICL,prev,ildv),EV_range_m(ildv),EV_range_b(ildv)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF

!...Estimate charge-depleting fuel economy    
	FE(igp_tmp,icl,current,ildv) = (EV_range(igp_tmp,icl,ildv,dsyrs)/(BatPackSize(dsyrs,icl,igp_tmp,ildv)*phev_dod(dsyrs))) * & 
                                   (CFMGQ(n)*1000000.0/42.0*(1.0/3412.0)) * evmpg_adj(igp_tmp,icl,ildv)
    
    PHEVMPG_D(igp_tmp,icl,yrs,ildv) = FE(igp_tmp,icl,current,ildv)
!...Estimate charge-sustaining fuel economy    
    if(ildv.eq.5) then
      if (FE(igp_tmp,icl,current,gas).gt.0.0) then
        PHEVMPG_S(igp_tmp,icl,yrs,ildv) = FE(igp_tmp,icl,current,gas)*csratio(icl,igp_tmp,1)
      else
        PHEVMPG_S(igp_tmp,icl,yrs,ildv) = (FE(igp_tmp,icl,current,ILDV)/2.464)
      endif
    elseif(ildv.eq.6) then
      if (FE(igp_tmp,icl,current,gas).gt.0.0) then
        PHEVMPG_S(igp_tmp,icl,yrs,ildv) = FE(igp_tmp,icl,current,gas)*csratio(icl,igp_tmp,2)
      else
        PHEVMPG_S(igp_tmp,icl,yrs,ildv) = (FE(igp_tmp,icl,current,ILDV)/1.7)
      endif
    endif

!   Calibrate projected mpg based on historical data (using CALRATIO_FE from CALIBNHTSA)
    if(curcalyr.gt.EPALYR) then
      PHEVMPG_S(igp_tmp,icl,yrs,ildv) = PHEVMPG_S(igp_tmp,icl,yrs,ildv) * CALRATIO_FE(igp_tmp,icl,ildv)
      PHEVMPG_D(igp_tmp,icl,yrs,ildv) = PHEVMPG_D(igp_tmp,icl,yrs,ildv) * CALRATIO_FE(igp_tmp,icl,ildv)
    endif
    
    FE(igp_tmp,icl,current,ildv) = 1.0/(phev_evmt(igp_tmp,icl,dsyrs,ildv)/PHEVMPG_D(igp_tmp,icl,yrs,ildv) + &          ! charge depleting mpg
								     (1.0-phev_evmt(igp_tmp,icl,dsyrs,ildv))/PHEVMPG_S(igp_tmp,icl,yrs,ildv))          ! charge sustaining mpg

    IF (FE(igp_tmp,icl,current,ildv).le.0.0.or.FE(igp_tmp,icl,current,ildv).ne.FE(igp_tmp,icl,current,ildv)) THEN
      WRITE(21,'(a,",",6(a4,","),7(a9,","))')'ERROR: PHEV mpg','itr','year','igp','icl','ildv','pass','mpg','batsize','evmpg_adj','phev_dod','phev_evmt','csrat1','csrat2'
      WRITE(21,'(a,",",6(i4,","),7(f9.2,","))')'ERROR: PHEV mpg',curitr,curcalyr,igp_tmp,icl,ildv,FEM_PASS,FE(igp_tmp,icl,current,ildv),BatPackSize(dsyrs,icl,igp_tmp,ildv),evmpg_adj(igp_tmp,icl,ildv),phev_dod(dsyrs),&
                                                                                                   phev_evmt(igp_tmp,icl,dsyrs,ildv),csratio(icl,igp_tmp,:)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF

!...calculate tanksize	
!   hard coded value is rough estimate of tanksize decrease v. equivalent non-HEV gasoline vehicle;
!   note these are calibrated to actual based on historical data immediately after (using CALRATIO_TSZ from CALIBNHTSA)
	if(tanksize(igp_tmp,icl,prev,ildv).eq.0.0) then
      if (ildv.eq.5) tanksize(igp_tmp,icl,current,ildv) = tanksize(igp_tmp,icl,current,gas) * 0.90
      if (ildv.eq.6) tanksize(igp_tmp,icl,current,ildv) = tanksize(igp_tmp,icl,current,gas) * 0.85
      if (curcalyr.gt.EPALYR) tanksize(igp_tmp,icl,current,ildv) = tanksize(igp_tmp,icl,current,ildv) * CALRATIO_TSZ(igp_tmp,icl,ildv)
	endif
    
!...fill FEMCALC base year values for grpflag year	
	if(grpflag(ildv,icl,igp_tmp).eq.curcalyr.and.fempri(igp_tmp,icl,epalyr,ildv).eq.0.0) then
!...  fill base and previous values
	  weight(igp_tmp,icl,base:prev,ildv) = weight(igp_tmp,icl,current,ildv)
	  fe(igp_tmp,icl,base:prev,ildv) = fe(igp_tmp,icl,current,ildv)
	  hp(igp_tmp,icl,base:prev,ildv) = hp(igp_tmp,icl,current,ildv)
	  tanksize(igp_tmp,icl,base:prev,ildv) = tanksize(igp_tmp,icl,current,ildv)
	  price(igp_tmp,icl,base:prev,ildv) = price(igp_tmp,icl,current,ildv)
	endif
	
  RETURN
  END SUBROUTINE PHEVCALC

! ==========================================================================================================
!...Subroutine EVCALC
!   Description:
!      Calculates electric vehicle (100,200, & 300 mile range) battery size, price, weight, and horsepower
! ==========================================================================================================
  SUBROUTINE EVCALC (dsyrs,igp_tmp)
  USE T_
  IMPLICIT NONE

!...local variable definitions
	integer       dsyrs
    INTEGER       igp_tmp

!...Calculate required battery size based on vehicle weight and depth of discharge improvement
!   Battery size requirement stays constant -- DOD improvements go to increasing range
	if(batpacksize(dsyrs-1,icl,igp_tmp,ildv).gt.0.0) then 
      BatPackSize(dsyrs,icl,igp_tmp,ildv) = batpacksize(dsyrs-1,icl,igp_tmp,ildv) 
	else
      BatPackSize(dsyrs,icl,igp_tmp,ildv) = weight(igp_tmp,ICL,current,gas) * LIONkWh_perLb(ICL,igp_tmp,ILDV)
	endif

    IF (BatPackSize(dsyrs,icl,igp_tmp,ildv).le.0.0.or.BatPackSize(dsyrs,icl,igp_tmp,ildv).ne.BatPackSize(dsyrs,icl,igp_tmp,ildv)) THEN
      WRITE(21,'(a,",",6(i4,","),4(f9.2,","))')'ERROR: BEV pack size',curitr,curcalyr,igp_tmp,icl,ildv,FEM_PASS,&
                                                BatPackSize(dsyrs,icl,igp_tmp,ildv),LIONkWh_perLb(ICL,igp_tmp,ILDV),ev_dod(epalyr),weight(igp_tmp,ICL,current,gas)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF

!...Calculate total battery electric vehicle incremental cost
    ElecSysIncCost(ICL,igp_tmp,current,ILDV) = BatPackSize(dsyrs,ICL,igp_tmp,ILDV) * Li_ion_Cost(ILDV,dsyrs) + &
										   + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)

    if (ElecSysIncCost(ICL,igp_tmp,prev,ILDV).eq.0.0) ElecSysIncCost(ICL,igp_tmp,prev,ILDV) = ElecSysIncCost(ICL,igp_tmp,current,ILDV)

!...Calculate battery electric vehicle price
!   Note that because of the way this works (building off previous years' prices)
!   we cannot use CALIB_NHTSA to calibrate price every year. If this methodology changes,
!   the calibratio_pri in CALIB_NHTSA must be re-applied (currently backed out).
    PRICE(igp_tmp,ICL,CURRENT,ILDV) = PRICE(igp_tmp,ICL,CURRENT,ILDV) + ElecSysIncCost(ICL,igp_tmp,CURRENT,ILDV)
    
!...Calculate average battery weight (lbs) per kWh
	BatPackWgt(dsyrs,icl,igp_tmp,ildv) = BatPackSize(dsyrs,icl,igp_tmp,ildv) * LION_LB_perkWh(ildv)

!...Calculate vehicle weight based on battery size
	if(weight(igp_tmp,ICL,prev,ildv).eq.0.0) WEIGHT(igp_tmp,ICL,current,ildv) = WEIGHT(igp_tmp,ICL,current,gas)-500.0 + BatPackWgt(dsyrs,icl,igp_tmp,ildv)

!...Assume EVs have equivalent performance (HP/weight ratio) to conventional gasoline vehicle
	if(hp(igp_tmp,ICL,prev,ildv).eq.0.0) then
      if(weight(igp_tmp,ICL,current,gas).ne.0.0) HP(igp_tmp,ICL,current,ildv) = HP(igp_tmp,ICL,current,gas) * (weight(igp_tmp,ICL,current,ildv)/weight(igp_tmp,ICL,current,gas)) 
	endif

!...Calculate vehicle range based on usable battery size
!   As noted in the pack size calcs above, DOD improvements go to increasing range.
!   Coefficients are estimated based on EV ranges adjusted for degradation
    if(BatPackSize(dsyrs-1,icl,igp_tmp,ildv).gt.0.0) then 
      EV_range(igp_tmp,icl,ildv,dsyrs) = EV_range(igp_tmp,icl,ildv,dsyrs-1) * (ev_dod(dsyrs)/ev_dod(dsyrs-1))
    else
      EV_range(igp_tmp,icl,ildv,dsyrs) = (EV_range_m(ildv) * BatPackSize(dsyrs,icl,igp_tmp,ildv) + EV_range_b(ildv)) * (ev_dod(dsyrs)/ev_dod(epalyr))
    endif

    IF (EV_range(igp_tmp,icl,ildv,dsyrs).le.0.0.or.EV_range(igp_tmp,icl,ildv,dsyrs).ne.EV_range(igp_tmp,icl,ildv,dsyrs)) THEN
      WRITE(21,'(a,",",6(i4,","),4(f9.2,","))')'ERROR: BEV range',curitr,curcalyr,igp_tmp,icl,ildv,FEM_PASS,EV_range(igp_tmp,icl,ildv,dsyrs),RANGE(igp_tmp,ICL,prev,ildv),EV_range_m(ildv),EV_range_b(ildv)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF
	
!...Calculate EV fuel economy equivalency based on range and battery size.
	FE(igp_tmp,icl,current,ildv) = (EV_range(igp_tmp,icl,ildv,dsyrs)/(BatPackSize(dsyrs,icl,igp_tmp,ildv)*EV_DOD(dsyrs))* &
	                            CFMGQ(n)*1000000.0/42.0*(1.0/3412.0)) * evmpg_adj(igp_tmp,icl,ildv)

!   Calibrate projected mpg based on historical data (using CALRATIO_FE from CALIBNHTSA)
    if(curcalyr.gt.EPALYR) then
      FE(igp_tmp,icl,current,ildv) = FE(igp_tmp,icl,current,ildv) * CALRATIO_FE(igp_tmp,icl,ildv)
    endif
    
    IF (FE(igp_tmp,icl,current,ildv).le.0.0.or.FE(igp_tmp,icl,current,ildv).ne.FE(igp_tmp,icl,current,ildv)) THEN
      WRITE(21,'(a,",",6(a4,","),4(a9,","))')'ERROR: BEV mpg','itr','year','igp','icl','ildv','pass','mpg','batsize','evmpg_adj','phev_dod'
      WRITE(21,'(a,",",6(i4,","),4(f9.2,","))')'ERROR: BEV mpg',curitr,curcalyr,igp_tmp,icl,ildv,FEM_PASS,FE(igp_tmp,icl,current,ildv),BatPackSize(dsyrs,icl,igp_tmp,ildv),evmpg_adj(igp_tmp,icl,ildv),EV_DOD(dsyrs)
      WRITE(*,*)'ERROR: TDM. See p1/TRNOUT.txt'
      STOP
    ENDIF

!...fill FEMCALC base year values for grpflag year	
	if(grpflag(ildv,icl,igp_tmp).eq.curcalyr.and.fempri(igp_tmp,icl,epalyr,ildv).eq.0.0) then
!...  fill base and previous values
	  weight(igp_tmp,icl,base:prev,ildv) = weight(igp_tmp,icl,current,ildv)
	  fe(igp_tmp,icl,base:prev,ildv) = fe(igp_tmp,icl,current,ildv)
	  hp(igp_tmp,icl,base:prev,ildv) = hp(igp_tmp,icl,current,ildv)
	  tanksize(igp_tmp,icl,base:prev,ildv) = tanksize(igp_tmp,icl,current,ildv)
	  price(igp_tmp,icl,base:prev,ildv) = price(igp_tmp,icl,current,ildv)
	endif

    RETURN
    END SUBROUTINE EVCALC

! ==========================================================================================================
! ... Subroutine FCCALC calculates battery costs and related quantities for fuel cell vehicles
! ==========================================================================================================
  SUBROUTINE FCCALC (dsyrs,igp_tmp)
  USE T_
  IMPLICIT NONE

    REAL          BATTERY_WT,BATTERY_POWER
    REAL          TANKCOST(13:14)  /     0.0,  3500.0 /
    REAL          GALPERMILE(13:14)/ 0.00625, 0.00570 /  
	integer       dsyrs
    INTEGER       igp_tmp
	
!...Calculate base fuel cell cost based on a need of 0.028 kW per vehicle pound and input
!...fuel cell costs in $/kW.
    FUELCELL(ICL,igp_tmp,CURRENT,ILDV) = WEIGHT(igp_tmp,icl,CURRENT,GAS) * 0.028 * FuelCell_D_kW(dsYRS,ILDV)

!...Calculate battery requirements for FCV 
    if(weight(igp_tmp,icl,current,gas).gt.0.0) BatPackSize(dsyrs,ICL,igp_tmp,ILDV) = weight(igp_tmp,icl,current,gas) * LIONkWh_perLb(ICL,igp_tmp,ILDV)

!...Calculate the total fuel cell, battery and hydrogen storage cost
    ElecSysIncCost(ICL,igp_tmp,CURRENT,ILDV) = (FUELCELL(ICL,igp_tmp,CURRENT,ILDV) + &
										BatPackSize(dsyrs,ICL,igp_tmp,ILDV) * Li_ion_Cost(ILDV,dsyrs)  + &
                                        TANKCOST(ILDV))

!...Calculate fuel cell electric vehicle price
!   Note that because of the way this works (building off previous years' prices)
!   we cannot use CALIB_NHTSA to calibrate price every year. If this methodology changes,
!   the calibratio_pri in CALIB_NHTSA must be re-applied (currently backed out).
    PRICE(igp_tmp,icl,CURRENT,ILDV) = PRICE(igp_tmp,icl,CURRENT,ILDV) + ElecSysIncCost(ICL,igp_tmp,CURRENT,ILDV)
	
!...Estimate fuel cell vehicle fuel economy using estimates of gallons per mile per
!...1000 pounds of vehicle weight.
	if(fe(igp_tmp,icl,prev,ildv).eq.0.0) FE(igp_tmp,icl,CURRENT,ILDV) = 1 / (GALPERMILE(ILDV) * (WEIGHT(igp_tmp,icl,CURRENT,GAS)/1000.0))
    if(curcalyr.gt.EPALYR) FE(igp_tmp,icl,CURRENT,ILDV) = FE(igp_tmp,icl,CURRENT,ILDV) * CALRATIO_FE(igp_tmp,icl,ildv)
    
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
    CHARACTER*18  FNAME
    INTEGER       WKUNIT
    INTEGER     m2, y2, c2, g2, v2
    INTEGER, PARAMETER      :: lyr_hist = 2023-1989     ! Last year of data in trnfem.csv

!   Parameters for pre-EPALYR sales and attribute data (trnfem) 
    INTEGER, PARAMETER      :: NUM_ROWS = 3328       ! Must set this manually based on inputs
    integer, PARAMETER      :: NUM_IND_COLS = 4
    INTEGER, PARAMETER      :: NUM_DATA_COLS = 13    ! Must set this manually based on inputs
    INTEGER, PARAMETER      :: TOTAL_COLS = NUM_IND_COLS + NUM_DATA_COLS
    INTEGER, PARAMETER      :: MAX_LINE_BUFFER_LENGTH = 20 * TOTAL_COLS 

    INTEGER, DIMENSION(NUM_ROWS) :: col1_year
    INTEGER, DIMENSION(NUM_ROWS) :: col2_icl
    INTEGER, DIMENSION(NUM_ROWS) :: col3_igp
    INTEGER, DIMENSION(NUM_ROWS) :: col4_ildv
    
    REAL, DIMENSION(NUM_ROWS) :: col1_price
    REAL, DIMENSION(NUM_ROWS) :: col2_mpgtst
    REAL, DIMENSION(NUM_ROWS) :: col3_mpgadj
    REAL, DIMENSION(NUM_ROWS) :: col4_mpgcomp
    REAL, DIMENSION(NUM_ROWS) :: col5_hp
    REAL, DIMENSION(NUM_ROWS) :: col6_tanksz
    REAL, DIMENSION(NUM_ROWS) :: col7_evrange
    REAL, DIMENSION(NUM_ROWS) :: col8_range
    REAL, DIMENSION(NUM_ROWS) :: col9_curbwgt
    REAL, DIMENSION(NUM_ROWS) :: col10_battkwh
    REAL, DIMENSION(NUM_ROWS) :: col11_ftprnt
    REAL, DIMENSION(NUM_ROWS) :: col12_phevevmt
    REAL, DIMENSION(NUM_ROWS) :: col13_sales

!   Parameters for csv read-in    
    CHARACTER(LEN=MAX_LINE_BUFFER_LENGTH) :: line_buffer
    INTEGER :: status, current_row_idx

!...Read in sales and attribute data (trnfem) from csv  

!   Open the file
    FNAME = 'TRNFEM'
    WKUNIT = FILE_MGR('O',FNAME,NEW)
    
!   -- Read file line by line --
!   Read and drop the header
    READ(WKUNIT, '(A)', IOSTAT=status) line_buffer
    
!   Read the data    
    current_row_idx = 0
    DO i = 1, NUM_ROWS 
      current_row_idx = current_row_idx + 1
      
      ! Read an entire line into the buffer
      READ(WKUNIT, '(A)', IOSTAT=status) line_buffer
      IF (status /= 0) THEN
        PRINT *, "Error reading line ", current_row_idx, " or unexpected EOF. IOSTAT=", status
        STOP
      END IF
      
      ! Now parse the line_buffer using an internal read (assumes commas are delimiters)
      READ(line_buffer, *, IOSTAT=status) &
           col1_year(current_row_idx), &
           col2_icl(current_row_idx), &
           col3_igp(current_row_idx), &
           col4_ildv(current_row_idx), &
           col1_price(current_row_idx), &
           col2_mpgtst(current_row_idx), &
           col3_mpgadj(current_row_idx), &
           col4_mpgcomp(current_row_idx), &
           col5_hp(current_row_idx), &
           col6_tanksz(current_row_idx), &
           col7_evrange(current_row_idx), &
           col8_range(current_row_idx), &
           col9_curbwgt(current_row_idx), &
           col10_battkwh(current_row_idx), &
           col11_ftprnt(current_row_idx), &
           col12_phevevmt(current_row_idx), &
           col13_sales(current_row_idx)
          
      IF (status /= 0) THEN
        PRINT *, "Error parsing line ", current_row_idx, " with data: '", TRIM(line_buffer), "' IOSTAT=", status
        STOP
      END IF
      
    ENDDO ! NUM_ROWS

    WKUNIT = FILE_MGR('C',FNAME,NEW)

!   Fill arrays with data from input file    
    do m2=1,NUM_ROWS
      y2 = col1_year(m2)
      c2 = col2_icl(m2)
      g2 = col3_igp(m2)
      v2 = col4_ildv(m2)
	  fempri(g2,c2,y2,v2) = col1_price(m2) * MC_JPGDP(1)/MC_JPGDP(y2-1989)
	  femmpg(g2,c2,y2,v2) = col2_mpgtst(m2)
	  mpgadj(g2,c2,y2,v2) = col3_mpgadj(m2)
	  mpgcomp(g2,c2,y2,v2)= col4_mpgcomp(m2)
	  femhp(g2,c2,y2,v2)  = col5_hp(m2)
	  femtsz(g2,c2,y2,v2) = col6_tanksz(m2)
	  ev_rng(g2,c2,y2,v2) = col7_evrange(m2)
	  femrng(g2,c2,y2,v2) = col8_range(m2)
	  femwgt(g2,c2,y2,v2) = col9_curbwgt(m2)
	  BatPackSize(y2,c2,g2,v2) = col10_battkwh(m2)
	  fprt(g2,c2,y2,v2)   = col11_ftprnt(m2)
	  phev_evmt(g2,c2,y2,v2) = col12_phevevmt(m2)      
	  cafesales(g2,c2,y2,v2) = col13_sales(m2)
    enddo

!...clear fem variables
    FE(1:maxgroup,1:maxclass,base:current,1:maxldv)       = 0.0
    WEIGHT(1:maxgroup,1:maxclass,base:current,1:maxldv)   = 0.0
    PRICE(1:maxgroup,1:maxclass,base:current,1:maxldv)    = 0.0
    HP(1:maxgroup,1:maxclass,base:current,1:maxldv)       = 0.0
    TANKSIZE(1:maxgroup,1:maxclass,base:current,1:maxldv) = 0.0
    RANGE(1:maxgroup,1:maxclass,base:current,1:maxldv)    = 0.0
	
!...set FEM base-current year data (xyr)
    do i=base,current
      FE(1:maxgroup,1:maxclass,i,1:maxldv)       = FEMMPG(1:maxgroup,1:maxclass,xyr,1:maxldv)
      WEIGHT(1:maxgroup,1:maxclass,i,1:maxldv)   = FEMWGT(1:maxgroup,1:maxclass,xyr,1:maxldv)
      PRICE(1:maxgroup,1:maxclass,i,1:maxldv)    = FEMPRI(1:maxgroup,1:maxclass,xyr,1:maxldv)
      HP(1:maxgroup,1:maxclass,i,1:maxldv)       = FEMHP(1:maxgroup,1:maxclass,xyr,1:maxldv)
      TANKSIZE(1:maxgroup,1:maxclass,i,1:maxldv) = FEMTSZ(1:maxgroup,1:maxclass,xyr,1:maxldv)
      RANGE(1:maxgroup,1:maxclass,i,1:maxldv)    = FEMRNG(1:maxgroup,1:maxclass,xyr,1:maxldv)
      DO ildv=1,maxldv
        MKT_PEN(1:maxgroup,1:maxclass,1:numtech,i,ildv) = MKT_PEN(1:maxgroup,1:maxclass,1:numtech,base,gas)
      ENDDO
    enddo

!   Fill regional sales variable
	do igp=1,maxgroup
	  do icl=1,maxclass 
		do ildv=1,maxldv 
		  ldv_sales(igp,icl,ildv,mnumcr,iy:lyr_hist) = cafesales(igp,icl,iy+1989:lyr_hist+1989,ildv) 
		enddo 
	  enddo
	enddo

	do ildv=1,maxldv
      if((ILDV.ge.4.and.ildv.le.7).or.ildv.ge.14) then 
        do icl=1,maxclass
          do igp=1,maxgroup
		    do i=base,current
		      if(igp.le.cargrp) then
		        ElecSysIncCost(icl,igp,i,ildv) = (BatPackSize(xyr,icl,igp,ildv)*Li_ion_Cost(ildv,xyr)) + ElecNonBattCst(icl,xyr,1,ildv)
			  else 
			    ElecSysIncCost(icl,igp,i,ildv) = (BatPackSize(xyr,icl,igp,ildv)*Li_ion_Cost(ildv,xyr)) + ElecNonBattCst(icl,xyr,2,ildv)
			  endif 
		    enddo
          enddo
        enddo
      endif
    enddo

    RETURN
    END SUBROUTINE READHIST

! ==========================================================================================================
! ... Subroutine READNHTSA reads the EPA/NHTSA historic data post xyr. 
! ==========================================================================================================
    SUBROUTINE READNHTSA
    USE T_
    USE MEAN_FUNCS
    IMPLICIT NONE

    LOGICAL*1      NEW/.FALSE./
    CHARACTER*18   INAME
    CHARACTER*18   FNAME
    INTEGER        WKUNIT

!...new detailed model year data
	INTEGER 	NUM_E, ICYR
	PARAMETER 	(NUM_E = 159) ! update this value when model year data are updated
	INTEGER*2	ICL_E(NUM_E), IGP_E(NUM_E), ILDV_E(NUM_E)
	REAL 		PRICE_E(NUM_E), HRSPWR_E(NUM_E), TANKSZ_E(NUM_E), TRNKSZ_E(NUM_E), CURBWGT_E(NUM_E)
	REAL 		BATTKWH_E(NUM_E), FTPRNT_E(NUM_E), SALES_E(NUM_E), RANGE_E(NUM_E) 
	REAL		MPGTST_E(NUM_E), MPGADJ_E(NUM_E), MPGCOMP_E(NUM_E), PHEV_EVMT_E(NUM_E)
	REAL		EV_RANGE_E(NUM_E), PHEVMPG_D_E(NUM_E), PHEVMPG_S_E(NUM_E), NAMEPLATE_E(NUM_E)
	REAL		DEN1, DEN2, own_sales_ttl(maxgroup,maxclass,maxldv,mnumcr-2), tempmpg(maxgroup,maxclass,maxldv)
	REAL		EVMPGADJ(maxclass,2,maxvtyp), luggage(maxvtyp,maxclass), num1, num2
	INTEGER*2	nm, gp, cl, ld, cy, ow, cd, m2

!   Parameters for detailed annual sales data (trnnhtsa.csv) 
    INTEGER, PARAMETER      :: NUM_ROWS = 29796       ! Must set this manually based on input file trnhtsa.csv
    integer, PARAMETER      :: NUM_IND_COLS = 6
    INTEGER, PARAMETER      :: NUM_DATA_COLS = 1
    INTEGER, PARAMETER      :: TOTAL_COLS = NUM_IND_COLS + NUM_DATA_COLS
    INTEGER, PARAMETER      :: MAX_LINE_BUFFER_LENGTH = 20 * TOTAL_COLS 

    INTEGER, DIMENSION(NUM_ROWS) :: col1_year
    INTEGER, DIMENSION(NUM_ROWS) :: col2_regn
    INTEGER, DIMENSION(NUM_ROWS) :: col3_icl
    INTEGER, DIMENSION(NUM_ROWS) :: col4_igp
    INTEGER, DIMENSION(NUM_ROWS) :: col5_ildv
    INTEGER, DIMENSION(NUM_ROWS) :: col6_iown
    
    REAL, DIMENSION(NUM_ROWS) :: LDVSALES_DETAIL

!   Parameters for csv read-in    
    CHARACTER(LEN=MAX_LINE_BUFFER_LENGTH) :: line_buffer
    INTEGER :: status, current_row_idx

!...Read in sales and attribute data (trnfem) from csv  

!   Open the file
    FNAME = 'TRNNHTSA'
    WKUNIT = FILE_MGR('O',FNAME,NEW)
    
!   -- Read file line by line --
!   Read and drop the header
    READ(WKUNIT, '(A)', IOSTAT=status) line_buffer
    
!   Read the data    
    current_row_idx = 0
    DO i = 1, NUM_ROWS 
      current_row_idx = current_row_idx + 1
      
      ! Read an entire line into the buffer
      READ(WKUNIT, '(A)', IOSTAT=status) line_buffer
      IF (status /= 0) THEN
        PRINT *, "Error reading line ", current_row_idx, " or unexpected EOF. IOSTAT=", status
        STOP
      END IF
      
      ! Now parse the line_buffer using an internal read (assumes commas are delimiters)
      READ(line_buffer, *, IOSTAT=status) &
           col1_year(current_row_idx), &
           col2_regn(current_row_idx), &
           col3_icl(current_row_idx), &
           col4_igp(current_row_idx), &
           col5_ildv(current_row_idx), &
           col6_iown(current_row_idx), &
           LDVSALES_DETAIL(current_row_idx)
          
      IF (status /= 0) THEN
        PRINT *, "Error parsing line ", current_row_idx, " with data: '", TRIM(line_buffer), "' IOSTAT=", status
        STOP
      END IF
      
    ENDDO ! NUM_ROWS

    WKUNIT = FILE_MGR('C',FNAME,NEW)

!   Fill the sales array (own_sales) with the input data from trnnhtsa.csv
    do m2=1,NUM_ROWS
      ow = col6_iown(m2)
	  gp = col4_igp(m2)
	  cl = col3_icl(m2)
	  ld = col5_ildv(m2)
	  cd = col2_regn(m2)
	  cy = col1_year(m2)
	  own_sales(ow,gp,cl,ld,cd,cy) = LDVSALES_DETAIL(m2)
    enddo

!...Read last historical year epa data for FEM
    INAME = 'TRNNHTSAX'                 ! trnnhtsaX.xlsx
    WKUNIT = FILE_MGR('O',INAME,NEW)    !open trnnhtsaX.xlsx input file
    CALL ReadRngXLSX(WKUNIT,'trnnhtsa') !read range names & corresponding data from worksheet "trnnhtsa"
    WKUNIT = FILE_MGR('C',INAME,NEW)    !close xlsx input file

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
	CALL GETRNGR('EVMPGADJ        ',EVMPGADJ(1:MAXCLASS,1:2,1:MAXVTYP),MAXCLASS,2,MAXVTYP)

	if(epalyr.gt.xyr) then
!...  populate LDV data arrays	  
      do nm=1,num_e
		gp = igp_e(nm)
		cl = icl_e(nm)
		ld = ildv_e(nm)
		cy = epalyr
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

	do i=EPALYR-1989,MNUMYR
      do icl=1,maxclass
		do ildv=1,maxldv 
          X210(1,icl,ildv,1:mnumcr-2,i)  = atvcocar1(ildv,1:mnumcr-2,icl)
          X210(2,icl,ildv,1:mnumcr-2,i)  = atvcocar2(ildv,1:mnumcr-2,icl)
          X210(3,icl,ildv,1:mnumcr-2,i)  = atvcocar3(ildv,1:mnumcr-2,icl)
          X210(4,icl,ildv,1:mnumcr-2,i)  = atvcocar4(ildv,1:mnumcr-2,icl)
          X210(5,icl,ildv,1:mnumcr-2,i)  = atvcocar5(ildv,1:mnumcr-2,icl)
          X210(6,icl,ildv,1:mnumcr-2,i)  = atvcotrk1(ildv,1:mnumcr-2,icl)
          X210(7,icl,ildv,1:mnumcr-2,i)  = atvcotrk2(ildv,1:mnumcr-2,icl)  
          X210(8,icl,ildv,1:mnumcr-2,i)  = atvcotrk3(ildv,1:mnumcr-2,icl)  
          X210(9,icl,ildv,1:mnumcr-2,i)  = atvcotrk4(ildv,1:mnumcr-2,icl)
          X210(10,icl,ildv,1:mnumcr-2,i) = atvcotrk5(ildv,1:mnumcr-2,icl)
          X210(11,icl,ildv,1:mnumcr-2,i) = atvcotrk6(ildv,1:mnumcr-2,icl)
          
!         Calibrate pseudo-historical year (AEO2026: 2025)
          if (i.gt.epalyr - 1989) then
            do igp=1,maxgroup
              ivtyp = GrpMap(igp)
              WHERE (X210(igp,icl,ildv,1:mnumcr-2,i).gt.0.0)
                X210(igp,icl,ildv,1:mnumcr-2,i) = X210(igp,icl,ildv,1:mnumcr-2,i) * ATVCOEF_CALIB(ildv,ivtyp)
              ELSEWHERE
                X210(igp,icl,ildv,1:mnumcr-2,i) = X210(igp,icl,ildv,1:mnumcr-2,i) * (1-ATVCOEF_CALIB(ildv,ivtyp)+1)
              END WHERE
            enddo
          endif
          
!         Calibrate 2026 sales (more HEV momentum if no MY27+, less if MY27+), maintain calibration factor through projection
!         Lean manufacturers/consumers further toward hybrids versus gasoline vehicles due to decline in consumer resistance to hybridization,
!         manufacturer hedging of future policy, and further dropping of non-HEV gasoline vehicles from lineups (e.g. RAV4, Camry)
          do igp = 1,maxgroup
            if (ildv.eq.16) then
              if(ENFORCE_MY27REGS.eq.1.and.i.ge.epalyr+2-1989) then
                if (i.le.2032-1989) then 
                  X210(igp,icl,ildv,1:mnumcr-2,i) = X210(igp,icl,ildv,1:mnumcr-2,i) + 0.1
                elseif (i.gt.2032-1989) then
                  X210(igp,icl,ildv,1:mnumcr-2,i) = X210(igp,icl,ildv,1:mnumcr-2,2032-1989) + 0.75*((i+1989.0)-(2032.0))/(mnumyr+1989.0-(2032.0))
                endif
              else
                if (i.eq.epalyr+2-1989) then
                  X210(igp,icl,ildv,1:mnumcr-2,i) = X210(igp,icl,ildv,1:mnumcr-2,i) + 0.2
                elseif (i.gt.epalyr+2-1989) then
                  X210(igp,icl,ildv,1:mnumcr-2,i) = X210(igp,icl,ildv,1:mnumcr-2,epalyr+2-1989) + 0.55*((i+1989.0)-(epalyr+2.0))/(mnumyr+1989.0-(epalyr+2.0))
                endif
              endif
            endif
          enddo
        enddo
      enddo
    enddo
    
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
    tempmpg(1:maxgroup,1:maxclass,1:maxldv) = 0.0
    do igp=1,maxgroup 
	  do icl=1,maxclass 
	    do ildv=1,maxldv
		  if(batpacksize(epalyr,icl,igp,ildv).ne.0.0) then
		    if(ildv.eq.5.or.ildv.eq.6) then ! phevs
		      tempmpg(igp,icl,ildv) = ev_rng(igp,icl,epalyr,ildv)/BatPackSize(epalyr,icl,igp,ildv)*CFMGQ(n)*1000000.0/42.0*(1.0/3412.0)
			elseif(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then ! evs
		      tempmpg(igp,icl,ildv) = ev_rng(igp,icl,epalyr,ildv)/BatPackSize(epalyr,icl,igp,ildv)*CFMGQ(n)*1000000.0/42.0*(1.0/3412.0)
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
		  if(epampg(igp,icl,epalyr,ildv).gt.0.0.and.epalug(igp,icl,epalyr,gas).gt.0.0) then
		    if(igp.le.cargrp) then
			  num1 = num1 + cafesales(igp,icl,epalyr,ildv) * epalug(igp,icl,epalyr,ildv)/epalug(igp,icl,epalyr,gas)
			  den1 = den1 + cafesales(igp,icl,epalyr,ildv) 
			else 
			  num2 = num2 + cafesales(igp,icl,epalyr,ildv) * epalug(igp,icl,epalyr,ildv)/epalug(igp,icl,epalyr,gas)
			  den2 = den2 + cafesales(igp,icl,epalyr,ildv)	
			endif
		  endif
		enddo
	  enddo
	  if(den1.ne.0.0) luggage(1,icl) = num1/den1
	  if(den2.ne.0.0) luggage(2,icl) = num2/den2
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
    
!   calculate vehicle foot print    
    do i = 2010, mnumyr+1989
      if (i.le.epalyr) then
        do igp = 1, maxgroup
          do icl = 1, maxclass
            fprint(icl,igp,i-1989) = WEIGHTED_MEAN_1D(fprt(igp,icl,i,1:maxldv), &
                                                 cafesales(igp,icl,i,1:maxldv),&
                                                 'fprint')
          enddo
        enddo
!     hold last year fprint constant for projection years
      else
        fprint(1:maxclass,1:maxgroup,i-1989) = fprint(1:maxclass,1:maxgroup,epalyr-1989)
      endif
    enddo

!   Sum sales (collapse owner type dimension)
    ldv_sales(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,2019-1989:stockyr-1989) = sum(own_sales(1:maxowner,1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,2019:stockyr),DIM=1)
    ldv_sales(1:maxgroup,1:maxclass,1:maxldv,11,2019-1989:stockyr-1989) = sum(ldv_sales(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,2019-1989:stockyr-1989),DIM=4)
    
!   Calculate share of sales by fleet, group, class, ildv, and region
!   first calculate sales total (collapse owner type dimension)
    own_sales_ttl(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2) = sum(own_sales(1:maxowner,1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,stockyr),DIM=1) 

!...calculate owner sales shares at stockyr
	ownsalesshr(1:maxowner,1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,stockyr-1989) = 0.0
    
    do iown=1,maxowner 
	  WHERE (own_sales_ttl(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2).gt.0.0)
        ownsalesshr(iown,1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,stockyr-1989) = own_sales(iown,1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,stockyr)/ &
                                                                                   own_sales_ttl(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2)
      END WHERE
    enddo

!   Temp ownershare to get model working
    do iown=1,maxowner
      WHERE (sum(own_sales_ttl(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2),DIM=3).gt.0.0)
        ownsaletemp(iown,1:maxgroup,1:maxclass,1:mnumcr-2,stockyr-1989) = sum(own_sales(iown,1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2,stockyr),DIM=3)/ &
                                                                          sum(own_sales_ttl(1:maxgroup,1:maxclass,1:maxldv,1:mnumcr-2),DIM=3)
      END WHERE
    enddo 
    
    do ildv=1,maxldv
      if (ildv.eq.4.or.ildv.eq.5.or.ildv.eq.6.or.ildv.eq.7.or.ildv.eq.15) then
        EV_range(1:maxgroup,1:maxclass,ildv,1990:epalyr) = ev_rng(1:maxgroup,1:maxclass,1990:epalyr,ildv)
      endif
    enddo

    RETURN
    END SUBROUTINE READNHTSA

! ==========================================================================================================
! ... Subroutine CALIBNHTSA calibrates FEM results to NHTSA values.
!     I.e., the bottom-up estimates of fuel economy and other attributes from FEMCALC are adjusted based
!     on the ratio of FEMCALC mpg to actual historical attributes in the last historical year.
!     This is only applied through the last historical year; projection year attributes are calculated
!     from already-calibrated previous-year values.
! ==========================================================================================================
    SUBROUTINE CALIBNHTSA
    USE T_
    IMPLICIT NONE

!...Calibration factors are based on historical EPA/NHTSA data after xyr and through the last available 
!   data year (EPALYR)
    IF (curcalyr.le.epalyr) THEN
      calratio_fe  = 1.0
      calratio_hp  = 1.0
      calratio_wgt = 1.0
      calratio_pri = 1.0
      calratio_tsz = 1.0

      WHERE (femmpg(1:maxgroup,1:maxclass,yrs,1:maxldv).ne.0.0) 
        calratio_fe (1:maxgroup,1:maxclass,1:maxldv) = epampg(1:maxgroup,1:maxclass,yrs,1:maxldv)/femmpg(1:maxgroup,1:maxclass,yrs,1:maxldv)
		calratio_hp (1:maxgroup,1:maxclass,1:maxldv) = epahp (1:maxgroup,1:maxclass,yrs,1:maxldv)/femhp (1:maxgroup,1:maxclass,yrs,1:maxldv)
		calratio_wgt(1:maxgroup,1:maxclass,1:maxldv) = epawgt(1:maxgroup,1:maxclass,yrs,1:maxldv)/femwgt(1:maxgroup,1:maxclass,yrs,1:maxldv)
		calratio_pri(1:maxgroup,1:maxclass,1:maxldv) = epapri(1:maxgroup,1:maxclass,yrs,1:maxldv)/fempri(1:maxgroup,1:maxclass,yrs,1:maxldv)
      END WHERE
      
      WHERE (femmpg(1:maxgroup,1:maxclass,yrs,1:maxldv).ne.0.0.and.femtsz(1:maxgroup,1:maxclass,yrs,1:maxldv).ne.0.0) 
        calratio_tsz(1:maxgroup,1:maxclass,1:maxldv) = epatsz(1:maxgroup,1:maxclass,yrs,1:maxldv)/femtsz(1:maxgroup,1:maxclass,yrs,1:maxldv)
      END WHERE
    
      WHERE (femmpg(1:maxgroup,1:maxclass,yrs,1:maxldv).ne.0.0) 
        femmpg(1:maxgroup,1:maxclass,yrs,1:maxldv) = femmpg(1:maxgroup,1:maxclass,yrs,1:maxldv) * calratio_fe (1:maxgroup,1:maxclass,1:maxldv)
        femhp (1:maxgroup,1:maxclass,yrs,1:maxldv) = femhp (1:maxgroup,1:maxclass,yrs,1:maxldv) * calratio_hp (1:maxgroup,1:maxclass,1:maxldv) 
        femwgt(1:maxgroup,1:maxclass,yrs,1:maxldv) = femwgt(1:maxgroup,1:maxclass,yrs,1:maxldv) * calratio_wgt(1:maxgroup,1:maxclass,1:maxldv)
        fempri(1:maxgroup,1:maxclass,yrs,1:maxldv) = fempri(1:maxgroup,1:maxclass,yrs,1:maxldv) * calratio_pri(1:maxgroup,1:maxclass,1:maxldv)
        femtsz(1:maxgroup,1:maxclass,yrs,1:maxldv) = femtsz(1:maxgroup,1:maxclass,yrs,1:maxldv) * calratio_tsz(1:maxgroup,1:maxclass,1:maxldv)
      END WHERE
    
    ENDIF
    
    if (curcalyr.eq.epalyr) then 
      do igp=1,maxgroup
        do icl=1,maxclass
          do ildv=1,maxldv
	  	  	  if(cafesales(igp,icl,yrs,ildv).ne.0.0.and.femmpg(igp,icl,yrs,ildv).eq.0.0) then
                write(21,*) ' fix group flag for the following:'
	  		    write(21,*) '   YRS   = ',yrs
                write(21,*) '   IGP   = ',igp
                write(21,*) '   ICL   = ',icl
                write(21,*) '   ILDV  = ',ildv
	  		    write(21,*) '  femmpg = ',femmpg(igp,icl,yrs,ildv) 
	  		    write(21,*) '  epampg = ',epampg(igp,icl,yrs,ildv)
	  	    endif
          enddo
        enddo
      enddo
    endif

    RETURN
    END SUBROUTINE CALIBNHTSA
