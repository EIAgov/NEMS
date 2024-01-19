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
INCLUDE 'HMMBLK'

! ... Global Declaration Section    
! ... Switches for transportation scenarios

INTEGER      RTOVALUE, FILE_MGR
EXTERNAL     RTOVALUE, FILE_MGR

INTEGER      H2UNIT                                                 ! unit for writing hydrogen output to TRNH2OUT.txt
INTEGER      NRG2007,  &                                            ! "1" turns on EISAMPG and EISAE85  
             NRG2008,  &                                            ! "1" turns on EIEA08 PHEV tax credit
             STIMULUS, &                                            ! "1" turns on 2009 Stimulus Package PHEV tax credit
			 LEGIRA,   &											! "1" turns on 2022 IRA tax credits for PHEV and eV
             TRANEFF                                                ! "1" turns on FRZNEFF
                                                                    ! "2" turns on HIGHEFF
                                                                    ! "3" turns on CONSUMER
                                                                    ! "4" turns off EISAMPG
                                                                    ! "5" turns on Phase 2 HDV standards
                                                                    ! "6" turns on CAFECASE
                                                                    ! "7" turns on MUCHE85
                                                                    ! "8" turns on CAFECASE + LVPRCADJ
INTEGER      IBank                                                  ! "1" turns on CAFE banking
INTEGER      CAFEMEET                                               ! "1" calls subroutine CAFETEST: ensures CAFE compliance by increasing
INTEGER      TRANAB32                                               ! Switch for California AB32 off/on
                                                                    !     sales of hybrid and diesel vehicles
INTEGER      FRTEFF                                                 ! technology switch for the heavy vehicles
                                                                    !     "1" turns on high technology
                                                                    !     "2" turns on frozen technology
REAL         FRZNEFF,  &                                            ! "1" frozen technology scenario
             HIGHEFF,  &                                            ! "1" high technology scenario
             CONSUMER, &                                            ! "1" consumer preference for increased fuel economy scenario
             MUCHE85,  &                                            ! "1" increased consumer preference for E-85 
             EISAMPG,  &                                            ! "1" implements EISA07 CAFE standards
             EISAE85,  &                                            ! "1" implements EISA07 ethanol requirements 
             APPLYPHEV,&                                            ! "1" enables PHEV learning EIEA08 and Stimulus
             PHEVSTIM, &                                            ! "1" switch for stimulus case
			 IRA_STIM, &											! "1" switch for IRA PHEV/EV tax credit
             MOREFFV                                                ! "1" switch for increased FFV sales
INTEGER      CAFECASE, &                                            ! "1" switch for CAFE cases
             LVPRCADJ                                               ! "1" switch for light vehicle price adjustment that subtracts discounted
                                                                    !     fuel savings      
             
! ... Global definitions

integer      iy                                                     ! year index corresponding to first year of inputs
integer      num_to_read                                            ! number of years in input ranges
integer      First_Read_Year                                        ! = 1995
INTEGER      YRS                                                    ! actual model year (1989+curiyr)
INTEGER      N                                                      ! tran variable for curiyr
INTEGER      LASTID                                                 ! parameter for index describing number of technologies

INTEGER      MAXGROUP,MAXVTYP,MAXCLASS,MAXTECH,MAXLDV,MAXATV        ! various
INTEGER      MAXNOTE,MAXAGE,MAXLTAGE,MAXFLEET,MAXNMLM               ! parameters,
INTEGER      MAXFUEL,FUELYRS,GAS,FCLO,FCHI,BASE,PREV,CURRENT        ! see
INTEGER      BYR,LYR,XYR, AGEGRP, MF, MAXPEV,MAXZEVR                ! definitions   
INTEGER      MIDSIZE,DOMCAR,MAXRAILREGN,CARGRP,LTKGRP               ! below
INTEGER      MAXHAV,numstkyrs, STOCKYR								

PARAMETER    (MAXGROUP    = 11,          &                          ! number of light duty vehicle groups  (5-car and 6-light truck)
              CARGRP      = 5,           &                          ! number of light duty vehicle groups that are car mfgs
			  LTKGRP      = 6,           &                          ! number of light duty vehicle groups that are light truck mfgs
			  MAXVTYP     = 2,           &                          ! number of light duty vehicle types (car/truck)
              MAXCLASS    = 8,           &                          ! number of vehicle classes in each light duty group
              MAXTECH     = 100,         &                          ! number of light duty vehicle technologies
              MAXLDV      = 16,          &                          ! number of light duty vehicle fueling configurations
              MAXATV      = MAXLDV-1,    &                          ! number of light duty vehicle non-gasoline fueling configurations
              MAXNOTE     = 400,         &                          ! number of light duty vehicle engineering notes
              MAXAGE      = 25,          &                          ! number of light duty vehicle vintages
              MAXLTAGE    = 20,          &                          ! number of commercial light truck vintages
              MAXAGE2B    = 34,          &                          ! number of commercial light truck vintages starting in AEO2017
			  MAXOWNER    = 5,           &                          ! number of owner types (household/business/government/utility/taxi)
              MAXFLEET    = 4,           &                          ! number of light duty fleet types (business/government/utility/taxi)
              MAXNMLM     = 13,          &                          ! number of coefficients for the nested multinomial logit model
              MAXFUEL     = 8,           &                          ! number of distinct fueling station types
              MAXZEVR     = 5,           &                          ! number of ZEV credit regulatory categories (ZEV,TZEV,ATPZEV,PZEV,NEV) 
              MAXPEV      = 4,           &                          ! number of PHEVs and BEVs
              FUELYRS     = 18,          &                          ! number of years of fueling station data
              GAS         = 1,           &                          ! vehicle fueling configuration index value for gasoline
              FCLO        = 13,          &                          ! vehicle fueling configuration minimum fuel cell index value
              FCHI        = 15,          &                          ! vehicle fueling configuration maximum fuel cell index value
              BASE        = 0,           &                          ! FEM attribute index value for the base year
              PREV        = 1,           &                          ! FEM attribute index value for the previous year
              CURRENT     = 2,           &                          ! FEM attribute index value for the current year
              BYR         = BASEYR,      &                          ! base year for TRAN (1990)
              XYR         = 2020,        &                          ! base year for FEM 			!CCL
              LYR         = ENDYR,       &                          ! last projection year
              MIDSIZE     = 4,           &                          ! vehicle class index value for a midsize car
              DOMCAR      = 1,           &                          ! vehicle group index value for a domestic car
              MAXRAILREGN = 2,           &                          ! number of rail regions (east/west)
              AGEGRP	  = 5,           &		                    ! population age groupings
              MF          = 2,           &		                    ! male =1, female =2
			  MAXHAV      = 4,           &                          ! number of levels of highly automated LDV (see ihav definition)
			  STOCKYR     = 2021,        &                          ! last year of census division level stock history years
			  NUMSTKYRS   = 27,			 &							! number of census division level stock history years
			  MG_HHV	  = 5253/42)                                ! Energy content (high heating value) of gasoline, in thousand btu per gallon

! ... Indices

integer      GrpMap(MAXGROUP)                                       ! Map for how groups are defined back to vehicle types: cars and light trucks
integer      GrpDIMap(MAXGROUP)                                     ! Map for how groups are defined back to domestic (=1) and import (=2)
INTEGER      IGP,     &                                             ! vehicle group (MAXGROUP)
             IVTYP,   &                                             ! vehicle type (MAXTYPE)
			 IOWN,    &                                             ! owner type (MAXOWNER)
             ICL,     &                                             ! vehicle size class (MAXCLASS) 
             ITECH,   &                                             ! technology type (MAXTECH)
             NUMTECH, &                                             ! actual number of input technologies                                   
             ILDV,    &                                             ! fuel engine technology (MAXLDV)
			 IPEV,    &                                             ! plug-in powertrain technology (MAXPEV)
			 IZEV
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
INTEGER      IZEVR
																	!    1 = ZEV
																	!	 2 = TZEV
																	!    3 = ATPZEV
																	!    4 = PZEV
																	!    5 = NEV

INTEGER      IATV                                                   ! ATV type (MAXATV)
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
             SIGN                                                   ! positive or negative indicator
								    ! age groupings
INTEGER      IAGR                   !   1  = 16-19
								    !   2  = 20-34
								    !   3  = 35-54
								    !   4  = 55-64
								    !   5  = 65+
INTEGER      IMF					!   1  = male
								    !   2  = female
INTEGER      IHAV                                                   ! 1 = L0 - non-HAV (SAE levels 0-3)
                                                                    ! 2 = L4a - low speed HAV (SAE level 4)
																	! 3 = L4b - high speed HAV (SAE level 4)
																	! 4 = L5 - fully automated vehicle (SAE level 5)
                                   
REAL         ROUNDOFF_ERROR                                         ! roundoff error buffer

! ... Transportation specific macro variables - subroutine TMAC

REAL         TMC_PGDP(MNUMYR)                                       ! GDP deflator
REAL         FUELTAX(MNUMYR)                                        ! incremental petroleum fuel tax - nominal $/million Btu
REAL         FUELTAX87(MNUMYR)                                      ! incremental petroleum fuel tax - in 1987$
REAL         LIC_TREND(AGEGRP,MF,MNUMCR-2)                          ! growth trend in licensing rates
REAL         LIC_ELAS(MNUMCR-2,AGEGRP)                              ! change in licensing rate realtive to change in employment
REAL         LIC_MAX(AGEGRP,MF,MNUMCR-2)                            ! maximum licensing rate
REAL         LICRATE_M(AGEGRP,MNUMYR,MNUMCR-2)                      ! regional male drivers licensing rate by age group
REAL         LICRATE_F(AGEGRP,MNUMYR,MNUMCR-2)                      ! regional female drivers licensing rate by age group
INTEGER      LicRHistYr                                             ! last historic data year for LicRate
REAL         LICDRIVER(AGEGRP,MF,MNUMCR,MNUMYR)                     ! licensed drivers by region, male/female and age group
REAL         TMC_CPI(MNUMCR,MNUMYR)                                 ! consumer price index
REAL         INC00$NPT(MNUMCR,MNUMYR)                               ! disposable income per capita (1000's OF 2000$) - transit rail module
REAL         INC00$16(MNUMCR,MNUMYR)                                ! disposable income per capita 16+ (2000$) - travel module
REAL         INC90$NP(MNUMCR,BYR:LYR)                               ! disposable income per capita (1990 $) - fuel economy module

! ... Total new light duty vehicle sales - subroutine NEWLDV

REAL         NEWCLS2B                                               ! New Class 2b vehicles
REAL         CARSHARE(mnumcr,MNUMYR)                                ! historic/normalized car share of ldv sales
REAL         TRKSHARE(mnumcr,MNUMYR)                                ! historic/normalized light truck share of ldv sales
REAL         CARSHRT(MNUMYR)                                        ! non-normalized projected car share 
REAL         TRKSHRT(MNUMYR)                                        ! non-normalized projected truck share
REAL         TTLSHR(MNUMYR)                                         ! total non-normalized share
REAL         NEWCARS(mnumcr,MNUMYR)                                 ! new car sales
REAL         NEWCLS12A(mnumcr,MNUMYR)                               ! new light truck sales, class 1-2A
REAL         LDVSALES(mnumcr-2,mnumyr)                              ! used to projet regional sales shares

! ... New light duty vehicle fuel economy - subroutine TMPGNEW

CHARACTER    FF                                                     ! for subroutine PRNTRSLT - MTCM diagnostic
INTEGER*2    O_UNIT                                                 ! for subroutine PRNTRSLT - MTCM diagnostic

! ...... historic values - subroutine READNHTSA

REAL         NHTSASAL(BYR:LYR,MAXCLASS,MAXGROUP)                    ! NHTSA sales
REAL         NHTSAHP(BYR:LYR,MAXCLASS,MAXGROUP)                     ! NHTSA horsepower
REAL         NHTSAFE(BYR:LYR,MAXCLASS,MAXGROUP)                     ! NHTSA fuel economy
REAL         NHTSAWGT(BYR:LYR,MAXCLASS,MAXGROUP)                    ! NHTSA weight
INTEGER      NHTSALYR                                               ! last year of NHTSA data

! ...... historic values for the fuel economy module - subroutine READHIST 

REAL         FEMMPG(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)               ! FEM fuel economy data for report writer
REAL         FEMHP(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)                ! FEM horsepower data for report writer
REAL         FEMPRI(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)               ! FEM low volume price data for report writer
REAL         FEMPRIH(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)              ! FEM high volume price data for report writer
REAL         FEMWGT(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)               ! FEM weight data for report writer
REAL         FEMRNG(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)               ! FEM range data for report writer
REAL         FEMTSZ(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)               ! FEM fuel tank size data for report writer
REAL         FEMVOL(MAXGROUP,MAXCLASS,BYR:LYR,MAXLDV)               ! FEM volume data for report writer
REAL         FEMPEN(MAXGROUP,MAXCLASS,MAXTECH,BYR:LYR,MAXLDV)       ! FEM technology penetration data for report writer

! ...... historic values for AFVs

REAL         AFVADJHP(MAXLDV,BYR:XYR)                               ! ATV horsepower differential
REAL         AFVADJRN(MAXLDV,BYR:XYR)                               ! ATV range differential
REAL         AFVADJFE(MAXLDV,BYR:XYR)                               ! ATV fuel economy differential
REAL         AFVADJWT(MAXLDV,BYR:XYR)                               ! ATV weight differential
REAL         AFVADJPR(MAXLDV,2,BYR:XYR)                             ! ATV low volume price differential 
REAL         AFVADJPRH(MAXLDV,2,BYR:XYR)                            ! ATV high volume price differential

! ...... input values for base year vehicles

REAL         FE(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)              ! vehicle class base fuel economy
REAL         WEIGHT(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)          ! vehicle class base curb weight
REAL         PRICE(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)           ! vehicle class base price (low volume)
REAL         PRICEHI(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)         ! vehicle class base price (high volume)
REAL         HP(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)              ! vehicle class base horsepower
real         vhp_adj(MAXCLASS,MAXGROUP,PREV:CURRENT,MAXLDV)         ! weight based hp adjustment
REAL         VOLUME(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)          ! vehicle class base interior volume
REAL         VALUEPERF(MAXCLASS,MAXGROUP)                           ! vehicle class base performance value
REAL         PERFFACT(MAXCLASS,MAXGROUP)                            ! vehicle class base performance factor
REAL         TANKSIZE(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)        ! vehicle class base fuel tank size
LOGICAL*1    CLASSFLAG(MAXCLASS,MAXGROUP,MAXLDV)                    ! AFV vehicle class applicability flag
INTEGER*2    CARFLG(MAXATV,MAXCLASS)                                ! ATV's introduction year for car classes
INTEGER*2    TRKFLG(MAXATV,MAXCLASS)                                ! ATV's introduction year for truck classes
REAL         CLASSBASEYR(MAXCLASS,MAXGROUP)                         ! vehicle class first year of sales

! ...... variable used in subroutine FEMCALC

INTEGER      PAYBACK                                                ! payback period
REAL         DISCOUNT                                               ! discount rate
REAL         PMGTR90$(MNUMCR,BYR:LYR)                               ! national gasoline price in 1990 dollars
REAL         CFE                                                    ! mpg used to calculate effectiveness = FE(ICL,IGP,PREV,ILDV)
REAL         PRICE_EX(12)                                           ! expected fuel price used in cost effectiveness calculation
REAL         PSLOPE                                                 ! expected rate of change in future fuel price
LOGICAL*1    CAFEPASS(MAXGROUP)                                     ! indicates manufacturer has passed CAFE standard
REAL         PERFCAP(MAXCLASS,MAXGROUP)                             ! vehicle class performance cap
REAL         USEDCAP(MAXCLASS,MAXGROUP)                             ! fraction of vehicle class performance cap used
REAL         MKT_PEN(MAXCLASS,MAXGROUP,MAXTECH,BASE:CURRENT,MAXLDV) ! technology market share
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
REAL         ADJPEN 
REAL         VMT(mnumcr,12)                                        ! annual vmt by vintage for each region

! ... STEO Benchmarking

REAL         SEDS_tran(mnumcr,4)                                    ! Last historic SEDS data for gasoline, jet fuel, distillate, and residual by region
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
REAL         BFVMTECHSC(2,9,mnumcr)
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
LOGICAL*1    PRINT_FE,PRINT_TECH,PRINT_DIAG                         ! print flags

!...Coporate Average Fuel economy (CAFE) standards for light duty vehicles 

REAL         CAFE_STAND(MAXGROUP,BYR:LYR)                           ! Single CAFE standards for cars and light trucks
!...Variables for Footprint CAFE.
REAL         FPrint(MAXCLASS,MAXGROUP,MNUMYR)                       ! vehicle footprint values
REAL         TrueMpgGrp(mnumcr,MAXGROUP,mnumyr)                     ! the true mpg of the group
REAL         CAFEMpgGrp(MAXGROUP,mnumyr)                            ! the mpg of the group for cafe purposes
REAL         GrpWgt                                                 ! proportion of cars that are in a group
REAL         MpgWgt                                                 ! proportional true mpg of the group
REAL         CafeMpgWgt                                             ! proportional mpg of the group using cafe standards 
REAL         DedAFVMpgWgt                                           ! propotional mpg of the group only including AFV, (to consider the cap on FF credits)
REAL         maxFFcredit                                            ! the maximum FF credits allowed in the proposal
REAL         AVSales_Old(MAXGROUP,MAXCLASS,MaxLdv,mnumyr)           ! initial vehicle sales
REAL         AVSales_New(MAXGROUP,MAXCLASS,MaxLdv,mnumyr)           ! revised vehicle sales
REAL         AVSales_Ttl(MAXGROUP,MAXCLASS,mnumyr)                  ! total sales
REAL         Apshr55_Grp(MAXGROUP,MaxLDV,MAXCLASS,mnumyr)

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
!...NHTSA GHG CAFE parameters for cars
REAL         CFCoefA2(MNUMYR)                                       ! max fuel economy target
REAL         CFCoefB2(MNUMYR)                                       ! min fuel eocnomy target
REAL         CFCoefC2(MNUMYR)                                       ! rate of change
REAL         CFCoefD2(MNUMYR)                                       ! constant
REAL         LDV_MPG_cl(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)            ! fuel economy by size class by fuel type
REAL         FPMpg(MAXCLASS,MAXGROUP,MNUMYR)                        ! CAFE by size class
REAL         FPMpgGrp(MAXGROUP,MNUMYR)                              ! CAFE by manufacturer
REAL         Cafe_Used(MAXGROUP,byr:lyr)                            ! CAFE standard used for light trucks
REAL         ALTTRUEMPG(3,MNUMYR)
INTEGER      AltCafe(MAXGROUP)                                      ! logic switch for CAFE used for light trucks
REAL         REF_MPG(MaxVTYP,6:MNUMYR)                              ! reference case tested fuel economy for cafe cases (update annually)
REAL         FUEL_$(MNUMYR)                                         ! fuel price for CAFE case
REAL         SAVED_$(MaxVTYP,MNUMYR)                                ! fuel saved from CAFE case
REAL		 AC_OC_CREDIT(MAXGROUP,MNUMYR)                          ! AC and off cycle CAFE credits 
INTEGER      FUEL_N

! various transportation variables

REAL         CLASS_SHARE(MNUMCR,MAXCLASS,MAXGROUP,BYR:LYR)          ! vehicle class market shares (within vehicle groups)
REAL         OCLASS_SHARE(mnumcr,MAXCLASS,MAXGROUP,BYR:LYR)         ! vehicle class market shares (across all vehicle groups)
REAL         COEF_A(MAXCLASS,MAXGROUP)                              ! ATV Y-intercept or alpha coefficient
REAL         COEF_B(MAXCLASS,MAXGROUP)                              ! ATV fuel price elasticities
REAL         COEF_C(MAXCLASS,MAXGROUP)                              ! ATV income elasticities
REAL         COEF_P(MAXCLASS,MAXGROUP)                              ! ATV vehicle price elasticities

!...Plug-in Electric Vehicle modeling
! 	Main battery results
REAL		 BatPackWgt(BYR:LYR,MAXCLASS,MAXGROUP,MAXLDV)			! Battery weight (full pack) for HEV,PHEV,BEV,and FCVs
REAL		 BatPackSize(BYR:LYR,MAXCLASS,MAXGROUP,MAXLDV)
REAL         ElecSysIncCost(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)  ! Cost of on-board electricity systems and storage
! 	Battery model parameters
REAL         NiMH_Cost(BYR:LYR)                                     ! Nickel metal hydride battery cost ($/kWhr)
!REAL         Li_ion_Cost(MAXLDV,BYR:LYR)                            ! Lithium-ion battery cost ($/kWhr) by vehicle type
REAL         PACK_A(MAXLDV)                                         ! Cumulative Li-ion battery pack initial cost parameter
REAL         PACK_B(MAXLDV)                                         ! Cumulative Li-ion battery pack learning rate parameter
REAL         MAT_A(MAXLDV)                                          ! Cumulative Li-ion materials parameter
REAL         MAT_B(MAXLDV)                                          ! Cumulative Li-ion materials learning rate parameter
REAL		 MAT_MARKUP(MNUMYR)										! Price adjustment to account for critical mineral supply constraints
REAL         EV_range(MAXCLASS,MAXGROUP,maxldv)                     ! EV all electric range for EV100, EV200, EV300
REAL		 EV_batt_size_m(MAXLDV)									! Battery sizing slope based on vehicle weight
REAL 		 EV_batt_size_b(MAXLDV)									! Battery sizing constant
REAL 		 EV_range_m(MAXLDV)										! Range slope based on EV battery size
REAL 		 EV_range_b(MAXLDV)										! Range constant based on EV battery size
REAL 		 LION_LB_perkWh(MAXLDV)									! Lithium-ion weight (lbs) per kWh battery capacity
REAL 		 LIONkWh_perLb(MAXLDV)									! Battery sizing factor (kWh) based on vehicle weight
REAL 		 EV_range_max(MAXLDV)									! Max EV range for each EV fuel type
REAL         PHEV_DOD(BYR:LYR)                                      ! depth of discharge percentage for PHEV battery
REAL         EV_DOD(BYR:LYR)                                        ! depth of discharge percentage for EV battery
! 	Non-battery incremental costs
REAL		 ElecNonBattCst(MAXCLASS,BYR:LYR,MAXVTYP,MAXLDV)		! Non-battery electric (HEV, PHEV, BEV) incremental costs
! 	Splitting PHEV travel and consumption into gas / electricity
REAL		 PHEV50_EVMT											!...Share of PHEV50 VMT on electric drive
REAL		 PHEV20_EVMT											!...Share of PHEV20 VMT on electric drive
REAL         PctPHEV20(MNUMCR,MNUMYR)                               ! Percent of PHEV20 energy use that is electric
REAL         PctPHEV50(MNUMCR,MNUMYR)                               ! Percent of PHEV50 energy use that is electric
REAL         mfg_eg_mpg20(MAXCLASS,MAXGROUP,MNUMYR)                 ! PHEV20 fuel economy ratio depletion(EV)/sustaining (HEV) mode by size class be mfg
REAL         mfg_eg_mpg50(MAXCLASS,MAXGROUP,MNUMYR)                 ! PHEV50 fuel economy ratio depletion(EV)/sustaining (HEV) mode by size class by mfg
REAL         tot_eg_mpg20(MAXCLASS,MAXGROUP,MNUMYR)                 ! PHEV20 fuel economy ratio sum
REAL         tot_eg_mpg50(MAXCLASS,MAXGROUP,MNUMYR)                 ! PHEV50 fuel economy ratio sum
REAL         EG_MPG20(MNUMYR)                                       ! ratio of PHEV20 electric drive MPG to gasoline drive MPG by car and light truck
REAL         EG_MPG50(MNUMYR)                                       ! ratio of PHEV50 electric drive MPG to gasoline drive MPG by car and light truck
REAL         EG_MPG(MNUMYR)
REAL         TE_MPG20(MNUMYR)
REAL         TE_MPG50(MNUMYR)
REAL         TG_MPG20(MNUMYR)
REAL         TG_MPG50(MNUMYR)
REAL         PHEVPlug(MNUMYR)

REAL         FuelCell$kW(BYR:LYR,FCLO:FCHI)                         ! fuel cell cost ($/kW)
REAL         FUELCELL(MAXCLASS,MAXGROUP,BASE:CURRENT,MAXLDV)        ! incremental fuel cell cost 
REAL         PHEV_Credit											! EIEA08/Stimulus PHEV vehicle credit  
REAL		 IRA_BAT_CRED											! IRA EV/PHEV battery tax credit
REAL		 IRA_VEH_CRED											! IRA EV/PHEV vehicle tax credit
REAL		 IRA_BAT_SHR(2,2023:LYR,3)								! Share of qualifying batteries {1 = EV, 2 = PHEV} {1:IRA,2:LoIRA,3:HiIRA}
REAL		 IRA_VEH_SHR(2,2023:LYR,3)								! Share of gualifying vehicles {1 = EV, 2 = PHEV}  {1:IRA,2:LoIRA,3:HiIRA}
REAL         kWh_Credit												! EIEA08/Stimulus PHEV credit per Kwh
REAL         PHEVTAXADJ(MNUMYR)										! Stimulus tax credit adjustment
REAL         Ttl_Credit(MaxVtyp,MaxLDV,MAXCLASS,BYR:LYR)            ! total PHEV credit
REAL         Max_Credit												! EIEA08/Stimulus PHEV max credit
REAL         AVG_KWH(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)
REAL         AVG_PHEV_CREDIT(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)
REAL         AVG_BAT_CREDIT(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)
REAL         TEC_ORNL(MAXCLASS,MAXGROUP,MAXTECH,MAXLDV)             ! tech cost
REAL         MKT_PENF(MAXGROUP,MAXTECH,MAXLDV)                      ! tech penetration agg over class
REAL         AVCOST(MAXGROUP,MAXTECH,MAXLDV)                        ! tech cost agg over class
REAL         MICROPEN(MAXVTYP,MAXLDV,MNUMYR)                        ! market penetration of micro hybrids
LOGICAL*1    MMSWITCH                                               ! ATV make/model availability switch
REAL         MMAVAIL(MAXVTYP,MAXCLASS,MAXLDV,MNUMCR-2,byr:lyr)      ! ATV make/model availability
REAL         X210(MAXVTYP,MAXLDV,MNUMCR-2)                          ! ATV calibration coefficients
REAL         X21(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 2, vehicle price
REAL         X22(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 2, fuel cost
REAL         X23(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 2, range
REAL         X24(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 2, battery replacement
REAL         X25(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 2, acceleration
REAL         X26(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 2, EV home refueling
REAL         X27(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 2, maintenance cost
REAL         X28(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 2, luggage space
REAL         X29(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 2, mak/mod availability
REAL         BETAFA2(MAXVTYP,MAXCLASS)                              ! ATV NMLM level 2, fuel availability 1
REAL         BETAFA22(MAXVTYP,MAXCLASS)                             ! ATV NMLM level 2, fuel availability 2
REAL         X11(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 1, tech set general cost
REAL         X31(MAXVTYP,MAXCLASS)                                  ! ATV NMLM level 3, multi-fuel gen cost
REAL         ATVCOEFF(MAXLDV,MNUMYR,MAXVTYP)                        ! ATV nested multinomial logit model calibration coefficients
REAL         FAVAIL(maxfuel,MNUMYR,MNUMCR-2)                        ! fuel availability by fuel, region, year
REAL         PCTFAVAIL(maxfuel,MNUMYR,MNUMCR-2)                     ! exogenous fuel availabiliy by fuel, region, year
REAL         INITSTA(maxfuel,FUELYRS,MNUMCR-2)                      ! initial refueling stations by fuel, yr, region
REAL         STA_RAT(maxfuel)                                       ! refuel stations per vehicle stock
REAL         MAINTCAR(MAXLDV,MAXCLASS)                              ! car maintenance cost by tech, size class
REAL         MAINTTRK(MAXLDV,MAXCLASS)                              ! truck maintenance cost by tech, size class
REAL         LUGGCAR(MAXLDV,MAXCLASS)                               ! luggage space by tech, size class
REAL         LUGGTRK(MAXLDV,MAXCLASS)                               ! luggage space by tech, size class
REAL         WGT(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)                   ! light duty vehicle weight
REAL         PSPR(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)                  ! average vehicle price
REAL         LDV_PRI(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)                ! vehice price by tech, class
REAL         LDV_RNG(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)                ! vehice range by tech, class
REAL         FPRICE(MAXLDV,MNUMCR,BYR:LYR)                          ! fuel price by region
REAL         VRNG(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)                  ! vehicle range
REAL         FLCOST(MAXVTYP,MAXLDV,MAXCLASS,MNUMCR,BYR:LYR)         ! fuel cost per mile
REAL         BRCOST25(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)              ! battery replacement cost - currently set to zero
REAL         ACCL(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)                  ! vehicle acceleration - 0 to 60 mph
REAL         HFUEL(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)                 ! home refueling 
REAL         MAINT(MAXVTYP,MAXLDV,MAXCLASS,BYR:LYR)                 ! vehicle maintenance cost
REAL         LUGG(MAXVTYP,MAXLDV,MAXCLASS)                          ! vehilce luggage space
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
REAL         Sales_Adjustment                                       ! adjustment factor used to increase sales by zev by CD
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

REAL         OWNER_SHARE(MNUMCR,MAXVTYP,MAXOWNER,MNUMYR)            ! share of new vehicle sales by owner type (household or fleet type) by region
REAL         FLTCRAT(MNUMYR)                                        ! fraction of total car sales attributed to fleets
REAL         FLTTRAT(MNUMYR)                                        ! fraction of total truck sales attributed to fleets
REAL         FLTAFSHRC(MAXCLASS,MNUMYR,MAXFLEET)                    ! percent of fleet afv cars by size class - dst added for read in
REAL         FLTAFSHRT(MAXCLASS,MNUMYR,MAXFLEET)                    ! percent of fleet afvs light trucks by size class - dst added for read in
REAL         FLTCARSHR(MAXLDV,MNUMYR,MAXFLEET)                      ! fleet car sales shares by ldv type
REAL         FLTTRKSHR(MAXLDV,MNUMYR,MAXFLEET)                      ! fleet light truck sales shares by ldv type
REAL         FLTSALSC(MAXVTYP,MAXCLASS,MAXLDV,MNUMYR)               ! fleet sales shares by size class
REAL         OLDFSTKT(MNUMCR,MAXVTYP,MAXLDV,MAXAGE)
REAL         SURVFLT(MAXFLEET,MAXAGE,MAXVTYP)                       ! LDV survival rate by fleet type
REAL         FLTSSHR(MAXCLASS,MNUMYR,MAXFLEET,MAXVTYP)              ! % of fleet vehicle by fleet type,size,vehicle type
REAL         FLTSSHRC(MAXCLASS,MNUMYR,MAXFLEET)                     ! % of fleet cars by fleet type,size,vehicle type
REAL         FLTSSHRT(MAXCLASS,MNUMYR,MAXFLEET)                     ! % of fleet light trucks by fleet type,size,vehicle type
REAL         FLTVMTYR(MAXFLEET,MNUMYR,MAXVTYP)                      ! annual miles of travel per vehicle
REAL		 FLT_COVID(MAXFLEET,MNUMYR)								! covid impact on fleet vehicle travel
REAL         FLTTRANSPC(MAXFLEET,MAXAGE)							! fraction of fleet passenger cars transfering from a given fleet to households 
REAL         FLTTRANSLT(MAXFLEET,MAXAGE)                            ! fraction of fleet light-duty trucks transfering from a given fleet to households
REAL         FLTTOTMPG(MAXVTYP)                                     ! total fleet mpg
REAL         FLTMPG(MAXVTYP,MAXFLEET,MAXLDV,MNUMYR)                 ! fleet MPG by vehicle type, fleet type, powertrain
REAL         TOTFLTCAR(MAXVTYP)                                     ! total fleet car
REAL         FLTMPGTOT2(MAXVTYP)                                    ! total fleet mpg
REAL         FLTVMTECH(MAXVTYP,MAXFLEET,MAXLDV,MAXHAV)				! fleet vmt
REAL         MPGFLTSTK(MAXVTYP,MAXFLEET,MAXLDV)                     ! fleet
REAL         FLTFUELBTU(MNUMCR,MAXFUEL,MNUMYR)                      ! fleet
REAL         FLTECH(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXHAV)                 ! fleet sales
REAL         OLDFSTK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXAGE)                ! fleet vehicles transfered to HH stock
REAL         TFLTECHSTK(MAXVTYP,MAXFLEET,MAXLDV,MAXHAV)             ! fleet stock
REAL         FLTECHSAL(MNUMCR,MAXVTYP,MAXFLEET,MAXCLASS,MAXLDV,MAXHAV)     ! fleet sales
REAL         FLTSALES(MAXVTYP,MAXFLEET,MAXCLASS,MNUMCR-2,MAXLDV)
REAL         AFLTSALES(MAXVTYP,MAXFLEET,MAXCLASS,MNUMCR-2,MAXLDV)
REAL         FLTSALES_T(MNUMCR-2,MAXFLEET,MAXLDV,MNUMYR)
REAL         FLTSALES_DELTA(MAXVTYP,MAXFLEET,MAXCLASS,MNUMCR,MAXLDV)
REAL         COVERED_FLTSALES(MNUMCR-2,MAXFLEET)
REAL         ZEV_FLTCREDIT_LDV(MNUMCR-2,MAXFLEET,MAXLDV,MNUMYR)
REAL         ZEV_FLTCREDIT_REG(MNUMCR-2,MAXFLEET,MAXZEV,MNUMYR)
REAL         ZEV_FLTCREDIT_EARN(MNUMCR-2,MAXFLEET,MAXZEV,MNUMYR)
REAL         FLTCREDIT_TRANSFER_RATE(MNUMCR-2,MAXFLEET)
REAL         CALIFORNIA_FLTCREDIT(MNUMCR-2,MAXFLEET,MAXLDV)
REAL         ADJUSTED_ZEV_FLTCREDIT(MNUMCR-2,MAXFLEET,MAXLDV)
REAL         FLT_STOCK(MNUMCR,MAXVTYP,MAXFLEET,MAXLDV,MAXAGE,MAXHAV,MNUMYR) ! Fleet Stock
REAL         FLTTLLDVBTU(MAXLDV,MNUMYR)                             ! fleet Btu by ldv type
REAL         FLTLDVC(MAXVTYP,MAXFLEET,MAXLDV,MNUMYR)

! ... Light Duty Vehicle Stock Module - LDV Stock Accounting Model

REAL         SURV25(MNUMCR,MAXAGE,MAXVTYP)                          ! 25 vintage survival rates for cars and light trucks
REAL         SURV_ADJ(MNUMYR)                                       ! low macro survival curve adjustment
REAL         SSURV25(MNUMCR,MAXAGE,MAXVTYP)                         ! 25 vintage survival rates for cars and light trucks
REAL         REFSURV(MAXVTYP,MAXAGE)                                ! initial survival rates
REAL         REFSALES(MNUMYR)                                       ! initial light duty vehicle sales
REAL         PVMT(MAXAGE,MNUMYR,MNUMCR,MAXLDV)                      ! car vmt per vintage by region
REAL         LVMT(MAXAGE,MNUMYR,MNUMCR,MAXLDV)                      ! light truck vmt per vintage by region
REAL		 VMT_SCHED_PARAM(MAXLDV,2)								! Parameters defining the convergence of EV VMT schedules with that of ICEs
REAL         CDF(MNUMYR)                                            ! degradation factor for car
REAL         LTDF(MNUMYR)                                           ! degradation factor for light truck
REAL         LDV_STOCK(mnumcr,maxvtyp,maxowner,maxldv,maxage,maxhav,mnumyr) ! LDV stock by region, vehicle type, owner type, fuel type, vintage 
REAL         fltstockr(MNUMCR-2,mnumyr)                             ! 
REAL         CARFLTSTKREGN(mnumcr-2,mnumyr,maxldv,maxage)           ! fleet car stock by census division 
REAL         LTFLTSTKREGN(mnumcr-2,mnumyr,maxldv,maxage)            ! fleet light truck by census division 
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
REAL         VMTLD(AGEGRP,MNUMYR,MF)                                ! VMT per licensed driver
REAL         VPLD(MNUMYR)                                           ! light duty vehicles per licensed driver
INTEGER      VMTLDHISTYR                                            ! last historic data year for VMTLD
REAL         VMTLDV(AGEGRP,MNUMYR,MF,MNUMCR)                 		! total ldv (<8,500 lbs. gvwr) household and fleet vmt
REAL         VMTHH(mnumyr,mnumcr,maxldv,maxvtyp)                    ! total household ldv vmt derived from licensed drivers
REAL         COSTMI(MNUMYR)                                         ! fuel cost of driving 1 mile (2004 cents per gallon)

! .. Commercial Light Truck Module

REAL         CLTVMTDIST(6)                                          ! Distribution of VMT by Industry
REAL         CLTSTKREGN(mnumyr,MAXAGE2B,9,mnumcr-2)                 ! CLT stock by region, year, vintage, and fuel
REAL         CLTSIC(MNUMYR)                                         ! SIC output averaged across 6 categories
REAL         CLTBTUT(10,MNUMYR)                                     ! total CLT consumption by Btu
REAL         CLTGAL(10)                                             ! CLT consumption in gals
REAL         CLTGALT
REAL         BCLTBTU(10,MNUMCR,MNUMYR)                              ! regional CLT fuel consumption Btu
REAL         CLTVMTT(9,MNUMYR)                                      ! CLT VMT by fuel type and total
REAL         CLS2bSTKHIST(6,mnumyr)                                 ! Class 2b stock history 1995-2011 (total stock not by vintage)
REAL         CLTSALESHIST(6,mnumyr)                                 ! Commerical light truck sales by fuel 1995-2011
REAL         CLTSALESPER(6,mnumyr)                                  ! Percent of new commerical light truck sales by fuel 1995-2011
REAL         CLTVMT_H(6,mnumyr)                                     ! Total commercial light truck VMT by fuel 1995-2011
REAL         CLTVMT2012(MAXAGE2B)                                   ! Commercial light truck VMT by vintage 1995-2011
REAL         CLTMPG2012(6,MAXAGE2B)                                 ! Commercial light truck MPG by vintage and fuel 1995-2011
REAL         CLTMPG(mnumyr,10)                                      ! Commercial light truck MPG by powertrain
REAL         NCLTMPG(mnumyr,10)                                     ! Commercial light truck new MPG by powertrain
REAL         CLTVMTVA(mnumyr,maxage2b)                              ! Commerical light truck vmt by vintage
REAL         CLTFBTU(mnumyr,10,mnumcr)                              ! Commerical truck BTU by fuel type

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

! ... Miscellaneous Transportation Energy Demand Module
REAL         PMGTR04$(MNUMCR,MNUMYR)                                ! regional gasoline price 2004$
REAL         PDSTR04$(MNUMCR,MNUMYR)                                ! regional diesel price 2004$

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
REAL		 TRCOVID(MNUMYR)										! transit rail COVID impact
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
REAL		 CRCOVID(MNUMYR)										! commuter rail COVID impact
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
REAL    TBCOVID(MNUMYR)												! transit bus COVID travel impact
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
REAL         SALESHR(mnumcr,MAXGROUP,MNUMYR)                        ! car and light truck sales shares by group
REAL         SALESHR_regn(mnumcr-2,MAXGROUP)                        ! car and light truck sales shares by group and census division
REAL         RATIO_BYR                                              ! used to determine size class shares 
REAL         RATIO_LN                                               ! used to determine size class shares
REAL         RATIO                                                  ! used to determine size class shares
REAL         GROUPSUM(MAXGROUP)                                     ! sum of class shares by manufacturer
REAL         PASSHRR(mnumcr,MAXCLASS,MNUMYR)                        ! car market shares by class
REAL         LTSHRR(mnumcr,MAXCLASS,MNUMYR)                         ! light truck market shares by class
REAL         NCSTSC(mnumcr,MAXCLASS,MNUMYR)                         ! car sales by class
REAL         NLTSTSC(mnumcr,MAXCLASS,MNUMYR)                        ! light truck sales by class
REAL         AHPCAR(MNUMCR,MNUMYR)                                  ! average car horsepower
REAL         AHPTRUCK(MNUMCR,MNUMYR)                                ! average light truck horsepower
REAL         AWTCAR(MNUMCR,MNUMYR)                                  ! average car weight
REAL         AWTTRUCK(MNUMCR,MNUMYR)                                ! average light truck weight
INTEGER*2    PASS,pass2
INTEGER*2    RETURN_STAT                                            ! technology supersedes check
REAL         GASMPG_ACTUAL(MAXGROUP,BYR:LYR)                        ! diagnostic check of forecasted vs. actual fuel economy 
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
REAL         PERGRP(MAXGROUP,MAXCLASS,MNUMYR)                       ! manufacture share of sales by size class
REAL         MPGHH(MNUMYR)											! household stock fuel economy
REAL         PE(MNUMYR)                                             ! vmt price elasticity
REAL         IE(MNUMYR)                                             ! vmt income elasticity
REAL         DE(MNUMYR)
REAL         MPGT(MAXLDV,MNUMYR)
REAL         MPGC(MAXLDV,MNUMYR)
REAL         CDFRFG(MNUMYR, MAXHAV)
REAL         LTDFRFG(MNUMYR, MAXHAV)
REAL         TTMPGLDV(MAXLDV,MNUMYR)
REAL         CCMPGLDV(MAXLDV,MNUMYR)
REAL         TLDVMPG(3,MNUMYR)                                      ! on-road stock fuel economy for all cars, light trucks, & total
REAL         TOTLEV(MNUMCR)
REAL         EPACTOT                                                ! Total EPAct92 fleet vehicle sales
REAL         NCSTECH(MNUMCR,MAXCLASS,MAXLDV,MNUMYR)
REAL         NLTECH(MNUMCR,MAXCLASS,MAXLDV,MNUMYR)
REAL         TQLDV(9,MNUMCR,MNUMYR)
REAL         FAVL(MAXLDV,MNUMCR-2,BYR:LYR)
REAL         APSHR11(MAXVTYP,MAXCLASS,MNUMCR-2,3,MNUMYR)
REAL         APSHR22(MAXVTYP,MAXCLASS,MNUMCR-2,MAXLDV,MNUMYR)
REAL         APSHR44(MAXVTYP,MAXCLASS,MNUMCR-2,MAXLDV,MNUMYR)
REAL         APSHR55(MAXVTYP,MAXCLASS,MNUMCR,MAXLDV)
REAL         MPG_ACTUAL(MAXVTYP,MNUMYR)
REAL         AVSALES(maxvtyp,MAXCLASS,MNUMCR,MAXLDV)
REAL         AVSALEST(maxvtyp,MAXCLASS,MNUMCR)
REAL         REG_SHARE_ADJ(MNUMCR-2,MNUMYR)
REAL         EXPENDVEH(2,MNUMYR)
REAL         MPGTECH(MAXLDV,MNUMYR)									!household stock fuel economy by fuel type
REAL         AFVFE(maxvtyp,MAXCLASS,MNUMYR)
REAL         AFVFETOT(maxvtyp,MNUMYR)
REAL         SCMPG(MNUMYR)                                  		! national on-road stock mpg household cars
REAL         STMPG(MNUMYR)                                  		! national on-road stock mpg household light trucks
REAL         CMPG_IT(MAXLDV,MNUMYR)									! household car stock fuel economy by fuel type
REAL         TMPG_IT(MAXLDV,MNUMYR)									! household LT stock fuel economy by fuel type
REAL         CMPGSTK(MAXLDV,MAXAGE,MNUMYR,mnumcr)                   ! regional car stock fuel economy by age
REAL         TTMPGSTK(MAXLDV,MAXAGE,MNUMYR,mnumcr)                  ! regional light truck stock fuel economy
REAL         VMT_STK_HH(maxvtyp,MAXLDV,MAXAGE,MAXHAV,mnumcr)      	! hh vehicle miles traveled car by region derived from stocks
REAL         BVMT_STK_HH(maxvtyp,MAXLDV,MAXAGE,MAXHAV,mnumcr-2)     ! benchmarked hh vehicle miles traveled car by region
REAL         NCS(MNUMCR-2,MAXCLASS,MNUMYR)
REAL         NLTS(MNUMCR-2,MAXCLASS,MNUMYR)
REAL         RSHR(MNUMCR,MNUMYR)									! regional share of total VMT (calculated from VMTLDV)
REAL         LDVSTK(MAXLDV,MNUMYR)                          		! total light duty vehicle stock by ILDV
REAL         VSPLDV(MAXLDV,MNUMYR,MNUMCR-2,MAXVTYP)          		! share of total household travel by ILDV by vehicle type
REAL         TECHNCS(MAXLDV,MNUMYR)
REAL         TECHNLT(MAXLDV,MNUMYR)
REAL         TECHNCSREGN(MNUMCR,MAXLDV,MAXHAV,MNUMYR)       		! new car sales by region, LDV type, HAV type, and year
REAL         TECHNLTREGN(MNUMCR,MAXLDV,MAXHAV,MNUMYR)       		! new lt sales by region, LDV type, HAVE type, and year
REAL         STKCAR(MNUMYR)                                 		! total non-fleet car stock
REAL         STKTR(MNUMYR)                                  		! total non-fleet light truck stock
REAL         VSTK(maxvtyp,MAXLDV)                           		! total light duty vehicle stock 
integer      PassNo               									! Controls two passes for high and low volume sales.
REAL         NVS7SC(mnumcr,MAXGROUP,MAXCLASS,MNUMYR)

! ... Variables for CAFE banking.
real CafeBankA(MAXGROUP),CafeBank(5,MAXGROUP),BankBal(MAXGROUP,byr:lyr),CafeWork(5,MAXGROUP),CafeNeed
common/BankVars/CafeBankA,CafeBank,CafeWork,CafeNeed


common/FPVars/FPrint,TFCoefA,TFCoefB,TFCoefC,TFCoefD,FPMpg, LDV_MPG_CL, FPMpgGrp,Cafe_Used,AltCafe,IBank,&
             CFCoefA,CFCoefB,CFCoefC,CFCoefD,TFCoefA2,TFCoefB2,TFCoefC2,TFCoefD2,&
             CFCoefA2,CFCoefB2,CFCoefC2,CFCoefD2

common/PHEVVars/EG_MPG,EG_MPG20,EG_MPG50,mfg_eg_mpg20,mfg_eg_mpg50,TE_MPG20,TE_MPG50,TG_MPG20,TG_MPG50,PHEVPlug,&
                tot_eg_mpg20,tot_eg_mpg50

real         PerMPG(2,17,MNUMYR),PerSal(2,17,MNUMYR)

!=======================================================================================================
REAL         TOTALSALSC(maxvtyp,MAXCLASS,MAXLDV,MNUMYR)

REAL         CarSales,TrkSales,CarSales2,TrkSales2
!===================================================================================
! ... to accomodate view variables in the Visual Studio, the variables are
! ... added to common blocks here.  Then the USE statement for module T_ must go in every 
! ... subroutine.

  common/trancomreal/ TMC_PGDP, TMC_CPI, NEWCARS, NEWCLS12A, &
INC00$16, ROUNDOFF_ERROR, DEL_FE, DEL_COSTABS, DEL_COSTWGT, &
DEL_WGTABS, DEL_WGTWGT, DEL_HP, COEFF_LEARN, COEFF_LRN1, COEFF_LRN2, YEARS_MKTD, &
LEARN_COST_MULTIPLIER, ADJPEN, VMT, BEN_MG,BEN_JF,BEN_DS,BEN_RS, &									
CLASSBASEYR, FE, WEIGHT, PRICE, PRICEHI, HP, VOLUME, VALUEPERF, PERFFACT, &                         
TANKSIZE, PERFCAP, USEDCAP, MKT_PEN, MKT_MAX, SYNR_DEL, MANDMKSH, DISCOUNT, &                       
REG_COST, CAFE_STAND, REF_MPG,CLASS_SHARE, OCLASS_SHARE, COEF_A, COEF_B, &
COEF_C, COEF_P, COEF_E, AFVADJHP, AFVADJRN, AFVADJFE, AFVADJWT, AFVADJPR, AFVADJPRH, &
NiMH_Cost, PHEV_DOD, EV_DOD, BATTERY, ElecSysIncCost, FuelCell$kW, ElecNonBattCst, &
PHEV20_EVMT, PctPHEV20, PHEV50_EVMT, PctPHEV50, BatPackWgt, BatPackSize, &
PHEV_Credit, IRA_Bat_CRed, IRA_Veh_cred, kWh_Credit, Ttl_Credit,Max_Credit, phevtaxadj, &
FUELCELL, TEC_ORNL, MKT_PENF, AVCOST, MMAVAIL, &
X210, X21, X22, X23, X24, X25, X26, X27, X28, X29, BETAFA2, BETAFA22, &
X11, X31, FAVAIL, PCTFAVAIL, INITSTA, STA_RAT, MAINTCAR, MAINTTRK, LUGGCAR, LUGGTRK, &
VSALES_T, Covered_Sales, ZEV_covered_sales, Credit_Transfer_Rate, zev_credit_req, ZEV_State_Alloc, VMT_CA_CO2, &
ZEV_Requirement, ZEV_Requirement_optional, ZEV_Req_CA_CO2, ZEV_Credit_LDV, California_Credit, ZEV_Credit_Earn, ZEV_Multiplier, Sales_Adjustment, s177_traveling_factor, &  
Adjusted_ZEV_Credit, TZEV_REQ_CA_CO2, CA_shr_of9, CO_shr_of8, VA_shr_of5, MN_shr_of4, Bank_buffer,  zev_basebank, &
SURVFLT, FLTCRAT, FLTTRAT,FLTSSHR,FLTSSHRC,FLTSSHRT, FLTAFSHR, FLTAFSHRC, FLTAFSHRT, FLTCARSHR, FLTTRKSHR, &
FLTVMTYR, FLTTOTMPG, TOTFLTCAR, FLTMPGTOT2, & 
FLTTRANSPC, FLTTRANSLT, SURVP, SURVLT, PVMT, LVMT, VMT_SCHED_PARAM,CDF, LTDF, bank_spending_rate, ZEV_sales_dist, & 
CMPGSTKGAS95, STKAVGWGT, TZEV_sales_dist, &
M_CD_AGE_DIST, F_CD_AGE_DIST,M_CD_AGE_DIST_L, F_CD_AGE_DIST_L, M_CD_AGE_DIST_H, F_CD_AGE_DIST_H,&
AGE_ADJ, AGE_ADJ_L, AGE_ADJ_H,&
BETACOST, BETAINC, BETAVMT, BETAVPLD, BETAEMP, ALPHA, CLTVMTDIST, & 
CLTVINTSHR, CLTSIC, CLTBTUT, CLTGAL, FREFF, HTFREFF, CLTMPG, RPROJ_NCTONMI, RPROJ_CTONMI, &
BCLTBTU, CLTVMTT, NEWCLS2B, RHIST_NCTONMI, RHIST_CTONMI, RAIL_TONMILE, DSHIP_TONMILE, RTM_OUTPUT, &
MILTRSHR90, TMODINIT, TMCOVID, TMPASMIL, TMEFFINIT, QMODFSHR, &
QMODFSHRH, CARLTSHR, NEWLDVPERLD, CARSHARE, trkshare, DUMM, & 
FUELTAX, FUELTAX87, NHTSASAL, NHTSAHP, NHTSAFE, TQDSHIP, &                 
NHTSAWGT, FEMMPG, FEMHP, FEMPRI, FEMPRIH, FEMWGT, FEMRNG, FEMTSZ, FEMVOL, FEMPEN, &
SALESHR, PASSHRR, LTSHRR, NCSTSC, NLTSTSC, AHPCAR, AHPTRUCK, AWTCAR, AWTTRUCK, &
GASMPG_ACTUAL, PMGTR90$, INC90$NP, REGCOST, FUELSAVE, TECHCOST, COSTEF_FUEL, COSTEF_PERF, &
MMAX, ACTUAL_MKT, MKT_FUEL, MKT_PERF, VAL_PERF, ADJFE, DELTA_MKT, REQ_MKT, CFE, &
PRICE_EX, PSLOPE, RATIO_LN, RATIO, GROUPSUM, TOT_MKT, MAX_SHARE, RATIO_BYR, &
RANGE, MAXHISTRNG, OLD_PMAX, SUM_MKS, SUM_MKS_FE, &
FRZNEFF, HIGHEFF, CONSUMER, MUCHE85, EISAMPG, EISAE85, APPLYPHEV, &
PERGRP, WGT, PSPR, LDV_PRI, LDV_RNG, FPRICE, VRNG, FLCOST, BRCOST25, ACCL, HFUEL, MAINT, LUGG, MPGHH, PE, IE, &
VMTLD, VPLD, LDVCOVID, VMTLDHISTYR, LICDRIVER, LIC_TREND, LIC_ELAS, LIC_MAX, &
LICRATE_M, LICRATE_F, LicRHistYr, DE, MPGT, MPGC, &
CDFRFG, LTDFRFG, TTMPGLDV, CCMPGLDV, FLTMPG, TLDVMPG, TOTLEV, EPACTOT, NCSTECH, &
NLTECH, TQLDV, BTQLDV, BTQISHIPR, BTQDSHIPR, BTQRAILR, BQJETR, BQAGR, &
BQMILTR, BQMODR, BQRECR, BQLUBR, BFLTFUELBTU, BTQFREIRSC, BFVMTECHSC, BVMTECH, BFLTVMTECH, &
BASMDEMD, BRTMTT, FAVL, APSHR11, APSHR22, APSHR44, APSHR55,&
MPG_ACTUAL, AVSALES, AVSALEST, FLTVMTECH, MPGFLTSTK, FLTFUELBTU,  &
FLTECH, OLDFSTK, TFLTECHSTK, FLTECHSAL, FLT_STOCK, EXPENDVEH, TQFRAILT, &
SSURV25, QMILTR, QMTRR, QMTBR, QRECR, &
MFD, TMFD, RECFD, LUBFD, TMOD, TMEFF, MPGTECH, AFVFE, AFVFETOT, SCMPG, &
STMPG, CMPG_IT, TMPG_IT, CMPGSTK, TTMPGSTK, VMT_STK_HH, BVMT_STK_HH, &
RTMTT, TQRAILR, NCS, NLTS, RSHR, LDVSTK, VSPLDV, &
TECHNCS, TECHNLT, STKCAR, STKTR, VSTK, VMTHH, VMTLDV, COSTMI, TOTALSALSC, FLTSALSC, &
CarSales,TrkSales,CarSales2,TrkSales2, fltstockr, &
CARFLTSTKREGN, LTFLTSTKREGN, CLTSTKREGN, TECHNCSREGN, TECHNLTREGN, &
CLTSALESHIST, CLTVMT_H, CLTVMT2012, CLTMPG2012, CLTVMTVA, &
CLS2bSTKHIST, NCLTMPG, cltfbtu, CLTSALESPER, &
SEDS_tran, MER_tran, carltshr_regn, CCONSTANT, CRHO, CINC, CFUEL, CHP, CWGT, CMPG, CDUMM, &
TCONSTANT, TRHO, TINC, TFUEL, THP, TWGT, TMPG, TDUMM, SaleShr_regn, &
truempg_regn, LDV_STOCK
			

common/tran_easy/tmgtcbus,tdstcpus,tjftcbus,trftcbus

  common/trancomint4/ iy, num_to_read, First_Read_Year,    & ! integer 4
                     FRTEFF,TRANEFF,CAFEMEET,TRANAB32, &
					 ymer, ysteo

  common/trancomint2/YRS,N, LASTID, ICL,IGP,IVTYP,ITECH,   & ! integer 2 
INOTE,IATV,IYR, ILDV,IPEV, ifuel, IFLEET,                       & 
IREGN,IRAILREGN,IAGE, I,J,K, SIGN, NUMTECH,                &
SYNERGY, MANDYEAR, NUM_REQ,NUM_SUP,NUM_MAN,NUM_SYN,        &
PAYBACK, NHTSALYR, O_UNIT,PASS, IHAV, RETURN_STAT

  common/trancomlog/ TECH_APPLIC, CLASSFLAG, MAND_ORIDE, PRINT_FE,PRINT_TECH,PRINT_DIAG, &
MMSWITCH, CAFEPASS, REQUIRED
  common/trancomchar/ TECHLABEL, SYS_AFFECT, GROUPLABEL, CLASSLABEL, FF 

  common/morevars/PassNo

common/JAHVar/PerMPG,PerSal,nvs7sc

!...Map for how groups are defined back to vehicle types: cars (1) and light trucks (2)
    data GrpMap/1,1,1,1,1,2,2,2,2,2,2/
!...Map for how groups are defined back to domestic (=1) and import (=2) (CAVs are considered domestic) 
    data GrpDIMap/1,2,2,2,1,1,1,1,2,2,1/

	
!...HMS-These are being added for the hydrogen market segment project.
!...The 3 should be replaced with a parameter. It is for large city, small city, and rural.
real  HPrice(3,mnumcr,mnumyr)  !Hydrogen price in cents per gallon.
real  MSSplit(3,mnumcr,mnumyr)  !Split of vehicle sales between large city, small city, and rural.
real  MSStkS(3,mnumcr)  !Shares of vehicle stocks for 3 types.
real  HFCost(3,maxvtyp,MAXCLASS,mnumcr)  !Hydrogen fuel cost, from price and mpg.
real  HAPShr44(3,maxvtyp,MAXCLASS,mnumcr,MAXLDV)  !ATV technology shares by market share.
real  HVSales(3,maxvtyp,MAXCLASS,mnumcr)  !Hydrogen vehicle sales.
real  HVStkV(3,maxvtyp,MAXCLASS,mnumcr-2,maxage,mnumyr)  !Hydrogen vehicle vintaged stocks.
real  HVStkT(3,mnumcr)  !Hydrogen vehicle total stocks by regin.
real  HVStkS(3,mnumcr,mnumyr)  !Hydrogen vehicle stock shares by region and year.
integer  HMSLoop,hms
common/HYVars1/HPrice,MSSplit,MSStkS,HFCost,HAPShr44,HVSales,HVStkV, &
 HVStkT,HVStkS,HMSLoop,hms

! ... HMS-Some additional variables for the user specified changes to hydrogen vehicle price.
integer HFSAvil(3)
real HFSAvilx(3)
real HFAvil(3,mnumyr)
common/HYVars2/HFSAvil,HFSAvilx,HFAvil

! ... Variables to extend the VMT shares to take account of hydrogen.
real RShrH(11,mnumyr),RShrE(11,mnumyr)
common/RShrVars/RShrH,RShrE      

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
! INTEGER*2 :: first_hav_year(maxhav-1)					!...First year HAVs (4a, 4b, 5) are available
! REAL :: hav_init_cost(maxhav-1)							!...HAV system initial cost for 4a, 4b, and 5, in 2015$
INTEGER*2 :: hav_lidar_map(maxhav-1)					!...lidar system id (1:maxlidar) used by HAV vehicles 4a [1], 4b [2], and 5 [3]
REAL :: HAV_sys_lrn(maxhav-1, BYR:LYR)					!...HAV system time-based cost reductions for 4a [1], 4b [2], and 5 [3]
! REAL :: hav_sys_wgt(maxhav-1)							!...HAV system weight for 4a, 4b, and 5; weight of sensors, support structure, wiring, computer, data storage, etc
REAL :: hav_sys_cost(maxhav, BYR:LYR)=0.0				!...HAV system costs for 4a [1], 4b [2], and 5 [3]
REAL :: hav_techmap(maxhav-1)							!...Maps HAV levels to tech matrix indices (e.g. 4a:90, 4b:91, 5:92)


!...Fleet HAV adoption variables (FLTHAV)
REAL :: taxi_rev_params(maxhav,6)			! fleet adoption model parameter block:	
REAL :: taxi_mi_life(maxhav)				!	taxi lifetime miles
REAL :: taxi_idle_gph(maxhav)				!	taxi idle fuel rate, gallons/hr
REAL :: taxi_maint_cost(maxhav)				!	taxi maintenance costs, monthly component, 2015$
REAL :: taxi_data_fee(maxhav)				!	HAV data fee per month, 2015$
REAL :: taxi_insur(maxhav)					!	taxi insurance fee per month, 2015$
REAL :: hav_mpg_fact(maxhav)				!	MPG multiplier for HAVs; applied to degradation factors cdfrfg and ltdfrfg
REAL :: hav_mpgdeg(maxhav, BYR:LYR)			!   MPG multiplier for HAVs; applied to degradation factors cdfrfg and ltdfrfg and used in taxi AV adoption decision.

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

!...Get the market shares from HMM.
    do i=1,mnumcr
!...  Make sure MShare is nonzero even if the HMM is off
      if(MShare(i,1).EQ.0.0) then
         MShare(i,1) = 0.48
         MShare(i,2) = 0.45
         MShare(i,3) = 0.07
      endif
      
      do hms=1,3        
        MSStkS(hms,i)=MShare(i,hms)
        MSSplit(hms,i,curiyr)=MSStkS(hms,i)
      end do
    end do

!...TRANEFF switches on alternate scenarios  
    TRANEFF    = RTOVALUE('TRANEFF ',0)
    TRANAB32   = RTOVALUE('AB32SW  ',1)
    FRZNEFF    = 0.0
    HIGHEFF    = 0.0
    CONSUMER   = 0.0
    MUCHE85    = 0.0
    EISAMPG    = 0.0
    EISAE85    = 0.0
    FRTEFF     = 0.0
    APPLYPHEV  = 0.0
    PHEVSTIM   = 0.0
	IRA_STIM   = 0.0
    MOREFFV    = 0.0
    CAFECASE   = 0
    LVPRCADJ   = 0

!...CAFEMEET (=1) Natural Gas Vehicle side cases
    CAFEMEET  = RTOVALUE('CAFEMEET',0)

if(cafemeet.eq.1) frteff=3    !...expanded natural gas market, permanent natural gas incremental cost tax credit, new logistic function, 1990$
if(cafemeet.eq.2) frteff=4    !...expanded natural gas market, temporary natural gas incremental cost tax credit, new logistic function, 1990$  

!...Energy Independence and Security Act (EISA) of 2007  
    NRG2007 = RTOVALUE('NRG2007 ',0)
    if(NRG2007.ge.1)then
      EISAE85 = 1.0
    endif
	
!...Energy Improvement and Extension Act (EIEA) of 2008
    NRG2008 = RTOVALUE('NRG2008 ',0)
    if(NRG2008.eq.1)then
      APPLYPHEV = 1.0
    endif

!...2009 Stimulus Package
    STIMULUS = RTOVALUE('STIMULUS',0)
    if(STIMULUS.eq.1)then
      PHEVSTIM =1.0        
    endif

!...2022 IRA tax credits
    LEGIRA = RTOVALUE('LEGIRA  ',0)
    if(LEGIRA.gt.0)then
      IRA_STIM = 1.0
    endif	
	
!...Increase FFV sales to help E-85 price convergence
    MOREFFV = 1.0
    
!...Switch to turn off(0)/on(1) for frozen technology scenario
    if(TRANEFF.eq.1) FRZNEFF = 1.0

!...Switch to turn off(0)/on(1) for high tech/efficiency scenario
    if(TRANEFF.eq.2)then 
      HIGHEFF = 1.0
    endif
!...Switch to turn off(0)/on(1) for high efficiency scenario /fe(2) for frozen technology
    if(HIGHEFF.eq.1) FRTEFF = 1
    if(FRZNEFF.eq.1) FRTEFF = 2

!...Switch to turn on agressive conventional technology adoption scenario
    if(TRANEFF.eq.3) CONSUMER = 1.0

!...Switch 
!    if(TRANEFF.eq.4) 

!...Switch to turn on Phase 2 HDV standards
    if(TRANEFF.eq.5) FRTEFF = 5
	
!...Switch to turn on CAFECASE adjustments
    if(TRANEFF.eq.6) CAFECASE = 1
    
!...Switch to turn on aggressive E-85 demand (revised logit)
    if(TRANEFF.eq.7) MUCHE85 = 1.0

!...Switch to turn on CAFECASE adjustments
    if(TRANEFF.eq.8) then
      CAFECASE = 1
      LVPRCADJ = 1
    endif

    YRS = CURCALYR
    N = CURIYR

    OPEN(21,FILE='TRNOUT.TXT')

    if(DO_ONCE.eq.0) then
      DO_ONCE=1
      CALL READLDV
	  CALL READSTOCK
    endif
    CALL TMAC
    CALL NEWLDV
    CALL TMPGNEW
    if (yrs.ge.MINVAL(first_lidar_year)) then 		! if lidar is available, determine taxi / ride-hail fleet adoption of HAVs
	    CALL FLTHAV
    endif
    CALL TFLTVMTS
    CALL TSMOD
    CALL TMPGSTK
    CALL TCURB
    CALL TFLTMPGS
    CALL TFLTCONS
    CALL TRANFRT(FRTEFF,0)
    CALL TVMT
    CALL TMPGAG
    CALL TCOMMCL_TRK
    CALL TRAIL
    CALL TSHIP
    CALL TRANAIR
    CALL TMISC
    CALL TCONS
    CALL TINTEG
    CALL TBENCHMARK
    CALL TEMISS
    CALL TREPORT
	CALL IEOVARS

!...Calculate total highway vehicle CNG demand    
!...QGFTR: total transportation natural gas demand
!...QGFTRFV: CNG central refueling
!...QGFTRPV: CNG retail purchase
!...QGLTRFV: LNG central refueling
!...QGLTRPV: LNG retail refueling

    do iregn=1,mnumcr
      QGFTR(iregn,n)   = QNGTR(iregn,n)
!...Calculate retail CNG demand...heavy-duty truck demand populated in tranfrt.f
	  QGFTRPV(iregn,n) = QGFTRPV(iregn,n) + ((PCTAF(3,iregn,n)*sum(VMTHH(N,iregn,9,1:maxvtyp))/MPGTECH(9,n)+ &
		sum(VMTHH(N,iregn,11,1:maxvtyp))/MPGTECH(11,n)) * MG_HHV) * BENNG(iregn,n)	

!...Calculate total fleet vehicle (light and heavy central refueling) NG demand
!...Add H2 consumption to NG consumption if the HMM is off
      if(EXH.eq.1) then
        QGFTRFV(iregn,n) =  QGFTRFV(iregn,n) + (FLTFUELBTU(iregn,4,n)) * BENNG(iregn,n)
      else	
		QGFTRFV(iregn,n) =  QGFTRFV(iregn,n) + (FLTFUELBTU(iregn,4,n)+(FLTFUELBTU(iregn,7,n)/0.7) + &
                           QMTBR(1,5,IREGN,N) +       & ! transit bus   
                           QMTBR(2,5,IREGN,N) +       & ! intercity bus  
                           QMTBR(3,5,IREGN,N) +       & ! school bus 
                            (sum(VMTHH(N,iregn,14,1:maxvtyp))/MPGTECH(14,n)) * MG_HHV/0.7) * BENNG(iregn,n)
      endif
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

!...for cafe cases adjust incremental vehicle price to account for discounted fuel savings
    if(lvprcadj.eq.1.and.n.ge.36)then 
      do IVTYP=1,maxvtyp
        Saved_$(IVTYP,n) = 0.0
        do i=1,5
          if(IVTYP.eq.1) VMT(i,mnumcr)=PVMT(i,n,mnumcr,1)	!THIS ASSUMES gas ICE VMT schedule, not EV/PHEV/Diesel/HEV schedules
          if(IVTYP.eq.2) VMT(i,mnumcr)=LVMT(i,n,mnumcr,1)
          fuel_n=n+i-1
          Fuel_$(I) = PMGTR(11,fuel_n)* TMC_PGDP(1) * MG_HHV/1000.0
          Saved_$(IVTYP,n) = Saved_$(IVTYP,n) + ((VMT(i,mnumcr) * (1/REF_MPG(IVTYP,n) - 1/TrueMPG(IVTYP,n)) * &
                           Fuel_$(i)) * ((1.07)**(-i)))
        enddo
      enddo

      Full_Prc_Car(curiyr)=Avg_Prc_Car(curiyr)-(Saved_$(1,n)/1000.0) 
      Full_Prc_Trk(curiyr)=Avg_Prc_Trk(curiyr)-(Saved_$(2,n)/1000.0)     
      Full_Prc_Veh(curiyr)=Full_Prc_Car(curiyr)*(Carsales/(CarSales+TrkSales))+Full_Prc_Trk(curiyr)*(TrkSales/(CarSales+TrkSales))
    endif
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
!!   flt_hav_shares(mnumcr, maxvtyp, MAXCLASS, maxldv, BYR:LYR,,maxhav)
!!   taxi_sales(mnumcr,maxvtyp,MAXCLASS,maxldv,BYR:LYR,maxhav)
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
		write (21,'(i4, 2x, 4f8.4)') i, tshr(:)
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
	write (21,*) 'Fleet HAV total VMT for all vtypes and ildvs:'		! Testing HAV VMT; added ihav dimension  --MDR
	do i=1995, 2050
	    write (21,'(i4, 4f13.0, 4f13.0, 4f13.0, 4f13.0)') i, sum(FLTVMTHAV(:,4,:,1,i)), sum(FLTVMTHAV(:,4,:,2,i)), sum(FLTVMTHAV(:,4,:,3,i)), sum(FLTVMTHAV(:,4,:,4,i))				!FLTVMTHAV(IVTYP,ifleet,ILDV,ihav,yrs)
	enddo
	
	write (21,*) 'Fleet HAV total stock for cars and trucks, taxis (fleet=4), all fuels, all ages (1:maxage) each HAV level:'		! Testing HAV stock  --MDR
	do i=1995, 2050
		write (21,'(i4, 4f8.0, 4f8.0, 4f8.0, 4f8.0)') i, sum(Flt_Stock(:,:,4,:,:,1,i-1989)), sum(Flt_Stock(:,:,4,:,:,2,i-1989)), sum(Flt_Stock(:,:,4,:,:,3,i-1989)), sum(Flt_Stock(:,:,4,:,:,4,i-1989))
	enddo
	
	write (21,*) 'Fleet HAV total sales for cars (IVTYP=1), taxis (fleet=4), gasoline (ILDV=1), all AV levels (0-3, 4a, 4b, 5):'		! Testing HAV sales  --MDR
	write (21,'(i4, 4f8.0)') i, FLTECH(mnumcr,1,4,1,:)
!!	FLTECHSAL(IVTYP,ifleet,ICL,ILDV,ihav)
!!	FLTECH(IVTYP,ifleet,ILDV,ihav)
!   ********************************************************************************************
	
    write(H2UNIT,'(/,a)') 'NHTSA Sales Data by Manufacturer Group and Vehicle Class (NHTSASal)'
    write(H2UNIT,'(11x,36i8)') (ymap(ix),ix=1,tmap)
    do IGP=1,MAXGROUP
      it=GrpMap(IGP)
      write(H2UNIT,'(a)') GrpLab(IGP)
        do ICL=1,MAXCLASS
          write(H2UNIT,'(2x,a,36f8.1)') ClsLab(ICL,it),(NHTSASal(ymap(ix),ICL,IGP),ix=1,tmap)
        end do
    end do

    write(H2UNIT,'(/,a)') 'Class Shares by Manufacturer Group (Class_Share)'
    write(H2UNIT,'(11x,36i8)') (ymap(ix),ix=1,tmap)
    do IGP=1,MAXGROUP
      it=GrpMap(IGP)
      write(H2UNIT,'(a)') GrpLab(IGP)
        do ICL=1,MAXCLASS
          write(H2UNIT,'(2x,a,36f8.4)') ClsLab(ICL,it),(class_share(mnumcr,ICL,IGP,ymap(ix)),ix=1,tmap)
        end do
    end do

    write(H2UNIT,'(/,a)') 'Overall Class Shares by Manufacturer Group (OClass_Share)'
    write(H2UNIT,'(11x,36i8)') (ymap(ix),ix=1,tmap)
    do IGP=1,MAXGROUP
      it=GrpMap(IGP)
      write(H2UNIT,'(a)') GrpLab(IGP)
      do ICL=1,MAXCLASS
        write(H2UNIT,'(2x,a,36f8.4)') ClsLab(ICL,it),(oclass_share(mnumcr,ICL,IGP,ymap(ix)),ix=1,tmap)
      end do
    end do

    write(H2UNIT,'(/,a)') 'Group Shares Within Types by Vehicle Class (PerGrp)'
    write(H2UNIT,'(11x,36i8)') (ymap(ix),ix=1,tmap)
    do IGP=1,MAXGROUP
      it=GrpMap(IGP)
      write(H2UNIT,'(a)') GrpLab(IGP)
      do ICL=1,MAXCLASS
        write(H2UNIT,'(2x,a,36f8.4)') ClsLab(ICL,it),(PerGrp(IGP,ICL,nmap(ix)),ix=1,tmap)
      end do
   end do

   write(H2UNIT,'(/,a)') 'Group Shares Within Types Over All Vehicle Classes (SaleShr)'
   write(H2UNIT,'(11x,36i8)') (ymap(ix),ix=1,tmap)
   do IGP=1,MAXGROUP
     it=GrpMap(IGP)
     write(H2UNIT,'(2x,a,36f8.4)') GrpLab(IGP),(SaleShr(mnumcr,IGP,nmap(ix)),ix=1,tmap)
   end do

   write(H2UNIT,'(/,a)') 'projection Sales of Non-Fleet (Personal) Vehicles by Manufacturer Group and Vehicle Class (nvs7sc)'
   write(H2UNIT,'(11x,36i8)') (ymap(ix),ix=1,tmap)
   do IGP=1,MAXGROUP
     it=GrpMap(IGP)
     write(H2UNIT,'(a)') GrpLab(IGP)
     do ICL=1,MAXCLASS
       write(H2UNIT,'(2x,a,36f8.1)') ClsLab(ICL,it),(nvs7sc(mnumcr,IGP,ICL,nmap(ix))*1000.0,ix=1,tmap)
     end do
   end do

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

! Sales model variables
REAL          mg_car_share(mnumcr-2,cargrp)
REAL          mg_lt_share(mnumcr-2,ltkgrp)

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
INTEGER*2     VEHBASEYEAR(MAXGROUP*MAXCLASS)       ! vehicle class first year of sales
REAL          VEHFE(MAXGROUP*MAXCLASS)             ! vehicle class base fuel economy
REAL          VEHWEIGHT(MAXGROUP*MAXCLASS)         ! vehicle class base curb weight
REAL          VEHPRICE(MAXGROUP*MAXCLASS)          ! vehicle class base price
REAL          VEHHP(MAXGROUP*MAXCLASS)             ! vehicle class base horsepower
REAL          VEHVOLUME(MAXGROUP*MAXCLASS)         ! vehicle class base interior volume
REAL          VEHPERFVAL(MAXGROUP*MAXCLASS)        ! vehicle class base performance value
REAL          VEHPERFFAC(MAXGROUP*MAXCLASS)        ! vehicle class base performance factor
REAL          VEHTANKSIZE(MAXGROUP*MAXCLASS)       ! vehicle class base fuel tank size
REAL          VEHPERFCAP(MAXGROUP*MAXCLASS)        ! vehicle class performance cap
REAL          VEHUSEDCAP(MAXGROUP*MAXCLASS)        ! fraction of vehicle class performance cap used since 1990
INTEGER*2     MKTGROUP(MAXGROUP*MAXTECH)                  ! vehicle group number
INTEGER*2     MKTTECH(MAXGROUP*MAXTECH)                   ! technology number
REAL          TECHMKTSHARE(MAXGROUP*MAXTECH,MAXCLASS*2)   ! technology base and maximum market shares
INTEGER*2     ENGNOTETYPE(MAXNOTE)                 ! engineering note type
INTEGER*2     ENGNOTETECH(MAXNOTE,10)              ! engineering note technology ID #1-10
INTEGER*2     ENGNOTEYEAR(MAXNOTE)                 ! engineering note year
REAL          ENGNOTEPCT(MAXNOTE)                  ! engineering note percent affected
REAL          FPrt(MAXGROUP*MAXCLASS,MNUMYR)       ! Reads in footprints by group and class

REAL          TFCOEFNRG(4,MNUMYR)                  ! 4 coefficients for EISA07 lt footprint CAFE
REAL          CFCOEFNRG(4,MNUMYR)                  ! 4 coefficients for EISA07 car footprint CAFE
REAL          TFCOEFGHG(4,MNUMYR)                  ! 4 coefficients for GHG lt footprint CAFE
REAL          CFCOEFGHG(4,MNUMYR)                  ! 4 coefficients for GHG car footprint CAFE
INTEGER       PRNTFET                              ! print fem results
INTEGER       PRNTECHT                             ! print market share results
INTEGER       PRNTDGT                              ! print diagnostic results
INTEGER*2     FRSTYEARX(MAXTECH,MAXGROUP)          ! Temp placeholder for first year of technology introduction
INTEGER*2     CAFEYEAR(MNUMYR)                     ! CAFE year
REAL          CAFE_STD(MAXVTYP,6:MNUMYR)           ! CAFE standards
REAL          ATVHP(MAXATV)                        ! ATV horsepower differential
REAL          ATVRN(MAXATV)                        ! ATV range differential
REAL          ATVFE(MAXATV)                        ! ATV fuel economy differential
REAL          ATVWT(MAXATV)                        ! ATV weight differential
REAL          ATVPR1(MAXATV)                       ! ATV low volume car price differential
REAL          ATVPR2(MAXATV)                       ! ATV low volume truck price differential
REAL          ATVPR3(MAXATV)                       ! ATV high volume car price differential
REAL          ATVPR4(MAXATV)                       ! ATV high volume truck price differential

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
REAL          CELLGCST(MNUMYR)                     ! gasoline fuel cell cost learning curve
CHARACTER*3   MMKEY                                ! indicator of whether to use input or internal ATV make/model data
REAL          MAKEMOD(MAXLDV,MNUMYR,MAXVTYP)       ! Car and light-truck ATV make/model availability
REAL          NMLMCOEFCAR(MAXNMLM,MAXCLASS)        ! ATV nested multinomial logit model coefficients for cars
REAL          NMLMCOEFTRK(MAXNMLM,MAXCLASS)        ! ATV nested multinomial logit model coefficients for trucks
REAL          NMLMCOEF(MAXNMLM,MAXVTYP,MAXCLASS)   ! ATV nested multinomial logit model coefficients
integer       ix
 
! debug output of trnldvx input: each variable written to unit xmlout, (trnldvx.txt in this case) when xmlout<>0
     common/nemswk1/xmlout
     integer xmlout
     call unitunopened(300,999,xmlout)  ! get unused unit number
     open(xmlout,file='trnldvx.txt',status='unknown')
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

!...Read in data for light duty VMT by vintage

      CALL GETRNGR('VMT             ',VMT,12,1,1)                 ! VMT by Vehicle Vintage (12 years) for FEM

!...Read in, validate, and store for global reference the LDV/LDT base year attributes

      CALL GETRNGC('VEHGROUPLABEL   ',VEHGROUPLABEL,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Group Label
      CALL GETRNGI('VEHGROUP        ',VEHGROUP     ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Group Number
      CALL GETRNGC('VEHCLASSLABEL   ',VEHCLASSLABEL,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Label
      CALL GETRNGI('VEHCLASS        ',VEHCLASS     ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Number
      CALL GETRNGI('VEHBASEYEAR     ',VEHBASEYEAR  ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class First Year of Sales
      CALL GETRNGR('VEHFE           ',VEHFE        ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Base Fuel Economy
      CALL GETRNGR('VEHWEIGHT       ',VEHWEIGHT    ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Base Curb Weight
      CALL GETRNGR('VEHPRICE        ',VEHPRICE     ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Base Price
      CALL GETRNGR('VEHHP           ',VEHHP        ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Base Horsepower
      CALL GETRNGR('VEHVOLUME       ',VEHVOLUME    ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Base Interior Volume
      CALL GETRNGR('VEHPERFVAL      ',VEHPERFVAL   ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Base Performance Value
      CALL GETRNGR('VEHPERFFAC      ',VEHPERFFAC   ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Base Performance Factor
      CALL GETRNGR('VEHTANKSIZE     ',VEHTANKSIZE  ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Base Fuel Tank Size
      CALL GETRNGR('VEHPERFCAP      ',VEHPERFCAP   ,1,MAXGROUP*MAXCLASS,1)    ! Vehicle Class Performance Cap
      CALL GETRNGR('VEHUSEDCAP      ',VEHUSEDCAP   ,1,MAXGROUP*MAXCLASS,1)    ! Fraction of Vehicle Class Perf Cap Used Since 1990

      DO IGP=1,MAXGROUP
        DO ICL=1,MAXCLASS
          K = ((IGP-1)*MAXCLASS) + ICL
          IF (VEHGROUP(K) .NE. IGP) STOP 110
          IF (ICL .EQ. 1) GROUPLABEL(IGP) = VEHGROUPLABEL(K)
          IF (VEHCLASS(K) .NE. ICL) STOP 111
          CLASSLABEL(ICL,IGP)        = VEHCLASSLABEL(K)
          CLASSBASEYR(ICL,IGP)       = VEHBASEYEAR(K)
          CLASSFLAG(ICL,IGP,GAS)     = .FALSE.
          IF (CLASSBASEYR(ICL,IGP) .LE. XYR) CLASSFLAG(ICL,IGP,GAS) = .TRUE.
          FE(ICL,IGP,BASE,GAS)       = VEHFE(K)
          WEIGHT(ICL,IGP,BASE,GAS)   = VEHWEIGHT(K)
		  PRICE(ICL,IGP,BASE,GAS)    = VEHPRICE(K) * (MC_JPGDP(1)/MC_JPGDP(XYR-BYR+1)) ! converting from base year +1 to 1990$
          PRICEHI(ICL,IGP,BASE,GAS)  = PRICE(ICL,IGP,BASE,GAS)
          HP(ICL,IGP,BASE,GAS)       = VEHHP(K)
          VOLUME(ICL,IGP,BASE,GAS)   = VEHVOLUME(K)
          VALUEPERF(ICL,IGP)         = VEHPERFVAL(K)
          PERFFACT(ICL,IGP)          = VEHPERFFAC(K)
          TANKSIZE(ICL,IGP,BASE,GAS) = VEHTANKSIZE(K)
          PERFCAP(ICL,IGP)           = VEHPERFCAP(K)
          USEDCAP(ICL,IGP)           = VEHUSEDCAP(K)
        ENDDO;
      ENDDO;

! ... Read in and store for global reference various miscellaneous parameters

      CALL GETRNGI('PBACKT          ',PAYBACK ,1,1,1)    !Payback period 
      CALL GETRNGR('DRATET          ',DISCOUNT,1,1,1)    !Discount rate 

      DISCOUNT = DISCOUNT * 0.01

      CALL GETRNGR('FINET           ',REG_COST,1,1,1)     ! CAFE fine
      CALL GETRNGI('PRNTFET         ',PRNTFET ,1,1,1)     ! Print FEM results
      CALL GETRNGI('PRNTECHT        ',PRNTECHT,1,1,1)     ! Print market share
      CALL GETRNGI('PRNTDGT         ',PRNTDGT ,1,1,1)     ! Print diagnostic
      PRINT_FE   = .FALSE.
      PRINT_TECH = .FALSE.
      PRINT_DIAG = .FALSE.
      IF (PRNTFET  .EQ. 1) PRINT_FE   = .TRUE.
      IF (PRNTECHT .EQ. 1) PRINT_TECH = .TRUE.
      IF (PRNTDGT  .EQ. 1) PRINT_DIAG = .TRUE.

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

!...Read in and store the vehicle footprints and the coefficients for the footprint-based cafe standards
    CALL GETRNGR('TFCOEFNRG       ',TFCOEFNRG(1:4,19:IJUMPYR),4,IJUMPYR-18,1)
    CALL GETRNGR('CFCOEFNRG       ',CFCOEFNRG(1:4,19:IJUMPYR),4,IJUMPYR-18,1)
    CALL GETRNGR('TFCOEFGHG       ',TFCOEFGHG(1:4,23:IJUMPYR),4,IJUMPYR-22,1)
    CALL GETRNGR('CFCOEFGHG       ',CFCOEFGHG(1:4,23:IJUMPYR),4,IJUMPYR-22,1)      
    CALL GETRNGR('FPRT            ',FPRT(1:MAXGROUP*MAXCLASS,19:ijumpyr),MAXGROUP*MAXCLASS,ijumpyr-18,1)
	
	CALL GETRNGR('AC_OC_CREDIT    ',AC_OC_CREDIT(1:MAXGROUP,iy:ijumpyr),MAXGROUP,num_to_read,1)

!...Copy foot print coefficients (EISA) into separate coefficient variables and FPRNT into groups by classes
    do i=19,ijumpyr
      CFCoefA(i)=CFCOEFNRG(1,i)
      CFCoefB(i)=CFCOEFNRG(2,i)
      CFCoefC(i)=CFCOEFNRG(3,i)
      CFCoefD(i)=CFCOEFNRG(4,i)
      TFCoefA(i)=TFCOEFNRG(1,i)
      TFCoefB(i)=TFCOEFNRG(2,i)
      TFCoefC(i)=TFCOEFNRG(3,i)
      TFCoefD(i)=TFCOEFNRG(4,i)
      ix=0
      do IGP=1,MAXGROUP
        do ICL=1,MAXCLASS
          ix=ix+1
          FPrint(ICL,IGP,i)=FPRT(ix,i)
        enddo
      enddo
    enddo

!...Copy foot print coefficients into separate coefficient variables for GHG CAFE standards
    do i=23,ijumpyr  
      CFCoefA2(i)=CFCOEFGHG(1,i)
      CFCoefB2(i)=CFCOEFGHG(2,i)
      CFCoefC2(i)=CFCOEFGHG(3,i)
      CFCoefD2(i)=CFCOEFGHG(4,i)
      TFCoefA2(i)=TFCOEFGHG(1,i)
      TFCoefB2(i)=TFCOEFGHG(2,i)
      TFCoefC2(i)=TFCOEFGHG(3,i)
      TFCoefD2(i)=TFCOEFGHG(4,i) 
    enddo

! ... Read in and store for global reference basic Advanced Technology Vehicle parameters

      CALL GETRNGR('COEF_A          ',COEF_A,MAXCLASS,MAXGROUP,1)     ! elasticity for time by veh group
      CALL GETRNGR('COEF_B          ',COEF_B,MAXCLASS,MAXGROUP,1)     ! elasticity for fuel price by veh group
      CALL GETRNGR('COEF_C          ',COEF_C,MAXCLASS,MAXGROUP,1)     ! elasticity for income by veh group
      CALL GETRNGR('COEF_P          ',COEF_P,MAXCLASS,MAXGROUP,1)     ! elasticity for veh price by veh group
      CALL GETRNGR('ATVHP           ',ATVHP ,1,MAXATV,1)              ! change in horsepower
      CALL GETRNGR('ATVRN           ',ATVRN ,1,MAXATV,1)              ! change in range
      CALL GETRNGR('ATVFE           ',ATVFE ,1,MAXATV,1)              ! change in mpg
      CALL GETRNGR('ATVWT           ',ATVWT ,1,MAXATV,1)              ! change in weight
      CALL GETRNGR('ATVPR1          ',ATVPR1,1,MAXATV,1)              ! change in price low volume car
      CALL GETRNGR('ATVPR2          ',ATVPR2,1,MAXATV,1)              ! change in price low volume lt trk
      CALL GETRNGR('ATVPR3          ',ATVPR3,1,MAXATV,1)              ! change in price high volume car
      CALL GETRNGR('ATVPR4          ',ATVPR4,1,MAXATV,1)              ! change in price low volume lt trk
	  
	  
! ... Read in break out of manufactuer group size class by census division  
	  CALL GETRNGR('mg_car_share    ',mg_car_share,mnumcr-2,cargrp,1)
	  CALL GETRNGR('mg_lt_share     ',mg_lt_share,mnumcr-2,ltkgrp,1)
	  
      do iregn = 1,mnumcr-2
		do IGP = 1,cargrp
		  SaleShr_regn(iregn,IGP) = mg_car_share(iregn,IGP)
		enddo
		do IGP = 1,ltkgrp
		  SaleShr_regn(iregn,IGP+cargrp) = mg_lt_share(iregn,IGP)
	    enddo
      enddo

! ... The following parameters indicate the availablity of advanced technology vehicles by group and class

      CALL GETRNGI('CARFLG          ',CARFLG,MAXATV,MAXCLASS,1)
      CALL GETRNGI('TRKFLG          ',TRKFLG,MAXATV,MAXCLASS,1)

! ... Set gasoline vehicle differentials to zero and then assign values for other vehicles

      AFVADJHP(GAS,XYR)   = 0.0
      AFVADJRN(GAS,XYR)   = 0.0
      AFVADJFE(GAS,XYR)   = 0.0
      AFVADJWT(GAS,XYR)   = 0.0
      AFVADJPR(GAS,1,XYR) = 0.0
      AFVADJPR(GAS,2,XYR) = 0.0
      AFVADJPRH(GAS,1,XYR) = 0.0
      AFVADJPRH(GAS,2,XYR) = 0.0

      DO ILDV=2,MAXLDV
        AFVADJHP(ILDV,XYR)   = ATVHP(ILDV-1)
        AFVADJRN(ILDV,XYR)   = ATVRN(ILDV-1)
        AFVADJFE(ILDV,XYR)   = ATVFE(ILDV-1)
        AFVADJWT(ILDV,XYR)   = ATVWT(ILDV-1)
        AFVADJPR(ILDV,1,XYR) = ATVPR1(ILDV-1)
        AFVADJPR(ILDV,2,XYR) = ATVPR2(ILDV-1)
        AFVADJPRH(ILDV,1,XYR) = ATVPR3(ILDV-1)
        AFVADJPRH(ILDV,2,XYR) = ATVPR4(ILDV-1)
      ENDDO

! ... Read in and store for global reference learning cost curves for batteries and fuel cells

      CALL GETRNGR('NIMHCOST        ',NIMHCOST(iy:IJUMPYR),Num_to_Read,1,1) 
      CALL GETRNGR('LIONCOST        ',LIONCOST(iy:IJUMPYR),Num_to_read,1,1)							!generic li-ion cost curve 
      CALL GETRNGR('LIONCOST_PHEV   ',LIONCOST_PHEV(iy:IJUMPYR),Num_to_read,1,1)					!generic li-ion cost curve 
      CALL GETRNGR('PHEV_DOD_FAC    ',PHEV_DOD_FAC(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('EV_DOD_FAC      ',EV_DOD_FAC(iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('CELLMCST        ',CELLMCST(iy:IJUMPYR),Num_to_Read,1,1) 
      CALL GETRNGR('CELLHCST        ',CELLHCST(iy:IJUMPYR),Num_to_Read,1,1) 
      CALL GETRNGR('CELLGCST        ',CELLGCST(iy:IJUMPYR),Num_to_Read,1,1) 
      CALL GETRNGR('HEVSYSCST       ',HEVSYSCST(1:MAXCLASS,iy:IJUMPYR,1:maxvtyp),MAXCLASS,Num_to_Read,MAXVTYP)       
      CALL GETRNGR('PHEV20SYSCST    ',PHEV20SYSCST(1:MAXCLASS,iy:IJUMPYR,1:maxvtyp),MAXCLASS,Num_to_Read,MAXVTYP) 
      CALL GETRNGR('PHEV50SYSCST    ',PHEV50SYSCST(1:MAXCLASS,iy:IJUMPYR,1:maxvtyp),MAXCLASS,Num_to_Read,MAXVTYP) 
      CALL GETRNGR('EVSYSCST        ',EVSYSCST(1:MAXCLASS,iy:IJUMPYR,1:maxvtyp),MAXCLASS,Num_to_Read,MAXVTYP) 
      CALL GETRNGR('PHEVTAXADJ      ',PHEVTAXADJ(iy:IJUMPYR),Num_to_Read,1,1)      
      CALL GETRNGR('PACKA           ',PACKA,1,MAXLDV,1)				! Li-ion battery pack initial cost of production
      CALL GETRNGR('PACKLR          ',PACKLR,1,MAXLDV,1)			! Li-ion battery pack learning rate
      CALL GETRNGR('MATA            ',MATA,1,MAXLDV,1)				! Li-ion materials input cost or production
      CALL GETRNGR('MATLR           ',MATLR,1,MAXLDV,1)				! Li-ion materials learning rate
	  CALL GETRNGR('MAT_MARKUP      ',MAT_MARKUP(33:ijumpyr),ijumpyr-32,1,1)
	  CALL GETRNGR('EV_range_max    ',EV_range_max,1,MAXLDV,1)		! Max EV range for each EV fuel type
	  CALL GETRNGR('LIONkWh_perLb   ',LIONkWh_perLb,1,MAXLDV,1)		! Battery sizing factor (kWh) based on vehicle weight
	  CALL GETRNGR('LION_LB_perkWh  ',LION_LB_perkWh,1,MAXLDV,1)	! Lithium-ion weight (lbs) per kWh battery capacity
	  CALL GETRNGR('EV_range_b      ',EV_range_b,1,MAXLDV,1)		! Range constant based on EV battery size
	  CALL GETRNGR('EV_range_m      ',EV_range_m,1,MAXLDV,1)		! Range slope based on EV battery size
	  CALL GETRNGR('EV_batt_size_b  ',EV_batt_size_b,1,MAXLDV,1)	! Battery sizing constant
	  CALL GETRNGR('EV_batt_size_m  ',EV_batt_size_m,1,MAXLDV,1)	! Battery sizing slope based on vehicle weight

!     fill pre-2023 MAT_MARKUP with 1.0	  
      do iyr = first_read_year,2021
		mat_markup(iyr-1989) = 1.0
	  enddo
	  
      do iyr = first_read_year,IJUMPCALYR
        FuelCell$kW(IYR,13)   = CELLMCST(IYR-1989)
        FuelCell$kW(IYR,14)   = CELLHCST(IYR-1989)
        NiMH_Cost(iyr)        = NIMHCOST(IYR-1989) / MC_JPGDP(11) * MC_JPGDP(1)      ! convert from 2000$ to 1990$
        PHEV_DOD(iyr)         = PHEV_DOD_FAC(IYR-1989)
        EV_DOD(iyr)           = EV_DOD_FAC(IYR-1989)
!        STIMTAXADJ(iyr)       = PHEVTAXADJ(IYR-1989)
        do ILDV=1,maxldv
          Li_ion_Cost(ILDV,iyr)  =  LIONCOST(IYR-1989) / MC_JPGDP(31) * MC_JPGDP(1)  ! convert from 2020$ to 1990$
		  if (ILDV.eq.5.OR.ILDV.eq.6.OR.ILDV.eq.16) Li_ion_Cost(ILDV,iyr)  =  LIONCOST_PHEV(IYR-1989) / MC_JPGDP(31) * MC_JPGDP(1)  ! convert from 2020$ to 1990$
!		  if (ILDV.eq.5.OR.ILDV.eq.6) Li_ion_Cost(ILDV,iyr)  =  LIONCOST_PHEV(IYR-1989) / MC_JPGDP(31) * MC_JPGDP(1)  ! convert from 2020$ to 1990$
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

! ... Read in and store for global reference various Advanced Technology Vehicle parameters

      CALL GETRNGC('MMKEY           ',MMKEY,1,1,1)                    ! ATV make/model key (yes = use TRNLDV.XML data)
      CALL UPPERCASE (MMKEY)
      MMSWITCH = .FALSE.
      IF (MMKEY .EQ. 'YES') MMSWITCH = .TRUE.
      CALL GETRNGR('MAKEMOD         ',MAKEMOD(1:maxldv,iy:IJumpYr,1:maxvtyp),MAXLDV,Num_to_read,MAXVTYP)

! ... Load make/model availability fractions if exogoneous input specified

      IF (MMSWITCH) THEN
        DO IVTYP=1,MAXVTYP
          DO ICL=1,MAXCLASS
            DO ILDV=1,MAXLDV
              DO IYR=byr,IJUMPCALYR
                DO IREGN=1,MNUMCR-2
                  MMAVAIL(IVTYP,ICL,ILDV,IREGN,iyr) = MAKEMOD(ILDV,iyr-1989,IVTYP)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF

! ... Read and load variables for plug-in hybrids.

      CALL GETRNGR('PHEV20_EVMT     ',PHEV20_EVMT,1,1,1)
      CALL GETRNGR('PHEV50_EVMT     ',PHEV50_EVMT,1,1,1)
	  CALL GETRNGR('IRA_BAT_SHR     ',IRA_BAT_SHR(1:2,2023:lyr,1:3),2,lyr-2023+1,3)
	  CALL GETRNGR('IRA_VEH_SHR     ',IRA_VEH_SHR(1:2,2023:lyr,1:3),2,lyr-2023+1,3)
! ... EG_MPG is the ratio of the MPG for the electric motor to the gasoline engine.
      CALL GETRNGR('EG_MPG20        ',EG_MPG20(16:ijumpyr),ijumpyr-15,1,1)  
      CALL GETRNGR('EG_MPG50        ',EG_MPG50(16:ijumpyr),ijumpyr-15,1,1)
	  
! ... PHEVPlug is the share of consumers who can plug in a PHEV.
      CALL GETRNGR('PHEVPLUG        ',PHEVPlug(16:ijumpyr),ijumpyr-15,1,1)
      do iyr=1,15  !sets years prior to 2005. Input from trnldv is 2005 and later
        EG_MPG20(iyr)=EG_MPG20(16)  
		EG_MPG50(iyr)=EG_MPG50(16)  
        PHEVPlug(iyr)=PHEVPlug(16)
      end do

! ... Read and load nested multinomial logit model coefficients

      CALL GETRNGR('NMLMCOEFCAR     ',NMLMCOEFCAR,MAXNMLM,MAXCLASS,1)   ! ATV nested multinomial logit model coefficients for cars
      CALL GETRNGR('NMLMCOEFTRK     ',NMLMCOEFTRK,MAXNMLM,MAXCLASS,1)   ! ATV nested multinomial logit model coefficients for trucks

      DO I=1,MAXNMLM
        DO ICL=1,MAXCLASS
          NMLMCOEF(I,1,ICL) = NMLMCOEFCAR(I,ICL)
          NMLMCOEF(I,2,ICL) = NMLMCOEFTRK(I,ICL)
        ENDDO
      ENDDO

      DO IVTYP=1,MAXVTYP
         DO ICL=1,MAXCLASS
            X21(IVTYP,ICL)      = NMLMCOEF( 1,IVTYP,ICL)              ! NMLM level 2, vehicle price
            X22(IVTYP,ICL)      = NMLMCOEF( 2,IVTYP,ICL)              ! NMLM level 2, fuel cost
            X23(IVTYP,ICL)      = NMLMCOEF( 3,IVTYP,ICL)              ! NMLM level 2, range
            X24(IVTYP,ICL)      = NMLMCOEF( 4,IVTYP,ICL)              ! NMLM level 2, battery replacement
            X25(IVTYP,ICL)      = NMLMCOEF( 5,IVTYP,ICL)              ! NMLM level 2, acceleration
            X26(IVTYP,ICL)      = NMLMCOEF( 6,IVTYP,ICL)              ! NMLM level 2, EV home refueling
            X27(IVTYP,ICL)      = NMLMCOEF( 7,IVTYP,ICL)              ! NMLM level 2, maintenance cost
            X28(IVTYP,ICL)      = NMLMCOEF( 8,IVTYP,ICL)              ! NMLM level 2, luggage space
            X29(IVTYP,ICL)      = NMLMCOEF(11,IVTYP,ICL)              ! NMLM level 2, make/model availability
            BETAFA2(IVTYP,ICL)  = NMLMCOEF( 9,IVTYP,ICL)              ! NMLM level 2, fuel availability 1
            BETAFA22(IVTYP,ICL) = NMLMCOEF(10,IVTYP,ICL)              ! NMLM level 2, fuel availability 2
            X11(IVTYP,ICL)      = NMLMCOEF(12,IVTYP,ICL)              ! NMLM level 1, tech set general cost
            X31(IVTYP,ICL)      = NMLMCOEF(13,IVTYP,ICL)              ! NMLM level 3, multi-fuel general cost
         ENDDO
      ENDDO
      
      CALL GETRNGR('ATVCOEFF        ',ATVCOEFF(1:maxldv,iy:IJumpYr,1:maxvtyp),MAXLDV,Num_to_read,MAXVTYP)     ! ATV calibration coefficients 

! ... read initial light duty vehicle attributes for consumer choice model

      CALL GETRNGR('PCTFAVAIL       ',PCTFAVAIL(1:maxfuel,iy:IJUMPYR,1:mnumcr-2),maxfuel,Num_to_Read,MNUMCR-2)   ! fuel availability
      CALL GETRNGR('INITSTA         ',INITSTA, maxfuel,FUELYRS,MNUMCR-2)  ! refueling stations by fuel type and reg
      CALL GETRNGR('STA_RAT         ',STA_RAT, maxfuel,1,1)               ! ratio of fueling stations to vehicle stock
      CALL GETRNGR('MAINTCAR        ',MAINTCAR,MAXLDV,MAXCLASS,1)         ! car maintenance cost by vehicle tech type
      CALL GETRNGR('MAINTTRK        ',MAINTTRK,MAXLDV,MAXCLASS,1)         ! lt trk maintenance cost by vehicle tech type
      CALL GETRNGR('LUGGCAR         ',LUGGCAR, MAXLDV,MAXCLASS,1)         ! car luggage space by vehicle tech type
      CALL GETRNGR('LUGGTRK         ',LUGGTRK, MAXLDV,MAXCLASS,1)         ! lt trk luggage space by vehicle tech type
      CALL GETRNGR('PRICE_HY        ',PRICE_HY(1:mnumcr,iy:IJUMPYR),MNUMCR,Num_to_Read,1) ! hydrogen fuel price

! ... Read alternative user-specified hydrogen fuel availability.

      CALL GETRNGR('HFSAVIL         ',HFSAvilx(1:3),3,1,1)   !Switch for fuel availability by market segment.
      CALL GETRNGR('HFAVIL          ',HFAvil(1:3,16:mnumyr),3,mnumyr-15,1)   !Fuel availability by market segment.

! ... Switches read as real, now converting to integer
 
      HFSAvil(1)=HFSAvilx(1)
      HFSAvil(2)=HFSAvilx(2)
      HFSAvil(3)=HFSAvilx(3)
      
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
! ...   Note: indexed by fleet type: 1) business  2) government  3) utility   
! ...         and vehicle type:      1) car       2) light truck              
! ...         by size class:         1) mini      2) subcompact  3) compact   
! ...                                4) midsize   5) large                    

      CALL GETRNGR('FLTCRAT         ',FLTCRAT(iy:IJUMPYR),Num_to_Read,1,1)   ! fraction of total car sales attributed to fleet
      CALL GETRNGR('FLTTRAT         ',FLTTRAT(iy:IJUMPYR),Num_to_Read,1,1)   ! fraction of total lt trk sales fleet
      CALL GETRNGR('FLTCARSHR       ',FLTCARSHR(1:maxldv,iy:IJUMPYR,1:MAXFLEET),MAXLDV,Num_to_Read,MAXFLEET)
      CALL GETRNGR('FLTTRKSHR       ',FLTTRKSHR(1:maxldv,iy:IJUMPYR,1:MAXFLEET),MAXLDV,Num_to_Read,MAXFLEET)
	  CALL GETRNGR('FLTAFSHRC       ',FLTAFSHRC(1:MAXCLASS,iy:IJUMPYR,1:MAXFLEET), MAXCLASS,Num_to_Read,MAXFLEET)
	  CALL GETRNGR('FLTAFSHRT       ',FLTAFSHRT(1:MAXCLASS,iy:IJUMPYR,1:MAXFLEET), MAXCLASS,Num_to_Read,MAXFLEET)
      CALL GETRNGR('FLTTRANSPC      ',FLTTRANSPC(1:MAXFLEET,1:MAXAGE),MAXFLEET,MAXAGE,1)    ! fraction of fleet passenger cars transfered to households
      CALL GETRNGR('FLTTRANSLT      ',FLTTRANSLT(1:MAXFLEET,1:MAXAGE),MAXFLEET,MAXAGE,1)    ! fraction of fleet passenger cars transfered to households
	  CALL GETRNGR('SURVFLT         ',SURVFLT,MAXFLEET,MaxAge,MAXVTYP)
	  CALL GETRNGR('FLTSSHRC        ',FLTSSHRC(1:MAXCLASS,iy:IJUMPYR,1:MAXFLEET), MAXCLASS,Num_to_Read,MAXFLEET)
	  CALL GETRNGR('FLTSSHRT        ',FLTSSHRT(1:MAXCLASS,iy:IJUMPYR,1:MAXFLEET), MAXCLASS,Num_to_Read,MAXFLEET)
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

      CALL GETRNGR('CDF             ',CDF         (iy:IJUMPYR),Num_to_Read,1,1)             
      CALL GETRNGR('LTDF            ',LTDF        (iy:IJUMPYR),Num_to_Read,1,1)
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
! ... * Commercial Light Truck Module                                               *
! ... *******************************************************************************

      CALL GETRNGR('CLTVMTDIST      ',CLTVMTDIST,1,6,1)                         
      CALL GETRNGR('CLTSALESHIST    ',CLTSALESHIST (1:6,6:22),6,17,1)
	  CALL GETRNGR('CLTSALESPER     ',CLTSALESPER(1:6,6:22),6,17,1)	
	  CALL GETRNGR('CLTVMT_H        ',CLTVMTT (1:6,6:22),6,17,1)
	  CALL GETRNGR('CLTVMT2012      ',CLTVMT2012, MAXAGE2B,1,1)
	  CALL GETRNGR('CLTMPG2012      ',CLTMPG2012, 6,MAXAGE2B,1)
	  CALL GETRNGR('CLTVMTVA_H      ',CLTVMTVA_H (6:22),17,1,1)  
	  
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
	  CALL GETRNGR('TRCOVID         ',TRCOVID   (iy:IJUMPYR),Num_to_read,1,1)
      CALL GETRNGR('TRRPMHIST       ',TRRPMHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TREFFHIST       ',TREFFHIST (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
      CALL GETRNGR('TREDHIST        ',TREDHIST  (1:mnumcr-2,iy:IJUMPYR),MNUMCR-2,Num_to_Read,1)
	  CALL GETRNGR('TR_CAV_ADJ      ',TR_CAV_ADJ(iy:IJUMPYR),Num_to_read,1,1)
	  CALL GETRNGI('CRHISTYEAR      ',CRHISTYEAR,1,1,1)
	  CALL GETRNGR('CR_COEF         ',CR_COEF   (1:MNUMCR-2,1:4),MNUMCR-2,4,1)
	  CALL GETRNGR('CRCOVID         ',CRCOVID   (iy:IJUMPYR),Num_to_read,1,1)	  
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
	  CALL GETRNGR('TBCOVID         ',TBCOVID   (iy:IJUMPYR),Num_to_read,1,1)
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

! ... *******************************************************************************
! ... * STEO History shared to regions with SEDS, used to benchmark fuel use by census region
! ... *******************************************************************************
	  CALL GETRNGR('SEDS_tran       ',SEDS_tran,  MNUMCR,4,1)              ! SEDS by region and fuel - gasoline, jet fuel, diesel, residual fuel
	  CALL GETRNGR('MER_tran        ',MER_tran,  1,4,1)					   ! MER by fuel - gasoline, jet fuel, diesel, residual fuel
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
          DEL_COSTABS(TECHID(K),IVTYP) = TECHCOSTA(K) / MC_JPGDP(29) * MC_JPGDP(1)  ! convert from 2018$ to 1990$
          DEL_COSTWGT(TECHID(K),IVTYP) = TECHCOSTR(K) / MC_JPGDP(29) * MC_JPGDP(1)  ! convert from 2018$ to 1990$
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
              WRITE (6,*)
              WRITE (6,*) '======================================'
              WRITE (6,*)
              IF (INPUT_ERROR .EQ. 1) THEN
                WRITE (6,*) 'Base Tech Pen is less than Zero'
              ELSEIF (INPUT_ERROR .EQ. 2) THEN
                WRITE (6,*) 'Base Tech Pen is Non-Zero Prior to'
                WRITE (6,*) '            Tech Introduction Year'
              ELSEIF (INPUT_ERROR .EQ. 3) THEN
                WRITE (6,*) 'Max Tech Pen is greater than One'
              ELSEIF (INPUT_ERROR .EQ. 4) THEN
                WRITE (6,*) 'Max Tech Pen is less than Base Pen'
              ENDIF
              WRITE (6,*)
              WRITE (6,*) 'Vehicle Group    = ',GROUPLABEL(IGP)
              WRITE (6,*) 'Vehicle Class    = ',CLASSLABEL(ICL,IGP)
              WRITE (6,*) 'Technology ID    = ',TECHLABEL(ITECH,IVTYP)
              WRITE (6,*) 'Base Penetration = ',MKT_PEN(ICL,IGP,ITECH,BASE,GAS)
              WRITE (6,*) 'Max Penetration  = ',MKT_MAX(ICL,IGP,ITECH,GAS)
              IF (INPUT_ERROR .EQ. 2) WRITE (6,*) 'Tech Intro Year  = ',FRSTYEAR(ITECH,IGP)
              WRITE (6,*)
              WRITE (6,*) '     ***** Run ABORTED *****'
              WRITE (6,*)
              WRITE (6,*) 'Fix Tech Market Share Matrix and Rerun'
              WRITE (6,*)
              WRITE (6,*) '======================================'
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

!      CALL GETRNGI('first_hav_year  ',first_hav_year,1,maxhav-1,1)						    					!...first year HAVs are available - 4a [1], 4b [2], and 5 [3]
!      CALL GETRNGR('hav_init_cost   ',hav_init_cost,maxhav-1,1,1)						    					!...HAV system initial cost (no lidar and battery) in intro year for 4a [1], 4b [2], and 5 [3]
      CALL GETRNGI('hav_lidar_map   ',hav_lidar_map,1,maxhav-1,1)						    					!...lidar (1, 2) used by each HAV - 4a [1], 4b [2], and 5 [3]
      CALL GETRNGR('hav_sys_lrn     ',hav_sys_lrn(1:maxhav-1,first_read_year:lyr),maxhav-1,num_to_read,1)    	!...HAV system cost (no lidar or battery) reductions for 4a [1], 4b [2], and 5 [3]
!      CALL GETRNGR('hav_sys_wgt     ',hav_sys_wgt,maxhav-1,1,1)						    						!...HAV system weight (no lidar or battery) for 4a [1], 4b [2], and 5 [3]
      CALL GETRNGR('HAV_battery_kWh ',HAV_battery_kWh,maxhav-1,1,1)    											!...HAV battery capacity for 4a [1], 4b [2], and 5 [3]
	  CALL GETRNGR('hav_techmap     ',hav_techmap,maxhav-1,1,1)						    						!...Maps HAV levels to tech matrix indices (e.g. 4a:90, 4b:91, 5:92)	
!      hav_init_cost(:) = hav_init_cost(:) / MC_JPGDP(26) * MC_JPGDP(1)											!...convert initial cost from 2015$ to 1990$

!...Fleet HAV adoption variable inputs
      CALL GETRNGR('taxi_rev_params ',taxi_rev_params,maxhav,6,1)	! parameters for taxi revenue calculation
	  do ihav = 1, maxhav
         taxi_mi_life(ihav) = taxi_rev_params(ihav,1)									! taxi lifetime miles
         taxi_idle_gph(ihav) = taxi_rev_params(ihav,2)									! taxi idle fuel rate, gallons/hr
         taxi_maint_cost(ihav) = taxi_rev_params(ihav,3) / MC_JPGDP(26) * MC_JPGDP(1)	! taxi maintenance costs per month (independent of mileage), input in 2015$; converted to 1990$
         taxi_data_fee(ihav) = taxi_rev_params(ihav,4) / MC_JPGDP(26) * MC_JPGDP(1)		! HAV data fee per month, input in 2015$; converted to 1990$
         taxi_insur(ihav) = taxi_rev_params(ihav,5) / MC_JPGDP(26) * MC_JPGDP(1)		! taxi insurance fee per month, input in 2015$; converted to 1990$
		 hav_mpg_fact(ihav) = taxi_rev_params(ihav,6)									! taxi mpg multiplier for HAV; applied to degradation factors cdfrfg and ltdfrfg
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
    if (yrs.lt.minyear) then
        write(21,*) "HAV cost subroutine called in year ", yrs, " prior to lowest lidar and hav first year."
        RETURN 
    endif

!...Get current year LIDARCOST (lidar_cost(maxlidar, yrs))
    CALL LIDARCOSTCALC          ! returns lidar_cost(maxhav,byr:lyr)

!...Calculate total HAV incremental cost
	do ihav = 2, maxhav
        if (yrs.ge.MINVAL(frstyear(hav_techmap(ihav-1),:))) then
			hav_sys_cost(ihav, yrs) = hav_sys_lrn(ihav-1, yrs)*DEL_COSTABS(hav_techmap(ihav-1),1) + Li_ion_Cost(5, yrs)*hav_battery_kWh(ihav-1) + lidar_cost(ihav,yrs)
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
    if (yrs.lt.first_lidar_year(1).and.yrs.lt.first_lidar_year(2)) then
        write(21,*) "Lidar cost subroutine called in year ", yrs, " prior to lidar first year."
        RETURN 
    endif
	
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
                if (cumul_lidar_prod(ilidar, yrs-2).eq.0) then 
                    write(21,*) "Historical cumulative lidar production for lidar system ", ilidar, "not populated in year ", yrs
                endif
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
	LOGICAL*1 :: havatvflag(maxvtyp,MAXCLASS,maxldv)=.true.		    ! local variable for CARFLG / TRKFLG - year ILDV is available in class
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
!...Collapse CARFLG/TRKFLG into single boolean variable (i.e., available in ivtype/ICL/ILDV this year or not?)
	
	do ILDV = 2, maxldv
		do ICL = 1, MAXCLASS
			havatvflag(1,ICL,ILDV) = CARFLG(ILDV-1,ICL).le.yrs
			havatvflag(2,ICL,ILDV) = TRKFLG(ILDV-1,ICL).le.yrs
		enddo
	enddo

!...make sure that hav is available
    if (yrs.lt.MINVAL(frstyear(hav_techmap,:))) then
        write(21,*) '***** WARNING: FLTHAV adoption subroutine called in year ', yrs, ' prior to first year any hav is available: ', MINVAL(frstyear(hav_techmap,:))
        RETURN 
    endif
!
!...calculate utility of HAV levels (including ihav = 1: L0-3) within type, class, and powertrain
	do IVTYP = 1, maxvtyp
		do ICL = 1, MAXCLASS
			do ILDV = 1, maxldv
			!...If no HAV is available in this class, vehicle type, and ILDV, exit ILDV loop and check next
			    if ((.not.(havatvflag(IVTYP,ICL,ILDV))).or.(.not.(any(tech_applic(hav_techmap,IVTYP,ILDV))))) then
				    taxi_sales(mnumcr,IVTYP,ICL,ILDV,yrs,1) = fltechsal(mnumcr,IVTYP,4,ICL,ILDV,1)
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
	!						write(21,'(A,4F9.3)') '    Fuel price:', avgfuelprice(iregn,:)
	!						write(21,'(A,4F9.3)') '    Fuel cost :', fuelpriceproj
	!						write(21,'(A,4F9.3)') '    Delta_fuel:', del_fuelprice(iregn)
	!						write(21,'(A,4I9)')   '    Taxi life :', taxi_life(iregn,:)
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
				    taxi_sales(iregn,IVTYP,ICL,ILDV,yrs,:) = flt_hav_shares(iregn,IVTYP,ICL,ILDV,yrs,:)*fltechsal(iregn,IVTYP,4,ICL,ILDV,1)
					! Populate other fleet model sales/stock variables (JUST a redistribution of existing vehicles into ihav bins -- no changes to the totals)
					do ihav=1,maxhav
					  fltechsal(iregn,IVTYP,4,ICL,ILDV,ihav) = taxi_sales(iregn,IVTYP,ICL,ILDV,yrs,ihav)
					  FLTECH(iregn,IVTYP,4,ILDV,ihav) = sum(FLTECHSAL(iregn,IVTYP,4,1:MAXCLASS,ILDV,ihav))
					  Flt_Stock(iregn,IVTYP,4,ILDV,1,ihav,n)=FLTECH(iregn,IVTYP,4,ILDV,ihav)
					enddo
				enddo	! next iregn
				! Populate national totals for taxi_sales (written out for debug later) and fleet variables (used in fleet subroutines later)
				do ihav = 1, maxhav
					taxi_sales(mnumcr,IVTYP,ICL,ILDV,yrs,ihav) = sum(taxi_sales(1:mnumcr-2,IVTYP,ICL,ILDV,yrs,ihav))
					fltechsal(mnumcr,IVTYP,4,ICL,ILDV,ihav) = sum(fltechsal(1:mnumcr-2,IVTYP,4,ICL,ILDV,ihav))
					FLTECH(mnumcr,IVTYP,4,ILDV,ihav) = sum(FLTECHSAL(mnumcr,IVTYP,4,1:MAXCLASS,ILDV,ihav))
					Flt_Stock(mnumcr,IVTYP,4,ILDV,1,ihav,n)=FLTECH(mnumcr,IVTYP,4,ILDV,ihav)
					TFLTECHSTK(IVTYP,4,ILDV,ihav)=sum(Flt_Stock(mnumcr,IVTYP,4,ILDV,1:maxage,ihav,n))
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
Integer*2     LDV_CENSUS(numhhstk), LDV_FUEL(numhhstk), LDV_VINTAGE(numhhstk), HDV_CENSUS(1836), HDV_FUEL(1836), HDV_VINTAGE(1836)
Integer*2     LDV_fleet(numfltstk), Fleet_census(numfltstk), Fleet_fuel(numfltstk), Fleet_vintage(numfltstk)
INTEGER*2     m2, r2, a2, f2, fl2, j2, y2

!...Output dataset
      INAME = 'TRNH2OUT'
      out78 = FILE_MGR('O',INAME,.TRUE.)

!...Store data ranges from xlsx file
      INAME = 'TRNSTOCKX'
      WKUNIT = FILE_MGR('O',INAME,NEW)   ! open trnstockx.xlsx input file
      CALL ReadRngXLSX(WKUNIT,'LDV')     ! read range names & coerresponding data from worksheet "LDV"

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
	  	  
! ... *******************************************************************************
! ... * Commercial Light Truck Module                                               *
! ... *******************************************************************************
!... Pulling in 1995 - 2011 data (national data)
       CALL GETRNGR('CLS2bSTKHIST    ',CLS2bSTKHIST(1:6,6:22),6,17,1)
!... Pulling in 2012 - onward data (regional data)

      WKUNIT = FILE_MGR('C',INAME,NEW)   !close .xlsx input file
      INAME = 'TRNSTOCKX'
      WKUNIT = FILE_MGR('O',INAME,NEW)   ! open trnstockx.xlsx input file

      CALL ReadRngXLSX(WKUNIT,'HDV')      !read range names & coerresponding data from worksheet "HDV"
      WKUNIT = FILE_MGR('C',INAME,NEW)   !close .xlsx input file
 
!... Variables to help with reading in regional stock variables
      CALL GETRNGI('HDV_CENSUS      ',HDV_CENSUS,  1,1836,1)
      CALL GETRNGI('HDV_FUEL        ',HDV_FUEL,  1,1836,1)
      CALL GETRNGI('HDV_VINTAGE     ',HDV_VINTAGE,  1,1836,1)   	  
	   
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
	      do ihav = 1,maxhav
		    do IVTYP = 1,maxvtyp
			  do iown=1,maxowner
			    if (iown.eq.1) then 
                  do iage=2,maxage-1
		            LDV_STOCK(iregn,IVTYP,iown,ILDV,iage,ihav,j2) = LDV_STOCK(iregn,IVTYP,iown,ILDV,iage-1,ihav,j2-1)*SSURV25(iregn,iage-1,IVTYP)	  
                  enddo
		          LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage,ihav,j2) = LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage-1,ihav,j2-1)*SSURV25(iregn,maxage-1,IVTYP) + &
			                                                     LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage,ihav,j2-1)*SSURV25(iregn,maxage,IVTYP)
				else
				  do iage=2,maxage-1
		            LDV_STOCK(iregn,IVTYP,iown,ILDV,iage,ihav,j2) = LDV_STOCK(iregn,IVTYP,iown,ILDV,iage-1,ihav,j2-1)*SURVFLT(iown-1,iage-1,IVTYP)	  
                  enddo
		          LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage,ihav,j2) = LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage-1,ihav,j2-1)*SURVFLT(iown-1,iage-1,IVTYP) + &
			                                                     LDV_STOCK(iregn,IVTYP,iown,ILDV,maxage,ihav,j2-1)*SURVFLT(iown-1,iage-1,IVTYP)
				endif
			  enddo
		    enddo
		  enddo
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

      INTEGER         ib

      Ib=n
      If(curcalyr.eq.first_read_year) ib=1

      TMC_PGDP(ib:) = MC_JPGDP(ib:)

! ... Add an incremental petroleum fuel tax to highway fuel costs.  The tax is read in as
! ... nominal $/million Btu, converted to 1987$, then applied to the fuel prices obtained from PMM.

      FUELTAX87 = FUELTAX / TMC_PGDP

! ... Multiplying FUELTAX87 by 0.901 adjusts for the higher Btu content of diesel fuel, thus
! ... maintaining an equivalent per gallon tax.
      DO IREGN=1,MNUMCR
        HWYPDSTR(IREGN,ib:N) = PDSTRHWY(IREGN,IB:N) !+ (FUELTAX87(IB:N) * 0.901)
      ENDDO

! ... tax credits for PHEVs and EVs	  
	  phev_credit  = 0.0
	  kwh_credit   = 0.0
	  max_credit   = 0.0
	  ira_veh_cred = 0.0
	  ira_bat_cred = 0.0 	  
! ... calculate values for EIEA08/Stimulus PHEV tax credits
! ... Convert credits to 1990 dollars and adjust for 200K vehicle limit by mfg
      if(phevstim.eq.1.0)then
        phev_credit = (2500.0/ MC_JPGDP(33) * MC_JPGDP(1)) * phevtaxadj(n) 
        kwh_credit  = (417.0/ MC_JPGDP(33) * MC_JPGDP(1)) * phevtaxadj(n) 
        max_credit  = (7500.0/ MC_JPGDP(33) * MC_JPGDP(1)) * phevtaxadj(n) 
	  endif
! ... IRA credits for PHEV and EV	  
	  if(ira_stim.eq.1.0)then
	    ira_veh_cred = 3750.0/ MC_JPGDP(33) * MC_JPGDP(1)
		ira_bat_cred = 3750.0/ MC_JPGDP(33) * MC_JPGDP(1)		
	  endif

      DO I=1,MNUMCR
        IF (I .EQ. 10) CYCLE
        DO J=1,MNSICM
          TMC_REVIND(I,J,ib:N) = MC_REVIND(I,J,ib:N) * 1000.
        ENDDO
! ... adjust price index when dollar year changes (from macro)						! MDRAEO2022 -- the "23" in these deflators needs to be automatically updated 
        TMC_CPI(I,ib:N)    = MC_CPI(I,IB:N)
        INC00$NPT(I,IB:N)  = MC_YPDR(I,IB:N)/MC_NP(I,IB:N)/(MC_JPGDP(23)/MC_JPGDP(11))
        INC00$NP(I,IB:N)   = (MC_YPDR(I,IB:N)/MC_NP(I,IB:N))*1000.0 /(MC_JPGDP(23)/MC_JPGDP(11))
        INC00$16(I,IB:N)   = (MC_YPDR(I,IB:N)/MC_NP16A(I,IB:N)) *1000.0 /(MC_JPGDP(23)/MC_JPGDP(11))
      ENDDO

! ... Convert to $1990.  MC_JPGDP is GDP Deflator Index (1987=1.0).
! ... MC_JPGDP(11) converts $00 to $87.  MC_JPGDP(1) converts $87 to $90.

      TMC_EX(ib:N)  = MC_XR(ib:N)   * (MC_JPGDP(1)/MC_JPGDP(7))
      TMC_IM(ib:N)  = MC_MR(ib:N)   * (MC_JPGDP(1)/MC_JPGDP(7))

! ... Calculate national average per capita income (1990 $) 
      do iregn = 1,mnumcr
          if(MC_NP(iregn,N).gt.0.0) INC90$NP(iregn,YRS) = (MC_YPDR(iregn,N)/MC_NP(iregn,N))*MC_JPGDP(1)*1000.0
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
    
!...fill regional drivers assuming licrates across census division  
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

    REAL            TEMPCLS12A(MNUMYR)
	REAL            PMGTR00$C(MNUMYR),PMGTR00$C_regn(mnumcr,mnumyr)
	REAL            CARSHARE_regn(mnumcr,mnumyr), TRKSHARE_regn(mnumcr,mnumyr), CARSHRT_regn(mnumcr,mnumyr), &
	                TRKSHRT_regn(mnumcr,mnumyr), TTLSHR_regn(mnumcr,mnumyr), regn_shr(mnumcr,mnumyr), &
					HP_avg(mnumyr),  Weight_avg(mnumyr), &
					MPG_avg(mnumyr) 
	Real            newcar_sum, newcls12a_sum

!...Calculate gasoline price in 2000$ per gallon
    PMGTR00$C(N) = PMGTR(11,N)* MG_HHV/1000.0 * MC_JPGDP(11)*100.0
	do iregn = 1,mnumcr-2
	    PMGTR00$C_regn(iregn,N) = PMGTR(iregn,N)* MG_HHV/1000.0 * MC_JPGDP(iregn)*100.0
	enddo

!...New light truck Class 2B - NOT effected by shift in fuel price
    TEMPCLS12A(n)=(MC_Vehicles(1,n)+MC_Vehicles(2,n))*LTSplit(n)
    NEWCLS2B = (MC_Vehicles(1,n)+MC_VEHICLES(2,N)-TEMPCLS12A(n))*1000000.  ! New Class 2b vehicle sales
!	Pre-Polk data, populate sales based on exogenous fuel shares
!	After 2011, CLT sales are populated from stock data read in by tranfrt.f
    if(curcalyr.le.2011) then
      CLTSALT(1,n) = NEWCLS2B*CLTSALESPER(1,n)  ! Gasoline sales
      CLTSALT(2,n) = NEWCLS2B*CLTSALESPER(2,n)  ! Diesel sales
	  CLTSALT(3,n) = NEWCLS2B*CLTSALESPER(3,n)  ! LPG sales
	  CLTSALT(4,n) = NEWCLS2B*CLTSALESPER(4,n)  ! CNG sales
	  CLTSALT(5,n) = NEWCLS2B*CLTSALESPER(5,n)  ! Flex fuel sales
	  CLTSALT(6,n) = NEWCLS2B*CLTSALESPER(6,n)  ! Electric sales
      CLTSALT(10,n)= NEWCLS2B                  ! Total New Class 2b vehicle sales for report writer
	endif


!...Populate historic total sales for car and class 1-2a (millions of vehicles) by IVTYP, ILDV and region
    if(curcalyr.le.STOCKYR)then
      do iregn=1,mnumcr-2
	    do ILDV=1,maxldv
	      TRLDSALC(ILDV,iregn,n) = sum(LDV_STOCK(iregn,1,1:maxowner,ILDV,1,1,n))
		  TRLDSALT(ILDV,iregn,n) = sum(LDV_STOCK(iregn,2,1:maxowner,ILDV,1,1,n))
	    enddo
	  enddo
		
!...  fill historic new car and light truck sales arrays
      do iregn=1,mnumcr-2
	    NEWCARS(iregn,n) = sum(LDV_STOCK(iregn,1,1:maxowner,1:maxldv,1,1,n))
		NEWCLS12A(iregn,N) = sum(LDV_STOCK(iregn,2,1:maxowner,1:maxldv,1,1,n))
	  enddo
	  NEWCARS(mnumcr,N) = sum(LDV_STOCK(mnumcr,1,1:maxowner,1:maxldv,1,1,n)) 
      NEWCLS12A(mnumcr,N) = sum(LDV_STOCK(mnumcr,2,1:maxowner,1:maxldv,1,1,n)) 
	  
!...  calculate shares of household cars and light trucks across region
	  do iregn=1,mnumcr-2
		CarShare(iregn,n) = sum(LDV_STOCK(iregn,1,1,1:maxldv,1,1,n))/sum(LDV_STOCK(mnumcr,1,1,1:maxldv,1,1,n))
	    TrkShare(iregn,n) = sum(LDV_STOCK(iregn,2,1,1:maxldv,1,1,n))/sum(LDV_STOCK(mnumcr,2,1,1:maxldv,1,1,n))
	  enddo
	  CarShare(11,n) = sum(LDV_STOCK(mnumcr,1,1,1:maxldv,1,1,n))/sum(LDV_STOCK(mnumcr,1:maxvtyp,1,1:maxldv,1,1,n))
	  TrkShare(11,n) = sum(LDV_STOCK(mnumcr,2,1,1:maxldv,1,1,n))/sum(LDV_STOCK(mnumcr,1:maxvtyp,1,1:maxldv,1,1,n))

!...  caluclate shares of cars and light trucks within region
      do iregn=1,mnumcr-2
		regn_shr(iregn,n) = sum(LDV_STOCK(iregn,1:maxvtyp,1:maxowner,1:maxldv,1,1:maxhav,n))/sum(LDV_STOCK(mnumcr,1:maxvtyp,1:maxowner,1:maxldv,1,1:maxhav,n))
		CARSHARE_regn(iregn,N) = sum(LDV_STOCK(iregn,1,1:maxowner,1:maxldv,1,1:maxhav,n))/sum(LDV_STOCK(iregn,1:maxvtyp,1:maxowner,1:maxldv,1,1:maxhav,n))
		TRKSHARE_regn(iregn,N) = sum(LDV_STOCK(iregn,2,1:maxowner,1:maxldv,1,1:maxhav,n))/sum(LDV_STOCK(iregn,1:maxvtyp,1:maxowner,1:maxldv,1,1:maxhav,n))     
	  enddo
	  
!...  Calculate owner share by owner type, vehicle type within region
      do iregn=1,mnumcr-2
		do IVTYP=1,maxvtyp
		  do iown=1,maxowner
		    Owner_Share(iregn,IVTYP,iown,n) = sum(LDV_STOCK(iregn,IVTYP,iown,1:maxldv,1,1,n))/sum(LDV_STOCK(iregn,IVTYP,1:maxowner,1:maxldv,1,1,n))
		  enddo
		enddo
	  enddo
	  
!...  EISA CAFE shifts manufacturer production to cars
      if(n.ge.23.and.n.le.31)then
        dumm(n)=dumm(n-1)-0.01
      elseif(n.ge.32)then
        dumm(n)=dumm(n-1)
      endif
		
!...  Calculate new vehicle sales per licensed driver by region	
	  do iregn=1,mnumcr-2
	    NewLDVPerLD(iregn,n) = (NewCars(iregn,n)+NewCLS12a(iregn,n))/sum(LicDriver(1:agegrp,1:mf,iregn,n))
	  enddo		
    endif

!...Project percent of total light vehicles <8,500 GVW that are cars and light trucks by region
    if(curcalyr.gt.stockyr) then

!...  Hold Owner_Share at last historic value
      do iregn=1,mnumcr-2
		do IVTYP=1,maxvtyp
		  do iown=1,maxowner
		    Owner_Share(iregn,IVTYP,iown,n) = Owner_share(iregn,IVTYP,iown,n-1)
		  enddo
		enddo
	  enddo   

!...  Determine new LDV sales shares by region post STOCKYR based on licensed drivers
      do iregn=1,mnumcr-2
	    LDVSales(iregn,n) = NewLDVPerLD(iregn,stockyr-1989)*sum(LICDriver(1:agegrp,1:mf,iregn,n))
	  enddo
	  do iregn=1,mnumcr-2
	    regn_shr(iregn,n) = LDVSales(iregn,n)/sum(LDVSales(1:mnumcr-2,n))
	  enddo
	  
!...  national shares
      CARSHRT(N) = EXP(CCONSTANT(1)*(1-CRHO(1))+(CRHO(1)*LOG(CARSHARE(mnumcr,n-1))) +          &
                   CINC(1) *(LOG(INC00$16(mnumcr,n))- (CRHO(1)*LOG(INC00$16(mnumcr,n-1)))) + &
                   CFUEL(1)*(LOG(PMGTR00$C(n))  - (CRHO(1)*LOG(PMGTR00$C(n-1)))) +   &
                   CHP(1)  *(LOG(AHPCAR(mnumcr,n-1))   - (CRHO(1)*LOG(AHPCAR(mnumcr,n-2)))) +    &
                   CWGT(1) *(LOG(AWTCAR(mnumcr,n-1))   - (CRHO(1)*LOG(AWTCAR(mnumcr,n-2)))) +    &
                   CMPG(1) *(LOG(TRUEMPG(1,n-1)) - (CRHO(1)*LOG(TRUEMPG(1,n-2)))) +    &
                   CDUMM(1)*(log(DUMM(n)) - (CRHO(1)*log(DUMM(n-1)))))

      TRKSHRT(N) = EXP(TCONSTANT(1)*(1-TRHO(1))+(TRHO(1)*LOG(TRKSHARE(mnumcr,n-1))) +          &
                   TINC(1) *(LOG(INC00$16(mnumcr,n))- (TRHO(1)*LOG(INC00$16(mnumcr,n-1)))) + &
                   TFUEL(1)*(LOG(PMGTR00$C(n))  - (TRHO(1)*LOG(PMGTR00$C(n-1)))) +   &
                   THP(1)  *(LOG(AHPTruck(mnumcr,n-1)) - (TRHO(1)*LOG(AHPTruck(mnumcr,n-2)))) +    &
                   TWGT(1) *(LOG(AWTTruck(mnumcr,n-1)) - (TRHO(1)*LOG(AWTTruck(mnumcr,n-2)))) +    &
                   TMPG(1) *(LOG(TRUEMPG(2,n-1)) - (TRHO(1)*LOG(TRUEMPG(2,n-2)))) +    &
                   TDUMM(1)*(log(DUMM(n)) - (TRHO(1)*log(DUMM(n-1)))))
				   
	  TTLSHR(N)   = CARSHRT(N) + TRKSHRT(N)
      CARSHARE(mnumcr,N) = CARSHRT(N)/TTLSHR(N)
      TRKSHARE(mnumcr,N) = TRKSHRT(N)/TTLSHR(N)	
	  
!...    regional shares
	  do iregn = 1,mnumcr-2		   
        CARSHRT_regn(iregn,N) = EXP(CCONSTANT(iregn)*(1-CRHO(iregn))+(CRHO(iregn)*LOG(CARSHARE_regn(iregn,n-1))) +          &
               CINC(iregn) *(LOG(INC00$16(iregn,n))- (CRHO(iregn)*LOG(INC00$16(iregn,n-1)))) + &
               CFUEL(iregn)*(LOG(PMGTR00$C_regn(iregn,n))  - (CRHO(iregn)*LOG(PMGTR00$C_regn(iregn,n-1)))) +   &
               CHP(iregn)  *(LOG(AHPCAR(iregn,n-1))   - (CRHO(iregn)*LOG(AHPCAR(iregn,n-2)))) +    &
               CWGT(iregn) *(LOG(AWTCAR(iregn,n-1))   - (CRHO(iregn)*LOG(AWTCAR(iregn,n-2)))) +    &
               CMPG(iregn) *(LOG(TRUEMPG_regn(iregn,1,n-1)) - (CRHO(iregn)*LOG(TRUEMPG_regn(iregn,1,n-2)))) +    &
               CDUMM(iregn)*(log(DUMM(n)) - (CRHO(iregn)*log(DUMM(n-1)))))

        TRKSHRT_regn(iregn,N) = EXP(TCONSTANT(iregn)*(1-TRHO(iregn))+(TRHO(iregn)*LOG(TRKSHARE_regn(iregn,n-1))) +          &
               TINC(iregn) *(LOG(INC00$16(iregn,n))- (TRHO(iregn)*LOG(INC00$16(iregn,n-1)))) + &
               TFUEL(iregn)*(LOG(PMGTR00$C_regn(iregn,n))  - (TRHO(iregn)*LOG(PMGTR00$C_regn(iregn,n-1)))) +   &
               THP(iregn)  *(LOG(AHPTruck(iregn,n-1)) - (TRHO(iregn)*LOG(AHPTruck(iregn,n-2)))) +    &
               TWGT(iregn) *(LOG(AWTTruck(iregn,n-1)) - (TRHO(iregn)*LOG(AWTTruck(iregn,n-2)))) +    &
               TMPG(iregn) *(LOG(TRUEMPG_regn(iregn,2,n-1)) - (TRHO(iregn)*LOG(TRUEMPG_regn(iregn,2,n-2)))) +    &
               TDUMM(iregn)*(log(DUMM(n)) - (TRHO(iregn)*log(DUMM(n-1)))))	
        
        TTLSHR_regn(iregn,n)   = CARSHRT_regn(iregn,n) + TRKSHRT_regn(iregn,n)
        CARSHARE_regn(iregn,n) = CARSHRT_regn(iregn,n)/TTLSHR_regn(iregn,n)
        TRKSHARE_regn(iregn,n) = TRKSHRT_regn(iregn,n)/TTLSHR_regn(iregn,n)		
	  enddo				   	
	  
!...New car and light truck sales projection by census division
	  do iregn=1,mnumcr-2
	    NEWCARS(iregn,n) = (MC_SUVA(N) + TEMPCLS12A(N)) * CARSHARE_regn(iregn,N)*regn_shr(iregn,n)
		NEWCLS12A(iregn,N) = (MC_SUVA(N) + TEMPCLS12A(N)) * (1.0-CARSHARE_regn(iregn,N))*regn_shr(iregn,n)
      enddo
	  NEWCARS(mnumcr,N) = sum(NEWCARS(1:mnumcr-2,n))
	  NEWCLS12A(mnumcr,N) = sum(NEWCLS12A(1:mnumcr-2,n))	
	  
!...  calculate sales shares across census division
	  do iregn=1,mnumcr-2
	    CarShare(iregn,n) = NewCars(iregn,n)/NewCars(mnumcr,n)
        TrkShare(iregn,n) = NewCLS12A(iregn,n)/NewCLS12A(mnumcr,n)
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

      FF     = CHAR(12)
      O_UNIT = 21

! ... Load all historic data files
      if(first_time.eq.0) then
        first_time=1
        CALL READNHTSA
        CALL READHIST
        write(*,'(a)') 'Finished reading ReadHist'
      endif

! ... In setting ATV car and truck flags, user inputs are overridden if the corresponding
! ... gasoline vehicle does not exist.  In other words, an ATV group/class combination
! ... is not allowed unless the same group/class combination is allowed for gasoline
! ... vehicles.  This is necessary since initial ATV attributes are expressed relative to
! ... gasoline. Note, the CLASSFLAG is set for all years after initial penetration year,
! ... since there is a time dimension.

      do ILDV=2,MAXLDV
        do ICL=1,MAXCLASS
          do IGP=1,MAXGROUP
            it=GrpMap(IGP)
            CLASSFLAG(ICL,IGP,ILDV)= .false.
            if(it.eq.1.and.CARFLG(ILDV-1,ICL).le.yrs) then
              if(CLASSFLAG(ICL,IGP,gas)) CLASSFLAG(ICL,IGP,ILDV)= .true.
            endif
            if(it.eq.2.and.TRKFLG(ILDV-1,ICL).le.yrs) then
              if(CLASSFLAG(ICL,IGP,gas)) CLASSFLAG(ICL,IGP,ILDV)= .true.
            endif
          enddo
        enddo
      enddo

      IF (YRS .GT. XYR) CALL AFVADJ (YRS) ! calc alt fuel BASE and CURRENT val for intro yr

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

      pass=1
        do IGP=1,MAXGROUP
         RegCost(IGP)=0.0
        end do
        CALL FEMCALC
        CALL CGSHARE
        CALL TREG
        CALL TLDV
        CALL CAFECALC
      pass=2
! ... And now through every one of the increments...
! ... Implement the fine incrementally on this pass.
       npass2=10
       do pass2=1,npass2
         do IGP=1,MAXGROUP
          RegCost(IGP)=Reg_Cost*(float(pass2)/float(npass2))
         end do
         CALL FEMCALC
         CALL CGSHARE
         CALL TREG
         CALL TLDV
         CALL CAFECALC
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
        CALL FEMCALC
        CALL CGSHARE
        CALL TREG
        CALL TLDV
        CALL CAFECALC               
       end do
	   
    if(yrs.ge.2022)then
      if(NewMPG(3,n).lt.CAFESTD(3,n)) call CAFETEST
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


! ... Sumcheck holds the IDs of techs that together should not sum to more than one.
! ... It is formatted to allow up to 15 IDs for a single check, with up to 20 checks
! ... total.  Additional checks can be accomodated by either adding new IDs to unused
! ... array elements or increasing the second array dimension (and the subsequent loop
! ... index parameter accordingly.  Values of -9 indicate "No Data."

      INTEGER       SUMCHECK(15,20)
      REAL          CHECKSUM
      REAL FUNCMAX
      EXTERNAL FUNCMAX
	  DATA SUMCHECK / 1,  2,  3,  4,  5, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 0l IDs (material substitution)
                      6,  7,  8,  9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 02 IDs (drag reduction)
                     10, 11, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 03 IDs (tires)
                     14, 15, 16, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 04 IDs (manual transmission)
                     21, 22, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 05 IDs (CVT transmission)
                     17, 18, 19, 22, 23, 24, 25, 27, 28, -9, -9, -9, -9, -9, -9, &   !Check 06 IDs (auto transmission)
                     29, 30, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 07 IDs (DCT transmission)
                     52, 53, 54, 55, 56, 57, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 08 IDs (turbo 1&2)
                     52, 53, 54, 55, 56, 57, 58, 59, 60, -9, -9, -9, -9, -9, -9, &   !Check 09 IDs (CEGR & turbo 1&2)
                     52, 53, 54, 55, 56, 57, 61, 62, 63, -9, -9, -9, -9, -9, -9, &   !Check 10 IDs (HCR 1 & turbo 1&2)
                     52, 53, 54, 55, 56, 57, 61, 62, 63, 64, 65, 66, -9, -9, -9, &   !Check 11 IDs (HCR 1+, HCR 1 & turbo 1&2)
                     52, 53, 54, 55, 56, 57, 61, 62, 63, 64, 65, 66, 67, 68, 69, &   !Check 12 IDs (HCR 2, HCR 1+, HCR 1 & turbo 1&2)
                     52, 53, 54, 55, 56, 57, 70, 71, 72, -9, -9, -9, -9, -9, -9, &   !Check 13 IDs (ADEAC & turbo 1&2)
					 58, 59, 60, 70, 71, 72, 73, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 14 IDs (ADEAC & CEGR) 
					 61, 62, 63, 65, 66, 67, 68, 69, 70, 71, 72, -9, -9, -9, -9, &   !Check 15 IDs (HCR 2, HCR 1+, HCR 1 & ADEAC)
                     52, 53, 54, 55, 56, 57, 73, 74, 75, 76, 77, 78, -9, -9, -9, &   !Check 16 IDs (turbo downsize 1&2 & turbo 1&2)
                     58, 59, 60, 70, 71, 72, 73, 74, 75, 76, 77, 78, -9, -9, -9, &   !Check 17 IDs (turbo downsize 1&2 & ADEAC & CEGR)
                     61, 62, 63, 64, 65, 66, 67, 68, 69, 73, 74, 75, 76, 77, 78, &   !Check 18 IDs (HCR 2, HCR 1+, HCR 1 & turbo downsize 1&2)
                     79, 80, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, &   !Check 19 IDs (EPS & IACC)
                     81, 82, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, -9/     !Check 20 IDs (SS12V & BISG)	  
                     
      IF (PASS .EQ. 1) TEC_ORNL = 0.0

! ... If this is the FEM base year or earlier, we will jump over the core FEM
! ... calculations.  However, there are several periperhal calculations that
! ... should be implemented even for pre-FEM estimate years.  Such calculations
! ... generally involve setting up lagged parameters and validating historic year
! ... inputs.  For example, the calculation of vehicle range and other NEMS statistics
! ... requires the definition of FEM "current year" data regardless of whether or not
! ... the current year is before or after the FEM base year.  For years up to the FEM
! ... base year, these data must first be extracted from the report writer or base year
! ... arrays since years through the FEM base year (XYR=2016) are never actually
! ... processed through the main portion of FEMCALC.

! ... On the first pass of the first TRAN iteration for ALL years, all "current year"
! ... data (which are actually data from the final pass/iteration for the previous year
! ... for all years after the FEM base year) must be copied into the "previous year"
! ... arrays so that they are available as the basis for the new evaluation year
! ... updates.  This assignment is required even for years prior to the FEM base year
! ... to provide for cross-iteration stability (even pre-base year data can be altered
! ... through calibration).

      IF (CURITR .EQ. 1 .AND. PASS .EQ. 1) THEN
        IF (YRS .LT. XYR) THEN                        ! years up through the FEM base year
          DO ILDV=1,MAXLDV
            DO IGP=1,MAXGROUP
              DO ICL=1,MAXCLASS
                FE(ICL,IGP,CURRENT,ILDV)       = FEMMPG(IGP,ICL,YRS,ILDV)
                WEIGHT(ICL,IGP,CURRENT,ILDV)   = FEMWGT(IGP,ICL,YRS,ILDV)
                PRICE(ICL,IGP,CURRENT,ILDV)    = FEMPRI(IGP,ICL,YRS,ILDV)
                PRICEHI(ICL,IGP,CURRENT,ILDV)  = FEMPRIH(IGP,ICL,YRS,ILDV)
                HP(ICL,IGP,CURRENT,ILDV)       = FEMHP(IGP,ICL,YRS,ILDV)
                VOLUME(ICL,IGP,CURRENT,ILDV)   = FEMVOL(IGP,ICL,YRS,ILDV)
                TANKSIZE(ICL,IGP,CURRENT,ILDV) = FEMTSZ(IGP,ICL,YRS,ILDV)
                RANGE(ICL,IGP,CURRENT,ILDV)    = FEMRNG(IGP,ICL,YRS,ILDV)

                if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) ElecSysIncCost(ICL,IGP,CURRENT,ILDV)=0.0
                DO ITECH=1,NUMTECH
                  MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) = 0.0 
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ELSEIF (YRS .EQ. XYR) THEN                    ! FEM base year
          DO ILDV=1,MAXLDV
            DO IGP=1,MAXGROUP
              DO ICL=1,MAXCLASS
                FE(ICL,IGP,CURRENT,ILDV)       = FE(ICL,IGP,BASE,ILDV)
                WEIGHT(ICL,IGP,CURRENT,ILDV)   = WEIGHT(ICL,IGP,BASE,ILDV)
                PRICE(ICL,IGP,CURRENT,ILDV)    = PRICE(ICL,IGP,BASE,ILDV)
                PRICEHI(ICL,IGP,CURRENT,ILDV)  = PRICEHI(ICL,IGP,BASE,ILDV)
                HP(ICL,IGP,CURRENT,ILDV)       = HP(ICL,IGP,BASE,ILDV)
                VOLUME(ICL,IGP,CURRENT,ILDV)   = VOLUME(ICL,IGP,BASE,ILDV)
                TANKSIZE(ICL,IGP,CURRENT,ILDV) = TANKSIZE(ICL,IGP,BASE,ILDV)
                RANGE(ICL,IGP,CURRENT,ILDV)    = 0.0

                if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) ElecSysIncCost(ICL,IGP,CURRENT,ILDV) = ElecSysIncCost(ICL,IGP,BASE,ILDV)

                DO ITECH=1,NUMTECH
                  MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) = MKT_PEN(ICL,IGP,ITECH,BASE,ILDV)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDIF

        DO ILDV=1,MAXLDV                            ! set previous year data only once for each evaluation year
          DO IGP=1,MAXGROUP                           ! to provide necessary cross-iteration stability
            DO ICL=1,MAXCLASS
              FE(ICL,IGP,PREV,ILDV)       = FE(ICL,IGP,CURRENT,ILDV)
              WEIGHT(ICL,IGP,PREV,ILDV)   = WEIGHT(ICL,IGP,CURRENT,ILDV)
              PRICE(ICL,IGP,PREV,ILDV)    = PRICE(ICL,IGP,CURRENT,ILDV)
              PRICEHI(ICL,IGP,PREV,ILDV)  = PRICEHI(ICL,IGP,CURRENT,ILDV)
              HP(ICL,IGP,PREV,ILDV)       = HP(ICL,IGP,CURRENT,ILDV)
              VOLUME(ICL,IGP,PREV,ILDV)   = VOLUME(ICL,IGP,CURRENT,ILDV)
              TANKSIZE(ICL,IGP,PREV,ILDV) = TANKSIZE(ICL,IGP,CURRENT,ILDV)
              RANGE(ICL,IGP,PREV,ILDV)    = RANGE(ICL,IGP,CURRENT,ILDV)

              if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) ElecSysIncCost(ICL,IGP,PREV,ILDV) = ElecSysIncCost(ICL,IGP,CURRENT,ILDV)

              DO ITECH=1,NUMTECH
                MKT_PEN(ICL,IGP,ITECH,PREV,ILDV) = MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        
!...For aggressive consumer behavior
      if(n.ge.23.and.CONSUMER.eq.1)then
        PAYBACK  = 5.0
        DISCOUNT = 0.07
      endif

    ENDIF

! ... Set fuel cost and income.  Lagged fuel costs (up to 7 years) and income (up to 1
! ... year) are used in FEM calcs, so these parameters must be set for years leading up
! ... to the FEM base year.  Right now, with the TRAN base year (BYR=1995) being a full
! ... 10 years ahead of the FEM base year (XYR=2016), simply setting fuel cost and income
! ... for each year from the TRAN base year on provides all the lag data necessary.  If
! ... the two base year ever converge, special processing may be required to initialize
! ... the necessary lagged data.

      do iregn = 1,mnumcr
          PMGTR90$(iregn,YRS) = (PMGTR(iregn,N)*MG_HHV/1000.0) * MC_JPGDP(1)
	  enddo
      if(yrs.eq.2009)pmgtr90$(11,yrs)=pmgtr90$(11,2008)*0.92

! ... For each technology, the expected fuel savings associated with incremental fuel
! ... economy impacts is calculated.  This calculation occurs below within a technology
! ... evaluation loop, but the fuel costs on which the calculation is dependent are
! ... fixed annually so that continual recalculation within the technology loop is
! ... redundant (and inefficient).  Accordingly, the basic fuel cost calculations are
! ... included here.  Nominally, fuel costs three years ago and the annual rate of fuel
! ... price change are used to estimate expected dollar savings.  However, since prices
! ... can spike and since manufacturing decisions will not be based on one-year spikes,
! ... the "three year ago" and "rate of change" prices used for this calculation are
! ... actually the "five year running average price" and the "difference between the
! ... three year ago five year average price and the four year ago five year average
! ... price."  Thus, the effect of short term transients is buffered.

      IF (YRS .GT. XYR) THEN              ! lagged parameters are not needed until after the FEM base year
        FIVEYR_FUELCOST = 0.0
        DO IYR=(YRS-8),(YRS-4)
          FIVEYR_FUELCOST(2) = FIVEYR_FUELCOST(2) + PMGTR90$(11,IYR)
        ENDDO
        FIVEYR_FUELCOST(1) = FIVEYR_FUELCOST(2) - PMGTR90$(11,YRS-8) + PMGTR90$(11,YRS-3)
        FIVEYR_FUELCOST(1) = FIVEYR_FUELCOST(1) / 5.0
        FIVEYR_FUELCOST(2) = FIVEYR_FUELCOST(2) / 5.0
        PSLOPE = MAX(0.0,FIVEYR_FUELCOST(1)-FIVEYR_FUELCOST(2))
      ENDIF

! ... Initiate FEM processing loop (loop through each fuel type, vehicle group,
! ... vehicle class, and fuel economy technology).

      DO ILDV=1,MAXLDV
        DO IGP=1,MAXGROUP

! ... In the 1st call to FEMCALC, CAFEPASS(IGP) should always be FALSE (i.e., none
! ... of the nine CAFE groups have demonstrated compliance).  In subsequent calls
! ... values may be TRUE or FALSE depending on the compliance status of each CAFE group.

!...      For GHG CAFE
          if(curcalyr.ge.XYR)then
            Payback = 10.0 !10
            Discount = 0.05
          elseif(curcalyr.ge.2027)then
            PAYBACK = 4.0 !5.0
            DISCOUNT = 0.07
          elseif(curcalyr.ge.2035)then
            Payback = 2.0 !3
            Discount = 0.1
          endif
        

          IF (CAFEPASS(IGP)) CYCLE

          DO ICL=1,MAXCLASS
            IF (.NOT. CLASSFLAG(ICL,IGP,ILDV)) CYCLE    ! skip loop if no vehicles in the class

! ... Set vehicle type index for correct reference to arrays that are type, rather
! ... than group specific.
            IVTYP=GrpMap(IGP)

! ... Set the initial estimates for each years fuel efficiency, weight, price,
! ... volume, and fuel tank size equal to the previous year's values before
! ... considering new technologies.

            FE(ICL,IGP,CURRENT,ILDV)       = FE(ICL,IGP,PREV,ILDV)
            WEIGHT(ICL,IGP,CURRENT,ILDV)   = WEIGHT(ICL,IGP,PREV,ILDV)
            PRICE(ICL,IGP,CURRENT,ILDV)    = PRICE(ICL,IGP,PREV,ILDV)
            PRICEHI(ICL,IGP,CURRENT,ILDV)  = PRICEHI(ICL,IGP,PREV,ILDV)
            HP(ICL,IGP,CURRENT,ILDV)       = HP(ICL,IGP,PREV,ILDV)
            VOLUME(ICL,IGP,CURRENT,ILDV)   = VOLUME(ICL,IGP,PREV,ILDV)
            TANKSIZE(ICL,IGP,CURRENT,ILDV) = TANKSIZE(ICL,IGP,PREV,ILDV)

! ... Jump over core FEM processing if this is FEM base year or earlier, but
! ... perform market share sum checks for related technologies to ensure reliable
! ... input data.  Note also that this jump could be performed prior to the
! ... attribute resets (i.e. CURRENT = PREV) above, but the resets are run first
! ... to make data in and out of FEM "perfectly" stable across iterations.  Output
! ... would be stable either way (as calibration factors "go to one" for 2nd and
! ... later iterations), but this approach makes debugging easier/cleaner.

            IF (YRS .LE. XYR) GO TO 1100

! ... For electric power train vehicles subtract last year's electric storage cost from vehicle price.

            if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) then
               price(ICL,IGP,CURRENT,ILDV)   = price(ICL,IGP,CURRENT,ILDV)- &
                                                ElecSysIncCost(ICL,IGP,PREV,ILDV)
               pricehi(ICL,IGP,current,ILDV) = pricehi(ICL,IGP,CURRENT,ILDV)- &
                                                ElecSysIncCost(ICL,IGP,PREV,ILDV)
            endif

! ... Calculate possible market share in the absence of any engineering notes

! ... Initialize the value of PERF_COEFF, the parameter used to constrain the
! ... incremental value of additional vehicle performance.  This parameter, which
! ... is independent of technology and can thus be set prior to beginning the main
! ... technology evaluation loop, increases as performance increases so that the
! ... incremental value of additional performance declines.  Since the value of
! ... performance is based on 1990 data, the consumer performance demand function
! ... also uses a base of 1990.  However, since the base data year is 2000, the
! ... demand that has already accrued between 1990 and 2000 must be accounted for
! ... through the use of parameter USEDCAP.

            IF (WEIGHT(ICL,IGP,BASE,ILDV) .NE. 0.0) THEN
              HP_WGT_BASE = HP(ICL,IGP,BASE,ILDV) / WEIGHT(ICL,IGP,BASE,ILDV)
            ELSE
              WRITE(6,*) 
              WRITE(6,*) ' Divisor in FEMCALC PERF_COEFF calc equals zero - RUN ABORTED.'
              WRITE(6,*)
              WRITE(6,*) ' --- At ABORT, index parameters were:'
              WRITE(6,*)
              WRITE(6,*) '   YRS   = ',YRS
              WRITE(6,*) '   ICL   = ',ICL
              WRITE(6,*) '   IGP   = ',IGP
              WRITE(6,*) '   ILDV = ',ILDV
              WRITE(6,*)
              WRITE(6,*) ' --- the offending denominator was:'
              WRITE(6,*)
              WRITE(6,*) '   WEIGHT(ICL,IGP,BASE,ILDV) = ',WEIGHT(ICL,IGP,BASE,ILDV)
              WRITE(6,*)
              WRITE(6,*) ' --- and the associated numerator was:'
              WRITE(6,*)
              WRITE(6,*) '   HP(ICL,IGP,BASE,ILDV)     = ',HP(ICL,IGP,BASE,ILDV)
              STOP 505
            ENDIF

            IF (WEIGHT(ICL,IGP,CURRENT,ILDV) .NE. 0.0) THEN
              HP_WGT = HP(ICL,IGP,CURRENT,ILDV) / WEIGHT(ICL,IGP,CURRENT,ILDV)
            ELSE
              WRITE(6,*) 
              WRITE(6,*) ' Divisor in FEMCALC PERF_COEFF calc equals zero - RUN ABORTED.'
              WRITE(6,*)
              WRITE(6,*) ' --- At ABORT, index parameters were:'
              WRITE(6,*)
              WRITE(6,*) '   YRS   = ',YRS
              WRITE(6,*) '   ICL   = ',ICL
              WRITE(6,*) '   IGP   = ',IGP
              WRITE(6,*) '   ILDV = ',ILDV
              WRITE(6,*)
              WRITE(6,*) ' --- the offending denominator was:'
              WRITE(6,*)
              WRITE(6,*) '   WEIGHT(ICL,IGP,CURRENT,ILDV) = ',WEIGHT(ICL,IGP,CURRENT,ILDV)
              WRITE(6,*)
              WRITE(6,*) ' --- and the associated numerator was:'
              WRITE(6,*)
              WRITE(6,*) '   HP(ICL,IGP,CURRENT,ILDV)     = ',HP(ICL,IGP,CURRENT,ILDV)
              STOP 506
            ENDIF

            IF (USEDCAP(ICL,IGP) .GE. 0.0 .AND. USEDCAP(ICL,IGP) .LT. 1.0) THEN
              DEMAND_USED = (PERFCAP(ICL,IGP) - HP_WGT_BASE) * (USEDCAP(ICL,IGP)/(1.0-USEDCAP(ICL,IGP)))
            ELSE
              WRITE(6,*) 
              WRITE(6,*) ' Divisor in FEMCALC PERF_COEFF calc is out-of-range - RUN ABORTED.'
              WRITE(6,*)
              WRITE(6,*) ' --- At ABORT, index parameters were:'
              WRITE(6,*)
              WRITE(6,*) '   YRS   = ',YRS
              WRITE(6,*) '   ICL   = ',ICL
              WRITE(6,*) '   IGP   = ',IGP
              WRITE(6,*) '   ILDV = ',ILDV
              WRITE(6,*)
              WRITE(6,*) ' --- the offending parameter (USEDCAP), which must be greater'
              WRITE(6,*) '     than or equal to zero and less than one, was:'
              WRITE(6,*)
              WRITE(6,*) '   USEDCAP(ICL,IGP) = ',USEDCAP(ICL,IGP)
              STOP 508
            ENDIF

            IF (PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED .NE. 0.0) THEN
              PERF_COEFF  = 1.0 - ((HP_WGT           - HP_WGT_BASE + DEMAND_USED) / &
                                   (PERFCAP(ICL,IGP) - HP_WGT_BASE + DEMAND_USED))
              PERF_COEFF  = MIN(1.0,PERF_COEFF)
              PERF_COEFF  = MAX(0.0,PERF_COEFF)
            ELSE
              WRITE(6,*) 
              WRITE(6,*) ' Divisor in FEMCALC PERF_COEFF calc equals zero.'
              WRITE(6,*)
              WRITE(6,*) ' --- Index parameters at the time of this disconcerting occurrence were:'
              WRITE(6,*)
              WRITE(6,*) '   YRS   = ',YRS
              WRITE(6,*) '   ICL   = ',ICL
              WRITE(6,*) '   IGP   = ',IGP
              WRITE(6,*) '   ILDV = ',ILDV
              WRITE(6,*)
              WRITE(6,*) ' --- the offending denominator was:'
              WRITE(6,*)
              WRITE(6,*) '   PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED = ',PERFCAP(ICL,IGP) - HP_WGT_BASE  + DEMAND_USED
              WRITE(6,*)
              WRITE(6,*) ' --- and the associated numerator was:'
              WRITE(6,*)
              WRITE(6,*) '   HP_WGT-HP_WGT_BASE+DEMAND_USED           = ',HP_WGT - HP_WGT_BASE + DEMAND_USED

              IF (CURIYR .GT. 1) THEN  ! Post 2030 keep the model running
                 IF (PERFCAP(ICL,IGP) .EQ. HP_WGT_BASE) THEN
                    PERF_COEFF = 0.01
                    PERFCAP(ICL,IGP) = PERFCAP(ICL,IGP) + 0.01
                    WRITE(6,*) 
                    WRITE(6,*) ' Divisor in FEMCALC PERF_COEFF calc equals zero -- RUN NOT ABORTED.'
                    WRITE(6,*) ' (Rather than make a big scene maybe i can get something to work.)' 
                    WRITE(6,*)
                    
                 END IF
              ELSE
                 WRITE(6,*) 
                 WRITE(6,*) ' Divisor in FEMCALC PERF_COEFF calc equals zero -- RUN ABORTED.'
                 WRITE(6,*)
                 STOP 507
              ENDIF
            ENDIF

            DO ITECH=1,NUMTECH

! ... The following initialization statements are used to define several
! ... parameters that are later referenced in determining price, fuel economy,
! ... hp, etc.  Since some technologies are skipped under certain conditions,
! ... the affected parameters might otherwise be undefined (or worse, retain
! ... values from previous iterations).

              MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) = MKT_PEN(ICL,IGP,ITECH,PREV,ILDV)
              ACTUAL_MKT(ITECH) = MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV)
              MMAX(ITECH) = MKT_MAX(ICL,IGP,ITECH,ILDV)
              MKT_PERF(ITECH) = 0.0
              MKT_FUEL(ITECH) = 0.0

              ACTUAL_MKT(ITECH) = 0.0

! ... Skip non-applicable technologies.
              IF(.NOT. TECH_APPLIC(ITECH,IVTYP,ILDV)) CYCLE

! ... Set tech penetration limiting parameter (OLD_PMAX).  Penetration curve should be
! ... accelerated if CAFE is not met, therefore OLD_PMAX should be set (updated) on both
! ... the first and second passes through FEM.  No further acceleration is appropriate
! ... on pass three, which simply "trades" performance gains for fuel economy.  Pass three
! ... constraints are handled further on in the code by restricting updates to OLD_PMAX.
! ... OLD_PMAX must also be reset for each iteration, so OLD_PMAX(...,1) is used to hold
! ... the first iteration/first pass value for each year so that it can be restored as
! ... necessary.  Within year processing is controlled via OLD_PMAX(...,2).

              IF (CURITR .EQ. 1 .AND. PASS .EQ. 1) THEN    ! 1st iter/1st pass -- store reset value (last value for year-1)
                OLD_PMAX(ICL,IGP,ITECH,ILDV,1) = OLD_PMAX(ICL,IGP,ITECH,ILDV,2)
              ELSEIF (PASS .EQ. 1) THEN                    ! subsequent iter/1st pass -- restore 1st iter/1st pass value
                OLD_PMAX(ICL,IGP,ITECH,ILDV,2) = OLD_PMAX(ICL,IGP,ITECH,ILDV,1)
              ENDIF

              IF (YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE

! ************************************ TESTING ONLY ******************************
!   IF (CURITR.EQ.1.AND.N.GE.16.AND.ILDV.EQ.1.AND.ICL.EQ.3.AND.IGP.LE.2.AND.PASS.GE.1.AND.ITECH.EQ.2)&
!    WRITE(6,*)'PASS-MKT-PMAX1-PMAX2',PASS, MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV), &
!           OLD_PMAX(ICL,IGP,ITECH,ILDV,1), OLD_PMAX(ICL,IGP,ITECH,ILDV,2)
! ************************************ TESTING ONLY ******************************

! ... Calculate expected fuel savings associated with incremental fuel economy.
              FUELSAVE(ITECH) = 0.0
              CFE = FE(ICL,IGP,PREV,ILDV)

              DO I=1,PAYBACK

                if(IGP.le.cargrp) VMT(I,mnumcr)=PVMT(I,STOCKYR-1989,mnumcr,1)	! Use the last available VMT schedule available (STOCKYR). Assume gas ICE VMT (need consistent assumption)
                if(IGP.ge.ltkgrp) VMT(I,mnumcr)=LVMT(I,STOCKYR-1989,mnumcr,1)
				
                PRICE_EX(I) = PSLOPE * (I+2) + FIVEYR_FUELCOST(1)
                IF (CFE .NE. 0.0) FUELSAVE(ITECH) = FUELSAVE(ITECH) + &
                                                    VMT(I,mnumcr) *          &
                                                   (1/CFE - (1/((1+DEL_FE(ITECH,IVTYP))*CFE))) * &
                                                    PRICE_EX(I) *     &
                                                   (1+DISCOUNT)**(-I)	
              ENDDO

!...          calculate incremental technology cost of specific technology starting in base year technology
!...          update: this year flag must be updated if technology base year attributes are set to a year other than 2010
              
!...          absolute technology cost
              TECHCOST(ITECH) = DEL_COSTABS(ITECH,IVTYP)            
!...          absolute weight-based technology cost
              SIGN = 1
              if (DEL_WGTABS(ITECH,IVTYP) .lt. 0.0) SIGN = -1       
              TECHCOST(ITECH) = TECHCOST(ITECH) +  &
                                (DEL_COSTWGT(ITECH,IVTYP) * DEL_WGTABS(ITECH,IVTYP) * SIGN)
!...          weight-based technology cost
              SIGN = 1
              if (DEL_WGTWGT(ITECH,IVTYP) .lt. 0.0) SIGN = -1       
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
                learnyear(itech,IGP)=2011
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

! ... Save technology cost for use in subroutine TLEGIS.

  500         TEC_ORNL(ICL,IGP,ITECH,ILDV) = TECHCOST(ITECH)

! ... Estimate the value of performance associated with the technology.  Scale the
! ... the value of performance downward (using PERF_COEFF) as HP/WGT increases to
! ... reflect the decreasing incremental value of more performance.
              VAL_PERF(ITECH) = 0.0
              IF (CFE .NE. 0.0) VAL_PERF(ITECH) = VALUEPERF(ICL,IGP) * PERF_COEFF * &
                                                  INC90$NP(11,YRS)/INC90$NP(11,YRS-1) *       &
                                                 (1+DEL_FE(ITECH,IVTYP)) *          &
                                                  PMGTR90$(11,YRS-1)/PMGTR90$(11,YRS) *   &
                                                  DEL_HP(ITECH,IVTYP)

! ... Calculate the cost effectiveness based on fuel savings and performance.
              IF (TECHCOST(ITECH) .GT. 0.0) THEN     ! if tech costs money, estimate cost effectiveness
                COSTEF_FUEL(ITECH) = (FUELSAVE(ITECH) - TECHCOST(ITECH) +    &
                                      REGCOST(IGP) * CFE * DEL_FE(ITECH,IVTYP)) / &
                                      TECHCOST(ITECH)

                COSTEF_PERF(ITECH) = -80.0

                IF (VAL_PERF(ITECH) .NE. 0.0) COSTEF_PERF(ITECH) = (VAL_PERF(ITECH) -  &
                                                                    TECHCOST(ITECH)) / &
                                                                    TECHCOST(ITECH)

                COSTEF_FUEL(ITECH) = MAX(-80.0,COSTEF_FUEL(ITECH))
                COSTEF_PERF(ITECH) = MAX(-80.0,COSTEF_PERF(ITECH))

              ELSE                                   ! if tech has zero or negative cost, either 100%
                                                     ! effective or ineffective based on savings/performance
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

!...Calculate the economic market share for fuel saving technology/performance. The cost effectiveness 
!...coefficient in the fuel market penetration equation varies between -2 and -4 depending on whether 
!...cost effectiveness is greater than or less than zero.  Originally a value of -2 was set under both
!...conditions, but at this value, even techs producing NO benefits derive a market penetration of 12 
!...percent, REGARDLESS OF COST.  With a coefficient -4, this "no benefit" penetration drops to a more 
!...reasonable 2 percent. Ideally, some measure of the absolute cost differential should be introduced
!...into the algorithm since a consumer is more likely to accept a cheap technology with poor payback 
!...than an expensive one (i.e., getting $10 back on a $20 investment is more palatable than getting 
!...$1500 back on $3000 invested). To some extent, the implemented approach accomplishes this since fuel 
!...savings vary less than tech costs, making the high cost techs more likely candidates for lower relative 
!...cost effectiveness estimates.

              MKT_COEFF = -2.0
              IF (COSTEF_FUEL(ITECH) .LT. 0.0) MKT_COEFF = -4.0

              MKT_FUEL(ITECH) = 1/(1+EXP(MKT_COEFF*COSTEF_FUEL(ITECH)))

              MKT_COEFF = -2.0
              IF (COSTEF_PERF(ITECH) .LT. 0.0) MKT_COEFF = -4.0

!if (MKT_COEFF*COSTEF_PERF(ITECH) .gt. 75.0) &
! write(6,'(" preventing taking exponent of ",I4,F14.3,F14.3,F14.3)') ITECH,MKT_COEFF,COSTEF_PERF(ITECH),MKT_COEFF*COSTEF_PERF(ITECH)
              MAX_TAKE_EXP = MIN(MKT_COEFF*COSTEF_PERF(ITECH),75.0)
              MKT_PERF(ITECH) = 1/(1+EXP(MAX_TAKE_EXP))

!...Calculate the actual economic market share.  PMAX defines the fraction of vehicle makes and models 
!...on which the technology is available and MKT_FUEL and MKT_PERF define the percentage of those model 
!...buyers who desire the technology from a cost effectiveness standpoint.  So the actual estimated
!...market share is the product of these two influences, constrained by the maximum market share and 
!..."no backsliding."

              ACTUAL_MKT(ITECH) = OLD_PMAX(ICL,IGP,ITECH,ILDV,2) * &
                                   MAX(MKT_FUEL(ITECH),MKT_PERF(ITECH))
              ACTUAL_MKT(ITECH) = MAX(MKT_PEN(ICL,IGP,ITECH,PREV,ILDV),ACTUAL_MKT(ITECH))
              ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),MMAX(ITECH),1.0)

            ENDDO   ! end technology (ITECH) loop

! ... Apply mandatory and supersedes engineering notes.
            DO ITECH=1,NUMTECH
              IF (YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE

              DO INOTE=1,NUM_MAN      ! loop through mandatory notes
                IF (MANDYEAR(1,INOTE) .EQ. ITECH .AND. YRS .GE. MANDYEAR(2,INOTE)) THEN

!...If a non-econometric technology (i.e., MAND_ORIDE is TRUE), zero any econometrically calculated market share.
                  IF (MAND_ORIDE(INOTE)) ACTUAL_MKT(ITECH) = 0.0

!...If the number of phase-in years is between 0 and 1, adopt the full market share immediately.  Since the 
!...maximum market penetration allowance can vary by vehicle class, the actual market share logic must 
!...consider the mandatory share, not in isolation, but in conjunction with the maximum allowable share
!...for the vehicle class.
                  IF (MANDYEAR(3,INOTE) .LE. 1) THEN

                    ACTUAL_MKT(ITECH) = MAX(MANDMKSH(INOTE),ACTUAL_MKT(ITECH))
                    ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),MKT_MAX(ICL,IGP,ITECH,ILDV))

                  ELSE

!...If the number of phase-in years is greater than 1, adopt a proportional share of the total mandatory share
!...each year.  Since both the base and maximum market penetrations can vary by vehicle class, the actual market
!...share logic must adopt annual shares in proportion to the allowable market share spread for each vehicle
!...class, with the minimum market share defined by the base share for the class.
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

              IF (RETURN_STAT .LT. 0) THEN
                RETURN_STAT = RETURN_STAT * (-1)
                I = MOD(RETURN_STAT,100)
                INOTE = RETURN_STAT/100
                WRITE (6,*)
                WRITE (6,*) '======================================'
                WRITE (6,*)
                WRITE (6,*) 'Logic Error in Supersedes Algorithm,'
                WRITE (6,*) 'Market Penetration is Less Than Zero !!!'
                WRITE (6,*)
                WRITE (6,*) 'Year             = ',YRS
                WRITE (6,*) 'Vehicle Group    = ',GROUPLABEL(IGP)
                WRITE (6,*) 'Vehicle Class    = ',CLASSLABEL(ICL,IGP)
                WRITE (6,*) 'Technology ID    = ',TECHLABEL(ITECH,IVTYP)
                WRITE (6,*) 'Mkt Penetration  = ',ACTUAL_MKT(ITECH)
                write (6,*) 'max mkt          = ',mmax(itech)
                write (6,*) 'mkt_max          = ',MKT_MAX(ICL,IGP,ITECH,ILDV)
                WRITE (6,*)
                WRITE (6,*) 'Superseded Market Penetrations are as follows:'
                WRITE (6,*) '(after algorithm short-circuit at tech def ',I,')'
                WRITE (6,*)
                DO J = 1,TECH_CNT(INOTE)
                WRITE (6,*) 'Technology ID    = ',TECHLABEL(SUPERSEDES(J,INOTE),IVTYP)
                WRITE (6,*) 'Mkt Penetration  = ',ACTUAL_MKT(SUPERSEDES(J,INOTE))
                WRITE (6,*)
                ENDDO
                WRITE (6,*) '  ***** Run ABORTED *****'
                WRITE (6,*)
                WRITE (6,*) 'Fix Program Logic and Rerun'
                WRITE (6,*)
                WRITE (6,*) '======================================'
                STOP
              ENDIF

            ENDDO   ! end technology (ITECH) loop for mandatory and supersedes notes

            DO ITECH=1,NUMTECH        ! loop through and apply required engineering notes
              IF (YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE
              REQUIRED = .FALSE.
              REQ_MKT  = 0.0
              DO INOTE=1,NUM_REQ
                IF (REQUIRES(1,INOTE) .EQ. ITECH) THEN
                  REQUIRED = .TRUE.
                  REQ_MKT  = REQ_MKT + MKT_PEN(ICL,IGP,REQUIRES(2,INOTE),CURRENT,ILDV)
                ENDIF
              ENDDO
              IF (REQUIRED) THEN
                REQ_MKT = MIN(REQ_MKT,1.0)
                ACTUAL_MKT(ITECH) = MIN(ACTUAL_MKT(ITECH),REQ_MKT)
              ENDIF

!...Note, this loop also very surreptitiously assigns the MKT_PEN value for all technologies.  It's highly
!...important, but easy to miss "hidden" within a loop otherwise "advertised" as processing required 
!...engineering notes.

              MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) = ACTUAL_MKT(ITECH)

            ENDDO   ! end technology (ITECH) loop for required notes

!...Loop through and apply the synergy engineering notes
            DO ITECH=1,NUMTECH        
              IF (YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE

              SYNERGY_LOSS(ITECH) = 0.0

!...Market share affected by synergy effects between two technologies is estimated as the probabilistic overlap
!...between the market shares of the two technologies. Mathematically, this market share is expressed as the 
!...product of the market shares of the two technologies.  The incremental market share overlap for a single year
!...is equal to the cumulative estimated overlap (based on cumulative estimated market penetrations) for the 
!...current year minus the cumulative estimated overlap for the previous year.  Note also, that the input value
!...of SYNR_DEL is negative so that the estimated synergy loss will also be negative and should be treated as an
!...additive parameter.

              DO INOTE=1,NUM_SYN
                IF (SYNERGY(1,INOTE) .EQ. ITECH) THEN

                  DELTA_MKT = (MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) *             &
                               MKT_PEN(ICL,IGP,SYNERGY(2,INOTE),CURRENT,ILDV)) - &
                              (MKT_PEN(ICL,IGP,ITECH,PREV,ILDV) *                &
                               MKT_PEN(ICL,IGP,SYNERGY(2,INOTE),PREV,ILDV))
                
                  IF (DEL_FE(ITECH,IVTYP) .LT. ABS(SYNR_DEL(INOTE))) THEN
                    WRITE (6,*)
                    WRITE (6,*) '==============================================================='
                    WRITE (6,*)
                    WRITE (6,*) 'Logic Error in Synergy Algorithm,'
                    WRITE (6,*) 'Synergy Loss is Greater than Unadjusted Fuel Economy Impact !!!'
                    WRITE (6,*)
                    WRITE (6,*) 'Year             = ',YRS
                    WRITE (6,*) 'Vehicle Group    = ',GROUPLABEL(IGP)
                    WRITE (6,*) 'Vehicle Class    = ',CLASSLABEL(ICL,IGP)
                    WRITE (6,*) 'Technology ID 1  = ',TECHLABEL(ITECH,IVTYP)
                    WRITE (6,*) 'Technology ID 2  = ',TECHLABEL(SYNERGY(2,INOTE),IVTYP)
                    WRITE (6,*) 'DELTA_MKT        = ',DELTA_MKT
                    WRITE (6,*) 'DEL_FE           = ',DEL_FE(ITECH,IVTYP)
                    WRITE (6,*) 'SYNR_DEL         = ',SYNR_DEL(INOTE)
                    WRITE (6,*) 'Net FE Effect    = ',DEL_FE(ITECH,IVTYP) + SYNR_DEL(INOTE)
                    WRITE (6,*)
                    WRITE (6,*) '  ***** Run ABORTED *****'
                    WRITE (6,*)
                    WRITE (6,*) 'Fix Program Logic and Rerun'
                    WRITE (6,*)
                    WRITE (6,*) '==============================================================='
                    STOP 291
                  ENDIF

                  SYNERGY_LOSS(ITECH) = SYNERGY_LOSS(ITECH) + (DELTA_MKT * SYNR_DEL(INOTE))

                ENDIF
              ENDDO
            ENDDO   ! end technology (ITECH) loop for synergy notes

!...Repeat the technology loop one last time to aggregate the impacts of changes in technology in market share.

            TECH_ADJHP  = 0.0
            HP_GIVEBACK = 0.0

            DO ITECH=1,NUMTECH
!...skip technology not applicable to vehicle type (by fuel type)
              IF(.NOT. TECH_APPLIC(ITECH,IVTYP,ILDV)) CYCLE

              IF (YRS .LT. FRSTYEAR(ITECH,IGP)) CYCLE
              DELTA_MKT = MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) - &
                          MKT_PEN(ICL,IGP,ITECH,PREV,ILDV)
!...For pure non-econometric consumer driven techs (e.g., forced size/weight increase), set DELTA_MKT to zero if
!...this is a CAFE pass through FEMCALC (i.e., PASS > 1).  Do not change the actual MKT_PEN values since this 
!...will screw up the tech phase-in for subsequent years.  Setting DELTA_MKT will nullify both the FE and WGT 
!...impacts of these techs for this year without creating compensating changes in subsequent years.

              FE(ICL,IGP,CURRENT,ILDV) = FE(ICL,IGP,CURRENT,ILDV) + &
                                          FE(ICL,IGP,PREV,ILDV) *    &
                                        ((DELTA_MKT * DEL_FE(ITECH,IVTYP)) + SYNERGY_LOSS(ITECH))

              WEIGHT(ICL,IGP,CURRENT,ILDV) = WEIGHT(ICL,IGP,CURRENT,ILDV) + &
                                             (DELTA_MKT *                     &
                                             (DEL_WGTABS(ITECH,IVTYP) +       &
                                             (WEIGHT(ICL,IGP,CURRENT,ILDV) * DEL_WGTWGT(ITECH,IVTYP))))

! ************************************ TESTING ONLY ******************************
!   IF (CURITR.EQ.1.AND.N.GE.16.AND.ILDV.EQ.1.AND.ICL.EQ.3.AND.IGP.LE.2.AND.PASS.GE.2.AND.ITECH.EQ.2)&
!      WRITE(6,*) 'DELMKT_SYN',DELTA_MKT,(SYNERGY_LOSS(I),I=1,64), &
!      'FE1-WGT1',FE(ICL,IGP,CURRENT,ILDV),WEIGHT(ICL,IGP,CURRENT,ILDV)
! ************************************ TESTING ONLY ******************************
 

              PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) +    &
                                            (DELTA_MKT * TECHCOST(ITECH))

              PRICEHI(ICL,IGP,CURRENT,ILDV) = PRICEHI(ICL,IGP,CURRENT,ILDV) +  &
                                            (DELTA_MKT * TECHCOST(ITECH))

!...Calculate annual horsepower adjustment due to technology introduction alone. This is only part of overall 
!...horsepower adjustment, so final horsepower is calculated below, outside the technology loop.

              TECH_ADJHP = TECH_ADJHP + (DELTA_MKT * DEL_HP(ITECH,IVTYP))

            ENDDO   ! end market share impact loop

            TECH_MAX_ADJHP = TECH_ADJHP

!...Run checks on the total market penetration of related technologies to ensure that it does not exceed 100 percent.

 1100       DO I=1,20
              CHECKSUM = 0.0
              DO J=1,15
                IF (SUMCHECK(J,I) .EQ. -9) GO TO 1200
                IF (SUMCHECK(J,I) .GT. MAXTECH) THEN
                  WRITE (6,*)
                  WRITE (6,*) '============================================'
                  WRITE (6,*)
                  WRITE (6,*) 'Checksum Tech ID is Out of Range'
                  WRITE (6,*)
                  WRITE (6,*) 'Maximum Tech ID     = ',MAXTECH
                  WRITE (6,*) 'Encountered Tech ID = ',SUMCHECK(J,I)
                  WRITE (6,*) 'Sumcheck array row  = ',I
                  WRITE (6,*) 'Sumcheck array col  = ',J
                  WRITE (6,*)
                  WRITE (6,*) '         ***** Run ABORTED *****'
                  WRITE (6,*)
                  WRITE (6,*) 'Fix Sumcheck Array Definitions in Subroutine'
                  WRITE (6,*) 'FEMCALC (of the TRAN module) and Rerun'
                  WRITE (6,*)
                  WRITE (6,*) '============================================'
                  STOP 701
                ENDIF
                CHECKSUM = CHECKSUM + MKT_PEN(ICL,IGP,SUMCHECK(J,I),CURRENT,ILDV)
              ENDDO
              J = 16
 1200         J = J - 1
              IF (CHECKSUM .GT. 1.0+ROUNDOFF_ERROR) THEN                    
                WRITE (6,*)
                WRITE (6,*) '============================================'
                WRITE (6,*)
                WRITE (6,*) 'Related Tech Pen Sum is greater than One'
                WRITE (6,*)
                WRITE (6,*) 'Year             = ',YRS
                WRITE (6,*) 'Vehicle Group    = ',GROUPLABEL(IGP)
                WRITE (6,*) 'Vehicle Class    = ',CLASSLABEL(ICL,IGP)
                WRITE (6,*) 'Vehicle Type     = ',FTYPELABEL(ILDV)
                WRITE (6,*)
                WRITE (6,*) 'Checksum         = ',CHECKSUM
                WRITE (6,*)
                WRITE (6,*) 'Individual Tech Penetrations are as follows:'
                WRITE (6,*)
                DO K = 1,J
                WRITE (6,*) 'Technology ID    = ',TECHLABEL(SUMCHECK(K,I),IVTYP)
                WRITE (6,*) 'Market Share     = ',MKT_PEN(ICL,IGP,SUMCHECK(K,I),CURRENT,ILDV)
                write (6,*) 'max mkt          = ',mmax(itech)
                write (6,*) 'mkt_max          = ',MKT_MAX(ICL,IGP,ITECH,ILDV)               
                WRITE (6,*)
                ENDDO
                WRITE (6,*) '    ***** Run ABORTED *****'
                WRITE (6,*)
                WRITE (6,*) 'Fix Tech Market Share Matrix or'
                WRITE (6,*) 'Engineering Notes and Rerun'
                WRITE (6,*)
                WRITE (6,*) '============================================'
                STOP 702
              ENDIF
            ENDDO

!...Jump over remainder of FEM processing if this is FEM base year or earlier.

            IF (YRS .LE. XYR) CYCLE

 2000       CONTINUE

!...Electric drive vehicles have an additional price adjustments to account for battery and fuel cell cost.  

            if(ILDV.eq.4.OR.ILDV.EQ.7.OR.ILDV.EQ.15) call EVCALC (yrs)
            IF (ILDV .EQ. 8 .OR. ILDV .EQ. 16) CALL HEVCALC (yrs)
			IF (ILDV.EQ.5.OR.ILDV.EQ.6) CALL PHEVCALC (yrs)
			IF (ILDV.GE.13 .AND. ILDV .LE. 14) CALL FCCALC (yrs)
			
!...Electric power train vehicles do NOT have HP adjustments.  EV HP is set 20 percent below equivalent gasoline 
!...vehicles (adjusted for any weight difference).  HEV and PHEV HP is set to 10 percent below gasoline vehicles.
!...HP for "current" year gasoline vehicles will have already been set during an earlier loop iteration through FEMCALC.

            if(ILDV.eq.4.or.ILDV.eq.7.or.(ILDV.ge.13.and.ILDV.le.15)) then 
                if(WEIGHT(ICL,IGP,CURRENT,GAS).ne.0.0) then
                   HP(ICL,IGP,CURRENT,ILDV) = 0.8 * HP(ICL,IGP,CURRENT,GAS) * (WEIGHT(ICL,IGP,CURRENT,ILDV) / WEIGHT(ICL,IGP,CURRENT,GAS))
			    else
				   HP(ICL,IGP,CURRENT,ILDV) = 0.0
				endif
              CYCLE
            ENDIF

!...Initially set horsepower for constant performance level based on last year's power to weight ratio.

            IF (WEIGHT(ICL,IGP,PREV,ILDV) .NE. 0.0) THEN
              HP(ICL,IGP,CURRENT,ILDV) = (HP(ICL,IGP,PREV,ILDV)/WEIGHT(ICL,IGP,PREV,ILDV)) * WEIGHT(ICL,IGP,CURRENT,ILDV)
            ELSE
              HP(ICL,IGP,CURRENT,ILDV) = 0.0
            ENDIF

!...Estimate annual horsepower adjustment due to consumer performance demand. Consumer performance demand is adjusted
!...downward as HP/WGT ratio increases so that performance gains cannot continue indefinitely.  Initial demand
!...coefficients are controlled via user input parameter PERFFACT (VEHPERFFAC in TRNLDV.XML) and demand caps via user
!...input parameter PERFCAP (VEHPERFCAP in TRLDV.XML).

            IF (INC90$NP(11,YRS-1)                          .NE. 0.0 .AND. &
                PRICE(ICL,IGP,CURRENT,ILDV)             .NE. 0.0 .AND. &
                FE(ICL,IGP,PREV,ILDV)                   .NE. 0.0 .AND. &
                PMGTR90$(11,YRS)                            .NE. 0.0 .AND. &
                WEIGHT(ICL,IGP,BASE,ILDV)               .NE. 0.0 .AND. &
                WEIGHT(ICL,IGP,CURRENT,ILDV)            .NE. 0.0 .AND. &
                PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED .NE. 0.0) THEN

              PERF_ADJHP = (((INC90$NP(11,YRS)/INC90$NP(11,YRS-1)) ** 0.9) * &
                            ((PRICE(ICL,IGP,PREV,ILDV)/PRICE(ICL,IGP,CURRENT,ILDV)) ** 0.9) * &
                            ((FE(ICL,IGP,CURRENT,ILDV)/FE(ICL,IGP,PREV,ILDV)) ** 0.2) * &
                            ((PMGTR90$(11,YRS-1)/PMGTR90$(11,YRS)) ** 0.2)) - 1.0

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
              PERF_COEFF  = 1.0 - ((HP_WGT           - HP_WGT_BASE + DEMAND_USED) / &
                                   (PERFCAP(ICL,IGP) - HP_WGT_BASE + DEMAND_USED))

!...Print warning message if PERF_COEFF takes on a value above one or below zero. Since WEIGHT is already adjusted,
!...but horsepower is not, allow a small buffer above one in the early years of the projection and a small buffer below
!...zero in the latter years of the projection to avoid warnings due solely to "noise" type changes in the initial 
!...HP/WGT ratio estimate for each projection year.  However,do not maintain this buffer in the actual PERF_ADJHP scaling
!...algorithm.  Instead, reset ANY value above one or below zero.

              UPPER_BUFFER = 1.0 + (MAX(XYR+1 - YRS,0) * 0.01)
              LOWER_BUFFER = 0.0 - (MAX(YRS - XYR+11,0) * 0.01)
              
              IF (PERF_COEFF .GT. UPPER_BUFFER .OR. &
                  PERF_COEFF .LT. LOWER_BUFFER) THEN
!                WRITE (21,*)
!                WRITE (21,*) 'Consumer Performance Coefficient is'
!                WRITE (21,*) 'less than Zero or greater than One.'
!                WRITE (21,*)
!                WRITE (21,*) '                   Year = ',YRS
!                WRITE (21,*) '         NEMS Iteration = ',CURITR
!                WRITE (21,*) '               FEM Pass = ',PASS
!                WRITE (21,*) '          Vehicle Group = ',IGP
!                WRITE (21,*) '          Vehicle Class = ',ICL
!                WRITE (21,*) '              Fuel Type = ',ILDV
!                WRITE (21,*)
!                WRITE (21,*) 'Value before Adjustmemt = ',PERF_COEFF
!                WRITE (21,*)
!                WRITE (21,*) 'Coefficient has been Reset (to 0 or 1).'
!                WRITE (21,*)
              ENDIF

              PERF_COEFF  = MIN(1.0,PERF_COEFF)
              PERF_COEFF  = MAX(0.0,PERF_COEFF)
              PERF_ADJHP  = PERF_ADJHP * PERFFACT(ICL,IGP) * PERF_COEFF

!...If this is an ultra CAFE pass (i.e., PASS = 3), zero any consumer HP demand at this point, but wait until after
!...the remaining HP/WGT consistency checks are performed to restrict tech-driven HP.  Note, some consumer demand may
!...be readded below if the required minimum HP/WGT ratio is not maintained.  Since a minimum HP/WGT ratio is considered
!...essential for driveability, a minimum HP requirement will not be waived even under a third pass CAFE scenario.

              IF (PASS .EQ. 3) PERF_ADJHP = MIN(PERF_ADJHP,0.0)

            ELSE

              WRITE(6,*)
              WRITE(6,*) ' Divisor in FEMCALC PERF_ADJHP calc equals zero - RUN ABORTED.'
              WRITE(6,*)
              WRITE(6,*) ' --- At ABORT, index parameters were:'
              WRITE(6,*)
              WRITE(6,*) '   YRS   = ',YRS
              WRITE(6,*) '   ICL   = ',ICL
              WRITE(6,*) '   IGP   = ',IGP
              WRITE(6,*) '   ILDV  = ',ILDV
              WRITE(6,*)
              WRITE(6,*) ' --- the various denominators were:'
              WRITE(6,*)
              WRITE(6,*) '   INC90$NP(11,YRS-1)                       = ',INC90$NP(11,YRS-1)
              WRITE(6,*) '   PRICE(ICL,IGP,CURRENT,ILDV)              = ',PRICE(ICL,IGP,CURRENT,ILDV)
              WRITE(6,*) '   FE(ICL,IGP,PREV,ILDV)                    = ',FE(ICL,IGP,PREV,ILDV)
              WRITE(6,*) '   PMGTR90$(11,YRS)                         = ',PMGTR90$(11,YRS)
              WRITE(6,*) '   WEIGHT(ICL,IGP,BASE,ILDV)                = ',WEIGHT(ICL,IGP,BASE,ILDV)
              WRITE(6,*) '   WEIGHT(ICL,IGP,CURRENT,ILDV)             = ',WEIGHT(ICL,IGP,CURRENT,ILDV)
              WRITE(6,*) '   PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED = ',PERFCAP(ICL,IGP)-HP_WGT_BASE+DEMAND_USED
              WRITE(6,*)
              WRITE(6,*) ' --- and associated numerators were:'
              WRITE(6,*)
              WRITE(6,*) '   INC90$NP(11,YRS)                         = ',INC90$NP(11,YRS)
              WRITE(6,*) '   PRICE(ICL,IGP,PREV,ILDV)                 = ',PRICE(ICL,IGP,PREV,ILDV)
              WRITE(6,*) '   FE(ICL,IGP,CURRENT,ILDV)                 = ',FE(ICL,IGP,CURRENT,ILDV)
              WRITE(6,*) '   PMGTR90$(11,YRS-1)                       = ',PMGTR90$(11,YRS-1)
              WRITE(6,*) '   HP(ICL,IGP,BASE,ILDV)                    = ',HP(ICL,IGP,BASE,ILDV)
              WRITE(6,*) '   HP(ICL,IGP,CURRENT,ILDV)                 = ',HP(ICL,IGP,CURRENT,ILDV)
              WRITE(6,*) '   HP_WGT-HP_WGT_BASE+DEMAND_USED           = ',HP(ICL,IGP,CURRENT,ILDV)/WEIGHT(ICL,IGP,CURRENT,ILDV)-&
                                                                          HP_WGT_BASE+DEMAND_USED
              STOP 509

            ENDIF

!...Calculate the total horsepower adjustment for this year (i.e., technology-driven plus consumer demand-driven adjustments).

            TTL_ADJHP = TECH_ADJHP + PERF_ADJHP

!...Limit total horsepower adjustment in any given year to 10%.  Take back consumer demand first since that fuel economy
!...effect is not yet considered.  Take any additionally needed horsepower demand back from the technology side, and track
!...this "giveback" since it must be converted back into its fuel economy equivalent.

            IF (TTL_ADJHP .GT. 0.1) THEN

!              WRITE (21,*)
!              WRITE (21,*) 'Total HP adjustment constrained to 10%'
!              WRITE (21,*)
!              WRITE (21,*) '                   Year = ',YRS
!              WRITE (21,*) '         NEMS Iteration = ',CURITR
!              WRITE (21,*) '               FEM Pass = ',PASS
!              WRITE (21,*) '          Vehicle Group = ',IGP
!              WRITE (21,*) '          Vehicle Class = ',ICL
!              WRITE (21,*) '              Fuel Type = ',ILDV
!              WRITE (21,*)
!              WRITE (21,*) 'Value before constraint = ',TTL_ADJHP
!              WRITE (21,*) '        Tech adjustment = ',TECH_ADJHP
!              WRITE (21,*) '        Perf adjustment = ',PERF_ADJHP

              HP_GIVEBACK = TTL_ADJHP - 0.1
              IF (PERF_ADJHP .GT. 0.0) THEN
                  PERF_ADJHP = PERF_ADJHP - HP_GIVEBACK
                IF (PERF_ADJHP .GE. 0.0) THEN
                  HP_GIVEBACK = 0.0
                ELSE
                  HP_GIVEBACK = 0.0 - PERF_ADJHP
                  PERF_ADJHP  = 0.0
                ENDIF
              ENDIF
              TECH_ADJHP = TECH_ADJHP - HP_GIVEBACK
              TTL_ADJHP  = TECH_ADJHP + PERF_ADJHP

!              WRITE (21,*)
!              WRITE (21,*) 'Value after constraint  = ',TTL_ADJHP
!              WRITE (21,*) '       Tech adjustment  = ',TECH_ADJHP
!              WRITE (21,*) '       Tech giveback    = ',HP_GIVEBACK
!              WRITE (21,*) '       Perf adjustment  = ',PERF_ADJHP

            ENDIF

!...Also impose a maximum limit on HP/WGT ratio so that performance characteristics do not become unreasonable.  
!...Take back any consumer demand first since that fuel economy effect is not yet considered.  However, at this
!...point it is likely that any consumer demand is small already due to the performance demand constraints
!...imposed above on HP/WGT ratio increases.  Take any additional required horsepower demand back from the technology
!...side, and track this "giveback" since it must be converted back into its fuel economy equivalent.

            IF (WEIGHT(ICL,IGP,CURRENT,ILDV) .NE. 0.0) THEN

              TEMP_HP = HP(ICL,IGP,CURRENT,ILDV) * (1.0 + TTL_ADJHP)
              HP_WGT  = TEMP_HP / WEIGHT(ICL,IGP,CURRENT,ILDV)

              IF (HP_WGT .GT. PERFCAP(ICL,IGP)) THEN

!                WRITE (21,*)
!                WRITE (21,*) 'Total HP adjustment exceeds HP/WGT max'
!                WRITE (21,*)
!                WRITE (21,*) '                   Year = ',YRS
!                WRITE (21,*) '         NEMS Iteration = ',CURITR
!                WRITE (21,*) '               FEM Pass = ',PASS
!                WRITE (21,*) '          Vehicle Group = ',IGP
!                WRITE (21,*) '          Vehicle Class = ',ICL
!                WRITE (21,*) '              Fuel Type = ',ILDV
!                WRITE (21,*)
!                WRITE (21,*) 'Value before constraint = ',TTL_ADJHP
!                WRITE (21,*) '        Tech adjustment = ',TECH_ADJHP
!                WRITE (21,*) '        Perf adjustment = ',PERF_ADJHP
!                WRITE (21,*) '             HP/WGT Cap = ',PERFCAP(ICL,IGP)
!                WRITE (21,*) '       Tentative HP/WGT = ',HP_WGT

                EXCESS_ADJHP = TTL_ADJHP
                if(hp_wgt.ne.0.0) TTL_ADJHP = ((1.0 + TTL_ADJHP)*(PERFCAP(ICL,IGP)/HP_WGT)) - 1.0
                EXCESS_ADJHP = EXCESS_ADJHP - TTL_ADJHP
                IF (PERF_ADJHP .GT. 0.0) THEN
                    PERF_ADJHP = PERF_ADJHP - EXCESS_ADJHP
                  IF (PERF_ADJHP .GE. 0.0) THEN
                    EXCESS_ADJHP = 0.0
                  ELSE
                    EXCESS_ADJHP = 0.0 - PERF_ADJHP
                    PERF_ADJHP   = 0.0
                  ENDIF
                ENDIF
                IF (EXCESS_ADJHP .GT. TECH_ADJHP) EXCESS_ADJHP = TECH_ADJHP
                TECH_ADJHP   = TECH_ADJHP - EXCESS_ADJHP
                TTL_ADJHP    = TECH_ADJHP + PERF_ADJHP
                HP_GIVEBACK  = HP_GIVEBACK + EXCESS_ADJHP

                TEMP_HP = HP(ICL,IGP,CURRENT,ILDV) * (1.0 + TTL_ADJHP)
                HP_WGT  = TEMP_HP / WEIGHT(ICL,IGP,CURRENT,ILDV)

!                WRITE (21,*)
!                WRITE (21,*) ' Value after constraint = ',TTL_ADJHP
!                WRITE (21,*) '        Tech adjustment = ',TECH_ADJHP
!                WRITE (21,*) '          Tech giveback = ',HP_GIVEBACK
!                WRITE (21,*) '        Perf adjustment = ',PERF_ADJHP
!                WRITE (21,*) '           Final HP/WGT = ',HP_WGT

!                IF (HP_WGT .GT. (PERFCAP(ICL,IGP)+ROUNDOFF_ERROR)) THEN
!                  WRITE (21,*)
!                  WRITE (21,*) ' Constraint Limited by Available Tech Adjustment'
!                ENDIF

              ENDIF

            ELSE

              WRITE(6,*)
              WRITE(6,*) ' Divisor in FEMCALC EXCESS_ADJHP calc'
              WRITE(6,*) ' equals zero - RUN ABORTED.'
              WRITE(6,*)
              STOP

            ENDIF

!...Finally, make sure HP/WGT ratio stays above that required for driveability (95% of base year value or 0.04
!...for two seaters, 0.033 otherwise; whichever is lower). In this case, add additional required demand to consumer
!...performance demand side since all "standardly" available technology performance impacts will already be considered
!...on the tech side.  Additional demand need not be specially tracked since it is reflected in PERF_ADJHP, which
!...is automatically converted into fuel economy equivalent impacts in the algorithms that follow.

            IF (WEIGHT(ICL,IGP,CURRENT,ILDV) .NE. 0.0 .AND. &
                WEIGHT(ICL,IGP,BASE,ILDV)    .NE. 0.0 .AND. &
                HP(ICL,IGP,CURRENT,ILDV)     .NE. 0.0) THEN
 
!              HP_WGT_MIN = 0.95 * (HP(ICL,IGP,BASE,ILDV)/WEIGHT(ICL,IGP,BASE,ILDV))
              HP_WGT_MIN = 0.9 * (HP(ICL,IGP,BASE,ILDV)/WEIGHT(ICL,IGP,BASE,ILDV))
              IF (ICL .EQ. 6 .AND. IGP .LE. 4) THEN
                HP_WGT_MIN = MIN(HP_WGT_MIN,1.0/25.0)
              ELSE
                HP_WGT_MIN = MIN(HP_WGT_MIN,1.0/30.0)
              ENDIF

              TEMP_HP = HP(ICL,IGP,CURRENT,ILDV) * (1.0 + TTL_ADJHP)
              HP_WGT = TEMP_HP / WEIGHT(ICL,IGP,CURRENT,ILDV)
              MIN_ADJHP = ((HP_WGT_MIN * WEIGHT(ICL,IGP,CURRENT,ILDV)) / &
                            HP(ICL,IGP,CURRENT,ILDV)) - 1.0
              MIN_ADJ = .FALSE.

            ELSE

              WRITE(6,*)
              WRITE(6,*) ' Divisor in FEMCALC HP/WGT min calc'
              WRITE(6,*) ' equals zero - RUN ABORTED.'
              WRITE(6,*)
              STOP

            ENDIF

            IF (HP_WGT .LT. HP_WGT_MIN) THEN

              MIN_ADJ = .TRUE.

!              WRITE (21,*)
!              WRITE (21,*) 'Total HP adjustment below HP/WGT min'
!              WRITE (21,*)
!              WRITE (21,*) '                   Year = ',YRS
!              WRITE (21,*) '         NEMS Iteration = ',CURITR
!              WRITE (21,*) '               FEM Pass = ',PASS
!              WRITE (21,*) '          Vehicle Group = ',IGP
!              WRITE (21,*) '          Vehicle Class = ',ICL
!              WRITE (21,*) '              Fuel Type = ',ILDV
!              WRITE (21,*) '       Tentative HP/WGT = ',HP_WGT
!              WRITE (21,*) '         Minimum HP/WGT = ',HP_WGT_MIN
!              WRITE (21,*)
!              WRITE (21,*) 'Value before constraint = ',TTL_ADJHP
!              WRITE (21,*) '        Tech adjustment = ',TECH_ADJHP
!              WRITE (21,*) '        Perf adjustment = ',PERF_ADJHP

!...Calculate the horsepower demand required to maintain a minimum HP/WGT ratio.

              NEED_ADJHP  = MIN_ADJHP - TTL_ADJHP
              PERF_ADJHP  = PERF_ADJHP + NEED_ADJHP
              TTL_ADJHP   = TECH_ADJHP + PERF_ADJHP

!              WRITE (21,*)
!              WRITE (21,*) 'Value after constraint  = ',TTL_ADJHP
!              WRITE (21,*) '       Tech adjustment  = ',TECH_ADJHP
!              WRITE (21,*) '       Perf adjustment  = ',PERF_ADJHP

            ENDIF

!...Finally, if this is a third (i.e., ultra CAFE) pass, take back all the tech driven HP demand except that required
!...to maintain the HP/WGT minimum.

          if(ILDV.lt.4.and.ILDV.ge.9.and.ILDV.le.12)then
            if(IGP.ne.4) then ! sports cars
              if(PASS.eq.3) then
                EXCESS_ADJHP = TTL_ADJHP - MAX(MIN_ADJHP,0.0)
                EXCESS_ADJHP = MAX(EXCESS_ADJHP,0.0)
!...            This new algorithm takes back only a user specified increment in each step.
                Excess_AdjHP = Excess_AdjHP*GBInc
                if(TECH_ADJHP.gt.Roundoff_Error.and.(MIN_ADJ.and.EXCESS_ADJHP.gt.TECH_ADJHP)) then
                  WRITE(6,*)
                  WRITE(6,*) ' Error in Pass 3 HP Adjustment Logic'
                  WRITE(6,*)
                  WRITE(6,*) '             Year = ',YRS
                  WRITE(6,*) '   NEMS Iteration = ',CURITR
                  WRITE(6,*) '         FEM Pass = ',PASS
                  WRITE(6,*) '    Vehicle Group = ',IGP
                  WRITE(6,*) '    Vehicle Class = ',ICL
                  WRITE(6,*) '        Fuel Type = ',ILDV
                  WRITE(6,*) '        Ttl_Adjhp = ',TTL_ADJHP
                  WRITE(6,*) '       Tech_Adjhp = ',TECH_ADJHP
                  WRITE(6,*) '       Perf_Adjhp = ',PERF_ADJHP
                  WRITE(6,*) '        Min_Adjhp = ',MIN_ADJHP
                  WRITE(6,*) '     Excess_Adjhp = ',EXCESS_ADJHP
                  WRITE(6,*)
                  WRITE(6,*) '***** RUN ABORTED *****'
                  WRITE(6,*)
                  WRITE(6,*) ' Correct Logic and Rerun.'
                  WRITE(6,*)
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
          
!...Finally, check to make sure horsepower giveback does not exceed maximum available tech-driven HP increase.  
!...If so, something's wrong in the HP adjustment logic.  Now there's a stretch!
            if(IGP.ne.4) then ! sports cars
              if((TECH_MAX_ADJHP.gt.0.0.and.HP_GIVEBACK.gt.(TECH_MAX_ADJHP+ROUNDOFF_ERROR)).or. &
                 (TECH_MAX_ADJHP.le.0.0.and.ABS(HP_GIVEBACK).gt.roundoff_error)) then
                WRITE(6,*)
                WRITE(6,*) ' Error in HP Adjustment Logic'
                WRITE(6,*) ' HP Giveback Exceeds Tech Max'
                WRITE(6,*)
                WRITE(6,*) '             Year = ',YRS
                WRITE(6,*) '   NEMS Iteration = ',CURITR
                WRITE(6,*) '         FEM Pass = ',PASS
                WRITE(6,*) '    Vehicle Group = ',IGP
                WRITE(6,*) '    Vehicle Class = ',ICL
                WRITE(6,*) '        Fuel Type = ',ILDV
                WRITE(6,*) '        Ttl_Adjhp = ',TTL_ADJHP
                WRITE(6,*) '       Tech_Adjhp = ',TECH_ADJHP
                WRITE(6,*) '       Perf_Adjhp = ',PERF_ADJHP
                WRITE(6,*) '   Tech_Max_Adjhp = ',TECH_MAX_ADJHP
                WRITE(6,*) '      HP_Giveback = ',HP_GIVEBACK
                WRITE(6,*)
                WRITE(6,*) '***** RUN ABORTED *****'
                WRITE(6,*)
                WRITE(6,*) ' Correct Logic and Rerun.'
                WRITE(6,*)
                STOP
              endif
            endif
          endif

!...Now ready to adjust fuel economy up or down in accordance with the sum of consumer driven horsepower adjustment
!...and any horsepower giveback.  Horsepower giveback is HP demand already considered in FE estimates, so FE estimates
!...need to be adjusted upward for any giveback.  Tech driven affects are already accounted for in the tech incremental
!...fuel economy values.  Note that the consumer and giveback estimates are aggregated into the consumer parameter 
!...PERF_ADJHP to facilitate the series of ensuing FE and PRICE algortihms, recognizing of course that giveback is 
!...negative demand.


            PERF_ADJHP = PERF_ADJHP - HP_GIVEBACK
            SIGN = 1
            IF (PERF_ADJHP .LT. 0.0) SIGN = -1
            ADJFE = (-0.220 * PERF_ADJHP) - (+0.560 * SIGN * PERF_ADJHP * PERF_ADJHP)

            HP(ICL,IGP,CURRENT,ILDV) = HP(ICL,IGP,CURRENT,ILDV)*(1+TTL_ADJHP)
            FE(ICL,IGP,CURRENT,ILDV) = FE(ICL,IGP,CURRENT,ILDV)*(1+ADJFE)

            PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) + &
                                           PERF_ADJHP * VALUEPERF(ICL,IGP)

            PRICEHI(ICL,IGP,CURRENT,ILDV) = PRICEHI(ICL,IGP,CURRENT,ILDV) + &
                                             PERF_ADJHP * VALUEPERF(ICL,IGP)

! ************************************ TESTING ONLY ******************************
!   IF (CURITR.EQ.1.AND.N.GE.16.AND.ILDV.EQ.1.AND.ICL.EQ.3.AND.IGP.LE.2.AND.PASS.GE.2)&
!      WRITE(6,*) 'FE2-HP2',FE(ICL,IGP,CURRENT,ILDV),HP(ICL,IGP,CURRENT,ILDV)
! ************************************ TESTING ONLY ******************************

          ENDDO   ! end vehicle class (ICL) loop
        ENDDO   ! end vehicle group (IGP) loop
      ENDDO   ! end vehicle fuel type (ILDV) loop

 5000 CALL FEMRANGE

! ... Assign FEM parms to report writer arrays

      DO ILDV=1,MAXLDV
        DO IGP=1,MAXGROUP
          DO ICL=1,MAXCLASS
            FEMMPG(IGP,ICL,YRS,ILDV)  = FE(ICL,IGP,CURRENT,ILDV)
            FEMWGT(IGP,ICL,YRS,ILDV)  = WEIGHT(ICL,IGP,CURRENT,ILDV)
            FEMPRI(IGP,ICL,YRS,ILDV)  = PRICE(ICL,IGP,CURRENT,ILDV)
            FEMPRIH(IGP,ICL,YRS,ILDV) = PRICEHI(ICL,IGP,CURRENT,ILDV)
            FEMHP(IGP,ICL,YRS,ILDV)   = HP(ICL,IGP,CURRENT,ILDV)
            FEMVOL(IGP,ICL,YRS,ILDV)  = VOLUME(ICL,IGP,CURRENT,ILDV)
            FEMTSZ(IGP,ICL,YRS,ILDV)  = TANKSIZE(ICL,IGP,CURRENT,ILDV)
            FEMRNG(IGP,ICL,YRS,ILDV)  = RANGE(ICL,IGP,CURRENT,ILDV)
            DO ITECH=1,NUMTECH
              FEMPEN(IGP,ICL,ITECH,YRS,ILDV) = MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      IF (PASS .EQ. 3) THEN  
        CALL CALIBNHTSA

! ... Copy calibrated data back into "current year" arrays for use with next evaluation year

        DO ILDV=1,MAXLDV
          DO IGP=1,MAXGROUP
            DO ICL=1,MAXCLASS
              FE(ICL,IGP,CURRENT,ILDV)     = FEMMPG(IGP,ICL,YRS,ILDV)
              WEIGHT(ICL,IGP,CURRENT,ILDV) = FEMWGT(IGP,ICL,YRS,ILDV)
              HP(ICL,IGP,CURRENT,ILDV)     = FEMHP(IGP,ICL,YRS,ILDV)
            ENDDO
          ENDDO
        ENDDO

! ... Rerun range calculation using calibrated fuel economy

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
! ... Set initial market share and market share maximum.
          TOT_MKT   = ACTUAL_MKT(ITECH)
          MAX_SHARE = MMAX(ITECH)
! ... Find maximum allowable tech chain penetration.
          DO I = 2,TECH_CNT(INOTE)
            MAX_SHARE = MAX(MAX_SHARE,MMAX(SUPERSEDES(I,INOTE)))
          ENDDO
! ... Find and adjust any EXCESS penetration downward.
          DO I = 2,TECH_CNT(INOTE)
            TOT_MKT = TOT_MKT + ACTUAL_MKT(SUPERSEDES(I,INOTE))
            IF (TOT_MKT .GT. MAX_SHARE) THEN
              ACTUAL_MKT(SUPERSEDES(I,INOTE)) = ACTUAL_MKT(SUPERSEDES(I,INOTE)) - &
                                               (TOT_MKT - MAX_SHARE)
              TOT_MKT = MAX_SHARE
! ... Must leave some margin for round-off error in less than zero check.
              IF (ACTUAL_MKT(SUPERSEDES(I,INOTE)) .LT. 0.0-ROUNDOFF_ERROR) THEN
                RETURN_STAT = (-100 * INOTE) -  I
                RETURN
! ... But go ahead and reset non-zero values due to round-off to zero.
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
! ... Subroutine CGSHARE combines domestic and import data for cars and light trucks (GASOLINE VEHICLES ONLY) 
! ... and calculates light vehicle size class shares and average horsepower and weight for cars and light 
! ... trucks                          
!
! ... For years through the last NHTSA data year, use sales data from NHTSA to aggregated.  For other years, 
! ... use sales fractions from the last NHTSA data year.
! ==========================================================================================================
    SUBROUTINE CGSHARE
    USE T_
    IMPLICIT NONE

      integer it
      real DiffLn,CarShr(mnumcr,MNUMYR),SumGrp(MAXGROUP),SumTyp(maxvtyp)
      real SaleTyp(mnumcr,maxvtyp,MAXCLASS),SaleTTyp(mnumcr,maxvtyp),SaleXTyp(mnumcr,maxvtyp,MAXCLASS)
	  real AHPTRUCK_regn(mnumcr,mnumyr), AHPCAR_regn(mnumcr,mnumyr), AWTTRUCK_regn(mnumcr,mnumyr), AWTCAR_regn(mnumcr,mnumyr)
      
! ... Calculate the shares of manufacturer groups and size class within each vehicle type (cars/trucks)
      do ICL=1,MAXCLASS
! ... Clear out the variables.
        do IGP=1,MAXGROUP
          PerGrp(IGP,ICL,n)=0.0
        end do
! ... Sum up the sales of manufacturer groups within each vehicle type (cars/trucks)
! ... Include only cases where FEM data has an MPG>0.
        if(yrs.le.NHTSALyr) then
          sumtyp(1)=0.0
          sumtyp(2)=0.0
          do IGP=1,MAXGROUP
            it=GrpMap(IGP)
            if(FemMpg(IGP,ICL,yrs,gas).ne.0.0) then
              SumTyp(it)=SumTyp(it)+NHTSASal(yrs,ICL,IGP)
            endif
          end do
! ... Then divide sales of each manufacturer group and size class by the total of vehicle type
          do IGP=1,MAXGROUP
            it=GrpMap(IGP)
            if(FemMpg(IGP,ICL,yrs,gas).ne.0.0) then
              if(SumTyp(it).ne.0.0) PerGrp(IGP,ICL,n)=NHTSASal(yrs,ICL,IGP)/SumTyp(it)
            endif
          end do
        else
! ... Shares for subsequent projection years are the same as the last historical year
          do IGP=1,MAXGROUP
            PerGrp(IGP,ICL,n)=PerGrp(IGP,ICL,n-1)
          end do
        endif
      end do
	  
! ... Go through each group. If the year is less than or equal to NHTSALyr, then use the historical data
! ... and calculate a class share. If the year is greater than NHTSALyr, then do an econometric projection
! ... of the class shares and normalize them to add to 1.0.

      do IGP=1,MAXGROUP
! ... Use the historical NHTSA data.
        if(yrs.le.nhtsalyr) then
          groupsum(IGP)=0.0
          do ICL=1,MAXCLASS
            class_share(mnumcr,ICL,IGP,yrs)=nhtsasal(yrs,ICL,IGP)
            groupsum(IGP)=groupsum(IGP)+class_share(mnumcr,ICL,IGP,yrs)
          end do
          do ICL=1,MAXCLASS
            if(groupsum(IGP).ne.0.0) class_share(mnumcr,ICL,IGP,yrs)=class_share(mnumcr,ICL,IGP,yrs)/groupsum(IGP)
			do iregn = 1,mnumcr-2
			    class_share(iregn,ICL,IGP,yrs) = class_share(mnumcr,ICL,IGP,yrs)  ! manuf_regn(iregn,ICL,IGP)   
			enddo
          end do
        else
		
! ... Use the econometric projection.
          groupsum(IGP)=0.0
		   ratio_byr=yrs-XYR		!dst should be updated
            do ICL=1,MAXCLASS
              if(pmgtr90$(11,yrs-1).ge.0.0.and.inc90$np(11,yrs-1).gt.13000.0.and.price(ICL,IGP,prev,gas).gt.0.0) then
							
                diffln=coef_a(ICL,IGP)*log(ratio_byr) + coef_b(ICL,IGP)*log(pmgtr90$(11,yrs)/pmgtr90$(11,yrs-1)) + &
                coef_c(ICL,IGP)*log((inc90$np(11,yrs)-13000.0)/(inc90$np(11,yrs-1)-13000.0)) + &
                coef_p(ICL,IGP)*log(price(ICL,IGP,current,gas)/price(ICL,IGP,prev,gas))

                ratio_ln=diffln + log(max(class_share(mnumcr,ICL,IGP,nhtsalyr),0.000001)/(1.0-max(class_share(mnumcr,ICL,IGP,nhtsalyr),0.000001)))				
                ratio=exp(ratio_ln)
                class_share(mnumcr,ICL,IGP,yrs)=ratio/(1.0+ratio)				
              endif				
              groupsum(IGP)=groupsum(IGP)+class_share(mnumcr,ICL,IGP,yrs)		

			  do iregn = 1,mnumcr-2	

                if(pmgtr90$(iregn,yrs-1).ge.0.0.and.inc90$np(iregn,yrs-1).gt.13000.0.and.price(ICL,IGP,prev,gas).gt.0.0) then
					
                    diffln = coef_a(ICL,IGP)*log(ratio_byr) + coef_b(ICL,IGP)*log(pmgtr90$(iregn,yrs)/pmgtr90$(iregn,yrs-1)) + &
                             coef_c(ICL,IGP)*log((inc90$np(iregn,yrs)-13000.0)/(inc90$np(iregn,yrs-1)-13000.0)) + &
                             coef_p(ICL,IGP)*log(price(ICL,IGP,current,gas)/price(ICL,IGP,prev,gas))

                    ratio_ln = diffln + log(max(class_share(iregn,ICL,IGP,nhtsalyr),0.000001)/(1.0-max(class_share(iregn,ICL,IGP,nhtsalyr),0.000001)))					
                    ratio = exp(ratio_ln)
                    class_share(iregn,ICL,IGP,yrs) = ratio/(1.0+ratio)
				endif
			  enddo
            enddo
! ... Normalize the shares.
          do ICL=1,MAXCLASS
            if(groupsum(IGP).ne.0.0) class_share(mnumcr,ICL,IGP,yrs)=class_share(mnumcr,ICL,IGP,yrs)/groupsum(IGP)
			do iregn = 1,mnumcr-2
			  if(sum(class_share(iregn,1:MAXCLASS,IGP,yrs)).gt.0.0) class_share(iregn,ICL,IGP,yrs) = class_share(iregn,ICL,IGP,yrs)/sum(class_share(iregn,1:MAXCLASS,IGP,yrs))
			enddo			
          end do
        endif
      end do
    
! ... Calculate the shares of manufacture groups within each vehicle type (cars/trucks) (summed over classes)
! ... Use the historical NHTSA data.
      if(yrs.le.NHTSALyr) then
! ... Sum up the NHTSA data.
        do IGP=1,MAXGROUP
          it=GrpMap(IGP)
          SumGrp(IGP)=0.0
          SumTyp(it)=0.0
        end do
        do ICL=1,MAXCLASS
          do IGP=1,MAXGROUP
            it=GrpMap(IGP)
            SumGrp(IGP)=SumGrp(IGP)+NHTSASal(yrs,ICL,IGP)
            SumTyp(it)=SumTyp(it)+NHTSASal(yrs,ICL,IGP)
          end do
        end do
! ... Calculate the shares from the NHTSA data summed.
        do IGP=1,MAXGROUP
          it=GrpMap(IGP)
		  SaleShr(mnumcr,IGP,n) = 0.0   
          if(SumTyp(it).ne.0.0) SaleShr(mnumcr,IGP,n)=SumGrp(IGP)/SumTyp(it)
		  do iregn = 1,mnumcr-2
		    SaleShr(iregn,IGP,n) = SaleShr_regn(iregn,IGP)
		  enddo
        end do
      else
        do IGP=1,MAXGROUP
		  do iregn = 1,mnumcr		
            SaleShr(iregn,IGP,n) = SaleShr(iregn,IGP,n-1)
		  enddo
        end do
      endif
	  
! ... Calculate personal (non-fleet) sales by group and class
	  do iregn = 1,mnumcr
        if(iregn.ne.10) CarShr(iregn,n)=NewCars(iregn,n)/(NewCars(iregn,n)+NewCls12A(iregn,n))	  
	  enddo
      do ICL=1,MAXCLASS
        do IGP=1,MAXGROUP
          it=GrpMap(IGP)
          if(it.eq.1) then
			do iregn = 1,mnumcr
              NVS7SC(iregn,IGP,ICL,n)=class_share(iregn,ICL,IGP,yrs)*(NewCars(iregn,n)*Owner_Share(iregn,1,1,n))*SaleShr(iregn,IGP,n)
              OClass_Share(iregn,ICL,IGP,yrs)=class_share(iregn,ICL,IGP,yrs)*CarShr(iregn,n)*SaleShr(iregn,IGP,n)
            enddo			
          elseif(it.eq.2) then
			do iregn = 1,mnumcr
              NVS7SC(iregn,IGP,ICL,n)=class_share(iregn,ICL,IGP,yrs)*(NewCls12A(iregn,n)*Owner_Share(iregn,2,1,n))*SaleShr(iregn,IGP,n)
              OClass_Share(iregn,ICL,IGP,yrs)=class_share(iregn,ICL,IGP,yrs)*(1.0-CarShr(iregn,n))*SaleShr(iregn,IGP,n)			
			enddo
          endif			  
        enddo
      enddo
	  
! ... Accumulate sales and shares for personal (non-fleet) vehicles. Clear the variables first.
      do it=1,maxvtyp
		do iregn = 1,mnumcr	  
          do ICL=1,MAXCLASS
            SaleTyp(iregn,it,ICL)=0.0
            SaleXTyp(iregn,it,ICL)=0.0		
          enddo			
	      SaleTTyp(iregn,it)=0.0
		enddo
      enddo
! ... Sum up the sales by group to the sales by type and put in temporary variable SaleTyp
      do ICL=1,MAXCLASS
        do IGP=1,MAXGROUP
          it=GrpMap(IGP)
		  do iregn = 1,mnumcr
            SaleTyp(iregn,it,ICL)=SaleTyp(iregn,it,ICL)+NVS7SC(iregn,IGP,ICL,n)
            SaleTTyp(iregn,it)=SaleTTyp(iregn,it)+NVS7SC(iregn,IGP,ICL,n)		  
		  enddo
		  do IVTYP=1,maxvtyp
		    SaleTyp(mnumcr,IVTYP,ICL)=sum(SaleTyp(1:mnumcr-2,IVTYP,ICL))
		  enddo
        enddo
      enddo
	  
! ... Use Sales by type to calculate class shares by type and put in temp variable SaleXTyp.
      do ICL=1,MAXCLASS
        do it=1,maxvtyp
		  do iregn = 1,mnumcr
            if(SaleTTyp(iregn,it).ne.0.0) SaleXTyp(iregn,it,ICL)=SaleTyp(iregn,it,ICL)/SaleTTyp(iregn,it)
		  enddo
        end do
      end do

! ... Put the temporary variables into the transportation model variables.
      do ICL=1,MAXCLASS   
		do iregn = 1,mnumcr-2
          passhrr(iregn,ICL,n) = SaleXTyp(iregn,1,ICL)
          ltshrr(iregn,ICL,n) = SaleXTyp(iregn,2,ICL)
          ncstsc(iregn,ICL,n) = SaleTyp(iregn,1,ICL)/sum(SaleTyp(1:mnumcr-2,1,1:MAXCLASS))*sum(SaleTyp(mnumcr,1,1:MAXCLASS))
          nltstsc(iregn,ICL,n) = SaleTyp(iregn,2,ICL)/sum(SaleTyp(1:mnumcr-2,2,1:MAXCLASS))*sum(SaleTyp(mnumcr,2,1:MAXCLASS))
		enddo			
        ncstsc(mnumcr,ICL,n) = sum(ncstsc(1:mnumcr-2,ICL,n))
        nltstsc(mnumcr,ICL,n) = sum(nltstsc(1:mnumcr-2,ICL,n))			
      enddo

      do ICL = 1,MAXCLASS	
        passhrr(mnumcr,ICL,n) = ncstsc(mnumcr,ICL,n)/sum(ncstsc(mnumcr,1:MAXCLASS,n))
        ltshrr(mnumcr,ICL,n) = nltstsc(mnumcr,ICL,n)/sum(nltstsc(mnumcr,1:MAXCLASS,n))	
	  enddo

! ... Use the results of the above calculations to weight up or to calculate a few things.      
! ... Weight up the various attributes from manufacturer groups to vehicle types (cars/trucks)
      do ICL=1,MAXCLASS
        do it=1,maxvtyp
          LDV_MPG_CL(it,gas,ICL,yrs)=0.0
          LDVHPW(it,gas,ICL,yrs)=0.0
          LDV_PRI(it,gas,ICL,yrs)=0.0
          LDV_RNG(it,gas,ICL,yrs)=0.0
          Wgt(it,gas,ICL,yrs)=0.0
        end do
        do IGP=1,MAXGROUP
          it=GrpMap(IGP)
          if(FemMpg(IGP,ICL,yrs,gas).ne.0.0) then
            LDV_MPG_CL(it,gas,ICL,yrs)=LDV_MPG_CL(it,gas,ICL,yrs)+PerGrp(IGP,ICL,n)/FemMpg(IGP,ICL,yrs,gas)
            LDVHPW(it,gas,ICL,yrs)=LDVHPW(it,gas,ICL,yrs)+PerGrp(IGP,ICL,n)*FemHP(IGP,ICL,yrs,gas)
            LDV_PRI(it,gas,ICL,yrs)=LDV_PRI(it,gas,ICL,yrs)+PerGrp(IGP,ICL,n)*FemPri(IGP,ICL,yrs,gas)
            LDV_RNG(it,gas,ICL,yrs)=LDV_RNG(it,gas,ICL,yrs)+PerGrp(IGP,ICL,n)*FemRng(IGP,ICL,yrs,gas)
            Wgt(it,gas,ICL,yrs)=Wgt(it,gas,ICL,yrs)+PerGrp(IGP,ICL,n)*FemWgt(IGP,ICL,yrs,gas)
          endif
        end do
        do it=1,maxvtyp
		  LDV_MPG_CL(it,gas,ICL,yrs)=1.0/LDV_MPG_CL(it,gas,ICL,yrs)
        end do
      end do
	  
! ... The following calculates the vehicle group-specific fuel economy. GASMPG_ACTUAL is
! ... used only for diagnostic output and is based on PROJECTED not calibrated fuel economy.
      DO IGP=1,MAXGROUP
       SUM_MKS    = 0.0
       SUM_MKS_FE = 0.0
       GASMPG_ACTUAL(IGP,YRS) = 0.0
       DO ICL=1,MAXCLASS
        IF (CLASSFLAG(ICL,IGP,GAS)) THEN
         SUM_MKS = SUM_MKS + CLASS_SHARE(mnumcr,ICL,IGP,YRS)
         IF (FE(ICL,IGP,CURRENT,GAS) .GT. 0.0) then
          SUM_MKS_FE = SUM_MKS_FE + CLASS_SHARE(mnumcr,ICL,IGP,YRS) / FE(ICL,IGP,CURRENT,GAS)
         endif
        ENDIF
       ENDDO
       IF (SUM_MKS_FE .GT. 0.0) GASMPG_ACTUAL(IGP,YRS) = SUM_MKS / SUM_MKS_FE
      ENDDO

! ... Calculate average horsepower and weight for new cars and light trucks
	  do iregn = 1,mnumcr
	      AHPCAR(iregn,N)   = 0.0
	      AHPTRUCK(iregn,N) = 0.0
	      AWTCAR(iregn,N)   = 0.0
	      AWTTRUCK(iregn,N) = 0.0
	  enddo
      DO ICL=1,MAXCLASS
        AHPCAR(11,N)   = AHPCAR(11,N)   + PASSHRR(11,ICL,N) * LDVHPW(1,GAS,ICL,YRS)
        AHPTRUCK(11,N) = AHPTRUCK(11,N) + LTSHRR(11,ICL,N)  * LDVHPW(2,GAS,ICL,YRS)
        AWTCAR(11,N)   = AWTCAR(11,N)   + PASSHRR(11,ICL,N) * WGT(1,GAS,ICL,YRS)
        AWTTRUCK(11,N) = AWTTRUCK(11,N) + LTSHRR(11,ICL,N)  * WGT(2,GAS,ICL,YRS)
		
		do iregn = 1,mnumcr-2
            AHPCAR(iregn,N)   = AHPCAR(iregn,N)   + PASSHRR(iregn,ICL,N) * LDVHPW(1,GAS,ICL,YRS)
            AHPTRUCK(iregn,N) = AHPTRUCK(iregn,N) + LTSHRR(iregn,ICL,N)  * LDVHPW(2,GAS,ICL,YRS)
            AWTCAR(iregn,N)   = AWTCAR(iregn,N)   + PASSHRR(iregn,ICL,N) * WGT(1,GAS,ICL,YRS)
            AWTTRUCK(iregn,N) = AWTTRUCK(iregn,N) + LTSHRR(iregn,ICL,N)  * WGT(2,GAS,ICL,YRS)		
!		  endif
		enddo		
      ENDDO
	    
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


!...calculate regional driving demand (household and fleet combined) VMTLDV,
!...assuming all in gasoline for the purpose of this calculation, which is only
!...done here to calculate regional travel shares (RSHR).
!...correct fuel shares for VMTLDV are calculated in subroutine TVMT

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

    do is=1,MAXCLASS
      do ir=1,mnumcr-2
!...    prior to first regional stock year, regionalization is done with license drivers
	    if(curcalyr.le.2011) then
          NCS(ir,is,n) = NCSTSC(mnumcr,is,n)*CarShare(ir,n)
          NLTS(ir,is,n)= NLTSTSC(mnumcr,is,n)*TrkShare(ir,n)
		else 
          NCS(ir,is,n) = NCSTSC(ir,is,n)
          NLTS(ir,is,n)= NLTSTSC(ir,is,n)
		endif
      enddo	 
    enddo
	  
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
! ... HMSLoop controls the looping through the hydrogen market segments.
      DO HMSLoop=1,3
        CALL TALT2X
      END DO
      CALL TFLTSTKS
      CALL TLEGIS

      PassNo=2
      CALL TATTRIB
      CALL TALT2
      DO HMSLoop=1,3
        CALL TALT2X
      END DO
      CALL TFLTSTKS
      CALL TLEGIS

    RETURN
    END SUBROUTINE TLDV

! ==========================================================================================================                                                   
! ... Subroutine TATTRIB adjusts the LDV attributes so they can be used throughout the model (MPG, price, 
! ... range, and horsepower)
! ...
! ... ATVs reflect differing price structures depending on whether they are in low or high volume production 
! ... (or somewhere in between).  As production moves from low to high volume, prices will decline.  The 
! ... following algorithm is used to estimate intermediate prices:
! ...
! ...                  Delta Price = (a)(ln Sales Volume) + b
! ...
! ... Algorithm parameters "a" and "b" are derived using FEM BASE year price differentials for low volume 
! ... (FEMPRI) and high volume (FEMPRIH) ATV sales.  Although these same differentials are maintained over 
! ... time, BASE year data must be used to estimate intermediate production volume prices because FEMPRI is 
! ... updated each year to reflect actual sales penetrations.  In effect, FEMPRI is not the low volume price,
! ... but the ACTUAL volume price so that only the base year price differential indicates the true low/high 
! ... volume price differential.
! ...
! ... The difference between the CURRENT and BASE year high volume price estimate (FEMPRIH) can be used to 
! ... update the actual production volume price estimated using base year price differentials for post-base 
! ... year technology introduction.  FEMPRIH is "pristine" in that it is adjusted only for technology 
! ... differences each year and is adjusted in a fashion identical to the actual volume price parameter FEMPRI.  
! ... Therefore, the difference between the CURRENT year estimate for FEMPRIH and the BASE year estimate for 
! ... FEMPRIH can be added to the actual production volume price estimate to account for the impacts of 
! ... technology adopted between the base and current years.
! ...
! ... Finally, BASE year FEMPRI and FEMPRIH reflect 2,500 and 25,000 sales volumes respectively, but they are 
! ... "single model" sales volumes and it is assumed that at least two models are required before sales volume 
! ... "scaling" will accrue. Therefore, implemented algorithm parameters are based on sales volumes of 5,000 
! ... for FEMPRI and 50,000 for FEMPRIH.
! ...
! ... NOTE - the LDV attributes for gasoline are calculated in the subroutine AGGDOMIMP     
! ========================================================================================================== 
  SUBROUTINE TATTRIB
  USE T_
  IMPLICIT NONE

    INCLUDE 'ANGTDM'
    INCLUDE 'AEUSPRC'

    INTEGER       ZYR 
    REAL          ATVSALES,ESTPRICE
    REAL          SLOPE,INTERCEPT,IRA_Credit(MAXLDV)
    integer it

!...For the first pass through the nested multinomial logit model, use the ATV "actual volume" price estimate 
!...(FEMPRI) without change.  This estimate incorporates all volume-based price adjustments through the PREVIOUS
!...year as well as all technology introductions through the CURRENT year.  On the second pass, FEMPRI is adjusted
!...to consider updated penetration estimates.

      IF (PASSNO .EQ. 2 .AND. N .GE. ((XYR+1)-BYR)+1) THEN

      do ICL=1,MAXCLASS
        do ILDV=2,maxldv
!...Align the two ATV indices used for the light duty vehicle portion of TRAN
          do IGP=1,MAXGROUP
            IVTYP=GrpMap(IGP)
            if(.NOT. CLASSFLAG(ICL,IGP,ILDV)) CYCLE
            if(IVTYP.eq.1) ZYR = CARFLG(ILDV-1,ICL)
            if(IVTYP.eq.2) ZYR = TRKFLG(ILDV-1,ICL)
            if(ZYR.lt.XYR) ZYR = XYR
!...Calculate "regression" parameters
            SLOPE = (FEMPRI(IGP,ICL,zyr,ILDV) - FEMPRIH(IGP,ICL,zyr,ILDV)) / &
                        (ALOG(5000.0)-ALOG(50000.0))
            INTERCEPT = FEMPRI(IGP,ICL,zyr,ILDV) - (SLOPE*(ALOG(5000.0)))
!...Estimate ATV production volume price point using BASE year price differentials, constrained at both ends by 
!...high and low production volume prices (i.e., price can never drop below high production volume price or rise 
!...above low volume production price).
            if(IVTYP.eq.1) then     !car
              ATVSALES = NCSTECH(mnumcr,ICL,ILDV,n)*1000000.0
            else                    !light truck
              ATVSALES = NLTECH(mnumcr,ICL,ILDV,n)*1000000.0
            endif

            ESTPRICE = FEMPRI(IGP,ICL,zyr,ILDV)
            if(ATVSALES.gt.0.0) ESTPRICE = INTERCEPT + (SLOPE * ALOG(ATVSALES))
            ESTPRICE = MIN(ESTPRICE,FEMPRI(IGP,ICL,zyr,ILDV))
            ESTPRICE = MAX(ESTPRICE,FEMPRIH(IGP,ICL,zyr,ILDV))
! ... Add in the cost of technology adopted between the BASE and CURRENT years.
            FEMPRI(IGP,ICL,yrs,ILDV) = ESTPRICE + (FEMPRIH(IGP,ICL,yrs,ILDV)-FEMPRIH(IGP,ICL,zyr,ILDV))
          enddo
        enddo
      enddo
    endif

!...Use the shares to weight up the various attributes from manufacturer groups to vehicle types (cars/trucks)
!...Assume the same domestic versus import sales shares as gasoline to provide for an equitable comparison of 
!...attributes across vehicle types.
    do ILDV=2,MAXLDV  !Gasoline was done already in aggdomimp
      do ICL=1,MAXCLASS
        do it=1,maxvtyp
          LDV_MPG_CL(it,ILDV,ICL,yrs)=0.0
          LDVHPW(it,ILDV,ICL,yrs)=0.0
          LDV_PRI(it,ILDV,ICL,yrs)=0.0
          LDV_RNG(it,ILDV,ICL,yrs)=0.0
          Wgt(it,ILDV,ICL,yrs)=0.0
        enddo
      enddo
    enddo
    do ILDV=2,MAXLDV  !Gasoline was done already in aggdomimp
      do ICL=1,MAXCLASS
        do IGP=1,MAXGROUP
          it=GrpMap(IGP)
          if(FemMpg(IGP,ICL,yrs,ILDV).ne.0.0) then
			LDV_MPG_CL(it,ILDV,ICL,yrs)=LDV_MPG_CL(it,ILDV,ICL,yrs)+PerGrp(IGP,ICL,n)/FemMpg(IGP,ICL,yrs,ILDV)
          endif
        enddo
        if(LDV_MPG_CL(1,ILDV,ICL,yrs).ne.0.0) LDV_MPG_CL(1,ILDV,ICL,yrs)=1.0/LDV_MPG_CL(1,ILDV,ICL,yrs)
        if(LDV_MPG_CL(2,ILDV,ICL,yrs).ne.0.0) LDV_MPG_CL(2,ILDV,ICL,yrs)=1.0/LDV_MPG_CL(2,ILDV,ICL,yrs)
        do IGP=1,MAXGROUP
          it=GrpMap(IGP)
          LDVHPW(it,ILDV,ICL,yrs)=LDVHPW(it,ILDV,ICL,yrs)+PerGrp(IGP,ICL,n)*FemHP(IGP,ICL,yrs,ILDV)
          LDV_PRI(it,ILDV,ICL,yrs)=LDV_PRI(it,ILDV,ICL,yrs)+PerGrp(IGP,ICL,n)*FemPri(IGP,ICL,yrs,ILDV)
          LDV_RNG(it,ILDV,ICL,yrs)=LDV_RNG(it,ILDV,ICL,yrs)+PerGrp(IGP,ICL,n)*FemRng(IGP,ICL,yrs,ILDV)
          Wgt(it,ILDV,ICL,yrs)=Wgt(it,ILDV,ICL,yrs)+PerGrp(IGP,ICL,n)*FemWgt(IGP,ICL,yrs,ILDV)
        enddo
      enddo
    enddo

! MDR commented out for AEO2023 (EV200s dropping below gas ICE)
!...Constrain AFV prices so that they cannot drop below gasoline vehicle price plus the high volume price 		
!...differential between gasoline and AFV. 
!    do ICL=1,MAXCLASS
!      do ILDV=2,MAXLDV
!        do IVTYP=1,maxvtyp
!          if(LDV_PRI(IVTYP,ILDV,ICL,yrs).ne.0.0.and.LDV_PRI(IVTYP,ILDV,ICL,yrs).lt.LDV_PRI(IVTYP,gas,ICL,yrs)) then
!            do IGP=MAXGROUP,1,-1
!              it=GrpMap(IGP)
!              if(it.eq.IVTYP.and.CLASSFLAG(ICL,IGP,ILDV)) then
!!...set zyr to be the introduction year, so the the delta added to LDV_PRI is the difference between the atv and gas vehicle prices
!                if(IVTYP.eq.1) ZYR = CARFLG(ILDV-1,ICL)
!                if(IVTYP.eq.2) ZYR = TRKFLG(ILDV-1,ICL)
!                if(ZYR.lt.XYR) ZYR = XYR
!                LDV_PRI(IVTYP,ILDV,ICL,yrs)=LDV_PRI(IVTYP,gas,ICL,yrs)+(FemPriH(IGP,ICL,zyr,ILDV)-FemPriH(IGP,ICL,zyr,gas))
!             endif
!            enddo
!          endif
!        enddo
!      end do
!    end do

!...Purchase price (1990 $).  Assign vehicle price estimate to the price parameter used in the logit model.
    do ILDV=1,maxldv
      do ICL=1,MAXCLASS
        PSPR(1,ILDV,ICL,yrs) = LDV_PRI(1,ILDV,ICL,yrs)
        PSPR(2,ILDV,ICL,yrs) = LDV_PRI(2,ILDV,ICL,yrs)
      enddo
    enddo

!...fuel cell vehicles
    DO IVTYP=1,MAXVTYP
      DO ICL=1,MAXCLASS
        IF (PSPR(IVTYP,14,ICL,YRS) .NE. 0.0) THEN
          PSPR(IVTYP,14,ICL,YRS) = PSPR(IVTYP,14,ICL,YRS) - (4000.0 * (MC_JPGDP(1)/MC_JPGDP(N)))
          IF (PSPR(IVTYP,14,ICL,YRS) .LE. 0.0) STOP 445
        ENDIF
      ENDDO
    ENDDO

!...Tax credits for plug-in hybrid and electric vehicles 

!...Calculate average battery capacity and plug-in vehicle credit re-arranged to support Li-ion GWh accounting
      do ILDV=1,maxldv
        if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.eq.15)then
   		  do ICL=1,MAXCLASS
            avg_kwh(1,ILDV,ICL,yrs)=0.0
            avg_kwh(2,ILDV,ICL,yrs)=0.0
            avg_phev_credit(1,ILDV,ICL,yrs)=0.0
            avg_phev_credit(2,ILDV,ICL,yrs)=0.0
            do IGP=1,MAXGROUP
              it=GrpMap(IGP)
			  avg_kwh(it,ILDV,ICL,yrs) = avg_kwh(it,ILDV,ICL,yrs)+PerGrp(IGP,ICL,n)*BatPackSize(yrs,ICL,IGP,ILDV)
              avg_phev_credit(it,ILDV,ICL,yrs) = avg_phev_credit(it,ILDV,ICL,yrs)+PerGrp(IGP,ICL,n)*phev_credit            
            enddo
          enddo
		endif
	  enddo
	  
!...Calculate Stimulus PHEV and EV tax credit
    if(phevstim.eq.1.0.and.yrs.ge.2009.and.yrs.le.2022)then
      do ILDV=1,maxldv
        if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.eq.15)then
!...Calculate battery and plug-in vehicle credit
          do ICL=1,MAXCLASS
            avg_kwh(1,ILDV,ICL,yrs)=0.0
            avg_kwh(2,ILDV,ICL,yrs)=0.0
            avg_phev_credit(1,ILDV,ICL,yrs)=0.0
            avg_phev_credit(2,ILDV,ICL,yrs)=0.0
            do IGP=1,MAXGROUP
              it=GrpMap(IGP)
			  avg_kwh(it,ILDV,ICL,yrs) = avg_kwh(it,ILDV,ICL,yrs)+PerGrp(IGP,ICL,n)*BatPackSize(yrs,ICL,IGP,ILDV)
              avg_phev_credit(it,ILDV,ICL,yrs) = avg_phev_credit(it,ILDV,ICL,yrs)+PerGrp(IGP,ICL,n)*phev_credit            
            enddo
          enddo 
!...Limit credit to max and apply to vehicle price     
          do IVTYP=1,maxvtyp
            do ICL=1,MAXCLASS
              if(LDV_MPG_CL(IVTYP,ILDV,ICL,yrs).ne.0.0) then
                if(avg_kwh(IVTYP,ILDV,ICL,yrs).ge.5.0) then
                  avg_bat_credit(IVTYP,ILDV,ICL,yrs) = (avg_kwh(IVTYP,ILDV,ICL,yrs)-4.0)*kwh_credit
                else
                  avg_bat_credit(IVTYP,ILDV,ICL,yrs) = 0.0
                endif
                Ttl_credit(IVTYP,ILDV,ICL,yrs) = avg_bat_credit(IVTYP,ILDV,ICL,yrs)+avg_phev_credit(IVTYP,ILDV,ICL,yrs)
                if(Ttl_credit(IVTYP,ILDV,ICL,yrs).gt.max_credit) Ttl_credit(IVTYP,ILDV,ICL,yrs) = max_credit
                PSPR(IVTYP,ILDV,ICL,yrs)=PSPR(IVTYP,ILDV,ICL,yrs)-Ttl_credit(IVTYP,ILDV,ICL,yrs)               
              endif
            enddo
          enddo
        endif 
      enddo
    endif
 
!...Calculate IRA PHEV and EV tax credit
    IRA_CREDIT = 0.0
    if(ira_stim.eq.1.0.and.yrs.ge.2023) then
      do ILDV=1,maxldv
        if(ILDV.eq.4.or.ILDV.le.7.or.ILDV.eq.15) then	
		  IRA_Credit(ildv) = (ira_veh_cred * ira_veh_shr(1,yrs,LEGIRA)) + (ira_bat_cred * ira_bat_shr(1,yrs,LEGIRA))
		endif
        if(ILDV.eq.5.or.ILDV.eq.6) then		  
		  IRA_Credit(ildv) = (ira_veh_cred * ira_veh_shr(2,yrs,LEGIRA)) + (ira_bat_cred * ira_bat_shr(2,yrs,LEGIRA))
		endif
	  enddo
      do ILDV=1,maxldv
        if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.eq.15) then
          do IVTYP=1,maxvtyp
            do ICL=1,MAXCLASS
              if(LDV_MPG_CL(IVTYP,ILDV,ICL,yrs).ne.0.0) then
                PSPR(IVTYP,ILDV,ICL,yrs) = PSPR(IVTYP,ILDV,ICL,yrs) - ira_credit(ildv)               
              endif
            enddo
          enddo
        endif 
      enddo
    endif
 
!...Zero fuel cell vehicle prices for years <= 2004 (mainly for report writing purposes)
    IF (YRS .LE. 2004) THEN
      DO ILDV=13,14 
        DO ICL=1,MAXCLASS
          LDV_PRI(1,ILDV,ICL,YRS) = 0.0
          LDV_PRI(2,ILDV,ICL,YRS) = 0.0
        ENDDO
      ENDDO
    ENDIF

    DO IREGN=1,MNUMCR-2
       IF (PETTR(IREGN,N) .EQ. 0.0) PETTR(IREGN,N) = PMGTR(IREGN,N)*1.18
    ENDDO

!...FLEXSHR calculates VMT shares for flex- and bi-fuel vehicles, PctAF. And more recently PctPHEV20.
    CALL FLEXSHR

!...Fuel cost (unit = nominal cents/mile)
    DO IREGN=1,MNUMCR-2
      FPRICE(1,IREGN,YRS)  =  PMGTR(IREGN,N)
      FPRICE(2,IREGN,YRS)  =  HWYPDSTR(IREGN,N)
      FPRICE(3,IREGN,YRS)  =  MIN(PETTR(IREGN,N),PMGTR(IREGN,N))
      FPRICE(4,IREGN,YRS)  =  PELVHTR(IREGN,N) 
      FPRICE(5,IREGN,YRS)  = (PctPHEV20(IREGN,N)*PELVHTR(IREGN,N)) + ((1.0-PctPHEV20(IREGN,N))*PMGTR(IREGN,N))
      FPRICE(6,IREGN,YRS)  = (PctPHEV50(IREGN,N)*PELVHTR(IREGN,N)) + ((1.0-PctPHEV50(IREGN,N))*PMGTR(IREGN,N)) 
      FPRICE(7,IREGN,YRS)  =  PELVHTR(IREGN,N)
      FPRICE(8,IREGN,YRS)  =  HWYPDSTR(IREGN,N)
      FPRICE(9,IREGN,YRS)  = (PCTAF(3,IREGN,N)*PGFTRPV(IREGN,N)) + ((1.0-PCTAF(3,IREGN,N))*PMGTR(IREGN,N))
      FPRICE(10,IREGN,YRS) = (PCTAF(4,IREGN,N)*PLGTR(IREGN,N)) + ((1.0-PCTAF(4,IREGN,N))*PMGTR(IREGN,N))
      FPRICE(11,IREGN,YRS) =  PGFTRPV(IREGN,N)
      FPRICE(12,IREGN,YRS) =  PLGTR(IREGN,N)
      FPRICE(13,IREGN,YRS) =  PMETR(IREGN,N)
      FPRICE(14,IREGN,YRS) =  PRICE_HY(IREGN,N)
      FPRICE(15,IREGN,YRS) =  PELVHTR(IREGN,N) 
      FPRICE(16,IREGN,YRS) =  PMGTR(IREGN,N)
    ENDDO

!...Convert fuel price in 1987$/mmBtu to 1990$ cents/gallon
    DO IREGN=1,MNUMCR-2
!...HMS-Hydrogen prices by market segment.
      HPrice(1,iregn,n)=ph1tr(iregn,n)*TMC_PGDP(1)*100.0*MG_HHV/1000.0
      HPrice(2,iregn,n)=ph2tr(iregn,n)*TMC_PGDP(1)*100.0*MG_HHV/1000.0
      HPrice(3,iregn,n)=ph3tr(iregn,n)*TMC_PGDP(1)*100.0*MG_HHV/1000.0
      
      DO ILDV=1,MAXLDV
        FPRICE(ILDV,IREGN,YRS) = FPRICE(ILDV,IREGN,YRS) * TMC_PGDP(1) * 100.0 * MG_HHV/1000.0
      ENDDO
    ENDDO

!...Calculate fuel cost
    DO IVTYP=1,MAXVTYP
      DO IREGN=1,MNUMCR-2
        DO ILDV=1,MAXLDV
          DO ICL=1,MAXCLASS
            FLCOST(IVTYP,ILDV,ICL,IREGN,YRS) = 0.0
            IF (LDV_MPG_CL(IVTYP,ILDV,ICL,YRS) .NE. 0.0) &
              FLCOST(IVTYP,ILDV,ICL,IREGN,YRS) = FPRICE(ILDV,IREGN,YRS)/LDV_MPG_CL(IVTYP,ILDV,ICL,YRS)
!...HMS-Calculate hydrogen fuel cost by market segment.
            if(ILDV.eq.14 .and. LDV_MPG_CL(IVTYP,ILDV,ICL,yrs) .ne. 0.0) then
			  if(exh.eq.0) then
                HFCost(1,IVTYP,ICL,iregn)= FLCOST(IVTYP,ILDV,ICL,IREGN,YRS)
                HFCost(2,IVTYP,ICL,iregn)= FLCOST(IVTYP,ILDV,ICL,IREGN,YRS)
                HFCost(3,IVTYP,ICL,iregn)= FLCOST(IVTYP,ILDV,ICL,IREGN,YRS)		  
			  else
                HFCost(1,IVTYP,ICL,iregn)= HPrice(1,iregn,n)/LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)
                HFCost(2,IVTYP,ICL,iregn)= HPrice(2,iregn,n)/LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)
                HFCost(3,IVTYP,ICL,iregn)= HPrice(3,iregn,n)/LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)
			  endif
            endif
          ENDDO
        ENDDO
      ENDDO
    ENDDO

!...Acceleration 0-60 mph (unit = seconds)
    DO IVTYP=1,MAXVTYP
      DO ILDV=1,MAXLDV
        DO ICL=1,MAXCLASS
          ACCL(IVTYP,ILDV,ICL,YRS) = 0.0
          IF(WGT(IVTYP,ILDV,ICL,YRS) .NE. 0.0) &
            ACCL(IVTYP,ILDV,ICL,YRS) = EXP(-0.002753221) * ((LDVHPW(IVTYP,ILDV,ICL,YRS) /   &
                                       WGT(IVTYP,ILDV,ICL,YRS)) ** (-.776131364))
        ENDDO
      ENDDO
    ENDDO

!...Maintenance & battery cost (unit = nominal $).  Convert cost in 1996$ back to 1987$ and then to nominal $
    DO ILDV=1,MAXLDV
      DO ICL=1,MAXCLASS
        MAINT(1,ILDV,ICL,YRS) = MAINTCAR(ILDV,ICL)/MC_JPGDP(7) * TMC_PGDP(N)
        MAINT(2,ILDV,ICL,YRS) = MAINTTRK(ILDV,ICL)/MC_JPGDP(7) * TMC_PGDP(N)
      ENDDO
    ENDDO

!...Luggage space (unit = relative to gasoline)
    DO ILDV=1,MAXLDV
      DO ICL=1,MAXCLASS
        LUGG(1,ILDV,ICL) = LUGGCAR(ILDV,ICL)
        LUGG(2,ILDV,ICL) = LUGGTRK(ILDV,ICL)
      ENDDO
    ENDDO

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
    REAL    tmpsum   
	REAL    sta_rat_base(maxfuel)
      
    CHARACTER*3 iflab(8)         
    DATA iflab/'Gas','Dsl','Eth','Mth','CNG','LPG','Elc','Hyd'/      

!...Re-assign initial number of refueling stations according to NREL data by Census Division
    if(curcalyr.le.2012)then
      do iregn=1, mnumcr-2
        do ifuel=1,maxfuel
          if(curcalyr.le.1995)then
            ALTSTA(iregn,ifuel,n) = INITSTA(ifuel,1,iregn)
            HSTAT(n,iregn,1)      = INITSTA(8,1,iregn)         ! Seed the H2 stations
          else
            ALTSTA(iregn,ifuel,n) = INITSTA(ifuel,n-5,iregn)
            HSTAT(n,iregn,1)      = INITSTA(8,n-5,iregn)       ! Seed the H2 stations
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

      if(curcalyr.ge.2020) then
        do ifuel=2,maxfuel
          Sta_rat(ifuel) = sta_rat_base(ifuel) + (predstk(ifuel,n)/predstk(1,n))*sta_rat(1)
        enddo
      endif
		  
!...  Calculate the total number of refueling stations needed based on an historic
!...  ratio of vehicle stock per refueling station
      do ifuel=1,maxfuel
        ALTSTAT(ifuel,N) = ALTSTAT(ifuel,N-1)+((PREDSTK(ifuel,n)-PREDSTK(ifuel,n-1))/STA_RAT(ifuel))
      enddo
	  
!...  Regionalize the predicted stations by regional vehicle sales 
      do iregn=1,mnumcr-2 
!...    Gasoline
        FUELVSAL(iregn,1,n) = sum(NCSTECH(iregn,1:MAXCLASS, 1,n-1)) + sum(NLTECH(iregn,1:MAXCLASS, 1,n-1)) + &
                              sum(NCSTECH(iregn,1:MAXCLASS,16,n-1)) + sum(NLTECH(iregn,1:MAXCLASS,16,n-1))
!...    Diesel
        FUELVSAL(iregn,2,n) = sum(NCSTECH(iregn,1:MAXCLASS, 2,n-1)) + sum(NLTECH(iregn,1:MAXCLASS, 2,n-1)) + &
                              sum(NCSTECH(iregn,1:MAXCLASS, 8,n-1)) + sum(NLTECH(iregn,1:MAXCLASS, 8,n-1))
!...    Ethanol
        FUELVSAL(iregn,3,n) = sum(NCSTECH(iregn,1:MAXCLASS, 3,n-1)) + sum(NLTECH(iregn,1:MAXCLASS, 3,n-1)) 
!...    Methanol
        FUELVSAL(iregn,4,n) = sum(NCSTECH(iregn,1:MAXCLASS,13,n-1)) + sum(NLTECH(iregn,1:MAXCLASS,13,n-1))
!...    CNG
        FUELVSAL(iregn,5,n) = sum(NCSTECH(iregn,1:MAXCLASS, 9,n-1)) + sum(NLTECH(iregn,1:MAXCLASS, 9,n-1)) + &
                              sum(NCSTECH(iregn,1:MAXCLASS,11,n-1)) + sum(NLTECH(iregn,1:MAXCLASS,11,n-1))
!...    LPG
        FUELVSAL(iregn,6,n) = sum(NCSTECH(iregn,1:MAXCLASS,10,n-1)) + sum(NLTECH(iregn,1:MAXCLASS,10,n-1)) + &
                              sum(NCSTECH(iregn,1:MAXCLASS,12,n-1)) + sum(NLTECH(iregn,1:MAXCLASS,12,n-1))
!...    Electric
        FUELVSAL(iregn,7,n) = sum(NCSTECH(iregn,1:MAXCLASS, 4,n-1)) + sum(NLTECH(iregn,1:MAXCLASS, 4,n-1)) + &
                              sum(NCSTECH(iregn,1:MAXCLASS, 5,n-1)) + sum(NLTECH(iregn,1:MAXCLASS, 5,n-1)) + &                                  
                              sum(NCSTECH(iregn,1:MAXCLASS, 6,n-1)) + sum(NLTECH(iregn,1:MAXCLASS, 6,n-1)) + & 
                              sum(NCSTECH(iregn,1:MAXCLASS, 7,n-1)) + sum(NLTECH(iregn,1:MAXCLASS, 7,n-1)) + &
							  sum(NCSTECH(iregn,1:MAXCLASS,15,n-1)) + sum(NLTECH(iregn,1:MAXCLASS,15,n-1)) 
!...    Hydrogen
        FUELVSAL(iregn,8,n) = sum(NCSTECH(iregn,1:MAXCLASS,14,n-1)) + sum(NLTECH(iregn,1:MAXCLASS,14,n-1))
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

!...  ALTSTA is no longer used for the hydrogen fuel availability calculation, the actual number of stations for each
!...  hydrogen market as calculated in the HMM, HSTAT, is used instead.  The following code is left so that we may 
!...  report the total regional stations below.
      do iregn=1,mnumcr-2
        do ifuel=1,maxfuel
          if(ifuel.eq.8.and.exh.eq.1) then  
            tmpsum = 0.0
            do hms = 1,3
              tmpsum = tmpsum + HSTAT(n-1,iregn,hms)
!             write(H2UNIT,'(2x,a,3i4,2f10.0)') 'H2 stations debug:',iregn,ifuel,hms,HSTAT(n-1,iregn,hms),tmpsum
            enddo
            ALTSTA(iregn,ifuel,n) = tmpsum
          else
            ALTSTA(iregn,ifuel,n) = ALTSTAT(ifuel,n) * AFVSHREG(iregn,ifuel,n)
          endif
        enddo
      enddo		
    endif

!...Estimate fuel availability
    do iregn=1,mnumcr-2 
      do ifuel=1,maxfuel
        if(ALTSTA(iregn,1,n).gt.0.0) FAVAIL(ifuel,n,iregn) = ALTSTA(iregn,ifuel,n)/ALTSTA(iregn,1,n)

!...    Do not allow fuel availability to decrease
        if(curcalyr.gt.2012)then
          if(FAVAIL(ifuel,n,iregn).lt.PCTFAVAIL(ifuel,n,iregn)) FAVAIL(ifuel,n,iregn) = PCTFAVAIL(ifuel,n,iregn)
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
        if(curcalyr.ge.2012.and.ifuel.eq.8.and.exh.eq.0) FAVAIL(8,N,IREGN) = FAVAIL(5,N,IREGN)                                                                         

!...    Do not allow any fuel availability to be larger than gasoline (100%)
        FAVAIL(ifuel,n,iregn) = min(FAVAIL(ifuel,n,iregn),FAVAIL(1,n,iregn))
      enddo
    enddo

!...Hydrogen fuel availability by market segment.
    do iregn=1,mnumcr-2
      do hms=1,3
if ((AltSta(iregn,1,n)*MSStkS(hms,iregn)) .ne. 0.0) &
        HFAvl(hms,iregn,n)=max(HFAvl(hms,iregn,n-1),HSTAT(N-1,IREGN,hms)/(AltSta(iregn,1,n)*MSStkS(hms,iregn)))
        if(HFSAvil(hms).eq.1) HFAvl(hms,iregn,n)=max(HFAvl(hms,iregn,n),HFAvil(hms,n)) 
        if(HFAvl(hms,iregn,n).gt.1.0) HFAvl(hms,iregn,n)=1.0
      enddo
    enddo

!...Write out fuel availability.  (Note:  The number for hydrogen here is not used, HFAvl is used instead)
    if(fcrl.eq.1.and.pass.eq.3.and.passno.eq.2) then
      write(H2UNIT,'(a,i4)') 'Fuel Availability by Region for ',n+1989
      do ifuel=1,maxfuel
        write(H2UNIT,'(2x,a,9f10.7)') iflab(ifuel),(favail(ifuel,n,iregn),iregn=1,mnumcr-2)
      end do
    endif
!...Write out hydrogen fuel availability by market segment.
    if(fcrl.eq.1.and.pass.eq.3.and.passno.eq.2) then
      write(H2UNIT,'(a,i4)') 'Hydrogen Fuel Availability by Market Segment and Region for ',n+1989
      do hms=1,3
        if(hms.eq.1) write(H2UNIT,'(a)') ' Large City'
        if(hms.eq.2) write(H2UNIT,'(a)') ' Small City'
        if(hms.eq.3) write(H2UNIT,'(a)') ' Rural'
        write(H2UNIT,'(2x,a,9f10.4)') 'Gas Shr ',(MSStkS(hms,iregn),iregn=1,mnumcr-2)
        write(H2UNIT,'(2x,a,9f10.4)') 'Hyd Shr ',(HVStkS(hms,iregn,n),iregn=1,mnumcr-2)
        write(H2UNIT,'(2x,a,9f10.0)') 'Gas Sta ',(AltSta(iregn,1,n)*MSStkS(hms,iregn),iregn=1,mnumcr-2)
        write(H2UNIT,'(2x,a,9f10.0)') 'Hyd Sta ',(HSTAT(N-1,iregn,hms),iregn=1,mnumcr-2)
        write(H2UNIT,'(2x,a,9f10.5)') 'Hyd FAvl',(HFAvl(hms,iregn,n),iregn=1,mnumcr-2)
      end do
    endif      

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
      FAVL(16,iregn,yrs) = max(FAVAIL(7,n,iregn),FAVAIL(1,n,iregn))
    enddo

    do IVTYP=1,maxvtyp 
      do iregn=1,mnumcr-2 
        do ICL=1,MAXCLASS 
          do ILDV=1,maxldv 
            VRNG(IVTYP,ILDV,ICL,yrs)  = LDV_RNG(IVTYP,ILDV,ICL,yrs)			
            HFUEL(IVTYP,ILDV,ICL,yrs) = 0.0
            if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.eq.15) HFUEL(IVTYP,ILDV,ICL,yrs) = 1.0 
          enddo
        enddo
      enddo
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
      data TAltLabel/'Gasoline','ADV Diesel','Ethanol Flex','Electric - 100','PHEV20','PHEV50', &
       'Electric - 200','Diesel Hybrid','CNG Bi-Fuel','LPG Bi-Fuel','CNG','LPG','FC Methanol', &
       'FC Hydrogen','Electric - 300','Gasoline Hybrid'/
	
      !HMS-These variable are for the hydrogen market segment project.
      real TFLCost,TFAvl,TotSales,TecSales(MAXLDV),XSales,TmpSales

      DATA (L2GROUP(1,JT),JT=1,6) / 1, 2, 3, 9,10, 0/  ! conventional (level 1 logit)
      DATA (L2GROUP(2,JT),JT=1,6) /16, 8, 5, 6, 0, 0/  ! elec. hybrid (level 1)
      DATA (L2GROUP(3,JT),JT=1,6) /11,12, 0, 0, 0, 0/  ! dedicated gaseous(level 1) 
      DATA (L2GROUP(4,JT),JT=1,6) /13,14, 0, 0, 0, 0/  ! fuel cell (level 1) 
      DATA (L2GROUP(5,JT),JT=1,6) / 4, 7,15, 0, 0, 0/  ! electric vehicle (level 1) 
      DATA (L2GRPTOT(JG) ,JG=1,5) / 5, 4, 2, 2, 3/     ! lookup table index for size of group

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

      DO IVTYP=1,MAXVTYP
        DO ILDV=1,MAXLDV
          DO IREGN=1,MNUMCR-2   
            X210(IVTYP,ILDV,IREGN) = ATVCOEFF(ILDV,n,IVTYP)
          ENDDO
        ENDDO
      ENDDO
     
! ... If not obtaining make/model availability data from TRNLDV.XML set current
! ... year values to maximum regional market penetration from previous year.
! ... First year (i.e., 1990) values set to 0.000001 times 100 (since subsequent
! ... algorithms assume data is in percent and therefore divide by 100).

      if(yrs.gt.2010) then
        DO IREGN = 1,(MNUMCR-2) 
          DO IVTYP=1,MAXVTYP
            DO ICL=1,MAXCLASS
              DO ILDV=1,MAXLDV
                IF(ILDV .EQ. 1) MMAVAIL(IVTYP,ICL,ILDV,IREGN,yrs) = 1.0
                MMAVAIL(IVTYP,ICL,ILDV,IREGN,yrs) = AMAX1(MMAVAIL(IVTYP,ICL,ILDV,IREGN,yrs-1),  &
                                                    (APSHR44(IVTYP,ICL,IREGN,ILDV,N-1)/2.0),0.000001)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF

! ... Temporarily set battery replacement cost to zero to nullify its impact on
! ... market penetrations.  If/when battery replacement costs are extracted from
! ... overall maintenance costs, this initialization should be removed in favor
! ... of a more appropriate alternative.
      DO IVTYP=1,MAXVTYP
        DO ICL=1,MAXCLASS
          DO ILDV=1,MAXLDV
            BRCOST25(IVTYP,ILDV,ICL,YRS) = 0.0
          ENDDO
        ENDDO
      ENDDO

      DO IREGN=1,(MNUMCR-2)
        DO IVTYP=1,MAXVTYP
          DO ICL=1,MAXCLASS
            DO JG=1,5
              ETOT(JG) = 0.0
              DO JT=1,L2GRPTOT(JG)        !tech within group
                ILDV = L2GROUP(JG,JT)
                UISUM(JT) = 0.0
                IF (PSPR(IVTYP,ILDV,ICL,YRS) .LT. 0.0) STOP 450

! ... Calculate value functions.  Because of the formulation, VRNG must > 0
! ... The importance of this should not be underestimated, it is more than a simple divide
! ... by zero precaution.  The requirement for a non-zero range estimate actually allows
! ... the ATV penetration model to properly bypass non-existant vehicle classes (assuming,
! ... of course, that the ranges for these classes are set to zero).

                IF (VRNG(IVTYP,ILDV,ICL,YRS) .GT. 0.0) THEN
		   
                  ! Provide hydrogen fuel cost and fuel availability by hydrogen market segment. 
                  TFlCost=FlCost(IVTYP,ILDV,ICL,iregn,yrs)
                  TFAvl=FAvl(ILDV,iregn,yrs)
                  if(ILDV.eq.14) then
                    TFlCost=HFCost(HMSLoop,IVTYP,ICL,iregn)
                    TFAvl=HFAvl(HMSLoop,iregn,n)
                  endif                
                
                  !Gather data for the debug writes.
                  cof(1)=x21(IVTYP,ICL); var(1)=pspr(IVTYP,ILDV,ICL,yrs); rst(1)=cof(1)*var(1)
                  cof(2)=x22(IVTYP,ICL); var(2)=TFlCost; rst(2)=cof(2)*var(2)
                  cof(3)=x23(IVTYP,ICL); var(3)=vrng(IVTYP,ILDV,ICL,yrs); rst(3)=cof(3)*(1.0/var(3))
                  cof(4)=x24(IVTYP,ICL); var(4)=brcost25(IVTYP,ILDV,ICL,yrs); rst(4)=cof(4)*var(4)
                  cof(5)=x25(IVTYP,ICL); var(5)=accl(IVTYP,ILDV,ICL,yrs); rst(5)=cof(5)*var(5)
                  cof(6)=x26(IVTYP,ICL); var(6)=hfuel(IVTYP,ILDV,ICL,yrs); rst(6)=cof(6)*var(6)
                  cof(7)=x27(IVTYP,ICL); var(7)=maint(IVTYP,ILDV,ICL,yrs); rst(7)=cof(7)*var(7)
                  cof(8)=x28(IVTYP,ICL); var(8)=lugg(IVTYP,ILDV,ICL); rst(8)=cof(8)*var(8)
                  cof(9)=betafa22(IVTYP,ICL); var(9)=TFAvl; rst(9)=0.0
                  cof(10)=betafa2(IVTYP,ICL); var(10)=0.0; rst(10)=cof(10)*exp(cof(9)*var(9))
                  cof(11)=x29(IVTYP,ICL); var(11)=mmavail(IVTYP,ICL,ILDV,iregn,yrs); rst(11)=cof(11)*alog(var(11))
                  cof(12)=x210(IVTYP,ILDV,iregn); var(12)=0.0; rst(12)=cof(12)

                  if(ILDV.ne.3.or.ILDV.ne.9.or.ILDV.ne.10) then

! ... Value function for all technologies except group 1 (conventional) FFV's and bi-fuel's
                    UISUM(JT) = X21(IVTYP,ICL) * PSPR(IVTYP,ILDV,ICL,YRS) +         &
                                X22(IVTYP,ICL) * TFlCost + &
                                X23(IVTYP,ICL) * (1/VRNG(IVTYP,ILDV,ICL,YRS)) +     &
                                X24(IVTYP,ICL) * BRCOST25(IVTYP,ILDV,ICL,YRS) +     &
                                X25(IVTYP,ICL) * ACCL(IVTYP,ILDV,ICL,YRS) +         &
                                X26(IVTYP,ICL) * HFUEL(IVTYP,ILDV,ICL,YRS) +        &
                                X27(IVTYP,ICL) * MAINT(IVTYP,ILDV,ICL,YRS) +        &
                                X28(IVTYP,ICL) * LUGG(IVTYP,ILDV,ICL) +             &
                                BETAFA2(IVTYP,ICL) * EXP(BETAFA22(IVTYP,ICL)*TFAvl) + &
                                X29(IVTYP,ICL) * ALOG(MMAVAIL(IVTYP,ICL,ILDV,IREGN,yrs)) +  &
                                X210(IVTYP,ILDV,IREGN)

                  ELSE

! ... Value function for FFV's and bi-fuels.  Note: fuel availability and range are
! ... calculated in call statements w/case(3), ..., case(6) and are in the generalized
! ... cost calculation, gencost.
                    XCOST = -10.0
                    select case (ILDV)          
                      CASE(3) ! Ethanol FFV
                        CALL TALT314(XCOST,X31(IVTYP,ICL),     &
                                           X22(IVTYP,ICL),     &
                                           X23(IVTYP,ICL),     &
                                           BETAFA2(IVTYP,ICL), &
                                           BETAFA22(IVTYP,ICL))
                      CASE(9) ! CNG bifuel
                        CALL TALT315(XCOST,X31(IVTYP,ICL),     &
                                           X22(IVTYP,ICL),     &
                                           X23(IVTYP,ICL),     &
                                           BETAFA2(IVTYP,ICL), &
                                           BETAFA22(IVTYP,ICL))
                      CASE(10) ! LPG bifuel
                        CALL TALT316(XCOST,X31(IVTYP,ICL),     &
                                           X22(IVTYP,ICL),     &
                                           X23(IVTYP,ICL),     &
                                           BETAFA2(IVTYP,ICL), &
                                           BETAFA22(IVTYP,ICL))
                      CASE DEFAULT
                        STOP 440
                    END SELECT
                    UISUM(JT) = X21(IVTYP,ICL) * PSPR(IVTYP,ILDV,ICL,YRS) +        &
                                X22(IVTYP,ICL) * XCOST +                             &
                                X24(IVTYP,ICL) * BRCOST25(IVTYP,ILDV,ICL,YRS) +    &
                                X25(IVTYP,ICL) * ACCL(IVTYP,ILDV,ICL,YRS) +        &
                                X26(IVTYP,ICL) * HFUEL(IVTYP,ILDV,ICL,YRS) +       &
                                X27(IVTYP,ICL) * MAINT(IVTYP,ILDV,ICL,YRS) +       &
                                X28(IVTYP,ICL) * LUGG(IVTYP,ILDV,ICL) +            &
                                X29(IVTYP,ICL) * ALOG(MMAVAIL(IVTYP,ICL,ILDV,IREGN,yrs)) + &
                                X210(IVTYP,ILDV,IREGN)

                    !Overwrite some data gathered for the debug writes.
                    cof(2)=x22(IVTYP,ICL); var(2)=xcost; rst(2)=cof(2)*var(2)
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
                if(n.eq.(ijumpyr-10).or.n.eq.ijumpyr) then
                 if(fcrl.eq.1.and.pass.eq.2.and.passno.eq.2.and.iregn.eq.5) then
                  if(IVTYP.eq.1.and.ICL.eq.5) then
                   DoBug=1
                  endif
                 endif
                endif

                !Write out details for each technology in each group.
                if(DoBug.eq.1) then
                 if(jt.eq.1) write(H2UNIT,'(/,a,i3,a,4i5,a)') 'Logit Nest: ',jg,'  (year, region, type, class: ',n+1989,iregn,IVTYP,ICL,')'
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

              ENDDO !JT 

! ... Level 2 shares
              DO JT=1,L2GRPTOT(JG)
                XSHARE(JG,JT) = 0.0
                IF (ETOT(JG) .NE. 0.0) XSHARE(JG,JT) = ESUM(JT)/ETOT(JG) * 100.0
! ... Write out details for the technology shares in the group.
                if(DoBug.eq.1) then
                 if(ESum(jt).eq.0.0) then
                  write(H2UNIT,'(a,3i3,2x,a,10x,a,f12.5)') '  Group Share: ',jg,jt,l2group(jg,jt),taltlabel(l2group(jg,jt)),'na',xshare(jg,jt)/100.0
                 else
                  write(H2UNIT,'(a,3i3,2x,a,f12.3,f12.5)') '  Group Share: ',jg,jt,l2group(jg,jt),taltlabel(l2group(jg,jt)),UISum(jt),xshare(jg,jt)/100.0
                 end if
                end if
              ENDDO

! ... Readjust hybrid shares (jg=2). Do PHEVPlug fraction of PHEV share (jt=3), put it back into other two hybrids.
! ... Not that the overall generalized cost stays the same, even with the reshuffling of these shares.
              if(jg.eq.2) then
               if(xshare(jg,1)+xshare(jg,2).gt.0.0) then
                TmpShr=xshare(jg,1)/(xshare(jg,1)+xshare(jg,2))
                TmpPlug=(xshare(jg,3)*(1.0-PHEVPlug(n)))+(xshare(jg,4)*(1.0-PHEVPlug(n)))
                TmpPlug1=xshare(jg,3)*(1.0-PHEVPlug(n))
                TmpPlug2=xshare(jg,4)*(1.0-PHEVPlug(n))
                xshare(jg,3)=xshare(jg,3)-TmpPlug1
                xshare(jg,4)=xshare(jg,4)-TmpPlug2
                xshare(jg,1)=xshare(jg,1)+(TmpPlug*TmpShr)
                xshare(jg,2)=xshare(jg,2)+(TmpPlug*(1.0-TmpShr))
! ... Write out details for the technology shares in the group.
                if(DoBug.eq.1) then
                 write(H2UNIT,'(a,f8.3)') '  Recalculated group share for hybrids. PHEVPlug fraction is: ',PHEVPlug(n)
                 do jt=1,l2grptot(jg)
                  write(H2UNIT,'(a,3i3,2x,a,f12.5)') '  Group Share: ',jg,jt,l2group(jg,jt),taltlabel(l2group(jg,jt)),xshare(jg,jt)/100.0
                 end do
                end if
               endif
              endif

              GCOST(JG) = 0.0
              IF (ETOT(JG) .NE. 0.0) GCOST(JG) = (1/X21(IVTYP,ICL)) * DLOG(ETOT(JG))

            ENDDO !JG

! ... Level 1 shares
            ETOT(1) = 0.0

            DO JG=1,5
              UISUM(JG) = 0.0
              UISUM(JG) = X11(IVTYP,ICL) * GCOST(JG)
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
              if(DoBug.eq.1) then
               if(jg.eq.1) then
                write(H2UNIT,'(/,a,4i5,a)') 'Overall Group Shares:  (year, region, type, class: ',n+1989,iregn,IVTYP,ICL,')'
                write(H2UNIT,'(2x,a,2f10.6)') 'x21 and x11:   ',x21(IVTYP,ICL),x11(IVTYP,ICL)
                write(H2UNIT,'(7x,a)') '     GCost     TUtil     Share'
               end if
               write(H2UNIT,'(2x,i3,2x,f10.1,f10.3,f10.5)') jg,gcost(jg),uisum(jg),yshare(jg)/100.0
              end if
            ENDDO

            GENCOST = 0.0
            IF (ETOT(1) .NE. 0.0) GENCOST = (1/X11(IVTYP,ICL)) * DLOG(ETOT(1))

            IFUELX = 1

            DO JG=1,5
              DO JT=1,L2GRPTOT(JG)
                XGS(ifuelx) = XSHARE(JG,JT) * YSHARE(JG) / 100.0
                !HMS-Capture the shares by hydrogen market segment.
                HAPShr44(HMSLoop,IVTYP,ICL,iregn,L2Group(jg,jt))=xgs(ifuelx)/100.0
                ifuelx = ifuelx + 1
              ENDDO
            ENDDO

! ... HMS-Create the weighted average shares totaled over large city, small city, and rural, using sales
! ... split into market segments. Save the hydrogen vehicle sales by market segment.
            if(HMSLoop.eq.3) then
              if(IVTYP.eq.1) XSales=ncs(iregn,ICL,n)
              if(IVTYP.eq.2) XSales=nlts(iregn,ICL,n)
              TotSales=0.0
              do ILDV=1,MAXLDV
                TecSales(ILDV)=0.0
                do hms=1,3
                  TmpSales=XSales*MSSplit(hms,iregn,n)*HAPShr44(hms,IVTYP,ICL,iregn,ILDV)
                  TecSales(ILDV)=TecSales(ILDV)+TmpSales
                  if(ILDV.eq.14) HVSales(hms,IVTYP,ICL,iregn)=TmpSales
                enddo
                TotSales=TotSales+TecSales(ILDV)
              end do
              do ILDV=1,MAXLDV
                APShr44(IVTYP,ICL,iregn,ILDV,n)=TecSales(ILDV)/TotSales
              enddo
            endif

	        If(curcalyr.le.stockyr) then
	          do ILDV=1,maxldv
		        APShr44(IVTYP,ICL,iregn,ILDV,n) = LDV_Stock(iregn,IVTYP,1,ILDV,1,1,n)/sum(LDV_Stock(iregn,IVTYP,1,1:maxldv,1,1,n))
	          enddo
	        endif
	  
! ... Write out details for the final shares for each of the technologies only once have market average of hdyrogen vehicles
            if(HMSLoop.eq.3) then
              DO JG=1,5
                DO JT=1,L2GRPTOT(JG)
                  if(DoBug.eq.1.and.fcrl.eq.1) then
                    if(jg.eq.1.and.jt.eq.1) write(H2UNIT,'(/,a,4i5,a)') 'Overall Technology Shares:  (year, region, type, class: ',n+1989,iregn,IVTYP,ICL,')'
                      write(H2UNIT,'(2x,i3,2x,a,f12.5)') l2group(jg,jt),taltlabel(l2group(jg,jt)),apshr44(IVTYP,ICL,iregn,l2group(jg,jt),n)
                  endif
                ENDDO
              ENDDO
            endif

! ... Write out shares and sales for a region, type, and class.
!      if(HMSLoop.eq.3.and.fcrl.eq.1.and.passno.eq.2.and.pass.eq.3) then
!        if(IVTYP.eq.2.and.ICL.eq.5.and.iregn.eq.5) then
!          write(H2UNIT,'(/,a,i4,3(a,i1))') 'Year ',curiyr+1989,', Region ',iregn,', Type ',IVTYP,', Class ',ICL
!          write(H2UNIT,'(a)') ' Alternate Fuel Technology Vehicle Shares'
!          write(H2UNIT,'(a)') '              Large City  Small City       Rural'
!          do ILDV=1,MAXLDV
!            write(H2UNIT,'(2x,i2,1x,a,1x,3f12.5)') ILDV,TAltLabel(ILDV),(HAPShr44(hms,IVTYP,ICL,iregn,ILDV),hms=1,3)
!          end do
!          write(H2UNIT,'(a)') ' Alternate Fuel Technology Vehicle Sales (millions)'
!          write(H2UNIT,'(a)') '              Large City  Small City       Rural       Total'
!          if(IVTYP.eq.1) XSales=ncs(iregn,ICL,n)
!          if(IVTYP.eq.2) XSales=nlts(iregn,ICL,n)
!          write(H2UNIT,'(a,4f12.4)') ' Total      ',(XSales*MSSplit(hms,iregn,n),hms=1,3),XSales
!          XSales=HVSales(1,IVTYP,ICL,iregn)+HVSales(2,IVTYP,ICL,iregn)+HVSales(3,IVTYP,ICL,iregn)
!          write(H2UNIT,'(a,4f12.4)') ' Hydrogen   ',(HVSales(hms,IVTYP,ICL,iregn),hms=1,3),XSales
!        endif
!      endif

          ENDDO
        ENDDO
      ENDDO

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
      FLCOST313(1) = FLCOST(IVTYP,1,ICL,IREGN,YRS)
      VRANG313(1) =  1 / VRNG(IVTYP,1,ICL,YRS)
      FAVAL313(1) = FAVL(1,IREGN,YRS)

! ... VL3IT = 1 - M85
      FLCOST313(2) = FLCOST(IVTYP,6,ICL,IREGN,YRS)
      VRANG313(2) =  1 / VRNG(IVTYP,6,ICL,YRS)
      FAVAL313(2) = FAVL(6,IREGN,YRS)

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
      FLCOST314(1) = FLCOST(IVTYP,1,ICL,IREGN,YRS)
      VRANG314(1) =  1 / VRNG(IVTYP,1,ICL,YRS)
      FAVAL314(1) = FAVL(1,IREGN,YRS)

! ... VL4IT = 2 - E85
      FLCOST314(2) = FLCOST(IVTYP,3,ICL,IREGN,YRS)
      VRANG314(2) =  1 / VRNG(IVTYP,3,ICL,YRS)
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
      FLCOST315(1) = FLCOST(IVTYP,1,ICL,IREGN,YRS)
      VRANG315(1) =  1 / VRNG(IVTYP,1,ICL,YRS)
      FAVAL315(1) = FAVL(1,IREGN,YRS)

! ... VL5IT = 2 - CNG
      FLCOST315(2) = FLCOST(IVTYP,11,ICL,IREGN,YRS)
      VRANG315(2) =  1 / VRNG(IVTYP,11,ICL,YRS)
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
      FLCOST316(1) = FLCOST(IVTYP,1,ICL,IREGN,YRS)
      VRANG316(1) =  1 / VRNG(IVTYP,1,ICL,YRS)
      FAVAL316(1) = FAVL(1,IREGN,YRS)

! ... VL6IT = 2 - LPG
      FLCOST316(2) = FLCOST(IVTYP,12,ICL,IREGN,YRS)
      VRANG316(2) =  1 / VRNG(IVTYP,12,ICL,YRS)
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

    REAL    debugsales(maxvtyp,maxfleet,MAXCLASS,maxldv)
!	REAL    FltLDVShrReg(mnumcr,maxvtyp,maxldv,maxfleet)		! MDRAEO2022 | Commented out, doesn't work as expected.

!...fleet vehicle sales by car and lt truck
!...size class distribution
    do ifleet=1,maxfleet
	  do ICL=1,MAXCLASS
		FLTSSHR(ICL,n,ifleet,1)=FLTSSHRC(ICL,n,ifleet)
		FLTSSHR(ICL,n,ifleet,2)=FLTSSHRT(ICL,n,ifleet)
	  enddo
    enddo
	
!...populate historical vehicle sales from stock 
    if(curcalyr.le.stockyr)then
      do IVTYP=1,maxvtyp
	    do ifleet=1,maxfleet
		  do ICL=1,MAXCLASS
		    do ILDV=1,maxldv
			  do iregn=1,mnumcr-2
		        if(ILDV.le.2)then
	              FltechSal(iregn,IVTYP,ifleet,ICL,ILDV,1)=LDV_Stock(iregn,IVTYP,ifleet+1,ILDV,1,1,n)*FltsShr(ICL,n,ifleet,IVTYP)*1000000.0
			    else
			      if(IVTYP.eq.1) FltechSal(iregn,IVTYP,ifleet,ICL,ILDV,1)=LDV_Stock(iregn,IVTYP,ifleet+1,ILDV,1,1,n)*FltAFShrC(ICL,n,ifleet)*1000000.0
				  if(IVTYP.eq.2) FltechSal(iregn,IVTYP,ifleet,ICL,ILDV,1)=LDV_Stock(iregn,IVTYP,ifleet+1,ILDV,1,1,n)*FltAFShrT(ICL,n,ifleet)*1000000.0
			    endif
			  enddo
            enddo
	      enddo
		enddo
	  enddo

	else ! post stockyr
!...  fleet vehicle sales by size class by fuel type
      do IVTYP=1,maxvtyp
        do ifleet=1,maxfleet
          do ICL=1,MAXCLASS
            do ILDV=1,maxldv
	  	      do iregn=1,mnumcr-2  
                if(ILDV.eq.1)then
                  if(IVTYP.eq.1)then
				    FLTECHSAL(iregn,IVTYP,ifleet,ICL,ILDV,1)=NewCars(iregn,n)*Owner_Share(iregn,IVTYP,ifleet+1,n)*FLTSSHR(ICL,n,ifleet,IVTYP)* & 
					                                         FltCarShr(ILDV,n,ifleet)*1000000.0!*FltLDVShrReg(iregn,ivtyp,ildv,ifleet)
                  else
                    FLTECHSAL(iregn,IVTYP,ifleet,ICL,ILDV,1)=NewCLS12A(iregn,n)*Owner_Share(iregn,IVTYP,ifleet+1,n)*FLTSSHR(ICL,n,ifleet,IVTYP)* &
															 FltTrkShr(ILDV,n,ifleet)*1000000.0!*FltLDVShrReg(iregn,ivtyp,ildv,ifleet)
                  endif 
                else
                  if(IVTYP.eq.1)then
				    FLTECHSAL(iregn,IVTYP,ifleet,ICL,ILDV,1)=NewCars(iregn,n)*Owner_Share(iregn,IVTYP,ifleet+1,n)*FLTAFSHRC(ICL,n,ifleet)* &
															 FltCarShr(ILDV,n,ifleet)*1000000.0!*FltLDVShrReg(iregn,ivtyp,ildv,ifleet)
                  else
				    FLTECHSAL(iregn,IVTYP,ifleet,ICL,ILDV,1)=NewCLS12A(iregn,n)*Owner_Share(iregn,IVTYP,ifleet+1,n)*FLTAFSHRT(ICL,n,ifleet)* &
															 FltTrkShr(ILDV,n,ifleet)*1000000.0!*FltLDVShrReg(iregn,ivtyp,ildv,ifleet)
                  endif
                endif
			  enddo
            enddo
          enddo
        enddo
      enddo
    endif	

!...sum national sales	  
    do IVTYP=1,maxvtyp
	  do ifleet=1,maxfleet
	    do ICL=1,MAXCLASS
		  do ILDV=1,maxldv
	  	    FltechSal(mnumcr,IVTYP,ifleet,ICL,ILDV,1)=sum(FltechSal(1:mnumcr-2,IVTYP,ifleet,ICL,ILDV,1))
		  enddo
		enddo
	  enddo
    enddo
	
!...  apply California ZEV mandates to fleet sales - fleet zev                                 
!...  calculate regional sales for fleets
      fltsales=0.
	  do IVTYP=1,maxvtyp
        do ifleet=1,maxfleet
	      do ICL=1,MAXCLASS
	        do iregn=1,mnumcr-2
              do ILDV=1,maxldv
                fltsales(IVTYP,ifleet,ICL,iregn,ILDV) = fltechsal(iregn,IVTYP,ifleet,ICL,ILDV,1) 
			  enddo
		    enddo
          enddo
        enddo
      enddo
	
!...  populate adjusted fleet vehicle sales		
	  do IVTYP=1,maxvtyp
	    do ifleet=1,maxfleet    
		  do ICL=1,MAXCLASS
		    do iregn=1,mnumcr-2
		      do ILDV=1,maxldv
                afltsales(IVTYP,ifleet,ICL,iregn,ILDV) = fltsales(IVTYP,ifleet,ICL,iregn,ILDV)
			  enddo
            enddo
          enddo
	    enddo
	  enddo 
	
	  
!...Calculate mandated sales of ZEVs by participating states: 
!	CD1 = Connecticut, Massachusetts, Maine, Rhode Island, Vermont
!	CD2 = New York, New Jersey
!	CD4 = Minnesota (MY2025+)
!	CD5 = Maryland, Virginia (MY2025+)
!	CD8 = Colorado (MY2023+), Nevada (MY2025+), New Mexico (MY2026+)
!	CD9 = California, Oregon, Washington (2025+)

!...determine vehicle fleet sales that will be used to determine zev requirements
    do iregn=1,mnumcr-2
	  do ifleet=1,maxfleet
        do ILDV=1,maxldv
          fltsales_t(iregn,ifleet,ILDV,n)=sum(fltsales(1:maxvtyp,ifleet,1:MAXCLASS,iregn,ILDV))
        enddo
		covered_fltsales(iregn,ifleet) = SUM(fltsales_t(iregn,ifleet,1,n-2:n-4))/3
	  enddo
    enddo 

  	
!...izev index
!... 1 = ATPZEV = HEVG, HEVD, and CNG
!... 2 = TZEV   = PHEV20 and PHEV50
!... 3 = ZEV    = EV100, EV200, EV300, FCVH, FCVM

!...determine fleet zev credits needed by CD
    do iregn=1,mnumcr-2
      if(curcalyr.ge.2021.and.zev_state_alloc(iregn,n).gt.0.)then
        do ifleet=1,maxfleet
	      do izev=1,maxzev
                 zev_fltcredit_reg(iregn,ifleet,izev,n)=zev_state_alloc(iregn,n)*covered_fltsales(iregn,ifleet)*zev_requirement(izev,n)
                 IF (TRANAB32 .NE. 0) THEN
                   if(iregn.eq.9.and.izev.eq.2) & 
                      zev_fltcredit_reg(iregn,ifleet,izev,n)=zev_state_alloc(iregn,n)*covered_fltsales(iregn,ifleet)*tzev_req_ca_co2(n)
                   if(iregn.eq.9.and.izev.eq.3) & 
                      zev_fltcredit_reg(iregn,ifleet,izev,n)=zev_state_alloc(iregn,n)*covered_fltsales(iregn,ifleet)*zev_req_ca_co2(n)
                 ENDIF
          enddo
	  
!...      determine fleet zev credits earned by CD
          do ILDV=1,maxldv
		    zev_fltcredit_ldv(iregn,ifleet,ILDV,n) = fltsales_t(iregn,ifleet,ILDV,n)*zev_multiplier(ILDV,n)
            zev_fltcredit_earn(iregn,ifleet,1,n) = zev_fltcredit_ldv(iregn,ifleet,11,n)+zev_fltcredit_ldv(iregn,ifleet,16,n)											
            zev_fltcredit_earn(iregn,ifleet,2,n) = zev_fltcredit_ldv(iregn,ifleet,5,n) + zev_fltcredit_ldv(iregn,ifleet,6,n)
		    zev_fltcredit_earn(iregn,ifleet,3,n) = zev_fltcredit_ldv(iregn,ifleet,4,n) + zev_fltcredit_ldv(iregn,ifleet,7,n)+&
                                                   zev_fltcredit_ldv(iregn,ifleet,14,n) + zev_fltcredit_ldv(iregn,ifleet,15,n)
          enddo
		enddo
		
!...    verify CDs are in compliance with zev requirements, make sales adjustments if needed
!...    calculate zev compliance 1st, highest order constraint        
        fltsales_delta = 0.
		do ifleet=1,maxfleet
          if(zev_fltcredit_earn(iregn,ifleet,3,n).lt.zev_fltcredit_reg(iregn,ifleet,3,n)) then
		    do IVTYP=1,maxvtyp
		      do ICL=1,MAXCLASS
		        do ILDV=1,maxldv
				   if(ILDV.eq.15.and.covered_fltsales(iregn,ifleet).gt.0.0)then  !remove all but EV300 jma
					afltsales(IVTYP,ifleet,ICL,iregn,ILDV) = covered_fltsales(iregn,ifleet)*fltsshr(ICL,n,ifleet,IVTYP) * zev_state_alloc(iregn,n) * &
					                                         zev_requirement(3,n)/zev_multiplier(ILDV,n)
			        fltsales_delta(IVTYP,ifleet,ICL,iregn,ILDV) = afltsales(IVTYP,ifleet,ICL,iregn,ILDV)-fltsales(IVTYP,ifleet,ICL,iregn,ILDV)
				    afltsales(IVTYP,ifleet,ICL,iregn,1) = afltsales(IVTYP,ifleet,ICL,iregn,1)-fltsales_delta(IVTYP,ifleet,ICL,iregn,ILDV)
                  endif
			    enddo
			  enddo
		    enddo
          endif
		enddo
	            		
!...    calculate tzev compliance 2nd, include credits earned from zevs
        fltsales_delta = 0.
		do ifleet=1,maxfleet		
          if(zev_fltcredit_earn(iregn,ifleet,2,n).lt.zev_fltcredit_reg(iregn,ifleet,2,n)) then	
		    do IVTYP=1,maxvtyp
		      do ICL=1,MAXCLASS
		        do ILDV=1,maxldv
		          if(ILDV.eq.5.or.ILDV.eq.6.and.covered_fltsales(iregn,ifleet).gt.0.0)then
					afltsales(IVTYP,ifleet,ICL,iregn,ILDV) 		= (covered_fltsales(iregn,ifleet)*fltsshr(ICL,n,ifleet,IVTYP) * zev_state_alloc(iregn,n) * &
																  zev_requirement(2,n)/zev_multiplier(ILDV,n))
					afltsales(IVTYP,ifleet,ICL,iregn,ILDV) 		= TZEV_sales_dist(ILDV)*afltsales(IVTYP,ifleet,ICL,iregn,ILDV)
        	        fltsales_delta(IVTYP,ifleet,ICL,iregn,ILDV) = afltsales(IVTYP,ifleet,ICL,iregn,ILDV)-fltsales(IVTYP,ifleet,ICL,iregn,ILDV)
				    afltsales(IVTYP,ifleet,ICL,iregn,1) 		= afltsales(IVTYP,ifleet,ICL,iregn,1)-fltsales_delta(IVTYP,ifleet,ICL,iregn,ILDV)
                  endif
				enddo
			  enddo
			enddo
		  endif
		enddo
      endif  
	enddo

	if(curcalyr.gt.stockyr)then
	  fltechsal=0.
      do IVTYP=1,maxvtyp
	    do ifleet=1,maxfleet
          do ICL=1,MAXCLASS
            do ILDV=1,maxldv
              do iregn=1,mnumcr-2
                fltechsal(iregn,IVTYP,ifleet,ICL,ILDV,1) = afltsales(IVTYP,ifleet,ICL,iregn,ILDV)
			  enddo
			  fltechsal(mnumcr,IVTYP,ifleet,ICL,ILDV,1) = SUM(fltechsal(1:MNUMCR-2,IVTYP,ifleet,ICL,ILDV,1))
            enddo
          enddo
        enddo
      enddo
	endif		
	
!...Sum sales across size classes
    do IVTYP=1,maxvtyp
      do ifleet=1,maxfleet
        do ILDV=1,maxldv
		  do ihav = 1,maxhav
		    do iregn=1,mnumcr-2
              FLTECH(iregn,IVTYP,ifleet,ILDV,ihav) = sum(FLTECHSAL(iregn,IVTYP,ifleet,1:MAXCLASS,ILDV,ihav))
			enddo
			FLTECH(mnumcr,IVTYP,ifleet,ILDV,ihav) = SUM(FLTECH(1:MNUMCR-2,IVTYP,ifleet,ILDV,ihav))
		  enddo
        enddo
      enddo
    enddo
	
!...for 1995-stockyr read in fleet stock totals and distribute by fleet type, tech, and vintage
    Flt_Stock(:,:,:,:,:,:,n) = 0.0
	if(curcalyr.le.stockyr) then
	  do IVTYP=1,maxvtyp
		do ifleet=1,maxfleet
		  do ILDV=1,maxldv
		    do iregn=1,mnumcr-2
			  do iage=1,maxage
				Flt_Stock(iregn,IVTYP,ifleet,ILDV,iage,1,n) = LDV_STOCK(iregn,IVTYP,ifleet+1,ILDV,iage,1,n)*1000000.0
			  enddo
		    enddo
			do iage=1,maxage
			  Flt_Stock(mnumcr,IVTYP,ifleet,ILDV,iage,1,n) = LDV_Stock(mnumcr,IVTYP,ifleet+1,ILDV,iage,1,n)*1000000.0
			enddo
		  enddo
	    enddo
	  enddo
	elseif(curcalyr.gt.stockyr) then
      do IVTYP=1,maxvtyp
		do ifleet=1,maxfleet
		  do ILDV=1,maxldv
			do ihav = 1,maxhav
			  do iregn=1,mnumcr-2
			    do iage=2,maxage-1
				  Flt_Stock(iregn,IVTYP,ifleet,ILDV,iage,ihav,n)=Flt_Stock(iregn,IVTYP,ifleet,ILDV,iage-1,ihav,n-1) * SURVFLT(ifleet,iage-1,IVTYP)
			    enddo ! maxage-1
				Flt_Stock(iregn,IVTYP,ifleet,ILDV,maxage,ihav,n) = Flt_Stock(iregn,IVTYP,ifleet,ILDV,maxage-1,ihav,n-1) * SURVFLT(ifleet,maxage-1,IVTYP)+ &
																   Flt_Stock(iregn,IVTYP,ifleet,ILDV,maxage,ihav,n-1) * SURVFLT(ifleet,maxage,IVTYP)
			    Flt_Stock(iregn,IVTYP,ifleet,ILDV,1,ihav,n)	= FLTECH(iregn,IVTYP,ifleet,ILDV,ihav)
			  enddo ! mnumcr-2
			enddo ! maxhav
		  enddo ! maxldv
		enddo ! maxfleet
	  enddo ! maxtype
		
!...  assign fleet vehicles to household vehicles
!...  no HAVs transfer to household vehicles
!...  for years greater than stockyr, use fleet transfer rates based on 2012-2016 data by fleet type 
	  OLDFSTK=0.0
	  do iregn=1,mnumcr-2
	    do ifleet=1,maxfleet
		  do ILDV=1,maxldv
		    if(ILDV.lt.9.or.ILDV.gt.12)then	! Keep natural gas and propane vehicles in the fleet
		      do iage=1,maxage
			    OLDFSTK(iregn,1,ifleet,ILDV,iage)=Flt_Stock(iregn,1,ifleet,ILDV,iage,1,n) * FLTTRANSPC(ifleet,iage)
			    OLDFSTK(iregn,2,ifleet,ILDV,iage)=Flt_Stock(iregn,2,ifleet,ILDV,iage,1,n) * FLTTRANSLT(ifleet,iage)
		      enddo
			  
			  do iage=1,maxage
			    do IVTYP=1,maxvtyp
			        Flt_Stock(iregn,IVTYP,ifleet,ILDV,iage,1,n)=Flt_Stock(iregn,IVTYP,ifleet,ILDV,iage,1,n) - OLDFSTK(iregn,IVTYP,ifleet,ILDV,iage)
			    enddo
			  enddo
			  
		    endif
		  enddo
		enddo
	  enddo

!...  fill ldv_stock array
	  do iregn=1,mnumcr-2
        do IVTYP=1,maxvtyp
	      do ifleet=1,maxfleet
		    do ILDV=1,maxldv
		      do iage=1,maxage
			    do ihav=1,maxhav
			      LDV_Stock(iregn,IVTYP,ifleet+1,ILDV,iage,ihav,n) = Flt_Stock(iregn,IVTYP,ifleet,ILDV,iage,ihav,n)/1000000.0
				enddo
			  enddo
			enddo
	      enddo
		enddo
	  enddo
	  
!...  sum national 
      do IVTYP=1,maxvtyp
	    do ifleet=1,maxfleet
		  do ILDV=1,maxldv
		    do iage=1,maxage
			  do ihav=1,maxhav
	            Flt_Stock(mnumcr,IVTYP,ifleet,ILDV,iage,ihav,n) = sum(Flt_Stock(1:mnumcr-2,IVTYP,ifleet,ILDV,iage,ihav,n))
			    LDV_Stock(mnumcr,IVTYP,ifleet+1,ILDV,iage,ihav,n) = sum(LDV_Stock(1:mnumcr-2,IVTYP,ifleet+1,ILDV,iage,ihav,n))
			  enddo
			enddo
		  enddo
		enddo
	  enddo
	endif	

!...Calculate total surviving vehicles, by vehicle, fleet type, and vehicle technology (TFLTECHSTK)
    do IVTYP=1,maxvtyp
      do ifleet=1,maxfleet
        do ILDV=1,maxldv
		  do ihav = 1,maxhav
            TFLTECHSTK(IVTYP,ifleet,ILDV,ihav) = sum(Flt_Stock(mnumcr,IVTYP,ifleet,ILDV,1:maxage,ihav,n))
		  enddo
        enddo
      enddo
    enddo

    do IVTYP=1,maxvtyp
      TOTFLTCAR(IVTYP) = SUM(TFLTECHSTK(IVTYP,1:maxfleet,1:maxldv,1:maxhav))/1000000.0
    enddo

!...EPACT legislative alternative vehicle sales, for table 48
    epactot = sum(fltechsal(mnumcr,1:2,2:3,1:6,3:16,1:4))/1000000.0

  RETURN
  END SUBROUTINE TFLTSTKS

! ==========================================================================================================
! ... Subroutine TLEGIS adjusts vehicle sales and market shares to reflect legislative mandates on sales of 
! ... ZEVs and ULEVs 
! ==========================================================================================================
    SUBROUTINE TLEGIS
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
!...integer variables
	INTEGER it,xy,iiregn
	INTEGER drawdown_yr										!...goal year that ZEV credit bank use ends 
	INTEGER draw_first_yr									!...year that ZEV credit bank starts 


! ... Table for technology penetration and technology cost                        
! ... 
! ... NOTE: These calculations can not be put in FEMCALC because it is not called 
! ... in 1990 and these calculations are necessary for estimating gasoline EV     
! ... hybrid credits for the LEVP.  NOTE 2: FEMCALC is now called for the base    
! ... year, but the calculations are retained here as there is no pressing need   
! ... to move them.                                                               

!	Calculate vehicle group-average technology penetration rate (%) and cost
    DO IGP=1,MAXGROUP
      DO ITECH=1,NUMTECH
        DO ILDV=1,MAXLDV
          MKT_PENF(IGP,ITECH,ILDV) = 0.0
          AVCOST(IGP,ITECH,ILDV) = 0.0
          DO ICL=1,MAXCLASS
            MKT_PENF(IGP,ITECH,ILDV) = MKT_PENF(IGP,ITECH,ILDV) + MKT_PEN(ICL,IGP,ITECH,CURRENT,ILDV) * CLASS_SHARE(mnumcr,ICL,IGP,YRS) * 100.0
            AVCOST(IGP,ITECH,ILDV) = AVCOST(IGP,ITECH,ILDV) + TEC_ORNL(ICL,IGP,ITECH,ILDV) * CLASS_SHARE(mnumcr,ICL,IGP,YRS)
          ENDDO
        ENDDO
      ENDDO
    ENDDO

!	Sum over the manufacturer groups to produce a market penetration rate (%) and average
!	cost (90_) tables, but only for gasoline vehicles.
    do itech=1,numtech
      do it=1,maxvtyp+1
        MKT_D_P(it,itech,yrs)=0.0
        AvgCost(it,itech,yrs)=0.0
      enddo
      do IGP=1,MAXGROUP
        it=GrpMap(IGP)
        MKT_D_P(it,itech,yrs)=MKT_D_P(it,itech,yrs)+(Mkt_Penf(IGP,itech,gas)*SaleShr(mnumcr,IGP,n))
        AvgCost(it,itech,yrs)=AvgCost(it,itech,yrs)+(AvCost(IGP,itech,gas)*SaleShr(mnumcr,IGP,n))
      enddo
      MKT_D_P(3,itech,yrs)=MKT_D_P(1,itech,yrs)*CarShare(mnumcr,n)+MKT_D_P(2,itech,yrs)*(1.0-CarShare(mnumcr,n))
      AvgCost(3,itech,yrs)=AvgCost(1,itech,yrs)*CarShare(mnumcr,n)+AvgCost(2,itech,yrs)*(1.0-CarShare(mnumcr,n))
    end do

!...sum micro hybrid vehicle sales shares
!	update this equation when tech list is updated (80 = 12V micro hybrid)
    do ILDV=1,maxldv
      do it=1,maxvtyp
        micropen(it,ILDV,n)=0.0
      enddo
      do IGP=1,MAXGROUP
        it=GrpMap(IGP)
        micropen(it,ILDV,n)=micropen(it,ILDV,n)+(Mkt_Penf(IGP,80,ILDV)/100*SaleShr(mnumcr,IGP,n))
      enddo
    enddo

!...calculate regional vehicle sales, by technology, within 6 size classes      
!...Note: California LEVP Mandates are not separate by car and l.t., so car and l.t. are   
!...combined to meet sales constraint
	norm_share = 0.
    DO IREGN=1,MNUMCR-2
      DO ILDV=1,MAXLDV
        DO ICL=1,MAXCLASS
          VSALES(1,ICL,IREGN,ILDV) = APSHR44(1,ICL,IREGN,ILDV,N) * NCS(IREGN,ICL,N)     !cars
          VSALES(2,ICL,IREGN,ILDV) = APSHR44(2,ICL,IREGN,ILDV,N) * NLTS(IREGN,ICL,N)    !light trucks
        ENDDO
      ENDDO

!...  normalize regional CVCM sales output to actual regional sales data for HEV, PHEV, EV and FCV
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
      endif !stockyr
    enddo ! mnumcr

	do ivtyp=1,maxvtyp
	  do ildv=1,maxldv
	    norm_share(mnumcr,ivtyp,ildv) = sum(norm_share(1:mnumcr-2,ivtyp,ildv))
	  enddo
	enddo
	
!...apply new regional sales shares to national CVCM output for HEV, PHEV and EV	  
    if(curcalyr.gt.stockyr) then
	  do iregn=1,mnumcr-2
	    do ivtyp=1,maxvtyp
	      do ildv=1,maxldv
!...		normalize shares to 1
		    if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then
		      norm_share(iregn,ivtyp,ildv) = norm_share(iregn,ivtyp,ildv)/norm_share(mnumcr,ivtyp,ildv)
		    endif
            do icl=1,maxclass
!...          for EV100, EV200 and EV300			
	          if(ildv.eq.4.or.ildv.eq.7.or.ildv.eq.15) then 	
!...            new vehicle sales across region
		        norm_vsales(ivtyp,icl,iregn,ildv) = sum(vsales(ivtyp,icl,1:mnumcr-2,ildv)) * norm_share(iregn,ivtyp,ildv)
                adj_vsales(ivtyp,icl,iregn,ildv) = norm_vsales(ivtyp,icl,iregn,ildv) - vsales(ivtyp,icl,iregn,ildv)
!...			adjust sales to account for revisions  
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
        IF(curcalyr.ge.2020) zev_credit_ldv(iregn,ILDV,n)=vsales_t(iregn,ILDV,n)*zev_state_alloc(iregn,n)*zev_multiplier(ILDV,n)
      enddo
    enddo

!	2. Calculate ZEV credits earned and traveling both East S177-to-California and California-to-S177 credits
    do iregn=1,mnumcr-2
      do ILDV=1,maxldv
	  
!		Estimate number of CA-earned credits that travel to other Section 177 states (traveling_CA_credits)
!		and number of non-CA-earned credits that travel to CA (traveling_S177_credits)
        if(curcalyr.ge.2020.and.zev_state_alloc(iregn,n).ne.0.0)then
          if(ILDV.eq.14)then		! Only FCV credits can travel in 2018+
            if(iregn.eq.1)then
              hold_zev_credit_ldv(:)=zev_credit_ldv(:,ILDV,n)
              traveling_S177_credits(ILDV)=dot_product(s177_traveling_factor,hold_zev_credit_ldv) ! East S177 credits travel to CA
              zev_credit_ldv(1:mnumcr-2,ILDV,n)=(1.-s177_traveling_factor(1:mnumcr-2))*zev_credit_ldv(1:mnumcr-2,ILDV,n) ! eliminate the share of originating credits from East S177 because travel to CA
            endif
            CA_credits(ILDV,n)=zev_credit_ldv(9,ILDV,n)*(CA_shr_of9/zev_state_alloc(9,n))+traveling_s177_credits(ILDV)
          endif
        endif
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
	  if(curcalyr.ge.2020.and.zev_state_alloc(iregn,n).gt.0.0)then
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
	  endif
	enddo

	IF(curcalyr.eq.2020) THEN
	  zev_credit_bank(:,:,:) = 0.0	! initialize
	  DO iregn=1,mnumcr-2
	    zev_credit_bank(iregn,1,n) = SUM(zev_basebank(iregn,3:4))	! ATPZEV, PZEV, NEV+		
	    zev_credit_bank(iregn,2,n) = zev_basebank(iregn,2)			! TZEV
	    zev_credit_bank(iregn,3,n) = zev_basebank(iregn,3)			! ZEV (FCV + BEV + BEV+)
	  ENDDO
!	Certain states' regs indicate that manufacturers will receive a credit deposit in the first MY
!	of the regulation (proportional to CA credits/sales). There is often a "max percent of ZEV requirement allowed to be met with these
!	credits" stipulation attached, but we don't model sales by state. So the initial credit deposits 
!	are completed below, but the use of those credits isn't limited later on.
!   Colorado (5 CCR 1001-24)
	ELSEIF(curcalyr.eq.2023) THEN
	  DO izev=1,maxzev
	    zev_credit_bank(8,izev,n) = (CO_shr_of8*(sum(vsales_t(8,1:16,n-4:n-2)) / 3.)) / &
									(CA_shr_of9*(sum(vsales_t(9,1:16,n-4:n-2)) / 3.)) * &
									zev_credit_bank(9,izev,n) + zev_credit_bank(8,izev,n)
	  ENDDO
!   Virginia (10.1-1307.04) and Minnesota (OAH 71-9003-36416)
	ELSEIF(curcalyr.eq.2025) THEN
	  DO izev=1,maxzev
	    zev_credit_bank(5,izev,n) = (VA_shr_of5*(sum(vsales_t(5,1:16,n-4:n-2)) / 3.)) / &
									(CA_shr_of9*(sum(vsales_t(9,1:16,n-4:n-2)) / 3.)) * &
									zev_credit_bank(9,izev,n) + zev_credit_bank(5,izev,n)
		zev_credit_bank(4,izev,n) = (MN_shr_of4*(sum(vsales_t(4,1:16,n-4:n-2)) / 3.)) / &
									(CA_shr_of9*(sum(vsales_t(9,1:16,n-4:n-2)) / 3.)) * &
									zev_credit_bank(9,izev,n) + zev_credit_bank(4,izev,n)
	  ENDDO
	ENDIF
	

!...verify Census Divisions are in compliance with ZEV requirement in 2018+ using credits earned and banking  
!...calculate ZEV compliance 1st, highest order constraint
	do iregn=1,mnumcr-2 !region
		if(curcalyr.ge.2020.and.zev_state_alloc(iregn,n).gt.0.0)then !year
			if(zev_credit_earn(iregn,3,n).ge.(zev_credit_req(iregn,3,n)*(1.0+Bank_buffer(n))))then  !met all reqs?
				annual_credit_balance(iregn,3,n) = zev_credit_earn(iregn,3,n) - zev_credit_req(iregn,3,n)
				zev_credit_bank(iregn,3,n) = zev_credit_bank(iregn,3,n-1) + annual_credit_balance(iregn,3,n)	
			else
				credit_shortfall(iregn,3,n)      =  zev_credit_req(iregn,3,n) - zev_credit_earn(iregn,3,n) ! req only
				credit_shortfall_buff(iregn,3,n) = (zev_credit_req(iregn,3,n)*(1.0+Bank_buffer(n))) - zev_credit_earn(iregn,3,n) ! req + buffer
!...determine bank spending rate: quadratic spending curve based on bank entering 2018, and targeted end year
					!...first year of credit shortfall
					if(credit_shortfall(iregn,3,n-1).le.0.001.and.credit_shortfall(iregn,3,n).gt.0.001.and.curcalyr.lt.2021)then
						draw_first_yr = n
						drawdown_yr = 41 - draw_first_yr		!draw down year 2030 is indexed from first year with shortfall
						c_bank(iregn,3) = 0.3 * credit_shortfall(iregn,3,n)		!set intercept 30% of first year shortfall
						b_bank(iregn,3) = 6 * (zev_credit_bank(iregn,3,n-1)-((2/3)*drawdown_yr*c_bank(iregn,3))) / (drawdown_yr**2)
						a_bank(iregn,3) = -((drawdown_yr*b_bank(iregn,3)) + c_bank(iregn,3)) / (drawdown_yr**2)
					endif
					!...spending rate curve: calculated spending quadratic divided by shortfall
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
								sales_adj_ratio(IVTYP,ICL,iregn,ILDV) = ((sales_adjustment-1.0)*zev_state_alloc(iregn,n)*ZEV_sales_dist(ILDV))+1.0
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
		endif !year
	enddo	!region	

!...Calculate TZEV compliance allowance for 2018+ using credits earned and banking
	do iregn=1,mnumcr-2 !region
	  if(curcalyr.ge.2021.and.zev_state_alloc(iregn,n).gt.0.0)then !year
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
		  if(credit_shortfall(iregn,2,n-1).le.0.001.and.credit_shortfall(iregn,2,n).gt.0.001.and.curcalyr.lt.2022)then
		    draw_first_yr = n
		    drawdown_yr = 41 - draw_first_yr		!draw down year 2030 "n=41" is indexed from first year with shortfall
		    c_bank(iregn,2) = 0.3 * credit_shortfall(iregn,2,n)		!set intercept 30% of first year shortfall
		    b_bank(iregn,2) = 6 * (zev_credit_bank(iregn,2,n-1)-((2/3)*drawdown_yr*c_bank(iregn,2))) / (drawdown_yr**2)
		    a_bank(iregn,2) = -((drawdown_yr*b_bank(iregn,2)) + c_bank(iregn,2)) / (drawdown_yr**2)
		  endif
		  !...spending rate curve: calculated spending quadratic divided by shortfall
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
		    	  	sales_adj_ratio(IVTYP,ICL,iregn,ILDV) = ((sales_adjustment-1.0)*zev_state_alloc(iregn,n)*ZEV_sales_dist(ILDV))+1.0
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
		endif !shortfall
	  endif !year
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
!													   
!	      ENDDO
!		ENDDO
!	  ENDDO
!	ENDIF

!...  Total adjusted vehicle sales
      DO IVTYP=1,MAXVTYP
        DO ICL=1,MAXCLASS
          DO ILDV=1,MAXLDV
            AVSALES(IVTYP,ICL,MNUMCR,ILDV) = 0.0
            DO IREGN=1,MNUMCR-2
              AVSALES(IVTYP,ICL,MNUMCR,ILDV) = AVSALES(IVTYP,ICL,MNUMCR,ILDV) + &
                                               AVSALES(IVTYP,ICL,IREGN,ILDV)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      avsalest=0.
      DO IVTYP=1,MAXVTYP
        DO IREGN=1,MNUMCR
          DO ICL=1,MAXCLASS
            AVSALEST(IVTYP,ICL,IREGN) = sum(AVSALES(IVTYP,ICL,IREGN,1:maxldv))
          ENDDO
        ENDDO
      ENDDO

! ... calculate new absolute market shares for each vehicle technology            
! ... Note: APSHR55 is necessary to divide total vehicle sales back into car and  
! ... l.t. separately after the total vehicle sales have been adjusted to the     
! ... mandates                                                                    
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

! ... reset new car and lt truck sales using market shares, mapped from 3 to 6
! ... size classes                                                                
      NCSTECH = 0.0
      NLTECH  = 0.0
      DO IREGN=1,MNUMCR-2
        DO ICL=1,MAXCLASS
          DO ILDV=1,MAXLDV
            NCSTECH(IREGN,ICL,ILDV,N) = NCS(IREGN,ICL,N)  * APSHR55(1,ICL,IREGN,ILDV)
            NLTECH(IREGN,ICL,ILDV,N)  = NLTS(IREGN,ICL,N) * APSHR55(2,ICL,IREGN,ILDV)
          ENDDO
        ENDDO
      ENDDO

! ... Sum NCSTECH and NLTECH across regions
      DO ICL=1,MAXCLASS
        DO ILDV=1,MAXLDV
          NCSTECH(mnumcr,ICL,ILDV,N) = 0.0
          NLTECH(mnumcr,ICL,ILDV,N)  = 0.0
          NCSTECH(mnumcr,ICL,ILDV,N) = sum(NCSTECH(1:mnumcr-2,ICL,ILDV,N))
          NLTECH(mnumcr,ICL,ILDV,N)  = sum(NLTECH(1:mnumcr-2,ICL,ILDV,N))
        ENDDO
      ENDDO

!...Calculations for the macroeconomic sector investment project.
!...Calculate fleet vehicle sales by size class and ldv type
    FLTSALSC = 0.0
    do IVTYP=1,maxvtyp
      do ICL=1,MAXCLASS
        do ILDV=1,maxldv
          FLTSALSC(IVTYP,ICL,ILDV,n)=sum(FLTECHSAL(mnumcr,IVTYP,1:maxfleet,ICL,ILDV,1:maxhav))
        enddo
      enddo
    enddo

!...Calculate total personal vehicle sales and fleet vehicle sales by size class and fueling technology
    do IVTYP=1,maxvtyp
      do ICL=1,MAXCLASS
        do ILDV=1,maxldv
          TOTALSALSC(IVTYP,ICL,ILDV,n) = 0.0
          if(IVTYP.eq.1) then
            TOTALSALSC(IVTYP,ICL,ILDV,n) = NCSTECH(mnumcr,ICL,ILDV,n)+(FLTSALSC(IVTYP,ICL,ILDV,n)/1000000.0)
          else
            TOTALSALSC(IVTYP,ICL,ILDV,n) = NLTECH(mnumcr,ICL,ILDV,n) +(FLTSALSC(IVTYP,ICL,ILDV,n)/1000000.0)
          endif
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
  END SUBROUTINE TLEGIS

! ==========================================================================================================
! ... Subroutine TFLTVMTS calculates VMT for fleets
! ==========================================================================================================
  SUBROUTINE TFLTVMTS
  USE T_
  IMPLICIT NONE

!...Total VMT by vehicle type and technology
    do IVTYP=1,maxvtyp
      do ifleet=1,maxfleet
        do ILDV=1,maxldv
		  do ihav=1, maxhav
             FLTVMTECH(IVTYP,ifleet,ILDV,ihav) = TFLTECHSTK(IVTYP,ifleet,ILDV,ihav)*FLTVMTYR(ifleet,n,IVTYP)*(1.-flt_covid(ifleet,n))
		  enddo
        enddo
      enddo
    enddo
  RETURN
  END SUBROUTINE TFLTVMTS

! ==========================================================================================================
! ... Subroutine CAFECALC checks whether fuel economy of all vehicles meets CAFE standards 
! ==========================================================================================================
  SUBROUTINE CAFECALC
  USE T_
  IMPLICIT NONE


    integer   it,ICafePass(MAXGROUP),L
    real      CafeNeedX(MAXGROUP,mnumyr)
    real      Mpgx(maxvtyp,maxldv,MAXCLASS,mnumyr) !adjustment to make mpg equivalent equal to mpg

!...Combine fuel economies from all vehicles (gasoline + ATVs)
!...This calculates CAFE for all the vehicles in each group. (So we have a different CAFE for each group 
!... - this is an important distinction - previously the model only estimated CAFE for vehicle type
!...Two MPG are calculated, MpgWgt is the miles per gallon without AFV credits for flex fuel vehicles,
!...FFMpgWgt is the miles per gallon with credits given for flex fuel vehicles.  These extra credits are phased out by 2019
    do iregn = 1,mnumcr
	    do IGP=1,MAXGROUP
          GrpWgt=0.0
          MpgWgt=0.0
          CafeMpgWgt = 0.0
          DedAFVMpgWgt = 0.0
          it=GrpMap(IGP)
          do ILDV=1,MAXLDV
            do ICL=1,MAXCLASS
			
              if(LDV_MPG_CL(it,ILDV,ICL,yrs).ne.0.0) then
!............Calculate extra credits available for flex fuel and dedicated alternative fuel vehicles.  Dedicated alternative
!............fuel vehicles have their MPG divided by 0.15.  Flex fuel vehicles are assumed to spend 50% of their time on gasoline
!............and 50% on alternative fuel that has its mpg divided by 0.15.  The credits for dedicated fuel vehicles are unlimited 
!............and last forever.  The credits for flex fuel vehicles are limited each year and are phased out.
                if(curcalyr.gt.2010) then 
!...              flex fuel vehicles
				  if(ILDV.eq.3) then
                    GrpWgt = GrpWgt+class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV)
                    MpgWgt = MpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs))
                    if(curcalyr.le.2019)then
					  CafeMpgWgt = CafeMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs)/0.605)
					else
					  CafeMpgWgt = CafeMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs)/0.8894) 
					endif
                    DedAFVMpgWgt = DedAFVMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs))
!...		      Electric vehicles
				  elseif((ILDV.eq.4).or.(ILDV.eq.7).or.(ILDV.eq.15)) then
				    GrpWgt = GrpWgt+class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV)
                    MpgWgt = MpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs))
                    CafeMpgWgt = CafeMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs)/0.295)
                    DedAFVMpgWgt = DedAFVMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs)/0.295)
!...              PHEVs
				  elseif((ILDV.eq.5).or.(ILDV.eq.6)) then
				    GrpWgt = GrpWgt+class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV)
                    MpgWgt = MpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs))
                    CafeMpgWgt = CafeMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs)/0.53)
                    DedAFVMpgWgt = DedAFVMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs)/0.53)
!...              Fuel Cell vehicles
				  elseif(ILDV.eq.14) then
				    GrpWgt = GrpWgt+class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV)
                    MpgWgt = MpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs))
                    CafeMpgWgt = CafeMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs)/0.74)
                    DedAFVMpgWgt = DedAFVMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs)/0.74)
!...              all other vehicles
                  else
                    GrpWgt = GrpWgt+class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV)
                    MpgWgt = MpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/LDV_MPG_CL(it,ILDV,ICL,yrs)
                    CafeMpgWgt = CafeMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/LDV_MPG_CL(it,ILDV,ICL,yrs)
                    DedAFVMpgWgt = DedAFVMpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/(LDV_MPG_CL(it,ILDV,ICL,yrs))
                  endif
                else
                  GrpWgt = GrpWgt+class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV)
                  MpgWgt = MpgWgt+(class_share(iregn,ICL,IGP,yrs)*apshr55(it,ICL,iregn,ILDV))/LDV_MPG_CL(it,ILDV,ICL,yrs)
                  CafeMpgWgt = MpgWgt
                  DedAFVMpgWgt = MpgWgt
                endif   
              endif
            enddo
          enddo
!.....The credits that can be gained by flex fuel vehicles is limited and is phased out by 2019
          if(MpgWgt.ne.0.0.and.CafeMpgWgt.gt.0.0.and.DedAFVMpgWgt.gt.0.0) then
            if(yrs.le.2010) then 
              maxFFcredit = 0.0
            elseif(yrs.le.2014) then
              maxFFcredit = 1.2
            elseif(yrs.eq.2015) then
              maxFFcredit = 1.0
            elseif(yrs.eq.2016) then
              maxFFcredit = 0.8
            elseif(yrs .eq. 2017) then
              maxFFcredit = 0.6
            elseif(yrs .eq. 2018) then
              maxFFcredit = 0.4
            elseif(yrs .eq. 2019) then
              maxFFcredit = 0.2
            endif
            if(iregn.ne.10) TrueMpgGrp(iregn,IGP,n) = GrpWgt/MpgWgt
            if(iregn.eq.11) then
			  if(curcalyr.le.2019) then
			    CafeMpgGrp(IGP,n) = min(GrpWgt/CafeMpgWgt, GrpWgt/DedAFVMpgWgt + maxFFcredit)
			  else
			    CafeMpgGrp(IGP,n) = GrpWgt/CafeMpgWgt
			  endif
			endif
          else
		    TrueMpgGrp(iregn,IGP,n) = 0.0
		    CafeMpgGrp(IGP,n) = 0.0
		  endif

!...    calculate AC and off cycle fuel economy credits
          if(curcalyr.ge.2020) then
            CafeMpgGrp(igp,n) = ((1/CafeMpgGrp(igp,n)) - ac_oc_credit(igp,n))**-1
		  endif
        enddo 
!...  calculate average new tested mpg by region
      TrueMPG_regn(iregn,1,n) = 0.0 
	  TrueMPG_regn(iregn,2,n) = 0.0
      do IGP = 1,cargrp
	    if(truempggrp(iregn,IGP,n).ne.0) TrueMPG_regn(iregn,1,n) = TrueMPG_regn(iregn,1,n) + saleshr(iregn,IGP,n)/truempggrp(iregn,IGP,n)
      enddo
	  do IGP = cargrp+1,MAXGROUP
	    if(truempggrp(iregn,IGP,n).ne.0) TrueMPG_regn(iregn,2,n) = TrueMPG_regn(iregn,2,n) + saleshr(iregn,IGP,n)/truempggrp(iregn,IGP,n)
	  enddo
	  TrueMPG_regn(iregn,1,n) = 1.0/TrueMPG_regn(iregn,1,n)
	  TrueMPG_regn(iregn,2,n) = 1.0/TrueMPG_regn(iregn,2,n)	
    enddo
	
!...calculate average new tested mpg 
    TrueMPG(1,n) = 0.0 
	TrueMPG(2,n) = 0.0
    do IGP = 1,cargrp
	  if(truempggrp(mnumcr,IGP,n).ne.0) TrueMPG(1,n) = TrueMPG(1,n) + saleshr(mnumcr,IGP,n)/truempggrp(mnumcr,IGP,n)
    enddo
	do IGP = cargrp+1,MAXGROUP
	  if(truempggrp(mnumcr,IGP,n).ne.0) TrueMPG(2,n) = TrueMPG(2,n) + saleshr(mnumcr,IGP,n)/truempggrp(mnumcr,IGP,n)
	enddo
	TrueMPG(1,n) = 1.0/TrueMPG(1,n)
	TrueMPG(2,n) = 1.0/TrueMPG(2,n)	
            
!...If the year is 2008 or later, calculate the alternative CAFE standard for each group of
!...cars and light trucks based upon the footprint of each of the classes in that group
    if(curcalyr.ge.2010) then 
      do IGP=1,MAXGROUP
        it=GrpMap(IGP)
        GrpWgt=0.0
        MpgWgt=0.0
        do ICL=1,MAXCLASS		
		  FPMpg(ICL,IGP,n) = 0.0
		  if(fprint(ICL,IGP,n).ne.0.0)  then 
!...Determine the mpg standard for each class in each group based upon the footprint.
!...      cars - NHTSA standards
          if(it.eq.1)then
            FPMpg(ICL,IGP,n)=1.0/((1.0/CFCoefA(n))+(1.0/CFCoefB(n)-1.0/CFCoefA(n))* &
                             (exp((FPrint(ICL,IGP,n)-CFCoefC(n))/CFCoefD(n))/ &
                             (1.0+exp((FPrint(ICL,IGP,n)-CFCoefC(n))/CFCoefD(n)))))
!...        GHG standards
            if(curcalyr.ge.2012)then 
              FPMpg(ICL,IGP,n)=1.0/(min(max(((CFCoefC2(n)*FPrint(ICL,IGP,n))+CFCoefD2(n)),1.0/CFCoefA2(n)),1.0/CFCoefB2(n)))
            endif
!...      light trucks - NHTSA standards
          else 
            FPMpg(ICL,IGP,n)=1.0/((1.0/TFCoefA(n))+(1.0/TFCoefB(n)-1.0/TFCoefA(n))* &
                             (exp((FPrint(ICL,IGP,n)-TFCoefC(n))/TFCoefD(n))/ &
                             (1.0+exp((FPrint(ICL,IGP,n)-TFCoefC(n))/TFCoefD(n)))))
!...        GHG standards
            if(curcalyr.ge.2012)then     
              FPMpg(ICL,IGP,n)=1.0/(min(max(((TFCoefC2(n)*FPrint(ICL,IGP,n))+TFCoefD2(n)),1.0/TFCoefA2(n)),1.0/TFCoefB2(n)))
            endif
			endif
          endif
!...Calculate the mpg standard for the group, weighted up over the classes.
          GrpWgt=GrpWgt+class_share(mnumcr,ICL,IGP,yrs)
          if(FPMpg(ICL,IGP,n).gt.0.0) MpgWgt=MpgWgt+class_share(mnumcr,ICL,IGP,yrs)/FPMpg(ICL,IGP,n)
          if(fcrl.eq.1.and.curcalyr.eq.XYR) then 
            write(H2UNIT,'(a,4i5,6f8.2)') '**ALT1 ',curiyr+1989,pass,IGP,ICL,FPMpg(ICL,IGP,n),CFCoefA(n),CFCoefB(n),CFCoefC(n),CFCoefD(n),FPrint(ICL,IGP,n)
          endif
        end do
        if(MpgWgt.ne.0.0) then
		    FPMpgGrp(IGP,n)=GrpWgt/MpgWgt
		else
		    FPMpgGrp(IGP,n) = 0.0
		endif
        if(fcrl.eq.1.and.curcalyr.eq.XYR) then 
          write(H2UNIT,'(a,3i5,3f8.2)') '**ALT2 ',curiyr+1989,pass,IGP,FPMpgGrp(IGP,n),GrpWgt,MpgWgt
        endif
      enddo
    endif

!...Decide on the standard to be used.  Cars will use the traditional standard before 2011 and the foot print standard after.
!...Light trucks will use the traditional standard before 2008, choose between 2008 and 2011, and use the foot print standard
!...after 2011. If the foot print standard is chosen between 2008 and 2011 then the group must continue to use it in later years.
    do IGP=1,MAXGROUP
      it=GrpMap(IGP)
!...  cars
      if(it.eq.1) then
        Cafe_Used(IGP,yrs)=Cafe_Stand(IGP,yrs)
        if(curcalyr.ge.2011)then
          Cafe_Used(IGP,yrs)=FPMpgGrp(IGP,n)
          if(yrs.ge.2017)then
            if(Cafe_Used(IGP,yrs).lt.Cafe_Used(IGP,yrs-1)) Cafe_Used(IGP,yrs) = Cafe_Used(IGP,yrs-1)
          endif          
        endif
!...  light trucks
      else
        if(yrs.le.2010) then
          Cafe_Used(IGP,yrs)=Cafe_Stand(IGP,yrs)
          AltCafe(IGP)=0
        endif
        if(yrs.ge.2011)then
          Cafe_Used(IGP,yrs)=FPMpgGrp(IGP,n)
          if(yrs.ge.2017)then
            if(Cafe_Used(IGP,yrs).lt.Cafe_Used(IGP,yrs-1)) Cafe_Used(IGP,yrs) = Cafe_Used(IGP,yrs-1)
          endif
        endif
      endif
    end do

!...calculate cafe standard for fleet
    cafestd(1,n) = 0.0 
	cafestd(2,n) = 0.0
    do IGP = 1,cargrp
	  if(cafe_used(IGP,yrs).ne.0) cafestd(1,n) = cafestd(1,n) + saleshr(mnumcr,IGP,n)/cafe_used(IGP,yrs)
    enddo
	do IGP = cargrp+1,MAXGROUP
	  if(cafe_used(IGP,yrs).ne.0) cafestd(2,n) = cafestd(2,n) + saleshr(mnumcr,IGP,n)/cafe_used(IGP,yrs)
	enddo
	if(cafestd(1,n).gt.0.0) cafestd(1,n) = 1.0/cafestd(1,n)
	if(cafestd(2,n).gt.0.0) cafestd(2,n) = 1.0/cafestd(2,n)	

!...Write out the base cafe standard, the alt cafe standard, the used cafe standard, the cafe - for each group:
    do IGP=1,MAXGROUP
      if(fcrl.eq.1) then
        write(H2UNIT,'(a,3i5,4f10.3)') '**Cfe ',curiyr+1989,pass,IGP,Cafe_Stand(IGP,yrs),FPMpgGrp(IGP,n),Cafe_Used(IGP,yrs),CafeMpgGrp(IGP,n)
      endif
    end do

!...Check individual group CAFE against the standard.
    do IGP=1,MAXGROUP
      cafepass(IGP)= .true.
      if(CafeMpgGrp(IGP,n).lt.Cafe_Used(IGP,yrs)) cafepass(IGP)= .false.
    end do

!...Write out some debug (calculate an integer indicating whether pass or fail...)
    do IGP=1,MAXGROUP
      ICafePass(IGP)=0
      if(CafePass(IGP).eq. .true.) ICafePass(IGP)=1
    end do
    if(fcrl.eq.1) then
      write(H2UNIT,'(a,2i5,11f10.2)') '**MpgGrp ',curiyr+1989,pass,(CafeMpgGrp(IGP,n),IGP=1,MAXGROUP)
      write(H2UNIT,'(a,2i5,11f10.2)') '**MpgStd ',curiyr+1989,pass,(Cafe_Used(IGP,yrs),IGP=1,MAXGROUP)
      write(H2UNIT,'(a,2i5,11i10)') '**MpgPas ',curiyr+1989,pass,(ICafePass(IGP),IGP=1,MAXGROUP)
    endif

!...Code for doing the CAFE banking. For current testing IBank=0 does not do banking, IBank=1 does banking.

    if(IBank.eq.1.and.curcalyr.ge.XYR-6) then
!...Go through each of the manufacturing groups.
      do IGP=1,MAXGROUP
!...    On first pass and first iteration, establish the starting values for this year from from last year's
!...    working values and from last year's new bank.
        if(pass.eq.1.and.curitr.eq.1) then
          CafeBank(5,IGP) = CafeWork(4,IGP)
          CafeBank(4,IGP) = CafeWork(3,IGP)
          CafeBank(3,IGP) = CafeWork(2,IGP)
          CafeBank(2,IGP) = CafeWork(1,IGP)
          CafeBank(1,IGP) = CafeBankA(IGP)
          CafeBankA(IGP)  = 0.0
          write(H2UNIT,'(a,4i5,5f8.3)') '**BnkInt ',curiyr+1989,curitr,pass,IGP,CafeBank(1,IGP),CafeBank(2,IGP),CafeBank(3,IGP),CafeBank(4,IGP),CafeBank(5,IGP)
        endif
!...    On first pass and every iteration, put the saved values into working values. This is necessary
!...    so that we have the fresh bank values at the start of each new iteration.
        if(pass.eq.1) then
          CafeWork(5,IGP) = CafeBank(5,IGP)
          CafeWork(4,IGP) = CafeBank(4,IGP)
          CafeWork(3,IGP) = CafeBank(3,IGP)
          CafeWork(2,IGP) = CafeBank(2,IGP)
          CafeWork(1,IGP) = CafeBank(1,IGP)
!...    On the first pass, if the group passed, then bank its excess MPG. Otherwise, pull values out of the bank.
          if(CafePass(IGP).eq. .true.) then
            CafeBankA(IGP) = CafeMpgGrp(IGP,n)-Cafe_Used(IGP,yrs)
            write(H2UNIT,'(a,4i5,f8.3)') '**BnkPas ',curiyr+1989,curitr,pass,IGP,CafeBankA(IGP)
          else
!...    Get the total amount by which the group did not pass the cafe standard.
            CafeNeed = Cafe_Used(IGP,yrs)-CafeMpgGrp(IGP,n)
            CafeNeedX(IGP,n) = CafeNeed
!...    Work backwards through the bank and see if we can make up the difference.
            do i=5,1,-1
              if(CafeNeed.gt.0.0) then
                if(CafeNeed.le.CafeWork(i,IGP)) then
                  CafeWork(i,IGP) = CafeWork(i,IGP)-CafeNeed
                  CafeNeed = 0.0
                  CafePass(IGP) = .true.
                else
                  CafeNeed = CafeNeed-CafeWork(i,IGP)
                  CafeWork(i,IGP) = 0.0
                endif
              endif
            enddo
            if(CafeNeed.eq.0.0)then
              CafePass(IGP)=.true.
              CafeMpgGrp(IGP,n) = Cafe_Used(IGP,yrs)
            endif

            write(H2UNIT,'(a,4i5,7f8.3,i8)') '**BnkFal ',curiyr+1989,curitr,pass,IGP,CafeNeedX(IGP,n),CafeWork(1,IGP),CafeWork(2,IGP),CafeWork(3,IGP),&
                                                         CafeWork(4,IGP),CafeWork(5,IGP),CafeNeed,CafePass(IGP)
          endif
        endif
      enddo
    endif
    
    do IGP=1,MAXGROUP
      bankbal(IGP,yrs)=sum(CafeBank(1:5,IGP))
    enddo

!...calculate average new tested mpg with AFV credits and banking
    NewMPG(1,n) = 0.0 
	NewMPG(2,n) = 0.0
    do IGP = 1,cargrp
	  if(cafempggrp(IGP,n).ne.0) NewMPG(1,n) = NewMPG(1,n) + saleshr(mnumcr,IGP,n)/cafempggrp(IGP,n)
    enddo
	do IGP = cargrp+1,MAXGROUP
	  if(cafempggrp(IGP,n).ne.0) NewMPG(2,n) = NewMPG(2,n) + saleshr(mnumcr,IGP,n)/cafempggrp(IGP,n)
	enddo
	NewMPG(1,n) = 1.0/NewMPG(1,n)
	NewMPG(2,n) = 1.0/NewMPG(2,n)	

  RETURN
  END SUBROUTINE CAFECALC

! ==========================================================================================================
! ... Subroutine CAFETEST determines the least cost, alternatively fueled vehicle that could meet CAFE   
! ... standards by size class and incrementally increases sales of these vehicles to meet CAFE shortfall
! ==========================================================================================================
  SUBROUTINE CAFETEST
  USE T_
  IMPLICIT NONE

!...local variable dictionary
!...integer variables 
  INTEGER :: l,xldv,xcl,ycl,xpass,xgp,stop_gas,stop_ffv		!...local looping, array, and conditional variables			
  INTEGER :: salesflag(maxvtyp,maxldv,MAXCLASS,mnumyr)		!...indicates vehicle is available 
  INTEGER cafeveh(5) /5,6,7,15,16/					        !...ILDV fuel types available for CAFETEST compliance
  INTEGER carcls(8) /4,3,7,2,5,8,6,1/						!...passenger car size classes
  INTEGER trkcls(8) /7,8,5,6,4,3,1,2/						!...light-duty truck size classes 
!...real variables
  REAL :: delta_fuel_$(maxvtyp,maxldv,MAXCLASS,mnumyr)		!...fuel savings by fuel type compared to conventional gasoline
  REAL :: delta_price(maxvtyp,maxldv,MAXCLASS,mnumyr)		!...incremental ILDV vehicle price compared to convetional gasoline
  REAL :: factor_$(maxvtyp,maxldv,MAXCLASS,mnumyr)			!...fuel economics cost effectiveness (fuel savings-incremental vehicle cost)
  REAL :: asort(5)                                          !...sort by cost effectiveness
  REAL :: isort(5)                                          !...sort by order
  REAL :: xsort(5,maxvtyp,MAXCLASS,mnumyr)					!...saved sort order 
  REAL :: mpg_grp(MAXGROUP,maxldv,MAXCLASS,mnumyr)			!...fuel economy by manufacturer
  REAL :: mpgx_grp(MAXGROUP,maxldv,MAXCLASS,mnumyr)			!...fuel economy dedicated afv by manufacturer 
  REAL :: delta(MAXGROUP,MAXCLASS)							!...maximum sales change per pass(loop)
  REAL :: apshr55_reg(mnumcr,maxldv)						!...sales shares by region
  REAL :: TrueMpgGrp2(MAXGROUP,mnumyr)
  REAL :: CafeMpgGrp2(MAXGROUP,mnumyr)
!...logical variables
  LOGICAL*1 :: CafePass2(MAXGROUP)
!...parameters
  INTEGER, PARAMETER :: MAXVEH = 5							!...ILDV fuel types availble for CAFETEST compliance
  INTEGER, PARAMETER :: PAYB = 5							!...years of payback for fuel economics calculation
  INTEGER, PARAMETER :: MAXPASS = 20						!...number of loop passes for adding vehicle sales

!...save initial vehicle sales by manufacturer
	do IGP=1,MAXGROUP
		IVTYP=grpmap(IGP)
		do ICL=1,MAXCLASS
			do ILDV=1,maxldv
				avsales_old(IGP,ICL,ILDV,n) = avsales(IVTYP,ICL,mnumcr,ILDV) * PerGrp(IGP,ICL,n)
!...            fill revised sales array for later calculations
				avsales_new(IGP,ICL,ILDV,n)=avsales_old(IGP,ICL,ILDV,n)
			enddo
!...        sum manufacturer sales by size class
			avsales_ttl(IGP,ICL,n) = sum(avsales_old(IGP,ICL,1:maxldv,n))
		enddo
	enddo
    
!...calculate cost effectiveness of advanced technology vehicles relative to conventional gasoline
!...calculate discounted fuel savings
	delta_fuel_$=0.
	do IVTYP=1,maxvtyp
		do l=1,maxveh
        xldv=cafeveh(l)
			do ICL=1,MAXCLASS
				salesflag(IVTYP,xldv,ICL,n)=0
				factor_$(IVTYP,xldv,ICL,n)=-50000.
				if(LDV_MPG_CL(IVTYP,xldv,ICL,yrs).ne.0.)then
					salesflag(IVTYP,xldv,ICL,n)=1
					do i=1,payb
						if(IVTYP.eq.1) VMT(i,mnumcr)=PVMT(i,STOCKYR-1989,mnumcr,1)	! Use the last available VMT schedule available (STOCKYR), and use ICE VMT for consistent assumption.
						if(IVTYP.eq.2) VMT(i,mnumcr)=LVMT(i,STOCKYR-1989,mnumcr,1)
						fuel_n = n+i-1
						if(fuel_n.gt.61) fuel_n=61
						Fuel_$(i) = PMGTR(11,fuel_n) * TMC_PGDP(1) * MG_HHV/1000.0	
						delta_fuel_$(IVTYP,xldv,ICL,n) = delta_fuel_$(IVTYP,xldv,ICL,n) + &
														 ((VMT(i,mnumcr)*(1/LDV_MPG_CL(IVTYP,1,ICL,yrs) - &
														 1./LDV_MPG_CL(IVTYP,xldv,ICL,yrs))*Fuel_$(i)) * &
														 ((1.07)**(-i)))															 
					enddo
				endif
!...            calculate incremental vehicle price
				delta_price(IVTYP,xldv,ICL,n) = LDV_PRI(IVTYP,xldv,ICL,yrs)-LDV_PRI(IVTYP,1,ICL,yrs)
!...            calculate fuel economics cost effectiveness factor
				factor_$(IVTYP,xldv,ICL,n) = delta_fuel_$(IVTYP,xldv,ICL,n) - delta_price(IVTYP,xldv,ICL,n)
			enddo
		enddo
	enddo

!...sort factor_$ by most cost effective
	do IVTYP=1,maxvtyp
		do ICL=1,MAXCLASS    
			do l=1,maxveh
				xldv=cafeveh(l)
				asort(l) = -1.0 * factor_$(IVTYP,xldv,ICL,n)
				isort(l)=cafeveh(l)
			enddo
			call RSORT(asort,isort,maxveh,maxveh)
			do l=1,maxveh
				xsort(l,IVTYP,ICL,n)= isort(l)
			enddo
		enddo
	enddo

!...set maximum allowable increase in alternatively fueled vehicle sales by manufacturer
	do IGP=1,MAXGROUP
		do ICL=1,MAXCLASS
			delta(IGP,ICL) = 0.05*0.25 * class_share(mnumcr,ICL,IGP,yrs) * saleshr(mnumcr,IGP,n)
		enddo
	enddo

!...calculate initial share of ILDV by mfg by class and assign fuel economy to mfgs
    do IGP=1,MAXGROUP
      IVTYP=grpmap(IGP)
      do ILDV=1,maxldv
        do ICL=1,MAXCLASS
		apshr55_grp(IGP,ILDV,ICL,n) = 0.0
          if(avsales_ttl(IGP,ICL,n).ne.0.)then
            apshr55_grp(IGP,ILDV,ICL,n)=avsales_new(IGP,ICL,ILDV,n)/avsales_ttl(IGP,ICL,n)
          endif
          Mpg_grp(IGP,ILDV,ICL,n)=0.
          if(avsales_old(IGP,ICL,ILDV,n).gt.0.)then
            Mpg_grp(IGP,ILDV,ICL,n)=LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)
          endif
        enddo
      enddo
    enddo

!...set CAFE pass indicator
	do IGP=1,MAXGROUP
		if(CafePass(IGP).eq. .true.)then
			CafePass2(IGP) = .true.
		else
			CafePass2(IGP) = .false.
		endif
	enddo
	CafePass2(5) = .true.			! luxury doesn't need to pass CAFE

!...increase sales of alternative fuel vehicles in order of cost effectiveness to meet CAFE standard
	do IGP=1,MAXGROUP
		if(CafePass2(IGP).eq..true.)cycle
		IVTYP=grpmap(IGP)
		do xcl=1,MAXCLASS
			if(CafePass2(IGP).eq..true.)exit
			if(IVTYP.eq.1) ycl=carcls(xcl)
			if(IVTYP.eq.2) ycl=trkcls(xcl)
			if(avsales_old(IGP,ycl,1,n).gt.0.)then
			do l=1,maxveh
				if(CafePass2(IGP).eq..true.)exit
				xldv=xsort(l,IVTYP,ycl,n)
!...            ensure alternative fuel vehicle and class sales exist for manufacturer and class
				if(salesflag(IVTYP,xldv,ycl,n).eq.1)then
!...                add alternative fuel vehicle sales until CAFE is met
					stop_gas=0
					stop_ffv=0
!...                incrementally add alternative fuel vehicle sales to meet CAFE
					do xpass=1,maxpass
!...                    subtract delta alternative vehicle sales from initial gasoline vehicle sales
!...                    add delta alternative vehicle sales to inital advanced vehicle sales                
						if(stop_gas.eq.0)then
							avsales_new(IGP,ycl,1,n) = avsales_new(IGP,ycl,1,n) - delta(IGP,ycl)
							avsales_new(IGP,ycl,xldv,n) = avsales_new(IGP,ycl,xldv,n) + delta(IGP,ycl) 
!...                        stop if conventional gasoline vehicle sales go to zero or negative                
							if(avsales_new(IGP,ycl,1,n).le.0.)then
								if(stop_gas.eq.0)then
									avsales_new(IGP,ycl,1,n) = avsales_new(IGP,ycl,1,n) + delta(IGP,ycl)
									avsales_new(IGP,ycl,xldv,n) = avsales_new(IGP,ycl,xldv,n) - delta(IGP,ycl)
								endif
								stop_gas=1
							endif
						endif
!...                    subtract delta alternative fuel vehicle sales from FFV if no more gasoline
						if(stop_ffv.eq.0 .and. stop_gas.eq.1)then
							avsales_new(IGP,ycl,3,n) = avsales_new(IGP,ycl,3,n) - delta(IGP,ycl)
							avsales_new(IGP,ycl,xldv,n) = avsales_new(IGP,ycl,xldv,n) + delta(IGP,ycl)
!...                        stop if ffv sales go to zero or negative
							if(avsales_new(IGP,ycl,3,n).le.0.)then
								if(stop_ffv.eq.0)then
									avsales_new(IGP,ycl,3,n) = avsales_new(IGP,ycl,3,n) + delta(IGP,ycl)
									avsales_new(IGP,ycl,xldv,n) = avsales_new(IGP,ycl,xldv,n) - delta(IGP,ycl)
								endif
								stop_ffv=1
							endif
						endif

!...                    for each xpass calculate new fuel economy.  Exit if standard is met.
!...                    recalculate market penetration by ILDV by class for each manufacturer
						do ILDV=1,maxldv
							do ICL=1,MAXCLASS
								apshr55_grp(IGP,ILDV,ICL,n)=0.
								if(avsales_ttl(IGP,ycl,n).ne.0.)then                    
									apshr55_grp(IGP,ILDV,ICL,n) = avsales_new(IGP,ICL,ILDV,n) / avsales_ttl(IGP,ICL,n)
								endif
							enddo
						enddo

!...                    calculate revised CAFE given alternatively fueled vehicle sales adjustment
						GrpWgt=0.
						MpgWgt=0.
						CafeMpgWgt=0.
						DedAFVMpgWgt=0.
						do ILDV=1,maxldv
							do ICL=1,MAXCLASS
								if(Mpg_grp(IGP,ILDV,ICL,n).ne.0.)then
									if(ILDV.eq.3)then
										GrpWgt = GrpWgt + class_share(mnumcr,ICL,IGP,yrs) * apshr55_grp(IGP,ILDV,ICL,n)
										MpgWgt = MpgWgt + (class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n)) / Mpg_grp(IGP,ILDV,ICL,n)
										CafeMpgWgt = CafeMpgWgt + (class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n)) / (Mpg_grp(IGP,ILDV,ICL,n)/0.8894)
										DedAFVMpgWgt = DedAFVMpgWgt + (class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n)) / (Mpg_grp(IGP,ILDV,ICL,n))
!...								EVs
									elseif((ILDV.eq.4).or.(ILDV.eq.7).or.(ILDV.eq.15)) then
										GrpWgt = GrpWgt+class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n)
										MpgWgt = MpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n))/(MPG_grp(IGP,ILDV,ICL,n))
										CafeMpgWgt = CafeMpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n))/(MPG_grp(IGP,ILDV,ICL,n)/0.295)
										DedAFVMpgWgt = DedAFVMpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n))/(MPG_grp(IGP,ILDV,ICL,n)/0.295)		
!...                                PHEVs
				                    elseif((ILDV.eq.5).or.(ILDV.eq.6)) then
				                        GrpWgt = GrpWgt+class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(igp,ildv,icl,n)
                                        MpgWgt = MpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(igp,ildv,icl,n))/(MPG_grp(igp,ILDV,ICL,n))
                                        CafeMpgWgt = CafeMpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(igp,ildv,icl,n))/(MPG_grp(igp,ILDV,ICL,n)/0.53)
                                        DedAFVMpgWgt = DedAFVMpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(igp,ildv,icl,n))/(MPG_grp(igp,ILDV,ICL,n)/0.53)    
!...                                FCVs
				                    elseif(ILDV.eq.14) then
				                        GrpWgt = GrpWgt+class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(igp,ildv,icl,n)
                                        MpgWgt = MpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(igp,ildv,icl,n))/(MPG_grp(igp,ILDV,ICL,n))
                                        CafeMpgWgt = CafeMpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(igp,ildv,icl,n))/(MPG_grp(igp,ILDV,ICL,n)/0.74)
                                        DedAFVMpgWgt = DedAFVMpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(igp,ildv,icl,n))/(MPG_grp(igp,ILDV,ICL,n)/0.74) 										
									else
										GrpWgt = GrpWgt+class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n)
										MpgWgt = MpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n))/Mpg_grp(IGP,ILDV,ICL,n)
										CafeMpgWgt = CafeMpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n))/(Mpg_grp(IGP,ILDV,ICL,n))
										DedAFVMpgWgt = DedAFVMpgWgt+(class_share(mnumcr,ICL,IGP,yrs)*apshr55_grp(IGP,ILDV,ICL,n))/(Mpg_grp(IGP,ILDV,ICL,n))                          
									endif
								endif
							enddo
						enddo
!...                    account for AFV CAFE credits 
						if(MpgWgt.ne.0.0 .and. CafeMpgWgt.ne.0.0 .and. DedAFVMpgWgt.ne.0.0 )then
							if(yrs.eq.2019)then
							  maxFFcredit = 0.2
							endif                
							TrueMpgGrp2(IGP,n)=GrpWgt/MpgWgt              
							CafeMpgGrp2(IGP,n)=min(GrpWgt/CafeMpgWgt, GrpWgt/DedAFVMpgWgt + maxFFcredit)
						else
							TrueMpgGrp2(IGP,n)=0.0
							CafeMpgGrp2(IGP,n)=0.0
						endif
!...                    calculate AC and off cycle fuel economy credits
                        if(curcalyr.ge.2020) then
                          CafeMpgGrp2(igp,n) = ((1/CafeMpgGrp2(igp,n)) - ac_oc_credit(igp,n))**-1
		                endif						
					
						if(CafeMpgGrp2(IGP,n).ge.cafe_used(IGP,yrs)) Cafepass2(IGP) = .true.
!...                    if CAFE is met then exit
						if(CafePass2(IGP).eq. .true.)exit
!...                    if no gasoline or ffv's to displace then exit                
						if(stop_gas.eq.1 .and. stop_ffv.eq.1)exit
					enddo
				endif
			enddo
		  endif
		enddo
	enddo

!...if no adv tech vehicle sales adjustment is needed set MFG mpg to initial estimation
    do IGP=1,MAXGROUP
      if(TrueMpgGrp2(IGP,n).eq.0.) TrueMpgGrp2(IGP,n) = TrueMpgGrp(mnumcr,IGP,n)
      if(CafeMpgGrp2(IGP,n).eq.0.) CafeMpgGrp2(IGP,n) = CafeMpgGrp(IGP,n)
    enddo

!...recalculate average new tested mpg 
    TrueMPG(1,n) = 0.0 
	TrueMPG(2,n) = 0.0
    do IGP = 1,cargrp
	  if(truempggrp2(IGP,n).ne.0) TrueMPG(1,n) = TrueMPG(1,n) + saleshr(mnumcr,IGP,n)/truempggrp2(IGP,n)
    enddo
	do IGP = cargrp+1,MAXGROUP
	  if(truempggrp2(IGP,n).ne.0) TrueMPG(2,n) = TrueMPG(2,n) + saleshr(mnumcr,IGP,n)/truempggrp2(IGP,n)
	enddo
	if(TrueMPG(1,n).ne.0.0) TrueMPG(1,n) = 1.0/TrueMPG(1,n)
	if(TrueMPG(2,n).ne.0.0) TrueMPG(2,n) = 1.0/TrueMPG(2,n)

!...recalculate average new tested mpg with credits
    NewMPG(1,n) = 0.0
	NewMPG(2,n) = 0.0
    do IGP = 1,cargrp
	  if(cafempggrp2(IGP,n).ne.0) NewMPG(1,n) = NewMPG(1,n) + saleshr(mnumcr,IGP,n)/cafempggrp2(IGP,n)
    enddo
	do IGP = cargrp+1,MAXGROUP
	  if(cafempggrp2(IGP,n).ne.0) NewMPG(2,n) = NewMPG(2,n) + saleshr(mnumcr,IGP,n)/cafempggrp2(IGP,n)
	enddo
	if(NewMPG(1,n).ne.0.0) NewMPG(1,n) = 1.0/NewMPG(1,n)
	if(NewMPG(2,n).ne.0.0) NewMPG(2,n) = 1.0/NewMPG(2,n)	

!...calculate new sales totals for light duty vehicles
    do ILDV=1,maxldv
      do ICL=1,MAXCLASS
        ncstech(mnumcr,ICL,ILDV,n) = sum(avsales_new(1:cargrp,ICL,ILDV,n))
        nltech(mnumcr,ICL,ILDV,n)  = sum(avsales_new(cargrp+1:MAXGROUP,ICL,ILDV,n))
      enddo
    enddo

!...calculate distribution of vehicle sales across regions  
    apshr55_reg = 0.
    do iregn=1,mnumcr
      do ILDV=1,maxldv
        if(sum(avsales(1:maxvtyp,1:MAXCLASS,1:mnumcr-2,ILDV)).ne. 0.)then
          apshr55_reg(iregn,ILDV)=sum(avsales(1:maxvtyp,1:MAXCLASS,iregn,ILDV))/sum(avsales(1:maxvtyp,1:MAXCLASS,1:mnumcr-2,ILDV))
		!  if(curcalyr.eq.2012) write(21,*)'iregn,ILDV,apshr55_reg',iregn,ILDV,apshr55_reg(iregn,ILDV)
        endif   
        do ICL=1,MAXCLASS
          ncstech(iregn,ICL,ILDV,n) = ncstech(mnumcr,ICL,ILDV,n) * apshr55_reg(iregn,ILDV)
          nltech(iregn,ICL,ILDV,n)  = nltech(mnumcr,ICL,ILDV,n) * apshr55_reg(iregn,ILDV)
        enddo
      enddo
    enddo
		  
!...recalculate total personal vehicle sales and fleet vehicle sales by size class and fueling technology
    do ICL=1,MAXCLASS
      do ILDV=1,maxldv
        TOTALSALSC(1,ICL,ILDV,n) = NCSTECH(mnumcr,ICL,ILDV,n)+(FLTSALSC(1,ICL,ILDV,n)/1000000.0)
        TOTALSALSC(2,ICL,ILDV,n) = NLTECH(mnumcr,ICL,ILDV,n) +(FLTSALSC(2,ICL,ILDV,n)/1000000.0)
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

!...Declare local parameters
!HMS-These variable are for the hydrogen market segment project.
real HVTot,TTemp,VTTemp1,VTTemp2,VTTemp3     
real sumcng(mnumcr,maxvtyp,mnumyr), sumlpg(mnumcr,maxvtyp,mnumyr)

    TECHNCSREGN(:,:,:,n) = 0.0
	TECHNLTREGN(:,:,:,n) = 0.0

!...Calculate total new vehicle sales by technology
    do ILDV=1,maxldv 
      TECHNCS(ILDV,n) = sum(NCSTECH(1:mnumcr-2,1:MAXCLASS,ILDV,n)) 
      TECHNLT(ILDV,n) = sum(NLTECH(1:mnumcr-2,1:MAXCLASS,ILDV,n)) 
    enddo

!...Calculate total new vehicle sales by region and technology 
    do iregn = 1, mnumcr
	 do ILDV=1,maxldv
      TECHNCSREGN(iregn,ILDV,1,n) = sum(NCSTECH(iregn,1:MAXCLASS,ILDV,n)) 
      TECHNLTREGN(iregn,ILDV,1,n) = sum(NLTECH(iregn,1:MAXCLASS,ILDV,n)) 
     enddo
    enddo	

!-----------------------------------------------------------------------------------------------------------------------------------
!...This section of code is implemented to populate reporting tables with historical data for light duty vehicles.  The 
!...1995-2020 vehicle stock values come from - Polk w/adjustments for vehicle stocks assigned to fleet and commercial light truck 
!...(Class 2b) stocks which have been subtracted from the total Polk LDV stock values.  The stock variables are disaggregated by 
!...fuel type, census division, and by fleet

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
	
!...sum active/retired fleet for transfer/subtraction to/from non-fleet - HAVs do not transfer
    OLDFSTKT=0.0
	if(curcalyr.gt.stockyr)then
	  do iregn=1,mnumcr-2
        do IVTYP=1,maxvtyp
          do ILDV=1,maxldv
		    if(ILDV.lt.9.and.ILDV.gt.12) then
              do iage=1,maxage
                oldfstkt(iregn,IVTYP,ILDV,iage)=sum(oldfstk(iregn,IVTYP,1:maxfleet,ILDV,iage))/1000000.0
              enddo
		    endif
          enddo
        enddo
	  enddo
	endif

!... calculate ldv stock post STOCKYR - by region
	if(curcalyr.gt.stockyr) then
	  do iregn = 1,mnumcr-2
        do ILDV=1,maxldv
	      do ihav = 1,maxhav
		    LDV_STOCK(iregn,1,1,ILDV,1,ihav,n) = TECHNCSREGN(iregn,ILDV,ihav,n)
		    LDV_STOCK(iregn,2,1,ILDV,1,ihav,n) = TECHNLTREGN(iregn,ILDV,ihav,n)
		    do IVTYP = 1,maxvtyp
              do iage=2,maxage-1
		        LDV_STOCK(iregn,IVTYP,1,ILDV,iage,ihav,n) = LDV_STOCK(iregn,IVTYP,1,ILDV,iage-1,ihav,n-1)*SSURV25(iregn,iage-1,IVTYP)	  
              enddo
		      LDV_STOCK(iregn,IVTYP,1,ILDV,maxage,ihav,n) = LDV_STOCK(iregn,IVTYP,1,ILDV,maxage-1,ihav,n-1)*SSURV25(iregn,maxage-1,IVTYP) + &
			                                                LDV_STOCK(iregn,IVTYP,1,ILDV,maxage,ihav,n-1)*SSURV25(iregn,maxage,IVTYP) 
		    enddo
		  enddo
!...    transfer retired fleet stock to non-fleet stock (taxis are not transfered to HH stock) 
          do IVTYP=1,maxvtyp
		    do iage=1,maxage
			  LDV_STOCK(iregn,IVTYP,1,ILDV,iage,1,n) = LDV_STOCK(iregn,IVTYP,1,ILDV,iage,1,n) + OLDFSTKT(iregn,IVTYP,ILDV,iage)
			enddo
          enddo
	    enddo
      enddo
	endif

!... Summing across regions to determine national stock quantities
	if(curcalyr.gt.stockyr) then
	  do IVTYP = 1,maxvtyp
		do iown = 1,maxowner
          do ILDV=1,maxldv
			do iage = 1,maxage
			  do ihav = 1,maxhav
		        LDV_STOCK(mnumcr,IVTYP,iown,ILDV,iage,ihav,n) = sum(LDV_STOCK(1:mnumcr-2,IVTYP,iown,ILDV,iage,ihav,n))
			  enddo
		    enddo
		  enddo	  
        enddo
	  enddo
    endif	  

! ... Calculate vintaged stocks for hydrogen vehicles by market segment.
      if(curcalyr.le.first_read_year) then
        do hms=1,3
          do IVTYP=1,maxvtyp
            do ICL=1,MAXCLASS
              do iregn=1,mnumcr-2
                do iage=1,maxage
                  HVStkV(hms,IVTYP,ICL,iregn,iage,n)=0.0
                enddo
              enddo
            enddo
          enddo
        enddo
      else
        do hms=1,3
          do IVTYP=1,maxvtyp
            do ICL=1,MAXCLASS
              do iregn=1,mnumcr-2
                HVStkV(hms,IVTYP,ICL,iregn,1,n)=HVSales(hms,IVTYP,ICL,iregn)
                do iage=2,maxage-1
                  HVStkV(hms,IVTYP,ICL,iregn,iage,n) = HVStkV(hms,IVTYP,ICL,iregn,iage-1,n-1)*SSURV25(iregn,iage-1,IVTYP)
                enddo
                  HVStkV(hms,IVTYP,ICL,iregn,maxage,n) = HVStkV(hms,IVTYP,ICL,iregn,maxage-1,n-1)*SSURV25(iregn,maxage-1,IVTYP)+ &
                                                         HVStkV(hms,IVTYP,ICL,iregn,maxage,n-1)*SSURV25(iregn,maxage,IVTYP)	
              enddo
            enddo
          enddo
        enddo
      endif
! ... Write sales and vintaged stocks for one region, type, and class.
!      if(fcrl.eq.1) then
!        iregn=5
!        IVTYP=2
!        ICL=5 ! FCV is class specific (no sports cars) - will some be CAVs  
!        write(H2UNIT,'(/,a,i4,3(a,i1))') 'Year ',curiyr+1989,', Region ',iregn,', Type ',IVTYP,', Class ',ICL
!        write(H2UNIT,'(a)') ' Alternate Fuel Technology Vehicle Sales (millions)'
!        write(H2UNIT,'(a)') '             Large City  Small City       Rural       Total'
!        TTemp=HVSales(1,IVTYP,ICL,iregn)+HVSales(2,IVTYP,ICL,iregn)+HVSales(3,IVTYP,ICL,iregn)
!        write(H2UNIT,'(a,2x,4f12.4)') ' Hydrogen',(HVSales(hms,IVTYP,ICL,iregn),hms=1,3),TTemp
!        write(H2UNIT,'(a)') ' Alternate Fuel Technology Vehicle Stocks by Vintage (millions)'
!        write(H2UNIT,'(a)') '             Large City  Small City       Rural       Total'
!        VTTemp1=0.0
!        VTTemp2=0.0
!        VTTemp3=0.0
!        do iage=1,maxage
!          TTemp=HVStkV(1,IVTYP,ICL,iregn,iage,n)+HVStkV(2,IVTYP,ICL,iregn,iage,n)+HVStkV(3,IVTYP,ICL,iregn,iage,n)
!          write(H2UNIT,'(a,i2,3x,4f12.4)') ' Vint ',iage-1,(HVStkV(hms,IVTYP,ICL,iregn,iage,n),hms=1,3),TTemp
!          VTTemp1=VTTemp1+HVStkV(1,IVTYP,ICL,iregn,iage,n)
!          VTTemp2=VTTemp2+HVStkV(2,IVTYP,ICL,iregn,iage,n)
!          VTTemp3=VTTemp3+HVStkV(3,IVTYP,ICL,iregn,iage,n)
!        enddo
!        TTemp=VTTemp1+VTTemp2+VTTemp3
!        write(H2UNIT,'(a,2x,4f12.4)') ' Total   ',VTTemp1,VTTemp2,VTTemp3,TTemp
!! ... Write out some details on sales shares.
!        write(H2UNIT,'(/,a,i4)') 'Hydrogen Sales Shares in ',curiyr+1989
!        write(H2UNIT,'(6x,a)') '    Reg1    Reg2    Reg3    Reg4    Reg5    Reg6    Reg7    Reg8    Reg9'
!        do IVTYP=1,2
!          do ICL=1,6
!            if(IVTYP.eq.1) write(H2UNIT,'(a,i1,9f8.3)') '  Car',ICL,(APShr44(IVTYP,ICL,iregn,14,n),iregn=1,9)
!            if(IVTYP.eq.2) write(H2UNIT,'(a,i1,9f8.3)') '  Trk',ICL,(APShr44(IVTYP,ICL,iregn,14,n),iregn=1,9)
!          enddo
!        enddo
!      endif
! ... Sum up over vintages, types, and classes and calculate market segment shares.
      do iregn=1,mnumcr-2
        HVTot=0.0
        do hms=1,3
          HVStkT(hms,iregn)=0.0
          do IVTYP=1,maxvtyp
            do ICL=1,MAXCLASS
              do iage=1,maxage
                HVStkT(hms,iregn)=HVStkT(hms,iregn)+HVStkV(hms,IVTYP,ICL,iregn,iage,n)
              enddo
            enddo
          enddo
          HVTot=HVTot+HVStkT(hms,iregn)
        enddo
        do hms=1,3
          if(HVTot.gt.0.0) then
            HVStkS(hms,iregn,n)=HVStkT(hms,iregn)/HVTot
          else
            HVStkS(hms,iregn,n)=0.0
          endif
        enddo
      enddo
! ... Sum up over regions and calculate national market shares.
      do hms=1,3
        HVStkT(hms,11)=0.0
        do iregn=1,mnumcr-2
          HVStkT(hms,11)=HVStkT(hms,11)+HVStkT(hms,iregn)
        enddo
      enddo
      HVTot=HVStkT(1,11)+HVStkT(2,11)+HVStkT(3,11)
      do hms=1,3
        if(HVTot.gt.0.0) then
          HVStkS(hms,11,n)=HVStkT(hms,11)/HVTot
        else
          HVStkS(hms,11,n)=0.0
        endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!...Calculate total ldv vehicle stock by fuel types (for table 49)
    do ILDV=1,maxldv
	  do IVTYP = 1,maxvtyp
	   VSTK(IVTYP,ILDV) = sum(LDV_STOCK(mnumcr,IVTYP,1:maxowner,ILDV,1:maxage,1:maxhav,n)) 
	  enddo
    enddo
    
!...calculate total non-fleet stocks
    STKCAR(n) = sum(LDV_STOCK(mnumcr,1,1,1:maxldv,1:maxage,1:maxhav,n)) 
    STKTR(n)  = sum(LDV_STOCK(mnumcr,2,1,1:maxldv,1:maxage,1:maxhav,n)) 

!...calculate light duty vehicles per licensed driver
    VPLD(n) = sum(vstk(1:maxvtyp,1:maxldv))/SUM(LicDriver(1:AGEGRP,1:MF,1:MNUMCR-2,n)) 

!...calculate total LDV stocks to determine alt fuel availability
    DO ILDV=1,MAXLDV
      LDVSTK(ILDV,N) = sum(vstk(1:maxvtyp,ILDV)) !..check this
    ENDDO
  RETURN
  END SUBROUTINE TSMOD

! ==========================================================================================================
! ... Subroutine TMPGSTK calculates light vehicle stock mpg by technology
! ==========================================================================================================
  SUBROUTINE TMPGSTK
  USE T_
  IMPLICIT NONE

    REAL     NVSALES(maxvtyp,MAXCLASS,MAXLDV,MNUMYR), &
             VSIZESAL(maxvtyp,MAXCLASS,MNUMYR), &
             CMPGT(MNUMYR,mnumcr), &
             TMPGT(MNUMYR,mnumcr), &
             NUM1,DEN1,NUM2,DEN2, &
             IAGE01, &
             MGSHARE
    INTEGER*4 ModelYR, MY01FLG
    LOGICAL   ONCE_E15/.TRUE./


!...Calculate new car and light truck sales for 8 size classes
    DO ILDV=1,MAXLDV
      DO ICL=1,MAXCLASS
        NVSALES(1,ICL,ILDV,N) = 0.0
        NVSALES(2,ICL,ILDV,N) = 0.0
        DO IREGN=1,MNUMCR-2
          NVSALES(1,ICL,ILDV,N) = NVSALES(1,ICL,ILDV,N) + NCSTECH(IREGN,ICL,ILDV,N)
          NVSALES(2,ICL,ILDV,N) = NVSALES(2,ICL,ILDV,N) + NLTECH(IREGN,ICL,ILDV,N)
        ENDDO
      ENDDO
    ENDDO

!...Calculate the average mpg of the 14 AFVs technologies - table 52
    DO IVTYP=1,MAXVTYP
      DO ICL=1,MAXCLASS
        NUM1 = 0.0
        DEN1 = 0.0
        DO ILDV=3,MAXLDV
          IF (LDV_MPG_CL(IVTYP,ILDV,ICL,YRS) .NE. 0.0) THEN
            NUM1 = NUM1 +  NVSALES(IVTYP,ICL,ILDV,N)
            DEN1 = DEN1 + (NVSALES(IVTYP,ICL,ILDV,N) / LDV_MPG_CL(IVTYP,ILDV,ICL,YRS))
          ENDIF
        ENDDO
        AFVFE(IVTYP,ICL,N) = 0.0
        IF (DEN1 .NE. 0.0) AFVFE(IVTYP,ICL,N) = NUM1/DEN1
      ENDDO
    ENDDO

    DO IVTYP=1,MAXVTYP
      DO ICL=1,MAXCLASS
        VSIZESAL(IVTYP,ICL,N) = 0.0
        DO ILDV=3,MAXLDV
          VSIZESAL(IVTYP,ICL,N) = VSIZESAL(IVTYP,ICL,N) + NVSALES(IVTYP,ICL,ILDV,N)
        ENDDO
      ENDDO
    ENDDO

    DO IVTYP=1,MAXVTYP
      NUM1 = 0.0
      DEN1 = 0.0
      DO ICL=1,MAXCLASS
        IF (AFVFE(IVTYP,ICL,N) .NE. 0.0) THEN
          NUM1 = NUM1 + VSIZESAL(IVTYP,ICL,N)
          DEN1 = DEN1 + VSIZESAL(IVTYP,ICL,N) / AFVFE(IVTYP,ICL,N)
        ENDIF
      ENDDO
      AFVFETOT(IVTYP,N) = 0.0
      IF (DEN1 .NE. 0.0) AFVFETOT(IVTYP,N) = NUM1/DEN1
    ENDDO

!...Compute the average new car and light truck mpg (MPGC & MPGT)   
!... 
!...Collapse new car and light truck mpg from 8 size classes to 1   
    DO ILDV=1,MAXLDV
      NUM1 = 0.0
      DEN1 = 0.0
      NUM2 = 0.0
      DEN2 = 0.0
      IF (ILDV .GT. GAS) THEN            ! alternative fuel technology
        DO ICL=1,MAXCLASS
          IF (LDV_MPG_CL(1,ILDV,ICL,YRS) .NE. 0.0) THEN
            NUM1 = NUM1 +  NVSALES(1,ICL,ILDV,N)
            DEN1 = DEN1 + (NVSALES(1,ICL,ILDV,N)/LDV_MPG_CL(1,ILDV,ICL,YRS))
          ENDIF
          IF (LDV_MPG_CL(2,ILDV,ICL,YRS) .NE. 0.0) THEN
            NUM2 = NUM2 +  NVSALES(2,ICL,ILDV,N)
            DEN2 = DEN2 + (NVSALES(2,ICL,ILDV,N)/LDV_MPG_CL(2,ILDV,ICL,YRS))
          ENDIF
        ENDDO
      ELSE                                ! conventional technology
        IF (curcalyr.LE.1994) THEN 
          DO ICL=1,MAXCLASS
            IF (LDV_MPG_CL(1,ILDV,ICL,YRS) .NE. 0.0) THEN
              NUM1 = NUM1 +  PASSHRR(11,ICL,N)
              DEN1 = DEN1 + (PASSHRR(11,ICL,N)/LDV_MPG_CL(1,ILDV,ICL,YRS))
            ENDIF
            IF (LDV_MPG_CL(2,ILDV,ICL,YRS) .NE. 0.0) THEN
              NUM2 = NUM2 +  LTSHRR(11,ICL,N)
              DEN2 = DEN2 + (LTSHRR(11,ICL,N)/LDV_MPG_CL(2,ILDV,ICL,YRS))
            ENDIF
          ENDDO
        ELSE
          DO ICL=1,MAXCLASS
            IF (LDV_MPG_CL(1,ILDV,ICL,YRS) .NE. 0.0) THEN
              NUM1 = NUM1 +  NVSALES(1,ICL,ILDV,N)
              DEN1 = DEN1 + (NVSALES(1,ICL,ILDV,N)/LDV_MPG_CL(1,ILDV,ICL,YRS))
            ENDIF
            IF (LDV_MPG_CL(2,ILDV,ICL,YRS) .NE. 0.0) THEN
              NUM2 = NUM2 +  NVSALES(2,ICL,ILDV,N)
              DEN2 = DEN2 + (NVSALES(2,ICL,ILDV,N)/LDV_MPG_CL(2,ILDV,ICL,YRS))
            ENDIF
          ENDDO
        ENDIF
      ENDIF
      MPGC(ILDV,N) = 0.0
      MPGT(ILDV,N) = 0.0
      IF (DEN1 .NE. 0.0) MPGC(ILDV,N) = NUM1/DEN1
      IF (DEN2 .NE. 0.0) MPGT(ILDV,N) = NUM2/DEN2
    ENDDO

!...Calculate stock mpg for cars and lt. trucks                                 
!...Note: Starting mpg values come from NHTSA data file                         
                                                                  
!...Adjust degradation factors to account for lower BTU content of reformulated 
!...gasoline
    do ihav=1,maxhav
        IF (curcalyr.le.2015) THEN 
          CDFRFG(N,ihav)  = CDF(N)*hav_mpgdeg(ihav, curcalyr)
          LTDFRFG(N,ihav) = LTDF(N)*hav_mpgdeg(ihav, curcalyr)
        ELSE
          CDFRFG(N,ihav)  = CDF(N)*hav_mpgdeg(ihav, curcalyr)
          LTDFRFG(N,ihav) = LTDF(N)*hav_mpgdeg(ihav, curcalyr)
        ENDIF
    enddo
    DO ILDV=1,MAXLDV
		!populate first vintage with new vehicle fuel economy
		!dst stocks and vmt, should be regionalized, with regionalized sales for MPGC and MPGT
		do iregn=1,mnumcr
          CMPGSTK(ILDV,1,N,iregn)  = MPGC(ILDV,N)	!first vintage (iage=1) stock FE equal new car FE
          TTMPGSTK(ILDV,1,N,iregn) = MPGT(ILDV,N)	!first vintage (iage=1) stock FE equal new truck FE
		enddo
		
      if(curcalyr.eq.first_read_year)then	!if 1995, set stock for all vintages to 1995 stock FE
        do iage=1,maxage
		  if(LDV_STOCK(mnumcr,1,1,ILDV,iage,1,n).ne.0.0) then		  
            if(ILDV.eq.4.or.ILDV.eq.7.or.ILDV.eq.15)then
                CMPGSTK(ILDV,iage,n,mnumcr) = CMPGSTKGAS95(1,iage) * 5.0
                TTMPGSTK(ILDV,iage,n,mnumcr) = CMPGSTKGAS95(2,iage) * 4.0
              else	!FE adjustment for other fuels
                CMPGSTK(ILDV,iage,n,mnumcr) = CMPGSTKGAS95(1,iage) * (1+AFVADJFE(ILDV,XYR))
                TTMPGSTK(ILDV,iage,n,mnumcr) = CMPGSTKGAS95(2,iage) * (1+AFVADJFE(ILDV,XYR))
            endif
          endif
        enddo
      elseif (yrs.le.2011) then	!years after 1995 through 2011 - stock FE not regionalized
        DO IAGE=2,MAXAGE
          IF (IAGE .LT. MAXAGE) THEN
		      !populate stock FE with previous age and year FE
            CMPGSTK(ILDV,IAGE,N,mnumcr)  = CMPGSTK(ILDV,IAGE-1,N-1,mnumcr)
            TTMPGSTK(ILDV,IAGE,N,mnumcr) = TTMPGSTK(ILDV,IAGE-1,N-1,mnumcr)
          ELSE	!if max age of 25, harmonically weight existing 25+ vehicle FE with "new" 25 year old vehicle FE
			  !if there are stocks of ldv type in previous year and vintage, stock FE is harmonic average of
			  !last year's previous vintage FE, and last years current vintage FE 
            CMPGSTK(ILDV,IAGE,N,mnumcr)=21.6 ! Source-Tran Energy Data Book-Edition 21-Tab 7.1		
			if(LDV_STOCK(mnumcr,1,1,ILDV,iage-1,1,n-1).gt.0.0001) &
			  CMPGSTK(ILDV,IAGE,N,mnumcr) = 1.0 / (((LDV_STOCK(mnumcr,1,1,ILDV,iage-1,1,n-1) / CMPGSTK(ILDV,IAGE-1,N-1,mnumcr)) + &
                                             (LDV_STOCK(mnumcr,1,1,ILDV,iage,1,n-1) / CMPGSTK(ILDV,IAGE,N-1,mnumcr))) /  &
                                             (LDV_STOCK(mnumcr,1,1,ILDV,iage,1,n-1) + LDV_STOCK(mnumcr,1,1,ILDV,iage-1,1,n-1)))
											 
            TTMPGSTK(ILDV,IAGE,N,mnumcr) = 17.1 ! Source-Tran Energy Data Book-Ed 21-Table 7.2
			if(LDV_STOCK(mnumcr,2,1,ILDV,iage-1,1,n-1).gt.0.0001) &
			  TTMPGSTK(ILDV,IAGE,N,mnumcr) = 1.0 / (((LDV_STOCK(mnumcr,2,1,ILDV,iage-1,1,n-1) / TTMPGSTK(ILDV,IAGE-1,N-1,mnumcr)) + &
                                             (LDV_STOCK(mnumcr,2,1,ILDV,iage,1,n-1) / TTMPGSTK(ILDV,IAGE,N-1,mnumcr))) /  &
                                             (LDV_STOCK(mnumcr,2,1,ILDV,iage,1,n-1) + LDV_STOCK(mnumcr,2,1,ILDV,iage-1,1,n-1)))
          ENDIF
        ENDDO !end iage loop		
		  
      else	!years after 2011 are regionalized
		
		DO iregn=1,mnumcr-2
          DO IAGE=2,MAXAGE
            IF (IAGE .LT. MAXAGE) THEN
!...            populate stock FE with previous age and year FE
                if(yrs.eq.2012) then  
!...                applies national mpg by age to all of 2012 since before 2011, regional isn't filled in
                    CMPGSTK(ILDV,IAGE,N,iregn)  = CMPGSTK(ILDV,IAGE-1,N-1,mnumcr)
					TTMPGSTK(ILDV,IAGE,N,iregn) = TTMPGSTK(ILDV,IAGE-1,N-1,mnumcr)
                else
					CMPGSTK(ILDV,IAGE,N,iregn)  = CMPGSTK(ILDV,IAGE-1,N-1,iregn)
					TTMPGSTK(ILDV,IAGE,N,iregn) = TTMPGSTK(ILDV,IAGE-1,N-1,iregn)
               endif
            ELSE           
!...          if max age of 25, harmonically weight existing 25+ vehicle FE with "new" 25 year old vehicle FE
!...          if there are stocks of ldv type in previous year and vintage, stock FE is harmonic average of
!...          last year's previous vintage FE, and last years current vintage FE
              CMPGSTK(ILDV,IAGE,N,iregn)=21.6 ! Source-Tran Energy Data Book-Edition 21-Tab 7.1                                                                                                                                    
              if(sum(LDV_STOCK(iregn,1,1,ILDV,iage-1,1:maxhav,n-1)).gt.0.0001) &
                CMPGSTK(ILDV,IAGE,N,iregn) = 1.0 / (((sum(LDV_STOCK(iregn,1,1,ILDV,iage-1,1:maxhav,n-1)) / CMPGSTK(ILDV,IAGE-1,N-1,iregn)) + &
                (sum(LDV_STOCK(iregn,1,1,ILDV,iage,1:maxhav,n-1)) / CMPGSTK(ILDV,IAGE,N-1,iregn))) /  &
                (sum(LDV_STOCK(iregn,1,1,ILDV,iage,1:maxhav,n-1)) + sum(LDV_STOCK(iregn,1,1,ILDV,iage-1,1:maxhav,n-1))))
                                                                                                                                                                                
              TTMPGSTK(ILDV,IAGE,N,iregn) = 17.1 ! Source-Tran Energy Data Book-Ed 21-Table 7.2
              if(sum(LDV_STOCK(iregn,2,1,ILDV,iage-1,1:maxhav,n-1)).gt.0.0001) &                                                                                                                                                                    
                TTMPGSTK(ILDV,IAGE,N,iregn) = 1.0 / (((sum(LDV_STOCK(iregn,2,1,ILDV,iage-1,1:maxhav,n-1)) / TTMPGSTK(ILDV,IAGE-1,N-1,iregn)) + &
                (sum(LDV_STOCK(iregn,2,1,ILDV,iage,1:maxhav,n-1)) / TTMPGSTK(ILDV,IAGE,N-1,iregn))) /  &
                (sum(LDV_STOCK(iregn,2,1,ILDV,iage,1:maxhav,n-1)) + sum(LDV_STOCK(iregn,2,1,ILDV,iage-1,1:maxhav,n-1))))                                                                                                                                                                          
            ENDIF
          ENDDO !end iage loop
		ENDDO !end iregn loop  
		
      ENDIF
    ENDDO	!end ILDV loop


!...Calculate national household vmt/vintage
	  do iage=1,maxage
		NUM1 = 0.0
		NUM2 = 0.0		
		do iregn=1,mnumcr-2
		  !sum each region's vmt/vintage weighted by their stock
		  DO ildv=1,maxldv
		    NUM1 = NUM1 + (PVMT(iage,n,iregn,ildv)*sum(LDV_STOCK(iregn,1,1,ildv,iage,1:maxhav,n)))
		    NUM2 = NUM2 + (LVMT(iage,n,iregn,ildv)*sum(LDV_STOCK(iregn,2,1,ildv,iage,1:maxhav,n)))
		  ENDDO
		enddo
		!divide sum by total stock to finish weighted average calculation
		  DO ildv=1,maxldv
		    PVMT(iage,n,mnumcr,ildv)=NUM1/sum(LDV_STOCK(mnumcr,1,1,ildv,iage,1:maxhav,n))
		    LVMT(iage,n,mnumcr,ildv)=NUM2/sum(LDV_STOCK(mnumcr,2,1,ildv,iage,1:maxhav,n))		
		  ENDDO
		enddo
 
!...Calculate total household miles driven by each type of vehicle by vintage
	VMT_STK_HH(:,:,:,:,:) = 0.0
    do ILDV=1,maxldv
      do iage=1,maxage
		do iregn=1,mnumcr-2
          do ihav = 1, maxhav 			
			VMT_STK_HH(1,ILDV,iage,ihav,iregn) = LDV_STOCK(iregn,1,1,ILDV,iage,ihav,n)*PVMT(iage,n,iregn,ildv)
			VMT_STK_HH(2,ILDV,iage,ihav,iregn) = LDV_STOCK(iregn,2,1,ILDV,iage,ihav,n)*LVMT(iage,n,iregn,ildv)
		  enddo			  
		enddo
		do IVTYP = 1,maxvtyp		
		  do ihav = 1,maxhav
		    VMT_STK_HH(IVTYP,ILDV,iage,ihav,mnumcr)=sum(VMT_STK_HH(IVTYP,ILDV,iage,ihav,1:mnumcr-2))
          enddo
		enddo	  
      enddo
    enddo
	
!...Calculate regional household LDV VMT shares for each tech and vehicle type (VSPLDV)
    do ILDV=1,maxldv
	  do iregn=1,mnumcr-2
        do IVTYP = 1,maxvtyp
			VSPLDV(ILDV,n,iregn,IVTYP) = sum(VMT_STK_HH(IVTYP,ILDV,1:maxage,1,iregn))/sum(VMT_STK_HH(1:maxvtyp,1:maxldv,1:maxage,1,iregn))
        enddo
	  enddo
    enddo			

	do iregn=1,mnumcr
      CMPGT(N,iregn) = 0.0	!car hh consumption (denominator for FE)
      TMPGT(N,iregn) = 0.0	!LT hh consumption (denominator for FE)
	enddo
	
	if(yrs.le.2011) then
	  DO ILDV=1,MAXLDV
        DO IAGE=1,MAXAGE    
		  IF (CMPGSTK(ILDV,IAGE,N,mnumcr).NE.0.0) CMPGT(N,mnumcr) = CMPGT(N,mnumcr)+(VMT_STK_HH(1,ILDV,iage,1,mnumcr) /  &
                                                   (CMPGSTK(ILDV,IAGE,N,mnumcr) * CDFRFG(N,1)))
          IF (TTMPGSTK(ILDV,IAGE,N,mnumcr).NE.0.0) TMPGT(N,mnumcr) = TMPGT(N,mnumcr)+(VMT_STK_HH(2,ILDV,iage,1,mnumcr) /  &
                                                    (TTMPGSTK(ILDV,IAGE,N,mnumcr) * LTDFRFG(N,1)))
		enddo
      ENDDO
	else	!after 2011, consumption is regionalized
      do iregn=1,mnumcr-2
	    DO ILDV=1,MAXLDV
          DO IAGE=1,MAXAGE  
            do ihav = 1,maxhav		  
			  IF (CMPGSTK(ILDV,IAGE,N,iregn).NE.0.0) CMPGT(N,iregn) = CMPGT(N,iregn)+(VMT_STK_HH(1,ILDV,iage,ihav,iregn) /  &
                                                   (CMPGSTK(ILDV,IAGE,N,iregn) * CDFRFG(N,ihav)))
              IF (TTMPGSTK(ILDV,IAGE,N,iregn).NE.0.0) TMPGT(N,iregn) = TMPGT(N,iregn)+(VMT_STK_HH(2,ILDV,iage,ihav,iregn) /  &
                                                    (TTMPGSTK(ILDV,IAGE,N,iregn) * LTDFRFG(N,ihav)))
			enddo
		  enddo
        ENDDO
      ENDDO
	  !calculate national consumption after 2011
	  CMPGT(N,mnumcr) = sum(CMPGT(N,1:mnumcr-2))	!hh car
	  TMPGT(N,mnumcr) = sum(TMPGT(N,1:mnumcr-2))	!hh truck	
	endif


!...Calculate average mpg of household light duty vehicles	

	!national household car and truck stock fuel economies	
	SCMPG(N) = sum(VMT_STK_HH(1,1:maxldv,1:maxage,1:maxhav,1:mnumcr-2))/CMPGT(N,mnumcr)
	STMPG(N) = sum(VMT_STK_HH(2,1:maxldv,1:maxage,1:maxhav,1:mnumcr-2))/TMPGT(N,mnumcr)
	
	!national household combined stock fuel economy
    MPGHH(N) = sum(VMT_STK_HH(1:maxvtyp,1:maxldv,1:maxage,1:maxhav,1:mnumcr-2)) / (CMPGT(N,mnumcr)+TMPGT(N,mnumcr))
	
!...Calculate average vehicle mpg by technology (combined car and LT)

    do ILDV=1,maxldv
      NUM1 = 0.0
      DEN1 = 0.0
      DO IAGE=1,MAXAGE
		!add car numerator and denominator components together (truck stacks onto car)
		if(yrs.le.2011) then
          IF (CMPGSTK(ILDV,IAGE,N,mnumcr)*CDFRFG(N,1)  .NE. 0.0) THEN
            NUM1 = NUM1 + VMT_STK_HH(1,ILDV,iage,1,mnumcr)
            DEN1 = DEN1 + (VMT_STK_HH(1,ILDV,iage,1,mnumcr) / (CMPGSTK(ILDV,IAGE,N,mnumcr) * CDFRFG(N,1)))
		  endif
          IF (TTMPGSTK(ILDV,IAGE,N,mnumcr)*LTDFRFG(N,1) .NE. 0.0) THEN												
            NUM1 = NUM1 + VMT_STK_HH(2,ILDV,iage,1,mnumcr)
            DEN1 = DEN1 + (VMT_STK_HH(2,ILDV,iage,1,mnumcr) / (TTMPGSTK(ILDV,IAGE,N,mnumcr) * LTDFRFG(N,1)))
		  endif
		else	!after 2011, regionalize 
		  do iregn=1,mnumcr-2 !dst add fe regionality
	        do ihav = 1,maxhav			
		      IF (CMPGSTK(ILDV,IAGE,N,iregn)*CDFRFG(N,ihav)  .NE. 0.0) THEN											
                NUM1 = NUM1 + VMT_STK_HH(1,ILDV,iage,ihav,iregn)
                DEN1 = DEN1 + (VMT_STK_HH(1,ILDV,iage,ihav,iregn) / (CMPGSTK(ILDV,IAGE,N,iregn) * CDFRFG(N,ihav)))
		      endif
		      IF (TTMPGSTK(ILDV,IAGE,N,iregn)*LTDFRFG(N,ihav) .NE. 0.0) THEN
                NUM1 = NUM1 + VMT_STK_HH(2,ILDV,iage,ihav,iregn)
                DEN1 = DEN1 + (VMT_STK_HH(2,ILDV,iage,ihav,iregn) / (TTMPGSTK(ILDV,IAGE,N,iregn) * LTDFRFG(N,ihav)))
			  endif
			enddo ! end ihav loop
		  enddo	!end iregn loop
        ENDIF	!end regional year control
	  enddo		!end iage loop
      MPGTECH(ILDV,n) = 0.0
      IF (DEN1 .NE. 0.0) MPGTECH(ILDV,n) = NUM1/DEN1
    enddo	!end ILDV loop
	

!...Calculate average car and light truck mpg by technology
    DO ILDV=1,MAXLDV
      NUM1 = 0.0
      DEN1 = 0.0
      NUM2 = 0.0
      DEN2 = 0.0
      DO IAGE=1,MAXAGE
	    do ihav = 1,maxhav
		  if(yrs.le.2011) then
            IF (CMPGSTK(ILDV,IAGE,N,mnumcr)*CDFRFG(N,ihav) .NE. 0.0) THEN
              NUM1 = NUM1 + VMT_STK_HH(1,ILDV,iage,ihav,mnumcr)
              DEN1 = DEN1 + (VMT_STK_HH(1,ILDV,iage,ihav,mnumcr) / (CMPGSTK(ILDV,IAGE,N,mnumcr) * CDFRFG(N,ihav)))
		    endif
            if (TTMPGSTK(ILDV,IAGE,N,mnumcr)*LTDFRFG(N,ihav) .NE. 0.0) THEN
              NUM2 = NUM2 + VMT_STK_HH(2,ILDV,iage,ihav,mnumcr)
              DEN2 = DEN2 + (VMT_STK_HH(2,ILDV,iage,ihav,mnumcr) / (TTMPGSTK(ILDV,IAGE,N,mnumcr) * LTDFRFG(N,ihav)))
		    endif
          else	!after 2011, regionalize
		    do iregn=1,mnumcr-2
		      IF (CMPGSTK(ILDV,IAGE,N,iregn)*CDFRFG(N,ihav) .NE. 0.0) THEN
                NUM1 = NUM1 + VMT_STK_HH(1,ILDV,iage,ihav,iregn)
                DEN1 = DEN1 + (VMT_STK_HH(1,ILDV,iage,ihav,iregn) / (CMPGSTK(ILDV,IAGE,N,iregn) * CDFRFG(N,ihav)))
			  endif
			  if (TTMPGSTK(ILDV,IAGE,N,iregn)*LTDFRFG(N,ihav) .NE. 0.0) THEN
                NUM2 = NUM2 + VMT_STK_HH(2,ILDV,iage,ihav,iregn)
                DEN2 = DEN2 + (VMT_STK_HH(2,ILDV,iage,ihav,iregn) / (TTMPGSTK(ILDV,IAGE,N,iregn) * LTDFRFG(N,ihav)))
			  endif
		    enddo	!end iregn loop
          ENDIF	!end year regional control
		enddo ! end ihav loop
	  enddo		!end iage loop
      CMPG_IT(ILDV,N) = 0.0
      TMPG_IT(ILDV,N) = 0.0
      IF (DEN1 .NE. 0.0) CMPG_IT(ILDV,N) = NUM1/DEN1
      IF (DEN2 .NE. 0.0) TMPG_IT(ILDV,N) = NUM2/DEN2
    ENDDO	!end ILDV loop
	
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

  REAL     CMPGFSTK(MAXFLEET,MAXLDV,MaxAge,MNUMYR),TMPGFSTK(MAXFLEET,MAXLDV,MaxAge,MNUMYR),NUM,DEN
  REAL     MPGADJ(maxvtyp,maxldv),MPGADJ_CLS( maxvtyp,maxldv,MAXCLASS),CLASS_COUNT(maxvtyp,maxldv)    

!...Calculate new car and lt. truck mpg for the fleet 
    do IVTYP=1,maxvtyp
      do ifleet=1,maxfleet
        do ILDV=1,maxldv
!...      calculate mpg adjustment factors by vehicle by tech type 
		  class_count(IVTYP,ILDV) = 0.0
          do ICL=1,MAXCLASS
		    if(ILDV.gt.1)then
		      if(IVTYP.eq.1.and.CARFLG(ILDV-1,ICL).lt.curcalyr) then
			    class_count(IVTYP,ILDV)= class_count(IVTYP,ILDV)+1.
				mpgadj_cls(IVTYP,ILDV,ICL) = LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)/LDV_MPG_CL(IVTYP,1,ICL,yrs)
			  endif
              if(IVTYP.eq.2.and.TRKFLG(ILDV-1,ICL).lt.curcalyr) then 
			    class_count(IVTYP,ILDV)= class_count(IVTYP,ILDV)+1.
				mpgadj_cls(IVTYP,ILDV,ICL) = LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)/LDV_MPG_CL(IVTYP,1,ICL,yrs)
		      endif
            endif
		  enddo
!...      calculate average MPG adjustment for fleet vehicles
          mpgadj=0.0
          if(ILDV.eq.1)then
            mpgadj(IVTYP,ILDV)=1.0
          else
            if(class_count(IVTYP,ILDV).ge.1.) mpgadj(IVTYP,ILDV) = sum(mpgadj_cls(IVTYP,ILDV,1:MAXCLASS))/class_count(IVTYP,ILDV)
            if(mpgadj(IVTYP,ILDV).lt.1.) mpgadj(IVTYP,ILDV)=1.
		  endif
		  
          NUM = 0.0
          DEN = 0.0
		  do ICL=1,MAXCLASS
		    do ihav = 1,maxhav
              if(FLTECHSAL(mnumcr,IVTYP,ifleet,ICL,ILDV,ihav).ne.0.0)then
			    if(ILDV.gt.1.and.mpgadj(IVTYP,ILDV).eq.1.)then
			      if(IVTYP.eq.1)then
			         if(CARFLG(ILDV-1,ICL).gt.curcalyr) then
				      mpgadj(IVTYP,ILDV) = 1.+ AFVADJFE(ILDV,XYR)
				      if(ILDV.eq.4) mpgadj(IVTYP,ILDV) = 4.023  ! EV100
				  	  if(ILDV.eq.5) mpgadj(IVTYP,ILDV) = 1.581  ! PHEV20
					  if(ILDV.eq.6) mpgadj(IVTYP,ILDV) = 1.928  ! PHEV50
					  if(ILDV.eq.7) mpgadj(IVTYP,ILDV) = 5.0    ! EV200
					  if(ILDV.eq.15) mpgadj(IVTYP,ILDV) = 6.0    ! EV300 
				    endif
				  else
				    if(TRKFLG(ILDV-1,ICL).gt.curcalyr) then
				      mpgadj(IVTYP,ILDV) = 1.+ AFVADJFE(ILDV,XYR)
				      if(ILDV.eq.4) mpgadj(IVTYP,ILDV) = 3.677  ! EV100
					  if(ILDV.eq.5) mpgadj(IVTYP,ILDV) = 1.581  ! PHEV20
					  if(ILDV.eq.6) mpgadj(IVTYP,ILDV) = 1.928  ! PHEV50
					  if(ILDV.eq.7) mpgadj(IVTYP,ILDV) = 5.0    ! EV200	
					  if(ILDV.eq.15) mpgadj(IVTYP,ILDV) = 6.0    ! EV300 				
				    endif
			      endif
			    endif
			  
                NUM = NUM + FLTECHSAL(mnumcr,IVTYP,ifleet,ICL,ILDV,ihav)
			    if(LDV_MPG_CL(IVTYP,ILDV,ICL,yrs).lt.LDV_MPG_CL(IVTYP,1,ICL,yrs)) then
			      DEN = DEN + FLTECHSAL(mnumcr,IVTYP,ifleet,ICL,ILDV,ihav)/(LDV_MPG_CL(IVTYP,1,ICL,yrs) * mpgadj(IVTYP,ILDV))
                else				 
                  DEN = DEN + FLTECHSAL(mnumcr,IVTYP,ifleet,ICL,ILDV,ihav)/LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)
                endif
              endif
			enddo
          enddo
!      	  write(21,*)' mpgadj =',mpgadj(IVTYP,ILDV),'IVTYP =',IVTYP,'ILDV =',ILDV
          FLTMPG(IVTYP,ifleet,ILDV,n)=0.0
          if(DEN.ne.0.0) FLTMPG(IVTYP,ifleet,ILDV,n) = NUM/DEN
        enddo
      enddo
    enddo
 
    do IVTYP=1,maxvtyp
      do ifleet=1,maxfleet
        do ILDV=1,maxldv
		  if(sum(FLTECHSAL(mnumcr,IVTYP,ifleet,1:6,ILDV,1:maxhav)).eq.0.0.and.sum(Flt_Stock(mnumcr,IVTYP,ifleet,ILDV,1,1:maxhav,n)).gt.0.0)then
            if(ILDV.eq.4.or.ILDV.eq.7.or.ILDV.eq.15)then
              FLTMPG(1,ifleet,ILDV,n) = MPGC(1,n) * (FLTMPG(1,ifleet,1,6)/MPGC(1,6)) * 5.0
              FLTMPG(2,ifleet,ILDV,n) = MPGT(1,n) * (FLTMPG(2,ifleet,1,6)/MPGT(1,6)) * 4.0
            else
              FLTMPG(1,ifleet,ILDV,n) = MPGC(1,n) * (FLTMPG(1,ifleet,1,6)/MPGC(1,6)) * (1+AFVADJFE(ILDV,XYR))
              FLTMPG(2,ifleet,ILDV,n) = MPGT(1,n) * (FLTMPG(2,ifleet,1,6)/MPGT(1,6)) * (1+AFVADJFE(ILDV,XYR))
            endif
          endif
        enddo
      enddo
    enddo
            
!...Calculate the average fleet mpg for cars and lt. trucks
    do IVTYP=1,maxvtyp  
      NUM = 0.0
      DEN = 0.0
      do ifleet=1,maxfleet
        do ILDV=1,maxldv
		  do ihav = 1,maxhav
            if(FLTMPG(IVTYP,ifleet,ILDV,n).ne.0.0) then
              NUM = NUM +  FLTECH(mnumcr,IVTYP,ifleet,ILDV,ihav)
              DEN = DEN + (FLTECH(mnumcr,IVTYP,ifleet,ILDV,ihav)/FLTMPG(IVTYP,ifleet,ILDV,n))
            endif
		  enddo
        enddo
      enddo
      FLTMPGTOT2(IVTYP) = 0.0
      if(DEN.ne.0.0) FLTMPGTOT2(IVTYP) = NUM/DEN
    enddo

!...Adjust vintage array of fleet stock efficiencies to account for new additions
    do ifleet=1,maxfleet
      do ILDV=1,maxldv
        CMPGFSTK(ifleet,ILDV,1,n) = FLTMPG(1,ifleet,ILDV,n)
        TMPGFSTK(ifleet,ILDV,1,n) = FLTMPG(2,ifleet,ILDV,n)
        if(curcalyr.eq.first_read_year) then
          do iage=2,maxage
            if(ILDV.eq.4.or.ILDV.eq.7.or.ILDV.eq.15)then
              CMPGFSTK(ifleet,ILDV,iage,n) = CMPGSTKGAS95(1,iage) * (FLTMPG(1,ifleet,1,6)/MPGC(1,6)) * 5.0
              TMPGFSTK(ifleet,ILDV,iage,n) = CMPGSTKGAS95(2,iage) * (FLTMPG(2,ifleet,1,6)/MPGT(1,6)) * 4.0
            else
              CMPGFSTK(ifleet,ILDV,iage,n) = CMPGSTKGAS95(1,iage) * (FLTMPG(1,ifleet,1,6)/MPGC(1,6)) * (1+AFVADJFE(ILDV,XYR))
              TMPGFSTK(ifleet,ILDV,iage,n) = CMPGSTKGAS95(2,iage) * (FLTMPG(2,ifleet,1,6)/MPGT(1,6)) * (1+AFVADJFE(ILDV,XYR))
            endif
          enddo
!...Bump each vintage up each year
        else
          do iage=2,maxage 
            CMPGFSTK(ifleet,ILDV,iage,n)=CMPGFSTK(ifleet,ILDV,iage-1,n-1)
            TMPGFSTK(ifleet,ILDV,iage,n)=TMPGFSTK(ifleet,ILDV,iage-1,n-1)
          enddo
        endif
      enddo
    enddo

!...Remove mpgs of CMPGFSTK and TMPGFSTK for zero stocks
    do ILDV=1,maxldv
	  do ifleet=1,maxfleet
	    do iage=1,maxage
          if(sum(Flt_Stock(1:mnumcr-2,1,ifleet,ILDV,iage,1,n)).eq.0.0) CMPGFSTK(ifleet,ILDV,iage,n) = 0.0
		  if(sum(Flt_Stock(1:mnumcr-2,2,ifleet,ILDV,iage,1,n)).eq.0.0) TMPGFSTK(ifleet,ILDV,iage,n) = 0.0
  	    enddo
	  enddo
    enddo
	
!...Calculate average mpg by vehicle and fleet type                             
!...Note: Weight MPG vintages by their stock (and not VMT since we assumed all  
!...vintages are driven the same annual VMT) and also apply the degradation factors    
    do IVTYP=1,maxvtyp
      do ifleet=1,maxfleet
        do ILDV=1,maxldv
          NUM = 0.0
          DEN = 0.0
          if(IVTYP.eq.1) then
            do iage=1,maxage
			  do ihav = 1,maxhav
                if(CMPGFSTK(ifleet,ILDV,iage,n).ne.0.0) then
				  NUM = NUM +  Flt_Stock(mnumcr,IVTYP,ifleet,ILDV,iage,ihav,n)
                  DEN = DEN + (Flt_Stock(mnumcr,IVTYP,ifleet,ILDV,iage,ihav,n)/(CMPGFSTK(ifleet,ILDV,iage,n)*CDFRFG(n,ihav)))
                endif
			  enddo
            enddo
          else
            do iage=1,maxage
			  do ihav = 1,maxhav
                if(TMPGFSTK(ifleet,ILDV,iage,n).ne.0.0) then
				  NUM = NUM +  Flt_Stock(mnumcr,IVTYP,ifleet,ILDV,iage,ihav,n)
                  DEN = DEN + (Flt_Stock(mnumcr,IVTYP,ifleet,ILDV,iage,ihav,n)/(TMPGFSTK(ifleet,ILDV,iage,n)*LTDFRFG(n,ihav)))				  
                endif
			  enddo
            enddo
          endif
          MPGFLTSTK(IVTYP,ifleet,ILDV) = 0.0
          if(DEN.ne.0.0) MPGFLTSTK(IVTYP,ifleet,ILDV) = NUM/DEN
        enddo
      enddo
    enddo

!...Calculate overall fleet average mpg by fuel technology (FLTTOTMPG)
    do IVTYP=1,maxvtyp
      NUM = 0.0
      DEN = 0.0
      do ifleet=1,maxfleet
        do ILDV=1,maxldv
		  do ihav = 1,maxhav
            if(MPGFLTSTK(IVTYP,ifleet,ILDV).ne.0.0) then
              NUM = NUM +  TFLTECHSTK(IVTYP,ifleet,ILDV,ihav)
              DEN = DEN + (TFLTECHSTK(IVTYP,ifleet,ILDV,ihav)/MPGFLTSTK(IVTYP,ifleet,ILDV))
            endif
		  enddo
        enddo
      enddo
      FLTTOTMPG(IVTYP) = 0.0
      if(DEN.ne.0.0) FLTTOTMPG(IVTYP) = NUM/DEN
    enddo

  RETURN
  END SUBROUTINE TFLTMPGS

! ==========================================================================================================
! ... Subroutine TFLTCONS calculates fuel consumption of fleet vehicles
! ==========================================================================================================
  SUBROUTINE TFLTCONS
  USE T_
  IMPLICIT NONE

!...Calculate fuel consumption (gallons) by ldv type
    do IVTYP=1,maxvtyp
      do ifleet=1,maxfleet
        do ILDV=1,maxldv
          FLTLDVC(IVTYP,ifleet,ILDV,n) = 0.0
          if(MPGFLTSTK(IVTYP,ifleet,ILDV).ne. 0.0) &
			FLTLDVC(IVTYP,ifleet,ILDV,n) = (sum(FLTVMTECH(IVTYP,ifleet,ILDV,1:maxhav))/1000000.0)/ &
                                             MPGFLTSTK(IVTYP,ifleet,ILDV)
        enddo
      enddo
    enddo

!...Sum fuel consumption by ldv type across fleet types and convert to Btu values
    do IVTYP=1,maxvtyp
      do ILDV=1,maxldv
        FLTFCLDVBTU(IVTYP,ILDV,n) = 0.0
        do ifleet=1,maxfleet
          FLTFCLDVBTU(IVTYP,ILDV,n) = FLTFCLDVBTU(IVTYP,ILDV,n) + &
                                      (FLTLDVC(IVTYP,ifleet,ILDV,n)* MG_HHV/1000)
        enddo
      enddo
    enddo

!...Sum fuel consumption by vehicle type
    do ILDV=1,maxldv
      FLTTLLDVBTU(ILDV,n)= 0.0
      do IVTYP=1,maxvtyp
        FLTTLLDVBTU(ILDV,n)= FLTTLLDVBTU(ILDV,n)+FLTFCLDVBTU(IVTYP,ILDV,n)
      enddo
    enddo

!...Calculate total fleet consumption by fuel type by region 
    do ifuel=1,maxfuel
      do iregn=1,mnumcr-2
!...    gasoline
        if(IFUEL.eq.1) then
          FLTFUELBTU(iregn,1,n)=( FLTTLLDVBTU( 1,n) + &                           ! Gasoline
                                 (FLTTLLDVBTU( 3,n)*(1.0-PctAF(2,iregn,n))) + &   ! Ethanol FFV
                                 (FLTTLLDVBTU( 5,n)*(1.0-PctPHEV20(iregn,n))) + & ! PHEV20
                                 (FLTTLLDVBTU( 6,n)*(1.0-PctPHEV50(iregn,n))) + & ! PHEV50 
                                 (FLTTLLDVBTU( 9,n)*(1.0-PctAF(3,iregn,n))) + &   ! CNG Bifuel
                                 (FLTTLLDVBTU(10,n)*(1.0-PctAF(4,iregn,n))) + &   ! LPG Bifuel
                                  FLTTLLDVBTU(16,n) ) * RSHR(iregn,n)             ! Hybrid Gasoline
!...    methanol
        elseif(IFUEL.eq.2) then
          FLTFUELBTU(iregn,2,n)=  FLTTLLDVBTU(13,n) * RSHR(iregn,n)               ! FCV Methanol
!...    ethanol consumption
        elseif(IFUEL.eq.3) then
          FLTFUELBTU(iregn,3,n)= (FLTTLLDVBTU( 3,n)*PctAF(2,iregn,n))*RSHR(iregn,n) ! Ethanol FFV
                                 
!...    CNG
        elseif(IFUEL.eq.4) then
          FLTFUELBTU(iregn,4,n)=((FLTTLLDVBTU( 9,n)*(PctAF(3,iregn,n))) + &       ! CNG Bifuel
                                  FLTTLLDVBTU(11,n) ) * RSHR(iregn,n)             ! CNG Dedicated 
!...    LPG
        elseif(IFUEL.eq.5) then
          FLTFUELBTU(iregn,5,n)=((FLTTLLDVBTU(10,n)*(PctAF(4,iregn,n))) + &       ! LPG Bifuel
                                  FLTTLLDVBTU(12,n) ) * RSHR(iregn,n)             ! LPG Dedicated  
!...    electricity
        elseif(IFUEL.eq.6) then
          FLTFUELBTU(iregn,6,n)=((FLTTLLDVBTU( 5,n)* PctPHEV20(iregn,n)) + &      ! PHEV20
                                 (FLTTLLDVBTU( 6,n)* PctPHEV50(iregn,n)) + &      ! PHEV50 
                                  FLTTLLDVBTU( 7,n)+ FLTTLLDVBTU( 4,n)+ FLTTLLDVBTU(15,n)) * RSHR(iregn,n)! EV 
!...    hydrogen
        elseif(IFUEL.eq.7) then
          FLTFUELBTU(iregn,7,n)=  FLTTLLDVBTU(14,n) * RSHR(IREGN,N)               ! FCV Hydrogen

!...    diesel consumption
        elseif(IFUEL.eq.8) then
          FLTFUELBTU(iregn,8,n)=( FLTTLLDVBTU( 2,n) + &                            ! Diesel
                                  FLTTLLDVBTU( 8,n) ) * RSHR(IREGN,N)              ! Hybrid Diesel
        endif
      enddo
    enddo

    do ifuel=1,maxfuel
      FLTFUELBTU(11,ifuel,n) = sum(FLTFUELBTU(1:MNUMCR-2,ifuel,n))
    enddo

  RETURN
  END SUBROUTINE TFLTCONS

! ==========================================================================================================
! ... Subroutine TVMT calculates total personal light vehicle VMT
! ==========================================================================================================
    SUBROUTINE TVMT
    USE T_
    IMPLICIT NONE

      REAL    VMTEXP(MF,AGEGRP), TotStk, VMTSum, VMTOth, CA_ADJ, VMTLD_1(AGEGRP,MNUMYR,MF)         
      integer ir           

! ... Calculate cost of driving per mile (COSTMI)
      COSTMI(n) = ((PMGTR(11,n)*CFMGQ(n)/42.0)/trldmpgf(3,n))*100.0 * MC_JPGDP(11)
	
!...Calculate employment rate for vmt growth
    EMP_RATE_VMT(n) = MC_EEA(n)/MC_NP16A(11,n)
	  
! ... Calculate vmt per licensed driver (VMTLD) for years greater than last historic year of data

      IF (curcalyr.gt.VMTLDHistYr) THEN
        VMTLD(:,n-1,:) = VMTLD_1(:,n-1,:)	    
        DO IMF=1,MF
          DO iagr=1,agegrp 
            VMTEXP(imf,iagr) = (ALPHA(imf,iagr) + BETAVMT(imf,iagr)*LOG(VMTLD(iagr,n-1,imf))+BETAINC(imf,iagr)*LOG(INC00$16(11,n))   + &
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
	  
	  
!...recalculate regional driving demand VMTLDV with projected VMT/licensed driver
!...set all to gasoline and redistribute to ILDV lower down
    do imf=1,MF
      do iagr=1,agegrp 
		do iregn=1,mnumcr-2
			VMTLDV(iagr,n,imf,iregn) = VMTLD(iagr,n,imf) * LICDRIVER(iagr,imf,iregn,n)
        enddo
      enddo
	enddo
			
 
!...adjust region 9 vmt for california GHG
!!!! dst vmt adjustment should be made directly to household, but we 
!... cant currently do so, since VMTLDV is precalculated for FE in subroutine TREG
!...in order to create regional VMT Share (RSHR). As fleets and CLT get regionalized
!... RSHR won't be needed anymore and then the adjustment can be made to household
!... vmt which is preferable
    IF (TRANAB32 .NE. 0) then
	  do imf=1,MF
	    do iagr=1,agegrp	
		  VMTLDV(iagr,n,imf,9) = VMTLDV(iagr,n,imf,9)*vmt_ca_co2(n) 
		enddo
	  enddo
	endif

!...dst calculate national vmt -can be brought into previous loop if california coefficient is moved to hh down below 
	do imf=1,MF
	  do iagr=1,agegrp
		VMTLDV(iagr,n,imf,mnumcr) = sum(VMTLDV(iagr,n,imf,1:mnumcr-2))
	  enddo
	enddo
	
!...Calculate household VMT (VMTHH) from total VMT, subtration of fleet VMT, and fuel shares calculated from stocks (VSPLDV)

!Fleet is regionally shared wtih RSHR, but can be removed as fleet vmt becomes regionalized
	do IVTYP =1,maxvtyp
	  do ILDV=1,maxldv
		do iregn=1,mnumcr-2
	    VMTHH(N,iregn,ILDV,IVTYP) = (SUM(VMTLDV(1:AGEGRP,n,1:MF,iregn))-(sum(FLTVMTECH(1:maxvtyp,1:3,1:maxldv,1:maxhav)/1000000000.0)*RSHR(IREGN,N)) &
	            - (((sum(FLTVMTECH(1:maxvtyp,4,1:maxldv,1:maxhav))/1000000000.0)*0.5)*RSHR(IREGN,N))) * VSPLDV(ILDV,n,iregn,IVTYP)

		!dst once RSHR can be removed, make california VMT adjustment to household directly
		!if(TRANAB32 .NE. 0.and.iregn.eq.9) VMTHH(N,iregn,ILDV,IVTYP) = VMTHH(N,iregn,ILDV,IVTYP)*vmt_ca_co2(n) !dst -moved AB32 adjustment to HH directly
		enddo
		VMTHH(N,mnumcr,ILDV,IVTYP) = sum(VMTHH(N,1:mnumcr-2,ILDV,IVTYP))	!national household vmt by fuel and vehicle type
	  enddo
	enddo

      PE(N) = 1 !BETACOST * (COSTMI(N) / VMTLD(11,N))
      IE(N) = 1 !BETAINC  * (INC00$16(11,N) / VMTLD(11,N))
      DE(N) = 1 !BETAVMT  * (VMTLD(11,N) / VMTLD(11,N))

! ... Calculate the hydrogen stock shares and use them as VMT shares for hydrogen vehicles.
      TotStk=0.0
      do ir=1,mnumcr-2
        TotStk=TotStk+HVStkT(1,ir)+HVStkT(2,ir)+HVStkT(3,ir)
      enddo
      do ir=1,mnumcr
        if(TotStk.gt.0.0) then
          RShrH(ir,n)=(HVStkT(1,ir)+HVStkT(2,ir)+HVStkT(3,ir))/TotStk
        endif
      enddo
! ... Assume that the overall share RShr is not going to change. But since the hydrogen
! ... shares have changed, the remaining part of RShr will change so that it all still adds up to
! ... the original RShr. Note that the shares for all the other vehicle technologies besides hydrogen
! ... are the same so I can treat them as one share. In other words, the overall share RShr is made
! ... up of only two pieces, the hydrogen piece and the everything else piece.
      do ir=1,mnumcr
! ... Sum up the weights over everything and over everything else
        VMTOth=0.0
        VMTSum=0.0
        do ILDV=1,maxldv
          if(ILDV.ne.14) VMTOth=VMTOth+sum(VMTHH(N,mnumcr,ILDV,1:maxvtyp))
          VMTSum=VMTSum+sum(VMTHH(N,mnumcr,ILDV,1:maxvtyp))
        enddo
! ... The remaining region share is the overall share and its weight minus the hydrogen share and its weight divided by the remaining weight.

!dst  - remove with rshr with further  regionalization
		RShrE(ir,n)=(RShr(ir,n)*VMTSum-RShrH(ir,n)*sum(VMTHH(N,mnumcr,14,1:maxvtyp)))/VMTOth	
		
      enddo
      if((n.eq.(mjumpyr-30).or.n.eq.(mjumpyr-20).or.n.eq.(mjumpyr-10).or.n.eq.mjumpyr) .and. fcrl.eq.1) then
       write(H2UNIT,'(a,2i5,9f8.3)') '**VSh1 ',curiyr,curitr,(RSHR(ir,n),ir=1,9)
       write(H2UNIT,'(a,2i5,9f8.3)') '**VSh2 ',curiyr,curitr,(RShrH(ir,n),ir=1,9)
       write(H2UNIT,'(a,2i5,9f8.3)') '**VSh3 ',curiyr,curitr,(RShrE(ir,n),ir=1,9)
      endif      

    RETURN
    END SUBROUTINE TVMT

! ==========================================================================================================
! ... Subroutine TMPGAG summarizes (personal and fleet) light vehicle sales and mpg by technology
! ==========================================================================================================
  SUBROUTINE TMPGAG
  USE T_
  IMPLICIT NONE

    REAL     FLTECHSALT(maxvtyp,maxfleet,maxldv),FLTMPGNEW(maxvtyp,maxldv), &
             FLTSTOCK(maxvtyp,maxldv),NUM,DEN, &
             SUMLDV1(maxvtyp),SUMLDV2(maxvtyp), &
             FLTECHSALTT(maxvtyp,maxldv),SALETECH(maxvtyp,maxldv), &
             SALETECHT(3),SHARE(maxvtyp,maxldv)

!...Sum up fleet vehicle sales across size class and HAV
    FLTECHSALT = 0.0
    do IVTYP=1,maxvtyp
      do ifleet=1,maxfleet
        do ILDV=1,maxldv
          FLTECHSALT(IVTYP,ifleet,ILDV) = sum(FLTECHSAL(mnumcr,IVTYP,ifleet,1:MAXCLASS,ILDV,1:maxhav))
        enddo
      enddo
    enddo

!...Sum up fleet vehicle sales across fleet type
    do IVTYP=1,maxvtyp
      do ILDV=1,maxldv
        FLTECHSALTT(IVTYP,ILDV) = 0.0
        do ifleet=1,maxfleet
          FLTECHSALTT(IVTYP,ILDV) = FLTECHSALTT(IVTYP,ILDV) + FLTECHSALT(IVTYP,ifleet,ILDV)
        enddo
      enddo
    enddo

!...Calculate new fleet vehicle mpg
    do IVTYP=1,maxvtyp
      do ILDV=1,maxldv
        NUM = 0.0
        DEN = 0.0
        do ifleet=1,maxfleet
          if(FLTMPG(IVTYP,ifleet,ILDV,n).ne. 0.0) then
             NUM = NUM +  FLTECHSALT(IVTYP,ifleet,ILDV)
             DEN = DEN + (FLTECHSALT(IVTYP,ifleet,ILDV)/FLTMPG(IVTYP,ifleet,ILDV,n))
          endif
        enddo
        FLTMPGNEW(IVTYP,ILDV) = 0.0
        if(DEN.ne.0.0) FLTMPGNEW(IVTYP,ILDV) = NUM/DEN
      enddo
    enddo

!...Calculate average mpg for cars and light trucks by technology (Table 50) 
!...Combine fleet and non-fleet cars and fleet and non-fleet light trucks    
    do ILDV=1,maxldv
	
      CCMPGLDV(ILDV,n) = MPGC(ILDV,n)
      TTMPGLDV(ILDV,n) = MPGT(ILDV,n)
!...  Car
      if(FLTMPGNEW(1,ILDV).gt.0.0.and.FLTECHSALTT(1,ILDV).gt.0.0001.and.MPGC(ILDV,n).ne.0.0) then                    
         CCMPGLDV(ILDV,n) = 1.0/((TECHNCS(ILDV,n) * 1000000.0/MPGC(ILDV,n) + &
                                  FLTECHSALTT(1,ILDV)/FLTMPGNEW(1,ILDV)) /   &
                                 (TECHNCS(ILDV,n) * 1000000.0 + FLTECHSALTT(1,ILDV)))
	  endif	
!...  Light Truck
      if(FLTMPGNEW(2,ILDV).gt.0.0.and.FLTECHSALTT(2,ILDV).gt.0.0001.and.MPGT(ILDV,n).ne. 0.0) then                    ! Light truck
         TTMPGLDV(ILDV,n) = 1.0/((TECHNLT(ILDV,n) * 1000000.0/MPGT(ILDV,n) + &
                                  FLTECHSALTT(2,ILDV)/FLTMPGNEW(2,ILDV))/   &
                                 (TECHNLT(ILDV,n) * 1000000.0 + FLTECHSALTT(2,ILDV)))

	  endif 
    enddo

!...Calculate total sales (personal + fleet)
    do IVTYP=1,maxvtyp
      SALETECHT(IVTYP) = 0.0
      do ILDV=1,maxldv
        SALETECH(1,ILDV) = TECHNCS(ILDV,n) * 1000000 + FLTECHSALTT(1,ILDV)     ! Cars 
        SALETECH(2,ILDV) = TECHNLT(ILDV,n) * 1000000 + FLTECHSALTT(2,ILDV)     ! Light trucks
        SALETECHT(IVTYP)  = SALETECHT(IVTYP) + SALETECH(IVTYP,ILDV)
      enddo
    enddo

!...Calculate sales shares for each technology within cars and l.t. and sum up  
!...denominator of new car and lt mpg harmonically                              
    do IVTYP=1,maxvtyp
      AltTrueMPG(IVTYP,n) = 0.0
      do ILDV=1,maxldv
        SHARE(IVTYP,ILDV) = 0.0
        if(SALETECHT(IVTYP).ne.0.0) SHARE(IVTYP,ILDV)=SALETECH(IVTYP,ILDV)/SALETECHT(IVTYP)
        if(IVTYP.eq.1) then
          if(CCMPGLDV(ILDV,n).ne.0.0) AltTrueMPG(IVTYP,n)=AltTrueMPG(IVTYP,n)+(SHARE(IVTYP,ILDV)/ &
                                                          CCMPGLDV(ILDV,n))
        else
          if(TTMPGLDV(ILDV,N).ne.0.0) AltTrueMPG(IVTYP,n)=AltTrueMPG(IVTYP,n)+(SHARE(IVTYP,ILDV) / &
                                                          TTMPGLDV(ILDV,n))
        endif
      enddo
    enddo

!...Harmonically average new car and lt mpg separately
    DO IVTYP=1,MAXVTYP
      IF (AltTrueMPG(IVTYP,N).ne.0.0) AltTrueMPG(IVTYP,N) = 1.0/AltTrueMPG(IVTYP,N)
    ENDDO

!...Harmonically average combined new car and lt mpg into ldv mpg         
!...Note: SALETECHT(3,N) is the combined ldv sales and TRUEMPG(3,N) is the 
!...combined ldv mpg                                                      

    SALETECHT(3) = SALETECHT(1) + SALETECHT(2)

    AltTrueMPG(3,n) = 0.0
    if(SALETECHT(3).ne.0.0.and.AltTrueMPG(1,n).ne.0.0.and.AltTrueMPG(2,n).ne.0.0) &
      AltTrueMPG(3,n) = 1.0/(((SALETECHT(1)/SALETECHT(3))/AltTrueMPG(1,n)) + &
                          ((SALETECHT(2)/SALETECHT(3))/AltTrueMPG(2,n)))

    TrueMPG(3,n) = 0.0
    if(SALETECHT(3).ne.0.0.and.TrueMPG(1,n).ne.0.0.and.TrueMPG(2,n).ne.0.0)  &
      TrueMPG(3,N) = 1.0/(((SALETECHT(1)/SALETECHT(3))/TrueMPG(1,n)) + &
                             ((SALETECHT(2)/SALETECHT(3))/TrueMPG(2,n)))

    NewMPG(3,n) = 0.0
    if(SALETECHT(3).ne.0.0.and.NewMPG(1,n).ne.0.0.and.NewMPG(2,n).ne.0.0)  &
      NewMPG(3,N) = 1.0/(((SALETECHT(1)/SALETECHT(3))/NewMPG(1,n)) + &
                         ((SALETECHT(2)/SALETECHT(3))/NewMPG(2,n)))
                             
    CAFESTD(3,n) = 0.0
    if(SALETECHT(3).ne.0.0.and.CAFESTD(1,n).ne.0.0.and.CAFESTD(2,n).ne.0.0)  &
      CAFESTD(3,N) = 1.0/(((SALETECHT(1)/SALETECHT(3))/CAFESTD(1,n)) + &
                          ((SALETECHT(2)/SALETECHT(3))/CAFESTD(2,n)))    

! ... Calculate fleet average stock car & light truck mpg (Table 50)
      SUMLDV1(1) = 0.0
      SUMLDV1(2) = 0.0
!...hh car and truck stock fuel economy to be weighted
!.. by VMT instead of stocks					 
      IF (SCMPG(N) .NE. 0.0 .AND. FLTTOTMPG(1) .NE. 0.0) &
        SUMLDV1(1) = (sum(VMT_STK_HH(1,1:maxldv,1:maxage,1:maxhav,1:mnumcr-2))/(SCMPG(N)*(CFMGQ(N)/CFMGQ(18)))) + &
                     (TOTFLTCAR(1)/(FLTTOTMPG(1)*(CFMGQ(N)/CFMGQ(18))))
      IF (STMPG(N) .NE. 0.0 .AND. FLTTOTMPG(2) .NE. 0.0) &
        SUMLDV1(2) = (sum(VMT_STK_HH(2,1:maxldv,1:maxage,1:maxhav,1:mnumcr-2))/(STMPG(N)* (CFMGQ(N)/CFMGQ(18)))) + &
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

! ... Calculate MPGs and sales separately for personal and fleet vehicles.
! ... Personal vehicles:
      do ILDV=1,MAXLDV
        PerMPG(1,ILDV,n)=mpgc(ILDV,n)
        PerMPG(2,ILDV,n)=mpgt(ILDV,n)
        PerSal(1,ILDV,n)=techncs(ILDV,n)
        PerSal(2,ILDV,n)=technlt(ILDV,n)
      end do
! ... Sum personal vehicles over the 16 vehicle technologies:
      do IVTYP=1,maxvtyp
        PerMPG(IVTYP,17,n)=0.0
        PerSal(IVTYP,17,n)=0.0
        do ILDV=1,MAXLDV
          if(PerMPG(IVTYP,ILDV,n).gt.0.0) then
            PerMPG(IVTYP,17,n)=PerMPG(IVTYP,17,n)+((1.0/PerMPG(IVTYP,ILDV,n))*PerSal(IVTYP,ILDV,n))
            PerSal(IVTYP,17,n)=PerSal(IVTYP,17,n)+PerSal(IVTYP,ILDV,n)
          endif
        end do
        PerMPg(IVTYP,17,n)=PerSal(IVTYP,17,n)/PerMPG(IVTYP,17,n)
      end do
! ... Write it out
      if(fcrl.eq.1) write(H2UNIT,'(a,2i4,2f10.3,2f10.3)') '**pmpg ',curiyr,curitr,PerMPG(1,17,n),PerMPG(2,17,n),PerSal(1,17,n),PerSal(2,17,n)

    RETURN
    END SUBROUTINE TMPGAG

! ==========================================================================================================
! ... Subroutine TCOMMCL_TRK: Commercial Light Truck Model - Class 2b Vehicles
! ...   8,500 TO 10,000 lbs Gross Vehicle Weight (GVW)
! ==========================================================================================================
    SUBROUTINE TCOMMCL_TRK
    USE T_
    IMPLICIT NONE

      REAL     GROWTH1,GROWTH2,G,D, NUM, DEN
	  REAL     var(9)
      integer  ifl

!...Vehicle miles traveled
!...Growth in travel is estimated as the weighted average growth of industry sector output
!...for 1) Agriculture, 2) Mining, 3) Construction, 4) Manufacturing, 5) Utilities, and
!...6) Personal travel.  Growth rates are averaged by the percent of total VMT occuring in
!...each sector.
	
    CLTSIC(N) = MC_REVIND(11,42,N)         * CLTVMTDIST(1) + &  ! agr
                SUM(MC_REVIND(11,45:47,N)) * CLTVMTDIST(2) + &  ! mining
                MC_REVIND(11,48,N)         * CLTVMTDIST(3) + &  ! constr
! total manufacturing needs to subtract out subcategories:
!   1 is food total of 2-5;
!  10 is paper total of 11-13;
!  20 is other chemical total of 21-24;
!  17 is ethanol, a subset of organic chemical;
!  29 is flat glass, a subset of glass;

               (SUM(MC_REVIND(11,1:41,N)) - &
                    SUM(MC_REVIND(11,2:5,N))-SUM(MC_REVIND(11,11:13,N))-SUM(MC_REVIND(11,21:24,N))- &
                    MC_REVIND(11,17,N)-MC_REVIND(11,29,N)) * CLTVMTDIST(4) + &
                SUM(MC_REVSER(11,3:4,N)) * CLTVMTDIST(5) + &    ! utility 
                (sum(VMTHH(N,1:mnumcr-2,1:maxldv,1:maxvtyp)) + & 
					SUM(FLTVMTECH(1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav)) / 1000000000.0) * CLTVMTDIST(6)
				
	if(CURITR.eq.1) then 	
	  CLTMPG(n,:) = 0.0

	endif
				
!------------------------------------------------------------------------------------------------------------------------------------!	
!...Distribute historical stock values across vintages and fuels
    if(CURCALYR.le.2011)then

	  if(CURITR.eq.1) then 
	    CLTSTK(n,:,:,:) = 0.0	
	    CLTVMT(n,:,:) = 0.0	  
	  endif
	
!...  Fill total stocks with values from trnstockx.xlsx
      cltstkt(1,n) = cls2bstkhist(2,n)/1000     ! gasoline
	  cltstkt(2,n) = cls2bstkhist(1,n)/1000     ! diesel	  
	  do ifuel = 3,6
	    cltstkt(ifuel,n) = cls2bstkhist(ifuel,n)/1000
	  enddo
	  
!...  Fill total sales with values from trnldvx.xlsx 
!...  Fill total sales with values from macro model, shared out
!	  by fuel using shares from trnldvx.xlsx
      do ifuel = 1,6
        CLTSTK(n,ifuel,1,11) = CLTSALT(ifuel,n)
	  enddo
!...  Total CLT New Sales
	  
!...  New vehicle 2b VMT
!	  do ifuel = 1,10
	  do ifuel = 1,9
        CLTVMT(n,ifuel,1) = CLTSTK(n,ifuel,1,11)*CLTVMTVA_H(n)
	  enddo 

!... New fuel economy by powertrain
      do ifuel = 1,6
        NCLTMPG(n,ifuel) = cltmpg_yr(n,ifuel,1)
	  enddo
!...  Sales weighted average new fuel economy (across all fuels)
	  do ifuel = 1,9
        var(ifuel) = 0.0
	    if(ncltmpg(n,ifuel).gt.0.0) var(ifuel) = (CLTSTK(n,ifuel,1,11)/SUM(CLTSTK(n,:,1,11)))/ncltmpg(n,ifuel)
      enddo
	  NCLTMPG(n,10) = 1/sum(var(1:9))
	  cltmpg_yr(n,10,1) = NCLTMPG(n,10)

!... VMT weighted average stock fuel economy
!	 Assumes same vintaged annual VMT schedule as 2012 (DOES NOT use this VMT for pre-2012 total Class 2b VMT)
	do ifuel=1,9
      NUM = 0.0
      DEN = 0.0
        do iage=1,maxage2b
            if(cltmpg_yr(n,ifuel,iage).gt.0.0) then
              NUM = NUM + cltvmt2012(iage)*CLTSTK(n,ifuel,iage,11)
              DEN = DEN + (cltvmt2012(iage)*CLTSTK(n,ifuel,iage,11))/cltmpg_yr(n,ifuel,iage)
            endif
        enddo
          if(DEN.gt.0.0) CLTMPG(n,ifuel) = NUM/DEN
    enddo
	  
!...  BTU by powertrain	
	  do ifuel=1,6
	    CLTGAL(ifuel) = 0.0
	      if(CLTMPG(n,ifuel).gt.0.0) CLTGAL(ifuel) = (CLTVMTT(ifuel,n)/CLTMPG(n,ifuel))*10.0**9
	    CLTBTUT(ifuel,n) = CLTGAL(ifuel)*MG_HHV/1000000000.0
	  enddo	
	  
      CLTGALT = sum(CLTGAL(1:6))
      CLTMPG(n,10) = (SUM(CLTVMTT(1:9,N))*10.0**9)/CLTGALT   ! average mpg over vintages	
	
	
!------------------------------------------------------------------------------------------------------------------------------------!	
	elseif(curcalyr.eq.2012) then		! First year of regionalized stocks
	
      do ifuel = 1,9
	    CLTSALT(ifuel,n) = sum(CLTSTK(n,ifuel,1,1:mnumcr-2))                ! Total sales by powertrain
	    CLTSTKT(ifuel,n) = sum(CLTSTK(n,ifuel,1:34,1:mnumcr-2))/ 1000.0     ! Total stock by powertrain
	  enddo
	  CLTSALT(10,n) = sum(CLTSALT(1:9,n))                            ! Total sales
	  CLTSTKT(10,n) = sum(CLTSTKT(1:9,n))                            ! Total stock

	do iage = 1,maxage2b
		do ifuel = 1,6
	      CLTVMTVA(n,iage) = cltvmt2012(iage)
		enddo

        do ifuel = 1,9
!... VMT for commerical light trucks by fuel type
          CLTVMT(n,ifuel,iage) = CLTSTK(n,ifuel,iage,11)*CLTVMTVA(n,iage)	  
		enddo 	  
	enddo	
	
	do ifuel = 1,9
!... New fuel economy by powertrain
      NCLTMPG(n,ifuel) = cltmpg_yr(n,ifuel,1)
	enddo
	
    do ifuel = 1,9
!... Total VMT for commerical light trucks by fuel type	  
      CLTVMTT(ifuel,n) = sum(CLTVMT(n,ifuel,1:maxage2b))/ 1000000000.0   
	enddo	
		
!... Sales weighted average new fuel economy
	do ifuel = 1,9
      var(ifuel) = 0.0
	  if(ncltmpg(n,ifuel).gt.0.0) var(ifuel) = (CLTSTK(n,ifuel,1,11)/SUM(CLTSTK(n,:,1,11)))/ncltmpg(n,ifuel)
    enddo
	NCLTMPG(n,10) = 1/sum(var(1:9))

!... VMT weighted average stock fuel economy	
	do ifuel=1,9
      NUM = 0.0
      DEN = 0.0
        do iage=1,maxage2b
            if(cltmpg_yr(n,ifuel,iage).gt.0.0) then
              NUM = NUM + CLTVMT(n,ifuel,iage)
              DEN = DEN + CLTVMT(n,ifuel,iage)/cltmpg_yr(n,ifuel,iage)
            endif
        enddo
          if(DEN.gt.0.0) CLTMPG(n,ifuel) = NUM/DEN
    enddo
	
!... BTU by powertrain	
	do ifuel=1,6
	  CLTGAL(ifuel) = 0.0
        do iage = 1,maxage2b	  
	      if(cltmpg_yr(n,ifuel,iage).gt.0.0) CLTGAL(ifuel) = CLTGAL(ifuel) + CLTVMT(n,ifuel,iage)/cltmpg_yr(n,ifuel,iage)	
        enddo		  
	  CLTBTUT(ifuel,n) = CLTGAL(ifuel)*MG_HHV/1000000000.0   
	enddo	
	
    CLTGALT = sum(CLTGAL(1:6))
    CLTMPG(n,10) = (SUM(CLTVMTT(1:9,N))*10.0**9)/CLTGALT   ! average mpg over vintages	
	
!------------------------------------------------------------------------------------------------------------------------------------!	
    elseif(curcalyr.le.stockyr) then
	
!...Calculate total Class 2b stocks by fuel
    do ifuel = 1,9
	  CLTSALT(ifuel,n) = sum(CLTSTK(n,ifuel,1,1:mnumcr-2))                ! Total sales by powertrain
	  CLTSTKT(ifuel,n) = sum(CLTSTK(n,ifuel,1:34,1:mnumcr-2))/ 1000.0     ! Total stock by powertrain
	enddo
	CLTSALT(10,n) = sum(CLTSALT(1:9,n))                            ! Total sales
	CLTSTKT(10,n) = sum(CLTSTKT(1:9,n)) 

!...VMT per vehicle by vintage 
    GROWTH2 = CLTSIC(N) / CLTSIC(N-1)
    do ifuel=1,6
      CLTVMT(n,ifuel,1:maxage2b) = CLTSTK(n,ifuel,1:maxage2b,11) * CLTVMTVA(n-1,1:maxage2b)
    enddo
    GROWTH1 = SUM(CLTVMT(n,1:9,1:maxage2b)) / SUM(CLTVMT(n-1,1:9,1:maxage2b))
!...Estimate growth in total travel
!...Adjust growth in total VMT by adjusting growth in VMT/Vehicle by vintage to
!...account for growth in vehicle stock
    CLTVMTVA(n,1:maxage2b) = CLTVMTVA(n-1,1:maxage2b) * GROWTH2/GROWTH1
!...Final Class 2b Total VMT by vintage
    do iage = 1,maxage2b 
        do ifuel=1,6
          CLTVMT(n,ifuel,iage) = CLTSTK(n,ifuel,iage,11) * CLTVMTVA(n,iage)
        enddo
	enddo
!...Total VMT for commerical light trucks (Class 2b)
!...by fuel
    do ifuel = 1,6
      CLTVMTT(ifuel,N) = SUM(CLTVMT(n,ifuel,1:maxage2b)) / 1000000000.0
	enddo

!...New fuel economy by powertrain
    do ifuel = 1,9
      NCLTMPG(n,ifuel) = cltmpg_yr(n,ifuel,1)
	enddo
!...Sales weighted average new fuel economy
	do ifuel = 1,9
      var(ifuel) = 0.0
	  if(ncltmpg(n,ifuel).gt.0.0) var(ifuel) = (CLTSTK(n,ifuel,1,11)/SUM(CLTSTK(n,:,1,11)))/ncltmpg(n,ifuel)
    enddo
	NCLTMPG(n,10) = 1/sum(var(1:9))
	
!...VMT weighted average stock fuel economy	
	do ifuel=1,9
      NUM = 0.0
      DEN = 0.0
        do iage=1,maxage2b
            if(cltmpg_yr(n,ifuel,iage).gt.0.0) then
              NUM = NUM + CLTVMT(n,ifuel,iage)
              DEN = DEN + CLTVMT(n,ifuel,iage)/cltmpg_yr(n,ifuel,iage)
            endif
        enddo
          if(DEN.gt.0.0) CLTMPG(n,ifuel) = NUM/DEN
    enddo
	
!...BTU by powertrain	
	do ifuel=1,6
	  CLTGAL(ifuel) = 0.0
        do iage = 1,maxage2b	  
	      if(cltmpg_yr(n,ifuel,iage).gt.0.0) CLTGAL(ifuel) = CLTGAL(ifuel) + CLTVMT(n,ifuel,iage)/cltmpg_yr(n,ifuel,iage)	
        enddo		  
	  CLTBTUT(ifuel,n) = CLTGAL(ifuel)*MG_HHV/1000000000.0   
	enddo	
	
    CLTGALT = sum(CLTGAL(1:6))
    CLTMPG(n,10) = (SUM(CLTVMTT(1:9,N))*10.0**9)/CLTGALT   ! average mpg over vintages	
	
	
!------------------------------------------------------------------------------------------------------------------------------------!
	else

!... Fill in values from tranfrt.f
	
!... Stock and sales	
    do ifuel = 1,9
	    CLTSALT(ifuel,n) = sum(CLTSTK(n,ifuel,1,1:mnumcr-2))                ! Total sales by powertrain
	    CLTSTKT(ifuel,n) = sum(CLTSTK(n,ifuel,1:34,1:mnumcr-2))/ 1000.0     ! Total stock by powertrain
	  enddo
	  CLTSALT(10,n) = sum(CLTSALT(1:9,n))                            ! Total sales
	  CLTSTKT(10,n) = sum(CLTSTKT(1:9,n)) 
	
!...VMT per vehicle by vintage 
      GROWTH2 = CLTSIC(N) / CLTSIC(N-1)
      do ifuel=1,9
        CLTVMT(n,ifuel,1:maxage2b) = CLTSTK(n,ifuel,1:maxage2b,11) * CLTVMTVA(n-1,1:maxage2b)
      enddo
      GROWTH1 = SUM(CLTVMT(n,1:9,1:maxage2b)) / SUM(CLTVMT(n-1,1:9,1:maxage2b))
!...Estimate growth in total travel
!...Adjust growth in total VMT by adjusting growth in VMT/Vehicle by vintage to
!...account for growth in vehicle stock
    CLTVMTVA(n,1:maxage2b) = CLTVMTVA(n-1,1:maxage2b) * GROWTH2/GROWTH1
!...Final Class 2b Total VMT by vintage
    do iage = 1,maxage2b 
        do ifuel=1,9
          CLTVMT(n,ifuel,iage) = CLTSTK(n,ifuel,iage,11) * CLTVMTVA(n,iage)
        enddo
	enddo
!...Total VMT for commerical light trucks (Class 2b)
!...by fuel
    do ifuel = 1,9
      CLTVMTT(ifuel,N) = SUM(CLTVMT(n,ifuel,1:maxage2b)) / 1000000000.0
	enddo

!... New fuel economy by powertrain
    do ifuel = 1,9
      NCLTMPG(n,ifuel) = cltmpg_yr(n,ifuel,1)
	enddo
!... Sales weighted average new fuel economy
	do ifuel = 1,9
      var(ifuel) = 0.0
	  if(ncltmpg(n,ifuel).gt.0.0) var(ifuel) = (CLTSTK(n,ifuel,1,11)/SUM(CLTSTK(n,:,1,11)))/ncltmpg(n,ifuel)
    enddo
	NCLTMPG(n,10) = 1/sum(var(1:9))
	
!... VMT weighted average stock fuel economy	
	do ifuel=1,9
      NUM = 0.0
      DEN = 0.0
        do iage=1,maxage2b
            if(cltmpg_yr(n,ifuel,iage).gt.0.0) then
              NUM = NUM + CLTVMT(n,ifuel,iage)
              DEN = DEN + CLTVMT(n,ifuel,iage)/cltmpg_yr(n,ifuel,iage)
            endif
        enddo
          if(DEN.gt.0.0) CLTMPG(n,ifuel) = NUM/DEN
    enddo
	
!... BTU by powertrain	
	do ifuel=1,9
	  CLTGAL(ifuel) = 0.0
        do iage = 1,maxage2b	  
	      if(cltmpg_yr(n,ifuel,iage).gt.0.0) CLTGAL(ifuel) = CLTGAL(ifuel) + CLTVMT(n,ifuel,iage)/cltmpg_yr(n,ifuel,iage)	
        enddo		  
	  CLTBTUT(ifuel,n) = CLTGAL(ifuel)*MG_HHV/1000000000.0   
	enddo	

    CLTGALT = sum(CLTGAL(1:9))
    CLTMPG(n,10) = (SUM(CLTVMTT(1:9,N))*10.0**9)/CLTGALT   ! average mpg over vintages	
	
    endif
	
!------------------------------------------------------------------------------------------------------------------------------------!		
!...Calculate aggregate sales weighted new clt mpg and VMT weighted clt stock average mpg
    NCLTMPGT(N) = NCLTMPG(n,10)
	CLTMPGT(N) = CLTMPG(n,10)
	
!... Filling in BTU by fuel variable
    do ifuel = 1,9
        if(CLTMPG(n,ifuel).eq.0.0) CLTMPG(n,ifuel) = 1.0
    enddo
		
    do iregn=1,mnumcr
        do ifuel = 1,7
! ... Calculate gasoline consumption
            if(ifuel.eq.1) then 
		      cltfbtu(n,1,iregn) = ((CLTVMTT(1,N)/CLTMPG(n,1))*MG_HHV)*RSHR(iregn,n) + &
		            ((1.0 - PCTAF(2,iregn,n))*(CLTVMTT(5,n)/CLTMPG(n,5))*MG_HHV)*RSHR(iregn,n) + &	
				    ((1.0 - PctPHEV_HDV)*(CLTVMTT(8,n)/CLTMPG(n,8))*MG_HHV)*RSHR(iregn,n)
! ... Calculate diesel consumption
            elseif(ifuel.eq.2) then 
		      cltfbtu(n,2,iregn) = ((CLTVMTT(2,N)/CLTMPG(n,2))*MG_HHV)*RSHR(iregn,n) + &			
				    ((1.0 - PctPHEV_HDV)*(CLTVMTT(7,n)/CLTMPG(n,7))*MG_HHV)*RSHR(iregn,n)
! ... Calculate LPG consumption
            elseif(ifuel.eq.3) then
              cltfbtu(n,3,iregn) = (CLTVMTT(3,n)/CLTMPG(n,3))*MG_HHV*RSHR(iregn,n)
! ... Calculate CNG consumption
            elseif(ifuel.eq.4) then
              cltfbtu(n,4,iregn) = (CLTVMTT(4,n)/CLTMPG(n,4))*MG_HHV*RSHR(iregn,n)
! ... Calculate ethanol consumption
            elseif(ifuel.eq.5) then
              cltfbtu(n,5,iregn) = (PCTAF(2,IREGN,N)*(CLTVMTT(5,n)/CLTMPG(n,5))*MG_HHV)*RSHR(iregn,n)
! ... Calculate electric consumption
            elseif(ifuel.eq.6) then
			  cltfbtu(n,6,iregn) = (CLTVMTT(6,n)/CLTMPG(n,6))*MG_HHV*RSHR(iregn,n) + &
			    (PctPHEV_HDV*(CLTVMTT(7,n)/CLTMPG(n,7))*MG_HHV)*RSHR(iregn,n) + & 
			    (PctPHEV_HDV*(CLTVMTT(8,n)/CLTMPG(n,8))*MG_HHV)*RSHR(iregn,n)
! ... Calculate hydrogen consumption
            elseif(ifuel.eq.7) then
              cltfbtu(n,7,iregn) = (CLTVMTT(9,n)/CLTMPG(n,9))*MG_HHV*RSHR(iregn,n)
            endif
		enddo
	  cltfbtu(n,10,iregn) = sum(cltfbtu(n,1:7,iregn))
	enddo
		
  RETURN
  END SUBROUTINE TCOMMCL_TRK

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
    INTEGER :: ISIC		       	     !...industrial sector subscript
    INTEGER, PARAMETER :: SIC = 16         !...industrial sectors that move domestic marine commodities:
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
      FUEL_PRICE(2,n)=PGLTRRAIL(1,US,n) * 1.115         !...LNG fuel price (1990$/mmbtu)
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
                BETALUB,BUSSYSEF(3),BOATS,NONFARMEMP(MNUMCR-2,MNUMYR),PMGTR19$(MNUMYR),TBPMTPCEXP(MNUMCR-2,MNUMYR), &
				CREFF_ADJ(mnumcr-2), TREFF_ADJ(mnumcr-1), TBBTUPM_ADJ(mnumcr-1)

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

! ... Calculate fuel cost in 2004 dollars per gallon
      DO IREGN=1,MNUMCR
         PMGTR04$(IREGN,N) = (PMGTR(IREGN,N)*MG_HHV/1000.0) * MC_JPGDP(15) !regional gasoline cost
         PDSTR04$(IREGN,N) = (PDSTR(IREGN,N)*5.79/42.0) * MC_JPGDP(15)  !regional diesel cost
      ENDDO
!...sum non-farm employs by CD
    nonfarmemp=0.0
    Do iregn=1,mnumcr-2
	   nonfarmemp(iregn,n) = sum(mc_empna(iregn,1:39,n))-sum(mc_empna(iregn,20:21,n))
	enddo   
!...gasoline price 2019$
    PMGTR19$(N) = PMGTR(11,N)*MG_HHV/1000.0 * MC_JPGDP(30)   
	  
! ... Transit rail 
    DO IREGN=1,MNUMCR-2
      if(curcalyr.le.TRHISTYEAR) then 
		TRRPM(iregn,n) = TRRPMHIST(iregn,n)
        TREFF(iregn,n) = TREFFHIST(iregn,n)
        TRED(iregn,n)  = TREDHIST(iregn,n)
      else
		if(tred(iregn,n-1).gt.0.0) then
          TRRPMPCEXP(iregn,n) = tr_coef(iregn,1)+(log(MC_GDPR(n)/MC_NP16A(11,n))*tr_coef(iregn,2)+log(pmgtr19$(n))*tr_coef(iregn,3)+trcovid(n)*tr_coef(iregn,4))
		  TRRPMPC(iregn,n) = exp(TRRPMPCEXP(iregn,n))
		! calculate transit rail efficiency -> assumes efficiency improves to histric value as pmt recovers
	      TRRPM(iregn,n)=TRRPMPC(iregn,n)*nonfarmemp(iregn,n)
		  TREFF_ADJ(iregn) = treffhist(iregn,trhistyear-1989) - treffhist(iregn,trhistyear-1990)
		  if(trrpm(iregn,n).lt.trrpm(iregn,trhistyear-1990)) then
            TREFF(IREGN,N) = TREFFHIST(IREGN,TRHISTYEAR-1989) - (treff_adj(iregn) * (trrpm(iregn,n)/trrpm(iregn,trhistyear-1990))) 
		  else
		    treff(iregn,n) = treff(iregn,trhistyear-1990)
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
          CRRPMPCEXP(iregn,n) = cr_coef(iregn,1)+(log(MC_GDPR(n)/MC_NP16A(11,n))*cr_coef(iregn,2)+log(pmgtr19$(n))*cr_coef(iregn,3)+crcovid(n)*cr_coef(iregn,4))
		  CRRPMPC(iregn,n) = exp(CRRPMPCEXP(iregn,n))
	      CRRPM(iregn,n)=CRRPMPC(iregn,n)*nonfarmemp(iregn,n)
		  CREFF_ADJ(iregn) = creffhist(iregn,crhistyear-1989) - creffhist(iregn,crhistyear-1990) 
		  if(crrpm(iregn,n).lt.crrpm(iregn,crhistyear-1990)) then
            CREFF(IREGN,N) = CREFFHIST(IREGN,CRHISTYEAR-1989) - (creff_adj(iregn) * (crrpm(iregn,n)/crrpm(iregn,crhistyear-1990))) !assumes efficiency improves to histric value as pmt recovers
		  else
		    creff(iregn,n) = creff(iregn,crhistyear-1990)
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
		IRPMPC(n) = IRPMPC1(n)*(1-(0.5*crcovid(n)))
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
        TBPMTPCEXP(iregn,n) =(tb_coef(iregn,1)+(log(MC_GDPR(n)/MC_NP16A(11,n))*tb_coef(iregn,2)+log(pmgtr19$(n))*tb_coef(iregn,3)+tbcovid(n)*tb_coef(iregn,4)))
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
      if(curcalyr.le.TBHISTYEAR)then
        do ifuelx=1,8 
          TBFSHR(iregn,ifuelx,n) = TBFSHRHIST(ifuelx,n,iregn)
        enddo
!...maintain all fuels except diesel and electricity at historic levels
!...electricity increase reduces diesel share based on previous year ratio of electricty/diesel
!...electricity grows at a faster rate in region 9 for california

!...if increase in electricy share reduces diesel below 5 percent, then maintain diesel share at 10%,
!...except for region 9 which is allowed to drop to 5%

      else !projection starts
        do ifuelx=8,1,-1
		  if(ifuelx.eq.8) TBFSHR(iregn,ifuelx,n) = TBFSHR(iregn,ifuelx,n-1) !hydrogen
		  if(ifuelx.eq.7) then
			if(iregn.eq.9)then
				TBFSHR(iregn,ifuelx,n) = TBFSHR(iregn,ifuelx,n-1) + (0.035*TBFSHR(iregn,ifuelx,n-1)/TBFSHR(iregn,2,n-1)) 
			else
				TBFSHR(iregn,ifuelx,n) = TBFSHR(iregn,ifuelx,n-1) + (0.01*TBFSHR(iregn,ifuelx,n-1)/TBFSHR(iregn,2,n-1))
			endif
		  endif !electricity	  
		  if(ifuelx.eq.6) TBFSHR(iregn,ifuelx,n) = TBFSHR(iregn,ifuelx,n-1) !LPG
		  if(ifuelx.eq.5) TBFSHR(iregn,ifuelx,n) = TBFSHR(iregn,ifuelx,n-1) !CNG
		  if(ifuelx.eq.4) TBFSHR(iregn,ifuelx,n) = 0 !methanol
		  if(ifuelx.eq.3) TBFSHR(iregn,ifuelx,n) = TBFSHR(iregn,ifuelx,n-1) !ethanol
          if(ifuelx.eq.2) &
            TBFSHR(iregn,ifuelx,n) = TBFSHR(iregn,ifuelx,n-1) - TBFSHR(iregn,7,n) + TBFSHR(iregn,7,n-1) 
			!diesel dependent on electricity		  	
          if(ifuelx.eq.1) TBFSHR(iregn,ifuelx,n) = TBFSHR(iregn,ifuelx,n-1)  !gasoline
        enddo 	
		
		!check and adjust for diesel dropping below minimum
		if(iregn.eq.9)then
			if(TBFSHR(iregn,2,n).lt.0.05)then
			TBFSHR(iregn,2,n) = 0.05 !region 9 bottons diesel at 5%	  
			TBFSHR(iregn,7,n) = 0.95 - TBFSHR(iregn,1,n) - TBFSHR(iregn,3,n) - TBFSHR(iregn,4,n) &
				- TBFSHR(iregn,5,n) - TBFSHR(iregn,6,n) - TBFSHR(iregn,8,n) !Electricity is then what remains
			endif
		else !other regions bottom diesel out at 10%
			if(TBFSHR(iregn,2,n).lt.0.10)then		
				TBFSHR(iregn,2,n) = 0.10  
				TBFSHR(iregn,7,n) = 0.90 - TBFSHR(iregn,1,n) - TBFSHR(iregn,3,n) - TBFSHR(iregn,4,n) &
					- TBFSHR(iregn,5,n) - TBFSHR(iregn,6,n) - TBFSHR(iregn,8,n) !Electricity is then what remains
			endif
		endif
      endif
    enddo
		
!...check validity of parens on TBBTUPM equation - JDM
!...transit bus Btu/passenger mile traveled
    do iregn=1,mnumcr-2
      if(curcalyr.le.TBHISTYEAR)then
        TBBTUPM(iregn,n)=TBBTUPMHIST(iregn,n)
      else
		TBBTUPM_ADJ(iregn) = tbbtupm(iregn,tbhistyear-1989) - tbbtupm(iregn,tbhistyear-1990)
		if(tbpmt(iregn,n).lt.tbpmt(iregn,tbhistyear-1990)) then
          Tbbtupm(IREGN,N) = Tbbtupmhist(IREGN,TbHISTYEAR-1989) - (tbbtupm_adj(iregn) * (tbpmt(iregn,n)/tbpmt(iregn,tbhistyear-1990))) 
		else
		  TBBTUPM(iregn,n)=TBBTUPM(iregn,tbhistyear-1990)*TBSYSEFF(iregn)* &
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
	    if(im.eq.1) TMOD(im,n) = (365.7535+(0.2519 * (MC_GDPR(n)/MC_NP16A(11,n)))) * MC_NP16A(11,n) * (1.-(0.375 * TMCOVID(im,n))) !intercity gorws with adult population
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
   	endif

!...regional share by fuel 
    if(curcalyr.ge.2014)then ! account for EV adjustment
	  do iregn=1,mnumcr-2
	    do ifuelx=1,8
	      if(ifuelx.eq.7) schbus_pmt_shr(ifuelx,iregn,n) = schbus_ev_shr(iregn,n)
		enddo  
!...    normalize shares to 1: subtract cng and ev from diesel
		if(curcalyr.le.IBSBHISTYEAR) then
		  schbus_pmt_shr(2,iregn,n) = schbus_pmt_shr(2,iregn,n)-schbus_pmt_shr(7,iregn,n)
		else
          schbus_pmt_shr(2,iregn,n) = schbus_pmt_shr(2,iregn,n)-schbus_pmt_shr(7,iregn,n)-schbus_pmt_shr(5,iregn,n)		
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
	  HYWAY(N) = sum(VMTHH(N,mnumcr,1:maxldv,1:maxvtyp)) + FTVMT(N) + sum(FLTVMTECH(1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav))

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
! duty vehicles by fuel type
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

      DO ILDV=1,MAXLDV
        IF (MPGTECH(ILDV,N) .EQ. 0.0) MPGTECH(ILDV,N) = 1.0
      ENDDO

!	  Calculate consumption by fuel
      DO IFUELX=1,9-1
        DO IREGN=1,MNUMCR
!		  Gasoline
          IF (IFUELX .EQ. 1) THEN
            TQLDV(1,IREGN,N) = 	(sum(VMTHH(N,iregn, 1,1:maxvtyp))/MPGTECH( 1,N)) * MG_HHV + &
								(sum(VMTHH(N,iregn, 5,1:maxvtyp))/MPGTECH( 5,N)) * MG_HHV * (1.0-PctPHEV20(iregn,n)) + &
								(sum(VMTHH(N,iregn, 6,1:maxvtyp))/MPGTECH( 6,N)) * MG_HHV * (1.0-PctPHEV50(IREGN,N)) + &
								(sum(VMTHH(N,iregn, 3,1:maxvtyp))/MPGTECH( 3,N)) * MG_HHV * (1.0-PCTAF(2,IREGN,N))   + &
								(sum(VMTHH(N,iregn,10,1:maxvtyp))/MPGTECH(10,N)) * MG_HHV * (1.0-PCTAF(4,IREGN,N))   + &
								(sum(VMTHH(N,iregn, 9,1:maxvtyp))/MPGTECH( 9,N)) * MG_HHV * (1.0-PCTAF(3,IREGN,N))   + &
								(sum(VMTHH(N,iregn,16,1:maxvtyp))/MPGTECH(16,N)) * MG_HHV
!		  Methanol
          ELSEIF (IFUELX .EQ. 2) THEN
            TQLDV(2,IREGN,N) = 	(sum(VMTHH(N,iregn,13,1:maxvtyp))/MPGTECH(13,N)) * MG_HHV
!		  Ethanol
          ELSEIF (IFUELX .EQ. 3) THEN
            TQLDV(3,IREGN,N) =  (sum(VMTHH(N,iregn, 3,1:maxvtyp))/MPGTECH( 3,N)) * MG_HHV * PCTAF(2,IREGN,N)
!		  CNG
          ELSEIF (IFUELX .EQ. 4) THEN
            TQLDV(4,IREGN,N) =  (sum(VMTHH(N,iregn,11,1:maxvtyp))/MPGTECH(11,N)) * MG_HHV + &
                                (sum(VMTHH(N,iregn, 9,1:maxvtyp))/MPGTECH( 9,N)) * MG_HHV * PCTAF(3,IREGN,N)
!		  LPG
          ELSEIF (IFUELX .EQ. 5) THEN
            TQLDV(5,IREGN,N) =  (sum(VMTHH(N,iregn,12,1:maxvtyp))/MPGTECH(12,N)) * MG_HHV + &
                                (sum(VMTHH(N,iregn,10,1:maxvtyp))/MPGTECH(10,N)) * MG_HHV * PCTAF(4,IREGN,N)
!		  Electricity
          ELSEIF (IFUELX .EQ. 6) THEN
            TQLDV(6,IREGN,N) = 	(sum(VMTHH(N,iregn, 4,1:maxvtyp))/MPGTECH(4,N))  * MG_HHV + & 
								(sum(VMTHH(N,iregn, 7,1:maxvtyp))/MPGTECH(7,N))  * MG_HHV + &
								(sum(VMTHH(N,iregn,15,1:maxvtyp))/MPGTECH(15,N)) * MG_HHV + &
								(sum(VMTHH(N,iregn, 5,1:maxvtyp))/MPGTECH(5,N))  * MG_HHV * PctPHEV20(iregn,n) + &
								(sum(VMTHH(N,iregn, 6,1:maxvtyp))/MPGTECH(6,N))  * MG_HHV * PctPHEV50(iregn,n)
!		  Hydrogen
          ELSEIF (IFUELX .EQ. 7) THEN
            TQLDV(7,IREGN,N) = 	(sum(VMTHH(N,iregn,14,1:maxvtyp))/MPGTECH(14,N)) * MG_HHV

!		  Diesel
          ELSEIF (IFUELX .EQ. 8) THEN
            TQLDV(8,IREGN,N) = 	(sum(VMTHH(N,iregn, 2,1:maxvtyp))/MPGTECH( 2,N)) * MG_HHV + &
								(sum(VMTHH(N,iregn, 8,1:maxvtyp))/MPGTECH( 8,N)) * MG_HHV

          ENDIF
        ENDDO
      ENDDO

! ... Sum total consumption of all fuels into TQLDV(9)
      DO IREGN=1,MNUMCR
        TQLDV(9,IREGN,N) = SUM(TQLDV(1:8,IREGN,N))
      ENDDO

! ... HMS - Calculate market segment consumption and write a debug report.
! ... For the hydrogen market segment project, use the previously calculated stocks by market segment
! ... and vintage to share out consumption to market segments. Note that the benchmarking factor
! ... (calculated later) is set to 1.0 for hydrogen so these do not need to be benchmarked.
      do iregn=1,mnumcr
        qh1tr(iregn,n)=tqldv(7,iregn,n)*HVStkS(1,iregn,n)
        qh2tr(iregn,n)=tqldv(7,iregn,n)*HVStkS(2,iregn,n)
        qh3tr(iregn,n)=tqldv(7,iregn,n)*HVStkS(3,iregn,n)
      enddo
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
               PCTFUEL$(4,MNUMCR,MNUMYR),PCTFAVL(4,MNUMCR,MNUMYR),ETHPRR
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
            PCTFUEL$(IFUELX,IREGN,N) = PMETR(IREGN,N)  ! methanol
          ELSEIF(IFUELX .EQ. 2)THEN
            PCTFUEL$(IFUELX,IREGN,N) = PETTR(IREGN,N)  ! ethanol
          ELSEIF(IFUELX .EQ. 3) THEN
            PCTFUEL$(IFUELX,IREGN,N) = PGFTRPV(IREGN,N)! CNG non-central fuel price
          ELSEIF(IFUELX .EQ. 4) THEN
            PCTFUEL$(IFUELX,IREGN,N) = PLGTR(IREGN,N)  ! LPG
          ENDIF
        ENDDO
      ENDDO

! ... Calculate regional price ratio for minimum alternative fuel use
      DO IFUELX=1,4
        DO IREGN=1,MNUMCR-2
          PRIRATIO(IFUELX,IREGN) = (PCTFUEL$(IFUELX,IREGN,N)**GAMMAP) / &
                                   ((PCTFUEL$(IFUELX,IREGN,N)**GAMMAP) + (PMGTR(IREGN,N)**GAMMAP))
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
                PCTFAVL(IFUELX,IREGN,N) = INITSTA(3,1,iregn)/INITSTA(1,1,iregn)  ! ethanol
              ELSE
                PCTFAVL(IFUELX,IREGN,N) = INITSTA(3,N-5,iregn)/INITSTA(1,N-5,iregn)  ! ethanol
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
           ALTPRR(IFUELX,IREGN) = DEXP(DBLE(FClogit0(iregn))+(DBLE(PCTFUEL$(IFUELX,IREGN,N)* MG_HHV/1000)*DBLE(FCLOGIT1))- &
                          DBLE(FCLOGIT2)*(DEXP(DBLE(PCTFAVL(IFUELX,IREGN,N))*DBLE(FCLOGIT3))))
           GASPRR(IREGN) = DEXP(DBLE(PMGTR(IREGN,N)* MG_HHV/1000)*DBLE(FCLogit4))          
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

! ... Calculate the MPG weighted VMT shares for PHEVS. Since the MPG for the gasoline engine and the electric
! ... motor are very different we need to weight the VMT shares with the MPGs.
! ... The values from user input are the electric fraction of the total VMT (PHEV20_EVMT) and the
! ... ratio of the electric MPG to the gasoline MPG (EG_MPG). The inputs are national level, but we calculate
! ... the result at a regional level to be consistent with the others (all the regions have the same value).
! ... Calculate the ratio of the total MPG to the gasoline MPG, TG_MPG.

! ... So, for example, if PHEV VMT is split out as follows: 20% electric, 80% gasoline, but the electric mpg is twice as high
!	  as the gasoline mpg, only 11% of the total PHEV energy will be electric, while 89% will be gasoline.
!	  This is important when benchmarking -- we adjust total VMT by powertrain to benchmark, and the PHEV VMT needs to be adjusted
!	  based on both BEN_EL and BEN_MG (weighted by the share of PHEV energy that is electricity and gasoline, respectively)

	  total_mkt20=0.0
	  total_mkt50=0.0
      do IGP=1,MAXGROUP
	    do ICL=1,MAXCLASS
   	      if(mfg_eg_mpg20(ICL,IGP,n).ne.0.)then 								! Ratio of electric mpg to hybrid (gasoline) mpg, from PHEVCALC subroutines
		    tot_eg_mpg20(ICL,IGP,n)=mfg_eg_mpg20(ICL,IGP,n)*pergrp(IGP,ICL,n)	
			total_mkt20=total_mkt20+pergrp(IGP,ICL,n)
		  endif
		  if(mfg_eg_mpg50(ICL,IGP,n).ne.0.)then
   		    tot_eg_mpg50(ICL,IGP,n)=mfg_eg_mpg50(ICL,IGP,n)*pergrp(IGP,ICL,n)
			total_mkt50=total_mkt50+pergrp(IGP,ICL,n)
		  endif
		enddo
	  enddo
	  
	  if(sum(mfg_eg_mpg20(1:MAXCLASS,1:MAXGROUP,n)).gt.0.0.and.total_mkt20.gt.0.0) EG_MPG20(n)=sum(tot_eg_mpg20(1:MAXCLASS,1:MAXGROUP,n))/total_mkt20

	  if(sum(mfg_eg_mpg50(1:MAXCLASS,1:MAXGROUP,n)).gt.0.0.and.total_mkt50.gt.0.0) EG_MPG50(n)=sum(tot_eg_mpg50(1:MAXCLASS,1:MAXGROUP,n))/total_mkt50	  
	  
      TG_MPG20(n)=EG_MPG20(n)/(PHEV20_EVMT+EG_MPG20(n)*(1.0-PHEV20_EVMT))
! ... Calculate the ratio of the total MPG to the electric MPG, TE_MPG.
      TE_MPG20(n)=TG_MPG20(n)/EG_MPG20(n)
! ... Using that result, calculate the electric VMT adjusted for MPG, PctPHEV20.
      do iregn=1,mnumcr
        PctPHEV20(iregn,n)=PHEV20_EVMT*TE_MPG20(n)
      end do
! ... Calculate the ratio of the total MPG to the gasoline MPG, TG_MPG.
      TG_MPG50(n)=EG_MPG50(n)/(PHEV50_EVMT+EG_MPG50(n)*(1.0-PHEV50_EVMT))
! ... Calculate the ratio of the total MPG to the electric MPG, TE_MPG.
      TE_MPG50(n)=TG_MPG50(n)/EG_MPG50(n)
! ... Using that result, calculate the electric VMT adjusted for MPG, PctPHEV50.
      do iregn=1,mnumcr
        PctPHEV50(iregn,n)=PHEV50_EVMT*TE_MPG50(n)
      end do
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
        QMGTR(IREGN,N) = TQLDV(1,IREGN,N)      + &                         ! ldv
                         FLTFUELBTU(iregn,1,n) + &                         ! ldv fleet 
                         cltfbtu(n,1,iregn)    + &                         ! commercial light truck						 
                         SUM(TFRBTU_F_T(N,1:3,2,IREGN)) + &                ! heavy truck
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
      IF (EXH .EQ.1) THEN
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
      ELSE
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
                         SUM(TFRBTU_F_T(N,1:3,4,IREGN)) + &                ! heavy truck
                        (TQLDV(7,IREGN,N)/0.7) + &                         ! Add hydrogen FCVs to NG if HMM is off
                        (FLTFUELBTU(iregn,7,n)/0.7)                        ! Add hydrogen fleet FCVs to NG if HMM is off 
      ENDIF
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
        QELTR(IREGN,N) = TQLDV(6,IREGN,N)      + &                         ! ldv
                         FLTFUELBTU(iregn,6,n) + &                         ! ldv fleet
                         QMTRR(2,IREGN,N)      + &                         ! passenger rail
                         QMTBR(1,7,IREGN,N)    + &                         ! transit bus
						 QMTBR(3,7,IREGN,N)    + &						   ! school bus
						 cltfbtu(n,6,iregn)    + &                         ! commercial light truck
						 SUM(TFRBTU_F_T(N,1:3,6,IREGN))                    ! heavy truck
! ... hydrogen
        QHYTR(IREGN,N) = TQLDV(7,IREGN,N)      + &                         ! ldv
                         FLTFUELBTU(iregn,7,n) + &                         ! ldv fleet    
                         cltfbtu(n,7,iregn)    + &                         ! commercial light truck
                         QMTBR(1,8,IREGN,N)    + &                         ! transit bus
                         SUM(TFRBTU_F_T(N,1:3,6,IREGN))                    ! heavy truck						 
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
      if(iregn.ne.10) E10SHARE(iregn,n) = (QRECR(1,iregn,n)/(MG_HHV*1000)*1000000.0)  & ! recreational boats
                                         /((QMGTR(iregn,n)/(MG_HHV*1000))*1000000.0)     ! total transportation motor gasoline

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
    tdstcpus=dstcpus
    trftcbus=rftcbus

  RETURN
  END SUBROUTINE TRANEASY
	
! ==========================================================================================================
! ... Subroutine TBENCHMARK benchmarks short term projections (2 year) to SEDS and the STEO 
! ==========================================================================================================
  SUBROUTINE TBENCHMARK
  USE T_
  IMPLICIT NONE

    INTEGER STEOBM,ifl
    integer imer,icheck
    integer isteo
    integer yseds      !  year index for last seds year
    REAL       SUMCAR,SUMLTT,BMFAC
    real   bm_mer(11,4) ! Benchmark factors for last history year from the MER.
    real   bm_steo(11,4) ! Benchmark factors for STEO year
	real   QELTR_B(mnumcr,mnumyr), QNGTR_B(mnumcr,mnumyr), QMGTR_B(mnumcr,mnumyr), QJFTR_B(mnumcr,mnumyr), QDSTR_B(mnumcr,mnumyr), QLGTR_B(mnumcr,mnumyr), QPRTR_B(mnumcr,mnumyr) 
    real   QRSTR_B(mnumcr,mnumyr), QOTTR_B(mnumcr,mnumyr), QMETR_B(mnumcr,mnumyr), QETTR_B(mnumcr,mnumyr), QHYTR_B(mnumcr,mnumyr), QAGTR_B(mnumcr,mnumyr), QLUTR_B(mnumcr,mnumyr)
	real   BEN_ME, BEN_ET, BEN_NG, BEN_LG, BEN_EL, BEN_HY
	REAL   SEDS_regn_per(mnumcr,4),  SEDS_regn(mnumcr,4), STMGTR_new(mnumcr,mnumyr), STJFTR_new(mnumcr,mnumyr), STDSTR_new(mnumcr,mnumyr), STRSTR_new(mnumcr,mnumyr) 
	
	
!...Benchmark factors are calculated using SEDS data for         
    yseds=baseyr+msedyr-1  ! calendar year for last SEDS data
    imer=ymer-baseyr+1    !  NEMS year index for final MER year. NEMS baseyr=1990
    isteo=ysteo-baseyr+1  !  NEMS year index for final STEO year.

    STEOBM = RTOVALUE('STEOBM  ',0)
    call traneasy
    do iregn=1,mnumcr    

!...  Benchmark to SEDS 
      if(CURCALYR.le.yseds)then 
        BENEL(iregn,n) = 1.0
        BENNG(iregn,n) = 1.0
        IF (QMGTR(iregn,n) .NE. 0.0) BENMG(iregn,n) = QSMGTR(iregn,n) / QMGTR(iregn,n)
        IF (QJFTR(iregn,n) .NE. 0.0) BENJF(iregn,n) = QSJFTR(iregn,n) / QJFTR(iregn,n) 
        IF (QDSTR(iregn,n) .NE. 0.0) BENDS(iregn,n) = QSDSTR(iregn,n) / QDSTR(iregn,n)
        IF (QRSTR(iregn,n) .NE. 0.0) BENRS(iregn,n) = QSRSTR(iregn,n) / QRSTR(iregn,n)
        BENLG(iregn,N) = 1.0
        BENOT(iregn,n) = 1.0
        BENME(iregn,n) = 1.0
        BENET(iregn,n) = 1.0
        BENHY(iregn,n) = 1.0		
		
! Initialize Balance Sector variables during SEDS period
        QMGBS(iregn,n)=0.
        QDSBS(iregn,n)=0.
        QJFBS(iregn,n)=0.

! Determining SEDS consumption by region and fuel based on MER annual values		
		if(curcalyr.eq.yseds) then
			do ifuel = 1,4
			    SEDS_regn_per(iregn,ifuel) = SEDS_tran(iregn,ifuel)/SEDS_tran(mnumcr,ifuel)
			    SEDS_regn(iregn,ifuel) = SEDS_regn_per(iregn,ifuel)*MER_tran(ifuel)
                if(iregn.eq.10) SEDS_regn(iregn,ifuel) = 0.0				
		    enddo	
		endif

!...  Benchmark to MER, STEO
      elseif(CURCALYR.ge.ymer.and.CURCALYR.le.ysteo)then 	
	     STMGTR_new(iregn,n) = SEDS_regn(iregn,1)
		 STJFTR_new(iregn,n) = SEDS_regn(iregn,2)
		 STDSTR_new(iregn,n) = SEDS_regn(iregn,3)
		 STRSTR_new(iregn,n) = SEDS_regn(iregn,4)
!...    STEO easy button
        if(curcalyr.gt.ymer) then
	        STMGTR_new(iregn,n) = STMGTR_new(iregn,n-1)*(TMGTCBUS(n)/TMGTCBUS(n-1))
		    STJFTR_new(iregn,n) = STJFTR_new(iregn,n-1)*(TJFTCBUS(n)/TJFTCBUS(n-1))
		    STDSTR_new(iregn,n) = STDSTR_new(iregn,n-1)*(TDSTCPUS(n)/TDSTCPUS(n-1))
		    STRSTR_new(iregn,n) = STRSTR_new(iregn,n-1)*(TRFTCBUS(n)/TRFTCBUS(n-1))			
		endif		  
	
        BENEL(iregn,n) = 1.0
        BENNG(iregn,n) = 1.0
        BENMG(iregn,n) = 1.0
        BENJF(iregn,n) = 1.0
        BENDS(iregn,n) = 1.0
        BENRS(iregn,n) = 1.0
        IF (QMGTR(iregn,n) .NE. 0.0) BENMG(iregn,n) = STMGTR_new(iregn,n) / QMGTR(iregn,n)
        IF (QJFTR(iregn,n) .NE. 0.0) BENJF(iregn,n) = STJFTR_new(iregn,n) / QJFTR(iregn,n) 
        IF (QDSTR(iregn,n) .NE. 0.0) BENDS(iregn,n) = STDSTR_new(iregn,n) / QDSTR(iregn,n)
        IF (QRSTR(iregn,n) .NE. 0.0) BENRS(iregn,n) = STRSTR_new(iregn,n) / QRSTR(iregn,n)		
		
        BENLG(iregn,n) = 1.0
        BENOT(iregn,n) = 1.0
        BENME(iregn,n) = 1.0
        BENET(iregn,n) = 1.0
        BENHY(iregn,n) = 1.0

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
        BENEL(iregn,n) = 1.0
        BENNG(iregn,n) = 1.0
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
        BENLG(iregn,n) = 1.0
        BENOT(iregn,n) = 1.0
        BENME(iregn,n) = 1.0
        BENET(iregn,n) = 1.0
        BENHY(iregn,n) = 1.0
!...  Benchmark factors after STEO phaseout based on final MER benchmark
      elseif(curcalyr.ge.ysteo+2) then
        BENEL(iregn,n) = 1.0
        BENNG(iregn,n) = 1.0
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
		BENLG(iregn,n) = 1.0
        BENOT(iregn,n) = 1.0
        BENME(iregn,n) = 1.0
        BENET(iregn,n) = 1.0
        BENHY(iregn,n) = 1.0
      endif	!end year control
    enddo	!end regional loop
	 
! ... Benchmarking transportation specific consumption variables
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
        QHYTR(IREGN,N) = QHYTR(IREGN,N) * BENHY(IREGN,N)      ! liquid hydrogen
        QAGTR(IREGN,N) = QAGTR(IREGN,N) * BENOT(IREGN,N)      ! aviation gasoline
        QLUTR(IREGN,N) = QLUTR(IREGN,N) * BENOT(IREGN,N)      ! lubricants        
      ENDDO
	 
!... makes sure the national bm factor is consistent with the regions
      IF (CURCALYR .GT. yseds) THEN
        BEN_MG = sum(BENMG(1:mnumcr-2,N)*QMGTR(1:mnumcr-2,N))/sum(QMGTR(1:mnumcr-2,N))
        BEN_JF = sum(BENJF(1:mnumcr-2,N)*QJFTR(1:mnumcr-2,N))/sum(QJFTR(1:mnumcr-2,N))
        BEN_DS = sum(BENDS(1:mnumcr-2,N)*QDSTR(1:mnumcr-2,N))/sum(QDSTR(1:mnumcr-2,N))
        BEN_RS = sum(BENRS(1:mnumcr-2,N)*QRSTR(1:mnumcr-2,N))/sum(QRSTR(1:mnumcr-2,N))
        BEN_ME = 1.0
        BEN_ET = 1.0
        BEN_NG = 1.0
        BEN_LG = 1.0
        BEN_EL = 1.0
        BEN_HY = 1.0		
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

! Fill the balance sector quantities that are substracted from associated rows in ftab Table 2.
! Retain seds-based Census Division sharing in balance sector by factoring the US benchmark.
    if(curcalyr.ge.ymer) then
      QMGBS(1:mnumcr,N)=(QMGTR(mnumcr,N)-(QMGTR(mnumcr,N)/BENMG(mnumcr,N)))*(QMGTR(1:mnumcr,n)/qmgtr(mnumcr,n))
      QDSBS(1:mnumcr,N)=(QDSTR(mnumcr,N)-(QDSTR(mnumcr,N)/BENDS(mnumcr,N)))*(QDSTR(1:mnumcr,n)/QDSTR(mnumcr,n))
    endif   
    QJFBS(1:mnumcr,N)=(QJFTR(mnumcr,N)-(QJFTR(mnumcr,N)/BENJF(mnumcr,N)))*(QJFTR(1:mnumcr,n)/QJFTR(mnumcr,n))

!...nce
! ... Write the benchmark factors and balance sector values to tranout.txt
    IF (N.eq.MNUMYR.and.FCRL.eq.1) THEN
	  WRITE(21,*)'============== BENCHMARK FACTORS =============='
	  WRITE(21,*)'YEAR  BENMG  BENDS  BENRS'
	  DO icheck = 6, MNUMYR
	    WRITE(21,*)icheck+1989, BENMG(11,icheck), BENDS(11,icheck),BENRS(11,icheck)
	  ENDDO
	ENDIF
 
 ! Some benchmark differences are now accounted for in the fuel balance sector and will be
 ! subtracted from the associated transportation consumption arrays in Table 2 of ftab.  So un-do
 ! the benchmarking for these fuels as applied to the remaining mode consumption and travel arrays.
 
      if(curcalyr.ge.ymer) then
         BENMG(1:mnumcr,n)=1.0
         BENDS(1:mnumcr,n)=1.0
         BEN_MG=1.
         BEN_DS=1.
      endif
      BENJF(1:mnumcr,n)=1.0
      BEN_JF=1.
         
!...FFV total fuel demand by region passed to PMM
    do iregn=1,mnumcr
      QFFV(iregn,n)=((1.0-PCTAF(2,iregn,n))*(sum(VMTHH(N,iregn,3,1:maxvtyp))/MPGTECH(3,n))*MG_HHV)*BEN_MG + &
                     (PCTAF(2,iregn,n)*(sum(VMTHH(N,iregn,3,1:maxvtyp))/MPGTECH(3,n))*MG_HHV)*BEN_ET + &
                       FLTTLLDVBTU(3,n)*RSHR(iregn,n)*(BEN_ET*PCTAF(2,iregn,n)+BEN_MG*(1.0-PCTAF(2,iregn,n)))					   
                       
!.. Recalibrate QETTR based on benchmarked total FFV demand
      QETTR(IREGN,N) = QFFV(IREGN,N) * PCTAF(2,iregn,n)                 
    enddo

!... Benchmark commercial light truck consumption by powertrain
    do IREGN=1,MNUMCR-2
      BCLTBTU(1,IREGN,N) = CLTBTUT(1,N)*RSHR(IREGN,N)*BEN_MG
      BCLTBTU(2,IREGN,N) = CLTBTUT(2,N)*RSHR(IREGN,N)*BEN_DS
	  BCLTBTU(3,IREGN,N) = CLTBTUT(3,N)*RSHR(IREGN,N)*BEN_LG
	  BCLTBTU(4,IREGN,N) = CLTBTUT(4,N)*RSHR(IREGN,N)*BEN_NG
	  BCLTBTU(5,IREGN,N) = CLTBTUT(5,N)*RSHR(IREGN,N)*(BEN_ET*PCTAF(2,mnumcr,N)+BEN_MG*(1.0-PCTAF(2,mnumcr,N)))
	  BCLTBTU(6,IREGN,N) = CLTBTUT(6,N)*RSHR(IREGN,N)*BEN_EL
	  BCLTBTU(7,IREGN,N) = CLTBTUT(7,N)*RSHR(IREGN,N)*(BEN_EL*PctPHEV_HDV + BEN_DS*(1.0 - PctPHEV_HDV)) 
	  BCLTBTU(8,IREGN,N) = CLTBTUT(8,N)*RSHR(IREGN,N)*(BEN_EL*PctPHEV_HDV + BEN_MG*(1.0 - PctPHEV_HDV)) 
	  BCLTBTU(9,IREGN,N) = CLTBTUT(9,N)*RSHR(IREGN,N)*BEN_HY
    enddo

    BCLTBTUT(1,N) = sum(BCLTBTU(1,1:mnumcr-2,N))                      ! Gasoline
    BCLTBTUT(2,N) = sum(BCLTBTU(2,1:mnumcr-2,N))                      ! Diesel
    BCLTBTUT(3,N) = sum(BCLTBTU(3,1:mnumcr-2,N))                      ! LPG
    BCLTBTUT(4,N) = sum(BCLTBTU(4,1:mnumcr-2,N))                      ! CNG
    BCLTBTUT(5,N) = sum(BCLTBTU(5,1:mnumcr-2,N))                      ! Flex fuel
    BCLTBTUT(6,N) = sum(BCLTBTU(6,1:mnumcr-2,N))                      ! Electric
    BCLTBTUT(7,N) = sum(BCLTBTU(7,1:mnumcr-2,N))                      ! PHEV diesel
    BCLTBTUT(8,N) = sum(BCLTBTU(8,1:mnumcr-2,N))                      ! PHEV gasoline
    BCLTBTUT(9,N) = sum(BCLTBTU(9,1:mnumcr-2,N))                      ! Hydrogen
    BCLTBTUT(10,N) = sum(BCLTBTUT(1:9,N))                             ! Total

	
!... Benchmark commercial light truck consumption by fuel 
    CLTFUELBTU(1,n) = sum(cltfbtu(n,1,1:mnumcr-2))*BEN_MG             ! Gasoline
    CLTFUELBTU(2,n) = sum(cltfbtu(n,2,1:mnumcr-2))*BEN_DS             ! Diesel
	CLTFUELBTU(3,n) = sum(cltfbtu(n,3,1:mnumcr-2))*BEN_LG             ! LPG
	CLTFUELBTU(4,n) = sum(cltfbtu(n,4,1:mnumcr-2))*BEN_NG             ! CNG
	CLTFUELBTU(5,n) = sum(cltfbtu(n,5,1:mnumcr-2))*BEN_ET             ! Ethanol
	CLTFUELBTU(6,n) = sum(cltfbtu(n,6,1:mnumcr-2))*BEN_EL             ! Electric
	CLTFUELBTU(7,n) = sum(cltfbtu(n,7,1:mnumcr-2))*BEN_HY             ! Hydrogen

	CLTFUELBTU(10,n) = sum(CLTFUELBTU(1:7,n))

! ... Benchmark consumption variables
      DO IREGN=1,MNUMCR
! ... Benchmark light duty vehicle consumption by fuel type
! ... 1=gasoline   2=methanol   3=ethanol          4=cng
! ... 5=lpg        6=electric   7=liquid hydrogen  8=distillate
        BTQLDV(1,IREGN) = TQLDV(1,IREGN,N) * BEN_MG
        BTQLDV(2,IREGN) = TQLDV(2,IREGN,N) * BENME(IREGN,N)
        BTQLDV(3,IREGN) = TQLDV(3,IREGN,N) * BENET(IREGN,N)
        BTQLDV(4,IREGN) = TQLDV(4,IREGN,N) * BENNG(IREGN,N)
        BTQLDV(5,IREGN) = TQLDV(5,IREGN,N) * BENLG(IREGN,N)
        BTQLDV(6,IREGN) = TQLDV(6,IREGN,N) * BENEL(IREGN,N)
        BTQLDV(7,IREGN) = TQLDV(7,IREGN,N) * BENHY(IREGN,N)
        BTQLDV(8,IREGN) = TQLDV(8,IREGN,N) * BEN_DS

! ... Benchmark freight truck consumption by fuel type and size class
!... Light heavy-duty trucks                                                                       
            BTQFREIRSC(1,1,IREGN) = TFRBTU_F_T(N,1,1,IREGN)*BENDS(iregn,n)        ! Diesel
            BTQFREIRSC(1,2,IREGN) = TFRBTU_F_T(N,1,2,IREGN)*BENMG(iregn,n)        ! Gasoline
            BTQFREIRSC(1,3,IREGN) = TFRBTU_F_T(N,1,3,IREGN)*BENLG(IREGN,N)        ! LPG
            BTQFREIRSC(1,4,IREGN) = TFRBTU_F_T(N,1,4,IREGN)*BENNG(IREGN,N)        ! Natural Gas
			BTQFREIRSC(1,5,IREGN) = TFRBTU_F_T(N,1,5,IREGN)*BENET(IREGN,N)        ! Ethanol
			BTQFREIRSC(1,6,IREGN) = TFRBTU_F_T(N,1,6,IREGN)*BENEL(IREGN,N)        ! Electric
			BTQFREIRSC(1,7,IREGN) = TFRBTU_F_T(N,1,7,IREGN)*BENHY(IREGN,N)        ! Hydrogen		
!... Medium heavy-duty trucks
            BTQFREIRSC(2,1,IREGN) = TFRBTU_F_T(N,2,1,IREGN)*BENDS(iregn,n)        ! Diesel
            BTQFREIRSC(2,2,IREGN) = TFRBTU_F_T(N,2,2,IREGN)*BENMG(iregn,n)        ! Gasoline
            BTQFREIRSC(2,3,IREGN) = TFRBTU_F_T(N,2,3,IREGN)*BENLG(IREGN,N)        ! LPG
            BTQFREIRSC(2,4,IREGN) = TFRBTU_F_T(N,2,4,IREGN)*BENNG(IREGN,N)        ! Natural Gas
			BTQFREIRSC(2,5,IREGN) = TFRBTU_F_T(N,2,5,IREGN)*BENET(IREGN,N)        ! Ethanol
			BTQFREIRSC(2,6,IREGN) = TFRBTU_F_T(N,2,6,IREGN)*BENEL(IREGN,N)        ! Electric
			BTQFREIRSC(2,7,IREGN) = TFRBTU_F_T(N,2,7,IREGN)*BENHY(IREGN,N)        ! Hydrogen						
!... Heavy heavy-duty trucks
            BTQFREIRSC(3,1,IREGN) = TFRBTU_F_T(N,3,1,IREGN)*BENDS(iregn,n)        ! Diesel
            BTQFREIRSC(3,2,IREGN) = TFRBTU_F_T(N,3,2,IREGN)*BENMG(iregn,n)        ! Gasoline
            BTQFREIRSC(3,3,IREGN) = TFRBTU_F_T(N,3,3,IREGN)*BENLG(IREGN,N)        ! LPG
            BTQFREIRSC(3,4,IREGN) = TFRBTU_F_T(N,3,4,IREGN)*BENNG(IREGN,N)        ! Natural Gas
			BTQFREIRSC(3,5,IREGN) = TFRBTU_F_T(N,3,5,IREGN)*BENET(IREGN,N)        ! Ethanol
			BTQFREIRSC(3,6,IREGN) = TFRBTU_F_T(N,3,6,IREGN)*BENEL(IREGN,N)        ! Electric
			BTQFREIRSC(3,7,IREGN) = TFRBTU_F_T(N,3,7,IREGN)*BENHY(IREGN,N)        ! Hydrogen

! ... Benchmark domestic shipping consumption by fuel type
! ... 1=distillate   2=residual   3=cng  4=lng 
        BTQDSHIPR(1,IREGN) = TQDSHIPR(1,IREGN,N) * BENDS(iregn,n)
        BTQDSHIPR(2,IREGN) = TQDSHIPR(2,IREGN,N) * BENRS(iregn,n)
        BTQDSHIPR(3,IREGN) = TQDSHIPR(3,IREGN,N)  ! CNG 
        BTQDSHIPR(4,IREGN) = TQDSHIPR(4,IREGN,N)  ! LNG 

! ... Benchmark international shipping consumption by fuel type
! ... 1=distillate   2=residual  3=cng  4=lng  5=low sulfur fuel oil
        BTQISHIPR(1,IREGN) = TQISHIPR(1,IREGN,N) * BEN_DS
        BTQISHIPR(2,IREGN) = (TQISHIPR(2,IREGN,N) +TQISHIPR(5,IREGN,N))* BEN_RS
        BTQISHIPR(3,IREGN) = TQISHIPR(3,IREGN,N)  ! CNG 
        BTQISHIPR(4,IREGN) = TQISHIPR(4,IREGN,N)  ! LNG 
        
! ... Benchmark rail consumption by fuel type
! ... 1=distillate   2=residual   3=cng  4=lng
        BTQRAILR(1,IREGN) = TQRAILR(1,IREGN,N) * BENDS(iregn,n)
        BTQRAILR(2,IREGN) = TQRAILR(2,IREGN,N) * BENRS(iregn,n)     
        BTQRAILR(3,IREGN) = TQRAILR(3,IREGN,N)
        BTQRAILR(4,IREGN) = TQRAILR(4,IREGN,N)

! ... Benchmark military consumption by fuel type
! ... 1=distillate   2=jet fuel naphtha   3=residual   4=jet fuel kerosene
        BQMILTR(1,IREGN) = QMILTR(1,IREGN,N) * BEN_DS
        BQMILTR(2,IREGN) = QMILTR(2,IREGN,N) * BEN_JF
        BQMILTR(3,IREGN) = QMILTR(3,IREGN,N) * BEN_RS
        BQMILTR(4,IREGN) = QMILTR(4,IREGN,N) * BEN_JF

! ... Benchmark mass transit by mode
! ... 1=ldv    2=transit bus      3=intercity bus   4=school bus
! ...          5=intercity rail   6=transit rail    7=commuter rail
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

!...Benchmark commercial fleet vehicle consumption by fuel type
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

!...Benchmark VMT by technology
	do iregn=1,mnumcr-2
      BVMTECH( 1,iregn) = sum(VMTHH(N,iregn,1,1:maxvtyp)) *   BEN_MG
      BVMTECH( 2,iregn) = sum(VMTHH(N,iregn,2,1:maxvtyp)) *   BEN_DS
      BVMTECH( 3,iregn) = sum(VMTHH(N,iregn,3,1:maxvtyp)) *  (BEN_ET*PCTAF(2,iregn,N)+BEN_MG*(1.0-PCTAF(2,iregn,N)))
      BVMTECH( 4,iregn) = sum(VMTHH(N,iregn,4,1:maxvtyp)) *   BEN_EL
      BVMTECH( 5,iregn) = sum(VMTHH(N,iregn,5,1:maxvtyp)) *  (BEN_EL*PctPHEV20(iregn,N)+BEN_MG*(1.0-PctPHEV20(iregn,N)))
      BVMTECH( 6,iregn) = sum(VMTHH(N,iregn,6,1:maxvtyp)) *  (BEN_EL*PctPHEV50(iregn,N)+BEN_MG*(1.0-PctPHEV50(iregn,N)))
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
		BVMT_STK_HH(1:maxvtyp,ILDV,1:maxage,1:maxhav,1:mnumcr-2) = VMT_STK_HH(1:maxvtyp,ILDV,1:maxage,1:maxhav,1:mnumcr-2)
		!VMT_STK_HH is in millions, and BVMTECH is in billions, 
		!...so BMFAC as calculated is adjusted by a factor of 1000 to normalize to 1
        BMFAC = 1.0
		IF (sum(VMT_STK_HH(1:maxvtyp,ILDV,1:maxage,1:maxhav,1:mnumcr-2)) .NE. 0.0) then
			BMFAC = 1000.0 * BVMTECH(ILDV,mnumcr) / sum(VMT_STK_HH(1:maxvtyp,ILDV,1:maxage,1:maxhav,1:mnumcr-2))
		endif
		BVMT_STK_HH(1:maxvtyp,ILDV,1:maxage,1:maxhav,1:mnumcr-2) = BVMT_STK_HH(1:maxvtyp,ILDV,1:maxage,1:maxhav,1:mnumcr-2) * BMFAC
      ENDDO
	  
!...Benchmark VMT for light duty business fleet vehicles
    do IVTYP=1,maxvtyp
      do ifleet=1,maxfleet
	    do ihav=1,maxhav
			FLTVMTHAV(IVTYP,ifleet, 1,ihav,yrs)= FLTVMTECH(IVTYP,ifleet, 1,ihav)*  BEN_MG
			FLTVMTHAV(IVTYP,ifleet, 2,ihav,yrs)= FLTVMTECH(IVTYP,ifleet, 2,ihav)*  BEN_DS 
			FLTVMTHAV(IVTYP,ifleet, 3,ihav,yrs)= FLTVMTECH(IVTYP,ifleet, 3,ihav)*((BEN_ET*PCTAF(2,mnumcr,n)) + (BEN_MG*(1.0-PCTAF(2,mnumcr,n))))
			FLTVMTHAV(IVTYP,ifleet, 4,ihav,yrs)= FLTVMTECH(IVTYP,ifleet, 4,ihav)*  BEN_EL 
			FLTVMTHAV(IVTYP,ifleet, 5,ihav,yrs)= FLTVMTECH(IVTYP,ifleet, 5,ihav)*((BEN_EL*PctPHEV20(11,n)) + (BEN_MG*(1.0-PctPHEV20(11,n))))
			FLTVMTHAV(IVTYP,ifleet, 6,ihav,yrs)= FLTVMTECH(IVTYP,ifleet, 6,ihav)*((BEN_EL*PctPHEV50(11,n)) + (BEN_MG*(1.0-PctPHEV50(11,n))))
			FLTVMTHAV(IVTYP,ifleet, 7,ihav,yrs)= FLTVMTECH(IVTYP,ifleet, 7,ihav)*  BEN_EL
			FLTVMTHAV(IVTYP,ifleet, 8,ihav,yrs)= FLTVMTECH(IVTYP,ifleet, 8,ihav)*  BEN_DS
			FLTVMTHAV(IVTYP,ifleet, 9,ihav,yrs)= FLTVMTECH(IVTYP,ifleet, 9,ihav)*((BEN_NG*PCTAF(3,mnumcr,n)) + (BEN_MG*(1.0-PCTAF(3,mnumcr,n))))
			FLTVMTHAV(IVTYP,ifleet,10,ihav,yrs)= FLTVMTECH(IVTYP,ifleet,10,ihav)*((BEN_LG*PCTAF(4,mnumcr,n)) + (BEN_MG*(1.0-PCTAF(4,mnumcr,n)))) 
			FLTVMTHAV(IVTYP,ifleet,11,ihav,yrs)= FLTVMTECH(IVTYP,ifleet,11,ihav)*  BEN_NG
			FLTVMTHAV(IVTYP,ifleet,12,ihav,yrs)= FLTVMTECH(IVTYP,ifleet,12,ihav)*  BEN_LG
			FLTVMTHAV(IVTYP,ifleet,13,ihav,yrs)= FLTVMTECH(IVTYP,ifleet,13,ihav)*  BEN_ME
			FLTVMTHAV(IVTYP,ifleet,14,ihav,yrs)= FLTVMTECH(IVTYP,ifleet,14,ihav)*  BEN_HY
			FLTVMTHAV(IVTYP,ifleet,15,ihav,yrs)= FLTVMTECH(IVTYP,ifleet,15,ihav)*  BEN_EL 
			FLTVMTHAV(IVTYP,ifleet,16,ihav,yrs)= FLTVMTECH(IVTYP,ifleet,16,ihav)*  BEN_MG
		enddo
      enddo
    enddo

	do IVTYP=1,maxvtyp
	  do ifleet=1, maxfleet
	    do ILDV = 1, maxldv
			BFLTVMTECH(IVTYP,ifleet,ILDV) = sum(FLTVMTHAV(IVTYP,ifleet,ILDV,1:maxhav,yrs))
		enddo
	  enddo
	enddo

!   Convert benchmarked fleet vmt to billion miles
    BFLTVMTECH(:,:,:) = BFLTVMTECH(:,:,:)/1000000000.0

!...Benchmark VMT for commercial light trucks by technology
    BCLTVMT(1,N) = CLTVMTT(1,N)*BENMG(mnumcr,N)                                                          ! Gasoline	
    BCLTVMT(2,N) = CLTVMTT(2,N)*BENDS(mnumcr,N)                                                          ! Diesel
    BCLTVMT(3,N) = CLTVMTT(3,N)*BENLG(mnumcr,N)                                                          ! LPG
	BCLTVMT(4,N) = CLTVMTT(4,N)*BENNG(mnumcr,N)                                                          ! CNG
	BCLTVMT(5,N) = CLTVMTT(5,N)*(BENET(mnumcr,N)*PCTAF(2,mnumcr,n) + BENMG(mnumcr,N)*(1.0-PCTAF(2,mnumcr,n)))        ! Flex fuel
	BCLTVMT(6,N) = CLTVMTT(6,N)*BENEL(mnumcr,N)                                                          ! Electric
	BCLTVMT(7,N) = CLTVMTT(7,N)*(BENEL(mnumcr,N)*PctPHEV_HDV + BENDS(mnumcr,N)*(1.0-PctPHEV_HDV))            ! PHEV Diesel
	BCLTVMT(8,N) = CLTVMTT(8,N)*(BENEL(mnumcr,N)*PctPHEV_HDV + BENMG(mnumcr,N)*(1.0-PctPHEV_HDV))            ! PHEV Gasoline
	BCLTVMT(9,N) = CLTVMTT(9,N)*BENHY(mnumcr,N)                                                          ! Hydrogen
	BCLTVMT(10,N) = sum(BCLTVMT(1:9,N))                                                              ! Total

! ... Benchmark VMT for freight truck by technology
    do iregn=1,mnumcr  		
!... Light and medium heavy-duty trucks	  
          BFVMTECHSC(1,1,iregn) = sum(VMTFLT_SAF_TR(1:2,1,iregn))*BENDS(iregn,N)                                                            ! Diesel
          BFVMTECHSC(1,2,iregn) = sum(VMTFLT_SAF_TR(1:2,2,iregn))*BENMG(iregn,N)                                                            ! Gasoline
          BFVMTECHSC(1,3,iregn) = sum(VMTFLT_SAF_TR(1:2,3,iregn))*BENLG(iregn,N)                                                            ! Lpg
          BFVMTECHSC(1,4,iregn) = sum(VMTFLT_SAF_TR(1:2,4,iregn))*BENNG(iregn,N)                                                            ! Cng
		  BFVMTECHSC(1,5,iregn) = sum(VMTFLT_SAF_TR(1:2,5,iregn))*(BENET(iregn,N)*PCTAF(2,iregn,n) + BENMG(iregn,N)*(1.0-PCTAF(2,iregn,n))) ! Flex fuel
		  BFVMTECHSC(1,6,iregn) = sum(VMTFLT_SAF_TR(1:2,6,iregn))*BENEL(iregn,n)                                                            ! Electric 
		  BFVMTECHSC(1,7,iregn) = sum(VMTFLT_SAF_TR(1:2,7,iregn))*(BEN_EL*PctPHEV_HDV + BEN_DS*(1.0-PctPHEV_HDV))                           ! PHEV Diesel
		  BFVMTECHSC(1,8,iregn) = sum(VMTFLT_SAF_TR(1:2,8,iregn))*(BEN_EL*PctPHEV_HDV + BEN_MG*(1.0-PctPHEV_HDV))                           ! PHEV Gasoline
		  BFVMTECHSC(1,9,iregn) = sum(VMTFLT_SAF_TR(1:2,9,iregn))*BENHY(iregn,n)                                                            ! Hydrogen 		
!... Heavy heavy-duty trucks		
          BFVMTECHSC(2,1,iregn) = VMTFLT_SAF_TR(3,1,iregn)*BENDS(iregn,N)                                                                   ! Diesel
          BFVMTECHSC(2,2,iregn) = VMTFLT_SAF_TR(3,2,iregn)*BENMG(iregn,N)                                                                   ! Gasoline
          BFVMTECHSC(2,3,iregn) = VMTFLT_SAF_TR(3,3,iregn)*BENLG(iregn,N)                                                                   ! Lpg
          BFVMTECHSC(2,4,iregn) = VMTFLT_SAF_TR(3,4,iregn)*BENNG(iregn,N)                                                                   ! Cng
		  BFVMTECHSC(2,5,iregn) = VMTFLT_SAF_TR(3,5,iregn)*(BENET(iregn,N)*PCTAF(2,iregn,n) + BENMG(iregn,N)*(1.0-PCTAF(2,iregn,n)))        ! Flex fuel
		  BFVMTECHSC(2,6,iregn) = VMTFLT_SAF_TR(3,6,iregn)*BENEL(iregn,n)                                                                   ! Electric 
		  BFVMTECHSC(2,7,iregn) = VMTFLT_SAF_TR(3,7,iregn)*(BEN_EL*PctPHEV_HDV + BEN_DS*(1.0-PctPHEV_HDV))                                  ! PHEV Diesel
		  BFVMTECHSC(2,8,iregn) = VMTFLT_SAF_TR(3,8,iregn)*(BEN_EL*PctPHEV_HDV + BEN_MG*(1.0-PctPHEV_HDV))                                  ! PHEV Gasoline
		  BFVMTECHSC(2,9,iregn) = VMTFLT_SAF_TR(3,9,iregn)*BENHY(iregn,n)  		                                                            ! Hydrogen 
    enddo  

!...Benchmark seat-miles demanded for air
    IF (N .GE. 6) THEN
      BASMDEMD(1) = ASMDEMD(1,1,N)
      BASMDEMD(2) = ASMDEMD(1,2,N)
    ENDIF

!...Benchmark TMT for rail and ship
!...rail
    do iregn=1,mnumcr
      if(n.le.RAILHISTYR) then
        BRTMTT(n,iregn) = RTMTT(n,iregn)
      else
        BRTMTT(n,iregn) = RTMTT(n,iregn) * BENDS(iregn,n)
      endif 
!...ship
      if(n.le.shiphistyr) then
        BSTMTT(n,iregn) = STMTT(n,iregn)
      else
	    if (STMTT(n,iregn).gt.0.0) then
          BSTMTT(n,iregn) = STMTT(n,iregn) * (TQDSHIPR(1,iregn,N)+TQDSHIPR(2,iregn,N)) / &
                            (TQDSHIPR(1,iregn,N)/BENDS(iregn,N)+TQDSHIPR(2,iregn,N)/BENRS(iregn,N))
		else
		  BSTMTT(n,iregn) = STMTT(n,iregn)
		endif
      endif     
    enddo

    RETURN
    END SUBROUTINE TBENCHMARK

! ==========================================================================================================
! ... Subroutine TEMISS calculates vehicle emissions by 3 pollutants by 12 vehicle types
! ... Pollutants - IP:   1) HC    2) CO    3) NOx                     
! ...  NEMS vehicle classifications                                   
! ...  Vehicle Type    Description                                    
! ...       1       Light-Duty Gasoline vehicles (Passenger cars)     
! ...       2       Light-Duty Diesel vehicles  (Passenger cars)      
! ...       3       Light-Duty Gasoline trucks 1-4 (8500 lbs LVW)     
! ...       4       Light-Duty Diesel trucks 1-4 (0-8500 lbs GVWR)    
! ...       5       Class 2b Heavy-Duty Gasoline veh (8501-10000 lbs) 
! ...       6       Class 2b Heavy-Duty Diesel veh (8501-10000 lbs)   
! ...       7       Class 3 Heavy-Duty Gasoline veh (10001-14000 lbs) 
! ...       8       Class 3 Heavy-Duty Diesel veh (10001-14000 lbs)   
! ...       9       Class 4-6 Heavy-Duty Gasoline veh(14001-26000 lbs)
! ...      10       Class 4-6 Heavy-Duty Diesel veh (14001-26000 lbs) 
! ...      11       Class 7-8b Heavy-Duty Gasoline veh (>26000 lbs)   
! ...      12       Class 7-8b Heavy-Duty Diesel veh (>26000 lbs)  
! ==========================================================================================================
    SUBROUTINE TEMISS
    USE T_
    IMPLICIT NONE

      INTEGER NPOL, NEMS_VT
      PARAMETER(NPOL=3,NEMS_VT=12)
      CHARACTER*18 FNAME
      CHARACTER*18 vmt_label(11) /'gas cars','dsl cars','gas ltrk','dsl ltrk', &
      'coml trk','gas lhvy','dsl lhvy','gas mhvy','dsl mhvy','gas  hvy','dsl  hvy'/
      LOGICAL NEW

      INTEGER  IUNIT10,IP,VT
      INTEGER  IP2,VT2,IYR2
      REAL     TEMIS_FCTR(MNUMYR,NPOL,NEMS_VT,20),TEMISSIONS(MNUMYR,NPOL,NEMS_VT,20)

! ... Sum up total vmt across size classes

      CALL TRANFRT(0,1)    ! in TRANFRT.F, 1 indicates a reporting call to TFRTRPT

 7997 FORMAT(/)
 7998 FORMAT(1x,A8)
 7999 FORMAT(1x,I4,31F8.3)
!      ENDIF

    RETURN
    END SUBROUTINE TEMISS

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

      REAL   FLTECHRPT_REG(2,MNUMCR,MAXLDV,MNUMYR),SC_NCSTECH(MNUMCR,MAXLDV,MNUMYR), &
             SC_NLTECH(MNUMCR,MAXLDV,MNUMYR), &
             ANCSALE(MNUMCR,MNUMYR),ANTSALE(MNUMCR,MNUMYR), CQ(MNUMYR),LTQ(MNUMYR), &
             FLTFCLDVBTUT(2,MNUMYR), FLTFCLDVBTUR(MNUMCR,maxldv),&
             BEN(8,MNUMYR)

      REAL   NUM1,NUM2,NUM3,DEN1,DEN2,DEN3,FLTNUM1,FLTNUM2,FLTDEN1,FLTDEN2,                  &  
             cmpg_1(maxldv),tmpg_1(maxldv),lmpg_1(maxldv),cmpg_2(maxldv),tmpg_2(maxldv),     &
             lmpg_2(maxldv)	 
             
! ... ***** TABLE 7 *******
! ... Total freight truck vmt
      DO IFUELX=1,9
        DO I=1,2
          TRVMTTRK(I,IFUELX,N) = sum(BFVMTECHSC(I,IFUELX,1:mnumcr-2)) / (1.0*1E9)
        ENDDO
      ENDDO

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
          CQ(N)  = CQ(N)  + (sum(BVMT_STK_HH(1,ILDV,1:maxage,1:maxhav,1:mnumcr-2)) /CMPG_IT(ILDV,N)) * MG_HHV / 1000.0
        IF (TMPG_IT(ILDV,N) .NE. 0.0) &
          LTQ(N) = LTQ(N) + (sum(BVMT_STK_HH(2,ILDV,1:maxage,1:maxhav,1:mnumcr-2))/TMPG_IT(ILDV,N)) * MG_HHV / 1000.0		  
      ENDDO

!...FTAB Table 53 - Fleet energy demand by vehicle type
!...Benchmark before writing to report writer
    do IVTYP=1,maxvtyp
      FLTFCLDVBTU(IVTYP, 1,n) = FLTFCLDVBTU(IVTYP, 1,n) * BENMG(11,n)
      FLTFCLDVBTU(IVTYP, 2,n) = FLTFCLDVBTU(IVTYP, 2,n) * BENDS(11,n)
      FLTFCLDVBTU(IVTYP, 3,n) = FLTFCLDVBTU(IVTYP, 3,n) * (1-PCTAF(2,11,n))*BENMG(11,n)+&
                                FLTFCLDVBTU(IVTYP, 3,n) * PCTAF(2,11,n)*BENET(11,n)
      FLTFCLDVBTU(IVTYP, 4,n) = FLTFCLDVBTU(IVTYP, 4,n) * BENEL(11,n) 
      FLTFCLDVBTU(IVTYP, 5,n) = FLTFCLDVBTU(IVTYP, 5,n) * (1-PctPHEV20(11,n))*BENMG(11,n)+&
                                FLTFCLDVBTU(IVTYP, 5,n) * PctPHEV20(11,n)*BENEL(11,n)
      FLTFCLDVBTU(IVTYP, 6,n) = FLTFCLDVBTU(IVTYP, 6,n) * (1-PctPHEV50(11,n))*BENMG(11,n)+&
                                FLTFCLDVBTU(IVTYP, 6,n) * PctPHEV50(11,n)*BENEL(11,n)
      FLTFCLDVBTU(IVTYP, 7,n) = FLTFCLDVBTU(IVTYP, 7,n) * BENEL(11,n)
      FLTFCLDVBTU(IVTYP, 8,n) = FLTFCLDVBTU(IVTYP, 8,n) * BENDS(11,n)
      FLTFCLDVBTU(IVTYP, 9,n) = FLTFCLDVBTU(IVTYP, 9,n) * (1-PCTAF(3,11,n))*BENMG(11,n)+&
                                FLTFCLDVBTU(IVTYP, 9,n) * PCTAF(3,11,n)*BENMG(11,n)
      FLTFCLDVBTU(IVTYP,10,n) = FLTFCLDVBTU(IVTYP,10,n) * (1-PCTAF(4,11,n))*BENMG(11,n)+&
                                FLTFCLDVBTU(IVTYP,10,n) * PCTAF(4,11,n)*BENLG(11,n)
      FLTFCLDVBTU(IVTYP,11,n) = FLTFCLDVBTU(IVTYP,11,n) * BENNG(11,n)
      FLTFCLDVBTU(IVTYP,12,n) = FLTFCLDVBTU(IVTYP,12,n) * BENLG(11,n)
      FLTFCLDVBTU(IVTYP,13,n) = FLTFCLDVBTU(IVTYP,13,n) * BENME(11,n)
      FLTFCLDVBTU(IVTYP,14,n) = FLTFCLDVBTU(IVTYP,14,n) * BENHY(11,n)
      FLTFCLDVBTU(IVTYP,15,n) = FLTFCLDVBTU(IVTYP,15,n) * BENEL(11,n) 
      FLTFCLDVBTU(IVTYP,16,n) = FLTFCLDVBTU(IVTYP,16,n) * BENMG(11,n)
    enddo

!...regional fleet vehicle energy demand by light vehicle type
    do iregn=1,mnumcr-2
      do ILDV=1,maxldv
        FLTFCLDVBTUR(iregn,ILDV) = (FLTFCLDVBTU(1,ILDV,n)+FLTFCLDVBTU(2,ILDV,n)) * RSHR(iregn,n)
      enddo
    enddo
!...total fleet vehicle energy demand by light vehicle type 
    do ILDV=1,maxldv
      FLTFCLDVBTUR(11,ILDV) = sum(FLTFCLDVBTUR(1:mnumcr-2,ILDV))
    enddo

!...total fleet vehicle energy demand by car and light truck 
    do IVTYP=1,maxvtyp
      FLTFCLDVBTUT(IVTYP,n) = 0.0
      do ILDV=1,maxldv
        FLTFCLDVBTUT(IVTYP,n) = FLTFCLDVBTUT(IVTYP,n)+FLTFCLDVBTU(IVTYP,ILDV,n)
      enddo
    enddo

! ... Calculate energy use by light duty vehicles (cars, l.t., motorcycles)
      TRQHWY_TMP(1,N) = CQ(N) + FLTFCLDVBTUT(1,N)

! ... Personal + fleet + freight for l.t.
      TRQHWY_TMP(2,N) = LTQ(N) + FLTFCLDVBTUT(2,N)
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
      TRQENUSE(12,N) = QHYTR(11,N)

!...Populate electricity consumption array for EMM
!	These values are all benchmarked.
	DO IREGN = 1, MNUMCR-2
	  TRQ_ELEC(1,IREGN,N) = TRQLDV(6,IREGN,N)							! LDV
	  TRQ_ELEC(2,IREGN,N) = SUM(QMTBR(1:3,7,IREGN,N)) * BENEL(IREGN,N)	! Buses (intercity, commuter, and transit)
	  TRQ_ELEC(3,IREGN,N) = cltfbtu(N,6,IREGN)* BENEL(IREGN,N)			! Commercial Light trucks (CLT)
	  TRQ_ELEC(4,IREGN,N) = SUM(BTQFREIRSC(1:3,6,IREGN))				! Freight trucks (light, medium, heavy)
	  TRQ_ELEC(5,IREGN,N) = TRQRAILR(1,IREGN,N)							! Passenger rail
	ENDDO

!...FTAB Table 47 - LDV energy consumption by LDV type
    do ILDV=1,maxldv
      TRLDQTEK(ILDV,N) = 0.0
    enddo

    do iregn=1,mnumcr-2
!...  Gas    
      TRLDQTEK(1,n) =  TRLDQTEK(1,n) + &                                                                           
                       ((sum(VMTHH(N,iregn,1,1:maxvtyp))/MPGTECH(1,N))*MG_HHV)*BENMG(iregn,n)+FLTFCLDVBTUR(iregn,1)
!...  Diesel
      TRLDQTEK(2,n) =  TRLDQTEK(2,n) + &                                                                           
                       ((sum(VMTHH(N,iregn,2,1:maxvtyp))/MPGTECH(2,n))*MG_HHV)*BENDS(iregn,n)+FLTFCLDVBTUR(iregn,2)
!...  FFV Ethanol
      TRLDQTEK(3,n) =  TRLDQTEK(3,n) + &                                                                          
                       ((1.0-PCTAF(2,iregn,n))*(sum(VMTHH(N,iregn,3,1:maxvtyp))/MPGTECH(3,n))*MG_HHV)*BENMG(iregn,n) + &
                       (PCTAF(2,iregn,n)*(sum(VMTHH(N,iregn,3,1:maxvtyp))/MPGTECH(3,n))*MG_HHV)*BENET(iregn,n) + &
                       FLTFCLDVBTUR(iregn,3)
!...  Electric - 100 
      TRLDQTEK(4,n) =  TRLDQTEK(4,n) + &                                                                          
                       ((sum(VMTHH(N,iregn,4,1:maxvtyp))/MPGTECH(4,n))*MG_HHV)*BENEL(iregn,n)+FLTFCLDVBTUR(iregn,7)
!...  PHEV20
      TRLDQTEK(5,n) =  TRLDQTEK(5,n) + &                                                                           
                       ((1.0-PctPHEV20(iregn,n))*(sum(VMTHH(N,iregn,5,1:maxvtyp))/MPGTECH(5,n))*MG_HHV)*BENMG(iregn,n) + &
                       (PctPHEV20(iregn,n)*(sum(VMTHH(N,iregn,5,1:maxvtyp))/MPGTECH(5,n))*MG_HHV)*BENEL(iregn,n) + &
                       FLTFCLDVBTUR(iregn,5)
!...  PHEV50
      TRLDQTEK(6,n) =  TRLDQTEK(6,n) + &                                                                          
                       ((1.0-PctPHEV50(iregn,n))*(sum(VMTHH(N,iregn,6,1:maxvtyp))/MPGTECH(6,n))*MG_HHV)*BENMG(iregn,n) + &
                       (PctPHEV50(iregn,n)*(sum(VMTHH(N,iregn,6,1:maxvtyp))/MPGTECH(6,n))*MG_HHV)*BENEL(iregn,n) + &
                       FLTFCLDVBTUR(iregn,6)
!...  Electric - 200 
      TRLDQTEK(7,n) =  TRLDQTEK(7,n) + &                                                                          
                       ((sum(VMTHH(N,iregn,7,1:maxvtyp))/MPGTECH(7,n))*MG_HHV)*BENEL(iregn,n) ! +FLTFCLDVBTUR(iregn,7)
!...  HEV Diesel
      TRLDQTEK(8,n) =  TRLDQTEK(8,n) + &                                                                           
                       ((sum(VMTHH(N,iregn,8,1:maxvtyp))/MPGTECH(8,n))*MG_HHV)*BENDS(iregn,n)+FLTFCLDVBTUR(iregn,8)
!...  Bi-Fuel CNG
      TRLDQTEK(9,n) =  TRLDQTEK(9,n) + &                                                                           
                       ((1.0-PCTAF(3,iregn,n))*(sum(VMTHH(N,iregn,9,1:maxvtyp))/MPGTECH(9,n))*MG_HHV)*BENMG(iregn,n) + &
                       (PCTAF(3,iregn,n)*(sum(VMTHH(N,iregn,9,1:maxvtyp))/MPGTECH(9,n))*MG_HHV)*BENNG(iregn,n) + &
                       FLTFCLDVBTUR(iregn,9)
!...  Bi-Fuel LPG
      TRLDQTEK(10,n) = TRLDQTEK(10,n) + &                                                                          
                       ((1.0-PCTAF(4,iregn,n))*(sum(VMTHH(N,iregn,10,1:maxvtyp))/MPGTECH(10,n))*MG_HHV)*BENMG(iregn,n) + &
                       (PCTAF(4,iregn,n)*(sum(VMTHH(N,iregn,10,1:maxvtyp))/MPGTECH(10,n))*MG_HHV)*BENLG(iregn,n) + &
                       FLTFCLDVBTUR(iregn,10)
!...  AFV CNG
      TRLDQTEK(11,n) = TRLDQTEK(11,n) + &                                                                          
                       ((sum(VMTHH(N,iregn,11,1:maxvtyp))/MPGTECH(11,n))*MG_HHV)*BENNG(iregn,n)+FLTFCLDVBTUR(iregn,11)
!...  AFV LPG
      TRLDQTEK(12,n) = TRLDQTEK(12,n) + &                                                                          
                       ((sum(VMTHH(N,iregn,12,1:maxvtyp)) / MPGTECH(12,n)) * MG_HHV)*BENLG(iregn,n)+FLTFCLDVBTUR(iregn,12) 
!...  FCV Methanol
      TRLDQTEK(13,n) = TRLDQTEK(13,n) + &                                                                         
                       ((sum(VMTHH(N,iregn,13,1:maxvtyp)) / MPGTECH(13,n)) * MG_HHV) * BENME(iregn,n) + FLTFCLDVBTUR(iregn,13)
!...  FCV Hydrogen
      TRLDQTEK(14,n) = TRLDQTEK(14,n) + &                                                                         
                       ((sum(VMTHH(N,iregn,14,1:maxvtyp)) / MPGTECH(14,n)) * MG_HHV) * BENHY(iregn,n) + FLTFCLDVBTUR(iregn,14)
!...  Electric - 300  
      TRLDQTEK(15,n) =  TRLDQTEK(15,n) + &                                                                          
                       ((sum(VMTHH(N,iregn,15,1:maxvtyp))/MPGTECH(15,n))*MG_HHV)*BENEL(iregn,n) ! +FLTFCLDVBTUR(iregn,15)
!...  HEV Gasoline
      TRLDQTEK(16,n) = TRLDQTEK(16,n) + &                                                                         
                       ((sum(VMTHH(N,iregn,16,1:maxvtyp)) / MPGTECH(16,n)) * MG_HHV) * BENMG(iregn,n) + FLTFCLDVBTUR(iregn,16)

    enddo

! ... ***** TABLE 52 *******
! ... Fuel efficiency - new conventional cars

! ... Fuel efficiency - new conventional cars and light trucks
      DO ICL=1,MAXCLASS
		TREFFCAR(ICL,N) = LDV_MPG_CL(1,GAS,ICL,YRS)
		TREFFTRK(ICL,N) = LDV_MPG_CL(2,GAS,ICL,YRS)
		if(ICL.ge.7.and.curcalyr.lt.2011) then
			!zero out CUV copied attributes prior to introduction in 2011
			TREFFCAR(ICL,N) = 0
			TREFFTRK(ICL,N) = 0	
		endif
	  enddo
      TREFFCAR(MAXCLASS+1,N) = MPGC(GAS,N)
      TREFFCAR(MAXCLASS+2,N) = MPGC(GAS,N) * CDFRFG(N,1)            ! average new cars on road
      TREFFTRK(MAXCLASS+1,N) = MPGT(GAS,N)
      TREFFTRK(MAXCLASS+2,N) = MPGT(GAS,N) * LTDFRFG(N,1)           ! average new light trucks on road

! ... Degradation factors
      DEGRPT(1,N) = CDFRFG(N,1)
      DEGRPT(2,N) = LTDFRFG(N,1)

! ... Fuel efficiency - new AFVs
      DO ICL=1,MAXCLASS
        TREFFALTC(ICL,N) = AFVFE(1,ICL,N)
        TREFFALTT(ICL,N) = AFVFE(2,ICL,N)
		if(ICL.ge.7.and.curcalyr.lt.2011) then
			!zero out CUV copied attributes prior to introduction in 2011
			TREFFALTC(ICL,N) = 0
			TREFFALTT(ICL,N) = 0	
		endif
      ENDDO

      TREFFALTC(MAXCLASS+1,N) = AFVFETOT(1,N)
      TREFFALTT(MAXCLASS+1,N) = AFVFETOT(2,N)
	  
      TREFFFLT(1,N) = FLTMPGTOT2(1)
      TREFFFLT(2,N) = FLTMPGTOT2(2)
      TREFFFLT(3,N) = FLTTOTMPG(1)
      TREFFFLT(4,N) = FLTTOTMPG(2)

! ... Size Class Sales shares	  
	  !output combined household and fleet class sales shares for all fuel types (aka all vehicles)
	  DO ICL=1,MAXCLASS
		TRSLSHRC(ICL,N) = sum(TOTALSALSC(1,ICL,1:maxldv,N))/sum(TOTALSALSC(1,1:MAXCLASS,1:maxldv,N))
		TRSLSHRT(ICL,N) = sum(TOTALSALSC(2,ICL,1:maxldv,N))/sum(TOTALSALSC(2,1:MAXCLASS,1:maxldv,N))
	  enddo
	  
! ... Horsepower
      DO ICL=1,MAXCLASS
		TRHPCAR(ICL,N) = LDVHPW(1,GAS,ICL,YRS)
		TRHPTRK(ICL,N) = LDVHPW(2,GAS,ICL,YRS)
		if(ICL.ge.7.and.curcalyr.lt.2011) then
			!zero out CUV copied attributes prior to introduction in 2011
			TRHPCAR(ICL,N) = 0
			TRHPTRK(ICL,N) = 0	
		endif
	  enddo
	  TRHPCAR(MAXCLASS+1,N) = AHPCAR(11,N)
	  TRHPTRK(MAXCLASS+1,N) = AHPTRUCK(11,N)
	  
! ... Weight	  
      DO ICL=1,MAXCLASS
		TRWTCAR(ICL,N) = WGT(1,GAS,ICL,YRS)
		TRWTTRK(ICL,N) = WGT(2,GAS,ICL,YRS)
		if(ICL.ge.7.and.curcalyr.lt.2011) then
			!zero out CUV copied attributes prior to introduction in 2011
			TRWTCAR(ICL,N) = 0
			TRWTTRK(ICL,N) = 0	
		endif
	  enddo
      TRWTCAR(MAXCLASS+1,N) = AWTCAR(11,N)
	  TRWTTRK(MAXCLASS+1,N) = AWTTRUCK(11,N)
	  
!...FTAB Table 54 - Fleet vehicle sales
	FLTECHRPT(:,:,n)=0.0
    do IVTYP=1,maxvtyp
      do ILDV=1,maxldv
        FLTECHRPT(IVTYP,ILDV,n)=sum(FLTECH(mnumcr,IVTYP,1:maxfleet,ILDV,1:maxhav))/1000.0
      enddo
    enddo

!...Regionalize fleet sales for FTAB Table 48
	FLTECHRPT_REG(:,:,:,n)=0.0
    do IVTYP=1,maxvtyp
      do ILDV=1,maxldv
        do iregn=1,mnumcr-2
		  FLTECHRPT_REG(IVTYP,iregn,ILDV,n) = sum(FLTECH(iregn,IVTYP,1:maxfleet,ILDV,1:maxhav))/1000.0
        enddo
      enddo
    enddo

   FLTSALTOTC(n) = 0.0
   FLTSALTOTT(n) = 0.0
   FLTSALTOTV(n) = 0.0
   do ILDV=1,maxldv
     FLTSALTOTC(n)=FLTSALTOTC(n)+FLTECHRPT(1,ILDV,n)
     FLTSALTOTT(n)=FLTSALTOTT(n)+FLTECHRPT(2,ILDV,n)
   enddo
   FLTSALTOTV(n)=FLTSALTOTC(n)+FLTSALTOTT(n)

!...FTAB Table 55 - Fleet vehicle stocks
    do IVTYP=1,maxvtyp
      do ILDV=1,maxldv
        FLTECHSTKRPT(IVTYP,ILDV,n)=sum(TFLTECHSTK(IVTYP,1:maxfleet,ILDV,1:maxhav))/1000.0
      enddo
    enddo

    FLTSTKTOTC(n) = 0.0
    FLTSTKTOTT(n) = 0.0
    FLTSTKTOTV(n) = 0.0
    do ILDV=1,maxldv
      FLTSTKTOTC(n)=FLTSTKTOTC(n)+FLTECHSTKRPT(1,ILDV,n)
      FLTSTKTOTT(n)=FLTSTKTOTT(n)+FLTECHSTKRPT(2,ILDV,n)
    enddo
    FLTSTKTOTV(n)=FLTSTKTOTC(n)+FLTSTKTOTT(n)

!...FTAB Table 56 - Fleet vmt by ldv type
    do IVTYP=1,maxvtyp
      do ILDV=1,maxldv
        FLTECHVMTRPT(IVTYP,ILDV,n) = 0.0
        do ifleet=1,maxfleet
          FLTECHVMTRPT(IVTYP,ILDV,n) = FLTECHVMTRPT(IVTYP,ILDV,n) + &
                                       BFLTVMTECH(IVTYP,ifleet,ILDV)		   
        enddo
      enddo
    enddo


! ... ***** TABLE 58 *******
! ... Railroad tmt & efficiency
      TRTMRR(1,N) = sum(BRTMTT(n,1:mnumcr-2))
      TRTMRR(2,N) = 1/FREFF(n)     

! ... Domestic shipping
      TRTMSHIP(1,N) = sum(BSTMTT(n,1:mnumcr-2))
      TRTMSHIP(2,N) = 1.0 / DSEFF(n)

! ... International shipping
      TRIMSHIP(N) = TMC_IM(N)                  ! Real imports

! ... rail electricity demand passed to EMM
      do iregn=1,mnumcr-2      
        TRQRAILR(1,IREGN,N) = qmtrr(2,iregn,n)* benel(iregn,n)
        TRQRAILR(2,IREGN,N) = qmtrr(1,iregn,n)* bends(iregn,n)
      enddo

!...FTAB Table 48 new ldv sales by ldv type
!...Sum NCSTECH and NLTECH across 6 size classes
      DO IREGN=1,MNUMCR
        DO ILDV=1,MAXLDV
          SC_NCSTECH(IREGN,ILDV,N) = 0.0
          SC_NLTECH(IREGN,ILDV,N)  = 0.0
          DO ICL=1,MAXCLASS
            SC_NCSTECH(IREGN,ILDV,N) = SC_NCSTECH(IREGN,ILDV,N) + NCSTECH(IREGN,ICL,ILDV,N)
            SC_NLTECH(IREGN,ILDV,N)  = SC_NLTECH(IREGN,ILDV,N) + NLTECH(IREGN,ICL,ILDV,N)
          ENDDO
        ENDDO
      ENDDO
		
!...Calculate total vehicle sales by tech and region
	TRLDSALC(:,:,n)=0.0
	TRLDSALT(:,:,n)=0.0
    if(n.le.STOCKYR-1989) then
	  do iregn=1,mnumcr-2
	    do ILDV=1,maxldv
	      TRLDSALC(ILDV,iregn,n) = sum(LDV_STOCK(iregn,1,1:maxowner,ILDV,1,1,n))
		  TRLDSALT(ILDV,iregn,n) = sum(LDV_STOCK(iregn,2,1:maxowner,ILDV,1,1,n))
	    enddo
	  enddo
	else
      do ILDV=1,maxldv
        do iregn=1,mnumcr-2
!...      cars
          TRLDSALC(ILDV,iregn,n) = SC_NCSTECH(iregn,ILDV,n) + FLTECHRPT_REG(1,iregn,ILDV,n) / 1000.0
!...      trucks
          TRLDSALT(ILDV,iregn,n) = SC_NLTECH(iregn,ILDV,n) + FLTECHRPT_REG(2,iregn,ILDV,n) / 1000.0
        enddo
      enddo
	endif
	do ILDV=1,maxldv
      TRLDSALC(ILDV,11,n) = sum(TRLDSALC(ILDV,1:mnumcr-2,n))
	  TRLDSALT(ILDV,11,n) = sum(TRLDSALT(ILDV,1:mnumcr-2,n))
	enddo
	
	!write(21,*) 'total national car and light truck sales '
	!write(21,'(i6,2f16.6)') curcalyr, sum(TRLDSALC(1:maxldv,mnumcr,n)), sum(TRLDSALT(1:maxldv,mnumcr,n)) 

!...calculate total micro hybrid (ISG) vehicles sales
    do IVTYP=1,maxvtyp
      do ILDV=1,maxldv
        do iregn=1,mnumcr
          if(IVTYP.eq.1) trmicros(IVTYP,ILDV,iregn,n) = trldsalc(ILDV,iregn,n)*micropen(IVTYP,ILDV,n)
          if(IVTYP.eq.2) trmicros(IVTYP,ILDV,iregn,n) = trldsalt(ILDV,iregn,n)*micropen(IVTYP,ILDV,n) 
          if(ILDV.eq.4)  trmicros(IVTYP,ILDV,iregn,n) = 0.0 
          if(ILDV.eq.7)  trmicros(IVTYP,ILDV,iregn,n) = 0.0
		  if(ILDV.eq.15) trmicros(IVTYP,ILDV,iregn,n) = 0.0 
          if(ILDV.ge.13.and.ILDV.le.14) trmicros(IVTYP,ILDV,iregn,n) = 0.0 
        enddo
      enddo
    enddo

    do ILDV=1,maxldv
      do iregn=1,mnumcr
        if(ILDV.ge.5.and.ILDV.le.6) then
          trmicros(1,ILDV,iregn,n) = trldsalc(ILDV,iregn,n)
          trmicros(2,ILDV,iregn,n) = trldsalt(ILDV,iregn,n)
        endif
        if(ILDV.eq. 8) then
          trmicros(1,ILDV,iregn,n) = trldsalc(ILDV,iregn,n)
          trmicros(2,ILDV,iregn,n) = trldsalt(ILDV,iregn,n)
        endif 
        if(ILDV.eq.16) then
           trmicros(1,ILDV,iregn,n) = trldsalc(ILDV,iregn,n)
           trmicros(2,ILDV,iregn,n) = trldsalt(ILDV,iregn,n)
        endif
      enddo
    enddo


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
    do ILDV=1,maxldv
      TRLDMPGC(ILDV,n) = CCMPGLDV(ILDV,n)
      TRLDMPGT(ILDV,n) = TTMPGLDV(ILDV,n) 
    enddo

      TRLDMPGF(1,N) = TLDVMPG(1,N)
      TRLDMPGF(2,N) = TLDVMPG(2,N)
      TRLDMPGF(3,N) = TLDVMPG(3,N)

!...Calculate national stock-vmt weighted ldv fuel economy
	do ILDV=1,maxldv 
		NUM1=0.0
		DEN1=0.0
		NUM2=0.0
		DEN2=0.0
		NUM3=0.0
		DEN3=0.0
	  do iage=1,maxage	!add all the ages and regions for hh
	    do ihav = 1,maxhav
	      if(yrs.le.2011) then
		    if(cmpgstk(ILDV,iage,n,mnumcr)*cdfrfg(n,ihav).ne.0.0) then
			  NUM1=NUM1 + VMT_STK_HH(1,ILDV,iage,ihav,mnumcr)
			  DEN1=DEN1 + VMT_STK_HH(1,ILDV,iage,ihav,mnumcr) / (CMPGSTK(ILDV,iage,n,mnumcr)*CDFRFG(n,ihav))
		    endif
		    if(ttmpgstk(ILDV,iage,n,mnumcr)*ltdfrfg(n,ihav).ne.0.0) then
		      NUM2=NUM2 + VMT_STK_HH(2,ILDV,iage,ihav,mnumcr)
			  DEN2=DEN2 + VMT_STK_HH(2,ILDV,iage,ihav,mnumcr) / (TTMPGSTK(ILDV,iage,n,mnumcr)*LTDFRFG(n,ihav))	
		    endif
		  else	!after 2011, regionalize
	        do iregn = 1,mnumcr-2
		      if(cmpgstk(ILDV,iage,n,iregn)*cdfrfg(n,ihav).ne.0.0) then
			    NUM1=NUM1 + VMT_STK_HH(1,ILDV,iage,ihav,iregn)
			    DEN1=DEN1 + VMT_STK_HH(1,ILDV,iage,ihav,iregn) / (CMPGSTK(ILDV,iage,n,iregn)*CDFRFG(n,ihav))
		      endif
		      if(ttmpgstk(ILDV,iage,n,iregn)*ltdfrfg(n,ihav).ne.0.0) then
		        NUM2=NUM2 + VMT_STK_HH(2,ILDV,iage,ihav,iregn)
			    DEN2=DEN2 + VMT_STK_HH(2,ILDV,iage,ihav,iregn) / (TTMPGSTK(ILDV,iage,n,iregn)*LTDFRFG(n,ihav))
		      endif
		    enddo !end iregn loop
		  endif	!end regional year control
		enddo ! end ihav loop
	  enddo !end  iage loop

!...  sum fleet vmt and consumption by fleet type, which is already national
	  FLTNUM1=sum(fltvmtech(1,1:maxfleet,ILDV,1:maxhav))/1000000.0
      FLTNUM2=sum(fltvmtech(2,1:maxfleet,ILDV,1:maxhav))/1000000.0
      FLTDEN1=sum(fltldvc(1,1:maxfleet,ILDV,n))
      FLTDEN2=sum(fltldvc(2,1:maxfleet,ILDV,n))
 
!...  add fleet vmt and consumption to household vehicles (for each ILDV)
	!after fuel economy works, remove these extra vmt variables
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
		VMT_STK_TOT(IVTYP,ILDV) = sum(VMT_STK_HH(IVTYP,ILDV,1:maxage,1:maxhav,mnumcr))+sum(fltvmtech(IVTYP,1:maxfleet,ILDV,1:maxhav))/1000000.0
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

!...FTAB Table 51 - ldv vmt by ldv type

!...replaced usage of TRLDVMT(ILDV,n), but it is a tran global variable set in tranrep.f
!...so its calculation remains in case it is used elsewhere

!...TRLDVMT is for reporting only
    do ILDV=1,maxldv
	!BFLTVMTECH already converted to billions
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

!...FTAB TABLES 113 - 115 
!Write out fuel economy, vehicle prices, and ranges by ldv type and size class
	do IVTYP=1,MAXVTYP
	  do ILDV=1,MAXLDV
	    do ICL=1,MAXCLASS
			if (ICL.ge.7.and.curcalyr.lt.2011) then
				!zero out CUV copied attributes prior to 2011
				LDVMPG(IVTYP,ILDV,ICL,yrs) = 0.0
				LDVPRI(IVTYP,ILDV,ICL,yrs) = 0.0
				LDVRNG(IVTYP,ILDV,ICL,yrs) = 0.0	
			else
				LDVMPG(IVTYP,ILDV,ICL,yrs) = LDV_MPG_CL(IVTYP,ILDV,ICL,yrs)
				LDVPRI(IVTYP,ILDV,ICL,yrs) = LDV_PRI(IVTYP,ILDV,ICL,yrs)
				LDVRNG(IVTYP,ILDV,ICL,yrs) = LDV_RNG(IVTYP,ILDV,ICL,yrs)
			endif
		enddo
	  enddo
	enddo
	
  RETURN
  END SUBROUTINE TREPORT
  
! ==========================================================================================================
! ... Subroutine IEOVARS generates variables that have not already been created for the ftab in the AEO
! ... but are needed for the IEO report writer
! ==========================================================================================================
    SUBROUTINE IEOVARS
    USE T_
    IMPLICIT NONE
	
	REAL  LDVSHR,TECHTTL,SHRCALC(8),MPG_CALC(8)


!...Passenger Travel (billion vehicle miles travelled) 		
	! 2/3 Wheelers
	if(curcalyr.le.CycHistYR)	then
	  PAS_RPM(1,n) = Cyc_RPM(n)
	else
	  PAS_RPM(1,n) = (TRLDVMTE(1,N)/TRLDVMTE(1,n-1))*PAS_RPM(1,n-1)
	endif
	! Bus
	PAS_RPM(2,n)=(sum(tmod(1:2,n))+tbpmt(11,n))/1000.0	!transit 
	! Rail (passenger miles travelled)
	PAS_RPM(3,n)=(sum(trrpm(1:9,n))+sum(crrpm(1:9,n))+irrpm(n))/1000.0
  
!...Freight Travel (billion ton miles travelled)	
    TRK_TMT(:,n) = 0.0
	if(curcalyr.gt.2012)then	
      ! Light Truck - class 3
      TRK_TMT(1,n)=(sum(tfr_vmt_fas_t(n,1,1:9,2))/sum(tfr_vmt_fas_t(23,1,1:9,2)))*69.8
      ! Medium Truck - class 4-6
      TRK_TMT(2,n)=(sum(tfr_vmt_fas_t(n,2,1:9,2))/sum(tfr_vmt_fas_t(23,2,1:9,2)))*188.6
      ! Heavy Truck
      TRK_TMT(3,n)=(sum(tfr_vmt_fas_t(n,3,1:9,2))/sum(tfr_vmt_fas_t(23,3,1:9,2)))*1635.6
    endif
 
 !...Energy Efficiency Indicators (mpg) (weighted average for LDV and CLT)
	! Light Duty Vehicle - Standard	Eff
	ldvshr=(tncsale(11,n)+tntsale(11,n))/(tncsale(11,n)+tntsale(11,n)+(cltsalt(10,n)*.000001))

	LDV_MPG(1,n)=(ldvshr/cafestd(3,n)+(1-ldvshr)/ncltmpgt(n))**-1  
	! Light Duty Vehicle - New mpg
	LDV_MPG(2,n)=(ldvshr/truempg(3,n)+(1-ldvshr)/ncltmpgt(n))**-1
	! Light Duty Vehicle - Stock mpg
	ldvshr=trldvmte(1,n)/(trldvmte(1,n)+bcltvmt(10,n))
	LDV_MPG(3,n)=(ldvshr/trldmpgf(3,n)+(1-ldvshr)/cltmpgt(n))**-1

!...Energy Efficiency Indicators (ton miles/thousand BTU)		
	! International Marine
    ! there isn't one in AEO
  
! Consumption by Fuel Type within Mode (billion btu)
  ! 2/3 Wheelers		
	TTHcons(1,n) = TRQHWY(3,N)                                   ! Motor Gasoline
	TTHcons(2,n) = 0.0                                           ! Diesel
	TTHcons(3,n) = 0.0                                           ! Liquid Petroleum Gas
	TTHcons(4,n) = 0.0                                           ! Electric
	TTHcons(5,n) = 0.0                                           ! Natural Gas	
	TTHcons(6,n) = 0.0                                           ! Hydrogen
 ! Freight
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


!...Light Duty Vehicle Miles per Gallon (MPG) by Technology Type (includes cars, light trucks and CLT)	
 !..New Light Duty Vehicle fuel economy TECHMPG - sales weighted
    techmpg(:,n)=0.0
  !ICE or Hybrid Motor Gasoline
  ! total sales for tech
	techttl=trldsalc(1,11,n)  + & ! gasoline car
	        trldsalc(3,11,n)  + & ! FFV car
			trldsalc(16,11,n) + & ! HEV car
	        trldsalt(1,11,n)  + & ! gasoline lt
	        trldsalt(3,11,n)  + & ! FFV lt
			trldsalt(16,11,n) + & ! HEV lt
			cltsalt(1,n)/1000000.0 + & ! gasoline clt
			cltsalt(5,n)/1000000.0     ! FFV clt
!	write(21,*)'NEW car =',trldsalc(1,11,n),' lt =',trldsalt(1,11,n),' clt =',cltsalt(1,n)
			
  ! sales shares						
    shrcalc(1)=trldsalc(1,11,n)/techttl
	shrcalc(2)=trldsalc(3,11,n)/techttl
	shrcalc(3)=trldsalc(16,11,n)/techttl
    shrcalc(4)=trldsalt(1,11,n)/techttl
    shrcalc(5)=trldsalt(3,11,n)/techttl		
    shrcalc(6)=trldsalt(16,11,n)/techttl
	shrcalc(7)=(cltsalt(1,n)/1000000.0)/techttl
	shrcalc(8)=(cltsalt(5,n)/1000000.0)/techttl
  ! fuel economy weighting		
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then
	    if(i.eq.1.and.trldmpgc(1,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(1,n)
		if(i.eq.2.and.trldmpgc(3,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(3,n)
		if(i.eq.3.and.trldmpgc(16,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(16,n)
	    if(i.eq.4.and.trldmpgt(1,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(1,n)
		if(i.eq.5.and.trldmpgt(3,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(3,n)
		if(i.eq.6.and.trldmpgt(16,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(16,n)
		if(i.eq.7.and.ncltmpg(n,1).gt.0.0) mpg_calc(i)=shrcalc(i)/ncltmpg(n,1)
		if(i.eq.8.and.ncltmpg(n,5).gt.0.0) mpg_calc(i)=shrcalc(i)/ncltmpg(n,5)
	  endif
	enddo
	  
    shrcalc=0.0
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) techmpg(1,n)=(sum(mpg_calc(1:8)))**-1
!	write(21,*)'New ICE/HEV Gas MPG =',techmpg(1,n)		
  !ICE or Hybrid Diesel
  ! total sales for tech
	techttl=trldsalc(2,11,n)  + & ! diesel car
	        trldsalc(8,11,n)  + & ! HEV diesel car
			trldsalt(2,11,n)  + & ! diesel lt
	        trldsalt(8,11,n)  + & ! HEV diesel lt
			cltsalt(2,n)/1000000.0 ! diesel clt
  ! sales shares						
    shrcalc(1)=trldsalc(2,11,n)/techttl
	shrcalc(2)=trldsalc(8,11,n)/techttl
	shrcalc(3)=trldsalt(2,11,n)/techttl
    shrcalc(4)=trldsalt(8,11,n)/techttl		
    shrcalc(5)=(cltsalt(2,n)/1000000.0)/techttl
  ! fuel economy weighting
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then		
	    if(i.eq.1.and.trldmpgc(2,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(2,n)
		if(i.eq.2.and.trldmpgc(8,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(8,n)
	    if(i.eq.3.and.trldmpgt(2,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(2,n)
		if(i.eq.4.and.trldmpgt(8,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(8,n)
		if(i.eq.5.and.ncltmpg(n,2).gt.0.0) mpg_calc(i)=shrcalc(i)/ncltmpg(n,2)
	  endif
	enddo
    shrcalc=0.0	
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) techmpg(2,n)=(sum(mpg_calc(1:8)))**-1
!	write(21,*)'New ICE/HEV Diesel MPG =',techmpg(2,n)			
  !ICE Natural Gas
  ! total sales for tech
    techttl=trldsalc(9,11,n)  + & ! bi-cng car
			trldsalc(11,11,n) + & ! cng car
	        trldsalt(9,11,n)  + & ! bi-cng lt
			trldsalt(11,11,n) + & ! cng lt
			cltsalt(4,n)/1000000.0 ! cng clt
  ! sales shares						
    shrcalc(1)=trldsalc(9,11,n)/techttl
	shrcalc(2)=trldsalc(11,11,n)/techttl
    shrcalc(3)=trldsalt(9,11,n)/techttl
    shrcalc(4)=trldsalt(11,11,n)/techttl		
	shrcalc(5)=(cltsalt(4,n)/1000000.0)/techttl
  ! fuel economy weighting		
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then		
	    if(i.eq.1.and.trldmpgc(9,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(9,n)
		if(i.eq.2.and.trldmpgc(11,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(11,n)
	    if(i.eq.3.and.trldmpgt(9,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(9,n)
		if(i.eq.4.and.trldmpgt(11,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(11,n)
		if(i.eq.5.and.ncltmpg(n,4).gt.0.0) mpg_calc(i)=shrcalc(i)/ncltmpg(n,4)
	  endif
	enddo
    shrcalc=0.0
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) techmpg(3,n)=(sum(mpg_calc(1:8)))**-1
!	write(21,*)'New CNG MPG =',techmpg(3,n)	
  !ICE LPG
  ! total sales for tech
    techttl=trldsalc(10,11,n) + & ! bi-cng car
			trldsalc(12,11,n) + & ! cng car
	        trldsalt(10,11,n) + & ! bi-cng lt
			trldsalt(12,11,n) + & ! cng lt
			cltsalt(3,n)/1000000.0 ! cng clt
  ! sales shares						
    shrcalc(1)=trldsalc(10,11,n)/techttl
	shrcalc(2)=trldsalc(12,11,n)/techttl
    shrcalc(3)=trldsalt(10,11,n)/techttl
    shrcalc(4)=trldsalt(12,11,n)/techttl		
	shrcalc(5)=(cltsalt(3,n)/1000000.0)/techttl
  ! fuel economy weighting		
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then		
	    if(i.eq.1.and.trldmpgc(10,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(10,n)
		if(i.eq.2.and.trldmpgc(12,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(12,n)
	    if(i.eq.3.and.trldmpgt(10,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(10,n)
		if(i.eq.4.and.trldmpgt(12,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(12,n)
		if(i.eq.5.and.ncltmpg(n,3).gt.0.0) mpg_calc(i)=shrcalc(i)/ncltmpg(n,3)
	  endif
	enddo
    shrcalc=0.0
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) techmpg(4,n)=(sum(mpg_calc(1:8)))**-1
!	write(21,*)'New LPG MPG =',techmpg(4,n)		
  !ICE or Hybrid Other
    techmpg(5,n)=0.0
  
  !Electric Vehicles
  ! total sales for tech
	techttl=trldsalc(4,11,n) + & ! ev100 car
			trldsalc(7,11,n) + & ! ev200 car
			trldsalc(15,11,n) + & ! ev300 car 
	        trldsalt(4,11,n) + & ! ev100 lt
			trldsalt(7,11,n) + & ! ev200 lt
			trldsalt(15,11,n) + & ! ev300 lt 
			cltsalt(6,n)/1000000.0 ! ev clt
  ! sales shares						
    shrcalc(1)=trldsalc(4,11,n)/techttl
	shrcalc(2)=trldsalc(7,11,n)/techttl
	shrcalc(3)=trldsalc(15,11,n)/techttl 
    shrcalc(4)=trldsalt(4,11,n)/techttl
    shrcalc(5)=trldsalt(7,11,n)/techttl	
	shrcalc(6)=trldsalt(15,11,n)/techttl 
	shrcalc(7)=(cltsalt(6,n)/1000000.0)/techttl
  ! fuel economy weighting		
	do i=1,8
	  mpg_calc(i)=0.0
!fixing 37000 mpg in 1999 from neglibibly low shrcalc
	  if(shrcalc(i).gt.0.0001) then 
		if(yrs.eq.1999.and.i.eq.1) shrcalc(i)=1.0	
	    if(i.eq.1.and.trldmpgc(4,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(4,n)
		if(i.eq.2.and.trldmpgc(7,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(7,n)
		if(i.eq.3.and.trldmpgc(15,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(15,n)
	    if(i.eq.4.and.trldmpgt(4,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(4,n)
		if(i.eq.5.and.trldmpgt(7,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(7,n)
		if(i.eq.6.and.trldmpgt(15,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(15,n)
		if(i.eq.7.and.ncltmpg(n,6).gt.0.0) mpg_calc(i)=shrcalc(i)/ncltmpg(n,6)
	  endif
	enddo
    shrcalc=0.0
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) techmpg(6,n)=(sum(mpg_calc(1:8)))**-1
!	write(21,*)'New EV MPG =',techmpg(6,n)		
  !Hydrogen Fuel Cell
  ! total sales for tech
	techttl=trldsalc(14,11,n) + & ! fcv car
	        trldsalt(14,11,n) + & ! fcv lt
			cltsalt(9,n)/1000000.0 ! fcv clt
  ! sales shares						
    shrcalc(1)=trldsalc(14,11,n)/techttl
	shrcalc(2)=trldsalt(14,11,n)/techttl
    shrcalc(3)=(cltsalt(9,n)/1000000.0)/techttl
  ! fuel economy weighting		
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then
	    if(i.eq.1.and.trldmpgc(14,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(14,n)
		if(i.eq.2.and.trldmpgt(14,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(14,n)
		if(i.eq.3.and.ncltmpg(n,9).gt.0.0) mpg_calc(i)=shrcalc(i)/ncltmpg(n,9)
	  endif
	enddo
    shrcalc=0.0
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) techmpg(7,n)=(sum(mpg_calc(1:8)))**-1
!	write(21,*)'New FCV MPG =',techmpg(7,n)	  
  !Plug In Hybrids
  ! total sales for tech
	techttl=trldsalc(5,11,n)  + & ! PHEV20 gasoline car
	        trldsalc(6,11,n)  + & ! PHEV50 gasoline car
			trldsalt(5,11,n)  + & ! PHEV20 gasoline lt
	        trldsalt(6,11,n)  + & ! PHEV50 gasoline lt
			cltsalt(7,n)/1000000.0 + & ! phev gasoline clt
			cltsalt(8,n)/1000000.0     ! phev diesel clt - Melissa (mly) don't forget about this diesel phev hiding with the gasoline versions!!!! 
  ! sales shares						
    shrcalc(1)=trldsalc(5,11,n)/techttl
	shrcalc(2)=trldsalc(6,11,n)/techttl
    shrcalc(3)=trldsalt(5,11,n)/techttl
    shrcalc(4)=trldsalt(6,11,n)/techttl		
	shrcalc(5)=(cltsalt(7,n)/1000000.0)/techttl
	shrcalc(6)=(cltsalt(8,n)/1000000.0)/techttl
  ! fuel economy weighting		
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then
	    if(i.eq.1.and.trldmpgc(5,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(5,n)
		if(i.eq.2.and.trldmpgc(6,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgc(6,n)
	    if(i.eq.3.and.trldmpgt(5,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(5,n)
		if(i.eq.4.and.trldmpgt(6,n).gt.0.0) mpg_calc(i)=shrcalc(i)/trldmpgt(6,n)
		if(i.eq.5.and.ncltmpg(n,7).gt.0.0) mpg_calc(i)=shrcalc(i)/ncltmpg(n,7)
		if(i.eq.6.and.ncltmpg(n,8).gt.0.0) mpg_calc(i)=shrcalc(i)/ncltmpg(n,8)
	  endif
	enddo
    shrcalc=0.0
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) techmpg(8,n)=(sum(mpg_calc(1:8)))**-1
!	write(21,*)'New PHEV MPG =',techmpg(8,n)	
!...Light Duty Vehicle Stock MPG - vmt weighted	
    stkmpg(:,n)=0.0	

  ! total vmt for tech 
	techttl=BVMTECH(1,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,1)) + & ! gasoline car/lt	
	        BVMTECH(3,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,3)) + & ! FFV car/lt		
			BVMTECH(16,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,16)) + & ! HEV car/lt		
	        bcltvmt(1,n)  + & ! gasoline clt
			bcltvmt(5,n)      ! FFV clt

  ! vmt shares						
    shrcalc(1)=(BVMTECH(1,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,1)))/techttl			
	shrcalc(2)=(BVMTECH(3,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,3)))/techttl			
	shrcalc(3)=(BVMTECH(16,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,16)))/techttl			
    shrcalc(4)=bcltvmt(1,n)/techttl
	shrcalc(5)=bcltvmt(5,n)/techttl

  ! fuel economy weighting		
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then
	    if(i.eq.1) mpg_calc(i)=shrcalc(i)/mpgtech(1,n)
		if(i.eq.2) mpg_calc(i)=shrcalc(i)/mpgtech(3,n)
		if(i.eq.3) mpg_calc(i)=shrcalc(i)/mpgtech(16,n)
	    if(i.eq.4) mpg_calc(i)=shrcalc(i)/cltmpg(n,1)
		if(i.eq.5) mpg_calc(i)=shrcalc(i)/cltmpg(n,5)
	  endif
	enddo
    shrcalc=0.0	
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) stkmpg(1,n)=(sum(mpg_calc(1:8)))**-1  

  ! total vmt for tech  
	techttl=BVMTECH(2,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,2))  + & ! diesel car/lt		
	        BVMTECH(8,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,8))  + & ! diesel HEV car/lt	
			bcltvmt(2,n)      ! diesel clt
  ! vmt shares						
    shrcalc(1)=(BVMTECH(2,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,2)))/techttl
	shrcalc(2)=(BVMTECH(8,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,8)))/techttl
	shrcalc(3)=bcltvmt(2,n)/techttl
  ! fuel economy weighting	
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then
	    if(i.eq.1) mpg_calc(i)=shrcalc(i)/mpgtech(2,n)
		if(i.eq.2) mpg_calc(i)=shrcalc(i)/mpgtech(8,n)
	    if(i.eq.3) mpg_calc(i)=shrcalc(i)/cltmpg(n,2)
	  endif
	enddo
    shrcalc=0.0
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) stkmpg(2,n)=(sum(mpg_calc(1:8)))**-1  

  ! total vmt for tech 
	techttl=BVMTECH(9,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,9))  + & ! bi-cng car/lt
	        BVMTECH(11,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,11)) + & ! cng car/lt
			bcltvmt(4,n)      ! cng clt
  ! vmt shares								
    shrcalc(1)=(BVMTECH(9,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,9)))/techttl
	shrcalc(2)=(BVMTECH(11,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,11)))/techttl
	shrcalc(3)=bcltvmt(4,n)/techttl
  ! fuel economy weighting	
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then
	    if(i.eq.1) mpg_calc(i)=shrcalc(i)/mpgtech(9,n)
		if(i.eq.2) mpg_calc(i)=shrcalc(i)/mpgtech(11,n)
	    if(i.eq.3) mpg_calc(i)=shrcalc(i)/cltmpg(n,4)
	  endif
	enddo
    shrcalc=0.0
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) stkmpg(3,n)=(sum(mpg_calc(1:8)))**-1  

  ! total vmt for tech  
	techttl=BVMTECH(10,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,10)) + & ! bi-lpg car/lt
	        BVMTECH(12,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,12)) + & ! lpg car/lt
			bcltvmt(3,n)      ! lpg clt
  ! vmt shares						
    shrcalc(1)=(BVMTECH(10,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,10)))/techttl
	shrcalc(2)=(BVMTECH(12,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,12)))/techttl
	shrcalc(3)=bcltvmt(3,n)/techttl
  ! fuel economy weighting		
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then
	    if(i.eq.1) mpg_calc(i)=shrcalc(i)/mpgtech(10,n)
		if(i.eq.2) mpg_calc(i)=shrcalc(i)/mpgtech(12,n)
	    if(i.eq.3) mpg_calc(i)=shrcalc(i)/cltmpg(n,3)
	  endif
	enddo
    shrcalc=0.0   
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) stkmpg(4,n)=(sum(mpg_calc(1:8)))**-1 
  !ICE or Hybrid Other
    stkmpg(5,n)=0.0

  ! total vmt for tech  
	techttl=BVMTECH(4,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,4)) + & ! ev100 car/lt
	        BVMTECH(7,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,7)) + & ! ev200 car/lt
			BVMTECH(15,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,15)) + & ! ev300 car/lt
			bcltvmt(6,n)     ! ev clt
  ! vmt shares						
    shrcalc(1)=(BVMTECH(4,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,4)))/techttl
	shrcalc(2)=(BVMTECH(7,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,7)))/techttl
	shrcalc(3)=(BVMTECH(15,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,15)))/techttl
	shrcalc(4)=bcltvmt(6,n)/techttl
  ! fuel economy weighting		
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then
	    if(i.eq.1) mpg_calc(i)=shrcalc(i)/mpgtech(4,n)
		if(i.eq.2) mpg_calc(i)=shrcalc(i)/mpgtech(7,n)
		if(i.eq.3) mpg_calc(i)=shrcalc(i)/mpgtech(15,n)
	    if(i.eq.4) mpg_calc(i)=shrcalc(i)/cltmpg(n,6)
	  endif
	enddo
    shrcalc=0.0    
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) stkmpg(6,n)=(sum(mpg_calc(1:8)))**-1  

  ! total vmt for tech
	techttl=BVMTECH(14,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,14)) + & ! fcv car/lt
	        bcltvmt(9,n)      ! fcv clt
  ! vmt shares						
    shrcalc(1)=(BVMTECH(14,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,14)))/techttl
	shrcalc(2)=bcltvmt(9,n)/techttl
  ! fuel economy weighting		
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then
	    if(i.eq.1) mpg_calc(i)=shrcalc(i)/mpgtech(14,n)
		if(i.eq.2) mpg_calc(i)=shrcalc(i)/cltmpg(n,9)
	  endif
	enddo
    shrcalc=0.0    
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) stkmpg(7,n)=(sum(mpg_calc(1:8)))**-1   

  ! total vmt for tech  
	techttl=BVMTECH(5,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,5)) + & ! PHEV20 car/lt
	        BVMTECH(6,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,6)) + & ! PHEV50 car/lt
			bcltvmt(7,n) + & ! phev clt
			bcltvmt(8,n)     ! phev clt DIESEL!!! Melissa (mly) don't forget about this one either!!!
  ! vmt shares						
    shrcalc(1)=(BVMTECH(5,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,5)))/techttl
	shrcalc(2)=(BVMTECH(6,mnumcr) + sum(BFLTVMTECH(1:maxvtyp,1:maxfleet,6)))/techttl
	shrcalc(3)=bcltvmt(7,n)/techttl
	shrcalc(4)=bcltvmt(8,n)/techttl		
  ! fuel economy weighting	
	do i=1,8
	  mpg_calc(i)=0.0
	  if(shrcalc(i).gt.0.0) then
	    if(i.eq.1) mpg_calc(i)=shrcalc(i)/mpgtech(5,n)
		if(i.eq.2) mpg_calc(i)=shrcalc(i)/mpgtech(6,n)
	    if(i.eq.3) mpg_calc(i)=shrcalc(i)/cltmpg(n,7)
	    if(i.eq.4) mpg_calc(i)=shrcalc(i)/cltmpg(n,8)		
	  endif
	enddo
    shrcalc=0.0    
  ! fuel economy calculation	
    if(sum(mpg_calc(1:8)).gt.0.0) stkmpg(8,n)=(sum(mpg_calc(1:8)))**-1   
 !	write(21,*)'Stk PHEV MPG =',stkmpg(8,n)	 
    END SUBROUTINE IEOVARS

! ==========================================================================================================
! ... Subroutine PRNTRSLT prints diagnostic and summary output  
! ==========================================================================================================
    SUBROUTINE PRNTRSLT
    USE T_
    IMPLICIT NONE

      INTEGER YL,IYY
      REAL SHARE_SUM
      LOGICAL PRINT_DEBUG

      DO 3000 IYR = BYR, IJUMPCALYR
         DO 2000 IGP = 1, MAXGROUP
            WRITE(O_UNIT,9901) FF, IYR
            WRITE(O_UNIT,9902) GROUPLABEL(IGP)
            DO 1000 ICL = 1, MAXCLASS
               DO 900 ILDV = 1, MAXLDV
                  IF (.NOT. CLASSFLAG(ICL,IGP,ILDV)) GO TO 900
                  WRITE(O_UNIT,9903) CLASSLABEL(ICL,IGP), &
                                     FTYPELABEL(ILDV), &
                                     FE(ICL,IGP,IYR,ILDV), &
                                     WEIGHT(ICL,IGP,IYR,ILDV), &
                                     HP(ICL,IGP,IYR,ILDV), &
                                     PRICE(ICL,IGP,IYR,ILDV), &
                                     PRICEHI(ICL,IGP,IYR,ILDV),&
                                     VOLUME(ICL,IGP,IYR,ILDV), &
                                     RANGE(ICL,IGP,IYR,ILDV)
  900          CONTINUE
 1000       CONTINUE
 2000    CONTINUE
 3000 CONTINUE

 3500 IF (.NOT. PRINT_TECH) GO TO 7500

      DO 7000 IGP = 1, MAXGROUP
         DO 6000 ICL = 1, MAXCLASS
            DO 5500 ILDV = 1, MAXLDV
               IF (.NOT. CLASSFLAG(ICL,IGP,ILDV)) GO TO 6000

!... Electrics and Fuel Cell vehicles have none of these technologies
               IF (ILDV.eq.4.or.ILDV .EQ. 7 .OR. (ILDV .GE. 13 .AND. ILDV .LE. 15)) GO TO 6000 

               IF (IGP .LE. 2) IVTYP = 1
               IF (IGP .GE. 3) IVTYP = 2
      PRINT_DEBUG = .FALSE.
      IF (IGP .LE. 2 .AND. ICL .EQ. 4) PRINT_DEBUG = .TRUE.
      IF (IGP .GE. 3 .AND. ICL .EQ. 3) PRINT_DEBUG = .TRUE.
      IF (.NOT. PRINT_DEBUG) GO TO 6000
               DO 5000 IYR = BYR, IJUMPCALYR-1, 10
                  IF (IYR .LT. IJUMPCALYR-11) YL = IYR+9
                  IF (IYR .GE. IJUMPCALYR-11) YL = IJUMPCALYR
                  WRITE(O_UNIT,9904) FF, GROUPLABEL(IGP), &
                                     CLASSLABEL(ICL,IGP), &
                                     FTYPELABEL(ILDV)
                  WRITE(O_UNIT,9905) (IYY, IYY=IYR,YL)

! ... NEXT WRITE PUTS A SPACE.  USED BECAUSE FORMAT 9905 MAY
! ... NOT USE ALL 11 OUTPUTS SO A FINAL LINE FEED MIGHT NOT APPEAR

                  WRITE(O_UNIT,9906)

                  DO 4000 ITECH = 1, NUMTECH
                     WRITE(O_UNIT,9907) TECHLABEL(ITECH,IVTYP), &
                           (MKT_PEN(ICL,IGP,ITECH,IYY,ILDV), IYY=IYR, YL)
 4000             CONTINUE
 5000          CONTINUE
 5500       CONTINUE
 6000    CONTINUE
 7000 CONTINUE

 7500 CONTINUE

      WRITE(O_UNIT,9908) FF
      DO 8000 IYR = BYR, IJUMPCALYR
         WRITE(O_UNIT,9909) IYR, &
                            CAFE_STAND(1,IYR), GASMPG_ACTUAL(1,IYR),&
                            CAFE_STAND(2,IYR), GASMPG_ACTUAL(2,IYR),&
                            CAFE_STAND(3,IYR), GASMPG_ACTUAL(3,IYR),&
                            CAFE_STAND(4,IYR), GASMPG_ACTUAL(4,IYR)
 8000 CONTINUE

      DO 8500 IGP = 1, MAXGROUP
         WRITE(O_UNIT,9912) FF, &
                            'Vehicle Class Market Shares for C.A.F.E.',&
                            GROUPLABEL(IGP)
         DO 8400 IYR = BYR, IJUMPCALYR
            SHARE_SUM = 0
            DO 8300 ICL = 1, MAXCLASS
               SHARE_SUM = SHARE_SUM + CLASS_SHARE(mnumcr,ICL,IGP,IYR)
 8300       CONTINUE
            WRITE(O_UNIT,9913) IYR, (CLASS_SHARE(mnumcr,ICL,IGP,IYR),ICL=1,MAXCLASS), SHARE_SUM
 8400    CONTINUE
 8500 CONTINUE

      RETURN

 9901 FORMAT(A1, T45, 'FEM Results for ', I4)
 9902 FORMAT(//, T5, ' Summary of Results for ', A, ' Classes', //, &
       T57, 'Fuel', T99, 'Lo Sales', T114,'Hi Sales',/, &
       T15, 'Class', T55, 'Economy', T70, 'Weight', &
       T89, 'HP', T101, 'Price', T116,'Price',T130, 'Volume', &
       T145, 'Range', /)
 9903 FORMAT(T2, A20, A15, T50, F12.2, 3X, 6(F12.0,3X))
 9904 FORMAT(A1, T30, 'Technology Market Penetration for ', &
       A15, ' ', A20, ' ',A15)
 9905 FORMAT(//, T5, 'Technology', T33, 11I9)
 9906 FORMAT('  ')
 9907 FORMAT(T2, A30, T33, 11F9.3)
 9908 FORMAT(A1, T45, 'C.A.F.E. Results', //, &
       T20, 'Domestic Cars', T41, 'Import Cars', &
       T58, 'Domestic Trucks', T80, 'Import Trucks', /, &
       T5, 'Year', T17, 'Standard', T29, 'Actual', &
       T37, 'Standard', T49, 'Actual', &
       T57, 'Standard', T69, 'Actual', &
       T77, 'Standard', T89, 'Actual', /)
 9909 FORMAT(T5, I4, T15, 8F10.2)
 9910 FORMAT(I4,A15,A30,A15,F6.2,F6.0,F6.0,F6.0,F6.0)
 9912 FORMAT (A1, T45, A, /,  T57, A, //)
 9913 FORMAT (T5, I4, 8F10.3)

    END SUBROUTINE PRNTRSLT

! ==========================================================================================================
! ... Subroutine FEMRANGE calculates vehicle range estimates
! ==========================================================================================================
    SUBROUTINE FEMRANGE
    USE T_
    IMPLICIT NONE

      INTEGER       ATVYEAR

      ATVYEAR = MIN(YRS,XYR)

      DO IGP=1,MAXGROUP
        DO ICL=1,MAXCLASS
          DO ILDV=1,MAXLDV
            IF (.NOT. CLASSFLAG(ICL,IGP,ILDV)) CYCLE
			if(IGP.le.5) then
				RANGE(ICL,IGP,CURRENT,ILDV) = TANKSIZE(ICL,IGP,CURRENT,1) * &
                                           FE(ICL,IGP,CURRENT,GAS) *  &
                                          (1+AFVADJRN(ILDV,ATVYEAR)) * &
										  0.7	!accounting for on-road degradation and perceived range(greater than that for energy strictly)
			else
				RANGE(ICL,IGP,CURRENT,ILDV) = TANKSIZE(ICL,IGP,CURRENT,1) * &
                                           FE(ICL,IGP,CURRENT,GAS) *  &
                                          (1+AFVADJRN(ILDV,ATVYEAR)) * &
										  0.7 	!accounting for on-road degradation and perceived range(greater than that for energy strictly)
			endif
			
!..range limiting function. Limiting projection range to 10% above historical maximum		
			if(yrs.eq.byr) MAXHISTRNG(ICL,IGP,ILDV)=0.0			!initialize max historical range
			if (yrs.le.XYR) then
				MAXHISTRNG(ICL,IGP,ILDV) = max(MAXHISTRNG(ICL,IGP,ILDV),RANGE(ICL,IGP,CURRENT,ILDV))
				
!...limit range growth due to increasing fuel economy. For alt vehicles, only use range limiting function
!...if the alt vehicle was sold by that group and in that class in history
			elseif(ILDV.eq.1) then
				RANGE(ICL,IGP,CURRENT,ILDV) = min(RANGE(ICL,IGP,CURRENT,ILDV),1.1*MAXHISTRNG(ICL,IGP,ILDV))
			elseif((IGP.le.5.and.CARFLG(ILDV-1,ICL).le.XYR).or.(IGP.ge.6.and.TRKFLG(ILDV-1,ICL).le.XYR)) then
				RANGE(ICL,IGP,CURRENT,ILDV) = min(RANGE(ICL,IGP,CURRENT,ILDV),1.1*MAXHISTRNG(ICL,IGP,ILDV))
			endif

! ... Electrics have a range based on battery capacity calculated in EVCALC subroutine
            if(ILDV.eq.4) RANGE(ICL,IGP,current,ILDV) = EV_range(ICL,IGP,ILDV)
            if(ILDV.eq.7) RANGE(ICL,IGP,current,ILDV) = EV_range(ICL,IGP,ILDV)
            if(ILDV.eq.15) RANGE(ICL,IGP,current,ILDV) = EV_range(ICL,IGP,ILDV)	
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

      INTEGER     ZYR, IYEAR

      IYEAR = ZYR

	  IF (YRS.GE.XYR) CALL LIONCOSTCALC      ! calculate lithium ion battery cost ($/kwh)

      DO ICL=1,MAXCLASS
        DO IGP=1,MAXGROUP
          DO IATV=2,MAXLDV

! ... Set vehicle type index for correct reference to arrays that are type, rather
! ... than group specific.
            IVTYP=GrpMap(IGP)

! ... Calculate BASE values for vehicle introduction year
            IF (ZYR.GT.XYR .AND. ((IGP.LE.cargrp.AND. CARFLG(IATV-1,ICL).EQ.ZYR .AND. CLASSFLAG(ICL,IGP,GAS)) & ! calc BASE 
            .OR. (IGP.GE.ltkgrp.AND. TRKFLG(IATV-1,ICL).EQ.ZYR .AND. CLASSFLAG(ICL,IGP,GAS))) ) THEN    ! for intro year  

              WEIGHT(ICL,IGP,BASE,IATV)  = WEIGHT(ICL,IGP,CURRENT,GAS) * (1+AFVADJWT(IATV,XYR))
              FE(ICL,IGP,BASE,IATV)      = FE(ICL,IGP,CURRENT,GAS)     * (1+AFVADJFE(IATV,XYR))
              HP(ICL,IGP,BASE,IATV)       = HP(ICL,IGP,CURRENT,GAS)    * (1+AFVADJHP(IATV,XYR))
              VOLUME(ICL,IGP,BASE,IATV)   = VOLUME(ICL,IGP,CURRENT,GAS)
              TANKSIZE(ICL,IGP,BASE,IATV) = TANKSIZE(ICL,IGP,CURRENT,GAS)
              PRICE(ICL,IGP,BASE,IATV)   = PRICE(ICL,IGP,CURRENT,GAS)  
              PRICEHI(ICL,IGP,BASE,IATV) = PRICEHI(ICL,IGP,CURRENT,GAS) 
              DO ITECH=1,NUMTECH
                MKT_PEN(ICL,IGP,ITECH,BASE,IATV) = MKT_PEN(ICL,IGP,ITECH,BASE,GAS)
              ENDDO
            ENDIF

! ... Calculate CURRENT values for either historical year or vehicle introduction year
            IF((ZYR.LE.XYR .AND. CLASSFLAG(ICL,IGP,IATV)) .OR. (ZYR.GT.XYR .AND.  &       ! Historical year or
              ((IGP.LE.cargrp.AND. CARFLG(IATV-1,ICL).EQ.ZYR .AND. CLASSFLAG(ICL,IGP,GAS)) .OR. & ! Year car or 
              (IGP.ge.ltkgrp.AND. TRKFLG(IATV-1,ICL).EQ.ZYR .AND. CLASSFLAG(ICL,IGP,GAS))))) THEN    ! trk introduced 

              IF (ZYR.GT.XYR) IYEAR = XYR  ! If foreccast year (zyr) > 2015, set zyr=2015 to use year = 2015 adjustment 

! ... Estimate AFV prices first
              PRICE(ICL,IGP,CURRENT,IATV)   = PRICE(ICL,IGP,CURRENT,GAS)   + AFVADJPR(IATV,IVTYP,IYEAR)
              PRICEHI(ICL,IGP,CURRENT,IATV) = PRICEHI(ICL,IGP,CURRENT,GAS) + AFVADJPRH(IATV,IVTYP,IYEAR)

   
! ... Estimate AFV fuel economy, weight, horsepower, interior volume, and tank size
              FE(ICL,IGP,CURRENT,IATV)       = FE(ICL,IGP,CURRENT,GAS)     * (1+AFVADJFE(IATV,IYEAR))
              WEIGHT(ICL,IGP,CURRENT,IATV)   = WEIGHT(ICL,IGP,CURRENT,GAS) * (1+AFVADJWT(IATV,IYEAR))
              HP(ICL,IGP,CURRENT,IATV)       = HP(ICL,IGP,CURRENT,GAS)    * (1+AFVADJHP(IATV,IYEAR))
              VOLUME(ICL,IGP,CURRENT,IATV)   = VOLUME(ICL,IGP,CURRENT,GAS)
              TANKSIZE(ICL,IGP,CURRENT,IATV) = TANKSIZE(ICL,IGP,CURRENT,GAS)

! ... All electric drive vehicles have an additional battery and fuel cell price, horsepower, and weight adjustments
              if(IATV.ge.4.and.IATV.le.8.or.IATV.ge.13) then
                ILDV=IATV
                if(ILDV.eq.4.or.ILDV.eq.7.or.ILDV.eq.15) call EVCALC (zyr)
				if(ILDV.eq.5.or.ILDV.eq.6) call PHEVCALC (zyr)
                if(ILDV.eq.8.or.ILDV.eq.16) call HEVCALC (zyr)
                if(ILDV.ge.13.and.ILDV.le.14) call FCCALC (zyr)
              endif
! ... Assume base and historic technology penetrations for AFVs are the same as gasoline
              DO ITECH=1,NUMTECH
                MKT_PEN(ICL,IGP,ITECH,CURRENT,IATV) = MKT_PEN(ICL,IGP,ITECH,CURRENT,GAS)
                MKT_MAX(ICL,IGP,ITECH,IATV)         = MKT_MAX(ICL,IGP,ITECH,GAS)
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDDO
    RETURN
    END SUBROUTINE AFVADJ

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
    lii_kWhr(ICL,IGP,ILDV) = weight(ICL,IGP,current,gas) * LIONkWh_perLb(ILDV)
    nmh_kWhr(ICL,IGP,ILDV) = weight(ICL,IGP,current,gas) * NiMHkWh_per_pound

    IF(lii_kWhr(ICL,IGP,ILDV)*Li_ion_Cost(ILDV,dsyrs).lt.nmh_kWhr(ICL,IGP,ILDV)*nimh_cost(dsyrs)) THEN
	  ElecSysIncCost(ICL,IGP,current,ILDV) = lii_kWhr(ICL,IGP,ILDV)*Li_ion_Cost(ILDV,dsyrs) + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)
	  BatPackWgt(dsyrs,ICL,IGP,ILDV) = LION_LB_perkWh(ILDV) * lii_kWhr(ICL,IGP,ILDV)
	ELSE
	  ElecSysIncCost(ICL,IGP,current,ILDV) = nmh_kWhr(ICL,IGP,ILDV)*nimh_cost(dsyrs) + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)
	  BatPackWgt(dsyrs,ICL,IGP,ILDV) = NiMHweight_per_kWh * nmh_kWhr(ICL,IGP,ILDV) 
	ENDIF
	
!...Calculate total battery electric vehicle price
    PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)
    PRICEHI(ICL,IGP,CURRENT,ILDV) = PRICEHI(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)

!...Calculate battery electric vehicle weight
    WEIGHT(ICL,IGP,current,ILDV) = weight(ICL,IGP,current,gas)+BatPackWgt(dsyrs,ICL,IGP,ILDV)

!...Assume HEVs have equivalent performance to conventional gasoline vehicle
    if(weight(ICL,IGP,CURRENT,GAS).ne.0.0) HP(ICL,IGP,current,ILDV) = HP(ICL,IGP,CURRENT,GAS) * (weight(ICL,IGP,CURRENT,ILDV)/weight(ICL,IGP,CURRENT,GAS))

  RETURN
  END SUBROUTINE HEVCALC

! ==========================================================================================================
!...Subroutine LIONCOSTCALC
!   Description:
!       Calculates lithium-ion battery cost ($/kWh) for battery electric vehicles
! ==========================================================================================================
  SUBROUTINE LIONCOSTCALC
  USE T_
  IMPLICIT NONE

!...Local variable dictionary
    INTEGER :: ic                                        !...temporary looping counter
    INTEGER :: first_bat_yr                              !...first year in calculator (tied to first FE projection year)
	REAL :: annual_gwh(mnumyr)                           !...annual EV and PHEV li-ion battery production
                                                         !...cumulative_gwh(mnumyr) = cumulative EV and PHEV li-ion battery production (defined in tranmain)
	first_bat_yr = (XYR+1) - 1989

!...Calculate cumulative production (GWh) of li-ion batteries in EVs and PHEVs (excluding HEVs)
!   The first year LIONCOSTCALC is called, sum up historical GWh (1995 to XYR)
!   Average kwh capacity calculated for vehicle classes that have capacity and 
!   multiplied by annual sales in each fuel type and vehicle type (car/light truck)
	if(n.eq.first_bat_yr)then 
		annual_gwh(:) 	  = 0.0
		cumulative_gwh(:) = 0.0
		do ic = 6,first_bat_yr
			do ILDV=1,maxldv
				if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.eq.15)then
					do IVTYP=1,maxvtyp
						if(COUNT(avg_kwh(IVTYP,ILDV,:,ic+1989).gt.0.0).eq.0.0)then
							annual_gwh(ic) = 0.0
						elseif(IVTYP.eq.1)then		! Uses average kWh per vehicle, averaged over all size classes
							annual_gwh(ic) = annual_gwh(ic) + trldsalc(ILDV,11,ic)*SUM(avg_kwh(IVTYP,ILDV,:,ic+1989))/ &
											 COUNT(avg_kwh(IVTYP,ILDV,:,ic+1989).gt.0.0)
						else
							annual_gwh(ic) = annual_gwh(ic) + trldsalt(ILDV,11,ic)*SUM(avg_kwh(IVTYP,ILDV,:,ic+1989))/ &
											 COUNT(avg_kwh(IVTYP,ILDV,:,ic+1989).gt.0.0)
						endif
					enddo ! maxvtyp
				endif ! end if EV or PHEV
			enddo ! maxldv
			cumulative_gwh(ic) = cumulative_gwh(ic-1) + annual_gwh(ic)
		enddo ! yr
!   The second and subsequent years LIONCOSTCALC is called, calculate current year GWh and add to cumulative total
!   Average KWh capacity for each fuel type, class, and vehicle type (Car/light truck) multiplied by sales
!   for each fuel type, class, and vehicle type.
	else
		annual_gwh(n) = 0.0
		do ILDV=1,maxldv
			if((ILDV.ge.4.and.ILDV.le.7).or.ILDV.eq.15)then
				do ICL=1,MAXCLASS
					annual_gwh(n) = annual_gwh(n) + avg_kwh(1,ILDV,ICL,yrs)*NCSTECH(11,ICL,ILDV,n)
					annual_gwh(n) = annual_gwh(n) + avg_kwh(2,ILDV,ICL,yrs)*NLTECH(11,ICL,ILDV,n)
				enddo ! MAXCLASS
			endif ! end if ILDV is EV or PHEV
		enddo ! maxldv
		cumulative_gwh(n) = cumulative_gwh(n-1) + annual_gwh(n)
	endif ! end check for first bat year
	
!...Calculation of cumulative production-based lithium-ion battery cost ($/kWh) by vehicle type
!	do ILDV=1,maxldv
!		if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) then
!			li_ion_cost(ILDV,yrs) = pack_a(ILDV)*cumulative_gwh(n-1)**(-pack_b(ILDV)) + mat_a(ILDV)*mat_markup(n)*cumulative_gwh(n-1)**(-mat_b(ILDV))
!		endif
!	enddo

!	In the first projection year, the default learning rate (16.5%) is assumed. This prevents the need for re-calibration to the "pseuo-historical" year
!	(usually the first projection year sales are calibrated based on whatever we have YTD from Ward's).  After the first projection year, the model re-estimates 
!	the base price coefficient (pack_a) for the curve, using the learning rate provided in trnldvx.xlsx.
	do ILDV=1,maxldv
	  if(ILDV.ge.4.and.ILDV.le.8.or.ILDV.ge.13) then
!		Align curve with historical data
	  	if(n.eq.first_bat_yr) pack_a(ILDV) = (li_ion_cost(ILDV,yrs-1)-mat_a(ILDV)*mat_markup(n)*cumulative_gwh(n-2)**(-mat_b(ILDV)))/(cumulative_gwh(n-2)**(-(-LOG(1.0-0.165)/LOG(2.0))))
		if(n.le.first_bat_yr+1) then		! Assume standard learning rate (16.5%) through first projection year (year that we calibrate sales to Ward's based on YTD values)
		  li_ion_cost(ILDV,yrs) = pack_a(ILDV)*cumulative_gwh(n-1)**(-(-LOG(1.0-0.165)/LOG(2.0))) + mat_a(ILDV)*mat_markup(n)*cumulative_gwh(n-1)**(-mat_b(ILDV))
	    else								! If past the first projection year, use the user-defined learning rate (pack_b)
	  	  if(n.eq.first_bat_yr+2) then		! Align curve coefficient -- pack_a -- so that the user-defined pack_b produces the same cost in first_bat_yr+2
	  	    pack_a(ILDV) = (li_ion_cost(ILDV,yrs-1)-mat_a(ILDV)*mat_markup(n)*cumulative_gwh(n-2)**(-mat_b(ILDV)))/(cumulative_gwh(n-2)**(-pack_b(ILDV)))
	  	  endif
	  	  li_ion_cost(ILDV,yrs) = pack_a(ILDV)*cumulative_gwh(n-1)**(-pack_b(ILDV)) + mat_a(ILDV)*mat_markup(n)*cumulative_gwh(n-1)**(-mat_b(ILDV))
	  	endif
	  endif
	enddo

	if(n.eq.MNUMYR.and.FCRL.eq.1)then
		write(21,*) "LIONCOSTCALC_debug_2020USD"
		write(21,*) "ic annual_gwh cumulative_gwh li_ion_cost (2020USD ILDV=4:7 and 15)"
		do ic=6,n
			write(21, '(I5,", ", 7(F12.3,", "))') ic+1989, annual_gwh(ic), cumulative_gwh(ic), li_ion_cost(4:7,ic+1989)/ MC_JPGDP(1) * MC_JPGDP(31), &
												  li_ion_cost(15,ic+1989)/ MC_JPGDP(1) * MC_JPGDP(31)
		enddo
	endif

  RETURN
  END SUBROUTINE LIONCOSTCALC

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
	REAL :: PHEV_range								

!...Calculate required battery size based on vehicle weight and depth of discharge improvement
	BatPackSize(dsyrs,ICL,IGP,ILDV) = 0.0
	if(weight(ICL,IGP,current,gas).gt.0.0) BatPackSize(dsyrs,ICL,IGP,ILDV) = weight(ICL,IGP,current,gas) * EV_batt_size_m(ILDV) + EV_batt_size_b(ILDV)

!...Calculate total battery electric vehicle incremental cost
	ElecSysIncCost(ICL,IGP,current,ILDV) = (BatPackSize(dsyrs,ICL,IGP,ILDV)*Li_ion_Cost(ILDV,dsyrs)) + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)

!...Calculate battery electric vehicle price
    PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)
    PRICEHI(ICL,IGP,CURRENT,ILDV) = PRICEHI(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)

!...Calculate battery electric vehicle weight
    WEIGHT(ICL,IGP,current,ILDV) = weight(ICL,IGP,current,gas)+(BatPackSize(dsyrs,ICL,IGP,ILDV)*LION_LB_perkWh(ILDV))   

!...Assume PHEVs have equivalent performance (HP/weight ratio) to conventional gasoline vehicle
    if(weight(ICL,IGP,CURRENT,GAS).ne.0.0) HP(ICL,IGP,current,ILDV) = HP(ICL,IGP,CURRENT,GAS) * (weight(ICL,IGP,CURRENT,ILDV)/weight(ICL,IGP,CURRENT,GAS))

!...Estimate Electric Range
    if(BatPackSize(dsyrs,ICL,IGP,ILDV).gt.0.0)then
		PHEV_range = EV_range_m(ILDV) * (BatPackSize(dsyrs,ICL,IGP,ILDV) * PHEV_DOD(dsyrs)) + EV_range_b(ILDV)
	else
		PHEV_range = 0.0
	endif

!...Calculate PHEV fuel economy equivalency based on range and electric share of VMT
	IF(ILDV.eq.5) mfg_eg_mpg20(ICL,IGP,n) = 0.0
	IF(ILDV.eq.6) mfg_eg_mpg50(ICL,IGP,n) = 0.0

    if(BatPackSize(dsyrs,ICL,IGP,ILDV).gt.0.0) then
	  FE(ICL,IGP,current,ILDV) = PHEV_range / (BatPackSize(dsyrs,ICL,IGP,ILDV)*PHEV_DOD(dsyrs)) * MG_HHV * 1000.0 * (1.0/3412.0)

	  IF(ILDV.eq.5) THEN		! PHEV20
		mfg_eg_mpg20(ICL,IGP,n) = FE(ICL,IGP,current,ILDV)/FE(ICL,IGP,current,gas)
        if(FE(ICL,IGP,current,ILDV).ne.0.0) FE(ICL,IGP,current,ILDV) = 1/((PHEV20_evmt/FE(ICL,IGP,current,ILDV) + (1.0-PHEV20_evmt)/FE(ICL,IGP,current,gas)))
	  ELSEIF(ILDV.eq.6) THEN	! PHEV50
	    mfg_eg_mpg50(ICL,IGP,n) = FE(ICL,IGP,current,ILDV)/FE(ICL,IGP,current,gas)
        if(FE(ICL,IGP,current,ILDV).ne.0.0) FE(ICL,IGP,current,ILDV) = 1/((PHEV50_evmt/FE(ICL,IGP,current,ILDV) + (1.0-PHEV50_evmt)/FE(ICL,IGP,current,gas)))
	  ENDIF
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

!...Calculate gross kWh needed based on vehicle weight
	if(weight(ICL,IGP,current,gas).gt.0.0) then
		BatPackSize(dsyrs,ICL,IGP,ILDV) = weight(ICL,IGP,current,gas) * EV_batt_size_m(ILDV) + EV_batt_size_b(ILDV)
	else
		BatPackSize(dsyrs,ICL,IGP,ILDV) = 0.0
	endif

!...Calculate total battery electric vehicle incremental cost
	ElecSysIncCost(ICL,IGP,current,ILDV) = BatPackSize(dsyrs,ICL,IGP,ILDV) * Li_ion_Cost(ILDV,dsyrs) + &
										   + ElecNonBattCst(icl,dsyrs,ivtyp,ildv)

!...Calculate battery electric vehicle price
    PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)
    PRICEHI(ICL,IGP,CURRENT,ILDV) = PRICEHI(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)

!...Calculate average battery weight (lbs) per kWh
	BatPackWgt(dsyrs,ICL,IGP,ILDV) = BatPackSize(dsyrs,ICL,IGP,ILDV) * LION_LB_perkWh(ILDV)

!...Calculate vehicle weight based on battery size
    WEIGHT(ICL,IGP,CURRENT,ILDV) = WEIGHT(ICL,IGP,CURRENT,GAS)-500.0 + BatPackWgt(dsyrs,ICL,IGP,ILDV)

!...Calculate vehicle range based on usable battery size
	if(BatPackSize(dsyrs,ICL,IGP,ILDV).gt.0.0) then
		EV_range(ICL,IGP,ILDV) = MIN(BatPackSize(dsyrs,ICL,IGP,ILDV) * EV_DOD(dsyrs) * ev_range_m(ILDV) + ev_range_b(ILDV), EV_range_max(ILDV))
	else
		EV_range(ICL,IGP,ILDV) = 0.0
	endif

!...Calculate EV fuel economy equivalency based on range and battery size.
!	Fuel economies are adjusted (0.82 and 0.75 below) to align more closely with EPA (fueleconomy.gov)
    if(BatPackSize(dsyrs,ICL,IGP,ILDV).gt.0.0) then
        if(IGP.le.5) then
		  FE(ICL,IGP,current,ILDV) = EV_range(ICL,IGP,ILDV) / (BatPackSize(dsyrs,ICL,IGP,ILDV) ) * MG_HHV * 1000.0 * (1.0/3412.0) * 0.82
		else
		  FE(ICL,IGP,current,ILDV) = EV_range(ICL,IGP,ILDV) / (BatPackSize(dsyrs,ICL,IGP,ILDV) ) * MG_HHV * 1000.0 * (1.0/3412.0) * 0.75
		endif
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
    REAL          TANKCOST(13:15)  /     0.0,  3500.0,     0.0 /
    REAL          GALPERMILE(13:15)/ 0.00625, 0.00570, 0.00667 /
	integer       dsyrs
	
!...Calculate base fuel cell cost based on a need of 0.028 kW per vehicle pound and input
!...fuel cell costs in $/kW.
    FUELCELL(ICL,IGP,CURRENT,ILDV) = WEIGHT(ICL,IGP,CURRENT,GAS) * 0.028 * FuelCell$kW(dsYRS,ILDV)

!...Calculate battery requirements for FCV 
	BatPackSize(dsyrs,ICL,IGP,ILDV) = weight(ICL,IGP,current,gas) * 0.0005

!...Calculate the total fuel cell, battery and hydrogen storage cost
    ElecSysIncCost(ICL,IGP,CURRENT,ILDV) = (FUELCELL(ICL,IGP,CURRENT,ILDV) + &
										BatPackSize(dsyrs,ICL,IGP,ILDV) * Li_ion_Cost(ILDV,dsyrs)  + &
                                        TANKCOST(ILDV))

!...Adjust vehicle price to include price of the fuel cell and battery.
    PRICE(ICL,IGP,CURRENT,ILDV) = PRICE(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)
    PRICEHI(ICL,IGP,CURRENT,ILDV) = PRICEHI(ICL,IGP,CURRENT,ILDV) + ElecSysIncCost(ICL,IGP,CURRENT,ILDV)
		
	
!...Estimate fuel cell vehicle fuel economy using estimates of gallons per mile per
!...1000 pounds of vehicle weight.
    FE(ICL,IGP,CURRENT,ILDV) = 1 / (GALPERMILE(ILDV) * (WEIGHT(ICL,IGP,CURRENT,GAS)/1000.0))

  RETURN
  END SUBROUTINE FCCALC

! ==========================================================================================================
! ... Subroutine READNHTSA reads the NHTSA historic calibration data file. 
! ==========================================================================================================
    SUBROUTINE READNHTSA
    USE T_
    IMPLICIT NONE

      LOGICAL*1     NEW/.FALSE./
      CHARACTER*18  INAME
      INTEGER       WKUNIT, JYR

! ... Read historical NHTSA data on sales, hp, fuel economy, and weight
      INAME = 'TRNNHTSAX'                 ! trnnhtsaX.xlsx
      WKUNIT = FILE_MGR('O',INAME,NEW)    !open trnnhtsaX.xlsx input file
      CALL ReadRngXLSX(WKUNIT,'trnnhtsa') !read range names & corresponding data from worksheet "trnnhtsa"
      WKUNIT = FILE_MGR('C',INAME,NEW)    !close xlsx input file
	
!	  MDR -- NHTSALYR was previously set using a "-9.9" in the first position of the DCSALES array that was no longer
!			 historical data (zeros). It is now set equal to XYR (last year of FEM historical data), since that is usually
!			 the case.
	  NHTSALYR = XYR
	  
	  CALL GETRNGR('DCSALES         ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,1 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('ACSALES         ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,2 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('ECSALES         ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,3 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('SCSALES         ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,4 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('CCAVSALES       ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,5 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T1SALES         ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,6 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T2SALES         ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,7 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T3SALES         ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,8 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T4SALES         ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,9 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T5SALES         ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,10) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('TCAVSALES       ',NHTSASAL (BYR:NHTSALYR,1:MAXCLASS,11) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('DCHP            ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,1 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('ACHP            ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,2 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('ECHP            ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,3 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('SCHP            ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,4 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('CCAVHP          ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,5 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T1HP            ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,6 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T2HP            ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,7 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T3HP            ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,8 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T4HP            ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,9 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T5HP            ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,10) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('TCAVHP          ',NHTSAHP  (BYR:NHTSALYR,1:MAXCLASS,11) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('DCFE            ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,1 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('ACFE            ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,2 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('ECFE            ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,3 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('SCFE            ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,4 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('CCAVFE          ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,5 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T1FE            ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,6 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T2FE            ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,7 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T3FE            ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,8 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T4FE            ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,9 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T5FE            ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,10) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('TCAVFE          ',NHTSAFE  (BYR:NHTSALYR,1:MAXCLASS,11) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('DCWGT           ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,1 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('ACWGT           ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,2 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('ECWGT           ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,3 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('SCWGT           ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,4 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('CCAVWGT         ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,5 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T1WGT           ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,6 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T2WGT           ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,7 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T3WGT           ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,8 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T4WGT           ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,9 ) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('T5WGT           ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,10) ,NHTSALYR-BYR+1,MAXCLASS,1)
	  CALL GETRNGR('TCAVWGT         ',NHTSAWGT (BYR:NHTSALYR,1:MAXCLASS,11) ,NHTSALYR-BYR+1,MAXCLASS,1)

    RETURN
    END SUBROUTINE READNHTSA

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
      INTEGER       INCOL,ICYR
      INTEGER 		it,iyx
	  REAL			WGT_FEM(MAXCLASS,XYR-1989,MAXGROUP)
	  REAL			FE_FEM(MAXCLASS,XYR-1989,MAXGROUP)
	  REAL			PRICE_FEM(MAXCLASS,XYR-1989,MAXGROUP)
	  REAL			HP_FEM(MAXCLASS,XYR-1989,MAXGROUP)
	  REAL			VOL_FEM(MAXCLASS,XYR-1989,MAXGROUP)
	  REAL			TANK_FEM(MAXCLASS,XYR-1989,MAXGROUP)
	  REAL          WGT_ATV(MAXATV,XYR-1989)
	  REAL          HP_ATV(MAXATV,XYR-1989)
	  REAL          PRICELT_ATV(MAXATV,XYR-1989)
	  REAL          PRICECAR_ATV(MAXATV,XYR-1989)
	  REAL          RANGE_ATV(MAXATV,XYR-1989)
	  REAL          FE_ATV(MAXATV,XYR-1989)

! ... Read historic FEM data
      INAME = 'TRNFEMX'
      WKUNIT = FILE_MGR('O',INAME,NEW)   ! open trnfemx.xlsx input file
      CALL ReadRngXLSX(WKUNIT,'trnfem')   ! read range names & corresponding data from worksheet "trnfem"
      WKUNIT = FILE_MGR('C',INAME,NEW)   ! close xlsx input file
	  
	  CALL GETRNGR('WGT_FEM          ',WGT_FEM, MAXCLASS,XYR-1989,MAXGROUP)
	  CALL GETRNGR('FE_FEM           ',FE_FEM, MAXCLASS,XYR-1989,MAXGROUP)
      CALL GETRNGR('PRICE_FEM        ',PRICE_FEM, MAXCLASS,XYR-1989,MAXGROUP)
      CALL GETRNGR('HP_FEM           ',HP_FEM, MAXCLASS,XYR-1989,MAXGROUP)
      CALL GETRNGR('VOL_FEM          ',VOL_FEM, MAXCLASS,XYR-1989,MAXGROUP)
      CALL GETRNGR('TANK_FEM         ',TANK_FEM, MAXCLASS,XYR-1989,MAXGROUP)
	  CALL GETRNGR('WGT_ATV          ',WGT_ATV, MAXATV,XYR-1989,1)
	  CALL GETRNGR('HP_ATV           ',HP_ATV, MAXATV,XYR-1989,1)
	  CALL GETRNGR('PRICELT_ATV      ',PRICELT_ATV, MAXATV,XYR-1989,1)
	  CALL GETRNGR('PRICECAR_ATV     ',PRICECAR_ATV, MAXATV,XYR-1989,1)
	  CALL GETRNGR('RANGE_ATV        ',RANGE_ATV, MAXATV,XYR-1989,1)
	  CALL GETRNGR('FE_ATV           ',FE_ATV, MAXATV,XYR-1989,1)

! ... Assign historic attribute data to report writer variables
      DO ICL=1,MAXCLASS
        DO IGP=1,MAXGROUP
		  DO ICYR=1990,XYR
		    FEMMPG(IGP,ICL,ICYR,GAS) = FE_FEM(ICL,ICYR-1989,IGP)
			FEMWGT(IGP,ICL,ICYR,GAS) = WGT_FEM(ICL,ICYR-1989,IGP)
			FEMPRI(IGP,ICL,ICYR,GAS) = PRICE_FEM(ICL,ICYR-1989,IGP) * MC_JPGDP(1)/MC_JPGDP(ICYR-1989)	! All in 1990USD
			FEMPRIH(IGP,ICL,ICYR,GAS)= FEMPRI(IGP,ICL,ICYR,GAS)
			FEMHP(IGP,ICL,ICYR,GAS)  = HP_FEM(ICL,ICYR-1989,IGP)
			FEMVOL(IGP,ICL,ICYR,GAS) = VOL_FEM(ICL,ICYR-1989,IGP)
			FEMTSZ(IGP,ICL,ICYR,GAS) = TANK_FEM(ICL,ICYR-1989,IGP)
			FEMRNG(IGP,ICL,ICYR,GAS) = FEMTSZ(IGP,ICL,ICYR,GAS) * FEMMPG(IGP,ICL,ICYR,GAS)
		  ENDDO
		ENDDO
	  ENDDO
	  
	  DO IATV=1,MAXATV
	    DO ICYR=1990,XYR
          ILDV = IATV + 1
		  AFVADJHP(ILDV,ICYR)      = HP_ATV(IATV,ICYR-1989)
          AFVADJRN(ILDV,ICYR)      = RANGE_ATV(IATV,ICYR-1989)
          AFVADJFE(ILDV,ICYR)      = FE_ATV(IATV,ICYR-1989)
          AFVADJWT(ILDV,ICYR)      = WGT_ATV(IATV,ICYR-1989)
          AFVADJPR(ILDV,1,ICYR)    = PRICECAR_ATV(IATV,ICYR-1989)
          AFVADJPR(ILDV,2,ICYR)    = PRICELT_ATV(IATV,ICYR-1989)
          AFVADJPRH(ILDV,1,ICYR)   = AFVADJPR(ILDV,1,ICYR)
          AFVADJPRH(ILDV,2,ICYR)   = AFVADJPR(ILDV,2,ICYR)
		ENDDO
	  ENDDO

! ... Subroutine AFVADJ is used to estimate ATV attributes from given gasoline data
! ... for the same year.  This routine is run for both the historic year data and
! ... the base year data to set the initial ATV attributes for FEM.  Therefore, it
! ... is important that the base year data be read (via subroutine READLDV) prior to
! ... reading the historic year data (via this subroutine).  Since subroutine AFVADJ
! ... uses the "current year" storage parameters for processing, the data must first
! ... be stored in the proper arrays.

      DO IYR=BYR,XYR                      ! store gasoline attributes for historic and base year in "current year" parameters
        DO ICL=1,MAXCLASS
          DO IGP=1,MAXGROUP
            IF (IYR .NE. XYR) THEN        ! load historic gasoline data if not the FEM base year
              FE(ICL,IGP,CURRENT,GAS)       = FEMMPG(IGP,ICL,IYR,GAS)
              WEIGHT(ICL,IGP,CURRENT,GAS)   = FEMWGT(IGP,ICL,IYR,GAS)
              PRICE(ICL,IGP,CURRENT,GAS)    = FEMPRI(IGP,ICL,IYR,GAS)
              PRICEHI(ICL,IGP,CURRENT,GAS)  = FEMPRIH(IGP,ICL,IYR,GAS)
              HP(ICL,IGP,CURRENT,GAS)       = FEMHP(IGP,ICL,IYR,GAS)
              VOLUME(ICL,IGP,CURRENT,GAS)   = FEMVOL(IGP,ICL,IYR,GAS)
              TANKSIZE(ICL,IGP,CURRENT,GAS) = FEMTSZ(IGP,ICL,IYR,GAS)
              DO ITECH=1,NUMTECH
                MKT_PEN(ICL,IGP,ITECH,CURRENT,GAS) = MKT_PEN(ICL,IGP,ITECH,BASE,GAS)
              ENDDO
            ELSE                          ! load base year gasoline data if the FEM base year
              FE(ICL,IGP,CURRENT,GAS)       = FE(ICL,IGP,BASE,GAS)
              WEIGHT(ICL,IGP,CURRENT,GAS)   = WEIGHT(ICL,IGP,BASE,GAS)
              PRICE(ICL,IGP,CURRENT,GAS)    = PRICE(ICL,IGP,BASE,GAS)
              PRICEHI(ICL,IGP,CURRENT,GAS)  = PRICEHI(ICL,IGP,BASE,GAS)
              HP(ICL,IGP,CURRENT,GAS)       = HP(ICL,IGP,BASE,GAS)
              VOLUME(ICL,IGP,CURRENT,GAS)   = VOLUME(ICL,IGP,BASE,GAS)
              TANKSIZE(ICL,IGP,CURRENT,GAS) = TANKSIZE(ICL,IGP,BASE,GAS)
              DO ITECH=1,NUMTECH
                MKT_PEN(ICL,IGP,ITECH,CURRENT,GAS) = MKT_PEN(ICL,IGP,ITECH,BASE,GAS)
              ENDDO
            ENDIF
          ENDDO
        ENDDO

        do ILDV=2,MAXLDV
          do ICL=1,MAXCLASS
            do IGP=1,MAXGROUP
              it=GrpMap(IGP)
              CLASSFLAG(ICL,IGP,ILDV)= .false.
              if(it.eq.1.and.CARFLG(ILDV-1,ICL).le.iyr) then
                if(CLASSFLAG(ICL,IGP,gas)) CLASSFLAG(ICL,IGP,ILDV)= .true.
              endif
              if(it.eq.2.and.TRKFLG(ILDV-1,ICL).le.iyr) then
                if(CLASSFLAG(ICL,IGP,gas)) CLASSFLAG(ICL,IGP,ILDV)= .true.
              endif
            end do
          end do
        end do

        CALL AFVADJ (IYR)                 ! calculate corresponding ATV attributes using "current year" gasoline data

        DO ICL=1,MAXCLASS                 ! save calculated ATV attributes in report writer variables
          DO IGP=1,MAXGROUP
            DO IATV=2,MAXLDV
              FEMMPG(IGP,ICL,IYR,IATV)  = FE(ICL,IGP,CURRENT,IATV)
              FEMWGT(IGP,ICL,IYR,IATV)  = WEIGHT(ICL,IGP,CURRENT,IATV)
              FEMPRI(IGP,ICL,IYR,IATV)  = PRICE(ICL,IGP,CURRENT,IATV)
              FEMPRIH(IGP,ICL,IYR,IATV) = PRICEHI(ICL,IGP,CURRENT,IATV)
              FEMHP(IGP,ICL,IYR,IATV)   = HP(ICL,IGP,CURRENT,IATV)
              FEMVOL(IGP,ICL,IYR,IATV)  = VOLUME(ICL,IGP,CURRENT,IATV)
              FEMTSZ(IGP,ICL,IYR,IATV)  = TANKSIZE(ICL,IGP,CURRENT,IATV)

! ... Be careful with ATV ranges as a "zero value range" is actually the parameter that
! ... currently allows subroutine TALT2X to properly handle non-existant vehicle classes.
! ... In other words, the range for non-existant classes should be zero.              

              IF (CLASSFLAG(ICL,IGP,IATV)) THEN                       ! class exists, set range as offset from gasoline

                FEMRNG(IGP,ICL,IYR,IATV) = TANKSIZE(ICL,IGP,CURRENT,GAS) * FE(ICL,IGP,CURRENT,GAS) * &
                                          (1+AFVADJRN(IATV,IYR))
                if(IATV.eq.4) FEMRNG(IGP,ICL,IYR,IATV) =  90.0 
                if(IATV.eq.7) FEMRNG(IGP,ICL,IYR,IATV) = 200.0 
				if(IATV.eq.15) FEMRNG(IGP,ICL,IYR,IATV) = 300.0 
              ENDIF

              DO ITECH=1,NUMTECH
                FEMPEN(IGP,ICL,ITECH,IYR,IATV) = MKT_PEN(ICL,IGP,ITECH,CURRENT,IATV)
              ENDDO
            ENDDO
          ENDDO
        ENDDO

        IF (IYR .EQ. XYR) THEN            ! set FEM base year data for ATVs (gasoline set via TRNLDV.XML)
          DO ICL=1,MAXCLASS
            DO IGP=1,MAXGROUP
              DO IATV=2,MAXLDV
                FE(ICL,IGP,BASE,IATV)       = FE(ICL,IGP,CURRENT,IATV)
                WEIGHT(ICL,IGP,BASE,IATV)   = WEIGHT(ICL,IGP,CURRENT,IATV)
                PRICE(ICL,IGP,BASE,IATV)    = PRICE(ICL,IGP,CURRENT,IATV)
                PRICEHI(ICL,IGP,BASE,IATV)  = PRICEHI(ICL,IGP,CURRENT,IATV)
                HP(ICL,IGP,BASE,IATV)       = HP(ICL,IGP,CURRENT,IATV)
                VOLUME(ICL,IGP,BASE,IATV)   = VOLUME(ICL,IGP,CURRENT,IATV)
                TANKSIZE(ICL,IGP,BASE,IATV) = TANKSIZE(ICL,IGP,CURRENT,IATV)
                DO ITECH=1,NUMTECH
                  MKT_PEN(ICL,IGP,ITECH,BASE,IATV) = MKT_PEN(ICL,IGP,ITECH,CURRENT,IATV)
                ENDDO

                if(IATV.ge.4.and.IATV.le.8.or.IATV.ge.13) &
                   ElecSysIncCost(ICL,IGP,BASE,IATV) = ElecSysIncCost(ICL,IGP,CURRENT,IATV)

              ENDDO
            ENDDO
          ENDDO
        ENDIF
	  ENDDO                               ! end year processing, all historic gasoline and ATV parms are now set

    RETURN
    END SUBROUTINE READHIST

! ==========================================================================================================
! ... Subroutine CALIBNHTSA calibrates FEM results to NHTSA values.  
! ==========================================================================================================
    SUBROUTINE CALIBNHTSA
    USE T_
    IMPLICIT NONE

      INTEGER       YRPTR,TLOOP,LONDX(2),HINDX(2),STEP(2),YRSET(4)
      REAL          TEMPFE,TEMPHP,TEMPWT,INTFAC

      REAL          CALRATIO_FE(MAXCLASS,MAXGROUP,MAXLDV)              ! Calibration factor for FE
      REAL          CALRATIO_HP(MAXCLASS,MAXGROUP,MAXLDV)              ! Calibration factor for HP
      REAL          CALRATIO_WGT(MAXCLASS,MAXGROUP,MAXLDV)             ! Calibration factor for WGHT

! ... Calibration factors will be based on historical NHTSA data through the
! ... last available data year.  Subsequent year calibration factors are set
! ... to unity since calibrated data for each year serve as input into FEM
! ... for the next year, in effect carrying the last set of calibration
! ... factors right on through the projection.  Right now, all ATV calibration
! ... factors are being set to equal to corresponding gasoline vehicle
! ... calibration factors.  This is necessary to preserve the differential
! ... relationships between gasoline vehicles and ATVs

! ... Also need to check for and work around any quirks in NHTSA sales data.
! ... Specifically, quirks in the form of years where sales drop to zero in a
! ... previously or future non-zero sales class create situations where the FE,
! ... WGT, and HP of vehicles in that class and year "go to zero" if a straight
! ... NHTSA calibration is performed.  This, in turn, results in zero FE, WGT,
! ... and HP for future year vehicles in that class since FEM begins each year
! ... using previous year FE, WGT, and HP values.  For example, NHTSA shows
! ... sales of import compact vans in each year between 1990 and 1999, except
! ... 1998.  To surrmount this problem, the following interpolation routine
! ... substitutes "expected" values of FE, WGT, and HP as necessary.  This
! ... does not affect NHTSA sales data, but does allow FEM to function
! ... continuously by producing "what if" attributes for zero sales years.
! ... If this calibration routine is ultimately expanded with other
! ... vehicle attributes (in addition to FE, HP, and WGT), this interpolation
! ... routine will need to be expanded with those attributes as well.

      DO ILDV = 1,MAXLDV
        DO IGP = 1,MAXGROUP
          DO ICL = 1,MAXCLASS
            IF (ILDV .EQ. GAS .AND. YRS .LE. NHTSALYR) THEN
              TEMPFE = NHTSAFE(YRS,ICL,IGP)
              TEMPHP = NHTSAHP(YRS,ICL,IGP)
              TEMPWT = NHTSAWGT(YRS,ICL,IGP)
              IF (NHTSASAL(YRS,ICL,IGP) .EQ. 0.0)  THEN     ! run interpolation/extrapolation routine for zero sales years
                YRSET = 0
                IF (YRS .EQ. BYR) THEN              ! set loop parms for forward from current year+1 to the last NHTSA year
                  TLOOP     = 1
                  LONDX(1) = YRS+1
                  HINDX(1) = NHTSALYR
                  STEP(1)  = 1
                ELSEIF (YRS .EQ. NHTSALYR) THEN     ! set loop parms for backward from current year-1 to the base year
                  TLOOP     = 1
                  LONDX(1) = YRS-1
                  HINDX(1) = BYR
                  STEP(1)  = -1
                ELSE                                ! set loop parms for forward from current year+1 to the last NHTSA year
                  TLOOP     = 2                      ! and backward from current year-1 to the base year
                  LONDX(1) = YRS+1
                  HINDX(1) = NHTSALYR
                  STEP(1)  = 1
                  LONDX(2) = YRS-1
                  HINDX(2) = BYR
                  STEP(2)  = -1
                ENDIF

                YRPTR = 1

                DO IYR = LONDX(1),HINDX(1),STEP(1)  ! run first loop for all cases
                  IF (YRPTR .LT. 3) THEN
                    IF (NHTSASAL(IYR,ICL,IGP) .NE. 0.0) THEN
                      YRSET(YRPTR) = IYR
                      YRPTR = YRPTR + 1
                    ENDIF
                  ENDIF
                ENDDO

                IF (TLOOP .EQ. 2) THEN               ! run second loop for forward and backward cases
                  YRPTR = 3
                  DO IYR = LONDX(2),HINDX(2),STEP(2)
                    IF (YRPTR .LT. 5) THEN
                      IF (NHTSASAL(IYR,ICL,IGP) .NE. 0.0) THEN
                        YRSET(YRPTR) = IYR
                        YRPTR = YRPTR + 1
                      ENDIF
                    ENDIF
                  ENDDO
                  IF (YRSET(1) .EQ. 0) THEN
                    YRSET(1) = YRSET(3)
                    YRSET(2) = YRSET(4)
                    YRPTR = YRPTR - 2
                  ELSEIF (YRSET(3) .NE. 0) THEN
                    YRSET(2) = YRSET(3)
                    YRPTR = 3
                  ELSEIF (YRSET(2) .EQ. 0) THEN
                    YRPTR = 2
                  ENDIF
                ENDIF

                YRPTR = YRPTR - 1

                IF (YRPTR .EQ. 2) THEN              ! interpolate/extrapolate

                  INTFAC = REAL(YRS-YRSET(1)) / &
                           REAL(YRSET(2)-YRSET(1))

                  TEMPFE = (1/NHTSAFE(YRSET(1),ICL,IGP)) + &
                         (((1/NHTSAFE(YRSET(2),ICL,IGP)) - &
                           (1/NHTSAFE(YRSET(1),ICL,IGP)))*INTFAC)

                  TEMPFE = 1/TEMPFE

                  TEMPHP = NHTSAHP(YRSET(1),ICL,IGP) + &
                         ((NHTSAHP(YRSET(2),ICL,IGP) - &
                           NHTSAHP(YRSET(1),ICL,IGP))*INTFAC)

                  TEMPWT = NHTSAWGT(YRSET(1),ICL,IGP) + &
                         ((NHTSAWGT(YRSET(2),ICL,IGP) - &
                           NHTSAWGT(YRSET(1),ICL,IGP))*INTFAC)

                ELSEIF (YRPTR .EQ. 1) THEN          ! assign closest non-zero year

                  TEMPFE = NHTSAFE(YRSET(1),ICL,IGP)
                  TEMPHP = NHTSAHP(YRSET(1),ICL,IGP)
                  TEMPWT = NHTSAWGT(YRSET(1),ICL,IGP)

                ENDIF

              ENDIF

              IF (TEMPFE .EQ. 0.0 .AND. FEMMPG(IGP,ICL,YRS,ILDV) .NE. 0.0) then
   
                WRITE(6,*) '   YRS   = ',YRS
                WRITE(6,*) '   ICL   = ',ICL
                WRITE(6,*) '   IGP   = ',IGP
                WRITE(6,*) '   ILDV = ',ILDV
                WRITE(6,*) '   FEMMPG(ICL,IGP,BASE,ILDV) = ',FEMMPG(IGP,ICL,YRS,ILDV)
                STOP 333
              ENDIF
              IF (TEMPHP .EQ. 0.0 .AND. FEMHP (IGP,ICL,YRS,ILDV) .NE. 0.0) STOP 334
              IF (TEMPWT .EQ. 0.0 .AND. FEMWGT(IGP,ICL,YRS,ILDV) .NE. 0.0) STOP 335

              IF (FEMMPG(IGP,ICL,YRS,ILDV) .GT. 0.0) THEN
                CALRATIO_FE(ICL,IGP,ILDV) = TEMPFE / FEMMPG(IGP,ICL,YRS,ILDV)
              ELSE
                CALRATIO_FE(ICL,IGP,ILDV) = 1.0
              ENDIF

              IF (FEMHP(IGP,ICL,YRS,ILDV) .GT. 0.0) THEN
                CALRATIO_HP(ICL,IGP,ILDV) = TEMPHP / FEMHP(IGP,ICL,YRS,ILDV)
              ELSE
                CALRATIO_HP(ICL,IGP,ILDV) = 1.0
              ENDIF

              IF (FEMWGT(IGP,ICL,YRS,ILDV) .GT. 0.0) THEN
                CALRATIO_WGT(ICL,IGP,ILDV) = TEMPWT / FEMWGT(IGP,ICL,YRS,ILDV)
              ELSE
                CALRATIO_WGT(ICL,IGP,ILDV) = 1.0
              ENDIF

            ELSEIF (ILDV .EQ. GAS) THEN

              CALRATIO_FE(ICL,IGP,ILDV)  = 1.0
              CALRATIO_HP(ICL,IGP,ILDV)  = 1.0
              CALRATIO_WGT(ICL,IGP,ILDV) = 1.0

            ELSE

              CALRATIO_FE(ICL,IGP,ILDV)  = CALRATIO_FE(ICL,IGP,GAS)
              CALRATIO_HP(ICL,IGP,ILDV)  = CALRATIO_HP(ICL,IGP,GAS)
              CALRATIO_WGT(ICL,IGP,ILDV) = CALRATIO_WGT(ICL,IGP,GAS)

            ENDIF

            FEMMPG(IGP,ICL,YRS,ILDV) = FEMMPG(IGP,ICL,YRS,ILDV) * CALRATIO_FE(ICL,IGP,ILDV)
            FEMHP(IGP,ICL,YRS,ILDV)  = FEMHP(IGP,ICL,YRS,ILDV)  * CALRATIO_HP(ICL,IGP,ILDV)
            FEMWGT(IGP,ICL,YRS,ILDV) = FEMWGT(IGP,ICL,YRS,ILDV) * CALRATIO_WGT(ICL,IGP,ILDV)

            ENDDO
         ENDDO
      ENDDO

    RETURN
    END SUBROUTINE CALIBNHTSA
