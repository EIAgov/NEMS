! $Header: M:/default/source/RCS/tranfrt.f,v 1.105 2020/10/13 12:52:05 MDR Exp $
module F_

!...This routine calculates fuel consumption for freight trucks, classes 2b-8

    include 'parametr'
    include 'ncntrl'
    include 'tranrep'
    include 'macout'
    include 'ampblk'
    include 'qsblk'

!...Freight variables passed from tranfrt.f to tran.f
    include 'tranmain'

!=============================================================================================================
!... The following declarations are shared by all subroutines "contained" by TRANFRT.
!... This is an alternative to using an include file to reference global variables.
!
!     Naming conventions
!
!     Rt   = Rate
!     Sh   = Share
!     Tr   = Trend
!     Tf   = Transfer (C) commercial (F)leet (P)ersonal
!     VMT  = Vehicle mile traveled
!     Dmd  = Demand
!     Yr   = Year
!     _T   = At end of variable ie VMT_T:  Total
!     I    = At start of variable IFLT:    Index

!=============================================================================================================
!...Parameters
    INTEGER AGE,SEC,SC,CAFE,MDL,MDH,HV,FUEL,FNEW,FSTK,EMISTECH, &
            TK,NFT,FLT,RGN,CUR,LAG,NVMT,BSYR_VMT,BSYR_STK, &
			CAFEP2, TECHP2, CAFE19, SC4, FUEL9

PARAMETER(AGE = 34)      ! Number of vintages for truck stocks
PARAMETER(SEC = 18)      ! Number of Industrial sectors:
                         !   1 = Chemicals
                         !   2 = Primary Metals
                         !   3 = Processed Food
                         !   4 = Paper Products
                         !   5 = Petroleum Products
                         !   6 = Stone, Clay, Glass, & Concret
                         !   7 = Metal Durables
                         !   8 = Other Manufacturing
                         !   9 = Agriculture
                         !  10 = Mining
                         !  11 - Beverages and tobacco
                         !  12 - Pharma
                         !  13 - Fertilizers (ag chemicals)
                         !  14 - Rubber and plastics
                         !  15 - Computers
                         !  16 - Furiture
                         !  17 = Utility
                         !  18 = Government
PARAMETER(SC = 3)        ! Number of Truck Size Classes (old)
                         !   1 = Medium Light, Class 3
                         !   2 = Medium Heavy, Classes 4-6
                         !   3 = Heavy,        Classes 7-8
PARAMETER(SC4 = 4)       ! Number of Truck Size Classes - used for sales
                         !   1 = Medium Light, Class 3
                         !   2 = Medium Heavy, Classes 4-6
                         !   3 = Heavy,        Classes 7-8		
                         !   4 = Medium Light, Class 2						 
PARAMETER(GVW = 9)       ! Number of truck size classes by gvw category
                         !   1 = (empty for now)
                         !   2 = Class 2 (Pickup/van and vocational)
						 !   3 = Class 3 (Pickup/van and vocational)
						 !   4 = Class 4 (Vocational)
						 !   5 = Class 5 (Vocational)
						 !   6 = Class 6 (Vocational)
						 !   7 = Class 7 (Tractor and vocational)
						 !   8 = Class 8 (Tractor and vocational)
						 !   9 = Class 8 (Heavy-haul tractors)
PARAMETER(CAFE = 13)     ! Number of truck size classes for fuel economy standard classification (GHG Phase I Standards)
                         !   1 = pickup and van class 2b-3
                         !   2 = vocational class 2b-3
                         !   3 = vocational class 4-6
                         !   4 = vocational class 7-8
                         !   5 = tractor 7 day cab low roof
                         !   6 = tractor 7 day cab mid roof
                         !   7 = tractor 7 day cab high roof
                         !   8 = tractor 8 day cab low roof
                         !   9 = tractor 8 day cab mid roof
                         !  10 = tractor 8 day cab high roof
                         !  11 = tractor 8 sleeper cab low roof
                         !  12 = tractor 8 sleeper cab mid roof
                         !  13 = tractor 8 sleeper cab high roof
PARAMETER(CAFEP2 = 14)  ! Number of truck size classes for fuel economy standard classification for GHG Phase II Standards (does not include trailers)
                         !   1 = pickup and van class 2b-3
                         !   2 = vocational class 2b-5
                         !   3 = vocational class 6-7
                         !   4 = vocational class 8
                         !   5 = tractor 7 day cab low roof
                         !   6 = tractor 7 day cab mid roof
                         !   7 = tractor 7 day cab high roof
                         !   8 = tractor 8 day cab low roof
                         !   9 = tractor 8 day cab mid roof
                         !  10 = tractor 8 day cab high roof
                         !  11 = tractor 8 sleeper cab low roof
                         !  12 = tractor 8 sleeper cab mid roof
                         !  13 = tractor 8 sleeper cab high roof
                         !  14 = heavy-haul tractor
						 !  15 = dry box long trailer
						 !  16 = dry box short trailer
						 !  17 = refrigeration box long trailer
						 !  18 = refrigeration box short trailer
						 !  19 = partial aero trailer
						 !  20 = non-aero trailer
PARAMETER(CAFE19 = 19)   ! Number of truck size classes for Phase 2 standards
                         !   1 = Class 2 pickup and van
                         !   2 = Class 2 vocational
                         !   3 = Class 3 pickup and van
                         !   4 = Class 3 vocational
                         !   5 = Class 4 vocational
                         !   6 = Class 5 vocational
                         !   7 = Class 6 vocational
                         !   8 = Class 7 vocational
                         !   9 = Class 7 day cab low roof
                         !  10 = Class 7 day cab mid roof
                         !  11 = Class 7 day cab high roof
                         !  12 = Class 8 vocational
                         !  13 = Class 8 day cab low roof
                         !  14 = Class 8 day cab mid roof
                         !  15 = Class 8 day cab high roof
                         !  16 = Class 8 sleeper cab low roof
                         !  17 = Class 8 sleeper cab mid roof
                         !  18 = Class 8 sleeper cab high roof
                         !  19 = Class 8 heavy-haul tractor
PARAMETER(MDL = 1)       ! Size Class of vehicle: medium light, Class 3
PARAMETER(MDH = 2)       ! Size Class of vehicle: medium heavy, Classes 4-6
PARAMETER(HV =  3)       ! Size Class of vehicle: heavy,        Classes 7-8
PARAMETER(FUEL = 4)      ! Number of fuel types:
                         !   1 = Diesel
                         !   2 = Gasoline
                         !   3 = LPG
                         !   4 = CNG
PARAMETER(FUEL9 = 9)     ! Number of fuel types:
                         !   1 = Diesel
                         !   2 = Gasoline
                         !   3 = LPG
                         !   4 = CNG
                         !   5 = Flex
                         !   6 = Electric
                         !   7 = PHEV diesel
                         !   8 = PHEV gasoline
                         !   9 = Hydrogen fuel cell vehicle						 
PARAMETER(FNEW = 1)      ! NEW VEHICLE    for reporting variables
PARAMETER(FSTK = 2)      ! FREIGHT STOCK  for reporting variables

PARAMETER(TK = 1)        ! Truck
PARAMETER(NFT = 1)       ! Truck: Non fleet
PARAMETER(FLT = 2)       ! Truck: Fleet
PARAMETER(NVOC = 1)      ! Non-vocational truck
PARAMETER(VOC = 2)       ! Vocational truck 
PARAMETER(TECHP2 = 83)   !  Phase 2 HDV GHG Standards New technologies available:		
			             !	1	= Lower RR tires 1
			             !	2	= Lower RR tires 2
			             !	3	= Lower RR tires 3
			             !	4	= Lower RR tires 4
			             !	5	= Lower RR tires 5
			             !	6	= Tire Pressure Monitoring System (TPMS)
			             !	7	= Automated Tire Inflation System (ATIS)
			             !	8	= Aero Bin 1
			             !	9	= Aero Bin 2
			             !	10	= Aero Bin 3
			             !	11	= Aero Bin 4
			             !	12	= Aero Bin 5
			             !	13	= Aero Bin 6
			             !	14	= Aero Bin 7
			             !	15	= Weight reduction (via single wide tires and/or aluminum wheels)
			             !	16	= Weight reduction via material changes (assuming 10% on a 6500lb vehicle), 5% for 2b-3
			             !	17	= Weight reduction via material changes, 200lb for LH/MH vocational, additional 5% for 2b-3
			             !	18	= Low drag brakes
			             !	19	= Electric power steering
			             !	20	= Driveline friction reduction
			             !	21	= Improved accessories IACC1 (electrification)
			             !	22	= Improved accessories IACC2 (electrification) (includes IACC1)
			             !	23	= Improved accessories (42 volt electical system, power steerng, & electric AC)
			             !	24	= Air conditioning efficiency
			             !	25	= “Right sized” diesel engine
			             !	26	= Aftertreatment improvements 1 - diesel
			             !	27	= Aftertreatment improvements 2 - diesel
			             !	28	= Low-Friction Lubrications - diesel
			             !	29	= Engine friction reduction - diesel
			             !	30	= Improved water, oil, & fuel pump, pistons; valve train friction (VTF pickup, LH, MH vocational only) - diesel
			             !	31	= Parasitic/Friction (Cyl Kits, pumps, FIE), lubrication 
			             !	32	= Valve Actuation - diesel
			             !	33	= Turbo efficiency improvements 1 - diesel
			             !	34	= Low temperature EGR, improved turbochargers - diesel
			             !	35	= Sequential downsizing/turbocharging - diesel
			             !	36	= Cylinder head, Fuel rail and injector, EGR Cooler improvements 1 - diesel
			             !	37	= EGR/Intake & exhaust manifolds/turbo/VVT/ports 
			             !	38	= Turbo compounding 1 - mechanical - diesel
			             !	39	= Turbo compound with clutch - diesel
			             !	40	= Waste heat recovery - diesel
			             !	41	= Model based control
			             !	42	= Combustion/FI/Control 
			             !	43	= Downspeed 
			             !	44	= Low friction lubricants - gasoline
			             !	45	= Engine friction reduction 1 - gasoline
			             !	46	= Engine changes to accommodate low friction lubes 
			             !	47	= Engine friction reduction 2
			             !	48	= Stoichiometric gasoline direct injection (SGDI) - gasoline
			             !	49	= Coupled Cam Phasing - SOHC & OHV only - gasoline
			             !	50	= Intake Cam Phasing VVT - DOHC gasoline
			             !	51	= Dual Cam Phasing VVT - DOHC gasoline
			             !	52	= Discrete Variable Valve Lift (DVVL) - gasoline
			             !	53	= Continuously Variable Valve Lift (CVVL) - gasoline
			             !	54	= Cylinder deactivation - gasoline
			             !	55	= Turbocharge and downsize SGDI V8 to V6
			             !	56	= Cooled EGR - gasoline
			             !	57	= 6x2 axle
			             !	58	= Axle disconnect
			             !	59	= Axle downspeed
			             !	60	= High efficiency axle
			             !	61	= 8 speed transmission (= 2 gears+HEG+ASL1 for pickups)
			             !	62	= Automated & Automated manual transmission (AMT)
			             !	63	= High efficiency gearbox (HEG)
			             !	64	= Advanced Shift Strategy
			             !	65	= Early torque converter lockup (TORQ)
			             !	66	= Auto transmission, power-shift
			             !	67	= Dual clutch transmission (DCT)
			             !	68	= Neutral coast - Requires automatic
			             !	69	= Advanced cruise control - requires automatic
			             !	70	= Stop-start (no regeneration for pickups, with enhancements for vocational)
			             !	71	= Neutral idle
			             !	72	= Tamper-Proof AESS
			             !	73	= Adjustable AESS programmed to 5 min
			             !	74	= Tamper-Proof AESS w/ Diesel APU
			             !	75	= Adjustable AESS w/ Diesel APU
			             !	76	= Tamper-Proof AESS w/ Battery APU
			             !	77	= Adjustable AESS w/ Battery APU
			             !	78	= Tamper-Proof AESS w/ Auto Stop-Start
			             !	79	= Adjustable AESS w/ auto stop-start
			             !	80	= Tamper-proof AESS w/ FOH Cold, Main Engine Warm
			             !	81	= Adjustable AESS w/ FOH Cold, Main engine warm
			             !	82	= Mild hybrid (HEV)
			             !	83	= Strong Hybrid (without stop-start for vocational)

						
PARAMETER(RGN = 11)        ! Regions
PARAMETER(CUR=2,LAG=1)     ! Current and Lag year subscripts
PARAMETER(NVMT = 11)       ! NUMBER OF VEHICLE GROUPS (3 size classes and 2 fleets)
PARAMETER(BSYR_VMT = 31)   ! Nems year index of base year for truck VMT data
PARAMETER(BSYR_STK = 2021) ! Nems year index of base year for Truck Stock data		! MDRAEO2023
!=============================================================================================================
!...Subscripts
INTEGER IAGE                    ! Index for vintage
INTEGER IFUEL                   ! Index for fuel type
INTEGER IFUEL9                  ! Index for fuel type with 9 fuel types
INTEGER IFLT                    ! Index for fleet
INTEGER IVOC                    ! Index for vocation
INTEGER ISC                     ! Index for size class or ship region
INTEGER ISC4                    ! Index for mapping 4 size classes to 19
INTEGER ISC19                   ! Index for Phase 2 size classes
INTEGER ICAFE                   ! Index for size classes for fuel consumption standard
INTEGER ICAFEP2                 ! Index for size classes for Phase 2 GHG standards
INTEGER ICAFE19                 ! Index for disaggregated Phase 2 GHG standards size classes
INTEGER IMODE                   ! Index for Transportation Mode TK,RL,SP
INTEGER ISEC                    ! Index for industrial sector
INTEGER IYR                     ! Index for year
INTEGER ITECH                   ! Index for technology
INTEGER ITECHP2                 ! Index for Phase 2 GHG technology
INTEGER ITR                     ! NEMS iteration
INTEGER IREGN                   ! census division
INTEGER IGVW                    ! Index for full size classes (GVW size classes 2b - 8)
INTEGER TrkMap(CAFE)            ! Maps CAFE classes back to 3 NEMS aggregated (3) classes
INTEGER P2Map(CAFE19)           ! Maps 14 CAFE classes to 19 GVW size categories for Phase 2
INTEGER SCP2Map(cafe19)         ! Maps 4 reporting classes to 19 GVW size classes
INTEGER*2 CYAFVXG(SC4,FUEL9,FLT) ! Logistic mkt penetration curve parameter # of yrs, AFV
INTEGER*2 TRGSHXG(CAFE19,FUEL9,FLT) ! Logistics paramter: 1/2 way to max Mkt penetration
!=============================================================================================================
!...switches for freight transportation scenarios
INTEGER      RTOVALUE
EXTERNAL     RTOVALUE
INTEGER      IFRTEFF      !...side case switches
                          !...1  = high tech case
                          !...2  = frozen tech case
                          !...3  = zero natural gas vehicle incremental cost (permanent credit)
                          !...4  = NATGAS Act tax credit phase out for incremental cost (temporary tax credit)
                          !...3  = expanded natural gas market
                          !...4  = expanded natural gas market NATGAS ACT (S.1863)
                          !...5  = turns on Phase 2 HDV standards
                          !...6  = reference natural gas market with NATGAS ACT
                          !...7  = reference natural gas market with permanent natural gas vehicle tax credit
                          !...8  = ubiquitious natural gas market
                          !...9  = ubiquitious natural gas market NATGAS ACT
                          !...10 = ubiquitious natural gas market with permanent natural gas vehicle tax credit
!=============================================================================================================
!...Copy of frteff
REAL ANNVMT(SC,AGE,FUEL9,VOC)         ! Average annual VMT per vehicle
REAL BFSHXG(SC4,FUEL9,FLT)             ! Base yr(92) mkt share of each fuel
REAL BASE_MKT_P1(FUEL9,CAFE19)          ! Market share for phase 1 based on 19 class sizes in phase 2 (the last size class is 0)
REAL BREAKOUT_46(6,3,AGE)              ! Stock breakout for reporting class 4-6 into individual classes
REAL BREAKOUT_78(6,2,AGE)              ! Stock breakout for reporting class 7-8 into individual classes
REAL Fuel_$(MNUMYR,FUEL9)              ! Price of fuel, in $ per MBtu
REAL Avg_Fuel_$(FUEL9)                 ! Average price of fuel over 3 years
REAL Fuel_$_regn(MNUMYR,FUEL9,MNUMCR)  ! Price of fuel, in $ per MBtu
REAL Avg_Fuel_$_regn(fuel9,mnumcr)     ! Average regional price of fuel over 3 years
REAL BMPGSTK2B(AGE,MNUMYR,6)            ! class 3 historic fuel economy by vintage
REAL BMPGSTK3(AGE,MNUMYR,6)            ! class 3 historic fuel economy by vintage
REAL BMPGSTK46(AGE,MNUMYR,6)           ! class 4-6 historic fuel economy by vintage
REAL BMPGSTK78(AGE,MNUMYR,6)           ! class 7-8 historic fuel economy by vintage
REAL HDV_MPG(MNUMYR,SC4,AGE,FUEL9)     ! fuel economy size class, vintage, and fuel in mpg miles/cubic cng
REAL HDV_MPG_S_F(MNUMYR,SC,FUEL9)      ! fuel economy by size class and fuel
REAL HDV_MPG_S(MNUMYR,SC)              ! fuel economy by size class
REAL CSTDXG(SC4,FLT)                   ! Mkt penetration curve parm for diesel	
REAL CSTDVXG(SC4,FLT)                  ! Mkt penetration curve parm for diesel
REAL DISCRTXG                          ! Discount rate
REAL DISCRTXGL                         ! Discount rate following standards
REAL HRATE(SC4,FUEL9)                  ! heat rate by fuel type
REAL EFSHXG(SC4,FUEL9,FLT)             ! Final mtk share of each fuel

!...Phase 2 GHG HDV Standards parameters
INTEGER*2 	TECHFYR_P2(TECHP2,CAFEP2,FUEL9)   		! First year technology is viable by phase-2 cafe class and fuel type
INTEGER*2 	REQUIRED_P2(TECHP2)          			! Which technologies require which
INTEGER*2 	TECH_REQ(TECHP2,2)           			! Required technologies 
INTEGER*2 	REQUIRES_TECH(3,TECHP2)      			! = [REQUIRED_P2 TECH_REQ]
INTEGER*2 	REQUIRED_ANY_P2(TECHP2)      			! Which technologies require which
INTEGER*2 	TECH_R_ANY(3,TECHP2)         			! Required technologies
INTEGER*2 	REQ_ANY_TECH(4,TECHP2)       			! = [REQUIRED_P2 TECH_REQ]
INTEGER*2 	TECH_NUM(TECHP2)             			! Technology number
INTEGER*2 	TECHLEARN_P2(TECHP2,CAFEP2)  			! Technology cost learning curve assignments
INTEGER*2 	PAYBACK_P2(CAFEP2)           			! Payback period for technology by phase-2 cafe class
INTEGER*2 	PAYBACK1_P2(TECHP2,CAFE19)
INTEGER*2 	SUPERSCEDE_P2(TECHP2)        			! Which technologies superscede which
INTEGER*2 	TECH_SUP(TECHP2,9)           			! Superscede technologies
INTEGER*2 	SUPERSEDES_TECH(10,TECHP2)   			! = [SUPERSEDED_P2 TECH_SUP]
INTEGER*2 	SYNERGY_P2(TECHP2)           			! Which technologies have synergies 
INTEGER*2 	TECH_SYN(TECHP2,1)           			! Synergy technologies
INTEGER*2 	SYNERGY_TECH(2,TECHP2)       			! = [SYNERGY_P2 TECH_SYN]
REAL 		BASE_MPG_P2(FUEL9,CAFE19)         		! Base MPG by gvw class
REAL 		NEW_MPG_19(MNUMYR,FUEL9,CAFE19)   		! New MPG by 19 cafe classes
REAL 		NEW_MPG_P2(MNUMYR,FUEL9,CAFEP2)   		! New MPG by 14 cafe classes
REAL 		BASE_MKT_P2(FUEL9,CAFE19)         		! Base market share by gvw class
REAL 		TRIGGER_PRICE_P2(TECHP2,CAFE19,FUEL9)
REAL 		TECHEFF_P2(TECHP2,CAFEP2)         		! Gain in efficiency from technology adoption by phase-2 cafe class
REAL 		TECHCOST_P2(TECHP2,CAFEP2)        		! Cost of technology adoption by phase-2 cafe class
REAL 		TECHVAR_P2(TECHP2,CAFEP2)         		! Fuel price sensitivity parameter
REAL 		TECHMID_P2(TECHP2,CAFEP2)         		! Number of years until 50% market penetration
REAL 		TECHSHAPE_P2(TECHP2,CAFEP2)       		! Market penetration shape constant
REAL 		TECHBASE_P2(TECHP2,CAFE19,FUEL9)  		! Base market penetration of technologies -- from final AEO2022 run with Phase 1 regulations
REAL 		TECHMAX_P2(TECHP2,CAFEP2)         		! Maximum market share
REAL 		TECH_PER(TECHP2,1)                		! Synergy percent increase/decrease in fuel economy
REAL 		SYN_PER(2,TECHP2)                 		! = tech_per
REAL 		FE25                              		! Base fuel economy class 2b-5 vocational
REAL 		FE67                              		! Base fuel economy class 6-7 vocational
REAL 		FE8                               		! Base fuel economcy class 8 vocational
REAL 		ADJ2                              		! Difference in fuel economy class 2b pickup and van / vocational
REAL 		ADJ45                             		! Difference in fuel economy class 4 / class 5
REAL 		HDV_STANDARD_P2(FUEL9,mnumyr,CAFEP2)	! HDV fuel consumption and GHG emissions standards expressed in MPG
REAL 		LEARNING_P2(14,MNUMYR)            		! Technology cost learning curve reductions
REAL 		DISCRTXG_P2                       		! Discount rate
REAL 		DISCRTXGL_P2                      		! Discount rate following the standard
REAL 		PREFF_P2(TECHP2,CAFE19,FUEL9)     		! Market penertation price sensitvity mulitpler
REAL 		ANNVMT_19(CAFE19,AGE,FUEL9)       		! Average annual VMT per vehicle - for each of the 19 different size categories
!REAL 		AVMT_14(CAFEP2,AGE,FUEL9)         		! Maximum average annual VMT per vehicle for each Phase 2 category		! MDRAEO2023
REAL 		VMTFLT_19(MNUMYR,CAFE19,AGE,FUEL9) 		! Total VMT
!REAL 		VMT_14(MNUMYR,CAFEP2,AGE,FUEL9)   		! Maxiumum total VMT for each Phase 2 category							! MDRAEO2023
REAL 		TRKSTK_19(MNUMYR,CAFE19,AGE,FUEL9)  	! Stock total by 19 different CAFE size classes
REAL 		CREDSALES(MNUMYR,CAFE19,FUEL9)
INTEGER*2 	TECH_CNT_SUP(TECHP2), TECH_CNT_SYN(TECHP2), tech_cnt_req(techp2), NUM_SUP, NUM_SYN, NUM_REQ 

!...natural gas sensitivity case	
REAL Fuel_Shr(SC,FUEL,FLT)             ! Fuel shares for New trucks by size class, fleet/nonfleet, region
REAL Fuel_Shr_Stk(SC,FUEL9,FLT)        ! Fuel shares for the entire stock of trucks
REAL Fuel_Shr_regn(mnumyr,cafe19,FUEL9,FLT,MNUMCR) ! Fuel shares for New trucks by size class, fleet/nonfleet, region
REAL FUELBTUR(SC,FUEL9,FLT,MNUMCR)     ! Total truck fuel consumption in trillion Btu
REAL FUELDMD(SC,FUEL,FLT)              ! Freight truck fuel consumption
REAL FUELDMDR(SC,FUEL9,FLT,MNUMCR)     ! Freight truck fuel consumption
REAL PRAFDFXG(SC4,FUEL9)               ! Parm: variation  AFV Mtk share due to diff fuel prices
REAL SCRAP(AGE,SC4)                    ! Read in truck scrappage rate
REAL SCRAP_D(AGE,SC4)                  ! Read in diesel truck scrappage rate
REAL SCRAP_RATE(SC4,AGE,FUEL9)         ! Truck scrappage rate
REAL TRKSTK(CUR,SC,AGE,FUEL9,FLT)      ! truck population (year,size class, vintage,fuel, nonflt/fleet)
REAL trkstock(mnumyr,GVW,age,fuel9,VOC,mnumcr)       ! truck stock history by year, class (2b-8), vintage, fuel type, vocational, and region
REAL TRK_19_REGN(mnumyr,cafe19,age,fuel9,flt,mnumcr) ! truck population history by year, phase 2 size class (19), vintage, fuel type, vocational, fleet, and region 

REAL TSTK_PER_P2(mnumyr,cafe19,fuel9)   ! Percent of total sales in a size class (19) out of Phase 2 size class
REAL TSTK_PER_SC(mnumyr,cafe19,fuel9)   ! Percent of total sales in a size class by reporting size class
REAL TFFXGRT(SC4,AGE)                   ! Exog % of trucks/vintage transfered from fleet to non_fleet
REAL VMTFLT(cur,SC,AGE,FUEL9,FLT)       ! VMT at its most detailed
REAL VMTFLTR(cur,SC,AGE,FUEL9,FLT,MNUMCR) ! VMT at its most detailed by census division
REAL CLS3STKHIST(AGE,6:22,6)
REAL CLS46STKHIST(AGE,6:22,6) 
REAL CLS78STKHIST(AGE,6:22,6) 
REAL FLEETSHR(SC4)                     ! percent of vehicles in fleet use
REAL VMTCLS3V(AGE,FUEL)                ! vmt per truck by fuel and vintage, class 3
REAL VMTCLS46V(AGE,FUEL)               ! vmt per truck by fuel and vintage, class 4-6
REAL VMTCLS78V(AGE,FUEL)               ! vmt per truck by fuel and vintage, class 7-8 vocation
REAL VMTCLS78(AGE,FUEL)                ! vmt per truck by fuel and vintage, class 7-8 tractor
REAL NEWCLS46(MNUMYR)                  ! share of truck sales in class 4-8 that are class 4-6, by year
REAL VMTDMDR(MNUMYR,MNUMCR)            ! VMT by census division
REAL TR_VMT(MNUMYR,MNUMCR)             ! Total truck vehicle miles travelled (billion)
REAL THIST_VMT(MNUMYR,MNUMCR-2)        ! Historical truck vehicle miles travelled (billion) by census divsion
REAL TTM_OUTPUT(MNUMCR-2,SEC-2)        ! Truck ton-miles per $ output
REAL TTONMI_ORIG(MNUMCR-2,SEC-2)       ! Truck ton-miles by census division and industrial sector for base year
REAL NEWTRUCKS_regn(MNUMYR,SC4,3,MNUMCR) ! Sales of new trucks by size clase, fleet/nonfleet+total, and region
REAL VMT_VEH(11,FLT,SC4)               ! vmt per vehicle by non-fleet, fleet, and size class 3-6 and 7-8
REAL VEH_SHR(11,FLT,SC4)               ! percent share of vehicle by non-fleet, fleet, and size class 3-6 and 7-8
REAL HARMONIC_MEAN                     ! Function to calculate average mpg weighted by VMT or MPG
REAL HDV_STANDARD_C(MNUMYR,CAFEP2)     ! HDV fuel consumption and GHG emissions standards expressed in MPG combined across fuels
REAL INC_TECH_COST(MNUMYR,FUEL9,SC4)
REAL INC_TECH_$(MNUMYR,FUEL9,SC4)
REAL EG_MPG                            ! Ratio of elec motor MPG to gas engine for PHEV engines
REAL PHEVELECVMT                       ! Elec motor share of VMT for PHEV engines
REAL PctPHEV
REAL TRKTMP(SC,FLT+1),VMTTEST,AGGVMT,VMTTMP(2),SUMOVERFUELS  ! temporary variables

EXTERNAL HARMONIC_MEAN

! ... Connected and autonomous freight fleets
INTEGER TOONYEAR*2
REAL TOONSHR(MNUMYR)

! ... Waterborne Freight Module
! ... Domestic Waterborne

REAL         DOMSHIP_FUEL_SHR(4,MNUMYR)                             ! fuel share
REAL         DSHIST_TONMI(MNUMYR, MNUMCR-2)                         ! historic total domestic marine ton-miles
REAL         DSTM_OUTPUT(MNUMCR-2,16)                               ! domestic marine ton-miles per $ output
REAL         ANN_DECLINE(MNUMYR)                                    ! domestic marine annual rate of ton-mile per $ output decline
REAL         TQDSHIPT(MNUMYR,MNUMCR)                                ! domestic marine energy demand
REAL         DTM_SHARES(MNUMCR-2,16)                              	! distribution of ton-miles by industrial sector within each census division
REAL		 DSADD_TONMI(MNUMYR,MNUMCR,16) 						! domestic marine ton-miles by census division and industrial sector

! ... International Waterborne
REAL         GROSST(MNUMYR)                                         ! gross tons shipped
REAL         ISFD(5,MNUMYR)                                         ! energy demand by fuel type
                                                                        !   1) diesel
                                                                        !   2) residual
                                                                        !   3) CNG
                                                                        !   4) LNG
                                                                        !   5) low sulfur fuel oil

REAL         INTSHIP_FUEL_SHR(5,mnumyr)                             ! fuel share
REAL         INTSHIPLOGIT_FAC(5,3)                           ! price selection factor for fuel shares
REAL         LSFORISK(mnumyr)                                       ! risk associated with the fuel availability of lsfo (IMO 2020)

REAL         FUELCONS(MNUMCR-2,9)                                   ! history/base year for computed ECA consumption by census division (9) and vessel and sub-vessel type (9)
REAL         FLEETTO(9)                                             ! fleet turnover, by vessel and sub-vessel type, starting in base year, currently denoted as a constant
REAL         EFFINC(9)                                              ! improvement per year in main engine efficiency, as a percentage

! ... Global definitions

integer      iy                                                     ! year index corresponding to first year of inputs
integer      num_to_read                                            ! number of years in input ranges
integer      First_Read_Year                                        ! = 1995
INTEGER      N                                                      ! tran variable for curiyr
INTEGER      YRS                                                    ! actual model year (1989+curiyr)

!=============================================================================================================
!...all variables placed in common--alternative to CONTAINS
common/tranfrtint/ IAGE,IFUEL,IFLT,ISC,ICAFE,IMODE,ISEC,IYR,ITECH,ITR,IREGN,CYAFVXG, &
                   TRGSHXG, IFRTEFF, ICAFEP2, ICAFE19, ISC19, PAYBACK_P2, PAYBACK1_P2, &
                   TECH_CNT_SUP, TECH_CNT_SYN, NUM_SUP, NUM_SYN, NUM_REQ,	SUPERSCEDE_P2, TECH_SUP, SYNERGY_P2, TECH_SYN,	REQUIRED_P2, TECH_REQ, TECH_NUM, &
				   SYNERGY_TECH, SUPERSEDES_TECH, REQUIRES_tech, TECHFYR_P2, TECHLEARN_P2, REQ_ANY_TECH, TECH_R_ANY, REQUIRED_ANY_P2, &
                   TOONYEAR

common/tranfrtreal/ANNVMT,BFSHXG,Avg_Fuel_$,HDV_MPG, Fuel_$,CSTDXG, CSTDVXG, DISCRTXG, EFSHXG,&
                   Fuel_Shr, Fuel_Shr_Stk, DISCRTXGL,FUELDMD, PRAFDFXG, SCRAP, TRKSTK, TFFXGRT, VMTFLT,&
                   VMTCLS3V, VMTCLS46V, VMTCLS78V, VMTCLS78, NEWCLS46,&
                   VMTDMDR, HDV_MPG_S_F, HDV_MPG_S, VMT_VEH, VEH_SHR, TRKTMP, SUMOVERFUELS,  &
                   TR_VMT, THIST_VMT, TTONMI_ORIG, VMTFLTR, FUELDMDR, FUELBTUR, TTM_OUTPUT, &
				   TRKSTOCK, SCRAP_RATE, Fuel_$_regn, Avg_Fuel_$_regn, fuel_shr_regn,newtrucks_regn, EG_MPG, PHEVELECVMT, PctPHEV, &
				   BASE_MKT_P1, BREAKOUT_46, BREAKOUT_78, BASE_MPG_P2, BASE_MKT_P2, TECHEFF_P2, TECHCOST_P2, TECHVAR_P2, & 
				   TECHMID_P2, TECHSHAPE_P2, TECHMAX_P2, HDV_STANDARD_P2, LEARNING_P2, DISCRTXG_P2, DISCRTXGL_P2, TECHBASE_P2, &
				   PREFF_P2, ANNVMT_19, VMTFLT_19, TRKSTK_19,  TECH_PER, FE25, FE67, FE8, ADJ2, ADJ45, TRIGGER_PRICE_P2, SYN_PER, &       
				   NEW_MPG_19, NEW_MPG_P2, TSTK_PER_P2, TSTK_PER_SC, TRK_19_REGN,TOONSHR, &			! , AVMT_14, VMT_14  MDRAEO2023
				   DTM_SHARES, DOMSHIP_FUEL_SHR, DSHIST_TONMI, DSTM_OUTPUT, DSADD_TONMI, ANN_DECLINE, TQDSHIPT

!...Map for how CAFE groups are defined back to NEMS HDV size classes: (1)light-medium-HDV (2)medium-HDV (3)heavy-HDV
    data TrkMap/1,1,2,3,3,3,3,3,3,3,3,3,3/
!...Map 14 CAFE groups to 19 size classes
    data P2Map/1,2,1,2,2,2,3,3,5,6,7,4,8,9,10,11,12,13,14/
!...Map 4 size categories into 19 size classes
    data SCP2Map/4,4,1,1,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3/
end module F_
!=============================================================================================================
!...Freight subroutines
  SUBROUTINE TRANFRT(FRTEFF,ICALL)
  USE F_
  IMPLICIT NONE
    integer FRTEFF        ! (was Technology scenario 0: none 1:high tech 2: frozen)
    INTEGER ICALL, once/.false./
    REAL :: VMTFLT_SAFF_TR(SC), VMT_TRR(MNUMYR)

    ifrteff=frteff        ! copy argument from tran.f to common

    IYR = CURIYR          ! nems year index
    ITR = CURITR          ! nems iteration
    N = CURIYR            ! nems year index for marine data reading
    YRS = CURCALYR        ! NEMS year index

    FIRST_READ_YEAR = 1995
    IY = FIRST_READ_YEAR - BASEYR + 1
    NUM_TO_READ = IJUMPYR -(FIRST_READ_YEAR - BASEYR)

    IF(ICALL.EQ.1) THEN
       CALL TFRTRPT
       RETURN
    ENDIF

!...On first iteration of each year, copy from current to lag year for any variables year indexed that way
    IF(CURITR.EQ.1) THEN
      VMTFLT(LAG,:,:,:,:)=VMTFLT(CUR,:,:,:,:)
      TRKSTK(lag,:,:,:,:)=TRKSTK(cur,:,:,:,:)
    ENDIF

    HRATE(:,1) = 138700                    ! Diesel
    HRATE(:,2) = 125071                    ! Gasoline
    HRATE(:,3) = 125071                    ! LPG
    HRATE(1,4) = 125071                    ! CNG Class 3
    HRATE(2,4) = 125071 		           ! CNG Class 4-6
    HRATE(3,4) = 138700                    ! LNG Class 7-8
	HRATE(4,4) = 125071                    ! CNG Class 2
	HRATE(:,5) = 125071                    ! E85
	HRATE(:,6) = 138700                    ! Electric
	HRATE(:,7) = 138700                    ! PHEV Diesel
	HRATE(:,8) = 125071                    ! PHEV Gasoline
	HRATE(:,9) = 138700                    ! Hydrogen

!...Determine Truck Fuel Use as follows
!...1) Update the truck stock vintages by applying scrappage rates.
!...2) Transfer some trucks stocks from fleet ownership to non-fleet ownership
!...3) Distribute new truck sales to size classes and ownership classes
!...4) Estimate fuel economy and fuel choice shares of new truck sales under
!...   technology penetration assumptions
!...5) Estimate VMT growth by economic sector as a function of economic output.
!...   Determine VMT per truck needed to meet that VMT growth, including the stock of
!...   new trucks. Calculate VMT by size class, vintage, fuel, and fleet/nonfleet
!...6) Calculate fuel consumption as vmt / mpg

    CALL INIT

!...Estimate new vehicle sales, stocks, and fuel economy
    if(curcalyr.gt.2017) then
      CALL TRUCK_NEW
    endif

    CALL TRUCK_STOCK

!...Estimate aggregate VMT and per truck VMT
    CALL TRUCK_VMT

!...Calculate fuel demand from vmt and mpg
    CALL TRUCK_FUEL

!...aggregate vmt by sector and vintage sent to TRAN for Table 7
    do isc=1,sc
      do ifuel=1,fuel9
        do iregn=1,mnumcr
          do iage=1,age
            VMTFLT_SF_TR(ISC,IAGE,IFUEL,iregn) =  sum( VMTFLTR(cur,ISC,IAGE,IFUEL,1:FLT,iregn) )
          enddo
          VMTFLT_SAF_TR(ISC,IFUEL,iregn) = sum( VMTFLT_SF_TR(ISC,1:AGE,IFUEL,iregn) )
        enddo
      enddo
        VMTFLT_SAFF_TR(ISC) =sum( VMTFLT_SAF_TR(ISC,1:FUEL9,1:mnumcr-2) )
    enddo

    VMT_TRR(IYR) = sum(VMTFLT_SAF_TR(1:SC,1:FUEL9,1:mnumcr-2) )

!...average MPG over vintages, fuels, and size class
    do isc=1,sc
      do ifuel=1,fuel9
        HDV_MPG_S_F(IYR,ISC,IFUEL) = HARMONIC_MEAN(HDV_MPG(iyr,ISC,1:AGE,IFUEL),VMTFLT_SF_TR(ISC,1:AGE,IFUEL,11),AGE)
	  if(HDV_MPG_S_F(IYR,ISC,IFUEL).eq.0) HDV_MPG_S_F(IYR,ISC,IFUEL) = HDV_MPG_S_F(IYR-1,ISC,IFUEL)
      enddo
      HDV_MPG_S(IYR,ISC) = HARMONIC_MEAN(HDV_MPG_S_F(IYR,ISC,1:FUEL9), VMTFLT_SAF_TR(ISC,1:FUEL9,11) ,FUEL)
	  if(HDV_MPG_S(IYR,ISC).eq.0) HDV_MPG_S(IYR,ISC) = HDV_MPG_S(IYR-1,ISC)
    enddo

		
!...Average MPG over size classes - sent to TRAN to determine bus efficiency
    FTMPG_TR(IYR) = HARMONIC_MEAN( HDV_MPG_S(IYR,1:SC),VMTFLT_SAFF_TR(1:SC),SC)

!...Duplicate average MPG sent to Table 7 (ftab)
    TRFTMPG(IYR) = FTMPG_TR(IYR)

  RETURN
  END SUBROUTINE TRANFRT

! CONTAINS

!=============================================================================================================
 SUBROUTINE TRUCK_NEW
 USE F_
 IMPLICIT NONE

    INTEGER IP, J ,I
	INTEGER*2 LEARN_YR(14)/2014,2014,2015,2014,2014,2017,2014,2015,2016,2014,2021,2018,2021,2021/
	INTEGER*2 FYR_P2(techp2,cafe19)
	INTEGER*2 LEARN_ADJ(techp2,cafe19)
    REAL    P                                              !...market penetration fraction
	REAL    mpg_calc(mnumyr,fuel9,cafep2)
	REAL    mpg_p2(mnumyr,cafep2)
	REAL    stndr_calc(mnumyr,fuel9,cafep2)
	REAL    HDV_STANDARD_dge(mnumyr,fuel9,cafep2)
    INTEGER return_stat, INOTE
	REAL    TECHCOSTyr_p2(mnumyr,techp2,cafe19)
	REAL    TEMP_BTU_19(cafe19,fuel9), TEMP_BTU_P2(cafep2,fuel9)   
	REAL    techeff_19(techp2,cafe19,fuel9), TEMP_TRIG_P2(techp2,cafe19)
	REAL    TECHSHR_P2(mnumyr,techp2,cafe19,fuel9), TECHPENYR_P2(mnumyr,techp2,cafe19,fuel9)
	REAL    TEMP_P2, TOT_MKT, MAX_SHARE, MAX_SHARE_S(techp2)
	REAL    REQ_MKT, SYNERGY_LOSS(techp2), delta_mkt, sales_share(cafep2,fuel9)
	REAL    MPGEFF_19(fuel9,cafe19), techadjshr_P2(mnumyr,techp2,cafe19,fuel9)
	REAL    mpg_p2_cred(mnumyr,cafep2),p2bank(5,cafep2),p2work(5,cafep2),p2banka(cafep2),p2need,p2needx(mnumyr,cafep2),p2bankbal(mnumyr,cafep2)
	REAL    pass_p2(cafep2), cls2b3puv(fuel9),cls2b5voc(fuel9),cls6_7voc(fuel9),mpg_adj_p2(cafep2)
	REAL    TECHSHARE_P2(mnumyr,techp2,7,fuel9), inc_tech_cost_19(mnumyr,cafe19,fuel9), inc_tech_cost_p2(mnumyr,cafep2,fuel9)
	LOGICAL pass_standards_p2(cafep2)
	LOGICAL tech_once_p2(techp2,cafe19,fuel9)
	LOGICAL REQUIRED
   
!--------------------------------------------------------------------------------------------------------------------------------------------!
!... Phase 2 Heavy Duty Vehicle GHG and Fuel Economy Standards (Phase 2)
!... This is currently not in the reference case and is not modeling trailers
!... Phase 2:
!...    - Picks up where Phase 1 left off. The first compliance year is 2021 and the last compliance year is 2027
!...    - Expands the standards to include 14 categories of vehicles (heavy haul added) instead of 13 and includes class 2b vehicles
!...    - The categories of vehicles have been realligned (e.g., light heavy in Phase 1 is not the same as light heavy in Phase 2 and so on)
!--------------------------------------------------------------------------------------------------------------------------------------------!
	
	IF (curcalyr.gt.2017) THEN
	
!    else ! curcalyr > 2017	
!...  calculate average fuel prices over the last 3 years	
	  do ifuel = 1,fuel9
        do iregn = 1,mnumcr-2
         Avg_Fuel_$_regn(ifuel,iregn) = (Fuel_$_regn(iyr,ifuel,iregn) + Fuel_$_regn(iyr-1,ifuel,iregn) + Fuel_$_regn(iyr-2,ifuel,iregn))/3.0 
        enddo
        Avg_Fuel_$(ifuel) = (Fuel_$(iyr,ifuel) + Fuel_$(iyr-1,ifuel) + Fuel_$(iyr-2,ifuel))/3.0
	  enddo	
	 
      if(curcalyr.eq.2018) then
	    do ifuel = 1,fuel9
!... Filling in phase 2 new_mpg variable - 19 CAFE class sizes
      	  new_mpg_19(iyr-1,ifuel,:) = base_mpg_p2(ifuel,:)
          TECHPENYR_P2(curiyr:mnumyr,:,:,:) = 0.0

!... Align techs from Phase 1 to Phase 2 for base tech penetration
          do icafe19 = 1,cafe19
            do itechp2 = 1,techp2	   
	          TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel) = TECHBASE_P2(itechp2,icafe19,ifuel)
		    enddo
	      enddo
        enddo     ! End fuel loop
	  endif      ! End Phase 2 initial year section
	
	  pass_standards_p2(:) = .false.
      tech_once_p2(:,:,:) = .false.
      do ifuel = 1,fuel9
        do icafe19 = 1,cafe19
!...    calculate new truck energy consumption (million Btu) per vehicle
          TEMP_BTU_19(icafe19,ifuel) = 0.0		
		  if(icafe.le.4) then
            TEMP_BTU_19(icafe19,ifuel) = (VMTFLT_19(iyr-1,icafe19,1,ifuel)/NEW_MPG_19(iyr-1,ifuel,icafe19))*HRATE(1,ifuel)*(1E-6)
		  elseif(icafe.le.7) then
            TEMP_BTU_19(icafe19,ifuel) = (VMTFLT_19(iyr-1,icafe19,1,ifuel)/NEW_MPG_19(iyr-1,ifuel,icafe19))*HRATE(2,ifuel)*(1E-6)
          else
            TEMP_BTU_19(icafe19,ifuel) = (VMTFLT_19(iyr-1,icafe19,1,ifuel)/NEW_MPG_19(iyr-1,ifuel,icafe19))*HRATE(3,ifuel)*(1E-6)
          endif				
          if(TRKSTK_19(iyr-1,icafe19,1,ifuel).gt.0.0) TEMP_BTU_19(icafe19,ifuel) = TEMP_BTU_19(icafe19,ifuel)/TRKSTK_19(iyr-1,icafe19,1,ifuel)	
	    enddo
!...    calculate sales distribution - normalize energy demand
        cls2b3puv(ifuel) = TrkStk_19(iyr-1,1,1,ifuel)+TrkStk_19(iyr-1,3,1,ifuel)
        cls2b5voc(ifuel) = Trkstk_19(iyr-1,2,1,ifuel)+TrkStk_19(iyr-1,4,1,ifuel)+TrkStk_19(iyr-1,5,1,ifuel)+TrkStk_19(iyr-1,6,1,ifuel)
        cls6_7voc(ifuel) = TrkStk_19(iyr-1,7,1,ifuel)+TrkStk_19(iyr-1,8,1,ifuel)		
!...	weighted average consumption
		Temp_Btu_p2(1,ifuel) = Temp_Btu_19(1,ifuel)*(TrkStk_19(iyr-1,1,1,ifuel)/cls2b3puv(ifuel))+&              ! Class 2b - 3 pickup and van
		                       Temp_Btu_19(3,ifuel)*(TrkStk_19(iyr-1,3,1,ifuel)/cls2b3puv(ifuel))
        Temp_Btu_p2(2,ifuel) = Temp_Btu_19(2,ifuel)*(TrkStk_19(iyr-1,2,1,ifuel)/cls2b5voc(ifuel))+&              ! Class 2b - 5 vocational
		                       Temp_Btu_19(4,ifuel)*(TrkStk_19(iyr-1,4,1,ifuel)/cls2b5voc(ifuel))+&
							   Temp_Btu_19(5,ifuel)*(TrkStk_19(iyr-1,5,1,ifuel)/cls2b5voc(ifuel))+&
		                       Temp_Btu_19(6,ifuel)*(TrkStk_19(iyr-1,6,1,ifuel)/cls2b5voc(ifuel))
		Temp_Btu_p2(3,ifuel) = Temp_Btu_19(7,ifuel)*(TrkStk_19(iyr-1,7,1,ifuel)/cls6_7voc(ifuel))+&               ! Class 6 - 7 vocational
		                       Temp_Btu_19(8,ifuel)*(TrkStk_19(iyr-1,8,1,ifuel)/cls6_7voc(ifuel))
        do icafe19 = 9,cafe19
          isc19 = P2Map(icafe19)
	      TEMP_BTU_p2(isc19,ifuel) = TEMP_BTU_19(icafe19,ifuel)
	    enddo

!... Incorporating technology cost learning curve into technology cost variable
	    do icafe19 = 1,cafe19
          isc19 = P2Map(icafe19)
	      do itechp2 = 1,techp2
            if(techlearn_p2(itechp2,isc19).gt.1) then 
              fyr_p2(itechp2,isc19) = minval(techfyr_p2(itechp2,isc19,1:9))
		      if(fyr_p2(itechp2,isc19).gt.learn_yr(techlearn_p2(itechp2,isc19))) learn_adj(itechp2,isc19) = fyr_p2(itechp2,isc19)-learn_yr(techlearn_p2(itechp2,isc19))
                   TECHCOSTyr_p2(iyr,itechp2,icafe19) = techcost_p2(itechp2,isc19)*learning_p2(techlearn_p2(itechp2,isc19),iyr-learn_adj(itechp2,isc19))
		    else
		      techcostyr_p2(iyr,itechp2,icafe19) = techcost_p2(itechp2,isc19)
            endif		  
		  enddo
	    enddo
      enddo		
	  
!...  determine technology penetration trigger price for new technology penetration
      do j = 1,10
	    do ifuel = 1,fuel9
	      do icafe19 = 1,cafe19
		    isc19 = P2Map(icafe19)	! 1-14	
            if(pass_standards_P2(isc19).and.j.ge.2) cycle	
            TECHSHR_P2(iyr,:,icafe19,ifuel) = 0.			
            do itechp2 = 1,techp2  	
			  techeff_19(itechp2,icafe19,ifuel) = techeff_p2(itechp2,isc19)
              TEMP_TRIG_P2(itechp2,icafe19) = 1.0
              PREFF_P2 = 1.0
!...        set trigger price for cost effectiveness calculation for technologies, penetrate technologies
!...        application criteria: is technology available by size class and fuel?
              if(curcalyr.ge.TECHFYR_P2(itechp2,isc19,ifuel)) then				
                if(pass_standards_P2(isc19)) then
                  Payback1_P2(itechp2,icafe19) = Payback_P2(isc19)
                else
                  Payback1_P2(itechp2,icafe19) = Payback_P2(isc19)+j-1
                endif
!...          calculate present value of energy savings over payback period
                do ip = 1,payback1_P2(itechp2,icafe19)
                  if(curcalyr.le.2027)then		
			        if(ANNVMT_19(icafe19,1,ifuel).gt.0) TEMP_TRIG_P2(itechp2,icafe19) = TEMP_TRIG_P2(itechp2,icafe19) + &
						((TEMP_BTU_p2(isc19,ifuel)*ANNVMT_19(icafe19,ip,ifuel)/ANNVMT_19(icafe19,1,ifuel)*techeff_19(itechp2,icafe19,ifuel))/((1.0 + DISCRTXG_P2)**ip))
                  else
					if(ANNVMT_19(icafe19,1,ifuel).gt.0) TEMP_TRIG_P2(itechp2,icafe19) = TEMP_TRIG_P2(itechp2,icafe19) + &
					    ((TEMP_BTU_p2(isc19,ifuel)*ANNVMT_19(icafe19,ip,ifuel)/ANNVMT_19(icafe19,1,ifuel)*techeff_19(itechp2,icafe19,ifuel))/((1.0 + DISCRTXGL_P2)**ip))
                  endif								
                enddo				
!...          calculate trigger price
                TRIGGER_PRICE_P2(itechp2,icafe19,ifuel) = TECHCOSTyr_p2(iyr,itechp2,icafe19)/TEMP_TRIG_P2(itechp2,icafe19) 
!...          set market penetration price sensitivity factor
                PREFF_P2(itechp2,icafe19,ifuel) = 1.0 + TECHVAR_P2(itechp2,isc19)*((Avg_Fuel_$(ifuel)/TRIGGER_PRICE_P2(itechp2,icafe19,ifuel)) - 1.0)
                if(PREFF_P2(itechp2,icafe19,ifuel).gt.1.4) PREFF_P2(itechp2,icafe19,ifuel) = 1.4 
                if((curcalyr).ge.2028)then
                  if(PREFF_P2(itechp2,icafe19,ifuel).gt.1.3) PREFF_P2(itechp2,icafe19,ifuel) = 1.3 
                endif
                if((curcalyr).ge.2035)then
                  if(PREFF_P2(itechp2,icafe19,ifuel).gt.1.2) PREFF_P2(itechp2,icafe19,ifuel) = 1.2 
                endif					
!...          is the fuel price above the trigger?		
                if(Avg_Fuel_$(ifuel).ge.TRIGGER_PRICE_P2(itechp2,icafe19,ifuel).or.TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel).gt.0.0) then
!...          market penetration equation:  s-shaped logistical equation to estimate market penetration over time
                  if(Avg_Fuel_$(ifuel).ge.TRIGGER_PRICE_P2(itechp2,icafe19,ifuel)) then
                    if(.not.tech_once_P2(itechp2,icafe19,ifuel)) then
                      TECHPENYR_P2(iyr,itechp2,icafe19,ifuel) = TECHPENYR_P2(iyr-1,itechp2,icafe19,ifuel) + 1.0
                      tech_once_P2(itechp2,icafe19,ifuel) = .true.
                    endif							
				    if(TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel).lt.TECHBASE_P2(itechp2,icafe19,ifuel)) then	  	
                      P = 1.0/(1.0 + exp(-1.0*(TECHPENYR_P2(iyr,itechp2,icafe19,ifuel) - TECHMID_P2(itechp2,isc19))/TECHSHAPE_P2(itechp2,isc19)))
                      P = TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel) + ((TECHMAX_P2(itechp2,isc19) - TECHBASE_P2(itechp2,icafe19,ifuel))*P)
                      if(P.gt.TECHMAX_P2(itechp2,isc19)) P = TECHMAX_P2(itechp2,isc19)
                      if(P.lt.0.0) P = 0.0
                      TEMP_P2 = PREFF_P2(itechp2,icafe19,ifuel)*P
                      if(TEMP_P2.lt.0.0) TEMP_P2 = 0.0
                      if(TEMP_P2.gt.TECHMAX_P2(itechp2,isc19)) TEMP_P2 = TECHMAX_P2(itechp2,isc19)
                      TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = TEMP_P2
					else
                      P = 1.0/(1.0 + exp(-1.0*(TECHPENYR_P2(iyr,itechp2,icafe19,ifuel) - TECHMID_P2(itechp2,isc19))/TECHSHAPE_P2(itechp2,isc19)))
                      P = TECHBASE_P2(itechp2,icafe19,ifuel) + ((TECHMAX_P2(itechp2,isc19) - TECHBASE_P2(itechp2,icafe19,ifuel))*P)
                      if(P.gt.TECHMAX_P2(itechp2,isc19)) P = TECHMAX_P2(itechp2,isc19)
                      if(P.lt.0.0) P = 0.0
                      TEMP_P2 = PREFF_P2(itechp2,icafe19,ifuel)*P
                      if(TEMP_P2.lt.0.0) TEMP_P2 = 0.0
                      if(TEMP_P2.gt.TECHMAX_P2(itechp2,isc19)) TEMP_P2 = TECHMAX_P2(itechp2,isc19)
                      TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = TEMP_P2						  
					endif
                  else
 				    if(TECHPENYR_P2(iyr-1,itechp2,icafe19,ifuel).gt.0.) TECHPENYR_P2(iyr,itechp2,icafe19,ifuel) = TECHPENYR_P2(iyr-1,itechp2,icafe19,ifuel) - 1.0		 
                        TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel)*PREFF_P2(itechp2,icafe19,ifuel)
 			      endif
                endif
              endif				
			enddo    ! End itechp2 loop
			
!...    Apply supersedes engineering notes.
            do itechp2=1,techp2
		      MAX_SHARE_S(itechp2) = TECHMAX_P2(itechp2,isc19)				
              if(curcalyr.lt.TECHFYR_P2(itechp2,isc19,ifuel)) cycle
!	          return_stat = 0
              do inote = 1,num_sup
                if(SUPERSEDES_tech(1,inote).eq.itechp2) then					
!...    Set initial market share and market share maximum.
                  TOT_MKT   = TECHSHR_P2(iyr,itechp2,icafe19,ifuel) 
                  MAX_SHARE = TECHMAX_P2(itechp2,isc19)							
!...    Find maximum allowable tech chain penetration.
				  do i = 2,tech_cnt_sup(inote)
					MAX_SHARE = max(max_share,MAX_SHARE_S(supersedes_tech(i,inote)))					  
                  enddo						
!...    Find and adjust any EXCESS penetration downward.
                  do i = 2,tech_cnt_sup(inote)					
                    TOT_MKT = TOT_MKT + TECHSHR_P2(iyr,SUPERSEDES_tech(i,inote),icafe19,ifuel)						  
                    if(TOT_MKT.gt.MAX_SHARE) then
                      TECHSHR_P2(iyr,SUPERSEDES_tech(i,inote),icafe19,ifuel) = TECHSHR_P2(iyr,SUPERSEDES_tech(i,inote),icafe19,ifuel) - (TOT_MKT - MAX_SHARE)
                      TOT_MKT = MAX_SHARE 	 						  						  
                    endif								
                  enddo  ! end tech_cnt loop						
                endif
			  enddo   ! end num_sup loop
            enddo   ! end itechp2 loop for supersedes notes			

!...    Apply required engineering notes
            do itechp2 = 1,techp2
              if(curcalyr.lt.TECHFYR_P2(itechp2,isc19,ifuel)) cycle
              REQUIRED = .FALSE.
              REQ_MKT  = 0.0
              do inote = 1,num_req
                if(REQUIRES_tech(1,inote).eq.itechp2) then					
                   REQUIRED = .TRUE.
				   REQ_MKT  = REQ_MKT + TECHSHR_P2(iyr,requires_tech(2,inote),icafe19,ifuel)				  
                 endif
              enddo
              if(REQUIRED) then
                REQ_MKT = min(REQ_MKT,1.0)
				TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = min(TECHSHR_P2(iyr,itechp2,icafe19,ifuel),REQ_MKT)
			  endif			
			enddo      

!...    Apply required engineering notes when the required tech could be one of a few
            TECHSHR_P2(iyr,68,icafe19,ifuel) = min(TECHSHR_P2(iyr,68,icafe19,ifuel), & 
			       (TECHSHR_P2(iyr,62,icafe19,ifuel) + TECHSHR_P2(iyr,66,icafe19,ifuel) + TECHSHR_P2(iyr,67,icafe19,ifuel)))
            TECHSHR_P2(iyr,69,icafe19,ifuel) = min(TECHSHR_P2(iyr,69,icafe19,ifuel), & 
			       (TECHSHR_P2(iyr,62,icafe19,ifuel) + TECHSHR_P2(iyr,66,icafe19,ifuel) + TECHSHR_P2(iyr,67,icafe19,ifuel)))
            TECHSHR_P2(iyr,71,icafe19,ifuel) = min(TECHSHR_P2(iyr,71,icafe19,ifuel), & 
			       (TECHSHR_P2(iyr,62,icafe19,ifuel) + TECHSHR_P2(iyr,66,icafe19,ifuel) + TECHSHR_P2(iyr,67,icafe19,ifuel)))
			
!...   Loop through and apply the synergy engineering notes
            do itechp2 = 1,techp2        
              if(curcalyr.lt.TECHFYR_P2(itechp2,isc19,ifuel)) cycle
              SYNERGY_LOSS(itechp2) = 0.0

!...Market share affected by synergy effects between two technologies is estimated as the probabilistic overlap
!...between the market shares of the two technologies. Mathematically, this market share is expressed as the 
!...product of the market shares of the two technologies.  The incremental market share overlap for a single year
!...is equal to the cumulative estimated overlap (based on cumulative estimated market penetrations) for the 
!...current year minus the cumulative estimated overlap for the previous year.  Note also, that the input value
!...of SYNR_DEL is negative so that the estimated synergy loss will also be negative and should be treated as an
!...additive parameter.
              do inote = 1,num_syn
                if(synergy_tech(1,inote).eq.itechp2) then  
				  do i = 2,tech_cnt_syn(inote)
					delta_mkt = (TECHSHR_P2(iyr,itechp2,icafe19,ifuel)*TECHSHR_P2(iyr,synergy_tech(i,inote),icafe19,ifuel)) - &
			              (TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel)*TECHSHR_P2(iyr-1,synergy_tech(i,inote),icafe19,ifuel)) 
                    SYNERGY_LOSS(itechp2) = SYNERGY_LOSS(itechp2) + (DELTA_MKT*syn_per(i,inote))
		          enddo
                endif
              enddo				
!... Flex fuel technology improvements should be the same as gasoline technology improvements
              if(ifuel.eq.5) TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = TECHSHR_P2(iyr,itechp2,icafe19,2)
!... PHEV aero, tire, and weight reduction should be the same as diesel and gasoline, respectively
              if(ifuel.eq.7.and.itechp2.le.10) TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = TECHSHR_P2(iyr,itechp2,icafe19,1)
              if(ifuel.eq.8.and.itechp2.le.10) TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = TECHSHR_P2(iyr,itechp2,icafe19,2)
            enddo               ! end itechp2 loop for synergy notes			
!...      determine combined MPG improvement of fuel-saving technologies by weighting each technologies improvement by market share
            MPGEFF_19(ifuel,icafe19) = 1.0
            do itechp2 = 1,techp2
              if(curcalyr.ge.techfyr_P2(itechp2,isc19,ifuel))then
                if(techshr_P2(iyr,itechp2,icafe19,ifuel).gt.0..and.techshr_p2(iyr,itechp2,icafe19,ifuel).ge.techbase_p2(itechp2,icafe19,ifuel)) then 
                  techadjshr_P2(iyr,itechp2,icafe19,ifuel) = techshr_P2(iyr,itechp2,icafe19,ifuel) - TECHBASE_P2(itechp2,icafe19,ifuel)
                  techadjshr_P2(iyr,itechp2,icafe19,ifuel) = max(techadjshr_P2(iyr,itechp2,icafe19,ifuel),-1.0*TECHBASE_P2(itechp2,icafe19,ifuel))
                else
                  techadjshr_P2(iyr,itechp2,icafe19,ifuel) = 0.
                endif
				MPGEFF_19(ifuel,icafe19) = MPGEFF_19(ifuel,icafe19)*(1.0 - ((techeff_19(itechp2,icafe19,ifuel) + & 
				    synergy_loss(itechp2))*TECHadjSHR_P2(iyr,itechp2,icafe19,ifuel)))
				MPGEFF_19(ifuel,icafe19) = min(mpgeff_19(ifuel,icafe19),1.0)
              endif
			enddo

!... techshare debug
!    if(ifuel.eq.1.and.icafe19.eq.18) then
!    write(21,*)'tech share - diesel'
!    write(21,'(10a8)') 'itechp2', '9', '10', '11', '13','14','15','16','17','18'	  
!    do itechp2=8,techp2
!	  write(21,'(i8,9f8.4)')itechp2,techshr_p2(iyr,itechp2,9,1),techshr_p2(iyr,itechp2,10,1),techshr_p2(iyr,itechp2,11,1),techshr_p2(iyr,itechp2,13,1), &
!			        techshr_p2(iyr,itechp2,14,1),techshr_p2(iyr,itechp2,15,1),techshr_p2(iyr,itechp2,16,1),techshr_p2(iyr,itechp2,17,1),techshr_p2(iyr,itechp2,18,1)
!	enddo
!    write(21,*)'tech adj share - diesel'
!    write(21,'(10a8)') 'itechp2', '9', '10', '11', '13','14','15','16','17','18'	  
!    do itechp2=8,techp2
!	  write(21,'(i8,9f8.4)')itechp2,techadjshr_p2(iyr,itechp2,9,1),techadjshr_p2(iyr,itechp2,10,1),techadjshr_p2(iyr,itechp2,11,1),techadjshr_p2(iyr,itechp2,13,1), &
!			        techadjshr_p2(iyr,itechp2,14,1),techadjshr_p2(iyr,itechp2,15,1),techadjshr_p2(iyr,itechp2,16,1),techadjshr_p2(iyr,itechp2,17,1), & 
!                   techadjshr_p2(iyr,itechp2,18,1)
!	enddo
!	endif		
!...    determine new fuel economy for the 19 cafe size classes 
            NEW_MPG_19(iyr,ifuel,icafe19) = BASE_MPG_p2(ifuel,icafe19)/MPGEFF_19(ifuel,icafe19)		  
!...    determine new fuel economy for the 14 actual cafe size classes
			! Class 2b-3 pickup and van
		    new_mpg_p2(iyr,ifuel,1) = (tstk_per_p2(iyr-1,1,ifuel)/NEW_MPG_19(iyr,ifuel,1) + tstk_per_p2(iyr-1,3,ifuel)/NEW_MPG_19(iyr,ifuel,3))**-1.0  
            ! Class 2b-5 vocational
            new_mpg_p2(iyr,ifuel,2) = (tstk_per_p2(iyr-1,2,ifuel)/NEW_MPG_19(iyr,ifuel,2) + tstk_per_p2(iyr-1,4,ifuel)/NEW_MPG_19(iyr,ifuel,4) + &
	                               tstk_per_p2(iyr-1,5,ifuel)/NEW_MPG_19(iyr,ifuel,5) + tstk_per_p2(iyr-1,6,ifuel)/NEW_MPG_19(iyr,ifuel,6))**-1.0   
		    ! Class 6-7 vocational						   
            new_mpg_p2(iyr,ifuel,3) = (tstk_per_p2(iyr-1,7,ifuel)/NEW_MPG_19(iyr,ifuel,7) + tstk_per_p2(iyr-1,8,ifuel)/NEW_MPG_19(iyr,ifuel,8))**-1.0               
		    new_mpg_p2(iyr,ifuel,4) = NEW_MPG_19(iyr,ifuel,12)                                                                                ! Class 8 vocational
		    new_mpg_p2(iyr,ifuel,5) = NEW_MPG_19(iyr,ifuel,9)                                                                                 ! Class 7 day low
		    new_mpg_p2(iyr,ifuel,6) = NEW_MPG_19(iyr,ifuel,10)                                                                                ! Class 7 day mid
		    new_mpg_p2(iyr,ifuel,7) = NEW_MPG_19(iyr,ifuel,11)                                                                                ! Class 7 day high
		    new_mpg_p2(iyr,ifuel,8) = NEW_MPG_19(iyr,ifuel,13)                                                                                ! Class 8 day low
		    new_mpg_p2(iyr,ifuel,9) = NEW_MPG_19(iyr,ifuel,14)                                                                                ! Class 8 day mid
		    new_mpg_p2(iyr,ifuel,10) = NEW_MPG_19(iyr,ifuel,15)                                                                               ! Class 8 day high
		    new_mpg_p2(iyr,ifuel,11) = NEW_MPG_19(iyr,ifuel,16)                                                                               ! Class 8 sleeper low
		    new_mpg_p2(iyr,ifuel,12) = NEW_MPG_19(iyr,ifuel,17)                                                                               ! Class 8 sleeper mid
		    new_mpg_p2(iyr,ifuel,13) = NEW_MPG_19(iyr,ifuel,18)                                                                               ! Class 8 sleeper high
		    new_mpg_p2(iyr,ifuel,14) = NEW_MPG_19(iyr,ifuel,19)                                                                               ! Class 8 heavy haul	  
          enddo    ! end cafe19 loop		
		  
!... Converting GGE fuels to DGE
	      do icafep2 = 1,3
		    if(ifuel.ge.2.and.ifuel.le.5.or.ifuel.eq.8) new_mpg_p2(iyr,ifuel,icafep2) = new_mpg_p2(iyr,ifuel,icafep2)*hrate(1,1)/hrate(2,1)
		  enddo
		  do icafep2 = 4,cafep2
		    if(ifuel.ge.2.and.ifuel.le.3.or.ifuel.eq.5.or.ifuel.eq.8) new_mpg_p2(iyr,ifuel,icafep2) = new_mpg_p2(iyr,ifuel,icafep2)*hrate(1,1)/hrate(2,1)
		  enddo
	
!... Finding the size class average mpg to check if the standard is passed
          mpg_calc(iyr,ifuel,:) = 0.0
          if(new_mpg_p2(iyr,ifuel,1).gt.0.0) mpg_calc(iyr,ifuel,1) = &
		    ((TRKSTK_19(iyr-1,1,1,ifuel) + TRKSTK_19(iyr-1,3,1,ifuel))/(sum(TRKSTK_19(iyr-1,1,1,1:9)) + sum(TRKSTK_19(iyr-1,3,1,1:9))))/new_mpg_p2(iyr,ifuel,1)
          if(new_mpg_p2(iyr,ifuel,2).gt.0.0) mpg_calc(iyr,ifuel,2) = &
		    ((TRKSTK_19(iyr-1,2,1,ifuel) + sum(TRKSTK_19(iyr-1,4:6,1,ifuel)))/(sum(TRKSTK_19(iyr-1,2,1,1:9)) + sum(TRKSTK_19(iyr-1,4:6,1,1:9))))/new_mpg_p2(iyr,ifuel,2)
		  if(new_mpg_p2(iyr,ifuel,3).gt.0.0) mpg_calc(iyr,ifuel,3) = (sum(TRKSTK_19(iyr-1,7:8,1,ifuel))/sum(TRKSTK_19(iyr-1,7:8,1,1:9)))/new_mpg_p2(iyr,ifuel,3)
		  if(new_mpg_p2(iyr,ifuel,4).gt.0.0) mpg_calc(iyr,ifuel,4) = (TRKSTK_19(iyr-1,12,1,ifuel)/sum(TRKSTK_19(iyr-1,12,1,1:9)))/new_mpg_p2(iyr,ifuel,4)
          if(new_mpg_p2(iyr,ifuel,5).gt.0.0) mpg_calc(iyr,ifuel,5) = (TRKSTK_19(iyr-1,9,1,ifuel)/sum(TRKSTK_19(iyr-1,9,1,1:9)))/new_mpg_p2(iyr,ifuel,5)
          if(new_mpg_p2(iyr,ifuel,6).gt.0.0) mpg_calc(iyr,ifuel,6) = (TRKSTK_19(iyr-1,10,1,ifuel)/sum(TRKSTK_19(iyr-1,10,1,1:9)))/new_mpg_p2(iyr,ifuel,6)
          if(new_mpg_p2(iyr,ifuel,7).gt.0.0) mpg_calc(iyr,ifuel,7) = (TRKSTK_19(iyr-1,11,1,ifuel)/sum(TRKSTK_19(iyr-1,11,1,1:9)))/new_mpg_p2(iyr,ifuel,7)
          if(new_mpg_p2(iyr,ifuel,8).gt.0.0) mpg_calc(iyr,ifuel,8) = (TRKSTK_19(iyr-1,13,1,ifuel)/sum(TRKSTK_19(iyr-1,13,1,1:9)))/new_mpg_p2(iyr,ifuel,8)
          if(new_mpg_p2(iyr,ifuel,9).gt.0.0) mpg_calc(iyr,ifuel,9) = (TRKSTK_19(iyr-1,14,1,ifuel)/sum(TRKSTK_19(iyr-1,14,1,1:9)))/new_mpg_p2(iyr,ifuel,9)
          if(new_mpg_p2(iyr,ifuel,10).gt.0.0) mpg_calc(iyr,ifuel,10) = (TRKSTK_19(iyr-1,15,1,ifuel)/sum(TRKSTK_19(iyr-1,15,1,1:9)))/new_mpg_p2(iyr,ifuel,10)
          if(new_mpg_p2(iyr,ifuel,11).gt.0.0) mpg_calc(iyr,ifuel,11) = (TRKSTK_19(iyr-1,16,1,ifuel)/sum(TRKSTK_19(iyr-1,16,1,1:9)))/new_mpg_p2(iyr,ifuel,11)
          if(new_mpg_p2(iyr,ifuel,12).gt.0.0) mpg_calc(iyr,ifuel,12) = (TRKSTK_19(iyr-1,17,1,ifuel)/sum(TRKSTK_19(iyr-1,17,1,1:9)))/new_mpg_p2(iyr,ifuel,12)
          if(new_mpg_p2(iyr,ifuel,13).gt.0.0) mpg_calc(iyr,ifuel,13) = (TRKSTK_19(iyr-1,18,1,ifuel)/sum(TRKSTK_19(iyr-1,18,1,1:9)))/new_mpg_p2(iyr,ifuel,13)
          if(new_mpg_p2(iyr,ifuel,14).gt.0.0) mpg_calc(iyr,ifuel,14) = (TRKSTK_19(iyr-1,19,1,ifuel)/sum(TRKSTK_19(iyr-1,19,1,1:9)))/new_mpg_p2(iyr,ifuel,14)
          do icafep2 = 1,cafep2				
            mpg_p2(iyr,icafep2) = (sum(mpg_calc(iyr,1:9,icafep2)))**-1.0
		  enddo						

!... Calculate credit weighted fuel economy
          mpg_calc(iyr,ifuel,:) = 0.0
          if(new_mpg_p2(iyr,ifuel,1).gt.0.0) mpg_calc(iyr,ifuel,1) = &
		    ((credsales(iyr-1,1,ifuel) + credsales(iyr-1,3,ifuel))/(sum(credsales(iyr-1,1,1:9)) + sum(credsales(iyr-1,3,1:9))))/new_mpg_p2(iyr,ifuel,1)
          if(new_mpg_p2(iyr,ifuel,2).gt.0.0) mpg_calc(iyr,ifuel,2) = &
		    ((credsales(iyr-1,2,ifuel) + sum(credsales(iyr-1,4:6,ifuel)))/(sum(credsales(iyr-1,2,1:9)) + sum(credsales(iyr-1,4:6,1:9))))/new_mpg_p2(iyr,ifuel,2)
		  if(new_mpg_p2(iyr,ifuel,3).gt.0.0) mpg_calc(iyr,ifuel,3) = (sum(credsales(iyr-1,7:8,ifuel))/sum(credsales(iyr-1,7:8,1:9)))/new_mpg_p2(iyr,ifuel,3)
		  if(new_mpg_p2(iyr,ifuel,4).gt.0.0) mpg_calc(iyr,ifuel,4) = (credsales(iyr-1,12,ifuel)/sum(credsales(iyr-1,12,1:9)))/new_mpg_p2(iyr,ifuel,4)
          if(new_mpg_p2(iyr,ifuel,5).gt.0.0) mpg_calc(iyr,ifuel,5) = (credsales(iyr-1,9,ifuel)/sum(credsales(iyr-1,9,1:9)))/new_mpg_p2(iyr,ifuel,5)
          if(new_mpg_p2(iyr,ifuel,6).gt.0.0) mpg_calc(iyr,ifuel,6) = (credsales(iyr-1,10,ifuel)/sum(credsales(iyr-1,10,1:9)))/new_mpg_p2(iyr,ifuel,6)
          if(new_mpg_p2(iyr,ifuel,7).gt.0.0) mpg_calc(iyr,ifuel,7) = (credsales(iyr-1,11,ifuel)/sum(credsales(iyr-1,11,1:9)))/new_mpg_p2(iyr,ifuel,7)
          if(new_mpg_p2(iyr,ifuel,8).gt.0.0) mpg_calc(iyr,ifuel,8) = (credsales(iyr-1,13,ifuel)/sum(credsales(iyr-1,13,1:9)))/new_mpg_p2(iyr,ifuel,8)
          if(new_mpg_p2(iyr,ifuel,9).gt.0.0) mpg_calc(iyr,ifuel,9) = (credsales(iyr-1,14,ifuel)/sum(credsales(iyr-1,14,1:9)))/new_mpg_p2(iyr,ifuel,9)
          if(new_mpg_p2(iyr,ifuel,10).gt.0.0) mpg_calc(iyr,ifuel,10) = (credsales(iyr-1,15,ifuel)/sum(credsales(iyr-1,15,1:9)))/new_mpg_p2(iyr,ifuel,10)
          if(new_mpg_p2(iyr,ifuel,11).gt.0.0) mpg_calc(iyr,ifuel,11) = (credsales(iyr-1,16,ifuel)/sum(credsales(iyr-1,16,1:9)))/new_mpg_p2(iyr,ifuel,11)
          if(new_mpg_p2(iyr,ifuel,12).gt.0.0) mpg_calc(iyr,ifuel,12) = (credsales(iyr-1,17,ifuel)/sum(credsales(iyr-1,17,1:9)))/new_mpg_p2(iyr,ifuel,12)
          if(new_mpg_p2(iyr,ifuel,13).gt.0.0) mpg_calc(iyr,ifuel,13) = (credsales(iyr-1,18,ifuel)/sum(credsales(iyr-1,18,1:9)))/new_mpg_p2(iyr,ifuel,13)
          if(new_mpg_p2(iyr,ifuel,14).gt.0.0) mpg_calc(iyr,ifuel,14) = (credsales(iyr-1,19,ifuel)/sum(credsales(iyr-1,19,1:9)))/new_mpg_p2(iyr,ifuel,14)

!... Converting GGE fuels to DGE
	      do icafep2 = 1,3
			if(ifuel.ge.2.and.ifuel.le.5.or.ifuel.eq.8) then 
			  HDV_STANDARD_dge(iyr,ifuel,icafep2) = HDV_STANDARD_p2(ifuel,iyr,icafep2)*hrate(1,1)/hrate(2,1)
			else
			  HDV_STANDARD_dge(iyr,ifuel,icafep2) = HDV_STANDARD_p2(ifuel,iyr,icafep2)
			endif
		  enddo
		  do icafep2 = 4,cafep2
			if(ifuel.ge.2.and.ifuel.le.3.or.ifuel.eq.5.or.ifuel.eq.8) then 
			  HDV_STANDARD_dge(iyr,ifuel,icafep2) = HDV_STANDARD_p2(ifuel,iyr,icafep2)*hrate(1,1)/hrate(2,1)				  
			else
			  HDV_STANDARD_dge(iyr,ifuel,icafep2) = HDV_STANDARD_p2(ifuel,iyr,icafep2)
			endif
		  enddo

!... Determining the standard that must be acheived
          stndr_calc(iyr,ifuel,:) = 0.0
          stndr_calc(iyr,ifuel,1) = &
			((TRKSTK_19(iyr-1,1,1,ifuel) + TRKSTK_19(iyr-1,3,1,ifuel))/(sum(TRKSTK_19(iyr-1,1,1,1:9)) + sum(TRKSTK_19(iyr-1,3,1,1:9))))/HDV_STANDARD_dge(iyr,ifuel,1)
          stndr_calc(iyr,ifuel,2) = &
			((TRKSTK_19(iyr-1,2,1,ifuel) + sum(TRKSTK_19(iyr-1,4:6,1,ifuel)))/(sum(TRKSTK_19(iyr-1,2,1,1:9)) + sum(TRKSTK_19(iyr-1,4:6,1,1:9))))/HDV_STANDARD_dge(iyr,ifuel,2)
		  stndr_calc(iyr,ifuel,3) = (sum(TRKSTK_19(iyr-1,7:8,1,ifuel))/sum(TRKSTK_19(iyr-1,7:8,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,3)
		  stndr_calc(iyr,ifuel,4) = (TRKSTK_19(iyr-1,12,1,ifuel)/sum(TRKSTK_19(iyr-1,12,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,4)
          stndr_calc(iyr,ifuel,5) = (TRKSTK_19(iyr-1,9,1,ifuel)/sum(TRKSTK_19(iyr-1,9,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,5)
          stndr_calc(iyr,ifuel,6) = (TRKSTK_19(iyr-1,10,1,ifuel)/sum(TRKSTK_19(iyr-1,10,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,6)
          stndr_calc(iyr,ifuel,7) = (TRKSTK_19(iyr-1,11,1,ifuel)/sum(TRKSTK_19(iyr-1,11,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,7)
          stndr_calc(iyr,ifuel,8) = (TRKSTK_19(iyr-1,13,1,ifuel)/sum(TRKSTK_19(iyr-1,13,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,8)
          stndr_calc(iyr,ifuel,9) = (TRKSTK_19(iyr-1,14,1,ifuel)/sum(TRKSTK_19(iyr-1,14,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,9)
          stndr_calc(iyr,ifuel,10) = (TRKSTK_19(iyr-1,15,1,ifuel)/sum(TRKSTK_19(iyr-1,15,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,10)
          stndr_calc(iyr,ifuel,11) = (TRKSTK_19(iyr-1,16,1,ifuel)/sum(TRKSTK_19(iyr-1,16,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,11)
          stndr_calc(iyr,ifuel,12) = (TRKSTK_19(iyr-1,17,1,ifuel)/sum(TRKSTK_19(iyr-1,17,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,12)
          stndr_calc(iyr,ifuel,13) = (TRKSTK_19(iyr-1,18,1,ifuel)/sum(TRKSTK_19(iyr-1,18,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,13)
          stndr_calc(iyr,ifuel,14) = (TRKSTK_19(iyr-1,19,1,ifuel)/sum(TRKSTK_19(iyr-1,19,1,1:9)))/HDV_STANDARD_dge(iyr,ifuel,14)
		enddo      ! end fuel loop	  

!...    does MPG for cafe size class meet HDV fuel consumption and greenhouse gas emissions standards?
        do icafep2 = 1,cafep2
          mpg_p2_cred(iyr,icafep2) = (sum(mpg_calc(iyr,1:9,icafep2)))**-1.0
		  HDV_STANDARD_C(iyr,icafep2) = (sum(stndr_calc(iyr,1:9,icafep2)))**-1.0
          if(mpg_p2_cred(iyr,icafep2).ge.HDV_STANDARD_C(iyr,icafep2)) pass_standards_P2(icafep2) = .true.		  
        enddo			
		  
	    do icafep2=1,cafep2	  
!...    On first pass and first iteration, establish the starting values for this year from from last year's
!...    working values and from last year's new bank.
          if(j.eq.1.and.curitr.eq.1) then
            P2Bank(5,icafep2) = P2Work(4,icafep2)
            P2Bank(4,icafep2) = P2Work(3,icafep2)
            P2Bank(3,icafep2) = P2Work(2,icafep2)
            P2Bank(2,icafep2) = P2Work(1,icafep2)
            P2Bank(1,icafep2) = P2BankA(icafep2)
            P2BankA(icafep2)  = 0.0
!            write(21,'(a,3i5,5f8.3)') '**BnkInt ',curiyr+1989,curitr,icafep2,P2Bank(1,icafep2),P2Bank(2,icafep2),P2Bank(3,icafep2),P2Bank(4,icafep2),P2Bank(5,icafep2)
          endif
!...    On first pass and every iteration, put the saved values into working values. This is necessary
!...    so that we have the fresh bank values at the start of each new iteration.
          if(j.eq.1) then
            P2Work(5,icafep2) = P2Bank(5,icafep2)
            P2Work(4,icafep2) = P2Bank(4,icafep2)
            P2Work(3,icafep2) = P2Bank(3,icafep2)
            P2Work(2,icafep2) = P2Bank(2,icafep2)
            P2Work(1,icafep2) = P2Bank(1,icafep2)
!...    On the first pass, if the group passed, then bank its excess MPG. Otherwise, pull values out of the bank.
            if(pass_standards_P2(icafep2).eq. .true.) then
              P2BankA(icafep2) = mpg_p2_cred(iyr,icafep2)-hdv_standard_c(iyr,icafep2)
!              write(21,'(a,3i5,f8.3,l4)') '**BnkPas ',curiyr+1989,curitr,icafep2,P2BankA(icafep2), pass_standards_P2(icafep2)
!			  write(21,*)mpg_p2_cred(iyr,icafep2),hdv_standard_c(iyr,icafep2)
            else
!...    Get the total amount by which the group did not pass the cafe standard.
              P2Need = hdv_standard_c(iyr,icafep2)-mpg_p2_cred(iyr,icafep2)
              P2NeedX(iyr,icafep2) = P2Need
			
!...    Work backwards through the bank and see if we can make up the difference.
              do i=5,1,-1
                if(P2Need.gt.0.0) then
                  if(P2Need.le.P2Work(i,icafep2)) then
                    P2Work(i,icafep2) = P2Work(i,icafep2)-P2Need
                    P2Need = 0.0
                    pass_standards_P2(icafep2) = .true.
                  else
                    P2Need = P2Need-P2Work(i,icafep2)
                    P2Work(i,icafep2) = 0.0
                  endif
                endif
              enddo
		      p2bankbal(iyr,icafep2)=sum(P2Bank(1:5,icafep2))
			  mpg_adj_p2(icafep2)=p2bankbal(iyr,icafep2)-p2bankbal(iyr-1,icafep2)
              if(P2Need.eq.0.0)then
                pass_standards_P2(icafep2)=.true.
                mpg_p2_cred(iyr,icafep2) = mpg_p2_cred(iyr,icafep2)+ mpg_adj_p2(icafep2)
              endif
!              write(21,'(a,3i5,7f8.3,l)') '**BnkFal ',curiyr+1989,curitr,icafep2,P2NeedX(iyr,icafep2),p2Work(1,icafep2),p2Work(2,icafep2),p2Work(3,icafep2),&
!                                                        p2Work(4,icafep2),p2Work(5,icafep2),p2Need,pass_standards_P2(icafep2)
!			  write(21,*)' mpg after banking=',mpg_p2_cred(iyr,icafep2)
            endif
          endif
  	    enddo
      enddo        ! end j loop	
	  
!	  write(21,*)'NEW HDV MPG'
!      write(21,'(10a8)') 'icafe', 'DIESE', 'GAS', 'LPG', 'CNG','FFV','EV','PHEVd','PHEVg','FCV'
! 	  do icafep2=1,cafep2
!		write(21,'(i8,9f8.4)')icafep2,new_mpg_p2(iyr,1,icafep2),new_mpg_p2(iyr,2,icafep2),new_mpg_p2(iyr,3,icafep2),new_mpg_p2(iyr,4,icafep2),&
!		            new_mpg_p2(iyr,5,icafep2),new_mpg_p2(iyr,6,icafep2),new_mpg_p2(iyr,7,icafep2),new_mpg_p2(iyr,8,icafep2),new_mpg_p2(iyr,9,icafep2)
!	  enddo 
!... debug sales_share - credsales
!	  do ifuel=1,fuel9
!      sales_share(1,ifuel) = ((credsales(iyr-1,1,ifuel) + credsales(iyr-1,3,ifuel))/(sum(credsales(iyr-1,1,1:9))+sum(credsales(iyr-1,3,1:9)))) 
!      sales_share(2,ifuel) = ((credsales(iyr-1,2,ifuel) + sum(credsales(iyr-1,4:6,ifuel)))/(sum(credsales(iyr-1,2,1:9)) + sum(credsales(iyr-1,4:6,1:9))))
!	  sales_share(3,ifuel) = (sum(credsales(iyr-1,7:8,ifuel))/sum(credsales(iyr-1,7:8,1:9)))
!	  sales_share(4,ifuel) = (credsales(iyr-1,12,ifuel)/sum(credsales(iyr-1,12,1:9)))
!      sales_share(5,ifuel) = (credsales(iyr-1,9,ifuel)/sum(credsales(iyr-1,9,1:9)))
!      sales_share(6,fuel) = (credsales(iyr-1,10,ifuel)/sum(credsales(iyr-1,10,1:9)))
!      sales_share(7,ifuel) = (credsales(iyr-1,11,ifuel)/sum(credsales(iyr-1,11,1:9)))
!      sales_share(8,ifuel) = (credsales(iyr-1,13,ifuel)/sum(credsales(iyr-1,13,1:9)))
!      sales_share(9,ifuel) = (credsales(iyr-1,14,ifuel)/sum(credsales(iyr-1,14,1:9)))
!      sales_share(10,ifuel) = (credsales(iyr-1,15,ifuel)/sum(credsales(iyr-1,15,1:9)))
!      sales_share(11,ifuel) = (credsales(iyr-1,16,ifuel)/sum(credsales(iyr-1,16,1:9)))
!      sales_share(12,ifuel) = (credsales(iyr-1,17,ifuel)/sum(credsales(iyr-1,17,1:9)))
!      sales_share(13,ifuel) = (credsales(iyr-1,18,ifuel)/sum(credsales(iyr-1,18,1:9)))		
!      sales_share(14,ifuel) = (credsales(iyr-1,19,ifuel)/sum(credsales(iyr-1,19,1:9)))
!	  enddo
	  
!	  write(21,*)'Sales Share - credsales'
!      write(21,'(10a8)') 'icafe', 'DIESE', 'GAS', 'LPG', 'CNG','FFV','EV','PHEVd','PHEVg','FCV'	  
!  	  do icafep2=1,cafep2
!		write(21,'(i8,9f8.4)')icafep2,sales_share(icafep2,1),sales_share(icafep2,2),sales_share(icafep2,3),sales_share(icafep2,4),&
!		            sales_share(icafep2,5),sales_share(icafep2,6),sales_share(icafep2,7),sales_share(icafep2,8),sales_share(icafep2,9)
!	  enddo 
!... debug sales_share - trkstk	  
!	do ifuel=1,fuel9  
!	sales_share(1,ifuel) = ((TRKSTK_19(iyr-1,1,1,ifuel) + TRKSTK_19(iyr-1,3,1,ifuel))/(sum(TRKSTK_19(iyr-1,1,1,1:9)) + sum(TRKSTK_19(iyr-1,3,1,1:9))))
!    sales_share(2,ifuel) = ((TRKSTK_19(iyr-1,2,1,ifuel) + sum(TRKSTK_19(iyr-1,4:6,1,ifuel)))/(sum(TRKSTK_19(iyr-1,2,1,1:9)) + sum(TRKSTK_19(iyr-1,4:6,1,1:9))))
!	sales_share(3,ifuel) = (sum(TRKSTK_19(iyr-1,7:8,1,ifuel))/sum(TRKSTK_19(iyr-1,7:8,1,1:9)))
!	sales_share(4,ifuel) = (TRKSTK_19(iyr-1,12,1,ifuel)/sum(TRKSTK_19(iyr-1,12,1,1:9)))
!    sales_share(5,ifuel) = (TRKSTK_19(iyr-1,9,1,ifuel)/sum(TRKSTK_19(iyr-1,9,1,1:9)))
!    sales_share(6,ifuel) = (TRKSTK_19(iyr-1,10,1,ifuel)/sum(TRKSTK_19(iyr-1,10,1,1:9)))
!    sales_share(7,ifuel) = (TRKSTK_19(iyr-1,11,1,ifuel)/sum(TRKSTK_19(iyr-1,11,1,1:9)))
!    sales_share(8,ifuel) = (TRKSTK_19(iyr-1,13,1,ifuel)/sum(TRKSTK_19(iyr-1,13,1,1:9)))
!    sales_share(9,ifuel) = (TRKSTK_19(iyr-1,14,1,ifuel)/sum(TRKSTK_19(iyr-1,14,1,1:9)))
!    sales_share(10,ifuel) = (TRKSTK_19(iyr-1,15,1,ifuel)/sum(TRKSTK_19(iyr-1,15,1,1:9)))
!    sales_share(11,ifuel) = (TRKSTK_19(iyr-1,16,1,ifuel)/sum(TRKSTK_19(iyr-1,16,1,1:9)))
!    sales_share(12,ifuel) = (TRKSTK_19(iyr-1,17,1,ifuel)/sum(TRKSTK_19(iyr-1,17,1,1:9)))
!    sales_share(13,ifuel) = (TRKSTK_19(iyr-1,18,1,ifuel)/sum(TRKSTK_19(iyr-1,18,1,1:9)))
!    sales_share(14,ifuel) = (TRKSTK_19(iyr-1,19,1,ifuel)/sum(TRKSTK_19(iyr-1,19,1,1:9))) 
!	enddo  
!	  write(21,*)'Sales Share - trkstock'
!      write(21,'(10a8)') 'icafe', 'DIESE', 'GAS', 'LPG', 'CNG','FFV','EV','PHEVd','PHEVg','FCV'	  
!  	  do icafep2=1,cafep2
!		write(21,'(i8,9f8.4)')icafep2,sales_share(icafep2,1),sales_share(icafep2,2),sales_share(icafep2,3),sales_share(icafep2,4),&
!		            sales_share(icafep2,5),sales_share(icafep2,6),sales_share(icafep2,7),sales_share(icafep2,8),sales_share(icafep2,9)
!	  enddo 	
  
!...reaggregate 14 Phase 2 CAFE size class fuel economies to NEMS reported 3 size classes - VMT weighted
    do ifuel=1,fuel9					
	    HDV_MPG(iyr,4,1,ifuel) = (tstk_per_sc(iyr-1,1,ifuel)/new_mpg_19(iyr,ifuel,1) + tstk_per_sc(iyr-1,2,ifuel)/new_mpg_19(iyr,ifuel,2))**-1.0
	    HDV_MPG(iyr,1,1,ifuel) = (tstk_per_sc(iyr-1,3,ifuel)/new_mpg_19(iyr,ifuel,3) + tstk_per_sc(iyr-1,4,ifuel)/new_mpg_19(iyr,ifuel,4))**-1.0
	    HDV_MPG(iyr,2,1,ifuel) = (tstk_per_sc(iyr-1,5,ifuel)/new_mpg_19(iyr,ifuel,5) + tstk_per_sc(iyr-1,6,ifuel)/new_mpg_19(iyr,ifuel,6) + &
		    tstk_per_sc(iyr-1,7,ifuel)/new_mpg_19(iyr,ifuel,7))**-1.0
	    HDV_MPG(iyr,3,1,ifuel) = (tstk_per_sc(iyr-1,8,ifuel)/new_mpg_19(iyr,ifuel,8) + &
	        tstk_per_sc(iyr-1,9,ifuel)/new_mpg_19(iyr,ifuel,9) + tstk_per_sc(iyr-1,10,ifuel)/new_mpg_19(iyr,ifuel,10) + &
		    tstk_per_sc(iyr-1,11,ifuel)/new_mpg_19(iyr,ifuel,11) + tstk_per_sc(iyr-1,12,ifuel)/new_mpg_19(iyr,ifuel,12) + &
		    tstk_per_sc(iyr-1,13,ifuel)/new_mpg_19(iyr,ifuel,13) + tstk_per_sc(iyr-1,14,ifuel)/new_mpg_19(iyr,ifuel,14) + &
		    tstk_per_sc(iyr-1,15,ifuel)/new_mpg_19(iyr,ifuel,15) + tstk_per_sc(iyr-1,16,ifuel)/new_mpg_19(iyr,ifuel,16) + &
		    tstk_per_sc(iyr-1,17,ifuel)/new_mpg_19(iyr,ifuel,17) + tstk_per_sc(iyr-1,18,ifuel)/new_mpg_19(iyr,ifuel,18) + &
		    tstk_per_sc(iyr-1,19,ifuel)/new_mpg_19(iyr,ifuel,19))**-1.0
    enddo
	
	if (n.eq.34.and.curitr.eq.(maxitr+1)) then
	  write(21,*)'Diesel HDV_MPG tstk_per_sc new_mpg_19 MDRAEO2023'
	  do i = 27,34	 ! 2016 - 2023
	    do j = 12,18   ! Class 8 vocational and all tractors
	      write(21,'(i4,",",i2,",",3(f8.4,","))')i+1989, j, HDV_MPG(i,3,1,1), tstk_per_sc(i-1,j,1), new_mpg_19(i,1,j)
		enddo
	  enddo
	endif

!... debug HDV_MPG diesel
!    write(21,*)'HDV_MPG - diesel'
!    write(21,'(10a8)') 'icafe19', '1', '2', '3', '4','5','6','7','8','9'	  
!    do icafe19=8,cafe19
!	  write(21,'(i8,9f8.4)')icafe19,tstk_per_sc(iyr-1,icafe19,1),tstk_per_sc(iyr-1,icafe19,2),tstk_per_sc(iyr-1,icafe19,3),tstk_per_sc(iyr-1,icafe19,4),&
!			               tstk_per_sc(iyr-1,icafe19,5),tstk_per_sc(iyr-1,icafe19,6),tstk_per_sc(iyr-1,icafe19,7),tstk_per_sc(iyr-1,icafe19,8),tstk_per_sc(iyr-1,icafe19,9)
!	enddo
!... debug HDV_MPG diesel
!    write(21,*)'HDV_MPG - diesel'
!    write(21,'(10a8)') 'icafe19', '1', '2', '3', '4','5','6','7','8','9'	  
!    do icafe19=8,cafe19
!	  write(21,'(i8,9f8.4)')icafe19,new_mpg_19(iyr,1,icafe19),new_mpg_19(iyr,2,icafe19),new_mpg_19(iyr,2,icafe19),new_mpg_19(iyr,4,icafe19),&
!	               new_mpg_19(iyr,5,icafe19),new_mpg_19(iyr,6,icafe19),new_mpg_19(iyr,7,icafe19),new_mpg_19(iyr,8,icafe19),new_mpg_19(iyr,9,icafe19)
!	enddo

!...reaggregate 19 Phase 2 size class technology shares into 14 Phase 2 class sizes
    do itechp2=1,techp2
      do ifuel=1,fuel9
!...    Can be used for report writer sum tech shares - same aggregation level as Phase 1 with the addition of heavy haul and class 2b
!...    class 1: class 2b pickup and van and vocational
!...    class 2: class 3 pickup and van and vocational
!...    class 3: class 4-6 vocational
!...    class 4: class 7-8 vocational
!...    class 5: class 7-8 day cab
!...    class 6: class 7-8 sleeper cab
!...    class 7: class 8 heavy haul
	    TECHSHARE_P2(iyr,itechp2,1,ifuel) = techshr_P2(iyr,itechp2,1,ifuel)*tstk_per_sc(iyr-1,1,ifuel) + techshr_P2(iyr,itechp2,2,ifuel)*tstk_per_sc(iyr-1,2,ifuel)	
        TECHSHARE_P2(iyr,itechp2,2,ifuel) = techshr_P2(iyr,itechp2,3,ifuel)*tstk_per_sc(iyr-1,3,ifuel) + techshr_P2(iyr,itechp2,4,ifuel)*tstk_per_sc(iyr-1,4,ifuel)
        TECHSHARE_P2(iyr,itechp2,3,ifuel) = techshr_P2(iyr,itechp2,5,ifuel)*tstk_per_sc(iyr-1,5,ifuel) + techshr_P2(iyr,itechp2,6,ifuel)*tstk_per_sc(iyr-1,6,ifuel) + &
			  techshr_P2(iyr,itechp2,7,ifuel)*tstk_per_sc(iyr-1,7,ifuel)
        TECHSHARE_P2(iyr,itechp2,4,ifuel) = techshr_P2(iyr,itechp2,8,ifuel)*(TRKSTK_19(iyr-1,8,1,ifuel)/(TRKSTK_19(iyr-1,8,1,ifuel) + TRKSTK_19(iyr-1,12,1,ifuel))) + &
			  techshr_P2(iyr,itechp2,12,ifuel)*(TRKSTK_19(iyr-1,12,1,ifuel)/(TRKSTK_19(iyr-1,8,1,ifuel) + TRKSTK_19(iyr-1,12,1,ifuel)))							
        TECHSHARE_P2(iyr,itechp2,5,ifuel) = techshr_P2(iyr,itechp2,9,ifuel)*(TRKSTK_19(iyr-1,9,1,ifuel)/ &
		      (sum(TRKSTK_19(iyr-1,9:11,1,ifuel)) + sum(TRKSTK_19(iyr-1,13:15,1,ifuel)))) + &
		      techshr_P2(iyr,itechp2,10,ifuel)*(TRKSTK_19(iyr-1,10,1,ifuel)/(sum(TRKSTK_19(iyr-1,9:11,1,ifuel)) + sum(TRKSTK_19(iyr-1,13:15,1,ifuel)))) + &
		      techshr_P2(iyr,itechp2,11,ifuel)*(TRKSTK_19(iyr-1,11,1,ifuel)/(sum(TRKSTK_19(iyr-1,9:11,1,ifuel)) + sum(TRKSTK_19(iyr-1,13:15,1,ifuel)))) + &
		      techshr_P2(iyr,itechp2,13,ifuel)*(TRKSTK_19(iyr-1,13,1,ifuel)/(sum(TRKSTK_19(iyr-1,9:11,1,ifuel)) + sum(TRKSTK_19(iyr-1,13:15,1,ifuel)))) + &
		      techshr_P2(iyr,itechp2,14,ifuel)*(TRKSTK_19(iyr-1,14,1,ifuel)/(sum(TRKSTK_19(iyr-1,9:11,1,ifuel)) + sum(TRKSTK_19(iyr-1,13:15,1,ifuel)))) + &
		      techshr_P2(iyr,itechp2,15,ifuel)*(TRKSTK_19(iyr-1,15,1,ifuel)/(sum(TRKSTK_19(iyr-1,9:11,1,ifuel)) + sum(TRKSTK_19(iyr-1,13:15,1,ifuel))))			  
        TECHSHARE_P2(iyr,itechp2,6,ifuel) = techshr_P2(iyr,itechp2,16,ifuel)*(TRKSTK_19(iyr-1,16,1,ifuel)/sum(TRKSTK_19(iyr-1,16:18,1,ifuel))) + &
		      techshr_P2(iyr,itechp2,17,ifuel)*(TRKSTK_19(iyr-1,17,1,ifuel)/sum(TRKSTK_19(iyr-1,16:18,1,ifuel))) + &
		      techshr_P2(iyr,itechp2,18,ifuel)*(TRKSTK_19(iyr-1,18,1,ifuel)/sum(TRKSTK_19(iyr-1,16:18,1,ifuel)))		
		TECHSHARE_P2(iyr,itechp2,7,ifuel) = techshr_P2(iyr,itechp2,19,ifuel)	
	  enddo  ! end fuel loop
!...for report writer sum tech shares for diesel and gasoline
          do ifuel=1,2
            TECHSHARE(iyr,itechp2,1,ifuel) = TECHSHARE_P2(iyr,itechp2,2,ifuel)
            TECHSHARE(iyr,itechp2,2,ifuel) = TECHSHARE_P2(iyr,itechp2,3,ifuel)
            TECHSHARE(iyr,itechp2,3,ifuel) = TECHSHARE_P2(iyr,itechp2,4,ifuel)
            TECHSHARE(iyr,itechp2,4,ifuel) = TECHSHARE_P2(iyr,itechp2,5,ifuel)
            TECHSHARE(iyr,itechp2,5,ifuel) = TECHSHARE_P2(iyr,itechp2,6,ifuel)
          enddo
      enddo ! end itechp2 loop
	  
!... Incremental cost of technology adoption  
!... by Phase 2 19 size classes
    do ifuel = 1,fuel9
      do icafe19 = 1,cafe19   
        inc_tech_cost_19(iyr,icafe19,ifuel) = sum(techshr_P2(iyr,1:70,icafe19,ifuel)*TECHCOSTyr_p2(iyr,1:70,icafe19))
      enddo

!... by Phase 2 14 size classes
	  inc_tech_cost_p2(iyr,1,ifuel) = inc_tech_cost_19(iyr,1,ifuel)*tstk_per_p2(iyr-1,1,ifuel) + inc_tech_cost_19(iyr,3,ifuel)*tstk_per_p2(iyr-1,3,ifuel)
      inc_tech_cost_p2(iyr,2,ifuel) = inc_tech_cost_19(iyr,2,ifuel)*tstk_per_p2(iyr-1,3,ifuel) + inc_tech_cost_19(iyr,4,ifuel)*tstk_per_p2(iyr-1,4,ifuel) + &
         inc_tech_cost_19(iyr,5,ifuel)*tstk_per_p2(iyr-1,5,ifuel) + inc_tech_cost_19(iyr,6,ifuel)*tstk_per_p2(iyr-1,6,ifuel)
      inc_tech_cost_p2(iyr,3,ifuel) = inc_tech_cost_19(iyr,7,ifuel)*tstk_per_p2(iyr-1,7,ifuel) + inc_tech_cost_19(iyr,8,ifuel)*tstk_per_p2(iyr-1,8,ifuel)	
	  inc_tech_cost_p2(iyr,4,ifuel) = inc_tech_cost_19(iyr,12,ifuel)
      inc_tech_cost_p2(iyr,5,ifuel) = inc_tech_cost_19(iyr,9,ifuel)
      inc_tech_cost_p2(iyr,6,ifuel) = inc_tech_cost_19(iyr,10,ifuel)
      inc_tech_cost_p2(iyr,7,ifuel) = inc_tech_cost_19(iyr,11,ifuel)
      inc_tech_cost_p2(iyr,8,ifuel) = inc_tech_cost_19(iyr,13,ifuel)
      inc_tech_cost_p2(iyr,9,ifuel) = inc_tech_cost_19(iyr,14,ifuel)
      inc_tech_cost_p2(iyr,10,ifuel) = inc_tech_cost_19(iyr,15,ifuel)
      inc_tech_cost_p2(iyr,11,ifuel) = inc_tech_cost_19(iyr,16,ifuel)
      inc_tech_cost_p2(iyr,12,ifuel) = inc_tech_cost_19(iyr,17,ifuel)
      inc_tech_cost_p2(iyr,13,ifuel) = inc_tech_cost_19(iyr,18,ifuel)
      inc_tech_cost_p2(iyr,14,ifuel) = inc_tech_cost_19(iyr,19,ifuel)	
	
!... by 3 (now 4) original size classes
	  inc_tech_cost(iyr,ifuel,4) = inc_tech_cost_19(iyr,1,ifuel)*tstk_per_sc(iyr-1,1,ifuel) + inc_tech_cost_19(iyr,2,ifuel)*tstk_per_sc(iyr-1,2,ifuel)
	  inc_tech_cost(iyr,ifuel,1) = inc_tech_cost_19(iyr,3,ifuel)*tstk_per_sc(iyr-1,3,ifuel) + inc_tech_cost_19(iyr,4,ifuel)*tstk_per_sc(iyr-1,4,ifuel)
      inc_tech_cost(iyr,ifuel,2) = inc_tech_cost_19(iyr,5,ifuel)*tstk_per_sc(iyr-1,5,ifuel) + inc_tech_cost_19(iyr,6,ifuel)*tstk_per_sc(iyr-1,6,ifuel) + &
		inc_tech_cost_19(iyr,7,ifuel)*tstk_per_sc(iyr-1,7,ifuel)
	  inc_tech_cost(iyr,ifuel,3) = inc_tech_cost_19(iyr,8,ifuel)*tstk_per_sc(iyr-1,8,ifuel) + inc_tech_cost_19(iyr,9,ifuel)*tstk_per_sc(iyr-1,8,ifuel) + &
		inc_tech_cost_19(iyr,10,ifuel)*tstk_per_sc(iyr-1,10,ifuel) + inc_tech_cost_19(iyr,11,ifuel)*tstk_per_sc(iyr-1,11,ifuel) + &
		inc_tech_cost_19(iyr,12,ifuel)*tstk_per_sc(iyr-1,12,ifuel) + inc_tech_cost_19(iyr,13,ifuel)*tstk_per_sc(iyr-1,13,ifuel) + &
		inc_tech_cost_19(iyr,14,ifuel)*tstk_per_sc(iyr-1,14,ifuel) + inc_tech_cost_19(iyr,15,ifuel)*tstk_per_sc(iyr-1,15,ifuel) + &
		inc_tech_cost_19(iyr,16,ifuel)*tstk_per_sc(iyr-1,16,ifuel) + inc_tech_cost_19(iyr,17,ifuel)*tstk_per_sc(iyr-1,17,ifuel) + &
		inc_tech_cost_19(iyr,18,ifuel)*tstk_per_sc(iyr-1,18,ifuel)
	enddo  ! end fuel loop

    write(21,*) '=========================================================='	
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, NEW_MPG_19(iyr,:,1), 'Class 2 puv'
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, NEW_MPG_19(iyr,:,3), 'Class 3 puv'	
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,1), 'Class 2b-3'
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,1), 'Standard'
	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(1)	
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,1,:)
    write(21,*) '=========================================================='	
	write(21,'(i4,2x,9f10.4,2x,a13)') iyr, NEW_MPG_19(iyr,:,2), 'Class 2 voc'
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, NEW_MPG_19(iyr,:,4), 'Class 3 voc'
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, NEW_MPG_19(iyr,:,5), 'Class 4 voc'
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, NEW_MPG_19(iyr,:,6), 'Class 5 voc'	
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,2), 'Class 2b-5'
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,2), 'Standard'
	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(2)
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,2,:)	
    write(21,*) '=========================================================='	
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, NEW_MPG_19(iyr,:,7), 'Class 6 voc'
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, NEW_MPG_19(iyr,:,8), 'Class 7 voc'	
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,3), 'Class 6-7'
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,3), 'Standard'
	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(3)
	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,3,:)	
	write(21,*) '=========================================================='
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,4), 'Class 8 voc'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,4), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(4)	
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,4,:)	
!	write(21,*) '=========================================================='
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,5), '7-day low'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,5), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(5)	
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,5,:)	
!	write(21,*) '=========================================================='
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,6), '7-day mid'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,6), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(6)	
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,6,:)	
!	write(21,*) '=========================================================='
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,7), '7-day high'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,7), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(7)	
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,7,:)	
!	write(21,*) '=========================================================='
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,8), '8-day low'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,8), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(8)	
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,8,:)	
!	write(21,*) '=========================================================='
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,9), '8-day mid'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,9), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(9)	
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,9,:)	
!	write(21,*) '=========================================================='
!    write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,10), '8-day high'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,10), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(10)	
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,10,:)	
!	write(21,*) '=========================================================='
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,11), '8-sleep low'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,11), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(11)	
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,11,:)	
!    write(21,*) '=========================================================='
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,12), '8-sleep mid'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,12), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(12)	
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,12,:)	
!	write(21,*) '=========================================================='
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,13), '8-sleep high'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,13), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(13)	
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,13,:)	
!	write(21,*) '=========================================================='
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, new_mpg_p2(iyr,:,14), 'Heavy-haul'
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, HDV_STANDARD_P2(:,iyr,14), 'Standard'
!	write(21,'(i4,2x,4l)') iyr, pass_standards_p2(14)		
!	write(21,'(i4,2x,9f10.4,2x,a12)') iyr, inc_tech_cost_p2(iyr,14,:)	

    do isc=1,sc4
      inc_tech_$(iyr,4,isc) = inc_tech_cost(iyr,1,isc)-inc_tech_cost(iyr,4,isc)
	  inc_tech_$(iyr,6,isc) = inc_tech_cost(iyr,1,isc)-inc_tech_cost(iyr,6,isc)    ! [smn]
    enddo
!... debug techshare_p2	
!    write(21,*)'Tech Share - diesel'
!    write(21,'(5a8)') 'itechp2', '7-8 voc', '7-8 day', '7-8 slp', '7-8 hvy'	  
!    do itechp2=1,techp2
!	  do ifuel=1,fuel9
!	    if(ifuel.eq.1) &
!		  write(21,'(i8,4f8.4)')itechp2,techshare_p2(iyr,itechp2,4,ifuel),techshare_p2(iyr,itechp2,5,ifuel),techshare_p2(iyr,itechp2,6,ifuel),&
!			                       techshare_p2(iyr,itechp2,7,ifuel)
!	  enddo
!	enddo
	
  endif
  
 !...reaggregate class 2b fuel economies to NEMS reported aggregation level NOTE: Have to switch gasoline and diesel 
!... New vehicles - Vintage 1
    cltmpg_yr(iyr,1,1) = ((VMTFLT_19(iyr-1,1,1,2)/sum(VMTFLT_19(iyr-1,1:2,1,2)))/NEW_MPG_19(iyr,2,1) + &
	         (VMTFLT_19(iyr-1,2,1,2)/sum(VMTFLT_19(iyr-1,1:2,1,2)))/NEW_MPG_19(iyr,2,2))**-1.0               ! gasoline
	cltmpg_yr(iyr,2,1) = ((VMTFLT_19(iyr-1,1,1,1)/sum(VMTFLT_19(iyr-1,1:2,1,1)))/NEW_MPG_19(iyr,1,1) + &
	         (VMTFLT_19(iyr-1,2,1,1)/sum(VMTFLT_19(iyr-1,1:2,1,1)))/NEW_MPG_19(iyr,1,2))**-1.0               ! diesel
    do ifuel = 3,fuel9
        if(sum(VMTFLT_19(iyr-1,1:2,1,ifuel)).gt.0.0) then 
	      cltmpg_yr(iyr,ifuel,1) = ((VMTFLT_19(iyr-1,1,1,ifuel)/sum(VMTFLT_19(iyr-1,1:2,1,ifuel)))/NEW_MPG_19(iyr,ifuel,1) + &
	         (VMTFLT_19(iyr-1,2,1,ifuel)/sum(VMTFLT_19(iyr-1,1:2,1,ifuel)))/NEW_MPG_19(iyr,ifuel,2))**-1.0 	
        else
          cltmpg_yr(iyr,ifuel,1) = (base_mkt_p1(ifuel,1)/NEW_MPG_19(iyr,ifuel,1) + base_mkt_p1(ifuel,2)/NEW_MPG_19(iyr,ifuel,2))**-1.0		
		endif
    enddo			 
!... Vintages 2 - 33
    do iage = 2,33      
	    do ifuel = 1,fuel9
		  cltmpg_yr(iyr,ifuel,iage) = cltmpg_yr(iyr-1,ifuel,iage-1)
        enddo
    enddo
!... Vintage 34+
	if(sum(VMTFLT_19(iyr-1,1:2,33:34,2)).gt.0.0) then
	  cltmpg_yr(iyr,1,34) = 1.0/(((sum(VMTFLT_19(iyr-1,1:2,33,2))/cltmpg_yr(iyr-1,1,34)) + &
        (sum(VMTFLT_19(iyr-1,1:2,34,2))/cltmpg_yr(iyr-1,1,33)))/(sum(VMTFLT_19(iyr-1,1:2,33:34,2))))
    else
	  cltmpg_yr(iyr,1,34) = cltmpg_yr(iyr-1,1,33)
	endif
	if(sum(VMTFLT_19(iyr-1,1:2,33:34,1)).gt.0.0) then
	  cltmpg_yr(iyr,2,34) = 1.0/(((sum(VMTFLT_19(iyr-1,1:2,33,1))/cltmpg_yr(iyr-1,2,34)) + &
       (sum(VMTFLT_19(iyr-1,1:2,34,1))/cltmpg_yr(iyr-1,2,33)))/sum(VMTFLT_19(iyr-1,1:2,33:34,1)))
	else
	  cltmpg_yr(iyr,2,34) = cltmpg_yr(iyr-1,2,33)
	endif
    do ifuel = 3,fuel9
	    if(sum(VMTFLT_19(iyr-1,1:2,33:34,ifuel)).gt.0.0) then
		    if(cltmpg_yr(iyr-1,ifuel,34).gt.0.0.or.cltmpg_yr(iyr-1,ifuel,33).gt.0.0) then
		      cltmpg_yr(iyr,ifuel,34) = 1.0/(((sum(VMTFLT_19(iyr-1,1:2,33,ifuel))/cltmpg_yr(iyr-1,ifuel,34)) + &
                (sum(VMTFLT_19(iyr-1,1:2,34,ifuel))/cltmpg_yr(iyr-1,ifuel,33)))/sum(VMTFLT_19(iyr-1,1:2,33:34,ifuel)))
			elseif(cltmpg_yr(iyr-1,ifuel,33).gt.0.0) then
			  cltmpg_yr(iyr,ifuel,34) = cltmpg_yr(iyr-1,ifuel,33)
			elseif(cltmpg_yr(iyr-1,ifuel,34).gt.0.0) then
			  cltmpg_yr(iyr,ifuel,34) = cltmpg_yr(iyr-1,ifuel,34)
			endif
	    else
	      cltmpg_yr(iyr,ifuel,34) = cltmpg_yr(iyr-1,ifuel,33)
		endif
	enddo	
 
!--------------------------------------------------------------------------------------------------------------------------------------------!	
!... End Phase 2	
!--------------------------------------------------------------------------------------------------------------------------------------------!	


 RETURN
 END SUBROUTINE TRUCK_NEW

!=============================================================================================================
 SUBROUTINE TRUCK_STOCK
 USE F_
 IMPLICIT NONE
 include 'angtdm'

    INTEGER JFUEL,IVMT, Y, US, ify

    REAL FCOST_regn(FUEL9,MNUMCR)                    ! fuel cost of driving a truck (using ifuel) in $ per mile 
    REAL FSHR_T                                      ! running total of fuel shares used to calculate fuel 2 and 1
    REAL DCOST                                       ! cost of diesel per mile relative to other fuels
    REAL MIDYR                                       ! logistic penetration curve parameter
    REAL MPATH_regn(CAFE19,FUEL9,FLT,MNUMYR,MNUMCR)  ! baseline market penetration trend for each fuel
    REAL SLOPE                                       ! logistic penetration curve parameter
    REAL TEMP
    REAL PBACK_SHR(4),                 &             ! distribution of payback periods by mfg
         INC_COST_CNG(NVMT,SC4),       &             ! incremental cost of cng vehicle (17000,40000,60000)
         ENG_Cost(SC4),                &             ! CNG engine/system cost
         Storage_$(SC4,MNUMYR),        &             ! Fuel storage cost ($/DGE or $/GGE)
         Tank_Size_3(NVMT),            &             ! Class 3 tank size (GGE)
         Tank_Size_48(NVMT),           &             ! Class 4-8 tank size (DGE)
         ANN_$_SAVINGS_CNG_regn(NVMT,FLT,CAFE19,MNUMCR), &! annual dollar savings by vmt group, fleet, size class, and region
         NPV_ADS_regn(NVMT,FLT,cafe19,4,MNUMCR), &   ! net present value of fuel dollar savings
         BUY_CNG_regn(nvmt,flt,cafe19,4,MNUMCR), &   ! BUY CNG vehicle = 1, not buy CNG vehicle = 0
         max_buy_regn(nvmt,flt,cafe19,MNUMCR)
    REAL INC_COST_BEV(NVMT,SC4),       &                   ! incremental cost of BEV vehicle [smn]
         ENG_Cost_BEV(SC4),                &               ! Battery EV engine/system cost [smn]
         Storage_$_BEV(SC4,MNUMYR),        &               ! Battery storage cost ($/kWh) [smn]
         Battery_size_2b3(NVMT),           &               ! Class 3 tank size (kWh) [smn]
         Battery_size_46(NVMT),            &               ! Class 4-6 Battery size (kWh) [smn]
         Battery_size_78(NVMT),            &               ! Class 7-8 Battery size (kWh) [smn]
         ANN_$_SAVINGS_BEV_regn(NVMT,FLT,CAFE19,MNUMCR), & ! annual dollar savings by vmt group, fleet, size class, and region [smn]
         BUY_BEV_regn(nvmt,flt,cafe19,4,MNUMCR), &         ! BUY BEV vehicle = 1, not buy BEV vehicle = 0 [smn]
         pack_frt_a(SC4), &                                !  battery Initial Cost (2020$) [smn]
         pack_frt_b, &                                     !  based on learning rate [smn]
         pack_frt_lr(SC4), &                               !  Battery Pack learning rates [smn]
         mat_frt_a(SC4), &                                 !  materials initial cost (2020$) [smn]
         mat_frt_b, &                                      !  based on learning rate [smn]
         mat_frt_lr(SC4), &                                !  materials learning rates [smn]
         new_gwh, &                                        !  annual GWh of battery capacity additions [smn]  
         battery_size_2b3_avg(mnumyr,flt,cafe19,mnumcr), &         !  Class 2b-3 market-share weighted average battery capacity [smn]
         battery_size_46_avg(mnumyr,flt,cafe19,mnumcr), &          !  Class 4-6 market-share weighted average battery capacity [smn]
         battery_size_78_avg(mnumyr,flt,cafe19,mnumcr)             !  Class 7-8 market-share weighted average battery capacity [smn]
         
    REAL TEMP_PRICE                                  ! temporary price variable for calculating natural gas price
	REAL share_fuel(cafe19,fuel9,flt,mnumcr)            ! Previous years fuel share 
    REAL temp_fuel_shr(cafe19,fuel9,flt,mnumcr)      ! Temporary holder of fuel shares
	REAL TEMPCLS12A(MNUMYR)
	REAL sc_share(cafe19,flt,mnumcr),credit

    US=11

    DATA Tank_Size_3/24.5, 24.5, 24.5, 31.1, 42.5, 55.0, 62.5, 73.8, 83.8, 93.8, 106.3/
    DATA Tank_Size_48/32.0, 32.0, 45.0, 55.0, 77.0, 90.0, 110.0, 110.0, 110.0, 110.0, 110.0/

	! Battery Sizing Table based on VMT [smn]
!              +-----------+-----------+-----------+
!              |         Battery Size (kWh)	       |
! +------------+-----------+-----------+-----------+
! | Annual VMT | Class 2-3 | Class 4-6 | Class 7-8 |
! +------------+-----------+-----------+-----------+
! |      12554 |      41.3 |      75.3 |     213.3 |
! |      27855 |      55.5 |     148.9 |     260.5 |
! |      46021 |      87.0 |     159.2 |     377.5 |
! |      62276 |      89.0 |     210.0 |     482.0 |
! |      85000 |           |           |     900.0 |
! +------------+-----------+-----------+-----------+
!
! for all null cells, adding 99999.0 to serve the purpose of making these too expensive to consider [smn]
	DATA Battery_size_2b3/41.3, 55.5, 87.0, 89.0, 99999.0, 99999.0, 99999.0, 99999.0, 99999.0, 99999.0, 99999.0/
	DATA Battery_size_46/75.3, 148.9, 159.2, 210.0, 99999.0, 99999.0, 99999.0, 99999.0, 99999.0, 99999.0, 99999.0/
	DATA Battery_size_78/213.3, 260.5, 377.5, 482.0 , 900.0 ,99999.0, 99999.0, 99999.0, 99999.0, 99999.0, 99999.0/

! Freight battery cost learning rate coefficients by size class (3, 4-6, 7-8, 2b) [smn]
	DATA pack_frt_a/1307.57, 1307.57, 1307.57, 1307.57 / ! initial cost 2020$ 
	DATA pack_frt_LR/0.165, 0.165, 0.165, 0.165/ ! battery learning rate
	DATA mat_frt_a/51.96, 51.96, 51.96, 51.96 / ! Initial Cost 2020$
	DATA mat_frt_LR/0.035,0.035,0.035,0.035/  ! materials learning rate

!... 2 year payback
!    PBACK_SHR(1)=0.164
!    PBACK_SHR(2)=0.617
!    PBACK_SHR(3)=0.155
!    PBACK_SHR(4)=0.064


!...Payback distribution to achieve 3 year average
    if(curcalyr.ge.2013)then
      PBACK_SHR(1)=0.0466
      PBACK_SHR(2)=0.1474
      PBACK_SHR(3)=0.5659
      PBACK_SHR(4)=0.2401
    endif

!...Calculate incremental natural gas vehicle price
    do ivmt=1,nvmt
      if(curcalyr.eq.2010)then
        ENG_Cost(1) = 1420.0/mc_jpgdp(20) * mc_jpgdp(1)
        ENG_Cost(2) = 19750.0/mc_jpgdp(20) * mc_jpgdp(1)
        ENG_Cost(3) = 33875.0/mc_jpgdp(20) * mc_jpgdp(1)
        ENG_Cost(4) = 1420.0/mc_jpgdp(20) * mc_jpgdp(1)
      endif
!...  fuel storage cost ($/DGE or $/GGE)
      if(curcalyr.le.2014)then
        Storage_$(1,iyr) = 341.0/mc_jpgdp(20) * mc_jpgdp(1)
        Storage_$(2,iyr) = 450.0/mc_jpgdp(20) * mc_jpgdp(1)
        Storage_$(3,iyr) = 475.0/mc_jpgdp(20) * mc_jpgdp(1)
        Storage_$(4,iyr) = 341.0/mc_jpgdp(20) * mc_jpgdp(1)
      endif
!...  implement cost reduction for natural gas storage system
      do isc=1,sc4
        if(curcalyr.ge.2015.and.curcalyr.le.2019) Storage_$(isc,iyr) = Storage_$(isc,iyr-1)*0.97
        if(curcalyr.ge.2020.and.curcalyr.le.2024) Storage_$(isc,iyr) = Storage_$(isc,iyr-1)*0.98
        if(curcalyr.ge.2025) Storage_$(isc,iyr) = Storage_$(isc,iyr-1)*0.99
        if(isc.eq.1.or.isc.eq.4)then
          INC_COST_CNG(ivmt,isc)=((ENG_Cost(isc)+(Storage_$(isc,iyr)*Tank_size_3(ivmt))))-inc_tech_$(iyr,4,isc)
        else
          INC_COST_CNG(ivmt,isc)=((ENG_Cost(isc)+(Storage_$(isc,iyr)*Tank_size_48(ivmt))))-inc_tech_$(iyr,4,isc)
        endif
      enddo
    enddo

!...Calculate incremental battery electric vehicle (BEV)  price [smn]
    do ivmt=1,nvmt
      if(curcalyr.eq.2010)then
        ENG_Cost_BEV(1) = 0
        ENG_Cost_BEV(2) = 0
        ENG_Cost_BEV(3) = 0
        ENG_Cost_BEV(4) = 0
      endif

!...  Battery price per kWh. Initial costs are in 2020 dollars, converted to 1990 dollars here [smn]
	!... Battery Pack Cost learning rate equation:
	!     Cost = a_battery*(cumulative production)^(-b_battery) + a_material*(cumulative production)^(-b_material)
	!     where b = -Log(1.0-(learning rate))/Log(2.0) 
      do isc=1,sc4
		if(curcalyr.le.BSYR_STK)then			! MDRAEO2022 -- was 2018
			Storage_$_BEV(isc,iyr) = (pack_frt_a(isc) + mat_frt_a(isc)) / mc_jpgdp(31) * mc_jpgdp(1)
		else
			pack_frt_b = -Log(1.0-(pack_frt_lr(isc)))/Log(2.0)
			mat_frt_b = -Log(1.0-(mat_frt_lr(isc)))/Log(2.0)
			Storage_$_BEV(isc,iyr) = (pack_frt_a(isc)*cumulative_gwh(iyr-1)**(-pack_frt_b) + mat_frt_a(isc)*cumulative_gwh(iyr-1)**(-mat_frt_b)) / mc_jpgdp(31) * mc_jpgdp(1)
		endif
		
!...    Calcualte Incremental BEV cost increase over conventional diesel [smn]
        if(isc.eq.1.or.isc.eq.4)then
          INC_COST_BEV(ivmt,isc)=((ENG_Cost_BEV(isc)+(Storage_$_BEV(isc,iyr)*Battery_size_2b3(ivmt))))-inc_tech_$(iyr,6,isc)
        else if(isc.eq.2)then
          INC_COST_BEV(ivmt,isc)=((ENG_Cost_BEV(isc)+(Storage_$_BEV(isc,iyr)*Battery_size_46(ivmt))))-inc_tech_$(iyr,6,isc)
        else
          INC_COST_BEV(ivmt,isc)=((ENG_Cost_BEV(isc)+(Storage_$_BEV(isc,iyr)*Battery_size_78(ivmt))))-inc_tech_$(iyr,6,isc)
        endif 
      enddo
	  
    enddo   

!...Testing
	IF(N.eq.MNUMYR.and.CURITR.eq.MAXITR+1) THEN
	  WRITE(21,*)'HDV Li-ion Cost'
	  DO isc=19,MNUMYR
	    WRITE(21,'(2I4, 2F10.3)') isc+1989, 1, cumulative_gwh(isc-1), Storage_$_BEV(1,isc)
	  ENDDO
	ENDIF

	
!...Process new trucks sales from MACRO by sharing
    NEWTRUCKS_regn(iyr,1,3,11)=  MC_VEHICLES(3,iyr) * 1000000.                  ! Class 3  
    NEWTRUCKS_regn(iyr,2,3,11)=      NEWCLS46(iyr)  * MC_SUVTHAM(iyr)*1000000.0 ! Class 4-6
    NEWTRUCKS_regn(iyr,3,3,11)= (1.0-NEWCLS46(iyr)) * MC_SUVTHAM(iyr)*1000000.0 ! Class 7&8
!...Class 2b breakout new vehicle sales
	TEMPCLS12A(iyr) = (MC_Vehicles(1,iyr) + MC_Vehicles(2,iyr))*LTSplit(iyr)
    NEWTRUCKS_regn(iyr,4,3,11) = (MC_Vehicles(1,iyr) + MC_VEHICLES(2,iyr) - TEMPCLS12A(iyr))*1000000.0

!...share new truck sales between fleet and non fleet using starting distribution 
    do isc=1,sc4
      NEWTRUCKS_regn(iyr,isc,FLT,11) = NEWTRUCKS_regn(iyr,isc,3,11)*FLEETSHR(isc)
      NEWTRUCKS_regn(iyr,isc,NFT,11) = NEWTRUCKS_regn(iyr,isc,3,11)*(1.0 - FLEETSHR(isc))
    enddo

!... Filling in historic fuel economy through bsyr_stk
    if(curcalyr.le.bsyr_stk) then 
      HDV_MPG(iyr,4,1:age,1:6) = BMPGSTK2B(1:age,iyr,1:6)
	  cltmpg_yr(iyr,2,1:age)   = HDV_MPG(iyr,4,1:age,1)					! Diesel 2b
	  cltmpg_yr(iyr,1,1:age)   = HDV_MPG(iyr,4,1:age,2)					! Gasoline 2b
	  do ifuel = 3,6 
	    cltmpg_yr(iyr,ifuel,1:age) = HDV_MPG(iyr,4,1:age,ifuel)      	! all other fuels, 2b
      enddo

      HDV_MPG(iyr,1,1:age,1:6) = BMPGSTK3(1:age,iyr,1:6)
      HDV_MPG(iyr,2,1:age,1:6) = BMPGSTK46(1:age,iyr,1:6)
      HDV_MPG(iyr,3,1:age,1:6) = BMPGSTK78(1:age,iyr,1:6)
	endif


!...Read in historical vehicle stocks by size class, age, and fuel type
    if(curcalyr.le.2011)then
      do isc=1,sc
        do iage=1,age
          do ifuel=1,6
!...        class 3
            if(isc.eq.1)then
              TRKSTK(cur,isc,iage,ifuel,NFT) = CLS3STKHIST(iage,iyr,ifuel)*(1.0-FLEETSHR(isc))
              TRKSTK(cur,isc,iage,ifuel,FLT) = CLS3STKHIST(iage,iyr,ifuel)*FLEETSHR(isc)
!...        class 4-6
            elseif(isc.eq.2)then
              TRKSTK(cur,isc,iage,ifuel,NFT) = CLS46STKHIST(iage,iyr,ifuel)*(1.0-FLEETSHR(isc))
              TRKSTK(cur,isc,iage,ifuel,FLT) = CLS46STKHIST(iage,iyr,ifuel)*FLEETSHR(isc)
!...        class 7&8
            else
              TRKSTK(cur,isc,iage,ifuel,NFT) = CLS78STKHIST(iage,iyr,ifuel)*(1.0-FLEETSHR(isc))
              TRKSTK(cur,isc,iage,ifuel,FLT) = CLS78STKHIST(iage,iyr,ifuel)*FLEETSHR(isc)
            endif
          enddo
        enddo
      enddo

!---------------------------------------------------------------------------------------------------------------------------------------------------!
!... Fill in stock for use in phase 1 subroutine
!---------------------------------------------------------------------------------------------------------------------------------------------------!
      if(curcalyr.ge.2010) then
!... Filling in new sales from class 2b since class 2b is not vintaged until 2012
 		TRKSTK_19(iyr,1,1,1) = CLTSTK(iyr-1,2,1,11)*base_mkt_p1(1,1)                                              ! Class 2b pickup and van - diesel
 		TRKSTK_19(iyr,2,1,1) = CLTSTK(iyr-1,2,1,11)*base_mkt_p1(1,2)                                              ! Class 2b vocational - diesel		 
 		TRKSTK_19(iyr,1,1,2) = CLTSTK(iyr-1,1,1,11)*base_mkt_p1(2,1)                                              ! Class 2b pickup and van - gasoline
 		TRKSTK_19(iyr,2,1,2) = CLTSTK(iyr-1,1,1,11)*base_mkt_p1(2,2)                                              ! Class 2b vocational - gasoline	
        do ifuel = 3,6
 		  TRKSTK_19(iyr,1,1,ifuel) = CLTSTK(iyr-1,ifuel,1,11)*base_mkt_p1(ifuel,1)                                 ! Class 2b pickup and van 
 		  TRKSTK_19(iyr,2,1,ifuel) = CLTSTK(iyr-1,ifuel,1,11)*base_mkt_p1(ifuel,2)                                 ! Class 2b vocational 	
        enddo		 
	  
!... Disagregating stock vehicles so they are by size class (3-8)
!	 After 2011, this breakout is done using Polk input data
	    do iage = 1,age
		  do ifuel = 1,6 
            TRKSTK_19(iyr,3,iage,ifuel) = sum(TRKSTK(cur,1,iage,ifuel,1:2))*base_mkt_p1(ifuel,3)                                       ! Class 3 pickup and van
            TRKSTK_19(iyr,4,iage,ifuel) = sum(TRKSTK(cur,1,iage,ifuel,1:2))*base_mkt_p1(ifuel,4)                                       ! Class 3 vocational	
            TRKSTK_19(iyr,5,iage,ifuel) = sum(TRKSTK(cur,2,iage,ifuel,1:2))*breakout_46(ifuel,1,iage)*base_mkt_p1(ifuel,5)             ! Class 4 vocational	
            TRKSTK_19(iyr,6,iage,ifuel) = sum(TRKSTK(cur,2,iage,ifuel,1:2))*breakout_46(ifuel,2,iage)*base_mkt_p1(ifuel,6)             ! Class 5 vocational	
            TRKSTK_19(iyr,7,iage,ifuel) = sum(TRKSTK(cur,2,iage,ifuel,1:2))*breakout_46(ifuel,3,iage)*base_mkt_p1(ifuel,7)             ! Class 6 vocational	
            TRKSTK_19(iyr,8,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,1,iage)*base_mkt_p1(ifuel,8)             ! Class 7 vocational	
		    TRKSTK_19(iyr,9,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,1,iage)*base_mkt_p1(ifuel,9)             ! Class 7 tractor day low
		    TRKSTK_19(iyr,10,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,1,iage)*base_mkt_p1(ifuel,10)           ! Class 7 tractor day mid
	        TRKSTK_19(iyr,11,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,1,iage)*base_mkt_p1(ifuel,11)           ! Class 7 tractor day high
	        TRKSTK_19(iyr,12,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,2,iage)*base_mkt_p1(ifuel,12)           ! Class 8 vocational
	        TRKSTK_19(iyr,13,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,2,iage)*base_mkt_p1(ifuel,13)           ! Class 8 tractor day low
	        TRKSTK_19(iyr,14,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,2,iage)*base_mkt_p1(ifuel,14)           ! Class 8 tractor day mid
	        TRKSTK_19(iyr,15,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,2,iage)*base_mkt_p1(ifuel,15)           ! Class 8 tractor day high
	        TRKSTK_19(iyr,16,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,2,iage)*base_mkt_p1(ifuel,16)           ! Class 8 tractor sleeper low
	        TRKSTK_19(iyr,17,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,2,iage)*base_mkt_p1(ifuel,17)           ! Class 8 tractor sleeper mid
	        TRKSTK_19(iyr,18,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,2,iage)*base_mkt_p1(ifuel,18)           ! Class 8 tractor sleeper high
	        TRKSTK_19(iyr,19,iage,ifuel) = sum(TRKSTK(cur,3,iage,ifuel,1:2))*breakout_78(ifuel,2,iage)*base_mkt_p1(ifuel,19)           ! Class 8 tractor heavy haul 
		  enddo                                                                                                                       ! (not in phase 1)
	    enddo
	  endif

	  do ifuel = 5,fuel9
	    if(sum(trkstk_19(iyr,1:2,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,4,1,ifuel) = 0.0
	    if(sum(trkstk_19(iyr,3:4,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,1,1,ifuel) = 0.0
	    if(sum(trkstk_19(iyr,5:7,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,2,1,ifuel) = 0.0
	    if(sum(trkstk_19(iyr,8:19,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,3,1,ifuel) = 0.0
	  enddo
	  
!---------------------------------------------------------------------------------------------------------------------------------------------------!
!... Starting in 2012, data is regional

!... Class 7&8 historical tractor stock (trkstock, from Polk)is broken out into low, mid, and high roof using shares from Polk 2012 (base_mkt_p1). 
!... These shares sum to 1 within each vehicle class (e.g., [C8 Vocational] + [C8 Day Low] + [C8 Day Mid] + [C8 Day High] + [C8 Sleeper Low] + [C8 Sleeper Mid] + [C8 Sleeper High] = 100%).
!... Since actual Polk data for each year is used to break out vocational and tractor, base_mkt_p1 is only needed to split out the tractor shares. It is 
!... re-normalized (e.g., [c8_day_lowroof_share = base_mkt_p1[c8_day_low]/(base_mkt_p1[c8_day_low] + base_mkt_p1[c8_day_mid] + base_mkt_p1[c8_day_high])
!... and applied to the tractor share derived from trkstock to disaggregate the tractor stock into low, mid, and high roof day and sleeper cabs.

!... Class 8 heavy haul historical stock is taken directly from Polk (trkstock).
!---------------------------------------------------------------------------------------------------------------------------------------------------!
    elseif(curcalyr.le.bsyr_stk) then
	  Fuel_Shr_regn(iyr,:,:,:,:) = 0.0
      do iregn = 1,mnumcr-2
        do iage=1,age
          do ifuel=1,6
	        TRK_19_regn(iyr,1,iage,ifuel,1,iregn) = trkstock(iyr,2,iage,ifuel,1,iregn)*(1.0-FLEETSHR(4))                                     ! Class 2 pickup and van
	        TRK_19_regn(iyr,1,iage,ifuel,2,iregn) = trkstock(iyr,2,iage,ifuel,1,iregn)*FLEETSHR(4)	
            TRK_19_regn(iyr,2,iage,ifuel,1,iregn) = trkstock(iyr,2,iage,ifuel,2,iregn)*(1.0-FLEETSHR(4))                                     ! Class 2 vocational
		    TRK_19_regn(iyr,2,iage,ifuel,2,iregn) = trkstock(iyr,2,iage,ifuel,2,iregn)*FLEETSHR(4)		
            TRK_19_regn(iyr,3,iage,ifuel,1,iregn) = trkstock(iyr,3,iage,ifuel,1,iregn)*(1.0-FLEETSHR(1))                                     ! Class 3 pickup and van
            TRK_19_regn(iyr,3,iage,ifuel,2,iregn) = trkstock(iyr,3,iage,ifuel,1,iregn)*FLEETSHR(1)  		 		 
            TRK_19_regn(iyr,4,iage,ifuel,1,iregn) = trkstock(iyr,3,iage,ifuel,2,iregn)*(1.0-FLEETSHR(1))                                     ! Class 3 vocational	
		    TRK_19_regn(iyr,4,iage,ifuel,2,iregn) = trkstock(iyr,3,iage,ifuel,2,iregn)*FLEETSHR(1) 
		    TRK_19_regn(iyr,5,iage,ifuel,1,iregn) = trkstock(iyr,4,iage,ifuel,2,iregn)*(1.0-FLEETSHR(2))                                     ! Class 4 vocational
            TRK_19_regn(iyr,5,iage,ifuel,2,iregn) = trkstock(iyr,4,iage,ifuel,2,iregn)*FLEETSHR(2)
            TRK_19_regn(iyr,6,iage,ifuel,1,iregn) = trkstock(iyr,5,iage,ifuel,2,iregn)*(1.0-FLEETSHR(2))                                     ! Class 5 vocational
            TRK_19_regn(iyr,6,iage,ifuel,2,iregn) = trkstock(iyr,5,iage,ifuel,2,iregn)*FLEETSHR(2)	
            TRK_19_regn(iyr,7,iage,ifuel,1,iregn) = trkstock(iyr,6,iage,ifuel,2,iregn)*(1.0-FLEETSHR(2))                                     ! Class 6 vocational	
            TRK_19_regn(iyr,7,iage,ifuel,2,iregn) = trkstock(iyr,6,iage,ifuel,2,iregn)*FLEETSHR(2)	 
            TRK_19_regn(iyr,8,iage,ifuel,1,iregn) = trkstock(iyr,7,iage,ifuel,2,iregn)*(1.0-FLEETSHR(3))                                     ! Class 7 vocational	
            TRK_19_regn(iyr,8,iage,ifuel,2,iregn) = trkstock(iyr,7,iage,ifuel,2,iregn)*FLEETSHR(3)		 		 
		    TRK_19_regn(iyr,9,iage,ifuel,1,iregn) = trkstock(iyr,7,iage,ifuel,1,iregn)*(1.0-FLEETSHR(3)) & 							 		 ! Class 7 tractor day low (non-fleet)
													*base_mkt_p1(ifuel,9)/(sum(base_mkt_p1(ifuel,9:11))+10E-12)      					 	 ! Class 7 Day Tractor split (low/mid/high) from Polk 2012
			TRK_19_regn(iyr,9,iage,ifuel,2,iregn) = trkstock(iyr,7,iage,ifuel,1,iregn)*FLEETSHR(3) & 							 	 		 ! Class 7 tractor day low (fleet)
													*base_mkt_p1(ifuel,9)/(sum(base_mkt_p1(ifuel,9:11))+10E-12)      						 ! Class 7 Day Tractor split (low-roof) from Polk 2012
		    TRK_19_regn(iyr,10,iage,ifuel,1,iregn) = trkstock(iyr,7,iage,ifuel,1,iregn)*(1.0-FLEETSHR(3)) &					         		 ! Class 7 tractor day mid (non-fleet)
													*base_mkt_p1(ifuel,10)/(sum(base_mkt_p1(ifuel,9:11))+10E-12)      						 ! Class 7 Day Tractor split (mid-roof) from Polk 2012
		    TRK_19_regn(iyr,10,iage,ifuel,2,iregn) = trkstock(iyr,7,iage,ifuel,1,iregn)*FLEETSHR(3) &					        	 		 ! Class 7 tractor day mid (fleet)
													*base_mkt_p1(ifuel,10)/(sum(base_mkt_p1(ifuel,9:11))+10E-12)      						 ! Class 7 Day Tractor split (mid-roof) from Polk 2012
	        TRK_19_regn(iyr,11,iage,ifuel,1,iregn) = trkstock(iyr,7,iage,ifuel,1,iregn)*(1.0-FLEETSHR(3)) &					         		 ! Class 7 tractor day high (fleet)
													*base_mkt_p1(ifuel,11)/(sum(base_mkt_p1(ifuel,9:11))+10E-12)       						 ! Class 7 Day Tractor split (high-roof) from Polk 2012
		    TRK_19_regn(iyr,11,iage,ifuel,2,iregn) = trkstock(iyr,7,iage,ifuel,1,iregn)*FLEETSHR(3) &					        	 		 ! Class 7 tractor day high (fleet)
													*base_mkt_p1(ifuel,11)/(sum(base_mkt_p1(ifuel,9:11))+10E-12)    						 ! Class 7 Day Tractor split (high-roof) from Polk 2012
		    TRK_19_regn(iyr,12,iage,ifuel,1,iregn) = trkstock(iyr,8,iage,ifuel,2,iregn)*(1.0-FLEETSHR(3))                                    ! Class 8 vocational
		    TRK_19_regn(iyr,12,iage,ifuel,2,iregn) = trkstock(iyr,8,iage,ifuel,2,iregn)*FLEETSHR(3)	 
	        TRK_19_regn(iyr,13,iage,ifuel,1,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*(1.0-FLEETSHR(3)) &									 ! Class 8 tractor day low (non-fleet)
													*base_mkt_p1(ifuel,13)/(sum(base_mkt_p1(ifuel,13:18))+10E-12)                                     
		    TRK_19_regn(iyr,13,iage,ifuel,2,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*FLEETSHR(3) &									 	 ! Class 8 tractor day low (fleet)
													*base_mkt_p1(ifuel,13)/(sum(base_mkt_p1(ifuel,13:18))+10E-12) 
	        TRK_19_regn(iyr,14,iage,ifuel,1,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*(1.0-FLEETSHR(3)) &									 ! Class 8 tractor day mid (non-fleet)
													*base_mkt_p1(ifuel,14)/(sum(base_mkt_p1(ifuel,13:18))+10E-12)
		    TRK_19_regn(iyr,14,iage,ifuel,2,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*FLEETSHR(3) &									 	 ! Class 8 tractor day low (fleet)
													*base_mkt_p1(ifuel,14)/(sum(base_mkt_p1(ifuel,13:18))+10E-12)
	        TRK_19_regn(iyr,15,iage,ifuel,1,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*(1.0-FLEETSHR(3)) &									 ! Class 8 tractor day high (non-fleet)
													*base_mkt_p1(ifuel,15)/(sum(base_mkt_p1(ifuel,13:18))+10E-12)
		    TRK_19_regn(iyr,15,iage,ifuel,2,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*FLEETSHR(3) &									 	 ! Class 8 tractor day high (fleet)
													*base_mkt_p1(ifuel,15)/(sum(base_mkt_p1(ifuel,13:18))+10E-12)
	        TRK_19_regn(iyr,16,iage,ifuel,1,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*(1.0-FLEETSHR(3)) &									 ! Class 8 tractor sleeper low (non-fleet)
													*base_mkt_p1(ifuel,16)/(sum(base_mkt_p1(ifuel,13:18))+10E-12)
		    TRK_19_regn(iyr,16,iage,ifuel,2,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*FLEETSHR(3) &									 	 ! Class 8 tractor sleeper low (fleet)
													*base_mkt_p1(ifuel,16)/(sum(base_mkt_p1(ifuel,13:18))+10E-12)
	        TRK_19_regn(iyr,17,iage,ifuel,1,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*(1.0-FLEETSHR(3)) &									 ! Class 8 tractor sleeper mid (non-fleet)
													*base_mkt_p1(ifuel,17)/(sum(base_mkt_p1(ifuel,13:18))+10E-12)
		    TRK_19_regn(iyr,17,iage,ifuel,2,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*FLEETSHR(3) &									 	 ! Class 8 tractor sleeper mid (fleet)
													*base_mkt_p1(ifuel,17)/(sum(base_mkt_p1(ifuel,13:18))+10E-12) 
	        TRK_19_regn(iyr,18,iage,ifuel,1,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*(1.0-FLEETSHR(3)) &									 ! Class 8 tractor sleeper high (non-fleet)
													*base_mkt_p1(ifuel,18)/(sum(base_mkt_p1(ifuel,13:18))+10E-12)
		    TRK_19_regn(iyr,18,iage,ifuel,2,iregn) = trkstock(iyr,8,iage,ifuel,1,iregn)*FLEETSHR(3) &									 	 ! Class 8 tractor sleeper mid (fleet)
													*base_mkt_p1(ifuel,18)/(sum(base_mkt_p1(ifuel,13:18))+10E-12)
	        TRK_19_regn(iyr,19,iage,ifuel,1,iregn) = trkstock(iyr,9,iage,ifuel,1,iregn)*(1.0-FLEETSHR(3))             						 ! Class 8 tractor heavy haul 
		    TRK_19_regn(iyr,19,iage,ifuel,2,iregn) = trkstock(iyr,9,iage,ifuel,1,iregn)*FLEETSHR(3)						                     ! (not in phase 1)
            do icafe19 = 1,cafe19
			  do iflt = 1,flt
			    if(iregn.eq.9) TRK_19_regn(iyr,icafe19,iage,ifuel,iflt,11) = sum(TRK_19_regn(iyr,icafe19,iage,ifuel,iflt,1:9))
	            Fuel_Shr_regn(iyr,icafe19,ifuel,iflt,iregn) = TRK_19_regn(iyr,icafe19,1,ifuel,iflt,iregn)/sum(TRK_19_regn(iyr,icafe19,1,1:9,iflt,iregn))
			  enddo   ! end flt
            enddo     ! end cafe19
		  enddo       ! end fuel
        enddo         ! end age
	  enddo           ! end regn
	
!... Summing the stock to match the reporting aggregation levels
      do iage=1,age
        do ifuel=1,6
   	      do iflt = 1,FLT
            TRKSTK(cur,1,iage,ifuel,iflt) = sum(TRK_19_regn(iyr,3:4,iage,ifuel,iflt,11))
			TRKSTK(cur,2,iage,ifuel,iflt) = sum(TRK_19_regn(iyr,5:7,iage,ifuel,iflt,11))
			TRKSTK(cur,3,iage,ifuel,iflt) = sum(TRK_19_regn(iyr,8:19,iage,ifuel,iflt,11))
		  enddo
		enddo
	  enddo	

	  do iage=1,age
        do ifuel=1,6
          do icafe19 = 1,cafe19
		    TRKSTK_19(iyr,icafe19,iage,ifuel) = sum(TRK_19_regn(iyr,icafe19,iage,ifuel,1:2,11))
		  enddo
	    enddo
	  enddo	    
	  	
      do ifuel = 5,fuel9
	    if(sum(trkstk_19(iyr,1:2,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,4,1,ifuel) = 0.0
	    if(sum(trkstk_19(iyr,3:4,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,1,1,ifuel) = 0.0
	    if(sum(trkstk_19(iyr,5:7,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,2,1,ifuel) = 0.0
	    if(sum(trkstk_19(iyr,8:19,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,3,1,ifuel) = 0.0
	  enddo
		
!---------------------------------------------------------------------------------------------------------------------------------------------------!
!...apply scrapage rates to initialize first projection year's truck populations, excluding new trucks
!...first fill everything but the new and the 34+ vintage slots
!...Sales are regionalized
!---------------------------------------------------------------------------------------------------------------------------------------------------!
    else  ! curcalyr.gt.bsyr_stk
	  TRK_19_regn(iyr,:,:,:,:,:) = 0.0	
      do iregn = 1,mnumcr-2
	    do icafe19 = 1,cafe19 
	      do ifuel = 1,fuel9
            do iage=2,age-1
!			  class 2 		
		      if(icafe19.le.2) TRK_19_regn(iyr,icafe19,iage,ifuel,1:flt,iregn) = TRK_19_regn(iyr-1,icafe19,iage-1,ifuel,1:flt,iregn)*(1.0 - SCRAP_RATE(4,iage-1,ifuel))
!			  class 3 		
			  if(icafe19.ge.3.and.icafe19.le.4) TRK_19_regn(iyr,icafe19,iage,ifuel,1:flt,iregn) = TRK_19_regn(iyr-1,icafe19,iage-1,ifuel,1:flt,iregn)*(1.0 - SCRAP_RATE(1,iage-1,ifuel))
!			  class 4-6
			  if(icafe19.ge.5.and.icafe19.le.7) TRK_19_regn(iyr,icafe19,iage,ifuel,1:flt,iregn) = TRK_19_regn(iyr-1,icafe19,iage-1,ifuel,1:flt,iregn)*(1.0 - SCRAP_RATE(2,iage-1,ifuel))
!			  class 7&8
		      if(icafe19.ge.8) TRK_19_regn(iyr,icafe19,iage,ifuel,1:flt,iregn) = TRK_19_regn(iyr-1,icafe19,iage-1,ifuel,1:flt,iregn)*(1.0 - SCRAP_RATE(3,iage-1,ifuel))
            enddo
			  
!			fold in the next-to-last vintage with the 34+ vintage, scrapping the relevant shares 
!			class 2
            if(icafe19.le.2)                    TRK_19_regn(iyr,icafe19,AGE,ifuel,1:flt,iregn) = TRK_19_regn(iyr-1,icafe19,AGE,ifuel,1:flt,iregn)*0.7 + &
			                                        TRK_19_regn(iyr-1,icafe19,AGE-1,ifuel,1:flt,iregn)*(1.0 - SCRAP_RATE(4,iage-1,ifuel)) 	
!			class 3
            if(icafe19.ge.3.and.icafe19.le.4)   TRK_19_regn(iyr,icafe19,AGE,ifuel,1:flt,iregn) = TRK_19_regn(iyr-1,icafe19,AGE,ifuel,1:flt,iregn)*0.7 + &
			                                        TRK_19_regn(iyr-1,icafe19,AGE-1,ifuel,1:flt,iregn)*(1.0 - SCRAP_RATE(1,iage-1,ifuel))
!			class 4-6
            if(icafe19.ge.5.and.icafe19.le.7)   TRK_19_regn(iyr,icafe19,AGE,ifuel,1:flt,iregn) = TRK_19_regn(iyr-1,icafe19,AGE,ifuel,1:flt,iregn)*0.7 + &
			                                        TRK_19_regn(iyr-1,icafe19,AGE-1,ifuel,1:flt,iregn)*(1.0 - SCRAP_RATE(2,iage-1,ifuel)) 	
!			class 7&8	
            if(icafe19.ge.8) 					TRK_19_regn(iyr,icafe19,AGE,ifuel,1:flt,iregn) = TRK_19_regn(iyr-1,icafe19,AGE,ifuel,1:flt,iregn)*0.7 + &
			                                        TRK_19_regn(iyr-1,icafe19,AGE-1,ifuel,1:flt,iregn)*(1.0 - SCRAP_RATE(3,iage-1,ifuel))  		
          enddo				

!		  calculate stock transfers from fleet to nonfleet ownership - transfer only gasoline and diesel vehicles
          do ifuel=1,2
            do iage=2,age
!			  class 2
			  if(icafe19.le.2) then
			    TRK_19_regn(iyr,icafe19,iage,ifuel,NFT,iregn) = TRK_19_regn(iyr,icafe19,iage,ifuel,NFT,iregn) + (TFFXGRT(4,iage)*TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn))
			    TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn) = TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn) - (TFFXGRT(4,iage)*TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn))
!			  class 3
              elseif(icafe19.ge.3.and.icafe19.le.4) then
			    TRK_19_regn(iyr,icafe19,iage,ifuel,NFT,iregn) = TRK_19_regn(iyr,icafe19,iage,ifuel,NFT,iregn) + (TFFXGRT(1,iage)*TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn))
			    TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn) = TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn) - (TFFXGRT(1,iage)*TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn))	
!			  class 4-6
              elseif(icafe19.ge.5.and.icafe19.le.7) then
			    TRK_19_regn(iyr,icafe19,iage,ifuel,NFT,iregn) = TRK_19_regn(iyr,icafe19,iage,ifuel,NFT,iregn) + (TFFXGRT(2,iage)*TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn))
			    TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn) = TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn) - (TFFXGRT(2,iage)*TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn))	
!			  class 7&8
			  elseif(icafe19.ge.8) then
			    TRK_19_regn(iyr,icafe19,iage,ifuel,NFT,iregn) = TRK_19_regn(iyr,icafe19,iage,ifuel,NFT,iregn) + (TFFXGRT(3,iage)*TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn))
			    TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn) = TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn) - (TFFXGRT(3,iage)*TRK_19_regn(iyr,icafe19,iage,ifuel,FLT,iregn))	
              endif
            enddo   ! end age
          enddo     ! end fuel
        enddo       ! end cafe19
      enddo         ! end region

!---------------------------------------------------------------------------------------------------------------------------------------------------!	 
!...  project AFV penetration rates given technology penetration/fuel economy projections
!...  determine the share of each fuel for new truck sales
!...  calculate fuel cost of driving per mile
!---------------------------------------------------------------------------------------------------------------------------------------------------!
      share_fuel =  0.0
	  sc_share = 0.0
      Fuel_Shr_regn(iyr,:,:,:,:) = 0.0

      do iregn = 1,mnumcr-2
        do iflt=1,flt
          do icafe19 = 1,cafe19
		    isc19 = SCP2Map(icafe19)
!... Determine what the starting fuel shares are
		    FCOST_regn(:,iregn) = 0.0
            do ifuel=1,fuel9
              if(ifuel.ne.4) FCOST_regn(ifuel,iregn)=Avg_Fuel_$_regn(ifuel,iregn)/NEW_MPG_19(iyr,ifuel,icafe19)*HRATE(isc19,ifuel)*1E-6			! ifuel = 4 --> CNG
            enddo
		    if(icafe19.le.7) then           ! Classes 2b - 6
		      TEMP_PRICE = 0.0
              if(iflt.eq.1) then
                TEMP_PRICE = ((PGFTRPV(iregn,iyr) + PGFTRPV(iregn,iyr-1) + PGFTRPV(iregn,iyr-2))/3.0*1.115)/NEW_MPG_19(iyr,4,icafe19)*HRATE(isc19,4)*1E-6
              else
                TEMP_PRICE = ((PGFTRFV(iregn,iyr) + PGFTRFV(iregn,iyr-1) + PGFTRFV(iregn,iyr-2))/3.0*1.115)/NEW_MPG_19(iyr,4,icafe19)*HRATE(isc19,4)*1E-6
              endif
            else                            ! Classes 7 - 8 
              if(iflt.eq.1) then
                TEMP_PRICE = ((PGLTRPV(iregn,iyr) + PGLTRPV(iregn,iyr-1) + PGLTRPV(iregn,iyr-2))/3.0*1.115)/NEW_MPG_19(iyr,4,icafe19)*HRATE(isc19,4)*1E-6
              else
                TEMP_PRICE = ((PGLTRFV(iregn,iyr) + PGLTRFV(iregn,iyr-1) + PGLTRFV(iregn,iyr-2))/3.0*1.115)/NEW_MPG_19(iyr,4,icafe19)*HRATE(isc19,4)*1E-6
              endif
            endif
            FCOST_regn(4,iregn)= TEMP_PRICE		  

!...        Determining what the previous years fuel share was
	        do ifuel = 1,fuel9				
			  share_fuel(icafe19,ifuel,iflt,iregn) = TRK_19_regn(iyr-1,icafe19,1,ifuel,iflt,iregn)/sum(TRK_19_regn(iyr-1,icafe19,1,1:9,iflt,iregn))
		    enddo

!...        Distribute size class by 19 cafe size categories
  	        if(icafe19.le.2)                    sc_share(icafe19,iflt,iregn) = sum(TRK_19_regn(iyr-1,icafe19,1,1:9,iflt,iregn))/sum(TRK_19_regn(iyr-1,1:2,1,1:9,iflt,iregn))
            if(icafe19.ge.3.and.icafe19.le.4)   sc_share(icafe19,iflt,iregn) = sum(TRK_19_regn(iyr-1,icafe19,1,1:9,iflt,iregn))/sum(TRK_19_regn(iyr-1,3:4,1,1:9,iflt,iregn))
		    if(icafe19.ge.5.and.icafe19.le.7)   sc_share(icafe19,iflt,iregn) = sum(TRK_19_regn(iyr-1,icafe19,1,1:9,iflt,iregn))/sum(TRK_19_regn(iyr-1,5:7,1,1:9,iflt,iregn))	
		    if(icafe19.ge.8)                    sc_share(icafe19,iflt,iregn) = sum(TRK_19_regn(iyr-1,icafe19,1,1:9,iflt,iregn))/sum(TRK_19_regn(iyr-1,8:19,1,1:9,iflt,iregn))

!...        Distribute new truck sales by region
            if(icafe19.eq.1) then                   ! Only need to do this once since icafe19 is set for each equation, will not change in each icafe19 loop        
		      NEWTRUCKS_regn(iyr,4,iflt,iregn) = NEWTRUCKS_regn(iyr,4,iflt,11)*(sum(TRK_19_regn(iyr-1,1:2,1,1:9,iflt,iregn))/sum(TRK_19_regn(iyr-1,1:2,1,1:9,iflt,11)))
		      NEWTRUCKS_regn(iyr,1,iflt,iregn) = NEWTRUCKS_regn(iyr,1,iflt,11)*(sum(TRK_19_regn(iyr-1,3:4,1,1:9,iflt,iregn))/sum(TRK_19_regn(iyr-1,3:4,1,1:9,iflt,11)))
  		      NEWTRUCKS_regn(iyr,2,iflt,iregn) = NEWTRUCKS_regn(iyr,2,iflt,11)*(sum(TRK_19_regn(iyr-1,5:7,1,1:9,iflt,iregn))/sum(TRK_19_regn(iyr-1,5:7,1,1:9,iflt,11)))
		      NEWTRUCKS_regn(iyr,3,iflt,iregn) = NEWTRUCKS_regn(iyr,3,iflt,11)*(sum(TRK_19_regn(iyr-1,8:19,1,1:9,iflt,iregn))/sum(TRK_19_regn(iyr-1,8:19,1,1:9,iflt,11)))
		    endif

!			Start sales / fuel choice model		  
		    if(icafe19.le.4) then
              FSHR_T=0.0
              do jfuel=fuel9,1,-1
                if(jfuel.ne.2) then
                  if(jfuel.ge.3) then
			        if(curcalyr.lt.TRGSHXG(icafe19,jfuel,iflt)) cycle                                    ! Cannot purchase if the powertrain is not available
                    DCOST=1-(FCOST_regn(jfuel,iregn)/FCOST_regn(1,iregn)-1)*PRAFDFXG(isc19,jfuel)
!...                logistic penetration curve for AFV HDV vehicles
                    SLOPE=LOG(0.01)/((1.*CYAFVXG(isc19,jfuel,iflt))/2)
                    MIDYR=TRGSHXG(icafe19,jfuel,iflt)+(1.*CYAFVXG(isc19,jfuel,iflt)/2)
                    MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)=DCOST*(BFSHXG(isc19,jfuel,iflt)+(EFSHXG(isc19,jfuel,iflt)-BFSHXG(isc19,jfuel,iflt))  &
                                        /(1.+(exp(SLOPE*(1.*curcalyr-MIDYR)))))				  
                    MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)=MIN(MPATH_regn(icafe19,jfuel,iflt,iyr,iregn),1.0)
					Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)
!...                calculate natural gas vehicle share
                    if(jfuel.eq.4)then
                      Fuel_Shr_regn(iyr,icafe19,4,iflt,iregn)=0.0
!...                  compute incremental cost and share of cng vehicles desired
                      do ivmt=1, nvmt
                        buy_cng_regn(ivmt,iflt,icafe19,1:4,iregn)=0.0
                        max_buy_regn(ivmt,iflt,icafe19,iregn)=0.0
                        if(veh_shr(ivmt,iflt,isc19).gt.0.0)then
!...                      calulate annual fuel saving of cng relative to diesel and sum net present value of savings by payback period
                          ANN_$_SAVINGS_CNG_regn(ivmt,iflt,icafe19,iregn)= VMT_VEH(ivmt,iflt,isc19)*(FCOST_regn(2,iregn)-FCOST_regn(4,iregn))
                          NPV_ADS_regn(ivmt,iflt,icafe19,1,iregn)=ANN_$_SAVINGS_CNG_regn(ivmt,iflt,icafe19,iregn)/(1.0+DISCRTXG)
                          do Y=2,4
                            NPV_ADS_regn(ivmt,iflt,icafe19,Y,iregn)=NPV_ADS_regn(ivmt,iflt,icafe19,Y-1,iregn)+ANN_$_SAVINGS_CNG_regn(ivmt,iflt,icafe19,iregn)/(1.0 + DISCRTXG)**(Y)
                          enddo
!...                      calculate market share purchase decision by payback group
                          do Y=1,4
                            if(INC_COST_CNG(ivmt,isc19).lt.NPV_ADS_regn(ivmt,iflt,icafe19,Y,iregn))then
                              buy_cng_regn(ivmt,iflt,icafe19,y,iregn) = pback_shr(y)*VEH_SHR(ivmt,iflt,isc19)
                            endif
                          enddo
!...                      sum the purchase decision across payback periods
                          max_buy_regn(ivmt,iflt,icafe19,iregn) = sum(buy_cng_regn(ivmt,iflt,icafe19,1:4,iregn))
                          Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) + (max_buy_regn(ivmt,iflt,icafe19,iregn)*MPATH_regn(icafe19,jfuel,iflt,iyr,iregn))
						endif	
				      enddo
!					  Limit growth/decline to +/- 10%
				      if(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn).gt.Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)) then
				        Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = MIN(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn), Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)*1.1)	
                      elseif(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn).lt.Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)) then
				        Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = MAX(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn),Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)*0.9)					
				      endif
                    endif
                    if(sum(fuel_shr_regn(iyr,icafe19,3:4,iflt,iregn)).gt.1.0) fuel_shr_regn(iyr,icafe19,3,iflt,iregn) = 1.0 - fuel_shr_regn(iyr,icafe19,4,iflt,iregn)	

!...                calculate Battery Electric Vehicle (BEV) Share  vehicle share [smn]
                    if(jfuel.eq.6)then
                      Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn)=0.0
				      battery_size_2b3_avg(iyr,iflt,icafe19,iregn) = 0.0
				      battery_size_46_avg(iyr,iflt,icafe19,iregn) = 0.0
				      battery_size_78_avg(iyr,iflt,icafe19,iregn) = 0.0
!...                  compute incremental cost and share of BEV vehicles desired
                      do ivmt=1, nvmt
                        buy_bev_regn(ivmt,iflt,icafe19,1:4,iregn)=0.0
                        max_buy_regn(ivmt,iflt,icafe19,iregn)=0.0
                        if(veh_shr(ivmt,iflt,isc19).gt.0.0)then
!...                      calulate annual fuel saving of BEV relative to gasoline and sum net present value of savings by payback period
                          ANN_$_SAVINGS_BEV_regn(ivmt,iflt,icafe19,iregn)= VMT_VEH(ivmt,iflt,isc19)*(FCOST_regn(2,iregn)-FCOST_regn(jfuel,iregn))
                          NPV_ADS_regn(ivmt,iflt,icafe19,1,iregn)=ANN_$_SAVINGS_BEV_regn(ivmt,iflt,icafe19,iregn)/(1.0+DISCRTXG)
                          do Y=2,4
                            NPV_ADS_regn(ivmt,iflt,icafe19,Y,iregn)=NPV_ADS_regn(ivmt,iflt,icafe19,Y-1,iregn)+ANN_$_SAVINGS_BEV_regn(ivmt,iflt,icafe19,iregn)/(1.0 + DISCRTXG)**(Y)
                          enddo
!...                      calculate market share purchase decision by payback group
                          do Y=1,4
                            if(INC_COST_BEV(ivmt,isc19).lt.NPV_ADS_regn(ivmt,iflt,icafe19,Y,iregn))then
                              buy_bev_regn(ivmt,iflt,icafe19,y,iregn) = pback_shr(y)*VEH_SHR(ivmt,iflt,isc19)
                            endif
                          enddo
!...                      sum the purchase decision across payback periods
                          max_buy_regn(ivmt,iflt,icafe19,iregn) = sum(buy_bev_regn(ivmt,iflt,icafe19,1:4,iregn))
                          Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) + (max_buy_regn(ivmt,iflt,icafe19,iregn)*MPATH_regn(icafe19,jfuel,iflt,iyr,iregn))
						endif
                      enddo  ! end VMT buckets loop
				      !calculate weighted average battery size
				      if(SUM(max_buy_regn(:,iflt,icafe19,iregn)).gt.0.0)then
					    do ivmt=1, nvmt
						  if(icafe19.le.4) &
						    battery_size_2b3_avg(iyr,iflt,icafe19,iregn) = battery_size_2b3_avg(iyr,iflt,icafe19,iregn) + &
						  											   max_buy_regn(ivmt,iflt,icafe19,iregn)/SUM(max_buy_regn(:,iflt,icafe19,iregn))*battery_size_2b3(ivmt)
					    enddo
				      endif
!					  Limit growth/decline to +/- 10%
				      if(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn).gt.Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)) then
				        Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = MIN(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn), Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)*1.1)	
                      elseif(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn).lt.Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)) then
				        Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = MAX(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn),Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)*0.9)					
				      endif
                    endif  ! end of jfuel.eq.6
              
                  elseif(jfuel.eq.1)then
                    MPATH_regn(icafe19,jfuel,iflt,iyr,iregn) = BFSHXG(isc19,jfuel,iflt) + (EFSHXG(isc19,jfuel,iflt) - & 
				                                               BFSHXG(isc19,jfuel,iflt))*(1. - exp(CSTDXG(isc19,iflt) + CSTDVXG(isc19,iflt)*(1.*curcalyr)))
                    TEMP=MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)*(1.-FSHR_T)
                    Fuel_Shr_regn(iyr,icafe19,1,iflt,iregn) = MIN(TEMP,1.0)
                    FSHR_T = FSHR_T+Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn)				
                  endif
                endif
              enddo  	! end fuel loop
		      Fuel_Shr_regn(iyr,icafe19,2,iflt,iregn) = MAX((1.-FSHR_T),0.0)

		    else  ! if(icafe19.gt.4)  Classes 4-8
              FSHR_T=0.0		  
              do jfuel=fuel9,1,-1
                if(jfuel.ne.2) then
                  if(jfuel.ge.3) then
		            if(curcalyr.lt.TRGSHXG(icafe19,jfuel,iflt)) cycle  			  
                    DCOST=1-(FCOST_regn(jfuel,iregn)/FCOST_regn(1,iregn)-1)*PRAFDFXG(isc19,jfuel)				
!...                logistic penetration curve for AFV HDV vehicles
                    SLOPE=LOG(0.01)/((1.*CYAFVXG(isc19,jfuel,iflt))/2)
                    MIDYR=TRGSHXG(icafe19,jfuel,iflt)+(1.*CYAFVXG(isc19,jfuel,iflt)/2)
                    MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)=DCOST*(BFSHXG(isc19,jfuel,iflt)+(EFSHXG(isc19,jfuel,iflt)-BFSHXG(isc19,jfuel,iflt))  &
                                          /(1.+(exp(SLOPE*(1.*curcalyr-MIDYR)))))								  
                    MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)=MIN(MPATH_regn(icafe19,jfuel,iflt,iyr,iregn),1.0)
					Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)
!...                calculate natural gas vehicle share
                    if(jfuel.eq.4)then
                      Fuel_Shr_regn(iyr,icafe19,4,iflt,iregn)=0.0
!...                  compute incremental cost and share of cng vehicles desired
                      do ivmt=1, nvmt
                        buy_cng_regn(ivmt,iflt,icafe19,1:4,iregn)=0.0
                        max_buy_regn(ivmt,iflt,icafe19,iregn)=0.0
                        if(veh_shr(ivmt,iflt,isc19).gt.0.0)then
!...                      calulate annual fuel saving of cng relative to diesel and sum net present value of savings by payback period
                          ANN_$_SAVINGS_CNG_regn(ivmt,iflt,icafe19,iregn)= VMT_VEH(ivmt,iflt,isc19)*(FCOST_regn(1,iregn)-FCOST_regn(4,iregn))
                          NPV_ADS_regn(ivmt,iflt,icafe19,1,iregn)=ANN_$_SAVINGS_CNG_regn(ivmt,iflt,icafe19,iregn)/(1.0+DISCRTXG)
						  do Y=2,4
                            NPV_ADS_regn(ivmt,iflt,icafe19,Y,iregn)=NPV_ADS_regn(ivmt,iflt,icafe19,Y-1,iregn)+ & 
						                                        ANN_$_SAVINGS_CNG_regn(ivmt,iflt,icafe19,iregn)/(1.0 + DISCRTXG)**(Y)
                          enddo
!...                      calculate market share purchase decision by payback group
                          do Y=1,4
                            if(INC_COST_CNG(ivmt,isc19).lt.NPV_ADS_regn(ivmt,iflt,icafe19,Y,iregn))then
                              buy_cng_regn(ivmt,iflt,icafe19,y,iregn) = pback_shr(y)*VEH_SHR(ivmt,iflt,isc19)
                            endif
                          enddo
!...                      sum the purchase decision across payback periods
                          max_buy_regn(ivmt,iflt,icafe19,iregn) = sum(buy_cng_regn(ivmt,iflt,icafe19,1:4,iregn))
                          Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) + &
					                                               (max_buy_regn(ivmt,iflt,icafe19,iregn)*MPATH_regn(icafe19,jfuel,iflt,iyr,iregn))
						endif
				      enddo
!					  Limit growth/decline to +/- 10%
				      if(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn).gt.Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)) then
				        Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = MIN(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn), Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)*1.1)	
                      elseif(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn).lt.Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)) then
				        Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = MAX(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn),Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)*0.9)					
				      endif
                    endif
                    if(sum(fuel_shr_regn(iyr,icafe19,3:4,iflt,iregn)).gt.1.0) fuel_shr_regn(iyr,icafe19,3,iflt,iregn) = 1.0 - fuel_shr_regn(iyr,icafe19,4,iflt,iregn)

!...                calculate Battery Electric Vehicle (BEV) Share  vehicle share [SMN] 
                    if(jfuel.eq.6)then
                      Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn)=0.0
!...                  compute incremental cost and share of BEV vehicles desired
                      do ivmt=1, nvmt
                        buy_bev_regn(ivmt,iflt,icafe19,1:4,iregn)=0.0
                        max_buy_regn(ivmt,iflt,icafe19,iregn)=0.0
                        if(veh_shr(ivmt,iflt,isc19).gt.0.0)then
!...                      calculate annual fuel saving of BEV relative to diesel and sum net present value of savings by payback period
                          ANN_$_SAVINGS_BEV_regn(ivmt,iflt,icafe19,iregn)= VMT_VEH(ivmt,iflt,isc19)*(FCOST_regn(1,iregn)-FCOST_regn(jfuel,iregn))
                          NPV_ADS_regn(ivmt,iflt,icafe19,1,iregn)=ANN_$_SAVINGS_BEV_regn(ivmt,iflt,icafe19,iregn)/(1.0+DISCRTXG)
                          do Y=2,4
                            NPV_ADS_regn(ivmt,iflt,icafe19,Y,iregn)=NPV_ADS_regn(ivmt,iflt,icafe19,Y-1,iregn)+ANN_$_SAVINGS_BEV_regn(ivmt,iflt,icafe19,iregn)/(1.0 + DISCRTXG)**(Y)
                          enddo
!...                      calculate market share purchase decision by payback group
                          do Y=1,4
                            if(INC_COST_BEV(ivmt,isc19).lt.NPV_ADS_regn(ivmt,iflt,icafe19,Y,iregn))then
                              buy_bev_regn(ivmt,iflt,icafe19,y,iregn) = pback_shr(y)*VEH_SHR(ivmt,iflt,isc19)
                            endif
                          enddo
!...                      sum the purchase decision across payback periods
                          max_buy_regn(ivmt,iflt,icafe19,iregn) = sum(buy_bev_regn(ivmt,iflt,icafe19,1:4,iregn))
                          Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) + (max_buy_regn(ivmt,iflt,icafe19,iregn)*MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)) 
                        endif
                      enddo  ! end VMT buckets loop
				      !... calculate weighted average battery size
				      battery_size_2b3_avg(iyr,iflt,icafe19,iregn) = 0.0
				      battery_size_46_avg(iyr,iflt,icafe19,iregn) = 0.0
				      battery_size_78_avg(iyr,iflt,icafe19,iregn) = 0.0
				      if(SUM(max_buy_regn(:,iflt,icafe19,iregn)).gt.0.0)then
					    do ivmt=1, nvmt
						  if(icafe19.ge.5.and.icafe19.le.7) &    ! class 4-6
						    battery_size_46_avg(iyr,iflt,icafe19,iregn) = battery_size_46_avg(iyr,iflt,icafe19,iregn) + &
						  											   max_buy_regn(ivmt,iflt,icafe19,iregn)/SUM(max_buy_regn(:,iflt,icafe19,iregn))*battery_size_46(ivmt)
						  if(icafe19.ge.8) &                     ! class 7&8			
						    battery_size_78_avg(iyr,iflt,icafe19,iregn) = battery_size_78_avg(iyr,iflt,icafe19,iregn) + &
																	   max_buy_regn(ivmt,iflt,icafe19,iregn)/SUM(max_buy_regn(:,iflt,icafe19,iregn))*battery_size_78(ivmt)
					    enddo
				      endif
!					  Limit growth/decline to +/- 10%
				      if(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn).gt.Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)) then
				        Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = MIN(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn), Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)*1.1)	
                      elseif(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn).lt.Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)) then
				        Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn) = MAX(Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn),Fuel_Shr_regn(iyr-1,icafe19,jfuel,iflt,iregn)*0.9)					
				      endif
                    endif  ! end of jfuel.eq.6     
				
                  elseif(jfuel.eq.1)then 
                    MPATH_regn(icafe19,jfuel,iflt,iyr,iregn) = BFSHXG(isc19,jfuel,iflt) + (EFSHXG(isc19,jfuel,iflt) - & 
				                                               BFSHXG(isc19,jfuel,iflt))*(1. - exp(CSTDXG(isc19,iflt) + CSTDVXG(isc19,iflt)*(1.*curcalyr)))
                    Fuel_Shr_regn(iyr,icafe19,1,iflt,iregn) = MIN(MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)*(1.-FSHR_T),1.0)
                    FSHR_T = FSHR_T+Fuel_Shr_regn(iyr,icafe19,jfuel,iflt,iregn)
                  endif
                endif
              enddo  	! end fuel loop
		      Fuel_Shr_regn(iyr,icafe19,2,iflt,iregn) = MAX((1.-FSHR_T),0.0)		
            endif

!			Cap gasoline and E85 share to the max allowable share of the two
            if(icafe19.le.7) then
              if((Fuel_Shr_regn(iyr,icafe19,2,iflt,iregn) + Fuel_Shr_regn(iyr,icafe19,5,iflt,iregn)).gt.max(EFSHXG(isc19,2,iflt),EFSHXG(isc19,5,iflt))) &
		                Fuel_Shr_regn(iyr,icafe19,2,iflt,iregn) = max(EFSHXG(isc19,2,iflt),EFSHXG(isc19,5,iflt)) - Fuel_Shr_regn(iyr,icafe19,5,iflt,iregn)	
			  if(Fuel_Shr_regn(iyr,icafe19,2,iflt,iregn).lt.0.0) then 
			    Fuel_Shr_regn(iyr,icafe19,5,iflt,iregn) = Fuel_Shr_regn(iyr,icafe19,5,iflt,iregn) + Fuel_Shr_regn(iyr,icafe19,2,iflt,iregn)
				Fuel_Shr_regn(iyr,icafe19,2,iflt,iregn) = 0.0
			  endif
            endif
!			Heavy haul can only use diesel
            if(icafe19.eq.19) then
			  Fuel_Shr_regn(iyr,icafe19,1,iflt,iregn) = 1.0
			  Fuel_Shr_regn(iyr,icafe19,2:9,iflt,iregn) = 0.0
			endif
          enddo   ! end size class loop		  
        enddo     ! end fleet loop
      enddo       ! end regional loop

!	  Normalize fuel shares	to ensure they sum to 1
      do iregn = 1,mnumcr-2
        do iflt=1,flt
          do icafe19 = 1,cafe19
		    do ifuel = 1,fuel9
			  if (ifuel.eq.1) TEMP = SUM(Fuel_Shr_regn(iyr,icafe19,:,iflt,iregn))
			  Fuel_Shr_regn(iyr,icafe19,ifuel,iflt,iregn) = Fuel_Shr_regn(iyr,icafe19,ifuel,iflt,iregn)/TEMP
!			  if (ifuel.eq.fuel9) write(21,'(a,i6,",",3i4,",",10(f10.4,","))') 'mdr_fuelshrcheck', curcalyr, icafe19, iflt, iregn, Fuel_Shr_regn(iyr,icafe19,1:9,iflt,iregn), sum(Fuel_Shr_regn(iyr,icafe19,1:9,iflt,iregn))
			enddo
		  enddo
		enddo
	  enddo

!	  Distribute total new truck sales to fuels
	  do iregn=1,mnumcr-2
	    do iflt=1,flt
		  do icafe19=1,cafe19
            do ifuel = 1,fuel9
              if(icafe19.le.2) &                     ! class 2  
		        TRK_19_regn(iyr,icafe19,1,ifuel,iflt,iregn) = NEWTRUCKS_regn(iyr,4,iflt,iregn)*Fuel_Shr_regn(iyr,icafe19,ifuel,iflt,iregn)*sc_share(icafe19,iflt,iregn)
              if(icafe19.ge.3.and.icafe19.le.4) &    ! class 3
		  	    TRK_19_regn(iyr,icafe19,1,ifuel,iflt,iregn) = NEWTRUCKS_regn(iyr,1,iflt,iregn)*Fuel_Shr_regn(iyr,icafe19,ifuel,iflt,iregn)*sc_share(icafe19,iflt,iregn)
              if(icafe19.ge.5.and.icafe19.le.7) &    ! class 4-6
		  	    TRK_19_regn(iyr,icafe19,1,ifuel,iflt,iregn) = NEWTRUCKS_regn(iyr,2,iflt,iregn)*Fuel_Shr_regn(iyr,icafe19,ifuel,iflt,iregn)*sc_share(icafe19,iflt,iregn)
              if(icafe19.ge.8) &                     ! class 7&8			
		  	    TRK_19_regn(iyr,icafe19,1,ifuel,iflt,iregn) = NEWTRUCKS_regn(iyr,3,iflt,iregn)*Fuel_Shr_regn(iyr,icafe19,ifuel,iflt,iregn)*sc_share(icafe19,iflt,iregn)
              do iage = 1,age
		  	    TRK_19_regn(iyr,icafe19,iage,ifuel,iflt,11) = sum(TRK_19_regn(iyr,icafe19,iage,ifuel,iflt,1:9))
		  	  enddo

!	 		  Sum up new GWh of EV battery added to stock and add back to tran.f [smn]
			  if (ifuel.eq.6) then
                new_gwh = 0.0
                if(icafe19.le.4) &                     ! class 2-3
                  new_gwh = TRK_19_regn(iyr,icafe19,1,6,iflt,iregn) * battery_size_2b3_avg(iyr,iflt,icafe19,iregn) / 1000.0
                if(icafe19.ge.5.and.icafe19.le.7) &    ! class 4-6
                  new_gwh = TRK_19_regn(iyr,icafe19,1,6,iflt,iregn) * battery_size_46_avg(iyr,iflt,icafe19,iregn) / 1000.0
                if(icafe19.ge.8) &                     ! class 7&8			
                  new_gwh = TRK_19_regn(iyr,icafe19,1,6,iflt,iregn) * battery_size_78_avg(iyr,iflt,icafe19,iregn) / 1000.0
		        cumulative_gwh(iyr) = cumulative_gwh(iyr) + new_gwh
!			    Testing HDV GWh accounting [smn]
!		        write(21, '(4I10, 1F10.6, 5F10.3)') iyr, icafe19, iflt, iregn, new_gwh, cumulative_gwh(iyr), & 
!			    			battery_size_2b3_avg(iyr,iflt,icafe19,iregn), battery_size_46_avg(iyr,iflt,icafe19,iregn), battery_size_78_avg(iyr,iflt,icafe19,iregn), &
!			    			trk_19_regn(iyr,icafe19,1,6,iflt,iregn)	
			  endif
            enddo
		  enddo
		enddo
	  enddo

!	  Summing the stock to match the reporting aggregation levels
      do iage=1,age
        do ifuel=1,fuel9
   	      do iflt = 1,FLT
            TRKSTK(cur,1,iage,ifuel,iflt) = sum(TRK_19_regn(iyr,3:4,iage,ifuel,iflt,11))
	  	    TRKSTK(cur,2,iage,ifuel,iflt) = sum(TRK_19_regn(iyr,5:7,iage,ifuel,iflt,11))
	  	    TRKSTK(cur,3,iage,ifuel,iflt) = sum(TRK_19_regn(iyr,8:19,iage,ifuel,iflt,11))
          enddo
!		  Putting the truck stock in the format used in the fuel economy model		  
          do icafe19 = 1,cafe19
            TRKSTK_19(iyr,icafe19,iage,ifuel) = sum(TRK_19_regn(iyr,icafe19,iage,ifuel,1:2,11)) 	  
	      enddo
        enddo
      enddo

!	  Calculate CAFE credit sales allowed for PHEVs, EVs, and FCVs
!	  apply credit multipliers to sales
      do icafe19=1,cafe19
	    do ifuel=1,fuel9
	      credit=1.0
	      if(ifuel.eq.6) credit=4.5
          if(ifuel.ge.7.and.ifuel.le.8) credit=3.5
          if(ifuel.eq.9) credit=5.5
          credsales(iyr,icafe19,ifuel)=trkstk_19(iyr,icafe19,1,ifuel)*credit
        enddo
      enddo	  

!	  age vehicle fuel economy to correspond with vehicle vintaging
      do isc=1,sc
        do iage=2,age-1
          HDV_MPG(iyr,isc,iage,1:FUEL9) = HDV_MPG(iyr-1,isc,IAGE-1,1:FUEL9)
        enddo
!...    combine mpg for the last two vintages , weighting by number of trucks
        do ifuel=1,fuel9
          VMTTMP(1)=SUM(VMTFLT(LAG,ISC,AGE-1,IFUEL,1:FLT))
          VMTTMP(2)=SUM(VMTFLT(LAG,ISC,AGE,IFUEL,1:FLT))
          if(VMTTMP(1)+VMTTMp(2).le.0.0)then
            HDV_MPG(iyr,isc,AGE,ifuel) = HDV_MPG(iyr-1,isc,age-1,ifuel)
          else
            HDV_MPG(iyr,isc,AGE,ifuel) = HARMONIC_MEAN(HDV_MPG(iyr-1,isc,AGE-1:AGE,ifuel),VMTTMP(1:2),2)
          endif
        enddo
      enddo
    endif	! curcalyr

	do ifuel = 5,fuel9
	  if(sum(trkstk_19(iyr,1:2,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,4,1,ifuel) = 0.0
	  if(sum(trkstk_19(iyr,3:4,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,1,1,ifuel) = 0.0
	  if(sum(trkstk_19(iyr,5:7,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,2,1,ifuel) = 0.0
	  if(sum(trkstk_19(iyr,8:19,1:age,ifuel)).eq.0.0) HDV_MPG(iyr,3,1,ifuel) = 0.0
	enddo

    do isc=1,sc
      TRKTMP(isc,FLT)=SUM(TRKSTK(cur,isc,1:AGE,1:FUEL9,FLT))
      TRKTMP(isc,NFT)=SUM(TRKSTK(cur,isc,1:AGE,1:FUEL9,NFT))
    enddo

!... These percents will be used in the fuel economy caluclations for CAFE size classes	
	if(curcalyr.ge.2010) then
	  do ifuel = 1,9
	    if((TRKSTK_19(iyr,1,1,ifuel) + TRKSTK_19(iyr,3,1,ifuel)).ne.0.0) then	                                                    ! Class 2b-3 pickup and van
	      tstk_per_p2(iyr,1,ifuel) = TRKSTK_19(iyr,1,1,ifuel)/(TRKSTK_19(iyr,1,1,ifuel) + TRKSTK_19(iyr,3,1,ifuel))
	      tstk_per_p2(iyr,3,ifuel) = TRKSTK_19(iyr,3,1,ifuel)/(TRKSTK_19(iyr,1,1,ifuel) + TRKSTK_19(iyr,3,1,ifuel))
        else
	      tstk_per_p2(iyr,1,ifuel) = sum(TRKSTK_19(iyr,1,1,1:9))/sum(TRKSTK_19(iyr,1,1,1:9) + TRKSTK_19(iyr,3,1,1:9))
	      tstk_per_p2(iyr,3,ifuel) = sum(TRKSTK_19(iyr,3,1,1:9))/sum(TRKSTK_19(iyr,1,1,1:9) + TRKSTK_19(iyr,3,1,1:9))
        endif	
	    if((TRKSTK_19(iyr,2,1,ifuel) + sum(TRKSTK_19(iyr,4:6,1,ifuel))).ne.0.0) then	                                            ! Class 2b-5 vocational
          tstk_per_p2(iyr,2,ifuel) = TRKSTK_19(iyr,2,1,ifuel)/(TRKSTK_19(iyr,2,1,ifuel) + sum(TRKSTK_19(iyr,4:6,1,ifuel)))
          tstk_per_p2(iyr,4,ifuel) = TRKSTK_19(iyr,4,1,ifuel)/(TRKSTK_19(iyr,2,1,ifuel) + sum(TRKSTK_19(iyr,4:6,1,ifuel)))
	      tstk_per_p2(iyr,5,ifuel) = TRKSTK_19(iyr,5,1,ifuel)/(TRKSTK_19(iyr,2,1,ifuel) + sum(TRKSTK_19(iyr,4:6,1,ifuel)))
	      tstk_per_p2(iyr,6,ifuel) = TRKSTK_19(iyr,6,1,ifuel)/(TRKSTK_19(iyr,2,1,ifuel) + sum(TRKSTK_19(iyr,4:6,1,ifuel)))
	    else
          tstk_per_p2(iyr,2,ifuel) = sum(TRKSTK_19(iyr,2,1,1:9))/(sum(TRKSTK_19(iyr,2,1,1:9)) + sum(TRKSTK_19(iyr,4:6,1,1:9)))
          tstk_per_p2(iyr,4,ifuel) = sum(TRKSTK_19(iyr,4,1,1:9))/(sum(TRKSTK_19(iyr,2,1,1:9)) + sum(TRKSTK_19(iyr,4:6,1,1:9)))
	      tstk_per_p2(iyr,5,ifuel) = sum(TRKSTK_19(iyr,5,1,1:9))/(sum(TRKSTK_19(iyr,2,1,1:9)) + sum(TRKSTK_19(iyr,4:6,1,1:9)))
	      tstk_per_p2(iyr,6,ifuel) = sum(TRKSTK_19(iyr,6,1,1:9))/(sum(TRKSTK_19(iyr,2,1,1:9)) + sum(TRKSTK_19(iyr,4:6,1,1:9)))	
	    endif
        if(sum(TRKSTK_19(iyr,7:8,1,ifuel)).ne.0.0) then                                                                               ! Class 6-7 vocational
          tstk_per_p2(iyr,7,ifuel) = TRKSTK_19(iyr,7,1,ifuel)/sum(TRKSTK_19(iyr,7:8,1,ifuel))
	      tstk_per_p2(iyr,8,ifuel) = TRKSTK_19(iyr,8,1,ifuel)/sum(TRKSTK_19(iyr,7:8,1,ifuel))
	    else
	      tstk_per_p2(iyr,7,ifuel) = sum(TRKSTK_19(iyr,7,1,1:9))/sum(TRKSTK_19(iyr,7:8,1,1:9))
	      tstk_per_p2(iyr,8,ifuel) = sum(TRKSTK_19(iyr,8,1,1:9))/sum(TRKSTK_19(iyr,7:8,1,1:9))
        endif
!... These percents are for reporting size classes
		if(sum(TRKSTK_19(iyr,1:2,1,ifuel)).gt.0.0) then
		  tstk_per_sc(iyr,1,ifuel) = TRKSTK_19(iyr,1,1,ifuel)/sum(TRKSTK_19(iyr,1:2,1,ifuel))
		  tstk_per_sc(iyr,2,ifuel) = TRKSTK_19(iyr,2,1,ifuel)/sum(TRKSTK_19(iyr,1:2,1,ifuel))
        else
		  tstk_per_sc(iyr,1,ifuel) = sum(TRKSTK_19(iyr,1,1,1:9))/sum(TRKSTK_19(iyr,1:2,1,1:9))
		  tstk_per_sc(iyr,2,ifuel) = sum(TRKSTK_19(iyr,2,1,1:9))/sum(TRKSTK_19(iyr,1:2,1,1:9)) 					
	    endif
		if(sum(TRKSTK_19(iyr,3:4,1,ifuel)).gt.0.0) then
		  tstk_per_sc(iyr,3,ifuel) = TRKSTK_19(iyr,3,1,ifuel)/sum(TRKSTK_19(iyr,3:4,1,ifuel))
		  tstk_per_sc(iyr,4,ifuel) = TRKSTK_19(iyr,4,1,ifuel)/sum(TRKSTK_19(iyr,3:4,1,ifuel))
        else
		  tstk_per_sc(iyr,3,ifuel) = sum(TRKSTK_19(iyr,3,1,1:9))/sum(TRKSTK_19(iyr,3:4,1,1:9))
		  tstk_per_sc(iyr,4,ifuel) = sum(TRKSTK_19(iyr,4,1,1:9))/sum(TRKSTK_19(iyr,3:4,1,1:9)) 					
	    endif		
		if(sum(TRKSTK_19(iyr,5:7,1,ifuel)).gt.0.0) then
		  tstk_per_sc(iyr,5,ifuel) = TRKSTK_19(iyr,5,1,ifuel)/sum(TRKSTK_19(iyr,5:7,1,ifuel))
		  tstk_per_sc(iyr,6,ifuel) = TRKSTK_19(iyr,6,1,ifuel)/sum(TRKSTK_19(iyr,5:7,1,ifuel))
		  tstk_per_sc(iyr,7,ifuel) = TRKSTK_19(iyr,7,1,ifuel)/sum(TRKSTK_19(iyr,5:7,1,ifuel))
        else
		  tstk_per_sc(iyr,5,ifuel) = sum(TRKSTK_19(iyr,5,1,1:9))/sum(TRKSTK_19(iyr,5:7,1,1:9))
		  tstk_per_sc(iyr,6,ifuel) = sum(TRKSTK_19(iyr,6,1,1:9))/sum(TRKSTK_19(iyr,5:7,1,1:9)) 		
		  tstk_per_sc(iyr,7,ifuel) = sum(TRKSTK_19(iyr,7,1,1:9))/sum(TRKSTK_19(iyr,5:7,1,1:9))		  
	    endif	
		if(sum(TRKSTK_19(iyr,8:19,1,ifuel)).gt.0.0) then	
		  tstk_per_sc(iyr,8,ifuel) = TRKSTK_19(iyr,8,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))	
		  tstk_per_sc(iyr,9,ifuel) = TRKSTK_19(iyr,9,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		  tstk_per_sc(iyr,10,ifuel) = TRKSTK_19(iyr,10,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		  tstk_per_sc(iyr,11,ifuel) = TRKSTK_19(iyr,11,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		  tstk_per_sc(iyr,12,ifuel) = TRKSTK_19(iyr,12,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		  tstk_per_sc(iyr,13,ifuel) = TRKSTK_19(iyr,13,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		  tstk_per_sc(iyr,14,ifuel) = TRKSTK_19(iyr,14,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		  tstk_per_sc(iyr,15,ifuel) = TRKSTK_19(iyr,15,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		  tstk_per_sc(iyr,16,ifuel) = TRKSTK_19(iyr,16,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		  tstk_per_sc(iyr,17,ifuel) = TRKSTK_19(iyr,17,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		  tstk_per_sc(iyr,18,ifuel) = TRKSTK_19(iyr,18,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		  tstk_per_sc(iyr,19,ifuel) = TRKSTK_19(iyr,19,1,ifuel)/sum(TRKSTK_19(iyr,8:19,1,ifuel))
		else
		  tstk_per_sc(iyr,8,ifuel) = sum(TRKSTK_19(iyr,8,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))	
		  tstk_per_sc(iyr,9,ifuel) = sum(TRKSTK_19(iyr,9,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))
		  tstk_per_sc(iyr,10,ifuel) = sum(TRKSTK_19(iyr,10,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))
		  tstk_per_sc(iyr,11,ifuel) = sum(TRKSTK_19(iyr,11,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))
		  tstk_per_sc(iyr,12,ifuel) = sum(TRKSTK_19(iyr,12,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))
		  tstk_per_sc(iyr,13,ifuel) = sum(TRKSTK_19(iyr,13,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))
		  tstk_per_sc(iyr,14,ifuel) = sum(TRKSTK_19(iyr,14,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))
		  tstk_per_sc(iyr,15,ifuel) = sum(TRKSTK_19(iyr,15,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))
		  tstk_per_sc(iyr,16,ifuel) = sum(TRKSTK_19(iyr,16,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))
		  tstk_per_sc(iyr,17,ifuel) = sum(TRKSTK_19(iyr,17,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))
		  tstk_per_sc(iyr,18,ifuel) = sum(TRKSTK_19(iyr,18,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9))
		  tstk_per_sc(iyr,19,ifuel) = sum(TRKSTK_19(iyr,19,1,1:9))/sum(TRKSTK_19(iyr,8:19,1,1:9)) 
        endif
	  enddo
	endif

!... Filling in the commerical light truck data to report back to tran.f
    if(curcalyr.ge.2012) then
      do iage = 1,age
	    do iregn = 1,mnumcr-2
		  CLTSTK(iyr,1,iage,iregn) = sum(TRK_19_regn(iyr,1,iage,2,1:2,iregn)) + sum(TRK_19_regn(iyr,2,iage,2,1:2,iregn))
		  CLTSTK(iyr,2,iage,iregn) = sum(TRK_19_regn(iyr,1,iage,1,1:2,iregn)) + sum(TRK_19_regn(iyr,2,iage,1,1:2,iregn))
	      do ifuel = 3,9   
			CLTSTK(iyr,ifuel,iage,iregn) = sum(TRK_19_regn(iyr,1,iage,ifuel,1:2,iregn)) + sum(TRK_19_regn(iyr,2,iage,ifuel,1:2,iregn))
	  	  enddo
	    enddo
!		Calculate national numbers as well
		do ifuel = 1,9
		  CLTSTK(n,ifuel,iage,11) = sum(CLTSTK(n,ifuel,iage,1:mnumcr-2))
		enddo
	  enddo
	endif

    CALL WR_FSHFLT

 RETURN
 END SUBROUTINE TRUCK_STOCK

! ==========================================================================================================
!...Subroutine TRUCK_VMT
!   Description:
!   	Projects truck vehicle miles travelled
! ==========================================================================================================
   SUBROUTINE TRUCK_VMT
   USE F_
   IMPLICIT NONE

!...Local variable dictionary
    REAL :: VMTADJ
    REAL :: TTONMI(MNUMYR,MNUMCR,SEC-2)	  !...ton-miles (billion) travelled by trucks
    REAL :: VMT_GR(MNUMYR,MNUMCR,SEC-2)   !...growth rate in truck VMT, calculated from truck ton-miles
    REAL :: TVMT(MNUMYR,MNUMCR,SEC-2)     !...truck VMT by census division and industrial sector
    REAL :: AGGVMTR                       !...aggregate HDV VMT
    REAL :: VMTADJR                       !...aggregate VMT adjustment factor
    REAL :: VMTSHRR(mnumyr,mnumcr)        !...VMT share by region
    REAL :: TOTVMTFLTR(MNUMYR,MNUMCR)
	INTEGER :: q

!...calculate regional heavy-duty truck vehicle miles travelled based on industrial output
!...first compute the static heavy-duty truck freight parameter (TTM_OUTPUT, ton-miles per $ of industrial output), based on last historical FAF year (iFAFyear),
!   and then estimate historical ton-miles b/w that last FAF year and the last historical NEMS tranfrt year (TTONMI).
    if ((iyr.ge.iFAFyear).and.(iyr.le.BSYR_VMT)) then
      do iregn=1,mnumcr-2
        do isec=1,sec-2
          if(iyr.eq.iFAFyear) then
            if(TSIC(isec,iregn,iFAFyear).gt.0.) then
              TTM_OUTPUT(iregn,isec)=TTONMI_ORIG(iregn,isec)/ &
              (TSIC(isec,iregn,iFAFyear)*0.001)
            else
              TTM_OUTPUT(iregn,isec)=0.
            endif
          endif
          TTONMI(iyr,iregn,isec)=TTM_OUTPUT(iregn,isec)*(TSIC(isec,iregn,iyr)*.001)
        enddo
      enddo
    endif
	
!...second compute heavy-duty truck vehicle miles travelled by census division
    do iregn=1,mnumcr-2
      if(iyr.le.BSYR_VMT)then
        VMTDMDR(iyr,iregn)=THIST_VMT(iyr,iregn)*1.0E9      !  Initialize VMT to history with input placeholder values
        if (iyr.ge.iFAFyear) then                           !  Calculate new FAF base year+ VMT by census division, using published national statistics
		  VMTDMDR(iyr,iregn)=(sum(THIST_VMT(iyr,1:mnumcr-2))* &
          (sum(TTONMI(iyr,iregn,1:sec-2))/ &
          (sum(TTONMI(iyr,1:mnumcr-2,1:sec-2)))))*1.0E9
        endif
      else
        do isec=1,sec-2
          if (iyr.eq.bsyr_vmt+1) then    ! Initialize base year truck VMT by census region and commodity group
            TVMT(bsyr_vmt,iregn,isec)=(sum(THIST_VMT(bsyr_vmt,1:mnumcr-2))* &
            (TTONMI(bsyr_vmt,iregn,isec)/ &
            sum(TTONMI(bsyr_vmt,1:mnumcr-2,1:sec-2))))*1.0E9
          endif
          TTONMI(iyr,iregn,isec)=TTM_OUTPUT(iregn,isec)*(TSIC(isec,iregn,iyr)/1000.)
          VMT_GR(iyr,iregn,isec)=TTONMI(iyr,iregn,isec)/TTONMI(iyr-1,iregn,isec)
          TVMT(iyr,iregn,isec)=TVMT(iyr-1,iregn,isec)*VMT_GR(iyr,iregn,isec)
        enddo
		VMTDMDR(iyr,iregn)=sum(TVMT(iyr,iregn,1:sec-2))
      endif
    enddo
    VMTDMDR(iyr,11)=sum(VMTDMDR(iyr,1:mnumcr-2))
	IF (iyr.eq.2050) THEN
	  WRITE(21,*)'check_faf_frt'
	  DO q = 21, MNUMYR
	    DO isec = 1, sec-2
		  WRITE(21,'(I4,", ",I3,", ",9(f9.2,", "))')q+1989,isec,TTONMI(iyr,1:9,isec)
		ENDDO
	  ENDDO
	  WRITE(21,*)'check_faf_frt2'
	  DO isec = 1, sec-2
		WRITE(21,'(I3,", ",9(f9.2,", "))')isec,TSIC(isec,1:9,iFAFyear)
      ENDDO
	ENDIF
	
!...adjust VMT per truck to match aggregate total
!...start by setting base year VMT per truck
    do ivoc = 1,voc
      ANNVMT(1,1:AGE,1:FUEL,ivoc)=VMTCLS3V(1:AGE,1:FUEL)
      ANNVMT(2,1:AGE,1:FUEL,ivoc)=VMTCLS46V(1:AGE,1:FUEL)
	enddo
    ANNVMT(3,1:AGE,1:FUEL,1)=VMTCLS78(1:AGE,1:FUEL)	          ! Tractor VMT	
    ANNVMT(3,1:AGE,1:FUEL,2)=VMTCLS78V(1:AGE,1:FUEL)          ! Vocation VMT
    if(curcalyr.ge.2013) then
	  ANNVMT(3,1:AGE,4,1)=VMTCLS78(1:AGE,1)
	  ANNVMT(3,1:AGE,4,2)=VMTCLS78V(1:AGE,1)
	endif
	do isc = 1,sc
	  do ifuel = 5,9
	    ANNVMT(isc,1:AGE,ifuel,1:voc) = ANNVMT(isc,1:AGE,2,1:voc) 
	  enddo
	enddo
	
!...aggregate total VMT by heavy-duty trucks
	AGGVMTR=0.    
    do isc = 1,3
        do iage = 1,age
            do ifuel = 1,fuel9	
                DO IFLT=1,FLT	  
                    if(curcalyr.le.2011) then					
                      AGGVMTR = AGGVMTR + SUM(ANNVMT(isc,iage,ifuel,1:voc))*TRKSTK(cur,isc,iage,ifuel,iflt)
					else
 	                  if(isc.eq.1) then 
					    AGGVMTR = AGGVMTR + ANNVMT(isc,iage,ifuel,1)*TRK_19_regn(iyr,3,iage,ifuel,iflt,11)
						AGGVMTR = AGGVMTR + ANNVMT(isc,iage,ifuel,2)*TRK_19_regn(iyr,4,iage,ifuel,iflt,11)
 	                  elseif(isc.eq.2) then
 	                    AGGVMTR = AGGVMTR + ANNVMT(isc,iage,ifuel,2)*sum(TRK_19_regn(iyr,5:7,iage,ifuel,iflt,11)) 
					  elseif(isc.eq.3) then
					    AGGVMTR = AGGVMTR + ANNVMT(isc,iage,ifuel,2)*(TRK_19_regn(iyr,8,iage,ifuel,iflt,11) + TRK_19_regn(iyr,12,iage,ifuel,iflt,11)) 
					    AGGVMTR = AGGVMTR + ANNVMT(isc,iage,ifuel,1)*(sum(TRK_19_regn(iyr,9:11,iage,ifuel,iflt,11)) + sum(TRK_19_regn(iyr,13:19,iage,ifuel,iflt,11)))
                      endif		  
                    endif
 			    enddo
		    enddo
		enddo
	enddo	
      VMTADJR=VMTDMDR(iyr,11)/AGGVMTR
    do isc = 1,3
        do iage = 1,age
            do ifuel = 1,fuel9	
				do ivoc = 1,voc			
                  ANNVMT(isc,iage,ifuel,ivoc) = ANNVMT(isc,iage,ifuel,ivoc)*VMTADJR	  
                enddo	
		    enddo
		enddo
	enddo		  
	  
    do isc = 1,3
        do iage = 1,age
            do ifuel = 1,fuel9	
                do IFLT=1,FLT	  
                    if(curcalyr.le.2011) then
                      VMTFLT(CUR,isc,iage,ifuel,IFLT) = sum(ANNVMT(isc,iage,ifuel,1:2)) * TRKSTK(cur,isc,iage,ifuel,iflt)
	                else
 	                  if(isc.eq.1) VMTFLT(CUR,isc,iage,ifuel,IFLT) = ANNVMT(isc,iage,ifuel,1)*TRK_19_regn(iyr,3,iage,ifuel,iflt,11) + &
					                      ANNVMT(isc,iage,ifuel,2)*TRK_19_regn(iyr,4,iage,ifuel,iflt,11)
 	                  if(isc.eq.2) VMTFLT(CUR,isc,iage,ifuel,IFLT) = ANNVMT(isc,iage,ifuel,2)*sum(TRK_19_regn(iyr,5:7,iage,ifuel,iflt,11))
 	                  if(isc.eq.3) VMTFLT(CUR,isc,iage,ifuel,IFLT) = & 
					                     ANNVMT(isc,iage,ifuel,1)*(sum(TRK_19_regn(iyr,9:11,iage,ifuel,iflt,11)) + sum(TRK_19_regn(iyr,13:19,iage,ifuel,iflt,11))) + &	
					                     ANNVMT(isc,iage,ifuel,2)*(TRK_19_regn(iyr,8,iage,ifuel,iflt,11) + TRK_19_regn(iyr,12,iage,ifuel,iflt,11)) 
                    endif
	            enddo
	        enddo
	    enddo
	enddo
	
    do iregn=1,mnumcr
      VMTSHRR(iyr,iregn)=VMTDMDR(iyr,iregn)/VMTDMDR(iyr,11)
      VMTFLTR(cur,1:SC,1:AGE,1:FUEL9,1:FLT,iregn) = VMTFLT(cur,1:SC,1:AGE,1:FUEL9,1:FLT) * VMTSHRR(iyr,iregn)
    enddo

!... Putting class 2b vehicles in the same format as class 3-8 vehicles

	do iage = 1,34
        if(curcalyr.le.2011) then
		  ANNVMT_19(1,1,:) = CLTVMTVA_H(n)
		  ANNVMT_19(2,1,:) = CLTVMTVA_H(n)	
    	elseif(curcalyr.le.2013) then
	      if(TRKSTK_19(iyr,1,iage,1).gt.0.0) ANNVMT_19(1,iage,1) = CLTVMT(iyr-1,2,iage)/TRKSTK_19(iyr,1,iage,1)*base_mkt_p1(1,1)        ! Class 2b pickup and van - diesel
	      if(TRKSTK_19(iyr,1,iage,2).gt.0.0) ANNVMT_19(1,iage,2) = CLTVMT(iyr-1,1,iage)/TRKSTK_19(iyr,1,iage,2)*base_mkt_p1(2,1)        ! Class 2b pickup and van - gasoline
	      if(TRKSTK_19(iyr,2,iage,1).gt.0.0) ANNVMT_19(2,iage,1) = CLTVMT(iyr-1,2,iage)/TRKSTK_19(iyr,2,iage,1)*base_mkt_p1(1,2)        ! Class 2b vocational - diesel
	      if(TRKSTK_19(iyr,2,iage,2).gt.0.0) ANNVMT_19(2,iage,2) = CLTVMT(iyr-1,1,iage)/TRKSTK_19(iyr,2,iage,2)*base_mkt_p1(2,2)        ! Class 2b vocational - gasoline
		    do ifuel = 3,6
		      if(TRKSTK_19(iyr,1,iage,ifuel).gt.0.0) ANNVMT_19(1,iage,ifuel) = CLTVMT(iyr-1,ifuel,iage)/TRKSTK_19(iyr,1,iage,ifuel)*base_mkt_p1(ifuel,1)    
			  if(TRKSTK_19(iyr,2,iage,ifuel).gt.0.0) ANNVMT_19(2,iage,ifuel) = CLTVMT(iyr-1,ifuel,iage)/TRKSTK_19(iyr,1,iage,ifuel)*base_mkt_p1(ifuel,2) 
		    enddo
	    else
	      if(TRKSTK_19(iyr,1,iage,1).gt.0.0) ANNVMT_19(1,iage,1) = CLTVMT(iyr-1,2,iage)/TRKSTK_19(iyr,1,iage,1)*base_mkt_p2(1,1)        ! Class 2b pickup and van - diesel
	      if(TRKSTK_19(iyr,1,iage,2).gt.0.0) ANNVMT_19(1,iage,2) = CLTVMT(iyr-1,1,iage)/TRKSTK_19(iyr,1,iage,2)*base_mkt_p2(2,1)        ! Class 2b pickup and van - gasoline
	      if(TRKSTK_19(iyr,2,iage,1).gt.0.0) ANNVMT_19(2,iage,1) = CLTVMT(iyr-1,2,iage)/TRKSTK_19(iyr,2,iage,1)*base_mkt_p2(1,2)        ! Class 2b vocational - diesel
	      if(TRKSTK_19(iyr,2,iage,2).gt.0.0) ANNVMT_19(2,iage,2) = CLTVMT(iyr-1,1,iage)/TRKSTK_19(iyr,2,iage,2)*base_mkt_p2(2,2)        ! Class 2b vocational - gasoline
		    do ifuel = 3,fuel9
		      if(TRKSTK_19(iyr,1,iage,ifuel).gt.0.0) ANNVMT_19(1,iage,ifuel) = CLTVMT(iyr-1,ifuel,iage)/TRKSTK_19(iyr,1,iage,ifuel)*base_mkt_p2(ifuel,1)    
			  if(TRKSTK_19(iyr,2,iage,ifuel).gt.0.0) ANNVMT_19(2,iage,ifuel) = CLTVMT(iyr-1,ifuel,iage)/TRKSTK_19(iyr,1,iage,ifuel)*base_mkt_p2(ifuel,2) 
		    enddo
	    endif
	enddo	  	
	
!... Splitting out across 19 different CAFE size classes
	 do iage=1,age
      do ifuel=1,fuel9
        ANNVMT_19(3,iage,ifuel) = ANNVMT(1,iage,ifuel,1)             ! Class 3 pickup and van
        ANNVMT_19(4,iage,ifuel) = ANNVMT(1,iage,ifuel,2)             ! Class 3 vocational	
        ANNVMT_19(5,iage,ifuel) = ANNVMT(2,iage,ifuel,2)             ! Class 4 vocational	
        ANNVMT_19(6,iage,ifuel) = ANNVMT(2,iage,ifuel,2)             ! Class 5 vocational	
        ANNVMT_19(7,iage,ifuel) = ANNVMT(2,iage,ifuel,2)             ! Class 6 vocational	
        ANNVMT_19(8,iage,ifuel) = ANNVMT(3,iage,ifuel,2)             ! Class 7 vocational	
		ANNVMT_19(9,iage,ifuel) = ANNVMT(3,iage,ifuel,1)             ! Class 7 tractor day low 
		ANNVMT_19(10,iage,ifuel) = ANNVMT(3,iage,ifuel,1)            ! Class 7 tractor day mid 
		ANNVMT_19(11,iage,ifuel) = ANNVMT(3,iage,ifuel,1)            ! Class 7 tractor day high 
		ANNVMT_19(12,iage,ifuel) = ANNVMT(3,iage,ifuel,2)            ! Class 8 vocational
		ANNVMT_19(13,iage,ifuel) = ANNVMT(3,iage,ifuel,1)            ! Class 8 tractor day low 
		ANNVMT_19(14,iage,ifuel) = ANNVMT(3,iage,ifuel,1)            ! Class 8 tractor day mid 
		ANNVMT_19(15,iage,ifuel) = ANNVMT(3,iage,ifuel,1)            ! Class 8 tractor day high 
		ANNVMT_19(16,iage,ifuel) = ANNVMT(3,iage,ifuel,1)            ! Class 8 tractor sleeper low 
		ANNVMT_19(17,iage,ifuel) = ANNVMT(3,iage,ifuel,1)            ! Class 8 tractor sleeper mid 
		ANNVMT_19(18,iage,ifuel) = ANNVMT(3,iage,ifuel,1)            ! Class 8 tractor sleeper high 
		ANNVMT_19(19,iage,ifuel) = ANNVMT(3,iage,ifuel,2)            ! Class 8 tractor heavy haul

!	   avmt_14(1,iage,ifuel) = max(ANNVMT_19(1,iage,ifuel),ANNVMT_19(3,iage,ifuel))                                                       ! Class 2b - 3 pickup and van
!	   avmt_14(2,iage,ifuel) = max(ANNVMT_19(2,iage,ifuel),ANNVMT_19(4,iage,ifuel),ANNVMT_19(5,iage,ifuel),ANNVMT_19(6,iage,ifuel))       ! Class 2b - 5 vocational
!	   avmt_14(3,iage,ifuel) = max(ANNVMT_19(7,iage,ifuel),ANNVMT_19(8,iage,ifuel))                                                       ! Class 6 - 7 vocational 
!	   avmt_14(4,iage,ifuel) = ANNVMT_19(9,iage,ifuel)
!	   avmt_14(5,iage,ifuel) = ANNVMT_19(10,iage,ifuel)
!	   avmt_14(6,iage,ifuel) = ANNVMT_19(11,iage,ifuel)
!	   avmt_14(7,iage,ifuel) = ANNVMT_19(12,iage,ifuel)
!	   avmt_14(8,iage,ifuel) = ANNVMT_19(13,iage,ifuel)
!	   avmt_14(9,iage,ifuel) = ANNVMT_19(14,iage,ifuel)
!	   avmt_14(10,iage,ifuel) = ANNVMT_19(15,iage,ifuel)
!	   avmt_14(11,iage,ifuel) = ANNVMT_19(16,iage,ifuel)
!	   avmt_14(12,iage,ifuel) = ANNVMT_19(17,iage,ifuel)
!	   avmt_14(13,iage,ifuel) = ANNVMT_19(18,iage,ifuel)
!	   avmt_14(14,iage,ifuel) = ANNVMT_19(19,iage,ifuel)	   
   	  enddo
	 enddo	

	 
!... Total VMT by 19 different CAFE size classes
	 do iage=1,age
      do ifuel=1,fuel9 
	   do icafe19 = 1,cafe19
        VMTFLT_19(iyr,icafe19,iage,ifuel) = TRKSTK_19(iyr,icafe19,iage,ifuel)*ANNVMT_19(icafe19,iage,ifuel) 
	   enddo
!	! Class 2b - 3 pickup and van
!	   vmt_14(iyr,1,iage,ifuel) = max(VMTFLT_19(iyr,1,iage,ifuel),VMTFLT_19(iyr,3,iage,ifuel))
!	! Class 2b - 5 vocational
!	   vmt_14(iyr,2,iage,ifuel) = max(VMTFLT_19(iyr,2,iage,ifuel),VMTFLT_19(iyr,4,iage,ifuel),VMTFLT_19(iyr,5,iage,ifuel),VMTFLT_19(iyr,6,iage,ifuel))
!	! Class 6 - 7 vocational
!	   vmt_14(iyr,3,iage,ifuel) = max(VMTFLT_19(iyr,7,iage,ifuel),VMTFLT_19(iyr,8,iage,ifuel))
!	   vmt_14(iyr,4,iage,ifuel) = VMTFLT_19(iyr,9,iage,ifuel)
!	   vmt_14(iyr,5,iage,ifuel) = VMTFLT_19(iyr,10,iage,ifuel)
!	   vmt_14(iyr,6,iage,ifuel) = VMTFLT_19(iyr,11,iage,ifuel)
!	   vmt_14(iyr,7,iage,ifuel) = VMTFLT_19(iyr,12,iage,ifuel)
!	   vmt_14(iyr,8,iage,ifuel) = VMTFLT_19(iyr,13,iage,ifuel)
!	   vmt_14(iyr,9,iage,ifuel) = VMTFLT_19(iyr,14,iage,ifuel)
!	   vmt_14(iyr,10,iage,ifuel) = VMTFLT_19(iyr,15,iage,ifuel)
!	   vmt_14(iyr,11,iage,ifuel) = VMTFLT_19(iyr,16,iage,ifuel)
!	   vmt_14(iyr,12,iage,ifuel) = VMTFLT_19(iyr,17,iage,ifuel)
!	   vmt_14(iyr,13,iage,ifuel) = VMTFLT_19(iyr,18,iage,ifuel)
!	   vmt_14(iyr,14,iage,ifuel) = VMTFLT_19(iyr,19,iage,ifuel)	  	   
      enddo
	 enddo			
	
   RETURN
   END SUBROUTINE TRUCK_VMT

!=============================================================================================================
   SUBROUTINE TRUCK_FUEL
   USE F_
   IMPLICIT NONE
   INCLUDE 'NGTDMOUT'

   REAL VMTFLTR_CAV_SHR(CUR,SC,AGE,FUEL9,FLT,MNUMCR),HDV_MPG_CAV_ADJ(SC,AGE,FUEL9),VMT_CAV_ELIG
   PARAMETER (VMT_CAV_ELIG =.656)

   HDV_MPG_CAV_ADJ=0.
   TOONSHR(1:TOONYEAR-1)=0.
   VMTFLTR_CAV_SHR=0.
   if(curiyr.ge.TOONYEAR.and.curiyr.le.lastyr) then
     do iage=1,curiyr-toonyear+1
       ISC=3
       IFLT=2
       VMTFLTR_CAV_SHR(CUR,ISC,iage,:,IFLT,:)=TOONSHR(curiyr)
     enddo
     HDV_MPG_CAV_ADJ(ISC,:,:)=.07 ! Operational energy savings from platooning, not fuel economy improvement
   endif

!   if(curitr.eq.maxitr+1.and.curiyr.ge.TOONYEAR-1) then
!     do ifuel=1,FUEL9
!       do iage=1,age
!         write(21,'(a,3(",",1x,i4),1(",",1x,f15.3),9(",",1x,f10.8))') 'msi,VMTFLTR_CAV_SHR',curiyr,ifuel,iage,sum(VMTFLTR(CUR,3,:,IFUEL,2,:)), &
!		                                                                   (VMTFLTR_CAV_SHR(CUR,3,IAGE,IFUEL,2,iregn),iregn=1,MNUMCR-2)
!       enddo
!     enddo
!   endif
   
   VMTFLTR_CAV_SHR= VMTFLTR_CAV_SHR*VMT_CAV_ELIG
   
!...Calculate fuel demand from VMT and MPG by size class, fuel and fleet/nonfleet, by census division
    do ISC=1,SC
      do IFLT=1,FLT
        do iregn=1,mnumcr-2
          do IFUEL=1,FUEL9
            FUELDMDR(ISC,IFUEL,IFLT,iregn)=0.
            DO IAGE=1,AGE
!              if(HDV_MPG(iyr,ISC,IAGE,IFUEL).gt.0.0) FUELDMDR(ISC,IFUEL,IFLT,iregn) =  FUELDMDR(ISC,IFUEL,IFLT,iregn) + & 
!			        VMTFLTR(CUR,ISC,IAGE,IFUEL,IFLT,iregn)/(HDV_MPG(iyr,ISC,IAGE,IFUEL))
              if(HDV_MPG(iyr,ISC,IAGE,IFUEL).gt.0.0) FUELDMDR(ISC,IFUEL,IFLT,iregn) =  FUELDMDR(ISC,IFUEL,IFLT,iregn) + &
                    ((1.- VMTFLTR_CAV_SHR(CUR,ISC,IAGE,IFUEL,IFLT,iregn))*VMTFLTR(CUR,ISC,IAGE,IFUEL,IFLT,iregn)/(HDV_MPG(iyr,ISC,IAGE,IFUEL))) + & 
                    (VMTFLTR_CAV_SHR(CUR,ISC,IAGE,IFUEL,IFLT,iregn))*VMTFLTR(CUR,ISC,IAGE,IFUEL,IFLT,iregn)/(HDV_MPG(iyr,ISC,IAGE,IFUEL)/(1.-HDV_MPG_CAV_ADJ(ISC,IAGE,IFUEL)))
            ENDDO
!            FUELBTUR(ISC,IFUEL,IFLT,iregn) = FUELDMDR(ISC,IFUEL,IFLT,iregn)*HRATE(isc,ifuel)*(1E-12)                                               ! TRILLION BTU
          enddo
!... Diesel		  
		  FUELBTUR(ISC,1,IFLT,iregn) = FUELDMDR(ISC,1,IFLT,iregn)*HRATE(isc,1)*(1E-12) + &
		                               FUELDMDR(ISC,7,IFLT,iregn)*HRATE(isc,7)*(1E-12)*(1.0-PctPHEV)
!... Gasoline	  
		  FUELBTUR(ISC,2,IFLT,iregn) = FUELDMDR(ISC,2,IFLT,iregn)*HRATE(isc,2)*(1E-12) + &
		                               FUELDMDR(ISC,5,IFLT,iregn)*HRATE(isc,5)*(1E-12)*(1.0-PCTAF(2,iregn,iyr)) + &
		                               FUELDMDR(ISC,8,IFLT,iregn)*HRATE(isc,8)*(1E-12)*(1.0-PctPHEV)	
!... LPG
		  FUELBTUR(ISC,3,IFLT,iregn) = FUELDMDR(ISC,3,IFLT,iregn)*HRATE(isc,3)*(1E-12)	
!... Natural Gas
		  FUELBTUR(ISC,4,IFLT,iregn) = FUELDMDR(ISC,4,IFLT,iregn)*HRATE(isc,4)*(1E-12)
!... Ethanol		   
		  FUELBTUR(ISC,5,IFLT,iregn) = FUELDMDR(ISC,5,IFLT,iregn)*HRATE(isc,5)*(1E-12)*PCTAF(2,iregn,iyr)
!... Electric
          FUELBTUR(ISC,6,IFLT,iregn) = FUELDMDR(ISC,6,IFLT,iregn)*HRATE(isc,6)*(1E-12) + &
		                               FUELDMDR(ISC,7,IFLT,iregn)*HRATE(isc,7)*(1E-12)*PctPHEV + &
		                               FUELDMDR(ISC,8,IFLT,iregn)*HRATE(isc,8)*(1E-12)*PctPHEV	
!... Hydrogen
          FUELBTUR(ISC,7,IFLT,iregn) = FUELDMDR(ISC,9,IFLT,iregn)*HRATE(isc,9)*(1E-12)
        enddo	
		do ifuel = 1,7
          FUELBTUR(isc,ifuel,iflt,11) = sum(FUELBTUR(isc,ifuel,iflt,1:mnumcr-2))
        enddo
      enddo
    enddo

	
!... The following variable is used in tran.f to calculate benchmark factors
    do ISC=1,SC
      do ifuel=1,7
        do iregn=1,mnumcr-2
          TFRBTU_F_T(IYR,ISC,ifuel,iregn) = sum(FUELBTUR(ISC,ifuel,1:FLT,iregn))
        enddo
        TFRBTU_F_T(iyr,isc,ifuel,11) = sum(TFRBTU_F_T(iyr,isc,ifuel,1:mnumcr-2))
      enddo
    enddo


!...QGFTRFV: CNG central refueling (class 3 and class 4-6)
!...QGFTRPV: CNG retail purchase (class 3 and class 4-6)
!...QGLTRFV: LNG central refueling (class 7-8)
!...QGLTRPV: LNG retail refueling (class 7-8)
    do iregn=1,mnumcr
      QGFTRPV(iregn,iyr) = sum(FUELBTUR(1:2,4,nft,iregn))
      QGFTRFV(iregn,iyr) = sum(FUELBTUR(1:2,4,flt,iregn))
      QGLTRPV(iregn,iyr) = FUELBTUR(3,4,nft,iregn)
      QGLTRFV(iregn,iyr) = FUELBTUR(3,4,flt,iregn)
    enddo

   RETURN
   END SUBROUTINE TRUCK_FUEL

!=============================================================================================================
   SUBROUTINE WR_FSHFLT
   USE F_
   IMPLICIT NONE

     CHARACTER*11 SIZECLASS(SC)/'Class 3','Classes 4-6','Classes 7-8'/
     CHARACTER*8  FLEET(FLT)/'NonFleet','Fleet'/
     integer i

!...Calculate fuel shares of the entire truck stock, excluding new trucks, for comparison
!...with the fuel shares assigned in subroutine CFFSHR
    Fuel_Shr_Stk=0.
    DO IAGE=1,AGE
      DO  IFLT=1,FLT
        DO ISC=1,SC
          SUMOVERFUELS=SUM(TRKSTK(cur,ISC,IAGE,1:FUEL9,iFLT))
          IF(SUMOVERFUELS.GT.0.) THEN
            Fuel_Shr_Stk(ISC,1:FUEL9,IFLT)=TRKSTK(cur,ISC,IAGE,1:FUEL9,iFLT)/SUMOVERFUELS
          ENDIF
        ENDDO
      ENDDO
    ENDDO

   RETURN
   END SUBROUTINE WR_FSHFLT

! ==========================================================================================================
!...Subroutine INIT
!   Description:
!   	Initializes freight module by calling read input subroutine and sets important variables such as &
!       industrial output and fuel prices
! ==========================================================================================================
  SUBROUTINE INIT
  USE F_
  IMPLICIT NONE
  include 'angtdm'
  INCLUDE 'EUSPRC'

!...Local variable dictionary
    INTEGER, PARAMETER :: US = 11         !...National level parameter (census division 11 = US)
    INTEGER :: itsic, itsicyr             !...Industry indexing
    LOGICAL done_once/.false./            !...Logic switch to ensure action done only one time
	REAL TG_MPG, TE_MPG

!...Call read input file and set variable (do only one a run)
    if(.not.done_once)then
      done_once=.true.
      CALL CFREAD
	  CALL CFREADSTOCK
    endif

!...Summarize economic output into sectors and calculate growth rate in output
!...1) basic chemicals                 2) primary metals
!...3) processed food                  4) paper products
!...5) petroleum products              6) stone, clay, glass, and concrete
!...7) metal durables, less computers  8) other manufacturing, unless listed
!...9) agriculture                     10) mining
!...11) alcohol (beverage) and tobacco 12) pharma
!...13) fertilizers                    14) rubber and plastics
!...15) computers                      16) furniture
!...17) utility                        18) goverment

    TSIC(:,:,iyr)=0.
    do iregn=1,mnumcr
      TSIC(1,iregn,iyr) =sum(TMC_REVIND(iregn,15:20,iyr)) - &
                         TMC_REVIND(iregn,17,iyr) - &
                         TMC_REVIND(iregn,19,iyr) - &
                         TMC_REVIND(iregn,21,iyr)
      TSIC(2,iregn,iyr) =sum(TMC_REVIND(iregn,33:35,iyr))
      TSIC(3,iregn,iyr) =sum(TMC_REVIND(iregn,2:5,iyr))
      TSIC(4,iregn,iyr) =sum(TMC_REVIND(iregn,11:13,iyr))
      TSIC(5,iregn,iyr) =sum(TMC_REVIND(iregn,25:26,iyr))
      TSIC(6,iregn,iyr) =TMC_REVIND(iregn,28,iyr) + &
                         TMC_REVIND(iregn,30,iyr) + &
                         TMC_REVIND(iregn,32,iyr)
      TSIC(7,iregn,iyr) =sum(TMC_REVIND(iregn,36:40,iyr)) - &
                         TMC_REVIND(iregn,38,iyr)
      TSIC(8,iregn,iyr) =sum(TMC_REVIND(iregn,7:8,iyr)) + &
                         TMC_REVIND(iregn,14,iyr) + &
                         TMC_REVIND(iregn,41,iyr)
      TSIC(9,iregn,iyr) =sum(TMC_REVIND(iregn,42:44,iyr))
      TSIC(10,iregn,iyr)=sum(TMC_REVIND(iregn,45:47,iyr))
      TSIC(11,iregn,iyr)=TMC_REVIND(iregn,6,iyr)
      TSIC(12,iregn,iyr)=TMC_REVIND(iregn,21,iyr)
      TSIC(13,iregn,iyr)=TMC_REVIND(iregn,19,iyr)
      TSIC(14,iregn,iyr)=TMC_REVIND(iregn,27,iyr)
      TSIC(15,iregn,iyr)=TMC_REVIND(iregn,38,iyr)
      TSIC(16,iregn,iyr)=TMC_REVIND(iregn,9,iyr)
      TSIC(17,iregn,iyr)=sum(MC_REVSER(iregn,3:4,iyr))
      TSIC(18,iregn,iyr)=MC_REVSER(iregn,10,iyr)
    enddo
	
! ... Calculate the MPG weighted VMT shares for PHEVS. Since the MPG for the gasoline engine and the electric
! ... motor are very different we need to weight the VMT shares with the MPGs.
! ... The values we have here from user input are the electric fraction of the total VMT (PHEV10ElecVMT) and the
! ... ratio of the electric MPG to the gasoline MPG (EG_MPG). The inputs are national level, but we calculate
! ... the result at a regional level to be consistent with the others (all the regions have the same value).
! ... Calculate the ratio of the total MPG to the gasoline MPG, TG_MPG.
      TG_MPG = EG_MPG/(PHEVElecVMT + EG_MPG*(1.0 - PHEVElecVMT))
! ... Calculate the ratio of the total MPG to the electric MPG, TE_MPG.
      TE_MPG = TG_MPG/EG_MPG
! ... Using that result, calculate the electric VMT adjusted for MPG, PctPHEV.
      PctPHEV = PHEVElecVMT*TE_MPG
	  PctPHEV_HDV = PctPHEV             ! Passes through to tran.f

!...Set average fuel prices and turn from 1987$/mmbtu to 1990$/mmbtu
    do iregn = 1,mnumcr
     Fuel_$_regn(iyr,1,iregn) = PDSTR(iregn,IYR)*1.115                                                                             ! Diesel
     Fuel_$_regn(iyr,2,iregn) = PMGTR(iregn,IYR)*1.115                                                                             ! Gasoline
     Fuel_$_regn(iyr,3,iregn) = PLGTR(iregn,IYR)*1.115                                                                             ! LPG
     Fuel_$_regn(iyr,4,iregn) = (PGFTRFV(iregn,IYR) + PGFTRPV(iregn,IYR) + PGLTRFV(iregn,IYR) + PGLTRPV(iregn,IYR))/4 * 1.115      ! CNG/LNG
	 Fuel_$_regn(iyr,5,iregn) = MIN(PETTR(iregn,IYR),PMGTR(iregn,IYR)) * 1.115                                                     ! Flex fuel
	 Fuel_$_regn(iyr,6,iregn) = PELVHTR(iregn,IYR) * 1.115                                                                         ! Electric
	 Fuel_$_regn(iyr,7,iregn) = (PctPHEV*PELVHTR(iregn,IYR) + (1.0-PctPHEV)*PDSTR(iregn,IYR)) * 1.115                              ! PHEV Diesel
	 Fuel_$_regn(iyr,8,iregn) = (PctPHEV*PELVHTR(iregn,IYR) + (1.0-PctPHEV)*PMGTR(iregn,IYR)) * 1.115                              ! PHEV Gasoline
	 Fuel_$_regn(iyr,9,iregn) = PRICE_HY(iregn,IYR) * 1.115                                                                        ! Hydrogen
    enddo

!...Set average fuel prices and turn from 1987$/mmbtu to 1990$/mmbtu
     Fuel_$(iyr,1) = PDSTR(US,IYR)*1.115                                                                                           ! Diesel
     Fuel_$(iyr,2) = PMGTR(US,IYR)*1.115                                                                                           ! Gasoline
     Fuel_$(iyr,3) = PLGTR(US,IYR)*1.115                                                                                           ! LPG
     Fuel_$(iyr,4) = (PGFTRFV(US,IYR) + PGFTRPV(US,IYR) + PGLTRFV(US,IYR) + PGLTRPV(US,IYR))/4 * 1.115                             ! CNG/LPG
	 Fuel_$(iyr,5) = MIN(PETTR(US,IYR),PMGTR(US,IYR)) * 1.115                                                                      ! Flex fuel
	 Fuel_$(iyr,6) = PELVHTR(US,IYR) * 1.115                                                                                       ! Electric
	 Fuel_$(iyr,7) = (PctPHEV*PELVHTR(US,IYR) + (1.0-PctPHEV)*PDSTR(US,IYR)) * 1.115                                               ! PHEV Diesel
	 Fuel_$(iyr,8) = (PctPHEV*PELVHTR(US,IYR) + (1.0-PctPHEV)*PMGTR(US,IYR)) * 1.115                                               ! PHEV Gasoline
	 Fuel_$(iyr,9) = PRICE_HY(US,IYR) * 1.115                                                                                      ! Hydrogen

   RETURN
   END SUBROUTINE INIT

!=============================================================================================================
   SUBROUTINE CFREAD
   USE F_
   IMPLICIT NONE

!...Reads input for the freight model from spreadsheet input file
    REAL*4 TEMPR_S(SC)
    REAL*4 TEMPR_A_S(AGE,SC4)
    INTEGER*2 IT,IV, J, I, INOTE
    LOGICAL NEW
    CHARACTER*18 FNAME
    INTEGER FILE_MGR
    EXTERNAL FILE_MGR
    CHARACTER*16 RNAME
    INTEGER WKUNIT,SYSUNIT

!...call subroutine to read defined ranges from spreadsheet
    FNAME='TRNHDVX'
    WKUNIT = FILE_MGR('O',FNAME,NEW) ! open trnhdvx.xlsx input file
    CALL ReadRngXLSX(WKUNIT,'trnhdv') ! read range names & corresponding data from worksheet "trnhdv"

!...fuel economy and market share for ICAFE categories
	CALL GETRNGR('BASE_MKT_P1     ',BASE_MKT_P1,9,CAFE19,1)
	CALL GETRNGR('BREAKOUT_46     ',BREAKOUT_46(1:6,1:3,1:34),6,3,AGE)
	CALL GETRNGR('BREAKOUT_78     ',BREAKOUT_78(1:6,1:2,1:34),6,2,AGE)

!   Connected and Autonomous freight assumptions
    CALL GETRNGI('TOONYEAR        ',TOONYEAR,1,1,1)
    CALL GETRNGR('TOONSHR         ',TOONSHR(TOONYEAR-1:MNUMYR),1,MNUMYR-TOONYEAR+2,1)    

    CALL GETRNGR('DISCRTXG        ',DISCRTXG,1,1,1)

    CALL GETRNGR('DISCRTXGL       ',DISCRTXGL,1,1,1)
    CALL GETRNGR('PRAFDFXG        ',PRAFDFXG,SC4,9,1)

    CALL GETRNGI('CYAFVXGF        ',CYAFVXG(1,1,FLT),SC4,9,1)
    CALL GETRNGI('CYAFVXGN        ',CYAFVXG(1,1,NFT),SC4,9,1)

    CALL GETRNGI('TRGSHXGF        ',TRGSHXG(1,1,FLT),CAFE19,9,1)
    CALL GETRNGI('TRGSHXGN        ',TRGSHXG(1,1,NFT),CAFE19,9,1)

    CALL GETRNGR('BFSHXGF         ',BFSHXG(1,1,FLT),SC4,9,1)
    CALL GETRNGR('BFSHXGN         ',BFSHXG(1,1,NFT),SC4,9,1)

    CALL GETRNGR('VMT_VEH_2       ',VMT_VEH(1,1,4),11,FLT,1)
    CALL GETRNGR('VMT_VEH_3       ',VMT_VEH(1,1,1),11,FLT,1)
    CALL GETRNGR('VMT_VEH_46      ',VMT_VEH(1,1,2),11,FLT,1)
    CALL GETRNGR('VMT_VEH_78      ',VMT_VEH(1,1,3),11,FLT,1)
    CALL GETRNGR('VEH_SHR_2       ',VEH_SHR(1,1,4),11,FLT,1)
    CALL GETRNGR('VEH_SHR_3       ',VEH_SHR(1,1,1),11,FLT,1)
    CALL GETRNGR('VEH_SHR_46      ',VEH_SHR(1,1,2),11,FLT,1)
    CALL GETRNGR('VEH_SHR_78      ',VEH_SHR(1,1,3),11,FLT,1)

    CALL GETRNGR('EFSHXGF         ',EFSHXG(1,1,FLT),SC4,9,1)
    CALL GETRNGR('EFSHXGN         ',EFSHXG(1,1,NFT),SC4,9,1)

!...natural gas vehicle sensitivity case
    CALL GETRNGR('CSTDXG          ',CSTDXG,SC4,FLT,1)
    CALL GETRNGR('CSTDVXG         ',CSTDVXG,SC4,FLT,1)

!...Share of vehicle stocks in fleets
    CALL GETRNGR('FLEETSHR        ',FLEETSHR,SC4,1,1)
	
!... Share of PHEV EV VMT Share
    CALL GETRNGR('PHEVELECVMT     ',PHEVELECVMT,1,1,1)	
	CALL GETRNGR('EG_MPG          ',EG_MPG,1,1,1)

!...VMT PER TRUCK BY FUEL AND VINTAGE
    CALL GETRNGR('VMTCLS3V        ',VMTCLS3V,AGE,4,1)
    CALL GETRNGR('VMTCLS46V       ',VMTCLS46V,AGE,4,1)
    CALL GETRNGR('VMTCLS78V       ',VMTCLS78V,AGE,4,1)
    CALL GETRNGR('VMTCLS78        ',VMTCLS78,AGE,4,1)	

!...NEW TRUCK SALES, CLASS 4-6 SHARE OF CLASS 4-8 BY YEAR
    CALL GETRNGR('NEWCLS46        ',NEWCLS46,MNUMYR,1,1)

!...Scrappage rates for diesel and all other fuels	
    CALL GETRNGR('SCRAP_D         ',SCRAP_D,AGE,SC4,1)
	CALL GETRNGR('SCRAP           ',SCRAP,AGE,SC4,1)
	CALL GETRNGR('TFFXGRT         ',TEMPR_A_S,AGE,SC4,1)
    DO ISC = 1, SC4
      DO IAGE = 1, AGE
        SCRAP_RATE(ISC,IAGE,1) = SCRAP_D(IAGE,ISC)
        SCRAP_RATE(ISC,IAGE,2:9) = SCRAP(IAGE,ISC)
		TFFXGRT(ISC,IAGE) = TEMPR_A_S(IAGE,ISC)
      ENDDO
    ENDDO

!...VMT ASSUMPTIONS

    CALL GETRNGR('THIST_VMT       ',THIST_VMT (6:BSYR_VMT,1:9),BSYR_VMT-6+1,MNUMCR-2,1)
    CALL GETRNGR('TTONMI_ORIG     ',TTONMI_ORIG (1:9,1:16),MNUMCR-2,SEC-2,1)

!...Historic MPG values by age and fuel
    CALL GETRNGR('BMPGSTK2B       ',BMPGSTK2B (1:AGE,IY:BSYR_STK-BASEYR+1,1:6),AGE,BSYR_STK-1995+1,6)
    CALL GETRNGR('BMPGSTK3        ',BMPGSTK3 (1:AGE,IY:BSYR_STK-BASEYR+1,1:6),AGE,BSYR_STK-1995+1,6)
    CALL GETRNGR('BMPGSTK46       ',BMPGSTK46(1:AGE,IY:BSYR_STK-BASEYR+1,1:6),AGE,BSYR_STK-1995+1,6)
    CALL GETRNGR('BMPGSTK78       ',BMPGSTK78(1:AGE,IY:BSYR_STK-BASEYR+1,1:6),AGE,BSYR_STK-1995+1,6)

!...Waterborne Freight Model values for international and domestic marine
    CALL GETRNGR('DOMSHIP_FUEL_SHR',DOMSHIP_FUEL_SHR(1:4,IY:IJUMPYR),4,num_to_read,1)
    CALL GETRNGR('INTSHIP_FUEL_SHR',INTSHIP_FUEL_SHR(1:5,IY:IJUMPYR),5,num_to_read,1)
    CALL GETRNGR('INTSHIPLOGIT_FAC',INTSHIPLOGIT_FAC(1:5,1:3),5,3,1)
    CALL GETRNGR('LSFORISK        ',LSFORISK(IY:IJUMPYR),Num_to_read,1,1)
    CALL GETRNGI('SHIPHISTYR      ',SHIPHISTYR,1,1,1)
    CALL GETRNGR('DSHIST_TONMI    ',DSHIST_TONMI(iy:SHIPHISTYR,1:9),SHIPHISTYR-iy+1,MNUMCR-2,1)
    CALL GETRNGR('DTM_SHARES       ',DTM_SHARES (1:9,1:16),MNUMCR-2,SEC-2,1)

    CALL GETRNGR('ANN_DECLINE     ',ANN_DECLINE(iy:IJUMPYR),Num_to_read,1,1)
    CALL GETRNGR('DSEFF           ',DSEFF(iy:IJUMPYR),Num_to_read,1,1)
    CALL GETRNGR('HTDSEFF         ',HTDSEFF(iy:IJUMPYR),Num_to_read,1,1)

    CALL GETRNGR('FUELCONS        ',FUELCONS,MNUMCR-2,9,1)
    CALL GETRNGR('FLEETTO         ',FLEETTO,1,9,1)
    CALL GETRNGR('EFFINC          ',EFFINC,1,9,1)

	CLOSE(WKUNIT)
	FNAME='TRNHDVX'
    WKUNIT = FILE_MGR('O',FNAME,NEW) ! open trnhdvx.xlsx input file

!...Reading in input data for Phase 2 HDV GHG emissions regulations
    CALL ReadRngXLSX(WKUNIT,'Phase_2') ! read range names & corresponding data from worksheet "phase_2"
    WKUNIT = FILE_MGR('C',FNAME,NEW)  ! close trnhdvx.xlsx input file
    CLOSE(WKUNIT)	
    CALL GETRNGR('BASE_MPG_P2     ',BASE_MPG_P2,9,19,1)
    CALL GETRNGR('BASE_MKT_P2     ',BASE_MKT_P2,9,19,1)
    CALL GETRNGI('TECHFYR_P2      ',TECHFYR_P2(1:83,1:14,1:9),TECHP2,CAFEP2,FUEL9)
    CALL GETRNGI('PAYBACK_P2      ',PAYBACK_P2,1,CAFEP2,1)

    CALL GETRNGR('TECHEFF_P2      ',TECHEFF_P2,TECHP2,CAFEP2,1)
    CALL GETRNGR('TECHCOST_P2     ',TECHCOST_P2,TECHP2,CAFEP2,1)

    CALL GETRNGR('TECHVAR_P2      ',TECHVAR_P2,TECHP2,CAFEP2,1)
    CALL GETRNGR('TECHMID_P2      ',TECHMID_P2,TECHP2,CAFEP2,1)
    CALL GETRNGR('TECHSHAPE_P2    ',TECHSHAPE_P2,TECHP2,CAFEP2,1)
    CALL GETRNGI('TECHBASE_P2     ',TECHBASE_P2(1:83,1:19,1:9),TECHP2,CAFE19,FUEL9)

    CALL GETRNGR('TECHMAX_P2      ',TECHMAX_P2,TECHP2,CAFEP2,1)
    CALL GETRNGR('HDV_STANDARD_P2 ',HDV_STANDARD_P2(1:9,28:61,1:14),FUEL9,34,CAFEP2)
	CALL GETRNGI('SUPERSCEDE_P2   ',SUPERSCEDE_P2,1,1,TECHP2)
	CALL GETRNGI('TECH_SUP        ',TECH_SUP,TECHP2,9,1)
	CALL GETRNGI('SYNERGY_P2      ',SYNERGY_P2,TECHP2,1,1)
	CALL GETRNGI('TECH_SYN        ',TECH_SYN,TECHP2,1,1)
	CALL GETRNGR('TECH_PER        ',TECH_PER,TECHP2,1,1)
	CALL GETRNGI('REQUIRED_P2     ',REQUIRED_P2,TECHP2,1,1)
	CALL GETRNGI('TECH_REQ        ',TECH_REQ,TECHP2,2,1)
	CALL GETRNGI('REQUIRED_ANY_P2 ',REQUIRED_ANY_P2,TECHP2,1,1)
	CALL GETRNGI('TECH_R_ANY      ',TECH_R_ANY,TECHP2,3,1)
	CALL GETRNGI('TECH_NUM        ',TECH_NUM,TECHP2,1,1)
	CALL GETRNGR('LEARNING_P2     ',LEARNING_P2(1:14,25:61),14,37,1)
	CALL GETRNGI('TECHLEARN_P2    ',TECHLEARN_P2,TECHP2,CAFEP2,1)

    CALL GETRNGR('DISCRTXG_P2     ',DISCRTXG_P2,1,1,1)
    CALL GETRNGR('DISCRTXGL_P2    ',DISCRTXGL_P2,1,1,1)

    CALL GETRNGR('FE2_5           ',FE25,1,1,1)
    CALL GETRNGR('FE6_7           ',FE67,1,1,1)
    CALL GETRNGR('FE8_            ',FE8,1,1,1)
    CALL GETRNGR('ADJ2_           ',ADJ2,1,1,1)
    CALL GETRNGR('ADJ4_5          ',ADJ45,1,1,1)	
	

!... For Phase 2
	TECHCOST_P2(:,:) = TECHCOST_P2(:,:)/MC_JPGDP(23)*MC_JPGDP(1)              ! Convert tech costs from 2012$ to 1990$
	
	 num_sup = 0
	 num_req = 0.
	 num_syn = 0.
    do itechp2 = 1,techp2	 
     if(SUPERSCEDE_P2(itechp2).eq.1) then 	                                  ! Setup parameters for SUPERSEDES notes 
	   num_sup = num_sup + 1
       SUPERSEDES_TECH(1,num_sup) = TECH_NUM(itechp2)
	   SUPERSEDES_tech(2,num_sup) = tech_sup(itechp2,1)
	   tech_cnt_sup(num_sup) = 2
      do j = 2,9
	    if(tech_sup(itechp2,j).eq.0) cycle
	    tech_cnt_sup(num_sup) = tech_cnt_sup(num_sup) + 1
        SUPERSEDES_tech(tech_cnt_sup(num_sup),num_sup) = tech_sup(itechp2,j)
      enddo 
	 endif
	  
     if(REQUIRED_P2(itechp2).eq.1) then                                     ! Setup parameters for REQUIRED notes
	   num_req = num_req +1
	   REQUIRES_tech(1,num_req) = TECH_NUM(itechp2)
       REQUIRES_tech(2,num_req) = TECH_REQ(itechp2,1)
	   tech_cnt_req(num_req) = 2
      do j = 2,3
	    if(tech_sup(itechp2,j).eq.0) cycle
	    tech_cnt_req(num_req) = tech_cnt_req(num_req) + 1
        REQUIRES_tech(tech_cnt_req(num_req),num_req) = TECH_REQ(itechp2,j)
      enddo 
	 endif	
	 
     if(SYNERGY_P2(itechp2).eq.1) then                                      ! Setup parameters for SYNERGY notes
	   num_syn = num_syn + 1
       SYNERGY_tech(1,num_syn) = tech_num(itechp2)
	   SYNERGY_tech(2,num_syn) = tech_syn(itechp2,1)
	   syn_per(2,num_syn) = tech_per(itechp2,1)
	 endif
	enddo            ! End itechp2 loop	
	
2     format(1x,'Read Spreadsheet Range: ',a)
100   format(1x,6I9)
200   format(1x,6f10.3)

  RETURN
  END SUBROUTINE CFREAD


!=============================================================================================================
   SUBROUTINE CFREADSTOCK
   USE F_
   IMPLICIT NONE

    LOGICAL       NEW
    CHARACTER*18  FNAME
    INTEGER       FILE_MGR
    EXTERNAL      FILE_MGR
    CHARACTER*16  RNAME
    INTEGER       WKUNIT,SYSUNIT
	INTEGER       j2, y2, YearsReadIn
    INTEGER*2     m2, r2, a2, f2
    Integer*2    HDV_CENSUS(1836), HDV_FUEL(1836), HDV_VINTAGE(1836)
	PARAMETER(YearsReadIn = bsyr_stk - 2011)
	REAL CLS2bSTKREGN(1836,YearsReadIn)          ! Class 2b stock by region                
    REAL CLS2bVSTKREGN(1836,YearsReadIn)         ! Class 2b vocational stock by region     
    REAL CLS3STKREGN(1836,YearsReadIn)           ! Class 3 stock by region                 
    REAL CLS3VSTKREGN(1836,YearsReadIn)          ! Class 3 vocational stock by region      
    REAL CLS4VSTKREGN(1836,YearsReadIn)          ! Class 4 vocational stock by region      
    REAL CLS5VSTKREGN(1836,YearsReadIn)          ! Class 5 vocational stock by region      
    REAL CLS6VSTKREGN(1836,YearsReadIn)          ! Class 6 vocational stock by region      
    REAL CLS7STKREGN(1836,YearsReadIn)           ! Class 7 stock by region                 
    REAL CLS7VSTKREGN(1836,YearsReadIn)          ! Class 7 vocational stock by region      
    REAL CLS8STKREGN(1836,YearsReadIn)           ! Class 8 stock by region                 
    REAL CLS8VSTKREGN(1836,YearsReadIn)          ! Class 8 vocational stock by region      
    REAL CLS8HSTKREGN(1836,YearsReadIn)          ! Class 8 super heavy trucks by region    


!...Reads stock data for the freight model from spreadsheet input file
    FNAME = 'TRNSTOCKX'
    WKUNIT = FILE_MGR('O',FNAME,NEW)   !open trnstockx.xlsx input file
    CALL ReadRngXLSX(WKUNIT,'HDV')      !read range names & coerresponding data from worksheet "HDV"
    WKUNIT = FILE_MGR('C',FNAME,NEW)   !close trnstockx.xlsx input file
    CLOSE(WKUNIT)
	
!...Historic stocks by vintage and fuel type
    CALL GETRNGR('CLS3STKHIST     ',CLS3STKHIST (1:34,6:22,1:6),AGE,17,6)
    CALL GETRNGR('CLS46STKHIST    ',CLS46STKHIST(1:34,6:22,1:6),AGE,17,6)
    CALL GETRNGR('CLS78STKHIST    ',CLS78STKHIST(1:34,6:22,1:6),AGE,17,6)
!... By region starting in 2012
    CALL GETRNGR('CLS2bSTKREGN    ',CLS2bSTKREGN, 1836,YearsReadIn,1)
    CALL GETRNGR('CLS2bVSTKREGN   ',CLS2bVSTKREGN, 1836,YearsReadIn,1)	
    CALL GETRNGR('CLS3STKREGN     ',CLS3STKREGN, 1836,YearsReadIn,1)
    CALL GETRNGR('CLS3VSTKREGN    ',CLS3VSTKREGN, 1836,YearsReadIn,1)
    CALL GETRNGR('CLS4VSTKREGN    ',CLS4VSTKREGN, 1836,YearsReadIn,1)
    CALL GETRNGR('CLS5VSTKREGN    ',CLS5VSTKREGN, 1836,YearsReadIn,1)
    CALL GETRNGR('CLS6VSTKREGN    ',CLS6VSTKREGN, 1836,YearsReadIn,1)
    CALL GETRNGR('CLS7STKREGN     ',CLS7STKREGN, 1836,YearsReadIn,1)
    CALL GETRNGR('CLS7VSTKREGN    ',CLS7VSTKREGN, 1836,YearsReadIn,1)
    CALL GETRNGR('CLS8STKREGN     ',CLS8STKREGN, 1836,YearsReadIn,1)
    CALL GETRNGR('CLS8VSTKREGN    ',CLS8VSTKREGN, 1836,YearsReadIn,1)
    CALL GETRNGR('CLS8HSTKREGN    ',CLS8HSTKREGN, 1836,YearsReadIn,1)
!... Variables to help with reading in regional stock variables
    CALL GETRNGI('HDV_CENSUS      ',HDV_CENSUS,  1,1836,1)
    CALL GETRNGI('HDV_FUEL        ',HDV_FUEL,  1,1836,1)
    CALL GETRNGI('HDV_VINTAGE     ',HDV_VINTAGE,  1,1836,1)   	
	
!...Reading in disaggregated stock data
  do m2 = 1,1836
    do j2 = 1,YearsReadIn
     y2 = j2 + 22
       r2 = HDV_census(m2)
	   a2 = HDV_vintage(m2)
	   f2 = HDV_fuel(m2)
	    trkstock(y2,2,a2,f2,1,r2) = CLS2bSTKREGN(m2,j2)
		trkstock(y2,2,a2,f2,2,r2) = CLS2bVSTKREGN(m2,j2)
		trkstock(y2,3,a2,f2,1,r2) = CLS3STKREGN(m2,j2)
		trkstock(y2,3,a2,f2,2,r2) = CLS3VSTKREGN(m2,j2)
		trkstock(y2,4,a2,f2,2,r2) = CLS4VSTKREGN(m2,j2)
		trkstock(y2,5,a2,f2,2,r2) = CLS5VSTKREGN(m2,j2)
		trkstock(y2,6,a2,f2,2,r2) = CLS6VSTKREGN(m2,j2)
		trkstock(y2,7,a2,f2,1,r2) = CLS7STKREGN(m2,j2)
		trkstock(y2,7,a2,f2,2,r2) = CLS7VSTKREGN(m2,j2)
		trkstock(y2,8,a2,f2,1,r2) = CLS8STKREGN(m2,j2)
		trkstock(y2,8,a2,f2,2,r2) = CLS8VSTKREGN(m2,j2)
		trkstock(y2,9,a2,f2,1,r2) = CLS8HSTKREGN(m2,j2)
	enddo
  enddo
	
  RETURN
  END SUBROUTINE CFREADSTOCK

!=============================================================================================================
  SUBROUTINE TFRTRPT
  USE F_
  IMPLICIT NONE

!...Local variable dictionary
    INTEGER IFNS
    REAL :: TEMP_V, TEMP
    REAL VMT_TMP,TRTMP
    REAL :: FUEL_VR(FUEL9,MNUMCR)

!...Benchmark VMT and FUEL for reporting using benchmark's determined in tran.f
    FUEL_VR(1,1:RGN) = BENDS(1:RGN,iyr)                                                                  ! Diesel
    FUEL_VR(2,1:RGN) = BENMG(1:RGN,iyr)                                                                  ! Motor Gasoline
    FUEL_VR(3,1:RGN) = BENLG(1:RGN,iyr)                                                                  ! Propane
    FUEL_VR(4,1:RGN) = BENNG(1:RGN,iyr)                                                                  ! Natural Gas
	FUEL_VR(5,1:RGN) = BENET(1:RGN,iyr)*PCTAF(2,1:RGN,iyr) + BENMG(1:RGN,iyr)*(1.0-PCTAF(2,1:RGN,iyr))   ! Ethanol
	FUEL_VR(6,1:RGN) = BENEL(1:RGN,iyr)                                                                  ! Electric
	FUEL_VR(7,1:RGN) = BENEL(1:RGN,iyr)*PctPHEV + BENDS(1:RGN,iyr)*(1 - PctPHEV)                         ! PHEV Diesel
	FUEL_VR(8,1:RGN) = BENEL(1:RGN,iyr)*PctPHEV + BENMG(1:RGN,iyr)*(1 - PctPHEV)                         ! PHEV Gasoline
	FUEL_VR(9,1:RGN) = BENHY(1:RGN,iyr)                                                                  ! Hydrogen
	

    do ifuel=1,fuel9
      do iregn=1,mnumcr
        if(FUEL_VR(ifuel,iregn).eq.0) FUEL_VR(ifuel,iregn)=1.0
      enddo
    enddo

    TEMP = 1000000.0
    TEMP_V = TEMP * 1000.0                      ! Scale for vmt

!...zero accumulating variables
    TFR_VMT_FAS_T(IYR,1:SC,1:FUEL9,:)=0.0
    TFR_TRK_FAS_T(IYR,1:SC,1:FUEL9,:)=0.0
    TFR_FBTU_FAS_T(IYR,1:SC,1:FUEL9,:)=0.0

    TFR_VMT_FASF_T(IYR,1:SC,:)=0.0
    TFR_TRK_FASF_T(IYR,1:SC,:)=0.0
    TFR_FBTU_FASF_T(IYR,1:SC,:)=0.0

    TFR_VMT_TR(IYR,:)=0.0
    TFR_TRK_TR(IYR,:)=0.0
    TFR_FBTU_TR(IYR,:)=0.0

!...Benchmark Stock VMT
    do isc=1,sc
      do ifuel=1,fuel9
        do iflt=1,flt
          do iregn=1,mnumcr-2
            TFR_VMT_FAS_T(IYR,ISC,IFUEL,FNEW) = TFR_VMT_FAS_T(IYR,ISC,IFUEL,FNEW) + (VMTFLTR(CUR,ISC,FNEW,IFUEL,IFLT,iregn)*FUEL_VR(IFUEL,iregn)/TEMP_V)
            TFR_VMT_FASF_T(IYR,ISC,FNEW)      = TFR_VMT_FASF_T(IYR,ISC,FNEW) + (VMTFLTR(CUR,ISC,FNEW,IFUEL,IFLT,iregn)*FUEL_VR(IFUEL,iregn)/TEMP_V)
            TFR_VMT_TR(IYR,FNEW)              = TFR_VMT_TR(IYR,FNEW) + (VMTFLTR(CUR,ISC,FNEW,IFUEL,IFLT,iregn)*FUEL_VR(IFUEL,iregn)/TEMP_V)
            do iage=1,age
              VMT_TMP = ((VMTFLTR(CUR,ISC,IAGE,IFUEL,IFLT,iregn)/TEMP_V)*FUEL_VR(IFUEL,iregn))
              TFR_VMT_FAS_T(IYR,ISC,IFUEL,FSTK) = TFR_VMT_FAS_T(IYR,ISC,IFUEL,FSTK) + VMT_TMP
              TFR_VMT_FASF_T(IYR,ISC,FSTK)      = TFR_VMT_FASF_T(IYR,ISC,FSTK) + VMT_TMP
              TFR_VMT_TR(IYR,FSTK)              = TFR_VMT_TR(IYR,FSTK) + VMT_TMP
            enddo
! ... Determine fuel consumption
            TFR_FBTU_FAS_T(IYR,ISC,IFUEL,FSTK)  = TFR_FBTU_FAS_T(IYR,ISC,IFUEL,FSTK) + ((1E-12)*HRATE(isc,ifuel))*FUEL_VR(IFUEL,iregn)*FUELDMDR(ISC,IFUEL,IFLT,iregn)
            TFR_FBTU_FASF_T(IYR,ISC,FSTK)       = TFR_FBTU_FASF_T(IYR,ISC,FSTK) + ((1E-12)*HRATE(isc,ifuel))*FUEL_VR(IFUEL,iregn)*FUELDMDR(ISC,IFUEL,IFLT,iregn)
            TFR_FBTU_TR(IYR,FSTK)               = TFR_FBTU_TR(IYR,FSTK) + ((1E-12)*HRATE(isc,ifuel))*FUEL_VR(IFUEL,iregn)*FUELDMDR(ISC,IFUEL,IFLT,iregn)
          enddo
        enddo
      enddo
	  
    enddo

    DO ISC = 1, SC
      DO IFUEL = 1, FUEL9
        DO IFLT = 1, FLT
          TFR_TRK_FAS_T(IYR,ISC,IFUEL,FNEW)   = TFR_TRK_FAS_T(IYR,ISC,IFUEL,FNEW) + (TRKSTK(cur,ISC,FNEW,IFUEL,IFLT)/TEMP)
          TFR_TRK_FASF_T(IYR,ISC,FNEW)        = TFR_TRK_FASF_T(IYR,ISC,FNEW) + (TRKSTK(cur,ISC,FNEW,IFUEL,IFLT)/TEMP)
          TFR_TRK_TR(IYR,FNEW)                = TFR_TRK_TR(IYR,FNEW) + (TRKSTK(cur,ISC,FNEW,IFUEL,IFLT)/TEMP)
          DO IAGE = 1, AGE
            TRTMP = (TRKSTK(cur,ISC,IAGE,IFUEL,IFLT) / TEMP)
            TFR_TRK_FAS_T(IYR,ISC,IFUEL,FSTK) = TFR_TRK_FAS_T(IYR,ISC,IFUEL,FSTK) + TRTMP
            TFR_TRK_FASF_T(IYR,ISC,FSTK)      = TFR_TRK_FASF_T(IYR,ISC,FSTK) +  TRTMP
            TFR_TRK_TR(IYR,FSTK)              = TFR_TRK_TR(IYR,FSTK) +  TRTMP
          ENDDO
        ENDDO
      ENDDO
    ENDDO

!...Fill MPG for reporting
    DO ISC = 1,SC
      DO IFUEL = 1, FUEL9
!...Store new vehicle MPG for fuels and size classes
        TFR_FTMPG(IYR,ISC,IFUEL,FNEW) = HDV_MPG(iyr,ISC,FNEW,IFUEL)		
!...Average mpg over vintages
        TFR_FTMPG(IYR,ISC,IFUEL,FSTK) = harmonic_mean(HDV_MPG(iyr,ISC,1:AGE,IFUEL),VMTFLT_SF_TR(ISC,1:AGE,IFUEL,11),age)
	    if(TFR_FTMPG(IYR,ISC,IFUEL,FSTK).eq.0) TFR_FTMPG(IYR,ISC,IFUEL,FSTK) = TFR_FTMPG(IYR-1,ISC,IFUEL,FSTK)
      ENDDO
!...Average MPG over fuel for new vehicles and the total stock
      DO IFNS = FNEW, FSTK
        TFR_FTMPG_S(IYR,ISC,IFNS) =harmonic_mean(TFR_FTMPG(IYR,ISC,1:FUEL9,IFNS),TFR_VMT_FAS_T(IYR,ISC,1:FUEL9,IFNS),fuel)
      ENDDO	
    ENDDO

!...Average MPG over Size Class
    DO IFNS = FNEW, FSTK
      TFR_FTMPG_TR(IYR,IFNS)=harmonic_mean(TFR_FTMPG_S(IYR,1:SC,IFNS),TFR_VMT_FASF_T(IYR,1:SC,IFNS),SC)
    ENDDO        ! IFNS   NEW/STOCK

!...Investment Calculation
    TFR_INVEST_TR(IYR) = TFR_TRK_FASF_T(IYR,MDL,FNEW) * 52500. + &
                         TFR_TRK_FASF_T(IYR,MDH,FNEW) * 52500. + &
                         TFR_TRK_FASF_T(IYR,HV, FNEW) * 110000.

  RETURN
  END SUBROUTINE TFRTRPT

! ==========================================================================================================
   FUNCTION HARMONIC_MEAN(MPGS,WEIGHTS,NVAL)
   IMPLICIT NONE

!...Computes harmonic mean, used for averaging fuel economy measured in miles per gallon.  The
!...calculation essentially takes the reciprocal of MPG, or efficieny,
!...computes the quantity weighted average, then converts the result back
!...to a miles-per-gallons by taking the reciprocal

!...INPUTS:
!...  mpgs   : arragy of mpg values to be averaged
!...  weights: array of quantity weights
!...  nval   : number of items in the array

    INTEGER NVAL,I
    REAL MPGS(NVAL),WEIGHTS(NVAL),TEMPSUM
    REAL HARMONIC_MEAN ! function return value

    HARMONIC_MEAN=0.

    TEMPSUM=SUM(WEIGHTS)
    IF(TEMPSUM.NE.0.) THEN
      DO I=1,NVAL
        IF(MPGS(I).NE.0.) THEN
          HARMONIC_MEAN= HARMONIC_MEAN+ WEIGHTS(I)*(1./MPGS(I))
        ENDIF
      ENDDO
      HARMONIC_MEAN=HARMONIC_MEAN/SUM(WEIGHTS)
      IF(HARMONIC_MEAN.NE.0.) THEN
        HARMONIC_MEAN=1./HARMONIC_MEAN
      ENDIF
    ENDIF
  RETURN
  END

! ==========================================================================================================
!...Subroutine TSHIP
!   Description:
!   Projects freight marine energy consumption by estimating ton-miles and energy efficiency
! ==========================================================================================================
    SUBROUTINE TSHIP
    USE F_
    IMPLICIT NONE

    include 'angtdm'
    include 'pmmrpt'
    include 'pmmout'
    include 'lfmmout'
    include 'ogsmout'
    include 'ngtdmrep'
    include 'convfact'
    include 'cdsparms'
    include 'coalout'

!...Local variable dictionary
    INTEGER :: ISIC                        !...industrial sector subscript
    INTEGER, PARAMETER :: SIC = 16         !...industrial sectors that move domestic marine commodities:
                                           !...1) basic chemicals                 2) primary metals
                                           !...3) processed food                  4) paper products
                                           !...5) petroleum products              6) stone, clay, glass, and concrete
                                           !...7) metal durables, less computers  8) other manufacturing
                                           !...9) agriculture                     10) mining, less coal mining
                                           !...11) alcohol and tobacco            12) pharma
                                           !   13) fertilizers                    14) rubber and plastics
                                           !   15) computers
                                           !   16) furniture
    INTEGER, PARAMETER :: VTYPE = 9        ! number of vessel types
    REAL, PARAMETER ::  IFO = .88          ! intermediate fuel oil (IFO)is a mix of residual and distillate fuel oils
    REAL, PARAMETER ::  IMO2020 = .60      ! IMO2020 compliant fuel is a mix of residual and distillate fuel oils
    REAL, PARAMETER ::  UpgradeCost = .43  ! fixed and variable cost per mmBtu (2000$) to upgrade residual fuel oil to IMO 2020 compliant lsfo
    REAL :: ISFDT(MNUMYR),ISFDFSHR(MNUMYR),ISFDT_B(MNUMYR)
    REAL :: INTS_B
    REAL :: reg_shr(MNUMCR)                ! regional fuel share
    REAL :: FLEETSURV(VTYPE)               ! fleet survival percentage for the current year, by vessel type
    REAL :: MEFFINC(VTYPE)                 ! hold marine engine efficiency improvements for the current year, by vessel type
    REAL :: ECAFUELCONS(MNUMCR,MNUMYR),FLTPROF(5,MNUMCR,MNUMYR),ECADEMAND
    REAL :: GEFFECTS(VTYPE,MNUMYR),MARFUEL(VTYPE,MNUMCR,MNUMYR)
    INTEGER :: IR
    INTEGER :: MFTYPE,MV
    REAL :: DSPFAC,RFPFAC,CNGPFAC,LNGPFAC,LSFOPFAC
    REAL :: ALPHA,W(5),LSUM, LSFO_UPGRADE(MNUMCR,MNUMYR)
    REAL :: AVEMEFF(5),ENPEN(5)            ! average marine engine fuel efficiency and penalties; multiple sources from Leidos marine report
	REAL :: price_input(5,mnumcr,mnumyr)	! MDRAEO2022 -- fuel prices pre-convergence

    AVEMEFF=(/0.50,0.48,0.28,0.50,0.48/)        ! multiple sources from Leidos marine fuel choice report
    ENPEN=(/0.0,0.02,0.0,0.0,0.0/)                ! penalty for scrubber with residuel fuel oil
    
!...calculate freight domestic marine freight parameter for latest FAF year
    if(n.eq.iFAFyear) then
      do iregn=1,mnumcr-2
        do isic=1,sic
          if(TSIC(isic,iregn,iFAFyear).gt.0.) then
            DSTM_OUTPUT(iregn,isic)=DSHIST_TONMI(n,iregn)*DTM_SHARES(iregn,isic)/TSIC(isic,iregn,iFAFyear)   ! compute static domestic marine freight parameter (ton-miles per $ of industrial output)
          else
            DSTM_OUTPUT(iregn,isic)=0.
          endif
        enddo
      enddo
    endif
    
!...calculate freight domestic marine ton-miles
    do iregn=1,mnumcr-2
      if(n.le.SHIPHISTYR)then
        STMTT(n,iregn)=DSHIST_TONMI(n,iregn)
      else
        do isic=1,sic
          DSADD_TONMI(n,iregn,isic)=TSIC(isic,iregn,n)*(DSTM_OUTPUT(iregn,isic)*(1.-ANN_DECLINE(n)))
        enddo
		STMTT(n,iregn)=sum(DSADD_TONMI(n,iregn,1:SIC))
      endif
    enddo

!...overwrite STMTT(:,:) census division freight domestic marine ton-miles for short-term history (between iFAFyear and SHIPHISTYR)
    if(n.ge.iFAFyear.and.n.le.SHIPHISTYR) then
      do iregn=1,mnumcr-2
        do isic=1,sic
          DSADD_TONMI(n,iregn,isic)=TSIC(isic,iregn,n)*(DSTM_OUTPUT(iregn,isic)*(1.-ANN_DECLINE(n)))
        enddo
      enddo
      do iregn=1,mnumcr-2
        if(sum(DSADD_TONMI(n,1:mnumcr-2,1:SIC)).gt.0.) then
          STMTT(n,iregn)=sum(DSHIST_TONMI(n,1:mnumcr-2))* &
                          (sum(DSADD_TONMI(n,iregn,1:SIC))/ &
                          sum(DSADD_TONMI(n,1:mnumcr-2,1:SIC)))
        else
          STMTT(n,iregn)=0.
        endif
      enddo
    endif
    STMTT(n,11)=sum(STMTT(n,1:mnumcr-2))           !...compute a national value from the regional

!...calculate domestic marine energy consumption
    if(n.gt.SHIPHISTYR)then
      if(IFRTEFF.eq.2) DSEFF(n)=DSEFF(SHIPHISTYR)  !...freezes domestic shipping efficiency at last historic year
      if(IFRTEFF.eq.1) DSEFF(n)=HTDSEFF(n)
    endif
    do iregn=1,mnumcr
      TQDSHIPT(n,iregn)=STMTT(n,iregn)*DSEFF(n)
    enddo

!...calculate fuel use by domestic shipping
    do iregn=1,mnumcr
      do MFTYPE=1,4
        TQDSHIPR(MFTYPE,iregn,n) = TQDSHIPT(n,iregn)*domship_fuel_shr(MFTYPE,n)
      enddo
    enddo

! ... calculate international shipping fuel demand

    grosst(n-1)=tmc_ex(n-1)+tmc_im(n-1)
    GROSST(N)=TMC_EX(N)+TMC_IM(N)

    ISFDT(6) =964.0   ! 1995
    ISFDT(7) =867.0
    ISFDT(8) =716.0
    ISFDT(9) =695.0
    ISFDT(10)=838.0   ! 1999

    ISFDT_B(N) = ISFDT(N)  !new reporting variable

    IF (N.GE.6) INTS_B =.05

    IF (YRS .GE. 2013) THEN
      IF (IFRTEFF.EQ.2) INTS_B = .06
      IF (IFRTEFF.EQ.1) INTS_B = .04
    ENDIF

! ... Benchmark to SEDS
    IF(N.GE.11.AND.N.LE.msedyr) THEN   !...SEDS history years = 2000-msedyr
      ISFDT(N) = QSRSTR(11,n) * .9340
    ENDIF

    IF(N.EQ.msedyr+1) then             !...MER history year = msedyr+1
      ISFDT(N)=695.086                 !...Bunkering fuel is a blend - Intermediate Fuel Oil (IFO380)
    ELSEIF(N.GT.msedyr+1) THEN         !...greater than last SEDS year (i.e., years > MER year)
      ISFDT(N)=(ISFDT(N-1)+(((GROSST(N)/GROSST(N-1))-1.0) &
        *0.5*INTS_B*ISFDT(N-1)))
    ELSE
    ENDIF

    ISFDT_B(N)=ISFDT(N)
    !if(curitr.eq.maxitr+1) write(21,'(a,i4,1(1x,f10.5))') 'msi ISFDT_B ', curcalyr, ISFDT_B(N)

! ... calculate international shipping fuel demand occurring in north american emission control areas for fuel sharing purposes

    geffects(1,n)=(rfqicrd(10,n) + rfsprim(n) +   &              ! crude imports
       rfpqiprdt(10,n,2) + ethimp(11,n)/1000. +   &              ! petroleum products imports
       biodimp(11,n)/1000. +                      &
       rfipqcbob(10,n,2)/1000. +                  &
       rfipqrbob(10,n,2)/1000. +                  &
       rfmtbi(10,n) +                             &
       rfpqufc(10,n,2))/                          &
       (rfqicrd(10,23) + rfsprim(23) +            &              ! crude imports
       rfpqiprdt(10,23,2) + ethimp(11,23)/1000. + &              ! petroleum products imports
       biodimp(11,23)/1000. +                     &
       rfipqcbob(10,23,2)/1000. +                 &
       rfipqrbob(10,23,2)/1000. +                 &
       rfmtbi(10,23) +                            &
       rfpqufc(10,23,2))
    geffects(2,n)=mc_xginr(n)/mc_xginr(23)
    geffects(3,n)=mc_mgcr(n)/mc_mgcr(23)
    geffects(4,n)=ngexpvol(3,n)/ngexpvol(3,23)
    geffects(5,n)=(qprdex(27,n)+      &                 ! ethane (cc2)
                    qprdex(1,n)+      &                 ! propane (cc3)
                    qprdex(15,n)+     &                 ! normal butane (nc4)
                    qprdex(28,n)+     &                 ! isobutane (ic4)
                    qprdex(29,n)+     &                 ! pentanes plus (nat)
                    qprdex(14,n))/    &                 ! propylene (uc3)
                    (qprdex(27,23)+   &                 ! ethane (cc2)
                    qprdex(1,23)+     &                 ! propane (cc3)
                    qprdex(15,23)+    &                 ! normal butane (nc4)
                    qprdex(28,23)+    &                 ! isobutane (ic4)
                    qprdex(29,23)+    &                 ! pentanes plus (nat)
                    qprdex(14,23))                      ! propylene (uc3)
    geffects(6,n)=mc_mgautor(n)/mc_mgautor(23)
    geffects(7,n)=cqdbfb(11,5,n)/cqdbfb(11,5,23)
    geffects(8,n)=mc_xgffbr(n)/mc_xgffbr(23)
    geffects(9,n)=mc_mgkr(n)/mc_mgkr(23)

    if(curcalyr.eq.2012) then
      do iregn=1,mnumcr-2
        !if(curitr.eq.maxitr+1) write(21,'(a,i4,9(1x,f13.2))') '2012 eca baseline ', iregn, (fuelcons(iregn,mv),mv=1,vtype)
      enddo
    endif

    ecafuelcons(:,n)=0.
    if(curcalyr.ge.2012) then
      do iregn=1,mnumcr-2
        do mv=1,vtype
          fleetsurv(mv)=max(0.,1.-((curcalyr-2012)*fleetto(mv)))
          meffinc(mv)=(1-effinc(mv))**((curcalyr-2012)*.5)
          ecademand=((fuelcons(iregn,mv)*fleetsurv(mv))+         &
            (fuelcons(iregn,mv)*(1.-fleetsurv(mv))*meffinc(mv)))* &
            geffects(mv,n)
            !if(curitr.eq.maxitr+1) write(21,'(A,3I4,f12.3)') 'msi, ecademand,', curcalyr, iregn, mv, ecademand
          ecafuelcons(iregn,n)=ecafuelcons(iregn,n)+ecademand
        enddo
        !if(curitr.eq.maxitr+1) write(21,'(a,2(1x,i4),9(1x,f6.4))') 'msi,fleetsurv,', curcalyr, iregn, (fleetsurv(mv),mv=1,vtype)
      enddo
      !if(curitr.eq.maxitr+1) write(21,'(a,i4,9(",",1x,f12.3))') 'msi,ecafuelcons,',CURCALYR,(ecafuelcons(iregn,n),iregn=1,mnumcr-2)
    endif

! ... calculate international shipping fuel demand fuel shares occurring in North American Emission Control Areas (ECA)

    w = 0.     ! initialize logit elements to zero
    alpha=-2.  ! set the "default" alpha
    fltprof(:,:,n)=0.
!    fltprof(2,:,1:25)=1.     ! residual fuel oil is 100 percent pre-2015
!    fltprof(4,:,26)=.012
!    fltprof(2,:,26)=.058
!    fltprof(1,:,26)=.930
    
    fltprof(5,:,31)=intship_fuel_shr(5,31)  !...initialize low sulfur fuel oil share
    fltprof(4,:,31)=intship_fuel_shr(4,31)  !...initialize LNG share
    fltprof(2,:,31)=intship_fuel_shr(2,31)  !...initialize Residual share
    fltprof(1,:,31)=intship_fuel_shr(1,31)  !...initialize Distillate share
    
    if(curcalyr.le.2020) then 
      dspfac= 1.00                ! considered the premium ECA fuel pre-2019
      rfpfac= 1.05
      cngpfac=1.00
      lngpfac=1.15
    else
      dspfac= INTSHIPLOGIT_FAC(1,1)+(INTSHIPLOGIT_FAC(1,2)-INTSHIPLOGIT_FAC(1,1))/ &
              (1+exp(-1.*(float(curcalyr)-INTSHIPLOGIT_FAC(1,3))))  ! penalize the earlier premium fuel post-2018, phase-in penalties with 2022 as midway point
      rfpfac= INTSHIPLOGIT_FAC(2,1)+(INTSHIPLOGIT_FAC(2,2)-INTSHIPLOGIT_FAC(2,1))/ &
              (1+exp(-.5*(float(curcalyr)-INTSHIPLOGIT_FAC(2,3))))  !
      cngpfac=INTSHIPLOGIT_FAC(3,1)+(INTSHIPLOGIT_FAC(3,2)-INTSHIPLOGIT_FAC(3,1))/ &
              (1+exp(-.5*(float(curcalyr)-INTSHIPLOGIT_FAC(3,3))))  !
      lngpfac=INTSHIPLOGIT_FAC(4,1)+(INTSHIPLOGIT_FAC(4,2)-INTSHIPLOGIT_FAC(4,1))/ &
              (1+exp(-.5*(float(curcalyr)-INTSHIPLOGIT_FAC(4,3))))  !
      lsfopfac=INTSHIPLOGIT_FAC(5,1)+(INTSHIPLOGIT_FAC(5,2)-INTSHIPLOGIT_FAC(5,1))/ &
              (1+exp(-.5*(float(curcalyr)-INTSHIPLOGIT_FAC(5,3))))  !
    endif 
    !if(curitr.eq.maxitr+1) write(21,'(a,i4,10(",",1x,f13.4))') 'msi,price,',curcalyr,dspfac,(pdstr(iregn,n),iregn=1,mnumcr-2)
    
    lsfo_upgrade(:,n)=0.0
    if(curcalyr.gt.2020) then
      do iregn=1,mnumcr-2
        if(prltr(iregn,n).gt.0.0) lsfo_upgrade(iregn,n)=(UpgradeCost/mc_jpgdp(11)*mc_jpgdp(1))+prltr(iregn,n)                ! UpgradeCost is a price adder for low sulfur fuel oil upgrade (2000$)
        w(1)=dspfac*max(1.,pdstr(iregn,n)/pgltrship(3,iregn,n))*(maxval(pdstr(iregn,n-2:n)))/(maxval(pdstr(iregn,26:27)))    ! indexed price of distillate (base year not = 1)
        w(2)=rfpfac*max(1.,prhtr(iregn,n)/pgltrship(3,iregn,n))*(maxval(prhtr(iregn,n-2:n)))/(maxval(prhtr(iregn,26:27)))    ! indexed price of high-sulfur residual fuel oil
        w(3)=cngpfac*pgftrship(3,iregn,n)/pgftrship(3,iregn,27)                                ! indexed price of cng
        w(4)=lngpfac*(maxval(pgltrship(3,iregn,n-2:n)))/(maxval(pgltrship(3,iregn,26:27)))     ! indexed price of lng
        w(5)=lsfopfac*max(1.,lsfo_upgrade(iregn,n)/pgltrship(3,iregn,n))*(maxval(lsfo_upgrade(iregn,n-2:n)))/(maxval(lsfo_upgrade(iregn,26:27)))    ! indexed price of upgraded low-sulfur fuel oil
!        if(curitr.eq.maxitr+1) write(21,'(a,i4,5(",",1x,f13.4))') 'msi,w,',curcalyr,ecafuelcons(iregn,n),(w(mftype),mftype=1,4)

!		MDRAEO2022 -- Grabbing pre-converged prices for offline Excel marine model checks
		IF(curitr.eq.maxitr) THEN		! No iterations - grab the raw "first guess" prices
	      price_input(1,iregn,n) = pdstr(iregn,n)
		  price_input(2,iregn,n) = prhtr(iregn,n)
		  price_input(3,iregn,n) = pgftrship(3,iregn,n)
		  price_input(4,iregn,n) = pgltrship(3,iregn,n)
		  price_input(5,iregn,n) = lsfo_upgrade(iregn,n)
		ENDIF

        lsum = 0.0
        do mftype=1,4+1
          if(w(mftype).le.0.05) w(mftype)=.05  ! prevent zero raised to negative exp (alpha) when prices set to zero
          lsum = lsum+(((w(mftype)**alpha)*fltprof(mftype,iregn,31))*avemeff(mftype))
        enddo
        if(lsum.gt.0) then
          do mftype=1,4+1
            fltprof(mftype,iregn,n)=((w(mftype)**alpha)*fltprof(mftype,iregn,31)*avemeff(mftype))/lsum
          enddo
        endif
!       if(curitr.eq.maxitr+1) write(21,'(a,i4,5(",",1x,f6.4))') 'msi,fltprof,',curcalyr,(fltprof(mftype,iregn,n),mftype=1,4),lsum
        ! normalize shares
        lsum=0.
        do mftype=1,4+1
          lsum=lsum+fltprof(mftype,iregn,n)
        enddo
        if((lsum.ne.0).and.(abs(lsum-1.).gt.0.001)) then
          do mftype=1,4+1
            fltprof(mftype,iregn,n)=fltprof(mftype,iregn,n)/lsum
          enddo
!          if(curitr.eq.maxitr+1) write(21,'(a,i4,6(",",1x,f6.4))') 'msi,fltprofnorm,',curcalyr,(fltprof(mftype,iregn,n),mftype=1,4+1),lsum
        endif
      enddo
    else
      do iregn=1,mnumcr-2
        if(prltr(iregn,n).gt.0.0) lsfo_upgrade(iregn,n)=(UpgradeCost/mc_jpgdp(11)*mc_jpgdp(1))+prltr(iregn,n)    ! 0.31 is a price adder for low sulfur fuel oil upgrade (2000$)
      enddo
    endif
	
!	MDRAEO2022 -- write out prices (NOTE: post-model convergence)
	IF(curitr.eq.maxitr+1.and.n.eq.mnumyr) THEN
	  DO mftype=1,MNUMYR			! Commandeer an index for year
	    DO iregn=1,mnumcr-2
!	      WRITE(21,'(a,i4,",",i3,6(",",1x,f13.4))') 'mdr_prices, ',mftype+1989,iregn,UpgradeCost/mc_jpgdp(11)*mc_jpgdp(1),prltr(iregn,mftype),pdstr(iregn,mftype),prhtr(iregn,mftype), pgltrship(3,iregn,mftype),pgftrship(3,iregn,mftype)
!	      write(21,'(a,i4,",",i2,5(",",1x,f10.4))') 'MDR_prices,',mftype+1989,iregn,price_input(:,iregn,mftype)
		ENDDO
	  ENDDO
	ENDIF
	

!...calculate eca regional (census division) consumption and add to international shipping
    marfuel=0.
    do iregn=1,mnumcr-2
      do mftype=1,4+1
        marfuel(mftype,iregn,n)=ecafuelcons(iregn,n)*fltprof(mftype,iregn,n)*(1+enpen(mftype))*.001
        !if(curitr.eq.maxitr+1) write(21,'(a,3(",",1x,i4),1(",",1x,f12.3))') 'msi,tqishipr',curcalyr,mftype,iregn,tqishipr(mftype,iregn,n)
        !if(curitr.eq.maxitr+1) write(21,'(a,3(",",1x,i4),1(",",1x,f12.3))') 'msi,marfuel',curcalyr,mftype,iregn,marfuel(mftype,iregn,n)
        !if(curitr.eq.maxitr+1) write(21,'(a,3(",",1x,i4),1(",",1x,f12.3))') 'msi,tqishiprmar',curcalyr,mftype,iregn,tqishipr(mftype,iregn,n)
      enddo
!      if(curitr.eq.maxitr+1) write(21,'(a,2(",",1x,i4),9(",",1x,f12.3))') 'msi,marfuel',curcalyr,iregn,(marfuel(mftype,iregn,n),mftype=1,5)
    enddo
    
! ... calculate the non-ECA international shipping fuel use split by the 4 fuel types:
! ...   1) distillate
! ...   2) residual
! ...   3) CNG
! ...   4) LNG

!...calculate fuel use by international shipping
    do MFTYPE=1,4+1
      if(curcalyr.le.2020) then
        isfd(MFTYPE,n) = isfdt_b(n) * intship_fuel_shr(MFTYPE,n)
      else
        intship_fuel_shr(MFTYPE,n)=sum(marfuel(MFTYPE,1:mnumcr-2,n))/sum(marfuel(:,1:mnumcr-2,n)) ! overwrites: ECA consumption projects fuel share for int'l marine
        !if(curcalyr.ge.2025) intship_fuel_shr(MFTYPE,n)=intshi_fuel_shr(MFTYPE,n-1)
        !if(curitr.eq.maxitr+1) write(21,'(a,i4,i4,1(1x,f10.5))') 'msi marfuel share ', curcalyr, mftype, sum(marfuel(MFTYPE,1:mnumcr-2,n))/sum(marfuel(1:4,1:mnumcr-2,n))
        isfd(MFTYPE,n) = isfdt_b(n) * intship_fuel_shr(MFTYPE,n)
      endif
    enddo
    
!    if(curitr.eq.maxitr+1.and.curcalyr.eq.2050) then
!      do mv=6,mnumyr
!        write(21,'(a,i2,1x,5(1x,f8.5))') 'msi marfuel share ', mv, intship_fuel_shr(1,mv),intship_fuel_shr(2,mv),intship_fuel_shr(3,mv),intship_fuel_shr(4,mv),intship_fuel_shr(5,mv)
!      enddo
!    endif

!...calculate regional consumption and adjust residual fuel oil to account for intermediate fuel oil (IFO) mixture
    do iregn=1,mnumcr-2
      do MFTYPE=1,4+1
        reg_shr(iregn) = sedshrrs(iregn,n)
        if(MFTYPE.eq.1) reg_shr(iregn) = sedshrds(iregn,n)
        tqishipr(MFTYPE,iregn,n) = isfd(MFTYPE,n) * reg_shr(iregn)
        if(mftype.eq.2) then
          tqishipr(1,iregn,n)=tqishipr(1,iregn,n)+(tqishipr(mftype,iregn,n)*(1.-ifo))       ! distillate fuel oil amount, ifo mix
          tqishipr(mftype,iregn,n)=tqishipr(mftype,iregn,n)*ifo                             ! residual fuel oil amount, ifo mix
        endif
        if(mftype.eq.5) then
          tqishipr(1,iregn,n)=tqishipr(1,iregn,n)+tqishipr(mftype,iregn,n)*(1.-IMO2020)     ! now increase distillate fuel oil amount for blending
          tqishipr(mftype,iregn,n)=tqishipr(mftype,iregn,n)*IMO2020                         ! now decrease lsfo amount to agree with distillate blending

          tqishipr(1,iregn,n)=tqishipr(1,iregn,n)+(tqishipr(mftype,iregn,n)*(lsfoRisk(n)))  ! distillate fuel oil amount, risk of lsfo availability
          tqishipr(mftype,iregn,n)=tqishipr(mftype,iregn,n)*(1.-lsfoRisk(n))                ! lsfo amount, 2020 risk of lsfo availability
          endif
      enddo
    enddo

    do mftype=1,4+1
      tqishipr(mftype,11,n)=sum(tqishipr(mftype,1:mnumcr-2,n))  ! sum to obtain national value, by fuel
      marfuel(mftype,11,n)=sum(marfuel(mftype,1:mnumcr-2,n))    ! sum to obtain national value, by fuel
    enddo
!    write(21,'(a,3(",",1x,i4),9(",",1x,f12.3))') 'msi,tqishiprmar',curitr,curcalyr,iregn+1,(tqishipr(mftype,11,n),mftype=1,4+1)

    RETURN
    END SUBROUTINE TSHIP
