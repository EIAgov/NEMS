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
!=============================================================================================================
!...Parameters
    INTEGER AGE,SEC,SC,FNEW,FSTK, &
            TK,NFT,FLT,RGN,CUR,LAG,NVMT,BSYR_VMT,BSYR_STK, &
			CAFE14,TECHP2,CAFE19,SC4,FUEL12,PBK_YR,VOC, &
            GHGTRY

PARAMETER(AGE = 34)      	! Number of vintages for truck stocks
PARAMETER(SEC = 18)      	! Number of Industrial sectors:
							!   1 = Chemicals
							!   2 = Primary Metals
							!   3 = Processed Food
							!   4 = Paper Products
							!   5 = Petroleum Products
							!   6 = Stone, Clay, Glass, & Concrete
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
PARAMETER(SC = 3)        	! Number of Truck Size Classes (old)
							!   1 = Medium Light, Class 3
							!   2 = Medium Heavy, Classes 4-6
							!   3 = Heavy,        Classes 7-8
PARAMETER(SC4 = 4)       	! Number of Truck Size Classes - used for sales
							!   1 = Medium Light, Class 3
							!   2 = Medium Heavy, Classes 4-6
							!   3 = Heavy,        Classes 7-8		
							!   4 = Medium Light, Class 2						 
PARAMETER(CAFE4 = 4)        !   Averaging sets for EPA/NHTSA GHG/CAFE reg
                            !   1: 2b3 PUV, 2: 2b-5, 3: 6-7, 4: 8
PARAMETER(CAFE14 = 14)      !	Number of truck size classes for fuel economy standard classification for GHG Phase II Standards (does not include trailers)
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
PARAMETER(CAFE19 = 19)   	! Number of truck size classes for Phase 2 standards
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
PARAMETER(GHGTRY = 5)      ! Number of attempts the model will make to meet EPA GHG regulation 
                            ! via tech adoption and powertrain choice before forcing ZEVs in TRUCK_GHGZEV
PARAMETER(FUEL12 = 12)   	! Number of fuel types:
							!   1 = Diesel
							!   2 = Gasoline
							!   3 = LPG
							!   4 = CNG
							!   5 = Flex
							!   6 = Electric
							!   7 = PHEV diesel
							!   8 = PHEV gasoline
							!   9 = Hydrogen fuel cell vehicle -- small battery
							!	10= Hydrogen fuel cell vehicle -- large battery
                            !	11= Gasoline hybrid (non-plug-in)
                            !   12= Hydrogen ICE
PARAMETER(FNEW = 1)      	! NEW VEHICLE    for reporting variables
PARAMETER(FSTK = 2)      	! FREIGHT STOCK  for reporting variables
PARAMETER(NFT = 1)       	! Truck: Non fleet
PARAMETER(FLT = 2)       	! Truck: Fleet
PARAMETER(NVOC = 1)      	! Non-vocational truck
PARAMETER(VOC = 2)       	! Vocational truck 
PARAMETER(TECHP2 = 83)   	! Phase 2 HDV GHG Standards New technologies available (see trnhdvx.xlsx for definitions)
PARAMETER(PBK_YR = 7)	 	! Maximum length of fleet financial horizon
PARAMETER(RGN = 11)        	! Regions
PARAMETER(CUR=2,LAG=1)     	! Current and Lag year subscripts
PARAMETER(NVMT = 11)       	! NUMBER OF VMT bins
PARAMETER(BSYR_VMT = 33)   	! Nems year index of base year for truck VMT data
PARAMETER(BSYR_STK = 2023) 	! Nems year index of base year for Truck Stock data
!=============================================================================================================
!...Subscripts
INTEGER IAGE                    	! Index for vintage
INTEGER IFUEL                   	! Index for fuel type
INTEGER JFUEL						! Index for fuel type, used in fuel choice module
INTEGER IFLT                    	! Index for fleet
INTEGER IVOC                    	! Index for vocation
INTEGER IVMT						! VMT bin (max = NVMT)
INTEGER ISC                     	! Index for size class or ship region
INTEGER ISC4                    	! Index for mapping 4 size classes to 19
INTEGER icafe4                 	    ! Index for averaging sets in EPA GHG standards
INTEGER icafe14                 	! Index for size classes in EPA GHG standards
INTEGER ICAFE19                 	! Index for disaggregated EPA GHG standards size classes
INTEGER itechrpt19                  ! Index for size classes (crosswalking FROM icafe19 TO techshr aggregation level)
INTEGER ISEC                    	! Index for industrial sector
INTEGER IYR                     	! Index for year
INTEGER ITECHP2                 	! Index for Phase 2 GHG technology
INTEGER ITR                     	! NEMS iteration
INTEGER IREGN                   	! census division
INTEGER itryghg                     ! iterator for GHG reg attempts
!...Maps between levels of aggregation
INTEGER P219Map(CAFE19)           	! Maps 14 CAFE classes to 19 EPA size categories for Phase 2
INTEGER P24Map(CAFE14)              ! Map 4 EPA GHG averaging sets into 14 size classes
INTEGER SC19Map(CAFE19)         	! Maps 4 reporting classes to 19 EPA size classes
INTEGER TechRptMap(CAFE19)			! Map 19 size classes into groupings to report technology adoption
INTEGER SurvMap(CAFE19)				! Maps 11 survival curves to 19 EPA size classes
INTEGER CLTMap(FUEL12)              ! Maps 12 freight truck powertrain types to 12 commercial light truck powertrain types (gas/diesel flipped)
INTEGER CLTMap2(7)
INTEGER GVWRP2Map(CAFE19)			! Maps GVWR classes (2,3,4,5,6,7,8,9) to 19 EPA size classes
INTEGER VocOrNoMap(CAFE19)			! Map of vocational/not for 19 size classes (1=Not,2=Voc)
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

! Market penetration curve parameters
INTEGER*2 CYAFVXG(SC4,FUEL12,FLT) 					! Logistic mkt penetration curve parameter # of yrs, AFV
INTEGER*2 TRGSHXG(CAFE19,FUEL12,FLT) 				! Logistics paramter: 1/2 way to max Mkt penetration
REAL 	  BFSHXG(SC4,FUEL12,FLT)             		! Starting market penetration for S-curve (in base year, TRGSHXG)
REAL 	  CSTDXG(SC4,FLT)                   		! Mkt penetration curve parm for diesel	
REAL 	  CSTDVXG(SC4,FLT)                  		! Mkt penetration curve parm for diesel
REAL 	  EFSHXG(SC4,FUEL12,FLT)             		! Final mtk share of each fuel

REAL ANNVMT_19(MNUMYR,CAFE19,AGE,FUEL12)					! Annual VMT by freight truck size class (19) and vintage (34)
REAL ANNVMT(SC4,AGE,FUEL12,VOC)         			! Average annual VMT per vehicle
REAL FUELPRICE_R(MNUMYR,FUEL12,MNUMCR)  			! Price of fuel, 1990$ per MBtu
REAL FUELPRICE_R_AVG(FUEL12,MNUMCR)     			! Average regional price of fuel over 3 years, 1990$ per MBtu
REAL BMPGSTK2B(AGE,MNUMYR,6)           				! class 3 historic fuel economy by vintage
REAL BMPGSTK3(AGE,MNUMYR,6)            				! class 3 historic fuel economy by vintage
REAL BMPGSTK46(AGE,MNUMYR,6)           				! class 4-6 historic fuel economy by vintage
REAL BMPGSTK78(AGE,MNUMYR,6)           				! class 7-8 historic fuel economy by vintage
REAL HDV_MPG(MNUMYR,SC4,AGE,FUEL12)     			! ON-ROAD fuel economy size class, vintage, and fuel in mpg miles/cubic cng
REAL HDV_MPG_S_F(MNUMYR,SC,FUEL12)      			! ON-ROAD fuel economy by size class and fuel
REAL HDV_MPG_S(MNUMYR,SC)              				! ON-ROAD fuel economy by size class
REAL HRATE(SC4,FUEL12)                  			! heat rate by fuel type
		
! Data inputs and results for fuel choice model
REAL INC_COST_CNG(NVMT,CAFE19,MNUMYR)				! Total CNG incremental cost
REAL INC_COST_BEV(NVMT,CAFE19,MNUMYR)				! Total BEV incremental cost
REAL INC_COST_HEV(NVMT,CAFE19,MNUMYR)				! Total Gasoline HEV incremental cost
REAL INC_COST_H2ICE(NVMT,CAFE19,MNUMYR)				! Total H2ICE incremental cost
REAL INC_COST_PHEVD(NVMT,CAFE19,MNUMYR)				! Total PHEV diesel incremental cost
REAL INC_COST_PHEVG(NVMT,CAFE19,MNUMYR)				! Total PHEV gasoline incremental cost
REAL cumulative_fc_stacks(MNUMYR)    				! cumulative Fuel Cell Stack additions
REAL cumulative_h2_tanks(MNUMYR)    				! cumulative H2 Storage Tank additions
REAL INC_COST_FCEV(NVMT,CAFE19,MNUMYR)				! Total FCEV incremental cost
REAL INC_COST_FCHEV(NVMT,CAFE19,MNUMYR)				! Total FCHEV incremental cost

REAL VMT_VEH(NVMT,FLT,CAFE19)               		! vmt per vehicle by non-fleet, fleet, and size class
REAL VEH_SHR(NVMT,FLT,CAFE19)               		! percent share of vehicle by non-fleet, fleet, and size class
REAL PBACK_SHR(PBK_YR,CAFE19)						! share of vehicles that require payback at a given threshold (1-7 years)
REAL npv_choice(CAFE19,FUEL12,NVMT,PBK_YR,MNUMCR-2,MNUMYR)	! Net present value of fuel/maintenance/insurance/other operating cost savings (or not)
REAL fuel_shr_regn(MNUMYR,CAFE19,FUEL12,FLT,MNUMCR) ! Fuel shares for New trucks by size class, fleet/nonfleet, region. NOTE: updated after TRUCK_ACT
REAL fuel_shr_ivmt(MNUMYR,NVMT,FLT,CAFE19,FUEL12,MNUMCR)	! Fuel shares for New trucks by size class, fleet/nonfleet, region, vmt bin.
REAL max_drop,max_rise								! Maximum YoY decrease and increase in alt-fuel powertrain market share 

REAL sales_tax_rate(MNUMCR-2)						! Truck sales-weighted state sales tax rate		
REAL fet_rate(CAFE19)								! Federal excise tax rate (Class 8 only)

REAL PT_emotorkW(CAFE19,FUEL12)                     ! Electric motor size for electrified powertrains
REAL PT_battkWh_BEV(NVMT,CAFE19,MNUMYR)				! Battery size (kWh), calculated endogenously based on relationship w/ daily range
REAL PT_battkWh_HEV(cafe19)                         ! HEV battery size 
REAL PT_battkWh_PHEV(NVMT,CAFE19)					! PHEV battery size
REAL PT_tanksize_CNG(NVMT,CAFE19)					! CNG freight truck tank size
REAL PT_tankkg_FCEV(NVMT,CAFE19)					! Fuel cell dominant FCV H2 tank size
REAL PT_tankkg_FCHEV(NVMT,CAFE19)					! Battery dominant FCV H2 tank size
REAL PT_tankkg_H2ICE(NVMT,CAFE19)					! H2ICE H2 tank size
REAL PT_tankcnt_FCHEV(NVMT,CAFE19)					! Fuel cell dominant FCV H2 tank count
REAL PT_tankcnt_FCEV(NVMT,CAFE19)					! Fuel cell dominant FCV H2 tank count
REAL PT_tankcnt_H2ICE(NVMT,CAFE19)					! H2ICE H2 tank count
REAL PT_fckW_FCEV(NVMT,CAFE19)						! Fuel cell dominant FCV fuel cell stack size
REAL PT_battkWh_FCEV(NVMT,CAFE19) 					! Fuel cell dominant FCV battery size
REAL PT_fckW_FCHEV(NVMT,CAFE19)						! Battery dominant FCV fuel cell stack size
REAL PT_battkWh_FCHEV(NVMT,CAFE19)					! Battery dominant FCV battery size
REAL cost_OBC(MNUMYR)								! On-board charger cost (2022USD)
REAL CHRG_EFF_IMP									! EVSE efficiency, annual improvement
REAL evse_maint										! EVSE maintenance cost (2022USD/kWh)
REAL insure_rate									! Insurance rate (%) applied to incremental cost for alt-fuel powertrains
REAL H2ICE_fueldisc									! Discount applied to H2 fuel for H2 ICE (less purity needed)
REAL MR_slope(CAFE19)								! Parameter for maintenance/repair, 2023USD
REAL MR_intercept(CAFE19)							! Parameter for maintenance/repair, 2023USD
REAL MR_BEVmult(CAFE19,MNUMYR)						! Multiplier to adjust Diesel ICE M&R $/mi to get BEV M&R $/mi
REAL MR_FCVmult(CAFE19,MNUMYR)						! Multiplier to adjust Diesel ICE M&R $/mi to get FCV M&R $/mi
REAL MR_MGmult										! Multiplier to adjust Diesel ICE M&R $/mi to get gasoline M&R $/mi
REAL MR_cost(CAFE19,FUEL12,PBK_YR,MNUMYR)			! M&R cost per mile, non-diesel

REAL h2_refuelopcost(NVMT,CAFE19,3),&               ! Extra daily refuelings required to meet average daily range [average]
     usable_h2,&                                    ! Ratio of usable h2 to nominal h2 (for a tank)
     h2_max_tanks(CAFE19,3), &                      ! Maximum number of tanks that can be fit on a vehicle
     h2_kg_per_tank(CAFE19,3), &                    ! kg per tank
     h2_refuel_time,&                               ! Time required to refuel
     bev_refuelopcost(NVMT,CAFE19,MNUMYR),&         ! Extra daily recharging sessions required to meet average daily range [average]
     kWh_nominal(NVMT,CAFE19,MNUMYR),&              ! Battery kWh installed on the vehicle
     max_kWh_cap(CAFE19,MNUMYR),&                   ! Maximum battery kWh that will fit on a vehicle
     charging_speed(CAFE19),&                       ! Public charging speed
     TRK_BEV_DOD,&                                  ! Depth of discharge allowed (for converting nominal to usable batt kWh)
     refuel_timeval                                 ! Value of time (labor + lost freight revenue) per hour

REAL avg_batt_kWh_FCEV(MNUMYR,flt,CAFE19,MNUMCR),&	! market-share weighted average battery capacity
	 avg_tank_cnt_FCEV(MNUMYR,flt,CAFE19,MNUMCR),&	! market-share weighted average per-vehicle H2 tank count
	 avg_tank_cnt_FCHEV(MNUMYR,flt,CAFE19,MNUMCR),&	! market-share weighted average per-vehicle H2 tank count
	 avg_tank_cnt_H2ICE(MNUMYR,flt,CAFE19,MNUMCR),&	! market-share weighted average per-vehicle H2 tank count
	 avg_batt_kWh_FCHEV(MNUMYR,flt,CAFE19,MNUMCR),&	! market-share weighted average battery capacity
	 avg_batt_kWh_BEV(MNUMYR,flt,CAFE19,MNUMCR),&	! market-share weighted average battery capacity
	 avg_batt_kWh_PHEVD(MNUMYR,flt,CAFE19,MNUMCR),&	! market-share weighted average battery capacity
	 avg_batt_kWh_PHEVG(MNUMYR,flt,CAFE19,MNUMCR)	! market-share weighted average battery capacity

REAL battsize_a_PHEV(CAFE19)						! Coefficient for battery sizing (based on daily VMT requirement from VMT_VEH)
REAL battsize_b_PHEV(CAFE19)						! Coefficient for battery sizing (based on daily VMT requirement from VMT_VEH)

REAL BEV_infra_cost(CAFE19,MNUMYR)					! Upfront investment required for EVSE equipment and installation (2021USD)
REAL BEV_elec_markup(CAFE19)						! Markup (multiplier) for retail/public electricity costs by size class
REAL NG_priceadj(2)									! Multiplier to adjust supply module NG costs up to actual {1: CNG, 2: LNG}
REAL cost_ice_CNG(CAFE19)								! Incremental cost of NG engine and aftertreatment (v. gasoline [2b-3] or diesel [4-8])
REAL cost_NGtank_DGE(SC4,MNUMYR)					! CNG tank cost ($/DGE)
REAL cost_ICE(CAFE19,MNUMYR)						! Incremental cost of ICE engine (incl. non-batt/FC/H2 components added back in); used in TRUCK_CHOICE
REAL cost_icetech(CAFE19,MNUMYR)	                ! Cost of technology adopted in TRUCK_TECHADOPT; part of cost_ICE; used in TRUCK_CHOICE
REAL lonox_cost(CAFE19,MNUMYR)                      ! Cost to comply with Low NOx rule; part of cost_ICE; used in TRUCK_CHOICE
REAL cost_ICE_PHEV(CAFE19,2,MNUMYR)				    ! Total cost savings for removing non-PHEV ICE components and adding PHEV ICE components (can be negative) {1: diesel, 2: gasoline)
REAL cost_ICE_HEV(CAFE19,MNUMYR)                    ! Total cost savings for removing non-HEV ICE components and adding HEV ICE components (can be negative)
REAL cost_ICE_H2ICE(CAFE19,MNUMYR)                  ! Total cost savings for removing non-H2ICE ICE components and adding H2ICE ICE components (can be negative)
REAL cost_ICE_USDyr									! USD year for component costs
REAL cost_DEF(MNUMYR)								! DEF per-gallon cost (2022USD)
REAL DEF_dose										! DEF dose rate (percent of diesel consumption)
REAL CPM_DEF(MNUMYR,CAFE19,MNUMCR)					! DEF cost per mile by icafe19 type

REAL cost_battpack_a(5)								! Initial pack manufacturing cost (param a), 2020USD/kWh, {1: Class 4-5, 2: Class 6-7V, 3: Class 7/8 Tractor, 4: 2b/3, 5: HEV}
REAL cost_battpack_lr(5)							! Learning rate, pack manufacturing
REAL cost_battpack_b(5)	
REAL cost_battmat_a(5)								! Initial active materials/synthesis cost (param a), 2020USD/kWh, {1: Class 4-5, 2: Class 6-7V, 3: Class 7/8 Tractor, 4: 2b/3, 5: HEV}
REAL cost_battmat_lr(5)								! Learning rate, active materials/synthesis
REAL cost_battmat_b(5)	
REAL trk_battc_hist(5)                              ! Ratio of freight truck battery prices to LDV (EV300) battery price, for filling in frt trk prices
REAL cost_h2tank_lr									! Learning rate for H2 storage tank costs
REAL cost_h2tank_a									! Initial cost per kg for H2 storage tanks
REAL cost_h2tank_b 									! Exponent value for H2 storage tank cost calculation
REAL cost_fcstack_lr								! Learning rate for fuel cell stack costs
REAL cost_fcstack_a 								! Initial cost per kW for fuel cell stacks
REAL cost_fcstack_b 								! Exponent value for fuel cell stack cost calculation
REAL cost_emotor_a 									! Inital cost per kW for motors
REAL cost_h2tank_rd									! YoY R&D tank cost reduction in the absence of non-negligible FCEV sales
REAL cost_fcstk_rd									! YoY R&D FC stack cost reduction in the absence of non-negligible FCEV sales
REAL cost_FCkW(MNUMYR)								! Fuel Cell system cost ($/kW)
REAL cost_motorkW(MNUMYR)							! BEV/FCEV/FCHEV Motor cost ($/kW)
REAL cost_h2tank(MNUMYR)							! FCEV/FCHEV hydrogen storage cost ($/kg)

REAL phev_mpg_adj									! PHEV mpg is a combination of BEV and ICE mpg -- this multiplier reduces that combined mpg due to add'l weight of two powertrains
REAL phev_chgsus_mult(CAFE19,2)						! Applied to conventional DS/MG fuel economy to estimate charge-sustaining PHEV mpg
REAL TRK_PHEV_DOD								    ! Depth of discharge allowed (for converting nominal to usable batt kWh)

REAL PHEV_eVMT_share(MNUMYR,flt,CAFE19,MNUMCR,2)	! PHEV eVMT share (needed for allocating energy consumption to H2 and Elec)
REAL PctEVMT_PHEV_AVG(MNUMYR,MNUMCR,2)				! Aggregate average stock-average share of PHEV VMT that is on electricity {1: DS, 2:MG}

!...IRA Tax credits
INTEGER IRA_switch									! {0: IRA not enacted, 1: IRA enacted}
REAL IRA_45W_max(CAFE19,FUEL12)						! Maximum allowable IRA 45W tax credit
REAL EVSE_30Ctaxcred								! Average tax credit for freight truck EVSE

!...Low NOx parameters
INTEGER NOX_switch									! {0: Low NOx enforced, 1: Low NOx not enforced}
REAL cost_LoNOx(CAFE19,FUEL12)							! EPA MY2027 Low NOx rule compliance costs {1: diesel, 2: gasoline}
REAL DEF_dose_LoNOx									! DEF dose rate (percent of diesel consumption); MY2027+, Low NOx rule

!...CA Advanced Clean Trucks Rule parameters
REAL act_req_2b3(MNUMCR,MNUMYR)						! ZEV sales share requirement by CD, 2b/3
REAL act_req_48v(MNUMCR,MNUMYR)						! ZEV sales share requirement by CD, 4-8 vocational
REAL act_req_78trac(MNUMCR,MNUMYR)					! ZEV sales share requirement by CD, 7&8 tractor
REAL act_wgtmod(CAFE19)							    ! Weight modifier (multiplied against sales for deficit/credit calcs)
REAL ZEVreqshr78(2)									! Share of ACT Class 78T deficits to apply to Class 8 daycab (1) and Class 8 Sleeper (2)
REAL phev_factor									! Multiplier applied to weight class modifier for PHEVs
INTEGER ACT_switch									! {0: ACT not enforced, 1: ACT enforced}
REAL ZEVreq(MNUMCR,FLT,CAFE19,MNUMYR)				! Additional sales required to meet ACT, by icafe19
REAL act_comp_cost(CAFE19,MNUMCR,MNUMYR)            ! Estimated cost to comply with ACT (1990USD)

!...Phase 2/3 GHG HDV Standards parameters
INTEGER     CAFE_P3_SWITCH
INTEGER*2 	TECHFYR_P2(TECHP2,CAFE14,FUEL12)   		! First year technology is viable by phase-2 cafe class and fuel type
INTEGER*2 	REQUIRED_P2(TECHP2)          			! Which technologies require which
INTEGER*2 	TECH_REQ(TECHP2,2)           			! Required technologies 
INTEGER*2 	REQUIRES_TECH(3,TECHP2)      			! = [REQUIRED_P2 TECH_REQ]
INTEGER*2 	REQUIRED_ANY_P2(TECHP2)      			! Which technologies require which
INTEGER*2 	TECH_R_ANY(3,TECHP2)         			! Required technologies
INTEGER*2 	REQ_ANY_TECH(4,TECHP2)       			! = [REQUIRED_P2 TECH_REQ]
INTEGER*2 	TECH_NUM(TECHP2)             			! Technology number
INTEGER*2 	TECHLEARN_P2(TECHP2,CAFE14)  			! Technology cost learning curve assignments
INTEGER*2 	PAYBACK_P2(CAFE14)           			! Payback period for technology by phase-2 cafe class
INTEGER*2 	PAYBACK1_P2(TECHP2,CAFE19)
INTEGER*2 	SUPERSCEDE_P2(TECHP2)        			! Which technologies superscede which
INTEGER*2 	TECH_SUP(TECHP2,9)           			! Superscede technologies
INTEGER*2 	SUPERSEDES_TECH(10,TECHP2)   			! = [SUPERSEDED_P2 TECH_SUP]
INTEGER*2 	SYNERGY_P2(TECHP2)           			! Which technologies have synergies 
INTEGER*2 	TECH_SYN(TECHP2,1)           			! Synergy technologies
INTEGER*2 	SYNERGY_TECH(2,TECHP2)       			! = [SYNERGY_P2 TECH_SYN]
REAL 		BASE_MPG_P2(FUEL12,CAFE19)         		! Base MPG by gvw class
REAL 		NEW_MPG_19(MNUMYR,FUEL12,CAFE19)   		! TESTED New MPG by 19 cafe classes
REAL 		NEW_MPG_P2(MNUMYR,FUEL12,CAFE14)   		! TESTED New MPG by 14 cafe classes
REAL 		BASE_MKT_P2(FUEL12,CAFE19)         		! Base market share by gvw class
REAL 		TRIGGER_PRICE_P2(TECHP2,CAFE19,FUEL12)
REAL 		TECHEFF_P2(TECHP2,CAFE14)         		! Gain in efficiency from technology adoption by phase-2 cafe class
REAL 		TECHCOST_P2(TECHP2,CAFE14)        		! Cost of technology adoption by phase-2 cafe class
REAL 		TECHVAR_P2(TECHP2,CAFE14)         		! Fuel price sensitivity parameter
REAL 		TECHMID_P2(TECHP2,CAFE14)         		! Number of years until 50% market penetration
REAL 		TECHSHAPE_P2(TECHP2,CAFE14)       		! Market penetration shape constant
REAL 		TECHBASE_P2(TECHP2,CAFE19,FUEL12)  		! Base market penetration of technologies -- from final AEO2022 run with Phase 1 regulations
REAL 		TECHMAX_P2(TECHP2,CAFE14)         		! Maximum market share
REAL 		TECH_PER(TECHP2,1)                		! Synergy percent increase/decrease in fuel economy
REAL 		SYN_PER(2,TECHP2)                 		! = tech_per
REAL 		HDV_GHGSTD(CAFE14,MNUMYR,FUEL12)	    ! HDV fuel consumption and GHG emissions standards expressed in gCO2/ton-mile
INTEGER*2   HDV_STD_MAP(FUEL12,CAFE14)              ! Which standard (CI or SI) does each powertrain use?
REAL 		LEARNING_P2(14,MNUMYR)            		! Technology cost learning curve reductions
REAL 		DISCRTXG_P2                       		! Discount rate
REAL 		DISCRTXGL_P2                      		! Discount rate following the standard
REAL 		PREFF_P2(TECHP2,CAFE19,FUEL12)     		! Market penertation price sensitvity mulitpler
REAL 		VMTFLT_19(MNUMYR,CAFE19,AGE,FUEL12) 	! Total VMT
REAL 		TRKSTK_19(MNUMYR,CAFE19,AGE,FUEL12)  	! Stock total by 19 different CAFE size classes
REAL 		CREDSALES_19(MNUMYR,CAFE19,FUEL12)
REAL 		credsales_p2(MNUMYR,CAFE14,FUEL12)
LOGICAL 	HDV_passGHG(mnumyr,CAFE4)
REAL        GHG_creds_agg(mnumyr,CAFE4)             ! Total ghg credits (could be negative) generated in a given year and averaging set
REAL		TECHSHR_P2(MNUMYR,techp2,CAFE19,FUEL12)
REAL        HDV_DF(FUEL12,CAFE14)                   ! Degradation factor (from lab/GEM, to on-road, mpg)
REAL        NEW_GCO2TONMI_P2(MNUMYR,FUEL12,CAFE19)  ! New gCO2/mile by 19 size classes
REAL        GCO2_GAL(FUEL12,CAFE14)                 ! Conversion from gallons/mile to gCO2/mile
REAL        HDV_USEFULLIFE(FUEL12,CAFE14)           ! Regulatory useful life for estimating GHG emission credits
REAL        HDV_AFV_CREDITS(FUEL12,MNUMYR)          ! Alternative technology multipliers
REAL        EPA_PAYLOAD(CAFE14)                     ! Regulatory payload for conversion from gCO2/mile to gCO2/ton-mile
REAL        ghg_comp_cost(CAFE19,MNUMCR,MNUMYR)     ! Estimated cost to comply with GHG (1990USD)

INTEGER*2 	TECH_CNT_SUP(TECHP2), TECH_CNT_SYN(TECHP2), tech_cnt_req(techp2), NUM_SUP, NUM_SYN, NUM_REQ 

REAL FUELBTUR(SC4,7,FLT,MNUMCR)     				! Freight truck fuel consumption by fuel
REAL FUELDMDR(SC4,FUEL12,FLT,MNUMCR)     			! Freight truck fuel consumption by powertrain
REAL PRAFDFXG(SC4,FUEL12)               			! Parm: variation  AFV Mtk share due to diff fuel prices
REAL SURV_RATE(AGE,MNUMCR,11,FUEL12)    			! Truck scrappage rate (combined)

REAL TRKSTK(MNUMYR,SC,AGE,FUEL12,FLT)      			! truck population (year,size class, vintage,fuel, nonflt/fleet)
REAL(4), allocatable :: TRKSTK_19R(:,:,:,:,:,:)     ! truck population history by year, phase 2 size class (19), vintage, fuel type, vocational, fleet, and region 

REAL TRKSAL_SC(MNUMYR,SC4,FUEL12)					! Truck sales aggregated to 4 reported size classes
REAL TRKSAL_P2(MNUMYR,CAFE14,FUEL12)				! Truck sales aggregated to 14 Phase 2 size classes

REAL TSTK_PER_P2(MNUMYR,CAFE19,FUEL12)   			! Percent of total sales in a size class (19) out of Phase 2 size class
REAL TFFXGRT(AGE,SC4)                   			! Exog % of trucks/vintage transfered from fleet to non_fleet
REAL VMTFLT(cur,SC,AGE,FUEL12,FLT)       			! VMT at its most detailed
REAL VMTFLTR(cur,SC,AGE,FUEL12,FLT,MNUMCR) 			! VMT at its most detailed by census division
REAL FLTSHR_STK(MNUMYR,CAFE19)						! Share of on-road stock that is fleet (large, rental, gov, or dealer/mfr)
REAL FLTSHR_SALES(MNUMYR,CAFE19)					! Share of new vehicle sales that were to fleets (large, rental, gov, or dealer/mfr)
REAL VMTDMDR(MNUMYR,MNUMCR)            				! VMT by census division
REAL TR_VMT(MNUMYR,MNUMCR)             				! Total truck vehicle miles travelled (billion)
REAL THIST_VMT(MNUMYR,MNUMCR-2)        				! Historical truck vehicle miles travelled (billion) by census divsion
REAL TTM_OUTPUT(MNUMCR-2,SEC-2)        				! Truck ton-miles per $ output
REAL TTONMI_ORIG(MNUMCR-2,SEC-2)       				! Truck ton-miles by census division and industrial sector for base year
REAL NEWTRUCKS_regn(MNUMYR,CAFE19,FLT,MNUMCR) 		! Sales of new trucks by size clase, fleet/nonfleet, and region

REAL HARMONIC_MEAN                     			! Function to calculate average mpg weighted by VMT or MPG
REAL HDV_STANDARD_C(MNUMYR,CAFE14)     			! HDV fuel consumption and GHG emissions standards expressed in MPG combined across fuels
REAL INC_TECH_COST(MNUMYR,FUEL12,SC4)			! Incremental cost for tech adoption, aggregated to 4 size classes
REAL inc_tech_cost_19(MNUMYR,CAFE19,FUEL12)		! Incremental cost for tech adoption
REAL PHEVELECVMT(CAFE19)                       	! Elec motor share of VMT for PHEV engines
REAL CLTSIC(MNUMYR)                    			! SIC output averaged across 6 categories, for Class 2b

EXTERNAL HARMONIC_MEAN

! ... Connected and autonomous freight fleets
INTEGER TOONYEAR*2
REAL TOONSHR(MNUMYR)

! ... Commercial light truck module
REAL CLTSALESPER(6,mnumyr)                  ! Percent of new commercial light truck sales by fuel 1995-2011
REAL CLTSTK(mnumyr,FUEL12,AGE,2,mnumcr)     ! Commercial light truck stock by year, powertrain, vintage, voc, and census division
REAL CLTVMT(mnumyr,FUEL12,AGE)              ! Commercial light truck total vmt by year, powertrain, and vintage
REAL CLTMPG_YR(mnumyr,FUEL12,AGE)           ! MPG by year rather than cur/lag
REAL CLTVMTDIST(6)                          ! Distribution of CLT VMT by industry

! ... Waterborne Freight Module
! ... Domestic Waterborne
REAL DOMSHIP_FUEL_SHR(4,MNUMYR)     			! fuel share
REAL DSHIST_TONMI(MNUMYR, MNUMCR-2) 			! historic total domestic marine ton-miles
REAL DSTM_OUTPUT(MNUMCR-2,16)       			! domestic marine ton-miles per $ output
REAL ANN_DECLINE(MNUMYR)            			! domestic marine annual rate of ton-mile per $ output decline
REAL TQDSHIPT(MNUMYR,MNUMCR)        			! domestic marine energy demand
REAL DTM_SHARES(MNUMCR-2,16)        			! distribution of ton-miles by industrial sector within each census division
REAL DSADD_TONMI(MNUMYR,MNUMCR,16) 				! domestic marine ton-miles by census division and industrial sector

! ... International Waterborne
REAL ISFD(5,MNUMYR)                             ! energy demand by fuel type
                                                            !   1) diesel
                                                            !   2) residual
                                                            !   3) CNG
                                                            !   4) LNG
                                                            !   5) low sulfur fuel oil

REAL INTSHIP_FUEL_SHR(5,MNUMYR)                 ! fuel share
REAL INTSHIPLOGIT_FAC(5,3)                 		! price selection factor for fuel shares
REAL LSFORISK(MNUMYR)                           ! risk associated with the fuel availability of lsfo (IMO 2020)

REAL FUELCONS(MNUMCR-2,9)                       ! history/base year for computed ECA consumption by census division (9) and vessel and sub-vessel type (9)
REAL FLEETTO(9)                                 ! fleet turnover, by vessel and sub-vessel type, starting in base year, currently denoted as a constant
REAL EFFINC(9)                                  ! improvement per year in main engine efficiency, as a percentage
REAL Q_SHIPBUNKDS(MNUMYR)                       ! Total vessel-bunkered diesel fuel (source: EIA 821)

! ... for timing code
REAL finish, start

! ... Global definitions
integer      iy                                 ! year index corresponding to first year of inputs
integer      num_to_read                        ! number of years in input ranges
integer      First_Read_Year                    ! = 1995
INTEGER      N                                  ! tran variable for curiyr
INTEGER      YRS                                ! actual model year (1989+curiyr)

!...Map 14 CAFE groups to 19 size classes
    data P219Map/1,2,1,2,2,2,3,3,5,6,7,4,8,9,10,11,12,13,14/
!...Map 4 size categories into 19 size classes
    data SC19Map/4,4,1,1,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3/
!...Map 4 EPA GHG averaging sets into 14 size classes
    data P24Map/1,2,3,3,3,3,3,4,4,4,4,4,4,4/
!...Map 19 size classes into groupings to report technology adoption
    data TechRptMap/1,1,1,1,2,2,2,3,4,4,4,3,4,4,4,5,5,5,5/
!...Map GVWR categories into 19 size classes
	data GVWRP2Map/2,2,3,3,4,5,6,7,7,7,7,8,8,8,8,8,8,8,9/
!...Map of vocational/not for 19 size classes (1=Not,2=Voc)
	data VocOrNoMap/1,2,1,2,2,2,2,2,1,1,1,2,1,1,1,1,1,1,1/
!...Map 19 size classes into the 11 available survival curves
	data SurvMap/1,2,3,4,5,6,7,9,8,8,8,11,10,10,10,10,10,10,10/
!...Map 12 freight truck powertrain types to 12 commercial light truck powertrain types (gas/diesel flipped)
!   2 maps 7 fuel types in a similar fashion
    data CLTMap/2,1,3,4,5,6,7,8,9,10,11,12/
    data CLTMap2/2,1,3,4,5,6,7/
    SAVE
    
end module F_
!=============================================================================================================
!...Freight subroutines
  SUBROUTINE TRANFRT(FRTEFF,ICALL)
  USE F_
  IMPLICIT NONE
    integer FRTEFF        ! (was Technology scenario 0: none 1:high tech 2: frozen)
    INTEGER ICALL, once/.false./
    REAL :: VMTFLT_SAFF_TR(SC), VMT_TRR(MNUMYR)
    
    IF(.not. allocated(TRKSTK_19R)) allocate(TRKSTK_19R(MNUMYR,CAFE19,age,FUEL12,flt,MNUMCR))

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
	HRATE(:,10) = 138700                   ! Hydrogen large Batt
	HRATE(:,11) = 125071                   ! HEV gasoline
	HRATE(:,12) = 138700                   ! H2 ICE

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
    IF(curcalyr.gt.2017) then
      CALL TRUCK_NEW
!	  WRITE(*,*)'TRANFRT TRUCK_NEW Complete', iyr+1989
    ENDIF

    CALL TRUCK_STOCK
!	WRITE(*,*)'TRANFRT TRUCK_STOCK Complete', iyr+1989

!...Estimate aggregate VMT and per truck VMT
    CALL TRUCK_VMT
!	WRITE(*,*)'TRANFRT TRUCK_VMT Complete', iyr+1989

    CALL TCOMMCL_TRK
!	WRITE(*,*)'TRANFRT TCOMMCL_TRK Complete', iyr+1989

!...Calculate fuel demand from vmt and mpg
    CALL TRUCK_FUEL
!	WRITE(*,*)'TRANFRT TRUCK_FUEL Complete', iyr+1989


!...aggregate vmt by sector and vintage sent to TRAN for Table 7
    DO isc=1,sc
      DO ifuel=1,FUEL12
        DO iregn=1,MNUMCR
          DO iage=1,age
            VMTFLT_SF_TR(ISC,IAGE,IFUEL,iregn) =  sum( VMTFLTR(cur,ISC,IAGE,IFUEL,1:FLT,iregn) )
          ENDDO
          VMTFLT_SAF_TR(ISC,IFUEL,iregn) = sum( VMTFLT_SF_TR(ISC,1:AGE,IFUEL,iregn) )
        ENDDO
      ENDDO
        VMTFLT_SAFF_TR(ISC) =sum( VMTFLT_SAF_TR(ISC,1:FUEL12,1:MNUMCR-2) )
    ENDDO

    VMT_TRR(IYR) = sum(VMTFLT_SAF_TR(1:SC,1:FUEL12,1:MNUMCR-2) )

!...average MPG over vintages, fuels, and size class
    DO isc=1,sc
      DO ifuel=1,FUEL12
        HDV_MPG_S_F(IYR,ISC,IFUEL) = HARMONIC_MEAN(HDV_MPG(iyr,ISC,1:AGE,IFUEL),VMTFLT_SF_TR(ISC,1:AGE,IFUEL,11),AGE)
	  IF(HDV_MPG_S_F(IYR,ISC,IFUEL).eq.0) HDV_MPG_S_F(IYR,ISC,IFUEL) = HDV_MPG_S_F(IYR-1,ISC,IFUEL)
      ENDDO
      HDV_MPG_S(IYR,ISC) = HARMONIC_MEAN(HDV_MPG_S_F(IYR,ISC,1:FUEL12), VMTFLT_SAF_TR(ISC,1:FUEL12,11),FUEL12)
	  IF(HDV_MPG_S(IYR,ISC).eq.0) HDV_MPG_S(IYR,ISC) = HDV_MPG_S(IYR-1,ISC)
    ENDDO

!...Average MPG over size classes - sent to TRAN to determine bus efficiency
    FTMPG_TR(IYR) = HARMONIC_MEAN( HDV_MPG_S(IYR,1:SC),VMTFLT_SAFF_TR(1:SC),SC)

!...Duplicate average MPG sent to Table 7 (ftab)
    TRFTMPG(IYR) = FTMPG_TR(IYR)
	
!	WRITE(21,*)'TRANFRT Complete', iyr+1989

  RETURN
  END SUBROUTINE TRANFRT

! CONTAINS

!=============================================================================================================
 SUBROUTINE TRUCK_NEW
 USE F_
 IMPLICIT NONE
 include 'angtdm'
 INCLUDE 'EUSPRC'

	INTEGER i
	REAL new_gwh, &											! annual GWh of battery capacity additions  
		 new_fc_stacks, &     								! annual Fuel Cell Stack additions
		 new_h2_tanks     									! annual H2 Storage Tank additions
    REAL TECHRPT_SAL(MNUMYR,10,FUEL12)

	IF (curcalyr.gt.BSYR_STK)then
!...  Distribute new truck sales (from macro) by region and fleet/non-fleet based on historical sales (iage=1)
!	  Note that the fleet sales distribution is different from the stock (fleets account for a much larger share of sales than stocks)
	  NEWTRUCKS_regn(iyr,:,:,:) = 0.0
	  DO iregn = 1,MNUMCR-2
		DO icafe19 = 1,CAFE19
		  IF (icafe19.le.2) then
		    NEWTRUCKS_regn(iyr,icafe19,FLT,iregn) = SUM(MC_Vehicles(1:2,iyr))*(1-LTSplit(iyr)) * 1000000. * &
													SUM(TRKSTK_19R(iyr-1,icafe19,1,:,:,iregn))/SUM(TRKSTK_19R(iyr-1,1:2,1,:,:,MNUMCR)) * &
													FLTSHR_SALES(iyr,icafe19)
!		    WRITE(21,'(a,4(",",i4),3(",",f12.0),2(",",f8.4))')'newtrucks',curcalyr,curitr,iregn,icafe19,SUM(MC_Vehicles(1:2,iyr)),SUM(TRKSTK_19R(iyr-1,icafe19,1,:,:,iregn)),&
!                                                        SUM(TRKSTK_19R(iyr-1,1:2,1,:,:,MNUMCR)),(1-LTSplit(iyr)),FLTSHR_SALES(iyr,icafe19)
          ELSEIF (icafe19.le.4) then
		    NEWTRUCKS_regn(iyr,icafe19,FLT,iregn) = MC_VEHICLES(3,iyr) * 1000000. * &
													SUM(TRKSTK_19R(iyr-1,icafe19,1,:,:,iregn))/SUM(TRKSTK_19R(iyr-1,3:4,1,:,:,MNUMCR)) * &
													FLTSHR_SALES(iyr,icafe19)
		  ELSE
		    NEWTRUCKS_regn(iyr,icafe19,FLT,iregn) = MC_SUVTHAM(iyr)    * 1000000. * &
													SUM(TRKSTK_19R(iyr-1,icafe19,1,:,:,iregn))/SUM(TRKSTK_19R(iyr-1,5:19,1,:,:,MNUMCR)) * &
													FLTSHR_SALES(iyr,icafe19)
		  ENDIF
!	      Populate non-fleet		
		  NEWTRUCKS_regn(iyr,icafe19,NFT,iregn) = NEWTRUCKS_regn(iyr,icafe19,FLT,iregn)/FLTSHR_SALES(iyr,icafe19)*(1-FLTSHR_SALES(iyr,icafe19))
		ENDDO
		IF (iregn.eq.9) then
		  DO iflt = 1, flt
		    DO icafe19 = 1, CAFE19
			  NEWTRUCKS_regn(iyr,icafe19,iflt,11) = SUM(NEWTRUCKS_regn(iyr,icafe19,iflt,1:MNUMCR-2))
			ENDDO
		  ENDDO
		ENDIF
	  ENDDO
    ENDIF
	
!...calculate average fuel prices over the last 5 years	
	DO ifuel = 1,FUEL12
      DO iregn = 1,MNUMCR-2
       FUELPRICE_R_AVG(ifuel,iregn) = SUM(FUELPRICE_R(iyr-4:iyr,ifuel,iregn))/5.0 
      ENDDO
      FUELPRICE_R_AVG(ifuel,MNUMCR) = SUM(FUELPRICE_R(iyr-4:iyr,ifuel,MNUMCR))/5.0 
	ENDDO	

	call cpu_time(start)
	IF (curcalyr.gt.2017) THEN
	  DO itryghg = 1, GHGTRY
        
        if (itryghg.eq.1) HDV_passGHG(iyr,:) = .false.
        
	    CALL TRUCK_TECHADOPT
        
!       Estimate alternative powertrain adoption, after building truck options based on tech adoption above	  
!	    CALL TRUCK_INCCOST        
	    IF (curcalyr.gt.BSYR_STK) THEN
          CALL TRUCK_INCCOST
          CALL TRUCK_CHOICE
        ENDIF

	    IF (ACT_switch.eq.1.and.curcalyr.ge.2021) CALL TRUCK_ACT

!	    Re-estimate aggregate sales, credit, and mpg variables now that fuel shares have changed	  
	    CALL TRUCK_AGGVARS
        
        CALL TRUCK_GHGMEET

        IF (ALL(HDV_passGHG(iyr,:))) THEN
          if(fcrl.eq.1) WRITE(21,'(a,i4,i2)')'Freight truck GHG met in ',curcalyr,itryghg
          EXIT
        ENDIF

!       If the reg has not been met, and we've reached the maximum number of attempts, 
!       convert sales to ZEVs to meet the reg.
        IF(.not.ALL(HDV_passGHG(iyr,:)).and.itryghg.eq.GHGTRY.and.curcalyr.ge.2021) THEN
          if(fcrl.eq.1) WRITE(21,'(a,i4,i2)')'Entering TRUCK_GHGZEV in ',curcalyr,itryghg
          CALL TRUCK_GHGZEV
          
          CALL TRUCK_AGGVARS
          
          IF (ALL(HDV_passGHG(iyr,:))) THEN
            if(fcrl.eq.1) WRITE(21,'(a,i4,i2)')'Freight truck GHG met with ZEVs in ',curcalyr,itryghg
            EXIT
          ELSE
          	DO icafe4 = 1, cafe4
              IF (.not.HDV_passGHG(iyr,icafe4).and.fcrl.eq.1) WRITE(21,'(a,4(",",i4),3(",",f12.2))')'ERROR: GHG FAIL',curcalyr,curitr,itryghg,icafe4,GHG_creds_agg(iyr,icafe4)
            ENDDO
          ENDIF
        ENDIF
        
	  ENDDO
	ENDIF
    
    call cpu_time(finish)      ! Stop Timer
!	WRITE(21,'(a,3(",",i4),8(",",f5.2))')'timer_TRANFRT_TRUCKNEW',curcalyr, curitr, itryghg, finish - start

    IF (curcalyr.gt.BSYR_STK)then
!	  Accumulate sales of tech that uses learning curves (batteries, fuel cell stacks, hydrogen tanks)
      avg_batt_kWh_BEV(iyr,:,:,:)   = 0.0
      avg_batt_kWh_PHEVD(iyr,:,:,:) = 0.0
      avg_batt_kWh_PHEVG(iyr,:,:,:) = 0.0
      avg_batt_kWh_FCEV(iyr,:,:,:)  = 0.0
      avg_tank_cnt_FCEV(iyr,:,:,:)  = 0.0
      avg_batt_kWh_FCHEV(iyr,:,:,:) = 0.0
      avg_tank_cnt_FCHEV(iyr,:,:,:) = 0.0
      avg_tank_cnt_H2ICE(iyr,:,:,:) = 0.0
      PHEV_eVMT_share(iyr,:,:,:,:)  = 0.0
	  DO iregn=1,MNUMCR-2
	    DO iflt=1,flt
	  	  DO icafe19=1,CAFE19
            isc = SC19Map(icafe19)
            DO ifuel = 6,FUEL12
!             First calculate average component sizes (vmt-bin-based, need to aggregate up)
			  IF(SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn)).gt.0.0)then
				DO ivmt=1, nvmt
			      IF (ifuel.eq.6) THEN
                    avg_batt_kWh_BEV(iyr,iflt,icafe19,iregn)    = avg_batt_kWh_BEV(iyr,iflt,icafe19,iregn) &
                                                                + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))&
                                                                   * kWh_nominal(ivmt,icafe19,iyr)
!                                                                  * PT_battkWh_BEV(ivmt,icafe19,iyr)
				  ELSEIF(ifuel.eq.7) THEN
                    avg_batt_kWh_PHEVD(iyr,iflt,icafe19,iregn)  = avg_batt_kWh_PHEVD(iyr,iflt,icafe19,iregn) &
					  										    + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))*PT_battkWh_PHEV(ivmt,icafe19)
                    PHEV_eVMT_share(iyr,iflt,icafe19,iregn,1)   = PHEV_eVMT_share(iyr,iflt,icafe19,iregn,1) &
                                                                + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn)) &
                                                                  * MIN(1.0,((PT_battkWh_PHEV(ivmt,icafe19) * TRK_PHEV_DOD * 3412.0 / HRATE(isc,6)) * &	! usable battery size in dge
                                                                              new_mpg_19(iyr,6,icafe19)*phev_mpg_adj) * 250.0 / &					    ! fuel economy in mpdge   (now we have annual battery miles of range available)
                                                                              VMT_VEH(ivmt,iflt,icafe19))
                  ELSEIF(ifuel.eq.8) THEN
                    avg_batt_kWh_PHEVG(iyr,iflt,icafe19,iregn)  = avg_batt_kWh_PHEVG(iyr,iflt,icafe19,iregn) &
					  										    + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))*PT_battkWh_PHEV(ivmt,icafe19)
                    PHEV_eVMT_share(iyr,iflt,icafe19,iregn,2)   = PHEV_eVMT_share(iyr,iflt,icafe19,iregn,2) &
                                                                + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn)) &
                                                                  * MIN(1.0,((PT_battkWh_PHEV(ivmt,icafe19) * TRK_PHEV_DOD * 3412.0 / HRATE(isc,6)) * &	! usable battery size in dge
                                                                              new_mpg_19(iyr,6,icafe19)*phev_mpg_adj) * 250.0 / &					    ! fuel economy in mpdge   (now we have annual battery miles of range available)
                                                                              VMT_VEH(ivmt,iflt,icafe19))
                  ELSEIF(ifuel.eq.9) THEN
                    avg_batt_kWh_FCEV(iyr,iflt,icafe19,iregn)   = avg_batt_kWh_FCEV(iyr,iflt,icafe19,iregn) &
                                                                + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))&
															      * PT_battkWh_FCEV(ivmt,icafe19)
                    avg_tank_cnt_FCEV(iyr,iflt,icafe19,iregn)   = avg_tank_cnt_FCEV(iyr,iflt,icafe19,iregn) &
                                                                + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))&
															      * PT_tankcnt_FCEV(ivmt,icafe19)
				  ELSEIF(ifuel.eq.10) THEN  
                    avg_batt_kWh_FCHEV(iyr,iflt,icafe19,iregn)  = avg_batt_kWh_FCHEV(iyr,iflt,icafe19,iregn) &
                                                                + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))&
															      * PT_battkWh_FCHEV(ivmt,icafe19)
				    avg_tank_cnt_FCHEV(iyr,iflt,icafe19,iregn)  = avg_tank_cnt_FCHEV(iyr,iflt,icafe19,iregn) &
                                                                + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))&
															      * PT_tankcnt_FCHEV(ivmt,icafe19)
                  ELSEIF(ifuel.eq.12) THEN
                    avg_tank_cnt_H2ICE(iyr,iflt,icafe19,iregn)  = avg_tank_cnt_H2ICE(iyr,iflt,icafe19,iregn) &
                                                                + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))&
															      * PT_tankcnt_H2ICE(ivmt,icafe19)
                  ENDIF
                ENDDO
			  ENDIF
            ENDDO	! ifuel
	  	  ENDDO	    ! icafe19
	  	ENDDO		! iflt
	  ENDDO		! iregn
      
	  DO iregn=1,MNUMCR-2
	    DO iflt=1,flt
	  	  DO icafe19=1,CAFE19
            DO ifuel = 6,FUEL12
!	   		  Sum up new GWh of BEV and PHEV battery added to stock and add back to tran.f
	  		  IF (ifuel.ge.6) THEN
	  		 	IF (ifuel.eq.6) new_gwh = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) * (avg_batt_kWh_BEV(iyr,iflt,icafe19,iregn))/ 1000000.0 
	  		 	IF (ifuel.eq.7) new_gwh = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) * (avg_batt_kWh_PHEVD(iyr,iflt,icafe19,iregn))/ 1000000.0 
	  		 	IF (ifuel.eq.8) new_gwh = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) * (avg_batt_kWh_PHEVG(iyr,iflt,icafe19,iregn))/ 1000000.0
                IF (ifuel.eq.9) new_gwh = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) * (avg_batt_kWh_FCEV(iyr,iflt,icafe19,iregn))/ 1000000.0
                IF (ifuel.eq.10)new_gwh = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) * (avg_batt_kWh_FCHEV(iyr,iflt,icafe19,iregn))/ 1000000.0
	  	        IF (ifuel.eq.11)new_gwh = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) * PT_battkWh_HEV(icafe19)/ 1000000.0 
                cumulative_gwh(iyr) = cumulative_gwh(iyr) + new_gwh
              ENDIF
	  		  
!	  		  Sum up new Fuel Cell Stacks and H2 Tanks added to stock
	  		  IF (ifuel.ge.9.and.ifuel.ne.11) then
!               Initialize using previous year's cumulative production
	  			IF (iregn==1 .and. iflt==1 .and. icafe19==1.and.ifuel.eq.9) then
	  			  cumulative_fc_stacks(iyr) = cumulative_fc_stacks(iyr-1)
	  			  cumulative_h2_tanks(iyr) = cumulative_h2_tanks(iyr-1)
                ENDIF
                
!               FCEV and FCHEV add H2 tank and fuel cell stack production
                IF (ifuel.eq.9.or.ifuel.eq.10) then
                  new_fc_stacks = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn)
	  			  IF (ifuel.eq.9) new_h2_tanks = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn)*avg_tank_cnt_FCEV(iyr,iflt,icafe19,iregn)
	  			  IF (ifuel.eq.10)new_h2_tanks = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn)*avg_tank_cnt_FCHEV(iyr,iflt,icafe19,iregn)
                  cumulative_fc_stacks(iyr) = cumulative_fc_stacks(iyr) + new_fc_stacks
	  			  cumulative_h2_tanks(iyr) = cumulative_h2_tanks(iyr) + new_h2_tanks
                  
!               H2 ICE adds H2 tank production	  			
	  			ELSEIF (ifuel.eq.12) then
                  new_h2_tanks = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn)*avg_tank_cnt_H2ICE(iyr,iflt,icafe19,iregn)
	  			  cumulative_h2_tanks(iyr) = cumulative_h2_tanks(iyr) + new_h2_tanks
                ENDIF
	  		  ENDIF
            ENDDO	! ifuel
	  	  ENDDO	    ! icafe19
	  	ENDDO		! iflt
	  ENDDO		! iregn
	ENDIF ! gt.bsyr_stk

!...Aggregate 19 Phase 2 size class technology shares into groups for reporting in ftab
    IF (curcalyr.gt.2017) THEN
      TECHSHARE(iyr,:,:,:) = 0.0
	  DO itechp2=1,techp2
        TECHRPT_SAL(iyr,:,:)= 0.0
!       for report writer sum tech shares for diesel and gasoline
        DO ifuel=1,2
	  	  DO icafe19 = 1, cafe19
	  	    itechrpt19 = TechRptMap(icafe19)
	  	    TECHSHARE(iyr,itechp2,itechrpt19,ifuel) = TECHSHARE(iyr,itechp2,itechrpt19,ifuel) + techshr_P2(iyr,itechp2,icafe19,ifuel) * TRKSTK_19(iyr,icafe19,1,ifuel)
	  	    TECHRPT_SAL(iyr,itechrpt19,ifuel)		= TECHRPT_SAL(iyr,itechrpt19,ifuel) + TRKSTK_19(iyr,icafe19,1,ifuel)
          ENDDO
	    ENDDO
	    TECHSHARE(iyr,itechp2,:,:) = TECHSHARE(iyr,itechp2,:,:)/sum(TECHRPT_SAL(iyr,:,:))
      ENDDO ! end itechp2 loop
    ENDIF

  IF (n.eq.MNUMYR.and.fcrl.eq.1) then
!	WRITE(21,*)'Total regional sales by icafe19'
!	DO i = 32, MNUMYR
!	  DO iregn = 1, MNUMCR-2
!	    WRITE(21,'(i4,",",i2,19(",",f9.0))')i+1989,iregn,sum(TRKSTK_19R(i,1,1,:,:,iregn)), sum(TRKSTK_19R(i,2,1,:,:,iregn)), sum(TRKSTK_19R(i,3,1,:,:,iregn)),&
!														 sum(TRKSTK_19R(i,4,1,:,:,iregn)), sum(TRKSTK_19R(i,5,1,:,:,iregn)), sum(TRKSTK_19R(i,6,1,:,:,iregn)),&
!														 sum(TRKSTK_19R(i,7,1,:,:,iregn)), sum(TRKSTK_19R(i,8,1,:,:,iregn)), sum(TRKSTK_19R(i,9,1,:,:,iregn)),&
!														 sum(TRKSTK_19R(i,10,1,:,:,iregn)),sum(TRKSTK_19R(i,11,1,:,:,iregn)),sum(TRKSTK_19R(i,12,1,:,:,iregn)),&
!														 sum(TRKSTK_19R(i,13,1,:,:,iregn)),sum(TRKSTK_19R(i,14,1,:,:,iregn)),sum(TRKSTK_19R(i,15,1,:,:,iregn)),&
!														 sum(TRKSTK_19R(i,16,1,:,:,iregn)),sum(TRKSTK_19R(i,17,1,:,:,iregn)),sum(TRKSTK_19R(i,18,1,:,:,iregn)),&
!														 sum(TRKSTK_19R(i,19,1,:,:,iregn))
!	  ENDDO
!	ENDDO	
	
!	WRITE(21,*)'Final sales by region by powertrain'
!	DO i = 32, MNUMYR
!	  DO iregn = 1, MNUMCR-2
!		DO ifuel = 1, FUEL12
!	        WRITE(21,'(i4,",",i2,",",i2,19(",",f12.0))')i+1989,iregn,ifuel,sum(TRKSTK_19R(i,1,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,2,1,ifuel,:,iregn)),&
!													  sum(TRKSTK_19R(i,3,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,4,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,5,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,6,1,ifuel,:,iregn)) ,&
!													  sum(TRKSTK_19R(i,7,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,8,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,9,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,10,1,ifuel,:,iregn)),&
!													  sum(TRKSTK_19R(i,11,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,12,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,13,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,14,1,ifuel,:,iregn)),&
!													  sum(TRKSTK_19R(i,15,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,16,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,17,1,ifuel,:,iregn)),sum(TRKSTK_19R(i,18,1,ifuel,:,iregn)),&
!													  sum(TRKSTK_19R(i,19,1,ifuel,:,iregn))
!		ENDDO
!	  ENDDO
!	ENDDO

	WRITE(21,*)'Freight truck BEV battery size (kWh)'
	WRITE(21,'(a2,19(",",a7))')'ivmt','2b','2bV','3','3V','4','5','6','7V','7D','7D','7D','8V','8D','8D','8D','8S','8S','8S','HH'
	DO i = 34, MNUMYR
      DO ivmt = 1, nvmt
	    WRITE(21,'(i4,",",i2,19(",",f7.1))') i+1989, ivmt, kWh_nominal(ivmt,:,i)
	  ENDDO
    ENDDO
!	WRITE(21,*)'Freight truck FCHEV h2 tank count (units)'
!	WRITE(21,'(a4,",",a4,",",a4,",",19(",",a7))')'year','regn','flt','2b','2bV','3','3V','4','5','6','7V','7D','7D','7D','8V','8D','8D','8D','8S','8S','8S','HH'
!	DO i=34,MNUMYR
!      DO iregn = 1, mnumcr-2
!        DO iflt = 1, flt
!	      WRITE(21,'(i4,",",i4,",",i4,19(",",f3.1))') i+1989,iregn,iflt,avg_tank_cnt_FCHEV(i,iflt,:,iregn)
!	    ENDDO
!      ENDDO
!    ENDDO
!	WRITE(21,*)'Freight truck FCEV h2 tank count (units)'
!	WRITE(21,'(a4,",",a4,",",a4,",",19(",",a7))')'year','regn','flt','2b','2bV','3','3V','4','5','6','7V','7D','7D','7D','8V','8D','8D','8D','8S','8S','8S','HH'
!	DO i=34,MNUMYR
!      DO iregn = 1, mnumcr-2
!        DO iflt = 1, flt
!	      WRITE(21,'(i4,",",i4,",",i4,19(",",f3.1))') i+1989,iregn,iflt,avg_tank_cnt_FCEV(i,iflt,:,iregn)
!	     ENDDO
!      ENDDO
!   ENDDO
  	WRITE(21,*)'HDV Alt-Powertrain Component Costs (2022USD)'
	WRITE(21,'(a4,12(",",a10))')'Year','cumulFCstk','c_FCkW','cumulH2tank','$_h2tank','$_batt45V','$_batt68V','$_batt78T','$_batt2b3','$_battHEV','$_motorkW','$_obc'
  	DO i=34,MNUMYR
  	  WRITE(21,'(I4, 12(",",F10.0))') i+1989, cumulative_fc_stacks(i-1), cost_FCkW(i) / mc_jpgdp(1) * mc_jpgdp(33), &
								  cumulative_h2_tanks(i-1), cost_h2tank(i) / mc_jpgdp(1) * mc_jpgdp(33),&
								  cost_battkWh_trk(:,i) / mc_jpgdp(1) * mc_jpgdp(33), &
								  cost_motorkW(i) / mc_jpgdp(1) * mc_jpgdp(33),cost_obc(i)/ mc_jpgdp(1) * mc_jpgdp(33)
  	ENDDO
  	WRITE(21,*)'BEV_infra_cost (2022USD) by icafe19, IRA (2032) and post-IRA (2033)'
  	DO icafe19=1,CAFE19
  	  WRITE(21,'(F8.1,",",F8.1)') BEV_infra_cost(icafe19,2032-1989:2033-1989) / mc_jpgdp(1) * mc_jpgdp(33)
  	ENDDO
  	WRITE(21,*)'Gasoline, Diesel, CNG and LNG, and H2 prices (2022 USD/MMBtu)'
  	WRITE(21,'(a4,",",a4,8(",",a6))')'year','regn','MG','DS','CNG_pub','CNG_flt','LNG_pub','LNG_flt','H2','H2$kg'
  	DO i = 31, MNUMYR
  	  DO iregn = 1, MNUMCR-2
  	    WRITE(21,'(i4,",",i4,8(",",f6.3))')i+1989,iregn,PMGTR(iregn,i)*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1), PDSTR(iregn,i)*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1),&
  														 PGFTRPV(iregn,i)*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1)*NG_priceadj(1),PGFTRFV(iregn,i)*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1)*NG_priceadj(1),&
  														 PGLTRPV(iregn,i)*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1)*NG_priceadj(2),PGLTRFV(iregn,i)*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1)*NG_priceadj(2),&
                                                         PH2TR(iregn,i)*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1),PH2TR(iregn,i)/7.5*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1)
  	  ENDDO
  	ENDDO
  	WRITE(21,*)'EV charging prices (2022 USD/MMBtu)'
  	WRITE(21,'(a4,3(",",a7))')'year','LDV_l2','HDV_flt','HDV_nft'
  	DO i = 31, MNUMYR
  	  DO iregn = 1, MNUMCR-2
  	    WRITE(21,'(i4,",",i4,3(",",f7.3))')i+1989,iregn,PELP2CM(iregn,i)*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1),PELIBCM(iregn,i)*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1),&
  														PELFNCM(iregn,i)*1.115*mc_jpgdp(2022-1989)/mc_jpgdp(1)
  	  ENDDO
  	ENDDO
!	WRITE(21,*)'techshr_p2'
!	DO i = 29, MNUMYR
!	  DO itechp2 = 1, TECHP2
!        WRITE(21,'(i4,",",i2,19(",",f5.3))') i+1989, itechp2, techshr_P2(i,itechp2,:,1)
!	  ENDDO
!	ENDDO
!	WRITE(21,*)'cost_ice (2022USD)'
!	WRITE(21,'(a4,",",a4,3(",",a8))')'Year','icaf','cost_ice','c_tech','c_lonox'
!  	DO i=34,MNUMYR
!  	  DO icafe19  = 1,CAFE19
!  	    WRITE(21,'(I4,",",I4,3(",",F8.2))') i+1989, icafe19, cost_ICE(icafe19,i) / mc_jpgdp(1) * mc_jpgdp(33), &
!															 cost_icetech(icafe19,i) / mc_jpgdp(1) * mc_jpgdp(33), &
!															 lonox_cost(icafe19,i) / mc_jpgdp(1) * mc_jpgdp(33)
!  	  ENDDO
!  	ENDDO
!  	WRITE(21,*)'HDV INC_COST_CNG (2022USD)'
!  	DO i=34,MNUMYR
!  	  DO ivmt  = 1,nvmt
!  	    WRITE(21,'(I4,",",I4,19(",",F8.0))') i+1989, ivmt, INC_COST_CNG(ivmt,:,i) / mc_jpgdp(1) * mc_jpgdp(33)
!  	  ENDDO
!  	ENDDO
!  	WRITE(21,*)'HDV INC_COST_BEV (2022USD)'
!  	DO i=34,MNUMYR
!  	  DO ivmt  = 1,nvmt
!  	    WRITE(21,'(I4,",",I4,19(",",F8.0))') i+1989, ivmt, INC_COST_BEV(ivmt,:,i) / mc_jpgdp(1) * mc_jpgdp(33)
!  	  ENDDO
!  	ENDDO
!  	WRITE(21,*)'HDV INC_COST_PHEVD (2022USD)'
!  	DO i=34,MNUMYR
!  	  DO ivmt  = 1,nvmt
!  	    WRITE(21,'(I4,",",I4,19(",",F8.0))') i+1989, ivmt, INC_COST_PHEVD(ivmt,:,i) / mc_jpgdp(1) * mc_jpgdp(33)
!  	  ENDDO
!  	ENDDO
!  	WRITE(21,*)'HDV INC_COST_PHEVG (2022USD)'
!  	DO i=34,MNUMYR
!  	  DO ivmt  = 1,nvmt
!  	    WRITE(21,'(I4,",",I4,19(",",F8.0))') i+1989, ivmt, INC_COST_PHEVG(ivmt,:,i) / mc_jpgdp(1) * mc_jpgdp(33)
!  	  ENDDO
!  	ENDDO
!	WRITE(21,*)'HDV INC_COST_FCEV (2022USD)'
!  	DO i=34,MNUMYR
!  	  DO ivmt  = 1,nvmt
!  	    WRITE(21,'(I4,",",I4,19(",",F7.0))') i+1989, ivmt, INC_COST_FCEV(ivmt,:,i) / mc_jpgdp(1) * mc_jpgdp(33)
!  	  ENDDO
!  	ENDDO
!  	WRITE(21,*)'HDV INC_COST_FCHEV (2022USD)'
!  	DO i=34,MNUMYR
!  	  DO ivmt  = 1,nvmt
!  	    WRITE(21,'(I4,",",I4,19(",",F7.0))') i+1989, ivmt, INC_COST_FCHEV(ivmt,:,i) / mc_jpgdp(1) * mc_jpgdp(33)
!  	  ENDDO
!  	ENDDO
! 	WRITE(21,*)'HDV INC_COST_H2ICE (2022USD)'
! 	DO i=34,MNUMYR
! 	  DO ivmt  = 1,nvmt
! 	    WRITE(21,'(I4,",",I4,19(",",F8.0))') i+1989, ivmt, INC_COST_H2ICE(ivmt,:,i) / mc_jpgdp(1) * mc_jpgdp(33)
! 	  ENDDO
! 	ENDDO
!  	WRITE(21,*)'HDV BEV inc_tech_cost_19 (2022USD)'
!  	DO i=34,MNUMYR
!  	  WRITE(21,'(I4,19(",",F9.0))') i+1989, (inc_tech_cost_19(i,:,6)-inc_tech_cost_19(34,:,6)) / mc_jpgdp(1) * mc_jpgdp(33)
!  	ENDDO
!  	WRITE(21,*)'HDV PHEVD inc_tech_cost_19 (2022USD)'
!  	DO i=34,MNUMYR
!  	  WRITE(21,'(I4,19(",",F9.0))') i+1989, (inc_tech_cost_19(i,:,7)-inc_tech_cost_19(34,:,7)) / mc_jpgdp(1) * mc_jpgdp(33)
!  	ENDDO
!  	WRITE(21,*)'HDV PHEVG inc_tech_cost_19 (2022USD)'
!  	DO i=34,MNUMYR
!  	  WRITE(21,'(I4,19(",",F9.0))') i+1989, (inc_tech_cost_19(i,:,8)-inc_tech_cost_19(34,:,8)) / mc_jpgdp(1) * mc_jpgdp(33)
!  	ENDDO
!   	WRITE(21,*)'HDV FCEV inc_tech_cost_19 (2022USD)'
!   	DO i=34,MNUMYR
!   	  WRITE(21,'(I4,19(",",F9.0))') i+1989, (inc_tech_cost_19(i,:,9)-inc_tech_cost_19(34,:,9)) / mc_jpgdp(1) * mc_jpgdp(33)
!   	ENDDO
!    WRITE(21,*)'HDV GHG Compliance Cost (2022USD)'
!    DO i=35,MNUMYR
!      DO iregn = 1, mnumcr-2
!   	    WRITE(21,'(I4,",",I2,19(",",F12.0))') i+1989,iregn, ghg_comp_cost(:,iregn,i) / mc_jpgdp(1) * mc_jpgdp(33)
!   	  ENDDO
!    ENDDO
!          
!    WRITE(21,*)'HDV ACT Compliance Cost (2022USD)'
!    DO i=35,MNUMYR
!      DO iregn = 1, mnumcr-2
!   	    WRITE(21,'(I4,",",I2,19(",",F12.0))') i+1989,iregn, act_comp_cost(:,iregn,i) / mc_jpgdp(1) * mc_jpgdp(33)
!   	  ENDDO
!    ENDDO
!    WRITE(21,*)'Sales share by all dimensions (no LPG or E85)'
!    DO i = 35,MNUMYR
!      DO iregn = 1, MNUMCR-2
!        DO icafe19 = 1, CAFE19
!          DO iflt = 1, FLT
!            DO ivmt = 1, NVMT
!              WRITE(21,'(I4,4(",",I4),12(",",f9.5))')i+1989,iregn,icafe19,iflt,ivmt,fuel_shr_ivmt(i,ivmt,iflt,icafe19,:,iregn)
!            ENDDO
!          ENDDO
!        ENDDO
!      ENDDO
!    ENDDO
  ENDIF


 RETURN
 END SUBROUTINE TRUCK_NEW


! ==========================================================================================================
!...Subroutine TRUCK_AGGVARS
!   Description:
!     Updates a number of key sales, stock, and mpg variables which need to change as different components of the model operate
!	  Aggregate sales. credits, and mpgs from 19 size classes to 1) 4 NEMS reported classes and 2) 14 Phase 2 classes
!	  The Phase 2 aggregate variables are used in the CAFE model, 4-class aggregation used in final reporting.
!	Output:
!		Updates TRKSTK_19,CREDSALES_19,TRKSAL_SC,TRKSAL_P2,credsales_p2,HDV_MPG,new_mpg_p2,cltmpg_yr
! ==========================================================================================================
 SUBROUTINE TRUCK_AGGVARS
 USE F_
 IMPLICIT NONE

	INTEGER i

!	Populate aggregate (national, no fleet detail) stock for rest of model, now that powertrain choice is done
	DO icafe19 = 1, cafe19
	  DO ifuel = 1, FUEL12
	    TRKSTK_19(iyr,icafe19,1,ifuel) = sum(TRKSTK_19R(iyr,icafe19,1,ifuel,:,11))
	  ENDDO
	ENDDO

!	Calculate CAFE credit sales allowed for PHEVs, EVs, and FCVs
!	apply credit multipliers to sales
    DO icafe19=1,CAFE19
	  DO ifuel=1,FUEL12
        CREDSALES_19(iyr,icafe19,ifuel)=TRKSTK_19(iyr,icafe19,1,ifuel)*HDV_AFV_CREDITS(ifuel,iyr)
      ENDDO
    ENDDO

	TRKSAL_SC(iyr,:,:) 		= 0.0
	TRKSAL_P2(iyr,:,:) 		= 0.0
	credsales_p2(iyr,:,:) 	= 0.0
	HDV_MPG(iyr,:,1,:) 		= 0.0
	new_mpg_p2(iyr,:,:)		= 0.0
	DO ifuel = 1,FUEL12
	  DO icafe19 = 1, cafe19 
		isc4 = SC19Map(icafe19)
		icafe14 = P219Map(icafe19)
	    TRKSAL_SC(iyr,isc4,ifuel) = TRKSAL_SC(iyr,isc4,ifuel) + TRKSTK_19(iyr,icafe19,1,ifuel)
		TRKSAL_P2(iyr,icafe14,ifuel) = TRKSAL_P2(iyr,icafe14,ifuel) + TRKSTK_19(iyr,icafe19,1,ifuel)
		credsales_p2(iyr,icafe14,ifuel) = credsales_p2(iyr,icafe14,ifuel) + CREDSALES_19(iyr,icafe19,ifuel)
        if(new_mpg_19(iyr,ifuel,icafe19).gt.0.0.and.TRKSTK_19(iyr,icafe19,1,ifuel).gt.0.0) THEN 
		  HDV_MPG(iyr,isc4,1,ifuel) = HDV_MPG(iyr,isc4,1,ifuel) + TRKSTK_19(iyr,icafe19,1,ifuel) / (new_mpg_19(iyr,ifuel,icafe19)*(1-HDV_DF(ifuel,icafe14)))  ! Apply degradation for on-road
		  new_mpg_p2(iyr,ifuel,icafe14) = new_mpg_p2(iyr,ifuel,icafe14) + TRKSTK_19(iyr,icafe19,1,ifuel) / new_mpg_19(iyr,ifuel,icafe19)
        ENDIF
	  ENDDO
	  DO isc = 1, sc4
	    if(HDV_MPG(iyr,isc,1,ifuel).gt.0.0) HDV_MPG(iyr,isc,1,ifuel) = TRKSAL_SC(iyr,isc,ifuel)/HDV_MPG(iyr,isc,1,ifuel)
		if(HDV_MPG(iyr,isc,1,ifuel).eq.0.0.and.HDV_MPG(iyr-1,isc,1,ifuel).gt.0.0) HDV_MPG(iyr,isc,1,ifuel) = HDV_MPG(iyr-1,isc,1,ifuel)
	  ENDDO
	  DO icafe14 = 1, CAFE14
		IF(new_mpg_p2(iyr,ifuel,icafe14).gt.0.0) new_mpg_p2(iyr,ifuel,icafe14) = TRKSAL_P2(iyr,icafe14,ifuel) / new_mpg_p2(iyr,ifuel,icafe14)
	  ENDDO
	ENDDO

!	Error-catching	  
	IF ((sum(HDV_MPG(iyr,:,1,:)).ne.sum(HDV_MPG(iyr,:,1,:))) .or. &
	    (sum(new_mpg_p2(iyr,:,:)).ne.sum(new_mpg_p2(iyr,:,:))) .or. &
	    (sum(TRKSAL_SC(iyr,:,:)).ne.sum(TRKSAL_SC(iyr,:,:))) .or. &
		(sum(TRKSAL_P2(iyr,:,:)).ne.sum(TRKSAL_P2(iyr,:,:)))) then
	  DO ifuel = 1, FUEL12
		DO isc = 1, sc4
		  IF (HDV_MPG(iyr,isc,1,ifuel).ne.HDV_MPG(iyr,isc,1,ifuel)) &
			WRITE(21,'(a,5(",",i4),20(",",f8.1))')'ERROR: HDV_MPG', curcalyr, curitr,itryghg,isc,ifuel,HDV_MPG(iyr,isc,1,ifuel),new_mpg_19(iyr,ifuel,:)
	      IF (TRKSAL_SC(iyr,isc,ifuel).ne.TRKSAL_SC(iyr,isc,ifuel)) &
		    WRITE(21,'(a,5(",",i4),20(",",f8.1))')'ERROR: TRKSAL_SC',curcalyr,curitr,itryghg,isc,ifuel,TRKSAL_SC(iyr,isc,ifuel),TRKSTK_19(iyr,:,1,ifuel)/10
            DO icafe19 = 1, cafe19
              IF (SC19Map(icafe19).ne.isc) CYCLE
              DO iregn = 1, mnumcr
                IF (iregn.eq.10) CYCLE
                  IF (SUM(TRKSTK_19R(iyr,icafe19,1,ifuel,:,iregn)).ne.SUM(TRKSTK_19R(iyr,icafe19,1,ifuel,:,iregn))) THEN
                    WRITE(21,'(a,7(",",i4),",",f8.1)')'ERROR: BAD_SALES',curcalyr,curitr,itryghg,isc,ifuel,icafe19,iregn,SUM(TRKSTK_19R(iyr,icafe19,1,ifuel,:,iregn))/10
                  ENDIF
              ENDDO
            ENDDO
		  WRITE(*,*)'ERROR: TDM TRANFRT fuel economy. See p1/TRNOUT.txt.'
		ENDDO
	  ENDDO
	  STOP
	ENDIF

!...reaggregate class 2b fuel economies to NEMS reported aggregation level NOTE: Have to switch gasoline and diesel 
!   New vehicles - Vintage 1
    DO ifuel = 1,FUEL12
      IF(sum(VMTFLT_19(iyr-1,1:2,1,ifuel)).gt.0.0) then 
	    cltmpg_yr(iyr,CLTMap(ifuel),1) = ((VMTFLT_19(iyr-1,1,1,ifuel)/sum(VMTFLT_19(iyr-1,1:2,1,ifuel)))/(NEW_MPG_19(iyr,ifuel,1)*(1-HDV_DF(ifuel,1))) + &
                                          (VMTFLT_19(iyr-1,2,1,ifuel)/sum(VMTFLT_19(iyr-1,1:2,1,ifuel)))/(NEW_MPG_19(iyr,ifuel,2)*(1-HDV_DF(ifuel,1))))**-1.0 	
      ELSE
        cltmpg_yr(iyr,CLTMap(ifuel),1) = ((SUM(TRKSTK_19R(iyr,1,1,ifuel,:,11))/(NEW_MPG_19(iyr,ifuel,1)*(1-HDV_DF(ifuel,1))) + &
                                           SUM(TRKSTK_19R(iyr,2,1,ifuel,:,11))/(NEW_MPG_19(iyr,ifuel,2)*(1-HDV_DF(ifuel,1)))) / &
                                          SUM(TRKSTK_19R(iyr,1:2,1,ifuel,:,11)))**-1.0
	  ENDIF
    ENDDO
!   Vintages 2 - 33
    DO iage = 2,33      
	  DO ifuel = 1,FUEL12
	    cltmpg_yr(iyr,ifuel,iage) = cltmpg_yr(iyr-1,ifuel,iage-1)
      ENDDO
    ENDDO
!   Vintage 34+
    DO ifuel = 1,FUEL12
	  IF(sum(VMTFLT_19(iyr-1,1:2,33:34,2)).gt.0.0.and.cltmpg_yr(iyr-1,CLTMap(ifuel),34).gt.0.0.and.cltmpg_yr(iyr-1,CLTMap(ifuel),33).gt.0.0) then
	    cltmpg_yr(iyr,CLTMap(ifuel),34) = 1.0/(((sum(VMTFLT_19(iyr-1,1:2,33,2))/cltmpg_yr(iyr-1,CLTMap(ifuel),34)) + &
                                   (sum(VMTFLT_19(iyr-1,1:2,34,2))/cltmpg_yr(iyr-1,CLTMap(ifuel),33)))/(sum(VMTFLT_19(iyr-1,1:2,33:34,2))))
      ELSE
	    cltmpg_yr(iyr,CLTMap(ifuel),34) = cltmpg_yr(iyr-1,CLTMap(ifuel),33)
	  ENDIF
    ENDDO

!!... New vehicles - Vintage 1
!    cltmpg_yr(iyr,1,1) = ((VMTFLT_19(iyr-1,1,1,2)/sum(VMTFLT_19(iyr-1,1:2,1,2)))/NEW_MPG_19(iyr,2,1) + &
!						  (VMTFLT_19(iyr-1,2,1,2)/sum(VMTFLT_19(iyr-1,1:2,1,2)))/NEW_MPG_19(iyr,2,2))**-1.0               ! gasoline
!	cltmpg_yr(iyr,2,1) = ((VMTFLT_19(iyr-1,1,1,1)/sum(VMTFLT_19(iyr-1,1:2,1,1)))/NEW_MPG_19(iyr,1,1) + &
!						  (VMTFLT_19(iyr-1,2,1,1)/sum(VMTFLT_19(iyr-1,1:2,1,1)))/NEW_MPG_19(iyr,1,2))**-1.0               ! diesel
!    DO ifuel = 3,FUEL12
!        IF(sum(VMTFLT_19(iyr-1,1:2,1,ifuel)).gt.0.0) then 
!	      cltmpg_yr(iyr,ifuel,1) = ((VMTFLT_19(iyr-1,1,1,ifuel)/sum(VMTFLT_19(iyr-1,1:2,1,ifuel)))/NEW_MPG_19(iyr,ifuel,1) + &
!									(VMTFLT_19(iyr-1,2,1,ifuel)/sum(VMTFLT_19(iyr-1,1:2,1,ifuel)))/NEW_MPG_19(iyr,ifuel,2))**-1.0 	
!        ELSE
!          cltmpg_yr(iyr,ifuel,1) = ((SUM(TRKSTK_19R(iyr,1,1,ifuel,:,11))/NEW_MPG_19(iyr,ifuel,1) + &
!		                             SUM(TRKSTK_19R(iyr,2,1,ifuel,:,11))/NEW_MPG_19(iyr,ifuel,2)) / &
!								   SUM(TRKSTK_19R(iyr,1:2,1,ifuel,:,11)))**-1.0
!		ENDIF
!    ENDDO
!        
!!... Vintages 2 - 33
!    DO iage = 2,33      
!	    DO ifuel = 1,FUEL12
!		  cltmpg_yr(iyr,ifuel,iage) = cltmpg_yr(iyr-1,ifuel,iage-1)
!        ENDDO
!    ENDDO
!!... Vintage 34+
!	IF(sum(VMTFLT_19(iyr-1,1:2,33:34,2)).gt.0.0) then
!	  cltmpg_yr(iyr,1,34) = 1.0/(((sum(VMTFLT_19(iyr-1,1:2,33,2))/cltmpg_yr(iyr-1,1,34)) + &
!        (sum(VMTFLT_19(iyr-1,1:2,34,2))/cltmpg_yr(iyr-1,1,33)))/(sum(VMTFLT_19(iyr-1,1:2,33:34,2))))
!    ELSE
!	  cltmpg_yr(iyr,1,34) = cltmpg_yr(iyr-1,1,33)
!	ENDIF
!	IF(sum(VMTFLT_19(iyr-1,1:2,33:34,1)).gt.0.0) then
!	  cltmpg_yr(iyr,2,34) = 1.0/(((sum(VMTFLT_19(iyr-1,1:2,33,1))/cltmpg_yr(iyr-1,2,34)) + &
!       (sum(VMTFLT_19(iyr-1,1:2,34,1))/cltmpg_yr(iyr-1,2,33)))/sum(VMTFLT_19(iyr-1,1:2,33:34,1)))
!	ELSE
!	  cltmpg_yr(iyr,2,34) = cltmpg_yr(iyr-1,2,33)
!	ENDIF
!    DO ifuel = 3,FUEL12
!	    IF(sum(VMTFLT_19(iyr-1,1:2,33:34,ifuel)).gt.0.0) then
!		    IF(cltmpg_yr(iyr-1,ifuel,34).gt.0.0.or.cltmpg_yr(iyr-1,ifuel,33).gt.0.0) then
!		      cltmpg_yr(iyr,ifuel,34) = 1.0/(((sum(VMTFLT_19(iyr-1,1:2,33,ifuel))/cltmpg_yr(iyr-1,ifuel,34)) + &
!                (sum(VMTFLT_19(iyr-1,1:2,34,ifuel))/cltmpg_yr(iyr-1,ifuel,33)))/sum(VMTFLT_19(iyr-1,1:2,33:34,ifuel)))
!			ELSEIF(cltmpg_yr(iyr-1,ifuel,33).gt.0.0) then
!			  cltmpg_yr(iyr,ifuel,34) = cltmpg_yr(iyr-1,ifuel,33)
!			ELSEIF(cltmpg_yr(iyr-1,ifuel,34).gt.0.0) then
!			  cltmpg_yr(iyr,ifuel,34) = cltmpg_yr(iyr-1,ifuel,34)
!			ENDIF
!	    ELSE
!	      cltmpg_yr(iyr,ifuel,34) = cltmpg_yr(iyr-1,ifuel,33)
!		ENDIF
!	ENDDO
 
 RETURN
 END SUBROUTINE TRUCK_AGGVARS

! ==========================================================================================================
!...Subroutine TRUCK_PBK
!   Description:
!       Calculates alternative powertrain adoption based on how quickly (and IF) the net present value of fuel savings
!		offsets the incremental upfront cost.
!	Output:
!		Updates fuel_shr_regn, the preliminary sales share for the powertrain within a given size class (icafe19), region (iregn), and fleet (iflt)
! ==========================================================================================================
 SUBROUTINE TRUCK_PBK(inc_cost,ann_vmt_per_veh,cpm,cpm_base,mr_ice,veh_shr_vmt_bin,refueltimeval)
 USE F_
 IMPLICIT NONE
 
    REAL buy_shr(PBK_YR), &
    	 buy_shr_agg, &
    	 ann_savings, &
		 ann_vmt_per_veh, &
		 veh_shr_vmt_bin,&
    	 cpm,&
    	 cpm_base,&
		 mr_ice(PBK_YR),&
    	 inc_cost, &
		 mpath, &
		 veh_cost,&
         refueltimeval,&
		 MIDYR_choice(CAFE19,FUEL12,FLT,NVMT), &
		 SLOPE_choice(CAFE19,FUEL12,FLT,NVMT)
		 
    INTEGER Y
	
	LOGICAL start_curve(CAFE19,FUEL12,FLT,NVMT)
	LOGICAL first_time/.true./
	
	IF (first_time) then
	  start_curve(:,:,:,:) = .false.
	  first_time = .false.
	ENDIF

    buy_shr(:) = 0.0
    buy_shr_agg = 0.0


!	Insurance costs are based on the vehicle MSRP delta
!	Remove infrastructure costs from inc_cost, for those vehicles that include it.
    veh_cost = inc_cost
    IF (jfuel.eq.6.and.iflt.eq.2.and.icafe19.lt.16) then	! Non-sleeper fleet vehicles (infra cost in inc_cost)
	  veh_cost = inc_cost -BEV_infra_cost(icafe19,iyr)
    ENDIF
    
    inc_cost = inc_cost * (1 + sales_tax_rate(iregn))
	
!	calculate annual fuel savings relative to conventional powertrain (Class 2b/3: gasoline, Class 4-8: diesel)
    ann_savings =  ann_vmt_per_veh * (cpm_base-cpm)
	
!	Sum net present value of cumulative savings by payback period
	npv_choice(icafe19,jfuel,ivmt,1,iregn,iyr) = (ann_savings &                                                    ! fuel savings
                                                  - (insure_rate*veh_cost) &                                       ! insurance
                                                  + (mr_ice(1) - MR_cost(icafe19,jfuel,1,iyr))*ann_vmt_per_veh &   ! maintenance and repair savings
                                                  - refueltimeval)&                                                ! cost of extra refuelings required
										       / (1.0+DISCRTXG_P2)
    DO Y=2,PBK_YR
	  npv_choice(icafe19,jfuel,ivmt,Y,iregn,iyr) = npv_choice(icafe19,jfuel,ivmt,Y-1,iregn,iyr) &
                                                 + (ann_savings - insure_rate*veh_cost + (mr_ice(Y) - MR_cost(icafe19,jfuel,Y,iyr))*ann_vmt_per_veh - refueltimeval)&
                                                   / (1.0+DISCRTXG_P2)**(Y)
    ENDDO

	IF (sum(npv_choice(icafe19,jfuel,ivmt,:,iregn,iyr)).ne.sum(npv_choice(icafe19,jfuel,ivmt,:,iregn,iyr))) then
	  WRITE(21,'(a,6(",",a4),8(",",a8),9(",",a4))')'ERROR: TRANFRT NaN NPV','year','regn','cafe','flt','fuel','ivmt','npv(1)','npv(2)','npv(3)','npv(4)','npv(5)','npv(6)',&
												   'npv(7)','annvmt','cpmbase','cpm','MR(1)','MR(2)','MR(3)','MR(4)','MR(5)','MR(6)','MR(7)'
	  WRITE(21,'(a,6(",",i4),8(",",f8.0),9(",",f4.3),2(",",f10.1))')'ERROR: TRANFRT NaN NPV',curcalyr,iregn,icafe19,iflt,jfuel,ivmt,npv_choice(icafe19,jfuel,ivmt,:,iregn,iyr),ann_vmt_per_veh,&
															  cpm_base,cpm,MR_cost(icafe19,jfuel,:,iyr),inc_cost,veh_cost
      WRITE(21,'(a,6(",",i4),7(",",f8.03),4(",",f8.2))')'ERROR: TRANFRT NaN NPV',curcalyr,iregn,icafe19,iflt,jfuel,ivmt,npv_choice(icafe19,jfuel,ivmt,:,iregn,iyr-1),MIDYR_choice(icafe19,jfuel,iflt,ivmt),&
                                            SLOPE_choice(icafe19,jfuel,iflt,ivmt),BFSHXG(isc4,jfuel,iflt),EFSHXG(isc4,jfuel,iflt)
      WRITE(*,*)'ERROR: TDM TRANFRT fuel choice module. See p1/TRNOUT.txt.'
	  STOP 901
	ENDIF

!	Calculate market share purchase decision by payback group
	DO Y=1,PBK_YR
!     If the mileage accumulated over the current payback period (Y) exceeds the vehicle's useful life (2x the CO2 emissions equipment's "useful life"), don't adopt.
      IF (Y*VMT_VEH(ivmt,iflt,icafe19).gt.HDV_USEFULLIFE(1,P219Map(icafe19))*2) THEN
!        IF(fcrl.eq.1.and.itryghg.eq.1) WRITE(21,'(a,7(",",i4),2(",",f10.1))')'skip_adopt',curcalyr,iregn,icafe19,iflt,jfuel,ivmt,Y,VMT_VEH(ivmt,iflt,icafe19),HDV_USEFULLIFE(1,P219Map(icafe19))
        CYCLE
      ENDIF
      IF (inc_cost.lt.npv_choice(icafe19,jfuel,ivmt,Y,iregn,iyr)) then
        buy_shr(y) = pback_shr(Y,icafe19)*veh_shr_vmt_bin
      ENDIF
    ENDDO

    buy_shr_agg = sum(buy_shr(:))

!	IF market penetration reaches the minimum level specified in trnhdvx (BFSHXG), and we are beyond
!	the earliest start year (TRGSHXG): start the s-curve to begin manufacturing ramp-up and infrastructure rollout.
	IF (buy_shr_agg.ge.BFSHXG(isc4,jfuel,iflt).and.curcalyr.ge.TRGSHXG(icafe19,jfuel,iflt)) then
	  IF (.not.start_curve(icafe19,jfuel,iflt,ivmt)) then
	    MIDYR_choice(icafe19,jfuel,iflt,ivmt)	= curcalyr+(1.*CYAFVXG(isc4,jfuel,iflt)/2)
	    start_curve(icafe19,jfuel,iflt,ivmt) = .true.
		SLOPE_choice(icafe19,jfuel,iflt,ivmt)  = LOG(0.01)/((1.*CYAFVXG(isc4,jfuel,iflt))/2)
	  ENDIF
	  mpath = BFSHXG(isc4,jfuel,iflt) + (EFSHXG(isc4,jfuel,iflt)-BFSHXG(isc4,jfuel,iflt)) &
			  /(1.+(exp(SLOPE_choice(icafe19,jfuel,iflt,ivmt)*(1.*curcalyr-MIDYR_choice(icafe19,jfuel,iflt,ivmt)))))

!     Two possible modifications:
!     1. If not meeting CAFE, squish the S-curve steeper for BEV, PHEVD, PHEVG, FCEV, FCHEV, HEV, H2ICE
      IF (itryghg.gt.1.and.jfuel.ge.6.and.HDV_passGHG(iyr,P24Map(P219Map(icafe19))).eq..FALSE.) THEN
        IF (itryghg.le.CEILING(ghgtry/2.0)) THEN     ! For the first 50% of attempts, push HEV and PHEV options
          IF (jfuel.eq.11.or.jfuel.eq.7.or.jfuel.eq.8) MIDYR_choice(icafe19,jfuel,iflt,ivmt) = MIDYR_choice(icafe19,jfuel,iflt,ivmt) - 0.5
        ELSE                                         ! For latter half of attempts, push ZEV options
          IF (jfuel.ne.11.and.jfuel.ne.7.and.jfuel.ne.8) MIDYR_choice(icafe19,jfuel,iflt,ivmt) = MIDYR_choice(icafe19,jfuel,iflt,ivmt) - 0.5
        ENDIF
        mpath = BFSHXG(isc4,jfuel,iflt) + (EFSHXG(isc4,jfuel,iflt)-BFSHXG(isc4,jfuel,iflt)) &
			  /(1.+(exp(SLOPE_choice(icafe19,jfuel,iflt,ivmt)*(1.*curcalyr-MIDYR_choice(icafe19,jfuel,iflt,ivmt)))))
!     2. If market share is lower than last year, freeze the S-curve ascent (bump MIDYR up 1 year)
!      ELSEIF (buy_shr_agg*mpath.lt.fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn)) THEN
!        MIDYR_choice(icafe19,jfuel,iflt,ivmt) = MIDYR_choice(icafe19,jfuel,iflt,ivmt) + 1.0
!        mpath = BFSHXG(isc4,jfuel,iflt) + (EFSHXG(isc4,jfuel,iflt)-BFSHXG(isc4,jfuel,iflt)) &
!			  /(1.+(exp(SLOPE_choice(icafe19,jfuel,iflt,ivmt)*(1.*curcalyr-MIDYR_choice(icafe19,jfuel,iflt,ivmt)))))
      ENDIF
	ELSE
	  mpath = 1.0
	ENDIF

!	sum(fuel_shr_ivmt(iyr,1:nvmt,jfuel)) is the sales share of powertrain jfuel in each iflt/icafe19/iregn group
!	I.e., it's not the sales share of powertrain jfuel within each VMT bin
	fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn)= buy_shr_agg*mpath
    fuel_shr_regn(iyr,icafe19,jfuel,iflt,iregn) = fuel_shr_regn(iyr,icafe19,jfuel,iflt,iregn) + buy_shr_agg*mpath

!   DEBUG
!	IF (jfuel.eq.11.and.icafe19.le.4.and.icafe19.ge.3) then
!	  WRITE(21,'(a,7(",",i4),10(",",f8.0),12(",",f4.3))')'npv_choice_debug',curitr,curcalyr,iregn,icafe19,iflt,jfuel,ivmt,npv_choice(icafe19,jfuel,ivmt,:,iregn,iyr),ann_vmt_per_veh,&
!															  inc_cost,refueltimeval, cpm_base,cpm,MR_cost(icafe19,jfuel,:,iyr),mpath,buy_shr_agg
!	ENDIF


 RETURN
 END SUBROUTINE TRUCK_PBK


!=============================================================================================================
 SUBROUTINE TRUCK_ACT
 USE F_
 IMPLICIT NONE
 include 'angtdm'
 INCLUDE 'EUSPRC'

	INTEGER i,k, act_maxice, act_maxhev,act_maxzev, izev
	PARAMETER(act_maxice = 5)                               ! Number of conventional ICE powertrains that will be replaced with ZEVs to comply
    PARAMETER(act_maxhev = 6)                               ! Number of conventional ICE and HEV powertrains that will be replaced with ZEVs to comply
    PARAMETER(act_maxzev = 3)                               ! Number of powertrains that count as "ZEV" for ACT compliance
	REAL total_sales,bevs,phevs,fcvs						! Temporary variables for ACT credit/deficit calcs
	REAL max_2b8V_NZEV,max_78_NZEV							! Maximum NZEV credits available for use in a given region/year
	REAL deficit_2b3(MNUMCR,MNUMYR),&						! Variables to track ACT credits/deficits by region
		 deficit_45v(MNUMCR,MNUMYR),&
		 deficit_67v(MNUMCR,MNUMYR),&
		 deficit_8v(MNUMCR,MNUMYR),&
         deficit_48v(MNUMCR,MNUMYR),&
		 deficit_78t(MNUMCR,MNUMYR),&
		 credit_2b3_ZEV(MNUMCR,MNUMYR),&
		 credit_45v_ZEV(MNUMCR,MNUMYR),&
		 credit_67v_ZEV(MNUMCR,MNUMYR),&
		 credit_8v_ZEV(MNUMCR,MNUMYR),&
		 credit_48v_ZEV(MNUMCR,MNUMYR),&
		 credit_78t_ZEV(MNUMCR,MNUMYR),&
		 credit_2b3_NZEV(MNUMCR,MNUMYR),&
		 credit_45v_NZEV(MNUMCR,MNUMYR),&
		 credit_67v_NZEV(MNUMCR,MNUMYR),&
		 credit_8v_NZEV(MNUMCR,MNUMYR),&
		 credit_48v_NZEV(MNUMCR,MNUMYR),&
		 credit_78t_NZEV(MNUMCR,MNUMYR),&
		 bank_2b8V_ZEV(MNUMCR,MNUMYR),&
		 bank_2b8V_NZEV(MNUMCR,MNUMYR),&
		 bank_78t_ZEV(MNUMCR,MNUMYR),&
		 bank_78t_NZEV(MNUMCR,MNUMYR)
	REAL total_48V
	REAL shr_2b3,shr_48V											! Temporary; allocate 2b8 credits to 2b3 and 48V
	REAL act_npv(3),act_npv_tot										! Temporary; net present value used to allocate ACT sales forcing by powertrain
	REAL zev_sales_req,zev_sales_req_hev							! Temporary; total sales needed to shift to ZEV
	REAL sales_avail,sales_availHEV,carryover_req					! Temporary; sales available to convert to ZEV for compliance 
	REAL ICE_shr(nvmt), ICEHEV_shr(nvmt)
	REAL ICE_shr_ivmt(nvmt,act_maxice), ICEHEV_shr_ivmt(nvmt,act_maxhev)
	REAL phev_sales_req												! Temporary; sales to shift from PHEVD/PHEVG to ZEV
	REAL tot_daycab_sales,tot_sleeper_sales							! Temporary; total daycab/sleeper tractor sales in a given region and year
	REAL pre_act_sales
	REAL avg_npv(act_maxzev)                                        ! Temporary; average NPV of savings/losses, across all financial horizon bins
    INTEGER fuel_choice												! Temporary; index of lowest cost (least negative NPV) ZEV option for ACT compliance
	LOGICAL act_bust_doonce,use_hybrids								! Temporary; only want to write the ACT_BUST header once in debug file (for readability)
	INTEGER conv_vehs(act_maxice)/1,2,3,4,5/                                 ! Conventional ICE powertrains (i.e.,can be replaced by ZEVs to generate needed ACT credits)
    INTEGER convhev_vehs(act_maxhev)/1,2,3,4,5,11/
    INTEGER zev_vehs(act_maxzev)/6,9,10/
	LOGICAL act_detailwrite
    
    act_detailwrite = .FALSE.
    act_bust_doonce = .TRUE.

!	  Estimate ACT (CA Code Section 1963.3) compliance
!	  Must force compliance before applying learning rates to components	    	  
	      IF (n.eq.2021-1989) then
	  	  bank_2b8V_ZEV(:,:) 	=  0.0
	  	  bank_2b8V_NZEV(:,:) 	=  0.0
	  	  bank_78t_ZEV(:,:) 	=  0.0
	  	  bank_78t_NZEV(:,:) 	=  0.0
	      ENDIF
	  
!	  	Calculate credits, deficits, banking
!		Note that NZEV credits (PHEVs) are no longer generated post-MY2035 (1963.2(b))
          DO iregn = 1, MNUMCR-2
!	  	    2b3
	  	    total_sales 				= sum(TRKSTK_19R(iyr,1:4,1,:,1:2,iregn))
	  	    bevs 						= sum(TRKSTK_19R(iyr,1:4,1,6,1:2,iregn))
	  	    phevs						= sum(TRKSTK_19R(iyr,1:4,1,7:8,1:2,iregn))
	  	    fcvs 						= sum(TRKSTK_19R(iyr,1:4,1,9:10,1:2,iregn))
	  	    deficit_2b3(iregn,n) 	 	= total_sales * act_wgtmod(1) * act_req_2b3(iregn,n)
	  	    credit_2b3_ZEV(iregn,n)  	= (bevs+fcvs) * act_wgtmod(1)
	  	    IF(n.le.2035-1989) then
			  credit_2b3_NZEV(iregn,n) 	= phevs * act_wgtmod(1) * phev_factor
			ELSE
			  credit_2b3_NZEV(iregn,n) 	= 0.0
			ENDIF
				  	    
!	  	    4-5 Vocational
	  	    total_sales 				= sum(TRKSTK_19R(iyr,5:6,1,:,1:2,iregn))
	  	    bevs 						= sum(TRKSTK_19R(iyr,5:6,1,6,1:2,iregn))
	  	    phevs						= sum(TRKSTK_19R(iyr,5:6,1,7:8,1:2,iregn))
	  	    fcvs 						= sum(TRKSTK_19R(iyr,5:6,1,9:10,1:2,iregn))
	  	    deficit_45v(iregn,n) 	 	= total_sales * act_wgtmod(5)
	  	    credit_45v_ZEV(iregn,n)  	= (bevs+fcvs) * act_wgtmod(5)
	  	    IF(n.le.2035-1989) then
			  credit_45v_NZEV(iregn,n) 	= phevs * act_wgtmod(5) * phev_factor
			ELSE
			  credit_45v_NZEV(iregn,n) 	= 0.0
			ENDIF

	  	    
!	  	    6-7 Vocational
	  	    total_sales 				= sum(TRKSTK_19R(iyr,7:8,1,:,1:2,iregn))
	  	    bevs 						= sum(TRKSTK_19R(iyr,7:8,1,6,1:2,iregn))
	  	    phevs						= sum(TRKSTK_19R(iyr,7:8,1,7:8,1:2,iregn))
	  	    fcvs 						= sum(TRKSTK_19R(iyr,7:8,1,9:10,1:2,iregn))
	  	    deficit_67v(iregn,n) 	 	= total_sales * act_wgtmod(7)
	  	    credit_67v_ZEV(iregn,n)  	= (bevs+fcvs) * act_wgtmod(7)
	  	    IF(n.le.2035-1989) then
			  credit_67v_NZEV(iregn,n) 	= phevs * act_wgtmod(7) * phev_factor
			ELSE
			  credit_67v_NZEV(iregn,n) 	= 0.0
			ENDIF
	  	    
!	  	    8 Vocational
	  	    total_sales 				= sum(TRKSTK_19R(iyr,12,1,:,1:2,iregn))
	  	    bevs 						= sum(TRKSTK_19R(iyr,12,1,6,1:2,iregn))
	  	    phevs						= sum(TRKSTK_19R(iyr,12,1,7:8,1:2,iregn))
	  	    fcvs 						= sum(TRKSTK_19R(iyr,12,1,9:10,1:2,iregn))
	  	    deficit_8v(iregn,n) 		= total_sales * act_wgtmod(12)
	  	    credit_8v_ZEV(iregn,n)  	= (bevs+fcvs) * act_wgtmod(12)
	  	    IF(n.le.2035-1989) then
			  credit_8v_NZEV(iregn,n) 	= phevs * act_wgtmod(12) * phev_factor
			ELSE
			  credit_8v_NZEV(iregn,n) 	= 0.0
			ENDIF
	  	    
!	  	    Total 4-8 vocational		
	  	    deficit_48v(iregn,n)    	= (deficit_45v(iregn,n) + deficit_67v(iregn,n) + deficit_8v(iregn,n)) * act_req_48v(iregn,n)
	  	    credit_48v_ZEV(iregn,n) 	= credit_45v_ZEV(iregn,n) + credit_67v_ZEV(iregn,n) + credit_8v_ZEV(iregn,n)
	  	    credit_48v_NZEV(iregn,n)	= credit_45v_NZEV(iregn,n) + credit_67v_NZEV(iregn,n) + credit_8v_NZEV(iregn,n)
	  	    
!	  	    7&8 Tractor
	  	    total_sales 				= sum(TRKSTK_19R(iyr,[9,10,11,13,14,15,16,17,18,19],1,:,1:2,iregn))
	  	    bevs 		  				= sum(TRKSTK_19R(iyr,[9,10,11,13,14,15,16,17,18,19],1,6,1:2,iregn))
	  	    phevs		  				= sum(TRKSTK_19R(iyr,[9,10,11,13,14,15,16,17,18,19],1,7:8,1:2,iregn))
	  	    fcvs 		  				= sum(TRKSTK_19R(iyr,[9,10,11,13,14,15,16,17,18,19],1,9:10,1:2,iregn))
	  	    deficit_78t(iregn,n) 	 	= total_sales * act_wgtmod(13) * act_req_78trac(iregn,n)
	  	    credit_78t_ZEV(iregn,n)  	= (bevs+fcvs) * act_wgtmod(13)
	  	    IF(n.le.2035-1989) then
			  credit_78t_NZEV(iregn,n) 	= phevs * act_wgtmod(13) * phev_factor
			ELSE
			  credit_78t_NZEV(iregn,n) 	= 0.0
			ENDIF
	  	    
!	  	    Update credit banks
	  	    bank_2b8V_ZEV(iregn,n) 		=  bank_2b8V_ZEV(iregn,n-1) + credit_48v_ZEV(iregn,n) + credit_2b3_ZEV(iregn,n)
	  	    bank_2b8V_NZEV(iregn,n) 	=  bank_2b8V_NZEV(iregn,n-1) + credit_48v_NZEV(iregn,n) + credit_2b3_NZEV(iregn,n)
	  	    bank_78t_ZEV(iregn,n) 		=  bank_78t_ZEV(iregn,n-1) + credit_78t_ZEV(iregn,n)
	  	    bank_78t_NZEV(iregn,n) 		=  bank_78t_NZEV(iregn,n-1) + credit_78t_NZEV(iregn,n)

!			Credits expire after 5 MYs (i.e., can't have more credits available than the cumulative total from the past 5 years) 1963.2(g)(2)
!			Credits earned in 2021-2023 can be used until 2030 (>5yrs after being generated) 1963.2(g)(1)
			IF(curcalyr.gt.2030) then
			  bank_2b8V_ZEV(iregn,n)	= MAX(0.0,MIN(bank_2b8V_ZEV(iregn,n), sum(credit_48v_ZEV(iregn,n-5:n)) + sum(credit_2b3_ZEV(iregn,n-5:n))))
			  bank_2b8V_NZEV(iregn,n)	= MAX(0.0,MIN(bank_2b8V_NZEV(iregn,n), sum(credit_48v_NZEV(iregn,n-5:n)) + sum(credit_2b3_NZEV(iregn,n-5:n))))
			  bank_78t_ZEV(iregn,n)		= MAX(0.0,MIN(bank_78t_ZEV(iregn,n), sum(credit_78t_ZEV(iregn,n-5:n))))
			  bank_78t_NZEV(iregn,n)	= MAX(0.0,MIN(bank_78t_NZEV(iregn,n), sum(credit_78t_NZEV(iregn,n-5:n))))
			ENDIF
		    
!	  	    Enforced starting in MY2024
	  	    IF (n.ge.2024-1989) then
!	  	      Credits are spent in the order specified in the regulation
!	  	      NZEVs can only meet 50% of the deficit (C78T --> C78T) and (C2b3 + C48V + C78T --> C2b3 + C48V) -- 1963.3(c)(4)
	  	      max_2b8V_NZEV 			= (deficit_48v(iregn,n)+deficit_2b3(iregn,n))*0.5
	  	      max_78_NZEV  				= deficit_78t(iregn,n)*0.5
	  	      
!	  	      1. C78T NZEV to meet C78T
	  	      IF (bank_78t_NZEV(iregn,n).ge.max_78_NZEV) then
	  	        bank_78t_NZEV(iregn,n)	= bank_78t_NZEV(iregn,n) - max_78_NZEV
	  	        deficit_78t(iregn,n)  	= deficit_78t(iregn,n) - max_78_NZEV
	  	      ELSE
	  	        deficit_78t(iregn,n) 	= deficit_78t(iregn,n) - bank_78t_NZEV(iregn,n)
	  	        bank_78t_NZEV(iregn,n)	= 0.0
	  	      ENDIF
!	  	      2. C2b3 and C48V NZEV to meet C2b3 and C48V deficits
	  	 	 IF (max_2b8V_NZEV.gt.0.0) then
	  	        IF (bank_2b8V_NZEV(iregn,n).ge.max_2b8V_NZEV) then		! IF we have more credits than we're allowed to spend, spend the max
!	  	          IF the 48V deficit is less than the max NZEV credits available, and since we know the bank is larger than the max allowed,
!	  	 	 	  eliminate the 48V deficit, reduce the max NZEV availablem and apply the rest to C2b3 deficit
	  	          IF (deficit_48v(iregn,n).le.max_2b8V_NZEV) then
	  	            bank_2b8V_NZEV(iregn,n) = bank_2b8V_NZEV(iregn,n) - deficit_48v(iregn,n)
	  	        	max_2b8V_NZEV			= max_2b8V_NZEV - deficit_48v(iregn,n)
	  	        	deficit_48v(iregn,n)	= 0.0
	  	        	deficit_2b3(iregn,n)	= deficit_2b3(iregn,n) - max_2b8V_NZEV
	  	        	bank_2b8V_NZEV(iregn,n) = bank_2b8V_NZEV(iregn,n) - max_2b8V_NZEV
	  	        	max_2b8V_NZEV			= 0.0
	  	          ELSE  ! IF the 48V deficit is larger than the max NZEV available, spend up to the max on C48V
	  	            bank_2b8V_NZEV(iregn,n) = bank_2b8V_NZEV(iregn,n) - max_2b8V_NZEV
	  	        	deficit_48v(iregn,n)	= deficit_48v(iregn,n) - max_2b8V_NZEV
	  	        	max_2b8V_NZEV			= 0.0
	  	          ENDIF
	  	        ELSE		! IF we have less credits than we're allowed to spend, use 'em all
	  	          IF (bank_2b8V_NZEV(iregn,n).ge.deficit_48v(iregn,n)) then						! IF banked > deficit, meet the 48V deficit
	  	            bank_2b8V_NZEV(iregn,n)	= bank_2b8V_NZEV(iregn,n) - deficit_48v(iregn,n)
	  	        	deficit_48v(iregn,n) 	= 0.0
	  	        	deficit_2b3(iregn,n)	= deficit_2b3(iregn,n) - bank_2b8V_NZEV(iregn,n)		! and spend the rest of it on 2b3
	  	          ELSE																			! otherwise (banked < deficit) spend it all on 48V
	  	            deficit_48v(iregn,n)	= deficit_48v(iregn,n) - bank_2b8V_NZEV(iregn,n)
	  	        	bank_2b8V_NZEV(iregn,n) = 0.0
	  	          ENDIF
	  	        ENDIF
	  	 	 ENDIF
!	  	      3. C78T NZEV to meet C2b3 and C48V
!	  	      Cannot use NZEV for > 50% of C2b3+C48V deficits; make sure we haven't hit the max yet (IF so, skip this step)
	  	      IF (max_2b8V_NZEV.gt.0.0) then
!	  	        IF we have more credits than we're allowed to spend (after having applied C2b3 and C48V NZEV creds above), spend the max
	  	        IF (bank_78t_NZEV(iregn,n).ge.max_2b8V_NZEV) then
!	  	          IF the 48V deficit is less than the max NZEV credits available, and since we know the bank is larger than the max allowed, 
!	  	 	 	eliminate the 48V deficit, reduce the max NZEV availablem and apply the rest to C2b3 deficit
	  	          IF (deficit_48v(iregn,n).le.max_2b8V_NZEV) then
	  	            bank_78t_NZEV(iregn,n)	= bank_78t_NZEV(iregn,n) - deficit_48v(iregn,n)
	  	        	max_2b8V_NZEV			= max_2b8V_NZEV - deficit_48v(iregn,n)
	  	        	deficit_48v(iregn,n)	= 0.0
	  	        	deficit_2b3(iregn,n)	= deficit_2b3(iregn,n) - max_2b8V_NZEV
	  	      	    bank_78t_NZEV(iregn,n)	= bank_78t_NZEV(iregn,n) - max_2b8V_NZEV
	  	      	    max_2b8V_NZEV			= 0.0
	  	          ELSE  ! IF the 48V deficit is larger than the max NZEV available, spend up to the max on C48V
	  	            bank_78t_NZEV(iregn,n)	= bank_78t_NZEV(iregn,n) - max_2b8V_NZEV
	  	        	deficit_48v(iregn,n)	= deficit_48v(iregn,n) - max_2b8V_NZEV
	  	        	max_2b8V_NZEV			= 0.0
	  	          ENDIF
	  	        ELSE		! IF there are fewer banked credits than the max allowed to spend
	  	          IF (bank_78t_NZEV(iregn,n).ge.deficit_48v(iregn,n)) then						! IF banked > deficit, meet the 48V deficit
	  	            bank_78t_NZEV(iregn,n)	= bank_78t_NZEV(iregn,n) - deficit_48v(iregn,n)
	  	        	deficit_48v(iregn,n) 	= 0.0
	  	        	deficit_2b3(iregn,n)	= deficit_2b3(iregn,n) - bank_78t_NZEV(iregn,n)			! and spend the rest of it on 2b3
	  	          ELSE																			! otherwise (banked < deficit) spend it all on 48V
	  	            deficit_48v(iregn,n)	= deficit_48v(iregn,n) - bank_78t_NZEV(iregn,n)
	  	        	bank_78t_NZEV(iregn,n)	= 0.0
	  	          ENDIF
	  	        ENDIF
	  	      ENDIF
!	  	      4. C78T ZEV to meet C78T
	  	      IF (bank_78t_ZEV(iregn,n).ge.deficit_78t(iregn,n)) then
	  	        bank_78t_ZEV(iregn,n)		= bank_78t_ZEV(iregn,n) - deficit_78t(iregn,n)
	  	        deficit_78t(iregn,n)  		= 0.0
	  	      ELSE
	  	        deficit_78t(iregn,n) 		= deficit_78t(iregn,n) - bank_78t_ZEV(iregn,n)
	  	        bank_78t_ZEV(iregn,n)		= 0.0
	  	      ENDIF
!	  	      5. C2b3 and C48 ZEV to meet C2b3 and C48
!	  	      First spend on C48 deficits (hardest to electrify)
			  IF (bank_2b8V_ZEV(iregn,n).ge.(deficit_48v(iregn,n) + deficit_2b3(iregn,n))) then
	  	        bank_2b8V_ZEV(iregn,n)		= bank_2b8V_ZEV(iregn,n) - deficit_48v(iregn,n)-deficit_2b3(iregn,n)
	  	        deficit_48v(iregn,n) 		= 0.0
	  	 	    deficit_2b3(iregn,n)		= 0.0

!			  Spend credits on C2b3 and C48V proportional to the C2b3/C48V deficit sizes 
			  ELSE 
			    shr_2b3 = bank_2b8V_ZEV(iregn,n) * deficit_2b3(iregn,n)/(deficit_2b3(iregn,n)+deficit_48v(iregn,n))
			    shr_48V = bank_2b8V_ZEV(iregn,n) - shr_2b3
			    
			    IF (shr_2b3.gt.deficit_2b3(iregn,n)) then
			      bank_2b8V_ZEV(iregn,n)	= bank_2b8V_ZEV(iregn,n) - deficit_2b3(iregn,n)
			 	  deficit_2b3(iregn,n)		= 0.0
			    ELSE
			      deficit_2b3(iregn,n)		= deficit_2b3(iregn,n) - shr_2b3
			 	  bank_2b8V_ZEV(iregn,n)	= bank_2b8V_ZEV(iregn,n) - shr_2b3
			    ENDIF
			    
			    IF (shr_48V.gt.deficit_48v(iregn,n)) then
			      bank_2b8V_ZEV(iregn,n)	= bank_2b8V_ZEV(iregn,n) - deficit_48v(iregn,n)
			 	  deficit_48v(iregn,n)		= 0.0
			    ELSE
			      deficit_48v(iregn,n)		= deficit_48v(iregn,n) - shr_48V
			 	  bank_2b8V_ZEV(iregn,n)	= bank_2b8V_ZEV(iregn,n) - shr_48V
			    ENDIF

			  ENDIF

!	  	      6. C78T ZEV to meet C2b3 and C48
	  	      IF (bank_78t_ZEV(iregn,n).ge.(deficit_48v(iregn,n)+deficit_2b3(iregn,n))) then
	  	        bank_78t_ZEV(iregn,n)		= bank_78t_ZEV(iregn,n) - deficit_48v(iregn,n)-deficit_2b3(iregn,n)
	  	        deficit_48v(iregn,n) 		= 0.0
	  	 	    deficit_2b3(iregn,n)		= 0.0
!	  	      IF not, spend the credits on the C48 deficit first (harder to electrify), then any extra on C2b3
	  	      ELSE
	  	        IF (bank_78t_ZEV(iregn,n).ge.deficit_48v(iregn,n)) then
	  	          bank_78t_ZEV(iregn,n)		= bank_78t_ZEV(iregn,n) - deficit_48v(iregn,n)
	  	          deficit_48v(iregn,n)  	= 0.0
	  	        ELSE
	  	          deficit_48v(iregn,n)  	= deficit_48v(iregn,n) - bank_78t_ZEV(iregn,n)
	  	          bank_78t_ZEV(iregn,n)		= 0.0
	  	        ENDIF
	  	        IF (bank_78t_ZEV(iregn,n).ge.deficit_2b3(iregn,n)) then
	  	          bank_78t_ZEV(iregn,n)		= bank_78t_ZEV(iregn,n) - deficit_2b3(iregn,n)
	  	          deficit_2b3(iregn,n)  	= 0.0
	  	        ELSE
	  	          deficit_2b3(iregn,n) 		= deficit_2b3(iregn,n) - bank_78t_ZEV(iregn,n)
	  	          bank_78t_ZEV(iregn,n)		= 0.0
	  	        ENDIF
	  	      ENDIF
	  	    ENDIF
	      ENDDO

		  IF (n.ge.2024-1989) then
		  
!	      Calculate sales requirements (i.e. sales to force into market) by size class, based on the distribution of existing sales across size classes
!		  and exogenous assumptions (ZEVreqshr)
	      ZEVreq(:,:,:,n) = 0.0
		  DO iregn = 1, MNUMCR-2
		    DO iflt = 1, flt

!			  Class 2b-3: distribute deficit using sales shares
			  DO icafe19 = 1,4
			    ZEVreq(iregn,iflt,icafe19,n)  = sum(TRKSTK_19R(iyr,icafe19,1,convhev_vehs,iflt,iregn))/sum(TRKSTK_19R(iyr,1:4,1,convhev_vehs,:,iregn)) * deficit_2b3(iregn,n) / act_wgtmod(icafe19)
			  ENDDO

!			  Class 4-8 vocational: distribute deficit using sales shares. This results in higher ZEVreq for the lower size classes, since they have smaller weight modifiers.
			  total_48V = sum(TRKSTK_19R(iyr,5:6,1,convhev_vehs,:,iregn))&
			  		    + sum(TRKSTK_19R(iyr,7:8,1,convhev_vehs,:,iregn))&
			  		    + sum(TRKSTK_19R(iyr,12,1,convhev_vehs,:,iregn))
			  DO icafe19 = 5,12
				IF (icafe19.ge.9.and.icafe19.le.11) CYCLE	! C7 tractors
			    ZEVreq(iregn,iflt,icafe19,n) = deficit_48v(iregn,n) * (sum(TRKSTK_19R(iyr,icafe19,1,convhev_vehs,iflt,iregn)) / total_48V) / act_wgtmod(icafe19)
			  ENDDO

!			  Tractors: Distribute deficits to daycab v. sleeper based on exogenous assumption (ZEVreqshr78); split by class/roof height based on sales shares
			  tot_daycab_sales  = sum(TRKSTK_19R(iyr,[9,10,11,13,14,15],1,convhev_vehs,:,iregn))
			  tot_sleeper_sales = sum(TRKSTK_19R(iyr,16:18,1,convhev_vehs,:,iregn))
			  DO icafe19 = 9,18
			    IF (icafe19.eq.12) CYCLE	! C8 vocational
				IF (icafe19.le.15) then		! Daycabs
!			      Sales needed 	          		=  deficit by by daycab v. sleeper 		  *  sales share (flt, class / roof height) w/in day and sleeper         / convert from cred/deficit to sales counts
	              ZEVreq(iregn,iflt,icafe19,n)  = (deficit_78t(iregn,n) * ZEVreqshr78(1)) * (sum(TRKSTK_19R(iyr,icafe19,1,convhev_vehs,iflt,iregn))/ tot_daycab_sales)  / act_wgtmod(icafe19)
	            ELSE 						! Sleepers
				  ZEVreq(iregn,iflt,icafe19,n)  = (deficit_78t(iregn,n) * ZEVreqshr78(2)) * (sum(TRKSTK_19R(iyr,icafe19,1,convhev_vehs,iflt,iregn))/ tot_sleeper_sales) / act_wgtmod(icafe19)
				ENDIF
			  ENDDO
			  
!			  IF (fcrl.eq.1) WRITE(21,'(a,3(",",i4),21(",",f6.0))')'ZEVreq_check',curcalyr,iregn,iflt,ZEVreq(iregn,iflt,:,n),deficit_2b3(iregn,n),deficit_48v(iregn,n)
			  
	        ENDDO
		  ENDDO		

!...      Adjust sales to meet ACT
	      DO iregn = 1, MNUMCR-2
	        DO icafe19 = 1, CAFE19
              act_comp_cost(icafe19,iregn,iyr) = 0.0
	      	  DO iflt = 1, flt
			    pre_act_sales = sum(TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn))
	            IF (ZEVreq(iregn,iflt,icafe19,n).eq.0.0) CYCLE
				  carryover_req = 0.0
                  use_hybrids = .false.

!                 Determine whether HEVs will need to be converted to ZEV to meet the reqs, or if there are enough conv. ICEs                  
                  sales_avail    = pre_act_sales * SUM(fuel_shr_regn(iyr,icafe19,conv_vehs,iflt,iregn))
                  sales_availHEV = pre_act_sales * SUM(fuel_shr_regn(iyr,icafe19,convhev_vehs,iflt,iregn))
                  
                  IF (ZEVreq(iregn,iflt,icafe19,n).gt.sales_avail) THEN
                    use_hybrids = .true.
                    IF (fcrl.eq.1.and.act_detailwrite) WRITE(21,'(a,4(",",i4),3(",",f8.1))') &
					 	  		   'ACT_USE_HEVs',curcalyr,iregn,icafe19,iflt,ZEVreq(iregn,iflt,icafe19,n),sales_avail,sales_availHEV
                  ENDIF

                  ICE_shr_ivmt(:,:) = 0.0
                  ICEHEV_shr_ivmt(:,:) = 0.0
				  DO ivmt = 1, nvmt
					IF (veh_shr(ivmt,iflt,icafe19).eq.0.0) CYCLE

!					Share of total iflt/icafe19/iregn sales that were in this ivmt bin AND of the specified fuel type
!                   This needs to be calculated ONCE in the first ivmt loop, since fuel_shr_ivmt is modified as ICEs are converted to ZEVs
				    if (ivmt.eq.1) THEN
                      DO i = 1, nvmt
                        DO k = 1, act_maxice
                          ICE_shr_ivmt(i,k)     = fuel_shr_ivmt(iyr,i,iflt,icafe19,conv_vehs(k),iregn)
                        ENDDO
                        DO k = 1, act_maxhev
                          ICEHEV_shr_ivmt(i,k)  = fuel_shr_ivmt(iyr,i,iflt,icafe19,convhev_vehs(k),iregn)
                        ENDDO
                      enddo
                      DO i = 1, nvmt
                        ICE_shr(i)              = sum(ICE_shr_ivmt(i,:)) / sum(ICE_shr_ivmt(:,:))
				        ICEHEV_shr(i)           = sum(ICEHEV_shr_ivmt(i,:)) / sum(ICEHEV_shr_ivmt(:,:))
                      ENDDO
                    endif

!					IF (fcrl.eq.1) &
!					  WRITE(21,'(a,",",i4,4(",",i2),",",f10.1,2(",",f5.3))')'zev_req_check',iyr+1989,iregn,icafe19,iflt,ivmt,ZEVreq(iregn,iflt,icafe19,n),&
!																					  ICE_shr(ivmt), sum(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,:,iregn))

!									total sales required	       share of group sales that were non-ZEV/NZEV AND in this ivmt bin
					zev_sales_req = ZEVreq(iregn,iflt,icafe19,n) * ICE_shr(ivmt) + carryover_req
					sales_avail = pre_act_sales * SUM(ICE_shr_ivmt(ivmt,:))
                    IF (use_hybrids) THEN
                      zev_sales_req = ZEVreq(iregn,iflt,icafe19,n) * ICEHEV_shr(ivmt) + carryover_req
                      sales_avail = pre_act_sales * SUM(ICEHEV_shr_ivmt(ivmt,:))
                    ENDIF
                    
                    IF (zev_sales_req.lt.0.5) CYCLE

!					Check that there are enough diesel/gasoline/CNG/E85/HEV sales to convert to ZEVs to meet the ACT requirement for this size class
	      	        IF (zev_sales_req.gt.sales_avail) then
!					    The logic below redistributes the "unavailable" sales to other vmt bins and size classes
!					    1. push the sales up to the next vmt bin
					    IF (ivmt.lt.nvmt.and.veh_shr(ivmt+1,iflt,icafe19).gt.0.0) then
					 	  IF (fcrl.eq.1.and.act_detailwrite) WRITE(21,'(a,3(",",i4),a,i2,a,i2,3(",",f8.1))') &
					 	  			  'ACT_MOVE_VMT',curcalyr,iregn,icafe19,' bin ',ivmt,' to ',ivmt+1,zev_sales_req,sales_avail,(zev_sales_req - sales_avail)
					 	  carryover_req = zev_sales_req - sales_avail
					 	  zev_sales_req = sales_avail
!					    2. push them to the next iflt type -- currently in ivmt=nvmt
					    ELSEIF (iflt.eq.1.and.veh_shr(1,iflt+1,icafe19).gt.0.0) then
					 	  IF (fcrl.eq.1.and.act_detailwrite) WRITE(21,'(a,3(",",i4),3(",",f8.1))')'ACT_MOVE_FLT',curcalyr,iregn,icafe19,zev_sales_req,sales_avail,(zev_sales_req - sales_avail)
					 	  ZEVreq(iregn,2,icafe19,n) = ZEVreq(iregn,2,icafe19,n) + (zev_sales_req - sales_avail)
					 	  zev_sales_req = sales_avail
!					    3. push them to the next size class (icafe19) -- currently in ivmt=nvmt and iflt=flt
					    ELSEIF (icafe19.lt.18) then
					 	  IF (fcrl.eq.1.and.act_detailwrite) WRITE(21,'(a,2(",",i4),a,",",i2,",",a,",",i2,",",3(",",f8.1))')'ACT_MOVE_CAFE',curcalyr,iregn,' CAFE19 ',icafe19,' to ',icafe19+1, zev_sales_req,sales_avail,(zev_sales_req - sales_avail)
					 	  ZEVreq(iregn,1,icafe19,n) = ZEVreq(iregn,1,icafe19,n) + (zev_sales_req - sales_avail) * act_wgtmod(icafe19) / act_wgtmod(icafe19+1)
					 	  zev_sales_req = sales_avail
					    ELSE
					 	  WRITE(21,'(a,i4,a,i4,a,f8.2,a)')'ERROR: ACT not met in ',curcalyr,'for CAFE19 ',icafe19,'. [redist loop]',(zev_sales_req - sales_avail),' ZEV sales needed.'
					    ENDIF
!                      ENDIF
					ENDIF

!	      		    Distribute ZEV requirements to ZEV powertrain options based on NPV of first-ownership-period savings
!	    			Assume maximum payback time -- PBK_YR (7 for AEO2025)
!					For BEVs, non-fleets (iflt=1) and sleeper cabs don't purchase infrastructure, therefore the EVSE cost is subtracted off incremental cost below.

!					Estimate the distribution of ZEVs (BEV v. FCEV v. FCHEV)
!                   First, estimate the average NPV of savings/losses (weighted by financial horizon bin)
                    DO izev = 1,act_maxzev
                      ifuel = zev_vehs(izev)
                      avg_npv(izev) = SUM(PBACK_SHR(:,icafe19) * npv_choice(icafe19,ifuel,ivmt,:,iregn,iyr))
                    ENDDO
                    
	      	        IF (iflt.eq.1.or.icafe19.ge.16) then
					  act_npv(1) 	= avg_npv(1) &
									- (INC_COST_BEV(ivmt,icafe19,iyr)-BEV_infra_cost(icafe19,iyr))
					ELSE 
					  act_npv(1) 	= avg_npv(1) &
									- INC_COST_BEV(ivmt,icafe19,iyr)
	      	        ENDIF
					act_npv(2) =  avg_npv(2) - INC_COST_FCEV(ivmt,icafe19,iyr)
	      	        act_npv(3) =  avg_npv(3) - INC_COST_FCHEV(ivmt,icafe19,iyr)

!					IF (fcrl.eq.1) then 
!					  WRITE(21,'(a,5(",",i4),5(",",f9.0))')'act_logit1',curcalyr,iregn,icafe19,iflt,ivmt,act_npv(:),MINVAL(act_npv(:)),ABS(MINVAL(act_npv(:)))
					  IF (sum(act_npv(:)).ne.sum(act_npv(:))) then
					    WRITE(21,'(a,5(",",i4),6(",",f10.0))')'ERROR: act_dist',curcalyr,iregn,icafe19,iflt,ivmt,npv_choice(icafe19,[6,9,10],ivmt,PBK_YR,iregn,iyr),INC_COST_BEV(ivmt,icafe19,iyr),&
																			  INC_COST_FCEV(ivmt,icafe19,iyr),INC_COST_FCHEV(ivmt,icafe19,iyr)
					  ENDIF
!					ENDIF

!					Smooth ZEV adoption to prevent jolts in market share		
					act_npv(:) = act_npv(:) / 10000							! Scale parameter

                    act_npv_tot = (exp(act_npv(1))+exp(act_npv(2))+exp(act_npv(3)))
					DO i = 1,3
					  act_npv(i) = exp(act_npv(i))/act_npv_tot
					ENDDO

!					Default Class 2b/3 ZEV option is BEV -- no FCEV/FCHEV
					IF (icafe19.le.4) then
					  act_npv(1) = 1
					  act_npv(2:3) = 0.0
					ENDIF

                    act_comp_cost(icafe19,iregn,iyr) = act_comp_cost(icafe19,iregn,iyr) + SUM(act_npv(:) * avg_npv(:))*zev_sales_req

!					IF (fcrl.eq.1) WRITE(21,'(a,5(",",i4),4(",",f9.2))')'act_logit2',curcalyr,iregn,icafe19,iflt,ivmt,act_npv(:),act_npv_tot

!	      		    Adjust truck sales: take ZEVs out of diesel, gasoline, CNG, E85, and HEV in proportion to their sales.
!                   If there aren't enough conventional vehicles (conv_vehs), convert ALL of them first, then start moving through HEVs
                    IF (use_hybrids) THEN
                      zev_sales_req_hev = zev_sales_req - SUM(ICE_shr_ivmt(ivmt,:)) * pre_act_sales        ! Assume all conventional vehicles are converted to ZEV
                      DO i = 1,act_maxice
                        TRKSTK_19R(iyr,icafe19,1,conv_vehs(i),iflt,iregn) = TRKSTK_19R(iyr,icafe19,1,conv_vehs(i),iflt,iregn) &
                                                                          - ICE_shr_ivmt(ivmt,i) * pre_act_sales
                        fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,conv_vehs(i),iregn) = 0.0
					  ENDDO
					  TRKSTK_19R(iyr,icafe19,1,11,iflt,iregn) = TRKSTK_19R(iyr,icafe19,1,11,iflt,iregn) - zev_sales_req_hev
                      fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,11,iregn) = fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,11,iregn) - zev_sales_req_hev/pre_act_sales
                      
                      TRKSTK_19R(iyr,icafe19,1,6,iflt,iregn)  = TRKSTK_19R(iyr,icafe19,1,6,iflt,iregn) + zev_sales_req * act_npv(1)	    ! BEV
					  TRKSTK_19R(iyr,icafe19,1,9,iflt,iregn)  = TRKSTK_19R(iyr,icafe19,1,9,iflt,iregn) + zev_sales_req * act_npv(2)	    ! FCEV
					  TRKSTK_19R(iyr,icafe19,1,10,iflt,iregn) = TRKSTK_19R(iyr,icafe19,1,10,iflt,iregn) + zev_sales_req * act_npv(3)	! FCHEV
                    
                    ELSE    ! Conventional powertrains only
                      DO i = 1,act_maxice        
                        TRKSTK_19R(iyr,icafe19,1,conv_vehs(i),iflt,iregn)       = TRKSTK_19R(iyr,icafe19,1,conv_vehs(i),iflt,iregn) &
                                                                                - ICE_shr_ivmt(ivmt,i) / SUM(ICE_shr_ivmt(ivmt,:)) &
                                                                                   * zev_sales_req
					    fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,conv_vehs(i),iregn) = fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,conv_vehs(i),iregn) &
                                                                                - ICE_shr_ivmt(ivmt,i) / SUM(ICE_shr_ivmt(ivmt,:)) * zev_sales_req/pre_act_sales
                      ENDDO
					  TRKSTK_19R(iyr,icafe19,1,6,iflt,iregn)  = TRKSTK_19R(iyr,icafe19,1,6,iflt,iregn) + zev_sales_req * act_npv(1)	    ! BEV
					  TRKSTK_19R(iyr,icafe19,1,9,iflt,iregn)  = TRKSTK_19R(iyr,icafe19,1,9,iflt,iregn) + zev_sales_req * act_npv(2)	    ! FCEV
					  TRKSTK_19R(iyr,icafe19,1,10,iflt,iregn) = TRKSTK_19R(iyr,icafe19,1,10,iflt,iregn) + zev_sales_req * act_npv(3)	! FCHEV
                    ENDIF
                      
                    fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,6,iregn)  = fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,6,iregn) + zev_sales_req * act_npv(1)/pre_act_sales
                    fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,9,iregn)  = fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,9,iregn) + zev_sales_req * act_npv(2)/pre_act_sales
                    fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,10,iregn) = fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,10,iregn) + zev_sales_req * act_npv(3)/pre_act_sales

!					Check for any negative sales values; IF b/w -1.0 and 0.0, zero out the sales (rounding issue). Otherwise, write out the error.
					DO i = 1,FUEL12
					  IF (i.eq.6.or.i.eq.9.or.i.eq.10) CYCLE
					  IF (TRKSTK_19R(iyr,icafe19,1,i,iflt,iregn).lt.0.0) then
					    IF (TRKSTK_19R(iyr,icafe19,1,i,iflt,iregn).gt.-1.0) then
						  TRKSTK_19R(iyr,icafe19,1,i,iflt,iregn) = 0.0
					    ELSE
					      IF (fcrl.eq.1) WRITE(21,'(a,6(",",i4),",",f8.1)')'ERROR: TRANFRT_NEGSALES',curcalyr,iregn,icafe19,iflt,ivmt,i,TRKSTK_19R(iyr,icafe19,1,i,iflt,iregn)
					    ENDIF
					  ENDIF
					ENDDO
					
!					Check for any NaNs -- will null the overall model results and write out the error below
					IF (sum(TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn)).ne.sum(TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn)).and.fcrl.eq.1) then
					  WRITE(21,'(a,5(",",a4),11(",",a8),",",a6)')'ERROR: TRANFRT NaN ACT Sales','year','regn','cafe','iflt','ivmt','zev_req',&
																	'ds','mg','lpg','cng','e85','bev','phvd','phvg','fcv','fchv','iceshr'
					  WRITE(21,'(a,5(",",i4),11(",",f8.1),",",f6.3)')'ERROR: TRANFRT NaN ACT Sales',curcalyr,iregn,icafe19,iflt,ivmt,zev_sales_req,&
																				TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn),ICE_shr(ivmt)
					  WRITE(*,*)'ERROR: TDM TRANFRT ACT compliance module. See p1/TRNOUT.txt.'
					  STOP 902
					ENDIF
	      		  ENDDO ! ivmt
				IF (abs(pre_act_sales - sum(TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn))).gt.0.5) then
				  IF (fcrl.eq.1) WRITE(21,'(a,4(",",i4),2(",",f8.1))')'ERROR: ACT changed sales',curcalyr,iregn,icafe19,iflt,sum(TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn)),pre_act_sales
	      	    ENDIF
			  ENDDO
	        ENDDO
	      ENDDO
          
!	  	RE-calculate credits, deficits, banking
!		Note that NZEV credits (PHEVs) are no longer generated post-MY2035 (1963.2(b))
          DO iregn = 1, MNUMCR-2
!	  	    2b3
	  	    total_sales 				= sum(TRKSTK_19R(iyr,1:4,1,:,1:2,iregn))
	  	    bevs 						= sum(TRKSTK_19R(iyr,1:4,1,6,1:2,iregn))
	  	    phevs						= sum(TRKSTK_19R(iyr,1:4,1,7:8,1:2,iregn))
	  	    fcvs 						= sum(TRKSTK_19R(iyr,1:4,1,9:10,1:2,iregn))
	  	    deficit_2b3(iregn,n) 	 	= total_sales * act_wgtmod(1) * act_req_2b3(iregn,n)
	  	    credit_2b3_ZEV(iregn,n)  	= (bevs+fcvs) * act_wgtmod(1)
	  	    IF(n.le.2035-1989) then
			  credit_2b3_NZEV(iregn,n) 	= phevs * act_wgtmod(1) * phev_factor
			ELSE
			  credit_2b3_NZEV(iregn,n) 	= 0.0
			ENDIF
				  	    
!	  	    4-5 Vocational
	  	    total_sales 				= sum(TRKSTK_19R(iyr,5:6,1,:,1:2,iregn))
	  	    bevs 						= sum(TRKSTK_19R(iyr,5:6,1,6,1:2,iregn))
	  	    phevs						= sum(TRKSTK_19R(iyr,5:6,1,7:8,1:2,iregn))
	  	    fcvs 						= sum(TRKSTK_19R(iyr,5:6,1,9:10,1:2,iregn))
	  	    deficit_45v(iregn,n) 	 	= total_sales * act_wgtmod(5)
	  	    credit_45v_ZEV(iregn,n)  	= (bevs+fcvs) * act_wgtmod(5)
	  	    IF(n.le.2035-1989) then
			  credit_45v_NZEV(iregn,n) 	= phevs * act_wgtmod(5) * phev_factor
			ELSE
			  credit_45v_NZEV(iregn,n) 	= 0.0
			ENDIF

	  	    
!	  	    6-7 Vocational
	  	    total_sales 				= sum(TRKSTK_19R(iyr,7:8,1,:,1:2,iregn))
	  	    bevs 						= sum(TRKSTK_19R(iyr,7:8,1,6,1:2,iregn))
	  	    phevs						= sum(TRKSTK_19R(iyr,7:8,1,7:8,1:2,iregn))
	  	    fcvs 						= sum(TRKSTK_19R(iyr,7:8,1,9:10,1:2,iregn))
	  	    deficit_67v(iregn,n) 	 	= total_sales * act_wgtmod(7)
	  	    credit_67v_ZEV(iregn,n)  	= (bevs+fcvs) * act_wgtmod(7)
	  	    IF(n.le.2035-1989) then
			  credit_67v_NZEV(iregn,n) 	= phevs * act_wgtmod(7) * phev_factor
			ELSE
			  credit_67v_NZEV(iregn,n) 	= 0.0
			ENDIF
	  	    
!	  	    8 Vocational
	  	    total_sales 				= sum(TRKSTK_19R(iyr,12,1,:,1:2,iregn))
	  	    bevs 						= sum(TRKSTK_19R(iyr,12,1,6,1:2,iregn))
	  	    phevs						= sum(TRKSTK_19R(iyr,12,1,7:8,1:2,iregn))
	  	    fcvs 						= sum(TRKSTK_19R(iyr,12,1,9:10,1:2,iregn))
	  	    deficit_8v(iregn,n) 		= total_sales * act_wgtmod(12)
	  	    credit_8v_ZEV(iregn,n)  	= (bevs+fcvs) * act_wgtmod(12)
	  	    IF(n.le.2035-1989) then
			  credit_8v_NZEV(iregn,n) 	= phevs * act_wgtmod(12) * phev_factor
			ELSE
			  credit_8v_NZEV(iregn,n) 	= 0.0
			ENDIF
	  	    
!	  	    Total 4-8 vocational		
	  	    deficit_48v(iregn,n)    	= (deficit_45v(iregn,n) + deficit_67v(iregn,n) + deficit_8v(iregn,n)) * act_req_48v(iregn,n)
	  	    credit_48v_ZEV(iregn,n) 	= credit_45v_ZEV(iregn,n) + credit_67v_ZEV(iregn,n) + credit_8v_ZEV(iregn,n)
	  	    credit_48v_NZEV(iregn,n)	= credit_45v_NZEV(iregn,n) + credit_67v_NZEV(iregn,n) + credit_8v_NZEV(iregn,n)
	  	    
!	  	    7&8 Tractor
	  	    total_sales 				= sum(TRKSTK_19R(iyr,[9,10,11,13,14,15,16,17,18,19],1,:,1:2,iregn))
	  	    bevs 		  				= sum(TRKSTK_19R(iyr,[9,10,11,13,14,15,16,17,18,19],1,6,1:2,iregn))
	  	    phevs		  				= sum(TRKSTK_19R(iyr,[9,10,11,13,14,15,16,17,18,19],1,7:8,1:2,iregn))
	  	    fcvs 		  				= sum(TRKSTK_19R(iyr,[9,10,11,13,14,15,16,17,18,19],1,9:10,1:2,iregn))
	  	    deficit_78t(iregn,n) 	 	= total_sales * act_wgtmod(13) * act_req_78trac(iregn,n)
	  	    credit_78t_ZEV(iregn,n)  	= (bevs+fcvs) * act_wgtmod(13)
	  	    IF(n.le.2035-1989) then
			  credit_78t_NZEV(iregn,n) 	= phevs * act_wgtmod(13) * phev_factor
			ELSE
			  credit_78t_NZEV(iregn,n) 	= 0.0
			ENDIF
          ENDDO

		ENDIF ! curcalyr.ge.2024

!       Detailed writes
!	    IF (n.eq.MNUMYR.and.fcrl.eq.1) then
!	      WRITE(21,*)'ACT Status - Deficits, Credits, and Banks'
!	      WRITE(21,'(a4,",",a2,17(",",a7))')'yr','r','d2b3','c2b3Z','c2b3NZ','d48v','c45vZ','c45NZ','c67vZ','c67NZ','c8vZ','c8VNZ',&
!	      								  'd78t','c78tZ','c78NZ','b2b8Z','b2b8NZ','b78tZ','b78tNZ'
!	      DO i = 32, MNUMYR
!	        DO iregn = 1, MNUMCR-2
!	          WRITE(21,'(i4,",",i2,17(",",f7.0))')i+1989,iregn,deficit_2b3(iregn,i),credit_2b3_ZEV(iregn,i),credit_2b3_NZEV(iregn,i),&
!	      													deficit_48v(iregn,i),credit_45v_ZEV(iregn,i),credit_45v_NZEV(iregn,i),&
!	      													credit_67v_ZEV(iregn,i),credit_67v_NZEV(iregn,i),&
!	      													credit_8v_ZEV(iregn,i),credit_8v_NZEV(iregn,i),&
!	      													deficit_78t(iregn,i),credit_78t_ZEV(iregn,i),credit_78t_NZEV(iregn,i),&
!	      													bank_2b8V_ZEV(iregn,i),bank_2b8V_NZEV(iregn,i),bank_78t_ZEV(iregn,i),&
!	      													bank_78t_NZEV(iregn,i)
!	        ENDDO
!	      ENDDO
!	      WRITE(21,*)'ACT Status - Additional ZEV sales required (thousands)'
!	      DO i = 32, MNUMYR
!	        DO iregn = 1, MNUMCR-2
!	          WRITE(21,'(i4,",",i2,19(",",f5.2))')i+1989,iregn,ZEVreq(iregn,iflt,:,i)/1000
!	        ENDDO
!	      ENDDO
!	    ENDIF
		
!	    Populate fuel share array with updated fuel shares post-ACT
!	    And fill national with updated regional sales 
        DO iregn = 1,MNUMCR-2
          DO iflt=1,flt
            DO icafe19 = 1,CAFE19
			  isc4 = SC19Map(icafe19)
	  	      DO ifuel = 1,FUEL12
			      fuel_shr_regn(iyr,icafe19,ifuel,iflt,iregn) = 0.0
				  IF (sum(TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn)).gt.0.0) then 
	  	  	        fuel_shr_regn(iyr,icafe19,ifuel,iflt,iregn) = sum(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))
                  ENDIF
                  TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) = fuel_shr_regn(iyr,icafe19,ifuel,iflt,iregn) * sum(TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn))
				  IF (iregn.eq.MNUMCR-2) TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,MNUMCR)  = sum(TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,1:MNUMCR-2))
	  		    ENDDO
	  	    ENDDO
	  	  ENDDO
	    ENDDO
	    
 RETURN
 END SUBROUTINE TRUCK_ACT

!=============================================================================================================
 SUBROUTINE TRUCK_STOCK
 USE F_
 IMPLICIT NONE

 REAL age_wgt(MNUMYR,CAFE19,MNUMCR)
 REAL avg_age(MNUMYR,CAFE19,MNUMCR)
 REAL VMTTMP(2)  			   					! temporary variables		
!---------------------------------------------------------------------------------------------------------------------------------------------------!
!...Apply survival rates to the previous year's stock
!...SURV_RATE contains 11 different survival rates -- these are applied to each of the 19 icafe classes based on the crosswalk in SurvMap
!...first fill everything but the new and the 34+ vintage slots
!...Sales are regionalized
!---------------------------------------------------------------------------------------------------------------------------------------------------!
    IF(curcalyr.gt.bsyr_stk) then
      DO iregn = 1,MNUMCR-2
	    DO icafe19 = 1,CAFE19 
	      DO ifuel = 1,FUEL12
			DO iage=2,AGE-1
			  TRKSTK_19R(iyr,icafe19,iage,ifuel,1:flt,iregn) = TRKSTK_19R(iyr-1,icafe19,iage-1,ifuel,1:flt,iregn)*SURV_RATE(iage,iregn,SurvMap(icafe19),ifuel)
			ENDDO
			
		    TRKSTK_19R(iyr,icafe19,AGE,ifuel,1:flt,iregn) = TRKSTK_19R(iyr-1,icafe19,AGE,ifuel,1:flt,iregn)*0.7 + &
															TRKSTK_19R(iyr-1,icafe19,AGE-1,ifuel,1:flt,iregn)*SURV_RATE(iage,iregn,SurvMap(icafe19),ifuel)	
          ENDDO				

!		  calculate stock transfers from fleet to nonfleet ownership - transfer only gasoline and diesel vehicles
          DO ifuel=1,2
            DO iage=2,age
			  TRKSTK_19R(iyr,icafe19,iage,ifuel,NFT,iregn) = TRKSTK_19R(iyr,icafe19,iage,ifuel,NFT,iregn) + (TFFXGRT(iage,SC19Map(icafe19))*TRKSTK_19R(iyr,icafe19,iage,ifuel,FLT,iregn))
			  TRKSTK_19R(iyr,icafe19,iage,ifuel,FLT,iregn) = TRKSTK_19R(iyr,icafe19,iage,ifuel,FLT,iregn) - (TFFXGRT(iage,SC19Map(icafe19))*TRKSTK_19R(iyr,icafe19,iage,ifuel,FLT,iregn))
            ENDDO   ! end age
          ENDDO     ! end fuel
        ENDDO       ! end CAFE19
      ENDDO         ! end region
	ENDIF
    
    DO icafe19 = 1, CAFE19
      DO ifuel = 1, FUEL12
        DO iage = 1, AGE
          DO iflt = 1, flt
            TRKSTK_19R(iyr,icafe19,iage,ifuel,iflt,11) = sum(TRKSTK_19R(iyr,icafe19,iage,ifuel,iflt,1:mnumcr-2))
          ENDDO
        ENDDO
      ENDDO
    ENDDO

	IF (curcalyr.gt.BSYR_STK)then
!      WRITE(21,'(a,2(",",i4),4(",",f12.0))')'trkstk2',curcalyr,curitr,sum(TRKSTK_19R(iyr,1:2,1,:,:,mnumcr)),sum(TRKSTK_19R(iyr,3:4,1,:,:,mnumcr)),sum(TRKSTK_19R(iyr,5:7,1,:,:,mnumcr)),sum(TRKSTK_19R(iyr,8:19,1,:,:,mnumcr))
            
!	  Summing the stock to match the reporting aggregation levels
      DO iage=1,age
        DO ifuel=1,FUEL12
   	      DO iflt = 1,FLT
            TRKSTK(iyr,1,iage,ifuel,iflt) = sum(TRKSTK_19R(iyr,3:4,iage,ifuel,iflt,mnumcr))
	  	    TRKSTK(iyr,2,iage,ifuel,iflt) = sum(TRKSTK_19R(iyr,5:7,iage,ifuel,iflt,mnumcr))
	  	    TRKSTK(iyr,3,iage,ifuel,iflt) = sum(TRKSTK_19R(iyr,8:19,iage,ifuel,iflt,mnumcr))
          ENDDO
!		  Putting the truck stock in the format used in the fuel economy model
          DO icafe19 = 1,CAFE19
            TRKSTK_19(iyr,icafe19,iage,ifuel) = sum(TRKSTK_19R(iyr,icafe19,iage,ifuel,1:2,mnumcr)) 	  
	      ENDDO
        ENDDO
      ENDDO

!	  age vehicle fuel economy to correspond with vehicle vintaging
      DO isc=1,sc
        DO iage=2,age-1
          HDV_MPG(iyr,isc,iage,1:FUEL12) = HDV_MPG(iyr-1,isc,IAGE-1,1:FUEL12)
        ENDDO
!...    combine mpg for the last two vintages , weighting by number of trucks
        DO ifuel=1,FUEL12
          VMTTMP(1)=SUM(VMTFLT(LAG,ISC,AGE-1,IFUEL,1:FLT))
          VMTTMP(2)=SUM(VMTFLT(LAG,ISC,AGE,IFUEL,1:FLT))
          IF(VMTTMP(1)+VMTTMp(2).le.0.0)then
            HDV_MPG(iyr,isc,AGE,ifuel) = HDV_MPG(iyr-1,isc,age-1,ifuel)
          ELSE
            HDV_MPG(iyr,isc,AGE,ifuel) = HARMONIC_MEAN(HDV_MPG(iyr-1,isc,AGE-1:AGE,ifuel),VMTTMP(1:2),2)
          ENDIF
        ENDDO
      ENDDO
    ENDIF	! curcalyr.gt.bsyr_stk

!...Commercial light truck
!	Note that for CLT, {1:gasoline, 2:diesel} (reversed from freight truck module)
    IF(curcalyr.ge.2012) then
      DO iage = 1,age
	    DO iregn = 1,MNUMCR-2
          DO ivoc = 1, 2
            DO ifuel = 1,FUEL12
              CLTSTK(iyr,CLTMap(ifuel),iage,ivoc,iregn) = sum(TRKSTK_19R(iyr,ivoc,iage,ifuel,1:2,iregn))
            ENDDO
          ENDDO
	    ENDDO
!		Calculate national numbers as well
		DO ifuel = 1,FUEL12
		  DO ivoc = 1,2
		    CLTSTK(n,ifuel,iage,ivoc,MNUMCR) = sum(CLTSTK(n,ifuel,iage,ivoc,1:MNUMCR-2))
		  ENDDO
		ENDDO
	  ENDDO
	  DO ifuel = 1,FUEL12
	    CLTSALT(ifuel,n) = sum(CLTSTK(n,ifuel,1,:,MNUMCR))                ! Total sales by powertrain
	    CLTSTKT(ifuel,n) = sum(CLTSTK(n,ifuel,1:34,:,MNUMCR))/ 1000.0     ! Total stock by powertrain
	  ENDDO
	ENDIF

!	Detailed outputs
	IF (curcalyr.eq.2050.and.FCRL.eq.1) THEN
!	  WRITE(21,*)'Vintaged_stock'
!	  DO i=2018-1989,MNUMYR
!	    DO iregn=1,MNUMCR-2
!	      DO icafe19=1,CAFE19
!		    DO ifuel=1,FUEL12
!	          DO iage=1,age
!		          WRITE(21,'(i3,",",i3,",",i4,",",i3,",",i3,",",9(f8.0,","))') iregn,ifuel,i+1989,icafe19,iage, &
!																			   TRKSTK_19R(i,icafe19,iage,ifuel,NFT,iregn) + TRKSTK_19R(i,icafe19,iage,ifuel,FLT,iregn)
!		      ENDDO
!		    ENDDO			
!		  ENDDO
!		ENDDO
!	  ENDDO
		
!	  Average vehicle age by size class and region
!	  avg_age(:,:,:) = 0.0
!	  age_wgt(:,:,:) = 0.0
!	  DO isc = 22, MNUMYR ! (2010-2050)
!	    DO iregn = 1, MNUMCR
!		    IF (iregn.eq.10) CYCLE
!		    DO icafe19 = 1, CAFE19
!		      DO iage = 1, age
!		        age_wgt(isc,icafe19,iregn) = age_wgt(isc,icafe19,iregn) + iage * sum(TRKSTK_19R(isc,icafe19,iage,:,:,iregn))
!		      ENDDO
!		      avg_age(isc,icafe19,iregn) = age_wgt(isc,icafe19,iregn) / sum(TRKSTK_19R(isc,icafe19,:,:,:,iregn))
!		    ENDDO
!		  ENDDO
!	  ENDDO
!
!	  WRITE(21,*)'Average M/HDV age'
!	  WRITE(21,'(a4,",",a6,10(",",a10))')'year','CAFE19','cd1','cd2','cd3','cd4','cd5','cd6','cd7','cd8','cd9','natl'
!	  DO isc = 24, MNUMYR ! (2012-2050)
!	    DO icafe19 = 1, CAFE19
!	      
!	      WRITE(21,'(I4,",",I6,10(",",F10.2))') isc+1989, icafe19, avg_age(isc,icafe19,1),avg_age(isc,icafe19,2),&
!	      										avg_age(isc,icafe19,3),avg_age(isc,icafe19,4),avg_age(isc,icafe19,5),&
!	      										avg_age(isc,icafe19,6),avg_age(isc,icafe19,7),avg_age(isc,icafe19,8),&
!	      										avg_age(isc,icafe19,9),avg_age(isc,icafe19,MNUMCR)
!		ENDDO
!	  ENDDO
	ENDIF	! (curcalyr.eq.2050.and.FCRL.eq.1)

 RETURN
 END SUBROUTINE TRUCK_STOCK


! ==========================================================================================================
!...Subroutine TRUCK_TECHADOPT
!   Description:
!   	Adopt technology from technology menu, based on cost effectiveness
!	Output:
!		Primarily modifies NEW_MPG_19 and inc_tech_cost_19
! ==========================================================================================================
   SUBROUTINE TRUCK_TECHADOPT
   USE F_
   IMPLICIT NONE

    INTEGER IP ,I
	INTEGER*2 LEARN_YR(14)/2014,2014,2015,2014,2014,2017,2014,2015,2016,2014,2021,2018,2021,2021/
    INTEGER   SLOWTECHS(17)/7,21,22,23,64,70,71,72,73,74,75,76,77,78,79,80,81/      ! Techs that EPA notes (Phase 3, 29491) are being adopted more slowly
	INTEGER*2 FYR_P2(techp2,CAFE19)
	INTEGER*2 LEARN_ADJ(techp2,CAFE19)
    REAL    P                                              !...market penetration fraction
    INTEGER INOTE
	REAL    TECHCOSTyr_p2(MNUMYR,techp2,CAFE19)
	REAL    TEMP_BTU_P2(CAFE14,FUEL12),Temp_sales_p2(CAFE14,FUEL12)
	REAL    TEMP_TRIG_P2(techp2,CAFE19)
	REAL    TECHPENYR_P2(MNUMYR,techp2,CAFE19,FUEL12)
	REAL    TOT_MKT, MAX_SHARE, MAX_SHARE_S(techp2)
	REAL    REQ_MKT, SYNERGY_LOSS(techp2), delta_mkt
	REAL    MPGEFF_19(FUEL12,CAFE19), techadjshr_P2(MNUMYR,techp2,CAFE19,FUEL12)
	REAL    TECHRPT_SAL(MNUMYR,10,FUEL12)
	REAL    TECHMID_P2_ADJ(TECHP2,CAFE14)
    
    LOGICAL tech_once_p2(techp2,CAFE19,FUEL12)
	LOGICAL REQUIRED
	
    IF(curcalyr.eq.2018) then
	  DO ifuel = 1,FUEL12
!...	Filling in phase 2 new_mpg variable - 19 CAFE class sizes
    	new_mpg_19(iyr-1,ifuel,:) = base_mpg_p2(ifuel,:)
        TECHPENYR_P2(:,:,:,:) = 0.0

!...	Align techs from Phase 1 to Phase 2 for base tech penetration
        DO icafe19 = 1,CAFE19
          DO itechp2 = 1,techp2	   
	        TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel) = TECHBASE_P2(itechp2,icafe19,ifuel)
		  ENDDO
	    ENDDO
      ENDDO     ! End fuel loop
	ENDIF      ! End Phase 2 initial year section

    if (itryghg.eq.1) then
      tech_once_p2(:,:,:) = .false.

      DO ifuel = 1,FUEL12
!...    calculate new truck energy consumption (million Btu) per vehicle
!	    Accumulate total annual energy consumed and total sales within each of the 14 regulatory size classes
!	    Note that ANNVMT_19 is from TRUCK_VMT and TRKSTK_19 (sales) is from the end of TRUCK_NEW, both subroutines that
!	    execute AFTER tech adoption, so use previous year values for weighting.
	    Temp_Btu_p2(:,ifuel) = 0.0
	    Temp_sales_p2(:,ifuel) = 0.0
	    DO icafe19 = 1, cafe19
	  	  icafe14 = P219Map(icafe19)	! crosswalk to 14 P2 regulatory groups
	  	  isc4 = SC19Map(icafe19)	! crosswalk to 4 NEMS reported size class groups
	  	  Temp_Btu_p2(icafe14,ifuel) 	= Temp_Btu_p2(icafe14,ifuel) &
	  	  							+ (ANNVMT_19(iyr,icafe19,1,ifuel) / NEW_MPG_19(iyr-1,ifuel,icafe19)*HRATE(isc4,ifuel)*(1E-6))*TRKSTK_19(iyr-1,icafe19,1,ifuel)
	  	  Temp_sales_p2(icafe14,ifuel)= Temp_sales_p2(icafe14,ifuel) + TRKSTK_19(iyr-1,icafe19,1,ifuel)
	    ENDDO
      
!	    Calculate average annual energy consumed within each of the 14 regulatory size classes
	    DO icafe14 = 1, CAFE14
	  	  if (Temp_sales_p2(icafe14,ifuel).gt.0.0) Temp_Btu_p2(icafe14,ifuel) = Temp_Btu_p2(icafe14,ifuel) / Temp_sales_p2(icafe14,ifuel)
	    ENDDO

!...    Incorporating technology cost learning curve into technology cost variable
	    DO icafe19 = 1,CAFE19
          icafe14 = P219Map(icafe19)	! crosswalk to 14 P2 regulatory groups
	      DO itechp2 = 1,techp2
            IF(techlearn_p2(itechp2,icafe14).gt.1) then 	! If assuming learning cost reductions for this tech
              fyr_p2(itechp2,icafe14) = minval(techfyr_p2(itechp2,icafe14,:))	! first year it is available on ANY powertrain
      
!	          Learning curves are pinned to actual years, rather than generic passage of time.
!             This shifts the learning curve so that it is "1" in the first year the tech is available, rather than a set calendar year
              learn_adj(itechp2,icafe14) = 0.0
	          IF(fyr_p2(itechp2,icafe14).gt.learn_yr(techlearn_p2(itechp2,icafe14))) then
	            learn_adj(itechp2,icafe14) 			= fyr_p2(itechp2,icafe14)-learn_yr(techlearn_p2(itechp2,icafe14))
	          ENDIF
	          TECHCOSTyr_p2(iyr,itechp2,icafe19) 	= techcost_p2(itechp2,icafe14)*learning_p2(techlearn_p2(itechp2,icafe14),iyr-learn_adj(itechp2,icafe14))
            ELSE
              techcostyr_p2(iyr,itechp2,icafe19) 	= techcost_p2(itechp2,icafe14)
            ENDIF

!            IF(ifuel.eq.1.and.fcrl.eq.1.and.icafe19.eq.cafe19) WRITE(21,'(a,",",i4,",",i2,19(",",f9.2))') 'techcost',curcalyr,itechp2,techcostyr_p2(iyr,itechp2,:)
            
          ENDDO
	    ENDDO
      ENDDO
    ENDIF   ! itryghg.eq.1

!...determine technology penetration trigger price for new technology penetration
	  DO icafe19 = 1,CAFE19
	    icafe14 = P219Map(icafe19)	! 1-14
	    icafe4 = P24Map(icafe14)
        IF(HDV_passGHG(iyr,icafe4)) cycle

	    DO ifuel = 1,FUEL12
          TECHSHR_P2(iyr,:,icafe19,ifuel) = 0.0
          DO itechp2 = 1,techp2  	
            TEMP_TRIG_P2(itechp2,icafe19) = 1.0     ! Don't think this should be 1.0 -- inflates the energy savings
            PREFF_P2 = 1.0
			  
!...        set trigger price for cost effectiveness calculation for technologies, penetrate technologies
!...        application criteria: is technology available by size class and fuel?
            IF(curcalyr.ge.TECHFYR_P2(itechp2,icafe14,ifuel)) then
              Payback1_P2(itechp2,icafe19) = Payback_P2(icafe14) + MIN(5,itryghg-1)
!...          calculate present value of energy savings over payback period (increase discount rate post-standards)
              DO ip = 1,payback1_P2(itechp2,icafe19)
!                IF(curcalyr.le.2032)then
			      IF(ANNVMT_19(iyr,icafe19,1,ifuel).gt.0) TEMP_TRIG_P2(itechp2,icafe19) = TEMP_TRIG_P2(itechp2,icafe19) + &
							((TEMP_BTU_p2(icafe14,ifuel)*ANNVMT_19(iyr,icafe19,ip,ifuel)/ANNVMT_19(iyr,icafe19,1,ifuel)*techeff_p2(itechp2,icafe14))/((1.0 + DISCRTXG_P2)**ip))
!                ELSE
!			      IF(ANNVMT_19(iyr,icafe19,1,ifuel).gt.0) TEMP_TRIG_P2(itechp2,icafe19) = TEMP_TRIG_P2(itechp2,icafe19) + &
!							((TEMP_BTU_p2(icafe14,ifuel)*ANNVMT_19(iyr,icafe19,ip,ifuel)/ANNVMT_19(iyr,icafe19,1,ifuel)*techeff_p2(itechp2,icafe14))/((1.0 + DISCRTXGL_P2)**ip))
!                ENDIF
              ENDDO
				
!...          calculate trigger price 									   [cost of tech]  / [energy saved from tech]  =  $ / mmbtu 
              TRIGGER_PRICE_P2(itechp2,icafe19,ifuel) = TECHCOSTyr_p2(iyr,itechp2,icafe19) / TEMP_TRIG_P2(itechp2,icafe19)
              
!              IF (TRIGGER_PRICE_P2(itechp2,icafe19,ifuel).lt.0.0) THEN
!                IF (TECHCOSTyr_p2(iyr,itechp2,icafe19).lt.0.0) THEN
!                  WRITE(21,'(a,5(",",i4),3(",",f10.2),3(",",i5))')'trig_neg_cost',curcalyr,itryghg,icafe19,ifuel,itechp2,TRIGGER_PRICE_P2(itechp2,icafe19,ifuel),TECHCOSTyr_p2(iyr,itechp2,icafe19),&
!                                    techcost_p2(itechp2,icafe14),fyr_p2(itechp2,icafe14),learn_yr(techlearn_p2(itechp2,icafe14)),techlearn_p2(itechp2,icafe14)
!                ELSE
!                  WRITE(21,'(a,5(",",i4),5(",",f10.2))')'trig_neg_trig',curcalyr,itryghg,icafe19,ifuel,itechp2,TRIGGER_PRICE_P2(itechp2,icafe19,ifuel),TECHCOSTyr_p2(iyr,itechp2,icafe19),&
!                                    TEMP_TRIG_P2(itechp2,icafe19),techeff_p2(itechp2,icafe14),TEMP_BTU_p2(icafe14,ifuel)
!                ENDIF
!              ENDIF

!...          set market penetration price sensitivity factor
              PREFF_P2(itechp2,icafe19,ifuel) = 1.0 + TECHVAR_P2(itechp2,icafe14)*((FUELPRICE_R_AVG(ifuel,MNUMCR)/TRIGGER_PRICE_P2(itechp2,icafe19,ifuel)) - 1.0)
			  PREFF_P2(itechp2,icafe19,ifuel) = MIN(PREFF_P2(itechp2,icafe19,ifuel),1.1)
!              IF((curcalyr).ge.2033) PREFF_P2(itechp2,icafe19,ifuel) = MIN(PREFF_P2(itechp2,icafe19,ifuel),1.1)

!...          is the fuel price above the trigger?		
              P = 0.0
              IF(FUELPRICE_R_AVG(ifuel,MNUMCR).ge.TRIGGER_PRICE_P2(itechp2,icafe19,ifuel).or.TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel).gt.0.0) then
!...          	market penetration equation:  s-shaped logistical equation to estimate market penetration over time
!				If the fuel price is above the trigger, grow market share along the tech's S-curve, scaled by price sensitivity factor (PREFF)
                IF(FUELPRICE_R_AVG(ifuel,MNUMCR).ge.TRIGGER_PRICE_P2(itechp2,icafe19,ifuel)) then
!                  IF (fcrl.eq.1) WRITE(21,'(a,5(",",i4),2(",",f8.2))')'trigger',curcalyr,itryghg,icafe19,ifuel,itechp2, TRIGGER_PRICE_P2(itechp2,icafe19,ifuel),FUELPRICE_R_AVG(ifuel,MNUMCR)
                  IF(.not.tech_once_P2(itechp2,icafe19,ifuel)) then	! Increment one year further up the s-curve
                    TECHPENYR_P2(iyr,itechp2,icafe19,ifuel) = TECHPENYR_P2(iyr-1,itechp2,icafe19,ifuel) + 1.0
                    tech_once_P2(itechp2,icafe19,ifuel) = .true.
                  ENDIF

!                 If failed CAFE/GHG in previous attempt, squeeze s-curve to ramp up allowable adoption rate more quickly
!                 Not applied to techs which EPA/manufacturers noted are being adopted more slowly than anticipated (SLOWTECHS)
                  IF (.not.ANY(SLOWTECHS(:).eq.itechp2)) THEN
                    TECHMID_P2_ADJ(itechp2,icafe14) = TECHMID_P2(itechp2,icafe14) * MAX(0.5,(0.95**(itryghg-1)))
                  ELSE
                    TECHMID_P2_ADJ(itechp2,icafe14) = TECHMID_P2(itechp2,icafe14)
                  ENDIF
!                  if(fcrl.eq.1) WRITE(21,*)'techmidadj',curcalyr,itryghg,icafe19,ifuel,itechp2,TECHMID_P2(itechp2,icafe14),TECHMID_P2_ADJ(itechp2,icafe14)
                  
                  P = 1.0/(1.0 + exp(-1.0*(TECHPENYR_P2(iyr,itechp2,icafe19,ifuel) - TECHMID_P2_ADJ(itechp2,icafe14))/TECHSHAPE_P2(itechp2,icafe14)))
                  P = MIN(TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel),TECHBASE_P2(itechp2,icafe19,ifuel)) &
                    + (TECHMAX_P2(itechp2,icafe14) - TECHBASE_P2(itechp2,icafe19,ifuel))*P

                  TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = MAX(0.0,MIN(TECHMAX_P2(itechp2,icafe14),PREFF_P2(itechp2,icafe19,ifuel)*P))	! Must be between 0 the maximum market share

!				If fuel price is below the trigger -- but there was adoption last year -- grow adoption with price sensitivity factor (PREFF)
                ELSE
                  IF(TECHPENYR_P2(iyr-1,itechp2,icafe19,ifuel).gt.0.) TECHPENYR_P2(iyr,itechp2,icafe19,ifuel) = TECHPENYR_P2(iyr-1,itechp2,icafe19,ifuel) - 1.0		 
                  TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel)*PREFF_P2(itechp2,icafe19,ifuel)
 			    ENDIF
              ENDIF
            ENDIF

!			Debug techshr
!			IF (ifuel.eq.2.and.icafe14.eq.1.and.itechp2.eq.83.and.fcrl.eq.1) THEN
!			  WRITE(21,'(a,",",i4,",",i2,16(",",f10.2))')'techshr',curcalyr,itryghg,TECHPENYR_P2(iyr,itechp2,icafe19,ifuel),TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel),TECHBASE_P2(itechp2,icafe19,ifuel),&
!										   TECHMID_P2_ADJ(itechp2,icafe14),TECHSHAPE_P2(itechp2,icafe14),TECHMAX_P2(itechp2,icafe14),P,PREFF_P2(itechp2,icafe19,ifuel),&
!                                           TEMP_TRIG_P2(itechp2,icafe19),TRIGGER_PRICE_P2(itechp2,icafe19,ifuel)*mc_jpgdp(2022-1989)/mc_jpgdp(1),FUELPRICE_R_AVG(ifuel,MNUMCR)*mc_jpgdp(2022-1989)/mc_jpgdp(1),TECHCOSTyr_p2(iyr,itechp2,icafe19)*mc_jpgdp(2022-1989)/mc_jpgdp(1),TEMP_BTU_p2(icafe14,ifuel),&
!                                           TECHSHR_P2(iyr,itechp2,icafe19,ifuel),NEW_MPG_19(iyr-1,ifuel,icafe19),ANNVMT_19(iyr,icafe19,1,ifuel)
!			ENDIF

			IF (TECHSHR_P2(iyr,itechp2,icafe19,ifuel).ne.TECHSHR_P2(iyr,itechp2,icafe19,ifuel)) THEN
			  WRITE(21,'(a,",",i2,",",i2,",",i4,",",i2,8(",",f6.2))') "TECHSHR_P2_BUSTED",icafe19,itechp2,iyr+1989,ifuel,TECHSHR_P2(iyr,itechp2,icafe19,ifuel),TECHBASE_P2(itechp2,icafe19,ifuel), &
																		TECHMAX_P2(itechp2,icafe14),techeff_p2(itechp2,icafe14),TEMP_BTU_p2(icafe14,ifuel), &
																		NEW_MPG_19(iyr-1,ifuel,icafe19),ANNVMT_19(iyr,icafe19,1,ifuel)/1000
			  WRITE(*,*)'ERROR: TDM TRANFRT TECHSHR CAFE compliance module. See p1/TRNOUT.txt.'
			  STOP 903
			ENDIF

		  ENDDO    ! End itechp2 loop

!...      Apply supersedes engineering notes.
          DO itechp2=1,techp2
		    MAX_SHARE_S(itechp2) = TECHMAX_P2(itechp2,icafe14)				
            IF(curcalyr.lt.TECHFYR_P2(itechp2,icafe14,ifuel)) cycle
            DO inote = 1,num_sup                                        ! Iterate through all supercede notes (num_sup)
              IF(SUPERSEDES_tech(1,inote).eq.itechp2) then              ! If this note is about tech itechp2 superceding
!...   	  		Set initial market share and market share maximum.
                TOT_MKT   = TECHSHR_P2(iyr,itechp2,icafe19,ifuel)
                MAX_SHARE = TECHMAX_P2(itechp2,icafe14)

!...   	  		Find maximum allowable tech chain penetration (maximum of current tech [max_share] and all techs it supercedes).
!               Cycle through the number of techs that are superceded in this note
				DO i = 2,tech_cnt_sup(inote)
				  MAX_SHARE = max(max_share,MAX_SHARE_S(supersedes_tech(i,inote)))					  
                ENDDO						

!...   	  		Find and adjust any EXCESS penetration downward.
!               One at a time, cycle through techs that are superceded, adding each of their adoption rates to the "parent" tech, and then
!               if that sum is higher than the max allowable, subtract share from the "child" tech
                DO i = 2,tech_cnt_sup(inote)
                  TOT_MKT = TOT_MKT + TECHSHR_P2(iyr,SUPERSEDES_tech(i,inote),icafe19,ifuel)
                  IF(TOT_MKT.gt.MAX_SHARE) then
                    TECHSHR_P2(iyr,SUPERSEDES_tech(i,inote),icafe19,ifuel) = MAX(0.0,TECHSHR_P2(iyr,SUPERSEDES_tech(i,inote),icafe19,ifuel) - (TOT_MKT - MAX_SHARE))
                    TOT_MKT = MAX_SHARE 	 						  						  
                  ENDIF								
                ENDDO  ! end tech_cnt loop						
              ENDIF
		    ENDDO   ! end num_sup loop
          ENDDO   ! end itechp2 loop for supersedes notes			

!...      Apply required engineering notes
          DO itechp2 = 1,techp2
            IF(curcalyr.lt.TECHFYR_P2(itechp2,icafe14,ifuel)) cycle
            REQUIRED = .FALSE.
            REQ_MKT  = 0.0
            DO inote = 1,num_req
              IF(REQUIRES_tech(1,inote).eq.itechp2) then					
                REQUIRED = .TRUE.
				REQ_MKT  = REQ_MKT + TECHSHR_P2(iyr,requires_tech(2,inote),icafe19,ifuel)				  
               ENDIF
            ENDDO
            IF(REQUIRED) then
              REQ_MKT = min(REQ_MKT,1.0)
			  TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = min(TECHSHR_P2(iyr,itechp2,icafe19,ifuel),REQ_MKT)
			ENDIF			
		  ENDDO      

!...      Apply required engineering notes when the required tech could be one of a few
          TECHSHR_P2(iyr,68,icafe19,ifuel) = min(TECHSHR_P2(iyr,68,icafe19,ifuel), & 
						(TECHSHR_P2(iyr,62,icafe19,ifuel) + TECHSHR_P2(iyr,66,icafe19,ifuel) + TECHSHR_P2(iyr,67,icafe19,ifuel)))
          TECHSHR_P2(iyr,69,icafe19,ifuel) = min(TECHSHR_P2(iyr,69,icafe19,ifuel), & 
						(TECHSHR_P2(iyr,62,icafe19,ifuel) + TECHSHR_P2(iyr,66,icafe19,ifuel) + TECHSHR_P2(iyr,67,icafe19,ifuel)))
          TECHSHR_P2(iyr,71,icafe19,ifuel) = min(TECHSHR_P2(iyr,71,icafe19,ifuel), & 
						(TECHSHR_P2(iyr,62,icafe19,ifuel) + TECHSHR_P2(iyr,66,icafe19,ifuel) + TECHSHR_P2(iyr,67,icafe19,ifuel)))

!...   	  Loop through and apply the synergy engineering notes
          DO itechp2 = 1,techp2        
            IF(curcalyr.lt.TECHFYR_P2(itechp2,icafe14,ifuel)) cycle
            SYNERGY_LOSS(itechp2) = 0.0

!...		Market share affected by synergy effects between two technologies is estimated as the probabilistic overlap
!...		between the market shares of the two technologies. Mathematically, this market share is expressed as the 
!...		product of the market shares of the two technologies.  The incremental market share overlap for a single year
!...		is equal to the cumulative estimated overlap (based on cumulative estimated market penetrations) for the 
!...		current year minus the cumulative estimated overlap for the previous year.  Note also, that the input value
!...		of SYNR_DEL is negative so that the estimated synergy loss will also be negative and should be treated as an
!...		additive parameter.
            DO inote = 1,num_syn
              IF(synergy_tech(1,inote).eq.itechp2) then
				DO i = 2,tech_cnt_syn(inote)
				  delta_mkt = (TECHSHR_P2(iyr,itechp2,icafe19,ifuel)*TECHSHR_P2(iyr,synergy_tech(i,inote),icafe19,ifuel)) - &
			              (TECHSHR_P2(iyr-1,itechp2,icafe19,ifuel)*TECHSHR_P2(iyr-1,synergy_tech(i,inote),icafe19,ifuel))
                  SYNERGY_LOSS(itechp2) = SYNERGY_LOSS(itechp2) + (DELTA_MKT*syn_per(i,inote))
		        ENDDO
              ENDIF
            ENDDO
!... 		Flex fuel technology improvements should be the same as gasoline technology improvements
            IF(ifuel.eq.5) TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = TECHSHR_P2(iyr,itechp2,icafe19,2)
!... 		PHEV aero, tire, and weight reduction should be the same as diesel and gasoline, respectively
            IF(ifuel.eq.7.and.itechp2.le.17) TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = TECHSHR_P2(iyr,itechp2,icafe19,1)
            IF(ifuel.eq.8.and.itechp2.le.17) TECHSHR_P2(iyr,itechp2,icafe19,ifuel) = TECHSHR_P2(iyr,itechp2,icafe19,2)
          ENDDO               ! end itechp2 loop for synergy notes

!...      determine combined MPG improvement of fuel-saving technologies by weighting each technologies improvement by market share
          MPGEFF_19(ifuel,icafe19) = 1.0
          DO itechp2 = 1,techp2
            IF(curcalyr.ge.techfyr_P2(itechp2,icafe14,ifuel))then
              IF(techshr_P2(iyr,itechp2,icafe19,ifuel).gt.0..and.techshr_p2(iyr,itechp2,icafe19,ifuel).ge.techbase_p2(itechp2,icafe19,ifuel)) then 
                techadjshr_P2(iyr,itechp2,icafe19,ifuel) = techshr_P2(iyr,itechp2,icafe19,ifuel) - TECHBASE_P2(itechp2,icafe19,ifuel)
                techadjshr_P2(iyr,itechp2,icafe19,ifuel) = max(techadjshr_P2(iyr,itechp2,icafe19,ifuel),-1.0*TECHBASE_P2(itechp2,icafe19,ifuel))
              ELSE
                techadjshr_P2(iyr,itechp2,icafe19,ifuel) = 0.
              ENDIF
			  MPGEFF_19(ifuel,icafe19) = MPGEFF_19(ifuel,icafe19)*(1.0 - ((techeff_p2(itechp2,icafe14) + & 
                                         synergy_loss(itechp2))*TECHadjSHR_P2(iyr,itechp2,icafe19,ifuel)))
			  MPGEFF_19(ifuel,icafe19) = min(mpgeff_19(ifuel,icafe19),1.0)
            ENDIF
		  ENDDO

          inc_tech_cost_19(iyr,icafe19,ifuel) = sum(techshr_P2(iyr,1:techp2,icafe19,ifuel)*TECHCOSTyr_p2(iyr,1:techp2,icafe19))

!...   	  determine new fuel economy for the 19 cafe size classes 
          NEW_MPG_19(iyr,ifuel,icafe19) = BASE_MPG_p2(ifuel,icafe19)/MPGEFF_19(ifuel,icafe19)
        ENDDO   ! ifuel		
      ENDDO		! icafe19

   RETURN
   END SUBROUTINE TRUCK_TECHADOPT

! ==========================================================================================================
!...Subroutine TRUCK_GHGMEET
!   Description:
!   	Ensure new vehicle sales meet EPA Phase 2 (MY2021-MY2027) and Phase 3 (MY2027-MY2032) GHG regulations
!       Sales are aggregated into "averaging sets", each of which must meet the regulation.
!           Light HDV (generally Class 2b-5 vocational), Medium HDV (generally Class 6-8 vocational), 
!           and Heavy HDV [generally Class 8 tractors]
!       Additionally, Class 2b-3 pickup/van standards are enforced here, as a separate averaging set (part of the LDV reg).
!       The result is captured in the variable HDV_passGHG, which is TRUE if the standard is passed and FALSE if not.
!       Quick order of operations for the subroutine: 
!         1. Determine the total emissions credit (positive) or deficit (negative) for each averaging set.
!         2. If positive, bank the credits. If negative, send the model back through the tech adoption and powertrain choice loop.
!         3. If still negative, spend historical banked credits. If still negative, trade credits across averaging sets.
!            Class 2b-3 PUV can only be traded to CLass 2b-5V and Class 6-7V (NOT tractor). 2b-5V, 6-7V, and 8 tractor can be exchanged
!            to each other (not to 2b-3 PUV).
!         4. If still negative, TRUCK_GHGZEV will ensure the standard is met (using ZEV adoption)
! ==========================================================================================================
   SUBROUTINE TRUCK_GHGMEET
   USE F_
   IMPLICIT NONE
    
    INTEGER IP, I,CRED_LIFE,MAX_CRED,spendall_yr,itrade,MAX_SPEND_SHR
    PARAMETER(CRED_LIFE = 5)
    
    REAL    GHG_creds(MNUMYR,CAFE14,FUEL12),GHGBank(MNUMYR,CAFE4,2)
    REAL    GHG_creds_avgset(MNUMYR,CAFE4,FUEL12)
    REAL    GHGBank_tot              ! Used for Phase 3 credit trading across avg sets. Doesn't include 2b/3 PUV (part of LDV reg)
    REAL    TEMP(FUEL12)
    REAL    AVG_AFV_CRED,SALES_TEMP
    
    LOGICAL ghg_debug
    INTEGER nonzevs(7) /1,2,3,4,5,11,12/             ! Powertrain indices for non-BEV/PHEV/FCV vehicles (H2 ICE ends up here since it doesn't get adv tech mult)
    INTEGER zevs(5) /6,7,8,9,10/                     ! Powertrain indices for BEV/PHEV/FCV vehicles 

    GHG_creds(6:2017-1989,:,:)       = 0.0
    GHGBank(6:2017-1989,:,:)         = 0.0
    GHG_creds_avgset(6:2017-1989,:,:)= 0.0
    GHG_creds_agg(6:2017-1989,:)     = 0.0
    TECHSHARE(6:2017-1989,:,:,:)     = 0.0
    
    ghg_debug = .false.
    spendall_yr = 2032

!   If running a case without Phase 3 GHG regulation (freeze standard at MY2027 level), let max credit spend start earlier    
    IF (HDV_GHGSTD(2,2032-1989,1).eq.HDV_GHGSTD(2,2027-1989,1)) spendall_yr = 2030
    
	GHG_creds(iyr,:,:) = 0.0
	DO icafe19 = 1, CAFE19
      icafe14 = P219Map(icafe19)
      icafe4 = P24Map(icafe14)
	  DO ifuel = 1, FUEL12
        if (new_mpg_19(iyr,ifuel,icafe19).eq.0.0) CYCLE
!       Calculate gCO2 per ton-mile, using EPA payloads and the conversion from gallons per mile to gCO2 per mile (note that ZEVs get an automatic zero here)
        new_gco2tonmi_p2(iyr,ifuel,icafe19) = 1.0/new_mpg_19(iyr,ifuel,icafe19) * GCO2_GAL(ifuel,icafe14) / (EPA_PAYLOAD(icafe14)/2000.0)
		GHG_creds(iyr,icafe14,ifuel) = GHG_creds(iyr,icafe14,ifuel) + (HDV_GHGSTD(icafe14,iyr,ifuel) - new_gco2tonmi_p2(iyr,ifuel,icafe19)) * (EPA_PAYLOAD(icafe14)/2000.0) &
                                                                      * SUM(TRKSTK_19R(iyr,icafe19,1,ifuel,:,1:mnumcr-2)) * HDV_AFV_CREDITS(ifuel,iyr) * HDV_USEFULLIFE(ifuel,icafe14) * 1.0E-6
!        if(fcrl.eq.1) WRITE(21,'(a,4(",",i4),3(",",f10.0),4(",",f9.2))')'ghgcred',curcalyr,itryghg,icafe19,ifuel,sum(GHG_creds(iyr,icafe14,:)),TRKSAL_P2(iyr,icafe14,ifuel),HDV_USEFULLIFE(ifuel,icafe14),HDV_GHGSTD(icafe14,iyr,ifuel),&
!                                                      new_gco2tonmi_p2(iyr,ifuel,icafe19),HDV_AFV_CREDITS(ifuel,iyr),EPA_PAYLOAD(icafe14)/2000
      ENDDO
    ENDDO

    GHG_creds_avgset(iyr,:,:) = 0.0
    GHG_creds_agg(iyr,:) = 0.0
    DO icafe14 = 1, CAFE14
      icafe4 = P24Map(icafe14)
      GHG_creds_avgset(iyr,icafe4,:) = GHG_creds_avgset(iyr,icafe4,:) + GHG_creds(iyr,icafe14,:)
      GHG_creds_agg(iyr,icafe4) = GHG_creds_agg(iyr,icafe4) + SUM(GHG_creds(iyr,icafe14,:))
    ENDDO

!...Do we meet the GHG standard?  Positive GHG_creds == yes, negative == no
!   This runs for every averaging set every itryghg -- i.e. if one fails, everyone re-runs, since the amount banked could change for other averaging sets (they could overshoot the standard MORE
!   than they did before, after the new tech adoption and alt powertrain choice results 
    GHGBank(iyr,:,:) = 0.0
    DO icafe4 = 1,CAFE4
!     Beyond MY2029, advanced technology credits can no longer be spent (Phase 3 -- p. 29605). Convert these to base credits, add them to the base bank, zero out the adv. bank
      IF (curcalyr.eq.2030.and.itryghg.eq.1) THEN
        DO i = 3,CRED_LIFE      ! 2027 (i=3) and before only, because no adv tech credits generated after MY2027 anyway
!         First estimate sales-weighted average credits per vehicle
          AVG_AFV_CRED = 0.0
          SALES_TEMP = 0.0
          DO icafe14 = 1, CAFE14
            IF (P24Map(icafe14).ne.icafe4) CYCLE
            AVG_AFV_CRED = AVG_AFV_CRED + SUM(TRKSAL_P2(iyr-i,icafe14,zevs)*HDV_AFV_CREDITS(zevs,iyr-i))
            SALES_TEMP = SALES_TEMP + SUM(TRKSAL_P2(iyr-i,icafe14,zevs))
          ENDDO
          AVG_AFV_CRED = AVG_AFV_CRED / SALES_TEMP
!         Convert advanced technology credits to base credits using above average credit per vehicle
          if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,4(",",i4),4(",",f12.1))')'2030_advcred',curcalyr,itryghg,icafe4,i,GHGBank(iyr-i,icafe4,1),GHGBank(iyr-i,icafe4,2),AVG_AFV_CRED,SALES_TEMP
          GHGBank(iyr-i,icafe4,1) = GHGBank(iyr-i,icafe4,1) + GHGBank(iyr-i,icafe4,2)/ AVG_AFV_CRED
          GHGBank(iyr-i,icafe4,2) = 0.0
        ENDDO
      ENDIF
            
!     If we meet the standard, bank the credits
      IF(GHG_creds_agg(iyr,icafe4).ge.0.0) then
		HDV_passGHG(iyr,icafe4) = .true.

!       If beyond the cutoff for generating advanced technology credits (in the case of Phase 3 GHG, MY2027 is the last year -- p. 29605), everything gets dumped in the same bank
        IF (ALL(HDV_AFV_CREDITS(:,iyr).eq.1.0)) THEN
          GHGBank(iyr,icafe4,1) = GHG_creds_agg(iyr,icafe4)

!       Otherwise, need to bank credits separately so they can be spent in the correct order (base credits first, then advanced)
        ELSE
          IF (SUM(GHG_creds_avgset(iyr,icafe4,nonzevs)).lt.0.0) THEN              ! If base credits are negative
            GHGBank(iyr,icafe4,1) = 0.0                                           ! bank no base credits 
            GHGBank(iyr,icafe4,2) = SUM(GHG_creds_avgset(iyr,icafe4,zevs)) &      ! payoff the base deficit and bank the remainder of advanced
                                  + SUM(GHG_creds_avgset(iyr,icafe4,nonzevs))     
          ELSE 
            GHGBank(iyr,icafe4,1) = SUM(GHG_creds_avgset(iyr,icafe4,nonzevs))     ! Otherwise bank both base and advanced credits separately
            GHGBank(iyr,icafe4,2) = SUM(GHG_creds_avgset(iyr,icafe4,zevs))
          ENDIF
        ENDIF
		if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'passedGHG',curcalyr,itryghg,icafe4,GHGBank(iyr,icafe4,1),GHGBank(iyr,icafe4,2)
        
!     If we don't meet the standard, hit the bank, but only after having exhausted tech adoption and alt powertrain options
      ELSEIF(itryghg.eq.ghgtry) THEN
        MAX_CRED = 5
        IF (curcalyr.ge.spendall_yr) MAX_CRED = 1
        IF (sum(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,icafe4,1)).gt.0.0) THEN      ! First spend all the base credits
          if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),",",f12.1)')'basebnk_nonzero',curcalyr,itryghg,icafe4,sum(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,icafe4,1))
!         If there are enough base credits to cover the entire deficit (all fuels), cover it
          IF (sum(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,icafe4,1)).gt.-GHG_creds_agg(iyr,icafe4)) THEN
            if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'basebnk_meets',curcalyr,itryghg,icafe4,sum(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,icafe4,1)),GHG_creds_agg(iyr,icafe4)
!           Work through credit bank, from oldest MY to latest MY
            DO i = CRED_LIFE,MAX_CRED,-1
!             If this MY has enough credits to cover the current MY deficit, cover it.
              IF (GHGBank(iyr-i,icafe4,1).ge.-GHG_creds_agg(iyr,icafe4)) THEN
		        if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'passedGHGbsebnk1',curcalyr,itryghg,icafe4,GHGBank(iyr-i,icafe4,1),GHG_creds_agg(iyr,icafe4)
                GHGBank(iyr-i,icafe4,1) = GHGBank(iyr-i,icafe4,1) + GHG_creds_agg(iyr,icafe4)
                GHG_creds_agg(iyr,icafe4) = 0.0
                if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'passedGHGbsebnk2',curcalyr,itryghg,icafe4,GHGBank(iyr-i,icafe4,1),GHG_creds_agg(iyr,icafe4)
                HDV_passGHG(iyr,icafe4) = .true.
                EXIT
!             If this MY doesn't have enough credits, spend them all and go to the next MY
              ELSEIF(GHGBank(iyr-i,icafe4,1).gt.0.0) THEN
                if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'spentsomebsebnk1',curcalyr,itryghg,icafe4,GHGBank(iyr-i,icafe4,1),GHG_creds_agg(iyr,icafe4)
                GHG_creds_agg(iyr,icafe4) = GHG_creds_agg(iyr,icafe4) + GHGBank(iyr-i,icafe4,1)
                GHGBank(iyr-i,icafe4,1) = 0.0
                if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'spentsomebsebnk2',curcalyr,itryghg,icafe4,GHGBank(iyr-i,icafe4,1),GHG_creds_agg(iyr,icafe4)
              ENDIF
            ENDDO
            if (HDV_passGHG(iyr,icafe4)) CYCLE  ! next icafe4
          ELSE        ! Empty the rest of the base credit bank if it is smaller than the deficit
            if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'basebnk_dntmeet1',curcalyr,itryghg,icafe4,sum(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,icafe4,1)),GHG_creds_agg(iyr,icafe4)
            GHG_creds_agg(iyr,icafe4) = GHG_creds_agg(iyr,icafe4) + sum(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,icafe4,1))
            GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,icafe4,1) = 0.0
            if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'basebnk_dntmeet2',curcalyr,itryghg,icafe4,sum(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,icafe4,1)),GHG_creds_agg(iyr,icafe4)
          ENDIF
        ENDIF
!       Now work through advanced technology credits
        IF (sum(GHGBank(iyr-CRED_LIFE:iyr-1,icafe4,2)).gt.0.0) THEN
          if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),",",f12.1)')'advbank_nonzero',curcalyr,itryghg,icafe4,sum(GHGBank(iyr-CRED_LIFE:iyr-1,icafe4,2))
          IF (sum(GHGBank(iyr-CRED_LIFE:iyr-1,icafe4,2)).gt.-GHG_creds_agg(iyr,icafe4)) THEN
            if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'advbank_meets',curcalyr,itryghg,icafe4,sum(GHGBank(iyr-CRED_LIFE:iyr-1,icafe4,2)),GHG_creds_agg(iyr,icafe4)
!           Work through credit bank, from oldest MY to latest MY
            DO i = CRED_LIFE,1,-1
!             If this MY has enough credits to cover the current MY deficit, cover it.
              IF (GHGBank(iyr-i,icafe4,2).ge.-GHG_creds_agg(iyr,icafe4)) THEN
                if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'passedGHGadvbnk1',curcalyr,itryghg,icafe4,GHGBank(iyr-i,icafe4,2),GHG_creds_agg(iyr,icafe4)
                GHGBank(iyr-i,icafe4,2) = GHGBank(iyr-i,icafe4,2) + GHG_creds_agg(iyr,icafe4)
                GHG_creds_agg(iyr,icafe4) = 0.0
                if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'passedGHGadvbnk2',curcalyr,itryghg,icafe4,GHGBank(iyr-i,icafe4,2),GHG_creds_agg(iyr,icafe4)
                HDV_passGHG(iyr,icafe4) = .true.
                EXIT
!             If this MY doesn't have enough credits, spend them all and go to the next MY
              ELSEIF(GHGBank(iyr-i,icafe4,2).gt.0.0) THEN
                if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'spentsomeadvbnk1',curcalyr,itryghg,icafe4,GHGBank(iyr-i,icafe4,2),GHG_creds_agg(iyr,icafe4)
                GHG_creds_agg(iyr,icafe4) = GHG_creds_agg(iyr,icafe4) + GHGBank(iyr-i,icafe4,2)
                GHGBank(iyr-i,icafe4,2) = 0.0
                if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'spentsomeadvbnk2',curcalyr,itryghg,icafe4,GHGBank(iyr-i,icafe4,2),GHG_creds_agg(iyr,icafe4)
              ENDIF
            ENDDO
          ELSE        ! Empty the bank if it is smaller than the deficit
            if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'advbnk_dntmeets1',curcalyr,itryghg,icafe4,sum(GHGBank(iyr-CRED_LIFE:iyr-1,icafe4,1)),GHG_creds_agg(iyr,icafe4)
            GHG_creds_agg(iyr,icafe4) = GHG_creds_agg(iyr,icafe4) + sum(GHGBank(iyr-CRED_LIFE:iyr-1,icafe4,2))
            GHGBank(iyr-CRED_LIFE:iyr-1,icafe4,2) = 0.0
            if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'advbnk_dntmeets2',curcalyr,itryghg,icafe4,sum(GHGBank(iyr-CRED_LIFE:iyr-1,icafe4,1)),GHG_creds_agg(iyr,icafe4)
          ENDIF
        ENDIF

        if(fcrl.eq.1.and.ghg_debug.and.HDV_passGHG(iyr,icafe4).eq..false.) WRITE(21,'(a16,3(",",i4),4(",",f12.1))')'failed_ghg_toZEV',curcalyr,itryghg,icafe4,sum(GHG_creds_avgset(iyr,icafe4,:)),GHG_creds_agg(iyr,icafe4),&
                                                                                       sum(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,icafe4,1)),sum(GHGBank(iyr-CRED_LIFE:iyr-1,icafe4,2))
        
      ELSE
        if(fcrl.eq.1.and.ghg_debug.and.HDV_passGHG(iyr,icafe4).eq..false.) WRITE(21,'(a16,3(",",i4),4(",",f12.1))')'failed_ghg_tryagain',curcalyr,itryghg,icafe4,GHG_creds_agg(iyr,icafe4)
      ENDIF        
    ENDDO   ! icafe4

!   If the standard is not met under the above credit spending assumptions (saving credits for later)
!   trade across averaging sets (per Phase 3 reg, p. 29607). In order to do this, all credits in an avg. set must be
!   spent against that avg. set's deficit FIRST (if that set has a deficit).
!   Additionally, the credits transferred from other averaging sets must be spent from NEWEST to OLDEST.
!   Note that 2b/3 PUV credits can only be transferred ONE WAY -- and only to Light HDV and Medium HDV avg sets (icafe = 2 and 3).
!   This starts the year AFTER TRUCK_GHGMEET allows OEMs to start using all of their credits (instead of just the oldest)
    IF (curcalyr.ge.spendall_yr.and.itryghg.eq.ghgtry) THEN

!     Transfer 2b/3 PUV credits to 2b-5V, then 6-7V
      DO icafe4 = 2,3
        IF (.not.HDV_passGHG(iyr,icafe4).and.SUM(GHGBank(iyr-CRED_LIFE:iyr-1,1,1)).gt.0.0) THEN
!         If there are enough 2b/3 PUV credits to meet the full 2b-5V deficit, meet it
          IF (sum(GHGBank(iyr-CRED_LIFE:iyr-1,1,1)).gt.-GHG_creds_agg(iyr,icafe4)) THEN
            if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,4(",",i4),2(",",f12.2))')'2b3_credshift',curcalyr,itryghg,icafe4,SUM(GHGBank(iyr-CRED_LIFE:iyr-1,1,1)),GHG_creds_agg(iyr,icafe4)
            DO i = 1,CRED_LIFE
              IF (GHGBank(iyr-i,1,1).ge.-GHG_creds_agg(iyr,icafe4)) THEN
                GHGBank(iyr-i,1,1)         = GHGBank(iyr-i,1,1) - GHG_creds_agg(iyr,icafe4)
                GHG_creds_agg(iyr,icafe4)  = 0.0
                HDV_passGHG(iyr,icafe4)    = .TRUE.
                if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,4(",",i4),2(",",f12.2))')'passedGHG_shift',curcalyr,itryghg,icafe4,i,GHGBank(iyr-i,1,1),GHG_creds_agg(iyr,icafe4)
              ELSE
                GHG_creds_agg(iyr,icafe4)  = GHG_creds_agg(iyr,icafe4) + GHGBank(iyr-i,1,1)
                GHGBank(iyr-i,1,1)         = 0.0
              ENDIF
            ENDDO
!         If not, empty the 2b/3 PUV bank into the 2b-5V deficit
          ELSE
            GHG_creds_agg(iyr,icafe4)  = GHG_creds_agg(iyr,icafe4) + SUM(GHGBank(iyr-CRED_LIFE:iyr-1,1,1))
            GHGBank(iyr-CRED_LIFE:iyr-1,1,1) = 0.0
          ENDIF
        ENDIF
      ENDDO

!     Transfer 2b-5V, 6-7V, and 7-8T credits as needed
!     Don't allow the full bank to be emptied -- maximum of MAX_SPEND_SHR 
      MAX_CRED = 3
      MAX_SPEND_SHR = 0.5
      GHGBank_tot    = SUM(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,2:4,1))
      if (GHGBank_tot.gt.0.0) THEN
        do icafe4 = 2, cafe4
          IF (HDV_passGHG(iyr,icafe4)) CYCLE
          do i = 2, cafe4
            IF (i.eq.icafe4) CYCLE
            IF (SUM(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,i,1)).gt.0.0) THEN
              if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,4(",",i4),2(",",f12.2))')'avgset_credshift',curcalyr,itryghg,i,icafe4,SUM(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,i,1))*MAX_SPEND_SHR,GHG_creds_agg(iyr,icafe4)
!             If there are enough credits in avg set (i) to meet the full deficit in avg set (icafe4), meet it
              IF (SUM(GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,i,1))*MAX_SPEND_SHR.gt.-GHG_creds_agg(iyr,icafe4)) THEN
                DO itrade = MAX_CRED,CRED_LIFE
                  IF (GHGBank(iyr-itrade,i,1)*MAX_SPEND_SHR.ge.-GHG_creds_agg(iyr,icafe4)) THEN
                    GHGBank(iyr-itrade,i,1)    = GHGBank(iyr-itrade,i,1) - GHG_creds_agg(iyr,icafe4)
                    GHG_creds_agg(iyr,icafe4)  = 0.0
                    HDV_passGHG(iyr,icafe4)    = .TRUE.
                    if(fcrl.eq.1.and.ghg_debug) WRITE(21,'(a16,4(",",i4),2(",",f12.2))')'passedGHG_shift',curcalyr,itryghg,icafe4,itrade,GHGBank(iyr-itrade,i,1),GHG_creds_agg(iyr,icafe4)
                  ELSE
                    GHG_creds_agg(iyr,icafe4)  = GHG_creds_agg(iyr,icafe4) + GHGBank(iyr-itrade,i,1)*MAX_SPEND_SHR
                    GHGBank(iyr-itrade,i,1)    = GHGBank(iyr-itrade,i,1) * (1-MAX_SPEND_SHR)
                  ENDIF
                ENDDO
!             If not, empty the avg set (i) bank into the avg set (icafe4) deficit
              ELSE
                GHG_creds_agg(iyr,icafe4)  = GHG_creds_agg(iyr,icafe4) + SUM(GHGBank(iyr-CRED_LIFE:iyr-1,i,1)) * MAX_SPEND_SHR
                GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,i,1) = GHGBank(iyr-CRED_LIFE:iyr-MAX_CRED,i,1) * (1-MAX_SPEND_SHR)
              ENDIF              
            ENDIF
          ENDDO ! icafe4
        ENDDO   ! icafe4
      ENDIF     ! GHG_bank_tot > 0
    ENDIF       ! curcalyr.ge.spendall_yr.and.itryghg.eq.ghgtry

!   Detailed output	  
	IF (curcalyr.eq.2050.and.fcrl.eq.1.and.ALL(HDV_passGHG(iyr,:))) then
!	  WRITE(21,*)'HDV_passGHG'
!	  DO i = 29, MNUMYR
!		WRITE(21,'(i4,14(",",L1))') i+1989,HDV_passGHG(i,:)
!	  ENDDO
!      WRITE(21,*)'GHG_creds'
!	  DO i = 29, MNUMYR
!		WRITE(21,'(i4,14(",",f10.0))') i+1989,GHG_creds(i,:)
!	  ENDDO
!	  WRITE(21,*)'GHGBank'
!	  DO i = 29, MNUMYR
!		WRITE(21,'(i4,14(",",f10.0))') i+1989,GHGBank(i,:)
!	  ENDDO
!	  WRITE(21,*)'new_mpg_p2'
!	  DO i = 29, MNUMYR
!		DO ifuel = 1, FUEL12
!		  WRITE(21,'(i4,",",i2,19(",",f7.3))') i+1989,ifuel,new_mpg_p2(i,ifuel,:)
!		ENDDO
!	  ENDDO
!	  WRITE(21,*)'TRKSTK_19 -- National sales by fuel'
!	  DO i = 29, MNUMYR
!		DO ifuel = 1, FUEL12
!		  WRITE(21,'(i4,",",i2,19(",",f10.1))') i+1989,ifuel,TRKSTK_19(i,:,1,ifuel)
!		ENDDO
!	  ENDDO
!	  WRITE(21,*)'new_gco2tonmi_p2'
!	  DO i = 29, MNUMYR
!		DO ifuel = 1, FUEL12
!		  WRITE(21,'(i4,",",i2,19(",",f10.1))') i+1989,ifuel,new_gco2tonmi_p2(i,ifuel,:)
!		ENDDO
!	  ENDDO
!	  WRITE(21,*)'HDV_GHGSTD'
!      DO i = 29, MNUMYR
!		DO ifuel = 1, FUEL12
!		  WRITE(21,'(i4,",",i2,14(",",f10.1))') i+1989,ifuel,HDV_GHGSTD(:,i,ifuel)
!		ENDDO
!	  ENDDO
      
	ENDIF

 RETURN
 END SUBROUTINE TRUCK_GHGMEET

! ==========================================================================================================
!...Subroutine TRUCK_GHGZEV
!   Description:
!   	In a situation where the model is unable to comply with EPA GHG standards via adoption of cost-effective tech, 
!       TRUCK_GHGZEV converts ICE vehicle sales to ZEVs to ensure compliance.
! ==========================================================================================================
 SUBROUTINE TRUCK_GHGZEV
 USE F_
 IMPLICIT NONE
    
    INTEGER ghg_maxice,ghg_maxzev,iice,izev,k
    PARAMETER(ghg_maxice = 4)                           ! Number of ICE powertrains that will be replaced with ZEVs to comply
    PARAMETER(ghg_maxzev = 4)                           ! Number of ZEV powertrains available to replace ICE to comply

    REAL pre_ghg_sales                                  ! Temporary; total sales by regn/icafe19/iflt to allow for fuel_shr_ivmt updates as sales are moved
    REAL ghg_npv(ghg_maxzev)			                ! Temporary; net present value used to allocate ACT sales forcing by powertrain
    REAL sales_needed(ghg_maxice)                       ! Sales to be converted to ZEV, by ICE powertrain
    REAL sales_shifted                                  ! Temporary accumulator for ICE sales removed; to add to ZEV sales
    REAL deficit(cafe19,ghg_maxice,mnumcr,flt,mnumyr)   ! Deficit disaggregated down to size class, powertrain, region, and fleet, to allocate ZEV conversion requirement across the fleet
    REAL tot_deficits(cafe4,mnumyr)                     ! Total deficit (aggregate of var deficit)
    REAL ICE_shr(ghg_maxice)                            ! Used to divy out sales (to be converted to ZEV) by ivmt bin
    REAL cred_gen(cafe4,mnumyr)
	REAL avg_npv(ghg_maxzev)                            ! Temporary; average NPV of savings/losses, across all financial horizon bins
    REAL fuel_shr_ivmt_temp(ghg_maxice)                 ! Temporary; hang on to shares before they are modified
    INTEGER ICE_map(ghg_maxice) /1,2,4,11/              ! Map from ghg_maxice to fuel12
    INTEGER ZEV_map(ghg_maxzev) /6,9,10,12/             ! Map from ghg_maxzev to fuel12
    LOGICAL ghgzev_debug
    
    ghgzev_debug = .false.
    
    do iregn = 1, mnumcr-2
      do iflt = 1, flt
        do icafe19 = 1, cafe19
          icafe14 = P219Map(icafe19)
          icafe4 = P24Map(icafe14)
          if (HDV_passGHG(iyr,icafe4)) CYCLE
          do iice = 1, ghg_maxice
            ifuel = ICE_map(iice)
            deficit(icafe19,iice,iregn,iflt,iyr) = max(0.0,(1/new_mpg_19(iyr,ifuel,icafe19) * GCO2_GAL(ifuel,icafe14) * (EPA_PAYLOAD(icafe14)/2000.0) - HDV_GHGSTD(icafe14,iyr,ifuel)) &
                                                         * TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) * HDV_USEFULLIFE(ifuel,icafe14) * 1.0E-6)
            if (fcrl.eq.1.and.ghgzev_debug) WRITE(21,'(a,5(",",i4),",",f9.1,2(",",f9.5),3(",",f9.1))')'deficit',curcalyr,iregn,iflt,icafe19,ifuel,deficit(icafe19,iice,iregn,iflt,iyr),1/new_mpg_19(iyr,ifuel,icafe19), &
                                                        HDV_GHGSTD(icafe14,iyr,ifuel),GCO2_GAL(ifuel,icafe14),EPA_PAYLOAD(icafe14)/2000.0,TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn)
            if (icafe19.eq.cafe19) deficit(icafe19,iice,iregn,iflt,iyr) = 0.0       ! Don't apply to heavy haul
          enddo
        enddo
      enddo
    enddo
    
    tot_deficits(:,iyr) = 0.0
    do icafe19 = 1, cafe19
      icafe4 = P24Map(P219Map(icafe19))
      tot_deficits(icafe4,iyr) = tot_deficits(icafe4,iyr) + sum(deficit(icafe19,:,1:mnumcr-2,:,iyr))
    enddo
    
    cred_gen(:,iyr) = 0.0
    do iregn = 1, mnumcr-2
      DO icafe19 = 1, cafe19  
        if (icafe19.eq.cafe19) CYCLE        ! Don't apply to heavy haul
        icafe14 = P219Map(icafe19)
        icafe4 = P24Map(icafe14)
        if (HDV_passGHG(iyr,icafe4)) CYCLE      ! No need to force in ZEVs if standard is met
        
        ghg_comp_cost(icafe19,iregn,iyr) = 0.0
        
        do iflt = 1, flt
!         Hang on to sales before the adjustment is made, so that fuel shares by vmt bin can be adjusted as we go
          pre_ghg_sales = sum(TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn))
!...      Calculate the number of sales that need to be converted to ZEV, by powertrain
          do iice = 1, ghg_maxice
            ifuel = ICE_map(iice)
!           Divy out the credits needed based on how the GHG deficits were distributed across
!           the major ICE powertrains
            sales_needed(iice) = deficit(icafe19,iice,iregn,iflt,iyr) / tot_deficits(icafe4,iyr) &
                                  * (-GHG_creds_agg(iyr,icafe4))
!           Convert those credits into sales
            sales_needed(iice) = sales_needed(iice) / HDV_USEFULLIFE(ifuel,icafe14) / 1.0E-6 &
                                  / (1/new_mpg_19(iyr,ifuel,icafe19) * GCO2_GAL(ifuel,icafe14) )
            if (fcrl.eq.1.and.ghgzev_debug) WRITE(21,'(a,5(",",i4),7(",",f12.2))')'sales_needed',curcalyr,icafe19,ifuel,iflt,iregn,sales_needed(iice),&
                                                            deficit(icafe19,iice,iregn,iflt,iyr),tot_deficits(icafe4,iyr),GHG_creds_agg(iyr,icafe4), &
                                                            HDV_GHGSTD(icafe14,iyr,ifuel),HDV_USEFULLIFE(ifuel,icafe14),EPA_PAYLOAD(icafe14)/2000.0

            IF (sales_needed(iice).ne.sales_needed(iice).and.fcrl.eq.1) THEN
              WRITE(21,'(a,5(",",i4),4(",",f9.1))')'ERROR: TRUCK_GHGZEV NaN sales_needed',curcalyr,icafe19,ifuel,iflt,iregn,sales_needed(iice),&
                                                            deficit(icafe19,iice,iregn,iflt,iyr),tot_deficits(icafe4,iyr),GHG_creds_agg(iyr,icafe4)
            ENDIF
          enddo
          
          do ivmt = 1, nvmt
!...        Calculate the share of the current iregn/icafe19/iflt group that is
!           in this VMT bin, for each of the powertrains that will be converted to ZEV
            do iice = 1, ghg_maxice
              ifuel = ICE_map(iice)
              if (ivmt.eq.1) fuel_shr_ivmt_temp(iice) = sum(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))
              if (fuel_shr_ivmt_temp(iice).gt.0.0) then
                ICE_shr(iice) = fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn) &
                              / fuel_shr_ivmt_temp(iice)
              else
                ICE_shr(iice) = 0.0
              endif
              IF (ICE_shr(iice).ne.ICE_shr(iice).and.fcrl.eq.1) THEN
                WRITE(21,'(a,6(",",i4),3(",",f9.5))')'ERROR: TRUCK_GHGZEV NaN ICE_shr',curcalyr,icafe19,ifuel,iflt,iregn,ivmt,ICE_shr(iice),&
                                                        fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn),sum(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))
              ENDIF
            enddo

!...        Estimate the distribution of ZEVs (BEV v. FCEV v. FCHEV v. H2 ICE)
!           based on the net present value of savings (or losses) over the average financial horizon [npv_choice]
!           Smoothed to prevent jolts in market share b/w the different ZEV options
!           First, estimate the average NPV of savings/losses (weighted by financial horizon bin)
            DO izev = 1,ghg_maxzev
              ifuel = ZEV_map(izev)
              avg_npv(izev) = SUM(PBACK_SHR(:,icafe19) * npv_choice(icafe19,ifuel,ivmt,:,iregn,iyr))
            ENDDO
!           Incorporate up-front cost into the NPV            
	        IF (iflt.eq.1.or.icafe19.ge.16) then
		      ghg_npv(1) 	= avg_npv(1) - (INC_COST_BEV(ivmt,icafe19,iyr)-BEV_infra_cost(icafe19,iyr))
		    ELSE 
		      ghg_npv(1) 	= avg_npv(1) - INC_COST_BEV(ivmt,icafe19,iyr)
	        ENDIF
		    ghg_npv(2) =  avg_npv(2) - INC_COST_FCEV(ivmt,icafe19,iyr)
	        ghg_npv(3) =  avg_npv(3) - INC_COST_FCHEV(ivmt,icafe19,iyr)
	        ghg_npv(4) =  avg_npv(4) - INC_COST_H2ICE(ivmt,icafe19,iyr)
            
!           Calculate sales shares based on NPVs
		    ghg_npv(:) = ghg_npv(:) / 10000							! Scale parameter
            ghg_npv(:) = exp(ghg_npv(:))/(exp(ghg_npv(1))+exp(ghg_npv(2))+exp(ghg_npv(3))+exp(ghg_npv(4)))

!		    Default Class 2b/3 ZEV option is BEV -- no FCEV/FCHEV/H2ICE
		    IF (icafe19.le.4) then
		      ghg_npv(1) = 1
		      ghg_npv(2:ghg_maxzev) = 0.0
		    ENDIF
            
            ghg_comp_cost(icafe19,iregn,iyr) = ghg_comp_cost(icafe19,iregn,iyr) + SUM(ghg_npv(:) * avg_npv(:)) * SUM(sales_needed(:)*ICE_shr(:))
            
!...        Move all the sales out of ICE and into ZEV
            sales_shifted = 0.0
            do iice = 1, ghg_maxice
              ifuel = ICE_map(iice)
!             Remove ICE sales that are to be converted to ZEV
              TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) &
                                                         - sales_needed(iice) * ICE_shr(iice)
!             Accumulate credits generated by replacing those ICE sales with ZEVs
              cred_gen(icafe4,iyr) = cred_gen(icafe4,iyr) + (sales_needed(iice) * ICE_shr(iice)) &
                                                             *(1/new_mpg_19(iyr,ifuel,icafe19) * GCO2_GAL(ifuel,icafe14) * HDV_USEFULLIFE(ifuel,icafe14)) * 1.0E-6
!             Accumulate ICE sales that have been removed, to add to ZEVs
              sales_shifted = sales_shifted + sales_needed(iice) * ICE_shr(iice)
!             Adjust fuel share within the vmt bin to account for shifted sales
              fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn) = fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn) &
                                                               - sales_needed(iice) * ICE_shr(iice) / pre_ghg_sales
              
              IF (TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn).ne.TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn)) THEN
                WRITE(21,'(a,6(",",i4),2(",",f8.1),",",f8.5)')'ERROR: TRUCK_GHGZEV NaN stocks',curcalyr,curitr,icafe19,ifuel,iflt,iregn,&
                                                              TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn),sales_needed(iice),ICE_shr(iice)
              ENDIF
            enddo
!           Now add the ICE vehicles (removed above) to the ZEVs, and adjust their fuel shares within each VMT bin
            do izev = 1, ghg_maxzev
              ifuel = ZEV_map(izev)
              TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) = TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) &
                                                         + sales_shifted * ghg_npv(izev)
              fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn) = fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn) &
                                                               + sales_shifted * ghg_npv(izev) / pre_ghg_sales
            enddo
            
            if(fcrl.eq.1.and.ghgzev_debug) WRITE(21,'(a,5(",",i4),",",f12.1,9(",",f9.5))')'sales_shifted',curcalyr,icafe19,iflt,iregn,ivmt,sales_shifted,ghg_npv(:),ICE_shr(:)
            
          enddo !ivmt
        enddo   !iflt
      enddo     !icafe19
    enddo       !iregn

!	Populate fuel share array with updated fuel shares post-GHGZEV
!	And fill national with updated regional sales 
    DO iregn = 1,MNUMCR-2
      DO iflt=1,flt
        DO icafe19 = 1,CAFE19
		  isc4 = SC19Map(icafe19)
	      DO ifuel = 1,FUEL12
		      fuel_shr_regn(iyr,icafe19,ifuel,iflt,iregn) = 0.0
			  IF (sum(TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn)).gt.0.0) then 
	  	        fuel_shr_regn(iyr,icafe19,ifuel,iflt,iregn) = sum(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))
              ENDIF
              TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) = fuel_shr_regn(iyr,icafe19,ifuel,iflt,iregn) * sum(TRKSTK_19R(iyr,icafe19,1,:,iflt,iregn))
			  IF (iregn.eq.MNUMCR-2) TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,MNUMCR)  = sum(TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,1:MNUMCR-2))
		    ENDDO
	    ENDDO
	  ENDDO
	ENDDO

    if(fcrl.eq.1.and.ghgzev_debug) WRITE(21,'(a,8(",",f12.1))')'GHG_creds_agg',GHG_creds_agg(iyr,:),cred_gen(:,iyr)

!   Final check to ensure regulation has been met
    DO icafe4 = 1, cafe4
      IF (.not.HDV_passGHG(iyr,icafe4)) THEN
        GHG_creds_agg(iyr,icafe4) = GHG_creds_agg(iyr,icafe4) + cred_gen(icafe4,iyr)
        IF (GHG_creds_agg(iyr,icafe4).ge.-1000.0) THEN
          HDV_passGHG(iyr,icafe4) = .TRUE.
          if(fcrl.eq.1.and.ghgzev_debug) WRITE(21,'(a16,3(",",i4),2(",",f12.1))')'passedGHGZEV',curcalyr,itryghg,icafe4,GHG_creds_agg(iyr,icafe4)
        ENDIF
      ENDIF
    ENDDO

 RETURN
 END SUBROUTINE TRUCK_GHGZEV
 
! ==========================================================================================================
!...Subroutine TRUCK_INCCOST
!   Description:
!   	Estimates incremental cost for alternative fuel powertrains
! ==========================================================================================================
   SUBROUTINE TRUCK_INCCOST
   USE F_
   IMPLICIT NONE
   include 'convfact'

   INTEGER i, ibatt
   REAL PHEV_daily_Evmt
   REAL kWh_used(NVMT,CAFE19,MNUMYR), &             ! Parameters for endogenous battery sizing
        kWh_needed(NVMT,CAFE19,MNUMYR), &
        kWh_usable(NVMT,CAFE19,MNUMYR), &
        daily_recharges(NVMT,CAFE19,MNUMYR), &
        daily_hours(NVMT,CAFE19,MNUMYR)

!...  Accumulate cost of technology adopted to meet NHTSA/EPA standards, incl. tech adopted due to cost effectiveness.
!	  The total here -- cost_ICE -- is subtracted off the alt-fuel powertrain incremental cost.
!     Note that cost_ICE values are read in from trnhdvx.xlsx, and are based on a BSYR_STK (2023 in AEO2025) vehicle. Therefore all tech adopted before
!     that year -- and the associated cost -- is baked in.
	  
      if(curcalyr.ne.BSYR_STK) cost_ICE(:,iyr) = 0.0
      
      DO icafe19 = 1, CAFE19
	    IF (sum(TRKSTK_19R(iyr-1,icafe19,1,1:2,:,MNUMCR)).eq.0.0) THEN
          cost_ICE(icafe19,iyr) = cost_ICE(icafe19,iyr-1)
        ELSE
          IF (icafe19.le.7) then 		    ! Class 2b-6, composite gasoline/diesel (sales-weighted)
	        cost_icetech(icafe19,iyr)       = (inc_tech_cost_19(iyr,icafe19,1)*sum(TRKSTK_19R(iyr-1,icafe19,1,1,:,MNUMCR)) &
                                            +  inc_tech_cost_19(iyr,icafe19,2)*sum(TRKSTK_19R(iyr-1,icafe19,1,2,:,MNUMCR))) &
                                                / sum(TRKSTK_19R(iyr-1,icafe19,1,1:2,:,MNUMCR))
!	        cost_ICE(icafe19,iyr) 	        = cost_ICE(icafe19,2016-1989) + cost_icetech(icafe19,iyr)
            if(curcalyr.gt.BSYR_STK) &
              cost_ICE(icafe19,iyr) 	    = cost_ICE(icafe19,BSYR_STK-1989) + (cost_icetech(icafe19,iyr) - cost_icetech(icafe19,BSYR_STK-1989))

	      ELSEIF (icafe19.gt.7) then	    ! Class 7&8, diesel only
	        cost_icetech(icafe19,iyr)	    = inc_tech_cost_19(iyr,icafe19,1)
!	        cost_ICE(icafe19,iyr) 	        = cost_ICE(icafe19,2016-1989) + cost_icetech(icafe19,iyr)
            if(curcalyr.gt.BSYR_STK) &
              cost_ICE(icafe19,iyr) 	    = cost_ICE(icafe19,BSYR_STK-1989) + (cost_icetech(icafe19,iyr) - cost_icetech(icafe19,BSYR_STK-1989))
	      ENDIF

!         HEV, PHEVG, PHEVD, and H2ICE incremental costs are based on EITHER diesel OR gasoline, so no averaging like above          
          IF (curcalyr.gt.BSYR_STK) THEN
            cost_ICE_HEV(icafe19,iyr)     = cost_ICE_HEV(icafe19,BSYR_STK-1989) + (inc_tech_cost_19(iyr,icafe19,2) - inc_tech_cost_19(BSYR_STK-1989,icafe19,2))
            cost_ICE_PHEV(icafe19,1,iyr)  = cost_ICE_PHEV(icafe19,1,BSYR_STK-1989) + (inc_tech_cost_19(iyr,icafe19,1) - inc_tech_cost_19(BSYR_STK-1989,icafe19,1))
            cost_ICE_PHEV(icafe19,2,iyr)  = cost_ICE_PHEV(icafe19,2,BSYR_STK-1989) + (inc_tech_cost_19(iyr,icafe19,2) - inc_tech_cost_19(BSYR_STK-1989,icafe19,2))
            cost_ICE_H2ICE(icafe19,iyr)   = cost_ICE_H2ICE(icafe19,BSYR_STK-1989) + (inc_tech_cost_19(iyr,icafe19,1) - inc_tech_cost_19(BSYR_STK-1989,icafe19,1))
          ENDIF 
          
        ENDIF
        
!	    if(itryghg.eq.1) WRITE(21,'(a,2(",",i4),5(",",f10.0))')'cost_ICE',curitr,curcalyr,cost_ICE(icafe19,iyr),sum(TRKSTK_19R(iyr-1,icafe19,1,1,:,MNUMCR)),&
!                                              inc_tech_cost_19(iyr,icafe19,1),sum(TRKSTK_19R(iyr-1,icafe19,1,2,:,MNUMCR)),inc_tech_cost_19(iyr,icafe19,2)
	  ENDDO

!...  Low NOx rule compliance cost, starting in MY2027
	  lonox_cost(:,iyr) = 0.0
	  IF (iyr.ge.2027-1989.and.NOX_switch.eq.1) then
	    DO icafe19 = 1, CAFE19
!		  IF no ds/mg sales, set equal to previous year 
		  IF (sum(TRKSTK_19R(iyr-1,icafe19,1,1:2,:,MNUMCR)).eq.0.0) then
		    lonox_cost(icafe19,iyr) = lonox_cost(icafe19,iyr-1)
	      ELSE
  		    IF (icafe19.le.7) then 	! Class 2b-6, composite gasoline/diesel
  	          lonox_cost(icafe19,iyr) = (cost_LoNOx(icafe19,1)*sum(TRKSTK_19R(iyr-1,icafe19,1,1,:,MNUMCR)) &
  								         +cost_LoNOx(icafe19,2)*sum(TRKSTK_19R(iyr-1,icafe19,1,2,:,MNUMCR))) &
  								      / sum(TRKSTK_19R(iyr-1,icafe19,1,1:2,:,MNUMCR))
  		    ELSEIF (icafe19.gt.7) then	! Class 7&8, diesel only
  		      lonox_cost(icafe19,iyr) = cost_LoNOx(icafe19,1)
  		    ENDIF
		  ENDIF
	    ENDDO
	  ENDIF

!...  CNG
	  INC_COST_CNG(:,:,n) = 0.0
	  DO ivmt=1,nvmt
	    DO icafe19=1, CAFE19
	      isc4 = SC19Map(icafe19)
	  	  INC_COST_CNG(ivmt,icafe19,n)  = cost_NGtank_DGE(isc4,iyr)*PT_tanksize_CNG(ivmt,icafe19) &
	  								    + (inc_tech_cost_19(iyr,icafe19,4) - inc_tech_cost_19(BSYR_STK-1989,icafe19,4)) &   ! adopted tech improvement (v. cost base year)
	  								    - cost_ice_CNG(icafe19) &                                                           ! Incremental CNG cost (non-tank, non tech adopt)
                                        - cost_icetech(icafe19,iyr)

!         Subtract delta of diesel/gasoline avg. LowNOx compliance cost [lonox_cost] and CNG LowNOx compliance cost [cost_LoNOx]
	  	  IF (iyr.ge.2027-1989.and.NOX_switch.eq.1) &
            INC_COST_CNG(ivmt,icafe19,n) = INC_COST_CNG(ivmt,icafe19,n) - (lonox_cost(icafe19,iyr) - cost_LoNOx(icafe19,4))
          
          INC_COST_CNG(ivmt,icafe19,n)  = INC_COST_CNG(ivmt,icafe19,n) &						                            ! Add excise tax (Class 8)
	  								    * (1+fet_rate(icafe19))
	    ENDDO
	  ENDDO

!...  Battery cost (applied to all BEV, PHEV-D, PHEV-G, FCEV, and FCHEV)
!     HEV battery prices (index 4) are also estimated here.
!     Battery price per kWh. Initial costs are in 2020 dollars, converted to 1990 dollars here
!     Battery Pack Cost learning rate equation:
!     Cost = a_battery*(cumulative production)^(-b_battery) + a_material*(cumulative production)^(-b_material)
!     where b = -Log(1.0-(learning rate))/Log(2.0)
!	  Different coefficients for Class 2b/3 (closer to LDV pricing) and Class 4-8 (more expensive)
	  DO ibatt = 1, 5
	    IF(curcalyr.gt.BSYR_STK) cost_battkWh_trk(ibatt,iyr) = (cost_battpack_a(ibatt)*cumulative_gwh(iyr-1)**(-cost_battpack_b(ibatt)) + &
														        cost_battmat_a(ibatt)*cumulative_gwh(iyr-1)**(-cost_battmat_b(ibatt))) / &
														       mc_jpgdp(31) * mc_jpgdp(1)
	  ENDDO

!...  Motor cost (applied to all BEV, FCEV, and FCHEV)
!     Weighted average of motor cost data from 2020 to 2025
	  IF (curcalyr.le.2025) then
	    cost_motorkW(iyr) = (-2.2*(curcalyr) + 4483)/ mc_jpgdp(31) * mc_jpgdp(1)
!     EPA: 3% from 2025 - 2030, 2% from 2030 - 2035, 1% from 2035 - 2050
	  ELSEIF (curcalyr.gt.2025.and.curcalyr.le.2030) then
	    cost_motorkW(iyr) = cost_motorkW(iyr-1)*.97
	  ELSEIF (curcalyr.gt.2030.and.curcalyr.le.2035) then
	    cost_motorkW(iyr) = cost_motorkW(iyr-1)*.98
	  ELSEIF (curcalyr.gt.2035) then
	    cost_motorkW(iyr) = cost_motorkW(iyr-1)*.99
	  ENDIF

!	  Incremental BEV, HEV, PHEV, FCEV, and FCHEV costs (only needed in BSYR_STK:MNUMYR)
	  IF(curcalyr.gt.BSYR_STK)then
!       FC Stack -- Constant YoY R&D cost reduction - switches to production-based when that falls below R&D curve
	    cost_FCkW(iyr) = MIN((cost_fcstack_a *(cumulative_fc_stacks(iyr-1) ** (-cost_fcstack_b)))/ mc_jpgdp(31) * mc_jpgdp(1), &
	  					      cost_FCkW(33)*((1-cost_fcstk_rd)**(iyr-33)))
!       H2 Tank -- Constant YoY R&D cost reduction - switches to production-based when that falls below R&D curve
	    cost_h2tank(iyr) = MIN((cost_h2tank_a *(cumulative_h2_tanks(iyr-1) ** (-cost_h2tank_b)))/ mc_jpgdp(31) * mc_jpgdp(1), &
	  						    cost_h2tank(33)*((1-cost_h2tank_rd)**(iyr-33)))

!...    BEV
	    INC_COST_BEV(:,:,n) = 0.0
        DO ivmt=1,nvmt
          DO icafe19=1,CAFE19
            isc4 = SC19Map(icafe19)
		    IF (icafe19.le.4) then                          ! Class 2b/3
              ibatt = 4
            ELSEIF (icafe19.le.6) then						! Class 4-5
		      ibatt = 1
		    ELSEIF (icafe19.le.8.or.icafe19.eq.12) then		! Class 6-8 vocational
		      ibatt = 2
		    ELSE											! Class 7&8 tractor
		      ibatt = 3
		    ENDIF
            
!           Size batteries                            average daily range                      / fuel economy (mpgde or mpgde)          / convert to kWh
            kWh_used(ivmt,icafe19,iyr)          = (SUM(VMT_VEH(ivmt,:,icafe19))/2) / 250       / new_mpg_19(iyr-1,6,icafe19)            / 3412  * (HRATE(isc4,6))           ! kWh consumed to meet average daily range
            kWh_needed(ivmt,icafe19,iyr)        = (MAX(100.0,(SUM(VMT_VEH(ivmt,:,icafe19))/2) / 250))  / new_mpg_19(iyr-1,6,icafe19)    / 3412  * (HRATE(isc4,6))           ! kWh consumed to meet average daily range (OR 100 miles, whichever is more)
            kWh_nominal(ivmt,icafe19,iyr)       = MIN(max_kWh_cap(icafe19,iyr),kWh_needed(ivmt,icafe19,iyr) / TRK_BEV_DOD)                                                  ! kWh installed on the vehicle
            kWh_usable(ivmt,icafe19,iyr)        = kWh_nominal(ivmt,icafe19,iyr) * TRK_BEV_DOD                                                                               ! kWh available for use
            daily_recharges(ivmt,icafe19,iyr)   = kWh_used(ivmt,icafe19,iyr) / kWh_usable(ivmt,icafe19,iyr)                                                                 ! daily recharging sessions required to achieve average daily range
            daily_hours(ivmt,icafe19,iyr)       = MAX(0.0,(daily_recharges(ivmt,icafe19,iyr)-1)) * kWh_used(ivmt,icafe19,iyr) / charging_speed(icafe19)                     ! hours spent recharging, daily (not including nightly charge)
            bev_refuelopcost(ivmt,icafe19,iyr)  = daily_hours(ivmt,icafe19,iyr) * refuel_timeval * 250                                                                      ! annual labor/time cost due to recharging stops
            
!           Calculate Incremental BEV cost increase over conventional diesel
	  	    INC_COST_BEV(ivmt,icafe19,n)  = PT_emotorkW(icafe19,6)*cost_motorkW(iyr)&					                    ! motor
	  								      + cost_battkWh_trk(ibatt,iyr)*kWh_nominal(ivmt,icafe19,iyr)&	                    ! battery
	  									  + (inc_tech_cost_19(iyr,icafe19,6) - inc_tech_cost_19(BSYR_STK-1989,icafe19,6)) & ! adopted tech improvement (v. cost base year)
										  + BEV_infra_cost(icafe19,iyr) &									                ! EVSE equipment + installation
	  									  + cost_OBC(iyr) &													                ! On-board charger cost
										  - cost_ICE(icafe19,iyr) &											                ! ICE components removed (incl. tech adopted for ICE truck)
										  - lonox_cost(icafe19,iyr)											                ! Low NOx compliance cost starting in 2027 (lonox_cost(:,1995:2027) = 0)
		    INC_COST_BEV(ivmt,icafe19,n)  = INC_COST_BEV(ivmt,icafe19,n) &									                ! Add excise tax (Class 8)
										  + (INC_COST_BEV(ivmt,icafe19,n)-BEV_infra_cost(icafe19,iyr)) &
										    * fet_rate(icafe19)

!	  	    IRA commercial clean vehicle credit (Section 45W)
!	  	    Class 2b-3 (up to  14k GVWR): Minimum of $7,500, 30% of vehicle cost, and incremental cost
!	  	    Class 4-8: Minimum of $40,000, 30% of vehicle cost, and incremental cost
!		    Note that the credit ceiling (full incremental cost) is VEHICLE ONLY -- so the infrastructure cost is subtracted off.
	  	    IF (curcalyr.ge.2023.and.curcalyr.le.2032) then
	  	      INC_COST_BEV(ivmt,icafe19,n) = INC_COST_BEV(ivmt,icafe19,n) - MAX(0.0,MIN(IRA_45W_max(icafe19,6) / mc_jpgdp(n) * mc_jpgdp(1),INC_COST_BEV(ivmt,icafe19,n)-BEV_infra_cost(icafe19,iyr)))
	  	    ENDIF

!		  IF (ivmt.eq.1.and.icafe19.eq.1.and.curcalyr.eq.2017) WRITE(21,'(a19,4(",",a4),10(",",a8))')'INC_COST_BEV_detail','itr','year','icaf','ivmt', &
!																	 'inc_cost','motorkW','motorkW$','battkWh','battkWh$','tech$','ice$','nox$','fet%','state%'
!		  WRITE(21,'(a19,4(",",i4),",",f8.0,7(",",f8.1),2(",",f8.5))')'INC_COST_BEV_detail',curitr,curcalyr,icafe19,ivmt,INC_COST_BEV(ivmt,icafe19,n)/ mc_jpgdp(1) * mc_jpgdp(33),PT_emotorkW(icafe19,6),cost_motorkW(iyr)/ mc_jpgdp(1) * mc_jpgdp(33),PT_battkWh_BEV(ivmt,icafe19,iyr),&
!																cost_battkWh_trk(ibatt,iyr)/ mc_jpgdp(1) * mc_jpgdp(33),inc_tech_cost_19(iyr,icafe19,6)/ mc_jpgdp(1) * mc_jpgdp(33),cost_ICE(icafe19,iyr)/ mc_jpgdp(1) * mc_jpgdp(33),&
!																lonox_cost(icafe19,iyr)/ mc_jpgdp(1) * mc_jpgdp(33),fet_rate(icafe19),sum(sales_tax_rate(1:MNUMCR-2))/(MNUMCR-2)
          ENDDO
        ENDDO
        
!        IF (iyr.eq.mnumyr) then
!          WRITE(21,*) 'battsizecheck',curitr
!          DO i = BSYR_STK-1989+1,MNUMYR
!            DO icafe19 = 1, cafe19
!              DO ivmt = 1, nvmt
!                WRITE(21,'(3(i4,","),4(f8.1,","),2(f5.1,","),f10.1)') i+1989,icafe19,ivmt,kWh_used(ivmt,icafe19,i),kWh_needed(ivmt,icafe19,i),kWh_nominal(ivmt,icafe19,i),kWh_usable(ivmt,icafe19,i),daily_recharges(ivmt,icafe19,i),daily_hours(ivmt,icafe19,i),bev_refuelopcost(ivmt,icafe19,i)
!              ENDDO
!            ENDDO
!          ENDDO
!        ENDIF

!...    Gasoline HEV
	    INC_COST_HEV(:,:,n) = 0.0
        ibatt = 5                       ! HEV battery price
        DO icafe19=1,CAFE19
!         Calculate Incremental HEV cost increase over conventional diesel
	  	  INC_COST_HEV(:,icafe19,n)  = PT_emotorkW(icafe19,11)*cost_motorkW(iyr)&			                            ! motor
	  								 + cost_battkWh_trk(ibatt,iyr)*PT_battkWh_HEV(icafe19)&			                    ! battery (total capacity, not usable)
	  								 + (inc_tech_cost_19(iyr,icafe19,11) - inc_tech_cost_19(BSYR_STK-1989,icafe19,11)) &! adopted tech improvement (v. cost base year)
									 - cost_ICE_HEV(icafe19,iyr) &									                    ! ICE components removed (incl. tech adopted for ICE truck)
									 - lonox_cost(icafe19,iyr)									                        ! Low NOx compliance cost starting in 2027 (lonox_cost(:,1995:2027) = 0)

!		IF (ivmt.eq.1.and.icafe19.eq.1.and.curcalyr.eq.BSYR_STK+1) WRITE(21,'(a19,3(",",a4),9(",",a9))')'INC_COST_HEV_detail','itr','year','icaf', &
!																	 'inc_cost','motorkW','motorkW$','battkWh','battkWh$','tech$','ice$','nox$'
!		IF (ivmt.eq.1) WRITE(21,'(a19,3(",",i4),8(",",f9.1))')'INC_COST_HEV_detail',curitr,curcalyr,icafe19,INC_COST_HEV(ivmt,icafe19,n)/ mc_jpgdp(1) * mc_jpgdp(33),PT_motorkW_HEV(icafe19),cost_motorkW(iyr)/ mc_jpgdp(1) * mc_jpgdp(33),PT_battkWh_HEV(icafe19),&
!																cost_battkWh_trk(4,iyr)/ mc_jpgdp(1) * mc_jpgdp(33),inc_tech_cost_19(iyr,icafe19,11)/ mc_jpgdp(1) * mc_jpgdp(33),cost_ICE_HEV(icafe19)/ mc_jpgdp(1) * mc_jpgdp(33),&
!																lonox_cost(icafe19,iyr)/ mc_jpgdp(1) * mc_jpgdp(33)
        ENDDO

!...    Hydrogen ICE
	    INC_COST_H2ICE(:,:,n) = 0.0
        DO icafe19=1,CAFE19
          isc4 = SC19Map(icafe19)
          DO ivmt = 1, nvmt
!           Size H2 tanks                      average daily range                    / fuel economy (mpgde or mpgde)  * convert to kgH2
            PT_tankkg_h2ice(ivmt,icafe19)  = ((SUM(VMT_VEH(ivmt,:,icafe19))/2) / 250) / new_mpg_19(iyr-1,12,icafe19)   * CFH2Q_KG  * (HRATE(isc4,12)*1E-6)
            PT_tankkg_h2ice(ivmt,icafe19)  = PT_tankkg_h2ice(ivmt,icafe19) / usable_h2

!           Calculate number of tanks (used in cost learning)
            PT_tankcnt_H2ICE(ivmt,icafe19) = MIN(h2_max_tanks(icafe19,3),REAL(CEILING(PT_tankkg_h2ice(ivmt,icafe19) / h2_kg_per_tank(icafe19,3))))

!           Calculate cost for extra required refuelings (if not enough space to install tanks sufficient to achieve daily range, have to stop and refuel)
!           First, ratio of kgH2 capacity needed to meet average daily range to actual on-board kgH2 capacity (>1 means need to refuel to achieve duty cycle)
            h2_refuelopcost(ivmt,icafe19,3) = PT_tankkg_H2ICE(ivmt,icafe19) / (PT_tankcnt_H2ICE(ivmt,icafe19) * h2_kg_per_tank(icafe19,3))
!           Second, scale up to annual, apply refueling time and cost per hour
            h2_refuelopcost(ivmt,icafe19,3) = MAX(0.0, (h2_refuelopcost(ivmt,icafe19,3) - 1) * 250 * h2_refuel_time * refuel_timeval)

!           Calculate Incremental HEV cost increase over conventional diesel
	  	    INC_COST_H2ICE(ivmt,icafe19,n)  = cost_h2tank(iyr)*PT_tankcnt_H2ICE(ivmt,icafe19)*h2_kg_per_tank(icafe19,3) &       ! h2 tank
                                            + (inc_tech_cost_19(iyr,icafe19,12) - inc_tech_cost_19(BSYR_STK-1989,icafe19,12)) & ! adopted tech improvement (v. cost base year)
                                            - cost_ICE_H2ICE(icafe19,iyr)								                        ! ICE components removed (incl. tech adopted for diesel ICE truck)
            INC_COST_H2ICE(ivmt,icafe19,n)  = INC_COST_H2ICE(ivmt,icafe19,n) &
                                            * (1+fet_rate(icafe19))

!           Add Low NOx compliance cost (assume equal to gasoline compliance cost)
		    IF (n.ge.2027-1989.and.NOX_switch.eq.1) &
			  INC_COST_H2ICE(ivmt,icafe19,n) = INC_COST_H2ICE(ivmt,icafe19,n) + cost_LoNOx(icafe19,12)

!		    IF (ivmt.eq.1.and.icafe19.eq.1.and.curcalyr.eq.BSYR_STK+1) WRITE(21,'(a19,3(",",a4),3(",",a9))')'INC_COST_H2ICE_detail','itr','year','icaf', &
!		  															 'inc_cost','tech$','ice$'
!		    IF (ivmt.eq.1) WRITE(21,'(a19,3(",",i4),3(",",f9.1))')'INC_COST_H2ICE_detail',curitr,curcalyr,icafe19,INC_COST_H2ICE(ivmt,icafe19,n)/ mc_jpgdp(1) * mc_jpgdp(33),
!                                                                 inc_tech_cost_19(iyr,icafe19,12)/ mc_jpgdp(1) * mc_jpgdp(33),cost_ICE_H2ICE(icafe19)/ mc_jpgdp(1) * mc_jpgdp(33)
!            WRITE(21,'(a6,4(",",i4),10(",",f7.2),",",f8.1)') 'H2ICE',curcalyr,curitr,icafe19,ivmt,(SUM(VMT_VEH(ivmt,:,icafe19))/2) / 250,new_mpg_19(iyr-1,12,icafe19),CFH2Q_KG,&
!                                                            (HRATE(isc4,12)*1E-6),h2_kg_per_tank(icafe19,3),PT_tankcnt_H2ICE(ivmt,icafe19),PT_tankkg_h2ice(ivmt,icafe19),&
!                                                            h2_max_tanks(icafe19,3),h2_refuel_time,refuel_timeval/ mc_jpgdp(1)*mc_jpgdp(33),h2_refuelopcost(ivmt,icafe19,3)/ mc_jpgdp(1)*mc_jpgdp(33)

          ENDDO
        ENDDO

!...    PHEV diesel and gasoline
	    INC_COST_PHEVD(:,:,n) = 0.0
	    INC_COST_PHEVG(:,:,n) = 0.0
        DO ivmt=1,nvmt
          DO icafe19=1,CAFE19
		    PHEV_daily_Evmt = ((SUM(VMT_VEH(ivmt,:,icafe19))/2) / 250 * PHEVElecVMT(icafe19))		  
		    ibatt = 2           ! Assume high-power battery

		    PT_battkWh_PHEV(ivmt,icafe19) = battsize_a_PHEV(icafe19) * (PHEV_daily_Evmt** battsize_b_PHEV(icafe19))
		    PT_battkWh_PHEV(ivmt,icafe19) = PT_battkWh_PHEV(ivmt,icafe19) * new_mpg_19(BSYR_STK-1989,6,icafe19)/new_mpg_19(iyr,6,icafe19)   !  Adjust battery size down as charge depleting fuel economy improves
            
!           Calculate Incremental BEV cost increase over conventional diesel
	  	    INC_COST_PHEVD(ivmt,icafe19,n) = PT_emotorkW(icafe19,7)*cost_motorkW(iyr)&			                                ! motor
	  								       + cost_battkWh_trk(ibatt,iyr)*PT_battkWh_PHEV(ivmt,icafe19)&	                        ! battery (total capacity, not usable)
	  									   + (inc_tech_cost_19(iyr,icafe19,7) - inc_tech_cost_19(BSYR_STK-1989,icafe19,7)) &	! adopted tech improvement
	  									   + cost_OBC(iyr) &											                        ! On-board charger cost
										   - cost_ICE_PHEV(icafe19,1,iyr)	                                                    ! ICE components removed/PHEV added (incl. removing tech adopted for diesel truck)
	  	    INC_COST_PHEVG(ivmt,icafe19,n) = PT_emotorkW(icafe19,8)*cost_motorkW(iyr)&			                                ! motor
	  								       + cost_battkWh_trk(ibatt,iyr)*PT_battkWh_PHEV(ivmt,icafe19)&	                        ! battery (total capacity, not usable)
	  									   + (inc_tech_cost_19(iyr,icafe19,8) - inc_tech_cost_19(BSYR_STK-1989,icafe19,8)) &	! adopted tech improvement (v. cost base year)
	  									   + cost_OBC(iyr) &											                        ! On-board charger cost
										   - cost_ICE_PHEV(icafe19,2,iyr)	                                                    ! ICE components removed/PHEV added (incl. removing tech adopted for gasoline truck)
		    INC_COST_PHEVD(ivmt,icafe19,n) = INC_COST_PHEVD(ivmt,icafe19,n) &							                        ! Add excise tax (Class 8)
										   * (1+fet_rate(icafe19))                      
		    INC_COST_PHEVG(ivmt,icafe19,n) = INC_COST_PHEVG(ivmt,icafe19,n) &							                        ! Add excise tax (Class 8)
										   * (1+fet_rate(icafe19))

!           Subtract delta of diesel/gasoline avg. LowNOx compliance cost [lonox_cost] and PHEV LowNOx compliance cost [cost_LoNOx]
		    IF (n.ge.2027-1989.and.NOX_switch.eq.1) then
			  INC_COST_PHEVD(ivmt,icafe19,n) = INC_COST_PHEVD(ivmt,icafe19,n) - (lonox_cost(icafe19,iyr) - cost_LoNOx(icafe19,7))
			  INC_COST_PHEVG(ivmt,icafe19,n) = INC_COST_PHEVG(ivmt,icafe19,n) - (lonox_cost(icafe19,iyr) - cost_LoNOx(icafe19,8))
		    ENDIF

!	  	    IRA commercial clean vehicle credit (Section 45W)
!	  	    Class 2b-3 (up to 14k GVWR): Minimum of $7,500, 15% of vehicle cost, and incremental cost
!	  	    Class 4-8: Minimum of $40,000, 15% of vehicle cost, and incremental cost
	  	    IF (curcalyr.ge.2023.and.curcalyr.le.2032) then
	  	      INC_COST_PHEVD(ivmt,icafe19,n) = INC_COST_PHEVD(ivmt,icafe19,n) - MAX(0.0,MIN(IRA_45W_max(icafe19,7) / mc_jpgdp(n) * mc_jpgdp(1),INC_COST_PHEVD(ivmt,icafe19,n)))
	  	      INC_COST_PHEVG(ivmt,icafe19,n) = INC_COST_PHEVG(ivmt,icafe19,n) - MAX(0.0,MIN(IRA_45W_max(icafe19,8) / mc_jpgdp(n) * mc_jpgdp(1),INC_COST_PHEVG(ivmt,icafe19,n)))
	  	    ENDIF

          ENDDO
        ENDDO

!...    FCEV and FCHEV
	    INC_COST_FCEV(:,:,n) = 0.0
	    INC_COST_FCHEV(:,:,n) = 0.0
	    DO ivmt=1,nvmt
	      DO icafe19=1,CAFE19
            isc4 = SC19Map(icafe19)
!           Size H2 tanks                      average daily range                   / fuel economy (mpgde or mpgde)   *  convert to kgH2
            PT_tankkg_FCEV(ivmt,icafe19)  = ((SUM(VMT_VEH(ivmt,:,icafe19))/2) / 250) / new_mpg_19(iyr-1,9,icafe19)     * CFH2Q_KG * (HRATE(isc4,9)*1E-6)
            PT_tankkg_FCHEV(ivmt,icafe19) = ((SUM(VMT_VEH(ivmt,:,icafe19))/2) / 250) / new_mpg_19(iyr-1,10,icafe19)    * CFH2Q_KG * (HRATE(isc4,10)*1E-6)
            PT_tankkg_FCEV(ivmt,icafe19)  = PT_tankkg_FCEV(ivmt,icafe19) / usable_h2
            PT_tankkg_FCHEV(ivmt,icafe19) = PT_tankkg_FCHEV(ivmt,icafe19) / usable_h2

!           Calculate number of tanks required (used in cost learning)
            PT_tankcnt_FCEV(ivmt,icafe19) = MIN(h2_max_tanks(icafe19,1),REAL(CEILING(PT_tankkg_FCEV(ivmt,icafe19) / h2_kg_per_tank(icafe19,1))))
            PT_tankcnt_FCHEV(ivmt,icafe19)= MIN(h2_max_tanks(icafe19,2),REAL(CEILING(PT_tankkg_FCHEV(ivmt,icafe19) / h2_kg_per_tank(icafe19,2))))

!           Calculate opportunity cost for extra required refuelings (if not enough space to install tanks sufficient to achieve daily range, have to stop and refuel)
!           First, ratio of kgH2 capacity needed to meet average daily range to actual on-board kgH2 capacity (>1 means need to refuel to achieve duty cycle)
            h2_refuelopcost(ivmt,icafe19,1) = PT_tankkg_FCEV(ivmt,icafe19) / (PT_tankcnt_FCEV(ivmt,icafe19) * h2_kg_per_tank(icafe19,1))
            h2_refuelopcost(ivmt,icafe19,2) = PT_tankkg_FCHEV(ivmt,icafe19) /(PT_tankcnt_FCHEV(ivmt,icafe19)* h2_kg_per_tank(icafe19,2))
!           Second, scale up to annual, apply refueling time and cost per hour             
            h2_refuelopcost(ivmt,icafe19,1) = MAX(0.0, (h2_refuelopcost(ivmt,icafe19,1) - 1) * 250 * h2_refuel_time * refuel_timeval)
            h2_refuelopcost(ivmt,icafe19,2) = MAX(0.0, (h2_refuelopcost(ivmt,icafe19,2) - 2) * 250 * h2_refuel_time * refuel_timeval)

		    ibatt = 2			! Assume high-power battery

	        INC_COST_FCEV(ivmt,icafe19,n)  = cost_FCkW(iyr)*PT_fckW_FCEV(ivmt,icafe19) &				                        ! fuel cell stack
	  									   + cost_motorkW(iyr)*PT_emotorkW(icafe19,9) &		                                    ! motor
	  									   + cost_h2tank(iyr)*PT_tankcnt_FCEV(ivmt,icafe19)*h2_kg_per_tank(icafe19,1) &	        ! h2 tank
	  									   + cost_battkWh_trk(ibatt,iyr)*PT_battkWh_FCEV(ivmt,icafe19) &                        ! battery
	  									   + (inc_tech_cost_19(iyr,icafe19,9) - inc_tech_cost_19(BSYR_STK-1989,icafe19,9)) &    ! adopted tech improvement (v. cost base year)
	  									   - cost_ICE(icafe19,iyr) &									                        ! ICE components removed (incl. tech adopted for ICE truck)
										   - lonox_cost(icafe19,iyr)									                        ! Low NOx compliance cost starting in 2027 (lonox_cost(:,1995:2027) = 0)
	        INC_COST_FCHEV(ivmt,icafe19,n) = cost_FCkW(iyr)*PT_fckW_FCHEV(ivmt,icafe19) &				                        ! fuel cell stack
	  									   + cost_motorkW(iyr)*PT_emotorkW(icafe19,10) &		                                ! motor
	  									   + cost_h2tank(iyr)*PT_tankcnt_FCHEV(ivmt,icafe19)*h2_kg_per_tank(icafe19,2) &        ! h2 tank
	  									   + cost_battkWh_trk(ibatt,iyr)*PT_battkWh_FCHEV(ivmt,icafe19)&                        ! battery
	  									   + (inc_tech_cost_19(iyr,icafe19,10) - inc_tech_cost_19(BSYR_STK-1989,icafe19,10)) &	! adopted tech improvement (v. cost base year)
	  									   - cost_ICE(icafe19,iyr) &									                        ! ICE components removed (incl. tech adopted for ICE truck)
										   - lonox_cost(icafe19,iyr)									                        ! Low NOx compliance cost starting in 2027 (lonox_cost(:,1995:2027) = 0)
		    INC_COST_FCEV(ivmt,icafe19,n)  = INC_COST_FCEV(ivmt,icafe19,n) &							                        ! Add excise tax (Class 8)
										   * (1+fet_rate(icafe19))                      
		    INC_COST_FCHEV(ivmt,icafe19,n) = INC_COST_FCHEV(ivmt,icafe19,n) &							                        ! Add excise tax (Class 8)
										   * (1+fet_rate(icafe19))

!	  	    IRA commercial clean vehicle credit (Section 45W). The higher 30% is used (would be 15% for PHEV)
!	  	    Class 2b-3 (up to  14k GVWR): Minimum of $7,500, 30% of vehicle cost, and incremental cost
!	  	    Class 4-8: Minimum of $40,000, 30% of vehicle cost, and incremental cost
	  	    IF (curcalyr.ge.2023.and.curcalyr.le.2032) then
	  	      INC_COST_FCEV(ivmt,icafe19,n) 	= INC_COST_FCEV(ivmt,icafe19,n)  - MAX(0.0,MIN(IRA_45W_max(icafe19,9)/ mc_jpgdp(n) * mc_jpgdp(1),INC_COST_FCEV(ivmt,icafe19,n)))
	  	      INC_COST_FCHEV(ivmt,icafe19,n) 	= INC_COST_FCHEV(ivmt,icafe19,n) - MAX(0.0,MIN(IRA_45W_max(icafe19,10)/ mc_jpgdp(n) * mc_jpgdp(1),INC_COST_FCHEV(ivmt,icafe19,n)))
	  	    ENDIF

!		  IF (ivmt.eq.1.and.icafe19.eq.1.and.curcalyr.gt.2040) WRITE(21,'(a20,4(",",a4),14(",",a8))')'INC_COST_FCEV_detail','itr','year','icaf','ivmt','inc_cost','motorkW','motorkW$','battkWh','battkWh$',&
!																				'tankkg','tankkg$','FCkW','FCkW$','tech$','ice$','nox$','tax%','state%'
!		  WRITE(21,'(a19,4(",",i4),",",f8.0,9(",",f8.1),2(",",f8.5))')'FCEV',curitr,curcalyr,icafe19,ivmt,INC_COST_FCEV(ivmt,icafe19,n),PT_motorkW_FCEV(ivmt,icafe19),cost_motorkW(iyr),PT_battkWh_FCEV(ivmt,icafe19),&
!																cost_battkWh_trk(ibatt,iyr),PT_tankkg_FCEV(ivmt,icafe19),cost_h2tank(iyr),PT_fckW_FCEV(ivmt,icafe19),cost_FCkW(iyr),inc_tech_cost_19(iyr,icafe19,9),cost_ICE(icafe19,iyr),&
!																lonox_cost(icafe19,iyr),fet_rate(icafe19),sum(sales_tax_rate(1:MNUMCR-2))/(MNUMCR-2)
!		  IF (curcalyr.gt.2040) WRITE(21,'(a20,4(",",i4),12(",",f8.1),2(",",f8.5))')'FCHEV',curitr,curcalyr,icafe19,ivmt,INC_COST_FCHEV(ivmt,icafe19,n),PT_motorkW_FCHEV(ivmt,icafe19),cost_motorkW(iyr),PT_battkWh_FCHEV(ivmt,icafe19),&
!																cost_battkWh_trk(ibatt,iyr),PT_tankkg_FCHEV(ivmt,icafe19),cost_h2tank(iyr),PT_fckW_FCHEV(ivmt,icafe19),cost_FCkW(iyr),inc_tech_cost_19(iyr,icafe19,10),cost_ICE(icafe19,iyr),&
!																lonox_cost(icafe19,iyr),fet_rate(icafe19),sum(sales_tax_rate(1:MNUMCR-2))/(MNUMCR-2)   
!            WRITE(21,'(a6,4(",",i4),10(",",f7.2),",",f8.1)')'FCEV',curcalyr,curitr,icafe19,ivmt,(SUM(VMT_VEH(ivmt,:,icafe19))/2) / 250,new_mpg_19(iyr-1,9,icafe19),CFH2Q_KG,(HRATE(isc4,9)*1E-6),h2_kg_per_tank(icafe19,1),&
!                                                        PT_tankcnt_FCEV(ivmt,icafe19),PT_tankkg_FCEV(ivmt,icafe19),h2_max_tanks(icafe19,1),h2_refuel_time,refuel_timeval/ mc_jpgdp(1)*mc_jpgdp(33),h2_refuelopcost(ivmt,icafe19,1)/ mc_jpgdp(1)*mc_jpgdp(33)
!            WRITE(21,'(a6,4(",",i4),10(",",f7.2),",",f8.1)')'FCHEV',curcalyr,curitr,icafe19,ivmt,(SUM(VMT_VEH(ivmt,:,icafe19))/2) / 250,new_mpg_19(iyr-1,10,icafe19),CFH2Q_KG,(HRATE(isc4,10)*1E-6),h2_kg_per_tank(icafe19,2),&
!                                                        PT_tankcnt_FCHEV(ivmt,icafe19),PT_tankkg_FCHEV(ivmt,icafe19),h2_max_tanks(icafe19,2),h2_refuel_time,refuel_timeval/ mc_jpgdp(1)*mc_jpgdp(33),h2_refuelopcost(ivmt,icafe19,2)/ mc_jpgdp(1)*mc_jpgdp(33)
		  ENDDO
	    ENDDO
	  ENDIF

 RETURN
 END SUBROUTINE TRUCK_INCCOST 

! ==========================================================================================================
!...Subroutine TRUCK_CHOICE
!   Description:
!   	Estimates market share of sales by powertrain based on incremental and operating costs
! ==========================================================================================================
 SUBROUTINE TRUCK_CHOICE
 USE F_
 IMPLICIT NONE
 include 'angtdm'
 INCLUDE 'EUSPRC'
   
 INTEGER i
 REAL CPM_R(MNUMYR,FUEL12,CAFE19,FLT,MNUMCR)            ! fuel cost of driving a truck (using ifuel) in $ per mile 
 REAL DCOST                                       		! cost of diesel per mile relative to other fuels
 REAL MIDYR                                       		! logistic penetration curve parameter
 REAL MPATH_regn(CAFE19,FUEL12,FLT,MNUMYR,MNUMCR)  		! baseline market penetration trend for each fuel
 REAL SLOPE                                       		! logistic penetration curve parameter
 REAL TEMP	
 REAL batt_range,phev_uf,CPM_PHEV,&		                ! All temp variables for estimating average PHEV CPM w/in each annual VMT bin
 	  CPM_R_ICE(MNUMYR,MNUMCR,flt,CAFE19),&				! Composite diesel/gasoline cost per mile
 	  MR_ICE(PBK_YR)									! M&R cost 
 REAL CPM_ICE_HEV, MR_ICE_HEV(PBK_YR)
 
  fuel_shr_regn(iyr,:,:,:,:) = 0.0
  TRKSTK_19R(iyr,:,1,:,:,:)  = 0.0
  
  DO iregn = 1,MNUMCR-2
    DO iflt=1,flt
      DO icafe19 = 1,CAFE19

!		Heavy-haul is diesel only -- no need to run choice module
        IF (icafe19.eq.19) then
		  DO ivmt = 1, nvmt
		    fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,1,iregn) 		= veh_shr(ivmt,iflt,icafe19)
		    fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,2:FUEL12,iregn) = 0.0
		  ENDDO
		  CYCLE
		ENDIF
		
	    isc4 = SC19Map(icafe19)
		
!       Cost per mile (1990USD/mi) for each powertrain
	    CPM_R(n,:,icafe19,iflt,iregn) = 0.0
        DO ifuel=1,FUEL12
          IF(ifuel.ne.4) CPM_R(n,ifuel,icafe19,iflt,iregn) = FUELPRICE_R_AVG(ifuel,iregn)/NEW_MPG_19(iyr,ifuel,icafe19)*HRATE(isc4,ifuel)*1E-6									! ifuel = 4 --> CNG
          IF(ifuel.eq.1) then
		    CPM_DEF(iyr,icafe19,iregn) = 1/NEW_MPG_19(iyr,ifuel,icafe19) * DEF_dose * cost_DEF(iyr)
			IF (NOX_switch.eq.1.and.curcalyr.ge.2027) CPM_DEF(iyr,icafe19,iregn) = 1/NEW_MPG_19(iyr,ifuel,icafe19) * DEF_dose_LoNOx * cost_DEF(iyr)						! Higher DEF consumption to meet low NOx reg
			CPM_R(iyr,ifuel,icafe19,iflt,iregn) = CPM_R(iyr,ifuel,icafe19,iflt,iregn) + CPM_DEF(iyr,icafe19,iregn)
		  ELSEIF(ifuel.eq.6) then 
		    IF(iflt.eq.1.or.icafe19.gt.16) then		! Vehicles charged over-the-road (retail/public) -- non-fleet and all sleeper cabs
			  CPM_R(n,ifuel,icafe19,iflt,iregn) = SUM(PELFNCM(iregn,iyr-4:iyr))/5.0*1.115 /NEW_MPG_19(iyr,ifuel,icafe19)*HRATE(isc4,ifuel)*1E-6	
			ELSE	! Depot-charged
			  CPM_R(n,ifuel,icafe19,iflt,iregn) = SUM(PELIBCM(iregn,iyr-4:iyr))/5.0*1.115 /NEW_MPG_19(iyr,ifuel,icafe19)*HRATE(isc4,ifuel)*1E-6
			ENDIF
			
			CPM_R(iyr,ifuel,icafe19,iflt,iregn) = (CPM_R(iyr,ifuel,icafe19,iflt,iregn) + &
												   evse_maint / NEW_MPG_19(iyr,ifuel,icafe19)  * HRATE(isc4,ifuel) / 3412) / &		! EVSE maintenance cost ($/kWh)
												  CHRG_EFF(iyr)																		! Apply charging losses
		  ENDIF
		ENDDO
	    IF(icafe19.le.7) then           ! Classes 2b - 6, CNG
          IF(iflt.eq.1) CPM_R(n,4,icafe19,iflt,iregn) = (sum(PGFTRPV(iregn,iyr-4:iyr))/5.0*1.115) * NG_priceadj(1) /NEW_MPG_19(iyr,4,icafe19)*HRATE(isc4,4)*1E-6
          IF(iflt.eq.2) CPM_R(n,4,icafe19,iflt,iregn) = (sum(PGFTRFV(iregn,iyr-4:iyr))/5.0*1.115) * NG_priceadj(1) /NEW_MPG_19(iyr,4,icafe19)*HRATE(isc4,4)*1E-6
        ELSE                            ! Classes 7 - 8, avg of CNG and LNG
          IF(iflt.eq.1) CPM_R(n,4,icafe19,iflt,iregn) = ((sum(PGLTRPV(iregn,iyr-4:iyr))/5.0* NG_priceadj(2)+sum(PGFTRPV(iregn,iyr-2:iyr))/3.0 * NG_priceadj(1))/2*1.115)  / NEW_MPG_19(iyr,4,icafe19)*HRATE(isc4,4)*1E-6
          IF(iflt.eq.2) CPM_R(n,4,icafe19,iflt,iregn) = ((sum(PGLTRFV(iregn,iyr-4:iyr))/5.0* NG_priceadj(2)+sum(PGFTRFV(iregn,iyr-2:iyr))/3.0 * NG_priceadj(1))/2*1.115)  / NEW_MPG_19(iyr,4,icafe19)*HRATE(isc4,4)*1E-6
        ENDIF

!.......Fuel choice model for new vehicle sales
!		This works in 3 distinct phases within each powertrain type (jfuel):
!		  1) Logistic curve. This is time-based, from exogenous inputs (trnhdvx.xlsx), with a fuel price component. For LPG, Flex, and PHEV, this is it (no step 2 or 3).
!		  2) Payback estimate. For BEV, CNG, PHEV, and FCV, the above curve is multiplied by a market share estimated from
!			 the payback due to net savings (fuel savings - incremental cost).
!		  3) Payback grouping. For BEV, CNG, and FCV, (1) and (2) are further multiplied by the share of vehicle purchases that
!			 will purchase a vehicle given a specific payback time (i.e., "IF BEV pays back in 1 year, X% will adopt; 2 years, Y%; 3 years, Z%, etc")
!		At the end, all shares are re-normalized (SUM(shares) = 100%)
		
		fuel_shr_ivmt(iyr,:,iflt,icafe19,:,iregn) = 0.0
        DO jfuel=FUEL12,1,-1
          IF(jfuel.ge.3) then
		    IF(curcalyr.lt.TRGSHXG(icafe19,jfuel,iflt)) cycle                     ! Cannot purchase IF the powertrain is not available

!		    Calculate fuel/def and M&R cost per mile for this size class and region, to compete against alt-fuels
!			IF no gasoline/diesel, set CPM equal to previous year and MR equal to diesel/gasoline/HEV average
			IF (sum(TRKSTK_19R(iyr-1,icafe19,1,1:2,iflt,iregn)).eq.0.0) then
			  CPM_R_ICE(iyr,iregn,iflt,icafe19) = CPM_R_ICE(iyr-1,iregn,iflt,icafe19)
			  DO i = 1, PBK_YR
			    MR_ICE(i) = SUM(MR_cost(icafe19,[1,2,11],i,iyr))/3
			  ENDDO
			ELSE
!			  Class 2b-6: Sales-weighted composite ICE (diesel + gasoline + HEV) fuel/def and M&R cost per mile
			  IF (icafe19.le.7) then
			    CPM_R_ICE(iyr,iregn,iflt,icafe19) = (CPM_R(iyr,1,icafe19,iflt,iregn)*TRKSTK_19R(iyr-1,icafe19,1,1,iflt,iregn) &
												     + CPM_R(iyr,2,icafe19,iflt,iregn)*TRKSTK_19R(iyr-1,icafe19,1,2,iflt,iregn) &
												     + CPM_R(iyr,11,icafe19,iflt,iregn)*TRKSTK_19R(iyr-1,icafe19,1,11,iflt,iregn)) &
												  / sum(TRKSTK_19R(iyr-1,icafe19,1,[1,2,11],iflt,iregn))
!		        WRITE(21,'(a,",",i4,3(",",i2),",",f5.3,2(",",f10.1))')'cpm_ice',iyr+1989,iregn,icafe19,iflt,CPM_R_ICE(iyr,iregn,iflt,icafe19),TRKSTK_19R(iyr-1,icafe19,1,1,iflt,iregn),TRKSTK_19R(iyr-1,icafe19,1,2,iflt,iregn)
			    DO i = 1, PBK_YR
				  MR_ICE(i) = (MR_cost(icafe19,1,i,iyr)*TRKSTK_19R(iyr-1,icafe19,1,1,iflt,iregn) &
					           + MR_cost(icafe19,2,i,iyr)*TRKSTK_19R(iyr-1,icafe19,1,2,iflt,iregn) &
                               + MR_cost(icafe19,11,i,iyr)*TRKSTK_19R(iyr-1,icafe19,1,11,iflt,iregn)) &
						    / sum(TRKSTK_19R(iyr-1,icafe19,1,[1,2,11],iflt,iregn))
			    ENDDO
!			    WRITE(21,'(a,",",i4,3(",",i2),7(",",f5.3))')'MR_ICE',iyr+1989,iregn,icafe19,iflt,MR_ICE(:)
!			  Class 7&8 alt-fuels are all competed against diesel
			  ELSEIF (icafe19.ge.8) then
			    CPM_R_ICE(iyr,iregn,iflt,icafe19) = CPM_R(iyr,1,icafe19,iflt,iregn)
			    MR_ICE(:) = MR_cost(icafe19,1,:,iyr)
			  ENDIF
			ENDIF
									
!           CNG
            IF(jfuel.eq.4)then
              DO ivmt=1, nvmt
                IF(veh_shr(ivmt,iflt,icafe19).gt.0.0.and.PT_tanksize_CNG(ivmt,icafe19).ne.9999.) then
				  CALL TRUCK_PBK(INC_COST_CNG(ivmt,icafe19,iyr),VMT_VEH(ivmt,iflt,icafe19),CPM_R(iyr,jfuel,icafe19,iflt,iregn), &
									CPM_R_ICE(iyr,iregn,iflt,icafe19),MR_ICE,veh_shr(ivmt,iflt,icafe19),0.0)
				    
!				  Limit pace of growth/decline to ensure future manufacturing scale-up/down is feasible.
!                 For CNG, prevent falling below last historical year's sales share
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn)   = MAX(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		   fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) - max_drop*veh_shr(ivmt,iflt,icafe19),&
                                                                           fuel_shr_ivmt(BSYR_STK-1989,ivmt,iflt,icafe19,jfuel,iregn))
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MIN(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		 fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) + max_rise*veh_shr(ivmt,iflt,icafe19))
				ENDIF
			  ENDDO
				
!           BEV
            ELSEIF(jfuel.eq.6)then
              DO ivmt=1, nvmt
                IF(veh_shr(ivmt,iflt,icafe19).gt.0.0) then
				  IF (iflt.eq.1.or.icafe19.ge.16) then		! Non-fleet OR sleeper cabs: no infrastructure installation but higher electricity costs (public charging)
					CALL TRUCK_PBK(INC_COST_BEV(ivmt,icafe19,iyr)-BEV_infra_cost(icafe19,iyr),VMT_VEH(ivmt,iflt,icafe19),&
									  CPM_R(iyr,jfuel,icafe19,iflt,iregn)*BEV_elec_markup(icafe19),CPM_R_ICE(iyr,iregn,iflt,icafe19),MR_ICE,veh_shr(ivmt,iflt,icafe19),bev_refuelopcost(ivmt,icafe19,iyr))
				  ELSE						! Fleet, depot charging, requires infrastructure install (already in INC_COST_BEV)
					CALL TRUCK_PBK(INC_COST_BEV(ivmt,icafe19,n),VMT_VEH(ivmt,iflt,icafe19),CPM_R(n,jfuel,icafe19,iflt,iregn), &
									  CPM_R_ICE(iyr,iregn,iflt,icafe19),MR_ICE,veh_shr(ivmt,iflt,icafe19),bev_refuelopcost(ivmt,icafe19,iyr))
				  ENDIF

!				  Limit pace of growth/decline to ensure future manufacturing scale-up/down is feasible
!                 For BEVs, prevent falling below last historical year's sales share
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn)   = MAX(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		   fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) - max_drop*veh_shr(ivmt,iflt,icafe19),&
                                                                           fuel_shr_ivmt(BSYR_STK-1989,ivmt,iflt,icafe19,jfuel,iregn))
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn)   = MIN(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
                                                                           fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) + max_rise*veh_shr(ivmt,iflt,icafe19))
				ENDIF
			  ENDDO

!           PHEV diesel
            ELSEIF(jfuel.eq.7)then
              DO ivmt=1, nvmt
!						   		usable battery size (kWh) in btu                   / now in gallons  * and now in miles of range*adj for phev losses * and scaled up to annual
                batt_range 	= ((PT_battkWh_PHEV(ivmt,icafe19) * TRK_PHEV_DOD * 3412.0 / HRATE(isc4,6)) * new_mpg_19(iyr,6,icafe19)*phev_mpg_adj) * 250.0
				phev_uf    	= MIN(batt_range / VMT_VEH(ivmt,iflt,icafe19),1.0)
				CPM_PHEV	= (phev_uf / (NEW_MPG_19(iyr,6,icafe19)*phev_mpg_adj * CHRG_EFF(iyr))) &				! Charge depleting (incl. add'l PHEV loss and charging loss)														! Miles traveled on electricity
				 			* FUELPRICE_R_AVG(6,iregn) * HRATE(isc4,6)*1E-6  &										! Electricity price
				 			+ ((1-phev_uf) / (NEW_MPG_19(iyr,1,icafe19)*phev_chgsus_mult(icafe19,1)*phev_mpg_adj)) &! Charge sustaining
				 			* FUELPRICE_R_AVG(1,iregn) * HRATE(isc4,1)*1E-6 										! Diesel price
                IF(veh_shr(ivmt,iflt,icafe19).gt.0.0) then
				  CALL TRUCK_PBK(INC_COST_PHEVD(ivmt,icafe19,n),VMT_VEH(ivmt,iflt,icafe19),CPM_PHEV, &
									CPM_R_ICE(iyr,iregn,iflt,icafe19),MR_ICE,veh_shr(ivmt,iflt,icafe19),0.0)
														  
!				  Limit pace of growth/decline to ensure future manufacturing scale-up/down is feasible
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MAX(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		 fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) - max_drop*veh_shr(ivmt,iflt,icafe19))
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MIN(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		 fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) + max_rise*veh_shr(ivmt,iflt,icafe19))
				ENDIF
!				WRITE(21,'(a,",",i4,4(",",i2),",",f7.0,",",f5.3,","f6.3)')'TRANFRT_check_PHEVD',n+1989,iregn,iflt,icafe19,ivmt,batt_range,phev_uf,CPM_PHEV
			  ENDDO

 !          PHEV gasoline
            ELSEIF(jfuel.eq.8)then
              DO ivmt=1, nvmt
!							   usable battery size (kWh) in btu                   / now in gallons  * and now in miles of range*adj for phev losses * and scaled up to annual
                batt_range 	= ((PT_battkWh_PHEV(ivmt,icafe19) * TRK_PHEV_DOD * 3412.0 / HRATE(isc4,6)) * new_mpg_19(iyr,6,icafe19)*phev_mpg_adj) * 250.0
			    phev_uf    	= MIN(batt_range / VMT_VEH(ivmt,iflt,icafe19),1.0)
			    CPM_PHEV	= (phev_uf / (NEW_MPG_19(iyr,6,icafe19)*phev_mpg_adj * CHRG_EFF(iyr))) &				! Charge depleting (incl. add'l PHEV loss and charging loss)														! Miles traveled on electricity
					  		* FUELPRICE_R_AVG(6,iregn) * HRATE(isc4,6)*1E-6  &										! Electricity price
					  		+ ((1-phev_uf) / (NEW_MPG_19(iyr,11,icafe19)*phev_mpg_adj)) &                           ! Charge sustaining
					  		* FUELPRICE_R_AVG(11,iregn) * HRATE(isc4,11)*1E-6 										! Gasoline price
				IF(veh_shr(ivmt,iflt,icafe19).gt.0.0) then
				  CALL TRUCK_PBK(INC_COST_PHEVG(ivmt,icafe19,n),VMT_VEH(ivmt,iflt,icafe19),CPM_PHEV, &
									CPM_R_ICE(iyr,iregn,iflt,icafe19),MR_ICE,veh_shr(ivmt,iflt,icafe19),0.0)
!				  Limit pace of growth/decline to ensure future manufacturing scale-up/down is feasible
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MAX(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		 fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) - max_drop*veh_shr(ivmt,iflt,icafe19))
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MIN(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		 fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) + max_rise*veh_shr(ivmt,iflt,icafe19))
				ENDIF
!				WRITE(21,'(a,",",i4,4(",",i2),",",f7.0,",",f5.3,","f6.3)')'TRANFRT_check_PHEVG',n+1989,iregn,iflt,icafe19,ivmt,batt_range,phev_uf,CPM_PHEV
			  ENDDO

!           FCEV
            ELSEIF(jfuel.eq.9)then
              DO ivmt=1, nvmt
                IF(veh_shr(ivmt,iflt,icafe19).gt.0.0) then
				  CALL TRUCK_PBK(INC_COST_FCEV(ivmt,icafe19,n),VMT_VEH(ivmt,iflt,icafe19),CPM_R(n,jfuel,icafe19,iflt,iregn), &
										   CPM_R_ICE(iyr,iregn,iflt,icafe19),MR_ICE,veh_shr(ivmt,iflt,icafe19),h2_refuelopcost(ivmt,icafe19,1))
!				  Limit pace of growth/decline to ensure future manufacturing scale-up/down is feasible
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MAX(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		   fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) - max_drop*veh_shr(ivmt,iflt,icafe19))
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MIN(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		   fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) + max_rise*veh_shr(ivmt,iflt,icafe19))
				ENDIF
              ENDDO

!           FCHEV
            ELSEIF(jfuel.eq.10)then
              DO ivmt=1, nvmt
			    IF(veh_shr(ivmt,iflt,icafe19).gt.0.0) then
				  CALL TRUCK_PBK(INC_COST_FCHEV(ivmt,icafe19,n),VMT_VEH(ivmt,iflt,icafe19),CPM_R(n,jfuel,icafe19,iflt,iregn), &
										   CPM_R_ICE(iyr,iregn,iflt,icafe19),MR_ICE,veh_shr(ivmt,iflt,icafe19),h2_refuelopcost(ivmt,icafe19,1))
!				  Limit pace of growth/decline to ensure future manufacturing scale-up/down is feasible
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MAX(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		   fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) - max_drop*veh_shr(ivmt,iflt,icafe19))
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MIN(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		   fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) + max_rise*veh_shr(ivmt,iflt,icafe19))
				ENDIF
              ENDDO

!           Gasoline HEV
            ELSEIF(jfuel.eq.11)then
!             HEVs are competed against gasoline/diesel composite vehicle (other alt fuels competed against DS/MG/HEV composite)
              IF (sum(TRKSTK_19R(iyr-1,icafe19,1,1:2,iflt,iregn)).eq.0.0) THEN 
                CPM_ICE_HEV = SUM(CPM_R(iyr,1:2,icafe19,iflt,iregn))/2
                DO i = 1, PBK_YR
			      MR_ICE(i) = SUM(MR_cost(icafe19,[1,2],i,iyr))/2
			    ENDDO
              ELSE
                CPM_ICE_HEV = (CPM_R(iyr,1,icafe19,iflt,iregn)*TRKSTK_19R(iyr-1,icafe19,1,1,iflt,iregn) &
                               + CPM_R(iyr,2,icafe19,iflt,iregn)*TRKSTK_19R(iyr-1,icafe19,1,2,iflt,iregn)) &
						    / sum(TRKSTK_19R(iyr-1,icafe19,1,1:2,iflt,iregn))
                DO i = 1, PBK_YR 
                  MR_ICE_HEV(i)  = (MR_cost(icafe19,1,i,iyr)*TRKSTK_19R(iyr-1,icafe19,1,1,iflt,iregn) &
					                + MR_cost(icafe19,2,i,iyr)*TRKSTK_19R(iyr-1,icafe19,1,2,iflt,iregn)) &
						         / sum(TRKSTK_19R(iyr-1,icafe19,1,[1,2],iflt,iregn))
                ENDDO
              ENDIF
              DO ivmt=1, nvmt
                IF(veh_shr(ivmt,iflt,icafe19).gt.0.0) then
				  CALL TRUCK_PBK(INC_COST_HEV(ivmt,icafe19,n),VMT_VEH(ivmt,iflt,icafe19),CPM_R(n,jfuel,icafe19,iflt,iregn), &
							     CPM_ICE_HEV,MR_ICE_HEV,veh_shr(ivmt,iflt,icafe19),0.0)
				    
!				  Limit pace of growth/decline to ensure future manufacturing scale-up/down is feasible
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MAX(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		 fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) - max_drop*veh_shr(ivmt,iflt,icafe19))
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MIN(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		 fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) + max_rise*veh_shr(ivmt,iflt,icafe19))
				ENDIF
			  ENDDO

!           H2ICE
            ELSEIF(jfuel.eq.12)then
              DO ivmt=1, nvmt
                IF(veh_shr(ivmt,iflt,icafe19).gt.0.0) then
				  CALL TRUCK_PBK(INC_COST_H2ICE(ivmt,icafe19,n),VMT_VEH(ivmt,iflt,icafe19),CPM_R(n,jfuel,icafe19,iflt,iregn), &
										   CPM_R_ICE(iyr,iregn,iflt,icafe19),MR_ICE,veh_shr(ivmt,iflt,icafe19),h2_refuelopcost(ivmt,icafe19,3))
!				  Limit pace of growth/decline to ensure future manufacturing scale-up/down is feasible
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MAX(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		   fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) - max_drop*veh_shr(ivmt,iflt,icafe19))
				  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MIN(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn),&
																		   fuel_shr_ivmt(iyr-1,ivmt,iflt,icafe19,jfuel,iregn) + max_rise*veh_shr(ivmt,iflt,icafe19))
				ENDIF
              ENDDO

!           IF not CNG, BEV, PHEVG, PHEVD, FCEV, FCHEV, HEV, or H2ICE the fuel share is set by time-based S-curve (MPATH)
			ELSE
!			  DCOST is the fuel cost per unit energy RELATIVE TO gasoline/diesel. This allows the s-curve to translate up and down
!			  depending on changing fuel prices.
!			  DCOST=MAX(1-(CPM_R(iyr,jfuel,icafe19,iflt,iregn)/CPM_R_ICE(iyr,iregn,iflt,icafe19))*PRAFDFXG(isc4,jfuel),0.0)
              DCOST = 1.0
!             Time-based logistic maximum market penetration curve.
              MIDYR = TRGSHXG(icafe19,jfuel,iflt)+(1.*CYAFVXG(isc4,jfuel,iflt)/2)
			  SLOPE = LOG(0.01)/((1.*CYAFVXG(isc4,jfuel,iflt))/2)
              MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)=DCOST*(BFSHXG(isc4,jfuel,iflt)+(EFSHXG(isc4,jfuel,iflt)-BFSHXG(isc4,jfuel,iflt))  &
                                  /(1.+(exp(SLOPE*(1.*curcalyr - MIDYR)))))				  				
			  DO ivmt=1, nvmt
				fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MPATH_regn(icafe19,jfuel,iflt,iyr,iregn)*veh_shr(ivmt,iflt,icafe19)
			  ENDDO
				
			ENDIF  ! end of jfuel.ge.3

!         Diesel's curve and parameters are based on a 1:1 competition b/w gasoline and diesel
		  ELSEIF(jfuel.eq.1)then
            MPATH_regn(icafe19,jfuel,iflt,iyr,iregn) = BFSHXG(isc4,jfuel,iflt) + (EFSHXG(isc4,jfuel,iflt) - & 
			                                           BFSHXG(isc4,jfuel,iflt))*(1. - exp(CSTDXG(isc4,iflt) + CSTDVXG(isc4,iflt)*(1.*curcalyr)))

			DO ivmt=1, nvmt
			  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,jfuel,iregn) = MAX(0.0,MPATH_regn(icafe19,jfuel,iflt,iyr,iregn) &
				                                                        *(veh_shr(ivmt,iflt,icafe19) &
																		 -sum(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,[3:FUEL12],iregn))))
			ENDDO
		  ENDIF
		ENDDO  	! end fuel loop

!		Gasoline gets the leftover
!       Prevent gasoline sales share growth in Class 7&8
		DO ivmt = 1, nvmt
		  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,2,iregn)= MAX(0.0, (veh_shr(ivmt,iflt,icafe19)-sum(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,[1,3:FUEL12],iregn))))
		  if (icafe19.gt.7) fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,2,iregn) = MIN(fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,2,iregn),fuel_shr_ivmt(BSYR_STK-1989,ivmt,iflt,icafe19,2,iregn))
        ENDDO
		
      ENDDO   ! end size class loop		  
    ENDDO     ! end fleet loop
  ENDDO       ! end regional loop
    
!	Normalize fuel shares to ensure they sum to 1
    DO iregn = 1,MNUMCR-2
      IF (iregn.eq.10) CYCLE
      DO iflt=1,flt
        DO icafe19 = 1,CAFE19
		  DO ifuel = 1,FUEL12
			DO ivmt = 1, nvmt
			  IF (ifuel.eq.1.and.ivmt.eq.1) TEMP = SUM(fuel_shr_ivmt(iyr,:,iflt,icafe19,:,iregn))
			  fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn) = fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/TEMP
			ENDDO

			fuel_shr_regn(iyr,icafe19,ifuel,iflt,iregn)  = sum(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn))

!			Distribute total new truck sales to fuels			  
			TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn) = NEWTRUCKS_regn(iyr,icafe19,iflt,iregn)*fuel_shr_regn(iyr,icafe19,ifuel,iflt,iregn)
            IF (iregn.eq.MNUMCR-2) then
			  DO iage = 1,age
		  	    TRKSTK_19R(iyr,icafe19,iage,ifuel,iflt,11) = sum(TRKSTK_19R(iyr,icafe19,iage,ifuel,iflt,1:mnumcr-2))
		  	  ENDDO
			ENDIF
		  ENDDO
		ENDDO
	  ENDDO
	ENDDO
      
    IF (curcalyr.eq.2050.and.FCRL.eq.1) THEN
!  	  WRITE(21,*)'CPM_R DS 2022USD (INCLUDES DEF, split out below)'
!      DO i = 33, MNUMYR
!  	  DO iregn = 1, MNUMCR-2
!          WRITE(21,'(i4,",",i2,19(",",f5.3))')i+1989,iregn,CPM_R(i,1,:,1,iregn) * mc_jpgdp(2022-1989) / mc_jpgdp(1)
!  	  ENDDO
!    	ENDDO
!  	
!  	  WRITE(21,*)'CPM_R DS DEF 2022USD'
!      DO i = 33, MNUMYR
!  	    DO iregn = 1, MNUMCR-2
!          WRITE(21,'(i4,",",i2,19(",",f5.3))')i+1989,iregn,CPM_DEF(i,:,iregn) * mc_jpgdp(2022-1989) / mc_jpgdp(1)
!  	    ENDDO
!      ENDDO
!  	
!  	  WRITE(21,*)'CPM_R BEV 2022USD'
!      DO i = 33, MNUMYR
!  	    DO iregn = 1, MNUMCR-2
!          WRITE(21,'(i4,",",i2,19(",",f5.3))')i+1989,iregn,CPM_R(i,6,:,1,iregn) * mc_jpgdp(2022-1989) / mc_jpgdp(1)
!  	    ENDDO
!      ENDDO
!  
!  	  WRITE(21,*)'CPM_R CNG 2022USD'
!      DO i = 33, MNUMYR
!  	    DO iregn = 1, MNUMCR-2
!          WRITE(21,'(i4,",",i2,19(",",f5.3))')i+1989,iregn,CPM_R(i,4,:,1,iregn) * mc_jpgdp(2022-1989) / mc_jpgdp(1)
!  	    ENDDO
!      ENDDO
!  
!  	  WRITE(21,*)'CPM_R FCEV 2022USD'
!      DO i = 33, MNUMYR
!  	    DO iregn = 1, MNUMCR-2
!          WRITE(21,'(i4,",",i2,19(",",f5.3))')i+1989,iregn,CPM_R(i,9,:,1,iregn) * mc_jpgdp(2022-1989) / mc_jpgdp(1)
!  	    ENDDO
!      ENDDO
!  
!  	  WRITE(21,*)'CPM_R FCHEV 2022USD'
!      DO i = 33, MNUMYR
!  	    DO iregn = 1, MNUMCR-2
!          WRITE(21,'(i4,",",i2,19(",",f5.3))')i+1989,iregn,CPM_R(i,10,:,1,iregn) * mc_jpgdp(2022-1989) / mc_jpgdp(1)
!  	    ENDDO
!      ENDDO
!  
!  	  WRITE(21,*)'CPM_R HEV 2022USD'
!      DO i = 33, MNUMYR
!  	    DO iregn = 1, MNUMCR-2
!          WRITE(21,'(i4,",",i2,19(",",f5.3))')i+1989,iregn,CPM_R(i,11,:,1,iregn) * mc_jpgdp(2022-1989) / mc_jpgdp(1)
!  	    ENDDO
!      ENDDO
!!  
!	  WRITE(21,*)'MPATH: maximum adoption rate w/ each VMT bin (fraction)'
!	  WRITE(21,'(a4,3(",",a2),19(",",a5))')'yr','r','fl','pt','2b','2bV','3','3V','4','5','6','7V','7Dlow','7Dmid','7Dhi','8V','8Dlow','8Dmid','8Dhi','8Slow','8Smid','8Shi','8HH'
!	  DO i = 34, MNUMYR
!	    DO iregn = 1, MNUMCR-2
!	      DO iflt = 1, 2
!	  	  DO jfuel = 1, FUEL12
!	          WRITE(21,'(i4,3(",",i2),19(",",f7.4))')i+1989,iregn,iflt,jfuel,MPATH_regn(:,jfuel,iflt,i,iregn)
!	  	  ENDDO
!	  	ENDDO
!	    ENDDO
!	  ENDDO
	ENDIF	! (curcalyr.eq.2050.and.FCRL.eq.1)

 RETURN
 END SUBROUTINE TRUCK_CHOICE

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
    REAL :: VMTSHRR(MNUMYR,MNUMCR)        !...VMT share by region
    REAL :: temp_stk_vmt_shr(MNUMYR,CAFE19,NVMT,FUEL12)
    REAL :: temp_stk_vmt_ratio(MNUMYR,CAFE19,AGE,FUEL12)
    REAL :: vmt_10yr_avg(cafe19)
	INTEGER :: q

!...calculate regional heavy-duty truck vehicle miles travelled based on industrial output
!...first compute the static heavy-duty truck freight parameter (TTM_OUTPUT, ton-miles per $ of industrial output), based on last historical FAF year (iFAFyear),
!   and then estimate historical ton-miles b/w that last FAF year and the last historical NEMS tranfrt year (TTONMI).
    IF ((iyr.ge.iFAFyear).and.(iyr.le.BSYR_VMT)) then
      DO iregn=1,MNUMCR-2
        DO isec=1,sec-2		! sec-2 only includes the non-service sectors
          IF(iyr.eq.iFAFyear) then
            IF(TSIC(isec,iregn,iFAFyear).gt.0.) then
              TTM_OUTPUT(iregn,isec)=TTONMI_ORIG(iregn,isec)/ &
              (TSIC(isec,iregn,iFAFyear)*0.001)
            ELSE
              TTM_OUTPUT(iregn,isec)=0.
            ENDIF
          ENDIF
          TTONMI(iyr,iregn,isec)=TTM_OUTPUT(iregn,isec)*(TSIC(isec,iregn,iyr)*.001)
        ENDDO
      ENDDO
    ENDIF
	
!...second compute heavy-duty truck vehicle miles travelled by census division
    DO iregn=1,MNUMCR-2
      IF(iyr.le.BSYR_VMT)then
        VMTDMDR(iyr,iregn)=THIST_VMT(iyr,iregn)*1.0E9      !  Initialize VMT to history with input placeholder values
        IF (iyr.ge.iFAFyear) then                           !  Calculate new FAF base year+ VMT by census division, using published national statistics
		  VMTDMDR(iyr,iregn)=(sum(THIST_VMT(iyr,1:MNUMCR-2))* &
          (sum(TTONMI(iyr,iregn,1:sec-2))/ &
          (sum(TTONMI(iyr,1:MNUMCR-2,1:sec-2)))))*1.0E9
        ENDIF
      ELSE
        DO isec=1,sec-2
          IF (iyr.eq.bsyr_vmt+1) then    ! Initialize base year truck VMT by census region and commodity group
            TVMT(bsyr_vmt,iregn,isec)=(sum(THIST_VMT(bsyr_vmt,1:MNUMCR-2))* &
            (TTONMI(bsyr_vmt,iregn,isec)/ &
            sum(TTONMI(bsyr_vmt,1:MNUMCR-2,1:sec-2))))*1.0E9
          ENDIF
          TTONMI(iyr,iregn,isec)=TTM_OUTPUT(iregn,isec)*(TSIC(isec,iregn,iyr)/1000.)
          VMT_GR(iyr,iregn,isec)=TTONMI(iyr,iregn,isec)/TTONMI(iyr-1,iregn,isec)
! 		  Covid recovery adjustment (industrial output growing faster than VMT)
          IF (iyr.eq.34.and.bsyr_vmt.eq.2022-1989) VMT_GR(iyr,iregn,isec)=(TTONMI(iyr,iregn,isec)/TTONMI(iyr-3,iregn,isec) + &
																				 TTONMI(iyr-1,iregn,isec)/TTONMI(iyr-2,iregn,isec) + &
																				 TTONMI(iyr-2,iregn,isec)/TTONMI(iyr-3,iregn,isec)) / 3
		  TVMT(iyr,iregn,isec)=TVMT(iyr-1,iregn,isec)*VMT_GR(iyr,iregn,isec)
        ENDDO
		VMTDMDR(iyr,iregn)=sum(TVMT(iyr,iregn,1:sec-2))
      ENDIF
    ENDDO
    VMTDMDR(iyr,11)=sum(VMTDMDR(iyr,1:MNUMCR-2))

!   Develop adjustment factors for annual VMT by powertrain based on distribution of adoption across VMT bins.
!   Before the first projection year (curcalyr.le.bsyr_stk), annual VMT is set exogenously
    IF (curcalyr.gt.bsyr_stk) THEN
      temp_stk_vmt_shr(iyr,:,:,:) = 0.0
      DO icafe19 = 1, cafe19
        IF (curcalyr.eq.bsyr_stk+1) THEN
          vmt_10yr_avg(icafe19) = SUM(VMT_VEH(1:nvmt,1:2,icafe19) * VEH_SHR(1:nvmt,1:2,icafe19))/2
          temp_stk_vmt_ratio(6:BSYR_STK-1989,icafe19,:,:) = 1.0
        ENDIF
!       Estimate the sales share by vmt bin for each size class (icafe19) and powertrain (ifuel) -- temp_stk_vmt_shr
!       First, aggregate iflt and iregn dims out of the data
        DO ifuel = 1, fuel12
          DO ivmt = 1, nvmt
            DO iflt = 1, flt
              DO iregn = 1, mnumcr-2
                IF (sum(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn)).eq.0.0) CYCLE
                temp_stk_vmt_shr(iyr,icafe19,ivmt,ifuel) = temp_stk_vmt_shr(iyr,icafe19,ivmt,ifuel) + fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn)/sum(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn)) &
                                                                                              * TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn)
!                WRITE(21,'(a,",",7(i4,","),2(f5.3,","),2(f8.1,","))')'vmt_check',curcalyr,curitr,icafe19,ifuel,ivmt,iflt,iregn,fuel_shr_ivmt(iyr,ivmt,iflt,icafe19,ifuel,iregn),sum(fuel_shr_ivmt(iyr,:,iflt,icafe19,ifuel,iregn)),&
!                                                                                    TRKSTK_19R(iyr,icafe19,1,ifuel,iflt,iregn),temp_stk_vmt_shr(iyr,icafe19,ivmt,ifuel)
              ENDDO
            ENDDO
          ENDDO
!         Convert the total sales within each cafe19/vmt/powertrain cohort into a distribution of sales by vmt bin w/in each cafe/powertrain cohort
          IF(sum(temp_stk_vmt_shr(iyr,icafe19,:,ifuel)).ne.0.0) temp_stk_vmt_shr(iyr,icafe19,:,ifuel) = temp_stk_vmt_shr(iyr,icafe19,:,ifuel)/sum(temp_stk_vmt_shr(iyr,icafe19,:,ifuel))
!         Use the share to calculate the average NEW VEHICLE annual VMT for each size class and powertrain
          temp_stk_vmt_ratio(iyr,icafe19,1,ifuel) = SUM(temp_stk_vmt_shr(iyr,icafe19,:,ifuel) * VMT_VEH(1:nvmt,1,icafe19))
!         And then the ratio of that annual VMT to the annual VMT for the whole new-vehicle fleet
          temp_stk_vmt_ratio(iyr,icafe19,1,ifuel) = temp_stk_vmt_ratio(iyr,icafe19,1,ifuel) / vmt_10yr_avg(icafe19)

!         Reset all annual VMT schedules based on how fleets adopted vehicles across different VMT bins (using baseline diesel VMT as a starting point)
!         Note that for gasoline (ifuel=2), annual VMT relative to diesel is held constant (gasoline isn't competed with diesel in the model, so it should shift vmt bins hand-in-hand with diesel)
          ANNVMT_19(iyr,icafe19,1,ifuel) = ANNVMT_19(BSYR_STK-1989,icafe19,1,1)*temp_stk_vmt_ratio(iyr,icafe19,1,ifuel)
          if (ifuel.eq.2.and.(icafe19.gt.7.or.icafe19.le.2)) ANNVMT_19(iyr,icafe19,1,ifuel) = ANNVMT_19(BSYR_STK-1989,icafe19,1,2)/ANNVMT_19(BSYR_STK-1989,icafe19,1,1) * ANNVMT_19(iyr,icafe19,1,1)
          DO iage = 2, age
            IF (iage.lt.age) temp_stk_vmt_ratio(iyr,icafe19,iage,ifuel) = temp_stk_vmt_ratio(iyr-1,icafe19,iage-1,ifuel)
            IF (iage.eq.age) temp_stk_vmt_ratio(iyr,icafe19,iage,ifuel) = temp_stk_vmt_ratio(iyr-1,icafe19,iage,ifuel)
            
            ANNVMT_19(iyr,icafe19,iage,ifuel) = ANNVMT_19(BSYR_STK-1989,icafe19,iage,1)*temp_stk_vmt_ratio(iyr,icafe19,iage,ifuel)
            if (ifuel.eq.2.and.(icafe19.gt.7.or.icafe19.le.2)) ANNVMT_19(iyr,icafe19,iage,ifuel) = ANNVMT_19(BSYR_STK-1989,icafe19,iage,2)/ANNVMT_19(BSYR_STK-1989,icafe19,iage,1) * ANNVMT_19(iyr,icafe19,iage,1)
          ENDDO
        ENDDO
!        WRITE(21,'(3(i4,","),13(f10.0,","))')curcalyr,curitr,icafe19,temp_stk_vmt_ratio(iyr,icafe19,1,:)*100,vmt_10yr_avg(icafe19)
      ENDDO
    ENDIF

!...aggregate total VMT by heavy-duty trucks (NOT including 2b)
!	This is done to calibrate Class 3-8 VMT to that estimated from industrial output growth (VMTDMDR)
	AGGVMTR=0.
	DO iage = 1,AGE
	  DO ifuel = 1,FUEL12
		DO iflt = 1,flt
		  IF(curcalyr.le.2011) then
		    DO isc = 1,3
		      AGGVMTR = AGGVMTR + SUM(ANNVMT(isc,iage,ifuel,1:voc))/2*TRKSTK(iyr,isc,iage,ifuel,iflt)
			ENDDO
		  ELSEIF (curcalyr.le.bsyr_stk) THEN
		    DO icafe19 = 3,CAFE19
		      AGGVMTR = AGGVMTR + ANNVMT_19(iyr,icafe19,iage,ifuel)*TRKSTK_19R(iyr,icafe19,iage,ifuel,iflt,11)
			ENDDO
		  ELSE
            DO icafe19 = 3,CAFE19
		      AGGVMTR = AGGVMTR + ANNVMT_19(iyr,icafe19,iage,ifuel)*TRKSTK_19R(iyr,icafe19,iage,ifuel,iflt,11)
			ENDDO
          ENDIF
		ENDDO
	  ENDDO
	ENDDO

!	Calibrate VMT schedules using factor calculated above (again, NOT incl. 2b)
    VMTADJR=VMTDMDR(iyr,11)/AGGVMTR
	ANNVMT(1:3,:,:,:) = ANNVMT(1:3,:,:,:)*VMTADJR			! ANNVMT(isc,iage,ifuel,ivoc)
	ANNVMT_19(iyr,3:19,:,:) = ANNVMT_19(iyr,3:19,:,:)*VMTADJR		! ANNVMT_19(icafe19,iage,ifuel)
	VMTFLT(CUR,1:3,:,:,:) = 0.0								! Initialize before filling below


!...Class 2b
!	Estimate pre-bsyr_stk VMT (not calibrated) based on stocks and annual VMT schedule
!	Similar to above (Class 3-8), adjust VMT schedule to account for projected VMT growth (using VMTADJR and AGGVMTR)
	AGGVMTR=0.0
	DO ifuel = 1,FUEL12
	  DO iage = 1, age
	    IF (curcalyr.le.2011) then
          IF (sum(CLTSTK(n,ifuel,:,:,11)).gt.0.0) then
		    CLTVMT(n,ifuel,iage) = CLTVMTT(ifuel,N) * 1000000000.0 * &
								   sum(CLTSTK(n,ifuel,iage,:,11)) / &
								   sum(CLTSTK(n,ifuel,:,:,11))
		  ELSE
			CLTVMT(n,ifuel,iage) = 0.0
		  ENDIF
		ELSEIF (curcalyr.eq.2012) then
          AGGVMTR = AGGVMTR + CLTSTK(n,ifuel,iage,1,11) * ANNVMT_19(iyr,1,iage,ifuel) + &
						      CLTSTK(n,ifuel,iage,2,11) * ANNVMT_19(iyr,2,iage,ifuel)		  
		ENDIF
	  ENDDO
	ENDDO

	IF (curcalyr.gt.2011) then
!	  Calibrate VMT schedule to align with historical VMT
	  IF (curcalyr.eq.2012) then
	    VMTADJR = (sum(CLTVMT(n-1,:,:)) * CLTSIC(N)/CLTSIC(N-1)) / AGGVMTR
	    ANNVMT_19(iyr,1:2,:,:) = ANNVMT_19(iyr,1:2,:,:) * VMTADJR
!	  Grow VMT at a rate that accounts for stock growth
!	  (i.e., how much faster is CLTSIC growing than CLTSTK?)
	  ELSEIF (curitr.eq.1) then
	    ANNVMT_19(iyr,1:2,:,:) = ANNVMT_19(iyr-1,1:2,:,:) * (CLTSIC(N)/CLTSIC(N-1)) / &
												  (sum(CLTSTK(n,:,:,:,11))/sum(CLTSTK(n-1,:,:,:,11)))
      ENDIF
!	  Calculate final aggregate Class 2b VMT
	  DO ifuel=1,FUEL12
        DO iage = 1,AGE 
!		  diesel
          IF(ifuel.eq.1) then
		    CLTVMT(n,2,iage) = CLTSTK(n,2,iage,1,11) * ANNVMT_19(iyr,1,iage,ifuel) + &
							   CLTSTK(n,2,iage,2,11) * ANNVMT_19(iyr,2,iage,ifuel)
!		  gasoline
		  ELSEIF(ifuel.eq.2) then
		    CLTVMT(n,1,iage) = CLTSTK(n,1,iage,1,11) * ANNVMT_19(iyr,1,iage,ifuel) + &
							   CLTSTK(n,1,iage,2,11) * ANNVMT_19(iyr,2,iage,ifuel)
		  ELSE
		    CLTVMT(n,ifuel,iage) = CLTSTK(n,ifuel,iage,1,11) * ANNVMT_19(iyr,1,iage,ifuel) + &
								   CLTSTK(n,ifuel,iage,2,11) * ANNVMT_19(iyr,2,iage,ifuel)
		  ENDIF
        ENDDO
	  ENDDO
!	  Calculate total VMT by fuel type -- can't be in above loop due to mismatch in gas/diesel indices (2b v. 3-8)
	  DO ifuel=1,FUEL12
	    CLTVMTT(ifuel,N) = SUM(CLTVMT(n,ifuel,1:age)) / 1000000000.0
	  ENDDO
	ENDIF

!...Populate VMT for all 19 size classes, and by three size classes for reporting (3, 4-6, 7&8)
	DO iage = 1,AGE
	  DO ifuel = 1,FUEL12
		DO iflt = 1,flt
		  IF(curcalyr.le.2011) then
		    DO isc = 1,3
		      VMTFLT(CUR,isc,iage,ifuel,IFLT) = sum(ANNVMT(isc,iage,ifuel,1:2))/2 * TRKSTK(iyr,isc,iage,ifuel,iflt)
			ENDDO
		  ELSE

!			Need aggregate VMT by Class 3, 4-6, and 7&8 for reporting
			VMTFLT(CUR,1,iage,ifuel,IFLT) = ANNVMT_19(iyr,3,iage,ifuel)*TRKSTK_19R(iyr,3,iage,ifuel,iflt,11) + &
											ANNVMT_19(iyr,4,iage,ifuel)*TRKSTK_19R(iyr,4,iage,ifuel,iflt,11)
			VMTFLT(CUR,2,iage,ifuel,IFLT) = ANNVMT_19(iyr,5,iage,ifuel)*TRKSTK_19R(iyr,5,iage,ifuel,iflt,11) + &
											ANNVMT_19(iyr,6,iage,ifuel)*TRKSTK_19R(iyr,6,iage,ifuel,iflt,11) + &
											ANNVMT_19(iyr,7,iage,ifuel)*TRKSTK_19R(iyr,7,iage,ifuel,iflt,11)
			DO icafe19 = 8,19
			  VMTFLT(CUR,3,iage,ifuel,IFLT) = VMTFLT(CUR,3,iage,ifuel,IFLT) + ANNVMT_19(iyr,icafe19,iage,ifuel)*TRKSTK_19R(iyr,icafe19,iage,ifuel,iflt,11)
			ENDDO

!			Need detailed VMT (by 19 size classes) for rest of model
!			Note that this disaggregate VMT doesn't have a "fleet" dimension
			IF (iflt.eq.flt) THEN
			  DO icafe19 = 1,CAFE19
			    VMTFLT_19(iyr,icafe19,iage,ifuel) = ANNVMT_19(iyr,icafe19,iage,ifuel)*SUM(TRKSTK_19R(iyr,icafe19,iage,ifuel,:,11))
			  ENDDO
			ENDIF

		  ENDIF
		ENDDO
	  ENDDO
	ENDDO

    DO iregn=1,MNUMCR
      VMTSHRR(iyr,iregn)=VMTDMDR(iyr,iregn)/VMTDMDR(iyr,11)
      VMTFLTR(cur,1:SC,1:AGE,1:FUEL12,1:FLT,iregn) = VMTFLT(cur,1:SC,1:AGE,1:FUEL12,1:FLT) * VMTSHRR(iyr,iregn)
    ENDDO

!	Debugs
	IF (iyr.eq.MNUMYR.and.fcrl.eq.1) THEN
!	  WRITE(21,*)'TRANFRT VMT CHECKS -- Iteration ',CURITR
!	  WRITE(21,*)'check_CLTSIC'
!	  DO q = 21, MNUMYR
!	    WRITE(21,'(I4,",",f9.0)')q+1989,CLTSIC(q)
!	  ENDDO
!	  WRITE(21,*)'check_faf_frt'
!	  DO q = 21, MNUMYR
!	    DO isec = 1, sec-2
!		  WRITE(21,'(I4,", ",I3,", ",9(f9.1,", "))')q+1989,isec,TVMT(q,1:9,isec)/1000000
!		ENDDO
!	  ENDDO
!	  WRITE(21,*)'check_faf_frt2'
!	  DO q = 1,MNUMYR
!		WRITE(21,'(I4,", ",16(f9.2,", "))')q+1989,sum(TSIC(1,:,q)),sum(TSIC(2,:,q)),sum(TSIC(3,:,q)),sum(TSIC(4,:,q)), &
!		sum(TSIC(5,:,q)), sum(TSIC(6,:,q)),sum(TSIC(7,:,q)),sum(TSIC(8,:,q)),sum(TSIC(9,:,q)),sum(TSIC(10,:,q)), &
!		sum(TSIC(11,:,q)),sum(TSIC(12,:,q)),sum(TSIC(13,:,q)),sum(TSIC(14,:,q)),sum(TSIC(15,:,q)),sum(TSIC(16,:,q))
!      ENDDO
!	  WRITE(21,*)'check_faf_frt3'
!	  DO q = 1,MNUMYR
!		WRITE(21,'(I4,", ",f9.2)')q+1989,sum(TSIC(:,:,q))
!      ENDDO
!	  WRITE(21,*)'annvmt_check'
!	  DO q = bsyr_stk-1989,MNUMYR
!	    DO icafe19 = 1, cafe19
!	      DO iage = 1, age
!	        WRITE(21,'(I4,",",I3,",",I3,",",12(F9.1,","))')q+1989,icafe19, iage, ANNVMT_19(q,icafe19,iage,:)
!		  ENDDO
!       ENDDO
!	  ENDDO
!	  WRITE(21,*)'vmt19_check'
!	  DO q = 22, MNUMYR
!	    DO ifuel = 1,FUEL12
!	      DO iage = 1,age
!		    IF(ifuel.le.2.or.ifuel.ge.9) WRITE(21,'(I4,",",I3,",",I3,",",19(F7.1,","))')q+1989,ifuel, iage, VMTFLT_19(q,:,iage,ifuel)/1000000
!	      ENDDO
!		ENDDO
!	  ENDDO
!	  WRITE(21,*)'check_2bvmt'
!	  DO q = 21, MNUMYR
!	    DO iage = 1, age
!		  WRITE(21,'(I4,", ",I3,", ",9(f9.1,", "))')q+1989,iage,CLTVMT(q,:,iage)/1000
!		ENDDO
!	  ENDDO
	ENDIF

   RETURN
   END SUBROUTINE TRUCK_VMT

!=============================================================================================================
   SUBROUTINE TRUCK_FUEL
   USE F_
   IMPLICIT NONE
   INCLUDE 'NGTDMOUT'

   REAL VMTFLTR_CAV_SHR(CUR,SC,AGE,FUEL12,FLT,MNUMCR),HDV_MPG_CAV_ADJ(SC,AGE,FUEL12),VMT_CAV_ELIG
   PARAMETER (VMT_CAV_ELIG =.656)
   INTEGER iphev,i

   HDV_MPG_CAV_ADJ=0.
   TOONSHR(1:TOONYEAR-1)=0.
   VMTFLTR_CAV_SHR=0.
   IF(curiyr.ge.TOONYEAR.and.curiyr.le.lastyr) then
     DO iage=1,curiyr-toonyear+1
       ISC=3
       IFLT=2
       VMTFLTR_CAV_SHR(CUR,ISC,iage,:,IFLT,:)=TOONSHR(curiyr)
     ENDDO
     HDV_MPG_CAV_ADJ(ISC,:,:)=.07 ! Operational energy savings from platooning, not fuel economy improvement
   ENDIF

!   IF(curitr.eq.maxitr+1.and.curiyr.ge.TOONYEAR-1) then
!     DO ifuel=1,FUEL12
!       DO iage=1,age
!         write(21,'(a,3(",",1x,i4),1(",",1x,f15.3),9(",",1x,f10.8))') 'msi,VMTFLTR_CAV_SHR',curiyr,ifuel,iage,sum(VMTFLTR(CUR,3,:,IFUEL,2,:)), &
!		                                                                   (VMTFLTR_CAV_SHR(CUR,3,IAGE,IFUEL,2,iregn),iregn=1,MNUMCR-2)
!       ENDDO
!     ENDDO
!   ENDIF
   
   VMTFLTR_CAV_SHR= VMTFLTR_CAV_SHR*VMT_CAV_ELIG
   
!   Get fleet-average eVMT share for the total on-road stock
!	by weighting up vintaged eVMT share (PHEV_eVMT_share) with stocks (TRKSTK_19) 
    DO iregn = 1, MNUMCR
	  PctEVMT_PHEV(iyr,:,iregn,:) = 0.0
	  PctEVMT_PHEV_AVG(iyr,iregn,:) = 0.0
	  IF (iregn.eq.10.or.iyr.le.33) CYCLE
	  DO icafe19=1,CAFE19			
        isc4 = SC19Map(icafe19)
		DO iflt=1,flt
!		  PHEV diesel
		  IF (SUM(TRKSTK_19R(iyr,icafe19,:,7,iflt,iregn)).gt.0.0) then
		    DO iage=1,age
			  IF (TRKSTK_19R(iyr,icafe19,iage,7,iflt,iregn).gt.0.0) &
		        PctEVMT_PHEV(iyr,isc4,iregn,1) = PctEVMT_PHEV(iyr,isc4,iregn,1) + PHEV_eVMT_share(iyr-iage+1,iflt,icafe19,iregn,1) * TRKSTK_19R(iyr,icafe19,iage,7,iflt,iregn)
			ENDDO
		  ENDIF
!		  PHEV gasoline
		  IF (SUM(TRKSTK_19R(iyr,icafe19,:,8,iflt,iregn)).gt.0.0) then
		    DO iage=1,age
			  IF (TRKSTK_19R(iyr,icafe19,iage,8,iflt,iregn).gt.0.0) &
		        PctEVMT_PHEV(iyr,isc4,iregn,2) = PctEVMT_PHEV(iyr,isc4,iregn,2) + PHEV_eVMT_share(iyr-iage+1,iflt,icafe19,iregn,2) * TRKSTK_19R(iyr,icafe19,iage,8,iflt,iregn)			    
		    ENDDO
		  ENDIF
		ENDDO
	  ENDDO
	  
	  DO i = 7,8
	    IF (i.eq.7) iphev = 1
		IF (i.eq.8) iphev = 2
	    IF (sum(TRKSTK_19R(iyr,:,:,i,:,iregn)).gt.0.0)    PctEVMT_PHEV_AVG(iyr,iregn,iphev) = SUM(PctEVMT_PHEV(iyr,:,iregn,iphev))/sum(TRKSTK_19R(iyr,:,:,i,:,iregn))
	    IF (sum(TRKSTK_19R(iyr, 3:4,:,i,:,iregn)).gt.0.0) PctEVMT_PHEV(iyr,1,iregn,iphev)   = PctEVMT_PHEV(iyr,1,iregn,iphev)/sum(TRKSTK_19R(iyr, 3:4,:,i,:,iregn))	! Class 3
	    IF (sum(TRKSTK_19R(iyr, 5:7,:,i,:,iregn)).gt.0.0) PctEVMT_PHEV(iyr,2,iregn,iphev)   = PctEVMT_PHEV(iyr,2,iregn,iphev)/sum(TRKSTK_19R(iyr, 5:7,:,i,:,iregn))	! Class 4-6
	    IF (sum(TRKSTK_19R(iyr,8:19,:,i,:,iregn)).gt.0.0) PctEVMT_PHEV(iyr,3,iregn,iphev)   = PctEVMT_PHEV(iyr,3,iregn,iphev)/sum(TRKSTK_19R(iyr,8:19,:,i,:,iregn))	! Class 7&8
	    IF (sum(TRKSTK_19R(iyr, 1:2,:,i,:,iregn)).gt.0.0) PctEVMT_PHEV(iyr,4,iregn,iphev)   = PctEVMT_PHEV(iyr,4,iregn,iphev)/sum(TRKSTK_19R(iyr, 1:2,:,i,:,iregn))	! Class 2b
	  ENDDO
	ENDDO

!   Detailed outputs
!	IF (curcalyr.eq.2050.and.FCRL.eq.1) THEN
!	  WRITE(21,*)'PHEV share of VMT on electricity'
!	  WRITE(21,'(a4,",",a2,",",a2,11(",",a6))')'yr','sc','iphev','cd1','cd2','cd3','cd4','cd5','cd6','cd7','cd8','cd9','cd10','cd11'
!	  DO i = 33, MNUMYR
!	    DO isc = 1, sc4
!		  DO iphev=1,2
!	        WRITE(21,'(i4,",",i2,",",i2,11(",",f6.4))')i+1989,isc,iphev,PctEVMT_PHEV(i,isc,:,iphev)
!		  ENDDO
!		ENDDO
!	  ENDDO
!	ENDIF
	
   
!...Calculate fuel demand from VMT and MPG by size class, fuel and fleet/nonfleet, by census division
    DO ISC=1,SC4
      DO IFLT=1,FLT
        DO iregn=1,MNUMCR-2
          DO IFUEL=1,FUEL12
            FUELDMDR(ISC,IFUEL,IFLT,iregn)=0.
            DO IAGE=1,AGE
			  IF (isc.ne.4) THEN
                IF(HDV_MPG(iyr,ISC,IAGE,IFUEL).gt.0.0) &
                  FUELDMDR(ISC,IFUEL,IFLT,iregn) =  FUELDMDR(ISC,IFUEL,IFLT,iregn) + &
												 ((1.- VMTFLTR_CAV_SHR(CUR,ISC,IAGE,IFUEL,IFLT,iregn))*VMTFLTR(CUR,ISC,IAGE,IFUEL,IFLT,iregn)/(HDV_MPG(iyr,ISC,IAGE,IFUEL))) + & 
												 (VMTFLTR_CAV_SHR(CUR,ISC,IAGE,IFUEL,IFLT,iregn))*VMTFLTR(CUR,ISC,IAGE,IFUEL,IFLT,iregn)/(HDV_MPG(iyr,ISC,IAGE,IFUEL)/(1.-HDV_MPG_CAV_ADJ(ISC,IAGE,IFUEL)))
              ELSE      ! Class 2b
                IF (curcalyr.le.2011) THEN      ! Regional stocks aren't populated pre-2012
                  IF(cltmpg_yr(iyr,CLTMap(ifuel),iage).gt.0.0.and.SUM(TRKSTK_19R(2012-1989,1:2,iage,ifuel,1:2,11)).gt.0.0) &
                    FUELDMDR(ISC,IFUEL,IFLT,iregn) =  FUELDMDR(ISC,IFUEL,IFLT,iregn) + SUM(VMTFLT_19(iyr,1:2,iage,ifuel)) &
                                                                                     * SUM(TRKSTK_19R(2012-1989,1:2,iage,ifuel,iflt,iregn))/SUM(TRKSTK_19R(2012-1989,1:2,iage,ifuel,1:2,11)) &
                                                                                     / cltmpg_yr(iyr,CLTMap(ifuel),iage)
                ELSE
                  IF(cltmpg_yr(iyr,CLTMap(ifuel),iage).gt.0.0.and.SUM(TRKSTK_19R(iyr,1:2,iage,ifuel,1:2,11)).gt.0.0) &
                    FUELDMDR(ISC,IFUEL,IFLT,iregn) =  FUELDMDR(ISC,IFUEL,IFLT,iregn) + SUM(VMTFLT_19(iyr,1:2,iage,ifuel)) &
                                                                                     * SUM(TRKSTK_19R(iyr,1:2,iage,ifuel,iflt,iregn))/SUM(TRKSTK_19R(iyr,1:2,iage,ifuel,1:2,11)) &
                                                                                     / cltmpg_yr(iyr,CLTMap(ifuel),iage)
                ENDIF
              ENDIF
			ENDDO
          ENDDO
!         Diesel (conventional, PHEV)
		  FUELBTUR(ISC,1,IFLT,iregn) = FUELDMDR(ISC,1,IFLT,iregn)*HRATE(isc,1)*(1E-12) + &
		                               FUELDMDR(ISC,7,IFLT,iregn)*HRATE(isc,7)*(1E-12)*(1.0-PctEVMT_PHEV(iyr,isc,iregn,1))
!         Gasoline (conventional, PHEV, HEV)
		  FUELBTUR(ISC,2,IFLT,iregn) = FUELDMDR(ISC,2,IFLT,iregn)*HRATE(isc,2)*(1E-12) + &
		                               FUELDMDR(ISC,5,IFLT,iregn)*HRATE(isc,5)*(1E-12)*(1.0-PCTAF(2,iregn,iyr)) + &
		                               FUELDMDR(ISC,8,IFLT,iregn)*HRATE(isc,8)*(1E-12)*(1.0-PctEVMT_PHEV(iyr,isc,iregn,2)) + &
                                       FUELDMDR(ISC,11,IFLT,iregn)*HRATE(isc,11)*(1E-12)
!         LPG
		  FUELBTUR(ISC,3,IFLT,iregn) = FUELDMDR(ISC,3,IFLT,iregn)*HRATE(isc,3)*(1E-12)	
!         Natural Gas
		  FUELBTUR(ISC,4,IFLT,iregn) = FUELDMDR(ISC,4,IFLT,iregn)*HRATE(isc,4)*(1E-12)
!         Ethanol		   
		  FUELBTUR(ISC,5,IFLT,iregn) = FUELDMDR(ISC,5,IFLT,iregn)*HRATE(isc,5)*(1E-12)*PCTAF(2,iregn,iyr)
!         Electric
          FUELBTUR(ISC,6,IFLT,iregn) = FUELDMDR(ISC,6,IFLT,iregn)*HRATE(isc,6)*(1E-12) + &
		                               FUELDMDR(ISC,7,IFLT,iregn)*HRATE(isc,7)*(1E-12)*PctEVMT_PHEV(iyr,isc,iregn,1) / CHRG_EFF(iyr) + &
		                               FUELDMDR(ISC,8,IFLT,iregn)*HRATE(isc,8)*(1E-12)*PctEVMT_PHEV(iyr,isc,iregn,2) / CHRG_EFF(iyr)
!         Hydrogen
          FUELBTUR(ISC,7,IFLT,iregn) = FUELDMDR(ISC,9,IFLT,iregn)*HRATE(isc,9)*(1E-12)+ &
									   FUELDMDR(ISC,10,IFLT,iregn)*HRATE(isc,10)*(1E-12)+&
                                       FUELDMDR(ISC,12,IFLT,iregn)*HRATE(isc,12)*(1E-12)
        ENDDO	
		DO ifuel = 1,7
          FUELBTUR(isc,ifuel,iflt,11) = sum(FUELBTUR(isc,ifuel,iflt,1:MNUMCR-2))
        ENDDO
      ENDDO
    ENDDO

	
!... The following variable is used in tran.f to calculate benchmark factors
    DO ISC=1,SC4
      DO ifuel=1,7
        DO iregn=1,MNUMCR-2
          IF (isc.ne.4) THEN
            TFRBTU_F_T(IYR,ISC,ifuel,iregn) = sum(FUELBTUR(ISC,ifuel,1:FLT,iregn))
          ELSE
            cltfbtu(iyr,CLTMap2(ifuel),iregn) = sum(FUELBTUR(ISC,ifuel,1:FLT,iregn))
          ENDIF 
        ENDDO
        IF (isc.ne.4) THEN
          TFRBTU_F_T(iyr,isc,ifuel,11) = sum(TFRBTU_F_T(iyr,isc,ifuel,1:MNUMCR-2))
        ELSE
          cltfbtu(iyr,CLTMap2(ifuel),11) = sum(cltfbtu(iyr,ifuel,1:MNUMCR-2))
        ENDIF
      ENDDO
    ENDDO

!...Calculate the share of truck electricity consumption by public/retail (1) and depot/fleet (2)
!   NOT including Class 2b (accounted for separately in TRQ_ELEC)
    IF (curcalyr.gt.2020) THEN
      TFRBTU_chgsplit(:,iyr) = 0.0
      DO icafe19 = 3, cafe19
        icafe14 = P219Map(icafe19)
        IF (NEW_MPG_19(iyr,6,icafe19).eq.0.0) CYCLE
        DO iregn = 1, mnumcr-2
          DO iage = 1, age
            IF (iflt.eq.1.or.icafe19.ge.16) then      ! Non-fleet and all sleeper cabs
              TFRBTU_chgsplit(1,iyr) = TFRBTU_chgsplit(1,iyr) + ANNVMT_19(iyr,icafe19,iage,6)*sum(TRKSTK_19R(iyr,icafe19,iage,6,1:2,iregn)) &
                                                                / (NEW_MPG_19(iyr,6,icafe19)*(1-HDV_DF(ifuel,icafe14)))
            ELSE                                      ! All others (non-sleeper-cab fleet vehicles)
              TFRBTU_chgsplit(2,iyr) = TFRBTU_chgsplit(2,iyr) + ANNVMT_19(iyr,icafe19,iage,6)*sum(TRKSTK_19R(iyr,icafe19,iage,6,1:2,iregn)) &
                                                                / (NEW_MPG_19(iyr,6,icafe19)*(1-HDV_DF(ifuel,icafe14)))
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      IF (sum(TFRBTU_chgsplit(:,iyr)).gt.0.0) TFRBTU_chgsplit(:,iyr) = TFRBTU_chgsplit(:,iyr)/sum(TFRBTU_chgsplit(:,iyr))
!      WRITE(21,'(a,2(",",i4),2(",",f5.3))')'chgsplit_check',curcalyr,curitr,TFRBTU_chgsplit(:,iyr)
    ELSE
      TFRBTU_chgsplit(:,iyr) = 0.5
    ENDIF

    QGFTRPV(iregn,iyr) = 0.0
    QGFTRFV(iregn,iyr) = 0.0
    QGLTRPV(iregn,iyr) = 0.0
    QGLTRFV(iregn,iyr) = 0.0
!...Similarly, calculate the share of truck NG consumption by public and depot/private
    DO iregn=1,MNUMCR
      QGFTRPV(iregn,iyr) = sum(FUELBTUR(1:2,4,nft,iregn))	! CNG central refueling (class 3 and class 4-6)
      QGFTRFV(iregn,iyr) = sum(FUELBTUR(1:2,4,flt,iregn))	! CNG retail purchase (class 3 and class 4-6)
      QGLTRPV(iregn,iyr) = FUELBTUR(3,4,nft,iregn)			! LNG central refueling (class 7-8)
      QGLTRFV(iregn,iyr) = FUELBTUR(3,4,flt,iregn)			! LNG retail refueling (class 7-8)
    ENDDO

   RETURN
   END SUBROUTINE TRUCK_FUEL

! ==========================================================================================================
! ... Subroutine TCOMMCL_TRK: Commercial Light Truck Model - Class 2b Vehicles
! ...   8,500 TO 10,000 lbs Gross Vehicle Weight (GVW)
! ==========================================================================================================
    SUBROUTINE TCOMMCL_TRK
    USE F_
    IMPLICIT NONE

    REAL     NUM, DEN
	REAL     var(FUEL12)
    REAL     CLTGAL

	if(CURITR.eq.1) then 	
	  CLTMPG(iyr,:) = 0.0
	endif

!... New fuel economy by powertrain
    do ifuel = 1,FUEL12
      NCLTMPG(iyr,ifuel) = cltmpg_yr(iyr,ifuel,1)
	enddo
!... Sales weighted average new fuel economy
	do ifuel = 1,FUEL12
      var(ifuel) = 0.0
	  if(ncltmpg(iyr,ifuel).gt.0.0) var(ifuel) = (sum(CLTSTK(iyr,ifuel,1,:,11))/SUM(CLTSTK(iyr,:,1,:,11)))/ncltmpg(iyr,ifuel)
    enddo
	NCLTMPGT(iyr) = 1/sum(var(:))

!... VMT weighted average stock fuel economy	
	do ifuel=1,FUEL12
      NUM = 0.0
      DEN = 0.0
      do iage=1,age
        if(cltmpg_yr(iyr,ifuel,iage).gt.0.0) then
          NUM = NUM + CLTVMT(iyr,ifuel,iage)
          DEN = DEN + CLTVMT(iyr,ifuel,iage)/cltmpg_yr(iyr,ifuel,iage)
        endif
      enddo
        if(DEN.gt.0.0) CLTMPG(iyr,ifuel) = NUM/DEN
    enddo

!...CLT BTU by powertrain (tbtu) -- regionalized using stocks
!   Note that ifuel = {1:gasoline, 2:diesel} (flipped from freight trucks)
	CLTGAL = 0.0
    do iregn = 1, mnumcr-2
      do ifuel=1,FUEL12
	    CLTBTUT(CLTMap(ifuel),iregn,iyr) = 0.0
        do iage = 1,age
	      IF (curcalyr.le.2011) THEN        ! No regional stocks before 2012
            IF (cltmpg_yr(iyr,CLTMap(ifuel),iage).gt.0.0.and.SUM(TRKSTK_19R(2012-1989,1:2,iage,ifuel,1:2,11)).gt.0.0) &
              CLTBTUT(CLTMap(ifuel),iregn,iyr) = CLTBTUT(CLTMap(ifuel),iregn,iyr) + CLTVMT(iyr,CLTMap(ifuel),iage)/cltmpg_yr(iyr,CLTMap(ifuel),iage)	&
                                                                                  * SUM(TRKSTK_19R(2012-1989,1:2,iage,ifuel,1:2,iregn))/SUM(TRKSTK_19R(2012-1989,1:2,iage,ifuel,1:2,11)) &
                                                                                  * HRATE(4,ifuel)/1000/1000000000.0
          
          ELSE
            IF (cltmpg_yr(iyr,CLTMap(ifuel),iage).gt.0.0.and.SUM(TRKSTK_19R(iyr,1:2,iage,ifuel,1:2,11)).gt.0.0) &
              CLTBTUT(CLTMap(ifuel),iregn,iyr) = CLTBTUT(CLTMap(ifuel),iregn,iyr) + CLTVMT(iyr,CLTMap(ifuel),iage)/cltmpg_yr(iyr,CLTMap(ifuel),iage)	&
                                                                                  * SUM(TRKSTK_19R(iyr,1:2,iage,ifuel,1:2,iregn))/SUM(TRKSTK_19R(iyr,1:2,iage,ifuel,1:2,11)) &
                                                                                  * HRATE(4,ifuel)/1000/1000000000.0
          ENDIF
        enddo
!        WRITE(21,'(a,5(",",i4),",",f18.9)')'cltbtut',curcalyr,curitr,iregn,ifuel,CLTMap(ifuel),CLTBTUT(CLTMap(ifuel),iregn,iyr),SUM(TRKSTK_19R(iyr,1:2,iage,ifuel,1:2,iregn)),CLTVMT(iyr,CLTMap(ifuel),iage),cltmpg_yr(iyr,CLTMap(ifuel),iage)
        CLTGAL = CLTGAL + CLTBTUT(CLTMap(ifuel),iregn,iyr) / (HRATE(4,ifuel)/1000/1000000000.0)
	  enddo
    enddo
    
	DO ifuel=1,FUEL12
	  CLTBTUT(CLTMap(ifuel),mnumcr,iyr) = SUM(CLTBTUT(CLTMap(ifuel),1:mnumcr-2,iyr))
	ENDDO
    
    CLTMPGT(iyr) = (SUM(CLTVMTT(:,iyr))*10.0**9)/CLTGAL   ! average mpg over vintages

!... Filling in BTU by fuel variable
    do ifuel = 1,FUEL12
      if(CLTMPG(iyr,ifuel).eq.0.0) CLTMPG(iyr,ifuel) = 1.0
    enddo

  RETURN
  END SUBROUTINE TCOMMCL_TRK

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

!...Call read input file and set variable (DO only one a run)
    IF(.not.done_once)then
      done_once=.true.
	  WRITE(21,*)'TRANFRT_doonce',iyr
      CALL CFREAD
	  CALL CFREADSTOCK
    ENDIF

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
    DO iregn=1,MNUMCR
      TSIC(1,iregn,iyr) = sum(MC_REVIND(iregn,15:20,iyr)) - &
                          MC_REVIND(iregn,17,iyr) - &
                          MC_REVIND(iregn,19,iyr) - &
                          MC_REVIND(iregn,21,iyr)
      TSIC(2,iregn,iyr) = sum(MC_REVIND(iregn,33:35,iyr))
      TSIC(3,iregn,iyr) = sum(MC_REVIND(iregn,2:5,iyr))
      TSIC(4,iregn,iyr) = sum(MC_REVIND(iregn,11:13,iyr))
      TSIC(5,iregn,iyr) = sum(MC_REVIND(iregn,25:26,iyr))
      TSIC(6,iregn,iyr) = MC_REVIND(iregn,28,iyr) + &
                          MC_REVIND(iregn,30,iyr) + &
                          MC_REVIND(iregn,32,iyr)
      TSIC(7,iregn,iyr) = sum(MC_REVIND(iregn,36:40,iyr)) - &
                          MC_REVIND(iregn,38,iyr)
      TSIC(8,iregn,iyr) = sum(MC_REVIND(iregn,7:8,iyr)) + &
                          MC_REVIND(iregn,14,iyr) + &
                          MC_REVIND(iregn,41,iyr)
      TSIC(9,iregn,iyr) = sum(MC_REVIND(iregn,42:44,iyr))
      TSIC(10,iregn,iyr)= sum(MC_REVIND(iregn,45:47,iyr))
      TSIC(11,iregn,iyr)= MC_REVIND(iregn,6,iyr)
      TSIC(12,iregn,iyr)= MC_REVIND(iregn,21,iyr)
      TSIC(13,iregn,iyr)= MC_REVIND(iregn,19,iyr)
      TSIC(14,iregn,iyr)= MC_REVIND(iregn,27,iyr)
      TSIC(15,iregn,iyr)= MC_REVIND(iregn,38,iyr)
      TSIC(16,iregn,iyr)= MC_REVIND(iregn,9,iyr)
      TSIC(17,iregn,iyr)= sum(MC_REVSER(iregn,3:4,iyr))
      TSIC(18,iregn,iyr)= MC_REVSER(iregn,10,iyr)
    ENDDO
	TSIC(1:16,:,iyr) = TSIC(1:16,:,iyr)*1000.

!...Class 2b Vehicle miles traveled
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
                (sum(VMTHH(N,1:MNUMCR-2,:,:)) + & 								! VMTHH(N,1:MNUMCR-2,1:maxldv,1:maxvtyp)	
					SUM(FLTVMTECH(:,:,:,:)) / 1000000000.0) * CLTVMTDIST(6)		! FLTVMTECH(1:maxvtyp,1:maxfleet,1:maxldv,1:maxhav)

!...Set average fuel prices and convert from 1987$/mmbtu to 1990$/mmbtu
!	GDP deflator from TEDB Table B18
!	PHEV utility factor is not consistent with that produced in the powertrain choice module; values below are only used for 
!	CAFE tech adoption, though, so not a major concern (don't see PHEVs adopting significant tech to meet CAFE).
    DO iregn = 1,MNUMCR
      FUELPRICE_R(iyr,1,iregn)  = PDSTR(iregn,IYR)*1.115                                                            ! Diesel
      FUELPRICE_R(iyr,2,iregn)  = PMGTR(iregn,IYR)*1.115                                                            ! Gasoline
      FUELPRICE_R(iyr,3,iregn)  = PLGTR(iregn,IYR)*1.115                                                            ! LPG
      FUELPRICE_R(iyr,4,iregn)  = ( (PGFTRFV(iregn,IYR) + PGFTRPV(iregn,IYR)) * NG_priceadj(1) &
								   +(PGLTRFV(iregn,IYR) + PGLTRPV(iregn,IYR)) * NG_priceadj(2) )/4 * 1.115          ! CNG/LNG
	  FUELPRICE_R(iyr,5,iregn)  = MIN(PETTR(iregn,IYR),PMGTR(iregn,IYR)) * 1.115                                    ! Flex fuel
	  FUELPRICE_R(iyr,6,iregn)  = PELIBCM(iregn,IYR)*1.115 	                                                        ! Electric
	  FUELPRICE_R(iyr,7,iregn)  = (SUM(PHEVElecVMT(:))/CAFE19*PELIBCM(iregn,IYR) &						            ! PHEV Diesel
								+ (1.0-SUM(PHEVElecVMT(:))/CAFE19)*PDSTR(iregn,IYR)) * 1.115
	  FUELPRICE_R(iyr,8,iregn)  = (SUM(PHEVElecVMT(:))/CAFE19*PELIBCM(iregn,IYR) &                                  ! PHEV Gasoline
								+ (1.0-SUM(PHEVElecVMT(:))/CAFE19)*PMGTR(iregn,IYR)) * 1.115
	  FUELPRICE_R(iyr,9,iregn)  = PH2TR(iregn,IYR) * 1.115                                                          ! Hydrogen
	  FUELPRICE_R(iyr,10,iregn) = PH2TR(iregn,IYR) * 1.115
	  FUELPRICE_R(iyr,11,iregn) = PMGTR(iregn,IYR) * 1.115                                                          ! Gasoline HEV
	  FUELPRICE_R(iyr,12,iregn) = PH2TR(iregn,IYR) * 1.115 * (1-H2ICE_fueldisc)                                     ! H2 ICE
    ENDDO

   RETURN
   END SUBROUTINE INIT

!=============================================================================================================
   SUBROUTINE CFREAD
   USE F_
   IMPLICIT NONE

!...Reads input for the freight model from spreadsheet input file
    REAL*4 TEMPR_S(SC)
    REAL*4 TEMPR_A_S(AGE,SC4)
    INTEGER*2 IT,IV, J, I, INOTE,FUEL4
    PARAMETER(FUEL4=4)       ! This is only used for annual VMT read-in
    LOGICAL NEW
    CHARACTER*18 FNAME
    INTEGER FILE_MGR
    EXTERNAL FILE_MGR
    CHARACTER*16 RNAME
    INTEGER WKUNIT,SYSUNIT
	REAL VMTCLS2b(AGE,FUEL4)                ! vmt per truck by fuel and vintage, class 2b
	REAL VMTCLS2bV(AGE,FUEL4)               ! vmt per truck by fuel and vintage, class 2b vocational
	REAL VMTCLS3V(AGE,FUEL4)                ! vmt per truck by fuel and vintage, class 3
	REAL VMTCLS46V(AGE,FUEL4)               ! vmt per truck by fuel and vintage, class 4-6
	REAL VMTCLS78V(AGE,FUEL4)               ! vmt per truck by fuel and vintage, class 7-8 vocation
	REAL VMTCLS78(AGE,FUEL4)                ! vmt per truck by fuel and vintage, class 7-8 tractor
    REAL batt_enden_imp(MNUMYR)
    
!...call subroutine to read defined ranges from spreadsheet
    FNAME='TRNHDVX'
    WKUNIT = FILE_MGR('O',FNAME,NEW) ! open trnhdvx.xlsx input file

!============ General freight module parameters and assumptions ============

    CALL ReadRngXLSX(WKUNIT,'trnhdv') ! read range names & corresponding data from worksheet "trnhdv"

!...Share of vehicle stocks in fleets
	CALL GETRNGR('FLTSHR_SALES    ',FLTSHR_SALES(IY:BSYR_STK-1989,1:CAFE19),BSYR_STK-1995+1,CAFE19,1)
	CALL GETRNGR('FLTSHR_STK      ',FLTSHR_STK(IY:BSYR_STK-1989,1:CAFE19),BSYR_STK-1995+1,CAFE19,1)
	
	DO i = BSYR_STK + 1-1989, MNUMYR
	  FLTSHR_SALES(i,:) = FLTSHR_SALES(BSYR_STK-1989,:)
	  FLTSHR_STK(i,:) = FLTSHR_STK(BSYR_STK-1989,:)
	ENDDO

!...VMT PER TRUCK BY FUEL AND VINTAGE
	CALL GETRNGR('VMTCLS2b        ',VMTCLS2b,AGE,FUEL4,1)
	CALL GETRNGR('VMTCLS2bV       ',VMTCLS2bV,AGE,FUEL4,1)
	CALL GETRNGR('VMTCLS3V        ',VMTCLS3V,AGE,FUEL4,1)
    CALL GETRNGR('VMTCLS46V       ',VMTCLS46V,AGE,FUEL4,1)
    CALL GETRNGR('VMTCLS78V       ',VMTCLS78V,AGE,FUEL4,1)
    CALL GETRNGR('VMTCLS78        ',VMTCLS78,AGE,FUEL4,1)	

!...Set VMT per truck, and adjust non-Class-2b VMT per truck to match aggregate total
!	IF (iyr.eq.6) then		! Only read these in and set initial ANN_VMT_19 once
	  DO iage=1,age
!	    Class 3-8 is exogenous
	    IF (iage.eq.1) then
          DO ivoc = 1,voc
            ANNVMT(1,1:AGE,1:FUEL4,ivoc)=VMTCLS3V(1:AGE,1:FUEL4)
            ANNVMT(2,1:AGE,1:FUEL4,ivoc)=VMTCLS46V(1:AGE,1:FUEL4)
	      ENDDO
		  
          ANNVMT(3,1:AGE,1:FUEL4,1)=VMTCLS78(1:AGE,1:FUEL4)	        ! Tractor VMT
          ANNVMT(3,1:AGE,1:FUEL4,2)=VMTCLS78V(1:AGE,1:FUEL4)          ! Vocational VMT
          ANNVMT(4,1:AGE,1:FUEL4,1)=VMTCLS2b(1:AGE,1:FUEL4)	        ! 2b Pickup/van VMT	
          ANNVMT(4,1:AGE,1:FUEL4,2)=VMTCLS2bV(1:AGE,1:FUEL4)          ! 2b Vocational VMT        

!		  Set non-diesel/gasoline/cng/lpg/bev vehicle VMT equal to gasoline vehicles
!		  Except for Class 7/8 BEV/FCEV/FCHEV/H2ICE, which are set equal to diesels
	      DO isc = 1,sc4
	        DO ifuel = 5,FUEL12
	          ANNVMT(isc,1:AGE,ifuel,1:voc) = ANNVMT(isc,1:AGE,2,1:voc)
			  IF (isc.eq.3.and.ifuel.ge.6) ANNVMT(isc,1:AGE,ifuel,1:voc) = ANNVMT(isc,1:AGE,1,1:voc)
	        ENDDO
	      ENDDO
	    ENDIF
	  
!	    Now allocate exogenous Class 3-8 values to disaggregate size classes (2b already done above)
	    DO ifuel=1,FUEL12
          ANNVMT_19(1:BSYR_STK-1989,1,iage,ifuel)  = ANNVMT(4,iage,ifuel,1)			 ! Class 2b pickup and van
	  	  ANNVMT_19(1:BSYR_STK-1989,2,iage,ifuel)  = ANNVMT(4,iage,ifuel,2)			 ! Class 2b vocational
	  	  ANNVMT_19(1:BSYR_STK-1989,3,iage,ifuel)  = ANNVMT(1,iage,ifuel,1)          ! Class 3 pickup and van
          ANNVMT_19(1:BSYR_STK-1989,4,iage,ifuel)  = ANNVMT(1,iage,ifuel,2)          ! Class 3 vocational	
          ANNVMT_19(1:BSYR_STK-1989,5,iage,ifuel)  = ANNVMT(2,iage,ifuel,2)          ! Class 4 vocational	
          ANNVMT_19(1:BSYR_STK-1989,6,iage,ifuel)  = ANNVMT(2,iage,ifuel,2)          ! Class 5 vocational	
          ANNVMT_19(1:BSYR_STK-1989,7,iage,ifuel)  = ANNVMT(2,iage,ifuel,2)          ! Class 6 vocational	
          ANNVMT_19(1:BSYR_STK-1989,8,iage,ifuel)  = ANNVMT(3,iage,ifuel,2)          ! Class 7 vocational	
	  	  ANNVMT_19(1:BSYR_STK-1989,9,iage,ifuel)  = ANNVMT(3,iage,ifuel,1)          ! Class 7 tractor day low 
	  	  ANNVMT_19(1:BSYR_STK-1989,10,iage,ifuel) = ANNVMT(3,iage,ifuel,1)          ! Class 7 tractor day mid 
	  	  ANNVMT_19(1:BSYR_STK-1989,11,iage,ifuel) = ANNVMT(3,iage,ifuel,1)          ! Class 7 tractor day high 
	  	  ANNVMT_19(1:BSYR_STK-1989,12,iage,ifuel) = ANNVMT(3,iage,ifuel,2)          ! Class 8 vocational
	  	  ANNVMT_19(1:BSYR_STK-1989,13,iage,ifuel) = ANNVMT(3,iage,ifuel,1)          ! Class 8 tractor day low 
	  	  ANNVMT_19(1:BSYR_STK-1989,14,iage,ifuel) = ANNVMT(3,iage,ifuel,1)          ! Class 8 tractor day mid 
	  	  ANNVMT_19(1:BSYR_STK-1989,15,iage,ifuel) = ANNVMT(3,iage,ifuel,1)          ! Class 8 tractor day high 
	  	  ANNVMT_19(1:BSYR_STK-1989,16,iage,ifuel) = ANNVMT(3,iage,ifuel,1)          ! Class 8 tractor sleeper low 
	  	  ANNVMT_19(1:BSYR_STK-1989,17,iage,ifuel) = ANNVMT(3,iage,ifuel,1)          ! Class 8 tractor sleeper mid 
	  	  ANNVMT_19(1:BSYR_STK-1989,18,iage,ifuel) = ANNVMT(3,iage,ifuel,1)          ! Class 8 tractor sleeper high 
	  	  ANNVMT_19(1:BSYR_STK-1989,19,iage,ifuel) = ANNVMT(3,iage,ifuel,2)          ! Class 8 tractor heavy haul   
   	    ENDDO
	  ENDDO
!	ENDIF

!...Survival rates for diesel and all other fuels
    CALL GETRNGR('SURV_D          ',SURV_RATE(1:AGE,1:9,1:11,1),AGE,MNUMCR-2,11)
    CALL GETRNGR('SURV_ND         ',SURV_RATE(1:AGE,1:9,1:11,2),AGE,MNUMCR-2,11)
    DO I = 2, FUEL12
	  CALL GETRNGR('SURV_ND         ',SURV_RATE(1:AGE,1:9,1:11,I),AGE,MNUMCR-2,11)
	ENDDO
	CALL GETRNGR('TFFXGRT         ',TFFXGRT,AGE,SC4,1)
	
!...VMT ASSUMPTIONS
    CALL GETRNGR('THIST_VMT       ',THIST_VMT (6:BSYR_VMT,1:9),BSYR_VMT-6+1,MNUMCR-2,1)
    CALL GETRNGR('TTONMI_ORIG     ',TTONMI_ORIG (1:9,1:16),MNUMCR-2,SEC-2,1)

!...Historic MPG values by age and fuel
    CALL GETRNGR('BMPGSTK2B       ',BMPGSTK2B(1:AGE,IY:BSYR_STK-BASEYR+1,1:6),AGE,BSYR_STK-1995+1,6)
    CALL GETRNGR('BMPGSTK3        ',BMPGSTK3 (1:AGE,IY:BSYR_STK-BASEYR+1,1:6),AGE,BSYR_STK-1995+1,6)
    CALL GETRNGR('BMPGSTK46       ',BMPGSTK46(1:AGE,IY:BSYR_STK-BASEYR+1,1:6),AGE,BSYR_STK-1995+1,6)
    CALL GETRNGR('BMPGSTK78       ',BMPGSTK78(1:AGE,IY:BSYR_STK-BASEYR+1,1:6),AGE,BSYR_STK-1995+1,6)

!...Filling in historic fuel economy through bsyr_stk
	HDV_MPG(:,:,:,:) = 0.0
	DO i = IY, bsyr_stk-1989
	  DO iage = 1,age
        HDV_MPG(i,1,iage,1:6) = BMPGSTK3 (iage,i,1:6)
        HDV_MPG(i,2,iage,1:6) = BMPGSTK46(iage,i,1:6)
        HDV_MPG(i,3,iage,1:6) = BMPGSTK78(iage,i,1:6)
		HDV_MPG(i,4,iage,1:6) = BMPGSTK2B(iage,i,1:6)
	    cltmpg_yr(i,2,iage)   = HDV_MPG(i,4,iage,1)					! Diesel 2b
	    cltmpg_yr(i,1,iage)   = HDV_MPG(i,4,iage,2)					! Gasoline 2b
	    DO ifuel = 3,6 
	      cltmpg_yr(i,ifuel,iage) = HDV_MPG(i,4,iage,ifuel)      	! all other fuels, 2b
        ENDDO
	  ENDDO
	ENDDO

!...Commercial light truck historical values
    CALL GETRNGR('CLTVMTDIST      ',CLTVMTDIST,1,6,1)                         
	CALL GETRNGR('CLTSALESPER     ',CLTSALESPER(1:6,6:22),6,17,1)	
	CALL GETRNGR('CLTVMT_H        ',CLTVMTT (1:6,6:22),6,17,1)
!   Populate pre-Polk-data sales (1995-2011)   
    DO i = 6,22    
      DO ifuel = 1,6      ! Total Class 1 and 2 LT sales          2b share          fuel share
        CLTSALT(ifuel,i) = (MC_Vehicles(1,i)+MC_VEHICLES(2,i)) * (1-LTSplit(i))  * CLTSALESPER(ifuel,i) * 1000000.
      ENDDO
    ENDDO

!...Waterborne Freight Model values for international and domestic marine
    CALL GETRNGR('DOMSHIP_FUEL_SHR',DOMSHIP_FUEL_SHR(1:4,IY:IJUMPYR),4,num_to_read,1)
    CALL GETRNGR('INTSHIP_FUEL_SHR',INTSHIP_FUEL_SHR(1:5,IY:IJUMPYR),5,num_to_read,1)
    CALL GETRNGR('INTSHIPLOGIT_FAC',INTSHIPLOGIT_FAC(1:5,1:3),5,3,1)
    CALL GETRNGR('LSFORISK        ',LSFORISK(IY:IJUMPYR),Num_to_read,1,1)
    CALL GETRNGI('SHIPHISTYR      ',SHIPHISTYR,1,1,1)
    CALL GETRNGR('DSHIST_TONMI    ',DSHIST_TONMI(iy:SHIPHISTYR,1:9),SHIPHISTYR-iy+1,MNUMCR-2,1)
    CALL GETRNGR('DTM_SHARES      ',DTM_SHARES (1:9,1:16),MNUMCR-2,SEC-2,1)

    CALL GETRNGR('ANN_DECLINE     ',ANN_DECLINE(iy:IJUMPYR),Num_to_read,1,1)
    CALL GETRNGR('DSEFF           ',DSEFF(iy:IJUMPYR),Num_to_read,1,1)
    CALL GETRNGR('HTDSEFF         ',HTDSEFF(iy:IJUMPYR),Num_to_read,1,1)

    CALL GETRNGR('FUELCONS        ',FUELCONS,MNUMCR-2,9,1)
    CALL GETRNGR('FLEETTO         ',FLEETTO,1,9,1)
    CALL GETRNGR('EFFINC          ',EFFINC,1,9,1)
    CALL GETRNGR('Q_SHIPBUNKDS    ',Q_SHIPBUNKDS(iy:IJUMPYR),Num_to_read,1,1)
	
!...Connected and Autonomous freight assumptions
    CALL GETRNGI('TOONYEAR        ',TOONYEAR,1,1,1)
    CALL GETRNGR('TOONSHR         ',TOONSHR(TOONYEAR-1:MNUMYR),1,MNUMYR-TOONYEAR+2,1)    

!============ Freight truck technology adoption parameters and assumptions ============

    CALL ReadRngXLSX(WKUNIT,'tech_adopt') ! read range names & corresponding data from worksheet "tech_adopt"

!..."maximum adoption" S-curve assumptions for CNG, BEV, and FCV market penetration
    CALL GETRNGR('PRAFDFXG        ',PRAFDFXG,SC4,FUEL12,1)
    CALL GETRNGI('CYAFVXGF        ',CYAFVXG(1,1,FLT),SC4,FUEL12,1)
    CALL GETRNGI('CYAFVXGN        ',CYAFVXG(1,1,NFT),SC4,FUEL12,1)
    CALL GETRNGI('TRGSHXGF        ',TRGSHXG(1,1,FLT),CAFE19,FUEL12,1)
    CALL GETRNGI('TRGSHXGN        ',TRGSHXG(1,1,NFT),CAFE19,FUEL12,1)
    CALL GETRNGR('BFSHXGF         ',BFSHXG(1,1,FLT),SC4,FUEL12,1)
    CALL GETRNGR('BFSHXGN         ',BFSHXG(1,1,NFT),SC4,FUEL12,1)
    CALL GETRNGR('EFSHXGF         ',EFSHXG(1,1,FLT),SC4,FUEL12,1)
    CALL GETRNGR('EFSHXGN         ',EFSHXG(1,1,NFT),SC4,FUEL12,1)

!...Scedes switch overrides trnhdvx setting for s-curve time-to-asymptote
    if (RTOVALUE('TRANEFF ',0).eq.3) CYAFVXG(:,:,:) = CYAFVXG(:,:,:) + 5

!...natural gas vehicle sensitivity case
    CALL GETRNGR('CSTDXG          ',CSTDXG,SC4,FLT,1)
    CALL GETRNGR('CSTDVXG         ',CSTDVXG,SC4,FLT,1)
	
!...Data inputs for fuel choice model
!	VMT bin information (avg. annual VMT, and share of vehicle stock, within each VMT bin)
	CALL GETRNGR('payback_shr     ',PBACK_SHR,PBK_YR,CAFE19,1)
	
	DO icafe19 = 1, CAFE19
	  IF (icafe19.le.2) then												! Class 2b
	    CALL GETRNGR('VMT_VEH_2       ',VMT_VEH(1:nvmt,1:2,icafe19),11,FLT,1)
		CALL GETRNGR('VEH_SHR_2       ',VEH_SHR(1:nvmt,1:2,icafe19),11,FLT,1)
	  ELSEIF (icafe19.le.4) then											! Class 3
		CALL GETRNGR('VMT_VEH_3       ',VMT_VEH(1:nvmt,1:2,icafe19),11,FLT,1)
		CALL GETRNGR('VEH_SHR_3       ',VEH_SHR(1:nvmt,1:2,icafe19),11,FLT,1)
	  ELSEIF (icafe19.le.7) then											! Class 4-6
	    CALL GETRNGR('VMT_VEH_46      ',VMT_VEH(1:nvmt,1:2,icafe19),11,FLT,1)
	    CALL GETRNGR('VEH_SHR_46      ',VEH_SHR(1:nvmt,1:2,icafe19),11,FLT,1)
	  ELSEIF (icafe19.eq.8.or.icafe19.eq.12) then							! Class 7&8 vocational
		CALL GETRNGR('VMT_VEH_78V     ',VMT_VEH(1:nvmt,1:2,icafe19),11,FLT,1)
        CALL GETRNGR('VEH_SHR_78V     ',VEH_SHR(1:nvmt,1:2,icafe19),11,FLT,1)
	  ELSE																	! Class 7&8 tractor
        CALL GETRNGR('VMT_VEH_78      ',VMT_VEH(1:nvmt,1:2,icafe19),11,FLT,1)
	    CALL GETRNGR('VEH_SHR_78      ',VEH_SHR(1:nvmt,1:2,icafe19),11,FLT,1)
      ENDIF
	ENDDO

	CALL GETRNGR('max_drop        ',max_drop,1,1,1)					! Maximum YoY decrease in alt-fuel powertrain market share 	
	CALL GETRNGR('max_rise        ',max_rise,1,1,1)					! Maximum YoY increase in alt-fuel powertrain market share 	


!	CNG, BEV, FCEV/FCHEV, HEV, H2ICE component data and cost parameters
	CALL GETRNGR('sales_tax_rate  ',sales_tax_rate,1,9,1)						! Truck sales weighted average state sales tax (9 CDs)
	CALL GETRNGR('fet_rate        ',fet_rate,1,CAFE19,1)						! Federal excise tax (FET) rate (Class 8 only)
    CALL GETRNGR('PT_emotorkW     ',PT_emotorkW,CAFE19,FUEL12,1)                ! Electric motor size for electrified powertrains
	CALL GETRNGR('PT_battkWh_HEV  ',PT_battkWh_HEV,1,CAFE19,1) 				    ! Battery size for HEVs
    CALL GETRNGR('PT_tanksize_CNG ',PT_tanksize_CNG,11,CAFE19,1)				! CNG tank size
	CALL GETRNGR('PT_battkWh_FCEV ',PT_battkWh_FCEV,11,CAFE19,1) 				! Battery size for FCEVs
	CALL GETRNGR('PT_fckW_FCEV    ',PT_fckW_FCEV,11,CAFE19,1) 					! Fuel cell stack size for FCEVs
	CALL GETRNGR('PT_battkWh_FCHEV',PT_battkWh_FCHEV,11,CAFE19,1) 				! Battery size for FCHEVs
	CALL GETRNGR('PT_fckW_FCHEV   ',PT_fckW_FCHEV,11,CAFE19,1) 					! Fuel cell stack size for FCHEVs
	CALL GETRNGR('cost_OBC        ',cost_OBC(2022-1989:MNUMYR),1,29,1)			! On-board charger cost (2022USD)
	CALL GETRNGR('evse_maint      ',evse_maint,1,1,1)							! EVSE maintenance cost (2022USD/kWh)
	CALL GETRNGR('insure_rate     ',insure_rate,1,1,1)							! Insurance rate (%) applied to incremental cost for alt-fuel powertrains
	CALL GETRNGR('MR_slope        ',MR_slope,1,CAFE19,1) 						! Parameter for maintenance/repair (slope), 2022USD
	CALL GETRNGR('MR_intercept    ',MR_intercept,1,CAFE19,1) 					! Parameter for maintenance/repair (intercept), 2022USD
	CALL GETRNGR('MR_MGmult       ',MR_MGmult,1,1,1)							! Gasoline multiplier on diesel ICE M&R $/mile
	CALL GETRNGR('MR_BEVmult      ',MR_BEVmult(1:CAFE19,33:MNUMYR),CAFE19,29,1)	! BEV multiplier on diesel ICE M&R $/mile
	CALL GETRNGR('MR_FCVmult      ',MR_FCVmult(1:CAFE19,33:MNUMYR),CAFE19,29,1)	! FCV multiplier on diesel ICE M&R $/mile

    CALL GETRNGR('h2_max_tanks    ',h2_max_tanks,CAFE19,3,1)                    ! Maximum number of tanks that can be fit on a vehicle
    CALL GETRNGR('h2_kg_per_tank  ',h2_kg_per_tank,CAFE19,3,1)                  ! kg per tank
	CALL GETRNGR('h2_refuel_time  ',h2_refuel_time,1,1,1)						! Time required to refuel 
	CALL GETRNGR('refuel_timeval  ',refuel_timeval,1,1,1)						! Value of time (labor + lost freight revenue) per hour
	CALL GETRNGR('usable_h2       ',usable_h2,1,1,1)						    ! Ratio of usable h2 to nominal h2 (for a tank)

	CALL GETRNGR('TRK_BEV_DOD     ',TRK_BEV_DOD,1,1,1)						    ! Depth of discharge allowed (for converting nominal to usable batt kWh)
	CALL GETRNGR('batt_enden_imp  ',batt_enden_imp(34:61),1,2050-2022,1)        ! Battery energy density improvement (for estimating maximum installable batt kWh)
	CALL GETRNGR('charging_speed  ',charging_speed,1,CAFE19,1)                  ! Public charging speed
	CALL GETRNGR('max_kWh_cap     ',max_kWh_cap(1:CAFE19,2027-1989),1,CAFE19,1) ! Maximum installable battery capacity (kWh, MY2027)

    DO i = 2023,MNUMYR+1989
      DO icafe19 = 1, CAFE19
        max_kWh_cap(icafe19,i-1989) = max_kWh_cap(icafe19,2027-1989) * batt_enden_imp(i-1989) / batt_enden_imp(2027-1989)
      ENDDO
!      WRITE(21,'(a,",",i4,19(",",f6.1))')'max_kwh_cap',i,max_kWh_cap(:,i-1989)
    ENDDO
    
	CALL GETRNGR('battsize_a_PHEV ',battsize_a_PHEV,1,CAFE19,1)					! Coefficient for battery sizing (based on daily VMT requirement from VMT_VEH)
	CALL GETRNGR('battsize_b_PHEV ',battsize_b_PHEV,1,CAFE19,1)					! Coefficient for battery sizing (based on daily VMT requirement from VMT_VEH)

	CALL GETRNGR('NG_priceadj     ',NG_priceadj,1,2,1)							! Multiplier to adjust supply module NG costs up to actual {1: CNG, 2: LNG}
	CALL GETRNGR('cost_ICE        ',cost_ICE(1:CAFE19,BSYR_STK-1989),1,CAFE19,1)! Incremental cost of ICE engine (incl. non-batt/FC/H2 components added back in)

	CALL GETRNGR('cost_ice_CNG    ',cost_ice_CNG,1,CAFE19,1)					! Incremental cost of NG engine and aftertreatment (v. gasoline [2b-3] or diesel [4-8])
	CALL GETRNGR('cost_NGtank_DGE ',cost_NGtank_DGE(1:SC4,25:MNUMYR),SC4,37,1)	! CNG tank cost ($/DGE)
	CALL GETRNGR('cost_ICE_HEV    ',cost_ICE_HEV(1:CAFE19,BSYR_STK-1989),1,CAFE19,1)					! Total cost savings for removing non-HEV ICE components and adding HEV ICE components (can be negative)
	CALL GETRNGR('cost_ICE_H2ICE  ',cost_ICE_H2ICE(1:CAFE19,BSYR_STK-1989),1,CAFE19,1)					! Total cost savings for removing non-H2ICE ICE components and adding H2ICE components (can be negative) (not incl. h2 tank) 
    CALL GETRNGR('cost_ICE_PHEV   ',cost_ICE_PHEV(1:CAFE19,1:2,BSYR_STK-1989),CAFE19,2,1)					! Total cost savings for removing non-PHEV ICE components and adding PHEV ICE components (can be negative) {1: diesel, 2: gasoline)
	CALL GETRNGR('BEV_infra_cost  ',BEV_infra_cost(1:CAFE19,34),CAFE19,1,1)		! Upfront investment required for EVSE equipment and installation
	CALL GETRNGR('BEV_elec_markup ',BEV_elec_markup,1,CAFE19,1)					! Markup (multiplier) for retail/public electricity costs by size class
	CALL GETRNGR('cost_DEF        ',cost_DEF(2012-1989:MNUMYR),1,39,1)			! DEF per-gallon cost (2022USD)
	CALL GETRNGR('DEF_dose        ',DEF_dose,1,1,1)								! DEF dose rate (percent of diesel consumption)
	CALL GETRNGR('DEF_dose_LoNOx  ',DEF_dose_LoNOx,1,1,1)						! DEF dose rate (percent of diesel consumption); MY2027+, Low NOx rule

!	IRA tax credits
	CALL GETRNGI('IRA_switch      ',IRA_switch,1,1,1)
	CALL GETRNGR('IRA_45W_max     ',IRA_45W_max,CAFE19,FUEL12,1)				! Maximum allowable IRA 45W tax credit (2022USD)
	CALL GETRNGR('EVSE_30Ctaxcred ',EVSE_30Ctaxcred,1,1,1)						! Average per-EVSE tax credit (%)

	IF (IRA_switch.eq.0.or.RTOVALUE('LEGIRA  ',0).eq.0) then
	  IRA_45W_max(:,:) = 0.0
	  EVSE_30Ctaxcred = 0.0
	ENDIF

!	EPA MY2027 Low NOx rule compliance costs
	CALL GETRNGI('NOX_switch      ',NOX_switch,1,1,1)
	CALL GETRNGR('cost_LoNOx      ',cost_LoNOx,CAFE19,FUEL12,1)						! Low NOx compliance cost{1: diesel, 2: gasoline}

!...Scedes switch overrides trnhdvx setting for NOCAFE, ACT, and LowNOx
    if (RTOVALUE('TRANEFF ',0).eq.3) NOX_switch = 0

    CALL GETRNGR('cost_battpack_a ',cost_battpack_a,1,5,1)						! Different parameters for Class 2b-5 (1), Class6-8V (2), and Class 7&8T (3)
    CALL GETRNGR('cost_battpack_lr',cost_battpack_lr,1,5,1)
    CALL GETRNGR('cost_battmat_a  ',cost_battmat_a,1,5,1)
    CALL GETRNGR('cost_battmat_lr ',cost_battmat_lr,1,5,1)
	CALL GETRNGR('trk_battc_hist  ',trk_battc_hist,1,5,1)						! Ratio of freight truck battery prices to LDV (EV300) battery price, for filling in frt trk prices
    CALL GETRNGR('cost_h2tank_a   ',cost_h2tank_a,1,1,1) 						! Initial cost ($/kg) for H2 tanks
	CALL GETRNGR('cost_h2tank_lr  ',cost_h2tank_lr,1,1,1) 						! Learning rate for H2 Tank cost
	CALL GETRNGR('cost_fcstack_a  ',cost_fcstack_a,1,1,1) 						! Initial cost ($/kW) for fuel cell stack
	CALL GETRNGR('cost_fcstack_lr ',cost_fcstack_lr,1,1,1) 						! Learning rate for fuel cell stack cost
	CALL GETRNGR('cost_emotor_a   ',cost_emotor_a,1,1,1) 						! Initial cots ($/kW) for motors
	CALL GETRNGR('cost_fcstk_rd   ',cost_fcstk_rd,1,1,1) 						! R&D YoY cost decline for FC stacks
	CALL GETRNGR('cost_h2tank_rd  ',cost_h2tank_rd,1,1,1) 						! R&D YoY cost decline for FC tanks

	CALL GETRNGR('cost_h2tank_init',cumulative_h2_tanks(33),1,1,1)  			! Initial count of H2 tanks
	CALL GETRNGR('cost_fcstk_init ',cumulative_fc_stacks(33),1,1,1) 			! Initial count of fuel cell stacks

	CALL GETRNGR('CHRG_EFF        ',CHRG_EFF(2024-1989),1,1,1) 					! EVSE efficiency
	CALL GETRNGR('CHRG_EFF_IMP    ',CHRG_EFF_IMP,1,1,1) 						! EVSE efficiency, annual improvement
	CALL GETRNGR('H2ICE_fueldisc  ',H2ICE_fueldisc,1,1,1) 						! Discount applied to H2 fuel for H2 ICE (less purity needed)

	CALL GETRNGR('phev_mpg_adj    ',phev_mpg_adj,1,1,1)							! PHEV mpg is a combination of BEV and HEV mpg -- this multiplier reduces that combined mpg due to add'l weight of two powertrains
	CALL GETRNGR('phev_chgsus_mult',phev_chgsus_mult,CAFE19,2,1)				! Applied to conventional DS/MG fuel economy to estimate charge-sustaining PHEV mpg
	CALL GETRNGR('phev_dod        ',TRK_PHEV_DOD,1,1,1)						    ! Depth of discharge allowed (for converting nominal to usable batt kWh)

!	Set learning rate parameter (b)
	cost_fcstack_b    = -Log(1.0-(cost_fcstack_lr))/Log(2.0)
	cost_h2tank_b  	  = -Log(1.0-(cost_h2tank_lr))/Log(2.0)
	cost_battpack_b(:)= -Log(1.0-(cost_battpack_lr(:)))/Log(2.0)
	cost_battmat_b(:) = -Log(1.0-(cost_battmat_lr(:)))/Log(2.0)

!   Estimate historical H2 tank and FC stack production b/w 2022 (first year for cost curve) and last historical stock year	
	do i = 34, BSYR_STK-1989
      cumulative_h2_tanks(i) = cumulative_h2_tanks(33) * 1.1 ** (i-33)
      cumulative_fc_stacks(i) = cumulative_fc_stacks(33) * 1.05 ** (i-33)
    enddo

!   Fill in historical truck battery prices based on ratio to LDV price
    do i = 6, BSYR_STK-1989
      do j = 1, 5
        cost_battkWh_trk(j,i) = Li_ion_Cost(15,i+1989) * trk_battc_hist(j)
      enddo
    enddo

!	Historical prices are set equal to the last historical year
    cost_FCkW(6:BSYR_STK-1989) 	 = (cost_fcstack_a *(cumulative_fc_stacks(BSYR_STK-1989) ** (-cost_fcstack_b)))/ mc_jpgdp(31) * mc_jpgdp(1)
	cost_h2tank(6:BSYR_STK-1989) = (cost_h2tank_a  *(cumulative_h2_tanks(BSYR_STK-1989)  ** (-cost_h2tank_b))) / mc_jpgdp(31) * mc_jpgdp(1)
	cost_motorkW(6:BSYR_STK-1989)= (cost_emotor_a) / mc_jpgdp(31) * mc_jpgdp(1)

!	Convert all to 1990USD for choice model
!   Incremental ICE cost (engine + gearbox + mech-acc + alternator + 12V batt - elec-acc - power electronics - gearbox_ev)
	cost_ice_CNG(:)	  		        = cost_ice_CNG(:) / mc_jpgdp(2022-1989) * mc_jpgdp(1)
	cost_NGtank_DGE(:,:)	        = cost_NGtank_DGE(:,:) / mc_jpgdp(2009-1989) * mc_jpgdp(1)
	cost_ICE(:,BSYR_STK-1989)       = cost_ICE(:,BSYR_STK-1989) / mc_jpgdp(2023-1989) * mc_jpgdp(1)
	cost_ICE_PHEV(:,:,BSYR_STK-1989)= cost_ICE_PHEV(:,:,BSYR_STK-1989) / mc_jpgdp(2023-1989) * mc_jpgdp(1)
	cost_ICE_HEV(:,BSYR_STK-1989)   = cost_ICE_HEV(:,BSYR_STK-1989) / mc_jpgdp(2023-1989) * mc_jpgdp(1)
    cost_ICE_H2ICE(:,BSYR_STK-1989) = cost_ICE_H2ICE(:,BSYR_STK-1989) / mc_jpgdp(2023-1989) * mc_jpgdp(1)
    cost_OBC(:)   	  		        = cost_OBC(:) / mc_jpgdp(2022-1989) * mc_jpgdp(1)
	cost_DEF(:)   	  		        = cost_DEF(:) / mc_jpgdp(2022-1989) * mc_jpgdp(1)
	evse_maint 	  	  		        = evse_maint  / mc_jpgdp(2022-1989) * mc_jpgdp(1)
	MR_slope(:)   	  		        = MR_slope(:) / mc_jpgdp(2022-1989) * mc_jpgdp(1)
	MR_intercept(:)	  		        = MR_intercept(:) / mc_jpgdp(2022-1989) * mc_jpgdp(1)
	cost_LoNOx(:,:)	  		        = cost_LoNOx(:,:) / mc_jpgdp(2022-1989) * mc_jpgdp(1)
    refuel_timeval                  = refuel_timeval / mc_jpgdp(2022-1989) * mc_jpgdp(1)

!	Populate maintenance & repair cost for payback model; Write to TRNOUT
!	WRITE(21,*)'Maintenance and repair cost by fuel {1:DS,2:MG,6:BEV,9:FCV}'
!	WRITE(21,*)'Year,fuel,vintage,cost (2022USD/mile)'
	DO i = iy, MNUMYR
	  DO icafe19 = 1, CAFE19
	    DO ifuel = 1, FUEL12
	      DO j = 1,PBK_YR
	        MR_cost(icafe19,ifuel,j,i) = MR_intercept(icafe19) + MR_slope(icafe19) * j
			IF (ifuel.eq.2.and.icafe19.lt.8) MR_cost(icafe19,ifuel,j,i) = MR_cost(icafe19,ifuel,j,i) * MR_MGmult    ! Class 6 and below -- gasoline lower M&R
			IF (ifuel.eq.6) MR_cost(icafe19,ifuel,j,i) = MR_cost(icafe19,ifuel,j,i) * MR_BEVmult(icafe19,i)
			IF (ifuel.eq.7) MR_cost(icafe19,ifuel,j,i) = (MR_cost(icafe19,1,j,i) + MR_cost(icafe19,6,j,i)) / 2		! Assume PHEV avg of DS/BEV
			IF (ifuel.eq.8) MR_cost(icafe19,ifuel,j,i) = (MR_cost(icafe19,2,j,i) + MR_cost(icafe19,6,j,i)) / 2		! Assume PHEV avg of MG/BEV
			IF (ifuel.eq.9.or.ifuel.eq.10) MR_cost(icafe19,ifuel,j,i) = MR_cost(icafe19,ifuel,j,i) * MR_FCVmult(icafe19,i)
            IF (ifuel.eq.11) MR_cost(icafe19,ifuel,j,i) = MR_cost(icafe19,2,j,i)        ! Gasoline HEV M&R equal to gasoline iCE
            IF (ifuel.eq.12) MR_cost(icafe19,ifuel,j,i) = (MR_cost(icafe19,1,j,i) + MR_cost(icafe19,2,j,i)) / 2        ! H2ICE M&R average of gas/diesel
			IF(icafe19.eq.CAFE19.and.i.gt.32) then
!			  IF (ifuel.eq.1.or.ifuel.eq.2.or.ifuel.eq.6.or.ifuel.eq.9) then
!			    WRITE(21,'(i4,",",i2,",",i2,19(",",f4.3))')i+1989,ifuel,j,MR_cost(:,ifuel,j,i)* mc_jpgdp(2022-1989) / mc_jpgdp(1)
!			  ENDIF
			ENDIF
		  ENDDO
		ENDDO
	  ENDDO
	ENDDO
	
!	BEV infrastructure installation cost (convert to 1990USD)
!	Apply IRA 30C alt-fuel infrastructure tax credit until sunset
	DO icafe19 = 1, CAFE19
	  BEV_infra_cost(icafe19,:) = BEV_infra_cost(icafe19,2023-1989)/mc_jpgdp(32)*mc_jpgdp(1)
	ENDDO
	BEV_infra_cost(:,2023-1989:2032-1989) = BEV_infra_cost(:,2023-1989:2032-1989) * (1 - EVSE_30Ctaxcred)
	
!	EVSE efficiency
!	WRITE(21,*)'Year, CHRG_EFF, CHRG_EFF_IMP, evse_maint (2022USD), insure_rate, cost_obc (2022USD)'
	DO i = iy,MNUMYR
	  IF (i.le.2024-1989) THEN
	    CHRG_EFF(i) = CHRG_EFF(2024-1989)
	  ELSEIF (i.le.2032-1989) THEN
	    CHRG_EFF(i) = CHRG_EFF(2024-1989) * CHRG_EFF_IMP ** (i - (2024-1989))
	  ELSE
	    CHRG_EFF(i) = CHRG_EFF(2032-1989)
	  ENDIF
!	  IF (i.gt.33) WRITE(21,'(i4,",",f4.2,",",F5.3,",",f6.4,",",f4.3)')i+1989, CHRG_EFF(i), CHRG_EFF_IMP, evse_maint * mc_jpgdp(2022-1989) / mc_jpgdp(1), &
!				 insure_rate
	ENDDO	

!... Share of PHEV EV VMT Share
    CALL GETRNGR('PHEVELECVMT     ',PHEVELECVMT,1,CAFE19,1)

!============ Input data for CA Advanced Clean Trucks (ACT) Rule ============

    CALL ReadRngXLSX(WKUNIT,'ACT') ! read range names & corresponding data from worksheet "ACT"
	CALL GETRNGR('act_req_2b3     ',act_req_2b3(1:MNUMCR-2,35:MNUMYR),MNUMCR-2,2050-2024+1,1)
	CALL GETRNGR('act_req_48v     ',act_req_48v(1:MNUMCR-2,35:MNUMYR),MNUMCR-2,2050-2024+1,1)
	CALL GETRNGR('act_req_78trac  ',act_req_78trac(1:MNUMCR-2,35:MNUMYR),MNUMCR-2,2050-2024+1,1)
	CALL GETRNGR('act_wgtmod      ',act_wgtmod,1,CAFE19,1)
	CALL GETRNGR('ZEVreqshr78     ',ZEVreqshr78,1,2,1)
	CALL GETRNGR('phev_factor     ',phev_factor,1,1,1)
	CALL GETRNGI('ACT_switch      ',ACT_switch,1,1,1)

!...Scedes switch overrides trnhdvx setting for NOCAFE, ACT, and LowNOx
    if (RTOVALUE('TRANEFF ',0).eq.3) ACT_switch = 0

!============ Input data for Phase 2 HDV GHG emissions regulations ============

    CALL ReadRngXLSX(WKUNIT,'Phase_2') ! read range names & corresponding data from worksheet "phase_2"
    WKUNIT = FILE_MGR('C',FNAME,NEW)  ! close trnhdvx.xlsx input file
    CLOSE(WKUNIT)	
	CALL GETRNGI('CAFE_P3_SWITCH  ',CAFE_P3_SWITCH,1,1,1)
    CALL GETRNGR('BASE_MPG_P2     ',BASE_MPG_P2,FUEL12,CAFE19,1)
    CALL GETRNGR('BASE_MKT_P2     ',BASE_MKT_P2,FUEL12,CAFE19,1)
    CALL GETRNGI('TECHFYR_P2      ',TECHFYR_P2(1:TECHP2,1:CAFE14,1:FUEL12),TECHP2,CAFE14,FUEL12)
    CALL GETRNGI('PAYBACK_P2      ',PAYBACK_P2,1,CAFE14,1)
    CALL GETRNGR('HDV_DF          ',HDV_DF,FUEL12,CAFE14,1)
    
    CALL GETRNGR('TECHEFF_P2      ',TECHEFF_P2,TECHP2,CAFE14,1)
    CALL GETRNGR('TECHCOST_P2     ',TECHCOST_P2,TECHP2,CAFE14,1)

    CALL GETRNGR('TECHVAR_P2      ',TECHVAR_P2,TECHP2,CAFE14,1)
    CALL GETRNGR('TECHMID_P2      ',TECHMID_P2,TECHP2,CAFE14,1)
    CALL GETRNGR('TECHSHAPE_P2    ',TECHSHAPE_P2,TECHP2,CAFE14,1)
    CALL GETRNGR('TECHBASE_P2     ',TECHBASE_P2(1:TECHP2,1:CAFE19,1:FUEL12),TECHP2,CAFE19,FUEL12)

    CALL GETRNGR('TECHMAX_P2      ',TECHMAX_P2,TECHP2,CAFE14,1)
    CALL GETRNGR('HDV_GHGSTD_CI   ',HDV_GHGSTD(1:CAFE14,28:61,1),CAFE14,34,1)
    CALL GETRNGR('HDV_GHGSTD_SI   ',HDV_GHGSTD(1:CAFE14,28:61,2),CAFE14,34,1)

!...Scedes switch overrides trnhdvx setting for NOCAFE, ACT, and LowNOx
    if (RTOVALUE('TRANEFF ',0).eq.3) CAFE_P3_SWITCH = 0
    
!   Use Phase 2 standards if running no cafe side case
    IF (CAFE_P3_SWITCH.eq.0) THEN
      CALL GETRNGR('HDV_GHGSTD_CI_NC',HDV_GHGSTD(1:CAFE14,28:61,1),CAFE14,34,1)
      CALL GETRNGR('HDV_GHGSTD_SI_NC',HDV_GHGSTD(1:CAFE14,28:61,2),CAFE14,34,1)
    ENDIF
    
    CALL GETRNGI('HDV_STD_MAP     ',HDV_STD_MAP,FUEL12,CAFE14,1)

    DO ifuel = 1, FUEL12
      DO icafe14 = 1, CAFE14
        if (ifuel.ge.3) HDV_GHGSTD(icafe14,:,ifuel) = HDV_GHGSTD(icafe14,:,HDV_STD_MAP(ifuel,icafe14))
      ENDDO
    ENDDO

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
	CALL GETRNGI('TECHLEARN_P2    ',TECHLEARN_P2,TECHP2,CAFE14,1)

    CALL GETRNGR('DISCRTXG_P2     ',DISCRTXG_P2,1,1,1)
    CALL GETRNGR('DISCRTXGL_P2    ',DISCRTXGL_P2,1,1,1)

!...Without underlying high-stringency policy push, fleets revert to higher discount rate
    if (RTOVALUE('TRANEFF ',0).eq.3) DISCRTXG_P2 = DISCRTXGL_P2
    
    CALL GETRNGR('EPA_PAYLOAD     ',EPA_PAYLOAD,CAFE14,1,1)
    CALL GETRNGR('gCO2_per_gal    ',GCO2_GAL,FUEL12,CAFE14,1)
    CALL GETRNGR('HDV_USEFULLIFE  ',HDV_USEFULLIFE,FUEL12,CAFE14,1)
    CALL GETRNGR('HDV_AFV_CREDITS ',HDV_AFV_CREDITS(1:FUEL12,2017-1989:2050-1989),FUEL12,34,1)

!... For Phase 2
	TECHCOST_P2(:,:) = TECHCOST_P2(:,:)/MC_JPGDP(2015-1989)*MC_JPGDP(1)  ! Convert tech costs from 2012$ to 1990$
	DO i = 1, 24
      LEARNING_P2(:,i) = LEARNING_P2(:,25)                              ! Learning curves start in 2014; set pre-2014 equal to 2014
    ENDDO
    
	 num_sup = 0
	 num_req = 0.
	 num_syn = 0.
    DO itechp2 = 1,techp2
!     Setup parameters for SUPERSEDES notes 
     IF(SUPERSCEDE_P2(itechp2).eq.1) then 	                ! If this tech supercedes others                       
	   num_sup = num_sup + 1                                ! assigning indices to each supercedes note
       SUPERSEDES_TECH(1,num_sup) = TECH_NUM(itechp2)       ! tech that supercedes
	   SUPERSEDES_tech(2,num_sup) = tech_sup(itechp2,1)     ! tech that is superceded
	   tech_cnt_sup(num_sup) = 2
      DO j = 2,9
	    IF(tech_sup(itechp2,j).eq.0) cycle
	    tech_cnt_sup(num_sup) = tech_cnt_sup(num_sup) + 1                       ! counting the number of techs that are superceded in this note
        SUPERSEDES_tech(tech_cnt_sup(num_sup),num_sup) = tech_sup(itechp2,j)    ! other techs that are superceded
      ENDDO 
	 ENDIF
	  
     IF(REQUIRED_P2(itechp2).eq.1) then                                     ! Setup parameters for REQUIRED notes
	   num_req = num_req +1
	   REQUIRES_tech(1,num_req) = TECH_NUM(itechp2)
       REQUIRES_tech(2,num_req) = TECH_REQ(itechp2,1)
	   tech_cnt_req(num_req) = 2
      DO j = 2,3
	    IF(tech_sup(itechp2,j).eq.0) cycle
	    tech_cnt_req(num_req) = tech_cnt_req(num_req) + 1
        REQUIRES_tech(tech_cnt_req(num_req),num_req) = TECH_REQ(itechp2,j)
      ENDDO 
	 ENDIF	
	 
     IF(SYNERGY_P2(itechp2).eq.1) then                                      ! Setup parameters for SYNERGY notes
	   num_syn = num_syn + 1
       SYNERGY_tech(1,num_syn) = tech_num(itechp2)
	   SYNERGY_tech(2,num_syn) = tech_syn(itechp2,1)
	   syn_per(2,num_syn) = tech_per(itechp2,1)
	 ENDIF
	ENDDO            ! End itechp2 loop	
	
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
	REAL CLS2bSTKHIST(AGE,6:22,6)				! Class 2b stock
	REAL CLS3STKHIST(AGE,6:22,6)				! Class 3 stock
	REAL CLS46STKHIST(AGE,6:22,6) 				! Class 4-6 stock
	REAL CLS78STKHIST(AGE,6:22,6)				! Class 7&8 stock
	REAL CLS2bSTKREGN(1836,YearsReadIn)         ! Class 2b stock by region                
    REAL CLS2bVSTKREGN(1836,YearsReadIn)        ! Class 2b vocational stock by region     
    REAL CLS3STKREGN(1836,YearsReadIn)          ! Class 3 stock by region                 
    REAL CLS3VSTKREGN(1836,YearsReadIn)         ! Class 3 vocational stock by region      
    REAL CLS4VSTKREGN(1836,YearsReadIn)         ! Class 4 vocational stock by region      
    REAL CLS5VSTKREGN(1836,YearsReadIn)         ! Class 5 vocational stock by region      
    REAL CLS6VSTKREGN(1836,YearsReadIn)         ! Class 6 vocational stock by region      
    REAL CLS7STKREGN(1836,YearsReadIn)          ! Class 7 stock by region                 
    REAL CLS7VSTKREGN(1836,YearsReadIn)         ! Class 7 vocational stock by region      
    REAL CLS8STKREGN(1836,YearsReadIn)          ! Class 8 stock by region                 
    REAL CLS8VSTKREGN(1836,YearsReadIn)         ! Class 8 vocational stock by region      
    REAL CLS8HSTKREGN(1836,YearsReadIn)         ! Class 8 super heavy trucks by region 
    REAL temp_stock_check                       ! TEMP
    REAL TEMP_STK

!...Reads stock data for the freight model from spreadsheet input file
    FNAME = 'TRNSTOCKX'
    WKUNIT = FILE_MGR('O',FNAME,NEW)   !open trnstockx.xlsx input file
    CALL ReadRngXLSX(WKUNIT,'HDV')      !read range names & coerresponding data from worksheet "HDV"
    WKUNIT = FILE_MGR('C',FNAME,NEW)   !close trnstockx.xlsx input file
    CLOSE(WKUNIT)
	
!...Historic stocks by vintage and fuel type
    CALL GETRNGR('CLS2bSTKHIST    ',CLS2bSTKHIST(1:34,6:22,1:6),AGE,17,6)
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

	TRKSTK(:,:,:,:,:) 		= 0.0
	TRKSTK_19R(:,:,:,:,:,:) = 0.0
	TRKSTK_19(:,:,:,:) 		= 0.0

!...Populate 2012+ stocks (regional). Non-fleet first, then fleet.
!	Note 
    DO m2 = 1,1836
      DO j2 = 1,YearsReadIn
        y2 = j2 + 22
        r2 = HDV_census(m2)
    	a2 = HDV_vintage(m2)
    	f2 = HDV_fuel(m2)
    	TRKSTK_19R(y2,1 ,a2,f2,1,r2) = CLS2bSTKREGN(m2,j2) *(1.0-FLTSHR_STK(y2,1)) 
    	TRKSTK_19R(y2,2 ,a2,f2,1,r2) = CLS2bVSTKREGN(m2,j2)*(1.0-FLTSHR_STK(y2,2)) 
    	TRKSTK_19R(y2,3 ,a2,f2,1,r2) = CLS3STKREGN(m2,j2)  *(1.0-FLTSHR_STK(y2,3)) 
    	TRKSTK_19R(y2,4 ,a2,f2,1,r2) = CLS3VSTKREGN(m2,j2) *(1.0-FLTSHR_STK(y2,4)) 
    	TRKSTK_19R(y2,5 ,a2,f2,1,r2) = CLS4VSTKREGN(m2,j2) *(1.0-FLTSHR_STK(y2,5)) 
    	TRKSTK_19R(y2,6 ,a2,f2,1,r2) = CLS5VSTKREGN(m2,j2) *(1.0-FLTSHR_STK(y2,6)) 
    	TRKSTK_19R(y2,7 ,a2,f2,1,r2) = CLS6VSTKREGN(m2,j2) *(1.0-FLTSHR_STK(y2,7)) 
    	TRKSTK_19R(y2,8 ,a2,f2,1,r2) = CLS7VSTKREGN(m2,j2) *(1.0-FLTSHR_STK(y2,8)) 
    	TRKSTK_19R(y2,9 ,a2,f2,1,r2) = CLS7STKREGN(m2,j2)  *(1.0-FLTSHR_STK(y2,9))  * base_mkt_p2(f2,9) 
    	TRKSTK_19R(y2,10,a2,f2,1,r2) = CLS7STKREGN(m2,j2)  *(1.0-FLTSHR_STK(y2,10)) * base_mkt_p2(f2,10)
    	TRKSTK_19R(y2,11,a2,f2,1,r2) = CLS7STKREGN(m2,j2)  *(1.0-FLTSHR_STK(y2,11)) * base_mkt_p2(f2,11)
    	TRKSTK_19R(y2,12,a2,f2,1,r2) = CLS8VSTKREGN(m2,j2) *(1.0-FLTSHR_STK(y2,12)) 
    	TRKSTK_19R(y2,13,a2,f2,1,r2) = CLS8STKREGN(m2,j2)  *(1.0-FLTSHR_STK(y2,13)) * base_mkt_p2(f2,13)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,14,a2,f2,1,r2) = CLS8STKREGN(m2,j2)  *(1.0-FLTSHR_STK(y2,14)) * base_mkt_p2(f2,14)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,15,a2,f2,1,r2) = CLS8STKREGN(m2,j2)  *(1.0-FLTSHR_STK(y2,15)) * base_mkt_p2(f2,15)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,16,a2,f2,1,r2) = CLS8STKREGN(m2,j2)  *(1.0-FLTSHR_STK(y2,16)) * base_mkt_p2(f2,16)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,17,a2,f2,1,r2) = CLS8STKREGN(m2,j2)  *(1.0-FLTSHR_STK(y2,17)) * base_mkt_p2(f2,17)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,18,a2,f2,1,r2) = CLS8STKREGN(m2,j2)  *(1.0-FLTSHR_STK(y2,18)) * base_mkt_p2(f2,18)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,19,a2,f2,1,r2) = CLS8HSTKREGN(m2,j2) *(1.0-FLTSHR_STK(y2,19)) 

        TRKSTK_19R(y2,1 ,a2,f2,2,r2) = CLS2bSTKREGN(m2,j2) * FLTSHR_STK(y2,1) 
    	TRKSTK_19R(y2,2 ,a2,f2,2,r2) = CLS2bVSTKREGN(m2,j2)* FLTSHR_STK(y2,2) 
    	TRKSTK_19R(y2,3 ,a2,f2,2,r2) = CLS3STKREGN(m2,j2)  * FLTSHR_STK(y2,3) 
    	TRKSTK_19R(y2,4 ,a2,f2,2,r2) = CLS3VSTKREGN(m2,j2) * FLTSHR_STK(y2,4) 
    	TRKSTK_19R(y2,5 ,a2,f2,2,r2) = CLS4VSTKREGN(m2,j2) * FLTSHR_STK(y2,5) 
    	TRKSTK_19R(y2,6 ,a2,f2,2,r2) = CLS5VSTKREGN(m2,j2) * FLTSHR_STK(y2,6) 
    	TRKSTK_19R(y2,7 ,a2,f2,2,r2) = CLS6VSTKREGN(m2,j2) * FLTSHR_STK(y2,7) 
    	TRKSTK_19R(y2,8 ,a2,f2,2,r2) = CLS7VSTKREGN(m2,j2) * FLTSHR_STK(y2,8) 
    	TRKSTK_19R(y2,9 ,a2,f2,2,r2) = CLS7STKREGN(m2,j2)  * FLTSHR_STK(y2,9)  * base_mkt_p2(f2,9) 
    	TRKSTK_19R(y2,10,a2,f2,2,r2) = CLS7STKREGN(m2,j2)  * FLTSHR_STK(y2,10) * base_mkt_p2(f2,10)
    	TRKSTK_19R(y2,11,a2,f2,2,r2) = CLS7STKREGN(m2,j2)  * FLTSHR_STK(y2,11) * base_mkt_p2(f2,11)
    	TRKSTK_19R(y2,12,a2,f2,2,r2) = CLS8VSTKREGN(m2,j2) * FLTSHR_STK(y2,12) 
    	TRKSTK_19R(y2,13,a2,f2,2,r2) = CLS8STKREGN(m2,j2)  * FLTSHR_STK(y2,13) * base_mkt_p2(f2,13)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,14,a2,f2,2,r2) = CLS8STKREGN(m2,j2)  * FLTSHR_STK(y2,14) * base_mkt_p2(f2,14)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,15,a2,f2,2,r2) = CLS8STKREGN(m2,j2)  * FLTSHR_STK(y2,15) * base_mkt_p2(f2,15)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,16,a2,f2,2,r2) = CLS8STKREGN(m2,j2)  * FLTSHR_STK(y2,16) * base_mkt_p2(f2,16)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,17,a2,f2,2,r2) = CLS8STKREGN(m2,j2)  * FLTSHR_STK(y2,17) * base_mkt_p2(f2,17)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,18,a2,f2,2,r2) = CLS8STKREGN(m2,j2)  * FLTSHR_STK(y2,18) * base_mkt_p2(f2,18)/(sum(base_mkt_p2(f2,13:18)))
    	TRKSTK_19R(y2,19,a2,f2,2,r2) = CLS8HSTKREGN(m2,j2) * FLTSHR_STK(y2,19) 
      ENDDO
    ENDDO

!	Re-distribute recent vintaged stock across fleet/non-fleet. We need to keep accurate accounting here, since the fleet/non-fleet distinction
!		drives the distribution of charging types for the total BEV stock.
!	For instance, in y2=2018, iage = 1 fleet split will be equal to FLTSHR_SALES(2018), since TFFXGRT(isc4,1) is just 0 (no transfers in the first year)
!	              in y2=2018, iage = 2 fleet split will be equal to FLTSHR_SALES(2017) - PRODUCT(1-TFFXGRT(isc4,1:2)), which accounts for 1 year of transfers.
!				  in y2=2018, iage = 3 fleet split will be equal to FLTSHR_SALES(2016) - PRODUCT(1-TFFXGRT(isc4,1:3)), which accounts for 2 year of transfers.
!	This assumes fleet and non-fleet scrappage are equal.
	DO y2 = 2016-1989, bsyr_stk-1989
      temp_stock_check = sum(TRKSTK_19R(y2,:,:,:,:,1:MNUMCR-2))
	  DO iregn = 1, MNUMCR-2
	    DO icafe19 = 1, CAFE19
		  isc4 = SC19Map(icafe19)
	      DO ifuel = 1, FUEL12
	  	    DO iage = 1, y2-(2016-1989)+1		! Only have fleet sales share back to 2016
              TEMP_STK = sum(TRKSTK_19R(y2,icafe19,iage,ifuel,:,iregn))
!              WRITE(21,'(a,5(",",i4),2(",",f12.0),2(",",f7.5))')'trkstkb4',y2+1989,iregn,icafe19,ifuel,iage,TRKSTK_19R(y2,icafe19,iage,ifuel,:,iregn),FLTSHR_SALES(y2-iage+1,icafe19),PRODUCT(1-TFFXGRT(2:iage,isc))
!			  IF (iage.eq.1) TRKSTK_19R(y2,icafe19,iage,ifuel,FLT,iregn) = sum(TRKSTK_19R(y2-iage+1,icafe19,1,ifuel,:,iregn)) * FLTSHR_SALES(y2-iage+1,icafe19)
			  IF (iage.eq.1) TRKSTK_19R(y2,icafe19,iage,ifuel,FLT,iregn) = sum(TRKSTK_19R(y2,icafe19,iage,ifuel,:,iregn)) * FLTSHR_SALES(y2-iage+1,icafe19)
			  IF (iage.gt.1) TRKSTK_19R(y2,icafe19,iage,ifuel,FLT,iregn) = sum(TRKSTK_19R(y2,icafe19,iage,ifuel,:,iregn)) * &
																		   (FLTSHR_SALES(y2-iage+1,icafe19) * PRODUCT(1-TFFXGRT(2:iage,isc4)))
			  TRKSTK_19R(y2,icafe19,iage,ifuel,NFT,iregn) = TEMP_STK - TRKSTK_19R(y2,icafe19,iage,ifuel,FLT,iregn)
!              WRITE(21,'(a,5(",",i4),2(",",f12.0),2(",",f7.5))')'trkstkaf',y2+1989,iregn,icafe19,ifuel,iage,TRKSTK_19R(y2,icafe19,iage,ifuel,:,iregn),FLTSHR_SALES(y2-iage+1,icafe19),PRODUCT(1-TFFXGRT(2:iage,isc))
			ENDDO
		  ENDDO
		ENDDO
	  ENDDO
      if (NINT(temp_stock_check).ne.NINT(sum(TRKSTK_19R(y2,:,:,:,:,1:MNUMCR-2)))) THEN
        WRITE(21,*)'WARNING:fleet dist',y2+1989,NINT(temp_stock_check),NINT(sum(TRKSTK_19R(y2,:,:,:,:,1:MNUMCR-2)))
        TRKSTK_19R(y2,:,:,:,:,1:MNUMCR-2) = TRKSTK_19R(y2,:,:,:,:,1:MNUMCR-2) * temp_stock_check/sum(TRKSTK_19R(y2,:,:,:,:,1:MNUMCR-2))
      endif
	ENDDO
	
	DO y2 = 2012-1989,bsyr_stk-1989
	  DO icafe19 = 1, CAFE19
	    DO iage = 1, age
	      DO ifuel = 1, FUEL12
	  	    DO iflt = 1, flt
	  	      TRKSTK_19R(y2,icafe19,iage,ifuel,iflt,11) = SUM(TRKSTK_19R(y2,icafe19,iage,ifuel,iflt,1:MNUMCR-2))
			ENDDO
		  ENDDO
		ENDDO
	  ENDDO
	ENDDO
		  
!...Summing the stock to match the reporting aggregation levels
    DO y2 = 2012-1989,bsyr_stk-1989
!	  by aggregate size class, no region
	  DO iage=1,age
        DO ifuel=1,FUEL12
   	      DO iflt = 1,FLT
            TRKSTK(y2,1,iage,ifuel,iflt) = sum(TRKSTK_19R(y2,3:4,iage,ifuel,iflt,11))
	  	    TRKSTK(y2,2,iage,ifuel,iflt) = sum(TRKSTK_19R(y2,5:7,iage,ifuel,iflt,11))
	  	    TRKSTK(y2,3,iage,ifuel,iflt) = sum(TRKSTK_19R(y2,8:19,iage,ifuel,iflt,11))
	  	  ENDDO
	    ENDDO
	  ENDDO	
!	  No region, no fleet type
	  DO iage=1,age
        DO ifuel=1,FUEL12
          DO icafe19 = 1,CAFE19
	  	    TRKSTK_19(y2,icafe19,iage,ifuel) = sum(TRKSTK_19R(y2,icafe19,iage,ifuel,1:2,11))
	  	  ENDDO
	    ENDDO
	  ENDDO
	ENDDO
	
	fuel_shr_regn(:,:,:,:,:) = 0.0
!...Fill historical new vehicle sales fuel shares, incl. estimate of share by vmt bin (assumes same powertrain dist across all VMT bins for hist)
	DO y2 = 2012-1989,bsyr_stk-1989
	  DO iregn = 1, MNUMCR-2
        DO icafe19 = 1, CAFE19
          DO iflt = 1, flt
              DO ifuel = 1, FUEL12
    	        fuel_shr_regn(y2,icafe19,ifuel,iflt,iregn) = TRKSTK_19R(y2,icafe19,1,ifuel,iflt,iregn)/sum(TRKSTK_19R(y2,icafe19,1,:,iflt,iregn))
    	        DO ivmt = 1, nvmt
				  fuel_shr_ivmt(y2,ivmt,iflt,icafe19,ifuel,iregn) = fuel_shr_regn(y2,icafe19,ifuel,iflt,iregn) * veh_shr(ivmt,iflt,icafe19)
			    ENDDO
			  ENDDO
          ENDDO
        ENDDO
	  ENDDO
    ENDDO  

!...Populate 1995-2011 stocks (not regional). Have to DO this after filling 2012+ above (TRKSTK_19R) since 2012 shares are used for voc/non-voc split in pre-2012.
    DO y2 = 6, 2011-1989
      DO isc=1,sc4
        DO iage=1,age
          DO ifuel=1,6
!           class 3
            IF(isc.eq.1)then
              TRKSTK(y2,isc,iage,ifuel,FLT) = CLS3STKHIST(iage,y2,ifuel) * (SUM(FLTSHR_STK(y2,3:4))/2)
			  TRKSTK(y2,isc,iage,ifuel,NFT) = CLS3STKHIST(iage,y2,ifuel) - TRKSTK(y2,isc,iage,ifuel,FLT)
!           class 4-6
            ELSEIF(isc.eq.2)then
              TRKSTK(y2,isc,iage,ifuel,FLT) = CLS46STKHIST(iage,y2,ifuel) * (SUM(FLTSHR_STK(y2,5:7))/3)
              TRKSTK(y2,isc,iage,ifuel,NFT) = CLS46STKHIST(iage,y2,ifuel) - TRKSTK(y2,isc,iage,ifuel,FLT)
!           class 7&8
            ELSEIF(isc.eq.3)then
              TRKSTK(y2,isc,iage,ifuel,FLT) = CLS78STKHIST(iage,y2,ifuel) * (SUM(FLTSHR_STK(y2,8:19))/12)
              TRKSTK(y2,isc,iage,ifuel,NFT) = CLS78STKHIST(iage,y2,ifuel) - TRKSTK(y2,isc,iage,ifuel,FLT)
 	   	  
!     		class 2b -- Estimate pre-2012 vocational/non-vocational split using 2012 stock distribution
            ELSE		! NOTE: CLTSTK uses 1:gasoline, 2:diesel (unlike other freight arrays 1:diesel, 2:gasoline)
!  	        diesel
              IF (ifuel.eq.1) then
      	        CLTSTK(y2,2,iage,1,11) = CLS2bSTKHIST(iage,y2,ifuel) * sum(TRKSTK_19R(2012-1989,1,iage,ifuel,:,1:MNUMCR-2)) / &			! TRKSTK_19R(iyr,icafe19,iage,ifuel,iflt,iregn)
      	      														   sum(TRKSTK_19R(2012-1989,1:2,iage,ifuel,:,1:MNUMCR-2))
      	        CLTSTK(y2,2,iage,2,11) = CLS2bSTKHIST(iage,y2,ifuel) - CLTSTK(y2,2,iage,1,11)
!      	      gasoline																					  
      	      ELSEIF (ifuel.eq.2) then
      	        CLTSTK(y2,1,iage,1,11) = CLS2bSTKHIST(iage,y2,ifuel) * sum(TRKSTK_19R(2012-1989,1,iage,ifuel,:,1:MNUMCR-2)) / &			! TRKSTK_19R(iyr,icafe19,iage,ifuel,iflt,iregn)
      	      														   sum(TRKSTK_19R(2012-1989,1:2,iage,ifuel,:,1:MNUMCR-2))
      	        CLTSTK(y2,1,iage,2,11) = CLS2bSTKHIST(iage,y2,ifuel) - CLTSTK(y2,1,iage,1,11)
!      	      other fuels -- use sum of non-diesel to estimate shares (in case of zeros for some alt-fuels)
!      	      all other fuel indices line up between Class 2b and 3-8
      	      ELSEIF (ifuel.gt.2) then
      	        CLTSTK(y2,ifuel,iage,1,11) = CLS2bSTKHIST(iage,y2,ifuel) * sum(TRKSTK_19R(2012-1989,1,iage,2:FUEL12,:,1:MNUMCR-2)) / &	! TRKSTK_19R(iyr,icafe19,iage,ifuel,iflt,iregn)
      	  	    													       sum(TRKSTK_19R(2012-1989,1:2,iage,2:FUEL12,:,1:MNUMCR-2))
      	        CLTSTK(y2,ifuel,iage,2,11) = CLS2bSTKHIST(iage,y2,ifuel) - CLTSTK(y2,ifuel,iage,1,11)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
	    
!       Populate Class 2b total stock by powertrain
	    DO ifuel = 1,FUEL12
          CLTSTKT(ifuel,y2) = sum(CLTSTK(y2,ifuel,1:34,:,11))/ 1000.0
        ENDDO
      ENDDO
	ENDDO
	
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
    REAL :: FUEL_VR(FUEL12,MNUMCR)

!...Benchmark VMT and FUEL for reporting using benchmark's determined in tran.f
    FUEL_VR(1,1:RGN) = BENDS(1:RGN,iyr)                                                                 ! Diesel
    FUEL_VR(2,1:RGN) = BENMG(1:RGN,iyr)                                                                 ! Motor Gasoline
    FUEL_VR(3,1:RGN) = BENLG(1:RGN,iyr)                                                                 ! Propane
    FUEL_VR(4,1:RGN) = BENNG(1:RGN,iyr)                                                                 ! Natural Gas
	FUEL_VR(5,1:RGN) = BENET(1:RGN,iyr)*PCTAF(2,1:RGN,iyr) + BENMG(1:RGN,iyr)*(1.0-PCTAF(2,1:RGN,iyr))  ! Ethanol
	FUEL_VR(6,1:RGN) = BENEL(1:RGN,iyr)                                                                 ! Electric
	FUEL_VR(7,1:RGN) = BENEL(1:RGN,iyr)*PctEVMT_PHEV_AVG(iyr,1:RGN,1) &									! PHEV Diesel
					 + BENDS(1:RGN,iyr)*(1 - PctEVMT_PHEV_AVG(iyr,1:RGN,1))
	FUEL_VR(8,1:RGN) = BENEL(1:RGN,iyr)*PctEVMT_PHEV_AVG(iyr,1:RGN,2) &									! PHEV Gasoline
					 + BENMG(1:RGN,iyr)*(1 - PctEVMT_PHEV_AVG(iyr,1:RGN,2))
	FUEL_VR(9,1:RGN) = BENHY(1:RGN,iyr)                                                                 ! Hydrogen
	FUEL_VR(10,1:RGN)= BENHY(1:RGN,iyr)																	! Hydrogen large batt	
	FUEL_VR(11,1:RGN)= BENMG(1:RGN,iyr)																	! Gasoline HEV	
	FUEL_VR(12,1:RGN)= BENHY(1:RGN,iyr)																	! Gasoline HEV	

    DO ifuel=1,FUEL12
      DO iregn=1,MNUMCR
        IF(FUEL_VR(ifuel,iregn).eq.0) FUEL_VR(ifuel,iregn)=1.0
      ENDDO
    ENDDO

    TEMP = 1000000.0
    TEMP_V = TEMP * 1000.0                      ! Scale for vmt

!...zero accumulating variables
    TFR_VMT_FAS_T(IYR,1:SC,1:FUEL12,:)=0.0
    TFR_FBTU_FAS_T(IYR,1:SC,1:FUEL12,:)=0.0

    TFR_VMT_FASF_T(IYR,1:SC,:)=0.0
    TFR_FBTU_FASF_T(IYR,1:SC,:)=0.0

    TFR_VMT_TR(IYR,:)=0.0
    TFR_FBTU_TR(IYR,:)=0.0

!...Benchmark Stock VMT
    DO isc=1,sc
      DO ifuel=1,FUEL12
        DO iflt=1,flt
          DO iregn=1,MNUMCR-2
            TFR_VMT_FAS_T(IYR,ISC,IFUEL,FNEW) = TFR_VMT_FAS_T(IYR,ISC,IFUEL,FNEW) + (VMTFLTR(CUR,ISC,FNEW,IFUEL,IFLT,iregn)*FUEL_VR(IFUEL,iregn)/TEMP_V)
            TFR_VMT_FASF_T(IYR,ISC,FNEW)      = TFR_VMT_FASF_T(IYR,ISC,FNEW) + (VMTFLTR(CUR,ISC,FNEW,IFUEL,IFLT,iregn)*FUEL_VR(IFUEL,iregn)/TEMP_V)
            TFR_VMT_TR(IYR,FNEW)              = TFR_VMT_TR(IYR,FNEW) + (VMTFLTR(CUR,ISC,FNEW,IFUEL,IFLT,iregn)*FUEL_VR(IFUEL,iregn)/TEMP_V)
            DO iage=1,age
              VMT_TMP = ((VMTFLTR(CUR,ISC,IAGE,IFUEL,IFLT,iregn)/TEMP_V)*FUEL_VR(IFUEL,iregn))
              TFR_VMT_FAS_T(IYR,ISC,IFUEL,FSTK) = TFR_VMT_FAS_T(IYR,ISC,IFUEL,FSTK) + VMT_TMP
              TFR_VMT_FASF_T(IYR,ISC,FSTK)      = TFR_VMT_FASF_T(IYR,ISC,FSTK) + VMT_TMP
              TFR_VMT_TR(IYR,FSTK)              = TFR_VMT_TR(IYR,FSTK) + VMT_TMP
            ENDDO
! ... Determine fuel consumption
            TFR_FBTU_FAS_T(IYR,ISC,IFUEL,FSTK)  = TFR_FBTU_FAS_T(IYR,ISC,IFUEL,FSTK) + ((1E-12)*HRATE(isc,ifuel))*FUEL_VR(IFUEL,iregn)*FUELDMDR(ISC,IFUEL,IFLT,iregn)
            TFR_FBTU_FASF_T(IYR,ISC,FSTK)       = TFR_FBTU_FASF_T(IYR,ISC,FSTK) + ((1E-12)*HRATE(isc,ifuel))*FUEL_VR(IFUEL,iregn)*FUELDMDR(ISC,IFUEL,IFLT,iregn)
            TFR_FBTU_TR(IYR,FSTK)               = TFR_FBTU_TR(IYR,FSTK) + ((1E-12)*HRATE(isc,ifuel))*FUEL_VR(IFUEL,iregn)*FUELDMDR(ISC,IFUEL,IFLT,iregn)
          ENDDO
        ENDDO
      ENDDO
	  
    ENDDO
        
    DO ISC = 1, SC
      DO IFUEL = 1, FUEL12
        TFR_TRK_FAS_T(IYR,ISC,IFUEL,FNEW) = SUM(TRKSTK(IYR,ISC,FNEW,IFUEL,:))/TEMP
        TFR_TRK_FAS_T(IYR,ISC,IFUEL,FSTK) = SUM(TRKSTK(IYR,ISC,:,IFUEL,:))/TEMP
      ENDDO
      TFR_TRK_FASF_T(IYR,ISC,FNEW)        = SUM(TRKSTK(IYR,ISC,FNEW,:,:))/TEMP
      TFR_TRK_FASF_T(IYR,ISC,FSTK)        = SUM(TRKSTK(IYR,ISC,:,:,:))/TEMP
      TFR_TRK_TR(IYR,FNEW)                = SUM(TRKSTK(IYR,:,FNEW,:,:))/TEMP
      TFR_TRK_TR(IYR,FSTK)                = SUM(TRKSTK(IYR,:,:,:,:))/TEMP
    ENDDO

!...Fill MPG for reporting
    DO ISC = 1,SC
      DO IFUEL = 1, FUEL12
!	    Store new vehicle MPG for fuels and size classes
        TFR_FTMPG(IYR,ISC,IFUEL,FNEW) = HDV_MPG(iyr,ISC,FNEW,IFUEL)		
!	    Average mpg over vintages
        TFR_FTMPG(IYR,ISC,IFUEL,FSTK) = harmonic_mean(HDV_MPG(iyr,ISC,1:AGE,IFUEL),VMTFLT_SF_TR(ISC,1:AGE,IFUEL,11),age)
	    IF(TFR_FTMPG(IYR,ISC,IFUEL,FSTK).eq.0) TFR_FTMPG(IYR,ISC,IFUEL,FSTK) = TFR_FTMPG(IYR-1,ISC,IFUEL,FSTK)
      ENDDO
! 	  Average MPG over fuel for new vehicles and the total stock
      DO IFNS = FNEW, FSTK
        TFR_FTMPG_S(IYR,ISC,IFNS) =harmonic_mean(TFR_FTMPG(IYR,ISC,1:FUEL12,IFNS),TFR_VMT_FAS_T(IYR,ISC,1:FUEL12,IFNS),FUEL12)
      ENDDO	
    ENDDO

!...Average MPG over Size Class
    DO IFNS = FNEW, FSTK
      TFR_FTMPG_TR(IYR,IFNS)=harmonic_mean(TFR_FTMPG_S(IYR,1:SC,IFNS),TFR_VMT_FASF_T(IYR,1:SC,IFNS),SC)
    ENDDO        ! IFNS   NEW/STOCK

!...Investment Calculation
    TFR_INVEST_TR(IYR) = TFR_TRK_FASF_T(IYR,1,FNEW) * 52500. + &
                         TFR_TRK_FASF_T(IYR,2,FNEW) * 52500. + &
                         TFR_TRK_FASF_T(IYR,3, FNEW) * 110000.

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
    REAL :: ISFDT(MNUMYR),ISFDT_B(MNUMYR)
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
	REAL :: price_input(5,MNUMCR,MNUMYR)	! fuel prices pre-convergence
    REAL :: temp_prhtr(MNUMCR,MNUMYR)       ! Imputed high-sulfur resid price for regions other than region 9


    AVEMEFF=(/0.50,0.48,0.28,0.50,0.48/)        ! multiple sources from Leidos marine fuel choice report
    ENPEN=(/0.0,0.02,0.0,0.0,0.0/)                ! penalty for scrubber with residuel fuel oil
    
!...calculate freight domestic marine freight parameter for latest FAF year
    IF(n.eq.iFAFyear) then
      DO iregn=1,MNUMCR-2
        DO isic=1,sic
          IF(TSIC(isic,iregn,iFAFyear).gt.0.) then
            DSTM_OUTPUT(iregn,isic)=DSHIST_TONMI(n,iregn)*DTM_SHARES(iregn,isic)/TSIC(isic,iregn,iFAFyear)   ! compute static domestic marine freight parameter (ton-miles per $ of industrial output)
          ELSE
            DSTM_OUTPUT(iregn,isic)=0.
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    
!...calculate freight domestic marine ton-miles
    DO iregn=1,MNUMCR-2
      IF(n.le.SHIPHISTYR)then
        STMTT(n,iregn)=DSHIST_TONMI(n,iregn)
      ELSE
        DO isic=1,sic
          DSADD_TONMI(n,iregn,isic)=TSIC(isic,iregn,n)*(DSTM_OUTPUT(iregn,isic)*(1.-ANN_DECLINE(n)))
        ENDDO
		STMTT(n,iregn)=sum(DSADD_TONMI(n,iregn,1:SIC))
      ENDIF
    ENDDO

!...overwrite STMTT(:,:) census division freight domestic marine ton-miles for short-term history (between iFAFyear and SHIPHISTYR)
    IF(n.ge.iFAFyear.and.n.le.SHIPHISTYR) then
      DO iregn=1,MNUMCR-2
        DO isic=1,sic
          DSADD_TONMI(n,iregn,isic)=TSIC(isic,iregn,n)*(DSTM_OUTPUT(iregn,isic)*(1.-ANN_DECLINE(n)))
        ENDDO
      ENDDO
      DO iregn=1,MNUMCR-2
        IF(sum(DSADD_TONMI(n,1:MNUMCR-2,1:SIC)).gt.0.) then
          STMTT(n,iregn)=sum(DSHIST_TONMI(n,1:MNUMCR-2))* &
                          (sum(DSADD_TONMI(n,iregn,1:SIC))/ &
                          sum(DSADD_TONMI(n,1:MNUMCR-2,1:SIC)))
        ELSE
          STMTT(n,iregn)=0.
        ENDIF
      ENDDO
    ENDIF
    STMTT(n,11)=sum(STMTT(n,1:MNUMCR-2))           !...compute a national value from the regional

!...calculate domestic marine energy consumption
    IF(n.gt.SHIPHISTYR)then
      IF(IFRTEFF.eq.2) DSEFF(n)=DSEFF(SHIPHISTYR)  !...freezes domestic shipping efficiency at last historic year
      IF(IFRTEFF.eq.1) DSEFF(n)=HTDSEFF(n)
    ENDIF
    DO iregn=1,MNUMCR
      TQDSHIPT(n,iregn)=STMTT(n,iregn)*DSEFF(n)
    ENDDO

!...calculate fuel use by domestic shipping
    DO iregn=1,MNUMCR
      DO MFTYPE=1,4
        TQDSHIPR(MFTYPE,iregn,n) = TQDSHIPT(n,iregn)*domship_fuel_shr(MFTYPE,n)
      ENDDO
    ENDDO

!   Calculate total energy consumption
!   Historical is based on the delta between SUM (SEDS resid (QRSDS) and EIA821 bunkered diesel (Q_SHIPBUNKDS)) and total domestic dmd (TQDSHIPR)
    IF (n.le.2020-1989) THEN
      ISFDT_B(N) = (QSRSTR(11,n) - SUM(TQDSHIPR(2,1:MNUMCR-2,n))) + (Q_SHIPBUNKDS(N) - SUM(TQDSHIPR(1,1:MNUMCR-2,n)))
!   Projection is estimated from growth in total exports (MC_XR) and import (MC_MR)
!   For 2021-2023, let energy grow 1:1 with trade (INTS_B=0)
    ELSEIF (n.le.2023-1989) THEN
      ISFDT_B(N) = ISFDT_B(N-1) * (MC_XR(N)+MC_MR(N)) / (MC_XR(N-1)+MC_MR(N-1))
!   For post-2021, assume continued efficiency improvements (tech, logistics) prevent and shift in value/ton of exports slows energy cons growth compared to trade
    ELSE
      ISFDT_B(N) = ISFDT_B(N-1) * (1 + ((MC_XR(N)+MC_MR(N)) / (MC_XR(N-1)+MC_MR(N-1))- 1.0) * 0.5)
    ENDIF

!    WRITE(21,'(a,2(",",i4),5(",",f9.1))')'ISFDT_B',curcalyr,curitr,ISFDT_B(N),QSRSTR(11,n),SUM(TQDSHIPR(2,1:MNUMCR-2,n)),Q_SHIPBUNKDS(N),SUM(TQDSHIPR(1,1:MNUMCR-2,n))

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

    IF(curcalyr.eq.2012) then
      DO iregn=1,MNUMCR-2
        !IF(curitr.eq.maxitr+1) write(21,'(a,i4,9(1x,f13.2))') '2012 eca baseline ', iregn, (fuelcons(iregn,mv),mv=1,vtype)
      ENDDO
    ENDIF

    ecafuelcons(:,n)=0.
    IF(curcalyr.ge.2012) then
      DO iregn=1,MNUMCR-2
        DO mv=1,vtype
          fleetsurv(mv)=max(0.,1.-((curcalyr-2012)*fleetto(mv)))
          meffinc(mv)=(1-effinc(mv))**((curcalyr-2012)*.5)
          ecademand=((fuelcons(iregn,mv)*fleetsurv(mv))+         &
            (fuelcons(iregn,mv)*(1.-fleetsurv(mv))*meffinc(mv)))* &
            geffects(mv,n)
            !IF(curitr.eq.maxitr+1) write(21,'(A,3I4,f12.3)') 'msi, ecademand,', curcalyr, iregn, mv, ecademand
          ecafuelcons(iregn,n)=ecafuelcons(iregn,n)+ecademand
        ENDDO
        !IF(curitr.eq.maxitr+1) write(21,'(a,2(1x,i4),9(1x,f6.4))') 'msi,fleetsurv,', curcalyr, iregn, (fleetsurv(mv),mv=1,vtype)
      ENDDO
      !IF(curitr.eq.maxitr+1) write(21,'(a,i4,9(",",1x,f12.3))') 'msi,ecafuelcons,',CURCALYR,(ecafuelcons(iregn,n),iregn=1,MNUMCR-2)
    ENDIF

!...Calculate international shipping fuel demand fuel shares occurring in North American Emission Control Areas (ECA)
!   These fuel shares are applied to the total int'l shipping fuel demand 

    w = 0.     ! initialize logit elements to zero
    alpha=-0.5  ! set the "default" alpha
    fltprof(:,:,n)=0.
    fltprof(5,:,31)=intship_fuel_shr(5,31)  !...initialize low sulfur fuel oil share
    fltprof(4,:,31)=intship_fuel_shr(4,31)  !...initialize LNG share
    fltprof(2,:,31)=intship_fuel_shr(2,31)  !...initialize Residual share
    fltprof(1,:,31)=intship_fuel_shr(1,31)  !...initialize Distillate share
    
    IF(curcalyr.le.2020) then 
      dspfac= 1.00                ! considered the premium ECA fuel pre-2019
      rfpfac= 1.05
      cngpfac=1.00
      lngpfac=1.15
    ELSE
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
    ENDIF 
    !IF(curitr.eq.maxitr+1) write(21,'(a,i4,10(",",1x,f13.4))') 'msi,price,',curcalyr,dspfac,(pdstr(iregn,n),iregn=1,MNUMCR-2)
    
    lsfo_upgrade(:,n)=0.0
    IF(curcalyr.gt.2020) then
      DO iregn=1,MNUMCR-2
        IF(prltr(iregn,n).gt.0.0) lsfo_upgrade(iregn,n)=(UpgradeCost/mc_jpgdp(11)*mc_jpgdp(1))+prltr(iregn,n)                ! UpgradeCost is a price adder for low sulfur fuel oil upgrade (2000$)
        w(1)=dspfac*max(1.,pdstr(iregn,n)/pgltrship(3,iregn,n))*(maxval(pdstr(iregn,n-2:n)))/(maxval(pdstr(iregn,26:27)))    ! indexed price of distillate (base year not = 1)
!       In several regions, high-sulfur resid pricing is highly volatile. Since region 9 has a stable price, use it's ratio
!       to low-sulfur resid price and apply it to the current region's low-sulfur price to get a more stable high-sulfur price
        if (iregn.eq.9) THEN
          w(2)=rfpfac*max(1.,prhtr(iregn,n)/pgltrship(3,iregn,n))*(maxval(prhtr(iregn,n-2:n)))/(maxval(prhtr(iregn,26:27)))    ! indexed price of high-sulfur residual fuel oil
        ELSE
          temp_prhtr(iregn,n) = prhtr(9,n)/prltr(9,n) * prltr(iregn,n)
          w(2)=rfpfac*max(1.,temp_prhtr(iregn,n)/pgltrship(3,iregn,n))*(maxval(temp_prhtr(iregn,n-2:n)))/(maxval(prhtr(iregn,26:27)))    ! indexed price of high-sulfur residual fuel oil
        ENDIF
        w(3)=cngpfac*pgftrship(3,iregn,n)/pgftrship(3,iregn,27)                                ! indexed price of cng
        w(4)=lngpfac*(maxval(pgltrship(3,iregn,n-2:n)))/(maxval(pgltrship(3,iregn,26:27)))     ! indexed price of lng
        w(5)=lsfopfac*max(1.,lsfo_upgrade(iregn,n)/pgltrship(3,iregn,n))*(maxval(lsfo_upgrade(iregn,n-2:n)))/(maxval(lsfo_upgrade(iregn,26:27)))    ! indexed price of upgraded low-sulfur fuel oil
!        IF(curitr.eq.maxitr+1) write(21,'(a,i4,5(",",1x,f13.4))') 'msi,w,',curcalyr,ecafuelcons(iregn,n),(w(mftype),mftype=1,4)

!		Grabbing pre-converged prices for offline Excel marine model checks
		IF(curitr.eq.maxitr) THEN		! No iterations - grab the raw "first guess" prices
	      price_input(1,iregn,n) = pdstr(iregn,n)
		  price_input(2,iregn,n) = prhtr(iregn,n)
		  price_input(3,iregn,n) = pgftrship(3,iregn,n)
		  price_input(4,iregn,n) = pgltrship(3,iregn,n)
		  price_input(5,iregn,n) = lsfo_upgrade(iregn,n)
		ENDIF

        lsum = 0.0
        DO mftype=1,4+1
          IF(w(mftype).le.0.05) w(mftype)=.05  ! prevent zero raised to negative exp (alpha) when prices set to zero
          lsum = lsum+(((w(mftype)**alpha)*fltprof(mftype,iregn,31))*avemeff(mftype))
        ENDDO
        IF(lsum.gt.0) then
          DO mftype=1,4+1
            fltprof(mftype,iregn,n)=((w(mftype)**alpha)*fltprof(mftype,iregn,31)*avemeff(mftype))/lsum
          ENDDO
        ENDIF
!       IF(curitr.eq.maxitr+1) write(21,'(a,i4,5(",",1x,f6.4))') 'msi,fltprof,',curcalyr,(fltprof(mftype,iregn,n),mftype=1,4),lsum
        ! normalize shares
        lsum=0.
        DO mftype=1,4+1
          lsum=lsum+fltprof(mftype,iregn,n)
        ENDDO
        IF((lsum.ne.0).and.(abs(lsum-1.).gt.0.001)) then
          DO mftype=1,4+1
            fltprof(mftype,iregn,n)=fltprof(mftype,iregn,n)/lsum
          ENDDO
!          IF(curitr.eq.maxitr+1) write(21,'(a,i4,6(",",1x,f6.4))') 'msi,fltprofnorm,',curcalyr,(fltprof(mftype,iregn,n),mftype=1,4+1),lsum
        ENDIF
      ENDDO
    ELSE
      DO iregn=1,MNUMCR-2
        IF(prltr(iregn,n).gt.0.0) lsfo_upgrade(iregn,n)=(UpgradeCost/mc_jpgdp(11)*mc_jpgdp(1))+prltr(iregn,n)    ! 0.31 is a price adder for low sulfur fuel oil upgrade (2000$)
      ENDDO
    ENDIF
	
!	write out prices (NOTE: post-model convergence)
	IF(curitr.eq.maxitr+1.and.n.eq.MNUMYR) THEN
	  DO mftype=1,MNUMYR			! Commandeer an index for year
	    DO iregn=1,MNUMCR-2
!	      WRITE(21,'(a,i4,",",i3,6(",",1x,f13.4))') 'mdr_prices, ',mftype+1989,iregn,UpgradeCost/mc_jpgdp(11)*mc_jpgdp(1),prltr(iregn,mftype),pdstr(iregn,mftype),prhtr(iregn,mftype), pgltrship(3,iregn,mftype),pgftrship(3,iregn,mftype)
!	      write(21,'(a,i4,",",i2,5(",",1x,f10.4))') 'MDR_prices,',mftype+1989,iregn,price_input(:,iregn,mftype)
		ENDDO
	  ENDDO
	ENDIF
	

!...calculate eca regional (census division) consumption and add to international shipping
    marfuel=0.
    DO iregn=1,MNUMCR-2
      DO mftype=1,4+1
        marfuel(mftype,iregn,n)=ecafuelcons(iregn,n)*fltprof(mftype,iregn,n)*(1+enpen(mftype))*.001
        !IF(curitr.eq.maxitr+1) write(21,'(a,3(",",1x,i4),1(",",1x,f12.3))') 'msi,tqishipr',curcalyr,mftype,iregn,tqishipr(mftype,iregn,n)
        !IF(curitr.eq.maxitr+1) write(21,'(a,3(",",1x,i4),1(",",1x,f12.3))') 'msi,marfuel',curcalyr,mftype,iregn,marfuel(mftype,iregn,n)
        !IF(curitr.eq.maxitr+1) write(21,'(a,3(",",1x,i4),1(",",1x,f12.3))') 'msi,tqishiprmar',curcalyr,mftype,iregn,tqishipr(mftype,iregn,n)
      ENDDO
!      IF(curitr.eq.maxitr+1) write(21,'(a,2(",",1x,i4),9(",",1x,f12.3))') 'msi,marfuel',curcalyr,iregn,(marfuel(mftype,iregn,n),mftype=1,5)
    ENDDO
    
! ... calculate the non-ECA international shipping fuel use split by the 4 fuel types:
! ...   1) distillate
! ...   2) residual
! ...   3) CNG
! ...   4) LNG

!...calculate fuel use by international shipping
    DO MFTYPE=1,4+1
      IF(curcalyr.le.2020) then
        isfd(MFTYPE,n) = isfdt_b(n) * intship_fuel_shr(MFTYPE,n)
      ELSE
        intship_fuel_shr(MFTYPE,n)=sum(marfuel(MFTYPE,1:MNUMCR-2,n))/sum(marfuel(:,1:MNUMCR-2,n)) ! overwrites: ECA consumption projects fuel share for int'l marine
        !IF(curcalyr.ge.2025) intship_fuel_shr(MFTYPE,n)=intshi_fuel_shr(MFTYPE,n-1)
        !IF(curitr.eq.maxitr+1) write(21,'(a,i4,i4,1(1x,f10.5))') 'msi marfuel share ', curcalyr, mftype, sum(marfuel(MFTYPE,1:MNUMCR-2,n))/sum(marfuel(1:4,1:MNUMCR-2,n))
        isfd(MFTYPE,n) = isfdt_b(n) * intship_fuel_shr(MFTYPE,n)
      ENDIF
    ENDDO
    
!    IF(curitr.eq.maxitr+1.and.curcalyr.eq.2050) then
!      DO mv=6,MNUMYR
!        write(21,'(a,i2,1x,5(1x,f8.5))') 'msi marfuel share ', mv, intship_fuel_shr(1,mv),intship_fuel_shr(2,mv),intship_fuel_shr(3,mv),intship_fuel_shr(4,mv),intship_fuel_shr(5,mv)
!      ENDDO
!    ENDIF

!...calculate regional consumption and adjust residual fuel oil to account for intermediate fuel oil (IFO) mixture
    DO iregn=1,MNUMCR-2
      DO MFTYPE=1,4+1
        reg_shr(iregn) = sedshrrs(iregn,n)
        IF(MFTYPE.eq.1) reg_shr(iregn) = sedshrds(iregn,n)
        tqishipr(MFTYPE,iregn,n) = isfd(MFTYPE,n) * reg_shr(iregn)
        IF(mftype.eq.2) then
          tqishipr(1,iregn,n)=tqishipr(1,iregn,n)+(tqishipr(mftype,iregn,n)*(1.-ifo))       ! distillate fuel oil amount, ifo mix
          tqishipr(mftype,iregn,n)=tqishipr(mftype,iregn,n)*ifo                             ! residual fuel oil amount, ifo mix
        ENDIF
        IF(mftype.eq.5) then
          tqishipr(1,iregn,n)=tqishipr(1,iregn,n)+tqishipr(mftype,iregn,n)*(1.-IMO2020)     ! now increase distillate fuel oil amount for blending
          tqishipr(mftype,iregn,n)=tqishipr(mftype,iregn,n)*IMO2020                         ! now decrease lsfo amount to agree with distillate blending

          tqishipr(1,iregn,n)=tqishipr(1,iregn,n)+(tqishipr(mftype,iregn,n)*(lsfoRisk(n)))  ! distillate fuel oil amount, risk of lsfo availability
          tqishipr(mftype,iregn,n)=tqishipr(mftype,iregn,n)*(1.-lsfoRisk(n))                ! lsfo amount, 2020 risk of lsfo availability
          ENDIF
      ENDDO
    ENDDO

    DO mftype=1,4+1
      tqishipr(mftype,11,n)=sum(tqishipr(mftype,1:MNUMCR-2,n))  ! sum to obtain national value, by fuel
      marfuel(mftype,11,n)=sum(marfuel(mftype,1:MNUMCR-2,n))    ! sum to obtain national value, by fuel
    ENDDO
!    write(21,'(a,3(",",1x,i4),9(",",1x,f12.3))') 'msi,tqishiprmar',curitr,curcalyr,iregn+1,(tqishipr(mftype,11,n),mftype=1,4+1)

    RETURN
    END SUBROUTINE TSHIP
