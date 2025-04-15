! *******************************************************************************
! **                                                                           **
! **  NEMS/WEPS Air travel model                                               **
! **                                                                           **
! *******************************************************************************

! NOTE on running NEMS v WEPS
! Be VERY careful making ANY changes to the model -- any change made to NEMS section must be copied to relevant WEPS sections and vice versa.
! To switch between NEMS and WEPS tranair, search for ALL instances of the string "NEMSWEPSSWITCH".
!   Comment out the model you don't want, uncomment the model you do. 
!	Some WEPS- or NEMS-specific variables are not marked for commenting. The model runs fine without switching them back and forth.

MODULE AIRMOD
! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
!use nemswk1
!use weps_xml
!INCLUDE 'Incl_NEMS.h90'
!INCLUDE 'Incl_tranvar.h90'
! WEPS section END -- NEMSWEPSSWITCH   ----------------------------------------------------------------------------------------------------------------

! NEMS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
INCLUDE 'parametr'
INCLUDE 'ncntrl'
INCLUDE 'apq'
INCLUDE 'macout'
INCLUDE 'convfact'
INCLUDE 'tranrep'
INCLUDE 'tranmain'
INCLUDE 'intout'
! NEMS section END -- NEMSWEPSSWITCH   ----------------------------------------------------------------------------------------------------------------

! ... Global definitions

! ... Switches for air transportation scenarios
INTEGER      RTOVALUE
EXTERNAL     RTOVALUE
INTEGER      TRANEFF
REAL         HIGHEFF                                          	! high efficiency
REAL         FRZNEFF                                          	! low efficiency

INTEGER       iy                                      		  	! year index corresponding to first year of inputs
INTEGER       Num_to_Read                                     	! number of years in input ranges
INTEGER       First_Read_Year                                 	! first year of data read from inputs
INTEGER       YRS                                             	! actual model year (1989+curiyr)
INTEGER       N                                               	! trans variable for curiyr
INTEGER       maxatyp,domint,dom,int                			! various parameters defined below
INTEGER       us,nus,maxwreg,maxareg,maxacage

INTEGER		  FIRST_FCST_YR, LAST_HIST_YR, FIRST_FCST_INDEX, LAST_HIST_INDEX

PARAMETER    (maxatyp     		= 3,           			&      	! number of aircraft types (narrow/wide/reg jet)
              domint      		= 2,           			&      	! dom=1, int=2    
              dom         		= 1,           			&      	! domestic (within a country)
              int         		= 2,           			&      	! international (outside of country)
              us          		= 1,           			&      	! us        
              nus         		= 2,           			&      	! non-us     
			  maxwreg			= 16,					&		!	Regions - Same as WEPS
																!	1  = USA
																!	2  = CAN
																!	3  = MXC
																!	4  = EUR
																!	5  = JPN
																!	6  = ANZ
																!	7  = SKO
																!	8  = RUS
																!	9  = URA
																!	10 = CHI
																!	11 = IND
																!	12 = OAS
																!	13 = MID
																!	14 = AFR
																!	15 = BRZ
																!	16 = CSA
              maxacage    		= 48)                           ! max aircraft age (47 years)
						
! ... Indices      
		   
INTEGER       AIRUNIT,          &							  	! Debug file for tranair (70)
              AGEUNIT,          &							  	! Debug file for tranair stock model (71)
              iwreg,            &                             	! world regions (maxwreg)
              iatyp,            &                             	! aircraft type (maxatyp)
              iage,             &                             	! aircraft age (maxacage)
              iregn,			&							  	! U.S. regions (mnumcr)
			  di											  	! domestic/international (domint)
	
CHARACTER*3   reg_def(maxwreg)
DATA          reg_def/'USA','CAN','MXC','EUR','JPN','ANZ','SKO','RUS','URA','CHI','IND','OAS','MID','AFR','BRZ','CSA'/

CHARACTER*2   reg_atyp(maxatyp)
DATA          reg_atyp/'NB','WB','RJ'/

! ... AIR DEMAND MODULE

! ... Air Energy Use
REAL          QJETR_NUS(maxwreg,mnumyr)                       	! total Non-U.S. jet fuel demanded (quads)
REAL          QJETR_US(mnumcr,mnumyr)                         	! total U.S. jet fuel consumption
REAL          QAGTR_US(mnumcr,mnumyr)                         	! total U.S. aviation gas consumption
! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
!REAL		  QJETR_DI(maxwreg,domint,2,mnumyr)					! total U.S. jet fuel consumption, disaggregate
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

! ... Aircraft Efficiency
REAL		  ASMPG_STK_TYP(maxatyp,mnumyr)					  	! Stock MPG by body type
REAL		  ASMPG_NEW_TYP(maxatyp,mnumyr)					  	! Sales MPG by body type
REAL		  ASMPG_VINT(maxwreg,maxatyp,maxacage,domint,mnumyr)! 
REAL		  GPTM(maxwreg,mnumyr,maxatyp,domint)
REAL          STK_ALIGN_MULT(maxwreg,mnumyr,maxatyp)            ! available seat miles per aircraft by type
REAL		  RPMPG(maxwreg,maxatyp,domint,mnumyr)
REAL		  ASMPG_AVG_AGE(maxwreg,maxatyp,domint,mnumyr)		! Available-seat-miles per gallon (incl. belly freight)
REAL		  ASMPGT(2,mnumyr)									! Available seat-miles per gallon, aggregate (1=new, 2=stock)
REAL          ASMAC(maxwreg,mnumyr,maxatyp)                   	! available seat miles per aircraft by type		  

! ... Payload conversions and shares
REAL		  PCT_PASS_MTOW(maxatyp,mnumyr)						! Percent of MTOW that is passenger (not incl. belly freight)
REAL	 	  PCT_BELLY_FRT(maxatyp,mnumyr,domint)				! Percent of total freight (belly + dedicated) that is belly
REAL		  PASS_WEIGHT										! Passenger weight assumption (incl. luggage) to convert between freight to seat-miles and passengers to ton-miles
REAL		  FUEL_BURN_RED										! Projected annual fuel burn reduction (per ton-mile), based on 50 year history (ICCT)
REAL		  PCT_BELLY_PLOAD(maxatyp,mnumyr,domint)			! Percent of passenger flight payload (pass + freight) that is freight (aka belly freight) 

! ... Revenue Passenger Miles (RPM)	
REAL          RPMT_HIST(maxwreg,mnumyr,domint)                	! hist revenue passenger miles by region, dom/int, billions
REAL          RPMT(maxwreg,domint,mnumyr)                     	! revenue passenger miles by region, dom/int, millions
REAL          RPMT_PC(maxwreg,domint,mnumyr)                  	! per capita revenue passenger miles by region, dom/int 
REAL          BASEYR_GDP_PC(maxwreg)                          	! per capita GDP   
REAL          RPM(maxwreg,domint,maxatyp,mnumyr)              	! revenue passenger miles by region, dom/int, aircraft type, millions
REAL		  AIR_MGMT_ADJ(maxwreg,mnumyr,domint)				! mileage adjustment for air traffic mgmt inefficiencies
REAL		  PASSAC_RPM_DMD(maxwreg,maxatyp,domint,mnumyr)	  	! RPM demand, passenger aircraft (including belly freight)
REAL    	  BELLY_RPM_EQ(maxwreg,maxatyp,domint,mnumyr)	  	! Equivalent RPMs of belly freight
REAL		  SHR_RPM(maxwreg,mnumyr,maxatyp,domint)		! RPM shares by domestic/int'l and body type, within each region
	
! ... Load Factors	
REAL          LOAD_FACTOR(maxwreg,mnumyr,domint)      			! load factor by region and body type
REAL          LFDOMAVG(mnumyr)                                	! average load factor for domestic travel
REAL          LFINTAVG(mnumyr)                                	! average load factor for international travel
	
! ... Available Seat Miles (ASM)	
REAL          ASM(maxwreg,domint,maxatyp,mnumyr)              	! available seat miles by region, dom/int, aircraft type
REAL          SMDEMD(maxwreg,mnumyr)                          	! total world available seat miles by region
	
! ... Aircraft Stocks
!     Totals
REAL          STKPA(maxwreg,mnumyr,maxatyp)         			! passenger aircraft total active stock history
REAL          STKPP(maxwreg,mnumyr,maxatyp) 					! passenger aircraft total parked stock history
REAL          STKPTOT(maxwreg,maxatyp,mnumyr)             		! passenger aircraft stock total by aircraft type
REAL          STKCA(maxwreg,mnumyr,maxatyp)         			! cargo aircraft total active stock history
REAL          STKCP(maxwreg,mnumyr,maxatyp) 					! cargo aircraft total parked stock history
REAL          STKCTOT(maxwreg,maxatyp,mnumyr)            		! total aircraft cargo stock by aircraft type
REAL          STK_SUP_NEW(maxwreg,mnumyr,maxatyp)               ! new aircraft sales by type
REAL		  MIN_PRK_SHR(maxwreg,maxatyp)						! Minimum required share of total passenger aircraft that must remain parked

!	  Vintaged
REAL          STKPAVINT(maxwreg,maxacage,maxatyp)             	! last hist yr active passenger aircraft stk by type and vintage
REAL          STKCAVINT(maxwreg,maxacage,maxatyp)            	! last hist yr active cargo aircraft stock by type and vintage
REAL          STKPPVINT(maxwreg,maxacage,maxatyp)            	! last hist yr parked passengeraircraft stock by type and vintage
REAL          STKCPVINT(maxwreg,maxacage,maxatyp)           	! last hist yr parked cargo act stock by type and vintage
REAL          STKCARGO(maxwreg,maxatyp,maxacage,mnumyr)        	! cargo aircraft stock by aircraft type and vintage
REAL          STKPASS(maxwreg,maxatyp,maxacage,mnumyr)        	! passenger aircraft stock by aircraft type and vintage

REAL		  STKCARGO_PARKED(maxwreg,maxatyp,maxacage,mnumyr)	! cargo aircraft parked stock, incl vintage
REAL		  STKCARGO_ACTIVE(maxwreg,maxatyp,maxacage,mnumyr)	! cargo aircraft active stock, incl vintage
REAL		  STKPASS_PARKED(maxwreg,maxatyp,maxacage,mnumyr)	! passenger aircraft parked stock, incl vintage
REAL		  STKPASS_ACTIVE(maxwreg,maxatyp,maxacage,mnumyr)	! passenger aircraft active stock, incl vintage

REAL          STK_SUP(maxwreg,maxatyp,maxacage,mnumyr)        	! aircraft stock (pass+cargo) by aircraft type and vintage
REAL          STK_SUP_TOT(maxwreg,maxatyp,mnumyr)             	! aircraft stock (pass+cargo) total by aircraft type
REAL          SURVAC(maxatyp,maxacage,2)                        ! aircraft survival curves by aircraft type, 1 = passenger, 2 = cargo

REAL		  STK_AVGAGE_PA(maxwreg,maxatyp,mnumyr)				! Average age of active passenger aircraft

! Annual tracking of scrapped, unparked, converted, and parked aircraft by region, aircraft type, passenger/cargo (1/2), and year
REAL    	  SCRAPPED_TOTAL(maxwreg,maxatyp,2,mnumyr) 			! Tally of scrapped aircraft 1 = passenger, 2 = cargo  [mainly for debug]
REAL    	  UNPARKED(maxwreg,maxatyp,2,mnumyr) 				! Tally of unparked aircraft 1 = passenger, 2 = cargo  [mainly for debug]
REAL    	  CONVERTED(maxwreg,maxatyp,2,mnumyr) 				! Tally of converted aircraft 1 = passenger, 2 = cargo  [mainly for debug]
REAL    	  PARKED(maxwreg,maxatyp,2,mnumyr) 					! Tally of parked aircraft 1 = passenger, 2 = cargo  [mainly for debug]

INTEGER*2	  MINAGE_PRK_F
INTEGER*2	  MINAGE_C_PPRK
INTEGER*2	  MINAGE_C_PACT
INTEGER*2	  MINAGE_PRK_P
INTEGER*2	  MAXAGE_UNPRK_P
REAL		  MAX_UNPRK_SHR
	
! ... Air Freight	
REAL		  SHR_FTM(maxwreg,mnumyr,maxatyp,domint)			! FTM shares by domestic/int'l and body type, within each region	
REAL          FTMAC(maxwreg,mnumyr,maxatyp)                   	! FTM per aircraft by type, thousands
REAL		  CARGOAC_FTM_DMD(maxwreg,maxatyp,domint,mnumyr)  	! FTM demand, cargo aircraft
REAL		  FTM_ALIGNHIST(maxwreg,domint)						! Delta b/w estimated FTM in Last hist year (from coefficients) and the actual last hist year value

! ... Yield	
REAL          YIELD(2,mnumyr)                                 	! historic revenue per passenger mile (1996 cents per mile)
REAL          YLD(2,mnumyr)                                   	! revenue per passenger mile
REAL          LCPMD                                           	! domestic yield lower bound (lowest cost per mile domestic)
REAL          LCPMI                                           	! international yield lower bound (lowest cost per mile international)
	
! ... Coefficients for air model	
	
! ... Domestic Yield 	
REAL          ALPHAYD                                         	! beta constant                                    
REAL          RHOYD                                           	! rho coefficient        
REAL          BETAFUELD                                       	! beta fuel price
REAL          BETATIMED                                       	! beta time
	
! ... International Yield	
REAL          ALPHAYI                                         	! beta constant
REAL          RHOYI                                           	! rho coefficient
REAL          BETAFUELI                                       	! beta fuel price
REAL          BETATIMEI                                       	! beta time
	
! ... RPM Demand
REAL		  intercept_rpm(maxwreg,domint)					  	! Intercept, RPM equation coefficients
REAL		  beta1_rpm(maxwreg,domint)						  	! GDP/capita elasticity of RPM/capita, RPM equation coefficients
REAL		  COVID_MULT(maxwreg,mnumyr,domint)					! Adjustment factor for covid impacts on rpm
REAL		  COVIDMULT_REF(maxwreg,mnumyr,domint)				! Adjustment factor for covid impacts on rpm, reference case
REAL		  COVIDMULT_LOMAC(maxwreg,mnumyr,domint)			! Adjustment factor for covid impacts on rpm, low macro case
REAL		  COVIDMULT_HIMAC(maxwreg,mnumyr,domint)			! Adjustment factor for covid impacts on rpm, high macro case
	
! ... Revenue Ton Miles (FTM) Demand
REAL		  intercept_ftm(maxwreg,domint)
REAL		  beta1_ftm(maxwreg,domint)								! gdp coefficient for FTM projection

! ... gdp and population for NEMS
REAL 		  WEPS_TRAN_GDP(maxwreg,mnumyr)	
REAL 		  WEPS_TRAN_POP(maxwreg,mnumyr)	

!===================================================================================
!     COMMON BLOCK
!===================================================================================
! to accomodate view variables in the Visual Studio, the variables are
! added to common blocks here.  Then the USE statement for module AIRMOD must go in every 
! subroutine.

  common/tranairreal/ HIGHEFF, FRZNEFF, QJETR_NUS, QJETR_US, QAGTR_US, &
  FIRST_FCST_YR, LAST_HIST_YR, FIRST_FCST_INDEX, LAST_HIST_INDEX, ASMPGT, ASMAC,  &
  RPMT_HIST, RPMT, RPMT_PC, BASEYR_GDP_PC, RPM, LOAD_FACTOR, LFDOMAVG, LFINTAVG,&
  ASM, SMDEMD, STK_SUP_NEW, STKPA,STKPP,STKCA,STKCP, STKPAVINT, STKCAVINT, STKPPVINT, STKCPVINT, &
  STKCARGO, SCRAPPED_TOTAL, STKCARGO_PARKED, STKCARGO_ACTIVE, STKPASS_PARKED, STKPASS_ACTIVE, MIN_PRK_SHR,&
  UNPARKED, CONVERTED, PARKED, STKPASS, STKPTOT, STKCTOT, FTM_ALIGNHIST, &
  STK_SUP, STK_SUP_TOT, SURVAC, SHR_FTM, FTMAC, YIELD, YLD, &
  LCPMD, LCPMI, ALPHAYD, RHOYD, BETAFUELD, BETATIMED, ALPHAYI, RHOYI, BETAFUELI, BETATIMEI, &
  AIR_MGMT_ADJ, ASMPG_STK_TYP, ASMPG_NEW_TYP, PASS_WEIGHT, GPTM, RPMPG, PASSAC_RPM_DMD,CARGOAC_FTM_DMD, &
  BELLY_RPM_EQ, PCT_PASS_MTOW, PCT_BELLY_PLOAD,ASMPG_VINT,ASMPG_AVG_AGE, PCT_BELLY_FRT,FUEL_BURN_RED, &
  intercept_rpm, beta1_rpm, intercept_ftm, beta1_ftm, SHR_RPM, WEPS_TRAN_GDP, WEPS_TRAN_POP,&
  COVID_MULT,COVIDMULT_REF,COVIDMULT_LOMAC,COVIDMULT_HIMAC, & ! QJETR_DI, &  !NEMSWEPSSWITCH -- Uncomment QJETR_DI for WEPS, comment out QJETR_DI for NEMS (in tranmain)
  MAX_UNPRK_SHR,STK_ALIGN_MULT, STK_AVGAGE_PA

  common/tranairint4/ TRANEFF, iy, Num_to_Read, First_Read_Year, AIRUNIT, AGEUNIT,di, &
					  MINAGE_PRK_F, MINAGE_C_PPRK, MINAGE_C_PACT, MINAGE_PRK_P, MAXAGE_UNPRK_P  ! integer 4

  common/tranairint2/ YRS, N

!!======================================================================================================            
end module AIRMOD
!!======================================================================================================

! NEMS and WEPS operate differently: 
! NEMS calls TRANAIR each year. WEPS calls TRANAIR once and it runs all years.
! TRANAIR, which runs all of the air model, is separately coded for WEPS and NEMS.

!======================================================================================================
!     SUBROUTINE TRANAIR -- NEMS version
!======================================================================================================
! NEMS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
SUBROUTINE TRANAIR
  USE AIRMOD
  IMPLICIT NONE
  LOGICAL     	NEW/.TRUE./
  INTEGER 		FILE_MGR
  EXTERNAL 		FILE_MGR
  CHARACTER*8 	FNAME
  
  FIRST_READ_YEAR = 1995
  iy = FIRST_READ_YEAR - BASEYR + 1		! BASEYR is 1990
  FIRST_FCST_YR = 2023
  LAST_HIST_YR = FIRST_FCST_YR-1 
  FIRST_FCST_INDEX = FIRST_FCST_YR-1989 
  LAST_HIST_INDEX = LAST_HIST_YR - 1989 
  
  N = CURIYR
  YRS = N + 1989
  NUM_TO_READ = IJUMPYR -(FIRST_READ_YEAR - BASEYR)
  
!! Open debug write files and read the air variables from trnairx.xlsx
  IF (YRS.eq.FIRST_READ_YEAR.and.curitr.eq.1) THEN
    FNAME = 'TRANAIR '
    AIRUNIT = FILE_MGR('O',FNAME,NEW)   ! open writes file for air subroutine
    FNAME = 'AIRAGE  '
    AGEUNIT = FILE_MGR('O',FNAME,NEW)   ! open writes file for air subroutine
    CALL READAIR
  ENDIF
  
  CALL TAIRT
  CALL TAIREFF
  CALL TAREPORT
  
  RETURN

END SUBROUTINE TRANAIR
! NEMS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

!======================================================================================================
!     SUBROUTINE TRANAIR -- WEPS version
!======================================================================================================
! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
!SUBROUTINE TRANAIR(tranair_input_file)  
!  USE AIRMOD
!  IMPLICIT NONE
!  CHARACTER*(*) tranair_input_file   
!   
!  FIRST_READ_YEAR = 1995
!  iy = FIRST_READ_YEAR - BASEYR + 1		! BASEYR is 1990
!  FIRST_FCST_YR = 2023
!  LAST_HIST_YR = FIRST_FCST_YR-1
!  FIRST_FCST_INDEX = FIRST_FCST_YR-1989
!  LAST_HIST_INDEX = LAST_HIST_YR - 1989
!
!  IJUMPYR = 61
!  DO CURCALYR = FIRST_READ_YEAR, LMYr
!    CURIYR = CURCALYR-1989
!    N = CURIYR
!    YRS = CURCALYR
!    NUM_TO_READ = IJUMPYR -(FIRST_READ_YEAR - BASEYR)
!  
!!!   Open debug write files and read the air variables from trnair.xml
!    IF (YRS.eq.FIRST_READ_YEAR) THEN
!      OPEN(unit=70,file='tranair.txt')
!  	  AIRUNIT=70
!  	  OPEN(unit=71,file='airage.csv')
!  	  AGEUNIT=71
!  	  CALL READAIR(tranair_input_file)
!	  WRITE(airunit,*)'CHECKPOINT: Completed subroutine READAIR'
!    ENDIF
!	
!!!   Combine historical/projected macro data	
!    IF (curcalyr.lt.pFFYr) THEN		! pFFYr = first forecast year, all of WEPS
!      WLD_POP(:,curiyr)  = HPOP(:,curcalyr)
!      WLD_GDP(:,curiyr)  = HGDP_PPP(:,curcalyr)
!    ELSE 
!      WLD_POP(:,curiyr)  = POP(:,curcalyr)
!      WLD_GDP(:,curiyr)  = GDP_PPP(:,curcalyr)
!    ENDIF      
!    WRITE(airunit,*)curcalyr, 'CHECKPOINT: Completed macro variable allocation'
!	
!    CALL TAIRT
!    WRITE(airunit,*)curcalyr, 'CHECKPOINT: Completed subroutine TAIRT'
!    CALL TAIREFF
!    WRITE(airunit,*)curcalyr, 'CHECKPOINT: Completed subroutine TAIREFF'
!    CALL TAREPORT
!    WRITE(airunit,*)curcalyr, 'CHECKPOINT: Completed subroutine TAREPORT'
!  
!  ENDDO	! curcalyr:LMYr
!	
!  RETURN
!
!END SUBROUTINE TRANAIR
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

!===============================================================================
!     SUBROUTINE READAIR reads the spreadsheet input file TRNAIRX.XLSX     
!===============================================================================
! NEMS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
SUBROUTINE READAIR
! NEMS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
!SUBROUTINE READAIR(tranair_input_file)
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
  USE AIRMOD
  IMPLICIT NONE
! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
  !CHARACTER*(*) tranair_input_file 
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

! Declare local parameters
  LOGICAL       NEW/.FALSE./
  CHARACTER*18  INAME
  INTEGER*2		FILE_MGR
  EXTERNAL		FILE_MGR
  INTEGER		WKUNIT
  INTEGER  		i
  REAL 			GPTMD_PASS_VINT(maxwreg,maxacage,maxatyp),GPTMI_PASS_VINT(maxwreg,maxacage,maxatyp)

  REAL			PASS_WEIGHTX(1),FUEL_BURN_REDX(1),ALPHAYDX(1),RHOYDX(1),BETAFUELDX(1),BETATIMEDX(1),ALPHAYIX(1),RHOYIX(1),BETAFUELIX(1),BETATIMEIX(1),MAX_UNPRK_SHRX(1)
  INTEGER*2		MINAGE_PRK_FX(1),MINAGE_C_PPRKX(1),MINAGE_C_PACTX(1),MINAGE_PRK_PX(1),MAXAGE_UNPRK_PX(1)

! NEMS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
  INAME = 'TRNAIRX'
  WKUNIT = FILE_MGR('O',INAME,NEW)                   ! open trnairx.xlsx input file
  CALL ReadRngXLSX(WKUNIT,'trnair')                  ! read range names & corresponding data from worksheet "trnair"
  WKUNIT = FILE_MGR('C',INAME,NEW)                   ! close wk1 input file
!! Non-US regions use exogenous gdp and population in NEMS  
  CALL GETRNGR('WLD_POP         ',WLD_POP(1:maxwreg,iy:IJUMPYR),maxwreg,Num_to_Read,1)                ! regional population
  CALL GETRNGR('WLD_GDP         ',WLD_GDP(1:maxwreg,iy:IJUMPYR),maxwreg,Num_to_Read,1)                ! regional gdp
! NEMS section END -- NEMSWEPSSWITCH ---------------------------------------------------------------------------------------------------------------- 

! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
  !INAME=tranair_input_file
  !OPEN(unit=80,file=trim(INAME))
  !CALL ReadRngXML(80,'trnair') 
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
  
  CALL GETRNGR('QAGTR_US        ',QAGTR_US(1:1,iy:IJUMPYR),1,Num_to_Read,1)                                             ! US aviation gasoline consumption

! RPM, FTM, Load factors, and RPM/FTM capacities
  CALL GETRNGR('LOAD_FACTOR     ',LOAD_FACTOR(1:maxwreg,iy:IJUMPYR,1:2),maxwreg,NUM_TO_READ,domint)					! Load factor by region and dom/int'l
  CALL GETRNGR('SHR_FTMD        ',SHR_FTM(1:maxwreg,iy:IJUMPYR,1:maxatyp,1),maxwreg,NUM_TO_READ,maxatyp)						! Share of domestic RTMs that are each body type, by region
  CALL GETRNGR('SHR_FTMI        ',SHR_FTM(1:maxwreg,iy:IJUMPYR,1:maxatyp,2),maxwreg,NUM_TO_READ,maxatyp)						! Share of international RTMs that are each body type, by region
  CALL GETRNGR('SHR_RPMD        ',SHR_RPM(1:maxwreg,iy:IJUMPYR,1:maxatyp,1),maxwreg,NUM_TO_READ,maxatyp)						! Share of domestic RTMs that are each body type, by region
  CALL GETRNGR('SHR_RPMI        ',SHR_RPM(1:maxwreg,iy:IJUMPYR,1:maxatyp,2),maxwreg,NUM_TO_READ,maxatyp)						! Share of international RTMs that are each body type, by region
  CALL GETRNGR('RPMT_HIST       ',RPMT_HIST(1:maxwreg,iy:LAST_HIST_INDEX,1:2),maxwreg,LAST_HIST_INDEX-iy+1,domint)		! hist rpm by reg, dom/int - billions
  CALL GETRNGR('FTM_HIST        ',FTM(1:maxwreg,iy:LAST_HIST_INDEX,1:2),maxwreg,LAST_HIST_INDEX-iy+1,domint)			! FTM by region - millions
  CALL GETRNGR('FTMAC           ',FTMAC(1:maxwreg,iy:IJUMPYR,1:3),maxwreg,NUM_TO_READ,maxatyp)      		 			! FTM per aircraft (FTM capacity)
  CALL GETRNGR('ASMAC           ',ASMAC(1:maxwreg,iy:IJUMPYR,1:3),maxwreg,NUM_TO_READ,maxatyp)       					! ASM per aircraft (ASM capacity)

! Stock model variables (Suffixes: XY, X={P:passenger, C: Cargo}, Y ={A: active, P: parked})
  CALL GETRNGR('STKPA           ',STKPA(1:maxwreg,iy:IJUMPYR,1:maxatyp),maxwreg,NUM_TO_READ,maxatyp)         			! active passenger aircraft stock
  CALL GETRNGR('STKPP           ',STKPP(1:maxwreg,iy:IJUMPYR,1:maxatyp),maxwreg,NUM_TO_READ,maxatyp) 					! parked passenger aircraft stock
  CALL GETRNGR('STKCA           ',STKCA(1:maxwreg,iy:IJUMPYR,1:maxatyp),maxwreg,NUM_TO_READ,maxatyp)         			! active cargo aircraft stock
  CALL GETRNGR('STKCP           ',STKCP(1:maxwreg,iy:IJUMPYR,1:maxatyp),maxwreg,NUM_TO_READ,maxatyp) 					! parked cargo aircraft stock
  CALL GETRNGR('STKPAVINT       ',STKPAVINT(1:maxwreg,1:maxacage,1:maxatyp),maxwreg,maxacage,maxatyp)     			    ! passenger aircraft stock by vintage
  CALL GETRNGR('STKCAVINT       ',STKCAVINT(1:maxwreg,1:maxacage,1:maxatyp),maxwreg,maxacage,maxatyp)           		! cargo aircraft stock by vintage
  CALL GETRNGR('STKPPVINT       ',STKPPVINT(1:maxwreg,1:maxacage,1:maxatyp),maxwreg,maxacage,maxatyp)           		! passenger ac stock by vintage
  CALL GETRNGR('STKCPVINT       ',STKCPVINT(1:maxwreg,1:maxacage,1:maxatyp),maxwreg,maxacage,maxatyp)          			! cargo ac by vintage
  CALL GETRNGR('SURVAC          ',SURVAC(1:maxatyp,1:maxacage,1:2),maxatyp,maxacage,2)                          		! aircraft survival curves. 1: passenger, 2: freight
  CALL GETRNGR('STK_SUP_NEW     ',STK_SUP_NEW(1:maxwreg,iy:IJUMPYR,1:maxatyp),maxwreg,NUM_TO_READ,maxatyp)              ! new aircraft stock
  CALL GETRNGR('MIN_PRK_SHR     ',MIN_PRK_SHR(1:maxwreg,1:maxatyp),maxwreg,maxatyp,1)													! Minimum required share of total passenger aircraft that must remain parked

  CALL GETRNGR('YIELD           ',YIELD(1:2,iy:IJUMPYR),domint,NUM_TO_READ,1)                                   ! passenger yield: revenue per passenger miles 

! Fuel consumption variables	
  CALL GETRNGR('GPTMD_PASS_VINT ',GPTMD_PASS_VINT(1:maxwreg,1:maxacage,1:maxatyp),maxwreg,maxacage,maxatyp)                      ! 2019 Domestic aircraft efficiency, vintaged (gal/ton-mile)
  CALL GETRNGR('GPTMI_PASS_VINT ',GPTMI_PASS_VINT(1:maxwreg,1:maxacage,1:maxatyp),maxwreg,maxacage,maxatyp)						! 2019 International aircraft efficiency, vintaged (gal/ton-mile)
  CALL GETRNGR('GPTMD_PASS      ',GPTM(1:maxwreg,iy:IJUMPYR,1:maxatyp,1),maxwreg,NUM_TO_READ,maxatyp)					! Domestic aircraft efficiency (gal/ton-mile)
  CALL GETRNGR('GPTMI_PASS      ',GPTM(1:maxwreg,iy:IJUMPYR,1:maxatyp,2),maxwreg,NUM_TO_READ,maxatyp)					! International aircraft efficiency (gal/ton-mile)

  CALL GETRNGR('PCT_PASS_MTOW   ',PCT_PASS_MTOW(1:maxatyp,iy:IJUMPYR),maxatyp,NUM_TO_READ,1)							! Percent of passenger flight MTOW that is passenger (not incl. belly freight)
  CALL GETRNGR('PCT_BELLY_PLOAD ',PCT_BELLY_PLOAD(1:maxatyp,iy:IJUMPYR,1:2),maxatyp,NUM_TO_READ,domint)				! Percent of passenger flight payload (pass + freight) that is freight (aka belly freight)
  CALL GETRNGR('PCT_BELLY_FRT   ',PCT_BELLY_FRT(1:maxatyp,iy:IJUMPYR,1:2),maxatyp,NUM_TO_READ,domint)		
  CALL GETRNGR('AIR_MGMT_ADJ    ',AIR_MGMT_ADJ(1:maxwreg,iy:IJUMPYR,1:2),maxwreg,NUM_TO_READ,domint)					! Factor applied to account for distance traveled beyond the great circle distance (relative to US value)
  CALL GETRNGR('STK_ALIGN_MULT  ',STK_ALIGN_MULT(1:maxwreg,31:37,1:maxatyp),maxwreg,7,maxatyp)								! GPTM calibration factor (align vintaged stock GPTM to total stock GPTM)

!.Econometric equation coefficients
! Passenger demand (RPM)
  CALL GETRNGR('intercept_rpm   ',intercept_rpm(1:maxwreg,1:2),maxwreg,domint,1)                 					! Intercept, RPM equation coefficients   
  CALL GETRNGR('beta1_rpm       ',beta1_rpm(1:maxwreg,1:2),maxwreg,domint,1)                     					! GDP/capita elasticity of RPM/capita, RPM equation coefficients
  CALL GETRNGR('COVIDMULT_REF   ',COVIDMULT_REF(1:maxwreg,30:41,1:2),maxwreg,12,domint)								! Adjustment factor for covid impacts on rpm, reference case (2019-2030)
  CALL GETRNGR('COVIDMULT_LOMAC ',COVIDMULT_LOMAC(1:maxwreg,30:41,1:2),maxwreg,12,domint)							! Adjustment factor for covid impacts on rpm, low macro case (2019-2030)
  CALL GETRNGR('COVIDMULT_HIMAC ',COVIDMULT_HIMAC(1:maxwreg,30:41,1:2),maxwreg,12,domint)							! Adjustment factor for covid impacts on rpm, high macro case (2019-2030)
  
! Freight demand (FTM)
  CALL GETRNGR('intercept_ftm   ',intercept_ftm(1:maxwreg,1:2),maxwreg,domint,1)
  CALL GETRNGR('beta1_ftm       ',beta1_ftm(1:maxwreg,1:2),maxwreg,domint,1)													! gdp coefficient for FTM projection
  
! NEMS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
!!   Fuel consumption variables
    CALL GETRNGR('PASS_WEIGHT     ',PASS_WEIGHT,1,1,1)															! Average passenger weight, including luggage (200lb, BTS)
    CALL GETRNGR('FUEL_BURN_RED   ',FUEL_BURN_RED,1,1,1)														! Projected annual fuel burn reduction (all body types and regions). Source: ICCT historical (2010+)
!!   Domestic yield  
    CALL GETRNGR('ALPHAYD         ',ALPHAYD,1,1,1)
    CALL GETRNGR('RHOYD           ',RHOYD,1,1,1)
    CALL GETRNGR('BETAFUELD       ',BETAFUELD,1,1,1)
    CALL GETRNGR('BETATIMED       ',BETATIMED,1,1,1)
!!   Int'l yield
    CALL GETRNGR('ALPHAYI         ',ALPHAYI,1,1,1)
    CALL GETRNGR('RHOYI           ',RHOYI,1,1,1)
    CALL GETRNGR('BETAFUELI       ',BETAFUELI,1,1,1)
    CALL GETRNGR('BETATIMEI       ',BETATIMEI,1,1,1)

    CALL GETRNGI('MINAGE_PRK_F    ',MINAGE_PRK_F,1,1,1)        	! The minimum age at which freight aircraft can be parked (due to over capacity).
    CALL GETRNGI('MINAGE_C_PPRK   ',MINAGE_C_PPRK,1,1,1)        	! The minimum age at which long-term-parked passenger aircraft can be converted to freighters to meet freight demand.
    CALL GETRNGI('MINAGE_C_PACT   ',MINAGE_C_PACT,1,1,1)        	! The minimum age at which active passenger aircraft can be converted to freighters to meet freight demand.
    CALL GETRNGI('MINAGE_PRK_P    ',MINAGE_PRK_P,1,1,1)        	! The minimum age at which passenger aircraft can be parked (due to over capacity).
    CALL GETRNGI('MAXAGE_UNPRK_P  ',MAXAGE_UNPRK_P,1,1,1)        	! The maximum age at which passenger aircraft can be un-parked to meet excess demand.
    CALL GETRNGR('MAX_UNPRK_SHR   ',MAX_UNPRK_SHR,1,1,1)        	! The maximum share of total parked passenger aircraft that can be unparked in a single year (to meet excess demand).

!! WEPS populates US WLD_POP and WLD_GDP with EIA US macro outputs; NEMS does not.	
	DO i=iy,IJUMPYR
	  WLD_POP(us,i) = MC_NP(11,i)
	  WLD_GDP(us,i) = MC_GDPR(i)*(MC_JPGDP(26)/MC_JPGDP(23))   ! gdp, in 2015 USD [match oxford units used in other regions]
	ENDDO
! NEMS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
!!! WEPS currently uses ReadXML rather than ReadXLSX. Single read-in values from ReadXML have to be re-dimensioned...
!!! Fuel consumption variables
!  CALL GETRNGR('PASS_WEIGHT     ',PASS_WEIGHTX,1,1,1)															       	! Average passenger weight, including luggage (200lb, BTS)
!  CALL GETRNGR('FUEL_BURN_RED   ',FUEL_BURN_REDX,1,1,1)																! Projected annual fuel burn reduction (all body types and regions). Source: ICCT historical (2010+)
!!! Domestic yield  
!  CALL GETRNGR('ALPHAYD         ',ALPHAYDX,1,1,1)
!  CALL GETRNGR('RHOYD           ',RHOYDX,1,1,1)
!  CALL GETRNGR('BETAFUELD       ',BETAFUELDX,1,1,1)
!  CALL GETRNGR('BETATIMED       ',BETATIMEDX,1,1,1)
!!! Int'l yield
!  CALL GETRNGR('ALPHAYI         ',ALPHAYIX,1,1,1)
!  CALL GETRNGR('RHOYI           ',RHOYIX,1,1,1)
!  CALL GETRNGR('BETAFUELI       ',BETAFUELIX,1,1,1)
!  CALL GETRNGR('BETATIMEI       ',BETATIMEIX,1,1,1)   
!
!  CALL GETRNGI('MINAGE_PRK_F    ',MINAGE_PRK_FX,1,1,1)        	! The minimum age at which freight aircraft can be parked (due to over capacity).
!  CALL GETRNGI('MINAGE_C_PPRK   ',MINAGE_C_PPRKX,1,1,1)        	! The minimum age at which long-term-parked passenger aircraft can be converted to freighters to meet freight demand.
!  CALL GETRNGI('MINAGE_C_PACT   ',MINAGE_C_PACTX,1,1,1)        	! The minimum age at which active passenger aircraft can be converted to freighters to meet freight demand.
!  CALL GETRNGI('MINAGE_PRK_P    ',MINAGE_PRK_PX,1,1,1)        	! The minimum age at which passenger aircraft can be parked (due to over capacity).
!  CALL GETRNGI('MAXAGE_UNPRK_P  ',MAXAGE_UNPRK_PX,1,1,1)        ! The maximum age at which passenger aircraft can be un-parked to meet excess demand.
!  CALL GETRNGR('MAX_UNPRK_SHR   ',MAX_UNPRK_SHRX,1,1,1)        	! The maximum share of total parked passenger aircraft that can be unparked in a single year (to meet excess demand).
!
!
!  CLOSE(80)	
!
!  PASS_WEIGHT   = PASS_WEIGHTX(1)
!  FUEL_BURN_RED = FUEL_BURN_REDX(1)
!  ALPHAYD 	  	= ALPHAYDX(1)
!  RHOYD 		= RHOYDX(1)
!  BETAFUELD 	= BETAFUELDX(1)
!  BETATIMED 	= BETATIMEDX(1)
!  ALPHAYI 	    = ALPHAYIX(1)
!  RHOYI 		= RHOYIX(1)
!  BETAFUELI 	= BETAFUELIX(1)
!  BETATIMEI 	= BETATIMEIX(1)
!  MINAGE_PRK_F  = MINAGE_PRK_FX(1)
!  MINAGE_C_PPRK = MINAGE_C_PPRKX(1)
!  MINAGE_C_PACT = MINAGE_C_PACTX(1)
!  MINAGE_PRK_P  = MINAGE_PRK_PX(1)
!  MAXAGE_UNPRK_P= MAXAGE_UNPRK_PX(1)
!  MAX_UNPRK_SHR = MAX_UNPRK_SHRX(1)
  
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
	
! Input pre-processing / re-arranging
! Copy stock history into separate stock totals for forecasting
  DO i=iy,IJUMPYR
    DO iatyp=1,maxatyp
      DO iwreg=1,maxwreg
        STK_SUP_TOT(iwreg,iatyp,i)	= STKPA(iwreg,i,iatyp) + STKPP(iwreg,i,iatyp) + STKCA(iwreg,i,iatyp) + STKCP(iwreg,i,iatyp)
		STKPTOT(iwreg,iatyp,i)      = STKPA(iwreg,i,iatyp) + STKPP(iwreg,i,iatyp)
        STKCTOT(iwreg,iatyp,i)      = STKCA(iwreg,i,iatyp) + STKCP(iwreg,i,iatyp)
      
	  ENDDO
    ENDDO
  ENDDO
    
! Convert from gal/ton-mi (GPTM_VINT) to seat-mile/gal; consolidate  (ASMPG_VINT)
  DO iwreg = 1, maxwreg
    DO iatyp = 1, maxatyp
      DO iage = 1, maxacage
	    ASMPG_VINT(iwreg,iatyp,iage,1,LAST_HIST_YR-1989) = 1/(GPTMD_PASS_VINT(iwreg,iage,iatyp)*pass_weight/2000)
		ASMPG_VINT(iwreg,iatyp,iage,2,LAST_HIST_YR-1989) = 1/(GPTMI_PASS_VINT(iwreg,iage,iatyp)*pass_weight/2000)
	  ENDDO
	ENDDO
  ENDDO !iwreg
  
! Set covid RPM correction factor; dependent on case.
  COVID_MULT      = 0.0
! NEMS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
  IF (MMAC.eq.4) THEN
    COVID_MULT(:,:,:) = COVIDMULT_LOMAC(:,:,:)
  ELSEIF (MMAC.eq.5) THEN
    COVID_MULT(:,:,:) = COVIDMULT_HIMAC(:,:,:)
  ELSE
    COVID_MULT(:,:,:) = COVIDMULT_REF(:,:,:)
  ENDIF
! NEMS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
  !COVID_MULT(:,:,:) = COVIDMULT_REF(:,:,:)
  !CFJFK = 5.670											! WEPS doesn't have an internal energy content variable. This is mmBTU/barrel of jet fuel.
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

! ************************************ TESTING/DEBUG ************************************
  WRITE(AIRUNIT,*)'COVIDMULT_REF check'
  DO i = 1, 41
    WRITE(airunit,'("dom",i5,16F6.3)')i+1989, COVIDMULT_REF(:,i,1)
  ENDDO
  DO i = 1, 41
    WRITE(airunit,'("int",i5,16F6.3)')i+1989, COVIDMULT_REF(:,i,2)
  ENDDO
! ************************************ TESTING/DEBUG ************************************
  RETURN
  
END SUBROUTINE READAIR

!================================================================================                                                                           
!                       AIRCRAFT DEMAND/EFFICIENCY MODEL                      
!================================================================================
!      This subroutine calculates total seat miles demanded for domestic and    
!      international air travel as well as revenue ton miles for air freight                                                                
!================================================================================ 

SUBROUTINE TAIRT
  USE AIRMOD
  IMPLICIT NONE

  INTEGER     	i
  INTEGER     	NB/1/, WB/2/, RJ/3/, WLD/3/
  REAL			PJFTR_US(MNUMYR)

! NEMS and WEPS have different jet fuel price variables (NEMS: PJFTR(mnumcr,1:61); WEPS: PJFTR(maxwreg,1990:2050))
! NEMS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
  PJFTR_US(N) = PJFTR(11,N)
! NEMS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
  !IF(n >= (pFFYr - 1989)) THEN
  !  PJFTR_US(N) = PJFTR(1,N+1989)
  !ELSE
  !  PJFTR_US(N) = PJFTR(1,pFFYr)
  !ENDIF
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
  
  IF(curcalyr.eq.FIRST_READ_YEAR) THEN 

!   Regionalize commercial jet fuel and aviation gas and drop new aircraft history into total stock variable
    DO i = iy,LAST_HIST_INDEX  
      DO iregn=1,mnumcr-2 
        QAGTR(11,i)=QAGTR_US(1,i)
        QAGTR(iregn,i)=QAGTR(11,i)*SEDSHRJF(iregn,6)
      ENDDO
      DO iwreg = 1, maxwreg
        DO iatyp = 1, maxatyp
          STK_SUP(iwreg,iatyp,1,i) = STK_SUP_NEW(iwreg,i,iatyp)			! MDRIEO2022 -- why isn't STK_SUP_NEW just read right into STK_SUP to start?
		ENDDO
      ENDDO
    ENDDO

  ENDIF  ! curcalyr.eq.FIRST_READ_YEAR

! ==================================================================================================================
! ... Calculate the yield (ticket price) in $96                              
! ... Bound below by lowest cost per mile - Domestic (LCPMD) & Int (LCPMI)        
! ... Modify by the avg dom (LFDOMAVG) & international (LFINTAVG) load factors
! ==================================================================================================================

  LFDOMAVG(N)  = 0.0
  LFINTAVG(N)  = 0.0
  DO iatyp = 1, maxatyp
    LFDOMAVG(N)=LFDOMAVG(N)+LOAD_FACTOR(us,N,dom)*SHR_RPM(us,N,iatyp,dom)
    LFINTAVG(N)=LFINTAVG(N)+LOAD_FACTOR(us,N,int)*SHR_RPM(us,N,iatyp,int)
  ENDDO
  
! Calculate Yield in 96 c/gal. Pjftr (convert 87$/mmbtu->96 cents/gal). Only done for US.
  IF (YRS.ge.FIRST_FCST_YR) THEN
    IF (YRS.eq.(FIRST_FCST_YR)) YLD(dom,N-1) = YIELD(dom,N-1)
    YLD(dom,N) = ALPHAYD*(1. - RHOYD) + RHOYD * YLD(dom,N-1) + BETAFUELD * &
                 (PJFTR_US(N) - RHOYD*PJFTR_US(N-1)) + BETATIMED * (N+11 - RHOYD*(N+10))
    YIELD(dom,N) = YLD(dom,N)
    YIELD(int,N) = ALPHAYI*(1. - RHOYI) + RHOYI*YIELD(int,N-1) + BETAFUELI * (PJFTR_US(N) &
                   - RHOYI*PJFTR_US(N-1)) + BETATIMEI * (N+11 - RHOYI*(N+10))
  ENDIF
  
! ************************************ TESTING/DEBUG ************************************
!  IF (curitr.eq.1) THEN 
!    WRITE(AIRUNIT,*)YRS,yield(dom,N),yield(int,N)
!  ENDIF  
! ************************************ TESTING/DEBUG ************************************

! ==================================================================================================================
! ... Calculate revenue passenger miles domestic (RPMT) 
! ==================================================================================================================

! Convert historical RPMT (RPMT_HIST) from billions to millions (RPMT) for RPMT/capita calculation
! Calculate RPMT per capita (RPMT_PC)
! Segment RPMT by body type (nb, wb, rj)
  IF (YRS.lt.FIRST_FCST_YR) THEN
    DO iwreg = 1, maxwreg
	  DO di = 1, domint
	    RPMT(iwreg,di,N) = 1000.*RPMT_HIST(iwreg,N,di)				! convert to millions
        RPMT_PC(iwreg,di,N) = RPMT(iwreg,di,N)/WLD_POP(iwreg,N)
      ENDDO
	ENDDO
	
    DO iwreg = 1, maxwreg
      DO di = 1, domint
    	DO iatyp = 1, maxatyp
    	  RPM(iwreg,di,iatyp,N)=RPMT(iwreg,di,N)*SHR_RPM(iwreg,N,iatyp,di)
    	ENDDO
      ENDDO
	ENDDO
	
  ELSE IF (YRS.ge.FIRST_FCST_YR) THEN 
  
! ************************************ TESTING/DEBUG ************************************
 
    DO iwreg = 1, maxwreg
      DO di = 1, domint
	  
!       Calculate RPM per capita by region and domestic / international
		RPMT_PC(iwreg,di,N) = RPMT_PC(iwreg,di,N-1)*exp(intercept_rpm(iwreg,di))* &
		                      (((WLD_GDP(iwreg,N)/WLD_POP(iwreg,N))/(WLD_GDP(iwreg,N-1)/WLD_POP(iwreg,N-1)))**beta1_rpm(iwreg,di))
		
        RPMT(iwreg,di,N) = RPMT_PC(iwreg,di,N) * WLD_POP(iwreg,N)
!		Covid impact applied to 2019 baseline, rather than pre-covid projection.
		IF (covid_mult(iwreg,N,di).ne.0) THEN
		  RPMT(iwreg,di,N) = RPMT(iwreg,di,30)*(1-covid_mult(iwreg,N,di))		! Covid impact is pinned to the 2019 baseline (n=30)
		  RPMT_PC(iwreg,di,N) = RPMT(iwreg,di,N)/WLD_POP(iwreg,N)
		ENDIF

      ENDDO ! di:domint

      DO di = 1, domint
	    DO iatyp = 1, maxatyp
		  RPM(iwreg,di,iatyp,N) = RPMT(iwreg,di,N)*SHR_RPM(iwreg,N,iatyp,di)
		ENDDO
	  ENDDO
	  
    ENDDO ! iwreg
  ENDIF   ! YRS.ge.FIRST_FCST_YR
     
! ==================================================================================================================
! ... Calculate revenue ton miles of air freight (FTM)
! ==================================================================================================================

! FTM_ALIGNHIST ensures that the projection aligns with history. It is phased out over the full projection period (LAST_HIST_YR --> 2050)
  IF (YRS.eq.LAST_HIST_YR) THEN
    FTM_ALIGNHIST = 0.0
    DO iwreg = 1, maxwreg
	  DO di = 1, domint
	    FTM_ALIGNHIST(iwreg,di) = exp(intercept_ftm(iwreg,di))*(wld_gdp(iwreg,N)**(beta1_ftm(iwreg,di))) - FTM(iwreg,N,di)
	  ENDDO
	ENDDO
  ENDIF

  IF (YRS.ge.FIRST_FCST_YR) THEN
	DO iwreg = 1, maxwreg
	  DO di = 1, domint
	    FTM(iwreg,N,di) = exp(intercept_ftm(iwreg,di))*(wld_gdp(iwreg,N)**(beta1_ftm(iwreg,di))) - FTM_ALIGNHIST(iwreg,di)*(0.85**(N-32))
	  ENDDO
	ENDDO
  ENDIF
  
! ************************************ TESTING/DEBUG ************************************
  IF (YRS.eq.2050) THEN
    WRITE(airunit,*)'reg di year WLD_POP WLD_GDP RPMT FTM FTM_ALIGNHIST'
    DO i = 6, mnumyr
	  DO iwreg = 1, maxwreg
	    DO di = 1, domint
		  WRITE(airunit,'(a,", ",I1,", ",I4,", ",5(F12.2,", "))') reg_def(iwreg), di, i+1989, WLD_POP(iwreg,i), WLD_GDP(iwreg,i), RPMT(iwreg,di,i), FTM(iwreg,i,di), FTM_ALIGNHIST(iwreg,di)
		ENDDO
	  ENDDO
	ENDDO
  ENDIF
! ************************************ TESTING/DEBUG ************************************

! ==================================================================================================================
! ... Estimate RPMs and RTMs that include belly freight, for energy/efficiency calculations 
!     (not used in reporting total FTM/RPM)
!     Calculate RPMS/gal (RPMPG) and non-vintaged stock MPG (ASMPG_AVG_AGE) for historical years
! ==================================================================================================================

  DO iwreg = 1, maxwreg
    DO iatyp = 1, maxatyp
	  DO di = 1, domint
	  
	    IF (N.eq.2001.and.iwreg.ne.1) THEN			! 9/11 impact not as strong in non-US regions (caused negative FTM demand in CA and RU)
		  PCT_BELLY_PLOAD(iatyp,N,di) = (PCT_BELLY_PLOAD(iatyp,N-1,di) + PCT_BELLY_PLOAD(iatyp,N+1,di))/2
		  PCT_BELLY_FRT(iatyp,N,di)   = (PCT_BELLY_FRT(iatyp,N-1,di)   + PCT_BELLY_FRT(iatyp,N+1,di))  /2
		ENDIF
	  
!       Estimate belly freight, in units of RPM-equivalent (where PCT_BELLY_PLOAD is the share of pass-flight payload (pass+freight) that is freight))
!		Note that RPM is only the passenger payload, while PCT_BELLY_PLOAD is freight share of pass + freight -- have to get freight indexed to passenger only (hence the 1- in denom)
!		Bound using the last historical (total RTMs * belly freight share of total RTMs [by body type and domint] from BTS).
		BELLY_RPM_EQ(iwreg,iatyp,di,N) = MIN(RPM(iwreg,di,iatyp,N)*PCT_BELLY_PLOAD(iatyp,N,di)/(1 - PCT_BELLY_PLOAD(iatyp,N,di)), &
		                                     SUM(FTM(iwreg,N,:))*PCT_BELLY_FRT(iatyp,N,di)*2000/pass_weight)
		PASSAC_RPM_DMD(iwreg,iatyp,di,N) = RPM(iwreg,di,iatyp,N) + BELLY_RPM_EQ(iwreg,iatyp,di,N)					
		
		IF (YRS.le.LAST_HIST_YR) THEN
		
!		  Convert GPTM (all seats full; includes belly freight) into RPMPG (based on load factors and payload shares) for historical years
!		  Calculate available-seat-miles per gallon (ASMPG) for the entire stock (convert all passenger flight ton-miles to seat-miles)
!		  GPTM is the fuel consumption for a fully loaded plane, i.e. it is equivalent to "gallons per seat-mile" not "gallons per RPM"
!		  A load factor must be applied to GPTM to get RPMs (as shown below).
		  IF (PASSAC_RPM_DMD(iwreg,iatyp,di,N).gt.0.0) THEN
		    RPMPG(iwreg,iatyp,di,N) = 1/(GPTM(iwreg,N,iatyp,di)*pass_weight/2000* &
                                           (1 - (1-LOAD_FACTOR(iwreg,N,di))*PCT_PASS_MTOW(iatyp,N))/ &													! Decrease in fuel use due to empty seats
                                           (1 - (1-LOAD_FACTOR(iwreg,N,di))*(1-(BELLY_RPM_EQ(iwreg,iatyp,di,N)/PASSAC_RPM_DMD(iwreg,iatyp,di,N)))) &	! Decrease in payload ton-miles due to empty seats
               					        )
		  ELSE	! if no RPM demand, populate the variable anyway 
		    RPMPG(iwreg,iatyp,di,N) = 1/(GPTM(iwreg,N,iatyp,di)*pass_weight/2000* &
                                           (1 - (1-LOAD_FACTOR(iwreg,N,di))*PCT_PASS_MTOW(iatyp,N))/ &
		  							       (1 - (1-LOAD_FACTOR(iwreg,N,di))) &
		  							    )
		  ENDIF


		  ASMPG_AVG_AGE(iwreg,iatyp,di,N) = 1/(GPTM(iwreg,N,iatyp,di)*pass_weight/2000)
		  
		ENDIF
	  ENDDO
	ENDDO
	
!   Segment freight ton-miles (FTM) by body type (NB, WB, RJ) after subtracting out total freight demand filled by pass-jet bellies
    DO iatyp = 1, maxatyp
  	  DO di = 1, domint
        CARGOAC_FTM_DMD(iwreg,iatyp,di,N) = (FTM(iwreg,N,di)- SUM(BELLY_RPM_EQ(iwreg,:,di,N))*pass_weight/2000)*SHR_FTM(iwreg,N,iatyp,di)
  	    CARGOAC_FTM_DMD(iwreg,iatyp,di,N) = MAX(0.0,CARGOAC_FTM_DMD(iwreg,iatyp,di,N))
	  ENDDO
  	ENDDO
	
  ENDDO

! ==================================================================================================================
! Calculate seat-mile totals for reporting             
! ==================================================================================================================

  SMDEMD(:,N) = 0.0																				! Initialize all regions' seat-mile demand to zero for the current year
  SMD_TOT(:,N) = 0.0
  ASM(:,:,:,N) = 0.0
  ASMDEMD(:,:,N) = 0.0
  DO iwreg = 1, maxwreg
    DO di = 1, domint
	  DO iatyp = 1, maxatyp
	    ASM(iwreg,di,iatyp,N) = RPM(iwreg,di,iatyp,N) / LOAD_FACTOR(iwreg,N,di)			! Calculate available seat miles ASM(iwreg,domint,iatyp,N) - not incl. belly freight
      ENDDO
	ENDDO
  ENDDO
  
  DO iwreg = 1, maxwreg													
    SMDEMD (iwreg,N) = sum(ASM(iwreg,:,:,N))													! Aggregate available seat miles to region
    DO iatyp = 1, maxatyp
	  ASMDEMD(iwreg,iatyp,N) = sum(ASM(iwreg,:,iatyp,N))										! Aggregate available seat miles to region, body type, millions
	ENDDO
	SMD_TOT(iwreg,N) = (SMDEMD(iwreg,N) + SUM(FTM(iwreg,N,:)) * 2000/pass_weight) * 0.001				! Calculate total seat mile demand, including freight, billions
  ENDDO

  RETURN

END SUBROUTINE TAIRT

! ===============================================================================
! SUBROUTINE TAIREFF           
! ===============================================================================
! This subroutine calculates aircraft sales, stocks, efficiency, and energy use for air travel      
! ===============================================================================

SUBROUTINE TAIREFF
    USE AIRMOD
    IMPLICIT NONE

!   Declare local parameters
    INTEGER 	i
    INTEGER 	NB/1/, WB/2/, RJ/3/
    REAL    	KAPPA, GAMMA
	
	REAL		ASMAC_ADJ(maxwreg,maxatyp)					! Adjustment factor to calibrate passenger aircraft annual capacity (ASM/plane) to actual in last_hist_yr
	REAL	  	FTMAC_ADJ(maxwreg,maxatyp)					! Adjustment factor to calibrate freighter annual capacity (FTM/plane) to actual in last_hist_yr

    REAL		SUM_ASMPG_STK_AGE(maxwreg,maxatyp,domint)	! Denominator for harmonic mean; collapse age dimension
	REAL		SUM_ASMPG_STK_R(maxwreg)					! Denominator for harmonic mean; collapse all but region dimensions
	REAL		SUM_ASMPG_STK_TYP(maxatyp)					! Denominator for harmonic mean; collapse all but body type dimensions
	REAL		SUM_ASMPG_NEW_TYP(maxatyp)					! Denominator for harmonic mean; collapse all but body type dimensions

    REAL		MAX_UNPARK									! Maximum number of passenger aircraft that can be unparked (overwritten annually)
	REAL		MIN_PARK(maxwreg,maxatyp)					! Minimum size of parked passenger fleet (overwritten annually)
	REAL		UNPARK										! Temporary counter for aircraft to be unparked
	REAL		SCRAPPED									! Temporary counter for scrapped aircraft
	REAL    	CARGOAC_NEEDED(maxwreg,maxatyp)				! Total new/unparked cargo aircraft needed to 1) replace retirements and 2) meet FTM demand
    REAL    	PASSAC_NEEDED(maxwreg,maxatyp)				! Total new/unparked passenger aircraft needed to 1) replace retirements and 2) meet RPM demand	

!   Calculate historical energy (passenger RPM (incl belly freight) + cargo FTM (not incl. belly freight)    
	QJETR_NUS(:,N)    = 0.0
    QJETR_DI(:,:,:,N) = 0.0
	SUM_ASMPG_STK_AGE = 0.0
	SUM_ASMPG_STK_R   = 0.0
	SUM_ASMPG_STK_TYP = 0.0
	SUM_ASMPG_NEW_TYP = 0.0
	
	IF (YRS.le.LAST_HIST_YR) THEN
	  DO iwreg = 1, maxwreg
	    DO iatyp = 1, maxatyp
		  DO di = 1, domint
		    IF (ASM(iwreg,di,iatyp,N).ne.0.0) THEN
		      SUM_ASMPG_STK_TYP(iatyp) = SUM_ASMPG_STK_TYP(iatyp) + ASM(iwreg,di,iatyp,N)/ASMPG_AVG_AGE(iwreg,iatyp,di,N)												! Stock 
	   	  	  SUM_ASMPG_NEW_TYP(iatyp) = SUM_ASMPG_NEW_TYP(iatyp) + ASM(iwreg,di,iatyp,N)/ASMPG_VINT(iwreg,iatyp,(LAST_HIST_YR-(1989+N)+1),di,LAST_HIST_YR-1989)		! New
			  
			  ! incl. miles flown in addition to revenue miles
			  QJETR_DI(iwreg,di,1,N) = QJETR_DI(iwreg,di,1,N) + ((PASSAC_RPM_DMD(iwreg,iatyp,di,N) * AIR_MGMT_ADJ(iwreg,N,di) / RPMPG(iwreg,iatyp,di,N))/ 42.0) * CFJFK	! quads (million gal * barrel / gal * million BTU / barrel), passenger
			  QJETR_DI(iwreg,di,2,N) = QJETR_DI(iwreg,di,2,N) + ((CARGOAC_FTM_DMD(iwreg,iatyp,di,N) * GPTM(iwreg,N,iatyp,di))/ 42.0) * CFJFK								! quads, freight
			ENDIF                                  
		  ENDDO
		  
	      ASMPG_STK_TYP(iatyp,N) = sum(ASM(:,:,iatyp,N))/MAX(1.0,SUM_ASMPG_STK_TYP(iatyp))				! Reported
	   	  ASMPG_NEW_TYP(iatyp,N) = sum(ASM(:,:,iatyp,N))/MAX(1.0,SUM_ASMPG_NEW_TYP(iatyp))				! Reported
		  
		ENDDO
		
	    QJETR_NUS(iwreg,N) = sum(QJETR_DI(iwreg,:,:,N))			! quads
		QJETR(11,N) 	   = QJETR_NUS(1,N)						! quads
		
		DO iregn=1,mnumcr-2
		  QJETR(iregn,N)=QJETR(11,N)*SEDSHRJF(iregn,6)
		ENDDO
		
	  ENDDO
	  
	  ASMPGT(1,N) = sum(ASM(:,:,:,N)) / &
	                ( (sum(ASM(:,:,nb,N))/MAX(1.0,ASMPG_NEW_TYP(nb,N))) + &
	  			      (sum(ASM(:,:,wb,N))/MAX(1.0,ASMPG_NEW_TYP(wb,N))) + &
	  			      (sum(ASM(:,:,rj,N))/MAX(1.0,ASMPG_NEW_TYP(rj,N))))
	  			  
	  ASMPGT(2,N) = sum(ASM(:,:,:,N)) / &
	                ( (sum(ASM(:,:,nb,N))/MAX(1.0,ASMPG_STK_TYP(nb,N))) + &
	  			      (sum(ASM(:,:,wb,N))/MAX(1.0,ASMPG_STK_TYP(wb,N))) + &
	  			      (sum(ASM(:,:,rj,N))/MAX(1.0,ASMPG_STK_TYP(rj,N))))
	  
	ENDIF

! *********************************** Testing/Debug ************************************    
    IF (YRS.eq.FIRST_FCST_YR) THEN
	  WRITE(airunit,*)'QJETR_NUS - History IES years'
      DO i = 21,30
        WRITE(airunit,'(i4, 16F11.4)')i+1989, QJETR_NUS(:,i)
 	  ENDDO
    ENDIF
! *********************************** Testing/Debug ************************************

    IF (YRS.lt.LAST_HIST_YR) RETURN

! ===============================================================================
! Stock model          
! ===============================================================================

	STKCARGO(:,:,:,N) 		= 0.0
	STKPASS(:,:,:,N) 		= 0.0
	SCRAPPED_TOTAL(:,:,:,N) = 0.0
	STKCARGO_PARKED(:,:,:,N)= 0.0
	STKCARGO_ACTIVE(:,:,:,N)= 0.0
	STKPASS_PARKED(:,:,:,N) = 0.0
	STKPASS_ACTIVE(:,:,:,N) = 0.0
	
    IF (YRS.eq.LAST_HIST_YR) THEN	! Initialize sales, parked & active aircraft 
      DO iwreg=1,maxwreg 
        DO iatyp = 1, maxatyp
          DO iage = 1,maxacage																					  	  	! Initialize passenger & cargo stock
            STKPASS(iwreg,iatyp,iage,N) 		= STKPAVINT(iwreg,iage,iatyp)+STKPPVINT(iwreg,iage,iatyp)				! Stock - Total Passenger
            STKCARGO(iwreg,iatyp,iage,N) 		= STKCAVINT(iwreg,iage,iatyp)+STKCPVINT(iwreg,iage,iatyp)				! Stock - Total Cargo
            STKPASS_PARKED(iwreg,iatyp,iage,N) 	= STKPPVINT(iwreg,iage,iatyp) 											! Stock - Parked Passenger
            STKCARGO_PARKED(iwreg,iatyp,iage,N) = STKCPVINT(iwreg,iage,iatyp) 											! Stock - Parked Cargo
            STKPASS_ACTIVE(iwreg,iatyp,iage,N) 	= STKPAVINT(iwreg,iage,iatyp) 											! Stock - Active Passenger
            STKCARGO_ACTIVE(iwreg,iatyp,iage,N) = STKCAVINT(iwreg,iage,iatyp)											! Stock - Active Cargo
          ENDDO  ! iage
		  
		  ! MDR IEO2023 populate historical sales for viewing in TIRE
		  DO iage = 6, LAST_HIST_INDEX-1 	! iage is being used as a counter for YEAR here
		    STKPASS_ACTIVE(iwreg,iatyp,1,iage) = STK_SUP_NEW(iwreg,iage,iatyp)
		  ENDDO

		  ASMAC_ADJ(iwreg,iatyp) = 0		! Calibrate ASMAC/FTMAC to 2019 levels
		  FTMAC_ADJ(iwreg,iatyp) = 0

          IF ((STKCTOT(iwreg,iatyp,32) .NE. 0.0).and.(SUM(CARGOAC_FTM_DMD(iwreg,iatyp,:,32)).NE.0.0)) THEN		! MDRIEO2023 switched all calibration from 2019 to 2021
              WRITE(AIRUNIT,'("  Adjusting projected ",a," FTMAC for region ",a," from ",F11.0," to ",F11.0)') reg_atyp(iatyp),reg_def(iwreg), &
					FTMAC(iwreg,32,iatyp),SUM(CARGOAC_FTM_DMD(iwreg,iatyp,:,32)) * 1000000. / STKCTOT(iwreg,iatyp,32)						! Calibrate to 2019 history, since that was "normal"
			  FTMAC_ADJ(iwreg,iatyp)	   = (SUM(CARGOAC_FTM_DMD(iwreg,iatyp,:,32)) * 1000000. / STKCTOT(iwreg,iatyp,32)) &
											 / FTMAC(iwreg,32,iatyp)
			  FTMAC(iwreg,32:mnumyr,iatyp) = FTMAC_ADJ(iwreg,iatyp)*FTMAC(iwreg,32:mnumyr,iatyp)		! Apply the 2019 adjustment to the whole exogenous FTMAC projection
		  ENDIF
		  
		  IF (STKPA(iwreg,32,iatyp).NE.0.0) THEN
		      WRITE(AIRUNIT,'("  Adjusting projected ",a," ASMAC for region ",a," from ",F13.0," to ",F13.0)') reg_atyp(iatyp),reg_def(iwreg), &
					ASMAC(iwreg,32,iatyp),ASMDEMD(iwreg,iatyp,32)*1000000 / STKPA(iwreg,32,iatyp)
			  ASMAC_ADJ(iwreg,iatyp)	   = (ASMDEMD(iwreg,iatyp,32) *1000000 / STKPA(iwreg,32,iatyp))/ASMAC(iwreg,32,iatyp)
			  ASMAC(iwreg,32:mnumyr,iatyp) = ASMAC_ADJ(iwreg,iatyp)*ASMAC(iwreg,32:mnumyr,iatyp)
		  ENDIF
		  
        ENDDO  ! iatyp
      ENDDO  ! iwreg
	  
	  DO i = 6,mnumyr
	    DO iwreg = 1, maxwreg
		  WRITE(airunit,'("FTMAC",", ",I5,", ",I3,", ",3(F12.1,", "))') i+1989,iwreg,FTMAC(iwreg,i,:)			
		  WRITE(airunit,'("ASMAC",", ",I5,", ",I3,", ",3(F12.1,", "))') i+1989,iwreg,ASMAC(iwreg,i,:)
		ENDDO
	  ENDDO
	  
    ELSE !  (YRS .ge. FIRST_FCST_YR) THEN
		
	  STK_SUP_TOT(:,:,N) 	= 0.0
	  STK_SUP(:,:,:,N) 		= 0.0
	  STKPA(:,N,:) 			= 0.0
	  STKPP(:,N,:) 			= 0.0
	  STKPTOT(:,:,N)        = 0.0
	  STKCA(:,N,:)			= 0.0
	  STKCP(:,N,:)			= 0.0
	  STKCTOT(:,:,N)       	= 0.0
	  CARGOAC_NEEDED 		= 0.0
	  PASSAC_NEEDED  		= 0.0
	  SCRAPPED				= 0.0
	  UNPARKED(:,:,:,N)  	= 0.0
	  CONVERTED(:,:,:,N) 	= 0.0
	  PARKED(:,:,:,N)    	= 0.0	  
! --------------------------------------------------------------------------------
! Cargo aircraft stock model
! --------------------------------------------------------------------------------
!	  Aircraft scrappage/retirement
      DO iwreg=1,maxwreg
        DO iatyp = 1, maxatyp
          DO iage = maxacage, 2, -1
		  
!			Cargo
			IF (iage.eq.maxacage) THEN																								! Make sure prev yr 48-yr-old aircraft are included (same survac as 47-yr-old)
			  SCRAPPED							 = STKCARGO_ACTIVE(iwreg,iatyp,iage-1,N-1) * (1.0 - SURVAC(iatyp,iage,2)) + &		! Retired planes
												   STKCARGO_ACTIVE(iwreg,iatyp,iage,N-1) * (1.0 - SURVAC(iatyp,iage,2))
			  STKCARGO_PARKED(iwreg,iatyp,iage,N)= STKCARGO_PARKED(iwreg,iatyp,iage-1,N-1) + STKCARGO_PARKED(iwreg,iatyp,iage,N-1)  ! Parked Cargo - no scrappage (100% survival). Except 48-yr old -- all disappear.
			  STKCARGO_ACTIVE(iwreg,iatyp,iage,N)= STKCARGO_ACTIVE(iwreg,iatyp,iage-1,N-1)+STKCARGO_ACTIVE(iwreg,iatyp,iage,N-1) - &! Active Cargo
                                    			   SCRAPPED
			ELSE
			  SCRAPPED							 = STKCARGO_ACTIVE(iwreg,iatyp,iage-1,N-1) * (1.0 - SURVAC(iatyp,iage,2))			! Retired planes
              STKCARGO_ACTIVE(iwreg,iatyp,iage,N)= STKCARGO_ACTIVE(iwreg,iatyp,iage-1,N-1) - SCRAPPED								! Active Cargo
              STKCARGO_PARKED(iwreg,iatyp,iage,N)= STKCARGO_PARKED(iwreg,iatyp,iage-1,N-1)											! Parked Cargo - no scrappage (100% survival)
			ENDIF
			
            STKCARGO(iwreg,iatyp,iage,N) 		 = STKCARGO_ACTIVE(iwreg,iatyp,iage,N) + STKCARGO_PARKED(iwreg,iatyp,iage,N)  		! Total Cargo
			SCRAPPED_TOTAL(iwreg,iatyp,2,N)		 = SCRAPPED_TOTAL(iwreg,iatyp,2, N) + SCRAPPED
            SCRAPPED							 = 0
			
!			Passenger
			IF (iage.eq.maxacage) THEN																								! Make sure prev yr 48-yr-old aircraft are included (same survac as 47-yr-old)
			  SCRAPPED							 = STKPASS_ACTIVE(iwreg,iatyp,iage-1,N-1) * (1.0 - SURVAC(iatyp,iage,1)) + &		! Retired planes
												   STKPASS_ACTIVE(iwreg,iatyp,iage,N-1) * (1.0 - SURVAC(iatyp,iage,1))          
			  STKPASS_PARKED(iwreg,iatyp,iage,N) = STKPASS_PARKED(iwreg,iatyp,iage-1,N-1) + STKPASS_PARKED(iwreg,iatyp,iage,N-1)    ! Parked Passenger - no scrappage (100% survival)
			  STKPASS_ACTIVE(iwreg,iatyp,iage,N) = STKPASS_ACTIVE(iwreg,iatyp,iage-1,N-1) + STKPASS_ACTIVE(iwreg,iatyp,iage,N-1) - &! Active Passenger
                                    			   SCRAPPED
			ELSE                                 
			  SCRAPPED							 = STKPASS_ACTIVE(iwreg,iatyp,iage-1,N-1) * (1.0 - SURVAC(iatyp,iage,1))			! Retired planes  
              STKPASS_ACTIVE(iwreg,iatyp,iage,N) = STKPASS_ACTIVE(iwreg,iatyp,iage-1,N-1) - SCRAPPED								! Active Passenger
              STKPASS_PARKED(iwreg,iatyp,iage,N) = STKPASS_PARKED(iwreg,iatyp,iage-1,N-1)											! Parked Passenger - no scrappage (100% survival)
			ENDIF

			STKPASS(iwreg,iatyp,iage,N)        	 = STKPASS_ACTIVE(iwreg,iatyp,iage,N) + STKPASS_PARKED(iwreg,iatyp,iage,N)			! Total Passenger
			SCRAPPED_TOTAL(iwreg,iatyp,1,N)		 = SCRAPPED_TOTAL(iwreg,iatyp,1, N) + SCRAPPED
			SCRAPPED							 = 0		  			
												 
			STK_SUP(iwreg,iatyp,iage,N) 		 = STKCARGO(iwreg,iatyp,iage,N) + STKPASS(iwreg,iatyp,iage,N)						! Total Passenger + Cargo
          ENDDO	  ! iage
	      MIN_PARK(iwreg,iatyp) = 0.0
	      MIN_PARK(iwreg,iatyp) = MAX(0.0,(SUM(STKPASS_PARKED(iwreg,iatyp,:,N)) + SUM(STKPASS_ACTIVE(iwreg,iatyp,:,N))) * MIN_PRK_SHR(iwreg,iatyp))		! Maintain historical levels of parked aircraft as a share of the total fleet
		  
        ENDDO	! iatyp
      ENDDO	! iwreg

!     Estimate shortfall in aircraft to meet demand [CARGOAC_NEEDED]
!	  Add this shortfall to the tally of add'l planes needed due to retirements [CARGOAC_NEEDED]
!	  If there is excess, leave the excess planes in the stock (assumption: planes only retire due to age, not due to excess cargo capacity).
	  WRITE(airunit,*)' '
	  WRITE(airunit,*)'Aircraft supply v demand - year region iatyp CARGOAC_NEEDED STKPASS_CARGO'
      DO iwreg=1,maxwreg
        DO iatyp = 1, maxatyp
!		  Cargo
            IF (SUM(STKCARGO_ACTIVE(iwreg,iatyp,:,N-1)).ne.0.0) THEN													! Otherwise there would be a negative CARGOAC_NEEDED. 		
              CARGOAC_NEEDED(iwreg,iatyp) = SUM(CARGOAC_FTM_DMD(iwreg,iatyp,:,N)) * 1000000. / FTMAC(iwreg,N,iatyp) &	! Calculate deficit in dedicated freighters 
                                       - SUM(STKCARGO_ACTIVE(iwreg,iatyp,:,N))											!  (freighters needed - available freighters)
		      WRITE(airunit,'(i4,a,i2,F8.1,F10.1)')YRS, reg_def(iwreg), iatyp, CARGOAC_NEEDED(iwreg,iatyp), SUM(STKCARGO_ACTIVE(iwreg,iatyp,:,N))
			ELSE																										
			  WRITE(airunit,'("No ",a," freighters in ",a)')reg_atyp(iatyp),reg_def(iwreg)								! Unpark or buy if there is demand
			  IF (SUM(CARGOAC_FTM_DMD(iwreg,iatyp,:,N)).gt.0.0) CARGOAC_NEEDED(iwreg,iatyp) = SUM(CARGOAC_FTM_DMD(iwreg,iatyp,:,N)) * 1000000. / FTMAC(iwreg,N,iatyp)
		    ENDIF
        ENDDO
      ENDDO		! iwreg  
	
!     Match cargo aircraft fleet capacity to FTM demand and replace SCRAPPED_TOTAL/retired cargo aircraft [total to add = CARGOAC_NEEDED]
!	  4 successive sources of aircraft: 1) parked cargo planes, 2) parked passenger planes, 3) older active passenger planes, 4) new cargo planes
	  WRITE(airunit,*)' '
	  WRITE(airunit,*)'Fill cargo capacity need [CARGOAC_NEEDED]'
      DO iwreg=1,maxwreg
	    DO iatyp = 1,maxatyp		  
		  IF (CARGOAC_NEEDED(iwreg,iatyp) .le. 0.0) THEN													! If there is enough existing FTM capacity, park some planes
		    DO iage = maxacage, MINAGE_PRK_F, -1
			  IF (STKCARGO_ACTIVE(iwreg,iatyp,iage,N).eq.0.0) CYCLE
			  IF (STKCARGO_ACTIVE(iwreg,iatyp,iage,N).ge.ABS(CARGOAC_NEEDED(iwreg,iatyp))) THEN
			    STKCARGO_PARKED(iwreg,iatyp,iage,N) = STKCARGO_PARKED(iwreg,iatyp,iage,N) + &
				                                      ABS(CARGOAC_NEEDED(iwreg,iatyp))
				STKCARGO_ACTIVE(iwreg,iatyp,iage,N) = STKCARGO_ACTIVE(iwreg,iatyp,iage,N) - &
				                                      ABS(CARGOAC_NEEDED(iwreg,iatyp))
				PARKED(iwreg,iatyp,2,N)			    = PARKED(iwreg,iatyp,2,N) + ABS(CARGOAC_NEEDED(iwreg,iatyp))
			    WRITE(airunit,'(i4,": Parked ",F5.1,i3,"-year-old ",a," freighters in ",a)') & 
				      YRS,CARGOAC_NEEDED(iwreg,iatyp),iage,reg_atyp(iatyp),reg_def(iwreg)
				CARGOAC_NEEDED(iwreg,iatyp) 		= 0.0
				GO TO 2801
			  ELSE
			    STKCARGO_PARKED(iwreg,iatyp,iage,N) = STKCARGO_PARKED(iwreg,iatyp,iage,N) + &
													  STKCARGO_ACTIVE(iwreg,iatyp,iage,N)
				PARKED(iwreg,iatyp,2,N)			    = PARKED(iwreg,iatyp,2,N) + STKCARGO_ACTIVE(iwreg,iatyp,iage,N)
			    WRITE(airunit,'(i4,": Parked ",F5.1,i3,"-year-old ",a," freighters in ",a)') & 
				      YRS,STKCARGO_ACTIVE(iwreg,iatyp,iage,N),iage,reg_atyp(iatyp),reg_def(iwreg)
				STKCARGO_ACTIVE(iwreg,iatyp,iage,N) = 0.0
		      ENDIF
			ENDDO
		  ELSE

!	        First, unpark parked cargo planes																	! MDR -- need to investigate age limit. do they want 50-yr old planes back?
		    DO iage = 1, maxacage
			  IF (STKCARGO_PARKED(iwreg,iatyp,iage,N).eq.0.0) CYCLE											! If there aren't any parked cargo planes available, skip to next vintage		    
		      IF (STKCARGO_PARKED(iwreg,iatyp,iage,N).ge.CARGOAC_NEEDED(iwreg,iatyp)) THEN					! If the current vintage meets or exceeds the deficit
			    STKCARGO_ACTIVE(iwreg,iatyp,iage,N) = STKCARGO_ACTIVE(iwreg,iatyp,iage,N) + &					!  unpark what we need
			                                          CARGOAC_NEEDED(iwreg,iatyp)
			    STKCARGO_PARKED(iwreg,iatyp,iage,N) = STKCARGO_PARKED(iwreg,iatyp,iage,N) - &
			                                          CARGOAC_NEEDED(iwreg,iatyp)
			    UNPARKED(iwreg,iatyp,2,N) 		  	= UNPARKED(iwreg,iatyp,2,N) + CARGOAC_NEEDED(iwreg,iatyp)
			    WRITE(airunit,'(i4,": Unparked ",F5.1,i3,"-year-old ",a," freighters in ",a)') & 
			  	    YRS,CARGOAC_NEEDED(iwreg,iatyp),iage,reg_atyp(iatyp),reg_def(iwreg)
			    CARGOAC_NEEDED(iwreg,iatyp)		  	= 0.0
                GO TO 2801																					!  and jump to the next region
              ELSE																							! If the current vintage does not meet or exceed the deficit
			    STKCARGO_ACTIVE(iwreg,iatyp,iage,N) = STKCARGO_ACTIVE(iwreg,iatyp,iage,N) + &					!  unpark the whole vintage and move to the next vintage
			                                          STKCARGO_PARKED(iwreg,iatyp,iage,N)
			    CARGOAC_NEEDED(iwreg,iatyp)		  	= CARGOAC_NEEDED(iwreg,iatyp) - &
			  										STKCARGO_PARKED(iwreg,iatyp,iage,N)
			    UNPARKED(iwreg,iatyp,2,N) 		  	= UNPARKED(iwreg,iatyp,2,N) + STKCARGO_PARKED(iwreg,iatyp,iage,N)
			    WRITE(airunit,'(i4,": Unparked ",F5.1,i3,"-year-old ",a," freighters in ",a)') & 
			  	    YRS,STKCARGO_PARKED(iwreg,iatyp,iage,N),iage,reg_atyp(iatyp),reg_def(iwreg)
  			    STKCARGO_PARKED(iwreg,iatyp,iage,N) = 0.0		
		      ENDIF
		    ENDDO  ! iage
			
		    
			IF ((SUM(STKPASS_PARKED(iwreg,iatyp,:,N))-MIN_PARK(iwreg,iatyp)).gt.0.0) THEN
!		      Next, convert parked passenger planes
		      DO iage = maxacage, MINAGE_C_PPRK, -1
			    IF (STKPASS_PARKED(iwreg,iatyp,iage,N).eq.0.0) CYCLE										! If there aren't any parked passenger planes available, skip to next vintage
		        UNPARK = 0.0
				IF (STKPASS_PARKED(iwreg,iatyp,iage,N).ge.CARGOAC_NEEDED(iwreg,iatyp)) THEN					! If the current vintage meets or exceeds the deficit
				  UNPARK = MIN(MAX(0.0,SUM(STKPASS_PARKED(iwreg,iatyp,:,N)) - MIN_PARK(iwreg,iatyp)), &		! Remaining "excess" parked capacity
						       CARGOAC_NEEDED(iwreg,iatyp))													! add'l active cargo aircraft needed
			      STKPASS_PARKED(iwreg,iatyp,iage,N)  	= STKPASS_PARKED(iwreg,iatyp,iage,N) - UNPARK
			      STKCARGO_ACTIVE(iwreg,iatyp,iage,N) 	= STKCARGO_ACTIVE(iwreg,iatyp,iage,N) + UNPARK		!  unpark what we need
			      CONVERTED(iwreg,iatyp,1,N) 			= CONVERTED(iwreg,iatyp,1,N) + UNPARK
			      CARGOAC_NEEDED(iwreg,iatyp)		 	= CARGOAC_NEEDED(iwreg,iatyp) - UNPARK
			      WRITE(airunit,'(i4,": Converted ",F5.1,i3,"-year-old ",a," parked passenger aircraft in ",a)') & 
				        YRS,UNPARK,iage,reg_atyp(iatyp),reg_def(iwreg)
			      IF(CARGOAC_NEEDED(iwreg,iatyp).eq.0.0) GO TO 2801											! If demand has been met, next region
                ELSE																						! If the current vintage does not meet or exceed the deficit
				  UNPARK = MIN(STKPASS_PARKED(iwreg,iatyp,iage,N), &									
						       MAX(0.0,SUM(STKPASS_PARKED(iwreg,iatyp,:,N))-MIN_PARK(iwreg,iatyp)))			  
			  
			      STKPASS_PARKED(iwreg,iatyp,iage,N)  = STKPASS_PARKED(iwreg,iatyp,iage,N) - UNPARK
			      STKCARGO_ACTIVE(iwreg,iatyp,iage,N) = STKCARGO_ACTIVE(iwreg,iatyp,iage,N) + UNPARK
			      CARGOAC_NEEDED(iwreg,iatyp)		  = CARGOAC_NEEDED(iwreg,iatyp) - UNPARK
			      CONVERTED(iwreg,iatyp,1,N) 		  = CONVERTED(iwreg,iatyp,1,N) + UNPARK
			  
			      WRITE(airunit,'(i4,": Converted ",F5.1,i3,"-year-old ",a," parked passenger aircraft in ",a)') & 
				        YRS,UNPARK,iage,reg_atyp(iatyp),reg_def(iwreg)
		        ENDIF
		      IF(SUM(STKPASS_PARKED(iwreg,iatyp,:,N)).le.MIN_PARK(iwreg,iatyp)) GO TO 2802				! If but minimum parked fleet threshold hit, proceed to the next option
		      ENDDO  ! iage
		    ENDIF  
		  
		    2802 CONTINUE
		  
!		    Then, convert older active passenger planes
		    DO iage = maxacage, MINAGE_C_PACT, -1
			  IF (STKPASS_ACTIVE(iwreg,iatyp,iage,N).eq.0.0) CYCLE											! If there aren't any parked passenger planes available, skip to next vintage
		      IF (STKPASS_ACTIVE(iwreg,iatyp,iage,N).ge.CARGOAC_NEEDED(iwreg,iatyp)) THEN						! If the current vintage meets or exceeds the deficit
			    STKCARGO_ACTIVE(iwreg,iatyp,iage,N) = STKCARGO_ACTIVE(iwreg,iatyp,iage,N) + &					!  unpark what we need
			                                          CARGOAC_NEEDED(iwreg,iatyp)
			    STKPASS_ACTIVE(iwreg,iatyp,iage,N)  = STKPASS_ACTIVE(iwreg,iatyp,iage,N) - &
			                                          CARGOAC_NEEDED(iwreg,iatyp)
			    CONVERTED(iwreg,iatyp,2,N) 			= CONVERTED(iwreg,iatyp,2,N) + CARGOAC_NEEDED(iwreg,iatyp)
			    WRITE(airunit,'(i4,": Converted ",F5.1,i3,"-year-old ",a," active passenger aircraft in ",a)') & 
				      YRS,CARGOAC_NEEDED(iwreg,iatyp),iage,reg_atyp(iatyp),reg_def(iwreg)
			    CARGOAC_NEEDED(iwreg,iatyp)		  = 0.0
                GO TO 2801																					!  and jump to the next region
              ELSE																							! If the current vintage does not meet or exceed the deficit
			    STKCARGO_ACTIVE(iwreg,iatyp,iage,N) = STKCARGO_ACTIVE(iwreg,iatyp,iage,N) + &					!  unpark the whole vintage and move to the next vintage
			                                          STKPASS_ACTIVE(iwreg,iatyp,iage,N)
			    CARGOAC_NEEDED(iwreg,iatyp)		  	= CARGOAC_NEEDED(iwreg,iatyp) - &
				  									  STKPASS_ACTIVE(iwreg,iatyp,iage,N)
			    CONVERTED(iwreg,iatyp,2,N) 		  	= CONVERTED(iwreg,iatyp,2,N) + STKPASS_ACTIVE(iwreg,iatyp,iage,N)
			    WRITE(airunit,'(i4,": Converted ",F5.1,i3,"-year-old ",a," active passenger aircraft in ",a)') & 
				      YRS,STKPASS_ACTIVE(iwreg,iatyp,iage,N),iage,reg_atyp(iatyp),reg_def(iwreg)
  			    STKPASS_ACTIVE(iwreg,iatyp,iage,N)  = 0.0		
		      ENDIF
		    ENDDO  ! iage		  
		  
!		    Finally, resort to purchasing new freighters
		    STKCARGO_ACTIVE(iwreg,iatyp,1,N) = STKCARGO_ACTIVE(iwreg,iatyp,1,N) + CARGOAC_NEEDED(iwreg,iatyp)
		    WRITE(airunit,'(i4,": Purchased ",F5.1," ",a," freighters in ",a)') & 
						    YRS,CARGOAC_NEEDED(iwreg,iatyp),reg_atyp(iatyp),reg_def(iwreg)
		  ENDIF			  
		  2801 CONTINUE

!		  Populate final stock arrays for reporting		  
		  STKPA(iwreg,N,iatyp)  = SUM(STKPASS_ACTIVE(iwreg,iatyp,:,N))
		  STKPP(iwreg,N,iatyp)  = SUM(STKPASS_PARKED(iwreg,iatyp,:,N))
		  STKCA(iwreg,N,iatyp) = SUM(STKCARGO_ACTIVE(iwreg,iatyp,:,N))
		  STKCP(iwreg,N,iatyp) = SUM(STKCARGO_PARKED(iwreg,iatyp,:,N))
		  
		  STKPTOT(iwreg,iatyp,N) = STKPA(iwreg,N,iatyp) + STKPP(iwreg,N,iatyp)
		  STKCTOT(iwreg,iatyp,N) = STKCA(iwreg,N,iatyp) + STKCP(iwreg,N,iatyp)
		  
		  STK_SUP_TOT(iwreg,iatyp,N)  = STKPTOT(iwreg,iatyp,N) + STKCTOT(iwreg,iatyp,N)
		  
		ENDDO  ! iatyp
      ENDDO ! iwreg	
	  
! --------------------------------------------------------------------------------
! Passenger aircraft
!   Passenger stock is calculated after cargo so that cargo can pick through
!   passenger planes to convert before those planes are scrapped.
! -------------------------------------------------------------------------------- 
!     Estimate shortfall in aircraft to meet demand [PASSAC_NEEDED]
!	  If there is excess, leave the excess planes in the stock (assumption: planes only retire due to age, not due to excess passenger capacity).
	  WRITE(airunit,*)' '
	  WRITE(airunit,*)'Aircraft supply v demand - year region iatyp PASSAC_NEEDED STKPASS_ACTIVE'
      DO iwreg=1,maxwreg
        DO iatyp = 1, maxatyp
		  IF (SUM(STKPASS_ACTIVE(iwreg,iatyp,:,N-1)).ne.0.0) THEN														! If there were planes
            PASSAC_NEEDED(iwreg,iatyp) = ASMDEMD(iwreg,iatyp,N) * 1000000. / ASMAC(iwreg,N,iatyp) &						! Calculate add'l aircraft needed 
                                         - SUM(STKPASS_ACTIVE(iwreg,iatyp,:,N))											!  (aircraft needed - available aircraft)
		    WRITE(airunit,'(i4,i3,i2,F8.1,F10.1)')YRS, iwreg, iatyp, PASSAC_NEEDED(iwreg,iatyp), SUM(STKPASS_ACTIVE(iwreg,iatyp,:,N))
		  ELSE
		    WRITE(airunit,'("No ",a," passenger AC in ",a)')reg_atyp(iatyp),reg_def(iwreg)								! Unpark or buy if there is demand
			WRITE(airunit,'(3F10.2)')SUM(STKPASS_ACTIVE(iwreg,iatyp,:,N)), ASMDEMD(iwreg,iatyp,N), ASMAC(iwreg,N,iatyp)
			IF (ASMDEMD(iwreg,iatyp,N).gt.0.0) PASSAC_NEEDED(iwreg,iatyp) = ASMDEMD(iwreg,iatyp,N) * 1000000. / ASMAC(iwreg,N,iatyp)
		  ENDIF
        ENDDO
      ENDDO		! iwreg:maxwreg  

!	  Match passenger aircraft supply to RPM demand
      DO iwreg = 1, maxwreg
	    DO iatyp = 1, maxatyp
		  IF (PASSAC_NEEDED(iwreg,iatyp) .le. 0.0) THEN													! If there is enough existing RPM capacity, park some planes
		    DO iage = maxacage, MINAGE_PRK_P, -1
			  IF (STKPASS_ACTIVE(iwreg,iatyp,iage,N).eq.0.0) CYCLE
			  IF (STKPASS_ACTIVE(iwreg,iatyp,iage,N).ge.ABS(PASSAC_NEEDED(iwreg,iatyp))) THEN
			    STKPASS_PARKED(iwreg,iatyp,iage,N) = STKPASS_PARKED(iwreg,iatyp,iage,N) + &
				                                     ABS(PASSAC_NEEDED(iwreg,iatyp))
				STKPASS_ACTIVE(iwreg,iatyp,iage,N) = STKPASS_ACTIVE(iwreg,iatyp,iage,N) - &
				                                     ABS(PASSAC_NEEDED(iwreg,iatyp))
				PARKED(iwreg,iatyp,1,N)			   = PARKED(iwreg,iatyp,1,N) + ABS(PASSAC_NEEDED(iwreg,iatyp))
				PASSAC_NEEDED(iwreg,iatyp) 		   = 0.0
				GO TO 3801
			  ELSE
			    STKPASS_PARKED(iwreg,iatyp,iage,N) = STKPASS_PARKED(iwreg,iatyp,iage,N) + &
													 STKPASS_ACTIVE(iwreg,iatyp,iage,N)
				PARKED(iwreg,iatyp,1,N)			   = PARKED(iwreg,iatyp,1,N) + STKPASS_ACTIVE(iwreg,iatyp,iage,N)
				STKPASS_ACTIVE(iwreg,iatyp,iage,N) = 0.0
		      ENDIF
			ENDDO
			GO TO 3801
		  ENDIF
		  
		  MAX_UNPARK = SUM(STKPASS_PARKED(iwreg,iatyp,:,N))*MAX_UNPRK_SHR									! Set a limit on the number of planes that can be unparked in a given year

!		  MDR: This is a temporary adjustment to allow more planes to unpark after covid
		  IF (YRS.EQ.2022) MAX_UNPARK = (SUM(STKPASS_PARKED(iwreg,iatyp,:,N))-MIN_PARK(iwreg,iatyp))*0.9
		  IF (YRS.EQ.2023) MAX_UNPARK = (SUM(STKPASS_PARKED(iwreg,iatyp,:,N))-MIN_PARK(iwreg,iatyp))*0.9		! MDRIEO2023 changed from 0.6 (and 0.3 for 2024)
		  IF (YRS.EQ.2024) MAX_UNPARK = (SUM(STKPASS_PARKED(iwreg,iatyp,:,N))-MIN_PARK(iwreg,iatyp))*0.6

		  IF ((SUM(STKPASS_PARKED(iwreg,iatyp,:,N))-MIN_PARK(iwreg,iatyp)).gt.0.0) THEN		! If there are jets available to unpark

!		  Start by unparking planes until demand is met [PASSAC_NEEDED]
		  DO iage = 1, MAXAGE_UNPRK_P																! MDR -- Review 35-yr "oldest age willing to bring back into service" age (perhaps different by region? maybe pin to average age?)
			IF (STKPASS_PARKED(iwreg,iatyp,iage,N).eq.0.0) CYCLE									! If there aren't any parked passenger planes available, skip to next vintage
			UNPARK = 0.0
			IF (STKPASS_PARKED(iwreg,iatyp,iage,N).ge.PASSAC_NEEDED(iwreg,iatyp)) THEN				! If this vintage has enough parked planes to fill the aircraft supply deficit
			  UNPARK = MIN(MAX_UNPARK, &															! Annual limit 
			               MAX(0.0,SUM(STKPASS_PARKED(iwreg,iatyp,:,N))-MIN_PARK(iwreg,iatyp)), &	! Parked capacity available for unparking (or 0, if <0)
						   PASSAC_NEEDED(iwreg,iatyp))												! Add'l active capacity needed
			  
			  STKPASS_ACTIVE(iwreg,iatyp,iage,N) = STKPASS_ACTIVE(iwreg,iatyp,iage,N) + UNPARK
			  STKPASS_PARKED(iwreg,iatyp,iage,N) = STKPASS_PARKED(iwreg,iatyp,iage,N) - UNPARK
			  MAX_UNPARK						 = MAX_UNPARK - UNPARK
			  PASSAC_NEEDED(iwreg,iatyp)		 = PASSAC_NEEDED(iwreg,iatyp) - UNPARK
			  UNPARKED(iwreg,iatyp,1,N)		     = UNPARKED(iwreg,iatyp,1,N) + UNPARK
			ELSE
			  UNPARK = MIN(STKPASS_PARKED(iwreg,iatyp,iage,N), &									
						   MAX_UNPARK, &
						   MAX(0.0,SUM(STKPASS_PARKED(iwreg,iatyp,:,N))-MIN_PARK(iwreg,iatyp)))

			  STKPASS_ACTIVE(iwreg,iatyp,iage,N) = STKPASS_ACTIVE(iwreg,iatyp,iage,N) + UNPARK
			  STKPASS_PARKED(iwreg,iatyp,iage,N) = STKPASS_PARKED(iwreg,iatyp,iage,N) - UNPARK			  
		      UNPARKED(iwreg,iatyp,1,N)		     = UNPARKED(iwreg,iatyp,1,N) + UNPARK
			  PASSAC_NEEDED(iwreg,iatyp) 		 = PASSAC_NEEDED(iwreg,iatyp) - UNPARK
			  MAX_UNPARK 		                 = MAX_UNPARK - UNPARK
			ENDIF
			IF (PASSAC_NEEDED(iwreg,iatyp).eq.0.0) GO TO 3801		! If we have met demand, go to the next region
		    IF (MAX_UNPARK.eq.0.0) GO TO 3802						! If we haven't met demand, but have exhausted our unparking limit, go buy some planes
		  ENDDO
		  
		  ENDIF
		  
		  3802 CONTINUE	
		  
!		  Since we still don't have enough aircraft to meet RPM demand, purchase new
		  STKPASS_ACTIVE(iwreg,iatyp,1,N) = STKPASS_ACTIVE(iwreg,iatyp,1,N) + PASSAC_NEEDED(iwreg,iatyp)
		  
		  3801 CONTINUE		! next iwreg, demand has been met
		  
!		  Populate final stock arrays for reporting		  
		  STKPA(iwreg,N,iatyp) = SUM(STKPASS_ACTIVE(iwreg,iatyp,:,N))
		  STKPP(iwreg,N,iatyp) = SUM(STKPASS_PARKED(iwreg,iatyp,:,N))
		  STKCA(iwreg,N,iatyp) = SUM(STKCARGO_ACTIVE(iwreg,iatyp,:,N))
		  STKCP(iwreg,N,iatyp) = SUM(STKCARGO_PARKED(iwreg,iatyp,:,N))
		  
		  STKPTOT(iwreg,iatyp,N) = STKPA(iwreg,N,iatyp) + STKPP(iwreg,N,iatyp)
		  STKCTOT(iwreg,iatyp,N) = STKCA(iwreg,N,iatyp) + STKCP(iwreg,N,iatyp)
		  
		  STK_SUP(iwreg,iatyp,1,N)    = STKPASS_ACTIVE(iwreg,iatyp,1,N) + STKCARGO_ACTIVE(iwreg,iatyp,1,N)
		  WRITE(AIRUNIT,'(i4, ": Sales of ",a," in ",a,":",3F10.2)')YRS,reg_def(iwreg),reg_atyp(iatyp),STKPASS_ACTIVE(iwreg,iatyp,1,N),STKCARGO_ACTIVE(iwreg,iatyp,1,N),STK_SUP(iwreg,iatyp,1,N)
		  STK_SUP_TOT(iwreg,iatyp,N)  = STKPTOT(iwreg,iatyp,N) + STKCTOT(iwreg,iatyp,N)

!		  Average active passenger aircraft age by region and body type
		  STK_AVGAGE_PA(iwreg,iatyp,N) = 0.0
		  IF (SUM(STKPASS_ACTIVE(iwreg,iatyp,:,N)).gt.0.0) THEN
		    DO iage=1,maxacage
		      STK_AVGAGE_PA(iwreg,iatyp,N) = STK_AVGAGE_PA(iwreg,iatyp,N) + STKPASS_ACTIVE(iwreg,iatyp,iage,N)*iage
		    ENDDO
		    STK_AVGAGE_PA(iwreg,iatyp,N) = STK_AVGAGE_PA(iwreg,iatyp,N)/SUM(STKPASS_ACTIVE(iwreg,iatyp,:,N))
		  ENDIF
		 
		 
        ENDDO  ! iatyp
	  ENDDO  ! iwreg
	  

    ENDIF  ! YRS .ge. FIRST_FCST_YR

! =============================================================================
! Calculate stock fuel efficiency by body type and region
!	Note that freight fuel efficiency is not included in the final outputs.
! =============================================================================

!   Adjust MPGs by tech improvement and re-powers (every 10 years). Estimate MPGs across different dimensions (harmonic mean)
	SUM_ASMPG_STK_AGE = 0.0
	SUM_ASMPG_STK_R   = 0.0
	SUM_ASMPG_STK_TYP = 0.0
	SUM_ASMPG_NEW_TYP = 0.0
    
    IF (YRS.ge.LAST_HIST_YR) THEN

!     Stock shares by vintage, rather than ASM shares by vintage, must be used to collapse age dimension (don't have vintaged ASM)
!     Calculate vintage stock shares for each region / body type (nb, wb, rj)	  
	  DO iwreg = 1, maxwreg
	    DO iatyp = 1, maxatyp
	      DO di = 1, domint
	        IF (YRS .ge. FIRST_FCST_YR) THEN 
              ASMPG_VINT(iwreg,iatyp,1,di,N) = 1/(1/ASMPG_VINT(iwreg,iatyp,1,di,N-1)*(1-FUEL_BURN_RED))
			ENDIF
			
			SUM_ASMPG_STK_AGE(iwreg,iatyp,di) = STKPASS_ACTIVE(iwreg,iatyp,1,N)/ASMPG_VINT(iwreg,iatyp,1,di,N)
			
!           Process stock vintaging and engine re-powers			
			DO iage = 2, maxacage

			  IF (YRS.ge.FIRST_FCST_YR) THEN
			    ASMPG_VINT(iwreg,iatyp,iage,di,N) = ASMPG_VINT(iwreg,iatyp,iage-1,di,N-1)

!               Install improved efficiency engine in aircraft every 10 years (1% increase in fuel economy)
			    IF (iage.eq.10 .or. iage.eq.20 .or. iage.eq.30 .or. iage.eq.40) THEN
                  ASMPG_VINT(iwreg,iatyp,iage,di,N) = 1.01 * ASMPG_VINT(iwreg,iatyp,iage,di,N)
                ENDIF				
				
		      ENDIF
			  
			  SUM_ASMPG_STK_AGE(iwreg,iatyp,di) = SUM_ASMPG_STK_AGE(iwreg,iatyp,di) + STKPASS_ACTIVE(iwreg,iatyp,iage,N)/ASMPG_VINT(iwreg,iatyp,iage,di,N)
			  
			ENDDO !iage
			
!           Non-vintaged fuel economy, for use in further aggregation (ASMPG_STK_TYP and ASMPGT)
!			GPTM and RPMPG are calculated for use in energy calculations
		    ASMPG_AVG_AGE(iwreg,iatyp,di,N) = STKPA(iwreg,N,iatyp)/SUM_ASMPG_STK_AGE(iwreg,iatyp,di)
			GPTM(iwreg,N,iatyp,di) = 1/(ASMPG_AVG_AGE(iwreg,iatyp,di,N)*pass_weight/2000)
			IF (YRS.ge.2020.and.YRS.le.2026) GPTM(iwreg,N,iatyp,di) = GPTM(iwreg,N,iatyp,di)*STK_ALIGN_MULT(iwreg,N,iatyp)		! MDRIEO2023 extended phaseout to 2026
			
			IF (PASSAC_RPM_DMD(iwreg,iatyp,di,N).gt.0.0) THEN
			  RPMPG(iwreg,iatyp,di,N) = 1/(GPTM(iwreg,N,iatyp,di)*pass_weight/2000* &
                                             (1 - (1-LOAD_FACTOR(iwreg,N,di))*PCT_PASS_MTOW(iatyp,N))/ &
                                             (1 - (1-LOAD_FACTOR(iwreg,N,di))*(1-(BELLY_RPM_EQ(iwreg,iatyp,di,N)/PASSAC_RPM_DMD(iwreg,iatyp,di,N)))) &
           	  					           )
			ELSE	! if no RPM demand, populate the variable anyway 
			  RPMPG(iwreg,iatyp,di,N) = 1/(GPTM(iwreg,N,iatyp,di)*pass_weight/2000* &
                                             (1 - (1-LOAD_FACTOR(iwreg,N,di))*PCT_PASS_MTOW(iatyp,N))/ &
										     (1 - (1-LOAD_FACTOR(iwreg,N,di))) &
										   )
			ENDIF

!           Region weights for aggregating fuel economy.
			SUM_ASMPG_STK_R(iwreg) = SUM_ASMPG_STK_R(iwreg) + ASM(iwreg,di,iatyp,N)/ASMPG_AVG_AGE(iwreg,iatyp,di,N)
						
		  ENDDO !di
		ENDDO !iatyp
	  ENDDO !iwreg
	  
!     Stock and Sales average fuel economy by body type (wb/nb/rj), for reporting (ASMPG_STK_TYP & ASMPG_NEW_TYP)	  
	  DO iatyp = 1, maxatyp
	    DO iwreg = 1, maxwreg
		  DO di = 1, domint
		    SUM_ASMPG_STK_TYP(iatyp) = SUM_ASMPG_STK_TYP(iatyp) + ASM(iwreg,di,iatyp,N)/ASMPG_AVG_AGE(iwreg,iatyp,di,N)
			SUM_ASMPG_NEW_TYP(iatyp) = SUM_ASMPG_NEW_TYP(iatyp) + ASM(iwreg,di,iatyp,N)/ASMPG_VINT(iwreg,iatyp,1,di,N)
		  ENDDO
		ENDDO	
		
	    ASMPG_STK_TYP(iatyp,N) = sum(ASM(:,:,iatyp,N))/MAX(1.0,SUM_ASMPG_STK_TYP(iatyp))				! Reported
		ASMPG_NEW_TYP(iatyp,N) = sum(ASM(:,:,iatyp,N))/MAX(1.0,SUM_ASMPG_NEW_TYP(iatyp))				! Reported

	  ENDDO !iatyp

!     Calculate total new (1) and stock (2) ASMPGs for reporting (ASMPGT). This is for passenger planes, and
!	  it includes belly freight (converted to available seats using passenger weight)
	  ASMPGT(1,N) = sum(ASM(:,:,:,N)) / &
	                ( (sum(ASM(:,:,nb,N))/MAX(1.0,ASMPG_NEW_TYP(nb,N))) + &
					  (sum(ASM(:,:,wb,N))/MAX(1.0,ASMPG_NEW_TYP(wb,N))) + &
					  (sum(ASM(:,:,rj,N))/MAX(1.0,ASMPG_NEW_TYP(rj,N))))
					  
	  ASMPGT(2,N) = sum(ASM(:,:,:,N)) / &
	                ( (sum(ASM(:,:,nb,N))/MAX(1.0,ASMPG_STK_TYP(nb,N))) + &
					  (sum(ASM(:,:,wb,N))/MAX(1.0,ASMPG_STK_TYP(wb,N))) + &
					  (sum(ASM(:,:,rj,N))/MAX(1.0,ASMPG_STK_TYP(rj,N))))

      QJETR_DI(:,:,:,N) = 0.0
!	  NOTE: Belly freight is in PASSAC_RPM_DMD, and it's energy consumption is therefore in passenger
	  DO iwreg = 1, maxwreg
	    DO iatyp = 1, maxatyp
		  DO di = 1, domint
			QJETR_DI(iwreg,di,1,N) = QJETR_DI(iwreg,di,1,N) + ((PASSAC_RPM_DMD(iwreg,iatyp,di,N) * AIR_MGMT_ADJ(iwreg,N,di) / RPMPG(iwreg,iatyp,di,N))/ 42.0) * CFJFK	! quads, passenger
			QJETR_DI(iwreg,di,2,N) = QJETR_DI(iwreg,di,2,N) + ((CARGOAC_FTM_DMD(iwreg,iatyp,di,N) * GPTM(iwreg,N,iatyp,di))/ 42.0) * CFJFK								! quads, freight
		  ENDDO
		ENDDO
		
		IF (YRS.ge.FIRST_FCST_YR) QJETR_NUS(iwreg,N) = sum(QJETR_DI(iwreg,:,:,N))
		
	  ENDDO
	  
      IF (YRS.ge.FIRST_FCST_YR) THEN
        QJETR(11,N)  = QJETR_NUS(1,N) 		! quads		
      ENDIF
	
	ENDIF ! YRS.ge.LAST_HIST_YR  !MDR0228 this overwrites historical value; may want to do gt.LAST_HIST_YR

! ==================================================================================================================
! Aviation gasoline projection
! This just results in a flat line projection... -- MDR
! ==================================================================================================================

!   Calculate aviation gas demand
    GAMMA   = 220.0
    KAPPA   = 0.1895
    IF (YRS.ge.FIRST_FCST_YR) QAGTR(11,N)=QAGTR(11,24)+GAMMA*EXP(-KAPPA*(YRS-1979)) 
    
	IF ((CURIYR.eq.LASTYR).and.(FCRL.eq.1)) THEN		! NEMSWEPSSWITCH   NEMS version
	!IF ((CURIYR.eq.LASTYR)) THEN								! NEMSWEPSSWITCH   WEPS version
	  WRITE(AIRUNIT,*)'MDR Check -- QJETR, QAGTR Projection'
	  DO i = iy,mnumyr
	    WRITE(AIRUNIT,'(i4,2F10.2)')i+1989,QJETR(11,i),QAGTR(11,i)
	  ENDDO
	ENDIF

!   Regionalize US commercial jet fuel and aviation gasoline
    DO iregn=1,mnumcr-2
     QJETR(iregn,N) = QJETR(11,N) * SEDSHRJF(iregn,N)
     QAGTR(iregn,N) = QAGTR(11,N) * SEDSHRJF(iregn,N)
    ENDDO

!   Close files
    IF (CURIYR.eq.LASTYR .and. FCRL.eq.1) THEN 
      CLOSE(airunit)
      CLOSE(ageunit)
    ENDIF

    RETURN
  END SUBROUTINE TAIREFF

! *******************************************************************************

SUBROUTINE TAREPORT
    USE AIRMOD
    IMPLICIT NONE
    
    INTEGER i, j, k, r

! ... ***** TABLE 57 *******

! NEMS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
    AIROUT( 1,N) = PJFTR(11,N)     	         	   ! jet fuel cost in 87$
! NEMS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------
    !IF(N.ge.(pFFYr-1989)) AIROUT(1,N) = PJFTR(1,N+1989)
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

!    AIROUT( 2,N) = YIELD(1,N)                      ! ticket price - domestic
!    AIROUT( 3,N) = YIELD(2,N)                      ! ticket price - international
!    AIROUT( 4,N) = YIELD(2,N)                      ! ticket price - Non U.S. 

!...Calculate U.S. Load Factor
    AIROUT( 2,N) = LOAD_FACTOR(us,N,dom)         ! load factor-dom
    AIROUT( 3,N) = LOAD_FACTOR(us,N,int)         ! load factor-int
    	
!...Writes for ftab
    DO j = 0,15
!	  Revenue passenge miles (note these are dimensioned a little differently in AIROUT v the proceeding metrics)
	  AIROUT(4+j,N) 	= RPMT(j+1,DOM,N)/1000.0	! Domestic revenue passenger miles
	  AIROUT(5+15+j,N)	= RPMT(j+1,INT,N)/1000.0	! International revenue passenger miles
!	  Freight ton mile demand (dedicated freighters + passenger belly freight)
	  AIROUT(39+j,N) 	= SUM(FTM(j+1,N,:))/1000.0
!	  Seat-mile demand
	  AIROUT(58+4*j,N)	= SMDEMD(j+1,   N)/1000.0
	  AIROUT(59+4*j,N)	= ASMDEMD(j+1,1,N)/1000.0
	  AIROUT(60+4*j,N)	= ASMDEMD(j+1,2,N)/1000.0
	  AIROUT(61+4*j,N)	= ASMDEMD(j+1,3,N)/1000.0
!	  New aircraft deliveries	
      AIROUT(126+4*j,N) = STK_SUP(j+1,1,1,N)+STK_SUP(j+1,2,1,N)+STK_SUP(j+1,3,1,N)
      AIROUT(127+4*j,N) = STK_SUP(j+1,1,1,N)
      AIROUT(128+4*j,N) = STK_SUP(j+1,2,1,N)
      AIROUT(129+4*j,N) = STK_SUP(j+1,3,1,N)
!	  Stock, total (pass + cargo)
      AIROUT(194+4*j,N) = STK_SUP_TOT(j+1,1,N)+STK_SUP_TOT(j+1,2,N)+STK_SUP_TOT(j+1,3,N)
      AIROUT(195+4*j,N) = STK_SUP_TOT(j+1,1,N)
      AIROUT(196+4*j,N) = STK_SUP_TOT(j+1,2,N)
      AIROUT(197+4*j,N) = STK_SUP_TOT(j+1,3,N)
!	  Stock, passenger active
      AIROUT(262+4*j,N) = STKPA(j+1,N,1)+STKPA(j+1,N,2)+ STKPA(j+1,N,3)
      AIROUT(263+4*j,N) = STKPA(j+1,N,1) 
      AIROUT(264+4*j,N) = STKPA(j+1,N,2) 
      AIROUT(265+4*j,N) = STKPA(j+1,N,3)

!	  Stock, passenger parked
      AIROUT(330+4*j,N) = STKPP(j+1,N,1)+ STKPP(j+1,N,2) + STKPP(j+1,N,3) 
      AIROUT(331+4*j,N) = STKPP(j+1,N,1) 
      AIROUT(332+4*j,N) = STKPP(j+1,N,2) 
      AIROUT(333+4*j,N) = STKPP(j+1,N,3)
!	  Dedicated freighter stock
	  AIROUT(398+j,N)	= SUM(STKCTOT(j+1,:,N))		! Active AND parked, ALL body types
!	  Jet fuel consumption
	  AIROUT(415+j,N)	= QJETR_NUS(j+1,N)
    ENDDO
!	Totals
!	Revenue passenger miles (not incl. belly freight)
	AIROUT(36,N)  = SUM(RPMT(:,DOM,N))/1000.0
	AIROUT(37,N)  = SUM(RPMT(:,INT,N))/1000.0
	AIROUT(38,N)  = SUM(RPMT(:,:,N))/1000.0
!	Freight ton-mile demand
	AIROUT(55,N)  = SUM(FTM(:,N,DOM))/1000.0
	AIROUT(56,N)  = SUM(FTM(:,N,INT))/1000.0
	AIROUT(57,N)  = SUM(FTM(:,N,  :))/1000.0
!	Seat-miles
	AIROUT(122,N) = SUM(SMDEMD(:   ,N))/1000.0
	AIROUT(123,N) = SUM(ASMDEMD(:,1,N))/1000.0
	AIROUT(124,N) = SUM(ASMDEMD(:,2,N))/1000.0
	AIROUT(125,N) = SUM(ASMDEMD(:,3,N))/1000.0
!	Deliveries
	AIROUT(190,N) = SUM(STK_SUP(:,:,1,N))
	AIROUT(191,N) = SUM(STK_SUP(:,1,1,N))
	AIROUT(192,N) = SUM(STK_SUP(:,2,1,N))
	AIROUT(193,N) = SUM(STK_SUP(:,3,1,N))
!	Stock, total (pass + cargo)
	AIROUT(258,N) = SUM(STK_SUP_TOT(:,:,N))
	AIROUT(259,N) = SUM(STK_SUP_TOT(:,1,N))
	AIROUT(260,N) = SUM(STK_SUP_TOT(:,2,N))
	AIROUT(261,N) = SUM(STK_SUP_TOT(:,3,N))
!	Stock, passenger active
	AIROUT(326,N) = SUM(STKPA(:,N,:))
	AIROUT(327,N) = SUM(STKPA(:,N,1))
	AIROUT(328,N) = SUM(STKPA(:,N,2))
	AIROUT(329,N) = SUM(STKPA(:,N,3))
!	Stock, passenger parked
	AIROUT(394,N) = SUM(STKPP(:,N,:))
	AIROUT(395,N) = SUM(STKPP(:,N,1))
	AIROUT(396,N) = SUM(STKPP(:,N,2))
	AIROUT(397,N) = SUM(STKPP(:,N,3))
!	Stock, cargo (active and parked)
	AIROUT(414,N) = SUM(STKCTOT(:,:,N))


! ************************************ TESTING/DEBUG ************************************
	IF ((N.eq.mnumyr).and.(FCRL.eq.1)) THEN		! NEMSWEPSSWITCH   NEMS version
	!IF ((N.eq.mnumyr)) THEN						! NEMSWEPSSWITCH   WEPS version
	  WRITE(airunit,*)'Energy check'
	  DO iwreg = 1, maxwreg
	    WRITE(airunit,*)reg_def(iwreg),QJETR_NUS(iwreg,21:MNUMYR)
	  ENDDO
	ENDIF
! ************************************ TESTING/DEBUG ************************************
    
!...Calculate revenue passenger miles for domestic
    TRTRAVLD(3,N) = LFDOMAVG(N)                     				! load factor
    
!...Revenue passenger miles for international
    TRTRAVLD(4,N) = (RPMT(US,INT,N)/1000.0)
    TRTRAVLD(5,N) = LFINTAVG(N)

!...Revenue ton miles for freight
    TRTRAVLD(6,N) = SUM(FTM(1,N,:))/1000.0
    TRTRAVLD(7,N) = WLD_GDP(1,N)
    TRTRAVLD(8,N) = MC_XGR(N)

    TRTRAVLD(9,N)  = YIELD(1,N)                    									! ticket price
    TRTRAVLD(10,N) = MC_YPDR(11,N)              									! disposable income	


! NEMS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------	
	TRTRAVLD( 11,N) = PJFTR(11,N)     	         	   		! jet fuel cost in 87$
! NEMS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------	
	!IF(N.ge.(pFFYr-1989)) TRTRAVLD(11,N) = PJFTR(1,N+1989)
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------

!...Seat miles demanded
    TRSTMDEM(1,N) = SMDEMD(1,N) / 1000.0

!...Aircraft new efficiency
    TRAIREFFN(1,N) = ASMPG_NEW_TYP(1,N)
    TRAIREFFN(2,N) = ASMPG_NEW_TYP(2,N)
    TRAIREFFN(3,N) = ASMPG_NEW_TYP(3,N)
    TRAIREFFN(4,N) = ASMPGT(1,N)

!...Aircraft stock efficiency
    TRAIREFFS(1,N) = ASMPG_STK_TYP(1,N)
    TRAIREFFS(2,N) = ASMPG_STK_TYP(2,N)
    TRAIREFFS(3,N) = ASMPG_STK_TYP(3,N)
    TRAIREFFS(4,N) = ASMPGT(2,N)

! WEPS section START -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------	
!!! Populate global WEPS variables
!
!	PMode_TonMiles(:,n+1989,5) 	= 0.0
!	FMode_TonMiles(:,n+1989,7)	= 0.0
!    PassCons(:,n+1989,:,5) 	  	= 0.0
!	FreightCons(:,n+1989,:,7) 	= 0.0
!	
!	DO iwreg = 1, maxwreg
!	  PMode_TonMiles(iwreg,n+1989,5) 	= SUM(RPMT(iwreg,:,n))
!	  FMode_TonMiles(iwreg,n+1989,7) 	= SUM(FTM(iwreg,N,:))
!	  PassCons(iwreg,n+1989,3,5)  		= SUM(QJETR_DI(iwreg,:,1,N))		! Includes belly freight
!	  FreightCons(iwreg,n+1989,3,7)  	= SUM(QJETR_DI(iwreg,:,2,N))		! DOES NOT include belly freight
!	ENDDO
!
!    DO i=1,16
!      PassCons(i,n+1989,11,5) = PassCons(i,n+1989,3,5)
!	  IF (Isnan(PassCons(i,n+1989,11,5)).or.PassCons(i,n+1989,11,5).gt.99999) WRITE(airunit,*)'check air',i,n,PassCons(i,n+1989,3,5),airout(294,n)
!    ENDDO
!	
!	
!IF((N+1989).ge.pFFYr) THEN
!!!	Air tech variables
!	DO i = 1,4             								! Type of airplane (1=narrow body; 2=wide body; 3=regional; 4=average)
!	  Airefficiencynew(i,n+1989) = TRAIREFFN(i,n)       !(seat miles per gallon)
!	  Airefficiencystock(i,n+1989) = TRAIREFFS(i,n)     !(seat miles per gallon)
!	ENDDO
!
!!!	Air variables
!	DO r = 1,maxwreg
!!!	  Revenue passenger miles (billions)
!	  Airpassengerrevenuemilesdomestic(r,n+1989) 	  = RPMT(r,DOM,N)/1000.0
!	  Airpassengerrevenuemilesinternational(r,n+1989) = RPMT(r,INT,N)/1000.0
!!!	  Available seat-miles (billions)
!	  Airpassengerseatmilesdemanded(r,n+1989) 		  = SMDEMD(r,N)/1000.0
!!!	  Freight ton-miles (billions)
!	  Airfreightrevenuemiles(r,n+1989) 				  = SUM(FTM(r,N,:))/1000.0
!!!	  Jet fuel use (trillion BTU)
!	  Airqjftrcommercial(r,n+1989) 					  = QJETR_NUS(r,N)
!	ENDDO 
!	
!!!	Air variables for TIRE
!	1240 Format(a,a,a,a,A,a,I4,a,A3,a,I4,a,F12.3)
!	1245 Format(a,a,a,a,A,a,I4,a,A3,a,I4,a,F8.3)
!	IF (YRS.eq.2050) THEN
!	  DO r=1,Nregions
!	    DO i=21,LMYr-1989
!	      WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM Dom Total (millions)'     			,',',r,',',RLabA(r),',',i+1989,',',RPMT(r,1,i)
!	  	  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM Int Total (millions)'				,',',r,',',RLabA(r),',',i+1989,',',RPMT(r,2,i)
!	      WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - FTM Dom incl. belly (millions)'			,',',r,',',RLabA(r),',',i+1989,',',FTM(r,i,1)
!	  	  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - FTM Int incl. belly (millions)'			,',',r,',',RLabA(r),',',i+1989,',',FTM(r,i,2)
!	      WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - FTM Hist Align Dom (millions)'			,',',r,',',RLabA(r),',',i+1989,',',FTM_ALIGNHIST(r,1)
!	  	  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - FTM Hist Align Int (millions)'			,',',r,',',RLabA(r),',',i+1989,',',FTM_ALIGNHIST(r,2)
!	      WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Load Factor Dom'     					,',',r,',',RLabA(r),',',i+1989,',',LOAD_FACTOR(r,i,1)
!	  	  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Load Factor Int'							,',',r,',',RLabA(r),',',i+1989,',',LOAD_FACTOR(r,i,2)
!	      WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM Dom NB (millions)'     				,',',r,',',RLabA(r),',',i+1989,',',RPM(r,1,1,i)
!	      WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM Dom WB (millions)'     				,',',r,',',RLabA(r),',',i+1989,',',RPM(r,1,2,i)
!	      WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM Dom RJ (millions)'     				,',',r,',',RLabA(r),',',i+1989,',',RPM(r,1,3,i)
!	      WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM Int NB (millions)'    				,',',r,',',RLabA(r),',',i+1989,',',RPM(r,2,1,i)
!	      WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM Int WB (millions)'    				,',',r,',',RLabA(r),',',i+1989,',',RPM(r,2,2,i)
!	      WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM Int RJ (millions)'    				,',',r,',',RLabA(r),',',i+1989,',',RPM(r,2,3,i)
!	      WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM per cap Dom' 						,',',r,',',RLabA(r),',',i+1989,',',RPMT_PC(r,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM per cap Int' 						,',',r,',',RLabA(r),',',i+1989,',',RPMT_PC(r,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight Dom NB (million RPMs)'		,',',r,',',RLabA(r),',',i+1989,',',BELLY_RPM_EQ(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight Dom WB (million RPMs)'		,',',r,',',RLabA(r),',',i+1989,',',BELLY_RPM_EQ(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight Dom RJ (million RPMs)'		,',',r,',',RLabA(r),',',i+1989,',',BELLY_RPM_EQ(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight Int NB (million RPMs)'		,',',r,',',RLabA(r),',',i+1989,',',BELLY_RPM_EQ(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight Int WB (million RPMs)'		,',',r,',',RLabA(r),',',i+1989,',',BELLY_RPM_EQ(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight Int RJ (million RPMs)'		,',',r,',',RLabA(r),',',i+1989,',',BELLY_RPM_EQ(r,3,2,i)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of payload Dom NB'		,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_PLOAD(1,i,1)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of payload Dom WB'		,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_PLOAD(2,i,1)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of payload Dom RJ'		,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_PLOAD(3,i,1)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of payload Int NB'		,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_PLOAD(1,i,2)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of payload Int WB'		,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_PLOAD(2,i,2)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of payload Int RJ'		,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_PLOAD(3,i,2)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of all freight Dom NB'	,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_FRT(1,i,1)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of all freight Dom WB'	,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_FRT(2,i,1)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of all freight Dom RJ'	,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_FRT(3,i,1)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of all freight Int NB'	,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_FRT(1,i,2)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of all freight Int WB'	,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_FRT(2,i,2)
!		  WRITE(63,1245)trim(WSCEN),',',trim(Dcode),',','Air - Belly freight shr of all freight Int RJ'	,',',r,',',RLabA(r),',',i+1989,',',PCT_BELLY_FRT(3,i,2)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM incl. belly Dom NB (millions)'		,',',r,',',RLabA(r),',',i+1989,',',PASSAC_RPM_DMD(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM incl. belly Dom WB (millions)'		,',',r,',',RLabA(r),',',i+1989,',',PASSAC_RPM_DMD(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM incl. belly Dom RJ (millions)'		,',',r,',',RLabA(r),',',i+1989,',',PASSAC_RPM_DMD(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM incl. belly Int NB (millions)'		,',',r,',',RLabA(r),',',i+1989,',',PASSAC_RPM_DMD(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM incl. belly Int WB (millions)'		,',',r,',',RLabA(r),',',i+1989,',',PASSAC_RPM_DMD(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM incl. belly Int RJ (millions)'		,',',r,',',RLabA(r),',',i+1989,',',PASSAC_RPM_DMD(r,3,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM share Dom NB'						,',',r,',',RLabA(r),',',i+1989,',',SHR_RPM(r,i,1,1)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM share Dom WB'						,',',r,',',RLabA(r),',',i+1989,',',SHR_RPM(r,i,2,1)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM share Dom RJ'						,',',r,',',RLabA(r),',',i+1989,',',SHR_RPM(r,i,3,1)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM share Int NB'						,',',r,',',RLabA(r),',',i+1989,',',SHR_RPM(r,i,1,2)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM share Int WB'						,',',r,',',RLabA(r),',',i+1989,',',SHR_RPM(r,i,2,2)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPM share Int RJ'						,',',r,',',RLabA(r),',',i+1989,',',SHR_RPM(r,i,3,2)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - FTM Freighter Dom NB (millions)'			,',',r,',',RLabA(r),',',i+1989,',',CARGOAC_FTM_DMD(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - FTM Freighter Dom WB (millions)'			,',',r,',',RLabA(r),',',i+1989,',',CARGOAC_FTM_DMD(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - FTM Freighter Dom RJ (millions)'			,',',r,',',RLabA(r),',',i+1989,',',CARGOAC_FTM_DMD(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - FTM Freighter Int NB (millions)'			,',',r,',',RLabA(r),',',i+1989,',',CARGOAC_FTM_DMD(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - FTM Freighter Int WB (millions)'			,',',r,',',RLabA(r),',',i+1989,',',CARGOAC_FTM_DMD(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - FTM Freighter Int RJ (millions)'			,',',r,',',RLabA(r),',',i+1989,',',CARGOAC_FTM_DMD(r,3,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASM Dom NB no freight (millions)'		,',',r,',',RLabA(r),',',i+1989,',',ASM(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASM Dom WB no freight (millions)'		,',',r,',',RLabA(r),',',i+1989,',',ASM(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASM Dom RJ no freight (millions)'		,',',r,',',RLabA(r),',',i+1989,',',ASM(r,1,3,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASM Int NB no freight (millions)'		,',',r,',',RLabA(r),',',i+1989,',',ASM(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASM Int WB no freight (millions)'		,',',r,',',RLabA(r),',',i+1989,',',ASM(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASM Int RJ no freight (millions)'		,',',r,',',RLabA(r),',',i+1989,',',ASM(r,2,3,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASMPG Dom NB incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',ASMPG_AVG_AGE(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASMPG Dom WB incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',ASMPG_AVG_AGE(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASMPG Dom RJ incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',ASMPG_AVG_AGE(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASMPG Int NB incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',ASMPG_AVG_AGE(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASMPG Int WB incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',ASMPG_AVG_AGE(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - ASMPG Int RJ incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',ASMPG_AVG_AGE(r,3,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Scrapped aircraft Passenger NB'			,',',r,',',RLabA(r),',',i+1989,',',SCRAPPED_TOTAL(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Scrapped aircraft Passenger WB'			,',',r,',',RLabA(r),',',i+1989,',',SCRAPPED_TOTAL(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Scrapped aircraft Passenger RJ'			,',',r,',',RLabA(r),',',i+1989,',',SCRAPPED_TOTAL(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Scrapped aircraft Passenger Total'		,',',r,',',RLabA(r),',',i+1989,',',SUM(SCRAPPED_TOTAL(r,:,1,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Scrapped aircraft Freighter NB'			,',',r,',',RLabA(r),',',i+1989,',',SCRAPPED_TOTAL(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Scrapped aircraft Freighter WB'			,',',r,',',RLabA(r),',',i+1989,',',SCRAPPED_TOTAL(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Scrapped aircraft Freighter RJ'			,',',r,',',RLabA(r),',',i+1989,',',SCRAPPED_TOTAL(r,3,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Scrapped aircraft Freighter Total'		,',',r,',',RLabA(r),',',i+1989,',',SUM(SCRAPPED_TOTAL(r,:,2,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Unparked Freighter NB'					,',',r,',',RLabA(r),',',i+1989,',',UNPARKED(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Unparked Freighter WB'					,',',r,',',RLabA(r),',',i+1989,',',UNPARKED(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Unparked Freighter RJ'					,',',r,',',RLabA(r),',',i+1989,',',UNPARKED(r,3,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Unparked Freighter Total'				,',',r,',',RLabA(r),',',i+1989,',',SUM(UNPARKED(r,:,2,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Unparked Passenger aircraft NB'			,',',r,',',RLabA(r),',',i+1989,',',UNPARKED(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Unparked Passenger aircraft WB'			,',',r,',',RLabA(r),',',i+1989,',',UNPARKED(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Unparked Passenger aircraft RJ'			,',',r,',',RLabA(r),',',i+1989,',',UNPARKED(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Unparked Passenger aircraft Total'		,',',r,',',RLabA(r),',',i+1989,',',SUM(UNPARKED(r,:,1,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Parked Freighter NB'						,',',r,',',RLabA(r),',',i+1989,',',PARKED(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Parked Freighter WB'						,',',r,',',RLabA(r),',',i+1989,',',PARKED(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Parked Freighter RJ'						,',',r,',',RLabA(r),',',i+1989,',',PARKED(r,3,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Parked Freighter Total'					,',',r,',',RLabA(r),',',i+1989,',',SUM(PARKED(r,:,2,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Parked Passenger aircraft NB'			,',',r,',',RLabA(r),',',i+1989,',',PARKED(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Parked Passenger aircraft WB'			,',',r,',',RLabA(r),',',i+1989,',',PARKED(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Parked Passenger aircraft RJ'			,',',r,',',RLabA(r),',',i+1989,',',PARKED(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Parked Passenger aircraft Total' 		,',',r,',',RLabA(r),',',i+1989,',',SUM(PARKED(r,:,1,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter conversion - active NB'		,',',r,',',RLabA(r),',',i+1989,',',CONVERTED(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter conversion - active WB'		,',',r,',',RLabA(r),',',i+1989,',',CONVERTED(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter conversion - active RJ'		,',',r,',',RLabA(r),',',i+1989,',',CONVERTED(r,3,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter conversion - parked NB'		,',',r,',',RLabA(r),',',i+1989,',',CONVERTED(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter conversion - parked WB'		,',',r,',',RLabA(r),',',i+1989,',',CONVERTED(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter conversion - parked RJ'		,',',r,',',RLabA(r),',',i+1989,',',CONVERTED(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter conversion - Total'			,',',r,',',RLabA(r),',',i+1989,',',SUM(CONVERTED(r,:,:,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter Sales - NB'					,',',r,',',RLabA(r),',',i+1989,',',STKCARGO_ACTIVE(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter Sales - WB'					,',',r,',',RLabA(r),',',i+1989,',',STKCARGO_ACTIVE(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter Sales - RJ'					,',',r,',',RLabA(r),',',i+1989,',',STKCARGO_ACTIVE(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter Sales - Total'					,',',r,',',RLabA(r),',',i+1989,',',SUM(STKCARGO_ACTIVE(r,:,1,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Passenger Aircraft Sales - NB'			,',',r,',',RLabA(r),',',i+1989,',',STKPASS_ACTIVE(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Passenger Aircraft Sales - WB'			,',',r,',',RLabA(r),',',i+1989,',',STKPASS_ACTIVE(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Passenger Aircraft Sales - RJ'			,',',r,',',RLabA(r),',',i+1989,',',STKPASS_ACTIVE(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Passenger Aircraft Sales - Total'		,',',r,',',RLabA(r),',',i+1989,',',SUM(STKPASS_ACTIVE(r,:,1,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter Stock - NB'					,',',r,',',RLabA(r),',',i+1989,',',STKCTOT(r,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter Stock - WB'					,',',r,',',RLabA(r),',',i+1989,',',STKCTOT(r,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter Stock - RJ'					,',',r,',',RLabA(r),',',i+1989,',',STKCTOT(r,3,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Freighter Stock - Total'					,',',r,',',RLabA(r),',',i+1989,',',SUM(STKCTOT(r,:,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Passenger Aircraft Stock - NB'			,',',r,',',RLabA(r),',',i+1989,',',STKPTOT(r,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Passenger Aircraft Stock - WB'			,',',r,',',RLabA(r),',',i+1989,',',STKPTOT(r,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Passenger Aircraft Stock - RJ'			,',',r,',',RLabA(r),',',i+1989,',',STKPTOT(r,3,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Passenger Aircraft Stock - Total'		,',',r,',',RLabA(r),',',i+1989,',',SUM(STKPTOT(r,:,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Aircraft Stock - Total'					,',',r,',',RLabA(r),',',i+1989,',',SUM(STKPTOT(r,:,i))+SUM(STKCTOT(r,:,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPMPG Dom NB incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',RPMPG(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPMPG Dom WB incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',RPMPG(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPMPG Dom RJ incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',RPMPG(r,3,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPMPG Int NB incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',RPMPG(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPMPG Int WB incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',RPMPG(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - RPMPG Int RJ incl. belly freight'		,',',r,',',RLabA(r),',',i+1989,',',RPMPG(r,3,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Gal/ton-mile Dom NB incl. belly freight' ,',',r,',',RLabA(r),',',i+1989,',',GPTM(r,i,1,1)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Gal/ton-mile Dom WB incl. belly freight' ,',',r,',',RLabA(r),',',i+1989,',',GPTM(r,i,2,1)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Gal/ton-mile Dom RJ incl. belly freight' ,',',r,',',RLabA(r),',',i+1989,',',GPTM(r,i,3,1)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Gal/ton-mile Int NB incl. belly freight' ,',',r,',',RLabA(r),',',i+1989,',',GPTM(r,i,1,2)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Gal/ton-mile Int WB incl. belly freight' ,',',r,',',RLabA(r),',',i+1989,',',GPTM(r,i,2,2)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Gal/ton-mile Int RJ incl. belly freight' ,',',r,',',RLabA(r),',',i+1989,',',GPTM(r,i,3,2)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Jet fuel consumption passenger Dom' 		,',',r,',',RLabA(r),',',i+1989,',',QJETR_DI(r,1,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Jet fuel consumption passenger Int' 		,',',r,',',RLabA(r),',',i+1989,',',QJETR_DI(r,2,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Jet fuel consumption freight Dom' 		,',',r,',',RLabA(r),',',i+1989,',',QJETR_DI(r,1,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Jet fuel consumption freight Int' 		,',',r,',',RLabA(r),',',i+1989,',',QJETR_DI(r,2,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Jet fuel consumption passenger Total' 	,',',r,',',RLabA(r),',',i+1989,',',SUM(QJETR_DI(r,:,1,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Jet fuel consumption freight Total' 		,',',r,',',RLabA(r),',',i+1989,',',SUM(QJETR_DI(r,:,2,i))
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','gdp per capita' 								,',',r,',',RLabA(r),',',i+1989,',',WLD_GDP(r,i)/WLD_POP(r,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Average age passenger aircraft - NB' 	,',',r,',',RLabA(r),',',i+1989,',',STK_AVGAGE_PA(r,1,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Average age passenger aircraft - WB' 	,',',r,',',RLabA(r),',',i+1989,',',STK_AVGAGE_PA(r,2,i)
!		  WRITE(63,1240)trim(WSCEN),',',trim(Dcode),',','Air - Average age passenger aircraft - RJ' 	,',',r,',',RLabA(r),',',i+1989,',',STK_AVGAGE_PA(r,3,i)
!		ENDDO
!	  ENDDO
!	ENDIF
!	
!	
!	
!ENDIF
! WEPS section END -- NEMSWEPSSWITCH ----------------------------------------------------------------------------------------------------------------	  	
    RETURN
  END SUBROUTINE TAREPORT