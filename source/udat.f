! $Header: M:/default/source/RCS/udat.f,v 1.741 2021/02/26 15:46:34 avk Exp $
    
   module hourly_restore_data
    ! < vikram.linga@eia.gov > 08/06/2024
    ! This module contains the hourly dispatch output data from REStore
    ! Todo: Figure out how not to hardcode numbers
           
    REAL*8 DISPATCH(2,28,23,4,12,3,24) !Real*8 DISPATCH(2,MNUMNR,MaxPtypesP,MaxNumbinsP,12,3,24)
    REAL*8 DispatchDelta(2,28,23,4,12,3,24) !Real*8 DISPATCH(2,MNUMNR,MaxPtypesP,MaxNumbinsP,12,3,24)
    
    REAL*8 existsto(2,28,23,12,3,24)
    
    ! hourly capacity factors from restore used to compute load electrolyzer consumption limits for hmm
    real supplytorestore(28,3,23,3) ! Region, Season, RestoreTech, Steps, Unit = GWs, CapacityPerSupplyStepFromRestore
    real pricetorestore(28,3,23,3) ! Region, Season, RestoreTech, Steps, Unit = $/GWhrs
    !real hourly_total_capacity(28,12,3,24) ! Region, Month, Day, Hour, year
    !real derate_m864(23,3) ! restore plantgroup, steps
    end module
    
    
   module ephrts_file_unit_numbers 
   ! < edward.thomas@eia.gov > 07/08/2023
   ! This module contains the unit numbers for the epphrts files,
   ! note: if the files were to be programatically fetched, the unit numbers would be fetched using FILE_MGR ('O','KDEBUG',.TRUE.) from
   !       file manager system located in filmgr.f. 
   
      integer unit_num_ephrts_supply ! ".\file = ephrts\output\D_Fuel.csv" 
      parameter(unit_num_ephrts_supply = 79026)
   
      integer unit_num_ephrts_dfuel ! ".\file = ephrts\output\D_Fuel.csv" 
      parameter(unit_num_ephrts_dfuel = 79026)
      
      integer unit_num_ephrts_minimum_demand ! "file = ephrts\\input\\minimum_demand_threshold.txt" 
      parameter(unit_num_ephrts_minimum_demand = 97313) 
      
      integer unit_num_ephrts_fixed_cost_factors ! "file = ephrts\\input\\ephrts_costs_factor.txt" 
      parameter(unit_num_ephrts_fixed_cost_factors = 29874)
      
      integer unit_num_ephrts_fixed_cost_factors_yearly ! "file = ephrts\\input\\ephrts_costs_factor_yearly.txt"
      parameter(unit_num_ephrts_fixed_cost_factors_yearly = 34588)

      integer unit_num_ephrts_cmd_file ! "file = ephrts_debug\\ephrts_cmds_"//year_str//"_"//pass_str//".txt", status='replace') 
      parameter(unit_num_ephrts_cmd_file = 56411)
   
      integer unit_num_ephrts_error_log ! "file = ephrts_debug\\ephrts_error_log-"//year_str//"-"//itr_str//".txt", status='unknown') 
      parameter(unit_num_ephrts_error_log = 59362)
      
      integer unit_num_ephrts_efp_fuel_prices ! "file = ephrts_debug\\ephrts_efp_fuel_prices_debug.csv", status='unknown') 
      parameter(unit_num_ephrts_efp_fuel_prices = 59363)
      
      integer unit_num_ephrts_price_reducer ! file="ephrts\\src\\sec45v_electricity_price_reducer.csv", 
      parameter(unit_num_ephrts_price_reducer = 839173)
   
      integer unit_num_ephrts_price_restore_mapping ! file="ephrts\\input\\efd-restore-mapping.csv" 
      parameter(unit_num_ephrts_price_restore_mapping = 239813)
      
      integer unit_num_ephrts_debug_file ! FILE="EPHRTS_DEBUG_FILE.TXT" 
      parameter(unit_num_ephrts_debug_file = 192152)
      
      integer unit_num_ephrts_load_debug_file ! FILE="EPHRTS_Load_FILE.TXT" 
      parameter(unit_num_ephrts_load_debug_file = 993499)
      
      integer unit_num_read_delta_gen ! Rep_GenerationDelta.csv
      parameter(unit_num_read_delta_gen = 993498)
   
    end module
   
   
   ! FETCHES THE DATA FROM HYDROGEN_DATA.CSV AND THEN POPULATES 
    SUBROUTINE GET_HYDROGEN_DATA(YEAR)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : IOSTAT_END
    IMPLICIT NONE
      
    include 'parametr'
    include 'emmparm'
    include 'control' 
    include 'ncntrl'
    include 'hmmblk'
    include 'convfact'

        
    INTEGER :: ERROR, REGION, SEASON, STEP_H2, year
    REAL :: QUANTITY, PRICE
        
    ! DEFINE REGIONS IN OBJECTS
        
    ! READ IN DATA AND PASS DATA INTO COMMONBLK

    OPEN(54152, FILE="input\hydrogen_data.csv") 
    READ(54152,*)

        
    ! The quanity input here is KGS and we want the output to be Trillion BTUS. And the Price is 1987$/MMBTUS, TBTU * 1987$/MMBTU = 
    ! TODO: Restart file changes
    DO
        READ( 54152, *, IOSTAT = ERROR ) REGION, SEASON, STEP_H2, QUANTITY, PRICE 
                       
!	  REAL CFH2Q_KG           !  CONVERSION FACTOR KILOGRAMS PER MILLION Btu
        
        IF (YEAR .GT. UPSTYR+2) THEN ! this should match when EPHRTS is run from util.f
            QUANTITY = (QUANTITY / CFH2Q_KG) / 1000000.0 ! KGS -> tbtu, where kgs/kgs/mmbtu = mmbtu/1000000.0 = tbtu
            PRICE =  (PRICE * CFH2Q_KG) ! 1987$/KGS * kgs/mbtu = $1987/mbtu
        ELSE
            QUANTITY = 0.001
            PRICE = 9999.9
        END IF
            
        ! SET Q/P H2 VARS FOUND IN HMMBLOCK
        H2SCRV_Q(REGION,STEP_H2,SEASON,CURIYR) = QUANTITY              
        H2SCRV_P(REGION,STEP_H2,SEASON,CURIYR) = PRICE                
            
        SELECT CASE( ERROR )
            CASE( 0 )

            CASE( IOSTAT_END )
                EXIT
            CASE DEFAULT
                WRITE( *, * ) 'ERROR IN READING EPHRTS FILE'
                STOP
            END SELECT
    END DO
        
    CLOSE(54152)
      
    END SUBROUTINE 
    
    
    
      SUBROUTINE RDCNTRL(I_CNTL)
      USE SQLITE
      IMPLICIT NONE

!     THIS SUBROUTINE CALLS ROUTINES TO READ DATA FROM EXTERNAL FILES INTO THE EMM COMMON BLOCKS

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'cdsparms'
      include'control'
      include'ecpcntl'
      include'bildin'
      include'fuelin'
      include'entcntl'
      include'emshrin'
      include'taxdat'
      include'coalemm'
      include'acoalprc'
      include'wrenew'
      include'eusprc'
      include'edbdef'
      include'emission'
      include'emoblk'
      include'epmbank'
      include'uso2grp'
      include'ecp_coal'
      include'ecp_nuc'
      include'uecpout'
      include'macout'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'dsmnercr'
      include'dsmcaldr'
      include'rggi'
      include'csapr'
      include'steoblock'
      include'elout'
      include'dispinyr'
      include'uefdout'
      include'emmemis'
      include'emm_aimms'
!
      REAL*8       GNP$GRW,CAP$GRW(ECP_D_CAP),CAP$CUM,XTRA_OM(ECP_D_CAP+1,MNUMYR+ECP_D_FPH),XTRA_DEF,XTRA_OM2(ECP_D_CAP)
      REAL*8       MO_EFD_SP(0:EFD_D_MSP,12),MO_ECP_SP(0:ECP_D_MSP,12)
      REAL*4       HCNT(EFD_D_NFL),RSO2(EFD_D_NFL),TOT(MNUMYR)
      REAL*4       CLCARAV,NGCARAV
      REAL*4       ST_CAPS(NOX_D_MST+1,MNUMYR)                 ! NOX EMISSION CAPS by STATE
      REAL*4       TMP_HRS(NOX_D_GRP,12)                       ! NOX CAP HOURS BY MONTH
      INTEGER*4    XTRA_NUM,AGE,JCAP,STEP,XTRA_YR,ICL,XYR,ICLRG,ISO2,TYR, EFDt, UPAGEOPT(ECP_D_CAP)
      INTEGER*4    IGRP,JYR,SYR,EYR,IST,INOXYR,T_NOXYRS,ISP,IMO,IRNK,T_NOX_NST,ICLS,IHR,IDTYP
      INTEGER*4    IECP,IEFD,IY,IR
      INTEGER*4    I_CNTL,IRG,IYR,IFL,ICHK,IP,ICAP,TECHSW,BYR,ITYP,KCAP,JTYP,ISTO, I_TRANCHE
      INTEGER*4    UPLTISW(ECP_D_CAP),ISTART,ISTOP, EMM_RG,IREG,ISEAS
      INTEGER*4    ECP$TMP
      INTEGER*4    FULLYR
      CHARACTER*40 FILENM
      CHARACTER*6  BASIS
      CHARACTER*3  TMP_EFD_CD_BY_ECP(ECP_D_CAP)
      CHARACTER*2  ST_NAME(NOX_D_MST+1)             ! State Codes
      INTEGER*4    ICO2YR,T_CO2YRS,T_CO2_NST,K,K1
!     INTEGER*4    NSTCODES
      REAL*4       ST_CAPSC(CO2_D_MST+1,MNUMYR)         ! CO2 EMISSION CAPS by STATE
!     INTEGER*4    ST_FLRG(CO2_D_MST+1)                 ! STATE-FUEL REGION MAPPING
!     CHARACTER*2  ST_NAMES(CO2_D_MST+1)                ! State Codes
      CHARACTER*2  ST_NAMEC(CO2_D_MST+1)                ! State Codes in Carbon Groups
      INTEGER*4    CO2_TYP,CO2_UNT
      REAL*4       CO2_LIM,CO2FLPR(MNUMYR),CO2RSQY(MNUMYR),CO2RSPR(MNUMYR),CO2OFQY(MNUMYR),CO2OFPR(MNUMYR),CO2ESPR(MNUMYR)
      REAL*4       CO2ECQY(MNUMYR),CO2ECPR(MNUMYR),CO2BKQY(MNUMYR),CO2NJEX(MNUMYR),CO2VAEX(MNUMYR)

!
      INTEGER*4   I_CNFG,I_ROPT,I_ACI,I_TST,J_CNFG,I_RCMB,J_TST,SV_MACT
      INTEGER*4   T_CNFG_NDX(MX_CNFG)             ! Coal Plant Configuration Index
      CHARACTER*4 T_CNFG_MACT(MX_CNFG)            ! Coal Plant Configuration MACT Control Category
      CHARACTER*3 T_CNFG_PART(MX_CNFG),NEW_PART   ! Coal Plant Configuration Particulate Control
      CHARACTER*3 T_CNFG_SO2(MX_CNFG)             ! Coal Plant Configuration SO2 Control
      CHARACTER*4 T_CNFG_COMB(MX_CNFG)            ! Coal Plant Configuration NOX COMB Control
      CHARACTER*4 T_CNFG_SNCR(MX_CNFG)            ! Coal Plant Configuration NOX SNCR Control
      CHARACTER*3 T_CNFG_SCR(MX_CNFG)             ! Coal Plant Configuration NOX SCR Control
      CHARACTER*2 T_CNFG_SC(MX_CNFG)              ! Coal Plant Configuration ACI Spray Cooler
      CHARACTER*2 T_CNFG_FF(MX_CNFG)              ! Coal Plant Configuration ACI Fabric Filter
!
      INTEGER*4   T_ROPT_NDX(MX_ROPT)             ! Coal Plant Retrofit Options Index
      CHARACTER*3 T_ROPT_SO2(MX_ROPT),NEW_SO2     ! Coal Plant Retrofit Options SO2 Control
      CHARACTER*4 T_ROPT_COMB(MX_ROPT),NEW_COMB   ! Coal Plant Retrofit Options NOX COMB Control
      CHARACTER*4 T_ROPT_SNCR(MX_ROPT),NEW_SNCR   ! Coal Plant Retrofit Options NOX SNCR Control
      CHARACTER*3 T_ROPT_SCR(MX_ROPT),NEW_SCR     ! Coal Plant Retrofit Options NOX SCR Control
      CHARACTER*2 T_ROPT_SC(MX_ROPT),NEW_SC       ! Coal Plant Retrofit Options ACI Spray Cooler
      CHARACTER*2 T_ROPT_FF(MX_ROPT),NEW_FF       ! Coal Plant Retrofit Options ACI Fabric Filter
!
      INTEGER*4   T_NUM_ACI                       ! NUMBER OF ACI Options
      REAL*4      TMPALLGN(MNUMYR,MNUMNR)         ! Temp variable for reading in CPP allocations
      REAL*4      TMPALLLD(MNUMYR,MNUMNR)
!
      REAL*8 OUT,HR,IN,PRCNT,LIM,GRAM,HG,EMF_P,EMF_M,F_FGD,F_SCR,A,B,C,D,EMF_T(NSTEP),ACI(NSTEP),EMF_PRCNT,EMF_IN,EMF_OUT,EMF_MAX,MACT_EMF,GRW
      INTEGER*4 Allowed(NSTEP),NMAX,N,CHOICE,MACT_SW,NUCDYR(MNUMYR)
      REAL*4 TSO2_OSH(NDREG),SO2CR(MNUMCR)
      INTEGER TRN,COA,OIL
!
      INTEGER CO2_STDYR,CO2_STDN0,CO2_STDN1,CO2_STDN2,CO2_STDN3,CO2_STDN4,CO2_STDN5
      INTEGER CO2_QTYN0,CO2_QTYN1,CO2_QTYN2,CO2_QTYN3
      REAL*4 CO2_STDST(EMM_D_ST+1,MNUMYR),CO2_STDS1(EMM_D_ST+1,MNUMYR),CO2_STDS2(EMM_D_ST+1,MNUMYR),CO2_STDS3(EMM_D_ST+1,MNUMYR)
      REAL*4 CO2_STDS4(EMM_D_ST+1,MNUMYR),CO2_STDS5(EMM_D_ST+1,MNUMYR)
      REAL*4 CO2_QTYQX(EMM_D_ST+1,MNUMYR),CO2_QTYQA(EMM_D_ST+1,MNUMYR),CO2_QTYFX(EMM_D_ST+1,MNUMYR),CO2_QTYFA(EMM_D_ST+1,MNUMYR)
      REAL*4 CO2_PLTMS(ECP_D_CAP),CO2_PLTRT(ECP_D_CAP),URCOST(MNUMYR)

      REAL*8 HR_EFF(NDREG,ECP_D_CAP,MAX_KNOTS), HR_LL(NDREG,ECP_D_CAP,MAX_KNOTS)
      INTEGER*4 HR_KNOTS(NDREG,ECP_D_CAP)
      type(sqlite_database)                      :: db
      type(sqlite_statement)                     :: stmt
      type(sqlite_statement)                     :: stmt2
      type(sqlite_statement)                     :: stmt3
      type(sqlite_statement)                     :: stmt4
      type(sqlite_statement)                     :: stmt5
      type(sqlite_statement)                     :: stmt6
      type(sqlite_column), dimension(:), pointer :: col
      type(sqlite_column), dimension(:), pointer :: col2
      type(sqlite_column), dimension(:), pointer :: col3
      type(sqlite_column), dimension(:), pointer :: col4
      type(sqlite_column), dimension(:), pointer :: col5
      type(sqlite_column), dimension(:), pointer :: col6
      character(len=40), pointer, dimension(:)   :: result
      character(len=80)                          :: errmsg
      logical                                    :: finished

      COMMON/HR_ADJ/HR_EFF, HR_LL, HR_KNOTS
      REAL*8 TMP1(MAX_KNOTS),TMP2(MAX_KNOTS),TMPD
      INTEGER*4 TMPA,TMPC,KNOT,IX,TEST_ITYP(ECP_D_CAP,NDREG)
      CHARACTER*2 TMPB
!
      LOGICAL NEW
!
!     USING RD$* FUNCTIONS
!
      INTEGER*4    RD$TBL,RD$I1,RD$R1,RD$R81,RD$C1,RET_CODE
      INTEGER*4    RD$I2,RD$R2,RD$R82,RD$C2,RD$I3,RD$R3
      INTEGER*4    COLLB,NCOL,CPT,L_COLLB,L,I
      INTEGER*4    DMSZ
      PARAMETER (DMSZ = 300)
      CHARACTER*40 DUMMY(DMSZ)             ! DUMMY COLUMN IN DATA TABLES
      INTEGER*4    IDUMMY(DMSZ)                    ! SCALAR DUMMY COLUMN
      INTEGER*4    CHK$RSUM,CHK$PRF
!
      INTEGER FILE_MGR,RTOVALUE
      EXTERNAL FILE_MGR,RTOVALUE

      COMMON/BNCHGEN/ BSNCGEN,BSHYGEN,BSSOGEN,BSPVGEN,BSWNGEN,BSGTGEN,BMNCLYR,BMHYLYR,BMSOLYR,BMPVLYR,BMWNLYR,BMGTLYR
      REAL BSNCGEN,BSHYGEN,BSSOGEN,BSPVGEN,BSWNGEN,BSGTGEN
      INTEGER BMNCLYR,BMHYLYR,BMSOLYR,BMPVLYR,BMWNLYR,BMGTLYR
      INTEGER*4 N_CHP(MX_ST_RPS), N_DG(MX_ST_RPS), N_QUAL(MX_ST_RPS), N_RQ(MX_ST_RPS), TMP_UT_NUM
      REAL*8 TEMP1(MX_ST_CODES,MNUMNR),SUM_SALES_ST(MX_ST_CODES),SUM_SALES_EMM(MNUMNR)
      REAL*8 TEMP2(MX_ST_RPS),TEMP3(MX_ST_RPS),TEMP4
      REAL*8 ST_TMP(MNUMYR+ECP_D_FPH,MX_TYPE+1),  ST_TMP_YR(MNUMYR+ECP_D_FPH,MX_TYPE+1), Delta_RQ
      CHARACTER*2 TEMPC1
      CHARACTER*2 TMP_ST_CD(MX_ST_CODES)
      CHARACTER*2 TMP_PT_CD(ECP_D_CAP)
      CHARACTER*2 TMP_TC_CD(TC_FUELS)
      CHARACTER*1 TEMPC2
      CHARACTER*40 TEMPC3
      CHARACTER*4 TEMPC4(MX_TYPE)
      CHARACTER*7 TEMPC7
      INTEGER*4 UT_TYPES, TEMPI1, TEMPI2, TEMPI3, TEMPI4, TEMPI5, TEMPI6(MX_ST_RPS)
      CHARACTER*12 TABLE_NAME
      CHARACTER*10 STR
      CHARACTER*2 Tranche_CD
      CHARACTER*4 TC_SECTOR_CODES(TC_SECTORS)
      REAL*4 TAXDEPRR
      INTEGER REG(MNUMNR)
!
      INTEGER GRD_CASD,GRD_FACN,J
      REAL*4  GRD_WGTS(13,4,MNUMNR)
      REAL*4  GRD_RATP(ECP_D_CAP,MNUMNR)
      REAL*4  GRD_RATW(ECP_D_CAP,MNUMNR)
      REAL*4  GRD_TGTX(MNUMYR,4,MNUMNR)
      
      REAL*4  GRD_RATED(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATRC(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATRG(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATGS(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATSC(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATNI(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATVC(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATFC(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATEF(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATOE(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATD1(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATD2(ECP_D_CAP,4,MNUMNR) 
      REAL*4  GRD_RATD3(ECP_D_CAP,4,MNUMNR) 
!
!      COMMON /GRDSRC/ GRD_CASN,GRD_SRCN,GRD_SRCC
!      INTEGER GRD_CASN                                       ! Number of grid resilience sources
      CHARACTER*15 tmp_GRD_SRCN                     ! Grid resilience source names
      CHARACTER*30 tmp_DESC                     ! Grid resilience source names
!      CHARACTER*15 GRD_SRCN(MX_GRDSRC)                       ! Grid resilience source names
!      CHARACTER*1  GRD_SRCC(MX_GRDSRC)                       ! Grid resilience source codes
      CHARACTER*2  tmp_GRD_CD                      ! Grid resilience source codes

!     Fuel Indexes for Traditional Cogen

      F_CL  =  1   ! Coal
      F_OL  =  2   ! Oil
      F_OG  =  3   ! Other Gaesous Fuels
      F_NG  =  4   ! Natural Gas
      F_HY  =  5   ! Hydro
      F_GT  =  6   ! Geothermal
      F_MS  =  7   ! MSW
      F_WD  =  8   ! Biomass
      F_SO  =  9   ! Solar Thermal
      F_PV  = 10   ! Solar PV
      F_OT  = 11   ! Other
      F_WN  = 12   ! Wind

      TC_FUEL_CODES( 1) = 'CL'    ! Coal
      TC_FUEL_CODES( 2) = 'OL'    ! Oil
      TC_FUEL_CODES( 3) = 'OG'    ! Other Gaesous Fuels
      TC_FUEL_CODES( 4) = 'NG'    ! Natural Gas
      TC_FUEL_CODES( 5) = 'HY'    ! Hydro
      TC_FUEL_CODES( 6) = 'GT'    ! Geothermal
      TC_FUEL_CODES( 7) = 'MS'    ! MSW
      TC_FUEL_CODES( 8) = 'WD'    ! Biomass
      TC_FUEL_CODES( 9) = 'SO'    ! Solar Thermal
      TC_FUEL_CODES(10) = 'PV'    ! Solar PV
      TC_FUEL_CODES(11) = 'OT'    ! Other
      TC_FUEL_CODES(12) = 'WN'    ! Wind

      TC_SECTOR_CODES(1) = 'IOU '
      TC_SECTOR_CODES(2) = 'COOP'
      TC_SECTOR_CODES(3) = 'MUNI'
      TC_SECTOR_CODES(4) = 'FED '

      DO IRG = 1 , MNUMNR
         REG(IRG) = IRG
      END DO

!
!     Add up Number of Hours in each Month That Belong to ECP and EFD Seasonal Periods: 0 Seasonal Period => All Hours in a Month
!
      MO_ECP_SP = 0.0
      MO_EFD_SP = 0.0
      DO IHR = 1 , 24
         DO IDTYP = 1 , 3
            DO IMO = 1 , 12
               IECP = HrToECPSeas(IMO,IDTYP,IHR)
               MO_ECP_SP(0,IMO) = MO_ECP_SP(0,IMO) + IDAYTQ(IDTYP,IMO)
               MO_ECP_SP(IECP,IMO) = MO_ECP_SP(IECP,IMO) + IDAYTQ(IDTYP,IMO)
               IEFD = HrToEFDSeas(IMO,IDTYP,IHR)
               MO_EFD_SP(0,IMO) = MO_EFD_SP(0,IMO) + IDAYTQ(IDTYP,IMO)
               MO_EFD_SP(IEFD,IMO) = MO_EFD_SP(IEFD,IMO) + IDAYTQ(IDTYP,IMO)
            END DO
         END DO
      END DO
!
!     OPEN DEBUG FILE (UNIT 12)
!
      FILENM = 'EFDDBUG'
      NEW = .TRUE.
      UF_MSG = FILE_MGR('O',FILENM,NEW)
!     IF (PRTDBGE .GT. 2) THEN
         UF_DBG = UF_MSG
!     ELSE
!        UF_DBG = 0
!     END IF
!
!     ECP_PSO2 = 0.0
!
!     OPEN THE CONTROL FILE
!
      NEW = .FALSE.
      FILENM='EMMCNTL'
      I_CNTL = FILE_MGR('O',FILENM,NEW)
!
      WRITE(UF_MSG,*) '***** BEGINNING RDCNTRL *****'
!
!     READ IN SCENARIO NAME FROM EMMCNTL
!
      RET_CODE = RD$TBL(I_CNTL,'%SCENARIO  %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(UF_SCEN,1,1)                      ! SCENARIO NAME
!
!     READ IN REPORT AND DEBUG SWITCH UNIT # = ON 0 = OFF FROM EMMCNTL
!
      RET_CODE = RD$TBL(I_CNTL,'%RPT/DBG SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UF_IN,1,1)                      ! UNIT # FOR FILE
      RET_CODE = RD$I1(UF_OUT,1,1)                     ! UNIT # FOR FILE
      RET_CODE = RD$I1(UF_PLT,1,1)                                     !
      RET_CODE = RD$I1(UF_CRV,1,1)                                     !
      RET_CODE = RD$I1(IDUMMY,1,DMSZ)          ! ASSIGNED TO UF_DBG IF 0
      RET_CODE = RD$I1(UF_SO2,1,1)                                     !
      RET_CODE = RD$I1(UF_LD,1,1)                                      !
      RET_CODE = RD$I1(UF_TMP,1,1)                                     !
      RET_CODE = RD$I1(UF_RPT,1,1)                                     !
      RET_CODE = RD$I1(UF_FL,1,1)                                      !
      RET_CODE = RD$I1(UF_ETTIN,1,1)                                   !
      RET_CODE = RD$I1(UF_ETTDF,1,1)                                   !
      RET_CODE = RD$I1(UF_ETT,1,1)                                     !
      RET_CODE = RD$I1(UF_CNCT,1,1)                                    !
      RET_CODE = RD$I1(UF_ETDM,1,1)                                    !
      RET_CODE = RD$I1(UF_RP2,1,1)                                     !
      RET_CODE = RD$I1(UF_MC,1,1)                                      !
!
      IF (IDUMMY(1) .EQ. 0) UF_DBG = 0
!
!        OPEN ETTDF FILE
!
      IF (UF_ETTDF .GT. 0) THEN
         NEW = .TRUE.
         FILENM = 'ETTDF'
         UF_ETTDF = FILE_MGR('O',FILENM,NEW)
      END IF
!
!     OPEN REPORT FILE (UNIT 13)
!
!
!     SM turning on @.EMMRPT for all runs
!
!     IF (PRTDBGE.GT.0) THEN
      FILENM = 'EMMRPT'
      NEW = .TRUE.
      UF_RPT = FILE_MGR('O',FILENM,NEW)
!     ELSE
!     UF_RPT = 0
!     END IF
!
!     IF (PRTDBGE.GT.0) THEN
      FILENM = 'EMMRP2'
      NEW = .TRUE.
      UF_RP2 = FILE_MGR('O',FILENM,NEW)
!     ELSE
!     UF_RP2 = 0
!     END IF
!

!
      IF (PRTDBGE .GT. 0) THEN
         FILENM = 'MCRPT'
         NEW = .TRUE.
         UF_MC = FILE_MGR('O',FILENM,NEW)
      ELSE
         UF_MC = 0
      END IF
!
      IF (PRTDBGE .GT. 1) THEN
         FILENM = 'EFDLDCV'
         NEW = .TRUE.
         UF_CRV = FILE_MGR('O',FILENM,NEW)
!CCC    UF_CRV = 15
!CCC    OPEN(UF_CRV,FILE=FILENM,STATUS='UNKNOWN')
      ELSE
         UF_CRV = 0
      END IF
!
!     ULCVSW ! SWITCH TO OUTPUT EFD LOAD CURVES IN EMMPRNT
!     FOR INPUT TO OFF-LINE LOAD CURVE MODIFICATION MODULE
!     OR FOR DEBUG/ANALYSIS
!
      RET_CODE = RD$TBL(I_CNTL,'%OUT LCV SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(ULCVSW,1,1)
!
!     USW_DBS USER SWITCH TO DETERMINE LEVEL OF OUTPUT TO EMM DATABASE.
!     0: NO DATABASE PRINTS
!     1: DEFAULT PRINTS (DLOAD AND DOSPTS PRINTED FOR FIRST & LAST YEAR ONLY)
!     2: FULL PRINTS (DLOAD AND DOSPTS PRINTED FOR ALL YEARS)
!
      RET_CODE = RD$TBL(I_CNTL,'%EMMDB SW  %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_DBS,1,1)
!
!     READ IN SWITCHES TO DETERMINE IF DIRECT ACCESS FILES OR INCORE
!     ARRAYS USED TO STORE DATA
!
      RET_CODE = RD$TBL(I_CNTL,'%DAF/CORE S%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_DISP,1,1)           ! SWITCH TO READ DISP DAF
      RET_CODE = RD$I1(USW_BILD,1,1)           ! SWITCH TO READ BILD DAF
      RET_CODE = RD$I1(USW_EIJ,1,1)             ! SWITCH TO READ EIJ DAF
      RET_CODE = RD$I1(USW_ETTDF,1,1)           ! SWITCH TO READ ETT DAF
!
!     READ IN NUMBER OF 16384 BYTE BLOCKS NEEDED  FOR DISP_IN ,DISP_CRV AND
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM BLOCKS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(LAST_STEO_YR,1,1)                                      !
      RET_CODE = RD$I1(EX_COAL,1,1)                                     !
      RET_CODE = RD$I1(NW_COAL,1,1)                                      !
      RET_CODE = RD$I1(UZ_PLT,1,1)                                     !
      RET_CODE = RD$I1(UZ_ETT,1,1)                                     !
!
!     READ IN FUEL PRICE TOLERANCE
!
      RET_CODE = RD$TBL(I_CNTL,'%FUEL P TOL%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UFPTOL,1,1)                                     !
!
!     ETT AND READ IN ETT SWITCH 0=ON,1=OFF
!
      RET_CODE = RD$TBL(I_CNTL,'%ETT SWITCH%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_ETT,1,1)                                    !
      RET_CODE = RD$I1(USW_XP,1,1)                                     !
      RET_CODE = RD$I1(USW_CANACC,1,1)
!
!     READ LOAD CURVE OPTION 0 = NERC, 1 = LDSM
!
      RET_CODE = RD$TBL(I_CNTL,'%LOAD CR SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_LDC,1,1)                                    !
!
!     READ OVERWRITE SWITCH 0 = NO, 1 = HIST, 2 = HIST & STEO
!
      RET_CODE = RD$TBL(I_CNTL,'%OVERWRITE SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_OVER,1,1)                                   !
!
!     READ REFURBISHMENT HEATRATE IMPROVEMENT FACTOR
!
      RET_CODE = RD$TBL(I_CNTL,'%REFURB%     ',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(URFURB,1,1)                                   !
      RET_CODE = RD$R1(UFACPS,1,1)                                   !
      RET_CODE = RD$R1(UFACP2,1,1)
!
!     READ OWNERSHIP SWITCH
!
      RET_CODE = RD$TBL(I_CNTL,'%OWNERSHIP SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_OWN,1,1)
!
!     READ PLANNED MAINTENANCE SWITCH
!
      RET_CODE = RD$TBL(I_CNTL,'%EFD MAINTENANCE SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_PM,1,1)

!
!     READ IN DIURNAL STORAGE PERFORMANCE PARAMETERS
!
      RET_CODE = RD$TBL(I_CNTL,'%DS PARAMS %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$I1(N_STO_INC,1,1)
      RET_CODE = RD$I1(hoursToBuy,1,1)
      RET_CODE = RD$I1(hoursToSell,1,1)
      RET_CODE = RD$R81(Battery_Extra_Discount,1,1)
      RET_CODE = RD$R81(batteryEfficiency,1,1)
      RET_CODE = RD$R81(Battery_Initial_Percent,1,1)
      RET_CODE = RD$R81(Battery_Incremental_Percent,1,1)
      RET_CODE = RD$R81(Battery_Maximum_Percent,1,1)

!      write(6,7654) 'BATTERY_EMMCNTL', CURIRUN, CURIYR+1989, hourstobuy, hourstosell, Battery_Extra_Discount, batteryefficiency, Battery_Initial_Percent, &
!         Battery_Incremental_Percent, Battery_Maximum_Percent
! 7654 FORMAT(1x,A20,4(",",I4),5(",",F21.6))

!     READ IN INPUT FUEL PRICE SWITCHES (NERC, CENSUS, GAS) 0 = ON, 1 = OFF
!
      RET_CODE = RD$TBL(I_CNTL,'%FUEL PR SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_NERC,1,1)       ! IF < 0, USE COAL/NUC PRICES
                                     ! FROM OTHER MODULES (NOT USED)
      RET_CODE = RD$I1(USW_CENS,1,1)       ! IF < 0, USE OIL PROD PRICES
                                     ! FROM OTHER MODULES (NOT USED)
      RET_CODE = RD$I1(USW_GASR,1,1)        ! IF < 0, USE NAT GAS PRICES
                                     ! FROM OTHER MODULES (NOT USED)
      RET_CODE = RD$I1(USW_CLRG,1,1)      ! INPUT COAL PRICE SWITCH (??)
!
!     READ IN INITIAL EXECUTION YEARS FOR SUBMODULES
!
      RET_CODE = RD$TBL(I_CNTL,'%EXEC YEARS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UHBSYR,1,1)
      RET_CODE = RD$I1(UESTYR,1,1)
      RET_CODE = RD$I1(UISTYR,1,1)
      RET_CODE = RD$I1(UPSTYR,1,1)
      RET_CODE = RD$I1(ULRSYR,1,1)
      RET_CODE = RD$I1(EFPSYR,1,1)
      UISTYR = UISTYR - UHBSYR

!      WRITE(6,7913) CURIRUN, CURIYR+1989, CURITR, UHBSYR,UESTYR,UISTYR,UPSTYR,ULRSYR,EFPSYR
! 7913 FORMAT(1X,"UHBSYR",9(":",I4))
!
!     READ IN NUMBER OF YEARS IN MODEL HORIZON
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM OF YRS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UNYEAR,1,1)
!     RESET INPUT FOR UNYEAR IF LAST NEMS YEAR EXCEEDS IT
!     print *,'!unyear',lastyr,unyear
      IF (LASTYR .GT. UNYEAR)THEN
         UNYEAR = LASTYR
      END IF
!     UNYEAR = MIN(UNYEAR,LASTYR)
!
!     READ IN YEAR TO TURN OFF CAIR IN EFD/CMM (0 = KEEP CAIR)
!
      RET_CODE = RD$TBL(I_CNTL,'%NO CAIR YEAR%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UYR_NOCAIR,1,1)
      RET_CODE = RD$I1(UYR_RSCAIR,1,1)
!
!     READ IN YEARS TO PHASE OUT MUST RUN FOR DIFFERENT SETTINGS
!
      RET_CODE = RD$TBL(I_CNTL,'%YEAR MUSTRUN%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I2(UPHMRY,1,1,4,1,4)
!
!     READ IN YEARS TO SIMULATE IN MODEL HORIZON
!
      RET_CODE = RD$TBL(I_CNTL,'%YEAR LABEL%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$I1(USYEAR,UISTYR,MNUMYR)                           ! YEAR LABELS
      RET_CODE = RD$C1(UCYEAR,UISTYR,MNUMYR)                           ! YEAR LABELS
      RET_CODE = RD$R1(BARRIER,UISTYR,MNUMYR)                          ! TRADE BARRIER
      RET_CODE = RD$I1(USW_DBGRP,UISTYR,MNUMYR)                        ! DBGRP SWITCH
      RET_CODE = RD$I1(US_GRPS,UISTYR,MNUMYR)                          ! Start New Groups for New Units 1=>Yes
      RET_CODE = RD$R2(EMRFSA,UISTYR,1,MX_SO2_GRP,MNUMYR,MX_SO2_GRP)   ! Initial SO2 Allowances
      RET_CODE = RD$R2(EMELBNK,UISTYR,1,MX_SO2_GRP,MNUMYR,MX_SO2_GRP)  ! Initial SO2 Banked Allowances
      RET_CODE = RD$R2(UPTPSO2,UISTYR,1,MX_SO2,MNUMYR,MX_SO2)          ! Target SO2 Penalty Price
      RET_CODE = RD$R1(USO2_BA_CRD,UISTYR,MNUMYR)                      ! SO2 Banked Allowance Credits
      RET_CODE = RD$R1(USO2_BA_WGT,UISTYR,MNUMYR)                      ! SO2 Banked Allowance Weights
      RET_CODE = RD$R2(SO2_SHR_ALW_GRP,UISTYR,1,MX_SO2_GRP,MNUMYR,MX_SO2_GRP)  ! Value of SO2 Allowances Shared Across Groups
      DO IYR = 1 , UISTYR - 1
         USYEAR(IYR) = UHBSYR + IYR
      END DO
!
      IF (UF_DBG .GT. 0) THEN
         DO IYR = 1 , UNYEAR
            FULLYR = IYR + UHBSYR
            WRITE (UF_DBG,1010) FULLYR,(EMRFSA(IYR,ISO2),EMELBNK(IYR,ISO2),SO2_SHR_ALW_GRP(IYR,ISO2),ISO2=1,MX_SO2_GRP)
 1010       FORMAT(1X,"SO2_INPUT:",I5,2(":",F12.2,":",F12.3,":",F8.3))
         END DO
      END IF
!
!     READ IN HISTORICAL SO2 ALLOWANCE PRICES (NOMINAL $/TON) USED TO OVERWRITE ECP_PSO2
!
      RET_CODE = RD$TBL(I_CNTL,'%SO2_ALLPR %',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R2(UPSO2PRC,UISTYR,1,MX_SO2_GRP,MNUMYR,MX_SO2_GRP)   ! Historical SO2 Allowance Prices
!
!     READ IN Definition of SO2 Compliance Groups by Coal Demand Region (Share Allocated to Each Compliance Group)
!
      RET_CODE = RD$TBL(I_CNTL,'%SO2_COL_BY%',MX_HG_GRP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
!     RET_CODE = RD$I1(SO2_GRP_BY_CLRG,1,MX_HG_GRP)     ! Define SO2 Compliance Groups by Coal Demand Region
      RET_CODE = RD$R2(SO2_SHR_BY_CLRG,1,1,MX_SO2_GRP,MX_HG_GRP,MX_SO2_GRP)   ! Shr of SO2 Emissions by Compliance Grp in CL Demand Region
!
!     READ IN Definition of SO2 Compliance Groups by Census (Oil) Region (Share Allocated to Each Compliance Group)
!
      RET_CODE = RD$TBL(I_CNTL,'%SO2_OIL_BY%',MNUMCR,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
!     RET_CODE = RD$I1(SO2_GRP_BY_CLRG,1,MX_HG_GRP)     ! Define SO2 Compliance Groups by Coal Demand Region
      RET_CODE = RD$R2(SO2_SHR_BY_OLRG,1,1,MX_SO2_GRP,MNUMCR,MX_SO2_GRP)   ! Shr of SO2 Emissions by Compliance Grp in CL Demand Region
!
      NUM_SO2_GRP = 0
      DO ICLRG = 1 , MX_HG_GRP
        DO ISO2 = 1 , MX_SO2_GRP
!        NUM_SO2_GRP = MAX(NUM_SO2_GRP,SO2_GRP_BY_CLRG(ICLRG))
         IF (SO2_SHR_BY_CLRG(ICLRG,ISO2) .GT. 0.0)NUM_SO2_GRP = MAX(NUM_SO2_GRP,ISO2)
        END DO
!        write(6,1234) iclrg,so2_shr_by_clrg(iclrg,1),so2_shr_by_clrg(iclrg,2),num_so2_grp
!1234 format(1h ,'!caircl',i3,2f10.4,i3)
      END DO
      DO ICLRG = 1 , MNUMCR
        DO ISO2 = 1 , MX_SO2_GRP
!        NUM_SO2_GRP = MAX(NUM_SO2_GRP,SO2_GRP_BY_CLRG(ICLRG))
         IF (SO2_SHR_BY_OLRG(ICLRG,ISO2) .GT. 0.0)NUM_SO2_GRP = MAX(NUM_SO2_GRP,ISO2)
        END DO
!        write(6,2345) iclrg,so2_shr_by_olrg(iclrg,1),so2_shr_by_olrg(iclrg,2),num_so2_grp
!2345 format(1h ,'!cairol',i3,2f10.4,i3)
      END DO
!
!     READ IN Switches (Years) for EPA Transport Rule
!
      RET_CODE = RD$TBL(I_CNTL,'%TRAN SO2 SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(TRANRULE1,1,1)
      RET_CODE = RD$I1(TRANRULE2,1,1)
      IF (TRANRULE1 .LE. 0)TRANRULE1 = 9999
      IF (TRANRULE2 .LE. 0)TRANRULE2 = 9999
!
!     READ IN SO2 Limits for EPA Transport Rule
!
      RET_CODE = RD$TBL(I_CNTL,'%TRAN SO2 EM%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R2(TEMRFSA,UISTYR,1,MX_SO2_GRP,MNUMYR,MX_SO2_GRP)   ! Initial SO2 Allowances
!
!     READ IN Definition of SO2 Transport  Groups by Coal Demand Region (Share Allocated to Each Compliance Group)
!
      RET_CODE = RD$TBL(I_CNTL,'%TSO2_COL_BY%',MX_HG_GRP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(TSO2_SHR_BY_CLRG,1,1,MX_SO2_GRP,MX_HG_GRP,MX_SO2_GRP)   ! Shr of SO2 Emissions by Compliance Grp in CL Demand Region
!     DO ICLRG = 1 , MX_HG_GRP
!       DO ISO2 = 1 , MX_SO2_GRP
!          IF (ISO2 .LE. MX_SO2_GRP)THEN
!             IF (SO2_SHR_BY_CLRG(ICLRG,ISO2) .GT. 0.0)  &
!                TSO2_SHR_BY_CLRG(ICLRG,ISO2) = MAX(TSO2_SHR_BY_CLRG(ICLRG,ISO2),0.0001)
!          END IF
!       END DO
!        write(6,1234) iclrg,so2_shr_by_clrg(iclrg,1),so2_shr_by_clrg(iclrg,2),  &
!                           tso2_shr_by_clrg(iclrg,1),tso2_shr_by_clrg(iclrg,2),tso2_shr_by_clrg(iclrg,3)
!1234 format(1h ,'!cshr1',i3,5f10.4)
!     END DO
!     DO ICLRG = 1 , MX_HG_GRP
!        write(6,1235) iclrg,tso2_shr_by_clrg(iclrg,1),tso2_shr_by_clrg(iclrg,2),  &
!                            tso2_shr_by_clrg(iclrg,3),num_so2_grp
!1235 format(1h ,'!cshr2',i3,3f10.4,i3)
!     END DO
!
!     READ IN Definition of SO2 Transport  Groups by Census (Oil) Region (Share Allocated to Each Compliance Group)
!
      RET_CODE = RD$TBL(I_CNTL,'%TSO2_OIL_BY%',MNUMCR,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(TSO2_SHR_BY_OLRG,1,1,MX_SO2_GRP,MNUMCR,MX_SO2_GRP)   ! Shr of SO2 Emissions by Compliance Grp in CL Demand Region
!
!     READ IN State SO2 Compliance Data for EPA Transport Rule
!
      RET_CODE = RD$TBL(I_CNTL,'%TRAN SO2 ST%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(TSO2_NST,1,1)
      RET_CODE = RD$I1(TSO2_YR_BY_ST,1,1)
      RET_CODE = RD$I1(TSO2_VR_BY_ST,1,1)
      RET_CODE = RD$I1(TSO2_YR_BY_CLRG,1,1)
      RET_CODE = RD$I1(TSO2_VR_BY_CLRG,1,1)
      IF (TSO2_YR_BY_ST .LE. 0)TSO2_YR_BY_ST = 9999
      IF (TSO2_YR_BY_CLRG .LE. 0)TSO2_YR_BY_CLRG = 9999
!     print *,'!tso2dat',curiyr+1989,tso2_yr_by_st,tso2_yr_by_clrg
!
      RET_CODE = RD$TBL(I_CNTL,'%TRAN SO2 INPUTS%',TSO2_NST,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(TSO2_ST,1,TSO2_NST)
      RET_CODE = RD$I1(TSO2_TR_BY_ST,1,TSO2_NST)
      RET_CODE = RD$I1(TSO2_CL_BY_ST,1,TSO2_NST)
      RET_CODE = RD$I1(TSO2_OL_BY_ST,1,TSO2_NST)
      RET_CODE = RD$R1(TSO2_LM1_BY_ST,1,TSO2_NST)
      RET_CODE = RD$R1(TSO2_LIM_BY_ST,1,TSO2_NST)
      RET_CODE = RD$R1(TSO2_VR1_BY_ST,1,TSO2_NST)
!     RET_CODE = RD$R1(TSO2_VR3_BY_ST,1,TSO2_NST)
      RET_CODE = RD$R1(TSO2_CSH_BY_ST,1,TSO2_NST)
      RET_CODE = RD$R1(TSO2_OSH_BY_ST,1,TSO2_NST)
!
      IF (TRANRULE1 .LT. 9999)THEN
!        INITIALIZE LIMITS BY COAL REGION TO ACCUMLATE
         TSO2_LM1_BY_CLRG = 0.0
         TSO2_LIM_BY_CLRG = 0.0
         TSO2_VR1_BY_CLRG = 0.0
!        TSO2_VR3_BY_CLRG = 0.0
         TSO2_OSH_BY_CLRG = 0.0
         TSO2_OSH_BY_OLCL = 0.0
         TSO2_OSH = 0.0
         DO ISO2 = 1 , TSO2_NST
            TRN = TSO2_TR_BY_ST(ISO2)
            COA = TSO2_CL_BY_ST(ISO2)
            OIL = TSO2_OL_BY_ST(ISO2)
            IF (TRN .EQ. 1)THEN
               TSO2_LM1_BY_CLRG(COA) = TSO2_LM1_BY_CLRG(COA) + TSO2_LM1_BY_ST(ISO2)
               TSO2_LIM_BY_CLRG(COA) = TSO2_LIM_BY_CLRG(COA) + TSO2_LIM_BY_ST(ISO2)
               TSO2_VR1_BY_CLRG(COA) = TSO2_VR1_BY_CLRG(COA) + TSO2_VR1_BY_ST(ISO2)
!              TSO2_VR3_BY_CLRG(COA) = TSO2_VR3_BY_CLRG(COA) + TSO2_VR3_BY_ST(ISO2)
               TSO2_OSH_BY_CLRG(COA) = TSO2_OSH_BY_CLRG(COA) + TSO2_OSH_BY_ST(ISO2)
            END IF
               TSO2_OSH(COA) = TSO2_OSH(COA) + TSO2_OSH_BY_ST(ISO2)
               TSO2_OSH_BY_OLCL(OIL,COA,TRN) = TSO2_OSH_BY_OLCL(OIL,COA,TRN) + TSO2_OSH_BY_ST(ISO2)
         END DO
         DO OIL = 1 , MNUMCR - 2
!           write(6,2232) curiyr+1989,OIL,(tso2_osh_by_olcl(oil,coa,1),coa = 1 , 16)
!2232 format(1h ,'!tso2olcl',i4,i3,16f6.3)
         END DO
!        DO COA = 1 , NDREG
!           write(6,2233) curiyr+1989,coa,  &
!              TSO2_LM1_BY_CLRG(COA),  &
!              TSO2_LIM_BY_CLRG(COA),  &
!              TSO2_VR1_BY_CLRG(COA),  &
!              TSO2_VR3_BY_CLRG(COA),  &
!              TSO2_OSH_BY_CLRG(COA)
!2233 format(1h ,'!tso2clrg',i4,i3,5f10.3)
!        END DO
         DO ISO2 = 1 , NDREG
            IF (TSO2_OSH(ISO2) .GT. 0.0)THEN
               TSO2_OSH_BY_CLRG(ISO2) = TSO2_OSH_BY_CLRG(ISO2) / TSO2_OSH(ISO2)
!              write(6,2323) curiyr+1989,iso2,tso2_osh_by_clrg(iso2)
!2323 format(1h ,'!tso2_osh',i4,i3,f10.3)
            END IF
            DO TRN = 1 , MX_SO2_GRP
               IF (TRN .LE. MX_SO2_GRP)THEN
                  IF (SO2_SHR_BY_CLRG(ISO2,TRN) .GT. 0.0)THEN
                     TSO2_SHR_BY_CLRG(ISO2,TRN) = MAX(TSO2_SHR_BY_CLRG(ISO2,TRN),0.0001)
                  END IF
                  IF (TSO2_SHR_BY_CLRG(ISO2,TRN) .GT. 0.0)THEN
                     SO2_SHR_BY_CLRG(ISO2,TRN) = MAX(SO2_SHR_BY_CLRG(ISO2,TRN),0.0001)
                  END IF
               END IF
            END DO
!     write(6,8899) curiyr+1989,iso2,  &
!                   SO2_SHR_BY_CLRG(ISO2,1),  &
!                   SO2_SHR_BY_CLRG(ISO2,2),  &
!                   TSO2_SHR_BY_CLRG(ISO2,1) , TSO2_SHR_BY_CLRG(ISO2,2)
!8899 format(1h ,'!so2cl',i4,i3,4f10.4)
         END DO
         SO2CR = 0.0
         TSO2_SHR_BY_OLRG = 0.0
         DO ISO2 = 1 , TSO2_NST
            TRN = TSO2_TR_BY_ST(ISO2)
            COA = TSO2_CL_BY_ST(ISO2)
            OIL = TSO2_OL_BY_ST(ISO2)
            SO2CR(OIL) = SO2CR(OIL) + TSO2_OSH_BY_ST(ISO2)
            TSO2_SHR_BY_OLRG(OIL,TRN) = TSO2_SHR_BY_OLRG(OIL,TRN) + TSO2_OSH_BY_ST(ISO2)
         END DO
!        DO ISO2 = 1 , MNUMCR - 2
!        write(6,2345) curiyr+1989,iso2,  &
!                      tso2_shr_by_olrg(iso2,1),  &
!                      tso2_shr_by_olrg(iso2,2)
!2345 format(1h ,'!tso2olrg',i4,i3,2f10.3)
!           DO TRN = 1 , MX_SO2_GRP
!              TSO2_SHR_BY_OLRG(ISO2,TRN) = TSO2_SHR_BY_OLRG(ISO2,TRN) / SO2CR(ISO2)
!              IF (TRN .LE. MX_SO2_GRP)THEN
!                 IF (SO2_SHR_BY_OLRG(ISO2,TRN) .GT. 0.0)THEN
!                    TSO2_SHR_BY_OLRG(ISO2,TRN) = MAX(TSO2_SHR_BY_OLRG(ISO2,TRN),0.0001)
!                 END IF
!                 IF (TSO2_SHR_BY_OLRG(ISO2,TRN) .GT. 0.0)THEN
!                    SO2_SHR_BY_OLRG(ISO2,TRN) = MAX(SO2_SHR_BY_OLRG(ISO2,TRN),0.0001)
!                 END IF
!              END IF
!           END DO
!        END DO
      END IF
!     do iso2 = 1 , ndreg
!     write(6,3437) curiyr+1989,iso2,tso2_shr_by_tran(iso2,1,1),  &
!                                    tso2_shr_by_tran(iso2,2,1),  &
!                                    tso2_shr_by_tran(iso2,3,1),  &
!                                    so2cl(iso2,1),  &
!                                    tso2_shr_by_tran(iso2,1,2),  &
!                                    tso2_shr_by_tran(iso2,2,2),  &
!                                    tso2_shr_by_tran(iso2,3,2),  &
!                                    so2cl(iso2,2)
!3437 format(1h ,'!trncl',i4,i3,8f10.3)
!     end do
!     do iso2 = 1 , tso2_nst
!     write(6,1357) iso2,tso2_st(iso2),tso2_cl_by_st(iso2),  &
!                                      tso2_ol_by_st(iso2),  &
!                                      tso2_lim_by_st(iso2),  &
!                                      tso2_vr1_by_st(iso2),  &
!                                      tso2_vr3_by_st(iso2),  &
!                                      tso2_csh_by_st(iso2),  &
!                                      tso2_osh_by_st(iso2)
!1357 format(1h ,'!tso2',i4,a4,2i4,6f10.3)
!     end do
!
!     READ IN Cap and Trade Pricing Options
!
      RET_CODE = RD$TBL(I_CNTL,'%C_T_P_OPT %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(PCAP_SO2,1,1)     ! Sulfur Dioxide Cap and Trade Pricing Option 0=>Auctioned Emission Permits 1=>Grandfathered Emission Permits
      RET_CODE = RD$I1(PCAP_NOX,1,1)     ! Nitrogen Oxide Cap and Trade Pricing Option 0=>Auctioned Emission Permits 1=>Grandfathered Emission Permits
      RET_CODE = RD$I1(PCAP_CAR,1,1)     ! Carbon Cap and Trade Pricing Option 0=>Auctioned Emission Permits 1=>Grandfathered Emission Permits
      RET_CODE = RD$I1(PCAP_HG,1,1)      ! Mercury Cap and Trade Pricing Option 0=>Auctioned Emission Permits 1=>Grandfathered Emission Permits
!
!     READ IN ASSUMED POST-NEMS GROWTH IN CARBON FEE (E.G., .01 = 1 PERCENT PER YEAR)
!
      RET_CODE = RD$TBL(I_CNTL,'%CAR_PNEMS %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UPCARGRW,1,1)   ! POST-NEMS Growth Rate for Carbon Fee
!     IF UPCARGRW > 999, COMPUTE GROWTH RATE USING BANK_ENDYR INSTEAD OF SPECIFIYING
      IF (UPCARGRW .GT. 999.0)THEN
        XYR = UNYEAR - MIN(BANK_ENDYR - UHBSYR,UNYEAR - 5)
        UPCARGRW = (EMETAX(2,UNYEAR) / EMETAX(2,UNYEAR - XYR)) ** (1.0 / FLOAT(XYR)) - 1.0
      END IF
      UPCARGRW = 1.0 + UPCARGRW
!
!     READ IN Carbon Allowance Allocation Specifications, If Any (i.e., McCain/Lieberman)
!
      RET_CODE = RD$TBL(I_CNTL,'%CAR_ALLOC %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UCAR_AL_YR,1,1)   ! Base Year for Deciding Carbon Allowance Allocations
      RET_CODE = RD$R1(UCAR_AL_RTE,1,1)  ! Share of Carbon Allowances Allocated to Ratepayers
!
!     READ IN Carbon Allowance Allocation Specifications, If Any (i.e., McCain/Lieberman)
!
      RET_CODE = RD$TBL(I_CNTL,'%CAR_ALLYR %',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(UCAR_AL_QTY,UISTYR,MNUMYR)   ! Annual Carbon Allowance Allocations to Generators
      RET_CODE = RD$R1(UCAR_LDAL_QTY,UISTYR,MNUMYR)   ! Annual Carbon Allowance Allocations to Load Entities
!
!     READ IN CO2 Capture Credit ($/MT)
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 CREDIT%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(UPCO2EOR,UISTYR,MNUMYR)   ! CO2 Capture Credit
!
!     READ IN SO2 Penalty Price Bound Info
!
      RET_CODE = RD$TBL(I_CNTL,'%PSO2 INFO %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UPSYEAR,1,1)                                     !
      RET_CODE = RD$I1(UPEBYR,1,1)                                     !
      RET_CODE = RD$R1(UPSXSMTH,1,1)                                     !
      RET_CODE = RD$R1(UPSUPFCTR,1,1)                                     !
      RET_CODE = RD$R1(UPSLWFCTR,1,1)                                     !
      RET_CODE = RD$R1(CL_GROW,1,1)                                     !
      RET_CODE = RD$R1(CL_SCST,1,1)                                     !
!
      RET_CODE = RD$TBL(I_CNTL,'%IJUMPEMRGN%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(IJUMPEMRGN,1,1)

       nNERCreg = UNRGNS
!            WRITE(6,2391) nNERCreg,UNRGNS,IJUMPEMRGN
! 2391       FORMAT(1X,"nNERCreg= ",I3,"UNRGNS=",I3,"IJUMPEMRGN=",I3)
!2391       FORMAT(1X,"ERROR: The Number of NERC Regions from the LDSMSTR READ, ",I3," Does Not Match the Number of NERC Regions in EMMCNTL File, ",I3," and This is a very bad Thing : ERROR")

      NUCDRAT = 1.0           ! Initialize to fill in early years
      
!
!     READ IN SHORT NAME FOR EACH REGION, ITT & CANADIAN IMPORT SWITCH

        DO I=1,MNUMNR
           URGNME(I) = "   "
           URGNUM(I) = 0
           UPRGCD(I) = " "
        ENDDO

        call EMM_CNTL
        Write(18,*)'URGNME ',(URGNME(I),I=1,MNUMNR)

      IF (UNRGNS.EQ.13) THEN
        URGNME(MNUMNR)=URGNME(16)
        URGNME(MNUMNR-1)=URGNME(15)
        URGNME(MNUMNR-2)=URGNME(14)
        URGNUM(MNUMNR)=URGNUM(16)
        URGNUM(MNUMNR-1)=URGNUM(15)
        URGNUM(MNUMNR-2)=URGNUM(14)
        UPRGCD(MNUMNR)=UPRGCD(16)
        UPRGCD(MNUMNR-1)=UPRGCD(15)
        UPRGCD(MNUMNR-2)=UPRGCD(14)
        DO I=14,22
           URGNME(I) = "   "
           URGNUM(I) = 0
           UPRGCD(I) = " "
        ENDDO
      ENDIF

!
!     READ IN NUMBER OF STATES
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM OF STAT',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UNSTAS,1,1)                                     !
!     MAKE SURE NUMBER OF STATES DOESN'T EXCEED DIMENSION
      IF (UNSTAS .GT. EMM_D_ST)THEN
         WRITE(6,3333)
 3333    FORMAT(1H ,'****** VALUE OF UNSTAS EXCEEDS EMM_D_ST ******')
         STOP
      END IF
!
!     READ IN STATE ABBREVIATION AND CODE FOR ROWS/COLUMNS
!
      RET_CODE = RD$TBL(I_CNTL,'%STATE PARM%',UNSTAS,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(USTNME,1,UNSTAS)                    ! STATE ABBREVIATION
      RET_CODE = RD$C1(USTACD,1,UNSTAS)                    ! STATE CODE
!
!     INITIALIZE IMPORT AND EXPORT INDICATORS TO 0 (OFF)
!     USE ECPDAT TRANSMISSION DATA TO TURN INDICATORS TO 1 (ON)
!
      DO IRG = 1 , MNUMNR
         UPGTRN(IRG) = 0
         UPCIMP(IRG) = 0
      END DO
!
!     EFD PLANT TYPE CODES
!
      RET_CODE = RD$TBL(I_CNTL,'%EFD PLT CD%',EFD_D_CAP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(EPPLCD,1,EFD_D_CAP)          ! EFD PLANT TYPE CODES
      RET_CODE = RD$C1(EFDNAME,1,EFD_D_CAP)         ! EFD LONG PLANT NAME
      RET_CODE = RD$I1(EPPOPR,1,EFD_D_CAP)          ! EFD OPERATE TYPE CODE
      RET_CODE = RD$I1(EPPOPM,1,EFD_D_CAP)          ! EFD PLANNED MAINT OPTION CODE
      RET_CODE = RD$I2(EPFLTP,1,1,ECP_D_FPP,EFD_D_CAP,ECP_D_FPP)          ! EFD FUEL OPTIONS
      RET_CODE = RD$I1(EPFTABI,1,EFD_D_CAP)          ! EFD mapping to ftab table 9 categories
!
      DO ICAP = 1 , EFD_D_CAP
         IF (EPPLCD(ICAP) .EQ. 'COU') UICOU = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CSU') UICSU = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CSC') UICSC = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CNC') UICNC = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CAV') UICAV = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CNG') UICNG = ICAP
         IF (EPPLCD(ICAP) .EQ. 'COQ') UICOQ = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CAS') UICAS = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CTN') UICTN = ICAP
         IF (EPPLCD(ICAP) .EQ. 'STO') UISTO = ICAP
         IF (EPPLCD(ICAP) .EQ. 'STG') UISTG = ICAP
         IF (EPPLCD(ICAP) .EQ. 'STX') UISTX = ICAP
         IF (EPPLCD(ICAP) .EQ. 'ICE') UIICE = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CTO') UICTO = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CTG') UICTG = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CTX') UICTX = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CTA') UICTA = ICAP
         IF (EPPLCD(ICAP) .EQ. 'ACT') UIACT = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CCO') UICCO = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CCG') UICCG = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CCX') UICCX = ICAP
         IF (EPPLCD(ICAP) .EQ. 'ACC') UIACC = ICAP
         IF (EPPLCD(ICAP) .EQ. 'AC2') UIAC2 = ICAP
         IF (EPPLCD(ICAP) .EQ. 'ACS') UIACS = ICAP
         IF (EPPLCD(ICAP) .EQ. 'FCG') UIFCG = ICAP
         IF (EPPLCD(ICAP) .EQ. 'CNU') UICNU = ICAP
         IF (EPPLCD(ICAP) .EQ. 'ANC') UIANC = ICAP
         IF (EPPLCD(ICAP) .EQ. 'SMR') UISMR = ICAP
         IF (EPPLCD(ICAP) .EQ. 'GFN') UIGFN = ICAP
         IF (EPPLCD(ICAP) .EQ. 'DGB') UIDGB = ICAP
         IF (EPPLCD(ICAP) .EQ. 'DGP') UIDGP = ICAP
         IF (EPPLCD(ICAP) .EQ. 'BMS') UIBMS = ICAP
         IF (EPPLCD(ICAP) .EQ. 'BIG') UIBIG = ICAP
         IF (EPPLCD(ICAP) .EQ. 'GTH') UIGTH = ICAP
         IF (EPPLCD(ICAP) .EQ. 'AGT') UIAGT = ICAP
         IF (EPPLCD(ICAP) .EQ. 'MSW') UIMSW = ICAP
         IF (EPPLCD(ICAP) .EQ. 'HYC') UIHYC = ICAP
         IF (EPPLCD(ICAP) .EQ. 'HYA') UIHYA = ICAP
         IF (EPPLCD(ICAP) .EQ. 'HYT') UIHYT = ICAP
         IF (EPPLCD(ICAP) .EQ. 'HYC') UIHYC = ICAP
         IF (EPPLCD(ICAP) .EQ. 'HYR') UIHYR = ICAP
         IF (EPPLCD(ICAP) .EQ. 'QST') UIQST = ICAP
         IF (EPPLCD(ICAP) .EQ. 'DST') UIDST = ICAP
         IF (EPPLCD(ICAP) .EQ. 'OST') UIOST = ICAP
         IF (EPPLCD(ICAP) .EQ. 'WND') UIWND = ICAP
         IF (EPPLCD(ICAP) .EQ. 'WNL') UIWNL = ICAP
         IF (EPPLCD(ICAP) .EQ. 'WFS') UIWFS = ICAP
         IF (EPPLCD(ICAP) .EQ. 'STH') UISTH = ICAP
         IF (EPPLCD(ICAP) .EQ. 'STS') UISTS = ICAP
         IF (EPPLCD(ICAP) .EQ. 'ST2') UIST2 = ICAP
         IF (EPPLCD(ICAP) .EQ. 'SPV') UISPV = ICAP
         IF (EPPLCD(ICAP) .EQ. 'PVT') UIPVT = ICAP
         IF (EPPLCD(ICAP) .EQ. 'OIN') UIOIN = ICAP
      END DO
!     write(*,*)'EPPPLCD, UIWFS= ',UIWFS
!
!     READ IN YEAR TO START GPS, IF ANY
!
      RET_CODE = RD$TBL(I_CNTL,'%GPS START YEAR%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(GPSSYR,1,1)                                      !
!
!     READ IN GPS ALLOCATION SWITCHES
!
      RET_CODE = RD$TBL(I_CNTL,'%GPS ALLOC %',EFD_D_CAP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(GPSCSW,1,EFD_D_CAP)          ! EFD OPERATE TYPE CODE
      RET_CODE = RD$I1(GPSSSW,1,EFD_D_CAP)          ! EFD OPERATE TYPE CODE
      RET_CODE = RD$I1(GPSNSW,1,EFD_D_CAP)          ! EFD OPERATE TYPE CODE
      RET_CODE = RD$I1(GPSHSW,1,EFD_D_CAP)          ! EFD OPERATE TYPE CODE
!     DO ICAP = 1, EFD_D_CAP
!     write (6,*) ICAP,GPSCSW(ICAP),GPSSSW(ICAP),GPSNSW(ICAP),GPSNSW(ICAP)
!     enddo
!
!     READ IN ASSUMED GROWTH IN NUC FUEL COST AFTER LAST MODEL YEAR
!
      RET_CODE = RD$TBL(I_CNTL,'%NUCLEAR PRCGRW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UPNUCGRW,1,1)                                    !
      UPNUCGRW = 1.0 + UPNUCGRW
!
!     READ IN YEAR DEFINITIONS FOR NUCLEAR CAPACITY FACTORS
!
      RET_CODE = RD$TBL(I_CNTL,'%NUCLEAR CFCYRS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UNCFBYR,1,1)                                     !
      RET_CODE = RD$I1(UNCFVYR,1,1)                                     !
!
!     READ IN BREAK POINTS FOR INCREASE/LEVEL/DECREASE IN CAPACITY FACTORS
!
      RET_CODE = RD$TBL(I_CNTL,'%NUCLEAR CFCPTS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UNCFEI1,1,1)                                     !
      RET_CODE = RD$I1(UNCFEI2,1,1)                                     !
      RET_CODE = RD$I1(UNCFEL1,1,1)                                     !
      RET_CODE = RD$I1(UNCFEL2,1,1)                                     !
!
!     READ IN BREAK POINTS FOR INCREASE/LEVEL/DECREASE IN CAPACITY FACTORS
!
      RET_CODE = RD$TBL(I_CNTL,'%NUCLEAR CFCRTS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UNCFRI1,1,1)                                     !
      RET_CODE = RD$R1(UNCFRI2,1,1)                                     !
      RET_CODE = RD$R1(UNCFRD1,1,1)                                     !
      RET_CODE = RD$R1(UNCFRD2,1,1)                                     !
!     MAXIMUM ALLOWABLE CAPACITY FACTOR
      RET_CODE = RD$R1(UNCFMAX,1,1)                                     !

!
!     READ IN NUCLEAR DERATE FACTOR BY YEAR
!
      RET_CODE = RD$TBL(I_CNTL,'%NUC DRATE RPT%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NUCDRPT,1,1)                                     !
!
!     READ IN OIL USE FRACTIONS IN COAL PLANTS BY OIL REGION
!
      RET_CODE = RD$TBL(I_CNTL,'%OIL  REG MAP %',MNUMCR,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UFROCS,1,EFD_D_MFRG)        ! Percent Oil Use in Coal Plants
!
!     READ IN MAPPING OF GAS REGIONS TO CENSUS AND NERC REGIONS
!
      RET_CODE = RD$TBL(I_CNTL,'%GAS  REG MAP %',NNGEM,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(UMP_GASR,1,1,2,NNGEM,2)      ! REGION MAPS
      RET_CODE = RD$R1(UFRGCS,1,EFD_D_MFRG)        ! Percent Gas Use in Coal Plants
!
!     READ IN MAPPING OF COAL REGIONS TO CENSUS AND NERC REGIONS
!
      RET_CODE = RD$TBL(I_CNTL,'%COAL  REG MAP %',NDRGG,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(UMP_COLR,1,1,2,NDREG,2)             ! REGION MAPS
!
!     READ IN FUEL REGION DATA
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM FUEL  %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UNFUELS,1,1)                    ! NUMBER OF FUELS
      RET_CODE = RD$I1(FLBASE,1,1)             ! BASE YEAR FOR FUEL DATA
      RET_CODE = RD$I1(FLLAST,1,1)             ! LAST YEAR FOR FUEL DATA
      RET_CODE = RD$I1(UIFPLT,1,1)     ! NUMBER OF FUELS PER PLANT GROUP
      RET_CODE = RD$I1(UFLSTITR,1,1)  ! START ITERATION FOR FUEL PRICE SMOOTHING
!
      RET_CODE = RD$TBL(I_CNTL,'%FUEL PARM %',UNFUELS,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(UNMFL,1,EFD_D_NFL)                 ! LONG FUEL NAME
      RET_CODE = RD$I1(UNFLRG,1,EFD_D_NFL)       ! # REGIONS PER FUEL CODE
      RET_CODE = RD$C1(UFLCODE,1,EFD_D_NFL)         ! 2 CHAR EFD FUEL CODE
      RET_CODE = RD$C2(UCDFLRG,1,1,EFD_D_MFRG,EFD_D_NFL,EFD_D_MFRG)
!
      RET_CODE = RD$TBL(I_CNTL,'%FUEL PARM2 %',UNFUELS,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UFRVOC,1,EFD_D_NFL)  ! VOC Emission Rate
      RET_CODE = RD$R1(HCNT,1,EFD_D_NFL)    ! Heat Content
      RET_CODE = RD$R1(RSO2,1,EFD_D_NFL)    ! SO2 Emission
!
!     ASH NOW ACCOUNTED FOR IN CMM FACTORS, CARBON WILL COME FROM EPM
!
      DO IRG = 1 , EFD_D_MFRG
         DO IFL = 1 , EFD_D_NFL
            UFHCNT(IFL,IRG) = HCNT(IFL)
            UFRSO2(IFL,IRG) = RSO2(IFL)
         END DO
      END DO
!
!     INITIALIZE MERCURY CONTENT BY FUEL TO 0.0
!
      DO IRG =1 , EFD_D_MFRG
         DO IFL = 1 , EFD_D_NFL
            UFRHG(IFL,IRG) = 0.0
         ENDDO
      ENDDO
!
!     SET UP INDEX ARRAY OF FUEL TYPES
!
      UNFLRGS = 0
      DO 30 IFL = 1, UNFUELS
         IF (UFLCODE(IFL).EQ.'B1') THEN
            UIB1 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'B2') THEN
            UIB2 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'B3') THEN
            UIB3 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'B4') THEN
            UIB4 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'B5') THEN
            UIB5 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'B6') THEN
            UIB6 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'B7') THEN
            UIB7 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'B8') THEN
            UIB8 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'C1') THEN
            UIC1 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'C2') THEN
            UIC2 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'C3') THEN
            UIC3 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'C4') THEN
            UIC4 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'C5') THEN
            UIC5 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'C6') THEN
            UIC6 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'C7') THEN
            UIC7 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'C8') THEN
            UIC8 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'C9') THEN
            UIC9 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'CX') THEN
            UICX = IFL
         ELSEIF (UFLCODE(IFL).EQ.'CY') THEN
            UICY = IFL
         ELSEIF (UFLCODE(IFL).EQ.'CZ') THEN
            UICZ = IFL
         ELSEIF (UFLCODE(IFL).EQ.'H1') THEN
            UIH1 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'H2') THEN
            UIH2 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'H3') THEN
            UIH3 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'H4') THEN
            UIH4 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'H5') THEN
            UIH5 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'H6') THEN
            UIH6 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'H7') THEN
            UIH7 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'H8') THEN
            UIH8 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'H9') THEN
            UIH9 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'HA') THEN
            UIHA = IFL
         ELSEIF (UFLCODE(IFL).EQ.'HB') THEN
            UIHB = IFL
         ELSEIF (UFLCODE(IFL).EQ.'HC') THEN
            UIHC = IFL
         ELSEIF (UFLCODE(IFL).EQ.'PC') THEN
            UIPC = IFL
         ELSEIF (UFLCODE(IFL).EQ.'OC') THEN
            UIOC = IFL
         ELSEIF (UFLCODE(IFL).EQ.'IG') THEN
            UIIG = IFL
         ELSEIF (UFLCODE(IFL).EQ.'I2') THEN
            UII2 = IFL
         ELSEIF (UFLCODE(IFL).EQ.'PQ') THEN
            UIPQ = IFL
         ELSEIF (UFLCODE(IFL).EQ.'IS') THEN
            UIIS = IFL
         ELSEIF (UFLCODE(IFL).EQ.'DS') THEN
            UIDS = IFL
         ELSEIF (UFLCODE(IFL).EQ.'RL') THEN
            UIRL = IFL
         ELSEIF (UFLCODE(IFL).EQ.'RH') THEN
            UIRH = IFL
         ELSEIF (UFLCODE(IFL).EQ.'GF') THEN
            UIGF = IFL
         ELSEIF (UFLCODE(IFL).EQ.'GI') THEN
            UIGI = IFL
         ELSEIF (UFLCODE(IFL).EQ.'GC') THEN
            UIGC = IFL
         ELSEIF (UFLCODE(IFL).EQ.'UF') THEN
            UIUF = IFL
         ELSEIF (UFLCODE(IFL).EQ.'OG') THEN
            UIOG = IFL
         ELSEIF (UFLCODE(IFL).EQ.'WD') THEN
            UIWD = IFL
         ELSEIF (UFLCODE(IFL).EQ.'SW') THEN
            UISW = IFL
         ELSEIF (UFLCODE(IFL).EQ.'WA') THEN
            UIWA = IFL
         ELSEIF (UFLCODE(IFL).EQ.'TI') THEN
            UITI = IFL
         ELSEIF (UFLCODE(IFL).EQ.'PS') THEN
            UIPS = IFL
         ELSEIF (UFLCODE(IFL).EQ.'WN') THEN
            UIWN = IFL
         ELSEIF (UFLCODE(IFL).EQ.'WL') THEN
            UIWL = IFL
         ELSEIF (UFLCODE(IFL).EQ.'WF') THEN
            UIWF = IFL
            !write(*,*)'UFLCODE, UIWF= ',UIWF
         ELSEIF (UFLCODE(IFL).EQ.'GT') THEN
            UIGT = IFL
         ELSEIF (UFLCODE(IFL).EQ.'SO') THEN
            UISO = IFL
         ELSEIF (UFLCODE(IFL).EQ.'PV') THEN
            UIPV = IFL
         ELSEIF (UFLCODE(IFL).EQ.'PT') THEN
            UIPT = IFL
         ELSEIF (UFLCODE(IFL).EQ.'DD') THEN
            UIDD = IFL
            !write(*,*)'UFLCODE, UIDD= ',UIDD
         ELSEIF (UFLCODE(IFL).EQ.'DG') THEN
            UIDG = IFL
         ENDIF
!

         IF (UNFLRGS .EQ. 0) THEN
            UNFLRGS = 1
            UPFLRG(1) = UCDFLRG(IFL,1)
         ELSE
            ICHK = 0
            DO IRG = 1 , UNFLRGS
               IF (UPFLRG(IRG) .EQ. UCDFLRG(IFL,1)) ICHK = 1
               ENDDO
            IF (ICHK .EQ. 0) THEN
               UNFLRGS = UNFLRGS + 1
               UPFLRG(UNFLRGS) = UCDFLRG(IFL,1)
            END IF
         END IF
 30   CONTINUE
!
      IF ((UIB1 .EQ. 0) .OR. (UIB2 .EQ. 0) .OR. (UIB3 .EQ. 0) .OR.   &
          (UIB4 .EQ. 0) .OR. (UIB5 .EQ. 0) .OR. (UIB6 .EQ. 0) .OR. &
          (UIB7 .EQ. 0) .OR. (UIB8 .EQ. 0) .OR. (UIC1 .EQ. 0) .OR. &
          (UIC2 .EQ. 0) .OR. (UIC3 .EQ. 0) .OR. (UIC4 .EQ. 0) .OR. &
          (UIC5 .EQ. 0) .OR. (UIC6 .EQ. 0) .OR. (UIC7 .EQ. 0) .OR. &
          (UIC8 .EQ. 0) .OR. (UIC9 .EQ. 0) .OR. (UICX .EQ. 0) .OR. &
          (UICY .EQ. 0) .OR. (UICZ .EQ. 0) .OR. (UIH1 .EQ. 0) .OR. &
          (UIH2 .EQ. 0) .OR. (UIH3 .EQ. 0) .OR. (UIH4 .EQ. 0) .OR. &
          (UIH5 .EQ. 0) .OR. (UIH6 .EQ. 0) .OR. (UIH7 .EQ. 0) .OR. &
          (UIH8 .EQ. 0) .OR. (UIH9 .EQ. 0) .OR. (UIHA .EQ. 0) .OR. &
          (UIHB .EQ. 0) .OR. (UIHC .EQ. 0) .OR. (UIPC .EQ. 0) .OR. &
          (UIOC .EQ. 0) .OR. (UIIG .EQ. 0) .OR. (UII2 .EQ. 0) .OR. &
          (UIPQ .EQ. 0) .OR. (UIIS .EQ. 0) .OR. (UIDS .EQ. 0) .OR. &
          (UIRL .EQ. 0) .OR. (UIRH .EQ. 0) .OR. (UIGF .EQ. 0) .OR. &
          (UIGI .EQ. 0) .OR. (UIGC .EQ. 0) .OR. (UIUF .EQ. 0) .OR. &
          (UIWL .EQ. 0) .OR. (UIPT .EQ. 0)) &
       WRITE(6,*) ' ** RDCNTRL ERROR: ONE OR MORE FUEL INDICES ARE 0'
!
!     READ SO2 EMISSION FACTORS
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM OF EMF%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NUM_EMF,1,1)                    ! NUMBER OF EMISSION FUELS
!
      RET_CODE = RD$TBL(I_CNTL,'%SO2 EMF%      ',NUM_EMF,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(EMS_FUEL,1,EMS_D_NFL)    ! EMISSION FUEL NAMES
      RET_CODE = RD$C1(EMS_UNIT,1,EMS_D_NFL)    ! EMISSION FUEL UNITS
      RET_CODE = RD$R1(SO2_EMF,1,EMS_D_NFL)     ! SO2 EMISSION FACTOR
      RET_CODE = RD$R1(SO2_MAX,1,EMS_D_NFL)     ! SO2 MAXIMUM EMISSION FACTOR
      RET_CODE = RD$R1(SO2_PCT,1,EMS_D_NFL)     ! SO2 EMISSIONS AS PERCENT OF SO2 IN FUEL
      RET_CODE = RD$C1(CNV_UNIT,1,EMS_D_NFL)    ! STANDARD TO STANDARD UNIT CONVERSION UNITS
      RET_CODE = RD$R1(CNV_FAC,1,EMS_D_NFL)     ! STANDARD TO STANDARD CONVERSION FACTORS
!
!     READ NOX EMISSION FACTORS
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM OF BTP%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NUM_BTP,1,1)                    ! NUMBER OF BOILER TYPES
!
      RET_CODE = RD$TBL(I_CNTL,'%NOX EMF%      ',NUM_BTP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(BTP_NAME,1,EMM_D_BTP)                   ! BOILER TYPE NAMES
      RET_CODE = RD$C1(BTP_FTCD,1,EMM_D_BTP)                   ! FIRE TYPE CODE
      RET_CODE = RD$C1(BTP_BTCD,1,EMM_D_BTP)                   ! BOTTOM TYPE CODE
      RET_CODE = RD$R2(NOX_EMF,1,1,NUM_EMF,EMM_D_BTP,EMS_D_NFL)  ! NOX EMISSION FACTOR
      RET_CODE = RD$R2(NOX_STD,1,1,2,EMM_D_BTP,2)              ! NOX EMISSION STANDARD
      RET_CODE = RD$I1(NOX_PH1,1,EMM_D_BTP)                    ! PHASE 1 START YEAR
      RET_CODE = RD$I1(NOX_PH2,1,EMM_D_BTP)                    ! PHASE 2 START YEAR
!
!     READ NOX EMISSION CONTROL TECHNOLOGY DATA
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM OF NCT%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NUM_NCT,1,1)                    ! NUMBER OF NOX CONTROL TECHNOLOGIES
!
      RET_CODE = RD$TBL(I_CNTL,'%NOX CONTROLS%',NUM_NCT,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(NCT_NAME,1,EMM_D_NCT)                   ! NOX CONTROL NAMES
      RET_CODE = RD$C1(NCT_CODE,1,EMM_D_NCT)                   ! NOX CONTROL CODES
      RET_CODE = RD$R1(NOX_CTL,1,EMM_D_NCT)                    ! NOX REDUCTION FACTORS
!
      NOX_LT = 1
!
!     READ NUMBER OF NOX CAP AND TRADE GROUPS
!
      DO IGRP = 1 , NOX_D_GRP
         DO JYR = 1, MNUMYR
!           EPNOXPR(IGRP,JYR) = 0.0
            NOXBYGRP(IGRP,JYR) = 0.0
         END DO
      END DO
!
!     Read in Info on NOX Banking - Applies Equally to National Cap Programs - Not to Northeast Seasonal Cap
!
      RET_CODE = RD$TBL(I_CNTL,'%NOX BNK INFO%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$R1(NOX_FCTR,1,1)                   ! Number of NOX Banks Allowed Over NOX Banks Created
      RET_CODE = RD$I1(NOX_BYR,1,1)                    ! Year NOX Banks can Start to be Created
      RET_CODE = RD$I1(NOX_SYR,1,1)                    ! Year NOX Banks can Start to be Used
!
      RET_CODE = RD$TBL(I_CNTL,'%NOX GROUPS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NOX_GRP,1,1)                    ! NOX_GRP - Number of NOX Cap and Trade Groups

!     WRITE(6,4313) CURIRUN, CURCALYR, NOX_GRP
!4313 FORMAT(1X,"NOX_GRP_Info",3(",",I5))

!     READ ID CODES FOR NOX REGIONS
!
      RET_CODE = RD$TBL(I_CNTL,'%NOX REGIONS%',NOX_GRP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(NOX_RG,1,NOX_GRP)               ! NOX_RG - NOX Region Codes
!
!     READ NOX TRADING YEAR, IF ANY
!
      RET_CODE = RD$TBL(I_CNTL,'%NOX TRADING%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NOX_TRDYR,1,1)                  ! NOX_TRDYR - NOX Trading Year, If Any
!
      DO IGRP = 1 , NOX_GRP

!        READ NUMBER OF STATES WITH NOX CAPS

         RET_CODE = RD$TBL(I_CNTL,'%NUM ST NOX%',1,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$I1(T_NOX_NST,1,1)                  ! NOX_NST - Number of States with NOX Caps
         RET_CODE = RD$I1(T_NOXYRS,1,1)                   ! NUMBER OF TIMES NOX CAPS CHANGE
         NOX_NST(IGRP) = T_NOX_NST
!
         RET_CODE = RD$TBL(I_CNTL,'%ST NOX CAPS%',T_NOX_NST+1,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(ST_NAME,1,NOX_D_MST+1)                   ! State Codes
         RET_CODE = RD$R2(ST_CAPS,1,1,T_NOXYRS,NOX_D_MST+1,MNUMYR) ! NOX EMISSION CAPS by STATE
!
         DO JYR = 1 , MNUMYR
            DO IST = 1 , NOX_D_MST
               NOXBYST(IST,IGRP,JYR) = 0.0
            END DO
            NOXBYGRP(IGRP,JYR) = 0.0
         END DO
!
         DO IST = 1 , NOX_NST(IGRP)
            NOX_ST(IST,IGRP) = ST_NAME(IST+1)
            DO INOXYR = 1 , T_NOXYRS
               SYR = ST_CAPS(1,INOXYR) - UHBSYR
               IF (INOXYR .LT. T_NOXYRS) THEN
                  EYR = ST_CAPS(1,INOXYR+1) - UHBSYR - 1
               ELSE
                  EYR = MNUMYR
               END IF
               DO JYR = SYR , EYR
                  NOXBYST(IST,IGRP,JYR) = ST_CAPS(IST+1,INOXYR)
                  NOXBYGRP(IGRP,JYR) = NOXBYGRP(IGRP,JYR) +  &
                  ST_CAPS(IST+1,INOXYR)
               END DO
            END DO
         END DO
         DO JYR = 1 , MNUMYR
            IF (NOXBYGRP(IGRP,JYR) .LT. 9999.0) THEN
               EMRFNA(IGRP,JYR) = NOXBYGRP(IGRP,JYR)
            ELSE
               EMRFNA(IGRP,JYR) = 0.0
            END IF
!           print *,'!noxallow',jyr+1989,igrp,emrfna(igrp,jyr)
         END DO
      END DO
!
!     READ NUMBER OF HOURS IN EACH MONTH IN WHICH NOX CAP APPLIES
!
      RET_CODE = RD$TBL(I_CNTL,'%NOX CAP SP%',NOX_GRP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(TMP_HRS,1,1,12,NOX_D_GRP,12)

!     WRITE(6,7335) CURIRUN, CURCALYR, CURITR, NOX_GRP, ECP_D_MSP, EFD_D_MSP, ECPns, EFDns
!7335 FORMAT(1X,"NOX_GRP_Num",7(",",I5))

      IF (ECPns .EQ. 0) ECPns = 3
      IF (EFDns .EQ. 0) EFDns = 3

      DO IGRP = 1 , NOX_GRP
         DO IMO = 1 , 12
            NOX_HRS(IMO,IGRP) = TMP_HRS(IGRP,IMO)
         END DO
!
         DO ISP = 1 , ECP_D_MSP
            NOX_ECP(ISP,IGRP) = 0.0
         END DO
!
         DO ISP = 1 , EFD_D_MSP
            NOX_EFD(ISP,IGRP) = 0.0
         END DO
      END DO

!     WRITE(6,3335) CURIRUN, CURIYR+1989, CURITR, EFDns, ECPns
!3335 FORMAT(1X,"EFDns_in_UDAT",5(":",I5))

      DO IGRP = 1 , NOX_GRP
         DO IMO = 1 , 12
            DO ISP = 1 , EFDnS
               NOX_EFD(ISP,IGRP) = NOX_EFD(ISP,IGRP) + NOX_HRS(IMO,IGRP) * (MO_EFD_SP(ISP,IMO) / MO_EFD_SP(0,IMO))

!              WRITE(6,6335) CURIRUN, CURCALYR, CURITR, IGRP, IMO, ISP, NOX_ECP(ISP,IGRP), NOX_HRS(IMO,IGRP), MO_ECP_SP(ISP,IMO), MO_ECP_SP(0,IMO)
!6335          FORMAT(1X,"NOX_EFD_Info",6(",",I5),4(",",F21.6))

            END DO
            DO ISP = 1 , ECPns
               NOX_ECP(ISP,IGRP) = NOX_ECP(ISP,IGRP) + NOX_HRS(IMO,IGRP) * (MO_ECP_SP(ISP,IMO) / MO_ECP_SP(0,IMO))

!              WRITE(6,5335) CURIRUN, CURCALYR, CURITR, IGRP, IMO, ISP, NOX_ECP(ISP,IGRP), NOX_HRS(IMO,IGRP), MO_ECP_SP(ISP,IMO), MO_ECP_SP(0,IMO)
!5335          FORMAT(1X,"NOX_ECP_Info",6(",",I5),4(",",F21.6))

            END DO
         END DO
      END DO
!
!     READ IN Definition of NOX Compliance Groups by Coal Demand Region (Share Allocated to Each Compliance Group)
!
      RET_CODE = RD$TBL(I_CNTL,'%NOX_COL_BY%',MX_HG_GRP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(NOX_SHR_BY_CLRG,1,1,NOX_GRP,MX_HG_GRP,NOX_GRP)   ! Shr of NOX Emissions by Compliance Grp in CL Demand Region
!
!     READ in Similar NOX Data for EPA Transport Rule
!
      RET_CODE = RD$TBL(I_CNTL,'%TNOX GROUPS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(TNOX_GRP,1,1)                    ! TNOX_GRP - Number of Transport Cap and Trade Groups
!     print *,'!tnoxgrp',tnox_grp
!
!     READ ID CODES FOR NOX REGIONS
!
      RET_CODE = RD$TBL(I_CNTL,'%TNOX REGIONS%',TNOX_GRP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(TNOX_RG,1,NOX_GRP)               ! TNOX_RG - NOX Region Codes
!
!     READ NOX TRADING YEAR, IF ANY
!
      RET_CODE = RD$TBL(I_CNTL,'%TNOX TRADING%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(TNOX_TRDYR,1,1)                  ! TNOX_TRDYR - NOX Trading Year, If Any
!
      DO IGRP = 1 , TNOX_GRP
!
!     READ NUMBER OF STATES WITH NOX CAPS
!
      RET_CODE = RD$TBL(I_CNTL,'%TNUM ST NOX%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$I1(T_NOX_NST,1,1)                  ! NOX_NST - Number of States with NOX Caps
      RET_CODE = RD$I1(T_NOXYRS,1,1)                   ! NUMBER OF TIMES NOX CAPS CHANGE
         TNOX_NST(IGRP) = T_NOX_NST
!
      RET_CODE = RD$TBL(I_CNTL,'%TST NOX CAPS%',T_NOX_NST+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(ST_NAME,1,NOX_D_MST+1)                   ! State Codes
      RET_CODE = RD$R2(ST_CAPS,1,1,T_NOXYRS,NOX_D_MST+1,MNUMYR) ! NOX EMISSION CAPS by STATE
!
       DO JYR = 1 , MNUMYR
         DO IST = 1 , NOX_D_MST
            TNOXBYST(IST,IGRP,JYR) = 0.0
         END DO
            TNOXBYGRP(IGRP,JYR) = 0.0
       END DO
!
       DO IST = 1 , TNOX_NST(IGRP)
            TNOX_ST(IST,IGRP) = ST_NAME(IST+1)
         DO INOXYR = 1 , T_NOXYRS
            SYR = ST_CAPS(1,INOXYR) - UHBSYR
            IF (INOXYR .LT. T_NOXYRS) THEN
               EYR = ST_CAPS(1,INOXYR+1) - UHBSYR - 1
            ELSE
               EYR = MNUMYR
            END IF
            DO JYR = SYR , EYR
               TNOXBYST(IST,IGRP,JYR) = ST_CAPS(IST+1,INOXYR)
               TNOXBYGRP(IGRP,JYR) = TNOXBYGRP(IGRP,JYR) +  &
               ST_CAPS(IST+1,INOXYR)
            END DO
         END DO
       END DO
       DO JYR = 1 , MNUMYR
         IF (NOXBYGRP(IGRP,JYR) .LT. 9999.0) THEN
            TEMRFNA(IGRP,JYR) = TNOXBYGRP(IGRP,JYR)
         ELSE
            TEMRFNA(IGRP,JYR) = 0.0
         END IF
!        print *,'!tnox',jyr+1989,igrp,temrfna(igrp,jyr)
       END DO
      END DO
!
!     READ NUMBER OF HOURS IN EACH MONTH IN WHICH NOX CAP APPLIES
!
      RET_CODE = RD$TBL(I_CNTL,'%TNOX CAP SP%',TNOX_GRP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(TMP_HRS,1,1,12,NOX_D_GRP,12)
!
      DO IGRP = 1 , TNOX_GRP
         DO IMO = 1 , 12
            TNOX_HRS(IMO,IGRP) = TMP_HRS(IGRP,IMO)
         END DO
!
         DO ISP = 1 , ECP_D_MSP
            TNOX_ECP(ISP,IGRP) = 0.0
         END DO
!
         DO ISP = 1 , EFD_D_MSP
            TNOX_EFD(ISP,IGRP) = 0.0
         END DO
      END DO

!
      DO IGRP = 1 , NOX_GRP
         DO IMO = 1 , 12
            DO ISP = 1 , EFDnS
               TNOX_EFD(ISP,IGRP) = TNOX_EFD(ISP,IGRP) + TNOX_HRS(IMO,IGRP) * (MO_EFD_SP(ISP,IMO) / MO_EFD_SP(0,IMO))
            END DO
            DO ISP = 1 , ECPns
               TNOX_ECP(ISP,IGRP) = TNOX_ECP(ISP,IGRP) + TNOX_HRS(IMO,IGRP) * (MO_ECP_SP(ISP,IMO) / MO_ECP_SP(0,IMO))
            END DO
         END DO
      END DO
!
!     READ IN Definition of NOX Compliance Groups by Coal Demand Region (Share Allocated to Each Compliance Group)
!
      RET_CODE = RD$TBL(I_CNTL,'%TNOX_COL_BY%',MX_HG_GRP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(TNOX_SHR_BY_CLRG,1,1,TNOX_GRP,MX_HG_GRP,TNOX_GRP)   ! Shr of NOX Emissions by Compliance Grp in CL Demand Region
!
!     READ IN Switch and settings for EPA 111 rule finalized in 2024
!
      RET_CODE = RD$TBL(I_CNTL,'%EPA 111 SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_EPA111,1,1)                  ! Switch to indicate EPA 111 rule is in place (0=no, 1=yes)
      RET_CODE = RD$I1(UEPA_GSYR,1,1)                   ! Last year that coal can convert to gas
      RET_CODE = RD$R1(UEPA_CGSH,1,1)                   ! Maximum coal share for coal-gas cofiring
      RET_CODE = RD$I1(UEPA_CLCFYR,1,1)                 ! Last year coal-gas cofiring can occur
      RET_CODE = RD$I1(UEPA_CLYR,1,1)                   ! Year that existing coal plants must retire/add CCS
      RET_CODE = RD$I1(UEPA_NWGSYR,1,1)                 ! Year that new gas CC plants must limit emissions (CF max or use CCS)
      RET_CODE = RD$R1(UEPA_NWGSCF,1,1)                 ! Maximum capacity factor allowed for new gas CC starting in NWGSYR     
!
!     READ IN Switch for State Grouping in National CO2 Standard and HR/Efficiency Improvements included, If any
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_STDSW,1,1)                    ! CO2_STDSW - Switch for Grouping States, If any
      RET_CODE = RD$I1(CO2_STDBY,1,1)                    ! CO2_STDBY - Year Data used for Grouping State Standards
      RET_CODE = RD$I1(CO2_STDWT,1,1)                    ! CO2_STDWT - Switch for Grouping Weights, If any
      RET_CODE = RD$I1(CO2_HRTSW,1,1)                    ! CO2_HRTSW - Switch for Including Heat Rate Improvements
      RET_CODE = RD$I1(CO2_EFFSW,1,1)                    ! CO2_EFFSW - Switch for Including Efficiency Improvements
      RET_CODE = RD$I1(CO2_STDPH,1,1)                    ! CO2_STDPH - Switch for Phasing in Intensity Standards for 1st few years
      RET_CODE = RD$I1(CO2_STDQT,1,1)                    ! CO2_STDQT - Switch for Type of Quantity Limit (0=existing cap,1 = ex/new)
      RET_CODE = RD$I1(CO2_EORSW,1,1)                    ! CO2_EORSW - Switch for Turn off CO2 for EOR (0= no,1 = yes)
      RET_CODE = RD$I1(CO2_PRCSW,1,1)                    ! CO2_PRCSW - Switch to Indicate pricing methodology (0=no adder, 1=EFD prc, 2=ECP prc, 3= diff (ECP-EFD)
      RET_CODE = RD$I1(CO2_ECPSW,1,1)                    ! CO2_ECPSW - Switch to Indicate ECP price to use (1=ECP year 2, 2=ECP levelized)
      RET_CODE = RD$I1(CO2_TRDSW,1,1)                    ! CO2_TRDSW - Switch to Include Interregional Trade in Intensity Standard (0=No, 1=Yes)
      RET_CODE = RD$I1(CO2_ERCSW,1,1)                    ! CO2_ERCSW - Switch to Include ERC Supply Curve in Final CPP (0=No, 1=Yes)
      RET_CODE = RD$I1(CO2_ERCPR,1,1)                    ! CO2_ERCPR - Switch for CPP ERC Pricing (0=No, 1=EFD, 2=ECP 1yr, 3=ECPlev)
      RET_CODE = RD$I1(CO2_ENDCG,1,1)                    ! CO2_ENDCG - Switch to Include End-Use Rnw in rate-base calculation (0=No, 1=yes)
!
!     READ IN EPA EMISSION RATE FOR BIOMASS
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 EMS WD%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(CO2_EMSWD,1,1)                    ! CO2_EMSWD - Biomass Emissions Rate
!
!     READ IN EPA EMISSION ADJUSTMENT FACTOR FOR NT COGEN
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 ADJ NT%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(CO2_ADJNT,1,1)                    ! CO2_ADJNT - NT Cogen Emissions Adjustment Factor
!
!     READ IN EPA EMISSION ADJUSTMENT FACTOR FOR 2040 (Fraction)
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 2040 P%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(CO2_PHS40,1,1)                    ! CO2_PHS40 - Additonal Emissions Reduction by 2040
      RET_CODE = RD$R1(CO2_PHSNY,1,1)                    ! CO2_PHSNY - Additonal Emissions Reduction by 2040 for NY, If Needed
!
!     READ IN CF THRESHOLD FOR ET AND LAST YEAR BEFORE COUNTING NEW AFF GEN
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 ET AFF%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(CO2_THRET,1,1)                    ! CO2_THRET - CF Threshold for ET
      RET_CODE = RD$I1(CO2_AFFYR,1,1)                    ! CO2_PHSNY - Last Yr before Counting New Aff Gen
!
!     READ IN FUEL REGION STANDARD TYPE (1=INTENSITY STANDARD, 2=MASS-BASED)
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD TF%',EFD_D_MFRG - 2,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_STDTF,1,EFD_D_MFRG - 2)              ! FUEL REGION STANDARD TYPE
!
!     READ IN EMM  REGION STANDARD TYPE (1=INTENSITY STANDARD, 2=MASS-BASED)
!!
!     Existing CT Generation to be Included In INTENSITY STANDARD
!!
!     READ IN STATE GROUPS AND HISTORICAL GENERATION/DEMAND
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD GR%',UNSTAS,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_STDGR,1,UNSTAS)                    ! STATE GROUPING, IF ANY
      RET_CODE = RD$R1(CO2_GENHS,1,UNSTAS)                    ! STATE GENERATION -- HISTORICAL
      RET_CODE = RD$R1(CO2_DEMHS,1,UNSTAS)                    ! STATE DEMAND -- HISTORICAL
!
!!     write(6,2323) (co2_gensn(2,ist),ist = 1 ,unrgns),co2_gensn(2,mnumnr)
!2323 format(1h ,'!gensn',23f8.0)
!
!     READ IN ALLOCATION SHARES FOR ALLOWANCES - USED IN QUANTITY CASE
!
      CO2_QALLGEN = 0.0              ! initialize / fill in early years
      CO2_QALLLD = 0.0
      CO2_QALLAUC = 0.0
!###################################################################
     
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
      allocate (col5(4))
      call sqlite3_column_query( col5(1), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col5(2), 'YEAR', sqlite_int )
      call sqlite3_column_query( col5(3), 'CO2QALLGEN', sqlite_real )
      call sqlite3_column_query( col5(4), 'CO2QALLLD', sqlite_real )
      call sqlite3_prepare_select( db, 'V_EMM_CNTL_CPP_ANNUAL', col5, stmt5)
     
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt5, col5, finished )
      if ( finished ) exit
      
      call sqlite3_get_column( col5(1), I)
      call sqlite3_get_column( col5(2), IY)
      call sqlite3_get_column( col5(3), TMPALLGN(IY-1989,I))
      call sqlite3_get_column( col5(4), TMPALLLD(IY-1989,I))
      end do
      deallocate ( col5 )

      call sqlite3_close( db )
!###################################################################

      DO IY = 1, MNUMYR
        DO IR = 1, MNUMNR
         CO2_QALLGEN(IR,IY) = TMPALLGN(IY,IR)
         CO2_QALLLD(IR,IY) = TMPALLLD(IY,IR)
         IF ((CO2_QALLGEN(IR,IY) + CO2_QALLLD(IR,IY)) .GT. 1.0) THEN
           write(6,*) 'ERROR IN CO2_QALL arrays '
           CO2_QALLGEN(IR,IY) = 1.0    ! assign all to generators if there is an error
           CO2_QALLLD(IR,IY) = 0.0
         ENDIF
         CO2_QALLAUC(IR,IY) = 1.0 - CO2_QALLGEN(IR,IY) - CO2_QALLLD(IR,IY)
        ENDDO
      ENDDO
!
!     READ IN Switch for Plant Types (Mass & Rate) and Incremental Generation Included in CO2 Standard, If any
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD PL%',ECP_D_CAP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(CO2_PLTMS,1,ECP_D_CAP)                   ! PLANT SWITCH, IF ANY FOR MASS STANDARDS
      RET_CODE = RD$R1(CO2_PLTRT,1,ECP_D_CAP)                   ! PLANT SWITCH, IF ANY FOR RATE STANDARDS
      RET_CODE = RD$R1(CO2_INCSW,1,ECP_D_CAP)                   ! INCREMENTAL GENERATION SWITCH, IF ANY
!     STORE PLANT SWITCH FOR REGION DEPENDING ON MASS OR RATE CASE
      DO IECP = 1 , ECP_D_CAP
         DO IR = 1 , MNUMNR
            IF (CO2_STDTN(IR) .EQ. 2)THEN
               CO2_PLTRG(IECP,IR) = CO2_PLTMS(IECP)
            ELSE
               CO2_PLTRG(IECP,IR) = CO2_PLTRT(IECP)
            END IF
!           INSURE THAT BOTH "INCREMENTAL" AND "ALL" SWITCHES AREN'T BOTH ACTIVE
            IF (CO2_INCSW(IECP) .GT. 0.0)CO2_PLTRG(IECP,IR) = 0.0
         END DO
      END DO
!
!     READ IN Switch for State Grouping in National CO2 Standard, If any
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD Y0%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_STDN0,1,1)                    ! CO2_STDYR - Number of Years for CO2 STD INPUTS
!     Annual Intensity Standard Targets - Original Rule
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD RT%',UNSTAS+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_STDST,1,1,CO2_STDN0,UNSTAS+1,MNUMYR) ! CO2 EMISSION STDS by STATE
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD Y1%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_STDN1,1,1)                    ! CO2_STDYR - Number of Years for CO2 STD INPUTS
!     Alternate Phasing by Increasing 1st year 10% and adjusting remaining years to compensate
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD R1%',UNSTAS+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_STDS1,1,1,CO2_STDN1,UNSTAS+1,MNUMYR) ! CO2 EMISSION STDS by STATE
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD Y2%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_STDN2,1,1)                    ! CO2_STDYR - Number of Years for CO2 STD INPUTS
!     Alternate Phasing by Holding Last 5 years at final value and increasing 1st 5 years accordingly
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD R2%',UNSTAS+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_STDS2,1,1,CO2_STDN2,UNSTAS+1,MNUMYR) ! CO2 EMISSION STDS by STATE
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD Y3%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_STDN3,1,1)                    ! CO2_STDYR - Number of Years for CO2 STD INPUTS
!     Alternate Phasing by Using EPA Alternate Goals
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD R3%',UNSTAS+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_STDS3,1,1,CO2_STDN3,UNSTAS+1,MNUMYR) ! CO2 EMISSION STDS by STATE
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD Y4%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_STDN4,1,1)                    ! CO2_STDYR - Number of Years for CO2 STD INPUTS
!     Annual Intensity Standard Targets - Final Rule No Phasein
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD R4%',UNSTAS+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_STDS4,1,1,CO2_STDN4,UNSTAS+1,MNUMYR) ! CO2 EMISSION STDS by STATE
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD Y5%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_STDN5,1,1)                    ! CO2_STDYR - Number of Years for CO2 STD INPUTS
!     Annual Intensity Standard Targets - Final Rule w/Phasein
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD R5%',UNSTAS+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_STDS5,1,1,CO2_STDN5,UNSTAS+1,MNUMYR) ! CO2 EMISSION STDS by STATE
!     Determine number of years and 1st/last year associated with selected targets
      IF (CO2_STDPH .LE. 0)THEN
         CO2_STDYR = CO2_STDN0
         CO2_STDY1 = CO2_STDST(1,1)
         CO2_STDYN = CO2_STDST(1,CO2_STDYR)
      ELSE IF (CO2_STDPH .EQ. 1)THEN
         CO2_STDYR = CO2_STDN1
         CO2_STDY1 = CO2_STDS1(1,1)
         CO2_STDYN = CO2_STDS1(1,CO2_STDYR)
         DO JYR = 1 , CO2_STDYR
            CO2_STDST(1,JYR) = CO2_STDS1(1,JYR)
         END DO
      ELSE IF (CO2_STDPH .EQ. 2)THEN
         CO2_STDYR = CO2_STDN2
         CO2_STDY1 = CO2_STDS2(1,1)
         CO2_STDYN = CO2_STDS2(1,CO2_STDYR)
         DO JYR = 1 , CO2_STDYR
            CO2_STDST(1,JYR) = CO2_STDS2(1,JYR)
         END DO
      ELSE IF (CO2_STDPH .EQ. 3)THEN
         CO2_STDYR = CO2_STDN3
         CO2_STDY1 = CO2_STDS3(1,1)
         CO2_STDYN = CO2_STDS3(1,CO2_STDYR)
         DO JYR = 1 , CO2_STDYR
            CO2_STDST(1,JYR) = CO2_STDS3(1,JYR)
         END DO
      ELSE IF (CO2_STDPH .EQ. 4)THEN
         CO2_STDYR = CO2_STDN4
         CO2_STDY1 = CO2_STDS4(1,1)
         CO2_STDYN = CO2_STDS4(1,CO2_STDYR)
         DO JYR = 1 , CO2_STDYR
            CO2_STDST(1,JYR) = CO2_STDS4(1,JYR)
         END DO
      ELSE IF (CO2_STDPH .EQ. 5)THEN
         CO2_STDYR = CO2_STDN5
         CO2_STDY1 = CO2_STDS5(1,1)
         CO2_STDYN = CO2_STDS5(1,CO2_STDYR)
         DO JYR = 1 , CO2_STDYR
            CO2_STDST(1,JYR) = CO2_STDS5(1,JYR)
         END DO
      END IF
!     IF (CO2_STDPH .EQ. 4 .OR. CO2_STDPH .EQ. 5)THEN
!        CO2_STDYR = CO2_STDYR - 2
!        CO2_STDST(1,1) = CO2_STDST(1,1) + FLOAT(2)
!        DO JYR = 2 , CO2_STDYR
!           CO2_STDST(1,JYR) = CO2_STDST(1,1) + FLOAT(JYR - 1)
!        END DO
!     END IF
!     IDENTIFY 1ST AND LAST YEARS OF TARGETS (LAST MEANS CONSTANT THEREAFTER)
!     CO2_STDY1 = CO2_STDST(1,1)
!     CO2_STDYN = CO2_STDST(1,CO2_STDYR)
!
      DO IST = 1 , UNSTAS
         DO JYR = 1 , CO2_STDYR
!           IF (CO2_STDST(IST + 1,JYR) .GT. 9000.0)THEN
!              CO2_STDRS(IST,CO2_STDST(1,JYR) - UHBSYR) = 0.0
!           ELSE
               IF (CO2_STDPH .LE. 0)THEN
                  CO2_STDRS(IST,CO2_STDST(1,JYR) - UHBSYR) = CO2_STDST(IST + 1,JYR)
!                 CO2_STDY1 = CO2_STDST(1,1)
!                 CO2_STDYN = CO2_STDST(1,CO2_STDYR)
               ELSE IF (CO2_STDPH .EQ. 1)THEN
                  CO2_STDRS(IST,CO2_STDST(1,JYR) - UHBSYR) = CO2_STDS1(IST + 1,JYR)
               ELSE IF (CO2_STDPH .EQ. 2)THEN
                  CO2_STDRS(IST,CO2_STDST(1,JYR) - UHBSYR) = CO2_STDS2(IST + 1,JYR)
               ELSE IF (CO2_STDPH .EQ. 3)THEN
                  CO2_STDRS(IST,CO2_STDST(1,JYR) - UHBSYR) = CO2_STDS3(IST + 1,JYR)
               ELSE IF (CO2_STDPH .EQ. 4)THEN
                  CO2_STDRS(IST,CO2_STDST(1,JYR) - UHBSYR) = CO2_STDS4(IST + 1,JYR)
               ELSE IF (CO2_STDPH .EQ. 5)THEN
                  CO2_STDRS(IST,CO2_STDST(1,JYR) - UHBSYR) = CO2_STDS5(IST + 1,JYR)
               END IF
!           END IF
         END DO
         DO JYR = 1 , CO2_STDY1 - UHBSYR - 1
            CO2_STDRS(IST,JYR) = 0.0
         END DO
!        IF (IST .EQ. 1)THEN
!        DO JYR = CO2_STDY1 , CO2_STDYN
!        write(6,3322) jyr,co2_stdrs(ist,jyr - uhbsyr)
!3322 format(1h ,'!stdyr',i4,f10.1)
!        END DO
!        END IF
         DO JYR = CO2_STDYN - UHBSYR + 1, MNUMYR + ECP_D_FPH
            IF (CO2_PHS40 .LE. 0.0)THEN
               CO2_STDRS(IST,JYR) = CO2_STDRS(IST,CO2_STDYN - UHBSYR)
            ELSE
               IF (JYR .LE. (2040 - UHBSYR)) THEN
                  IF (USTNME(IST) .EQ. 'NY' .AND. CO2_PHSNY .GT. 0.0)THEN
                     CO2_STDRS(IST,JYR) = CO2_STDRS(IST,CO2_STDYN - UHBSYR) * (1.0 - CO2_PHSNY * FLOAT(UHBSYR + JYR - CO2_STDYN) / FLOAT (2040 - CO2_STDYN))
                  ELSE
                     CO2_STDRS(IST,JYR) = CO2_STDRS(IST,CO2_STDYN - UHBSYR) * (1.0 - CO2_PHS40 * FLOAT(UHBSYR + JYR - CO2_STDYN) / FLOAT (2040 - CO2_STDYN))
                  END IF
               ELSE
                  CO2_STDRS(IST,JYR) = CO2_STDRS(IST,2040 - UHBSYR)
               END IF
            END IF
         END DO
!           IF (IST .EQ. 1)THEN
!        write(6,3322) co2_stdph,co2_stdyr,co2_stdy1,co2_stdyn,  &
!                         (co2_stdrs(ist,jyr - uhbsyr),jyr = co2_stdy1 - 1, co2_stdyn + 1)
!3322 format(1h ,'!stdyr',i3,i3,i5,i5,12f8.1)
!              DO JYR = CO2_STDYN + 1 , UHBSYR + MNUMYR + ECP_D_FPH
!              write(6,3322) jyr,co2_stdrs(ist,jyr - uhbsyr)
!              END DO
!           END IF
      END DO
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD YX%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_QTYN0,1,1)                    ! CO2_STDYR - Number of Years for CO2 STD INPUTS
!     Annual Mass-Based Standard Targets -- existing capacity only (preliminary rule)
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD QX%',UNSTAS+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_QTYQX,1,1,CO2_QTYN0,UNSTAS+1,MNUMYR) ! CO2 EMISSION STDS by STATE
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD YA%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_QTYN1,1,1)                    ! CO2_STDYR - Number of Years for CO2 STD INPUTS
!     Annual Mass-Based Standard Targets -- all      capacity (preliminary rule)
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD QA%',UNSTAS+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_QTYQA,1,1,CO2_QTYN1,UNSTAS+1,MNUMYR) ! CO2 EMISSION STDS by STATE
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 FIN YX%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_QTYN2,1,1)                    ! CO2_STDYR - Number of Years for CO2 STD INPUTS
!     Annual Mass-Based Standard Targets -- final rule (existing only)
      RET_CODE = RD$TBL(I_CNTL,'%CO2 FIN QX%',UNSTAS+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_QTYFX,1,1,CO2_QTYN2,UNSTAS+1,MNUMYR) ! CO2 EMISSION STDS by STATE
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 FIN YA%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_QTYN3,1,1)                    ! CO2_STDYR - Number of Years for CO2 STD INPUTS
!     Annual Mass-Based Standard Targets -- final rule (existing + new)
      RET_CODE = RD$TBL(I_CNTL,'%CO2 FIN QA%',UNSTAS+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_QTYFA,1,1,CO2_QTYN3,UNSTAS+1,MNUMYR) ! CO2 EMISSION STDS by STATE
!
      IF (CO2_STDQT .LE. 0)THEN
         CO2_STDYR = CO2_QTYN0
         CO2_STDY1 = CO2_QTYQX(1,1)
         CO2_STDYN = CO2_QTYQX(1,CO2_STDYR)
         DO JYR = 1 , CO2_STDYR
            CO2_STDST(1,JYR) = CO2_QTYQX(1,JYR)
         END DO
      ELSE IF (CO2_STDQT .EQ. 1)THEN
         CO2_STDYR = CO2_QTYN1
         CO2_STDY1 = CO2_QTYQA(1,1)
         CO2_STDYN = CO2_QTYQA(1,CO2_STDYR)
         DO JYR = 1 , CO2_STDYR
            CO2_STDST(1,JYR) = CO2_QTYQA(1,JYR)
         END DO
      ELSE IF (CO2_STDQT .EQ. 2)THEN
         CO2_STDYR = CO2_QTYN2
         CO2_STDY1 = CO2_QTYFX(1,1)
         CO2_STDYN = CO2_QTYFX(1,CO2_STDYR)
         DO JYR = 1 , CO2_STDYR
            CO2_STDST(1,JYR) = CO2_QTYFX(1,JYR)
         END DO
      ELSE IF (CO2_STDQT .EQ. 3)THEN
         CO2_STDYR = CO2_QTYN3
         CO2_STDY1 = CO2_QTYFA(1,1)
         CO2_STDYN = CO2_QTYFA(1,CO2_STDYR)
         DO JYR = 1 , CO2_STDYR
            CO2_STDST(1,JYR) = CO2_QTYFA(1,JYR)
         END DO
      END IF
!     print *,'!qtyin',co2_stdyr,co2_stdy1,co2_stdyn,co2_qtyfx(1,1),co2_qtyfa(1,1)
!
      DO IST = 1 , UNSTAS
         DO JYR = 1 , CO2_STDYR
!           IF (CO2_STDQX(IST + 1,JYR) .GT. 9000.0)THEN
!              CO2_STDQS(IST,CO2_STDST(1,JYR) - UHBSYR) = 0.0
!           ELSE
               IF (CO2_STDQT .LE. 0)THEN
                  CO2_STDQS(IST,CO2_STDST(1,JYR) - UHBSYR) = CO2_QTYQX(IST + 1,JYR)
               ELSE IF (CO2_STDQT .EQ. 1)THEN
                  CO2_STDQS(IST,CO2_STDST(1,JYR) - UHBSYR) = CO2_QTYQA(IST + 1,JYR)
               ELSE IF (CO2_STDQT .EQ. 2)THEN
!              CONVERT FINAL MASS STANDARDS FROMS SHORT TONS TO METRIC TONS
                  CO2_STDQS(IST,CO2_STDST(1,JYR) - UHBSYR) = CO2_QTYFX(IST + 1,JYR) * 2000.0 / 2204.0
               ELSE IF (CO2_STDQT .EQ. 3)THEN
!              CONVERT FINAL MASS STANDARDS FROMS SHORT TONS TO METRIC TONS
                  CO2_STDQS(IST,CO2_STDST(1,JYR) - UHBSYR) = CO2_QTYFA(IST + 1,JYR) * 2000.0 / 2204.0
               END IF
!           END IF
         END DO
         DO JYR = 1 , CO2_STDY1 - UHBSYR - 1
            CO2_STDQS(IST,JYR) = 0.0
         END DO
         DO JYR = CO2_STDYN - UHBSYR + 1, MNUMYR + ECP_D_FPH
            CO2_STDQS(IST,JYR) = CO2_STDQS(IST,CO2_STDYN - UHBSYR)
         END DO
!        if (ist .eq. 1)then
!        write(6,3323) co2_stdqt,co2_stdyr,co2_stdy1,co2_stdyn,  &
!                         (co2_stdqs(ist,jyr - uhbsyr),jyr = co2_stdy1 - 1, co2_stdyn + 1)
!3323 format(1h ,'!qtyyr',i3,i3,i5,i5,12f8.1)
!        end if
      END DO

!     IF THIS IS A 111D CASE SPECIFY THAT PLANTS WITH CO2 CAPTURE MUST STORE CAPTURED CARBON

      IF (CO2_STDSW .GT. 0) THEN
         DO JYR = CO2_STDY1 - UHBSYR , UNYEAR
            DO IST = 1 , EFD_D_MFRG - 2
               MUST_STORE(IST,JYR) = 1
            END DO
         END DO
      END IF
!
!     READ IN SWITCH TO GROUP INTENSITY STANDARDS BY FUEL REGION, IF APPROPRIATE
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 STD GF%',EFD_D_MFRG-2,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I2(CO2_STDGF,1,1,EFD_D_MFRG-2,EFD_D_MFRG,EFD_D_MFRG) ! GROUPING MATRIX
!
!     READ IN SWITCH TO GROUP INTENSITY STANDARDS BY EMM  REGION, IF APPROPRIATE
!
!     do igrp = 1 , EFD_D_MFRG - 2
!        write(6,3434) curiyr+1989,igrp,(co2_stdgf(igrp,ist),ist=1,EFD_D_MFRG-2)
!3434 format(1h ,'!grpf',i4,i3,23i3)
!     end do
!     do igrp = 1 , unrgns
!        write(6,3435) curiyr+1989,igrp,(co2_stdgn(igrp,ist),ist=1,unrgns)
!3435 format(1h ,'!grpn',i4,i3,23i3)
!     end do
!
!     READ NUMBER OF CO2 CAP AND TRADE GROUPS
!
      DO IGRP = 1 , CO2_D_GRP
         DO JYR = 1, MNUMYR
            EPCARPR(IGRP,JYR) = 0.0
            CO2BYGRP(IGRP,JYR) = 0.0
         END DO
      END DO
!
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 GROUPS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_GRP,1,1)                    ! CO2_GRP - Number of CO2 Cap and Trade Groups
!
!     READ ID CODES FOR CO2 REGIONS
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 REGIONS%',CO2_GRP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(CO2_RG,1,CO2_GRP)               ! CO2_RG - CO2 Region Codes
      DO IGRP = 1 , CO2_GRP
         IF (CO2_RG(IGRP) .EQ. 'RG')THEN
            CARGRP_RG = IGRP
         ELSE IF (CO2_RG(IGRP) .EQ. 'OT')THEN
            CARGRP_MW = IGRP
         ELSE IF (CO2_RG(IGRP) .EQ. 'CA')THEN
            CARGRP_CA = IGRP
         END IF
      END DO
!
      DO IGRP = 1 , CO2_GRP
!
!     READ NUMBER OF STATES WITH CO2 CAPS
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM ST CO2%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$I1(T_CO2_NST,1,1)                  ! CO2_NST - Number of States with CO2 Caps
      RET_CODE = RD$I1(T_CO2YRS,1,1)                   ! NUMBER OF TIMES CO2 CAPS CHANGE
         CO2_NST(IGRP) = T_CO2_NST
!
!     READ IN TYPE OF LIMIT (CAR/CO2 AND MT/ST)
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2 LIMITS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(CO2_TYP,1,1)                    ! CO2_TYP - CAR = 1, CO2 = 2
      RET_CODE = RD$I1(CO2_UNT,1,1)                    ! CO2_UNT - MT = 1, ST = 2
!
      RET_CODE = RD$TBL(I_CNTL,'%ST CO2 CAPS%',T_CO2_NST+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(ST_NAMEC,1,CO2_D_MST+1)                   ! State Codes
      RET_CODE = RD$R2(ST_CAPSC,1,1,T_CO2YRS,CO2_D_MST+1,MNUMYR) ! CO2 EMISSION CAPS by STATE
!
       DO JYR = 1 , MNUMYR
         DO IST = 1 , CO2_D_MST
               CO2BYST(IST,IGRP,JYR) = 0.0
         END DO
            CO2BYGRP(IGRP,JYR) = 0.0
       END DO
!
       DO IST = 1 , CO2_NST(IGRP)
            CO2_ST(IST,IGRP) = ST_NAMEC(IST+1)
!     print *,'!co2st',igrp,ist,co2_st(ist,igrp)
         DO ICO2YR = 1 , T_CO2YRS
            SYR = ST_CAPSC(1,ICO2YR) - UHBSYR
            IF (ICO2YR .LT. T_CO2YRS) THEN
               EYR = ST_CAPSC(1,ICO2YR+1) - UHBSYR - 1
            ELSE
               EYR = MNUMYR
            END IF
            DO JYR = SYR , EYR
               CO2_LIM = ST_CAPSC(IST+1,ICO2YR)
!              CONVERT TO MM METRIC TONS CARBON IF NEEDED
             IF (CO2_LIM .GT. 0.0 .AND. CO2_LIM .LT. 9999.0)THEN
               IF (CO2_TYP .EQ. 2)CO2_LIM = CO2_LIM * (12.0 / 44.0)
               IF (CO2_UNT .EQ. 2)CO2_LIM = CO2_LIM * (2000.0 / 2204.0)
             END IF
               CO2BYST(IST,IGRP,JYR) = CO2_LIM
               CO2BYGRP(IGRP,JYR) = CO2BYGRP(IGRP,JYR) + CO2_LIM
            END DO
         END DO
       END DO
       DO JYR = 1 , MNUMYR
         IF (CO2BYGRP(IGRP,JYR) .LT. 9999.0) THEN
            EMRFCA(IGRP,JYR) = CO2BYGRP(IGRP,JYR)
!           if (igrp .eq. 1 .and. jyr .gt. 25)write(6,3232) jyr + 1989,emrfca(igrp,jyr)
!3232 format(1h ,'!rggi',i4,f10.3)
         ELSE
            EMRFCA(IGRP,JYR) = 0.0
         END IF
       END DO
!
!     READ IN CO2 PRICE FLOOR, IF ANY.  CA (AB32) USES EPMDATA INSTEAD
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2FLOOR YR%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(CO2FLPR,UISTYR,MNUMYR)    ! CO2 PRICE FLOOR
      RET_CODE = RD$R1(CO2RSQY,UISTYR,MNUMYR)    ! CO2 RESERVE QUANTITY
      RET_CODE = RD$R1(CO2RSPR,UISTYR,MNUMYR)    ! CO2 RESERVE PRICE
      RET_CODE = RD$R1(CO2OFQY,UISTYR,MNUMYR)    ! CO2 OFFSET  QUANTITY
      RET_CODE = RD$R1(CO2OFPR,UISTYR,MNUMYR)    ! CO2 OFFSET  PRICE
      RET_CODE = RD$R1(CO2ESPR,UISTYR,MNUMYR)    ! CO2 ESCAPE OR MAX PRICE
      IF (IGRP .EQ. CARGRP_RG)THEN
         RET_CODE = RD$R1(CO2ECQY,UISTYR,MNUMYR)    ! CO2 EMISSION CONTAINMENT RESERVE QUANTITY
         RET_CODE = RD$R1(CO2ECPR,UISTYR,MNUMYR)    ! CO2 EMISSION CONTAINMENT RESERVE PRICE
         RET_CODE = RD$R1(CO2BKQY,UISTYR,MNUMYR)    ! CO2 BANK ADJUSTMENT QUANTITY
         RET_CODE = RD$R1(CO2NJEX,UISTYR,MNUMYR)    ! CO2 EMISSIONS EXCLUDED WHEN NJ ISN'T IN RGGI (FRACTION OF NJ/NY)
         RET_CODE = RD$R1(CO2VAEX,UISTYR,MNUMYR)    ! CO2 EMISSIONS EXCLUDED WHEN VA ISN'T IN RGGI (1.0 : VA = fuel region)
      END IF
       DO JYR = 1 , MNUMYR
         IF ((JYR + UHBSYR) .LE. 2016)THEN
            CO2FLGRP(IGRP,JYR) = CO2FLPR(JYR) / MC_JPGDP(2016 - UHBSYR)
         ELSE IF ((JYR + UHBSYR) .LE. 2030)THEN
            CO2FLGRP(IGRP,JYR) = CO2FLPR(JYR) / MC_JPGDP(JYR)
         ELSE
            CO2FLGRP(IGRP,JYR) = CO2FLPR(JYR) / MC_JPGDP(2030 - UHBSYR)
         END IF
         CO2RQGRP(IGRP,JYR) = CO2RSQY(JYR)
         IF ((JYR + UHBSYR) .LE. 2030)THEN
            CO2RPGRP(IGRP,JYR) = CO2RSPR(JYR) / MC_JPGDP(JYR)
         ELSE
            CO2RPGRP(IGRP,JYR) = CO2RSPR(JYR) / MC_JPGDP(2030 - UHBSYR)
         END IF
         CO2OQGRP(IGRP,JYR) = CO2OFQY(JYR)
!        CO2OPGRP(IGRP,JYR) = CO2OFPR(JYR) / SCALPR
         CO2OPGRP(IGRP,JYR) = CO2FLGRP(IGRP,JYR)
         CO2ESGRP(IGRP,JYR) = CO2ESPR(JYR) / SCALPR
         IF (IGRP .EQ. CARGRP_RG)THEN
            CO2EQGRP(IGRP,JYR) = CO2ECQY(JYR)
            IF ((JYR + UHBSYR) .LE. 2030)THEN
               CO2EPGRP(IGRP,JYR) = CO2ECPR(JYR) / MC_JPGDP(JYR)
            ELSE
               CO2EPGRP(IGRP,JYR) = CO2ECPR(JYR) / MC_JPGDP(2030 - UHBSYR)
            END IF
            CO2BQGRP(IGRP,JYR) = CO2BKQY(JYR)
            CO2NJEXC(IGRP,JYR) = CO2NJEX(JYR)
            CO2VAEXC(IGRP,JYR) = CO2VAEX(JYR)
         END IF
!        CONVERT TO MM METRIC TONS CARBON IF NEEDED
         IF (CO2FLPR(JYR) .GT. 0.0)THEN
            IF (CO2_TYP .EQ. 2)CO2FLGRP(IGRP,JYR) = CO2FLGRP(IGRP,JYR) * (44.0 / 12.0)
            IF (CO2_UNT .EQ. 2)CO2FLGRP(IGRP,JYR) = CO2FLGRP(IGRP,JYR) * (2204.0 / 2000.0)
         END IF
         IF (CO2RSQY(JYR) .GT. 0.0)THEN
            IF (CO2_TYP .EQ. 2)CO2RQGRP(IGRP,JYR) = CO2RQGRP(IGRP,JYR) * (12.0 / 44.0)
            IF (CO2_UNT .EQ. 2)CO2RQGRP(IGRP,JYR) = CO2RQGRP(IGRP,JYR) * (2000.0 / 2204.0)
         END IF
         IF (CO2RSPR(JYR) .GT. 0.0)THEN
            IF (CO2_TYP .EQ. 2)CO2RPGRP(IGRP,JYR) = CO2RPGRP(IGRP,JYR) * (44.0 / 12.0)
            IF (CO2_UNT .EQ. 2)CO2RPGRP(IGRP,JYR) = CO2RPGRP(IGRP,JYR) * (2204.0 / 2000.0)
         END IF
!        RGGI OFFSETS ARE A PCT OF LIMIT
         IF (IGRP .EQ. CARGRP_RG .AND. CO2OFQY(JYR) .GT. 0.0)THEN
            CO2OQGRP(IGRP,JYR) = CO2OFQY(JYR) * CO2BYGRP(IGRP,JYR)
         END IF
         IF (CO2OFPR(JYR) .GT. 0.0)THEN
            IF (CO2_TYP .EQ. 2)CO2OPGRP(IGRP,JYR) = CO2OPGRP(IGRP,JYR) * (44.0 / 12.0)
            IF (CO2_UNT .EQ. 2)CO2OPGRP(IGRP,JYR) = CO2OPGRP(IGRP,JYR) * (2204.0 / 2000.0)
         END IF
         IF (IGRP .EQ. CARGRP_RG)THEN
            IF (CO2ECQY(JYR) .GT. 0.0)THEN
               IF (CO2_TYP .EQ. 2)CO2EQGRP(IGRP,JYR) = CO2EQGRP(IGRP,JYR) * (12.0 / 44.0)
               IF (CO2_UNT .EQ. 2)CO2EQGRP(IGRP,JYR) = CO2EQGRP(IGRP,JYR) * (2000.0 / 2204.0)
            END IF
            IF (CO2ECPR(JYR) .GT. 0.0)THEN
               IF (CO2_TYP .EQ. 2)CO2EPGRP(IGRP,JYR) = CO2EPGRP(IGRP,JYR) * (44.0 / 12.0)
               IF (CO2_UNT .EQ. 2)CO2EPGRP(IGRP,JYR) = CO2EPGRP(IGRP,JYR) * (2204.0 / 2000.0)
            END IF
            IF (CO2BKQY(JYR) .GT. 0.0)THEN
               IF (CO2_TYP .EQ. 2)CO2BQGRP(IGRP,JYR) = CO2BQGRP(IGRP,JYR) * (12.0 / 44.0)
               IF (CO2_UNT .EQ. 2)CO2BQGRP(IGRP,JYR) = CO2BQGRP(IGRP,JYR) * (2000.0 / 2204.0)
            END IF
         END IF
!        STORE RGGI INPUTS INTO ARRAYS FOR FTAB
         IF (IGRP .EQ. CARGRP_RG)THEN
!           SUBTRACT BANK ADJUSTMENT FOR RGGI AND ADD NJ CAP FOR APPROPRIATE YEARS
!     write(6,4433) curiyr+1989,jyr+1989,co2bygrp(igrp,jyr),co2bqgrp(igrp,jyr),  &
!                                co2njexc(igrp,jyr),  co2vaexc(igrp,jyr),&
!                                CO2BYGRP(IGRP,JYR) - CO2BQGRP(IGRP,JYR)
!4433 format(1h,'!rgginj',i4,i5,6f10.3)
            EMRFCA(IGRP,JYR) = EMRFCA(IGRP,JYR) - CO2BQGRP(IGRP,JYR) 
            CO2BYGRP(IGRP,JYR) = CO2BYGRP(IGRP,JYR) - CO2BQGRP(IGRP,JYR) 
            CO2OQGRP(IGRP,JYR) = CO2OQGRP(IGRP,JYR) - CO2BQGRP(IGRP,JYR)  * CO2OFQY(JYR)
            RG_CAP_TOT(JYR) = CO2BYGRP(IGRP,JYR)
            RG_OFFSETS_AVL(JYR) = CO2OQGRP(IGRP,JYR)
            RG_RESERVE_AVL(JYR) = CO2RQGRP(IGRP,JYR)
            RG_RSRVECR_AVL(JYR) = CO2EQGRP(IGRP,JYR)
            RG_AUCTION_P(JYR) = CO2FLGRP(IGRP,JYR)
            RG_RESERVE_P(JYR) = CO2RPGRP(IGRP,JYR)
            RG_RSRVECR_P(JYR) = CO2EPGRP(IGRP,JYR)
!     write(6,3222) jyr + 1989,co2flgrp(igrp,jyr),co2rpgrp(igrp,jyr),co2opgrp(igrp,jyr),co2epgrp(igrp,jyr)
!3222 format(1h ,'!rggiprc',i4,5f10.2)
!     write(6,3223) jyr + 1989,co2bygrp(igrp,jyr)*44./12.,co2rqgrp(igrp,jyr)*44./12.,co2oqgrp(igrp,jyr)*44./12.,co2eqgrp(igrp,jyr)*44./12.
!3223 format(1h ,'!rggiqty',i4,5f10.2)
         END IF
       END DO
!
      END DO
!
!     READ IN MAP OF FUEL REGION to CO2 Region Shares for Coal and Gas/Oil
!
      RET_CODE = RD$TBL(I_CNTL,'%FUEL RGN CO2_CL',MAXNFR - 1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_CL_BY_FL,1,1,3,MAXNFR,CO2_GRP)  ! Map Census, Coal, Gas, and Carbon Regions to Fuel Regions
!     print *,'!co2cl',co2_cl_by_fl(2,1),co2_cl_by_fl(2,4)
!
      RET_CODE = RD$TBL(I_CNTL,'%FUEL RGN CO2_OG',MAXNFR - 1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(CO2_OG_BY_FL,1,1,3,MAXNFR,CO2_GRP)  ! Map Census, Coal, Gas, and Carbon Regions to Fuel Regions
!     print *,'!co2og',co2_og_by_fl(2,1),co2_og_by_fl(2,4)
!!
!     READ IN Carbon Allowance Allocation Specifications, If Any (i.e., McCain/Lieberman)
!
      RET_CODE = RD$TBL(I_CNTL,'%CO2_EMISRT%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(CO2_IM_BY_CA,UISTYR,MNUMYR)   ! Annual Carbon Emission Rate for Imports (International)
      RET_CODE = RD$R1(CO2_DF_BY_CA,UISTYR,MNUMYR)   ! Annual Carbon Emission Rate for Firm Imports (Domestic)
      RET_CODE = RD$R1(CO2_DE_BY_CA,UISTYR,MNUMYR)   ! Annual Carbon Emission Rate for Economy Imports (Domestic)
      RET_CODE = RD$R1(CO2_OS_BY_CA,UISTYR,MNUMYR)   ! Annual Carbon Emission Rate for Out-Of-State Generation
!
!     write(6,1234) co2_im_by_ca(21),co2_im_by_ca(51),  &
!                   co2_df_by_ca(21),co2_df_by_ca(51),  &
!                   co2_de_by_ca(21),co2_de_by_ca(51),  &
!                   co2_os_by_ca(21),co2_os_by_ca(51)
!1234 format(1h ,'!co2im',8f10.4)
!
!     EFD TO ECP FUEL CODE MAP
!
      RET_CODE = RD$TBL(I_CNTL,'%EFD-ECP FU%',UNFUELS,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I2(UFL_D_ECP,1,1,ECP_D_FPP,EFD_D_NFL,ECP_D_FPP)  !EFP-ECP FC
      RET_CODE = RD$I1(UIGAS,1,UNFUELS)
      RET_CODE = RD$I1(UIRES,1,UNFUELS)
      RET_CODE = RD$I1(UIDIS,1,UNFUELS)
      RET_CODE = RD$I1(UICOL,1,UNFUELS)
      RET_CODE = RD$I1(UINUC,1,UNFUELS)
      RET_CODE = RD$I1(EFLFTABI,1,EFD_D_NFL)                  ! map to ftab table 8 category

!     do IFP = 1, EFD_D_NFL
!     write(6,*) 'read efd fuels ',UIGAS(IFP),UIReS(IFP),UIDIS(IFP)
!     enddo
!
!     READ IN EFD RPS ROW SWITCH 0 = OFF, 1 = ON
!
      RET_CODE = RD$TBL(I_CNTL,'%EFD RPSROW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_EFDRPS,1,1)                                  !
!
!     READ IN IFFS/CAP.EXP. MODEL (UCAPE) EXECUTION SWITCH 1 = ON, 0 = OFF
!
      RET_CODE = RD$TBL(I_CNTL,'%UCAPE CAP?%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_UCAPE,1,1)                                  !
!
!     READ IN ECP COAL GROUPING SWITCH 0 = GROUP, 1 = DON'T GROUP
!
      RET_CODE = RD$TBL(I_CNTL,'%ECP CL GRP%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_CLGRP,1,1)                                  !
!
!     READ IN SWITCH TO IMPOSE EMM CARBON CONSTRAINT - 0 =  NO, 1 = ECP, 2 = EFD, 3 = BOTH ECP/EFD
!     YEAR TO IMPOSE CARBON CONSTRAINT - 0 = NONE
!     READ IN SWITCH TO PASS EMM CARBON PRICE    - 0 =  NO, 1 = ECP, 2 = EFD
!
      RET_CODE = RD$TBL(I_CNTL,'%CARB CNST?%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_CAR,1,1)                                  !
      RET_CODE = RD$I1(UYR_CAR,1,1)                                  !
      RET_CODE = RD$I1(USW_CAREMM,1,1)                               !
!     print *,'!car',usw_car,uyr_car,usw_caremm
!
!     READ IN SWITCH AND RATE FOR COAL CARBON SUBSIDY
!
      RET_CODE = RD$TBL(I_CNTL,'%CARB CLSUB%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_CARCL,1,1)                                  !
      RET_CODE = RD$R1(URCCLSUB,1,1)
!
!   FOR SENSITIVITY, OVERWRITE SUBSIDY RATE WITH RATIO OF NG/CL CARBON CONTENTS
!
      IF (USW_CARCL .EQ. 2) THEN
!
!   GET AVERAGE NG CARBON CONTENT
!
         NGCARAV = 14.4
!
!   GET AVERAGE CL CARBON CONTENT (VARIES BY REGION AND TYPE)
!
         CLCARAV = 0.0
         DO ICL = 1 , NCLUT1
         DO IRG = 1 , NDRG2
               CLCARAV = CLCARAV + CCLCLNR(IRG,CURIYR,ICL)
            END DO
         END DO
            CLCARAV = CLCARAV / FLOAT(NCLUT1 * NDRG2)
            URCCLSUB = NGCARAV / CLCARAV
      END IF
!
!     READ IN START YR AND VALUE OF WAITING ADJ. PRIOR TO CARBON STABILIZATION
!
      RET_CODE = RD$TBL(I_CNTL,'%CARB VALWAIT%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UYR_CARWS,1,1)                                  !
      RET_CODE = RD$R1(URCARWT,1,1)
!
!     READ IN SWITCH TO USE ECP COFIRING SHARES IN EFD
!
      RET_CODE = RD$TBL(I_CNTL,'%COFIRING SW?%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_ECPCF,1,1)                                  !
!
!     READ IN CAPACITY EXPANSION SWITCH (0 = ON, 1 = OFF) AND
!     LENGTH OF EXPLICIT AND FULL PLANNING HORIZON
!     INTEGER*4 USW_ECP            ! RUN ECP?
!     INTEGER*4 UNXPH              ! LENGTH OF EXPLICIT PLANNING
!     INTEGER*4 UNFPH              ! LENGTH OF FULL PLANNING HORI
!     INTEGER*4 UNAPH              ! NUMBER OF YRS TO USE ANN ADJ FAC IN LAST ECP YR
!     REAL*4    UPTCRT             ! TARGET CVG RATIO FOR PUR. CO
!     INTEGER*4 USW_DIGIT          ! SWITCH TO USE DIGITS2 FUNCTION TO LIMIT SIGNIFICANT DIGITS (0=No, 1=Yes)
!
      RET_CODE = RD$TBL(I_CNTL,'%ECP PARAMS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_ECP,1,1)                                    !
      RET_CODE = RD$I1(UNXPH,1,1)                                      !
      RET_CODE = RD$I1(UNFPH,1,1)                                      !
      RET_CODE = RD$I1(UNAPH,1,1)                                      !
      RET_CODE = RD$R1(UPTCRT,1,1)                                     !
      RET_CODE = RD$I1(USW_DIGIT,1,1)                                  !
      UNAPH = UNXPH + UNAPH - 1
      UNAPH = MAX(UNAPH,UNXPH)
      UNAPH = MIN(UNAPH,UNFPH)
!     ASSIGN VARIABLES SO CMM CAN RUN STANDALONE
      TNXPH = UNXPH
      TNFPH = UNFPH
!
!     MPS PARAMETERS
!     !  1 - DO PRT MP
      RET_CODE = RD$TBL(I_CNTL,'%MPS PARAMS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(ECP_D_PRNT,1,1)      ! 0-DON'T PRINT MP, 1-PRINT MP
      RET_CODE = RD$I1(ECP_D_MODE,1,1)   ! 0-REPLACE MATRX, 1-REVISE MATRX
      RET_CODE = RD$C1(ECP_D_FILE,1,1)             ! MPS FORMAT INPUT FILE
      RET_CODE = RD$C1(ECP_D_FILEB,1,1)             ! EMM BASIS INPUT FILE
!
!     OML CONTROL PARAMETERS
!
      RET_CODE = RD$TBL(I_CNTL,'%OML PARAMS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(UPOBJ,1,1)              ! OBJECTIVE FUNCTION NAME
      RET_CODE = RD$C1(UPRHS,1,1)                 ! RIGHT-HAND SIDE NAME
      RET_CODE = RD$C1(UPBND,1,1)                       ! BOUND ROW NAME
      RET_CODE = RD$C1(ECP_D_DBNM,1,1)                    ! DATA BASE NAME
      RET_CODE = RD$C1(ECP_D_PROB,1,1)                      ! PROBLEM NAME
!
      RET_CODE = RD$TBL(I_CNTL,'%DECK,BASIS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(ECP_D_DECK,1,1)                         ! DECK NAME
      RET_CODE = RD$C1(BASIS,1,1)                 ! BASIS FILE DECK NAME
!
      DO 200 IYR = 1 , UNYEAR
         WRITE(ECP_D_DECKB(IYR),1140) BASIS,IYR
 1140    FORMAT(A6,I2)
  200 CONTINUE
!
!     OML PARAMETERS FOR EFD
!
      RET_CODE = RD$TBL(I_CNTL,'%OML EFD PMS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(EFDOBJ,1,1)              ! OBJECTIVE FUNCTION NAME
      RET_CODE = RD$C1(EFDRHS,1,1)                 ! RIGHT-HAND SIDE NAME
      RET_CODE = RD$C1(EFDBND,1,1)                       ! BOUND ROW NAME
      RET_CODE = RD$C1(EFD$DBNM,1,1)                    ! DATA BASE NAME
      RET_CODE = RD$C1(EFD$PROB,1,1)                      ! PROBLEM NAME
!
!     DERATES, DSM
!
      RET_CODE = RD$TBL(I_CNTL,'%DERATE,DSM%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(ECP_D_DRTE,1,1)      ! REVISE DERATE SW: 0=NO,1=YES
      RET_CODE = RD$I1(ECP_D_IDSM,1,1)        ! INCLUDE DSM SW: 0=NO,1=YES
!
!     MARKET SHARING PARAMETERS
!
      RET_CODE = RD$TBL(I_CNTL,'%USE MS/LRN%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(ECP_D_MSHR,1,1)       ! MARKET SHARE SW: 0=NO,1=YES
      RET_CODE = RD$R81(EPMSTOL,1,1)            ! MARKET SHARE TOLERANCE
      RET_CODE = RD$R81(EPMSEXP,1,1)             ! MARKET SHARE EXPONENT
      RET_CODE = RD$I1(ECP_D_LFCC,1,1)        ! LRN SW CAPCOST: 0=NO,1=YES
      RET_CODE = RD$I1(ECP_D_LFHR,1,1)      ! LRN SW HEATRATES: 0=NO,1=YES
!
!     SWITCH FOR USING RISK PREMIUMS
!
      RET_CODE = RD$TBL(I_CNTL,'%USE RSK PREM%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(ECP_D_RSK,1,1)        ! RISK PREMIUM SW: 0=NO,1=YES
!
!     ANNUAL SWITCH FOR PACKING ECP SOLUTION MATRIX
!
      RET_CODE = RD$TBL(I_CNTL,'%ECPPACK YR%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$I1(ECP_D_PACK,UISTYR,MNUMYR)    ! PACK ECP SOLUTION : 0=NO,1=YES
!
!     VARIABLES FOR GAS SUPPLY CURVE
!
      RET_CODE = RD$TBL(I_CNTL,'%GAS SUPPLY CURVE%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UPCRVSW,1,1)         ! SUP CRV SWITCH FOR NG GROWTH RATE
      RET_CODE = RD$I1(UPCRVSTP,1,1)        ! NUMBER OF SUP CRV STEPS ABV/BLW MDPT
      RET_CODE = RD$R1(UPCRVSIZ,1,1)        ! SIZE (FRAC) OF SUP CRV STEPS
      RET_CODE = RD$R1(UPCRVELA,1,1)        ! SUP CRV ELASTICITY (1.00 = 100%)
!
!     VARIABLES FOR GROWTH (DECLINE) IN COAL PRICE EXPECTATIONS
!
      RET_CODE = RD$TBL(I_CNTL,'%COAL EXPECTATIONS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UPXCLGRW,1,1)        ! FRAC INCR/DECR IN COAL EXP
      RET_CODE = RD$R1(UPXCLFLR,1,1)        ! MIN FRAC OF CUR PRC
!
!     SWITCH FOR USING ELEVATION MULTIPLIERS
!
      RET_CODE = RD$TBL(I_CNTL,'%USE AMB MULT%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(ECP_D_AMB,1,1)        ! AMB COND  MULT. SW: 0=NO,1=YES
!
!     CALCULATE ANNUAL GNP DEFLATOR FROM FIXED INFLATION RATE
!
      RET_CODE = RD$TBL(I_CNTL,'%INFLAT RAT%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R81(GNP$GRW,1,1)       ! ANNUAL GNP DEFLATION FACTOR
!
!     REVISE GNP DEFLATORS
!
      DO IYR = 1 , UNYEAR
         UPGNPD(IYR) = MC_JPGDP(IYR)
         WRITE(18,*) 'YR,GNP DEFLATOR ',IYR,UPGNPD(IYR)
      END DO
      GRW = DBLE(MC_JPGDP(UNYEAR) / MC_JPGDP(1)) ** (DBLE(1.0) / DBLE(UNYEAR - 1.0))
      DO IYR = UNYEAR + 1, UNYEAR + UNFPH
         UPGNPD(IYR) = UPGNPD(IYR - 1) * GRW
      END DO
!
!     UPGNPD(1) = 1.0
!     GNP$CUM = DBLE(1.0)
!     DO IYR = 2 , UNYEAR + ECP_D_FPH
!        GNP$CUM = GNP$CUM * (DBLE(1.0) + GNP$GRW)
!        UPGNPD(IYR) = GNP$CUM
!     ENDDO
!     IF (UF_DBG .GT. 0) THEN
!        DO IYR = 1,UNYEAR + UNXPH
!           WRITE(UF_DBG,3302)'IYR',IYR,'GNP',UPGNPD(IYR)
!3302       FORMAT(A4,1X,I4,1X,A4,1X,F8.3,1X)
!        ENDDO
!     END IF
!
!     TAX DEPRECIATION RATES
!
      RET_CODE = RD$TBL(I_CNTL,'%TAX DEP RT%',26,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R82(TAXDEPR,1,1,6,26,6)     ! TAX DEPRECIATION RATES
!
!     READ IN NEW YEAR 1 BONUS DEPRECIATION UNDER NEW TAX LAW
!
      RET_CODE = RD$TBL(I_CNTL,'%TAXDEPR Y1%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(TAXDEPR1,UISTYR,MNUMYR)    ! YEAR 1 BONUS DEPRECIATION FROM 2017 TAX LAW
      RET_CODE = RD$R1(TAXFCFTT,UISTYR,MNUMYR)    ! TRANSMISSION FCF REDUCTION FROM 2017 TAX LAW TAX RATE
      RET_CODE = RD$R1(TAXFCFTD,UISTYR,MNUMYR)    ! TRANSMISSION FCF REDUCTION FROM 2017 TAX LAW DEPRECIATION CHG
      DO IYR = UISTYR , MNUMYR
         IF (TAXDEPR1(IYR) .LE. 0.0)THEN
            DO ICAP = 1 , 6
               DO XYR = 1 , 26
                  TAXDEPRN(XYR,ICAP,IYR) = TAXDEPR(XYR,ICAP)
               END DO
            END DO
         ELSE
            DO ICAP = 1 , 6
               IF (TAXDEPR(1,ICAP) .LT. TAXDEPR1(IYR))THEN
                  TAXDEPRR = 0.0
                  DO XYR = 2 , 26
                     TAXDEPRR = TAXDEPRR + TAXDEPR(XYR,ICAP)
                  END DO
                     TAXDEPRN(1,ICAP,IYR) = TAXDEPR1(IYR)
                  DO XYR = 2 , 26
                     TAXDEPRN(XYR,ICAP,IYR) = (1.0 - TAXDEPR1(IYR)) * TAXDEPR(XYR,ICAP) / TAXDEPRR
                  END DO
               ELSE
                  DO XYR = 1 , 26
                     TAXDEPRN(XYR,ICAP,IYR) = TAXDEPR(XYR,ICAP)
                  END DO
               END IF
            END DO
         END IF
      END DO

!     READ IN ECP TECHNOLOGY CODES AND MAPPING VARIABLES

      RET_CODE = RD$TBL(I_CNTL,'%ECP TECH  %',ECP_D_CAP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(UPLNTCD,1,ECP_D_CAP)      ! Plant Codes
      RET_CODE = RD$C1(UPLNAME,1,ECP_D_CAP)      ! Long Plant Names
      RET_CODE = RD$I1(UPLTISW,1,ECP_D_CAP)      ! Switch: Is Tech used in run?
      RET_CODE = RD$I1(UCPDSPIS,1,ECP_D_CAP)     ! Switch: Is Tech DSP Tech?
      RET_CODE = RD$I1(UCPINTIS,1,ECP_D_CAP)     ! Switch: Is Tech INT Tech?
      RET_CODE = RD$I1(UCPRNWIS,1,ECP_D_CAP)     ! Switch: Is Tech RNW Tech?
      RET_CODE = RD$I1(UCPDGNIS,1,ECP_D_CAP)     ! Switch: Is Tech DGN Tech?
      RET_CODE = RD$I1(UCPSTOIS,1,ECP_D_CAP)     ! Switch: Is Tech STO Tech?
      RET_CODE = RD$I1(UCPFTABI,1,ECP_D_CAP)     ! Map to FTAB Table 9 Category.
      RET_CODE = RD$I1(UCPVSTOR,1,ECP_D_CAP)     ! Map to Value of Storage Category.
      RET_CODE = RD$R1(UECP_HTRT_ADJ,1,ECP_D_CAP) ! Adjustment Factor to Convert to Unscrubbed - UnCarbon Capture Heatrate
      RET_CODE = RD$R1(UECP_CPEN_ADJ,1,ECP_D_CAP) ! Adjustment Factor to Convert to UnCarbon Capture O&M Rates
      RET_CODE = RD$R1(ALT_UECP_CPEN_ADJ,1,ECP_D_CAP) ! Minimum Adjustment Factor to Convert to UnCarbon Capture O&M Rates
      RET_CODE = RD$C1(NO_CCS_PLNTCD,1,ECP_D_CAP)      ! For units with carbon capture identify the associated plant codes for plant types without capture
!
      NUM_864_TYPES = 0
      DO ICAP = 1 , ECP_D_CAP

         DO JCAP = 1 , ECP_D_CAP
            IF (NO_CCS_PLNTCD(ICAP) .EQ. UPLNTCD(JCAP)) THEN
               NO_CCS_PLNT_NDX(ICAP) = JCAP
            END IF
         END DO

         NUM_864_TYPES = MAX(NUM_864_TYPES, UCPVSTOR(icap))
         Write(18,3303) icap, uplntcd(icap), uplname(icap), ucpdspis(icap), ucpintis(icap), ucprnwis(icap), ucpdgnis(icap), ucpstois(icap), upvtyp(icap), UCPFTABI(icap), UCPVSTOR(icap), UECP_HTRT_ADJ(icap), ALT_UECP_CPEN_ADJ(icap), NO_CCS_PLNTCD(ICAP), NO_CCS_PLNT_NDX(ICAP)
 3303    format(1x,"ECP_TECH",",",I3,",",A2,",",A40,8(",",I2),2(",",F6.3),",",A2,",",I3)
      END DO

!     READ IN NUMBER OF Storage Groups
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM OF GRP%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NumStoGrp,1,1)                                     !

!     READ IN NUMBER OF BINS for each Storage Groups
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM OF BIN%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(MaxNumbins,1,1)                                     !

      RET_CODE = RD$TBL(I_CNTL,'%STO TECH  %',NUM_864_TYPES,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(StorageNames,1,ECP_D_CAP)      ! Plant Names for 864 Storage Model
      RET_CODE = RD$C1(StorageCodes,1,ECP_D_CAP)      ! Plant Codes for 864 Storage Model
      RET_CODE = RD$I1(STO_CAP_INDX,1,ECP_D_CAP)       ! Plant Index for 864 Storage Model
      RET_CODE = RD$I1(NUM_STEPS_PER_TYPE,1,ECP_D_CAP) ! Number of PRICE Bins per each Storage Model Plant Type
	  RET_CODE = RD$I1(CurtTypMap,1,ECP_D_CAP) ! Curtailment Index for restart file
	  RET_CODE = RD$I1(StoTypMap,1,ECP_D_CAP)  ! Storage index for restart file

      MaxPtypes = 1
      DO ICAP = 1 , ECP_D_CAP
         ISTO = UCPVSTOR(ICAP)
         MaxPtypes = MAX(MaxPtypes,ISTO)
         Write(18,4303) icap, ISTO, uplname(icap), StorageNames(ISTO), StorageCodes(ISTO), UCPVSTOR(icap), NUM_STEPS_PER_TYPE(ISTO), MaxPtypes
 4303    format(1x,"STO_TECH",2(":",I3),2(":",A40),":",A2,3(":",I3))
      END DO

!
!     ASSIGN VALUES TO TECH INDEX ARRAYS AND MAXIMUM LIMITS
!
      ECPCAPM = 0
      ECPDSPM = 0
      ECPINTM = 0
      ECPRNWM = 0
      ECPDGNM = 0
      ECPSTOM = 0
      ECPI_RM = 0
      DO ICAP = 1 , ECP_D_CAP
        UPLTSW(ICAP)  = .FALSE.
        UCPDSPS(ICAP) = .FALSE.
        UCPINTS(ICAP) = .FALSE.
        UCPRNWS(ICAP) = .FALSE.
        UCPDGNS(ICAP) = .FALSE.
        UCPSTOS(ICAP) = .FALSE.
        ECPCAPM = ECPCAPM + 1
        TECHSW = 0

        IF(ICAP.LT.ECP_D_DSP) UCPDSPI(ICAP) = 0
        IF(ICAP.LT.ECP_D_INT) THEN
          UCPINTI(ICAP) = 0
          UIRINTI(ICAP) = 0
        END IF
        IF(ICAP.LT.ECP_D_RNW) THEN
          UCPRNWI(ICAP) = 0
          UIRRNWI(ICAP) = 0
        END IF
        IF(ICAP.LT.ECP_D_DGN) UCPDGNI(ICAP) = 0
        IF(ICAP.LT.ECP_D_STO) UCPSTOI(ICAP) = 0

        IF(UCPDSPIS(ECPCAPM).EQ.1) THEN
          ECPDSPM = ECPDSPM + 1
          UCPDSPIS(ECPCAPM) = ECPDSPM
          UCPDSPI(ECPDSPM) = ECPCAPM
          UCPDSPS(ECPCAPM) = .TRUE.
          TECHSW = TECHSW + 1
          PTYPE(ECPCAPM) = 'D '
        END IF
        IF(UCPINTIS(ECPCAPM).EQ.1) THEN
          ECPINTM = ECPINTM + 1
          UCPINTIS(ECPCAPM) = ECPINTM
          ECPI_RM = ECPI_RM + 1
          UCPINTI(ECPINTM) = ECPCAPM
          UIRINTI(ECPINTM) = ECPI_RM
          UCPINTS(ECPCAPM) = .TRUE.
          UIRINTS(ECPI_RM) = .TRUE.
          TECHSW = TECHSW + 1
          PTYPE(ECPCAPM) = 'I '
        END IF
        IF(UCPRNWIS(ECPCAPM).EQ.1) THEN
          ECPRNWM = ECPRNWM + 1
          UCPRNWIS(ECPCAPM) = ECPRNWM
          ECPI_RM = ECPI_RM + 1
          UCPRNWI(ECPRNWM) = ECPCAPM
          UIRRNWI(ECPRNWM) = ECPI_RM
          UCPRNWS(ECPCAPM) = .TRUE.
          UIRRNWS(ECPI_RM) = .TRUE.
          TECHSW = TECHSW + 1
          PTYPE(ECPCAPM) = 'R '
        END IF
        IF(UCPSTOIS(ECPCAPM).EQ.1) THEN
          ECPSTOM = ECPSTOM + 1
          UCPSTOIS(ECPCAPM) = ECPSTOM
          ECPI_RM = ECPI_RM + 1
          UCPSTOI(ECPSTOM) = ECPCAPM
          UIRSTOI(ECPSTOM) = ECPI_RM
          UCPSTOS(ECPCAPM) = .TRUE.
          UIRSTOS(ECPI_RM) = .TRUE.
          TECHSW = TECHSW + 1
          PTYPE(ECPCAPM) = 'S '
        END IF
        IF(UCPDGNIS(ECPCAPM).EQ.1) THEN
          ECPDGNM = ECPDGNM + 1
          UCPDGNIS(ECPCAPM) = ECPDGNM
          UCPDGNI(ECPDGNM) = ECPCAPM
          UCPDGNS(ECPCAPM) = .TRUE.
          TECHSW = TECHSW + 1
          PTYPE(ECPCAPM) = 'G '
        END IF

        IF(TECHSW.LT.1) THEN
          WRITE(6,*) '*** RDCNTRL ERROR: ECP TECHNOLOGY ', &
           UPLNTCD(ECPCAPM),' NOT ASSIGNED TO GROUP ***'
        END IF
!
!       Assign Index to all ECP technologies
!
        IF (UPLNTCD(ICAP) .EQ. "B1") WIB1 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "B2") WIB2 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "B3") WIB3 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "B4") WIB4 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "B5") WIB5 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "B6") WIB6 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "B7") WIB7 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "B8") WIB8 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "C1") WIC1 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "C2") WIC2 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "C3") WIC3 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "C4") WIC4 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "C5") WIC5 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "C6") WIC6 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "C7") WIC7 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "C8") WIC8 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "C9") WIC9 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "CX") WICX = ICAP
        IF (UPLNTCD(ICAP) .EQ. "CY") WICY = ICAP
        IF (UPLNTCD(ICAP) .EQ. "CZ") WICZ = ICAP
        IF (UPLNTCD(ICAP) .EQ. "H1") WIH1 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "H2") WIH2 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "H3") WIH3 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "H4") WIH4 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "H5") WIH5 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "H6") WIH6 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "H7") WIH7 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "H8") WIH8 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "H9") WIH9 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "HA") WIHA = ICAP
        IF (UPLNTCD(ICAP) .EQ. "HB") WIHB = ICAP
        IF (UPLNTCD(ICAP) .EQ. "HC") WIHC = ICAP
        IF (UPLNTCD(ICAP) .EQ. "PC") WIPC = ICAP
        IF (UPLNTCD(ICAP) .EQ. "OC") WIOC = ICAP
        IF (UPLNTCD(ICAP) .EQ. "IG") WIIG = ICAP
        IF (UPLNTCD(ICAP) .EQ. "I2") WII2 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "PQ") WIPQ = ICAP
        IF (UPLNTCD(ICAP) .EQ. "IS") WIIS = ICAP
        IF (UPLNTCD(ICAP) .EQ. "NG") WING = ICAP
        IF (UPLNTCD(ICAP) .EQ. "ST") WIST = ICAP
        IF (UPLNTCD(ICAP) .EQ. "ET") WIET = ICAP
        IF (UPLNTCD(ICAP) .EQ. "IC") WIIC = ICAP
        IF (UPLNTCD(ICAP) .EQ. "CT") WICT = ICAP
        IF (UPLNTCD(ICAP) .EQ. "T2") WIT2 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "AT") WIAT = ICAP
        IF (UPLNTCD(ICAP) .EQ. "EC") WIEC = ICAP
        IF (UPLNTCD(ICAP) .EQ. "CC") WICC = ICAP
        IF (UPLNTCD(ICAP) .EQ. "AC") WIAC = ICAP
        IF (UPLNTCD(ICAP) .EQ. "A2") WIA2 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "CS") WICS = ICAP
        IF (UPLNTCD(ICAP) .EQ. "FC") WIFC = ICAP
        IF (UPLNTCD(ICAP) .EQ. "CN") WICN = ICAP
        IF (UPLNTCD(ICAP) .EQ. "AN") WIAN = ICAP
        IF (UPLNTCD(ICAP) .EQ. "SM") WISM = ICAP
        IF (UPLNTCD(ICAP) .EQ. "GN") WIGN = ICAP
        IF (UPLNTCD(ICAP) .EQ. "WD") WIWD = ICAP
        IF (UPLNTCD(ICAP) .EQ. "BI") WIBI = ICAP
        IF (UPLNTCD(ICAP) .EQ. "MS") WIMS = ICAP
        IF (UPLNTCD(ICAP) .EQ. "GT") WIGT = ICAP
        IF (UPLNTCD(ICAP) .EQ. "AG") WIAG = ICAP
        IF (UPLNTCD(ICAP) .EQ. "HY") WIHY = ICAP
        IF (UPLNTCD(ICAP) .EQ. "HI") WIHI = ICAP
        IF (UPLNTCD(ICAP) .EQ. "TI") WITI = ICAP
        IF (UPLNTCD(ICAP) .EQ. "PS") WIPS = ICAP
        IF (UPLNTCD(ICAP) .EQ. "P2") WIP2 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "SQ") WISQ = ICAP
        IF (UPLNTCD(ICAP) .EQ. "DS") WIDS = ICAP
        IF (UPLNTCD(ICAP) .EQ. "OS") WIOS = ICAP
        IF (UPLNTCD(ICAP) .EQ. "WN") WIWN = ICAP
        IF (UPLNTCD(ICAP) .EQ. "WL") WIWL = ICAP
        IF (UPLNTCD(ICAP) .EQ. "WF") WIWF = ICAP
        IF (UPLNTCD(ICAP) .EQ. "SO") WISO = ICAP
        IF (UPLNTCD(ICAP) .EQ. "SS") WISS = ICAP
        IF (UPLNTCD(ICAP) .EQ. "S2") WIS2 = ICAP
        IF (UPLNTCD(ICAP) .EQ. "PV") WIPV = ICAP
        IF (UPLNTCD(ICAP) .EQ. "IN") WIIN = ICAP
        IF (UPLNTCD(ICAP) .EQ. "PT") WIPT = ICAP
        IF (UPLNTCD(ICAP) .EQ. "DB") WIDB = ICAP
        IF (UPLNTCD(ICAP) .EQ. "DP") WIDP = ICAP

      END DO ! ICAP
!     ASSIGN COAL PLANT TYPE FOR CMM STANDALONE
      TECPPLT = WIIG

!C BTEMP

      WRITE(18,9091) (UCPDSPIS(ICAP),ICAP=1,ECP_D_CAP)
 9091 FORMAT(1X,'ICAP,UCPDSPIS = ',<ECP_D_CAP>(I2,1X))

      WRITE(18,9092) (UCPINTIS(ICAP),ICAP=1,ECP_D_CAP)
 9092 FORMAT(1X,'ICAP,UCPINTIS = ',<ECP_D_CAP>(I2,1X))

      WRITE(18,9093) (UCPRNWIS(ICAP),ICAP=1,ECP_D_CAP)
 9093 FORMAT(1X,'ICAP,UCPRNWIS = ',<ECP_D_CAP>(I2,1X))

      WRITE(18,8093) (UCPSTOIS(ICAP),ICAP=1,ECP_D_CAP)
 8093 FORMAT(1X,'ICAP,UCPSTOIS = ',<ECP_D_CAP>(I2,1X))

      WRITE(18,9094) (UCPDGNIS(ICAP),ICAP=1,ECP_D_CAP)
 9094 FORMAT(1X,'ICAP,UCPDGNIS = ',<ECP_D_CAP>(I2,1X))

      WRITE(18,9095) (UCPDSPI(ICAP),ICAP=1,ECP_D_DSP)
 9095 FORMAT(1X,'ICAP,UCPDSPI = ',<ECP_D_DSP>(I2,1X))

      WRITE(18,9096) (UCPINTI(ICAP),ICAP=1,ECP_D_INT)
 9096 FORMAT(1X,'ICAP,UCPINTI = ',<ECP_D_INT>(I2,1X))

      WRITE(18,9097) (UCPRNWI(ICAP),ICAP=1,ECP_D_RNW)
 9097 FORMAT(1X,'ICAP,UCPRNWI = ',<ECP_D_RNW>(I2,1X))

      WRITE(18,8097) (UCPSTOI(ICAP),ICAP=1,ECP_D_STO)
 8097 FORMAT(1X,'ICAP,UCPSTOI = ',<ECP_D_STO>(I2,1X))

      WRITE(18,9099) (UIRRNWI(ICAP),ICAP=1,ECP_D_RNW)
 9099 FORMAT(1X,'ICAP,UIRRNWI = ',<ECP_D_RNW>(I2,1X))

      WRITE(18,8098) (UIRSTOI(ICAP),ICAP=1,ECP_D_STO)
 8098 FORMAT(1X,'ICAP,UIRSTOI = ',<ECP_D_STO>(I2,1X))

      WRITE(18,9098) (UIRINTI(ICAP),ICAP=1,ECP_D_INT)
 9098 FORMAT(1X,'ICAP,UIRINTI = ',<ECP_D_INT>(I2,1X))

      WRITE(18,9100) (UCPDGNI(ICAP),ICAP=1,ECP_D_DGN)
 9100 FORMAT(1X,'ICAP,UCPDGNI = ',<ECP_D_DGN>(I2,1X))

      WRITE(18,9101) ECPCAPM,ECPDSPM,ECPDGNM,ECPSTOM,ECPINTM,ECPRNWM,ECPI_RM
 9101 FORMAT(1X,'ECP CAP,DSP,DGN,INT,RNW,I_R MAX = ',7(":",I3))

!C ETEMP
!
!     Read Coal Plant Retro Vintage Groups
!
      RET_CODE = RD$TBL(I_CNTL,'%CL RET  VN%',UNXPH,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UCL_VIN,1,ECP_D_XPH)                    ! Coal Plant Retrofit Vintage Groups
!
!     Read Simple ACI Injection Costs
!
      RET_CODE = RD$TBL(I_CNTL,'%ACI  COSTS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UCL_ACI_O,1,1)                    ! Simple Activated Carbon Overnight Cost
      RET_CODE = RD$R1(UCL_ACI_F,1,1)                    ! Simple Activated Carbon Fixed O&M Costs
!
!     Read Spray Cooling Costs
!
      RET_CODE = RD$TBL(I_CNTL,'%SC   COSTS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UCL_SC_O,1,1)                    ! Overnight Cost
      RET_CODE = RD$R1(UCL_SC_F,1,1)                    ! Fixed O&M Cost
      RET_CODE = RD$R81(UCL_CF1,1,1)                     ! CLOSS coefficient
      RET_CODE = RD$R81(UCL_CF2,1,1)                     ! CLOSS coefficient
      RET_CODE = RD$R81(UCL_CF3,1,1)                     ! CLOSS coefficient
      RET_CODE = RD$R81(UCL_CF4,1,1)                     ! CLOSS coefficient
      RET_CODE = RD$I1(NG_CCS_SYR,1,1)                   ! Read in First Year Existing Combined Cycle Units can add Carbon Capture

!     Write(6,4302)'CLOSS coeffs ',UCL_CF1, UCL_CF2, UCL_CF3, UCL_CF4
4302  FORMAT(A12,1x,4(F12.8,1x))
!
!     Read Fabric Filter Costs
!
      RET_CODE = RD$TBL(I_CNTL,'%FF   COSTS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UCL_FF_O,1,1)                    ! Overnight Cost
      RET_CODE = RD$R1(UCL_FF_F,1,1)                    ! Fixed O&M Cost
!
!     Read in Activated Carbon Supply Curve
!
      RET_CODE = RD$TBL(I_CNTL,'%NUM AC STP%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NUM_ACSS,1,1)                    ! NUMBER OF Activated Carbon Supply Steps
      N_AC_SC = NUM_ACSS
!
      RET_CODE = RD$TBL(I_CNTL,'%AC SUP STP%',NUM_ACSS,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UCL_PAC,1,MX_ACSS)                   ! Activated Carbon Prices
      RET_CODE = RD$R1(UCL_QAC,1,MX_ACSS)                   ! Activated Carbon Supplies
!
!     Store Supply Curve for Activated Carbon in Array Available to CDS
!
      DO I_ACI = 1 , NUM_ACSS
         P_AC_SC(I_ACI) = UCL_PAC(I_ACI)
         Q_AC_SC(I_ACI) = UCL_QAC(I_ACI)
      END DO
!
!     Read in List of Coal Plant Configurations
!
      RET_CODE = RD$TBL(I_CNTL,'%CL CNFG NM%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NUM_CNFG,1,1)                    ! NUMBER OF Coal Plant Configurations
!
      DO I_CNFG = 1 , NUM_CNFG
         DO I_RCMB = 1 , NUM_RCMB + 2
            UCL_CNFG(I_RCMB,I_CNFG) = 0
         END DO
      END DO
!
      RET_CODE = RD$TBL(I_CNTL,'%CL CONFIGS%',NUM_CNFG,UF_DBG,UF_MSG)
      RET_CODE = RD$I1(T_CNFG_NDX,1,MX_CNFG)                    ! Coal Plant Configuration Index
      RET_CODE = RD$C1(T_CNFG_MACT,1,MX_CNFG)                   ! Coal Plant Configuration MACT Control Category
      RET_CODE = RD$C1(T_CNFG_PART,1,MX_CNFG)                   ! Coal Plant Configuration Particulate Control
      RET_CODE = RD$C1(T_CNFG_SO2,1,MX_CNFG)                    ! Coal Plant Configuration SO2 Control
      RET_CODE = RD$C1(T_CNFG_COMB,1,MX_CNFG)                   ! Coal Plant Configuration NOX COMB Control
      RET_CODE = RD$C1(T_CNFG_SNCR,1,MX_CNFG)                   ! Coal Plant Configuration NOX SNCR Control
      RET_CODE = RD$C1(T_CNFG_SCR,1,MX_CNFG)                    ! Coal Plant Configuration NOX SCR Control
      RET_CODE = RD$C1(T_CNFG_SC,1,MX_CNFG)                     ! Coal Plant Configuration ACI Spray Cooler
      RET_CODE = RD$C1(T_CNFG_FF,1,MX_CNFG)                     ! Coal Plant Configuration ACI Fabric Filter
      RET_CODE = RD$I1(UCL_ECP,1,MX_CNFG)                       ! Coal Plant Configuration ECP Type
      RET_CODE = RD$I1(UCL_NSR_CFG,1,MX_CNFG)                   ! Coal Plant Configuration Acceptable NSR Config 0=>No, 1=>Yes

!  Initialize to "Not scrubbed"

      ECP_SCRUB   = 2
      C_ECP_PART  = 2
      C_ECP_COMB  = 2
      C_ECP_SNCR  = 2
      C_ECP_SCR   = 2
      C_ECP_CCS   = 2
      C_ECP_FF    = 2
!
!     Save Coal Configuration Components
!
      DO I_CNFG = 1 , NUM_CNFG
         IECP = UCL_ECP(I_CNFG)
!
!        MACT Control Category - Component Index 1 - Bit. 100% Sub.   0% Lig.   0% => 1
!                                                  - Bit.  75% Sub.  25% Lig.   0% => 2
!                                                  - Bit.  50% Sub.  50% Lig.   0% => 3
!                                                  - Bit.  25% Sub.  75% Lig.   0% => 4
!                                                  - Bit.   0% Sub. 100% Lig.   0% => 5
!                                                  - Bit.   0% Sub.  50% Lig.  50% => 6
!                                                  - Bit.   0% Sub.   0% Lig. 100% => 7
!
         IF (T_CNFG_MACT(I_CNFG) .EQ. "B100") THEN
            UCL_CNFG(1,I_CNFG) = 1
         ELSE IF (T_CNFG_MACT(I_CNFG) .EQ. "B075") THEN
            UCL_CNFG(1,I_CNFG) = 2
         ELSE IF (T_CNFG_MACT(I_CNFG) .EQ. "B050") THEN
            UCL_CNFG(1,I_CNFG) = 3
         ELSE IF (T_CNFG_MACT(I_CNFG) .EQ. "B025") THEN
            UCL_CNFG(1,I_CNFG) = 4
         ELSE IF (T_CNFG_MACT(I_CNFG) .EQ. "S100") THEN
            UCL_CNFG(1,I_CNFG) = 5
         ELSE IF (T_CNFG_MACT(I_CNFG) .EQ. "S050") THEN
            UCL_CNFG(1,I_CNFG) = 6
         ELSE IF (T_CNFG_MACT(I_CNFG) .EQ. "L100") THEN
            UCL_CNFG(1,I_CNFG) = 7
         END IF
!
!        Particulate Control - Component Index 2 - Bag House => 1, Cold Side ESP => 2, Hot Side ESP and Other => 3, New Unit => 4
!
         IF (T_CNFG_PART(I_CNFG) .EQ. " BH") THEN
            UCL_CNFG(2,I_CNFG) = 1
         ELSE IF (T_CNFG_PART(I_CNFG) .EQ. "CSE") THEN
            UCL_CNFG(2,I_CNFG) = 2
         ELSE IF (T_CNFG_PART(I_CNFG) .EQ. "HSE") THEN
            UCL_CNFG(2,I_CNFG) = 3
         ELSE IF (T_CNFG_PART(I_CNFG) .EQ. "NEW") THEN
            UCL_CNFG(2,I_CNFG) = 4
         END IF
         IF (UCL_CNFG(2,I_CNFG) .EQ. 1 .OR. UCL_CNFG(2,I_CNFG) .EQ. 4) THEN
            C_ECP_PART(IECP) = 1
         END IF
!
!        SO2 Emission Controls - Component Index 3 - Wet FGD => 1, Dry Scrubber => 2, None => 0
!
         IF (T_CNFG_SO2(I_CNFG) .EQ. "FGD") THEN
            UCL_CNFG(3,I_CNFG) = 1
         ELSE IF (T_CNFG_SO2(I_CNFG) .EQ. "Dry") THEN
            UCL_CNFG(3,I_CNFG) = 2
         END IF
!
         IF (UCL_CNFG(3,I_CNFG) .EQ. 0) THEN         !  set through end of projections
            ECP_SCRUB(IECP,:) = 2
         ELSE
            ECP_SCRUB(IECP,:) = 1
         END IF
!
!        NOX Combustion Controls - Component Index 4 - With Combustion Control => 1, None => 0
!
         IF (T_CNFG_COMB(I_CNFG) .EQ. "COMB") THEN
            UCL_CNFG(4,I_CNFG) = 1
            C_ECP_COMB(IECP) = 1
         END IF
!
!        NOX SNCR Controls - Component Index 5 - With SNCR => 1, None => 0
!
         IF (T_CNFG_SNCR(I_CNFG) .EQ. "SNCR") THEN
            UCL_CNFG(5,I_CNFG) = 1
            C_ECP_SNCR(IECP) = 1
         END IF
!
!        NOX SCR Controls - Component Index 6 - With SCR => 1, None => 0
!
         IF (T_CNFG_SCR(I_CNFG) .EQ. "SCR") THEN
            UCL_CNFG(6,I_CNFG) = 1
            C_ECP_SCR(IECP) = 1
         END IF
!
!        Mercury Spray Cooler - Component Index 7 - With Spray Cooler => 1, None => 0
!
         IF (T_CNFG_SC(I_CNFG) .EQ. "SC") THEN
            UCL_CNFG(7,I_CNFG) = 1
            C_ECP_CCS(IECP) = 1
         END IF
!
!        Mercury Supplemental Fabric Filter - Component Index 8 - With Fabric Filter => 1, None => 0
!
         IF (T_CNFG_FF(I_CNFG) .EQ. "FF") THEN
            UCL_CNFG(8,I_CNFG) = 1
            C_ECP_FF(IECP) = 1
         END IF
      END DO
!
!     New Coal Units are Scrubbed
!
      ECP_SCRUB(WIPC,:) = 1
      ECP_SCRUB(WIIG,:) = 1
      ECP_SCRUB(WIIS,:) = 1
      ECP_SCRUB(WIOC,:) = 1
      ECP_SCRUB(WII2,:) = 1
      ECP_SCRUB(WIPQ,:) = 1
      C_ECP_PART(WIPC) = 1
      C_ECP_PART(WIIG) = 1
      C_ECP_PART(WIIS) = 1
      C_ECP_PART(WIOC) = 1
      C_ECP_PART(WII2) = 1
      C_ECP_PART(WIPQ) = 1
      C_ECP_COMB(WIPC) = 1
      C_ECP_COMB(WIIG) = 1
      C_ECP_COMB(WIIS) = 1
      C_ECP_COMB(WIOC) = 1
      C_ECP_COMB(WII2) = 1
      C_ECP_COMB(WIPQ) = 1
      C_ECP_SCR(WIPC) = 1
      C_ECP_SCR(WIIG) = 1
      C_ECP_SCR(WIIS) = 1
      C_ECP_SCR(WIOC) = 1
      C_ECP_SCR(WII2) = 1
      C_ECP_SCR(WIPQ) = 1
      C_ECP_CCS(WIIS) = 1
      C_ECP_CCS(WIPQ) = 1
      C_ECP_FF(WIPC) = 1
      C_ECP_FF(WIIG) = 1
      C_ECP_FF(WIIS) = 1
      C_ECP_FF(WIOC) = 1
      C_ECP_FF(WII2) = 1
      C_ECP_FF(WIPQ) = 1
!
      DO IECP = 1 , WIIS
         write(18,1335) curiyr+uhbsyr,IECP,UPLNTCD(IECP),ECP_SCRUB(IECP,1),C_ECP_PART(IECP),C_ECP_COMB(IECP),C_ECP_SNCR(IECP),  &
                        C_ECP_SCR(IECP),C_ECP_CCS(IECP),C_ECP_FF(IECP)
 1335    format(1x,"C_ECP_CNTL",2(":",I4),":",A4,7(":",I2))
      END DO
!
!     Read in List of Coal Plant Retrofit Options
!
      RET_CODE = RD$TBL(I_CNTL,'%RET OPT NM%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NUM_ROPT,1,1)                    ! NUMBER OF Coal Plant Configurations
!
      RET_CODE = RD$TBL(I_CNTL,'%RETRO OPTS%',NUM_ROPT,UF_DBG,UF_MSG)
      RET_CODE = RD$I1(T_ROPT_NDX,1,MX_ROPT)                    ! Coal Plant Retrofit Options Index
      RET_CODE = RD$C1(T_ROPT_SO2,1,MX_ROPT)                    ! Coal Plant Retrofit Options SO2 Control
      RET_CODE = RD$C1(T_ROPT_COMB,1,MX_ROPT)                   ! Coal Plant Retrofit Options NOX COMB Control
      RET_CODE = RD$C1(T_ROPT_SNCR,1,MX_ROPT)                   ! Coal Plant Retrofit Options NOX SNCR Control
      RET_CODE = RD$C1(T_ROPT_SCR,1,MX_ROPT)                    ! Coal Plant Retrofit Options NOX SCR Control
      RET_CODE = RD$C1(T_ROPT_SC,1,MX_ROPT)                     ! Coal Plant Retrofit Options ACI Spray Cooler
      RET_CODE = RD$C1(T_ROPT_FF,1,MX_ROPT)                     ! Coal Plant Retrofit Options ACI Fabric Filter
!
      NUM_RCMB = MX_RCMB
      DO I_ROPT = 1 , NUM_ROPT
         DO I_RCMB = 1 , NUM_RCMB
            UCL_ROPT(I_RCMB,I_ROPT) = 0
         END DO
      END DO
      DO I_ROPT = 1 , NUM_ROPT
         IF (T_ROPT_SO2(I_ROPT)  .NE. "   ")  UCL_ROPT(1,I_ROPT) = 1
         IF (T_ROPT_COMB(I_ROPT) .NE. "    ") UCL_ROPT(2,I_ROPT) = 1
         IF (T_ROPT_SNCR(I_ROPT) .NE. "    ") UCL_ROPT(3,I_ROPT) = 1
         IF (T_ROPT_SCR(I_ROPT)  .NE. "   ")  UCL_ROPT(4,I_ROPT) = 1
         IF (T_ROPT_SC(I_ROPT)   .NE. "  ")   UCL_ROPT(5,I_ROPT) = 1
         IF (T_ROPT_FF(I_ROPT)   .NE. "  ")   UCL_ROPT(6,I_ROPT) = 1
         WRITE(18,2315) CURIYR+UHBSYR,I_ROPT,T_ROPT_SO2(I_ROPT),UCL_ROPT(1,I_ROPT), &
            T_ROPT_COMB(I_ROPT),UCL_ROPT(2,I_ROPT),T_ROPT_SNCR(I_ROPT),UCL_ROPT(3,I_ROPT), &
            T_ROPT_SCR(I_ROPT),UCL_ROPT(4,I_ROPT),T_ROPT_SC(I_ROPT),UCL_ROPT(5,I_ROPT), &
            T_ROPT_FF(I_ROPT),UCL_ROPT(6,I_ROPT)
 2315    FORMAT(1X,"ROPT",2(":",I4),6(":",A,":",I1))
      END DO
!
      DO I_CNFG = 1 , NUM_CNFG
         DO I_ROPT = 1 , NUM_ROPT
            UCL_RCMB(I_ROPT,I_CNFG) = 0
         END DO
      END DO
!
      DO I_CNFG = 1 , NUM_CNFG
         IF (USW_HG .EQ. 1 .OR. (USW_HG .EQ. 0 .AND. T_CNFG_SC(I_CNFG) .NE. "SC" .AND. T_CNFG_FF(I_CNFG) .NE. "FF")) THEN
            WRITE(18,2316) CURIYR+UHBSYR,I_CNFG,T_CNFG_PART(I_CNFG),T_CNFG_SO2(I_CNFG),T_CNFG_COMB(I_CNFG), &
               T_CNFG_SNCR(I_CNFG),T_CNFG_SCR(I_CNFG),T_CNFG_SC(I_CNFG),T_CNFG_FF(I_CNFG),I_CNFG,(UCL_CNFG(I_RCMB,I_CNFG),I_RCMB=1,NUM_RCMB+2)
 2316       FORMAT(1X,"CNFG0",2(":",I4),7(":",A),":",I4,8(":",I1))
            SV_MACT = UCL_CNFG(1,I_CNFG)
            DO I_ROPT = 1 , NUM_ROPT
               IF (USW_HG .EQ. 1 .OR. (USW_HG .EQ. 0 .AND. T_ROPT_SC(I_ROPT) .NE. "SC" .AND. T_ROPT_FF(I_ROPT) .NE. "FF")) THEN
                  I_TST = 0
                  J_TST = 0
                  IF (T_CNFG_SO2(I_CNFG)  .NE. "   "  .AND. T_ROPT_SO2(I_ROPT)  .NE. "   ")  I_TST = I_TST + 1
                  IF (T_CNFG_COMB(I_CNFG) .NE. "    " .AND. T_ROPT_COMB(I_ROPT) .NE. "    ") I_TST = I_TST + 1
                  IF (T_CNFG_SNCR(I_CNFG) .NE. "    " .AND. T_ROPT_SNCR(I_ROPT) .NE. "    ") I_TST = I_TST + 1
                  IF (T_CNFG_SCR(I_CNFG)  .NE. "   "  .AND. T_ROPT_SCR(I_ROPT)  .NE. "   ")  I_TST = I_TST + 1
                  IF (T_CNFG_SC(I_CNFG)   .NE. "  "   .AND. T_ROPT_SC(I_ROPT)   .NE. "  ")   I_TST = I_TST + 1
                  IF (T_CNFG_FF(I_CNFG)   .NE. "  "   .AND. T_ROPT_FF(I_ROPT)   .NE. "  ")   I_TST = I_TST + 1
!
                  IF (T_CNFG_SO2(I_CNFG)  .EQ. "   "  .AND. T_ROPT_SO2(I_ROPT)  .NE. "   ")  J_TST = J_TST + 1
                  IF (T_CNFG_COMB(I_CNFG) .EQ. "    " .AND. T_ROPT_COMB(I_ROPT) .NE. "    ") J_TST = J_TST + 1
                  IF (T_CNFG_SNCR(I_CNFG) .EQ. "    " .AND. T_ROPT_SNCR(I_ROPT) .NE. "    ") J_TST = J_TST + 1
                  IF (T_CNFG_SCR(I_CNFG)  .EQ. "   "  .AND. T_ROPT_SCR(I_ROPT)  .NE. "   ")  J_TST = J_TST + 1
                  IF (T_CNFG_SC(I_CNFG)   .EQ. "  "   .AND. T_ROPT_SC(I_ROPT)   .NE. "  ")   J_TST = J_TST + 1
                  IF (T_CNFG_FF(I_CNFG)   .EQ. "  "   .AND. T_ROPT_FF(I_ROPT)   .NE. "  ")   J_TST = J_TST + 1
                  IF (I_TST .EQ. 0 .AND. J_TST .GT. 0) THEN
                     NEW_PART = T_CNFG_PART(I_CNFG)
                     NEW_SO2  = T_CNFG_SO2(I_CNFG)
                     NEW_COMB = T_CNFG_COMB(I_CNFG)
                     NEW_SNCR = T_CNFG_SNCR(I_CNFG)
                     NEW_SCR  = T_CNFG_SCR(I_CNFG)
                     NEW_SC   = T_CNFG_SC(I_CNFG)
                     NEW_FF   = T_CNFG_FF(I_CNFG)
                     IF (T_CNFG_SO2(I_CNFG)  .EQ. "   ")  NEW_SO2  = T_ROPT_SO2(I_ROPT)
                     IF (T_CNFG_COMB(I_CNFG) .EQ. "    ") NEW_COMB = T_ROPT_COMB(I_ROPT)
                     IF (T_CNFG_SNCR(I_CNFG) .EQ. "    ") NEW_SNCR = T_ROPT_SNCR(I_ROPT)
                     IF (T_CNFG_SCR(I_CNFG)  .EQ. "   ")  NEW_SCR  = T_ROPT_SCR(I_ROPT)
                     IF (T_CNFG_SC(I_CNFG)   .EQ. "  ")   NEW_SC   = T_ROPT_SC(I_ROPT)
                     IF (T_CNFG_FF(I_CNFG)   .EQ. "  ")   NEW_FF   = T_ROPT_FF(I_ROPT)
                     DO J_CNFG = 1 , NUM_CNFG
                        IF (SV_MACT .EQ. UCL_CNFG(1,J_CNFG)) THEN
                           I_TST = 0
                           IF (NEW_PART .NE. T_CNFG_PART(J_CNFG)) I_TST = I_TST + 1
                           IF (NEW_SO2  .NE. T_CNFG_SO2(J_CNFG))  I_TST = I_TST + 1
                           IF (NEW_COMB .NE. T_CNFG_COMB(J_CNFG)) I_TST = I_TST + 1
                           IF (NEW_SNCR .NE. T_CNFG_SNCR(J_CNFG)) I_TST = I_TST + 1
                           IF (NEW_SCR  .NE. T_CNFG_SCR(J_CNFG))  I_TST = I_TST + 1
                           IF (NEW_SC   .NE. T_CNFG_SC(J_CNFG))   I_TST = I_TST + 1
                           IF (NEW_FF   .NE. T_CNFG_FF(J_CNFG))   I_TST = I_TST + 1
                           IF (I_TST .EQ. 0) THEN
                              UCL_RCMB(I_ROPT,I_CNFG) = J_CNFG
                              WRITE(18,2313) CURIYR+UHBSYR,SV_MACT,I_CNFG,I_ROPT,J_CNFG,T_CNFG_PART(I_CNFG),T_CNFG_SO2(I_CNFG),  &
                                 T_CNFG_COMB(I_CNFG),T_CNFG_SNCR(I_CNFG),T_CNFG_SCR(I_CNFG),T_CNFG_SC(I_CNFG),          &
                                 T_CNFG_FF(I_CNFG),T_ROPT_SO2(I_ROPT),T_ROPT_COMB(I_ROPT),T_ROPT_SNCR(I_ROPT),          &
                                 T_ROPT_SCR(I_ROPT),T_ROPT_SC(I_ROPT),T_ROPT_FF(I_ROPT),NEW_PART,NEW_SO2,NEW_COMB,      &
                                 NEW_SNCR, NEW_SCR,NEW_SC,NEW_FF
 2313                         FORMAT(1X,"CNFG1",5(":",I4),":",A3,A3,A4,A4,A3,A2,A2,":",A3,A4,A4,A3,A2,A2,":",A3,A3,A4,A4,A3,A2,A2,":")
                           END IF
                        END IF
                     END DO
                  END IF
               END IF
            END DO
         END IF
      END DO
!
! END NEW TECH CODE WCW
!
!     READ IN INDICES TO MAP TECHNOLOGIES FOR AIMMS
!
      RET_CODE = RD$TBL(I_CNTL,'%AIMMS INDX%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(AICL,1,1)
      RET_CODE = RD$I1(AIST,1,1)
      RET_CODE = RD$I1(AICT,1,1)
      RET_CODE = RD$I1(AICC,1,1)
      RET_CODE = RD$I1(AIFC,1,1)
      RET_CODE = RD$I1(AINU,1,1)
      RET_CODE = RD$I1(AIWD,1,1)
      RET_CODE = RD$I1(AIRN,1,1)
      RET_CODE = RD$I1(AIHY,1,1)
      RET_CODE = RD$I1(AIPS,1,1)
      RET_CODE = RD$I1(AIP2,1,1)
      RET_CODE = RD$I1(AISG,1,1)
      RET_CODE = RD$I1(AIWN,1,1)
      RET_CODE = RD$I1(AISO,1,1)
      RET_CODE = RD$I1(AIPV,1,1)
      RET_CODE = RD$I1(AIIN,1,1)
      RET_CODE = RD$I1(AIDG,1,1)
!
!     READ IN DISPATCHABLE PLANT CLASS PARAMETERS
!
      RET_CODE = RD$TBL(I_CNTL,'%ECP$PLT V1%',ECP_D_CAP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(UPLNTCD,1,ECP_D_CAP)  ! 2 CHAR ECP PLANT CODES
      RET_CODE = RD$I1(UPNUGSW,1,ECP_D_CAP)  ! NON-UTIL GEN BUILD SWITCH
      !RET_CODE = RD$I1(UPETTSW,1,ECP_D_CAP)  ! IRT BUILD SWITCH
      RET_CODE = RD$R81(CAP$GRW,1,ECP_D_CAP) ! ANNUAL GNP DEFLATOR
      RET_CODE = RD$I1(UPVTYP,1,ECP_D_CAP)   ! VINT TYPE,0=EXIST,1=NEW
      RET_CODE = RD$I1(UPEFPT,1,ECP_D_CAP)   ! EFP TYPE
      RET_CODE = RD$C1(TMP_EFD_CD_BY_ECP,1,ECP_D_CAP)   ! EFD PLANT TYPE
      RET_CODE = RD$I1(UPCPTRSW,1,ECP_D_CAP) ! TRANS EXISTING CAP SWITCH
      RET_CODE = RD$I1(UPTTYP,1,ECP_D_CAP)   ! PLANT TYPE CODE
      RET_CODE = RD$I1(UPTOPR,1,ECP_D_CAP)   ! OPERATE TYPE SWITCH
      RET_CODE = RD$I1(UACI_OPR,1,ECP_D_CAP) ! Acitivated Carbon Injection 1=>Operate without ACI 0=> Don't Operate w/0 ACI
      RET_CODE = RD$I1(AIMMS_GRP,1,ECP_D_CAP) ! Techology groupings for columns with similar AIMMS names

      NUM_RANK = MX_RANK
      DO IP = 1, ECP_D_CAP
         DO IRNK = 1 , NUM_RANK
         END DO
         UPCAPD(1,IP) = 1.0
         CAP$CUM = DBLE(1.0)
         DO IYR = 2 , UNYEAR + UNXPH
            CAP$CUM = CAP$CUM * (DBLE(1.0) + CAP$GRW(IP))
            UPCAPD(IYR,IP) = CAP$CUM
         END DO

!        Map ECP type to EFD type

         DO EFDt = 1 , EFD_D_CAP
            IF (TMP_EFD_CD_BY_ECP(IP) .EQ. EPPLCD(EFDt)) THEN
               UPEFDT(IP) = EFDt

               WRITE(18,3112) CURIRUN, CURIYR+1989, IP, EFDt, TMP_EFD_CD_BY_ECP(IP)
 3112          FORMAT(1X,"MAP_ECP_TO_EFD_TYPE",4(":",I5),":",A3)

               EXIT
            END IF
         END DO
         IF (EFDt .GT. EFD_D_CAP) THEN

            WRITE(6,3113) CURIRUN, CURIYR+1989, IP, TMP_EFD_CD_BY_ECP(IP)
 3113       FORMAT(1X,"MAP_ECP_TO_EFD_TYPE_OOPS",3(":",I5),":",A3)

         END IF
      END DO
!
!     CHARACTER*6 UPNAME(ECP_D_CAP,3) PLANT TYPE LABELS FOR REPORTS
!
      RET_CODE = RD$TBL(I_CNTL,'%ECP$PLT V2%',ECP_D_CAP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C2(UPNAME,1,1,3,ECP_D_CAP,3)
!
!     Read in Info on Mercury Banking
!
      RET_CODE = RD$TBL(I_CNTL,'%HG BNK INFO%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$R1(HG_FCTR,1,1)                   ! Number of HG Banks Allowed Over HG Banks Created
      RET_CODE = RD$I1(HG_BYR,1,1)                    ! Year HG Banks can Start to be Created
      RET_CODE = RD$I1(HG_SYR,1,1)                    ! Year HG Banks can Start to be Used
!
!     READ IN Mercury Emission Modification Factor Data
!
      RET_CODE = RD$TBL(I_CNTL,'%HG EMF INFO%',ECP_D_CAP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)                                  ! ECP Code
      RET_CODE = RD$C1(DUMMY,1,DMSZ)                                  ! ECP Index
      RET_CODE = RD$R2(PLNT_EMF,1,1,MX_RANK,ECP_D_CAP,MX_RANK)          ! Mercury Emission Modification Factors
      RET_CODE = RD$I2(ACI_STEPS,1,1,MX_RANK,ECP_D_CAP,MX_RANK)         ! Number of ACI Steps to Create
      RET_CODE = RD$I1(HG_CHOICE,1,ECP_D_CAP)                           ! Choice of Input or Output Mercury Standards: 0=>None, 1=>Input, 2=>Output, 3=>Either
      RET_CODE = RD$I1(HG_CLASS,1,ECP_D_CAP)                            ! HG Compliance Class : 0=>NA, 1=>Existing Coal, 2=>New PC Coal, 3=>New IGCC and Other Coal
      RET_CODE = RD$R2(MIN_EMF,1,1,MX_RANK,ECP_D_CAP,MX_RANK)           ! Minimum Mercury EMF achievable with ACI
      RET_CODE = RD$I1(ACI_FOM_SW,1,ECP_D_CAP)                          ! 2=> Include Capital Costs and Fixed O&M in Operates 1=> Just Variable Costs in Operates
      RET_CODE = RD$R2(ACI_OandM,1,1,MX_RANK,ECP_D_CAP,MX_RANK)         ! Incremental O&M for ACI - Bituminous (87$/MWh)
!
!     IF PAST MATS YEAR, ASSUME ACI ALREADY DONE AND RESET MIN EMF TO MATS STANDARD
!
      IF (UDSI_YR .GT. 0 .AND. UPSTYR .GE. UDSI_YR)THEN
         DO IP = 1 , ECP_D_CAP
!              write(6,3322) uplntcd(ip), MIN_EMF(IP,1),HG_MEFNC(1,UDSI_YR - UHBSYR),  &
!                                MAX(MIN_EMF(IP,1),HG_MEFNC(1,UDSI_YR - UHBSYR))
!3322 format(1h ,'!acidsi',a3,10f10.3)
            DO IRNK = 1 , MX_RANK
               MIN_EMF(IP,IRNK) = MAX(MIN_EMF(IP,IRNK),HG_MEFNC(1,UDSI_YR - UHBSYR))
            END DO
               ACI_FOM_SW(IP) = 1
         END DO
         DO IP = 1 , NUM_CNFG
            UCL_NSR_CFG(IP) = 1
         END DO
      END IF
!
      DO IP = 1 , ECP_D_CAP
         DO IRNK = 1 , MX_RANK
            ACI_STEPS(IP,IRNK) = MIN(MX_ACI , ACI_STEPS(IP,IRNK))
         END DO
      END DO
!
!     READ IN Activated Carbon Parameters
!
      RET_CODE = RD$TBL(I_CNTL,'%ACI RQ INFO%',NCLUT1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)                                  ! ECP Type ( Code and Index )
      RET_CODE = RD$R2(FGD_FCTR,1,1,MX_RANK,NCLUT1,MX_RANK)           ! FGD Factor
      RET_CODE = RD$R2(SCR_FCTR,1,1,MX_RANK,NCLUT1,MX_RANK)           ! SCR Factor
      RET_CODE = RD$R2(PARM_A,1,1,MX_RANK,NCLUT1,MX_RANK)             ! Parameter A
      RET_CODE = RD$R2(PARM_B,1,1,MX_RANK,NCLUT1,MX_RANK)             ! Parameter B
      RET_CODE = RD$R2(PARM_C,1,1,MX_RANK,NCLUT1,MX_RANK)             ! Parameter C
      RET_CODE = RD$R2(PARM_D,1,1,MX_RANK,NCLUT1,MX_RANK)             ! Parameter D
!
!     Initialize ACI Arrays to Share with the ECP
!
      UCL_MACT_CFG = 2
!
!     Initialize ACI Arrays to Share Input with Coal Model
!
      EMM_MEF = 1.0
      ACI_CST = 99.999
      ACI_QAC = 0.0
      ACI_OPT = 0
      NUM_ACI = 0
!
      DO XYR = 1 , UNYEAR
         DO IP = 1 , NUTSEC
            ICLS = HG_CLASS(IP)
            DO IRNK = 1 , NUM_RANK
                  IF (PLNT_EMF(IP,IRNK) .LE. MIN_EMF(IP,IRNK)) ACI_STEPS(IP,IRNK) = 0
               I_ACI = 1
               IF (UACI_OPR(IP) .EQ. 1) THEN
                  ACI_OPT(I_ACI,IRNK,IP,XYR) = 1
                  ACI_CST(I_ACI,IRNK,IP) = 0.0
                  IF (HG_MEF(ICLS,IRNK,XYR) .GT. 0.0 .AND. HG_MEF(ICLS,IRNK,XYR) .LT. PLNT_EMF(IP,IRNK)) ACI_OPT(I_ACI,IRNK,IP,XYR) = 0
               END IF
               IF (USW_HG .GT. 0) THEN
                  T_NUM_ACI = 0
                  IF (HG_OUTPUT(ICLS,IRNK,XYR) .GT. 0.0) T_NUM_ACI = ACI_STEPS(IP,IRNK)
                  IF (HG_INPUT(ICLS,IRNK,XYR) .GT. 0.0) T_NUM_ACI = ACI_STEPS(IP,IRNK)
                  IF (HG_MEF(ICLS,IRNK,XYR) .GT. 0.0) T_NUM_ACI = ACI_STEPS(IP,IRNK)
                  IF (EMLIM(4,XYR) .LT. 100.0) T_NUM_ACI = ACI_STEPS(IP,IRNK)
                  IF (HG_GRAMS_MWH(ICLS,XYR) .GT. 0.0) T_NUM_ACI = ACI_STEPS(IP,IRNK)
                  IF (USW_CAMR .GT. 0) T_NUM_ACI = ACI_STEPS(IP,IRNK)
!                 IF (USW_FEACI .GT. 0) T_NUM_ACI = FACI_STP(IP,IRNK,XYR)
                  DO I_ACI = 2 , T_NUM_ACI + 1
                     ACI_OPT(I_ACI,IRNK,IP,XYR) = 1
                     ACI_CST(I_ACI,IRNK,IP) = ACI_OandM(IP,IRNK)
                     IF (ACI_FOM_SW(IP) .EQ. 2) THEN
                        ACI_CST(I_ACI,IRNK,IP) = ACI_CST(I_ACI,IRNK,IP) + (UCL_ACI_F + UCL_ACI_O * 0.12) / 8.760
                     END IF
                  END DO
                  NUM_ACI = MAX(NUM_ACI , T_NUM_ACI)
               END IF
            END DO
         END DO
      END DO
!
!     Calculate ACI Requirements
!
      DO XYR = 1 , UNYEAR
!     If No CAMR, Determine Minimum MEF
         IF (USW_CAMR .GT. 0)THEN
            MACT_EMF = 1.0
            DO IRG = 1 , NDREG
               IF (HG_MEFNC(IRG,XYR) .LT. MACT_EMF)MACT_EMF = HG_MEFNC(IRG,XYR)
            END DO
         END IF
         DO IP = 1 , NUTSEC
            ICLS = HG_CLASS(IP)
            I_TST = 0
            J_TST = 0
            DO IRNK = 1 , NUM_RANK
               NMAX = NSTEP
               N = ACI_STEPS(IP,IRNK)
               CHOICE = HG_CHOICE(IP)
               OUT = HG_OUTPUT(ICLS,IRNK,XYR)
               HR = 10000.0
               IN = HG_INPUT(ICLS,IRNK,XYR)
               PRCNT = HG_MEF(ICLS,IRNK,XYR)
               LIM = EMLIM(4,XYR)
               GRAM = HG_GRAMS_MWH(ICLS,XYR)
               HG = 8.0
               EMF_P = PLNT_EMF(IP,IRNK)
               EMF_M = MIN_EMF(IP,IRNK)
               F_FGD = FGD_FCTR(IP,IRNK)
               F_SCR = SCR_FCTR(IP,IRNK)
               A = PARM_A(IP,IRNK)
               B = PARM_B(IP,IRNK)
               C = PARM_C(IP,IRNK)
               D = PARM_D(IP,IRNK)
               IF (ACI_CST(1,IRNK,IP) .EQ. 0.0) Allowed(1) = 1
!
               CALL GET_EMF_AND_ACI(NMAX,N,CHOICE,OUT,HR,IN,PRCNT,LIM,GRAM,HG,EMF_P,F_FGD,F_SCR,A,B,C,D,EMF_M,Allowed,EMF_T,ACI,MACT_SW,MACT_EMF)
!
               DO I_ACI = 1 , NSTEP
                  ACI_OPT(I_ACI,IRNK,IP,XYR) = Allowed(I_ACI)
                  IF (I_ACI .EQ. 2 .AND. N .GT. 0 .AND. (OUT .GT. 0.0 .OR. IN .GT. 0.0)) ACI_OPT(I_ACI,IRNK,IP,XYR) = 1
                  EMM_MEF(I_ACI,IRNK,IP) = EMF_T(I_ACI)
                  ACI_QAC(I_ACI,IRNK,IP) = ACI(I_ACI)
                  J_TST = J_TST + ACI_OPT(I_ACI,IRNK,IP,XYR)
               END DO
               I_TST = I_TST + ACI_OPT(1,IRNK,IP,XYR)
!
               IF (PRCNT .GT. 0.0) THEN
                  EMF_PRCNT = PRCNT
               ELSE
                  EMF_PRCNT = 1.0
               END IF
!
               IF (IN .GT. 0.0) THEN
                  EMF_IN = MIN(1.0 , IN / HG)
               ELSE
                  EMF_IN = 1.0
               END IF
!
               IF (OUT .GT. 0.0) THEN
                  EMF_OUT = MIN(1.0 , 1000.0 * OUT / HR / HG)
               ELSE
                  EMF_OUT = 1.0
               END IF
!
               EMF_MAX = MIN(EMF_P , EMF_PRCNT)
               IF (CHOICE .EQ. 1) THEN
                  EMF_MAX = MIN(EMF_MAX , EMF_IN)
               ELSE IF (CHOICE .EQ. 2) THEN
                  EMF_MAX = MIN(EMF_MAX , EMF_OUT)
               ELSE IF (CHOICE .EQ. 3) THEN
                  EMF_MAX = MAX(MIN(EMF_MAX , EMF_IN) , MIN(EMF_MAX , EMF_OUT))
               END IF
!
               DO I_ACI = 1 , NSTEP
                  IF (ACI_OPT(I_ACI,IRNK,IP,XYR) .GT. 0 .OR. ACI_OPT(I_ACI,IRNK,IP,1) .GT. 0) THEN
                     WRITE(18,2312) CURIYR+UHBSYR,XYR+UHBSYR,USW_HG,USW_BACT,UBACT_YR,IP,IRNK,I_ACI,EMM_MEF(I_ACI,IRNK,IP),ACI_CST(I_ACI,IRNK,IP), &
                        ACI_QAC(I_ACI,IRNK,IP), &
                        MACT_SW,N,CHOICE,LIM,GRAM,PRCNT,OUT,IN,HG,HR*0.001,EMF_M,EMF_P,EMF_PRCNT,EMF_IN,EMF_OUT,EMF_MAX,MACT_EMF, &
                        (ACI_OPT(I_ACI,IRNK,IP,TYR),TYR=1,UNYEAR)
 2312                FORMAT(1X,"ACI_OPT2",2(":",I4),":",I1,1(":",I1,":",I4),3(":",I3),":",F7.3,":",F7.3,":",F12.1,3(":",I2),14(":",F7.3),":T",<UNYEAR>(I1))
!                    Label:ACI_OPT2:CYEAR:XYR:USW_HG:USW_BACT:UBACT_YR:IP:IRNK:I_ACI:EMM_MEF:ACI_CST:ACI_QAC:ACI_OPT
                  END IF
               END DO
!
            END DO
!
            IF (J_TST .EQ. 0) THEN
               UCL_MACT_CFG(IP) = 0
            ELSE IF (I_TST .EQ. 0) THEN
               UCL_MACT_CFG(IP) = 1
            END IF
         END DO
      END DO
!
!
!     READ ECONOMIC RETIREMENT SWITCH FOR NONNUCS
!
      RET_CODE = RD$TBL(I_CNTL,'%ECONOMIC RETIRE SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_ERET,1,1)
      RET_CODE = RD$I1(UPRTLT,1,1)
      RET_CODE = RD$I1(UREV_NYR,1,1)
      RET_CODE = RD$I1(UREV_NYRN,1,1)
!
!     READ ECONOMIC RETIREMENT / 2ND LICENSE RENEWAL-RETIREMENT SWITCH FOR NUCS
!
      RET_CODE = RD$TBL(I_CNTL,'%NUC ECON RETIRE SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_NRET,1,1)
      RET_CODE = RD$I1(UPNASW,1,1)
!
!     Read Nuclear Retirement Leadtime
!
      RET_CODE = RD$TBL(I_CNTL,'%NUCLEAR LT%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UNUC_RLT,1,1)                    ! Nuclear Retirement Leadtime
      RET_CODE = RD$I1(UNUC_SYR,1,1)                    ! Nuclear Retirement Start Year
!
!     READ MAXIMUM RETIREMENT RATE, IF ANY
!
      RET_CODE = RD$TBL(I_CNTL,'%MAX RET RATE%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UPRETRAT,1,1)
      RET_CODE = RD$R1(UPRETRT0,1,1)
      RET_CODE = RD$R1(UPRETRTL,1,1)
      RET_CODE = RD$R1(UPRETCHG,1,1)
!
!     READ FOSSIL (FRAC) AND NUCLEAR (MILLS) RETIREMENT HURDLE RATES
!
      RET_CODE = RD$TBL(I_CNTL,'%RET HURDLE RATES%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UPRETFHU,1,1)
      RET_CODE = RD$R1(UPRETNHU,1,1)
!
!     RET_GRP NUMBER OF RETIREMENT GROUPS BY ECP TYPE
!     RET_PCT PERCENT OF CAPACITY IN EACH RETIREMENT GROUP
!
      RET_CODE = RD$TBL(I_CNTL,'%RET GRPS  %',ECP_D_CAP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(RET_GRP,1,ECP_D_CAP)             !
      RET_CODE = RD$R2(RET_PCT,1,1,ECP_D_RET,ECP_D_CAP,ECP_D_RET) !
!
!     Read in Number of Discrete Steps in Age Dependent O&M Adders
!
      RET_CODE = RD$TBL(I_CNTL,'%XTRA Ages %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(XTRA_NUM,1,1)  ! Number of Discrete Steps in Age Dependent O&M Adders
      RET_CODE = RD$I1(XTRA_YR,1,1)   ! Year of Dollars in following Cost Table
      RET_CODE = RD$R81(XTRA_DEF,1,1) ! Deflator to convert costs to 1987 dollars
!
!     UPXTRA Age Dependent O&M Adders
!
      RET_CODE = RD$TBL(I_CNTL,'%XTRA OandM%',ECP_D_CAP+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R82(XTRA_OM,1,1,XTRA_NUM,ECP_D_CAP+1,MNUMYR+ECP_D_FPH) !
!
      DO STEP = 1 , XTRA_NUM + 1
         IF (STEP .EQ. 1) THEN
            ISTART = 1
         ELSE
            ISTART = ISTOP + 1
         END IF
         IF (STEP .EQ. XTRA_NUM + 1) THEN
            ISTOP = UNYEAR + ECP_D_FPH
         ELSE
            ISTOP = INT(XTRA_OM(1,STEP)) - 1
         END IF
         DO AGE = ISTART , ISTOP
            DO JCAP = 2 , ECP_D_CAP + 1
               ICAP = JCAP - 1
               IF (STEP .EQ. 1) THEN
                  UPXTRA(AGE,ICAP) = 0.0
               ELSE
                  UPXTRA(AGE,ICAP) = XTRA_OM(JCAP,STEP-1) / XTRA_DEF
               END IF
            END DO
         END DO
      END DO

!     Read in second option for aging factors - parameter for linear function of age

      RET_CODE = RD$TBL(I_CNTL,'%XTRA Opt 2 %',ECP_D_CAP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UPAGEOPT,1,ECP_D_CAP)             !option by technology
      RET_CODE = RD$R81(XTRA_OM2,1,ECP_D_CAP) !parameter for aging function

      DO ICAP = 1, ECP_D_CAP
         IF (UPAGEOPT(ICAP) .EQ. 1) THEN
           DO AGE = 1, UNYEAR + ECP_D_FPH
              UPXTRA(AGE,ICAP) = XTRA_OM2(ICAP) * AGE
           ENDDO
         ENDIF
      ENDDO

!
!     READ IN GROWTH RATES AND WEIGHTS FOR DEAMAND EXPECTATION CALCULATIONS
!
      RET_CODE = RD$TBL(I_CNTL,'%DEM GROWTH%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(GRW_XYR,1,1)
!
      RET_CODE = RD$TBL(I_CNTL,'%DEM RATE  %',5,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(DEM_GRW,1,1,MNUMCR,5,MNUMCR)
!
      RET_CODE = RD$TBL(I_CNTL,'%DEM WEIGHT%',5,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R2(DEM_WGHT,1,1,MNUMCR,5,MNUMCR)
!
!     READ IN EMMPLNT INDX TO INT/RENEW INDEX
!
      RET_CODE = RD$TBL(I_CNTL,'%INT-RNW V1%',ECP_D_I_R,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UPLNTIDX,1,ECP_D_I_R)  ! EMM PLT INDX MAP TO RNW INDX
!
!     YEAR CODES
!
      RET_CODE = RD$TBL(I_CNTL,'%YEAR CODE %',ECP_D_XPH,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)

      RET_CODE = RD$C1(UPYRCD,1,ECP_D_XPH)                    ! YEAR CODES
!
!     DSM GROUP CODES
!
      RET_CODE = RD$TBL(I_CNTL,'%DSM CODES %',ECP_D_DSM,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(UPDMCD,1,ECP_D_DSM)        ! 2 CHAR DSM GROUP CODES
!
!     RETROFIT CLUSTER CODE
!
      RET_CODE = RD$TBL(I_CNTL,'%RETRO GRPS%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UPSGRP,1,1)         ! RETROFIT CLUSTER CODE
      DO IGRP = 1 , UPSGRP
         WRITE(UPSCCD(IGRP),1393) IGRP
 1393    FORMAT(I1)
      END DO

!
!     ECP FUEL CODES AND FUEL TYPES
!
      ECP$TMP = ECP_D_NFL
      RET_CODE = RD$TBL(I_CNTL,'%ECP FUEL C%',ECP$TMP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(FLNAME,1,ECP_D_NFL)                ! ECP LONG FUEL NAME
      RET_CODE = RD$C1(UPFLCD,1,ECP_D_NFL)                ! ECP FUEL CODES
      RET_CODE = RD$I1(UPFNUC,1,ECP_D_NFL)                ! ECP FUEL INDEX -- NUCLEAR
      RET_CODE = RD$I1(UPFCOL,1,ECP_D_NFL)                ! ECP FUEL INDEX -- COAL
      RET_CODE = RD$I1(UPFGAS,1,ECP_D_NFL)                ! ECP FUEL INDEX -- GAS
      RET_CODE = RD$I1(UPFRES,1,ECP_D_NFL)                ! ECP FUEL INDEX -- RESID
      RET_CODE = RD$I1(UPFDIS,1,ECP_D_NFL)                ! ECP FUEL INDEX -- DISTILLATE
      RET_CODE = RD$I1(UPFHYT,1,ECP_D_NFL)                ! ECP FUEL INDEX -- HYDROGEN
!
!     FINANCIAL OWNERSHIP TYPE
!
      RET_CODE = RD$TBL(I_CNTL,'%ECP OWN CD%',ECP_D_OWN,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(OWNNAME,1,ECP_D_OWN)     ! ECP LONG OWNER NAME
      RET_CODE = RD$C1(UPOWNCD,1,ECP_D_OWN)     ! FINANCIAL OWNERSHIP TYPE
!
!     LOAD SEGMENT PARAMETERS
!
      RET_CODE = RD$TBL(I_CNTL,'%LOAD SG CD%',ECP_D_VLS,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$C1(UPLDCD,1,ECP_D_VLS)     ! 1 CHAR LOAD SEGMENT CODES
      RET_CODE = RD$C1(UPMDCD,1,ECP_D_VLS)       ! 1 CHAR MODE OF OP CODES
!
!     Scenario Definition Switches
!     GLOBAL DEMAND SCALING
!
      RET_CODE = RD$TBL(I_CNTL,'%DMD SCL SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_CEL,1,1)     ! USER SWITCH FOR DEMAND SCALING
!
      RET_CODE = RD$TBL(I_CNTL,'%DMD SCL FC%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(UQELFAC,UISTYR,MNUMYR)    ! ANN GLOBAL DMD SCALING FAC
!
!     GLOBAL ADJUSTMENTS FOR SCENARIONS (E.G., MDR RUNS)
!
      RET_CODE = RD$TBL(I_CNTL,'%GLOBAL SCL%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_MDR,1,1)     ! USER SWITCH FOR GLOBAL SCALING
      RET_CODE = RD$R1(UQELPCT,1,1)          ! % CHANGE IN GLOBAL DEMAND
      RET_CODE = RD$R1(UPFLPCT,1,1)        ! % CHANGE IN COAL/GAS PRICES
      RET_CODE = RD$R1(UPCEPCT,1,1)            ! % CHANGE IN COST EQUITY
!
!     PARAMETERS FOR POLICY RESTRUCTURING CASES
!
      RET_CODE = RD$TBL(I_CNTL,'%RESTR SCAL%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_POL,1,1)    ! USER SWITCH FOR POLICY RESTRUCT
      RET_CODE = RD$R1(UPOMPCT,1,1)               ! % MULTIPLIER FOR O&M
!
!     UPXPFL:  MULTIPLIER FOR SENSITIVITIES ON FUEL PRICE EXPECTATIONS
!
      RET_CODE = RD$TBL(I_CNTL,'%SCALAR XPFL%',ECP$TMP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UPXPFL,1,ECP_D_NFL) ! MULTIPLIER FOR EXPECTATIONS
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
!
!     USW_HYD:  SWITCH TO REVISE AVERAGE HYDRO CAPACITY FACTORS
!     UQHYFAC:  REGIONAL ADJUSTMENT TO AVERAGE HYDRO GENERATION
!
      RET_CODE = RD$TBL(I_CNTL,'%HYDRO ADJ SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_HYD,1,1)     ! SWITCH (0=NO,1=YES)
      !
!     USW_HRP:  SWITCH TO USE HEAT RATE IMPROVEMENT PROFILES
!
      RET_CODE = RD$TBL(I_CNTL,'%HR IMPROV SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_HRP,1,1)     ! SWITCH (0=NO,1=YES)
!
!     UHR_TRGT:  HEAT RATE IMPROVEMENT TARGET
!
      RET_CODE = RD$TBL(I_CNTL,'%HRATE TARGETS%',EFD_D_DSP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UHR_TRGT,1,EFD_D_DSP)     ! PLANT-TYPE TARGETS
!
!     UHR_PROF:  HEAT RATE IMPROVEMENT PROFILES
!
      RET_CODE = RD$TBL(I_CNTL,'%HR PROFILES%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R2(UHR_PROF,UISTYR,1,EFD_D_DSP,MNUMYR,EFD_D_DSP) ! HR PROFILES
!
!     USW_HRP:  SWITCH TO USE O&M IMPROVEMENT PROFILES
!
      RET_CODE = RD$TBL(I_CNTL,'%OM IMPROV SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_OMP,1,1)     ! SWITCH (0=NO,1=YES)
!
!     UHR_TRGT:  O&M IMPROVEMENT TARGET
!
      RET_CODE = RD$TBL(I_CNTL,'%OANDM TARGETS%',EFD_D_DSP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UOM_TRGT,1,EFD_D_DSP)     ! PLANT-TYPE TARGETS
!
!     UOM_PROF:  O&M IMPROVEMENT PROFILES
!
      RET_CODE = RD$TBL(I_CNTL,'%OM PROFILES%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R2(UOM_PROF,UISTYR,1,EFD_D_DSP,MNUMYR,EFD_D_DSP) ! HR PROFILES
!
!     USW_FOM:  SWITCH TO INCLUDE PCT OF FOM IN MARGINAL PRICE
!
      RET_CODE = RD$TBL(I_CNTL,'%PCT OM SWITCH%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_FOM,1,1)     ! SWITCH (0=NO,1=YES)
!
!     UFOM_CFC:  CAPACITY FACTOR TO CONVERT FOM TO MILLS/KWH
!     UFOM_PCT:  PCT OF FOM INCLUDED IN MARGINAL PRICE
!
      RET_CODE = RD$TBL(I_CNTL,'%OM IN MRGPRC%',EFD_D_CAP,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(UFOM_CFC,1,EFD_D_CAP) ! FOM CAPACITY FACTORS
      RET_CODE = RD$R1(UFOM_PCT,1,EFD_D_CAP) ! FOM PCT
!
!     USW_DSM:  SWITCH TO USE SECTORAL DSM DEMAND REDUCTIONS
!
      RET_CODE = RD$TBL(I_CNTL,'%DEMAND RED SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_DSM,1,1)     ! SWITCH (0=NO,1=YES)
!
!     QELRSDSM:  RESIDENTIAL DSM DEMAND REDUCTION
!     QELCMDSM:  COMMERCIAL DSM DEMAND REDUCTION
!     QELINDSM:  INDUSTRIAL DSM DEMAND REDUCTION
!     QELTRDSM:  TRANSPORTATION DSM DEMAND REDUCTION
!
      RET_CODE = RD$TBL(I_CNTL,'%DEMAND RED QTY%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(QELRSDSM,UISTYR,MNUMYR) ! RESIDENTIAL DSM
      RET_CODE = RD$R1(QELCMDSM,UISTYR,MNUMYR) ! COMMERCIAL DSM
      RET_CODE = RD$R1(QELINDSM,UISTYR,MNUMYR) ! INDUSTRIAL DSM
      RET_CODE = RD$R1(QELTRDSM,UISTYR,MNUMYR) ! TRANSPORTATION DSM
!
!     USW_CDG:  SWITCH TO USE COMMERCIAL DISTRIBUTED GENERATION
!
      RET_CODE = RD$TBL(I_CNTL,'%COM DISTGEN SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_CDG,1,1)     ! SWITCH (0=NO,1=YES)
!
!     DGCOMGNG:  COMMERCIAL DISTRIBUTED GENERATION -- NG
!     DGCOMGDS:  COMMERCIAL DISTRIBUTED GENERATION -- DS
!     DGCOMGRS:  COMMERCIAL DISTRIBUTED GENERATION -- RS
!     DGCOMGLP:  COMMERCIAL DISTRIBUTED GENERATION -- LPG
!     DGCOMGCL:  COMMERCIAL DISTRIBUTED GENERATION -- CL
!     DGCOMGMS:  COMMERCIAL DISTRIBUTED GENERATION -- MSW
!
      RET_CODE = RD$TBL(I_CNTL,'%COM DISTGEN QTY%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(DGCOMGNG,UISTYR,MNUMYR) ! DISTGEN - GAS
      RET_CODE = RD$R1(DGCOMGDS,UISTYR,MNUMYR) ! DISTGEN - DIST
      RET_CODE = RD$R1(DGCOMGRS,UISTYR,MNUMYR) ! DISTGEN - RESID
      RET_CODE = RD$R1(DGCOMGLP,UISTYR,MNUMYR) ! DISTGEN - LPG
      RET_CODE = RD$R1(DGCOMGCL,UISTYR,MNUMYR) ! DISTGEN - COAL
      RET_CODE = RD$R1(DGCOMGMS,UISTYR,MNUMYR) ! DISTGEN - MSW
      RET_CODE = RD$R1(TOT,UISTYR,MNUMYR)      ! Total
!
!     USW_CDG:  SWITCH TO USE INDUSTRIAL DISTRIBUTED GENERATION
!
      RET_CODE = RD$TBL(I_CNTL,'%IND DISTGEN SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_IDG,1,1)     ! SWITCH (0=NO,1=YES)
!
!     DGINDGSG:  INDUSTRIAL DISTRIBUTED GENERATION -- SALES TO GRID
!     DGCOMGOU:  INDUSTRIAL DISTRIBUTED GENERATION -- OWN USE
!
      RET_CODE = RD$TBL(I_CNTL,'%IND DISTGEN QTY%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(DGINDGSG,UISTYR,MNUMYR) ! IND DISTGEN - SALES TO GRID
      RET_CODE = RD$R1(DGINDGOU,UISTYR,MNUMYR) ! IND DISTGEN - OWN USE
      RET_CODE = RD$R1(TOT,UISTYR,MNUMYR)      ! Total
!
!     USW_BMYR:  SWITCH TO MAKE BIOMASS PLANTS MUST RUN
!                (NO=0,YES=YEAR TO START)
!
      RET_CODE = RD$TBL(I_CNTL,'%MUST RUN BIO SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_BMYR,1,1)     ! SWITCH (0=NO,YEAR>0=YES)
!
!     USW_CFSH:  SWITCH TO OVERWRITE PLANT FILE COFIRING SHARES
!
      RET_CODE = RD$TBL(I_CNTL,'%COFIRE SHR SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_CFSH,1,1)     ! SWITCH (0=NO,1=YES)
      RET_CODE = RD$I1(UYR_CFRG,1,1)     ! YR SWITCH TO REG SHARE
!
!     URWDCFST:  STATE COFIRING SHARES, IF USED

!     URHYCFA:  ANNUAL ADJUSTMENT TO HYDRO CAPACITY FACTORS  *** this is now calculated in the code, and not read in ** 9/2015
!
!     RET_CODE = RD$TBL(I_CNTL,'%HYD CFC ADJ%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
!     RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
!     RET_CODE = RD$R1(URHYCFA,UISTYR,MNUMYR) ! HYDRO ADJUSTMENT FACTOR
!
!     READ IN SMART GRID CASE PARAMETERS
!
!     USW_SGRID: SWITCH FOR SMART GRID CASE
!
      RET_CODE = RD$TBL(I_CNTL,'%SMRT GRD SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_SGRID,1,1)    ! SWITCH TO RUN SMART GRID CASE
      RET_CODE = RD$I1(UYR_SGRID,1,1)    ! START YEAR FOR SMART GRID IMPROVEMENTS
!
!     SMART GRID CASE IMPROVEMENT VALUES
!
      RET_CODE = RD$TBL(I_CNTL,'%SMRT GRD IMP%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(USGTDIMP,1,1)     ! % T&D EFFICIENCY IMPROVEMENT DUE TO SMART GRID
      RET_CODE = RD$R1(USGTCAPIMP,1,1)   ! % IMPROVEMENT TO TRANSMISSION CAPACITY DUE TO SMART GRID
      RET_CODE = RD$R1(USGTDOMIMP,1,1)   ! % IMPROVEMENT TO T&D O&M COSTS DUE TO SMART GRID
      RET_CODE = RD$R1(USGRELIMP,1,1)    ! % ANNUAL IMPROVEMENT TO SYSTEM RELIABILITY DUE TO SMART GRID
      RET_CODE = RD$I1(UYR_SGREL,1,1)    ! START YEAR FOR RELIABILITY IMPROVEMENTS
      RET_CODE = RD$I1(UNYRSGREL,1,1)    ! NUMBER OF YEARS TO IMPROVE RELIABILITY
!
!     SMART GRID IMPROVEMENT COSTS
!!
!     USW_DMLOSSAD: SWITCH FOR LOSS ADJUSTMENTS DUE TO CHANGES IN DEMAND
!
      RET_CODE = RD$TBL(I_CNTL,'%DM LOSS ADJ SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_DMLSADJ,1,1)    ! SWITCH TO ADJUST T&D LOSS FACTORS FOR CHANGES IN DEMAND
      RET_CODE = RD$R1(UDMLOSSTOL,1,1)      ! tolerance level for demand change loss adjustments
      RET_CODE = RD$I1(UYR_DMLSADJ,1,1)     ! start year for demand change loss adjustments
!
!     SMART GRID BASE DEMANDS FOR LOSS FACTRO ADJUSTMENTS
!
!     SMART GRID REGIONAL LOSS FACTOR ADJUSTMENTS
!
!     END OF SMARTGRID CASE PARAMETERS

!     NGBS:

      RET_CODE = RD$TBL(I_CNTL,'%NGBS VARS %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NGBS_SW,1,1)    ! NGBS SWITCH
      RET_CODE = RD$R1(TOL_SP_CAP_FAC,1,1)
      RET_CODE = RD$R1(TOL_CF,1,1)
      RET_CODE = RD$R1(TOL_VOM,1,1)
      RET_CODE = RD$R1(TOL_RVAL,1,1)
      RET_CODE = RD$R1(TOL_HTRT_NGBS,1,1)
      RET_CODE = RD$R1(TOL_FOM,1,1)
      RET_CODE = RD$R1(TOL_CAP,1,1)

!     Write(6,'(a22,I6)')   'debug ngbs_sw',NGBS_SW
!     Write(6,'(a22,F6.2)') 'debug TOL_SP_CAP_FAC',TOL_SP_CAP_FAC
!     Write(6,'(a22,F6.2)') 'debug TOL_CF',TOL_CF
!     Write(6,'(a22,F6.2)') 'debug TOL_VOM',TOL_VOM
!     Write(6,'(a22,F6.2)') 'debug TOL_RVAL',TOL_RVAL
!     Write(6,'(a22,F6.2)') 'debug TOL_HTRT_NGBS',TOL_HTRT_NGBS
!     Write(6,'(a22,F6.2)') 'debug TOL_FOM',TOL_FOM
!     Write(6,'(a22,F6.2)') 'debug TOL_CAP',TOL_CAP

!     CL_to_NG:

      RET_CODE = RD$TBL(I_CNTL,'%CL_to_NG  %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UCL_CL_NG_SYR,1,1)       ! CL to NG Start Year
      RET_CODE = RD$R1(UCL_CL_NG_FOM_ADJ,1,1)   ! CL to NG Fixed O&M Adjustment Factor
      RET_CODE = RD$R1(UCL_CL_NG_VOM_ADJ,1,1)   ! CL to NG Variable O&M Adjustment Factor
      RET_CODE = RD$R1(UCL_CL_NG_HR_PEN,1,1)    ! CL to NG Heatrate Penalty

!     Write(6,'(a25,I6)')   'debug UCL_CL_NG_SYR', UCL_CL_NG_SYR
!     Write(6,'(a25,F6.2)') 'debug UCL_CL_NG_FOM_ADJ', UCL_CL_NG_FOM_ADJ
!     Write(6,'(a25,F6.2)') 'debug UCL_CL_NG_VOM_ADJ', UCL_CL_NG_VOM_ADJ
!     Write(6,'(a25,F6.2)') 'debug UCL_CL_NG_HR_PEN ', UCL_CL_NG_HR_PEN

!     HRI Control Variables for Plant File Inputs

      RET_CODE = RD$TBL(I_CNTL,'%HRI QUARNM%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UPHRNQRT,1,1)        ! Number of Quartiles for HRI
!
      RET_CODE = RD$TBL(I_CNTL,'%HRI QUARSW%',UPHRNQRT,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UPHRIMPQ,1,UPHRNQRT) ! Allow HRI for Quartile (0=No,1=Yes)

!     TOLERANCE FOR STEO BENCHMARKING CONSTRAINTS (0.0 = DON'T USE)

      RET_CODE = RD$TBL(I_CNTL,'%BM TOLERAN%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$R1(BMCLTOL,1,1)             ! BENCHMARK TOLERANCE (PCT AS SCALAR) -- COAL
      RET_CODE = RD$R1(BMNGTOL,1,1)             ! BENCHMARK TOLERANCE (PCT AS SCALAR) -- GAS
      RET_CODE = RD$R1(BMOLTOL,1,1)             ! BENCHMARK TOLERANCE (PCT AS SCALAR) -- OIL
      RET_CODE = RD$R1(BMIMPTOL,1,1)            ! BENCHMARK TOLERANCE (PCT AS SCALAR) -- NET IMPORTS

!     PHASE OUT PERIOD FOR STEO BENCHMARKING FACTOR (NONFOSSIL TYPES)

      RET_CODE = RD$TBL(I_CNTL,'%BM PHASE  %',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(BMPHSNC,1,1)             ! NUMBER OF YEARS TO PHASE OUT BENCHMARKING CF ADJ NUCLEAR (NONFOSSIL)
      RET_CODE = RD$I1(BMPHSHY,1,1)             ! NUMBER OF YEARS TO PHASE OUT BENCHMARKING CF ADJ HYDRO (NONFOSSIL)
      RET_CODE = RD$I1(BMPHSSO,1,1)             ! NUMBER OF YEARS TO PHASE OUT BENCHMARKING CF ADJ SOLAR (NONFOSSIL)
      RET_CODE = RD$I1(BMPHSWN,1,1)             ! NUMBER OF YEARS TO PHASE OUT BENCHMARKING CF ADJ WIND (NONFOSSIL)
      RET_CODE = RD$I1(BMPHSGT,1,1)             ! NUMBER OF YEARS TO PHASE OUT BENCHMARKING CF ADJ GEOTH (NONFOSSIL)
!
      RET_CODE = RD$TBL(I_CNTL,'%BM QUANTY %',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(BMCLGEN,UISTYR,MNUMYR) ! BENCHMARK CL GEN (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMCLCON,UISTYR,MNUMYR) ! BENCHMARK CL CON (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMNGGEN,UISTYR,MNUMYR) ! BENCHMARK NG GEN (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMNGCON,UISTYR,MNUMYR) ! BENCHMARK NG CON (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMOLGEN,UISTYR,MNUMYR) ! BENCHMARK OL GEN (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMOLCON,UISTYR,MNUMYR) ! BENCHMARK OL CON (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMNCGEN,UISTYR,MNUMYR) ! BENCHMARK NUC GEN (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMHYGEN,UISTYR,MNUMYR) ! BENCHMARK HYD GEN (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMSOGEN,UISTYR,MNUMYR) ! BENCHMARK SOL TH GEN (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMPVGEN,UISTYR,MNUMYR) ! BENCHMARK SOL PV GEN (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMWNGEN,UISTYR,MNUMYR) ! BENCHMARK WND GEN (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMGTGEN,UISTYR,MNUMYR) ! BENCHMARK GEO GEN (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMNETIMP,UISTYR,MNUMYR) ! BENCHMARK NET IMP (0.0 = DON"T BM)
      RET_CODE = RD$R1(BMELPRC,UISTYR,MNUMYR) ! BENCHMARK ELEC PRC (0.0 = DON"T BM)

!     FOR NOW ZERO OUT WL and PT benchmark factor
      BMPTGEN = 0.0
      BMWLGEN = 0.0

!     overwrite values with those from STEO include
      DO IY = UISTYR, MNUMYR
         IF (BMCLGEN(IY) .GT. 0.0) BMCLGEN(IY) = CLEPGEN_US(IY)
         IF (BMNGGEN(IY) .GT. 0.0) BMNGGEN(IY) = NGEPGEN_US(IY)
         IF (BMOLGEN(IY) .GT. 0.0) BMOLGEN(IY) = PAEPGEN_US(IY)
         IF (BMNCGEN(IY) .GT. 0.0) BMNCGEN(IY) = NUEPGEN_US(IY)
         IF (BMHYGEN(IY) .GT. 0.0) BMHYGEN(IY) = HVEPGEN_US(IY)
       IF (DPVDISPATCH) THEN
         IF (BMSOGEN(IY) .GT. 0.0) BMSOGEN(IY) = SOEPGEN_US(IY) + SOCMGEN_US(IY) + SODTP_US(IY)      !solar is combined in STEO, also need to include end-use if DPVDISPATCH on
       ELSE
         IF (BMSOGEN(IY) .GT. 0.0) BMSOGEN(IY) = SOEPGEN_US(IY)                !if DPVDISPATCH false then just bench to power sector value
       ENDIF
!        IF (BMPVGEN(IY) .GT. 0.0) BMPVGEN(IY) = SOPV_US(IY)    !ignore PV switch, only one solar variable to bench to
         IF (BMWNGEN(IY) .GT. 0.0) BMWNGEN(IY) = WNEPGEN_US(IY)
         IF (BMGTGEN(IY) .GT. 0.0) BMGTGEN(IY) = GEEPGEN_US(IY)
         IF (BMNETIMP(IY) .GT. 0.0) BMNETIMP(IY) = ELNIPUS(IY)

!   consumption vars
         IF (BMCLCON(IY) .GT. 0.0) BMCLCON(IY) = CLEPCONB(IY) * 1000.0
         IF (BMNGCON(IY) .GT. 0.0) BMNGCON(IY) = NGEPCONB(IY) * 1000.0
         IF (BMOLCON(IY) .GT. 0.0) BMOLCON(IY) = PAEPCONB(IY) * 1000.0
      ENDDO

!     initialize annual benchmark factors
      URNCCFA = 1.0
      URHYCFA = 1.0
      URSOCFA = 1.0
      URPVCFA = 1.0
      URPTCFA = 1.0
      URWNCFA = 1.0
      URWLCFA = 1.0
      URGTCFA = 1.0

      BMNCLYR = 0
      BMHYLYR = 0
      BMSOLYR = 0
!     BMPVLYR = 0
      BMWNLYR = 0
      BMGTLYR = 0
      DO BYR = 2, MNUMYR
!       write(6,'(a10,I4,6F10.2)') 'BMDAT',BYR,BMNCGEN(BYR),BMHYGEN(BYR),BMSOGEN(BYR),BMPVGEN(BYR),BMWNGEN(BYR),BMGTGEN(BYR)
        IF (BMNCGEN(BYR) .EQ. 0.0 .AND. BMNCGEN(BYR-1) .GT. 0.0) BMNCLYR = BYR-1
        IF (BMHYGEN(BYR) .EQ. 0.0 .AND. BMHYGEN(BYR-1) .GT. 0.0) BMHYLYR = BYR-1
        IF (BMSOGEN(BYR) .EQ. 0.0 .AND. BMSOGEN(BYR-1) .GT. 0.0) BMSOLYR = BYR-1
!       IF (BMPVGEN(BYR) .EQ. 0.0 .AND. BMPVGEN(BYR-1) .GT. 0.0) BMPVLYR = BYR-1
        IF (BMWNGEN(BYR) .EQ. 0.0 .AND. BMWNGEN(BYR-1) .GT. 0.0) BMWNLYR = BYR-1
        IF (BMGTGEN(BYR) .EQ. 0.0 .AND. BMGTGEN(BYR-1) .GT. 0.0) BMGTLYR = BYR-1
      ENDDO
      write(22,'(a20,5I6)') 'Bench last year ',BMNCLYR, BMHYLYR,BMSOLYR,BMWNLYR,BMGTLYR

!     HIRPSSW: Hawaii RPS Switch

      RET_CODE = RD$TBL(I_CNTL,'%HAW RPS SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(HIRPSSW,1,1)        ! RPS Switch for Hawaii (0=No,1=Yes)
!     print *,'!hirpssw',hirpssw
!
!     HIRPSRT:  RPS REQUIREMENT FOR HAWAII (RATE)
!
      RET_CODE = RD$TBL(I_CNTL,'%HAWAII RPS%',MNUMYR-UISTYR+1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,UISTYR,DMSZ)
      RET_CODE = RD$R1(HIRPSRT,UISTYR,MNUMYR) ! HAWAII RPS

      HR_KNOTS = 0
      HR_EFF = 0.0
      HR_LL = 0.0
      TEST_ITYP = 0

      READ(I_CNTL,*)
      READ(I_CNTL,*)
      DO ICHK = 1 , NDREG * ECP_D_CAP
         READ(I_CNTL,*) TMPA,TMPB,TMPD,TMPC,( (TMP1(KNOT),TMP2(KNOT)),KNOT=1,TMPC  )
         IF (TMPA .EQ. 0) EXIT
         DO ICAP=1,ECP_D_CAP
            IF (UPLNTCD(ICAP).EQ. TMPB) THEN
               ITYP = UPTTYP(ICAP)
               TEST_ITYP(ITYP,TMPA) = TEST_ITYP(ITYP,TMPA) + 1
               HR_KNOTS(TMPA,ICAP) = TMPC
               DO KNOT = 1 , TMPC
                  HR_EFF(TMPA,ICAP,KNOT) = TMP1(KNOT)
                  HR_LL(TMPA,ICAP,KNOT) = TMP2(KNOT)
               ENDDO
            ENDIF
         ENDDO
      ENDDO

      DO IX = 1 , NDREG
         DO ICAP = 1 , ECP_D_CAP
            ITYP = UPTTYP(ICAP)
            KCAP = 0
            IF (TEST_ITYP(ITYP,IX) .GT. 0 .AND. HR_KNOTS(IX,ICAP) .GT. 0) THEN

!              WRITE(6,4455) 'HRstuffb_INIT ',IX,ICAP, HR_KNOTS(IX,ICAP), 11,( (HR_EFF(IX,ICAP,KNOT),HR_LL(IX,ICAP,KNOT)),KNOT=1,MAX_KNOTS )
!4455          FORMAT(A15,1x,I4,1x,I4,1x,I4,1x,<MAX_KNOTS>(F8.3,1x,F8.3,1x))

               KCAP = ICAP
               DO JCAP = ICAP+1 , ECP_D_CAP
                  JTYP = UPTTYP(JCAP)
                  IF (JTYP .EQ. ITYP) THEN
                     IF (HR_KNOTS(IX,JCAP) .GT. 0) THEN
                        KCAP = JCAP

!                       WRITE(6,4455)'HRstuffb_INIT ',IX,KCAP, HR_KNOTS(IX,KCAP), 11,( (HR_EFF(IX,KCAP,KNOT),HR_LL(IX,KCAP,KNOT)),KNOT=1,MAX_KNOTS )

                     ELSE
                        HR_KNOTS(IX,JCAP) = HR_KNOTS(IX,KCAP)
                        DO KNOT = 1 , HR_KNOTS(IX,KCAP)
                           HR_EFF(IX,JCAP,KNOT) = HR_EFF(IX,KCAP,KNOT)
                           HR_LL(IX,JCAP,KNOT) = HR_LL(IX,KCAP,KNOT)
                        END DO

!                       WRITE(6,4455)'HRstuffb_ADDS ',IX,JCAP, HR_KNOTS(IX,JCAP), 11,( (HR_EFF(IX,JCAP,KNOT),HR_LL(IX,JCAP,KNOT)),KNOT=1,MAX_KNOTS )

                     END IF
                  ELSE
                     TEST_ITYP(ITYP,IX) = 0
                     EXIT
                  END IF
               END DO
            END IF
         END DO
      END DO

!     Read matrices used for mapping between States and EMM regions

      NM_ST_CODES = 0
      NM_UT_TYPES = 0
      UT_TYPE_CODES = '    '

      RET_CODE = RD$TBL(I_CNTL,'%SALES_INFO%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$I1(NM_UT_TYPES,1,1)
      RET_CODE = RD$I1(NM_ST_CODES,1,1)

      RET_CODE = RD$TBL(I_CNTL,'%UT_TYPEPCD%',NM_UT_TYPES,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(UT_TYPE_CODES,1,MX_TYPE)

      WRITE(18,5310) CURIRUN, CURIYR+1989, NM_ST_CODES, NM_UT_TYPES, (UT_TYPE_CODES(ICHK),ICHK=1,NM_UT_TYPES)
 5310 FORMAT(1X,"ST_RPS_UT_TYPES",4(":",I4),<NM_UT_TYPES>(":",A4))
      
      ST_CODES(0)='US'
      DO I=0,NM_ST_CODES
         WRITE(18,*)'STATES ',I,ST_CODES(I)
      ENDDO

      DO UT_TYPES = 1 , NM_UT_TYPES
         TEMP1 = 0.0
         SUM_SALES_ST = 0.0
         SUM_SALES_EMM = 0.0         

         DO I = 1, NM_ST_CODES
            IX = I
            IF (I .EQ. NM_ST_CODES) IX = 0

            DO ICHK = 1, MNUMNR
               IF (I .EQ. NM_ST_CODES) H_SALES(0,UT_TYPES,ICHK) =  H_SALES(NM_ST_CODES,UT_TYPES,ICHK)
               IF (UT_TYPES .EQ. NM_UT_TYPES) H_SALES(0,0,ICHK) = H_SALES(NM_ST_CODES,0,ICHK)
               IF (ICHK .LT. MNUMNR) SUM_SALES_ST(I) = SUM_SALES_ST(I) + H_SALES(IX,UT_TYPES,ICHK) 
               IF (I .LT. NM_ST_CODES) SUM_SALES_EMM(ICHK) = SUM_SALES_EMM(ICHK) + H_SALES(IX,UT_TYPES,ICHK) 
            ENDDO

            WRITE(18,5311) CURIRUN, CURIYR+1989, UT_TYPES, UT_TYPE_CODES(UT_TYPES), I, IX, ST_CODES(IX), (H_SALES(IX,UT_TYPES,ICHK),ICHK=1,MNUMNR), SUM_SALES_ST(I)
 5311       FORMAT(1X,"ST_RPS_HSALES_INFO",3(":",I4),":",A4,2(":",I4),":",A2,<MNUMNR>(":",F21.6),":",F21.6)

         ENDDO
         
         I = NM_ST_CODES
         IX = -1
         WRITE(18,5312) CURIRUN, CURIYR+1989, UT_TYPES, UT_TYPE_CODES(UT_TYPES), I, IX, ST_CODES(0), (SUM_SALES_EMM(ICHK),ICHK=1,MNUMNR), SUM_SALES_ST(NM_ST_CODES)
 5312    FORMAT(1X,"ST_RPS_HSALES_INFO",3(":",I4),":",A4,2(":",I4),":",A2,<MNUMNR>(":",F21.6),":",F21.6)

      ENDDO

      TEMP1 = 0.0
      SUM_SALES_ST = 0.0
      SUM_SALES_EMM = 0.0

      DO I = 1, NM_ST_CODES
         IX = I
         IF (I .EQ.NM_ST_CODES) IX=0
         DO ICHK = 1, MNUMNR
            IF (ICHK .LT. MNUMNR) SUM_SALES_ST(I) = SUM_SALES_ST(I) + H_SALES(IX,0,ICHK) 
            IF (I .LT. NM_ST_CODES) SUM_SALES_EMM(ICHK) = SUM_SALES_EMM(ICHK) + H_SALES(IX,0,ICHK)
         ENDDO

         WRITE(18,5311) CURIRUN, CURIYR+1989, 0, 'TOT ', I, IX, ST_CODES(IX), (H_SALES(IX,0,ICHK),ICHK=1,MNUMNR), SUM_SALES_ST(I)

      ENDDO

      I = NM_ST_CODES
      IX = 0
      WRITE(18,5312) CURIRUN, CURIYR+1989, 0, 'TOT ', I, IX, ST_CODES(IX), (SUM_SALES_EMM(ICHK),ICHK=1,MNUMNR), SUM_SALES_ST(NM_ST_CODES)

      TEMP1=0.0

!     Initialize State RPS Info

      NM_ST_RPS = 0
      ST_RPS_STnm = 0
      ST_RPS_SYR = 0
      ST_RPS_EYR = 0
      ST_RPS_POST = 0
      ST_RPS_PCAP_YR = 0
      ST_RPS_PCAP_TYP = 0
      ST_RPS_UT_NM = 0
      ST_RPS_UT_TYPE = 0
      ST_RPS_IMPORTS = 0
      ST_RPS_CHP_SW = 0
      ST_RPS_DG_SW = 0
      ST_RPS_EMM_MAP = 0
      ST_RPS_TRD = 0
      ST_RPS_TITLE = "                                        "
      ST_RPS_STcd = "  "
      ST_RPS_ID = " "
      ST_RPS_UT_PCNT = 0.0
      ST_RPS_PCAP_87 = 0.0
      ST_RPS_CHP = 0.0
      ST_RPS_DG = 0.0
      ST_RPS_COFIRE = 0.0
      ST_RPS_ECP_EX = 0.0
      ST_RPS_ECP_NW = 0.0
      ST_RPS_REQ = 0.0
      ST_RPS_TOLERANCE = 0.0

!     Read in state RPS tranche details, starting with the number of tranches

      NEW = .FALSE.
      FILENM='RPSTRANCHE'
      I_TRANCHE = FILE_MGR('O',FILENM,NEW)

      RET_CODE = RD$TBL(I_TRANCHE,'%ST_RPS_NUM%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$I1(NM_ST_RPS,1,1)
      RET_CODE = RD$I1(ST_RPS_SW,1,1)
      RET_CODE = RD$R81(ST_RPS_TOLERANCE,1,1)

      WRITE(18,5313) CURIRUN, CURIYR+1989, NM_ST_RPS, ST_RPS_TOLERANCE
 5313 FORMAT(1X,"NUMBER_ST_RPS_TRANCHES",3(":",I4),":",F21.6)

      DO I = 1, NM_ST_RPS

         WRITE(Tranche_CD,'(I2.2)') I
         STR=' '
         WRITE(STR,'(I1)') I

         TABLE_NAME = "%T" // Tranche_CD // "_STATE %"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,1,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(TEMPC1,1,1)
         RET_CODE = RD$C1(TEMPC2,1,1)
         RET_CODE = RD$I1(TEMPI1,1,1)
         RET_CODE = RD$I1(TEMPI2,1,1)
         RET_CODE = RD$I1(TEMPI3,1,1)
         RET_CODE = RD$I1(TEMPI4,1,1)
         RET_CODE = RD$C1(TEMPC7,1,1)
         RET_CODE = RD$C1(TEMPC3,1,1)

         ST_RPS_STcd(I)=TEMPC1
         DO IST = 1, NM_ST_CODES
            IF (ST_RPS_STcd(I) .EQ. ST_CODES(IST)) THEN
               ST_RPS_STnm(I) = IST
               EXIT
            END IF
         END DO

         ST_RPS_ID(I)=TEMPC2
         ST_RPS_SYR(I)=TEMPI1
         ST_RPS_EYR(I)=TEMPI2
         ST_RPS_POST(I)=TEMPI3
         ST_RPS_PCAP_YR(I)=TEMPI4
         IF (TRIM(TEMPC7) .EQ. 'NA') ST_RPS_PCAP_TYP(I)=0
         IF (TRIM(TEMPC7) .EQ. 'Real') ST_RPS_PCAP_TYP(I)=1
         IF (TRIM(TEMPC7) .EQ. 'Nominal') ST_RPS_PCAP_TYP(I)=2
         ST_RPS_TITLE(I)=TEMPC3

         WRITE(18,5314) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ST_RPS_Stcd(I), ST_RPS_STnm(I), ST_RPS_ID(I), ST_RPS_SYR(I), ST_RPS_EYR(I), ST_RPS_POST(I), ST_RPS_PCAP_YR(I), ST_RPS_PCAP_TYP(I)
 5314    FORMAT(1X,"ST_RPS_INFO_",A12,3(":",I4),":",A40,":",A2,":",I2,":",A1,5(":",I4))

         IF (IST .GT. NM_ST_CODES) THEN

            DO IST = 1, NM_ST_CODES

               WRITE(6,7314) CURIRUN, CURIYR+1989, I, ST_CODES(IST), ST_RPS_Stcd(I), TEMPC1, IST, ST_RPS_STnm(I)
 7314          FORMAT(1X,"IST greater than NM_ST_CODES",3(":",I4),3(":",A2),2(":",I2))

            END DO

            WRITE(6,6314) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ST_RPS_Stcd(I), ST_RPS_STnm(I), ST_RPS_ID(I), ST_RPS_SYR(I), ST_RPS_EYR(I), ST_RPS_POST(I), ST_RPS_PCAP_YR(I), ST_RPS_PCAP_TYP(I)
 6314       FORMAT(1X,"ST_RPS_OOPS_",A12,3(":",I4),":",A40,":",A2,":",I2,":",A1,5(":",I4))

            STOP
         END IF

!        Determine which EMM regions can directly serve a state RPS tranche

         TEMP1 = 0.0
         SUM_SALES_ST =0.0

         DO EMM_RG = 1 , UNRGNS
            IF (H_SALES(ST_RPS_STnm(I),0,EMM_RG) / H_SALES(ST_RPS_STnm(I),0,MNUMNR) .GT. ST_RPS_TOLERANCE) THEN
               ST_RPS_EMM_MAP(EMM_RG,I) = 1
               TEMP1(ST_RPS_STnm(I),EMM_RG) = H_SALES(ST_RPS_STnm(I),0,EMM_RG) / H_SALES(ST_RPS_STnm(I),0,MNUMNR)
               SUM_SALES_ST(ST_RPS_STnm(I)) = 1.001
            END IF
         END DO

!        When state EMM_RG share to total state sales ratio falls below tolerance for all regions then pick the EMM_RG with most of the state sales

         IF (SUM_SALES_ST(ST_RPS_STnm(I)) .LT. 1.0) THEN
            TEMP4 = 0.0
            TEMPI1 = 0
            DO EMM_RG = 1 , UNRGNS
               IF (TEMP1(ST_RPS_STnm(I),EMM_RG) .GT. TEMP4) THEN
                  TEMPI1 = EMM_RG
                  TEMP4 = TEMP1(ST_RPS_STnm(I),EMM_RG)
               END IF
            END DO
            IF (TEMPI1 .GT. 0) THEN
               ST_RPS_EMM_MAP(TEMPI1,I) = 1
            ELSE
               STOP
            END IF
         END IF

         TEMPI1 = 0
         TEMP1 = 0.0
         TEMP4 = 0.0
         SUM_SALES_ST =0.0

         DO EMM_RG = 1 , UNRGNS
            IF (H_SALES(ST_RPS_STnm(I),0,EMM_RG) .GT. 0.0) THEN

               WRITE(18,8314) CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ST_RPS_Stcd(I), ST_RPS_STnm(I), ST_RPS_ID(I), EMM_RG, ST_RPS_EMM_MAP(EMM_RG,I), &
                  H_SALES(ST_RPS_STnm(I),0,EMM_RG), H_SALES(ST_RPS_STnm(I),0,MNUMNR), ST_RPS_TOLERANCE
 8314          FORMAT(1X,"ST_RPS_EMM_MAP",3(":",I4),":",A40,":",A2,":",I2,":",A1,2(":",I4),3(":",F21.6))

            END IF
         END DO

         TEMPC1=''
         TEMPC2=''
         TEMPI1=0
         TEMPI2=0
         TEMPI3=0
         TEMPI4=0
         TEMPI5=0
         TEMPC3=''

         TABLE_NAME = "%T" // Tranche_CD // "_IMPORT%"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,1,UF_DBG,UF_MSG)
         RET_CODE = RD$I1(TEMPI1,1,1)
         ST_RPS_IMPORTS(I)=TEMPI1

         WRITE(18,5315) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ST_RPS_IMPORTS(I)
 5315    FORMAT(1X,"ST_RPS_INFO_",A12,3(":",I4),":",A40,":",I4)

         TEMPI1=0

         TABLE_NAME = "%T" // Tranche_CD // "_CHP_NM%"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,1,UF_DBG,UF_MSG)
         RET_CODE = RD$I1(TEMPI1,1,1)
         RET_CODE = RD$I1(TEMPI2,1,1)
         N_CHP(I)=TEMPI1
         ST_RPS_CHP_SW(I)=TEMPI2

         TEMPI1=0
         TEMPI2=0
         TMP_TC_CD = '  '

         TABLE_NAME = "%T" // Tranche_CD // "_CHP_PT%"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,N_CHP(I),UF_DBG,UF_MSG)
         RET_CODE = RD$C1(TMP_TC_CD,1,N_CHP(I))
         RET_CODE = RD$R81(TEMP2,1,TC_FUELS)

         IF (N_CHP(I) .EQ. 1 .AND. TRIM(TMP_TC_CD(1)) .EQ. 'NA') THEN
            N_CHP(I) = 0
         ELSE
            DO ICHK = 1, N_CHP(I)
               DO JTYP = 1, TC_FUELS
                  IF (TRIM(TMP_TC_CD(ICHK)) .EQ. TRIM(TC_FUEL_CODES(JTYP))) THEN
                     ST_RPS_CHP(JTYP,I) = TEMP2(ICHK)

                     WRITE(18,5316) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ST_RPS_CHP_SW(I), ICHK, JTYP, TRIM(TC_FUEL_CODES(JTYP)), ST_RPS_CHP(JTYP,I)
 5316                FORMAT(1X,"ST_RPS_INFO_",A12,3(":",I4),":",A40,3(":",I4),":",A2,":",F21.6)

                     EXIT
                  ENDIF
               ENDDO
               IF (JTYP .GT. TC_FUELS) THEN

                  WRITE(6,6316) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), TEMP2(ICHK), ICHK, JTYP, TRIM(TMP_TC_CD(ICHK)), TEMP2(ICHK)
 6316             FORMAT(1X,"ST_RPS_OOPS_",A12,3(":",I4),":",A40,3(":",I4),":",A2,":",F21.6)
                  STOP

               END IF
            ENDDO
         END IF

         TEMP2 = 0.0
         TMP_TC_CD = '  '
         N_CHP=0

         TABLE_NAME = "%T" // Tranche_CD // "_DG_NUM%"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,1,UF_DBG,UF_MSG)
         RET_CODE = RD$I1(TEMPI1,1,1)
         RET_CODE = RD$I1(TEMPI2,1,1)

         N_DG(I)=TEMPI1

         ST_RPS_DG_SW(I)=TEMPI2

         TEMPI1=0
         TEMPI2=0

         TABLE_NAME = "%T" // Tranche_CD // "_DG_PT %"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,N_DG(I),UF_DBG,UF_MSG)
         RET_CODE = RD$C1(TMP_TC_CD,1,N_DG(I))
         RET_CODE = RD$R81(TEMP2,1,N_DG(I))

         IF (N_DG(I) .EQ. 1 .AND. TRIM(TMP_TC_CD(1)) .EQ. 'NA') THEN
            N_DG(I) = 0
         ELSE
            DO ICHK = 1, N_DG(I)
               DO JTYP = 1, TC_FUELS
                  IF (TRIM(TMP_TC_CD(ICHK)) .EQ. TRIM(TC_FUEL_CODES(JTYP))) THEN
                     ST_RPS_DG(JTYP,I) = TEMP2(ICHK)

                     WRITE(18,5317) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ST_RPS_DG_SW(I), ICHK, JTYP, TRIM(TC_FUEL_CODES(JTYP)), ST_RPS_DG(JTYP,I)
 5317                FORMAT(1X,"ST_RPS_INFO_",A12,3(":",I4),":",A40,3(":",I4),":",A2,":",F21.6)

                     EXIT
                  END IF
               ENDDO
               IF (JTYP .GT. TC_FUELS) THEN

                  WRITE(6,6317) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), TEMP2(ICHK), ICHK, JTYP, TRIM(TMP_TC_CD(ICHK)), TEMP2(ICHK)
 6317             FORMAT(1X,"ST_RPS_OOPS_",A12,3(":",I4),":",A40,3(":",I4),":",A2,":",F21.6)
                  STOP

               END IF
            ENDDO
         ENDIF

         TEMP2 = 0.0
         TMP_TC_CD = '  '
         N_DG=0

         TABLE_NAME = "%T" // Tranche_CD // "_COFIRE%"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,1,UF_DBG,UF_MSG)
         RET_CODE = RD$R81(TEMP4,1,1)
         ST_RPS_COFIRE(I)=TEMP4

         WRITE(18,5318) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ST_RPS_COFIRE(I)
 5318    FORMAT(1X,"ST_RPS_INFO_",A12,3(":",I4),":",A40,":",F21.6)

         TEMP2 = 0.0

         TABLE_NAME = "%T" // Tranche_CD // "_EL_NUM%"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,1,UF_DBG,UF_MSG)
         RET_CODE = RD$I1(TEMPI1,1,1)
         N_QUAL(I)=TEMPI1

         TEMPI1=0
         TMP_PT_CD = '  '

         TABLE_NAME = "%T" // Tranche_CD // "_EL_PT %"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,N_QUAL(I),UF_DBG,UF_MSG)
         RET_CODE = RD$C1(TMP_PT_CD,1,N_QUAL(I))
         RET_CODE = RD$R81(TEMP2,1,N_QUAL(I))
         RET_CODE = RD$R81(TEMP3,1,N_QUAL(I))

         DO ICHK=1,N_QUAL(I)
            DO JTYP=1,ECP_D_CAP
               IF (TRIM(TMP_PT_CD(ICHK)) .EQ. TRIM(UPLNTCD(JTYP))) THEN
                  ST_RPS_ECP_EX(JTYP,I)= TEMP2(ICHK)
                  ST_RPS_ECP_NW(JTYP,I)= TEMP3(ICHK)

                  WRITE(18,5319) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ICHK, JTYP, UPLNTCD(JTYP), ST_RPS_ECP_EX(JTYP,I), ST_RPS_ECP_NW(JTYP,I)
 5319             FORMAT(1X,"ST_RPS_INFO_",A12,3(":",I4),":",A40,2(":",I4),":",A2,2(":",F21.6))

                  EXIT
               ENDIF
            ENDDO
            IF (JTYP .GT. ECP_D_CAP) THEN

               WRITE(6,6319) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ICHK, JTYP, TRIM(TMP_PT_CD(ICHK)), TEMP2(ICHK), TEMP3(ICHK)
 6319          FORMAT(1X,"ST_RPS_OOPS_",A12,3(":",I4),":",A40,2(":",I4),":",A2,2(":",F21.6))
               STOP

            END IF
         ENDDO
         TEMP2=0.0
         TEMP3=0.0
         N_QUAL = 0
         TMP_PT_CD = '  '

         TABLE_NAME = "%T" // Tranche_CD // "_RPS_NM%"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,1,UF_DBG,UF_MSG)
         RET_CODE = RD$I1(TEMPI1,1,1)
         RET_CODE = RD$I1(TEMPI2,1,1)
         N_RQ(I) = TEMPI1
         ST_RPS_UT_NM(I) = TEMPI2

         TABLE_NAME = "%T" // Tranche_CD // "_RPS_UT%"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,ST_RPS_UT_NM(I),UF_DBG,UF_MSG)
         RET_CODE = RD$C1(TEMPC4,1,MX_TYPE)
         RET_CODE = RD$R81(TEMP2,1,MX_ST_RPS)

         DO ICHK = 1, ST_RPS_UT_NM(I)
            DO JTYP = 1, NM_UT_TYPES
               IF (TRIM(TEMPC4(ICHK)) .EQ. TRIM(UT_TYPE_CODES(JTYP))) THEN
                  ST_RPS_UT_TYPE(ICHK,I) = JTYP
                  ST_RPS_UT_PCNT(ICHK,I) = TEMP2(ICHK)

                  WRITE(18,5320) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_UT_NM(I), ST_RPS_UT_PCNT(ICHK,I), ICHK, JTYP, TRIM(TEMPC4(ICHK)), TRIM(UT_TYPE_CODES(JTYP))
 5320             FORMAT(1X,"ST_RPS_INFO_",A12,4(":",I4),":",F21.6,2(":",I4),2(":",A4))

                  EXIT
               ENDIF
            ENDDO
            IF (JTYP .GT. NM_UT_TYPES) THEN

               WRITE(6,6320) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_UT_NM(I), TEMP2(ICHK), ICHK, TRIM(TEMPC4(ICHK))
 6320          FORMAT(1X,"ST_RPS_OOPS_",A12,4(":",I4),":",F21.6,1(":",I4),1(":",A4))

               STOP
            END IF
         ENDDO

         TEMP2=0.0
         ST_TMP = 0.0
         ST_TMP_YR = 0.0

         TMP_UT_NUM = ST_RPS_UT_NM(I)
         IF (ST_RPS_PCAP_YR(I) .GT. 0) THEN
            TMP_UT_NUM = ST_RPS_UT_NM(I) + 1
         END IF
         TABLE_NAME = "%T" // Tranche_CD // "_RPS_RQ%"
         RET_CODE = RD$TBL(I_TRANCHE,TABLE_NAME,N_RQ(I),UF_DBG,UF_MSG)
         RET_CODE = RD$I1(TEMPI6,1,N_RQ(I))
         RET_CODE = RD$R82(ST_TMP,1,1,TMP_UT_NUM,MNUMYR+ECP_D_FPH,MX_TYPE+1)

         DO JTYP = 1, TMP_UT_NUM
            DO ICHK = 1, N_RQ(I)
               IX = TEMPI6(ICHK) - UHBSYR
               ST_TMP_YR(IX,JTYP) = ST_TMP(ICHK,JTYP)
            END DO
         END DO

         DO ICHK = 1, N_RQ(I)
            IX = TEMPI6(ICHK) - UHBSYR

            WRITE(18,5321) TABLE_NAME, CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ICHK, TEMPI6(ICHK), IX, (ST_TMP_YR(IX,JTYP), ST_TMP(ICHK,JTYP), JTYP = 1 , TMP_UT_NUM)
 5321       FORMAT(1X,"ST_RPS_INFO_",A12,3(":",I4),":",A40,3(":",I4),<TMP_UT_NUM>(2(":",F21.6)))

         END DO

         DO JTYP = 1, TMP_UT_NUM
            DO ICHK = 2, N_RQ(I)
               Delta_RQ = (ST_TMP_YR((TEMPI6(ICHK)-UHBSYR),JTYP) - ST_TMP_YR((TEMPI6(ICHK-1)-UHBSYR),JTYP)) / (TEMPI6(ICHK) - TEMPI6(ICHK-1))

               WRITE(18,6321) CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ICHK-1, TEMPI6(ICHK-1), JTYP, ST_TMP_YR((TEMPI6(ICHK-1)-UHBSYR),JTYP), Delta_RQ

               DO JYR = TEMPI6(ICHK-1)-UHBSYR+1 , TEMPI6(ICHK)-UHBSYR-1
                  ST_TMP_YR(JYR,JTYP) = ST_TMP_YR(JYR-1,JTYP) + Delta_RQ

                  WRITE(18,6321) CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ICHK, JYR+1989, JTYP, ST_TMP_YR(JYR,JTYP), Delta_RQ
 6321             FORMAT(1X,"ST_RPS_INFO_REQ_FULL",3(":",I4),":",A40,3(":",I4),2(":",F21.6))

               END DO
            END DO
         END DO

         IF (ST_RPS_POST(I) .EQ. 1) THEN
            DO JTYP = 1, TMP_UT_NUM
               DO JYR = TEMPI6(N_RQ(I))-UHBSYR+1 , MNUMYR+ECP_D_FPH
                  ST_TMP_YR(JYR,JTYP) = ST_TMP_YR(JYR-1,JTYP)
               END DO
            END DO
         END IF

         DO JYR = TEMPI6(1)-UHBSYR , MNUMYR+ECP_D_FPH
            DO EMM_RG = 1, UNRGNS
               DO JTYP = 1 , ST_RPS_UT_NM(I)
                  ITYP = ST_RPS_UT_TYPE(JTYP,I)
                  ST_RPS_REQ(EMM_RG,I,JYR) = ST_RPS_REQ(EMM_RG,I,JYR) + ST_TMP_YR(JYR,JTYP) * ST_RPS_UT_PCNT(JTYP,I) * H_SALES(ST_RPS_STnm(I),ITYP,EMM_RG)

                  IF (H_SALES(ST_RPS_STnm(I),ITYP,EMM_RG) .GT. 0.0 .AND. JYR .LE. 2030 - UHBSYR) THEN
                     WRITE(18,5322) CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ST_RPS_STcd(I), ST_RPS_ID(I), ST_RPS_STnm(I), JYR+UHBSYR, EMM_RG, JTYP, ITYP, &
                        ST_TMP_YR(JYR,JTYP), ST_RPS_UT_PCNT(JTYP,I), H_SALES(ST_RPS_STnm(I),ITYP,EMM_RG)
 5322                FORMAT(1X,"ST_RPS_REQ_INFO",3(":",I4),":",A40,":",A2,":",A1,5(":",I4),3(":",F21.6))
                  END IF

               END DO
               ST_RPS_REQ(EMM_RG,I,JYR) = ST_RPS_REQ(EMM_RG,I,JYR) / H_SALES(0,0,EMM_RG)

               IF (H_SALES(ST_RPS_STnm(I),0,EMM_RG) .GT. 0.0 .AND. JYR .EQ. 2020 - UHBSYR) THEN
                  WRITE(18,5323) CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ST_RPS_STcd(I), ST_RPS_ID(I), ST_RPS_STnm(I), JYR+UHBSYR, EMM_RG,  &
                     ST_RPS_REQ(EMM_RG,I,JYR), ST_RPS_REQ(EMM_RG,I,JYR) * H_SALES(0,0,EMM_RG), H_SALES(0,0,EMM_RG), H_SALES(ST_RPS_STnm(I),0,EMM_RG)
 5323             FORMAT(1X,"ST_RPS_REQ_PCNT",3(":",I4),":",A40,":",A2,":",A1,3(":",I4),4(":",F21.6))
               END IF

            END DO
         END DO

         IF (ST_RPS_PCAP_YR(I) .GT. 0) THEN
            DO JYR = TEMPI6(1)-UHBSYR , MNUMYR+ECP_D_FPH
               IF (ST_RPS_PCAP_TYP(I) .EQ. 1) THEN                                                     ! REAL
                  ST_RPS_PCAP_87(I,JYR) = ST_TMP_YR(JYR,TMP_UT_NUM) / UPGNPD(ST_RPS_PCAP_YR(I)-UHBSYR) * 1000.0  !convert to $/MWh
               ELSE IF (ST_RPS_PCAP_TYP(I) .EQ. 2) THEN                                                ! NOMINAL
                  ST_RPS_PCAP_87(I,JYR) = ST_TMP_YR(JYR,TMP_UT_NUM) / UPGNPD(JYR) * 1000.0   !convert to $/MWh
               END IF

               IF (JYR .EQ. 2020 - UHBSYR) THEN
                  WRITE(18,5324) CURIRUN, CURIYR+1989, I, ST_RPS_TITLE(I), ST_RPS_STcd(I), ST_RPS_ID(I), ST_RPS_STnm(I), JYR+UHBSYR, ST_RPS_PCAP_TYP(I), ST_RPS_PCAP_YR(I),  &
                     ST_RPS_PCAP_87(I,JYR), ST_TMP_YR(JYR,TMP_UT_NUM), UPGNPD(ST_RPS_PCAP_YR(I)-UHBSYR), UPGNPD(JYR)
 5324             FORMAT(1X,"ST_RPS_PCAP_INFO",3(":",I4),":",A40,":",A2,":",A1,4(":",I4),4(":",F21.6))
               END IF

            END DO
         END IF

         TEMPC4=''
         TEMP2=0.0
         TEMPI6=0
         TEMP1=0.0

      ENDDO

      NEW = .FALSE.
      FILENM='RPSTRANCHE'
      I_TRANCHE = FILE_MGR('C',FILENM,NEW)

      IF (ST_RPS_SW .EQ. 0) NM_ST_RPS = 0

      RET_CODE = RD$TBL(I_CNTL,'%RNW WACC SW%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_WACC,1,1)        ! WACC Adjustment Switch for Tax Credits (0=No,1=Yes)
!
!     READ IN STATE ZEC SWITCHES
!
      RET_CODE = RD$TBL(I_CNTL,'%ZEC SWITCH%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_ZECREV,1,1)      ! ZEC Activation (0=No, 1=neg net revs only, 2=All)
      RET_CODE = RD$I1(USW_ZECNET,1,1)      ! Net Revenue Method (1=w/capacity pymt, 2=w/o capacity pymt)
      RET_CODE = RD$I1(USW_ZECSUB,1,1)      ! Subsidy Source (1=ECP then EFD, 2=ECP only, 3=EFD only)
!
!     READ IN STATE ZEC PERIOD, IF ANY
!
      RET_CODE = RD$TBL(I_CNTL,'%STATE ZECS%',UNSTAS,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(URG_ZEC,1,UNSTAS)                   ! Region for State ZEC (0=all)
      RET_CODE = RD$I1(UY1_ZEC,1,UNSTAS)                   ! Start Year for State ZEC (0=no ZEC)
      RET_CODE = RD$I1(UYR_ZEC,1,UNSTAS)                   ! End Year for State ZEC (0=no ZEC)
      RET_CODE = RD$I1(URG_ZECYRD,1,UNSTAS)                ! Year $ (if any) for State ZEC Maximum (0=NA)
      RET_CODE = RD$R1(URG_ZECMAX,1,UNSTAS)                ! Maximum Annual Subsidy (if any) for State ZEC (0=no maximum)
      RET_CODE = RD$I1(UYR_ZECRNW,1,UNSTAS)                ! Number of Years to Renew Subsidy (if any) for State ZEC
      RET_CODE = RD$I1(URG_ZECPRC,1,UNSTAS)                ! Flag to indicate whether to recover costs in prices: 0=no (federal infrastructure bill), 1=yes (state program)

      DO I = 1 , UNSTAS
         IF (URG_ZECYRD(I) .GT. 0)URG_ZECMAX(I) = URG_ZECMAX(I) / UPGNPD(URG_ZECYRD(I) - UHBSYR)
!        print *,'!zecin',i,ustnme(i),urg_zec(i),uyr_zec(i),urg_zecyrd(i),urg_zecmax(i)
      END DO
!     Do i = 1 , UNSTAS
!        write(6,3423) curiyr+1989,i,ustnme(i),urg_zec(i),uyr_zec(i)
!3423 format(1h ,'!zec',i4,i3,a3,i5,i5)
!     end do
!
!     READ IN NEGATIVE REVENUE SWITCHES
!
      RET_CODE = RD$TBL(I_CNTL,'%REV SWITCH%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_NRVTYP,1,1)      ! (1=current year revenues/expenses, 2=lag year revenues/expenses)
      RET_CODE = RD$I1(USW_NRVREV,1,1)      ! (1=Include capacity payment, 2=exclude capacity payment)
!
!     READ IN STATE ACE SWITCH
!
      RET_CODE = RD$TBL(I_CNTL,'%ACE SWITCH%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_ACE,1,1)      ! ACE Activation (0=No, 1=use actual HRI inputs from ecpdaty, 2=use incremental HRI inputs from ecpdaty)
!
!     READ IN STATE ACE IMPLEMENTATION YEAR, IF ANY
!
      RET_CODE = RD$TBL(I_CNTL,'%STATE ACES%',UNSTAS,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(UYR_ACE,1,UNSTAS)                   ! Year for State ACE (0=no ACE)
!     print *,'!ace',usw_ace,uyr_ace(1),uyr_ace(49)
!
!     READ ANNUAL NUCLEAR FUEL COST 
!      Costs are read in as mills/kwh converted to $/mmbtu in historical read routine with historical heat rates.
!
      RET_CODE = RD$TBL(I_CNTL,'%NUCLEAR FLCSTS%',MNUMYR,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(NUCDYR,1,MNUMYR)                           ! dollar year of nuclear fuel costs
      RET_CODE = RD$R1(URCOST,1,MNUMYR)                           ! nuclear fuel costs
!
      DO IYR = 1 , MNUMYR
!        convert nuclear costs to 1987$
         UPURELN(MNUMNR,IYR) = URCOST(IYR) / MC_JPGDP(NUCDYR(IYR)-UHBSYR)
      END DO
	  
!     NUCLEAR RETIREMENT SMOOTHING
!     SWITCH TO MAKE NUCLEAR ECP UTILIZATION FOR RETIREMENT SMOOTHED OVER CYCLES
!     (0=NO SMOOTH,2=SMOOTHING USING CURRENT AND 1 PREVIOUS CYCLE, 3=SMOOTHING USING CURRENT AND 2 PREVIOUS CYCLES)
!
      RET_CODE = RD$TBL(I_CNTL,'%NUCLEAR RETIREMENT SMOOTHING%',1,UF_DBG,UF_MSG)
      RET_CODE = RD$C1(DUMMY,1,DMSZ)
      RET_CODE = RD$I1(USW_NUCRETSM,1,1)						! nuclear utilization switch for smoothing

!     READ IN GRID RESILIENCE NUMBER OF SOURCE INPUTS (I.E. IHS + PJM)
    !#######################################  

      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
        
      allocate (col(5))
      call sqlite3_column_query( col(1), 'USW_GRD', sqlite_int )
      call sqlite3_column_query( col(2), 'GRD_TYP', sqlite_int )
      call sqlite3_column_query( col(3), 'GRD_CASN', sqlite_int )
      call sqlite3_column_query( col(4), 'GRD_CASD', sqlite_int )
      call sqlite3_column_query( col(5), 'GRD_FACN', sqlite_int )
      call sqlite3_prepare_select( db, 'V_EMM_CNTL_GRIDRES_SW', col, stmt)
      
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt, col, finished )
      if ( finished ) exit
      
      call sqlite3_get_column( col(1), USW_GRD)
      call sqlite3_get_column( col(2), GRD_TYP)
      call sqlite3_get_column( col(3), GRD_CASN)
      call sqlite3_get_column( col(4), GRD_CASD)
      call sqlite3_get_column( col(5), GRD_FACN)
 !     Write(6,*)'grid switches ', USW_GRD, GRD_TYP, GRD_CASN, GRD_CASD, GRD_FACN
      
      end do
      deallocate ( col )

      allocate (col2(3))
      call sqlite3_column_query( col2(1), 'GRDSRC', sqlite_int )
      call sqlite3_column_query( col2(2), 'GRD_SRCC', sqlite_char )
      call sqlite3_column_query( col2(3), 'GRD_SRCN', sqlite_char )
      call sqlite3_prepare_select( db, 'V_EMM_CNTL_GRIDRES_SRC', col2, stmt2)
      
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt2, col2, finished )
      if ( finished ) exit
      
      call sqlite3_get_column( col2(1), I)
      call sqlite3_get_column( col2(2), GRD_SRCC(I))
      call sqlite3_get_column( col2(3), GRD_SRCN(I))
!      Write(6,*)'grid src ',I,GRD_SRCC(I), GRD_SRCN(I)
      end do
      deallocate ( col2 )

      allocate (col3(5))
      call sqlite3_column_query( col3(1), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col3(2), 'GRDSRCN', sqlite_char )
      call sqlite3_column_query( col3(3), 'GRDFACT', sqlite_int )
      call sqlite3_column_query( col3(4), 'GRDFACT_DESC', sqlite_char )
      call sqlite3_column_query( col3(5), 'GRD_WGTS', sqlite_real )
      call sqlite3_prepare_select( db, 'V_EMM_CNTL_GRIDRES_WGT', col3, stmt3)
      
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt3, col3, finished )
      if ( finished ) exit
      call sqlite3_get_column( col3(1), I)
      call sqlite3_get_column( col3(2), tmp_GRD_SRCN)
      call sqlite3_get_column( col3(3), K)
       if (trim(tmp_GRD_SRCN) .eq. 'IHS') then 
           j = 1
        elseif (trim(tmp_GRD_SRCN) .eq. 'PJM') then 
           j = 2
        elseif (trim(tmp_GRD_SRCN) .eq. 'GRID STRATEGIES') then 
           j = 3
        elseif (trim(tmp_GRD_SRCN) .eq. 'GRIDLAB ') then 
           j = 4           
        endif            
      call sqlite3_get_column( col3(4), tmp_desc)
      call sqlite3_get_column( col3(5), GRD_WGTS(K,J,I))
!      WRITE(6,*)'grd wgts ', I,  tmp_GRD_SRCN, k, J,tmp_desc, GRD_WGTS(K,J,I)
      end do
      deallocate ( col3 )

      allocate (col4(4))
      call sqlite3_column_query( col4(1), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col4(2), 'GRDSRCN', sqlite_char )
      call sqlite3_column_query( col4(3), 'YEAR', sqlite_int )
      call sqlite3_column_query( col4(4), 'GRD_TGTS', sqlite_real )
      call sqlite3_prepare_select( db, 'V_EMM_CNTL_GRIDRES_TGT', col4, stmt4)
      
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt4, col4, finished )
      if ( finished ) exit     
      call sqlite3_get_column( col4(1), I)
      call sqlite3_get_column( col4(2), tmp_GRD_SRCN)
      call sqlite3_get_column( col4(3), K)      
       if (trim(tmp_GRD_SRCN) .eq. 'IHS') then 
           j = 1
        elseif (trim(tmp_GRD_SRCN) .eq. 'PJM') then 
           j = 2
        elseif (trim(tmp_GRD_SRCN) .eq. 'GRID STRATEGIES') then 
           j = 3
        elseif (trim(tmp_GRD_SRCN) .eq. 'GRIDLAB ') then 
           j = 4 
        endif
      call sqlite3_get_column( col4(4), GRD_TGTX(K-1989,J,I))
!      WRITE(6,*)'GRD TGT ',I, tmp_GRD_SRCN, k-1989,j, GRD_TGTX(K-1989,j,i)
      end do
      deallocate ( col4 )
      
     allocate (col6(16))
     call sqlite3_column_query( col6(1), 'EMM_REG', sqlite_int )
     call sqlite3_column_query( col6(2), 'GRDSRCN', sqlite_char )
     call sqlite3_column_query( col6(3), 'ECP_CD', sqlite_char )
     call sqlite3_column_query( col6(4), 'GRD_RATED', sqlite_real )
     call sqlite3_column_query( col6(5), 'GRD_RATRC', sqlite_real )
     call sqlite3_column_query( col6(6), 'GRD_RATRG', sqlite_real )
     call sqlite3_column_query( col6(7), 'GRD_RATGS', sqlite_real )
     call sqlite3_column_query( col6(8), 'GRD_RATSC', sqlite_real )
     call sqlite3_column_query( col6(9), 'GRD_RATNI', sqlite_real )
     call sqlite3_column_query( col6(10), 'GRD_RATVC', sqlite_real )
     call sqlite3_column_query( col6(11), 'GRD_RATFC', sqlite_real )
     call sqlite3_column_query( col6(12), 'GRD_RATEF', sqlite_real )
     call sqlite3_column_query( col6(13), 'GRD_RATOE', sqlite_real )
     call sqlite3_column_query( col6(14), 'GRD_RATD1', sqlite_real )
     call sqlite3_column_query( col6(15), 'GRD_RATD2', sqlite_real )
     call sqlite3_column_query( col6(16), 'GRD_RATD3', sqlite_real )
     call sqlite3_prepare_select( db, 'V_EMM_CNTL_GRIDRES_RAT', col6, stmt6)
     
     ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
     do
     call sqlite3_next_row( stmt6, col6, finished )
     
     if ( finished ) exit
     
     call sqlite3_get_column( col6(1), I)
     call sqlite3_get_column( col6(2), tmp_GRD_SRCN)
            if (trim(tmp_GRD_SRCN) .eq. 'IHS') then 
                j = 1
             elseif (trim(tmp_GRD_SRCN) .eq. 'PJM') then 
                j = 2
             elseif (trim(tmp_GRD_SRCN) .eq. 'GRID STRATEGIES') then 
                j = 3
             elseif (trim(tmp_GRD_SRCN) .eq. 'GRIDLAB ') then 
                j = 4 
             endif
     call sqlite3_get_column( col6(3), tmp_GRD_CD)
     
     DO K1=1,ECP_D_CAP
       IF (trim(UPLNTCD(K1)).eq.trim(tmp_GRD_CD)) K=K1
     ENDDO
        
     call sqlite3_get_column( col6(4), GRD_RATED(K,J,I))
     call sqlite3_get_column( col6(5), GRD_RATRC(K,J,I))
     call sqlite3_get_column( col6(6), GRD_RATRG(K,J,I))
     call sqlite3_get_column( col6(7), GRD_RATGS(K,J,I))
     call sqlite3_get_column( col6(8), GRD_RATSC(K,J,I))
     call sqlite3_get_column( col6(9), GRD_RATNI(K,J,I))
     call sqlite3_get_column( col6(10), GRD_RATVC(K,J,I))
     call sqlite3_get_column( col6(11), GRD_RATFC(K,J,I))
     call sqlite3_get_column( col6(12), GRD_RATEF(K,J,I))
     call sqlite3_get_column( col6(13), GRD_RATOE(K,J,I))
     call sqlite3_get_column( col6(14), GRD_RATD1(K,J,I))
     call sqlite3_get_column( col6(15), GRD_RATD2(K,J,I))
     call sqlite3_get_column( col6(16), GRD_RATD3(K,J,I))
!     WRITE(6,*)'GRD PRAT ',I,tmp_GRD_SRCN,j,k,GRD_RATED(K,J,I),GRD_RATRC(K,J,I)
    
     end do
     deallocate ( col6 )

      
      

      call sqlite3_close( db )
!######################################################

!     GRID RESILIENCE CODES
!
      DO I = 1 , GRD_CASN
         WRITE(GRD_SRCC(I),'(I1)') I
      END DO
!
!     IDENTIFY INDEX FOR GRID RESILIENCE SOURCE INPUTS
!
      IF (USW_GRD .EQ. 0)THEN
         GRD_SRCI = GRD_CASD
      ELSE
         GRD_SRCI = USW_GRD
      END IF

      GRD_TGTS = 0.0

      DO I = 1 , GRD_CASN
!
         DO ITYP = 1 , GRD_FACN
!           IF (I .EQ. GRD_SRCI)THEN
               IF (ITYP .EQ. 1)THEN
                  GRD_RATS = 0.0
                  GRD_RATW = 0.0
               END IF
               DO IR = 1 , UNRGNS
                  IF (GRD_WGTS(ITYP,I,IR) .GT. 0.0)THEN
                     DO IP = 1 , ECP_D_CAP
                           IF (ITYP.EQ.1)GRD_RATP(IP,IR)=GRD_RATED(IP,I,IR)
                           IF (ITYP.EQ.2)GRD_RATP(IP,IR)=GRD_RATRC(IP,I,IR)
                           IF (ITYP.EQ.3)GRD_RATP(IP,IR)=GRD_RATRG(IP,I,IR)
                           IF (ITYP.EQ.4)GRD_RATP(IP,IR)=GRD_RATGS(IP,I,IR)
                           IF (ITYP.EQ.5)GRD_RATP(IP,IR)=GRD_RATSC(IP,I,IR)
                           IF (ITYP.EQ.6)GRD_RATP(IP,IR)=GRD_RATNI(IP,I,IR)
                           IF (ITYP.EQ.7)GRD_RATP(IP,IR)=GRD_RATVC(IP,I,IR)
                           IF (ITYP.EQ.8)GRD_RATP(IP,IR)=GRD_RATFC(IP,I,IR)
                           IF (ITYP.EQ.9)GRD_RATP(IP,IR)=GRD_RATEF(IP,I,IR)
                           IF (ITYP.EQ.10)GRD_RATP(IP,IR)=GRD_RATOE(IP,I,IR)
                           IF (ITYP.EQ.11)GRD_RATP(IP,IR)=GRD_RATD1(IP,I,IR)
                           IF (ITYP.EQ.12)GRD_RATP(IP,IR)=GRD_RATD2(IP,I,IR)
                           IF (ITYP.EQ.13)GRD_RATP(IP,IR)=GRD_RATD3(IP,I,IR)
                        IF (GRD_RATP(IP,IR) .GE. 0.0)THEN
                           GRD_RATS(IP,IR) = GRD_RATS(IP,IR) + GRD_WGTS(ITYP,I,IR) * GRD_RATP(IP,IR)
                           GRD_RATW(IP,IR) = GRD_RATW(IP,IR) + GRD_WGTS(ITYP,I,IR)
                        END IF
                     END DO
                  END IF
               END DO
!           END IF
         END DO
!
            DO IR = 1 , UNRGNS
               DO IP = 1 , ECP_D_CAP
!              ALL SOURCES
                  IF (GRD_RATW(IP,IR) .GT. 0.0)THEN
       ! write(6,*), 'grdp debug',I,ITYP,IR,IP,GRD_RATS(IP,IR),GRD_RATW(IP,IR),GRD_RATW(ITYP,IR)
                  GRD_RATSA(IP,IR,I) = GRD_RATS(IP,IR) / GRD_RATW(ITYP,IR)
                  ELSE
                  GRD_RATSA(IP,IR,I) = -1.0
                  END IF
               END DO
            END DO
            WRITE(13,2000)
 2000       FORMAT(1h ,'!grdp')
            WRITE(13,2100) GRD_SRCN(I)
 2100       FORMAT(1h ,'!grdp',T25,'GRID RESILIENCE FACTORS BY PLANT AND REGION -- SOURCE = ',a15)
         WRITE(13,2200) (REG(IRG),IRG = 1 , UNRGNS)
 2200 FORMAT(1h ,'!grdp',2x,'Plant Type  ',1x,22I6)
!           DO IP = 1 , ECP_D_CAP
               WRITE(13,3000) (GRD_RATSA(WIPC,IRG,I),IRG = 1 , UNRGNS)
 3000          FORMAT(1h ,'!grdp',2x,'Coal Steam  ',1x,22f6.2)
               WRITE(13,3100) (GRD_RATSA(WIST,IRG,I),IRG = 1 , UNRGNS)
 3100          FORMAT(1h ,'!grdp',2x,'NG Steam    ',1x,22f6.2)
               WRITE(13,3200) (GRD_RATSA(WICC,IRG,I),IRG = 1 , UNRGNS)
 3200          FORMAT(1h ,'!grdp',2x,'Comb. Cycle ',1x,22f6.2)
               WRITE(13,3300) (GRD_RATSA(WICT,IRG,I),IRG = 1 , UNRGNS)
 3300          FORMAT(1h ,'!grdp',2x,'Comb. Turb. ',1x,22f6.2)
               WRITE(13,3400) (GRD_RATSA(WICN,IRG,I),IRG = 1 , UNRGNS)
 3400          FORMAT(1h ,'!grdp',2x,'Nuclear     ',1x,22f6.2)
               WRITE(13,3500) (GRD_RATSA(WIHY,IRG,I),IRG = 1 , UNRGNS)
 3500          FORMAT(1h ,'!grdp',2x,'Hydro       ',1x,22f6.2)
               WRITE(13,3600) (GRD_RATSA(WIWN,IRG,I),IRG = 1 , UNRGNS)
 3600          FORMAT(1h ,'!grdp',2x,'Onshore Wind',1x,22f6.2)
               WRITE(13,3700) (GRD_RATSA(WIPV,IRG,I),IRG = 1 , UNRGNS)
 3700          FORMAT(1h ,'!grdp',2x,'Photovoltaic',1x,22f6.2)
               WRITE(13,3800) (GRD_RATSA(WIP2,IRG,I),IRG = 1 , UNRGNS)
 3800          FORMAT(1h ,'!grdp',2x,'Demand Resp.',1x,22f6.2)
               WRITE(13,3900) (GRD_RATSA(WIDS,IRG,I),IRG = 1 , UNRGNS)
 3900          FORMAT(1h ,'!grdp',2x,'Diurnal Stg.',1x,22f6.2)
!           END DO
            WRITE(13,2000)
         WRITE(13,2300)
 2300 FORMAT(1h ,'!grdp',2x,'Note: -1.00 Rating means option is not represented')
            WRITE(13,2000)
!        END IF
!        SELECTED SOURCE
         DO IR = 1 , UNRGNS
            DO IP = 1 , ECP_D_CAP
               GRD_RATS(IP,IR) = GRD_RATSA(IP,IR,GRD_SRCI)
            END DO
         END DO
!
!        READ IN GRID RESILIENCE TARGETS
!
!        TURN OFF GRID RESILIENCE TARGETS IF USW_GRD = 0 OR BEFORE ECP CAN BUILD
!
         IF (USW_GRD .GT. 0)THEN
            IF (USW_GRD .EQ. I)THEN
               DO JYR = 1 , MNUMYR
                  DO IR = 1 , UNRGNS
                     IF (JYR .GT. (UPSTYR + (UNXPH - 1) - UHBSYR))THEN
!                       GRD_TGTS(JYR,IR) = 0.0
!                    ELSE
                        GRD_TGTS(JYR,IR) = GRD_TGTX(JYR,I,IR)
                     END IF
                  END DO

               END DO
            END IF
         END IF
!        DO JYR = 1 , MNUMYR
!           IF (JYR .GT. (UPSTYR + (UNXPH - 1) - UHBSYR))THEN
!              write(6,6543) jyr + uhbsyr,(grd_tgts(jyr,ir),ir=1,unrgns)
!6543 format(1h ,'!grdtg',i4,22f6.3)
!           END IF
!        END DO
      END DO
!              WRITE(6,3000) (GRD_RATS(WIPC,IRG),IRG = 1 , UNRGNS)
!              WRITE(6,3100) (GRD_RATS(WIST,IRG),IRG = 1 , UNRGNS)
!              WRITE(6,3200) (GRD_RATS(WICC,IRG),IRG = 1 , UNRGNS)
!              WRITE(6,3300) (GRD_RATS(WICT,IRG),IRG = 1 , UNRGNS)
!              WRITE(6,3400) (GRD_RATS(WICN,IRG),IRG = 1 , UNRGNS)
!              WRITE(6,3500) (GRD_RATS(WIHY,IRG),IRG = 1 , UNRGNS)
!              WRITE(6,3600) (GRD_RATS(WIWN,IRG),IRG = 1 , UNRGNS)
!              WRITE(6,3700) (GRD_RATS(WIPV,IRG),IRG = 1 , UNRGNS)
!              WRITE(6,3800) (GRD_RATS(WIP2,IRG),IRG = 1 , UNRGNS)
!              WRITE(6,3900) (GRD_RATS(WIDS,IRG),IRG = 1 , UNRGNS)

!              WRITE(6,3000) (GRD_RATSA(WIPC,IRG,GRD_SRCI),IRG = 1 , UNRGNS)
!              WRITE(6,3100) (GRD_RATSA(WIST,IRG,GRD_SRCI),IRG = 1 , UNRGNS)
!              WRITE(6,3200) (GRD_RATSA(WICC,IRG,GRD_SRCI),IRG = 1 , UNRGNS)
!              WRITE(6,3300) (GRD_RATSA(WICT,IRG,GRD_SRCI),IRG = 1 , UNRGNS)
!              WRITE(6,3400) (GRD_RATSA(WICN,IRG,GRD_SRCI),IRG = 1 , UNRGNS)
!              WRITE(6,3500) (GRD_RATSA(WIHY,IRG,GRD_SRCI),IRG = 1 , UNRGNS)
!              WRITE(6,3600) (GRD_RATSA(WIWN,IRG,GRD_SRCI),IRG = 1 , UNRGNS)
!              WRITE(6,3700) (GRD_RATSA(WIPV,IRG,GRD_SRCI),IRG = 1 , UNRGNS)
!              WRITE(6,3800) (GRD_RATSA(WIP2,IRG,GRD_SRCI),IRG = 1 , UNRGNS)
!              WRITE(6,3900) (GRD_RATSA(WIDS,IRG,GRD_SRCI),IRG = 1 , UNRGNS)


!     TD - moved gas consumption to EMM_DB 12/2020
!     Read in historical seasonal percentages of gas consumption by gas region
!
 !     READ(I_CNTL,*)
!      READ(I_CNTL,*)
!      READ(I_CNTL,*)
!      READ(I_CNTL,*)
!      READ(I_CNTL,*)

   !   DO IRG = 1 , NNGEM
    !    DO ISP = 1 , EFDNS
    !      READ(I_CNTL, * ) IREG,ISEAS,(HSEAGASPER(IRG,ISP,IYR),IYR = HSTYEAR-UHBSYR,HSTYEAR-UHBSYR+NHIST-1)
    !    ENDDO
   !   ENDDO
  

      NEW = .FALSE.
      FILENM='EMMCNTL'
      I_CNTL = FILE_MGR('C',FILENM,NEW)
!
!     GET VALUE FOR EMMRENEW (RUN OPTION) USING RTOVALUE FUNCTION.
!     0 = NO INTEGRATION WITH RENEWABLES
!     1 = INTEGRATE WITH RENEWABLES.
!
      USW_RNW = RTOVALUE('EMMRENEW',0)
!!     GET VALUE OF ECPACT - FROM NEMS JCL FT06 USING FUNCTION RTOVALUE
!     ECPACT = YEAR TO WRITE OUT ACTFILE, 1990,1991, ETC.
!     DEFAULT = 0, WRITE OUT FIRST YEAR OF RUN
      ECPACT = RTOVALUE('ECPACT  ',0)

!     GET VALUE OF ECPBASIS - FROM NEMS JCL FT06 USING FUNCTION RTOVALUE
!     IF ECPBASIS=0 THEN USE BASIS FROM PREVIOUS YEAR FROM CURRENT RUN
!     IF ECPBASIS=1 THEN READ BASIS FOR CURRENT YEAR FROM BASEMMI
      ECPBASIS = RTOVALUE('ECPBASIS',0)
!
!     WRITE(22,*) 'USW_RNW ',USW_RNW
!     WRITE(22,*) 'ECPACT ',ECPACT
!     WRITE(22,*) 'ECPBASIS ',ECPBASIS
!
!     INITIALIZE THE EXPORT/IMPORT ARRAY
!
!     UPXRGN = 0
!
      RETURN
      END


      SUBROUTINE CRPGRP(IYR,JRG,MAXHRZN)

      IMPLICIT NONE

!     THIS SUBROUTINE CREATES PLANT GROUP LEVEL DATA USED FOR DISPATCHING IN EFD

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'cdsparms'
      include 'control'
      include 'uefpout'     ! UEFP output variables
      include 'uefdout'     ! EFD output variables
      include 'udatout'     ! UDAT output variables
      include 'uecpout'     ! UECP output variables
      include 'uldsmout'    ! LDSM output variables
      include 'uettout'     ! UETT output variables
      include 'dispin'
      include 'dispuse'
      include 'dispout'
      include 'dispett'
      include 'emmcnv'
      include 'plntin'
      include 'plntctl'
      include 'ecpcntl'
      include 'entcntl'
      include 'bildin'
      include 'fuelin'
      include 'efpin'
      include 'efpr'
      include 'dispinyr'
      include 'enewtech'
      include 'cogen'
      include 'coalemm'
      include 'eusprc'
      include 'efpint'
      include 'elcntl'
      include 'elout'
      include 'emission'
      include 'emoblk'
      include 'emeblk'
      include 'epmbank'
      include 'wrenew'
      include 'wwdcomon'
      include 'efdin'
      include 'ecp_coal'
      include 'ecp_nuc'
      include 'uso2grp'
      include 'macout'
      include 'dsmdimen'
      include 'dsmtoefd'
      include 'dsmtfecp'
      include 'dsmcaldr'
      include 'dsmsectr'
      include 'csapr'
      include 'efpgen'
      include 'edbdef'
      include 'tcs45q'
      include 'emmemis'
      include 'emm_aimms'
!
      !COMMON /TMP_COAL/ ECL_SYR
      !REAL ECL_SYR(MAX_CL)
      COMMON /TMP_RESTORE/ PM_FRAC
      REAL*8 PM_FRAC(EMM_D_GRP,EFD_D_MSP)
!
      COMMON /TMP_CNTL/ UPCCSSYR
      REAL UPCCSSYR
!
      COMMON/COFSHR/COFCAP
      REAL*4 COFCAP(ECP_D_RCF,ECP_D_DSP,MNUMNR,MAXNFR)
!
      COMMON/COFSHRP/COFGENPC,TOTGENPC,COFGENPN,TOTGENPN
      REAL*4 COFGENPC(ECP_D_CAP,NDREG)
      REAL*4 TOTGENPC(ECP_D_CAP,NDREG)
      REAL*4 COFGENPN(ECP_D_CAP,MNUMNR)
      REAL*4 TOTGENPN(ECP_D_CAP,MNUMNR)
!
!     COMMON/CLCON/CLCONFC
!     REAL*4 CLCONFC(NDREG,MAXNFR + 1)

!      COMMON /VARCOST/ VARCOL,VAROTH,CFCPLT
!      REAL*4 VARCOL(MAXNFR,ECP_D_CAP)
!      REAL*4 VAROTH(MNUMNR,ECP_D_CAP)

      COMMON /MR111D/ MRGENF,MRCO2F,MRGENN,MRCO2N
      REAL*4 MRGENF(MAXNFR,MNUMYR + ECP_D_XPH)
      REAL*4 MRCO2F(MAXNFR,MNUMYR + ECP_D_XPH)
      REAL*4 MRGENN(MNUMNR,MNUMYR + ECP_D_XPH)
      REAL*4 MRCO2N(MNUMNR,MNUMYR + ECP_D_XPH)
!
      INTEGER*4 SINDX, RINDX, CYR_INDEX(ECP_D_XPH), LYR_INDEX(ECP_D_XPH), YINDX, JMO, PLYR
      REAL*8 HTRT_ECP(EMM_D_GRP,ECP_D_XPH), HTRT_EFD(EMM_D_GRP,EFD_D_MSP), TST_TGEN(EMM_D_GRP)
      REAL*8 LCAP_EFD(EMM_D_GRP,EFD_D_MSP), MO_CNT(0:EFD_D_MSP)

!      REAL*4 CFCPLT(MNUMNR,ECP_D_CAP)
      REAL*8 OVRCST
      REAL*8 CAPNNSTL,CAPNUG,PRATIO,FNBOOK,TXBOOK
      REAL*4 EWGROR,EWGINT,EWGROE
      REAL*4 CPR(ECP_D_LCP),CAPD(MNUMYR+ECP_D_XPH),PCST
      INTEGER*4 NLLF,NCLF,TLF,CLT,PLT,PLANT,JECP,TMP_RCFG(MNUMYR,MAX_CL),IHR
!
      REAL*8 REVS,SO2P,VCST,FCST,TST_VAL,CUM_FOM,CUM_VAL,TCAP,NOXP,RPSP,HGP,GHGP
      REAL*8 NEW_VAL
      INTEGER*4 KGRP,JGRP,NERC,KYR,SET_RFURB(EMM_D_GRP),ECPt_MAP(ECP_D_CAP,ECP_D_CAP)
      INTEGER*4 IL,LDGRP                                       ! LOAD GROUP INDICES
!
      REAL*8 CAPRPT,CL$CAP,CL$GEN
      REAL*8 CAP,GCAP,CAPHR,CAPSCR,CAPVOM,CAPFOM,CAPXTR,CAPGSUB,CAPCSUB,GCFC,GSUB,SPAYYR
      REAL*8 TNOX_R(EFD_D_MSP),TSUM_SHR,TWIN_SHR
      REAL*8 TNOX_RC(NOX_D_GRP,EFD_D_MSP)
      REAL*8 ACT_CF
      REAL*8 AVGCF,MAXCF,TFOR,TPMR,FCTR,WGHT1,WGHT2,TLFR,MXLFR,FCTR_AC,FCTR_CC
      REAL*8 SCAP(EFD_D_MSP),GSCAP(EFD_D_MSP),GECPSCAP(ECP_D_RNW,ECP_D_MSP)
      REAL*8 EYSCAP,EYRETR,EYFCST,EYCCST,ECPSCAP(ECP_D_MSP)
      REAL*8 CFCAP(EFD_D_MSP),ECFCAP(ECP_D_RNW,ECP_D_MSP,ECP_D_XPH)
      REAL*8 CAPCF(EFD_D_MSP),ECAPCF(ECP_D_RNW,ECP_D_MSP,ECP_D_XPH)
      REAL*8 RNW_CFC(ECP_D_RNW),RNW_MCFC(ECP_D_RNW)
      REAL*8 NUC_AVL,RMCF,RSCFC(ECP_D_RNW,ECP_D_MSP)
      REAL*8 MXFSHR(EFD_D_NFL),TSTFSHR,FSHR
      REAL*4 RET_MIN(ECP_D_CAP),RET_MAX(ECP_D_CAP)
      REAL*8 ECP_GEN(ECP_D_CAP),ECP_CF,TGEN,ECP_GEN_XPH(ECP_D_XPH,ECP_D_CAP)
      REAL*8 PVV
      REAL*8 ECP_VOM(ECP_D_CAP),ECP_NOX(ECP_D_CAP),P_HTRT_MAX(ECP_D_CAP)
      REAL*8 P_GEN(ECP_D_CAP),P_GEN_XPH(ECP_D_XPH,ECP_D_CAP),P_HTRT0(ECP_D_CAP),P_FOM(ECP_D_CAP),P_VOM(ECP_D_CAP),P_NOX(ECP_D_MSP,ECP_D_XPH,ECP_D_CAP,NOX_D_GRP)
      REAL*8 P_MCF(ECP_D_CAP,ECP_D_XPH),P_CAP(ECP_D_CAP,ECP_D_XPH),ABTU,C2_GEN(NCLUT1,NDRGG),C2_HTRT(NCLUT1,NDRGG)
      REAL*8 PV_CF,TCF(ECP_D_FPH)
      REAL*8 PV_KW,KW(ECP_D_FPH)
      INTEGER*4 N_GRP,ITYP,P_TTYP(ECP_D_CAP), CLtNG_TYP, CFYR
      REAL*8 AVG_PV_CF_MO(MNUMNR,MNUMYR,12), AVG_WN_CF_MO(MNUMNR,MNUMYR,12), AVG_SO_CF_MO(MNUMNR,MNUMYR,12), AVG_WF_CF_MO(MNUMNR,MNUMYR,12)
      REAL*8 SUM_MO_PV_CF,SUM_MO_WN_CF, SUM_MO_SO_CF,SUM_MO_WF_CF,SUM_MO_HRS
!
      REAL*4 NUC_CFC,N_CF,CFMULT,W_CFP
      REAL*4 QGEN(EFD_D_DSP),QCAP(EFD_D_DSP),LCF(EFD_D_DSP),ACF(EFD_D_DSP)
      REAL*4 SCRCST(ECP_D_SGP,ECP_D_DSP,ECP_D_SCR)
      REAL*8 N_CFY(ECP_D_FPH),N_KWY(ECP_D_FPH)
      REAL*8 SCRHPEN
      REAL*8 CF_WGHT(NDREG,ECP_D_DSP,ECP_D_XPH),T_CF,B_CF
      REAL*8 MAXCF_TOT(ECP_D_CAP),MAXCF_WGT(ECP_D_CAP),MAXCF_TOT_2(ECP_D_CAP),MAXCF_WGT_2(ECP_D_CAP)
      INTEGER*4 AGE
!
      INTEGER*4 IDTYP,CRG,MIN_ECPt(MAX_CL),I_CLRG,ICLS,FRG, m, d, h
      INTEGER*4 TOT_GRP
      INTEGER*4 I,J,K,PHRGN,OWRGN,IYR2
      REAL*8    WSEAS(0:EFD_D_MSP),ECP_SUMMER_SHR(0:ECP_D_MSP),SUMMER_MONTHS(12),CAP8,SHR8
      REAL*8    EFD_HOURS(0:EFD_D_MSP),ECP_HOURS(0:ECP_D_MSP)
      INTEGER*4 IVIN3                 ! 1 = EXIST, 2 = PIPELINE, 3 = NEW
      INTEGER*4 MRSW
      INTEGER*4 IRECL,ITEST,RTEST,IYR,JRG,IRG,IGRP,SYRMO,RYRMO,MONTHS,TRG,T_COF,OLD_REC
      INTEGER*4 YEAR,FULLYR,VINTAGE,FTYPE,OTYPE,IOWN,NSCR,FULLCYR
      INTEGER*4 IYRMO,IFL,IEFD,ISP,FL_NUM,RG_NUM,JYR,IYRMONX,IY
      INTEGER*4 CL_REG,INOX,IST,XNOX,LNOX(NOX_D_GRP)
      INTEGER*4 JTEST,SYRMO1,SYRMO2,STRTYR
      INTEGER*4 IFLRG,IECP,IECP2,ISCR,CYEAR_ECP,CYEAR_EFD,FLRG
      INTEGER*4 FIRM,INTR,COMP,COAL
      INTEGER*4 MAXHRZN
      INTEGER*4 FL_IND(EFD_D_FPP),MTCH_FL,ISEQ
      INTEGER*4 IBTP,INCT
      INTEGER*4 FREC_GP,GIEFD,GIECP,GIVIN3,GW_GRP,GRG_NUM         ! Check Plant Groups
      INTEGER*4 GIBTP,GINCT                                       ! Check Plant Groups
      INTEGER*4 DAYS(12),IMO
      REAL*8 MOTOSP(0:EFD_D_MSP,12),MOTOESP(0:ECP_D_MSP,12)
      REAL*8 HRS_ECP(ECP_D_MSP),HRS_EFD(EFD_D_MSP),MXSHR,R8TEMP
      INTEGER*4 PRMSG                     ! Flag to set message print on
      INTEGER*4 UCFSYR(EFD_D_CAP),UCFEYR(EFD_D_CAP)
      INTEGER*4 FREC,OREC,NREC,RGRP,LYR,XYR,KRG,KECP,KFL
      INTEGER*4 NUCRETSW
      REAL*8 W_CFA,WHRATEA,WFOMA,WVOMA,WGAA
      REAL*8 TOM,TOMY,T_AVG(ECP_D_CAP,MNUMNR),T_CAP(ECP_D_CAP,MNUMNR),T_VCST,C_CAP(MAX_CL),T_VADJ(EMM_D_GRP),T_WGRP(EMM_D_GRP)
      REAL*8 T_GEN(ECP_D_CAP,MNUMNR),T_HRATE(ECP_D_CAP,MNUMNR),A_HR
      INTEGER*4 C_GRP(MAX_CL)
      REAL*8 C_CF(MAX_CL)
      INTEGER*4 C_NXG(NOX_D_GRP,MAX_CL)
      REAL*8 C_VADJ(MAX_CL),CAP_NUC2(MAXNUC),T_RG_CAP(MNUMYR,MNUMNR,MAX_CL),RG_SHR
      REAL*8 C_FOM(MAX_CL)
      REAL*8 C_NXR(MAX_CL)
      REAL*8 C_SO2R(MAX_CL)
      REAL*8 C_HTRT(MAX_CL),COAL_FACTOR(MNUMYR+ECP_D_XPH)
      REAL*8 TMP_EWGFIX
      REAL*4 WDFSHR,WDSHR(ECP_D_DSP,ECP_D_XPH,NDREG),WDSUM(ECP_D_DSP,ECP_D_XPH,NDREG)
      REAL*4 MRFAC
      INTEGER YRCECA,IRNK,I_COAL,ICL,IP,ITST,C_ECP(MAX_CL),I_NUC,SV_RCFG(ECP_D_XPH,MAX_CL)
      INTEGER*4 TST_RETRO(ECP_D_CAP),I_ROPT,I_CNFG,T_ECP(ECP_D_CAP),I_MACT,J_CNFG,BYYYYMM,TYYYYMM,SYYYYMM,RYYYYMM,I_ECPt,J_ECPt
      INTEGER MYR
      INTEGER GRP, INTERMIT
      REAL*4 CO2_GEOT(MNUMNR),CO2_MSWT(MNUMNR),CO2_DGNT(MNUMNR)
      REAL*4 CCSADJ
      INTEGER COA,TRN
      REAL*4 CLCFC,PLBTU,CLBTU
      REAL*4 CFADJ(MNUMYR)
      REAL*8 DSIOVR,DSIFOM,DSIVOM,DSISEF
      REAL*8 RETOVCST,URATIO,CAPUNSTL,CAPUTIL
      REAL*4 RETIRT,RETROR,RETFPE,RETCRE
      INTEGER RETYR, STXLF
      INTEGER*4 GYR
      INTEGER*4 W_ST
      REAL*8 CAP_NGBS(MX_NGBS)
      INTEGER I_NGBS
      REAL*4  NUCADJ,CAPDER
      REAL*8 T_TCAP,T_TFCF
      INTEGER TRN_LIFE
      REAL*8  CAPCCR(ECP_D_CAP)
      REAL*8 AVG_HTRT(0:ECP_D_CAP), AVG_HTRT_MR(0:ECP_D_CAP), AVG_HTRT_MOD(0:ECP_D_CAP), AVG_HTRT_MR_MOD(0:ECP_D_CAP)
      REAL*8 T_ECP_GEN(0:ECP_D_CAP), T_ECP_GEN_MR(0:ECP_D_CAP), T_ECP_GEN_MOD(0:ECP_D_CAP), T_ECP_GEN_MR_MOD(0:ECP_D_CAP)
      INTEGER IRECLL,I_COALL,I_NUCL
      REAL*8 CAPFAC
      INTEGER JNT
      REAL NUCNET1, NUCNET2, ENUC_SUB(MAXNUC)
      REAL COLNET1, COLNET2
      REAL CCYNET1, CCYNET2
      REAL STMNET1, STMNET2
      REAL ELNUCSBDY(50,MNUMNR,MNUMYR)    ! ZEC SUBSIDY AMOUNT (NEGATIVE REVENUES) BY STATE, REGION, AND YEAR
      REAL ELNUCSBGN(50,MNUMNR)           ! ZEC SUBSIDY GENERATION BY STATE, REGION, AND YEAR
      
      COMMON/NUCSUB/ELNUCSBDY,ELNUCSBGN

      INTEGER*4  FUEL_RGN, ECPt, W_GRPL
      REAL*8 Load_Level, HTRT_ADJ, Target_EFF, Max_EFF
      CHARACTER*12 FROM_LABEL
!
      REAL*4 HRIOVR,HRIRED,HRIMP
      INTEGER HRQ
!
      REAL*4 MRGEN,MRCO2,TLSHR,FLSHR,CPCTN(MAXNFR,EFD_D_NFL), CPCTNTOT(MAXNFR)
!
      INTEGER*4 GWNOWN,GWFL(EFD_D_FPP),GWFOWN
!
      COMMON /NUCRTX/ URTNUCX
      REAL URTNUCX(MNUMYR,MNUMNR)

      REAL MXNUCREV, NETNUCREV, BSNUCCRED, NUCWGMULT
      
      DATA MXNUCREV/25.0/     ! IRA max revenues used to phaseout existing nuclear tax credit - $/MWh - in same year $ as tax credit - UPGSYD
      DATA BSNUCCRED/3/       ! Based credit without wage multiplier for IRA 45U - $/MWh - in same dollar years as MXNUCREV and input tax credit
      
      LOGICAL TR

      DATA ACF/3*0.70, 6*0.75, 4*0.40, 6*0.20, 6*0.60, 0.75, 4*0.75, 2*0.75, 2*0.60/
!     DATA CFADJ/14*1.0,1.1,1.2,1.3,1.4,1.5,2.0,2.5,3.0,3.5,38*0.0/
      DATA CFADJ/19*1.0,1.5,1.5,2.0,3.5,2.5,37*2.5/

      COMMON/BNCHGEN/ BSNCGEN,BSHYGEN,BSSOGEN,BSPVGEN,BSWNGEN,BSGTGEN,BMNCLYR,BMHYLYR,BMSOLYR,BMPVLYR,BMWNLYR,BMGTLYR
      REAL BSNCGEN,BSHYGEN,BSSOGEN,BSPVGEN,BSWNGEN,BSGTGEN
      INTEGER BMNCLYR,BMHYLYR,BMSOLYR,BMPVLYR,BMWNLYR,BMGTLYR

      REAL*4 T1(MNUMNR,12),T2(MNUMNR,12)

      INTEGER*4 OVEC, ICLTP, ICRV, CLRG
!
!     INITIALIZATION C
      IF (JRG .EQ. 1) THEN
        CPCTN = 0.0
        CPCTNTOT = 0.0
      ENDIF
!
      IF (CURITR .EQ. 1 .AND. JRG .EQ. 1) THEN
         BSNCGEN = 0.0
         BSHYGEN = 0.0
         BSSOGEN = 0.0
         BSPVGEN = 0.0
         BSWNGEN = 0.0
         BSGTGEN = 0.0
         DO IRG = 1 , MNUMNR
            UCAPWNR(IRG,CURIYR) = 0.0
            UCAPWLR(IRG,CURIYR) = 0.0

!           WRITE(6,5013) CURIRUN,CURIYR+1989,CURITR,IRG,UCAPWNR(IRG,CURIYR),UCAPWLR(IRG,CURIYR)
 5013       FORMAT(1X,"UCAPWNR_UTIL",4(":",I5),2(":",F15.3))

            UCAPWFR(IRG,CURIYR) = 0.0

            URTNUCX(CURIYR,IRG) = 0.0

            DO KRG = 1 , 50
               ELNUCSBDY(KRG,IRG,CURIYR) = 0.0
               ELNUCSBGN(KRG,IRG) = 0.0
            END DO
         END DO
         IF ((CURIYR + UHBSYR) .EQ. UESTYR)THEN
            EST_FRG = 0
            EMAP_STFR = 0.0
            NUCPLNF = 0.0
            NUCPLNN = 0.0
         END IF
         MRGENF = 0.0
         MRCO2F = 0.0
         MRGENN = 0.0
         MRCO2N = 0.0
!
!     Initialize Arrays Needed to Track Incremental Gen for Final 111d Rule
!
         IF ((CURIYR + UHBSYR) .EQ. CO2_AFFYR .OR. (CO2_AFFYR .EQ. 0 .AND. (CURIYR + UHBSYR) .EQ. (UPSTYR - 1)))THEN
            EPBLDGEN = 0.0
         END IF

         STORAGE_CAP = 0.0
         IF (CURCALYR .EQ. (UPSTYR - 1)) THEN
            STORAGE_GEN = 0.0
            STORAGE_CAP = 0.0
            STORAGE_CST = 0.0
            STORAGE_RGN = 0
            STORAGE_ECPn = 0
            STORAGE_ECPc = "  "
         END IF

! set XCL_TRNINDX so that unscrubbed plants cannot use med/high sulfur coal after 2015
         !XCL_TRNINDX = 1     ! default to 1 initially, set to 0 where not allowed
         !IF (CURIYR .GE. 26) THEN
         !DO ICLTP = 1, NUTSEC
         !   DO ICRV = 1, MX_NCOALS
         !     DO CLRG = 1, NDREG
         !        IF (ECP_SCRUB(ICLTP,CURIYR) .EQ. 2) THEN   ! unscrubbed, first set to 0
         !           XCL_TRNINDX(ICLTP,ICRV,CLRG) = 0
         !        ENDIF
         !        IF (ICRV .EQ. 7 .OR. ICRV .EQ.  10 .OR. ICRV .EQ. 12 .OR. ICRV .EQ. 14 .OR. &
         !            ICRV .EQ. 15 .OR. ICRV .EQ. 26 .OR. ICRV .EQ. 27 .OR. ICRV .EQ. 29 .OR. &
         !            ICRV .EQ. 31 .OR. ICRV .EQ. 32 .OR. ICRV .EQ. 33 .OR. ICRV .EQ. 35 .OR. &
         !            ICRV .EQ. 36 .OR. ICRV .EQ. 37 .OR. ICRV .EQ. 39 .OR. ICRV .EQ. 41 )THEN    ! low sulfur curves, these are allowed
         !            XCL_TRNINDX(ICLTP,ICRV,CLRG) = 1
         !        ENDIF
         !     ENDDO
         !   ENDDO
         !ENDDO       
         !ENDIF
         !XCL_TRNINDX(1,18,12) = 1   ! hardwire these to allow coal needed to meet contract for Braeme Energy Center
         !XCL_TRNINDX(1,20,12) = 1
!  update max CF for EPA 111 new gas plants - take adjustment out of PMR
         IF (USW_EPA111 .EQ. 1 .AND. (CURIYR + UHBSYR) .GE. UEPA_NWGSYR) THEN
         ! ECP variables
           UPMCF(WICC) = UEPA_NWGSCF
           UPMCF(WIAC) = UEPA_NWGSCF
		   FCTR_CC = (UPMCF(WICC)/((1.0 - UPPMRT(WICC))*(1-UPFORT(WICC))) ) 
		   UPPMRT(WICC) = 1.0 - FCTR_CC * (1.0 - UPPMRT(WICC))
		   FCTR_AC = (UPMCF(WIAC)/((1.0 - UPPMRT(WIAC))*(1-UPFORT(WIAC))) ) 
		   UPPMRT(WIAC) = 1.0 - FCTR_AC * (1.0 - UPPMRT(WIAC))
		 ! EFD variable (also change W_CF below)  
           WMXCP(UIACC) = UEPA_NWGSCF      !don't change UICC* EFD type because these are mostly existing 
		   FCTR_AC = (WMXCP(UIACC)/((1.0 - WPMR(UIACC))*(1-WFOR(UIACC))) ) 
		   WPMR(UIACC) = 1.0 - FCTR_AC * (1.0 - WPMR(UIACC)) 
         ENDIF             
! update UPCCEF_MIN if EPA 111 in place - can't turn off capture
         IF (USW_EPA111 .EQ. 1) THEN
            DO IECP = 1 , ECP_D_CAP
               IF ((UPTTYP(IECP) .LE. EX_COAL) .AND. (CURIYR + UHBSYR .GE. UEPA_CLYR) ) THEN
                  UPPCEF_MIN(IECP) = UPPCEF(IECP)     !must keep capture at max for existing coal 
               ELSEIF ((UPTTYP(IECP) .GT. NW_COAL .AND. UPVTYP(IECP) .EQ. 1) .AND. (CURIYR + UHBSYR .GE. UEPA_NWGSYR) ) THEN
                  UPPCEF_MIN(IECP) = UPPCEF(IECP)     !must keep capture at max for new gas (not retrofit at this time)
               ENDIF       
            ENDDO
         END IF
      ENDIF
!
      TR = .TRUE.
!
      SCRHPEN = UPSHPEN(1,1) ! Heatrate Penalty
!
      IRG = URGNUM(JRG)

!     if ((curiyr+1989) .ge. upstyr)then
!        do iecp = 1 , ECP_D_DSP
!        write(6,3321) curiyr+1989,irg,uplntcd(iecp),cfcplt(irg,iecp)
!3321 format(1h ,'!cfcplt',i4,i3,1x,a2,f10.4)
!        end do
!     end if

      FULLCYR = USYEAR(CURIYR)

      IF (IRG .EQ. 1 .AND. CURITR .EQ. 1) THEN
         HTRT_CAP = 0.0
         HTRT_CAP_MR = 0.0
      END IF

      IF (IRG .EQ. 1)THEN
        EMREV(9,CURIYR) = 0.0
        TBTU_SHR_BY_CLRG = 0.0
        TBTU_SHR_BY_ST = 0.0
      END IF
!
!     GET EFP DATA
!
      IF (USW_XP .EQ. 0)  THEN               !not for Canada
         IF (FULLCYR .GT. UESTYR) THEN
            CALL GETR(IRG)
         END IF
      ENDIF
!
!     MAP CURRENT CAPACITY TYPE TO ALL POTENTIAL CAPACITY TYPES
!
      ECPt_MAP = 0
      DO I_ECPt = 1 , ECP_D_CAP
         ECPt_MAP(I_ECPt,I_ECPt) = 1
      END DO
      DO I_CNFG = 1 , NUM_CNFG
         I_ECPt = UCL_ECP(I_CNFG)
         DO I_ROPT = 1 , NUM_ROPT
            IF (UCL_RCMB(I_ROPT,I_CNFG) .GT. 0) THEN
               J_CNFG = UCL_RCMB(I_ROPT,I_CNFG)
               J_ECPt = UCL_ECP(J_CNFG)
               ECPt_MAP(I_ECPt,J_ECPt) = 1
            END IF
         END DO
      END DO
!
!     Initialize Arrays Needed to Calculate Average Max Capacity Factor for Dispatchable Capacity
!
      MAXCF_TOT = 0.0
      MAXCF_WGT = 0.0
      MAXCF_TOT_2 = 0.0
      MAXCF_WGT_2 = 0.0
!
!     Assign Value PMR Max Load Following Rate - Later it can be calculated from Load Shapes
!     Zeroing this out - logic no longer relevant and impacts ability to implement lower max CF for EPA111      
!
      MXLFR = 0.0
!
!     For existing coal types initialize sum of current or potential capacity by coal demand region
!
      IF (IRG .EQ. 1) TST_CAP_BY_CRG = 0.0
!
!     For existing coal types initialize sum of coal capacity by coal and nerc region (for sharing cofiring)
!
      IF (IRG .EQ. 1)THEN
         COFCAP = 0.0
       IF ((CURIYR + UHBSYR) .LE. UESTYR)THEN
         COFGENPC = 0.0
         TOTGENPC =  0.0
         COFGENPN = 0.0
         TOTGENPN =  0.0
       END IF
         CLCONFC =  0.0
!
         CO2_GEO =  0.0
         CO2_MSW =  0.0
         CO2_DGN =  0.0
         CO2_GEOT =  0.0
         CO2_MSWT =  0.0
         CO2_DGNT =  0.0
!      111d HISTORICAL GENERATION BASED ON SPECIFIED YEAR
       IF ((CURIYR + UHBSYR) .LE. CO2_STDBY)THEN
         CO2_GENQF = 0.0
         CO2_GENQN = 0.0
         CO2_GENTL = 0.0
         CO2_GENTN = 0.0
       END IF
      END IF
!
!     GET PREVIOUS YEARS GENERATION AND CAPACITY FACTORS TO COMPUTE
!     CAPACITY FACTORS TO PUT FIXED O&M WITH VARIABLE O&M FOR DISPATCH
!
      IF (IRG .EQ. 1) THEN
         DO CRG = 1 , NDREG
            DO IECP = 1 , ECP_D_DSP
               IF (UPTTYP(IECP) .LE. EX_COAL) THEN
                  ITYP = UPTTYP(IECP)
                  DO KYR = 1 , UNXPH - 1
                     IF (UECP_CAP(KYR+1,ITYP,CRG) .GT. 0.0) THEN
                        T_CF = UECP_GEN(KYR+1,ITYP,CRG) / (8.760 * UECP_CAP(KYR+1,ITYP,CRG))
                        B_CF = UEFD_GEN(IECP,CRG,CURIYR) / (8.760 * UEFD_CAP(IECP,CRG,CURIYR))
                        CF_WGHT(CRG,IECP,KYR) = (UPMCF(IECP) - T_CF) / (UPMCF(IECP) - B_CF)
                        CF_WGHT(CRG,IECP,KYR) = MAX(DBLE(0.0) , MIN(DBLE(0.9) , CF_WGHT(CRG,IECP,KYR)))
                     ELSE
                        CF_WGHT(CRG,IECP,KYR) = 0.9
                     END IF
                  END DO
                  IF (CF_WGHT(CRG,IECP,1) .GT. DBLE(0.0)) THEN
                     CF_WGHT(CRG,IECP,UNXPH) = CF_WGHT(CRG,IECP,UNXPH-1) * ((CF_WGHT(CRG,IECP,UNXPH-1) / CF_WGHT(CRG,IECP,1)) ** DBLE(1/(UNXPH - 2)))
                     CF_WGHT(CRG,IECP,UNXPH) = MAX(DBLE(0.0) , MIN(DBLE(1.0) , CF_WGHT(CRG,IECP,UNXPH)))
                  ELSE
                     CF_WGHT(CRG,IECP,UNXPH) = CF_WGHT(CRG,IECP,UNXPH-1)
                  END IF
               END IF
            END DO
         END DO
      END IF
!
      IF (FULLCYR .GT. UESTYR) THEN
         DO IGRP = 1 , EFD_D_DSP
            QGEN(IGRP) = 0.0
            QCAP(IGRP) = 0.0
            LCF(IGRP) = 0.0
         END DO

         CALL STROUT(CURIYR,IRG)

         DO I = 1 , CURIYR - 1
            CALL GETOUT(I,IRG)
            DO IGRP = 1 , EFD_D_DSP
               DO IOWN = 1 , USW_OWN
                  QGEN(IGRP) = QGEN(IGRP) + EQPGN(IGRP,IOWN)
               END DO
               QCAP(IGRP) = QCAP(IGRP) + EQPCP(IGRP)
            END DO
         END DO

         CALL GETOUT(CURIYR,IRG)

         DO IGRP = 1 , EFD_D_DSP
            IF (QCAP(IGRP) .GT. 0.0) THEN
               LCF(IGRP) = QGEN(IGRP) / (QCAP(IGRP) * 8.760)
           ELSE
               LCF(IGRP) = ACF(IGRP)
            END IF
!
!           INSURE MINIMUM CAPACITY FACTOR SO FIXED O&M ISN'T INFINITE
!
             LCF(IGRP) = MAX(LCF(IGRP) , 0.10)
         END DO
      END IF
!
      IF (FULLCYR .EQ. UESTYR) THEN
         IF (IRG .EQ. 1) THEN
            DO IGRP = 1 , EMM_D_GRP
               SET_RFURB(IGRP) = 9999
            END DO
            T_RG_CAP = 0.0
            MIN_ECPt = NCLUT1 + 1
         END IF
      END IF
!
!     Initialize UPHTRT for Dispatchables First Time Through
!
      IF (FULLCYR .EQ. UESTYR .AND. IRG .EQ. 1) THEN
         DO IEFD = 1 , ECP_D_DSP
            UPHTRT(IEFD) = UPPHRT0(IEFD)
         END DO
      END IF

!
!     Initialize variables for resource costs first time through
!
!     IF (FULLCYR .EQ. UESTYR .AND. IRG .EQ. 1) THEN

      IF (FULLCYR .EQ. UESTYR) THEN
         DO IY = 1, MNUMYR
            Ret_Cst(IRG,IY) = 0.0
            RET_INV(IRG,IY) = 0.0
            NEW_CAP_EL(IRG,IY) = 0.0
            G_INST_ALL(IRG,IY) = 0.0
            T_OVR(IRG,IY) = 0.0
            G_ANN(IRG,IY) = 0.0
            T_ANN(IRG,IY) = 0.0
            IF (IRG .EQ. 1) THEN
               DO KRG = MNUMNR-2,MNUMNR
                  Ret_Cst(KRG,IY) = 0.0
                  RET_INV(KRG,IY) = 0.0
                  NEW_CAP_EL(KRG,IY) = 0.0
                  G_INST_ALL(KRG,IY) = 0.0
                  T_OVR(KRG,IY) = 0.0
                  G_ANN(KRG,IY) = 0.0
                  T_ANN(KRG,IY) = 0.0
               ENDDO
               DO KRG =1,4
                  DO IFLRG = 1, MAXNFR
                     CAPCCS(KRG,IFLRG,IY) = 0.0
                  ENDDO
               ENDDO
!
            ENDIF
         ENDDO
      ENDIF

!     ONLY REINTIALIZE BEFORE THEY ARE CALCULATED, THIS ALLOWS THE CTUS TO HAVE REASONABLE VALUES TO WORK WITH

      EMELCCS(IRG,CURIYR) = 0.0
      EMELCDR(IRG,CURIYR) = 0.0
      EMELGCS(IRG,CURIYR) = 0.0
      EMELGDR(IRG,CURIYR) = 0.0
      IF (IRG .EQ. 1) THEN
         DO KRG = UNRGNS + 1, MNUMNR
            EMELCCS(KRG,CURIYR) = 0.0
            EMELCDR(KRG,CURIYR) = 0.0
            EMELGCS(KRG,CURIYR) = 0.0
            EMELGDR(KRG,CURIYR) = 0.0
         END DO
      END IF

!     Initilize cummulative intermittent capacity values used in PV curtailment calculations

      PV_CAP(IRG) = 0.0                                  ! Cummulative PV Capacity
      PT_CAP(IRG) = 0.0                                  ! Cummulative PV Capacity - Fixed Axis
      SO_CAP(IRG) = 0.0                                  ! Cummulative SO Capacity
      WN_CAP(IRG) = 0.0                                  ! Cummulative WN Capacity
      WL_CAP(IRG) = 0.0                                  ! Cummulative WN Capacity - Low Speed
      WF_CAP(IRG) = 0.0                                  ! Cummulative WF Capacity
      PV_NEW(IRG) = 0.0                                  ! New PV Capacity
      WN_NEW(IRG) = 0.0                                  ! New WN Capacity
      PV_CAP_ADJ(IRG) = 0.0                              ! Cummulative PV Capacity Adjusted to consistent with Hourly Capacity Factors
      PT_CAP_ADJ(IRG) = 0.0                              ! Cummulative PT Capacity Adjusted to consistent with Hourly Capacity Factors
      SO_CAP_ADJ(IRG) = 0.0                              ! Cummulative SO Capacity Adjusted to consistent with Hourly Capacity Factors
      WN_CAP_ADJ(IRG) = 0.0                              ! Cummulative WN Capacity Adjusted to consistent with Hourly Capacity Factors
      WL_CAP_ADJ(IRG) = 0.0                              ! Cummulative WL Capacity Adjusted to consistent with Hourly Capacity Factors
      WF_CAP_ADJ(IRG) = 0.0                              ! Cummulative WF Capacity Adjusted to consistent with Hourly Capacity Factors
      PV_NEW_ADJ(IRG) = 0.0                              ! New PV Adjusted Capacity
      WN_NEW_ADJ(IRG) = 0.0                              ! New WN Adjusted Capacity

      IF (IRG .EQ. 1) THEN
         IF (FULLCYR .LE. UESTYR) THEN !WSS** variables need to be lagged one year to pick up correct value
            CFYR = CURIYR
         ELSE
            CFYR = CURIYR - 1
         ENDIF
         AVG_PV_CF(:,CURIYR) = 0
         AVG_PT_CF(:,CURIYR) = 0
         AVG_SO_CF(:,CURIYR) = 0
         AVG_WN_CF(:,CURIYR) = 0
         AVG_WL_CF(:,CURIYR) = 0
         AVG_WF_CF(:,CURIYR) = 0

         DO KRG = 1 , UNRGNS
 !           DO IY = CURIYR, MNUMYR       only do one year at a time, because future year CF values are not updated yet from renew
               Do m=1,12
                 SUM_MO_PV_CF = 0
                 SUM_MO_WN_CF = 0
                 SUM_MO_WF_CF = 0
                 SUM_MO_SO_CF = 0
                 SUM_MO_HRS = 0
                 do d=1,3
                     do h=1,24
                        AVG_PV_CF(KRG,CURIYR) = AVG_PV_CF(KRG,CURIYR) + (WSSPVEL_CF(KRG,CFYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                        AVG_PT_CF(KRG,CURIYR) = AVG_PT_CF(KRG,CURIYR) + (WSSPTEL_CF(KRG,CFYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                        AVG_SO_CF(KRG,CURIYR) = AVG_SO_CF(KRG,CURIYR) + (WSSSTEL_CF(KRG,CFYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                        AVG_WN_CF(KRG,CURIYR) = AVG_WN_CF(KRG,CURIYR) + (WSFWIEL_CF(KRG,CFYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                        AVG_WL_CF(KRG,CURIYR) = AVG_WL_CF(KRG,CURIYR) + (WSFWLEL_CF(KRG,CFYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                        AVG_WF_CF(KRG,CURIYR) = AVG_WF_CF(KRG,CURIYR) + (WSFWFEL_CF(KRG,CFYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                        SUM_MO_PV_CF = SUM_MO_PV_CF + (WSSPVEL_CF(KRG,CFYR,d,m,h) * IDAYTQ(d,m))  
                        SUM_MO_WN_CF = SUM_MO_WN_CF + (WSFWIEL_CF(KRG,CFYR,d,m,h) * IDAYTQ(d,m))  
                        SUM_MO_SO_CF = SUM_MO_SO_CF + (WSSSTEL_CF(KRG,CFYR,d,m,h) * IDAYTQ(d,m))  
                        SUM_MO_WF_CF = SUM_MO_WF_CF + (WSFWFEL_CF(KRG,CFYR,d,m,h) * IDAYTQ(d,m))  
                        SUM_MO_HRS = SUM_MO_HRS + IDAYTQ(d,m)
                     enddo
                  enddo
                   AVG_PV_CF_MO(KRG,CURIYR,m) = SUM_MO_PV_CF / SUM_MO_HRS
                   AVG_WN_CF_MO(KRG,CURIYR,m) = SUM_MO_WN_CF / SUM_MO_HRS
                   AVG_SO_CF_MO(KRG,CURIYR,m) = SUM_MO_SO_CF / SUM_MO_HRS
                   AVG_WF_CF_MO(KRG,CURIYR,m) = SUM_MO_WF_CF / SUM_MO_HRS
               enddo
            WRITE(22,1212) CURIYR,CFYR,KRG,AVG_PV_CF(KRG,CURIYR),AVG_PT_CF(KRG,CURIYR),AVG_SO_CF(KRG,CURIYR),AVG_WN_CF(KRG,CURIYR),AVG_WL_CF(KRG,CURIYR),AVG_WF_CF(KRG,CURIYR)
            WRITE(22,1213) CURIYR,CFYR,KRG,AVG_PV_CF_MO(KRG,CURIYR,1),AVG_PV_CF_MO(KRG,CURIYR,6),AVG_WN_CF_MO(KRG,CURIYR,1),AVG_WN_CF_MO(KRG,CURIYR,6), &
                AVG_SO_CF_MO(KRG,CURIYR,1),AVG_SO_CF_MO(KRG,CURIYR,6), AVG_WF_CF_MO(KRG,CURIYR,1),AVG_WF_CF_MO(KRG,CURIYR,6)
!            ENDDO
         ENDDO
      END IF
1212  FORMAT(1x,'AVG_INT_CF ',3I6,6F12.6)
1213  FORMAT(1x,'AVG_INT_CF_MO ',3I6,8F12.6)

!
!     DETERMINE IF CURRENT YEAR HAS COMPETITION FOR CECA, WHICH
!     WILL THEN INCLUDE A PORTION OF FIXED O&M IN MARGINAL PRICE
!
      YRCECA = 0
      DO IEFD = 1 , EFD_D_DSP
         IF(UOM_PROF(CURIYR,IEFD) .LT. 1.0)YRCECA = 1
      END DO
!
      DO IEFD = 1 , EFD_D_CAP
         DO IOWN = 1 , USW_OWN
            ERTOMF(IEFD,IOWN) = 0.0
            ERTOMX(IEFD,IOWN) = 0.0
         END DO
      END DO
!
      EWGFIX = 0.0
      EFACTR = 0.001
      EIMCG = 1                            ! NUMBER OF COMPLIANCE GROUPS
!
      CL$CAP = 0.0
      CL$GEN = 0.0
!
      NUCRETSW = 0
!
      UCFSYR(1) = 2019
      UCFEYR(1) = 2030
      UCFSYR(2) = 2019
      UCFEYR(2) = 2030
      UCFSYR(3) = 2019
      UCFEYR(3) = 2030
      UCFSYR(4) = 2019
      UCFEYR(4) = 2030
      UCFSYR(5) = 2019
      UCFEYR(5) = 2030
      UCFSYR(6) = 2019
      UCFEYR(6) = 2030
      UCFSYR(7) = 1990
      UCFEYR(7) = 1991
      UCFSYR(8) = 1990
      UCFEYR(8) = 1991
      UCFSYR(9) = 1990
      UCFEYR(9) = 1991
      UCFSYR(10) = 2019        !make CTN phase in same as coal
      UCFEYR(10) = 2030
      UCFSYR(11) = 1990
      UCFEYR(11) = 1991
      UCFSYR(12) = 1990
      UCFEYR(12) = 1991
      UCFSYR(13) = 1990
      UCFEYR(13) = 1991
      UCFSYR(14) = 1990
      UCFEYR(14) = 1991
      UCFSYR(15) = 1990
      UCFEYR(15) = 1991
      UCFSYR(16) = 1990
      UCFEYR(16) = 1991
      UCFSYR(17) = 1990
      UCFEYR(17) = 1991
      UCFSYR(18) = 1990
      UCFEYR(18) = 1991
      UCFSYR(19) = 1990
      UCFEYR(19) = 1991
      UCFSYR(20) = 1990
      UCFEYR(20) = 1991
      UCFSYR(21) = 1990
      UCFEYR(21) = 1991
      UCFSYR(22) = 2005
      UCFEYR(22) = 2015
      UCFSYR(23) = 1990
      UCFEYR(23) = 1991
      UCFSYR(24) = 1990
      UCFEYR(24) = 1991
      UCFSYR(25) = 1990
      UCFEYR(25) = 1991
      UCFSYR(26) = 1990
      UCFEYR(26) = 1991
      UCFSYR(27) = 1990
      UCFEYR(27) = 1991
      UCFSYR(28) = 1990
      UCFEYR(28) = 1991
      UCFSYR(29) = 1990
      UCFEYR(29) = 1991
      UCFSYR(30) = 1990
      UCFEYR(30) = 1991
      UCFSYR(31) = 1990
      UCFEYR(31) = 1991

!     ASSIGN EFD GLOBAL PARAMETERS
!
      EIFPLT = UIFPLT                   ! MAX NUMBER OF PLANTS PER GROUP
      EIPGRP = EFD_D_DSP
      EIHGRP = EFD_D_RNW
      EIDGRP = EFD_D_DGN
!
!     Define Summer Months for the purpose of assigning summer or winter capacity to a season 1.0 => Summer Month 0.0 => Winter Months
!
      SUMMER_MONTHS(1)  = 0.0
      SUMMER_MONTHS(2)  = 0.0
      SUMMER_MONTHS(3)  = 0.0
      SUMMER_MONTHS(4)  = 1.0
      SUMMER_MONTHS(5)  = 1.0
      SUMMER_MONTHS(6)  = 1.0
      SUMMER_MONTHS(7)  = 1.0
      SUMMER_MONTHS(8)  = 1.0
      SUMMER_MONTHS(9)  = 1.0
      SUMMER_MONTHS(10) = 0.0
      SUMMER_MONTHS(11) = 0.0
      SUMMER_MONTHS(12) = 0.0

      EFD_HOURS = 0.0
      WSEAS = 0.0
      ECP_HOURS = 0.0
      ECP_SUMMER_SHR = 0.0
      DO IHR = 1 , 24
         DO IDTYP = 1 , 3
            DO IMO = 1 , 12
               IECP = HrToECPSeas(IMO,IDTYP,IHR)
               ECP_HOURS(0) = ECP_HOURS(0) + IDAYTQ(IDTYP,IMO)
               ECP_HOURS(IECP) = ECP_HOURS(IECP) + IDAYTQ(IDTYP,IMO)
               ECP_SUMMER_SHR(0) = ECP_SUMMER_SHR(0) + IDAYTQ(IDTYP,IMO) * SUMMER_MONTHS(IMO)
               ECP_SUMMER_SHR(IECP) = ECP_SUMMER_SHR(IECP) + IDAYTQ(IDTYP,IMO) * SUMMER_MONTHS(IMO)
               IEFD = HrToEFDSeas(IMO,IDTYP,IHR)
               EFD_HOURS(0) = EFD_HOURS(0) + IDAYTQ(IDTYP,IMO)
               EFD_HOURS(IEFD) = EFD_HOURS(IEFD) + IDAYTQ(IDTYP,IMO)
               WSEAS(0) = WSEAS(0) + IDAYTQ(IDTYP,IMO) * SUMMER_MONTHS(IMO)
               WSEAS(IEFD) = WSEAS(IEFD) + IDAYTQ(IDTYP,IMO) * SUMMER_MONTHS(IMO)
            END DO
         END DO
      END DO

      DO IECP = 0 , ECPns
         ECP_SUMMER_SHR(IECP) = ECP_SUMMER_SHR(IECP) / ECP_HOURS(IECP)
      END DO

      DO IEFD = 0 , EFDnS
         WSEAS(IEFD) = WSEAS(IEFD) / EFD_HOURS(IEFD)
      END DO

      IF (CURIYR+UHBSYR .EQ. UESTYR .AND. IRG .EQ. 1) THEN
         WRITE(18,7392) CURIYR+UHBSYR,CURITR,(ECP_SUMMER_SHR(IECP),IECP=0,ECPns)
 7392    FORMAT(1X,"ECP_SUMMER_SHR",2(":",I4),10(":",F6.3))
         WRITE(18,7393) CURIYR+UHBSYR,CURITR,(WSEAS(IEFD),IEFD=0,EFDnS)
 7393    FORMAT(1X,"WSEAS",2(":",I4),10(":",F6.3))
      END IF
!
      DAYS = 0
      DO IMO = 1 , 12
         DO IDTYP = 1 , 3
            DAYS(IMO) = DAYS(IMO) + IDAYTQ(IDTYP,IMO)
         END DO
      END DO
!
      MONTHS = NINT(12.0/REAL(EENSP))
!
      MOTOSP = 0.0
      MOTOESP = 0.0
      DO IHR = 1 , 24
         DO IDTYP = 1 , 3
            DO IMO = 1 , 12
               IECP = HrToECPSeas(IMO,IDTYP,IHR)
               MOTOESP(0,IMO) = MOTOESP(0,IMO) + IDAYTQ(IDTYP,IMO)
               MOTOESP(IECP,IMO) = MOTOESP(IECP,IMO) + IDAYTQ(IDTYP,IMO)
               IEFD = HrToEFDSeas(IMO,IDTYP,IHR)
               MOTOSP(0,IMO) = MOTOSP(0,IMO) + IDAYTQ(IDTYP,IMO)
               MOTOSP(IEFD,IMO) = MOTOSP(IEFD,IMO) + IDAYTQ(IDTYP,IMO)
            END DO
         END DO
      END DO

!     DETERMINE HOURS PER ECP SEASONAL PERIOD

      HRS_ECP = 0.0
      HRS_EFD = 0.0
      DO IHR = 1 , 24
         DO IDTYP = 1 , 3
            DO IMO = 1 , 12
               IECP = HrToECPSeas(IMO,IDTYP,IHR)
               HRS_ECP(IECP) = HRS_ECP(IECP) + IDAYTQ(IDTYP,IMO)
               IEFD = HrToEFDSeas(IMO,IDTYP,IHR)
               HRS_EFD(IEFD) = HRS_EFD(IEFD) + IDAYTQ(IDTYP,IMO)
            END DO
         END DO
      END DO
!
!     INITIALIZE PLANT GROUP NAMES, MAX FUEL SHARES AND CAPACITY BOUNDS
!
      DO IGRP = 1 , EIPGRP
         WASTS(IGRP) = IGRP
         WHYTP(IGRP) = 0
         ENPGRP(IGRP) = WNAME(IGRP)
         ECCFBD(IGRP,1) = WLOWER(IGRP)
         ECCFBD(IGRP,2) = WUPPER(IGRP)
         DO IVIN3 = 1 , EFD_D_VIN
            DO IOWN = 1 , USW_OWN
               ECSCAP(IGRP,IVIN3,IOWN) = 0.0
               ECSADD(IGRP,IVIN3,IOWN) = 0.0
            END DO
         END DO
         DO IOWN = 1 , USW_OWN
            ECSRET(IGRP,IOWN) = 0.0
         END DO
      END DO                                                      ! IGRP

!     For Gas Steam Capacity Converted that are coal cofiring identify in CRPGRP instead of in UTIL based on fuel share options

      UCAPNGCF(JRG,CURIYR) = 0.0
      IF (JRG .EQ. 1) THEN
         UCAPNGCF(MNUMNR,CURIYR) = 0.0
      END IF      
      
      DO IOWN = 1 , USW_OWN
         ECSCAPSQ(IOWN) = 0.0
      END DO                                                      ! IGRP
       IF ((CURIYR+1989).EQ.UPSTYR.AND.CURITR.GT.1 .OR. (CURIYR+1989).GT.UPSTYR ) THEN
        IF (IRG .EQ. 1) THEN
           R_PLTS = 0
          DO IGRP = 1,MAX_CL
             R_RY(IGRP) = 0
             R_IGRP(IGRP) = 0
          ENDDO
        ENDIF
      ENDIF
!
!        COFIRING CAPACITY (AVAILABLE)
!
      IF (JRG .EQ. 1) THEN
         DO CRG = 1 ,NDREG
            DO IGRP = 1 , ECP_D_RCF
               DO XYR = CURIYR , UNYEAR + UNXPH
                  UCF_TCAP(IGRP,CRG,XYR) = 0.0
               END DO
               UPCFBTU(IGRP,CRG) = 0.0
            END DO
         END DO
         DO KRG = 1 , UNRGNS
          DO CRG = 1 ,NDREG
            DO IGRP = 1 , ECP_D_RCF
               DO XYR = CURIYR , UNYEAR + UNXPH
                  UCF_TCAP1(IGRP,KRG,CRG,XYR) = 0.0
               END DO
            END DO
          END DO
         END DO
      END IF
!
      IF (FULLCYR .EQ. UESTYR) THEN
!
         IF (JRG .EQ. 1) THEN
            NUM_CL = NUM_CL_UNIT
            ECL_IGRP = 0.0
            ECL_ICFG = 0.0
            UCL_CGRP2 = 0
            UCL_CGRP = 0
            UNUC_CGRP = 0
            NUM_NUC = 0
            ENUC_GRP = 0
            ENUC_IGRP = 0
            NUM_NGBS = 0
!
!           INITIALIZE PLANNED SO2 RETROFIT ARRAY
!
            EMELRETP = 0.0
            EMELNOXP = 0.0
            EMELSCRP = 0.0
            EMELNCRP = 0.0
         END IF
!
!        COFIRING CAPACITY (AVAILABLE)
!
         IF (JRG .EQ. 1) THEN
            DO XYR = 1 , UNYEAR + UNXPH
               DO CRG = 1 ,NDREG
                  DO IGRP = 1 , ECP_D_RCF
                     UCF_RCAP(IGRP,CRG,XYR) = 0.0
                  END DO
               END DO
              DO KRG = 1 , UNRGNS
               DO CRG = 1 ,NDREG
                  DO IGRP = 1 , ECP_D_RCF
                     UCF_RCAP1(IGRP,KRG,CRG,XYR) = 0.0
                  END DO
               END DO
              END DO
            END DO
         END IF
!
!        INITIALIZE OMADJ FOR FIRST YEAR (BEFORE EFP IS CALLED) AND IN CANADIAN RUN
!
         DO JYR = 1 , UNYEAR
            DO IEFD = 1 , EFD_D_CAP
               OMADJ(IEFD,JYR) = 1.0
            END DO
         END DO
!
!        INITIALIZE EFP'S BUILD LINKED LIST
!
         CALL UINIBLD(IRG)
         DO VINTAGE = 1 , EFP_D_VIN
            DO FTYPE = 1 , EFP_D_CAP
               DO OTYPE = 1 , EFP_D_OWN
                  EOMW(VINTAGE,FTYPE,OTYPE,JRG) = 0.0
               END DO
            END DO
         END DO
         PRMSG = 1
      ELSE
         PRMSG = 0
      END IF
!
      DO IGRP = 1 , EIHGRP
         WASTS(IGRP + EIPGRP) = 0
         WHYTP(IGRP + EIPGRP) = IGRP + EIPGRP
         ENHGRP(IGRP) = WNAME(IGRP + EIPGRP)
         DO IVIN3 = 1 , EFD_D_VIN
            DO IOWN = 1 , USW_OWN
               EHSCAP(IGRP,IVIN3,IOWN) = 0.0
               EHSADD(IGRP,IVIN3,IOWN) = 0.0
            END DO
            IF (IGRP .EQ. 1) THEN
               EHSCAPP2(IVIN3) = 0.0
               EHSADDP2(IVIN3) = 0.0
            END IF
         END DO
         DO IOWN = 1 , USW_OWN
            EHSRET(IGRP,IOWN) = 0.0
         END DO
      END DO
!
      DO IGRP = 1 , EIDGRP
         WASTS(IGRP + EIPGRP + EIHGRP) = 0
         WHYTP(IGRP + EIPGRP + EIHGRP) = IGRP + EIPGRP + EIHGRP
         ENDGRP(IGRP) = WNAME(IGRP + EIPGRP + EIHGRP)
         EDCFBD(IGRP,1) = WLOWER(IGRP + EIPGRP + EIHGRP)
         EDCFBD(IGRP,2) = WUPPER(IGRP + EIPGRP + EIHGRP)
         DO IVIN3 = 1 , EFD_D_VIN
            DO IOWN = 1 , USW_OWN
               EDSCAP(IGRP,IVIN3,IOWN) = 0.0
               EDSADD(IGRP,IVIN3,IOWN) = 0.0
            END DO
         END DO
         DO IOWN = 1 , USW_OWN
            EDSRET(IGRP,IOWN) = 0.0
         END DO
      END DO
!
!     INITIALIZE
!
      DO IECP = 1 , ECP_D_CAP
         T_ECP(IECP) = 0
         T_AVG(IECP,IRG) = 0.0
         T_CAP(IECP,IRG) = 0.0
         T_GEN(IECP,IRG) = 0.0
         T_HRATE(IECP,IRG) = 0.0

         ECP_GEN(IECP) = 0.0
         P_GEN(IECP) = 0.0
         P_HTRT0(IECP) = 0.0

         P_HTRT_MAX(IECP) = 0.0

         DO XYR = 1 , ECP_D_XPH
            ECP_GEN_XPH(XYR,IECP) = 0.0
         END DO
         ECP_VOM(IECP) = 0.0
         ECP_NOX(IECP) = 0.0
         RET_AVG(IECP,IRG) = 0.0
         RET_MAX(IECP) = -999.999
         RET_MIN(IECP) = 999.999
         MUSTRUN(IECP,IRG) = 0.0

         IF (IRG .EQ. 1)THEN
            DO IYR2 = 1 , UNYEAR
               ESTYRADD(IECP,IYR2) = 0.0
               ESTYRPLN(IECP,IYR2) = 0.0
            END DO
         END IF
         DO RGRP = 1 , ECP_D_RET
            RET_FRST(RGRP,IECP,IRG) = 0
            HTRT_FRST(RGRP,IECP,IRG) = 0
            HTRT_UNITS(RGRP,IECP,IRG) = 0
            DO IFLRG = 0 , EFD_D_MFRG
               EPGCAP(IFLRG,RGRP,IECP) = 0.0
               HTRT_EPGCAP(IFLRG,RGRP,IECP,IRG) = 0.0
            END DO
            EPGFOM(RGRP,IECP) = 0.0
         END DO
      END DO
!
      DO I_CNFG = 1 , NUM_CNFG
         IF (UCL_CNFG(3,I_CNFG) .GT. 0) THEN
            IECP = UCL_ECP(I_CNFG)
            T_ECP(IECP) = 1
         END IF
      END DO
!
      IF (JRG .EQ. 1) THEN
         DO FREC = 1 , WPLT_D_REC
            RET_NEXT(FREC) = 0
            RET_VAL(FREC) = 0.0
            RET_CAP(FREC) = 0.0
            RET_FOM(FREC) = 0.0
            RET_FRG(FREC) = 0
         END DO
         C2_GEN = 0.0
         C2_HTRT = 0.0
!
!        INITIALIZE COUNTERS FOR NON-REVENUE YEARS
         IF ((CURIYR + UHBSYR) .EQ. UESTYR)THEN
            EOTH_RYRS = 0
            ECL_RYRS = 0
            ENUC_RYRS = 0
         END IF
      END IF
!
!     INITIALIZE ECP CAPACITY SUMS FOR LP VERSION OF ECP
!
      EPESCFC = 0.0
      DO YEAR = 1 , ECP_D_XPH
!
!        INITIALIZE GAS-FIRED CAPACITY (ONLY 1ST TIME)
!
         IF (JRG .EQ. 1) THEN
            DO IFLRG = 1 , NNGEM
               EGFCAP(IFLRG,YEAR) = 0.0
               EGICAP(IFLRG,YEAR) = 0.0
               EGCCAP(IFLRG,YEAR) = 0.0
            END DO
            DO IFLRG = 1 , NDRGG
               ECLCAP(IFLRG,YEAR) = 0.0
            END DO
            DO IGRP = 1 , ECP_D_DSP
               DO CRG = 1 , NDREG
                  WDSHR(IGRP,YEAR,CRG) = 0.0
                  WDSUM(IGRP,YEAR,CRG) = 0.0
               END DO
            END DO
         END IF
         N_GRP = 0
         DO IGRP = 1, ECP_D_CAP
            N_GRP = MAX(N_GRP , UPTTYP(IGRP))
            P_HTRT0(IGRP) = 0.0
            P_FOM(IGRP) = 0.0
            P_VOM(IGRP) = 0.0
            P_GEN(IGRP) = 0.0
            DO XYR = 1 , ECP_D_XPH
               P_GEN_XPH(XYR,IGRP) = 0.0
            END DO
            WPTTYP(IGRP,IRG) = 0
            P_TTYP(IGRP) = 0
            DO ISP = 1 , ECP_D_MSP
               EP_SP_CAP_FAC(ISP,IGRP,YEAR) = 0.0
            END DO
            EXSGEN(IGRP,IRG) = 0.0
            IF (IRG .EQ. 1)EXSGEN(IGRP,MNUMNR) = 0.0
         END DO
         DO IGRP = 1 , ECP_D_DSP
            JGRP = UCPDSPI(IGRP)
            DO IFLRG = 0 , UNFRGN
               EPECAP(IFLRG,UCPDSPI(IGRP),YEAR) = 0.0
               EPECAP_MR(IFLRG,UCPDSPI(IGRP),YEAR) = 0.0
            END DO
            EPECFC(UCPDSPI(IGRP),YEAR) = 0.0
            P_CAP(UCPDSPI(IGRP),YEAR) = 0.0
            P_MCF(UCPDSPI(IGRP),YEAR) = 0.0
            IF ((UPVTYP(JGRP) .EQ. 0) .OR. (UPTOPR(JGRP) .EQ. 3)) THEN
               EPFOM(JGRP) = 0.0
               EPVOM(JGRP) = 0.0
               EPPHRT0(JGRP) = 0.0
               DO INOX = 1 , NOX_D_GRP
                  DO XYR = 1 , ECP_D_XPH
                     DO ISP = 1 , ECP_D_MSP
                        EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                        P_NOX(ISP,XYR,JGRP,INOX) = 0.0
                     END DO
                  END DO
               END DO
            END IF
            EPUNIT(UCPDSPI(IGRP),YEAR) = 0.0
            IF (YEAR .EQ. 1) EPNCAP(JGRP) = 0.0
            DO ISCR = 1 , ECP_D_SCR
               EPSCAP(IGRP,ISCR) = 0.0
               EPSOVR(IGRP,ISCR) = 0.0
               EPNSCR(IGRP,ISCR) = 0
            END DO
         END DO
!
         DO IGRP = 1 , ECP_D_INT
            JGRP = UCPINTI(IGRP)
            KGRP = UIRINTI(IGRP)
            DO IFLRG = 0 , UNFRGN
               EPECAP(IFLRG,JGRP,YEAR) = 0.0
               EPECAP_UPV(IFLRG,JGRP,YEAR) = 0.0
               EPECAP_MR(IFLRG,JGRP,YEAR) = 0.0
            END DO
            EPECFC(JGRP,YEAR) = 0.0
            EPECFC_UPV(JGRP,YEAR) = 0.0
            P_CAP(JGRP,YEAR) = 0.0
            P_MCF(JGRP,YEAR) = 0.0
            IF ((UPVTYP(JGRP) .EQ. 0) .OR. (UPTOPR(JGRP) .EQ. 3)) THEN
               EPVOM(JGRP) = 0.0
               EPFOM(JGRP) = 0.0
               DO INOX = 1 , NOX_D_GRP
                  DO XYR = 1 , ECP_D_XPH
                     DO ISP = 1 , ECP_D_MSP
                        EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                        P_NOX(ISP,XYR,JGRP,INOX) = 0.0
                     END DO
                  END DO
               END DO
            END IF
            EPUNIT(JGRP,YEAR) = 0.0
            IF (YEAR .EQ. 1) EPNCAP(JGRP) = 0.0
         END DO

         DO IGRP = 1 , ECP_D_STO
            JGRP = UCPSTOI(IGRP)
            IF (JGRP .GT. 0) THEN
               DO IFLRG = 0 , UNFRGN
                  EPECAP(IFLRG,JGRP,YEAR) = 0.0
                  EPECAP_MR(IFLRG,JGRP,YEAR) = 0.0
               END DO
               EPECFC(JGRP,YEAR) = 0.0
               P_CAP(JGRP,YEAR) = 0.0
               P_MCF(JGRP,YEAR) = 0.0
               IF ((UPVTYP(JGRP) .EQ. 0) .OR. (UPTOPR(JGRP) .EQ. 3)) THEN
                  EPVOM(JGRP) = 0.0
                  EPFOM(JGRP) = 0.0
                  DO INOX = 1 , NOX_D_GRP
                     DO XYR = 1 , ECP_D_XPH
                        DO ISP = 1 , ECP_D_MSP
                           EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                           P_NOX(ISP,XYR,JGRP,INOX) = 0.0
                        END DO
                     END DO
                  END DO
               END IF
               EPUNIT(UCPRNWI(IGRP),YEAR) = 0.0
               IF (YEAR .EQ. 1) EPNCAP(JGRP) = 0.0
            END IF
         END DO
!
         DO IGRP = 1 , ECP_D_RNW
            JGRP = UCPRNWI(IGRP)
            KGRP = UIRRNWI(IGRP)
            IF (JGRP .GT. 0) THEN
               DO IFLRG = 0 , UNFRGN
                  EPECAP(IFLRG,JGRP,YEAR) = 0.0
                  EPECAP_MR(IFLRG,JGRP,YEAR) = 0.0
               END DO
               EPECFC(JGRP,YEAR) = 0.0
               P_CAP(JGRP,YEAR) = 0.0
               P_MCF(JGRP,YEAR) = 0.0
               IF ((UPVTYP(JGRP) .EQ. 0) .OR. (UPTOPR(JGRP) .EQ. 3)) THEN
                  EPVOM(JGRP) = 0.0
                  EPFOM(JGRP) = 0.0
                  DO INOX = 1 , NOX_D_GRP
                     DO XYR = 1 , ECP_D_XPH
                        DO ISP = 1 , ECP_D_MSP
                           EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                           P_NOX(ISP,XYR,JGRP,INOX) = 0.0
                        END DO
                     END DO
                  END DO
               END IF
               EPUNIT(UCPRNWI(IGRP),YEAR) = 0.0
               IF (YEAR .EQ. 1) EPNCAP(JGRP) = 0.0
               RNW_CFC(IGRP) = 0.0
               RNW_MCFC(IGRP) = 0.0
               DO ISP = 1 , EPNMSP
                 ECFCAP(IGRP,ISP,YEAR) = 0.0
                 GECPSCAP(IGRP,ISP) = 0.0
                 ECAPCF(IGRP,ISP,YEAR) = 0.0
                 RSCFC(IGRP,ISP) = 0.0
               END DO
               DO ISP = 1 , EPNMSP
                 EPRSCFC(IGRP,:,ISP) = 0.0
               END DO
            END IF
         END DO
!
         DO IGRP = 1 , ECP_D_DGN
            JGRP = UCPDGNI(IGRP)
            DO IFLRG = 0 , UNFRGN
               EPECAP(IFLRG,JGRP,YEAR) = 0.0
               EPECAP_MR(IFLRG,JGRP,YEAR) = 0.0
            END DO
            EPECFC(JGRP,YEAR) = 0.0
            P_CAP(JGRP,YEAR) = 0.0
            P_MCF(JGRP,YEAR) = 0.0
            IF ((UPVTYP(JGRP) .EQ. 0) .OR. (UPTOPR(JGRP) .EQ. 3)) THEN
               EPVOM(JGRP) = 0.0
               EPFOM(JGRP) = 0.0
               DO INOX = 1 , NOX_D_GRP
                  DO XYR = 1 , ECP_D_XPH
                     DO ISP = 1 , ECP_D_MSP
                        EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                        P_NOX(ISP,XYR,JGRP,INOX) = 0.0
                     END DO
                  END DO
               END DO
            END IF
            EPUNIT(UCPDGNI(IGRP),YEAR) = 0.0
            IF (YEAR .EQ. 1) EPNCAP(JGRP) = 0.0
         END DO
!
!        INITIALIZE OWN USE CAPACITY - FOR DISPIN & BILDIN COMMONS
!
         EOUIPP = 0
         EOUNT = 0
         EOUNCP = 0
         EOUCCP = 0
         UOUNCP(IRG,CURIYR) = 0.0
         UOUCCP(IRG,CURIYR) = 0.0
         CGOTGEN(JRG,IYR - UHBSYR,1) = 0
         CGOTGEN(JRG,IYR - UHBSYR,2) = 0
!        INITIALIZE NONTRADITIONAL GENERATION FOR RPS ACCOUNTING  ! move to unugs, each iteration
!        EDRPSRG = 0.0
!        EDRPSTG = 0.0
!        EDRPSRO = 0.0
!        EDRPSTO = 0.0
      END DO
!
!     Initialize new FTAB regional mapping capacity variables
!
      IF (FULLCYR .EQ. UESTYR .AND. IRG .EQ. 1) THEN
         IF (PRTDBGE .EQ. 9) WRITE(UF_DBG,*)'CRPGRP:year=',IYR,'  Initialize'
         DO I = 1 , MNUMNR
            DO J = 1 , UNYEAR
               UCAPINR(I,J) = 0.0
               UCAPOTR(I,J) = 0.0
               UCAPSRV(I,J) = 0.0
               DO K = 1 , 2
                  UADDINR(K,I,J) = 0.0
                  UADDOTR(K,I,J) = 0.0
                  UADDSRV(K,I,J) = 0.0
               END DO
            END DO
         END DO
      END IF

!     Initialize Negative Revenue Info if Using Lag Year Results

      IF ((CURIYR + UHBSYR) .GE. (UPSTYR + UPPLYR(WICT)) .AND. JRG .EQ. 1 .AND. USW_NRVTYP .EQ. 2) THEN
         DO KRG = 1 , MNUMNR
            UNRVCOL(KRG,CURIYR) = 0.0
            UNRVCCY(KRG,CURIYR) = 0.0
            UNRVSTM(KRG,CURIYR) = 0.0
            UNRVNUC(KRG,CURIYR) = 0.0
            UNCPCOL(KRG,CURIYR) = 0.0
            UNCPCCY(KRG,CURIYR) = 0.0
            UNCPSTM(KRG,CURIYR) = 0.0
            UNCPNUC(KRG,CURIYR) = 0.0
         END DO
      END IF
!
!        Resource cost - CP_ADD
!
      CP_ADD(IRG,CURIYR) = 0.0
      IF (IRG .EQ. 1) THEN
         CP_ADD(MNUMNR,CURIYR) = 0.0
      ENDIF

!     Initialize 45Q Tax Credit Start Year

      IF (FULLCYR .EQ. UESTYR .AND. IRG .EQ. 1) THEN
         ULORGN = 0
      END IF

!     Initialize Group Heatrate Info

      IF (JRG .EQ. 1) THEN
         HTRT_ECP = 0.0
         HTRT_EFD = 0.0
         ULSINDX = 999999
         ULRINDX = 0
         ULSCAP_ECP = 0.0
         ULSCAP_EFD = 0.0
         ULTGEN_ECP = 0.0
         ULMRUN = 0
         LCAP_EFD = 0.0
         ULAGE = 999
         TST_TGEN = ULTGEN
         ULSTATE = 0
         
         DO PLYR = 1, UNXPH
             ULTGEN_ECP(:,PLYR) = 1.0
         ENDDO
      END IF

!     Initialize NGBS Group Arrays

      IF (JRG .EQ. 1) THEN
         CAP_NGBS = 0.0
         ENG_SP_CAP_FAC = 0.0
         ENG_CF = 0.0
         ENG_FOM = 0.0
         ENG_VOM = 0.0
         ENG_RVAL = 0.0
         ENG_CAP = 0.0
         ENG_CCS_O = 0.0
         ENG_CCS_F = 0.0
         ENG_CCS_V = 0.0
         ENG_CCS_R = 0.0
         ENG_CCS_H = 0.0
         ENG_CCS_C = 0.0
         NUM_NGBS = 0
         ENG_RG = 0
         ENG_ECP = 0
         ENG_FLRG = 0
         ENG_GRP = 0
         ENG_MR = 0
         ENG_SYR = 0
         ENG_RYR = 0
         ENG_FREC = 0
         ENG_NREC = 0
         UNG_CGRP = 0
         UNG_WGRP = 0
      END IF

!     Initialize Coal Group Arrays

      IF (JRG .EQ. 1) THEN

         DO KYR = 1 , MNUMYR
            IF (TCCF_GL(KYR,1) .LT. 0.10) TCCF_GL(KYR,1) = 1.0
            IF (TCCV_GL(KYR,1) .LT. 0.10) TCCV_GL(KYR,1) = 1.0
         END DO

         T_VADJ = 0.0
         T_WGRP = 0.0
         DO I_COAL = 1 , MAX_CL
            ECL_RG(I_COAL) = 0
            ECL_RYR(I_COAL) = 0
            IF (FULLCYR .EQ. UESTYR) THEN
               DO TRG = 1 , MNUMNR
                  ECL_RG_CAP(TRG,I_COAL) = 0.0
               END DO
               TMP_RCFG = 0
            END IF
            ECL_CLRG(I_COAL) = 0
            ECL_YR(I_COAL) = 0
            ECL_MGRP(I_COAL) = 0
            ECL_CMB_O(I_COAL) = 0.0
            ECL_CMB_F(I_COAL) = 0.0
            ECL_CMB_V(I_COAL) = 0.0
            ECL_CMB_R(I_COAL) = 0.0
            ECL_SNCR_O(I_COAL) = 0.0
            ECL_SNCR_F(I_COAL) = 0.0
            ECL_SNCR_V(I_COAL) = 0.0
            ECL_SNCR_R(I_COAL) = 0.0
            ECL_SCR_O(I_COAL) = 0.0
            ECL_SCR_F(I_COAL) = 0.0
            ECL_SCR_V(I_COAL) = 0.0
            ECL_SCR_R(I_COAL) = 0.0
            ECL_FGD_O(I_COAL) = 0.0
            ECL_FGD_F(I_COAL) = 0.0
            ECL_FGD_V(I_COAL) = 0.0
            ECL_FGD_R(I_COAL) = 0.0
            ECL_DSI_O(I_COAL) = 0.0
            ECL_DSI_F(I_COAL) = 0.0
            ECL_DSI_V(I_COAL) = 0.0
            ECL_DSI_R(I_COAL) = 0.0
            ECL_CCS_O(I_COAL) = 0.0
            ECL_CCS_F(I_COAL) = 0.0
            ECL_CCS_V(I_COAL) = 0.0
            ECL_CCS_R(I_COAL) = 0.0
            ECL_CCS_H(I_COAL) = 0.0
            ECL_CCS_C(I_COAL) = 0.0
            ECL_FF_O(I_COAL) = 0.0
            ECL_FF_F(I_COAL) = 0.0
            ECL_FF_V(I_COAL) = 0.0
            ECL_HRI_O(I_COAL) = 0.0
            ECL_HRI_F(I_COAL) = 0.0
            ECL_HRI_V(I_COAL) = 0.0
            ECL_HRI_H(I_COAL) = 0.0
            ECL_ESP_O(I_COAL) = 0.0
            ECL_CFB_O(I_COAL) = 0.0
            ECL_CFB_F(I_COAL) = 0.0
            ECL_CFB_V(I_COAL) = 0.0
            ECL_ACEYR(I_COAL) = 0.0
            ECL_ACEST(I_COAL) = 0.0
            ECL_CL_NG_COST(I_COAL) = 0.0
            ECL_CL_NG_TRAN(I_COAL) = 0.0
            DO ISP = 1 , ECP_D_MSP
               ECL_SP_CAP_FAC(ISP,I_COAL) = 0.0
            END DO
            C_CAP(I_COAL) = 0.0
            C_ECP(I_COAL) = 0
            C_GRP(I_COAL) = 0
            C_CF(I_COAL) = 0.0
            DO XNOX = 1 , NOX_GRP
               C_NXG(XNOX,I_COAL) = 0
            END DO
            C_VADJ(I_COAL) = 0.0
            C_FOM(I_COAL) = 0.0
            C_NXR(I_COAL) = 0.0
            C_SO2R(I_COAL) = 0.0
            C_HTRT(I_COAL) = 0.0
            ECL_FREC(I_COAL) = 0
            ECL_RVAL(I_COAL) = 0.0
            DO XYR = 1 , UNXPH
               ECL_GRP(XYR,I_COAL) = 0
               ECL_ECP(XYR,I_COAL) = 0
               IF (FULLCYR .EQ. UESTYR) THEN
                  SV_RCFG(XYR,I_COAL) = 0
               ELSE
                  SV_RCFG(XYR,I_COAL) = ECL_RCFG(XYR,I_COAL)
               END IF
               ECL_RCFG(XYR,I_COAL) = 0
               DO XNOX = 1 , NOX_GRP
                  ECL_NXG(XYR,XNOX,I_COAL) = 0
               END DO
               ECL_CAP(XYR,I_COAL) = 0.0
               ECL_FOM(XYR,I_COAL) = 0.0
               ECL_VADJ(XYR,I_COAL) = 0.0
               ECL_CF(XYR,I_COAL) = 0.0
               ECL_NXR(XYR,I_COAL) = 0.0
               ECL_SO2R(XYR,I_COAL) = 0.0
            END DO
         END DO
         DO IP = 1 , WPLT_D_REC
            ECL_NREC(IP) = 0
            ENUC_NREC(IP) = 0
         END DO
         DO I_NUC = UNRGNS*2 + 1 , MAXNUC
            DO TRG = 1 , MNUMNR
               ENUC_RG(TRG,I_NUC) = 0.0
            END DO
            ENUC_ECP(I_NUC) = 0
         END DO
         DO I_NUC = 1 , MAXNUC
            CAP_NUC2(I_NUC) = 0.0
            ENUC_FREC(I_NUC) = 0
            ENUC_HTRT(I_NUC) = 0.0
            ENUC_VOM(I_NUC) = 0.0
            ENUC_ASYR(I_NUC) = 0
            ENUC_ACST(I_NUC) = 0.0
            ENUC_ZECST(I_NUC) = 0
            ENUC_ZECY1(I_NUC) = 0
            ENUC_ZECYR(I_NUC) = 0
            ENUC_PTC(I_NUC) = 0.0
            IF (FULLCYR .EQ. UESTYR)THEN
               ENUC_ZECRN(I_NUC) = 0
               ENUC_ZECNM(I_NUC) = 0
            END IF
            ENUC_SUB(I_NUC) = 0.0
            DO XYR = 1 , UNXPH
               ENUC_CAP(XYR,I_NUC) = 0.0
               ENUC_CF(XYR,I_NUC) = 0.0
            END DO
            DO XYR = 1 , UNFPH
               ENUC_FOM(XYR,I_NUC) = 0.0
            END DO
            DO ISP = 1 , ECP_D_MSP
               ENUC_SP_CAP_FAC(ISP,I_NUC) = 0.0
            END DO
         END DO

      END IF

!
!     fill in capacity credit by region/type to assign capacity payments
!
        DO IP = 1, ECP_D_CAP
          CAPCCR(IP) = UPCCR(IP)
        ENDDO

        DO IP = 1, ECP_D_RNW + ECP_D_STO + ECP_D_INT
          CAPCCR(ECP_D_DSP+IP) = EPIRCCR(IP)
        ENDDO

        CAPCCR(WIHY) = RNWFAC

!     write(6,2222) CURIYR,CURITR,IRG,(CAPCCR(IP),IP=1,ECP_D_CAP)
2222  FORMAT(1x,'capccr ',3I4,59F10.4)

!
         IRECLL = 0
         I_COALL = 0
         I_NUCL = 0
         W_GRPL = 0

         EC_45Q = 0
         ECNOCCS = 0.0

!CCCCCCCCCCCCCCCCCCCCC
!     END INITIALIZATION C
!CCCCCCCCCCCCCCCCCCCCC
!
!     KEEP TRACK OF NUMBER OF DISPATCHABLE AND RENEWABLE PLANT GROUPS
!
      ECNTP = 1                                    ! EFD DSP PLNT GROUPS
      EHNTP = 1                                    ! EFD RNW PLNT GROUPS
      EDNTP = 1                                    ! EFD DGN PLNT GROUPS
      ISEQ = 0                               ! NUMBER OF CALLS TO GETPLT
      ECNMR = 0                  ! NUMBER OF DISPATCHABLE MUST RUN UNITS
      EDNMR = 0                  ! NUMBER OF DIST GEN MUST RUN UNITS
!
!     MAIN PLANT GROUP LOOP C
!
      TOT_GRP = WNGRPS(IRG)

      DO XYR = 1 , UNXPH
         IF (XYR .LT. UNXPH) THEN
            CYR_INDEX(XYR) = (CURIYR + XYR - 1 + UHBSYR) * 100
            LYR_INDEX(XYR) = (CURIYR + XYR - 1 + UHBSYR) * 100 + 12
         ELSE
            CYR_INDEX(XYR) = (CURIYR + XYR - 1 + UHBSYR) * 100
            LYR_INDEX(XYR) = (CURIYR + UNFPH - 1 + UHBSYR) * 100 + 12
         END IF
      END DO

      T1 = 0.0
      T2 = 0.0

      DO 500 IGRP = 1 , TOT_GRP
!
         IF (ECNTP .GE. EFD_D_MPG) WRITE(6,*) ' CRPGRP ERROR: # ECNTP=', &
             ECNTP,' EXCEEDS EFD_D_MPG=',EFD_D_MPG
         IF (EHNTP .GE. EFD_D_MHG) WRITE(6,*) ' CRPGRP ERROR: # EHNTP=', &
             EHNTP,' EXCEEDS EFD_D_MHG=',EFD_D_MHG
         IF (EDNTP .GE. EFD_D_MDG) WRITE(6,*) ' CRPGRP ERROR: # EDNTP=', &
             EDNTP,' EXCEEDS EFD_D_MDG=',EFD_D_MDG
         IF (IGRP .GE. WPLT_D_GRP) WRITE(6,*) ' CRPGRP ERROR: # IGRP=', &
             IGRP,' EXCEEDS WPLT_D_GRP=',WPLT_D_GRP
!
         IRECL = W_INT(IRG,IGRP)
         RTEST = IRECL
         ITEST = 0
!
         DO ISP = 1 , EENSP
            CFCAP(ISP) = 0.0
            GSCAP(ISP) = 0.0
            CAPCF(ISP) = 0.0
            TNOX_R(ISP) = 0.0
            DO INOX = 1 , NOX_GRP
               TNOX_RC(INOX,ISP) = 0.0
            END DO
         END DO
         DO IFL = 1 , EFD_D_NFL
            MXFSHR(IFL) = 0.0
         END DO
         EYSCAP = 0.0
         EYFCST = 0.0
         EYCCST = 0.0
         EYRETR = 0.0
         GCAP = 0.0
         GSUB = 0.0
         AVGCF = 0.0
         CAPHR = 0.0
         CAPVOM = 0.0
         CAPFOM = 0.0
         CAPXTR = 0.0
         CAPGSUB = 0.0
         CAPCSUB = 0.0
         CAPSCR = 0.0
         FREC_GP = 1
         ECUNIT(ECNTP) = 0.0
         EHUNIT(EHNTP) = 0.0
         EDUNIT(EDNTP) = 0.0
         CYEAR_ECP = 0
         CYEAR_EFD = 0
!
!        PLANT LOOP: READ ALL PLT RECS FOR GRP C
!
         DO WHILE (IRECL .GT. 0)
!
            CLtNG_Typ = 0

            ISEQ = ISEQ + 1
            CALL GETPLT(IRECL)

! update  max CF for new gas CC for EPA 111 
            IF (USW_EPA111 .EQ. 1 .AND. CURIYR + UHBSYR .GE. UEPA_NWGSYR .AND. WVIN .GT. 1 .AND. (WECPt .EQ. WIAC .OR. WECPt .EQ. WICC)) THEN
                W_CF = MIN(W_CF,UEPA_NWGSCF)
            ENDIF
           
! update retirement year for coal/gas cofiring plants if EPA 111 in place
            IF (USW_EPA111 .EQ. 1 .AND. (WECPt .EQ. WING .AND. WFL(2) .GT. 0) .AND. W_RYR .GT. UEPA_CLCFYR) THEN
                W_RYR = UEPA_CLCFYR
                W_RMO = 12
            ENDIF
            
! update coal/gas shares if EPA 111 is in place (at least 40% after 2030)
            IF (USW_EPA111 .EQ. 1 .AND. (WECPt .EQ. WING .AND. WFL(2) .GT. 0) .AND. CURIYR + UHBSYR .GE. UEPA_GSYR) THEN
                W_FSHR(1) = MAX(W_FSHR(1),1.0 - UEPA_CGSH)     
                W_FSHR(2) = 1 - W_FSHR(1)
            ENDIF            
            
           CALL STRPLT(IRECL) 
            
!           ASSIGN CCSCAPA for units with carbon capture but no capacity penality (CCSCAPA)
!
!           IF (WECPt .LE. ECP_D_DSP) THEN
!              IF (UPPCEF(WECPt) .GT. 0.0 .AND. CCSCAPA .EQ. 0.0) THEN
!                 CCSCAPA = 1.0 - UECP_CPEN_ADJ(WECPt)
!                 CALL STRPLT(IRECL)
!              END IF
!           END IF

!           implement nuclear retirement / derate methodology

            IF (WEFDT .EQ. UICNU .AND. W_SYR .LE. CURIYR + UHBSYR .AND. W_RYR .GE. CURIYR + UHBSYR)  THEN   ! nuclear plant in operation this year
              IF (WVIN .NE. 2 .AND. WVIN .NE. 3) THEN                                                          !only apply to existing plants
               IF (NUCDRAT(CURIYR,MNUMNR) .NE. 1.0) THEN
                  NUCADJ = WC_SUM * (1.0 - NUCDRAT(CURIYR,MNUMNR))
                  WC_SUM = WC_SUM * NUCDRAT(CURIYR,MNUMNR)                                                      ! apply derate factor for year and region
                  WC_WIN = WC_WIN * NUCDRAT(CURIYR,MNUMNR)
                  WC_NP  = WC_NP  * NUCDRAT(CURIYR,MNUMNR)
                  CALL STRPLT(IRECL)
               ELSE
                  NUCADJ = WC_SUM * (1.0 - NUCDRAT(CURIYR,WNOWN))
                  WC_SUM = WC_SUM * NUCDRAT(CURIYR,WNOWN)                                                       ! apply derate factor for year and region
                  WC_WIN = WC_WIN * NUCDRAT(CURIYR,WNOWN)
                  WC_NP  = WC_NP  * NUCDRAT(CURIYR,WNOWN)
                  CALL STRPLT(IRECL)
               ENDIF
               URTNUCX(CURIYR,WNOWN) = URTNUCX(CURIYR,WNOWN) + NUCADJ
               IF (CURIYR .LT. MNUMYR) THEN
                 IF (NUCDRAT(CURIYR+1,WNOWN) .EQ. 0.0) THEN            !derate of 0 => retire
                  W_RYR = CURIYR + UHBSYR
                  W_RMO = 12
                  CALL STRPLT(IRECL)
                 ENDIF
               ENDIF
              ENDIF
            ENDIF

!           Record earliest year the unit added CO2 capture but only if unit came on while 45Q policy is in effect

!            IF (WECPt .LE. ECP_D_DSP) THEN     !BECCS is > ECP_D_DSP but needs to go through here - UPPCEF will keep out anything else
               IF (UPPCEF(WECPt) .GT. 0.0) THEN
                  IF (UPVTYP(WECPt) .EQ. 0) THEN            ! Existing Capacity
                     IF (W_SYR .GE. I_45Q_SYR .AND. W_SYR .LE. I_45Q_LYR_RET) THEN
                        IF (UL_45Q_YR(W_IGRP) .EQ. 0) THEN
                           UL_45Q_YR(W_IGRP) = W_SYR

                           WRITE(18,4417) CURIRUN, CURIYR+1989, W_IGRP, 0, WECPt, IRECL, W_SYR, W_RYR, WVIN, UL_45Q_YR(W_IGRP), WC_SUM, UPPCEF(WECPt)
 4417                      FORMAT(1X,"UL_45Q_YR_INFO",10(",",I5),2(",",F21.6))

                        ELSE IF (UL_45Q_YR(W_IGRP) .GT. W_SYR) THEN
                           UL_45Q_YR(W_IGRP) = W_SYR

                           WRITE(18,4417) CURIRUN, CURIYR+1989, W_IGRP, 0, WECPt, IRECL, W_SYR, W_RYR, WVIN, UL_45Q_YR(W_IGRP), WC_SUM, UPPCEF(WECPt)
   
                        END IF
                     END IF
                  ELSE                                      ! New Capacity
                     IF (W_SYR .GE. I_45Q_SYR .AND. W_SYR .LE. I_45Q_LYR_NEW) THEN
                        IF (UL_45Q_YR(W_IGRP) .EQ. 0) THEN
                           UL_45Q_YR(W_IGRP) = W_SYR

                           WRITE(18,4417) CURIRUN, CURIYR+1989, W_IGRP, 0, WECPt, IRECL, W_SYR, W_RYR, WVIN, UL_45Q_YR(W_IGRP), WC_SUM, UPPCEF(WECPt)

                        ELSE IF (UL_45Q_YR(W_IGRP) .GT. W_SYR) THEN
                           UL_45Q_YR(W_IGRP) = W_SYR

                           WRITE(18,4417) CURIRUN, CURIYR+1989, W_IGRP, 0, WECPt, IRECL, W_SYR, W_RYR, WVIN, UL_45Q_YR(W_IGRP), WC_SUM, UPPCEF(WECPt)
   
                        END IF
                     END IF
                  END IF
               END IF
!            END IF

            HTRT_IGRP(IRECL) = W_IGRP

!           write(6,1313) curirun, curiyr+1989, irecl, w_igrp,wecpt,wefdt,wc_sum,wfown
!1313       Format(1X,"whats_up",6(":",I7),":",F7.2,":",I4)

!           Resource cost - CP_ADD

!           BETTER ???
!
!           IYRMO = 100 * (CURIYR + UHBSYR) + 6
!           SYRMO = 100 * W_SYR + W_SMO
!           RYRMO = 100 * W_RYR + W_RMO
!           IF (WFOWN .LE. 4 .AND. WNOWN .LE. UNRGNS .AND. SYRMO .LE. IYRMO .AND. RYRMO .GT. IYRMO) THEN
!
!           AS IS
!
            IF (WFOWN .LE. 4 .AND. WNOWN .LE. UNRGNS .AND. W_SYR .LE. CURIYR + UHBSYR .AND. W_RYR .GE. CURIYR + UHBSYR) THEN
               CP_ADD(IRG,CURIYR) = CP_ADD(IRG,CURIYR) + WC_SUM * W_CAPAD * 0.001
               CP_ADD(MNUMNR,CURIYR) = CP_ADD(MNUMNR,CURIYR) + WC_SUM * W_CAPAD * 0.001
            END IF
!
!           CAPTURE PLANNED RETROFITS
!
            IF (WFOWN .LE. 4 .AND. WNOWN .LE. UNRGNS .AND. CURIYR + UHBSYR .EQ. (UPSTYR - 1)) THEN
               RETOVCST = 0.0
               RETYR = 9999
               IECP = UCPDSPIS(WECPT)
!
!              SO2
!
               IF (WSCBYR .GT. UHBSYR .AND. W_SYR .EQ. WSCBYR .AND. (WVIN .EQ. 6 .OR. WVIN .EQ. 7)) THEN
                  EMELRETP(WNOPER,W_SYR - UHBSYR) = EMELRETP(WNOPER,W_SYR - UHBSYR) + WC_SUM * 0.001
                  EMELRETP(MNUMNR,W_SYR - UHBSYR) = EMELRETP(MNUMNR,W_SYR - UHBSYR) + WC_SUM * 0.001
                  RETOVCST = RETOVCST + WSCBCST
                  RETYR = W_SYR
               END IF
!
!              SNCR
!
               IF (W_POST .EQ. 3 .AND. (WVIN .EQ. 6 .OR. WVIN .EQ. 7)) THEN
                  EMELNCRP(WNOPER,W_SYR - UHBSYR) = EMELNCRP(WNOPER,W_SYR - UHBSYR) + WC_SUM * 0.001
                  EMELNCRP(MNUMNR,W_SYR - UHBSYR) = EMELNCRP(MNUMNR,W_SYR - UHBSYR) + WC_SUM * 0.001
                  RETOVCST = RETOVCST + WSNCR_O
                  RETYR = W_SYR
               END IF
!
!              SCR
!
               IF (W_POST .EQ. 4 .AND. (WVIN .EQ. 6 .OR. WVIN .EQ. 7)) THEN
                  EMELSCRP(WNOPER,W_SYR - UHBSYR) = EMELSCRP(WNOPER,W_SYR - UHBSYR) + WC_SUM * 0.001
                  EMELSCRP(MNUMNR,W_SYR - UHBSYR) = EMELSCRP(MNUMNR,W_SYR - UHBSYR) + WC_SUM * 0.001
                  RETOVCST = RETOVCST + WSCR_O
                  RETYR = W_SYR
               END IF

!              Add planned retrofits to resource cost estimates (assumes financing as of (UPSTYR - 1))

               IF (RETOVCST .GT. 0.001 .AND. (RETYR .LT. 9999 .AND. RETYR .GT. (UPSTYR - 1))) THEN

                  IF ((RETYR - UHBSYR) .LE. UNYEAR)  &
                     RET_INV(IRG,RETYR-UHBSYR) = RET_INV(IRG,RETYR-UHBSYR) + RETOVCST * WC_SUM * 0.001
                  RET_INV(MNUMNR,RETYR-UHBSYR) = RET_INV(MNUMNR,RETYR-UHBSYR) + RETOVCST * WC_SUM * 0.001

!                 Collect Data for Resource Cost Calculations - Ret_Cst

!                 USE RETROFIT RISK PREMIUM, IF SWITCH IS ON

                  IF (USW_RTRSK .LE. 0)THEN
                     RETIRT = EPUIRT
                     RETROR = EPUROR
                     RETFPE = EPUFPE
                     RETCRE = EPUCRE
                  ELSE
                     RETIRT = EPUIRTR
                     RETROR = EPURORR
                     RETFPE = EPUFPER
                     RETCRE = EPUCRER
                  END IF

!
                  TXBOOK = 0.0
                  FNBOOK = 0.0
                  CAPUTIL = 0.0


                  CALL EPINCST(UNYEAR,ECP_D_XPH,ECP_D_FPH,ECP_D_LCP,UPSCLT,UPSCLT,CURIYR,1,RETOVCST,UPSCPR, &
                     UPCAPD(1,IECP),UPGNPD,RETIRT,EPUFDT,RETROR,TXBOOK,FNBOOK)
                  URATIO = TXBOOK / FNBOOK
                  CAPUNSTL = FNBOOK

!                 Check Vintage for Retrofit Tax Life

                  IF (WRFURB .LT. UPSTXYR)THEN
                     STXLF = UPSTXLF(1)
                  ELSE
                     STXLF = UPSTXLF(2)
                  END IF

                  CALL EPNBLD(DBLE(RETROR),DBLE(EPUTDSCRT),URATIO,DBLE(RETFPE),UPSELF,STXLF,DBLE(UPTXRT),UPSELF,CAPUTIL)

                  DO XYR = RETYR-UHBSYR , min(UNYEAR,(RETYR-UHBSYR+UPSELF-1))
                     Ret_Cst(IRG,XYR) = Ret_Cst(IRG,XYR) + CAPUNSTL * CAPUTIL * WC_SUM * 0.001 / UPGNPD(XYR)
                     Ret_Cst(MNUMNR,XYR) = Ret_Cst(MNUMNR,XYR) + CAPUNSTL * CAPUTIL * WC_SUM * 0.001 / UPGNPD(XYR)
                  END DO
               END IF
            END IF
!
!           Add planned units to resource cost estimates
!
            IF ((WVIN .EQ. 2 .OR. WVIN .GT. 9) .AND. WVIN .NE. 15 .AND. WFOWN .LE. 4 .AND. W_SYR .EQ. CURIYR + UHBSYR .AND. &
               W_SYR .GT. (UPSTYR - 1) ) THEN
               PLANT = WECPT
               FULLYR = CURIYR + UHBSYR
               FRG = 0
               IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0 .AND. W_CAR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)

!              IF (UPVTYP(PLANT) .GT. 0 .OR. UPTOPR(PLANT) .EQ. 3) THEN

               IF (UCPDSPIS(PLANT) .GT. 0) THEN               ! DISPATCHABLE
                  IECP = UCPDSPIS(PLANT)
                  JECP = UCPDSPI(IECP)
                  IF (JECP .LT. WIPC) THEN
                     OVRCST = UPOVR(WIPC) * UPLRPC(JECP) * UPLROPT(JECP) * UPLRLC(JECP) * EPRGM(JECP) * EPACM(JECP) * UPANNADJ(JECP,CURIYR)

                     IF (OVRCST .EQ. 0 .OR. ISNAN(OVRCST)) THEN
                        WRITE(6,3781) CURIRUN, CURIYR+UHBSYR, CURITR, (UPSTYR - 1), W_IGRP, W_GRP, W_GRP2, WVIN, PLANT, IECP, JECP, &
                           OVRCST, UPOVR(WIPC), UPLRPC(JECP), UPLROPT(JECP), UPLRLC(JECP), EPRGM(JECP), EPACM(JECP), UPANNADJ(JECP,CURIYR)
 3781                   FORMAT(1X,"BAD_OVRCST",11(":",I5),8(":",F21.6))
                     END IF

                  ELSE
                     OVRCST = UPOVR(JECP) * UPLRPC(JECP) * UPLROPT(JECP) * UPLRLC(JECP) * EPRGM(JECP) * EPACM(JECP) * UPANNADJ(JECP,CURIYR)

                     IF (OVRCST .EQ. 0 .OR. ISNAN(OVRCST)) THEN
                        WRITE(6,3781) CURIRUN, CURIYR+UHBSYR, CURITR, (UPSTYR - 1), W_IGRP, W_GRP, W_GRP2, WVIN, PLANT, IECP, JECP, &
                           OVRCST, UPOVR(JECP), UPLRPC(JECP), UPLROPT(JECP), UPLRLC(JECP), EPRGM(JECP), EPACM(JECP), UPANNADJ(JECP,CURIYR)
                     END IF

                  ENDIF
                  NLLF = UPNLLF(JECP)
                  NCLF = UPNCLF(JECP)
                  TLF = UPTXLF(JECP)
                  CLT = UPCLYR(JECP)
                  PLT = UPPLYR(JECP)
                  DO JYR = 1 , CLT
                     CPR(JYR) = UPCPRO(JECP,JYR)
                  END DO
                  DO JYR = 1 , UNYEAR + UNXPH
                     CAPD(JYR) = UPCAPD(JYR,JECP)
                  END DO
               ELSE IF (UCPINTIS(PLANT) .GT. 0) THEN           ! INTERMITTENT
                  IECP = UCPINTIS(PLANT)
                  JECP = UCPINTI(IECP)
                  OVRCST = EPIROVR(UIRINTI(IECP)) * UPLRPC(JECP) * UPLROPT(JECP) * UPLRLC(JECP) * EPRGM(JECP) * EPACM(JECP) * UPANNADJ(JECP,CURIYR)

                  IF (OVRCST .EQ. 0 .OR. ISNAN(OVRCST)) THEN
                     WRITE(6,3781) CURIRUN, CURIYR+UHBSYR, CURITR, (UPSTYR - 1), W_IGRP, W_GRP, W_GRP2, WVIN, PLANT, IECP, JECP, &
                        OVRCST, EPIROVR(UIRINTI(IECP)), UPLRPC(JECP), UPLROPT(JECP), UPLRLC(JECP), EPRGM(JECP), EPACM(JECP), UPANNADJ(JECP,CURIYR)
                  END IF

                  NLLF = UPNLLF(JECP)
                  NCLF = UPNCLF(JECP)
                  TLF = UPTXLF(JECP)
                  CLT = UPCLYR(JECP)
                  PLT = UPPLYR(JECP)
                  DO JYR = 1 , CLT
                     CPR(JYR) = UPCPRO(JECP,JYR)
                  END DO
                  DO JYR = 1 , UNYEAR + UNXPH
                     CAPD(JYR) = UPCAPD(JYR,JECP)
                  END DO
               ELSE IF (UCPRNWIS(PLANT) .GT. 0) THEN           ! RENEWABLE
                  IECP = UCPRNWIS(PLANT)
                  JECP = UCPRNWI(IECP)
                  OVRCST = EPIROVR(UIRRNWI(IECP)) * UPLRPC(JECP) * UPLROPT(JECP) * UPLRLC(JECP) * EPRGM(JECP) * EPACM(JECP) * UPANNADJ(JECP,CURIYR)

                  IF (OVRCST .EQ. 0 .OR. ISNAN(OVRCST)) THEN
                     WRITE(6,3781) CURIRUN, CURIYR+UHBSYR, CURITR, (UPSTYR - 1), W_IGRP, W_GRP, W_GRP2, WVIN, PLANT, IECP, JECP, &
                        OVRCST, EPIROVR(UIRRNWI(IECP)), UPLRPC(JECP), UPLROPT(JECP), UPLRLC(JECP), EPRGM(JECP), EPACM(JECP), UPANNADJ(JECP,CURIYR)
                  END IF

                  NLLF = UPNLLF(JECP)
                  NCLF = UPNCLF(JECP)
                  TLF = UPTXLF(JECP)
                  CLT = UPCLYR(JECP)
                  PLT = UPPLYR(JECP)
                  DO JYR = 1 , CLT
                     CPR(JYR) = UPCPRO(JECP,JYR)
                  END DO
                  DO JYR = 1 , UNYEAR + UNXPH
                     CAPD(JYR) = UPCAPD(JYR,JECP)
                  END DO
               ELSE IF (UCPSTOIS(PLANT) .GT. 0) THEN           ! STORAGE
                  IECP = UCPSTOIS(PLANT)
                  JECP = UCPSTOI(IECP)
                  OVRCST = UPOVR(JECP) * UPLRPC(JECP) * UPLROPT(JECP) * UPLRLC(JECP) * EPRGM(JECP) * EPACM(JECP) * UPANNADJ(JECP,CURIYR)

                  IF (OVRCST .EQ. 0 .OR. ISNAN(OVRCST)) THEN
                     WRITE(6,3781) CURIRUN, CURIYR+UHBSYR, CURITR, (UPSTYR - 1), W_IGRP, W_GRP, W_GRP2, WVIN, PLANT, IECP, JECP, &
                        OVRCST, EPIROVR(UIRRNWI(IECP)), UPLRPC(JECP), UPLROPT(JECP), UPLRLC(JECP), EPRGM(JECP), EPACM(JECP), UPANNADJ(JECP,CURIYR)
                  END IF

                  NLLF = UPNLLF(JECP)
                  NCLF = UPNCLF(JECP)
                  TLF = UPTXLF(JECP)
                  CLT = UPCLYR(JECP)
                  PLT = UPPLYR(JECP)
                  DO JYR = 1 , CLT
                     CPR(JYR) = UPCPRO(JECP,JYR)
                  END DO
                  DO JYR = 1 , UNYEAR + UNXPH
                     CAPD(JYR) = UPCAPD(JYR,JECP)
                  END DO
               ELSE IF (UCPDGNIS(PLANT) .GT. 0) THEN           ! DISTRIBUTED
                  IECP = UCPDGNIS(PLANT)
                  JECP = UCPDGNI(IECP)
                  OVRCST = UPOVR(JECP) * UPLRPC(JECP) * UPLROPT(JECP) * UPLRLC(JECP) * EPRGM(JECP) * EPACM(JECP) * UPANNADJ(JECP,CURIYR)

                  IF (OVRCST .EQ. 0 .OR. ISNAN(OVRCST)) THEN
                     WRITE(6,3781) CURIRUN, CURIYR+UHBSYR, CURITR, (UPSTYR - 1), W_IGRP, W_GRP, W_GRP2, WVIN, PLANT, IECP, JECP, &
                        OVRCST, UPOVR(JECP), UPLRPC(JECP), UPLROPT(JECP), UPLRLC(JECP), EPRGM(JECP), EPACM(JECP), UPANNADJ(JECP,CURIYR)
                  END IF

                  NLLF = UPNLLF(JECP)
                  NCLF = UPNCLF(JECP)
                  TLF = UPTXLF(JECP)
                  CLT = UPCLYR(JECP)
                  PLT = UPPLYR(JECP)
                  DO JYR = 1 , CLT
                     CPR(JYR) = UPCPRO(JECP,JYR)
                  END DO
                  DO JYR = 1 , UNYEAR + UNXPH
                     CAPD(JYR) = UPCAPD(JYR,JECP)
                  END DO
               END IF
!
               PCST = OVRCST
               FNBOOK = OVRCST
               CAPNUG = 1.0
               EWGROE = EPUCRE + UPNRPRM
               EWGINT = EPUIRT + UPNIPRM
               EWGROR = EWGROE * (1.0 - UPNFDT) + EWGINT * UPNFDT

!              write(6,8493)'TXBOOK b4 ', IRG,IECP,MNUMYR,ECP_D_XPH,ECP_D_FPH,ECP_D_LCP,CLT,PLT,CURIYR,1, &
!                 OVRCST,CPR,CAPD,UPGNPD,EWGINT,UPNFDT,EWGROR,TXBOOK,FNBOOK,EWGROE,EWGINT,EWGROR
!8493          FORMAT(A25,1x,10(I4,1x), 13(F12.3,1x))

               CALL EPINCST(MNUMYR,ECP_D_XPH,ECP_D_FPH,ECP_D_LCP,CLT,PLT,CURIYR,1,OVRCST,CPR,CAPD,UPGNPD,EWGINT,UPNFDT,EWGROR,TXBOOK,FNBOOK)

               PRATIO = TXBOOK / FNBOOK
               CAPNNSTL = FNBOOK
               CALL EPCNBLD(DBLE(UPNFDT),PRATIO,DBLE(UPNRPRM),DBLE(UPNIPRM),DBLE(EPUCRE),DBLE(EPUIRT),DBLE(UPTXRT),TLF,NCLF,NLLF,CAPNUG)
               PCST = CAPNNSTL * CAPNUG

               IF (OVRCST .EQ. 0 .OR. ISNAN(PCST)) THEN
                  WRITE(6,3782) CURIRUN, CURIYR+UHBSYR, CURITR, (UPSTYR - 1), W_IGRP, W_GRP, W_GRP2, WVIN, PLANT, IECP, JECP, &
                     CLT, PLT, TLF, NCLF, NLLF,  &
                     OVRCST, (CPR(JYR),JYR = 1, ECP_D_LCP), CAPD(1), UPGNPD(CURIYR), EWGINT, UPNFDT, EWGROR, TXBOOK, FNBOOK, &
                     UPNFDT, PRATIO, UPNRPRM, UPNIPRM, EPUCRE, EPUIRT, UPTXRT, CAPNUG
 3782             FORMAT(1X,"BAD_PCST",16(":",I5),<ECP_D_LCP>(":",F21.6),16(":",F21.6))

                  PCST = 0.0
                  CAPNUG = 0.0
               END IF

!              store costs for planned builds for EFP pricing

               IF (WFOWN .EQ. 3) THEN  !EWG
                  WPCST = PCST
               ENDIF
               CALL STRPLT(IRECL)
!
               WRITE(18,36) CURIYR+UHBSYR, FULLYR, IRG, W_IGRP, W_GRP, W_GRP2, PLANT, WFOWN, WC_SUM, OVRCST, FNBOOK, CAPNUG, PCST, EWGROR, &
                  EPDSCRT, UPRSK(PLANT), W_CF, UPGNPD(CURIYR),UPCSB(PLANT)
   36          FORMAT(1x,"Planned_Builds",8(":",I5),11(":",F9.3))

               IF (CURIYR .LE. UNYEAR) THEN
                  NEW_CAP_EL(IRG,CURIYR) = NEW_CAP_EL(IRG,CURIYR) + WC_SUM * 0.001
                  NEW_CAP_EL(MNUMNR,CURIYR) = NEW_CAP_EL(MNUMNR,CURIYR) + WC_SUM * 0.001
                  G_INST_ALL(IRG,CURIYR) = G_INST_ALL(IRG,CURIYR) + 0.001 * WC_SUM * PCST / UPGNPD(CURIYR) / ( EPDSCRT / ( 1.0 - ( 1.0 + EPDSCRT) ** -DBLE(NCLF)))/ (1.0 - UPCSB(PLANT))
                  G_INST_ALL(MNUMNR,CURIYR) = G_INST_ALL(MNUMNR,CURIYR) + 0.001 * WC_SUM * PCST / UPGNPD(CURIYR) / ( EPDSCRT / ( 1.0 - ( 1.0 + EPDSCRT) ** -DBLE(NCLF)))/(1.0 - UPCSB(PLANT))
                  DO XYR = CURIYR, UNYEAR
                     G_ANN(IRG,XYR) = G_ANN(IRG,XYR) + WC_SUM * 0.001 * PCST / UPGNPD(XYR)/(1.0 - UPCSB(PLANT))
                     G_ANN(MNUMNR,XYR) = G_ANN(MNUMNR,XYR) + WC_SUM * 0.001 * PCST / UPGNPD(XYR)/(1.0 - UPCSB(PLANT))
                  END DO
               END IF

!              add transmission for planned builds

               IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. (PLANT .EQ. WIWD) .OR. (PLANT .EQ. WIBI)) THEN
                  T_TCAP = FL_CNXT_CST(IRG,FRG)
                  IF (T_TCAP .GT. 200.0) T_TCAP = EPCOVR(WIAN)            !FLCNXTCST may be artificially high if region combo doesn't exist for new builds, set to AN as a proxy
               ELSE
                  T_TCAP = EPCOVR(PLANT)
               ENDIF
               T_TFCF = EPCCRF(PLANT)

               T_OVR(IRG,CURIYR) = T_OVR(IRG,CURIYR) + WC_SUM * 0.001 * T_TCAP
               T_OVR(MNUMNR,CURIYR) = T_OVR(MNUMNR,CURIYR) + WC_SUM * 0.001 * T_TCAP

               TRN_LIFE = ESBKLF(EITRAN,1)
               DO XYR = CURIYR, MIN(UNYEAR , CURIYR + TRN_LIFE - 1)
                  T_ANN(IRG,XYR) = T_ANN(IRG,XYR) + WC_SUM * 0.001 * T_TCAP * T_TFCF * UPGNPD(CURIYR)/UPGNPD(XYR)
                  T_ANN(MNUMNR,XYR) = T_ANN(MNUMNR,XYR) + WC_SUM * 0.001 * T_TCAP * T_TFCF * UPGNPD(CURIYR)/UPGNPD(XYR)
               ENDDO

!              END IF

            END IF   ! Planned Units Cost Estimates

!           DETERMINE EFD PLANT TYPE

            IEFD = WEFDT

!           FOR BACT Case unset SNCR identification

            IF (W_POST .EQ. 1) THEN
               W_POST = 0
            END IF

!           DETERMINE ECP PLANT TYPE

            IF (W_IGRP .EQ. 0) THEN
               WRITE(6,3337) CURIRUN, CURIYR+1989, CURITR, IRG, IGRP, W_INT(IRG,IGRP), OLD_REC, IRECL, W_NXT(IRECL), &
                  W_IGRP, W_GRP, W_SYR, W_RYR, WC_SUM
 3337          FORMAT(1X,"CRPGRP_OOPS_W_IGRP",13(":",I5),":",F20.3)
               IF (W_GRP .GT. 0) THEN
                  W_IGRP = W_GRP
               ELSE
                  WREC_NXT = WREC_NXT + 1
                  W_IGRP = WREC_NXT
                  W_GRP = WREC_NXT
               END IF
            END IF

            IF (W_IGRP .GT. 0) THEN
               SET_RFURB(W_IGRP) = MIN(SET_RFURB(W_IGRP),W_SYR)
               IF (SET_RFURB(W_IGRP) .LT. WRFURB) THEN
                  WRFURB = SET_RFURB(W_IGRP)
                  CALL STRPLT(IRECL)
               END IF
            END IF
            IECP = WECPT
            IF (IECP .EQ. 0) WRITE(6,*) 'IECP error ',W_IGRP,IEFD,IECP,WFOWN,WC_SUM
            ITYP = UPTTYP(IECP)
            WPTTYP(IECP,IRG) = WPTTYP(IECP,IRG) + 1
            P_TTYP(ITYP) = P_TTYP(ITYP) + 1

!           For all existing coal plants also identify with CL to NG Conversion ECP type

            IF (ITYP .LE. EX_COAL) THEN
               CLtNG_Typ = UPTTYP(WING)
               P_TTYP(CLtNG_TYP) = P_TTYP(CLtNG_TYP) + 1
            END IF

!           INSURE EXISTING PLANTS HAVE PROPER VINTAGE CODE

            IF ((CURIYR + 1989) .EQ. UESTYR) THEN
               IF (W_SYR .LE. (UPSTYR - 1) .AND. WVIN .EQ. 2)THEN
                  WVIN = 1
                  CALL STRPLT(IRECL)
               END IF
            END IF

!           REVISE CF for planned and existing wind or PV plants to match new
            IF ( (IECP .EQ. WIWN .OR. IECP .EQ. WIPV .OR. IECP .EQ. WIWF .OR. IECP .EQ. WISO) .AND. (WVIN .EQ. 2 ) )THEN !planned
               IF ((CURIYR + UHBSYR) .EQ. WRFURB) THEN
                   IF (IECP .EQ. WIPV) THEN
                     W_CF = AVG_PV_CF(IRG,CURIYR)
                     DO m=1, 12
                      WCF_M(m) = AVG_PV_CF_MO(IRG,CURIYR,m)
                     ENDDO
                   ELSEIF (IECP .EQ. WISO) THEN
                     W_CF = AVG_SO_CF(IRG,CURIYR)
                     DO m=1, 12
                      WCF_M(m) = AVG_SO_CF_MO(IRG,CURIYR,m)
                     ENDDO
                   ELSEIF (IECP .EQ. WIWF) THEN
                     W_CF = AVG_WF_CF(IRG,CURIYR)
                     DO m=1, 12
                      WCF_M(m) = AVG_WF_CF_MO(IRG,CURIYR,m)
                     ENDDO
                   ELSE
                     W_CF = AVG_WN_CF(IRG,CURIYR)
                     DO m=1, 12
                      WCF_M(m) = AVG_WN_CF_MO(IRG,CURIYR,m)
                     ENDDO
                   ENDIF
                   CALL STRPLT(IRECL)
               ENDIF
            ELSEIF ( (IECP .EQ. WIWN .OR. IECP .EQ. WIPV .OR. IECP .EQ. WIWF .OR. IECP .EQ. WISO) .AND. (WVIN .EQ. 1 .OR. WVIN .EQ. 15 ) ) THEN ! REVISE monthly CF for existing wind or PV plants to match new
                  IF (IECP .EQ. WIPV .AND. AVG_PV_CF(IRG,CURIYR) .GT. 0) THEN
                      !W_CF = AVG_PV_CF(IRG,CURIYR)
                     DO m=1, 12
                      WCF_M(m) = AVG_PV_CF_MO(IRG,CURIYR,m) * W_CF / AVG_PV_CF(IRG,CURIYR) !EPIACF(IGRP) MIN(999,
                     ENDDO
                  ELSEIF (IECP .EQ. WISO .AND. AVG_SO_CF(IRG,CURIYR) .GT. 0) THEN
                     !W_CF = AVG_SO_CF(IRG,CURIYR)
                     DO m=1, 12
                          WCF_M(m) = AVG_SO_CF_MO(IRG,CURIYR,m) * W_CF / AVG_SO_CF(IRG,CURIYR) !EPIACF(IGRP)
                     ENDDO
                  ELSEIF (IECP .EQ. WIWF .AND. AVG_WF_CF(IRG,CURIYR) .GT. 0) THEN
                     !W_CF = AVG_WF_CF(IRG,CURIYR)
                     DO m=1, 12
                          WCF_M(m) = AVG_WF_CF_MO(IRG,CURIYR,m) * W_CF / AVG_WF_CF(IRG,CURIYR) !EPIACF(IGRP)
                     ENDDO
                   ELSE
                     !W_CF = AVG_WN_CF(IRG,CURIYR)
                     DO m=1, 12
                      WCF_M(m) = AVG_WN_CF_MO(IRG,CURIYR,m) * W_CF / AVG_WN_CF(IRG,CURIYR)
                     ENDDO
                   ENDIF
                   CALL STRPLT(IRECL)
            ENDIF

!           DETERMINE OWNERSHIP TYPE

            IOWN = WFOWN

!           CHECK MUST RUN STATUS

            IF (W_MRUN .GT. 0 .AND. IOWN .LE. 3 .AND. IECP .LT. WIDB .AND. (CURIYR + UHBSYR) .GT. W_MRUN) THEN
               W_MRUN = 0
               CALL STRPLT(IRECL)
            ENDIF

!           IF (CO2_PHS40 .GT. 0.0)THEN
! remove all nt biomass mrun
!              IF ((IECP .EQ. WIWD .AND. W_MRUN .GT. 0) .OR. (IECP .EQ. WIWD .AND. W_MRUN .GT. 0)) THEN
!                  W_MRUN = 0
!                  CALL STRPLT(IRECL)
!              ENDIF

!              IF CO2 PERFORMANCE STANDARD, PHASE OUT MUST RUN STATUS FOR CHP

!              IF (W_MRUN .GT. 0 .AND. IOWN .EQ. 4 .AND. CO2_STDSW .GT. 0 .AND. CO2_STDRS(1,CURIYR) .GT. 0.0 .AND. CO2_PLTSW(IECP) .GT. 0.0) THEN
!                 write(6,6767) curiyr+1989,irg,wstate,w_grp,iown,uplntcd(iecp),wc_sum,w_mrun
!6767             format(1h ,'!mri',i4,i3,a3,i6,i3,a3,f10.1,i5)

!                 IF (W_MRUN .GT. 2035) W_MRUN = 2035
!                 CALL STRPLT(IRECL)
!                 write(6,6768) curiyr+1989,irg,wstate,w_grp,iown,uplntcd(iecp),wc_sum,w_mrun
!6768             format(1h ,'!mro',i4,i3,a3,i6,i3,a3,f10.1,i5)
!              ENDIF
!           ENDIF
            MRSW = W_MRUN

!           IF (ITYP .LE. EX_COAL .AND. IOWN .LT. 4 .AND. MRSW .GT. 0)write(6,3344) curiyr+1989,w_igrp,w_grp,w_mrun,wc_sum
!3344 format(1h ,'!ovec',i4,i6,i6,i5,f10.3)
!
!           IF CARBON LIMIT, THEN ELIMINATE MUST RUN STATUS
!
!           CARMIN = 1.0 * MC_JPGDP((UPSTYR - 1)-UHBSYR) / 1000.0  !carbon fee at least $1/mmton in current dollars
!           IF (EMETAX(1,CURIYR) .GT. CARMIN .AND. IOWN .LE. 3 .AND. IEFD .NE. UIDGB .AND. IEFD .NE. UIDGP .AND. IEFD .NE. UIGTH) THEN
!              IF ( (BANK_FLAG) .AND. (CURCALYR .LT. BANK_STARTYR) ) THEN
!                 MRSW = MRSW
!              ELSE
!                 MRSW = 0
!              ENDIF
!           ENDIF
!
!           IF MACT or BACT Eliminate Must Run Status
!
            ICLS = HG_CLASS(IECP)
            IF (UPTTYP(IECP) .LE. NW_COAL) THEN
               DO IRNK = 1 , 3
                  IF (HG_MEF(ICLS,IRNK,MIN(UNYEAR,CURIYR+2)) .GT. 0.0)  MRSW = 0
                  IF (HG_OUTPUT(ICLS,IRNK,MIN(UNYEAR,CURIYR+2)) .GT. 0.0)  MRSW = 0
                  IF (HG_INPUT(ICLS,IRNK,MIN(UNYEAR,CURIYR+2)) .GT. 0.0)  MRSW = 0
               END DO
               IF (USW_BACT .GT. 0 .AND. CURIYR+UHBSYR .GE. UMACT_YR-2)  MRSW = 0
               IF (HG_GRAMS_MWH(ICLS,MIN(UNYEAR,CURIYR+2)) .GT. 0.0)  MRSW = 0
            END IF

            IF (USW_EPA111 .GT. 0 .AND. UPTTYP(IECP) .LE. NW_COAL .AND. UPPCEF(IECP) .LT. 0.5) THEN  !add check for 111 status, turn off must -run
               IF (MRSW .GT. UEPA_CLYR) MRSW=UEPA_CLYR
            ENDIF
            
            IF (CURIYR + UHBSYR .GE. UDSI_YR .AND. IEFD .EQ. UISTX .AND. WPART .NE. 'E') THEN
               W_FSHR(1) = 1.0        !max gas share 100% and oil share 0 for MATS
               W_FSHR(2) = 0.0
            ENDIF
!
!           MAKE BIOMASS PLANTS MUST RUN, IF SWITCH IS SET (E.G., CECA RUNS)
!
            IF (IEFD .EQ. UIBMS .AND. USW_BMYR .GT. 0 .AND. (CURIYR + UHBSYR) .GE. USW_BMYR) MRSW = 9999
!
!           SET CAPAD NUMBERS TO ZERO IF CANADIAN RUN
!
            IF (USW_XP .GT. 0) THEN              !Canadian run
               W_CAPAD = 0.0
            ENDIF
!
!           THESE FACTORS ARE USED TO ASSIGN UTILIZATION FACTORS FOR COAL PLANTS IN THE ECP.
!           THEY ALSO ADJUST THE FACTORS FOR THE EFD - THEY ARE UPDATED TO MATCH STEO
!
            DO XYR = 1, UNYEAR + ECP_D_XPH
               COAL_FACTOR(XYR) = 1.000
            END DO
            COAL_FACTOR(12) = 1.08
            COAL_FACTOR(13) = 1.08
            COAL_FACTOR(14) = 1.08
            COAL_FACTOR(15) = 1.08
            COAL_FACTOR(16) = 1.08
            COAL_FACTOR(17) = 1.08
            COAL_FACTOR(18) = 1.08
            COAL_FACTOR(19) = 1.05
            COAL_FACTOR(20) = 1.00
            COAL_FACTOR(21) = 1.000
            COAL_FACTOR(22) = 0.960
            COAL_FACTOR(23) = 0.93
            COAL_FACTOR(24) = 0.92
            COAL_FACTOR(25) = 0.880
            COAL_FACTOR(26) = 0.900
            COAL_FACTOR(27) = 0.870
            COAL_FACTOR(28) = 0.900
            COAL_FACTOR(29) = 0.930
            COAL_FACTOR(30) = 0.970
!
!
!           CALIBRATE COAL GENERATION AND FUEL CONSUMPTION
!
!           phase out must runs based on switch  ** don't use this with must-run year setting
!
!           IF (MRSW .GT. 0) THEN
!              IF (UPHMRY(MRSW) .GT. (UHBSYR + MNUMYR)) THEN
!                 MRFAC = 1.0
!              ELSE
!                 IF ((CURIYR + UHBSYR) .LE. (UPSTYR - 1)) THEN
!                    MRFAC = 1.0
!                 ELSEIF ((CURIYR + UHBSYR) .LE. UPHMRY(MRSW)) THEN
!                    MRFAC = 1.0 - DBLE(CURIYR + UHBSYR - (UPSTYR - 1)) / DBLE(UPHMRY(MRSW) - (UPSTYR - 1))
!                 ELSE
!                    MRFAC = 0.0
!                 ENDIF
!              ENDIF
!           ELSE

            MRFAC = 1.0

!           ENDIF

            W_CFA = W_CF * MRFAC
!
!           PHASE IN MAX CAPACITY FACTOR FOR NEW DISPATCHABLE PLANTS (ignore if max is already low  < .5)
!
            IF (UCPDSPIS(IECP) .GT. 0 .AND. (WVIN .EQ. 2 .OR. WVIN .EQ. 3 .OR. WVIN .GT. 9) .AND. WVIN .NE. 15 .AND. W_CFA .GT. 0.5)THEN
             IF (W_SMO .LT. 6)THEN
               IF ((CURIYR + UHBSYR) .EQ. WRFURB)W_CFA = W_CFA * 0.60
               IF ((CURIYR + UHBSYR) .EQ. (WRFURB+ 1))W_CFA = W_CFA * 0.80
             ELSE
               IF ((CURIYR + UHBSYR) .LE. (WRFURB + 1))W_CFA = W_CFA * 0.60
               IF ((CURIYR + UHBSYR) .EQ. (WRFURB + 2))W_CFA = W_CFA * 0.80
             END IF
            END IF

!           GJB 9/14/12  PHASE IN MAX CAPACITY FACTOR FOR NEW INTERMITTENT PLANTS

            IF (UCPINTIS(IECP) .GT. 0 .AND. (WVIN .EQ. 2 .OR. WVIN .EQ. 3 .OR. WVIN .GT. 9) .AND. WVIN .NE. 15)THEN
               IF ((CURIYR + UHBSYR) .EQ. WRFURB) THEN
                  W_CFA = W_CFA * 0.80
                  DO IMO = 1, 12
                     WCF_M(IMO)=WCF_M(IMO)*0.80
                  ENDDO
               ENDIF
            END IF

            WHRATEA = WHRATE
!
            IF (IEFD .EQ. UICOU .OR. IEFD .EQ. UICSU .OR. IEFD .EQ. UICSC) THEN
               IF (BMCLTOL .LE. 0.0) THEN
                  IF ((CURIYR + UHBSYR) .LT. UCFSYR(IEFD)) THEN
                     W_CFA = W_CF - DBLE((UCFSYR(IEFD) - (CURIYR + UHBSYR)) * 0.01)
                  END IF
                  W_CFA = W_CFA * COAL_FACTOR(CURIYR)

                  IF (CURIYR .LT. 20) WHRATEA = WHRATE * 1.00
                  IF (CURIYR .EQ. 20) WHRATEA = WHRATE * 1.03
                  IF (CURIYR .EQ. 21) WHRATEA = WHRATE * 1.03
                  IF (CURIYR .EQ. 22) WHRATEA = WHRATE * 1.02
                  IF (CURIYR .EQ. 23) WHRATEA = WHRATE * 1.01
                  IF (CURIYR .EQ. 24) WHRATEA = WHRATE * 1.01
                  IF (CURIYR .EQ. 25) WHRATEA = WHRATE * 1.01
                  IF (CURIYR .EQ. 26) WHRATEA = WHRATE * 1.01
                  IF (CURIYR .EQ. 27) WHRATEA = WHRATE * 1.005
                  IF (CURIYR .GE. 28) WHRATEA = WHRATE * 1.00
               !ELSE
               !  IF (CURIYR .LE. 33) THEN
               !    WHRATEA = WHRATE * 1.02
               !  ELSE
               !    WHRATEA = WHRATE * 1.01
               !  ENDIF
               END IF
            ELSE
               IF (IEFD .EQ. UICCG .OR. IEFD .EQ. UICCX) THEN
                  IF (BMNGTOL .LE. 0.0) THEN
                     IF (CURIYR .LE. 17) WHRATEA = WHRATE
                     IF (CURIYR .EQ. 18) WHRATEA = WHRATE * 1.05
                     IF (CURIYR .EQ. 19) WHRATEA = WHRATE * 1.05
                     IF (CURIYR .EQ. 20) WHRATEA = WHRATE * 1.02
                     IF (CURIYR .EQ. 21) WHRATEA = WHRATE * 1.02
                     IF (CURIYR .EQ. 22) WHRATEA = WHRATE * 1.03
                     IF (CURIYR .EQ. 23) WHRATEA = WHRATE * 1.03
                     IF (CURIYR .EQ. 24) WHRATEA = WHRATE * 1.02
                     IF (CURIYR .EQ. 25) WHRATEA = WHRATE * 1.02
                     IF (CURIYR .EQ. 26) WHRATEA = WHRATE * 1.01
                     IF (CURIYR .EQ. 27) WHRATEA = WHRATE * 1.01
                     IF (CURIYR .GT. 27) WHRATEA = WHRATE
                  !ELSE
                  !   IF (CURIYR .LE. 30) WHRATEA = WHRATE * 1.050
                  !   IF (CURIYR .EQ. 31) WHRATEA = WHRATE * 1.030
                  !   IF (CURIYR .EQ. 32) WHRATEA = WHRATE * 1.020
                  !   IF (CURIYR .EQ. 33) WHRATEA = WHRATE * 1.015
                  !   IF (CURIYR .GT. 33) WHRATEA = WHRATE * 1.010
                  ENDIF
               ELSE
                  WHRATEA = WHRATE
               ENDIF
            END IF

!           ADJUST TO REFLECT OIL SPIKE in 2014 in NE and south

            IF ((IRG .EQ. 5 .OR. IRG .EQ. 2) .AND. CURIYR .EQ. 25) THEN
               IF (IEFD .EQ. UISTO) W_CFA = MIN(1.0, W_CFA + 0.1)
            ENDIF

!           ADJUST SOLAR THERMAL AND PV PLANT CAPACITY FACTORS BY EFFICIENCY MULTIPLIERS
!           MULTIPLIERS GET APPLIED ACCORDING TO START YEAR OF PLANT FILE.
!           MULTIPLIERS DO NOT APPLY TO NEW BUILDS.

            IF ( IEFD .EQ. UISTH .OR. IEFD .EQ. UISPV .OR. IEFD .EQ. UIPVT) THEN
               IF (WVIN .NE. 3 .AND. WVIN .NE. 15) THEN
                  STRTYR = W_SYR - UHBSYR
                  IF (STRTYR .LE. 0) STRTYR = 1
                  IF (IEFD .EQ. UISTH) THEN
                     CFMULT = EFFMULST(STRTYR)
                  ELSEIF (IEFD .EQ. UISPV) THEN
                     CFMULT = EFFMULPV(STRTYR)
                  ELSEIF (IEFD .EQ. UIPVT) THEN
                     CFMULT = EFFMULPT(STRTYR)
                  ENDIF
                  IF ((CURIYR .EQ. 1) .AND. (CURITR .EQ. 1)) CFMULT = 1.0
                  W_CFA = W_CFA * CFMULT
                  DO IMO = 1 , 12
                     WCF_M(IMO) = WCF_M(IMO) * CFMULT
                  ENDDO
               ENDIF
            ENDIF

!           CONSIDER NEW WIND IN NEXT YEAR TO BE END-OF-YEAR BECAUSE OF INADEQUATE LEAD TIME

            IF (IECP .EQ. WIWN .AND. WVIN .EQ. 3 .AND.  &
               W_SYR .EQ. (ECP_FYR + UPPLYR(IECP)) .AND. (CURIYR + UHBSYR) .EQ. (ECP_FYR + UPPLYR(IECP)))THEN
               W_CFA = 0.0
               DO IMO = 1 , 12
                  WCF_M(IMO) = 0.0
               ENDDO
            END IF
  993       format(a,3(1x,i4),2(1x,f7.4))
  994       format(a,3(1x,f7.4))

!           FOR CECA CASES, ASSUME IMPROVEMENT IN HEAT RATES AND O&M
!           AND INCLUDE PORTION OF FIXED O&M IN VARIABLE COMPONENT

            IF(USW_HRP .EQ. 1 .AND. IEFD .LE. EFD_D_DSP)THEN
               IF(WHRATEA .GT. DBLE(UHR_TRGT(IEFD)) .AND. DBLE(UHR_TRGT(IEFD)) .LT. DBLE(99999.0))THEN
                  WHRATEA = DBLE(UHR_TRGT(IEFD)) +  DBLE(UHR_PROF(CURIYR,IEFD)) * (WHRATEA - DBLE(UHR_TRGT(IEFD)))
               END IF
            END IF
            IF (USW_HRP .EQ. 2 .AND. IEFD .LE. EFD_D_DSP)THEN
                  WHRATEA = WHRATEA * DBLE(UHR_PROF(CURIYR,IEFD))
            END IF
            WFOMA = W_FOM * OMADJ(IEFD,CURIYR)
            WVOMA = W_VOM * OMADJ(IEFD,CURIYR)
            WGAA  = W_GA  * GAADJ(1,IRG,CURIYR)
!
!           O&M
!
            IF (USW_OMP .EQ. 1 .AND. IEFD .LE. EFD_D_DSP)THEN
!              GET TOTAL O&M FROM PLANT FILE
               TOM = W_FOM + W_VOM * W_CF * 8.760
               IF(TOM .GT. DBLE(UOM_TRGT(IEFD)))THEN
!                 DETERMINE AND APPLY RATE OF IMPROVEMENT FROM CECA
                  TOMY = DBLE(UOM_TRGT(IEFD)) + &
                     DBLE(UOM_PROF(CURIYR,IEFD)) * &
                     (TOM - DBLE(UOM_TRGT(IEFD)))
               ELSE
                  TOMY = TOM
               END IF
               WFOMA = W_FOM * TOMY / TOM
               WVOMA = W_VOM * TOMY / TOM
            END IF
            IF(USW_FOM .EQ. 1 .AND. YRCECA .GT. 0)THEN
               IF(UFOM_CFC(IEFD) .GT. 0.0)THEN
                  WVOMA = TOMY * DBLE(UFOM_PCT(IEFD)) / &
                     (DBLE(UFOM_CFC(IEFD)) * 8.760)
                  WFOMA = TOMY * (DBLE(1.0) - DBLE(UFOM_PCT(IEFD)))
               END IF
            END IF
!
!           DETERMINE NOX CONTROL GROUP NUMBER IF ANY
!
            DO INOX = 1 , NOX_GRP
               LNOX(INOX) = 0
               DO IST = 1 , NOX_NST(INOX)
                  IF (WSTATE .EQ. NOX_ST(IST,INOX)) LNOX(INOX) = 1
               END DO
               IF (WVIN .EQ. 3 .OR. WVIN .EQ. 20) LNOX(INOX) = NOX_RGN(IRG,INOX)
            END DO

!           Create / Update Plant Group Heatrate and the Timeframe in which this capacity exists

            SINDX = W_SYR * 100 + W_SMO
            RINDX = W_RYR * 100 + W_RMO
            ULAGE(W_GRP) = (CURIYR + UHBSYR) - WRFURB
            ULSINDX(W_GRP) = MIN(ULSINDX(W_GRP) , SINDX)
            ULRINDX(W_GRP) = MAX(ULRINDX(W_GRP) , RINDX)

            DO IST = 1 , NM_ST_CODES
               IF (WSTATE .EQ. ST_CODES(IST)) THEN
                  ULSTATE(W_GRP) = IST
                  EXIT
               END IF
            END DO

            WRITE(FROM_LABEL,'("CRPGRP_",I5.5)') W_IGRP
            FUEL_RGN = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)
            ECPt = WECPT
            Load_Level = W_CFA
            CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)

            DO XYR = 1 , UNXPH
               IF (SINDX .LE. LYR_INDEX(XYR) .AND. RINDX .GE. CYR_INDEX(XYR)) THEN
                  IF (XYR .LT. UNXPH) THEN
                     JYR = CURIYR + XYR - 1
                     SHR8 = DBLE(1.0) / DBLE(12.0)
                     DO JMO = 1 , 12
                        IF (SUMMER_MONTHS(JMO) .EQ. 1.0) THEN
                           CAP8 = DBLE(WC_SUM) * SHR8
                        ELSE
                           CAP8 = DBLE(WC_WIN) * SHR8
                        END IF
                        YINDX = (JYR + UHBSYR) * 100 + JMO
                        IF (SINDX .LE. YINDX .AND. RINDX .GE. YINDX) THEN
                           ULSCAP_ECP(W_GRP,XYR) = ULSCAP_ECP(W_GRP,XYR) + CAP8
                           HTRT_ECP(W_GRP,XYR) = HTRT_ECP(W_GRP,XYR) + WHRATEA * (Target_EFF / Max_EFF) * CAP8
                        END IF
                     END DO
                  ELSE
                     SHR8 = DBLE(1.0) / (DBLE(UNFPH) - DBLE(XYR) + DBLE(1.0)) / DBLE(12.0)
                     DO JYR = CURIYR + XYR - 1 , CURIYR + UNFPH - 1
                        DO JMO = 1 , 12
                           IF (SUMMER_MONTHS(JMO) .EQ. 1.0) THEN
                              CAP8 = DBLE(WC_SUM) * SHR8
                           ELSE
                              CAP8 = DBLE(WC_WIN) * SHR8
                           END IF
                           YINDX = (JYR + UHBSYR) * 100 + JMO
                           IF (SINDX .LE. YINDX .AND. RINDX .GE. YINDX) THEN
                              ULSCAP_ECP(W_GRP,XYR) = ULSCAP_ECP(W_GRP,XYR) + CAP8
                              HTRT_ECP(W_GRP,XYR) = HTRT_ECP(W_GRP,XYR) + WHRATEA * (Target_EFF / Max_EFF) * CAP8
                           END IF
                        END DO
                     END DO
                  END IF
               END IF
            END DO

            MO_CNT = 0.0
            DO JMO = 1 , 12
               MO_CNT(0) = MO_CNT(0) + DBLE(1.0)
               ISP = HrToEFDSeas(JMO,1,1)
               MO_CNT(ISP) = MO_CNT(ISP) + DBLE(1.0)
               IF (SUMMER_MONTHS(JMO) .EQ. 1.0) THEN
                  CAP8 = DBLE(WC_SUM)
               ELSE
                  CAP8 = DBLE(WC_WIN)
               END IF
               YINDX = (CURIYR + UHBSYR) * 100 + JMO
               IF (SINDX .LE. YINDX .AND. RINDX .GE. YINDX) THEN
                  LCAP_EFD(W_GRP,ISP) = LCAP_EFD(W_GRP,ISP) + CAP8
                  HTRT_EFD(W_GRP,ISP) = HTRT_EFD(W_GRP,ISP) + WHRATEA * (Target_EFF / Max_EFF) * CAP8
               END IF
            END DO

            ULORGN(W_GRP) = WNOWN
            ULECPT(W_GRP) = WECPT
            ULEFDT(W_GRP) = WEFDT
            IF (SINDX .LE. LYR_INDEX(1) .AND. RINDX .GE. CYR_INDEX(1)) THEN
               ULMRUN(W_GRP) = MAX(MRSW,ULMRUN(W_GRP))
            END IF
            ULFRGN(W_GRP) = 0
            ULIGRP(W_GRP) = W_IGRP
            ULHRQ(W_GRP) = MAX(1,WHR_QT)

            IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0 .AND. W_CAR .GT. 0) ULFRGN(W_GRP) = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)

            DO XYR = 1 , UNXPH
               IF (ULSCAP_ECP(W_GRP,XYR) .GT. 0.0) THEN
                  ULHTRT_ECP(W_GRP,XYR) = HTRT_ECP(W_GRP,XYR) / ULSCAP_ECP(W_GRP,XYR)
               ELSE
                  IF (WHRATEA .GT. 0) THEN
                     ULHTRT_ECP(W_GRP,XYR) = WHRATEA * (Target_EFF / Max_EFF)
                  ELSE
                     ULHTRT_ECP(W_GRP,XYR) = WHRATE * (Target_EFF / Max_EFF)
                  END IF
               END IF
            END DO

            DO ISP = 1 , EENSP
               IF (LCAP_EFD(W_GRP,ISP) .GT. 0.0) THEN
                  ULSCAP_EFD(W_GRP,ISP) = LCAP_EFD(W_GRP,ISP) / MO_CNT(ISP)
                  ULHTRT_EFD(W_GRP,ISP) = HTRT_EFD(W_GRP,ISP) / LCAP_EFD(W_GRP,ISP)
               ELSE
                  IF (WHRATEA .GT. 0) THEN
                     ULHTRT_EFD(W_GRP,ISP) = WHRATEA * (Target_EFF / Max_EFF)
                  ELSE
                     ULHTRT_EFD(W_GRP,ISP) = WHRATE * (Target_EFF / Max_EFF)
                  END IF
               END IF
            END DO

            IF (SINDX .LE. LYR_INDEX(1) .AND. RINDX .GE. CYR_INDEX(1)) THEN
               IF (TST_TGEN(W_GRP) .EQ. 0.0) THEN
                  IF (CURIYR + UHBSYR .EQ. UESTYR) THEN
                     ULTGEN(W_GRP) = ULSCAP_ECP(W_GRP,1) * MAX(W_CF , 0.001) * 8.760
                  ELSE
                     ULTGEN(W_GRP) = ULSCAP_ECP(W_GRP,1) * 0.001 * 8.760
                  END IF
               END IF
            END IF

!           IF OWNERSHIP TYPE IS VALID THEN CONTINUE
!
            IF (IOWN .LE. USW_OWN) THEN
!
!              DETERMINE REPORT VINTAGE
!
               IVIN3 = WVIN
               IF (WVIN .GT. 3) IVIN3 = 1
               IF (WVIN .GT. 9) IVIN3 = 2
               IF (WVIN .EQ. 20) IVIN3 = 3   ! unplanned units in wfloors
               IF (WVIN .EQ. 15) IVIN3 = 1   ! enduse PV capacity, later removed from reports
               IF ((WVIN .GT. 3 .AND. WVIN .LE. 9) .AND. WRFURB .GT. (UPSTYR - 1)) IVIN3=2
               IF (IVIN3 .GT. 1 .AND. W_SYR .LE. (UPSTYR - 1)) IVIN3 = 1
!              IF (IVIN3 .GT. 1 .AND. WRFURB .LE. (UPSTYR - 1)) IVIN3 = 1
!
!              IDENTIFY IF GAS-FIRED CAPACITY OR COAL-FIRED CAPACITY
!
               FIRM = 0
               INTR = 0
               COMP = 0
               COAL = 0
!
!              SET GAS AND COAL FLAGS, AND REGION INDICES
!
               DO IFL = 1 , EIFPLT
                  FL_NUM = WFL(IFL)
                  IF (FL_NUM .EQ. UIGF) THEN
                     FIRM = 1
                  ELSE IF ( (FL_NUM .EQ. UIGI) .OR. (FL_NUM .EQ. UIDG) ) THEN
                     INTR = 1
                  ENDIF
!
!                 FOR COAL, PRIMARY FUEL ONLY
!
                  IF ( (IFL .EQ. 1) .AND. &
                     (FL_NUM .EQ. UIB1 .OR. FL_NUM .EQ. UIB2 .OR. FL_NUM .EQ. UIB3 .OR. &
                      FL_NUM .EQ. UIB4 .OR. FL_NUM .EQ. UIB5 .OR. FL_NUM .EQ. UIB6 .OR. &
                      FL_NUM .EQ. UIB7 .OR. FL_NUM .EQ. UIB8 .OR. FL_NUM .EQ. UIC1 .OR. &
                      FL_NUM .EQ. UIC2 .OR. FL_NUM .EQ. UIC3 .OR. FL_NUM .EQ. UIC4 .OR. &
                      FL_NUM .EQ. UIC5 .OR. FL_NUM .EQ. UIC6 .OR. FL_NUM .EQ. UIC7 .OR. &
                      FL_NUM .EQ. UIC8 .OR. FL_NUM .EQ. UIC9 .OR. FL_NUM .EQ. UICX .OR. &
                      FL_NUM .EQ. UICY .OR. FL_NUM .EQ. UICZ .OR. FL_NUM .EQ. UIH1 .OR. &
                      FL_NUM .EQ. UIH2 .OR. FL_NUM .EQ. UIH3 .OR. FL_NUM .EQ. UIH4 .OR. &
                      FL_NUM .EQ. UIH5 .OR. FL_NUM .EQ. UIH6 .OR. FL_NUM .EQ. UIH7 .OR. &
                      FL_NUM .EQ. UIH8 .OR. FL_NUM .EQ. UIH9 .OR. FL_NUM .EQ. UIHA .OR. &
                      FL_NUM .EQ. UIHB .OR. FL_NUM .EQ. UIHC .OR. FL_NUM .EQ. UIPC .OR. &
                      FL_NUM .EQ. UIIG .OR. FL_NUM .EQ. UIIS .OR. FL_NUM .EQ. UIOC .OR. FL_NUM .EQ. UII2 .OR. FL_NUM .EQ. UIPQ))  &
                     COAL = 1
                  IF (W_CLRG .LE. 0 .OR. W_CLRG .GT. NDRGG)W_CLRG = EPCLRG
                  IF ( FL_NUM .GT. 0) THEN
                     DO IFLRG = 1, UNFLRG(FL_NUM)
                        IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'GR') THEN
                           RG_NUM = W_GR
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'CL') THEN
                           CL_REG = W_CLRG
                        END IF
                     END DO
                  END IF
               END DO                             ! FUELS PER PLANT LOOP

!              BENCHMARK NON-TRADITIONAL COGEN GAS GENERATION TO 96 HISTORICAL NUMBERS
!              IF ((IOWN .EQ. 4) .AND. ((FIRM .EQ. 1) .OR.
!    +            (INTR .EQ. 1) .OR. (COMP .EQ. 1))) THEN
!                 W_CFA = W_CFA * 1.07
!              ENDIF

               FULLYR = IYR + UNXPH - 1

!              SETUP UPGRADE CAPACITY

               ACT_CF = UPMCF(IECP)
               IF (FULLCYR .GT. UESTYR ) THEN
                  IF (ULCAPC(W_GRP) .GT. 0.001) THEN
                     ACT_CF = ULTGEN(W_GRP) / (8.76 * ULCAPC(W_GRP))
                  END IF
               END IF
               T_VCST = WVOMA
               ITYP = UPTTYP(IECP)
               T_AVG(ITYP,IRG) = T_AVG(ITYP,IRG) + ULTGEN(W_GRP) * WVOMA
               T_CAP(ITYP,IRG) = T_CAP(ITYP,IRG) + ULCAPC(W_GRP)
               T_GEN(ITYP,IRG) = T_GEN(ITYP,IRG) + ULTGEN(W_GRP)
               T_HRATE(ITYP,IRG) = T_HRATE(ITYP,IRG) + WHRATE * ULTGEN(W_GRP)
!
!              IF EXISTING PLANT IS IN OPERATION FOR CURRENT PLANNING HORIZON
!
!              IF ((IYR .GE. W_SYR) .AND. W_RYR .LT. 9999 .AND. W_RYR .GT. 2015 .AND. IVIN3 .EQ. 1 .AND. IECP .LE. 32)THEN
!               write(6,2346) curiyr+1989,w_igrp,w_grp,wstate,w_syr,w_ryr,w_rmo,wc_sum
!2346 format(1h ,'!retcl',i4,i6,i6,a3,i5,i5,i5,f10.1)
!              END IF
               IF ((IYR .GE. W_SYR) .AND. (FULLYR .LE. W_RYR) .AND. (W_RYR .GE. 9999) .AND. &
                  (IVIN3 .EQ. 1) .AND. (USW_ERET .EQ. 1) .AND. &
                  (IOWN .LE. 3) .AND. (RET_GRP(IECP) .GT. 0) .AND. MRSW .EQ. 0) THEN
                  LYR = CURIYR - 1
                  RGRP = 1
                  IF (LYR .GE. 1) THEN
                     TCAP = ULCAPC(W_GRP)
                     IF (TCAP .GT. 0.0) THEN
                        VCST = ULVCST(W_GRP) / TCAP
                        FCST = ULFCST(W_GRP) / TCAP
                        SO2P = ULSO2P(W_GRP) / TCAP
                        NOXP = ULNOXP(W_GRP) / TCAP
                        RPSP = ULRPSP(W_GRP) / TCAP
                        HGP =  ULHGP(W_GRP) / TCAP
                        GHGP = ULGHG(W_GRP) / TCAP
                        KRG = ULORGN(W_GRP)
                        if (krg.eq.0) krg=23

                        WRITE(18,4920)'1-ULREVS b4 ',CURIYR,IRG,KRG,W_GRP,REVS,ULREVS(W_GRP) ,ULCAPC(W_GRP) , RMAVG(CURIYR-1,KRG),  MC_JPGDP(CURIYR) , TCAP
 4920                   FORMAT(A25,1x,4(I5,1x),6(F20.9,1x))

                        IF (TST_TGEN(W_GRP) .GT. 0.0) THEN
                          REVS = (ULREVS(W_GRP)  - (ULCAPC(W_GRP) * CAPCCR(IECP) * RMAVG(CURIYR-1,KRG) )/1000 ) / TCAP
                        ELSE
                           REVS = ULREVS(W_GRP) / TCAP
                        ENDIF

                        WRITE(18,4920)'1-ULREVS af ',CURIYR,IRG,KRG,W_GRP,REVS,ULREVS(W_GRP) ,ULCAPC(W_GRP) , RMAVG(CURIYR-1,KRG),  CAPCCR(IECP) , TCAP

                        RET_VAL(IRECL) = REVS - VCST - FCST - SO2P - NOXP - RPSP - HGP - GHGP
                        RET_MIN(IECP) = MIN(RET_MIN(IECP) , RET_VAL(IRECL))
                        RET_MAX(IECP) = MAX(RET_MAX(IECP) , RET_VAL(IRECL))
                        RET_AVG(IECP,IRG) = RET_AVG(IECP,IRG) + (REVS - VCST) * TCAP
                     ELSE
                        RET_VAL(IRECL) = 0.0
                     END IF
                  ELSE
                     RET_VAL(IRECL) = 200.0 - WFOMA - WGAA - W_CAPAD
                  END IF
!
!                 ACCUMULATE NON-REVENUE YEARS
!
                  IF ((CURIYR + UHBSYR) .GE. MAX(UPSTYR,UNUC_SYR - UREV_NYR + 1))THEN
                     IF (RET_VAL(IRECL) .LT. -0.001 .AND. IRECL .NE. IRECLL)EOTH_RYRS(IRECL) = EOTH_RYRS(IRECL) + 1
                     IRECLL = IRECL
                  END IF
!
                  AGE = MAX(MIN(UNYEAR+ECP_D_FPH , CURIYR + UHBSYR - WRFURB + 1),1)
                  RET_FOM(IRECL) = WFOMA + WGAA + W_CAPAD + UPXTRA(AGE,IECP)
                  RET_CAP(IRECL) = WC_SUM
                  EPGCAP(0,RGRP,IECP) = EPGCAP(0,RGRP,IECP) + WC_SUM
                  FRG = 0

!                 IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR)

                  IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0 .AND. W_CAR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)

!                 IF (IRG .EQ. 5 .AND. FRG .NE. 1) THEN
!                    write(6,1968)  CURIYR,W_GRP,WC_SUM,IECP,IRG,W_CR,W_CLRG,W_GR,W_CAR
!1968                FORMAT(1x,' IRG FRG 1',2I6,F10.3,6I6)
!                 ENDIF

                  RET_FRG(IRECL) = FRG
                  NEW_VAL = RET_VAL(IRECL)
                  FREC = RET_FRST(RGRP,IECP,IRG)
                  IF (FREC .EQ. 0) THEN
                     RET_FRST(RGRP,IECP,IRG) = IRECL
                  ELSE
                     TST_VAL = RET_VAL(FREC)
                     IF (NEW_VAL .LE. TST_VAL) THEN
                        RET_NEXT(IRECL) = FREC
                        RET_FRST(RGRP,IECP,IRG) = IRECL
                     ELSE
                        DO WHILE (FREC .GT. 0 .AND. NEW_VAL .GT. TST_VAL)
                           OREC = FREC
                           FREC = RET_NEXT(FREC)
                           IF (FREC .GT. 0) TST_VAL = RET_VAL(FREC)
                        END DO
                        RET_NEXT(OREC) = IRECL
                        RET_NEXT(IRECL) = FREC
                     END IF
                  END IF
               END IF
!
!              ACCUMULATE CAPACITY FOR PLANNING EXERCISE (OML) AND EXPECTATIONS
!              FOR COAL AND GAS MODULES
!
               DO YEAR = 1 , UNXPH
                  FULLYR = IYR + YEAR - 1
!
!                 IF PLANT IS IN OPERATION IN CURRENT YEAR
!
                  IF ((FULLYR .GE. W_SYR) .AND. (FULLYR .LE. W_RYR)) THEN
!
!                    GAS-FIRED CAPACITY
!
                     IF (FIRM .GT. 0) THEN
                        EGFCAP(RG_NUM,YEAR) = EGFCAP(RG_NUM,YEAR) + WC_SUM
                     ELSE IF (INTR .GT. 0) THEN
                        EGICAP(RG_NUM,YEAR) = EGICAP(RG_NUM,YEAR) + WC_SUM
                     ELSE IF (COMP .GT. 0) THEN
                        EGCCAP(RG_NUM,YEAR) = EGCCAP(RG_NUM,YEAR) + WC_SUM
                     END IF
!
!                    COAL-FIRED CAPACITY
!
                     IF (COAL .GT. 0) THEN
                        ECLCAP(CL_REG,YEAR) = ECLCAP(CL_REG,YEAR) + WC_SUM
!                     IF (YEAR .EQ. 1)THEN
!                       COFCAP(IECP,W_CLRG,IRG) = COFCAP(IECP,W_CLRG,IRG) + WC_SUM
!                       COFCAP(IECP,W_CLRG,MNUMNR) = COFCAP(IECP,W_CLRG,MNUMNR) + WC_SUM
!                     END IF
                     END IF
!
!                    DISPATCHABLE CAPACITY
!
                     IF (UCPDSPIS(IECP) .GT. 0) THEN
                        IF (YEAR .EQ. 1 .AND. IECP .LE. WIIS)THEN
!                          IF ((CURIYR + UHBSYR) .LE. UPSTYR)THEN
                              CLCFC = W_CFA
!                          ELSE
!                             CLCFC = CFCPLT(IRG,IECP)
!                          END IF
                           DO KRG = 1 , TSO2_NST
                              IF (WSTATE .EQ. TSO2_ST(KRG))THEN
                                 TRN = TSO2_TR_BY_ST(KRG)
                                 COA = TSO2_CL_BY_ST(KRG)
                                 TBTU_SHR_BY_ST(KRG,IECP) = TBTU_SHR_BY_ST(KRG,IECP) + WC_SUM *  &
                                                   CLCFC + 8.760 * WHRATEA * 0.000001
                                 TBTU_SHR_BY_ST(KRG,ECP_D_DSP + 1) = TBTU_SHR_BY_ST(KRG,ECP_D_DSP + 1) + WC_SUM *  &
                                                   CLCFC + 8.760 * WHRATEA * 0.000001
                                 TBTU_SHR_BY_CLRG(COA,IECP,TRN) = TBTU_SHR_BY_CLRG(COA,IECP,TRN) + WC_SUM *  &
                                                   CLCFC + 8.760 * WHRATEA * 0.000001
                                 TBTU_SHR_BY_CLRG(COA,ECP_D_DSP + 1,TRN) = TBTU_SHR_BY_CLRG(COA,ECP_D_DSP + 1,TRN) + WC_SUM *  &
                                                   CLCFC + 8.760 * WHRATEA * 0.000001
                              END IF
                           END DO
                        END IF
                        EPECAP(0,IECP,YEAR) =  EPECAP(0,IECP,YEAR) + WC_SUM
                        IF (MRSW .GE. (CURCALYR + YEAR - 1)) EPECAP_MR(0,IECP,YEAR) =  EPECAP_MR(0,IECP,YEAR) + WC_SUM
                        FRG = 0

!                       IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR)

                        IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0 .AND. W_CAR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)

!                       IF (IRG .EQ. 5 .AND. FRG .NE. 1) THEN
!                          write(6,1968)  CURIYR,W_GRP,WC_SUM,IECP,IRG,W_CR,W_CLRG,W_GR,W_CAR
!                       ENDIF

                        IF (FRG .GT. 0) EPECAP(FRG,IECP,YEAR) = EPECAP(FRG,IECP,YEAR) + WC_SUM
                        IF (FRG .GT. 0 .AND. MRSW .GE. (CURCALYR + YEAR - 1)) EPECAP_MR(FRG,IECP,YEAR) = EPECAP_MR(FRG,IECP,YEAR) + WC_SUM

                        IF (HTRT_RESULTS(W_IGRP) .EQ. 0) THEN
                           IF (MRSW .GE. (CURCALYR + YEAR - 1)) THEN
                              IF (FRG .GT. 0) HTRT_CAP_MR(IRG,ITYP,FRG,YEAR) = HTRT_CAP_MR(IRG,ITYP,FRG,YEAR) + WC_SUM
                           ELSE
                              IF (FRG .GT. 0) HTRT_CAP(IRG,ITYP,FRG,YEAR) = HTRT_CAP(IRG,ITYP,FRG,YEAR) + WC_SUM
                           END IF
                        END IF

!
                        DO ISP = 1 , ECP_D_MSP
                           EP_SP_CAP_FAC(ISP,IECP,YEAR) = EP_SP_CAP_FAC(ISP,IECP,YEAR) + ECP_SUMMER_SHR(ISP) * WC_SUM + (1.0 - ECP_SUMMER_SHR(ISP)) * WC_WIN
                        END DO
!
                        DO J_ECPt = 1 , ECP_D_CAP
                           IF (ECPt_MAP(IECP,J_ECPt) .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_CLRG .LE. NDREG) THEN
                              TST_CAP_BY_CRG(J_ECPt,W_CLRG,YEAR) = TST_CAP_BY_CRG(J_ECPt,W_CLRG,YEAR) + WC_SUM
                           END IF
                        END DO
                        EPUNIT(IECP,YEAR) =  EPUNIT(IECP,YEAR) + WCOUNT
                        IF (ITYP .LE. EX_COAL) THEN
                           EPECFC(IECP,YEAR) =  EPECFC(IECP,YEAR) + WC_SUM * W_CF * COAL_FACTOR(CURIYR+YEAR-1)
!                          IF (W_BTP .EQ. 'CY')THEN
!                             IF (WC_SUM .GT. 500.0)THEN
!                                T_COF = 1
!                             ELSE
!                                T_COF = 2
!                             END IF
!                          ELSE
                              IF (WC_SUM / WCOUNT .GT. 500.0)THEN
                                 T_COF = 3
                              ELSEIF (WC_SUM / WCOUNT .GE. 200.0)THEN
                                 T_COF = 4
                              ELSE
                                 T_COF = 5
                              END IF
!                          END IF
                           UCF_TCAP(T_COF,W_CLRG,CURIYR+YEAR-1) = UCF_TCAP(T_COF,W_CLRG,CURIYR+YEAR-1) + WC_SUM
                           UCF_TCAP1(T_COF,IRG,W_CLRG,CURIYR+YEAR-1) = UCF_TCAP1(T_COF,IRG,W_CLRG,CURIYR+YEAR-1) + WC_SUM
                           IF (YEAR .EQ. 1 .AND. FRG .GT. 0) THEN
                              COFCAP(T_COF,IECP,IRG,FRG) = COFCAP(T_COF,IECP,IRG,FRG) + WC_SUM
                              COFCAP(T_COF,IECP,MNUMNR,FRG) = COFCAP(T_COF,IECP,MNUMNR,FRG) + WC_SUM
                              ABTU = 0.0
                              IF (ULCAPC(W_GRP) .GT. 0.0) THEN
                                 DO IFL = 1 , EFD_D_FPP
                                    ABTU = ABTU + ULBTUE(IFL,W_GRP)
                                 END DO
                                 ABTU = ABTU / ULCAPC(W_GRP)
                                 UPCFBTU(T_COF,W_CLRG) = UPCFBTU(T_COF,W_CLRG) + ABTU * WC_SUM
                              END IF
                           END IF
                        ELSE
                           EPECFC(IECP,YEAR) =  EPECFC(IECP,YEAR) + WC_SUM * W_CFA
                        END IF
                        IF (ITYP .LE. EX_COAL) THEN
                           WGHT1 = MAX( MIN( ((DBLE(UCFEYR(UICOU)) - DBLE(CURIYR+UHBSYR+YEAR-1)) /   &
                              (DBLE(UCFEYR(UICOU)) - DBLE(UCFSYR(UICOU)))) , DBLE(1.0)) , DBLE(0.0))
                        ELSE
                           WGHT1 = MAX( MIN( ((DBLE(UCFEYR(UICCX)) - DBLE(CURIYR+UHBSYR+YEAR-1)) /   &
                              (DBLE(UCFEYR(UICCX)) - DBLE(UCFSYR(UICCX)))) , DBLE(1.0)) , DBLE(0.0))
                        END IF
                        P_CAP(ITYP,YEAR) =  P_CAP(ITYP,YEAR) + WC_SUM
                        WGHT2 = DBLE(1.0) - WGHT1
                        IF (ITYP .LE. EX_COAL) THEN
                           P_MCF(ITYP,YEAR) =  P_MCF(ITYP,YEAR) + WC_SUM * (WGHT1 * (W_CF * COAL_FACTOR(CURIYR+YEAR-1)) + WGHT2 * UPMCF(IECP))
                        ELSE
                           P_MCF(ITYP,YEAR) =  P_MCF(ITYP,YEAR) + WC_SUM * (WGHT1 * W_CFA + WGHT2 * UPMCF(IECP))
                        END IF
!
!                       ACCUMULATE DISPATCHABLE CAPACITY ADDITIONS FOR NEW TECHNOLOGIES SUBMODULE
!
                        IF (ITYP .LE. NW_COAL .AND. FULLCYR .GT. UESTYR) THEN
                           I_COAL = MAP_TO_COAL_ID(W_IGRP)
                           IF (I_COAL .GT. 0) THEN
                              IF (YEAR .LT. UNXPH) THEN
                                 ECP_CF = EMM_CL_CF(I_COAL,MIN(UNYEAR,CURIYR+YEAR-1))
                              ELSE
                                 DO XYR = 1 , UNFPH - YEAR + 1
                                    TCF(XYR) = EMM_CL_CF(I_COAL,MIN(UNYEAR,CURIYR+YEAR+XYR-2))
                                    KW(XYR) = 1.0
                                 END DO
                                 PV_CF = PVV(TCF,ECP_D_FPH,UNFPH-YEAR+1,DBLE(EPDSCRT))
                                 PV_KW = PVV(KW,ECP_D_FPH,UNFPH-YEAR+1,DBLE(EPDSCRT))
                                 ECP_CF = PV_CF / PV_KW
                              END IF
                           ELSE
                              IF (ULCAPC(W_GRP) .GT. 0.0) THEN
                                 ECP_CF = ULTGEN(W_GRP) / (8.76 * ULCAPC(W_GRP))
                              ELSE
                                 ECP_CF =  W_CFA
                              END IF
                           END IF
                           IF (CURIYR .EQ. 21) WRITE(18,2385) CURIYR+UHBSYR,CURIYR+UHBSYR+YEAR-1,W_IGRP,W_GRP,I_COAL,IECP,ITYP,IRG,WSTATE,ECP_CF, &
                              ULCAPC(W_GRP),ULTGEN(W_GRP),WC_SUM,EMM_CL_CF(I_COAL,MIN(UNYEAR,CURIYR+YEAR-1)),WNOX_R,EPDSCRT
 2385                      FORMAT(1X,"ECP_CF_COAL",8(":",I5),":",A2,7(":",F15.6))
                        ELSE
                           IF (ULCAPC(W_GRP) .GT. 0.0) THEN
                              ECP_CF = ULTGEN(W_GRP) / (8.76 * ULCAPC(W_GRP))
                           ELSE
                              ECP_CF =  W_CFA
                           END IF
                        END IF
                        IF (ULSCAP_ECP(W_GRP,YEAR) .GT. 0.0) THEN
                           TGEN = MAX(1.0 , ULSCAP_ECP(W_GRP,YEAR) * ECP_CF * 8.76)
                        ELSE
                           TGEN = MAX(1.0 , WC_SUM * ECP_CF * 8.76)
                        END IF

                        ULTGEN_ECP(W_GRP,YEAR) = TGEN

                        IF ((UPVTYP(IECP) .EQ. 0) .OR.  (UPTOPR(IECP) .EQ. 3)) THEN
                           ECP_GEN_XPH(YEAR,IECP) = ECP_GEN_XPH(YEAR,IECP) + TGEN
                           P_GEN_XPH(YEAR,ITYP) = P_GEN_XPH(YEAR,ITYP) + TGEN
                           IF (CLtNG_TYP .GT. 0) P_GEN_XPH(YEAR,CLtNG_TYP) = P_GEN_XPH(YEAR,CLtNG_TYP) + TGEN
                           DO INOX = 1 , NOX_GRP
                              IF (LNOX(INOX) .GT. 0) THEN
                                 DO ISP = 1 , ECP_D_MSP
                                    EPNOX_G(ISP,YEAR,IECP,INOX) = EPNOX_G(ISP,YEAR,IECP,INOX) + TGEN * WNOX_R * NOX_ECP(ISP,INOX)
                                    P_NOX(ISP,YEAR,ITYP,INOX) = P_NOX(ISP,YEAR,ITYP,INOX) + TGEN * WNOX_R * NOX_ECP(ISP,INOX)
                                    IF (CLtNG_TYP .GT. 0) P_NOX(ISP,YEAR,CLtNG_TYP,INOX) = P_NOX(ISP,YEAR,CLtNG_TYP,INOX) + TGEN * WNOX_R * NOX_ECP(ISP,INOX)
                                END DO
                              END IF
                           END DO
                        END IF
!
                        IF (YEAR .EQ. 1) THEN
                           IF ((FULLYR .GT. WRFURB) .AND. (UPLRSYR(IECP) .LE. WRFURB) .AND. (UPLRLYR(IECP) .GE. WRFURB)) THEN
                              EPNCAP(IECP) = EPNCAP(IECP) + WC_SUM      !

                              IF (IECP .eq. WIAN .AND. WVIN .EQ. 1) THEN   !generic uprate, don't count toward learning
                                EPNCAP(IECP) = EPNCAP(IECP) - WC_SUM
                              ENDIF
!
!                             ADD PLANT FILE NUCLEAR ADDITIONS TO ADV NUC CAPACITY, NOT CONV
!
!                             IF (IECP .eq. WICN .AND. (WVIN .EQ. 2 .OR. WVIN .EQ. 3 .OR. WVIN .EQ. 20)) THEN
!                                EPNCAP(WIAN) = EPNCAP(WIAN) + WC_SUM
!                                EPNCAP(IECP) = EPNCAP(IECP) - WC_SUM
!                             END IF
                           END IF
                        ELSE IF (YEAR .EQ. UNXPH) THEN
                           IF ((UPVTYP(IECP) .EQ. 0) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
                              ECP_GEN(IECP) = ECP_GEN(IECP) + TGEN

!                             IDENTIFY IF CANDIDATE FOR HR IMPROVEMENT

                              IF (IECP .LT. WIPC)THEN
                                 HRIOVR = HTRT_OVRQ(IECP,MAX(WHR_QT,1))
                                 HRIRED = HTRT_REDQ(IECP,MAX(WHR_QT,1))
                              ELSE
                                 HRIOVR = HTRT_OVRQ(IECP,1)
                                 HRIRED = HTRT_REDQ(IECP,1)
                              END IF
                              AGE = MAX(MIN(UNYEAR+ECP_D_FPH , CURIYR + UHBSYR - WRFURB + 1),1)
                              EPFOM(IECP) = EPFOM(IECP) + (WFOMA + WGAA + W_CAPAD + UPXTRA(AGE,IECP)) * WC_SUM * UECP_CPEN_ADJ(IECP)
                              ECP_VOM(IECP) = MAX(ECP_VOM(IECP), WVOMA)
                              IF (CLtNG_TYP .GT. 0) P_HTRT_MAX(CLtNG_TYP) = MAX(P_HTRT_MAX(CLtNG_TYP), WHRATEA / UECP_HTRT_ADJ(IECP))
                              ECP_NOX(IECP) = MAX(ECP_NOX(IECP), WNOX_R)
                              EPVOM(IECP) = EPVOM(IECP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                              IF (TGEN .GT. 1.0) THEN
                                 T_VADJ(W_GRP) = T_VADJ(W_GRP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                                 T_WGRP(W_GRP) = T_WGRP(W_GRP) + TGEN
                              ELSE
                                 T_VADJ(W_GRP) = T_VADJ(W_GRP) + WVOMA * UECP_CPEN_ADJ(IECP)
                                 T_WGRP(W_GRP) = T_WGRP(W_GRP) + 1.0
                              END IF

!                             IF (IRG .EQ. 1 .AND. (IECP .EQ. WIST .OR. IECP .EQ. WIET .OR. IECP .EQ. WIEC .OR. IECP .EQ. WIC4)) THEN
!                                WRITE(18,8133) CURIRUN, CURIYR+1989, CURIYR+YEAR+1988, IRG, FRG, IECP, ITYP, ULMRUN(W_GRP), MRSW, W_SYR, W_SMO, W_RYR, W_RMO, W_GRP, W_IGRP, ULHRQ(W_GRP), HTRT_RESULTS(W_IGRP), &
!                                   WHRATEA, TGEN, UECP_HTRT_ADJ(IECP), WC_SUM, WC_WIN
18133                            FORMAT(1X,"HTRT0_CRPGRP",17(":",I6),5(":",F21.6))
!                             END IF

                              EPPHRT0(IECP) = EPPHRT0(IECP) + WHRATEA * TGEN / UECP_HTRT_ADJ(IECP)
                              P_HTRT0(ITYP) = P_HTRT0(ITYP) + WHRATEA * TGEN / UECP_HTRT_ADJ(IECP)
                              IF (CLtNG_TYP .GT. 0) P_HTRT0(CLtNG_TYP) = P_HTRT0(CLtNG_TYP) + WHRATEA * TGEN / UECP_HTRT_ADJ(IECP)
                              P_GEN(ITYP) = P_GEN(ITYP) + TGEN
                              P_FOM(ITYP) = P_FOM(ITYP) + (WFOMA + WGAA + W_CAPAD + UPXTRA(AGE,IECP)) * WC_SUM * UECP_CPEN_ADJ(IECP)
                              P_VOM(ITYP) = P_VOM(ITYP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                              IF (CLtNG_TYP .GT. 0) THEN
                                 P_GEN(CLtNG_TYP) = P_GEN(CLtNG_TYP) + TGEN
                                 P_FOM(CLtNG_TYP) = P_FOM(CLtNG_TYP) + (WFOMA + WGAA + W_CAPAD + UPXTRA(AGE,IECP)) * WC_SUM * UECP_CPEN_ADJ(IECP)
                                 P_VOM(CLtNG_TYP) = P_VOM(CLtNG_TYP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                              END IF
                              IF (W_CLRG .LE. NDRGG .AND. IECP .LE. NCLUT1) THEN
                                 C2_GEN(ITYP,W_CLRG) = C2_GEN(ITYP,W_CLRG) + TGEN
                                 IF (CLtNG_TYP .GT. 0) C2_GEN(CLtNG_TYP,W_CLRG) = C2_GEN(CLtNG_TYP,W_CLRG) + TGEN
                                 IF (WSCBT .EQ. "W" .OR. WSCBT .EQ. "D") THEN
                                    C2_HTRT(ITYP,W_CLRG) =  C2_HTRT(ITYP,W_CLRG) + WHRATEA * TGEN / (1.0 + SCRHPEN)
                                    IF (CLtNG_TYP .GT. 0) C2_HTRT(CLtNG_TYP,W_CLRG) =  C2_HTRT(CLtNG_TYP,W_CLRG) + WHRATEA * TGEN / (1.0 + SCRHPEN)
                                 ELSE
                                    C2_HTRT(ITYP,W_CLRG) =  C2_HTRT(ITYP,W_CLRG) + WHRATEA * TGEN
                                    IF (CLtNG_TYP .GT. 0) C2_HTRT(CLtNG_TYP,W_CLRG) =  C2_HTRT(CLtNG_TYP,W_CLRG) + WHRATEA * TGEN
                                 END IF
                              END IF
                           END IF
                           IF (WSCBGRP .GT. 0 .AND. WSCBGRP .LE. ECP_D_SCR) THEN ! UPDATE SCR STATS
                              EPSCAP(IECP,WSCBGRP) = EPSCAP(IECP,WSCBGRP) + WC_SUM
                              NSCR = EPNSCR(IECP,WSCBGRP) + 1
                              EPNSCR(IECP,WSCBGRP) = NSCR
                              EPSREC(NSCR,IECP,WSCBGRP) = IRECL
                              SCRCST(NSCR,IECP,WSCBGRP) = WSCBCST
                              EPSOVR(IECP,WSCBGRP) = EPSOVR(IECP,WSCBGRP) + WC_SUM * WSCBCST
                           END IF
                        END IF

!                       DETERMINE GENERATION BY NERC AND CO2 REGION FOR DISTRIBUTED GENERATION SHARES

                        IF (YEAR .EQ. 1 .AND. W_CAR .GT. 0)THEN
                           CO2_DGN(W_CAR,IRG) = CO2_DGN(W_CAR,IRG) + TGEN
                           CO2_DGNT(IRG) = CO2_DGNT(IRG) + TGEN
                        END IF
!
!                    INTERMITTENT CAPACITY
!
                     ELSE IF (UCPINTIS(IECP) .GT. 0) THEN
                        EPECAP(0,IECP,YEAR) = EPECAP(0,IECP,YEAR) + WC_SUM
                        IF (IECP .EQ. WIPV .AND. WVIN .NE. 15) THEN
                            EPECAP_UPV(0,IECP,YEAR) = EPECAP_UPV(0,IECP,YEAR) + WC_SUM
                        ENDIF
                        IF (MRSW .GE. (CURCALYR + YEAR - 1)) EPECAP_MR(0,IECP,YEAR) = EPECAP_MR(0,IECP,YEAR) + WC_SUM
                        IF (W_CAR .LE. 0)W_CAR = UPCARG(IRG)

                        IF (YEAR .EQ. 1) THEN
                           IF (IECP .EQ. WIPV) THEN
                              PV_CAP(IRG) = PV_CAP(IRG) + WC_SUM * 0.001              ! Cummulative PV Capacity
                              IF (AVG_PV_CF(IRG,CURIYR) .GT. 0.001) THEN
                                 PV_CAP_ADJ(IRG) = PV_CAP_ADJ(IRG) + WC_SUM * W_CFA / AVG_PV_CF(IRG,CURIYR) * 0.001              ! Cummulative Adjusted PV Capacity
                              ELSE
                                 PV_CAP_ADJ(IRG) = PV_CAP_ADJ(IRG) + WC_SUM * 0.001              ! Cummulative Adjusted PV Capacity
                              END IF
                           END IF
                           IF (IECP .EQ. WIPT) THEN
                              PT_CAP(IRG) = PT_CAP(IRG) + WC_SUM * 0.001              ! Cummulative PT Capacity
                              IF (AVG_PT_CF(IRG,CURIYR) .GT. 0.001) THEN
                                 PT_CAP_ADJ(IRG) = PT_CAP_ADJ(IRG) + WC_SUM * W_CFA / AVG_PT_CF(IRG,CURIYR) * 0.001              ! Cummulative Adjusted PT Capacity
                              ELSE
                                 PT_CAP_ADJ(IRG) = PT_CAP_ADJ(IRG) + WC_SUM * 0.001              ! Cummulative Adjusted PT Capacity
                              END IF
                           END IF
                           IF (IECP .EQ. WISO) THEN
                              SO_CAP(IRG) = SO_CAP(IRG) + WC_SUM * 0.001              ! Cummulative SO Capacity
                              IF (AVG_SO_CF(IRG,CURIYR) .GT. 0.001) THEN
                                 SO_CAP_ADJ(IRG) = SO_CAP_ADJ(IRG) + WC_SUM * W_CFA / AVG_SO_CF(IRG,CURIYR) * 0.001              ! Cummulative SO Capacity
                              ELSE
                                 SO_CAP_ADJ(IRG) = SO_CAP_ADJ(IRG) + WC_SUM * 0.001              ! Cummulative SO Capacity
                              END IF
                           END IF
                           IF (IECP .EQ. WIWN) THEN
                              WN_CAP(IRG) = WN_CAP(IRG) + WC_SUM * 0.001              ! Cummulative WN Capacity
                              IF (AVG_WN_CF(IRG,CURIYR) .GT. 0.001) THEN
                                 WN_CAP_ADJ(IRG) = WN_CAP_ADJ(IRG) + WC_SUM * W_CFA / AVG_WN_CF(IRG,CURIYR) * 0.001              ! Cummulative WN Capacity
                              ELSE
                                 WN_CAP_ADJ(IRG) = WN_CAP_ADJ(IRG) + WC_SUM * 0.001              ! Cummulative WN Capacity
                              END IF
                           END IF
                           IF (IECP .EQ. WIWL) THEN
                              WL_CAP(IRG) = WL_CAP(IRG) + WC_SUM * 0.001              ! Cummulative WL Capacity
                              IF (AVG_WL_CF(IRG,CURIYR) .GT. 0.001) THEN
                                 WL_CAP_ADJ(IRG) = WL_CAP_ADJ(IRG) + WC_SUM * W_CFA / AVG_WL_CF(IRG,CURIYR) * 0.001              ! Cummulative WL Capacity
                              ELSE
                                 WL_CAP_ADJ(IRG) = WL_CAP_ADJ(IRG) + WC_SUM * 0.001              ! Cummulative WL Capacity
                              END IF
                           END IF
                           IF (IECP .EQ. WIWF) THEN
                              WF_CAP(IRG) = WF_CAP(IRG) + WC_SUM * 0.001              ! Cummulative WF Capacity
                              IF (AVG_WF_CF(IRG,CURIYR) .GT. 0.001) THEN
                                 WF_CAP_ADJ(IRG) = WF_CAP_ADJ(IRG) + WC_SUM * W_CFA / AVG_WF_CF(IRG,CURIYR) * 0.001              ! Cummulative WF Capacity
                              ELSE
                                 WF_CAP_ADJ(IRG) = WF_CAP_ADJ(IRG) + WC_SUM * 0.001              ! Cummulative WF Capacity
                              END IF
                           END IF
                           IF (IECP .EQ. WIPV .AND. CURIYR+UHBSYR .EQ. W_SYR) THEN
                              PV_NEW(IRG) = PV_NEW(IRG) + WC_SUM * 0.001              ! Cummulative PV Capacity
                              IF (AVG_PV_CF(IRG,CURIYR) .GT. 0.001) THEN
                                 PV_NEW_ADJ(IRG) = PV_NEW_ADJ(IRG) + WC_SUM * W_CFA / AVG_PV_CF(IRG,CURIYR) * 0.001              ! Cummulative PV Capacity
                              ELSE
                                 PV_NEW_ADJ(IRG) = PV_NEW_ADJ(IRG) + WC_SUM * 0.001              ! Cummulative PV Capacity
                              END IF
                           END IF
                           IF (IECP .EQ. WIWN .AND. CURIYR+UHBSYR .EQ. W_SYR) THEN
                              WN_NEW(IRG) = WN_NEW(IRG) + WC_SUM * 0.001              ! Cummulative PV Capacity
                              IF (AVG_WN_CF(IRG,CURIYR) .GT. 0.001) THEN
                                 WN_NEW_ADJ(IRG) = WN_NEW_ADJ(IRG) + WC_SUM * W_CFA / AVG_WN_CF(IRG,CURIYR) * 0.001              ! Cummulative WN Capacity
                              ELSE
                                 WN_NEW_ADJ(IRG) = WN_NEW_ADJ(IRG) + WC_SUM * 0.001              ! Cummulative WN Capacity
                              END IF
                           END IF
                        END IF
!
                        DO ISP = 1 , ECP_D_MSP
                           EP_SP_CAP_FAC(ISP,IECP,YEAR) = EP_SP_CAP_FAC(ISP,IECP,YEAR) + ECP_SUMMER_SHR(ISP) * WC_SUM + (1.0 - ECP_SUMMER_SHR(ISP)) * WC_WIN
                        END DO
!
                        EPUNIT(IECP,YEAR) =  EPUNIT(IECP,YEAR) + WCOUNT
                        EPECFC(IECP,YEAR) =  EPECFC(IECP,YEAR) + WC_SUM * W_CFA
                        IF (IECP .EQ. WIPV .AND. WVIN .NE. 15) THEN
                            EPECFC_UPV(IECP,YEAR) = EPECFC_UPV(IECP,YEAR) + WC_SUM * W_CFA
                        ENDIF
                        P_MCF(ITYP,YEAR) =  P_MCF(ITYP,YEAR) + WC_SUM * W_CFA
                        P_CAP(ITYP,YEAR) =  P_CAP(ITYP,YEAR) + WC_SUM
!                       WN, PV, SO GENERATION WILL BE DETERMINED IN RENEFD USING RENEWABLE OVERWRITES
                        IF (YEAR .EQ. 1 .AND. WVIN .EQ. 1 .AND. WVIN .NE. 15) THEN
                           EXSGEN(IECP,IRG) = EXSGEN(IECP,IRG) + (WC_SUM * WSEAS(0) + WC_WIN * (1.0 - WSEAS(0))) * 0.001 *  &
                              W_CFA * 8.760
                           EXSGEN(IECP,MNUMNR) = EXSGEN(IECP,MNUMNR) + (WC_SUM * WSEAS(0) + WC_WIN * (1.0 - WSEAS(0))) * 0.001 *  &
                              W_CFA * 8.760
                        END IF
!
!                       ACCUMULATE INTERMITTENT CAPACITY ADDITIONS
!                       FOR NEW TECHNOLOGIES SUBMODULE
!
                        IF (ULCAPC(W_GRP) .GT. 0.0) THEN
                           ECP_CF = ULTGEN(W_GRP) / (8.76 * ULCAPC(W_GRP))
                        ELSE
                           ECP_CF =  W_CFA
                        END IF

                        IF (ULSCAP_ECP(W_GRP,YEAR) .GT. 0.0) THEN
                           TGEN = MAX(1.0 , ULSCAP_ECP(W_GRP,YEAR) * ECP_CF * 8.76)
                        ELSE
                           TGEN = MAX(1.0 , WC_SUM * ECP_CF * 8.76)
                        END IF

                        ULTGEN_ECP(W_GRP,YEAR) = TGEN

                        IF ((UPVTYP(IECP) .EQ. 0) .OR.  (UPTOPR(IECP) .EQ. 3)) THEN
                           ECP_GEN_XPH(YEAR,IECP) = ECP_GEN_XPH(YEAR,IECP) + TGEN
                           P_GEN_XPH(YEAR,ITYP) = P_GEN_XPH(YEAR,ITYP) + TGEN
                           DO INOX = 1 , NOX_GRP
                              IF (LNOX(INOX) .GT. 0) THEN
                                 DO ISP = 1 , ECP_D_MSP
                                    EPNOX_G(ISP,YEAR,IECP,INOX) = EPNOX_G(ISP,YEAR,IECP,INOX) + TGEN * WNOX_R * NOX_ECP(ISP,INOX)
                                    P_NOX(ISP,YEAR,ITYP,INOX) = P_NOX(ISP,YEAR,ITYP,INOX) + TGEN * WNOX_R * NOX_ECP(ISP,INOX)
                                END DO
                              END IF
                           END DO
                        END IF
!
                        IF (YEAR .EQ. 1) THEN
                           IF ((FULLYR .GT. WRFURB) .AND.  (UPLRSYR(IECP) .LE. WRFURB) .AND. (UPLRLYR(IECP) .GE. WRFURB) .AND. (WVIN .NE. 15)) THEN
                              EPNCAP(IECP) = EPNCAP(IECP) + WC_SUM      !
                           END IF
                           IF (MRSW .GT. 0) THEN
                              MUSTRUN(IECP,IRG) = MUSTRUN(IECP,IRG) + WC_SUM * W_CFA * 8.760

!                             WRITE(6,4371) CURIRUN, CURIYR+1989, CURIYR+YEAR+1988, MRSW, W_IGRP, W_GRP, IECP, IRG, MUSTRUN(IECP,IRG), WC_SUM, W_CFA, 8.760

                           END IF
                        ELSE IF (YEAR .EQ. UNXPH) THEN
                           IF ((UPVTYP(IECP) .EQ. 0) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
                              ECP_GEN(IECP) = ECP_GEN(IECP) + TGEN
                              EPFOM(IECP) = EPFOM(IECP) + (WFOMA + WGAA + W_CAPAD) * WC_SUM * UECP_CPEN_ADJ(IECP)
                              ECP_VOM(IECP) = MAX(ECP_VOM(IECP), WVOMA)
                              P_HTRT_MAX(ITYP) = MAX(P_HTRT_MAX(ITYP), WHRATEA / UECP_HTRT_ADJ(IECP))
                              ECP_NOX(IECP) = MAX(ECP_NOX(IECP), WNOX_R)
                              EPVOM(IECP) = EPVOM(IECP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                              IF (TGEN .GT. 1.0) THEN
                                 T_VADJ(W_GRP) = T_VADJ(W_GRP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                                 T_WGRP(W_GRP) = T_WGRP(W_GRP) + TGEN
                              ELSE
                                 T_VADJ(W_GRP) = T_VADJ(W_GRP) + WVOMA * UECP_CPEN_ADJ(IECP)
                                 T_WGRP(W_GRP) = T_WGRP(W_GRP) + 1.0
                              END IF
                              P_GEN(ITYP) = P_GEN(ITYP) + TGEN
                              P_FOM(ITYP) = P_FOM(ITYP) + (WFOMA + WGAA + W_CAPAD) * WC_SUM * UECP_CPEN_ADJ(IECP)
                              P_VOM(ITYP) = P_VOM(ITYP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                              P_HTRT0(ITYP) = P_HTRT0(ITYP) + WHRATEA * TGEN / UECP_HTRT_ADJ(IECP)
                           END IF
                        END IF
!                       DETERMINE GENERATION BY NERC AND CO2 REGION FOR DISTRIBUTED GENERATION SHARES
                        IF (YEAR .EQ. 1 .AND. W_CAR .GT. 0)THEN
                           CO2_DGN(W_CAR,IRG) = CO2_DGN(W_CAR,IRG) + TGEN
                           CO2_DGNT(IRG) = CO2_DGNT(IRG) + TGEN
                        END IF
!
!                STORAGE CAPACITY
!
                     ELSE IF (UCPSTOIS(IECP) .GT. 0) THEN
                        EPECAP(0,IECP,YEAR) =  EPECAP(0,IECP,YEAR) + WC_SUM
                        IF (MRSW .GE. (CURCALYR + YEAR - 1)) EPECAP_MR(0,IECP,YEAR) =  EPECAP_MR(0,IECP,YEAR) + WC_SUM
                        IF (YEAR .EQ. 1) THEN
                           IF ((FULLYR .GT. WRFURB) .AND. (UPLRSYR(IECP) .LE. WRFURB) .AND. (UPLRLYR(IECP) .GE. WRFURB)) THEN
                              EPNCAP(IECP) = EPNCAP(IECP) + WC_SUM
                           END IF
                        END IF
!
!                    RENEWABLE CAPACITY
!
                     ELSE IF (UCPRNWIS(IECP) .GT. 0) THEN
                        IECP2 = UCPRNWIS(IECP)
                        EPECAP(0,IECP,YEAR) = EPECAP(0,IECP,YEAR) + WC_SUM
                        IF (MRSW .GE. (CURCALYR + YEAR - 1)) THEN
                           EPECAP_MR(0,IECP,YEAR) = EPECAP_MR(0,IECP,YEAR) + WC_SUM
                        END IF
                        IF (W_CAR .LE. 0)W_CAR = UPCARG(IRG)
!
                        DO ISP = 1 , ECP_D_MSP
                           EP_SP_CAP_FAC(ISP,IECP,YEAR) = EP_SP_CAP_FAC(ISP,IECP,YEAR) + ECP_SUMMER_SHR(ISP) * WC_SUM + (1.0 - ECP_SUMMER_SHR(ISP)) * WC_WIN
                        END DO
                        FRG = 0

!                       IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR)

                        IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0 .AND. W_CAR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)
!
                        if (frg .le. 0)write (6,3333) curiyr+1989,irg,wstate,uplntcd(iecp),w_grp,w_cr,w_clrg,w_gr,w_car,wc_sum
 3333                   format(1h ,'!car0',i4,i3,a3,a3,i6,i3,i3,i3,i3,f10.1)

                        IF (IECP .EQ. WIWD .OR. IECP .EQ. WIBI) THEN

!                          EPECAP(W_CLRG,IECP,YEAR) =  EPECAP(W_CLRG,IECP,YEAR) + WC_SUM
!                          IF (MRSW .GT. 0) EPECAP_MR(W_CLRG,IECP,YEAR) =  EPECAP_MR(W_CLRG,IECP,YEAR) + WC_SUM

                           FRG = 0

!                          IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR)

                           IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0 .AND. W_CAR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)
                           IF (FRG .GT. 0) EPECAP(FRG,IECP,YEAR) = EPECAP(FRG,IECP,YEAR) + WC_SUM
                           IF (FRG .GT. 0 .AND. MRSW .GE. (CURCALYR + YEAR - 1)) EPECAP_MR(FRG,IECP,YEAR) = EPECAP_MR(FRG,IECP,YEAR) + WC_SUM
                        END IF
                        EPUNIT(IECP,YEAR) =  EPUNIT(IECP,YEAR) + WCOUNT
                        EPECFC(IECP,YEAR) =  EPECFC(IECP,YEAR) + WC_SUM * W_CFA
                        P_MCF(ITYP,YEAR) =  P_MCF(ITYP,YEAR) + WC_SUM * W_CFA
                        P_CAP(ITYP,YEAR) =  P_CAP(ITYP,YEAR) + WC_SUM
!
!                       ACCUMULATE RENEWABLE CAPACITY ADDITIONS
!                       FOR NEW TECHNOLOGIES SUBMODULE
!
                        IF (ULCAPC(W_GRP) .GT. 0.0) THEN
                           ECP_CF = ULTGEN(W_GRP) / (8.76 * ULCAPC(W_GRP))
                        ELSE
                           ECP_CF =  W_CFA
                        END IF

                        IF (ISNAN(ECP_CF)) THEN
                           WRITE(18,8369) CURIRUN, CURIYR+1989, CURIYR+YEAR+1988, W_GRP, IECP, ULTGEN(W_GRP), ULCAPC(W_GRP), W_CFA, WC_SUM
 8369                      FORMAT(1X,"BAD_RENEWABLE_CF",5(":",I5),4(":",F21.6))

                           ECP_CF = 0.01
                        END IF

                        IF (ULSCAP_ECP(W_GRP,YEAR) .GT. 0.0) THEN
                           TGEN = MAX(1.0 , ULSCAP_ECP(W_GRP,YEAR) * ECP_CF * 8.76)
                        ELSE
                           TGEN = MAX(1.0 , WC_SUM * ECP_CF * 8.76)
                        END IF

                        ULTGEN_ECP(W_GRP,YEAR) = TGEN

                        IF ((UPVTYP(IECP) .EQ. 0) .OR.  (UPTOPR(IECP) .EQ. 3)) THEN
                           ECP_GEN_XPH(YEAR,IECP) = ECP_GEN_XPH(YEAR,IECP) + TGEN
                           P_GEN_XPH(YEAR,ITYP) = P_GEN_XPH(YEAR,ITYP) + TGEN
                           DO INOX = 1 , NOX_GRP
                              IF (LNOX(INOX) .GT. 0) THEN
                                 DO ISP = 1 , ECP_D_MSP
                                    EPNOX_G(ISP,YEAR,IECP,INOX) = EPNOX_G(ISP,YEAR,IECP,INOX) + TGEN * WNOX_R * NOX_ECP(ISP,INOX)
                                    P_NOX(ISP,YEAR,ITYP,INOX) = P_NOX(ISP,YEAR,ITYP,INOX) + TGEN * WNOX_R * NOX_ECP(ISP,INOX)
                                    IF (WNOX_R .GT. 0.0 .AND. (IECP .EQ. 49 .OR. IECP .EQ. 47)) THEN
                                       WRITE(18,9370) CURIYR+UHBSYR,CURIYR+YEAR-1+UHBSYR,CURITR,IRG,IECP,INOX,ISP,W_GRP,WSTATE, &
                                          EPNOX_G(ISP,YEAR,IECP,INOX),TGEN,WNOX_R,NOX_ECP(ISP,INOX)
 9370                                  FORMAT(1X,"EPNOX_G_MSW",8(":",I5),":",A2,4(":",F12.3))
                                    END IF
                                END DO
                              END IF
                           END DO
                        END IF

!                       Weight Summer and Winter Capacity to Achieve Average by ECP Season

                        ECPSCAP = 0.0
                        DO ISP = 1 , EPNMSP
                           DO IMO = 1 , 12
                              ECPSCAP(ISP) = ECPSCAP(ISP) + (SUMMER_MONTHS(IMO) * WC_SUM + (1.0 - SUMMER_MONTHS(IMO)) * WC_WIN) * (MOTOESP(ISP,IMO) / MOTOESP(0,IMO))
                           END DO
                        END DO
!
!                       Do Hydro adjustments by year and plant group
!
                        MYR = MIN(UNYEAR , CURIYR + YEAR - 1)
                        DO ISP = 1 , EPNMSP
                           DO IMO = 1 , 12
                              ECFCAP(IECP2,ISP,YEAR) = ECFCAP(IECP2,ISP,YEAR) + ECPSCAP(ISP) * (MOTOESP(ISP,IMO) / MOTOESP(0,IMO))
                              IF (IECP .EQ. WIHY) THEN
                                 IF (WVIN .NE. 3) THEN
                                    IF (USW_HYD .GT. 0) THEN
                                       ECAPCF(IECP2,ISP,YEAR) = ECAPCF(IECP2,ISP,YEAR) + ECPSCAP(ISP) * MAX(WCF_M(IMO) , 0.001) * UQHYFAC(MYR,IRG) * URHYCFA(MYR) * (MOTOESP(ISP,IMO) / MOTOESP(0,IMO))
                                    ELSE
                                       ECAPCF(IECP2,ISP,YEAR) = ECAPCF(IECP2,ISP,YEAR) + ECPSCAP(ISP) * MAX(WCF_M(IMO) , 0.001) * URHYCFA(MYR) * (MOTOESP(ISP,IMO) / MOTOESP(0,IMO))
                                    END IF
                                 ELSE
                                    ECAPCF(IECP2,ISP,YEAR) = ECAPCF(IECP2,ISP,YEAR) + ECPSCAP(ISP) * MAX(WCF_M(IMO) , 0.001) * (MOTOESP(ISP,IMO) / MOTOESP(0,IMO))
                                 END IF
                              ELSE
                                 ECAPCF(IECP2,ISP,YEAR) = ECAPCF(IECP2,ISP,YEAR) + ECPSCAP(ISP) * MAX(WCF_M(IMO) , 0.001) * (MOTOESP(ISP,IMO) / MOTOESP(0,IMO))
                              END IF
                           END DO
                        END DO
!
                        IF (YEAR .EQ. 1) THEN
                           IF ((FULLYR .GT. WRFURB) .AND. (UPLRSYR(IECP) .LE. WRFURB) .AND. (UPLRLYR(IECP) .GE. WRFURB)) THEN
                              EPNCAP(IECP) = EPNCAP(IECP) + WC_SUM      !
                           END IF

!                          ASSIGN WINTER OR SUMMER CAPACITY TO ECP SEASON

                           DO ISP = 1 , EPNMSP
                              GECPSCAP(IECP2,ISP) = GECPSCAP(IECP2,ISP) + ECPSCAP(ISP)
                           END DO

                           RNW_CFC(IECP2) = RNW_CFC(IECP2) + WC_SUM * W_CFA

                           RMCF = 0.0
                           DO IMO = 1 , 12
                             RMCF = RMCF + WCF_M(IMO)
                           END DO
                           RMCF = RMCF / 12.0

!                          HY EXISTING GENERATION WILL BE DETERMINED IN RENEFD USING RENEWABLE OVERWRITES
                           IF (IECP .EQ. WIHY) THEN
                              IF (WVIN .NE. 3) THEN
                                 IF (USW_HYD .GT. 0) THEN
                                    RNW_MCFC(IECP2) = RNW_MCFC(IECP2) + WC_SUM * UQHYFAC(MYR,IRG) * URHYCFA(MYR) * RMCF
                                  IF (WVIN .EQ. 1) THEN
                                    EXSGEN(IECP,IRG) = EXSGEN(IECP,IRG) + (WC_SUM * WSEAS(0) + WC_WIN * (1.0 - WSEAS(0))) * 0.001 *  &
                                                       UQHYFAC(MYR,IRG) * URHYCFA(MYR) * RMCF * 8.760
                                    EXSGEN(IECP,MNUMNR) = EXSGEN(IECP,MNUMNR) + (WC_SUM * WSEAS(0) + WC_WIN * (1.0 - WSEAS(0))) * 0.001 *  &
                                                          UQHYFAC(MYR,IRG) * URHYCFA(MYR) * RMCF * 8.760
                                  END IF
                                 ELSE
                                    RNW_MCFC(IECP2) = RNW_MCFC(IECP2) + WC_SUM * URHYCFA(MYR) * RMCF
                                  IF (WVIN .EQ. 1) THEN
                                    EXSGEN(IECP,IRG) = EXSGEN(IECP,IRG) + (WC_SUM * WSEAS(0) + WC_WIN * (1.0 - WSEAS(0))) * 0.001 *  &
                                                       URHYCFA(MYR) * RMCF * 8.760
                                    EXSGEN(IECP,MNUMNR) = EXSGEN(IECP,MNUMNR) + (WC_SUM * WSEAS(0) + WC_WIN * (1.0 - WSEAS(0))) * 0.001 *  &
                                                          URHYCFA(MYR) * RMCF * 8.760
                                  END IF
                                 END IF
                              ELSE
                                 RNW_MCFC(IECP2) = RNW_MCFC(IECP2) + WC_SUM * RMCF
                              END IF
                           ELSE
                              RNW_MCFC(IECP2) = RNW_MCFC(IECP2) + WC_SUM * RMCF
!                             EXISTING GENERATION FOR GEOTHERMAL DETERMINED LATER
                              IF (WVIN .EQ. 1 .AND. IECP .NE. WIGT) THEN
                                 EXSGEN(IECP,IRG) = EXSGEN(IECP,IRG) + (WC_SUM * WSEAS(0) + WC_WIN * (1.0 - WSEAS(0))) * 0.001 * RMCF * 8.760
                                 EXSGEN(IECP,MNUMNR) = EXSGEN(IECP,MNUMNR) + (WC_SUM * WSEAS(0) + WC_WIN * (1.0 - WSEAS(0))) * 0.001 * RMCF * 8.760
                              END IF
                           END IF

                           IF (MRSW .GT. 0) THEN
                              MUSTRUN(IECP,IRG) = MUSTRUN(IECP,IRG) + WC_SUM * W_CFA * 8.760

!                             WRITE(6,4371) CURIRUN, CURIYR+1989, CURIYR+YEAR+1988, MRSW, W_IGRP, W_GRP, IECP, IRG, MUSTRUN(IECP,IRG), WC_SUM, W_CFA, 8.760
 4371                         FORMAT(1X,"MUSTRUN_RNW_UDAT",8(":",I5),4(":",F21.6))

                           END IF
                        ELSE IF (YEAR .EQ. UNXPH) THEN

!                          IF ((UPVTYP(IECP) .EQ. 0) .OR.  (UPTOPR(IECP) .EQ. 3)) THEN

                           ECP_GEN(IECP) = ECP_GEN(IECP) + TGEN
                           P_HTRT_MAX(ITYP) = MAX(P_HTRT_MAX(ITYP), WHRATEA / UECP_HTRT_ADJ(IECP))
                           IF ((UPVTYP(IECP) .EQ. 0) .OR.  (UPTOPR(IECP) .EQ. 3)) THEN
                              EPFOM(IECP) = EPFOM(IECP) + (WFOMA + WGAA + W_CAPAD) * WC_SUM * UECP_CPEN_ADJ(IECP)
                           END IF
                           ECP_VOM(IECP) = MAX(ECP_VOM(IECP), WVOMA)
                           ECP_NOX(IECP) = MAX(ECP_NOX(IECP), WNOX_R)
                           IF ((UPVTYP(IECP) .EQ. 0) .OR.  (UPTOPR(IECP) .EQ. 3)) THEN
                              EPVOM(IECP) = EPVOM(IECP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                           END IF
                           IF (TGEN .GT. 1.0) THEN
                              T_VADJ(W_GRP) = T_VADJ(W_GRP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                              T_WGRP(W_GRP) = T_WGRP(W_GRP) + TGEN
                           ELSE
                              T_VADJ(W_GRP) = T_VADJ(W_GRP) + WVOMA * UECP_CPEN_ADJ(IECP)
                              T_WGRP(W_GRP) = T_WGRP(W_GRP) + 1.0
                           END IF
                           P_GEN(ITYP) = P_GEN(ITYP) + TGEN
                           P_FOM(ITYP) = P_FOM(ITYP) + (WFOMA + WGAA + W_CAPAD) * WC_SUM * UECP_CPEN_ADJ(IECP)
                           P_VOM(ITYP) = P_VOM(ITYP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                           P_HTRT0(ITYP) = P_HTRT0(ITYP) + WHRATEA * TGEN / UECP_HTRT_ADJ(IECP)

!                          END IF

                        END IF           ! YEAR .EQ. 1

!                       DETERMINE GENERATION BY NERC AND CO2 REGION FOR GEO, MSW, AND DISTRIBUTED GENERATION SHARES

                        IF (YEAR .EQ. 1 .AND. W_CAR .GT. 0)THEN
                           IF (IECP .EQ. WIGT)THEN
                              CO2_GEO(W_CAR,IRG) = CO2_GEO(W_CAR,IRG) + TGEN
                              CO2_GEOT(IRG) = CO2_GEOT(IRG) + TGEN
                           END IF
                           IF (IECP .EQ. WIMS)THEN
                              CO2_MSW(W_CAR,IRG) = CO2_MSW(W_CAR,IRG) + TGEN
                              CO2_MSWT(IRG) = CO2_MSWT(IRG) + TGEN
                           END IF
                              CO2_DGN(W_CAR,IRG) = CO2_DGN(W_CAR,IRG) + TGEN
                              CO2_DGNT(IRG) = CO2_DGNT(IRG) + TGEN
                        END IF
!
!                    DISTRIBUTED GENERATION CAPACITY
!
                     ELSE IF (UCPDGNIS(IECP) .GT. 0) THEN
                        EPECAP(0,IECP,YEAR) = EPECAP(0,IECP,YEAR) + WC_SUM
                        IF (MRSW .GE. (CURCALYR + YEAR - 1)) EPECAP_MR(0,IECP,YEAR) = EPECAP_MR(0,IECP,YEAR) + WC_SUM
                        IF (W_CAR .LE. 0)W_CAR = UPCARG(IRG)
!
                        DO ISP = 1 , ECP_D_MSP
                           EP_SP_CAP_FAC(ISP,IECP,YEAR) = EP_SP_CAP_FAC(ISP,IECP,YEAR) + ECP_SUMMER_SHR(ISP) * WC_SUM + (1.0 - ECP_SUMMER_SHR(ISP)) * WC_WIN
                        END DO
!
                        EPUNIT(IECP,YEAR) =  EPUNIT(IECP,YEAR) + WCOUNT
                        EPECFC(IECP,YEAR) =  EPECFC(IECP,YEAR) + WC_SUM * W_CFA
                        P_MCF(ITYP,YEAR) =  P_MCF(ITYP,YEAR) + WC_SUM * W_CFA
                        P_CAP(ITYP,YEAR) =  P_CAP(ITYP,YEAR) + WC_SUM
!
!                       ACCUMULATE DISTRIBUTED GENERATION CAPACITY ADDITIONS
!                       FOR NEW TECHNOLOGIES SUBMODULE
!
                        IF (ULCAPC(W_GRP) .GT. 0.0) THEN
                           ECP_CF = ULTGEN(W_GRP) / (8.76 * ULCAPC(W_GRP))
                        ELSE
                           ECP_CF =  W_CFA
                        END IF

                        IF (ULSCAP_ECP(W_GRP,YEAR) .GT. 0.0) THEN
                           TGEN = MAX(1.0 , ULSCAP_ECP(W_GRP,YEAR) * ECP_CF * 8.76)
                        ELSE
                           TGEN = MAX(1.0 , WC_SUM * ECP_CF * 8.76)
                        END IF

                        ULTGEN_ECP(W_GRP,YEAR) = TGEN

                        IF ((UPVTYP(IECP) .EQ. 0) .OR.  (UPTOPR(IECP) .EQ. 3)) THEN
                           ECP_GEN_XPH(YEAR,IECP) = ECP_GEN_XPH(YEAR,IECP) + TGEN
                           P_GEN_XPH(YEAR,ITYP) = P_GEN_XPH(YEAR,ITYP) + TGEN
                           DO INOX = 1 , NOX_GRP
                              IF (LNOX(INOX) .GT. 0) THEN
                                 DO ISP = 1 , ECP_D_MSP
                                    EPNOX_G(ISP,YEAR,IECP,INOX) = EPNOX_G(ISP,YEAR,IECP,INOX) + TGEN * WNOX_R * NOX_ECP(ISP,INOX)
                                    P_NOX(ISP,YEAR,ITYP,INOX) = P_NOX(ISP,YEAR,ITYP,INOX) + TGEN * WNOX_R * NOX_ECP(ISP,INOX)
                                END DO
                              END IF
                           END DO
                        END IF
!
                        IF (YEAR .EQ. 1) THEN
                           IF ((FULLYR .GT. WRFURB) .AND. (UPLRSYR(IECP) .LE. WRFURB) .AND. (UPLRLYR(IECP) .GE. WRFURB)) THEN
                              EPNCAP(IECP) = EPNCAP(IECP) + WC_SUM      !
                           END IF
                           IF (MRSW .GT. 0) THEN
                              MUSTRUN(IECP,IRG) = MUSTRUN(IECP,IRG) + WC_SUM * W_CFA * 8.760

!                             WRITE(6,4371) CURIRUN, CURIYR+1989, CURIYR+YEAR+1988, MRSW, W_IGRP, W_GRP, IECP, IRG, MUSTRUN(IECP,IRG), WC_SUM, W_CFA, 8.760

                           END IF
                        ELSE IF (YEAR .EQ. UNXPH) THEN
                           IF ((UPVTYP(IECP) .EQ. 0) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
                              ECP_GEN(IECP) = ECP_GEN(IECP) + TGEN
                              EPFOM(IECP) = EPFOM(IECP) + (WFOMA + WGAA + W_CAPAD) * WC_SUM * UECP_CPEN_ADJ(IECP)
                              ECP_VOM(IECP) = MAX(ECP_VOM(IECP), WVOMA)
                              P_HTRT_MAX(ITYP) = MAX(P_HTRT_MAX(ITYP), WHRATEA / UECP_HTRT_ADJ(IECP))
                              ECP_NOX(IECP) = MAX(ECP_NOX(IECP), WNOX_R)
                              EPVOM(IECP) = EPVOM(IECP) +  WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                              IF (TGEN .GT. 1.0) THEN
                                 T_VADJ(W_GRP) = T_VADJ(W_GRP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                                 T_WGRP(W_GRP) = T_WGRP(W_GRP) + TGEN
                              ELSE
                                 T_VADJ(W_GRP) = T_VADJ(W_GRP) + WVOMA * UECP_CPEN_ADJ(IECP)
                                 T_WGRP(W_GRP) = T_WGRP(W_GRP) + 1.0
                              END IF
                              P_GEN(ITYP) = P_GEN(ITYP) + TGEN
                              P_FOM(ITYP) = P_FOM(ITYP) + (WFOMA + WGAA + W_CAPAD) * WC_SUM * UECP_CPEN_ADJ(IECP)
                              P_VOM(ITYP) = P_VOM(ITYP) + WVOMA * TGEN * UECP_CPEN_ADJ(IECP)
                              P_HTRT0(ITYP) = P_HTRT0(ITYP) + WHRATEA * TGEN / UECP_HTRT_ADJ(IECP)
                           END IF
                        END IF
                     END IF           ! DSP, INT, RNW, DGN
                  END IF           ! END IF PLANT IN OP IN CURRENT YEAR
               END DO           ! YEAR TO UNXPH
!
!              accumulate capacity mapping to assign new combined fuel region
!                 IF PLANT IS IN OPERATION IN CURRENT YEAR
!
              IF ((IYR .GE. W_SYR) .AND. (IYR .LE. W_RYR)) THEN
               IF (IOWN .LE. 4 .AND. WNOWN .LE. UNRGNS) THEN

                  if (W_CR .eq. 0 .or. W_CR .GT. MNUMCR) write(6,*) 'error with census region',W_CR
                  if (W_CLRG .eq. 0 .or. W_CLRG .GT. EFD_D_MFRG) write(6,*) 'error with coal region',W_CLRG
                  if (W_GR .eq. 0 .or. W_GR .GT. EFD_D_MFRG)  write(6,*) 'error with gas region',W_GR

                  CPFLRG(W_CR,W_CLRG,W_GR) = CPFLRG(W_CR,W_CLRG,W_GR) + WC_SUM
                  CPFLEFD(IEFD,W_CR,W_CLRG,W_GR) = CPFLEFD(IEFD,W_CR,W_CLRG,W_GR) + WC_SUM
                  CPFLECP(IECP,W_CR,W_CLRG,W_GR) = CPFLECP(IECP,W_CR,W_CLRG,W_GR) + WC_SUM

!                  WRITE(22,3113) CURIRUN, CURIYR+1989, CURITR, IOWN, WNOWN, W_IGRP, W_GRP, W_SYR, W_RYR, IECP, IEFD, W_CR, W_CLRG, W_GR, WC_SUM
! 3113             FORMAT(1X,"UDAT_CPFLECP",14(":",I5),":",F21.6)

               END IF
              END IF
!
!
!              UPDATE END OF YEAR CAPACITY
!
               IYRMO = IYR * 100 + 12
               RYRMO = W_RYR * 100 + W_RMO
               SYRMO = W_SYR * 100 + W_SMO
!
!              ACCUMULATE ANNUAL STARTS FOR PREVIOUS CAPACITY ADDITIONS
!
               IF (((W_SYR - UHBSYR) - UPPLYR(IECP)) .GT. 0)THEN

!                 IF (CURIYR .EQ. 6) WRITE(6,3411) CURIYR+UHBSYR,CURITR,W_IGRP,W_GRP,W_GRP2,W_SYR,WVIN,IECP,WC_SUM
!3411             FORMAT(1X,"GROUPS",8(":",I5),":",F9.3)

                  IF ((WVIN .EQ. 1 .OR. WVIN .EQ. 2 .OR. WVIN .EQ.3 .OR. WVIN .GT. 9) .AND. WVIN .NE. 15) THEN
                     KECP = IECP
                    IF (IECP .EQ. WIWL) KECP = WIWN    !this is accounted for in ucape already - linking WL/WN and PT/PV
                    IF (IECP .EQ. WIPT) KECP = WIPV    ! so not needed for ESTYRADD, but is needed for ESTYRPLN (for elastcity limit)
                     ESTYRADD(IECP,W_SYR - UHBSYR - UPPLYR(KECP)) = ESTYRADD(IECP,W_SYR - UHBSYR - UPPLYR(KECP)) + WC_SUM
                     IF (WVIN .EQ. 2 .AND. W_SYR .GE. (CURIYR + UHBSYR))THEN
                        ESTYRPLN(KECP,W_SYR - UHBSYR) = ESTYRPLN(KECP,W_SYR - UHBSYR) + WC_SUM
                     END IF
                  END IF
               END IF
               IF (UPTTYP(IECP) .LE. NW_COAL .OR. IECP .EQ. WIA2) THEN
                  CCSADJ = CCSCAPA
               ELSE
                  CCSADJ = 0.0
               END IF
!
!              GET STATE NUMBER
!
               ITST = 0
               W_ST = 0
               DO KRG = 1 , 49
                  IF (WSTATE .EQ. USTNME(KRG))THEN
                     W_ST = KRG
                     ITST = 1
                  END IF
                  IF (ITST .EQ. 1)EXIT
               END DO

               IF ((SYRMO .LE. IYRMO) .AND. (RYRMO .GE. IYRMO)) THEN
                  EYSCAP = EYSCAP + WC_SUM
                  AGE = MAX(MIN(UNYEAR+ECP_D_FPH , CURIYR + UHBSYR - WRFURB + 1),1)
                  IECP = WECPT
                  CYEAR_ECP = WECPT
                  CYEAR_EFD = WEFDT
                  OWRGN = WNOWN
                  PHRGN = WNOPER

                  EYFCST = EYFCST + WC_SUM * (WFOMA + WGAA + W_CAPAD + UPXTRA(AGE,IECP)) * 0.001
                  IF (IOWN .GE. 3 .AND. (WVIN .EQ. 3 .OR. WVIN .EQ. 20)) THEN
                     EYCCST = EYCCST + WC_SUM * WPCST * 0.001 *  (DBLE(1.0) / UPGNPD(CURIYR))
                  END IF

!                 Store Wind Capacity by Source Region for Resource Calculations

                  IF (IECP .EQ. WIWN) THEN
                     UCAPWNR(PHRGN,CURIYR) = UCAPWNR(PHRGN,CURIYR) + WC_SUM
                     UCAPWNR(MNUMNR,CURIYR) = UCAPWNR(MNUMNR,CURIYR) + WC_SUM
!                    WRITE(6,4013) CURIRUN,CURIYR+1989,CURITR,PHRGN,OWRGN,W_IGRP,W_GRP,W_SYR,WVIN,IECP,WC_SUM,UCAPWNR(PHRGN,CURIYR)
 4013                FORMAT(1X,"UCAPWNR_UDAT",10(":",I5),2(":",F15.3))
                  END IF

                  IF (IECP .EQ. WIWL) THEN
                     UCAPWLR(PHRGN,CURIYR) = UCAPWLR(PHRGN,CURIYR) + WC_SUM
                     UCAPWLR(MNUMNR,CURIYR) = UCAPWLR(MNUMNR,CURIYR) + WC_SUM
!                    WRITE(6,7013) CURIRUN,CURIYR+1989,CURITR,PHRGN,OWRGN,W_IGRP,W_GRP,W_SYR,WVIN,IECP,WC_SUM,UCAPWLR(PHRGN,CURIYR)
 7013                FORMAT(1X,"UCAPWLR_UDAT",10(":",I5),2(":",F15.3))
                  END IF

                  IF (IECP .EQ. WIWF) THEN
                     UCAPWFR(PHRGN,CURIYR) = UCAPWFR(PHRGN,CURIYR) + WC_SUM
                     UCAPWFR(MNUMNR,CURIYR) = UCAPWFR(MNUMNR,CURIYR) + WC_SUM
                  END IF
!
!                 ACCUMULATE HISTORICAL "AFFECTED" GENERATION (111d)
!
                  IF ((CURIYR + UHBSYR) .LE. CO2_STDBY .AND. W_ST .GT. 0)THEN
                     CO2_GENTL(W_ST) = CO2_GENTL(W_ST) + WC_SUM * W_CFA * 8.760 / 1000.0
                     CO2_GENTN(WNOWN,W_ST) = CO2_GENTN(WNOWN,W_ST) + WC_SUM * W_CFA * 8.760 / 1000.0
                     IF (CO2_PLTSW(IECP) .GT. 0.0)THEN
                        CO2_GENQF(W_ST) = CO2_GENQF(W_ST) + WC_SUM * W_CFA * 8.760 * CO2_PLTSW(IECP) / 1000.0
                        CO2_GENQN(WNOWN,W_ST) = CO2_GENQN(WNOWN,W_ST) + WC_SUM * W_CFA * 8.760 * CO2_PLTSW(IECP) / 1000.0
                     END IF
                  END IF
!
!                 FOR 111d, ACCUMULATE MUST-RUN GENERATION AND CO2 TO INCLUDE IN STANDARD
!
                  IF (MRSW .GT. 0 .AND. CO2_STDSW .GT. 0 .AND. CO2_STDRS(1,CURIYR + UNXPH - 1) .GT. 0.0 .AND. CO2_PLTSW(IECP) .GT. 0.0 .AND. W_ST .GT. 0)THEN
                     MRGEN = (WC_SUM * 0.001) * W_CF * 8.760
                     MRCO2 = 0.0
                     TLSHR = 1.0
                     DO IFL = 1 , EIFPLT
                        IF (W_FSHR(IFL) .GT. 0.0)THEN
                           FLSHR = MIN(W_FSHR(IFL),TLSHR)
                           IF (FLSHR .GT. 0.0)THEN
!                    write(6,3343) curiyr+1989,irg,w_st,est_frg(w_st),uplntcd(iecp),wfown,wc_sum,whrate,w_cf,ifl,wfl(ifl),uflcode(wfl(ifl)),w_fshr(ifl),flshr
!3343 format(1h ,'!mrunt',i4,i3,i3,i3,a3,i3,3f10.3,2i3,a3,2f10.3)
                              IF (WFL(IFL) .LE. UIIS)THEN
                                 MRCO2 = MRCO2 + UFRCO2(IECP,W_CLRG) * W_FSHR(IFL)
                              ELSE IF (WFL(IFL) .EQ. UIGF .OR. WFL(IFL) .EQ. UIGC .OR. WFL(IFL) .EQ. UIGI) THEN
                                 MRCO2 = MRCO2 + ENGEL(CURIYR) * (44.0 / 12.0) * 2.204 * W_FSHR(IFL)
                              ELSE IF (WFL(IFL) .EQ. UIRL .OR. WFL(IFL) .EQ. UIRH) THEN
                                 MRCO2 = MRCO2 + ERSEL(CURIYR) * (44.0 / 12.0) * 2.204 * W_FSHR(IFL)
                              ELSE IF (WFL(IFL) .EQ. UIWD) THEN
                                 MRCO2 = MRCO2 + CO2_EMSWD * W_FSHR(IFL)
                              END IF
                              TLSHR = TLSHR - FLSHR
                           END IF
                        END IF
                     END DO
                     MRCO2 = MRCO2 * (WHRATE * 0.001)
                     IF (IECP .LE. ECP_D_DSP)THEN
                        MRCO2 = MRCO2 * (1.0 - UPPCEF(IECP))
                     END IF
!                    APPLY NT COGEN CREDIT, IF APPROPRIATE
                     IF (WFOWN .EQ. 4)MRCO2 = MRCO2 / CO2_ADJNT
!
                     DO JYR = MAX(CURIYR,CO2_STDY1 - UHBSYR) , MNUMYR + ECP_D_XPH
                        IF ((JYR + UHBSYR) .GE. W_SYR .AND. (JYR + UHBSYR) .LE. W_RYR .AND. (JYR + UHBSYR) .LE. MRSW)THEN
                           MRGENF(EST_FRG(W_ST),JYR) = MRGENF(EST_FRG(W_ST),JYR) + MRGEN
                           MRCO2F(EST_FRG(W_ST),JYR) = MRCO2F(EST_FRG(W_ST),JYR) + MRGEN * MRCO2
                           MRGENN(IRG,JYR) = MRGENN(IRG,JYR) + MRGEN
                           MRCO2N(IRG,JYR) = MRCO2N(IRG,JYR) + MRGEN * MRCO2
                        END IF
                     END DO
!                    write(6,4343) curiyr+1989,irg,w_st,ustnme(w_st),est_frg(w_st),uplntcd(iecp),wfown,wc_sum,whrate,w_cf,mrgen,mrco2,mrgen*mrco2,  &
!                                  mrgenf(est_frg(w_st),curiyr),mrco2f(est_frg(w_st),curiyr),mrgenn(irg,curiyr),mrco2n(irg,curiyr)
!4343 format(1h ,'!mrco2',i4,i3,i3,a3,i3,a3,i3,f6.1,f8.1,f6.3,3f8.2,4f11.2)
                  END IF

!                 IF FINAL CO2 PERFORMANCE STANDARDS, DETERMINE INCREMENTAL GEN FOR AFFECTED TYPES

!
                  IF (CO2_INCSW(IECP) .GT. 0)THEN
                     IF (IECP .EQ. WIHY)THEN
                        W_CFP = W_CF * UQHYFAC(CURIYR,IRG)
                     ELSE
                        W_CFP = W_CF
                     END IF
                     IF (CO2_AFFYR .LE. 0)THEN
                        IF (WVIN .EQ. 3 .AND. W_SYR .EQ. (CURIYR + UHBSYR))THEN
                           EPBLDGEN(IECP,IRG,W_SYR - UHBSYR) = EPBLDGEN(IECP,IRG,W_SYR - UHBSYR) + WC_SUM * 0.001 * W_CFP * 8.760
                           EPBLDGEN(IECP,MNUMNR,W_SYR - UHBSYR) = EPBLDGEN(IECP,MNUMNR,W_SYR - UHBSYR) + WC_SUM * 0.001 * W_CFP * 8.760
                        END IF
                     ELSE
                        IF ((WVIN .LE. 3 .OR. (WVIN .GT. 9 .AND. WVIN .NE. 15)) .AND. W_SYR .GT. CO2_AFFYR .AND. W_SYR .EQ. (CURIYR + UHBSYR))THEN
                           EPBLDGEN(IECP,IRG,W_SYR - UHBSYR) = EPBLDGEN(IECP,IRG,W_SYR - UHBSYR) + WC_SUM * 0.001 * W_CFP * 8.760
                           EPBLDGEN(IECP,MNUMNR,W_SYR - UHBSYR) = EPBLDGEN(IECP,MNUMNR,W_SYR - UHBSYR) + WC_SUM * 0.001 * W_CFP * 8.760
                        END IF
                     END IF
                  ENDIF


                  IF (IEFD .LE. EFD_D_DSP) THEN
                     IF (IECP .EQ. WIIS) CAPCCS(1,FRG,CURIYR) = CAPCCS(1,FRG,CURIYR) + WC_SUM/1000
                     IF (IECP .EQ. WIIS) CAPCCS(1,MAXNFR,CURIYR) = CAPCCS(1,MAXNFR,CURIYR) + WC_SUM/1000
                     IF (IECP .EQ. WICS) CAPCCS(2,FRG,CURIYR) = CAPCCS(2,FRG,CURIYR) + WC_SUM/1000
                     IF (IECP .EQ. WICS) CAPCCS(2,MAXNFR,CURIYR) = CAPCCS(2,MAXNFR,CURIYR) + WC_SUM/1000
                     IF (IECP .LE. ECP_D_DSP .AND. UPTTYP(IECP) .LE. NW_COAL .AND. UPVTYP(IECP) .LE. 0)THEN
                        IF (UPPCEF(IECP) .GT. 0.0) THEN
                            ECSCAPSQ(IOWN) = ECSCAPSQ(IOWN) + WC_SUM / (1.0 - CCSADJ)
!                            IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0 .AND. W_CAR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)
                            CAPCCS(3,FRG,CURIYR) = CAPCCS(3,FRG,CURIYR)  + WC_SUM/ (1.0 - CCSADJ)/1000
                            CAPCCS(3,MAXNFR,CURIYR) = CAPCCS(3,MAXNFR,CURIYR)  + WC_SUM/ (1.0 - CCSADJ)/1000
                            EMELCCS(IRG,CURIYR) = EMELCCS(IRG,CURIYR) + WC_SUM/ (1.0 - CCSADJ)/1000
                            EMELCCS(MNUMNR,CURIYR) = EMELCCS(MNUMNR,CURIYR) +  WC_SUM/ (1.0 - CCSADJ)/1000
                            EMELCDR(IRG,CURIYR) = EMELCDR(IRG,CURIYR) + WC_SUM/ 1000
                            EMELCDR(MNUMNR,CURIYR) = EMELCDR(MNUMNR,CURIYR) + WC_SUM/ 1000
                        ENDIF
                     END IF
                     IF (IECP .EQ. WIA2)THEN
                        IF (UPPCEF(IECP) .GT. 0.0) THEN
                            CAPCCS(4,FRG,CURIYR) = CAPCCS(4,FRG,CURIYR)  + WC_SUM/ (1.0 - CCSADJ)/1000
                            CAPCCS(4,MAXNFR,CURIYR) = CAPCCS(4,MAXNFR,CURIYR)  + WC_SUM/ (1.0 - CCSADJ)/1000
                            EMELGCS(IRG,CURIYR) = EMELGCS(IRG,CURIYR) + WC_SUM/ (1.0 - CCSADJ)/1000
                            EMELGCS(MNUMNR,CURIYR) = EMELGCS(MNUMNR,CURIYR) +  WC_SUM/ (1.0 - CCSADJ)/1000
                            EMELGDR(IRG,CURIYR) = EMELGDR(IRG,CURIYR) + WC_SUM/ 1000
                            EMELGDR(MNUMNR,CURIYR) = EMELGDR(MNUMNR,CURIYR) + WC_SUM/ 1000

                            IF (W_SYR .EQ. (curiyr+1989)) THEN
                               EG_PLTS = EG_PLTS + 1
                               EG_IGRP(EG_PLTS) = W_IGRP
                               EG_RY(EG_PLTS) = W_SYR
                               EG_RG(EG_PLTS) = FRG
                               EG_PTP(EG_PLTS) = WIA2
                            ENDIF
                        ENDIF
                     END IF
!                    DO KRG=1,3

!                       IF (CAPCCS(KRG,FRG,CURIYR).GT.0.0) WRITE(6,*)'debug capccs ',KRG,FRG,CURIYR,CAPCCS(KRG,FRG,CURIYR)
!                    ENDDO
                     IF ((IECP .EQ. WIIS .or. IECP .EQ. WICS ) .and. W_SYR.EQ.(curiyr+1989) .and. WVIN.eq.2 ) THEN
                                               N_PLTS = N_PLTS + 1
                                               N_IGRP(N_PLTS) = W_IGRP
                                               N_RY(N_PLTS) = W_SYR
                                               N_CFR(N_PLTS) = W_CF
                                               N_RG(N_PLTS) = FRG
                                               N_HRAT(N_PLTS) = WHRATE
                                               N_CPTY(N_PLTS) = WC_SUM
                                               IF (IECP .EQ. WIIS) N_PTP(N_PLTS) = WIIS
                                               IF (IECP .EQ. WICS) N_PTP(N_PLTS) = WICS
                           write(18,1433) curiyr+1989,N_PLTS,N_RY(N_PLTS),N_IGRP(N_PLTS),N_RG(N_PLTS),WC_SUM*0.001,N_CFR(N_PLTS),N_HRAT(N_PLTS),FRG
                      1433 format(1h,'!ccsout4 ',I5,1x,I4,1x,3(F10.0,1x),3(F10.3,1x),I4)
                     ENDIF
                     IF (IVIN3 .EQ. 1) THEN
                           ECSCAP(IEFD,IVIN3,IOWN) = ECSCAP(IEFD,IVIN3,IOWN) + WC_SUM / (1.0 - CCSADJ)
                          IF (IECP .EQ. WING .AND. W_FSHR(2) .GT. 0.0) THEN    !add to variable tracking coal/gas cofiring subset of WING
                              UCAPNGCF(JRG,CURIYR) = UCAPNGCF(JRG,CURIYR) + 0.001 * WC_SUM / (1.0 - CCSADJ)
                              UCAPNGCF(MNUMNR,CURIYR) = UCAPNGCF(MNUMNR,CURIYR) + 0.001 * WC_SUM / (1.0 - CCSADJ)                          
                          ENDIF
                     ELSE
                        IF (W_SYR .GT. (UPSTYR - 1)) THEN
                              ECSCAP(IEFD,IVIN3,IOWN) = ECSCAP(IEFD,IVIN3,IOWN) + WC_SUM / (1.0 - CCSADJ)
                            IF (IECP .EQ. WING .AND. W_FSHR(2) .GT. 0.0) THEN    !add to variable tracking coal/gas cofiring subset of WING
                                UCAPNGCF(JRG,CURIYR) = UCAPNGCF(JRG,CURIYR) + 0.001 * WC_SUM / (1.0 - CCSADJ)
                                UCAPNGCF(MNUMNR,CURIYR) = UCAPNGCF(MNUMNR,CURIYR) + 0.001 * WC_SUM / (1.0 - CCSADJ)                          
                            ENDIF
                              IF (IECP .EQ. WIIS) THEN
                                 WRITE(18,7361) CURIRUN, CURCALYR, JRG, W_IGRP, IOWN, IECP, IEFD, IVIN3, WC_SUM, CCSADJ, ECSCAP(IEFD,IVIN3,IOWN)
 7361                            FORMAT(1X,"UDAT_ECSCAP_INFO",8(",",I5),3(",",F21.6))
                              END IF
                        END IF
                     END IF
!
!                    Add to FTAB capacity regional mapping variables
!
                     IYR2 = IYR - USYEAR(1) + 1
                     IF (IOWN .EQ. 1 .OR. IOWN .EQ. 2) THEN
                        CAPRPT = WC_SUM * .001 / (1.0 - CCSADJ)
                        UCAPINR(PHRGN,IYR2) = UCAPINR(PHRGN,IYR2) + CAPRPT
                        UCAPINR(MNUMNR,IYR2) = UCAPINR(MNUMNR,IYR2) + CAPRPT
                        IF (OWRGN .NE. PHRGN) THEN
                           UCAPOTR(OWRGN,IYR2) = UCAPOTR(OWRGN,IYR2) + CAPRPT
                           UCAPOTR(MNUMNR,IYR2) = UCAPOTR(MNUMNR,IYR2) + CAPRPT
                           UCAPSRV(PHRGN,IYR2) = UCAPSRV(PHRGN,IYR2) + CAPRPT
                           UCAPSRV(MNUMNR,IYR2) = UCAPSRV(MNUMNR,IYR2) + CAPRPT
                        END IF
!
                        IF (W_SYR .GT. (UPSTYR - 1))THEN
                           IF (IVIN3 .EQ. 2) THEN                    !planned additions
                              UADDINR(1,PHRGN,IYR2) = UADDINR(1,PHRGN,IYR2) + CAPRPT
                              UADDINR(1,MNUMNR,IYR2) = UADDINR(1,MNUMNR,IYR2) + CAPRPT
                              IF (OWRGN .NE. PHRGN) THEN
                                 UADDOTR(1,OWRGN,IYR2) = UADDOTR(1,OWRGN,IYR2) + CAPRPT
                                 UADDOTR(1,MNUMNR,IYR2) = UADDOTR(1,MNUMNR,IYR2) + CAPRPT
                                 UADDSRV(1,PHRGN,IYR2) = UADDSRV(1,PHRGN,IYR2) + CAPRPT
                                 UADDSRV(1,MNUMNR,IYR2) = UADDSRV(1,MNUMNR,IYR2) + CAPRPT
                              ENDIF
                           ENDIF
!
                           IF (IVIN3 .EQ. 3) THEN                  !unplanned additions
                              UADDINR(2,PHRGN,IYR2) = UADDINR(2,PHRGN,IYR2) + CAPRPT
                              UADDINR(2,MNUMNR,IYR2) = UADDINR(2,MNUMNR,IYR2) + CAPRPT
                              IF (OWRGN .NE. PHRGN) THEN
                                 UADDOTR(2,OWRGN,IYR2) = UADDOTR(2,OWRGN,IYR2) + CAPRPT
                                 UADDOTR(2,MNUMNR,IYR2) = UADDOTR(2,MNUMNR,IYR2) + CAPRPT
                                 UADDSRV(2,PHRGN,IYR2) = UADDSRV(2,PHRGN,IYR2) + CAPRPT
                                 UADDSRV(2,MNUMNR,IYR2) = UADDSRV(2,MNUMNR,IYR2) + CAPRPT
                              ENDIF
                           ENDIF
                        END IF
                     ENDIF
                  ELSEIF (IEFD .LE. ( EFD_D_DSP + EFD_D_RNW ) ) THEN     ! Renewables
                   IF (IECP .NE. WIP2) THEN
                     IF (IVIN3 .EQ. 1) THEN
                        EHSCAP(IEFD - EFD_D_DSP,IVIN3,IOWN) = EHSCAP(IEFD - EFD_D_DSP,IVIN3,IOWN) + WC_SUM
                     ELSE
                        IF (W_SYR .GT. (UPSTYR - 1))THEN
                           EHSCAP(IEFD - EFD_D_DSP,IVIN3,IOWN) = EHSCAP(IEFD - EFD_D_DSP,IVIN3,IOWN) + WC_SUM
                        END IF
                     END IF
                   ELSE
                     IF (IVIN3 .EQ. 1) THEN
                        EHSCAPP2(IVIN3) = EHSCAPP2(IVIN3) + WC_SUM
                     ELSE
                        IF (W_SYR .GT. (UPSTYR - 1))THEN
                           EHSCAPP2(IVIN3) = EHSCAPP2(IVIN3) + WC_SUM
                        END IF
                     END IF
                   ENDIF
                  ELSE                                  !Distributed Generation
                     IF (IVIN3 .EQ. 1)THEN
                        EDSCAP(IEFD - EFD_D_DSP - EFD_D_RNW,IVIN3,IOWN) = &
                           EDSCAP(IEFD - EFD_D_DSP - EFD_D_RNW,IVIN3,IOWN) + WC_SUM
                     ELSE
                        IF (W_SYR .GT. (UPSTYR - 1))THEN
                           EDSCAP(IEFD - EFD_D_DSP - EFD_D_RNW,IVIN3,IOWN) = &
                              EDSCAP(IEFD - EFD_D_DSP - EFD_D_RNW,IVIN3,IOWN) + WC_SUM
                        END IF
                     END IF
                  END IF
               END IF
!
!              UPDATE CUMULATIVE ADDITIONS
!
               IF ( (WVIN .EQ. 2) .OR. (WVIN .EQ. 3) .OR. (WVIN .GT. 9)) THEN
                 IF (WVIN .NE. 15) THEN
                  IF (W_SYR .GT. (UPSTYR - 1) .AND. SYRMO .LE. IYRMO)THEN
                     IF (IEFD .LE. EFD_D_DSP) THEN
!
                        ECSADD(IEFD,IVIN3,IOWN) = ECSADD(IEFD,IVIN3,IOWN) + WC_SUM / (1.0 - CCSADJ)
                     ELSEIF (IEFD .LE. ( EFD_D_DSP + EFD_D_RNW ) ) THEN     ! Renewables
                        IF (IECP .NE. WIP2) THEN
                           EHSADD(IEFD - EFD_D_DSP,IVIN3,IOWN) = EHSADD(IEFD - EFD_D_DSP,IVIN3,IOWN) + WC_SUM
                        ELSE
                           EHSADDP2(IVIN3) = EHSADDP2(IVIN3) + WC_SUM
                        END IF
                     ELSE
                        EDSADD(IEFD - EFD_D_DSP - EFD_D_RNW,IVIN3,IOWN) = &
                           EDSADD(IEFD - EFD_D_DSP - EFD_D_RNW,IVIN3,IOWN) + WC_SUM
                     END IF
                  END IF
                 ENDIF
               END IF

!
!              UPDATE CUMULATIVE RETIREMENTS
!
               JTEST = 1
               IF ( (WVIN .EQ. 0) .OR. (WVIN .EQ. 6) .OR. (WVIN .EQ. 8)) JTEST = 0
               IF ((JTEST .EQ. 1) .AND. (RYRMO .LT. IYRMO)) THEN
                  EYRETR = EYRETR + WC_SUM
                  IF (INT(W_RYR +W_RMO/12.0) .GT. (UPSTYR - 1)) THEN
                     IF (IEFD .LE. EFD_D_DSP) THEN
                        ECSRET(IEFD,IOWN) = ECSRET(IEFD,IOWN) + WC_SUM

!                       WRITE(6,8732) CURIRUN, CURIYR+1989, CURITR, W_IGRP, W_GRP, W_GRP2, &
!                          W_MRUN, INT(W_RYR + W_RMO / 12.0), (UPSTYR - 1), W_SYR, W_SMO, W_RYR, W_RMO, &
!                          IEFD, IOWN, WVIN, IECP, JRG, WC_SUM, WHRATE, ECSRET(IEFD,IOWN)
!8732                   FORMAT(1X,"UDAT_ECSRET",18(":",I4),3(":",F12.3))

                       IF ((CURIYR+1989).EQ.UPSTYR.AND.CURITR.GT.1 .OR. (CURIYR+1989).GT.UPSTYR ) THEN
                         IF (IEFD .LE. 3) THEN
                            R_PLTS = R_PLTS+1
                            R_RY(R_PLTS) = W_RYR
                            R_IGRP(R_PLTS) = W_IGRP

                            WRITE(18,8934)'coal retire debugd ',CURIYR,CURITR,R_PLTS,R_RY(R_PLTS),R_IGRP(R_PLTS),WC_SUM
 8934                         FORMAT(A25,1x,3(I4,1x),2(F10.0,1x),F12.3)
                         ENDIF
                       ENDIF
                     ELSEIF ( IEFD .LE. (EFD_D_DSP + EFD_D_RNW) ) THEN     ! Renewables
                        EHSRET(IEFD - EFD_D_DSP,IOWN) = EHSRET(IEFD - EFD_D_DSP,IOWN) + WC_SUM
                     ELSE                                 !Distributed Generation
                        EDSRET(IEFD - EFD_D_DSP - EFD_D_RNW,IOWN) = EDSRET(IEFD - EFD_D_DSP - EFD_D_RNW,IOWN) + WC_SUM
                     END IF
                  END IF
               END IF
!
               OTYPE = WFOWN
               IF (FULLCYR .EQ. UESTYR .AND. OTYPE .LE. 2 .AND.  W_SYR .LE. EFPSYR) THEN
                  FTYPE = WEFPT
                  VINTAGE = MIN (EFP_D_VIN , EFPSYR - W_SYR+1)
                  EOMW(VINTAGE,FTYPE,OTYPE,JRG) = EOMW(VINTAGE,FTYPE,OTYPE,JRG) + WC_SUM
               END IF
!
!              Determine Boiler Type
!
               IBTP = 0
!              DO JBTP = 1 , NUM_BTP
!                 IF ((W_BTP .EQ. BTP_FTCD(JBTP)) .AND. (W_BTM .EQ. BTP_BTCD(JBTP))) IBTP = JBTP
!              END DO
!              IF (IBTP .EQ. 0) THEN
!                 IBTP = NUM_BTP
!              END IF

!              First Time Through Initialize FRG_EMM_MAP

               IF (FULLCYR .EQ. UESTYR .AND. W_RYR .GE. UESTYR .AND. WFOWN .LE. 4 .AND. WNOWN .LE. UNRGNS) THEN
!                 FRG = EPNFLRG(W_CR,W_CLRG,W_GR)
                  FRG = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)
                  IF (FRG .EQ. 0) THEN

                     WRITE(6,3791) CURIRUN, CURIYR+1989, CURITR, W_IGRP, W_GRP, WNOWN, WNOPER, W_CR, W_CLRG, W_GR, W_CAR, WECPT, WEFDT
 3791                FORMAT(1X,"FRG_OOPS",15(":",I5))

                     FRG = 1
                  END IF
                  FRG_EMM_MAP(WNOWN,FRG) = FRG_EMM_MAP(WNOWN,FRG) + WC_SUM * 0.001

!                 IF EXISTING CAPACITY FOR PLANT TYPES DESIGNATED AS NEW AND THE EMM REGION AND FUEL REGION ARE NOT ALLOWED, ASSIGN A HIGH COST CONNECTION COST.
!                 THIS ALLOWS FOR THE EXISTING UNITS BUT IS ECONOMICALLY PROHIBITIVE TO BUILD NEW UNITS IN THE EMM-FUEL REGION COMBINATION

                  IF (UPVTYP(IECP) .EQ. 1 .AND. FRG_EMM_MAP(WNOWN,FRG) .GT. 0.0 .AND. (FL_CNXT_CST(WNOWN,FRG) .LE. 0.0 .OR. FL_CNXT_CST(WNOWN,FRG) .GT. 200.0)) THEN
                     IF (IECP .LT. WICN) THEN


                        WRITE(18,4791) CURIRUN, CURIYR+1989, CURITR, W_IGRP, W_GRP, WNOWN, FRG, WNOPER, W_CR, W_CLRG, W_GR, W_CAR, WECPT, WEFDT, WC_SUM, FL_CNXT_CST(WNOWN,FRG), URGNME(WNOWN)(1:4), FLRGNAME(FRG)(1:20)
 4791                   FORMAT(1X,"FL_CNXT_CST_OOPS",14(":",I5),2(":",F21.6),":",A4,":",A20)

                        FL_CNXT_CST(WNOWN,FRG) = 2000.0

                     END IF
                  END IF
               END IF
!
!
!              First Time Through Determine Initial Configuration, Owner Region Shares for all Existing and Planned Coal and Assign Coal Unit Index to Plant Record
!
               I_COAL = 0
               IF (UPTTYP(IECP) .LE. NW_COAL .AND. FULLCYR .EQ. UESTYR .AND. W_RYR .GE. UESTYR) THEN
!
!                 First insure that record is compliant with NOX CAA Regulations
!
!                 CALL NOX_CAA(IBTP,IRECL,IRG)
!
                  IF (UPTTYP(IECP) .GT. EX_COAL) THEN
                     WPART = "0"
                     WSCBT = "W"
                     W_COMB = 1
                     W_POST = 2
                     IF (UPPCEF(IECP) .GT. 0.0) THEN
                       W_ACI = "S"
                     ELSE
                       W_ACI = "N"
                     ENDIF
                  END IF
!
                  IF (UCL_CGRP(IRECL) .GT. 0) THEN
                     I_COAL = UCL_CGRP(IRECL)
                  ELSE
                     I_COAL = MAP_TO_COAL_ID(W_IGRP)
                     IF (I_COAL .LE. 0) WRITE(6,2392) CURIYR+UHBSYR,W_IGRP,W_GRP,IRECL,W_SYR,W_RYR,WVIN,I_COAL
 2392                FORMAT(1X,"UNIDENTIFIED_COAL_UNIT",8(":",I5))
                     UCL_CGRP(IRECL) = I_COAL
                     UCL_CGRP2(W_GRP) = I_COAL
                  END IF
                  UCL_CGRP2(W_GRP) = I_COAL
                  CRG = W_CLRG
                  EMM_CL_CLRG(I_COAL) = W_CLRG
                  DO I_CNFG = 1 , NUM_CNFG
                     IF (UPTTYP(IECP) .EQ. UPTTYP(UCL_ECP(I_CNFG))) THEN
                        ITST = 0
                        IF (WPART .EQ. "B" .AND. UCL_CNFG(2,I_CNFG) .EQ. 1 .OR. &
                            WPART .EQ. "E" .AND. UCL_CNFG(2,I_CNFG) .EQ. 2 .OR. &
                            WPART .EQ. "N" .AND. UCL_CNFG(2,I_CNFG) .EQ. 3 .OR. &
                            WPART .EQ. "0" .AND. UCL_CNFG(2,I_CNFG) .EQ. 4) ITST = ITST + 1
                        IF (WSCBT .EQ. "W" .AND. UCL_CNFG(3,I_CNFG) .EQ. 1 .OR. &
                            WSCBT .EQ. "D" .AND. UCL_CNFG(3,I_CNFG) .EQ. 2 .OR. &
                            WSCBT .EQ. "N" .AND. UCL_CNFG(3,I_CNFG) .EQ. 0) ITST = ITST + 1
                        IF (W_COMB .GT. 0 .AND. UCL_CNFG(4,I_CNFG) .EQ. 1 .OR. &
                            W_COMB .EQ. 0 .AND. UCL_CNFG(4,I_CNFG) .EQ. 0) ITST = ITST + 1
                        IF (W_POST .EQ. 1 .AND. UCL_CNFG(5,I_CNFG) .EQ. 1 .OR. &
                            W_POST .EQ. 3 .AND. UCL_CNFG(5,I_CNFG) .EQ. 1 .OR. &
                            W_POST .EQ. 5 .AND. UCL_CNFG(5,I_CNFG) .EQ. 1 .OR. &
                            W_POST .EQ. 0 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                            W_POST .EQ. 2 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                            W_POST .EQ. 4 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                            W_POST .EQ. 6 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                            W_POST .EQ. 7 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0) ITST = ITST + 1
                        IF (W_POST .EQ. 2 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                            W_POST .EQ. 4 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                            W_POST .EQ. 6 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                            W_POST .EQ. 7 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                            W_POST .EQ. 0 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0 .OR. &
                            W_POST .EQ. 1 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0 .OR. &
                            W_POST .EQ. 3 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0 .OR. &
                            W_POST .EQ. 5 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0) ITST = ITST + 1
                        IF (W_ACI .EQ. "N" .AND. UCL_CNFG(7,I_CNFG) .EQ. 0 .OR. &
                            W_ACI .EQ. "A" .AND. UCL_CNFG(7,I_CNFG) .EQ. 0 .OR. &
                            W_ACI .EQ. "S" .AND. UCL_CNFG(7,I_CNFG) .EQ. 1 .OR. &
                            W_ACI .EQ. "F" .AND. UCL_CNFG(7,I_CNFG) .EQ. 0 .OR. &
                            W_ACI .EQ. "B" .AND. UCL_CNFG(7,I_CNFG) .EQ. 1) ITST = ITST + 1
                        IF (W_ACI .EQ. "N" .AND. UCL_CNFG(8,I_CNFG) .EQ. 0 .OR. &
                            W_ACI .EQ. "A" .AND. UCL_CNFG(8,I_CNFG) .EQ. 0 .OR. &
                            W_ACI .EQ. "S" .AND. UCL_CNFG(8,I_CNFG) .EQ. 0 .OR. &
                            W_ACI .EQ. "F" .AND. UCL_CNFG(8,I_CNFG) .EQ. 1 .OR. &
                            W_ACI .EQ. "B" .AND. UCL_CNFG(8,I_CNFG) .EQ. 1) ITST = ITST + 1
                        IF (ITST .EQ. 7) THEN
                           IF (ECL_ICFG(I_COAL) .EQ. 0 .OR. ECL_ICFG(I_COAL) .GT. I_CNFG) THEN
                              ECL_ICFG(I_COAL) = I_CNFG
                              DO XYR = 1 , UNYEAR
                                 TYYYYMM = 100 * (XYR + UHBSYR) + 12
                                 SYYYYMM = 100 * W_SYR + W_SMO
                                 RYYYYMM = 100 * W_RYR + W_RMO
                                 IF (SYYYYMM .LT. TYYYYMM .AND. RYYYYMM .GE. TYYYYMM) THEN
                                    IF (IECP .NE. UCL_ECP(I_CNFG) .OR. (TMP_RCFG(XYR,I_COAL) .NE. I_CNFG .AND. TMP_RCFG(XYR,I_COAL) .GT. 0)) THEN
                                       WRITE(18,9317) CURIYR+UHBSYR,JRG,W_GRP,IRECL,I_COAL,I_CNFG,ECL_ICFG(I_COAL),IECP,UCL_ECP(I_CNFG)
 9317                                  FORMAT(1X,"CNFG_OOPS",9(":",I5))
!                                      Label:CNFG_OOPS:CYEAR:JRG:W_GRP:IRECL:I_COAL:I_CNFG:ECL_ICFG:IECP:UCL_ECP
                                    END IF
                                    TMP_RCFG(XYR,I_COAL) = I_CNFG
                                 END IF
                              END DO
                           END IF
                        END IF
!
                        IF (ITST .EQ. 7) WRITE(18,9316) CURIYR+UHBSYR,I_COAL,I_CNFG,ITST,WPART,WSCBT,W_COMB,W_POST,W_ACI, &
                           (UCL_CNFG(I_ROPT,I_CNFG),I_ROPT=1,7),ECL_ICFG(I_COAL),IECP,UCL_ECP(I_CNFG),W_SYR,W_RYR
 9316                   FORMAT(1X,"ICNFG",4(":",I4),2(":",A1),2(":",I2),":",A1,7(":",I2),5(":",I4))
!                       Label:ICNFG:CYEAR:I_COAL:I_CNFG:ITST:WPART:WSCBT:W_COMB:W_POST:W_ACI:UCL_CNFG_1:UCL_CNFG_2:UCL_CNFG_3:UCL_CNFG_4:UCL_CNFG_5:UCL_CNFG_6:UCL_CNFG_7:ECL_ICFG:IECP:UCL_ECP:SYEAR:RYEAR
!
                        IF (ITST .EQ. 7 .AND. ECL_ICFG(I_COAL) .NE. I_CNFG .AND. ECL_ICFG(I_COAL) .GT. 0) THEN
                           WRITE(18,9327) CURIYR+UHBSYR,JRG,W_GRP,IRECL,I_COAL,I_CNFG,ECL_ICFG(I_COAL),IECP,UCL_ECP(I_CNFG)
                        END IF
 9327                   FORMAT(1X,"CNFG_MULTIPLE_RECORDS",9(":",I5))
                     END IF
                  END DO
!
                  DO XYR = 1 , UNYEAR
                     BYYYYMM = 100 * (XYR+ UHBSYR)
                     TYYYYMM = 100 * (XYR + UHBSYR) + 12
                     SYYYYMM = 100 * W_SYR + W_SMO
                     RYYYYMM = 100 * W_RYR + W_RMO
                     IF (SYYYYMM .LT. TYYYYMM .AND. RYYYYMM .GE. BYYYYMM) THEN
                        T_RG_CAP(XYR,IRG,I_COAL) = T_RG_CAP(XYR,IRG,I_COAL) + WC_SUM
                        EMM_CL_ECPt(I_COAL,XYR) = WECPT
                        MIN_ECPt(I_COAL) = MIN( MIN_ECPt(I_COAL) , WECPT)
                     ELSEIF (RYYYYMM .LT. BYYYYMM .AND. (WVIN .EQ. 1 .OR. WVIN .EQ. 7)) THEN
                        EMM_CL_ECPt(I_COAL,XYR) = 99
!                        WRITE(6,*) 'Set EMM_CL_ECPt to 99 ',W_IGRP,XYR,RYYYYMM,BYYYYMM,WVIN
                     END IF
                  END DO
                  WRITE(18,9315) CURIYR+UHBSYR,I_COAL,W_IGRP,W_GRP,W_GRP2,ECL_ICFG(I_COAL),WPART,WSCBT,W_COMB,W_POST,W_ACI, &
                     WC_SUM,WVIN,WNOPER,WNOWN,WFOWN,W_SYR,W_RYR,MIN_ECPt(I_COAL),(EMM_CL_ECPt(I_COAL,XYR),XYR=CURIYR,UNYEAR)
 9315             FORMAT(1X,"ECL_ICFG",6(":",I5),2(":",A1),2(":",I2),":",A1,":",F9.3,6(":",I4),37(":",I2))
                  WRITE(18,9325) CURIYR+UHBSYR,I_COAL,W_IGRP,W_GRP,W_GRP2,ECL_ICFG(I_COAL),WPART,WSCBT,W_COMB,W_POST,W_ACI, &
                     WC_SUM,WVIN,WNOPER,WNOWN,WFOWN,W_SYR,W_RYR,MIN_ECPt(I_COAL),(TMP_RCFG(XYR,I_COAL),XYR=CURIYR,UNYEAR)
 9325             FORMAT(1X,"ECL_RCFG",6(":",I5),2(":",A1),2(":",I2),":",A1,":",F9.3,6(":",I4),37(":",I2))
               END IF
!
!              IF PLANT IS IN OPERATION IN CURRENT YEAR
!
               IF ((IYR .GE. W_SYR) .AND. (IYR .LE. W_RYR)) THEN
                  SYRMO = W_SYR * 100 + W_SMO - MONTHS
                  RYRMO = W_RYR * 100 + W_RMO
                  syrmo1 = IYR * 100
                  syrmo2 = IYR * 100 + MONTHS
!
                  if ((WVIN .EQ. 6 .OR. WVIN .EQ. 8) .AND. (RYRMO .GE. syrmo1 .AND. RYRMO .LT. syrmo2)) goto 2000
!
                  ITEST = ITEST + 1                   ! COUNTER OF CURRENT OP PLANTS
!
!                 ACCUMULATE EWG CAPACITY COSTS
!
                  IF (IOWN .GE. 3) THEN
                      TMP_EWGFIX = WPCST * WC_NP * 0.001 * (DBLE(1.0) / UPGNPD(CURIYR))
                      IF (ISNAN(TMP_EWGFIX)) THEN
                         WRITE(6,8335) CURIRUN, CURIYR+1989, CURITR, I_COAL, W_IGRP, W_GRP, W_GRP2, WECPT, WC_SUM, &
                            TMP_EWGFIX, EWGFIX, WPCST, WC_NP, UPGNPD(CURIYR)
 8335                    FORMAT(1X,"BAD_TMP_EWGFIX",8(":",I6),6(":",F21.6))

                         TMP_EWGFIX = 0.0
                      END IF
                      EWGFIX = EWGFIX + TMP_EWGFIX

!                     write(6,9335) 'Add to EWGFIX ',CURIYR,W_IGRP,IECP,W_SYR,WRFURB,IOWN,WC_SUM,WPCST
!9335                 FORMAT(1X,a15,6I5,2f10.2)

                  ENDIF
!
                  CAP = 0.0
                  DO ISP = 1 , EENSP
                     SCAP(ISP) = 0.0
                     IYRMO = IYR * 100 + ISP * MONTHS - MONTHS
                     IYRMONX = IYR * 100 + ISP * MONTHS
                     IF ((SYRMO .LE. IYRMO) .AND. (RYRMO .GE. IYRMO)) THEN
                        SCAP(ISP) = WC_SUM * WSEAS(ISP) + WC_WIN * (1.0 - WSEAS(ISP))
                     END IF
                     IF ((WVIN .EQ. 6  .OR.  WVIN .EQ. 8) .AND.  &
                         (RYRMO .GE. IYRMO .AND. RYRMO .LT. IYRMONX)) THEN
                       SCAP(ISP) = 0.0
                     ENDIF
                     CAP = CAP + SCAP(ISP)
                  END DO

                  IF (IECP .EQ. WIHY) THEN
                     DO IMO = 1 , 12
                        T1(IRG,IMO) = T1(IRG,IMO) + SUMMER_MONTHS(IMO) * WC_SUM + (1.0 - SUMMER_MONTHS(IMO)) * WC_WIN

!                       URHYCFA(CURIYR) has not been defined yet, it has been initilized to 1.0 instead, so remove here and apply it after it has been defined

                        IF (USW_HYD .GT. 0) THEN

!                          T2(IRG,IMO) = T2(IRG,IMO) + (SUMMER_MONTHS(IMO) * WC_SUM + (1.0 - SUMMER_MONTHS(IMO)) * WC_WIN) * WCF_M(IMO) * UQHYFAC(CURIYR,IRG) * URHYCFA(CURIYR) * (1.0 / (1.0 - WFOR(IEFD)))

                           T2(IRG,IMO) = T2(IRG,IMO) + (SUMMER_MONTHS(IMO) * WC_SUM + (1.0 - SUMMER_MONTHS(IMO)) * WC_WIN) * WCF_M(IMO) * UQHYFAC(CURIYR,IRG) * (1.0 / (1.0 - WFOR(IEFD)))

!                          T2(IRG,IMO) = T2(IRG,IMO) + (SUMMER_MONTHS(IMO) * WC_SUM + (1.0 - SUMMER_MONTHS(IMO)) * WC_WIN) * WCF_M(IMO) * UQHYFAC(CURIYR,IRG)
                        ELSE

!                          T2(IRG,IMO) = T2(IRG,IMO) + (SUMMER_MONTHS(IMO) * WC_SUM + (1.0 - SUMMER_MONTHS(IMO)) * WC_WIN) * WCF_M(IMO) * URHYCFA(CURIYR) * (1.0 / (1.0 - WFOR(IEFD)))

                           T2(IRG,IMO) = T2(IRG,IMO) + (SUMMER_MONTHS(IMO) * WC_SUM + (1.0 - SUMMER_MONTHS(IMO)) * WC_WIN) * WCF_M(IMO) * (1.0 / (1.0 - WFOR(IEFD)))

!                          T2(IRG,IMO) = T2(IRG,IMO) + (SUMMER_MONTHS(IMO) * WC_SUM + (1.0 - SUMMER_MONTHS(IMO)) * WC_WIN) * WCF_M(IMO)
                        END IF

                        IF (CURIYR+1989 .EQ. 2017) &
                           WRITE(18,4104) CURIRUN, CURIYR+1989, IRG, IMO, IEFD, W_IGRP, USW_HYD, T2(IRG,IMO), T1(IRG,IMO), SUMMER_MONTHS(IMO), WC_SUM, WC_WIN, WCF_M(IMO), WFOR(IEFD), URHYCFA(CURIYR), UQHYFAC(CURIYR,IRG)
 4104                   FORMAT(1X,"HY_CF_DETAIL",7(",",I5),9(",",F12.4))

                     ENDDO
                  END IF
!
                  CAP = CAP / REAL(EENSP)
                  GCAP = GCAP + CAP
                  AVGCF = AVGCF + W_CFA * CAP
                  DO ISP = 1 , EENSP
                     GSCAP(ISP) = GSCAP(ISP) + SCAP(ISP)
                  END DO

                  DO ISP = 1 , EENSP
                     DO IMO = 1 , 12
                        CFCAP(ISP) = CFCAP(ISP) + SCAP(ISP) * (MOTOSP(ISP,IMO) / MOTOSP(0,IMO))
                        CAPCF(ISP) = CAPCF(ISP) + SCAP(ISP) * WCF_M(IMO) * (MOTOSP(ISP,IMO) / MOTOSP(0,IMO))
                     END DO
                  END DO
!
!                 FOR COAL PLANTS, GET SHARE OF FUEL REGION USE BY COAL REGION TO SPLIT CTL
!
                  IF (UPTTYP(IECP) .LE. NW_COAL)THEN
                     CLCONFC(W_CLRG,FRG) = CLCONFC(W_CLRG,FRG) + CAP * W_CFA * 8.760 * WHRATEA
                     CLCONFC(W_CLRG,MAXNFR + 1) = CLCONFC(W_CLRG,MAXNFR + 1) + CAP * W_CFA * 8.760 * WHRATEA
                  END IF

                  DO IFL = 1 , UIFPLT
                     FL_NUM = WFL(IFL)
                     IF (FL_NUM .GT. 0) THEN
                        FSHR = W_FSHR(IFL)
!
!                       CALIBRATE OIL SHARE IN DUAL-FIRED PLANTS
!
!
                        IF (IEFD .EQ. UISTX .AND. FL_NUM .EQ. UIRL) THEN
                          IF (UNRGNS.EQ.13) THEN
                           IF ((IRG.NE.2) .and. (CURIYR .LE. 13)) &
                              FSHR = MIN(FSHR + DBLE(0.25),DBLE(1.0))
                           if (IRG .eq. 13) FSHR = 0.01
                          ELSEIF (UNRGNS.EQ.22) THEN
                           IF ((IRG.NE.1) .and. (CURIYR .LE. 13)) &
                              FSHR = MIN(FSHR + DBLE(0.25),DBLE(1.0))
                           if (IRG .eq. 20) FSHR = 0.01
                          ELSEIF (UNRGNS.EQ.25) THEN
                           IF ((IRG.NE.1) .and. (CURIYR .LE. 13)) &   ! DGR - will need to change the region index later
                              FSHR = MIN(FSHR + DBLE(0.25),DBLE(1.0))
                           if (IRG .eq. 20) FSHR = 0.01
                          ENDIF
                        END IF
!                       LIMIT DISTILLATE USE IN DUAL-FIRED PLANTS
                        IF (IEFD .EQ. UISTX .AND. FL_NUM .EQ. UIDS) THEN
                            FSHR = 0.05
                        ENDIF
!
!                       CALIBRATE GAS SHARE IN DUAL-FIRED PLANTS
!
                        IF (IEFD .EQ. UISTX .AND. FL_NUM .EQ. UIGC) THEN
                           if (curiyr .le. 12) then
                              FSHR = FSHR * DBLE(0.70)
                           elseif (curiyr .eq. 13) then
                              FSHR = FSHR * DBLE(0.60)
                           endif
                        END IF
!
!                       BIOMASS COFIRING, PHASE IN
!
                        IF ((IEFD.NE.UIBMS .AND. IEFD.NE.UIBIG) .AND.FL_NUM.EQ.UIWD) THEN ! Biomass fuel but not biomass plant (WD or BECCS)
!
!                          OVERWRITE MAX COFIRING SHARE, IF SWITCH IS SET
!
                           IF (USW_CFSH .GT. 0) THEN
                              FSHR = URWDCFST(IRG)
                              IF ((CURIYR + UHBSYR) .GE. UYR_CFRG)FSHR = URWDCFRG(IRG)
                           END IF
!
!                          STORE FOR PLANNING DECISIONS
!
                           DO YEAR = 1 , ECP_D_XPH
                              IF ((IYR + YEAR - 1) .LT. UPSTYR) THEN
                                 IF ((IYR + YEAR - 1) .GT. 1998) THEN
                                    WDFSHR = FSHR * (FLOAT((IYR + YEAR - 1) - 1998)) /  (FLOAT(UPSTYR - 1998))
                                 ELSE
                                    WDFSHR = DBLE(0.0)
                                 END IF
                              ELSE
                                 WDFSHR = FSHR
                              END IF
                              IF ((CURIYR + UHBSYR) .LE. UESTYR .AND. IECP .LE. ECP_D_DSP .AND. (IYR + YEAR - 1) .LT. 9999) THEN
                               IF (YEAR .EQ. 1 .AND. W_CFA .GT. 0.0)THEN
                                 WDSUM(IECP,YEAR,W_CLRG) = WDSUM(IECP,YEAR,W_CLRG) + WC_SUM
                                 WDSHR(IECP,YEAR,W_CLRG) = WDSHR(IECP,YEAR,W_CLRG) + WC_SUM * WDFSHR
                                 TOTGENPC(IECP,W_CLRG) = TOTGENPC(IECP,W_CLRG) + WC_SUM * W_CFA * 8.760
                                 TOTGENPN(IECP,IRG) = TOTGENPN(IECP,IRG) + WC_SUM * W_CFA * 8.760

!                                CFSHR = MIN(W_FSHR(IFL),0.001)
!                                CFSHR = W_FSHR(IFL)
!                                COFGENPC(IECP,W_CLRG) = COFGENPC(IECP,W_CLRG) + WC_SUM * W_CFA * CFSHR * 8.760
!                                COFGENPN(IECP,IRG) = COFGENPN(IECP,IRG) + WC_SUM * W_CFA * CFSHR * 8.760

                                 COFGENPC(IECP,W_CLRG) = COFGENPC(IECP,W_CLRG) + WC_SUM * W_CFA * WDFSHR* 8.760
                                 COFGENPN(IECP,IRG) = COFGENPN(IECP,IRG) + WC_SUM * W_CFA * WDFSHR* 8.760

!                                write(6,4321) curiyr+1989,irg,wc_sum,w_cfa,w_fshr(ifl),wc_sum*w_cfa*wdfshr*8.76
!4321                            format(1h ,'!cfgen',i4,i3,4f10.3)
                               END IF
                              END IF
                           END DO
!
!                          STORE FOR DISPATCHING DECISIONS
!
                           IF (IYR .LT. UPSTYR .OR. FULLCYR .EQ. UESTYR) THEN
                              IF (IYR .GT. 1998) THEN
                                 FSHR = FSHR * (FLOAT(IYR - 1998)) /  (FLOAT(UPSTYR - 1998))
                              ELSE
                                 FSHR = DBLE(0.0)
                              END IF
                           END IF
                        END IF
                        MXFSHR(FL_NUM) = MXFSHR(FL_NUM) + FSHR * CAP
                     END IF
                  END DO
                  CAPHR = CAPHR + CAP * WHRATEA
                  CAPVOM = CAPVOM + CAP * WVOMA
                  CAPFOM = CAPFOM + CAP * WFOMA
                  CAPXTR = CAPXTR + CAP * UPXTRA(AGE,IECP)
!
!                 calculate PTC subsidy if appropriate
!
!                 IF ((WVIN .EQ. 2 .OR. WVIN .EQ.3 .OR. WVIN .GT. 9) .AND. WVIN .NE. 15) THEN
                  IF (WVIN .NE. 15) THEN
                   IF (UPGSUBPT(IECP) .GT. 0)THEN
                      IF (IYR .LE. (W_SYR + UPGSYR(IECP))) THEN
                         GYR = W_SYR - UHBSYR
                         GSUB = UPGSUBYR(IECP,GYR)
                         CAPGSUB = CAPGSUB + CAP * GSUB
                      ENDIF
                   ELSE IF (UPGSUB(IECP) .GT. 0.0) THEN
                     IF (W_SYR .GE. UPGSY1(IECP) .AND. W_SYR .LE. UPGSYL(IECP) .AND. IECP .NE. WICN ) THEN
                        IF (IYR .LE. (W_SYR + UPGSYR(IECP))) THEN
                           GCFC = UPMCF(IECP)
                           IF (UCPRNWIS(IECP) .GT. 0) GCFC = EPRCFC(IECP - ECP_D_DSP)
                           IF (UCPINTIS(IECP) .GT. 0) GCFC = EPIACF(IECP - (ECP_D_DSP + ECP_D_RNW + ECP_D_STO))
                           IF (IECP .NE. WIAN) THEN
                              GSUB = UPGSUB(IECP)
                           ELSE
                              IF (CAPSUB(IECP) .LT. UPSUBLIM(IECP)) THEN
                                 GSUB = UPGSUB(IECP)
                              ELSE
                                 GSUB = UPGSUB(IECP) * UPSUBLIM(IECP) / CAPSUB(IECP)
                              END IF
                           END IF
                           SPAYYR = 1.0 * GCFC * 8.760 * GSUB

!                          check if limit on annual payment / GW

                           IF (UPGSMX(IECP) .GT. 0.0 .AND. SPAYYR .GT. UPGSMX(IECP)) GSUB = UPGSMX(IECP) / SPAYYR

!                          if nominal, convert to real

                           IF (UPGSTY(IECP) .GT. 0) GSUB = GSUB / UPGNPD(CURIYR)

                           CAPGSUB = CAPGSUB + CAP * GSUB
                        ENDIF
                     ELSEIF (IECP .EQ. WICN) THEN                   ! existing nuclear credit is given during specified years, not based on online year - should be filled in for IRA runs only
                          IF (IYR .GE. UPGSY1(IECP) .AND. IYR .LE. UPGSYL(IECP)) THEN
                            NUCWGMULT = UPGSUB(IECP) / (BSNUCCRED/UPGNPD(UPGSYD(IECP) - UHBSYR))      !determine what wage multiplier was assumed in UPGSUB input in ecpdaty.xlsx
                            IF (ULTGEN(W_GRP) .GT. 0.0) THEN          !calculate avg revenues to determine size of subsidy
                               NETNUCREV = ULENGREVS(W_GRP)/ULTGEN(W_GRP) * 1000.0 - MXNUCREV/UPGNPD(UPGSYD(IECP) - UHBSYR)
                               IF (NETNUCREV .GT. 0.0) THEN
                                 GSUB = MAX(UPGSUB(IECP)/NUCWGMULT - 0.16*NETNUCREV,0.0) * NUCWGMULT     
                               ELSE  
                                 GSUB = UPGSUB(IECP)
                               ENDIF
                             ELSE
                                GSUB = UPGSUB(IECP)
                             ENDIF
                           CAPGSUB = CAPGSUB + CAP * GSUB
                          ENDIF
                     ENDIF
                   ENDIF
                  ENDIF
!
!                 calculate CO2 sequestration subsidy if appropriate
!
                  IF (IECP .LE. ECP_D_DSP)THEN
                     IF (UPPCEF(IECP) .GT. 0.0 .AND. (WVIN .EQ. 2 .OR. WVIN .EQ.3 .OR. WVIN .GT. 9)) THEN
                        IF (IYR .LT. (W_SYR + 10)) THEN
                           IF (IECP .EQ. WIIS .OR. IECP.EQ. WIPQ)THEN
                              CAPCSUB = CAPCSUB + CAP * (WHRATEA / 1000000.0) * ECLEL(CURIYR) * UPSEQBYR(CURIYR) * UPPCEF(IECP) *  &
                                        (EMETAX(1,CURIYR) * 1000.0)
                              EMREV(9,CURIYR) = EMREV(9,CURIYR) + CAP * UPMCF(IECP) * 8.76 * (WHRATEA / 1000000.0) *  &
                                         ECLEL(CURIYR) * UPSEQBYR(CURIYR) * UPPCEF(IECP) *  &
                                         EMETAX(1,CURIYR) * UPGNPD(CURIYR) * 0.001

!                             write(6,4321) curiyr+1989,irg,CAP , UPMCF(IECP) ,WHRATEA,  &
!                                        ECLEL(CURIYR), UPSEQBYR(CURIYR) , UPPCEF(IECP) ,  &
!                                        EMETAX(1,CURIYR) * 1000,0, UPGNPD(CURIYR) ,  &
!                                        CAP * UPMCF(IECP) * 8.76 * (WHRATEA / 1000000.0) *  &
!                                        ECLEL(CURIYR) * UPSEQBYR(CURIYR) * UPPCEF(IECP) *  &
!                                        EMETAX(1,CURIYR) * UPGNPD(CURIYR) * 0.001,  &
!                                        emrev(9,curiyr)
!4321                         format(1h ,'!seqcrdi',i4,i3,12f10.3)
                           ELSE
                              CAPCSUB = CAPCSUB + CAP * (WHRATEA / 1000000.0) * ENGEL(CURIYR) * UPSEQBYR(CURIYR) * UPPCEF(IECP) *  &
                                        (EMETAX(1,CURIYR) * 1000.0)
                              EMREV(9,CURIYR) = EMREV(9,CURIYR) + CAP * UPMCF(IECP) * 8.76 * (WHRATEA / 1000000.0) *  &
                                         ENGEL(CURIYR) * UPSEQBYR(CURIYR) * UPPCEF(IECP) *  &
                                         EMETAX(1,CURIYR) * UPGNPD(CURIYR) * 0.001
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF

                  IF (IYR .GE. WSCBYR) CAPSCR = CAPSCR + CAP * WSCBEF
!
                  DO ISP = 1 , EENSP
                     TSUM_SHR = DBLE(0.0)
                     DO XNOX = 1 , NOX_GRP
                        IF (LNOX(XNOX) .GT. 0) THEN
                           IF (NOXBYGRP(XNOX,CURIYR) .GT. 0.0 .AND. NOXBYGRP(XNOX,CURIYR) .LT. 9990.0) THEN
                              TSUM_SHR = MAX(TSUM_SHR,NOX_EFD(ISP,XNOX) / HRS_EFD(ISP))
                           END IF
                           TNOX_RC(XNOX,ISP) = TNOX_RC(XNOX,ISP) + CAP * NOX_EFD(ISP,XNOX) / HRS_EFD(ISP) * WNOX_R
                        END IF
                     END DO
                     TWIN_SHR = DBLE(1.0) - TSUM_SHR
                     TNOX_R(ISP) = TNOX_R(ISP) + CAP *  (TSUM_SHR * WNOX_R + TWIN_SHR * WNOX_B4)
                  END DO
!
!                 Debug Output First Year and then if unit is new or if it is retired
!
                  IF ((UF_DBG .GT. 0) .AND. (PRTDBGE .LT. 4)) THEN
                     IF (FULLCYR .EQ. UESTYR) THEN
                        WRITE(UF_DBG,1978) CURIYR+UHBSYR,IRG,IGRP,W_IGRP,W_GRP, &
                           W_GRP2,WNOPER,WNOWN,WFOWN,W_CR,W_GR,W_CLRG,WEFDT,WECPT, &
                           CCSROV,CCSF,CCSV,WC_SUM,WC_WIN,WHRATEA,W_COMB,W_POST, &
                           WNOX_B4,WNOX_R,W_SYR,W_SMO,W_RYR,W_RMO,W_CFA,ECNTP,EHNTP, &
                           (WFL(IFL),IFL = 1 , EIFPLT),COAL
 1978                   FORMAT(1X,'CRPGRP0',":",I4,":",I3,":",I5,":",I5,":",I5, &
                           ":",I5,":",I3,":",I3,":",I1,":",I2,":",I2,":",I2, &
                           ":",I2,":",I2, &
                           ":",F7.1,":",F7.3,":",F7.3,":",F6.1,":",F6.1,":",F6.0, &
                           ":",I1,":",I1, &
                           2(":",F6.4),":",I4,":",I2,":",I4,":",I2,":",F6.3,":",I4,":",I4, &
                           3(":",I2),":",I2)
                     ELSE IF (CURIYR+UHBSYR .EQ. W_SYR) THEN
                        WRITE(UF_DBG,1979) CURIYR+UHBSYR,IRG,IGRP,W_IGRP,W_GRP, &
                           W_GRP2,WNOPER,WNOWN,WFOWN,W_CR,W_GR,W_CLRG,WEFDT,WECPT, &
                           CCSROV,CCSF,CCSV,WC_SUM,WC_WIN,WHRATEA,W_COMB,W_POST, &
                           WNOX_B4,WNOX_R,W_SYR,W_SMO,W_RYR,W_RMO,W_CFA,ECNTP,EHNTP, &
                           (WFL(IFL),IFL = 1 , EIFPLT),COAL
 1979                   FORMAT(1X,'CRPGRP+',":",I4,":",I3,":",I5,":",I5,":",I5, &
                           ":",I5,":",I3,":",I3,":",I1,":",I2,":",I2,":",I2, &
                           ":",I2,":",I2, &
                           ":",F7.1,":",F7.3,":",F7.3,":",F6.1,":",F6.1,":",F6.0, &
                           ":",I1,":",I1, &
                           2(":",F6.4),":",I4,":",I2,":",I4,":",I2,":",F6.3,":",I4,":",I4, &
                           3(":",I2),":",I2)
                     ELSE IF (CURIYR+UHBSYR .EQ. W_RYR) THEN
                        WRITE(UF_DBG,1980) CURIYR+UHBSYR,IRG,IGRP,W_IGRP,W_GRP, &
                           W_GRP2,WNOPER,WNOWN,WFOWN,W_CR,W_GR,W_CLRG,WEFDT,WECPT, &
                           CCSROV,CCSF,CCSV,WC_SUM,WC_WIN,WHRATEA,W_COMB,W_POST, &
                           WNOX_B4,WNOX_R,W_SYR,W_SMO,W_RYR,W_RMO,W_CFA,ECNTP,EHNTP, &
                           (WFL(IFL),IFL = 1 , EIFPLT),COAL
 1980                   FORMAT(1X,'CRPGRP-',":",I4,":",I3,":",I5,":",I5,":",I5, &
                           ":",I5,":",I3,":",I3,":",I1,":",I2,":",I2,":",I2, &
                           ":",I2,":",I2, &
                           ":",F7.1,":",F7.3,":",F7.3,":",F6.1,":",F6.1,":",F6.0, &
                           ":",I1,":",I1, &
                           2(":",F6.4),":",I4,":",I2,":",I4,":",I2,":",F6.3,":",I4,":",I4, &
                           3(":",I2),":",I2)
                     END IF
                  END IF
!
!                 Validate That Parameters Defining Plant Group Do Not
!                 Change For Any Plants In Group
!
                  IF (UF_DBG .GT. 0) THEN
                     IF (FREC_GP .EQ. 1) THEN
                        GIBTP = IBTP
                        GINCT = INCT
                        GW_GRP = W_GRP
                        GIEFD = IEFD
                        GIECP = IECP
                        GIVIN3 = IVIN3
                        GWFOWN = WFOWN
                        GWNOWN = WNOWN
                        GRG_NUM = RG_NUM
                        DO IFL = 1, EIFPLT
                           GWFL(IFL) = WFL(IFL)
                        END DO
                        FREC_GP = 0
                     END IF
                     MTCH_FL = 1
                     DO IFL = 1, EIFPLT
                        IF (GWFL(IFL) .NE. WFL(IFL)) MTCH_FL = 0
                     END DO
                     IF (GIEFD .NE. IEFD .OR. GIECP .NE. IECP .OR. &
                        GW_GRP .NE. W_GRP .OR. GWFOWN .NE. WFOWN .OR. &
                        GWNOWN .NE. WNOWN .OR. GRG_NUM .NE. RG_NUM .OR. &
                        MTCH_FL .EQ. 0 .OR. GIBTP .NE. IBTP .OR. &
                        GINCT .NE. INCT) THEN
                        WRITE(UF_DBG,'(A,A,/," ***CRPGRP:",52X,I8,I5,I7,I7,I8)') &
                           ' ***CRPGRP: PLNTIN DATA WARNING: PLANTS NOT', &
                           ' GRPED CONSISTENTLY IN IRECL, JRG,   IYR, W_GRP, W_IGRP:', &
                           IRECL,JRG,IYR,W_GRP,W_IGRP
!  in these writes, GWFL and WFL are arrays dimensioned by EFD_D_FPP
                        WRITE(UF_DBG,'(A,A,/," ***CRPGRP:",21X,<10+EFD_D_FPP-1>I7)') &
                           ' ***CRPGRP: COMPARE GROUP VALUES: GIEFD, GIECP,GIVIN3,GW_GRP,GWFOWN,', &
                           'GWNOWN,GRG_NUM,GIBTP, GINCT,  GWFL(3)=', &
                           GIEFD,GIECP,GIVIN3, &
                           GW_GRP,GWFOWN,GWNOWN,GRG_NUM,GWFL, &
                           GIBTP,GINCT
                        WRITE(UF_DBG,'(A,A,/," ***CRPGRP:",21X,<10+EFD_D_FPP-1>I7)') &
                           ' ***CRPGRP:      TO PLANT VALUES:  IEFD,  IECP, IVIN3, W_GRP, WFOWN,', &
                           ' WNOWN, RG_NUM, IBTP,  INCT,   WFL(3)=', &
                           IEFD,IECP,IVIN3, &
                           W_GRP,WFOWN,WNOWN,RG_NUM,WFL, &
                           IBTP,INCT
                     END IF
                  END IF                     ! UF_DBG .GT. 0
               END IF                     ! CAPACITY IN OPERATION IN CURRENT YEAR
!
 2000          CONTINUE
!
               IF (UPTTYP(IECP) .LE. NW_COAL .AND. W_RYR .GE. UPSTYR) THEN
                  ITYP = UPTTYP(IECP)
                  I_COAL = UCL_CGRP(IRECL)
                  IF (I_COAL .LE. 0) THEN
                     I_COAL = MAP_TO_COAL_ID(W_IGRP)
                     WRITE(6,2395) CURIYR+UHBSYR,CURITR,W_IGRP,W_GRP,I_COAL,UCL_CGRP2(W_GRP)
 2395                FORMAT(1X,"BAD_CL_GRP_5644",6(":",I6))
                     IF (I_COAL .GT. 0) THEN
                        UCL_CGRP2(W_GRP) = I_COAL
                        UCL_CGRP(IRECL) = I_COAL
                     END IF
                  END IF
                  IF (UPTTYP(IECP) .GT. EX_COAL) THEN
                     WPART = "0"
                     WSCBT = "W"
                     W_COMB = 1
                     W_POST = 2
                     IF (UPPCEF(IECP) .GT. 0.0) THEN  !CCS plant should be marked for 'SC'
                         W_ACI = "S"
                     ELSE
                         W_ACI = "N"
                     ENDIF
                     DO I_CNFG = 1 , NUM_CNFG
                        IF (UCL_ECP(I_CNFG) .EQ. IECP) THEN
                           ECL_ICFG(I_COAL) = I_CNFG
                           DO XYR = 1 , ECP_D_XPH
                              ECL_RCFG(XYR,I_COAL) = I_CNFG
                           END DO
                        END IF
                     END DO
                  END IF
                  IF (I_COAL .GT. 0) THEN
                     CRG = W_CLRG
!
                     if (i_coal .gt. max_cl) print *,'!maxcl',curiyr+1989,irg,max_cl,i_coal,IRECL,W_SYR,W_IGRP,W_GRP,WC_SUM
!
                     ECL_IGRP(I_COAL) = W_IGRP
!                    ECL_RG(I_COAL) = WNOPER
                     ECL_RG(I_COAL) = WNOWN
                     ECL_RYR(I_COAL) = MAX(ECL_RYR(I_COAL),W_RYR)
!                    ECL_CLRG(I_COAL) = EPNFLRG(W_CR,W_CLRG,W_GR)
                     ECL_CLRG(I_COAL) = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)
                     C_CAP(I_COAL) = C_CAP(I_COAL) + WC_SUM
                     DO ISP = 1 , ECP_D_MSP
                        ECL_SP_CAP_FAC(ISP,I_COAL) = ECL_SP_CAP_FAC(ISP,I_COAL) + ECP_SUMMER_SHR(ISP) * WC_SUM + &
                           (1.0 - ECP_SUMMER_SHR(ISP)) * WC_WIN
                     END DO
                     C_ECP(I_COAL) = IECP
                     C_GRP(I_COAL) = W_GRP
                     IF (UECP_CAP(2,ITYP,CRG) .GT. 0.0) THEN
                        C_CF(I_COAL) = UECP_GEN(2,ITYP,CRG) / (8.76 * UECP_CAP(2,ITYP,CRG))
                     ELSE
                        C_CF(I_COAL) = ACT_CF
                     END IF
                     DO XNOX = 1 , NOX_GRP
                        C_NXG(XNOX,I_COAL) = LNOX(XNOX)
                     END DO
                     IF (ACT_CF .GT. 0.0001 .AND. ULCAPC(W_GRP) .GT. 0.0001) THEN
                        C_VADJ(I_COAL) = WVOMA * ULTGEN(W_GRP)
                     END IF
                     C_FOM(I_COAL) = WFOMA + W_CAPAD
                     C_NXR(I_COAL) = WNOX_R
                     C_SO2R(I_COAL) = WSCBEF / 100.0
                     C_HTRT(I_COAL) = WHRATE
!
                        ECL_CMB_O(I_COAL) = ECL_CMB_O(I_COAL) + WCOMB_O * WC_SUM
                        ECL_CMB_F(I_COAL) = ECL_CMB_F(I_COAL) + WCOMB_F * WC_SUM
                        ECL_CMB_V(I_COAL) = ECL_CMB_V(I_COAL) + WCOMB_V * WC_SUM
                        ECL_CMB_R(I_COAL) = ECL_CMB_R(I_COAL) + WCOMB_R * WC_SUM
                     ECL_SNCR_O(I_COAL) = ECL_SNCR_O(I_COAL) + WSNCR_O * WC_SUM
                     ECL_SNCR_F(I_COAL) = ECL_SNCR_F(I_COAL) + WSNCR_F * WC_SUM
                     ECL_SNCR_V(I_COAL) = ECL_SNCR_V(I_COAL) + WSNCR_V * WC_SUM
                     ECL_SNCR_R(I_COAL) = ECL_SNCR_R(I_COAL) + WSNCR_R * WC_SUM
                        ECL_SCR_O(I_COAL) = ECL_SCR_O(I_COAL) + WSCR_O * WC_SUM
                        ECL_SCR_F(I_COAL) = ECL_SCR_F(I_COAL) + WSCR_F * WC_SUM
                        ECL_SCR_V(I_COAL) = ECL_SCR_V(I_COAL) + WSCR_V * WC_SUM
                        ECL_SCR_R(I_COAL) = ECL_SCR_R(I_COAL) + WSCR_R * WC_SUM
                     IF (WSCBCST .GT. 0.0 .AND. W_RYR .GT. 9000) THEN
                        ECL_FGD_O(I_COAL) = ECL_FGD_O(I_COAL) + WSCBCST * WC_SUM
                     ELSE
                        ECL_FGD_O(I_COAL) = ECL_FGD_O(I_COAL) + 999.99 * WC_SUM
                     END IF
                     ECL_FGD_F(I_COAL) = 0.0
                     ECL_FGD_V(I_COAL) = 0.0
                     ECL_FGD_R(I_COAL) = 0.95
!                    DSI INPUTS
                     IF (ECP_SCRUB(IECP,CURIYR) .EQ. 2)THEN
                        CALL EPA$DSICST(IECP,WC_SUM,DSIOVR,DSIFOM,DSIVOM,DSISEF)
                        ECL_DSI_O(I_COAL) = ECL_DSI_O(I_COAL) + W_DSIOV * WC_SUM
                        ECL_DSI_F(I_COAL) = ECL_DSI_F(I_COAL) + W_DSIF * WC_SUM
                        ECL_DSI_V(I_COAL) = ECL_DSI_V(I_COAL) + W_DSIV * WC_SUM
                        ECL_DSI_R(I_COAL) = ECL_DSI_R(I_COAL) + W_DSIR * WC_SUM
!                       ECL_DSI_O(I_COAL) = ECL_DSI_O(I_COAL) + DSIOVR * WC_SUM
!                       ECL_DSI_F(I_COAL) = ECL_DSI_F(I_COAL) + DSIFOM * WC_SUM
!                       ECL_DSI_V(I_COAL) = ECL_DSI_V(I_COAL) + DSIVOM * WC_SUM
!                       ECL_DSI_R(I_COAL) = ECL_DSI_R(I_COAL) + DSISEF * WC_SUM
                     ELSE
                        ECL_DSI_O(I_COAL) = ECL_DSI_O(I_COAL) + 999.99 * WC_SUM
                        ECL_DSI_F(I_COAL) = 0.0
                        ECL_DSI_V(I_COAL) = 0.0
                        ECL_DSI_R(I_COAL) = 0.0
                     END IF
!                    IF PLANT SCHEDULED TO RETIRE OR CONVERT THEN SET COSTS TO 0
                     IF (W_RYR .LT. 9000)THEN
                        ECL_DSI_O(I_COAL) = 0.0
                        ECL_DSI_F(I_COAL) = 0.0
                        ECL_DSI_V(I_COAL) = 0.0
                        ECL_DSI_R(I_COAL) = 0.0
                     END IF
!                    CCS INPUTS
                     IF (UPPCEF(IECP) .LE. 0.0)THEN
                        IF (CCSROV .LE. 0.0 .OR. W_RYR .LT. 9000)CCSROV = 9999.9
                        ECL_CCS_O(I_COAL) = ECL_CCS_O(I_COAL) + CCSROV * WC_SUM
                        ECL_CCS_F(I_COAL) = ECL_CCS_F(I_COAL) + CCSF   * WC_SUM
                        ECL_CCS_V(I_COAL) = ECL_CCS_V(I_COAL) + CCSV   * WC_SUM
                        ECL_CCS_R(I_COAL) = ECL_CCS_R(I_COAL) + 0.90   * WC_SUM
                        ECL_CCS_H(I_COAL) = ECL_CCS_H(I_COAL) + CCSHR  * WC_SUM
                        IF (UPTTYP(IECP) .LE. EX_COAL .AND. CCSHR .NE. 0.0) THEN
                           CCSADJ = 1.0 - WHRATE / CCSHR
                        ELSE
                           CCSADJ = 0.0
                        END IF
                        ECL_CCS_C(I_COAL) = ECL_CCS_C(I_COAL) + CCSADJ * WC_SUM
                     ELSE
                        ECL_CCS_O(I_COAL) = ECL_CCS_O(I_COAL) + 9999.9 * WC_SUM
                        ECL_CCS_F(I_COAL) = 0.0
                        ECL_CCS_V(I_COAL) = 0.0
                        ECL_CCS_R(I_COAL) = 0.0
                        ECL_CCS_H(I_COAL) = 0.0
                        ECL_CCS_C(I_COAL) = 0.0
                     END IF
                     ECL_FF_O(I_COAL) = ECL_FF_O(I_COAL) + W_FFOV * WC_SUM
                     ECL_FF_F(I_COAL) = ECL_FF_F(I_COAL) + W_FFF * WC_SUM
                     ECL_FF_V(I_COAL) = ECL_FF_V(I_COAL) + W_FFV * WC_SUM
!                    HEAT RATE IMPROVEMENT
                     HRQ = MAX(1,WHR_QT)
                     ECL_QT(I_COAL) = HRQ
                     IF (UPHRIMPQ(HRQ) .GT. 0 .AND. HTRT_OVRQ(IECP,HRQ) .GT. 0.0 .AND. WC_SUM .GE. HTRT_MIN_CAP(IECP) .AND. W_RYR .LT. 9000)THEN
                        ECL_HRI_O(I_COAL) = ECL_HRI_O(I_COAL) + HTRT_OVRQ(IECP,HRQ) * WC_SUM
!                       ECL_HRI_F(I_COAL) = ECL_HRI_F(I_COAL) + 0.0000 * WC_SUM
!                       ECL_HRI_V(I_COAL) = ECL_HRI_V(I_COAL) + 0.0000 * WC_SUM
                        HRIMP = MIN(HTRT_REDQ(IECP,HRQ)*WHRATE,WHRATE - HTRT_FLOOR(IECP))
                        HRIMP = MAX(HRIMP,0.0)
                        ECL_HRI_H(I_COAL) = ECL_HRI_H(I_COAL) + (HRIMP / WHRATE) * WC_SUM
                     ELSE
                        ECL_HRI_O(I_COAL) = ECL_HRI_O(I_COAL) + 999.99 * WC_SUM
                        ECL_HRI_F(I_COAL) = ECL_HRI_F(I_COAL) + 99.999 * WC_SUM
                        ECL_HRI_V(I_COAL) = ECL_HRI_V(I_COAL) + 9.9999 * WC_SUM
                        ECL_HRI_H(I_COAL) = ECL_HRI_H(I_COAL) + 0.0000 * WC_SUM
                     END IF
                     ECL_ESP_O(I_COAL) = ECL_ESP_O(I_COAL) + W_ESPU  * WC_SUM
                     ECL_CFB_O(I_COAL) = ECL_CFB_O(I_COAL) + W_CFBU  * WC_SUM
                     ECL_CFB_F(I_COAL) = ECL_CFB_F(I_COAL) + W_CFBUF * WC_SUM
                     ECL_CFB_V(I_COAL) = ECL_CFB_V(I_COAL) + W_CFBUV * WC_SUM

                     ECL_CL_NG_COST(I_COAL) = ECL_CL_NG_COST(I_COAL) + NG_COST * WC_SUM
                     ECL_CL_NG_TRAN(I_COAL) = ECL_CL_NG_TRAN(I_COAL) + NG_TRAN * WC_SUM
!
!                 STORE YEAR FOR AFFORDABLE CLEAN ENERGY IMPLEMENTATION, IF ANY
!
                     ECL_ACEST(I_COAL) = W_ST
                     IF (USW_ACE .GT. 0 .AND. W_ST .GT. 0)THEN
!                       IF ((CURIYR + UHBSYR + UPRTLT) .EQ. UYR_ACE(W_ST))THEN
                           IF (MRSW .LE. 0 .AND. (HTRT_OVRQ(IECP,ECL_QT(I_COAL)) .LT. 900.0 .AND. HTRT_REDQ(IECP,ECL_QT(I_COAL)) .GT. 0.001))THEN
                              ECL_ACEYR(I_COAL) = UYR_ACE(W_ST)
                           ELSE
                              ECL_ACEYR(I_COAL) = 0.0
                           END IF
!                       END IF
                     END IF
!
!     write(6,5454) curiyr+1989,w_igrp,w_grp,i_coal,wstate,uplntcd(iecp),ecl_qt(i_coal),ecl_acest(i_coal),ecl_aceyr(i_coal),wc_sum
!5454 format(1h ,'!acein',i4,i6,i6,i6,a3,a3,i3,i5,i5,f10.3)

                     ECL_MGRP(I_COAL) = HG_GRP(W_CLRG)
                     ECL_YR(I_COAL) = WRFURB
                     NREC = ECL_FREC(I_COAL)
                     ECL_FREC(I_COAL) = IRECL
                     ECL_NREC(IRECL) = NREC
                     IF (CURIYR .GE. 1 .AND. IVIN3 .EQ. 1) THEN
                        TCAP = ULCAPC(W_GRP)
                        IF (TCAP .GT. 0.0) THEN
                           VCST = ULVCST(W_GRP) / TCAP
                           FCST = ULFCST(W_GRP) / TCAP
                           SO2P = ULSO2P(W_GRP) / TCAP
                           NOXP = ULNOXP(W_GRP) / TCAP
                           RPSP = ULRPSP(W_GRP) / TCAP
                           HGP =  ULHGP(W_GRP) / TCAP
                           GHGP = ULGHG(W_GRP) / TCAP
                           KRG = ULORGN(W_GRP)
                           IF (TST_TGEN(W_GRP) .GT. 0.0) THEN
!                            REVS = (ULREVS(W_GRP)  - (ULCAPC(W_GRP) * RMAVG(CURIYR-1,KRG) *  MC_JPGDP(CURIYR))/1000 ) / TCAP
                             REVS = (ULREVS(W_GRP)  - (ULCAPC(W_GRP) * CAPCCR(IECP) * RMAVG(CURIYR-1,KRG) )/1000 ) / TCAP
                           ELSE
                           REVS = ULREVS(W_GRP) / TCAP
                           ENDIF
                           ECL_RVAL(I_COAL) = REVS - VCST - FCST - SO2P - NOXP - RPSP - HGP - GHGP
                        ELSE
                           ECL_RVAL(I_COAL) = 0.0
                           VCST = 0.0
                           FCST = 0.0
                           SO2P = 0.0
                           NOXP = 0.0
                           RPSP = 0.0
                           HGP =  0.0
                           REVS = 0.0
                        END IF
                     ELSE
                        ECL_RVAL(I_COAL) = 200.0 - WFOMA - W_CAPAD
                     END IF

                     IF ((CURIYR + UHBSYR) .GE. (UPSTYR + UPPLYR(WICT)) .AND. ULCAPC(W_GRP) .GT. 0.0 .AND. W_SYR .LT. (CURIYR + UHBSYR) .AND. W_RYR .GE. (CURIYR + UHBSYR - 1) .AND. W_GRP .NE. W_GRPL)THEN
                      IF (USW_NRVTYP .EQ. 2)THEN
                        COLNET1 = ULREVS(W_GRP) - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
                        COLNET2 = ULENGREVS(W_GRP) - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
!                       IF (ULTGEN(W_GRP) .GT. 0.0)THEN
!                          COLNET2 = (ULREVS(W_GRP) - (ULCAPC(W_GRP) * CAPCCR(IECP) * RMAVG(CURIYR-1,ULORGN(W_GRP)))/1000.0)  &
!                                 - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
!                       ELSE
!                          COLNET2 = COLNET1
!                       END IF
                        IF (USW_NRVREV .EQ. 1 .AND. COLNET1 .LT. 0.0) THEN
                           UNRVCOL(IRG,CURIYR) = UNRVCOL(IRG,CURIYR) - COLNET1
                           UNRVCOL(MNUMNR,CURIYR) = UNRVCOL(MNUMNR,CURIYR) - COLNET1
                           UNCPCOL(IRG,CURIYR) = UNCPCOL(IRG,CURIYR) + ULCAPC(W_GRP)
                           UNCPCOL(MNUMNR,CURIYR) = UNCPCOL(MNUMNR,CURIYR) + ULCAPC(W_GRP)
                        END IF
                        IF (USW_NRVREV .EQ. 2 .AND. COLNET2 .LT. 0.0) THEN
                           UNRVCOL(IRG,CURIYR) = UNRVCOL(IRG,CURIYR) - COLNET2
                           UNRVCOL(MNUMNR,CURIYR) = UNRVCOL(MNUMNR,CURIYR) - COLNET2
                           UNCPCOL(IRG,CURIYR) = UNCPCOL(IRG,CURIYR) + ULCAPC(W_GRP)
                           UNCPCOL(MNUMNR,CURIYR) = UNCPCOL(MNUMNR,CURIYR) + ULCAPC(W_GRP)
                        END IF
                      END IF
                      W_GRPL = W_GRP
                     END IF
!
!                    ACCUMULATE NON-REVENUE YEARS
!
                     OVEC = 0
                     IF (ITYP .LE. EX_COAL .AND. IOWN .LT. 4 .AND. (CURIYR + UHBSYR) .LE. MRSW)OVEC = 1
!                    IF (OVEC .EQ. 1)write(6,4455) curiyr+1989,w_igrp,w_grp,wc_sum
!4455 format(1h ,'!ovec1',i4,i6,i6,f10.3)
                     IF (OVEC .EQ. 0)THEN
                      IF ((CURIYR + UHBSYR) .GE. MAX(UPSTYR,UNUC_SYR - UREV_NYR + 1))THEN
                        IF (ECL_RVAL(I_COAL) .LT. -0.001 .AND. I_COAL .NE. I_COALL)ECL_RYRS(I_COAL) = ECL_RYRS(I_COAL) + 1
                        I_COALL = I_COAL
                      END IF
                     END IF
                     ECL_MR(I_COAL) = MAX(ECL_MR(I_COAL), MRSW)
!
                     DO XYR = 1 , UNXPH
                        IF (XYR .LT. UNXPH) THEN
                           TYYYYMM = (CURIYR + XYR + UHBSYR - 1) * 100 + 12
                        ELSE
!                          TYYYYMM = (CURIYR + UNFPH + UHBSYR - 1) * 100 + 12
                           TYYYYMM = (CURIYR + UNXPH + UHBSYR - 1) * 100 + 12
                        END IF
                        SYYYYMM = 100 * W_SYR + W_SMO
                        RYYYYMM = 100 * W_RYR + W_RMO
                        IF (SYYYYMM .LT. TYYYYMM .AND. RYYYYMM .GE. TYYYYMM) THEN
                           ECL_GRP(XYR,I_COAL) = W_GRP
                           ECL_ECP(XYR,I_COAL) = IECP
                           IF (EMM_CL_CF(I_COAL,MIN(UNYEAR,CURIYR+XYR-1)) .GT. 0.0) THEN
                              ECL_CF(XYR,I_COAL) = EMM_CL_CF(I_COAL,MIN(UNYEAR,CURIYR+XYR-1))
                           ELSE
                              ECL_CF(XYR,I_COAL) = ACT_CF
                           END IF
                           DO XNOX = 1 , NOX_GRP
                              ECL_NXG(XYR,XNOX,I_COAL) = LNOX(XNOX)
                           END DO

                           IF (T_WGRP(W_GRP) .GT. 0.0) THEN
                              ECL_VADJ(XYR,I_COAL) = T_VADJ(W_GRP) / T_WGRP(W_GRP)
                           ELSE
                              ECL_VADJ(XYR,I_COAL) = 0.0
                           END IF

                           WRITE(18,2317) CURIYR+UHBSYR, XYR, I_COAL, W_IGRP, W_GRP, T_ECP(IECP), IECP, ULCAPC(W_GRP), WC_SUM, &
                              T_WGRP(W_GRP), ECL_CF(XYR,I_COAL), &
                              ACT_CF, C_CF(I_COAL), WVOMA, ECL_VADJ(XYR,I_COAL), WFOMA, W_CAPAD, T_VADJ(W_GRP), C_VADJ(I_COAL), &
                              ULVCST(W_GRP), ULFCST(W_GRP), WHRATE, W_FOM, W_VOM
 2317                      FORMAT(1X,"ECL_VADJ_1",7(":",I5), 17(":",F12.3))

                           AGE = MIN(UNYEAR+ECP_D_FPH , CURIYR + UHBSYR + XYR - WRFURB)
                           ECL_CAP(XYR,I_COAL) = ECL_CAP(XYR,I_COAL) + WC_SUM
                           ECL_FOM(XYR,I_COAL) = ECL_FOM(XYR,I_COAL) + (WFOMA + WGAA + W_CAPAD + UPXTRA(AGE,IECP)) * WC_SUM
                           ECL_NXR(XYR,I_COAL) = ECL_NXR(XYR,I_COAL) + WNOX_R * WC_SUM
                           ECL_SO2R(XYR,I_COAL) = ECL_SO2R(XYR,I_COAL) + WSCBEF / 100.0 * WC_SUM
                           ECL_RCFG(XYR,I_COAL) = 0
                           IF (XYR .EQ. 1)ECL_SYR(I_COAL) = WRFURB
                           I_CNFG = ECL_ICFG(I_COAL)
                           I_MACT = UCL_CNFG(1,I_CNFG)
                           DO I_CNFG = 1 , NUM_CNFG
                              IF (UPTTYP(IECP) .EQ. UPTTYP(UCL_ECP(I_CNFG))) THEN
                                 ITST = 0
                                 IF (I_MACT .EQ. UCL_CNFG(1,I_CNFG)) ITST = ITST + 1
                                 IF (WPART .EQ. "B" .AND. UCL_CNFG(2,I_CNFG) .EQ. 1 .OR. &
                                     WPART .EQ. "E" .AND. UCL_CNFG(2,I_CNFG) .EQ. 2 .OR. &
                                     WPART .EQ. "N" .AND. UCL_CNFG(2,I_CNFG) .EQ. 3 .OR. &
                                     WPART .EQ. "0" .AND. UCL_CNFG(2,I_CNFG) .EQ. 4) ITST = ITST + 1
                                 IF (WSCBT .EQ. "W" .AND. UCL_CNFG(3,I_CNFG) .EQ. 1 .OR. &
                                     WSCBT .EQ. "D" .AND. UCL_CNFG(3,I_CNFG) .EQ. 2 .OR. &
                                     WSCBT .EQ. "N" .AND. UCL_CNFG(3,I_CNFG) .EQ. 0) ITST = ITST + 1
                                 IF (W_COMB .GT. 0 .AND. UCL_CNFG(4,I_CNFG) .EQ. 1 .OR. &
                                     W_COMB .EQ. 0 .AND. UCL_CNFG(4,I_CNFG) .EQ. 0) ITST = ITST + 1
                                 IF (W_POST .EQ. 1 .AND. UCL_CNFG(5,I_CNFG) .EQ. 1 .OR. &
                                     W_POST .EQ. 3 .AND. UCL_CNFG(5,I_CNFG) .EQ. 1 .OR. &
                                     W_POST .EQ. 5 .AND. UCL_CNFG(5,I_CNFG) .EQ. 1 .OR. &
                                     W_POST .EQ. 0 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                                     W_POST .EQ. 2 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                                     W_POST .EQ. 4 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                                     W_POST .EQ. 6 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                                     W_POST .EQ. 7 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0) ITST = ITST + 1
                                 IF (W_POST .EQ. 2 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                                     W_POST .EQ. 4 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                                     W_POST .EQ. 6 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                                     W_POST .EQ. 7 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                                     W_POST .EQ. 0 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0 .OR. &
                                     W_POST .EQ. 1 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0 .OR. &
                                     W_POST .EQ. 3 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0 .OR. &
                                     W_POST .EQ. 5 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0) ITST = ITST + 1
                                 IF (W_ACI .EQ. "N" .AND. UCL_CNFG(7,I_CNFG) .EQ. 0 .OR. &
                                     W_ACI .EQ. "A" .AND. UCL_CNFG(7,I_CNFG) .EQ. 0 .OR. &
                                     W_ACI .EQ. "S" .AND. UCL_CNFG(7,I_CNFG) .EQ. 1 .OR. &
                                     W_ACI .EQ. "F" .AND. UCL_CNFG(7,I_CNFG) .EQ. 0 .OR. &
                                     W_ACI .EQ. "B" .AND. UCL_CNFG(7,I_CNFG) .EQ. 1) ITST = ITST + 1
                                 IF (W_ACI .EQ. "N" .AND. UCL_CNFG(8,I_CNFG) .EQ. 0 .OR. &
                                     W_ACI .EQ. "A" .AND. UCL_CNFG(8,I_CNFG) .EQ. 0 .OR. &
                                     W_ACI .EQ. "S" .AND. UCL_CNFG(8,I_CNFG) .EQ. 0 .OR. &
                                     W_ACI .EQ. "F" .AND. UCL_CNFG(8,I_CNFG) .EQ. 1 .OR. &
                                     W_ACI .EQ. "B" .AND. UCL_CNFG(8,I_CNFG) .EQ. 1) ITST = ITST + 1
                                 IF (ITST .EQ. NUM_RCMB + 2 .AND. ECL_RCFG(XYR,I_COAL) .NE. I_CNFG .AND. ECL_RCFG(XYR,I_COAL) .GT. 0) THEN
                                    WRITE(18,9318) CURIYR+UHBSYR,CURIYR+UHBSYR+XYR-1,IRG,W_GRP,IRECL,I_COAL,I_CNFG, &
                                       ECL_RCFG(XYR,I_COAL),IECP,UCL_ECP(I_CNFG)
    9318                            FORMAT(1X,"CNFG_OOPS2",10(":",I5))
                                 END IF
                                 IF (ITST .EQ. NUM_RCMB + 2) THEN
                                    ECL_RCFG(XYR,I_COAL) = I_CNFG
                                    IF (IECP .NE. UCL_ECP(I_CNFG)) THEN
!                                      WRITE(6,9318) CURIYR+UHBSYR,CURIYR+UHBSYR+XYR-1,IRG,W_GRP,IRECL,I_COAL,I_CNFG, &
!                                         ECL_RCFG(XYR,I_COAL),IECP,UCL_ECP(I_CNFG)
                                    END IF
                                 END IF
                              END IF
                           END DO
                           IF (ECL_RCFG(XYR,I_COAL) .EQ. 0) THEN

                              WRITE(18,9319) CURIYR+UHBSYR,CURIYR+UHBSYR+XYR-1,W_SYR,W_RYR,IRG,W_GRP,IECP,I_COAL, &
                                 ECL_RCFG(XYR,I_COAL),ECL_ICFG(I_COAL),WPART,WSCBT,W_COMB,W_POST,W_ACI,ECL_CAP(XYR,I_COAL)
 9319                         FORMAT(1X,"CNFG_OOPS1",10(":",I5),2(":",A1),2(":",I1),":",A1,":",F12.3)

                              ECL_RCFG(XYR,I_COAL) = SV_RCFG(XYR,I_COAL)
                           END IF
                        END IF
                     END DO
!
!                    IF (W_SYR .GE. UPSTYR .AND. CURIYR+UHBSYR .GE. W_SYR .AND. CURIYR+UHBSYR .LE. W_RYR .AND. UF_DBG .GT. 0) THEN
                     IF (CURIYR+UHBSYR .GE. W_SYR .AND. CURIYR+UHBSYR .LE. W_RYR) THEN
                        WRITE(UF_DBG,2112) CURIYR+UHBSYR,W_IGRP,W_GRP,W_GRP2,I_COAL,ECL_ICFG(I_COAL),W_SYR,W_RYR,ECL_YR(I_COAL), &
                           ECL_RG(I_COAL),ECL_CLRG(I_COAL),ECL_FREC(I_COAL),ECL_ECP(1,I_COAL),ECL_IGRP(I_COAL),ECL_MGRP(I_COAL), &
                           C_CAP(I_COAL),T_VCST,ECL_VADJ(1,I_COAL),ECL_FOM(1,I_COAL),ECL_NXR(1,I_COAL),ECL_SO2R(1,I_COAL), &
                           (ECL_RCFG(XYR,I_COAL),ECL_CAP(XYR,I_COAL),XYR=1,UNXPH),ECL_CF(1,I_COAL),ACT_CF,UPMCF(IECP), &
                           ULTGEN(W_GRP),ULCAPC(W_GRP),ECL_RVAL(I_COAL), VCST, FCST, SO2P, NOXP, RPSP, HGP, REVS, &
                           (ECL_SP_CAP_FAC(ISP,I_COAL),ISP=1,3)
 2112                   FORMAT(1X,"ECL_DATA",15(":",I5),3(":",F9.3),1(":",F9.1),2(":",F9.3),<UNXPH>(":",I3,":",F7.1),5(":",F9.3),8(":",F7.3),3(":",F7.1))
                     END IF
                  END IF ! Coal Unit Index is non-zero
               END IF ! Coal Units in existence after NEMS Start Year

!              SET UP NG CC RETROFIT GROUP DATA

               I_NGBS = 0
               IF ((IECP .EQ. WIEC .OR. IECP .EQ. WIST .OR. IECP .EQ. WIA2) .AND. W_RYR .GE. CURIYR + UHBSYR) THEN
                  IF (UNG_CGRP(IRECL) .GT. 0) THEN
                     I_NGBS = UNG_CGRP(IRECL)
                  ELSE
                     IF (UNG_WGRP(W_GRP) .GT. 0) THEN
                        I_NGBS = UNG_WGRP(W_GRP)
                        UNG_CGRP(IRECL) = I_NGBS
                     ELSE
                        NUM_NGBS = NUM_NGBS + 1
                        UNG_CGRP(IRECL) = NUM_NGBS
                        UNG_WGRP(W_GRP) = NUM_NGBS
                        I_NGBS = NUM_NGBS
                     END IF
                  END IF

                  IF (I_NGBS .GT. MX_NGBS) THEN
                     WRITE(6,9341) CURIRUN, CURIYR+1989, IRG, MX_NGBS, I_NGBS, IRECL, W_SYR, W_IGRP, W_GRP, WC_SUM
 9341                FORMAT(1X,"MAXIMUM NGBS Groups has been exceeded",9(":",I5),":",F10.3)
                  END IF

                  ENG_GRP(I_NGBS) = W_GRP
                  ENG_RG(I_NGBS) = IRG
                  NREC = ENG_FREC(I_NGBS)
                  ENG_FREC(I_NGBS) = IRECL
                  ENG_NREC(IRECL) = NREC
                  IF ((CURIYR + UHBSYR) .GE. UPCCSSYR .AND. (W_RYR-UHBSYR) .GT. UNYEAR) THEN
                     TCAP = ULCAPC(W_GRP)
                     IF (TCAP .GT. 0.0) THEN
                        VCST = ULVCST(W_GRP) / TCAP
                        FCST = ULFCST(W_GRP) / TCAP
                        SO2P = ULSO2P(W_GRP) / TCAP
                        NOXP = ULNOXP(W_GRP) / TCAP
                        RPSP = ULRPSP(W_GRP) / TCAP
                        HGP =  ULHGP(W_GRP) / TCAP
                        GHGP = ULGHG(W_GRP) / TCAP
                        REVS = ULREVS(W_GRP) / TCAP
                        ENG_RVAL(I_NGBS) = REVS - VCST - FCST - SO2P - NOXP - RPSP - HGP - GHGP
                     ELSE
                        ENG_RVAL(I_NGBS) = 0.0
                        VCST = 0.0
                        FCST = 0.0
                        SO2P = 0.0
                        NOXP = 0.0
                        RPSP = 0.0
                        HGP =  0.0
                        REVS = 0.0
                     END IF
                  ELSE
                     ENG_RVAL(I_NGBS) = 350.0 - WFOMA - W_CAPAD
                     VCST = 0.0
                     FCST = 0.0
                     SO2P = 0.0
                     NOXP = 0.0
                     RPSP = 0.0
                     HGP =  0.0
                     REVS = 0.0
                  END IF

                  ENG_ECP(I_NGBS) = IECP
                  ENG_MR(I_NGBS) = MAX(ENG_MR(I_NGBS),MRSW)
                  ENG_SYR(I_NGBS) = W_SYR
                  ENG_RYR(I_NGBS) = MAX(ENG_RYR(I_NGBS),W_RYR)

                  DO ISP = 1 , ECP_D_MSP
                     ENG_SP_CAP_FAC(ISP,I_NGBS) = ENG_SP_CAP_FAC(ISP,I_NGBS) + ECP_SUMMER_SHR(ISP) * WC_SUM + (1.0 - ECP_SUMMER_SHR(ISP)) * WC_WIN
                  END DO

                  ENG_VOM(I_NGBS) = ENG_VOM(I_NGBS) + WVOMA * WC_SUM

                  ENG_FLRG(I_NGBS) = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)

                  CAP_NGBS(I_NGBS) = CAP_NGBS(I_NGBS) + WC_SUM

                  DO XYR = 1 , UNXPH
                     IYRMO = (CURIYR + XYR + UHBSYR - 1) * 100 + 6
                     RYRMO = W_RYR * 100 + W_RMO
                     SYRMO = W_SYR * 100 + W_SMO
                     IF (SYRMO .LE. IYRMO .AND. RYRMO .GT. IYRMO) THEN
                        AGE = MIN(UNYEAR+ECP_D_FPH , CURIYR + UHBSYR + XYR - WRFURB)
                        ENG_FOM(XYR,I_NGBS) = ENG_FOM(XYR,I_NGBS) + (WFOMA + WGAA + W_CAPAD + UPXTRA(AGE,IECP)) * WC_SUM

                        ENG_CAP(XYR,I_NGBS) = ENG_CAP(XYR,I_NGBS) + WC_SUM
                     END IF
                  END DO

                  ENG_CF(I_NGBS) = ENG_CF(I_NGBS) + W_CFA * WC_SUM

!                 CCS INPUTS

                  IF (IECP .NE. WIST) THEN
                     IF (UPPCEF(IECP) .LE. 0.0)THEN
                        IF (CCSROV .LE. 0.0 .OR. W_RYR .LT. 9000)CCSROV = 9999.9
                        ENG_CCS_O(I_NGBS) = ENG_CCS_O(I_NGBS) + CCSROV * WC_SUM
                        ENG_CCS_F(I_NGBS) = ENG_CCS_F(I_NGBS) + CCSF   * WC_SUM
                        ENG_CCS_V(I_NGBS) = ENG_CCS_V(I_NGBS) + CCSV   * WC_SUM
                        ENG_CCS_R(I_NGBS) = ENG_CCS_R(I_NGBS) + 0.90   * WC_SUM
                        ENG_CCS_H(I_NGBS) = ENG_CCS_H(I_NGBS) + CCSHR  * WC_SUM
                        IF (CCSHR .GT. 0.001) THEN
                           CCSADJ = 1.0 - WHRATE / CCSHR
                        ELSE
                           CCSADJ = 0.0
                        END IF
                        ENG_CCS_C(I_NGBS) = ENG_CCS_C(I_NGBS) + CCSADJ * WC_SUM
                     ELSE
                        ENG_CCS_O(I_NGBS) = ENG_CCS_O(I_NGBS) + 9999.9 * WC_SUM
                        ENG_CCS_F(I_NGBS) = 0.0
                        ENG_CCS_V(I_NGBS) = 0.0
                        ENG_CCS_R(I_NGBS) = 0.0
                        ENG_CCS_H(I_NGBS) = 0.0
                        ENG_CCS_C(I_NGBS) = 0.0
                     END IF
                  ELSE
                     ENG_CCS_O(I_NGBS) = ENG_CCS_O(I_NGBS) + 9999.9 * WC_SUM
                     ENG_CCS_F(I_NGBS) = ENG_CCS_F(I_NGBS) + 9999.9 * WC_SUM
                     ENG_CCS_V(I_NGBS) = ENG_CCS_V(I_NGBS) + 9999.9 * WC_SUM
                     ENG_CCS_R(I_NGBS) = ENG_CCS_R(I_NGBS) + 0.0 * WC_SUM
                     ENG_CCS_H(I_NGBS) = ENG_CCS_H(I_NGBS) + 1.0 * WC_SUM
                     ENG_CCS_C(I_NGBS) = ENG_CCS_C(I_NGBS) + 1.0 * WC_SUM
                  END IF

                  WRITE(UF_DBG,3112) CURIRUN, CURIYR+UHBSYR, W_IGRP, W_GRP, W_GRP2, I_NGBS, IECP, W_SYR, W_RYR, ENG_FLRG(I_NGBS), IRG, &
                     ENG_FREC(I_NGBS), ENG_ECP(I_NGBS), CAP_NGBS(I_NGBS), &
                     (ENG_FOM(XYR,I_NGBS)/MAX(1.0,ENG_CAP(XYR,I_NGBS)), ENG_CAP(XYR,I_NGBS),XYR=1,UNXPH), &
                     WC_SUM, WHRATE, W_CF, W_CFA, ULTGEN(W_GRP), ULCAPC(W_GRP), &
                     ENG_RVAL(I_NGBS), VCST, FCST, REVS, WFOMA, W_CAPAD, UPPCEF(IECP), CCSROV, CCSF, CCSV, CCSHR, CCSADJ
 3112            FORMAT(1X,"ENG_DATA",13(":",I5),30(":",F9.3))

               END IF

               IF (IECP .EQ. WIEC .OR. IECP .EQ. WICC .OR. IECP .EQ. WIAC .OR. IECP .EQ. WIST .OR. IECP .EQ. WIA2) THEN

                  IF ((CURIYR + UHBSYR) .GE. (UPSTYR + UPPLYR(WICT)) .AND. ULCAPC(W_GRP) .GT. 0.0 .AND. W_SYR .LT. (CURIYR + UHBSYR) .AND. W_RYR .GE. (CURIYR + UHBSYR - 1) .AND. W_GRP .NE. W_GRPL)THEN
                   IF (USW_NRVTYP .EQ. 2)THEN
                     IF (IECP .NE. WIST)THEN
                        CCYNET1 = ULREVS(W_GRP) - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
                        CCYNET2 = ULENGREVS(W_GRP) - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
!                       IF (ULTGEN(W_GRP) .GT. 0.0)THEN
!                          CCYNET2 = (ULREVS(W_GRP) - (ULCAPC(W_GRP) * CAPCCR(IECP) * RMAVG(CURIYR-1,ULORGN(W_GRP)))/1000.0)  &
!                                  - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
!                       ELSE
!                          CCYNET2 = CCYNET1
!                       END IF
                        IF (USW_NRVREV .EQ. 1 .AND. CCYNET1 .LT. 0.0) THEN
                           UNRVCCY(IRG,CURIYR) = UNRVCCY(IRG,CURIYR) - CCYNET1
                           UNRVCCY(MNUMNR,CURIYR) = UNRVCCY(MNUMNR,CURIYR) - CCYNET1
                           UNCPCCY(IRG,CURIYR) = UNCPCCY(IRG,CURIYR) + ULCAPC(W_GRP)
                           UNCPCCY(MNUMNR,CURIYR) = UNCPCCY(MNUMNR,CURIYR) + ULCAPC(W_GRP)
                        END IF
                        IF (USW_NRVREV .EQ. 2 .AND. CCYNET2 .LT. 0.0) THEN
                           UNRVCCY(IRG,CURIYR) = UNRVCCY(IRG,CURIYR) - CCYNET2
                           UNRVCCY(MNUMNR,CURIYR) = UNRVCCY(MNUMNR,CURIYR) - CCYNET2
                           UNCPCCY(IRG,CURIYR) = UNCPCCY(IRG,CURIYR) + ULCAPC(W_GRP)
                           UNCPCCY(MNUMNR,CURIYR) = UNCPCCY(MNUMNR,CURIYR) + ULCAPC(W_GRP)
!     write(6,4565) curiyr+1989,irg,w_igrp,w_grp,uplntcd(iecp),w_syr,w_ryr,wc_sum,ulcapc(w_grp),ultgen(w_grp),ulrevs(w_grp),  &
!                   max(0.0,-ccynet1),-ccynet2,unrvccy(irg,curiyr),unrvccy(mnumnr,curiyr)
!4565 format(1h ,'!ccyneg',i4,i3,i6,i6,a3,i5,i5,12f10.3)
                        END IF
                     ELSE
                        STMNET1 = ULREVS(W_GRP) - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
                        STMNET2 = ULENGREVS(W_GRP) - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
      if (w_grp .eq. 4012 .or. w_grp .eq. 4013)write(6,5566) curiyr+1989,w_igrp,w_grp,w_syr,w_ryr,wc_sum,stmnet1,stmnet2
 5566 format(1h ,'!pltstm',i4,i6,i6,i6,i6,5f10.3)
!                       IF (ULTGEN(W_GRP) .GT. 0.0)THEN
!                          STMNET2 = (ULREVS(W_GRP) - (ULCAPC(W_GRP) * CAPCCR(IECP) * RMAVG(CURIYR-1,ULORGN(W_GRP)))/1000.0)  &
!                                  - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
!                       ELSE
!                          STMNET2 = STMNET1
!                       END IF
                        IF (USW_NRVREV .EQ. 1 .AND. STMNET1 .LT. 0.0) THEN
                           UNRVSTM(IRG,CURIYR) = UNRVSTM(IRG,CURIYR) - STMNET1
                           UNRVSTM(MNUMNR,CURIYR) = UNRVSTM(MNUMNR,CURIYR) - STMNET1
                           UNCPSTM(IRG,CURIYR) = UNCPSTM(IRG,CURIYR) + ULCAPC(W_GRP)
                           UNCPSTM(MNUMNR,CURIYR) = UNCPSTM(MNUMNR,CURIYR) + ULCAPC(W_GRP)
                        END IF
                        IF (USW_NRVREV .EQ. 2 .AND. STMNET2 .LT. 0.0) THEN
                           UNRVSTM(IRG,CURIYR) = UNRVSTM(IRG,CURIYR) - STMNET2
                           UNRVSTM(MNUMNR,CURIYR) = UNRVSTM(MNUMNR,CURIYR) - STMNET2
                           UNCPSTM(IRG,CURIYR) = UNCPSTM(IRG,CURIYR) + ULCAPC(W_GRP)
                           UNCPSTM(MNUMNR,CURIYR) = UNCPSTM(MNUMNR,CURIYR) + ULCAPC(W_GRP)
!                if (irg .eq. 20)write (6,2346) curiyr + 1989,w_grp,  &
!                                 ULREVS(W_GRP) , ULENGREVS(W_GRP),  &
!                                         ULFCST(W_GRP) , ULVCST(W_GRP) , ULSO2P(W_GRP) , ULNOXP(W_GRP) , ULRPSP(W_GRP) , ULHGP(W_GRP) , ULGHG(W_GRP), STMNET2, ULCAPC(W_GRP),  &
!                                         UNRVSTM(IRG,CURIYR) ,  &
!                                         UNCPSTM(IRG,CURIYR)
!2346 format(1h ,'!negstmlag',i4,i6,15f10.3)
!     write(6,4566) curiyr+1989,irg,w_igrp,w_grp,uplntcd(iecp),w_syr,w_ryr,wc_sum,ulcapc(w_grp),ultgen(w_grp),ulrevs(w_grp),  &
!                   max(0.0,-stmnet1),-stmnet2,unrvstm(irg,curiyr),unrvstm(mnumnr,curiyr)
!4566 format(1h ,'!stmneg',i4,i3,i6,i6,a3,i5,i5,15f10.3)
                        END IF
                     END IF
                   END IF
                   W_GRPL = W_GRP
                  END IF
               END IF
!
               I_NUC = 0
               IF (IECP .EQ. WICN .OR. IECP .EQ. WIAN .OR. IECP .EQ. WISM) THEN
                  IF (UNUC_CGRP(IRECL) .GT. 0) THEN
                     I_NUC = UNUC_CGRP(IRECL)
                  ELSE
                     IF (UNUC_WGRP(W_GRP) .GT. 0) THEN
                        I_NUC = UNUC_WGRP(W_GRP)
                        UNUC_CGRP(IRECL) = I_NUC
                     ELSE
                        NUM_NUC = NUM_NUC + 1
                        UNUC_CGRP(IRECL) = NUM_NUC
                        UNUC_WGRP(W_GRP) = NUM_NUC
                        I_NUC = NUM_NUC
                     END IF
                  END IF
!
                  if(I_NUC .gt. MAXNUC)print *,'!maxNUC',curiyr+1989, &
                     irg,MAXNUC,I_NUC,IRECL,W_SYR,W_IGRP,W_GRP,WC_SUM
!
                  ENUC_GRP(I_NUC) = W_GRP
                  ENUC_IGRP(I_NUC) = W_IGRP
                  ENUC_RG(IRG,I_NUC) = ENUC_RG(IRG,I_NUC) + WC_SUM
                  NREC = ENUC_FREC(I_NUC)
                  ENUC_FREC(I_NUC) = IRECL
                  ENUC_NREC(IRECL) = NREC
                  IF ((CURIYR + UHBSYR) .GE. W_SYR .AND. (W_RYR-UHBSYR) .GT. (CURIYR + 5) &    ! don't evaluate if within 5 years of announced retirement
                        .AND. USW_NRET .GT. 0 ) THEN
                     TCAP = ULCAPC(W_GRP)
                     IF (TCAP .GT. 0.0) THEN
                        VCST = ULVCST(W_GRP) / TCAP
                        FCST = ULFCST(W_GRP) / TCAP
                        SO2P = ULSO2P(W_GRP) / TCAP
                        NOXP = ULNOXP(W_GRP) / TCAP
                        RPSP = ULRPSP(W_GRP) / TCAP
                        HGP =  ULHGP(W_GRP) / TCAP
                        GHGP = ULGHG(W_GRP) / TCAP
                        KRG = ULORGN(W_GRP)
                        IF (TST_TGEN(W_GRP) .GT. 0.0) THEN
!                         REVS = (ULREVS(W_GRP)  - (ULCAPC(W_GRP) * RMAVG(CURIYR-1,KRG) *  MC_JPGDP(CURIYR))/1000 ) / TCAP
                          REVS = (ULREVS(W_GRP)  - (ULCAPC(W_GRP) * CAPCCR(IECP) * RMAVG(CURIYR-1,KRG) )/1000 ) / TCAP
                        ELSE
                        REVS = ULREVS(W_GRP) / TCAP
                        ENDIF
                        ENUC_RVAL(I_NUC) = REVS - VCST - FCST - SO2P - NOXP - RPSP - HGP - GHGP
                     ELSE
                        ENUC_RVAL(I_NUC) = 0.0
                        VCST = 0.0
                        FCST = 0.0
                        SO2P = 0.0
                        NOXP = 0.0
                        RPSP = 0.0
                        HGP =  0.0
                        REVS = 0.0
                     END IF
                  ELSE
                     ENUC_RVAL(I_NUC) = 350.0 - WFOMA - W_CAPAD
                     VCST = 0.0
                     FCST = 0.0
                     SO2P = 0.0
                     NOXP = 0.0
                     RPSP = 0.0
                     HGP =  0.0
                     REVS = 0.0
                  END IF
!
!                 STORE INFO FOR ZERO-CREDIT SUBSIDY, IF ANY
!
                  ENUC_ZECST(I_NUC) = W_ST
                  IF (USW_ZECREV .GT. 0 .AND. W_ST .GT. 0)THEN
                     IF (UYR_ZEC(W_ST) .GT. 0 .AND. W_RYR .GT. UY1_ZEC(W_ST))THEN
                        IF (URG_ZEC(W_ST) .LE. 0 .OR. IRG .EQ. URG_ZEC(W_ST))THEN
                           ENUC_ZECY1(I_NUC) = UY1_ZEC(W_ST)
                           ENUC_ZECYR(I_NUC) = UYR_ZEC(W_ST) + ENUC_ZECRN(I_NUC)
                        END IF
                     END IF
                  END IF
!
!                 ACCUMULATE NON-REVENUE YEARS
!
                  IF ((CURIYR + UHBSYR) .GE. MAX(UPSTYR,UNUC_SYR - UREV_NYRN + 1))THEN
!                    DON'T COUNT YEAR IF ZEC SUBSIDY
                     IF ((CURIYR + UHBSYR) .GT. ENUC_ZECYR(I_NUC))THEN
                        IF (ENUC_RVAL(I_NUC) .LT. -0.001 .AND. I_NUC .NE. I_NUCL)ENUC_RYRS(I_NUC) = ENUC_RYRS(I_NUC) + 1
                     END IF
                     I_NUCL = I_NUC
                  END IF

!                 ACCUMULATE NET REVENUES AND REQUIRED SUBSIDY IF ZEC AVAILABLE
                  NUCNET1 = ULREVS(W_GRP) - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
                  NUCNET2 = ULENGREVS(W_GRP) - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
!                 IF (ULTGEN(W_GRP) .GT. 0.0)THEN
!                    NUCNET2 = (ULREVS(W_GRP) - (ULCAPC(W_GRP) * CAPCCR(IECP) * RMAVG(CURIYR-1,ULORGN(W_GRP)))/1000.0)  &
!                              - ULVCST(W_GRP) - ULFCST(W_GRP) - ULSO2P(W_GRP) - ULNOXP(W_GRP) - ULRPSP(W_GRP) - ULHGP(W_GRP) - ULGHG(W_GRP)
!                 ELSE
!                    NUCNET2 = NUCNET1
!                 END IF
              
                  IF ((CURIYR + UHBSYR) .GE. (UPSTYR + UPPLYR(WICT)) .AND. ULCAPC(W_GRP) .GT. 0.0 .AND. W_SYR .LT. (CURIYR + UHBSYR) .AND. W_RYR .GE. (CURIYR + UHBSYR - 1) .AND. W_GRP .NE. W_GRPL)THEN
                     IF (USW_NRVTYP .EQ. 2)THEN
                        IF (USW_NRVREV .EQ. 1 .AND. NUCNET1 .LT. 0.0) THEN
                           UNRVNUC(IRG,CURIYR) = UNRVNUC(IRG,CURIYR) - NUCNET1
                           UNRVNUC(MNUMNR,CURIYR) = UNRVNUC(MNUMNR,CURIYR) - NUCNET1
                           UNCPNUC(IRG,CURIYR) = UNCPNUC(IRG,CURIYR) + ULCAPC(W_GRP)
                           UNCPNUC(MNUMNR,CURIYR) = UNCPNUC(MNUMNR,CURIYR) + ULCAPC(W_GRP)
                        END IF
                        IF (USW_NRVREV .EQ. 2 .AND. NUCNET2 .LT. 0.0) THEN
                           UNRVNUC(IRG,CURIYR) = UNRVNUC(IRG,CURIYR) - NUCNET2
                           UNRVNUC(MNUMNR,CURIYR) = UNRVNUC(MNUMNR,CURIYR) - NUCNET2
                           UNCPNUC(IRG,CURIYR) = UNCPNUC(IRG,CURIYR) + ULCAPC(W_GRP)
                           UNCPNUC(MNUMNR,CURIYR) = UNCPNUC(MNUMNR,CURIYR) + ULCAPC(W_GRP)
                        END IF
                     END IF
                     W_GRPL = W_GRP
                  END IF

!        if (wstate .eq. 'IL' .or. wstate .eq. 'NJ' .or. wstate .eq. 'NY')write(6,5544) curiyr+1989,i_nuc,wstate,w_igrp,w_grp,  &
!                    enuc_zecy1(i_nuc),enuc_zecyr(i_nuc),enuc_zecrn(i_nuc),wc_sum,nucnet1,nucnet2
!5544 format(1h ,'!zecyr',i4,i6,a3,i6,i6,i6,i6,i6,3f10.3)
                  IF ((CURIYR+UHBSYR) .GE. ENUC_ZECY1(I_NUC) .AND. (CURIYR+UHBSYR) .LE. ENUC_ZECYR(I_NUC)) THEN

                     IF (USW_ZECNET .EQ. 1) THEN    !calc subsidy using revenues with cap payment
                        IF (NUCNET1 .LT. 0.0) THEN
                           ENUC_SUB(I_NUC) = -1.0 * NUCNET1
                           ENUC_ZECNM(I_NUC) = ENUC_ZECNM(I_NUC) + 1
                        ELSE
                           ENUC_SUB(I_NUC) = 0.0
                        ENDIF
                     ELSEIF (USW_ZECNET .EQ. 2) THEN   !calc subsidy using revenues without cap payment
                        IF (NUCNET2 .LT. 0.0) THEN
                           ENUC_SUB(I_NUC) = -1.0 * NUCNET2
                           ENUC_ZECNM(I_NUC) = ENUC_ZECNM(I_NUC) + 1
                        ELSE
                           ENUC_SUB(I_NUC) = 0.0
                        ENDIF
                     ELSE
                        ENUC_SUB(I_NUC) = 0.0         !don't calculate subsidy
                     ENDIF
!     write(6,2311) curiyr+1989,w_igrp,w_grp,wstate,w_st,i_nuc,enuc_zecy1(i_nuc),enuc_zecyr(i_nuc),enuc_zecrn(i_nuc),uyr_zecrnw(w_st),enuc_zecnm(i_nuc),nucnet1,nucnet2
!2311 format(1h ,'!zecout',i4,i6,i6,a3,i3,i6,i5,i5,i5,i5,i5,2f10.3)

!                    AT END OF ZEC PERIOD, IF RENEWAL ALLOWED (E.G. NJ) AND LOSSES INCURRED THEN EXTEND

                     IF (UYR_ZECRNW(W_ST) .GT. 0)THEN
                        IF ((CURIYR + UHBSYR) .EQ. ENUC_ZECYR(I_NUC))THEN
                           IF (ENUC_ZECNM(I_NUC) .GT. 0)THEN
                              ENUC_ZECRN(I_NUC) = ENUC_ZECRN(I_NUC) + UYR_ZECRNW(W_ST)
                              ENUC_ZECNM(I_NUC) = 0
                           END IF
                        END IF
                     END IF

                     IF (ENUC_SUB(I_NUC) .GT. 0.0)THEN
                       IF (URG_ZECPRC(W_ST) .GT. 0) THEN
                        ELNUCSBDY(W_ST,ULORGN(W_GRP),CURIYR) = ELNUCSBDY(W_ST,ULORGN(W_GRP),CURIYR) + ENUC_SUB(I_NUC)
                        ELNUCSBDY(UNSTAS + 1,ULORGN(W_GRP),CURIYR) = ELNUCSBDY(UNSTAS + 1,ULORGN(W_GRP),CURIYR) + ENUC_SUB(I_NUC)
                        ELNUCSBDY(W_ST,MNUMNR,CURIYR) = ELNUCSBDY(W_ST,MNUMNR,CURIYR) + ENUC_SUB(I_NUC)
                        ELNUCSBDY(UNSTAS + 1,MNUMNR,CURIYR) = ELNUCSBDY(UNSTAS + 1,MNUMNR,CURIYR) + ENUC_SUB(I_NUC)
                        ELNUCSBGN(W_ST,ULORGN(W_GRP)) = ELNUCSBGN(W_ST,ULORGN(W_GRP)) + ULTGEN(W_GRP) / 1000.0
                        ELNUCSBGN(UNSTAS + 1,ULORGN(W_GRP)) = ELNUCSBGN(UNSTAS + 1,ULORGN(W_GRP)) + ULTGEN(W_GRP) / 1000.0
                        ELNUCSBGN(W_ST,MNUMNR) = ELNUCSBGN(W_ST,MNUMNR) + ULTGEN(W_GRP) / 1000.0
                        ELNUCSBGN(UNSTAS + 1,MNUMNR) = ELNUCSBGN(UNSTAS + 1,MNUMNR) + ULTGEN(W_GRP) / 1000.0
!                       write(6,3456) curiyr+1989,w_igrp,w_grp,wstate,irg,wc_sum,w_cfa,wc_sum*w_cfa*8.760,nucnet1,nucnet2,enuc_sub(i_nuc)
!3456 format(1h ,'!zeca',i4,i6,i6,1x,a3,i3,6f10.3)
                       ENDIF
                     ELSE
!                       Only Apply ZEC to Plants with Negative Revenues, if that option selected
                        IF (USW_ZECREV .EQ. 1)THEN
                           ENUC_ZECYR(I_NUC) = 0
!                    write(6,3457) curiyr+1989,w_igrp,w_grp,wstate,irg,wc_sum,w_cfa,wc_sum*w_cfa*8.760,enuc_sub(i_nuc)
!3457 format(1h ,'!zecn',i4,i6,i6,1x,a3,i3,4f10.3)
                        ELSE IF (USW_ZECREV .EQ. 2)THEN
!                       Include Plants without Negative Revenues, if that option selected
                           ELNUCSBGN(W_ST,ULORGN(W_GRP)) = ELNUCSBGN(W_ST,ULORGN(W_GRP)) + ULTGEN(W_GRP) / 1000.0
                           ELNUCSBGN(UNSTAS + 1,ULORGN(W_GRP)) = ELNUCSBGN(UNSTAS + 1,ULORGN(W_GRP)) + ULTGEN(W_GRP) / 1000.0
                           ELNUCSBGN(W_ST,MNUMNR) = ELNUCSBGN(W_ST,MNUMNR) + ULTGEN(W_GRP) / 1000.0
                           ELNUCSBGN(UNSTAS + 1,MNUMNR) = ELNUCSBGN(UNSTAS + 1,MNUMNR) + ULTGEN(W_GRP) / 1000.0
                        END IF
                     END IF
                  ENDIF

                 write(13,4433) curiyr+1989,i_nuc,w_grp,w_igrp,w_st,ulorgn(W_GRP),usw_zecnet,nucnet1,nucnet2,enuc_sub(i_nuc),elnucsbdy(unstas + 1,ulorgn(W_GRP),curiyr),ulcapc(W_GRP),ultgen(W_GRP)
4433    format(1x,'!nucsub',7I6,6F12.5)
!
                  ENUC_ECP(I_NUC) = IECP
                  CAP_NUC2(I_NUC) = CAP_NUC2(I_NUC) + WC_SUM
!
                  DO ISP = 1 , ECP_D_MSP
                     ENUC_SP_CAP_FAC(ISP,I_NUC) = ENUC_SP_CAP_FAC(ISP,I_NUC) + ECP_SUMMER_SHR(ISP) * WC_SUM + (1.0 - ECP_SUMMER_SHR(ISP)) * WC_WIN
                  END DO
!
                  ENUC_HTRT(I_NUC) = ENUC_HTRT(I_NUC) + WHRATE * WC_SUM
                  ENUC_VOM(I_NUC) = ENUC_VOM(I_NUC) + WVOMA * WC_SUM
                  IF (IECP .EQ. WICN)  ENUC_PTC(I_NUC) = ENUC_PTC(I_NUC) + GSUB * WC_SUM   !only fill for existing nuclear PTC, so it affects ECP dispatch
!
                  ENUC_ASYR(I_NUC) = WRFURB + UPNAYR
                  ENUC_ACST(I_NUC) = ENUC_ACST(I_NUC) + WNUCA_O * WC_SUM
!     write(6,4433) curiyr+1989,i_nuc,w_igrp,wstate,wc_sum,wrfurb,w_fom,wvoma,enuc_asyr(i_nuc),enuc_acst(i_nuc)
!4433 format(1h ,'!nucage',i4,i4,i6,a3,f10.1,i5,2f10.3,i5,f10.3)

                  DO XYR = 1 , UNXPH
                     IYRMO = (CURIYR + XYR + UHBSYR - 1) * 100 + 6
                     RYRMO = W_RYR * 100 + W_RMO
                     SYRMO = W_SYR * 100 + W_SMO
                     IF (SYRMO .LE. IYRMO .AND. RYRMO .GT. IYRMO) THEN
                        IF (XYR .EQ. 1)THEN
                           CAPDER = 1.0
                        ELSE
                           CAPDER = CAPDER * NUCDRAT(MIN(CURIYR + XYR - 1,MNUMYR),WNOWN)
                        END IF
                        ENUC_CAP(XYR,I_NUC) = ENUC_CAP(XYR,I_NUC) + WC_SUM * CAPDER
                        IF (W_CFA .GT. 0.0)THEN
                         IF (XYR .LT. UNXPH)THEN
                           CALL ENUCFAC(XYR+CURIYR-1+UHBSYR,W_CFA,N_CF)
                         ELSE
                          DO JYR = 1 , UNFPH - UNXPH + 1
                           CALL ENUCFAC(JYR+CURIYR+UNXPH-2+UHBSYR,W_CFA,N_CF)
                           N_CFY(JYR) = DBLE(N_CF)
                           N_KWY(JYR) = DBLE(1.0)
                          END DO
                           N_CF = PVV(N_CFY,ECP_D_FPH,UNFPH - UNXPH + 1,DBLE(EPDSCRT)) /  &
                                  PVV(N_KWY,ECP_D_FPH,UNFPH - UNXPH + 1,DBLE(EPDSCRT))
                         END IF
                        ELSE
                           N_CF = 0.0
                        END IF
                        ENUC_CF(XYR,I_NUC) = ENUC_CF(XYR,I_NUC) + N_CF * WC_SUM * CAPDER

                     END IF
                  END DO

                  DO XYR = 1 , UNFPH
                     IYRMO = (CURIYR + XYR + UHBSYR - 1) * 100 + 6
                     RYRMO = W_RYR * 100 + W_RMO
                     SYRMO = W_SYR * 100 + W_SMO
                     IF (SYRMO .LE. IYRMO .AND. RYRMO .GT. IYRMO) THEN
                        AGE = MIN(UNYEAR+ECP_D_FPH , CURIYR + UHBSYR + XYR - WRFURB)
                       IF (XYR .LE. UNXPH) THEN
                        ENUC_FOM(XYR,I_NUC) = ENUC_FOM(XYR,I_NUC) + (WFOMA + WGAA + W_CAPAD + UPXTRA(AGE,IECP)) * ENUC_CAP(XYR,I_NUC)
                       ELSE
                        ENUC_FOM(XYR,I_NUC) = ENUC_FOM(XYR,I_NUC) + (WFOMA + WGAA + W_CAPAD + UPXTRA(AGE,IECP)) * ENUC_CAP(UNXPH,I_NUC)
                       ENDIF
                     END IF
                  END DO
!
                  MXSHR = 0.0
                  DO TRG = 1 , UNRGNS
                     IF (MXSHR .LT. ENUC_RG(TRG,I_NUC)) THEN
                        KRG = TRG
                        MXSHR = ENUC_RG(TRG,I_NUC)
                     END IF
                  END DO
                  if (uf_dbg .gt. 0) &
                     WRITE(UF_DBG,5112) CURIYR+UHBSYR,W_IGRP,W_GRP,W_GRP2,I_NUC,W_SYR,W_RYR,KRG,JRG, &
                     ENUC_FREC(I_NUC),ENUC_ECP(I_NUC), &
                     (ENUC_FOM(XYR,I_NUC)/MAX(1.0,ENUC_CAP(XYR,I_NUC)),ENUC_CAP(XYR,I_NUC),XYR=1,UNXPH), &
                     WHRATE,W_CF,W_CFA,N_CF,ULTGEN(W_GRP),ULCAPC(W_GRP), &
                     ENUC_RVAL(I_NUC), VCST, FCST, REVS, WFOMA, W_CAPAD
 5112             FORMAT(1X,"ENUC_DATA",11(":",I5),26(":",F9.3))
               END IF
!
!              ADD PIPELINE BUILDS TO EFP'S BUILD LINKED LIST
!
               IF ((FULLCYR .EQ. UESTYR) .AND. (W_SYR .GE. EFPSYR) .AND. (WVIN .EQ. 2 .OR. WVIN .GT. 9) .AND. (WVIN .NE. 15)) CALL UPIPBLD(IRG)
!
!              Add Planned scrubber to EFP's linked list
!
               IF ((FULLCYR .EQ. UESTYR) .AND. (WVIN .EQ. 7) .AND. (W_SYR .eq. WSCBYR)) then
                  WEFPT = 8
                  WPCST = WSCBCST
                  if ((W_POST .eq. 2) .and. (WSCR_O .gt. 0.0)) then
                     WPCST = WPCST + WSCR_O
                  endif
                  write(22,111) CURIYR,WVIN,W_SYR,W_GRP,W_GRP2,WC_NP,WSCBCST,WSCR_O,WPCST
                  CALL UEXPBLD(IRG)
               ENDIF
  111          format(1x,'calling uexpbld planned scrub', 2I4,3I8,4f10.2)
            END IF      ! END OWNERSHIP BLOCK (I.E. GO TO NEXT RECORD WHEN OWNER CODE NOT VALID)

!           WRITE(18,9331) CURIRUN, CURIYR+1989, IRG, IGRP, IRECL, ITEST, ULIGRP(W_GRP), W_GRP, CYR_INDEX(1), LYR_INDEX(1), &
!              SINDX, RINDX, ULSINDX(W_GRP), ULRINDX(W_GRP), ULAGE(W_GRP), ULHRQ(W_GRP), HTRT_RESULTS(W_IGRP), &
!              ULORGN(W_GRP), ULFRGN(W_GRP), ULECPT(W_GRP), ULEFDT(W_GRP), MRSW, ULMRUN(W_GRP), (ULHTRT_ECP(W_GRP,XYR), ULSCAP_ECP(W_GRP,XYR), ULTGEN_ECP(W_GRP,XYR),XYR=1,UNXPH), ULTGEN(W_GRP), &
!              WHRATE, WHRATEA, WC_SUM, WC_WIN, W_CF, &
!              (ULHTRT_EFD(W_GRP,ISP), ULSCAP_EFD(W_GRP,ISP),ISP=1,EENSP)
!9331       FORMAT(1X,"HTRT_CRPGRP",23(":",I6),<UNXPH>(":",F21.6,":",F21.6,":",F21.6),6(":",F21.6),<EENSP>(":",F21.6,":",F21.6))
!
            OLD_REC = IRECL
            IRECL = W_NXT(IRECL)                   ! NEXT RECORD IN PLANT FILE
!
         END DO                                            ! END PLANT LOOP
!
!        END PLANT LOOP
!
         IF (EYSCAP .GT. 0.0 .OR. EYRETR .GT. 0.0) THEN
!
!           PLANT TYPE EFD, ECP, EFP
!
            IF (CYEAR_EFD .GT. 0) IEFD = CYEAR_EFD
            ULEFDT(W_GRP) = IEFD
            IF (CYEAR_ECP .GT. 0) IECP = CYEAR_ECP
            ITYP = UPTTYP(IECP)
            ULECPT(W_GRP) = IECP
            FTYPE = WEFPT
            ULEFPT(W_GRP) = FTYPE
!            ULMRUN(W_GRP) = MRSW  ! already filled above with max - don't overwrite
!
!           OWNERSHIP TYPE AND VINTAGE
!
            ULOWNT(W_GRP) = IOWN
            ULVINT(W_GRP) = IVIN3
!
!           EMM REGIONS (OWNER AND OPERATOR) AND CENSUS REGION
!
            ULIGRP(W_GRP) = W_IGRP
            RG_NUM = WNOWN
            ULORGN(W_GRP) = RG_NUM
            RG_NUM = WNOPER
            ULOPER(W_GRP) = RG_NUM
            IF (ULORGN(W_GRP) .EQ. ULOPER(W_GRP)) THEN
               ULRGNT(W_GRP) = 1
            ELSE
               ULRGNT(W_GRP) = 2
            END IF
            RG_NUM = W_CR
            ULCENS(W_GRP) = RG_NUM
            ULHGGP(W_GRP) = W_CLRG ! mercury trade group, same as coal region
            if ((W_CLRG .LT. 1) .OR. (W_CLRG .GT. NDRGG)) THEN
               write(6,*) 'error assigning ULHGGP ',W_GRP,W_CLRG
               ULHGGP(W_GRP) = 1
            endif
!
!           FUEL TYPES
!
            DO IFL = 1 , EIFPLT
               FL_NUM = WFL(IFL)
               FL_IND(IFL) = FL_NUM
               ULFUEL(IFL,W_GRP) = FL_NUM
!
!              PRIMARY FUEL REGION
!
               IF (FL_NUM .GT. 0) THEN
                  IF (UCDFLRG(FL_NUM,1) .EQ. 'CR') THEN
                     RG_NUM = W_CR
                  ELSE IF (UCDFLRG(FL_NUM,1) .EQ. 'GR') THEN
                     RG_NUM = W_GR
                  ELSE IF (UCDFLRG(FL_NUM,1) .EQ. 'NR') THEN
                     RG_NUM = WNOWN
                  ELSE IF (UCDFLRG(FL_NUM,1) .EQ. 'N2') THEN
                     RG_NUM = WNOPER
                  ELSE IF (UCDFLRG(FL_NUM,1) .EQ. 'CL') THEN
                     RG_NUM = W_CLRG
                  END IF
               END IF
               ULFLRG(IFL,W_GRP) = RG_NUM
            END DO
!
!           STORE END OF YEAR SUMMER CAPACITY BY PLANT GROUP
!
            ULCAPC(W_GRP) = EYSCAP
            ULFCST(W_GRP) = EYFCST
            ULCCST(W_GRP) = EYCCST
!
!           STORE END OF YEAR RETIREMENTS BY PLANT GROUP
!           AND UPDATE FTAB INPUT
!
            ULRETC(W_GRP) = EYRETR
         END IF
!
!        IF 1+ PLANT IS OPERATING IN CURRENT YEAR
!
         IF (ITEST .GT. 0) THEN

            ITYP = UPTTYP(IECP)
!
!           IF DISPATCHABLE PLANT GROUP
!
            IF (IEFD .LE. EFD_D_DSP) THEN
!
               ECUNIT(ECNTP) = ECUNIT(ECNTP) + WCOUNT
               ECDBID(ECNTP) = W_GRP
               ECFOWN(ECNTP) = WFOWN
               ECCR(ECNTP) = W_CR
               ECNR(ECNTP) = WNOWN
               ECOPR(ECNTP) = WNOPER
               ECLR(ECNTP) = W_CLRG
               ECAR(ECNTP) = W_CAR
               ECST(ECNTP) = W_ST
               ECNOCCS(ECNTP) = CCSCAPA

               IF (UPPCEF(WECPt) .GT. 0.0) THEN
                  WRITE(18,2346) CURIRUN, CURCALYR, CURITR, IRG, ECNTP, W_IGRP, W_GRP, WECPt, UPLNTCD(WECPT), CCSCAPA, UECP_CPEN_ADJ(WECPt)
 2346             FORMAT(1X,"ECNOCCS_INFO",8(",",I5),",",A2,2(",",F21.6))
               END IF

               IF (UPPCEF(WECPt) .GT. 0.0 .AND. CCSCAPA .LE. 0.0) THEN
                  ECNOCCS(ECNTP) = 1.0 - UECP_CPEN_ADJ(WECPt)
               END IF
               IF ((CURIYR + UHBSYR) .EQ. UESTYR .AND. (IEFD .LT. UICNU .AND. W_ST .GT. 0))THEN
                  IF (EST_FRG(W_ST) .EQ. 0)THEN
                     EST_FRG(W_ST) = FRG
                     EMAP_STFR(FRG,W_ST) = EMAP_STFR(FRG,W_ST) + 1.0
                     EMAP_STFR(FRG,EMM_D_ST + 1) = EMAP_STFR(FRG,EMM_D_ST + 1) + 1.0

!                    write(6,2345) curiyr+1989,w_st,ustnme(w_st),frg,emap_stfr(frg,w_st),emap_stfr(frg,w_s
!2345                format(1h ,'!stfr',i4,i4,a4,i4)

                  END IF
               END IF

!              DETERMINE IF UNIT IS ENTITLED TO 45Q TAX CREDIT SUBSIDY THIS YEAR

               IF (CURIYR+UHBSYR .GE. I_45Q_SYR) THEN ! .AND. WECPt .LE. ECP_D_DSP) THEN !BECCS will be > ECP_D_DSP but we want it to get credit
                  IF (UPPCEF(WECPt) .GT. 0.0) THEN
                     IF (UL_45Q_YR(W_IGRP) .GT. 0) THEN
                        IF (CURIYR+UHBSYR .GE. UL_45Q_YR(W_IGRP) .AND. CURIYR+UHBSYR .LT. UL_45Q_YR(W_IGRP) + I_45Q_Duration) THEN
                           EC_45Q(ECNTP) = 1
                        END IF
                     END IF
                  END IF
               END IF

!              ACCUMULATE PLANNED NUCLEAR GENERATION FOR 111D (ORIGINAL RULE ONLY -- IN FINAL RULE INCREMENTAL GEN ACCUMULATED EARLIER)
!
               IF (CO2_AFFYR .LE. 0 .AND. WVIN .EQ. 2 .AND. (IECP .EQ. WICN .OR. IECP .EQ. WIAN.OR. IECP .EQ. WISM) .AND. (CURIYR + UHBSYR) .EQ. W_SYR)THEN
                  IF (W_ST .GT. 0)THEN
                     DO XYR = CURIYR , MNUMYR + ECP_D_XPH
                        IF (CO2_PLTSW(IECP) .LT. 1.0)THEN
                           NUCPLNF(EST_FRG(W_ST),XYR) = NUCPLNF(EST_FRG(W_ST),XYR) + (WC_SUM / 1000.0) * W_CF * 8.760 * (1.0 - CO2_PLTSW(IECP))
                           NUCPLNN(IRG,XYR) = NUCPLNN(IRG,XYR) + (WC_SUM / 1000.0) * W_CF * 8.760 * (1.0 - CO2_PLTSW(IECP))
                        END IF
                     END DO
                  ELSE
      write(6,2345) curiyr+1989,irg,w_st,ustnme(w_st),uplntcd(iecp),wc_sum
 2345 format(1h ,'!stnu',i4,i4,a4,a4,f10.1)
                  END IF
               END IF

               FLRG = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)

               ERTOMF(IEFD,IOWN) = ERTOMF(IEFD,IOWN) + CAPFOM * 0.001
               ERTOMX(IEFD,IOWN) = ERTOMX(IEFD,IOWN) + CAPXTR * 0.001
!
               AVGCF = MAX(0.001 , AVGCF / GCAP)
               IF (UPTTYP(IECP) .LE. NW_COAL) THEN
                  AVGCF = MIN(AVGCF,0.900)
               ELSE
                  AVGCF = MIN(AVGCF,0.998)
               END IF
!
!              CALIBRATE HISTORICAL CAPACITY FACTORS FOR COAL-FIRED PLANTS
!
               DO ISP = 1 , EENSP
                  ECCAP(ECNTP,ISP) = GSCAP(ISP)
                  ECCOPM(ECNTP,ISP) = 0.0
                  ECFNOX(ECNTP,ISP) = TNOX_R(ISP) / GCAP
                  DO INOX = 1 , NOX_GRP
                     ECFNOXC(INOX,ECNTP,ISP) = TNOX_RC(INOX,ISP) / GCAP
                  END DO
               END DO
!
               TFOR = WFOR(IEFD)
               TPMR = WPMR(IEFD)
!
!              Make Sure the Availability Rate is consistent with FOR and PMR -
!              if not eliminate LFR and move PMR and FOR by equal factors so that (1 - FOR) * (1 - PMR) = WMXCP
!
               IF (WMXCP(IEFD) .GT. (1.0 - TFOR) * (1.0 - TPMR)) THEN
                  FCTR = (WMXCP(IEFD) / ((1.0 - TFOR) * (1.0 - TPMR) * (1.0 - 0.032))) ** DBLE(0.5)
                  TFOR = 1.0 - (1.0 - TFOR) * FCTR
                  TPMR = 1.0 - (1.0 - TPMR) * FCTR
                  WRITE(6,7313) CURIYR,IRG,IEFD,WMXCP(IEFD),WFOR(IEFD),WPMR(IEFD),TFOR,TPMR
 7313             FORMAT(1X,"FOR_and_PMR_Changed_to_be_Consistent_with_MX_CF",3(":",I4),5(":",F6.3))
                  WFOR(IEFD) = TFOR
                  WPMR(IEFD) = TPMR
               END IF
               WGHT1 = 1.0
               IF ((DBLE(UCFEYR(IEFD)) - DBLE(UCFSYR(IEFD))) .NE. 0.0) &
               WGHT1 = MAX( MIN( ((DBLE(UCFEYR(IEFD)) - DBLE(IYR)) / (DBLE(UCFEYR(IEFD)) - DBLE(UCFSYR(IEFD)))) , DBLE(1.0)) , DBLE(0.0))
               WGHT2 = DBLE(1.0) - WGHT1
!
!              FOR MUST RUN UNITS USE AVERAGE ANNUAL CAPACITY FACTOR OTHERWISE PHASE AVERAGE ANNUAL CAPACITY FACTOR to AVAILABILTY
!
               IF (MRSW .GT. 0) THEN
                  ECNMR = ECNMR + 1
                  ECMRUN(ECNTP) = 1
               ELSE
                  ECMRUN(ECNTP) = 0
               END IF
!
               IF (AVGCF .LT. WMXCP(IEFD) .AND. (WVIN .NE. 2 .AND. WVIN .NE. 3)) THEN
                  MAXCF = WGHT1 * AVGCF + WGHT2 * WMXCP(IEFD)
               ELSE
                  MAXCF = AVGCF
               END IF
               IF (UPTTYP(IECP) .LE. NW_COAL) THEN
                  MAXCF = MIN(MAX(MAXCF,0.001),0.900)
               ELSE
                  MAXCF = MIN(MAX(MAXCF,0.001),0.998)
               END IF
               IF ( EPPLCD(IEFD) .EQ. 'GTH' .OR. EPPLCD(IEFD) .EQ. 'BMS') MAXCF = AVGCF
!
!              Make PMR, FOR and implied LFR Consistent with MAXCF,
!              Unless LFR Exceeds maximum specified then Use Max LFR and Split difference between FOR and PMR
!
               TLFR = 1.0 - (WMXCP(IEFD) / ((1.0 - WFOR(IEFD)) * (1.0 - WPMR(IEFD))))
               FCTR = (MAXCF / WMXCP(IEFD)) ** (DBLE(1.0) / DBLE(3.0))
               TLFR = DBLE(1.0) - FCTR * (DBLE(1.0) - TLFR)
               IF (TLFR .LE. MXLFR .AND. TLFR .GE. 0.0) THEN
                  TFOR = DBLE(1.0) - FCTR * (DBLE(1.0) - TFOR)
                  TPMR = DBLE(1.0) - FCTR * (DBLE(1.0) - TPMR)
               ELSE IF (TLFR .LT. 0.0) THEN
                  FCTR = (MAXCF / WMXCP(IEFD)) ** (DBLE(1.0) / DBLE(2.0))
                  TFOR = DBLE(1.0) - FCTR * (DBLE(1.0) - TFOR)
                  TPMR = DBLE(1.0) - FCTR * (DBLE(1.0) - TPMR)
                  TLFR = 0.0
               ELSE
                  TLFR = MXLFR
                  FCTR = (MAXCF / ((1.0 - WFOR(IEFD)) * (1.0 - WPMR(IEFD)) * (1.0 - TLFR))) ** 0.5
                  TFOR = DBLE(1.0) - FCTR * (DBLE(1.0) - TFOR)
                  TPMR = DBLE(1.0) - FCTR * (DBLE(1.0) - TPMR)
               END IF
!
               IF (TLFR .LT. 0.0 .OR. TFOR .LT. 0.0 .OR. TPMR .LT. 0.0) THEN
                  TLFR = 1.0 - (MAXCF ** (1.0 / 3.0))
                  TFOR = 1.0 - (MAXCF ** (1.0 / 3.0))
                  TPMR = 1.0 - (MAXCF ** (1.0 / 3.0))
               END IF
!
               IF (COAL .GT. 0 .AND. IYR .EQ. 1995) THEN
                  WRITE(22,7113) IYR,IRG,ECNTP,IECP,IEFD,MAXCF,AVGCF,WMXCP(IEFD),FCTR,TFOR,TPMR
 7113             FORMAT(1X,'CF HUH ',5(":",I5),6(":",F7.3))
                  CL$GEN = CL$GEN + GCAP * MAXCF * 8.76
                  CL$CAP = CL$CAP + GCAP
               END IF
!
               IF (IOWN .EQ. 3 .AND. IVIN3 .EQ. 1) THEN
                  EOUIPP = EOUIPP + (GCAP * (1.0 - W_GRID) * AVGCF * 0.001)
                  EOUNCP = EOUNCP + (GCAP * (1.0 - W_GRID) * AVGCF * 0.001)
                  UOUNCP(IRG,CURIYR) = UOUNCP(IRG,CURIYR) + (GCAP * (1.0 - W_GRID) * AVGCF * 0.001)
                  IF (CURIYR .EQ. 22 .AND. UF_DBG .GT. 0) WRITE(UF_DBG,3313) CURIYR+UHBSYR, &
                     JRG,W_SYR,W_RYR,W_GRP,W_GRP2,WC_SUM,W_GRID,EOUIPP,AVGCF,MAXCF
 3313             FORMAT(1X,"EOUIPP",6(":",I5),5(":",F15.6))
               END IF
!
               IF (IOWN .EQ. 4 .AND. IVIN3 .EQ. 1) THEN
                  EOUNT = EOUNT + (GCAP * (1.0 - W_GRID) * AVGCF * 0.001)
                  EOUCCP = EOUCCP + (GCAP * (1.0 - W_GRID) * AVGCF * 0.001)
                  UOUCCP(IRG,CURIYR) = UOUCCP(IRG,CURIYR) + (GCAP * (1.0 - W_GRID) * AVGCF * 0.001)
                  IF (CURIYR .EQ. 22 .AND. UF_DBG .GT. 0) WRITE(UF_DBG,3314) CURIYR+UHBSYR, &
                     JRG,W_SYR,W_RYR,W_GRP,W_GRP2,WC_SUM,W_GRID,EOUIPP,AVGCF,MAXCF
 3314             FORMAT(1X,"EOUNT ",6(":",I5),5(":",F15.6))
               END IF
!
               ECMXCP(ECNTP) = INT(MAXCF / EFACTR)
               ECMRUNCF(W_GRP) = INT(AVGCF / EFACTR)
               ECPMR(W_GRP) = REAL(EFACTR * INT(TPMR / EFACTR))
               ECFOR(W_GRP) = REAL(EFACTR * INT(TFOR / EFACTR))
               ECLFR(W_GRP) = REAL(EFACTR * INT(TLFR / EFACTR))

!              For All Units Add to capacity sent to ReStore

                  STORAGE_RGN(W_GRP) = JRG
                  STORAGE_ECPn(W_GRP) = IECP
                  STORAGE_ECPc(W_GRP) = UPLNTCD(IECP)
                  JNT = 0

                  DO ISP = 1 , EENSP
                     
                       IF (WVIN .EQ. 2 .AND. W_SYR .EQ. CURCALYR) THEN ! new planned build
                             STORAGE_CAP(W_GRP,ISP) = ECCAP(ECNTP,ISP) * 0.001 * (1.0 - ECFOR(W_GRP))*(1.0 - ECPMR(W_GRP))
                            STORAGE_GEN(W_GRP,ISP) = ECCAP(ECNTP,ISP) * 0.001 * INT(AVGCF / EFACTR) * EFACTR * EETIME(ISP) * 0.001
                             STORAGE_CST(W_GRP,ISP) = (0.001 * WHRATE * 2.0) + WVOMA
                       ELSEIF (W_SYR .EQ. CURCALYR) THEN ! new unplanned build, costs already filled in uecp
                           STORAGE_CAP(W_GRP,ISP) = ECCAP(ECNTP,ISP) * 0.001 * (1.0 - ECFOR(W_GRP))*(1.0 - ECPMR(W_GRP))
                       ELSE ! rest, use PMR from previous year's EFD decision
                           STORAGE_CAP(W_GRP,ISP) = ECCAP(ECNTP,ISP) * 0.001 * (1.0 - ECFOR(W_GRP))*(1.0 - PM_FRAC(W_GRP,ISP))
                       ENDIF
                       
					  IF (STORAGE_CAP(W_GRP,ISP) .EQ. 0.0) THEN ! reset if retired
						  STORAGE_GEN(W_GRP,ISP) = 0.0
                   		  STORAGE_CST(W_GRP,ISP) = 0.0
					  ENDIF

                     WRITE(18,3962) CURIRUN, CURCALYR, CURITR, JRG, ISP, IECP, JNT, WVIN, ECNTP, W_GRP, IEFD, UPLNTCD(IECP), STORAGE_CAP(W_GRP,ISP), ECCAP(ECNTP,ISP), WHRATE, WVOMA, &
                        STORAGE_GEN(W_GRP,ISP), EETIME(ISP), STORAGE_CST(W_GRP,ISP), PM_FRAC(W_GRP,ISP), ECFOR(W_GRP), ECPMR(W_GRP)

                  END DO
!
               IF (MRSW .GT. 0) THEN
                  IF (UCPDSPIS(IECP) .GT. 0) THEN
                     DO ISP = 1 , EENSP
                        MUSTRUN(IECP,IRG) = MUSTRUN(IECP,IRG) + ECCAP(ECNTP,ISP) * INT(AVGCF / EFACTR) * EFACTR * EETIME(ISP) * 0.001

!                       WRITE(6,4371) CURIRUN, CURIYR+1989, CURIYR+YEAR+1988, MRSW, ECNTP, ISP, IECP, IRG, MUSTRUN(IECP,IRG), ECCAP(ECNTP,ISP), INT(AVGCF / EFACTR) * EFACTR, EETIME(ISP) * 0.001

                     END DO
                  END IF
               END IF

!STEOBM    calculate baseline geothermal generation for benchmarking
                  IF (IECP .EQ. WIGT )  THEN
                    DO ISP = 1, EENSP
                      BSGTGEN = BSGTGEN + (ECCAP(ECNTP,ISP) * 0.001) * MAXCF * EETIME(ISP) * 0.001
                      EXSGEN(IECP,IRG) = EXSGEN(IECP,IRG) + (ECCAP(ECNTP,ISP) * 0.001) * MAXCF * EETIME(ISP) * 0.001
                      EXSGEN(IECP,MNUMNR) = EXSGEN(IECP,MNUMNR) + (ECCAP(ECNTP,ISP) * 0.001) * MAXCF * EETIME(ISP) * 0.001
                    ENDDO
                  ENDIF
!
!              CALL ENUCFAC TO DETERMINE CAPACITY FACTOR FOR CURRENTLY OPERATING PLANTS
!
               IF(IEFD .EQ. UICNU .OR. IEFD .EQ. UIANC) THEN
                  IF(AVGCF .GT. 0.0)THEN
                     CALL ENUCFAC(IYR,AVGCF,NUC_CFC)
                  ELSE
                     NUC_CFC = 0.0
                  END IF
!
!                 USE NUCLEAR CAPACITY FACTOR ASSUMPTIONS AND SCALE OUTAGE RATES TO BE CONSISTENT
!
                  NUC_AVL = (1.0 - WPMR(IEFD)) * (1.0 - WFOR(IEFD))
                  MAXCF = NUC_CFC
                  TPMR = 1.0 - SQRT(NUC_CFC/NUC_AVL) * (1.0 - WPMR(IEFD))
                  TFOR = 1.0 - SQRT(NUC_CFC/NUC_AVL) * (1.0 - WFOR(IEFD))
                  TLFR = 0.0
!
                  IF (TLFR .LT. 0.0 .OR. TFOR .LT. 0.0 .OR. TPMR .LE. 0) THEN
                     TFOR = 1.0 - (MAXCF ** (1.0 / 2.0))
                     TPMR = 1.0 - (MAXCF ** (1.0 / 2.0))
                  END IF
!
                  ECMXCP(ECNTP) = INT(MAXCF / EFACTR)
                  ECPMR(W_GRP) = REAL(EFACTR * INT(TPMR / EFACTR))
                  ECFOR(W_GRP) = REAL(EFACTR * INT(TFOR / EFACTR))
                  ECLFR(W_GRP) = REAL(EFACTR * INT(TLFR / EFACTR))

!STEOBM    calculate baseline nuclear generation for benchmarking
                  IF (IECP .EQ. WICN .OR. IECP .EQ. WIAN .OR. IECP .EQ. WISM)  THEN
                    DO ISP = 1, EENSP
                      BSNCGEN = BSNCGEN + (ECCAP(ECNTP,ISP) * 0.001) * NUC_CFC * EETIME(ISP) * 0.001
                    ENDDO
                  ENDIF
               END IF
!
!              IF (W_IGRP .EQ. 5375) THEN
!                 WRITE(6,6313) CURIYR+UHBSYR,CURITR,W_IGRP,W_GRP,W_GRP2,WVIN,W_SYR,W_SMO,W_RYR,W_RMO,W_CF,W_CFA,AVGCF,NUC_CFC,GCAP
!6313             FORMAT(1X,"DAVIS-BESSE",10(":",I5),5(":",F12.3))
!              END IF
!
!              Collect Info to Calculate Average Max Capacity Factor for Dispatchable Capacity
!
               DO ISP = 1 , EENSP
                  MAXCF_TOT(IECP) = MAXCF_TOT(IECP) + DBLE(ECMXCP(ECNTP)) * EFACTR * EETIME(ISP) / 8760.0 * ECCAP(ECNTP,ISP)
                  MAXCF_WGT(IECP) = MAXCF_WGT(IECP) + EETIME(ISP) / 8760.0 * ECCAP(ECNTP,ISP)
                  DO JECP = 1 , ECP_D_CAP
                     IF (ECPt_MAP(IECP,JECP) .EQ. 1) THEN
                        MAXCF_TOT_2(JECP) = MAXCF_TOT_2(JECP) + DBLE(ECMXCP(ECNTP)) * EFACTR * EETIME(ISP) / 8760.0 * ECCAP(ECNTP,ISP)
                        MAXCF_WGT_2(JECP) = MAXCF_WGT_2(JECP) + EETIME(ISP) / 8760.0 * ECCAP(ECNTP,ISP)
                     END IF
                  END DO

                  IF (IECP .EQ. 21 .AND. IRG .EQ. 5) THEN
                     WRITE(18,7391) CURIYR+UHBSYR,CURITR,ISP,IRG,ITYP,IECP,IEFD,ECNTP,EETIME(ISP),ECCAP(ECNTP,ISP),WMXCP(IEFD),WFOR(IEFD),WPMR(IEFD), &
                        DBLE(ECMXCP(ECNTP))*EFACTR, ECFOR(W_GRP), ECPMR(W_GRP), &
                        MAXCF_WGT(IECP),MAXCF_TOT(IECP),UPMCF(IECP),UPFORT(IECP),UPPMRT(IECP), &
                        MAXCF_WGT_2(IECP),MAXCF_TOT_2(IECP)
 7391                FORMAT(1X,"MACCF_TOT",8(":",I5),15(":",E10.3))
                  END IF
               END DO
!
               ECASTS(ECNTP) = WASTS(IEFD)
               ECTECP(ECNTP) = IECP
               ECVIN(ECNTP) = IVIN3
               IF (UPTTYP(IECP) .LE. NW_COAL) THEN
                  ICL = UCL_CGRP2(W_GRP)
                  IF (ICL .LE. 0) THEN
                     ICL = MAP_TO_COAL_ID(W_IGRP)
                     WRITE(6,2394) CURIYR+UHBSYR,CURITR,W_IGRP,W_GRP,ICL,UCL_CGRP2(W_GRP)
 2394                FORMAT(1X,"BAD_CL_GRP_6303",6(":",I6))
                     IF (ICL .GT. 0) UCL_CGRP2(W_GRP) = ICL
                  END IF
                  IF (ICL .GT. 0) THEN
                     IF (IECP .NE. EMM_CL_ECPt(ICL,CURIYR)) THEN
                        WRITE(6,2393) CURIYR+UHBSYR,CURITR,ICL,IECP,EMM_CL_ECPt(ICL,CURIYR),-1.0
 2393                   FORMAT(1X,"EMM_CL_ECPt_OOPS",5(":",I4),":",F12.3)
                     END IF
                  END IF
               END IF
!
               IF (ECASTS(ECNTP) .EQ. 0) THEN
                  WRITE(6,*) ' ECASTS ERROR:IRG,IGRP,W_SYR,ECNTP',',EHNTP,IEFD,','IECP,ECASTS=',IRG,IGRP,W_SYR,ECNTP,EHNTP,IEFD,IECP,ECASTS(ECNTP)
                  STOP
               END IF
!
               ECALLW(ECNTP) = 0.0
               EISO2(ECNTP,1) = 1
               IF (IECP .LE. ECP_D_DSP) THEN
                  ECSCRB(ECNTP) = INT(UPPSEF(IECP) * (1.0 / EFACTR))
               ELSE
                  ECSCRB(ECNTP) = INT((CAPSCR/GCAP) * (1.0 / EFACTR / 100.0))
               END IF

               ECSEQS(ECNTP) = WSEQEF
!
!              DETERMINE FUEL INDICES FOR PLANT GROUP:
!
               TSTFSHR = 0.0
               DO IFL = 1 , EIFPLT
                  FL_NUM = WFL(IFL)
                  FL_IND(IFL) = FL_NUM
                  ECFLTP(ECNTP,IFL) = FL_NUM
!
!                 Calculate Variable Non-Fuel O&M and Heatrate
!
                  ECOMR(ECNTP,IFL) = CAPVOM / GCAP
                  ECGSUB(ECNTP) = (CAPGSUB + CAPCSUB) / GCAP
!
!                 Determine Fuel Regions
!
                  IF (FL_NUM .GT. 0) THEN
!
!                    Calculate Maximum Fuel Share by Fuel Type
!
                     ECMFSH(ECNTP,IFL) = MXFSHR(FL_NUM) / GCAP
                     TSTFSHR = TSTFSHR + ECMFSH(ECNTP,IFL)
!
!                    GET GAS REGION FOR ALL TYPES TO DETERMINE GAS USE IN COAL PLANTS
!
                     ECGR(ECNTP) = W_GR
                     DO IFLRG = 1, UNFLRG(FL_NUM)
                        IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'CR') THEN
                           RG_NUM = W_CR
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'GR') THEN
                           RG_NUM = W_GR
                           ECGR(ECNTP) = RG_NUM
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'NR') THEN
                           RG_NUM = WNOWN
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'N2') THEN
                           RG_NUM = WNOPER
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'CL') THEN
                           RG_NUM = W_CLRG
                        END IF
!
                        ECFLRG(ECNTP,IFL,IFLRG) = RG_NUM
!
                        IF ((UF_DBG .GT. 0) .AND. (RG_NUM .EQ. 0)) THEN
                           WRITE(UF_DBG,2030) IRG,IGRP,ECNTP,IEFD
                           WRITE(UF_DBG,2031) IFL,IFLRG, &
                              UCDFLRG(FL_NUM,IFLRG)
                        END IF
                     END DO
                  END IF
 2030             FORMAT(1X,'GRPGRP:IRG,IGRP,ECNTP,IEFD:',4(1X,I6))
 2031             FORMAT(1X,'GRPGRP:IFL,IFLRG,UCDFLRG:',2(1X,I6),A2)
               END DO                                               ! FUEL LOOP
!
!              IF NO FUEL CONSUMPTION, RESET BIOMASS COFIRING FUEL SHARE TO ZERO
!
               IF ( TSTFSHR .EQ. DBLE(0.0) ) THEN
                  DO IFL = 1 , EIFPLT
                     IF ((IEFD .NE. UIBMS .AND. IEFD .NE. UIBIG) .AND.  & ! Not Biomass plant (WD or BECCS) but using WD fuel - cofiring
                        FL_IND(IFL) .EQ. UIWD) THEN
                        ECMFSH(ECNTP,IFL) = 0.0
                     ELSE
                        ECMFSH(ECNTP,IFL) = 1.0
                     END IF
                  END DO
               END IF
!
!              IDENTIFY PHASE 1 UNITS
!
!              IF ( WPHS1 .EQ. 1 ) THEN
!                 ECPHS1(ECNTP) = 1
!              ELSE
!                 ECPHS1(ECNTP) = 0
!              END IF
!
!              ASSIGN NUMERIC BOILER TYPE TO EACH PLANT GROUP
!
               ECBTP(ECNTP) = 0
!              DO IBTP = 1 , NUM_BTP
!                 IF ((W_BTP .EQ. BTP_FTCD(IBTP)) .AND.  &
!                    (W_BTM .EQ. BTP_BTCD(IBTP))) ECBTP(ECNTP) = IBTP
!              END DO
!              IF (ECBTP(ECNTP) .EQ. 0) THEN
!                 WRITE(6,6395) CURIYR,IRG,IGRP,IEFD,W_BTP,W_BTM
!6395             FORMAT(1X,"ECBTP",4I6,1X,A2,1X,A1)
!                 ECBTP(ECNTP) = 1
!              END IF
!
!              ASSIGN NUMERIC NOX CONTROL TYPE TO EACH PLANT GROUP
!
               ECNCT(ECNTP) = 0
!              DO INCT = 1 , NUM_NCT
!                 IF (W_NCT .EQ. NCT_CODE(INCT)) ECNCT(ECNTP) = INCT
!              END DO
!              IF (ECNCT(ECNTP) .EQ. 0) THEN
!                 WRITE(6,6396) CURIYR,IRG,IGRP,IEFD,W_NCT
!6396             FORMAT(1X,"ECNCT",4I6,1X,A2)
!                 ECNCT(ECNTP) = 1
!              END IF
               
               !this is a new array to hold CTN coal cofiring (w gas) capacity by fuel region and EFD fuel type
               IF (WEFDT .EQ. UICTN .AND. WFL(2) .GT. 0) THEN    
                    CPCTN(FLRG, WFL(2))  = CPCTN(FLRG, WFL(2)) + WC_SUM 
                    CPCTNTOT(FLRG) =CPCTNTOT(FLRG) + WC_SUM
               ENDIF
               
               IF (UF_DBG .GT. 0) THEN
                  INOX = 0
                  WRITE(UF_DBG,9311) CURIYR+UHBSYR,CURITR,W_IGRP,W_GRP,ECNTP,IRG,WSTATE,WC_SUM,GCAP,INOX,(ECFNOX(ECNTP,ISP),ISP=1,EENSP)
                  DO INOX = 1 , NOX_GRP
                     WRITE(UF_DBG,9311) CURIYR+UHBSYR,CURITR,W_IGRP,W_GRP,ECNTP,IRG,WSTATE,WC_SUM,GCAP,INOX,(ECFNOXC(INOX,ECNTP,ISP),ISP=1,EENSP)
                  END DO
               END IF
 9311          FORMAT(1X,"FNOX",6(":",I5),":",A2,2(":",F6.1),":",I2,6(":",F9.6))
               ECNTP = ECNTP + 1
!
!           IF NON-DISPATCHABLE PLANT GROUP
!
            ELSEIF (IEFD .LE. ( EFD_D_DSP + EFD_D_RNW ) ) THEN
               EHUNIT(EHNTP) = EHUNIT(EHNTP) + WCOUNT
               EHDBID(EHNTP) = W_GRP
               EHFOWN(EHNTP) = WFOWN
               EHCR(EHNTP) = W_CR
               EHNR(EHNTP) = WNOWN
               EHGR(EHNTP) = W_GR
               EHLR(EHNTP) = W_CLRG
               EHAR(EHNTP) = W_CAR
               EHST(EHNTP) = W_ST
       if (w_car .le. 0)write(6,1232) curiyr+1989,irg,wstate,uplntcd(iecp),wvin,w_grp,wc_sum
 1232  format(1h ,'!wcar0',i4,i3,a3,a3,i3,i7,f10.0)
       
               
               INTERMIT = UCPINTIS(IECP)
!
               MAXCF = MAX(0.0 , AVGCF / GCAP)
!
               ERTOMF(IEFD,IOWN) = ERTOMF(IEFD,IOWN) + CAPFOM * 0.001
               ERTOMX(IEFD,IOWN) = ERTOMX(IEFD,IOWN) + CAPXTR * 0.001
               IF (IOWN .EQ. 3 .AND. IVIN3 .EQ. 1) THEN
                  EOUIPP = EOUIPP + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
                  EOUNCP = EOUNCP + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
                  UOUNCP(IRG,CURIYR) = UOUNCP(IRG,CURIYR) + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
               END IF
               IF (IOWN .EQ. 4 .AND. IVIN3 .EQ. 1) THEN
                  EOUNT = EOUNT + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
                  EOUCCP = EOUCCP + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
                  UOUCCP(IRG,CURIYR) = UOUCCP(IRG,CURIYR) + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
               END IF
               DO ISP = 1 , EENSP
                  EHCAP(EHNTP,ISP) = GSCAP(ISP)
                  IF (CFCAP(ISP) .GT. 0.0) THEN
                       EHHYCF(EHNTP,ISP) = MIN(999,INT((CAPCF(ISP) / CFCAP(ISP)) / EFACTR ))
                  ELSE
                     EHHYCF(EHNTP,ISP) = 0.0
                  END IF
                  write(22,'(a,8i6,F21.6)') 'DBG EHHYCF  , ',CURIYR,CURITR,IRG, IEFD, W_GRP, EHNTP, ISP, EHHYCF(EHNTP,ISP),EHCAP(EHNTP,ISP)
                  ELSO2P(ISP,W_GRP) = 0.0
                  EHFNOX(EHNTP,ISP) = TNOX_R(ISP) / GCAP
                  DO INOX = 1 , NOX_GRP
                     EHFNOXC(INOX,EHNTP,ISP) = TNOX_RC(INOX,ISP) / GCAP
                  END DO
               END DO

!              For All Units Add to capacity sent to ReStore

                  STORAGE_RGN(W_GRP) = JRG
                  STORAGE_ECPn(W_GRP) = IECP
                  STORAGE_ECPc(W_GRP) = UPLNTCD(IECP)

                  DO ISP = 1 , EENSP
                     CAPFAC = EHHYCF(EHNTP,ISP) * 0.001

                     JNT = UCPINTIS(IECP)
                     IF (JNT .GT. 0 ) THEN
                        IF (STO_CFefdSEA(JNT,ISP,JRG) .GT. 0.0 .AND. IECP .NE. WIPT) THEN
                           STORAGE_CAP(W_GRP,ISP) = EHCAP(EHNTP,ISP) * 0.001 * (CAPFAC / STO_CFefdSEA(JNT,ISP,JRG))
                        ELSE
                           STORAGE_CAP(W_GRP,ISP) = EHCAP(EHNTP,ISP) * 0.001
                        END IF

                        WRITE(18,3962) CURIRUN, CURCALYR, CURITR, JRG, ISP, IECP, JNT, WVIN, EHNTP, W_GRP, IEFD, UPLNTCD(IECP), STORAGE_CAP(W_GRP,ISP), EHCAP(EHNTP,ISP), CAPFAC, STO_CFefdSEA(JNT,ISP,JRG), &
                           STORAGE_GEN(W_GRP,ISP), EETIME(ISP), STORAGE_CST(W_GRP,ISP)
 3962                   FORMAT(1X,"UDAT_STORAGE_CAP",11(",",I6),",",A2,10(",",F21.6))

                     ELSE
                        IF (IECP .EQ. WIDS) THEN
                           STORAGE_CAP(W_GRP,ISP) = EHCAP(EHNTP,ISP) * 0.001
                        ELSE
                           STORAGE_CAP(W_GRP,ISP) = EHCAP(EHNTP,ISP) * 0.001 * (1.0 - WFOR(IEFD))
                           IF (WVIN .EQ. 2 .AND. W_SYR .EQ. CURCALYR) THEN !if planned, CST hasn't been filled yet
                              STORAGE_CST(W_GRP,ISP) = WVOMA
                           ENDIF
                        ENDIF

                        WRITE(18,3962) CURIRUN, CURCALYR, CURITR, JRG, ISP, IECP, 0, WVIN, EHNTP, W_GRP, IEFD, UPLNTCD(IECP), STORAGE_CAP(W_GRP,ISP), EHCAP(EHNTP,ISP), CAPFAC, 0.0, &
                           STORAGE_GEN(W_GRP,ISP), EETIME(ISP), STORAGE_CST(W_GRP,ISP)

                     ENDIF

                     IF (STORAGE_CAP(W_GRP,ISP) .GT. 0 )  THEN 
                         IF (IECP .GE. WIWN) THEN
                          STORAGE_GEN(W_GRP,ISP) = EHCAP(EHNTP,ISP) * 0.001 * CAPFAC * EETIME(ISP) * 0.001
					 
					 IF (WVIN .EQ. 15 .AND. IECP .EQ. WIPV) THEN ! DPV is first in dispatch order
						STORAGE_CST(W_GRP,ISP) = 0.0001
					 ELSE
						STORAGE_CST(W_GRP,ISP) = 0.001
                       ENDIF
                       ENDIF
                     ELSE
                         STORAGE_GEN(W_GRP,ISP) = 0
                         STORAGE_CST(W_GRP,ISP) = 0
                         
                      ENDIF

                  END DO

               EHVOMR(EHNTP) = CAPVOM / GCAP
               EHGSUB(EHNTP) = (CAPGSUB + CAPCSUB) / GCAP
               EHHYTP(EHNTP) = WHYTP(IEFD)
               EHTECP(EHNTP) = IECP
               EHVIN(EHNTP) = IVIN3
               TSTFSHR = 0.0
               DO IFL = 1 , EIFPLT
                  FL_NUM = WFL(IFL)
                  EHFLTP(EHNTP,IFL) = FL_NUM
!
!                 Determine Fuel Regions and Fuel Shares for Renewable Units
!
                  IF (FL_NUM .GT. 0) THEN
                     EHMFSH(EHNTP,IFL) = MXFSHR(FL_NUM) / GCAP
                     TSTFSHR = TSTFSHR + EHMFSH(EHNTP,IFL)
                     DO IFLRG = 1, UNFLRG(FL_NUM)
                        IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'CR') THEN
                           RG_NUM = W_CR
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'GR') THEN
                           RG_NUM = W_GR
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'NR') THEN
                           RG_NUM = WNOWN
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'N2') THEN
                           RG_NUM = WNOPER
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'CL') THEN
                           RG_NUM = W_CLRG
                        END IF
                        EHFLRG(EHNTP,IFL,IFLRG) = RG_NUM
                     END DO
                  END IF
                  IF ( TSTFSHR .EQ. DBLE(0.0) ) THEN
                     EHMFSH(EHNTP,1) = 1.0
                  END IF
               END DO
               IF (UF_DBG .GT. 0) THEN
                  INOX = 0
                  WRITE(UF_DBG,9311) CURIYR+UHBSYR,CURITR,W_IGRP,W_GRP,EHNTP,IRG,WSTATE,WC_SUM,GCAP,INOX,(EHFNOX(EHNTP,ISP),ISP=1,EENSP)
                  DO INOX = 1 , NOX_GRP
                     WRITE(UF_DBG,9311) CURIYR+UHBSYR,CURITR,W_IGRP,W_GRP,EHNTP,IRG,WSTATE,WC_SUM,GCAP,INOX,(EHFNOXC(INOX,EHNTP,ISP),ISP=1,EENSP)
                  END DO
               END IF
!
               EHNTP = EHNTP + 1
!
!           IF DISTRIBUTED GENERATION PLANT GROUP
!
            ELSE
               EDUNIT(EDNTP) = EDUNIT(EDNTP) + WCOUNT
               EDDBID(EDNTP) = W_GRP
               EDFOWN(EDNTP) = WFOWN
               EDCR(EDNTP) = W_CR
               EDNR(EDNTP) = WNOWN
               EDLR(EDNTP) = W_CLRG
               EDAR(EDNTP) = W_CAR
               EDST(EDNTP) = W_ST
               ERTOMF(IEFD,IOWN) = ERTOMF(IEFD,IOWN) + CAPFOM * 0.001
               ERTOMX(IEFD,IOWN) = ERTOMX(IEFD,IOWN) + CAPXTR * 0.001
               AVGCF = AVGCF / GCAP
!
!              FOR MUST RUN UNITS USE AVERAGE ANNUAL CAPACITY FACTOR
!              FROM PLANT FILE ALLOCATE ALL OUTAGES TO FORCED OUTAGES,
!              I.E. DERATE EVENLY OVER FULL YEAR
!
               IF (MRSW .GT. 0) THEN
                  EDNMR = EDNMR + 1
                  EDMRUN(EDNTP) = 1
                  MAXCF = AVGCF
                  TFOR = 1.0 - MAXCF
               ELSE
                  MAXCF = WMXCP(IEFD)
                  EDMRUN(EDNTP) = 0
               END IF
               IF (IOWN .EQ. 3 .AND. IVIN3 .EQ. 1) THEN
                  EOUIPP = EOUIPP + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
                  EOUNCP = EOUNCP + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
                  UOUNCP(IRG,CURIYR) = UOUNCP(IRG,CURIYR) + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
               END IF
               IF (IOWN .EQ. 4 .AND. IVIN3 .EQ. 1) THEN
                  EOUNT = EOUNT + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
                  EOUCCP = EOUCCP + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
                  UOUCCP(IRG,CURIYR) = UOUCCP(IRG,CURIYR) + (GCAP * (1.0 - W_GRID) * MAXCF * 0.001)
               END IF

               DO ISP = 1 , EENSP
                  EDCAP(EDNTP,ISP) = GSCAP(ISP)
                  ELSO2P(ISP,W_GRP) = 0.0
                  EDFNOX(EDNTP,ISP) = TNOX_R(ISP) / GCAP
                  DO INOX = 1 , NOX_GRP
                     EDFNOXC(INOX,EDNTP,ISP) = TNOX_RC(INOX,ISP) / GCAP
                  END DO
               END DO
!
               TFOR = WFOR(IEFD)
               TPMR = WPMR(IEFD)
               IF (WMXCP(IEFD) .GT. (1.0 - TFOR) * (1.0 - TPMR)) THEN
                  FCTR = (WMXCP(IEFD) / ((1.0 - TFOR) * (1.0 - TPMR) * (1.0 - 0.032))) ** DBLE(0.5)
                  TFOR = 1.0 - (1.0 - TFOR) * FCTR
                  TPMR = 1.0 - (1.0 - TPMR) * FCTR
                  WRITE(6,7313) CURIYR,IRG,IEFD,WMXCP(IEFD),WFOR(IEFD),WPMR(IEFD),TFOR,TPMR
                  WFOR(IEFD) = TFOR
                  WPMR(IEFD) = TPMR
               END IF
               
               !              For All Units Add to capacity sent to ReStore
               DO ISP = 1 , EENSP
                  STORAGE_CAP(W_GRP,ISP) = EDCAP(EDNTP,ISP) * 0.001 * (1.0 - WFOR(IEFD))*(1.0 - WPMR(IEFD))
               ENDDO
!
               EDMXCP(EDNTP) = INT(MAXCF / EFACTR)
               EDPMR(EDNTP) = INT(TPMR / EFACTR)
               EDFOR(EDNTP) = INT(TFOR / EFACTR)
               EDASTS(EDNTP) = WHYTP(IEFD)
               EDTECP(EDNTP) = IECP
               TSTFSHR = 0.0

               DO IFL = 1 , EIFPLT
                  FL_NUM = WFL(IFL)
                  FL_IND(IFL) = FL_NUM
                  EDFLTP(EDNTP,IFL) = FL_NUM
!
!                 Determine Fuel Regions and Fuel Shares for Renewable Units
!
                  IF (FL_NUM .GT. 0) THEN
                     EDVOMR(EDNTP,IFL) = CAPVOM / GCAP
                     EDMFSH(EDNTP,IFL) = MXFSHR(FL_NUM) / GCAP
                     TSTFSHR = TSTFSHR + EDMFSH(EDNTP,IFL)
                     EDGR(EDNTP) = W_GR
                     DO IFLRG = 1, UNFLRG(FL_NUM)
                        IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'CR') THEN
                           RG_NUM = W_CR
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'GR') THEN
                           RG_NUM = W_GR
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'NR') THEN
                           RG_NUM = WNOWN
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'N2') THEN
                           RG_NUM = WNOPER
                        ELSE IF (UCDFLRG(FL_NUM,IFLRG) .EQ. 'CL') THEN
                           RG_NUM = W_CLRG
                        END IF
                        EDFLRG(EDNTP,IFL,IFLRG) = RG_NUM
                     END DO
                  END IF
                  IF ( TSTFSHR .EQ. DBLE(0.0) ) THEN
                     EDMFSH(EDNTP,1) = 1.0
                  END IF
               END DO
!
               EDNTP = EDNTP + 1
               IF (UF_DBG .GT. 0) THEN
                  INOX = 0
                  WRITE(UF_DBG,9311) CURIYR+UHBSYR,CURITR,W_IGRP,W_GRP,EDNTP,IRG,WSTATE,WC_SUM,GCAP,INOX,(EDFNOX(EDNTP,ISP),ISP=1,EENSP)
                  DO INOX = 1 , NOX_GRP
                     WRITE(UF_DBG,9311) CURIYR+UHBSYR,CURITR,W_IGRP,W_GRP,EDNTP,IRG,WSTATE,WC_SUM,GCAP,INOX,(EDFNOXC(INOX,EDNTP,ISP),ISP=1,EENSP)
                  END DO
               END IF
            END IF                                                  ! IF IEFD is Dispatchable, Renewable or Distributed
         ELSE
            IF (ULCAPC(W_GRP) .GT. 0.0 .AND. RTEST .GT. 0) THEN
               IF (UF_DBG .GT. 0) THEN
                  WRITE(UF_DBG,2391) CURIYR,CURITR,W_GRP,ULIGRP(W_GRP), &
                     ULORGN(W_GRP),ULOPER(W_GRP),ULCENS(W_GRP),ULECPT(W_GRP), &
                     ULEFDT(W_GRP),ULEFPT(W_GRP),ULVINT(W_GRP),ULOWNT(W_GRP), &
                     (ULFUEL(IFL,W_GRP),ULFLRG(IFL,W_GRP),IFL=1,EFD_D_FPP), &
                     ULMRUN(W_GRP),ULRPS(W_GRP),ULCAPC(W_GRP),ULRETC(W_GRP), &
                     ULTGEN(W_GRP),(ULGENE(IFL,W_GRP),IFL=1,EFD_D_FPP), &
                     ULREVS(W_GRP),ULCCST(W_GRP),ULFCST(W_GRP),ULVCST(W_GRP), &
                     ULSO2P(W_GRP),ULNOXP(W_GRP),ULRPSP(W_GRP),ULHGP(W_GRP),IGRP,IRECL,RTEST
!
 2391             FORMAT(1X,"WOUT",2(":",I2),2(":",I5),2(":",I3), &
                     14(":",I2),14(":",F10.3),3(":",I5))
               END IF
               ULCAPC(W_GRP) = 0.0
               ULRETC(W_GRP) = 0.0
               ULFCST(W_GRP) = 0.0
               ULCCST(W_GRP) = 0.0
            END IF
         END IF                                           ! IF ITEST .GT. 0

  500 CONTINUE

!     END PLANT GROUP LOOP

      DO IMO = 1 , 12
         IF (T1(IRG,IMO) .GT. 0.001) THEN
            HYCFMO_AV(IRG,IMO) = T2(IRG,IMO) / T1(IRG,IMO)
         ELSE
            HYCFMO_AV(IRG,IMO) = 0.001
         END IF

         WRITE(18,7104) CURIRUN, CURIYR+1989, IRG, IMO, HYCFMO_AV(IRG,IMO), T2(IRG,IMO), T1(IRG,IMO), SUMMER_MONTHS(IMO)
 7104    FORMAT(1X,"HY_CF_SUMMARY_0",4(",",I4),4(",",F12.4))

      ENDDO

      DO XYR = 1 , UNXPH
         DO IECP = 1 , ECP_D_CAP
            DO ISP = 1 , ECP_D_MSP
               IF (EPECAP(0,IECP,XYR) .GT. 0.0) THEN
                  EP_SP_CAP_FAC(ISP,IECP,XYR) = EP_SP_CAP_FAC(ISP,IECP,XYR)  / EPECAP(0,IECP,XYR)
               ELSE
                  EP_SP_CAP_FAC(ISP,IECP,XYR) = 1.0
               END IF
            END DO
         END DO
      END DO
!
      ECNTP = ECNTP - 1
      EHNTP = EHNTP - 1
      EDNTP = EDNTP - 1
      WRITE(22,7114) IYR,IRG,CL$GEN,CL$CAP
 7114 FORMAT(1X,'CF2 HUH ',2I5,2F14.3)

      IF (UF_DBG .GT. 0) THEN
      WRITE(UF_DBG,1100) IYR,IRG,ECNTP,EHNTP,WNGRPS(IRG)
 1100    FORMAT(1X,'CRPGRP:IYR,IRG,MAX ECNTP,MAX EHNTP,MAX WNGRPS=',5(1X,I6))
            END IF
!
!     For Existing Coal Types Determine Capacity Which Can not be retrofitted to
!
      DO IECP = 1 , ECP_D_CAP
         TST_RETRO(IECP) = 0
      END DO
      TST_RETRO(WING) = 1
      TST_RETRO(WIA2) = 1
!
      DO I_CNFG = 1 , NUM_CNFG
         DO I_ROPT = 1 , NUM_ROPT
            IF (UCL_RCMB(I_ROPT,I_CNFG) .GT. 0) THEN
               IECP = UCL_ECP(UCL_RCMB(I_ROPT,I_CNFG))
               TST_RETRO(IECP) = TST_RETRO(IECP) + 1
            END IF
         END DO
      END DO
!
!     DETERMINE RETIREMENT GROUPS
!
      IF (USW_ERET .EQ. 1) THEN
         DO IECP = 1 , ECP_D_CAP
            CUM_VAL = EPGCAP(0,1,IECP)
            IF (CUM_VAL .GT. 0.0) THEN
            RET_AVG(IECP,JRG) = RET_AVG(IECP,JRG) / CUM_VAL
            RGRP = 1
            CUM_VAL = RET_MAX(IECP) - RET_MIN(IECP)
            NEW_VAL = RET_MIN(IECP) + CUM_VAL * RET_PCT(IECP,RGRP)
            NREC = RET_FRST(RGRP,IECP,JRG)
            DO WHILE (RGRP .LE. RET_GRP(IECP) .AND. NREC .GT. 0)
               TST_VAL = 0.0
               CUM_FOM = 0.0
               RET_FRST(RGRP,IECP,IRG) = NREC
                  HTRT_FRST(RGRP,IECP,JRG) = NREC
                  HTRT_NEXT(NREC) = 0
               DO WHILE (RET_VAL(NREC) .LE. NEW_VAL)

                     IF (IECP .GT. 35 .AND. HTRT_OVR_CST(IECP) .GT. 0.0) THEN
                        WRITE(6,4712) CURIRUN, CURIYR+1989, CURITR, JRG, IECP, RGRP, RET_FRG(NREC), NREC, RET_NEXT(NREC), &
                           RET_CAP(NREC), RET_VAL(NREC), RET_AVG(IECP,JRG), RET_FOM(NREC), RET_PCT(IECP,RGRP)
 4712                   FORMAT(1X,"RET_CAP_HTRT",9(":",I6),5(":",F20.6))
                     END IF

                  FRG = RET_FRG(NREC)
                     IF (FRG .GT. 0) THEN
                        EPGCAP(FRG,RGRP,IECP) = EPGCAP(FRG,RGRP,IECP) + RET_CAP(NREC)
                        IF (HTRT_RESULTS(HTRT_IGRP(NREC)) .EQ. 0) THEN
                           HTRT_UNITS(RGRP,IECP,JRG) = HTRT_UNITS(RGRP,IECP,JRG) + 1
                           HTRT_EPGCAP(FRG,RGRP,IECP,JRG) = HTRT_EPGCAP(FRG,RGRP,IECP,JRG) + RET_CAP(NREC)
                           HTRT_EPGCAP(0,RGRP,IECP,JRG) = HTRT_EPGCAP(0,RGRP,IECP,JRG) + RET_CAP(NREC)
                        END IF
                     END IF
                  TST_VAL = TST_VAL + RET_CAP(NREC)
                  VCST = 1000.0 * RET_AVG(IECP,JRG) -  &
                     1000.0 * RET_VAL(NREC) - RET_FOM(NREC)
                     CUM_FOM = CUM_FOM + (RET_FOM(NREC) + VCST) * RET_CAP(NREC)
                  OREC = NREC
                  NREC = RET_NEXT(NREC)
                     IF (NREC .GT. 0) THEN
                        HTRT_FRST(RGRP,IECP,JRG) = NREC
                        HTRT_NEXT(NREC) = OREC
                     END IF
                  IF (NREC .EQ. 0) GO TO 222
               END DO
                  IF (RET_VAL(NREC) .GT. NEW_VAL) HTRT_FRST(RGRP,IECP,JRG) = OREC

222            CONTINUE

               EPGCAP(0,RGRP,IECP) = TST_VAL
               IF (TST_VAL .GT. 0.0) THEN
                  EPGFOM(RGRP,IECP) = CUM_FOM / TST_VAL
               END IF
               RET_NEXT(OREC) = 0
               RGRP = RGRP + 1
               IF (RGRP .LT. RET_GRP(IECP)) THEN
                  NEW_VAL = NEW_VAL + CUM_VAL * RET_PCT(IECP,RGRP)
               ELSE
                  NEW_VAL = RET_MAX(IECP) + 0.001
               END IF
            END DO

!              VIEW RANKING IN BEST TO WORST

               IF (IECP .GT. 35 .AND. HTRT_OVR_CST(IECP) .GT. 0.0) THEN
                  DO RGRP = 1 , RET_GRP(IECP)
                     NREC = HTRT_FRST(RGRP,IECP,JRG)
                     TST_VAL = 0.0
                     DO WHILE (NREC .GT. 0 .AND. TST_VAL .LT. 1000.0)
                        FRG = RET_FRG(NREC)
                        WRITE(6,5712) CURIRUN, CURIYR+1989, CURITR, JRG, IECP, RGRP, FRG, HTRT_IGRP(NREC), &
                           HTRT_RESULTS(HTRT_IGRP(NREC)), NREC, HTRT_NEXT(NREC), HTRT_UNITS(RGRP,IECP,JRG), &
                           RET_CAP(NREC), RET_VAL(NREC), RET_AVG(IECP,JRG), RET_FOM(NREC), RET_PCT(IECP,RGRP), &
                           EPGCAP(FRG,RGRP,IECP), EPGCAP(0,RGRP,IECP), HTRT_EPGCAP(FRG,RGRP,IECP,JRG), HTRT_EPGCAP(0,RGRP,IECP,JRG)
 5712                   FORMAT(1X,"NEW_CAP_HTRT",12(":",I6),9(":",F20.6))
                        NREC = HTRT_NEXT(NREC)
                        TST_VAL = TST_VAL + 1.0
                     END DO
                  END DO
               END IF
            END IF

         END DO
      END IF
!
!     DISPATCHABLE PLANT GROUPS:
!     APPLY UPPER BOUND ON HEAT RATES
!     UPDATE SCRUBBER STATISTICS
!
      DO IGRP = 1 , ECP_D_DSP
         JGRP = UCPDSPI(IGRP)
         ITYP = UPTTYP(JGRP)
         IF (UPVTYP(JGRP) .GT. 0 .AND. UPAVLYR(IGRP) .LE. CURIYR + UHBSYR + UNFPH) THEN
            WPTTYP(JGRP,IRG) = MAX(1 , WPTTYP(JGRP,IRG))
         ELSE IF (UPTTYP(JGRP) .LE. EX_COAL .OR. JGRP .EQ. WING .OR. JGRP .EQ. WIA2) THEN
            IF (P_TTYP(ITYP) .GT. 0) THEN
               IF (TST_RETRO(JGRP) .GT. 0 .OR. WPTTYP(JGRP,IRG) .GT. 0) THEN
                  WPTTYP(JGRP,IRG) = MAX(1 , WPTTYP(JGRP,IRG))
               END IF
            ELSE
               WPTTYP(JGRP,IRG) = 0
            END IF
         ELSE
            IF (WPTTYP(JGRP,IRG) .GT. 0) THEN
               WPTTYP(JGRP,IRG) = MAX(1 , WPTTYP(JGRP,IRG))
            ELSE
               WPTTYP(JGRP,IRG) = 0
            END IF
         END IF

         IF (ITYP .LE. EX_COAL .OR. JGRP .EQ. WING) THEN
            IF (P_TTYP(ITYP) .GT. 0 .AND. P_GEN(ITYP) .GT. 0.0) THEN
               IF (ECP_GEN(JGRP) .GT. 0.0) THEN
                  EPPHRT0(JGRP) = EPPHRT0(JGRP) / ECP_GEN(JGRP) * UECP_HTRT_ADJ(JGRP)
               ELSE
                  EPPHRT0(JGRP) = P_HTRT0(ITYP) / P_GEN(ITYP) * UECP_HTRT_ADJ(JGRP)
                  IF (JGRP.EQ. WING) EPPHRT0(JGRP) = EPPHRT0(JGRP) * (1.0 + UCL_CL_NG_HR_PEN)
               END IF
               EPFOM(JGRP) = P_FOM(ITYP) / P_GEN(ITYP) / UECP_CPEN_ADJ(JGRP)
               IF (JGRP.EQ. WING) EPFOM(JGRP) = EPFOM(JGRP) * UCL_CL_NG_FOM_ADJ
               IF (ECP_GEN(JGRP) .GT. DBLE(0.0001)) THEN
                  EPVOM(JGRP) = EPVOM(JGRP) / ECP_GEN(JGRP) / UECP_CPEN_ADJ(JGRP)
               ELSE
                  EPVOM(JGRP) = P_VOM(ITYP) / P_GEN(ITYP) / UECP_CPEN_ADJ(JGRP)
                  IF (JGRP.EQ. WING) EPVOM(JGRP) = EPVOM(JGRP) * UCL_CL_NG_VOM_ADJ
               END IF
               DO INOX = 1 , NOX_GRP
                  DO XYR = 1 , UNXPH
                     DO ISP = 1 , ECP_D_MSP
                        IF (P_GEN_XPH(XYR,ITYP) .GT. 0.0 .AND. HRS_ECP(ISP) .NE. 0.0) THEN
                           EPNOX_G(ISP,XYR,JGRP,INOX) = P_NOX(ISP,XYR,ITYP,INOX) / P_GEN_XPH(XYR,ITYP) / HRS_ECP(ISP)
                        ELSE
                           EPNOX_G(ISP,XYR,JGRP,INOX) = ECP_NOX(JGRP)
                        END IF
                     END DO
                  END DO
               END DO
!
            ELSE
               EPPHRT0(JGRP) = UPHTRT(JGRP)
               EPVOM(JGRP) = ECP_VOM(JGRP)
               EPVOM(JGRP) = UPVOM(JGRP)
               DO INOX = 1 , NOX_GRP
                  IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                           EPNOX_G(ISP,XYR,JGRP,INOX) = ECP_NOX(JGRP)
                        END DO
                     END DO
                  END IF
               END DO
            END IF

!           IF (IRG .EQ. 1 .AND. (JGRP .EQ. WIST .OR. JGRP .EQ. WIET .OR. JGRP .EQ. WIEC .OR. JGRP .EQ. WIC4)) THEN
!
!              FROM_LABEL = "CRPGRP_DSP"
!              CALL ECP_AVG_HTRT(FROM_LABEL, IRG, 0, JGRP, 1, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, T_ECP_GEN, T_ECP_GEN_MR, T_ECP_GEN_MOD, T_ECP_GEN_MR_MOD)
!
!              WRITE(18,8134) CURIRUN, CURIYR+1989, IRG, JGRP, ITYP, &
!                 EPECAP(0,JGRP,UNXPH),    T_ECP_GEN(JGRP), AVG_HTRT(JGRP), UPHTRT(JGRP), UECP_HTRT_ADJ(JGRP), &
!                 EPECAP_MR(0,JGRP,UNXPH), T_ECP_GEN_MR(JGRP), AVG_HTRT_MR(JGRP)
!           END IF
         ELSE
            IF ((UPVTYP(JGRP) .EQ. 0) .OR. (UPTOPR(JGRP) .EQ. 3)) THEN
               IF (EPECAP(0,JGRP,UNXPH) .GT. 0.0) THEN
                  EPFOM(JGRP) = EPFOM(JGRP) / EPECAP(0,JGRP,UNXPH) / UECP_CPEN_ADJ(JGRP)
                  IF (ECP_GEN(JGRP) .GT. DBLE(0.0001)) THEN
                     EPPHRT0(JGRP) = EPPHRT0(JGRP) / ECP_GEN(JGRP) * UECP_HTRT_ADJ(JGRP)
                     EPVOM(JGRP) = EPVOM(JGRP) / ECP_GEN(JGRP) / UECP_CPEN_ADJ(JGRP)
                     DO INOX = 1 , NOX_GRP
                        DO XYR = 1 , UNXPH
                           DO ISP = 1 , ECP_D_MSP
                              IF (ECP_GEN_XPH(XYR,JGRP) .GT. 0.0 .AND. HRS_ECP(ISP) .NE. 0.0) THEN
                                 EPNOX_G(ISP,XYR,JGRP,INOX) = EPNOX_G(ISP,XYR,JGRP,INOX) / ECP_GEN_XPH(XYR,JGRP) / HRS_ECP(ISP)
                              ELSE
                                 EPNOX_G(ISP,XYR,JGRP,INOX) = ECP_NOX(JGRP)
                              END IF
                           END DO
                        END DO
                     END DO
                  ELSE
                     EPPHRT0(JGRP) = P_HTRT0(JGRP) * UECP_HTRT_ADJ(JGRP)
                     EPVOM(JGRP) = ECP_VOM(JGRP)
                     DO INOX = 1 , NOX_GRP
                        IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                           DO XYR = 1 , UNXPH
                              DO ISP = 1 , ECP_D_MSP
                                 EPNOX_G(ISP,XYR,JGRP,INOX) = ECP_NOX(JGRP)
                              END DO
                           END DO
                        END IF
                     END DO
                  END IF
               ELSE
                  EPPHRT0(JGRP) = UPHTRT(JGRP)
                  EPFOM(JGRP) = UPFOM(JGRP)
                  EPVOM(JGRP) = UPVOM(JGRP)
                  DO INOX = 1 , NOX_GRP
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                           EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                        END DO
                     END DO
                  END DO
               END IF

!              IF (IRG .EQ. 1 .AND. (JGRP .EQ. WIST .OR. JGRP .EQ. WIET .OR. JGRP .EQ. WIEC .OR. JGRP .EQ. WIC4)) THEN
!
!                 FROM_LABEL = "CRPGRP_DSP"
!                 CALL ECP_AVG_HTRT(FROM_LABEL, IRG, 0, JGRP, 1, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, T_ECP_GEN, T_ECP_GEN_MR, T_ECP_GEN_MOD, T_ECP_GEN_MR_MOD)
!
!                 WRITE(18,8134) CURIRUN, CURIYR+1989, IRG, JGRP, ITYP, &
!                    EPECAP(0,JGRP,UNXPH),    T_ECP_GEN(JGRP), AVG_HTRT(JGRP), UPHTRT(JGRP), UECP_HTRT_ADJ(JGRP), &
!                    EPECAP_MR(0,JGRP,UNXPH), T_ECP_GEN_MR(JGRP), AVG_HTRT_MR(JGRP)
!8134             FORMAT(1X,"HTRT1_CRPGRP",5(":",I6),8(":",F21.6))
!              END IF
            ELSE
               DO INOX = 1 , NOX_GRP
                  IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                  IF (HRS_ECP(ISP) .NE. 0.0) &
                           EPNOX_G(ISP,XYR,JGRP,INOX) = NOX_NEW(1,JGRP) * NOX_ECP(ISP,INOX) / HRS_ECP(ISP)
                        END DO
                     END DO
                  ELSE
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                           EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                        END DO
                     END DO
                  END IF
               END DO
            END IF
         END IF

         IF (EPECAP(0,JGRP,UNXPH) .GT. 0.0) THEN
            DO INOX = 1 , NOX_GRP
               DO ISP = 1 , ECP_D_MSP
                  R8TEMP = 0.0
                  DO XYR = 1 , UNXPH
                     R8TEMP = R8TEMP + EPNOX_G(ISP,XYR,JGRP,INOX)
                  END DO
                  IF (R8TEMP .GT. 0.0) &
                  WRITE(18,9468) CURIYR,IRG,IGRP,JGRP,INOX,ISP,(EPNOX_G(ISP,XYR,JGRP,INOX),XYR=1,UNXPH),EPECAP(0,JGRP,UNXPH),EPECAP_MR(0,JGRP,UNXPH),(ECP_GEN_XPH(XYR,JGRP),XYR=1,UNXPH), &
                     (P_GEN_XPH(XYR,ITYP),XYR=1,UNXPH),NOX_ECP(ISP,INOX)
 9468             FORMAT(1X,'EPNOX_G',6(":",I4),3(":",F9.6),2(":",F7.1),6(":",F8.1),":",F6.1)
               END DO
            END DO
         END IF
!
         DO ISCR = 1 , UPSGRP
            IF (EPSCAP(IGRP,ISCR) .GT. 0.0) THEN
               EPSOVR(IGRP,ISCR) = EPSOVR(IGRP,ISCR) / EPSCAP(IGRP,ISCR)
               NSCR = EPNSCR(IGRP,ISCR)
               IF (NSCR .GT. 1) THEN
                  CALL ESCRSRT(NSCR,SCRCST(1,IGRP,ISCR),EPSREC(1,IGRP,ISCR))
               END IF
            END IF
         END DO
      END DO
!
!     Turn off capacity exclusively related to mercury emission control when no mercury limits exist
!
      IF (USW_HG .EQ. 0) THEN
         DO I_CNFG = 1 , NUM_CNFG
            IF (UCL_CNFG(7,I_CNFG) .GT. 0 .OR. UCL_CNFG(8,I_CNFG) .GT. 0) THEN
               IECP = UCL_ECP(I_CNFG)
               WPTTYP(IECP,IRG) = 0
            END IF
         END DO
         NUM_ACI = 0
      END IF
!
      DO IGRP = 1 , ECP_D_DSP
         JGRP = UCPDSPI(IGRP)
         ITYP = UPTTYP(JGRP)

         FROM_LABEL = "CRPGRP_DSP"
         CALL ECP_AVG_HTRT(FROM_LABEL, IRG, 0, IGRP, 1, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, T_ECP_GEN, T_ECP_GEN_MR, T_ECP_GEN_MOD, T_ECP_GEN_MR_MOD)

         WRITE(18,2213) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, IGRP, JGRP, ITYP, TST_RETRO(JGRP), WPTTYP(JGRP,IRG), P_TTYP(ITYP), &
            ECP_GEN(JGRP), P_GEN(ITYP), EPPHRT0(JGRP), EPFOM(JGRP), EPVOM(JGRP),  &
            EPECAP(0,JGRP,1), T_ECP_GEN(JGRP), T_ECP_GEN(0), AVG_HTRT(JGRP), &
            EPECAP_MR(0,JGRP,1), T_ECP_GEN_MR(JGRP), T_ECP_GEN_MR(0), AVG_HTRT_MR(JGRP), UECP_HTRT_ADJ(JGRP), UECP_CPEN_ADJ(JGRP), P_HTRT_MAX(ITYP)
 2213    FORMAT(1X,"WPTTYP",10(":",I6),18(":",E20.6))
      END DO
!
      DO IGRP = 1 , ECP_D_DSP
         JGRP = UCPDSPI(IGRP)
         ITYP = UPTTYP(JGRP)
!
!        DETERMINE MAXIMUM WOOD COFIRING SHARE
!
         DO YEAR = 1 , ECP_D_XPH
            IF (IRG .EQ. UNRGNS) THEN
               DO CRG = 1 , NDREG
                  IF(WDSUM(IGRP,YEAR,CRG) .GT. 0)THEN
                     UPWDCOF(IGRP,YEAR,CRG) = WDSHR(IGRP,YEAR,CRG) / WDSUM(IGRP,YEAR,CRG)
                  ELSE
                     UPWDCOF(IGRP,YEAR,CRG) = 0.0
                  END IF
               END DO
            END IF
            IF (P_MCF(ITYP,YEAR) .GT. 0.0 .AND. P_CAP(ITYP,YEAR) .GT. 0.0) THEN
               EPECFC(IGRP,YEAR) = P_MCF(ITYP,YEAR) / P_CAP(ITYP,YEAR)
            ELSE
               EPECFC(IGRP,YEAR) = UPMCF(IGRP)
            END IF
            IF (ITYP .LE. NW_COAL .AND. CURIYR .GE. 11 .AND. P_CAP(ITYP,YEAR) .GT. 0.0) &
               WRITE(18,7317) CURIYR+UHBSYR,YEAR,IRG,ITYP,IGRP,JGRP,P_CAP(ITYP,YEAR),P_MCF(ITYP,YEAR)/P_CAP(ITYP,YEAR),EPECAP(0,IGRP,YEAR),EPECFC(IGRP,YEAR)
 7317       FORMAT(1X,"P_MCF1",6(":",I5),4(":",F12.3))
         END DO
      END DO
!
!     FILL ARRAYS FOR STORAGE TECHNOLOGIES
!
      DO IGRP = 1 , ECP_D_STO
         JGRP = UCPSTOI(IGRP)
         IF (JGRP .GT. 0) THEN
            IF (UPVTYP(JGRP) .GT. 0 .OR. WPTTYP(JGRP,IRG) .GT. 0) THEN
               WPTTYP(JGRP,IRG) = MAX(1 , WPTTYP(JGRP,IRG))
            END IF
            IF (UPVTYP(JGRP) .EQ. 0) THEN
               IF (EPECAP(0,JGRP,UNXPH) .GT. 0.0) THEN
                  EPFOM(JGRP) = EPFOM(JGRP) / EPECAP(0,JGRP,UNXPH) / UECP_CPEN_ADJ(JGRP)
                  IF (ECP_GEN(JGRP) .GT. DBLE(0.0001)) THEN
                     EPVOM(JGRP) = EPVOM(JGRP) / ECP_GEN(JGRP) / UECP_CPEN_ADJ(JGRP)
                  ELSE
                     EPVOM(JGRP) = ECP_VOM(JGRP)
                  END IF
               ELSE
                  EPFOM(JGRP) = UPFOM(JGRP)
                  EPVOM(JGRP) = UPVOM(JGRP)
               END IF
            ELSE
               EPFOM(JGRP) = UPFOM(JGRP)
               EPVOM(JGRP) = UPVOM(JGRP)
            END IF
         END IF ! JGRP GT 0
      END DO
!
!     ADJUST MAX RNW CAPACITY FACTOR IF EXISTING CAP EXCEEDS LIMIT
!     ADJUST FOR EXISTING PLANT TYPES ONLY
!
      DO IGRP = 1 , ECP_D_RNW
         JGRP = UCPRNWI(IGRP)
         IF (JGRP .GT. 0) THEN
            IF (UPVTYP(JGRP) .GT. 0 .OR. WPTTYP(JGRP,IRG) .GT. 0) THEN
               WPTTYP(JGRP,IRG) = MAX(1 , WPTTYP(JGRP,IRG))
            END IF
            IF ((UPVTYP(JGRP) .EQ. 0) .OR. (UPTOPR(JGRP) .EQ. 3)) THEN
               IF (EPECAP(0,JGRP,UNXPH) .GT. 0.0) THEN
                  EPFOM(JGRP) = EPFOM(JGRP) / EPECAP(0,JGRP,UNXPH) / UECP_CPEN_ADJ(JGRP)
                  IF (ECP_GEN(JGRP) .GT. DBLE(0.0001)) THEN
                     EPVOM(JGRP) = EPVOM(JGRP) / ECP_GEN(JGRP) / UECP_CPEN_ADJ(JGRP)
                     DO INOX = 1 , NOX_GRP
                        DO XYR = 1 , UNXPH
                           DO ISP = 1 , ECP_D_MSP
                              IF (ECP_GEN_XPH(XYR,JGRP) .GT. 0.0 .AND. HRS_ECP(ISP) .NE. 0.0) THEN
                                 EPNOX_G(ISP,XYR,JGRP,INOX) = EPNOX_G(ISP,XYR,JGRP,INOX) / ECP_GEN_XPH(XYR,JGRP) / HRS_ECP(ISP)
                              ELSE
                                 EPNOX_G(ISP,XYR,JGRP,INOX) = ECP_NOX(JGRP)
                              END IF
                              IF (EPNOX_G(ISP,XYR,JGRP,INOX) .GT. 0.0 .AND. JGRP .EQ. 49) THEN
                                 WRITE(18,9371) CURIYR+UHBSYR,CURIYR+UHBSYR+XYR-1,CURITR,IRG,JGRP,INOX,XYR,ISP,EPNOX_G(ISP,XYR,JGRP,INOX), &
                                    ECP_GEN_XPH(XYR,JGRP),HRS_ECP(ISP)
 9371                            FORMAT(1X,"EPNOX_G_HUH",7(":",I5),3(":",F12.3))
                              END IF
                           END DO
                        END DO
                     END DO
                  ELSE
                     EPVOM(JGRP) = ECP_VOM(JGRP)
                     DO INOX = 1 , NOX_GRP
                        IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                           DO XYR = 1 , UNXPH
                              DO ISP = 1 , ECP_D_MSP
                                 EPNOX_G(ISP,XYR,JGRP,INOX) = ECP_NOX(JGRP)
                                 IF (EPNOX_G(ISP,XYR,JGRP,INOX) .GT. 0.0 .AND. JGRP .EQ. 49) THEN
                                    WRITE(18,9371) CURIYR+UHBSYR,CURIYR+UHBSYR+XYR-1,CURITR,IRG,JGRP,INOX,XYR,ISP,EPNOX_G(ISP,XYR,JGRP,INOX), &
                                       ECP_GEN_XPH(XYR,JGRP),HRS_ECP(ISP)
                                 END IF
                              END DO
                           END DO
                        END IF
                     END DO
                  END IF
               ELSE
                  EPFOM(JGRP) = UPFOM(JGRP)
                  EPVOM(JGRP) = UPVOM(JGRP)
                  DO INOX = 1 , NOX_GRP
                     IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                        DO XYR = 1 , UNXPH
                           DO ISP = 1 , ECP_D_MSP
                     IF (HRS_ECP(ISP) .NE. 0.0) &
                              EPNOX_G(ISP,XYR,JGRP,INOX) = NOX_NEW(1,JGRP) * NOX_ECP(ISP,INOX) / HRS_ECP(ISP)
                           END DO
                        END DO
                     ELSE
                        DO XYR = 1 , UNXPH
                           DO ISP = 1 , ECP_D_MSP
                              EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                           END DO
                        END DO
                     END IF
                  END DO
               END IF

!              Calculate Average Seasonal (ECP) Capacity Factors
!
               IF (EPECAP(0,UCPRNWI(IGRP),1) .GT. 0.001) THEN
                  EPRCFC(IGRP) = RNW_MCFC(IGRP) / EPECAP(0,UCPRNWI(IGRP),1)
                  EPIRCCR(UIRRNWI(IGRP)) = EPRCFC(IGRP)
                  EPIRCCS(UIRRNWI(IGRP),:) = EPRCFC(IGRP)
                  DO ISP = 1,EPNMSP
                     RSCFC(IGRP,ISP) = ECAPCF(IGRP,ISP,1) / ECFCAP(IGRP,ISP,1)

!                    hydro adjustments now done by plant group above

                     DO YEAR = 1 , UNXPH
                        IF (ECFCAP(IGRP,ISP,YEAR) .GE. 0.001) THEN
                           EPESCFC(ISP,JGRP,YEAR) = ECAPCF(IGRP,ISP,YEAR) / ECFCAP(IGRP,ISP,YEAR)
                        END IF

                     END DO
                  END DO
               ELSE
                  EPRCFC(IGRP) = 0.0
                  EPIRCCR(UIRRNWI(IGRP)) = 0.0
                  EPIRCCS(UIRRNWI(IGRP),:) = 0.0
                  DO ISP = 1,EPNMSP
                     RSCFC(IGRP,ISP) = 0.0
                  END DO
               END IF
!
!              MAP CAP FACTORS INTO LOAD GROUPS FOR REGION
!
               DO IL = 1,EPNSTP(1)
                  LDGRP = EPLDGR(IL,1)
                  ISP = EPGECP(LDGRP)
                  IF ((LDGRP .GE. 1) .AND. (LDGRP .LE. 3)) THEN
                     EPRSCFC(IGRP,:,ISP) = RSCFC(IGRP,1)
                  ELSEIF ((LDGRP .GT. 3) .AND. (LDGRP .LE. 6)) THEN
                     EPRSCFC(IGRP,:,ISP) = RSCFC(IGRP,2)
                  ELSEIF ((LDGRP .GT. 6) .AND. (LDGRP .LE. 9)) THEN
                     EPRSCFC(IGRP,:,ISP) = RSCFC(IGRP,3)
                  ENDIF
               ENDDO
            ELSE

               DO INOX = 1 , NOX_GRP
                  IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                  IF (HRS_ECP(ISP) .NE. 0.0) &
                           EPNOX_G(ISP,XYR,JGRP,INOX) = NOX_NEW(1,JGRP) * NOX_ECP(ISP,INOX) / HRS_ECP(ISP)
                        END DO
                     END DO
                  ELSE
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                           EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                        END DO
                     END DO
                  END IF
               END DO
            END IF
            IF (UF_DBG .GT. 0) THEN
               DO INOX = 1 , NOX_GRP
                  DO ISP = 1 , ECP_D_MSP
                     R8TEMP = 0.0
                     DO XYR = 1 , UNXPH
                        R8TEMP = R8TEMP + EPNOX_G(ISP,XYR,JGRP,INOX)
                     END DO
                     IF (R8TEMP .GT. 0.0) &
                        WRITE(18,9468) CURIYR,IRG,IGRP,JGRP,INOX,ISP,(EPNOX_G(ISP,XYR,JGRP,INOX),XYR=1,UNXPH),EPECAP(0,JGRP,UNXPH),EPECAP_MR(0,JGRP,UNXPH),(ECP_GEN_XPH(XYR,JGRP),XYR=1,UNXPH), &
                        (P_GEN_XPH(XYR,ITYP),XYR=1,UNXPH),NOX_ECP(ISP,INOX)
                  END DO
               END DO
            END IF
!
!           DETERMINE AVG CAPACITY FACTOR
!
            DO YEAR = 1 , ECP_D_XPH
               IF (EPECFC(JGRP,YEAR) .GT. 0)THEN
                  EPECFC(JGRP,YEAR) = EPECFC(JGRP,YEAR) / EPECAP(0,JGRP,YEAR)
               ELSE
                  EPECFC(JGRP,YEAR) = EPRCFC(IGRP)
               END IF
                           
            END DO

            FROM_LABEL = "CRPGRP_RNW"
            CALL ECP_AVG_HTRT(FROM_LABEL, IRG, 0, JGRP, 1, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, T_ECP_GEN, T_ECP_GEN_MR, T_ECP_GEN_MOD, T_ECP_GEN_MR_MOD)

            WRITE(18,2213) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, IGRP, JGRP, ITYP, TST_RETRO(JGRP), WPTTYP(JGRP,IRG), P_TTYP(ITYP), &
               ECP_GEN(JGRP), P_GEN(ITYP), AVG_HTRT(JGRP), EPFOM(JGRP), EPVOM(JGRP),  &
               EPECAP(0,JGRP,1), T_ECP_GEN(JGRP), T_ECP_GEN(0), AVG_HTRT(JGRP), &
               EPECAP_MR(0,JGRP,1), T_ECP_GEN_MR(JGRP), T_ECP_GEN_MR(0), AVG_HTRT_MR(JGRP), UECP_HTRT_ADJ(JGRP), UECP_CPEN_ADJ(JGRP), P_HTRT_MAX(ITYP)
         END IF ! JGRP GT 0
!
      END DO
!
      DO IGRP = 1 , ECP_D_INT
         JGRP = UCPINTI(IGRP)
         IF (UPVTYP(JGRP) .GT. 0 .OR. WPTTYP(JGRP,IRG) .GT. 0) THEN
            WPTTYP(JGRP,IRG) = MAX(1 , WPTTYP(JGRP,IRG))
         END IF
         IF ((UPVTYP(JGRP) .EQ. 0) .OR. (UPTOPR(JGRP) .EQ. 3)) THEN
            IF (EPECAP(0,JGRP,UNXPH) .GT. 0.0) THEN
               EPFOM(JGRP) = EPFOM(JGRP) / EPECAP(0,JGRP,UNXPH) / UECP_CPEN_ADJ(JGRP)
               IF (ECP_GEN(JGRP) .GT. DBLE(0.0001)) THEN
                  EPVOM(JGRP) = EPVOM(JGRP) / ECP_GEN(JGRP) / UECP_CPEN_ADJ(JGRP)
                  DO INOX = 1 , NOX_GRP
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                           IF (ECP_GEN_XPH(XYR,JGRP) .GT. 0.0 .AND. HRS_ECP(ISP) .NE. 0.0) THEN
                              EPNOX_G(ISP,XYR,JGRP,INOX) = EPNOX_G(ISP,XYR,ITYP,INOX) / ECP_GEN_XPH(XYR,ITYP) / HRS_ECP(ISP)
                           ELSE
                              EPNOX_G(ISP,XYR,JGRP,INOX) = ECP_NOX(JGRP)
                           END IF
                        END DO
                     END DO
                  END DO
               ELSE
                  EPVOM(JGRP) = ECP_VOM(JGRP)
                  DO INOX = 1 , NOX_GRP
                     IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                        DO XYR = 1 , UNXPH
                           DO ISP = 1 , ECP_D_MSP
                              EPNOX_G(ISP,XYR,JGRP,INOX) = ECP_NOX(JGRP)
                           END DO
                        END DO
                     END IF
                  END DO
               END IF
            ELSE
               EPFOM(JGRP) = UPFOM(JGRP)
               EPVOM(JGRP) = UPVOM(JGRP)
               DO INOX = 1 , NOX_GRP
                  IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
               IF (HRS_ECP(ISP) .NE. 0.0) &
                           EPNOX_G(ISP,XYR,JGRP,INOX) = NOX_NEW(1,JGRP) * NOX_ECP(ISP,INOX) / HRS_ECP(ISP)
                        END DO
                     END DO
                  ELSE
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                           EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                        END DO
                     END DO
                  END IF
               END DO
            END IF
         ELSE
            DO INOX = 1 , NOX_GRP
               IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                  DO XYR = 1 , UNXPH
                     DO ISP = 1 , ECP_D_MSP
               IF (HRS_ECP(ISP) .NE. 0.0) &
                        EPNOX_G(ISP,XYR,JGRP,INOX) = NOX_NEW(1,JGRP) * NOX_ECP(ISP,INOX) / HRS_ECP(ISP)
                     END DO
                  END DO
               ELSE
                  DO XYR = 1 , UNXPH
                     DO ISP = 1 , ECP_D_MSP
                        EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                     END DO
                  END DO
               END IF
            END DO
         END IF
         IF (UF_DBG .GT. 0) THEN
            DO INOX = 1 , NOX_GRP
               DO ISP = 1 , ECP_D_MSP
                  R8TEMP = 0.0
                  DO XYR = 1 , UNXPH
                     R8TEMP = R8TEMP + EPNOX_G(ISP,XYR,JGRP,INOX)
                  END DO
                  IF (R8TEMP .GT. 0.0) &
                     WRITE(18,9468) CURIYR,IRG,IGRP,JGRP,INOX,ISP,(EPNOX_G(ISP,XYR,JGRP,INOX),XYR=1,UNXPH),EPECAP(0,JGRP,UNXPH),EPECAP_MR(0,JGRP,UNXPH),(ECP_GEN_XPH(XYR,JGRP),XYR=1,UNXPH), &
                     (P_GEN_XPH(XYR,ITYP),XYR=1,UNXPH),NOX_ECP(ISP,INOX)
               END DO
            END DO
         END IF
!
!        DETERMINE AVG CAPACITY FACTOR
!
         DO YEAR = 1 , ECP_D_XPH
            IF (EPECFC(JGRP,YEAR) .GT. 0)THEN
               EPECFC(JGRP,YEAR) = EPECFC(JGRP,YEAR) / EPECAP(0,JGRP,YEAR)
            ELSE
               EPECFC(JGRP,YEAR) = EPIACF(IGRP)
            END IF
            
            IF (JGRP .EQ. WIPV) THEN
               IF (EPECFC_UPV(JGRP,YEAR) .GT. 0) THEN
                   EPECFC_UPV(JGRP,YEAR) = EPECFC_UPV(JGRP,YEAR) / EPECAP_UPV(0,JGRP,YEAR)
               ELSE
                   EPECFC_UPV(JGRP,YEAR) = EPIACF(IGRP)
               ENDIF
            ENDIF
         END DO

         FROM_LABEL = "CRPGRP_INT"
         CALL ECP_AVG_HTRT(FROM_LABEL, IRG, 0, JGRP, 1, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, T_ECP_GEN, T_ECP_GEN_MR, T_ECP_GEN_MOD, T_ECP_GEN_MR_MOD)

         WRITE(18,2213) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, IGRP, JGRP, ITYP, TST_RETRO(JGRP), WPTTYP(JGRP,IRG), P_TTYP(ITYP), &
            ECP_GEN(JGRP), P_GEN(ITYP), AVG_HTRT(JGRP), EPFOM(JGRP), EPVOM(JGRP),  &
            EPECAP(0,JGRP,1), T_ECP_GEN(JGRP), T_ECP_GEN(0), AVG_HTRT(JGRP), &
            EPECAP_MR(0,JGRP,1), T_ECP_GEN_MR(JGRP), T_ECP_GEN_MR(0), AVG_HTRT_MR(JGRP), UECP_HTRT_ADJ(JGRP), UECP_CPEN_ADJ(JGRP), P_HTRT_MAX(ITYP)
!
      END DO
!
      DO IGRP = 1 , ECP_D_DGN
         JGRP = UCPDGNI(IGRP)
         IF (UPVTYP(JGRP) .GT. 0 .OR. WPTTYP(JGRP,IRG) .GT. 0) THEN
            WPTTYP(JGRP,IRG) = MAX(1 , WPTTYP(JGRP,IRG))
         END IF
         IF ((UPVTYP(JGRP) .EQ. 0) .OR. (UPTOPR(JGRP) .EQ. 3)) THEN
            IF (EPECAP(0,JGRP,UNXPH) .GT. 0.0) THEN
               EPFOM(JGRP) = EPFOM(JGRP) / EPECAP(0,JGRP,UNXPH) * UECP_CPEN_ADJ(IECP)
               IF (ECP_GEN(JGRP) .GT. DBLE(0.0001)) THEN
                  EPVOM(JGRP) = EPVOM(JGRP) / ECP_GEN(JGRP) / UECP_CPEN_ADJ(JGRP)
                  DO INOX = 1 , NOX_GRP
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                           IF (ECP_GEN_XPH(XYR,JGRP) .GT. 0.0 .AND. HRS_ECP(ISP) .NE. 0.0) THEN
                              EPNOX_G(ISP,XYR,JGRP,INOX) = EPNOX_G(ISP,XYR,ITYP,INOX) / ECP_GEN_XPH(XYR,ITYP) / HRS_ECP(ISP)
                           ELSE
                              EPNOX_G(ISP,XYR,JGRP,INOX) = ECP_NOX(JGRP)
                           END IF
                        END DO
                     END DO
                  END DO
               ELSE
                  EPVOM(JGRP) = ECP_VOM(JGRP)
                  DO INOX = 1 , NOX_GRP
                     IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                        DO XYR = 1 , UNXPH
                           DO ISP = 1 , ECP_D_MSP
                              EPNOX_G(ISP,XYR,JGRP,INOX) = ECP_NOX(JGRP)
                           END DO
                        END DO
                     ELSE
                        DO XYR = 1 , UNXPH
                           DO ISP = 1 , ECP_D_MSP
                              EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                           END DO
                        END DO
                     END IF
                  END DO
               END IF
            ELSE
               EPFOM(JGRP) = UPFOM(JGRP)
               EPVOM(JGRP) = UPVOM(JGRP)
               DO INOX = 1 , NOX_GRP
                  IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                  IF (HRS_ECP(ISP) .NE. 0.0) &
                           EPNOX_G(ISP,XYR,JGRP,INOX) = NOX_NEW(1,JGRP) * NOX_ECP(ISP,INOX) / HRS_ECP(ISP)
                        END DO
                     END DO
                  ELSE
                     DO XYR = 1 , UNXPH
                        DO ISP = 1 , ECP_D_MSP
                           EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                        END DO
                     END DO
                  END IF
               END DO
            END IF
         ELSE
            DO INOX = 1 , NOX_GRP
               IF (NOX_RGN(IRG,INOX) .GT. 0) THEN
                  DO XYR = 1 , UNXPH
                     DO ISP = 1 , ECP_D_MSP
                  IF (HRS_ECP(ISP) .NE. 0.0) &
                        EPNOX_G(ISP,XYR,JGRP,INOX) = NOX_NEW(1,JGRP) * NOX_ECP(ISP,INOX) / HRS_ECP(ISP)
                     END DO
                  END DO
               ELSE
                  DO XYR = 1 , UNXPH
                     DO ISP = 1 , ECP_D_MSP
                        EPNOX_G(ISP,XYR,JGRP,INOX) = 0.0
                     END DO
                  END DO
               END IF
            END DO
         END IF
         IF (UF_DBG .GT. 0) THEN
            DO INOX = 1 , NOX_GRP
               DO ISP = 1 , ECP_D_MSP
                  R8TEMP = 0.0
                  DO XYR = 1 , UNXPH
                     R8TEMP = R8TEMP + EPNOX_G(ISP,XYR,JGRP,INOX)
                  END DO
                  IF (R8TEMP .GT. 0.0) &
                     WRITE(18,9468) CURIYR,IRG,IGRP,JGRP,INOX,ISP,(EPNOX_G(ISP,XYR,JGRP,INOX),XYR=1,UNXPH),EPECAP(0,JGRP,UNXPH),EPECAP_MR(0,JGRP,UNXPH),(ECP_GEN_XPH(XYR,JGRP),XYR=1,UNXPH), &
                     (P_GEN_XPH(XYR,ITYP),XYR=1,UNXPH),NOX_ECP(ISP,INOX)
               END DO
            END DO
         END IF
!
!        DETERMINE AVG CAPACITY FACTOR
!
         DO YEAR = 1 , ECP_D_XPH
            IF (EPECFC(JGRP,YEAR) .GT. 0)THEN
               EPECFC(JGRP,YEAR) = EPECFC(JGRP,YEAR) / EPECAP(0,JGRP,YEAR)
            ELSE
               EPECFC(JGRP,YEAR) = UPMCF(JGRP)
            END IF
         END DO

         FROM_LABEL = "CRPGRP_DGN"
         CALL ECP_AVG_HTRT(FROM_LABEL, IRG, 0, JGRP, 1, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, T_ECP_GEN, T_ECP_GEN_MR, T_ECP_GEN_MOD, T_ECP_GEN_MR_MOD)

         WRITE(18,2213) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, IGRP, JGRP, ITYP, TST_RETRO(JGRP), WPTTYP(JGRP,IRG), P_TTYP(ITYP), &
            ECP_GEN(JGRP), P_GEN(ITYP), AVG_HTRT(JGRP), EPFOM(JGRP), EPVOM(JGRP),  &
            EPECAP(0,JGRP,1), T_ECP_GEN(JGRP), T_ECP_GEN(0), AVG_HTRT(JGRP), &
            EPECAP_MR(0,JGRP,1), T_ECP_GEN_MR(JGRP), T_ECP_GEN_MR(0), AVG_HTRT_MR(JGRP), UECP_HTRT_ADJ(JGRP), UECP_CPEN_ADJ(JGRP), P_HTRT_MAX(ITYP)
!
      END DO
!
!
      CGOTGEN(JRG,IYR - UHBSYR,1) = EOUIPP * 8760 * .001
      CGOTGEN(JRG,IYR - UHBSYR,2) = EOUNT * 8760 * .001
      IF (UF_DBG .GT. 0) THEN
         WRITE( UF_DBG ,988) JRG,IYR,CGOTGEN(JRG,IYR - UHBSYR,1), &
            EOUIPP
  988    FORMAT('  REGION = ',I4,'  YEAR=',I4,'  CGOTGEN = ',F10.2, &
            '  EOUIPP = ',F10.2)
      END IF
!
!     STORE EWG / IPP OWN USE VALUES
!
      IOWN = 3   ! EWG / IPP
      ULOWNE(IOWN,JRG) = EOUIPP * 8760.0
!
!     STORE NON-TRADIONAL COGEN OWN USE VALUES
!
      IOWN = 4   ! NON-TRADITIONAL COGEN
      ULOWNE(IOWN,JRG) = EOUIPP * 8.760
!
!     Store Average Coal Plant Heatrates by ECP Type and Coal Region
!
      IF (IRG .EQ. UNRGNS) THEN
         DO I_CLRG = 1 , NDRGG
            DO IECP =  1 , NCLUT1
               ITYP = UPTTYP(IECP)
               IF (C2_GEN(ITYP,I_CLRG) .GT. 0.001) THEN
                  IF (T_ECP(IECP) .EQ. 0) THEN
                     HRTCLNR(I_CLRG,CURIYR,IECP) = C2_HTRT(ITYP,I_CLRG) /  C2_GEN(ITYP,I_CLRG)
                  ELSE
                     HRTCLNR(I_CLRG,CURIYR,IECP) = C2_HTRT(ITYP,I_CLRG) /  C2_GEN(ITYP,I_CLRG) * (1.0 + SCRHPEN)
                  END IF
               ELSE
                  HRTCLNR(I_CLRG,CURIYR,IECP) = UPHTRT(IECP)
               END IF
               WRITE(18,3978) CURIYR+UHBSYR,I_CLRG,IECP,ITYP,T_ECP(IECP),C2_GEN(ITYP,I_CLRG),C2_HTRT(ITYP,I_CLRG),SCRHPEN,UPHTRT(IECP),HRTCLNR(I_CLRG,CURIYR,IECP)
 3978          FORMAT(1X,"HRTCLNR",5(":",I4),5(":",F15.3))
            END DO
         END DO
      END IF
!
!     Average NG CC Plant Data across Multiple Records if Any
!
      IF (IRG .EQ. UNRGNS) THEN
         DO I_NGBS = 1 , NUM_NGBS
            IF (CAP_NGBS(I_NGBS) .GT. 0.0001) THEN
               ENG_CCS_O(I_NGBS) = ENG_CCS_O(I_NGBS) / CAP_NGBS(I_NGBS)
               ENG_CCS_F(I_NGBS) = ENG_CCS_F(I_NGBS) / CAP_NGBS(I_NGBS)
               ENG_CCS_V(I_NGBS) = ENG_CCS_V(I_NGBS) / CAP_NGBS(I_NGBS)
               ENG_CCS_R(I_NGBS) = ENG_CCS_R(I_NGBS) / CAP_NGBS(I_NGBS)
               ENG_CCS_H(I_NGBS) = ENG_CCS_H(I_NGBS) / CAP_NGBS(I_NGBS)
               ENG_CCS_C(I_NGBS) = ENG_CCS_C(I_NGBS) / CAP_NGBS(I_NGBS)
            ELSE
               ENG_CCS_O(I_NGBS) = 9999.999
               ENG_CCS_F(I_NGBS) = 9999.999
               ENG_CCS_V(I_NGBS) = 9999.999
               ENG_CCS_R(I_NGBS) = 0.0
               ENG_CCS_H(I_NGBS) = 1.0
               ENG_CCS_C(I_NGBS) = 1.0
            END IF
            DO XYR = 1 , UNXPH
               IF (ENG_CAP(XYR,I_NGBS) .GT. 0.0) THEN
                  ENG_FOM(XYR,I_NGBS) = ENG_FOM(XYR,I_NGBS) / ENG_CAP(XYR,I_NGBS)
               ELSE
                  ENG_FOM(XYR,I_NGBS) = 100.0
               END IF
            END DO
            IF (CAP_NGBS(I_NGBS) .GT. 0.0) THEN
               ENG_CF(I_NGBS) = ENG_CF(I_NGBS) / CAP_NGBS(I_NGBS)
               IF (ENG_ECP(I_NGBS) .EQ. WIEC) THEN
                  ENG_CF(I_NGBS) = MAX(ENG_CF(I_NGBS),0.25)
               ELSEIF (ENG_ECP(I_NGBS) .EQ. WIA2) THEN
                  ENG_CF(I_NGBS) = MAX(ENG_CF(I_NGBS),0.50)
               ELSE
                  ENG_CF(I_NGBS) = MAX(ENG_CF(I_NGBS),0.06)
               END IF
               ENG_VOM(I_NGBS) = ENG_VOM(I_NGBS) / CAP_NGBS(I_NGBS)
               DO ISP = 1 , ECP_D_MSP
                  ENG_SP_CAP_FAC(ISP,I_NGBS) = ENG_SP_CAP_FAC(ISP,I_NGBS) / CAP_NGBS(I_NGBS)
               END DO
            ELSE
               ENG_CF(I_NGBS) = 0.001
               ENG_VOM(I_NGBS) = 1.0
               DO ISP = 1 , ECP_D_MSP
                  ENG_SP_CAP_FAC(ISP,I_NGBS) = 1.0
               END DO
            END IF

!           IF (CAP_NGBS(I_NGBS) .GT. 0.0) THEN
!              WRITE(6,8113) CURIRUN, CURIYR+UHBSYR, I_NGBS, IGRP, ENG_GRP(I_NGBS), ENG_RG(I_NGBS), ENG_FLRG(I_NGBS), &
!                 ENG_FREC(I_NGBS), ENG_ECP(I_NGBS), ENG_MR(I_NGBS), &
!                 (ENG_CAP(XYR,I_NGBS), ENG_FOM(XYR,I_NGBS), XYR=1,UNXPH), &
!                 ENG_CF(I_NGBS), ULHTRT_ECP(ENG_GRP(I_NGBS),1), ENG_VOM(I_NGBS), ENG_RVAL(I_NGBS)
!8113          FORMAT(1X,"ENG_DAT2",10(":",I5),15(":",F10.3))
!           END IF

         END DO
      END IF
!
!     Average Nuclear Plant Data across Multiple Records if Any
!
      IF (IRG .EQ. UNRGNS) THEN
         DO I_NUC = 1 , NUM_NUC
            MXSHR = 0.0
            DO TRG = 1 , UNRGNS
               MXSHR = MXSHR + ENUC_RG(TRG,I_NUC)
            END DO
            DO TRG = 1 , UNRGNS
               IF (MXSHR .GT. 1.0) ENUC_RG(TRG,I_NUC) = ENUC_RG(TRG,I_NUC) / MXSHR
            END DO
            DO XYR = 1 , UNXPH
               IF (ENUC_CAP(XYR,I_NUC) .GT. 0.0) THEN
                  ENUC_FOM(XYR,I_NUC) = ENUC_FOM(XYR,I_NUC) / ENUC_CAP(XYR,I_NUC)
                  ENUC_CF(XYR,I_NUC) = ENUC_CF(XYR,I_NUC) / ENUC_CAP(XYR,I_NUC)
!                 ENUC_CF(XYR,I_NUC) = MAX(ENUC_CF(XYR,I_NUC),0.60)
               ELSE
                  ENUC_FOM(XYR,I_NUC) = 100.0
                  ENUC_CF(XYR,I_NUC) = 0.001
               END IF
            END DO
            DO XYR = UNXPH + 1 , UNFPH
               IF (ENUC_CAP(UNXPH,I_NUC) .GT. 0.0) THEN
                  ENUC_FOM(XYR,I_NUC) = ENUC_FOM(XYR,I_NUC) / ENUC_CAP(UNXPH,I_NUC)
               ELSE
                  ENUC_FOM(XYR,I_NUC) = 100.0
               END IF
            END DO
            IF (CAP_NUC2(I_NUC) .GT. 0.0) THEN
               ENUC_HTRT(I_NUC) = ENUC_HTRT(I_NUC) / CAP_NUC2(I_NUC)
               ENUC_VOM(I_NUC) = ENUC_VOM(I_NUC) / CAP_NUC2(I_NUC)
               ENUC_PTC(I_NUC) = ENUC_PTC(I_NUC) / CAP_NUC2(I_NUC)
               ENUC_ACST(I_NUC) = ENUC_ACST(I_NUC) / CAP_NUC2(I_NUC)
!     write(6,4434) curiyr+1989,i_nuc,enuc_grp(i_nuc),cap_nuc2(i_nuc),enuc_cap(1,i_nuc),enuc_fom(1,i_nuc),enuc_vom(i_nuc),enuc_asyr(i_nuc),enuc_acst(i_nuc)
!4434 format(1h ,'!nucageo',i4,i5,i6,2f10.1,2f10.3,i5,f10.3)
               DO ISP = 1 , ECP_D_MSP
                  ENUC_SP_CAP_FAC(ISP,I_NUC) = ENUC_SP_CAP_FAC(ISP,I_NUC) / CAP_NUC2(I_NUC)
               END DO
            ELSE
               ENUC_HTRT(I_NUC) = 10623.0
               ENUC_VOM(I_NUC) = 1.0
               ENUC_PTC(I_NUC) = 0.0
               ENUC_ACST(I_NUC) = 0.0
               DO ISP = 1 , ECP_D_MSP
                  ENUC_SP_CAP_FAC(ISP,I_NUC) = 1.0
               END DO
            END IF
            IF (UF_DBG .GT. 0) THEN
               DO TRG = 1 , UNRGNS
                  IF (ENUC_RG(TRG,I_NUC) .GT. 0.0) THEN
                     WRITE(UF_DBG,5113) CURIYR+UHBSYR,I_NUC,ENUC_GRP(I_NUC),TRG, &
                        ENUC_FREC(I_NUC),ENUC_ECP(I_NUC),ENUC_RG(TRG,I_NUC), &
                        (ENUC_CAP(XYR,I_NUC),ENUC_FOM(XYR,I_NUC),ENUC_CF(XYR,I_NUC),XYR=1,UNXPH), &
                        ENUC_HTRT(I_NUC),ENUC_VOM(I_NUC),ENUC_RVAL(I_NUC),ENUC_PTC(I_NUC)
 5113                FORMAT(1X,"ENUC_DAT2",6(":",I5),26(":",F10.3))
                  END IF
               END DO
            END IF
         END DO
      END IF
!
!     Collect Info to Calculate Average Max Capacity Factor for Dispatchable Capacity
!
      DO IECP = 1 , ECP_D_DSP
         ITYP = UPTTYP(IECP)
         IF (MAXCF_WGT(IECP) .GT. 0.0) THEN
            UPMAXCF(IECP,IRG,CURIYR) = MAXCF_TOT(IECP) / MAXCF_WGT(IECP)
         ELSE IF (MAXCF_WGT_2(IECP) .GT. 0.0) THEN
            UPMAXCF(IECP,IRG,CURIYR) = MAXCF_TOT_2(IECP) / MAXCF_WGT_2(IECP)
         ELSE
            UPMAXCF(IECP,IRG,CURIYR) = UPMCF(IECP)
         END IF
!
         MAXCF = UPMAXCF(IECP,IRG,CURIYR)
         MAXCF = MIN(MAX(MAXCF,0.001),0.998)
!
!        Make PMR, FOR and implied LFR Consistent with MAXCF,
!        Unless LFR Exceeds maximum specified then Use Max LFR and Split difference between FOR and PMR
!
         TFOR = UPFORT(IECP)
         TPMR = UPPMRT(IECP)
         TLFR = 1.0 - (UPMCF(IECP) / ((1.0 - TFOR) * (1.0 - TPMR)))
         FCTR = (MAXCF / UPMCF(IECP)) ** (DBLE(1.0) / DBLE(3.0))
         TLFR = DBLE(1.0) - FCTR * (DBLE(1.0) - TLFR)
         IF (TLFR .LE. MXLFR .AND. TLFR .GE. 0.0) THEN
            TFOR = DBLE(1.0) - FCTR * (DBLE(1.0) - TFOR)
            TPMR = DBLE(1.0) - FCTR * (DBLE(1.0) - TPMR)
         ELSE IF (TLFR .LT. 0.0) THEN
            FCTR = (MAXCF / UPMCF(IECP)) ** (DBLE(1.0) / DBLE(2.0))
            TFOR = DBLE(1.0) - FCTR * (DBLE(1.0) - TFOR)
            TPMR = DBLE(1.0) - FCTR * (DBLE(1.0) - TPMR)
            TLFR = 0.0
         ELSE
            TLFR = MXLFR
            FCTR = (MAXCF / ((1.0 - WFOR(IEFD)) * (1.0 - WPMR(IEFD)) * (1.0 - TLFR))) ** 0.5
            TFOR = DBLE(1.0) - FCTR * (DBLE(1.0) - TFOR)
            TPMR = DBLE(1.0) - FCTR * (DBLE(1.0) - TPMR)
         END IF
!
         IF (TLFR .LT. 0.0 .OR. TFOR .LT. 0.0 .OR. TPMR .LE. 0) THEN
            TLFR = 1.0 - (MAXCF ** (1.0 / 3.0))
            TFOR = 1.0 - (MAXCF ** (1.0 / 3.0))
            TPMR = 1.0 - (MAXCF ** (1.0 / 3.0))
         END IF
!
         UPMAXCF(IECP,IRG,CURIYR) = MAXCF
         UP_XFOR(IECP,IRG,CURIYR) = TFOR
         UP_XPMR(IECP,IRG,CURIYR) = TPMR
!
         WRITE(18,3492) CURIYR+UHBSYR,CURITR,IRG,IECP,MAXCF_WGT(IECP),UPMAXCF(IECP,IRG,CURIYR),UP_XFOR(IECP,IRG,CURIYR),UP_XPMR(IECP,IRG,CURIYR)
 3492    FORMAT(1X,"UPMAXCF",4(":",I4),4(":",E13.6))
      END DO

!     Average Coal Plant Data across Multiple Records if Any
!
      IF (IRG .EQ. UNRGNS ) THEN
         DO I_COAL = 1 , NUM_CL
            IF (FULLCYR .EQ. UESTYR) THEN
               IF (MIN_ECPt(I_COAL) .LE. 0 .OR. MIN_ECPt(I_COAL) .GT. NCLUT1) THEN
                  WRITE(22,5222) CURIYR+UHBSYR,I_COAL,EMM_CL_UNITS(I_COAL),MIN_ECPt(I_COAL)
 5222             FORMAT(1X,"COAL_UNIT_WITH_NO_ECP_TYPE",2(":",I4),":",A10,":",I3)
               END IF
!
               DO XYR = 1 , UNYEAR
                  IF (EMM_CL_ECPt(I_COAL,XYR) .LE. 0) THEN
                     EMM_CL_ECPt(I_COAL,XYR) = MIN_ECPt(I_COAL)
                  END IF
               END DO
               DO TRG = 1 , UNRGNS
                  DO XYR = 1 , UNYEAR
                     ECL_RG_CAP(TRG,I_COAL) = MAX(ECL_RG_CAP(TRG,I_COAL) , T_RG_CAP(XYR,TRG,I_COAL))
                  END DO
                  ECL_RG_CAP(MNUMNR,I_COAL) = ECL_RG_CAP(MNUMNR,I_COAL) + ECL_RG_CAP(TRG,I_COAL)
               END DO
               IF (ECL_RG_CAP(MNUMNR,I_COAL) .LE. 0.0) THEN
                  WRITE(22,5223) I_COAL,EMM_CL_UNITS(I_COAL)
 5223             FORMAT(1X,"NO CAPACITY FOR THE FOLLOWING COAL UNIT",":",I5,":",A10)
                  ECL_RG(I_COAL) = 0
               ELSE
                  IF (ECL_RG(I_COAL) .GT. 0) THEN
                     TRG = ECL_RG(I_COAL)
                     IF (ECL_RG_CAP(TRG,I_COAL) / ECL_RG_CAP(MNUMNR,I_COAL) .LT. 0.50) THEN
                        WRITE(6,5224) I_COAL,EMM_CL_UNITS(I_COAL),TRG,ECL_RG_CAP(TRG,I_COAL)/ECL_RG_CAP(MNUMNR,I_COAL), &
                           ECL_RG_CAP(TRG,I_COAL),ECL_RG_CAP(MNUMNR,I_COAL)
 5224                   FORMAT(1X,"Maximum capacity share is not in the operator region for the following COAL UNIT",":",I5,":",A10,":",I3,3(":",F9.3))
                     END IF
                  ELSE
                     RG_SHR = 0.0
                     DO TRG = 1 , UNRGNS
                        IF (ECL_RG_CAP(TRG,I_COAL) / ECL_RG_CAP(MNUMNR,I_COAL) .GT. RG_SHR) THEN
                           ECL_RG(I_COAL) = TRG
                           RG_SHR = ECL_RG_CAP(TRG,I_COAL) / ECL_RG_CAP(MNUMNR,I_COAL)
                        END IF
                     END DO
                     TRG = ECL_RG(I_COAL)
                     WRITE(18,5225) I_COAL,EMM_CL_UNITS(I_COAL),TRG,ECL_RG_CAP(TRG,I_COAL)/ECL_RG_CAP(MNUMNR,I_COAL), &
                        ECL_RG_CAP(TRG,I_COAL),ECL_RG_CAP(MNUMNR,I_COAL)
 5225                FORMAT(1X,"Operator region assigned to owner region with maximum capacity share for the following COAL UNIT",":",I5,":",A10,":",I3,3(":",F9.3))
                  END IF
               END IF
            END IF
            KRG = ECL_RG(I_COAL)
            IF (KRG .GT. 0) THEN
            IF (C_CAP(I_COAL) .GT. 0.0) THEN
               ECL_CMB_O(I_COAL) = ECL_CMB_O(I_COAL) / C_CAP(I_COAL)
               ECL_CMB_F(I_COAL) = ECL_CMB_F(I_COAL) / C_CAP(I_COAL)
               ECL_CMB_V(I_COAL) = ECL_CMB_V(I_COAL) / C_CAP(I_COAL)
               ECL_CMB_R(I_COAL) = ECL_CMB_R(I_COAL) / C_CAP(I_COAL)
               ECL_SNCR_O(I_COAL) = ECL_SNCR_O(I_COAL) / C_CAP(I_COAL)
               ECL_SNCR_F(I_COAL) = ECL_SNCR_F(I_COAL) / C_CAP(I_COAL)
               ECL_SNCR_V(I_COAL) = ECL_SNCR_V(I_COAL) / C_CAP(I_COAL)
               ECL_SNCR_R(I_COAL) = ECL_SNCR_R(I_COAL) / C_CAP(I_COAL)
               ECL_SCR_O(I_COAL) = ECL_SCR_O(I_COAL) / C_CAP(I_COAL)
               ECL_SCR_F(I_COAL) = ECL_SCR_F(I_COAL) / C_CAP(I_COAL)
               ECL_SCR_V(I_COAL) = ECL_SCR_V(I_COAL) / C_CAP(I_COAL)
               ECL_SCR_R(I_COAL) = ECL_SCR_R(I_COAL) / C_CAP(I_COAL)
               ECL_FGD_O(I_COAL) = ECL_FGD_O(I_COAL) / C_CAP(I_COAL)
               ECL_DSI_O(I_COAL) = ECL_DSI_O(I_COAL) / C_CAP(I_COAL)
               ECL_DSI_F(I_COAL) = ECL_DSI_F(I_COAL) / C_CAP(I_COAL)
               ECL_DSI_V(I_COAL) = ECL_DSI_V(I_COAL) / C_CAP(I_COAL)
               ECL_DSI_R(I_COAL) = ECL_DSI_R(I_COAL) / C_CAP(I_COAL)
               ECL_CCS_O(I_COAL) = ECL_CCS_O(I_COAL) / C_CAP(I_COAL)
               ECL_CCS_F(I_COAL) = ECL_CCS_F(I_COAL) / C_CAP(I_COAL)
               ECL_CCS_V(I_COAL) = ECL_CCS_V(I_COAL) / C_CAP(I_COAL)
               ECL_CCS_R(I_COAL) = ECL_CCS_R(I_COAL) / C_CAP(I_COAL)
               ECL_CCS_H(I_COAL) = ECL_CCS_H(I_COAL) / C_CAP(I_COAL)
               ECL_CCS_C(I_COAL) = ECL_CCS_C(I_COAL) / C_CAP(I_COAL)
               ECL_FF_O(I_COAL) = ECL_FF_O(I_COAL) / C_CAP(I_COAL)
               ECL_FF_F(I_COAL) = ECL_FF_F(I_COAL) / C_CAP(I_COAL)
               ECL_FF_V(I_COAL) = ECL_FF_V(I_COAL) / C_CAP(I_COAL)
               ECL_HRI_O(I_COAL) = ECL_HRI_O(I_COAL) / C_CAP(I_COAL)
               ECL_HRI_F(I_COAL) = ECL_HRI_F(I_COAL) / C_CAP(I_COAL)
               ECL_HRI_V(I_COAL) = ECL_HRI_V(I_COAL) / C_CAP(I_COAL)
               ECL_HRI_H(I_COAL) = ECL_HRI_H(I_COAL) / C_CAP(I_COAL)
               ECL_ESP_O(I_COAL) = ECL_ESP_O(I_COAL) / C_CAP(I_COAL)
               ECL_CFB_O(I_COAL) = ECL_CFB_O(I_COAL) / C_CAP(I_COAL)
               ECL_CFB_F(I_COAL) = ECL_CFB_F(I_COAL) / C_CAP(I_COAL)
               ECL_CFB_V(I_COAL) = ECL_CFB_V(I_COAL) / C_CAP(I_COAL)
               ECL_CL_NG_COST(I_COAL) = ECL_CL_NG_COST(I_COAL) / C_CAP(I_COAL)
               ECL_CL_NG_TRAN(I_COAL) = ECL_CL_NG_TRAN(I_COAL) / C_CAP(I_COAL)
               DO ISP = 1 , ECP_D_MSP
                  ECL_SP_CAP_FAC(ISP,I_COAL) = ECL_SP_CAP_FAC(ISP,I_COAL) / C_CAP(I_COAL)
               END DO
            ELSE
               DO ISP = 1 , ECP_D_MSP
                  ECL_SP_CAP_FAC(ISP,I_COAL) = 1.0
               END DO
            END IF
            DO XYR = 1 , UNXPH
               IECP = ECL_ECP(XYR,I_COAL)
               IF (IECP .EQ. 0) THEN
                  IECP = C_ECP(I_COAL)
                  ECL_ECP(XYR,I_COAL) = IECP
                  ECL_GRP(XYR,I_COAL) = C_GRP(I_COAL)
                  ECL_CF(XYR,I_COAL) = C_CF(I_COAL)
                  DO XNOX = 1 , NOX_GRP
                     ECL_NXG(XYR,XNOX,I_COAL) = C_NXG(XNOX,I_COAL)
                  END DO
                  ECL_FOM(XYR,I_COAL) = C_FOM(I_COAL)
                  ECL_NXR(XYR,I_COAL) = C_NXR(I_COAL)
                  ECL_SO2R(XYR,I_COAL) = C_SO2R(I_COAL)
                  IF (SV_RCFG(XYR,I_COAL) .GT. 0) THEN
                     ECL_RCFG(XYR,I_COAL) = SV_RCFG(XYR,I_COAL)
                  ELSE
                     ECL_RCFG(XYR,I_COAL) = ECL_ICFG(I_COAL)
                  END IF
                  T_VCST = 0.0
                  ECP_CF = C_CF(I_COAL)
               ELSE
                  ITYP = UPTTYP(IECP)
                  IF (ECL_CAP(XYR,I_COAL) .GT. 0.0) THEN
                     ECL_FOM(XYR,I_COAL) = ECL_FOM(XYR,I_COAL) / ECL_CAP(XYR,I_COAL)
                     ECL_NXR(XYR,I_COAL) = ECL_NXR(XYR,I_COAL) / ECL_CAP(XYR,I_COAL)
                     ECL_SO2R(XYR,I_COAL) = ECL_SO2R(XYR,I_COAL) / ECL_CAP(XYR,I_COAL)
                  ELSE
                     ECL_FOM(XYR,I_COAL) = 100.0
                     ECL_NXR(XYR,I_COAL) = 10.0
                     ECL_SO2R(XYR,I_COAL) = 0.0001
                  END IF
!
                  IF (T_GEN(ITYP,KRG) .GT. 0.0) THEN
                     A_HR = UECP_HTRT_ADJ(IECP) * T_HRATE(ITYP,KRG) / T_GEN(ITYP,KRG)
                  ELSE
                     A_HR = 18000.0
                  END IF
!
                  IF (T_CAP(ITYP,KRG) .GT. 0.0) THEN
                     T_VCST = 1000.0 * T_AVG(ITYP,KRG) / T_CAP(ITYP,KRG)
                     ECP_CF = T_GEN(ITYP,KRG)  / (8.76 * T_CAP(ITYP,KRG))

                     WRITE(18,3317) CURIYR+UHBSYR, XYR, I_COAL, ECL_IGRP(I_COAL), ECL_GRP(XYR,I_COAL), ITYP, IECP, KRG, &
                        T_VCST, T_AVG(ITYP,KRG), T_CAP(ITYP,KRG), ECL_VADJ(XYR,I_COAL), ULHTRT_ECP(ECL_GRP(XYR,I_COAL),XYR), A_HR, ECP_CF, T_GEN(ITYP,KRG), &
                        UECP_HTRT_ADJ(IECP), UECP_CPEN_ADJ(IECP), ECL_FOM(XYR,I_COAL)
 3317                FORMAT(1X,"ECL_VADJ_2", 8(":",I5), 14(":",F12.3))

                  ELSE
                     T_VCST = 0.0
!                    ECL_VADJ(XYR,I_COAL) = 0.0
                     ECP_CF = 0.0
                  END IF
               END IF
            END DO
            IF (IECP .GT. 0) THEN
               ACT_CF = 0.0
               IF (ECL_GRP(1,I_COAL) .GT. 0 .AND. ULCAPC(ECL_GRP(1,I_COAL)) .GT. 0.0) THEN
                  ACT_CF = ULTGEN(ECL_GRP(1,I_COAL)) / (8.76 * ULCAPC(ECL_GRP(1,I_COAL)))
               END IF
                  IF (UF_DBG .GT. 0) THEN
                     DO TRG = 1 , UNRGNS
                        IF (ECL_RG_CAP(TRG,I_COAL) .GT. 0.0) THEN
                           WRITE(UF_DBG,2113) CURIYR+UHBSYR,ECL_IGRP(I_COAL),ECL_GRP(1,I_COAL),ECL_YR(I_COAL),I_COAL, &
                              ECL_ICFG(I_COAL),ECL_YR(I_COAL),ECL_RG(I_COAL),ECL_CLRG(I_COAL), &
                              ECL_FREC(I_COAL),ECL_ECP(1,I_COAL),ECL_IGRP(I_COAL),ECL_MGRP(I_COAL),C_CAP(I_COAL),T_VCST, &
                              ECL_VADJ(1,I_COAL),ECL_FOM(1,I_COAL),ECL_NXR(1,I_COAL),ECL_SO2R(1,I_COAL), &
                              (ECL_RCFG(XYR,I_COAL),ECL_CAP(XYR,I_COAL),XYR=1,UNXPH),ECL_CF(1,I_COAL),ACT_CF,            &
                              UPMCF(IECP),ECP_CF, ULHTRT_ECP(ECL_GRP(1,I_COAL),1), A_HR,UPTTYP(ECL_ECP(1,I_COAL)),TRG, &
                              ECL_RG_CAP(TRG,I_COAL),ECL_RG_CAP(MNUMNR,I_COAL),ECL_SP_CAP_FAC(2,I_COAL)
 2113                      FORMAT(1X,"ECL_DAT2",13(":",I5),6(":",F10.3),<UNXPH>(":",I3,":",F10.3),4(":",F10.3),2(":",F10.0),":",I2,":",I3,3(":",F10.3))
!                          Label:ECL_DAT2:CYEAR:ECL_IGRP:ECL_GRP1:ECL_YR:I_COAL:ECL_ICFG:ECL_YR:ECL_RG:ECL_CLRG:ECL_FREC:ECL_ECP1:ECL_IGRP2:ECL_MGRP:C_CAP:T_VCST:ECL_VADJ1:ECL_FOM1:ECL_NXR1:ECL_SO2R1:ECL_RCFG1:ECL_CAP1:ECL_RCFG2:ECL_CAP2:ECL_RCFG3:ECL_CAP3:ECL_CF1:ACT_CF:UPMCF:ECP_CF:ULHTRT_ECP_1:A_HR:UPTTYP1:TRG:ECL_RG_CAP:ECL_TOT_CAP:ECL_SP_CAP_FAC2
                        END IF
                     END DO
                  END IF
               END IF
            END IF ! KRG greater than zero
         END DO
         DO CRG = 1 , NDREG
            DO T_COF = 1 , ECP_D_RCF
               ABTU = UPCFBTU(T_COF,CRG)
               IF (UCF_TCAP(T_COF,CRG,CURIYR) .GT. 0.0) THEN
                  UPCFBTU(T_COF,CRG) = UPCFBTU(T_COF,CRG) / UCF_TCAP(T_COF,CRG,CURIYR)
               ELSE
                  UPCFBTU(T_COF,CRG) = 0.0
               END IF
               WRITE(18,2732) CURIYR+UHBSYR,CRG,T_COF,UCF_TCAP(T_COF,CRG,CURIYR),UCF_RCAP(T_COF,CRG,CURIYR),ABTU,UPCFBTU(T_COF,CRG)
 2732          FORMAT(1X,"AVG_BTU_",3(":",I4),4(":",F15.3))
               WRITE(18,2733) CURIYR+UHBSYR,CRG,T_COF,(UCF_TCAP1(T_COF,KRG,CRG,CURIYR),KRG = 1 , MNUMNR - 1)
 2733          FORMAT(1X,"COF_CAP_",3(":",I4),<mnumnr>(":",F8.1))
            END DO
         END DO
      END IF
!
!     DETERMINE COAL SHARE BY FUEL REGION AND COAL REGION TO SPLIT CTL CO2 EMISSIONS
!
      IF (IRG .EQ. UNRGNS)THEN
            DO TRG = 1 , NDREG
               DO FRG = 1 , MAXNFR
                  IF (CLCONFC(TRG,MAXNFR + 1) .GT. 0.0)THEN
                     CLCONFC(TRG,FRG) = CLCONFC(TRG,FRG) / CLCONFC(TRG,MAXNFR + 1)
                  ELSE
                     CLCONFC(TRG,FRG) = 0.0
                  END IF
               END DO
            END DO
      END IF
!
!     DETERMINE GEO, MSW, AND DG GENERATION AS A SHARE OF CARBON REGION AND EMM REGION
!
      IF (IRG .EQ. UNRGNS)THEN
            DO FRG = 1 , UNRGNS
            DO GRP = 1 , CO2_GRP
!              GEO
               IF (CO2_GEOT(FRG) .GT. 0.0)THEN
                  CO2_GEO(GRP,FRG) = CO2_GEO(GRP,FRG) / CO2_GEOT(FRG)
               END IF
!              MSW
               IF (CO2_MSWT(FRG) .GT. 0.0)THEN
                  CO2_MSW(GRP,FRG) = CO2_MSW(GRP,FRG) / CO2_MSWT(FRG)
               END IF
!              DGN
               IF (CO2_DGNT(FRG) .GT. 0.0)THEN
                  CO2_DGN(GRP,FRG) = CO2_DGN(GRP,FRG) / CO2_DGNT(FRG)
               END IF
            END DO
         END DO
      END IF

!     SHOW EPECAP AND EPGCAP

      DO IECP = 1 , ECP_D_CAP
         DO YEAR = 1 , UNXPH
            WRITE(18,1316) CURIYR+UHBSYR,IRG,IECP,UPLNTCD(IECP),YEAR,(EPECAP(FRG,IECP,YEAR),FRG=0,UNFRGN)
 1316       FORMAT(1X,"EPECAP",3(":",I4),":",A2,":",I4,26(":",F9.3))
         END DO
         DO RGRP = 1 , RET_GRP(IECP)
            WRITE(18,1318) CURIYR+UHBSYR,IRG,IECP,UPLNTCD(IECP),RGRP,(EPGCAP(FRG,RGRP,IECP),FRG=0,UNFRGN)
 1318       FORMAT(1X,"EPGCAP",3(":",I4),":",A2,":",I4,26(":",F9.3))
         END DO
      END DO
      if (irg .eq. UNRGNS) then
         do iecp = 1 , ECP_D_DSP
          if ((curiyr + uhbsyr) .le. uestyr)then
            do krg = 1 , unrgns
               if (totgenpn(iecp,krg) .gt. 0.0) then

!                 write(6,3331) curirun, curiyr+1989, curitr, krg, iecp, uplntcd(iecp), cofgenpn(iecp,krg), totgenpn(iecp,krg), &
!                    cofgenpn(iecp,krg) / totgenpn(iecp,krg)
!3331             format(1X,'!cfshrn',5(":",i4),":",a3,3(":",F15.3))

                  cofgenpn(iecp,krg) = cofgenpn(iecp,krg) / totgenpn(iecp,krg)
               else
                  cofgenpn(iecp,krg) = 0.0
               end if
            end do
            do crg = 1 , ndreg
               if (totgenpc(iecp,crg) .gt. 0.0)then

!                 write(6,3334) curirun, curiyr+1989, curitr, crg, iecp, uplntcd(iecp), cofgenpc(iecp,crg), totgenpc(iecp,crg), &
!                    cofgenpc(iecp,crg) / totgenpc(iecp,crg)
!3334             format(1X,'!cfshrc',5(":",i4),":",a3,3(":",F15.3))

                  cofgenpc(iecp,crg) = cofgenpc(iecp,crg) / totgenpc(iecp,crg)
               else
                  cofgenpc(iecp,crg) = 0.0
               end if
            end do
          else
            do krg = 1 , unrgns
               cofgenpn(iecp,krg) = cofgenpn(iecp,krg) * cfadj(curiyr) / cfadj(curiyr - 1)
            end do
            do crg = 1 , ndreg
               cofgenpc(iecp,crg) = cofgenpc(iecp,crg) * cfadj(curiyr) / cfadj(curiyr - 1)
!                 write(6,3334) curirun, curiyr+1989, curitr, crg, iecp, uplntcd(iecp), cofgenpc(iecp,crg)
            end do
          end if
         end do
!        STATE LEVEL BTU SHARES BY PLANT TYPE AND TRANSPORT REGION
         DO KRG = 1 , TSO2_NST
            TRN = TSO2_TR_BY_ST(KRG)
            COA = TSO2_CL_BY_ST(KRG)
!           do iecp = 1 , wiis
!              write(6,4433) curiyr+1989,tso2_st(krg),coa,uplntcd(iecp),tbtu_shr_by_st(krg,iecp),  &
!                            tbtu_shr_by_clrg(coa,iecp,1),  &
!                            tbtu_shr_by_clrg(coa,iecp,2),  &
!                            tbtu_shr_by_clrg(coa,iecp,3),  &
!                            tbtu_shr_by_clrg(coa,iecp,1) + tbtu_shr_by_clrg(coa,iecp,2) + tbtu_shr_by_clrg(coa,iecp,3)
!4433 format(1h ,'!btust',i4,1x,a2,i3,1x,a2,8f10.1)
!           end do
!           iecp = ECP_D_DSP + 1
!              write(6,4434) curiyr+1989,tso2_st(krg),coa,tbtu_shr_by_st(krg,iecp),  &
!                            tbtu_shr_by_clrg(coa,iecp,1),  &
!                            tbtu_shr_by_clrg(coa,iecp,2),  &
!                            tbtu_shr_by_clrg(coa,iecp,3),  &
!                            tbtu_shr_by_clrg(coa,iecp,1) + tbtu_shr_by_clrg(coa,iecp,2) + tbtu_shr_by_clrg(coa,iecp,3)
!4434 format(1h ,'!btust',i4,1x,a2,i3,1x,'TL',8f10.1)
            IECP = ECP_D_DSP + 1
            CLBTU = 0.0
            DO TRN = 1 , MX_SO2_GRP
               CLBTU = CLBTU + TBTU_SHR_BY_CLRG(COA,IECP,TRN)
            END DO
               TBTU_SHR_BY_ST(KRG,IECP) = TBTU_SHR_BY_ST(KRG,ECP_D_DSP + 1) / CLBTU
            DO IECP = 1 , WIIS
               PLBTU = 0.0
               DO TRN = 1 , MX_SO2_GRP
                  PLBTU = PLBTU + TBTU_SHR_BY_CLRG(COA,IECP,TRN)
               END DO
               IF (PLBTU .GT. 0.0)THEN
                  TBTU_SHR_BY_ST(KRG,IECP) = TBTU_SHR_BY_ST(KRG,IECP) / PLBTU
               ELSE
                  TBTU_SHR_BY_ST(KRG,IECP) = TBTU_SHR_BY_ST(KRG,ECP_D_DSP + 1)
               END IF
!              write(6,5433) curiyr+1989,tso2_st(krg),coa,uplntcd(iecp),tbtu_shr_by_st(krg,iecp),  &
!                            tbtu_shr_by_clrg(coa,iecp,1),  &
!                            tbtu_shr_by_clrg(coa,iecp,2),  &
!                            tbtu_shr_by_clrg(coa,iecp,3),  &
!                            plbtu
!5433 format(1h ,'!shrst',i4,1x,a2,i3,1x,a2,f10.4,5f10.1)
            END DO
!           iecp = ECP_D_DSP + 1
!           write(6,5434) curiyr+1989,tso2_st(krg),coa,tbtu_shr_by_st(krg,iecp),  &
!                         tbtu_shr_by_clrg(coa,iecp,1),  &
!                         tbtu_shr_by_clrg(coa,iecp,2),  &
!                         tbtu_shr_by_clrg(coa,iecp,3),  &
!                         clbtu
!5434 format(1h ,'!shrst',i4,1x,a2,i3,1x,'TL',f10.4,5f10.1)
         END DO
         DO KRG = 1 , NDREG
!           do iecp = 1 , wiis
!              write(6,4431) curiyr+1989,krg,uplntcd(iecp),(tbtu_shr_by_clrg(krg,iecp,trn),trn=1,3)
!4431 format(1h ,'!btucl',i4,i3,1x,a2,3f10.1)
!           end do
!           iecp = ECP_D_DSP + 1
!           write(6,4432) curiyr+1989,krg,(tbtu_shr_by_clrg(krg,iecp,trn),trn=1,3)
!4432 format(1h ,'!btucl',i4,i3,1x,'TL',3f10.1)
            IECP = ECP_D_DSP + 1
            CLBTU = 0.0
            DO TRN = 1 , MX_SO2_GRP
               CLBTU = CLBTU + TBTU_SHR_BY_CLRG(KRG,IECP,TRN)
            END DO
            DO TRN = 1 , MX_SO2_GRP
               TBTU_SHR_BY_CLRG(KRG,IECP,TRN) = TBTU_SHR_BY_CLRG(KRG,IECP,TRN) / CLBTU
            END DO
            DO IECP = 1 , WIIS
               PLBTU = 0.0
               DO TRN = 1 , MX_SO2_GRP
                  PLBTU = PLBTU + TBTU_SHR_BY_CLRG(KRG,IECP,TRN)
               END DO
               DO TRN = 1 , MX_SO2_GRP
                  IF (PLBTU .GT. 0.0)THEN
                     TBTU_SHR_BY_CLRG(KRG,IECP,TRN) = TBTU_SHR_BY_CLRG(KRG,IECP,TRN) / PLBTU
                  ELSE
                     TBTU_SHR_BY_CLRG(KRG,IECP,TRN) = TBTU_SHR_BY_CLRG(KRG,ECP_D_DSP + 1,TRN)
                  END IF
               END DO
!              write(6,5431) curiyr+1989,krg,uplntcd(iecp),  &
!              so2_shr_by_clrg(krg,1),tso2_shr_by_clrg(krg,1),tbtu_shr_by_clrg(krg,iecp,1),  &
!              so2_shr_by_clrg(krg,2),tso2_shr_by_clrg(krg,2),tbtu_shr_by_clrg(krg,iecp,2)
!5431 format(1h ,'!shrcl',i4,i3,1x,a2,6f10.4)
            END DO
!           iecp = ECP_D_DSP + 1
!           write(6,5432) curiyr+1989,krg,  &
!                         ( so2_shr_by_clrg(krg,trn),trn=1,1),  &
!                         (tso2_shr_by_clrg(krg,trn),trn=1,1),  &
!                         (tbtu_shr_by_clrg(krg,iecp,trn),trn=1,1),  &
!                         ( so2_shr_by_clrg(krg,trn),trn=2,2),  &
!                         (tso2_shr_by_clrg(krg,trn),trn=2,2),  &
!                         (tbtu_shr_by_clrg(krg,iecp,trn),trn=2,2)
!5432 format(1h ,'!shrcl',i4,i3,1x,'TL',10f10.4)
         END DO
!     print *,'!styr',curiyr+uhbsyr,uestyr
!        IF ((CURIYR + 1989) .EQ. UESTYR)THEN
!           DO KRG = 1 , EMM_D_ST
!              write(6,4567) krg,ustnme(krg),est_frg(krg)
!4567 format(1h ,'!stfrg',i4,a4,i4)
!              write(6,4568) krg,ustnme(krg),est_frg(krg),(emap_stfr(iecp,krg),iecp=1,unfrgn)
!4568 format(1h ,'!stmap',i4,a4,i4,23f4.0)
!           END DO
!              write(6,4569) krg,EFD_D_MFRG+1,(emap_stfr(iecp,krg),iecp=1,unfrgn)
!4569 format(1h ,'!stmap',i4,'  TL',i4,23f4.0)
!              write(6,4568) krg,(emap_stfr(iecp,krg),iecp=1,unfrgn)
!4568 format(1h ,'!stmap',i3,50f3.0)
!              write(6,4568) (ustnme(krg),krg=1,EMM_D_ST)
!4568 format(1h ,'!stmap','Reg',49a3,' TL')
!           DO IECP = 1 , UNFRGN
!              write(6,4569) iecp,(emap_stfr(iecp,krg),krg=1,EMM_D_ST+1)
!4569 format(1h ,'!stmap',i3,50f3.0)
!           END DO
!        END IF
      end if
!
!     FOR 111d, DETERMINE AVERAGE CO2 CONTENT FOR MUST-RUN CAPACITY
!
      IF (IRG .EQ. UNRGNS .AND. CO2_STDSW .GT. 0 .AND. CO2_STDRS(1,CURIYR + UNXPH - 1) .GT. 0.0)THEN
         DO KRG = 1 , UNFRGN
            DO JYR = CURIYR , MNUMYR + ECP_D_XPH
               IF (MRGENF(KRG,JYR) .GT. 0.0)THEN
                  MRCO2F(KRG,JYR) = MRCO2F(KRG,JYR) / MRGENF(KRG,JYR)
               END IF
            END DO
         END DO
!
         DO KRG = 1 , UNRGNS
            DO JYR = CURIYR , MNUMYR + ECP_D_XPH
               IF (MRGENN(KRG,JYR) .GT. 0.0)THEN
                  MRCO2N(KRG,JYR) = MRCO2N(KRG,JYR) / MRGENN(KRG,JYR)
               END IF
            END DO
         END DO
!        DO KRG = 1 , UNFRGN
!           write(6,5678) curiyr+1989,krg,(mrgenf(krg,jyr),jyr = 2019 - uhbsyr, 2040 - uhbsyr)
!5678 format(1h ,'!mrgenf',i4,i3,22f8.1)
!           write(6,5679) curiyr+1989,krg,(mrco2f(krg,jyr),jyr = 2019 - uhbsyr, 2040 - uhbsyr)
!5679 format(1h ,'!mrco2f',i4,i3,22f8.1)
!        END DO
!        DO KRG = 1 , UNRGNS
!           write(6,6678) curiyr+1989,krg,(mrgenn(krg,jyr),jyr = 2019 - uhbsyr, 2040 - uhbsyr)
!6678 format(1h ,'!mrgenn',i4,i3,22f8.1)
!           write(6,6679) curiyr+1989,krg,(mrco2n(krg,jyr),jyr = 2019 - uhbsyr, 2040 - uhbsyr)
!6679 format(1h ,'!mrco2n',i4,i3,22f8.1)
!        END DO
      END IF

!     STORE DISPOUT FOR CURRENT YEAR AND REGION TO SAVE FIXED O&M VALUES

      CALL STROUT(CURIYR,IRG)

!     IF ((CURIYR + 1989) .EQ. 2020 .AND. IRG .EQ. UNRGNS)THEN
!        do xyr = 21 , mnumyr
!           write(6,4334) xyr + 1989,(nucplnf(krg,xyr),krg = 1 , 23)
!4334 format(1h ,'!nucfr',i4,23f6.1)
!           write(6,4335) xyr + 1989,(nucplnn(krg,xyr),krg = 1 , 22)
!4335 format(1h ,'!nucnr',i4,23f6.1)
!        end do
!     end if
!
      IF (IRG .EQ. UNRGNS)THEN
         DO FRG = 1 , UNSTAS
            IF (URG_ZECMAX(FRG) .GT. 0.0)THEN
               IF (ELNUCSBDY(FRG,MNUMNR,CURIYR) .GT. URG_ZECMAX(FRG))THEN
                  DO KRG = 1 , MNUMNR
                     IF (ELNUCSBDY(FRG,KRG,CURIYR) .GT. 0.0)THEN
                        ELNUCSBDY(UNSTAS + 1,KRG,CURIYR) = ELNUCSBDY(UNSTAS + 1,KRG,CURIYR) -  &
                                                           ELNUCSBDY(FRG,KRG,CURIYR) * (1.0 - URG_ZECMAX(FRG) / ELNUCSBDY(FRG,MNUMNR,CURIYR))
                        ELNUCSBDY(FRG,KRG,CURIYR) = ELNUCSBDY(FRG,KRG,CURIYR) * URG_ZECMAX(FRG) / ELNUCSBDY(FRG,MNUMNR,CURIYR)
                     END IF
                  END DO
               END IF
            END IF
!           write(6,6677) curiyr+1989,frg,(elnucsbdy(frg,krg,curiyr) * scalpr,krg = 1 , unrgns),elnucsbdy(frg,mnumnr,curiyr) * scalpr
!6677 format(1h ,'!zrv',i4,i3,23f6.0)
!           write(6,6678) curiyr+1989,frg,(elnucsbgn(frg,krg),krg = 1 , unrgns),elnucsbgn(frg,mnumnr)
!6678 format(1h ,'!zgn',i4,i3,23f6.0)
         END DO
!           write(6,6677) curiyr+1989,frg,(elnucsbdy(frg,krg,curiyr) * scalpr,krg = 1 , unrgns),elnucsbdy(frg,mnumnr,curiyr) * scalpr
!           write(6,6678) curiyr+1989,frg,(elnucsbgn(frg,krg),krg = 1 , unrgns),elnucsbgn(frg,mnumnr)

!        if (curiyr .eq. lastyr)then
!           do krg = 1 , mnumnr
!              write(6,7700) (xyr,xyr = upstyr,2040)
!7700 format(1h ,'!rev','rg',3x,30I7)
!              if (krg .le. unrgns .or. krg .eq. mnumnr)then
!              write(6,7710) krg,(unrvcol(krg,xyr),xyr = upstyr - uhbsyr,2040 - uhbsyr)
!7710 format(1h ,'!rev$',i3,' cl',30f10.1)
!              write(6,7720) krg,(unrvccy(krg,xyr),xyr = upstyr - uhbsyr,2040 - uhbsyr)
!7720 format(1h ,'!rev$',i3,' cc',30f10.1)
!              write(6,7730) krg,(unrvstm(krg,xyr),xyr = upstyr - uhbsyr,2040 - uhbsyr)
!7730 format(1h ,'!rev$',i3,' st',30f10.1)
!              write(6,7740) krg,(unrvnuc(krg,xyr),xyr = upstyr - uhbsyr,2040 - uhbsyr)
!7740 format(1h ,'!rev$',i3,' nu',30f10.1)
!              write(6,7750) krg,((unrvcol(krg,xyr) + unrvccy(krg,xyr) + unrvstm(krg,xyr) + unrvnuc(krg,xyr)),xyr = upstyr - uhbsyr,2040 - uhbsyr)
!7750 format(1h ,'!rev$',i3,' tl',30f10.1)
!              write(6,7760) krg,(uncpcol(krg,xyr) / 1000.0,xyr = upstyr - uhbsyr,2040 - uhbsyr)
!7760 format(1h ,'!revc',i3,' cl',30f10.1)
!              write(6,7770) krg,(uncpccy(krg,xyr) / 1000.0,xyr = upstyr - uhbsyr,2040 - uhbsyr)
!7770 format(1h ,'!revc',i3,' cc',30f10.1)
!              write(6,7780) krg,(uncpstm(krg,xyr) / 1000.0,xyr = upstyr - uhbsyr,2040 - uhbsyr)
!7780 format(1h ,'!revc',i3,' st',30f10.1)
!              write(6,7790) krg,(uncpnuc(krg,xyr) / 1000.0,xyr = upstyr - uhbsyr,2040 - uhbsyr)
!7790 format(1h ,'!revc',i3,' nu',30f10.1)
!              write(6,7800) krg,((uncpcol(krg,xyr) + uncpccy(krg,xyr) + uncpstm(krg,xyr) + uncpnuc(krg,xyr)) / 1000.0,xyr = upstyr - uhbsyr,2040 - uhbsyr)
!7800 format(1h ,'!revc',i3,' tl',30f10.1)
!              end if
!           end do
!        end if
      END IF
      
      IF (CURITR .EQ. 1 .AND. JRG .EQ. UNRGNS) THEN
          DO KRG = 1 , UNFRGN
            DO KFL= 1 , EFD_D_NFL
                CPCTNSH(KRG,KFL)=CPCTN(KRG,KFL)/CPCTNTOT(KRG)
            enddo
          enddo
      endif

      RETURN
      END
!
!
      SUBROUTINE NOX_CAA(IBTP,IRECL,IRG)
!
      IMPLICIT NONE

!     THIS SUBROUTINE DETERMINES IF CURRENT COAL UNIT IS COMPLIANT WITH NOX CAA REGULATION
!      IF NOT: ADD REQUIRED NOX CONTROLS AND SAVE THE PLANT RECORD

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'plntin'
      include'plntctl'
      include'ecp_coal'
      include'elcntl'
      include'ecpcntl'
!
      INTEGER*4 IBTP, NOXYR, IPHASE, IRECL, NRECL, JRECL, CHOICE, IRG, JGRP, IECP, I_CNFG, ITST, TEFPT, NERC
      REAL*8 TFURB, NOX_CF, A_CST, C_CST, N_CST, S_CST, NOX_R, RNOX, CCST, FCST, VCST, NOXFCF, TPCST
!
      DATA NOXFCF/0.12/
!
!     IF ( WPHS1 .EQ. 1 ) THEN
!        IPHASE = 1
!     ELSE
         IPHASE = 2
!     END IF
      IF (IPHASE .EQ. 1) THEN
         NOXYR = 9999
      ELSE
         NOXYR = NOX_PH2(IBTP)
      END IF
!
      IF (W_RYR .GE. NOXYR) THEN
         IF (WNOX_R .GT. NOX_STD(IBTP,IPHASE)) THEN
            IF (W_SYR .LT. NOXYR) THEN
!
!              RETIRE UNIT WITH INSUFFICIENT NOX CONTROLS
!
               WO_RYR(IRECL) = W_RYR
               WO_RMO(IRECL) = W_RMO
               WO_VIN(IRECL) = WVIN
               W_RYR = NOXYR - 1
               W_RMO = 12
               WVIN = 8
               TFURB = WRFURB
               CALL STRPLT(IRECL)
!
!              INCREMENT COUNTER
!
               WREC_NXT = WREC_NXT + 1
               NRECL = WREC_NXT
               WRFURB = TFURB
!
!              ENSURE THAT NUMBER OF BUILDS DOESN'T EXCEED ALLOCATION
!
               IF (NRECL .GT. WPLT_D_REC) WRITE(6, * ) &
                  ' NOX_STD ERROR: TOO MANY PLANT RECORDS', &
                  ' (WPLT_D_REC) ',WPLT_D_REC
!
               WNGRPS(IRG) = WNGRPS(IRG) + 1
               JGRP = WNGRPS(IRG)   ! Use New Group For Retros
               JRECL = W_NXT(IRECL)
               W_NXT(IRECL) = WREC_NXT
               W_NXT(WREC_NXT) = JRECL
               WGRP_NXT = WGRP_NXT + 1
               W_GRP = WGRP_NXT
               W_GRP2 = WNXT_SGRP(W_GRP) + 1
               WTYPE(W_GRP) = JGRP
               WNXT_SGRP(W_GRP) = W_GRP2
               IF ((WO_VIN(IRECL) .NE. 8) .and. &
                   (WO_VIN(IRECL) .NE. 6)) WVIN = 9
               ULIGRP(W_GRP) = W_IGRP
               W_SYR = NOXYR
               W_SMO = 1
               W_RYR = WO_RYR(IRECL)
               W_RMO = WO_RMO(IRECL)
            ELSE
               NRECL = IRECL
            END IF
!
!           Store Capacity with new NOX Controls
!
            NOX_CF = 0.5
            A_CST = 0.0
            C_CST = 0.0
            N_CST = 0.0
            S_CST = 0.0
            NOX_R = 0.0
            CHOICE = 0
            RNOX = WNOX_R * ( 1.0 - WCOMB_R)
            IF (RNOX .LE. NOX_STD(IBTP,IPHASE)+0.01) THEN
               IF (RNOX .LE. NOX_STD(IBTP,IPHASE)) THEN
                  NOX_R = WCOMB_R
               ELSE
                  NOX_R = 1.0 - (NOX_STD(IBTP,IPHASE) / WNOX_R)
               END IF
               C_CST = NOXFCF * WCOMB_O + WCOMB_F + WCOMB_V * NOX_CF * 8.76
               A_CST = C_CST
               CHOICE = 1
            END IF
!           RNOX = WNOX_R * ( 1.0 - WSNCR_R)
!           RNOX2 = WNOX_R * ( 1.0 - WSNCR_R) * ( 1.0 - WCOMB_R)
!           IF (RNOX .LE. NOX_STD(IBTP,IPHASE) .AND. RNOX2 .LE. 0.11) THEN
!              N_CST = NOXFCF * WSNCR_O + WSNCR_F + WSNCR_V * NOX_CF * 8.76
!              IF (CHOICE .EQ. 0 .OR. N_CST .LT. A_CST) THEN
!                 A_CST = N_CST
!                 NOX_R = WSNCR_R
!                 CHOICE = 2
!              END IF
!           END IF
            RNOX = WNOX_R * ( 1.0 - WSCR_R)
            IF (RNOX .LE. NOX_STD(IBTP,IPHASE)) THEN
               S_CST = NOXFCF * WSCR_O + WSCR_F + WSCR_V * NOX_CF * 8.76
               IF (CHOICE .EQ. 0 .OR. S_CST .LT. A_CST) THEN
                  A_CST = S_CST
                  NOX_R = WSCR_R
                  CHOICE = 3
               END IF
            END IF
            IF (CHOICE .EQ. 0) THEN
               NOX_R = WNOX_R
               WNOX_R = NOX_STD(IBTP,IPHASE)
               WNOX_B4 = NOX_STD(IBTP,IPHASE)
               IF (CURIYR .EQ. NOXYR) &
                  WRITE(18,2341) CURIYR,NOXYR,IRG,W_IGRP, &
                  W_GRP,W_GRP2,IRECL,NRECL,WEFDT,IBTP, &
                  W_COMB,W_POST,IPHASE,CHOICE,NOX_R, &
                  NOX_STD(IBTP,IPHASE), &
                  WNOX_R,WCOMB_R,WSNCR_R,WSCR_R, &
                  A_CST,C_CST,N_CST,S_CST, &
                  WCOMB_O,WSNCR_O,WSCR_O, &
                  WCOMB_F,WSNCR_F,WSCR_F, &
                  WCOMB_V,WSNCR_V,WSCR_V, &
                  NOX_CF,WC_SUM
            ELSE
               RNOX = WNOX_R
               WNOX_R = WNOX_R * ( 1.0 - NOX_R)
               WNOX_B4 = WNOX_R
               IF (CURIYR .EQ. NOXYR) &
                  WRITE(18,2341) CURIYR,NOXYR,IRG,W_IGRP, &
                  W_GRP,W_GRP2,IRECL,NRECL,WEFDT,IBTP, &
                  W_COMB,W_POST,IPHASE,CHOICE,NOX_R, &
                  NOX_STD(IBTP,IPHASE), &
                  WNOX_R,WCOMB_R,WSNCR_R,WSCR_R, &
                  A_CST,C_CST,N_CST,S_CST, &
                  WCOMB_O,WSNCR_O,WSCR_O, &
                  WCOMB_F,WSNCR_F,WSCR_F, &
                  WCOMB_V,WSNCR_V,WSCR_V, &
                  NOX_CF,WC_SUM
 2341          FORMAT(1X,"NOX_STD1",2(":",I2),":",I3, &
                  5(":",I5),2(":",I2),4(":",I1), &
                  6(":",F6.3),4(":",F12.3),3(":",F6.1), &
                  6(":",F6.2),":",F6.3,":",F6.0)
!
               IF (CHOICE .EQ. 1) THEN
                  W_COMB = 2
                  WCOMB_R = 0.0
                  CCST = WCOMB_O
                  FCST = WCOMB_F
                  VCST = WCOMB_V
               ELSE IF (CHOICE .EQ. 2) THEN
                  W_POST = 3
                  WSNCR_R = 0.0
                  CCST = WSNCR_O
                  FCST = WSNCR_F
                  VCST = WSNCR_V
               ELSE IF (CHOICE .EQ. 3) THEN
                  W_POST = 4
                  WSCR_R = 0.0
                  CCST = WSCR_O
                  FCST = WSCR_F
                  VCST = WSCR_V
               END IF
!
!              Find ECP Type for new Configuration
!
               IECP = WECPT
               DO I_CNFG = 1 , NUM_CNFG
                  IF (UPTTYP(IECP) .EQ. UPTTYP(UCL_ECP(I_CNFG))) THEN
                     ITST = 0
                     IF (WPART .EQ. "B" .AND. UCL_CNFG(2,I_CNFG) .EQ. 1 .OR. &
                         WPART .EQ. "E" .AND. UCL_CNFG(2,I_CNFG) .EQ. 2 .OR. &
                         WPART .EQ. "N" .AND. UCL_CNFG(2,I_CNFG) .EQ. 3 .OR. &
                         WPART .EQ. "0" .AND. UCL_CNFG(2,I_CNFG) .EQ. 4) ITST = ITST + 1
                     IF (WSCBT .EQ. "W" .AND. UCL_CNFG(3,I_CNFG) .EQ. 1 .OR. &
                         WSCBT .EQ. "D" .AND. UCL_CNFG(3,I_CNFG) .EQ. 2 .OR. &
                         WSCBT .EQ. "N" .AND. UCL_CNFG(3,I_CNFG) .EQ. 0) ITST = ITST + 1
                     IF (W_COMB .GT. 0 .AND. UCL_CNFG(4,I_CNFG) .EQ. 1 .OR. &
                         W_COMB .EQ. 0 .AND. UCL_CNFG(4,I_CNFG) .EQ. 0) ITST = ITST + 1
                     IF (W_POST .EQ. 1 .AND. UCL_CNFG(5,I_CNFG) .EQ. 1 .OR. &
                         W_POST .EQ. 3 .AND. UCL_CNFG(5,I_CNFG) .EQ. 1 .OR. &
                         W_POST .EQ. 5 .AND. UCL_CNFG(5,I_CNFG) .EQ. 1 .OR. &
                         W_POST .EQ. 0 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                         W_POST .EQ. 2 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                         W_POST .EQ. 4 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                         W_POST .EQ. 6 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0 .OR. &
                         W_POST .EQ. 7 .AND. UCL_CNFG(5,I_CNFG) .EQ. 0) ITST = ITST + 1
                     IF (W_POST .EQ. 2 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                         W_POST .EQ. 4 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                         W_POST .EQ. 6 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                         W_POST .EQ. 7 .AND. UCL_CNFG(6,I_CNFG) .EQ. 1 .OR. &
                         W_POST .EQ. 0 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0 .OR. &
                         W_POST .EQ. 1 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0 .OR. &
                         W_POST .EQ. 3 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0 .OR. &
                         W_POST .EQ. 5 .AND. UCL_CNFG(6,I_CNFG) .EQ. 0) ITST = ITST + 1
                     IF (W_ACI .EQ. "N" .AND. UCL_CNFG(7,I_CNFG) .EQ. 0 .OR. &
                         W_ACI .EQ. "A" .AND. UCL_CNFG(7,I_CNFG) .EQ. 0 .OR. &
                         W_ACI .EQ. "S" .AND. UCL_CNFG(7,I_CNFG) .EQ. 1 .OR. &
                         W_ACI .EQ. "F" .AND. UCL_CNFG(7,I_CNFG) .EQ. 0 .OR. &
                         W_ACI .EQ. "B" .AND. UCL_CNFG(7,I_CNFG) .EQ. 1) ITST = ITST + 1
                     IF (W_ACI .EQ. "N" .AND. UCL_CNFG(8,I_CNFG) .EQ. 0 .OR. &
                         W_ACI .EQ. "A" .AND. UCL_CNFG(8,I_CNFG) .EQ. 0 .OR. &
                         W_ACI .EQ. "S" .AND. UCL_CNFG(8,I_CNFG) .EQ. 0 .OR. &
                         W_ACI .EQ. "F" .AND. UCL_CNFG(8,I_CNFG) .EQ. 1 .OR. &
                         W_ACI .EQ. "B" .AND. UCL_CNFG(8,I_CNFG) .EQ. 1) ITST = ITST + 1
                     IF (ITST .EQ. 7) THEN
                        WECPT = UCL_ECP(I_CNFG)
                     END IF
!
                  END IF
               END DO
!
               WRITE(18,6313) CURIYR+UHBSYR,NOXYR,W_IGRP,W_GRP,W_SYR,W_RYR,IECP,WECPT,WPART,WSCBT,W_COMB,W_POST,W_ACI,WNOX_R,RNOX,WC_SUM
 6313          FORMAT(1X,"NOX_STD2",8(":",I5),2(":",A1),2(":",I1),":",A1,3(":",F9.3))
            END IF
            IF (CCST .GT. DBLE(0.0)) THEN
               W_FOM = W_FOM + FCST
               W_VOM = W_VOM + VCST
            END IF
            CALL STRPLT(NRECL)
            IF (CCST .GT. DBLE(0.0)) THEN
               TPCST = WPCST
               TEFPT = WEFPT
               WPCST = CCST
               WEFPT = 8
               NERC = WNOWN
               CALL UEXPBLD(NERC)
               WPCST = TPCST
               WEFPT = TEFPT
            END IF
            CALL GETPLT(IRECL)
         END IF
      END IF
!
      RETURN
      END


      SUBROUTINE ECPLVCST(LVSWITCH,JYR,JRG,CFC,LVCSTMIN)
!
      IMPLICIT NONE

!     THIS SUBROUTINE COMPUTES MINIMUM LEVELIZED COST OF REPLACEMENT POWER

      INCLUDE'parametr'
      INCLUDE'ncntrl'
      INCLUDE'emmparm'
      INCLUDE'control'
      INCLUDE'ecpcntl'
      INCLUDE'bildin'
      INCLUDE'bildout'
      INCLUDE'uefdout'

      INTEGER*4 LVSWITCH,JRG,IRG,JYR,PLT,OWN,OLYR,PMIN,OMIN
      REAL*4 LEVCF(ECP_D_DSP),CFC,LVMINREG(MNUMNR),LVCSTPLT,LVCSTMIN

!     FIRST TIME THROUGH, DETERMINE MINUMUM LEVELIZED COSTS BY REGION

      IF (UF_DBG .GT. 0) WRITE(UF_DBG,*) 'ECPLVCST',CURIYR+1989,LVSWITCH,JYR,CFC
      IF (LVSWITCH .EQ. 1) THEN

!        LOOP OVER REGIONS

         DO IRG = 1 , UNRGNS

!           LOOP OVER DISPATCHABLE PLANT TYPES GET LEVELIZED COST DATA FROM ECP OUTPUT DAF

            CALL GETBOUT(JYR,IRG)
            LVMINREG(IRG) = 999.99

!           DETERMINE MINIMUM LEVELIZED COST

            DO PLT = 1 , ECP_D_DSP
               IF (UPLNTCD(UCPDSPI(PLT)) .NE. 'AN') THEN

!                 CHECK IF PLANT/OWNER TYPE CAN BE BUILT

                  IF (UPVTYP(PLT) .EQ. 1) THEN
                   IF (UPBLDREG(UCPDSPI(PLT),IRG) .GT. 0.0 .AND. UPBLDREG(UCPDSPI(PLT),MNUMNR) .GT. 0.0) THEN
                     OLYR = UHBSYR + (CURIYR - 1) + UPPLYR(PLT)
                     IF (UPAVLYR(PLT) .LE. OLYR) THEN

!                       ASSIGN CAPACITY FACTORS FOR LEVELIZATION OF DISPATCHABLES: USE 30% FOR TURBINES, 70% FOR ALL OTHER

                        IF ( (UPLNTCD(UCPDSPI(PLT)) .EQ. 'CT') .OR. (UPLNTCD(UCPDSPI(PLT)) .EQ. 'ET') .OR. (UPLNTCD(UCPDSPI(PLT)) .EQ. 'AT') ) THEN
                           LEVCF(PLT) = 0.30
                        ELSE
                           LEVCF(PLT) = 0.70
                        END IF
                        CFC = LEVCF(PLT)

!                       DETERMINE LEVELIZED COST FOR PLANT/OWNER TYPE AND STORE MINIMUM

                        OWN = UPBLDTYP(IRG)
                        LVCSTPLT = (EPLVCAP(PLT,OWN) + EPLVFOM(PLT)) / (CFC * 8.760) + (EPLVFLC(PLT) + EPLVVOM(PLT) + EPLVEXT(PLT))
                        IF (LVCSTPLT .LT. LVMINREG(IRG)) THEN
                           LVMINREG(IRG) = LVCSTPLT
                           PMIN = PLT
                           OMIN = OWN
                        END IF
                     END IF
                  END IF
               END IF
               END IF
            END DO

!           IF(LVMINREG(IRG).LT.20.0) &
!              PRINT *,'CNULOW',CURIYR+1989,IRG,PMIN,OMIN,UPMCF(UICNU), &
!              EPLVCAP(PMIN,OMIN)*SCALPR,EPLVFOM(PMIN)*SCALPR, &
!              EPLVFLC(PMIN)*SCALPR,EPLVVOM(PMIN)*SCALPR, &
!              EPLVEXT(PMIN)*SCALPR,LVCSTMIN*SCALPR,SCALPR
!           PRINT *,'LVCST1',CURIYR+1989,IRG,LVMINREG(IRG)

         END DO
      ELSE

!        RETRIEVE MINIMUM LEVELIZE COSTS FOR SPECIFIED REGION

         LVCSTMIN = LVMINREG(JRG)

!        PRINT *,'LVCST2',CURIYR+1989,JRG,LVMINREG(JRG),LVSWITCH

      END IF

      RETURN
      END
!
!
      SUBROUTINE ENUCFAC(IYR,W_CFA,NUC_CFC)
!
      IMPLICIT NONE

!     THIS SUBROUTINE SETS NUCLEAR CAPACITY FACTORS
!     DIFFERENT PATHS ARE USED IF THE INITIAL OPERATION DATE IS PRE-1983 OR POST-1983
!     THERE ARE THREE INTERVALS IN A NUCLEAR PLANT LIFESPAN.  THE CAPACITY FACTOR:
!         INCREASES OVER 1ST INTERVAL, IS LEVEL OVER 2ND INTERMAL, DECREASES OVER 3RD INTERVAL

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'plntin'
!
      INTEGER IYR
      INTEGER AGENUC,END_INC,END_LEV,YR_INC,YR_DEC
      REAL RATE_INC,RATE_DEC,NUC_CFC
      REAL*8 W_CFA
!
      AGENUC = IYR - WRFURB
!
      IF(WRFURB .LE. 1983)THEN
         END_INC = UNCFEI1
         RATE_INC = UNCFRI1
         END_LEV = UNCFEL1
         RATE_DEC = UNCFRD1
      ELSE
         END_INC = UNCFEI2
         RATE_INC = UNCFRI2
         END_LEV = UNCFEL2
         RATE_DEC = UNCFRD2
      END IF
!
!     1ST INTERVAL -- INCREASING CAPACITY FACTORS
!
      IF(AGENUC .LE. END_INC)THEN
         YR_INC = IYR - UNCFBYR
         YR_DEC = 0
      ELSE
         YR_INC = WRFURB + END_INC - UNCFBYR
         YR_INC = MAX(YR_INC,0)
        IF (AGENUC .LE. END_LEV)THEN
         YR_DEC = 0
        ELSE
         YR_DEC = IYR - (WRFURB + END_LEV)
        END IF
      END IF
      NUC_CFC = W_CFA + FLOAT(YR_INC) * RATE_INC
!
!     DON'T ALLOW CAPACITY FACTOR TO INCREASE ABOVE MAXIMUM
!
      IF (W_CFA .LE. UNCFMAX)THEN
         NUC_CFC = MIN(NUC_CFC,UNCFMAX)
!
!        IF HISTORICAL CAPACITY FACTOR EXCEEDS MAXIMUM ALLOWABLE, PERMIT NO FURTHER INCREASES
      ELSE
         NUC_CFC = W_CFA
      END IF
!
!     NOW ACCOUNT FOR DECREASE, IF APPROPRIATE
!
      NUC_CFC = NUC_CFC - FLOAT(YR_DEC) * RATE_DEC
      if (NUC_CFC .lt. 0.0) NUC_CFC = 0.0
!
      RETURN
      END
!
!
      SUBROUTINE GETFUEL(IYR)
!
      IMPLICIT NONE

!     THIS SUBROUTINE SETS UP FUEL PRICES ACCOUNTING FOR ENVIRONMENTAL ADDERS

      include'parametr'
      include'ncntrl'
      include'apq'
      include'angtdm'
      include'emmparm'
      include'cdsparms'
      include'control'
      include'dispin'
      include'dispout'
      include'dispuse'
      include'fuelin'
      include'ecpcntl'
      include'emission'
      include'emeblk'
      include'emablk'
      include'emoblk'
      include'epmbank'
      include'dispinyr'
      include'uso2grp'
      include 'uefpout'     ! UEFP output variables
      include 'uefdout'     ! EFD output variables
      include 'udatout'     ! UDAT output variables
      include 'uecpout'     ! UECP output variables
      include 'uldsmout'    ! LDSM output variables
      include 'uettout'     ! UETT output variables
      include'coalemm'
      include'acoalprc'
      include'wrenew'
      include'macout'
      include'dsmdimen'
      include'dsmtoefd'
!
      REAL*4 WDPRCL(NDREG),NGFAC,RSFAC
!
!     STEO FUEL PRICES FOR CALIBRATION FOR (UPSTYR - 1) TO (UPSTYR - 1) + 2 (IN NOMINAL $)
!
      INTEGER IYR,IEFD,KYR,ICENSUS,IGR,INR,IFL,ITRSW,IRG,IEMF,IS,ISC
!
      NGFAC = 1.0
      RSFAC = 1.0
!
!     STORE FUEL NAME IN OUTPUT DATA BLOCK
!
      ENFLTP = UNFUELS
      DO IFL = 1 , ENFLTP
         ENMFL(IFL) = UNMFL(IFL)
      END DO
!
!     SET NEMS YEAR
!
      KYR = IYR - UHBSYR
!
!     CHECK SWITCH TO SEE IF OIL PRODUCT PRICES (BY CENSUS REGION) FROM
!     OTHER MODULES ARE USED INSTEAD OF PRICES FROM DIRECT ACCESS FILE
!
      IF(USW_CENS .LE. 0)THEN
!
!        GET NEMS DISTILLATE PRICES IF ANY
!
         IEFD = UIDS
         IEMF = 5
         DO ICENSUS = 1 , MNUMCR
            IF (PDSEL(ICENSUS,KYR) .GT. 0.000001) &
                UPFUEL(IEFD,ICENSUS) = PDSEL(ICENSUS,KYR) * RSFAC
            IF (UFHCNT(IEFD,ICENSUS) .LE. 0.0)  &
               UFHCNT(IEFD,ICENSUS) = 5.825
!
! USE CARBON FACTORS FROM EPM (CONVERT FROM MM MT/TRILL TO LB/MMBTU)
!
            UFRCAR(IEFD,ICENSUS) = EDSEL(CURIYR) * 2204.0  * 0.001
            UFRCO2(IEFD,ICENSUS) = UFRCAR(IEFD,ICENSUS) * 44.0 / 12.0
            UFRCO1(IEFD,ICENSUS) = 0.0
!
! DETERMINE CARBON COMPONENT OF FUEL PRICE
!
            IF (CURIYR .GT. FIRSYR) UPNCAR(IEFD,ICENSUS) = JDSEL(CURIYR)
            UFRASH(IEFD,ICENSUS) = 1.0 - (SO2_EMF(5) / SO2_MAX(5))
!
! USE MERCURY FACTORS FROM EPM
!
            UFRHG(IEFD,ICENSUS) = HGDSEL
         END DO
!
!        GET NEMS RESID LOW SULFUR PRICES IF ANY
!
         IEFD = UIRL
         IEMF = 4
         DO ICENSUS = 1 , MNUMCR
            IF (PRLEL(ICENSUS,KYR) .GT. 0.000001) &
                UPFUEL(IEFD,ICENSUS) = PRLEL(ICENSUS,KYR) * RSFAC
            IF (UFHCNT(IEFD,ICENSUS) .LE. 0.0)  &
               UFHCNT(IEFD,ICENSUS) = 6.285
!
!           USE CARBON FACTORS FROM EPM (CONVERT FROM MM MT/TRILL TO LB/MMBTU)
!
            UFRCAR(IEFD,ICENSUS) = ERLEL(CURIYR) * 2204.0 * 0.001
            UFRCO2(IEFD,ICENSUS) = UFRCAR(IEFD,ICENSUS) * 44.0 / 12.0
            UFRCO1(IEFD,ICENSUS) = 0.0
!
! DETERMINE CARBON COMPONENT OF FUEL PRICE
!
            IF (CURIYR .GT. FIRSYR) UPNCAR(IEFD,ICENSUS) = JRLEL(CURIYR)
            UFRASH(IEFD,ICENSUS) = 1.0 - (SO2_EMF(4) / SO2_MAX(4))
!
! USE MERCURY FACTORS FROM EPM
!
            UFRHG(IEFD,ICENSUS) = HGRLEL
         END DO
!
!        GET NEMS RESID HIGH SULFUR PRICES IF ANY
!
         IEFD = UIRH
         IEMF = 4
         DO ICENSUS = 1 , MNUMCR
            IF (PRHEL(ICENSUS,KYR) .GT. 0.000001) &
                UPFUEL(IEFD,ICENSUS) = PRHEL(ICENSUS,KYR) * RSFAC
            IF (UFHCNT(IEFD,ICENSUS) .LE. 0.0)  &
               UFHCNT(IEFD,ICENSUS) = 6.285
!
!           USE CARBON FACTORS FROM EPM (CONVERT FROM MM MT/TRILL TO LB/MMBTU)
!
            UFRCAR(IEFD,ICENSUS) = ERHEL(CURIYR) * 2204.0 * 0.001
            UFRCO2(IEFD,ICENSUS) = UFRCAR(IEFD,ICENSUS) * 44.0 / 12.0
            UFRCO1(IEFD,ICENSUS) = 0.0
!
! DETERMINE CARBON COMPONENT OF FUEL PRICE
!
            IF (CURIYR .GT. FIRSYR) UPNCAR(IEFD,ICENSUS) = JRHEL(CURIYR)
            UFRASH(IEFD,ICENSUS) = 1.0 - (SO2_EMF(4) / SO2_MAX(4))
!
! USE MERCURY FACTORS FROM EPM
!
            UFRHG(IEFD,ICENSUS) = HGRHEL
         END DO
!
!  use weighted average resid high/low for both, calculated by resid model
!
         DO ICENSUS = 1, MNUMCR
           UPFUEL(UIRL,ICENSUS) = PRSEL(ICENSUS,KYR) * RSFAC
           UPFUEL(UIRH,ICENSUS) = PRSEL(ICENSUS,KYR) * RSFAC
!           qrstot = QRLEL(ICENSUS,KYR) + QRHEL(ICENSUS,KYR)
!          if (qrstot .gt. 0.00001) then
!           prsavg = (UPFUEL(UIRL,ICENSUS) * QRLEL(ICENSUS,KYR) + &
!                     UPFUEL(UIRH,ICENSUS) * QRHEL(ICENSUS,KYR) ) / qrstot
!          else
!           prsavg = (UPFUEL(UIRL,ICENSUS) + UPFUEL(UIRH,ICENSUS)) / 2.0
!          endif
!           UPFUEL(UIRL,ICENSUS) = prsavg
!           UPFUEL(UIRH,ICENSUS) = prsavg
         ENDDO

!
!        GET NEMS DISTRIBUTED GENERATION OIL PRICES.
!        USE AVERAGE OF COMMERCIAL AND INDUSTRIAL
!
         IEFD = UIDD
         IEMF = 5
         DO ICENSUS = 1 , MNUMCR
!           IF (PDSEL(ICENSUS,KYR) .GT. 0.000001) &
               UPFUEL(IEFD,ICENSUS) = PDSEL(ICENSUS,KYR)
!        ADD DISTRIBUTED GENERATION FUEL PRICE PREMIUM
               UPFUEL(IEFD,ICENSUS) = UPFUEL(IEFD,ICENSUS) +  &
                                      UPDGFPR
            IF (UFHCNT(IEFD,ICENSUS) .LE. 0.0)  &
               UFHCNT(IEFD,ICENSUS) = 5.825
!
! USE CARBON FACTORS FROM EPM (CONVERT FROM MM MT/TRILL TO LB/MMBTU)
!
            UFRCAR(IEFD,ICENSUS) = EDSEL(CURIYR) * 2204.0  * 0.001
            UFRCO2(IEFD,ICENSUS) = UFRCAR(IEFD,ICENSUS) * 44.0 / 12.0
            UFRCO1(IEFD,ICENSUS) = 0.0
!
! DETERMINE CARBON COMPONENT OF FUEL PRICE
!
            IF (CURIYR .GT. FIRSYR) UPNCAR(IEFD,ICENSUS) = JDSEL(CURIYR)
            UFRASH(IEFD,ICENSUS) = 1.0 - (SO2_EMF(5) / SO2_MAX(5))
!
! USE MERCURY FACTORS FROM EPM
!
            UFRHG(IEFD,ICENSUS) = HGDSEL
         END DO
      END IF
!
!     CHECK SWITCH TO SEE IF NATURAL GAS PRICES (BY GAS REGION) FROM
!     OTHER MODULES ARE USED INSTEAD OF PRICES FROM DIRECT ACCESS FILE
!
      IF(USW_GASR .LE. 0)THEN

!
!        GET NEMS NATURAL GAS FIRM PRICES IF ANY
!        use new average NGMM prices for AEO2018 - no more firm/inter/comp
!
         IEFD = UIGF
         IEMF = 6
         DO IGR = 1 , NNGEM
            IF (PNGELGR(IGR,KYR) .GT. 0.000001) UPFUEL(IEFD,IGR) = PNGELGR(IGR,KYR) * NGFAC
            IF (UFHCNT(IEFD,IGR) .LE. 0.0) UFHCNT(IEFD,IGR) = 1.032
!
!           USE CARBON FACTORS FROM EPM (CONVERT FROM MM MT/TRILL TO LB/MMBTU)
!
            UFRCAR(IEFD,IGR) = EGFELGR(CURIYR) * 2204.0 * 0.001
            UFRCO2(IEFD,IGR) = UFRCAR(IEFD,IGR) * 44.0 / 12.0
            UFRCO1(IEFD,IGR) = 0.0
!
! DETERMINE CARBON COMPONENT OF FUEL PRICE
!
           IF(CURIYR .GT. FIRSYR)UPNCAR(IEFD,IGR)=JGFELGR(CURIYR)
            UFRASH(IEFD,IGR) = 1.0 - (SO2_EMF(6) / SO2_MAX(6))
!
! USE MERCURY FACTORS FROM EPM
!
            UFRHG(IEFD,IGR) = HGNGEL
         END DO
!
!        GET NEMS NATURAL GAS INTERRUPTIBLE PRICES IF ANY
!        use new average NGMM prices for AEO2018 - no more firm/inter/comp
!
         IEFD = UIGI
         IEMF = 6
         DO IGR = 1 , NNGEM
            IF (PNGELGR(IGR,KYR) .GT. 0.000001) &
                UPFUEL(IEFD,IGR) = PNGELGR(IGR,KYR) * NGFAC
            IF (UFHCNT(IEFD,IGR) .LE. 0.0)  &
               UFHCNT(IEFD,IGR) = 1.032
!
!           USE CARBON FACTORS FROM EPM (CONVERT FROM MM MT/TRILL TO LB/MMBTU)
!
            UFRCAR(IEFD,IGR) = EGIELGR(CURIYR) * 2204.0 * 0.001
            UFRCO2(IEFD,IGR) = UFRCAR(IEFD,IGR) * 44.0 / 12.0
            UFRCO1(IEFD,IGR) = 0.0
!
! DETERMINE CARBON COMPONENT OF FUEL PRICE
!
            IF (CURIYR .GT. FIRSYR) UPNCAR(IEFD,IGR) = JGIELGR(CURIYR)
            UFRASH(IEFD,IGR) = 1.0 - (SO2_EMF(6) / SO2_MAX(6))
!
! USE MERCURY FACTORS FROM EPM
!
            UFRHG(IEFD,IGR) = HGNGEL

         END DO
!
!        GET NEMS NATURAL GAS COMPETITIVE PRICES IF ANY
!        use new average NGMM prices for AEO2018 - no more firm/inter/comp
!

! NOTE UPFUEL FOR UIGC WHICH IS THE HYDROGEN FUEL IN EPHRTS IS FILLED IN PROGRAMATICALLY IN EPHRTS.F90
         IEFD = UIGC
         IEMF = 6
         DO ICENSUS = 1, MNUMCR
         
            ! NOTE THIS UPFUEL PRICE IS UPDATED IN EPHRTS.F90 PROGRAMTICALLY, BELOW IS JUST AN INITIALIZED VERSION.
 !           UPFUEL(IEFD,ICENSUS) = 3.25 ! $1/kg which is  0.37 $2023/kg *( 1 kg/ 2.20462 lbs) * (1/ 51628 btu/lb) * (10^9 billion) = 3.25 $87/MBTU
            
            IF (UFHCNT(IEFD,ICENSUS) .LE. 0.0)  &
               UFHCNT(IEFD,ICENSUS) = 0.0   ! reporting variable for natural gas, set to zero after investigating.
!
!           USE CARBON FACTORS FROM EPM (CONVERT FROM MM MT/TRILL TO LB/MMBTU)
!
            UFRCAR(IEFD,ICENSUS) = 0.0 !EGCELGR(CURIYR) * 2204.0 * 0.001
            UFRCO2(IEFD,ICENSUS) = 0.0 !UFRCAR(IEFD,IGR) * 44.0 / 12.0
            UFRCO1(IEFD,ICENSUS) = 0.0
!
! DETERMINE CARBON COMPONENT OF FUEL PRICE
!
            IF (CURIYR .GT. FIRSYR) UPNCAR(IEFD,ICENSUS) = 0.0 !JGCELGR(CURIYR)
            UFRASH(IEFD,ICENSUS) = 0.0 ! 1.0 - (SO2_EMF(6) / SO2_MAX(6))
!
! USE MERCURY FACTORS FROM EPM
!
            UFRHG(IEFD,ICENSUS) = 0.0 !HGNGEL

         END DO

!
!        GET NEMS DISTRIBUTED GENERATION GAS PRICES
!        ADD FUEL PRICE PREMIUM
!
         IEFD = UIDG
         IEMF = 6
         DO IGR = 1 , NNGEM
            IF (PNGELGR(IGR,KYR) .GT. 0.000001) &
                UPFUEL(IEFD,IGR) = PNGELGR(IGR,KYR)
                UPFUEL(IEFD,IGR) =  UPFUEL(IEFD,IGR) + UPDGFPR
            IF (UFHCNT(IEFD,IGR) .LE. 0.0)  &
               UFHCNT(IEFD,IGR) = 1.032
!
!           USE CARBON FACTORS FROM EPM (CONVERT FROM MM MT/TRILL TO LB/MMBTU)
!
            UFRCAR(IEFD,IGR) = EGIELGR(CURIYR) * 2204.0 * 0.001
            UFRCO2(IEFD,IGR) = UFRCAR(IEFD,IGR) * 44.0 / 12.0
            UFRCO1(IEFD,IGR) = 0.0
!
! DETERMINE CARBON COMPONENT OF FUEL PRICE
!
            IF (CURIYR .GT. FIRSYR) UPNCAR(IEFD,IGR) = JGIELGR(CURIYR)
            UFRASH(IEFD,IGR) = 1.0 - (SO2_EMF(6) / SO2_MAX(6))
!
! USE MERCURY FACTORS FROM EPM
!
            UFRHG(IEFD,IGR) = HGNGEL
         END DO
      END IF
!
!     CHECK SWITCH TO SEE IF COAL AND NUC PRICES (BY CENSUS REGION) FROM
!     OTHER MODULES ARE USED INSTEAD OF PRICES FROM DIRECT ACCESS FILE
!
!     IF(USW_NERC .LE. 0)THEN
!
!     GET NEMS NUCLEAR FUEL PRICES IF ANY
!     USE NUCLEAR FUEL PRICES PROVIDED BY LARRY SANDERS
!
      IEFD = UIUF
      DO INR = 1 , EFD_D_MFRG     !MNUMNR
         UPFUEL(IEFD,INR) = UPURELN(INR,KYR)
         IF (USW_XP.GT.0  .AND. INR.EQ.14) THEN  !CANADA NUC FLRG=14
              UPFUEL(IEFD,INR) = 7.0            !MAKE NUC FLCST=$7.00
         ENDIF
!
! USE CARBON FACTORS FROM EPM (CONVERT FROM MM MT/Q TO MM ST/TRILL)
!
            UFRCAR(IEFD,INR) = 0.0
            UFRCO2(IEFD,INR) = 0.0
            UFRCO1(IEFD,INR) = 0.0
!
! DETERMINE CARBON COMPONENT OF FUEL PRICE
!
         IF (CURIYR .GT. FIRSYR) UPNCAR(IEFD,INR) = 0.0
      END DO
!
      IF (USW_CLRG .LE. 0) THEN
         ITRSW = MOD(CURITR,2)
!
         DO IEFD = 1 , NCLUT1
            ISC = ECP_SCRUB(IEFD,CURIYR)
         DO IRG = 1 , NDRG2
               UPFUEL(IEFD,IRG) = DBLE(PCLELCDR(ISC,IRG,KYR))

!              UPFUEL(IEFD,IRG) = DBLE(PCLCLNR(IRG,KYR,IEFD))

!              IF (FCRL .EQ. 1) THEN
!                 WRITE(6,3711) CURIRUN, CURIYR+1989, CURITR, IRG, IEFD, ISC, KYR, UPFUEL(IEFD,IRG), PCLCLNR(IRG,KYR,IEFD), PCLELCDR(ISC,IRG,KYR)
!3711             FORMAT(1X,"PCLELCDR",7(":",I4),3(":",E21.6))
!              END IF

               UFRSO2(IEFD,IRG) = SCLCLNR(IRG,KYR,IEFD)
               UFHCNT(IEFD,IRG) = BCLCLNR(IRG,KYR,IEFD)
               UFRHG(IEFD,IRG) = HCLCLNR(IRG,KYR,IEFD) * (1.0 - GCLCLNR(IRG,KYR,IEFD))
               IF (UFHCNT(IEFD,IRG) .LE. 0.0) UFHCNT(IEFD,IRG) = 18.00
!
! USE CARBON FACTORS FROM EPM (CONVERT FROM MM MT/Q TO MM ST/TRILL)
!
               IF (CCLCLNR(IRG,CURIYR,IEFD) .GT. 0.0)THEN
                  UFRCAR(IEFD,IRG) = CCLCLNR(IRG,CURIYR,IEFD) * 2204.0 * 0.001
               ELSE
                  UFRCAR(IEFD,IRG) = ECLEL(CURIYR) * 2204.0 * 0.001
               END IF
            UFRCO2(IEFD,IRG) = UFRCAR(IEFD,IRG) * 44.0 / 12.0
            UFRCO1(IEFD,IRG) = 0.0
!
! DETERMINE CARBON COMPONENT OF FUEL PRICE
!
               IF (CURIYR .GT. FIRSYR) UPNCAR(IEFD,IRG) = JCLCLNR(CURIYR,IEFD)
!
!              ASH RETENTION NOW INCLUDED IN CMM SO2 FACTORS
!
            UFRASH(IEFD,IRG) = 0.0
            END DO
         END DO
      END IF
!
!     GET NEMS BIOMASS FUEL PRICES
!
      IEFD = UIWD
      DO IRG = 1 , NDREG
!
!        SMOOTH BIOMASS PRICE BY AVERAGING CURRENT AND PREVIOUS PRICES
!
         UPFUEL(IEFD,IRG) = PBMPWCL(0,IRG,KYR)
         IF (CURITR .GT. 1) UPFUEL(IEFD,IRG) =  0.5 * (UPFUEL(IEFD,IRG) + WDPRCL(IRG))
         IF (ISNAN(UPFUEL(IEFD,IRG)).OR. ABS(UPFUEL(IEFD,IRG)) .GT. HUGE(UPFUEL(IEFD,IRG))) THEN   ! check for NaNQ this way
            UPFUEL(IEFD,IRG) = 9.999
         END IF
         WDPRCL(IRG) = UPFUEL(IEFD,IRG)
!        USE EPA CARBON FACTOR, IF APPROPRIATE
         IF (CO2_STDSW .GT. 0 .AND. CO2_STDRS(1,CURIYR) .GT. 0.1)THEN
         UFRCO2(IEFD,IRG) = CO2_EMSWD
         UFRCAR(IEFD,IRG) = UFRCO2(IEFD,IRG) * 12.0 / 44.0
         ELSE
            UFRCO2(IEFD,IRG) = 0.0
            UFRCAR(IEFD,IRG) = 0.0
         END IF
         UFRCO1(IEFD,IRG) = 0.0
      END DO
!
      DO INR = 1 , EFD_D_MFRG    ! MNUMNR
!
!        SET GEOTHERMAL PRICE TO SMALL POSITIVE NUMBER
!
         UPFUEL(UIGT,INR) = 0.001
!
!        SET CARBON FACTORS FOR RENEWABLES
!
!        OTHER GASEOUS FUELS
               UFRCAR(UIOG,INR) = 0.0
               UFRCO2(UIOG,INR) = 0.0
               UFRCO1(UIOG,INR) = 0.0
               UPNCAR(UIOG,INR) = 0.0
!        WOOD
!              UFRCAR(UIWD,INR) = 0.0
!              UFRCO2(UIWD,INR) = 0.0
!              UFRCO1(UIWD,INR) = 0.0
!              UPNCAR(UIWD,INR) = 0.0
!        SOLID WASTE
               UFRCAR(UISW,INR) = 0.0
               UFRCO2(UISW,INR) = 0.0
               UFRCO1(UISW,INR) = 0.0
               UPNCAR(UISW,INR) = 0.0
!        HYDRO
               UFRCAR(UIWA,INR) = 0.0
               UFRCO2(UIWA,INR) = 0.0
               UFRCO1(UIWA,INR) = 0.0
               UPNCAR(UIWA,INR) = 0.0
!        PUMP STORAGE
               UFRCAR(UIPS,INR) = 0.0
               UFRCO2(UIPS,INR) = 0.0
               UFRCO1(UIPS,INR) = 0.0
               UPNCAR(UIPS,INR) = 0.0
!        WIND
               UFRCAR(UIWN,INR) = 0.0
               UFRCO2(UIWN,INR) = 0.0
               UFRCO1(UIWN,INR) = 0.0
               UPNCAR(UIWN,INR) = 0.0
!        WIND - LOW SPEED
               UFRCAR(UIWL,INR) = 0.0
               UFRCO2(UIWL,INR) = 0.0
               UFRCO1(UIWL,INR) = 0.0
               UPNCAR(UIWL,INR) = 0.0
!        OFFSHORE WIND
               UFRCAR(UIWF,INR) = 0.0
               UFRCO2(UIWF,INR) = 0.0
               UFRCO1(UIWF,INR) = 0.0
               UPNCAR(UIWF,INR) = 0.0
!        GEOTHERMAL
               UFRCAR(UIGT,INR) = 0.0
               UFRCO2(UIGT,INR) = 0.0
               UFRCO1(UIGT,INR) = 0.0
               UPNCAR(UIGT,INR) = 0.0
!        SOLAR THERMAL
               UFRCAR(UISO,INR) = 0.0
               UFRCO2(UISO,INR) = 0.0
               UFRCO1(UISO,INR) = 0.0
               UPNCAR(UISO,INR) = 0.0
!        SOLAR PV
               UFRCAR(UIPV,INR) = 0.0
               UFRCO2(UIPV,INR) = 0.0
               UFRCO1(UIPV,INR) = 0.0
               UPNCAR(UIPV,INR) = 0.0
!        SOLAR PV - Fixed Axis
               UFRCAR(UIPT,INR) = 0.0
               UFRCO2(UIPT,INR) = 0.0
               UFRCO1(UIPT,INR) = 0.0
               UPNCAR(UIPT,INR) = 0.0
      END DO
!     IF CARBON FEE IS USED FOR ECP ONLY (I.E., PRIOR TO ACTUAL START YEAR), THEN ZERO OUT
!     FOR DISPATCHING
      DO IEFD = 1 , EFD_D_NFL
        DO IRG = 1 , EFD_D_MFRG
          UPXCAR(IEFD,IRG) = UPNCAR(IEFD,IRG)
          IF((BANK_FLAG /= 0) .AND. CURCALYR .LT. BANK_STARTYR)UPNCAR(IEFD,IRG) = 0.0
        END DO
      END DO
!
      RETURN
      END
!
!
      SUBROUTINE EMM$XPQ
!
      IMPLICIT NONE

!     THIS SUBROUTINE SETS UP THE PRICE EXPECTATION ARRAYS IF THEY ARE CURRENTLY EMPTY

      include'parametr'
      include'ncntrl'
      include 'mxpblk'
      include'emmparm'
      include'control'
      include 'ecpcntl'
      include'dispin'
      include'dispout'
      include'dispuse'
      include'fuelin'
!
      REAL*8 INDEX,ESC
      INTEGER KYR,ICENSUS
      INTEGER USW_EXP
      REAL UCL_GRW,UNG_GRW,UOL_GRW,UUR_GRW
!
!     SET UP PRICE EXPECTATION ARRAYS IF THEY ARE CURRENTLY EMPTY
!
      USW_EXP = 1
      UCL_GRW = 0.01
      UNG_GRW = 0.015
      UOL_GRW = 0.02
      UUR_GRW = 0.01
      DO 400 ICENSUS = 1 , MNUMCR
!
!        REAL XPCLEL(MNUMCR,MNXYR)
!
         IF ((XPCLEL(ICENSUS,1) .LT. 0.1 ).OR.(USW_EXP .EQ. 1)) THEN
            INDEX = DBLE(1.0)
            ESC = DBLE(UCL_GRW)
            XPCLEL(ICENSUS,1) = INDEX
            DO 300 KYR = 2 , UNYEAR + UNXPH
               INDEX = INDEX * (DBLE(1.0) + ESC)
               XPCLEL(ICENSUS,KYR) = INDEX
  300       CONTINUE
         END IF
!
!        REAL XPGFEL(MNUMCR,MNXYR)
!
!        IF ((XPGFEL(ICENSUS,1) .LT. 0.1 ).OR.(USW_EXP .EQ. 1)) THEN
!        INDEX = DBLE(1.0)
!        ESC = DBLE(UNG_GRW)
!        XPGFEL(ICENSUS,1) = INDEX
!        DO 310 KYR = 2 , UNYEAR + UNXPH
!        INDEX = INDEX * (DBLE(1.0) + ESC)
!        XPGFEL(ICENSUS,KYR) = INDEX
!        310       CONTINUE
!        END IF
!
!        REAL XPGIEL(MNUMCR,MNXYR)
!
!        IF ((XPGIEL(ICENSUS,1) .LT. 0.1 ).OR.(USW_EXP .EQ. 1)) THEN
!        INDEX = DBLE(1.0)
!        ESC = DBLE(UNG_GRW)
!        XPGIEL(ICENSUS,1) = INDEX
!        DO 320 KYR = 2 , UNYEAR + UNXPH
!        INDEX = INDEX * (DBLE(1.0) + ESC)
!        XPGIEL(ICENSUS,KYR) = INDEX
!        320       CONTINUE
!        END IF
!
!        REAL XPNGEL(MNUMCR,MNXYR)
!
!        IF ((XPNGEL(ICENSUS,1) .LT. 0.1 ).OR.(USW_EXP .EQ. 1)) THEN
!        INDEX = DBLE(1.0)
!        ESC = DBLE(UNG_GRW)
!        XPNGEL(ICENSUS,1) = INDEX
!        DO 330 KYR = 2 , UNYEAR + UNXPH
!        INDEX = INDEX * (DBLE(1.0) + ESC)
!        XPNGEL(ICENSUS,KYR) = INDEX
!        330       CONTINUE
!        END IF
!
!        REAL XPDSEL(MNUMCR,MNXYR)
!
!        IF ((XPDSEL(ICENSUS,1) .LT. 0.1 ).OR.(USW_EXP .EQ. 1)) THEN
!        INDEX = DBLE(1.0)
!        ESC = DBLE(UOL_GRW)
!        XPDSEL(ICENSUS,1) = INDEX
!        DO 340 KYR = 2 , UNYEAR + UNXPH
!        INDEX = INDEX * (DBLE(1.0) + ESC)
!        XPDSEL(ICENSUS,KYR) = INDEX
!        340       CONTINUE
!        END IF
!
!        REAL XPRHEL(MNUMCR,MNXYR)
!
!        IF ((XPRHEL(ICENSUS,1) .LT. 0.1 ).OR.(USW_EXP .EQ. 1)) THEN
!        INDEX = DBLE(1.0)
!        ESC = DBLE(UOL_GRW)
!        XPRHEL(ICENSUS,1) = INDEX
!        DO 350 KYR = 2 , UNYEAR + UNXPH
!        INDEX = INDEX * (DBLE(1.0) + ESC)
!        XPRHEL(ICENSUS,KYR) = INDEX
!        350       CONTINUE
!        END IF
!
!        REAL XPRSEL(MNUMCR,MNXYR)
!
!        IF ((XPRSEL(ICENSUS,1) .LT. 0.1 ).OR.(USW_EXP .EQ. 1)) THEN
!        INDEX = DBLE(1.0)
!        ESC = DBLE(UOL_GRW)
!        XPRSEL(ICENSUS,1) = INDEX
!        DO 360 KYR = 2 , UNYEAR + UNXPH
!        INDEX = INDEX * (DBLE(1.0) + ESC)
!        XPRSEL(ICENSUS,KYR) = INDEX
!        360       CONTINUE
!        END IF
!
!        REAL XPRLEL(MNUMCR,MNXYR)
!
!        IF ((XPRLEL(ICENSUS,1) .LT. 0.1 ).OR.(USW_EXP .EQ. 1)) THEN
!        INDEX = DBLE(1.0)
!        ESC = DBLE(UOL_GRW)
!        XPRLEL(ICENSUS,1) = INDEX
!        DO 370 KYR = 2 , UNYEAR +UNXPH
!        INDEX = INDEX * (DBLE(1.0) + ESC)
!        XPRLEL(ICENSUS,KYR) = INDEX
!        370       CONTINUE
!        END IF
!
!        REAL XPUREL(MNUMCR,MNXYR)
!
         IF ((XPUREL(ICENSUS,1) .LT. 0.1 ).OR.(USW_EXP .EQ. 1)) THEN
            INDEX = DBLE(1.0)
            ESC = DBLE(UUR_GRW)
            XPUREL(ICENSUS,1) = INDEX
            DO 380 KYR = 2 , UNYEAR + UNXPH
               INDEX = INDEX * (DBLE(1.0) + ESC)
               XPUREL(ICENSUS,KYR) = INDEX
  380       CONTINUE
         END IF
!
  400 CONTINUE
      RETURN
      END
!
!This main program is used for testing subroutine RDBD_UECP outside of NEMS as a standalone program
!        PROGRAM TESTECPDAT
!        IMPLICIT NONE
!        include 'parametr'
!        include 'ncntrl'
!        INTEGER IRG,UNRGNS
!        call rdcntrl  (special test version with open statements instead of file_mgr calls
!        UNRGNS=22 ! OR 23
!        CURIYR=11 ! 2000 for testing
!        DO IRG=1,UNRGNS
!          CALL RDBD_UECP(IRG)
!        ENDDO
!
!        END
!
        SUBROUTINE RDBD_UECP(NERC)
		
		USE EPHRTS_SWTICHES
		
         IMPLICIT NONE

!      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY.xlsx

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'entcntl'
      include 'ecpcntl'
      include 'bildin'
      include 'dispinyr'
      include 'uefdout'
      include 'uecpout'
      include 'emission'
      include 'ecp_coal'
      include 'ecp_nuc'
      include 'macout'
      include 'eusprc'
      include 'edbdef'
      include 'elout'
!
      COMMON /RSVMRG/ EPMRMIN
      REAL EPMRMIN(MNUMNR)

      REAL*8 HR_EFF(NDREG,ECP_D_CAP,MAX_KNOTS), HR_LL(NDREG,ECP_D_CAP,MAX_KNOTS)
      INTEGER*4 HR_KNOTS(NDREG,ECP_D_CAP)
      COMMON/HR_ADJ/HR_EFF, HR_LL, HR_KNOTS

      common/nemswk1/xmlout
      integer xmlout
!
      INTEGER*4 NERC,IRG,ECP,IC,IECP,FRG,IY,KNOTS
      INTEGER*2 ADJYRBS
      REAL*4 ADJYRWT(MNUMYR)
      INTEGER*4 IMPORT,POL,IP,IBTP,IBBN,JYR
      INTEGER*4 ICAP,INT,RNW,I,J,IP2,ICHK,JCAP
      INTEGER*4 TMP_BTP
      INTEGER*2 TMP_INDX(ECP_D_CAP)
      INTEGER*2 BTP_INDX(ECP_D_CAP), IRCF, ICFS
      INTEGER*4 FUEL_RGN_MAP(4,23)
      REAL*4 TMP_NOX(ECP_D_CAP*ECP_D_BTP)
      REAL*4 TMP_PCT(ECP_D_CAP*ECP_D_BTP)
      REAL*4 TCES_DUALS(MNUMYR,ECP_D_XPH)
      REAL*8 MAXCF,TFOR,TPMR,TLFR,FCTR
      CHARACTER*2 TMP_CAP(ECP_D_CAP*ECP_D_BTP)
      CHARACTER*2 TMP_FT(ECP_D_CAP*ECP_D_BTP)
      CHARACTER*2 TMP_NCT(ECP_D_CAP*ECP_D_BTP)
      CHARACTER*1 TMP_BTM(ECP_D_CAP*ECP_D_BTP)
      INTEGER*4 ret_code
      LOGICAL NEW
      REAL*4 METALPPI(MNUMYR)
      REAL*4 P2MAX
      REAL*4 DSICAP(7),DSIOVR(7),DSIFOM(7)
      REAL*4 P2LIM(MNUMYR-UISTYR+1,MNUMNR)
      REAL*4 TEMCES_DUALS(MNUMYR-UISTYR+1,UNXPH)

      INTEGER*4    DMSZ
      PARAMETER (DMSZ = 60)
      CHARACTER*40 DUMMY(DMSZ)             ! DUMMY COLUMN IN DATA TABLES
      CHARACTER*1 NUMCH(10)

      INTEGER*4 YR,RG,XYR,XFR
!
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
!
      INTEGER*4 CHK$RSUM,CHK$PRF
      DATA NUMCH/'1','2','3','4','5','6','7','8','9','0'/
      character*30 sheet
      integer wkunit
      character*16 rngnam,regstr*2

      real rtemp(NOX_D_GRP)
      real rtemp17(mnumnr-3,mnumnr-3),RTEMP19(mnumcr-1,MNUMNR-3),RTEMP20(51,MNUMNR-3),rtemp16(mnumnr-3,maxnfr),rtemp18(mnumnr-3,mnumnr-3)
      REAL EXTANNADJ(ECP_D_CAP,MNUMYR)
      REAL EPTLOSSPRE(mnumnr-3,mnumnr-3),EPTCRFPRE
      INTEGER K


      IF (NERC .EQ. 1) THEN
        WRITE (6,*)  '***BEGINNING RDBD_UECP FOR U.S. DATA - US00***'
        WRITE (UF_MSG,*)&
            '***BEGINNING RDBD_UECP FOR U.S. DATA - US00***'
      ENDIF
    !  wkunit=699                                      ! for testing outside of NEMS
    !  fname='m:\rec\cm1\emm_documentation\ECPDATX.XLSX' ! for testing outside of NEMS


      if(nerc.eq.1) then
      !
      !        OPEN FILE OF NEW BUILD DATA
      !
       FL_CNXT_CST = 0.0
       EMM_CNXT_CST = 0.0
       EMM_CNDC_CST = 0.0
       EPTLOSS=0. 
       call ECPDAT_REG_FUEL
       call ECPDAT_REG_REG
       CALL ECPDAT_REG1
!       write(6,*)'check  UPBLDTYP ', (UPBLDTYP(k),k=1,MNUMNR)
       CALL ECPDAT_REG_ECPT1
       CALL ECPDAT_REG_NOX
       CALL ECPDAT_REG_YR


         NEW = .true.
         XMLOUT = FILE_MGR('O','ECPXMLOUT',NEW)      ! debug file to echo data read from xlsx file

     !   open(wkunit,file=fname,status='old',readonly)  ! for testing outside of NEMS

        !INITIALIZE FIRST DECISION YEAR FOR ECP AND RESET ECP START YEAR
!       ECP_FYR=YEARPR
!       UPSTYR=ECP_FYR
        ECP_FYR=UPSTYR
        !STORE EMM SO2 PRICE (READ IN) UNTIL ECP GENERATES IT
        DO IY=1, UPSTYR-UHBSYR-1
            DO IC=1, MX_SO2_GRP
                IF (UPSO2PRC(IY,IC) .LT. 9999.0) ECP_PSO2(0,IY,IC)=UPSO2PRC(IY,IC)/UPGNPD(IY)
            END DO
        END DO
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !GENERAL SHEET, WRITTEN BY CM1 4/19/13                                                             !
        !THESE ITEMS DID NOT FIT NEATLY INTO ANY OTHER CATEGORY, AND SO ARE READ IN HERE                   !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
        wkunit = FILE_MGR('O','ECPDATY',.false.)
        SHEET='GENERAL'
        call readRngXLSX(wkunit,trim(sheet))
        !READ INDEX TO RELIABILITY PRICE METHOD (0=INPUT, 1=SEARCH)
        call getrngi4('UPRELSW         ',UPRELSW,1,1,1)      !SWITCH FOR RSV MRG TYPE
        !READ INDEX TO IDENTIFY BUILD TYPE (1 = UTL(REG), 2 = NUG(COMP))
        call getrngr('UPDCFB          ',UPDCFB,ECP_D_DGN,2,1)  !MIN/MAX CAPACITY FACTOR
        call getrngr('UPDSEF          ',UPDSEF,ECP_D_DGN,1,1)  !SCRUBBER EFFICIENCY FRACTION
        call getrngr('UPDSRT          ',UPDSRT,ECP_D_DGN,1,1)  !SCRUBBER SO2 EMISSION RATE
        call getrngr('UPDCEF          ',UPDCEF,ECP_D_DGN,1,1)  !CARBON SEQUESTRATION EFFICIENCY FRACTION
        call getrngi4('UDFLTP          ',UDFLTP,ECP_D_DGN,ECP_D_FPP,1)!ECP FL IND
        !DISTRIBUTED GENERATION VARIABLES
        call getrngr('UPDGAVR         ',UPDGAVR,1,1,1)       !FRACTION AVOIDED T&D BY DG
        call getrngr('UPDGFPR         ',UPDGFPR,1,1,1)       !DG FUEL PRICE PREMIUM
        UPDGFPR = UPDGFPR / SCALPR

        !READ INT BOUND SWITCH UPINTSW(0=NO,1=UPIBND(PEAK),2=UPINTBND(GEN),3=UPINTBND(CAP)
        call getrngi4('UPINTSW         ',UPINTSW,1,1,1)       !SW FOR INT BND TYPE
        !READ INTERMITTENT BOUND INCREMENT (FOR UPINTSW=2 OR =3)
        call getrngr('UPINTINC        ',UPINTINC,1,1,1)       !FRACTION OF TOTAL GEN/CAP
        !READ INTERMITTENT BOUND (FOR UPINTSW=2 OR =3)
        call getrngr('UPINTBND        ',UPINTBND,1,1,1)       !FRACTION OF TOTAL GEN/CAP (MAX)
        call getrngr('UPINTBD0        ',UPINTBD0,1,1,1)       !FRACTION OF TOTAL GEN/CAP (INITIAL)
        !READ IN Z STATISTIC FOR INTERMITTENT CAPACITY CREDIT CALCULATION
        call getrngr('UPINTZ          ',UPINTZ,1,1,1)
        !READ IN INTERMITTENT ONLY VARIABLES
        call getrngr('UPICCF          ',UPICCF,ECP_D_INT,1,1)   !CAPACITY CREDIT SCALING FACTOR
        call getrngr('UPIBND          ',UPIBND,ECP_D_INT,1,1)   !BLD LIMIT AS A FRACTION OF PEAK
        call getrngr('UPINTLOW        ',UPINTLOW,ECP_D_INT,1,1) !LOWER INTERMITTENT THRESHOLD (CAP CRED=UPICCF)
        call getrngr('UPINTEXP        ',UPINTEXP,ECP_D_INT,1,1) !HALF LIFE OF CAP CREDIT DECAY
	    !READ IN MARKET SHARING GROUPS (0=NOT IN MARKET SHARING, 1=GROUP 1, 2=GROUP2, ETC)
	    call getrngi4('UPMSHSW         ',UPMSHSW,ECP_D_CAP,1,1) !SW FOR MARKET SHARING GROUPING
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !READ ECP PLANT DATA                                                                               !
        !COSTPERF SHEET FOR COST AND PERFORMANCE DATA, WRITTEN BY CM1 4/19/13                              !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        sheet='COSTPERF'
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
        wkunit = FILE_MGR('O','ECPDATY',.false.)
        call readRngXLSX(wkunit,trim(sheet))
        !COST AND FINANCIAL DATA
        call getrngi4('UPAVLOV         ',UPAVLOV,ECP_D_CAP,1,1) !CAL YEAR OF 1ST COMM OPER - OVERWRITE IF NOT 0
        call getrngi4('UPAVLYR         ',UPAVLYR,ECP_D_CAP,1,1) !CAL YEAR OF 1ST COMM OPER
        call getrngi4('UPECLF          ',UPECLF,ECP_D_CAP,1,1)  !ECONOMIC LIFE IN YEARS
        call getrngi4('UPTXLF          ',UPTXLF,ECP_D_CAP,1,1)  !TAX LIFE IN YEARS

!       DO ECP = 1 , ECP_D_CAP
!          WRITE(6,3411) ECP, UPECLF(ECP), UPTXLF(ECP)
!3411      FORMAT(1X,"UPECLF_UPTXLF",3(":",I4))
!       END DO

        call getrngi4('UPUCLF          ',UPUCLF,ECP_D_CAP,1,1)  !UTIL CONTRACT LIFE IN YEARS
        call getrngi4('UPNCLF          ',UPNCLF,ECP_D_CAP,1,1)  !NUG CONTRACT LIFE IN YEARS
        call getrngi4('UPNLLF          ',UPNLLF,ECP_D_CAP,1,1)  !NUG LOAN LIFE IN YEARS
        call getrngr('UPVOM           ',UPVOM,ECP_D_CAP,1,1)   !VARIABLE O&M COST
        call getrngr('UPFOM           ',UPFOM,ECP_D_CAP,1,1)   !FIXED O&M COST
        call getrngr('UPCSB           ',UPCSB,ECP_D_CAP,1,1)   !CAPITAL COST CREDIT
        call getrngr('UPVSB           ',UPVSB,ECP_D_CAP,1,1)   !O&M COST CREDIT
        call getrngr('UPHTRT          ',UPHTRT,ECP_D_CAP,1,1)  !HEAT RATE
        !OPERATIONAL PARAMETER DATA
        call getrngr('UPFORT          ',UPFORT,ECP_D_CAP,1,1)  !FORCED OUTAGE RATE FRACTION
        call getrngr('UPPMRT          ',UPPMRT,ECP_D_CAP,1,1)  !PLANNED OUTAGE RATE FRACTION
        call getrngr('UPCCR           ',UPCCR,ECP_D_CAP,1,1)   !CAPACITY CREDIT FRACTION
        call getrngr('UPMCF           ',UPMCF,ECP_D_CAP,1,1)   !MAX CAP FACTOR FRACTION
        call getrngr('UPCTRM          ',UPCTRM,ECP_D_CAP,1,1)  !ADD ON TRANSMISSION COST
        call getrngi4('UPPLYR          ',UPPLYR,ECP_D_CAP,1,1)  !ECP CONSTRUCTION LEAD TIME IN YEARS
        call getrngi4('UPCLYR          ',UPCLYR,ECP_D_CAP,1,1)  !FINANCIAL CONSTRUCTION LEAD TIME IN YEARS
        !CONSTRUCTION PROFILE INPUT AND CALCULATIONS
        call getrngr('UPCPRO          ',UPCPRO,ECP_D_CAP,ECP_D_LCP,1)!CONSTRUCTION PROFILE
!        INSURE THAT RETIREMENT LEAD TIME DOESN'T GO BEYOND PLANNING HORIZON
!        DO JYR = 1 , UNXPH
!          UCL_VIN(JYR) = MIN(UCL_VIN(JYR),UNXPH - UPPLYR(WIPC))
!        END DO
!        INSURE THAT RETIREMENT LEAD TIME DOESN'T EXCEED PLANNING HORIZON
           UNUC_RLT= MIN(UNUC_RLT,UNXPH - UPPLYR(WIAN))
!        INSURE THAT OTHER RETIREMENT LEAD TIME DOESN'T EXCEED PLANNING HORIZON
           UPRTLT = MIN(UPRTLT,UNXPH - UPPLYR(WIPC))
!        INSURE THAT FIRST RETIREMENT DATE IS WHEN ALL PLANTS ARE AVAILABLE
         UNUC_SYR = ECP_FYR + UPCLYR(WICC) - UPRTLT - 1
!
!        Set UPHTRT heat rates to AER values for last historical year for geo,wind, solar, pv, and hydro
!
         DO IECP = 1, ECP_D_CAP
			 
         IF (IECP .EQ. WIGT .OR. IECP .EQ. WIHY .OR. IECP .EQ. WIPS .OR.          &
              IECP .EQ. WIWN .OR. IECP .EQ. WIWF .OR. IECP .EQ. WISO .OR. IECP .EQ. WIPV ) THEN

            UPHTRT(IECP) = EPHTRT_AER(IECP)
         ELSEIF (IECP .EQ. WIWL) THEN
            UPHTRT(IECP) = EPHTRT_AER(WIWN)
         ELSEIF (IECP .EQ. WIPT) THEN
            UPHTRT(IECP) = EPHTRT_AER(WIPV)
         ENDIF
         ENDDO
!
!           Make Sure That Max Capacity Factor is Consistent with Forced Outage Rate, Planned Maintenance Rate and implied Load Following Rate
!

         DO ECP = 1 , ECP_D_CAP
    !        WRITE(6,*)'upmcf ',CURIYR,CURITR,ECP,UPMCF(ECP)
            TFOR = UPFORT(ECP)
            TPMR = UPPMRT(ECP)
            MAXCF = UPMCF(ECP)
            TLFR = 1.0 - (MAXCF / ((1.0 - TFOR) * (1.0 - TPMR)))
            IF (TLFR .LT. 0.0) THEN
               IF (MAXCF .LT. 1.000) THEN
                  MAXCF = (1.0 - TFOR) * (1.0 - TPMR)
                  FCTR = (UPMCF(ECP) / MAXCF) ** (DBLE(1.0) / DBLE(2.0))
                  UPFORT(ECP) = DBLE(1.0) - FCTR * (DBLE(1.0) - TFOR)
                  UPPMRT(ECP) = DBLE(1.0) - FCTR * (DBLE(1.0) - TPMR)
!
                  IF (UPFORT(ECP) .LT. 0.0 .OR. UPPMRT(ECP) .LT. 0.0) THEN
                     UPFORT(ECP) = 1.0 - (UPMCF(ECP) ** (1.0 / 2.0))
                     UPPMRT(ECP) = 1.0 - (UPMCF(ECP) ** (1.0 / 2.0))
                  END IF
               ELSE
                  UPMCF(ECP) = (1.0 - TFOR) * (1.0 - TPMR)
                  TLFR = 0.0
               END IF
               WRITE(6,3492) CURIYR+UHBSYR,CURITR,ECP,MAXCF,TFOR,TPMR,TLFR,UPMCF(ECP),UPFORT(ECP),UPPMRT(ECP)
 3492          FORMAT(1X,"ERROR_in_ECPDAT_OUTAGE_RATES",3(":",I4),":Input_Values",4(":",F6.3),":Revised_Rates",3(":",F6.3),": 0.000")
            END IF
         END DO
!
!        INSURE FIRST AVAILABLE YEAR FOR NEW CAPACITY ISN'T BEFORE LEAD TIME HAS ELAPSED
!        TURBINES ARE EXCLUDED TO PROVIDE ESCAPE VALVE
!
         DO ICAP = 1 , ECP_D_CAP
           IF (UPVTYP(ICAP) .EQ. 1 .AND. UPLNTCD(ICAP) .NE. 'CT')THEN
              IF (UPAVLOV(ICAP) .LE. 0)THEN
                 UPAVLYR(ICAP) = MAX(UPAVLYR(ICAP),ECP_FYR + UPCLYR(ICAP))
              ELSE
                 UPAVLYR(ICAP) = UPAVLOV(ICAP)
              END IF
           END IF
!          INSURE INITIAL DATE FOR COAL TO NG CONVERSIONS and CCS retrofits ALSO ACCOUNTS FOR LEAD TIME
           IF (ICAP .EQ. WING)THEN
              IF (UPAVLOV(ICAP) .LE. 0)THEN
                 UCL_CL_NG_SYR = MAX(UCL_CL_NG_SYR,ECP_FYR + UPCLYR(ICAP)+1)
                 UPAVLYR(ICAP) = UCL_CL_NG_SYR
              ELSE
                 UCL_CL_NG_SYR = MAX(UPAVLOV(ICAP),ECP_FYR + UPCLYR(ICAP)+1)
                 UPAVLYR(ICAP) = UCL_CL_NG_SYR
              END IF
           END IF
           IF (ICAP .EQ. WIA2) THEN
              IF (UPAVLOV(ICAP) .LE. 0)THEN
                 NG_CCS_SYR = MAX(NG_CCS_SYR,ECP_FYR + UPCLYR(ICAP)+1)
                 UPAVLYR(ICAP) = NG_CCS_SYR
              ELSE
                 NG_CCS_SYR = MAX(UPAVLOV(ICAP),ECP_FYR + UPCLYR(ICAP)+1)
                 UPAVLYR(ICAP) = NG_CCS_SYR
              END IF
           ENDIF
         END DO

!        CHECK PROFILE TO MAKE SURE THAT THEY SUM TO 1.0 OVER PROPER # YRS

         RET_CODE = CHK$PRF(UPCPRO,UPCLYR,'UPCPRO',ECP_D_CAP,ECP_D_LCP,1.0, &
          .TRUE.,UF_MSG)

!   PARAMETERS RELATED TO SCRUBBER AND SEQ. RETROFITS
        call getrngr('UPPCFB          ',UPPCFB,ECP_D_DSP,2,1) !MIN/MAX CAP FACTORS
        call getrngr('UPPSEF          ',UPPSEF,ECP_D_DSP,1,1) !SCRUBBER EFFICIENCY FRACTION
        call getrngr('UPPSRT          ',UPPSRT,ECP_D_DSP,1,1) !SCRUBBER SO2 EMISSION RATE
        call getrngr('UPPCEF          ',UPPCEF,ECP_D_CAP,1,1)!CARBON SEQUESTRATION EFFICIENCY FRACTION
        call getrngi4('UPFLTP          ',UPFLTP,ECP_D_DSP,ECP_D_FPP,1)  !ECP FL IND
        call getrngr('UPOVR           ',UPOVR,ECP_D_CAP,1,1)   !OVERNIGHT CAPITAL COST
        call getrngi4('UPLRSYR         ',UPLRSYR,ECP_D_CAP,1,1) !START CALENDAR YEAR FOR NEW TECHNOLOGY
        call getrngi4('UPLRLYR         ',UPLRLYR,ECP_D_CAP,1,1) !LAST CALENDAR YEAR FOR NEW TECHNOLOGY
        call getrngr('UPMSSIZ         ',UPMSSIZ,ECP_D_CAP,1,1) !TYPICAL UNIT SIZE
        call getrngr('UPLRMIN         ',UPLRMIN,ECP_D_CAP,1,1) !MINIMUM UNIT FRACTION SIZE FOR M-S
        call getrngr('UPLRPC          ',UPLRPC,ECP_D_CAP,1,1)  !PROJECT CONTINGENCY FACTOR
        call getrngr('UPPCEF_MIN      ',UPPCEF_MIN,ECP_D_CAP,1,1)!MINIMUM CARBON SEQUESTRATION EFFICIENCY FRACTION FOR UNITS AFTER 45Q CREDIT IS OVER
        call getrngr('UPCCS_INVSH     ',UPCCS_INVSH,ECP_D_CAP,1,1)!CCS share of new plant OVR cost (for CCATS)
        call getrngr('UPCCS_VOMSH     ',UPCCS_VOMSH,ECP_D_CAP,1,1)!CCS share of new plant VOM cost (for CCATS)
        call getrngr('UPCCS_FOMSH     ',UPCCS_FOMSH,ECP_D_CAP,1,1)!CCS share of new plant FOM cost (for CCATS)
        DO ECP = 1 , ECP_D_CAP
            UPCCS_INVSH0(ECP) = UPCCS_INVSH(ECP)   !store initial OVR share before adjusting for learning
        ENDDO
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !READ SUBSIDY PLANT DATA                                                                           !
        !SUBSIDY SHEET FOR RENEWABLES, WRITTEN BY CM1 4/19/13                                              !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        sheet='SUBSIDY'
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
        wkunit = FILE_MGR('O','ECPDATY',.false.)
        call readRngXLSX(wkunit,trim(sheet))
        
        !READ IRA PARAMETERS FOR DYNAMIC PHASE OUT OF TAX CREDITS
        call getrngi4('UPIRAYR0        ',UPIRAYR0,1,1,1)       !YEAR TO TRIGGER CHECKING FOR DYNAMIC PHASE OUT OF IRA TAX CREDITS
        call getrngi4('UPIRA_EMYR      ',UPIRA_EMYR,1,1,1)     !BASE YEAR FOR EMISSIONS TARGET FOR IRA TAX CREDIT PHASE OUT
        call getrngr('UPIRA_EMPCT     ',UPIRA_EMPCT,1,1,1)     !REQUIRED REDUCTION IN EMISSISONS FROM TARGET YEAR TO TRIGGER PHASE OUT
        
        !READ IN PRODUCTION TAX CREDIT (PTC) FOR ALL TECHNOLOGIES
        call getrngr('UPGSUB          ',UPGSUB,ECP_D_CAP,1,1)  !GENERATION SUBSIDY IN MILLS/KWH
        call getrngi4('UPGSY1          ',upgsy1,ECP_D_CAP,1,1)  !START YEAR OF GENERATION SUBSIDY
        call getrngi4('UPGSYL          ',upgsyl,ECP_D_CAP,1,1)  !LAST YEAR OF GENERATION SUBSIDY
        call getrngi4('UPGSYR          ',UPGSYR,ECP_D_CAP,1,1)  !NUMBER OF YEARS SUBSIDY IN EFFECT FROM ONLINE YEAR
        call getrngi4('UPGSTY          ',UPGSTY,ECP_D_CAP,1,1)  !REAL (=0) OR NOMINAL(~=0) SUBSIDY
        call getrngi4('UPGSYD          ',UPGSYD,ECP_D_CAP,1,1)  !YEAR DOLLARS SPECIFIED FOR SUBSIDY
        call getrngr('UPGSMX          ',UPGSMX,ECP_D_CAP,1,1)  !MAXIMUM ANNUAL PAYMENT (0=NO MAX)
        call getrngi4('UPGSUBPT        ',UPGSUBPT,ECP_D_CAP,1,1)!SWITCH TO USE ANNUAL PRESPECIFIED PTC VALUES

!        IF USING ANNUAL PTC VALUES, MAKE SURE PTC CASE SWITCH IS TURNED ON
!        SET FIRST AND LAST YEARS OF PTC USING ANNUAL INPUTS
!        CHECK TO MAKE SURE NUMBER OF YEARS (UPGSYR) AND YEAR $ (UPGSYD) ARE SET
         DO ECP = 1 , ECP_D_CAP
            IF (UPSUBCAS(ECP) .EQ. 1 .OR. UPSUBCAS(ECP) .EQ. 3)THEN
!              UPSUBCAS(ECP) = 1
               IF (UPGSY1(ECP) .LE. 0 .OR. UPGSYL(ECP) .LE. 0 .OR.  &
                   UPGSYR(ECP) .LE. 0 .OR. UPGSYD(ECP) .LE. 0)THEN
                  WRITE(6,1234) UPLNTCD(ECP),UPGSY1(ECP),UPGSYL(ECP),UPGSYR(ECP),UPGSYD(ECP)
 1234 FORMAT(1h ,'STOPPING RUN DUE TO INVALID PTC PARAMETER FOR PLANT TYPE',A3,  &
                  ': FIRST PTC YEAR (UPGSY1) =',I5,  &
                  '; LAST PTC YEAR (UPGSYL) =',I5,  &
                  '; NUMBER OF PTC YEARS (UPGSYR) =',I3,  &
                  '; YEAR $ FOR PTC (UPGSYD) =',I5)
                  STOP
               END IF
            END IF
         END DO

        DO ICAP = 1 , ECP_D_CAP
!          WRITE(18,7717) CURIRUN, CURIYR+1989, ICAP, WISM, UPAVLYR(ICAP), UPOVR(ICAP), UPFOM(ICAP)
 7717      FORMAT(1X,"UPOVR_INPUT",4(":",I4),2(":",F21.3))
        END DO

        !READ CASE SUBJECT TO CAPACITY LIMITS ON SUBSIDIES, IF ANY
        call getrngi4('UPSUBCAS        ',UPSUBCAS,ECP_D_CAP,1,1)!SUBSIDY CASE
        !READ CAPACITY LIMIT ON SUBSIDY, IF ANY
        call getrngr('UPSUBLIM        ',UPSUBLIM,ECP_D_CAP,1,1)!FRACTION OF CAPACITY IN CPS BND
        DO ECP = 1 , ECP_D_CAP
           IF (UPSUBCAS(ECP) .GT. 0 .AND. UPSUBLIM(ECP) .LE. 0.0)UPSUBLIM(ECP) = 999.99
        END DO
        call getrngi4('UPSUBFCF        ',UPSUBFCF,ECP_D_CAP,1,1)!SUBSIDY CAP LIMIT ON 1ST COME 1ST SERVE BASIS (0=YES, 1=NO)

        call getrngr('UPGSUBYR        ',UPGSUBYR(1,UISTYR),ECP_D_CAP,MNUMYR-UISTYR+1,1)!PTC OVERRIDES
!
!        CONVERT PTC FROM SPECIFIED YR $ TO MODEL YEAR $
!
         DO ICAP = 1 , ECP_D_CAP
            IF (UPSUBCAS(ICAP) .EQ. 1 .OR. UPSUBCAS(ICAP) .EQ. 3)THEN
               IF (UPGSYD(ICAP) .GT. 0 .AND. UPGSTY(ICAP) .LE. 0)THEN
                  UPGSUB(ICAP) = UPGSUB(ICAP) / UPGNPD(UPGSYD(ICAP) - UHBSYR)
                  UPGSMX(ICAP) = UPGSMX(ICAP) / UPGNPD(UPGSYD(ICAP) - UHBSYR)
               END IF
               IF (UPGSUBPT(ICAP) .LE. 0)THEN
                  DO IY = 1 , MNUMYR
                     IF ((IY + UHBSYR) .GE. UPGSY1(ICAP) .AND. (IY + UHBSYR) .LE. UPGSYL(ICAP))THEN
                        UPGSUBYR(ICAP,IY) = UPGSUB(ICAP)
                     ELSE
                        UPGSUBYR(ICAP,IY) = 0.0
                     END IF
                  END DO
               ELSE
                  UPGSY1(ICAP) = 0
                  UPGSYL(ICAP) = 0
                  DO IY = 1 , MNUMYR
                     IF (UPGSUBYR(ICAP,IY) .GT. 0.0)THEN
                        IF (UPGSY1(ICAP) .EQ. 0)UPGSY1(ICAP) = IY + UHBSYR
                        UPGSYL(ICAP) = IY + UHBSYR
                     END IF
                     IF (UPGSYD(ICAP) .GT. 0 .AND. UPGSTY(ICAP) .LE. 0)THEN
                        UPGSUBYR(ICAP,IY) = UPGSUBYR(ICAP,IY) / UPGNPD(UPGSYD(ICAP) - UHBSYR)
                     END IF
                  END DO
               END IF
            ELSE
               DO IY = 1 , MNUMYR
                  UPGSUBYR(ICAP,IY) = 0.0
               END DO
            END IF
         END DO
!
!     UPCSBYR: ITC OVERRIDES
!
        call getrngr('UPCSBYR         ',UPCSBYR(1,UISTYR),ECP_D_CAP,MNUMYR-UISTYR+1,1) !ITC OVERRIDES

        IF (UPIRAYR0 .LT. 9000) call IRAPHASE  ! calculate if phase out of tax credits is needed for IRA 
                                     ! can set UPIRAYR0 to 9999 to skip this and phaseout can be hardwired through annual inputs
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !READ CCS RETROFIT DATA                                                                            !
        !CCSRET SHEET FOR CCS RETROFIT DATA, WRITTEN BY CM1 4/19/13                                        !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        sheet='CCSRET'
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
        wkunit = FILE_MGR('O','ECPDATY',.false.)
        call readRngXLSX(wkunit,trim(sheet))
        call getrngr('UPCQMAX         ',UPCQMAX,1,1,1)      !CCS RETROFIT ANNUAL LIMIT IN GW
        call getrngr('UPCRMAX         ',UPCRMAX,1,1,1)      !CCS RETROFIT MAX ANNUAL INC FRACTION
        call getrngr('UPCCSADJ        ',UPCCSADJ(UISTYR),MNUMYR-UISTYR +1,1,1)!CCS RETROFIT CAPITAL COST ADJUSTMENT FACTOR
        call getrngr('UPCCSCAP        ',UPCCSCAP,1,1,1)     !CAPACITY THRESHOLD (I.E. 500.0 OR 0.0 FOR NONE)
        call getrngr('UPCCSHRT        ',UPCCSHRT,1,1,1)     !HEATRATE THRESHOLD (i.E., 12000.0 OR 99999.9 FOR NONE)
        call getrngi4('UPSEQCAS        ',UPSEQCAS,1,1,1)     !SEQ CASE -- 1766, 2191, ETC.
        call getrngi4('UPSEQCAR        ',UPSEQCAR,1,1,1)     !SEQ CASE -- TYPE (1=CAR, 2=CO2)
        call getrngi4('UPSEQNYR        ',UPSEQNYR,1,1,1)     !SEQ CASE -- DURATION OF BONUS ALLOWANCES
        call getrngi4('UPSEQLIM        ',UPSEQLIM,1,1,1)     !SEQ BONUS ALLOWANCES LIMITED (0=NO, 1=YES)
!        READ ANNUAL BONUS RATE FOR CCS ALLOWANCES
        call getrngr('UPSEQBYR        ',UPSEQBYR(UISTYR),MNUMYR-UISTYR +1,1,1)!SEQ BONUS ALLOWANCE RATE

        IF (UPSEQCAS .EQ. 2454)THEN
           DO IY = UISTYR , MNUMYR
              IF (EMETAX(2,IY) .GT. 0.0)THEN
                 IF (IY .LE. UNYEAR)THEN
                    UPSEQBYR(IY) = (90.0 / MC_JPGDP(2009 - UHBSYR)) / (EMETAX(2,IY) * (12.0 / 44.0) * 1000.0)
                 ELSE
                    UPSEQBYR(IY) = (90.0 / MC_JPGDP(2009 - UHBSYR)) /   &
                    (EMETAX(2,UNYEAR) * (12.0 / 44.0) * 1000.0 * UPCARGRW ** FLOAT(IY - UNYEAR))
                 END IF
              END IF
           END DO
        END IF
        !        READ ANNUAL (OR TOTAL) LIMITS ON CCS BONUS ALLOWANCES

        call getrngr('UPSEQBLM        ',UPSEQBLM(UISTYR),MNUMYR-UISTYR +1,1,1)!SEQ BONUS ALLOWANCE LIMITS BY YEAR (OR TOTAL)

!        IF BONUS LIMITS IN CO2 THEN CONVERT TO CAR
         IF (UPSEQLIM .GT. 0 .AND. UPSEQCAR .EQ. 2)THEN
           DO ECP = UISTYR , MNUMYR
             UPSEQBLM(ECP) = UPSEQBLM(ECP) * 12 / 44
           END DO
         END IF
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !READ REGIONAL BUILD INFORMATION DATA                                                              !
        !BLDINFO SHEET FOR BUILD LIMIT DATA, WRITTEN BY CM1 4/19/13                                        !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        sheet='BLDINFO'
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
        wkunit = FILE_MGR('O','ECPDATY',.false.)
        call readRngXLSX(wkunit,trim(sheet))
        call getrngi4('ESTSWCSM        ',ESTSWCSM,1,1,1)             !ELASTICITY MULTIPLIER SWITCH (1=CST/CAP RATIO, 2=DIRECT INPUT)
        call getrngi4('ESTSWTCH        ',ESTSWTCH,ECP_D_CAP,1,1)       !SWITCH TO USE
        call getrngr('ESTMNADD        ',ESTMNADD,ECP_D_CAP,1,1)       !MIN CAPACITY ADDITION FOR STEP 1
        call getrngr('ESTCPINC        ',ESTCPINC,ECP_D_CAP,1,1)       !INCREASE IN CAPACITY
        call getrngr('ESTCSINC        ',ESTCSINC,ECP_D_CAP,1,1)       !INCREASE IN COST/INCREASE IN CAPACITY
        call getrngr('ESTCPSTP        ',ESTCPSTP,ECP_D_CAP,ECP_D_SSTP,1)!SIZE OF SUPPLY STEPS
        call getrngr('ESTCPCSM        ',ESTCPCSM,ECP_D_CAP,ECP_D_SSTP,1)!DIRECT MULTIPLIER INPUT (IF SW=2)

!        RESET ELASTICITY SWITCH TO NEGATIVE IF USING OTHER SUPPLY CURVES (E.G., GEO,MSW)
         DO I = 1 , ECP_D_CAP
           IF (ESTSWTCH(I) .GE. 10)ESTSWTCH(I) = -1
         END DO
        call getrngi4('UPTMRM          ',UPTMRM,ECP_D_CAP,1,1)         !SWITCH TO USE CAPACITY OR OPERATES IN RESERVE MARGIN ROWS
        call getrngr('UPANNLMT        ',UPANNLMT(1,UISTYR),ECP_D_CAP,MNUMYR-UISTYR+1,1)  !NATIONAL ANNUAL BUILD LIMITS


!       READ UPCALPHA -- RELAXATION PARAMETER FOR CARBON TAX EXPECTATIONS
        call getrngr('UPCALPHA        ',UPCALPHA,1,1,1)

        !RD OVERBUILD LIMIT; MAX INCREASE FROM PREV YEAR
        call getrngi4('UPOVBDSY        ',UPOVBDSY,1,1,1)             !START YEAR FOR MAX INCREASE
        call getrngr('UPOVBDRT        ',UPOVBDRT,1,1,1)             !MAX INCREASE FOR OVERBUILD FRACTION

        !READ OVERBUILD SWITCH TYPE -- 1=FRACTION PEAK, 2=FRACTION CAP
        call getrngi4('UPOVBDSW        ',UPOVBDSW,1,1,1)

        !READ OVERBUILD LIMIT -- EXOGENOUS YR BY YR METHOD
        call getrngr('UPOVRBLD        ',UPOVRBLD(UISTYR),MNUMYR-UISTYR +1 ,1,1)!FRACTION OF OVERBUILDS ALLOWED
        DO JYR=MNUMYR+1,MNUMYR+UNXPH
            UPOVRBLD(JYR)=UPOVRBLD(MNUMYR)
        END DO
        call getrngr('UPP2LIM         ',P2LIM,MNUMYR-UISTYR+1,IJUMPEMRGN,1)!P2 LIMIT OF PEAK DEMAND

        !SINCE WE CHANGED THE START YEAR TO EXCLUDE THE 1990S, READING INTO THE PERMANENT ARRAY STARTING AT THE MNUMYR-UISTYR +1 ROW
        DO YR=1,MNUMYR-UISTYR+1
            DO RG=1,UNRGNS
                UPP2LIM(YR+UISTYR-1,RG)=P2LIM(YR,RG)
            ENDDO
        ENDDO


        DO I=1,UNRGNS
           DO JYR=1, MNUMYR
              WRITE(18,'(A,I4,I6,F12.6)')' P2 SETTINGS ', I, JYR, UPP2LIM(JYR,I)
           ENDDO
         ENDDO

         P2MAX = 0.0
         DO I=1,MNUMNR
         DO JYR = 1 , MNUMYR + ECP_D_FPH
            IF (JYR .LE. MNUMYR)THEN
               IF (UPP2LIM(JYR,I) .GT. P2MAX)P2MAX = UPP2LIM(JYR,I)
            ELSE
               UPP2LIM(JYR,I) = UPP2LIM(MNUMYR,I)
            END IF
         END DO
         END  DO
!        IF P2 LIMIT IS 0, RESET AVAILABLE YEAR TO AFTER FORECAST PERIOD
         IF (P2MAX .LE. 0.0)UPAVLYR(WIP2) = UHBSYR + MNUMYR + ECP_D_FPH

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !READ OVERWRITE DATA                                                                               !
        !OVERWRT SHEET FOR OVERWRITES, WRITTEN BY CM1 4/19/13                                              !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         sheet='OVERWRT'
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
         wkunit = FILE_MGR('O','ECPDATY',.false.)
         call readRngXLSX(wkunit,trim(sheet))
!        FIRST INITIALIZE ANNUAL ADJUSTMENT FACTORS TO 1.0
         DO IP = 1, ECP_D_CAP
           DO IY = 1, MNUMYR
            UPANNADJ(IP,IY) = 1.0
            ADJYRWT(IY) = 1.0
           ENDDO
         ENDDO
         call getrngi('UPADJYRBS       ',ADJYRBS,1,1,1)  !BASE YEAR FOR MAKING ADJUSTMENTS
         IF (ADJYRBS .NE. 0) THEN  !leave 0 value which indicates overwrite the values
             ADJYRBS = ULRSYR     !set equal to base year for learning (year of last capital cost update)
         ENDIF
         call getrngr('UPADJYRWT       ',ADJYRWT(26),1,36,1)  !WEIGHTING FACTOR FOR MAKING ADJUSTMENTS

         call getrngr('UPANNADJ        ',UPANNADJ(1,26),ECP_D_CAP,36,1) !ANNUAL ADJUSTMENTS


        !cna- temporary overwrite of upannadj with macro data
           DO IP=1,ECP_D_CAP
             DO IY=1, MNUMYR
         !ADJYRBS is the baseline  year to index metals price index from ECPDAT
             if ((MNUMYR.GE.(ADJYRBS-1989)).and.(ADJYRBS.GT.1989)) THEN
               METALPPI(IY)=MC_WPI10(IY)/MC_JPGDP(IY)
                        if (METALPPI(IY).LE.0) METALPPI(IY)=METALPPI(IY-1)
                  IF (IY .GE. (ADJYRBS-1989)) THEN
                   UPANNADJ(IP,IY)=METALPPI(IY)/METALPPI(ADJYRBS-1989)
                   UPANNADJ(IP,IY)= ADJYRWT(IY) * UPANNADJ(IP,IY) + (1.0 * (1.0 - ADJYRWT(IY)))
                  ELSE
                   UPANNADJ(IP,IY) = 1.0
                  ENDIF
             endif
             ENDDO
           ENDDO
!        DO IP = 1,ECP_D_CAP
!        write(6,923) 'UPANNADJ1', IP,UPANNADJ(IP,31),UPANNADJ(IP,32),UPANNADJ(IP,33),UPANNADJ(IP,34),UPANNADJ(IP,35),UPANNADJ(IP,36),UPANNADJ(IP,46)
!        ENDDO
923      FORMAT(1x,A12,I5,7F8.3)

!        read in additional cost factors by year and technology, used for solar tariffs in AEO2019
         DO IP = 1, ECP_D_CAP
           DO IY = 1, MNUMYR
            EXTANNADJ(IP,IY) = 1.0
           ENDDO
         ENDDO

         call getrngr('EXTANNADJ       ',EXTANNADJ(1,16),ECP_D_CAP,31,1) !EXTRA ANNUAL ADJUSTMENTS FOR TARIFFS

         DO IY = 2035 - UHBSYR + 1, MNUMYR
            DO IP = 1, ECP_D_CAP
               EXTANNADJ(IP,IY) = EXTANNADJ(IP,2035 - UHBSYR)
            ENDDO
         ENDDO

!   COMBINE FACTORS INTO UPANNADJ TO FLOW THROUGH TO CAPITAL COSTS

         DO IY = 1, MNUMYR
           DO IP = 1, ECP_D_CAP
           UPANNADJ(IP,IY) = UPANNADJ(IP,IY) * EXTANNADJ(IP,IY)
           ENDDO
         ENDDO
!        DO IP = 1,ECP_D_CAP
!        write(18,923) 'UPANNADJ2', IP,UPANNADJ(IP,28),UPANNADJ(IP,29),UPANNADJ(IP,30),UPANNADJ(IP,31),UPANNADJ(IP,41)
!        ENDDO

         call getrngi4('ECP_OVCC        ',ECP_D_OVCC,ECP_D_CAP,1,1)      !SWITCH TO OVERWRITE CAPITAL COSTS
         call getrngi4('ECP_OVHR        ',ECP_D_OVHR,ECP_D_CAP,1,1)      !SWITCH TO OVERWRITE HEAT RATES
         call getrngi4('ECP_OVFX        ',ECP_D_OVFX,ECP_D_CAP,1,1)      !SWITCH TO OVERWRITE FIXED O&M WITH PRE-SPECIFIED VALUES
         call getrngi4('ECP_OVVR        ',ECP_D_OVVR,ECP_D_CAP,1,1)      !SWITCH TO OVERWRITE VARIABLE O&M WITH PRE-SPECIFIED VALUES
         call getrngr('UPOVCC          ',UPOVCC(1,UISTYR),ECP_D_CAP,MNUMYR-UISTYR +1,1)   !REPLACEMENT CAPITAL COSTS PER PLANT TYPE AND YEAR
         call getrngr('UPOVHR          ',UPOVHR(1,UISTYR),ECP_D_CAP,MNUMYR-UISTYR +1,1)   !REPLACEMENT HEAT RATES PER PLANT TYPE AND YEAR
         call getrngr('UPOVFX          ',UPOVFX(1,UISTYR),ECP_D_CAP,MNUMYR-UISTYR +1,1)   !REPLACEMENT FIXED O&M COSTS PER PLANT TYPE AND YEAR
         call getrngr('UPOVVR          ',UPOVVR(1,UISTYR),ECP_D_CAP,MNUMYR-UISTYR +1,1)   !REPLACEMENT VARIABLE O&M COSTS PER PLANT TYPE AND YEAR
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !READ OVERWRITE DATA                                                                               !
        !OPTLRN SHEET FOR OPTIMISM LEARNING FACTORS, WRITTEN BY CM1 4/22/13                                !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        sheet='OPTLRN'
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
        wkunit = FILE_MGR('O','ECPDATY',.false.)
        call readRngXLSX(wkunit,trim(sheet))
        call getrngc('DUMMY            ',DUMMY,ECP$COMP,1,1)
!        Assign Index to all ECP technologies
         DO IP = 1 , ECP$COMP
            IF (DUMMY(IP) .EQ. "EXC") WIEXC = IP
            IF (DUMMY(IP) .EQ. "PVC") WIPVC = IP
            IF (DUMMY(IP) .EQ. "EXS") WIEXS = IP
            IF (DUMMY(IP) .EQ. "CCT") WICCT = IP
            IF (DUMMY(IP) .EQ. "ACT") WIACT = IP
            IF (DUMMY(IP) .EQ. "SCC") WISCC = IP
            IF (DUMMY(IP) .EQ. "GSF") WIGSF = IP
            IF (DUMMY(IP) .EQ. "SEQ") WISEQ = IP
            IF (DUMMY(IP) .EQ. "BIG") WIBIG = IP
            IF (DUMMY(IP) .EQ. "BCT") WIBCT = IP
            IF (DUMMY(IP) .EQ. "BCC") WIBCC = IP
            IF (DUMMY(IP) .EQ. "FLC") WIFLC = IP
            IF (DUMMY(IP) .EQ. "CNU") WICNU = IP
            IF (DUMMY(IP) .EQ. "ANU") WIANU = IP
            IF (DUMMY(IP) .EQ. "WDP") WIWDP = IP
            IF (DUMMY(IP) .EQ. "GTH") WIGTH = IP
            IF (DUMMY(IP) .EQ. "MSW") WIMSW = IP
            IF (DUMMY(IP) .EQ. "HYD") WIHYD = IP
            IF (DUMMY(IP) .EQ. "HPS") WIHPS = IP
            IF (DUMMY(IP) .EQ. "HP2") WIHP2 = IP
            IF (DUMMY(IP) .EQ. "WND") WIWND = IP
            IF (DUMMY(IP) .EQ. "WFS") WIWFS = IP
            IF (DUMMY(IP) .EQ. "STH") WISTH = IP
            IF (DUMMY(IP) .EQ. "PVM") WIPVM = IP
            IF (DUMMY(IP) .EQ. "PVB") WIPVB = IP
            IF (DUMMY(IP) .EQ. "DGB") WIDGB = IP
            IF (DUMMY(IP) .EQ. "DGP") WIDGP = IP
         END DO
        !DISPATCHABLE PLANT DATA -- OPTIMISM FACTORS BY COMPONENT
        call getrngr('UPLRSIZ         ',UPLRSIZ,ECP$COMP,1,1)      !TYPICAL UNIT SIZE
        call getrngi4('UPLROPS         ',UPLROPS,ECP$COMP,1,1)      !UN # TO START DEC IN TECH OPTIMISM
        call getrngi4('UPLROPE         ',UPLROPE,ECP$COMP,1,1)      !UN # TO END DEC IN TECH OPTIMISM
        call getrngr('UPLROP0         ',UPLROP0,ECP$COMP,1,1)      !INITIAL TECHNICAL OPTIMISM FACTOR

        !LEARNING CURVE DATA FOR CAPITAL COSTS
        call getrngi4('UPLRLCX         ',UPLRLCX,ECP$COMP,1,1)      !EX. TECH--LEARNING
        call getrngi4('UPLRLCU         ',UPLRLCU,ECP$COMP,ECP$LRN,1)!BREAKPOINTS -- LEARNING
        call getrngr('UPLRLCR         ',UPLRLCR,ECP$COMP,ECP$LRN,1)!lEARNING RATES/ SEG.
        call getrngr('UPLRDBL         ',UPLRDBL,ECP$COMP,1,1)      !DOUBLING LIMIT
        call getrngr('UPMNLRN         ',UPMNLRN,ECP$COMP,1,1)      !LEARNING MINIMUM
        call getrngr('UPLRMAX         ',UPLRMAX,1,ECP$LVN,1)       !MAX LEARNING
        call getrngr('UPLCSTWT        ',UPLCSTWT,ECP_D_CAP,ECP$COMP,1)!COST WEIGHTS FOR COMPONENTS FOR LEARNING
        call getrngr('UPLCAPWT        ',UPLCAPWT,ECP_D_CAP,ECP$COMP,1)!CAPACITY WEIGHTS FOR COMPONENTS FOR LEARNING
        call getrngr('UPLCAPWT_INT    ',UPLCAPWT_INT,ECP_D_CAP,ECP$COMP,1)!CAPACITY WEIGHTS FOR COMPONENTS FOR LEARNING - international

!    check inputs are consistent
         DO I = 1, ECP_D_CAP
           DO J = 1, ECP$COMP
             IF (UPLCAPWT(I,J) .GT. 0.0 .and. UPLCSTWT(I,J) .EQ. 0.0) THEN
              write(UF_MSG,100) 'mismatch in capwt/cstwt ',I,J,UPLCAPWT(I,J),UPLCSTWT(I,J)
             ENDIF
           ENDDO
         ENDDO
100      FORMAT(1x,A25,2I4,2F8.4)

        !LEARNING FACTORS FOR HEATRATES
        call getrngr('UPPHRT0         ',UPPHRT0,ECP_D_DSP,1,1)       !INITIAL HEAT RATE
        call getrngr('UPPHRTN         ',UPPHRTN,ECP_D_DSP,1,1)       !NTH-OF-A KIND HEAT RATE
        call getrngi4('UPDHRSW         ',UPDHRSW,ECP_D_DSP,1,1)       !SWITCH FOR HR IMPROVEMENT
        call getrngi4('UPDHRY0         ',UPDHRY0,ECP_D_DSP,1,1)       !YEAR TO START HEATRATE IMPROVEMENT
        call getrngi4('UPDHRYN         ',UPDHRYN,ECP_D_DSP,1,1)       !YEAR TO END HEATRATE IMPROVEMENT
         DO I = 1, ECP_D_DSP
           IF (UPDHRY0(I) .GT. 0) THEN
             UPDHRY0(I) = MAX(UPDHRY0(I),UPAVLYR(I))
           ENDIF
!        OVERWRITE NUCLEAR HEATRATES WITH VALUE FROM ELDATYR
           IF (I .EQ. WICN .OR. I .EQ. WIAN .OR. I .EQ. WISM)THEN
             UPPHRT0(I) = EPHTRT_AER(WIAN)
             UPPHRTN(I) = EPHTRT_AER(WIAN)
           END IF
         ENDDO
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !READ RETROFIT DATA                                                                                !
        !RETROFIT SHEET, WRITTEN BY CM1 4/22/13                                                            !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         sheet='RETROFIT'
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
        wkunit = FILE_MGR('O','ECPDATY',.false.)
        call readRngXLSX(wkunit,trim(sheet))
        call getrngi('TMP_BTP         ',TMP_BTP,1,1,1)            !NUMBER OF PLANT TYPE/BOILER TYPE COMBINATIONS
        !READING IN THE BOILER TYPE CONFIGURATIONS
        call getrngc('TMP_CAP         ',TMP_CAP,ECP_D_CAP,1,1)
        call getrngi('TMP_INDX        ',TMP_INDX,ECP_D_CAP,1,1)
        call getrngi('BTP_INDX        ',BTP_INDX,ECP_D_CAP,1,1)
        call getrngc('TMP_FT          ',TMP_FT,ECP_D_CAP,1,1)
        call getrngc('TMP_BTM         ',TMP_BTM,ECP_D_CAP,1,1)
        call getrngc('TMP_NCT         ',TMP_NCT,ECP_D_CAP,1,1)
        call getrngr('TMP_NOX         ',TMP_NOX,ECP_D_CAP,1,1)
        call getrngr('TMP_PCT         ',TMP_PCT,ECP_D_CAP,1,1)
!
        DO IP = 1 , ECP_D_CAP
           ECP_BBN(IP) = 0
        END DO
!
        DO IBTP = 1 , TMP_BTP
           IP = TMP_INDX(IBTP)
           IBBN = BTP_INDX(IBTP)
           ECP_BBN(IP) = MAX( ECP_BBN(IP) , IBBN)
           ECP_FTP(IBBN,IP) = TMP_FT(IBTP)
           ECP_BTM(IBBN,IP) = TMP_BTM(IBTP)
           ECP_NCT(IBBN,IP) = TMP_NCT(IBTP)
           NOX_NEW(IBBN,IP) = TMP_NOX(IBTP)
           ECP_PCT(IBBN,IP) = TMP_PCT(IBTP)
        END DO

        call getrngi4('USW_RTRSK       ',USW_RTRSK,1,1,1)         !SWITCH TO USE RISK PREMIUM ON RETROFITS
        call getrngr('UPSQMAX         ',UPSQMAX,1,1,1)           !SCRUBBER RETROFIT ANNUAL LIMIT IN GW
        call getrngr('UPSRMAX         ',UPSRMAX,1,1,1)           !SCRUBBER RETROFIT MAX ANNUAL INC (FRACTION)
        call getrngi4('UPSELF          ',UPSELF,1,1,1)            !SCRUBBER RETROFIT ECONOMIC LIFE IN YEARS
        call getrngi4('UPSTXLF         ',UPSTXLF,1,2,1)           !SCRUBBER RETROFIT TAX LIFE IN YEARS
        call getrngi4('UPSTXYR         ',UPSTXYR,1,1,1)           !SCRUBBER RETRO YEAR TO DEFINE TAX TREATMENT
        call getrngi4('UPSCLT          ',UPSCLT,1,1,1)            !SCRUB RETRO CONSTRUCTION LEAD TIME
        call getrngr('UPSCPR          ',UPSCPR,1,ECP_D_LCP,1)      !SCRUBBER RETROFIT CONSTRUCTION PROFILE
        !CHECK PROFILE TO MAKE SURE THAT THEY SUM TO 1.0 OVER THE PROPER # OF YEARS
        RET_CODE=CHK$PRF(UPSCPR,UPSCLT,'UPSCPR',1,ECP_D_LCP,1.0,.TRUE.,UF_MSG)
        call getrngi4('UPSCR2          ',UPSCR2,ECP_D_DSP,1,1)      !ECP PLT TYPE INDEX CONV TO
        call getrngr('UPSHPEN         ',UPSHPEN,ECP_D_DSP,ECP_D_SCR,1)!HEAT RATE PENALTY
        call getrngr('UPSCPEN         ',UPSCPEN,ECP_D_DSP,ECP_D_SCR,1)!CAPACITY PENALTY
        call getrngi4('UPSCSCR         ',UPSCSCR,ECP_D_DSP,ECP_D_CSC,1)!SCR IND

         DO IP = 1 , ECP_D_DSP
            DO I = 1 , ECP_D_CSC
               UPSCRFRM(IP,I) = 0

!              WRITE(6,2043)'RSC UPSCPEN ',IP,I,UPSCPEN(IP,I),UPSCR2(IP)
!2043          FORMAT(A15,1x,2(I4,1x),2(F12.3,1x))

            END DO
         END DO
         DO IP = 1 , ECP_D_DSP
            IF (UPSCR2(IP) .GT. 0) THEN
               IP2 = UPSCR2(IP)
               I = 1
               ICHK = 0
               DO WHILE (ICHK .EQ. 0)
                  IF (UPSCRFRM(IP2,I) .EQ. 0 ) ICHK = 1
                  IF (ICHK .EQ. 0 .AND. I .LT. ECP_D_CSC) THEN
                     I = I + 1
                  ELSE
                     ICHK = 1
                  END IF
               END DO
               UPSCRFRM(IP2,I) = IP
            END IF
         END DO
         !READING IN THE COFIRING RETROFIT DATA
         call getrngr('UPCFCST         ',UPCFCST,ECP_D_RCF,1,1)   !COFIRING RETROFIT CAPITAL COST
         call getrngr('UPCFFOM         ',UPCFFOM,ECP_D_RCF,1,1)   !COFIRING RETROFIT FIXED O&M
         call getrngr('UPCFVOM         ',UPCFVOM,ECP_D_RCF,1,1)   !COFIRING RETROFIT VARIABLE O&M
         call getrngr('UPCFCPEN        ',UPCFCPEN,ECP_D_RCF,1,1)  !COFIRING RETROFIT CAPACITY PENALTY
         call getrngr('UPCFHPEN        ',UPCFHPEN,ECP_D_RCF,1,1)  !COFIRING RETROFIT HEATRATE PENALTY
         call getrngr('UPCFLEV         ',UPCFLEV,ECP_D_RCF,ECP_D_CFS,1)!COFIRING RETROFIT LIMIT FRACTION
         call getrngr('UPCFSTEP        ',UPCFSTEP,ECP_D_RCF,ECP_D_CFS,1)!COFIRING RETROFIT EXTRA TRANSPORTATION COST (87$/MMBTU)
!
         DO IRCF = 1 , ECP_D_RCF
            DO ICFS = 1 , ECP_D_CFS
               IF (UPCFSTEP(IRCF,ICFS) .LT. 9.0) THEN
                  UPCFNSTP(IRCF) = ICFS
               END IF
            END DO
            WRITE(18,7311) CURIYR+UHBSYR,IRCF,(UPCFLEV(IRCF,ICFS),UPCFSTEP(IRCF,ICFS),ICFS=1,UPCFNSTP(IRCF))
 7311       FORMAT(1X,"UPCFLEV",":",I4,":",I2,12(":",F9.3))
         END DO

        call getrngi4('UPCFELF         ',UPCFELF,1,1,1)          !COFIRING RETROFIT ECONOMIC LIFE IN YEARS
        call getrngi4('UPCFCLT         ',UPCFCLT,1,1,1)          !COFIRING RETROFIT CONSTRUCTION LEAD TIME IN YEARS
        call getrngr('UPCFCPR         ',UPCFCPR,1,ECP_D_LCP,1)    !COFIRING RETROFIT CON PROFILE
        call getrngr('UPBQMAX         ',UPBQMAX,1,1,1)          !COFIRING RETROFIT ANNUAL LIMIT (GW)
        call getrngr('UPBRMAX         ',UPBRMAX,1,1,1)          !COFIRING RETROFIT MAX ANNUAL INC AS FRACTION

        !IF NUG DEBT AND EQUITY RATES ARE SPECIFIED, USE THESE AND RESET PREMIUMS TO ZERO
        IF (UPNIRTE .GT. 0.0)UPNIPRM=0.0
        IF (UPNRRTE .GT. 0.0)UPNRPRM=0.0
        !DSI RETROFIT DATA
        call getrngr('UPDSIVOM        ',UPDSIVOM,2,1,1)         !DSI VOM (FF,ESP)
        call getrngr('UPDSISEF        ',UPDSISEF,2,1,1)         !DSI SO2 REMOVAL RATE (FF,ESP)
        call getrngr('DSICAP          ',DSICAP,7,1,1)           !DSI CAPACITY CATEGORY-FF
        call getrngr('DSIOVR          ',DSIOVR,7,1,1)           !DSI CAPITAL COST - FF
        call getrngr('DSIFOM          ',DSIFOM,7,1,1)           !DSI FIXED O&M -FF
        DO ICAP=1, 7
            UPDSICAP(1,ICAP)=DSICAP(ICAP)
            UPDSIOVR(1,ICAP)=DSIOVR(ICAP)
            UPDSIFOM(1,ICAP)=DSIFOM(ICAP)
        END DO
        call getrngr('UPDSICAP        ',DSICAP,7,1,1)           !DSI CAPACITY CATEGORY-ESP
        call getrngr('UPDSIOVR        ',DSIOVR,7,1,1)           !DSI CAPITAL COST - ESP
        call getrngr('UPDSIFOM        ',DSIFOM,7,1,1)           !DSI FIXED O&M -ESP
        DO ICAP=1, 7
            UPDSICAP(2,ICAP)=DSICAP(ICAP)
            UPDSIOVR(2,ICAP)=DSIOVR(ICAP)
            UPDSIFOM(2,ICAP)=DSIFOM(ICAP)
        END DO
        !NUC AGING RETROFIT DATA
        call getrngi4('UPNAYR          ',UPNAYR,1,1,1)          !NUCLEAR AGING RETROFIT AGE TO START
        call getrngi4('UPNELF          ',UPNELF,1,1,1)          !NUCLEAR AGING RETROFIT ECONOMIC LIFE IN YEARS
        call getrngi4('UPNCLT          ',UPNCLT,1,1,1)          !NUCLEAR AGING RETROFIT CONSTRUCTION LEAD TIME IN YEARS
        call getrngr('UPNCPR          ',UPNCPR,1,ECP_D_LCP,1)     !NUCLEAR AGING RETROFIT CON PROFILE
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !READ FINANCIAL DATA                                                                               !
        !FINANCIAL SHEET, WRITTEN BY CM1 4/23/13                                                           !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        sheet='FINANCIAL'
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
        wkunit = FILE_MGR('O','ECPDATY',.false.)
        call readRngXLSX(wkunit,trim(sheet))
        !FINANCIAL PARAMETERS
        call getrngr('UPTXRT          ',UPTXRT,1,1,1)           !UTILITY NET TAX RATE
        call getrngr('UPNIRTE         ',UPNIRTE,1,1,1)          !NUG INTEREST RATE
        call getrngr('UPNRRTE         ',UPNRRTE,1,1,1)          !NUG RET. ON EQUITY RATE
        call getrngr('UPNRPRM         ',UPNRPRM,1,1,1)          !NUG RETURN ON EQUITY
        call getrngr('UPNFDT          ',UPNFDT,1,1,1)           !NUG DEBT FRACTION
        call getrngr('UPBETA          ',UPBETA,1,1,1)           !BETA FOR EWG BUILDS COST OF CAPITAL
        call getrngr('UTBETA          ',UTBETA,1,1,1)           !BETA FOR REGULATED COST OF CAPITAL
        !READ CAPACITY-DEPENDENT INTEREST RATE AND DEBT FRACTION ADJUSTMENTS BY PLANT TYPE
        call getrngr('UPNIRTEA        ',UPNIRTEA,ECP_D_CAP,1,1)   !ADJ. TO COST OF DEBT
        call getrngr('UPNRRTEA        ',UPNRRTEA,ECP_D_CAP,1,1)   !ADJ. TO COST OF EQUITY
        call getrngr('UPNFDTA         ',UPNFDTA,ECP_D_CAP,1,1)   !ADJ. TO DEBT FRACTION
        call getrngr('UPRSKSW         ',UPRSKSW,ECP_D_CAP,1,1)   !SEITCH FOR GLOBAL RISK PREMIUM
        !READ TIME-DEPENDENT INTEREST RATE AND DEBT FRACTION ADJUSTMENTS BY PLANT TYPE, IF ANY
        call getrngr('UPNIRTEY        ',UPNIRTEY,ECP_D_CAP,1,1)   !ADJ. TO COST OF DEBT - AMOUNT
        call getrngi4('UPNIRTES        ',UPNIRTES,ECP_D_CAP,1,1)   !ADJ. TO COST OF DEBT - 1ST ONLINE YEAR
        call getrngi4('UPNIRTEL        ',UPNIRTEL,ECP_D_CAP,1,1)   !ADJ. TO COST OF DEBT - LAST ONLINE YEAR
        call getrngr('UPNRRTEY        ',UPNRRTEY,ECP_D_CAP,1,1)   !ADJ. TO COST OF EQUITY - AMOUNT
        call getrngi4('UPNRRTES        ',UPNRRTES,ECP_D_CAP,1,1)   !ADJ. TO COST OF EQUITY - 1ST ONLINE YEAR
        call getrngi4('UPNRRTEL        ',UPNRRTEL,ECP_D_CAP,1,1)   !ADJ. TO COST OF EQUITY - LAST ONLINE YEAR
        call getrngr('UPNFDTY         ',UPNFDTY,ECP_D_CAP,1,1)   !ADJ. TO FRACTION OF DEBT - AMOUNT
        call getrngi4('UPNFDTS         ',UPNFDTS,ECP_D_CAP,1,1)    !ADJ. TO FRACTION OF DEBT - 1ST ONLINE YEAR
        call getrngi4('UPFNDTL         ',UPNFDTL,ECP_D_CAP,1,1)    !ADJ. TO FRACTION OF DEBT - LAST ONLINE YEAR

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !READ RES AND CES DATA                                                                             !
        !RESCES SHEET, WRITTEN BY CM1 4/23/13                                                              !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         sheet='RESCES'
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
         wkunit = FILE_MGR('O','ECPDATY',.false.)
         call readRngXLSX(wkunit,trim(sheet))
        !INDEX TO INIDCATE RENEWABLE PORTFOLIO CONSTRAINT BILL, IF ANY
         UPRNWCAS=UPRNWCASR(MNUMNR)
         UPRNWRPS=UPRNWRPSR(MNUMNR)
         UPRNWCAP=UPRNWCAPR(MNUMNR)
         UPRNWCOG=UPRNWCOGR(MNUMNR)
 !    Write(*,*)'RGN ',(uprnwRGN(ecp),ecp = 1 , ijumpemrgn)
 !    Write(*,*)'COG ',(uprnwCOGR(ecp),ecp = 1 , ijumpemrgn),uprNWCOG
 !    Write(*,*)'CAP ',(uprnwCAPR(ecp),ecp = 1 , ijumpemrgn),uprnwCAP
         IF (UNRGNS .EQ. 25) THEN
            UPRNWRGN(8) = 9
         END IF
         call getrngi4('UPRNWREG        ',UPRNWREG,1,1,1)         !RPS TYPE -- NATIONAL/REGIONAL
!     print *,'!cogsw',(uprnwcogr(ecp),ecp = 1 , ijumpemrgn),uprnwcog
         call getrngi4('UPRNWIMP        ',UPRNWIMP,1,1,1)         !SWITCH TO INCLUDE IMPORTS IN RENEWABLE PORTFOLIO CONSTRAINT
         call getrngi4('UPRNWLEV        ',UPRNWLEV,1,1,1)         !LEVELIZED PRICE SWITCH TO USE SINGLE-YEAR OR LEVELIZED CREDIT PRICES FROM ECP
         !READ SWITCH FOR CREDIT PRICES (0=NO RPS, 1=ROLLING AVG, 3=CURRENT YEAR)
         call getrngi4('UPRNWCRD        ',UPRNWCRD,1,1,1)         !CREDIT PRICE SWITCH
         call getrngr('UPRNWCGR        ',UPRNWCGR,1,1,1)         !CREDIT PRICE CAP ANNUAL GROWTH RATE
         !FRACTIONS OF GENERATION EXCLUDED FROM THE BASELINE (NUM/DEM)
         UPRNWBAS(:)=UPRNWBASR(:,MNUMNR)

         !FRACTION OF EXISTING GENERATION EXCLUDED IN RENEWABLE PORTFOLIO CONSTRAINT
         UPRNWEXG(:)=UPRNWEXGR(:,MNUMNR)

         UPRNWSHR(:)=UPRNWSHRR(:,MNUMNR)

!        IF NO REGIONAL RPS, USE NATIONAL FRACTION
!        DO ECP = 1 , ECP_D_CAP
!         DO I = 1 , UNRGNS                 
!           IF (UPRNWCASR(I) .LE. 0)UPRNWSHRR(ECP,I) = UPRNWSHR(ECP)
!         END DO
!        END DO

!        READ LOWER LIMIT FOR RENEWABLE GENERATION IN PORTFOLIO CONSTRAINT
         UPRNWBND(:)=UPRNWBNDR(:,MNUMNR)
          
         call getrngr('EPRPSTGT        ',EPRPSTGT(UISTYR),MNUMYR-UISTYR +1,1,1)!UNADJUSTED MINIMUM SHARE OF GENERATION - NATIONAL
 
         
!        IF THIS IS A CES CASE SPECIFY THAT PLANTS WITH CO2 CAPTURE MUST STORE CAPTURED CARBON

         IF (UPRNWCAS .GT. 0 .AND. (UPRNWSHR(WIIS) .GT. 0.0 .OR. UPRNWSHR(WIPQ) .GT. 0.0 ) ) THEN
            DO XYR = 1 , UNYEAR
               IF (UPRNWBND(XYR) .GT. 0.0) THEN
                  DO XFR = 1 , UNFRGN
                     MUST_STORE(XFR,XYR) = 1
                  END DO
               END IF
            END DO
         END IF


!        IF NO LOWER LIMIT (I.E. UPRNWBND=0), SET TO 0.0001 TO ALLOW FOR
!        LATER REVISIONS IN THE ECP
!
         DO ECP = MNUMYR + 1 , MNUMYR + UNXPH
            UPRNWBND(ECP) = UPRNWBND(MNUMYR)
          DO I = 1 , UNRGNS
            UPRNWBNDR(ECP,I) = UPRNWBNDR(MNUMYR,I)
          END DO
         END DO
!
         DO ECP = 1 , MNUMYR + UNXPH
            UPRNWBND(ECP) = MAX(UPRNWBND(ECP),0.0001)
         END DO
!
!        READ YEAR FOR RPS SUNSET, IF ANY
         UPRNWSUN=UPRNWSUNR(MNUMNR)

!        READ YEARS TO KEEP RPS CREDIT BANKS, IF ANY (0=NONE, 99=FOREVER)
         UPRNWBNK=UPRNWBNKR(MNUMNR)

!        READ DISCOUNT RATE AND SMOOTHING FACTOR FOR RPS/CES CREDIT PRICE PATH
         call getrngr('CES_DCR         ',CES_DCR,1,1,1)             !CES REAL DISCOUNT RATE
         call getrngr('CES_SMOOTH_TGT  ',CES_SMOOTH_TGT,1,1,1)      !CES SMOOTHING FACTOR FOR CES PRICE TARGET
         call getrngr('CES_SMOOTH_DUAL ',CES_SMOOTH_DUAL,1,1,1)     !CES SMOOTHING FACTOR FOR CES PRICE DUALS

!        READ INITIAL CES/RPS DUALS IF NONE IN RESTART FILE
         call getrngr('TCES_DUALS      ',TEMCES_DUALS,MNUMYR-UISTYR+1,UNXPH,1) !INITIAL CES DUALS IF NONE IN RESTART FILE

         DO YR=1,MNUMYR-UISTYR+1
            DO RG=1,UNXPH
                TCES_DUALS(YR+UISTYR-1,RG)=TEMCES_DUALS(YR,RG)
            ENDDO
         ENDDO
         DO I = UISTYR , MNUMYR
            DO ECP = 1 , UNXPH
               IF (CES_DUALS(I,ECP) .LE. 0.001)CES_DUALS(I,ECP) = TCES_DUALS(I,ECP)
            END DO
         END DO
!        Positions 1, 2 and 3 for year_index = 1 (1990) are used to control the CES/RPS Banking Algorithm
!        Position 2 is the first target price (1987$s), Position 1 is a lower limit for the Target Price and Position 3 is the upper limit

         IF (CURIRUN .EQ. 1 .AND. TCES_DUALS(UISTYR,3) .GT. 0.0) THEN
            CES_DUALS(1,1) = TCES_DUALS(UISTYR,1)  ! Should both arrays have UISTYR?  DSA, re: ECPDAT.TXT conversion to excel
            CES_DUALS(1,2) = TCES_DUALS(UISTYR,2)  ! Should both arrays have UISTYR?
            CES_DUALS(1,3) = TCES_DUALS(UISTYR,3)  ! Should both arrays have UISTYR?
         END IF

!        READ INDEX TO INDICATE CAPACITY PORTFOLIO STANDARD, IF ANY
         call getrngi4('UPCAPCAS        ',UPCAPCAS,1,1,1)        !INDEX TO INDICATE CAPACITY PORTFOLIO STANDARD IF ANY

!        READ FRACTION OF CAPACITY INCLUDED IN CAPACITY PORTFOLIO CONSTRAINT
         call getrngr('UPCAPSHR        ',UPCAPSHR,ECP_D_CAP,1,1)!FRACTION OF CAPACITY IN CPS BND

!        READ LOWER LIMIT FOR CAPACITY LEVEL IN PORTFOLIO CONSTRAINT
         call getrngr('UPCAPBND        ',UPCAPBND(UISTYR),MNUMYR-UISTYR+1,1,1)!MIN SHARE OF CAPACITY

!
!        IF NO LOWER LIMIT (I.E. UPCAPBND=0), SET TO 0.0001 TO ALLOW FOR
!        LATER REVISIONS IN THE ECP
!
         DO ECP = MNUMYR + 1 , MNUMYR + UNXPH
            UPCAPBND(ECP) = UPCAPBND(MNUMYR)
         END DO
!
         DO ECP = 1 , MNUMYR + UNXPH
            UPCAPBND(ECP) = MAX(UPCAPBND(ECP),0.0001)
         END DO

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !READ FUEL TRANSPORTATION DATA                                                                     !
        !FUELTRAN SHEET, WRITTEN BY CM1 4/24/13                                                            !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         sheet='FUELTRAN'
! reopen the file unit for each worksheet to obtain the orginal xlsx filename and prepare for readRngXLSX call
         wkunit = FILE_MGR('O','ECPDATY',.false.)
         call readRngXLSX(wkunit,trim(sheet))
         call getrngi4('UNFRGN          ',UNFRGN,1,1,1)          !NUMBER OF FUEL REGIONS
         write(UF_DBG,*) 'UNFRGN ',UNFRGN

!        READ IN FUEL REGIONS DEFINITIONS
         call getrngc('FLRGCODE        ',FLRGCODE,1,UNFRGN,1)   !FUEL REGION TWO LETTER CODE
         call getrngc('FLRGNAME        ',FLRGNAME,1,UNFRGN,1)   !FUEL REGION LONG NAME

!        READ IN MAP OF FUEL REGION to Census Regions, Coal Regions, Natural Gas, and Carbon Regions
         call getrngi4('FUEL_RGN_MAP    ',FUEL_RGN_MAP,4,23,1)   !FUEL REGION LONG NAME

!        Create Mapping from Fuel Region to Census, Coal and Gas Regions

         DO IRG = 1 , UNFRGN
            EPCSMP(IRG) = FUEL_RGN_MAP(1,IRG)
            EPCLMP(IRG) = FUEL_RGN_MAP(2,IRG)
            EPGSMP(IRG) = FUEL_RGN_MAP(3,IRG)
            EPCAMP(IRG) = FUEL_RGN_MAP(4,IRG)
!           EPNFLRG(FUEL_RGN_MAP(1,IRG), FUEL_RGN_MAP(2,IRG), FUEL_RGN_MAP(3,IRG)) = IRG
            CPFLRG(FUEL_RGN_MAP(1,IRG), FUEL_RGN_MAP(2,IRG), FUEL_RGN_MAP(3,IRG)) = CPFLRG(FUEL_RGN_MAP(1,IRG), FUEL_RGN_MAP(2,IRG), FUEL_RGN_MAP(3,IRG)) + 1.0
            EPNFLRG(FUEL_RGN_MAP(1,IRG), FUEL_RGN_MAP(2,IRG), FUEL_RGN_MAP(3,IRG), FUEL_RGN_MAP(4,IRG)) = IRG
!           CPFLRG(FUEL_RGN_MAP(1,IRG), FUEL_RGN_MAP(2,IRG), FUEL_RGN_MAP(3,IRG), FUEL_RGN_MAP(4,IRG)) =  &
!             CPFLRG(FUEL_RGN_MAP(1,IRG), FUEL_RGN_MAP(2,IRG), FUEL_RGN_MAP(3,IRG), FUEL_RGN_MAP(4,IRG)) + 1.0
            IF (IRG .LE. 10) THEN
               EPFLCD(IRG) = NUMCH(IRG)
            ELSEIF (IRG .LE. 36) THEN
               EPFLCD(IRG) = CHAR(54+IRG)
            ELSE
               EPFLCD(IRG) = CHAR(60+IRG)
            ENDIF
!           write(UF_DBG,9993) CURIYR+UHBSYR,CURITR,EPCSMP(IRG),EPCLMP(IRG),EPGSMP(IRG),IRG,EPNFLRG(FUEL_RGN_MAP(1,IRG),FUEL_RGN_MAP(2,IRG),FUEL_RGN_MAP(3,IRG)), &
            write(UF_DBG,9993) CURIYR+UHBSYR,CURITR,EPCSMP(IRG),EPCLMP(IRG),EPGSMP(IRG),IRG,EPNFLRG(FUEL_RGN_MAP(1,IRG),FUEL_RGN_MAP(2,IRG),FUEL_RGN_MAP(3,IRG),FUEL_RGN_MAP(4,IRG)), &
               EPFLCD(IRG),CPFLRG(FUEL_RGN_MAP(1,IRG),FUEL_RGN_MAP(2,IRG),FUEL_RGN_MAP(3,IRG))
 9993       format(1x,"EPNFLRG",7(":",I4),":",A1,":",F12.3)

!           Determine if fuel region belongs to an active carbon region

            DO XYR = 1 , UNYEAR
               IF (EMRFCA(EPCAMP(IRG),XYR) .GT. 0.0) MUST_STORE(IRG,XYR) = 1
            END DO

!           Transfer Heatrate Adjustments Info from coal regions to fuel regions

            DO IECP = 1 , ECP_D_CAP
               FLRG_HR_KNOTS(IRG,IECP) = HR_KNOTS(EPCLMP(IRG),IECP)
               DO KNOTS = 1 , MAX_KNOTS
                  FLRG_HR_EFF(IRG,IECP,KNOTS) = HR_EFF(EPCLMP(IRG),IECP,KNOTS)
                  FLRG_HR_LL(IRG,IECP,KNOTS) = HR_LL(EPCLMP(IRG),IECP,KNOTS)
               END DO
            END DO

         END DO
!
!        READ IN Transmission Cost By EMM and CDS Demand Region
!
  
         FRG_EMM_MAP = 0.0
         
        DO I=1,IJUMPEMRGN
          Do J=1,UNFRGN
              Write(18,*)'fl_cnxt_cst ',I,J,(FL_CNXT_CST(I,J))
           ENddo
        ENDDO
        
!        Insure that new connections are allowed

         DO FRG = 1 , UNFRGN
            DO IRG = 1 , UNRGNS
               IF (FL_CNXT_CST(IRG,FRG) .GT. 0.0) THEN
                  FRG_EMM_MAP(IRG,FRG) = 1.0
               END IF
            WRITE(UF_DBG,*)'FRG_EMM_MAP ',FRG,IRG,FL_CNXT_CST(IRG,FRG),FRG_EMM_MAP(IRG,FRG)
            END DO
         END DO

  
         UPXRGN = 0
         UPGTRN = 0
         UPCIMP = 0

        DO I=1,IJUMPEMRGN
          Do J=1,IJUMPEMRGN
              Write(18,*)'emm_cnxt_cst ',I,J,(EMM_CNXT_CST(I,J))
           ENddo
        ENDDO

         DO FRG = 1 , UNRGNS
            EMM_CNXT_CST(FRG,FRG) = 0.0
            DO IRG = FRG + 1 , UNRGNS
               EMM_CNXT_CST(FRG,IRG) = MAX(EMM_CNXT_CST(FRG,IRG) , EMM_CNXT_CST(IRG,FRG))
               EMM_CNXT_CST(IRG,FRG) = MAX(EMM_CNXT_CST(FRG,IRG) , EMM_CNXT_CST(IRG,FRG))
               IF (EMM_CNXT_CST(FRG,IRG) .GT. 0.0) THEN
                  UPXRGN(FRG,IRG) = 1
                  UPXRGN(IRG,FRG) = 1
                  UPGTRN(FRG) = 1
                  UPGTRN(IRG) = 1
                  UPCIMP(FRG) = 1
                  UPCIMP(IRG) = 1
               END IF
            END DO
         END DO

        DO I=1,IJUMPEMRGN
          Do J=1,IJUMPEMRGN
              Write(18,*)'emm_cndc_cst ',I,J,(EMM_CNDC_CST(I,J))
           ENddo
        ENDDO
         
         DO FRG = 1, UNRGNS

            DO IRG = FRG+1, UNRGNS
               EMM_CNDC_CST(FRG,IRG) = MAX(EMM_CNDC_CST(FRG,IRG), EMM_CNDC_CST(IRG,FRG))
               EMM_CNDC_CST(IRG,FRG) = MAX(EMM_CNDC_CST(FRG,IRG), EMM_CNDC_CST(IRG,FRG))
               IF (EMM_CNDC_CST(FRG,IRG) .GT. 0.0) THEN
                  !UPXRGN(FRG,IRG) = 1
                  !UPXRGN(IRG,FRG) = 1
                  !UPGTRN(FRG) = 1
                  !UPGTRN(IRG) = 1
                  !UPCIMP(FRG) = 1
                  !UPCIMP(IRG) = 1
               END IF
            END DO             
         END DO  

         EPTCRFPRE=0.
         EPTCRF=0.
         call getrngr('EPTCRF          ',EPTCRFPRE,1,1,1)   !annuity factor for interregional builds
         
         DO I=1,IJUMPEMRGN
           Do J=1,IJUMPEMRGN
              Write(18,*)'eptloss ',I,J,(EPTLOSS(I,J))
            ENddo
          ENDDO    
        
         EPTIRGN = 0
         EPTCST = 0.
         DO FRG=1,UNRGNS
            K=1
            DO IRG=1,UNRGNS
               IF (EMM_CNDC_CST(FRG,IRG) .GT. 0. .AND. EMM_CNDC_CST(FRG,IRG) .LT. 9999.0 .AND. IRG .NE. FRG) THEN 
                  EPTIRGN(FRG,K) = IRG
                  EPTCST(FRG,K) = EMM_CNDC_CST(FRG,IRG)
                  EPTCRF(FRG,K) = EPTCRFPRE
!                  EPTLOSS(FRG,K) = EPTLOSSPRE(FRG,IRG)
                  !write(*,*) 'EMM_CNDC_CST',FRG,IRG,EMM_CNDC_CST(FRG,IRG)
                  !write(*,*) 'EPTIRGN',FRG,K,EPTIRGN(FRG,K)
                  !write(*,*) 'EPTCRF',FRG,K,EPTCRF(FRG,K)
                  !write(*,*) 'EPTLOSS',FRG,K,EPTLOSS(FRG,K)
                  K=K+1                  
               END IF 
            END DO         
         END DO 
         
         
         !DO IRG=1,IJUMPEMRGN
            !write(*,*) 'EPTCRF',IRG,EPTCRF(IRG)
            !write(*,*) 'EPTLOSS',IRG,EPTLOSS(IRG)
         !END DO
         
         call getrngi4('UPETTSW         ',UPETTSW,1,ECP_D_CAP,1) !switch for new interregional builds logic for intermittent
!        DO IECP=1,ECP_D_CAP
!           write(*,*) 'UPETTSW',IECP,UPETTSW(IECP)
!        END DO 
         
         

!        READ IN CTL Census Division / EMM Region Mapping         

!        READ IN CTL Census Division / EMM Region Mapping

        call getrngr('CTL_CD_NR       ',rtemp19,MNUMCR-1,IJUMPEMRGN,1)       !TRANSMISSION COSTS TO CREATE NEW LINKS BETWEEN EMM REGIONS
        CTL_CD_NR(1:mnumcr-1,1:UNRGNS)=RTEMP19(1:mnumcr-1,1:UNRGNS)

!       READ SPINNING RESERVES - SPN RSV1 AND RSV2
        sheet='SPNRSV'
        wkunit = FILE_MGR('O','ECPDATY',.false.)
        call readRngXLSX(wkunit,trim(sheet))
        call getrngr('SR_CREDIT       ',SR_CREDIT,ECP_D_CAP,1,1)    ! Percent of excess capacity commited to load that is credited against
        call getrngr('SR_MIN_CF       ',SR_MIN_CF,ECP_D_CAP,1,1)    ! Minimum Capacity Factor Allowed for Determining Split between
        call getrngr('SR_MIN_LF       ',SR_MIN_LF,ECP_D_CAP,1,1)    ! Derate for min el and max sr mode = (1 - sr_min_lf) * sr_min_cf * (ld /
        call getrngr('SR_MAX_LF       ',SR_MAX_LF,ECP_D_CAP,1,1)    ! Derate for max el and min sr mode = (1 - sr_max_lf) * (1 - for) * (ld /
        call getrngr('SR_TRAN_CREDIT  ',SR_TRAN_CREDIT,1,1,1)    !  PERCENT OF EXCESS TRANSMISSION CAPACITY THAT IS CREDITED AGAINS


!       READ HEATRATE IMPROVEMENT DATA - HRT IMP1 AND HRT IMP2
        sheet='HTRTIMP'
        wkunit = FILE_MGR('O','ECPDATY',.false.)
        call readRngXLSX(wkunit,trim(sheet))
        call getrngr('HTRT_OVR_CST    ',HTRT_OVR_CST,ECP_D_CAP,1,1)    ! Heatrate improvement cost MM$/unit per year
        call getrngr('HTRT_REDUCTION  ',HTRT_REDUCTION,ECP_D_CAP,1,1)  ! Heatrate improvement Percent Reduction per year
        call getrngr('HTRT_FLOOR      ',HTRT_FLOOR,ECP_D_CAP,1,1)      ! Minimum Heatrate allowed after improvement
        call getrngr('HTRT_MIN_CAP    ',HTRT_MIN_CAP,ECP_D_CAP,1,1)    ! Minimum Size unit to which improvement is allowed/required

        call getrngr('HTRT_OVRQ       ',HTRT_OVRQ,ECP_D_CAP,4,1)       ! Heatrate improvement cost MM$/unit per quarter
        call getrngr('HTRT_REDQ       ',HTRT_REDQ,ECP_D_CAP,4,1)       ! Heatrate improvement Percent Reduction per quarter

       call getrngi4('HTRT_YEAR       ',HTRT_YEAR,1,1,1)             !
       call getrngi4('HTRT_DOLLAR_YEAR',HTRT_DOLLAR_YEAR,1,1,1)      !

!       Set HTRT Imp Cost to 0 if Switch to set to 0
        IF (CO2_HRTSW .LE. 0) THEN
           Do Icap=1,ECP_D_CAP
              HTRT_OVR_CST(icap) = 0.0
           Enddo
        END IF

        Do Icap=1,ECP_D_CAP
          Write(UF_DBG,*)'debug HTRT_OVR_CST  ',icap,HTRT_OVR_CST(icap)
          Write(UF_DBG,*)'debug HTRT_REDUCTION',icap,HTRT_REDUCTION(icap)
          Write(UF_DBG,*)'debug HTRT_FLOOR    ',icap,HTRT_FLOOR(icap)
          Write(UF_DBG,*)'debug HTRT_MIN_CAP  ',icap,HTRT_MIN_CAP(icap)
        Enddo
        Write(UF_DBG,*)'debug HTRT_YEAR          ',HTRT_YEAR
        Write(UF_DBG,*)'debug HTRT_DOLLAR_YEAR   ',HTRT_DOLLAR_YEAR
!
!       ADJUST CAPITAL COSTS BY CONSTRUCTION SUBSIDY
!       ADJUST FIXED O&M BY O&M MULTIPLIER
!
        DO ICAP = 1 , ECP_D_CAP
            UPOVR(ICAP) = UPOVR(ICAP) * ( 1.0 - UPCSB(ICAP) )
            UPFOM(ICAP) = UPFOM(ICAP) * ( 1.0 - UPOMPCT )
        ENDDO

      endif ! of (if (nerc.eq.1 ) then
!
!     MAP NATIONAL INITIAL VALUES FOR HEAT RATES, ADD ON TRANSMISSION COST,
!     AND RENEWABLE VARIABLES INTO REGIONAL VALUES.
!
        DO ICAP = 1 , ECP_D_CAP
            EPCTRM(ICAP) = UPCTRM(ICAP)
            EPVOM(ICAP) = UPVOM(ICAP)
            EPFOM(ICAP) = UPFOM(ICAP)
          IF (UCPDSPIS(ICAP) .GT. 0) THEN
            EPPHRT0(ICAP) = UPPHRT0(ICAP)
            EPPHRTN(ICAP) = UPPHRTN(ICAP)
          ELSE IF (UCPINTIS(ICAP) .GT. 0) THEN
            INT = UCPINTIS(ICAP)
            JCAP = UIRINTI(INT)
            EPIROVR(JCAP) = UPOVR(ICAP)
            EPIRFOM(JCAP) = UPFOM(ICAP)
            EPIRVOM(JCAP) = UPVOM(ICAP)
            EPIRCCR(JCAP) = UPCCR(ICAP)
            EPIRCCS(JCAP,:) = UPCCR(ICAP)
          ELSE IF (UCPRNWIS(ICAP) .GT. 0) THEN
            RNW = UCPRNWIS(ICAP)
            JCAP = UIRRNWI(RNW)
            EPIROVR(JCAP) = UPOVR(ICAP)
            EPIRFOM(JCAP) = UPFOM(ICAP)
            EPIRVOM(JCAP) = UPVOM(ICAP)
            EPIRCCR(JCAP) = UPCCR(ICAP)
            EPIRCCS(JCAP,:) = UPCCR(ICAP)
            EPRCFC(JCAP) = UPMCF(ICAP)
          END IF
        END DO







      ! READING IN THE REGIONAL FILES FROM THE REGIONAL WORKSHEETS
!     regstr=' ' ! character*2 regstr,rangestr*16
!     write(regstr,'(i2.2)') NERC
!     sheet='NERC'//regstr
!     wkunit = FILE_MGR('O','ECPDATY',.false.)
!     call readRngXLSX(wkunit,trim(sheet))
    
 !   SET REGIONAL ELEVATION COST MULTIPLIERS TO 1.0 IF MULTIPLIER SWITCH IS SET TO 0
       IF (ECP_D_AMB .EQ. 0) THEN
            DO ICAP = 1, ECP_D_CAP
                EPACM(ICAP) = 1.0
            ENDDO
       ENDIF
       DO ICAP=1, ECP_D_CAP
            EPCTRM(ICAP) = 0.0
       ENDDO

     call ECPDAT_REG_ECPT2(NERC)

       TRCTLOVR(NERC)=EPCOVR(WIIG)
       DO ICAP=1,ECP_D_CAP
         WRite(18,*)'check EPCOVR ',ICAP,EPCOVR(ICAP)
       ENDDO
       !
       TRCTLFCF(NERC)=EPCCRF(WIIG)

!     SET REGIONAL ELEVATION COST MULTIPLIERS TO 1.0 IF MULTIPLIER SWITCH IS SET TO 0
!
       DO ICAP=1,ECP_D_CAP
         WRite(18,*)'check EPacm ',ICAP,EPacm(ICAP)
       ENDDO


!  STORE IGCC TRANMSISSION VALUE FOR PMM/CTL BUILDS
       TRCTLOVR(NERC) = EPCOVR(WIIG)
       TRCTLFCF(NERC) = EPCCRF(WIIG)

       DO ICAP=1,23
         WRite(18,*)'check EPVOCRT ',ICAP,EPVOCRT(ICAP)
       ENDDO

       CALL ECPDAT_REG2(NERC)
       
       EPMRMIN(NERC) = EPMRM
       write(18,*)'EPMRM =',EPMRM
      

!     READ IN DATA TO ESTABLISH INTERREGIONAL LINKS FOR NEW BUILDS
!     (IE. ID OF IMPORT REGION(S), TRANSMISSION COST AND PERFORMANCE DAT
!
!!!     
!      DO IMPORT = 1 , MNUMNR
!         EPTIRGN(IMPORT) = 0
!         EPTCST(IMPORT) = 0.0
!         EPTCRF(IMPORT) = 0.0
!         EPTLOSS(IMPORT) = 0.0
!!!
!      END DO
! IRG was 0 in all regions of ecpdat.txt and there was no transmission data in the file, so this
! code was not converged to the XML format.
!       rngnam='IRG_'//regstr
!      CALL GETRNGI4(rngnam,IRG,1,1,1)      !NUMBER OF ELE IMPORT REGIONS
!      IF (IRG .GT. 0) THEN
!        UPGTRN(NERC) = 1
!        RET_CODE = RD$C1(DUMMY,1,DMSZ)
!        RET_CODE = RD$I1(EPTIRGN,1,MNUMNR)         ! # TRANS IRG BUILDS
!        RET_CODE = RD$R1(EPTCST,1,MNUMNR)     ! TRANS COST FOR IRG BLDS
!        RET_CODE = RD$R1(EPTCRF,1,MNUMNR)  ! TRANS ANN FAC FOR IRG BLDS
!        RET_CODE = RD$R1(EPTLOSS,1,MNUMNR)    ! TRANS LOSS FOR IRG BLDS
!        DO IMPORT = 1 , IRG          ! CWC LOGIC ADDED TO AVOID 0 INDEX
!           UPCIMP(EPTIRGN(IMPORT)) = 1
!           IF (EPTIRGN(IMPORT) .GT. 0) UPXRGN(NERC,EPTIRGN(IMPORT)) = 1
!        END DO
!     END IF
!

!        READ IN NATURAL GAS AND COAL REGION FOR NEW BUILDS

       UPNGRG(NERC)=EPNGRG

       UPCLRG(NERC)=EPCLRG
        write(18,*)"EPCLRG =",EPCLRG  

       UPCARG(NERC)=EPCARG
       write(18,*)"EPCARG =",EPCARG


!      READ IN CENSUS REGION FOR NEW BUILDS (CAN DIFFER BY PLANT/ FUEL)
       UPCENSUS(:,NERC)=EPCENSUS(:)
       DO I=1,ECP_D_CAP
          Write(18,*) 'check epcensus ',NERC,EPCENSUS(I)
       ENDDO

!   SUM UP EXTERNALITY COSTS
!
      DO ECP = 1 , ECP_D_CAP
         EPEXT(ECP,MNPOLLUT + 1) = 0.0
         DO POL = 1 , MNPOLLUT
            EPEXT(ECP,MNPOLLUT + 1) = EPEXT(ECP,MNPOLLUT + 1) +  EPEXT(ECP,POL)
         END DO
      END DO

      DO ECP=1,ECP_D_CAP
        DO POL=1,MNPOLLUT
           WRITE(18,*)'check epext ',NERC,ECP,POL,EPEXT(ECP,POL)
        ENDDO
      ENDDO

      CALL ECPDAT_REG_ECPT_S(NERC)
       !
       !READ IN DISTRIBUTED GENERATION STEP SIZES AND AVOIDED COSTS
       
      DO ECP=1,ECP_D_DGS
           WRITE(18,*)'check EPDAVD ',NERC,ECP,EPDAVD(ECP)          
      ENDDO


! On last time through, close input and debug files
       if(nerc.eq.unrgns) then
         wkunit = FILE_MGR('C','ECPDATY',NEW)
         XMLOUT = FILE_MGR('C','ECPXMLOUT',NEW)
         XMLOUT=0
       endif
      RETURN

      end
!
!
      SUBROUTINE RDINTLRN
!
      IMPLICIT NONE

!     THIS SUBROUTINE READS IN RENEWABLE INTERNATIONAL LEARNING DATA

      include 'parametr'
      include 'ncntrl'
      include'emmparm'
      include 'control'
      include'entcntl'
      include'ecpcntl'
      include 'bildin'
!
      INTEGER*4 IREC
      PARAMETER (IREC = 500)
      INTEGER*4 NREC,OYEAR(IREC),IY,IP,IR,OYR
      REAL*4 CAP(IREC)
      CHARACTER*40 FILENM
      CHARACTER*2 PLTCODE(IREC)
      LOGICAL NEW
!
!     USING RD$* FUNCTIONS
!
      INTEGER*4    RD$TBL,RD$I1,RD$R1,RD$R81,RD$C1,RET_CODE
      INTEGER*4    RD$I2,RD$R2,RD$R82,RD$C2,RD$I3,RD$R3
      INTEGER*4    COLLB,NCOL,CPT,L_COLLB
      INTEGER*4    DMSZ
      PARAMETER (DMSZ = 300)
      CHARACTER*40 DUMMY(DMSZ)             ! DUMMY COLUMN IN DATA TABLES
!
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
!
      INTEGER*4 CHK$RSUM,CHK$PRF
!
!
         WRITE (6,*) '*** BEGINNING READ FOR INTERNATIONAL LEARNING ***'
         WRITE (UF_MSG,*) &
               '*** BEGINNING READ FOR INTERNATIONAL LEARNING ***'
!
!        OPEN FILE
!
         FILENM = 'EINTLRN'
         NEW = .FALSE.
         UF_TMP = FILE_MGR('O',FILENM,NEW)
!
!        READ IN NUMBER OF RECORDS IN INTERNATIONAL LEARNING TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%NREC INTLN%',1,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$I1(NREC,1,1)

         IF (NREC .GT. IREC) THEN
           WRITE(6,*)  ' **************  WARNING *************  '
           WRITE(6,*)  '*** NUMBER OF RECORDS IN INT LRNING TABLE ***'
           WRITE(6,*)  '** GREATER THAN READ ARRAYS - RE-SIZE ARRAYS **'
           STOP
         END IF
!
!        READ INTERNATIONAL LEARNING DATA
!
         RET_CODE = RD$TBL(UF_TMP,'%INTLRN    %',NREC,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(PLTCODE,1,NREC)                     ! PLANT CODE
         RET_CODE = RD$I1(OYEAR,1,NREC)                     ! ON LINE YEAR
         RET_CODE = RD$R1(CAP,1,NREC)                      ! CAPACITY (MW)
!
!        READ SHARE OF INTERNATIONAL CAPACITY TO APPLY TO LEARNING
!
         RET_CODE = RD$TBL(UF_TMP,'%INTLRN_SHARE',ECP_D_CAP,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$R1(UPISHR,1,ECP_D_CAP)                 ! LEARNING SHARE
!

         DO IY = 1, UNYEAR
           DO IP = 1 , ECP_D_CAP
              UPLRLCI(IP,IY) = 0.0
           END DO
         END DO
!
         DO IR = 1 , NREC
            OYR = OYEAR(IR) - 1989
            DO IP = 1 , ECP_D_CAP
              IF (PLTCODE(IR) .EQ. UPLNTCD(IP)) THEN
                DO IY = 1 , UNYEAR
                  IF (OYR .LE. IY) THEN
                     UPLRLCI(IP,IY) = UPLRLCI(IP,IY) + CAP(IR)
                  ENDIF
                END DO
              END IF
            END DO
         END DO
!
      CLOSE (UF_TMP)
!
      RETURN
      END
!
!
      SUBROUTINE ESCRSRT(N,RA,RB)
!
      IMPLICIT NONE

!     THIS SUBROUTINES PERFORMS A HEAP SORT
!     Heapsort - Sorts an array RA of length N into descending numerical order using the
!     Heapsort Algorithm.  N is input; RA is replaced on output by its sorted rearrangement
!     and RB is rearranged correspondingly.

      include'emmparm'

      REAL*4 RA(ECP_D_SGP),RRA
      INTEGER*4 RB(ECP_D_SGP),RRB
      INTEGER N,L,IR,I,J
!
      L = N / 2 + 1
      IR = N
!
!     The index L will be decremented from its initial value down to 1 durin
!     the "hiring" (heap creation) phase. Once it reaches 1, the index IR
!     will be decremented from its initial value down to 1 during the
!     "retirement-and-promotion" (heap selection) phase.
!
   10 CONTINUE
      IF (L .GT. 1) THEN                       ! Still in hiring phase ?
         L = L - 1
         RRA = RA(L)
         RRB = RB(L)
      ELSE                           !  In retirement-and-promotion phase.
         RRA = RA(IR)                  !  Clear a space at end of array.
         RRB = RB(IR)
         RA(IR) = RA(1)              !  Retire the top of the heap into it.
         RB(IR) = RB(1)
         IR = IR - 1                 !  Decrease the size of the corporation
         IF (IR .EQ. 1) THEN            !  Done with the last promotion.
            RA(1) = RRA              !  The least competent worker of all !
            RB(1) = RRB
            RETURN
         ENDIF
      ENDIF
!
!     Whether we are in the hiring phase or promotion phase, we here set
!     up to sift down element RRA to its proper level.
!
      I = L
      J = L + L
   20 IF (J .LE. IR) THEN                             ! Do while J <= IR
         IF (J .LT. IR) THEN
            IF (RA(J) .LT. RA(J + 1)) J = J + 1   ! Comp to the better under
            ENDIF
         IF (RRA .LT. RA(J)) THEN                           ! Demote RRA
            RA(I) = RA(J)
            RB(I) = RB(J)
            I = J
            J = J + J
         ELSE         ! This is RRA's level. Set J to terminate the sift-dow
            J = IR + 1
         END IF
         GO TO 20
      ENDIF
      RA(I) = RRA                                  ! Put RRA into its slot.
      RB(I) = RRB
      GO TO 10
      END
!
!
!
      SUBROUTINE ELDMND(JYR)
!
      IMPLICIT NONE

!     THIS SUBROUTINE INITIALIZES NERC REGION DEMANDS

      include'parametr'
      include'ncntrl'
      include'apq'
      include'xpq'
      include'emmparm'
      include'ecpcntl'
      include'control'
      include'elshrs'
!
      INTEGER JYR,ICENSUS,IYR
      REAL*8 FACTOR,GRWINDX
      IF(USW_XP .EQ. 0) THEN
         FACTOR = 0.003412
      ELSE                                                       !CANADA
         FACTOR = 1
      ENDIF
!
!     CONVERT CENSUS REGION DEMANDS TO NERC REGION DEMANDS
!
      DO 30 ICENSUS = 1 , MNUMCR
         EFD$CENSUS(ICENSUS) = QELAS(ICENSUS,JYR) / FACTOR
!        IF (UF_DBG.GT.0)
!        +   WRITE(UF_DBG,1432)'EFD$CENSUS',EFD$CENSUS(ICENSUS)
!1432    FORMAT(2(A10,1X,F10.2))
   30 CONTINUE
!
      IF (UF_DBG .GT. 0) THEN
         WRITE(UF_DBG,334)'YEAR',JYR,'CURITR',CURITR
334      FORMAT(2(A8,1X,I4,1X))
      END IF
      CALL CNV$CRNR(EFD$CENSUS,ELGNNRCR,EFD$NERC)
!
!     IF(UF_DBG.GT.0) WRITE(UF_DBG,1444)'EFD$NERC',EFD$NERC
!1444    FORMAT(A8,1X,F10.2)

      DO 200 IYR = 1 , UNFPH
         DO 130 ICENSUS = 1 , MNUMCR
            IF ( XQELAS(ICENSUS,JYR) .GT. 0.001) THEN
               GRWINDX = XQELAS(ICENSUS,JYR + IYR - 1) / XQELAS(ICENSUS,JYR)
               GRWINDX = MIN ( GRWINDX , DBLE(1.015 ** (IYR - 1)))
            ELSE
               GRWINDX = 1.0
            END IF
            ECP$CENSUS(ICENSUS,IYR) = QELAS(ICENSUS,JYR) * GRWINDX / FACTOR
  130    CONTINUE
!
         CALL CNV$CRNR(ECP$CENSUS(1,IYR),ELGNNRCR,ECP$NERC(1,IYR))
!
  200 CONTINUE
!
      RETURN
      END
!
!
!
      SUBROUTINE CNV$CRNR(CENSUS,SHARES,NERC)
!
      IMPLICIT NONE

!     THIS SUBROUTINE CONVERTS DATA FROM CENSUS DIVISIONS TO EMM (NERC-BASED) REGIONS

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
!
      INTEGER ICENSUS,INERC
      REAL NERC(MNUMNR),CENSUS(MNUMCR)
      REAL*8 SHARES(MNUMNR,MNUMCR)
      REAL*8 TCENSUS,TNERC,TSHR,TTOT
!
!     CONVERT FROM CENSUS TO NERC DATA
!
      DO 10 INERC = 1 , MNUMNR
         NERC(INERC) = 0.0
         DO 20 ICENSUS = 1 , MNUMCR - 1
            TCENSUS = CENSUS(ICENSUS)
            TNERC = NERC(INERC)
            TSHR = SHARES(INERC,ICENSUS)
            TTOT = SHARES(MNUMNR,ICENSUS)
            IF ( SHARES(MNUMNR,ICENSUS) .GT. 0.000001 ) THEN
               NERC(INERC) = NERC(INERC) + CENSUS(ICENSUS) * &
                (SHARES(INERC,ICENSUS) / SHARES(MNUMNR,ICENSUS))
            END IF
!           IF ((INERC.EQ.1).AND.(UF_DBG.GT.0)) THEN
!           WRITE(UF_DBG,1343)' CENSUS',TCENSUS,'NERC',TNERC,'SHARES',TSHR,
!           +   'SHAREST',TTOT
!1343   FORMAT(4(A8,1X,F10.2))
!           write(uf_dbg,242)'inerc',inerc,'nerc',nerc(inerc)
!242      format(a8,1x,i4,1x,a8,1x,f11.1)
!           ENDIF
   20    CONTINUE
   10 CONTINUE
!
      RETURN
      END
!
!
!
      SUBROUTINE ELDECP(IRG)
!
      IMPLICIT NONE

!     THIS SUBROUTINE FILLS ECP LOAD REPRESENTATION FROM LDSM DATA

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'bildin'
      include'dispin'
      include'udatout'
      include'dsmdimen'
      include'dsmtfecp'
      include'dsmtoefd'
      include'dsmtfefp'
      include'dsmnemsc'
!
      REAL*8 PVV,PV_KW,PV_GW,KW(ECP_D_FPH),GW(ECP_D_FPH),DSCRT,GRW
      INTEGER I,IGRP,ISEG,JYR,IYR,IRG,ISP,KYR,LYR
!
!     ESTABLISH NUMBER OF ECP SEASONS AND SEASON/TIME-OF-DAY GROUPS (PATCH)
!
      EPNMSP = ECPnS
      EPNGRP = ECPnumSg
!
!     ESTABLISH GROUP TO SEASON (TEMPORARY PATCH)
!
!        assign ECP group to ECP season directly from ldsm structure file data
!
      DO I = 1,ECPnumSg
         DO ISP = 1, ECPnS
            IF (I.GE.ECPSEDEF(ISP,1) .AND. I.LE.ECPSEDEF(ISP,2))  THEN
               EPGECP(I) = ISP
            ENDIF

!           WRITE(6,5413) CURIRUN, CURIYR+1989, I, ISP, ECPSEDEF(ISP,1), ECPSEDEF(ISP,2)
!5413       FORMAT(1X,"EPGECP_INFO",6(":",I6))

         ENDDO
      ENDDO
!
!     SET REAL*8 VARIABLE TO DISCOUNT RATE
!
      DSCRT = EPDSCRT
!
!     LOOP ON YEARS AND VERTICAL SLICES
!
      DO IYR = 1 , UNXPH
         IF (IYR .EQ. UNXPH) THEN
            DO KYR = 1 , UNFPH - UNXPH + 1
               KW(KYR) = 1.0
            END DO
            PV_KW = PVV(KW,ECP_D_FPH,(UNFPH-UNXPH+1),DSCRT)
         END IF
         EPPEAK(IYR) = DBLE(0.0)
         DO ISP = 1 , ECP_D_MSP
            IF (IYR .LT. UNXPH) THEN
               EPSPK(ISP,IYR) = UPPEAK(ISP,IYR,IRG)
            ELSE
               DO KYR = 1 , UNFPH - UNXPH + 1
                  GW(KYR) = UPPEAK(ISP,UNXPH + KYR - 1,IRG)
               END DO
               PV_GW = PVV(GW,ECP_D_FPH,(UNFPH-UNXPH+1),DSCRT)
               EPSPK(ISP,IYR) = PV_GW / PV_KW
            END IF
            EPFRST(ISP,IYR) = 0
            EPPEAK(IYR) = MAX( EPPEAK(IYR) , EPSPK(ISP,IYR) )
         END DO
!
         EPNSTP(IYR) = ECPNUMBL
         DO I = 1 , EPNSTP(IYR)
            EPNEXT(I,IYR) = 0
         END DO
         DO I = 1, ELD_D_DAY
            EPNSPG(I) = 0
         END DO
!
         JYR = CURIYR + IYR - 1
         DO I = EPNSTP(IYR) , 1 , - 1
            IF (IYR .LT. UNXPH) THEN
               EPHGHT(I,IYR) = ECPLDCBH(JYR,IRG,I)
            ELSE
               DO KYR = 1 , UNFPH - UNXPH + 1
                  LYR = CURIYR + UNXPH + KYR - 2
!               IF ((CURIYR + UHBSYR) .EQ. 2010 .OR. (CURIYR + UHBSYR) .EQ. 2011)THEN
                IF ((CURIYR + UHBSYR) .EQ. 2011)THEN
                  GRW = (QELASN(IRG,UNYEAR) / QELASN(IRG,CURIYR)) ** (1.0 / FLOAT(UNYEAR - CURIYR))
                  GRW = GRW ** FLOAT(LYR - CURIYR)
                  GW(KYR) = ECPLDCBH(CURIYR,IRG,I) * GRW
                ELSE
                  GW(KYR) = ECPLDCBH(LYR,IRG,I)
                END IF
               END DO
               PV_GW = PVV(GW,ECP_D_FPH,(UNFPH-UNXPH+1),DSCRT)
               EPHGHT(I,IYR) = PV_GW / PV_KW
            END IF
            EPWDTH(I,IYR) = ECPLDCBW(JYR,IRG,I) * DBLE(8760.0)
            IGRP = ECPLDCBS(JYR,IRG,I)
            ISP = EPGECP(IGRP)
            EPLDGR(I,IYR) = IGRP
            EPNEXT(I,IYR) = EPFRST(ISP,IYR)
            EPFRST(ISP,IYR) = I

            IF (CURIYR+1989 .EQ. 2021) WRITE(18,7733) CURIRUN, CURIYR+1989, JYR+1989, IYR, IRG, ISP, IGRP, I, EPHGHT(I,IYR), EPWDTH(I,IYR), ECPLDCBH(JYR,IRG,I), QELASN(IRG,UNYEAR), QELASN(IRG,CURIYR)
 7733       FORMAT(1X,"EPHGHT_UECP",8(":",I5),5(":",F21.6))
         END DO
         DO I = 1 , EPNSTP(IYR)
            IGRP = EPLDGR(I,IYR)
            EPNSPG(IGRP) = EPNSPG(IGRP) + 1
            ISEG = EPNSPG(IGRP)
            EPLDSG(I,IYR) = EPNSPG(IGRP)
            EPLMAP(IGRP,ISEG,IYR) = I
            EPGECPS(I,IYR) = EPGECP(IGRP)
         END DO
      END DO
!
!     ESTABLISH SEASONAL DIMENSION FOR EFD
!
      EENSP = EFDNS
!
      RETURN
      END
!
!
      SUBROUTINE ELDEFD(IYR,ITR,IRG)
!
      IMPLICIT NONE

!     SUBROUTINE ELDEFD FILLS EFD LOAD REPRESENTATION FROM LDSM DATA

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispcrv'
      include'dispin'
      include'dispett'
      include'dsmdimen'
      include'dsmtoefd'
!
      INTEGER I,J,IRG,IGRP,ISEG,IYR,ITR
      REAL*8 SEASON,ANNUAL
      REAL EFDNERC2

!
      IF (IRG .EQ. 1) THEN
         UTNGRP = 0
         UTNSEG = 0
      END IF
!
      ANNUAL = DBLE(0.0)
      EENSP = EFDNS

      DO I = 1 , EENSP
         ELNVCT(I) = ULNVCT(I)
         ELPEAK(I) = ULPEAK(I,IRG)
         EETIME(I) = DBLE(0.0)
         SEASON = DBLE(0.0)
         DO J = 1 , ELNVCT(I)
            ELWDTH(J,I) = ULWDTH(J,I,IRG)
            EETIME(I) = EETIME(I) + ELWDTH(J,I)
            ELHGHT(J,I) = ULHGHT(J,I,IRG)
            ELGRP(J,I) = ULGRP(J,I,IRG)
            ELSEG(J,I) = ULSEG(J,I,IRG)
            IGRP = ELGRP(J,I)
            ISEG = ELSEG(J,I)
            UTNGRP = MAX(UTNGRP , IGRP)
            UTNSEG = MAX(UTNSEG , ISEG)
            IF (IRG .GT. 1 .AND. (UTSEAS(IGRP) .NE. I .OR.  &
               UTWDTH(ISEG,IGRP) .NE. ULWDTH(J,I,IRG))) THEN
               WRITE(6,1311) CURIYR,CURITR,IRG,I,J,IGRP,ISEG, &
                  UTSEAS(IGRP),UTWDTH(ISEG,IGRP),ULWDTH(J,I,IRG)
 1311          FORMAT(1X,"BAD_LOAD_GROUP",8(":",I4),2(":",F12.3))
            END IF
            UTSEAS(IGRP) = I
            UTWDTH(ISEG,IGRP) = ULWDTH(J,I,IRG)
            UTHGHT(ISEG,IGRP,IRG) = ULHGHT(J,I,IRG)
            UTXMKT(ISEG,IGRP,IRG) = ULHGHT(J,I,IRG)
            UCRNW(ISEG,IGRP,IRG) = 0.0
            UCRPS(ISEG,IGRP,IRG) = 0.0
            ELMAPS(ISEG,IGRP) = I
            ELMAPV(ISEG,IGRP) = J
            SEASON = SEASON + ELWDTH(J,I) * ELHGHT(J,I)
            ANNUAL = ANNUAL + ELWDTH(J,I) * ELHGHT(J,I)
         END DO  ! J
!
         IF (UF_DBG .GT. 0) THEN
            WRITE(UF_DBG,1001) IYR,ITR,IRG,I,SEASON,ELPEAK(I),EETIME(I)
         END IF
 1001    FORMAT(1X,'ULDSM Y I R S D P T ',4I3,F18.3,F12.6,F6.0)
!
      END DO  ! I
!
      IF (UF_DBG .GT. 0) THEN
         WRITE(UF_DBG,1000) IYR,ITR,IRG,ANNUAL
      END IF
 1000 FORMAT(1X,'ULDSM Y I R   D     ',3I3,3X,F18.3)

!     IF (IRG.EQ.1 .AND. IYR.EQ.1) THEN
!     WRITE(UF_ETT,*)'UQTDLS=',(UQTDLS(I),I=1,13)
!     ENDIF

      EFDNERC2 = ANNUAL * (1 - UQTDLS(IRG))

      WRITE(UF_ETT,366)IYR,IRG,'8', (ANNUAL - EFDNERC2)/1000, EFDNERC2/1000
366   FORMAT(I4,1X,I4,1X,A5,F10.4,1X,F10.4)

      RETURN
      END
!
!
!     Heapsort - Sorts an array RA of length N into descending numerical
!     order using the Heapsort Algorithm. N is input; RA is
!     replaced on output by its sorted rearrangement and RB, RC,
!     RD, RE are rearranged correspondingly.
!
      SUBROUTINE HSORT3(N,RA,RB,RC)
!
      IMPLICIT NONE


      include'emmparm'

      REAL*8 RA(EFD_D_MVS+1),RRA
      INTEGER RB(EFD_D_MVS+1),RRB
      INTEGER RC(EFD_D_MVS+1),RRC
      INTEGER N,L,IR,I,J
!
      L = N / 2 + 1
      IR = N
!
!     The index L will be decremented from its initial value down to 1 durin
!     the "hiring" (heap creation) phase. Once it reaches 1, the index IR
!     will be decremented from its initial value down to 1 during the
!     "retirement-and-promotion" (heap selection) phase.
!
   10 CONTINUE
      IF (L .GT. 1) THEN                       ! Still in hiring phase ?
         L = L - 1
         RRA = RA(L)
         RRB = RB(L)
         RRC = RC(L)
      ELSE                           !  In retirement-and-promotion phase.
         RRA = RA(IR)                  !  Clear a space at end of array.
         RRB = RB(IR)
         RRC = RC(IR)
         RA(IR) = RA(1)              !  Retire the top of the heap into it.
         RB(IR) = RB(1)
         RC(IR) = RC(1)
         IR = IR - 1                 !  Decrease the size of the corporation
         IF (IR .EQ. 1) THEN            !  Done with the last promotion.
            RA(1) = RRA              !  The least competent worker of all !
            RB(1) = RRB
            RC(1) = RRC
            RETURN
         ENDIF
      ENDIF
!
!     Whether we are in the hiring phase or promotion phase, we here set
!     up to sift down element RRA to its proper level.
!
      I = L
      J = L + L
   20 IF (J .LE. IR) THEN                             ! Do while J <= IR
         IF (J .LT. IR) THEN
            IF (RA(J) .GT. RA(J + 1)) J = J + 1   ! Comp to the better under
            ENDIF
         IF (RRA .GT. RA(J)) THEN                           ! Demote RRA
            RA(I) = RA(J)
            RB(I) = RB(J)
            RC(I) = RC(J)
            I = J
            J = J + J
         ELSE         ! This is RRA's level. Set J to terminate the sift-dow
            J = IR + 1
         END IF
         GO TO 20
      ENDIF
      RA(I) = RRA                                  ! Put RRA into its slot.
      RB(I) = RRB
      RC(I) = RRC
      GO TO 10
      END
!
      SUBROUTINE UINIBLD(IRG)
      IMPLICIT NONE
!******************************************************************
!     THIS SUBROUTINE INITIALIZES THE BUILDS LINKED LIST CONTROL ARRAYS FOR ONE REGION
!     THE CONTROL ARRAYS ARE BLDLNK AND LASTBLD
!     THE FILE IS EFPBLDS.daf
!******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'bldctl'
      include 'control'
      Include 'ncntrl'
      include 'uefdout'
      INTEGER IRG                                         !REGION NUMBER
      INTEGER I
      INTEGER J
      DO I = 1,6
         BLDLNK(IRG,I) = 0
      END DO
!     GET LAST RECORD IN DIRECT ACCESS FILE LINKED LIST
      LASTBLD = 0
      DO I = 1,UNRGNS
         DO J = 2,6,2
            IF (LASTBLD .LT. BLDLNK(I,J)) LASTBLD = BLDLNK(I,J)
            END DO
         END DO
         RETURN
         END
!
      SUBROUTINE UPIPBLD(IRG)
      IMPLICIT NONE
!******************************************************************
!     THIS SUBROUTINE CREATES THE INITIAL EFPBLDS.daf DIRECT ACCESS FILE FOR PIPELINE BUILDS
!******************************************************************
      INTEGER O
      PARAMETER (O = 2)
      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'plntin'
      include 'bildin'
      include 'efpgen'
      REAL*4 DITC(O)                     !DEFERRED INVESTMENT TAX CREDIT
      REAL*4 AFDC(O)                                              !AFUDC
      REAL*4 BKVL(O)                                         !BOOK VALUE
      REAL*4 ASVL(O)                                        !ASSET VALUE
      REAL*4 BCWP(O)                                        !BOOKED CWIP
      REAL*4 RCWP(O)                                  !CWIP IN RATE BASE
      INTEGER PTYP                                           !PLANT TYPE
      INTEGER SYR                                            !START YEAR
      REAL*4 PCST                                            !PLANT COST
      REAL*4 PCAP(O)                                !NAME PLATE CAPACITY
      REAL*4 SERP                            !% OF FIRST YEAR IN SERVICE
      REAL*4 ABDE(O)                      !ACCUMULATED BOOK DEPRECIATION
      INTEGER IRG                                         !REGION NUMBER
      INTEGER IO                                     !OWNERSHIP OF BUILD
      INTEGER NO         !THE OWNERSHIP THE BUILD IS NOT (INVERSE OF IO)
      INTEGER FRG
!
!     SET OWNERSHIP INDICES
!
      IO = WFOWN
      FRG = 0
      IF (W_CR .GT. 0 .AND. W_CLRG .GT. 0 .AND. W_GR .GT. 0 .AND. W_CAR .GT. 0) FRG = EPNFLRG(W_CR,W_CLRG,W_GR,W_CAR)
      IF (FRG .EQ. 0) WRITE(6,*) 'UPIPBLD - FRG = 0'
!
!*** IGNORE BUILD IF OWNERSHIP IS GREATER THAN 2 - THIS IS NUGS (?)
!
      IF ((IO .GE. 1) .AND. (IO .LE. 2)) THEN
         IF (IO .EQ. 1) THEN
            NO = 2
         ELSE
            NO = 1
         ENDIF
!
!        ESTABLISH BUILD PARAMETERS BASED ON PLNTIN VALUES
!
         DITC(IO) = 0.0
         DITC(NO) = 0.0
         AFDC(IO) = 0.0
         AFDC(NO) = 0.0
         BKVL(IO) = WBCWP/1000.0
         BKVL(NO) = 0.0
         ASVL(IO) = WASVL/1000.0
         ASVL(NO) = 0.0
         BCWP(IO) = WBCWP/1000.0
         BCWP(NO) = 0.0
         RCWP(IO) = WRCWP/1000.0
         RCWP(NO) = 0.0
         PTYP = WEFPT
         SYR = W_SYR - UHBSYR
         PCST = WPCST
         PCAP(IO) = WC_NP/1000.0
         PCAP(NO) = 0.0
         SERP = (12.0 - W_SMO + 1.0)/12.0
         ABDE(IO) = 0.0
         ABDE(NO) = 0.0
!        ADD BUILD TO LINKED LIST
!        WRITE(22,*)'DEBUG... NEWBLD CALLED FROM PIPBLD (PIPE LINE BUILD)'
         CALL UNEWBLD(IRG,DITC,AFDC,BKVL,ASVL,BCWP,RCWP,PTYP, &
          SYR,PCST,PCAP,SERP,ABDE)
      ENDIF

!     ADD TRANSMISSION COSTS TO EFP LINKED LIST
         WEFPT = EITRAN
         IF ((WECPT .LE. ECP_D_DSP .AND. WECPT .NE. WICN .AND. WECPT .NE. WIAN .AND. WECPT .NE. WISM) .OR. (WECPT .EQ. WIWD) .OR. (WECPT .EQ. WIBI)) THEN
            WPCST = FL_CNXT_CST(IRG,FRG)
            IF (WPCST .GT. 200.0) WPCST = EPCOVR(WIAN)   ! FLCNXTCST may be artificially high for region combos that can't be built, use AN cost as a proxy
         ELSE
            WPCST = EPCOVR(WECPT)
         ENDIF
         CALL UEXPBLD(IRG)
      RETURN
      END
!
      SUBROUTINE UEXPBLD(IRG)
      IMPLICIT NONE
!***********************************************************************
!     THIS SUBROUTINE IS A WRAPPER TO ADD A NEW BUILD FROM THE ECP TO THE EFPBLDS.daf DIRECT ACCESS FILE
!       VIA UNEWBLD
!***********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'plntin'
      INTEGER O
      PARAMETER (O = 2)
      REAL*4 SHR(O)                                    !OWNERSHIP SHARES
      REAL*4 DITC(O)                     !DEFERRED INVESTMENT TAX CREDIT
      REAL*4 AFDC(O)                                              !AFUDC
      REAL*4 BKVL(O)                                         !BOOK VALUE
      REAL*4 ASVL(O)                                        !ASSET VALUE
      REAL*4 BCWP(O)                                        !BOOKED CWIP
      REAL*4 RCWP(O)                                  !CWIP IN RATE BASE
      INTEGER PTYP                                           !PLANT TYPE
      INTEGER SYR                                            !START YEAR
      REAL*4 PCST                                            !PLANT COST
      REAL*4 PCAP(O)                                !NAME PLATE CAPACITY
      REAL*4 SERP                            !% OF FIRST YEAR IN SERVICE
      REAL*4 ABDE(O)                      !ACCUMULATED BOOK DEPRECIATION
      INTEGER IRG
      INTEGER I
      PTYP = WEFPT
      SYR = W_SYR - UHBSYR
      PCST = WPCST
      DO I = 1,2
         DITC(I) = 0.0
         AFDC(I) = 0.0
         BKVL(I) = 0.0
         ASVL(I) = 0.0
         BCWP(I) = 0.0
         RCWP(I) = 0.0
         ABDE(I) = 0.0
!**********TEMPORARY ASSIGNMENT TO PRIVATE OWNERSHIP************
         IF (I .EQ. 1) THEN
            PCAP(I) = WC_NP/1000.0
         ELSE
            PCAP(I) = 0.0
         ENDIF
      END DO
      SERP = 1.0

      WRITE(18,2000) IRG,W_SYR,WECPT,WEFPT,W_IGRP,W_GRP,W_GRP2,WPCST,WC_NP,WC_SUM
 2000 FORMAT(1X,"UEXPBLD_INPUT",7(":",I5),3(":",F9.3))

      CALL UNEWBLD(IRG,DITC,AFDC,BKVL,ASVL,BCWP,RCWP,PTYP,SYR,PCST,PCAP,SERP,ABDE)

      WRITE(18,2001) IRG,PTYP,SYR,DITC(1),AFDC(1),BKVL(1),ASVL(1),BCWP(1),RCWP(1),PCST,PCAP,SERP,ABDE(1)
 2001 FORMAT(1X,"UEXPBLD_OUTPUT",3(":",I4),11(":",F9.3))

      RETURN
      END
!
      SUBROUTINE UNEWBLD(IRG,DITC,AFDC,BKVL,ASVL,BCWP,RCWP,PTYP,SYR,PCST,PCAP,SERP,ABDE)

      IMPLICIT NONE

!     THIS SUBROUTINE ADDS A NEW BUILD TO THE BUILD TO THE EFPBLDS.daf DIRECT ACCESS FILE

      INTEGER O
      PARAMETER (O = 2)

      include'parametr'
      include'ncntrl'
      include 'bldctl'

      REAL*4 DITC(O)                     !DEFERRED INVESTMENT TAX CREDIT
      REAL*4 AFDC(O)                                              !AFUDC
      REAL*4 BKVL(O)                                         !BOOK VALUE
      REAL*4 ASVL(O)                                        !ASSET VALUE
      REAL*4 BCWP(O)                                        !BOOKED CWIP
      REAL*4 RCWP(O)                                  !CWIP IN RATE BASE
      INTEGER PTYP                                           !PLANT TYPE
      INTEGER SYR                                            !START YEAR
      REAL*4 PCST                                            !PLANT COST
      REAL*4 PCAP(O)                                !NAME PLATE CAPACITY
      REAL*4 SERP                            !% OF FIRST YEAR IN SERVICE
      REAL*4 ABDE(O)                      !ACCUMULATED BOOK DEPRECIATION
      REAL*4 TDITC(O)          !TEMPORARY DEFERRED INVESTMENT TAX CREDIT
      REAL*4 TAFDC(O)                                   !TEMPORARY AFUDC
      REAL*4 TBKVL(O)                              !TEMPORARY BOOK VALUE
      REAL*4 TASVL(O)                             !TEMPORARY ASSET VALUE
      REAL*4 TBCWP(O)                             !TEMPORARY BOOKED CWIP
      REAL*4 TRCWP(O)                       !TEMPORARY CWIP IN RATE BASE
      INTEGER TPTYP                                !TEMPORARY PLANT TYPE
      INTEGER TSYR                                 !TEMPORARY START YEAR
      REAL*4 TPCST                                 !TEMPORARY PLANT COST
      REAL*4 TPCAP(O)                     !TEMPORARY NAME PLATE CAPACITY
      REAL*4 TSERP                 !TEMPORARY % OF FIRST YEAR IN SERVICE
      REAL*4 TABDE(O)           !TEMPORARY ACCUMULATED BOOK DEPRECIATION
      INTEGER CATMAP(14)                 !MAPS PLANT TYPES TO CATEGORIES
      INTEGER IRG                                         !REGION NUMBER
      INTEGER ICAT                            !ARRAY INDEX OF FIRST LINK
      INTEGER I
      INTEGER IREC
      DATA CATMAP/1,1,1,1,1,1,1,1,1,1,1,1,2,3/

      LASTBLD = LASTBLD + 1
      ICAT = CATMAP(PTYP)   !MAP PLANT TYPE TO CATEGORY (GEN,TRAN,DIST,OTHER
      ICAT = ((ICAT - 1) * 2) + 1  !CONVERT TO ARRAY INDEX OF FIRST LINK
      IF (BLDLNK(IRG,ICAT) .EQ. 0) THEN

!        FIRST BUILD ON LINKED LIST

         BLDLNK(IRG,ICAT) = LASTBLD     !MAKE IT THE FIRST BUILD ON LIST
      ELSE

!        GET LAST RECORD OF REGION AND UPDATE THE LINK

         IREC = BLDLNK(IRG,ICAT + 1)
         READ(IBD,REC = &
            IREC)BLINK,(TDITC(I),I = 1,2),(TAFDC(I),I = 1,2), &
            (TBKVL(I),I = 1,2),(TASVL(I),I = 1,2), &
            (TBCWP(I),I = 1,2),(TRCWP(I),I = 1,2), &
            TPCST,(TPCAP(I),I = 1,2),TSERP, &
            (TABDE(I),I = 1,2),TPTYP,TSYR
         BLINK = LASTBLD

         WRITE(IBD,REC = &
          IREC)BLINK,(TDITC(I),I = 1,2),(TAFDC(I),I = 1,2), &
          (TBKVL(I),I = 1,2),(TASVL(I),I = 1,2), &
          (TBCWP(I),I = 1,2),(TRCWP(I),I = 1,2), &
          TPCST,(TPCAP(I),I = 1,2),TSERP, &
          (TABDE(I),I = 1,2),TPTYP,TSYR
      ENDIF

!     WRITE OUT NEW BUILD

      BLINK = 0                       !IT IS NOT LINKED TO ANOTHER BUILD

      WRITE(IBD,REC = LASTBLD)BLINK,(DITC(I),I = 1,2),(AFDC(I),I = 1,2), &
         (BKVL(I),I = 1,2),(ASVL(I),I = 1,2), &
         (BCWP(I),I = 1,2),(RCWP(I),I = 1,2), &
         PCST,(PCAP(I),I = 1,2),SERP, &
         (ABDE(I),I = 1,2),PTYP,SYR

      WRITE(18,1301) CURIYR+1989, CURITR, IRG, ICAT, LASTBLD, LASTBLD, BLINK, PTYP, SYR + 1989, PCST, SERP,  &
         PCAP(1), BKVL(1), ASVL(1), AFDC(1), BCWP(1), RCWP(1), ABDE(1),  &
         PCAP(2), BKVL(2), ASVL(2), AFDC(2), BCWP(2), RCWP(2), ABDE(2)
 1301 FORMAT(1X,"UNEWBLD",3(":",I4),":",I1,5(":",I5),16(":",F9.3))

      BLDLNK(IRG,ICAT + 1) = LASTBLD      !UPDATE THE LAST BUILD ON LIST

      RETURN
      END

!
      SUBROUTINE RDYRDAT
!
      USE SQLITE
      IMPLICIT NONE

!     THIS SUBROUTINE READS YEARLY DATA FOR NUCLEAR CAPACITY FACTORS AND BENCHMARKING

      include 'parametr'
      include 'ncntrl'
      include 'macout'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'dispinyr'
      include 'dispett'
      include 'bildin'
      include 'uefdout'
      include 'emission'
      include 'mpblk'
      include 'dsmdimen'
      include 'dsmcaldr'
      include 'dsmtfecp'
      include 'dsmtoefd'
      include 'dsmsectr'
!
      CHARACTER*80 FILENM
      INTEGER NERC,YEAR,BYEAR,IYR,LHYEAR,POL,IFL,IGRD,IRG,ICR,REG,ICAP,YRCG,CEN,FLCG,RGCG
      REAL*8 TEMP, REV_RS, REV_CM, REV_IN, REV_TR, REV_AS,TMPOIL
      REAL*8 PEL_RS, PEL_CM, PEL_IN, PEL_TR, PEL_AS
      REAL*8 T_NUC, T_GEO, T_WOD, T_MSW, T_FOS
      REAL*8 T_NG, T_OL, T_CL, T_OT, T_DS, T_RS
      REAL*8 GN_CL, GN_OL, GN_NG, GN_OG, GN_UR, GN_PS, GN_GE, GN_MS, GN_WD, GN_SO, GN_PV, GN_PT, GN_WN, GN_WL, GN_WF, GN_HY, GN_OT
      REAL*8 GEN_PC, GEN_IG, GEN_IS, GEN_ST, GEN_CC, GEN_AC, GEN_AS, GEN_CT, GEN_AT, GEN_IC, GEN_FC, GEN_DG, GEN_NU, GEN_RN, GEN_PS, GEN_DS
      REAL*8 FL_CL, FL_DS, FL_RS, FL_NG,FL_OL,CR_SH
      REAL*8 FL_NGCONS(12) !TD
      REAL*8 T_CM, T_IN, T_TR, T_AS, T_SO2, T_NOX, T_CO2, T_IMPF, T_IMPE, T_EXPF, T_EXPE
      INTEGER IS,IFUEL,ISEAS,ICENSUS,INERC
      REAL SUMCOGCEN(TC_FUELS,MNUMCR)
      
      INTEGER II
      REAL TotalG
      LOGICAL NEW
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
!
      type(sqlite_database)                      :: db
      type(sqlite_statement)                     :: stmt
      type(sqlite_column), dimension(:), pointer :: col
      character(len=40), pointer, dimension(:)   :: result
      character(len=80)                          :: errmsg

      integer                                    :: i
      integer                                    :: crg
      integer                                    :: igrid
      integer                                    :: iown
      integer                                    :: cgf
      real                                       :: bkgain
      logical                                    :: finished

!     OPEN FILE OF YEAR DATA
!
      FILENM = 'ELDATYR'
      NEW = .FALSE.
      UF_TMP = FILE_MGR('O',FILENM,NEW)
!
      UYR_HIST = HSTYEAR + NHIST - 1                                         !
      UYR_STEO = UYR_HIST + NSTEO
      UYR_EMIS = UYR_HIST
      
      write(6,'(" HSTYEAR,UYR_HIST,UYR_STEO,UYR_EMIS=",3I5)')  HSTYEAR,UYR_HIST,UYR_STEO,UYR_EMIS
!
      write(18,1001) CURIRUN, CURCALYR, UYR_HIST, UYR_STEO, UYR_EMIS
 1001 FORMAT(1X,"ELDATYR_UYR_INFO_OLD",5(",",I5))

!
!     READ BENCHMARKING DATA FOR HISTORICAL AND STEO YEARS
!     ALL ELDATYR DATA IS NOW READ IN FROM THE EMM REGION TOOL
!
!
!  Obtain remaining eldatyr data from regional tool database
!
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
!     QUERY THE DATABASE TABLE V_EMM_ELDATYR - GENERATION BY FUEL TYPE / RENEWABLE TYPE

      allocate ( col(20) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'OWNERSHIP', sqlite_int )
      call sqlite3_column_query( col(004), 'HGNCLNR', sqlite_real )
      call sqlite3_column_query( col(005), 'HGNOLNR', sqlite_real )
      call sqlite3_column_query( col(006), 'HGNNGNR', sqlite_real )
      call sqlite3_column_query( col(007), 'HGNOGNR', sqlite_real )
      call sqlite3_column_query( col(008), 'HGNURNR', sqlite_real )
      call sqlite3_column_query( col(009), 'HGNPSNR', sqlite_real )
      call sqlite3_column_query( col(010), 'HGNGENR', sqlite_real )
      call sqlite3_column_query( col(011), 'HGNMSNR', sqlite_real )
      call sqlite3_column_query( col(012), 'HGNWDNR', sqlite_real )
      call sqlite3_column_query( col(013), 'HGNSONR', sqlite_real )
      call sqlite3_column_query( col(014), 'HGNPVNR', sqlite_real )
      call sqlite3_column_query( col(015), 'HGNPTNR', sqlite_real )
      call sqlite3_column_query( col(016), 'HGNWNNR', sqlite_real )
      call sqlite3_column_query( col(017), 'HGNWLNR', sqlite_real )
      call sqlite3_column_query( col(018), 'HGNWFNR', sqlite_real )
      call sqlite3_column_query( col(019), 'HGNHYNR', sqlite_real )
      call sqlite3_column_query( col(020), 'HGNOTNR', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_ELDATYR', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), iown)
         call sqlite3_get_column( col(004), GN_CL)
         call sqlite3_get_column( col(005), GN_OL)
         call sqlite3_get_column( col(006), GN_NG)
         call sqlite3_get_column( col(007), GN_OG)
         call sqlite3_get_column( col(008), GN_UR)
         call sqlite3_get_column( col(009), GN_PS)
         call sqlite3_get_column( col(010), GN_GE)
         call sqlite3_get_column( col(011), GN_MS)
         call sqlite3_get_column( col(012), GN_WD)
         call sqlite3_get_column( col(013), GN_SO)
         call sqlite3_get_column( col(014), GN_PV)
         call sqlite3_get_column( col(015), GN_PT)
         call sqlite3_get_column( col(016), GN_WN)
         call sqlite3_get_column( col(017), GN_WL)
         call sqlite3_get_column( col(018), GN_WF)
         call sqlite3_get_column( col(019), GN_HY)
         call sqlite3_get_column( col(020), GN_OT)

         iyr = year - UHBSYR
         
         IF (IOWN.LE.2) THEN
         WRITE(18,2020) CURIRUN, CURCALYR, irg, year, iyr, iown, HGNCLNR(iown,irg,iyr), HGNOLNR(iown,irg,iyr), HGNNGNR(iown,irg,iyr), &
            HGNOGNR(iown,irg,iyr), HGNURNR(iown,irg,iyr), HGNPSNR(iown,irg,iyr), HGNGENR(iown,irg,iyr), HGNMSNR(iown,irg,iyr), &
            HGNWDNR(iown,irg,iyr), HGNSONR(iown,irg,iyr), HGNPVNR(iown,irg,iyr), HGNPTNR(iown,irg,iyr), HGNWNNR(iown,irg,iyr), &
            HGNWLNR(iown,irg,iyr), HGNWFNR(iown,irg,iyr), HGNHYNR(iown,irg,iyr), HGNOTNR(iown,irg,iyr)
 2020    FORMAT(1X,"V_EMM_ELDATYR_GN_OLD",6(",",I5),17(",",F21.6))

         WRITE(18,2021), CURIRUN, CURCALYR, irg, year, iyr, iown, GN_CL, GN_OL, GN_NG, GN_OG, GN_UR, GN_PS, GN_GE, GN_MS, GN_WD, GN_SO, &
            GN_PV, GN_PT, GN_WN, GN_WL, GN_WF, GN_HY, GN_OT
 2021    FORMAT(1X,"V_EMM_ELDATYR_GN_NEW",6(",",I5),17(",",F21.6))

           HGNCLNR(iown,irg,iyr) = GN_CL
           HGNOLNR(iown,irg,iyr) = GN_OL
           HGNNGNR(iown,irg,iyr) = GN_NG
           HGNOGNR(iown,irg,iyr) = GN_OG
           HGNURNR(iown,irg,iyr) = GN_UR
           HGNPSNR(iown,irg,iyr) = GN_PS
           HGNGENR(iown,irg,iyr) = GN_GE
           HGNMSNR(iown,irg,iyr) = GN_MS
           HGNWDNR(iown,irg,iyr) = GN_WD
           HGNSONR(iown,irg,iyr) = GN_SO
           HGNPVNR(iown,irg,iyr) = GN_PV
           HGNPTNR(iown,irg,iyr) = GN_PT
           HGNWNNR(iown,irg,iyr) = GN_WN
           HGNWLNR(iown,irg,iyr) = GN_WL
           HGNWFNR(iown,irg,iyr) = GN_WF
           HGNHYNR(iown,irg,iyr) = GN_HY
           HGNOTNR(iown,irg,iyr) = GN_OT
         ENDIF
      end do

      deallocate(col)

!     QUERY THE DATABASE TABLE V_EMM_ELDATYR - GENERATION BY FOSSIL PLANT TYPE

      allocate ( col(19) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'OWNERSHIP', sqlite_int )
      call sqlite3_column_query( col(004), 'HGENPC', sqlite_real )
      call sqlite3_column_query( col(005), 'HGENIG', sqlite_real )
      call sqlite3_column_query( col(006), 'HGENIS', sqlite_real )
      call sqlite3_column_query( col(007), 'HGENST', sqlite_real )
      call sqlite3_column_query( col(008), 'HGENCC', sqlite_real )
      call sqlite3_column_query( col(009), 'HGENAC', sqlite_real )
      call sqlite3_column_query( col(010), 'HGENAS', sqlite_real )
      call sqlite3_column_query( col(011), 'HGENCT', sqlite_real )
      call sqlite3_column_query( col(012), 'HGENAT', sqlite_real )
      call sqlite3_column_query( col(013), 'HGENIC', sqlite_real )
      call sqlite3_column_query( col(014), 'HGENFC', sqlite_real )
      call sqlite3_column_query( col(015), 'HGENDG', sqlite_real )
      call sqlite3_column_query( col(016), 'HGENNU', sqlite_real )
      call sqlite3_column_query( col(017), 'HGENRN', sqlite_real )
      call sqlite3_column_query( col(018), 'HGENPS', sqlite_real )
      call sqlite3_column_query( col(019), 'HGENDS', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_ELDATYR', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), iown)
         call sqlite3_get_column( col(004), GEN_PC)
         call sqlite3_get_column( col(005), GEN_IG)
         call sqlite3_get_column( col(006), GEN_IS)
         call sqlite3_get_column( col(007), GEN_ST)
         call sqlite3_get_column( col(008), GEN_CC)
         call sqlite3_get_column( col(009), GEN_AC)
         call sqlite3_get_column( col(010), GEN_AS)
         call sqlite3_get_column( col(011), GEN_CT)
         call sqlite3_get_column( col(012), GEN_AT)
         call sqlite3_get_column( col(013), GEN_IC)
         call sqlite3_get_column( col(014), GEN_FC)
         call sqlite3_get_column( col(015), GEN_DG)
         call sqlite3_get_column( col(016), GEN_NU)
         call sqlite3_get_column( col(017), GEN_RN)
         call sqlite3_get_column( col(018), GEN_PS)
         call sqlite3_get_column( col(019), GEN_DS)

         iyr = year - UHBSYR

         WRITE(18,2022) CURIRUN, CURCALYR, irg, year, iyr, iown, HGENPC(iown,irg,iyr), HGENIG(iown,irg,iyr), HGENIS(iown,irg,iyr), &
            HGENST(iown,irg,iyr), HGENCC(iown,irg,iyr), HGENAC(iown,irg,iyr), HGENAS(iown,irg,iyr), HGENCT(iown,irg,iyr), &
            HGENAT(iown,irg,iyr), HGENIC(iown,irg,iyr), HGENFC(iown,irg,iyr), HGENDG(iown,irg,iyr), HGENNU(iown,irg,iyr), &
            HGENRN(iown,irg,iyr), HGENPS(iown,irg,iyr), HGENDS(iown,irg,iyr)
 2022    FORMAT(1X,"V_EMM_ELDATYR_GEN_OLD",6(",",I5),16(",",F21.6))

         WRITE(18,2023) CURIRUN, CURCALYR, irg, year, iyr, iown, GEN_PC, GEN_IG, GEN_IS, GEN_ST, GEN_CC, GEN_AC, GEN_AS, GEN_CT, GEN_AT, &
            GEN_IC, GEN_FC, GEN_DG, GEN_NU, GEN_RN, GEN_PS, GEN_DS
 2023    FORMAT(1X,"V_EMM_ELDATYR_GEN_NEW",6(",",I5),16(",",F21.6))

         HGENPC(iown,irg,iyr) = GEN_PC
         HGENIG(iown,irg,iyr) = GEN_IG
         HGENIS(iown,irg,iyr) = GEN_IS
         HGENST(iown,irg,iyr) = GEN_ST
         HGENCC(iown,irg,iyr) = GEN_CC
         HGENAC(iown,irg,iyr) = GEN_AC
         HGENAS(iown,irg,iyr) = GEN_AS
         HGENCT(iown,irg,iyr) = GEN_CT
         HGENAT(iown,irg,iyr) = GEN_AT
         HGENIC(iown,irg,iyr) = GEN_IC
         HGENFC(iown,irg,iyr) = GEN_FC
         HGENDG(iown,irg,iyr) = GEN_DG
         HGENNU(iown,irg,iyr) = GEN_NU
         HGENRN(iown,irg,iyr) = GEN_RN
         HGENPS(iown,irg,iyr) = GEN_PS
         HGENDS(iown,irg,iyr) = GEN_DS

      end do

!     Sum generation by plant type

      DO YEAR = HSTYEAR , UYR_HIST
         BYEAR = YEAR - UHBSYR
         DO NERC = 1 , MNUMNR
            HGENPC(4,NERC,BYEAR) = HGENPC(1,NERC,BYEAR) + HGENPC(2,NERC,BYEAR) + HGENPC(3,NERC,BYEAR)
            HGENIG(4,NERC,BYEAR) = HGENIG(1,NERC,BYEAR) + HGENIG(2,NERC,BYEAR) + HGENIG(3,NERC,BYEAR)
            HGENIS(4,NERC,BYEAR) = HGENIS(1,NERC,BYEAR) + HGENIS(2,NERC,BYEAR) + HGENIS(3,NERC,BYEAR)
            HGENST(4,NERC,BYEAR) = HGENST(1,NERC,BYEAR) + HGENST(2,NERC,BYEAR) + HGENST(3,NERC,BYEAR)
            HGENCC(4,NERC,BYEAR) = HGENCC(1,NERC,BYEAR) + HGENCC(2,NERC,BYEAR) + HGENCC(3,NERC,BYEAR)
            HGENAC(4,NERC,BYEAR) = HGENAC(1,NERC,BYEAR) + HGENAC(2,NERC,BYEAR) + HGENAC(3,NERC,BYEAR)
            HGENAS(4,NERC,BYEAR) = HGENAS(1,NERC,BYEAR) + HGENAS(2,NERC,BYEAR) + HGENAS(3,NERC,BYEAR)
            HGENCT(4,NERC,BYEAR) = HGENCT(1,NERC,BYEAR) + HGENCT(2,NERC,BYEAR) + HGENCT(3,NERC,BYEAR)
            HGENAT(4,NERC,BYEAR) = HGENAT(1,NERC,BYEAR) + HGENAT(2,NERC,BYEAR) + HGENAT(3,NERC,BYEAR)
            HGENIC(4,NERC,BYEAR) = HGENIC(1,NERC,BYEAR) + HGENIC(2,NERC,BYEAR) + HGENIC(3,NERC,BYEAR)
            HGENFC(4,NERC,BYEAR) = HGENFC(1,NERC,BYEAR) + HGENFC(2,NERC,BYEAR) + HGENFC(3,NERC,BYEAR)
            HGENDG(4,NERC,BYEAR) = HGENDG(1,NERC,BYEAR) + HGENDG(2,NERC,BYEAR) + HGENDG(3,NERC,BYEAR)
            HGENNU(4,NERC,BYEAR) = HGENNU(1,NERC,BYEAR) + HGENNU(2,NERC,BYEAR) + HGENNU(3,NERC,BYEAR)
            HGENRN(4,NERC,BYEAR) = HGENRN(1,NERC,BYEAR) + HGENRN(2,NERC,BYEAR) + HGENRN(3,NERC,BYEAR)
            HGENPS(4,NERC,BYEAR) = HGENPS(1,NERC,BYEAR) + HGENPS(2,NERC,BYEAR) + HGENPS(3,NERC,BYEAR)
            HGENDS(4,NERC,BYEAR) = HGENDS(1,NERC,BYEAR) + HGENDS(2,NERC,BYEAR) + HGENDS(3,NERC,BYEAR)
         ENDDO
      ENDDO

      deallocate(col)

!     QUERY THE DATABASE TABLE V_EMM_ELDATYR - CONSUMPTION BY FOSSIL FUEL TYPE AND EMM REGION

      allocate ( col(7) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'OWNERSHIP', sqlite_int )
      call sqlite3_column_query( col(004), 'HFLCLNR', sqlite_real )
      call sqlite3_column_query( col(005), 'HFLDSNR', sqlite_real )
      call sqlite3_column_query( col(006), 'HFLRSNR', sqlite_real )
      call sqlite3_column_query( col(007), 'HFLNGNR', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_ELDATYR', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), iown)
         call sqlite3_get_column( col(004), FL_CL)
         call sqlite3_get_column( col(005), FL_DS)
         call sqlite3_get_column( col(006), FL_RS)
         call sqlite3_get_column( col(007), FL_NG)

         iyr = year - UHBSYR
         
         IF (IOWN.LE.2) THEN
         WRITE(18,2024) CURIRUN, CURCALYR, irg, year, iyr, iown, HFLCLNR(iown,irg,iyr), HFLDSNR(iown,irg,iyr), HFLRSNR(iown,irg,iyr), HFLNGNR(iown,irg,iyr)
 2024    FORMAT(1X,"V_EMM_ELDATYR_FL_OLD",6(",",I5),4(",",F21.6))

         WRITE(18,2025) CURIRUN, CURCALYR, irg, year, iyr, iown, FL_CL, FL_DS, FL_RS, FL_NG
 2025    FORMAT(1X,"V_EMM_ELDATYR_FL_NEW",6(",",I5),4(",",F21.6))


         HFLCLNR(iown,irg,iyr) = FL_CL
           HFLDSNR(iown,irg,iyr) = FL_DS
           HFLRSNR(iown,irg,iyr) = FL_RS
           HFLNGNR(iown,irg,iyr) = FL_NG
           HFLOLNR(iown,irg,iyr) = HFLDSNR(iown,irg,iyr) + HFLRSNR(iown,irg,iyr)
         ENDIF

      end do

      deallocate(col)

!     QUERY THE DATABASE TABLE H_ELDATYR_GAS - CONSUMPTION BY FOSSIL FUEL TYPE BY GAS REGION

      allocate ( col(19) ) !TD

      call sqlite3_column_query( col(001), 'GAS_RGN', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'HFLCLGR', sqlite_real )
      call sqlite3_column_query( col(004), 'HFLNGGR', sqlite_real )
      call sqlite3_column_query( col(005), 'HFLDSGR', sqlite_real )
      call sqlite3_column_query( col(006), 'HFLRSGR', sqlite_real )
      call sqlite3_column_query( col(007), 'HFLOLGR', sqlite_real )
      !TD
      call sqlite3_column_query( col(008), 'HGASCONM1', sqlite_real )
      call sqlite3_column_query( col(009), 'HGASCONM2', sqlite_real )
      call sqlite3_column_query( col(010), 'HGASCONM3', sqlite_real )
      call sqlite3_column_query( col(011), 'HGASCONM4', sqlite_real )
      call sqlite3_column_query( col(012), 'HGASCONM5', sqlite_real )
      call sqlite3_column_query( col(013), 'HGASCONM6', sqlite_real )
      call sqlite3_column_query( col(014), 'HGASCONM7', sqlite_real )
      call sqlite3_column_query( col(015), 'HGASCONM8', sqlite_real )
      call sqlite3_column_query( col(016), 'HGASCONM9', sqlite_real )
      call sqlite3_column_query( col(017), 'HGASCONM10', sqlite_real )
      call sqlite3_column_query( col(018), 'HGASCONM11', sqlite_real )
      call sqlite3_column_query( col(019), 'HGASCONM12', sqlite_real )
      
      !TD

      call sqlite3_prepare_select( db, 'H_ELDATYR_GAS', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), FL_CL)
         call sqlite3_get_column( col(004), FL_NG)
         call sqlite3_get_column( col(005), FL_DS)
         call sqlite3_get_column( col(006), FL_RS)
         call sqlite3_get_column( col(007), FL_OL)
         !TD
         do ii=1,12
           call sqlite3_get_column( col(ii+7), FL_NGCONS(ii))
         enddo
         !TD

         iyr = year - UHBSYR

         WRITE(18,2044) CURIRUN, CURCALYR, irg, year, iyr, HFLCLGR(irg,iyr), HFLNGGR(irg,iyr), HFLOLGR(irg,iyr)
 2044    FORMAT(1X,"H_ELDATYR_GAS_OLD",5(",",I5),3(",",F21.6))

         WRITE(18,2045) CURIRUN, CURCALYR, irg, year, iyr, FL_CL, FL_DS, FL_RS, FL_NG, FL_OL
 2045    FORMAT(1X,"H_ELDATYR_GAS_NEW",5(",",I5),4(",",F21.6))


           HFLCLGR(irg,iyr) = FL_CL
           HFLNGGR(irg,iyr) = FL_NG
           TMPOIL = FL_DS + FL_RS
           IF ( TMPOIL .EQ. 0.0 ) THEN
             HFLOLGR(irg,iyr) = FL_OL       
           ELSE
             HFLOLGR(irg,iyr) = TMPOIL             
           ENDIF

           ! sum seasons; MONTYP is the mapping from month to season
           HSEAGASPER(irg,1:4,iyr)=0.0
           do ii=1,12
             HSEAGASPER(irg,MONTYP(ii),iyr)=HSEAGASPER(irg,MONTYP(ii),iyr)+FL_NGCONS(ii)
           enddo
           !get annual total by region
           TotalG=SUM(HSEAGASPER(irg,1:3,iyr))
          
          ! calculate seasonal percentage
           do ii=1,3
              HSEAGASPER(irg,ii,iyr)=HSEAGASPER(irg,ii,iyr)/TotalG
           enddo
!          write(667,*) 'compare emmcntl',iyr+1989,irg,HSEAGASPER(irg,1:3,iyr)
      end do

      deallocate(col)

!     QUERY THE DATABASE TABLE H_ELDATYR_CENSUS - CONSUMPTION BY FOSSIL FUEL TYPE BY CENSUS REGION & Census Region Share of Net Imports

      allocate ( col(7) )

      call sqlite3_column_query( col(001), 'CENSUS', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'HFLCLCR', sqlite_real )
      call sqlite3_column_query( col(004), 'HFLNGCR', sqlite_real )
      call sqlite3_column_query( col(005), 'HFLDSCR', sqlite_real )
      call sqlite3_column_query( col(006), 'HFLRSCR', sqlite_real )
      call sqlite3_column_query( col(007), 'NIMPCRSH', sqlite_real )

      call sqlite3_prepare_select( db, 'H_ELDATYR_CENSUS', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), FL_CL)
         call sqlite3_get_column( col(004), FL_NG)
         call sqlite3_get_column( col(005), FL_DS)
         call sqlite3_get_column( col(006), FL_RS)
         call sqlite3_get_column( col(007), CR_SH)
         
         iyr = year - UHBSYR
 
         WRITE(18,2064) CURIRUN, CURCALYR, irg, year, iyr, HFLCLCR(irg,iyr), HFLDSCR(irg,iyr), HFLRSCR(irg,iyr), HFLNGCR(irg,iyr), NIMPCRSH(irg,iyr)
 2064    FORMAT(1X,"H_ELDATYR_CENSUS_OLD",5(",",I5),5(",",F21.6))

         WRITE(18,2065) CURIRUN, CURCALYR, irg, year, iyr, FL_CL, FL_DS, FL_RS, FL_NG, CR_SH
 2065    FORMAT(1X,"H_ELDATYR_CENSUS_NEW",5(",",I5),5(",",F21.6))


           HFLCLCR(irg,iyr) = FL_CL
           HFLDSCR(irg,iyr) = FL_DS
           HFLRSCR(irg,iyr) = FL_RS
           HFLNGCR(irg,iyr) = FL_NG
           HFLOLCR(irg,iyr) = HFLDSCR(irg,iyr) + HFLRSCR(irg,iyr)
           NIMPCRSH(irg,iyr) = CR_SH

         WRITE(18,2067) irg, year, iyr, HFLCLCR(irg,iyr), HFLDSCR(irg,iyr), HFLRSCR(irg,iyr), HFLNGCR(irg,iyr), NIMPCRSH(irg,iyr)
 2067    FORMAT(1X,"H_ELDATYR_CENSUS_AFT HFLCLCR DSCR RDCR NGCR ",5(",",I5),5(",",F21.6))

      end do

      deallocate(col)

!     QUERY THE DATABASE TABLE V_EMM_ELDATYR_PR

      allocate ( col(12) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'BYEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'HREVRSNR', sqlite_real )
      call sqlite3_column_query( col(004), 'HREVCMNR', sqlite_real )
      call sqlite3_column_query( col(005), 'HREVINNR', sqlite_real )
      call sqlite3_column_query( col(006), 'HREVTRNR', sqlite_real )
      call sqlite3_column_query( col(007), 'HREVASNR', sqlite_real )
      call sqlite3_column_query( col(008), 'HPELRSNR', sqlite_real )
      call sqlite3_column_query( col(009), 'HPELCMNR', sqlite_real )
      call sqlite3_column_query( col(010), 'HPELINNR', sqlite_real )
      call sqlite3_column_query( col(011), 'HPELTRNR', sqlite_real )
      call sqlite3_column_query( col(012), 'HPELASNR', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_ELDATYR_PR', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), REV_RS)
         call sqlite3_get_column( col(004), REV_CM)
         call sqlite3_get_column( col(005), REV_IN)
         call sqlite3_get_column( col(006), REV_TR)
         call sqlite3_get_column( col(007), REV_AS)
         call sqlite3_get_column( col(008), PEL_RS)
         call sqlite3_get_column( col(009), PEL_CM)
         call sqlite3_get_column( col(010), PEL_IN)
         call sqlite3_get_column( col(011), PEL_TR)
         call sqlite3_get_column( col(012), PEL_AS)

         iyr = year - UHBSYR

         WRITE(18,2005) CURIRUN, CURCALYR, irg, year, iyr, HREVRSNR(irg,iyr), HREVCMNR(irg,iyr), HREVINNR(irg,iyr), HREVTRNR(irg,iyr), HREVASNR(irg,iyr), &
            HPELRSNR(irg,iyr), HPELCMNR(irg,iyr), HPELINNR(irg,iyr), HPELTRNR(irg,iyr), HPELASNR(irg,iyr), MC_JPGDP(iyr)
 2005    FORMAT(1X,"V_EMM_ELDATYR_PR_OLD",5(",",I5),11(",",F21.6))

         WRITE(18,2006) CURIRUN, CURCALYR, irg, year, iyr, REV_RS, REV_CM, REV_IN, REV_TR, REV_AS, PEL_RS, PEL_CM, PEL_IN, PEL_TR, PEL_AS
 2006    FORMAT(1X,"V_EMM_ELDATYR_PR_NEW",5(",",I5),10(",",F21.6))

         HREVRSNR(irg,iyr) = REV_RS
         HREVCMNR(irg,iyr) = REV_CM
         HREVINNR(irg,iyr) = REV_IN
         HREVTRNR(irg,iyr) = REV_TR
         HREVASNR(irg,iyr) = REV_AS
         HPELRSNR(irg,iyr) = PEL_RS / MC_JPGDP(iyr)
         HPELCMNR(irg,iyr) = PEL_CM / MC_JPGDP(iyr)
         HPELINNR(irg,iyr) = PEL_IN / MC_JPGDP(iyr)
         HPELTRNR(irg,iyr) = PEL_TR / MC_JPGDP(iyr)
         HPELASNR(irg,iyr) = PEL_AS / MC_JPGDP(iyr)

      end do

      deallocate(col)

!     QUERY THE DATABASE TABLE H_ELDATYR_CENSUS2   add year here in case byear and year arent' the same!!!

      allocate ( col(12) )

      call sqlite3_column_query( col(001), 'CENSUS', sqlite_int )
      call sqlite3_column_query( col(002), 'BYEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'HREVRSCR', sqlite_real )
      call sqlite3_column_query( col(004), 'HREVCMCR', sqlite_real )
      call sqlite3_column_query( col(005), 'HREVINCR', sqlite_real )
      call sqlite3_column_query( col(006), 'HREVTRCR', sqlite_real )
      call sqlite3_column_query( col(007), 'HREVASCR', sqlite_real )
      call sqlite3_column_query( col(008), 'HPELRSCR', sqlite_real )
      call sqlite3_column_query( col(009), 'HPELCMCR', sqlite_real )
      call sqlite3_column_query( col(010), 'HPELINCR', sqlite_real )
      call sqlite3_column_query( col(011), 'HPELTRCR', sqlite_real )
      call sqlite3_column_query( col(012), 'HPELASCR', sqlite_real )

      call sqlite3_prepare_select( db, 'H_ELDATYR_CENSUS2', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), REV_RS)
         call sqlite3_get_column( col(004), REV_CM)
         call sqlite3_get_column( col(005), REV_IN)
         call sqlite3_get_column( col(006), REV_TR)
         call sqlite3_get_column( col(007), REV_AS)
         call sqlite3_get_column( col(008), PEL_RS) 
         call sqlite3_get_column( col(009), PEL_CM) 
         call sqlite3_get_column( col(010), PEL_IN) 
         call sqlite3_get_column( col(011), PEL_TR)
         call sqlite3_get_column( col(012), PEL_AS) 

         iyr = year - UHBSYR

         WRITE(18,2075) CURIRUN, CURCALYR, irg, year, iyr, HREVRSCR(irg,iyr), HREVCMCR(irg,iyr), HREVINCR(irg,iyr), HREVTRCR(irg,iyr), HREVASCR(irg,iyr), &
            HPELRS(irg,iyr), HPELCM(irg,iyr), HPELIN(irg,iyr), HPELTR(irg,iyr), HPELAS(irg,iyr), MC_JPGDP(iyr)
 2075    FORMAT(1X,"H_ELDATYR_CENSUS2_OLD",5(",",I5),11(",",F21.6))

         WRITE(18,2076) CURIRUN, CURCALYR, irg, year, iyr, REV_RS, REV_CM, REV_IN, REV_TR, REV_AS, PEL_RS, PEL_CM, PEL_IN, PEL_TR, PEL_AS
 2076    FORMAT(1X,"H_ELDATYR_CENSUS2_NEW",5(",",I5),10(",",F21.6))

         HREVRSCR(irg,iyr) = REV_RS
         HREVCMCR(irg,iyr) = REV_CM
         HREVINCR(irg,iyr) = REV_IN
         HREVTRCR(irg,iyr) = REV_TR
         HREVASCR(irg,iyr) = REV_AS
         HPELRS(irg,iyr) = PEL_RS / MC_JPGDP(iyr)
         HPELCM(irg,iyr) = PEL_CM / MC_JPGDP(iyr)
         HPELIN(irg,iyr) = PEL_IN / MC_JPGDP(iyr)
         HPELTR(irg,iyr) = PEL_TR / MC_JPGDP(iyr)
         HPELAS(irg,iyr) = PEL_AS / MC_JPGDP(iyr)

      end do

      deallocate(col)

!     QUERY THE DATABASE TABLE H_ELDATYR_ANN

      allocate ( col(6) )

      call sqlite3_column_query( col(001), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(002), 'HNUCHR', sqlite_real )
      call sqlite3_column_query( col(003), 'HGEOHR', sqlite_real )
      call sqlite3_column_query( col(004), 'HWODHR', sqlite_real )
      call sqlite3_column_query( col(005), 'HMSWHR', sqlite_real )
      call sqlite3_column_query( col(006), 'HFOSHR', sqlite_real )

      call sqlite3_prepare_select( db, 'H_ELDATYR_ANN', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), year)
         call sqlite3_get_column( col(002), T_NUC)
         call sqlite3_get_column( col(003), T_GEO)
         call sqlite3_get_column( col(004), T_WOD)
         call sqlite3_get_column( col(005), T_MSW)
         call sqlite3_get_column( col(006), T_FOS)

         iyr = year - UHBSYR

         WRITE(18,2009) CURIRUN, CURCALYR, year, iyr, HNUCHR(iyr), HGEOHR(iyr), HWODHR(iyr), HMSWHR(iyr), HFOSHR(iyr)
 2009    FORMAT(1X,"H_ELDATYR_ANN_OLD",4(",",I5),5(",",F21.6))

         WRITE(18,2010) CURIRUN, CURCALYR, year, iyr, T_NUC, T_GEO, T_WOD, T_MSW, T_FOS
 2010    FORMAT(1X,"H_ELDATYR_ANN_NEW",4(",",I5),5(",",F21.6))

         HNUCHR(iyr) = T_NUC
         HGEOHR(iyr) = T_GEO
         HWODHR(iyr) = T_WOD
         HMSWHR(iyr) = T_MSW
         HFOSHR(iyr) = T_FOS

      end do
!
!     Check for 0 nuclear heat rates
      DO YEAR = HSTYEAR , UYR_HIST
        IF ( HNUCHR(YEAR-UHBSYR) .EQ. 0.0 ) THEN
          IF ( YEAR .EQ. HSTYEAR ) THEN
            HNUCHR(YEAR-UHBSYR) = 14500.
          ELSE
            HNUCHR(YEAR-UHBSYR) = HNUCHR(YEAR-UHBSYR-1)
          ENDIF
        ENDIF
      ENDDO

!     In last historical year set ephtrt aer hrates to aer values.  for now all set to fossil fuel eq except nuke, wood, msw

      YEAR = UYR_HIST
      DO ICAP = 1 , ECP_D_CAP
        EPHTRT_AER(ICAP) = HFOSHR(YEAR-UHBSYR)
      ENDDO

      EPHTRT_AER(WICN) = HNUCHR(YEAR-UHBSYR)
      EPHTRT_AER(WIAN) = HNUCHR(YEAR-UHBSYR)
      EPHTRT_AER(WISM) = HNUCHR(YEAR-UHBSYR)
      EPHTRT_AER(WIWD) = HWODHR(YEAR-UHBSYR)
      EPHTRT_AER(WIBI) = HWODHR(YEAR-UHBSYR)
      EPHTRT_AER(WIMS) = HMSWHR(YEAR-UHBSYR)

      !    set hydro, geo, wind, solar to 3412 instead of fossil-fuel average (AEO2025 update)  
      EPHTRT_AER(WIGT) = 3412.00
      EPHTRT_AER(WIHY) = 3412.00
      EPHTRT_AER(WIPS) = 3412.00
      EPHTRT_AER(WIWN) = 3412.00
      EPHTRT_AER(WIWF) = 3412.00
      EPHTRT_AER(WIWL) = 3412.00
      EPHTRT_AER(WISO) = 3412.00
      EPHTRT_AER(WIPV) = 3412.00
      EPHTRT_AER(WIPT) = 3412.00

      deallocate(col)

!     QUERY THE DATABASE TABLE H_ELDATYR_CENSUS - FOSSIL HEAT RATE BY CENSUS REGION

      allocate ( col(3) )

      call sqlite3_column_query( col(001), 'CENSUS', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'HFOSHRCR', sqlite_real )

      call sqlite3_prepare_select( db, 'H_ELDATYR_CENSUS', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), T_FOS)

         iyr = year - UHBSYR
         
         WRITE(18,2764) CURIRUN, CURCALYR, irg, year, iyr, HFOSHRCR(irg,iyr)
 2764    FORMAT(1X,"H_ELDATYR_CENSUS_FOSHR_OLD",5(",",I5),1(",",F21.6))

         WRITE(18,2765) CURIRUN, CURCALYR, irg, year, iyr, T_FOS
 2765    FORMAT(1X,"H_ELDATYR_CENSUS_FOSHR_NEW",5(",",I5),1(",",F21.6))


         HFOSHRCR(irg,iyr) = T_FOS

      end do

      deallocate(col)
!
!     Set census region 10 and total region heat rates to national fossil fuel hr
!
      DO YEAR = HSTYEAR , UYR_HIST
        HFOSHRCR(10,YEAR-UHBSYR) = HFOSHR(YEAR-UHBSYR)
        HFOSHRCR(11,YEAR-UHBSYR) = HFOSHR(YEAR-UHBSYR)
      ENDDO
!
!     EMISSIONS

!     QUERY THE DATABASE TABLE H_ELDATYR_POLLUT

      allocate ( col(6) )

      call sqlite3_column_query( col(001), 'POLLUTANT', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'HEMNGTL', sqlite_real )
      call sqlite3_column_query( col(004), 'HEMOLTL', sqlite_real )
      call sqlite3_column_query( col(005), 'HEMCLTL', sqlite_real )
      call sqlite3_column_query( col(006), 'HEMOTTL', sqlite_real )

      call sqlite3_prepare_select( db, 'H_ELDATYR_POLLUT', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), POL)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), T_NG)
         call sqlite3_get_column( col(004), T_OL)
         call sqlite3_get_column( col(005), T_CL)
         call sqlite3_get_column( col(006), T_OT)

         iyr = year - UHBSYR

         WRITE(18,2011) CURIRUN, CURCALYR, year, iyr, POL, HEMNGTL(POL,iyr), HEMOLTL(POL,iyr), HEMCLTL(POL,iyr), HEMOTTL(POL,iyr)
 2011    FORMAT(1X,"H_ELDATYR_POLLUT_OLD",5(",",I5),4(",",F21.6))

         WRITE(18,2012) CURIRUN, CURCALYR, year, iyr, POL, T_NG, T_OL, T_CL, T_OT
 2012    FORMAT(1X,"H_ELDATYR_POLLUT_NEW",5(",",I5),4(",",F21.6))

         HEMNGTL(POL,iyr) = T_NG
         HEMOLTL(POL,iyr) = T_OL
         HEMCLTL(POL,iyr) = T_CL
         HEMOTTL(POL,iyr) = T_OT

      end do

      deallocate(col)
      
!     QUERY THE DATABASE TABLE V_EMM_ELDATYR2

      allocate ( col(14) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'HELRSNR', sqlite_real )
      call sqlite3_column_query( col(004), 'HELCMNR', sqlite_real )
      call sqlite3_column_query( col(005), 'HELINNR', sqlite_real )
      call sqlite3_column_query( col(006), 'HELTRNR', sqlite_real )
      call sqlite3_column_query( col(007), 'HELASNR', sqlite_real )
      call sqlite3_column_query( col(008), 'HEMSO2R', sqlite_real )
      call sqlite3_column_query( col(009), 'HEMNOXR', sqlite_real )
      call sqlite3_column_query( col(010), 'HEMCO2R', sqlite_real )
      call sqlite3_column_query( col(011), 'HTIMPF', sqlite_real )
      call sqlite3_column_query( col(012), 'HTIMPE', sqlite_real )
      call sqlite3_column_query( col(013), 'HTEXPF', sqlite_real )
      call sqlite3_column_query( col(014), 'HTEXPE', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_ELDATYR2', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), T_RS)
         call sqlite3_get_column( col(004), T_CM)
         call sqlite3_get_column( col(005), T_IN)
         call sqlite3_get_column( col(006), T_TR)
         call sqlite3_get_column( col(007), T_AS)
         call sqlite3_get_column( col(008), T_SO2)
         call sqlite3_get_column( col(009), T_NOX)
         call sqlite3_get_column( col(010), T_CO2)
         call sqlite3_get_column( col(011), T_IMPF)
         call sqlite3_get_column( col(012), T_IMPE)
         call sqlite3_get_column( col(013), T_EXPF)
         call sqlite3_get_column( col(014), T_EXPE)

         iyr = year - UHBSYR

         WRITE(18,2026) CURIRUN, CURCALYR, irg, year, iyr, HELRSNR(irg,iyr), HELCMNR(irg,iyr), HELINNR(irg,iyr), HELTRNR(irg,iyr), HELASNR(irg,iyr), &
            HEMSO2R(irg,iyr), HEMNOXR(irg,iyr), HEMCO2R(irg,iyr), HTIMPF(irg,iyr), HTIMPE(irg,iyr), HTEXPF(irg,iyr), HTEXPE(irg,iyr)
 2026    FORMAT(1X,"V_EMM_ELDATYR2_OLD",5(",",I5),12(",",F21.6))

         WRITE(18,2027) CURIRUN, CURCALYR, irg, year, iyr, T_RS, T_CM, T_IN, T_TR, T_AS, T_SO2, T_NOX, T_CO2, T_IMPF, T_IMPE, T_EXPF, T_EXPE
 2027    FORMAT(1X,"V_EMM_ELDATYR2_NEW",5(",",I5),12(",",F21.6))

         HELRSNR(irg,iyr) = T_RS
         HELCMNR(irg,iyr) = T_CM
         HELINNR(irg,iyr) = T_IN
         HELTRNR(irg,iyr) = T_TR
         HELASNR(irg,iyr) = T_AS
         HEMSO2R(irg,iyr) = T_SO2
         HEMNOXR(irg,iyr) = T_NOX
         HEMCO2R(irg,iyr) = T_CO2
         HTIMPF(irg,iyr) = T_IMPF
         HTIMPE(irg,iyr) = T_IMPE
         HTEXPF(irg,iyr) = T_EXPF
         HTEXPE(irg,iyr) = T_EXPE

         !HEMSO2R(MNUMNR,iyr) = HEMSO2R(MNUMNR,iyr) + HEMSO2R(irg,iyr)
         !HEMNOXR(MNUMNR,iyr) = HEMNOXR(MNUMNR,iyr) + HEMNOXR(irg,iyr)
         !HEMCO2R(MNUMNR,iyr) = HEMCO2R(MNUMNR,iyr) + HEMCO2R(irg,iyr)

      end do

      deallocate(col)

!
!     Set historical years expci values using historical Trade values
!
      DO YEAR = HSTYEAR , UYR_HIST
       IYR = YEAR - UHBSYR
       DO IRG = 1 , MNUMNR
        EXPCI(IYR,IRG) = ( HTEXPF(IRG,IYR) + HTEXPE(IRG,IYR) ) * 1000.0
       ENDDO
      ENDDO
!
!     Set forecast years expci values using average of last threes years and subtract contracts.              
!
      LHYEAR = UYR_HIST - UHBSYR
      DO IYR = LHYEAR + 1 , MNUMYR
        CALL GETEIJ(IYR)
        DO IRG = 1 , MNUMNR  
          EXPCI(IYR,IRG) = ((EXPCI(LHYEAR,IRG) + EXPCI(LHYEAR-1,IRG) + EXPCI(LHYEAR-2,IRG)) / 3.0) - ZTEXPF(IRG)
        ENDDO
      ENDDO

!     QUERY THE DATABASE TABLE H_ELDATYR_CENSUS  FOR SALES data by census region

      allocate ( col(7) )

      call sqlite3_column_query( col(001), 'CENSUS', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'HELRSCR', sqlite_real )
      call sqlite3_column_query( col(004), 'HELCMCR', sqlite_real )
      call sqlite3_column_query( col(005), 'HELINCR', sqlite_real )
      call sqlite3_column_query( col(006), 'HELTRCR', sqlite_real )
      call sqlite3_column_query( col(007), 'HELASCR', sqlite_real )

      call sqlite3_prepare_select( db, 'H_ELDATYR_CENSUS', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), T_RS)
         call sqlite3_get_column( col(004), T_CM)
         call sqlite3_get_column( col(005), T_IN)
         call sqlite3_get_column( col(006), T_TR)
         call sqlite3_get_column( col(007), T_AS)

         iyr = year - UHBSYR

         WRITE(18,2086) CURIRUN, CURCALYR, irg, year, iyr, HELRSNR(irg,iyr), HELCMNR(irg,iyr), HELINNR(irg,iyr), HELTRNR(irg,iyr), HELASNR(irg,iyr)
 2086    FORMAT(1X,"H_ELDATYR_CENSUS_SALES_OLD",5(",",I5),4(",",F21.6))

         WRITE(18,2087) CURIRUN, CURCALYR, irg, year, iyr, T_RS, T_CM, T_IN, T_TR, T_AS
 2087    FORMAT(1X,"H_ELDATYR_CENSUS_SALES_NEW",5(",",I5),4(",",F21.6))

         HELRSCR(irg,iyr) = T_RS
         HELCMCR(irg,iyr) = T_CM
         HELINCR(irg,iyr) = T_IN
         HELTRCR(irg,iyr) = T_TR
         HELASCR(irg,iyr) = T_AS

      end do

      deallocate(col)
!
!     CALCULATE NATIONAL PRICES BY CENSUS REGION
!
      DO YEAR = HSTYEAR , UYR_HIST
       IYR = YEAR - UHBSYR

       HELRSCR(MNUMCR,iyr) = 0.0
       HELCMCR(MNUMCR,iyr) = 0.0
       HELINCR(MNUMCR,iyr) = 0.0
       HELTRCR(MNUMCR,iyr) = 0.0
       HELASCR(MNUMCR,iyr) = 0.0
       HREVRSCR(MNUMCR,iyr) = 0.0
       HREVCMCR(MNUMCR,iyr) = 0.0
       HREVINCR(MNUMCR,iyr) = 0.0
       HREVTRCR(MNUMCR,iyr) = 0.0
       HREVASCR(MNUMCR,iyr) = 0.0
       HPELRS(MNUMCR,iyr) = 0.0
       HPELCM(MNUMCR,iyr) = 0.0
       HPELIN(MNUMCR,iyr) = 0.0
       HPELTR(MNUMCR,iyr) = 0.0
       HPELAS(MNUMCR,iyr) = 0.0
!
       DO IRG = 1 , MNUMCR - 1
         HELRSCR(MNUMCR,iyr) = HELRSCR(MNUMCR,IYR) + HELRSCR(IRG,IYR)
         HELCMCR(MNUMCR,iyr) = HELCMCR(MNUMCR,IYR) + HELCMCR(IRG,IYR)
         HELINCR(MNUMCR,iyr) = HELINCR(MNUMCR,IYR) + HELINCR(IRG,IYR)
         HELTRCR(MNUMCR,iyr) = HELTRCR(MNUMCR,IYR) + HELTRCR(IRG,IYR)
         HELASCR(MNUMCR,iyr) = HELASCR(MNUMCR,IYR) + HELASCR(IRG,IYR)
         HREVRSCR(MNUMCR,iyr) = HREVRSCR(MNUMCR,IYR) + HREVRSCR(IRG,IYR)
         HREVCMCR(MNUMCR,iyr) = HREVCMCR(MNUMCR,IYR) + HREVCMCR(IRG,IYR)
         HREVINCR(MNUMCR,iyr) = HREVINCR(MNUMCR,IYR) + HREVINCR(IRG,IYR)
         HREVTRCR(MNUMCR,iyr) = HREVTRCR(MNUMCR,IYR) + HREVTRCR(IRG,IYR)
         HREVASCR(MNUMCR,iyr) = HREVASCR(MNUMCR,IYR) + HREVASCR(IRG,IYR)
       ENDDO
!
       HPELRS(MNUMCR,iyr) = HREVRSCR(MNUMCR,IYR) / HELRSCR(MNUMCR,IYR) / MC_JPGDP(iyr)
       HPELCM(MNUMCR,iyr) = HREVCMCR(MNUMCR,IYR) / HELCMCR(MNUMCR,IYR) / MC_JPGDP(IYR)
       HPELIN(MNUMCR,iyr) = HREVINCR(MNUMCR,IYR) / HELINCR(MNUMCR,IYR) / MC_JPGDP(IYR)
       HPELTR(MNUMCR,iyr) = HREVTRCR(MNUMCR,IYR) / HELTRCR(MNUMCR,IYR) / MC_JPGDP(IYR)
       HPELAS(MNUMCR,iyr) = HREVASCR(MNUMCR,IYR) / HELASCR(MNUMCR,IYR) / MC_JPGDP(IYR)

      ENDDO  

!     QUERY THE DATABASE TABLE V_EMM_ELDATYR_COGEN_2

      allocate ( col(5) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'CGF', sqlite_int )
      call sqlite3_column_query( col(004), 'HCNTGEN', sqlite_real )
      call sqlite3_column_query( col(005), 'IGRID', sqlite_int )

      call sqlite3_prepare_select( db, 'V_EMM_ELDATYR_COGEN1', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), cgf)
         call sqlite3_get_column( col(004), TEMP)
         call sqlite3_get_column( col(005), igrid)

         iyr = year - UHBSYR

         WRITE(18,2008) CURIRUN, CURCALYR, irg, year, iyr, cgf, igrid, TEMP, HCNTGEN(irg,iyr,cgf,igrid)
 2008    FORMAT(1X,"V_EMM_ELDATYR_COGEN_1",7(",",I5),2(",",F21.6))

         HCNTGEN(irg,iyr,cgf,igrid) = TEMP

      end do

      deallocate(col)

!     non-trad cogen by plant type

!     QUERY THE DATABASE TABLE V_EMM_ELDATYR_COGEN_2

      allocate ( col(4) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'CGF', sqlite_int )
      call sqlite3_column_query( col(004), 'HCNTQNR', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_ELDATYR_COGEN2', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), cgf)
         call sqlite3_get_column( col(004), TEMP)

         iyr = year - UHBSYR

         WRITE(18,2003) CURIRUN, CURCALYR, irg, year, iyr, cgf, TEMP, HCNTQNR(irg,iyr,cgf)
 2003    FORMAT(1X,"V_EMM_ELDATYR_COGEN_2",6(",",I5),2(",",F21.6))

         HCNTQNR(irg,iyr,cgf) = TEMP

      end do

      deallocate(col)

!     non-trad cogen fuel consumptions by fuel type and census region.

!     QUERY THE DATABASE TABLE V_EMM_ELDATYR_COGEN_3

      allocate ( col(4) )

      call sqlite3_column_query( col(001), 'CENSUS', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'CGF', sqlite_int )
      call sqlite3_column_query( col(004), 'HCNTQCR', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_ELDATYR_COGEN3', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), cgf)
         call sqlite3_get_column( col(004), TEMP)

         iyr = year - UHBSYR

         WRITE(18,2093) CURIRUN, CURCALYR, irg, year, iyr, cgf, TEMP, HCNTQCR(irg,iyr,cgf)
 2093    FORMAT(1X,"V_EMM_ELDATYR_COGEN_3",6(",",I5),2(",",F21.6))

         HCNTQCR(irg,iyr,cgf) = TEMP

      end do

      deallocate(col)


!     QUERY THE DATABASE TABLE V_EMM_ELDATYR_GRIDOWN

      allocate ( col(5) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'GRIDOWN', sqlite_int )
      call sqlite3_column_query( col(004), 'IFL', sqlite_int )
      call sqlite3_column_query( col(005), 'HCIPGEN', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_ELDATYR_GRIDOWN', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), igrid)
         call sqlite3_get_column( col(004), ifl)
         call sqlite3_get_column( col(005), TEMP)

         iyr = year - UHBSYR

         WRITE(18,2001) CURIRUN, CURCALYR, irg, year, iyr, igrid, ifl, TEMP, HCIPGEN(irg,iyr,ifl,igrid)
 2001    FORMAT(1X,"V_EMM_ELDATYR_GRIDOWN",7(",",I5),2(",",F21.6))

         HCIPGEN(irg,iyr,ifl,igrid) = TEMP

      end do

      deallocate(col)

!     QUERY THE DATABASE TABLE V_EMM_ELDATYR_COGEN_MAP

      allocate ( col(5) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'CEN_REG', sqlite_int )
      call sqlite3_column_query( col(003), 'CGF', sqlite_int )
      call sqlite3_column_query( col(004), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(005), 'MPCGCtoN', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_ELDATYR_COGEN_MAP', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), crg)
         call sqlite3_get_column( col(003), cgf)
         call sqlite3_get_column( col(004), year)
         call sqlite3_get_column( col(005), TEMP)

         iyr = year - UHBSYR

         WRITE(18,2002) CURIRUN, CURCALYR, irg, year, iyr, crg, cgf, TEMP, MPCGCtoN(irg,crg,cgf)
 2002    FORMAT(1X,"V_EMM_ELDATYR_COGEN_MAP",7(",",I5),2(",",F21.6))

         MPCGCtoN(irg,crg,cgf) = TEMP

      end do

      deallocate(col)

      call sqlite3_close( db )

!
!     fill and convert nuclear costs to $/MMBTU
!
      DO YEAR = 1, MNUMYR
          UPURELN(MNUMNR,YEAR) = UPURELN(MNUMNR,YEAR) / (HNUCHR(UYR_HIST-UHBSYR)/1000.0)
       DO NERC = 1 , MNUMNR - 1
          UPURELN(NERC,YEAR) = UPURELN(MNUMNR,YEAR) 
       END DO
       DO CEN = 1, MNUMCR
          PUREL(CEN,YEAR) = UPURELN(MNUMNR,YEAR) 
       ENDDO
      ENDDO
!
!     EMISSIONS

      DO YEAR = HSTYEAR , UYR_HIST
       BYEAR = YEAR - UHBSYR
 
 !  added for flexreg process - fill in national emission values that were previously filled in overwrites process
!
!   CARBON = CO2 * .2727
         HEMNGTL(1,BYEAR) = HEMNGTL(3,BYEAR) * .2727              
         HEMOLTL(1,BYEAR) = HEMOLTL(3,BYEAR) * .2727             
         HEMCLTL(1,BYEAR) = HEMCLTL(3,BYEAR) * .2727            
         HEMOTTL(1,BYEAR) = HEMOTTL(3,BYEAR) * .2727           
!
!    CO1 and VOC not available in EPA emissions data so filling in with old estimated values
         HEMNGTL(2,BYEAR) = .01                                    
         HEMOLTL(2,BYEAR) = .03                                   
         HEMCLTL(2,BYEAR) = .25                                  
         HEMOTTL(2,BYEAR) = .04                                 
         HEMNGTL(6,BYEAR) = .01                                    
         HEMOLTL(6,BYEAR) = .00                                   
         HEMCLTL(6,BYEAR) = .03                                  
         HEMOTTL(6,BYEAR) = .00                                 
    
      END DO                                                               ! end emissions year loop
!

!
!  SET PV SHARES EQUAL TO the average of the resid and commercial sector sales shares  
!
     DO ICENSUS = 1 , MNUMCR - 2
       DO INERC = 1 , MNUMNR - 1
         MPCGCToN(INERC,ICENSUS,10) = ((MAPPCTON(INERC,ICENSUS,1) + MAPPCTON(INERC,ICENSUS,2)) / 2.)
       ENDDO
     ENDDO
!
!  SUM GENERATION BY CENSUS AND FUEL
!
    SUMCOGCEN = 0.0
    DO IFUEL = 1 , TC_FUELS
      DO INERC = 1 , MNUMNR - 3 
        DO ICENSUS = 1 , MNUMCR - 1
          SUMCOGCEN(IFUEL,ICENSUS) = SUMCOGCEN(IFUEL,ICENSUS) + MPCGCToN(INERC,ICENSUS,IFUEL)               
        ENDDO
      ENDDO
    ENDDO
!
!   check to see if there are no shares set for a census region, i.e. no generation for a fuel type in a census region
!    if so set shares equal to current transportation (total) sales shares
!    do not overwrite pv shares from above
!
     DO IFUEL = 1 , TC_FUELS
      IF (IFUEL .NE. 10) THEN
       DO ICENSUS = 1 , MNUMCR - 2
         IF ( SUMCOGCEN(IFUEL,ICENSUS) .EQ. 0.0 ) THEN
           DO INERC = 1 , MNUMNR - 3 
             MPCGCToN(INERC,ICENSUS,IFUEL) = MAPPCTON(INERC,ICENSUS,4)
           ENDDO
         ENDIF
       ENDDO
      ENDIF
     ENDDO
!
!    Check if there are hawaii/alaska shares and recalculate if so
!
     SUMCOGCEN = 0.0
!
     DO IFUEL = 1 , TC_FUELS
       DO ICENSUS = 1, MNUMCR - 2
        IF ( (MPCGCTon(MNUMNR-2,ICENSUS,IFUEL) .GT. 0.0) .OR. (MPCGCTon(MNUMNR-1,ICENSUS,IFUEL) .GT. 0.0 ) ) THEN
          MPCGCTon(MNUMNR-2,ICENSUS,IFUEL) = 0.0
          MPCGCTon(MNUMNR-1,ICENSUS,IFUEL) = 0.0
          DO INERC = 1 , MNUMNR - 3
            SUMCOGCEN(IFUEL,ICENSUS) = SUMCOGCEN(IFUEL,ICENSUS) + MPCGCToN(INERC,ICENSUS,IFUEL)               
          ENDDO
          DO INERC = 1 , MNUMNR - 3
            MPCGCTon(INERC,ICENSUS,IFUEL) = MPCGCTon(INERC,ICENSUS,IFUEL) * (1.0 / SUMCOGCEN(IFUEL,ICENSUS))
          ENDDO
        ENDIF
       ENDDO
      ENDDO
!
!     UF_TMP = FILE_MGR('C',FILENM,NEW)

      CLOSE (UF_TMP)
!
      RETURN
      END
!
      SUBROUTINE RENOVR(NERC)
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE REPLACES EMM INTERMITTENT/RENEWABLE TECHNOLOGY VALUES WITH VALUES FROM RENEW
!     THE RENEW MODULE HAS NO EFFECT ON STORAGE TECHNOLOGIES
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'bildin'
      include 'control'
      include 'ecpcntl'
      include 'wrenew'
      include 'wwdcomon'
      include 'uefdout'
      include 'dsmdimen'
      include 'dsmcaldr'

      INTEGER*4   NERC,INT,RNW,ICAP                       ! EMM INDICES
      INTEGER*4   ITECH,RTECH                             ! INDEX IN RENEW COMMON FOR EMM TECH
      INTEGER*4   FULLYR,RYR,PYR,BYR                      ! YEAR INDICES
      INTEGER*4   IL,ISP,ISTP,LDGRP
      INTEGER*4   DAYS(12),IMO,IDTYP,IECP,IHR
      REAL*8      MOTOESP(0:ECP_D_MSP,12)
      REAL*8      SCAPCF(ECP_D_MSP),HCAPCF,HYSCAPF(ECP_D_MSP),HRS_ECP(0:ECP_D_MSP)
      REAL*8      VALUE
      CHARACTER*1 BNDMAP(23)                                           !
      REAL*8 AVG_HTRT(0:ECP_D_CAP), AVG_HTRT_MR(0:ECP_D_CAP), AVG_HTRT_MOD(0:ECP_D_CAP), AVG_HTRT_MR_MOD(0:ECP_D_CAP)
      REAL*8 T_ECP_GEN(0:ECP_D_CAP), T_ECP_GEN_MR(0:ECP_D_CAP), T_ECP_GEN_MOD(0:ECP_D_CAP), T_ECP_GEN_MR_MOD(0:ECP_D_CAP)
      CHARACTER*12 FROM_LABEL

      INTEGER FILE_MGR,RTOVALUE
      EXTERNAL FILE_MGR,RTOVALUE

      DATA BNDMAP /'L','G','E',' ','Z',' ',' ',' ',' ',' ', &
       'L','G','E',' ','Z',' ',' ',' ',' ',' ','L','G','E'/

      FULLYR = USYEAR(CURIYR)

      IF (CURIYR .EQ. FIRSYR) THEN     ! OPEN FILE AND INITIALIZE IN 1ST YEAR
         RYR = CURIYR
      ELSE
         RYR = CURIYR - 1
      ENDIF

      DAYS = 0
      DO IMO = 1 , 12
         DO IDTYP = 1 , 3
            DAYS(IMO) = DAYS(IMO) + IDAYTQ(IDTYP,IMO)
         END DO
      END DO

      MOTOESP = 0.0
      DO IHR = 1 , 24
         DO IDTYP = 1 , 3
            DO IMO = 1 , 12
               IECP = HrToECPSeas(IMO,IDTYP,IHR)
               MOTOESP(0,IMO) = MOTOESP(0,IMO) + IDAYTQ(IDTYP,IMO)
               MOTOESP(IECP,IMO) = MOTOESP(IECP,IMO) + IDAYTQ(IDTYP,IMO)
            END DO
         END DO
      END DO

!     DETERMINE HOURS PER ECP SEASONAL PERIOD

      HRS_ECP = 0.0
      DO IHR = 1 , 24
         DO IDTYP = 1 , 3
            DO IMO = 1 , 12
               IECP = HrToECPSeas(IMO,IDTYP,IHR)
               HRS_ECP(0) = HRS_ECP(0) + IDAYTQ(IDTYP,IMO)
               HRS_ECP(IECP) = HRS_ECP(IECP) + IDAYTQ(IDTYP,IMO)
            END DO
         END DO
      END DO

!     INITIALIZE EPBNDTYP

      EPBNDTYP = " "
!CC
!CC   OVERWRITE RENEWABLE DATA WITH RENEW COMMON
!CC
      DO RNW = 1 , ECP_D_RNW

!         WRITE(6,1377) CURIYR+1989, CURITR, RNW, UCPRNWI(RNW), UIRRNWI(RNW), ECP_D_RNW
 1377    FORMAT(1X,"P2_HUH_1",6(":",I4))

         IF (UIRRNWI(RNW) .GT. 0) THEN

 !          WRITE(6,2377) CURIYR+1989, CURITR, RNW, UCPRNWI(RNW), UIRRNWI(RNW), UPLNTIDX(UIRRNWI(RNW)), UPLNTCD(UCPRNWI(RNW))
 2377       FORMAT(1X,"P2_HUH_2",6(":",I4),":",A2)

            RTECH = UPLNTIDX(UIRRNWI(RNW))
            ICAP = UCPRNWI(RNW)
            IF (UPLNTCD(UCPRNWI(RNW)).EQ.'P2') THEN
               EPRCFC(RNW) = UPMCF(ICAP)
               EPSTSUP(UIRRNWI(RNW))  = 1
               EPIRCCR(UIRRNWI(RNW))  = 1.0
               DO PYR = 1 , UNXPH
                  EPCCSUP(UIRRNWI(RNW),PYR)=1.0

!                 WRITE(6,3755) CURIRUN, CURIYR+1989, CURITR, NERC, UIRRNWI(RNW), PYR, EPCCSUP(UIRRNWI(RNW),PYR)
!3755             FORMAT(1X,"EPCCSUP_RENOVR",6(":",I4),":",F21.6)

                  DO ISP = 1 , EPNMSP
                     EPESCFC(ISP,UCPRNWI(RNW),PYR) = EPRCFC(RNW)
                  END DO
               END DO
               EPBNDTYP(UIRRNWI(RNW)) = "2"

!              WRITE(6,3377) CURIYR+1989,CURITR,RNW,RTECH,UCPRNWI(RNW),UIRRNWI(RNW),EPRCFC(RNW),EPCCSUP(UIRRNWI(RNW),1),EPBNDTYP(UIRRNWI(RNW))
!3377          FORMAT(1X,"EPRCFC",6(":",I4),2(":",F6.3),":",A1)

            END IF

!CC
!CC         EXIT IF TECHNOLOGY IN EMM IS NOT IN RENEW COMMON
!CC
            IF (RTECH .GT. 0) THEN

               DO PYR = 1 , UNXPH
                  EPBLDBND(UIRRNWI(RNW),PYR) = 0.0
               ENDDO

!               WRITE(6,*) 'rtech ', RTECH
!               WRITE(6,*) 'nerc',NERC
!               WRITE(6,*) 'wbtech',WBTECH(RTECH,NERC)

               IF (WBTECH(RTECH,NERC).gt.0 .and.WBTECH(RTECH,NERC) .lt. 21) &
                    EPBNDTYP(UIRRNWI(RNW)) = BNDMAP(WBTECH(RTECH,NERC) + 1)                  !!!!!!!!!!!!!!!DGR!!!!Temporary fix!!!!!!!!

               IF(MSPTMX.LE.3)THEN

!                 SUPPLY CURVE ELASTICITY SWITCH

                  EPSWSUP(UIRRNWI(RNW)) = UTCSSW(RTECH)

!                 NUMBER OF STEPS IN SUPPLY CURVE

!                 IF(UTCSSW(RTECH).GT.0)THEN

                  IF (ESTSWTCH(ICAP) .LT. 0)THEN
                     EPSTSUP(UIRRNWI(RNW)) = MSPTMX
                  ELSE
                     EPSTSUP(UIRRNWI(RNW)) = MAX(1,ESTSWTCH(ICAP))
                  END IF

!                 ELSE
!                 EPSTSUP(UIRRNWI(RNW)) = 1
!                 END IF

                  DO PYR = 1 , EPSTSUP(UIRRNWI(RNW))

!                    COST MULTIPLIERS AND BOUNDS FOR RENEWABLE TECHNOLOGY SUPPLY CURVES
!                    REGIONAL CURVES

!                    IF(EPSWSUP(UIRRNWI(RNW)) .GT. MSPTMX)THEN

                     IF(ESTSWTCH(ICAP) .LT. 0)THEN
                        EPCCSUP(UIRRNWI(RNW),PYR) = UTCSFN(NERC,RTECH,PYR)

!                       WRITE(6,3755) CURIRUN, CURIYR+1989, CURITR, NERC, UIRRNWI(RNW), PYR, EPCCSUP(UIRRNWI(RNW),PYR)

                        EPFXSUP(UIRRNWI(RNW),PYR) = UTFXFN(NERC,RTECH,PYR)
                        EPVRSUP(UIRRNWI(RNW),PYR) = UTVRFN(NERC,RTECH,PYR)
                        EPBDSUP(UIRRNWI(RNW),PYR) = UTCAFNR(NERC,RTECH,PYR) * 0.001

!                       ELASTICITIES -- LONG-TERM ONLY (ST DONE IN ECP)

                     ELSE
                        IF (ICAP .EQ. WIGT) THEN
                           EPCCSUP(UIRRNWI(RNW),PYR) = ESTCPCST(ICAP,PYR)

!                          WRITE(6,3755) CURIRUN, CURIYR+1989, CURITR, NERC, UIRRNWI(RNW), PYR, EPCCSUP(UIRRNWI(RNW),PYR)

                        ELSE
                           EPCCSUP(UIRRNWI(RNW),PYR) = UTCSFN(NERC,RTECH,PYR) + ESTCPCST(ICAP,PYR)

!                          WRITE(6,3755) CURIRUN, CURIYR+1989, CURITR, NERC, UIRRNWI(RNW), PYR, EPCCSUP(UIRRNWI(RNW),PYR)

                        ENDIF
                        EPFXSUP(UIRRNWI(RNW),PYR) = 1.0
                        EPVRSUP(UIRRNWI(RNW),PYR) = 1.0

!                       EPBDSUP(UIRRNWI(RNW),PYR) = UTCAFN(RTECH,PYR) * 0.001

                        EPBDSUP(UIRRNWI(RNW),PYR) = ESTCPLIM(ICAP,PYR) * 0.001
                     END IF
                  ENDDO
               ELSE
                  PRINT *,'PARAMETER MSPTMX EXCEEDS 3'
                  STOP
               END IF
!
               IF (UPLNTCD(UCPRNWI(RNW)).EQ.'HY') THEN

!                 if building hydro using new hydro supply curves then overwrite info with supply curve calculations

                  IF (ESTSWTCH(ICAP) .LT. 0) THEN

!                    Calculate seasonal, and average hydro capacity factors
!                    from renewable module monthly hydro capacity factors

                     DO ISTP = 1 , MSPTMX
                        SCAPCF = 0.0
                        HCAPCF = 0.0
                        DO ISP = 1 , EPNMSP
                           DO IMO = 1 , 12
                              SCAPCF(ISP) = SCAPCF(ISP) + WMFHYEL(NERC,ISTP,IMO,RYR) * MOTOESP(ISP,IMO)
                              HCAPCF = HCAPCF + WMFHYEL(NERC,ISTP,IMO,RYR) * MOTOESP(ISP,IMO)
                           END DO
                        END DO
                        IF ( ISTP .EQ. 1 ) THEN
                           EPRCFC(RNW) = HCAPCF / HRS_ECP(0)
                        ENDIF
                        DO ISP = 1 , EPNMSP
                           HYSCAPF(ISP) = SCAPCF(ISP) / HRS_ECP(ISP)
                        END DO

!CC                     Map Hydro Seasonal Capacity Factors to ECP Load Groups.

                        DO IL = 1,EPNSTP(1)
                           LDGRP = EPLDGR(IL,1)
                           ISP = EPGECP(LDGRP)
                           EPRSCFC(RNW,ISTP,ISP) = HYSCAPF(ISP)
						   IF (IL .EQ. 1) THEN
!CC                     Set Hydro Capacity Credit to Capacity Factor of peak load group.
								EPIRCCR(UIRRNWI(RNW)) = EPRSCFC(RNW,1,ISP)
								EPIRCCS(UIRRNWI(RNW),ISTP) = EPRSCFC(RNW,ISTP,ISP)
						   ENDIF
						   
                        ENDDO



                     ENDDO

!CC                  set other parameters equal to value for 1st step in supply curve.

                     EPIROVR(UIRRNWI(RNW)) = &
                        WCCHYEL(NERC,RYR) * (1.0 - UPCSB(UCPRNWI(RNW)))
                     EPIRFOM(UIRRNWI(RNW)) = WOCHYEL(NERC,RYR) * (1.0 - UPOMPCT)
                     IF (WVCHYEL(NERC,RYR) .LT. 900.0)THEN
                        EPIRVOM(UIRRNWI(RNW)) = WVCHYEL(NERC,RYR)
                     ELSE
                        EPIRVOM(UIRRNWI(RNW)) = UPVOM(WIHY)
                     END IF
                     EPBLDBND(UIRRNWI(RNW),1) = WCAHYEL(NERC,RYR) * .001

                  ENDIF                           !  end if using supply curves

!                 GEOTHERMAL  C

               ELSE IF (UPLNTCD(UCPRNWI(RNW)).EQ.'GT') THEN

!C                USE RFM GEOTHERMAL CAPITAL COSTS AND FIXED O&M COSTS
!C                ACCORDING TO RUN TIME OVERRIDE SWITCH - RENEWGE
!                    RENEWGE = 2 - USE ECPDAT CAPITAL COSTS
!                    RENEWGE = 3 - USE ECPDAT FIXED O&M COSTS
!                    RENEWGE = 4 - USE ECPDAT CAPITAL AND FIXED O&M COSTS
!                 ALWAYS USE RFM HEAT RATES AND EMISSIONS.

                  IF ((RTOVALUE('RENEWGE ',0) .NE. 2) .AND.   &
                     (RTOVALUE('RENEWGE ',0) .NE. 4)) THEN
                     EPIROVR(UIRRNWI(RNW)) = &
                        WCCGPEL(NERC,RYR) * (1.0 - UPCSB(UCPRNWI(RNW)))
                  ENDIF
                  IF ((RTOVALUE('RENEWGE ',0) .NE. 3) .AND.   &
                     (RTOVALUE('RENEWGE ',0) .NE. 4)) THEN
                     EPIRFOM(UIRRNWI(RNW)) = WOCGPEL(NERC,RYR) * (1.0 - UPOMPCT)
                  ENDIF

                  EPRCFC(RNW) = WCFGPEL(NERC,RYR)
!
                  EPCRBRT(UIRRNWI(RNW)) = WEMGPEL(1,NERC,RYR)
                  EPCO1RT(UIRRNWI(RNW)) = WEMGPEL(2,NERC,RYR)
                  EPCO2RT(UIRRNWI(RNW)) = WEMGPEL(3,NERC,RYR)
                  EPCH4RT(UIRRNWI(RNW)) = WEMGPEL(7,NERC,RYR)
                  EPSOXRT(UIRRNWI(RNW)) = WEMGPEL(4,NERC,RYR)
                  EPNOXRT(UIRRNWI(RNW)) = WEMGPEL(5,NERC,RYR)
                  EPVOCRT(UIRRNWI(RNW)) = WEMGPEL(6,NERC,RYR)

!CC               CALCULATE AVAILABLE CAPACITY TO BUILD

                  IF (EPBNDTYP(UIRRNWI(RNW)) .NE. 'Z') THEN
                     DO PYR = 1 , (UNXPH - UPPLYR(UCPRNWI(RNW)))
                        IF ((RYR+PYR-1) .GT. UNYEAR) THEN
                           BYR = UNYEAR
                        ELSE
                            BYR = RYR + PYR - 1
                        ENDIF
                        IF (WBTECH(RTECH,NERC) .LT. 10) THEN
                           VALUE = (WCAGPEL(NERC,BYR) -  &
                              EPECAP(0,UCPRNWI(RNW),PYR)) * .001
                        ELSE
                           VALUE = (WCAGPEL(NERC,BYR) * .001)
                        ENDIF

                        IF ((VALUE .LT. 0.0) .AND. (FULLYR .GE. UPSTYR)) THEN
                           WRITE(13,400) NERC,CURIYR,PYR,UPLNTCD(UCPRNWI(RNW)), &
                              WCAGPEL(NERC,BYR),EPECAP(0,UCPRNWI(RNW),PYR)
                           VALUE = 0.0
                        ENDIF

                        EPBLDBND(UIRRNWI(RNW),PYR) = VALUE

                        IF (UF_DBG .GT. 0) THEN
                           WRITE(13,410) NERC,RNW,CURIYR,PYR, &
                              UPLNTCD(UCPRNWI(RNW)), &
                              WCAGPEL(NERC,BYR),EPECAP(0,UCPRNWI(RNW),PYR), &
                              EPBLDBND(UIRRNWI(RNW),PYR),EPBNDTYP(UIRRNWI(RNW))
                        ENDIF
                     ENDDO
                  ENDIF

                  EPBLDBND(UIRRNWI(RNW),1) = WCAGPEL(NERC,RYR) * .001

!              BIOMASS  C

               ELSEIF (UPLNTCD(UCPRNWI(RNW)).EQ.'WD' .OR. UPLNTCD(UCPRNWI(RNW)).EQ.'BI') THEN

!C               IF BIOMASS VALUES BEING USED FROM ECPDAT DO NOT OVERWRITE

                 IF (RTOVALUE('RENEWBM ',0) .NE. 2) THEN
                    EPIRVOM(UIRRNWI(RNW)) = WVCBMEL(RYR)
                 ENDIF

!CC              CALCULATE AVAILABLE CAPACITY TO BUILD

                 IF (EPBNDTYP(UIRRNWI(RNW)) .NE. 'Z') THEN
                    DO PYR = 1 , (UNXPH - UPPLYR(UCPRNWI(RNW)))
                       IF ((RYR+PYR-1) .GT. UNYEAR) THEN
                          BYR = UNYEAR
                       ELSE
                          BYR = RYR + PYR - 1
                       ENDIF
                       IF (WBTECH(RTECH,NERC) .LT. 10) THEN
                          VALUE = (WCABMEL(NERC,BYR) - &
                             EPECAP(0,UCPRNWI(RNW),PYR)) * .001
                       ELSE
                          VALUE = (WCABMEL(NERC,BYR) * .001)
                       ENDIF

                       IF ((VALUE .LT. 0.0) .AND. (FULLYR .GE. UPSTYR)) THEN
                          WRITE(13,400) NERC,CURIYR,PYR,UPLNTCD(UCPRNWI(RNW)), &
                             WCABMEL(NERC,BYR),EPECAP(0,UCPRNWI(RNW),PYR)
                          VALUE = 0.0
                       ENDIF

                       EPBLDBND(UIRRNWI(RNW),PYR) = VALUE

                       IF (UF_DBG .GT. 0) THEN
                          WRITE(13,410) NERC,RNW,CURIYR,PYR, &
                             UPLNTCD(UCPRNWI(RNW)), &
                             WCABMEL(NERC,BYR),EPECAP(0,UCPRNWI(RNW),PYR), &
                             EPBLDBND(UIRRNWI(RNW),PYR),EPBNDTYP(UIRRNWI(RNW))
                        ENDIF
                     ENDDO
                  ENDIF

!              MSW   C

               ELSEIF (UPLNTCD(UCPRNWI(RNW)).EQ.'MS') THEN

!C                IF MSW VALUES BEING USED FROM ECPDAT DO NOT OVERWRITE

                  IF (RTOVALUE('RENEWMS ',0) .NE. 2) THEN
                     EPIRVOM(UIRRNWI(RNW)) = WVCMSEL(NERC,RYR)
                  ENDIF

!CC               CALCULATE AVAILABLE CAPACITY TO BUILD

                  IF (EPBNDTYP(UIRRNWI(RNW)) .NE. 'Z') THEN
                     DO PYR = 1 , (UNXPH - UPPLYR(UCPRNWI(RNW)))
                        IF ((RYR+PYR-1) .GT. UNYEAR) THEN
                           BYR = UNYEAR
                        ELSE
                          BYR = RYR + PYR - 1
                        ENDIF
                        IF (WBTECH(RTECH,NERC) .LT. 10) THEN
                           VALUE = (WCAMSEL(NERC,BYR) -  &
                              EPECAP(0,UCPRNWI(RNW),PYR)) * .001
                        ELSE
                           VALUE = (WCAMSEL(NERC,BYR) * .001)
                        ENDIF

                        IF ((VALUE .LT. 0.0) .AND. (FULLYR .GE. UPSTYR)) THEN
                           WRITE(13,400) NERC,CURIYR,PYR,UPLNTCD(UCPRNWI(RNW)), &
                              WCAMSEL(NERC,BYR),EPECAP(0,UCPRNWI(RNW),PYR)
                           VALUE = 0.0
                        ENDIF

                        EPBLDBND(UIRRNWI(RNW),PYR) = VALUE

                        IF (UF_DBG .GT. 0) THEN
                           WRITE(13,410) NERC,RNW,CURIYR,PYR, &
                              UPLNTCD(UCPRNWI(RNW)), &
                              WCAMSEL(NERC,BYR),EPECAP(0,UCPRNWI(RNW),PYR), &
                              EPBLDBND(UIRRNWI(RNW),PYR),EPBNDTYP(UIRRNWI(RNW))
                        ENDIF
                     ENDDO
                  ENDIF

!              PUMP STORAGE

               ELSEIF (UPLNTCD(UCPRNWI(RNW)).EQ.'PS') THEN

!              OTHER STORAGE

               ELSEIF (UPLNTCD(UCPRNWI(RNW)).EQ.'P2') THEN

               ENDIF
            ENDIF

            IF (UF_DBG .GT. 0) THEN

               FROM_LABEL = "RENOVR_RNW"
               CALL ECP_AVG_HTRT(FROM_LABEL, NERC, 0, UCPRNWI(RNW), 1, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, T_ECP_GEN, T_ECP_GEN_MR, T_ECP_GEN_MOD, T_ECP_GEN_MR_MOD)

               WRITE(13,411) UPFORT(UCPRNWI(RNW)),UPPMRT(UCPRNWI(RNW)), &
                  UPAVLYR(UCPRNWI(RNW)),UPCSB(UCPRNWI(RNW)), &
                  UPVSB(UCPRNWI(RNW))
               WRITE(13,412) UPCLYR(UCPRNWI(RNW)),UPECLF(UCPRNWI(RNW)), &
                  EPIROVR(UIRRNWI(RNW)),EPIRVOM(UIRRNWI(RNW)), &
                  EPIRFOM(UIRRNWI(RNW))
               WRITE(13,413) AVG_HTRT(UCPRNWI(RNW)),EPRCFC(RNW), &
                  EPIRCCR(UIRRNWI(RNW)),EPCRBRT(UIRRNWI(RNW)), &
                  EPCO1RT(UIRRNWI(RNW))
               WRITE(13,414) EPCO2RT(UIRRNWI(RNW)),EPCH4RT(UIRRNWI(RNW)), &
                  EPSOXRT(UIRRNWI(RNW)),EPNOXRT(UIRRNWI(RNW)), &
                  EPVOCRT(UIRRNWI(RNW))
            END IF
  411       FORMAT(3X,' RENOVR: UPFORT=',G11.3,' UPPMRT=',G11.3, &
            ' UPAVLYR=',I5,' UPCSB=',G11.3,' UPVSB=',G11.3)
  412       FORMAT(3X,' RENOVR: UPCLYR=',G11.3,' UPECLF=',G11.3, &
            ' EPIROVR=',G11.3,' EPIRVOM=',G11.3,' EPIRFOM=',G11.3)
  413       FORMAT(3X,' RENOVR: AVG_HTRT=',G11.3,' EPRCFC=',G11.3, &
            ' EPIRCCR=',G11.3,' EPCRBRT=',G11.3,' EPCO1RT=',G11.3)
  414       FORMAT(3X,' RENOVR: EPCO2RT=',G11.3,' EPCH4RT=',G11.3, &
            ' EPSOXRT=',G11.3,' EPNOXRT=',G11.3,' EPVOCRT=',G11.3)

         END IF
      END DO                                            ! END RENEWABLES

!CC   OVERWRITE INTERMITTENT DATA WITH RENEWABLE COMMON DATA
!CC   NOTE: MAX CAPACITY FACTORS FOR INTERMITTENTS WILL BE OVERWRITTEN IN CFCOVR.

      DO 200 INT = 1 , ECP_D_INT
         ICAP = UCPINTI(INT)
         ITECH = UPLNTIDX(UIRINTI(INT))

!CC      EXIT IF TECHNOLOGY IN EMM IS NOT IN RENEW COMMON

         IF (ITECH .EQ. 0) GOTO 200

            IF (WBTECH(ITECH,NERC).gt.0 .and.WBTECH(ITECH,NERC) .lt. 21) &
               WRITE(18,4417) CURIRUN, CURIYR+1989, NERC, INT, ICAP, ITECH, WBTECH(ITECH,NERC), UIRINTI(INT), BNDMAP(WBTECH(ITECH,NERC) + 1)
 4417       FORMAT(1X,"UDAT_BNDMAP",8(":",I5),":",A1)

            IF (WBTECH(ITECH,NERC).gt.0 .and.WBTECH(ITECH,NERC) .lt. 21) &
               EPBNDTYP(UIRINTI(INT)) = BNDMAP(WBTECH(ITECH,NERC) + 1)

!           OVERWRITE INCORRECT BOUND TYPE FROM RENEWABLES MODULE

            EPBNDTYP(UIRINTI(INT)) = BNDMAP(11)

            IF(MSPTMX.LE.3)THEN

!              SUPPLY CURVE ELASTICITY SWITCH

               EPSWSUP(UIRINTI(INT)) = UTCSSW(ITECH)

!              NUMBER OF STEPS IN SUPPLY CURVE

               IF(UTCSSW(ITECH).GT.0)THEN
                  IF (ESTSWTCH(ICAP) .LT. 0)THEN
                     EPSTSUP(UIRINTI(INT)) = MSPTMX
                  ELSE
                     EPSTSUP(UIRINTI(INT)) = MAX(1,ESTSWTCH(ICAP))
                  END IF
               ELSE
                  EPSTSUP(UIRINTI(INT)) = 1
               END IF
               DO PYR = 1 , EPSTSUP(UIRINTI(INT))

!                 COST MULTIPLIERS AND BOUNDS FOR RENEWABLE TECHNOLOGY SUPPLY CURVES
!                 REGIONAL CURVES
!                 IF(EPSWSUP(UIRINTI(INT)) .GT. MSPTMX)THEN

                  IF(ESTSWTCH(ICAP) .LT. 0)THEN
                     EPCCSUP(UIRINTI(INT),PYR) = UTCSFN(NERC,ITECH,PYR)

!                    WRITE(6,3755) CURIRUN, CURIYR+1989, CURITR, NERC, UIRINTI(INT), PYR, EPCCSUP(UIRINTI(INT),PYR)

                     EPFXSUP(UIRINTI(INT),PYR) = UTFXFN(NERC,ITECH,PYR)
                     EPVRSUP(UIRINTI(INT),PYR) = UTVRFN(NERC,ITECH,PYR)
                     EPBDSUP(UIRINTI(INT),PYR) = UTCAFNR(NERC,ITECH,PYR) * 0.001

!                    NATIONAL LEVEL CONSTRAINTS (ELASTICITIES)

                  ELSE
                     EPCCSUP(UIRINTI(INT),PYR) = UTCSFN(NERC,ITECH,PYR) + ESTCPCST(ICAP,PYR)

!                    WRITE(6,3755) CURIRUN, CURIYR+1989, CURITR, NERC, UIRINTI(INT), PYR, EPCCSUP(UIRINTI(INT),PYR)

                     EPFXSUP(UIRINTI(INT),PYR) = 1.0
                     EPVRSUP(UIRINTI(INT),PYR) = 1.0

!                    EPBDSUP(UIRINTI(INT),PYR) = UTCAFN(ITECH,PYR) * 0.001

                     EPBDSUP(UIRINTI(INT),PYR) = ESTCPLIM(ICAP,PYR) * 0.001
                  END IF
               ENDDO
            ELSE
               PRINT *,'PARAMETER MSPTMX EXCEEDS 3'
               STOP
            END IF

!           IF(NERC.EQ.1)THEN
!           PRINT *,'CSTCRV',CURIYR+1989,INT,UIRINTI(INT), &
!              EPSWSUP(UIRINTI(INT)),EPSTSUP(UIRINTI(INT)), &
!              (EPCCSUP(UIRINTI(INT),PYR),PYR = 1 , EPSTSUP(UIRINTI(INT))), &
!              (EPBDSUP(UIRINTI(INT),PYR),PYR = 1 , EPSTSUP(UIRINTI(INT)))
!           END IF


!           SOLAR THERMAL C

            IF (UPLNTCD(UCPINTI(INT)).EQ.'SO') THEN

!CC            CALCULATE AVAILABLE CAPACITY TO BUILD

               IF (EPBNDTYP(UIRINTI(INT)) .NE. 'Z') THEN
                  DO PYR = 1 , (UNXPH - UPPLYR(UCPINTI(INT)))
                     IF ((RYR+PYR-1) .GT. UNYEAR) THEN
                        BYR = UNYEAR
                     ELSE
                        BYR = RYR + PYR - 1
                     ENDIF
                     IF (WBTECH(ITECH,NERC) .LT. 10) THEN
                        VALUE = (WCASTEL(NERC,BYR) - EPECAP(0,UCPINTI(INT),PYR)) * .001
                     ELSE
                        VALUE = (WCASTEL(NERC,BYR) * .001)
                     ENDIF

                     IF ((VALUE .LT. 0.0) .AND. (FULLYR .GE. UPSTYR)) THEN
                        WRITE(13,420) NERC,CURIYR,PYR,UPLNTCD(UCPINTI(INT)), &
                           WCASTEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR)
                        VALUE = 0.0
                     ENDIF

                     EPBLDBND(UIRINTI(INT),PYR) = VALUE

                     IF (UF_DBG .GT. 0) THEN
                        WRITE(13,415) NERC,INT,CURIYR,PYR, &
                           UPLNTCD(UCPINTI(INT)), &
                           WCASTEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR), &
                           EPBLDBND(UIRINTI(INT),PYR),EPBNDTYP(UIRINTI(INT))
                     ENDIF
                  ENDDO
               ENDIF

!              WIND C

            ELSEIF (UPLNTCD(UCPINTI(INT)).EQ.'WN') THEN

!C             IF WIND ADD ON TRANSMISSION COSTS SET IN ECPDAT DO NOT OVERWRITE.

               IF (RTOVALUE('RENEWWI ',0) .NE. 2) THEN
                  EPCTRM(ICAP) = WWNTD(NERC,RYR)
               ENDIF

!CC            CALCULATE AVAILABLE CAPACITY TO BUILD

               IF (EPBNDTYP(UIRINTI(INT)) .NE. 'Z') THEN
                  DO PYR = 1 , (UNXPH - UPPLYR(UCPINTI(INT)))
                     IF ((RYR+PYR-1) .GT. UNYEAR) THEN
                        BYR = UNYEAR
                     ELSE
                        BYR = RYR + PYR - 1
                     ENDIF
                     IF (WBTECH(ITECH,NERC) .LT. 10) THEN
                        VALUE = (WCAWIEL(NERC,BYR) - EPECAP(0,UCPINTI(INT),PYR)) * .001
                     ELSE
                        VALUE = (WCAWIEL(NERC,BYR) * .001)
                     ENDIF

                     IF ((VALUE .LT. 0.0) .AND. (FULLYR .GE. UPSTYR)) THEN
                        WRITE(13,420) NERC,CURIYR,PYR,UPLNTCD(UCPINTI(INT)), &
                           WCAWIEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR)
                        VALUE = 0.0
                     ENDIF

                     EPBLDBND(UIRINTI(INT),PYR) = VALUE

                     IF (UF_DBG .GT. 0) THEN
                        WRITE(13,415) NERC,INT,CURIYR,PYR, &
                           UPLNTCD(UCPINTI(INT)), &
                           WCAWIEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR), &
                           EPBLDBND(UIRINTI(INT),PYR),EPBNDTYP(UIRINTI(INT))
                     ENDIF
                  ENDDO
               ENDIF

!              WIND Low Speed

            ELSEIF (UPLNTCD(UCPINTI(INT)).EQ.'WL') THEN

!              IF WIND ADD ON TRANSMISSION COSTS SET IN ECPDAT DO NOT OVERWRITE.

               IF (RTOVALUE('RENEWWI ',0) .NE. 2) THEN
                  EPCTRM(ICAP) = WWLTD(NERC,RYR)
               ENDIF

!CC            CALCULATE AVAILABLE CAPACITY TO BUILD

               IF (EPBNDTYP(UIRINTI(INT)) .NE. 'Z') THEN
                  DO PYR = 1 , (UNXPH - UPPLYR(UCPINTI(INT)))
                     IF ((RYR+PYR-1) .GT. UNYEAR) THEN
                        BYR = UNYEAR
                     ELSE
                        BYR = RYR + PYR - 1
                     ENDIF
                     IF (WBTECH(ITECH,NERC) .LT. 10) THEN
                        VALUE = (WCAWLEL(NERC,BYR) - EPECAP(0,UCPINTI(INT),PYR)) * .001
                     ELSE
                        VALUE = (WCAWLEL(NERC,BYR) * .001)
                     ENDIF

                     IF ((VALUE .LT. 0.0) .AND. (FULLYR .GE. UPSTYR)) THEN
                        WRITE(13,420) NERC,CURIYR,PYR,UPLNTCD(UCPINTI(INT)), &
                           WCAWLEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR)
                        VALUE = 0.0
                     ENDIF

                     EPBLDBND(UIRINTI(INT),PYR) = VALUE

                     IF (UF_DBG .GT. 0) THEN
                        WRITE(13,415) NERC,INT,CURIYR,PYR, &
                           UPLNTCD(UCPINTI(INT)), &
                           WCAWLEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR), &
                           EPBLDBND(UIRINTI(INT),PYR),EPBNDTYP(UIRINTI(INT))
                     ENDIF
                  ENDDO
               ENDIF

!              OFFSHORE WIND C

            ELSEIF (UPLNTCD(UCPINTI(INT)).EQ.'WF') THEN

!C             IF WIND ADD ON TRANSMISSION COSTS SET IN ECPDAT DO NOT OVERWRITE.

               IF (RTOVALUE('RENEWWI ',0) .NE. 2) THEN
                  EPCTRM(ICAP) = WWFTD(NERC,RYR)
               ENDIF

!CC            CALCULATE AVAILABLE CAPACITY TO BUILD

               IF (EPBNDTYP(UIRINTI(INT)) .NE. 'Z') THEN
                  DO PYR = 1 , (UNXPH - UPPLYR(UCPINTI(INT)))
                     IF ((RYR+PYR-1) .GT. MNUMYR) THEN
                        BYR = MNUMYR
                     ELSE
                        BYR = RYR + PYR - 1
                     ENDIF
                     IF (WBTECH(ITECH,NERC) .LT. 10) THEN
                        VALUE = (WCAWFEL(NERC,BYR) - EPECAP(0,UCPINTI(INT),PYR)) * .001
                     ELSE
                        VALUE = (WCAWFEL(NERC,BYR) * .001)
                     ENDIF

                     IF ((VALUE .LT. 0.0) .AND. (FULLYR .GE. UPSTYR)) THEN
                        WRITE(13,420) NERC,CURIYR,PYR,UPLNTCD(UCPINTI(INT)), &
                           WCAWFEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR)
                        VALUE = 0.0
                     ENDIF

                     EPBLDBND(UIRINTI(INT),PYR) = VALUE

                     IF (UF_DBG .GT. 0) THEN
                        WRITE(13,415) NERC,INT,CURIYR,PYR, &
                           UPLNTCD(UCPINTI(INT)), &
                           WCAWFEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR), &
                           EPBLDBND(UIRINTI(INT),PYR),EPBNDTYP(UIRINTI(INT))
                     ENDIF
                  ENDDO
               ENDIF

!              SOLAR PV      C

            ELSEIF (UPLNTCD(UCPINTI(INT)).EQ.'PV') THEN

!CC            CALCULATE AVAILABLE CAPACITY TO BUILD

               IF (EPBNDTYP(UIRINTI(INT)) .NE. 'Z') THEN
                  DO PYR = 1 , (UNXPH - UPPLYR(UCPINTI(INT)))
                     IF ((RYR+PYR-1) .GT. UNYEAR) THEN
                        BYR = UNYEAR
                     ELSE
                        BYR = RYR + PYR - 1
                     ENDIF
                     IF (WBTECH(ITECH,NERC) .LT. 10) THEN
                        VALUE = (WCAPVEL(NERC,BYR) -  &
                           EPECAP(0,UCPINTI(INT),PYR)) * .001
                     ELSE
                        VALUE = (WCAPVEL(NERC,BYR) * .001)
                     ENDIF

                     IF ((VALUE .LT. 0.0) .AND. (FULLYR .GE. UPSTYR)) THEN
                        WRITE(13,420) NERC,CURIYR,PYR,UPLNTCD(UCPINTI(INT)), &
                           WCAPVEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR)
                        VALUE = 0.0
                     ENDIF

                     EPBLDBND(UIRINTI(INT),PYR) = VALUE

                     IF (UF_DBG .GT. 0) THEN
                        WRITE(13,415) NERC,INT,CURIYR,PYR, &
                           UPLNTCD(UCPINTI(INT)), &
                           WCAPVEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR), &
                           EPBLDBND(UIRINTI(INT),PYR),EPBNDTYP(UIRINTI(INT))
                     ENDIF
                  ENDDO
               ENDIF

!              SOLAR PV TILT    C

            ELSEIF (UPLNTCD(UCPINTI(INT)).EQ.'PT') THEN

!CC            CALCULATE AVAILABLE CAPACITY TO BUILD

               IF (EPBNDTYP(UIRINTI(INT)) .NE. 'Z') THEN
                  DO PYR = 1 , (UNXPH - UPPLYR(UCPINTI(INT)))
                     IF ((RYR+PYR-1) .GT. UNYEAR) THEN
                        BYR = UNYEAR
                     ELSE
                        BYR = RYR + PYR - 1
                     ENDIF
                     IF (WBTECH(ITECH,NERC) .LT. 10) THEN
                        VALUE = (WCAPTEL(NERC,BYR) - EPECAP(0,UCPINTI(INT),PYR)) * .001
                     ELSE
                        VALUE = (WCAPTEL(NERC,BYR) * .001)
                     ENDIF

                     IF ((VALUE .LT. 0.0) .AND. (FULLYR .GE. UPSTYR)) THEN
                        WRITE(13,420) NERC,CURIYR,PYR,UPLNTCD(UCPINTI(INT)), &
                           WCAPTEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR)
                        VALUE = 0.0
                     ENDIF

                     EPBLDBND(UIRINTI(INT),PYR) = VALUE

                     IF (UF_DBG .GT. 0) THEN
                        WRITE(13,415) NERC,INT,CURIYR,PYR, &
                           UPLNTCD(UCPINTI(INT)), &
                           WCAPTEL(NERC,BYR),EPECAP(0,UCPINTI(INT),PYR), &
                           EPBLDBND(UIRINTI(INT),PYR),EPBNDTYP(UIRINTI(INT))
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF

            CALL STRBLD(1,NERC)

            IF (UF_DBG .GT. 0) THEN
               FROM_LABEL = "RENOVR_INT"

               CALL ECP_AVG_HTRT(FROM_LABEL, NERC, 0, UCPINTI(INT), 1, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, T_ECP_GEN, T_ECP_GEN_MR, T_ECP_GEN_MOD, T_ECP_GEN_MR_MOD)

               WRITE(13,416) UPFORT(UCPINTI(INT)),UPPMRT(UCPINTI(INT)), &
                  UPAVLYR(UCPINTI(INT)),UPCSB(UCPINTI(INT)), &
                  UPVSB(UCPINTI(INT))
               WRITE(13,417) UPCLYR(UCPINTI(INT)),UPECLF(UCPINTI(INT)), &
                  EPIROVR(UIRINTI(INT)),EPIRVOM(UIRINTI(INT)), &
                  EPIRFOM(UIRINTI(INT))
               WRITE(13,418) AVG_HTRT(UCPINTI(INT)), &
                  EPCRBRT(UIRINTI(INT)),EPCO1RT(UIRINTI(INT))
               WRITE(13,419) EPCO2RT(UIRINTI(INT)),EPCH4RT(UIRINTI(INT)), &
                  EPSOXRT(UIRINTI(INT)),EPNOXRT(UIRINTI(INT)), &
                  EPVOCRT(UIRINTI(INT))
            END IF

  416       FORMAT(3X,' RENOVR: UPFORT=',G11.3,' UPPMRT=',G11.3, &
               ' UPAVLYR=',I5,' UPCSB=',G11.3,' UPVSB=',G11.3)
  417       FORMAT(3X,' RENOVR: UPCLYR=',G11.3,' UPECLF=',G11.3, &
               ' EPIOVR=',G11.3,' EPIVOM=',G11.3,' EPIFOM=',G11.3)
  418       FORMAT(3X,' RENOVR: AVG_HTRT=',G11.3, &
               ' EPCRBRT=',G11.3,' EPCO1RT=',G11.3)
  419       FORMAT(3X,' RENOVR: EPCO2RT=',G11.3,' EPCH4RT=',G11.3, &
               ' EPSOXRT=',G11.3,' EPNOXRT=',G11.3,' EPVOCRT=',G11.3)
!
  200    CONTINUE                                    ! END INTERMITTENTS
!
  400    FORMAT(1X,' RENOVR: NEG RNW AVAILABLE BOUNDS, SET TO 0.0', &
         1X,' NERC ',I2,' YR ', I2, ' PYR ',I2,A4,' RENCAP(YR-1) ',F12.3, &
         ' EXCAP(YR) ',F12.3)
  410    FORMAT(3X,' RENOVR: NERC ',I2,' RNW TECH ',I2, &
         ' CURIYR ',I2,' PYR ',I2,' UPRNCD ',A4, &
         ' WCA__IEL ',G11.3,' BUILD BOUND(GW) (EPRCAP(PLT,1)) ', &
         G11.3,' EPRBND(PLT) ',G11.3,' BOUND TYPE (EPRBTY) ',A2)
  415    FORMAT(3X,' RENOVR: NERC ',I2,' INT TECH ',I2, &
         ' CURIYR ',I2,' PYR ',I2,' UPINCD ',A4, &
         ' WCA__IEL ',G11.3,' BUILD BOUND(GW) (EPICAP(PLT,1)) ', &
         G11.3,' EPIBND(PLT) ',G11.3,' BOUND TYPE (EPIBTY) ',A2)
  420    FORMAT(1X,' RENOVR: NEG INT AVAILABLE BOUNDS, SET TO 0.0', &
         1X,' NERC ',I2,' YR ', I2,' PYR ',I2, A4,' RENCAP(YR-1) ',F12.3, &
         ' EXCAP(YR) ',F12.3)
      RETURN
      END
!
      SUBROUTINE CFCOVR(NERC)

      IMPLICIT NONE

!     THIS SUBROUTINE REPLACES EMM INTERMITTENT MAXIMUM CAPACITY FACTORS WITH VALUES FROM RENEW

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'bildin'
      include 'dsmdimen'
      include 'dsmcaldr'
      include 'dsmtfecp'
      include 'dsmtoefd'
      include 'wrenew'
      include 'uefdout'
      include 'entcntl'
      include 'wwind'
      include 'dispcrv'

!     MAXIMUM CAPACITY FACTORS BY INTERMITTENT TECH AND LOAD STEP

      INTEGER*4 IS,IL,LDGRP,FULLYR                               ! LOAD GROUP INDICES
      INTEGER*4 NERC,INT,RYR,CPTYP,ICAP,IR_CAP                   ! EMM INDICES
      INTEGER J,I,m,d,h,ECP_GRP,EFD_GRP,ECP_SEA,EFD_SEA,ISEG,STO_YR
      REAL*8 SumCF_ECPg(ECP_D_VLS),SumHrs_ECPg(ECP_D_VLS)
      REAL*8 SumCF_EFDg(EFD_D_SSZ,ELD_D_DAY),SumHrs_EFDg(EFD_D_SSZ,ELD_D_DAY)
      REAL*8 SumCF_ECPs(ECP_D_MSP),SumHrs_ECPs(ECP_D_MSP)
      REAL*8 SumCF_EFDs(EFD_D_MSP),SumHrs_EFDs(EFD_D_MSP)
      REAL*8 SumCF_month(12),SumHrs_month(12)

      IF (CURIYR .EQ. FIRSYR) THEN
         RYR = CURIYR
      ELSE
         RYR = CURIYR - 1
      ENDIF

      STO_YR = RYR

      FULLYR = USYEAR(CURIYR)

      DO EFD_SEA = 1 , EFD_D_MSP
         DO INT = 1, ECP_D_INT
            STO_CFefdSEA(INT,EFD_SEA,NERC) = 0.0
         END DO
      END DO

      DO INT = 1, ECP_D_INT
         ICAP = UCPINTI(INT)
         IR_CAP = UIRINTI(INT)
         EPIACF(INT) = 0.0
         Do i= 1,EPNSTP(1)
            CFecpGRP(INT,i) = 0.0
            SumCF_ECPg(i) = 0.0
            SumHrs_ECPg(i) = 0.0
         enddo
         Do i =1,UTNGRP
           DO ISEG = 1, UTNSEG
             DO J = 0, MX_STO_INC
                HREFDCF(J,INT,:,ISEG,i,NERC) = 0.0
             END DO
            SumCF_EFDg(ISEG,i) = 0.0
            SumHrs_EFDg(ISEG,i) = 0.0
           ENDDO
         enddo
         Do i= 1,ECPnS
            CFecpSEA(INT,i) = 0.0
            SumCF_ECPs(i) = 0.0
            SumHrs_ECPs(i) = 0.0
         enddo
         Do i=1,EFDnS
            CFefdSEA(INT,i) = 0.0
            SumCF_EFDs(i) = 0.0
            SumHrs_EFDs(i) = 0.0
         enddo

         DO i = 1 ,12
            CFmonth(INT,i,NERC) = 0.0
            SumCF_month(i) = 0.0
            SumHrs_month(i) = 0.0
         END DO

!        CCCCCCCCCCCCCCCCC
!           SOLAR THERMAL
!        CCCCCCCCCCCCCCCCC

         IF (UPLNTCD(UCPINTI(INT)).EQ.'SO') THEN

           EPCURCL_SO(NERC,CURIYR) = WCURCL_SO(NERC,CURIYR-1)

!           CALCULATE Average Annual Cap Factor

            Do m=1,12
               do d=1,3
                  do h=1,24
                     EPIACF(INT) = EPIACF(INT) + (WSSSTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                  enddo
               enddo
            enddo

!           CALCULATE Average Capacity Factors for each ECP Group, EFD group, ecp season, efd season

            Do m=1,12
               do d=1,3
                  do h=1,24
                     IF (CURIYR .LT. (UPSTYR-UHBSYR)) THEN
                        ECP_GRP = HrToECPGRP(m,d,h)
                     ELSE
                        ECP_GRP = HrToECPSL(CURIYR,NERC,m,d,h)
                     ENDIF
                     SumCF_ECPg(ECP_GRP) = SumCF_ECPg(ECP_GRP) + WSSSTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPg(ECP_GRP) = SumHrs_ECPg(ECP_GRP) + IDAYTQ(d,m)

                     IF (CURIYR+1989 .EQ. 2017 .OR. CURIYR+1989 .EQ. 2040) THEN
                        WRITE(18,2317) CURIRUN, CURIYR+1989, NERC, RYR+UHBSYR, UPSTYR, ECP_GRP, m, d, h, INT, UCPINTI(INT), IDAYTQ(d,m), UPLNTCD(UCPINTI(INT)), &
                           WSSSTEL_CF(NERC,RYR,d,m,h), SumCF_ECPg(ECP_GRP), SumHrs_ECPg(ECP_GRP)
 2317                   FORMAT(1X,"UDAT_EPICFC",12(",",I5),",",A2,3(",",F21.6))
                     END IF

                     IF (RYR.LT.(UPSTYR-UHBSYR)) THEN
                        EFD_GRP = HrToEFDGrp(m,d,h)
                        DO ISEG = 1, 3
                           SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSSSTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                           SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                        ENDDO
                     ELSE
                        EFD_GRP =HRTOEFDGP(m,d,h)
                        ISEG =HRTOEFDSL(NERC,m,d,h)
                        SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSSSTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                        SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                     ENDIF

                     ECP_SEA = HrToECPSeas(m,d,h)
                     SumCF_ECPs(ECP_SEA) = SumCF_ECPs(ECP_SEA) + WSSSTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPs(ECP_SEA) = SumHrs_ECPs(ECP_SEA) + IDAYTQ(d,m)
                     EFD_SEA = HrToEFDSeas(m,d,h)
                     SumCF_EFDs(EFD_SEA) = SumCF_EFDs(EFD_SEA) + WSSSTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_EFDs(EFD_SEA) = SumHrs_EFDs(EFD_SEA) + IDAYTQ(d,m)

                     STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) + WSSSTEL_CF(NERC,STO_YR,d,m,h) * IDAYTQ(d,m)

                     SumCF_month(m) = SumCF_month(m) + WSSSTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_month(m) = SumHrs_month(m) + IDAYTQ(d,m)

                  enddo             !hr
               enddo                !daytype
            enddo                   !month

            Do ECP_GRP = 1, EPNSTP(1)
               if (SumHrs_ECPg(ECP_GRP) .gt. 0 ) then
                  CFecpGRP(INT,ECP_GRP) = SumCF_ECPg(ECP_GRP) / SumHrs_ECPg(ECP_GRP)

               else

                  IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'ERROR-No Daytype hours in ECP_GROUP ',ECP_GRP,' for SolarThermal'

                  CFecpGRP(INT,ECP_GRP) = 0.0
               endif
            enddo

            Do EFD_GRP =1,UTNGRP
               DO ISEG = 1, UTNSEG
                  if (SumHrs_EFDg(ISEG,EFD_GRP) .gt. 0 ) then
                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = SumCF_EFDg(ISEG,EFD_GRP) / SumHrs_EFDg(ISEG,EFD_GRP)
                  else

                     IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'ERROR-No Daytype hours in EFD_GROUP ',EFD_GRP,' for SolarThermal'

                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = 0.0
                  endif

!                 write(*,'(a,5i6,2f9.4)') 'ST - COMPARE EFDRegCF,y,r,int,grp, New', RYR, CURITR, NERC, INT, EFD_GRP, CFefdGRP(INT,EFD_GRP)

               enddo
            Enddo

            Do ECP_SEA = 1, ECPnS
               if (SumHrs_ECPs(ECP_SEA) .gt. 0 ) then
                  CFecpSEA(INT,ECP_SEA) = SumCF_ECPs(ECP_SEA) / SumHrs_ECPs(ECP_SEA)
               else

                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in ECPseasonal CF calculation,season ',ECP_SEA,' for SolarThermal'

                  CFecpSEA(INT,ECP_SEA) = 0.0
               endif

!              write(*,'(a,5i6,2f9.4)')'ST - COMPARE ECP SeasonalCF,y,i,r,int,ecpSeasn, Orig, New ',RYR,CURITR,NERC, INT,ECP_SEA, EPISCF(INT,ECP_SEA), CFecpSEA(INT,ECP_SEA)

            enddo

            Do EFD_SEA = 1, EFDnS
               if (SumHrs_EFDs(EFD_SEA) .gt. 0 ) then
                  CFefdSEA(INT,EFD_SEA) = SumCF_EFDs(EFD_SEA) / SumHrs_EFDs(EFD_SEA)

                  STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) / SumHrs_EFDs(EFD_SEA)
               else
                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in EFDseasonal CF calculation,season ',EFD_SEA,' for SolarThermal'
                  CFefdSEA(INT,EFD_SEA) = 0.0
                  STO_CFefdSEA(INT,EFD_SEA,NERC) = 0.0001
               endif

!              write(*,'(a,5i6,2f9.4)')'ST - COMPARE EFD SeasonalCF,y,i,r,int,grp, New ',RYR,CURITR,NERC, INT,EFD_SEA,  CFefdSEA(INT,EFD_SEA)

               WRITE(18,9313) CURIRUN, CURIYR+1989, CURITR, STO_YR+1989, RYR+1989, NERC, INT, EFD_SEA, STO_CFefdSEA(INT,EFD_SEA,NERC), CFefdSEA(INT,EFD_SEA), SumCF_EFDs(EFD_SEA), SumHrs_EFDs(EFD_SEA)

            enddo

            DO m = 1, 12
               CFmonth(INT,m,NERC) = SumCF_month(m) / SumHrs_month(m)
            END DO

         ELSEIF (UPLNTCD(UCPINTI(INT)).EQ.'WN') THEN

!           CCCCCCCCC
!              WIND
!           CCCCCCCCC

            EPCURCL_WN(NERC,CURIYR) = WCURCL(NERC,CURIYR-1)
            EPCURBF_WN(NERC,CURIYR) = WCURBF(NERC,CURIYR-1)

!           CALCULATE Average Annual Cap Factor

            Do m=1,12
               do d=1,3
                  do h=1,24
                     EPIACF(INT) = EPIACF(INT) + (WSFWIEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                  enddo
               enddo
            enddo

!           CALCULATE Average Capacity Factors for each ECP Group, EFD group, ecp season, efd season

            Do m=1,12
               do d=1,3
                  do h=1,24
                     IF (CURIYR .LT. (UPSTYR-UHBSYR)) THEN
                        ECP_GRP = HrToECPGRP(m,d,h)
                     ELSE
                        ECP_GRP = HrToECPSL(CURIYR,NERC,m,d,h)
                     ENDIF
                     SumCF_ECPg(ECP_GRP) = SumCF_ECPg(ECP_GRP) + WSFWIEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPg(ECP_GRP) = SumHrs_ECPg(ECP_GRP) + IDAYTQ(d,m)

                     IF (CURIYR+1989 .EQ. 2017 .OR. CURIYR+1989 .EQ. 2040) THEN
                        WRITE(18,2317) CURIRUN, CURIYR+1989, NERC, RYR+UHBSYR, UPSTYR, ECP_GRP, m, d, h, INT, UCPINTI(INT), IDAYTQ(d,m), UPLNTCD(UCPINTI(INT)), &
                           WSFWIEL_CF(NERC,RYR,d,m,h), SumCF_ECPg(ECP_GRP), SumHrs_ECPg(ECP_GRP)
                     END IF

                     IF (RYR.LT.(UPSTYR-UHBSYR)) THEN
                        EFD_GRP = HrToEFDGrp(m,d,h)
                        DO ISEG = 1, 3
                           SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSFWIEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                           SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                        ENDDO
                     ELSE
                        EFD_GRP =HRTOEFDGP(m,d,h)
                        ISEG =HRTOEFDSL(NERC,m,d,h)
                        SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSFWIEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                        SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                     ENDIF

                     ECP_SEA = HrToECPSeas(m,d,h)
                     SumCF_ECPs(ECP_SEA) = SumCF_ECPs(ECP_SEA) + WSFWIEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPs(ECP_SEA) = SumHrs_ECPs(ECP_SEA) + IDAYTQ(d,m)
                     EFD_SEA = HrToEFDSeas(m,d,h)
                     SumCF_EFDs(EFD_SEA) = SumCF_EFDs(EFD_SEA) + WSFWIEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_EFDs(EFD_SEA) = SumHrs_EFDs(EFD_SEA) + IDAYTQ(d,m)

                     STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) + WSFWIEL_CF(NERC,STO_YR,d,m,h) * IDAYTQ(d,m)

                     SumCF_month(m) = SumCF_month(m) + WSFWIEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_month(m) = SumHrs_month(m) + IDAYTQ(d,m)

                  enddo          !hr
               enddo             !daytype
            enddo                !month

            Do ECP_GRP = 1, EPNSTP(1)
               if (SumHrs_ECPg(ECP_GRP) .gt. 0 ) then
                  CFecpGRP(INT,ECP_GRP) = SumCF_ECPg(ECP_GRP) / SumHrs_ECPg(ECP_GRP)
               else

                  IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'No Daytype hours in ECP_GROUP ',ECP_GRP,' for Wind'

                  CFecpGRP(INT,ECP_GRP) = 0.0
               endif
            enddo

            Do EFD_GRP =1,UTNGRP
               DO ISEG = 1, UTNSEG
                  if (SumHrs_EFDg(ISEG,EFD_GRP) .gt. 0 ) then
                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = SumCF_EFDg(ISEG,EFD_GRP) / SumHrs_EFDg(ISEG,EFD_GRP)
                  else

                     IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'ERROR-No Daytype hours in EFD_GROUP ',EFD_GRP,' for Wind'

                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = 0.0
                  endif
               enddo
            Enddo

            Do ECP_SEA = 1, ECPnS
               if (SumHrs_ECPs(ECP_SEA) .gt. 0 ) then
                  CFecpSEA(INT,ECP_SEA) = SumCF_ECPs(ECP_SEA) / SumHrs_ECPs(ECP_SEA)
               else

                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in ECPseasonal CF calculation,season ',ECP_SEA,' for Wind'

                  CFecpSEA(INT,ECP_SEA) = 0.0
               endif

!              write(*,'(a,5i6,2f9.4)')'WI - COMPARE ECP SeasonalCF,y,i,r,int,ecpSeasn, Orig, New ',RYR,CURITR,NERC, INT,ECP_SEA, EPISCF(INT,ECP_SEA), CFecpSEA(INT,ECP_SEA)

            enddo

            Do EFD_SEA = 1, EFDnS
               if (SumHrs_EFDs(EFD_SEA) .gt. 0 ) then
                  CFefdSEA(INT,EFD_SEA) = SumCF_EFDs(EFD_SEA) / SumHrs_EFDs(EFD_SEA)

                  STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) / SumHrs_EFDs(EFD_SEA)
               else
                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in EFDseasonal CF calculation,season ',EFD_SEA,' for Wind'
                  CFefdSEA(INT,EFD_SEA) = 0.0
                  STO_CFefdSEA(INT,EFD_SEA,NERC) = 0.0001
               endif

!              write(*,'(a,5i6,2f9.4)')'WI - COMPARE EFD SeasonalCF,y,i,r,int,grp, New ',RYR,CURITR,NERC, INT,EFD_SEA,  CFefdSEA(INT,EFD_SEA)

               WRITE(18,9313) CURIRUN, CURIYR+1989, CURITR, STO_YR+1989, RYR+1989, NERC, INT, EFD_SEA, STO_CFefdSEA(INT,EFD_SEA,NERC), CFefdSEA(INT,EFD_SEA), SumCF_EFDs(EFD_SEA), SumHrs_EFDs(EFD_SEA)

            enddo

            DO m = 1, 12
               CFmonth(INT,m,NERC) = SumCF_month(m) / SumHrs_month(m)
            END DO


         ELSEIF (UPLNTCD(UCPINTI(INT)).EQ.'WL') THEN

!           CCCCCCCCC
!              WL WIND
!           CCCCCCCCC

            EPCURCL_WL(NERC,CURIYR) = WCURCL_WL(NERC,CURIYR-1)
            EPCURBF_WL(NERC,CURIYR) = WCURBF_WL(NERC,CURIYR-1)

!           CALCULATE Average Annual Cap Factor

            Do m=1,12
               do d=1,3
                  do h=1,24
                     EPIACF(INT) = EPIACF(INT) + (WSFWLEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                  enddo
               enddo
            enddo

!           CALCULATE Average Capacity Factors for each ECP Group, EFD group, ecp season, efd season

            Do m=1,12
               do d=1,3
                  do h=1,24
                     IF (CURIYR .LT. (UPSTYR-UHBSYR)) THEN
                        ECP_GRP = HrToECPGRP(m,d,h)
                     ELSE
                        ECP_GRP = HrToECPSL(CURIYR,NERC,m,d,h)
                     ENDIF
                     SumCF_ECPg(ECP_GRP) = SumCF_ECPg(ECP_GRP) + WSFWLEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPg(ECP_GRP) = SumHrs_ECPg(ECP_GRP) + IDAYTQ(d,m)


                     IF (CURIYR+1989 .EQ. 2017 .OR. CURIYR+1989 .EQ. 2040) THEN
                        WRITE(18,2317) CURIRUN, CURIYR+1989, NERC, RYR+UHBSYR, UPSTYR, ECP_GRP, m, d, h, INT, UCPINTI(INT), IDAYTQ(d,m), UPLNTCD(UCPINTI(INT)), &
                           WSFWLEL_CF(NERC,RYR,d,m,h), SumCF_ECPg(ECP_GRP), SumHrs_ECPg(ECP_GRP)
                     END IF

                     IF (RYR.LT.(UPSTYR-UHBSYR)) THEN
                        EFD_GRP = HrToEFDGrp(m,d,h)
                        DO ISEG = 1, 3
                           SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSFWLEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                           SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                        ENDDO
                     ELSE
                        EFD_GRP =HRTOEFDGP(m,d,h)
                        ISEG =HRTOEFDSL(NERC,m,d,h)
                        SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSFWLEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                        SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                     ENDIF

                     ECP_SEA = HrToECPSeas(m,d,h)
                     SumCF_ECPs(ECP_SEA) = SumCF_ECPs(ECP_SEA) + WSFWLEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPs(ECP_SEA) = SumHrs_ECPs(ECP_SEA) + IDAYTQ(d,m)
                     EFD_SEA = HrToEFDSeas(m,d,h)
                     SumCF_EFDs(EFD_SEA) = SumCF_EFDs(EFD_SEA) + WSFWLEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_EFDs(EFD_SEA) = SumHrs_EFDs(EFD_SEA) + IDAYTQ(d,m)

                     STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) + WSFWLEL_CF(NERC,STO_YR,d,m,h) * IDAYTQ(d,m)

                     SumCF_month(m) = SumCF_month(m) + WSFWLEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_month(m) = SumHrs_month(m) + IDAYTQ(d,m)

                  enddo          !hr
               enddo             !daytype
            enddo                !month

            Do ECP_GRP = 1, EPNSTP(1)
               if (SumHrs_ECPg(ECP_GRP) .gt. 0 ) then
                  CFecpGRP(INT,ECP_GRP) = SumCF_ECPg(ECP_GRP) / SumHrs_ECPg(ECP_GRP)
               else

                  IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'No Daytype hours in ECP_GROUP ',ECP_GRP,' for WLnd'

                  CFecpGRP(INT,ECP_GRP) = 0.0
               endif
            enddo

            Do EFD_GRP =1,UTNGRP
               DO ISEG = 1, UTNSEG
                  if (SumHrs_EFDg(ISEG,EFD_GRP) .gt. 0 ) then
                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = SumCF_EFDg(ISEG,EFD_GRP) / SumHrs_EFDg(ISEG,EFD_GRP)
                  else

                     IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'ERROR-No Daytype hours in EFD_GROUP ',EFD_GRP,' for WLnd'

                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = 0.0
                  endif
               enddo
            Enddo

            Do ECP_SEA = 1, ECPnS
               if (SumHrs_ECPs(ECP_SEA) .gt. 0 ) then
                  CFecpSEA(INT,ECP_SEA) = SumCF_ECPs(ECP_SEA) / SumHrs_ECPs(ECP_SEA)
               else

                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in ECPseasonal CF calculation,season ',ECP_SEA,' for WLnd'

                  CFecpSEA(INT,ECP_SEA) = 0.0
               endif

!              write(*,'(a,5i6,2f9.4)')'WL - COMPARE ECP SeasonalCF,y,i,r,int,ecpSeasn, Orig, New ',RYR,CURITR,NERC, INT,ECP_SEA, EPISCF(INT,ECP_SEA), CFecpSEA(INT,ECP_SEA)

            enddo

            Do EFD_SEA = 1, EFDnS
               if (SumHrs_EFDs(EFD_SEA) .gt. 0 ) then
                  CFefdSEA(INT,EFD_SEA) = SumCF_EFDs(EFD_SEA) / SumHrs_EFDs(EFD_SEA)

                  STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) / SumHrs_EFDs(EFD_SEA)
               else

                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in EFDseasonal CF calculation,season ',EFD_SEA,' for WLnd'

                  CFefdSEA(INT,EFD_SEA) = 0.0
                  STO_CFefdSEA(INT,EFD_SEA,NERC) = 0.0001
               endif

!              write(*,'(a,5i6,2f9.4)')'WL - COMPARE EFD SeasonalCF,y,i,r,int,grp, New ',RYR,CURITR,NERC, INT,EFD_SEA,  CFefdSEA(INT,EFD_SEA)

               WRITE(18,9313) CURIRUN, CURIYR+1989, CURITR, STO_YR+1989, RYR+1989, NERC, INT, EFD_SEA, STO_CFefdSEA(INT,EFD_SEA,NERC), CFefdSEA(INT,EFD_SEA), SumCF_EFDs(EFD_SEA), SumHrs_EFDs(EFD_SEA)

            enddo

            DO m = 1, 12
               CFmonth(INT,m,NERC) = SumCF_month(m) / SumHrs_month(m)
            END DO


         ELSEIF (UPLNTCD(UCPINTI(INT)).EQ.'WF') THEN

!           CCCCCCCCC
!              OFFSHORE WIND
!           CCCCCCCCC

!           CALCULATE Average Annual Cap Factor

            Do m=1,12
               do d=1,3
                  do h=1,24
                     EPIACF(INT) = EPIACF(INT) + (WSFWFEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                  enddo
               enddo
            enddo

!           CALCULATE Average Capacity Factors for each ECP Group, EFD group, ecp season, efd season

            Do m=1,12
               do d=1,3
                  do h=1,24
                     IF (CURIYR .LT. (UPSTYR-UHBSYR)) THEN
                        ECP_GRP = HrToECPGRP(m,d,h)
                     ELSE
                        ECP_GRP = HrToECPSL(CURIYR,NERC,m,d,h)
                     ENDIF
                     SumCF_ECPg(ECP_GRP) = SumCF_ECPg(ECP_GRP) + WSFWFEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPg(ECP_GRP) = SumHrs_ECPg(ECP_GRP) + IDAYTQ(d,m)

                     IF (CURIYR+1989 .EQ. 2017 .OR. CURIYR+1989 .EQ. 2040) THEN
                        WRITE(18,2317) CURIRUN, CURIYR+1989, NERC, RYR+UHBSYR, UPSTYR, ECP_GRP, m, d, h, INT, UCPINTI(INT), IDAYTQ(d,m), UPLNTCD(UCPINTI(INT)), &
                           WSFWFEL_CF(NERC,RYR,d,m,h), SumCF_ECPg(ECP_GRP), SumHrs_ECPg(ECP_GRP)
                     END IF

                     IF (RYR.LT.(UPSTYR-UHBSYR)) THEN
                        EFD_GRP = HrToEFDGrp(m,d,h)
                        DO ISEG = 1, 3
                           SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSFWFEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                           SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                        ENDDO
                     ELSE
                        EFD_GRP =HRTOEFDGP(m,d,h)
                        ISEG =HRTOEFDSL(NERC,m,d,h)
                        SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSFWFEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                        SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                     ENDIF

                     ECP_SEA = HrToECPSeas(m,d,h)
                     SumCF_ECPs(ECP_SEA) = SumCF_ECPs(ECP_SEA) + WSFWFEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPs(ECP_SEA) = SumHrs_ECPs(ECP_SEA) + IDAYTQ(d,m)
                     EFD_SEA = HrToEFDSeas(m,d,h)
                     SumCF_EFDs(EFD_SEA) = SumCF_EFDs(EFD_SEA) + WSFWFEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_EFDs(EFD_SEA) = SumHrs_EFDs(EFD_SEA) + IDAYTQ(d,m)

                     STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) + WSFWFEL_CF(NERC,STO_YR,d,m,h) * IDAYTQ(d,m)

                     SumCF_month(m) = SumCF_month(m) + WSFWFEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_month(m) = SumHrs_month(m) + IDAYTQ(d,m)

                  enddo          !hr
               enddo             !daytype
            enddo                !month

            Do ECP_GRP = 1, EPNSTP(1)
               if (SumHrs_ECPg(ECP_GRP) .gt. 0 ) then
                  CFecpGRP(INT,ECP_GRP) = SumCF_ECPg(ECP_GRP) / SumHrs_ECPg(ECP_GRP)
               else

                  IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'No Daytype hours in ECP_GROUP ',ECP_GRP,' for Offshore Wind'

                  CFecpGRP(INT,ECP_GRP) = 0.0
               endif
            enddo

            Do EFD_GRP =1,UTNGRP
               DO ISEG = 1, UTNSEG
                  if (SumHrs_EFDg(ISEG,EFD_GRP) .gt. 0 ) then
                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = SumCF_EFDg(ISEG,EFD_GRP) / SumHrs_EFDg(ISEG,EFD_GRP)
                  else

                     IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'ERROR-No Daytype hours in EFD_GROUP ',EFD_GRP,' for Offshore Wind'

                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = 0.0
                  endif
               enddo
            Enddo

            Do ECP_SEA = 1, ECPnS
               if (SumHrs_ECPs(ECP_SEA) .gt. 0 ) then
                  CFecpSEA(INT,ECP_SEA) = SumCF_ECPs(ECP_SEA) / SumHrs_ECPs(ECP_SEA)
               else

                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in ECPseasonal CF calculation,season ',ECP_SEA,' for WLnd'

                  CFecpSEA(INT,ECP_SEA) = 0.0
               endif

!              write(*,'(a,5i6,2f9.4)')'WL - COMPARE ECP SeasonalCF,y,i,r,int,ecpSeasn, Orig, New ',RYR,CURITR,NERC, INT,ECP_SEA, EPISCF(INT,ECP_SEA), CFecpSEA(INT,ECP_SEA)

            enddo

            Do EFD_SEA = 1, EFDnS
               if (SumHrs_EFDs(EFD_SEA) .gt. 0 ) then
                  CFefdSEA(INT,EFD_SEA) = SumCF_EFDs(EFD_SEA) / SumHrs_EFDs(EFD_SEA)

                  STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) / SumHrs_EFDs(EFD_SEA)
               else

                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in EFDseasonal CF calculation,season ',EFD_SEA,' for SolarPV'

                  CFefdSEA(INT,EFD_SEA) = 0.0
                  STO_CFefdSEA(INT,EFD_SEA,NERC) = 0.0001
               endif

!              write(*,'(a,5i6,2f9.4)')'WF - COMPARE EFD SeasonalCF,y,i,r,int,grp, New ',RYR,CURITR,NERC, INT,EFD_SEA,  CFefdSEA(INT,EFD_SEA)

               WRITE(18,9313) CURIRUN, CURIYR+1989, CURITR, STO_YR+1989, RYR+1989, NERC, INT, EFD_SEA, STO_CFefdSEA(INT,EFD_SEA,NERC), CFefdSEA(INT,EFD_SEA), SumCF_EFDs(EFD_SEA), SumHrs_EFDs(EFD_SEA)

            enddo

            DO m = 1, 12
               CFmonth(INT,m,NERC) = SumCF_month(m) / SumHrs_month(m)
            END DO

         ELSEIF (UPLNTCD(UCPINTI(INT)).EQ.'PT') THEN

!           CCCCCCCCCCCCCCCCC
!              SOLAR PT
!           CCCCCCCCCCCCCCCCC

           EPCURCL_PT(NERC,CURIYR) = WCURCL_PT(NERC,CURIYR-1)

!           CALCULATE Average Annual Cap Factor

            Do m=1,12
               do d=1,3
                  do h=1,24
                     EPIACF(INT) = EPIACF(INT) + (WSSPTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                  enddo
               enddo
            enddo

!           CALCULATE Average Capacity Factors for each ECP Group, EFD group, ecp season, efd season

            Do m=1,12
               do d=1,3
                  do h=1,24
                     IF (CURIYR .LT. (UPSTYR-UHBSYR)) THEN
                        ECP_GRP = HrToECPGRP(m,d,h)
                     ELSE
                        ECP_GRP = HrToECPSL(CURIYR,NERC,m,d,h)
                     ENDIF
                     SumCF_ECPg(ECP_GRP) = SumCF_ECPg(ECP_GRP) + WSSPTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPg(ECP_GRP) = SumHrs_ECPg(ECP_GRP) + IDAYTQ(d,m)

                     IF (CURIYR+1989 .EQ. 2017 .OR. CURIYR+1989 .EQ. 2040) THEN
                        WRITE(18,2317) CURIRUN, CURIYR+1989, NERC, RYR+UHBSYR, UPSTYR, ECP_GRP, m, d, h, INT, UCPINTI(INT), IDAYTQ(d,m), UPLNTCD(UCPINTI(INT)), &
                           WSSPTEL_CF(NERC,RYR,d,m,h), SumCF_ECPg(ECP_GRP), SumHrs_ECPg(ECP_GRP)
                     END IF

                     IF (RYR.LT.(UPSTYR-UHBSYR)) THEN
                        EFD_GRP = HrToEFDGrp(m,d,h)
                        DO ISEG = 1, 3
                           SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSSPTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                           SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                        ENDDO
                     ELSE
                        EFD_GRP =HRTOEFDGP(m,d,h)
                        ISEG =HRTOEFDSL(NERC,m,d,h)
                        SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSSPTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                        SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                     ENDIF

                     ECP_SEA = HrToECPSeas(m,d,h)
                     SumCF_ECPs(ECP_SEA) = SumCF_ECPs(ECP_SEA) + WSSPTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPs(ECP_SEA) = SumHrs_ECPs(ECP_SEA) + IDAYTQ(d,m)
                     EFD_SEA = HrToEFDSeas(m,d,h)
                     SumCF_EFDs(EFD_SEA) = SumCF_EFDs(EFD_SEA) + WSSPTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_EFDs(EFD_SEA) = SumHrs_EFDs(EFD_SEA) + IDAYTQ(d,m)

                     STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) + WSSPTEL_CF(NERC,STO_YR,d,m,h) * IDAYTQ(d,m)

                     SumCF_month(m) = SumCF_month(m) + WSSPTEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_month(m) = SumHrs_month(m) + IDAYTQ(d,m)

                  enddo
               enddo
            enddo

            Do ECP_GRP = 1, EPNSTP(1)
               if (SumHrs_ECPg(ECP_GRP) .gt. 0 ) then
                  CFecpGRP(INT,ECP_GRP) = SumCF_ECPg(ECP_GRP) / SumHrs_ECPg(ECP_GRP)
               else

                  IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'No Daytype hours in ECP GROUP ',ECP_GRP,' for SolarPT'

                  CFecpGRP(INT,ECP_GRP) = 0.0
               endif

!             write(*,'(a,5i6,3(f12.4,1x))')'PT - COMPARE ECPRegCF,y,i,r,int,grp, Orig, New ',RYR,CURITR,NERC, INT,ECP_GRP, SumCF_ECPg(ECP_GRP) , SumHrs_ECPg(ECP_GRP), CFecpGRP(INT,ECP_GRP)

            enddo

            Do EFD_GRP =1,UTNGRP
               DO ISEG = 1, UTNSEG
                  if (SumHrs_EFDg(ISEG,EFD_GRP) .gt. 0 ) then
                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = SumCF_EFDg(ISEG,EFD_GRP) / SumHrs_EFDg(ISEG,EFD_GRP)
                  else

                     IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'ERROR-No Daytype hours in EFD_GROUP ',EFD_GRP,' for SolarPT'

                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = 0.0
                  endif

!                 write(*,'(a,6i6,3(f12.4,1x))')'PT - COMPARE EFDRegCF,y,i,r,int,grp, Orig, New ',RYR,CURITR,NERC, INT,EFD_GRP, ISEG, &
!                    SumCF_EFDg(ISEG,EFD_GRP) , SumHrs_EFDg(ISEG,EFD_GRP), HREFDCF(0,INT,ISEG,EFD_GRP,NERC)
               enddo
            Enddo

            Do ECP_SEA = 1, ECPnS
               if (SumHrs_ECPs(ECP_SEA) .gt. 0 ) then
                  CFecpSEA(INT,ECP_SEA) = SumCF_ECPs(ECP_SEA) / SumHrs_ECPs(ECP_SEA)
               else

                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in ECPseasonal CF calculation,season ',ECP_SEA,' for SolarPT'

                  CFecpSEA(INT,ECP_SEA) = 0.0
               endif

!              write(*,'(a,5i6,2f9.4)')'PT - COMPARE ECP SeasonalCF,y,i,r,int,ecpSeasn, Orig, New ',RYR,CURITR,NERC, INT,ECP_SEA, EPISCF(INT,ECP_SEA), CFecpSEA(INT,ECP_SEA)

            enddo

            Do EFD_SEA = 1, EFDnS
               if (SumHrs_EFDs(EFD_SEA) .gt. 0 ) then
                  CFefdSEA(INT,EFD_SEA) = SumCF_EFDs(EFD_SEA) / SumHrs_EFDs(EFD_SEA)

                  STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) / SumHrs_EFDs(EFD_SEA)
               else

                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in EFDseasonal CF calculation,season ',EFD_SEA,' for SolarPT'

                  CFefdSEA(INT,EFD_SEA) = 0.0
                  STO_CFefdSEA(INT,EFD_SEA,NERC) = 0.0001
               endif

!              write(*,'(a,5i6,2f9.4)')'PT - COMPARE EFD SeasonalCF,y,i,r,int,grp, New ',RYR,CURITR,NERC, INT,EFD_SEA,  CFefdSEA(INT,EFD_SEA)

               WRITE(18,9313) CURIRUN, CURIYR+1989, CURITR, STO_YR+1989, RYR+1989, NERC, INT, EFD_SEA, STO_CFefdSEA(INT,EFD_SEA,NERC), CFefdSEA(INT,EFD_SEA), SumCF_EFDs(EFD_SEA), SumHrs_EFDs(EFD_SEA)

            enddo

            DO m = 1, 12
               CFmonth(INT,m,NERC) = SumCF_month(m) / SumHrs_month(m)
            END DO

         ELSEIF (UPLNTCD(UCPINTI(INT)).EQ.'PV') THEN

!           CCCCCCCCCCCCCCCCC
!              SOLAR PV
!           CCCCCCCCCCCCCCCCC

           EPCURCL_PV(NERC,CURIYR) = WCURCL_PV(NERC,CURIYR-1)

!           CALCULATE Average Annual Cap Factor

            Do m=1,12
               do d=1,3
                  do h=1,24
                     EPIACF(INT) = EPIACF(INT) + (WSSPVEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m))  / 8760.0
                  enddo
               enddo
            enddo

!           CALCULATE Average Capacity Factors for each ECP Group, EFD group, ecp season, efd season

            Do m=1,12
               do d=1,3
                  do h=1,24
                     IF (CURIYR .LT. (UPSTYR-UHBSYR)) THEN
                        ECP_GRP = HrToECPGRP(m,d,h)
                     ELSE
                        ECP_GRP = HrToECPSL(CURIYR,NERC,m,d,h)
                     ENDIF
                     SumCF_ECPg(ECP_GRP) = SumCF_ECPg(ECP_GRP) + WSSPVEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPg(ECP_GRP) = SumHrs_ECPg(ECP_GRP) + IDAYTQ(d,m)

                     IF (CURIYR+1989 .EQ. 2017 .OR. CURIYR+1989 .EQ. 2040) THEN
                        WRITE(18,2317) CURIRUN, CURIYR+1989, NERC, RYR+UHBSYR, UPSTYR, ECP_GRP, m, d, h, INT, UCPINTI(INT), IDAYTQ(d,m), UPLNTCD(UCPINTI(INT)), &
                           WSSPVEL_CF(NERC,RYR,d,m,h), SumCF_ECPg(ECP_GRP), SumHrs_ECPg(ECP_GRP)
                     END IF

                     IF (RYR.LT.(UPSTYR-UHBSYR)) THEN
                        EFD_GRP = HrToEFDGrp(m,d,h)
                        DO ISEG = 1, 3
                           SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSSPVEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                           SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                        ENDDO
                     ELSE
                        EFD_GRP =HRTOEFDGP(m,d,h)
                        ISEG =HRTOEFDSL(NERC,m,d,h)
                        SumCF_EFDg(ISEG,EFD_GRP) = SumCF_EFDg(ISEG,EFD_GRP) + WSSPVEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                        SumHrs_EFDg(ISEG,EFD_GRP) = SumHrs_EFDg(ISEG,EFD_GRP) + IDAYTQ(d,m)
                     ENDIF

                     ECP_SEA = HrToECPSeas(m,d,h)
                     SumCF_ECPs(ECP_SEA) = SumCF_ECPs(ECP_SEA) + WSSPVEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_ECPs(ECP_SEA) = SumHrs_ECPs(ECP_SEA) + IDAYTQ(d,m)
                     EFD_SEA = HrToEFDSeas(m,d,h)
                     SumCF_EFDs(EFD_SEA) = SumCF_EFDs(EFD_SEA) + WSSPVEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_EFDs(EFD_SEA) = SumHrs_EFDs(EFD_SEA) + IDAYTQ(d,m)

                     STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) + WSSPVEL_CF(NERC,STO_YR,d,m,h) * IDAYTQ(d,m)

                     SumCF_month(m) = SumCF_month(m) + WSSPVEL_CF(NERC,RYR,d,m,h) * IDAYTQ(d,m)
                     SumHrs_month(m) = SumHrs_month(m) + IDAYTQ(d,m)

                  enddo
               enddo
            enddo

            Do ECP_GRP = 1, EPNSTP(1)
               if (SumHrs_ECPg(ECP_GRP) .gt. 0 ) then
                  CFecpGRP(INT,ECP_GRP) = SumCF_ECPg(ECP_GRP) / SumHrs_ECPg(ECP_GRP)
               else

                  IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'No Daytype hours in ECP GROUP ',ECP_GRP,' for SolarPV'

                  CFecpGRP(INT,ECP_GRP) = 0.0
              endif

!             write(*,'(a,5i6,3(f12.4,1x))')'PV - COMPARE ECPRegCF,y,i,r,int,grp, Orig, New ',RYR,CURITR,NERC, INT,ECP_GRP, SumCF_ECPg(ECP_GRP) , SumHrs_ECPg(ECP_GRP), CFecpGRP(INT,ECP_GRP)

            enddo

            Do EFD_GRP =1,UTNGRP
               DO ISEG = 1, UTNSEG
                  if (SumHrs_EFDg(ISEG,EFD_GRP) .gt. 0 ) then
                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = SumCF_EFDg(ISEG,EFD_GRP) / SumHrs_EFDg(ISEG,EFD_GRP)
                  else

                     IF (RYR.GE.(UPSTYR-UHBSYR)) write(*,'(a,i4,a)')'ERROR-No Daytype hours in EFD_GROUP ',EFD_GRP,' for SolarPT'

                     HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC) = 0.0
                  endif

!                 write(*,'(a,6i6,3(f12.4,1x))')'PT - COMPARE EFDRegCF,y,i,r,int,grp, Orig, New ',RYR,CURITR,NERC, INT,EFD_GRP, ISEG, &
!                    SumCF_EFDg(ISEG,EFD_GRP) , SumHrs_EFDg(ISEG,EFD_GRP), HREFDCF(0,INT,ISEG,EFD_GRP,NERC)
               enddo
            Enddo

            Do ECP_SEA = 1, ECPnS
               if (SumHrs_ECPs(ECP_SEA) .gt. 0 ) then
                  CFecpSEA(INT,ECP_SEA) = SumCF_ECPs(ECP_SEA) / SumHrs_ECPs(ECP_SEA)
               else

                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in ECPseasonal CF calculation,season ',ECP_SEA,' for SolarPT'

                  CFecpSEA(INT,ECP_SEA) = 0.0
               endif

!              write(*,'(a,5i6,2f9.4)')'PT - COMPARE ECP SeasonalCF,y,i,r,int,ecpSeasn, Orig, New ',RYR,CURITR,NERC, INT,ECP_SEA, EPISCF(INT,ECP_SEA), CFecpSEA(INT,ECP_SEA)

            enddo

            Do EFD_SEA = 1, EFDnS
               if (SumHrs_EFDs(EFD_SEA) .gt. 0 ) then
                  CFefdSEA(INT,EFD_SEA) = SumCF_EFDs(EFD_SEA) / SumHrs_EFDs(EFD_SEA)

                  STO_CFefdSEA(INT,EFD_SEA,NERC) = STO_CFefdSEA(INT,EFD_SEA,NERC) / SumHrs_EFDs(EFD_SEA)
               else

                  write(*,'(a,i4,a)')'ERROR-Invalid num hours in EFDseasonal CF calculation,season ',EFD_SEA,' for SolarPT'

                  CFefdSEA(INT,EFD_SEA) = 0.0
                  STO_CFefdSEA(INT,EFD_SEA,NERC) = 0.0001
               endif

!              write(*,'(a,5i6,2f9.4)')'PT - COMPARE EFD SeasonalCF,y,i,r,int,grp, New ',RYR,CURITR,NERC, INT,EFD_SEA,  CFefdSEA(INT,EFD_SEA)

               WRITE(18,9313) CURIRUN, CURIYR+1989, CURITR, STO_YR+1989, RYR+1989, NERC, INT, EFD_SEA, STO_CFefdSEA(INT,EFD_SEA,NERC), CFefdSEA(INT,EFD_SEA), SumCF_EFDs(EFD_SEA), SumHrs_EFDs(EFD_SEA)
 9313          FORMAT(1X,"SEASONAL_EFD_CF",8(",",I5),4(",",F21.6))

            enddo

            DO m = 1, 12
               CFmonth(INT,m,NERC) = SumCF_month(m) / SumHrs_month(m)
            END DO

         ELSE

            EPIACF(INT) = 0.001
            DO m = 1, 12
               CFmonth(INT,m,NERC) = 0.001
            END DO
         ENDIF


!     MAP CAP FACTORS INTO LOAD GROUPS FOR REGION

      DO IL = 1,EPNSTP(1)
         LDGRP = EPLDGR(IL,1)
            ISEG = EPLDSG(IL,1)
            ICAP = UCPINTI(INT)
            IR_CAP = UIRINTI(INT)
            IF (RYR.LT.(UPSTYR-UHBSYR)) THEN
               EPICFC(INT,IL) = CFecpGRP(INT,LDGRP)
               UPICFC(0,INT,LDGRP,ISEG) = CFecpGRP(INT,LDGRP)
            ELSE
               EPICFC(INT,IL) = CFecpGRP(INT,IL)
               UPICFC(0,INT,LDGRP,ISEG) = CFecpGRP(INT,IL)
            ENDIF

            write(22,9314) curirun, curiyr+1989, curitr, nerc, IL, LDGRP, ISEG, INT, EPIGCF(INT,LDGRP), CFecpGRP(INT,IL), EPICFC(INT,IL), UPICFC(0,INT,LDGRP,ISEG), SumCF_ECPg(IL), SumHrs_ECPg(IL)
!           write(6,9314) curirun, curiyr+1989, curitr, nerc, IL, LDGRP, ISEG, INT, EPIGCF(INT,LDGRP), CFecpGRP(INT,IL), EPICFC(INT,IL), UPICFC(0,INT,LDGRP,ISEG), SumCF_ECPg(IL), SumHrs_ECPg(IL)
 9314       FORMAT(1X,"DBG_EPICFC",8(",",i5),6(",",f21.6))

         ENDDO
      END DO      ! end of INT loop

      DO EFD_GRP =1,UTNGRP
         DO ISEG = 1, UTNSEG
            DO INT = 1, ECP_D_INT
               ICAP = UCPINTI(INT)
               IR_CAP = UIRINTI(INT)

               write(22,'(a,6i6,3(f12.4,1x))')'DBG EFD CFS,y,i,r,int,grp, Orig, New ',CURIYR,CURITR,NERC, INT,ISEG, EFD_GRP,   HREFDCF(0,INT,1,ISEG,EFD_GRP,NERC)

            ENDDO
         ENDDO
      ENDDO

      DO INT = 1,ECP_D_INT
	  
		 ! setting EPIRCCR to default value for historical years, only affects ULREVS calculation. EPIRCCR will be overwritten later in ECP when ECP is running (CURIYR >= UPSTYR).
		 EPIRCCR(UIRINTI(INT)) = EPIACF(INT) 
		 WRITE(22,999) CURIRUN, CURIYR+1989, CURITR, INT, UIRINTI(INT), NERC, EPIRCCR(UIRINTI(INT))

         DO IL = 1,EPNSTP(1)
            LDGRP = EPLDGR(IL,1)
            ISEG = EPLDSG(IL,1)
            IF (EPICFC(INT,IL) .EQ. 0.0) EPICFC(INT,IL) = 0.00001
            IF (UPICFC(0,INT,LDGRP,ISEG) .EQ. 0.0) UPICFC(0,INT,LDGRP,ISEG) = 0.00001
         ENDDO

         IF ((UIRINTI(INT)) .EQ. 0.0) EPIRCCR(UIRINTI(INT)) = 0.0    !   0001
         IF (EPIACF(INT) .EQ. 0.0) EPIACF(INT) = 0.00001

  999    Format(1X,'Intermittent_CCRED',6(':',I4),1(':',F10.5))
      ENDDO

      RETURN
      END

!     RENEFD

      SUBROUTINE RENEFD(NERC)

      IMPLICIT NONE

!     THIS SUBROUTINE REPLACES EFD VALUES WITH VALUES FROM RENEW

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'dispin'
      include'control'
      include'bildin'
      include'ecpcntl'
      include'elcntl'
      include'dispcrv'
      include'dsmdimen'
      include'dsmtoefd'
      include'wrenew'
      include'elout'
      include 'dispinyr'
      include'dsmtfecp'
      include'uefdout'
      include'cdsparms'
      include'emission'
      include'csapr'
      include'emmemis'

      INTEGER*4 IRNW,JRNW,RYR,NERC,IREN,ISP,IECP,TFR,IPGRP,XYR
      INTEGER*4 IGRP,LGRP,ISEG,INT,ITEST
      INTEGER IVIN
      REAL INTAVGCF, INTAVGCAP,AVGSEACF(EFD_D_MSP)

      COMMON/BNCHGEN/ BSNCGEN,BSHYGEN,BSSOGEN,BSPVGEN,BSWNGEN,BSGTGEN,BMNCLYR,BMHYLYR,BMSOLYR,BMPVLYR,BMWNLYR,BMGTLYR
      REAL BSNCGEN,BSHYGEN,BSSOGEN,BSPVGEN,BSWNGEN,BSGTGEN
      INTEGER BMNCLYR,BMHYLYR,BMSOLYR,BMPVLYR,BMWNLYR,BMGTLYR
!
!     INITIALIZE RENEWABLE GENERATION FOR HY, WN, PV, SO THAT ARE BENCHMARKED
!
      EXSGEN(WIHY,NERC) = 0.0
      EXSGEN(WIWN,NERC) = 0.0
      EXSGEN(WIWL,NERC) = 0.0
      EXSGEN(WIPV,NERC) = 0.0
      EXSGEN(WIPT,NERC) = 0.0
      EXSGEN(WISO,NERC) = 0.0

!     OVERWRITE EFD RENEWABLE DATA WITH RENEW VALUES

      RYR = CURIYR

      DO IRNW = 1, ECNTP
         TFR = EPNFLRG(ECCR(IRNW),ECLR(IRNW),ECGR(IRNW),ECAR(IRNW))
         IPGRP = ECDBID(IRNW)
         IECP = ECTECP(IRNW)
         IVIN = ECVIN(IRNW)
         IREN = 0

!        check for geothermal or biomass plant

         IF (IECP .EQ. WIGT .OR. IECP .EQ. WIAG .OR. IECP .EQ. WIWD .OR. IECP .EQ. WIBI ) THEN
            JRNW = UCPRNWIS(IECP)
            IREN = UIRRNWI(JRNW)
         END IF

         IF (IREN .GT. 0) THEN

!           overwrite geo with aer heat rate values

            IF (IECP .EQ. WIGT .OR. IECP .EQ. WIAG) THEN

               ITEST = 1
!              WRITE(18,3391) CURIRUN, CURIYR+1989, CURITR, IRNW, ITEST, IREN, IPGRP, IECP, ULECPT(IPGRP), TFR, ULFRGN(IPGRP), NERC, ULORGN(IPGRP), (ECCAP(IRNW,ISP),ISP=1,EENSP), &
!                 (ULSCAP_ECP(IPGRP,XYR),XYR=1,UNXPH), (ULHTRT_ECP(IPGRP,XYR),XYR=1,UNXPH), UPHTRT(IECP), EPHTRT_AER(IECP), (ULSCAP_EFD(IPGRP,ISP), ULHTRT_EFD(IPGRP,ISP),ISP=1,EENSP)

               DO XYR = 1 , UNXPH
                  ULHTRT_ECP(IPGRP,XYR) = EPHTRT_AER(IECP)
               END DO

               DO ISP = 1 , EENSP
                  ULHTRT_EFD(IPGRP,ISP) = EPHTRT_AER(IECP)
               END DO
            ELSE           !biomass  only overwrites historical biomass heat rate, using plant file hrates for forecast

               ITEST = 2
!              WRITE(18,3391) CURIRUN, CURIYR+1989, CURITR, IRNW, ITEST, IREN, IPGRP, IECP, ULECPT(IPGRP), TFR, ULFRGN(IPGRP), NERC, ULORGN(IPGRP), (ECCAP(IRNW,ISP),ISP=1,EENSP), &
!                 (ULSCAP_ECP(IPGRP,XYR),XYR=1,UNXPH), (ULHTRT_ECP(IPGRP,XYR),XYR=1,UNXPH), UPHTRT(IECP), EPHTRT_AER(IECP), (ULSCAP_EFD(IPGRP,ISP), ULHTRT_EFD(IPGRP,ISP),ISP=1,EENSP)
!3391          FORMAT(1X,"HTRT_RENEFD",13(":",I6),<EENSP>(":",F21.6),<UNXPH>(":",F21.6),<UNXPH>(":",F21.6),2(":",F21.6),<EENSP>(":",F21.6,":",F21.6))


             IF (((CURIYR + UHBSYR) .LE. UYR_OVER)     &
               .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN
               DO XYR = 1 , UNXPH
                   ULHTRT_ECP(IPGRP,XYR) = HWODHR(CURIYR)
               END DO

               DO ISP = 1 , EENSP
                   ULHTRT_EFD(IPGRP,ISP) = HWODHR(CURIYR)
               END DO
            ENDIF
           ENDIF
         END IF
      END DO

      DO IRNW = 1 , EHNTP
         TFR = EPNFLRG(EHCR(IRNW),EHLR(IRNW),EHGR(IRNW),EHAR(IRNW))
         IECP = EHTECP(IRNW)
         IGRP = EHDBID(IRNW)
         IVIN = EHVIN(IRNW)
         IREN = 0

!        IF not equal to storage plant then revise heat rates?

         IF ( IECP .NE. WIOS .OR. IECP .NE. WIDS .OR. IECP .NE. WISQ .OR. IECP .NE. WIP2 ) THEN

           IF ( (IECP .GT. ECP_D_DSP) .AND. (IECP .LE. ECP_D_DSP + ECP_D_RNW)) THEN
            JRNW = UCPRNWIS(IECP)
            IREN = UIRRNWI(JRNW)
           ELSEIF ((IECP .GT. ECP_D_DSP + ECP_D_RNW + ECP_D_STO) .and. (IECP .LE. ECP_D_CAP - ECP_D_DGN )) THEN
            JRNW = UCPINTIS(IECP)
            IREN = UIRINTI(JRNW)
         END IF

         IF (IREN .GT. 0) THEN

!           Overwrite geo,hydro,wind and solar heat rates with aer heat rate values

            IF (IECP .NE. WIMS ) THEN

               ITEST = 3
!              WRITE(18,3391) CURIRUN, CURIYR+1989, CURITR, IRNW, ITEST, IREN, IGRP, IECP, ULECPT(IGRP), TFR, ULFRGN(IGRP), NERC, ULORGN(IGRP), (EHCAP(IRNW,ISP),ISP=1,EENSP), &
!                 (ULSCAP_ECP(IGRP,XYR),XYR=1,UNXPH), (ULHTRT_ECP(IGRP,XYR),XYR=1,UNXPH), UPHTRT(IECP), EPHTRT_AER(IECP), (ULSCAP_EFD(IGRP,ISP), ULHTRT_EFD(IGRP,ISP),ISP=1,EENSP)

               DO XYR = 1 , UNXPH
                  ULHTRT_ECP(IGRP,XYR) = EPHTRT_AER(IECP)
               END DO

               DO ISP = 1 , EENSP
                  ULHTRT_EFD(IGRP,ISP) = EPHTRT_AER(IECP)
               END DO

            ELSE          ! msw only overwrite historical MSW hrates, using plant file heat rates for forecast

               ITEST = 4
!              WRITE(18,3391) CURIRUN, CURIYR+1989, CURITR, IRNW, ITEST, IREN, IGRP, IECP, ULECPT(IGRP), TFR, ULFRGN(IGRP), NERC, ULORGN(IGRP), (EHCAP(IRNW,ISP),ISP=1,EENSP), &
!                 (ULSCAP_ECP(IGRP,XYR),XYR=1,UNXPH), (ULHTRT_ECP(IGRP,XYR),XYR=1,UNXPH), UPHTRT(IECP), EPHTRT_AER(IECP), (ULSCAP_EFD(IGRP,ISP), ULHTRT_EFD(IGRP,ISP),ISP=1,EENSP)

             IF (((CURIYR + UHBSYR) .LE. UYR_OVER)     &
               .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN
               DO XYR = 1 , UNXPH
                   ULHTRT_ECP(IPGRP,XYR) = HMSWHR(CURIYR)
               END DO

               DO ISP = 1 , EENSP
                   ULHTRT_EFD(IPGRP,ISP) = HMSWHR(CURIYR)
               END DO
             ENDIF
            END IF
            END IF
         END IF

         JRNW = EHHYTP(IRNW)

!        FOR SELECTED SCENARIOS (SUCH AS CARBON STABILIZATION) ADJUST HYDRO UTILIZATION RATES

         IF (EPPLCD(JRNW) .EQ. 'HYC' .AND. USW_HYD .GT. 0)THEN
            DO ISP = 1,EENSP
               EHHYCF(IRNW,ISP) = EHHYCF(IRNW,ISP) * UQHYFAC(CURIYR,NERC)
            END DO
         END IF

!        STEOBM calculate baseline hydro gen for benchmarking

         IF (IECP .EQ. WIHY) THEN
            DO ISP = 1,EENSP
               BSHYGEN = BSHYGEN + EHCAP(IRNW,ISP) * 0.001 * (EHHYCF(IRNW,ISP) * 0.001) * EETIME(ISP) * 0.001
               IF (IVIN .EQ. 1)THEN
               EXSGEN(IECP,NERC) = EXSGEN(IECP,NERC) + EHCAP(IRNW,ISP) * 0.001 * (EHHYCF(IRNW,ISP) * 0.001) * EETIME(ISP) * 0.001
               EXSGEN(IECP,MNUMNR) = EXSGEN(IECP,MNUMNR) + EHCAP(IRNW,ISP) * 0.001 * (EHHYCF(IRNW,ISP) * 0.001) * EETIME(ISP) * 0.001
               END IF
            ENDDO
         ENDIF

!        IF NON-INTERMITTENT RENEWABLE MAP RENEWABLE EFD SEASONAL CAPACITY FACTORS INTO EFD LOAD SEGMENTS

         IF (IECP .EQ. WIMS .OR. IECP .EQ. WIHY .OR. IECP .EQ. WIPS ) THEN

            DO LGRP = 1 , UTNGRP
               ISP = UTSEAS(LGRP)
               DO ISEG = 1 , UTNSEG
                  EHLDCF(IRNW,ISEG,LGRP) = EHHYCF(IRNW,ISP)
               ENDDO
            ENDDO

!           IF INTERMITTENT RENEWABLE MAP INTERMITTENT EFD SEASONAL CAPACITY FACTORS/ECP CAP FACTORS BY ECP LOAD INTO EFD LOAD SEGMENTS

         ELSEIF ((IECP .GT. ECP_D_DSP + ECP_D_RNW + ECP_D_STO) .and. (IECP .LE. ECP_D_CAP - ECP_D_DGN )) THEN

            INT = IECP - ECP_D_DSP - ECP_D_RNW - ECP_D_STO

            !INTAVGCF = 0.0
            !INTAVGCAP = 0.0
            !DO ISP = 1, EENSP
            !   INTAVGCF = INTAVGCF + EHHYCF(IRNW,ISP) * EHCAP(IRNW,ISP) * EETIME(ISP) / 8760.0
            !   INTAVGCAP = INTAVGCAP + EHCAP(IRNW,ISP) * EETIME(ISP) / 8760.0
            !ENDDO
            !IF (INTAVGCAP .NE. 0.0) THEN
            !    INTAVGCF = INTAVGCF / INTAVGCAP
            !ELSE
            !    INTAVGCF = INTAVGCF / EENSP
            !ENDIF

            !AVGSEACF = 0.0
            DO LGRP = 1 , UTNGRP
               ISP = UTSEAS(LGRP)
               DO ISEG = 1 , UTNSEG
!                 EHLDCF(IRNW,ISEG,LGRP) = EHHYCF(IRNW,ISP) * HREFDCF(0,INT,ISEG,LGRP,NERC)  / CFefdSEA(INT,ISP)
                  IF (IECP .EQ. WIPT) THEN
                     EHLDCF(IRNW,ISEG,LGRP) = HREFDCF(0,INT,1,ISEG,LGRP,NERC)*1000
                  ELSE
                     EHLDCF(IRNW,ISEG,LGRP) = EHHYCF(IRNW,ISP) * HREFDCF(0,INT,1,ISEG,LGRP,NERC)  / STO_CFefdSEA(INT,ISP,NERC) !INTAVGCF * HREFDCF(0,INT,1,ISEG,LGRP,NERC)  / EPIACF(INT)
                  ENDIF
                  EHLDCF(IRNW,ISEG,LGRP) = MIN(999,EHLDCF(IRNW,ISEG,LGRP))        !keep below 1000 (1.0) for EFD dispatch

                  !AVGSEACF(ISP) = AVGSEACF(ISP) + EHLDCF(IRNW,ISEG,LGRP) * UTWDTH(ISEG,LGRP)

!                 STEOBM calculate baseline intermittent gen for benchmarking

                  IF (IECP .EQ. WIWN .OR. IECP .EQ. WIWL .OR. IECP .EQ. WIWF) THEN
                     BSWNGEN = BSWNGEN + EHCAP(IRNW,ISP) * 0.001 * (EHLDCF(IRNW,ISEG,LGRP) * 0.001) * UTWDTH(ISEG,LGRP) * 0.001
                     IF (IVIN .EQ. 1)THEN
                     EXSGEN(IECP,NERC) = EXSGEN(IECP,NERC) + EHCAP(IRNW,ISP) * 0.001 * (EHLDCF(IRNW,ISEG,LGRP) * 0.001) * UTWDTH(ISEG,LGRP) * 0.001
                     EXSGEN(IECP,MNUMNR) = EXSGEN(IECP,MNUMNR) + EHCAP(IRNW,ISP) * 0.001 * (EHLDCF(IRNW,ISEG,LGRP) * 0.001) * UTWDTH(ISEG,LGRP) * 0.001
                     END IF
                  ELSE IF (IECP .EQ. WISO) THEN
                     BSSOGEN = BSSOGEN + EHCAP(IRNW,ISP) * 0.001 * (EHLDCF(IRNW,ISEG,LGRP) * 0.001) * UTWDTH(ISEG,LGRP) * 0.001
                     IF (IVIN .EQ. 1)THEN
                     EXSGEN(IECP,NERC) = EXSGEN(IECP,NERC) + EHCAP(IRNW,ISP) * 0.001 * (EHLDCF(IRNW,ISEG,LGRP) * 0.001) * UTWDTH(ISEG,LGRP) * 0.001
                     EXSGEN(IECP,MNUMNR) = EXSGEN(IECP,MNUMNR) + EHCAP(IRNW,ISP) * 0.001 * (EHLDCF(IRNW,ISEG,LGRP) * 0.001) * UTWDTH(ISEG,LGRP) * 0.001
                     END IF
                  ELSE IF (IECP .EQ. WIPV .OR. IECP .EQ. WIPT) THEN
                     BSSOGEN = BSSOGEN + EHCAP(IRNW,ISP) * 0.001 * (EHLDCF(IRNW,ISEG,LGRP) * 0.001) * UTWDTH(ISEG,LGRP) * 0.001        !add together, only one solar total to bench
                     IF (IVIN .EQ. 1)THEN
                     EXSGEN(IECP,NERC) = EXSGEN(IECP,NERC) + EHCAP(IRNW,ISP) * 0.001 * (EHLDCF(IRNW,ISEG,LGRP) * 0.001) * UTWDTH(ISEG,LGRP) * 0.001
                     EXSGEN(IECP,MNUMNR) = EXSGEN(IECP,MNUMNR) + EHCAP(IRNW,ISP) * 0.001 * (EHLDCF(IRNW,ISEG,LGRP) * 0.001) * UTWDTH(ISEG,LGRP) * 0.001
                     END IF
                  END IF

                   write(22,'(a,8i6,1x,7(f12.4,1x))') 'DBG EFD IN RENEW, ',CURIYR,CURITR,NERC, INT, IGRP,IRNW,ISEG, LGRP, EHCAP(IRNW,ISP),HREFDCF(0,INT,1,ISEG,LGRP,NERC), 0.001 * DBLE(EHLDCF(IRNW,ISEG,LGRP)), INTAVGCF, EPIACF(INT),UTWDTH(ISEG,LGRP),BSSOGEN
               ENDDO
            ENDDO

            DO ISP = 1, EENSP
              IF (EETIME(ISP) .NE. 0.0) &
              !EHHYCF(IRNW,ISP) = AVGSEACF(ISP) / EETIME(ISP)             !update seasonal CF to be consistent with time slice CF
              write(22,'(a,7i6,I10)') 'DBG EFD EHHYCF  , ',CURIYR,CURITR,NERC, INT, IGRP,IRNW,ISP, EHHYCF(IRNW,ISP)
            ENDDO

         ENDIF

      END DO
!
!     SUM UP ADJUSTED REGIONAL TOTALS
!
      IF (NERC .EQ. UNRGNS)THEN
         EXSGEN(WIHY,MNUMNR) = 0.0
         EXSGEN(WIWN,MNUMNR) = 0.0
         EXSGEN(WIWL,MNUMNR) = 0.0
         EXSGEN(WIPV,MNUMNR) = 0.0
         EXSGEN(WIPT,MNUMNR) = 0.0
         EXSGEN(WISO,MNUMNR) = 0.0
         DO TFR = 1 , UNRGNS
            EXSGEN(WIHY,MNUMNR) = EXSGEN(WIHY,MNUMNR) + EXSGEN(WIHY,TFR)
            EXSGEN(WIWN,MNUMNR) = EXSGEN(WIWN,MNUMNR) + EXSGEN(WIWN,TFR)
            EXSGEN(WIWL,MNUMNR) = EXSGEN(WIWL,MNUMNR) + EXSGEN(WIWL,TFR)
            EXSGEN(WIPV,MNUMNR) = EXSGEN(WIPV,MNUMNR) + EXSGEN(WIPV,TFR)
            EXSGEN(WIPT,MNUMNR) = EXSGEN(WIPT,MNUMNR) + EXSGEN(WIPT,TFR)
            EXSGEN(WISO,MNUMNR) = EXSGEN(WISO,MNUMNR) + EXSGEN(WISO,TFR)
         END DO
      END IF

      RETURN
      END

      SUBROUTINE IRAPHASE

      IMPLICIT NONE

!     THIS SUBROUTINE CALCULATES WHETHER IRA PHASE OUT HAS BEEN TRIGGERED BASE ON LAST CYCLE EMISSIONS AND 
!      UPDATES ANNUAL PTC/ITC VALUES IF PHASE OUT IS NEEDED

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'emission'
      include'uecpout'

      INTEGER IECP, IYR, APPYR, kyr
      REAL  EMIS_TGT
      
      EMIS_TGT = EMELC(MNUMCR,1,UPIRA_EMYR - UHBSYR) * UPIRA_EMPCT
      
      APPYR = 9999
      DO IYR = UESTYR-UHBSYR, MNUMYR
          IF (EMELC(MNUMCR,1,IYR) .LT. EMIS_TGT .AND. APPYR .GT. 9000) THEN !catch first year that target is passed
              APPYR = IYR 
             write(6,*) ' IRA APPYR = ',APPYR + UHBSYR,EMIS_TGT,EMELC(MNUMCR,1,IYR)             
          ENDIF
      ENDDO

      APPYR = MAX(APPYR,UPIRAYR0 - UHBSYR)         ! applicable year must be 2032 or later
      UPIRA_APPYR = APPYR + UHBSYR                 ! fill variable for res/comm - calendar year it has been hit
      
      IF (APPYR .EQ. 9999) THEN
          write(6,*) ' IRA APPYR NOT HIT'
      ELSE             !implement phase down 
          DO IECP = 1, ECP_D_CAP
              IF (UPSUBCAS(IECP) .EQ. 1 .AND. UPGSUBPT(IECP) .GT. 0) THEN    !PTC
                 IF (APPYR + UPCLYR(IECP) + 1 .LE. MNUMYR) THEN       !if first affected online year is before 2050, then adjust PTC 
                   DO IYR = APPYR + UPCLYR(IECP) + 1, MNUMYR
                     IF (IYR .EQ. APPYR + UPCLYR(IECP) + 1) THEN
                       UPGSUBYR(IECP,IYR) = UPGSUBYR(IECP,APPYR) * 0.75
                     ELSEIF (IYR .EQ. APPYR + UPCLYR(IECP) + 2) THEN
                       UPGSUBYR(IECP,IYR) = UPGSUBYR(IECP,APPYR) * 0.5
                     ELSE
                       UPGSUBYR(IECP,IYR) = 0.0
                     ENDIF
                   ENDDO
                 ENDIF
                 write(6,'(A10,I4,21F8.3)') 'PTCSUBYR ',IECP,(UPGSUBYR(IECP,kyr),KYR=41,61)
              ENDIF
! split these conditions up because hydro has UPSUBCAS=1 (PTC) for early years, but also has ITC in later years              
              IF (UPSUBCAS(IECP) .EQ. 2 .OR. UPCSBYR(IECP,UPIRAYR0-UHBSYR) .GT. 0.0) THEN          !ITC is active in 2032
                 IF (APPYR + UPCLYR(IECP) + 1 .LE. MNUMYR) THEN       !if first affected online year is before 2050, then adjust ITC 
                   DO IYR = APPYR + UPCLYR(IECP) + 1, MNUMYR
                     IF (IYR .EQ. APPYR + UPCLYR(IECP) + 1) THEN
                       UPCSBYR(IECP,IYR) = UPCSBYR(IECP,APPYR) * 0.75
                     ELSEIF (IYR .EQ. APPYR + UPCLYR(IECP) + 2) THEN
                       UPCSBYR(IECP,IYR) = UPCSBYR(IECP,APPYR) * 0.5
                     ELSE
                       UPCSBYR(IECP,IYR) = 0.0
                     ENDIF
                   ENDDO
                 ENDIF
                 write(6,'(A10,I4,21F8.3)') 'ITCSUBYR ',IECP,(UPCSBYR(IECP,kyr),KYR=41,61)
              ENDIF
          ENDDO
      ENDIF
      
      RETURN
      END