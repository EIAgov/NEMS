
***** $Header: m:/default/input/RCS/lfprep.gms,v 1.172 2020/11/06 20:19:15 EM4 Exp $

$if not set DirPath $set DirPath '.\'

$if not set INPUT_FILE_SET  $set INPUT_FILE_SET 'lfminset.gdx'
$if not set INPUT_FILE_PARAM $set INPUT_FILE_PARAM 'lfminput.gdx'
$if not set INPUT_FILE_PARAM_INVEST $set INPUT_FILE_PARAM_INVEST 'lfinvest.gdx'


*  Model control sets and parameters
Set
   Step                             Supply steps
;
$gdxin %DirPath%\input\%INPUT_FILE_SET%
$loaddc Step
$gdxin



set
  StepMDCRV(Step,WDCRVS)           Map Step to NDCRVS                           /set.step:set.WDCRVS/
  StepM11(Step,M11)                Map Step to M11                              /set.STEP:set.M11/
  StepMC(Step,MCSTEP)              Map Step to MCSTEP                           /set.STEP:set.MCSTEP/
  StepCR(Step,CRSTEP)              Map Step to CRSTEP                           /set.STEP:set.CRSTEP/
  StepCI(Step,CISTEP)              Map Step to CISTEP                           /set.STEP:set.CISTEP/
  StepS1(Step,S1)                  Map Step to S1                               /set.STEP:set.S1/
;

* Time-related structures
Sets
  t                                Model horizon
  tIdx                             Ordinal model year index
  CapExpYr(t)                      Year used for cap expansion costs
  ReportYr(t)                      FTAB reporting year
;
$gdxin %DirPath%\input\%INPUT_FILE_SET%
$loaddc t, tIdx, CapExpYr, ReportYr
$gdxin

set
  Period                           Planning periods
  PrcPeriod(Period)                Period used for product pricing (no capacity expansion)
  BldPeriod(Period)                Period for which capacity expansion decisions are retained
  ActivePeriod(Period)             Dynamic set which tells the model which periods to solve for
  ActivePeriod2(Period)            Alias for ActivePeriod dynamic set
  FuturePeriods(Period)            All periods after the first period
  InactivePeriod(Period)           All inactive periods
;
$gdxin %DirPath%\input\%INPUT_FILE_SET%
$loaddc Period, PrcPeriod, BldPeriod
$gdxin

* Only solve for all three periods in the first iteration
ActivePeriod(Period) = yes ;
if(not NCNTRL_FCRL=1,
  ActivePeriod('Per2') = no ;
  ActivePeriod('Per3') = no ;
);
ActivePeriod2(Period)  = ActivePeriod(Period) ;
InactivePeriod(Period) = not ActivePeriod(Period) ;

FuturePeriods(Period) = yes ;
FuturePeriods('Per1') = no ;

set
   RunYears(t)                      Years to run the model - will use NCNTRL_CURCALYR if not specified here
   CurrentYr(t)                     Current NEMS model year
   ModelYears(t)                    years starting at NCNTRL_CURYR through N_Yrs
   NFBaseYr(t)                      Base year for the Nelson-Farr refinery cost indicies

   tPer(t,Period)                   Map year to Period
   Per1_t_MNUMYR(Period,t,MNUMYR)   Map Per1 to current MNUMYR entry and t
   Per1_t_MNXYRS(Period,t,MNXYRS)   Map Per1 to current MNXYRS entry and t
   Per1_MNUMYR(Period,MNUMYR)       Map Per1 to current MNUMYR entry
   Per1_MNMYRF(Period,MNMYRF)       Map Per1 to current MNMYRF entry
   Per1_MNXYRS(Period,MNXYRS)       Map Per1 to current MNXYRS entry
   MNUMYR_MNXYRS(MNUMYR,MNXYRS)     Map MNUMYR to MNXYRS                   /set.MNUMYR:set.MNXYRS/


* the following is not used anymore????
* MPSYears cannot be empty! Set MPSYears to /2080/ if you don't want an MPS file. */
   MPSYears(t)                      Years to write an MPS file                                  /2011/

;
* Copy years set for use in summations
Alias(t,tt) ;
Alias(M2,M2a);

* LFMM sets that need to be defined in order to map NEMS time data to LFMM
Sets
   tMNUM(t,MNUMYR)                  Map t (LFMM) to MNUMYR (NEMS)                    /set.t:set.MNUMYR/
   tMNMYRF(t,MNMYRF)                Map t (LFMM) to current MNMYRF(NEMS)             /set.t:set.MNMYRF/
   ttNUMyr(tt,MNUMYR)               Map tt (LFMM) to MNUMYR (NEMS)                   /set.tt:set.MNUMYR/
   tMNXYRS(t,MNXYRS)                Map t (LFMM) to MNXYRS (NEMS)                    /set.t:set.MNXYRS/
;
set
  RefRegBiodieselImportsAllowed(RefReg) /1_REFREG, 2_REFREG, 3_REFREG, 4_REFREG, 7_REFREG, 8_REFREG/

*-----------------------------------------------------------------------------------------------------------
Parameter
   GDP(t)
   agGDP87(t)
   Treas10Y(t)
   CORPBaa(t)
;

Treas10Y(t) = sum(MNUMYR,MACOUT_MC_RMTCM10Y(MNUMYR)$tMNUM(t,MNUMYR)) ;
CORPBaa(t)  = sum(MNUMYR,MACOUT_MC_RMCORPBAA(MNUMYR)$tMNUM(t,MNUMYR)) ;

*  MACOUT_MC_JPGDP starts in 1987, but t starts at 1990, so the data needs to be offset by 3 to match up the year
loop((t,MNUMY3)$(ord(t)=ord(MNUMY3)-3),
  GDP(t) = MACOUT_MC_JPGDP(MNUMY3) ;
) ;

scalar
  MC_DETAIL1987 'temporary placeholder for 1987 ag deflator for Polysys'
;
  MC_DETAIL1987 = 0.97*MCDETAIL_MC_DETAIL('100','1990_MNUMYR');

loop((t,MNUMYR)$(ord(t)=ord(MNUMYR)),
  agGDP87(t) = MCDETAIL_MC_DETAIL('100',MNUMYR)/ MC_DETAIL1987 ;
) ;


scalar
  LastYear      'Last planning year index (ordinal year)'
;
LastYear                       = 61 ;

* Fill in values to grow at 2.2% per year after 2040
loop(t$(ord(t)>LastYear),
   GDP(t) = sum(tt$(ord(tt)=(ord(t)-1)), GDP(tt) )*1.022 ;
);
*-----------------------------------------------------------------------------------------------------------


parameter
  plant_counter
  WEIGHTEDPRICE(MNCRUD)
  wtiave(t)
  wtiavelag(t)
  hhave(t)
  hhavelag(t)
  hhcoef(t)
  wticoef
  lpgbase
;


Parameters
   P_yrs(period)                Number of years in each planning period /
                                    Per1 1
                                    Per2 1
                                    Per3 19 /
;

Scalars
   N_yrs                       Total number of years in the planning horizon

   MaxYear                     Largest possible planning horizon year (ordinal year)
   PrcPerIdx                   Ordinal index of pricing period in Period set
   BldPerIdx                   Ordinal index of build period in Period set
   CurYrIdx                    Ordinal index of current model year
   CurrentDeflator             Deflator to get from CurrentYr to ReportYr dollars
;

N_yrs                          = sum(Period, P_yrs(Period)) ;

MaxYear                        = LastYear + N_yrs ;
PrcPerIdx                      = sum((Period,PrcPeriod)$PrcPeriod(Period), ord(Period)) ;
BldPerIdx                      = sum((Period,BldPeriod)$BldPeriod(Period), ord(Period)) ;




Scalars
   MbblCD_2_BGY  Convert M BBLs per calendar day to billion gallons per year  / 0.01533 /
;

* Copy period set for use in summations
Alias(Period,Period2);

* For calibration purposes
if(DoCalibrate = 1,
  NCNTRL_CURCALYR = 2010 ;
) ;

RunYears(t) = t.val = NCNTRL_CURCALYR;
Per1_t_MNUMYR('Per1',t,MNUMYR) = tMNUM(t,MNUMYR)$Runyears(t);
Per1_t_MNXYRS('Per1',t,MNXYRS) = tMNXYRS(t,MNXYRS)$Runyears(t);
Per1_MNUMYR('Per1',MNUMYR)     = sum(t,tMNUM(t,MNUMYR)$Runyears(t));
Per1_MNMYRF('Per1',MNMYRF)     = sum(t,tMNMYRF(t,MNMYRF)$Runyears(t));
Per1_MNXYRS('Per1',MNXYRS)     = sum(t,tMNXYRS(t,MNXYRS)$Runyears(t));

tPER(t,'Per1')$(t.val <= NCNTRL_CURCALYR + P_yrs('Per1') - 1) = 1;
tPER(t,'Per2')$(t.val > NCNTRL_CURCALYR + P_yrs('Per1') - 1 and t.val <= NCNTRL_CURCALYR + P_yrs('Per1') + P_yrs('Per2') - 1) = 1;
tPER(t,'Per3')$(t.val > NCNTRL_CURCALYR + P_yrs('Per1') + P_yrs('Per2') - 1) = 1;

if(NCNTRL_FCRL=1,
  ModelYears(t)$(t.val >= NCNTRL_CURCALYR and t.val < NCNTRL_CURCALYR + N_Yrs) = yes;
else
  ModelYears(t)$(t.val  = NCNTRL_CURCALYR) = yes ;
);


* Region definitions and mappings
Sets
   CenDiv                           Census demand divisions
   ActiveDem(CenDiv)                Dynamic set for toggling active census demand divisions
   CoalDReg                         Coal demand regions
   CoalSReg                         Coal supply regions
   FuelRegion                       EMM fuel regions  / 1*25 /
   OGCO2Reg                         OGSM CO2 regions  / 1*7 /
   CO2_Source                       OGSM EOR CO2 sources
                                    / HYDROGEN
                                      AMMONIA
                                      ETHANOL
                                      NATURAL
                                      CEMENT
                                      HYDROGEN_REF
                                      PLANNED_POWER
                                      POWER
                                      NG_PROCESSING
                                      PLANNED_CTL
                                      CTL
                                      NEW_1
                                      NEW_2 /
;

$gdxin %DirPath%\input\%INPUT_FILE_SET%
$loaddc CenDiv, ActiveDem, CoalDReg, CoalSReg
$gdxin

Alias(OGCO2Reg,OGCO2Reg2);

set
   From                             Supply and census divisions that transport product   /set.CenDiv,set.RefReg/
   crN2L(CenDiv,MNUMCR)             Map Census Region CenDiv(LFMM) to MNUMCR(NEMS)       /set.CenDiv:set.MNUMCR/
   CenDiv_2_M11(CenDiv,M11)         Map Census Region to M11                             /set.CenDiv:set.M11/
   CoaN2L(CoalDReg,NDREGN)          Map CoalDReg(LFMM) to NDREGN(NEMS)                   /set.CoalDReg:set.NDREGN/
   CoaSN2L(CoalSReg,MX_NCL)         Map CoalSReg(LFMM) to MX_NCL(NEMS)                   /set.CoalSReg:set.MX_NCL/
   CoaSN3L(CoalSReg,MX_NCI)         Map CoalSReg(LFMM) to MX_NCI(NEMS)                   /set.CoalSReg:set.MX_NCI/
   CoalDReg_2_Census(CoalDReg,CenDiv)

   MNUMC2_2_MNUMCR(MNUMC2,MNUMCR)   Map MNUMC2 to MNUMCR                                 /set.MNUMC2:set.MNUMCR/
;
crN2L('CenDiv11','11_United_States')= no;
crN2L('CenDiv10','10_Unused')       = no;


* Create copy of the census division set for use in census-to-census transport
Alias(CenDiv,CenDivA);
Alias(ActiveDem,ActiveDemA);
Alias (MNUMCR, MNUMCR2);

* Create copy of the supply region set for use in RefReg-to-RefReg transport
Alias(RefReg,RefRegA);


*--------------------------------------------------------------------------------------------------
* sets read from lfminput.gdx

*  Non-NEMS Model set definitions
* These sets are populated from external data (lfminput.gdx, etc.)
Sets
  RefType
    PetRefType(RefType)         Petroleum and other non-ethanol refinery types
    EthRefType(RefType)         Refinery type that makes ethanol

  Process                       Process Units
    PetroleumProcess(Process)     Petroleum Refinery Processes
    FuelUse_Process(Process)      Processes that consume fuel in refineries
    AltFuel_Process(Process)      Non-ethanol alternative processes
    EthanolProcess(Process)       All ethanol processes
      CornProcess(EthanolProcess)   Corn ethanol processes

  ProcessMode                   Process Modes

  RcpMode                       Recipe modes for recipe-blended products

  rmBlendGas(RcpMode)
    rmBlendGasCFG10(rmBlendGas)
    rmBlendGasRFG10(rmBlendGas)
    rmBlendGasCarBOB10(rmBlendGas)
  rmBlendDist(RcpMode)
  rmBlendResid(RcpMode)

  Stream                        All input and intermedidate and product streams
    IntStream(Stream)             Intermedidate streams from ProcessTables table
    RecipeProd(Stream)            recipe-blended products that have demands
    RecipeOut(Stream)             outputs of RecipeBlending (EndProduct+CoProduct)
    NGLProduct(Stream)            NGL output products

    RecipeInputs(Stream)          Inputs to recipe blending

    AB32_Stream(Stream)           Streams relevant to AB-32




  NonCrudeUtil(IntStream)       'Non-crude, non-utility intermediate streams'

  Crude(IntStream)              Crude input streams
  CrudeAll                      Crude + 'All' + 'None'

  CoalStr(IntStream)            Coal stream

  RefInputStr(IntStream)          Refinery input streams
    NGLInputStr(RefInputStr)      NGL input streams (purchased NGPL)
    Purchase_Streams(RefInputStr) liquid refinery input purchase streams
    AltPurchase_Streams(RefInputStr)

  BioStr(IntStream)             Biomass streams
  EthStream(IntStream)          ethanol streams
    EthStreamRFS(EthStream)     ethanol streams that count towards the RFS
    EthStreamNoRFS(EthStream)   ethanol streams that do not count towards the RFS

* NoMat is used in lfreport.gms
  NoMat(stream)                 non-material streams

  Utility(IntStream)            Utility streams

  FuelUse_Streams(Stream)       Note: does not include purchased nat gas

  EndProduct(Stream)            end-products that satisfy demands
    EndProductGas(EndProduct)     gasoline end products
    RecipeEnd(EndProduct)         end products that came from recipe blending

  CoProduct(Stream)             refinery co-products
    CoProductSales(CoProduct)   refinery co-products that can be sold
    EthCoProduct(CoProduct)     ethanol process co-products


  Property                      stream properties
    GasProp(Property)           properties relevant to gasoline blending
      GasSulfurSpec(GasProp)
    DistProp(Property)          properties relevent to distillate blending
     DistSulfurSpec(DistProp)
    ResidProp(Property)         properties relevant to resid blending
     ResidSulfurSpec(ResidProp)

  SpecProd(Stream)              specification-blended products
    GasSpecProd(SpecProd)       specification blended gasoline products
    DistSpecProd(SpecProd)      specification blended distillate products
    ResidSpecProd(SpecProd)     specification blended residuum products

  TranMode                      Tranportion modes between supply and demand regions
    MarineMode(TranMode)        Marine vessel or barge modes
    NonMarineMode(TranMode)     Transport modes that are not marine vessel or barge modes

  RefType                       Refinery type classification
  Source                        Foreign crude region

  InvParam                      Investment factor parameter names

  RFSCategory                   RFS categories
  RFSCategoryA(RFSCategory)     RFS categories (alias)
  RFSCategoryWaiver(RFSCategory,RFSCategory)

  MarkupSector                  End-use sectors for fuel markups
  MarkupFuel                    End-use fuel types for fuel markups

  FCOType                       Types of seed oil

  LrnPhase                      Phase categories for technology learning
  LrnSpeed                      Speed categories for technology learning
  LrnParam                      Parameter categories for technology learning
  MoreLrnParam                  More parameter categories for technology learning
  LearningProcess(Process)      Set of processes that have learning

  BldStep(Step)                 Capacity expansion steps

  BldRiskClass                         Risk classes for capacity expansion cost calculations
  ProcessRisk(Process,BldRiskClass)    Mapping of processes to risk classes

* WARNING: set RefReg is defined in lf_nem.gdx
  DomRefReg(RefReg)           Domestic refining regions

  DieselSector            'Diesel use sectors'
  LPGSector               'Sectors for LPG pricing'

  noRFSModes(ProcessMode) 'Corn modes that do not count towards the RFS for the 15 bgy restriction'

* WARNING: set OGDIST is defined in lf_nem.gms
  NGPL_2_RefReg(OGDIST,RefReg)

  state                   States

;

$gdxin %DirPath%\input\%INPUT_FILE_SET%
*loaded from NEM_TO_LFMM1.gdx: RefReg
$loaddc DomRefReg
$loaddc RefType, PetRefType, EthRefType
$Loaddc Process, PetroleumProcess, AltFuel_Process, EthanolProcess, CornProcess, FuelUse_Process
$loaddc ProcessMode, noRFSModes
$loaddc Property, GasProp, DistProp, ResidProp
$loaddc GasSulfurSpec, DistSulfurSpec, ResidSulfurSpec
$Loaddc Stream, IntStream, EndProduct, CoProduct, CoProductSales, RecipeOut, NGLProduct, NonCrudeUtil, NoMat, CoalStr
$Loaddc SpecProd, GasSpecProd, DistSpecProd, ResidSpecProd
$loaddc EndProductGas
$loaddc RcpMode
$loaddc rmBlendGas, rmBlendGasCFG10, rmBlendGasRFG10, rmBlendGasCarBOB10
$loaddc rmBlendDist
$loaddc rmBlendResid
$Loaddc RecipeProd, InvParam, NFBaseYr
$loaddc Crude, CrudeAll, Source
$loaddc TranMode, MarineMode, NonMarineMode, FuelUse_Streams, Utility,
$loaddc RefInputStr, NGLInputStr, Purchase_Streams, AltPurchase_Streams, BioStr
$Loaddc RFSCategory, RFSCategoryA, RFSCategoryWaiver
$loaddc EthStream, EthStreamRFS, EthStreamNoRFS, EthCoProduct
$Loaddc MarkupSector, MarkupFuel, CoalDReg_2_Census, NGPL_2_RefReg
$Loaddc FCOType, LrnPhase, LrnSpeed, LrnParam, MoreLrnParam, LearningProcess, BldStep, BldRiskClass, ProcessRisk
$loaddc DieselSector, LPGSector
$loaddc RecipeInputs, RecipeEnd, state

$gdxin


*------------------------------------------------------------------------------------------------------------


*------------------------------------------------------------------------------------------------------------
* California LCFS stuff --- ask Niko Kydes for info on these sets
Sets
     LCFS_PetCategory                                                                     LCFS Petroleum Constraint Cetegories
     LCFS_BioProcess(Process)                                                             Process which produce Non-Petroleum Liquids
     LCFS_BioMode(ProcessMode)                                                            ProcessModes which produce Non-Petroleum Liquids
     LCFS_RefReg(RefReg)                                                                  Regions with Active LCFS Requirement
     LCFS_Years(t)                                                                        LCFS Years
     LCFS_BioStreams(Stream)                                                              Non-Petroleum Liquids counted in a LCFS Requirement
     LCFS_PetCategory_BioStreams(LCFS_PetCategory,LCFS_BioStreams)                        BioStreams by PetCategory
     LCFS_PetStreams(RecipeProd)                                                          Petroleum Streams counted in a LCFS Requirement
     LCFS_PetCategory_PetStreams(LCFS_PetCategory,LCFS_PetStreams)                        PetStreams by PetCategory
     LCFS_BioStreams_PetStreams(LCFS_BioStreams,LCFS_PetStreams)                          Petroleum Stream Substitute for Non-Petroleum Liquid
     LCFS_Vehicle_Types                                                                   Alternative Vehicle Types
     LCFS_PetCategory_Vehicle_Type(LCFS_PetCategory,LCFS_Vehicle_Types)                   PetCategory Vehicle Type Pairs
     LCFS_BioImports(EthStream)                                                           Imported Bio-Liquids
     LCFS_BioImport_PetStream(LCFS_PetStreams,LCFS_BioImports)                            Petroleum Stream Substitute for Imported Bio-Liquids
;

* temporary read LCFS sets from "param" GDX file until I get LCFS sets into .gms text files
$gdxin %DirPath%\input\%INPUT_FILE_PARAM%
$Load LCFS_PetCategory, LCFS_BioProcess, LCFS_BioMode
$Load LCFS_RefReg, LCFS_Years, LCFS_BioStreams, LCFS_PetCategory_BioStreams, LCFS_PetStreams
$Load LCFS_PetCategory_PetStreams, LCFS_BioStreams_PetStreams
$Load LCFS_Vehicle_Types, LCFS_PetCategory_Vehicle_Type
$Load LCFS_BioImports
$Load LCFS_BioImport_PetStream
$Load AB32_Stream
$gdxin

* end of "sets read from lfminput.gdx"
*--------------------------------------------------------------------------------------------------

*------------------------------------------------------------------------------------------------------------
* Oregon CFP (Clean Fuels Program) stuff, esh 3/17/2022
Sets
     CFP_PetCategory                                                                     CFP Petroleum Constraint Cetegories
     CFP_BioProcess(Process)                                                             Process which produce Non-Petroleum Liquids
     CFP_BioMode(ProcessMode)                                                            ProcessModes which produce Non-Petroleum Liquids
     CFP_RefReg(RefReg)                                                                  Regions with Active CFP Requirement
     CFP_Years(t)                                                                        CFP Years
     CFP_BioStreams(Stream)                                                              Non-Petroleum Liquids counted in a CFP Requirement
     CFP_PetCategory_BioStreams(CFP_PetCategory,CFP_BioStreams)                          BioStreams by PetCategory
     CFP_PetStreams(RecipeProd)                                                          Petroleum Streams counted in a CFP Requirement
     CFP_PetCategory_PetStreams(CFP_PetCategory,CFP_PetStreams)                          PetStreams by PetCategory
     CFP_BioStreams_PetStreams(CFP_BioStreams,CFP_PetStreams)                            Petroleum Stream Substitute for Non-Petroleum Liquid
     CFP_Vehicle_Types                                                                   Alternative Vehicle Types
     CFP_PetCategory_Vehicle_Type(CFP_PetCategory,CFP_Vehicle_Types)                     PetCategory Vehicle Type Pairs
     CFP_BioImports(EthStream)                                                           Imported Bio-Liquids
     CFP_BioImport_PetStream(CFP_PetStreams,CFP_BioImports)                              Petroleum Stream Substitute for Imported Bio-Liquids
;

* read CFP sets from "param" GDX file (lfpolicy.xlsx)
$gdxin %DirPath%\input\%INPUT_FILE_PARAM%
$Load CFP_PetCategory, CFP_BioProcess, CFP_BioMode
$Load CFP_RefReg, CFP_Years, CFP_BioStreams, CFP_PetCategory_BioStreams, CFP_PetStreams
$Load CFP_PetCategory_PetStreams, CFP_BioStreams_PetStreams
$Load CFP_Vehicle_Types, CFP_PetCategory_Vehicle_Type
$Load CFP_BioImports
$Load CFP_BioImport_PetStream
$gdxin

* Washington WACFS (Clean Fuels Standard) stuff, esh 11/4/24
Sets
     WACFS_PetCategory                                                                     WACFS Petroleum Constraint Cetegories
     WACFS_BioProcess(Process)                                                             Process which produce Non-Petroleum Liquids
     WACFS_BioMode(ProcessMode)                                                            ProcessModes which produce Non-Petroleum Liquids
     WACFS_RefReg(RefReg)                                                                  Regions with Active WACFS Requirement
     WACFS_Years(t)                                                                        WACFS Years
     WACFS_BioStreams(Stream)                                                              Non-Petroleum Liquids counted in a WACFS Requirement
     WACFS_PetCategory_BioStreams(WACFS_PetCategory,WACFS_BioStreams)                      BioStreams by PetCategory
     WACFS_PetStreams(RecipeProd)                                                          Petroleum Streams counted in a WACFS Requirement
     WACFS_PetCategory_PetStreams(WACFS_PetCategory,WACFS_PetStreams)                      PetStreams by PetCategory
     WACFS_BioStreams_PetStreams(WACFS_BioStreams,WACFS_PetStreams)                        Petroleum Stream Substitute for Non-Petroleum Liquid
     WACFS_Vehicle_Types                                                                   Alternative Vehicle Types
     WACFS_PetCategory_Vehicle_Type(WACFS_PetCategory,WACFS_Vehicle_Types)                 PetCategory Vehicle Type Pairs
     WACFS_BioImports(EthStream)                                                           Imported Bio-Liquids
     WACFS_BioImport_PetStream(WACFS_PetStreams,WACFS_BioImports)                          Petroleum Stream Substitute for Imported Bio-Liquids
;

* read WACFS sets from "param" GDX file (lfpolicy.xlsx)
$gdxin %DirPath%\input\%INPUT_FILE_PARAM%
$Load WACFS_PetCategory, WACFS_BioProcess, WACFS_BioMode
$Load WACFS_RefReg, WACFS_Years, WACFS_BioStreams, WACFS_PetCategory_BioStreams, WACFS_PetStreams
$Load WACFS_PetCategory_PetStreams, WACFS_BioStreams_PetStreams
$Load WACFS_Vehicle_Types, WACFS_PetCategory_Vehicle_Type
$Load WACFS_BioImports
$Load WACFS_BioImport_PetStream
$gdxin
* end of "sets read from lfminput.gdx"
*--------------------------------------------------------------------------------------------------
set
  LCFS_M2(LCFS_PetCategory,LCFS_C)  Map LCFS_PetCategory to LCFS_C index
                                 /set.LCFS_PetCategory:set.LCFS_C/

  CFP_M2(CFP_PetCategory,LCFS_C)     Map CFP_PetCategory to CFP_C index
                                 /set.CFP_PetCategory:set.LCFS_C/

  WACFS_M2(WACFS_PetCategory,LCFS_C)     Map WACFS_PetCategory to CFP_C index
                                 /set.WACFS_PetCategory:set.LCFS_C/

  Reg3CrdExp(Crude)      'crude export fr RR3 to Canada'
                                 / UL_Sweet   'composite crude 10'
                                   Condensate 'composite crude 11' /

;

*--------------------------------------------------------------------------------------------------
* Mapping tables to map NEMS sets to LFMM sets
set
MNPROD2Str(MNPROD,Stream)   Map MNPROD to stream entries
                                              / 01_MNPROD . ASPHout
                                                02_MNPROD . AVGout
                                                03_MNPROD . CaRBOBout
                                                04_MNPROD . CarbDSUout
                                                05_MNPROD . CFGout
                                                06_MNPROD . DSLout
                                                07_MNPROD . DSUout
                                                08_MNPROD . JTAout
                                                09_MNPROD . LUBout
                                                10_MNPROD . N2Hout
                                                11_MNPROD . N6Bout
                                                12_MNPROD . N6Iout
                                                13_MNPROD . PCFout
                                                14_MNPROD . RFGout
                                                15_MNPROD . CBOB
                                                16_MNPROD . RBOB
                                                17_MNPROD . MET
                                                18_MNPROD . AR3
                                                19_MNPROD . GO3
                                                20_MNPROD . MN3  /

* NOTE:  ECR is only a placeholder until we redefine BioStr to include FRR (Fed forest residue)
Bio2MNUMFS(BioStr,MNUMFS)  Map LFMM and NEMS biomass streams
                                               /  UWW.1_MNUMFS
                                                  ECR.2_MNUMFS
                                                  AGR.3_MNUMFS
                                                  NFR.4_MNUMFS     /

* NOTE:  ECR is only a placeholder until we redefine BioStr to include FRR (Fed forest residue)
Bio2MNMFS1(BioStr,MNMFS1)   Map LFMM and NEMS biomass streams
                                               /  UWW.2_MNMFS1
                                                  ECR.3_MNMFS1
                                                  AGR.4_MNMFS1
                                                  NFR.5_MNMFS1     /


M10_2_RefReg(M10,RefReg)     Map generic header to LFMM refining regions
                                               /set.M10:set.RefReg/

M10_2_MNUMPR(M10,MNUMPR)     Map generic header to PADD regions
                                               /set.M10:set.MNUMPR/

MNCRUD_2_Crude(MNCRUD,Crude) Map NEMS crude types to LFMM crude types
                                               / 01_MNCRUD . L_Sweet
                                                 02_MNCRUD . L_Sour
                                                 03_MNCRUD . M_Msour
                                                 04_MNCRUD . M_Sour
                                                 05_MNCRUD . H_Sweet
                                                 06_MNCRUD . H_Sour
                                                 07_MNCRUD . Calif
                                                 08_MNCRUD . Syncrude
                                                 09_MNCRUD . Dilbit
                                                 10_MNCRUD . UL_Sweet
                                                 11_MNCRUD . Condensate /

RefReg2MNUMPR(MNUMPR,RefReg)      Map PADD regions to LFMM refining regions
                                               / 01_MNUMPR . 1_RefReg
                                                 02_MNUMPR . 2_RefReg
                                                 03_MNUMPR . 3_RefReg
                                                 04_MNUMPR . 4_RefReg
                                                 05_MNUMPR . 5_RefReg
                                                 06_MNUMPR . 6_RefReg
                                                 07_MNUMPR . 7_RefReg
                                                 08_MNUMPR . 8_RefReg
                                                 09_MNUMPR . 9_RefReg /


CoalDReg2NDRGN1(CoalDReg,NDRGN1)  Map LFMM coal regions to NDRGN1 set for TOTAL in 1
                                               / CoalDReg1  . 02_NDRGN1
                                                 CoalDReg2  . 03_NDRGN1
                                                 CoalDReg3  . 04_NDRGN1
                                                 CoalDReg4  . 05_NDRGN1
                                                 CoalDReg5  . 06_NDRGN1
                                                 CoalDReg6  . 07_NDRGN1
                                                 CoalDReg7  . 08_NDRGN1
                                                 CoalDReg8  . 09_NDRGN1
                                                 CoalDReg9  . 10_NDRGN1
                                                 CoalDReg10 . 11_NDRGN1
                                                 CoalDReg11 . 12_NDRGN1
                                                 CoalDReg12 . 13_NDRGN1
                                                 CoalDReg13 . 14_NDRGN1
                                                 CoalDReg14 . 15_NDRGN1
                                                 CoalDReg15 . 16_NDRGN1
                                                 CoalDReg16 . 17_NDRGN1 /

CoalDReg2NDRGN17(CoalDReg,NDRGN1)  Map LFMM coal regions to NDRGN1 set for TOTAL in 17
                                               / CoalDReg1  . 01_NDRGN1
                                                 CoalDReg2  . 02_NDRGN1
                                                 CoalDReg3  . 03_NDRGN1
                                                 CoalDReg4  . 04_NDRGN1
                                                 CoalDReg5  . 05_NDRGN1
                                                 CoalDReg6  . 06_NDRGN1
                                                 CoalDReg7  . 07_NDRGN1
                                                 CoalDReg8  . 08_NDRGN1
                                                 CoalDReg9  . 09_NDRGN1
                                                 CoalDReg10 . 10_NDRGN1
                                                 CoalDReg11 . 11_NDRGN1
                                                 CoalDReg12 . 12_NDRGN1
                                                 CoalDReg13 . 13_NDRGN1
                                                 CoalDReg14 . 14_NDRGN1
                                                 CoalDReg15 . 15_NDRGN1
                                                 CoalDReg16 . 16_NDRGN1 /

RefReg_2_FuelRegion(RefReg,FuelRegion)   Map coal demand regions to EMM fuel regions
                                               / 1_RefReg . 3
                                                 2_RefReg . 18
                                                 3_RefReg . 6
                                                 4_RefReg . 18
                                                 5_RefReg . 18
                                                 6_RefReg . 20
                                                 7_RefReg . 24
                                                 8_RefReg . 24
                                                 9_RefReg . 24  /

FuelRegion_2_MAXNFR(FuelRegion,MAXNFR)   Map GAMS fuel regions to EMM fuel regions
                                               / set.FuelRegion:set.MAXNFR /

FuelRegion_2_MAXNF2(FuelRegion,MAXNF2)   Map GAMS fuel regions to EMM fuel regions
                                               / set.FuelRegion:set.MAXNF2 /

OGCO2Reg_2_M8(OGCO2Reg,M8)               Map GAMS OGSM CO2 regions to M8
                                               / set.OGCO2Reg:set.M8 /

CO2_Source_2_M13(CO2_Source,M13)         Map OGSM EOR CO2 sources to M13
                                               / set.CO2_Source:set.M13 /

M4_2_RFSCat(M4,RFSCategory)       Map RFS categories to M4 set
                                               / 1_M4 . Total
                                                 2_M4 . Advanced
                                                 3_M4 . Cellulosic
                                                 4_M4 . Biodiesel   /

M57_2_Process(M57,Process)        Map refinery processes to M57 set
                                              / 01_M57.ACU
                                                02_M57.VCU
                                                03_M57.KRD
                                                04_M57.SDA
                                                05_M57.HCD
                                                06_M57.FCC
                                                07_M57.ALK
                                                08_M57.C4I
                                                09_M57.CPL
                                                10_M57.PHI
                                                11_M57.TRI
                                                12_M57.RCR
                                                13_M57.RSR
                                                14_M57.BSA
                                                15_M57.DDA
                                                16_M57.DDS
                                                17_M57.GDS
                                                18_M57.NDS
                                                19_M57.FDS
                                                20_M57.GDT
                                                21_M57.DC4
                                                22_M57.DC5
                                                23_M57.FGS
                                                24_M57.LNS
                                                25_M57.SGP
                                                26_M57.UGP
                                                27_M57.ARP
                                                28_M57.LUB
                                                29_M57.SUL
                                                30_M57.CGN
                                                31_M57.RGN
                                                32_M57.H2P
                                                33_M57.H2R
                                                34_M57.KWG
                                                35_M57.FUM
                                                36_M57.STG
                                                37_M57.AET
                                                38_M57.BPU
                                                39_M57.BTL
                                                40_M57.CLE
                                                41_M57.CBL
                                                42_M57.CTL
                                                43_M57.FBD
                                                44_M57.GTL
                                                45_M57.EDH
                                                46_M57.EDM
                                                47_M57.NCE
                                                48_M57.SEW
                                                49_M57.CTLCCS
                                                50_M57.CBLCCS
                                                51_M57.IBA
                                                52_M57.LTE
                                                53_M57.CSU
                                                54_M57.SAF
                                                            /

FCC_InputStream_Mode(ProcessMode,Stream)  Map of FCC modes to their feedstock streams
/ FCC_CGO1H75.CGO1H
FCC_CGO1H95.CGO1H
FCC_CGO1V75.CGO1V
FCC_CGO1V95.CGO1V
FCC_CGO3H75.CGO3H
FCC_CGO3H95.CGO3H
FCC_CGO3V75.CGO3V
FCC_CGO3V95.CGO3V
FCC_CGO4H75.CGO4H
FCC_CGO4H95.CGO4H
FCC_CGO4V75.CGO4V
FCC_CGO4V95.CGO4V
FCC_CGO6H75.CGO6H
FCC_CGO6H95.CGO6H
FCC_CGO7H75.CGO7H
FCC_CGO7H95.CGO7H
FCC_CGO9H75.CGO9H
FCC_CGO9H95.CGO9H
FCC_CGOH75.CGOH
FCC_CGOH95.CGOH
FCC_CGOL75.CGOL
FCC_CGOL95.CGOL
FCC_DAOH75.DAOH
FCC_DAOH95.DAOH
FCC_DAOHH75.DAOHH
FCC_DAOHH95.DAOHH
FCC_DAOL75.DAOL
FCC_DAOL95.DAOL
FCC_DAOLH75.DAOLH
FCC_DAOLH95.DAOLH
FCC_GO175.GO1
FCC_GO175D.GO1
FCC_GO195.GO1
FCC_GO1H75.GO1H
FCC_GO1H75D.GO1H
FCC_GO1H95.GO1H
FCC_GO275.GO2
FCC_GO275D.GO2
FCC_GO295.GO2
FCC_GO2H75.GO2H
FCC_GO2H75D.GO2H
FCC_GO2H95.GO2H
FCC_GO375.GO3
FCC_GO375D.GO3
FCC_GO395.GO3
FCC_GO3H75.GO3H
FCC_GO3H75D.GO3H
FCC_GO3H95.GO3H
FCC_GO475.GO4
FCC_GO475D.GO4
FCC_GO495.GO4
FCC_GO4H75.GO4H
FCC_GO4H75D.GO4H
FCC_GO4H95.GO4H
FCC_GO575.GO5
FCC_GO575D.GO5
FCC_GO595.GO5
FCC_GO5H75.GO5H
FCC_GO5H75D.GO5H
FCC_GO5H95.GO5H
FCC_GO6H75.GO6H
FCC_GO6H75D.GO6H
FCC_GO6H95.GO6H
FCC_GO7H75.GO7H
FCC_GO7H75D.GO7H
FCC_GO7H95.GO7H
FCC_GO875.GO8
FCC_GO875D.GO8
FCC_GO895.GO8
FCC_GO8H75.GO8H
FCC_GO8H75D.GO8H
FCC_GO8H95.GO8H
FCC_GO9H75.GO9H
FCC_GO9H75D.GO9H
FCC_GO9H95.GO9H /

;

*--------------------------------------------------------------------------------------------------



Parameters
     TEMP_QCCRF(CenDiv)                     Placeholder for QCCRF in lfreport HARDCODED
     TEMPALLOCATE(RefReg,CenDiv)            Placeholder for reallocating NG fr RefReg2 to CD4 and CD6

Parameters
     ProcessTable(Stream,Process,ProcessMode)                 Refinery process table
     ProcessTableCrude(Stream,CrudeAll,Process,ProcessMode)   Refinery process table showing crude type
     CapFactor(Process,ProcessMode)                           Capacity factor - process mode utiliztion
     OpVarCost(Process,ProcessMode)                           Variable Operating Cost for process modes

     BrzAdvTranCost(t)                                        Brazilian ethanol production supply curve regression coefficients
     BrzAdvP0(t)
     BrzAdvQ0(t)
     BrzAdvEpsilon(t)

     BrzEthDmdTranCost(t)                                     Brazilian ethanol demand curve regression coefficients
     BrzEthDmdP0(t)
     BrzEthDmdQ0(t)
     BrzEthDmdEpsilon(t)

     NonUSEthTranCost(t)                                      Non-US ethanol demand curve regression coefficients
     NonUSEthP0(t)
     NonUSEthQ0(t)
     NonUSEthEpsilon(t)

*    EthBrazilPrc(EthStream,Step,t)                           Price at each step of Brazilian ethanol production
*    EthBrazilSup(EthStream,Step,t)                           Supply at each step of Brazilian ethanol production
     EthImpSupPrc(EthStream,Step,t)                           Price at each step of ethanol import curve to US
     EthImpSupQty(EthStream,Step,t)                           Supply qty at each step of ethanol import curve to US
     EthExpDmdPrc(EthStream,Step,t)                           Price at each step of ethanol export curve fr US
     EthExpDmdQty(EthStream,Step,t)                           Supply qty at each step of ethanol export curve fr US

     ExistingCap(RefReg,RefType,Process,t)                    Existing and planned process capacities
     StreamFactors(Process)                                   Overall unit capacity factors (max utilizations)
*     ExistingCapLB(RefReg,RefType,Process,t)                  Existing process lower bounds on operations

     AvailCap(RefReg,RefType,Process)                         Total existing capacity available at start of planning horizon

     GlobalImpPrice(CenDiv,EndProduct,Step,t)                 Dollars per M BBL at each step of global import curve for each end product
     GlobalImpSupply(CenDiv,EndProduct,Step,t)                Supply at each step of global import curve for each end product

     ImportPrice(Stream,INTREG,Step,t)                        Price at each step of product import supply curve
     ImportSupply(Stream,INTREG,Step,t)                       Quantity at each step of product import supply curve

     ExportPrice(Stream,INTREG,Step,t)                        Price at each step of product export demand curve
     ExportSupply(Stream,INTREG,Step,t)                       Quantity at each step of product export demand curve

     RefInpPrc(RefReg,RefInputStr,Step,t)                     Price at each step of refinery input supply
     RefInpSup(RefReg,RefInputStr,Step,t)                     Supply at each step of refinery input supply

     BiomassPrc(CoalDreg,BioStr,Step,t)                       Price at each step of biomass supply
     BiomassSup(CoalDReg,BioStr,Step,t)                       Supply at each step of biomass
     BioOthDemand(CoalDReg,BioStr,Other_MKT,t)                Biomass demand fom other NEMS modules to offset supply curves in billion BTUs per day

     MCCPrice(Stream,Step,t)                                  Caribbean and Maritime Canada demand curve prices
     MCCSupply(Stream,Step,t)                                 Caribbean and Maritime Canada demand curve supply

* 10-18-18 IMO price adj adders for pdstr, pjftr, pmgtr
* 10-18-18 for AEO2019 only
     adj4IMOdsuGAL                                            price adj for PDSTRHWY 2018 dollars per gal -set in lfreport
     adj4IMOmgGAL                                             price adj for PMGTR 2018 dollars per gal -set in lfreport
     adj4IMOjetGAL                                            price adj for PJFTR 2018 dollars per gal -set in lfreport
     adj4IMOyrPCT(t)                                          percent phase-out of price adj -set in lfreport

     CoalPrc(CoalSReg,CoalStr,Step,t)                         Price at each step of coal supply
     CoalSup(CoalSReg,CoalStr,Step,t)                         Supply at each step of coal supply
     SO2Prc(CoalStr,MX_SO2,t)                                 SO2 emission price
     HGPrc(CoalDReg,CoalStr,t)                                Mercury emission price
     CoalTrnspCst(CoalSReg,CoalDReg,CoalStr,t)                Coal transport cost
     CoalOthDemand(CoalSreg,CoalStr,t)                        Coal demand fom other NEMS modules to offset supply curves
     COALDtoREFmap(CoalDReg,RefReg)                           Coal demand region - RefReg allocation
     COALDtoREFmap2(CoalDReg,RefReg,BioStr)                   Coal demand region - RefReg - BioStr allocation
     COALDtoREFtot(RefReg,BioStr)                             Total BioStr transported to RegReg

     SO2Emission(CoalSReg,CoalDReg,CoalStr,MX_SO2,t)          SO2 emission rate
     HGEmission(CoalSReg,CoalDReg,CoalStr,t)                  Mercury emission rate

     RecipeBlending(RcpMode,Stream)                           Recipe blending table
     RecipeOVC(RcpMode)                                       Other variable costs for blending recipes

     UtilityPrice(RefReg,Utility,t)                           Price of each refinery utility type

     ProductDemand(CenDiv,EndProduct,t)                       Demand for each end product in each census division in period t
     ProductDemand_nonQLU(CenDiv,t)                           Demand for QOTIN excluding QLUIN in each census division in period t
     BTUDemand(CenDiv,t)                                      Demand in billion BTUs of all motor gas blends
     ProductPrice(CenDiv,EndProduct,t)                        Spot prices for each end product in each census division in period t

     FlexFuelDemand(CenDiv,t)                                 Flex Fuel Demand - Maximum E85 Demand

     BioOthDemand(CoalDReg,BioStr,Other_MKT,t)                BioMass demand fom other NEMS modules to offset supply curves MMbtu per day

     StreamProp(Stream,Property)                              Intermediate stream properties

     StreamSpecProd(IntStream,SpecProd)                       Stream-Blending Pool incidence matrix

     GasSpecMin(GasSpecProd,GasProp,t)                        Gasoline blending MIN specifications
     GasSpecMax(GasSpecProd,GasProp,t)                        Gasoline blending MAX specifications

     DistSpecMin(DistSpecProd,DistProp,t)                     Distillate blending MIN specifications
     DistSpecMax(DistSpecProd,DistProp,t)                     Distillate blending MAX specifications

     ResidSpecMin(ResidSpecProd,ResidProp,t)                  Resid blending MIN specifications
     ResidSpecMax(ResidSpecProd,ResidProp,t)                  Resid blending MAX specifications

     REFtoCDTranMap(RefReg,CenDiv)                            Equals 1 if transport is allowed between RefReg and CenDiv on any mode
     REFtoCDTranCap(RefReg,CenDiv,TranMode)                   PADD-to-Census transport capacity by tran mode
     REFtoCDTranCost(RefReg,CenDiv,TranMode,RecipeProd)       PADD-to-Census transport cost by tran mode and product

     REFtoREFTranCap(RefReg,RefRegA,TranMode,t)               PADD-to-PADD transport capacity by tran mode by year
     REFtoREFTranCost(RefReg,RefRegA,TranMode,Stream)         PADD-to-PADD transport cost by tran mode and product

     REFtoINTTranCap(RefReg,INTREG)                           RefReg to IntReg transport capacity
     REFtoINTTranCost(RefReg,INTREG)                          RefReg to IntReg transport cost
     INTtoREFTranCap(INTREG,RefReg)                           IntReg to RefReg transport capacity
     INTtoREFTranCost(INTREG,RefReg)                          IntReg to RefReg transport cost

     INTtoREFFBDTranCost(INTREG,RefReg)                       IntReg to RefReg transport cost for FBD (biodimp)

     CO2EmFactors(Stream,Process,ProcessMode)                 CO2 emission factors

     CapExpSize(Process)                                      Standard size for capacity expansion in M bbl per day
     CapExpISBL(Process)                                      B&M ISBL cost in thousands of dollars
     CapExpLabor(Process)                                     B&M Labor cost in dollars per day
     NFInvest(t)                                              Nelson-Farr investment cost indices
     NFLabor(t)                                               Nelson-Farr labor cost indices
     NFOper(t)                                                Nelson-Farr operation cost indices
     StateTax(t,RefReg)                                       Average state tax rate by supply region
     FedTax(t)                                                Federal tax rate
     LaborLoc(RefReg)                                         Regional cost multiplier for labor
     InvLoc(RefReg)                                           Regional cost multiplier for construction investment
     InvFactors(InvParam,BldRiskClass)                        Capacity exapansion investment parameters by risk class

     CrudePriceTotal(Step,t)                                  Marker Crude Prices - $87 per BBL
     CrudeSupplyTotal(Step,t)                                 Total World Crude Supply - 1000s BBL per day
     CrudePriceIncremental(Source,Crude,Step,t)               Incremental Crude Price Differential to Marker Crude Price - $87 per BBL
     CrudeSupplyIncremental(Source,Crude,Step,t)              Imported Crude Supply by Crude Type and Source - 1000s BBL per day
     DomesticCrudeSup(RefReg,Crude,t)                         Domestic Crude Supply by Crude Type and RefReg - 1000s BBL per day
     CrudeImportCost_Temp(RefReg,Source,Crude,Step)           Temp - Imported Crude Transport Cost From Crude Source to Refinery supply Region - $87 per BBL
     CrudeExportCost_Temp(RefReg,Source,Crude,Step)           Temp - Crude Transport Cost From Refinery supply Region to Crude Source - $87 per BBL
     CrudeImportCost(RefReg,Source,Crude,Step,t)              Imported Crude Transport Cost From Crude Source to Refinery supply Region - $87 per BBL
     CrudeExportCost(RefReg,Source,Crude,Step,t)              Exported Crude Transport Cost From Refinery supply Region to Crude Source - $87 per BBL
     CrudeImportCap(RefReg,Source,Step,t)                     Imported Crude Transport Limits From Crude Source to Refinery supply Region - M BBL per day
     CrudeExportCap(RefReg,Source,Step,t)                     Exported Crude Transport Limits From Refinery supply Region to Crude Source - M BBL per day

     SPR_Withdraw(RefReg,Crude,t)                             Crude withdrawn from the SPR - M BBL per day

     NonUSCrudeDemandPrice(Source,Crude,Step,t)               Non-US Crude Demand Price by Crude Source - $87 per BBL
     NonUSCrudeDemandValue(Source,Crude,Step,t)               Non-US Crude Demand by Crude Source - 1000s BBL per day
     CrudeToCanada(RefReg,Crude,t)                            US fixed crude exports to Canada

     CrdImpAllowed(RefReg,Crude)                              Incidence matrix indicating valid crude type-RefReg import combinations
     CrdExpAllowed(RefReg,Crude)                              Incidence matrix indicating valid crude type-RefReg export combinations

*     CrudeImportShares(Crude,t)                               Target Import Shares
     CrudeImportFactor(t)                                     Crude Import Price Factor

     NonPetDS(t)                                              Non-petroleum diesel production (MM bbl per day)
     NonPetMG(t)                                              Non-petroleum motor gas production (MM bbl per day)
     NonPetMG_noETH(t)                                        Non-petroleum motor gas production not counting ethanol (MM bbl per day)
     NonPetMG_ETHonly(t)                                      Ethanol to motor gas and E85 (MM bbl per day)

     PetDSFrac(t)                                             Fraction of DSU derived from petroleum
     PetMGFrac(t)                                             Fraction of gasoline and E85 derived from petroleum
     PetMGFrac_noETH(t)                                       Fraction of gasoline and E85 derived from petroleum not counting ethanol

     RFSMandates(RFSCategory,t)                               RFS mandates in bil gal per year
     RFSdenom(Period)                                         Denomonator - check for zero
     RFSFraction(RFSCategory,Period)                          Ratio of RFS mandate to transportation demand for each RFS category
     RFSTranDmd(Period)                                       Total volume of transportation fuels involved in the RFS
     RFSScores(Process,Stream)                                RFS scoring for the various biofuel streams as compared to ethanol
     RFSProcCategory(RFSCategory,Stream)                      RFS category-Stream incidence matrix
     RFSWaiverPrice(RFSCategory,t)                            RFS waiver price in 87$ per gallon
     WSGasPrices(t)                                           Wholesale gasoline price in 87$ per barrel (should be wholesale)
     RFSCreditPrice(RFSCategory,t)                            RFS Credit Price by RFS Category
     RFSWaivers(RFSCategory,t)                                RFS waivers from period 2 of prior year - used to bound period 1 RFS_PRD_VAL vector

     E15MaxPen(CenDiv,t)                                      Max E15 fraction of the total motor gas demand

     RefReg_to_CenDiv_ACU_Map(RefReg,CenDiv)                  Refinery Region ACU Capacity by Census Division
     RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv)                 Refinery Region to Census Mapping
     CenDiv_to_RefReg_ACU_Frac(RefReg,CenDiv)                 Census to RefReg Mapping

     ProductMarkups(t,MarkupFuel,MarkupSector,CenDiv)         Fuel product markups in $87 per MMBTU
     StateFuelTax(t,MarkupFuel,MarkupSector,CenDiv)           State fuel tax markups in $87 per MMBTU
     FedFuelTax(t,MarkupFuel)                                 Federal fuel taxes in NOMINAL DOLLARS per MMBTU
     EnvMarkups(CenDiv,MarkupFuel)                            Environmental fuel markups in $87 per BBL

     TotalMarkups(t,MarkupFuel,MarkupSector,CenDiv)           Total markups in 87$ per MMBTU

     CarbonTax(Stream,CenDiv,t)                               Carbon tax markups in 87$ per bbl

     ExpectedWOP(t)                                           Expected world oil price in 87$ per bbl

     CoprodAlpha(Stream)                                      Regression coefficients for coproduct regression equations
     CoprodBeta(Stream)

     LPGAlpha(LPGSector)                                      Regression coefficients for LPG regression equations
     LPGBeta(LPGSector)

     CoproductPrice(Stream,RefReg,t)                          Coproduct price in 87$ - units vary
     LPGPrice(LPGSector,t)                                    LPG price in $87$ per MMBTU

     DieselFrac(EndProduct,DieselSector,t)                    Transition thru time of frac of DIESEL by market to DSU DSL N2H

*=== NGLFracNGPL(RefInputStr,OGDIST,RefReg)                   NGL fractions by natural gas plant liquid regions
     NGLBounds(RefInputStr,RefReg,t)                          NGL fractions by refining region in M bbl per day
     QTotalReg(RefReg)                                        Total NGL liquid production by refining region in MM bbl per day
*=== NGLTotal(RefReg,t)                                       Total NGL liquid production by refining region in MM bbl per day
     NGLDemands(CenDiv,NGLProduct,t)                          NGL demands by census division in M bbl per day
     NGLImportCost(CenDiv,NGLProduct,t)                       NGL import transport cost in 87$ per bbl
     NGLExportCost(CenDiv,NGLProduct,t)                       NGL export transport cost in 87$ per bbl

     LCFS_LandfillGas_Carbon_Factor(LCFS_PetCategory)                                              LCFS Landfill Gas Carbon Factor
     LCFS_LandfillGas_total(t)                                                                     LCFS Landfill Gas demand -trill BTU
     LCFS_LFGas_CFactor(LCFS_PetCategory,t)                                                        LCFS Landfill Gas Carbon Factor Index Offset
     LCFS_Alt_CFactor(LCFS_PetCategory,LCFS_Vehicle_Types,t)                                       LCFS Alternate Vehicle Carbon Factor Index Offset
     LCFS_BioStream_Carbon_Factor(LCFS_PetCategory,LCFS_BioProcess,LCFS_BioMode,LCFS_BioStreams,t)   Carbon Factor for Non-Petroleum Liquids included in LCFS Requirement
     LCFS_BioStream_PetCarbonFactor(LCFS_PetCategory,LCFS_BioProcess,LCFS_BioMode,LCFS_BioStreams,t) Carbon Factor for Petroleum Substitute for Non-Petroleum Liquids (1000 metric tons C per trill)
     LCFS_BioImport_Carbon_Factor(LCFS_PetCategory,LCFS_BioImports,t)                                Carbon Factor for Imported Bio-Liquids included in LCFS Requiremnet (1000 metric tons C per trill)
     LCFS_BioImport_PetCarbonFactor(LCFS_PetCategory,LCFS_BioImports,t)                              Carbon Factor for Petroleum Substitute for Imported Bio-Liquids (1000 metric tons C per trill)
     LCFS_PetStream_Carbon_Factor(LCFS_PetCategory,LCFS_PetStreams,t)                                Carbon Factor for Petroleum Liquids included in LCFS Requirement (1000 metric tons C per trill)
     LCFS_Pet_Energy_Density(RecipeProd,t)                                                         LCFS Energy Density for Petroleum Streams
     LCFS_Target(LCFS_PetCategory,LCFS_RefReg,t)                                                   LCFS Targets by Refinery Region and Year
     LCFS_Penalty_Cost(LCFS_PetCategory)                                                           LCFS Penalty Cost 87$ per ton of excess Carbon
     LCFS_Bio_Energy_Density(LCFS_BioStreams)                                                      LCFS Energy Density of Non-Petroleum Streams
     LCFS_Imp_Energy_Density(LCFS_BioImports)                                                      LCFS Energy Density of Imported Bio-Liquid Streams
     Energy_Density(Stream)                                                                        Energy Density
     LCFS_Bio_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioProcess,LCFS_BioMode,t)                   LCFS Carbon Factor Index for Non-Petroleum Production Process (CF_bio-CF_pet) in metric tons per bbl
     LCFS_Imp_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioImports,t)                                LCFS Carbon Factor Index for Imported Bio-Liquids (CF_bio-CF_pet) in metric tons per bbl
     LCFS_Pet_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_PetStreams,t)                                LCFS Petroleum Carbon Factor Index (CF_pet - CF_target) in metric tons per bbl

     LCFS_BiodImp_Carbon_Factor(LCFS_PetCategory,t)                                                Carbon Factor for Imported Biodiesel included in LCFS Requiremnet in 1000 metric tons C per trill
     LCFS_BiodImp_PetCarbonFactor(LCFS_PetCategory,t)                                              Carbon Factor for Petroleum Substitute for Imported Biodiesel in 1000 metric tons C per trill
     LCFS_BiodImp_CFactor(LCFS_PetCategory,LCFS_RefReg,t)                                          LCFS Carbon Factor Index for Imported Biodiesl (CF_bioD-CF_pet) in metric tons per bbl

     LCFS_RenewDImp_Carbon_Factor(LCFS_PetCategory,t)                                              Carbon Factor for Imported renewable diesel included in LCFS Requiremnet in 1000 metric tons C per trill
     LCFS_RenewDImp_PetCarbonFactor(LCFS_PetCategory,t)                                            Carbon Factor for Petroleum Substitute for renewable diesel in 1000 metric tons C per trill
     LCFS_RenewDImp_CFactor(LCFS_PetCategory,LCFS_RefReg,t)                                        LCFS Carbon Factor Index for Imported renewable diesel (CF_renewD-CF_pet) in metric tons per bbl

     LCFS_Price(LCFS_PetCategory,t)                                                                LCFS Credit Price
     LCFSSafetyPrice(LCFS_PetCategory,t)                                                           LCFS Penalty Price by year
     LCFS_AltVehicle_Carbon_Factor(LCFS_PetCategory,LCFS_Vehicle_Types)                            LCFS Carbon Factor by Vehicle Type
     LCFS_AltVehicle_CA_Share(LCFS_PetCategory,LCFS_Vehicle_Types)                                 LCFS Carbon Factor by Vehicle Type
     LCFS_AltVehicle_Demand(LCFS_PetCategory,LCFS_Vehicle_Types,LCFS_RefReg,t)                     LCFS Alternate Vehicle Demands Trillion Btus
     LCFS_Eth_CFactor(LCFS_PetCategory,LCFS_BioStreams,t)                                          LCFS Carbon Factor Index for Ethanol Production Process (CF_bio-CF_pet)

     CFP_LandfillGas_Carbon_Factor(CFP_PetCategory)                                                CFP Landfill Gas Carbon Factor
     CFP_LandfillGas_total(t)                                                                      CFP Landfill Gas demand -trill BTU
     CFP_LFGas_CFactor(CFP_PetCategory,t)                                                          CFP Landfill Gas Carbon Factor Index Offset
     CFP_Alt_CFactor(CFP_PetCategory,CFP_Vehicle_Types,t)                                          CFP Alternate Vehicle Carbon Factor Index Offset
     CFP_BioStream_Carbon_Factor(CFP_PetCategory,CFP_BioProcess,CFP_BioMode,CFP_BioStreams,t)      Carbon Factor for Non-Petroleum Liquids included in CFP Requirement
     CFP_BioStream_PetCarbonFactor(CFP_PetCategory,CFP_BioProcess,CFP_BioMode,CFP_BioStreams,t)    Carbon Factor for Petroleum Substitute for Non-Petroleum Liquids (1000 metric tons C per trill)
     CFP_BioImport_Carbon_Factor(CFP_PetCategory,CFP_BioImports,t)                                 Carbon Factor for Imported Bio-Liquids included in CFP Requiremnet (1000 metric tons C per trill)
     CFP_BioImport_PetCarbonFactor(CFP_PetCategory,CFP_BioImports,t)                               Carbon Factor for Petroleum Substitute for Imported Bio-Liquids (1000 metric tons C per trill)
     CFP_PetStream_Carbon_Factor(CFP_PetCategory,CFP_PetStreams,t)                                 Carbon Factor for Petroleum Liquids included in CFP Requirement (1000 metric tons C per trill)
     CFP_Pet_Energy_Density(RecipeProd,t)                                                          CFP Energy Density for Petroleum Streams
     CFP_Target(CFP_PetCategory,CFP_RefReg,t)                                                      CFP Targets by Refinery Region and Year
     CFP_Penalty_Cost(CFP_PetCategory)                                                             CFP Penalty Cost 87$ per ton of excess Carbon
     CFP_Bio_Energy_Density(CFP_BioStreams)                                                        CFP Energy Density of Non-Petroleum Streams
     CFP_Imp_Energy_Density(CFP_BioImports)                                                        CFP Energy Density of Imported Bio-Liquid Streams
     CFP_Bio_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioProcess,CFP_BioMode,t)                      CFP Carbon Factor Index for Non-Petroleum Production Process (CF_bio-CF_pet) in metric tons per bbl
     CFP_Imp_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioImports,t)                                  CFP Carbon Factor Index for Imported Bio-Liquids (CF_bio-CF_pet) in metric tons per bbl
     CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,t)                                  CFP Petroleum Carbon Factor Index (CF_pet - CF_target) in metric tons per bbl

     CFP_BiodImp_Carbon_Factor(CFP_PetCategory,t)                                                Carbon Factor for Imported Biodiesel included in CFP Requiremnet in 1000 metric tons C per trill
     CFP_BiodImp_PetCarbonFactor(CFP_PetCategory,t)                                              Carbon Factor for Petroleum Substitute for Imported Biodiesel in 1000 metric tons C per trill
     CFP_BiodImp_CFactor(CFP_PetCategory,CFP_RefReg,t)                                           CFP Carbon Factor Index for Imported Biodiesl (CF_bioD-CF_pet) in metric tons per bbl

     CFP_RenewDImp_Carbon_Factor(CFP_PetCategory,t)                                              Carbon Factor for Imported renewable diesel included in CFP Requiremnet in 1000 metric tons C per trill
     CFP_RenewDImp_PetCarbonFactor(CFP_PetCategory,t)                                            Carbon Factor for Petroleum Substitute for renewable diesel in 1000 metric tons C per trill
     CFP_RenewDImp_CFactor(CFP_PetCategory,CFP_RefReg,t)                                         CFP Carbon Factor Index for Imported renewable diesel (CF_renewD-CF_pet) in metric tons per bbl

     CFP_Price(CFP_PetCategory,t)                                                                CFP Credit Price
     CFPSafetyPrice(CFP_PetCategory,t)                                                           CFP Penalty Price by year
     CFP_AltVehicle_Carbon_Factor(CFP_PetCategory,CFP_Vehicle_Types)                             CFP Carbon Factor by Vehicle Type
     CFP_AltVehicle_OR_Share(CFP_PetCategory,CFP_Vehicle_Types)                                  CFP Carbon Factor by Vehicle Type
     CFP_AltVehicle_Demand(CFP_PetCategory,CFP_Vehicle_Types,CFP_RefReg,t)                       CFP Alternate Vehicle Demands Trillion Btus
     CFP_Eth_CFactor(CFP_PetCategory,CFP_BioStreams,t)                                           CFP Carbon Factor Index for Ethanol Production Process (CF_bio-CF_pet)

     WACFS_LandfillGas_Carbon_Factor(WACFS_PetCategory)                                                WACFS Landfill Gas Carbon Factor
     WACFS_LandfillGas_total(t)                                                                      WACFS Landfill Gas demand -trill BTU
     WACFS_LFGas_CFactor(WACFS_PetCategory,t)                                                          WACFS Landfill Gas Carbon Factor Index Offset
     WACFS_Alt_CFactor(WACFS_PetCategory,WACFS_Vehicle_Types,t)                                          WACFS Alternate Vehicle Carbon Factor Index Offset
     WACFS_BioStream_Carbon_Factor(WACFS_PetCategory,WACFS_BioProcess,WACFS_BioMode,WACFS_BioStreams,t)      Carbon Factor for Non-Petroleum Liquids included in WACFS Requirement
     WACFS_BioStream_PetCarbonFactor(WACFS_PetCategory,WACFS_BioProcess,WACFS_BioMode,WACFS_BioStreams,t)    Carbon Factor for Petroleum Substitute for Non-Petroleum Liquids (1000 metric tons C per trill)
     WACFS_BioImport_Carbon_Factor(WACFS_PetCategory,WACFS_BioImports,t)                                 Carbon Factor for Imported Bio-Liquids included in WACFS Requiremnet (1000 metric tons C per trill)
     WACFS_BioImport_PetCarbonFactor(WACFS_PetCategory,WACFS_BioImports,t)                               Carbon Factor for Petroleum Substitute for Imported Bio-Liquids (1000 metric tons C per trill)
     WACFS_PetStream_Carbon_Factor(WACFS_PetCategory,WACFS_PetStreams,t)                                 Carbon Factor for Petroleum Liquids included in WACFS Requirement (1000 metric tons C per trill)
     WACFS_Pet_Energy_Density(RecipeProd,t)                                                          WACFS Energy Density for Petroleum Streams
     WACFS_Target(WACFS_PetCategory,WACFS_RefReg,t)                                                      WACFS Targets by Refinery Region and Year
     WACFS_Penalty_Cost(WACFS_PetCategory)                                                             WACFS Penalty Cost 87$ per ton of excess Carbon
     WACFS_Bio_Energy_Density(WACFS_BioStreams)                                                        WACFS Energy Density of Non-Petroleum Streams
     WACFS_Imp_Energy_Density(WACFS_BioImports)                                                        WACFS Energy Density of Imported Bio-Liquid Streams
     WACFS_Bio_CFactor(WACFS_PetCategory,WACFS_RefReg,WACFS_BioProcess,WACFS_BioMode,t)                      WACFS Carbon Factor Index for Non-Petroleum Production Process (CF_bio-CF_pet) in metric tons per bbl
     WACFS_Imp_CFactor(WACFS_PetCategory,WACFS_RefReg,WACFS_BioImports,t)                                  WACFS Carbon Factor Index for Imported Bio-Liquids (CF_bio-CF_pet) in metric tons per bbl
     WACFS_Pet_CFactor(WACFS_PetCategory,WACFS_RefReg,WACFS_PetStreams,t)                                  WACFS Petroleum Carbon Factor Index (CF_pet - CF_target) in metric tons per bbl

     WACFS_BiodImp_Carbon_Factor(WACFS_PetCategory,t)                                                Carbon Factor for Imported Biodiesel included in WACFS Requiremnet in 1000 metric tons C per trill
     WACFS_BiodImp_PetCarbonFactor(WACFS_PetCategory,t)                                              Carbon Factor for Petroleum Substitute for Imported Biodiesel in 1000 metric tons C per trill
     WACFS_BiodImp_CFactor(WACFS_PetCategory,WACFS_RefReg,t)                                           WACFS Carbon Factor Index for Imported Biodiesl (CF_bioD-CF_pet) in metric tons per bbl

     WACFS_RenewDImp_Carbon_Factor(WACFS_PetCategory,t)                                              Carbon Factor for Imported renewable diesel included in WACFS Requiremnet in 1000 metric tons C per trill
     WACFS_RenewDImp_PetCarbonFactor(WACFS_PetCategory,t)                                            Carbon Factor for Petroleum Substitute for renewable diesel in 1000 metric tons C per trill
     WACFS_RenewDImp_CFactor(WACFS_PetCategory,WACFS_RefReg,t)                                         WACFS Carbon Factor Index for Imported renewable diesel (CF_renewD-CF_pet) in metric tons per bbl

     WACFS_Price(WACFS_PetCategory,t)                                                                WACFS Credit Price
     WACFSSafetyPrice(WACFS_PetCategory,t)                                                           WACFS Penalty Price by year
     WACFS_AltVehicle_Carbon_Factor(WACFS_PetCategory,WACFS_Vehicle_Types)                             WACFS Carbon Factor by Vehicle Type
     WACFS_AltVehicle_WA_Share(WACFS_PetCategory,WACFS_Vehicle_Types)                                  WACFS Carbon Factor by Vehicle Type
     WACFS_AltVehicle_Demand(WACFS_PetCategory,WACFS_Vehicle_Types,WACFS_RefReg,t)                       WACFS Alternate Vehicle Demands Trillion Btus
     WACFS_Eth_CFactor(WACFS_PetCategory,WACFS_BioStreams,t)                                           WACFS Carbon Factor Index for Ethanol Production Process (CF_bio-CF_pet)

     AB32_CapAdjFactor(t)                                            AB-32 cap adjustment factors
     AB32_AssistFactor(t)                                            AB-32 industry assistance factors
     AB32_BenchFactor                                                AB-32 benchmark for primary refinery product in allowances per barrel per year
     AB32_StartYr(Stream)                                            AB-32 coverage start year by product
     AB32_CoverageFrac(Stream)                                       AB-32 coverage fraction by product
     AB32_AllowPrice(t)                                              AB-32 allowance price in 87$ per tonne CO2

     AFGrowthRate(Process)                                           Allowed builds multipliier for process builds (a 2 would mean that you can double capacity for example)
     AFInitPlants(Process)                                           Number of units of a process you are allowed to build if you currently have zero capacity
     AFCapMult(Process,BldStep)                                      Multiplier for number of additonal units by step as compared to step 1 of MaxBuilds
     AFCostMult(Process,BldStep)                                     Cost multiplier for additonal units by step as compared to step 1 of MaxBuilds
     MaxBuilds(Process,BldStep,t)                                    Maximum allowed builds in M bbl per day by year
     TotalBuilds(Process,t)                                          Total yearly capacity if you build at the max growth rate in each lump year

     CornPriceExp(t)                                                 Parameter for the corn price regression

     CornTranCost(RefReg)                                            Corn transport cost in 87$ per bushel

     SeedOilQnty(RefReg,FCOType)                                     Quantities of seed oil by region and type in M bbl per day

     GrainQnty(RefReg)                                               Grain stocks in MM bushels per year

     LearningData(Process,LrnPhase,LrnSpeed,LrnParam)                Technology learning data for capacity expansion costs
     MoreLearningData(Process,MoreLrnParam)                          More technology learning data for capacity expansion costs
     NumPlants(Process)                                              Number of existing plants
     LrnFactor(Process)                                              Learning multiplier for capital cost

     FBDImpQuant(RefReg,t)                                           Biodiesel max import quantities by region in M bbl per day
     FBDImpCoefB(t)                                                  Biodiesel import price equation coefficient
     FBDImpCoefM(t)                                                  Biodiesel import price equation coefficient
     FBDMaxQty(t)                                                    Total biodiesel max import quantity in M bbl per day

     FBDImpPrice(Step,t)                                             Biodiesel import supply step prices
     FBDImpSupply(Step,t)                                            Biodiesel import supply step quantities

     FBDImpElas                                                      biodiesel import supply curve elasticity
                                                                     /0.30/

     LFMMOUT_RenewDIMP(MNUMCR,MNUMYR)                                Renewable diesel imports by census in 1000 bbl per day- renew version of BIODIMP
     LFMMOUT_RenewDImpPD(MNUMPR,MNUMYR)                              Renewable Diesel import by subPADD in 1000 bbl per day- renew version of BIODIMPPD
     RDHImpElas                                                      Renewable Diesel import supply curve elasticity
                                                                     /0.30/

     RDHImpPrice(Step,t)                                             Renewable diesel import supply step prices
     RDHImpSupply(Step,t)                                            Renewable diesel import supply step quantities

     CDtoREFMap(RefReg,CenDiv)                                       Census-to-RefReg mapping for NGPL demands

     CO2TranStoreCost(RefReg,t)                                      CO2 transport and storage cost in 87$ per ton CO2

     CO2EmissionTax(t)                                               CO2 emission tax in 87$ per ton CO2


*CCATS data here
     CCATSDAT_CO2_PRC_DIS_45Q(MNUMCR,MNUMYR)                         CO2 price output after optimization (45Q eligible)
     CCATSDAT_CO2_PRC_DIS_NTC(MNUMCR,MNUMYR)                         CO2 price output after optimization (no tax credit)
     CCATSDAT_SUP_ETH_45Q_lfmm(CenDiv,t)                             CCATS co2 volumes from ethanol production eligible for 45Q tax credit
     CCATSDAT_SUP_ETH_NTC_lfmm(CenDiv,t)                             CCATS co2 volumes from ethanol production not eligible for tax credit
     CCATSDAT_CST_ETH_INV_lfmm(CenDiv,t)                             Investment cost for carbon capture from ethanol production
     CCATSDAT_CST_ETH_OM_lfmm(CenDiv,t)                              O&M cost for carbon capture from ethanol production

     ULSD_N2H_CD(CenDiv,t)                                           calculated from N2H dmds-ULSD heating oil mandates in Mbbl per year
     ULSD_N2H(CenDiv,State,t)                                        hist data to calc ULSD_N2H_CD in Mbbl per year
     ULSD_N2H_CDhistot(CenDiv)                                       total hist N2H dmd by CD in Mbbl per year
     ULSD_N2H_percent(CenDiv,t)                                      annual percent of CD scheduled to meet ULSD heating oil mandates

     State_Biodiesel(RefReg,State,t)                                 Biodiesel mandates in M bbl per day

     BiofuelSubsidy(Stream,t)                                        Biofuel subisidies in 87$ per bbl
;

$gdxin %DirPath%\input\%INPUT_FILE_PARAM%
$Loaddc CapFactor, OpVarCost, ProcessTableCrude, ExistingCap, StreamFactors
$Loaddc RefInpPrc, RefInpSup
$Loaddc GlobalImpSupply, GlobalImpPrice
$Loaddc StreamProp, StreamSpecProd, RecipeBlending, RecipeOVC
$Loaddc GasSpecMin, GasSpecMax, DistSpecMin, DistSpecMax, ResidSpecMin, ResidSpecMax
$Loaddc REFtoCDTranMap, CO2EmFactors, ULSD_N2H, State_Biodiesel, BiofuelSubsidy

* WARNING turn off domain checking until we fix the 99_REFREG item (which is used for MarineMode costs)
$load REFtoREFTranCap

* WARNING  turn off domain checking until we figure out transport mode TDS
$load REFtoREFTranCost

$Loaddc REFtoINTTranCap, REFtoINTTranCost, INTtoREFTranCap, INTtoREFTranCost
$Loaddc REFtoCDTranCap, REFtoCDTranCost
$Loaddc COALDtoREFmap, CrdImpAllowed, CrdExpAllowed, SPR_Withdraw
$Loaddc CrudeImportCap,CrudeImportCost_Temp,CrudeExportCap,CrudeExportCost_Temp
$Loaddc RFSMandates, RFSScores, RFSProcCategory, RFSWaiverPrice
$Loaddc E15MaxPen, RefReg_to_CenDiv_ACU_Map
$Loaddc ProductMarkups, StateFuelTax, FedFuelTax, EnvMarkups
* 10-23-15 em4 removed NGLFracNGPL from LFMinput.gdx - no longer needed;
*              however, NEED to remove from Excel and Access files
$Loaddc CoprodAlpha, CoprodBeta, LPGAlpha, LPGBeta, DieselFrac
$Loaddc CornPriceExp, CornTranCost, SeedOilQnty, GrainQnty
$Loaddc BrzAdvTranCost, BrzAdvP0, BrzAdvQ0, BrzAdvEpsilon
$Loaddc BrzEthDmdTranCost, BrzEthDmdP0, BrzEthDmdQ0, BrzEthDmdEpsilon
$Loaddc NonUSEthTranCost, NonUSEthP0, NonUSEthQ0, NonUSEthEpsilon
$Loaddc FBDImpQuant, FBDImpCoefB, FBDImpCoefM, CDtoREFMap

$Loaddc AB32_CapAdjFactor, AB32_AssistFactor, AB32_BenchFactor, AB32_StartYr, AB32_CoverageFrac
$Loaddc CCATSDAT_SUP_ETH_45Q_lfmm, CCATSDAT_SUP_ETH_NTC_lfmm
$Loaddc CCATSDAT_CST_ETH_INV_lfmm, CCATSDAT_CST_ETH_OM_lfmm

$gdxin

* 1-31-17 em4: correction to prepare code to define ULSD_N2H_CD

  ULSD_N2H_CDhistot(CenDiv) = 0.0 ;
  ULSD_N2H_percent(CenDiv,t) = 0.0 ;
  ULSD_N2H_CD(CenDiv,t) = 0.0 ;

  ULSD_N2H_CDhistot(ActiveDem) = sum( (State,t),ULSD_N2H(ActiveDem,State,t) ) ;
  ULSD_N2H_percent(ActiveDem,t) =
        sum( (state,tt)$(ord(tt)<=ord(t) and ULSD_N2H(ActiveDem,state,tt)>0),
        ULSD_N2H(ActiveDem,state,tt)/ ULSD_N2H_CDhistot(ActiveDem) ) ;
*
* 8-31-13 em4: need to redefine CGN existing cap from GWh/day to M bfoe/day of fuel feed
*              using 722 KWh/bfoe feed into CGN

ExistingCap(RefReg,RefType,'CGN',t)   =
ExistingCap(RefReg,RefType,'CGN',t)* 1000. /  722. ;

*----------------------------------------------------------------------------
$gdxin %DirPath%\input\%INPUT_FILE_PARAM_INVEST%
$Loaddc AFGrowthRate, AFInitPlants, AFCapMult, AFCostMult
$Loaddc CapExpSize, CapExpISBL, CapExpLabor
$Loaddc LearningData, MoreLearningData
$Loaddc StateTax, FedTax, LaborLoc, InvLoc, InvFactors

* turn off domain checking until we resolve the 1980-2008 data issue
*$loaddc NFInvest, NFLabor, NFOper
$load NFInvest, NFLabor, NFOper
$gdxin
*----------------------------------------------------------------------------


$ontext
*---------------------------------------------------------------------------
parameter
     RFSMandates_orig(RFSCategory,t)     'Original RFS mandates, as revised yearly by EPA'
     RFSMandates_T(t,RFSCategory)        'Transpose of revised RFSMandates(RFSCategory,t)'
;

RFSMandates_orig(RFSCategory,t) = RFSMandates(RFSCategory,t);

Table RFSMandates_T(t,RFSCategory)
                    Total         Advanced         Cellulosic     Biodiesel
      2006          4             0                0              0
      2007          4.7           0                0              0
      2008          9             0                0              0
      2009          11.1          0.6              0              0.75
      2010          12.95         0.95             0              0.975
      2011          13.95         1.35             0              1.20
      2012          15.2          2.00             0              1.50
      2013          15.34         2.426            0.006          1.92
      2014          15.34         2.426            0.011          1.92
      2015          15.30         1.937            0.017          1.92
      2016          15.30         1.945            0.025          1.92
      2017          15.30         1.96             0.04           1.92
      2018          15.30         1.98             0.06           1.92
      2019          15.30         2.01             0.09           1.92
      2020          15.30         2.05             0.13           1.92
      2021          15.30         2.12             0.20           1.92
      2022          15.30         2.22             0.30           1.92
(2023*2080)         15.30         2.22             0.30           1.92
;

RFSMandates(RFSCategory,t) = RFSMandates_T(t,RFSCategory);

*---------------------------------------------------------------------------
$offtext



*-----------------------------
* mc6 7/9/13 temporary: double until Tony Radich provides updated curve
SeedOilQnty(RefReg,FCOType) = 2.0 * SeedOilQnty(RefReg,FCOType);
*-----------------------------

* WARNING: use loadDC for domain checking
$gdxin %DirPath%\input\%INPUT_FILE_PARAM%
$Load LCFS_BioStream_Carbon_Factor, LCFS_PetStream_Carbon_Factor, LCFS_Target, LCFS_Penalty_Cost
$Load LCFS_BioImport_Carbon_Factor
$Load LCFS_AltVehicle_Carbon_Factor, LCFS_AltVehicle_CA_Share
$gdxin

$gdxin %DirPath%\input\%INPUT_FILE_PARAM%
$Loaddc CFP_BioStream_Carbon_Factor, CFP_PetStream_Carbon_Factor, CFP_Target, CFP_Penalty_Cost
$Loaddc CFP_BioImport_Carbon_Factor
$Loaddc CFP_AltVehicle_Carbon_Factor, CFP_AltVehicle_OR_Share
$gdxin

$gdxin %DirPath%\input\%INPUT_FILE_PARAM%
$Loaddc WACFS_BioStream_Carbon_Factor, WACFS_PetStream_Carbon_Factor, WACFS_Target, WACFS_Penalty_Cost
$Loaddc WACFS_BioImport_Carbon_Factor
$Loaddc WACFS_AltVehicle_Carbon_Factor, WACFS_AltVehicle_WA_Share
$gdxin

*11-4-2020 TEMP allow HGL transport along R1 to R4
*               so ethane imports can be set to zero, in 1000 bbl/d
REFtoREFTranCap('1_REFREG','4_REFREG','tmPLG','2008') = 1000.0 ;
REFtoREFTranCap('2_REFREG','4_REFREG','tmPLG','2008') =  500.0 ;
REFtoREFTranCap('3_REFREG','4_REFREG','tmPLG','2008') =  500.0 ;

* Sum entries in the REFtoREFTranCap table over prior years to get each year's capacity
Parameter tempCap(RefReg,RefRegA,TranMode,t) ;
tempCap(RefReg,RefRegA,TranMode,t)         = REFtoREFTranCap(RefReg,RefRegA,TranMode,t) ;
REFtoREFTranCap(RefReg,RefRegA,TranMode,t) =
  max(0, sum(tt$(ord(tt)<=ord(t)), tempCap(RefReg,RefRegA,TranMode,tt))) ;

*----------------------------------------------------------------------------------------------------
* HARDCODED  Add 1% locality tax based on current nominal value

Loop((MNUMCR,MNUMYR),
     StateFuelTax(t,'M','T',ActiveDem)$(crN2L(ActiveDem,MNUMCR) and tMNUM(t,MNUMYR)) =
        StateFuelTax(t,'M','T',ActiveDem) + MPBLK_PMGTR(MNUMCR,MNUMYR) * 0.01;
);

* HARDCODED  Set CM and IN = TR for MG
     StateFuelTax(t,'M','C',ActiveDem) = StateFuelTax(t,'M','T',ActiveDem);
     StateFuelTax(t,'M','I',ActiveDem) = StateFuelTax(t,'M','T',ActiveDem);
*----------------------------------------------------------------------------------------------------

* Convert state biodiesel mandates from bil gal/yr to M bbl/day
State_Biodiesel(RefReg,State,t) = State_Biodiesel(RefReg,State,t)*1000000/365/42 ;
State_Biodiesel(RefReg,State,'2010') = 0 ;

set
   BiodieselRecipe(RcpMode)        Biodiesel recipe modes
;
BiodieselRecipe(RcpMode) = no ;
BiodieselRecipe(RcpMode) = RecipeBlending(RcpMode,'FBD') ;

* Assign LCFS Penalty Price for all years
LCFSSafetyPrice(LCFS_PetCategory,t) = LCFS_Penalty_Cost(LCFS_PetCategory);

CFPSafetyPrice(CFP_PetCategory,t) = CFP_Penalty_Cost(CFP_PetCategory);

WACFSSafetyPrice(WACFS_PetCategory,t) = WACFS_Penalty_Cost(WACFS_PetCategory);

* Define World Crude Supply Curve with data from the World Model
CrudePriceTotal(Step,ModelYears(t)) =
   sum((CRSTEP,MNXYRS)$(StepCR(Step,CRSTEP) and tMNXYRS(t,MNXYRS)),INTOUT_P_TOTAL_CRUDE(CRSTEP,MNXYRS));

CrudeSupplyTotal(Step,ModelYears(t)) =
   sum((CRSTEP,MNXYRS)$(StepCR(Step,CRSTEP) and tMNXYRS(t,MNXYRS)),INTOUT_Q_TOTAL_CRUDE(CRSTEP,MNXYRS));

CrudePriceIncremental('Foreign',Crude,Step,ModelYears(t)) =
   sum((MNCRUD,M1,CISTEP,MNXYRS)$(StepCI(Step,CISTEP) and MNCRUD_2_Crude(MNCRUD,Crude) and tMNXYRS(t,MNXYRS)),
   INTOUT_P_Foreign_Crude(MNCRUD,M1,CISTEP,MNXYRS));

CrudeSupplyIncremental('Foreign',Crude,Step,ModelYears(t)) =
   sum((MNCRUD,M1,CISTEP,MNXYRS)$(StepCI(Step,CISTEP) and MNCRUD_2_Crude(MNCRUD,Crude) and tMNXYRS(t,MNXYRS)),
   INTOUT_Q_Foreign_Crude(MNCRUD,M1,CISTEP,MNXYRS));

CrudeSupplyIncremental('Foreign',Crude,Step,ModelYears(t)) =
  max(0, CrudeSupplyIncremental('Foreign',Crude,Step,ModelYears)) ;

DomesticCrudeSup(RefReg,Crude,ModelYears(t)) =
   sum((MNUMPR,MNCRUD,MNUMYR)$(RefReg2MNUMPR(MNUMPR,RefReg) and MNCRUD_2_Crude(MNCRUD,Crude) and tMNUM(t,MNUMYR)),
   OGSMOUT_OGCRUDEREF(MNUMPR,MNCRUD,MNUMYR) * 1000 / 365.0) ;

DomesticCrudeSup(RefReg,Crude,ModelYears(t))$(ord(t) > (AEOLSTYR - 1989)) =
   sum((MNUMPR,MNCRUD,MNUMYR)$(RefReg2MNUMPR(MNUMPR,RefReg) and MNCRUD_2_Crude(MNCRUD,Crude) and (ord(MNUMYR) = (AEOLSTYR - 1989))),
   OGSMOUT_OGCRUDEREF(MNUMPR,MNCRUD,MNUMYR) * 1000 / 365.0) ;

NonUSCrudeDemandPrice(Source,Crude,Step,ModelYears(t)) = 0.0;
NonUSCrudeDemandPrice('Foreign',Crude,Step,ModelYears(t)) =
   sum((MNCRUD,M1,S1,MNXYRS)$(StepS1(Step,S1) and MNCRUD_2_Crude(MNCRUD,Crude) and tMNXYRS(t,MNXYRS)),
   INTOUT_P_Non_US_Demand(MNCRUD,M1,S1,MNXYRS));

CrudeToCanada(RefReg,Crude,ModelYears(t)) =
   sum((MNUMPR,MNCRUD,MNXYRS)$(RefReg2MNUMPR(MNUMPR,RefReg) and MNCRUD_2_Crude(MNCRUD,Crude) and tMNXYRS(t,MNXYRS)),
     LFMMOUT_Q_CRUDE_TO_CAN(MNUMPR,MNCRUD,MNXYRS)) ;

*ANWR M-Msour crude exports -- Algorithm to maintain reasonable crude prices
parameter ExcessANWRproduction(t);
parameter EstMMsourMaxProcInRR78 ;     /* thousand bbl per day */
parameter ReduceANWRexport       ;     /* fraction less than 1 */

EstMMsourMaxProcInRR78 = 750  ;      /* thousand bbl per day */
ReduceANWRexport       = 0.90 ;      /* fraction less than 1 */

ExcessANWRproduction(ModelYears(t)) =  DomesticCrudeSup('8_RefReg','M_Msour',t) -
                                       EstMMsourMaxProcInRR78 ;
loop( ModelYears(t),
   IF( ExcessANWRproduction(t) > CrudeToCanada('8_RefReg','M_Msour',t) ,
      CrudeToCanada('8_RefReg','M_Msour',t) = ReduceAnwrexport*ExcessANWRproduction(t) ;
   );
);


* Feb-2019 XLS file was added to nems default, scedes, housekeeping
*
parameter CoalDReg2RefRegMap(CoalDReg,RefReg);
parameter CornTranSupMove(RefReg);     /* 1000 bushel per day net corn movement */
parameter FirstBldYearMatrix(Process,RefReg);
parameter FirstBldYear;                /*Control 1st build year for most PUs (2018 aeo2019) */

$call GDXXRW %DirPath%input\CoalD2RefMap.xls o=CoalDRefMap.gdx index=IndexData!A1
*ABOVE replaces BELOW
*$call GDXXRW %DirPath%input\CoalD2RefMap.xls o=CoalDRefMap.gdx par=CoalDreg2RefRegMap rng=CoalDReg2RefReg!A3:I19 Cdim=1  Rdim=1 par=FirstBldYearMatrix rng=bldyrPUmatrix!C7:K61   Cdim=1  Rdim=1  par=CornTranSupMove    rng=CornProductionTransfer!A3:B10  Rdim=1

*  $call GDXXRW %DirPath%input\CoalD2RefMap.xls o=CoalDRefMap.gdx par=CornTranSupMove    rng=CornProductionTransfer!A3:B10 Rdim=1

$gdxin CoalDRefMap.gdx
$load CoalDReg2RefRegMap, CornTranSupMove, FirstBldYearMatrix, FirstBldYear
$gdxin

* Loop over FirstBldYearMatrix to redefine:
*   0 = 2100
*   1 = FirstBldYear

Loop( (Process,RefReg),
  if (FirstBldYearMatrix(Process,RefReg) = 0,
      FirstBldYearMatrix(Process,RefReg) = 2100 );
  if (FirstBldYearMatrix(Process,RefReg) = 1,
      FirstBldYearMatrix(Process,RefReg) = FirstBldYear );
);

*FirstBldYear = FirstBldYear-1989 ;


parameter CrudeImportShares(Crude,t);

$call GDXXRW %DirPath%input\intcrdshr.xls o=crudeshares.gdx par=CrudeImportShares rng=A1:CN10 Cdim=1  Rdim=1

*Display  '%DirPath%input\intcrdshr.xls';

*INTCRDSH

$gdxin crudeshares.gdx
$load CrudeImportShares
$gdxin

*Display  CrudeImportShares;


CrudeImportFactor(t) = 1.000;


NonUSCrudeDemandValue(Source,Crude,Step,ModelYears(t)) = 0.0;
NonUSCrudeDemandValue('Foreign',Crude,Step,ModelYears(t)) =
   sum((MNCRUD,M1,S1,MNXYRS)$(StepS1(Step,S1) and MNCRUD_2_Crude(MNCRUD,Crude) and tMNXYRS(t,MNXYRS)),
   INTOUT_Q_Non_US_Demand(MNCRUD,M1,S1,MNXYRS));


*  Manage process modes allowed based on the crudes available in the regions operated ....SupTyp_RefType_Process_Mode
Sets
    ProcProcMode(process,ProcessMode)              valid process - process mode combinations
    SupTypMode(RefReg,RefType,Process,ProcessMode) valid Refinery Region - Ref Type - Process Mode combinations excludes use of non_allowed crudes
    RefTypProc(RefType,Process)                    valid Reftyp - process combos
;

*Update these parameters and sets only once for each run
$if not exist LFrundata.gdx $goto SkipImport
$gdxin LFrundata.gdx
$Load ProcessTable, ProcProcMode, SupTypMode, RefTypProc
$gdxin
$GoTo SkipCalculations
$Label SkipImport

ProcessTable(Stream,Process,ProcessMode) = sum(CrudeAll,ProcessTableCrude(Stream,CrudeAll,Process,ProcessMode));


*TEMP TEMP TEMP 8-19-19 em4
*TEMP TEMP TEMP hardcode reduciton in still produced by each PU
*TEMP TEMP TEMP but apply only to output from SGP,UGP, LTE
ProcessTable('PGS','SGP',ProcessMode)$(ProcessTable('PGS','SGP',ProcessMode) > 0.01) =
                         ProcessTable('PGS','SGP',ProcessMode) - 0.01 ;
ProcessTable('PGS','UGP',ProcessMode)$(ProcessTable('PGS','UGP',ProcessMode) > 0.01) =
                         ProcessTable('PGS','UGP',ProcessMode) - 0.01 ;
ProcessTable('PGS','LTE',ProcessMode)$(ProcessTable('PGS','LTE',ProcessMode) > 0.01) =
                         ProcessTable('PGS','LTE',ProcessMode) - 0.01 ;


*-------------------------------------------------------------------------------------------------
* Benchmark factors for the ProcessTable parameters
Parameters
    Benchmark(stream)                 Benchmark factor to be applied to selected refinery PU fuels
;
Sets
    PetroleumProcessBench(PetroleumProcess)
;
*  HARDCODED benchmark factors
Benchmark(stream) = 1.0 ;
* orig
* Benchmark('KWH')  = 1.6 ;
* Benchmark('FUL')  = 1.20;
* 1st try (low)
* Benchmark('KWH')  = 1.9 ;
* Benchmark('FUL')  = 1.25;
* Benchmark('KWH')  = 1.80;
Benchmark('KWH')  = 2.05;
Benchmark('FUL')  = 1.15;

*  HARDCODED subset of petroleum process to undergo benchmarking
PetroleumProcessBench(PetroleumProcess) = 1 ;
PetroleumProcessBench('FUM') = 0 ;
PetroleumProcessBench('STG') = 0 ;
PetroleumProcessBench('KWG') = 0 ;
PetroleumProcessBench('CGN') = 0 ;

ProcessTable(Stream,PetroleumProcessBench,ProcessMode) =
  Benchmark(stream) * ProcessTable(Stream,PetroleumProcessBench,ProcessMode);

*-------------------------------------------------------------------------------------------------

ProcProcMode(Process,ProcessMode)        = sum(stream,abs(ProcessTable(stream,Process,ProcessMode)));
ProcProcMode('SAF','GDT_FCO') = yes ;
ProcProcMode('SAF','GDT_WGR') = yes ;
ProcProcMode('SAF','GDT_YGR') = yes ;

SupTypMode(RefReg,RefType,ProcProcMode(process,ProcessMode))
                                         = sum(stream,abs(ProcessTableCrude(Stream,'All',Process,ProcessMode))) +
                                           sum((stream,SAMEAS(Crude,CrudeAll)),abs(ProcessTableCrude(Stream,CrudeAll,Process,ProcessMode)))  ;

SupTypMode(RefReg,EthRefType(RefType),ProcProcMode(process,ProcessMode))
                                         = sum(stream,abs(ProcessTableCrude(Stream,'None',Process,ProcessMode)));

RefTypProc(RefType,Process)              = sum((RefReg,ProcessMode),SupTypMode(RefReg,RefType,Process,ProcessMode));

Execute_Unload 'LFrundata.gdx' ,Stream,Process,ProcessMode,RefReg,RefType, CrudeAll, ProcessTable, ProcProcMode, SupTypMode, RefTypProc ;

$Label SkipCalculations


*------Convert NEMS tables to LFMM tables for LCFS data

* Assign Associated Petroleum Carbon Factor to Non-Petroleum Substitute

LCFS_BioStream_PetCarbonFactor(LCFS_PetCategory,LCFS_BioProcess,LCFS_BioMode,LCFS_BioStreams,t)$ProcessTable(LCFS_BioStreams,LCFS_BioProcess,LCFS_BioMode) =
   Sum(LCFS_PetStreams$LCFS_BioStreams_PetStreams(LCFS_BioStreams,LCFS_PetStreams),
   LCFS_PetStream_Carbon_Factor(LCFS_PetCategory,LCFS_PetStreams,t));

LCFS_BioImport_PetCarbonFactor(LCFS_PetCategory,LCFS_BioImports,t) =
   Sum(LCFS_PetStreams$LCFS_BioImport_PetStream(LCFS_PetStreams,LCFS_BioImports),
   LCFS_PetStream_Carbon_Factor(LCFS_PetCategory,LCFS_PetStreams,t));

Energy_Density(IntStream) = StreamProp(IntStream,'END');

LCFS_Bio_Energy_Density(LCFS_BioStreams) = Energy_Density(LCFS_BioStreams);

LCFS_Imp_Energy_Density(LCFS_BioImports) = Energy_Density(LCFS_BioImports);

LCFS_Pet_Energy_Density('ASPHout',LCFS_Years) = CONVFACT_CFASQ;
LCFS_Pet_Energy_Density('N6Bout',LCFS_Years)  = CONVFACT_CFRSQ;
LCFS_Pet_Energy_Density('N6Iout',LCFS_Years)  = CONVFACT_CFRSQ;
LCFS_Pet_Energy_Density('AVGout',LCFS_Years)  = CONVFACT_CFAVQ;
LCFS_Pet_Energy_Density('LUBout',LCFS_Years)  = CONVFACT_CFLUQ;

* Initialize here  with a default value then overwrite where real data exists
LCFS_PET_Energy_Density('N2Hout',LCFS_Years)     = CONVFACT_CFDSRS('2011_MNUMYR');
LCFS_PET_Energy_Density('CarbDSUout',LCFS_Years) = CONVFACT_CFDSCQ('2011_MNUMYR');
LCFS_PET_Energy_Density('DSUout',LCFS_Years)     = CONVFACT_CFDSUQ('2011_MNUMYR');
LCFS_PET_Energy_Density('DSLout',LCFS_Years)     = CONVFACT_CFDSLQ('2011_MNUMYR');
LCFS_PET_Energy_Density('CaRBOBout',LCFS_Years)  = CONVFACT_CFCBQ('2011_MNUMYR');
LCFS_PET_Energy_Density('RFGout',LCFS_Years)     = CONVFACT_CFRGQ('2011_MNUMYR');
LCFS_PET_Energy_Density('RFG15out',LCFS_Years)   = CONVFACT_CFRGQ('2011_MNUMYR');
LCFS_PET_Energy_Density('CFGout',LCFS_Years)     = CONVFACT_CFTGQ('2011_MNUMYR');
LCFS_PET_Energy_Density('CFG15out',LCFS_Years)   = CONVFACT_CFTGQ('2011_MNUMYR');
LCFS_PET_Energy_Density('JTAout',LCFS_Years)     = CONVFACT_CFJFQ('2011_MNUMYR');
LCFS_PET_Energy_Density('E85out',LCFS_Years)     = CONVFACT_CFE85Q('2011_MNUMYR');

Loop(tMNUM(LCFS_Years,MNUMYR),
  LCFS_PET_Energy_Density('N2Hout',LCFS_Years)$(CONVFACT_CFDSRS(MNUMYR)>0) = CONVFACT_CFDSRS(MNUMYR);
  LCFS_PET_Energy_Density('CarbDSUout',LCFS_Years)$(CONVFACT_CFDSCQ(MNUMYR)>0) = CONVFACT_CFDSCQ(MNUMYR);
  LCFS_PET_Energy_Density('DSUout',LCFS_Years)$(CONVFACT_CFDSUQ(MNUMYR)>0)     = CONVFACT_CFDSUQ(MNUMYR);
  LCFS_PET_Energy_Density('DSLout',LCFS_Years)$(CONVFACT_CFDSLQ(MNUMYR)>0)     = CONVFACT_CFDSLQ(MNUMYR);
  LCFS_PET_Energy_Density('CaRBOBout',LCFS_Years)$(CONVFACT_CFCBQ(MNUMYR)>0)  = CONVFACT_CFCBQ(MNUMYR);
  LCFS_PET_Energy_Density('RFGout',LCFS_Years)$(CONVFACT_CFRGQ(MNUMYR)>0)     = CONVFACT_CFRGQ(MNUMYR);
  LCFS_PET_Energy_Density('RFG15out',LCFS_Years)$(CONVFACT_CFRGQ(MNUMYR)>0)  = CONVFACT_CFRGQ(MNUMYR);
  LCFS_PET_Energy_Density('CFGout',LCFS_Years)$(CONVFACT_CFTGQ(MNUMYR)>0)     = CONVFACT_CFTGQ(MNUMYR);
  LCFS_PET_Energy_Density('CFG15out',LCFS_Years)$(CONVFACT_CFTGQ(MNUMYR)>0)   = CONVFACT_CFTGQ(MNUMYR);
  LCFS_PET_Energy_Density('JTAout',LCFS_Years)$(CONVFACT_CFJFQ(MNUMYR)>0)     = CONVFACT_CFJFQ(MNUMYR);
  LCFS_PET_Energy_Density('E85out',LCFS_Years)$(CONVFACT_CFE85Q(MNUMYR)>0)     = CONVFACT_CFE85Q(MNUMYR);
);

* CFP data starts here, Assign Associated Petroleum Carbon Factor to Non-Petroleum Substitute

CFP_BioStream_PetCarbonFactor(CFP_PetCategory,CFP_BioProcess,CFP_BioMode,CFP_BioStreams,t)$ProcessTable(CFP_BioStreams,CFP_BioProcess,CFP_BioMode) =
   Sum(CFP_PetStreams$CFP_BioStreams_PetStreams(CFP_BioStreams,CFP_PetStreams),
   CFP_PetStream_Carbon_Factor(CFP_PetCategory,CFP_PetStreams,t));

CFP_BioImport_PetCarbonFactor(CFP_PetCategory,CFP_BioImports,t) =
   Sum(CFP_PetStreams$CFP_BioImport_PetStream(CFP_PetStreams,CFP_BioImports),
   CFP_PetStream_Carbon_Factor(CFP_PetCategory,CFP_PetStreams,t));

Energy_Density(IntStream) = StreamProp(IntStream,'END');

CFP_Bio_Energy_Density(CFP_BioStreams) = Energy_Density(CFP_BioStreams);

CFP_Imp_Energy_Density(CFP_BioImports) = Energy_Density(CFP_BioImports);

CFP_Pet_Energy_Density('ASPHout',CFP_Years) = CONVFACT_CFASQ;
CFP_Pet_Energy_Density('N6Bout',CFP_Years)  = CONVFACT_CFRSQ;
CFP_Pet_Energy_Density('N6Iout',CFP_Years)  = CONVFACT_CFRSQ;
CFP_Pet_Energy_Density('AVGout',CFP_Years)  = CONVFACT_CFAVQ;
CFP_Pet_Energy_Density('LUBout',CFP_Years)  = CONVFACT_CFLUQ;

* Initialize here  with a default value then overwrite where real data exists
CFP_PET_Energy_Density('N2Hout',CFP_Years)     = CONVFACT_CFDSRS('2011_MNUMYR');
CFP_PET_Energy_Density('DSUout',CFP_Years)     = CONVFACT_CFDSUQ('2011_MNUMYR');
CFP_PET_Energy_Density('DSLout',CFP_Years)     = CONVFACT_CFDSLQ('2011_MNUMYR');
CFP_PET_Energy_Density('CFGout',CFP_Years)     = CONVFACT_CFTGQ('2011_MNUMYR');
CFP_PET_Energy_Density('CFG15out',CFP_Years)   = CONVFACT_CFTGQ('2011_MNUMYR');
CFP_PET_Energy_Density('JTAout',CFP_Years)     = CONVFACT_CFJFQ('2011_MNUMYR');
CFP_PET_Energy_Density('E85out',CFP_Years)     = CONVFACT_CFE85Q('2011_MNUMYR');

Loop(tMNUM(CFP_Years,MNUMYR),
  CFP_PET_Energy_Density('N2Hout',CFP_Years)$(CONVFACT_CFDSRS(MNUMYR)>0) = CONVFACT_CFDSRS(MNUMYR);
  CFP_PET_Energy_Density('DSUout',CFP_Years)$(CONVFACT_CFDSUQ(MNUMYR)>0)     = CONVFACT_CFDSUQ(MNUMYR);
  CFP_PET_Energy_Density('DSLout',CFP_Years)$(CONVFACT_CFDSLQ(MNUMYR)>0)     = CONVFACT_CFDSLQ(MNUMYR);
  CFP_PET_Energy_Density('CFGout',CFP_Years)$(CONVFACT_CFTGQ(MNUMYR)>0)     = CONVFACT_CFTGQ(MNUMYR);
  CFP_PET_Energy_Density('CFG15out',CFP_Years)$(CONVFACT_CFTGQ(MNUMYR)>0)   = CONVFACT_CFTGQ(MNUMYR);
  CFP_PET_Energy_Density('JTAout',CFP_Years)$(CONVFACT_CFJFQ(MNUMYR)>0)     = CONVFACT_CFJFQ(MNUMYR);
  CFP_PET_Energy_Density('E85out',CFP_Years)$(CONVFACT_CFE85Q(MNUMYR)>0)     = CONVFACT_CFE85Q(MNUMYR);
);

* WACFS data starts here, Assign Associated Petroleum Carbon Factor to Non-Petroleum Substitute

WACFS_BioStream_PetCarbonFactor(WACFS_PetCategory,WACFS_BioProcess,WACFS_BioMode,WACFS_BioStreams,t)$ProcessTable(WACFS_BioStreams,WACFS_BioProcess,WACFS_BioMode) =
   Sum(WACFS_PetStreams$WACFS_BioStreams_PetStreams(WACFS_BioStreams,WACFS_PetStreams),
   WACFS_PetStream_Carbon_Factor(WACFS_PetCategory,WACFS_PetStreams,t));

WACFS_BioImport_PetCarbonFactor(WACFS_PetCategory,WACFS_BioImports,t) =
   Sum(WACFS_PetStreams$WACFS_BioImport_PetStream(WACFS_PetStreams,WACFS_BioImports),
   WACFS_PetStream_Carbon_Factor(WACFS_PetCategory,WACFS_PetStreams,t));

Energy_Density(IntStream) = StreamProp(IntStream,'END');

WACFS_Bio_Energy_Density(WACFS_BioStreams) = Energy_Density(WACFS_BioStreams);

WACFS_Imp_Energy_Density(WACFS_BioImports) = Energy_Density(WACFS_BioImports);

WACFS_Pet_Energy_Density('ASPHout',WACFS_Years) = CONVFACT_CFASQ;
WACFS_Pet_Energy_Density('N6Bout',WACFS_Years)  = CONVFACT_CFRSQ;
WACFS_Pet_Energy_Density('N6Iout',WACFS_Years)  = CONVFACT_CFRSQ;
WACFS_Pet_Energy_Density('AVGout',WACFS_Years)  = CONVFACT_CFAVQ;
WACFS_Pet_Energy_Density('LUBout',WACFS_Years)  = CONVFACT_CFLUQ;

* Initialize here  with a default value then overwrite where real data exists
WACFS_PET_Energy_Density('N2Hout',WACFS_Years)     = CONVFACT_CFDSRS('2011_MNUMYR');
WACFS_PET_Energy_Density('DSUout',WACFS_Years)     = CONVFACT_CFDSUQ('2011_MNUMYR');
WACFS_PET_Energy_Density('DSLout',WACFS_Years)     = CONVFACT_CFDSLQ('2011_MNUMYR');
WACFS_PET_Energy_Density('CFGout',WACFS_Years)     = CONVFACT_CFTGQ('2011_MNUMYR');
WACFS_PET_Energy_Density('CFG15out',WACFS_Years)   = CONVFACT_CFTGQ('2011_MNUMYR');
WACFS_PET_Energy_Density('JTAout',WACFS_Years)     = CONVFACT_CFJFQ('2011_MNUMYR');
WACFS_PET_Energy_Density('E85out',WACFS_Years)     = CONVFACT_CFE85Q('2011_MNUMYR');

Loop(tMNUM(WACFS_Years,MNUMYR),
  WACFS_PET_Energy_Density('N2Hout',WACFS_Years)$(CONVFACT_CFDSRS(MNUMYR)>0) = CONVFACT_CFDSRS(MNUMYR);
  WACFS_PET_Energy_Density('DSUout',WACFS_Years)$(CONVFACT_CFDSUQ(MNUMYR)>0)     = CONVFACT_CFDSUQ(MNUMYR);
  WACFS_PET_Energy_Density('DSLout',WACFS_Years)$(CONVFACT_CFDSLQ(MNUMYR)>0)     = CONVFACT_CFDSLQ(MNUMYR);
  WACFS_PET_Energy_Density('CFGout',WACFS_Years)$(CONVFACT_CFTGQ(MNUMYR)>0)     = CONVFACT_CFTGQ(MNUMYR);
  WACFS_PET_Energy_Density('CFG15out',WACFS_Years)$(CONVFACT_CFTGQ(MNUMYR)>0)   = CONVFACT_CFTGQ(MNUMYR);
  WACFS_PET_Energy_Density('JTAout',WACFS_Years)$(CONVFACT_CFJFQ(MNUMYR)>0)     = CONVFACT_CFJFQ(MNUMYR);
  WACFS_PET_Energy_Density('E85out',WACFS_Years)$(CONVFACT_CFE85Q(MNUMYR)>0)     = CONVFACT_CFE85Q(MNUMYR);
);

*-------
RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv)$(sum(CenDivA, RefReg_to_CenDiv_ACU_Map(RefReg,CenDivA) )>0) =
   RefReg_to_CenDiv_ACU_Map(RefReg,CenDiv) /
    sum(CenDivA, RefReg_to_CenDiv_ACU_Map(RefReg,CenDivA) ) ;

CenDiv_to_RefReg_ACU_Frac(RefReg,CenDiv)$(sum(RefRegA, RefReg_to_CenDiv_ACU_Map(RefRegA,CenDiv) )>0) =
   RefReg_to_CenDiv_ACU_Map(RefReg,CenDiv) /
    sum(RefRegA, RefReg_to_CenDiv_ACU_Map(RefRegA,CenDiv) ) ;

CenDiv_to_RefReg_ACU_Frac('1_RefReg','CenDiv1') = 1;

* Bootstrap for first year
LFMMOUT_RFSCREDPRC('1_M4','2011_MNUMYR') = 9.00;
LFMMOUT_RFSCREDPRC('4_M4','2011_MNUMYR') = 14.00;

LOOP ((M4,MNUMYR),
   IF ((LFMMOUT_RFSCREDPRC(M4,MNUMYR) = INF),
      LFMMOUT_RFSCREDPRC(M4,MNUMYR) = 0.0;
   );
);

*updated 8/16/16
* Fraction of HWY, ONR, OLM, and HOF distillate in each sector (rfctrl.txt)
Scalars
   QTotal
   HOFTRN / 0.00 /
   OLMTRN / 0.15 /
   ONRTRN / 0.00 /
   HWYTRN / 0.85 /
   HOFIND / 0.22 /
   OLMIND / 0.00 /
   ONRIND / 0.44 /
   HWYIND / 0.34 /
   HOFCOM / 0.33 /
   OLMCOM / 0.00 /
   ONRCOM / 0.00 /
   HWYCOM / 0.67 /
;

DieselFrac(EndProduct,DieselSector,ModelYears(t))$(ord(t)>LastYear) = sum(tt$(ord(tt)=LastYear), DieselFrac(EndProduct,DieselSector,tt) ) ;

*updated 8/16/16
Table MGShare(EndProduct,CenDiv)
          CenDiv1  CenDiv2 CenDiv3 CenDiv4 CenDiv5 CenDiv6 CenDiv7 CenDiv8 CenDiv9
CFGout     0.1449  0.3483  0.8109  0.8550  0.8317  0.9529  0.6939  0.8220  0.2576
RFGout     0.8551  0.6517  0.1891  0.1450  0.1683  0.0471  0.3061  0.1780  0.0000
CarBOBout  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.7424     ;




Table CarbDSUShare(EndProduct,CenDiv)
            CenDiv1 CenDiv2 CenDiv3 CenDiv4 CenDiv5 CenDiv6 CenDiv7 CenDiv8 CenDiv9
CarbDSUout   0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.7518   ;

* Convert NEMS demands into LFMM demands
Parameters
   SUMTRHWY(t)
   SUMTR(t)
   SUMIN(t)
   SUMCM(t)
   TRHWYPCT_N2H(t)
   TRHWYPCT_DSL(t)
   TRHWYPCT_DSU(t)
   TRPCT_N2H(t)
   TRPCT_DSL(t)
   TRPCT_DSU(t)
   INPCT_N2H(t)
   INPCT_DSL(t)
   INPCT_DSU(t)
   CMPCT_N2H(t)
   CMPCT_DSL(t)
   CMPCT_DSU(t)

   QCCRF(CenDiv,t)    Refinery cat coke demand
;

CONVFACT_CFDSLQ(MNUMYR)$(CONVFACT_CFDSLQ(MNUMYR)=0) = 5.8 ;
CONVFACT_CFDSUQ(MNUMYR)$(CONVFACT_CFDSUQ(MNUMYR)=0) = 5.8 ;

Loop((ModelYears(t),MNUMYR)$tMNUM(t,MNUMYR),

   SUMTRHWY(t)     = HWYTRN*DieselFrac('DSLout','HWY',t)/CONVFACT_CFDSLQ(MNUMYR) +
                     HWYTRN*DieselFrac('DSUout','HWY',t)/CONVFACT_CFDSUQ(MNUMYR) +
                     HWYTRN*DieselFrac('N2Hout','HWY',t)/CONVFACT_CFDSRS(MNUMYR) ;
   TRHWYPCT_N2H(t) = 0.0 ;
   TRHWYPCT_DSL(t) = (HWYTRN*DieselFrac('DSLout','HWY',t)/CONVFACT_CFDSLQ(MNUMYR)) / SUMTRHWY(t) ;
   TRHWYPCT_DSU(t) = (HWYTRN*DieselFrac('DSUout','HWY',t)/CONVFACT_CFDSUQ(MNUMYR)) / SUMTRHWY(t) ;

   SUMTR(t)        = HOFTRN/CONVFACT_CFDSLQ(MNUMYR) +
                     (OLMTRN*DieselFrac('DSLout','OLM',t) +
                      ONRTRN*DieselFrac('DSLout','ONR',t) +
                      HWYTRN*DieselFrac('DSLout','HWY',t))/CONVFACT_CFDSLQ(MNUMYR) +
                     (OLMTRN*DieselFrac('DSUout','OLM',t) +
                      ONRTRN*DieselFrac('DSUout','ONR',t) +
                      HWYTRN*DieselFrac('DSUout','HWY',t))/CONVFACT_CFDSUQ(MNUMYR) +
                     (OLMTRN*DieselFrac('N2Hout','OLM',t) +
                      ONRTRN*DieselFrac('N2Hout','ONR',t) +
                      HWYTRN*DieselFrac('N2Hout','HWY',t))/CONVFACT_CFDSRS(MNUMYR) ;

   TRPCT_N2H(t)    = (HOFTRN/CONVFACT_CFDSLQ(MNUMYR)) / SUMTR(t) ;

   TRPCT_DSL(t)    = (OLMTRN*DieselFrac('DSLout','OLM',t) +
                      ONRTRN*DieselFrac('DSLout','ONR',t) +
                      HWYTRN*DieselFrac('DSLout','HWY',t))/CONVFACT_CFDSLQ(MNUMYR) / SUMTR(t) ;

   TRPCT_DSU(t)    = (OLMTRN*DieselFrac('DSUout','OLM',t) +
                      ONRTRN*DieselFrac('DSUout','ONR',t) +
                      HWYTRN*DieselFrac('DSUout','HWY',t))/CONVFACT_CFDSUQ(MNUMYR) / SUMTR(t) ;

   SUMIN(t)        = HOFIND/CONVFACT_CFDSLQ(MNUMYR) +
                     (OLMIND*DieselFrac('DSLout','OLM',t) +
                      ONRIND*DieselFrac('DSLout','ONR',t) +
                      HWYIND*DieselFrac('DSLout','HWY',t))/CONVFACT_CFDSLQ(MNUMYR) +
                     (OLMIND*DieselFrac('DSUout','OLM',t) +
                      ONRIND*DieselFrac('DSUout','ONR',t) +
                      HWYIND*DieselFrac('DSUout','HWY',t))/CONVFACT_CFDSUQ(MNUMYR) ;

   INPCT_N2H(t)    = (HOFIND/CONVFACT_CFDSLQ(MNUMYR)) / SUMIN(t) ;

   INPCT_DSL(t)    = (OLMIND*DieselFrac('DSLout','OLM',t) +
                      ONRIND*DieselFrac('DSLout','ONR',t) +
                      HWYIND*DieselFrac('DSLout','HWY',t))/CONVFACT_CFDSLQ(MNUMYR) / SUMIN(t) ;

   INPCT_DSU(t)    = (OLMIND*DieselFrac('DSUout','OLM',t) +
                      ONRIND*DieselFrac('DSUout','ONR',t) +
                      HWYIND*DieselFrac('DSUout','HWY',t))/CONVFACT_CFDSUQ(MNUMYR) / SUMIN(t) ;

   SUMCM(t)        = HOFCOM/CONVFACT_CFDSLQ(MNUMYR) +
                     (OLMCOM*DieselFrac('DSLout','OLM',t) +
                      ONRCOM*DieselFrac('DSLout','ONR',t) +
                      HWYCOM*DieselFrac('DSLout','HWY',t))/CONVFACT_CFDSLQ(MNUMYR) +

                     (OLMCOM*DieselFrac('DSUout','OLM',t) +
                      ONRCOM*DieselFrac('DSUout','ONR',t) +
                      HWYCOM*DieselFrac('DSUout','HWY',t))/CONVFACT_CFDSUQ(MNUMYR) ;

   CMPCT_N2H(t)    = (HOFCOM/CONVFACT_CFDSLQ(MNUMYR)) / SUMCM(t) ;

   CMPCT_DSL(t)    = (OLMCOM*DieselFrac('DSLout','OLM',t) +
                      ONRCOM*DieselFrac('DSLout','ONR',t) +
                      HWYCOM*DieselFrac('DSLout','HWY',t))/CONVFACT_CFDSLQ(MNUMYR) / SUMCM(t) ;

   CMPCT_DSU(t)    = (OLMCOM*DieselFrac('DSUout','OLM',t) +
                      ONRCOM*DieselFrac('DSUout','ONR',t) +
                      HWYCOM*DieselFrac('DSUout','HWY',t))/CONVFACT_CFDSUQ(MNUMYR) / SUMCM(t) ;

   Loop((CenDiv,MNUMCR)$crN2L(CenDiv,MNUMCR),
       FlexFuelDemand(CenDiv,t) = TRANREP_QFFV(MNUMCR,MNUMYR);
   );

   QCCRF(ActiveDem,t) = sum(M11$CenDiv_2_M11(ActiveDem,M11), INDREP_QCCRF(M11,MNUMYR) ) ;

);

loop((ModelYears(t),tt)$((ord(t)>LastYear) and (ord(tt)=LastYear)),

   SUMTRHWY(t)              = SUMTRHWY(tt) ;
   TRHWYPCT_DSL(t)          = TRHWYPCT_DSL(tt) ;
   TRHWYPCT_DSU(t)          = TRHWYPCT_DSU(tt) ;
   SUMTR(t)                 = SUMTR(tt) ;
   TRPCT_N2H(t)             = TRPCT_N2H(tt) ;
   TRPCT_DSL(t)             = TRPCT_DSL(tt) ;
   TRPCT_DSU(t)             = TRPCT_DSU(tt) ;
   SUMIN(t)                 = SUMIN(tt) ;
   INPCT_N2H(t)             = INPCT_N2H(tt) ;
   INPCT_DSL(t)             = INPCT_DSL(tt) ;
   INPCT_DSU(t)             = INPCT_DSU(tt) ;
   SUMCM(t)                 = SUMCM(tt) ;
   CMPCT_N2H(t)             = CMPCT_N2H(tt) ;
   CMPCT_DSL(t)             = CMPCT_DSL(tt) ;
   CMPCT_DSU(t)             = CMPCT_DSU(tt) ;
   FlexFuelDemand(CenDiv,t) = FlexFuelDemand(CenDiv,tt) ;

   QCCRF(ActiveDem,t)       = QCCRF(ActiveDem,tt) ;

);

* 5-11-17, according to TRAN, array definition for XTRQLDV & TRQLDV
* ... 1=gasoline   2=methanol   3=ethanol          4=cng
* ... 5=lpg       *6=electric*  7=liquid hydrogen  8=distillate

* LCFS Alternative Vehicle Demands in California
loop((ModelYears(LCFS_Years),MNXYRS)$tMNXYRS(LCFS_Years,MNXYRS),
  LCFS_AltVehicle_Demand(LCFS_PetCategory,'CNG',LCFS_RefReg,LCFS_Years) = MXQBLK_XQNGTR('09_Pacific',MNXYRS)*
     LCFS_AltVehicle_CA_Share(LCFS_PetCategory,'CNG');
  LCFS_AltVehicle_Demand(LCFS_PetCategory,'LPG',LCFS_RefReg,LCFS_Years) = MXQBLK_XQLGTR('09_Pacific',MNXYRS)*
     LCFS_AltVehicle_CA_Share(LCFS_PetCategory,'LPG');
  LCFS_AltVehicle_Demand(LCFS_PetCategory,'ELV',LCFS_RefReg,LCFS_Years) = TRANREP_XTRQLDV('6_M8','09_Pacific',MNXYRS)*
     LCFS_AltVehicle_CA_Share(LCFS_PetCategory,'ELV');
);
loop((ModelYears(LCFS_Years),MNUMYR)$tMNUM(LCFS_Years,MNUMYR),
  LCFS_AltVehicle_Demand(LCFS_PetCategory,'CNG',LCFS_RefReg,LCFS_Years) = QBLK_QNGTR('09_Pacific',MNUMYR)*
     LCFS_AltVehicle_CA_Share(LCFS_PetCategory,'CNG');
  LCFS_AltVehicle_Demand(LCFS_PetCategory,'LPG',LCFS_RefReg,LCFS_Years) = QBLK_QLGTR('09_Pacific',MNUMYR)*
     LCFS_AltVehicle_CA_Share(LCFS_PetCategory,'LPG');
  LCFS_AltVehicle_Demand(LCFS_PetCategory,'ELV',LCFS_RefReg,LCFS_Years) = TRANREP_TRQLDV('6_M8','09_Pacific',MNUMYR)*
     LCFS_AltVehicle_CA_Share(LCFS_PetCategory,'ELV');
);

* CFP Alternative Vehicle Demands in Oregon
loop((ModelYears(CFP_Years),MNXYRS)$tMNXYRS(CFP_Years,MNXYRS),
  CFP_AltVehicle_Demand(CFP_PetCategory,'CNG',CFP_RefReg,CFP_Years) = MXQBLK_XQNGTR('08_Mountain',MNXYRS)*
     CFP_AltVehicle_OR_Share(CFP_PetCategory,'CNG');
  CFP_AltVehicle_Demand(CFP_PetCategory,'LPG',CFP_RefReg,CFP_Years) = MXQBLK_XQLGTR('08_Mountain',MNXYRS)*
     CFP_AltVehicle_OR_Share(CFP_PetCategory,'LPG');
  CFP_AltVehicle_Demand(CFP_PetCategory,'ELV',CFP_RefReg,CFP_Years) = TRANREP_XTRQLDV('6_M8','08_Mountain',MNXYRS)*
     CFP_AltVehicle_OR_Share(CFP_PetCategory,'ELV');
);
loop((ModelYears(CFP_Years),MNUMYR)$tMNUM(CFP_Years,MNUMYR),
  CFP_AltVehicle_Demand(CFP_PetCategory,'CNG',CFP_RefReg,CFP_Years) = QBLK_QNGTR('08_Mountain',MNUMYR)*
     CFP_AltVehicle_OR_Share(CFP_PetCategory,'CNG');
  CFP_AltVehicle_Demand(CFP_PetCategory,'LPG',CFP_RefReg,CFP_Years) = QBLK_QLGTR('08_Mountain',MNUMYR)*
     CFP_AltVehicle_OR_Share(CFP_PetCategory,'LPG');
  CFP_AltVehicle_Demand(CFP_PetCategory,'ELV',CFP_RefReg,CFP_Years) = TRANREP_TRQLDV('6_M8','08_Mountain',MNUMYR)*
     CFP_AltVehicle_OR_Share(CFP_PetCategory,'ELV');
);

* WACFS Alternative Vehicle Demands in Washington
loop((ModelYears(WACFS_Years),MNXYRS)$tMNXYRS(WACFS_Years,MNXYRS),
  WACFS_AltVehicle_Demand(WACFS_PetCategory,'CNG',WACFS_RefReg,WACFS_Years) = MXQBLK_XQNGTR('08_Mountain',MNXYRS)*
     WACFS_AltVehicle_WA_Share(WACFS_PetCategory,'CNG');
  WACFS_AltVehicle_Demand(WACFS_PetCategory,'LPG',WACFS_RefReg,WACFS_Years) = MXQBLK_XQLGTR('08_Mountain',MNXYRS)*
     WACFS_AltVehicle_WA_Share(WACFS_PetCategory,'LPG');
  WACFS_AltVehicle_Demand(WACFS_PetCategory,'ELV',WACFS_RefReg,WACFS_Years) = TRANREP_XTRQLDV('6_M8','08_Mountain',MNXYRS)*
     WACFS_AltVehicle_WA_Share(WACFS_PetCategory,'ELV');
);
loop((ModelYears(WACFS_Years),MNUMYR)$tMNUM(WACFS_Years,MNUMYR),
  WACFS_AltVehicle_Demand(WACFS_PetCategory,'CNG',WACFS_RefReg,WACFS_Years) = QBLK_QNGTR('08_Mountain',MNUMYR)*
     WACFS_AltVehicle_WA_Share(WACFS_PetCategory,'CNG');
  WACFS_AltVehicle_Demand(WACFS_PetCategory,'LPG',WACFS_RefReg,WACFS_Years) = QBLK_QLGTR('08_Mountain',MNUMYR)*
     WACFS_AltVehicle_WA_Share(WACFS_PetCategory,'LPG');
  WACFS_AltVehicle_Demand(WACFS_PetCategory,'ELV',WACFS_RefReg,WACFS_Years) = TRANREP_TRQLDV('6_M8','08_Mountain',MNUMYR)*
     WACFS_AltVehicle_WA_Share(WACFS_PetCategory,'ELV');
);
*parameter CCATSDAT_CO2_PRC_DIS_45Q_lfmm(DomRefReg, t)   CO2 price dimensioned by DomRefReg for LFMM;

*CCATSDAT_CO2_PRC_DIS_45Q_lfmm(DomRefReg, t) = sum(MNUMYR$tMNUM(t,MNUMYR),sum((ActiveDem,MNUMCR)$(crN2L(ActiveDem,MNUMCR)and RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)),
*        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*CCATSDAT_CO2_PRC_DIS_45Q(MNUMCR,MNUMYR)));

*loop((CenDiv,MNUMCR)$crN2L(CenDiv,MNUMCR),
*   loop((t,MNUMYR)$tMNUM(t,MNUMYR),
*     CCATSDAT_CO2_PRC_DIS_45Q(MNUMCR,MNUMYR) = CCATSDAT_CO2_PRC_DIS_45Q(CenDiv,t) ));

loop((CenDiv,MNUMCR)$crN2L(CenDiv,MNUMCR),
   loop((ModelYears(t),MNXYRS)$tMNXYRS(t,MNXYRS),

      /* Fill in all_years with expected values, then_overwrite run year */

     ProductDemand(CenDiv,'AVGout',t)    =  TRANREP_XQAGTR(MNUMCR,MNXYRS);
     ProductDemand(CenDiv,'LUBout',t)    =  MXQBLK_XQLUIN(MNUMCR,MNXYRS) + TRANREP_XQLUTR(MNUMCR,MNXYRS);
     ProductDemand_nonQLU(CenDiv,t)      =  MXQBLK_XQOTIN(MNUMCR,MNXYRS) - MXQBLK_XQOTRF(MNUMCR,MNXYRS) -
                                            MXQBLK_XQLUIN(MNUMCR,MNXYRS) ;

     ProductDemand(CenDiv,'JTAout',t)    =  MXQBLK_XQJFTR(MNUMCR,MNXYRS) + MXQBLK_XQKSAS(MNUMCR,MNXYRS) ;
     ProductDemand(CenDiv,'N6Iout',t)    =  max(MXQBLK_XQRLAS(MNUMCR,MNXYRS),0);
     ProductDemand(CenDiv,'N6Bout',t)    =  max(MXQBLK_XQRHAS(MNUMCR,MNXYRS),0) ;
*     ProductDemand(CenDiv,'N6Iout',t)    =  max(MXQBLK_XQRLAS(MNUMCR,MNXYRS)- MXQBLK_XQRLEL(MNUMCR,MNXYRS) - MXQBLK_XQRLRF(MNUMCR,MNXYRS),0);
*     ProductDemand(CenDiv,'N6Bout',t)    =  max(MXQBLK_XQRHAS(MNUMCR,MNXYRS)- MXQBLK_XQRHEL(MNUMCR,MNXYRS),0) ;
     ProductDemand(CenDiv,'ASPHout',t)   =      MXQBLK_XQASIN(MNUMCR,MNXYRS) ;
     ProductDemand(CenDiv,'PCFout',t)    =      MXQBLK_XQPFIN(MNUMCR,MNXYRS) ;
     ProductDemand(CenDiv,'CFGout',t)    =      MXQBLK_XQMGAS(MNUMCR,MNXYRS) ;
     ProductDemand(CenDiv,'RFGout',t)    =      MXQBLK_XQMGAS(MNUMCR,MNXYRS) ;
     ProductDemand(CenDiv,'CaRBOBout',t) =      MXQBLK_XQMGAS(MNUMCR,MNXYRS) ;
     ProductDemand(CenDiv,'E85out',t)    =      MXQBLK_XQETTR(MNUMCR,MNXYRS) ;

* Assumed 5.0 COE/short ton coke
     ProductDemand(CenDiv,'COKout',t)  = max(MXQBLK_XQPCAS(MNUMCR,MNXYRS) - MXQBLK_XQPCRF(MNUMCR,MNXYRS) - QCCRF(CenDiv,t),0) ;
*     ProductDemand(CenDiv,'COKout',t)  = MXQBLK_XQPCAS(MNUMCR,MNXYRS) ;

* Distillate demands using the appraoch taken in subroutine PMM_DIESEL() in NEMS
     ProductDemand(CenDiv,'N2Hout',t)    =  HOFTRN*MXQBLK_XQDSTR(MNUMCR,MNXYRS) + HOFIND*MXQBLK_XQDSIN(MNUMCR,MNXYRS) +
                                            HOFCOM*MXQBLK_XQDSCM(MNUMCR,MNXYRS) + MXQBLK_XQDSRS(MNUMCR,MNXYRS) + MXQBLK_XQDSEL(MNUMCR,MNXYRS) ;

     ProductDemand(CenDiv,'DSLout',t)    = (OLMTRN*MXQBLK_XQDSTR(MNUMCR,MNXYRS) + OLMIND*MXQBLK_XQDSIN(MNUMCR,MNXYRS) + OLMCOM*MXQBLK_XQDSCM(MNUMCR,MNXYRS))*DieselFrac('DSLout','OLM',t) +
                                           (ONRTRN*MXQBLK_XQDSTR(MNUMCR,MNXYRS) + ONRIND*MXQBLK_XQDSIN(MNUMCR,MNXYRS) + ONRCOM*MXQBLK_XQDSCM(MNUMCR,MNXYRS))*DieselFrac('DSLout','ONR',t) +
                                           (HWYTRN*MXQBLK_XQDSTR(MNUMCR,MNXYRS) + HWYIND*MXQBLK_XQDSIN(MNUMCR,MNXYRS) + HWYCOM*MXQBLK_XQDSCM(MNUMCR,MNXYRS))*DieselFrac('DSLout','HWY',t);

     ProductDemand(CenDiv,'DSUout',t)    = (1.0 - CarbDSUShare('CarbDSUout',CenDiv))*((OLMTRN*MXQBLK_XQDSTR(MNUMCR,MNXYRS) + OLMIND*MXQBLK_XQDSIN(MNUMCR,MNXYRS) + OLMCOM*MXQBLK_XQDSCM(MNUMCR,MNXYRS))*DieselFrac('DSUout','OLM',t) +
                                           (ONRTRN*MXQBLK_XQDSTR(MNUMCR,MNXYRS) + ONRIND*MXQBLK_XQDSIN(MNUMCR,MNXYRS) + ONRCOM*MXQBLK_XQDSCM(MNUMCR,MNXYRS))*DieselFrac('DSUout','ONR',t) +
                                           (HWYTRN*MXQBLK_XQDSTR(MNUMCR,MNXYRS) + HWYIND*MXQBLK_XQDSIN(MNUMCR,MNXYRS) + HWYCOM*MXQBLK_XQDSCM(MNUMCR,MNXYRS))*DieselFrac('DSUout','HWY',t) );

     ProductDemand(CenDiv,'CarbDSUout',t)= CarbDSUShare('CarbDSUout',CenDiv)*((OLMTRN*MXQBLK_XQDSTR(MNUMCR,MNXYRS) + OLMIND*MXQBLK_XQDSIN(MNUMCR,MNXYRS) + OLMCOM*MXQBLK_XQDSCM(MNUMCR,MNXYRS))*DieselFrac('DSUout','OLM',t) +
                                           (ONRTRN*MXQBLK_XQDSTR(MNUMCR,MNXYRS) + ONRIND*MXQBLK_XQDSIN(MNUMCR,MNXYRS) + ONRCOM*MXQBLK_XQDSCM(MNUMCR,MNXYRS))*DieselFrac('DSUout','ONR',t) +
                                           (HWYTRN*MXQBLK_XQDSTR(MNUMCR,MNXYRS) + HWYIND*MXQBLK_XQDSIN(MNUMCR,MNXYRS) + HWYCOM*MXQBLK_XQDSCM(MNUMCR,MNXYRS))*DieselFrac('DSUout','HWY',t) );

     BTUDemand(CenDiv,t)                 = MXQBLK_XQMGAS(MNUMCR,MNXYRS) + MXQBLK_XQETTR(MNUMCR,MNXYRS) ;

   );  /* end_of_loop: (t,MNXYRS) */

* Now update the first year to use actual demand and apply conversion factors  - still in CenDiv_loop
   loop((Runyears(t),MNUMYR)$tMNUM(t,MNUMYR),
     ProductDemand(CenDiv,'AVGout',t)     =   TRANREP_QAGTR(MNUMCR,MNUMYR);
     ProductDemand(CenDiv,'LUBout',t)     =   QMORE_QLUIN(MNUMCR,MNUMYR) + TRANREP_QLUTR(MNUMCR,MNUMYR);
     ProductDemand_nonQLU(CenDiv,t)       =   QBLK_QOTIN(MNUMCR,MNUMYR) - QBLK_QOTRF(MNUMCR,MNUMYR) -
                                              QMORE_QLUIN(MNUMCR,MNUMYR) ;

     ProductDemand(CenDiv,'JTAout',t)     =   QBLK_QJFTR(MNUMCR,MNUMYR)+ QBLK_QKSAS(MNUMCR,MNUMYR) ;
     ProductDemand(CenDiv,'N6Iout',t)     =   max(QBLK_QRLAS(MNUMCR,MNUMYR),0) ;
     ProductDemand(CenDiv,'N6Bout',t)     =   max(QBLK_QRHAS(MNUMCR,MNUMYR),0) ;
*     ProductDemand(CenDiv,'N6Iout',t)     =   max(QBLK_QRLAS(MNUMCR,MNUMYR)- QBLK_QRLEL(MNUMCR,MNUMYR) - QBLK_QRLRF(MNUMCR,MNUMYR),0) ;
*     ProductDemand(CenDiv,'N6Bout',t)     =   max(QBLK_QRHAS(MNUMCR,MNUMYR)- QBLK_QRHEL(MNUMCR,MNUMYR),0) ;
     ProductDemand(CenDiv,'ASPHout',t)    =   QBLK_QASIN(MNUMCR,MNUMYR) ;
     ProductDemand(CenDiv,'PCFout',t)     =   QBLK_QPFIN(MNUMCR,MNUMYR) ;
     ProductDemand(CenDiv,'CFGout',t)     =   QBLK_QMGAS(MNUMCR,MNUMYR) ;
     ProductDemand(CenDiv,'RFGout',t)     =   QBLK_QMGAS(MNUMCR,MNUMYR) ;
     ProductDemand(CenDiv,'CaRBOBout',t)  =   QBLK_QMGAS(MNUMCR,MNUMYR) ;
     ProductDemand(CenDiv,'E85out',t)     =   QBLK_QETTR(MNUMCR,MNUMYR) ;

* Assumed 5.0 COE/short ton coke
     ProductDemand(CenDiv,'COKout',t)  = max(QBLK_QPCAS(MNUMCR,MNUMYR) - QBLK_QPCRF(MNUMCR,MNUMYR) - QCCRF(CenDiv,t), 0) ;
*     ProductDemand(CenDiv,'COKout',t)  = QBLK_QPCAS(MNUMCR,MNUMYR) ;

* Distillate demands using the appraoch taken in subroutine PMM_DIESEL() in NEMS
     ProductDemand(CenDiv,'N2Hout',t) =  HOFTRN*QBLK_QDSTR(MNUMCR,MNUMYR) + HOFIND*QBLK_QDSIN(MNUMCR,MNUMYR) +
                                                   HOFCOM*QBLK_QDSCM(MNUMCR,MNUMYR) + QBLK_QDSRS(MNUMCR,MNUMYR) + QBLK_QDSEL(MNUMCR,MNUMYR) ;

     ProductDemand(CenDiv,'DSLout',t) = (OLMTRN*QBLK_QDSTR(MNUMCR,MNUMYR) + OLMIND*QBLK_QDSIN(MNUMCR,MNUMYR) + OLMCOM*QBLK_QDSCM(MNUMCR,MNUMYR))*DieselFrac('DSLout','OLM',t) +
                                                  (ONRTRN*QBLK_QDSTR(MNUMCR,MNUMYR) + ONRIND*QBLK_QDSIN(MNUMCR,MNUMYR) + ONRCOM*QBLK_QDSCM(MNUMCR,MNUMYR))*DieselFrac('DSLout','ONR',t) +
                                                  (HWYTRN*QBLK_QDSTR(MNUMCR,MNUMYR) + HWYIND*QBLK_QDSIN(MNUMCR,MNUMYR) + HWYCOM*QBLK_QDSCM(MNUMCR,MNUMYR))*DieselFrac('DSLout','HWY',t) ;

     ProductDemand(CenDiv,'DSUout',t) = (1.0 - CarbDSUShare('CarbDSUout',CenDiv))*((OLMTRN*QBLK_QDSTR(MNUMCR,MNUMYR) + OLMIND*QBLK_QDSIN(MNUMCR,MNUMYR) + OLMCOM*QBLK_QDSCM(MNUMCR,MNUMYR))*DieselFrac('DSUout','OLM',t) +
                                                  (ONRTRN*QBLK_QDSTR(MNUMCR,MNUMYR) + ONRIND*QBLK_QDSIN(MNUMCR,MNUMYR) + ONRCOM*QBLK_QDSCM(MNUMCR,MNUMYR))*DieselFrac('DSUout','ONR',t) +
                                                  (HWYTRN*QBLK_QDSTR(MNUMCR,MNUMYR) + HWYIND*QBLK_QDSIN(MNUMCR,MNUMYR) + HWYCOM*QBLK_QDSCM(MNUMCR,MNUMYR))*DieselFrac('DSUout','HWY',t) ) ;

     ProductDemand(CenDiv,'CarbDSUout',t) = CarbDSUShare('CarbDSUout',CenDiv)*((OLMTRN*QBLK_QDSTR(MNUMCR,MNUMYR) + OLMIND*QBLK_QDSIN(MNUMCR,MNUMYR) + OLMCOM*QBLK_QDSCM(MNUMCR,MNUMYR))*DieselFrac('DSUout','OLM',t) +
                                                  (ONRTRN*QBLK_QDSTR(MNUMCR,MNUMYR) + ONRIND*QBLK_QDSIN(MNUMCR,MNUMYR) + ONRCOM*QBLK_QDSCM(MNUMCR,MNUMYR))*DieselFrac('DSUout','ONR',t) +
                                                  (HWYTRN*QBLK_QDSTR(MNUMCR,MNUMYR) + HWYIND*QBLK_QDSIN(MNUMCR,MNUMYR) + HWYCOM*QBLK_QDSCM(MNUMCR,MNUMYR))*DieselFrac('DSUout','HWY',t) );

*On-road distillate quantity, transportation sector
     QONROAD_QDSTRHWY(MNUMCR,MNUMYR)            = HWYTRN * QBLK_QDSTR(MNUMCR,MNUMYR) ;

     BTUDemand(CenDiv,t)              = QBLK_QMGAS(MNUMCR,MNUMYR) + QBLK_QETTR(MNUMCR,MNUMYR) ;

   );  /* end_of_loop: (t,MNUMYR) */

);     /* end_of_loop: (CenDiv,MNUMCR) */

ProductDemand(CenDiv,'E85out',ModelYears(t)) = Min(ProductDemand(CenDiv,'E85out',t) , FlexFuelDemand(CenDiv,t));

* Now apply conversion factors to the merged demand values
ProductDemand(CenDiv,'N6Iout',ModelYears(t))  = ProductDemand(CenDiv,'N6Iout',t) * 1000.0/CONVFACT_CFRSQ /365.0 ;
ProductDemand(CenDiv,'N6Bout',ModelYears(t))  = ProductDemand(CenDiv,'N6Bout',t) * 1000.0/CONVFACT_CFRSQ /365.0 ;
ProductDemand(CenDiv,'ASPHout',ModelYears(t)) = ProductDemand(CenDiv,'ASPHout',t) * 1000.0/CONVFACT_CFASQ /365.0 ;
ProductDemand(CenDiv,'COKout',ModelYears(t)) = ProductDemand(CenDiv,'COKout',t) * 1000.0/CONVFACT_CFPCQ /365.0 ;
ProductDemand(CenDiv,'AVGout',ModelYears(t)) = ProductDemand(CenDiv,'AVGout',t) * 1000.0/CONVFACT_CFAVQ /365.0 ;
*  ProductDemand(CenDiv,'LUBout',ModelYears(t)) = ProductDemand(CenDiv,'LUBout',t) * 1000.0/CONVFACT_CFLUQ /365.0 ;

ProductDemand(CenDiv,'LUBout',ModelYears(t))$(ord(t)<=LastYear) =
   sum(MNUMYR$tMNUM(t,MNUMYR), ProductDemand(CenDiv,'LUBout',t) * 1000.0/CONVFACT_CFLUQ /365.0 +
                               ProductDemand_nonQLU(CenDiv,t)   * 1000.0/CONVFACT_CFOTQ(MNUMYR) /365.0 ) ;
ProductDemand(CenDiv,'LUBout',ModelYears(t))$(ord(t)>LastYear) =
  sum(MNUMYR$(ord(MNUMYR)=LastYear), ProductDemand(CenDiv,'LUBout',t) * 1000.0/CONVFACT_CFLUQ /365.0 +
                                     ProductDemand_nonQLU(CenDiv,t)   * 1000.0/CONVFACT_CFOTQ(MNUMYR) /365.0 ) ;

ProductDemand(CenDiv,'JTAout',ModelYears(t))$(ord(t)<=LastYear) =
  sum(MNUMYR$tMNUM(t,MNUMYR), ProductDemand(CenDiv,'JTAout',t) * 1000.0/CONVFACT_CFJFQ(MNUMYR) /365.0 ) ;
ProductDemand(CenDiv,'JTAout',ModelYears(t))$(ord(t)>LastYear) =
  sum(MNUMYR$(ord(MNUMYR)=LastYear), ProductDemand(CenDiv,'JTAout',t) * 1000.0/CONVFACT_CFJFQ(MNUMYR))/365.0 ;

ProductDemand(CenDiv,'PCFout',ModelYears(t))$(ord(t)<=LastYear) =
  sum(MNUMYR$tMNUM(t,MNUMYR), ProductDemand(CenDiv,'PCFout',t) * 1000.0/CONVFACT_CFPFQ(MNUMYR))/365.0 ;
ProductDemand(CenDiv,'PCFout',ModelYears(t))$(ord(t)>LastYear) =
  sum(MNUMYR$(ord(MNUMYR)=LastYear), ProductDemand(CenDiv,'PCFout',t) * 1000.0/CONVFACT_CFPFQ(MNUMYR))/365.0 ;

* Leave BTU demand in billion BTUs per day
BTUDemand(CenDiv,ModelYears(t))  = BTUDemand(CenDiv,t)*1000/365.0 ;

* Motor gas demand using the approach taken in NEMS
Parameters
     CFTGQ(t)                 Conventional gasoline Conversion Factor for each year t
     CFRGQ(t)                 Reformulated gasoline Conversion Factor for each year t
     CFETQ(t)                 E85 Conversion Factor for each year t
     CFRBOB(t)                RBOB Conversion Factor for each year t
     CFCBOB(t)                CBOB Conversion Factor for each year t
     CFCBQ(t)                 CaRBOB Conversion Factor for each year t
     CFCaGQ(t)                CARB gasoline Conversion Factor for each year t
;

CFTGQ(ModelYears(t)) = sum(MNUMYR$tMNUM(t,MNUMYR),CONVFACT_CFTGQ(MNUMYR));
CFRGQ(ModelYears(t)) = sum(MNUMYR$tMNUM(t,MNUMYR),CONVFACT_CFRGQ(MNUMYR));
CFETQ(ModelYears(t)) = sum(MNUMYR$tMNUM(t,MNUMYR),CONVFACT_CFETQ(MNUMYR));
CFRBOB(ModelYears(t)) = sum(MNUMYR$tMNUM(t,MNUMYR),CONVFACT_CFRBOB(MNUMYR));
CFCBOB(ModelYears(t)) = sum(MNUMYR$tMNUM(t,MNUMYR),CONVFACT_CFCBOB(MNUMYR));
CFCBQ(ModelYears(t)) = sum(MNUMYR$tMNUM(t,MNUMYR),CONVFACT_CFCBQ(MNUMYR));
CFCaGQ(ModelYears(t)) = sum(MNUMYR$tMNUM(t,MNUMYR),CONVFACT_APICAMG('2_M2',MNUMYR));

* Use data from year 2050 for everything beyond LastYear
CFTGQ(ModelYears(t))$(ord(t)>(AEOLSTYR-1989)) = CFTGQ('2050');
CFRGQ(ModelYears(t))$(ord(t)>(AEOLSTYR-1989)) = CFRGQ('2050');
CFETQ(ModelYears(t))$(ord(t)>(AEOLSTYR-1989)) = CFETQ('2050');
CFRBOB(ModelYears(t))$(ord(t)>(AEOLSTYR-1989)) = CFRBOB('2050');
CFCBOB(ModelYears(t))$(ord(t)>(AEOLSTYR-1989)) = CFCBOB('2050');
CFCBQ(ModelYears(t))$(ord(t)>(AEOLSTYR-1989)) = CFCBQ('2050');
CFCaGQ(ModelYears(t))$(ord(t)>(AEOLSTYR-1989)) = CFCaGQ('2050');

Parameters
   CFMGQCD(CenDiv,t)
   CFE85Q(t)
;
   CFMGQCD(CenDiv,ModelYears(t)) = MGShare('CFGout',CenDiv)*CFTGQ(t) + MGShare('RFGout',CenDiv)*CFRGQ(t) + MGShare('CaRBOBout',CenDiv)*CFCaGQ(t);
   CFE85Q(ModelYears(t))         = -RecipeBlending('RCP_E85a','CBOB')*CFCBOB(t) + (1+RecipeBlending('RCP_E85a','CBOB'))*CFETQ(t) ;


Loop((ModelYears(t),MNUMYR)$(tMNUM(t,MNUMYR) and (ord(t)<=LastYear)),

* calculate ULSD heating oil mandate level (1000 bbl/yr) as percent of N2H ProductDemand
     ULSD_N2H_CD(ActiveDem,t) = ULSD_N2H_percent(ActiveDem,t) *
                                ProductDemand(ActiveDem,'N2Hout',t) *1000.0/ CONVFACT_CFDSRS(MNUMYR) ;

* These are only used to calculate the RFS fractions, these demands are enforced through a combined BTU constraint!
     ProductDemand(ActiveDem,'CFGout',t)     = (MGShare('CFGout',ActiveDem)    * ProductDemand(ActiveDem,'CFGout',t)*1000.0/CFTGQ(t))/365.0 ;
     ProductDemand(ActiveDem,'RFGout',t)     = (MGShare('RFGout',ActiveDem)    * ProductDemand(ActiveDem,'RFGout',t)*1000.0/CFRGQ(t))/365.0 ;
     ProductDemand(ActiveDem,'CaRBOBout',t)  = (MGShare('CaRBOBout',ActiveDem) * ProductDemand(ActiveDem,'CaRBOBout',t)*1000.0/CFCaGQ(t))/365.0 ;
     ProductDemand(CenDiv,'E85out',t)        = ProductDemand(CenDiv,'E85out',t)*1000.0/CFE85Q(t)/365.0 ;

* Distillate demands using the appraoch taken in subroutine PMM_DIESEL() in NEMS
     ProductDemand(ActiveDem,'N2Hout',t)     = (ProductDemand(ActiveDem,'N2Hout',t) * 1000.0 / CONVFACT_CFDSRS(MNUMYR) / 365.0) -
                                               ULSD_N2H_CD(ActiveDem,t) / 365.0 ;
     ProductDemand(ActiveDem,'N2Hout',t)     = max(ProductDemand(ActiveDem,'N2Hout',t), 0.0001) ;
     ProductDemand(CenDiv,'DSLout',t)        = ProductDemand(CenDiv,'DSLout',t) * 1000.0 / CONVFACT_CFDSLQ(MNUMYR) / 365.0 ;
     ProductDemand(ActiveDem,'DSUout',t)     = (ProductDemand(ActiveDem,'DSUout',t) * 1000.0 / CONVFACT_CFDSUQ(MNUMYR) / 365.0) +
                                               ULSD_N2H_CD(ActiveDem,t) / 365.0 ;
     ProductDemand(CenDiv,'CarbDSUout',t)    = ProductDemand(CenDiv,'CarbDSUout',t) * 1000.0 / CONVFACT_CFDSCQ(MNUMYR) / 365.0 ;

* Calculate pure petroleum production quantities for RFS fractions
* CBTL: '2_M2' is from biomass
* CTL:  do NOT include as NonPet
* GTL:  do NOT include as NonPet

     NonPetDS(t) = (sum(M4$(ord(M4)>2),
                      PMMOUT_BTLFRAC(M4,'10_MNUMPR',MNUMYR) +
                      PMMOUT_CBTLFRAC('2_M2',M4,'10_MNUMPR',MNUMYR) ) +
                   PMMFTAB_UBAVOLDS('10_MNUMPR',MNUMYR) +
                   LFMMOUT_GRD2DSQTY('10_MNUMPR',MNUMYR) +
                   sum(M4, PMMRPT_BIMQTYCD(M4,'11_United_States',MNUMYR)) +
                   (PMMRPT_BIODIMP('11_United_States',MNUMYR) - PMMRPT_BIODEXP('11_United_States',MNUMYR)) ) / 1000 ;

     NonPetMG(t) = (sum(M4$(ord(M4)<3),
                   PMMOUT_BTLFRAC(M4,'10_MNUMPR',MNUMYR) +
                   PMMOUT_CBTLFRAC('2_M2',M4,'10_MNUMPR',MNUMYR) ) +
                   PMMFTAB_UBAVOLMG('10_MNUMPR',MNUMYR) +
                   PMMRPT_ETHTOTCD('11_United_States',MNUMYR) +
                   LFMMOUT_RFBIOBUTECD('11_United_States',MNUMYR) +
                   LFMMOUT_GRN2MGQTY('10_MNUMPR',MNUMYR)) / 1000 ;

     NonPetMG_noETH(t) = (sum(M4$(ord(M4)<3),
                   PMMOUT_BTLFRAC(M4,'10_MNUMPR',MNUMYR) +
                   PMMOUT_CBTLFRAC('2_M2',M4,'10_MNUMPR',MNUMYR) ) +
                   PMMFTAB_UBAVOLMG('10_MNUMPR',MNUMYR) +
                   LFMMOUT_GRN2MGQTY('10_MNUMPR',MNUMYR)) / 1000 ;

* 3/21/13 em4: ETHTOTCD now WITH denaturant
     NonPetMG_ETHonly(t) = PMMRPT_ETHTOTCD('11_United_States',MNUMYR) / 1000 ;

* Set up NGPL demands in M bbl per day

     NGLDemands(ActiveDem,'CC2out',t) =
       sum(MNUMCR$crN2L(ActiveDem,MNUMCR),
         QMORE_QETIN(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFEEQ / 365.0 ;

     NGLDemands(ActiveDem,'LPGout',t) =
       sum(MNUMCR$crN2L(ActiveDem,MNUMCR),
         QMORE_QPRIN(MNUMCR,MNUMYR) -
         QMORE_QPRRF(MNUMCR,MNUMYR) +
         QMORE_QPRRS(MNUMCR,MNUMYR) +
         QMORE_QPRCM(MNUMCR,MNUMYR) +
         QMORE_QPRTR(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFPRQ / 365.0 ;

     NGLDemands(ActiveDem,'NC4out',t) =
       sum(MNUMCR$crN2L(ActiveDem,MNUMCR),
         QMORE_QBUIN(MNUMCR,MNUMYR) - QMORE_QBURF(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFBUQ / 365.0 ;

     NGLDemands(ActiveDem,'IC4out',t) =
       sum(MNUMCR$crN2L(ActiveDem,MNUMCR),
         QMORE_QISIN(MNUMCR,MNUMYR) - QMORE_QISRF(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFIBQ / 365.0 ;

     NGLDemands(ActiveDem,'NATout',t) =
       sum(MNUMCR$crN2L(ActiveDem,MNUMCR),
         QMORE_QPPIN(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFPPQ / 365.0 ;

     NGLDemands(ActiveDem,'UC3out',t) =
       sum(MNUMCR$crN2L(ActiveDem,MNUMCR),
         QMORE_QPROLENERF(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFPRQ / 365.0 ;

     NGLDemands(ActiveDem,NGLProduct,t) = max(0,NGLDemands(ActiveDem,NGLProduct,t)) ;
);

PetDSFrac(t) = 0 ;
PetMGFrac(t) = 0 ;

loop(ModelYears(t)$(ord(t)>LastYear),
* These are only used to calculate the RFS fractions, these demands are enforced through a combined BTU constraint!
     ProductDemand(ActiveDem,'CFGout',t)     = (MGShare('CFGout',ActiveDem)    * ProductDemand(ActiveDem,'CFGout',t)*1000.0/CFMGQCD(ActiveDem,t))/365.0 ;
     ProductDemand(ActiveDem,'RFGout',t)     = (MGShare('RFGout',ActiveDem)    * ProductDemand(ActiveDem,'RFGout',t)*1000.0/CFMGQCD(ActiveDem,t))/365.0 ;
     ProductDemand(ActiveDem,'CaRBOBout',t)  = (MGShare('CaRBOBout',ActiveDem) * ProductDemand(ActiveDem,'CaRBOBout',t)*1000.0/CFMGQCD(ActiveDem,t))/365.0 ;
     ProductDemand(CenDiv,'E85out',t)        = ProductDemand(CenDiv,'E85out',t)*1000.0/CFE85Q(t)/365.0 ;

* Distillate demands using the appraoch taken in subroutine PMM_DIESEL() in NEMS
     ProductDemand(ActiveDem,'N2Hout',t)        = sum(MNUMYR$(ord(MNUMYR)=LastYear), ProductDemand(ActiveDem,'N2Hout',t) * 1000.0 / CONVFACT_CFDSRS(MNUMYR) / 365.0 ) -
                                               ULSD_N2H_CD(ActiveDem,t) / 365.0 ;
     ProductDemand(ActiveDem,'N2Hout',t)     = max(ProductDemand(ActiveDem,'N2Hout',t), 0.0001) ;
     ProductDemand(CenDiv,'DSLout',t)        = sum(MNUMYR$(ord(MNUMYR)=LastYear), ProductDemand(CenDiv,'DSLout',t) * 1000.0 / CONVFACT_CFDSLQ(MNUMYR) / 365.0 ) ;
     ProductDemand(ActiveDem,'DSUout',t)        = sum(MNUMYR$(ord(MNUMYR)=LastYear), ProductDemand(ActiveDem,'DSUout',t) * 1000.0 / CONVFACT_CFDSUQ(MNUMYR) / 365.0 ) +
                                               ULSD_N2H_CD(ActiveDem,t) / 365.0 ;
     ProductDemand(CenDiv,'CarbDSUout',t)    = sum(MNUMYR$(ord(MNUMYR)=LastYear), ProductDemand(CenDiv,'CarbDSUout',t) * 1000.0 / CONVFACT_CFDSCQ(MNUMYR) / 365.0 ) ;

     NonPetDS(t) = sum(tt$(ord(tt)=LastYear), NonPetDS(tt) ) ;
     NonPetMG(t) = sum(tt$(ord(tt)=LastYear), NonPetMG(tt) ) ;


* Set up NGPL demands in M bbl per day

     NGLDemands(ActiveDem,'CC2out',t) =
       sum((tt,MNUMYR,MNUMCR)$(crN2L(ActiveDem,MNUMCR) and ord(tt)=LastYear and tMNUM(tt,MNUMYR)),
         QMORE_QETINPF(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFEEQ / 365.0 ;

     NGLDemands(ActiveDem,'LPGout',t) =
       sum((tt,MNUMYR,MNUMCR)$(crN2L(ActiveDem,MNUMCR) and ord(tt)=LastYear and tMNUM(tt,MNUMYR)),
        (QMORE_QPRIN(MNUMCR,MNUMYR) -
         QMORE_QPRRF(MNUMCR,MNUMYR) +
         QMORE_QPRRS(MNUMCR,MNUMYR) +
         QMORE_QPRCM(MNUMCR,MNUMYR) +
         QMORE_QPRTR(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFPRQ / 365.0 );

     NGLDemands(ActiveDem,'NC4out',t) =
       sum((tt,MNUMYR,MNUMCR)$(crN2L(ActiveDem,MNUMCR) and ord(tt)=LastYear and tMNUM(tt,MNUMYR)),
         QMORE_QBUIN(MNUMCR,MNUMYR) - QMORE_QBURF(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFBUQ / 365.0 ;

     NGLDemands(ActiveDem,'IC4out',t) =
       sum((tt,MNUMYR,MNUMCR)$(crN2L(ActiveDem,MNUMCR) and ord(tt)=LastYear and tMNUM(tt,MNUMYR)),
         QMORE_QISIN(MNUMCR,MNUMYR) - QMORE_QISRF(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFIBQ / 365.0 ;

     NGLDemands(ActiveDem,'NATout',t) =
       sum((tt,MNUMYR,MNUMCR)$(crN2L(ActiveDem,MNUMCR) and ord(tt)=LastYear and tMNUM(tt,MNUMYR)),
         QMORE_QPPIN(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFPPQ / 365.0 ;

     NGLDemands(ActiveDem,'UC3out',t) =
       sum((tt,MNUMYR,MNUMCR)$(crN2L(ActiveDem,MNUMCR) and ord(tt)=LastYear and tMNUM(tt,MNUMYR)),
         QMORE_QPROLENERF(MNUMCR,MNUMYR) ) * 1000.0 / CONVFACT_CFPRQ / 365.0 ;

     NGLDemands(ActiveDem,NGLProduct,t) = max(0,NGLDemands(ActiveDem,NGLProduct,t)) ;
);

Parameter Denom(t)  Denominator to check for division by zero;

Denom(ModelYears(t)) = sum(ActiveDem,ProductDemand(ActiveDem,'DSUout',t)+ProductDemand(ActiveDem,'CarbDSUout',t)) / 1000;

PetDSFrac(ModelYears(t))$(Denom(t)>0) = 1 - (NonPetDS(t)/Denom(t)) ;

Denom(ModelYears(t)) = sum(ActiveDem, ProductDemand(ActiveDem,'CFGout',t) +
                          ProductDemand(ActiveDem,'RFGout',t) +
                          ProductDemand(ActiveDem,'CaRBOBout',t) +
                          ProductDemand(ActiveDem,'E85out',t)) /1000 ;

PetMGFrac(ModelYears(t))$(Denom(t)>0) = 1 - (NonPetMG(t)/Denom(t)) ;

Denom(ModelYears(t)) = sum(ActiveDem, ProductDemand(ActiveDem,'CFGout',t) +
                          ProductDemand(ActiveDem,'RFGout',t) +
                          ProductDemand(ActiveDem,'CaRBOBout',t) +
                          ProductDemand(ActiveDem,'E85out',t)) /1000 -
                          NonPetMG_ETHonly(t) ;

PetMGFrac_noETH(ModelYears(t))$(Denom(t)>0) = 1 - (NonPetMG_noETH(t)/Denom(t)) ;


* Calculate NGL fraction upper bounds by refining region
*=== No longer needed
*===NGLTotal(RefReg,ModelYears(t))$(ord(t)<=LastYear) =
*===  sum((MNUMYR,OGDIST)$(NGPL_2_RefReg(OGDIST,RefReg) and tMNUM(t,MNUMYR)), OGSMOUT_OGNGPLPRD(OGDIST,MNUMYR) ) ;
*===NGLTotal(RefReg,ModelYears(t))$(ord(t)>LastYear)  =
*===  sum(tt$(ord(tt)=LastYear), NGLTotal(RefReg,tt) ) ;
*===
*===NGLBounds(NGLInputStr,RefReg,ModelYears(t))$((NGLTotal(RefReg,t)>0) and (ord(t)<=LastYear)) =
*===  0.01*sum((MNUMYR,OGDIST)$(NGPL_2_RefReg(OGDIST,RefReg) and tMNUM(t,MNUMYR)),
*===*10-23-15 em4 removed NGLFracNGPL from LFMinput.gdx - no longer needed/defined;
*===         NGLFracNGPL(NGLInputStr,OGDIST,RefReg)*OGSMOUT_OGNGPLPRD(OGDIST,MNUMYR)*1000 ) ;
*===NGLBounds(NGLInputStr,RefReg,ModelYears(t))$((NGLTotal(RefReg,t)>0) and (ord(t)>LastYear)) =
*===  sum(tt$(ord(tt)=LastYear), NGLBounds(NGLInputStr,RefReg,tt) ) ;


* aggregate NGLs from OGSM reg to RefReg, convert to 1000 bbl/d, fill for 20 years

* ethane
NGLBounds('CC2ngl',DomRefReg,ModelYears(t))$(ord(t)<=LastYear) =
  sum((MNUMYR,MNUMPR)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and tMNUM(t,MNUMYR)), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'1_M6') ) ;

NGLBounds('CC2ngl',DomRefReg,t)$RunYears(t) =
  sum( (MNUMYR,OGDIST)$(NGPL_2_RefReg(OGDIST,DomRefReg) and tMNUM(t,MNUMYR)),
        OGSMOUT_OGNGPLET(OGDIST,MNUMYR)*1000
  ) ;

NGLBounds('CC2ngl',RefReg,ModelYears(t))$(ord(t) >LastYear) =
  sum( tt$(ord(tt)=LastYear), NGLBounds('CC2ngl',RefReg,tt)
  ) ;

* propane
NGLBounds('CC3ngl',DomRefReg,ModelYears(t))$(ord(t)<=LastYear) =
  sum((MNUMYR,MNUMPR)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and tMNUM(t,MNUMYR)), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'2_M6') ) ;

NGLBounds('CC3ngl',DomRefReg,t)$RunYears(t) =
  sum( (MNUMYR,OGDIST)$(NGPL_2_RefReg(OGDIST,DomRefReg) and tMNUM(t,MNUMYR)),
        OGSMOUT_OGNGPLPR(OGDIST,MNUMYR)*1000
     ) ;

NGLBounds('CC3ngl',RefReg,ModelYears(t))$(ord(t) >LastYear) =
  sum( tt$(ord(tt)=LastYear), NGLBounds('CC3ngl',RefReg,tt)
     ) ;

* iso-butane
NGLBounds('IC4ngl',DomRefReg,ModelYears(t))$(ord(t)<=LastYear) =
  sum((MNUMYR,MNUMPR)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and tMNUM(t,MNUMYR)), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'4_M6') ) ;

NGLBounds('IC4ngl',DomRefReg,t)$RunYears(t) =
  sum( (MNUMYR,OGDIST)$(NGPL_2_RefReg(OGDIST,DomRefReg) and tMNUM(t,MNUMYR)),
        OGSMOUT_OGNGPLIS(OGDIST,MNUMYR)*1000
     ) ;

NGLBounds('IC4ngl',RefReg,ModelYears(t))$(ord(t) >LastYear) =
  sum( tt$(ord(tt)=LastYear), NGLBounds('IC4ngl',RefReg,tt)
     ) ;

* n-butane
NGLBounds('NC4ngl',DomRefReg,ModelYears(t))$(ord(t)<=LastYear) =
  sum((MNUMYR,MNUMPR)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and tMNUM(t,MNUMYR)), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'3_M6') ) ;

NGLBounds('NC4ngl',DomRefReg,t)$RunYears(t) =
  sum( (MNUMYR,OGDIST)$(NGPL_2_RefReg(OGDIST,DomRefReg) and tMNUM(t,MNUMYR)),
        OGSMOUT_OGNGPLBU(OGDIST,MNUMYR)*1000
     ) ;

NGLBounds('NC4ngl',RefReg,ModelYears(t))$(ord(t) >LastYear) =
  sum( tt$(ord(tt)=LastYear), NGLBounds('NC4ngl',RefReg,tt)
     ) ;

* pentanes plus
NGLBounds('NATngl',DomRefReg,ModelYears(t))$(ord(t)<=LastYear) =
  sum((MNUMYR,MNUMPR)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and tMNUM(t,MNUMYR)), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'5_M6') ) ;

NGLBounds('NATngl',DomRefReg,t)$RunYears(t) =
  sum( (MNUMYR,OGDIST)$(NGPL_2_RefReg(OGDIST,DomRefReg) and tMNUM(t,MNUMYR)),
        OGSMOUT_OGNGPLPP(OGDIST,MNUMYR)*1000
     ) ;

NGLBounds('NATngl',RefReg,ModelYears(t))$(ord(t) >LastYear) =
  sum( tt$(ord(tt)=LastYear), NGLBounds('NATngl',RefReg,tt)
     ) ;



* Pull expected/current year world oil prices in 87$/bbl into new variable
ExpectedWOP(ModelYears(t))$(ord(t)<=LastYear) =
  sum(MNUMYR$tMNUM(t,MNUMYR), INTOUT_START_PRICE(MNUMYR) );
ExpectedWOP(ModelYears(t))$(ord(t)>LastYear) =
  sum(tt$(ord(tt)=LastYear), ExpectedWOP(tt) ) ;

* Transfer RFS Credit Price from NEMS Variable to LFMM Parameter
RFSCreditPrice(RFSCategory,ModelYears(t))$(ord(t)<=LastYear) =
  sum(MNUMYR$tMNUM(t,MNUMYR), sum(M4$M4_2_RFSCat(M4,RFSCategory), LFMMOUT_RFSCREDPRC(M4,MNUMYR)));
RFSCreditPrice(RFSCategory,ModelYears(t))$(ord(t)>LastYear) =
  sum(tt$(ord(tt)=LastYear), RFSCreditPrice(RFSCategory,tt) ) ;

* Transfer RFS Credit Price from NEMS Variable to LFMM Parameter
RFSWaivers(RFSCategory,ModelYears(t))$(ord(t)<=LastYear) =
  sum(MNUMYR$tMNUM(t,MNUMYR), sum(M4$M4_2_RFSCat(M4,RFSCategory), LFMMOUT_RFSSAFETY(M4,MNUMYR)));
RFSWaivers(RFSCategory,ModelYears(t))$(ord(t)>LastYear) =
  sum(tt$(ord(tt)=LastYear), RFSWaivers(RFSCategory,tt) ) ;

*Convert NEMS data for Caribbean and Maritime Canada demand curves to LFMM format
Loop((MNPROD2Str(MNPROD,Stream),tMNXYRS(ModelYears(t),MNXYRS),StepMC(Step,MCSTEP)),
  MCCPrice(Stream,Step,t)  = INTOUT_P_C_MC_Demand(MCSTEP,MNXYRS,MNPROD);
  MCCSupply(Stream,Step,t) = INTOUT_Q_C_MC_Demand(MCSTEP,MNXYRS,MNPROD);
);

* LGPPrice, 87$/MMBtu = LPGAlpha(LPGSector)*ExpectedWOP(t) + LPGBeta(LPGSector)/GDP('2009') ;
* Since NGLImportcost is already lower than LPGprice, do not change NGLImportcost algorithm YET, 9-15-12, em4, hardcoded
* NGLImportCost, 87$/bbl

NGLImportCost(CenDiv,'CC2out',ModelYears(t)) = 10 ;
NGLImportCost(CenDiv,'LPGout',ModelYears(t)) = CoprodAlpha('LPGout')*ExpectedWOP(t) + CoprodBeta('LPGout') ;
NGLImportCost(CenDiv,'UC3out',ModelYears(t)) = CoprodAlpha('UC3out')*ExpectedWOP(t) + CoprodBeta('UC3out') ;
NGLImportCost(CenDiv,'NC4out',ModelYears(t)) = CoprodAlpha('NC4out')*ExpectedWOP(t) + CoprodBeta('NC4out') ;
NGLImportCost(CenDiv,'IC4out',ModelYears(t)) = CoprodAlpha('IC4out')*ExpectedWOP(t) + CoprodBeta('IC4out') ;
NGLImportCost(CenDiv,'NATout',ModelYears(t)) = CoprodAlpha('NATngl')*ExpectedWOP(t) + CoprodBeta('NATngl') ;

* 10-8-13 em4, temp, set NGLExportCost to 60% of NGLImportCost (was ZERO before)

NGLExportCost(CenDiv,'CC2out',ModelYears(t)) = NGLImportCost(CenDiv,'CC2out',t) * 0.60 ;
NGLExportCost(CenDiv,'LPGout',ModelYears(t)) = NGLImportCost(CenDiv,'LPGout',t) * 0.60 ;
NGLExportCost(CenDiv,'UC3out',ModelYears(t)) = NGLImportCost(CenDiv,'UC3out',t) * 0.60 ;
NGLExportCost(CenDiv,'NC4out',ModelYears(t)) = NGLImportCost(CenDiv,'NC4out',t) * 0.60 ;
NGLExportCost(CenDiv,'IC4out',ModelYears(t)) = NGLImportCost(CenDiv,'IC4out',t) * 0.60 ;
NGLExportCost(CenDiv,'NATout',ModelYears(t)) = NGLImportCost(CenDiv,'NATout',t) * 0.60 ;


* temp for hrexport side case test
loop((CenDiv,NGLProduct,t),
  if(NGLImportCost(CenDiv,NGLProduct,t)<0,
     NGLImportCost(CenDiv,NGLProduct,t) = 0.01;);
  if(NGLExportCost(CenDiv,NGLProduct,t)<0,
     NGLExportCost(CenDiv,NGLProduct,t) = 0.01 * 0.60 ;);
);

* Convert Biomass supply curves and other data

* Sum up demand from other NEMS modules to offset Biomass supply

Loop(Bio2MNUMFS(BioStr,MNUMFS),

  Loop(CoaN2L(CoalDReg,NDREGN),

    BioOthDemand(CoalDReg,BioStr,Other_MKT,t)$WRENEW_AVL_SUP(MNUMFS,Other_MKT)
           = sum(MNMYRF$tMNMYRF(t,MNMYRF), WRENEW_QBM_CL(MNUMFS,NDREGN,MNMYRF,Other_MKT)) * 1000.0/365 ;

    BiomassSup(CoalDReg,BioStr,Step,t)
           = sum((WDCRVS,MNMYRF)$(tMNMYRF(t,MNMYRF) and StepMDCRV(Step,WDCRVS)), WDSUP_Inc(NDREGN,MNUMFS,WDCRVS,MNMYRF))*1000.0/365.0;
* Set up prices on biomass supply steps in 87$/mmbtu
  BiomassPrc(CoalDreg,BioStr,Step,t)
         = sum((WDCRVS,MNMYRF)$(tMNMYRF(t,MNMYRF) and StepMDCRV(Step,WDCRVS)), WRENEW_WDSUP_P(NDREGN,MNUMFS,WDCRVS,MNMYRF) );
  );
);

* Convert NEMS tables to LFMM tables for Coal Supply  XCL - expectations - coal

Parameters
XCL_Steps(Step)
XCL_Step_Total
XCL_QECP_MP(CoalSReg,t)
XCL_QECP(CoalSReg,Step,t)                      Coal Supply per step Billion Btu per Calendar Day
XCL_PECP(CoalSReg,Step,t)                      Coal Price per step 87$ per Million Btu
XCL_PCAP(CoalSReg,t)
XCL_MX_PCAP(CoalSReg)
XCL_Other(CoalSReg,t)
XCL_PrdLim
XCL_Type(CoalSReg)                             Coal type by coal supply region
XCL_QTOT_ADJ(CoalSReg,t)

XCL_Trate(CoalSReg,CoalDReg,t)                 Cost to transport coal from coal supply to coal demand region 87$ per Million Btu

XCL_NUM_SO2_GRP
XCL_SO2_SHR_BY_CLRG(CoalDReg,MX_SO2)
XCL_SO2_RMV(CoalDReg,t)                        Percent Removal of SO2 at integrated gas combined cycle
XCL_SO2(CoalSReg)                              SO2 content by coal supply region
XCL_PSO2(MX_SO2,t)                             SO2 Emission Price  87$ per Thousandths of Tons SO2
XCL_SO2_Emission(CoalSReg,CoalDReg,MX_SO2,t)   SO2 emission rate Thousandths of Tons SO2 per Million Btu

XCL_HG(CoalSReg)                               Mercury content by coal supply region
XCL_PHG(CoalDReg,t)                            Mercury Emission Price  87$ per Thousandths of Tons HG
XCL_HG_Emission(CoalSReg,CoalDReg,t)           Mercury emission rate Thousandths of Tons HG per Million Btu

XCL_EFD_RANK(CoalSReg)
XCL_PLNT_EMF_IGCC(MX_RNK)
XCL_JCLIGNR(t)
;

Sets
XCL_CDSL1(CoalDReg,RefReg)                     Coal demand region to Refinery region mapping
XCL_CLDR(CoalDReg)
;

XCL_CDSL1(CoalDReg,RefReg)                     = sum(NDREGN,USO2GRP_CTL_CDSL1(NDREGN,REFREG)$CoaN2L(CoalDReg,NDREGN));
XCL_CLDR(CoalDReg)                             = sum(NDREGN,USO2GRP_CTL_CLDR(NDREGN)$CoaN2L(CoalDReg,NDREGN));
XCL_Type(CoalSReg)                             = sum(MX_NCL,USO2GRP_CTL_TYPE(MX_NCL)$CoaSN2L(CoalSReg,MX_NCL));

Loop(CoaSN3L(CoalSReg,MX_NCI)$XCL_Type(CoalSReg),
  XCL_EFD_RANK(CoalSReg)      = USO2GRP_EFD_RANK(MX_NCI);
);

Loop((CoaSN2L(CoalSReg,MX_NCL),CoaN2L(CoalDReg,NDREGN),tMNUM(ModelYears(t),MNUMYR))$(XCL_CLDR(CoalDreg) and XCL_Type(CoalSReg)),
  XCL_Trate(CoalSReg,CoalDreg,t) = USO2GRP_CTL_TRATE(MX_NCL,NDREGN) * USO2GRP_XCL_1TESC(MX_NCL,'01_ECPFPH',MNUMYR,NDREGN)$(USO2GRP_CTL_TRATE(MX_NCL,NDREGN) < 900.0 );
);

XCL_PLNT_EMF_IGCC(MX_RNK) =
  COALEMM_PLNT_EMF('35_ECPCAP',MX_RNK);

** SO2 Emissions data
XCL_SO2(CoalSReg)$XCL_Type(CoalSReg) =
  sum(MX_NCI,USO2GRP_XCL_SO2(MX_NCI)$CoaSN3L(CoalSReg,MX_NCI));

XCL_NUM_SO2_GRP =
  EMISSION_NUM_SO2_GRP;

XCL_SO2_SHR_BY_CLRG(CoalDReg,MX_SO2)$XCL_CLDR(CoalDReg) =
  sum(NDREGN,EMISSION_SO2_SHR_BY_CLRG(NDREGN,MX_SO2)$CoaN2L(CoalDReg,NDREGN));

XCL_SO2_RMV(CoalDReg,ModelYears(t))$XCL_CLDR(CoalDReg) =
  sum((NDREGN,MNUMYR),COALEMM_RCLIGNR(NDREGN,MNUMYR)$(CoaN2L(CoalDReg,NDREGN) and tMNUM(t,MNUMYR)));
*Use last year's value in XCL_SO2_RMV for extended years
XCL_SO2_RMV(CoalDReg,ModelYears(t))$(XCL_CLDR(CoalDReg)and ord(t)>(AEOLSTYR - 1989)) =
  sum(tt$(ord(tt) = (AEOLSTYR - 1989)), XCL_SO2_RMV(CoalDReg,tt) );

XCL_PSO2(MX_SO2,ModelYears(t)) =
  sum((NDREGN,MNUMYR)$tMNUM(t,MNUMYR), EMISSION_EMELPSO2(MNUMYR,MX_SO2) );
*Use last year's value in XCL_PSO2 for extended years
XCL_PSO2(MX_SO2,t)$(ord(t)>(AEOLSTYR - 1989)) =
  sum(tt$(ord(tt) = (AEOLSTYR - 1989)), XCL_PSO2(MX_SO2,tt) );

XCL_SO2_Emission(CoalSReg,CoalDReg,MX_SO2,ModelYears(t))$XCL_Trate(CoalSReg,CoalDReg,t) =
  XCL_SO2(CoalSReg) * (1.0 - XCL_SO2_RMV(CoalDReg,t)) * XCL_SO2_SHR_BY_CLRG(CoalDReg,MX_SO2) * 0.5;


** Mercury Emissions data
XCL_HG(CoalSReg)$XCL_Type(CoalSReg) =
  sum(MX_NCI,USO2GRP_XCL_HG(MX_NCI)$CoaSN3L(CoalSReg,MX_NCI));

XCL_PHG(CoalDReg,ModelYears(t))$XCL_CLDR(CoalDReg) =
  sum((MNUMYR,NDREGN),EMISSION_EMEL_PHG(NDREGN,MNUMYR)$(CoaN2L(CoalDReg,NDREGN) and tMNUM(t,MNUMYR)));
*Use last year's value in XCL_PHG for extended years
XCL_PHG(CoalDReg,ModelYears(t))$(XCL_CLDR(CoalDReg) and ord(t)>(AEOLSTYR - 1989)) =
  sum(tt$(ord(tt) = (AEOLSTYR - 1989)), XCL_PHG(CoalDReg,t) );

XCL_HG_Emission(CoalSReg,CoalDReg,ModelYears(t))$XCL_Trate(CoalSReg,CoalDReg,t) =
  sum(MX_RNK$(ord(MX_RNK) = XCL_EFD_RANK(CoalSReg)), XCL_HG(CoalSReg) * XCL_PLNT_EMF_IGCC(MX_RNK) * 0.5);


XCL_JCLIGNR(ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR), EMABLK_JCLIGNR(MNUMYR) ) ;
*Use last year's value in XCL_JCLIGNR for extended years
XCL_JCLIGNR(ModelYears(t))$(ord(t)>(AEOLSTYR - 1989)) =
  sum(tt$(ord(tt) = (AEOLSTYR - 1989)), XCL_JCLIGNR(tt) ) ;

XCL_Steps(Step) =
  sum(M11$(ord(M11) > 1),USO2GRP_XCL_STEPS(M11)$StepM11(Step,M11) - USO2GRP_XCL_STEPS(M11-1)$StepM11(Step,M11)) ;
XCL_Steps('STEP01') =
  1.0 + USO2GRP_XCL_STEPS('01_M11') ;
XCL_Step_Total =
  sum(Step,XCL_Steps(Step));


Loop(CoaSN2L(CoalSReg,MX_NCL)$XCL_Type(CoalSReg),
  XCL_MX_PCAP(CoalSReg)         = USO2GRP_XCL_MX_PCAP(MX_NCL);
  Loop(tMNUM(ModelYears(t),MNUMYR),
    XCL_QECP_MP(CoalSReg,t)     = USO2GRP_XCL_QECP(MX_NCL,'01_ECPFPH',MNUMYR);
    XCL_QECP(CoalSReg,Step,t)   = XCL_Steps(Step) * USO2GRP_XCL_QECP(MX_NCL,'01_ECPFPH',MNUMYR);
  );
  Loop(tMNUM(ModelYears(t),MNUMYR),
    XCL_PCAP(CoalSReg,t)        = USO2GRP_XCL_PCAP(MX_NCL,MNUMYR);
    XCL_Other(CoalSReg,t)       = USO2GRP_CTL_Other(MX_NCL,MNUMYR);
  );
);

*Use last year's value in XCL_PCAP for extended years
XCL_PCAP(CoalSReg,ModelYears(t))$(XCL_Type(CoalSReg) and ord(t)>(AEOLSTYR - 1989)) =
  sum(tt$(ord(tt) = (AEOLSTYR - 1989)),XCL_PCAP(CoalSReg,tt))* (1.0 + XCL_MX_PCAP(CoalSReg)) ** (ord(t) - (AEOLSTYR - 1989));

*Use last year's value in XCL_Other for extended years
XCL_Other(CoalSReg,ModelYears(t))$(XCL_Type(CoalSReg) and ord(t)>(AEOLSTYR - 1989)) =
  sum(tt$(ord(tt) = (AEOLSTYR - 1989)),XCL_Other(CoalSReg,tt));

Loop (ModelYears(t)$(ord(t) >= (NCNTRL_CURCALYR - 1989)),
   Loop ((ECPFPH,tt)$((ord(tt) = NCNTRL_CURCALYR - 1989) and (ord(ECPFPH) = (ord(t) - NCNTRL_CURCALYR + 1990))),
      XCL_QECP_MP(CoalSReg,t)$XCL_Type(CoalSReg) = sum((MX_NCL,MNUMYR),USO2GRP_XCL_QECP(MX_NCL,ECPFPH,MNUMYR)
                                                   $(CoaSN2L(CoalSReg,MX_NCL) and ttNUMyr(tt,MNUMYR))) ;
   );
   XCL_QTOT_ADJ(CoalSReg,t)$XCL_Type(CoalSReg) = 1.0;
   XCL_QTOT_ADJ(CoalSReg,t)$(XCL_QECP_MP(CoalSReg,t) > 0.0 and XCL_Type(CoalSReg)) =
      MAX(1.0 , (XCL_Other(CoalSReg,t) * 1.0001) / XCL_QECP_MP(CoalSReg,t));
);

Loop (ModelYears(t)$(ord(t) >= (NCNTRL_CURCALYR - 1989)),
   Loop ((ECPFPH,tt)$((ord(tt) = NCNTRL_CURCALYR - 1989) and (ord(ECPFPH) = (ord(t) - NCNTRL_CURCALYR + 1990))),
      XCL_QECP(CoalSReg,Step,t)$XCL_Type(CoalSReg)
             = XCL_Steps(Step) * XCL_QTOT_ADJ(CoalSReg,t) * sum((MX_NCL,MNUMYR),
               USO2GRP_XCL_QECP(MX_NCL,ECPFPH,MNUMYR)$(CoaSN2L(CoalSReg,MX_NCL) and ttNUMyr(tt,MNUMYR))) ;

      XCL_PECP(CoalSReg,Step,t)$XCL_Type(CoalSReg)
             = sum((MX_NCL,M11,MNUMYR),USO2GRP_XCL_PECP(MX_NCL,M11,ECPFPH,MNUMYR)$(CoaSN2L(CoalSReg,MX_NCL) and StepM11(Step,M11) and ttNUMyr(tt,MNUMYR)));

      XCL_Trate(CoalSReg,CoalDReg,t)$(XCL_CLDR(CoalDreg) and XCL_Type(CoalSReg))
             = sum((MX_NCL,NDREGN,MNUMYR)$(USO2GRP_CTL_TRATE(MX_NCL,NDREGN) < 900.0),USO2GRP_CTL_TRATE(MX_NCL,NDREGN) * USO2GRP_XCL_1TESC(MX_NCL,ECPFPH,MNUMYR,NDREGN)
               $(CoaSN2L(CoalSReg,MX_NCL) and CoaN2L(CoalDReg,NDREGN) and ttNUMyr(tt,MNUMYR)));
   );
);

Loop (ModelYears(t)$(ord(t) >= (NCNTRL_CURCALYR - 1989)),
   Loop (CoalSReg$XCL_Type(CoalSReg),
      XCL_PrdLim = Max(XCL_PCAP(CoalSReg,t) , XCL_Other(CoalSReg,t) * 1.0001);
      Loop (Step,
         XCL_QECP(CoalSReg,Step,t) = MIN(XCL_PrdLim , XCL_QECP(CoalSReg,Step,t));
         XCL_PrdLim                = XCL_PrdLim - XCL_QECP(CoalSReg,Step,t);
      );
   );
);

** Tables used in LFMM formulation
CoalPrc(CoalSReg,CoalStr,Step,ModelYears(t))                   = XCL_PECP(CoalSReg,Step,t);
SO2Prc(CoalStr,MX_SO2,ModelYears(t))                           = XCL_PSO2(MX_SO2,t);
HGPrc(CoalDReg,CoalStr,ModelYears(t))                          = XCL_PHG(CoalDReg,t);
CoalTrnspCst(CoalSReg,CoalDReg,CoalStr,ModelYears(t))          = XCL_Trate(CoalSReg,CoalDReg,t);
CoalSup(CoalSReg,CoalStr,step,ModelYears(t))                   = XCL_QECP(CoalSReg,Step,t)  * 1000.0 / 365.0;
CoalOthDemand(CoalSreg,CoalStr,ModelYears(t))                  = XCL_Other(CoalSreg,t) * 1000.0 / 365.0;
SO2Emission(CoalSReg,CoalDReg,CoalStr,MX_SO2,ModelYears(t))    = XCL_SO2_Emission(CoalSReg,CoalDReg,MX_SO2,t);
HGEmission(CoalSReg,CoalDReg,CoalStr,ModelYears(t))            = XCL_HG_Emission(CoalSReg,CoalDReg,t);

*----Convert NEMS tables to LFMM tables for product imports
Sets
MapCrude(Crude,MNCRUD)               Map Crude (LFMM) to MNCRUD (NEMS)                    /set.Crude:set.MNCRUD/
MapINStep(Step,INSTEP)               Map Step  (LFMM) to INSTEP (NEMS)                   /set.Step:set.INSTEP/
;

Loop((tMNXYRS(ModelYears(t),MNXYRS),MNPROD2Str(MNPROD,Stream),MapINStep(Step,INSTEP)),
  ImportPrice(Stream,INTREG,Step,t)  = INTOUT_Product_Import_P(MNPROD,INTREG,INSTEP,MNXYRS) ;
  ImportSupply(Stream,INTREG,Step,t) = INTOUT_Product_Import_Q(MNPROD,INTREG,INSTEP,MNXYRS) ;

  ExportPrice(Stream,INTREG,Step,t)  = INTOUT_Product_Export_P(MNPROD,INTREG,INSTEP,MNXYRS) ;
  ExportSupply(Stream,INTREG,Step,t) = INTOUT_Product_Export_Q(MNPROD,INTREG,INSTEP,MNXYRS) ;
);

* Use regression equation to set price for ethanol import and export curves
Alias(Step,StepA) ;

* Set quantities on import supply steps to allow a total of 110% of base import levels
* TEMP: BrzAdvQ0 now contains data for US ethanol imports only (1000 bbl/d)

EthImpSupQty('ETHAETbrz','STEP01',ModelYears(t))           = BrzAdvQ0(t) ;
EthImpSupQty('ETHAETbrz',Step,ModelYears(t))$((ord(step)>1) and (ord(step)<11)) = 0.1*BrzAdvQ0(t)/9 ;


* Regression price --- Only go for 10 steps
* TEMP: BrzAdvP0 now contains data for US ethanol imports only (2018 $/bbl) ---
*       Convert to 87$/bbl below so that npv_ can be defined properly
* TEMP: BrzAdvEpsilon now contains data for US ethanol imports only

EthImpSupPrc('ETHAETbrz',Step,ModelYears(t)) = BrzAdvP0(t) *
  ((sum(StepA$(ord(StepA)<=ord(Step)),EthImpSupQty('ETHAETbrz',StepA,t))/BrzAdvQ0(t))**(1/BrzAdvEpsilon(t)))
  / GDP('2018') ;


* Set quantities on export demand steps to allow a total of 150% of base export levels
* TEMP: NonUSEthQ0 now contains data for US ethanol exports only (1000 bbl/d)

EthExpDmdQty('ETHCRNexp','STEP01',ModelYears(t))           = NonUSEthQ0(t) ;
EthExpDmdQty('ETHCRNexp',Step,ModelYears(t))$((ord(step)>1) and (ord(step)<11)) = 0.5*NonUSEthQ0(t)/9 ;


* Regression price --- Only go for 10 steps
* TEMP: NonUSEthP0 now contains data for US ethanol exports only (2018 $/bbl) ---
*       Convert to 87$/bbl below so that npv_ can be defined properly
* TEMP: NonUSEthEpsilon now contains data for US ethanol exports only

EthExpDmdPrc('ETHCRNexp',Step,ModelYears(t)) = NonUSEthP0(t) *
  ((sum(StepA$(ord(StepA)<=ord(Step)),EthExpDmdQty('ETHCRNexp',StepA,t))/NonUSEthQ0(t))**(1/NonUSEthEpsilon(t)))
  / GDP('2018') ;



*-- Biodiesel import curves --*
Parameters

     FBDImpQuant(RefReg,t)
     FBDCumQuant(Step,t)                                             holds cum biodiesel import supply at step 1000 bbl per day
     FBDMiddlePrice(t)                                               placeholder for Po biodiesel import price 1987$ per bbl
     FBDMiddleQuant(t)                                               placeholder for Qo bio diesel import supply 1000 bbl per day
     FBDImpSupply(Step,t)                                            biodiesel import supply step quantities 1000 bbl per day
     FBDImpPrice(Step,t)                                             biodiesel import supply step prices 1987 $ per bbl
     FBDMaxQty(t)

*   3/12/20 - esh
*   INTtoREFFBDTranCost:  1987$/bbl
Table INTtoREFFBDTranCost(INTREG,RefReg)
                   1_RefReg     2_RefReg     3_RefReg     4_RefReg     5_RefReg     6_RefReg     7_RefReg      8_RefReg
      1_INTREG       0.01         0.01         0.01         0.01         0.01         0.01         0.01          0.01

;

INTtoREFFBDTranCost('1_INTREG','7_RefReg') =  sum((MNUMYR,RunYears)$tMNUM(RunYears,MNUMYR), 0.5*MPBLK_PDSTR('11_United_States',MNUMYR)*CONVFACT_CFDSUQ(MNUMYR) ) ;
INTtoREFFBDTranCost('1_INTREG','2_RefReg') = -sum((MNUMYR,RunYears)$tMNUM(RunYears,MNUMYR), 0.02*MPBLK_PDSTR('11_United_States',MNUMYR)*CONVFACT_CFDSUQ(MNUMYR) ) ;
INTtoREFFBDTranCost('1_INTREG','3_RefReg') = -sum((MNUMYR,RunYears)$tMNUM(RunYears,MNUMYR), 0.02*MPBLK_PDSTR('11_United_States',MNUMYR)*CONVFACT_CFDSUQ(MNUMYR) ) ;

*FBDMiddlePrice(t)                                            placeholder for Po renewable diesel import price 1987$ per bbl
*  FBDMiddleQuant(t)                                            placeholder for Qo renewable or bio diesel import supply 1000 bbl per day
*  FBDCumQuant(Step,t)
*  MaxFlag(t)

;

FBDMiddlePrice(t) = 1.25*sum( MNUMYR$tMNUM(t,MNUMYR),
                               1.20*MPBLK_PDSTR('11_United_States',MNUMYR)*CONVFACT_CFDSUQ(MNUMYR) );
FBDCumQuant(Step,t) = 0.0 ;

*MaxFlag(t)            = 0.0 ;
*FBDImpSplit(RefReg,t) = 0.0 ;
*MiddleQuant(t)        = 0.0 ;
*CumQuant(Step,t)      = 0.0 ;

loop((ModelYears(t),MNUMYR)$tMNUM(t,MNUMYR),

  FBDMaxQty(t) = 0.75*sum(DomRefReg, FBDImpQuant(DomRefReg,t) ) ;

FBDMiddleQuant(t) = FBDMaxQty(t) ;

    FBDCumQuant('STEP01',t) = 0.750* FBDMiddleQuant(t) ;
    FBDCumQuant('STEP02',t) = 0.925* FBDMiddleQuant(t) ;
    FBDCumQuant('STEP03',t) = 1.075* FBDMiddleQuant(t) ;
    FBDCumQuant('STEP04',t) = 1.250* FBDMiddleQuant(t) ;
    FBDCumQuant('STEP05',t) = 1.750* FBDMiddleQuant(t) ;
*  if(PMMRPT_TDIESEL('11_United_States',MNUMYR)=0.0,
*   FBDMaxQty(t) = FBDMaxQty(t) ;
*  else
*     FBDMaxQty(t) = FBDMaxQty(t) * (PMMRPT_TDIESEL('11_United_States',MNUMYR)/PMMRPT_TDIESEL('11_United_States',MNUMYR-1)) ;
*  );

*  if(PMMRPT_BIODIMP('11_United_States',MNUMYR) <= 0.0,
*     MiddleQuant(t) = 0.25 * FBDMaxQty(t) ;
*  elseif(PMMRPT_BIODIMP('11_United_States',MNUMYR) >= 0.95*FBDMaxQty(t)),
*     MiddleQuant(t) = 0.95 * FBDMaxQty(t) ;
*     MaxFlag(t) = 1 ;
*  else
*     MiddleQuant(t) = PMMRPT_BIODIMP('11_United_States',MNUMYR) ;
*  );

*  if(MaxFlag(t)=1,
*     CumQuant('STEP03',t) = 0.075*(FBDMaxQty(t)-MiddleQuant(t)) + MiddleQuant(t) ;
*     CumQuant('STEP04',t) = 0.25*(FBDMaxQty(t)-MiddleQuant(t)) + MiddleQuant(t) ;
*  else
*     CumQuant('STEP03',t) = 0.075*MiddleQuant(t) + MiddleQuant(t) ;
*     CumQuant('STEP04',t) = 0.25*MiddleQuant(t) + MiddleQuant(t) ;
*  );
*  CumQuant('STEP02',t) = MiddleQuant(t) - 0.075*MiddleQuant(t) ;
*  CumQuant('STEP01',t) = MiddleQuant(t) - 0.25*MiddleQuant(t) ;

* 9-14-18 em4 * adjust biodiesel import quantity downward (60%) to better match current levels
*  CumQuant(Step,t) = 0.60*CumQuant(Step,t) ;

*  FBDImpPrice('STEP01',t) = ((FBDImpCoefB(t)*FBDImpCoefM(t)**(0.991*CumQuant('STEP01',t)))-6.855) * 0.991 + 6.448 + 12.518 ;
*  FBDImpPrice('STEP02',t) = ((FBDImpCoefB(t)*FBDImpCoefM(t)**(0.991*CumQuant('STEP02',t)))-6.855) * 0.991 + 6.448 + 12.518 ;
*  FBDImpPrice('STEP03',t) = ((FBDImpCoefB(t)*FBDImpCoefM(t)**(0.991*MiddleQuant(t)))-6.855) * 0.991 + 6.448 + 12.518 ;
*  FBDImpPrice('STEP04',t) = ((FBDImpCoefB(t)*FBDImpCoefM(t)**(0.991*CumQuant('STEP03',t)))-6.855) * 0.991 + 6.448 + 12.518 ;
*  FBDImpPrice('STEP05',t) = ((FBDImpCoefB(t)*FBDImpCoefM(t)**(0.991*CumQuant('STEP04',t)))-6.855) * 0.991 + 6.448 + 12.518 ;

  Loop( Step$(ord(Step) <= 5),
    FBDImpPrice(Step,t) = FBDMiddlePrice(t) *
                                   ( (FBDCumQuant(Step,t) / FBDMiddleQuant(t))**(1./FBDImpElas) ) ;
  );

  FBDImpSupply('STEP01',t) = FBDCumQuant('STEP01',t) ;
  FBDImpSupply('STEP02',t) = FBDCumQuant('STEP02',t) - FBDCumQuant('STEP01',t) ;
  FBDImpSupply('STEP03',t) = FBDCumQuant('STEP03',t) - FBDCumQuant('STEP02',t) ;
  FBDImpSupply('STEP04',t) = FBDCumQuant('STEP04',t) - FBDCumQuant('STEP03',t) ;
  FBDImpSupply('STEP05',t) = FBDCumQuant('STEP05',t) - FBDCumQuant('STEP04',t) ;
*  FBDImpSupply('STEP05',t) = max(0.0,0.6*FBDMaxQty(t)-CumQuant('STEP04',t))  ;
* 9-14-18 em4 * adjust biodiesel import FBDMaxQty downward (60%) to better match current levels

);

* extend beyond 2050
loop( t$(ord(t)>LastYear),
  FBDImpSupply(Step,t) = FBDImpSupply(Step,'2050');
  FBDImpPrice(Step,t)  = FBDImpPrice(Step,'2050');
*  FBDImpSplit(RefReg,t)= FBDImpSplit(RefReg,'2050') ;
);


* =========================================================
*
*   (02-10-2020, em4)
*   RENEWABLE DIESEL Import Data and placeholders
*   RDHImpQuant:  1000 bbl/cd (hardcoded; renewable diesel, hist/max2050, like FBDImpQuant)
*   RDHCumQuant:  1000 bbl per day (renewable diesel cum quant per step)
*   RDHMiddlePrice:  1987$/bbl
*   RDHMiddleQuant:  1000 bbl/cd
*   RDHImpPrice:  1987 $/bbl
*   RDHImpSupply: 1000 bbl/cd
*   LFMMOUT_RenewDImp:    1000 bbl/cd (renewable diesel, soln, like BIODIMP)
*   LFMMOUT_RenewDImpPD:  1000 bbl/cd (renewable diesel, soln, like BIODIMPPD)
* =========================================================

*-- Renewable Diesel import curves --*
Parameters
     RDHImpQuant(RefReg,t)                                           historical renewable diesel import supply by reg 1000 bbl per day
     RDHCumQuant(Step,t)                                             holds cum renewable diesel import supply at step 1000 bbl per day
     RDHMiddlePrice(t)                                               placeholder for Po renewable diesel import price 1987$ per bbl
     RDHMiddleQuant(t)                                               placeholder for Qo renewable or bio diesel import supply 1000 bbl per day
     RDHImpSupply(Step,t)                                            Renewable Diesel import supply step quantities 1000 bbl per day
     RDHImpPrice(Step,t)                                             Renewable Diesel import supply step prices 1987 $ per bbl
     RDHMaxQty(t)                                                    Total renewable diesel U.S. import quantity in M bbl per day
;

* hardcoded RDHImpQuant for historical years- until move to lfimportpurch.xlsx newTAB:RDHImpQuant
* 2019 estimated from Dec2018-Nov2019
* set 2010, 2011 = 2012 so no zero totals exist
* 1000 bbl/cd
* 2022

Table RDHImpQuant(RefReg,t)
                   2010     2011     2012     2013     2014     2015     2016     2017     2018     2019     2020     2021     2022
      1_RefReg     1.00     1.00     1.00     1.50     0.00     1.00     1.00     0.00     0.00     0.00     0.00     0.00     0.00
      4_RefReg     0.00     0.00     0.00     2.50     1.00     1.00     0.00     0.00     0.00     0.00     0.00     0.00     0.00
      7_RefReg     1.00     1.00     1.00     9.00     7.00    11.00    13.00    12.00    11.00    15.00    17.66    26.00    36.34
;

RDHImpQuant('8_RefReg',t) = 0.25*RDHImpQuant('7_RefReg',t) ;
RDHImpQuant('7_RefReg',t)= 0.75*RDHImpQuant('7_RefReg',t) ;

* hardcoded grow at 1.25% per year over projection until move to lfimportpurch.xlsx
loop( t$(t.val > 2022),
  RDHImpQuant(RefReg,t) = 1.0125* RDHImpQuant(RefReg,t-1) ;
);

* hardcoded LFMMOUT_RenewDIMP=0 until updates made in lfreport.gms and restart.unf file
*loop( t$(t.val<2020),
*  LFMMOUT_RenewDIMP(MNUMCR,MNUMYR)$tMNUM(t,MNUMYR) = 0.0 ;
*);

* hardcoded to match RDHImpQuant for historical years, until move to rfhist.txt
* 1000 bbl/d

*loop( (t,MNUMYR)$tMNUM(t,MNUMYR),
*  if( LFMMOUT_RenewDIMP('11_United_States',MNUMYR) <= 0.0,
*
*      LFMMOUT_RenewDIMP('11_United_States',MNUMYR) = sum(DomRefReg, RDHImpQuant(DomRefReg,t) );
*      LFMMOUT_RenewDIMPpd('10_MNUMPR',MNUMYR)      = sum(DomRefReg, RDHImpQuant(DomRefReg,t) );
*      loop( DomRefReg,
*        LFMMOUT_RenewDIMPpd(MNUMPR,MNUMYR)$RefReg2MNUMPR(MNUMPR,DomRefReg)
*                                           = RDHImpQuant(DomRefReg,t) ;
*      );
*  );
*);

*    RDHImpPrice                                    Renewable Diesel import supply step prices
*    RDHImpSupply                                   Renewable Diesel import supply step quantities
*    RDHImpElas                                     Renewable Diesel import supply curve elasticity
*    RDHMiddlePrice                                 placeholder for Po renewable diesel import price 1987$ per bbl
*    RDHMiddleQuant                                 placeholder for Qo renewable or bio diesel import supply 1000 bbl per day


* hardcoded mapping of RefReg to CenDiv
* hardcoded MiddlePrice (Po) = 25% above diesel price, converted from 1987$/MMBtu to 1987$/bbl

RDHMiddlePrice(t)     = sum( MNUMYR$tMNUM(t,MNUMYR),
                               1.25*MPBLK_PDSTR('11_United_States',MNUMYR)*CONVFACT_CFDSUQ(MNUMYR) );


RDHCumQuant(Step,t) = 0.0 ;

loop( (ModelYears(t),MNUMYR)$tMNUM(t,MNUMYR),

  RDHMaxQty(t)             = sum(DomRefReg, RDHImpQuant(DomRefReg,t) ) ;

RDHMiddleQuant(t) = RDHMaxQty(t) ;


* loop( DomRefReg,
    RDHCumQuant('STEP01',t) = 0.750* RDHMiddleQuant(t) ;
    RDHCumQuant('STEP02',t) = 0.925* RDHMiddleQuant(t) ;
    RDHCumQuant('STEP03',t) = 1.075* RDHMiddleQuant(t) ;
    RDHCumQuant('STEP04',t) = 1.250* RDHMiddleQuant(t) ;
    RDHCumQuant('STEP05',t) = 1.750* RDHMiddleQuant(t) ;

* P = Po* (Q/Qo)^(1/elas) (1987$/bbl)
* Q = Qo* (P/Po)^elas     (1000 bbl/d)
* where Po = RDHMiddlePrice
*       Qo = MiddleQuant
*       Q  = CumQuant
    Loop( Step$(ord(Step) <= 5),
      RDHImpPrice(Step,t) = RDHMiddlePrice(t) *
                                   ( (RDHCumQuant(Step,t) / RDHMiddleQuant(t))**(1./RDHImpElas) ) ;
    );

    RDHImpSupply('STEP01',t) = RDHCumQuant('STEP01',t) ;
    RDHImpSupply('STEP02',t) = RDHCumQuant('STEP02',t) - RDHCumQuant('STEP01',t) ;
    RDHImpSupply('STEP03',t) = RDHCumQuant('STEP03',t) - RDHCumQuant('STEP02',t) ;
    RDHImpSupply('STEP04',t) = RDHCumQuant('STEP04',t) - RDHCumQuant('STEP03',t) ;
    RDHImpSupply('STEP05',t) = RDHCumQuant('STEP05',t) - RDHCumQuant('STEP04',t) ;

);

* extend beyond 2050
loop( t$(ord(t)>LastYear),
  RDHImpSupply(Step,t) = RDHImpSupply(Step,'2050');
  RDHImpPrice(Step,t)  = RDHImpPrice(Step,'2050');
);




* For now, set all input prices and quantities in future years to the 2008 values
* TEMPORARY!!  These will eventually come from NEMS..
RefInpPrc(RefReg,RefInputStr,Step,ModelYears(t))           = RefInpPrc(RefReg,RefInputStr,Step,'2008') ;
RefInpSup(RefReg,RefInputStr,Step,ModelYears(t))$(not sameas(RefInputStr, 'H2')) = RefInpSup(RefReg,RefInputStr,Step,'2008') ;
GlobalImpPrice(CenDiv,EndProduct,Step,ModelYears(t))       = GlobalImpPrice(CenDiv,EndProduct,Step,'2008') ;
GlobalImpSupply(CenDiv,EndProduct,Step,ModelYears(t))      = GlobalImpSupply(CenDiv,EndProduct,Step,'2008') ;
RFSWaiverPrice(RFSCategory,ModelYears(t))                  = RFSWaiverPrice(RFSCategory,'2008') ;
CrudeImportCost(RefReg,Source,Crude,Step,ModelYears(t))    = CrudeImportCost_Temp(RefReg,Source,Crude,Step) ;
CrudeExportCost(RefReg,Source,Crude,Step,ModelYears(t))    = CrudeExportCost_Temp(RefReg,Source,Crude,Step) ;


* Set up feedstock curves for CRN, GRN, FCO, YGR, WGR

* Corn
parameter
  MaxCornSupply(RefReg)           Max corn supply in M bushels per day
                                   / 2_REFREG 19300
                                     3_REFREG 15000  /
  MaxCornSupplyTotal

  CRNREGMULT(RefReg)              Max corn supply in M bushels per day
                                   / 2_REFREG 1.5151515
                                     3_REFREG 3.030303  /

  MidQuant_CoalDReg(CoalDReg,t)    Store in CoalDReg in MM Bushels per year
  MidPrice_CoalDReg(CoalDReg,t)    Store in CoalDReg in 2008 $ per bushel

  MidQuantR3(t)                  1000 bushels per day for RefReg 3
  MidPriceR3(t)                  2008 $ per bushel for RefReg 3
  MidPriceRR(RefReg,t)           2008 $ per bushel for all RefReg
  MidQuant(t)                    1000 bushels per day
  MidPrice(t)                    2008 $ per bushel
  CrnPrcElas(RefReg)             P equal P0 times Q over Q0 raised to elas
*                                 / 1.2 /
*                                 / 0.6 /
* 0.50 for RefReg 2,3; 0.75 for other RefRegs
                                  / 1_REFREG 0.75
                                    2_REFREG 0.50
                                    3_REFREG 0.50
                                    4_REFREG 0.75
                                    5_REFREG 0.75
                                    6_REFREG 0.75
                                    7_REFREG 0.75
                                    8_REFREG 0.75 /

  CUMLQUANT(RefReg,Step,t)         Cumulative corn quantities in billion bushels per year

* Seed Oil supply

  SoyOMidQuant_CoalDReg(CoalDReg,t)    Store SoyOil in CoalDReg in MM pounds per year
  SoyOMidPrice_CoalDReg(CoalDReg,t)    Store SoyOil in CoalDReg in 2008 $ per pound

  SoyOMidQuant(RefReg,t)         1000 barrels per day
  SoyOMidPrice(RefReg,t)         2008 $ per barrel
  FCOPrcElas                     P equal P0 times Q over Q0 raised to elas
                                  / 1.2 /

  FCOTotal(RefReg,t)           Total seed oil quantities in M bbl per day
  FCOCumulative(RefReg,Step,t) Total FCO at each step in M bbl per day

* grease supply (WGR and YGR combined)

  FATPROD(RefReg,t)              Total fat production used to calc grease sup in M bbl per day
  GreaseCum(RefReg,Step,t)       Total grease qty at each step in M bbl per day
  GreaseMidQuant(RefReg,t)       Qo for grease sup curve in M bbl per day
  GreaseMidPrice(RefReg,t)       Po for grease sup curve in 2008 $ per bbl
  GreasePrcElas                  P equal P0 times Q over Q0 raised to elas
                                  / 0.50 /


  QUANTFRAC(Step)                Quantity fractions for corn supply steps
                                  / STEP01 0.80
                                    STEP02 0.875
                                    STEP03 0.95
                                    STEP04 0.975
                                    STEP05 1.00
                                    STEP06 1.025
                                    STEP07 1.05
                                    STEP08 1.10
                                    STEP09 1.15
                                    STEP10 1.20
                                    STEP11 1.25
                                    STEP12 1.30
                                    STEP13 1.35
                                    STEP14 1.40
                                    STEP15 1.45
                                    STEP16 1.50
                                    STEP17 1.55
                                    STEP18 1.60
                                    STEP19 1.65
                                    STEP20 1.70
                                    STEP21 1.75
                                    STEP22 1.80
                                    STEP23 1.85
                                    STEP24 1.90
                                    STEP25 1.95
                                    STEP26 2.00
                                    STEP27 3.00
                                    STEP28 4.00
                                    STEP29 10.0 /

  PRICEratio(Step)               Price Ratio for corn supply steps 87$ per bushel
                                  / STEP01 0.80
                                    STEP02 0.875
                                    STEP03 0.95
                                    STEP04 0.975
                                    STEP05 1.00
                                    STEP06 1.025
                                    STEP07 1.05
                                    STEP08 1.10
                                    STEP09 1.15
                                    STEP10 1.20
                                    STEP11 1.25
                                    STEP12 1.30
                                    STEP13 1.35
                                    STEP14 1.45
                                    STEP15 1.50
                                    STEP16 1.55
                                    STEP17 1.65
                                    STEP18 1.70
                                    STEP19 1.80
                                    STEP20 1.88
                                    STEP21 1.96
                                    STEP22 2.05
                                    STEP23 2.15
                                    STEP24 2.25
                                    STEP25 2.35
                                    STEP26 2.50
                                    STEP27 2.75
                                    STEP28 3.00
                                    STEP29 10.0 /

  CRNPERLB(RefReg,t)               Corn price in 1987 cents per pound

* COALDtoREFmapPCT(CoalDReg,RefReg)    Coal demand region to RefReg allocation
;

Scalar
  ExpectedWOP2029                  Expected WOP in 2029 for growth 87 $ per bbl
  Ratio2WOP2029                    Ratio of WOP in current year to WOP in 2029 +1
* Total4CoalD                      Placeholder to help define COALDtoREFmapPCT
;

;


* COALDtoREFmapPCT(CoalDReg,RefReg) = 0.0 ;
* loop( CoalDReg,
*   Total4CoalD = sum(DomRefReg, COALDtoREFmap(CoalDReg,DomRefReg) ) ;
*   if( Total4CoalD > 0,
*     COALDtoREFmapPCT(CoalDReg,DomRefReg) = COALDtoREFmap(CoalDReg,DomRefReg) / Total4CoalD ;
*   );
*  );

* choose step 3 only, per ConnieG, convert to 2008 year $
* Share TOTAL corn-to-eth into regional values by using regional shares of TOTAL corn-produced

*  Loop((ModelYears(t),MNMYRF) $tMNMYRF(t,MNMYRF),
 loop( (t,MNMYRF)$(ModelYears(t) and tMNMYRF(t,MNMYRF)),

    MidQuant_CoalDReg(CoalDReg,t) $(WRENEW_CRNSUP_TOT_Q('03_M10','17_NDRGN1',MNMYRF) > 0) =
     sum( NDRGN1$CoalDReg2NDRGN17(CoalDReg,NDRGN1),
       WRENEW_CRNSUP_ETH_Q('03_M10','17_NDRGN1',MNMYRF) *
       WRENEW_CRNSUP_TOT_Q('03_M10',NDRGN1,MNMYRF) / WRENEW_CRNSUP_TOT_Q('03_M10','17_NDRGN1',MNMYRF)
     );

    MidPrice_CoalDReg(CoalDReg,t) = sum(NDRGN1$CoalDReg2NDRGN17(CoalDReg,NDRGN1) ,
       WRENEW_CRNSUP_P('03_M10',NDRGN1,MNMYRF) * agGDP87('2008') );

* TEMP: set values after 2029 equal to 2029 value (year 40=2029)
*    ExpectedWOP2029 = INTOUT_Start_Price('2029_MNUMYR') ;
*    if( (ord(t) > 40),
*      Ratio2WOP2029 = 1 + 0.25 * ( (ExpectedWOP(t) - ExpectedWOP2029) / ExpectedWOP2029 ) ;
*      MidPrice_CoalDReg(CoalDReg,t) = Ratio2WOP2029 * sum(NDRGN1$CoalDReg2NDRGN17(CoalDReg,NDRGN1) ,
*       WRENEW_CRNSUP_P('03_M10',NDRGN1,'2029_MNMYRF') * GDP('2008') );
*    );
  );


* fill MidQuant, convert from MM bushels/yr to 1000 bushels/day
* fill MidPrice, 2008 $/bushel

* ============================================================
* 1-31-19, NEW approach (CrnPrcElas now dimensioned by region)
* ============================================================

* =========================================================================
* 2-28-19, NEW approach (CornTranCost now delta off Polysys avg corn price)
* =========================================================================

* since all MidPrice in CoalDReg (2008$/bu) are the same (national average),
* take price from 'CoalDReg1'; convert CornTranCost fr 87$/bu to 2008$/bu

MidPriceRR(DomRefReg,t) = MidPrice_CoalDReg('CoalDReg1',t) + (CornTranCost(DomRefReg) * GDP('2008')) ;

loop( DomRefReg,

  loop(t$ModelYears(t),
    MidQuant(t)  = sum(CoalDReg$(CoalDReg2RefRegMap(CoalDReg,DomRefReg) >0),
                       MidQuant_CoalDReg(CoalDReg,t) *
                       CoalDReg2RefRegMap(CoalDReg,DomRefReg) )*
                   1000 /365 ;
    MidQuant(t)  = MidQuant(t) + CornTranSupMove(DomRefReg);
    MidPrice(t)  = MidPriceRR(DomRefReg,t) ;

    CUMLQUANT(DomRefReg,Step,t)       $QUANTFRAC(Step) = MidQuant(t)*QUANTFRAC(Step) ;
    RefInpSup(DomRefReg,'CRN',Step,t) $QUANTFRAC(Step) =
      CUMLQUANT(DomRefReg,Step,t) - CUMLQUANT(DomRefReg,Step-1,t) ;

* P = Po* (Q/Qo)^elas (2008$/bushel)
* where QuantFrac = Q / Q0
* add transport cost from production to process point = func(diesel price)
* 1-25-19, reduce CRN costs by 10% - hardcoded
*   RefInpPrc(DomRefReg,'CRN',Step,t) $QUANTFRAC(Step) = 0.90*
*     MidPrice(t) * (QUANTFRAC(Step) ** CrnPrcElas(DomRefReg)) +
*     sum(MNUMYR$tMNUM(t,MNUMYR), MPBLK_PDSTR('11_United_States',MNUMYR)*(5.825/42.0)*0.2664 - 0.3242 ) * GDP('2008') ;

* w/o reduced CRN costs, w/o extra diesel tran cost
    RefInpPrc(DomRefReg,'CRN',Step,t) $QUANTFRAC(Step) =
      MidPrice(t) * (QUANTFRAC(Step) ** CrnPrcElas(DomRefReg)) ;
  );
);

* extend beyond 2050

loop( t$(ord(t)>LastYear),
  RefInpSup(RefReg,'CRN',Step,t) = RefInpSup(RefReg,'CRN',Step,'2050') ;
  RefInpPrc(RefReg,'CRN',Step,t) = RefInpPrc(RefReg,'CRN',Step,'2050') ;
);


* ====================
*
* Use POLYSYS data to define soybean oil component of FCO supply curves, 8-31-17
*
* ====================

 loop( (t,MNMYRF)$(ModelYears(t) and tMNMYRF(t,MNMYRF)),

* MM lb/yr; allocate over regions based on regional soybean production
    SoyOMidQuant_CoalDReg(CoalDReg,t) $(WRENEW_SOYSUP_TOT_Q('03_M10','17_NDRGN1',MNMYRF) > 0) =
     1.5*sum(NDRGN1$CoalDReg2NDRGN17(CoalDReg,NDRGN1),
       WRENEW_SOYOILSUP_TOT_Q('03_M10','17_NDRGN1',MNMYRF) * (WRENEW_SOYSUP_TOT_Q('03_M10',NDRGN1,MNMYRF) /
                                                              WRENEW_SOYSUP_TOT_Q('03_M10','17_NDRGN1',MNMYRF) )
     );

* 2008 $/lb; set all regions equal to US price (convert from 87$/lb to 2008$/lb)
    SoyOMidPrice_CoalDReg(CoalDReg,t) = 0.5*WRENEW_SOYOILSUP_P('03_M10','17_NDRGN1',MNMYRF) * agGDP87('2008') ;

* set values after 2029 equal to 2029 value (year 40=2029)
*    ExpectedWOP2029 = INTOUT_Start_Price('2029_MNUMYR') ;
*    if( (ord(t) > 40),
*      Ratio2WOP2029 = 1 + 0.25 * ( (ExpectedWOP(t) - ExpectedWOP2029) / ExpectedWOP2029 ) ;
*      SoyOMidPrice_CoalDReg(CoalDReg,t) = Ratio2WOP2029 * sum(NDRGN1$CoalDReg2NDRGN17(CoalDReg,NDRGN1) ,
*       0.75*WRENEW_SOYOILSUP_P('03_M10','17_NDRGN1','2029_MNMYRF') * GDP('2008') );
*    );
 );

* fill SoyOMidQuant, convert from MM pounds/yr to 1000 bbl/day, 7.677 lb/gal for soybeanoil
* fill SoyOMidPrice, convert from 2008 $/lb to 2008$/bbl,       7.677 lb/gal for soybeanoil

SoyOMidQuant(RefReg,t)    = 0.0 ;
SoyOMidPrice(RefReg,t)    = 0.0 ;
loop( DomRefReg,
  loop(t$ModelYears(t),
    SoyOMidQuant(DomRefReg,t)    = sum(CoaldReg$(CoalDReg2RefRegMap(CoalDReg,DomRefReg) >0),
                                       CoalDReg2RefRegMap(CoalDReg,DomRefReg) *
                                       SoyOMidQuant_CoalDReg(CoalDReg,t) *
                                       1000/365 /7.677/42 ) ;
* ====================
* temp temp temp temp
*  - 3-20-19, em4
*  - since all coalD regions have same US avg price,
*    arbitrarily set all RefReg prices to single coalD region 5
* temp temp temp temp
* ====================

    SoyOMidPrice(DomRefReg,t)    = SoyOMidPrice_CoalDReg('CoalDReg5',t) * 7.677 *42  ;
  ) ;
) ;

* ====================
* temp temp temp temp
*  - 3-20-19, em4
*  - since mapping sets volume data in R4 to zero, split R5 results 50:50 into R4 and R5
* temp temp temp temp
* ====================

loop(ModelYears(t),

  SoyOMidQuant('4_RefReg',t) = 0.5* SoyOMidQuant('5_RefReg',t) ;
  SoyOMidQuant('5_RefReg',t) = 0.5* SoyOMidQuant('5_RefReg',t) ;
  SoyOMidPrice('4_RefReg',t) = SoyOMidPrice('5_RefReg',t) ;
) ;


* Make uniform supply steps for seed oil (FCO)
* - replace 'soybean' data from SeedOilQnty (1000 bbl/d)
*   with 30% (hardcoded based on 2016 USDA data) of Total SoyOil data from Polysys

* 7-13-20, esh
* - replace 30% with updated USDA projection for 2019/20, 34.3%
* - https://www.ers.usda.gov/data-products/us-bioenergy-statistics/

RefInpSup(RefReg,'FCO',Step,ModelYears(t)) = 0.0 ;
FCOTotal(DomRefReg,ModelYears(t)) = sum(FCOType$(not sameas(FCOType,'Soybean')), SeedOilQnty(DomRefReg,FCOType) ) +
                                    0.343 * SoyOMidQuant(DomRefReg,t) ;


loop(ModelYears(t),
   FCOTotal(RefReg,t)             = Max(0.001,FCOTotal(RefReg,t)) ;
   FCOCumulative(RefReg,Step,t)$QUANTFRAC(Step)= FCOTotal(RefReg,t)*QUANTFRAC(Step) ;
   RefInpSup(RefReg,'FCO',Step,t)$QUANTFRAC(Step) = FCOCumulative(RefReg,Step,t) - FCOCumulative(RefReg,Step-1,t) ;


* use SoyO price as proxy for FCO price, 2008$/bbl
* where QuantFrac = Q / Q0

   RefInpPrc(RefReg,'FCO',Step,t)$QUANTFRAC(Step) = SoyOMidPrice(RefReg,t) *
    (QUANTFRAC(Step) ** FCOPrcElas) ;
) ;


* ================================
*
* 3-26-19 em4: redo RefInpSup and RefInpPrc for WGR, YGR
*
* - combine into single grease supply curve
*   (for now, use 'WGR'slot; therefore, set 'YGR' = 0)
* - keep FATPROD calculation/assumptions
* - Qo for grease: set (new) GreaseMidQuant as func of FATPROD (*2/3)
* - Po for grease: set as 75% of soyOil price (per Steve Hanson research)
*
* ================================


* Fat production is 5.51 gal per person per year; 1.84 gal is available for biodiesel. MC_NP is in millions of persons

FATPROD(RefReg,t) = 0.0 ;
Loop(tMNUM(t,MNUMYR),
  FATPROD('1_REFREG',ModelYears(t)) = 1.84*MACOUT_MC_NP('01_New_England',MNUMYR)*1000/(42*365)  +
                                      1.84*MACOUT_MC_NP('02_Middle_Atlantic',MNUMYR)*1000/(42*365) +
                                      1.84*MACOUT_MC_NP('05_South_Atlantic',MNUMYR)*1000/(42*365) ;

  FATPROD('2_REFREG',ModelYears(t)) = 1.84*MACOUT_MC_NP('04_West_North_Central',MNUMYR)*1000/(42*365)  ;
  FATPROD('3_REFREG',ModelYears(t)) = 1.84*MACOUT_MC_NP('03_East_North_Central',MNUMYR)*1000/(42*365)  ;
  FATPROD('4_REFREG',ModelYears(t)) = 1.84*MACOUT_MC_NP('02_Middle_Atlantic',MNUMYR)*1000/(42*365)  ;
  FATPROD('5_REFREG',ModelYears(t)) = 1.84*MACOUT_MC_NP('07_West_South_Central',MNUMYR)*1000/(42*365)  ;
  FATPROD('6_REFREG',ModelYears(t)) = 1.84*MACOUT_MC_NP('08_Mountain',MNUMYR)*1000/(42*365) ;
* California is has 76.5% of the population in Census 9
  FATPROD('7_REFREG',ModelYears(t)) = 0.765*1.84*MACOUT_MC_NP('09_Pacific',MNUMYR)*1000/(42*365) ;
* Oregon, Washington, and Alaska have 23.5% of the population in Census 9
  FATPROD('8_REFREG',ModelYears(t)) = 0.235*1.84*MACOUT_MC_NP('09_Pacific',MNUMYR)*1000/(42*365) ;
);

* extend beyond 2050
loop( t$(ord(t)>LastYear),
  FATPROD(RefReg,t) = FATPROD(RefReg,'2050');
);

loop(ModelYears(t),
* define Qo and Po for grease
  GreaseMidQuant(RefReg,t) = 0.667 *FATPROD(RefReg,t) ;
  GreaseMidPrice(RefReg,t) = 0.750 *SoyOMidPrice(RefReg,t) ;

  GreaseCum(RefReg,Step,t) = 0.0 ;
  GreaseCum(DomRefReg,Step,t)$QUANTFRAC(Step)= GreaseMidQuant(DomRefReg,t)*QUANTFRAC(Step) ;
  RefInpSup(DomRefReg,'WGR',Step,t)$QUANTFRAC(Step) = GreaseCum(DomRefReg,Step,t) -
                                                      GreaseCum(DomRefReg,Step-1,t) ;

* use SoyO price as proxy for grease price, 2008$/bbl
* where QuantFrac = Q / Q0
  RefInpPrc(DomRefReg,'WGR',Step,t)$QUANTFRAC(Step) = GreaseMidPrice(DomRefReg,t) *
    (QUANTFRAC(Step) ** GreasePrcElas) ;


* set 'YGR' version = 0 because no longer used
  RefInpSup(DomRefReg,'YGR',Step,t)$QUANTFRAC(Step) = 0.0 ;
  RefInpPrc(DomRefReg,'YGR',Step,t)$QUANTFRAC(Step) = 0.0 ;

) ;



*-- Grain --*
scalar
*analyst judgment (PGR, mc6) as to where the Barley quantity curve should be centered.
  ALLOWED /0.50/

* National Barley($/bu)/Corn($/bu) price ratio, averaged over 2000-2010 by PGR
* Source: www.ers.usfa.gov/Data/feedgrains/FeedYearbook.aspx#USacre
* 10-4-17 em4:  chg .9147851 to 0.5 to reduce price of grain to AET and NCE
* BC_RATIO /0.9147851/
  BC_RATIO /0.500/
;

Parameters
  GROWTH(t)                Growth rate in corn to ethanol from 2008 to current year

;

GROWTH(ModelYears(t)) = sum(MNUMYR$tMNUM(t,MNUMYR),
  LFMMOUT_CORNCD('3_Total','11_United_States',MNUMYR) / LFMMOUT_CORNCD('3_Total','11_United_States','2008_MNUMYR') ) ;

* 10-4-17 em4:  double GrainQnty by *2 in equation below (to help reduce marginal price of ethanol)
RefInpSup(RefReg,'GRN',Step,ModelYears(t))$QUANTFRAC(Step) = 1.25*
  GROWTH(t) * ALLOWED * 2.* GrainQnty(RefReg) * (QUANTFRAC(Step) - QUANTFRAC(Step-1)) ;

RefInpPrc(RefReg,'GRN',Step,ModelYears(t))$QUANTFRAC(Step) =
  BC_RATIO * RefInpPrc(RefReg,'CRN',Step,t) ;
*cst0  BC_RATIO * RefInpPrc(RefReg,'CRN',Step,t) +
*cst0  sum(MNUMYR$tMNUM(t,MNUMYR), MPBLK_PDSTR('11_United_States',MNUMYR)*(5.825/42.0)*0.2664 - 0.3242 ) * GDP('2008') ;

* extend beyond 2050
loop( t$(ord(t)>LastYear),
  RefInpSup(RefReg,'GRN',Step,t) = RefInpSup(RefReg,'GRN',Step,'2050') ;
  RefInpPrc(RefReg,'GRN',Step,t) = RefInpPrc(RefReg,'GRN',Step,'2050') ;
);

* Merchant Hydrogen price
RefInpPrc(DomRefReg,'H2','STEP01',t) = sum(MNUMYR$tMNUM(t,MNUMYR),sum((ActiveDem,MNUMCR)$(crN2L(ActiveDem,MNUMCR)and RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*HMMBLK_PH2RF(MNUMCR,MNUMYR)));

*-- Merchant Hydrogen --*
* Allow H2 purchasing in all refinery regions (merchant now includes refinery SMR H2).

* extend beyond 2050
loop( t$(ord(t)>LastYear),
  RefInpSup(DomRefReg,'H2','STEP01',t) = RefInpSup(DomRefReg,'H2','STEP01','2050') ;
);


*-----------------------------------------------------------------------------------------------------------
* Parameters for discounting
Parameters
   npv_OpVarCost(Process,ProcessMode,Period)                      NPV of stream of process OVCs for current period in NOMINAL dollars
   npv_RecipeOVC(RcpMode,Period)                                  NPV of stream of recipe OVCs for current period in NOMINAL dollars
   npv_UtilityPrice(RefReg,Utility,Period)                        NPV of stream of utility prices at each step in each period in NOMINAL dollars
   npv_EthImpSupPrc(EthStream,Step,Period)                        NPV of Price at each step of ethanol import curve to US
   npv_EthImpSupQty(EthStream,Step,Period)                        NPV of Supply qty at each step of ethanol import curve to US
   npv_EthExpDmdPrc(EthStream,Step,Period)                        NPV of Price at each step of ethanol export curve fr US
   npv_EthExpDmdQty(EthStream,Step,Period)                        NPV of Supply qty at each step of ethanol export curve fr US
*  npv_EthBrazilPrc(EthStream,Step,Period)                        NPV of stream of Brazilian ethanol prices at each step in each period in NOMINAL dollars
   npv_BrzAdvTranCost(Period)                                     NPV of stream of ethanol export transport prices at each step in each period in NOMINAL dollars
   npv_TranCostToBrazil(Period)                                   NPV of cost coeff for ETHEXP
   npv_TranCostFromBrazil(Period)                                 NPV of cost coeff for ETHIMP

   npv_FBDImpPrice(Step,Period)                                   NPV of stream of biodiesel import prices at each step in each period in NOMINAL dollars
   npv_RDHImpPrice(Step,Period)                                   NPV of stream of renewable diesel import prices at each step in each period in NOMINAL dollars
   npv_RefInpPrc(RefReg,RefInputStr,Step,Period)                  NPV of stream of refinery input prices at each step in each period in NOMINAL dollars
   npv_BiomassPrc(CoalDReg,BioStr,Step,Period)                    NPV of stream of biomass prices at each step in each period in NOMINAL dollars
   npv_CoalPrc(CoalSReg,CoalStr,Step,Period)                      NPV of stream of coal prices at each step in each period in NOMINAL dollars
   npv_SO2Prc(CoalStr,MX_SO2,Period)                              NPV of stream of SO2 emission prices at each step in each period in NOMINAL dollars
   npv_HGPrc(CoalDReg,CoalStr,Period)                             NPV of stream of HG emission prices at each step in each period in NOMINAL dollars
   npv_CoalTrnspCst(CoalSReg,CoalDReg,CoalStr,Period)             NPV of stream of coal transport prices at each step in each period in NOMINAL dollars
   npv_REFtoCDTranCost(RefReg,CenDiv,TranMode,RecipeProd,Period)  NPV of stream of Sup-CD transport prices at each step in each period in NOMINAL dollars
   npv_REFtoREFTranCost(RefReg,RefRegA,TranMode,Stream,Period)    NPV of stream of Sup-Sup transport prices at each step in each period in NOMINAL dollars
   npv_REFtoINTTranCost(RefReg,INTREG,Period)                     NPV of stream of RefRef-IntReg transport prices in each period in NOMINAL dollars
   npv_INTtoREFTranCost(INTREG,RefReg,Period)                     NPV of stream of IntReg-RefRef transport prices in each period in NOMINAL dollars

   npv_INTtoREFFBDTranCost(INTREG,RefReg,Period)                  NPV of stream of IntReg-RefRef transport prices for FBD in each period in NOMINAL dollars

   npv_GlobalImpPrice(CenDiv,EndProduct,Step,Period)              NPV of stream of product import prices at each step in each period in NOMINAL dollars
   npv_FXOCCost(Process,RefReg,Period)                            NPV of fixed cost charges for existing capacity operates in NOMINAL dollars
   npv_BuildCost(Process,RefReg,BldStep,Period)                   NPV of capital + fixed cost charges for new builds in NOMINAL dollars
   npv_CrudePriceTotal(Step,Period)                               NPV of stream of marker crude prices at each step in each period
   npv_CrudePriceIncremental(Source,Crude,Step,Period)            NPV of stream of crude price differential at each step in each period
   npv_CrudeImportCost(RefReg,Source,Crude,Step,Period)           NPV of Crude Transport Cost From Crude Source to Refinery supply region
   npv_CrudeExportCost(RefReg,Source,Crude,Step,Period)           NPV of Crude Transport Cost From Refinery supply region to Crude Source
   npv_NonUSCrudeDemandPrice(Source,Crude,Step,Period)            NPV of Non-US Crude Demand Price by Crude Source
   npv_RFSWaiverPrice(RFSCategory,Period)                         NPV of RFS saftey valve price
   npv_WSGasPrices(Period)                                        NPV of wholesale gas prices
   npv_MOGASPRICE(CenDiv,Period)
   npv_TotalMarkups(Period,MarkupFuel,MarkupSector,CenDiv)        NPV of total fuel markups
   npv_CoproductPrice(Stream,RefReg,Period)                       NPV of coproduct prices
   npv_MCCPrice(Stream,Step,Period)                               NPV of Caribbean and Maritime Canada demand curve prices

   npv_ImportPrice(Stream,INTREG,Step,Period)
   npv_ImportSupply(Stream,INTREG,Step,Period)

   npv_ExportPrice(Stream,INTREG,Step,Period)
   npv_ExportSupply(Stream,INTREG,Step,Period)

   npv_SPR_Withdraw(RefReg,Crude,Period)

   npv_LCFSSafetyPrice(LCFS_PetCategory,Period)
   npv_CFPSafetyPrice(CFP_PetCategory,Period)
   npv_WACFSSafetyPrice(WACFS_PetCategory,Period)
   npv_NGLImportCost(CenDiv,NGLProduct,Period)
   npv_NGLExportCost(CenDiv,NGLProduct,Period)

   npv_GlobalImpSupply(CenDiv,EndProduct,Step,Period)             NPV AVERAGE of global import supply at each step in each period
   npv_BiomassSup(CoalDReg,BioStr,Step,Period)                    NPV AVERAGE of biomass supply bounds at each step in each period
   npv_BioOthDemand(CoalDReg,BioStr,Other_MKT,Period)             NPV AVERAGE of biomass demands from other NEMS modeules at each step in each period
   npv_EthBrazilSup(EthStream,Step,Period)                        NPV AVERAGE of Brazilian ethanol supply bounds at each step in each period
   npv_FBDImpSupply(Step,Period)                                  NPV AVERAGE of biodiesel import supply bounds at each step in each period
   npv_RDHImpSupply(Step,Period)                                  NPV AVERAGE of renewable diesel import supply bounds at each step in each period
   npv_FBDImpSplit(RefReg,Period)                                 NPV AVERAGE of biodiesel import split in each period
   npv_NonUSEthDmd(Period)                                        NPV AVERAGE of Non-US ethanol ethanol demand in each period
   npv_RefInpSup(RefReg,RefInputStr,Step,Period)                  NPV AVERAGE of supply region input supply bounds at each step in each period
   npv_GasSpecMin(GasSpecProd,GasProp,Period)                     NPV AVERAGE of gas spec minimums at each step in each period
   npv_GasSpecMax(GasSpecProd,GasProp,Period)                     NPV AVERAGE of gas spec maximums at each step in each period
   npv_DistSpecMin(DistSpecProd,DistProp,Period)                  NPV AVERAGE of diesel spec minimums at each step in each period
   npv_DistSpecMax(DistSpecProd,DistProp,Period)                  NPV AVERAGE of diesel spec maximums at each step in each period
   npv_ResidSpecMin(ResidSpecProd,ResidProp,Period)               NPV AVERAGE of resid spec minimums at each step in each period
   npv_ResidSpecMax(ResidSpecProd,ResidProp,Period)               NPV AVERAGE of resid spec maximums at each step in each period
   npv_ProductDemand(CenDiv,EndProduct,Period)                    NPV AVERAGE of product demands in each period
   npv_NGLDemands(CenDiv,NGLProduct,Period)                       NPV AVERAGE of NGPL demands in each period
   npv_BTUDemand(CenDiv,Period)                                   NPV AVERAGE of gas blend demands in billion BTUs each period
   npv_CoalSup(CoalSReg,CoalStr,Step,Period)                      NPV AVERAGE of coal supply bounds at each step in each period
   npv_CoalOthDemand(CoalSreg,CoalStr,Period)                     NPV AVERAGE of coal demands from other NEMS modeules at each step in each period
   npv_SO2Emission(CoalSReg,CoalDReg,CoalStr,MX_SO2,Period)       NPV AVERAGE of coal demands from other NEMS modeules at each step in each period
   npv_HGEmission(CoalSReg,CoalDReg,CoalStr,Period)               NPV AVERAGE of coal demands from other NEMS modeules at each step in each period
   npv_CrudeSupplyTotal(Step,Period)                              NPV AVERAGE of Total World Crude Supply
   npv_CrudeSupplyIncremental(Source,Crude,Step,Period)           NPV AVERAGE of Crude Supply by Crude Type and Foreign Source
   npv_DomesticCrudeSup(RefReg,Crude,Period)                      NPV AVERAGE of Domestic Crude Supply by Crude Type
   npv_CrudeImportCap(RefReg,Source,Step,Period)                  NPV AVERAGE of Crude Transport Limits From Crude Source to Refinery supply region
   npv_CrudeExportCap(RefReg,Source,Step,Period)                  NPV AVERAGE of Crude Transport Limits From Refinery supply region to Crude Source

   npv_NonUSCrudeDemandValue(Source,Crude,Step,Period)            NPV AVERAGE of Non-US Crude Demand by Crude Source
   npv_CrudeToCanada(RefReg,Crude,Period)                         NPV AVERAGE of US crude exports to Canada

   npv_NGLBounds(NGLInputStr,RefReg,Period)                       NPV AVERAGE of natural gas liquid input streams

   npv_RFSMandates(RFSCategory,Period)                            NPV AVERAGE RFS mandates in M BBL per day

   npv_PetDSFrac(Period)                                          NPV AVERAGE of fraction of DSU derived from petroleum
   npv_PetMGFrac(Period)                                          NPV AVERAGE of fraction of gasoline and E85 derived from petroleum
   npv_PetMGFrac_noETH(Period)                                    NPV AVERAGE of fraction of gasoline and E85 derived from petroleum not counting ethanol

   npv_CFMGQCD(CenDiv,Period)                                     NPV AVERAGE average motor gas energy density
   npv_CFE85Q(Period)                                             NPV AVERAGE E85 energy density
   npv_E15MaxPen(CenDiv,Period)                                   NPV AVERAGE max E15 penetration
   npv_MCCSupply(Stream,Step,Period)                              NPV AVERAGE Caribbean and Maritime Canada demand curve supply

   npv_RFSCreditPrice(RFSCategory,Period)                         NPV of RFS Credit Prices
   npv_RFSWaivers(RFSCategory,Period)                             NPV of RFS Safety Valves
   npv_MaxBuilds(Process,BldStep,Period)                          NPV AVERAGE maximum allowed builds per period

   npv_REFtoREFTranCap(RefReg,RefRegA,TranMode,Period)            NPV AVERAGE RefReg-RefReg transportation capacity

   npv_LCFS_Bio_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioProcess,LCFS_BioMode,Period) NPV Average of LCFS Non-Petroleum Carbon Factor Index
   npv_LCFS_Imp_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioImports,Period)             NPV Average of LCFS Imported Bio-Liquids Carbon Factor Index

   npv_LCFS_BiodImp_CFactor(LCFS_PetCategory,LCFS_RefReg,Period)                         NPV Average of LCFS Imported Biodiesel Carbon Factor Index in tonne carbon per bbl
   npv_LCFS_RenewDImp_CFactor(LCFS_PetCategory,LCFS_RefReg,Period)                         NPV Average of LCFS Imported Renewable diesel Carbon Factor Index in tonne carbon per bbl

   npv_LCFS_Pet_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_PetStreams,Period)             NPV Average of LCFS Petroleum Carbon Factor Index
   npv_LCFS_LFGas_CFactor(LCFS_PetCategory,Period)                                       NPV Average of LCFS Landfill Gas Carbon Factor Index Offset
   npv_LCFS_LandfillGas_total(Period)                                                    NPV Average of LCFS Landfill Gas demand -trill BTU

   npv_LCFS_Alt_CFactor(LCFS_PetCategory,LCFS_Vehicle_Types,Period)                      NPV Average of LCFS Alternate Vehicle Carbon Factor Index
   npv_LCFS_AltVehicle_Demand(LCFS_PetCategory,LCFS_Vehicle_Types,LCFS_RefReg,Period)    NPV Average of LCFS Alternate Vehicle Demand (Trills)
   npv_LCFS_Price(LCFS_PetCategory,Period)                                               NPV of LCFS Credit Price
   npv_LCFS_Eth_CFactor(LCFS_PetCategory,LCFS_BioStreams,Period)

   npv_CFP_Bio_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioProcess,CFP_BioMode,Period) NPV Average of CFP Non-Petroleum Carbon Factor Index
   npv_CFP_Imp_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioImports,Period)             NPV Average of CFP Imported Bio-Liquids Carbon Factor Index

   npv_CFP_BiodImp_CFactor(CFP_PetCategory,CFP_RefReg,Period)                         NPV Average of CFP Imported Biodiesel Carbon Factor Index in tonne carbon per bbl
   npv_CFP_RenewDImp_CFactor(CFP_PetCategory,CFP_RefReg,Period)                         NPV Average of CFP Imported Renewable diesel Carbon Factor Index in tonne carbon per bbl

   npv_CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,Period)             NPV Average of CFP Petroleum Carbon Factor Index
   npv_CFP_LFGas_CFactor(CFP_PetCategory,Period)                                       NPV Average of CFP Landfill Gas Carbon Factor Index Offset
   npv_CFP_LandfillGas_total(Period)                                                    NPV Average of CFP Landfill Gas demand -trill BTU

   npv_CFP_Alt_CFactor(CFP_PetCategory,CFP_Vehicle_Types,Period)                      NPV Average of CFP Alternate Vehicle Carbon Factor Index
   npv_CFP_AltVehicle_Demand(CFP_PetCategory,CFP_Vehicle_Types,CFP_RefReg,Period)    NPV Average of CFP Alternate Vehicle Demand (Trills)
   npv_CFP_Price(CFP_PetCategory,Period)                                               NPV of CFP Credit Price
   npv_CFP_Eth_CFactor(CFP_PetCategory,CFP_BioStreams,Period)

   npv_WACFS_Bio_CFactor(WACFS_PetCategory,WACFS_RefReg,WACFS_BioProcess,WACFS_BioMode,Period) NPV Average of WACFS Non-Petroleum Carbon Factor Index
   npv_WACFS_Imp_CFactor(WACFS_PetCategory,WACFS_RefReg,WACFS_BioImports,Period)             NPV Average of WACFS Imported Bio-Liquids Carbon Factor Index

   npv_WACFS_BiodImp_CFactor(WACFS_PetCategory,WACFS_RefReg,Period)                         NPV Average of WACFS Imported Biodiesel Carbon Factor Index in tonne carbon per bbl
   npv_WACFS_RenewDImp_CFactor(WACFS_PetCategory,WACFS_RefReg,Period)                         NPV Average of WACFS Imported Renewable diesel Carbon Factor Index in tonne carbon per bbl

   npv_WACFS_Pet_CFactor(WACFS_PetCategory,WACFS_RefReg,WACFS_PetStreams,Period)             NPV Average of WACFS Petroleum Carbon Factor Index
   npv_WACFS_LFGas_CFactor(WACFS_PetCategory,Period)                                       NPV Average of WACFS Landfill Gas Carbon Factor Index Offset
   npv_WACFS_LandfillGas_total(Period)                                                    NPV Average of WACFS Landfill Gas demand -trill BTU

   npv_WACFS_Alt_CFactor(WACFS_PetCategory,WACFS_Vehicle_Types,Period)                      NPV Average of WACFS Alternate Vehicle Carbon Factor Index
   npv_WACFS_AltVehicle_Demand(WACFS_PetCategory,WACFS_Vehicle_Types,WACFS_RefReg,Period)    NPV Average of WACFS Alternate Vehicle Demand (Trills)
   npv_WACFS_Price(WACFS_PetCategory,Period)                                               NPV of WACFS Credit Price
   npv_WACFS_Eth_CFactor(WACFS_PetCategory,WACFS_BioStreams,Period)


   npv_AB32_CapAdjFactor(Period)                                                         NPV Average AB-32 cap adjustment factors
   npv_AB32_AssistFactor(Period)                                                         NPV Average AB-32 industry assistance factors
   npv_AB32_AllowPrice(Period)                                                           NPV AB-32 allowance price

   npv_CarbonTax(Stream,CenDiv,Period)                            NPV carbon tax in nominal dollars per barrel
   npv_CarbonTaxCredit(Stream,Period)                             NPV biofuel carbon tax credits in nominal dollars per barrel

*   npv_CCATSDAT_CO2_PRC_DIS_45Q(RefReg,Period)                    NPV CO2 price output after optimization (45Q eligible CO2) Census Division

   npv_CO2EmissionTax(Period)                                     NPV refinery CO2 emission tax in dollars per ton CO2

   npv_CO2_Price(OGCO2Reg,CO2_Source,Period)                      NPV CO2 price by source in each OGSM region ($ per ton CO2)
   npv_OGSMCO2Dem(OGCO2Reg,Period)                                NPV AVERAGE OGSM EOR CO2 demand in M tons CO2 per day
   npv_CO2_Avail(OGCO2Reg,CO2_Source,Period)                      NPV AVERAGE Available CO2 Supply by OGSM region and CO2 source in Mtons CO2 per day

   npv_CO2_OGTranCost(OGCO2Reg,OGCO2Reg2,Period)                  NPV CO2 transport cost from OGMS to OGSM (87$ per ton CO2)
   npv_FRtoSalineCost(FuelRegion,Period)                          NPV CO2 transport cost to saline from each Fuel Region (87$ per ton CO2)
   npv_FRtoEORCost(FuelRegion,OGCO2Reg,Period)                    NPV CO2 transport cost to EOR from Fuel Region to OGSM region (87$ per ton CO2)
   npv_CO2_SafetyPrice(OGCO2Reg,Period)                           NPV CO2 safety valve price

   npv_State_Biodiesel(RefReg,State,Period)                       NPV Average state biodiesel mandates

   npv_BiofuelSubsidy(Stream,Period)                              NPV federal biofuel subsidies

   rate(t)                                                        Discount rate
   CrudeRate(t)                                                   Discount rate for crude price differentials
;

* HARDCODE THE DISCOUNT RATE FOR NOW!
rate(ModelYears(t))      = 0.18 ;
CrudeRate(ModelYears(t)) = 0.18 ;

Parameter YrIdx(t)               Ordinal index of t in the Model years ;

YrIdx(ModelYears(t))   = ord(t) - sum(tt,ord(tt)$(tt.val= NCNTRL_CURCALYR)) ;

* Defining Discount as a macro allows the interest rate parameter (RateParam) to be of any form or dimension, e.g. rate(t), TestRate(process)
$macro Discount(RateParam)   (1 + RateParam) ** (-YrIdx(t))

*-----------------------------------------------------------
* Calculate NPV of relevant parameters

$macro npv1(costtable,RateParam)        sum(tPer(ModelYears(t),ActivePeriod), costtable * GDP(t)* Discount(RateParam) )

* NPV Average
$macro npv2(costtable,RateParam)        sum(tPer(ModelYears(t),ActivePeriod), costtable * Discount(RateParam)) / sum(tPer(ModelYears(t),ActivePeriod), Discount(RateParam))

* Build costs carry out to all future periods, i.e. Per2 value = Per2 + Per3 values
$macro npv3(table1,table2,RateParam)    sum(Period2$(ord(Period2)>=ord(Period)),sum(tPer(ModelYears(t),Period2), (table1 + table2) * GDP(t)* Discount(RateParam) ) )


*-----------------------------------------------------------
* Capacity expansion and fixed cost calculation parameters
Scalars
   NFInvFac                                Nelson-Farr investment cost index
   NFLaborFac                              Nelson-Farr labor cost index
;

Parameters
   EffectiveTax(RefReg)                    Effective tax rate
   CostOfEquity(Process,RefReg)            Cost of equity
   CostOfDebt(Process,RefReg)              Cost of debt
   CostOfCap(Process,RefReg)               Cost of Capital after taxes
   IntDuringConst(Process,RefReg)          FV over build years (interest during construction)
   FVInstPay(Process,RefReg)               FV for instantaneous payment
   PVProject(Process,RefReg)               PV of prj fr end of life (salvage-decom)
   CRF(Process,RefReg)                     Capital recovery factor

   AdjISBL(Process,RefReg)                 ISBL cost in MM 87$ adjusted by Nelson-Farr investment index
   AdjLabor(Process,RefReg)                ISBL labor cost in 87$ per day adjusted by Nelson-Farr labor index
   TotalFieldCost(Process,RefReg)          Total Field Cost (ISBL+OSBL) (MM 87$)
   LandOTC(Process,RefReg)                 Land OTC (MM 87$)
   WorkingCapOTC(Process,RefReg)           Working Capital OTC (MM 87$)
   TotalOTCFac(Process,RefReg)             Total OTC Factor
   TotalOTC(Process,RefReg)                Total OTC (MM 87$)
   TotalProjInv(Process,RefReg)            Total Project Investment (TPI) (MM 87$)
   FinalInvest(Process,RefReg)             Final Investment (87$ per bpcd)
   FixedCapInv(Process,RefReg)             Fixed Capital Investment (MM 87$)
   UnitFCI(Process,RefReg)                 Unitized Fixed Capital Investment (87$ per bpcd)
   TotDeprInv(Process,RefReg)              Total Depreciable Investment (MM 87$) (assumed approx = Fixed Capital Investment)
   TPIStartYr(Process,RefReg)              TPI for startup yr (MM 87$)
   RecInvStart(Process,RefReg)             Recoverable Investment at Startup (MM 87$)
   PVPrjInv(Process,RefReg)                PV for project investment (MM 87$)
   Annuity(Process,RefReg)                 Annual Capital Recovery (MM 87$)
   AnnLevDepr(Process,RefReg)              Annual Levelized depreciation (SRL method) (MM 87$)
   AnnDeprTaxCred(Process,RefReg)          Levelized depreciation tax credit after tax (MM 87$)
   AnnCapCharge(Process,RefReg)            Annual capital charge after tax credit (MM 87$)
   DailyCapChrg(Process,RefReg)            Daily cap charge after tax credit (87$ per cd)
   CapRec(Process,RefReg)                  Capital recovery costs before taxes (87$ per bpcd)
   InsFXOC(Process,RefReg)                 Insurance FXOC (87$ per cd)
   TaxFXOC(Process,RefReg)                 Local Tax FXOC (87$ per cd)
   MaintFXOC(Process,RefReg)               Maintenance FXOC (87$ per cd)
   OtherFXOC(Process,RefReg)               Supplies-OH-Env FXOC (87$ per cd)
   CapFXOC(Process,RefReg)                 Total Capital-related FXOC (87$ per cd)
   StaffFXOC(Process,RefReg)               Staff FXOC (87$ per cd)
   OverheadFXOC(Process,RefReg)            Benefits & OH FXOC (87$ per cd)
   LaborFXOC(Process,RefReg)               Total Labor-related FXOC (87$ per cd)
   TotalFXOC(Process,RefReg)               Total FXOC (87$ per bpcd)
;

* Calculate N-F cost deflators
NFInvFac   = sum(t$CapExpYr(t),NFInvest(t))/sum(t$NFBaseYr(t),NFInvest(t)) ;
NFLaborFac = sum(t$CapExpYr(t),NFLabor(t))/sum(t$NFBaseYr(t),NFLabor(t)) ;

* Change the following cost parameters to positive numbers

OpVarCost(Process,ProcessMode) = -1*OpVarCost(Process,ProcessMode) ;
RecipeOVC(RcpMode)             = -1*RecipeOVC(RcpMode) ;

* Deflate all prices and costs to 87$ since that is what will be coming from
* NEMS in the future. All LFMM spreadsheet input data is currently in 2008$.
* TEMPORARY UNTIL NEMS INTEGRATION!
OpVarCost(Process,ProcessMode) =
   OpVarCost(Process,ProcessMode)/GDP('2008') ;
RecipeOVC(RcpMode) =
   RecipeOVC(RcpMode)/GDP('2008') ;
RefInpPrc(RefReg,RefInputStr,Step,ModelYears(t)) =
   RefInpPrc(RefReg,RefInputStr,Step,t)/GDP('2008') ;
GlobalImpPrice(ActiveDem,EndProduct,Step,ModelYears(t)) =
   GlobalImpPrice(ActiveDem,EndProduct,Step,t)/GDP('2008') ;
*-----------------------------------------------------------

*-----------------------------------------------------------
* Set up RFS waiver prices (1987$ per bbl)


*--- NEED TO USE THE WHOLESALE PRICE HERE INSTEAD!!!! ----*
* 1987$ per bbl
WSGasPrices(ModelYears(t)) = sum(MNXYRS$tMNXYRS(t,MNXYRS), MXPBLK_XPALMG('11_United_States',MNXYRS) ) ;

* Set the Cellulosic waiver price according the the EISA2007 statutory formula
RFSWaiverPrice('Cellulosic',t)$ModelYears(t) =
  max(0.25*42/GDP('2008'), 3.00*42/GDP('2008') - WSGasPrices(t) );


* Set the non-statutory Advanced waiver price to an "economic harm" number
parameter RFSWaiverPriceAdv(t);
RFSWaiverPriceAdv(t)$ModelYears(t) = 5.00 * 42/GDP('2011');

*-----------------
RFSWaiverPrice('Advanced',t)$ModelYears(t) = RFSWaiverPriceAdv(t);
RFSWaiverPrice('Total',t)$ModelYears(t) = RFSWaiverPrice('Advanced',t) - .01*42/GDP('2011') ;
RFSWaiverPrice('Biodiesel',t)$ModelYears(t) = RFSWaiverPrice('Advanced',t) + 1.00*42/GDP('2011');
*-----------------

* end of "Set up RFS waiver prices
*-----------------------------------------------------------


*----------------------------------------------------------------------------------------------------------
* E85 Demand Curve Data Prep

Sets
   E85_Steps                     E85 Demand Curve Steps               /E85STP_01*E85STP_55/
;

Parameters
     Q_E85STP(E85_Steps,CenDiv,Period)        E85 Demand Curve Incremental Step Quantity
     P_E85STP(E85_Steps,CenDiv,Period)        E85 Demand Curve Step Price Total
     P_E85_IC(E85_Steps,CenDiv,Period)        E85 Demand Curve Step Price - Infrastructure Cost
     P_E85_SC(E85_Steps,CenDiv,Period)        E85 Demand Curve Step Price - Station Cost
     A_E85STP(E85_Steps,CenDiv,Period)        E85 Demand Curve Optimal Availability at each step

     Q_E85STP_t(E85_Steps,CenDiv,t)           E85 Demand Curve Incremental Step Quantity
     P_E85STP_t(E85_Steps,CenDiv,t)           E85 Demand Curve Step Price Total
     P_E85_IC_t(E85_Steps,CenDiv,t)           E85 Demand Curve Step Price - Infrastructure Cost
     P_E85_SC_t(E85_Steps,CenDiv,t)           E85 Demand Curve Step Price - Station Cost
     A_E85STP_t(E85_Steps,CenDiv,t)           E85 Demand Curve Optimal Availability at each step

     E85availLYR                              Last year for historical E85AVAIL data

     E85curveSwitch                           SWITCH btwn 1-original and 2-new E85 curve shape
     E85_stepsLOOP                            Number of steps on E85 curve original or new curve type
     E85_ELAS(E85_Steps)                      Elasticity for each step of hockey stick curve
     PoE85(CenDiv,t)                          Po for E85 hockey stick curve 1987$ per MMBtu
     QoE85(CenDiv,t)                          Qo for E85 hockey stick curve tril Btu
     P_E85Markups_delta(t,CenDiv)             Difference between P0E85 and TotalMarkups for E85
     npv_E85TotalMarkups(Period,CenDiv)       total markups for just E85
     PMGTRadjSTEO(CenDiv)                     STEO adj PMGTR 87dollar per MMBtu
     tempPMGTR(CenDiv)                        PMGTR adjusted for STEO phaseout after STEOLYR 87dollar per MMBtu

     QFFVt(CenDiv,t)                          Maximum E85 Demand - by year t and region MNUMCR
     QFFV(CenDiv,Period)                      Maximum E85 Demand - by period and region MNUMCR
     MOGASPRICE(CenDiv,t)                     Motor Gasoline Price - Regional
     Trill_Btu_to_Bgal(t)                     Conversion Factor for E85 - Trillion Btu to Billions of Gallons
     Trill_Btu_to_MBBL_CD(t)                  Conversion Factor for E85 - Trillion Btu per year to Thousands of Barrels per Calendar Day
     D_MMBTU_to_D_BBL_CD(t)                   Conversion Factor for E85 - Million Btu per Barrel
     Upper_PRC                                Maximum E85 Price Considered
     Lower_QTY                                Amount of E85 associated with Upper_PRC
     Lower_PRC                                Minimum E85 Price Considered
     Upper_QTY                                Amount of E85 associated with Lower_PRC
     Lower_SHR                                E85 share associated with Lower_PRC
     Upper_SHR                                E85 share associated with Upper_PRC
     GASPRR                                   Intermediate Value in E85 - Gasoline Logit Function
     ALTPRR                                   Intermediate Value in E85 - Gasoline Logit Function
     Max_Trial                                Maximum number of trials in Bisection Algorithm
     I_Trial                                  Counter for number of trials in Bisection Algorithm
     Test_Trial                               Test_Trial = 1 when Bisection Algorithm has converged - 0 otherwise
     Trial_PRC                                Trial E85 price in Bisection Algorithm
     Trial_QTY                                Amount of E85 associated with Trial_PRC
     Trial_SHR                                E85 share associated with Trial_PRC
     Test_Price                               E85 price resulting from Bisection Algorithm
     Max_E85_STP(CenDiv,Period)               Last setp taken - output
     T_QMGTR(MNUMCR,MNUMYR)                   Total Motor Gas Demand Trills?
     T_QETTR_SHR(MNUMCR,MNUMYR)               E85 Share of FFV Demand

     LCFSOPT                                  California LCFS Switch 0=> Off 1=> On
     AB32SW                                   California AB-32 Cap-and-Trade Switch 0=> Off 1=> On
     LEGIRA                                   Inflation Reduction Act Switch 0=> Off 1=> On
     CO2PLFMM                                 LFMM carbon tax markup Switch 0=> LFMM does not mark up prices 1=> LFMM marks up prices
     CRUDEXP                                  Crude export Switch 0=> no exports allowed 1=> exports allowed

     TAX_FLAG                                 EPMCNTL carbon tax switch 0=> Off -1=> On
     ELEC_FLAG                                EPMCNTL Electricity-Only carbon tax switch 0=> Off -1=> On
     TRAN_FLAG                                EPMCNTL Transportation-Only carbon tax switch 0=> Off -1=> On
;

*---- Load E85 demand curves
$gdxin E85.gdx
$Load P_E85STP_t, Q_E85STP_t, A_E85STP_t, Trill_Btu_to_MBBL_CD, D_MMBtu_to_D_BBL_CD, E85availLYR
$Load Trill_Btu_to_Bgal, P_E85_IC_t, P_E85_SC_t, MOGASPRICE, LCFSOPT, AB32SW, CO2PLFMM, CRUDEXP
$Load TAX_FLAG, ELEC_FLAG, TRAN_FLAG
$Load E85curveSwitch, E85_stepsLOOP, E85_ELAS, PoE85, QoE85, PMGTRadjSTEO
$Load LEGIRA
$gdxin
*----------------------------------------------------------------------------------------------------------

*TEMP TEMP TEMP TEMP TEMP
*if ((E85curveSwitch = 2),
*   E85delta(MNUMCR,MNXYR) = PoE85(MNUMCR,MNXYR) � TotalMarkups(t,'E','T',CenDiv)  ;
*elseif (E85curveSwitch = 1),
*   P_E85STP_t(E85_Steps,ActiveDem,t) = 0.70* P_E85STP_t(E85_Steps,ActiveDem,t) ;
*
*);
*TEMP TEMP TEMP TEMP TEMP


*-----------------------------------------------------------
* Set refinery input prices for the inputs that have regressions (87$ per bbl)
RefInpPrc(RefReg,NGLInputStr,'STEP01',ModelYears(t)) = CoprodAlpha(NGLInputStr)*ExpectedWOP(t) + CoprodBeta(NGLInputStr) + 0.001 ;

* Calculate coproduct prices in 87$ per BBL from the regression equations
CoproductPrice(Stream,RefReg,ModelYears(t))$CoprodAlpha(Stream) =
  CoprodAlpha(Stream)*ExpectedWOP(t) + CoprodBeta(Stream) ;
* Set still gas sales price very low so that it will use it for fuel instead of selling it
*CoproductPrice('PGSout',RefReg,ModelYears(t))  = 0.0001 ;

* Set up price for electricity sales to grid
CoproductPrice('KWGridOut',RefReg,ModelYears(t))$(ord(t)<=LastYear) =
   sum(MNUMYR$tMNUM(t,MNUMYR),
     sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
       RefReg_to_CenDiv_ACU_Frac(RefReg,ActiveDem)*UEFPOUT_PELBS(MNUMCR,MNUMYR)*3412.3/1000000.0 ) );
CoproductPrice('KWGridOut',RefReg,ModelYears(t))$(ord(t)>LastYear) =
  sum(tt$(ord(tt)=LastYear), CoproductPrice('KWGridOut',RefReg,tt) ) ;

* Set RefReg9 wholesale electricity price to RefReg1 for now
CoproductPrice('KWGridOut','9_RefReg',ModelYears(t)) = CoproductPrice('KWGridOut','1_RefReg',t) ;

* Define ethanol co-product values
* 3-30-20, esh: added in DDGout price using average of 2016-2019 DDG/Corn Price ratio from biofuels dashboard
CoproductPrice('DDGout',DomRefReg,ModelYears(t)) = 1.014 * RefInpPrc(DomRefReg,'CRN','Step05',t) / 56 * 2000;
*CoproductPrice('LIGout',DomRefReg,ModelYears(t)) = 17.5 ;

* Put small value on other coproducts so that the model doesn't want to dump them
* CoproductPrice(Stream,RefReg,ModelYears(t)) = max(0.01,CoproductPrice(Stream,RefReg,t));

* Set price for dumping pet coke
* CoproductPrice('COKdump',RefReg,ModelYears(t)) = 0 ;
* ...replace above with below...
* ...using PMORE_PPCIN(MNUMCR,MNUMYR)...
* Hardcoded: COKdump 'export' price(87$/bfoe) = 95%*PPCIN*CFPCQ

CoproductPrice('COKdump',RefReg,ModelYears(t))$(ord(t)<=LastYear) =
   sum(MNUMYR$tMNUM(t,MNUMYR),
    sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
     RefReg_to_CenDiv_ACU_Frac(RefReg,ActiveDem)
     * 0.95 * (PMORE_PPCIN(MNUMCR,MNUMYR) - EMABLK_JPCIN(MNUMYR)$(CO2PLFMM=1))) ) * CONVFACT_CFPCQ ;

CoproductPrice('COKdump',RefReg,ModelYears(t))$(ord(t)>LastYear) =
   sum(tt$(ord(tt)=LastYear), CoproductPrice('COKdump',RefReg,tt) ) ;

* Get utility prices from NEMS *

* Natural gas (convert to $/FOEB)
UtilityPrice(RefReg,'NGS',ModelYears(t))$(ord(RefReg)<card(RefReg)) =
   sum(MNXYRS$tMNXYRS(t,MNXYRS),
     sum((CenDiv,MNUMCR)$(crN2L(CenDiv,MNUMCR) and RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv)),
       RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv)*MXPBLK_XPGIIN(MNUMCR,MNXYRS) ) ) * CONVFACT_CFRSQ ;
UtilityPrice(RefReg,'NGS',RunYears(t))$(ord(RefReg)<card(RefReg)) =
   sum(MNUMYR$tMNUM(t,MNUMYR),
     sum((CenDiv,MNUMCR)$(crN2L(CenDiv,MNUMCR) and RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv)),
       RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv)*MPBLK_PNGIN(MNUMCR,MNUMYR) ) ) * CONVFACT_CFRSQ ;

* Electricity (convert to $/kwh)
UtilityPrice(RefReg,'KWH',ModelYears(t))$(ord(RefReg)<card(RefReg)) =
   sum(MNXYRS$tMNXYRS(t,MNXYRS),
     sum((CenDiv,MNUMCR)$(crN2L(CenDiv,MNUMCR) and RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv)),
       RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv)*MXPBLK_XPELIN(MNUMCR,MNXYRS) ) ) * 0.0034123 ;
UtilityPrice(RefReg,'KWH',RunYears(t))$(ord(RefReg)<card(RefReg)) =
   sum(MNUMYR$tMNUM(t,MNUMYR),
     sum((CenDiv,MNUMCR)$(crN2L(CenDiv,MNUMCR) and RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv)),
       RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv)*MPBLK_PELIN(MNUMCR,MNUMYR) ) ) * 0.0034123 ;

* Set Canadian/Caribbean utility prices to RefReg1 for now
UtilityPrice('9_REFREG','NGS',ModelYears(t)) = UtilityPrice('1_REFREG','NGS',t) ;
UtilityPrice('9_REFREG','KWH',ModelYears(t)) = UtilityPrice('1_REFREG','KWH',t) ;

* NAK Make sure the price for electricity sold to the grid is cheaper than purchased electricity
CoproductPrice('KWGridOut',RefReg,ModelYears(t)) =
  min(CoproductPrice('KWGridOut',RefReg,t), UtilityPrice(RefReg,'KWH',t) - 0.01) ;


* Consolidate all markups into one per fuel, sector, CenDiv, year in 87$/MMBTU
TotalMarkups(ModelYears(t),MarkupFuel,MarkupSector,ActiveDem)$(ord(t)<=(AEOLSTYR-1989)) =
   ProductMarkups(t,MarkupFuel,MarkupSector,ActiveDem) +
   StateFuelTax(t,MarkupFuel,MarkupSector,ActiveDem) +
  (FedFuelTax(t,MarkupFuel)/GDP(t)) +
  (EnvMarkups(ActiveDem,MarkupFuel)/CFMGQCD(ActiveDem,t))$MarkupFuel('M') +
  (EnvMarkups(ActiveDem,MarkupFuel)/CONVFACT_CFDSQ)$MarkupFuel('D') +
  (EnvMarkups(ActiveDem,MarkupFuel)/sum(MNUMYR$tMNUM(t,MNUMYR),CONVFACT_CFJFQ(MNUMYR)))$MarkupFuel('J') ;

TotalMarkups(ModelYears(t),MarkupFuel,MarkupSector,ActiveDem)$(ord(t)>(AEOLSTYR-1989)) =
   sum(tt$(ord(tt)=(AEOLSTYR-1989)), TotalMarkups(tt,MarkupFuel,MarkupSector,ActiveDem) ) ;

TotalMarkups(ModelYears(t),MarkupFuel,MarkupSector,ActiveDem)$(ord(t)<(NCNTRL_CURCALYR-1989)) = 0 ;


* Add in carbon taxes, both national and AB-32 in 87$ per barrel

* Convert national carbon price from $87 per kg C to $87 per tonne CO2
CO2EmissionTax(t) = 0 ;
if ( ((TAX_FLAG=-1) and (ELEC_FLAG=0) and (TRAN_FLAG=0)),
   CO2EmissionTax(ModelYears(t)) = sum(MNUMYR$tMNUM(t,MNUMYR), EMISSION_EMETAX('01_M15',MNUMYR) ) * 1000 * (12/44) ;
   CO2EmissionTax(ModelYears(t))$(ord(t)>LastYear) = sum(tt$(ord(tt)=LastYear), CO2EmissionTax(tt) ) ;
);

* Convert AB-32 allowance price from $87 per kg C to $87 per tonne CO2
AB32_AllowPrice(ModelYears(t)) = sum(MNUMYR$tMNUM(t,MNUMYR), AB32_AB_ALLOW_P(MNUMYR) ) * 1000 * (12/44) ;
AB32_AllowPrice(ModelYears(t))$(ord(t)>LastYear) = sum(tt$(ord(tt)=LastYear), AB32_AllowPrice(tt) ) ;

* Set allowance price to zero before 2013
AB32_AllowPrice(t)$(t.val<2013) = 0.0 ;

Parameters
  MG_BTUShare_E10(t)            BTU share of E10 gasoline that is pure gas
  MG_BTUShare_E15(t)            BTU share of E15 gasoline that is pure gas
  MG_BTUShare_B16(t)            BTU share of B16 gasoline that is pure gas
  MG_BTUShare_E85(t)            BTU share of E85 gasoline that is pure gas
  MG_Share_US                   National share of petroleum in motor gasoline

  CA_dmd_shr_MG                 California share of census 9 mogas demand
  CA_dmd_shr_DS                 California share of census 9 distillate demand
  CA_dmd_shr_JF                 California share of census 9 jet fuel demand
  CA_dmd_shr_E85                California share of census 9 E85 demand
  CA_dmd_shr_LPG                California share of census 9 LPG demand
;

MG_BTUShare_E10(ModelYears(t)) = 0.90*CFRBOB(t) / (0.90*CFRBOB(t) + 0.10*CFETQ(t)) ;
MG_BTUShare_E15(ModelYears(t)) = 0.85*CFRBOB(t) / (0.85*CFRBOB(t) + 0.15*CFETQ(t)) ;
MG_BTUShare_B16(ModelYears(t)) = 0.84*CFRBOB(t) / (0.84*CFRBOB(t) + 0.16*sum(MNUMYR$tMNUM(t,MNUMYR),CONVFACT_CFBIOBUTE(MNUMYR))) ;
MG_BTUShare_E85(ModelYears(t)) = PMMOUT_TRGNE85*CFRBOB(t) / (PMMOUT_TRGNE85*CFRBOB(t) + PMMOUT_ETHNE85*CFETQ(t)) ;

* code below will take data from LAST available year to calc CA_dmd_shr_** (eg, 2014 for aeo2017)

loop(MNUMYR$
    ( (QSBLK_QSMGTR('09_Pacific',MNUMYR)+QSBLK_QSMGCM('09_Pacific',MNUMYR)+QSBLK_QSMGIN('09_Pacific',MNUMYR))>0 ) ,
  CA_dmd_shr_MG  =
    (QSBLK_QSMGTR('10_Unused',MNUMYR)+QSBLK_QSMGCM('10_Unused',MNUMYR)+QSBLK_QSMGIN('10_Unused',MNUMYR) ) /
    (QSBLK_QSMGTR('09_Pacific',MNUMYR)+QSBLK_QSMGCM('09_Pacific',MNUMYR)+QSBLK_QSMGIN('09_Pacific',MNUMYR) );
);

* E85 SEDS data is missing, so use the mogas share
  CA_dmd_shr_E85 = CA_dmd_shr_MG ;

loop(MNUMYR$
    ( (QSBLK_QSDSTR('09_Pacific',MNUMYR)+QSBLK_QSDSCM('09_Pacific',MNUMYR)+QSBLK_QSDSRS('09_Pacific',MNUMYR)+
       QSBLK_QSDSIN('09_Pacific',MNUMYR))>0 ),
  CA_dmd_shr_DS =
      (QSBLK_QSDSTR('10_Unused',MNUMYR)+QSBLK_QSDSCM('10_Unused',MNUMYR)+QSBLK_QSDSRS('10_Unused',MNUMYR)+
       QSBLK_QSDSIN('10_Unused',MNUMYR) ) /
      (QSBLK_QSDSTR('09_Pacific',MNUMYR)+QSBLK_QSDSCM('09_Pacific',MNUMYR)+QSBLK_QSDSRS('09_Pacific',MNUMYR)+
       QSBLK_QSDSIN('09_Pacific',MNUMYR) );
);

loop(MNUMYR$
    ( (QSBLK_QSLGTR('09_Pacific',MNUMYR)+QSBLK_QSLGCM('09_Pacific',MNUMYR)+QSBLK_QSLGRS('09_Pacific',MNUMYR)+
       QSBLK_QSLGIN('09_Pacific',MNUMYR))>0 ),
  CA_dmd_shr_LPG =
      (QSBLK_QSLGTR('10_Unused',MNUMYR)+QSBLK_QSLGCM('10_Unused',MNUMYR)+QSBLK_QSLGRS('10_Unused',MNUMYR)+
       QSBLK_QSLGIN('10_Unused',MNUMYR) ) /
      (QSBLK_QSLGTR('09_Pacific',MNUMYR)+QSBLK_QSLGCM('09_Pacific',MNUMYR)+QSBLK_QSLGRS('09_Pacific',MNUMYR)+
       QSBLK_QSLGIN('09_Pacific',MNUMYR) );
);

loop(MNUMYR$
    ( QSBLK_QSJFTR('09_Pacific',MNUMYR)>0 ),
  CA_dmd_shr_JF  =
    QSBLK_QSJFTR('10_Unused',MNUMYR) / QSBLK_QSJFTR('09_Pacific',MNUMYR) ;
);

* Initialize
CarbonTax(Stream,CenDiv,t) = 0.0 ;

*-- National --*
* Comment out until tax switches from EPM are brought over *

* E10 mogas, tax as if pure gasoline because credit is given for ethanol blended
* 9-25-19 em4 TEMP:  however, rmvd ethanol credit, so apply as if to BOB portion

CarbonTax('CFGout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    0.90* EMABLK_JMGTR(MNUMYR)*CONVFACT_CFMGQ(MNUMYR) ) ;

CarbonTax('RFGout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    0.90* EMABLK_JMGTR(MNUMYR)*CONVFACT_CFMGQ(MNUMYR) ) ;

CarbonTax('CaRBOBout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    0.90* EMABLK_JMGTR(MNUMYR)*CONVFACT_CFMGQ(MNUMYR) ) ;

* E15 mogas, tax as if pure gasoline because credit is given for ethanol blended
* 9-25-19 em4 TEMP:  however, rmvd ethanol credit, so apply as if to BOB portion
CarbonTax('CFG15out',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    0.85* EMABLK_JMGTR(MNUMYR)*CONVFACT_CFMGQ(MNUMYR) ) ;

CarbonTax('RFG15out',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    0.85* EMABLK_JMGTR(MNUMYR)*CONVFACT_CFMGQ(MNUMYR) ) ;

* Bu16 mogas, tax as if pure gasoline because credit is given for biobutanol blended
* 9-25-19 em4 TEMP:  however, rmvd ethanol credit, so apply as if to BOB portion
CarbonTax('CFGb16out',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    0.84* EMABLK_JMGTR(MNUMYR)*CONVFACT_CFMGQ(MNUMYR) ) ;

* E85, tax as if pure gasoline because credit is given for ethanol blended
* 9-25-19 em4 TEMP:  however, rmvd ethanol credit, so apply as if to BOB portion
CarbonTax('E85out',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    0.26* EMABLK_JMGTR(MNUMYR)*CONVFACT_CFMGQ(MNUMYR) ) ;

* Distillate
CarbonTax('DSUout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMABLK_JDSTR(MNUMYR)*CONVFACT_CFDSQ ) ;

CarbonTax('DSLout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMABLK_JDSTR(MNUMYR)*CONVFACT_CFDSQ ) ;

CarbonTax('CarbDSUout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMABLK_JDSTR(MNUMYR)*CONVFACT_CFDSQ ) ;

CarbonTax('N2Hout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMABLK_JDSTR(MNUMYR)*CONVFACT_CFDSQ ) ;

* Jet fuel
CarbonTax('JTAout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMABLK_JJFTR(MNUMYR)*CONVFACT_CFJFK ) ;

* Set up carbon tax credits for biofuels
Parameter
  CarbonTaxCredit(Stream,t)      Biofuel tax offsets from national carbon tax
;

Set
  MGBioStreams(Stream)
  DSBioStreams(Stream)
  JFBioStreams(Stream)
  AllBioTaxStr(Stream)
;

MGBioStreams(Stream) = no ;
DSBioStreams(Stream) = no ;
JFBioStreams(Stream) = no ;
AllBioTaxStr(Stream) = no ;

MGBioStreams(EthStreamRFS) = yes ;
MGBioStreams('IBA')        = yes ;
MGBioStreams('BNL')        = yes ;
MGBioStreams('BNH')        = yes ;
MGBioStreams('BPN')        = yes ;
MGBioStreams('RNH')        = yes ;

DSBioStreams('GDS')        = yes ;
DSBioStreams('FBD')        = yes ;
DSBioStreams('BPD')        = yes ;
DSBioStreams('BDS')        = yes ;
DSBioStreams('RDH')        = yes ;

JFBioStreams('BKE')        = yes ;
JFBioStreams('RJH')        = yes ;

AllBioTaxStr(MGBioStreams) = yes ;
AllBioTaxStr(DSBioStreams) = yes ;
AllBioTaxStr(JFBioStreams) = yes ;

* Calculate the credits in 87$/bbl
CarbonTaxCredit(MGBioStreams,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMISSION_EMETAX('01_M15',MNUMYR)*EMEBLK_EMGTR(MNUMYR)*CONVFACT_CFMGQ(MNUMYR) ) ;

* 9-25-19 em4 TEMP: remove credit to eth streams

CarbonTaxCredit(EthStreamRFS,ModelYears) = 0.0;
CarbonTaxCredit('IBA',ModelYears) = 0.0;

* 9-25-19 em4 END TEMP change

CarbonTaxCredit(DSBioStreams,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMISSION_EMETAX('01_M15',MNUMYR)*EMEBLK_EDSTR(MNUMYR)*CONVFACT_CFDSQ ) ;

CarbonTaxCredit(JFBioStreams,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMISSION_EMETAX('01_M15',MNUMYR)*EMEBLK_EJFTR(MNUMYR)*CONVFACT_CFJFK ) ;

*IRA biofuel subsidy extension

set
  IRAbiosubYr(t) /2023, 2024/
  ETHCLEsubYr(t) /2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024/
  CleanFuelProductionYr(t) /2025, 2026, 2027/

if ((LEGIRA = 1  or LEGIRA = 2 or LEGIRA = 3),
   BiofuelSubsidy('RJH',IRAbiosubYr) = 1.25 ;
   BiofuelSubsidy('FBD',IRAbiosubYr) = 1.00 ;
   BiofuelSubsidy('RDH',IRAbiosubYr) = 1.00 ;
   BiofuelSubsidy('RNH',IRAbiosubYr) = 1.00 ;
   BiofuelSubsidy('ETHCLE',ETHCLEsubYr) = 1.01;
   BiofuelSubsidy('RJH',CleanFuelProductionYr) = 1.25 ;
   BiofuelSubsidy('FBD',CleanFuelProductionYr) = 1.00 ;
   BiofuelSubsidy('RDH',CleanFuelProductionYr) = 1.00 ;
   BiofuelSubsidy('RNH',CleanFuelProductionYr) = 1.00 ;
   BiofuelSubsidy('ETHCLE',CleanFuelProductionYr) = 1.01;
elseif (LEGIRA=0),
   BiofuelSubsidy(Stream,t) = BiofuelSubsidy(Stream,t) ;
);

* Convert biofuel subsidies to 87$/bbl from nominal dollars per gallon
BiofuelSubsidy(Stream,t) = BiofuelSubsidy(Stream,t)*42/GDP(t) ;

$ontext

* LPG
CarbonTax('LPGout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMISSION_EMETAX('01_M15',MNUMYR)*EMEBLK_ELGTR(MNUMYR)*CONVFACT_CFLGQ(MNUMYR) ) ;

* Resid - Low sulfur
CarbonTax('N6Iout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMISSION_EMETAX('01_M15',MNUMYR)*EMEBLK_ERSTR(MNUMYR)*CONVFACT_CFRSQ ) ;

* Resid - High sulfur
CarbonTax('N6Bout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMISSION_EMETAX('01_M15',MNUMYR)*EMEBLK_ERSTR(MNUMYR)*CONVFACT_CFRSQ ) ;

* Petrochemical feedstocks
CarbonTax('PCFout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMISSION_EMETAX('01_M15',MNUMYR)*EMEBLK_EPFIN(MNUMYR)*CONVFACT_CFRSQ ) ;

* Other petroleum - industrial (lubricants)
CarbonTax('LUBout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMISSION_EMETAX('01_M15',MNUMYR)*EMEBLK_EOTIN(MNUMYR)*CONVFACT_CFLUQ ) ;

* Other petroleum - transportation (aviation gasoline)
CarbonTax('AVGout',ActiveDem,ModelYears(t)) =
  sum(MNUMYR$tMNUM(t,MNUMYR),
    EMISSION_EMETAX('01_M15',MNUMYR)*EMEBLK_EOTTR(MNUMYR)*CONVFACT_CFOTQ(MNUMYR) ) ;

$offtext

*-- AB-32 --*

* E10 motor gas, markup only petroleum part for AB-32
CarbonTax('CFGout',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('CFGout')) = CarbonTax('CFGout',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('CFGout')*CA_dmd_shr_MG*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*CFCBOB(t)*MG_BTUShare_E10(t)*44/12/1000.0 )$(AB32SW)  ;

CarbonTax('RFGout',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('RFGout')) = CarbonTax('RFGout',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('RFGout')*CA_dmd_shr_MG*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*CFRBOB(t)*MG_BTUShare_E10(t)*44/12/1000.0 )$(AB32SW)  ;

* E10 CarBOB
CarbonTax('CaRBOBout',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('CaRBOBout')) = CarbonTax('CaRBOBout',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('CaRBOBout')*CA_dmd_shr_MG*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*CFCBQ(t)*MG_BTUShare_E10(t)*44/12/1000.0 )$(AB32SW)  ;

* E15 motor gas, markup only petroleum part for AB-32
CarbonTax('CFG15out',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('CFG15out')) = CarbonTax('CFG15out',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('CFG15out')*CA_dmd_shr_MG*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*CFCBOB(t)*MG_BTUShare_E15(t)*44/12/1000.0 )$(AB32SW)  ;

CarbonTax('RFG15out',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('RFG15out')) = CarbonTax('RFG15out',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('RFG15out')*CA_dmd_shr_MG*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*CFRBOB(t)*MG_BTUShare_E15(t)*44/12/1000.0 )$(AB32SW)  ;

* E15 motor gas, markup only petroleum part for AB-32
CarbonTax('CFGb16out',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('CFGb16out')) = CarbonTax('CFGb16out',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('CFGb16out')*CA_dmd_shr_MG*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*CFCBOB(t)*MG_BTUShare_B16(t)*44/12/1000.0 )$(AB32SW)  ;

* E85, markup only petroleum part for AB-32
CarbonTax('E85out',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('E85out')) = CarbonTax('E85out',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('E85out')*CA_dmd_shr_E85*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*CFCBOB(t)*MG_BTUShare_E85(t)*44/12/1000.0 )$(AB32SW)  ;

* Distillate
CarbonTax('DSUout',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('DSUout')) = CarbonTax('DSUout',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('DSUout')*CA_dmd_shr_DS*AB32_AllowPrice(t)*EMEBLK_EDSTR(MNUMYR)*CONVFACT_CFDSUQ(MNUMYR)*44/12/1000.0 )$(AB32SW)  ;

CarbonTax('DSLout',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('DSLout')) = CarbonTax('DSLout',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('DSLout')*CA_dmd_shr_DS*AB32_AllowPrice(t)*EMEBLK_EDSTR(MNUMYR)*CONVFACT_CFDSLQ(MNUMYR)*44/12/1000.0 )$(AB32SW)  ;

CarbonTax('CarbDSUout',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('CarbDSUout')) = CarbonTax('CarbDSUout',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('CarbDSUout')*CA_dmd_shr_DS*AB32_AllowPrice(t)*EMEBLK_EDSTR(MNUMYR)*CONVFACT_CFDSCQ(MNUMYR)*44/12/1000.0 )$(AB32SW)  ;

CarbonTax('N2Hout',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('N2Hout')) = CarbonTax('N2Hout',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('N2Hout')*CA_dmd_shr_DS*AB32_AllowPrice(t)*EMEBLK_EDSTR(MNUMYR)*CONVFACT_CFDSQ*44/12/1000.0 )$(AB32SW)  ;

* Jet fuel - kerosene
CarbonTax('JTAout',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('JTAout')) = CarbonTax('JTAout',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('JTAout')*CA_dmd_shr_JF*AB32_AllowPrice(t)*EMEBLK_EJFTR(MNUMYR)*CONVFACT_CFJFK*44/12/1000.0 )$(AB32SW)  ;

* LPG
CarbonTax('LPGout',ActiveDem('CenDiv9'),ModelYears(t))$(t.val>=AB32_StartYr('LPGout')) = CarbonTax('LPGout',ActiveDem,t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_CoverageFrac('LPGout')*CA_dmd_shr_LPG*AB32_AllowPrice(t)*EMEBLK_ELGTR(MNUMYR)*CONVFACT_CFPRQ*44/12/1000.0 )$(AB32SW)  ;

* Mark up natural gas price to refining for AB-32
UtilityPrice('7_RefReg','NGS',ModelYears(t))$((t.val>=AB32_StartYr('LPGout')) and AB32SW) = UtilityPrice('7_RefReg','NGS',t) +
  sum(MNUMYR$tMNUM(t,MNUMYR),
    AB32_AllowPrice(t)*EMEBLK_ENGIN(MNUMYR)*CONVFACT_CFRSQ*44/12/1000.0 ) ;


* Extend out past last model year if necessary
CarbonTax(Stream,ActiveDem,ModelYears(t))$(ord(t)>LastYear) =
   sum(tt$(ord(tt)=LastYear), CarbonTax(Stream,ActiveDem,tt) ) ;

CarbonTaxCredit(Stream,ModelYears(t))$(ord(t)>LastYear) =
   sum(tt$(ord(tt)=LastYear), CarbonTaxCredit(Stream,tt) ) ;

*---------------------------------------------------------------------------------------------
* Calculate weighted-average CO2 transport and storage costs by fuel region from CTS model
Set
  IGRP     / 1*100000 /

  UnitCost_att    source attributes in unitcost
   /  SCAP
      Total_Tonnes_Captured
      NPV_Tonnes_Captured
      Unit_Capture_Costs-Variable
      Total_Capture_Costs-Fixed
      Unit_Capture_Costs-Fixed
      Unit_Transport_Costs-Variable
      Total_Transport_Costs-Fixed
      Unit_Transport_Costs-Fixed
      Unit_Injection_Costs-Variable
      Total_Injection_Costs-Fixed
      Unit_Injection_Costs-Fixed
      Total_Cost_Per_Tonne
      Activation_Year     /
;

Parameter
  OGSMCO2Dem(OGCO2Reg,t)               OGSM EOR CO2 demand in M tons CO2 per day
  CO2_Avail(OGCO2Reg,CO2_Source,t)     Available CO2 Supply by OGSM region and CO2 source in Mtons CO2 per day
  CO2_Price(OGCO2Reg,CO2_Source,t)     CO2 price by source in each OGSM region (87$ per ton CO2)
  CO2_OGTranCost(OGCO2Reg,OGCO2Reg2,t) CO2 transport cost from OGMS to OGSM (87$ per ton CO2)
  FRtoSalineCost(FuelRegion,t)         CO2 transport cost to saline from each Fuel Region (87$ per ton CO2)
  FRtoEORCost(FuelRegion,OGCO2Reg,t)   CO2 transport cost to EOR from Fuel Region to OGSM region (87$ per ton CO2)
  CO2_SafetyPrice(OGCO2Reg,t)          CO2 safety valve cost (87$ per ton CO2)
;

Alias(M8,M8a) ;

CO2_SafetyPrice(OGCO2Reg,ModelYears(t)) = 500 ;

* 18 mmcf CO2 = 1 M ton CO2
OGSMCO2Dem(OGCO2Reg,ModelYears(t)) =
  sum((M8,M13,MNUMYR)$(OGCO2Reg_2_M8(OGCO2Reg,M8) and tMNUM(t,MNUMYR)),
    OGSMOUT_OGCO2PUR2(M8,M13,MNUMYR) ) / 18 / 365 ;

* Make sure every region has at least 1 unit of CO2 demand from OGSM
OGSMCO2Dem(OGCO2Reg,ModelYears(t)) =
  max(1,OGSMCO2Dem(OGCO2Reg,t));

OGSMCO2Dem(OGCO2Reg,ModelYears(t))$(ord(t)>LastYear) =
   sum(tt$(ord(tt)=LastYear), OGSMCO2Dem(OGCO2Reg,tt) ) ;

CO2_OGTranCost(OGCO2Reg,OGCO2Reg2,ModelYears(t)) =
  sum((M8,M8a)$(OGCO2Reg_2_M8(OGCO2Reg,M8) and OGCO2Reg_2_M8(OGCO2Reg2,M8a)),
    OGSMOUT_OGCO2TAR(M8,M8a) ) * 18 / GDP('2008') ;

CO2_Avail(OGCO2Reg,CO2_Source,ModelYears(t)) =
  sum((M8,M13,MNUMYR)$(OGCO2Reg_2_M8(OGCO2Reg,M8) and CO2_Source_2_M13(CO2_Source,M13) and tMNUM(t,MNUMYR)),
    OGSMOUT_OGCO2AVL(M8,M13,MNUMYR) ) / 18 / 365 ;

CO2_Avail(OGCO2Reg,CO2_Source,ModelYears(t))$(ord(t)>LastYear) =
   sum(tt$(ord(tt)=LastYear), CO2_Avail(OGCO2Reg,CO2_Source,tt) ) ;

CO2_Price(OGCO2Reg,CO2_Source,ModelYears(t)) =
  sum((M8,M13,MNUMYR)$(OGCO2Reg_2_M8(OGCO2Reg,M8) and CO2_Source_2_M13(CO2_Source,M13) and tMNUM(t,MNUMYR)),
    OGSMOUT_OGCO2PRC(M8,M13,MNUMYR) ) * 18 / GDP('2008') ;

CO2_Price(OGCO2Reg,CO2_Source,ModelYears(t))$(ord(t)>LastYear) =
   sum(tt$(ord(tt)=LastYear), CO2_Price(OGCO2Reg,CO2_Source,tt) ) ;

FRtoSalineCost(FuelRegion,ModelYears(t)) =
   sum((MAXNF2,MNUMYR)$(FuelRegion_2_MAXNF2(FuelRegion,MAXNF2) and tMNUM(t,MNUMYR)),
     UECPOUT_TnS_Costs(MAXNF2,MNUMYR) ) ;

FRtoSalineCost(FuelRegion,ModelYears(t))$(ord(t)>LastYear) =
   sum(tt$(ord(tt)=LastYear), FRtoSalineCost(FuelRegion,tt) ) ;

* Set saline storage cost to a small negative cost if there is no CO2 regulation in a given fuel region
* This basically means the CO2 is vented instead of going to saline
Loop((MAXNFR,MNMYRF,FuelRegion,ModelYears(t))$(tMNMYRF(t,MNMYRF) and FuelRegion_2_MAXNFR(FuelRegion,MAXNFR)),
  if(UECPOUT_MUST_STORE(MAXNFR,MNMYRF)=0,
     FRtoSalineCost(FuelRegion,t) = -0.01 ;
  );
);

FRtoEORCost(FuelRegion,OGCO2Reg,ModelYears(t)) =
   sum((MAXNFR,M8,MNUMYR)$(OGCO2Reg_2_M8(OGCO2Reg,M8) and FuelRegion_2_MAXNFR(FuelRegion,MAXNFR) and tMNUM(t,MNUMYR)),
     UECPOUT_FR_OR_TRANCOST(MAXNFR,M8,MNUMYR) ) ;

FRtoEORCost(FuelRegion,OGCO2Reg,ModelYears(t))$(ord(t)>LastYear) =
   sum(tt$(ord(tt)=LastYear), FRtoEORCost(FuelRegion,OGCO2Reg,tt) ) ;


*------------------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------------------
* for lfreport.gms

set DomM10(M10)                   Domestic refining regions NEMS indexes
                                               / 01_M10
                                                 02_M10
                                                 03_M10
                                                 04_M10
                                                 05_M10
                                                 06_M10
                                                 07_M10
                                                 08_M10  /
;

set
Gain_Streams(stream)       Set of all streams referenced in gain calculation
                           /Calif, Dilbit, H_Sour, H_Sweet, L_Sour, L_Sweet, M_Msour, M_Sour, Syncrude, UL_Sweet, Condensate
                            BDS, BKE, BNH, BNL
                            CBDS, CBKE, CBNH, CBNL
                            CDS, CKE, CNH, CNL
                            GDS, GKE, GNH, GNL
                            FCO, WGR, YGR
                            GO1, GO2, GO3, GO4, GO5, GO6, GO7, GO8, GO9
                            IC4, IC6
                            MN1, MN2, MN3, MN4, MN5, MN6, MN7, MN8, MN9
                            NGS, BPN, BPD
                            AL5, ALB, ALP
                            C5F, C5PH, C5PL, CC2ngl, CC3S, CC3U, CC3ngl
                            CGO1H, CGO1V, CGO3H, CGO3V, CGO4H, CGO4V, CGO6H
                            CGO6V, CGO7H, CGO7V, CGO9H, CGO9V, CGOH, CGOL
                            CHDSH, CHDSHD, CHDSHDA, CHDSL, CHDSLD, CHDSLDA, CHDSM, CHDSMD, CHDSMDA
                            CLDSH, CLDSHD, CLDSL, CLDSLD, CLDSM, CLDSMD, CLOH, CLOL
                            CNHH, CNHHD, CNHL, CNHLD,
                            CNLD, CNLH, CNLL, CNMD, CNMDB, CNMDT, CNMDTS, CNMH, CNML
                            COK, CPG
                            DAOH, DAOHH, DAOL, DAOLH, DEN
                            DFCD75, DFCD7505, DFCD7550, DFCD75M, DFCD75M05, DFCD75M50
                            DFCD95, DFCD9505, DFCD9550, DFCD95M, DFCD95M05, DFCD95M50
                            DFCH75, DFCH7505, DFCH7550, DFCH75M, DFCH75M05, DFCH75M50
                            DFCH95, DFCH9505, DFCH9550, DFCH95M, DFCH95M05, DFCH95M50
                            DFCL75, DFCL7505, DFCL7550, DFCL75M, DFCL75M05, DFCL75M50
                            DFCL95, DFCL9505, DFCL9550, DFCL95M, DFCL95M05, DFCL95M50
                            DS1, DS1D, DS1DA
                            DS2, DS2D, DS2DA
                            DS3, DS3D, DS3DA
                            DS4, DS4D, DS4DA
                            DS5, DS5D, DS5DA
                            DS6, DS6D, DS6DA
                            DS7, DS7D, DS7DA
                            DS8, DS8D, DS8DA
                            DS9, DS9D, DS9DA
                            DSNDH, DSNDL
                            FCD75, FCD7505, FCD7550, FCD75M, FCD75M05, FCD75M50
                            FCD95, FCD9505, FCD9550, FCD95M, FCD95M05, FCD95M50
                            FCH75, FCH7505, FCH7550, FCH75M, FCH75M05, FCH75M50
                            FCH95, FCH9505, FCH9550, FCH95M, FCH95M05, FCH95M50
                            FCL75, FCL7505, FCL7550, FCL75M, FCL75M05, FCL75M50
                            FCL95, FCL9505, FCL9550, FCL95M, FCL95M05, FCL95M50
                            GO1H, GO2H, GO3H, GO4H, GO5H, GO6H, GO7H, GO8H, GO9H
                            H2, H2U
                            HCBOTH, HCBOTL, HCDSTH, HCDSTL, HCHND, HCHNH, HCHNL, HCKERH, HCKERL
                            HCLNH, HCLNHS, HCLNL, HCLNLS, HCMND, HCMNH, HCMNL, HCND, HCNH, HCNL
                            HDS1, HDS1D, HDS1DA
                            HDS2, HDS2D, HDS2DA
                            HDS3, HDS3D, HDS3DA
                            HDS4, HDS4D, HDS4DA
                            HDS5, HDS5D, HDS5DA
                            HDS6, HDS6D, HDS6DA
                            HDS7, HDS7D, HDS7DA
                            HDS8, HDS8D, HDS8DA
                            HDS9, HDS9D, HDS9DA
                            HG1, HG2, HG3, HG4, HG5, HG6, HG7, HG8, HG9
                            HN1, HN1D
                            HN2, HN2D
                            HN3, HN3D
                            HN4, HN4D
                            HN5, HN5D
                            HN6, HN6D
                            HN7, HN7D
                            HN8, HN8D
                            HN9, HN9D
                            IC4C, IC4S, IC4U, IC4ngl
                            IC6H, IC6HT, IC6T
                            ISOP, ISOB, ISON, ISOT, ISOTB, ISOTN
                            KR1, KR1D
                            KR2, KR2D
                            KR3, KR3D
                            KR4, KR4D
                            KR5, KR5D
                            KR6, KR6D
                            KR7, KR7D
                            KR8, KR8D
                            KR9, KR9D
                            LCOH, LCOHD, LCOHDA
                            LCOL, LCOLD, LCOLDA
                            LCOM, LCOMD, LCOMDA
                            LG1, LG2, LG3, LG4, LG5, LG6, LG7, LG8, LG9
                            LN1, LN2, LN3, LN4, LN5, LN6, LN7, LN8, LN9
                            LNDHH, LNDHHB, LNDHHT, LNDHL, LNDHLB, LNDHLT, LNDHTS
                            LNDLH, LNDLHB, LNDLHT, LNDLL, LNDLLB, LNDLLT, LNDLTS
                            LUB, LUBX
                            MET, MNAP, MNDHH, MNDHL, MNDLH, MNDLL
                            NAPDA, NC4, NC4C, NC4S, NC4U, NC4ngl, NATngl, NATD
                            PGSS, PGSU
                            R100, R105, R90, R95, R95N, R95B
                            RAF100, RAF90, RAF95, RDH, RNH, FBD, RJH
                            RS100, RS105, RS90, RS95
                            SN1, SN2, SN3, SN4, SN5, SN6, SN7, SN8, SN9
                            SNDH, SNDL
                            TARH, TARL
                            UC3, UC3U, UC4, UC4C, UC4U
                            V100, V90, V95, V95N, V95B
                            VR1, VR2, VR3, VR4, VR5, VR6, VR7, VR8, VR9
                            VS100, VS90, VS95
                            VSW1, VSW2, VSW3, VSW4, VSW5, VSW6, VSW7, VSW8, VSW9
                            W100, W90, W95, W95N, W95B, WS100, WS90, WS95
                            BNZ, TOL, XYL
                            CBOB, CarbDSU, CaRBOB, CATC, CC3, DSL, DSU, JTA, N2H, N6B, N6I, PGS, RBOB
                            AR1, AR2, AR3, AR4, AR5, AR6, AR7, AR8, AR9, AR10, AR11
                            ASPHout, AVGout, BTXout, COKout, LUBout, PCFout, SULout
                            LTE1, LTE2, LTE3, LTE4, LTE5, LTE6, LTE7, LTE8, LTE9, LTE10, LTE11  /

Gain_Process(Process)       Processes to be included in Gain Calculation
                            / ACU, ALK, ARP, BSA, C4I, CPL, CSU, DC4, DC5, DDA, DDS, FCC
                              FDS, FGS, GDS, HCD, KRD, LNS, LUB, LTE
                              NDS, PHI, RCR, RSR, SDA, SGP, TRI, TRS, UGP, VCU /
;


set
  CO2Stream(Stream)           CO2 stream                               / CO2 /
  RecipeSteam(Stream)         Steam input stream for recipe blending   / STM /
;



set RFSFuelCredCat        Reporting categories for RFS credit reporting
                      / 'Corn Ethanol'
                        'Advanced Ethanol'
                        'Cellulosic Ethanol'
                        'Imported Ethanol'
                        'Biodiesel'
                        'Renewable Diesel'
                        'Renewable Gasoline'
                        'BTL - FT'
                        'BTL - Pyrolysis'
                        'CBTL'
                        'Safety Valve'
                        'Biobutanol'
                        'Category Total' /
;

set RFSFuelCredCat_M13(RFSFuelCredCat,M13)    Map RFS fuel reporting categories to M13 index for NEMS
                      / 'Corn Ethanol'.01_M13
                        'Advanced Ethanol'.02_M13
                        'Cellulosic Ethanol'.03_M13
                        'Imported Ethanol'.04_M13
                        'Biodiesel'.05_M13
                        'Renewable Diesel'.06_M13
                        'Renewable Gasoline'.07_M13
                        'BTL - FT'.08_M13
                        'BTL - Pyrolysis'.09_M13
                        'CBTL'.10_M13
                        'Safety Valve'.11_M13
                        'Biobutanol'.12_M13
                        'Category Total'.13_M13 /
;


set
CTLLiq(IntStream)           CTL output streams / CNL 'Coal Based FT Light Naphtha'
                                                 CNH 'Coal Based FT Heavy Naphtha'
                                                 CKE 'Coal Based FT Kerosene'
                                                 CDS 'Coal Based FT Diesel'  /

CBTLLiq(IntStream)          CBTL output streams /CBNL 'Coal and Biomass Based FT Light Naphtha'
                                                 CBNH 'Coal and Biomass Based FT Heavy Naphtha'
                                                 CBKE 'Coal and Biomass Based FT Kerosene'
                                                 CBDS 'Coal and Biomass Based FT Diesel'  /

BTLLiq(IntStream)           BTL output streams / BNL 'Biomass Based FT Light Naphtha'
                                                 BNH 'Biomass Based FT Heavy Naphtha'
                                                 BKE 'Biomass Based FT Kerosene'
                                                 BDS 'Biomass Based FT Diesel'  /

GTLLiq(IntStream)           GTL output streams / GNL 'NG Based FT Light Naphtha'
                                                 GNH 'NG Based FT Heavy Naphtha'
                                                 GKE 'NG Based FT Kerosene'
                                                 GDS 'NG Based FT Diesel'  /
;


Set

ImpStr(Stream)            Set of finished product import streams
                                              / ASPHout
                                                AVGout
                                                CaRBOBout
                                                CarbDSUout
                                                CFGout
                                                DSLout
                                                DSUout
                                                JTAout
                                                LUBout
                                                N2Hout
                                                N6Bout
                                                N6Iout
                                                PCFout
                                                RFGout
                                                CBOB
                                                RBOB
                                                MET
                                                AR3
                                                GO3
                                                MN3  /

ProdImpStr(Stream)        Set of finished product import streams
                                              / ASPHout
                                                AVGout
                                                CaRBOBout
                                                CarbDSUout
                                                CFGout
                                                COKout
                                                DSLout
                                                DSUout
                                                JTAout
                                                LUBout
                                                N2Hout
                                                N6Bout
                                                N6Iout
                                                PCFout
                                                RFGout  /

*BlendImpStr(Stream)       Set of blending import streams
*                                              / CBOB
*                                                RBOB /

UFOImpStr(ImpStr)         Set of unfinished oil import streams
                                              / AR3
                                                GO3
                                                MN3  /
;

set AltStream(Stream)             Alternative fuel streams except ethanols
                                 / BDS, BKE, BNH, BNL
                                   CDS, CKE, CNH, CNL
                                   GDS, GKE, GNH, GNL
                                   BPN, BPD, FBD, RNH, RDH, RJH, IBA  /
;

set EthCornBlends(RcpMode)        ethanol from corn
                                 / RCP_RFG10a
                                   RCP_RFG15a
                                   RCP_CFG10a
                                   RCP_CFG15a
                                   RCP_CaRBOBa
                                   RCP_E85a /
;

set

M4_2_CTL(M4,CTLLiq)         Map generic header to CTL liquid streams
                                               / 1_M4 . CNL
                                                 2_M4 . CNH
                                                 3_M4 . CKE
                                                 4_M4 . CDS    /

M4_2_CBTL(M4,CBTLLiq)         Map generic header to BTL liquid streams
                                               / 1_M4 . CBNL
                                                 2_M4 . CBNH
                                                 3_M4 . CBKE
                                                 4_M4 . CBDS   /

M4_2_BTL(M4,BTLLiq)         Map generic header to BTL liquid streams
                                               / 1_M4 . BNL
                                                 2_M4 . BNH
                                                 3_M4 . BKE
                                                 4_M4 . BDS   /

M4_2_GTL(M4,GTLLiq)         Map generic header to BTL liquid streams
                                               / 1_M4 . GNL
                                                 2_M4 . GNH
                                                 3_M4 . GKE
                                                 4_M4 . GDS   /




Parameters
     Gain_Detail(RefReg,RefType,Stream,t)                     Gain - Loss by Stream
     Gain_ToSpec(RefReg,RefType,Stream,t)                     Gain - Products into SpecBlending and Products out
     Crude_Detail(RefReg,RefType,Stream,t)                     Gain - Loss by Stream
     Total_Gain(Gain_Streams,Gain_Process)                    Total Gain - Loss by Process and Stream

*    Parameters needed to display net revenues for refineries
     nr_r_OpCost(RefReg,RefType,Process,t)                    Variable Operating Costs
     nr_q_OpCost(RefReg,RefType,Process,t)                    Activity of Processes
     nr_p_Stream(RefReg,RefType,Stream,t)                     Dual Value of Stream Balance Rows
     nr_q_Stream(RefReg,RefType,Stream,t)                     Net Flow on Stream Balances
     nr_p_Process(RefReg,RefType,Process,t)                   Dual Value of Capacity Balance Rows
     nr_q_Process(RefReg,RefType,Process,t)                   Net Activity of Processes
;





*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Table 21 items

Sets
  StreamType         Set of All Stream Types
   / Feedstocks, Intermediates, Products, EndProducts  /


  Industry           Set of All Industries
    / Petroleum, Biofuels, NonPetroleumFossil, Blending   /

  StreamNames        Set of all Stream Names
    /
    Crude
    DomesticCrude
    CrudeImports
    UFOImports
    GasolineImports
    GasolineExports
    BOBImports
    BOBExports
    IndustrialFeedNetImports
    DistillateImports
    DistillateExports
    ResidImports
    ResidExports
    ChemicalImports
    ChemicalExports
    Grains
    Cellulose
    Oils
    Methanol
    Electricity
    EthanolImports
    EthanolExports
    BiobasedDieselImports
    Coal
    Gas
    GasFeed
    GasFuel
    GasLiquids
    DomesticNGPL
    NGPLImports
    NGLExports
    LightGases
    Alkylate
    Reformate
    NaphthaLight
    NaphthaMedium
    NaphthaHeavy
    CatGasoline
    Isomerate
    PolyGasoline
    Naphtha
    Kerosene
    Diesel
    DieselHeavy
    LightCycleOil
    GasOilLight
    GasOil
    GasOilHeavy
    Resid
    Ethanol
    Distillate
    Chemicals
    NonFuelStreams
    Propane
    Gasoline
    CBOB
    RBOB
    CaRBOB
    ULSD
    LowSulfurDiesel
    CarbULSD
    HeatingOil
    JetFuel
    E10CFGEthanol
    E10RFGEthanol
    E15CFGEthanol
    E15RFGEthanol
    E10CarbRFGEthanol
    E85Ethanol
    B20DSU
    B20DSL
    B20CarbDSU
    B20N2H
    E10BOB
    E15BOB
    E85BOB
    NonFAMEDist
    B20Dist
    ResidFuel /

  CostType  Set of process dependent costs and levels
   /  FuelYield
      FeedUse
      CoproductYield
      Nameplate
      ProductionCapacity
      Production
      CapacityFactor
      BuildYears
      EquipmentCost
      FieldCost
      FixedCapitalInvestment
      BaseOvernight
      OvernightCapex
      TotalProjectCost
      ProjectInvestment
      AfterTaxCreditAnnuity
      BuildCost
      FeedstockCost
      FixedCapitalCost
      FIxedLaborCost
      FixedOpCost
      VariableOpCost
      ElectricityUse
      ElectricitySales    /
;



* Note: GasProd is identical to EndProductGas ????
set GasProd(RecipeProd)         Gasoline final products
 /   CFGout
     CFG15out
     RFGout
     RFG15out
     E85out
     CaRBOBout /
;

set DistProd(RecipeProd)        Distillate final products
 /   DSLout     'Low Sulfur Diesel'
     CarbDSUout 'California diesel'
     N2Hout     '#2 Heating oil'
     JTAout     'Jet Fuel'
     DSUout     'Ultra-low sulfur diesel' /
;

set Feedstocks(IntStream)       Feedstocks
  /   CRN
      GRN
      AGR
      ECR
      NFR
      UWW
      FCO
      WGR
      YGR
      COA
      NGS /
;

set FuelStreams(IntStream)      Liquid fuel streams produced by conversion units
  /   ETHCRN
      ETHCRNexp
      ETHGRN
      ETHAET
      ETHCLE
      BNL
      BNH
      BKE
      BDS
      BPN
      BPD
      CBNL
      CBNH
      CBKE
      CBDS
      CNL
      CNH
      CKE
      CDS
      GNL
      GNH
      GKE
      GDS
      IBA
      /
;

set NonFuelStreams(IntStream)   Streams produced by conversion units whose destination is other industries
  /  DDG
     GLY
     BCH
     BCK
     GCK
     SLG
     /
;


set RefinedProduct(Stream)      All products be they recipe or coproduct
  / set.RecipeProd, set.Coproduct /

set Chemicals(RefinedProduct)   Chemical products
 /  CC2out   'Ethane for sale to other industrial facilities'
    LPGout   'Propane for sale to other industrial facilities'
    UC3out   'Propylene for sale to other industrial facilities'
    GLYout   'Glycerine from biodiesel plants'
    NC4out   'n-butane for sale to other industrial facilities'
    IC4out   'isobutane for sale to other industrial facilities'
    NATout   'pentanes plus for sale to other industrial facilities'
    PCFout   'naphtha for chemical feedstock use'
    LUBout   'Base oils for sale to lube plants'
    ASPHout  'Petroleum based asphalt'
    COKout   'Petroleum coke'
    DDGout   'Dried distillers grains'
    DGSout   'Wet distillers grains' /
;


set LiquidChem(Chemicals)       Liquid chemical products
  /  CC2out   'Ethane for sale to other industrial facilities'
     LPGout   'Propane for sale to other industrial facilities'
     UC3out   'Propylene for sale to other industrial facilities'
     GLYout   'Glycerine from biodiesel plants'
     NC4out   'n-butane for sale to other industrial facilities'
     IC4out   'isobutane for sale to other industrial facilities'
     NATout   'pentanes plus for sale to other industrial facilities'
     PCFout   'naphtha for chemical feedstock use'
     LUBout   'Base oils for sale to lube plants'  /
;

set LiquefiedGases(LiquidChem) Light products which can be liqufied by increased pressure or temperature in industrial settings
  /  CC2out
     LPGout
     UC3out
     NC4out
     IC4out /

set RTLiquids(LiquidChem)       Chemicals that are liquid at room temperature
  / PCFout
    LUBout /

set Solids(Chemicals)           Solid petroleum products
  /  ASPHout  'Petroleum based asphalt'
     COKout   'Petroleum coke' /
;

set BioSolids(Chemicals)        Solid bio-based coproducts
  / DDGout
    DGSout /

sets
  BalanceRepLabel                  Labels for refinery balance report
    /'Crude Inputs'
     'Other Supply'
     'Other Non-Pet'
     'Refinery Gain'
     'Consumption'
     'Fuel Use'
     'Discrepency'  /

   NGLInputLabels                   Labels for NGL sources
    /'Refinery'
     'Gas Plant'
     'Imports' /

   NGLOutputLabels                  Labels for NGL outputs from LFMM
    /'Exports'
     'Industrial'
     'Refinery Use' /
;


sets
  Distillate(Stream)          Distillate intermediate products
  /  DSU      'EPA ultra-low sulfur diesel'
     DSL      'EPA low sulfur diesel'
     CarbDSU  'California ultra-low sulfur diesel'
     N2H      'EPA heating oil'
     JTA      'Spec Jet fuel'
     CDS      'Coal based diesel'
     CBDS     'Coal/biomass based diesel'
     GDS      'Natural gas based diesel'
     FBD      'FAME biodiesel'
     BDS      'Cellulosic diesel'
     RDH      'Hydrotreated renewable distillate'
     RJH      'HEFA-SPK renewable jet' /

  ResidFuel(RecipeProd)       #6 Heating Oils
    / N6Iout    'Intermediate sulfur'
      N6Bout    'High Sulfur' /

  IndustrialFeeds(Stream)     Petrochemical Feedstock streams
  /  CC2out 'Ethane'
     NC4out 'n-butane'
     NATout 'Natural gasoline'
     PCFout 'Naphtha for petrochemical use'
     LUBout 'Lubricants' /

  GrainCrops(IntStream)       Starch feedstocks
  /  CRN 'Corn'
     GRN 'Other grain based starches' /

  RenewableOils(IntStream)    Biobased diesel feedstocks
  /  FCO 'Virgin seed oils (e.g. soy, corn, and canola)'
     YGR 'High Free Fatty acid waste grease (e.g. Yellow Grease)'
     WGR 'Low Free Fatty acid waste grease  (e.g. Tallow)' /

  Coal(IntStream)             Coal and feedstocks
  /  COA 'Coal' /

  Gas(IntStream)              Dry natural gas feedstock
  / NGS 'Dry Natural Gas' /

  ChemIntermediates(IntStream) Chemical Intermediate streams
  / CC3 'Propane'
    NC4 'n-butane'
    IC4 'isobutane'
    NATngl /

  BioProcess(Process)         All biofuel process units
   / EDH 'Corn ethanol high yield'
     EDM 'Corn ethanol low yield'
     NCE 'Non-corn non-advanced ethanol'
     AET 'Non-corn advanced ethanol'
     FBD 'FAME Biodiesel'
     GDT 'Green diesel hydrotreater'
     SAF 'HEFA-SPK renewable jet or RDH'
     CLE 'Cellulosic ethanol'
     BTL 'Cellulosic distillate'
     BPU 'Cellulosic gasoline'
     IBA 'Biobutanol Pseudo-Unit' /

  NPFProcess(Process)         CTL and GTL Processes
   /  CTL 'Coal to Liquids process'
      CBL 'Coal/biomass to liquids'
      GTL 'Gas to liquids process' /

  BiofuelImports(IntStream)   Biofuel streams that can be imported
  /  ETHAET 'Sugarcane ethanol'
     FBD    'Fatty acid methyl ester biodiesel' /

  BioNaphtha(IntStream)       Biobased gasoline and chemical blendstocks
  /  BNH   'Biomass Based FT Heavy Naphtha'
     BPN   'Gasoline from light hydrocarbon based cellulosic biomass'
     ISOB  'C5/C6 Isomerate'
     ISOTB 'C5/C6 total recycle isomerate'
     R95B  'R Reformate 95 octane'
     V95B  'V Reformate 95 octane'
     W95B  'W Reformate 95 octane'
     RNH   'Gasoline from renewable oils'
     BNL   'Biomass based LPG/Petrochem feed' /

  BioDistillate(IntStream)    Biobased distillate streams
  /  FBD  'Fatty Acid Methyl Ester biodiesel'
     RDH  'Diesel from renewable oils'
     RJH  'HEFA-SPK renewable jet'
     BPD
     BDS  'Biomass based Diesel'
     BKE  'Biomass based Kerojet' /

  NPFNaphtha(IntStream)      Non-petroleum fossil gasoline and chemical blendstocks
  /  CNH   'Coal based heavy naphtha'
     CBNH  'Coal/biomass based heavy naphtha'
     GNH   'Natural gas based heavy naphtha'
     ISON
     ISOTN
     R95N
     V95N
     W95N
     CNL   'Coalbased LPG/petrochem feed'
     CBNL  'Coal/biomass based LPG/petrochem feed'
     GNL   'Natural gas based LPG/petrochem feed' /

  NPFDistillate(IntStream)        Non-petroleum fossil distillate streams
  /   CDS  'Coal based diesel'
      GDS  'Natural gas based diesel'
      CBDS
      CKE  'Coal based kerojet'
      GKE  'Natural gas based kerojet'
      CBKE   /

  BioNap(IntStream)             Biobased gasoline
  /  BNH   'Biomass Based FT Heavy Naphtha'
     BNL   'Biomass Based FT Light Naphtha'
     BPN   'Gasoline from pyrolysis'
     RNH   'Gasoline from renewable oils' /

  BioKero(IntStream)            Biobased kerosene streams
  /  BKE  'Biomass based Kerojet' /

  BioDist(IntStream)            Biobased distillate streams (excludes BKE)
  /  FBD  'Fatty Acid Methyl Ester biodiesel'
     RDH  'Diesel from renewable oils'
     RJH  'HEFA-SPK renewable jet'
     BPD  'Distillates from pyrolysis'
     BDS  'Biomass based Diesel' /

  AllDist(SpecProd)             Distillate streams
  /  N2H      'Heating oil'
     DSL      'Low sulfur distillate'
     DSU      'Ultra low sulfur distillate'
     CarbDSU  'California Air Resouuce Board distillate' /
;

set BioDieselStr(Stream)          Biodiesel stream
  / FBD /
;

set RenewDieselStr(Stream)          Renewable diesel stream
  / RDH /
;

Set E10(RecipeProd)               10% Ethanol blended gasoline streams
  / CFGout, RFGout, CaRBOBout /

Set E15(RecipeProd)               15% Ethanol blended gasoline streams
 / CFG15out, RFG15out /

Set Diesel(RecipeProd)            #2 Diesel blended products (including biodiesel and renewable diesel)
 / DSUout, DSLout, CarbDSUout /

set
  CRBOB(GasSpecProd)
 /CBOB,RBOB/
;



*alias(rmBlendGas, GasolineBlends);
set GasolineBlends(RCPMode)     Gasoline blends
 /RCP_CFG10a  'Conventional E10 from corn'
  RCP_CFG10b
  RCP_CFG10c  'Conventional E10 from cellulose'
  RCP_CFG10d  'Conventional E10 from non-corn advanced starch'
  RCP_CFG10e  'Conventional E10 from cellulose'
  RCP_CFG10f  'Conventional E10 from brazilian advanced'
  RCP_CFGb16  'Conventional Biobutanol 16%'
  RCP_RFG10a  'Reformulated E10 from corn'
  RCP_RFG10b
  RCP_RFG10c  'Reformulated E10 from cellulose'
  RCP_RFG10d  'Reformulated E10 from non-corn advanced starch'
  RCP_RFG10e
  RCP_RFG10f
  RCP_RFGb16  'Reformulated Biobutanol 16%'
  RCP_CFG15a  'Conventional E15 from corn'
  RCP_CFG15b  'Conventional E15 from cellulose'
  RCP_CFG15c  'Conventional E15 from cellulose'
  RCP_CFG15d  'Conventional E15 from non-corn advanced starch'
  RCP_CFG15e
  RCP_CFG15f  'Conventional E15 from non-corn advanced starch'
  RCP_RFG15a  'Reformulated E15 from corn'
  RCP_RFG15b
  RCP_RFG15c  'Reformulated E15 from cellulose'
  RCP_RFG15d  'Reformulated E15 from non-corn advanced starch'
  RCP_RFG15e
  RCP_RFG15f  'Reformulated from brazilian advanced'
  RCP_E85a    'E85 from corn'
  RCP_E85b
  RCP_E85c    'E85 from cellulose'
  RCP_E85d    'E85 from non-corn advanced starch'
  RCP_E85e
  RCP_E85f    'E85 from brazilian advanced'
  RCP_CaRBOBa 'California reformulated from corn'
  RCP_CaRBOBb
  RCP_CaRBOBc 'California reformulated from cellulose'
  RCP_CaRBOBd 'California reformulated from non-corn advanced starch'
  RCP_CaRBOBe
  RCP_CaRBOBf 'California reformulated from brazilian advanced' /
;

set
  E10Blends(GasolineBlends)   E10 blends
    /RCP_CFG10a  'Conventional E10 from corn'
     RCP_CFG10b  'Conventional E10 from US advanced'
     RCP_CFG10c  'Conventional E10 from cellulose'
     RCP_CFG10d  'Conventional E10 from non-corn advanced starch'
     RCP_CFG10e  'Conventional E10 for export from corn'
     RCP_CFG10f  'Conventional E10 from brazilian advanced'
     RCP_CFGb16  'Conventional Biobutanol 16%'
     RCP_RFG10a  'Reformulated E10 from corn'
     RCP_RFG10b  'Reformulated E10 from US advanced'
     RCP_RFG10c  'Reformulated E10 from cellulose'
     RCP_RFG10d  'Reformulated E10 from non-corn advanced starch'
     RCP_RFG10e  'Reformulated E10 for export from corn'
     RCP_RFG10f  'Reformulated E10 from brazilian advanced'
     RCP_RFGb16  'Reformulated Biobutanol 16%'
     RCP_CaRBOBa 'California reformulated from corn'
     RCP_CaRBOBb
     RCP_CaRBOBc 'California reformulated from cellulose'
     RCP_CaRBOBd 'California reformulated from non-corn advanced starch'
     RCP_CaRBOBe
     RCP_CaRBOBf 'California reformulated from brazilian advanced' /

  E15Blends(GasolineBlends)   E15 blends
    /RCP_CFG15a  'Conventional E15 from corn'
    RCP_CFG15b
    RCP_CFG15c  'Conventional E15 from cellulose'
    RCP_CFG15d  'Conventional E15 from non-corn advanced starch'
    RCP_CFG15e  'Conventional E15 for export from corn'
    RCP_CFG15f  'Conventional E15 from brazilian advanced'
    RCP_RFG15a  'Reformulated E15 from corn'
    RCP_RFG15b
    RCP_RFG15c  'Reformulated E15 from cellulose'
    RCP_RFG15d  'Reformulated E15 from non-corn advanced starch'
    RCP_RFG15e
    RCP_RFG15f  /
;


*set E10CFGBlends; alias(rmBlendGasCFG10,E10CFGBlends);
*set E10RFGBlends; alias(rmBlendGasRFG10,E10RFGBlends);
*set E10CarbRFGBlends; alias(rmBlendGasCarBOB10,E10CarbRFGBlends);


set E10CFGBlends(E10Blends)   10% ethanol blends into conventional gasoline
  /  RCP_CFG10a  'Conventional E10 from corn'
     RCP_CFG10b
     RCP_CFG10c  'Conventional E10 from cellulose'
     RCP_CFG10d  'Conventional E10 from non-corn advanced starch'
     RCP_CFG10e  'Conventional E10 from cellulose'
     RCP_CFG10f  'Conventional E10 from brazilian advanced'
     RCP_CFGb16  'Conventional Biobutanol 16%' /
;

set E10RFGBlends(E10Blends)   10% ethanol blends into reformulated gasoline
  /  RCP_RFG10a
     RCP_RFG10b
     RCP_RFG10c  'Reformulated E10 from cellulose'
     RCP_RFG10d  'Reformulated E10 from non-corn advanced starch'
     RCP_RFG10e
     RCP_RFG10f
     RCP_RFGb16  'Reformulated Biobutanol 16%' /
;


set E10CarbRFGBlends(E10Blends)   10% ethanol blends into conventional gasoline
  / RCP_CaRBOBa 'California reformulated from corn'
    RCP_CaRBOBb
    RCP_CaRBOBc 'California reformulated from cellulose'
    RCP_CaRBOBd 'California reformulated from non-corn advanced starch'
    RCP_CaRBOBe
    RCP_CaRBOBf 'California reformulated from brazilian advanced' /
;


set E15CFGBlends(GasolineBlends)   15% ethanol-motor gasoline blends
  /  RCP_CFG15a  'Conventional E15 from corn'
     RCP_CFG15b  'Conventional E15 from cellulose'
     RCP_CFG15c  'Conventional E15 from cellulose'
     RCP_CFG15d  'Conventional E15 from non-corn advanced starch'
     RCP_CFG15e
     RCP_CFG15f  'Conventional E15 from non-corn advanced starch' /
;


set E15RFGBlends(E15Blends)   15% ethanol-motor gasoline blends
  /  RCP_RFG15a  'Reformulated E15 from corn'
     RCP_RFG15b
     RCP_RFG15c  'Reformulated E15 from cellulose'
     RCP_RFG15d  'Reformulated E15 from non-corn advanced starch'
     RCP_RFG15e
     RCP_RFG15f  'Reformulated from brazilian advanced' /
;


set E85Blends(GasolineBlends)   Ethanol motor gasoline blends whose ethanol content averages 74% over the course of a year
  /  RCP_E85a    'E85 from corn'
     RCP_E85b
     RCP_E85c    'E85 from cellulose'
     RCP_E85d    'E85 from non-corn advanced starch'
     RCP_E85e
     RCP_E85f    'E85 from brazilian advanced' /
;


set DistBlends(RCPMode)         Distillate blends
  /RCP_CARBDSU0 'California ultralow sulfur petroleum diesel'
   RCP_CARBDSU1 'California ultralow sulfur B20'
   RCP_CARBDSU2 'California ultralow sulfur renewable diesel RDH'
   RCP_DSL0     'Low sulfur petroleum diesel'
   RCP_DSL1     'Low sulfur B20'
   RCP_DSU0     'Ultralow sulfur petroleum diesel'
   RCP_DSU1     'Ultralow sulfur B20'
   RCP_DSU2     'Ultralow sulfur renewable diesel RDH'
   RCP_JTA0     'Petroleum jet fuel'
   RCP_JTA1     'SAF 25% blend with petroleum jet fuel'
   RCP_JTA2     'SAF 10% blend with petroleum jet fuel'
   RCP_JTA3     'SAF 1% blend with petroleum jet fuel'
   RCP_N2H0     '#2 Petroleum petroleum heating oil'
   RCP_N2H1     '#2 Heating oil B20'
   RCP_N6I0
   RCP_N6B0  /
;


set NonFAMEBlends(DistBlends)   Distillate blends with no FAME biodiesel
  / RCP_CARBDSU0 'California ultralow sulfur petroleum diesel'
    RCP_DSL0     'Low sulfur petroleum diesel'
    RCP_DSU0     'Ultralow sulfur petroleum diesel'
    RCP_JTA0     'Petroleum jet fuel'
    RCP_JTA1     'SAF 25% blend with petroleum jet fuel'
    RCP_JTA2     'SAF 10% blend with petroleum jet fuel'
    RCP_JTA3     'SAF 1% blend with petroleum jet fuel'
    RCP_N2H0     '#2 Petroleum petroleum heating oil' /
;


set B20Blends(DistBlends)       Distillate blends with 20% FAME biodiesel
  / RCP_CARBDSU1 'California ultralow sulfur B20'
    RCP_DSL1     'Low sulfur B20'
    RCP_DSU1     'Ultralow sulfur B20'
    RCP_N2H1     '#2 Heating oil B20' /
;

set RDHBlends(DistBlends)       Distillate blends only with renewable diesel RDH
  / RCP_CARBDSU2 'California ultralow sulfur renewable diesel RDH'
    RCP_DSU2     'Ultralow sulfur renewable diesel RDH' /
;

set
  PetChemBlends(RcpMode)      Petrochemical blending modes                                            /
* RCP_PGSa  'Methane/still gas'
   RCP_CC3a  'Propane'
   RCP_UC3   'Propylene'
   RCP_NC4a  'n-butane'
   RCP_IC4a  'isobutane'
   RCP_C5P0  'C5 paraffins'
   RCP_C5P1  'C5 paraffins'
   RCP_C5P2  'C5 unsaturated mix'
   RCP_BNZ   'Benzene'
   RCP_TOL   'Toluene'
   RCP_XYL   'Xylene'
   RCP_SPN0  'Special naphtha'
   RCP_LUB   'Base oils for lubricants'
   RCP_GOP   'Gas oil as lube for air compressors'
   RCP_COKa  'Petroleum coke for sales'
   RCP_COKb  'Petroleum coke for disposal'
   RCP_SUL   'Elemental Sulfur for sales'
   RCP_ASPH0 'Asphalt from vacuum residue a'
   RCP_ASPH1 'Asphalt from vacuum residue b'
   RCP_ASPH2 'Asphalt from vacuum residue c'
   RCP_ASPH3 'Asphalt from low sulfur SDA tar'
   RCP_ASPH4 'Asphalt from high sulfur SDA tar' /

NPFChemBlends(RcpMode)      NonPetroleum fossil chemical blending modes
  / RCP_CC2   'Ethane'
    RCP_CC3b  'Propane'
    RCP_NC4b  'n-butane'
    RCP_IC4b  'isobutane'
    RCP_NAT   'Natural gasoline'
    RCP_SLG0  'Vitreous slag for sale' /

BioChemBlends(RcpMode)      Biochemical blending modes
  / RCP_DDG0  'Dried distillers grains'
    RCP_GLY0  'Glycerin from biodiesel plants'
    RCP_BCK0  'Biobased coke for sale/disposal'
    RCP_BCH0  'Biobased soil ammendment for sale/disposal' /

UnfinishedOils(Stream)      Unfinished oil streams for import and export
  / AR3       'Atmospheric Residuum'
    GO3       'Gas Oil'
    MN3       'Medium Naphtha' /


Gasoline(Stream)            Gasoline and blending component streams
  / CFGout    'Conventional gasoline'
    CaRBOBout 'California gasoline'
    RFGout    'Reformulated gasoline'
    CFG15out  'Conventional E15'
    RFG15out  'Reformulated E15'
    E85out    'E85' /
;

*CoProduct_Bal_Str(CoProduct) Co-product streams used for refinery balance calculation
*                            / COKout, GOPout, IC4out, LPGout, NC4out, PGSout, UC3out /

set
  CoProduct_Bal_Str(CoProduct) Co-product streams used for refinery balance calculation
    / GOPout, COKdump /
;


Parameters
   repMassBalance(Process,ProcessMode)                              Mass balance check for each ProcessMode
   repMassBalanceIn(Process,ProcessMode)                            Mass inputs for each ProcessMode
   repMassBalanceOut(Process,ProcessMode)                           Mass outputs for each ProcessMode

   repProcModeVolBal(Process,ProcessMode)                           Volume gain by process and mode
   repPetOutput(RefReg,Stream,Period,t)                             Refinery output streams used for balance calculation (M bbl per day)
   repPetFuelUse(RefReg,Stream,Period,t)                            Refinery fuel-use streams used for balance calculation (M bbl per day)
   repProcessGain(RefReg,Process,Period,t)                          Refinery process gain used for balance calculation (M bbl per day)
   repOtherSupply(RefReg,Stream,Period,t)                           Refinery other petroleum input streams used for balance calculation (M bbl per day)
   repNonPetOther(RefReg,Stream,Period,t)                           Refinery other non-petroleum input streams used for balance calculation (M bbl per day)
   repDomesticCrudeSup(RefReg,Crude,Period,t)                       Refinery dom crude production used for reporting (M bbl per day)
   repCrudeImports(RefReg,Crude,Period,t)                           Refinery crude imports streams used for reporting (M bbl per day)
   repCrudeExports(RefReg,Crude,Period,t)                           Refinery crude exports streams used for reporting (M bbl per day)
   repCrudeInputs(RefReg,Crude,Period,t)                            Refinery crude input streams used for balance calculation (M bbl per day)

   repCrudeInputsTotal(Period,t)                                    National crude use total (MM bbl per day)
   repOtherSupplyTotal(Period,t)                                    National other supply total (MM bbl per day)
   repNonPetOtherTotal(Period,t)                                    National non-petroleum supply total (MM bbl per day)
   repProcessGainTotal(Period,t)                                    National refinery gain total (MM bbl per day)
   repPetOutputTotal(Period,t)                                      National output total (MM bbl per day)
   repPetFuelUseTotal(Period,t)                                     National fuel use total (MM bbl per day)
   repBalanceRep(BalanceRepLabel,Period,t)                          Aggregated refinery balance report (MM bbl per day)
   repNGLSource(NGLInputLabels,Period,t)
   repNGLConsumption(NGLOutputLabels,Period,t)

   repDiscrepency(Period,t)                                         Refinery discrepency (supply - consumption)
   repUtilization(Process,RefReg,RefType,Period,t)                  Process Utilization Report [0-1]
   repActivity(RefReg,RefType,Stream,Process,ProcessMode,Period,t)  Activity Report
   repCO2Emissions(RefReg,RefType,Period,t)                         Refinery CO2 emissions by supply region and refinery type (M tons per day)
   repTotalCO2(Period,t)                                            National C02 emissions (M tons per day)
   repRFSCredits(RFSCategory,RFSFuelCredCat,Period,t)               RFS credits by category and biofuel in billion credits per year
   repRefWSPrice(RefReg,RefType,RecipeProd,Period,t)                Wholesale RefReg-level product prices in ReportYr dollars per BBL
   repCenWSPrice(CenDiv,EndProduct,Period,t)                        Wholesale CenDiv-level product prices in ReportYr dollars per BBL
   repSpecProdPrice(RefReg,RefType,SpecProd,Period,t)               Wholesale RefReg-level spec-blended product prices in ReportYr dollars per BBL
   repRefRefTran(RefReg,RefRegA,Stream,Period,t)                    RefReg-RefReg transfers in M BBL per day
   repRefRefTranPLL(RefReg,RefRegA,Stream,Period,t)                 RefReg-RefReg transfers in M BBL per day
   repRefRefTranCPL(RefReg,RefRegA,Stream,Period,t)                 RefReg-RefReg transfers in M BBL per day
   repRefRefTranModeCapUtz(M3,RefReg,RefRegA,TranMode,t)            RefReg-RefReg 1 transfers 2 cap 3 utz in M BBL per day
   repRefRefTranMode(RefReg,RefRegA,TranMode,t)                     RefReg-RefReg transfers by mode in M BBL per day
   repREFtoREFTranCap(RefReg,RefRegA,TranMode,t)                    RefReg-RefReg transport capacity by tran mode by year
   repRefCDTranModeCapUtz(M3,RefReg,CenDiv,NonMarineMode,t)         RefRef-CD 1 transfers 2 cap 3 utz in M BBL per day
   repRefCDTranMode(RefReg,CenDiv,NonMarineMode,t)                  RefReg-CD transfers by mode in M BBL per day
   repRefCDTranCap(RefReg,CenDiv,NonMarineMode,t)                   RefReg-CD transport capacity by tran mode by year

   repRefCenTran(RefReg,CenDiv,RecipeProd,Period,t)                 RefReg-CenDiv transfers in M BBL per day
   repRefProd(RefReg,RefType,RecipeProd,Period,t)                   Production levels in M BBL per day
   repRefH2Use(RefReg,RefType,Period,t)                             Refinery hydrogen purchases in TBtu
   repRefFuelUse(RefReg,RefType,Period,t)                           Refinery fuel use (trills per year)
   repRefElecPurch(RefReg,RefType,Period,t)                         Refinery electricity purchases (trills per year)
   repTotEnergyUse(RefReg,RefType,Period,t)                         Total refinery energy use (trills per year)
   repMargCrdPrice(RefReg,Crude,Period,t)                           Crude prices in ReportYr dollars per BBL
   repCrudeUse(RefReg,RefType,Crude,Period,t)                       Crude use in M BBL per day
   repCrudeRAC(RefReg,RefType,Period,t)                             Crude RAC price in ReportYr dollars per BBL
   repRACMargins(RefReg,RefType,SpecProd,Period,t)                  Spec-product RAC margins in ReportYr dollars per BBL
   repWTIMargins(RefReg,RefType,SpecProd,Period,t)                  Spec-product WTI margins in ReportYr dollars per BBL
   repSpecProp(RefReg,RefType,SpecProd,Property,Period,t)           Specification product properties
   repEnergyBalance(Process,ProcessMode)                            Energy balance check for each ProcessMode (MMBTU)  (does not work for alt fuels yet!)
   repWSAltFuelPrc(RefReg,Stream,Period,t)                          Alternative fuel stream marginal prices in ReportYr dollars per BBL
   repRFSMarkups(RFSCategory,EndProduct,Period,t)                   RFS markups by category by fuel in ReportYr dollars per BBL
;


parameters
   MMBPD_TO_BCF
   MMBPD_TO_BTONS
   repStreamFlows(DomRefReg,Stream,StreamNames,StreamType,Industry,t) FTAB Table 21: Liquid fuels consumption by sector and source
   repProcesses(RefReg,Process,Industry,CostType,t)                Costs ascribed to the objective function for a unit of activity on a process and mode
   repStreamPrices(RefReg,Stream,StreamNames,StreamType,Industry,t) Value ascribed to each stream by the LP (Balance row shadow prices)
   repStreamProperties(Stream,StreamNames,StreamType,Industry,Property)
;

MMBPD_TO_BCF = 365 * 42 / 7.48 / 1000 ;
MMBPD_TO_BTONS = 62.4 / 7.48 * 42 / 2000 * 365 / 1000 ;

Parameters
   TotalCFG                                                         Total national volume of CFG10+CFG15+CFGb16
   TotalRFG                                                         Total national volume of RFG10+RFG15+RFGb16
   TotalCaRBOB                                                      Total national volume of CaRBOB
   TotalDistAll                                                     Total distillate
   TotalDistPet                                                     Total petroleum-based distillate
   TotalDistBio                                                     Total biodistillate in distillate
   TotalKeroBio                                                     Total biokerosene in distillate
   TotalDistN2H                                                     Total regular distillate
   TotalDistDSL                                                     Total low sulfur distillate
   TotalDistDSU                                                     Total ultra low sulfur distillate
   TotalDistCarb                                                    Total CARB distillate
   TotalDistPetN2H                                                  Total petroleum-based regular distillate
   TotalDistPetDSU                                                  Total petroleum-based ultra low sulfur distillate
   TotalDistPetDSL                                                  Total petroleum-based low sulfur distillate
   TotalDistPetCarb                                                 Total petroleum-based CARB distillate
   TotalDistNonN2H                                                  Total non-petroleum-based regular distillate
   TotalDistNonDSU                                                  Total non-petroleum-based ultra low sulfur distillate
   TotalDistNonDSL                                                  Total non-petroleum-based low sulfur distillate
   TotalDistNonCarb                                                 Total non-petroleum-based CARB distillate
   TotalDistBKEN2H                                                  Total non-petroleum-based regular kerosene
   TotalDistBKEDSU                                                  Total non-petroleum-based ultra low sulfur kerosene
   TotalDistBKEDSL                                                  Total non-petroleum-based low sulfur kerosene
   TotalDistBKECarb                                                 Total non-petroleum-based CARB kerosene
   TotalE85                                                         Total national volume of E85
   TotalCTL                                                         Total national volume of CTL
   TotalBTL                                                         Total national volume of BTL
   TotalGTL                                                         Total national volume of GTL
   TotalCBTL                                                        Total national volume of CBTL

   TempSum
;


Parameters
  BiomassHeatContent(BioStr)    Higher Heating Value of various types of biomass
    / UWW 8600
      ECR 7700
      AGR 8600
      NFR 8600  /
  CoalHeatContent(CoalStr)      Higher Heating Value of Coal
    / COA 8000 /
  GasHeatContent(Gas)        Higher Heating Value of Natural Gas
    / NGS 1000  /
;



* Sets defining streams (originally just for reporting)
* These subsets are also used to build-up other sets prior to creation of lfminset.gdx via lfinput.gms,
* but that is separate from a NEMS run and lfprep.gms

Sets
   LightGases(IntStream)
   NaphthaLightPet(IntStream)
   NaphthaLightNPF(IntStream)
   NaphthaLightBiofuels(IntStream)
   NaphthaMedium(IntStream)
   NaphthaHeavyPet(IntStream)
   ReformatePet(IntStream)
   Alkylate(IntStream)
   GasolineCat(IntStream)
   IsomeratePet(IntStream)
   GasolinePoly(IntStream)
   NaphthaHeavyNPF(IntStream)
   NaphthaHeavyBiofuels(IntStream)
   ReformateNPF(IntStream)
   ReformateBiofuels(IntStream)
   IsomerateNPF(IntStream)
   IsomerateBiofuels(IntStream)
   KerosenePet(IntStream)
   DieselPet(IntStream)
   DieselHeavy(IntStream)
   LightCycleOil(IntStream)
   KeroseneNPF(IntStream)
   KeroseneBiofuels(IntStream)
   DieselNPF(IntStream)
   DieselBiofuels(IntStream)
   GasOilLight(IntStream)
   GasOil(IntStream)
   GasOilHeavy(IntStream)
   ResidualOil(IntStream)
   PetNaphtha(IntStream)
   PetDistillate(IntStream)
   PetResid(IntStream)
;

Sets
  AltFuelStream(Stream)
;

$gdxin %DirPath%\input\%INPUT_FILE_SET%
$loaddc LightGases, NaphthaLightPet, NaphthaLightNPF, NaphthaLightBiofuels,
$loaddc NaphthaMedium, NaphthaHeavyPet, ReformatePet, Alkylate, GasolineCat, IsomeratePet, GasolinePoly,
$loaddc NaphthaHeavyNPF, NaphthaHeavyBiofuels, ReformateNPF, ReformateBiofuels, IsomerateNPF, IsomerateBiofuels,
$loaddc KerosenePet, DieselPet, DieselHeavy, LightCycleOil,
$loaddc KeroseneNPF, KeroseneBiofuels, DieselNPF, DieselBiofuels,
$loaddc GasOilLight, GasOil, GasOilHeavy, ResidualOil,
$loaddc PetNaphtha, PetDistillate, PetResid, AltFuelStream
$gdxin

* End of Table 21 items
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------

Parameters
  ProductPrices(Stream,RefReg,Period)
  ProductQuantities(Stream,RefReg,Period)
  ProfitPerBBL(MNUMPR,Period)
  TotalCrude(RefReg,Period)
;

Scalar
  trancost  'Placeholder for crude imp and exp transport cost'
;

Parameters
  LFMMOUT_P_RFCRUDEINP(MNUMPR,MNCRUD,MNUMYR)     Crude oil refinery value by refinery region and type in 87$ per bbl
;

parameter
  CrudeBalance_M_LP(DomRefReg,Crude,Period)     nominal dollars per barrel
  CrudeBalance_M_DELTA(RefReg,Crude,Period)     nominal dollars per barrel
  CrudeBalance_M_ALPHA(DomRefReg,Crude,Period)  scaling factor
  CrudeBalance_M_BETA                           scaling factor
;
CrudeBalance_M_ALPHA(DomRefReg,Crude,PrcPeriod) = 0.0;
CrudeBalance_M_DELTA(RefReg,Crude,Period) = 0.0;


*---------------------------------------------------------------------------
parameter
  CrudeWHP_rent(DomRefReg,Crude,t)          'phased-in reduction in domestic wellhead pr, 1987$/bbl'
;
Scalar
* 2013$/bbl
   TargetRENT     / 6.90 /
   TargetRENTcond / 1.00 /
;

* set rent=0 in 1990-2014
* set rent=0 in 2022+ only for 50+API (0 $/bbl in 2013 dollars)
* set rent=linear transition from 0 to 0 in 2014-2022

  CrudeWHP_rent(DomRefReg,Crude,t)   = 0. ;

  if(LFMMOUT_lfREFRENT >0,

* HARDCODED
* set rent=0 in 1990-2014
* set rent=1 in 2022+ only for 50+API  (1 $/bbl in 2013 dollars)
* set rent=linear transition from 0 to 1 in 2014-2022
* set rent=TargetRENT in 2022+ all other crudes (6.9 $/bbl in 2013 dollars)
* set rent=phase in of linear transition from 0 to 6.9 in 2014-2022

    CrudeWHP_rent(DomRefReg,Crude,t)   = 0. ;
    CrudeWHP_rent(DomRefReg,Crude,t)   = TargetRENT/gdp('2013') ;
    CrudeWHP_rent(DomRefReg,Crude,t)$(ord(t)<25) = 0. ;
    CrudeWHP_rent(DomRefReg,Crude,'2014') = 0. ;
    CrudeWHP_rent(DomRefReg,Crude,'2015') = (1 * (TargetRENT / (2022-2014))) /gdp('2013') ;
* phase in for 2016: +1.10 = 2.825
    CrudeWHP_rent(DomRefReg,Crude,'2016') =((2 * (TargetRENT / (2022-2014)))+1.10) /gdp('2013') ;
* phase in for 2017: +0.60 = 3.1875
    CrudeWHP_rent(DomRefReg,Crude,'2017') =((3 * (TargetRENT / (2022-2014)))+0.60) /gdp('2013') ;
* phase in for 2018: +0.30 = 3.75
    CrudeWHP_rent(DomRefReg,Crude,'2018') =((4 * (TargetRENT / (2022-2014)))+0.30) /gdp('2013') ;
    CrudeWHP_rent(DomRefReg,Crude,'2019') = (5 * (TargetRENT / (2022-2014))) /gdp('2013') ;
    CrudeWHP_rent(DomRefReg,Crude,'2020') = (6 * (TargetRENT / (2022-2014))) /gdp('2013') ;
    CrudeWHP_rent(DomRefReg,Crude,'2021') = (7 * (TargetRENT / (2022-2014))) /gdp('2013') ;

    CrudeWHP_rent(DomRefReg,'Condensate',t)   = TargetRENTcond/gdp('2013') ;
    CrudeWHP_rent(DomRefReg,'Condensate',t)$(ord(t)<25) = 0. ;
    CrudeWHP_rent(DomRefReg,'Condensate','2014') = 0. ;
    CrudeWHP_rent(DomRefReg,'Condensate','2015') = 1 * (TargetRENTcond / (2022-2014)) /gdp('2013') ;
    CrudeWHP_rent(DomRefReg,'Condensate','2016') = 2 * (TargetRENTcond / (2022-2014)) /gdp('2013') ;
    CrudeWHP_rent(DomRefReg,'Condensate','2017') = 3 * (TargetRENTcond / (2022-2014)) /gdp('2013') ;
    CrudeWHP_rent(DomRefReg,'Condensate','2018') = 4 * (TargetRENTcond / (2022-2014)) /gdp('2013') ;
    CrudeWHP_rent(DomRefReg,'Condensate','2019') = 5 * (TargetRENTcond / (2022-2014)) /gdp('2013') ;
    CrudeWHP_rent(DomRefReg,'Condensate','2020') = 6 * (TargetRENTcond / (2022-2014)) /gdp('2013') ;
    CrudeWHP_rent(DomRefReg,'Condensate','2021') = 7 * (TargetRENTcond / (2022-2014)) /gdp('2013') ;
  );

* Display  CrudeWHP_rent ;
*---------------------------------------------------------------------------


*---------------------------------------------------------------------------
* STEO benchmarking definitions

set STEO_Category  STEO volume benchmarking categories /
  NetCrudeImports
  TotalCrudeInputs
*  CrudeImports
*  CrudeExports
  NetUFOImports
*  UFOImports
*  UFOExports
  NetProductImports
*  ProductImports
*  ProductExports
/;

sets
  STEO_Years(t)       Years to apply STEO volume adjustments
  MNUMYR_STEOLYR(MNUMYR)        MNUMYR version of STEOLYR
;


scalars
  STEO_ApplyContraints  Flag indicating if STEO volume constraints are active
;

parameters
  STEO_PhaseOut(t)                                    STEO volume target phase-out factors
  STEO_Target(STEO_Category,t)                        STEO volume targets in M bbl per day
  STEO_Penalty(Step,t)                                STEO penalties for missing volume targets

  npv_STEO_Target(STEO_Category,Period)               NPV STEO volume targets in M bbl per day
  npv_STEO_PhaseOut(Period)                           NPV STEO volume target phase-out factors
  npv_STEO_Penalty(Step,Period)                       NPV STEO penalties for missing volume targets

  STEO_CONIPUS(MNUMYR)                                STEO Net Crude Imports in MM bbl per day
  STEO_CORIPUS(MNUMYR)                                STEO Crude Input to Refineries in MM bbl per day
  STEO_UONIPUS(MNUMYR)                                STEO Net UFO Imports in MM bbl per day
  STEO_PANIPUS(MNUMYR)                                STEO Net Product Imports in MM bbl per day
  STEO_MBNIPUS(MNUMYR)                                STEO Net BOB Imports in MM bbl per day
  STEO_OHNIPUS(MNUMYR)                                STEO Net Other Hydrocarbon and Oxygenate Imports in MM bbl per day

  HIST_RFQEXCRD(MNUMPR,MNUMYR)                        STEO Gross Crude Exports in M bbl per day
  HIST_RFQICRD(MNUMPR,MNUMYR)                         STEO Gross Crude Imports in MM bbl per day
  HIST_RFQEXPRDT(MNUMPR,MNUMYR)                       STEO Gross Product Exports in MM bbl per day
  HIST_RFPQIPRDT(MNUMPR,MNUMYR,M2)                    STEO Gross Product Imports in MM bbl per day
  HIST_BLDIMP(MNUMPR,MNUMYR)                          STEO Net BOB Imports in MM bbl per day
  HIST_RFPQUFC(MNUMPR,MNUMYR,M2)                      STEO Gross UFO Imports in MM bbl per day

  STEOLYR                                             Last year of STEO data
  HISTLYR                                             Last year of data read
  LFadjvolON                                          Global switch for STEO volume constraints from RFHIST.txt
  STEO_NumPhaseoutYrs                                 Number of STEO phase out years
  STEO_PhaseoutFactor                                 Phase out factor per year in percent divided by 100

  counter
;

*---- Load STEO and historical data
$gdxin STEO_Hist.gdx
$Loaddc STEO_CONIPUS, STEO_CORIPUS, STEO_UONIPUS, STEO_PANIPUS, STEO_MBNIPUS, STEO_OHNIPUS
$Loaddc HIST_RFQEXCRD, HIST_RFQICRD, HIST_RFQEXPRDT, HIST_RFPQIPRDT
$Loaddc HIST_BLDIMP, HIST_RFPQUFC, STEOLYR, HISTLYR, LFadjvolON
$Load STEO_NumPhaseoutYrs=NumPhaseoutYrs
$Load STEO_PhaseoutFactor=PhaseoutFactor
$gdxin

STEOLYR = STEOLYR + 1989 ;
HISTLYR = HISTLYR + 1989 ;

STEO_Years(t) = no ;
STEO_Years(ModelYears(t))$((t.val>=STEOLYR-1) and (t.val<=STEOLYR+STEO_NumPhaseoutYrs)) = yes ;

MNUMYR_STEOLYR(MNUMYR) = no ;
Loop( (t,MNUMYR) $tMNUM(t,MNUMYR),
  if (t.val = STEOLYR,
     MNUMYR_STEOLYR(MNUMYR) = yes ;
  );
);

STEO_Penalty(Step,ModelYears)     = 0 ;
*STEO_Penalty('STEP01',ModelYears) = smax((CenDiv,EndProduct,Step), GlobalImpPrice(CenDiv,EndProduct,Step,ModelYears)) ;
STEO_Penalty('STEP01',ModelYears) = 3 * CrudePriceTotal('STEP08',ModelYears) ;

STEO_PhaseOut(ModelYears) = 0 ;
STEO_PhaseOut(ModelYears(t))$((t.val=STEOLYR) or (t.val=STEOLYR-1)) = 0.9999 ;

counter = 1 ;
loop(STEO_Years(t)$(not ((t.val=STEOLYR) or (t.val=STEOLYR-1))),

  STEO_PhaseOut(t) = 1 - counter*STEO_PhaseoutFactor ;
  counter = counter + 1 ;
);

* Assign volume benchmarking targets
STEO_Target(STEO_Category,t) = 0 ;
Loop((ModelYears(t),MNUMYR)$(tMNUM(t,MNUMYR) and (t.val<=STEOLYR)),

  STEO_Target('NetCrudeImports',t)   = STEO_CONIPUS(MNUMYR) * 1000 ;
  STEO_Target('TotalCrudeInputs',t)  = STEO_CORIPUS(MNUMYR) * 1000 ;
  STEO_Target('NetUFOImports',t)     = STEO_UONIPUS(MNUMYR) * 1000 ;
  STEO_Target('NetProductImports',t) = STEO_PANIPUS(MNUMYR) * 1000 ;
*    STEO_PANIPUS(MNUMYR) * 1000 -
*    STEO_UONIPUS(MNUMYR) * 1000 -
*    STEO_MBNIPUS(MNUMYR) * 1000 -
*    STEO_OHNIPUS(MNUMYR) * 1000 ;
);

* Fill in years past the last STEO year
Loop((ModelYears(t),tt,MNUMYR)$(tMNUM(tt,MNUMYR) and (t.val>STEOLYR) and (tt.val=STEOLYR)),

  STEO_Target('NetCrudeImports',t)   = STEO_CONIPUS(MNUMYR) * 1000 ;
  STEO_Target('TotalCrudeInputs',t)  = STEO_CORIPUS(MNUMYR) * 1000 ;
  STEO_Target('NetUFOImports',t)     = STEO_UONIPUS(MNUMYR) * 1000 ;
  STEO_Target('NetProductImports',t) = STEO_PANIPUS(MNUMYR) * 1000 ;
*    STEO_PANIPUS(MNUMYR) * 1000 -
*    STEO_UONIPUS(MNUMYR) * 1000 -
*    STEO_MBNIPUS(MNUMYR) * 1000 -
*    STEO_OHNIPUS(MNUMYR) * 1000 ;
);











