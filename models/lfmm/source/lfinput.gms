$if not set DirPathSet $set DirPathSet '.\input\sets\'
$if not set DirPathDB $set DirPathDB '.\input\'
$if not set DBFile $set DBFile 'LFMMMaster.accdb'


*  Data Import Subroutine

* This module reads in data, not stored in NEMS, maintained instead in
* GMS files (text files), and a  database LFMMMaster.accdb
* The data read in goes into two gdx files:
*   lfminset.gdx - set data (except LFCS sets, for now)
*   LFMinput.gdx - parameter data (and LCFS sets, for now)



*----------------------------------------------------------------------------------------------------------
* Read set data from GMS files, dump to lfminset.gdx


*sets: t, tIdx, CapExpYr, ReportYr, Step, Period, PrcPeriod, BldPeriod
$include "%DirPathSet%SetMisc.gms"

*sets: Source, DomRefReg, NonDomRefReg (but NOT RefReg), CenDiv, ActiveDem, CoalDReg, CoalSReg,
* and mappings: CoalDReg_2_Census, NGPL_2_RefReg
$include "%DirPathSet%SetRegionality.gms"


$include "%DirPathSet%SetRefType.gms"


$include "%DirPathSet%SetProcess.gms"
$include "%DirPathSet%SetProcessMode.gms"


$include "%DirPathSet%SetSpecProd.gms"
$include "%DirPathSet%SetSpecProdProp.gms"
$include "%DirPathSet%SetProperty.gms"

*Sets: EndProduct, CoProduct, NGLProduct, RecipeProd, RecipeOut
$include "%DirPathSet%SetProduct.gms"

*Sets: Crude, RefInputStr, NGLInputStr, Purchase_Streams, AltPurchase_Streams, IntStream  ETC ETC
$include "%DirPathSet%SetIntStream.gms"


$include "%DirPathSet%SetStream.gms"

$include "%DirPathSet%SetRcpMode.gms"

$include "%DirPathSet%SetTranMode.gms"

$include "%DirPathSet%SetInvParam.gms"

$include "%DirPathSet%SetNFBaseYr.gms"

$include "%DirPathSet%SetProcessRisk.gms"

$include "%DirPathSet%SetBldStep.gms"

$include "%DirPathSet%SetLrn.gms"

$include "%DirPathSet%SetMarkup.gms"

$include "%DirPathSet%SetRFSCategory.gms"

$include "%DirPathSet%SetSector.gms"

execute_unload 'lfminset.gdx';

*$stop

$ontext
execute_unload "%DirPath%input\LFMinputset.gdx" RefType, Crude, Source,
* RefReg, NGPL_2_RefReg <-defined in lf_nem.gms
  Process, ProcessMode, Stream, IntStream,
  SpecProd, DistSpecProd, GasSpecProd, ResidSpecProd, GasProp, DistProp, ResidProp,
  Property,
  EndProduct,RecipeProd, NGLProduct, CoProduct, RecipeOut,
  TranMode, MarineMode, NonMarineMode,
  InvParam, NFBaseYr, BldRiskClass, ProcessRisk, BldStep,
  LearningProcess, LrnSpeed, LrnParam, LrnPhase,
  MarkupFuel, MarkupSector,
  FCOType,
  RcpMode, RFSCategory,
  CoalDReg_2_Census;
$offtext

*----------------------------------------------------------------------------------------------------------


*$stop


*----------------------------------------------------------------------------------------------------------
* Read parameter data from %DBFile%, dump to lfminput.gdx

* Set number of decimal places for rounding of data
$set RND 6

$onecho >cmd.txt
I="%DirPathDB%%DBFile%"
X=LFMinput.gdx

* Import parameters from database


* Import mode capacity factors
Q8=SELECT Process, Mode, Value FROM ProcessTables WHERE Stream = 'CAPCON' ;
P8=CapFactor

* Import other variable costs for process modes
Q9=SELECT Process, Mode, ROUND(Value,%RND%) FROM ProcessTables WHERE Stream = 'OVC' ;
P9=OpVarCost

* Import process table with crude type assignment
Q10=SELECT Stream, Crude,Process, Mode, ROUND(Value,%RND%) \
FROM ProcessTables WHERE Stream <> 'CAPCON' AND Stream <> 'OVC' \
AND Stream <> 'LOS' AND Stream <> 'CONV' ;
P10=ProcessTableCrude

* ExistingCapacity
Q11=SELECT RefReg, RefType, Process, Year, ROUND(sum(ExCap),%RND%) \
FROM ExistingCapacity GROUP BY RefReg, RefType, Process, Year ;
P11=ExistingCap


* Global Imports
Q12=SELECT Census, Input, Step, Year, ROUND(Price,%RND%) FROM GlobalImports ;
P12=GlobalImpPrice

Q21=SELECT Census, Input, Step, Year, UB FROM GlobalImports;
P21=GlobalImpSupply

* Supply Region Refinery/Ethanol Input Purchases

Q97=SELECT RefReg, Input, Step, Year, ROUND(Price,%RND%) FROM RefInpPurch;
P97=RefInpPrc

Q98=SELECT RefReg, Input, Step, Year, UB FROM RefInpPurch;
P98=RefInpSup

*RecipeBlending

Q23=SELECT RcpMode, Stream, Value FROM RecipeBlending WHERE Stream <> 'OVC';
P23=RecipeBlending


* Blend Properties tables

Q31=SELECT Stream, SpecProd, ROUND(Value,%RND%) FROM StreamSpecProd ;
P31=StreamSpecProd


Q33=SELECT Stream, Property, ROUND(Value,%RND%) FROM StreamProp ;
P33=StreamProp


* Specifications tables


Q41=SELECT SpecProd, Property, Year, ROUND(Value,%RND%) \
FROM PropSpecProdLB  WHERE SpecType = 'Gas';
P41=GasSpecMin

Q47=SELECT SpecProd, Property, Year, ROUND(Value,%RND%) \
FROM PropSpecProdUB WHERE SpecType = 'Gas' ;
P47=GasSpecMax

Q42=SELECT SpecProd, Property, Year, ROUND(Value,%RND%) \
FROM PropSpecProdLB WHERE SpecType = 'Dist'  ;
P42=DistSpecMin

Q48=SELECT SpecProd, Property, Year, ROUND(Value,%RND%) \
FROM PropSpecProdUB WHERE SpecType = 'Dist' ;
P48=DistSpecMax

Q43=SELECT SpecProd, Property, Year, ROUND(Value,%RND%) \
FROM PropSpecProdLB WHERE SpecType = 'Resid' ;
P43=ResidSpecMin

Q49=SELECT SpecProd, Property, Year, ROUND(Value,%RND%) \
FROM PropSpecProdUB WHERE SpecType = 'Resid' ;
P49=ResidSpecMax


* Import transportation capacities and costs

Q201=SELECT RefReg, Census, TranMode, UB FROM REFtoCDTranCap;
P201=REFtoCDTranCap

Q202=SELECT RefReg, Census, TranMode, RecipeProd, Cost FROM REFtoCDTranCost;
P202=REFtoCDTranCost

Q203=SELECT RefReg, Census, 1 FROM REFtoCDTranCost GROUP BY RefReg, Census ;
P203=REFtoCDTranMap

Q207=SELECT RefReg1, RefReg2, TranMode, Year, sum(UB) \
FROM REFtoREFTranCap GROUP BY RefReg1, RefReg2, TranMode, Year ;
P207=REFtoREFTranCap

Q208=SELECT RefReg1, RefReg2, TranMode, EthStream, Cost FROM REFtoREFTranCost;
P208=REFtoREFTranCost

Q211=SELECT IntReg, RefReg, UB FROM INTtoREFTranCap;
P211=INTtoREFTranCap

Q212=SELECT IntReg, RefReg, Cost FROM INTtoREFTranCost;
P212=INTtoREFTranCost

Q213=SELECT RefReg, IntReg, UB FROM REFtoINTTranCap;
P213=REFtoINTTranCap

Q214=SELECT RefReg, IntReg, Cost FROM REFtoINTTranCost;
P214=REFtoINTTranCost

Q215=SELECT Census, state, start_year, vol_Mbbl_yr FROM ULSD_N2H ;
P215=ULSD_N2H

Q216=SELECT refreg, state, year, vol_bilGal_yr FROM State_Biodiesel ;
P216=State_Biodiesel

Q217=SELECT stream, year, value FROM BiofuelSubsidy ;
P217=BiofuelSubsidy

* Import process table
Q75=SELECT Stream, Process, Mode, ROUND(Value,%RND%) \
FROM ProcessTables WHERE Stream = 'CO2' ;
P75=CO2EmFactors

* Import other variable costs for blending recipes
Q76=SELECT RcpMode, ROUND(Value,%RND%) FROM RecipeBlending WHERE Stream = 'OVC' ;
P76=RecipeOVC

* Capital expansion parameters
*Q77=SELECT Process, ROUND(Capacity,%RND%) FROM CapitalCosts ;
*P77=CapExpSize

*Q78=SELECT Process, ROUND(ISBL,%RND%) FROM CapitalCosts ;
*P78=CapExpISBL

*Q79=SELECT Process, ROUND(Labor,%RND%) FROM CapitalCosts ;
*P79=CapExpLabor

* Nelson-Farr refinery cost indicies
*Q80=SELECT Year, ROUND(NF_Invest,%RND%) FROM NelsonFarrIdx ;
*P80=NFInvest

*Q81=SELECT Year, ROUND(NF_Labor,%RND%) FROM NelsonFarrIdx ;
*P81=NFLabor

*Q82=SELECT Year, ROUND(NF_Oper,%RND%) FROM NelsonFarrIdx ;
*P82=NFOper

* Average state tax rates
*Q83=SELECT Year, RefReg, ROUND(StateTax,%RND%) FROM StateTax ;
*P83=StateTax

* Federal tax rate
*Q84=SELECT Year, ROUND(FedTax,%RND%) FROM FederalTax ;
*P84=FedTax

* Regional cost multipliers
*Q85=SELECT RefReg, ROUND(LaborLoc,%RND%) FROM RegionalData ;
*P85=LaborLoc

*Q86=SELECT RefReg, ROUND(InvLoc,%RND%) FROM RegionalData ;
*P86=InvLoc

* Investment cost parameters
*Q88=SELECT InvFactor, BldRiskClass, ROUND(Value,%RND%) \
*FROM InvestFactors WHERE InvFactor <> 'NFBaseYr' ;
*P88=InvFactors


***** Crude process, supply, and purchase data


Q116=SELECT RefReg, Source, Crude, Step, ROUND(Import_Price,%RND%) FROM Crude_Import_Cost ;
P116=CrudeImportCost_Temp

Q117=SELECT RefReg, Source, Step, Year, Import_UB FROM Crude_Import_Cap ;
P117=CrudeImportCap

Q118=SELECT RefReg, Source, Crude, Step, ROUND(Export_Price,%RND%) FROM Crude_Export_Cost ;
P118=CrudeExportCost_Temp

Q119=SELECT RefReg, Source, Step, Year, Export_UB FROM Crude_Export_Cap ;
P119=CrudeExportCap


Q198=SELECT RefReg, Crude, Value FROM Crude_Import_Region ;
P198=CrdImpAllowed

Q199=SELECT RefReg, Crude, Value FROM Crude_Export_Region ;
P199=CrdExpAllowed

Q250=SELECT RefReg, Crude, Year, Volume*1000 FROM SPR_Withdraw ;
P250=SPR_Withdraw

* Read in RFS mandates
Q123=SELECT Category, Year, ROUND(Value,%RND%) FROM RFSMandates ;
P123=RFSMandates

* Get mappings between various regional breakdowns
Q150=SELECT CoalDReg,RefReg,MapValue FROM COALDtoREFmap;
P150=COALDtoREFmap

Q152=SELECT Census, Year, MaxPen FROM E15MaxPen ;
P152=E15MaxPen

Q153=SELECT Process, Stream, Score FROM RFSScores ;
P153=RFSScores

Q154=SELECT Category, Stream, Value FROM RFSCategory ;
P154=RFSProcCategory

Q156=SELECT RFSCategory, Year, MinWaiverPrice FROM RFSWaiver ;
P156=RFSWaiverPrice

Q157=SELECT ExistingCapacity.RefReg, StateMaps.CenDiv, ROUND(Sum(ExCap),%RND%) \
FROM ExistingCapacity INNER JOIN StateMaps ON ExistingCapacity.State = StateMaps.State \
WHERE Process ='ACU' \
GROUP BY ExistingCapacity.RefReg, StateMaps.CenDiv;
P157=RefReg_to_CenDiv_ACU_Map



Q160=SELECT Year, FUEL, SECTOR, CENSUS_DIVISION, VALUE \
FROM ProductMarkups ;
P160=ProductMarkups

Q161=SELECT Year, FUEL, SECTOR, CENSUS_DIVISION, VALUE \
FROM StateFuelTax ;
P161=StateFuelTax

Q162=SELECT Year, FUEL, VALUE FROM FedFuelTax ;
P162=FedFuelTax

Q163=SELECT Census, FUEL, VALUE FROM MARKUPS_ENV ;
P163=EnvMarkups

Q164=SELECT Stream, alpha FROM CoproductPricing ;
P164=CoprodAlpha

Q165=SELECT Stream, beta FROM CoproductPricing ;
P165=CoprodBeta

Q166=SELECT EndProduct, Sector, Year, Value FROM DieselFrac ;
P166=DieselFrac

*10-23-15 em4 variable no longer used in LFMM
*             still NEED to remove from Excel and Access DB files
*Q168=SELECT Stream, NGPLRG, RefReg, Frac FROM NGLFractions ;
*P168=NGLFracNGPL


Q171=SELECT Sector, alpha FROM LPGPricing ;
P171=LPGAlpha

Q172=SELECT Sector, beta FROM LPGPricing ;
P172=LPGBeta

*Q173=SELECT Process, BuildMult FROM AFGrowthRates ;
*P173=AFGrowthRate

*Q174=SELECT Process, InitPlants FROM AFGrowthRates ;
*P174=AFInitPlants

Q175=SELECT Year, A FROM CornPriceExp ;
P175=CornPriceExp

Q176=SELECT RefReg, Cost FROM CornTranCost ;
P176=CornTranCost

Q177=SELECT RefReg, OilType, Value FROM SeedOilQnty ;
P177=SeedOilQnty

Q179=SELECT RefReg, Qnty FROM GrainQnty ;
P179=GrainQnty


*Q183=SELECT Process, Phase, Speed, ParamName, Value FROM Learning ;
*P183=LearningData

*Q184=SELECT Process, MoreLrnParam, Factor FROM Learning2 ;
*P184=MoreLearningData

*Q186=SELECT Process, BldStep, CapMult FROM AFBldSteps ;
*P186=AFCapMult

*Q187=SELECT Process, BldStep, CostMult FROM AFBldSteps ;
*P187=AFCostMult

Q188=SELECT RefReg, Year, Quant FROM FBDImpQuant ;
P188=FBDImpQuant

Q189=SELECT Year, BCOEFF FROM FBDImpCoef ;
P189=FBDImpCoefB

Q190=SELECT Year, MCOEFF FROM FBDImpCoef ;
P190=FBDImpCoefM

Q191=SELECT RefReg, Census, MapValue FROM CDtoREFMap ;
P191=CDtoREFMap

Q192=SELECT Process, StreamFactor FROM StreamFactors ;
P192=StreamFactors


Q350=SELECT Year, trancost FROM BrzAdvEthProd ;
P350=BrzAdvTranCost

Q351=SELECT Year, P0 FROM BrzAdvEthProd ;
P351=BrzAdvP0

Q352=SELECT Year, Q0 FROM BrzAdvEthProd ;
P352=BrzAdvQ0

Q353=SELECT Year, epsilon FROM BrzAdvEthProd ;
P353=BrzAdvEpsilon

Q354=SELECT Year, trancost FROM BrzEthDmd ;
P354=BrzEthDmdTranCost

Q355=SELECT Year, P0 FROM BrzEthDmd ;
P355=BrzEthDmdP0

Q356=SELECT Year, Q0 FROM BrzEthDmd ;
P356=BrzEthDmdQ0

Q357=SELECT Year, epsilon FROM BrzEthDmd ;
P357=BrzEthDmdEpsilon

Q358=SELECT Year, trancost FROM NonUSEthDmd ;
P358=NonUSEthTranCost

Q359=SELECT Year, P0 FROM NonUSEthDmd ;
P359=NonUSEthP0

Q360=SELECT Year, Q0 FROM NonUSEthDmd ;
P360=NonUSEthQ0

Q361=SELECT Year, epsilon FROM NonUSEthDmd ;
P361=NonUSEthEpsilon


*Input LCFS data

Q300=SELECT LCFS_Target.PetCategory FROM LCFS_Target WHERE (((LCFS_Target.LCFS_Target)<>0)) GROUP BY LCFS_Target.PetCategory;
S300=LCFS_PetCategory

Q305=SELECT LCFS_Target.Year FROM LCFS_Target WHERE (((LCFS_Target.LCFS_Target)>0)) GROUP BY LCFS_Target.Year;
S305=LCFS_Years

Q306=SELECT LCFS_Target.RefReg FROM LCFS_Target WHERE (((LCFS_Target.LCFS_Target)>0)) GROUP BY LCFS_Target.RefReg;
S306=LCFS_RefReg

Q307=SELECT LCFS_BioStreams_Process.Process FROM LCFS_BioStreams_Process \
WHERE (((LCFS_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY LCFS_BioStreams_Process.Process;
S307=LCFS_BioProcess

Q317=SELECT LCFS_BioStreams_Process.Mode FROM LCFS_BioStreams_Process \
WHERE (((LCFS_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY LCFS_BioStreams_Process.Mode;
S317=LCFS_BioMode

Q327=SELECT LCFS_BioStreams_Process.PetCategory, LCFS_BioStreams_Process.BioStream FROM LCFS_BioStreams_Process \
WHERE (((LCFS_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY LCFS_BioStreams_Process.PetCategory, LCFS_BioStreams_Process.BioStream;
S327=LCFS_PetCategory_BioStreams

Q308=SELECT LCFS_BioStreams_Process.BioStream FROM LCFS_BioStreams_Process \
WHERE (((LCFS_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY LCFS_BioStreams_Process.BioStream;
S308=LCFS_BioStreams

Q318=SELECT LCFS_BioStreams_Process.BioStream, LCFS_BioStreams_Process.PetCategory FROM LCFS_BioStreams_Process \
WHERE (((LCFS_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY LCFS_BioStreams_Process.BioStream, LCFS_BioStreams_Process.PetCategory;
S318=LCFS_BioStreams_PetStreams

Q309=SELECT LCFS_PetStreams.PetStream FROM LCFS_PetStreams \
WHERE (((LCFS_PetStreams.Carbon_Factor)<>0)) GROUP BY LCFS_PetStreams.PetStream;
S309=LCFS_PetStreams

Q319=SELECT LCFS_PetStreams.PetCategory, LCFS_PetStreams.PetStream FROM LCFS_PetStreams \
WHERE (((LCFS_PetStreams.Carbon_Factor)<>0)) GROUP BY LCFS_PetStreams.PetCategory, LCFS_PetStreams.PetStream;
S319=LCFS_PetCategory_PetStreams

Q310=SELECT LCFS_BioImports.BioStream FROM LCFS_BioImports \
WHERE (((LCFS_BioImports.Carbon_Factor)<>0)) GROUP BY LCFS_BioImports.BioStream;
S310=LCFS_BioImports

Q312=SELECT DISTINCT PetCategory, BioStream FROM LCFS_BioImports;
S312=LCFS_BioImport_PetStream

Q313=SELECT LCFS_AltVehicles.Vehicle_Type FROM LCFS_AltVehicles;
S313=LCFS_Vehicle_Types

Q314=SELECT LCFS_AltVehicles.PetCategory, LCFS_AltVehicles.Vehicle_Type FROM LCFS_AltVehicles \
WHERE (((LCFS_AltVehicles.Carbon_Factor)<>0)) GROUP BY LCFS_AltVehicles.PetCategory, LCFS_AltVehicles.Vehicle_Type;
S314=LCFS_PetCategory_Vehicle_Type


Q299=SELECT LCFS_Penalty_Cost.PetCategory, LCFS_Penalty_Cost.LCFS_Penalty FROM LCFS_Penalty_Cost;
P299=LCFS_Penalty_Cost

Q301=SELECT PetCategory, RefReg, Year, LCFS_Target FROM LCFS_Target WHERE (((LCFS_Target.LCFS_Target)>0));
P301=LCFS_Target

Q302=SELECT PetCategory, Process, Mode, BioStream, Year, Carbon_Factor FROM LCFS_BioStreams_Process;
P302=LCFS_BioStream_Carbon_Factor

Q304=SELECT PetCategory, PetStream, Year, Carbon_Factor from LCFS_PetStreams;
P304=LCFS_PetStream_Carbon_Factor

Q311=SELECT PetCategory, BioStream, Year, Carbon_Factor FROM LCFS_BioImports;
P311=LCFS_BioImport_Carbon_Factor

Q315=SELECT PetCategory, Vehicle_Type, Carbon_Factor FROM LCFS_AltVehicles;
P315=LCFS_AltVehicle_Carbon_Factor

Q316=SELECT PetCategory, Vehicle_Type, CA_Share FROM LCFS_AltVehicles;
P316=LCFS_AltVehicle_CA_Share

* AB-32 emissions cap adjustment factor
Q340=SELECT Year, CapAdjFactor FROM AB32_CapAdjFactor ;
P340=AB32_CapAdjFactor

* AB-32 industry assistance factors
Q341=SELECT Year, AssistFactor FROM AB32_AssistFactor ;
P341=AB32_AssistFactor

* AB-32 allowances ber barrel
Q342=SELECT BenchFactor FROM AB32_BenchFactor ;
P342=AB32_BenchFactor

* AB-32 combustion emissions factors for products imported into California
Q343=SELECT Stream, StartYr FROM AB32_Control ;
P343=AB32_StartYr

* AB-32 streams
Q344=SELECT Stream FROM AB32_Control ;
S344=AB32_Stream

* AB-32 energy densities for products imported into California
Q345=SELECT Stream, CoverageFrac FROM AB32_Control ;
P345=AB32_CoverageFrac

*Input CFP data

Q400=SELECT CFP_Target.PetCategory FROM CFP_Target WHERE (((CFP_Target.CFP_Target)<>0)) GROUP BY CFP_Target.PetCategory;
S400=CFP_PetCategory

Q405=SELECT CFP_Target.Year FROM CFP_Target WHERE (((CFP_Target.CFP_Target)>0)) GROUP BY CFP_Target.Year;
S405=CFP_Years

Q406=SELECT CFP_Target.RefReg FROM CFP_Target WHERE (((CFP_Target.CFP_Target)>0)) GROUP BY CFP_Target.RefReg;
S406=CFP_RefReg

Q407=SELECT CFP_BioStreams_Process.Process FROM CFP_BioStreams_Process \
WHERE (((CFP_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY CFP_BioStreams_Process.Process;
S407=CFP_BioProcess

Q417=SELECT CFP_BioStreams_Process.Mode FROM CFP_BioStreams_Process \
WHERE (((CFP_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY CFP_BioStreams_Process.Mode;
S417=CFP_BioMode

Q427=SELECT CFP_BioStreams_Process.PetCategory, CFP_BioStreams_Process.BioStream FROM CFP_BioStreams_Process \
WHERE (((CFP_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY CFP_BioStreams_Process.PetCategory, CFP_BioStreams_Process.BioStream;
S427=CFP_PetCategory_BioStreams

Q408=SELECT CFP_BioStreams_Process.BioStream FROM CFP_BioStreams_Process \
WHERE (((CFP_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY CFP_BioStreams_Process.BioStream;
S408=CFP_BioStreams

Q418=SELECT CFP_BioStreams_Process.BioStream, CFP_BioStreams_Process.PetCategory FROM CFP_BioStreams_Process \
WHERE (((CFP_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY CFP_BioStreams_Process.BioStream, CFP_BioStreams_Process.PetCategory;
S418=CFP_BioStreams_PetStreams

Q409=SELECT CFP_PetStreams.PetStream FROM CFP_PetStreams \
WHERE (((CFP_PetStreams.Carbon_Factor)<>0)) GROUP BY CFP_PetStreams.PetStream;
S409=CFP_PetStreams

Q419=SELECT CFP_PetStreams.PetCategory, CFP_PetStreams.PetStream FROM CFP_PetStreams \
WHERE (((CFP_PetStreams.Carbon_Factor)<>0)) GROUP BY CFP_PetStreams.PetCategory, CFP_PetStreams.PetStream;
S419=CFP_PetCategory_PetStreams

Q410=SELECT CFP_BioImports.BioStream FROM CFP_BioImports \
WHERE (((CFP_BioImports.Carbon_Factor)<>0)) GROUP BY CFP_BioImports.BioStream;
S410=CFP_BioImports

Q412=SELECT DISTINCT PetCategory, BioStream FROM CFP_BioImports;
S412=CFP_BioImport_PetStream

Q413=SELECT CFP_AltVehicles.Vehicle_Type FROM CFP_AltVehicles;
S413=CFP_Vehicle_Types

Q414=SELECT CFP_AltVehicles.PetCategory, CFP_AltVehicles.Vehicle_Type FROM CFP_AltVehicles \
WHERE (((CFP_AltVehicles.Carbon_Factor)<>0)) GROUP BY CFP_AltVehicles.PetCategory, CFP_AltVehicles.Vehicle_Type;
S414=CFP_PetCategory_Vehicle_Type


Q399=SELECT CFP_Penalty_Cost.PetCategory, CFP_Penalty_Cost.CFP_Penalty FROM CFP_Penalty_Cost;
P399=CFP_Penalty_Cost

Q401=SELECT PetCategory, RefReg, Year, CFP_Target FROM CFP_Target WHERE (((CFP_Target.CFP_Target)>0));
P401=CFP_Target

Q402=SELECT PetCategory, Process, Mode, BioStream, Year, Carbon_Factor FROM CFP_BioStreams_Process;
P402=CFP_BioStream_Carbon_Factor

Q404=SELECT PetCategory, PetStream, Year, Carbon_Factor from CFP_PetStreams;
P404=CFP_PetStream_Carbon_Factor

Q411=SELECT PetCategory, BioStream, Year, Carbon_Factor FROM CFP_BioImports;
P411=CFP_BioImport_Carbon_Factor

Q415=SELECT PetCategory, Vehicle_Type, Carbon_Factor FROM CFP_AltVehicles;
P415=CFP_AltVehicle_Carbon_Factor

Q416=SELECT PetCategory, Vehicle_Type, OR_Share FROM CFP_AltVehicles;
P416=CFP_AltVehicle_OR_Share


* Investment cost for carbon capture from ethanol production
Q500=SELECT Census, Year, Investment_Cost FROM CCATSDAT_CST_ETH_INV ;
P500=CCATSDAT_CST_ETH_INV_lfmm

* O&M cost for carbon capture from ethanol production
Q501=SELECT Census, Year, OM_Cost  FROM CCATSDAT_CST_ETH_OM ;
P501=CCATSDAT_CST_ETH_OM_lfmm

* CO2 volumes from ethanol production that are 45Q eligible
Q502=SELECT Census, Year, CO2_volume FROM CCATSDAT_SUP_ETH_45Q ;
P502=CCATSDAT_SUP_ETH_45Q_lfmm

* CO2 volumes from ethanol production no tax credit
Q503=SELECT Census, Year, CO2_volume FROM CCATSDAT_SUP_ETH_NTC ;
P503=CCATSDAT_SUP_ETH_NTC_lfmm

*Input WACFS data

Q600=SELECT WACFS_Target.PetCategory FROM WACFS_Target WHERE (((WACFS_Target.WACFS_Target)<>0)) GROUP BY WACFS_Target.PetCategory;
S600=WACFS_PetCategory

Q605=SELECT WACFS_Target.Year FROM WACFS_Target WHERE (((WACFS_Target.WACFS_Target)>0)) GROUP BY WACFS_Target.Year;
S605=WACFS_Years

Q606=SELECT WACFS_Target.RefReg FROM WACFS_Target WHERE (((WACFS_Target.WACFS_Target)>0)) GROUP BY WACFS_Target.RefReg;
S606=WACFS_RefReg

Q607=SELECT WACFS_BioStreams_Process.Process FROM WACFS_BioStreams_Process \
WHERE (((WACFS_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY WACFS_BioStreams_Process.Process;
S607=WACFS_BioProcess

Q617=SELECT WACFS_BioStreams_Process.Mode FROM WACFS_BioStreams_Process \
WHERE (((WACFS_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY WACFS_BioStreams_Process.Mode;
S617=WACFS_BioMode

Q627=SELECT WACFS_BioStreams_Process.PetCategory, WACFS_BioStreams_Process.BioStream FROM WACFS_BioStreams_Process \
WHERE (((WACFS_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY WACFS_BioStreams_Process.PetCategory, WACFS_BioStreams_Process.BioStream;
S627=WACFS_PetCategory_BioStreams

Q608=SELECT WACFS_BioStreams_Process.BioStream FROM WACFS_BioStreams_Process \
WHERE (((WACFS_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY WACFS_BioStreams_Process.BioStream;
S608=WACFS_BioStreams

Q618=SELECT WACFS_BioStreams_Process.BioStream, WACFS_BioStreams_Process.PetCategory FROM WACFS_BioStreams_Process \
WHERE (((WACFS_BioStreams_Process.Carbon_Factor)<>0)) GROUP BY WACFS_BioStreams_Process.BioStream, WACFS_BioStreams_Process.PetCategory;
S618=WACFS_BioStreams_PetStreams

Q609=SELECT WACFS_PetStreams.PetStream FROM WACFS_PetStreams \
WHERE (((WACFS_PetStreams.Carbon_Factor)<>0)) GROUP BY WACFS_PetStreams.PetStream;
S609=WACFS_PetStreams

Q619=SELECT WACFS_PetStreams.PetCategory, WACFS_PetStreams.PetStream FROM WACFS_PetStreams \
WHERE (((WACFS_PetStreams.Carbon_Factor)<>0)) GROUP BY WACFS_PetStreams.PetCategory, WACFS_PetStreams.PetStream;
S619=WACFS_PetCategory_PetStreams

Q610=SELECT WACFS_BioImports.BioStream FROM WACFS_BioImports \
WHERE (((WACFS_BioImports.Carbon_Factor)<>0)) GROUP BY WACFS_BioImports.BioStream;
S610=WACFS_BioImports

Q612=SELECT DISTINCT PetCategory, BioStream FROM WACFS_BioImports;
S612=WACFS_BioImport_PetStream

Q613=SELECT WACFS_AltVehicles.Vehicle_Type FROM WACFS_AltVehicles;
S613=WACFS_Vehicle_Types

Q614=SELECT WACFS_AltVehicles.PetCategory, WACFS_AltVehicles.Vehicle_Type FROM WACFS_AltVehicles \
WHERE (((WACFS_AltVehicles.Carbon_Factor)<>0)) GROUP BY WACFS_AltVehicles.PetCategory, WACFS_AltVehicles.Vehicle_Type;
S614=WACFS_PetCategory_Vehicle_Type


Q499=SELECT WACFS_Penalty_Cost.PetCategory, WACFS_Penalty_Cost.WACFS_Penalty FROM WACFS_Penalty_Cost;
P499=WACFS_Penalty_Cost

Q601=SELECT PetCategory, RefReg, Year, WACFS_Target FROM WACFS_Target WHERE (((WACFS_Target.WACFS_Target)>0));
P601=WACFS_Target

Q602=SELECT PetCategory, Process, Mode, BioStream, Year, Carbon_Factor FROM WACFS_BioStreams_Process;
P602=WACFS_BioStream_Carbon_Factor

Q604=SELECT PetCategory, PetStream, Year, Carbon_Factor from WACFS_PetStreams;
P604=WACFS_PetStream_Carbon_Factor

Q611=SELECT PetCategory, BioStream, Year, Carbon_Factor FROM WACFS_BioImports;
P611=WACFS_BioImport_Carbon_Factor

Q615=SELECT PetCategory, Vehicle_Type, Carbon_Factor FROM WACFS_AltVehicles;
P615=WACFS_AltVehicle_Carbon_Factor

Q616=SELECT PetCategory, Vehicle_Type, WA_Share FROM WACFS_AltVehicles;
P616=WACFS_AltVehicle_WA_Share

$offecho

* write %DirPath%input\myLFMinput.gdx
$call mdb2gms.exe @cmd.txt
*--------------------------------------------------------------------------------------------------------

