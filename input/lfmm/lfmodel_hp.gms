***** $Header: m:/default/input/RCS/lfmodel.gms,v 1.72 2020/07/06 15:44:58 ESH Exp $
*LP model definition

Positive Variables
   PROCMODE(RefReg,RefType,Process,ProcessMode,Period)            M BBLs per day for each process and mode in each period
   RECIPEMODE(RefReg,RefType,RcpMode,Period)                      M BBLs per day on recipe in each period
   CRUDEPURCH(Source,Crude,Step,Period)                           M BBLs per day of each crude purchased at each supply step in each period
   CRUDETRANS(RefReg,Crude,Source,Step,Period)                    M BBLs per day of each crude transported from source to refining region in each period
   CRUDETOTAL(Step,Period)                                        M BBLs per day total world crude at each supply step in each period
   CRUDENONUS(Source,Crude,Step,Period)                           M BBLs per day Non-US crude demand at each demand step in each period
   CRUDEEXPORT(RefReg,Crude,Source,Step,Period)                   M BBLs per day US crude exports to foreign region
   UTILPURCH(RefReg,RefType,Utility,Period)                       Amount(units vary) of each utility purchased in each period
   REFPURCH(RefReg,RefInputStr,Step,Period)                       Amount(units vary) of refinery input purchased at each supply step in each period
   REFPURused(RefReg,RefType,RefInputStr,Period)                  Amount(units vary) of purchased refinery input used by each refinery type
   BIOPURCH(CoalDReg,BioStr,Step,Period)                          Amount(units vary) of biomass purchased at each supply step in each period
   BIODEMAND(CoalDReg,BioStr,Other_MKT,Period)                    Amount(units vary) of biomoass in demand by other NEMS modules
   BIOXFER(CoalDReg,RefReg,BioStr,Period)                         Amount of biomass transferred from each coal demand region
   ToRECIPEBLEND(RefReg,RefType,RcpMode,Stream,Period)            M BBLs per day of stream going to recipe product recipies
   ToSPECBLEND(RefReg,RefType,SpecProd,IntStream,Period)          M BBLs per day of stream going to specification blending pool
   RECIPETOPROD(RefReg,RefType,RecipeProd,Period)                 M BBLs per day transferred of recipes to end products
   TOTPROD(RefReg,RecipeProd,Period)                              M BBLs per day Total production of each product by supply region
   CDSUPPLY(CenDiv,Stream,Period)                                 M BBLs per day domestically-produced end product available by census division
   RECIPESUPPLY(CenDiv,Stream,Period)                             M BBLs per day total end product available by census division including imports

   ETHIMP(RefReg,EthStream,Period)                                M BBLs per day of ethanol imported at each supply step in each period
   ETHEXP(RefReg,EthStream,Period)                                M BBLs per day of corn ethanol exported at each supply step in each period
   ETHEXP_2(RefReg,EthStream,Period)                              M BBLs per day of corn ethanol exported to World (not Brazil)
   ETHIMPUSsup(EthStream,Step,Period)                             M BBLs per day of ethanol available for import to US
   ETHEXPUSdmd(EthStream,Step,Period)                             M BBLs per day of ethanol available to receive exports fr US
*  ETH_BRAZIL(EthStream,Step,Period)                              M BBLs per day of TOTAL ethanol produced in Brazil
*  ETHNonUS(Period)                                               M BBLs per day of ethanol Non-US demand
*  BrzEthExpNonUs(Period)                                         M BBLs per day of ethanol exported from Brazil to satisfy non-US demand

   DISTRESSIMPORT(CenDiv,EndProduct,Step,Period)                  M BBLs per day of global import
   COPRODUCTS(RefReg,RefType,CoProduct,Period)                    Amount (units vary) of co-products produced from refinery processes
   ImpToSPECBLEND(RefReg,RefType,GasSpecProd,Period)              Amount BOB transferred to Spec Blending
   ExpFromSPECBLEND(RefReg,RefType,GasSpecProd,Period)            Amount BOB transferred from Spec Blending
   IMPORTS(Stream,RefReg,Period)                                  Amount of product imports
   EXPORTS(Stream,RefReg,Period)                                  Amount of product exports

   PRODIMP(ImpStr,INTREG,Step,Period)                             M BBLs per day of product imported at each supply step of international product import curves
   PRODEXP(ImpStr,INTREG,Step,Period)                             M BBLs per day of product exported at each supply step of international product import curves

   PRODImpTRAN(INTREG,DomRefReg,ImpStr,Period)                    M BBLs per day of each import product transported from IntReg to refining region in each period
   PRODExpTRAN(DomRefReg,INTREG,ImpStr,Period)                    M BBLs per day of each export product transported from refining region to IntReg in each period

   NGLIMPORTS(NGLProduct,CenDiv,Period)                           Amount of NGL imports
   NGLEXPORTS(NGLProduct,CenDiv,Period)                           Amount of NGL exports

   COALPURCH(CoalSReg,CoalStr,Step,Period)                        Amount(units ????) of coal purchased at each supply step in each period
   COALDEMAND(CoalSReg,CoalStr,Period)                            Amount(units ????) of coal in demand by other NEMS modules
   COALTRANSP(CoalSReg,CoalDReg,CoalStr,Period)                   Amount(units ????) of coal transported from coal suppy to coal demand region
   COALXFER(CoalDReg,RefReg,CoalStr,Period)                       Amount(units ????) of coal transferred from coal demand region to refinery region
   SO2CREDITS(MX_SO2,Period)                                      SO2 credits purchased
   HGCREDITS(Period)                                              Mercury credits purchased

   PRODTOCENSUS(RefReg,CenDiv,Stream,Period)                      M BBLs transferred per day from supply regions to census divisions
   RefCenTRAN(RefReg,CenDiv,TranMode,RecipeProd,Period)           M BBLs transferred per day by transp mode between supply region and census division
   RefRefTRAN(RefRegA,RefReg,TranMode,Stream,Period)              M BBLs transferred per day from supply region RefRegA to RefReg

   BUILDS(RefReg,RefType,Process,BldStep,Period)                          M BBLs per day capacity added to processes in each period

   OPERATECAP(RefReg,RefType,Process,Period)                      M BBLs per day EXISTING capacity operated in each period

   E85STP(E85_Steps,CenDiv,Period)                                M BBLs per day E85 production on each pricing step

   QMG10(CenDiv,Period)                                           M BBLs per day total E10 gasoline used to satisfy demand
   QMG15(CenDiv,Period)                                           M BBLs per day total E15 gasoline used to satisfy demand
   QMGb16(CenDiv,Period)                                          M BBLs per day total B16 gasoline used to satisfy demand

   QMG(CenDiv,Period)                                             M BBLs per day total of all gasoline used to satisfy flex-fuel BTU constraint
   QE85(CenDiv,Period)                                            M BBLs per day total of all E85 used to satisfy flex-fuel BTU constraint

   RFSESCAPE(RFSCategory,Period)                                  RFS escape valves
   RFS_PRD_to_RQM(RFSCategory,Period)                             RFS Transfer Production to Requirement Constraints
   RFS_PRD_VAL(RFSCategory,Period)                                RFS Subsidy for Biofuel Production by RFS Category for Price Period only
   RFS_RQM_CST(RFSCategory,Period)                                RFS Cost for Purchasing RFS Credits by RFS Category for Price Period only

   LCFS_Pet_to_Bio(LCFS_PetCategory,Period)                       LCFS Transfer Petroleum Deficit to Bio-Liquid Relief
   LCFS_Pet_Cst(LCFS_PetCategory,Period)                          LCFS Cost For Petroleum Deficit in Price Period only
   LCFS_Bio_Sub(LCFS_PetCategory,Period)                          LCFS Subsidy For Bio-Liquid Relief in Price Period only
   LCFSSafety(LCFS_PetCategory,Period)
   LCFS_LandfillGas(LCFS_PetCategory,Period)                      LCFS Landfill Gas Demand
   LCFS_AltVehicles(LCFS_PetCategory,LCFS_Vehicle_Types,Period)   LCFS Alternate Vehicle Demand
   LCFS_landfillGas(LCFS_PetCategory,Period)                      LCFS landfill gas allowed to go to either PetCategory

   CFP_Pet_to_Bio(CFP_PetCategory,Period)                         CFP Transfer Petroleum Deficit to Bio-Liquid Relief
   CFP_Pet_Cst(CFP_PetCategory,Period)                            CFP Cost For Petroleum Deficit in Price Period only
   CFP_Bio_Sub(CFP_PetCategory,Period)                            CFP Subsidy For Bio-Liquid Relief in Price Period only
   CFPSafety(CFP_PetCategory,Period)
   CFP_LandfillGas(CFP_PetCategory,Period)                        CFP Landfill Gas Demand
   CFP_AltVehicles(CFP_PetCategory,CFP_Vehicle_Types,Period)      CFP Alternate Vehicle Demand
   CFP_landfillGas(CFP_PetCategory,Period)                        CFP landfill gas allowed to go to either PetCategory

   AB32_PurchAllow(Period)                                        AB-32 purchased carbon emission allowances in M tonnes CO2 per day

   MCCDEMAND(Stream,Step,Period)                                  Discretionary demand from Maritime Canada and the Carribean (RefReg1 only)

   BIODIMP(Step,Period)                                           M BBLs per day of biodiesel imported at each supply step in each period
   BIODIMPref(RefReg,RefType,Period)

   RENEWDIMP(Step,Period)                                         M BBLs per day of renewable diesel imported at each supply step in each period
   RENEWDIMPref(RefReg,RefType,Period)

   CO2_TRAN(OGCO2Reg,OGCO2Reg2,Period)                            CO2 transported from OGSM region to OGSM region in M tons CO2 per day
   CO2_PURCH(OGCO2Reg,CO2_Source,Period)                          CO2 purchased in OGSM region by source in M tons CO2 per day
   CO2toEOR(FuelRegion,OGCO2Reg,Period)                           XTL CO2 transported from FuelRegion to OGSM region for EOR in M tons CO2 per day
   CO2toSALINE(FuelRegion,Period)                                 XTL CO2 sent to saline storage in M tons CO2 per day
   CO2_SAFETY(OGCO2Reg,Period)                                    CO2 safety valve for OGSM CO2 demand

   STEOplus(STEO_Category,Step,Period)                            Overshoot penalty variable for STEO volume targets
   STEOminus(STEO_Category,Step,Period)                           Undershoot penalty variable for STEO volume targets
;

Variables
   TotalCost                                                      Objective function value
   STEO_VOLUME(STEO_Category,Period)                              STEO volume targets in M bbl per day
;


Equations
   OBJ                                                              Objective function
   CrudeBalance(RefReg,Crude,Period)                                Crude-ACU-Export balance equations
   CrudeSupCurveForeign(Source,Crude,Period)                        Enforce balance between foreign crude supply and crude transport
   WorldCrudeSup(Period)                                            Enforce balance between total world crude supply and total crude demand
   CrudeImportLimit(RefReg,Source,Step,Period)                      Enforce Crude Transport Limits between foreign sources and RefReg
   CrudeExportLimit(RefReg,Source,Step,Period)                      Enforce Crude Transport Limits between RefReg and foreign sources
   CrudeExportLimit3CAN(Source,Period)                              Enforce Crude Transport Limits from RefReg3 to Canada
   ExportDomCrudeOnly(RefReg,Crude,Period)

*  BrzMaxExportsToUS(EthStream,Period)                              Maximum exports from Brazil to U.S.
*  EthWorldBal(Period)                                              Brazil ethanol production balance
*  BrzEthProdBal(Period)                                            Satisfy non-US ethanol demand
   EthImpUSBal(Period)                                              Ethanol import balance to US
   EthExpUSBal(Period)                                              Ethanol export balance from US
   EthBalance(RefReg,EthStream,Period)                              Ethanol stream balance equations
*  EthTranLimit(RefReg,RefRegA,TranMode,Period)                     Limits on ethanol transportation capacity

   UtilBalance(RefReg,RefType,Utility,Period)                       Utility stream balance equations
*   UtilSupCurve(RefReg,Utility,Step,Period)                         Enforce bounds on ethanol import supply steps
   RefInpBalance(RefReg,RefType,RefInputStr,Period)
   RefPurchBal(RefReg,RefInputStr,Period)                           Balance refinery purchase variables

   BioBalance(CoalDReg,BioStr,Period)                               Biomass balance at coal demand regions
   BioRefRegBal(RefReg,BioStr,Period)                             Biomass balance at refining regions

   CoalDemBalance(CoalDReg,CoalStr,Period)                          Coal balance by coal demand region
   CoalSupBalance(CoalSReg,CoalStr,Period)                          Coal balance by coal supply region
   CoalRefRegBal(RefReg,CoalStr,Period)                             Coal balance by refinery supply region
   SO2EmisBal(MX_SO2,Period)                                        SO2 Emissions balance
   HGEmisBal(Period)                                                Mercury Emissions balance

   StreamBalance(RefReg,RefType,Stream,Period)                      Intermediate steam balance equations
   CapacityBalance(RefReg,RefType,Process,Period)                   Balance of overall process capacities with the activity on their modes
   MaxLTE(RefReg,RefType,Period)                                    Limit growth of ACU naphtha processing pseudo-unit 'LTE' to 10% of original capacity

   RecipeBalance(RefReg,RefType,RcpMode,Stream,Period)               Recipe volume balance between input streams and recipes
   RecipeTransfer(RefReg,RefType,RecipeProd,Period)
   RecipeBPTransfer(RefReg,RefType,CoProduct,Period)

   GasSpecQualMin(RefReg,RefType,GasSpecProd,GasProp,Period)        Specification blending constraints for gasoline products
   GasSpecQualMax(RefReg,RefType,GasSpecProd,GasProp,Period)        Specification blending constraints for gasoline products
   GasSpecBalance(RefReg,RefType,GasSpecProd,Period)                Specification blending volume balance for gasoline products
   DistSpecQualMin(RefReg,RefType,DistSpecProd,DistProp,Period)     Specification blending constraints for distillate products
   DistSpecQualMax(RefReg,RefType,DistSpecProd,DistProp,Period)     Specification blending constraints for distillate products
   DistSpecBalance(RefReg,RefType,DistSpecProd,Period)              Specification blending volume balance for distillate products
   ResidSpecQualMin(RefReg,RefType,ResidSpecProd,ResidProp,Period)  Specification blending constraints for resid products
   ResidSpecQualMax(RefReg,RefType,ResidSpecProd,ResidProp,Period)  Specification blending constraints for resid products
   ResidSpecBalance(RefReg,RefType,ResidSpecProd,Period)            Specification blending volume balance for resid products

   CombineSupply(RefReg,RecipeProd,Period)                          Sum regional production over refinery types
   CDSupplyTot(CenDiv,RecipeProd,Period)                            Recipe product supply balance for each census
   REFtoREFTran(RefReg,RefRegA,TranMode,Period)                     Balance RefReg to RefReg transfers for a given Reftype
   TotPRODTran(RefReg,RecipeProd,Period)                            Transfer end products between supply regions and census divisions
   REFtoCDTran(RefReg,CenDiv,RecipeProd,Period)                     PADD-to-Census transportation balance
   REFtoCDCap(RefReg,CenDiv,TranMode,Period)                        Enforce bounds on non-marine transportation capacity
   REFtoREFCapMB(RefReg,RefRegA,MarineMode,Period)                  Enforce bounds on marine transportation capacity
   ImpTranLimit(INTREG,RefReg,Period)                               Enforce bounds on product import transportation capacity
   ExpTranLimit(RefReg,INTREG,Period)                               Enforce bounds on product export transportation capacity

   RecipeDemands(CenDiv,Stream,Period)                              Must satisfy demand non-gasoline recipe-blended end products in all census divisions
   NGLDmd(CenDiv,NGLProduct,Period)                                 Must at least satisfy NGPL demands
   RecipeEndBal(CenDiv,Stream,Period)                               Total all product available by census over domestic and imports
   NGLEndBal(CenDiv,NGLProduct,Period)                              Total all NGPL available by census over domestic and imports
   FlexBTUDmd(CenDiv,Period)                                        Must satisfy total gasoline blend demand in BTUs in each census
   MGTotal(CenDiv,Period)                                           Total gasoline used to satisfy flex-fuel BTU constraint into combined vector
   E85Total(CenDiv,Period)                                          E85 used to satisfy flex-fuel BTU constraint into combined vector

   E85Balance(CenDiv,Period)                                        Balance E85 transferred to each census with amount used on supply steps by census
   CFGBalance(CenDiv,Period)                                        Balance CFG transferred to each census with amount used towards BTU constraint
   RFGBalance(CenDiv,Period)                                        Balance RFG transferred to each census with amount used towards BTU constraint
   CFG15Balance(CenDiv,Period)                                      Balance CFG15 transferred to each census with amount used towards BTU constraint
   RFG15Balance(CenDiv,Period)                                      Balance RFG15 transferred to each census with amount used towards BTU constraint
   CFGb16Balance(CenDiv,Period)                                     Balance CFG B16 transferred to each census with amount used towards BTU constraint
   RFGb16Balance(CenDiv,Period)                                     Balance RFG B16 transferred to each census with amount used towards BTU constraint
   CaRBOBBalance(CenDiv,Period)                                     Balance CaRBOB transferred to each census with amount used towards BTU constraint

   E15Max(CenDiv,Period)                                            Enforce E15 maximum penetration factors

   RFSConstraintsPRD(RFSCategory,Period)                            RFS Constraints - Production
   RFSConstraintsRQM(RFSCategory,Period)                            RFS Constraints - Requirment
   RFSLimitCornEthCredits(Period)                                   limit on corn ethanol credits counted toward RFS

   LCFS_Petroleum(LCFS_PetCategory,Period)                          LCFS Constraint - Petroleum Deficit
   LCFS_BioFuel(LCFS_PetCategory,Period)                            LCFS Constraint - BioFuel Relieve
   LCFS_LandfillGasConstr(Period)                                   LCFS Constraint - allow landfill gas credit to both PetCategories

   CFP_Petroleum(CFP_PetCategory,Period)                            CFP Constraint - Petroleum Deficit
   CFP_BioFuel(CFP_PetCategory,Period)                              CFP Constraint - BioFuel Relieve
   CFP_LandfillGasConstr(Period)                                    CFP Constraint - allow landfill gas credit to both PetCategories

   AB32_Constraint(Period)                                          AB-32 carbon emission constraint in M tonnes CO2 per day

   BOBBalance(RefReg,GasSpecProd,Period)                            RBOB-CBOB balance

   RestrictGrowth(Process,BldStep,Period)                           Capacity expansion growth restrictions

   RestrictGrowthCTL(BldStep,Period)                                Combined CTL-CTLCCS capacity expansion growth restrictions
   RestrictGrowthCTLGTL(BldStep,Period)                             Combined CTL-CTLCCS-GTL capacity expansion growth restrictions

   RestrictGrowthCBL(BldStep,Period)                                Combined CBL-CBLCCS capacity expansion growth restrictions

   BiodieselBalance(Period)                                         Balance biodiesel imports over refinery types

   RenewDieselBalance(Period)                                       Balance renewable diesel imports over refinery types

   MaxCCRtoFCC(RefReg,RefType,Period)                               Limit the average Conradson Carbon %wt of the FCC feed to 2% or less

   OGSM_CO2Demand(OGCO2Reg,Period)                                  Satisfy OGSM EOR CO2 demand
   OGSM_CO2TranBal(OGCO2Reg,Period)                                 Make sure CO2 used is transported
   CO2CCSBalance(FuelRegion,Period)                                 XTL captured CO2 must go to either EOR or Saline storage

   ProdImpBalance(RefReg,Stream,Period)                             Ensure product imports are transported between IntReg and RefReg
   ProdImpSupCurve(INTREG,Stream,Period)                            Enforce balance between import product supply and product transport

   ProdExpBalance(RefReg,Stream,Period)                             Ensure product exports are transported between RefReg and IntReg
   ProdExpSupCurve(INTREG,Stream,Period)                            Enforce balance between export product demand and product transport

   StateBiodiesel(RefReg,Period)                                    Enforce state-level biodiesel requirements

*   CrudeImportsSTEO(Period)                                         Enforce STEO benchmarking crude gross import volume targets
*   CrudeExportsSTEO(Period)                                         Enforce STEO benchmarking crude gross export volume targets
*   NetCrudeImportsSTEO(Period)                                      Enforce STEO benchmarking crude NET import volume targets
   TotalCrudeInputsSTEO(Period)                                     Enforce STEO benchmarking crude input to refineries volume targets

*   UFOImportsSTEO(Period)                                           Enforce STEO benchmarking UFO gross import volume targets
*   UFOExportsSTEO(Period)                                           Enforce STEO benchmarking UFO gross export volume targets
   NetUFOImportsSTEO(Period)                                        Enforce STEO benchmarking UFO export volume targets

*   ProdImportsSTEO(Period)                                          Enforce STEO benchmarking product gross import volume targets
*   ProdExportsSTEO(Period)                                          Enforce STEO benchmarking product gross export volume targets
   NetProdImportsSTEO(Period)                                       Enforce STEO benchmarking product NET import volume targets
;

****** Model equations definitions ******
OBJ..
  - TotalCost +
  sum((SupTypMode(RefReg,RefType,Process,ProcessMode),ActivePeriod),
    npv_OpVarCost(Process,ProcessMode,ActivePeriod)* PROCMODE(RefReg,RefType,Process,ProcessMode,ActivePeriod) ) +
  sum((Step,ActivePeriod)$npv_CrudeSupplyTotal(Step,ActivePeriod),
    npv_CrudePriceTotal(Step,ActivePeriod)*CRUDETOTAL(Step,ActivePeriod) ) +
  sum((Source,Crude,Step,ActivePeriod)$npv_CrudeSupplyIncremental(Source,Crude,Step,ActivePeriod),
    npv_CrudePriceIncremental(Source,Crude,Step,ActivePeriod)*CRUDEPURCH(Source,Crude,Step,ActivePeriod) ) +
  sum((RefReg,Crude,Source,Step,ActivePeriod),
    npv_CrudeExportCost(RefReg,Source,Crude,Step,ActivePeriod) * CRUDEEXPORT(RefReg,Crude,Source,Step,ActivePeriod)$npv_CrudeExportCap(RefReg,Source,Step,ActivePeriod)) +
  sum((RefReg,Crude,Source,Step,ActivePeriod),
    npv_CrudeImportCost(RefReg,Source,Crude,Step,ActivePeriod) * CRUDETRANS(RefReg,Crude,Source,Step,ActivePeriod)) -
  sum((Source,Crude,Step,ActivePeriod),
    npv_NonUSCrudeDemandPrice(Source,Crude,Step,ActivePeriod) * CRUDENONUS(Source,Crude,Step,ActivePeriod) ) +
  sum((EthStream,Step,ActivePeriod),
   npv_EthImpSupPrc(EthStream,Step,ActivePeriod)*ETHIMPUSsup(EthStream,Step,ActivePeriod)  ) -
* revenue is negative in OBJ
  sum((EthStream,Step,ActivePeriod),
     npv_EthExpDmdPrc(EthStream,Step,ActivePeriod)*ETHEXPUSdmd(EthStream,Step,ActivePeriod)  ) +
* sum((EthStream,Step,ActivePeriod),
*  npv_EthBrazilPrc(EthStream,Step,ActivePeriod)*ETH_BRAZIL(EthStream,Step,ActivePeriod)  ) +
  sum((RefReg,EthStream,ActivePeriod),
   npv_TranCostToBrazil(ActivePeriod)*ETHEXP(RefReg,EthStream,ActivePeriod)  ) +
  sum((RefReg('1_REFREG'),EthStream('ETHCRNexp'),ActivePeriod),
   -250 *ETHEXP_2(RefReg,EthStream,ActivePeriod)  ) +
  sum((RefReg,EthStream,ActivePeriod),
   npv_TranCostFromBrazil(ActivePeriod)*ETHIMP(RefReg,EthStream,ActivePeriod)  ) +
  sum((RefReg,RefType,Utility,ActivePeriod),
    npv_UtilityPrice(RefReg,Utility,ActivePeriod)*UTILPURCH(RefReg,RefType,Utility,ActivePeriod) ) +
  sum((RefReg,RefInputStr,Step,ActivePeriod),
    npv_RefInpPrc(RefReg,RefInputStr,Step,ActivePeriod)*REFPURCH(RefReg,RefInputStr,Step,ActivePeriod) ) +
  sum((CoalDReg,BioStr,Step,ActivePeriod),
    npv_BiomassPrc(CoalDreg,BioStr,Step,ActivePeriod)* BIOPURCH(CoalDReg,BioStr,Step,ActivePeriod) ) +
  sum((CoalSReg,CoalStr,Step,ActivePeriod),
    npv_CoalPrc(CoalSReg,CoalStr,Step,ActivePeriod)*COALPURCH(CoalSReg,CoalStr,Step,ActivePeriod) ) +
  sum((RefReg,RefType,RcpMode,ActivePeriod),
    npv_RecipeOVC(RcpMode,ActivePeriod)*RECIPEMODE(RefReg,RefType,RcpMode,ActivePeriod) ) +
  sum((RefReg,ActiveDem,TranMode,RecipeProd,ActivePeriod)$REFtoCDTranCost(RefReg,ActiveDem,TranMode,RecipeProd),
     npv_REFtoCDTranCost(RefReg,ActiveDem,TranMode,RecipeProd,ActivePeriod)*RefCenTRAN(RefReg,ActiveDem,TranMode,RecipeProd,ActivePeriod) ) +
  sum((RefReg,RefRegA,TranMode,Stream,ActivePeriod)$REFtoREFTranCost(RefReg,RefRegA,TranMode,Stream),
     npv_REFtoREFTranCost(RefReg,RefRegA,TranMode,Stream,ActivePeriod)*RefRefTRAN(RefReg,RefRegA,TranMode,Stream,ActivePeriod) ) +
  sum((ActiveDem,EndProduct,Step,ActivePeriod),
     npv_GlobalImpPrice(ActiveDem,EndProduct,Step,ActivePeriod)*DISTRESSIMPORT(ActiveDem,EndProduct,Step,ActivePeriod) ) +
  sum((RefReg,RefType,Process,ActivePeriod),
     npv_FXOCCost(Process,RefReg,ActivePeriod)*OPERATECAP(RefReg,RefType,Process,ActivePeriod) ) +
  sum((RefReg,RefType,Process,BldStep,ActivePeriod)$FuturePeriods(ActivePeriod),
     npv_BuildCost(Process,RefReg,BldStep,ActivePeriod)*BUILDS(RefReg,RefType,Process,BldStep,ActivePeriod) )  -
  sum((RefReg,RefType,CoProductSales,ActivePeriod), npv_CoproductPrice(CoProductSales,RefReg,ActivePeriod)*COPRODUCTS(RefReg,RefType,CoProductSales,ActivePeriod) ) +
  sum((E85_Steps,ActiveDem,ActivePeriod), P_E85STP(E85_Steps,ActiveDem,ActivePeriod)*E85STP(E85_Steps,ActiveDem,ActivePeriod) ) +
  sum((ActivePeriod,ActiveDem), npv_TotalMarkups(ActivePeriod,'M','T',ActiveDem)*(QMG10(ActiveDem,ActivePeriod)+QMGb16(ActiveDem,ActivePeriod))*npv_CFMGQCD(ActiveDem,ActivePeriod) ) +
  sum((ActivePeriod,ActiveDem), npv_TotalMarkups(ActivePeriod,'M','T',ActiveDem)*QMG15(ActiveDem,ActivePeriod)*(npv_CFMGQCD(ActiveDem,ActivePeriod)-0.11) ) +
  sum((ActivePeriod,ActiveDem), npv_E85TotalMarkups(ActivePeriod,ActiveDem)*QE85(ActiveDem,ActivePeriod)*npv_CFE85Q(ActivePeriod) ) +
  sum((RFSCategory,ActivePeriod), npv_RFSWaiverPrice(RFSCategory,ActivePeriod)*RFSESCAPE(RFSCategory,ActivePeriod) ) +
  sum((RFSCategory,PrcPeriod(ActivePeriod)), npv_RFSCreditPrice(RFSCategory,ActivePeriod) * RFS_RQM_CST(RFSCategory,PrcPeriod) ) +
  sum((RFSCategory,PrcPeriod(ActivePeriod)), -1.0*npv_RFSCreditPrice(RFSCategory,ActivePeriod) * RFS_PRD_VAL(RFSCategory,PrcPeriod) ) +
  sum((LCFS_PetCategory,PrcPeriod(ActivePeriod))$(LCFSOPT), npv_LCFS_Price(LCFS_PetCategory,ActivePeriod) * LCFS_Pet_CST(LCFS_PetCategory,PrcPeriod) ) +
  sum((LCFS_PetCategory,PrcPeriod(ActivePeriod))$(LCFSOPT), -npv_LCFS_Price(LCFS_PetCategory,ActivePeriod) * LCFS_Bio_Sub(LCFS_PetCategory,PrcPeriod) ) +
  sum((CFP_PetCategory,PrcPeriod(ActivePeriod)), npv_CFP_Price(CFP_PetCategory,ActivePeriod) * CFP_Pet_CST(CFP_PetCategory,PrcPeriod) ) +
  sum((CFP_PetCategory,PrcPeriod(ActivePeriod)), -npv_CFP_Price(CFP_PetCategory,ActivePeriod) * CFP_Bio_Sub(CFP_PetCategory,PrcPeriod) ) +
  sum((CoalSReg,CoalDReg,CoalStr,ActivePeriod),npv_CoalTrnspCst(CoalSReg,CoalDReg,CoalStr,ActivePeriod)*COALTRANSP(CoalSReg,CoalDReg,CoalStr,ActivePeriod)) +
  sum((MX_SO2,CoalStr,ActivePeriod),npv_SO2Prc(CoalStr,MX_SO2,ActivePeriod) * SO2CREDITS(MX_SO2,ActivePeriod)) +
  sum((CoalDReg,CoalStr,ActivePeriod), npv_HGPrc(CoalDReg,CoalStr,ActivePeriod) * HGCREDITS(ActivePeriod) ) -
  sum((Stream,Step,ActivePeriod),npv_MCCPrice(Stream,Step,ActivePeriod) * MCCDEMAND(Stream,Step,ActivePeriod) ) +
  sum((ImpStr,INTREG,Step,ActivePeriod), npv_ImportPrice(ImpStr,INTREG,Step,ActivePeriod)*PRODIMP(ImpStr,INTREG,Step,ActivePeriod) ) -
  sum((ImpStr,INTREG,Step,ActivePeriod), npv_ExportPrice(ImpStr,INTREG,Step,ActivePeriod)*PRODEXP(ImpStr,INTREG,Step,ActivePeriod) ) +

* incentivize renewD production using 0.10 $(nom)/bbl revenue (for AEO2022)
* NOTE:  ERROR BELOW: should not 'sum' over Stream because multiplied -0.10 * number of streams(568) to get OBJ = -56.8
* sum((Stream,ActivePeriod), -0.1 * ToSPECBLEND('7_RefReg','COKING','DSU','RDH',ActivePeriod)$StreamSpecProd('RDH','DSU') )    +
* sum((Stream,ActivePeriod), -0.1 * ToSPECBLEND('4_RefReg','COKING','DSU','RDH',ActivePeriod)$StreamSpecProd('RDH','DSU') )    +
* NOTE:  ERROR ABOVE is corrected below
  sum((ActivePeriod), -60. * ToSPECBLEND('7_RefReg','COKING','DSU','RDH',ActivePeriod)$StreamSpecProd('RDH','DSU') )    +
  sum((ActivePeriod), -60. * ToSPECBLEND('4_RefReg','COKING','DSU','RDH',ActivePeriod)$StreamSpecProd('RDH','DSU') )    +

* incentivize renewD production using 60.0 $(nom)/bbl revenue (for AEO2022)
  sum((ActivePeriod), -60. * ToRECIPEBLEND('7_RefReg','COKING','RCP_carbDSU2','RDH',ActivePeriod)$RecipeBlending('RCP_carbDSU2','RDH') ) +
  sum((ActivePeriod), -60. * ToRECIPEBLEND('4_RefReg','COKING','RCP_carbDSU2','RDH',ActivePeriod)$RecipeBlending('RCP_carbDSU2','RDH') ) +
  sum((ActivePeriod), -60. * ToRECIPEBLEND('7_RefReg','COKING','RCP_DSU2','RDH',ActivePeriod)$RecipeBlending('RCP_DSU2','RDH') ) +
  sum((ActivePeriod), -60. * ToRECIPEBLEND('4_RefReg','COKING','RCP_DSU2','RDH',ActivePeriod)$RecipeBlending('RCP_DSU2','RDH') ) +

  sum((INTREG,DomRefReg,ImpStr,ActivePeriod), npv_INTtoREFTranCost(INTREG,DomRefReg,ActivePeriod)*PRODImpTRAN(INTREG,DomRefReg,ImpStr,ActivePeriod) ) +
  sum((DomRefReg,INTREG,ImpStr,ActivePeriod), npv_REFtoINTTranCost(DomRefReg,INTREG,ActivePeriod)*PRODExpTRAN(DomRefReg,INTREG,ImpStr,ActivePeriod) ) +
  sum((DomRefReg,IntReg,RefType,ActivePeriod), npv_INTtoREFFBDTranCost(IntReg,DomRefReg,ActivePeriod)*BIODIMPref(DomRefReg,RefType,ActivePeriod) ) +
  sum((LCFS_PetCategory,ActivePeriod)$(LCFSOPT), npv_LCFSSafetyPrice(LCFS_PetCategory,ActivePeriod)*LCFSSafety(LCFS_PetCategory,ActivePeriod)) +
  sum((CFP_PetCategory,ActivePeriod), npv_CFPSafetyPrice(CFP_PetCategory,ActivePeriod)*CFPSafety(CFP_PetCategory,ActivePeriod)) +
  sum(ActivePeriod, npv_AB32_AllowPrice(ActivePeriod) * AB32_PurchAllow(ActivePeriod) ) +
  sum((Step,ActivePeriod), npv_FBDImpPrice(Step,ActivePeriod)*BIODIMP(Step,ActivePeriod) ) +
  sum((Step,ActivePeriod), npv_RDHImpPrice(Step,ActivePeriod)*RENEWDIMP(Step,ActivePeriod) ) +
  sum((NGLProduct,ActiveDem,ActivePeriod), npv_NGLImportCost(ActiveDem,NGLProduct,ActivePeriod)*NGLIMPORTS(NGLProduct,ActiveDem,ActivePeriod) ) -
  sum((NGLProduct,ActiveDem,ActivePeriod), npv_NGLExportCost(ActiveDem,NGLProduct,ActivePeriod)*NGLEXPORTS(NGLProduct,ActiveDem,ActivePeriod) ) +
  sum((Stream,ActivePeriod)$AB32_StartYr(Stream), npv_AB32_AllowPrice(ActivePeriod)*AB32_BenchFactor*IMPORTS(Stream,'7_RefReg',ActivePeriod)/1000 )$(AB32SW) +
  sum((RefRegA,TranMode,Stream,ActivePeriod)$(npv_REFtoREFTranCost(RefRegA,'7_RefReg',TranMode,Stream,ActivePeriod) and AB32_StartYr(Stream)),
     npv_AB32_AllowPrice(ActivePeriod)*AB32_BenchFactor*RefRefTRAN(RefRegA,'7_RefReg',TranMode,Stream,ActivePeriod)/1000 )$(AB32SW) +
  sum((RecipeEnd,ActiveDem,ActivePeriod), npv_CarbonTax(RecipeEnd,ActiveDem,ActivePeriod)*RECIPESUPPLY(ActiveDem,RecipeEnd,ActivePeriod) ) +
  sum((ActivePeriod,ActiveDem),
    npv_CarbonTax('CFGout',ActiveDem,ActivePeriod)*QMG10(ActiveDem,ActivePeriod) +
    npv_CarbonTax('CFG15out',ActiveDem,ActivePeriod)*QMG15(ActiveDem,ActivePeriod) +
    npv_CarbonTax('CFGb16out',ActiveDem,ActivePeriod)*QMGb16(ActiveDem,ActivePeriod) +
    npv_CarbonTax('E85out',ActiveDem,ActivePeriod)*QE85(ActiveDem,ActivePeriod) ) +
*  sum((DomRefReg,RefType,Process,ProcessMode,ActivePeriod)$ProcessTable('CO2CCS',Process,ProcessMode),
*    npv_CO2TranStoreCost(DomRefReg,ActivePeriod) * PROCMODE(DomRefReg,RefType,Process,ProcessMode,ActivePeriod) * ProcessTable('CO2CCS',Process,ProcessMode) ) +
  sum((DomRefReg,RefType,Process,ProcessMode,ActivePeriod)
    $(ProcessTable('CO2',Process,ProcessMode) and
     not SameAs(Process,'BTL') and not SameAs(Process,'BPU') and
     not SameAs(Process,'CLE') and not SameAs(Process,'NCE') and
     not SameAs(Process,'EDH') and not SameAs(Process,'EDM') and
     not SameAs(Process,'SEW') and not SameAs(Process,'AET') ),
    npv_CO2EmissionTax(ActivePeriod) * PROCMODE(DomRefReg,RefType,Process,ProcessMode,ActivePeriod) * ProcessTable('CO2',Process,ProcessMode) ) -
  sum((AllBioTaxStr,DomRefReg,RefType,Process,ProcessMode,ActivePeriod)$(ProcessTable(AllBioTaxStr,Process,ProcessMode) and RefTypProc(RefType,Process) and SupTypMode(DomRefReg,RefType,Process,ProcessMode)),
    npv_CarbonTaxCredit(AllBioTaxStr,ActivePeriod) * PROCMODE(DomRefReg,RefType,Process,ProcessMode,ActivePeriod) * ProcessTable(AllBioTaxStr,Process,ProcessMode) ) -
  sum((DomRefReg,EthStream('ETHAETbrz'),ActivePeriod),
    npv_CarbonTaxCredit(EthStream,ActivePeriod) * ETHIMP(DomRefReg,EthStream,ActivePeriod) ) +
  sum((OGCO2Reg,OGCO2Reg2,ActivePeriod),
    npv_CO2_OGTranCost(OGCO2Reg,OGCO2Reg2,ActivePeriod)*CO2_TRAN(OGCO2Reg,OGCO2Reg2,ActivePeriod) ) +
  sum((OGCO2Reg,CO2_Source,ActivePeriod),
    npv_CO2_Price(OGCO2Reg,CO2_Source,ActivePeriod)*CO2_PURCH(OGCO2Reg,CO2_Source,ActivePeriod) ) +
  sum((FuelRegion,ActivePeriod),
    npv_FRtoSalineCost(FuelRegion,ActivePeriod)*CO2toSALINE(FuelRegion,ActivePeriod) ) +
  sum((FuelRegion,OGCO2Reg,ActivePeriod),
    npv_FRtoEORCost(FuelRegion,OGCO2Reg,ActivePeriod)*CO2toEOR(FuelRegion,OGCO2Reg,ActivePeriod) ) +
  sum((OGCO2Reg,ActivePeriod),
    npv_CO2_SafetyPrice(OGCO2Reg,ActivePeriod)*CO2_SAFETY(OGCO2Reg,ActivePeriod) ) -
  sum((Stream,DomRefReg,RefType,Process,ProcessMode,ActivePeriod)$((ProcessTable(stream,Process,ProcessMode)>0) and
                                                                   RefTypProc(RefType,Process) and
                                                                   SupTypMode(DomRefReg,RefType,Process,ProcessMode) and
                                                                   (npv_BiofuelSubsidy(Stream,ActivePeriod)>0)),
    npv_BiofuelSubsidy(Stream,ActivePeriod) * PROCMODE(DomRefReg,RefType,Process,ProcessMode,ActivePeriod) * ProcessTable(Stream,Process,ProcessMode) ) -

*-- NOTE: BioDieselStr and BIODIMPref refer ONLY to stream type FBD
  sum((Stream,DomRefReg,RefType,ActivePeriod)$(BioDieselStr(Stream)),
    (npv_BiofuelSubsidy(Stream,ActivePeriod) * BIODIMPref(DomRefReg,RefType,ActivePeriod)) )  -

*-- NOTE: RenewDieselStr and RENEWDIMPref refer ONLY to stream type RDH
  sum((Stream,DomRefReg,RefType,ActivePeriod)$(RenewDieselStr(Stream)),
    (npv_BiofuelSubsidy(Stream,ActivePeriod) * RENEWDIMPref(DomRefReg,RefType,ActivePeriod)) ) +


  sum((STEO_Category,Step,ActivePeriod)$npv_STEO_Penalty(Step,ActivePeriod),
    npv_STEO_Penalty(Step,ActivePeriod)*(STEOplus(STEO_Category,Step,ActivePeriod) + STEOminus(STEO_Category,Step,ActivePeriod)) )
  =E= 0 ;


CrudeBalance(RefReg,Crude,ActivePeriod)..
   sum((PetRefType,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),
     ProcessTable(Crude,Process,ProcessMode)* PROCMODE(RefReg,PetRefType,Process,ProcessMode,ActivePeriod)$SupTypMode(RefReg,PetRefType,Process,ProcessMode) ) +
   sum((Source,Step), CRUDETRANS(RefReg,Crude,Source,Step,ActivePeriod) ) +
   npv_DomesticCrudeSup(RefReg,Crude,ActivePeriod) +
   npv_SPR_Withdraw(RefReg,Crude,ActivePeriod) -
   sum((Source,Step), CRUDEEXPORT(RefReg,Crude,Source,Step,ActivePeriod)$npv_CrudeExportCap(RefReg,Source,Step,ActivePeriod) ) -
   sum((RefRegA,TranMode)$npv_REFtoREFTranCost(RefReg,RefRegA,TranMode,Crude,ActivePeriod), RefRefTRAN(RefReg,RefRegA,TranMode,Crude,ActivePeriod) ) +
   sum((RefRegA,TranMode)$npv_REFtoREFTranCost(RefRegA,RefReg,TranMode,Crude,ActivePeriod), RefRefTRAN(RefRegA,RefReg,TranMode,Crude,ActivePeriod) )
   =E= 0 ;

CrudeSupCurveForeign(Source,Crude,ActivePeriod)..
   sum(Step$npv_CrudeSupplyIncremental(Source,Crude,Step,ActivePeriod), CRUDEPURCH(Source,Crude,Step,ActivePeriod)) +
   sum((RefReg,Step), CRUDEEXPORT(RefReg,Crude,Source,Step,ActivePeriod)$npv_CrudeExportCap(RefReg,Source,Step,ActivePeriod) ) -
   sum((RefReg,Step), CRUDETRANS(RefReg,Crude,Source,Step,ActivePeriod) ) -
   sum(Step,CRUDENONUS(Source,Crude,Step,ActivePeriod))
   =G= 0 ;

$ontext
CrudeImportsSTEO(ActivePeriod)..
   sum((DomRefReg,Crude,Source,Step)$npv_CrudeImportCap(DomRefReg,Source,Step,ActivePeriod),
     CRUDETRANS(DomRefReg,Crude,Source,Step,ActivePeriod) )
   =E=
   STEO_VOLUME('CrudeImports',ActivePeriod) +
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOplus('CrudeImports',Step,ActivePeriod)) -
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOminus('CrudeImports',Step,ActivePeriod)) ;

CrudeExportsSTEO(ActivePeriod)..
   sum((DomRefReg,Crude,Source,Step)$npv_CrudeExportCap(DomRefReg,Source,Step,ActivePeriod),
     CRUDEEXPORT(DomRefReg,Crude,Source,Step,ActivePeriod) )
   =E=
   STEO_VOLUME('CrudeExports',ActivePeriod) +
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOplus('CrudeExports',Step,ActivePeriod)) -
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOminus('CrudeExports',Step,ActivePeriod)) ;
$offtext

$ontext
NetCrudeImportsSTEO(ActivePeriod)$STEO_ApplyContraints..
   sum((DomRefReg,Crude,Source,Step)$npv_CrudeImportCap(DomRefReg,Source,Step,ActivePeriod),
     CRUDETRANS(DomRefReg,Crude,Source,Step,ActivePeriod) ) -
   sum((DomRefReg,Crude,Source,Step)$npv_CrudeExportCap(DomRefReg,Source,Step,ActivePeriod),
     CRUDEEXPORT(DomRefReg,Crude,Source,Step,ActivePeriod) )
   =E=
   STEO_VOLUME('NetCrudeImports',ActivePeriod) +
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOplus('NetCrudeImports',Step,ActivePeriod)) -
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOminus('NetCrudeImports',Step,ActivePeriod)) ;
$offtext

TotalCrudeInputsSTEO(ActivePeriod)$STEO_ApplyContraints..
   sum((DomRefReg,PetRefType,Crude,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),
     abs(ProcessTable(Crude,Process,ProcessMode)) *
     PROCMODE(DomRefReg,PetRefType,Process,ProcessMode,ActivePeriod)$SupTypMode(DomRefReg,PetRefType,Process,ProcessMode) )
   =E=
   STEO_VOLUME('TotalCrudeInputs',ActivePeriod) +
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOplus('TotalCrudeInputs',Step,ActivePeriod)) -
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOminus('TotalCrudeInputs',Step,ActivePeriod)) ;


NetUFOImportsSTEO(ActivePeriod)$STEO_ApplyContraints..
   sum((UFOImpStr,DomRefReg), IMPORTS(UFOImpStr,DomRefReg,ActivePeriod) ) -
   sum((UFOImpStr,DomRefReg), EXPORTS(UFOImpStr,DomRefReg,ActivePeriod) )
   =E=
   STEO_VOLUME('NetUFOImports',ActivePeriod) +
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOplus('NetUFOImports',Step,ActivePeriod)) -
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOminus('NetUFOImports',Step,ActivePeriod)) ;

NetProdImportsSTEO(ActivePeriod)$STEO_ApplyContraints..
  (sum((ImpStr,DomRefReg),
     IMPORTS(ImpStr,DomRefReg,ActivePeriod) )  +
   sum((DomRefReg,TranMode,ImpStr),
     RefRefTRAN('9_REFREG',DomRefReg,TranMode,ImpStr,ActivePeriod) ) +
   sum((DomRefReg,TranMode,Stream('LPGout')),
     RefRefTRAN('9_REFREG',DomRefReg,TranMode,Stream,ActivePeriod) ) +
   sum((NGLProduct,DomRefReg,ActiveDem),
     CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS(NGLProduct,ActiveDem,ActivePeriod) ) ) -
  (sum((ImpStr,DomRefReg),
     EXPORTS(ImpStr,DomRefReg,ActivePeriod) ) +
   sum((NGLProduct,DomRefReg,ActiveDem),
     CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLEXPORTS(NGLProduct,ActiveDem,ActivePeriod) ) +
   sum((DomRefReg,RefType),
     COPRODUCTS(DomRefReg,RefType,'COKdump',ActivePeriod) ) +
   sum((DomRefReg,RefType),
     COPRODUCTS(DomRefReg,RefType,'GOPout',ActivePeriod) ) )
   =E=
   STEO_VOLUME('NetProductImports',ActivePeriod) +
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOplus('NetProductImports',Step,ActivePeriod)) -
   sum(Step$npv_STEO_Penalty(Step,ActivePeriod), STEOminus('NetProductImports',Step,ActivePeriod)) ;

CrudeImportLimit(RefReg,Source,Step,ActivePeriod)$(sum(crude,npv_CrudeImportCost(RefReg,Source,Crude,Step,ActivePeriod))>0)..
   sum(Crude, CRUDETRANS(RefReg,Crude,Source,Step,ActivePeriod) ) -
   npv_CrudeImportCap(RefReg,Source,Step,ActivePeriod)
   =L= 0 ;

CrudeExportLimit(RefReg,Source,Step,ActivePeriod)$npv_CrudeExportCap(RefReg,Source,Step,ActivePeriod)..
   sum(Crude, CRUDEEXPORT(RefReg,Crude,Source,Step,ActivePeriod) ) -
   npv_CrudeExportCap(RefReg,Source,Step,ActivePeriod)
   =L= 0 ;

*-- Limit crude exports to Canada from RR3
*-- hardcoded, max of 393 M bbl/cd allowed to Sarnia
CrudeExportLimit3CAN(Source,ActivePeriod)..
   sum( (Reg3CrdExp,Step), CRUDEEXPORT('3_RefReg',Reg3CrdExp,Source,Step,ActivePeriod)$npv_CrudeExportCap('3_RefReg',Source,Step,ActivePeriod) )
   =L= 393 ;

ExportDomCrudeOnly(RefReg,Crude,ActivePeriod)..
   npv_DomesticCrudeSup(RefReg,Crude,ActivePeriod) +
   npv_SPR_Withdraw(RefReg,Crude,ActivePeriod) +
   sum((RefRegA,TranMode)$npv_REFtoREFTranCost(RefRegA,RefReg,TranMode,Crude,ActivePeriod), RefRefTRAN(RefRegA,RefReg,TranMode,Crude,ActivePeriod) ) -
   sum((Source,Step), CRUDEEXPORT(RefReg,Crude,Source,Step,ActivePeriod)$npv_CrudeExportCap(RefReg,Source,Step,ActivePeriod) )
   =G= 0 ;

WorldCrudeSup(ActivePeriod)..
   sum(Step$npv_CrudeSupplyTotal(Step,ActivePeriod), CRUDETOTAL(Step,ActivePeriod)) -
   sum((Source,Crude,Step)$npv_CrudeSupplyIncremental(Source,Crude,Step,ActivePeriod),
     CRUDEPURCH(Source,Crude,Step,ActivePeriod)) -
   sum((RefReg,Crude), npv_DomesticCrudeSup(RefReg,Crude,ActivePeriod) + npv_SPR_Withdraw(RefReg,Crude,ActivePeriod))
   =G= 0 ;

ProdImpBalance(DomRefReg,ImpStr,ActivePeriod)..
   IMPORTS(ImpStr,DomRefReg,ActivePeriod) -
   sum(INTREG, PRODImpTRAN(INTREG,DomRefReg,ImpStr,ActivePeriod) )
   =E= 0 ;

ProdImpSupCurve(INTREG,ImpStr,ActivePeriod)..
   sum(Step, PRODIMP(ImpStr,INTREG,Step,ActivePeriod) ) -
   sum(DomRefReg, PRODImpTRAN(INTREG,DomRefReg,ImpStr,ActivePeriod) )
   =G= 0 ;

ProdExpBalance(DomRefReg,ImpStr,ActivePeriod)..
   EXPORTS(ImpStr,DomRefReg,ActivePeriod) -
   sum(INTREG, PRODExpTRAN(DomRefReg,INTREG,ImpStr,ActivePeriod) )
   =E= 0 ;

ProdExpSupCurve(INTREG,ImpStr,ActivePeriod)..
   sum(Step, PRODEXP(ImpStr,INTREG,Step,ActivePeriod) ) -
   sum(DomRefReg, PRODExpTRAN(DomRefReg,INTREG,ImpStr,ActivePeriod) )
   =L= 0 ;

ImpTranLimit(INTREG,DomRefReg,ActivePeriod)..
  sum(ImpStr, PRODImpTRAN(INTREG,DomRefReg,ImpStr,ActivePeriod) ) -
  INTtoREFTranCap(INTREG,DomRefReg)
  =L= 0 ;

ExpTranLimit(DomRefReg,INTREG,ActivePeriod)..
  sum(ImpStr, PRODExpTRAN(DomRefReg,INTREG,ImpStr,ActivePeriod) ) -
  REFtoINTTranCap(DomRefReg,INTREG)
  =L= 0 ;

UtilBalance(RefReg,RefType,Utility,ActivePeriod)..
   sum((Process,ProcessMode)$ProcessTable(Utility,Process,ProcessMode) ,
     ProcessTable(Utility,Process,ProcessMode)* PROCMODE(RefReg,RefType,Process,ProcessMode,ActivePeriod)$SupTypMode(RefReg,RefType,Process,ProcessMode)) +
   UTILPURCH(RefReg,RefType,Utility,ActivePeriod)
   =E= 0 ;

* removed 2-8-19
*EthWorldBal(ActivePeriod)..
*   sum(EthStream,
*     sum(Step,ETH_BRAZIL(EthStream,Step,ActivePeriod)) -
*     sum(RefReg, ETHIMP(RefReg,EthStream,ActivePeriod) ) ) -
*     BrzEthExpNonUs(ActivePeriod)
*   =G= 0 ;
*
* replace with:
EthImpUSBal(ActivePeriod)..
   sum(EthStream,
     sum(Step,ETHIMPUSsup(EthStream,Step,ActivePeriod)) -
     sum(RefReg,ETHIMP(RefReg,EthStream,ActivePeriod)) )
   =E= 0 ;

* removed 2-8-19
*BrzEthProdBal(ActivePeriod)..
*   BrzEthExpNonUs(ActivePeriod) +
*   sum(EthStream,
*     sum(RefReg, ETHEXP(RefReg,EthStream,ActivePeriod)) ) -
*   ETHNonUS(ActivePeriod)
*   =G= 0 ;
*
* replace with:
EthExpUSBal(ActivePeriod)..
   sum(EthStream,
     sum(RefReg,ETHEXP(RefReg,EthStream,ActivePeriod)) -
     sum(Step,ETHEXPUSdmd(EthStream,Step,ActivePeriod)) )
   =E= 0 ;

* The following is an approximation
* Would be better to use "Exports to US" + "Exports elsewhere" < max
* But BrzEthExpNonUs apparently includes BRZ domestic consumption?
* removed 2-8-19 em4
*BrzMaxExportsToUS(EthStream,ActivePeriod)..
*   (0.50 * ETH_BRAZIL(EthStream,'STEP01',ActivePeriod)) -
*   sum(RefReg, ETHIMP(RefReg,EthStream,ActivePeriod))
*   =G= 0 ;

EthBalance(RefReg,EthStream,ActivePeriod)..
   ETHIMP(RefReg,EthStream,ActivePeriod) - ETHEXP(RefReg,EthStream,ActivePeriod) - ETHEXP_2(RefReg,EthStream,ActivePeriod) +
   sum((RefRegA,TranMode)$npv_REFtoREFTranCost(RefRegA,RefReg,TranMode,EthStream,ActivePeriod),RefRefTRAN(RefRegA,RefReg,TranMode,EthStream,ActivePeriod)) +
   sum((Process,ProcessMode)$ProcessTable(EthStream,Process,ProcessMode),
     sum(EthRefType,ProcessTable(EthStream,Process,ProcessMode)* PROCMODE(RefReg,EthRefType,Process,ProcessMode,ActivePeriod)$SupTypMode(RefReg,EthRefType,Process,ProcessMode) ) ) -
   sum((RefRegA,TranMode)$npv_REFtoREFTranCost(RefReg,RefRegA,TranMode,EthStream,ActivePeriod),RefRefTRAN(RefReg,RefRegA,TranMode,EthStream,ActivePeriod))  -
   sum(RcpMode$RecipeBlending(RcpMode,EthStream), sum(RefType,ToRECIPEBLEND(RefReg,RefType,RcpMode,EthStream,ActivePeriod) ) )
  =E= 0 ;

* EthTranLimit(RefReg,RefRegA,TranMode,ActivePeriod)..
*    sum(EthStream$npv_REFtoREFTranCost(RefReg,RefRegA,TranMode,EthStream,ActivePeriod), RefRefTRAN(RefReg,RefRegA,TranMode,EthStream,ActivePeriod) ) -
*    npv_REFtoREFTranCap(RefReg,RefRegA,TranMode,ActivePeriod)
*    =L= 0 ;

RefInpBalance(RefReg,RefType,RefInputStr,ActivePeriod)..
   REFPURused(RefReg,RefType,RefInputStr,ActivePeriod) +
   sum((Process,ProcessMode)$ProcessTable(RefInputStr,Process,ProcessMode),
     ProcessTable(RefInputStr,Process,ProcessMode)* PROCMODE(RefReg,RefType,Process,ProcessMode,ActivePeriod)$SupTypMode(RefReg,RefType,Process,ProcessMode)) -
   sum(RcpMode$RecipeBlending(RcpMode,RefInputStr), ToRECIPEBLEND(RefReg,RefType,RcpMode,RefInputStr,ActivePeriod) ) -
   sum(GasSpecProd, ToSPECBLEND(RefReg,RefType,GasSpecProd,RefInputStr,ActivePeriod)$StreamSpecProd(RefInputStr,GasSpecProd) ) -
   sum(DistSpecProd, ToSPECBLEND(RefReg,RefType,DistSpecProd,RefInputStr,ActivePeriod)$StreamSpecProd(RefInputStr,DistSpecProd) ) -
   sum(ResidSpecProd, ToSPECBLEND(RefReg,RefType,ResidSpecProd,RefInputStr,ActivePeriod)$StreamSpecProd(RefInputStr,ResidSpecProd) )
   =E= 0 ;

RefPurchBal(RefReg,RefInputStr,ActivePeriod)..
   sum(step,REFPURCH(RefReg,RefInputStr,Step,ActivePeriod)) - sum(Reftype,REFPURused(RefReg,RefType,RefInputStr,ActivePeriod))+
   IMPORTS(RefInputStr,RefReg,ActivePeriod) - EXPORTS(RefInputStr,RefReg,ActivePeriod)
   =E= 0 ;

BioBalance(CoalDReg,BioStr,ActivePeriod)..
   sum(Step,BIOPURCH(CoalDReg,BioStr,Step,ActivePeriod)) -
   sum(Other_MKT,BIODEMAND(CoalDReg,BioStr,Other_MKT,ActivePeriod)) -
   sum(DomRefReg$CoalDToRefMap(CoalDReg,DomRefReg),BIOXFER(CoalDReg,DomRefReg,BioStr,ActivePeriod))
   =E= 0 ;

* mc6 To Do: in lfprep and GDX, change CoalDToRefMap from parameter to set (0/1)
BioRefRegBal(DomRefReg,BioStr,ActivePeriod)..
   sum(CoalDReg$CoalDToRefMap(CoalDReg,DomRefReg), BIOXFER(CoalDReg,DomRefReg,BioStr,ActivePeriod) ) +
   sum((RefType,Process,ProcessMode)$(ProcessTable(BioStr,Process,ProcessMode) and SupTypMode(DomRefReg,RefType,Process,ProcessMode)),
      ProcessTable(BioStr,Process,ProcessMode)* PROCMODE(DomRefReg,RefType,Process,ProcessMode,ActivePeriod) )
      =G= 0 ;

CoalDemBalance(CoalDReg,CoalStr,ActivePeriod)$XCL_CLDR(CoalDReg)..
   sum(CoalSReg$npv_CoalTrnspCst(CoalSReg,CoalDReg,CoalStr,ActivePeriod),COALTRANSP(CoalSReg,CoalDReg,CoalStr,ActivePeriod)) -
   sum(RefReg,COALXFER(CoalDReg,RefReg,CoalStr,ActivePeriod))
   =G= 0 ;

CoalSupBalance(CoalSReg,CoalStr,ActivePeriod)$XCL_Type(CoalSReg)..
   sum(step,COALPURCH(CoalSReg,CoalStr,Step,ActivePeriod))  -
   COALDEMAND(CoalSReg,CoalStr,ActivePeriod) -
   sum(CoalDReg$npv_CoalTrnspCst(CoalSReg,CoalDReg,CoalStr,ActivePeriod),COALTRANSP(CoalSReg,CoalDReg,CoalStr,ActivePeriod))
   =G= 0 ;

CoalRefRegBal(RefReg,CoalStr,ActivePeriod)..
   sum(CoalDReg$XCL_CDSL1(CoalDReg,RefReg),COALXFER(CoalDReg,RefReg,CoalStr,ActivePeriod) ) +
     sum((RefType,Process,ProcessMode)$(ProcessTable(CoalStr,Process,ProcessMode) and SupTypMode(RefReg,RefType,Process,ProcessMode)),
       ProcessTable(CoalStr,Process,ProcessMode)* PROCMODE(RefReg,RefType,Process,ProcessMode,ActivePeriod) )
      =G= 0 ;

SO2EmisBal(MX_SO2,ActivePeriod)$(sum(CoalStr,npv_SO2Prc(CoalStr,MX_SO2,ActivePeriod)) > 0.0 and ord(MX_SO2) > XCL_NUM_SO2_GRP)..
   sum((CoalSReg,CoalDReg,CoalStr),npv_SO2Emission(CoalSReg,CoalDReg,CoalStr,MX_SO2,ActivePeriod)* COALTRANSP(CoalSReg,CoalDReg,CoalStr,ActivePeriod)) - SO2CREDITS(MX_SO2,ActivePeriod)
   =L= 0 ;

HGEmisBal(ActivePeriod)$(sum((CoalDReg,CoalStr), npv_HGPrc('CoalDReg1',CoalStr,ActivePeriod)>0))..
   sum((CoalSReg,CoalDReg,CoalStr),npv_HGEmission(CoalSReg,CoalDReg,CoalStr,ActivePeriod) * COALTRANSP(CoalSReg,CoalDReg,CoalStr,ActivePeriod)) - HGCREDITS(ActivePeriod)
   =L= 0 ;

BiodieselBalance(ActivePeriod)..
   sum(Step, BIODIMP(Step,ActivePeriod)) -
   sum((RefType,RefReg)$(RefRegBiodieselImportsAllowed(RefReg)), BIODIMPref(RefReg,RefType,ActivePeriod) )
   =G= 0 ;

RenewDieselBalance(ActivePeriod)..
   sum(Step, RENEWDIMP(Step,ActivePeriod)) -
   sum((RefType,RefReg)$(RefRegBiodieselImportsAllowed(RefReg)), RENEWDIMPref(RefReg,RefType,ActivePeriod) )
   =G= 0 ;


* Non crude/utility balance constraints
StreamBalance(RefReg,RefType,NonCrudeUtil,ActivePeriod)..
   (BIODIMPref(RefReg,RefType,ActivePeriod))$BioDieselStr(NonCrudeUtil) +
   (RENEWDIMPref(RefReg,RefType,ActivePeriod))$RenewDieselStr(NonCrudeUtil) +
   (IMPORTS(NonCrudeUtil,RefReg,ActivePeriod) -  EXPORTS(NonCrudeUtil,RefReg,ActivePeriod))$(ORD(RefType)=1) +
   sum((Process,ProcessMode)$(ProcessTable(NonCrudeUtil,Process,ProcessMode) and SupTypMode(RefReg,RefType,Process,ProcessMode)),
     ProcessTable(NonCrudeUtil,Process,ProcessMode)* PROCMODE(RefReg,RefType,Process,ProcessMode,ActivePeriod)) -
   sum(RcpMode$RecipeBlending(RcpMode,NonCrudeUtil), ToRECIPEBLEND(RefReg,RefType,RcpMode,NonCrudeUtil,ActivePeriod) ) -
   sum(GasSpecProd, ToSPECBLEND(RefReg,RefType,GasSpecProd,NonCrudeUtil,ActivePeriod)$StreamSpecProd(NonCrudeUtil,GasSpecProd) ) -
   sum(DistSpecProd, ToSPECBLEND(RefReg,RefType,DistSpecProd,NonCrudeUtil,ActivePeriod)$StreamSpecProd(NonCrudeUtil,DistSpecProd) ) -
   sum(ResidSpecProd, ToSPECBLEND(RefReg,RefType,ResidSpecProd,NonCrudeUtil,ActivePeriod)$StreamSpecProd(NonCrudeUtil,ResidSpecProd) )
   =E= 0 ;

CapacityBalance(RefReg,RefType,Process,Period)$ActivePeriod(Period)..
   sum(ProcessMode, CapFactor(Process,ProcessMode)*PROCMODE(RefReg,RefType,Process,ProcessMode,Period) ) -
   StreamFactors(Process)*OPERATECAP(RefReg,RefType,Process,Period) -
   StreamFactors(Process)*sum((BldStep,Period2)$(ord(Period2)<=ord(Period) and ord(Period2)>=BldPerIdx),
                            BUILDS(RefReg,RefType,Process,BldStep,Period2))
   =L= 0 ;

*CapacityBalance(RefReg,RefType,Process,Period)$RefTypProc(RefType,Process)..
*   sum(ProcessMode$SupTypMode(RefReg,RefType,Process,ProcessMode), CapFactor(Process,ProcessMode)*PROCMODE(RefReg,RefType,Process,ProcessMode,Period) ) -
*   StreamFactors(Process)*OPERATECAP(RefReg,RefType,Process,Period) -
*   StreamFactors(Process)*sum((BldStep,Period2)$(ord(Period2)<=ord(Period) and ord(Period2)>=BldPerIdx),
*                            BUILDS(RefReg,RefType,Process,BldStep,Period2))
*   =L= 0 ;

* Limit growth of ACU-debottlenecking pseudo-unit LTE to 10% of original capacity
* do not build row for Per1
MaxLTE(RefReg,PetRefType,Period)$FuturePeriods(Period)..
   AvailCap(RefReg,PetRefType,'LTE') +
   sum((BldStep,Period2)$(ord(Period2)<=ord(Period) and ord(Period2)>=BldPerIdx),
     BUILDS(RefReg,PetRefType,'LTE',BldStep,Period2) ) -
   1.35 * ExistingCap(RefReg,PetRefType,'LTE','2008')
   =L= 0 ;

RestrictGrowth(Process,BldStep,Period)$(ActivePeriod(Period) and (ord(Period)>1) and AFGrowthRate(Process) and (not sameas(Process,'CTL')) and (not sameas(Process,'CTLCCS')) and (not sameas(Process,'CBL')) and (not sameas(Process,'CBLCCS')) )..
   sum((RefReg,RefType), BUILDS(RefReg,RefType,Process,BldStep,Period) ) -
   npv_MaxBuilds(Process,BldStep,Period)
   =L= 0 ;

* Restrict growth of CTL and CTLCCS using the CTL growth rate
RestrictGrowthCTL(BldStep,Period)$(ActivePeriod(Period) and (ord(Period)>1) and AFGrowthRate('CTL'))..
   sum((RefReg,RefType), BUILDS(RefReg,RefType,'CTL',BldStep,Period) + BUILDS(RefReg,RefType,'CTLCCS',BldStep,Period) ) -
   npv_MaxBuilds('CTL',BldStep,Period)
   =L= 0 ;

* Restrict growth of CTL, CTLCCS, GTL using the CTL growth rate
RestrictGrowthCTLGTL(BldStep,Period)$(ActivePeriod(Period) and (ord(Period)>1) and AFGrowthRate('CTL'))..
   sum((RefReg,RefType), BUILDS(RefReg,RefType,'CTL',BldStep,Period) +
                         BUILDS(RefReg,RefType,'CTLCCS',BldStep,Period) +
                         BUILDS(RefReg,RefType,'GTL',BldStep,Period) ) -
   npv_MaxBuilds('CTL',BldStep,Period)
   =L= 0 ;

* Restrict growth of CBL and CBLCCS using the CBL growth rate
RestrictGrowthCBL(BldStep,Period)$(ActivePeriod(Period) and (ord(Period)>1) and AFGrowthRate('CBL'))..
   sum((RefReg,RefType), BUILDS(RefReg,RefType,'CBL',BldStep,Period) + BUILDS(RefReg,RefType,'CBLCCS',BldStep,Period) ) -
   npv_MaxBuilds('CBL',BldStep,Period)
   =L= 0 ;

* Gasoline product specification constraints
GasSpecQualMin(RefReg,RefType,GasSpecProd,GasProp,ActivePeriod)$npv_GasSpecMin(GasSpecProd,GasProp,ActivePeriod)..
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,GasSpecProd), StreamProp(NonCrudeUtil,GasProp)*ToSPECBLEND(RefReg,RefType,GasSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,GasSpecProd), npv_GasSpecMin(GasSpecProd,GasProp,ActivePeriod)*ToSPECBLEND(RefReg,RefType,GasSpecProd,NonCrudeUtil,ActivePeriod) )
   =G= 0 ;

GasSpecQualMax(RefReg,RefType,GasSpecProd,GasProp,ActivePeriod)$npv_GasSpecMax(GasSpecProd,GasProp,ActivePeriod)..
  (sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,GasSpecProd), StreamProp(NonCrudeUtil,GasProp)*(141.5/(131.5+StreamProp(NonCrudeUtil,'API')))*ToSPECBLEND(RefReg,RefType,GasSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,GasSpecProd), npv_GasSpecMax(GasSpecProd,GasProp,ActivePeriod)*(141.5/(131.5+StreamProp(NonCrudeUtil,'API')))*ToSPECBLEND(RefReg,RefType,GasSpecProd,NonCrudeUtil,ActivePeriod) ))$GasSulfurSpec(GasProp) +
  (sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,GasSpecProd), StreamProp(NonCrudeUtil,GasProp)*ToSPECBLEND(RefReg,RefType,GasSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,GasSpecProd), npv_GasSpecMax(GasSpecProd,GasProp,ActivePeriod)*ToSPECBLEND(RefReg,RefType,GasSpecProd,NonCrudeUtil,ActivePeriod) ))$(not GasSulfurSpec(GasProp))
   =L= 0 ;

GasSpecBalance(RefReg,RefType,GasSpecProd,ActivePeriod)..
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,GasSpecProd), ToSPECBLEND(RefReg,RefType,GasSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(RcpMode$RecipeBlending(RcpMode,GasSpecProd),ToRECIPEBLEND(RefReg,RefType,RcpMode,GasSpecProd,ActivePeriod)) +
   ImpToSPECBLEND(RefReg,RefType,GasSpecProd,ActivePeriod)$(CRBOB(GasSpecProd) and PetRefType(RefType)) -
   ExpFromSPECBLEND(RefReg,RefType,GasSpecProd,ActivePeriod)$(CRBOB(GasSpecProd) and PetRefType(RefType))
   =E= 0 ;

BOBBalance(RefReg,GasSpecProd,ActivePeriod)..
   IMPORTS(GasSpecProd,RefReg,ActivePeriod) -
   sum(PetRefType,ImpToSPECBLEND(RefReg,PetRefType,GasSpecProd,ActivePeriod)$CRBOB(GasSpecProd)) -
   EXPORTS(GasSpecProd,RefReg,ActivePeriod) +
   sum(PetRefType,ExpFromSPECBLEND(RefReg,PetRefType,GasSpecProd,ActivePeriod)$CRBOB(GasSpecProd)) +
   sum((TranMode,RefRegA)$REFtoREFTranCost(RefRegA,RefReg,TranMode,GasSpecProd),RefRefTRAN(RefRegA,RefReg,TranMode,GasSpecProd,ActivePeriod)) -
   sum((TranMode,RefRegA)$REFtoREFTranCost(RefReg,RefRegA,TranMode,GasSpecProd),RefRefTRAN(RefReg,RefRegA,TranMode,GasSpecProd,ActivePeriod))
   =E= 0 ;

* Distillate product specification constraints
DistSpecQualMin(RefReg,RefType,DistSpecProd,DistProp,ActivePeriod)$npv_DistSpecMin(DistSpecProd,DistProp,ActivePeriod)..
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,DistSpecProd), StreamProp(NonCrudeUtil,DistProp)*ToSPECBLEND(RefReg,RefType,DistSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,DistSpecProd), npv_DistSpecMin(DistSpecProd,DistProp,ActivePeriod)*ToSPECBLEND(RefReg,RefType,DistSpecProd,NonCrudeUtil,ActivePeriod) )
   =G= 0 ;

DistSpecQualMax(RefReg,RefType,DistSpecProd,DistProp,ActivePeriod)$npv_DistSpecMax(DistSpecProd,DistProp,ActivePeriod)..
  (sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,DistSpecProd), StreamProp(NonCrudeUtil,DistProp)*(141.5/(131.5+StreamProp(NonCrudeUtil,'API')))*ToSPECBLEND(RefReg,RefType,DistSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,DistSpecProd), npv_DistSpecMax(DistSpecProd,DistProp,ActivePeriod)*(141.5/(131.5+StreamProp(NonCrudeUtil,'API')))*ToSPECBLEND(RefReg,RefType,DistSpecProd,NonCrudeUtil,ActivePeriod) ))$DistSulfurSpec(DistProp) +
  (sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,DistSpecProd), StreamProp(NonCrudeUtil,DistProp)*ToSPECBLEND(RefReg,RefType,DistSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,DistSpecProd), npv_DistSpecMax(DistSpecProd,DistProp,ActivePeriod)*ToSPECBLEND(RefReg,RefType,DistSpecProd,NonCrudeUtil,ActivePeriod) ))$(not DistSulfurSpec(DistProp))
   =L= 0 ;

DistSpecBalance(RefReg,RefType,DistSpecProd,ActivePeriod)..
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,DistSpecProd), ToSPECBLEND(RefReg,RefType,DistSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(RcpMode$RecipeBlending(RcpMode,DistSpecProd),ToRECIPEBLEND(RefReg,RefType,RcpMode,DistSpecProd,ActivePeriod) )
   =E= 0 ;

* Resid product specification constraints
ResidSpecQualMin(RefReg,RefType,ResidSpecProd,ResidProp,ActivePeriod)$npv_ResidSpecMin(ResidSpecProd,ResidProp,ActivePeriod)..
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,ResidSpecProd), StreamProp(NonCrudeUtil,ResidProp)*ToSPECBLEND(RefReg,RefType,ResidSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,ResidSpecProd), npv_ResidSpecMin(ResidSpecProd,ResidProp,ActivePeriod)*ToSPECBLEND(RefReg,RefType,ResidSpecProd,NonCrudeUtil,ActivePeriod) )
   =G= 0 ;

ResidSpecQualMax(RefReg,RefType,ResidSpecProd,ResidProp,ActivePeriod)$npv_ResidSpecMax(ResidSpecProd,ResidProp,ActivePeriod)..
  (sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,ResidSpecProd), StreamProp(NonCrudeUtil,ResidProp)*(141.5/(131.5+StreamProp(NonCrudeUtil,'API')))*ToSPECBLEND(RefReg,RefType,ResidSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,ResidSpecProd), npv_ResidSpecMax(ResidSpecProd,ResidProp,ActivePeriod)*(141.5/(131.5+StreamProp(NonCrudeUtil,'API')))*ToSPECBLEND(RefReg,RefType,ResidSpecProd,NonCrudeUtil,ActivePeriod) ))$ResidSulfurSpec(ResidProp) +
  (sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,ResidSpecProd), StreamProp(NonCrudeUtil,ResidProp)*ToSPECBLEND(RefReg,RefType,ResidSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,ResidSpecProd), npv_ResidSpecMax(ResidSpecProd,ResidProp,ActivePeriod)*ToSPECBLEND(RefReg,RefType,ResidSpecProd,NonCrudeUtil,ActivePeriod) ))$(not ResidSulfurSpec(ResidProp))
   =L= 0 ;

ResidSpecBalance(RefReg,RefType,ResidSpecProd,ActivePeriod)..
   sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,ResidSpecProd), ToSPECBLEND(RefReg,RefType,ResidSpecProd,NonCrudeUtil,ActivePeriod) ) -
   sum(RcpMode$RecipeBlending(RcpMode,ResidSpecProd),ToRECIPEBLEND(RefReg,RefType,RcpMode,ResidSpecProd,ActivePeriod) )
   =E= 0 ;

* Recipe volume balance between input streams and recipes
RecipeBalance(RefReg,RefType,RcpMode,RecipeInputs,ActivePeriod)$RecipeBlending(RcpMode,RecipeInputs)..
    ToRECIPEBLEND(RefReg,RefType,RcpMode,RecipeInputs,ActivePeriod) +
    RecipeBlending(RcpMode,RecipeInputs)*RECIPEMODE(RefReg,RefType,RcpMode,ActivePeriod)
    =E= 0 ;

* Transfer recipes that satisfy demands into products for consumption
RecipeTransfer(RefReg,RefType,RecipeProd,ActivePeriod)..
   sum(RcpMode$RecipeBlending(RcpMode,RecipeProd), RECIPEMODE(RefReg,RefType,RcpMode,ActivePeriod) ) -
   RECIPETOPROD(RefReg,RefType,RecipeProd,ActivePeriod)
   =E= 0 ;

* Transfer co-products into products for sales
RecipeBPTransfer(RefReg,RefType,CoProduct,ActivePeriod)..
   sum(RcpMode$RecipeBlending(RcpMode,CoProduct), RECIPEMODE(RefReg,RefType,RcpMode,ActivePeriod) ) -
   COPRODUCTS(RefReg,RefType,CoProduct,ActivePeriod)
   =E= 0 ;

* Combine supply region product supply across refinery types
CombineSupply(RefReg,RecipeProd,ActivePeriod)..
  IMPORTS(RecipeProd,RefReg,ActivePeriod)+
  sum((TranMode,RefRegA)$REFtoREFTranCost(RefRegA,RefReg,TranMode,RecipeProd),RefRefTRAN(RefRegA,RefReg,TranMode,RecipeProd,ActivePeriod)) -
  EXPORTS(RecipeProd,RefReg,ActivePeriod)-
  sum((TranMode,RefRegA)$REFtoREFTranCost(RefReg,RefRegA,TranMode,RecipeProd),RefRefTRAN(RefReg,RefRegA,TranMode,RecipeProd,ActivePeriod)) +
  sum(RefType,RECIPETOPROD(RefReg,RefType,RecipeProd,ActivePeriod)) -
  sum(Step,MCCDEMAND(RecipeProd,Step,ActivePeriod)$(ord(RefReg) = 9))-
  TOTPROD(RefReg,RecipeProd,ActivePeriod)
  =E= 0 ;

* Equate total recipe product supply in each census division to total production
CDSupplyTot(ActiveDem,RecipeProd,ActivePeriod)..
   CDSUPPLY(ActiveDem,RecipeProd,ActivePeriod) -
   sum(RefReg, PRODTOCENSUS(RefReg,ActiveDem,RecipeProd,ActivePeriod))
   =E= 0 ;

REFtoREFTran(RefReg,RefRegA,TranMode,ActivePeriod)$(not MarineMode(TranMode))..
   sum(Stream$npv_REFtoREFTranCost(RefReg,RefRegA,TranMode,Stream,ActivePeriod),
     RefRefTRAN(RefReg,RefRegA,TranMode,Stream,ActivePeriod) ) -
   npv_REFtoREFTranCap(RefReg,RefRegA,TranMode,ActivePeriod)
   =L= 0 ;

* Distribute end products from supply regions to the census divisions they serve
TotPRODTran(RefReg,RecipeProd,ActivePeriod)..
   TOTPROD(RefReg,RecipeProd,ActivePeriod) -
   sum(ActiveDem, PRODTOCENSUS(RefReg,ActiveDem,RecipeProd,ActivePeriod))
   =E= 0 ;

REFtoCDTran(RefReg,ActiveDem,RecipeProd,ActivePeriod)..
   PRODTOCENSUS(RefReg,ActiveDem,RecipeProd,ActivePeriod) -
   sum(TranMode$REFtoCDTranCost(RefReg,ActiveDem,TranMode,RecipeProd),
     RefCenTRAN(RefReg,ActiveDem,TranMode,RecipeProd,ActivePeriod))
   =E= 0 ;

* Enforce non-marine/barge transport capacity bounds over all valid RefReg/Census combos
REFtoCDCap(RefReg,ActiveDem,NonMarineMode,ActivePeriod)..
   sum(RecipeProd$REFtoCDTranCost(RefReg,ActiveDem,NonMarineMode,RecipeProd),
     RefCenTRAN(RefReg,ActiveDem,NonMarineMode,RecipeProd,ActivePeriod)) -
   REFtoCDTranCap(RefReg,ActiveDem,NonMarineMode)
   =L= 0 ;

* Enforce marine/barge transport capacity bounds over all valid RefReg-RefReg combos (0.002 DWT/bbl product)
REFtoREFCapMB(RefReg,RefRegA,MarineMode,ActivePeriod)..
   0.002 * sum(Stream$REFtoREFTranCost(RefReg,RefRegA,MarineMode,Stream),
             RefRefTRAN(RefReg,RefRegA,MarineMode,Stream,ActivePeriod)) -
           npv_REFtoREFTranCap(RefReg,RefRegA,MarineMode,ActivePeriod)
   =L= 0 ;

* Enforce demands for recipe-blended products
RecipeEndBal(ActiveDem,RecipeEnd,ActivePeriod)..
   CDSUPPLY(ActiveDem,RecipeEnd,ActivePeriod) + sum(step,DISTRESSIMPORT(ActiveDem,RecipeEnd,Step,ActivePeriod)) -
   RECIPESUPPLY(ActiveDem,RecipeEnd,ActivePeriod)
   =E= 0 ;

RecipeDemands(ActiveDem,RecipeEnd,ActivePeriod)..
   RECIPESUPPLY(ActiveDem,RecipeEnd,ActivePeriod) -
   npv_ProductDemand(ActiveDem,RecipeEnd,ActivePeriod)
   =E= 0 ;

NGLEndBal(ActiveDem,NGLProduct,ActivePeriod)..
   CDSUPPLY(ActiveDem,NGLProduct,ActivePeriod) +
   NGLIMPORTS(NGLProduct,ActiveDem,ActivePeriod) -
   NGLEXPORTS(NGLProduct,ActiveDem,ActivePeriod) -
   RECIPESUPPLY(ActiveDem,NGLProduct,ActivePeriod)
   =E= 0 ;

NGLDmd(ActiveDem,NGLProduct,ActivePeriod)..
   RECIPESUPPLY(ActiveDem,NGLProduct,ActivePeriod) -
   npv_NGLDemands(ActiveDem,NGLProduct,ActivePeriod)
   =E= 0 ;

E85Balance(ActiveDem,ActivePeriod)..
   CDSUPPLY(ActiveDem,'E85out',ActivePeriod) -
   QE85(ActiveDem,ActivePeriod)
   =E= 0 ;

CFGBalance(ActiveDem,ActivePeriod)..
   CDSUPPLY(ActiveDem,'CFGout',ActivePeriod) + sum(step,DISTRESSIMPORT(ActiveDem,'CFGout',Step,ActivePeriod)) -
   MGShare('CFGout',ActiveDem)*QMG10(ActiveDem,ActivePeriod)
   =E= 0 ;

RFGBalance(ActiveDem,ActivePeriod)..
   CDSUPPLY(ActiveDem,'RFGout',ActivePeriod) + sum(step,DISTRESSIMPORT(ActiveDem,'RFGout',Step,ActivePeriod)) -
   MGShare('RFGout',ActiveDem)*QMG10(ActiveDem,ActivePeriod)
   =E= 0 ;

CaRBOBBalance(ActiveDem,ActivePeriod)..
   CDSUPPLY(ActiveDem,'CaRBOBout',ActivePeriod) + sum(step,DISTRESSIMPORT(ActiveDem,'CaRBOBout',Step,ActivePeriod)) -
   MGShare('CaRBOBout',ActiveDem)*QMG10(ActiveDem,ActivePeriod)
   =E= 0 ;

CFG15Balance(ActiveDem,ActivePeriod)..
   CDSUPPLY(ActiveDem,'CFG15out',ActivePeriod) -
   MGShare('CFGout',ActiveDem)*QMG15(ActiveDem,ActivePeriod)
   =E= 0 ;

RFG15Balance(ActiveDem,ActivePeriod)..
   CDSUPPLY(ActiveDem,'RFG15out',ActivePeriod) -
   MGShare('RFGout',ActiveDem)*QMG15(ActiveDem,ActivePeriod)
   =E= 0 ;

CFGb16Balance(ActiveDem,ActivePeriod)..
   CDSUPPLY(ActiveDem,'CFGb16out',ActivePeriod) -
   MGShare('CFGout',ActiveDem)*QMGb16(ActiveDem,ActivePeriod)
   =E= 0 ;

RFGb16Balance(ActiveDem,ActivePeriod)..
   CDSUPPLY(ActiveDem,'RFGb16out',ActivePeriod) -
   MGShare('RFGout',ActiveDem)*QMGb16(ActiveDem,ActivePeriod)
   =E= 0 ;

E15Max(ActiveDem,ActivePeriod)..
   QMG15(ActiveDem,ActivePeriod) -
   npv_E15MaxPen(ActiveDem,ActivePeriod)*QMG(ActiveDem,ActivePeriod)
   =L= 0 ;

* For ActivePeriod 1 assign RFS Credit Prices from Previous Forecast Year Solution - Else Transfer Credit from Production to Requirement Constraint
* Divide ethanol terms by (1 - 0.0249) to account for the 2.49% (no longer 4.5%) additional denaturant added later
* EXCLUDE renewable fuels going to heating oil (N2H)
RFSConstraintsPRD(RFSCategory,ActivePeriod)..
    sum( (DomRefReg,RefType,Process('GDT'),Stream('RDH'))$( RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream) and RefRegBiodieselImportsAllowed(DomRefReg) ),
       RFSScores(Process,Stream) * RENEWDIMPref(DomRefReg,RefType,ActivePeriod) ) +
    sum( (DomRefReg,RefType,Process('FBD'),Stream('FBD'))$( RFSProcCategory(RFSCategory,Stream)and RFSScores(Process,Stream) and RefRegBiodieselImportsAllowed(DomRefReg) ),
       RFSScores(Process,Stream) * BIODIMPref(DomRefReg,RefType,ActivePeriod) ) +

    sum((SupTypMode(DomRefReg,RefType,Process,ProcessMode),Stream)$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
       RFSScores(Process,Stream) * ProcessTable(Stream,Process,ProcessMode) * PROCMODE(DomRefReg,RefType,Process,ProcessMode,ActivePeriod)) -

    sum( (DomRefReg,RefType,Process,Stream('FBD'),RcpMode('RCP_N2H1'))$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
       RFSScores(Process,Stream) * ToRECIPEBLEND(DomRefReg,RefType,RcpMode,Stream,ActivePeriod) ) -

    sum( (DomRefReg,RefType,BioDistillate,SpecProd('N2H'),Process)$(RFSProcCategory(RFSCategory,BioDistillate) and RFSScores(Process,BioDistillate) and (not sameas(BioDistillate,'FBD'))),
           RFSScores(Process,BioDistillate) * ToSPECBLEND(DomRefReg,RefType,SpecProd,BioDistillate,ActivePeriod) ) +

    sum((DomRefReg,RefType,RcpMode,EthStreamRFS)$(RFSProcCategory(RFSCategory,EthStreamRFS) and RecipeBlending(RcpMode,EthStreamRFS)),
       ToRECIPEBLEND(DomRefReg,RefType,RcpMode,EthStreamRFS,ActivePeriod)/(1.-0.0249) ) -
    RFS_PRD_to_RQM(RFSCategory,ActivePeriod) -
    sum(PrcPeriod(ActivePeriod),RFS_PRD_VAL(RFSCategory,PrcPeriod))
   =G= 0;

RFSLimitCornEthCredits(ActivePeriod)..
        sum((DomRefReg,RefType,RcpMode(EthCornBlends),EthStreamRFS('ETHCRN')),
       ToRECIPEBLEND(DomRefReg,RefType,RcpMode,EthStreamRFS,ActivePeriod)/(1.-0.0249) * MbblCD_2_BGY)
   =L= 15 ;

RFSConstraintsRQM(RFSCategory,ActivePeriod)..
    RFS_PRD_to_RQM(RFSCategory,ActivePeriod) +
    sum(PrcPeriod(ActivePeriod),RFS_RQM_CST(RFSCategory,PrcPeriod)) +
    sum(RFSCategoryA$RFSCategoryWaiver(RFSCategory,RFSCategoryA),RFSESCAPE(RFSCategoryA,ActivePeriod)) -
    RFSFraction(RFSCategory,ActivePeriod)*(
      npv_PetMGFrac_noETH(ActivePeriod)*sum(ActiveDem,
         0.90*QMG10(ActiveDem,ActivePeriod) +
         0.85*QMG15(ActiveDem,ActivePeriod) +
         0.84*QMGb16(ActiveDem,ActivePeriod) +
         abs(RecipeBlending('RCP_E85a','CBOB'))*QE85(ActiveDem,ActivePeriod) ) +
      npv_PetDSFrac(ActivePeriod)*sum(ActiveDem, CDSUPPLY(ActiveDem,'DSUout',ActivePeriod) + CDSUPPLY(ActiveDem,'CarbDSUout',ActivePeriod) ) )
   =G= 0;

*  For ActivePeriod 1 assin LCFS Price from Previous Forecast Year Solution - Else Transfer from Petroleum Deficit to BioFuel Relieve
LCFS_Petroleum(LCFS_PetCategory,ActivePeriod)$(LCFSOPT)..
   LCFS_Pet_to_Bio(LCFS_PetCategory,ActivePeriod) +
   sum(PrcPeriod(ActivePeriod),LCFS_Pet_Cst(LCFS_PetCategory,PrcPeriod)) -
   sum((LCFS_RefReg,RefType,LCFS_PetStreams),
     npv_LCFS_Pet_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_PetStreams,ActivePeriod) *
     RecipetoProd(LCFS_RefReg,RefType,LCFS_PetStreams,ActivePeriod)) -
   sum((RefRegA,LCFS_RefReg,TranMode,LCFS_PetStreams)$REFtoREFTranCost(RefRegA,LCFS_RefReg,TranMode,LCFS_PetStreams),
     npv_LCFS_Pet_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_PetStreams,ActivePeriod) *
     RefRefTRAN(RefRegA,LCFS_RefReg,TranMode,LCFS_PetStreams,ActivePeriod)) -
   sum((LCFS_RefReg,LCFS_PetStreams),
     npv_LCFS_Pet_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_PetStreams,ActivePeriod) *
     IMPORTS(LCFS_PetStreams,LCFS_RefReg,ActivePeriod))
   =G= 0;

* 8-27-19, added BIODIMP to LCFS constraint
LCFS_BioFuel(LCFS_PetCategory,ActivePeriod)$(LCFSOPT)..
   LCFSSafety(LCFS_PetCategory,ActivePeriod)$(not PrcPeriod(ActivePeriod)) -
   npv_LCFS_LFGas_CFactor(LCFS_PetCategory,ActivePeriod) *
     LCFS_LandfillGas(LCFS_PetCategory,ActivePeriod) -
   sum(LCFS_Vehicle_Types,npv_LCFS_Alt_CFactor(LCFS_PetCategory,LCFS_Vehicle_Types,ActivePeriod) *
     LCFS_AltVehicles(LCFS_PetCategory,LCFS_Vehicle_Types,ActivePeriod)) -
   sum(SupTypMode(LCFS_RefReg,RefType,LCFS_BioProcess,LCFS_BioMode),
     npv_LCFS_Bio_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioProcess,LCFS_BioMode,ActivePeriod) *
     ProcMode(LCFS_RefReg,RefType,LCFS_BioProcess,LCFS_BioMode,ActivePeriod)) -
   sum((RefRegA,LCFS_RefReg,TranMode,LCFS_BioStreams(EthStream))$REFtoREFTranCost(RefRegA,LCFS_RefReg,TranMode,LCFS_BioStreams),
     npv_LCFS_Eth_CFactor(LCFS_PetCategory,LCFS_BioStreams,ActivePeriod) *
     RefRefTRAN(RefRegA,LCFS_RefReg,TranMode,LCFS_BioStreams,ActivePeriod)) +
   sum((RefRegA,LCFS_RefReg,TranMode,LCFS_BioStreams(EthStream))$REFtoREFTranCost(LCFS_RefReg,RefRegA,TranMode,LCFS_BioStreams),
     npv_LCFS_Eth_CFactor(LCFS_PetCategory,LCFS_BioStreams,ActivePeriod) *
     RefRefTRAN(LCFS_RefReg,RefRegA,TranMode,LCFS_BioStreams,ActivePeriod)) -
   sum((LCFS_RefReg,LCFS_BioImports),
     npv_LCFS_Imp_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioImports,ActivePeriod) *
     ETHIMP(LCFS_RefReg,LCFS_BioImports,ActivePeriod)) -

   sum((LCFS_RefReg,RefType),
     npv_LCFS_BiodImp_CFactor(LCFS_PetCategory,LCFS_RefReg,ActivePeriod) *
     BIODIMPref(LCFS_RefReg,RefType,ActivePeriod)) -

   sum((LCFS_RefReg,RefType),
     npv_LCFS_RenewDImp_CFactor(LCFS_PetCategory,LCFS_RefReg,ActivePeriod) *
     RENEWDIMPref(LCFS_RefReg,RefType,ActivePeriod)) -

   LCFS_Pet_to_Bio(LCFS_PetCategory,ActivePeriod) -
   sum(PrcPeriod(ActivePeriod),LCFS_Bio_Sub(LCFS_PetCategory,PrcPeriod))
   =G= 0;

*  allow landfill gas to split between pet categories, based on model-determined need;
*  such that total in each year/period is not exceeded
*  trillion BTU per year
LCFS_LandfillGasConstr(Period)$(LCFSOPT)..
   sum(LCFS_PetCategory, LCFS_LandfillGas(LCFS_PetCategory,Period)) -
   npv_LCFS_LandfillGas_total(Period)
   =L= 0;

* 3-17-22, esh: adding in CFP constraints
* structured the same way as LCFS constraints
*  For ActivePeriod 1 assin CFP Price from Previous Forecast Year Solution - Else Transfer from Petroleum Deficit to BioFuel Relieve
CFP_Petroleum(CFP_PetCategory,ActivePeriod)..
   CFP_Pet_to_Bio(CFP_PetCategory,ActivePeriod) +
   sum(PrcPeriod(ActivePeriod),CFP_Pet_Cst(CFP_PetCategory,PrcPeriod)) -
   sum((CFP_RefReg,RefType,CFP_PetStreams)$(CFP_PetStreams('DSUout')),
     npv_CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,ActivePeriod) *
     0.22 * RecipetoProd(CFP_RefReg,RefType,CFP_PetStreams,ActivePeriod)) -
   sum((CFP_RefReg,RefType,CFP_PetStreams)$(not CFP_PetStreams('DSUout')),
     npv_CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,ActivePeriod) *
     0.17 * RecipetoProd(CFP_RefReg,RefType,CFP_PetStreams,ActivePeriod)) -
   sum((RefRegA,CFP_RefReg,TranMode,CFP_PetStreams)$((REFtoREFTranCost(RefRegA,CFP_RefReg,TranMode,CFP_PetStreams)) and (CFP_PetStreams('DSUout'))),
     npv_CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,ActivePeriod) *
     0.22 * RefRefTRAN(RefRegA,CFP_RefReg,TranMode,CFP_PetStreams,ActivePeriod)) -
   sum((RefRegA,CFP_RefReg,TranMode,CFP_PetStreams)$((REFtoREFTranCost(RefRegA,CFP_RefReg,TranMode,CFP_PetStreams)) and (not CFP_PetStreams('DSUout'))),
     npv_CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,ActivePeriod) *
     0.17 * RefRefTRAN(RefRegA,CFP_RefReg,TranMode,CFP_PetStreams,ActivePeriod)) -
   sum((CFP_RefReg,CFP_PetStreams)$(CFP_PetStreams('DSUout')),
     npv_CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,ActivePeriod) *
     0.22  * IMPORTS(CFP_PetStreams,CFP_RefReg,ActivePeriod)) -
   sum((CFP_RefReg,CFP_PetStreams)$(not CFP_PetStreams('DSUout')),
     npv_CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,ActivePeriod) *
     0.17 * IMPORTS(CFP_PetStreams,CFP_RefReg,ActivePeriod))
   =G= 0;

CFP_BioFuel(CFP_PetCategory,ActivePeriod)..
   CFPSafety(CFP_PetCategory,ActivePeriod)$(not PrcPeriod(ActivePeriod)) -
   npv_CFP_LFGas_CFactor(CFP_PetCategory,ActivePeriod) *
     CFP_LandfillGas(CFP_PetCategory,ActivePeriod) -
   sum(CFP_Vehicle_Types,npv_CFP_Alt_CFactor(CFP_PetCategory,CFP_Vehicle_Types,ActivePeriod) *
     CFP_AltVehicles(CFP_PetCategory,CFP_Vehicle_Types,ActivePeriod)) -
   sum(SupTypMode(CFP_RefReg,RefType,CFP_BioProcess,CFP_BioMode),
     npv_CFP_Bio_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioProcess,CFP_BioMode,ActivePeriod) *
     0.20 * ProcMode(CFP_RefReg,RefType,CFP_BioProcess,CFP_BioMode,ActivePeriod)) -
   sum((RefRegA,CFP_RefReg,TranMode,CFP_BioStreams(EthStream))$REFtoREFTranCost(RefRegA,CFP_RefReg,TranMode,CFP_BioStreams),
     npv_CFP_Eth_CFactor(CFP_PetCategory,CFP_BioStreams,ActivePeriod) *
     0.20 * RefRefTRAN(RefRegA,CFP_RefReg,TranMode,CFP_BioStreams,ActivePeriod)) +
   sum((RefRegA,CFP_RefReg,TranMode,CFP_BioStreams(EthStream))$REFtoREFTranCost(CFP_RefReg,RefRegA,TranMode,CFP_BioStreams),
     npv_CFP_Eth_CFactor(CFP_PetCategory,CFP_BioStreams,ActivePeriod) *
     0.20 * RefRefTRAN(CFP_RefReg,RefRegA,TranMode,CFP_BioStreams,ActivePeriod)) -
   sum((CFP_RefReg,CFP_BioImports),
     npv_CFP_Imp_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioImports,ActivePeriod) *
     ETHIMP(CFP_RefReg,CFP_BioImports,ActivePeriod)) -

   sum((CFP_RefReg,RefType),
     npv_CFP_BiodImp_CFactor(CFP_PetCategory,CFP_RefReg,ActivePeriod) *
     0.22 * BIODIMPref(CFP_RefReg,RefType,ActivePeriod)) -

   sum((CFP_RefReg,RefType),
     npv_CFP_RenewDImp_CFactor(CFP_PetCategory,CFP_RefReg,ActivePeriod) *
     0.22 * RENEWDIMPref(CFP_RefReg,RefType,ActivePeriod)) -

   CFP_Pet_to_Bio(CFP_PetCategory,ActivePeriod) -
   sum(PrcPeriod(ActivePeriod),CFP_Bio_Sub(CFP_PetCategory,PrcPeriod))
   =G= 0;

*  allow landfill gas to split between pet categories, based on model-determined need;
*  such that total in each year/period is not exceeded
*  trillion BTU per year
CFP_LandfillGasConstr(Period)..
   sum(CFP_PetCategory, CFP_LandfillGas(CFP_PetCategory,Period)) -
   npv_CFP_LandfillGas_total(Period)
   =L= 0;

AB32_Constraint(ActivePeriod)$(AB32SW)..
   sum((Stream('CO2'),Process,ProcessMode,RefType)$(ProcessTable(Stream,Process,ProcessMode) and SupTypMode('7_RefReg',RefType,Process,ProcessMode)),
     ProcessTable(Stream,Process,ProcessMode)* PROCMODE('7_RefReg',RefType,Process,ProcessMode,ActivePeriod))
   =L=
   AB32_PurchAllow(ActivePeriod) +
   AB32_BenchFactor * npv_AB32_AssistFactor(ActivePeriod) * npv_AB32_CapAdjFactor(ActivePeriod) *
     ( sum((RefType,RecipeProd(AB32_Stream)),RECIPETOPROD('7_RefReg',RefType,RecipeProd,ActivePeriod)) -
       sum(GasSpecProd, IMPORTS(GasSpecProd,'7_RefReg',ActivePeriod)) -
       sum((RefRegA,TranMode,EthStream)$npv_REFtoREFTranCost(RefRegA,'7_RefReg',TranMode,EthStream,ActivePeriod),RefRefTRAN(RefRegA,'7_RefReg',TranMode,EthStream,ActivePeriod)) );

MGTotal(ActiveDem,ActivePeriod)..
   QMG10(ActiveDem,ActivePeriod) + QMG15(ActiveDem,ActivePeriod) + QMGb16(ActiveDem,ActivePeriod) -
   QMG(ActiveDem,ActivePeriod)
   =E= 0 ;

E85Total(ActiveDem,ActivePeriod)..
   QE85(ActiveDem,ActivePeriod) -
   sum(E85_Steps, E85STP(E85_Steps,ActiveDem,ActivePeriod) )
   =E= 0 ;

FlexBTUDmd(ActiveDem,ActivePeriod)..
   npv_CFMGQCD(ActiveDem,ActivePeriod)*QMG(ActiveDem,ActivePeriod) +
   npv_CFE85Q(ActivePeriod)*sum(E85_Steps, E85STP(E85_Steps,ActiveDem,ActivePeriod) ) -
   npv_BTUDemand(ActiveDem,ActivePeriod)
   =E= 0 ;

* Limit the average Conradson Carbon %wt of the FCC feed to 2% or less
MaxCCRtoFCC(RefReg,RefType,ActivePeriod)..
   sum((Process('FCC'),ProcessMode)$ProcProcMode(Process,ProcessMode),
     sum(stream$FCC_InputStream_Mode(ProcessMode,Stream), StreamProp(Stream,'CC')*PROCMODE(RefReg,RefType,Process,ProcessMode,ActivePeriod) ) )
   =L=
   2.0 * sum((Process('FCC'),ProcessMode)$ProcProcMode(Process,ProcessMode), PROCMODE(RefReg,RefType,Process,ProcessMode,ActivePeriod) ) ;

* Must satisfy OGMS EOR demand over all CO2 sources
* The 'PLANNED_POWER' source is assumed to be already delivered, so no transportation is necessary
OGSM_CO2Demand(OGCO2Reg,ActivePeriod)..
   sum(OGCO2Reg2, CO2_TRAN(OGCO2Reg2,OGCO2Reg,ActivePeriod) ) +
   sum(FuelRegion, CO2toEOR(FuelRegion,OGCO2Reg,ActivePeriod) ) +
   CO2_PURCH(OGCO2Reg,'PLANNED_POWER',ActivePeriod) +
   CO2_SAFETY(OGCO2Reg,ActivePeriod)
   =E=
   npv_OGSMCO2Dem(OGCO2Reg,ActivePeriod) ;

OGSM_CO2TranBal(OGCO2Reg,ActivePeriod)..
   sum(CO2_Source$(not SameAs(CO2_Source,'PLANNED_POWER')), CO2_PURCH(OGCO2Reg,CO2_Source,ActivePeriod) ) -
   sum(OGCO2Reg2, CO2_TRAN(OGCO2Reg,OGCO2Reg2,ActivePeriod) )
   =E= 0 ;

CO2CCSBalance(FuelRegion,ActivePeriod)..
   sum((DomRefReg,RefType,Process('CTLCCS'),ProcessMode)$RefReg_2_FuelRegion(DomRefReg,FuelRegion),
     PROCMODE(DomRefReg,RefType,Process,ProcessMode,ActivePeriod)*ProcessTable('CO2CCS',Process,ProcessMode) ) +
   sum((DomRefReg,RefType,Process('CBLCCS'),ProcessMode)$RefReg_2_FuelRegion(DomRefReg,FuelRegion),
     PROCMODE(DomRefReg,RefType,Process,ProcessMode,ActivePeriod)*ProcessTable('CO2CCS',Process,ProcessMode) ) -
   sum(OGCO2Reg, CO2toEOR(FuelRegion,OGCO2Reg,ActivePeriod) ) -
   CO2toSALINE(FuelRegion,ActivePeriod)
   =E= 0 ;

StateBiodiesel(DomRefReg,ActivePeriod)$(sum(state, npv_State_Biodiesel(DomRefReg,State,ActivePeriod))>0)..
   sum((PetRefType,BiodieselRecipe), RECIPEMODE(DomRefReg,PetRefType,BiodieselRecipe,ActivePeriod) ) -
   sum(state, npv_State_Biodiesel(DomRefReg,State,ActivePeriod))
   =G= 0 ;


Model LFMM  / all / ;







