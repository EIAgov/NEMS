$Title  LFMM model
$inlinecom   /*   */
$Ontext
By: OnLocation, Inc.
$Offtext



$set LFMM_BUILDS 'LFMM_Builds.gdx'
$set NEM_TO_LFMM 'NEM_TO_LFMM1.gdx'


*----------------------------------------------------------------------------------------------------------
*set VerboseDebug ON within lfshell.gms via: $set DEBUG_VERBOSE 1
* or via command-line argument: --DEBUG_VERBOSE=1
$set DEBUG_VERBOSE 0
$if not set DEBUG_VERBOSE $set DEBUG_VERBOSE 0
parameter DebugVerbose;
DebugVerbose = %DEBUG_VERBOSE%;
*display DebugVerbose;
*----------------------------------------------------------------------------------------------------------



*---------------Begin User Settings----------------------
* Set directory path for model input and source code - include back slash at the end
* Use "..\"  for current/run directory
*e.g.$set DirPath "C:\VAF\LFMM\Models\CrudeModeMap\"
$set DirPath ".\"

$onUNDF

$ontext
* To indicate running integrated with NEMS or stand-alone
* NOTE:  LEAVE THIS SET TO 1 EVEN WHEN RUNNING INTEGRATED - STILL UNDER DEVELOPMENT!!!!
Scalar StandAlone                  Set to 1 to run stand-alone - 0 for NEMS integrated ;
       StandAlone                  = 1 ;
$offtext

* To turn reporting on or off
Scalar LFMMReporting               Set to 1 to produce reporting GDX - 0 otherwise ;
       LFMMReporting               = 1 ;


Scalar CreateAccessDB              Set to 1 to produce output database from reporting GDX - 0 otherwise ;
       CreateAccessDB              = 0 ;


* To turn debugging on or off
Scalar DoDebugging                 Set to 1 to turn on debugging - 0 otherwise ;
       DoDebugging                 = 0 ;

* Turn on the refinery capacity calibration output
Scalar DoCalibrate                 Set to 1 to put out the calibration output ;
       DoCalibrate                 = 0 ;

* To set solver...  set to Cplex or Xpress
$set   LPsolver  Xpress
*$set   LPsolver  Cplex


$set dbversion 9
$onecho > nemsdefault.ini
[settings]
dbversion=9
$offecho

*-------------- End User Settings ------------------------------

$onecho > xpress.opt
algorithm      barrier
crossover      1
trace          1
rerun          1
bariterlimit   500
barGapStop     1e-12
*reslim        600
*optimalitytol 1.0e-4
$offecho


$onecho > xpress_simplex.opt
algorithm      simplex
*reslim        600
*crossover     1
*optimalitytol 1.0e-4
$offecho

$ontext
$onecho > cplex.opt
LPmethod 1
$offecho
$offtext



* Unique set element listing
*$onuellist

* Create 'put' file for the 'put_utility' commands
* Not really needed...
*file puttemp; put puttemp ;

file infeas /INFEAS.lst/;
put infeas;

*This macro used to eliminate Undefined and other unacceptable characters when passing data back to NEMS.  See LFReport.gms
* a value of 8 means epsilon - change to 0, everything else make very small
$macro CheckValue(TableValue)   if(Mapval(TableValue) > 0,TableValue = 0.0001; if(Mapval(TableValue) = 8, TableValue = 0;));


*---------------------------------------------------------------------------------------------
* Creating input GDX files is currently an off-line activity that must be performed before a NEMS run.
* Thus, the following code is obsolete (for now)
*
* Call data import subroutine if the input GDX does not exist
* This module writes lfminset.gdx and lfminput.gdx
*$if not exist %DirPath%input\LFMinput.gdx $include %DirPath%LFinput.gms
*$include %DirPath%LFinput.gms
*---------------------------------------------------------------------------------------------


*Call LFMM - NEMS integration data preparation subroutine
$include %DirPath%LF_NEM.gms

* For debugging
*NCNTRL_CURITR = 1 ;
*NCNTRL_FCRL   = 0 ;

*---------------------------------------------------------------------------------------------
$ontext
* Code as delivered from OnLocation
* Temporarily set these for now
If(StandAlone = 1,
  NCNTRL_CURITR_orig = NCNTRL_CURITR ;
  NCNTRL_CURITR      = 1;
);
$offtext

*leftover due to above code. Still used in lfshell debug GDX dumps.
NCNTRL_CURITR_orig = NCNTRL_CURITR ;

*---------------------------------------------------------------------------------------------


* Call data preparation subroutine
$include %DirPath%LFprep.gms


* Pull in lp model
$include %DirPath%LFmodel.gms





* Set solver and algoritm options
option lp = %LPsolver% ;
LFMM.optfile=1;
LFMM.Holdfixed=1;
option limrow = 0 ;
option limcol = 0 ;
option solprint = silent ;
*option solprint = on ;


$ontext
* The following does not work within NEMS, unfortunately
*----------------------------------------------------------------------------------------------------------
file NOHUP /nohup.out/;
NOHUP.AP=1
put NOHUP /;
put NOHUP 'Greetings from LFMM!  -mc6'/;
put NOHUP /;
*----------------------------------------------------------------------------------------------------------
$offtext

* Open the debug file
file LFDBG /LFdebug.txt/;
LFDBG.AP=1;  /* .AP=1 makes GAMS append rather than overwrite */
put LFDBG /;
put LFDBG '*---'/;
put LFDBG 'NEMS Cycle/Year/Iter: ' CYCLEINFO_CURIRUN:0:0 ' - ' NCNTRL_CURCALYR:4:0 ' - '  NCNTRL_CURITR_orig:2:0 /;
put LFDBG 'Solver: ', system.lp/;
put LFDBG 'Computer Date/Time: ', system.date:8 ' - ' system.time /;



*----------------------------------------------------------------------------------------------------------
* maybe belongs in lfprep???

* Load prior build decisions
Parameters
   PriorBuilds(RefReg,RefType,Process,t)  Parameter to store build decisions in build period
;

$if not set LFMM_BUILDS $set LFMM_BUILDS 'LFMM_Builds.gdx'
* Check if the builds gdx file exists. If so, load it
execute 'test -s %LFMM_BUILDS%';
if(errorlevel=0,
  put_utility 'gdxin' / '%LFMM_BUILDS%' ;
  execute_load PriorBuilds ;
*  display '%LFMM_BUILDS% found!' ;
else
  PriorBuilds(RefReg,RefType,Process,t) = 0 ;
  display '%LFMM_BUILDS% NOT found!' ;
);
*----------------------------------------------------------------------------------------------------------


scalar CreateYearlyGDXFile;
CreateYearlyGDXFile=NCNTRL_FCRL;

*NCNTRL_FCRL=1;

sets
 ExtraRiskProc(Process)                        / CTL, CBL / ;

Scalar
 OptLrnFactor                                  apply to TotalProjInv instead of CapRec
;

*---------------------------------------------

* Solve the model for all years in the RunYears set
loop(RunYears,

  CurrentYr(t)        = no  ;
  CurrentYr(RunYears) = yes ;
  CurYrIdx = sum((t,CurrentYr)$CurrentYr(t), ord(t)) ;

  CurrentDeflator = sum((t,ReportYr)$ReportYr(t), GDP(t)) / sum((t,CurrentYr)$CurrentYr(t), GDP(t)) ;

* Calculate total existing capacity for this model year (=existing + planned + prior builds)
  AvailCap(RefReg,RefType,Process) =
    sum(t$(Ord(t)<=CurYrIdx), ExistingCap(RefReg,RefType,Process,t)) +
    sum(t$(Ord(t)< CurYrIdx), PriorBuilds(RefReg,RefType,Process,t)) ;

* 9-29-14 em4, to resolve infeas in Per1 for MaxLTE row
* (also changed lfmodel.gms to not build row for Per1)
  AvailCap(RefReg,RefType,'LTE') $(not sameas(RefType,'ETH_REF')) =
               max(AvailCap(RefReg,RefType,'LTE'),
               ExistingCap(RefReg,RefType,'LTE','2008')) + 0.001 ;

* moved from CapRec adj to TotalProjInv 7-19-2016
*---------------------------------------------
* Apply learning factors to capital costs
*---------------------------------------------

  NumPlants(LearningProcess) =
    sum((RefReg,RefType), AvailCap(RefReg,RefType,LearningProcess) ) / CapExpSize(LearningProcess) ;
  NumPlants(LearningProcess) = max(1.0,NumPlants(LearningProcess));

  LrnFactor(LearningProcess)$(NumPlants(LearningProcess)<=LearningData(LearningProcess,'Phase2','Fast','K')) =
    1 + MoreLearningData(LearningProcess,'Optimism')/NumPlants(LearningProcess) ;

  LrnFactor(LearningProcess)$
    ((NumPlants(LearningProcess)>LearningData(LearningProcess,'Phase2','Fast','K')) and
     (NumPlants(LearningProcess)<=LearningData(LearningProcess,'Phase3','Fast','K'))) =
    (LearningData(LearningProcess,'Phase2','Fast','A') * (NumPlants(LearningProcess)**(-LearningData(LearningProcess,'Phase2','Fast','B'))) +
     LearningData(LearningProcess,'Phase2','Slow','A') * (NumPlants(LearningProcess)**(-LearningData(LearningProcess,'Phase2','Slow','B'))) ) ;

  LrnFactor(LearningProcess)$(NumPlants(LearningProcess)>LearningData(LearningProcess,'Phase3','Fast','K')) =
   (LearningData(LearningProcess,'Phase3','Fast','A') * (NumPlants(LearningProcess)**(-LearningData(LearningProcess,'Phase3','Fast','B'))) +
    LearningData(LearningProcess,'Phase3','Slow','A') * (NumPlants(LearningProcess)**(-LearningData(LearningProcess,'Phase3','Slow','B'))) ) ;

*  CapRec(LearningProcess,RefReg) = CapRec(LearningProcess,RefReg) * LrnFactor(LearningProcess) ;

*  execute_unload 'learn', NumPlants, LrnFactor, CapRec;



* Calculate capital costs for process expansion and fixed operating costs
  loop((t,Process,BldRiskClass)$(CurrentYr(t) and CapExpSize(Process) and ProcessRisk(Process,BldRiskClass)),

    /* Effective tax rate */
    EffectiveTax(RefReg) =
      StateTax(t,RefReg) + FedTax(t) - (StateTax(t,RefReg)*FedTax(t)) ;

    /* Cost of equity  */
    CostOfEquity(Process,RefReg) =
      (Treas10Y(t)/100) + InvFactors('BEQ_BLD',BldRiskClass)*InvFactors('EMRP_BLD',BldRiskClass) ;

    /* Cost of debt   */
    CostOfDebt(Process,RefReg) =
      (CORPBaa(t)/100) * (1 - EffectiveTax(RefReg)) ;

    /* EXTRARISK-emissions */
    CostOfEquity(Process,RefReg)$ExtraRiskProc(Process) =
      CostOfEquity(Process,RefReg) + sum(MNUMYR$TMNUM(t,MNUMYR), emission_EXTRARISK(MNUMYR)) ;
    CostOfDebt(Process,RefReg)$ExtraRiskProc(Process) =
      CostOfDebt(Process,RefReg) + sum(MNUMYR$TMNUM(t,MNUMYR), emission_EXTRARISK(MNUMYR)) ;

    /* Cost of Capital after taxes */
    CostOfCap(Process,RefReg) =
      CostOfDebt(Process,RefReg)*(1-InvFactors('EQUITY_OPR',BldRiskClass)) +
      CostOfEquity(Process,RefReg)*InvFactors('EQUITY_OPR',BldRiskClass) ;

     /* FV over build years (interest during constr.)   */
    IntDuringConst(Process,RefReg) =
      sum(tt$(Ord(tt)<=InvFactors('BLDYRS',BldRiskClass)), (1+CostOfCap(Process,RefReg))**Ord(tt) ) /
      InvFactors('BLDYRS',BldRiskClass) ;

    /* FV for instantaneous payment */
    FVInstPay(Process,RefReg) =
       (1+CostOfCap(Process,RefReg))**InvFactors('BLDYRS',BldRiskClass) ;

    /* PV of prj fr end of life (salvage/decom)  */
    PVProject(Process,RefReg) =
      1 / ((1+CostOfCap(Process,RefReg))**InvFactors('PRJLIFE',BldRiskClass)) ;

    /* Capital recovery factor */
    CRF(Process,RefReg) =
      (CostOfCap(Process,RefReg)*((1+CostOfCap(Process,RefReg))**InvFactors('PRJLIFE',BldRiskClass))) /
      (((1+CostOfCap(Process,RefReg))**InvFactors('PRJLIFE',BldRiskClass)) - 1) ;

    /* ISBL cost in MM 87$    */
    AdjISBL(Process,RefReg) =
      CapExpISBL(Process) * NFInvFac * InvLoc(RefReg) / sum(tt$CapExpYr(tt),GDP(tt)) / 1000 ;

    /* ISBL labor cost in 87$/cd    */
    AdjLabor(Process,RefReg) =
      CapExpLabor(Process) * NFLaborFac * LaborLoc(RefReg) / sum(tt$CapExpYr(tt),GDP(tt)) ;

    /* Total Field Cost (ISBL+OSBL) (MM 87$)  */
    TotalFieldCost(Process,RefReg) =
      AdjISBL(Process,RefReg)*(1 + InvFactors('OSBLFAC',BldRiskClass)) ;

* corrected 7-19-2016
    /* Land OTC (MM 87$)   */
    LandOTC(Process,RefReg) =
      InvFactors('PCTLND',BldRiskClass) *
      TotalFieldCost(Process,RefReg) ;

    /* Working Capital OTC (MM 87$) */
    WorkingCapOTC(Process,RefReg) =
      TotalFieldCost(Process,RefReg)*InvFactors('PCTWC',BldRiskClass) ;

    /* Total OTC Factor */
    TotalOTCFac(Process,RefReg) =
      InvFactors('PCTENV',BldRiskClass)+InvFactors('PCTCNTG',BldRiskClass)+InvFactors('PCTSPECL',BldRiskClass)+
      InvFactors('PCTWC',BldRiskClass) + InvFactors('PCTLND',BldRiskClass);

    /* Total OTC (MM 87$)  */
    TotalOTC(Process,RefReg) =
      TotalFieldCost(Process,RefReg) * TotalOTCFac(Process,RefReg) ;

    /* Total Project Investment (TPI) (MM 87$)  */
    TotalProjInv(Process,RefReg) =
      TotalFieldCost(Process,RefReg) + TotalOTC(Process,RefReg) ;

* replaced CapRec adj with below 7-19-2016
    OptLrnFactor = 1 ;
    OptLrnFactor$LearningProcess(Process) = LrnFactor(Process) ;
    TotalProjInv(Process,RefReg) = TotalProjInv(Process,RefReg) * OptLrnFactor ;

    /* Final Investment (87$/bpcd)      */
    FinalInvest(Process,RefReg) =
      TotalProjInv(Process,RefReg)*1000/CapExpSize(Process) ;

    /* Fixed Capital Investment (MM 87$) */
    FixedCapInv(Process,RefReg) =
      TotalProjInv(Process,RefReg) - WorkingCapOTC(Process,RefReg) ;

    /* Unitized Fixed Capital Investment (87$/bpcd) */
    UnitFCI(Process,RefReg) =
      FixedCapInv(Process,RefReg)*1000/CapExpSize(Process) ;

    /* Total Depreciable Investment (MM 87$) (assumed approx = Fixed Capital Investment)  */
    TotDeprInv(Process,RefReg) =
      FixedCapInv(Process,RefReg) ;

    /* TPI at startup yr (MM 87$) */
    TPIStartYr(Process,RefReg) =
      FVInstPay(Process,RefReg)*LandOTC(Process,RefReg) +
      IntDuringConst(Process,RefReg)*(FixedCapInv(Process,RefReg)-LandOTC(Process,RefReg)) +
      WorkingCapOTC(Process,RefReg) ;

    /* Recoverable Investment at Startup (MM 87$)   */
    RecInvStart(Process,RefReg) =
      PVProject(Process,RefReg)*(LandOTC(Process,RefReg) + WorkingCapOTC(Process,RefReg) +
      InvFactors('PRJSDECOM',BldRiskClass)) ;

    /* PV of project investment (MM 87$)   */
    PVPrjInv(Process,RefReg) =
      TPIStartYr(Process,RefReg) - RecInvStart(Process,RefReg) ;

    /* Annual Capital Recovery (MM 87$)   */
    Annuity(Process,RefReg) =
      PVPrjInv(Process,RefReg) * CRF(Process,RefReg) ;

    /* Annual Levelized depreciation (SRL method) (MM 87$)  */
    AnnLevDepr(Process,RefReg) =
      TotDeprInv(Process,RefReg)/InvFactors('PRJLIFE',BldRiskClass) ;

    /* Levelized depreciation tax credit, after tax (MM 87$)  */
    AnnDeprTaxCred(Process,RefReg) =
      AnnLevDepr(Process,RefReg) * EffectiveTax(RefReg) ;

    /* Annual cap charge, after tax credit (MM 87$)  */
    AnnCapCharge(Process,RefReg) =
      Annuity(Process,RefReg) - AnnDeprTaxCred(Process,RefReg) ;

    /* Daily cap charge, after tax credit (87$/cd)   */
    DailyCapChrg(Process,RefReg) =
      AnnCapCharge(Process,RefReg)*1000000/365 ;

     /* Insurance FXOC (87$/cd) */
    InsFXOC(Process,RefReg) =
      FixedCapInv(Process,RefReg)*InvFactors('INS_FAC',BldRiskClass)*1000000/365 ;

    /* Local Tax FXOC (87$/cd)    */
    TaxFXOC(Process,RefReg) =
      FixedCapInv(Process,RefReg)*InvFactors('TAX_FAC',BldRiskClass)*1000000/365 ;

    /* Maintenance FXOC (87$/cd) */
    MaintFXOC(Process,RefReg) =
      FixedCapInv(Process,RefReg)*InvFactors('MAINT_FAC',BldRiskClass)*1000000/365 ;

    /* Supplies, OH, Env FXOC (87$/cd)     */
    OtherFXOC(Process,RefReg) =
      FixedCapInv(Process,RefReg)*InvFactors('OTH_FAC',BldRiskClass)*1000000/365 ;

    /* Total Capital-related FXOC (87$/cd)  */
    CapFXOC(Process,RefReg) =
      InsFXOC(Process,RefReg) + TaxFXOC(Process,RefReg) +
      MaintFXOC(Process,RefReg) + OtherFXOC(Process,RefReg);

    /* Staff FXOC (87$/cd) */
    StaffFXOC(Process,RefReg) =
      AdjLabor(Process,RefReg) * InvFactors('STAFF_FAC',BldRiskClass) ;

    /* Benefits & OH FXOC (87$/cd) */
    OverheadFXOC(Process,RefReg) =
      (AdjLabor(Process,RefReg) + StaffFXOC(Process,RefReg)) * InvFactors('OH_FAC',BldRiskClass) ;

    /* Total Labor-related FXOC (87$/cd)  */
    LaborFXOC(Process,RefReg) =
      AdjLabor(Process,RefReg) + StaffFXOC(Process,RefReg) + OverheadFXOC(Process,RefReg) ;

    /* Capital recovery costs, before taxes (87$/bpcd) */
    CapRec(Process,RefReg) =
      DailyCapChrg(Process,RefReg)/(1000*CapExpSize(Process)) ;

    /* Total FXOC (87$/bpcd)  */
    TotalFXOC(Process,RefReg) =
      (CapFXOC(Process,RefReg) + LaborFXOC(Process,RefReg))/(1000*CapExpSize(Process)) ;

  );     /* end_of_loop: (t,Process,BldRiskClass)  */

$ontext
*---------------------------------------------
* Apply learning factors to capital costs
*---------------------------------------------

  NumPlants(LearningProcess) =
    sum((RefReg,RefType), AvailCap(RefReg,RefType,LearningProcess) ) / CapExpSize(LearningProcess) ;
  NumPlants(LearningProcess) = max(1.0,NumPlants(LearningProcess));

  LrnFactor(LearningProcess)$(NumPlants(LearningProcess)<=LearningData(LearningProcess,'Phase2','Fast','K')) =
    1 + MoreLearningData(LearningProcess,'Optimism')/NumPlants(LearningProcess) ;

  LrnFactor(LearningProcess)$
    ((NumPlants(LearningProcess)>LearningData(LearningProcess,'Phase2','Fast','K')) and
     (NumPlants(LearningProcess)<=LearningData(LearningProcess,'Phase3','Fast','K'))) =
    (LearningData(LearningProcess,'Phase2','Fast','A') * (NumPlants(LearningProcess)**(-LearningData(LearningProcess,'Phase2','Fast','B'))) +
     LearningData(LearningProcess,'Phase2','Slow','A') * (NumPlants(LearningProcess)**(-LearningData(LearningProcess,'Phase2','Slow','B'))) ) ;

  LrnFactor(LearningProcess)$(NumPlants(LearningProcess)>LearningData(LearningProcess,'Phase3','Fast','K')) =
   (LearningData(LearningProcess,'Phase3','Fast','A') * (NumPlants(LearningProcess)**(-LearningData(LearningProcess,'Phase3','Fast','B'))) +
    LearningData(LearningProcess,'Phase3','Slow','A') * (NumPlants(LearningProcess)**(-LearningData(LearningProcess,'Phase3','Slow','B'))) ) ;

  CapRec(LearningProcess,RefReg) = CapRec(LearningProcess,RefReg) * LrnFactor(LearningProcess) ;

*  execute_unload 'learn', NumPlants, LrnFactor, CapRec;
$offtext

*---------------------------------------------
* Calculate NPV of relevant parameters *
*---------------------------------------------

*---------------------------------------------------------------------
*mc6 10/7/13
* Add credit for wax sales against GTL operating cost: 10% of expected oil price
* In a 10/7/2013, 3:27 pm email, Vish Mantri noted approx $10 credit per bbl liquid output,
* which is approx 10% of the 2013 oil price.

OpVarCost('GTL',ProcessMode)$(ProcProcMode('GTL',ProcessMode)) =
  OpVarCost('GTL',ProcessMode) - (0.10 * ExpectedWOP(RunYears));

*---------------------------------------------------------------------

* NPV of cost parameters inflated to NOMINAL dollars

  npv_OpVarCost(Process,ProcessMode,ActivePeriod)                        = npv1(OpVarCost(Process,ProcessMode),Rate(t));
  npv_RecipeOVC(RcpMode,ActivePeriod)                                    = npv1(RecipeOVC(RcpMode),Rate(t));

  npv_CrudePriceTotal(Step,ActivePeriod)                                 = npv1(CrudePriceTotal(Step,t),Rate(t));
  npv_CrudePriceIncremental(Source,Crude,Step,ActivePeriod)              = npv1(CrudePriceIncremental(Source,Crude,Step,t),CrudeRate(t));
  npv_CrudeImportCost(RefReg,Source,Crude,Step,ActivePeriod)             = npv1(CrudeImportCost(RefReg,Source,Crude,Step,t),Rate(t));
  npv_CrudeExportCost(RefReg,Source,Crude,Step,ActivePeriod)             = npv1(CrudeExportCost(RefReg,Source,Crude,Step,t),Rate(t));
  npv_NonUSCrudeDemandPrice(Source,Crude,Step,ActivePeriod)              = npv1(NonUSCrudeDemandPrice(Source,Crude,Step,t),Rate(t));

  npv_RFSCreditPrice(RFSCategory,ActivePeriod)                           = npv1(RFSCreditPrice(RFSCategory,t),Rate(t));

  npv_UtilityPrice(RefReg,Utility,ActivePeriod)                          = npv1(UtilityPrice(RefReg,Utility,t),Rate(t));
  npv_RefInpPrc(RefReg,RefInputStr,Step,ActivePeriod)                    = npv1(RefInpPrc(RefReg,RefInputStr,Step,t),Rate(t));
  npv_FBDImpPrice(Step,ActivePeriod)                                     = npv1(FBDImpPrice(Step,t),Rate(t));
  npv_RDHImpPrice(Step,ActivePeriod)                                     = npv1(RDHImpPrice(Step,t),Rate(t));
  npv_BiomassPrc(CoalDreg,BioStr,Step,ActivePeriod)                      = npv1(BiomassPrc(CoalDreg,BioStr,Step,t),Rate(t));
  npv_CoalPrc(CoalSReg,CoalStr,Step,ActivePeriod)                        = npv1(CoalPrc(CoalSReg,CoalStr,Step,t),Rate(t));
  npv_SO2Prc(CoalStr,MX_SO2,ActivePeriod)                                = npv1(SO2Prc(CoalStr,MX_SO2,t),Rate(t));
  npv_HGPrc(CoalDReg,CoalStr,ActivePeriod)                               = npv1(HGPrc(CoalDReg,CoalStr,t),Rate(t));
  npv_CoalTrnspCst(CoalSReg,CoalDReg,CoalStr,ActivePeriod)               = npv1(CoalTrnspCst(CoalSReg,CoalDReg,CoalStr,t),Rate(t));
  npv_GlobalImpPrice(ActiveDem,EndProduct,Step,ActivePeriod)             = npv1(GlobalImpPrice(ActiveDem,EndProduct,Step,t),Rate(t));
  npv_REFtoCDTranCost(RefReg,ActiveDem,TranMode,RecipeProd,ActivePeriod) = npv1(REFtoCDTranCost(RefReg,ActiveDem,TranMode,RecipeProd),Rate(t));
  npv_REFtoREFTranCost(RefReg,RefRegA,TranMode,Stream,ActivePeriod)      = npv1(REFtoREFTranCost(RefReg,RefRegA,TranMode,Stream),Rate(t));
* npv_EthBrazilPrc(EthStream,Step,ActivePeriod)                          = npv1(EthBrazilPrc(EthStream,Step,t),Rate(t));
  npv_EthImpSupPrc(EthStream,Step,ActivePeriod)                          = npv1(EthImpSupPrc(EthStream,Step,t),Rate(t));
  npv_EthExpDmdPrc(EthStream,Step,ActivePeriod)                          = npv1(EthExpDmdPrc(EthStream,Step,t),Rate(t));

  npv_REFtoINTTranCost(RefReg,IntReg,ActivePeriod)                       = npv1(REFtoINTTranCost(RefReg,IntReg),Rate(t));
  npv_INTtoREFTranCost(IntReg,RefReg,ActivePeriod)                       = npv1(INTtoREFTranCost(IntReg,RefReg),Rate(t));

  npv_INTtoREFFBDTranCost(IntReg,RefReg,ActivePeriod)                    = npv1(INTtoREFFBDTranCost(IntReg,RefReg),Rate(t));

  npv_BrzAdvTranCost(ActivePeriod)                                       = npv1(BrzAdvTranCost(t),Rate(t));
* 1.225 = 5% of the max Advanced waiver price ($1/gal 2010$) in 87$/bbl
  npv_TranCostToBrazil(ActivePeriod)                                     = npv1((BrzAdvTranCost(t)+1.225),Rate(t));
  npv_TranCostFromBrazil(ActivePeriod)                                   = npv1((BrzAdvTranCost(t)),Rate(t));

  npv_TotalMarkups(ActivePeriod,MarkupFuel,MarkupSector,CenDiv)          = npv1(TotalMarkups(t,MarkupFuel,MarkupSector,CenDiv),Rate(t));
  npv_CoproductPrice(Stream,RefReg,ActivePeriod)                         = npv1(CoproductPrice(Stream,RefReg,t),Rate(t));
* NOMINAL fixed cost charges for existing capacity operates
  npv_FXOCCost(Process,RefReg,ActivePeriod)                              = npv1(TotalFXOC(Process,RefReg),Rate(t));
  npv_MCCPrice(Stream,Step,ActivePeriod)                                 = npv1(MCCPrice(Stream,Step,t),Rate(t));

  npv_CO2_Price(OGCO2Reg,CO2_Source,ActivePeriod)                        = npv1(CO2_Price(OGCO2Reg,CO2_Source,t),Rate(t));

  npv_CO2_OGTranCost(OGCO2Reg,OGCO2Reg2,ActivePeriod)                    = npv1(CO2_OGTranCost(OGCO2Reg,OGCO2Reg2,t),Rate(t));
  npv_FRtoSalineCost(FuelRegion,ActivePeriod)                            = npv1(FRtoSalineCost(FuelRegion,t),Rate(t));
  npv_FRtoEORCost(FuelRegion,OGCO2Reg,ActivePeriod)                      = npv1(FRtoEORCost(FuelRegion,OGCO2Reg,t),Rate(t));
  npv_CO2_SafetyPrice(OGCO2Reg,ActivePeriod)                             = npv1(CO2_SafetyPrice(OGCO2Reg,t),Rate(t));

  npv_ImportPrice(Stream,IntReg,Step,ActivePeriod)                      = npv1(ImportPrice(Stream,IntReg,Step,t),Rate(t));
  npv_ExportPrice(Stream,IntReg,Step,ActivePeriod)                      = npv1(ExportPrice(Stream,IntReg,Step,t),Rate(t));

  npv_LCFSSafetyPrice(LCFS_PetCategory,ActivePeriod)                     = npv1(LCFSSafetyPrice(LCFS_PetCategory,t),Rate(t));
  npv_CFPSafetyPrice(CFP_PetCategory,ActivePeriod)                       = npv1(CFPSafetyPrice(CFP_PetCategory,t),Rate(t));
  npv_NGLImportCost(CenDiv,NGLProduct,ActivePeriod)                      = npv1(NGLImportCost(CenDiv,NGLProduct,t),Rate(t));
  npv_NGLexportCost(CenDiv,NGLProduct,ActivePeriod)                      = npv1(NGLexportCost(CenDiv,NGLProduct,t),Rate(t));

  npv_BiofuelSubsidy(Stream,ActivePeriod)                                = npv1(BiofuelSubsidy(Stream,t),Rate(t));

* ** NPV of supply, demand, and specification parameters
  npv_ProductDemand(CenDiv,EndProduct,ActivePeriod)               = npv2(ProductDemand(CenDiv,EndProduct,t),Rate(t));
  npv_NGLDemands(CenDiv,NGLProduct,ActivePeriod)                  = npv2(NGLDemands(CenDiv,NGLProduct,t),Rate(t));
  npv_BTUDemand(CenDiv,ActivePeriod)                              = npv2(BTUDemand(CenDiv,t),Rate(t));

  npv_CrudeSupplyTotal(Step,ActivePeriod)                         = npv2(CrudeSupplyTotal(Step,t),Rate(t));
  npv_CrudeSupplyIncremental(Source,Crude,Step,ActivePeriod)      = npv2(CrudeSupplyIncremental(Source,Crude,Step,t),Rate(t));
  npv_DomesticCrudeSup(RefReg,Crude,ActivePeriod)                 = npv2(DomesticCrudeSup(RefReg,Crude,t),Rate(t));

  npv_CrudeImportCap(RefReg,Source,Step,ActivePeriod)             = npv2(CrudeImportCap(RefReg,Source,Step,t),Rate(t));
  npv_CrudeExportCap(RefReg,Source,Step,ActivePeriod)             = npv2(CrudeExportCap(RefReg,Source,Step,t),Rate(t));

  npv_NonUSCrudeDemandValue(Source,Crude,Step,ActivePeriod)       = npv2(NonUSCrudeDemandValue(Source,Crude,Step,t),Rate(t));
  npv_CrudeToCanada(RefReg,Crude,ActivePeriod)                    = npv2(CrudeToCanada(RefReg,Crude,t),Rate(t));

  npv_SPR_Withdraw(RefReg,Crude,ActivePeriod)                     = npv2(SPR_Withdraw(RefReg,Crude,t),Rate(t));

  npv_RefInpSup(RefReg,RefInputStr,Step,ActivePeriod)             = npv2(RefInpSup(RefReg,RefInputStr,Step,t),Rate(t));
  npv_FBDImpSupply(Step,ActivePeriod)                             = npv2(FBDImpSupply(Step,t),Rate(t));

  npv_RDHImpSupply(Step,ActivePeriod)                             = npv2(RDHImpSupply(Step,t),Rate(t));
  npv_BiomassSup(CoalDReg,BioStr,Step,ActivePeriod)               = npv2(BiomassSup(CoalDReg,BioStr,Step,t),Rate(t));
  npv_BioOthDemand(CoalDReg,BioStr,Other_MKT,ActivePeriod)        = npv2(BioOthDemand(CoalDReg,BioStr,Other_MKT,t),Rate(t));
  npv_CoalSup(CoalSReg,CoalStr,Step,ActivePeriod)                 = npv2(CoalSup(CoalSReg,CoalStr,Step,t),Rate(t));
  npv_CoalOthDemand(CoalSreg,CoalStr,ActivePeriod)                = npv2(CoalOthDemand(CoalSreg,CoalStr,t),Rate(t));
  npv_GasSpecMin(GasSpecProd,GasProp,ActivePeriod)                = npv2(GasSpecMin(GasSpecProd,GasProp,t),Rate(t));
  npv_GasSpecMax(GasSpecProd,GasProp,ActivePeriod)                = npv2(GasSpecMax(GasSpecProd,GasProp,t),Rate(t));
  npv_DistSpecMin(DistSpecProd,DistProp,ActivePeriod)             = npv2(DistSpecMin(DistSpecProd,DistProp,t),Rate(t));
  npv_DistSpecMax(DistSpecProd,DistProp,ActivePeriod)             = npv2(DistSpecMax(DistSpecProd,DistProp,t),Rate(t));
  npv_ResidSpecMin(ResidSpecProd,ResidProp,ActivePeriod)          = npv2(ResidSpecMin(ResidSpecProd,ResidProp,t),Rate(t));
  npv_ResidSpecMax(ResidSpecProd,ResidProp,ActivePeriod)          = npv2(ResidSpecMax(ResidSpecProd,ResidProp,t),Rate(t));
  npv_GlobalImpSupply(CenDiv,EndProduct,Step,ActivePeriod)        = npv2(GlobalImpSupply(CenDiv,EndProduct,Step,t),Rate(t));
* npv_EthBrazilSup(EthStream,Step,ActivePeriod)                   = npv2(EthBrazilSup(EthStream,Step,t),Rate(t));
  npv_EthImpSupQty(EthStream,Step,ActivePeriod)                   = npv2(EthImpSupQty(EthStream,Step,t),Rate(t));
  npv_EthExpDmdQty(EthStream,Step,ActivePeriod)                   = npv2(EthExpDmdQty(EthStream,Step,t),Rate(t));
  npv_NonUSEthDmd(ActivePeriod)                                   = npv2(BrzEthDmdQ0(t),Rate(t)) + npv2(NonUSEthQ0(t),Rate(t));

  npv_SO2Emission(CoalSReg,CoalDReg,CoalStr,MX_SO2,ActivePeriod)  = npv2(SO2Emission(CoalSReg,CoalDReg,CoalStr,MX_SO2,t),Rate(t));
  npv_HGEmission(CoalSReg,CoalDReg,CoalStr,ActivePeriod)          = npv2(HGEmission(CoalSReg,CoalDReg,CoalStr,t),Rate(t));

  npv_RFSMandates(RFSCategory,ActivePeriod)                       = npv2(RFSMandates(RFSCategory,t),Rate(t));
  npv_RFSMandates(RFSCategory,ActivePeriod)                       = npv_RFSMandates(RFSCategory,ActivePeriod)*1000000/365.0/42.0;

  npv_RFSWaivers(RFSCategory,ActivePeriod)                        = npv2(RFSWaivers(RFSCategory,t),Rate(t));
  npv_RFSWaivers(RFSCategory,ActivePeriod)                        = npv_RFSWaivers(RFSCategory,ActivePeriod)*1000000/365.0/42.0;

  npv_PetDSFrac(ActivePeriod)                                     = npv2(PetDSFrac(t),Rate(t)) ;
  npv_PetMGFrac(ActivePeriod)                                     = npv2(PetMGFrac(t),Rate(t)) ;
  npv_PetMGFrac_noETH(ActivePeriod)                               = npv2(PetMGFrac_noETH(t),Rate(t)) ;

  npv_CFMGQCD(CenDiv,ActivePeriod)                                = npv2(CFMGQCD(CenDiv,t),Rate(t));
  npv_CFE85Q(ActivePeriod)                                        = npv2(CFE85Q(t),Rate(t));

  npv_E15MaxPen(CenDiv,ActivePeriod)                              = npv2(E15MaxPen(CenDiv,t),Rate(t));

  npv_ImportSupply(Stream,IntReg,Step,ActivePeriod)               = npv2(ImportSupply(Stream,IntReg,Step,t),Rate(t)) ;
  npv_ExportSupply(Stream,IntReg,Step,ActivePeriod)               = npv2(ExportSupply(Stream,IntReg,Step,t),Rate(t)) ;

  npv_MCCSupply(Stream,Step,ActivePeriod)                         = npv2(MCCSupply(Stream,Step,t),Rate(t));
  npv_NGLBounds(NGLInputStr,RefReg,ActivePeriod)                  = npv2(NGLBounds(NGLInputStr,RefReg,t),Rate(t));

  npv_OGSMCO2Dem(OGCO2Reg,ActivePeriod)                           = npv2(OGSMCO2Dem(OGCO2Reg,t),Rate(t));
  npv_CO2_Avail(OGCO2Reg,CO2_Source,ActivePeriod)                 = npv2(CO2_Avail(OGCO2Reg,CO2_Source,t),Rate(t));

  npv_State_Biodiesel(RefReg,State,ActivePeriod)                  = npv2(State_Biodiesel(RefReg,State,t),Rate(t));

  npv_REFtoREFTranCap(RefReg,RefRegA,TranMode,ActivePeriod)       = npv2(REFtoREFTranCap(RefReg,RefRegA,TranMode,t),Rate(t));

  npv_STEO_PhaseOut(ActivePeriod)                                 = npv2(STEO_PhaseOut(t),Rate(t));
  npv_STEO_Target(STEO_Category,ActivePeriod)                     = npv2(STEO_Target(STEO_Category,t),Rate(t));
  npv_STEO_Penalty(Step,Period)                                   = npv2(STEO_Penalty(Step,t),Rate(t));

* NOMINAL capital + fixed cost charges for new builds
* Note:  New builds must pay fixed costs for the rest of the years in the planning horizon
*         as opposed to existing capacity which only pays fixed costs in the current period
*         if it chooses to operate
  npv_BuildCost(Process,RefReg,'STEP01',Period)$ActivePeriod(Period) = npv3(TotalFXOC(Process,RefReg),CapRec(Process,RefReg),Rate(t)) ;

* Build costs for capacity expansion supply steps for the alternative fuels
  npv_BuildCost(Process,RefReg,BldStep,ActivePeriod)$AFGrowthRate(Process) =
    AFCostMult(Process,BldStep) * npv_BuildCost(Process,RefReg,'STEP01',ActivePeriod) ;

* ** LCFS comment here please
  LCFS_Bio_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioProcess,LCFS_BioMode,ModelYears(t)) = sum((LCFS_Years(t),LCFS_BioStreams(Stream)),
     ( LCFS_BioStream_Carbon_Factor(LCFS_PetCategory,LCFS_BioProcess,LCFS_BioMode,LCFS_BioStreams,t)-
      LCFS_BioStream_PetCarbonFactor(LCFS_PetCategory,LCFS_BioProcess,LCFS_BioMode,LCFS_BioStreams,t)) *
      ProcessTable(LCFS_BioStreams,LCFS_BioProcess,LCFS_BioMode) * LCFS_Bio_Energy_Density(LCFS_BioStreams) * 0.001);

  npv_LCFS_Bio_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioProcess,LCFS_BioMode,ActivePeriod) =
      npv2(LCFS_Bio_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioProcess,LCFS_BioMode,t),Rate(t));

  LCFS_Imp_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioImports,ModelYears(t)) = sum(LCFS_Years(t),
     (LCFS_BioImport_Carbon_Factor(LCFS_PetCategory,LCFS_BioImports,t) -
      LCFS_BioImport_PetCarbonFactor(LCFS_PetCategory,LCFS_BioImports,t)) *
      LCFS_Imp_Energy_Density(LCFS_BioImports) * 0.001 );

  npv_LCFS_Imp_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioImports,ActivePeriod) =
      npv2(LCFS_Imp_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_BioImports,t),Rate(t));



* add BiodImp to LCFS constraint (similar to EthIMP)
* need to hardcode CI data from FBD, FBD_FCO (LCFS_BioStream_Carbon_Factor, 1000 tonne Carbon/trill BTU)
* to define LCFS_BiodImp_CFactor (tonne Carbon/bbl)
  LCFS_BiodImp_Carbon_Factor(LCFS_PetCategory,t)   = LCFS_BioStream_Carbon_Factor(LCFS_PetCategory,'FBD','FBD_FCO','FBD',t) ;
  LCFS_BiodImp_PetCarbonFactor(LCFS_PetCategory,t) = LCFS_PetStream_Carbon_Factor(LCFS_PetCategory,'CarbDSUout',t);
  LCFS_BiodImp_CFactor(LCFS_PetCategory,LCFS_RefReg,ModelYears(t)) = sum(LCFS_Years(t),
     (LCFS_BiodImp_Carbon_Factor(LCFS_PetCategory,t) - LCFS_BiodImp_PetCarbonFactor(LCFS_PetCategory,t)) *
      LCFS_Bio_Energy_Density('FBD') * 0.001 );

  npv_LCFS_BiodImp_CFactor(LCFS_PetCategory,LCFS_RefReg,ActivePeriod) =
      npv2(LCFS_BiodImp_CFactor(LCFS_PetCategory,LCFS_RefReg,t),Rate(t));

* add RenewDImp to LCFS constraint
  LCFS_RenewDImp_Carbon_Factor(LCFS_PetCategory,t)   = LCFS_BioStream_Carbon_Factor(LCFS_PetCategory,'GDT','GDT_FCO','RDH',t) ;
  LCFS_RenewDImp_PetCarbonFactor(LCFS_PetCategory,t) = LCFS_PetStream_Carbon_Factor(LCFS_PetCategory,'CarbDSUout',t);
  LCFS_RenewDImp_CFactor(LCFS_PetCategory,LCFS_RefReg,ModelYears(t)) = sum(LCFS_Years(t),
     (LCFS_RenewDImp_Carbon_Factor(LCFS_PetCategory,t) - LCFS_RenewDImp_PetCarbonFactor(LCFS_PetCategory,t)) *
      LCFS_Bio_Energy_Density('RDH') * 0.001 );

  npv_LCFS_RenewDImp_CFactor(LCFS_PetCategory,LCFS_RefReg,ActivePeriod) =
      npv2(LCFS_RenewDImp_CFactor(LCFS_PetCategory,LCFS_RefReg,t),Rate(t));


  LCFS_Pet_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_PetStreams,ModelYears(t))$LCFS_PetStream_Carbon_Factor(LCFS_PetCategory,LCFS_PetStreams,t) =
      sum(LCFS_Years(t),
      (LCFS_PetStream_Carbon_Factor(LCFS_PetCategory,LCFS_PetStreams,LCFS_Years) - LCFS_Target(LCFS_PetCategory,LCFS_RefReg,LCFS_Years)) *
      LCFS_Pet_Energy_Density(LCFS_PetStreams,LCFS_Years) * 0.001);

  npv_LCFS_Pet_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_PetStreams,ActivePeriod) =
      npv2(LCFS_Pet_CFactor(LCFS_PetCategory,LCFS_RefReg,LCFS_PetStreams,t),Rate(t));

* 8-16-2019,em4
* hardcode CI for LandfillGas (1000 tonne Carbon/trill BTU)
  LCFS_LandfillGas_Carbon_Factor(LCFS_PetCategory) = 14.532 ;

* attach to new LCFS credit source:  LCFS_LandfillGas (trill BTU/yr)
* since LCFS_LandfillGas in trills, then divided by 365 in eq below; if trills/day, do not divide
  LCFS_LFGas_CFactor(LCFS_PetCategory,LCFS_Years)$LCFS_LandfillGas_Carbon_Factor(LCFS_PetCategory) = sum(LCFS_RefReg,
      (LCFS_LandfillGas_Carbon_Factor(LCFS_PetCategory) - LCFS_Target(LCFS_PetCategory,LCFS_RefReg,LCFS_Years)) / 365.0 );

  npv_LCFS_LFGas_CFactor(LCFS_PetCategory,ActivePeriod)$LCFS_LandfillGas_Carbon_Factor(LCFS_PetCategory) =
      npv2(LCFS_LFGas_CFactor(LCFS_PetCategory,t),Rate(t));

* Hardcode:    LCFS_LandfillGas_total(t)       (trill BTU)
* LCFS_LandfillGas_total(t) = 0.0 ;
* LCFS_LandfillGas_total(t) = 130.0 ;
  LCFS_LandfillGas_total('2018') = 16.0 ;
  LCFS_LandfillGas_total('2017') = LCFS_LandfillGas_total('2018') /1.12 ;
  LCFS_LandfillGas_total('2016') = LCFS_LandfillGas_total('2017') /1.18 ;
  LCFS_LandfillGas_total('2015') = LCFS_LandfillGas_total('2016') /1.30 ;
  Loop( t,
    If( (t.val < 2015),
      LCFS_LandfillGas_total(t) = LCFS_LandfillGas_total('2015') ;
    elseif( (t.val > 2018) and (t.val >= 2030)),
      LCFS_LandfillGas_total(t) = LCFS_LandfillGas_total(t-1)*1.05 ;
    elseif( (t.val > 2030) ),
      LCFS_LandfillGas_total(t) = LCFS_LandfillGas_total('2030') ;
    );
  );

  npv_LCFS_LandfillGas_total(ActivePeriod) =
      npv2(LCFS_LandfillGas_Total(t),Rate(t));

* end 8-16-2019 update


  LCFS_Alt_CFactor(LCFS_PetCategory,LCFS_Vehicle_Types,LCFS_Years)$LCFS_AltVehicle_Carbon_Factor(LCFS_PetCategory,LCFS_Vehicle_Types) = sum(LCFS_RefReg,
      (LCFS_AltVehicle_Carbon_Factor(LCFS_PetCategory,LCFS_Vehicle_Types) - LCFS_Target(LCFS_PetCategory,LCFS_RefReg,LCFS_Years)) / 365.0 );

  npv_LCFS_Alt_CFactor(LCFS_PetCategory,LCFS_Vehicle_Types,ActivePeriod)$LCFS_AltVehicle_Carbon_Factor(LCFS_PetCategory,LCFS_Vehicle_Types) =
      npv2(LCFS_Alt_CFactor(LCFS_PetCategory,LCFS_Vehicle_Types,t),Rate(t));

  npv_LCFS_AltVehicle_Demand(LCFS_PetCategory,LCFS_Vehicle_Types,LCFS_RefReg,ActivePeriod)$LCFS_AltVehicle_Carbon_Factor(LCFS_PetCategory,LCFS_Vehicle_Types) =
      npv2(LCFS_AltVehicle_Demand(LCFS_PetCategory,LCFS_Vehicle_Types,LCFS_RefReg,t),Rate(t));

  LCFS_Price(LCFS_PetCategory,LCFS_Years) = sum((LCFS_C,MNXYRS)$(LCFS_M2(LCFS_PetCategory,LCFS_C) and tMNXYRS(LCFS_Years,MNXYRS)),
      max(0,LFMMOUT_LCFS_Offset_Prc(LCFS_C,MNXYRS)) );

  npv_LCFS_Price(LCFS_PetCategory,ActivePeriod) = npv1(LCFS_Price(LCFS_PetCategory,t),Rate(t));

* Isolate ethanol factors from process/mode to account for transfers in and out of CA
  LCFS_Eth_CFactor(LCFS_PetCategory('CaRBOBout'),LCFS_BioStreams(EthStream),t)$((sum((LCFS_BioProcess,LCFS_BioMode)$LCFS_BioStream_Carbon_Factor(LCFS_PetCategory,LCFS_BioProcess,LCFS_BioMode,LCFS_BioStreams,t), 1))>0) =
    sum((LCFS_BioProcess,LCFS_BioMode)$LCFS_BioStream_Carbon_Factor(LCFS_PetCategory,LCFS_BioProcess,LCFS_BioMode,LCFS_BioStreams,t),
     (LCFS_BioStream_Carbon_Factor(LCFS_PetCategory,LCFS_BioProcess,LCFS_BioMode,LCFS_BioStreams,t) -
      LCFS_BioStream_PetCarbonFactor(LCFS_PetCategory,LCFS_BioProcess,LCFS_BioMode,LCFS_BioStreams,t)) * LCFS_Bio_Energy_Density(LCFS_BioStreams) * 0.001 ) /
    sum((LCFS_BioProcess,LCFS_BioMode)$LCFS_BioStream_Carbon_Factor(LCFS_PetCategory,LCFS_BioProcess,LCFS_BioMode,LCFS_BioStreams,t),
      1 ) ;

  npv_LCFS_Eth_CFactor(LCFS_PetCategory,LCFS_BioStreams,ActivePeriod) =
      npv2(LCFS_Eth_CFactor(LCFS_PetCategory,LCFS_BioStreams,t),Rate(t));


* ** CFP comment here please
  CFP_Bio_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioProcess,CFP_BioMode,ModelYears(t)) = sum((CFP_Years(t),CFP_BioStreams(Stream)),
     ( CFP_BioStream_Carbon_Factor(CFP_PetCategory,CFP_BioProcess,CFP_BioMode,CFP_BioStreams,t)-
      CFP_BioStream_PetCarbonFactor(CFP_PetCategory,CFP_BioProcess,CFP_BioMode,CFP_BioStreams,t)) *
      ProcessTable(CFP_BioStreams,CFP_BioProcess,CFP_BioMode) * CFP_Bio_Energy_Density(CFP_BioStreams) * 0.001);

  npv_CFP_Bio_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioProcess,CFP_BioMode,ActivePeriod) =
      npv2(CFP_Bio_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioProcess,CFP_BioMode,t),Rate(t));

  CFP_Imp_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioImports,ModelYears(t)) = sum(CFP_Years(t),
     (CFP_BioImport_Carbon_Factor(CFP_PetCategory,CFP_BioImports,t) -
      CFP_BioImport_PetCarbonFactor(CFP_PetCategory,CFP_BioImports,t)) *
      CFP_Imp_Energy_Density(CFP_BioImports) * 0.001 );

  npv_CFP_Imp_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioImports,ActivePeriod) =
      npv2(CFP_Imp_CFactor(CFP_PetCategory,CFP_RefReg,CFP_BioImports,t),Rate(t));



* add BiodImp to CFP constraint (similar to EthIMP)
* need to hardcode CI data from FBD, FBD_FCO (CFP_BioStream_Carbon_Factor, 1000 tonne Carbon/trill BTU)
* to define CFP_BiodImp_CFactor (tonne Carbon/bbl)
  CFP_BiodImp_Carbon_Factor(CFP_PetCategory,t)   = CFP_BioStream_Carbon_Factor(CFP_PetCategory,'FBD','FBD_FCO','FBD',t) ;
  CFP_BiodImp_PetCarbonFactor(CFP_PetCategory,t) = CFP_PetStream_Carbon_Factor(CFP_PetCategory,'DSUout',t);
  CFP_BiodImp_CFactor(CFP_PetCategory,CFP_RefReg,ModelYears(t)) = sum(CFP_Years(t),
     (CFP_BiodImp_Carbon_Factor(CFP_PetCategory,t) - CFP_BiodImp_PetCarbonFactor(CFP_PetCategory,t)) *
      CFP_Bio_Energy_Density('FBD') * 0.001 );

  npv_CFP_BiodImp_CFactor(CFP_PetCategory,CFP_RefReg,ActivePeriod) =
      npv2(CFP_BiodImp_CFactor(CFP_PetCategory,CFP_RefReg,t),Rate(t));

* add RenewDImp to CFP constraint
  CFP_RenewDImp_Carbon_Factor(CFP_PetCategory,t)   = CFP_BioStream_Carbon_Factor(CFP_PetCategory,'GDT','GDT_FCO','RDH',t) ;
  CFP_RenewDImp_PetCarbonFactor(CFP_PetCategory,t) = CFP_PetStream_Carbon_Factor(CFP_PetCategory,'DSUout',t);
  CFP_RenewDImp_CFactor(CFP_PetCategory,CFP_RefReg,ModelYears(t)) = sum(CFP_Years(t),
     (CFP_RenewDImp_Carbon_Factor(CFP_PetCategory,t) - CFP_RenewDImp_PetCarbonFactor(CFP_PetCategory,t)) *
      CFP_Bio_Energy_Density('RDH') * 0.001 );

  npv_CFP_RenewDImp_CFactor(CFP_PetCategory,CFP_RefReg,ActivePeriod) =
      npv2(CFP_RenewDImp_CFactor(CFP_PetCategory,CFP_RefReg,t),Rate(t));


  CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,ModelYears(t))$CFP_PetStream_Carbon_Factor(CFP_PetCategory,CFP_PetStreams,t) =
      sum(CFP_Years(t),
      (CFP_PetStream_Carbon_Factor(CFP_PetCategory,CFP_PetStreams,CFP_Years) - CFP_Target(CFP_PetCategory,CFP_RefReg,CFP_Years)) *
      CFP_Pet_Energy_Density(CFP_PetStreams,CFP_Years) * 0.001);

  npv_CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,ActivePeriod) =
      npv2(CFP_Pet_CFactor(CFP_PetCategory,CFP_RefReg,CFP_PetStreams,t),Rate(t));

* hardcode CI for LandfillGas (1000 tonne Carbon/trill BTU)
  CFP_LandfillGas_Carbon_Factor(CFP_PetCategory) = 14.532 ;

* attach to new CFP credit source:  CFP_LandfillGas (trill BTU/yr)
* since CFP_LandfillGas in trills, then divided by 365 in eq below; if trills/day, do not divide
  CFP_LFGas_CFactor(CFP_PetCategory,CFP_Years)$CFP_LandfillGas_Carbon_Factor(CFP_PetCategory) = sum(CFP_RefReg,
      (CFP_LandfillGas_Carbon_Factor(CFP_PetCategory) - CFP_Target(CFP_PetCategory,CFP_RefReg,CFP_Years)) / 365.0 );

  npv_CFP_LFGas_CFactor(CFP_PetCategory,ActivePeriod)$CFP_LandfillGas_Carbon_Factor(CFP_PetCategory) =
      npv2(CFP_LFGas_CFactor(CFP_PetCategory,t),Rate(t));

* Hardcode:    CFP_LandfillGas_total(t)       (trill BTU)
* CFP_LandfillGas_total(t) = 0.0 ;
* CFP_LandfillGas_total(t) = 130.0 ;
  CFP_LandfillGas_total('2018') = 16.0 ;
  CFP_LandfillGas_total('2017') = CFP_LandfillGas_total('2018') /1.12 ;
  CFP_LandfillGas_total('2016') = CFP_LandfillGas_total('2017') /1.18 ;
  CFP_LandfillGas_total('2015') = CFP_LandfillGas_total('2016') /1.30 ;
  Loop( t,
    If( (t.val < 2015),
      CFP_LandfillGas_total(t) = CFP_LandfillGas_total('2015') ;
    elseif( (t.val > 2018) and (t.val >= 2030)),
      CFP_LandfillGas_total(t) = CFP_LandfillGas_total(t-1)*1.05 ;
    elseif( (t.val > 2030) ),
      CFP_LandfillGas_total(t) = CFP_LandfillGas_total('2030') ;
    );
  );

  npv_CFP_LandfillGas_total(ActivePeriod) =
      npv2(CFP_LandfillGas_Total(t),Rate(t));

* end 8-16-2019 update


  CFP_Alt_CFactor(CFP_PetCategory,CFP_Vehicle_Types,CFP_Years)$CFP_AltVehicle_Carbon_Factor(CFP_PetCategory,CFP_Vehicle_Types) = sum(CFP_RefReg,
      (CFP_AltVehicle_Carbon_Factor(CFP_PetCategory,CFP_Vehicle_Types) - CFP_Target(CFP_PetCategory,CFP_RefReg,CFP_Years)) / 365.0 );

  npv_CFP_Alt_CFactor(CFP_PetCategory,CFP_Vehicle_Types,ActivePeriod)$CFP_AltVehicle_Carbon_Factor(CFP_PetCategory,CFP_Vehicle_Types) =
      npv2(CFP_Alt_CFactor(CFP_PetCategory,CFP_Vehicle_Types,t),Rate(t));

  npv_CFP_AltVehicle_Demand(CFP_PetCategory,CFP_Vehicle_Types,CFP_RefReg,ActivePeriod)$CFP_AltVehicle_Carbon_Factor(CFP_PetCategory,CFP_Vehicle_Types) =
      npv2(CFP_AltVehicle_Demand(CFP_PetCategory,CFP_Vehicle_Types,CFP_RefReg,t),Rate(t));

  CFP_Price(CFP_PetCategory,CFP_Years) = sum((LCFS_C,MNXYRS)$(CFP_M2(CFP_PetCategory,LCFS_C) and tMNXYRS(CFP_Years,MNXYRS)),
      max(0,LFMMOUT_CFP_Offset_Prc(LCFS_C,MNXYRS)) );

  npv_CFP_Price(CFP_PetCategory,ActivePeriod) = npv1(CFP_Price(CFP_PetCategory,t),Rate(t));

* Isolate ethanol factors from process/mode to account for transfers in and out of OR
  CFP_Eth_CFactor(CFP_PetCategory('CFGout'),CFP_BioStreams(EthStream),t)$((sum((CFP_BioProcess,CFP_BioMode)$CFP_BioStream_Carbon_Factor(CFP_PetCategory,CFP_BioProcess,CFP_BioMode,CFP_BioStreams,t), 1))>0) =
    sum((CFP_BioProcess,CFP_BioMode)$CFP_BioStream_Carbon_Factor(CFP_PetCategory,CFP_BioProcess,CFP_BioMode,CFP_BioStreams,t),
     (CFP_BioStream_Carbon_Factor(CFP_PetCategory,CFP_BioProcess,CFP_BioMode,CFP_BioStreams,t) -
      CFP_BioStream_PetCarbonFactor(CFP_PetCategory,CFP_BioProcess,CFP_BioMode,CFP_BioStreams,t)) * CFP_Bio_Energy_Density(CFP_BioStreams) * 0.001 ) /
    sum((CFP_BioProcess,CFP_BioMode)$CFP_BioStream_Carbon_Factor(CFP_PetCategory,CFP_BioProcess,CFP_BioMode,CFP_BioStreams,t),
      1 ) ;

  npv_CFP_Eth_CFactor(CFP_PetCategory,CFP_BioStreams,ActivePeriod) =
      npv2(CFP_Eth_CFactor(CFP_PetCategory,CFP_BioStreams,t),Rate(t));


* AB-32 parameters
  npv_AB32_CapAdjFactor(ActivePeriod)                            = npv2(AB32_CapAdjFactor(t),Rate(t)) ;
  npv_AB32_AssistFactor(ActivePeriod)                            = npv2(AB32_AssistFactor(t),Rate(t)) ;
  npv_AB32_AllowPrice(ActivePeriod)                              = npv1(AB32_AllowPrice(t),Rate(t)) ;

* Total carbon tax (national + AB-32)
  npv_CarbonTax(Stream,CenDiv,ActivePeriod)                      = npv1(CarbonTax(Stream,CenDiv,t),Rate(t)) ;
  npv_CarbonTaxCredit(Stream,ActivePeriod)                       = npv1(CarbonTaxCredit(Stream,t),Rate(t)) ;

* Refinery CO2 emissions tax in dollars per ton CO2
  npv_CO2EmissionTax(ActivePeriod)                               = npv1(CO2EmissionTax(t),Rate(t)) ;

* Set up fractions for RFS constraints
  RFSdenom(ActivePeriod)                        = sum(CenDiv, (npv_ProductDemand(CenDiv,'CFGout',ActivePeriod) +
                                                         npv_ProductDemand(CenDiv,'RFGout',ActivePeriod) +
                                                         npv_ProductDemand(CenDiv,'CaRBOBout',ActivePeriod) +
                                                         npv_ProductDemand(CenDiv,'E85out',ActivePeriod)) * npv_PetMGFrac(ActivePeriod) +
                                                        (npv_ProductDemand(CenDiv,'DSUout',ActivePeriod) +
                                                         npv_ProductDemand(CenDiv,'CarbDSUout',ActivePeriod)) * npv_PetDSFrac(ActivePeriod));

  RFSFraction(RFSCategory,ActivePeriod)$(RFSdenom(ActivePeriod)>0)
                                          = npv_RFSMandates(RFSCategory,ActivePeriod) /RFSdenom(ActivePeriod) ;


* * Set the advanced and total RFS waiver prices so that the delivered gasoline price is lower than the stipulated hardship price (based on E10 gas)
  npv_RFSWaiverPrice(RFSCategory,ActivePeriod) = npv1(RFSWaiverPrice(RFSCategory,t),Rate(t));
  npv_WSGasPrices(ActivePeriod)                = npv1(WSGasPrices(t),Rate(t));

*  npv_RFSWaiverPrice(RFSCategory('Advanced'),ActivePeriod) =
*    (npv_RFSWaiverPrice(RFSCategory,ActivePeriod) - npv_WSGasPrices(ActivePeriod)) /
*    (RFSFraction(RFSCategory,ActivePeriod)*npv_PetMGFrac_noETH(ActivePeriod)*0.9) ;
*  npv_RFSWaiverPrice(RFSCategory('Total'),ActivePeriod) =
*    (npv_RFSWaiverPrice(RFSCategory,ActivePeriod) - npv_WSGasPrices(ActivePeriod)) /
*    (RFSFraction(RFSCategory,ActivePeriod)*npv_PetMGFrac_noETH(ActivePeriod)*0.9) ;

*set E85 markups based on E85curveSwitch
if ((E85curveSwitch = 1),
   npv_E85TotalMarkups(ActivePeriod,ActiveDem) = npv_TotalMarkups(ActivePeriod,'E','T',ActiveDem) ;
elseif (E85curveSwitch = 2),
   npv_E85TotalMarkups(ActivePeriod,ActiveDem) = 0.0 ;
);

*----- Set up E85 Demand Curve -----
  QFFVt(CenDiv,ModelYears(t))  = FlexFuelDemand(CenDiv,t);
  QFFV(CenDiv,ActivePeriod)          = npv2(QFFVt(CenDiv,t),Rate(t));

* Convert curves into Mbbl/day and 87$/bbl
  Q_E85STP(E85_Steps,CenDiv,ActivePeriod)          = npv2((Q_E85STP_t(E85_Steps,CenDiv,t) * Trill_Btu_to_MBBL_CD(t)),Rate(t)) ;

* Set E85 price steps to be the incremental profit over selling gasoline

if (E85curveSwitch = 1,
   P_E85STP_t(E85_Steps,ActiveDem,t) = 0.70* P_E85STP_t(E85_Steps,ActiveDem,t) ;
   P_E85STP(E85_Steps,CenDiv,ActivePeriod)          = npv1(((MOGASPRICE(CenDiv,t)-P_E85STP_t(E85_Steps,CenDiv,t)) * D_MMBtu_to_D_BBL_CD(t)),Rate(t)) ;
elseif (E85curveSwitch = 2),
   P_E85Markups_delta(t,CenDiv) = PoE85(CenDiv,t)  -  TotalMarkups(t,'E','T',CenDiv);
   P_E85STP(E85_Steps,CenDiv,ActivePeriod)          = npv1((((P_E85STP_t(E85_Steps,CenDiv,t))-P_E85Markups_delta(t,CenDiv)) * D_MMBtu_to_D_BBL_CD(t)),Rate(t)) ;
);


  P_E85_IC(E85_Steps,CenDiv,ActivePeriod)          = npv1(P_E85_IC_t(E85_Steps,CenDiv,t),Rate(t));
  P_E85_SC(E85_Steps,CenDiv,ActivePeriod)          = npv1(P_E85_SC_t(E85_Steps,CenDiv,t),Rate(t));
  A_E85STP(E85_Steps,CenDiv,ActivePeriod)          = npv2(A_E85STP_t(E85_Steps,CenDiv,t),Rate(t));


* DEBUGGING
* DISTRESSIMPORT.fx(ActiveDem,EndProduct,Step,'Per1') = 0.0 ;
* DISTRESSIMPORT.fx(ActiveDem,EndProduct,Step,'Per2') = 0.0 ;
* DISTRESSIMPORT.fx(ActiveDem,EndProduct,Step,'Per3') = 0.0 ;

* Toggle product demands during debugging
* RecipeEnd('DSUout')     = no ;
* RecipeEnd('JTAout')     = no ;
* RecipeEnd('DSLout')     = no ;
* RecipeEnd('AVGout')     = no ;
* RecipeEnd('N2Hout')     = no ;
* RecipeEnd('PCFout')     = no ;
* RecipeEnd('N6Iout')     = no ;
* RecipeEnd('N6Bout')     = no ;
* RecipeEnd('LUBout')     = no ;
* RecipeEnd('ASPHout')    = no ;

* For calibration purposes
  if(DoCalibrate = 1,
**  BUILDS.fx(RefReg,RefType,'SEW',BldStep,Period) = 0 ;
**  BUILDS.fx(RefReg,RefType,'NCE',BldStep,Period) = 0 ;
**  BUILDS.fx(RefReg,RefType,'GTL',BldStep,Period) = 0 ;

*   Turn off the RFS
    RFSFraction(RFSCategory,Period) = 0 ;

    AvailCap(RefReg,RefType,'FUM') = 5000;
    AvailCap(RefReg,RefType,'DC4') = 5000;
*   AvailCap(RefReg,RefType,'KWG') = 5000;
*   AvailCap(RefReg,RefType,'SGP') = 5000;
*   AvailCap(RefReg,RefType,'UGP') = 5000;
    AvailCap(RefReg,RefType,'STG') = 50000;
*   AvailCap(RefReg,RefType,'SUL') = 50000;
    AvailCap(RefReg,RefType,'RGN') = 0;
    AvailCap(RefReg,RefType,'RGN') $(not sameas(RefType,'ETH_REF')) = 500;
*   AvailCap(RefReg,RefType,'FGS') = AvailCap(RefReg,RefType,'FCC');
*   AvailCap(RefReg,RefType,'DC5') = AvailCap(RefReg,RefType,'FCC');
*   AvailCap(RefReg,RefType,'LNS') = AvailCap(RefReg,RefType,'NDS');

  else
*   Set these process capacities for now since they are not in the OGJ data, NEEDS UPDATING!!

*   AvailCap(RefReg,RefType,'FUM') = 5000;
*   AvailCap(RefReg,RefType,'DC4') = 5000;
*   AvailCap(RefReg,RefType,'KWG') = 5000;
*   AvailCap(RefReg,RefType,'SGP') = 5000;
*   AvailCap(RefReg,RefType,'UGP') = 5000;
*   AvailCap(RefReg,RefType,'STG') = 50000;
*   AvailCap(RefReg,RefType,'SUL') = 50000;
    AvailCap(RefReg,RefType,'RGN') = 0;
    AvailCap(RefReg,RefType,'RGN') $(not sameas(RefType,'ETH_REF')) = 500;
*   AvailCap(RefReg,RefType,'FGS') = AvailCap(RefReg,RefType,'FCC');
*   AvailCap(RefReg,RefType,'DC5') = AvailCap(RefReg,RefType,'FCC');
*   AvailCap(RefReg,RefType,'LNS') = AvailCap(RefReg,RefType,'NDS');

  );


* Set up the maximum builds allowed for first build step (usually for alternative fuels)
  loop((Process,BldStep('STEP01'),t)$(ModelYears(t) and AFGrowthRate(Process) and
                                     (not sameas(Process,'CTL')) and (not sameas(Process,'CTLCCS')) and
                                     (not sameas(Process,'CBL')) and (not sameas(Process,'CBLCCS'))),
*== if(RunYears(t),
    if( (RunYears(t) or (t.val < FirstBldYear)),
      TotalBuilds(Process,t) = sum((RefReg,RefType), AvailCap(RefReg,RefType,Process) ) ;
      MaxBuilds(Process,BldStep,t)   = 0 ;
    else
      MaxBuilds(Process,BldStep,t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds(Process,tt))>0) =
        max(CapExpSize(Process), sum(tt$(ord(tt)=ord(t)-1), TotalBuilds(Process,tt)) * AFGrowthRate(Process) ) ;

      TotalBuilds(Process,t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds(Process,tt))>0) =
        sum(tt$(ord(tt)=ord(t)-1), TotalBuilds(Process,tt)) + MaxBuilds(Process,BldStep,t) ;

      MaxBuilds(Process,BldStep,t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds(Process,tt))=0) =
        AFInitPlants(Process)*CapExpSize(Process) ;
      TotalBuilds(Process,t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds(Process,tt))=0) =
        AFInitPlants(Process)*CapExpSize(Process) ;
    );
  );

* Compute growth bounds for CTL/CTLCCS and CBL/CBLCCS as combined bounds
  loop((BldStep('STEP01'),t)$ModelYears(t),
*== if(RunYears(t),
    if( (RunYears(t) or (t.val < FirstBldYear)),
      TotalBuilds('CTL',t) = sum((RefReg,RefType), AvailCap(RefReg,RefType,'CTL') + AvailCap(RefReg,RefType,'CTLCCS') ) ;
      MaxBuilds('CTL',BldStep,t)   = 0 ;
    else
      MaxBuilds('CTL',BldStep,t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds('CTL',tt))>0) =
        max(CapExpSize('CTL'), sum(tt$(ord(tt)=ord(t)-1), TotalBuilds('CTL',tt)) * AFGrowthRate('CTL') ) ;

      TotalBuilds('CTL',t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds('CTL',tt))>0) =
        sum(tt$(ord(tt)=ord(t)-1), TotalBuilds('CTL',tt)) + MaxBuilds('CTL',BldStep,t) ;

      MaxBuilds('CTL',BldStep,t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds('CTL',tt))=0) =
        AFInitPlants('CTL')*CapExpSize('CTL') ;
      TotalBuilds('CTL',t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds('CTL',tt))=0) =
        AFInitPlants('CTL')*CapExpSize('CTL') ;
    );

*== restrict growth until after FirstBldYear (hardcoded in lfprep.gms),
*==     except for FUM, SGP, UGP, STG, SUL, LUB, LNS, DC4, H2P (later in code)
*== if(RunYears(t),
    if( (RunYears(t) or (t.val < FirstBldYear)),
      TotalBuilds('CBL',t) = sum((RefReg,RefType), AvailCap(RefReg,RefType,'CBL') + AvailCap(RefReg,RefType,'CBLCCS') ) ;
      MaxBuilds('CBL',BldStep,t)   = 0 ;
    else
      MaxBuilds('CBL',BldStep,t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds('CBL',tt))>0) =
        max(CapExpSize('CBL'), sum(tt$(ord(tt)=ord(t)-1), TotalBuilds('CBL',tt)) * AFGrowthRate('CBL') ) ;

      TotalBuilds('CBL',t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds('CBL',tt))>0) =
        sum(tt$(ord(tt)=ord(t)-1), TotalBuilds('CBL',tt)) + MaxBuilds('CBL',BldStep,t) ;

      MaxBuilds('CBL',BldStep,t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds('CBL',tt))=0) =
        AFInitPlants('CBL')*CapExpSize('CBL') ;
      TotalBuilds('CBL',t)$(sum(tt$(ord(tt)=ord(t)-1),TotalBuilds('CBL',tt))=0) =
        AFInitPlants('CBL')*CapExpSize('CBL') ;
    );
  );


  MaxBuilds(Process,BldStep,t)           = AFCapMult(Process,BldStep) * MaxBuilds(Process,'STEP01',t) ;
  npv_MaxBuilds(Process,BldStep,ActivePeriod)  = npv2(MaxBuilds(Process,BldStep,t),Rate(t)) ;

*-------------------------------------------------------------------------------
* Put any upper bound values here - needs to be here instead of LFMM_model




* Assign upper bounds to Crude Supply Vectors - Total and Incremental
  CRUDETOTAL.up(Step,ActivePeriod)$npv_CrudeSupplyTotal(Step,ActivePeriod) =
    npv_CrudeSupplyTotal(Step,ActivePeriod) ;

  CRUDEPURCH.up(Source,Crude,Step,ActivePeriod)$npv_CrudeSupplyIncremental(Source,Crude,Step,ActivePeriod) =
    npv_CrudeSupplyIncremental(Source,Crude,Step,ActivePeriod) ;

  CRUDEPURCH.up(Source,Crude,"STEP14",ActivePeriod) = 50000.0;

* Bound the operations of existing capacity to the amount previously built
  OPERATECAP.up(RefReg,RefType,Process,ActivePeriod) =
    AvailCap(RefReg,RefType,Process)$(AvailCap(RefReg,RefType,Process)>0) ;

$ontext

* set lower limit on KRD utilization, beginning in 2015
  if(CurYrIdx >= 26,
    OPERATECAP.lo('1_REFREG','COKING','KRD',Period) =
      0.735 * OPERATECAP.up('1_REFREG','COKING','KRD',Period) ;
    OPERATECAP.lo('2_REFREG','COKING','KRD',Period) =
      0.893 * OPERATECAP.up('2_REFREG','COKING','KRD',Period) ;
    OPERATECAP.lo('3_REFREG','COKING','KRD',Period) =
      0.893 * OPERATECAP.up('3_REFREG','COKING','KRD',Period) ;
    OPERATECAP.lo('4_REFREG','COKING','KRD',Period) =
      0.885 * OPERATECAP.up('4_REFREG','COKING','KRD',Period) ;
    OPERATECAP.lo('5_REFREG','COKING','KRD',Period) =
      0.885 * OPERATECAP.up('5_REFREG','COKING','KRD',Period) ;
    OPERATECAP.lo('6_REFREG','COKING','KRD',Period) =
      0.795 * OPERATECAP.up('6_REFREG','COKING','KRD',Period) ;
    OPERATECAP.lo('7_REFREG','COKING','KRD',Period) =
      0.895 * OPERATECAP.up('7_REFREG','COKING','KRD',Period) ;
    OPERATECAP.lo('8_REFREG','COKING','KRD',Period) =
      0.895 * OPERATECAP.up('8_REFREG','COKING','KRD',Period) ;
  );

$offtext


* Enforce bounds on supply steps for purchases and imports
  DISTRESSIMPORT.up(ActiveDem,EndProduct,Step,ActivePeriod) =
    npv_GlobalImpSupply(ActiveDem,EndProduct,Step,ActivePeriod)$(npv_GlobalImpSupply(ActiveDem,EndProduct,Step,ActivePeriod)>0) ;

* Set up supply curve bounds for advance ethanol imports from Brazil
* ETH_BRAZIL.up(EthStream,Step,ActivePeriod) = npv_EthBrazilSup(EthStream,Step,ActivePeriod) ;

* Set up supply curve bounds for advance ethanol imports to US
  ETHIMPUSsup.up(EthStream,Step,ActivePeriod) = npv_EthImpSupQty(EthStream,Step,ActivePeriod) ;
* Set up demand curve bounds for corn ethanol exports from US
  ETHEXPUSdmd.up(EthStream,Step,ActivePeriod) = npv_EthExpDmdQty(EthStream,Step,ActivePeriod) ;

* For the last step, have no upper bound
  BIOPURCH.up(CoalDReg,BioStr,Step,ActivePeriod)$(not ord(Step) = card(Step)) =
    npv_BiomassSup(CoalDReg,BioStr,Step,ActivePeriod)$(npv_BiomassSup(CoalDReg,BioStr,Step,ActivePeriod)>0) ;

  REFPURCH.up(RefReg,RefInputStr,Step,ActivePeriod)$(not NGLInputStr(RefInputStr)) =
    npv_RefInpSup(RefReg,RefInputStr,Step,ActivePeriod)$(npv_RefInpSup(RefReg,RefInputStr,Step,ActivePeriod)>0) ;

  REFPURCH.fx(RefReg,NGLInputStr,Step,ActivePeriod)$(ord(Step)=1) =
    npv_NGLBounds(NGLInputStr,RefReg,ActivePeriod)$(npv_NGLBounds(NGLInputStr,RefReg,ActivePeriod)>0) ;
  REFPURCH.up(RefReg,NGLInputStr,Step,ActivePeriod)$(ord(Step)>1) = 0 ;

*  NGLIMPORTS.fx('UC3out',CenDiv,ActivePeriod) = 0 ;
  if ( CurYrIdx+1989 >= 2014,
    NGLIMPORTS.fx('UC3out',CenDiv,ActivePeriod) = 0 ;
  elseif (CurYrIdx+1989+1 >= 2014),
    NGLIMPORTS.fx('UC3out',CenDiv,'Per2') = 0 ;
    NGLIMPORTS.fx('UC3out',CenDiv,'Per3') = 0 ;
  );

  E85STP.up(E85_Steps,CenDiv,ActivePeriod) =
    Q_E85STP(E85_Steps,CenDiv,ActivePeriod)$(Q_E85STP(E85_Steps,CenDiv,ActivePeriod)>0) ;

  COALPURCH.up(CoalSReg,CoalStr,Step,ActivePeriod) =
    npv_CoalSup(CoalSReg,CoalStr,Step,ActivePeriod)$(npv_CoalSup(CoalSReg,CoalStr,Step,ActivePeriod)>0) ;

  MCCDEMAND.up(Stream,Step,ActivePeriod) =
    npv_MCCSupply(Stream,Step,ActivePeriod)$(npv_MCCSupply(Stream,Step,ActivePeriod)>0) ;

  IMPORTS.fx(Stream,RefReg,ActivePeriod)$(not ImpStr(Stream)) = 0 ;
  IMPORTS.fx(Stream,'9_RefReg',ActivePeriod) = 0 ;

  EXPORTS.fx(Stream,RefReg,ActivePeriod)$(not ImpStr(Stream)) = 0 ;
  EXPORTS.fx(Stream,'9_RefReg',ActivePeriod) = 0 ;

  PRODIMP.up(ImpStr,IntReg,Step,ActivePeriod) =
    npv_ImportSupply(ImpStr,IntReg,Step,ActivePeriod) ;

  PRODEXP.up(ImpStr,IntReg,Step,ActivePeriod) =
    npv_ExportSupply(ImpStr,IntReg,Step,ActivePeriod) ;

  COALDEMAND.lo(CoalSReg,CoalStr,ActivePeriod)            = npv_CoalOthDemand(CoalSreg,CoalStr,ActivePeriod);
  BIODEMAND.lo(CoalDReg,BioStr,Other_MKT,ActivePeriod)    = npv_BioOthDemand(CoalDReg,BioStr,Other_MKT,ActivePeriod) ;

  BIODIMP.up(Step,ActivePeriod)$(ord(Step)<6)  = npv_FBDImpSupply(Step,ActivePeriod) ;

  BIODIMP.up(Step,ActivePeriod)$(ord(Step)>=6) = 0.0 ;

  RENEWDIMP.up(Step,ActivePeriod)$(ord(Step)<6)  = npv_RDHImpSupply(Step,ActivePeriod) ;

  RENEWDIMP.up(Step,ActivePeriod)$(ord(Step)>=6) = 0.0 ;

  CO2_PURCH.up(OGCO2Reg,CO2_Source,ActivePeriod) =
    max(0.0001, npv_CO2_Avail(OGCO2Reg,CO2_Source,ActivePeriod)) ;

* Positive net STEO volume targets
  STEO_VOLUME.lo(STEO_Category,ActivePeriod)$((npv_STEO_PhaseOut(ActivePeriod)>0) and (npv_STEO_Target(STEO_Category,ActivePeriod)>=0)) =
    npv_STEO_PhaseOut(ActivePeriod) * npv_STEO_Target(STEO_Category,ActivePeriod) ;
  STEO_VOLUME.up(STEO_Category,ActivePeriod)$((npv_STEO_PhaseOut(ActivePeriod)>0) and (npv_STEO_Target(STEO_Category,ActivePeriod)>=0)) =
    (1 + (1-npv_STEO_PhaseOut(ActivePeriod))) * npv_STEO_Target(STEO_Category,ActivePeriod) ;
* Negative net STEO volume targets
  STEO_VOLUME.up(STEO_Category,ActivePeriod)$((npv_STEO_PhaseOut(ActivePeriod)>0) and (npv_STEO_Target(STEO_Category,ActivePeriod)<0)) =
    npv_STEO_PhaseOut(ActivePeriod) * npv_STEO_Target(STEO_Category,ActivePeriod) ;
  STEO_VOLUME.lo(STEO_Category,ActivePeriod)$((npv_STEO_PhaseOut(ActivePeriod)>0) and (npv_STEO_Target(STEO_Category,ActivePeriod)<0)) =
    (1 + (1-npv_STEO_PhaseOut(ActivePeriod))) * npv_STEO_Target(STEO_Category,ActivePeriod) ;

*---------------------------------------------------------------------------------------------------------
* Put any fixed variable values here

$ontext
*Fix PROCMODE=0 if not a valid (Process,ProcessMode) pair
  PROCMODE.fx(RefReg,RefType,Process,ProcessMode,ActivePeriod)$(not ProcProcMode(Process,ProcessMode)) = 0;
$offtext

* Don't allow biodiesel imports in some RefReg

  BIODIMPref.fx(RefReg,RefType,ActivePeriod)$(not RefRegBiodieselImportsAllowed(RefReg)) = 0;

  RENEWDIMPref.fx(RefReg,RefType,ActivePeriod)$(not RefRegBiodieselImportsAllowed(RefReg)) = 0;


* Assign fixed bounds to Non-US Crude Demand Vectors
  CRUDENONUS.fx(Source,Crude,Step,ActivePeriod)           = npv_NonUSCrudeDemandValue(Source,Crude,Step,ActivePeriod);

*-- Toggle crude exports based on the 'CRUDEXP' scedes switch --*
  if(NCNTRL_CURCALYR>=2011,
    if(CRUDEXP=0,
      CRUDEEXPORT.fx(RefReg,Crude,Source,Step,ActivePeriod) = 0 ;
      CRUDEEXPORT.fx(RefReg,Crude,Source,'Step01',ActivePeriod) = npv_CrudeToCanada(RefReg,Crude,ActivePeriod) ;

*-- allow M-Msour export out of Alaska (RR8)
      CRUDEEXPORT.up('8_REFREG','M_Msour',Source,Step,ActivePeriod) =  99999. ;
      CRUDEEXPORT.lo('8_REFREG','M_Msour',Source,'Step01',ActivePeriod) = npv_CrudeToCanada('8_REFREG','M_Msour',ActivePeriod) ;
      if(LFMMOUT_lfEXPlease>0,
        CRUDEEXPORT.up(RefReg,'Condensate',Source,Step,ActivePeriod) =  99999. ;
        CRUDEEXPORT.lo(RefReg,'Condensate',Source,'Step01',ActivePeriod) = npv_CrudeToCanada(RefReg,'Condensate',ActivePeriod) ;

*-- allow Reg3CrdExp crudes to be exported only from RR3 (to Canada)
        CRUDEEXPORT.up('3_RefReg',Reg3CrdExp,Source,Step,ActivePeriod) =  99999. ;
        CRUDEEXPORT.lo('3_RefReg',Reg3CrdExp,Source,'Step01',ActivePeriod) = npv_CrudeToCanada('3_RefReg',Reg3CrdExp,ActivePeriod) ;

      ) ;
    else
      CRUDEEXPORT.lo(RefReg,Crude,Source,'Step01',ActivePeriod) = npv_CrudeToCanada(RefReg,Crude,ActivePeriod) ;
      CRUDEEXPORT.fx(RefReg,Crude,Source,'Step01',ActivePeriod)$((Not CrdExpAllowed(RefReg,Crude)) and (npv_CrudeToCanada(RefReg,Crude,ActivePeriod)>0)) =
        npv_CrudeToCanada(RefReg,Crude,ActivePeriod) ;
      CRUDEEXPORT.fx(RefReg,Crude,Source,Step,ActivePeriod)$((Not CrdExpAllowed(RefReg,Crude)) and
                                                             (npv_CrudeToCanada(RefReg,Crude,ActivePeriod)>0) and
                                                             (ord(Step)>1) and
                                                              npv_CrudeExportCap(RefReg,Source,Step,ActivePeriod) ) = 0 ;
      CRUDEEXPORT.fx(RefReg,Crude,Source,Step,ActivePeriod)$((Not CrdExpAllowed(RefReg,Crude)) and
                                                             (npv_CrudeToCanada(RefReg,Crude,ActivePeriod)=0) and
                                                              npv_CrudeExportCap(RefReg,Source,Step,ActivePeriod) ) = 0 ;
    );
  else
*   Have to allow crude exports in Period 1 of 2010 since the refineries don't work yet since they are
*   uncalibrated and domestic crude needs to go somewhere
    if(CRUDEXP=0,
      CRUDEEXPORT.fx(RefReg,Crude,Source,Step,'Per2') = 0 ;
      CRUDEEXPORT.fx(RefReg,Crude,Source,Step,'Per3') = 0 ;
    );
  );

*---- Neutralize vectors that are not a valid RefReg / RefType combination
* PROCMODE.fx(RefReg,RefType,Process,ProcessMode,ActivePeriod)$(Not SupTypMode(RefReg,RefType,Process,ProcessMode)) = 0;

* Do not allow any processes to use biomass when they do not draw from any coal demand region
  PROCMODE.fx(RefReg,RefType,'BPU',ProcessMode,ActivePeriod)$(sum(CoalDreg, COALDtoRefMap(CoalDreg,RefReg))=0 ) = 0 ;
  PROCMODE.fx(RefReg,RefType,'CBL',ProcessMode,ActivePeriod)$(sum(CoalDreg, COALDtoRefMap(CoalDreg,RefReg))=0 ) = 0 ;
  PROCMODE.fx(RefReg,RefType,'CBLCCS',ProcessMode,ActivePeriod)$(sum(CoalDreg, COALDtoRefMap(CoalDreg,RefReg))=0 ) = 0 ;
  PROCMODE.fx(RefReg,RefType,'BTL',ProcessMode,ActivePeriod)$(sum(CoalDreg, COALDtoRefMap(CoalDreg,RefReg))=0 ) = 0 ;
  PROCMODE.fx(RefReg,RefType,'CLE',ProcessMode,ActivePeriod)$(sum(CoalDreg, COALDtoRefMap(CoalDreg,RefReg))=0 ) = 0 ;

* 12-18-15 em4, restrict cogen (CGN) to regular efficiency mode (HI eff not allowed) because cause own-use splike
  PROCMODE.fx(RefReg,RefType,'CGN','CGN_HIEFF',ActivePeriod) = 0 ;
  PROCMODE.fx(RefReg,RefType,'CGN','CGN_HIEFFgrd',ActivePeriod) = 0 ;

  CRUDETRANS.fx(RefReg,Crude,Source,Step,ActivePeriod)$(Not npv_CrudeImportCap(RefReg,Source,Step,ActivePeriod)) = 0 ;
  CRUDETRANS.fx(RefReg,Crude,Source,Step,ActivePeriod)$(Not CrdImpAllowed(RefReg,Crude)) = 0 ;



* 03-23-15 pco, constrain upper limit for syncrude imports in RefReg3 to 200 mbpd
* CRUDETRANS.up('3_RefReg','Syncrude','Foreign','Step01',ActivePeriod) = 200;

CRUDETRANS.up('1_RefReg','Syncrude','Foreign','Step01',ActivePeriod) = 10;
CRUDETRANS.up('2_RefReg','Syncrude','Foreign','Step01',ActivePeriod) = 45;
CRUDETRANS.up('3_RefReg','Syncrude','Foreign','Step01',ActivePeriod) = 45;
CRUDETRANS.up('4_RefReg','Syncrude','Foreign','Step01',ActivePeriod) = 10;
CRUDETRANS.up('5_RefReg','Syncrude','Foreign','Step01',ActivePeriod) = 10;
CRUDETRANS.up('6_RefReg','Syncrude','Foreign','Step01',ActivePeriod) = 45;
CRUDETRANS.up('7_RefReg','Syncrude','Foreign','Step01',ActivePeriod) = 5;
CRUDETRANS.up('8_RefReg','Syncrude','Foreign','Step01',ActivePeriod) = 45;

CRUDETRANS.fx(RefReg,'Syncrude','Foreign','Step02',ActivePeriod) = 0.0;



  RefRefTRAN.fx(RefReg,RefRegA,TranMode,Stream,ActivePeriod)$(not npv_REFtoREFTranCost(RefReg,RefRegA,TranMode,Stream,ActivePeriod)) = 0 ;
  RefCenTRAN.fx(RefReg,CenDiv,TranMode,RecipeProd,ActivePeriod)$(not npv_REFtoCDTranCost(RefReg,CenDiv,TranMode,RecipeProd,ActivePeriod)) = 0 ;

  ToSPECBLEND.fx(RefReg,EthRefType,SpecProd,IntStream,ActivePeriod) = 0 ;
  ToRECIPEBLEND.fx(RefReg,EthRefType,RcpMode,SpecProd,ActivePeriod) = 0 ;
  RECIPETOPROD.fx(RefReg,EthRefType,RecipeProd,ActivePeriod) = 0 ;
  COPRODUCTS.fx(RefReg,EthRefType,CoProduct,ActivePeriod)$(not EthCoProduct(CoProduct)) = 0 ;

* Do not allow cracking refineries to build coking capacity
* OBSOLETE, no longer representing CRACKING refineries
* BUILDS.fx(RefReg,'CRACKING','KRD',BldStep,ActivePeriod) = 0 ;

* Don't allow any BUILDS in 'per1'... ever.
  BUILDS.fx(RefReg,RefType,Process,BldStep,'per1') = 0;

* No builds allowed (in per2) if curcalyr+1 is less than first build year
* No builds allowed (in per3) if first build year > 2050
  Loop( (Process,RefReg),
    if( NCNTRL_CURCALYR+1 < FirstBldYearMatrix(Process,RefReg),
      BUILDS.fx(RefReg,RefType,Process,BldStep,'per2')    = 0 ;
    );

    if( NCNTRL_CURCALYR+2+P_yrs('per3') < FirstBldYearMatrix(Process,RefReg),
      BUILDS.fx(RefReg,RefType,Process,BldStep,'per3')    = 0 ;
    );
  );

* Don't allow ethanol process builds at petroleum refineries
  BUILDS.fx(RefReg,PetRefType,EthanolProcess,BldStep,ActivePeriod) = 0;

* Don't allow petroleum process builds at ethanol refineries
  BUILDS.fx(RefReg,'ETH_REF',Process,BldStep,ActivePeriod)$(not EthanolProcess(Process)) = 0;


*????
  BUILDS.fx(RefReg,RefType,Process,BldStep,ActivePeriod)$((ord(BldStep)>1) and (AFGrowthRate(Process)=0)) = 0 ;
*????

  RFS_PRD_to_RQM.fx(RFSCategory,PrcPeriod)                   = 0 ;
  RFS_RQM_CST.fx(RFSCategory,ActivePeriod)$(not PrcPeriod(ActivePeriod)) = 0 ;
  RFS_PRD_VAL.fx(RFSCategory,ActivePeriod)$(not PrcPeriod(ActivePeriod)) = 0 ;

  if(NCNTRL_CURCALYR > 2010,
    RFS_PRD_VAL.lo(RFSCategory,ActivePeriod(PrcPeriod)) =
      max(0, sum((M4,MNUMYR)$(M4_2_RFSCat(M4,RFSCategory) and Per1_MNUMYR(ActivePeriod,MNUMYR)), LFMMOUT_RFSACTUAL(M4,MNUMYR)) - 0.025) ;

    RFS_PRD_VAL.up(RFSCategory,ActivePeriod(PrcPeriod)) =
      sum((M4,MNUMYR)$(M4_2_RFSCat(M4,RFSCategory) and Per1_MNUMYR(ActivePeriod,MNUMYR)), LFMMOUT_RFSACTUAL(M4,MNUMYR)) + 0.025 ;
  ) ;

  LCFS_Pet_to_Bio.fx(LCFS_PetCategory,PrcPeriod)                   = 0 ;
  LCFS_Pet_Cst.fx(LCFS_PetCategory,ActivePeriod)$(not PrcPeriod(ActivePeriod)) = 0 ;
  LCFS_Bio_Sub.fx(LCFS_PetCategory,ActivePeriod)$(not PrcPeriod(ActivePeriod)) = 0 ;
  LCFSSafety.fx(LCFS_PetCategory,PrcPeriod)                        = 0 ;
  LCFS_AltVehicles.fx(LCFS_PetCategory,LCFS_Vehicle_Types,ActivePeriod) = sum(LCFS_RefReg,
    npv_LCFS_AltVehicle_Demand(LCFS_PetCategory,LCFS_Vehicle_Types,LCFS_RefReg,ActivePeriod)) ;

  CFP_Pet_to_Bio.fx(CFP_PetCategory,PrcPeriod)                   = 0 ;
  CFP_Pet_Cst.fx(CFP_PetCategory,ActivePeriod)$(not PrcPeriod(ActivePeriod)) = 0 ;
  CFP_Bio_Sub.fx(CFP_PetCategory,ActivePeriod)$(not PrcPeriod(ActivePeriod)) = 0 ;
  CFPSafety.fx(CFP_PetCategory,PrcPeriod)                        = 0 ;
  CFP_AltVehicles.fx(CFP_PetCategory,CFP_Vehicle_Types,ActivePeriod) = sum(CFP_RefReg,
    npv_CFP_AltVehicle_Demand(CFP_PetCategory,CFP_Vehicle_Types,CFP_RefReg,ActivePeriod)) ;

* Can only export No-RFS corn
  ETHEXP.fx(RefReg,Ethstream('ETHCRN'),ActivePeriod) = 0 ;
  ETHEXP.fx(RefReg,Ethstream('ETHGRN'),ActivePeriod) = 0 ;
  ETHEXP.fx(RefReg,Ethstream('ETHCLE'),ActivePeriod) = 0 ;
  ETHEXP.fx(RefReg,Ethstream('ETHAET'),ActivePeriod) = 0 ;
  ETHEXP.fx(RefReg,Ethstream('ETHAETbrz'),ActivePeriod) = 0 ;
  ETHEXP.fx(RefReg,Ethstream('IBA'),ActivePeriod) = 0 ;
  ETHEXP.fx(RefReg,Ethstream('IBApre'),ActivePeriod) = 0 ;

* Restrict direct ethanol exports from all refining but 1 and 4
  ETHEXP.fx('2_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP.fx('3_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP.fx('5_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP.fx('6_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP.fx('7_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP.fx('8_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP.fx('9_REFREG',Ethstream,ActivePeriod) = 0 ;

  ETHEXP_2.fx('2_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP_2.fx('3_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP_2.fx('4_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP_2.fx('5_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP_2.fx('6_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP_2.fx('7_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP_2.fx('8_REFREG',Ethstream,ActivePeriod) = 0 ;
  ETHEXP_2.fx('9_REFREG',Ethstream,ActivePeriod) = 0 ;
* Set lower bounds on corn ethanol exports
* Temp, until we make this a proper constraint, where LP decides source of exports
* and lower bound is read from data file rather than hard-coded
* 1000 million gallons per year from RefReg 1 & 3
* to convert to mbbl/d : * (1/42) * 1000 * (1/365)

*rm  ETHEXP_2.up('1_REFREG','ETHCRNexp',ActivePeriod) = 1000 *(1/42) * (1000) * (1/365) ;
*rm
*rm if(NCNTRL_CURCALYR>2017,
*rm   ETHEXP_2.up('1_REFREG','ETHCRNexp',ActivePeriod) = (1000 + (NCNTRL_CURCALYR-2016)*100)* (1/42) * (1000) * (1/365) ;
*rm ) ;
*rm
*rm if(NCNTRL_CURCALYR>2030,
*rm   ETHEXP_2.up('1_REFREG','ETHCRNexp',ActivePeriod) = 2300  *(1/42) * (1000) * (1/365) ;
*rm ) ;

*rm 1-25-19, replace above with below (no 'special' exports out of RefReg 1, ETHEXP_2)

  ETHEXP_2.fx('1_REFREG',Ethstream,ActivePeriod) = 0 ;




* Restrict ethanol imports to regions 4,7 only
* Restrict ethanol imports to regions 1,4,7 only
* ETHIMP.fx('1_REFREG',Ethstream('ETHAETbrz'),ActivePeriod)    = 1 ;
  ETHIMP.fx('1_REFREG',Ethstream('ETHAETbrz'),ActivePeriod)    = 0 ;
  ETHIMP.fx('2_REFREG',Ethstream('ETHAETbrz'),ActivePeriod)    = 0 ;
  ETHIMP.fx('3_REFREG',Ethstream('ETHAETbrz'),ActivePeriod)    = 0 ;
* ETHIMP.fx('4_REFREG',Ethstream('ETHAETbrz'),ActivePeriod)    = 2 ;
  ETHIMP.fx('4_REFREG',Ethstream('ETHAETbrz'),ActivePeriod)    = 0 ;
  ETHIMP.fx('5_REFREG',Ethstream('ETHAETbrz'),ActivePeriod)    = 0 ;
  ETHIMP.fx('6_REFREG',Ethstream('ETHAETbrz'),ActivePeriod)    = 0 ;
  ETHIMP.fx('8_REFREG',Ethstream('ETHAETbrz'),ActivePeriod)    = 0 ;


  ETHIMP.up('7_REFREG',Ethstream('ETHAETbrz'),ActivePeriod)    = 0 ;

  if(NCNTRL_CURCALYR=2016,
    ETHIMP.up('7_REFREG',Ethstream('ETHAETbrz'),'Per1')    = 0 ;
    ETHIMP.up('7_REFREG',Ethstream('ETHAETbrz'),'Per2')    = 20 * (1.00+(((NCNTRL_CURCALYR+1)-2017)*.12))  ;
    ETHIMP.up('7_REFREG',Ethstream('ETHAETbrz'),'Per3')    = 20 * (1.00+(((NCNTRL_CURCALYR+2)-2017)*.12))  ;
  elseif(NCNTRL_CURCALYR=2017),
    ETHIMP.up('7_REFREG',Ethstream('ETHAETbrz'),'Per1')    = 0 ;
    ETHIMP.up('7_REFREG',Ethstream('ETHAETbrz'),'Per2')    = 20 * (1.00+(((NCNTRL_CURCALYR+1)-2017)*.12))  ;
    ETHIMP.up('7_REFREG',Ethstream('ETHAETbrz'),'Per3')    = 20 * (1.00+(((NCNTRL_CURCALYR+2)-2017)*.12))  ;
  elseif(NCNTRL_CURCALYR>2017),
    ETHIMP.up('7_REFREG',Ethstream('ETHAETbrz'),'Per1')    = 30 * (1.00+((NCNTRL_CURCALYR-2017)*.12))  ;
    ETHIMP.up('7_REFREG',Ethstream('ETHAETbrz'),'Per2')    = 30 * (1.00+(((NCNTRL_CURCALYR+1)-2017)*.12))  ;
    ETHIMP.up('7_REFREG',Ethstream('ETHAETbrz'),'Per3')    = 30 * (1.00+(((NCNTRL_CURCALYR+2)-2017)*.12))  ;
  ) ;


* Can only import advanced ethanol at this point
  ETHIMP.fx(RefReg,Ethstream('ETHCRN'),ActivePeriod)    = 0 ;
  ETHIMP.fx(RefReg,Ethstream('ETHCRNexp'),ActivePeriod) = 0 ;
  ETHIMP.fx(RefReg,Ethstream('ETHGRN'),ActivePeriod)    = 0 ;
  ETHIMP.fx(RefReg,Ethstream('ETHCLE'),ActivePeriod)    = 0 ;
  ETHIMP.fx(RefReg,Ethstream('ETHAET'),ActivePeriod)    = 0 ;
  ETHIMP.fx(RefReg,Ethstream('ETHAET'),ActivePeriod)    = 0 ;
  ETHIMP.fx(RefReg,Ethstream('IBA'),ActivePeriod)       = 0 ;
  ETHIMP.fx(RefReg,Ethstream('IBApre'),ActivePeriod)    = 0 ;

* Non-Us Ethanol demand is a fixed value
* ETHNonUS.fx(ActivePeriod)       = npv_NonUSEthDmd(ActivePeriod) ;

  RFSESCAPE.fx(RFSCategory,'Per1')  = 0 ;






* Try to address the issue of lube demand being larger then capacity
  npv_ProductDemand(CenDiv,'LUBout','Per2')$ActivePeriod('Per2') =
    npv_ProductDemand(CenDiv,'LUBout','Per2') * 1.03 ;

* Turn off E15 and B16 gasoline blends in Census 9
  QMG15.fx('CenDiv9',ActivePeriod)  = 0 ;
  QMGb16.fx('CenDiv9',ActivePeriod) = 0 ;


$ontext
LFMM.optfile=1
$onecho > lfshell.gck
ANALYSIS
$offecho
option LP=GAMSCHK;
$offtext

*  put_utility 'msglog' /
  put_utility 'msglog' / '(cycle, year, iteration): ('
    CYCLEINFO_CURIRUN:0:0 ', ' RunYears.te(RunYears) ', ' NCNTRL_CURITR_orig:0:0 ')';

maxExecError=10;
* Solve the model
  if (STEO_Years(RunYears) and LFadjvolON=1,

*   First solve with STEO volume constraints off and record the results
    STEO_ApplyContraints = 0 ;
    LFMM.savepoint       = 2 ;
    LFMM.Holdfixed       = 0 ;
    Solve LFMM minimizing TotalCost using lp ;

*   Save build period capacity expansion decisions for UNCONSTRAINED case
    PriorBuilds(RefReg,RefType,Process,RunYears) = sum((BldStep,Period)$BldPeriod(Period),BUILDS.l(RefReg,RefType,Process,BldStep,Period)) ;
    Execute_Unload 'LFMM_Builds.gdx', PriorBuilds ;

*   Now solve with the STEO volume constraints on
    STEO_ApplyContraints = 1 ;
    Solve LFMM minimizing TotalCost using lp ;

*   Load the contraint marginal values from the unconstrainted case to overwrite the constrained case marginals
    Execute_LoadPoint 'LFMM_p1.gdx',
     CrudeBalance.m, CrudeSupCurveForeign.m, WorldCrudeSup.m, CrudeImportLimit.m,
     CrudeExportLimit.m, CrudeExportLimit3CAN.m, ExportDomCrudeOnly.m,
*    BrzMaxExportsToUS.m, EthWorldBal.m, BrzEthProdBal.m,
     EthBalance.m, UtilBalance.m,
     RefInpBalance.m, RefPurchBal.m, BioBalance.m, BioRefRegBal.m,
     CoalDemBalance.m, CoalSupBalance.m, CoalRefRegBal.m, SO2EmisBal.m, HGEmisBal.m,
     StreamBalance.m, CapacityBalance.m, MaxLTE.m, RecipeBalance.m, RecipeTransfer.m,
     RecipeBPTransfer.m, GasSpecQualMin.m, GasSpecQualMax.m, GasSpecBalance.m,
     DistSpecQualMin.m, DistSpecQualMax.m, DistSpecBalance.m, ResidSpecQualMin.m,
     ResidSpecQualMax.m, ResidSpecBalance.m, CombineSupply.m, CDSupplyTot.m,
     REFtoREFTran.m, TotPRODTran.m, REFtoCDTran.m, REFtoCDCap.m, REFtoREFCapMB.m,
     ImpTranLimit.m, ExpTranLimit.m,
     RecipeDemands.m, NGLDmd.m, RecipeEndBal.m, NGLEndBal.m, FlexBTUDmd.m,
     MGTotal.m, E85Total.m, E85Balance.m, CFGBalance.m, RFGBalance.m,
     CFG15Balance.m, RFG15Balance.m, CFGb16Balance.m, RFGb16Balance.m,
     CaRBOBBalance.m, E15Max.m, RFSConstraintsPRD.m, RFSConstraintsRQM.m,
     LCFS_Petroleum.m, LCFS_BioFuel.m, AB32_Constraint.m, BOBBalance.m,
     CFP_Petroleum.m, CFP_Biofuel.m,
     RestrictGrowth.m, RestrictGrowthCTL.m, RestrictGrowthCBL.m, RestrictGrowthCTLGTL.m,
     BiodieselBalance.m, MaxCCRtoFCC.m, OGSM_CO2Demand.m, OGSM_CO2TranBal.m,
     CO2CCSBalance.m, ProdImpBalance.m, ProdImpSupCurve.m, ProdExpBalance.m,
     ProdExpSupCurve.m, StateBiodiesel.m ;

  else
    STEO_ApplyContraints = 0 ;
    Solve LFMM minimizing TotalCost using lp ;

*   Save build period capacity expansion decisions
    PriorBuilds(RefReg,RefType,Process,RunYears) = sum((BldStep,Period)$BldPeriod(Period),BUILDS.l(RefReg,RefType,Process,BldStep,Period)) ;
    Execute_Unload 'LFMM_Builds.gdx', PriorBuilds ;
  );

  put_utility 'msglog' / '(solvestat, modelstat): (' LFMM.SolveStat:0:0 ', ' LFMM.ModelStat:0:0 ')';

  If(LFMM.ModelStat>1,
    LFMMReporting = 0 ;
  );

  LFMMOUT_LFMMCODE = LFMM.ModelStat ;
  If(LFMM.SolveStat>1,
     LFMMOUT_LFMMCODE = 20 + LFMM.SolveStat;
     LFMMReporting = 0 ;
*  reset LFMM.ModelStat so that it doesn't do the next IF block.
     LFMM.ModelStat = 1 ;
  );

* Run the reporting code
$include %DirPath%LFreport.gms

* If model does not return optimal solution, copy files.  Needs to be done after lfreport.gms
  If(LFMM.ModelStat>1,
    put_utility 'shell' / 'copy LFshell.lst INFEAS.' RunYears.te(RunYears) '.' CYCLEINFO_CURIRUN:0:0 '.' NCNTRL_CURITR_orig:0:0 '.lst' ;
    put_utility 'shell' / 'copy LFMM_Builds.gdx LFMM_Builds.' RunYears.te(RunYears) '.' CYCLEINFO_CURIRUN:0:0 '.' NCNTRL_CURITR_orig:0:0 '.gdx' ;
    put_utility 'shell' / 'copy NEM_TO_LFMM1.gdx NEM_TO_LFMM1.' RunYears.te(RunYears) '.' CYCLEINFO_CURIRUN:0:0 '.' NCNTRL_CURITR_orig:0:0 '.gdx' ;
    put_utility 'gdxout' / 'LFMM_p' RunYears.te(RunYears) '.' CYCLEINFO_CURIRUN:0:0 '.' NCNTRL_CURITR_orig:0:0 '.gdx' ;
    Execute_Unload ;
  );

if (DebugVerbose=1 and RunYears.val>=2036,
  put_utility 'msglog' / 'GDX dump of (cycle,year,iteration): ('
    CYCLEINFO_CURIRUN:0:0 ', ' RunYears.te(RunYears) ', ' NCNTRL_CURITR_orig:0:0 ')';

  put_utility 'shell' / 'copy LFMM_Builds.gdx LFMM_Builds.' RunYears.te(RunYears) '.' CYCLEINFO_CURIRUN:0:0 '.' NCNTRL_CURITR_orig:0:0 '.gdx' ;
  put_utility 'shell' / 'copy NEM_TO_LFMM1.gdx NEM_TO_LFMM1.' RunYears.te(RunYears) '.' CYCLEINFO_CURIRUN:0:0 '.' NCNTRL_CURITR_orig:0:0 '.gdx' ;
  put_utility 'shell' / 'copy LFMM_TO_NEMS.gdx LFMM_TO_NEMS.' RunYears.te(RunYears) '.' CYCLEINFO_CURIRUN:0:0 '.' NCNTRL_CURITR_orig:0:0 '.gdx' ;
  put_utility 'gdxout' / 'LFMM_p' RunYears.te(RunYears) '.' CYCLEINFO_CURIRUN:0:0 '.' NCNTRL_CURITR_orig:0:0 '.gdx' ;
  execute_unload ;
);


* Data dump for this year
*  if(NCNTRL_FCRL=1,
  if((CreateYearlyGDXFile>0),
    put_utility 'msglog' / 'Creating yearly GDX file: LFMM_p'RunYears.te(RunYears)'.gdx';
    put_utility 'gdxout' / 'LFMM_p'RunYears.te(RunYears)'.gdx' ;
    execute_unload ;

    if (STEO_Years(RunYears) and LFadjvolON=1,
      put_utility 'shell' / 'move LFMM_p1.gdx LFMM_steo1_' RunYears.te(RunYears) '.gdx' ;
      put_utility 'shell' / 'move LFMM_p2.gdx LFMM_steo2_' RunYears.te(RunYears) '.gdx' ;
    );
  );

);  /* end_of_loop: RunYears */

*--------------------------------------------------------------------------------------
* write debug info
* http://gams.com/mccarl/mccarlhtml/index.html?
* model_solution_status_attributes__modelstat__solvestat__tmodstat__tsolstat.htm

put LFDBG "Model status: ", LFMM.tmodstat;
  if (LFMM.modelstat gt %ModelStat.Optimal%,
    put LFDBG "  Error ";
    if (LFMM.modelstat eq %ModelStat.Infeasible%,
      put LFDBG " - Number of infeasibilities: ", LFMM.numinfes;)
  )
put LFDBG /;
*--------------------------------------------------------------------------------------












