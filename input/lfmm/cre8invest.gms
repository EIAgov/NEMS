*---------------------------------------------------------------------------
$ontext
To Do:


$offtext
*---------------------------------------------------------------------------


$onUNDF

* DirPath must have trailing backslash!
$if not set DirPathInvest $set DirPathInvest '.\input\parameters\param\investment\'
$if not set INPUT_FILE_XLSX $set INPUT_FILE_XLSX 'lfinvestment.xlsx'

set
  DataFile /'%DirPathInvest%%INPUT_FILE_XLSX%'/
  DateTime /'%system.DATE%_%system.TIME%'/
  GAMSFile /'%system.ifile%'/
;

$onecho > cmd.txt
Acronyms=1 par=XLSX_CapCost rng=CapCostImp!B1 Cdim=1 Rdim=1
Acronyms=1 par=XLSX_NF rng=NFImport!A1 Cdim=1 Rdim=1
Acronyms=1 par=XLSX_AFGrowthRates rng=AFGrowthRates!A1 Cdim=1 Rdim=1
Acronyms=1 par=XLSX_AFBldSteps rng=AFBldSteps!A1 Cdim=1 Rdim=2
*Acronyms=1 par=XLSX_StateTax rng=StateTax!B3 Cdim=1 Rdim=1
Acronyms=1 par=XLSX_StateTax rng=StateTax!A1 Cdim=1 Rdim=1
Acronyms=1 par=XLSX_FedTax rng=FedTax!A1 Cdim=1 Rdim=1
Acronyms=1 par=XLSX_Learning rng=Learning!A1 Cdim=1 Rdim=3
Acronyms=1 par=XLSX_Learning2 rng=Learning2!A1 Cdim=1 Rdim=1
Acronyms=1 par=XLSX_RegionalData rng=RegionalData!A1 Cdim=1 Rdim=1
Acronyms=1 par=XLSX_InvestmentFactors rng=InvestmentFactors!B3 Cdim=1 Rdim=1
*Acronyms=1 par=XLSX_ProcessRisk rng=ProcessRisk!A1 Cdim=1 Rdim=1
$offecho

$call GDXXRW %DirPathInvest%%INPUT_FILE_XLSX% o=invest_XLSX.gdx @cmd.txt


alias(*,Process);
alias(*,Step);
alias(*,t);
alias(*,RefReg);

set
  INVEST_SET 'sets used only by investment code'
    /BldRiskClass, BldStep, CapExpYr, InvParam, LrnPhase, LrnSpeed, LrnParam, NFBaseYr, ProcessRisk/

  BldRiskClass /AvgRiskPet, HighRiskPet, NonPet/

InvParam /
  PCTENV     'HomeOffice and Contractor fees'
  PCTCNTG    'Contractor and Owners Contingency'
  PCTLND     'Land'
  PCTSPECL   'Prepaid Royalties License and Start-up costs'
  PCTWC      'Working Capital'
  STAFF_FAC  'Supervisory and other Staffing'
  OH_FAC     'Benefits and other overhead'
  INS_FAC    'Yearly Insurance fraction of P and E'
  TAX_FAC    'Local Tax Rate fraction of P and E'
  MAINT_FAC  'Yearly Maintenance fraction of P and E'
  OTH_FAC    'Yearly Supplies OH Env fraction of P and E'
  OSBLFAC    'Ratio OSBL to ISBL'
  BLDYRS     'Construction Period in years'
  PRJLIFE    'Project Life in years'
  PRJINFL    'NOT USED Inflation Rate During Construction'
  PRJSDECOM  'Salvage Value Less Decommission (MM $)'
  BEQ_OPR    'equity beta opr decision'
  EMRP_OPR   'EMRP opr decision'
  EQUITY_OPR 'NOT USED - equity opr decision'
  BEQ_BLD    'equity beta  build decision'
  EMRP_BLD   'NOT USED - EMRP build decision'
  EQUITY_BLD 'equity build decision'

  NFBaseYr   'Nelson-Farr base year'
/

  LrnPhase /Phase2, Phase3/
  LrnSpeed /Fast, Slow/
  LrnParam /A, B, F, K, M/

  NFBaseYr(t) /1993/
  CapExpYr(t) /2008/

  BldStep(Step) /STEP01, STEO02, STEP03/

* Set ProcessRisk is read from the spreadsheet file
  ProcessRisk(Process,BldRiskClass)
;

parameter
  XLSX_CapCost(Process,*)
  CapExpISBL(Process)
  CapExpLabor(Process)
  CapExpSize(Process)

  XLSX_NF(t,*)
  NFInvest(t)
  NFLabor(t)
  NFOper(t)

  XLSX_AFGrowthRates(Process,*)
  AFInitPlants(Process)
  AFGrowthRate(Process)

  XLSX_AFBldSteps(Process,Step, *)
  AFCapMult(Process,Step)
  AFCostMult(Process,Step)

  XLSX_StateTax(t,RefReg)
  StateTax(t,RefReg)

  XLSX_FedTax(t,*)
  FedTax(t)

  XLSX_Learning(Process,LrnPhase,LrnSpeed,LrnParam)
  LearningData(Process,LrnPhase,LrnSpeed,LrnParam)

  XLSX_Learning2(Process,*)
  MoreLearningData(Process,*)

  XLSX_RegionalData(RefReg,*)
  InvLoc(RefReg)
  LaborLoc(RefReg)

  XLSX_InvestmentFactors(InvParam,BldRiskClass)
  InvFactors(InvParam,BldRiskClass)


  XLSX_ProcessRisk(Process,BldRiskClass)
;

$gdxin invest_XLSX.gdx
$loaddc XLSX_CapCost
$loaddc XLSX_AFGrowthRates
$loaddc XLSX_AFBldSteps
$loaddc XLSX_StateTax
$loaddc XLSX_FedTax
$loaddc XLSX_NF
$loaddc XLSX_Learning
$loaddc XLSX_Learning2
$loaddc XLSX_RegionalData
$loaddc XLSX_InvestmentFactors
*$loaddc XLSX_ProcessRisk
$gdxin


* simplify using macro ???
CapExpISBL(Process) = XLSX_CapCost(Process,'ISBL');
CapExpLabor(Process) = XLSX_CapCost(Process,'Labor');
CapExpSize(Process) = XLSX_CapCost(Process,'Capacity');

*ProcessRisk(Process,BldRiskClass) = XLSX_ProcessRisk(Process,BldRiskClass);

NFInvest(t) = XLSX_NF(t,'NF_Invest');
NFLabor(t) = XLSX_NF(t,'NF_Labor');
NFOper(t) = XLSX_NF(t,'NF_Oper');

AFInitPlants(Process) = XLSX_AFGrowthRates(Process,'InitPlants');
AFGrowthRate(Process) = XLSX_AFGrowthRates(Process,'BuildMult');

AFCapMult(Process,Step)  = XLSX_AFBldSteps(Process,Step,'CapMult');
AFCostMult(Process,Step) = XLSX_AFBldSteps(Process,Step,'CostMult');

StateTax(t,RefReg) = XLSX_StateTax(t,RefReg);
FedTax(t) = XLSX_FedTax(t,'FedTax');

LearningData(Process,LrnPhase,LrnSpeed,LrnParam) = XLSX_Learning(Process,LrnPhase,LrnSpeed,LrnParam);

LearningData(Process,LrnPhase,LrnSpeed,'A') =
  ROUND(LearningData(Process,LrnPhase,LrnSpeed,'A'),3);

MoreLearningData(Process,'Optimism') = XLSX_Learning2(Process,'Optimism');
MoreLearningData(Process,'Contingency') = XLSX_Learning2(Process,'Contingency');


InvLoc(RefReg) = XLSX_RegionalData(RefReg,'InvLoc');
LaborLoc(RefReg) = XLSX_RegionalData(RefReg,'LaborLoc');

InvFactors(InvParam,BldRiskClass) = XLSX_InvestmentFactors(InvParam,BldRiskClass);
InvFactors('NFBaseYr',BldRiskClass) = 0;

*----------------------------------------------------------------------------------------
* to make consistent with previous GDX data ---
LearningData(Process,LrnPhase,LrnSpeed,LrnParam) =
  round(LearningData(Process,LrnPhase,LrnSpeed,LrnParam),4);
CapExpSize(Process) = round(CapExpSize(Process),3);
CapExpISBL(Process) = round(CapExpISBL(Process));
*----------------------------------------------------------------------------------------

* include XLSX filename, system date/time in GDX ?
execute_unload 'lfinvest.gdx', DataFile, GAMSFile, DateTime
  INVEST_SET, BldRiskClass, BldStep, InvParam, LrnPhase, LrnSpeed, LrnParam,
*ProcessRisk
  CapExpISBL, CapExpLabor, CapExpSize
  CapExpYr, NFBaseYr, NFInvest, NFLabor, NFOper
  AFInitPlants, AFGrowthRate
  AFCapMult, AFCostMult
  StateTax, FedTax
  LearningData, MoreLearningData
  InvLoc, LaborLoc
  InvFactors
;


$call gdxdump lfinvest.gdx output=lfinvest.inc


