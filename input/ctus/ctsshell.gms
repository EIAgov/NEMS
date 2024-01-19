$Title  Script for CTS-ECP integration
$inlinecom   { }
$Ontext

$Offtext

* Set directory path for root directory  - include back slash at the end
* Use "..\"  for current/run directory
*e.g.$set RootPath "C:\VAF\CTS\Scenario1\"
* --RootPath=".\"
$set RootPath ".\"

*Set directory path for location of gams model files - include back slash at the end
* In NEMS, the model code is in the root path
*$set SrcDir %RootPath%"ModelCode\"
*$set SrcDir  "S:\NETL\CTS_ModelCode\MasterCode\"
$set SrcDir %RootPath%

*Set directory path for location of results files
$set  Outdir %RootPath%


*********    Begin  User Settings   ****************************************************************

* For example, when running base mode for ECP, set Trunks=-1, ForceAll=1, ECPNewBuilds=-1,  etc.
Parameter  RunDefn  Settings for this run - set to -1(Off) or 1(On)
* Referenced in CTUSprep, CTUSmodel, and CTUSreprt
/  Trunks                 1

   ECP_NewBuilds          1
   ECP_Retirements        1
   ECP_Retrofits          1

   ProgramGoals           1

   ZeroLT                 1
   ZeroCarbonPrice        -1

   MatrixAnalyze         -1
/;

* Set = -1 not to force any retrofits
* Set = 1 to force on all retrofits in first model year (Trunks should also be set to 1 above!!)
* Set = 2 to force on all retrofits AND turn off saline sites that not under an OGSM node (use for NEMS topology!!)
$set ForceAll  -1

* Set = 1 to turn off selected constraints that should not apply in the NEMS-integrated runs
$set NEMSIntegrated 1

* To set solver...  set to Cplex or Xpress
$set  LPsolver               Xpress

* To send output to current local directory, set this value to 1; otherwise set it to -1
$set  CurrentDirectory        -1

* To update CTSextData.gdx, set this value to 1; otherwise set it to -1
*$set  Update_External_Data    -1

*********    End User Settings   ****************************************************************

$If %CurrentDirectory% == 1 $set OutDir .\

$gdxout %RootPath%RunDefn.gdx
$unload RunDefn
$gdxout

*Assign correct version of model files
$set GAMS_DataPrep_Name %SrcDir%CTSprep.gms
$set GAMS_Model_Name    %SrcDir%CTSmodel.gms
$set GAMS_Validate_Name %SrcDir%CTScheck.gms
$set GAMS_Reports_Name  %SrcDir%CTSreprt.gms

$set environ LO=%gams.lo% IDE=%gams.ide% workdir=%RootPath%

* about.put captures information about the versions used for this run
file about /%Outdir%about.put/;
put about;
put 'Run started on = ' system.date ' at ' system.time /

*GAMS_DataPrep_Name calaculates the model costs and bounds
$call gams %GAMS_DataPrep_Name% WorkDir=%RootPath% OptDir=%RootPath% ScrDir=%RootPath% PutDir=%RootPath% ProcDir=%RootPath% --dir=%RootPath% --ForceAllOn=%ForceAll% --InNEMS=%NEMSIntegrated%
$IF errorlevel 1 $Abort 'Error occurred during data preparation'
*put 'Data Preparation file: %GAMS_DataPrep_Name% - run completed at  ' system.time  /
$log Data Preparation completed after %system.elapsed% seconds

* GAMS_Validate_Name contains the model formulation
*$call gams %GAMS_Validate_Name% WorkDir=%RootPath% OptDir=%RootPath% ScrDir=%RootPath% PutDir=%RootPath% ProcDir=%RootPath% --dir=%RootPath%
*$IF errorlevel 1 $Abort 'Validation failed. Check CTSDataCheck.gdx'
*put 'Data Validation file: %GAMS_Validate_Name% - run completed at  ' system.time  /
*$IF errorlevel 1 put 'Validation failed. Check CTSDataCheck.gdx'
*$log Data Validation completed after %system.elapsed% seconds

* GAMS_Model_Name contains the model formulation
$call gams %GAMS_Model_Name%  s=CTStmp %environ% WorkDir=%RootPath% OptDir=%RootPath% ScrDir=%RootPath% PutDir=%RootPath% ProcDir=%RootPath% --LPsolver=%LPsolver% --dir=%RootPath% --ForceAllOn=%ForceAll% --InNEMS=%NEMSIntegrated%
$IF errorlevel 1 $Abort 'Model did not terminate with integer solution'
*$IF errorlevel 1 put 'Model did not terminate with integer solution'
*put 'CTS model file: %GAMS_Model_Name% - run completed at  ' system.time  /
$log Model Execution completed after %system.elapsed% seconds

* Report generation, sends results to CTS%Date%.gdx and CTSSoln.gdx
$call gams %GAMS_Reports_Name%  r=CTStmp  gdx=%RootPath%ctusALL %environ% WorkDir=%RootPath% OptDir=%RootPath% ScrDir=%RootPath% PutDir=%RootPath% ProcDir=%RootPath% --Outdir=%Outdir%  --LPsolver=%LPsolver% --dir=%RootPath% --ForceAllOn=%ForceAll%
$IF errorlevel 1 $goto next3
$IF errorlevel 1 $Abort 'Error occurred in report generation'
put 'CTS report file: %GAMS_Reports_Name% - run completed at  ' system.time  /
$goto next4

$label next3

* GAMS_Model_Name contains the model formulation
$call gams %GAMS_Model_Name%  s=CTStmp %environ% WorkDir=%RootPath% OptDir=%RootPath% ScrDir=%RootPath% PutDir=%RootPath% ProcDir=%RootPath% --LPsolver=%LPsolver% --dir=%RootPath% --ForceAllOn=%ForceAll% --InNEMS=%NEMSIntegrated%
$IF errorlevel 1 $Abort 'Model did not terminate with integer solution'
*$IF errorlevel 1 put 'Model did not terminate with integer solution'
*put 'CTS model file: %GAMS_Model_Name% - run completed at  ' system.time  /
$log Model Execution completed after %system.elapsed% seconds

* Report generation, sends results to CTS%Date%.gdx and CTSSoln.gdx
$call gams %GAMS_Reports_Name%  r=CTStmp  gdx=%RootPath%ctusALL %environ% WorkDir=%RootPath% OptDir=%RootPath% ScrDir=%RootPath% PutDir=%RootPath% ProcDir=%RootPath% --Outdir=%Outdir%  --LPsolver=%LPsolver% --dir=%RootPath% --ForceAllOn=%ForceAll%
$IF errorlevel 1 $Abort 'Error occurred in report generation'
put 'CTS report file: %GAMS_Reports_Name% - run completed at  ' system.time  /

$label next4

$label next2

$IF NOT errorlevel 1 $goto ok
$log *****************
$log more text
$log ******************
$abort

$label ok
