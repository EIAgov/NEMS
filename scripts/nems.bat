@echo off
rem This can be reexecuted.

if exist jcl.dat goto filelist
echo NO jcl.dat FILE !!!!!
echo ENDING NEMS run submission
goto end
:filelist
if exist FILELIST goto nohupout
echo NO FILELIST FILE !!!!!
echo ENDING NEMS run submission
goto end
:nohupout
if exist nohup.out del nohup.out

echo Running NEMS scenario:
echo ?SCEN@ ?DATE@

if exist baxpmm*.da del baxpmm*.da
if exist baspmm*.da del baspmm*.da
if exist bas2*.dat del bas2*.dat
if exist cmm1*.dat del cmm1*.dat
if exist cmm2*.dat del cmm2*.dat
if exist efd1*.dat del efd1*.dat
if exist efd2*.dat del efd2*.dat
if exist IMPCURV.txt del IMPCURV.txt
if exist RESTART.unf del RESTART.unf
if exist RESTART.rlx del RESTART.rlx
if exist baxpmm2 del baxpmm2
if exist baspmm2 del baspmm2
if exist bascmmi del bascmmi
if exist basemmi del basemmi
if exist basefdi del basefdi
if exist sprflrt del sprflrt
if exist RESTART.IN del RESTART.IN
if exist ..\..\..\input\bascmmi.dat copy ..\..\..\input\bascmmi.dat bascmmi
if exist ..\..\..\input\basemmi.dat copy ..\..\..\input\basemmi.dat basemmi
if exist ..\..\..\input\basefdi.dat copy ..\..\..\input\basefdi.dat basefdi
if exist ..\..\..\input\baspmm2.dat copy ..\..\..\input\baspmm2.dat baspmm2
if exist ..\..\..\input\baxpmm2.dat copy ..\..\..\input\baxpmm2.dat baxpmm2
if exist ..\..\..\input\sprflrt.txt copy ..\..\..\input\sprflrt.txt sprflrt
if exist ..\..\..\input\restart.unf copy ..\..\..\input\restart.unf RESTART.unf
if exist ..\..\..\input\cmm*.dat    copy ..\..\..\input\cmm*.dat
if exist ..\..\..\input\efd*.dat    copy ..\..\..\input\efd*.dat

if exist nems.exe del nems.exe
if exist udbp.exe del udbp.exe
if exist nemscycle.bat del nemscycle.bat
if exist MOREOPT.update del MOREOPT.update
if exist ftab.exe del ftab.exe
if exist ..\..\..\source\nems.exe goto copynems
echo No NEMS executable in source directory - stopping
goto end
:copynems
copy ..\..\..\source\nems.exe nems.exe
if exist ..\..\..\source\udbp.exe copy ..\..\..\source\udbp.exe udbp.exe
if not exist cycle_nems.exe copy ..\..\..\source\cycle_nems.exe cycle_nems.exe
if not exist intercv.exe copy ..\..\..\source\intercv.exe intercv.exe
if not exist ..\..\..\source\ftab.exe echo No FTAB executable in source directory - NEMS can still execute - continuing
if exist ..\..\..\source\ftab.exe copy ..\..\..\source\ftab.exe ftab.exe
if not exist cycle_nems.exe echo @echo off > nemscycle.bat
if not exist cycle_nems.exe echo echo No cycle_nems.exe - doing only one NEMS run. >> nemscycle.bat
if not exist cycle_nems.exe echo call eachcycle.bat >> nemscycle.bat
if not exist cycle_nems.exe goto afterarg

rem check for argument requesting multiple cycles
if "%1" == "" goto noarg

goto yesarg

:noarg
cycle_nems.exe
goto afterarg

:yesarg
cycle_nems.exe %1
if exist MOREOPT.update copy MOREOPT.update MOREOPT

:afterarg
call nemscycle.bat > nohup.out
if not exist ftab.exe goto end
if not exist RESTART.unf goto end
ftab < ftab.dat

if not exist udbp.exe goto end
if not exist emmdb.mdb goto end
if not exist edbpgrp.txt goto end
udbp.exe > udbp.out

:end
