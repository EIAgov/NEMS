@echo off
rem This can be reexecuted.

if exist p1\jcl.dat goto filelist1
echo NO jcl.dat FILE in p1 !!!!!
echo ENDING NEMS run submission
goto end
:filelist1
if exist p1\FILELIST goto nohupout1
echo NO FILELIST FILE in p1 !!!!!
echo ENDING NEMS run submission
goto end
:nohupout1
if exist p1\nohup.out del p1\nohup.out
if exist p2\jcl.dat goto filelist2
echo NO jcl.dat FILE in p2 !!!!!
echo ENDING NEMS run submission
goto end
:filelist2
if exist p2\FILELIST goto nohupout2
echo NO FILELIST FILE in p2 !!!!!
echo ENDING NEMS run submission
goto end
:nohupout2
if exist p2\nohup.out del p2\nohup.out
if exist p3\jcl.dat goto filelist3
echo NO jcl.dat FILE in p3 !!!!!
echo ENDING NEMS run submission
goto end
:filelist3
if exist p3\FILELIST goto nohupout3
echo NO FILELIST FILE in p3 !!!!!
echo ENDING NEMS run submission
goto end
:nohupout3
if exist p3\nohup.out del p3\nohup.out
if exist nohup.out del nohup.out
if exist tfhup.out del tfhup.out

echo Running NEMS scenario:
echo ?SCEN@ ?DATE@

if exist p2\bas2*.dat del p2\bas2*.dat
if exist p2\cmm2*.dat del p2\cmm2*.dat
if exist p2\efd2*.dat del p2\efd2*.dat
if exist RESTART.unf del RESTART.unf
if exist RESTART.rlx del RESTART.rlx
if exist bascmmi del bascmmi
if exist basemmi del basemmi
if exist basefdi del basefdi
if exist RESTART.IN del RESTART.IN
if exist ..\..\..\input\bascmmi.dat copy ..\..\..\input\bascmmi.dat p2\bascmmi
if exist ..\..\..\input\basemmi.dat copy ..\..\..\input\basemmi.dat p2\basemmi
if exist ..\..\..\input\basefdi.dat copy ..\..\..\input\basefdi.dat p2\basefdi
if exist ..\..\..\input\restart.unf copy ..\..\..\input\restart.unf RESTART.unf

if exist tfiler.exe del tfiler.exe
if exist ftab.exe del ftab.exe
if exist p2\udbp.exe del p2\udbp.exe
if exist intercv.exe del intercv.exe
if exist nemscycle.bat del nemscycle.bat
if exist MOREOPT.update del MOREOPT.update
if exist ..\..\..\source\nems1.exe goto copy1ok
echo No NEMS1 executable in source directory - stopping
goto end
:copy1ok
if exist ..\..\..\source\nems2.exe goto copy2ok
echo No NEMS2 executable in source directory - stopping
goto end
:copy2ok
if exist ..\..\..\source\nems3.exe goto copy3ok
echo No NEMS3 executable in source directory - stopping
goto end
:copy3ok
if exist ..\..\..\source\tfiler.exe goto copynems
echo No tfiler executable in source directory - stopping
goto end
:copynems
copy ..\..\..\source\nems1.exe p1\nems1.exe
copy ..\..\..\source\nems2.exe p2\nems2.exe
if exist ..\..\..\source\udbp.exe copy ..\..\..\source\udbp.exe p2\udbp.exe
copy ..\..\..\source\nems3.exe p3\nems3.exe
copy RESTART.unf p1\input\restart.unf
copy RESTART.unf p2\input\restart.unf
copy RESTART.unf p3\input\restart.unf
copy ..\..\..\source\tfiler.exe tfiler.exe
if exist ..\..\..\source\cycle_nems.exe copy ..\..\..\source\cycle_nems.exe cycle_nems.exe
if exist ..\..\..\source\oscillate.exe copy ..\..\..\source\oscillate.exe oscillate.exe
if exist ..\..\..\source\intercv.exe    copy ..\..\..\source\intercv.exe intercv.exe
if not exist ..\..\..\source\ftab.exe echo No FTAB executable in source directory but NEMS can still execute - continuing
if exist ..\..\..\source\ftab.exe copy ..\..\..\source\ftab.exe ftab.exe
if not exist cycle_nems.exe echo @echo off > nemscycle.bat
if not exist cycle_nems.exe echo echo No cycle_nems.exe - doing only one NEMS run. >> nemscycle.bat
if not exist cycle_nems.exe type eachcycle.bat >> nemscycle.bat
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

rem need to run p1 one extra time
if exist RESTART.unf  copy RESTART.unf  p1\input\restart.unf
cd p1
echo NEMS Run encore execution started.    >> nohup.out
nems1.exe >> nohup.out
echo NEMS run encore execution completed.  >> nohup.out

cd ..\
copy p1\RESTART.unf RESTART.unf

intercv.exe ?MINSCORE@ < intercvfiles.txt > intercvout.txt
if not exist ftab.exe goto skipftab
if not exist RESTART.unf goto skipftab
ftab < ftab.dat
if exist oscillate.exe oscillate.exe >> nohup.out

:skipftab

rem execute EMM program udbp to convert text file into access database table
cd p2
if not exist udbp.exe goto end
if not exist emmdb.mdb goto end
if not exist edbpgrp.txt goto end
udbp.exe > udbp.out

:end
