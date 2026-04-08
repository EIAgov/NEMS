for /f "tokens=2 delims=," %%a in ('findstr /c "pyver" "%~dp0\..\..\..\input\init_configs.csv"') do set NEMSPYENV=%%a
call %NEMSPYENV%\Scripts\activate.bat
echo %COMPUTERNAME% | findstr /i "TSTNEM" >nul && SET MYDIR=Z:\RabbitMQ\||SET MYDIR=Y:\RabbitMQ\
python nemseventmonitor.py -o %MYDIR% -d %MYDIR%nems_runs.db
cmd /k