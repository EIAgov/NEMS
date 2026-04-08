for /f "tokens=2 delims=," %%a in ('findstr /c "pyver" "%~dp0\..\..\input\init_configs.csv"') do set NEMSPYENV=%%a
call %NEMSPYENV%\Scripts\activate
celery -A tasks worker -n %COMPUTERNAME% -Q %COMPUTERNAME% --loglevel info --pool threads --events --without-heartbeat --concurrency 30 --logfile Y:\RabbitMQ\worker-priv-%COMPUTERNAME%.log
if '%ERRORLEVEL%' == '2' (
    echo -- ERROR --
    echo Worker did not start correctly! If you see an error like:
    echo [91mUnable to load celery application.[0m
    echo [91mThe module tasks was not found.[0m
    echo Make sure you are in the correct working directory. When executing the scripts.
    echo Expected directory is %cd% should be %~dp0.
)