SET NEMSPYENV="c:\python_environments\aeo2025_py311_D"
call %NEMSPYENV%\Scripts\activate
celery -A tasks worker -n %COMPUTERNAME% -Q %COMPUTERNAME% --loglevel info --pool threads --events --without-heartbeat --concurrency 30 --logfile Y:\RabbitMQ\worker-priv-%COMPUTERNAME%.log
