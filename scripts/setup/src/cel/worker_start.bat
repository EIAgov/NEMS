SET NEMSPYENV="c:\python_environments\aeo2025_py311_D"
call %NEMSPYENV%\Scripts\activate
celery -A tasks worker -Q shared,%COMPUTERNAME% --loglevel info --pool threads --events --without-heartbeat --concurrency 20 --logfile Y:\RabbitMQ\worker-%COMPUTERNAME%.log
