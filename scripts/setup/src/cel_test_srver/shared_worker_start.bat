SET NEMSPYENV="c:\python_environments\aeo2025_py311_D"
call %NEMSPYENV%\Scripts\activate
celery -A tasks worker -n %COMPUTERNAME%_pub -Q shared --loglevel info --pool threads --events --without-heartbeat --concurrency 10 --logfile Y:\RabbitMQ\worker-pub-%COMPUTERNAME%.log
