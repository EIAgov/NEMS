:: This script is for calling the __init__.py for PyFiler invoke NEMSPYENV and run the __init__.py file

::load the python environment and necessary inputs for PyFiler and HSM.
SET myENV=%1
echo=%myENV%>PyFilerNEMSPYENV.txt
::call \\nem5\O\python_environments\%myENV%\scripts\activate
call %myENV%\scripts\activate

::launch the __init__.py code
IF exist "PyFiler\" (python PyFiler\__init__.py >PyFilerInit.log 2>PyFilerInitDebug.log) ELSE (python ..\PyFiler\__init__.py >PyFilerInit.log 2>PyFilerInitDebug.log)