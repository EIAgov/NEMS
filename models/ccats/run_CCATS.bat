:: This script is for calling the PyFiler Wrapper code to invoke NEMSPYENV and run the wrapper

::load the python environment and necessary inputs for PyFiler and HSM.
SET cycle=%1
SET curcalyr=%2
SET curitr=%3
SET myENV=%4
echo=%curcalyr%>CCATSCurCalYr.txt
echo=%curitr%>CCATSCurItr.txt
echo=%myENV%>PyFilerNEMSPYENV.txt
call %myENV%\scripts\activate

::This part runs CCATS
::CCATS.py is the code that will be called
python ccats\ccats.py -y %curcalyr% -t %curitr% > CCATSPy.log 2>CCATSPyDebug.log