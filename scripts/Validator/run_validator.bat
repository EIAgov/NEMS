::This script is all that needs to be called from cycle.sh or cycle_par.sh to run the validator

:: change our working directory to that of the validator
set cur_dir=%cd%
:: the current dir is on the working dir output root (ex. T:\workdir\m2_p\d062222n) where cycle_par sits and executes this
set validator_path=%NEMS%\scripts\Validator

:: copy Validator scripts to the current directory
:: When we are using Git change this to not copy code
cd %cur_dir%\Validator
::cp -r %validator_path%\* .

::load the python environment
SET myENV=%1
call %myENV%\scripts\activate
::for debugging:
echo NEMS=%NEMS% > ValidatorNEMSPYENV.txt
echo cur_dir=%cur_dir% >> ValidatorNEMSPYENV.txt
::echo cur_dir=%cur_dir% >> ValidatorNEMSPYENV.txt
:: %cur_dir% = T:\workdir\m2_p\d062222n\Validator
::echo %myENV% >> ValidatorNEMSPYENV.txt 
echo "NEMS path below" >> ValidatorNEMSPYENV.txt
echo %NEMS% >> ValidatorNEMSPYENV.txt

::launch the validator script
python validate.py %cur_dir%
::python validate.py %cur_dir% %cur_dir%

echo %cd% >> ValidatorNEMSPYENV.txt
:: still stay in output root (ex. T:\workdir\m2_p\d062222n ). no need to move the file
move validator_*.xlsx %cur_dir%

::go back to the NEMS working directory
cd %cur_dir%

:: remove the validator source code from the NEMS directory
:: rm -rf %cur_dir%/Validator