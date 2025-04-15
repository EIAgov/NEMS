pushd %~dp0
SET myENV=aeo2024_py311
call O:\python_environments\%myENV%\scripts\activate
python run_ecp_parallel.py
