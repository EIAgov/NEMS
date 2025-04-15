pushd %~dp0

::call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2018.1.156\windows\bin\ifortvars.bat" intel64
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022

::align with the name of the scedes key NEMSPYENV in the scedes file
for /f "tokens=2 delims=," %%a in ('findstr /c:"pyver" ".\scripts\setup\input\init_configs.csv"') do set NEMSPYENV=%%a
call %NEMSPYENV%\Scripts\activate
python nems_gui.py
pause