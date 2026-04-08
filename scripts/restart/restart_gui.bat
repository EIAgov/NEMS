::align with the name of the scedes key NEMSPYENV in the scedes file
for /f "tokens=2 delims=," %%a in ('findstr /c:"pyver" "..\setup\input\init_configs.csv"') do set NEMSPYENV=%%a
call %NEMSPYENV%\Scripts\activate.bat
start pythonw compare_gui.py