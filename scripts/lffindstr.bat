@echo off
findstr /I "erro" lfshell.lst | findstr /I /V "if(err" | findstr /I /V /C:"parenthesis erro" | findstr /I /V "LFDBG" | findstr /I /V "maxexecerror" >> lfdebug.txt
if not exist lfshell_err.lst (
findstr /I /C:" erro" lfmm.log > nul && type lfshell.lst >> lfshell_err.lst
)
cmd /c exit 0
