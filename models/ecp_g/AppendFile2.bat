set myFile=%1
set addFile=composite_plus.txt
set sourceFile=./ecp/%addFile%
set targetFile=./ecp/%myFile%
cat %sourceFile% >> %targetFile%
