set myFile=%1
set sourceFile=./ecp/%myFile%
set targetFile1=./ecp/ecp_ge/%myFile%
set targetFile2=./ecp/ecp_gsw/%myFile%
cp -f %sourceFile%  %targetFile1%
cp -f %sourceFile%  %targetFile2%
rm %sourceFile%
