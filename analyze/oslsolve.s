# THIS IS SCRIPT FILE TO RUN OSLSOLVE WITH MATRIX FILE = $1.MAT
rm fort.10
rm fort.11
rm fort.15
ln $1.MAT fort.10
oslsolve
mv fort.11 $1.OSL
ls -ls $1.OSL
