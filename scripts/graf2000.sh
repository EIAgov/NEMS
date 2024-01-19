echo " "
echo " "
echo " "
#echo     "               Select GRAF2000 version"
#echo " "
#echo     "               1) Old -- NEMS 2020    "
#echo     "               2) New -- NEMS 2025    "
#echo " "
#read ans?"               Enter 1 or 2 [1] : " 
#case $ans in
#  2) prg=ladybeta.exe;;
#  *) prg=graf2000.exe;;
#esac
prg=graf2000_1017.exe
cd m:/graf2000
./$prg
cd -
