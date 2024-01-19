# to create input files for arraycode_ecp.f90,
# check this line of code and change to ".true." if it is ".false.":     make_ecp_aimms /.true./
#
# Next, make run with aimmsecp=1 in the scedes file with the version of uecp.f from above..  

# after run is done there should be a bunch of files like "ecpcoeff_*.txt" where * is the year.   This script will extract 
# the aimms parameter names to ecparrays_all.txt and a list of the sets for row/column groups (ecp_rows_columns.txt) 

# (cd to the output folder)
# optional argument: last year of run:
echo "argument passed was: $1.  "
y="$1"
if [ "$y" != '' ] ; then
  echo "will use ecp_rows_columns_$y.txt"
else
  y="2050"
fi
grep -i missing ecpcoeff_*.txt | sort -u > missing.txt
cat missing.txt | sed 's/ecpcoeff_.....txt\://;s/MISSING\: no corresponding coefficient matrix for this pair//;s/\:RHS/RHS/;s/\:/coeff_/;s/ /_/g' | sort -u > ecparrays_all.txt
cp ecp_rows_columns_$y.txt ecp_rows_columns.txt

ls -l ecparrays_all.txt ecp_rows_columns.txt
echo copy ecparrays_all.txt and ecp_rows_columns.txt to the folder where arraycode_ecp.exe is located
echo then invoke arraycode_ecp.exe
