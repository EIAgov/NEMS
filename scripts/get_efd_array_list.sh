# to create input files for arraycode_efd.f90,
# check this line of code and change to ".true." if it is ".false.":     make_efd_aimms /.true./
#
# Next, make run with aimmsefd=1 in the scedes file with the version of uefd.f from above..  

# after run is done there should be a bunch of files like "efdcoeff_*_01.txt" where * is the year.   This script will extract 
# the aimms parameter names to efdarrays_all.txt and a list of the sets for row/column groups (efd_rows_columns.txt) 

# (cd to the output folder)

# optional argument: last year of run:
echo "$1"
y="$1"
if [[ "y" != '' ]] ; then
  echo "will use efd_rows_columns_$y.txt"
else
  y="2050"
fi

grep -i missing efdcoeff_*_01.txt | sort -u > missing.txt
cat missing.txt | sed 's/efdcoeff_...._01.txt\:MISSING\: no corresponding coefficient matrix for this pair//;s/\:RHS/RHS/;s/\:/coeff_/;s/ /_/g' | sort -u > efdarrays_all.txt
cp efd_rows_columns_$y.txt efd_rows_columns.txt

ls -l efdarrays_all.txt efd_rows_columns.txt
echo copy efdarrays_all.txt and efd_rows_columns.txt to the folder where arraycode_efd.exe is located
echo then invoke arraycode_efd.exe
