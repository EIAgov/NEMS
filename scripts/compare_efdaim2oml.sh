echo 'example ./compare_aimms2oml.sh S:/output/lc2/tstams_efdtst/d090820c efd 2011 01'
echo 'comparing aimms efd to oml efd'
echo ''
echo 'output folder 	: ' $1
echo 'year 		: ' $3
echo 'iteration 	: ',$4

output=
while [ -z $output ]
do
    echo -n 'output folder : (S:/output/lc2/tstams_efdtst/d090820c)? '
    read output
done

modeltype='efd'

yearused=
while [ -z $yearused ]
do
    echo -n 'Year ?:'
    read yearused
done

iteration=
while [ -z $iteration ]
do
    echo -n 'iteration ? (two digits e.g. 01 for 1, usually the final one):'
    read iteration
done

cd $output
echo 'current path	:' $PWD
cd $modeltype
echo 'current path	:' $PWD

# look for cpx eg cpx_2013
lookfor="cpx_"$yearused"_"$iteration
echo "uncompressing " $lookfor
var=$(find . -name "*$lookfor*")

echo 'uncompressing 	:' $var
uncompress $var

cd log
echo 'current path	:' $PWD
lookfor="efd_"$yearused"_"$iteration
echo "uncompressing " $lookfor
var=$(find . -name "*$lookfor*")
uncompress $var

function cap {
    typeset -u f
    f=${1:0:3}
    printf "%s%s\n" "$f"
}

cp ../../input/aimefd.xlsx .
$NEMS/scripts/readaimlis.exe $yearused $modeltype $iteration
cd ../..
ls -l *sorte*
sort -ad aimmat_unsorted_$yearused.txt > aimmat.txt
echo 'current path	:' $PWD
upper=$(cap $modeltype)
$NEMS/scripts/makemps.sh $upper $yearused
grep '^ N  ' efd_$yearused.mps > freerows.txt

echo "creating rhs file ..."
grep -i 'RHSEFD ' "efd_"$yearused".mps" > rhs_mps.txt
awk -F" " '{print "ED : '"$yearused"' :", $1,"          :", $2, "        : ", $3}' rhs_mps.txt > rhs_temp.txt
sed 's/ : /:/g' rhs_temp.txt > rhs_final.txt

echo "creating bound file ..."
grep -i 'BOUND ' "efd_"$yearused".mps" > bound_mps.txt
awk -F" " '{printf "%11s %-8s %9s %2s %16s %f\n", "ED : '"$yearused"' :", $3,"        :", $1, "              : ", $4}' bound_mps.txt > bound_temp.txt
sed 's/ : /:/g' bound_temp.txt > bound_final.txt
                                                                                                                                
echo "now appending rhs and bound files to mat file ..."
cat "ED_"$yearused"_MAT.TXT" rhs_final.txt bound_final.txt > "ED_"$yearused"_matPlusRHSPlusBound.txt" 

echo "now sorting ..."
sort -ad -t":" -k 3 -k 4 "ED_"$yearused"_matPlusRHSPlusBound.txt" > omlmat.txt
$NEMS/scripts/readmat.exe
echo '********* matout.txt is now saved as matout_'"$upper"'_'"$yearused"'_'"$iteration"'.txt ********'
mv matout.txt matout_"$upper"_"$yearused"_"$iteration".txt
echo 'done'
