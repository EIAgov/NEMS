# copy this to the run's output directory if ran files not created for each cycle.
# Edit this files to set up the scenario name, datecode, and the list of cycles to do in "item"
scenario="highresource_jbe"
datecode="d010215a"
item="1 2 3 4 5 6 7 8 9 10 11"


    for n in $item
    do
        echo "n=$n"
        if [ -f p3/restart.$n.unf ] ; then
           echo "Running ftab for intermediate cycle $n"
           sed "s/RESTART\.unf/p3\/RESTART\.$n\.unf/" ftab.running.dat >ftab.$n.dat
           ls -al ftab.$n.dat
           ftab < ftab.$n.dat 
           mv fort.20 fort.$n.20
           mv $scenario.${datecode:1:4}${datecode:7:2}.xml $scenario.$n.${datecode:1:4}${datecode:7:2}.xml
           mv $scenario.${datecode:1:4}${datecode:7:2}.ran $scenario.$n.${datecode:1:4}${datecode:7:2}.RAN
           ls -al fort.$n.20 $scenario.$n.${datecode:1:4}${datecode:7:2}.xml $scenario.$n.${datecode:1:4}${datecode:7:2}.RAN
       fi
    done