if [[ $# -ne 3 ]];then
   print "Wrong number of arguments."
   print "Please specify (1) input MPS, (2) input dictionary, and (3) output MPS."
   print "Example: gamsmps LFMMfixed.mps LFMMdict.txt LFMMfree.mps"
   exit
fi

MPS_In=$1
Dict_In=$2
MPS_Out=$3


if [ ! -f $MPS_In ]; then
    print "Error. MPS input file not found: " $MPS_In
    exit
fi

if [ ! -f $Dict_In ]; then
    print "Error. Dictionary input file not found: " $Dict_In
    exit
fi

if [[ -f $MPS_Out ]]; then 
   print "Deleting existing output file: " $MPS_Out
   rm $MPS_Out
fi


awk -v MPS_In=$MPS_In -v Dict_In=$Dict_In -v MPS_Out=$MPS_Out -f m:/default/scripts/gamsmps.awk $MPS_In 


print " "
ls -l $MPS_Out