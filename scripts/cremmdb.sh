# /emm/scripts/cremmb.sh
# This macro will create an emmdb.mdb database file from the specified EDBPGRP.txt NEMS output file.                   
#
#
trap "exit" 1 2 15

# Start of script - echo a heading
echo " "
echo " "
echo "**********************************************"
echo "***                                        ***"
echo "***  EMMDB database creation script        ***"
echo "***                                        ***"
echo "***   This script will create an emmdb.mdb ***"
echo "***   database file for a specified NEMS   ***"
echo "***   run and cycle. The emmdb.mdb is      ***"
echo "***   created from the NEMS output file:   ***"
echo "***   EDBPGRP.txt                          ***"
echo "**********************************************"
echo " "
echo "*** You must be in the output run directory when running the script ***"
echo " "
      # --- run script in run directory 
      echo " " 
      echo " *** Uncompressing and copying emmdb.mdb to emmdb.finalcycle.mdb ***"
      echo " " 
      gzip -d emmdb.mdb
      cp emmdb.mdb EMMDB.FINALCYCLE.MDB
      echo " "

   if ls edbpgrp* 1> /dev/null 2>&1; then
       echo " " 
       echo " *** Uncompressing edbpgrp.txt files *** "
       echo " " 
       gzip -d "edbpgrp"*
       echo " " 
       echo "**********************************************"
       echo "***   The following EDBPGRP files are      ***"
       echo "***     availble for emmdb database        ***"
       echo "***     creation from this run.            ***"
       echo "**********************************************"
       echo " "
       ls -l edbpgrp.*.txt
   else
       echo " "
       echo " EDBPGRP.txt file not found in this directory. "
       echo " Check that you are in the correct run output directory (p2 if parallel)"
       echo " "
       exit   
   fi
      echo " "
      echo " "
      accept=0
      while [[ $accept = 0 ]]
      do
        echo -n "Please enter NEMS run cycle for emmdb creation   : "
        read CYCLE  
        if [[ -z "$CYCLE" ]]; then
          echo '$0: *** CYCLE HAS NO DEFAULT VALUE - RE-ENTER ***'
          echo ' '
        else
          accept=1
        fi
      done
      EDBP="EDBPGRP.${CYCLE}.txt"
      OUTFIL=${CYCLE}                
    
 
echo " "
echo "***  Copying necessary files -- may take a few minutes ***"
echo " "

cp $EDBP edbpgrp.txt
emmdb="$NEMS/input/emm/emmdb.mdb"
# echo " emmdb ${emmdb}"
cp $emmdb emmdb.mdb
udbpe="$NEMS/source/udbp.exe"
# echo " udbpe ${udbpe}"
cp $udbpe udbp.exe  

echo " "
echo "***  Running database creation progam  ***"
echo "***    This can take up to 20 minutes  ***"
echo " "

 udbp.exe > udbp.$OUTFIL.out

mv emmdb.mdb EMMDB.$OUTFIL.MDB
rm edbpgrp.txt
mv emmdb.finalcycle.mdb EMMDB.MDB


  echo " "
  echo "  Your emmdb database file has been created and is stored in -- emmdb.$OUTFIL.mdb -- "
  echo "  Writes to unit 6 are stored in -- udbp.$OUTFIL.out -- "
  echo " "

