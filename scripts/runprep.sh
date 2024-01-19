# script file for linking and running the emm pre processors.  This
# is executed from prepett.shell, prepplt.shell, and prepdsmd.shell

# The output will be stored in $OUTDIR/$scenario/$datekey
# We find the scenario and datecode from our nems.sh name
 echo $0 | sed 's/.*nems\.sh\.//' | sed 's/\./ /g' > temp.$$
 read scenario datekey < temp.$$ ; rm temp.$$

# We need to know the "launched from" directory
 OLDDIR=$PWD

# We get $OUTDIR from a command line argument or from the runlog
 if [[ -n "$1" ]]; then
    OUTDIR=$1
 else
    grep "$scenario/$datekey" $NEMSJOBLOG/runlog | sed "s@/$scenario/$datekey@@" > tempp.$$
    read user OUTDIR < tempp.$$ ; rm tempp.$$
 fi
 if [ ! -a FILELIST.$scenario.$datekey ]; then
   echo ' NO FILELIST FILE !!!!! '
   echo ' SCRIPT ENDING '
   exit
 fi

 if [ -d $OUTDIR/$scenario ]; then
   echo directory $OUTDIR/$scenario exists
   echo continuing
 else
   echo creating $OUTDIR/$scenario
   mkdir $OUTDIR/$scenario
 fi

 cd $OUTDIR/$scenario
 if [ -d $datekey ]; then
   echo directory $datekey exists
   echo continuing
 else
   echo creating $datekey
   mkdir $datekey
 fi

 cd $datekey
 cp  $OLDDIR/keys.sed.$scenario.$datekey keys.sed
 cp  $OLDDIR/FILELIST.$scenario.$datekey FILELIST
 cp $OLDDIR/scedes.all.$scenario.$datekey scedes.all
 rm $OLDDIR/scedes.all.$scenario.$datekey

#  check to see if all files in FILELIST exist ('u' tells it to uncompress only)
 $NEMS/scripts/cac_filelist.exe FILELIST u

# file created by cac_filelist to uncompress files
 sh -K uncmprss.sh
 rm -f uncmprss.sh

# now link and submit the preprocessor upon returning to calling routine
