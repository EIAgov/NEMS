 # --- Get full restart file name using runlog
TK_ECHO_USE_BASH_BEHAVIOR=1
 SCEN=$1
 DATEKEY=$2
 if [ -z "$SCEN" ] ; then
   echo -n " Enter scenario of run: ";read SCEN
   echo -n " Enter datekey of run: ";read DATEKEY
 fi
 if [ -z "$DATEKEY" ] ; then
   echo -n " Enter datekey of run: ";read DATEKEY
 fi

# echo $SCEN $DATEKEY
 grep  "\/$SCEN\/$DATEKEY" $NEMSJOBLOG/runlog | sed 's!^... !!' > temp.$$
 read RESTART dummy < temp.$$ ; rm temp.$$
 echo $RESTART | sed "s!/$SCEN!!" | sed "s!/$DATEKEY!!" > temp.$$
 read RUNDIR < temp.$$ ; rm temp.$$
 echo "$RESTART/RESTART.unf" > temp.$$ ; read RESTART < temp.$$ ; rm temp.$$
 # --- check existence, access, and file type of restart file
 #echo $RESTART
 sh $NEMS/scripts/checkfor.z.sh $RESTART
 if [ -z "$RUNDIR" ] ; then
   echo "$1 $2 not found in the run log"
 else
   if [ -f "$RESTART" ] ; then
     found=1
     echo $RESTART
   else
     echo "$RESTART : Restart file not found in $RUNDIR"
   fi
 fi
