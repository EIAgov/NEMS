function isanum {
TK_ECHO_USE_BASH_BEHAVIOR=1
  var=$1
  var2=${1%%[a-z,A-Z]*} 
  if [ "$var" != "$var2" ] ; then
#    echo -n "$1 contains letters: Hit enter to continue";read ans
    return 0
  else
#    echo -n "all numbers";read ans         
    return 1 
  fi
}
#**********
#  delrun
#**********
cd
user=$USER
echo $USER
nruns=10
if [ -n "$1" ] ; then
  nruns=$1
fi
if [ -n "$2" -a $USER="dsa" ] ; then
  user=$2
fi
trap "rm -f templog.$$ readinp hmm hmm2 delrun.menu ;cd -; exit" 0 1 2 
choice="get_run"
#clear
echo "delrun:  This will delete the output files from a run"
echo "         and remove the output datekey directory."
echo "  " 
echo "      Only displays runs from the log if the output directory exists."
echo "      Can display more than 10 runs using command line argument, eg: 'delrun n', "
echo "         where n is the number of runs to display"
echo "      Holds the menu numbers constant.  Re-run DELRUN to freshen the menu."
echo " "
grep -i "^$USER " $NEMSJOBLOG/runlog | grep -iv " Deleted" | awk '{print tolower($0)}' | sed 's@\/\/nem2\/l\/@l:\/@' > hmm2
wc -l hmm2 > hmm; read nmax dum < hmm 
nfirst=$((nmax-nruns))
awk '{n=n+1;print "if [[ -e " $2 " && ", n," -gt $nfirst ]] ; then echo ",n,$1, $2," ; fi" } ' hmm2 > delrun.menu 

while [ "$choice" != "quit" ] 
do
   case "$choice" in
      (get_run)
         . delrun.menu > hmm
         . delrun.menu | sed 's/^/     /'
         echo "     0  Quit" 
         number_of_scenarios=`wc -l < hmm`
         tail -1 hmm > hmm2
         echo " "
         if [ $number_of_scenarios -eq 0 ] ; then
            echo "         You have no runs to delete."
            break
         fi
         echo -n "Enter a number from the menu or a scenario: [0] > ";read SCENARIO 
            if [ -z "$SCENARIO" ] ;then
               SCENARIO=0
               echo "scenario=$SCENARIO"
            fi
            isanum $SCENARIO 
            isanumb=$?
            if [ "$isanumb" -eq 1 ] ; then
               if [[ "$SCENARIO" -ge 1 ]] ; then

                  grep "^$SCENARIO " hmm | awk '{print $2 $3}' > hmm2
                  i=1
                  count=`sed "s/\\// /g" hmm2 | wc -w`
                  count1=`expr $count - 1`
                  for each in `sed "s/\\// /g" hmm2`
                  do
                     if [ $i -eq $count ] ; then
                        DATEKEY=$each
                     elif [ $i -eq $count1 ] ;then
                        SCENARIO=$each
                        isanumb=0   
                     fi
                     i=`expr $i + 1`
                  done
                  echo " Scenario=$SCENARIO Datecode=$DATEKEY"
               else
                  choice="quit"
                  echo "choice=$choice"
               fi
            else
               grep "\/$SCENARIO\/" hmm | tail -1 > hmm2
               i=1
               count=`sed "s/\\// /g" hmm2 | wc -w`
               for each in `sed "s/\\// /g" hmm2`
               do
                  if [ $i -eq $count ] ;then
                     Ddefault=$each
                  fi
                  i=`expr $i + 1`
               done
               echo -n "Enter the Datecode [$Ddefault] > ";read DATEKEY
               if [ -z "$DATEKEY" ] ;then
                  DATEKEY=$Ddefault
               fi
            fi 

         if [[ "$SCENARIO" > "$number_of_scenarios" ]]; then
           if [ $isanumb -eq 1 ] ;then
               if [[ $SCENARIO -ge 1 && $SCENARIO -le $number_of_scenarios ]] ; then
                 choice="get_run"
               fi
           else
               grep -n " " $NEMSJOBLOG/runlog | sed "s/:/ /" > templog.$$
               grep "\/$SCENARIO\/$DATEKEY" templog.$$ | grep $user > readinp
               choice="check_path"
           fi 
         elif [ "$choice" != "quit" ] ; then
            grep -n " " $NEMSJOBLOG/runlog | sed "s/: / /" > templog.$$
            grep "/$SCENARIO/$DATEKEY" templog.$$ | grep "$user" > readinp
            choice="check_path"
         fi;;

      (check_path)
         head -1 readinp > temp.dat ; read dummy dummy TOP dummy < temp.dat;rm temp.dat
         tail -1 readinp > temp.dat ; read LINE_NUMBER2 dummy BOTTOM dummy < temp.dat;rm temp.dat
         #echo "TOP=$TOP, BOTTOM=$BOTTOM"
         if [ "$TOP" ] ;then
            if [ "$TOP" != "$BOTTOM" ] ;then
               unique_scenario_datekey=not_unique
            else 
               unique_scenario_datekey=unique
            fi
         else
            unique_scenario_datekey=none
         fi

         case "$unique_scenario_datekey" in 
            (none)
               echo " "
               echo "This scenario/datekey combination was not found in the runlog"
               choice="clean";;
            (not_unique)
               echo " "
               echo "This is a duplicate scenario/datekey combination."
               cat readinp
               echo " "
               echo "Operation Aborted!!!"
               choice="clean";;
            (unique) 
               choice="set_path"
         esac;;

      (set_path)
         read LINE_NUMBER USERID PathP DELCODE < readinp
echo $PathP | awk '{print tolower($0)}' | sed 's@\/\/nems-f6\/l\/@l:\/@;s@\/\/nems-f5\/m\/@m:\/@;s@\/\/nems-f5\/n\/@n:\/@' > path2
         read PathP < path2;rm path2
         if [ "$DELCODE" = "Deleted" ] ;then
            choice="previously_deleted"
         else
            choice="check_existence"
         fi;;

      (previously_deleted)
         echo " " 
         echo "This run is already listed as deleted."
         choice="clean";;

      (check_existence)
         if [ -d "$PathP" ] ;then 
            choice="delete_run"
         else
            choice="modify_runlog"
         fi;; 

      (delete_run)
         $NEMS/scripts/freshness "$PathP\/\*"
         ifresh=$?
         if [ $ifresh -eq 0 ] ; then
            
           echo 'Files for that run have been modified'
           echo 'recently or the run may still be executing.  To '
           echo 'avoid potential system problems, please wait' 
           echo 'for 30 minutes before removing that directory.'
           choice="get_run"
         else
           echo 
           DEF='y'
           echo "Are you sure you want to delete  "
           echo -n "$PathP? y/n [$DEF] > ";read RESPONSE
           if [ -z "$RESPONSE" ] ; then
             RESPONSE="$DEF"
           fi
           case "$RESPONSE" in
            (n|N) choice="clean"
               echo " "
               echo "Deletion Aborted!!!";;
            (y|Y) rm -rf $PathP 
               echo
               echo Deletion completed
               choice="modify_runlog";;
            (*)echo
               echo Incorrect response
               choice="delete_run";;
           esac
         fi
         ;;
      (modify_runlog)
         sed "$LINE_NUMBER s/$/ Deleted/" $NEMSJOBLOG/runlog > templog.$$
         echo Runlog updated
         cp templog.$$ $NEMSJOBLOG/runlog
         choice="clean";;

      (clean)
         rm templog.$$
         rm readinp
         rm hmm 
         choice="get_run";;

      (quit)
         DEF='y'
         echo -n "Continue? y|n [$DEF] > ";read RESPONSE
         echo
         if [ -z "$RESPONSE" ] ; then
            RESPONSE="$DEF"
         fi
         case "$RESPONSE" in
            (n|N|no|NO|No) break;; 
            (y|Y|yes|YES|Yes) choice="get_run";; 
            (*)  echo Incorrect response
               choice="quit";;
         esac;;

   esac        

done
cd - 
echo "delrun completed "
