# /emm/scripts/geodbug.sh 
# This macro will create the files necessary to create the excel geo dbug file
#
 rm geo_input_data.txt
 rm geo_curve_data.txt
 rm geo_builds.txt
 rm geo_temp_data.txt
 rm geo_curve_info.txt
 rm geo_final_curves.txt
#
#
trap "exit" 1 2 15
function read_access {
#   function read_access checks to see if:
#     1. file exist
#     2. user has read access to file
#     3. file is not a directory or some other special file type
#  function returns 0 if all test passed and returns -1 on error
#  
# --- does file exist
 echo $1
  $NEMS/scripts/checkfor.z.sh $1
  if [[ ! -a $1 ]]; then
    echo "$0: file $1 not found"
    return -1
  fi
# --- does user have read access
  if [[ ! -r $1 ]]; then   
    echo "$0: do not have read permission for $1"
    return -1
  fi  
# --- file is not directory or other special file type
  if [[ ! -f $1 ]]; then   
    echo "$0: file $1 is either a directory or other special type"
    return -1
  fi  
  return 0
  }
# Start of script - Print a heading
echo " "
echo " "
echo "**********************************************"
echo "***                                        ***"
echo "***  Geothermal Debug Script               ***"
echo "***                                        ***"
echo "***   This script will create the input    ***"
echo "***   files to the geothermal debug        ***"
echo "***   spreadsheet file. The data is        ***"
echo "***   obtained from the WDUMP.txt file.    ***"
echo "**********************************************"
echo " "
echo "Press return to get default answers shown in brackets <>"
echo ""
acceptable=0
while [[ $acceptable = 0 ]]
do
  echo "Choose WDUMP option : "
  echo "  1.  Locate WDUMP file using NEMS scenario "
  echo "  2.  User specifies full name of WDUMP file   "
  echo ""
  echo -n "Please enter WDUMP option < 1 > : "
  read EOPT 
  case $EOPT in
       [1]    ) emmopt=1
                 acceptable=1;;
       [2]    ) emmopt=2
                 acceptable=1;;         
       *      ) if [[ -z "$EOPT" ]]; then
                   let emmopt=1
                   acceptable=1
                 else
                   echo " *** UNACCEPTABLE RESPONSE - RE-ENTER ***"
                 fi;;
  esac     
done
echo " "
  found=0
  while [[ $found != 1 ]]
  do
    if [[ $emmopt = 1 ]]; then
    echo -n "Please enter NEMS run scenario <aeo2001> : "
    read SCEN
    if [[ -z "$SCEN" ]]; then
      SCEN='aeo2001'
    fi
    accept=0
    while [[ $accept = 0 ]]
    do
      echo -n "Please enter NEMS run datekey  : "
      read DATEKEY
      if [[ -z "$DATEKEY" ]]; then
        echo '$0: *** DATEKEY HAS NO DEFAULT VALUE - RE-ENTER ***'
        echo ' '
      else
        accept=1
      fi
    done      
      # --- Get full scenario name using runlog
    grep  \/${SCEN}\/${DATEKEY} $NEMSJOBLOG/runlog | sed "s!^.* !!" > temp.$$
    read RUNDIR < temp.$$ ; rm temp.$$
    echo "${RUNDIR}/p2/RFM_WDUMP.txt" > temp.$$ ; read WDUMP < temp.$$ ; rm temp.$$
    fi
      # --- Get WDUMP file name for option 2
    if [[ $emmopt = 2 ]]; then
      echo ""
      echo -n "Please enter WDUMP file full name: "
      read WDUMP  
      echo ""
    fi 
      # --- check existence, access, and file type of restart file
    if read_access ${WDUMP}; then
      found=1
    else
      echo "$0: *** RE-ENTER RUN SCENARIO & DATEKEY AGAIN ***"
      echo " "
      echo " "
    fi
  done
grep -i geo_input_data ${WDUMP} > geo_input_data.txt
grep -i geo_curve_data ${WDUMP} > geo_curve_data.txt
grep -i geo_builds ${WDUMP} > geo_builds.txt
grep -i geo_temp_data ${WDUMP} > geo_temp_data.txt
grep -i geo_curve_info ${WDUMP} > geo_curve_info.txt
grep -i geo_final_curves ${WDUMP} > geo_final_curves.txt
echo " "
echo " Your Geothermal Debug Files have been created "
echo " "
