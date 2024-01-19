# /emm/scripts/hyddbug.sh 
# This macro will create the files necessary to create the excel hyd dbug file
#
 rm hyd_input_data.txt
 rm hyd_curve_data.txt
 rm hyd_builds.txt
 rm hyd_temp_data.txt
 rm hyd_curve_info.txt
 rm hyd_final_curves.txt
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
 print $1
  $NEMS/scripts/checkfor.z.sh $1
  if [[ ! -a $1 ]]; then
    print "$0: file $1 not found"
    return -1
  fi
# --- does user have read access
  if [[ ! -r $1 ]]; then   
    print "$0: do not have read permission for $1"
    return -1
  fi  
# --- file is not directory or other special file type
  if [[ ! -f $1 ]]; then   
    print "$0: file $1 is either a directory or other special type"
    return -1
  fi  
  return 0
  }
# Start of script - Print a heading
print " "
print " "
print "**********************************************"
print "***                                        ***"
print "***  Hydro Debug Script                    ***"
print "***                                        ***"
print "***   This script will create the input    ***"
print "***   files to the hydro debug             ***"
print "***   spreadsheet file. The data is        ***"
print "***   obtained from the WDUMP.txt file.    ***"
print "**********************************************"
print " "
print "Press return to get default answers shown in brackets <>"
print ""
acceptable=0
while [[ $acceptable = 0 ]]
do
  print "Choose WDUMP option : "
  print "  1.  Locate WDUMP file using NEMS scenario "
  print "  2.  User specifies full name of WDUMP file   "
  print ""
  print -n "Please enter WDUMP option < 1 > : "
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
                   print " *** UNACCEPTABLE RESPONSE - RE-ENTER ***"
                 fi;;
  esac     
done
print " "
  found=0
  while [[ $found != 1 ]]
  do
    if [[ $emmopt = 1 ]]; then
    print -n "Please enter NEMS run scenario <aeo2001> : "
    read SCEN
    if [[ -z "$SCEN" ]]; then
      SCEN='aeo2001'
    fi
    accept=0
    while [[ $accept = 0 ]]
    do
      print -n "Please enter NEMS run datekey  : "
      read DATEKEY
      if [[ -z "$DATEKEY" ]]; then
        print '$0: *** DATEKEY HAS NO DEFAULT VALUE - RE-ENTER ***'
        print ' '
      else
        accept=1
      fi
    done      
      # --- Get full scenario name using runlog
    grep  \/${SCEN}\/${DATEKEY} $NEMSJOBLOG/runlog | sed "s!^.* !!" > temp.$$
    read RUNDIR < temp.$$ ; rm temp.$$
    print "${RUNDIR}/p2/WDUMP.txt" > temp.$$ ; read WDUMP < temp.$$ ; rm temp.$$
    fi
      # --- Get WDUMP file name for option 2
    if [[ $emmopt = 2 ]]; then
      print ""
      print -n "Please enter WDUMP file full name: "
      read WDUMP  
      print ""
    fi 
      # --- check existence, access, and file type of restart file
    if read_access ${WDUMP}; then
      found=1
    else
      print "$0: *** RE-ENTER RUN SCENARIO & DATEKEY AGAIN ***"
      print " "
      print " "
    fi
  done
grep -I hyd_input_data ${WDUMP} > hyd_input_data.txt
grep -I hyd_curve_data ${WDUMP} > hyd_curve_data.txt
grep -I hyd_builds ${WDUMP} > hyd_builds.txt
grep -I hyd_temp_data ${WDUMP} > hyd_temp_data.txt
grep -I hyd_curve_info ${WDUMP} > hyd_curve_info.txt
grep -I hyd_final_curves ${WDUMP} > hyd_final_curves.txt
print " "
print " Your Hydro Debug Files have been created "
print " "
