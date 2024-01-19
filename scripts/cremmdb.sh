# /emm/scripts/cremmb.sh
# This macro will create an emmdb.mdb database file from the specified EDBPGRP.txt NEMS output file.                   
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
print "***  EMMDB database creation script        ***"
print "***                                        ***"
print "***   This script will create an emmdb.mdb ***"
print "***   database file for a specified NEMS   ***"
print "***   run and cycle. The emmdb.mdb is      ***"
print "***   created from the NEMS output file:   ***"
print "***   EDBPGRP.txt                          ***"
print "**********************************************"
print " "
print "Press return to get default answers shown in brackets <>"
print ""
acceptable=0
while [[ $acceptable = 0 ]]
do
  print "Choose EDBPGRP option : "
  print "  1.  Run this script within a NEMS run directory "
  print "  2.  Locate EMMDB EDBPGRP input file using NEMS run scenario "
  print "  3.  User specifies full name of EMMDB EDBPGRP input file   "
  print ""
  print -n "Please enter EMMDB input file option < 1 > : "
  read EOPT 
  case $EOPT in
       [1]    ) emmopt=1
                 acceptable=1;;
       [2]    ) emmopt=2
                 acceptable=1;;         
       [3]    ) emmopt=3
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
      # --- run script in run directory for option 1
    if [[ $emmopt = 1 ]]; then
      print " " 
      print " *** Uncompressing and copying emmdb.mdb to emmdb.finalcycle.mdb ***"
      print " " 
      uncompress emmdb.mdb
      copy emmdb.mdb EMMDB.FINALCYCLE.MDB
      print " "
  if [ ! -f edbpgrp* ]; then   
       print " "
       print " EDBPGRP.txt file not found in this directory. "
       print " Rerun script in run directory or with a different option."
       print " "
       exit   
  else
       print " " 
       print " *** Uncompressing edbpgrp.txt files *** "
       print " " 
       uncompress edbpgrp*
       print " " 
       print "**********************************************"
       print "***   The following EDBPGRP files are      ***"
       print "***     availble for emmdb database        ***"
       print "***     creation from this run.            ***"
       print "**********************************************"
       print " "
       ls -l edbpgrp.*.txt
   fi
      print " "
      print " "
      accept=0
      while [[ $accept = 0 ]]
      do
        print -n "Please enter NEMS run cycle for emmdb creation   : "
        read CYCLE  
        if [[ -z "$CYCLE" ]]; then
          print '$0: *** CYCLE HAS NO DEFAULT VALUE - RE-ENTER ***'
          print ' '
        else
          accept=1
        fi
      done
      EDBP="EDBPGRP.${CYCLE}.txt"
      OUTFIL=${CYCLE}                
    fi 
    
    
    if [[ $emmopt = 2 ]]; then
       found=0
    while [[ $found != 1 ]]
    do
       print -n "Please enter NEMS run scenario <REF2019> : "
       read SCEN
       if [[ -z "$SCEN" ]]; then
          SCEN='ref2019'
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

       grep  \/${SCEN}\/${DATEKEY} $NEMSJOBLOG/runlog | sed "s!^.* !!" > temp.$$ 
       read RUNDIR < temp.$$ ; rm temp.$$
       if [ -d  "$RUNDIR" ]; then
         found=1
       else
         echo " This run was not found in the runlog please reenter run name" $SCEN $DATEKEY
       fi
      
    done
    print " " 
    print " *** Uncompressing edbpgrp.txt files *** "
    print " " 
    uncompress ${RUNDIR}/p2/edbpgrp*
    print " " 
    print "**********************************************"
    print "***   The following EDBPGRP files are      ***"
    print "***     availble for emmdb database        ***"
    print "***     creation from this run.            ***"
    print "**********************************************"
    print " "
    ls -l ${RUNDIR}/p2/edbpgrp.*.txt
    print " "
    print " "
    accept=0
    while [[ $accept = 0 ]]
    do
      print -n "Please enter NEMS run cycle for emmdb creation   : "
      read CYCLE  
      if [[ -z "$CYCLE" ]]; then
        print '$0: *** CYCLE HAS NO DEFAULT VALUE - RE-ENTER ***'
        print ' '
      else
        accept=1
      fi
    done      
#    print " cycle ${CYCLE}"
      # --- Get full scenario name using runlog
      grep  \/${SCEN}\/${DATEKEY} $NEMSJOBLOG/runlog | sed "s!^... !!" > temp.$$
      read RUNDIR < temp.$$ ; rm temp.$$
      print "${RUNDIR}/p2/EDBPGRP."${CYCLE}".txt" > temp.$$ ; read EDBP < temp.$$ ; rm temp.$$
#    print " EDBP ${EDBP}"
     OUTFIL="${SCEN}.${DATEKEY}.${CYCLE}"                
  fi
  
  
  
      # --- Get EDBPGRP.txt file name for option 2
    if [[ $emmopt = 3 ]]; then
     found=0
     while [[ $found != 1 ]]
     do
      print ""
      print -n "Please enter EDBPGRP.txt file full name: "
      read EDBP   
      print ""
#    print " EDBP ${EDBP}"
      # --- check existence, access, and file type of restart file
      if read_access ${EDBP}; then
        found=1
      else
        print "$0: *** EDBPGRP FILE NOT FOUND -- RE-ENTER FILE NAME ***"
        print " "
        print " "
      fi
     done
     echo " "
     echo -n " Please enter scenario name to attach to emmdb file < test > : "
     read OUTFIL
      if [[ -z "$OUTFIL" ]]; then
#      --- base case is default                           
        OUTFIL="test"
      fi
    fi 

print " "
print "***  Copying necessary files -- may take a few minutes ***"
print " "

cp $EDBP edbpgrp.txt
emmdb="$NEMS/input/emmdb.mdb"
# print " emmdb ${emmdb}"
cp $emmdb emmdb.mdb
udbpe="$NEMS/source/udbp.exe"
# print " udbpe ${udbpe}"
cp $udbpe udbp.exe  

print " "
print "***  Running database creation progam  ***"
print "***    This can take up to 20 minutes  ***"
print " "

 udbp.exe > udbp.$OUTFIL.out

mv emmdb.mdb EMMDB.$OUTFIL.MDB
rm edbpgrp.txt
 if [[ $emmopt = 1 ]]; then
   mv emmdb.finalcycle.mdb EMMDB.MDB
 fi    

  echo " "
  echo "  Your emmdb database file has been created and is stored in -- emmdb.$OUTFIL.mdb -- "
  echo "  Writes to unit 6 are stored in -- udbp.$OUTFIL.out -- "
  echo " "

