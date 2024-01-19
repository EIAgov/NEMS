# script to allow launch ANALYZE from another directory.
# this essentially insures that ANALYZE will be able to
# find a SETUP.EXC file.  If one is not in the user's
# home directory, then one is copied from the 
# /default/analyze/ directory
clear
echo " -------------- ANALYZE Script for EIA -----------------"
echo "   This script brought to you by the NEMS Branch     "
echo " -------------------------------------------------------"
echo " "
if [ "&PWD" != "/default/analyze" ] ; then
  if [ ! -a SETUP.EXC ] ; then
    echo "     I did not find a copy of your SETUP.EXC file, so "
    echo "     I will copy the ANALYZE setup file, SETUP.EXC, to"
    echo "     $PWD."
    echo " "
    echo "     You may modify SETUP.EXC to define file prefixes"
    echo "     to point to particular directories.  Otherwise, "
    echo "     most files are assumed to be in your current" 
    echo "     working directory.  "
    echo " "
    echo "     Use the _SETUP command in ANALYZE to"
    echo "     verify what file prefixes are in effect."
    echo " "
    read var?"     Press ENTER to continue, CTRL-c to abort..."
    cp /default/analyze/SETUP.EXC .
  else
   echo "     The following ANALYZE Setup file has been found:"
   echo " "
   more SETUP.EXC
   echo " "
   read var?"     Press ENTER to continue, CTRL-c to abort..."
  fi
else
  echo "     You are running ANALYZE from $PWD"
  read var?"     Press ENTER to continue, CTRL-c to abort..."
fi
#/default/analyze/ANALYZE
/default/analyze/source/ANALYZE.new.exe 
