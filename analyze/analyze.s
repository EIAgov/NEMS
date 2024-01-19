# script to allow launch ANALYZE from another directory.
TK_ECHO_USE_BASH_BEHAVIOR=1
# this essentially insures that ANALYZE will be able to
# find a SETUP.EXC file.  If one is not in the user's
# home directory, then one is copied from the 
# /default/analyze/ directory

echo " -------------- ANALYZE Script for EIA -----------------"
echo " "
var=' ' 
if [[ "$PWD" != "$NEMS/analyze" ]] ; then
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
    echo -n "     Press ENTER to continue, CTRL-c to abort...";read var
    cp $NEMS/analyze/SETUP.EXC .
  else
   echo "     The following ANALYZE Setup file has been found:"
   echo " "
   cat SETUP.EXC 
  fi
   echo " "
   echo "Select Version of Analyze"
   echo "==> just press ENTER to run $NEMS/analyze/ANALYZE"
   echo '==> "fat" to get a larger version $NEMS/analyze/source/ANALYZE.new.exe'
   echo -n "     Version: ";read var
else
  echo "     You are running ANALYZE from $PWD"
  echo "Select Version of Analyze"
  echo "==> just press ENTER to run /default/analyze/ANALYZE"
  echo '==> "fat" to get a larger version /default/analyze/source/ANALYZE'
  echo -n "     Version: ";read var
fi
if [ "$var" = "dan" ] ; then
  $NEMS/analyze/source/ANALYZE.dan.exe
else
  $NEMS/analyze/source/ANALYZE.new.exe
fi
