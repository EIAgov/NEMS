# $Header: M:/default/scripts/RCS/submit.sh,v 1.54 2020/08/19 22:57:50 adr Exp $
TK_ECHO_USE_BASH_BEHAVIOR=1
#
# Optional argument:  triggers use of nemspar.shell instead of nemsh.shell
#
# The files used by this script are 
# o    primary input files    
#      --  $NEMS/scripts/varkeys             (list of key names and defaults)
#      --  $NEMS/scripts/scedes.aeo95b       (Scenario Descriptor files)
#      --  $NEMS/logs/DEFAULTS             (lookups for RCS versions )
# o    normal shell input files
#      --  $NEMS/scripts/filemgr.shell       (filemgr Shell file) 
#      --  $NEMS/scripts/jcl.shell           (unit 5 runtime options data shell)
#      --  $NEMS/scripts/nemsh.shell         (submitter shell invoked after string replacements here.)
#      --  $NEMS/scripts/nemspar.shell       (alternate to nemsh.shell for parallel version_
#      --  $NEMS/scripts/moreopt.shell       (more runtime options data shell)
# o    Previously awk and sed scripts, now fortran files invoked by this script (in $NEMS/scripts/)
#      --  varkeysawk.f (.exe)  (creates varkeys.files from varkeys defaults)
#      --  scenawk.f (.exe)     (creates string substitutions from scedes files)
#      --  keysawk.f (.exe)     (creates string substitutions from varkeys file)
# o    output files (current directory)  
#      --  keys.sed            (List of string substitutions)
#      --  varkeys.files       (List of default input files)
#      --  nems.sh             (script for executing NEMS)
#      --  filelist            (file_mgr input file for NEMS)
#      --  jcl.dat             (runtime options input file for NEMS)
#      --  moreopt             (more runtime options)
#=========================================================================================
echo;echo;echo;echo;echo;echo;echo
 echo "============================================================="
 echo "                           NEMS RUN"
 echo "============================================================="

# if PARNEMS environment variable not set, use normal shell, else use parallel shell and write message.
 if [ -z "$PARNEMS" ] ; then
   nemsh=nemsh.shell
 else
  echo "****"
  echo "****"
  echo "****    Parallel version of job setup.  Using nemspar.shell"
  echo "****"
  echo "****"
  nemsh=nemspar.shell
 fi

trap "exit" 1 2 15

#  First, check to see if NEMS is locked

if [ -f $NEMS/freeze/freeze.nems ]; then
   echo "NEMS Run Submittal is temporarily disabled while"
   echo "defaults are being put into effect by: "
   cat $NEMS/freeze/freeze.nems
   exit
fi


if [ "$OSTYPE" = "cygwin" ] ; then
  wpwd=`cygpath -m $PWD`
else
  wpwd=$PWD
fi
 echo "Press ENTER to accept the default answers shown in []"    
 echo "Use command line arguments as follows:"
 echo " "
 echo " runnems [user-scedes [output-directory] ] "
 echo "  "
 echo "  "
 echo "   user-scedes is the name of your scenario descriptor file"
 echo "      if it is found in $PWD. "
 echo "      It is also the scenario name used to identify this run" 
 echo "  "
 echo " Command line examples: "
 echo "   jognems myrun u:/output/dsa/aimefd"                
 echo "============================================================="
# make sure they're not in the directory for this script when they execute it.       
 if [ $wpwd = "$NEMS/scripts" ] 
   then
   echo "     Sorry.  There is a slight problem."
   echo "     Your current directory is $PWD."
   echo "     Please execute this from another directory."
   exit 1 
 fi
 if [ -z "$PREPRO" -o "$PREPRO" = "no" ] ; then 
# initialize number of shell files and their names
   numshell=4
   shellfile[1]="$NEMS/scripts/filemgr.shell"
   if [ -f filemgr.shell ] ; then
     echo "  Local filemgr.shell found!  I guess it should be used.  Why not."
     shellfile[1]="filemgr.shell"
   fi
   shellfile[2]="$NEMS/scripts/jcl.shell"
   if [ -f jcl.shell ] ; then
     echo "  Local jcl.shell found!  I guess it should be used.  OK?"
     shellfile[2]="jcl.shell"
   fi
   shellfile[3]="$NEMS/scripts/moreopt.shell"
   if [ -f moreopt.shell ] ; then
     echo "  Local moreopt.shell found!  I guess it should be used.  All rightie then."
     shellfile[3]="moreopt.shell"
   fi
   shellfile[4]="$NEMS/scripts/$nemsh"
   if [ -f $nemsh ] ; then
     echo "  Local $nemsh found!  I guess it should be used.  What the heck."
     shellfile[4]="$nemsh"
   fi

   actualfile[1]="FILELIST"
   actualfile[2]="jcl.dat"
   actualfile[3]="MOREOPT"
   actualfile[4]="nems.sh"
 else
   numshell=2
   echo " "
   echo "   The PREPRO option is set.  Select the program:"
   echo "   1) prepplt   :  The plntdaf preprocessor"
   echo "   2) prepett   :  The ettdem  preprocessor" 
#  echo "   3) prepdsmd  :  The ldsm DAF preprocessor"
   echo -n "   Enter the program number: ";read prepron
   case $prepron in
    1) prepro="prepplt";;
    2) prepro="prepett";;
#    3) prepro="prepdsmd";; 
    *) echo "Exiting..."
       exit;;
   esac
   shellfile[1]="$NEMS/scripts/filemgr.shell"
   if [ -f filemgr.shell ] ; then
     echo "  Local filemgr.shell found!  I guess it should be used.  Why not."
     shellfile[1]="filemgr.shell"
   fi
   shellfile[2]="$NEMS/scripts/$prepro.shell"
   if [ -f $prepro.shell ] ; then
     echo "  Local $prepro.shell found!  I guess it should be used.  What the heck."
     shellfile[2]="$prepro.shell"
   fi
   echo "This will create the NEMS script (nems.sh) from ${shellfile[2]}"
   actualfile[1]="FILELIST"
   actualfile[2]="nems.sh"
 fi
 USCEN="ref2021"
  
 numarg=$#
# check for argument string   
# arg 1:  common scenario or scenario/datecode of run to emulate
# arg 2:  user scenario
# arg 3:  output directory path
# arg 4:
# arg 5:  alternate shell file on which to apply string substitutions     
# arg 6:  alternate output file  

if [ $numarg -gt 0 ]; then
   USCEN=$1
   shift
   numarg=$#
 else
   numscedes=`ls -1t scedes.* 2>/dev/null | grep -v 'scedes\.all\.' | grep -c "scedes"`
   if [ $numscedes -ge 1 ] ; then
     ls -1t scedes.* 2>/dev/null | grep -v 'scedes\.all\.' | sed 's/^.*\.//g' >temp.$$; read USCEN <temp.$$;rm temp.$$
   fi
   echo -n "    Enter User Scenario [$USCEN] :";read uscen
   if [ -n "$uscen" ]; then
     USCEN=$uscen
   fi
 fi
 

# Use date code as the unique run identifier for this scenario.
 vers="a"
 date +"%m%d%y" >temp.$$; read DATE < temp.$$ ; rm temp.$$
 DATE="d$DATE"
 verfile="$NEMSJOBLOG/@.verscomp.$USCEN.$DATE"
 if [ -f $verfile ]; then
   wc -l "$verfile" > temp.dat
   read vers fil < temp.dat
   rm -f temp.dat
#  echo "DEBUG: verfile=$verfile and it exists, last version=$vers"
   vers=`expr $vers + 1 `
   sed "/^$vers /!d;/^$vers / s/[0-9]* //g" $NEMS/scripts/version.seq > temp.dat
   read vers < temp.dat
   rm -f temp.dat
#  echo "This version will be $vers"
 else
   vers="a"
#  echo "DEBUG: verfile=$verfile and it doesnt exist so version=$vers"  
 fi
 DATE="$DATE$vers"
 echo " "
 echo "     The unique name for this run will be $USCEN/$DATE "
 suggested="T:"
# 1) establish default output directory based on what computer is being used 
 if [ $numarg -gt 0 ]; then
   OUTDIR=$1
   shift
   numarg=$#
   needmsg=1
 else
  OUTDIR="/dodah"
  echo " "
  echo "Available space on some NEMS drives "

  # get list of space on drives to store runs on from menu in $NEMS/scripts/submit_outmenu.txt and construct a search expression
  d=""
  for item in `grep -v "#" $NEMS/scripts/submit_outmenu.txt | grep ":" | awk -F, '{print $2}' | sed 's/:.*//;s/ //g'` 
  do
      d="$d$item,"
  done
  d="[$d]"
  d=`echo $d | sed 's/,]/]\\:/;s/\[/^[/' `
  sh $NEMS/scripts/showspace.sh | egrep -i "$d" 
  
  # redo the list to find the testing drive with most space:
    d=""
    for item in `grep -v "#" $NEMS/scripts/submit_outmenu.txt | grep ":" | grep -i "testing" | awk -F, '{print $2}' | sed 's/:.*//;s/ //g'` 
    do
        d="$d$item,"
    done
    d="[$d]"
    d=`echo $d | sed 's/,]/]\\:/;s/\[/^[/' `
  
  # get drive with most space by sorting
  suggested=`sh $NEMS/scripts/showspace.sh | egrep -i "^$d" | sort -k2 | tail -1 | awk '{print $1}'`

 fi
 until [ -d $OUTDIR ]
 do
   needmsg=0
   if [ $OUTDIR != "/dodah" ]; then
     echo " "
     echo "    Directory $OUTDIR does not exist.  Re-enter."
   fi
   computer=`hostname`
# assign general output drive and folder depending on what computer is being used
     
   OUTDIR="$suggested/output/$USER"
# Get Output Directory
  lastmenu=`grep -v "#" $NEMS/scripts/submit_outmenu.txt | awk -F, '{print $1}' | tail -1 | sed 's/[)].*//'`
   outchoice=0
   echo $lastmenu
   while [ $outchoice -lt 1 -o $outchoice -gt $lastmenu ]
   do
     echo " "
     echo "     Select an option for your output directory parent location"
     echo " "
     grep -v "#" $NEMS/scripts/submit_outmenu.txt | awk -F, '{printf("%s) %s %s \n", $1, $2, $3)}' | sed "s/user/$USER/" | sed "s/zzz/$suggested/"
     echo " "
     echo -n "         Enter option number [1] : ";read outchoice
     if [ ! -n "$outchoice" ]; then 
       outchoice=1
     fi
   done
   if [ $outchoice = "1" ]; then
       OUTDIR="$suggested/output/$USER"
   elif [ $outchoice = "2" ]; then
     echo -n "           Enter parent directory in which /$USCEN/$DATE/ will reside [$OUTDIR] :";read outdir1
     if [ -n "$outdir1" ] ; then
       OUTDIR=$outdir1
     fi 
   else
     OUTDIR=`grep "^$outchoice," $NEMS/scripts/submit_outmenu.txt | awk -F, '{print $2}' | sed "s/user/$USER/;s/ //g"`
     echo "you selected $outchoice) $outdir"
   fi

   if [ ! -d $OUTDIR ] ; then
     mkdir $OUTDIR
   fi
 done
# if [ needmsg = "1" ] ; then
   echo "     This run will be stored in $OUTDIR/$USCEN/$DATE "
# fi
#=========================================================================================
# make log entries.  The version file is used to make sure version numbers are unique
# for a given scenario.  could be run numbers rather than datecodes

 if [ ! -f $verfile ] ; then
   echo "$USER $OUTDIR/$USCEN/$DATE " > $verfile
   chmod o+w $verfile
 else
   echo "$USER $OUTDIR/$USCEN/$DATE " >> $verfile
 fi
# substitute UNC file name if outdir is a shared directory on current PC
 logentry=`$NEMS/scripts/getshare.exe $OUTDIR/$USCEN/$DATE`
 logentry="$USER $logentry"
# echo "$USER $OUTDIR/$USCEN/$DATE " >> $NEMSJOBLOG/runlog 
 echo $logentry >> $NEMSJOBLOG/runlog 
#=========================================================================================
 echo " "
# Next arg :  alternate shell file on which to apply string substitutions     
 if [ $numarg -gt 0 ] ; then
   if [ -f $1 ] ; then
     echo "debug: 4th argument is $1"
     echo "Alternate shell file argument found:  $1"
     temp=$1
     shellfile[1]=$1
#    numshell=1    Commented out - using as filemgr.shell override only - temporary due to ktech change
     shift
     numarg=$#
     if [ $numarg -gt 0 ] ; then
       echo "debug: 5th argument is $1"
       if [ ! -f $1 ] ; then
          actualfile[1]=$1
          shift
       else
          echo "   WARNING:  $1 already exists."    
          echo -n "   Enter output file to be made from shell file [${actualfile[1]}] : ";read temp
          if [ -n $temp ]; then
            actualfile[1]=$temp
          fi
       fi
#    else
#      actualfile[1]=$$
#      echo -n "     Enter output file to be made from shell file [${actualfile[1]}] : ";read temp
#      if [ -n $temp ]; then
#        actualfile[1]=$temp
#      fi
     fi
   else
     echo "  ERROR:  The shell file override does not exist: $1"
     exit 1
   fi
 fi
 if [ -f "$scentext" -o -f "scentext.$USCEN" ] ; then
   echo " ===============================================================================\n" > scentext.$USCEN.$DATE 
   echo " Description of run $USCEN.$DATE \n" >> scentext.$USCEN.$DATE 
 fi  
    
 if [ -f "$scentext" ] ; then
   sed 's/^/ /' $scentext >> scentext.$USCEN.$DATE
 fi
 if [ -f "scentext.$USCEN" ] ; then
   sed 's/^/ /'  scentext.$USCEN >> scentext.$USCEN.$DATE
 fi  

# Copy shell files.

 n=1
 while [ "$n" -le "$numshell" ] 
 do
   cp ${shellfile[((n))]} ${actualfile[((n))]}
   chmod ugo+w ${actualfile[((n))]} 
# increment counter for number of shell files. Remember: no space allowed after the =
   n=`expr $n + 1 `
 done 

#  2) Generate list of input files and their default names from varkeys file

# Don't run varkeysawk.exe in Git environment, just copy over varkeys.files from scripts folder
# $NEMS/scripts/varkeysawk.exe <$NEMS/scripts/varkeys >varkeys.files
#   cp $NEMS/scripts/varkeys.files .

#  3) Invoke string substitutions in shell files.
#     The order of the substitutions establishes the precedence:  first value is used

#     3a) Set output directory first
 echo 'OUTDIR='$OUTDIR  > keys.sed
 echo 'SCEN='$USCEN    >> keys.sed
 echo 'DATE='$DATE     >> keys.sed
 if [ ! -z "$PARNEMS" ] ; then
   echo 'EXBUILD=0' >> keys.sed
   echo 'CUTITR=0' >> keys.sed
 fi
#     3b) Look for user scedes file and, if found, use it first
 uscedes="$wpwd/scedes.$USCEN"
 inuser="noitsnot"
 if [ -f $uscedes ] ; then
   echo "The user scenario descriptor file, $uscedes, will be used."
   grep -i 'intlfdbk=0' $uscedes > /dev/null
   if [ $? -eq 0 ] ; then
     inuser="yesitis0"
   else
     grep -i 'intlfdbk=1' $uscedes > /dev/null
     if [ $? -eq 0 ] ; then
       inuser="yesitis1"
     fi
   fi
   rm -f warn

   $NEMS/scripts/scenawk.exe $uscedes >>keys.sed
   if [ -f warn ] ; then cat warn;rm warn; fi
 else
   echo "The optional user scenario descriptor file, $uscedes , not found."
 fi
# NO LONGER NEEDED    3c) Look for common scedes file

 #     3d) Get all other keys from the default varkeys file 
 # $NEMS/scripts/keysawk.exe <$NEMS/scripts/varkeys >>keys.sed

#  Apply string substitutions to the shell files to create the actual files
 n=1
 while [ "$n" -le "$numshell" ] 
 do
   $NEMS/scripts/keyssed.exe keys.sed ${actualfile[((n))]}
   n=`expr $n + 1 `
 done 

#make the object files
echo "Compiling .obj files:"
make -C $NEMS/source all
echo "Done compiling .obj files"

 rm -f tempfile 
 #rm -f keys.sed
 rm -f nems.sh
 rm -f FILELIST
 rm -f MOREOPT
 rm -f jcl.dat

#    8) Run the nems.sh  script, creating directories and linking
if [ -z "$PARNEMS" ] ; then
  using="jognems"
else
  using="parnems"
fi
echo "echo This run was set up using $using >> launched.from " >> nems.sh.$USCEN.$DATE
echo "echo The common scedes file or run used to set up this run: $scedes >> launched.from " >> nems.sh.$USCEN.$DATE
echo "echo The user scedes file to set up this run:               $uscedes >> launched.from " >> nems.sh.$USCEN.$DATE
echo "echo OUTDIR=$OUTDIR/$USCEN/$DATE >> launched.from " >> nems.sh.$USCEN.$DATE
sh nems.sh.$USCEN.$DATE | tee nems.sh.out.$USCEN.$DATE

#export $NEMS for tagging
export $DATE

echo " "
echo " "
echo "A copy of the messages above was saved in nems.sh.out.$USCEN.$DATE"
