# $Header: m:/default/scripts/RCS/nftab.sh,v 1.69 2020/02/07 14:32:32 pkc Exp $
TK_ECHO_USE_BASH_BEHAVIOR=1
trap "exit" 1 2 15
function read_access {
#   function read_access checks to see if:
#     1. file exist
#     2. user has read access to file
#     3. file is not a directory or some other special file type
#  function returns 0 if all test passed and returns -1 on error
#  
# --- does file exist
  sh $NEMS/scripts/checkfor.z.sh $1
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
. $NEMS/scripts/commands.sh
echo " "
echo " "
echo "***************************************"
echo "*   NEMS Ftab Report Writer Script    *"
echo "***************************************"
echo " "
echo "Press return to get default answers shown in brackets <>"
echo " "
#grep "^fwk1io.obj objects default" $NEMS/logs/DEFAULTS > temp.$$
#  read junk1 junk2 junk3 vers4 junkend < temp.$$ ; rm temp.$$
#grep "^cio4wk1.obj objects default" $NEMS/logs/DEFAULTS > temp.$$
#  read junk1 junk2 junk3 vers5 junkend < temp.$$ ; rm temp.$$
fwk1name="$NEMS/source/fwk1io.obj"
cioname="$NEMS/source/cio4wk1.obj"
# --- See how many restart files are to be in the report
acceptable=0
default_year=2019
while [[ $acceptable = 0 ]]
do
  echo -n 'Number of runs to report (1-7): <1> '
  read NUMSCEN
  case $NUMSCEN in
     [1-7]   ) acceptable=1;;
     *       ) if [[ -z "$NUMSCEN" ]]; then              
                 let NUMSCEN=1
                 acceptable=1
               else
                 echo '*** UNACCEPTABLE RESPONSE -- RE-ENTER ***'   
               fi;;
  esac
done           
# --- Get valid names for restart files, uses function read_access
let count=0
while [[ $count < $NUMSCEN ]]
do
  let count=count+1
  found=0
  while [[ $found != 1 ]]
  do
    echo -n "Enter restart version (vx.x) or run scenario $count: <ref2022> "
    read SCEN[$count]
    if [[ -z "${SCEN[$count]}" ]]; then
      SCEN[$count]='ref2022'
    fi
#  see if the entry is in the form vx.x
    ivers=0
    ivers=`echo "${SCEN[$count]}" | grep -c "v[1-9][0-9]*\.[0-9]*" `
    if [ "$ivers" -gt 0 ] ; then
      RESTART[$count]="$NEMS/input/restart.${SCEN[$count]}" 
      DATEKEY[$count]="${SCEN[$count]}"
      SCEN[$count]="restart"
    else
      acceptable=0
      while [[ $acceptable = 0 ]]
      do
        echo -n "Enter run datekey $count:"
        read DATEKEY[$count]
        if [[ -z "${DATEKEY[$count]}" ]]; then
          echo '$0: *** DATEKEY HAS NO DEFAULT VALUE - RE-ENTER ***'
          echo ' '
        else
          acceptable=1
        fi
      done      
      # --- Get full restart file name using runlog
      grep -i "\/${SCEN[$count]}\/${DATEKEY[$count]}" $NEMSJOBLOG/runlog | sed "s!^... !!"  > temp.$$
      read RESTART[$count] < temp.$$ ; rm temp.$$
      echo "${RESTART[$count]}" | sed "s!/${SCEN[$count]}!!" | sed "s!/${DATEKEY[$count]}!!" > temp.$$
      read RUNDIR < temp.$$ ; rm temp.$$
      echo "${RESTART[$count]}/RESTART.unf" > temp.$$ ; read RESTART[$count] < temp.$$ ; rm temp.$$
      # --- check existence, access, and file type of restart file
    fi
    sh $NEMS/scripts/checkfor.z.sh ${RESTART[$count]} 
    if read_access ${RESTART[$count]}; then
      found=1
    else
      echo "$0: *** RE-ENTER RUN SCENARIO & DATEKEY AGAIN ***"
      echo " "
      echo " "
    fi
  done
done  
# --- TABREQ portion of script
# --- Find out which tables to be shown in the report
# lnkinems:  use Intel linker and libraries for intel-compiled versions in aeo2006 and beyond
lnkinems=0  
self=0
acceptable=0
while [[ $acceptable = 0 ]]
do
  echo " "
  echo "Select from the following types of output reports:"
  echo "     1 - AEO tables"
  echo "     4 - Other (defined by your input file)"
  echo -n "Enter your choice (1, or 4) :<1>"
  read SELECTED

  case $SELECTED in
      1        )  TABREQ=$NEMS/input/tabreq.txt
                  acceptable=1 ;;
      4        )  self=1; acceptable=1;;
      *        )  if [[ -z "$SELECTED" ]]; then
 #                   fdef tabreq.txt s default | sed "s/.*is //g" > temp.$$ ; read tbrqvers < temp.$$; rm temp.$$
                    TABREQ=$NEMS/input/tabreq.txt
                    acceptable=1
                  else
                    echo "*** UNACCEPTABLE RESPONSE - RE-ENTER ***"
                  fi;;
  esac
done  
#echo " SELECTED = $SELECTED"
#
# --- get a valid name for tabreq file if 4 selected
while [[ "$self" = 1 ]]
do
  echo -n 'Enter the name of the tabreq file: '
  read TABREQ
# --- check existence, access, and file type of tabreq file
  sh $NEMS/scripts/checkfor.z.sh $TABREQ
  if read_access $TABREQ; then
    self=0
  else
    echo "$0: *** RE-ENTER TABREQ FILE AGAIN ***"
    echo " "
    echo " "
  fi
done
# see if default printing options to be used
acceptable=0
while [[ $acceptable = 0 ]]
do
  echo " "
  echo -n "Use the default printing options (y/n)? "
  read PRINTDEF
  case $PRINTDEF in
       [Yy]    ) self=0
                 acceptable=1;;
       [Nn]    ) self=1
                 acceptable=1;;         
       *       ) if [[ -z "$PRINTDEF" ]]; then
                   echo " *** THIS ITEM HAS NO DEFAULT "
                 fi  
                 echo " *** UNACCEPTABLE RESPONSE - RE-ENTER ***";;
  esac     
done
DEFDOLYR="$default_year"
if [[ $self = 1 ]]; then
  # --- if default print settings not selected
  # --- see if wish to print regions
  echo " "
  echo -n "Do you want to print regions (y/n)? <n>"
  read YAORNA
  case $YAORNA in
     [Yy]    ) REGIONS=1;;
     *       ) REGIONS=0;;         
  esac     
  echo " "
  echo -n "Do you want to print carbon dioxide (as opposed to carbon) (y/n)? <y>"
  read YAORNA
  case $YAORNA in
     [Nn]    ) FTABCARB="c";;
     *       ) FTABCARB="co2";;         
  esac 
  echo " "
  echo -n "Do you want to print the bonus rows (y/n)? <y>"
  read YAORNA
  case $YAORNA in
     [Nn]    ) FTABONUS="0";;
     *       ) FTABONUS="1";;         
  esac 
  # --- Get first print year for report
  acceptable=0
  while [[ $acceptable = 0 ]]
  do
    echo -n "Enter the first print year: <2014> "
    read FYR
    if [[ -z "$FYR" ]]; then
      FYR=2014; acceptable=1
    else
      if [[ $FYR < 1990 || $FYR > 2050 ]]; then
        echo " *** UNACCEPTABLE START YEAR - RE-ENTER *** "
        echo " "
      else
        acceptable=1
      fi
    fi
  done
  # --- Get last print year of report        
  acceptable=0
  while [[ $acceptable = 0 ]]
  do
    echo -n "Enter the last print year: <2050> "
    read LYR
    if [[ -z "$LYR" ]]; then
      LYR=2050; acceptable=1
    else
      if [[ $LYR < $FYR || $LYR > 2050 ]]; then
        echo " *** UNACCEPTABLE LAST YEAR - RE-ENTER *** "
        echo " "
      else 
        acceptable=1
      fi
    fi
  done     
  # --- See if want to use 5 year skip option
  acceptable=0
  while [[ $acceptable = 0 ]]
  do
    echo -n "Do you want the 5 year printing option (y/n)? <y> "
    read YAORNA
    case $YAORNA in
         [Yy]    ) Y5OPTION=1
                 SKIPYR=2020
                 acceptable=1;;
         [Nn]    ) Y5OPTION=0
                 SKIPYR=2050
                 acceptable=1;;         
         *       ) if [[ -z "$YAORNA" ]]; then
                   Y5OPTION=1
                   SKIPYR=2020
                   acceptable=1
                 else  
                   echo " *** UNACCEPTABLE RESPONSE - RE-ENTER ***"     
                 fi;;
    esac     
  done
  # --- If 5 year skip option selected, determine first skip year
  if [[ "$Y5OPTION" = 1 ]]; then
    acceptable=0
    while [[ $acceptable = 0 ]]
    do
      echo -n "Enter the year to start skipping: <$SKIPYR> "
      read SKIPYR
      if [[ "$SKIPYR" < $FYR || "$SKIPYR" > $LYR ]]; then
        if [[ -z "$SKIPYR" ]]; then
          SKIPYR=2020
          acceptable=1
        else
          echo " *** UNACCEPTABLE RESPONSE - RE-ENTER ***"
          echo " "
        fi
      else
        acceptable=1  
      fi
    done
  fi
  # --- See if want growth column in the report
  acceptable=0
  while [[ $acceptable = 0 ]]
  do
    YAORNA=""
    echo -n "Do you want to have the growth column (y/n)? <y>"
    read YAORNA
    if [[ -z "$YAORNA" ]]; then
          YAORNA="y"
    fi
    case $YAORNA in
       [Yy]    ) GROWCOL="y";;
       *       ) GROWCOL="n";;
    esac     
    case $GROWCOL in
         [Yy]    ) DOGROW=1
                   C_reply=""
                   echo -n "In calendar years, which year would you like as a base year for calculating the growth rate? <$default_year> "
                   read C_reply
                      if [[ -z "$C_reply" ]]; then
                          GROWYR="$default_year"
                      else
                          GROWYR="$C_reply"
                      fi
                 acceptable=1;;
         [Nn]    ) DOGROW=0
                   GROWYR="$default_year"
                 acceptable=1;;         
         *       ) if [[ -z "$GROWCOL" ]]; then
                   DOGROW=1
                   acceptable=1
                 else  
                   echo " *** UNACCEPTABLE RESPONSE - RE-ENTER ***"
                 fi;;
    esac
  done
  # --- Set year to start growth rate
C_reply=""
echo -n "In calendar years, which year would you like the cumulative capacity additions to begin? <$default_year> "
read C_reply
  if [[ -z "$C_reply" ]]; then
      CUMCAPADD="$default_year"
  else
      CUMCAPADD="$C_reply"
  fi
C_reply=""
echo -n "In calendar years, which year would you like the cumulative oil and gas production to begin? <$default_year> "
read C_reply
  if [[ -z "$C_reply" ]]; then
      CUMOILGAS="$default_year"
  else
      CUMOILGAS="$C_reply"
  fi
else
  # --- Default print options set, if they were selected
  REGIONS=0
  FYR=2015
  LYR=2050
  Y5OPTION=0
  SKIPYR=2020
  DOGROW=0
  GROWYR="$default_year"
  DOLARYR="$default_year"
  CUMCAPADD="$default_year"
  CUMOILGAS="$default_year"
  FTABCARB="co2"
  FTABONUS="1"
fi
# --- See if user wishes to generate a worksheet of the report.
echo " "
echo -n "Do you want to create a worksheet (y/n)? <n>"
read YAORNA
case "$YAORNA" in
   [Yy]    ) DOWK1=1;;
   *       ) DOWK1=0;;         
esac 
# --- See if user wishes to generate a graf2000 file
echo " "
echo -n "Do you want to create a file for graf2000 (y/n)? <n>"
read YAORNA
case "$YAORNA" in
   [Yy]    ) LADYFILE=4;;
   *       ) LADYFILE=0;;         
esac 
if [ "$NUMSCEN" = 1 ]; then
  date > temp.$$ ; read VAR1 < temp.$$ ; rm temp.$$
  COMMENT=" in $RUNDIR reported on $VAR1"
else
  COMMENT=" (1),"
  let count=2
  while [[ $count < $NUMSCEN ]]
  do
    echo "$COMMENT ${SCEN[$count]}.${DATEKEY[$count]} ($count), " > temp.$$
    read COMMENT < temp.$$ ; rm temp.$$
    let count=count+1
  done
  echo "$COMMENT and ${SCEN[$count]}.${DATEKEY[$count]} ($count). " > temp.$$
  read COMMENT < temp.$$ ; rm temp.$$
fi  
if [ $DOLARYR != $DEFDOLYR ]; then
  echo "$COMMENT. Dollar year is $DOLARYR." > temp.$$ ; read COMMENT < temp.$$ ; rm temp.$$
fi   
echo "Please enter a comment for the report:"
echo "< ${SCEN[1]}.${DATEKEY[1]} $COMMENT >"
read COMMENT2
if [[ -n "$COMMENT2" ]]; then
  COMMENT=$COMMENT2
fi  

  
 
     reptype=default
     lnkinems=5
     IVERS="18.1";export IVERS
      . $NEMS/scripts/ifortvars18.sh
           
      
 
# --- Get default version of dictionary
#grep "dict.txt input $reptype " $NEMS/logs/DEFAULTS > temp.$$
#read junk1 junk2 junk3 dictvers junkend < temp.$$ ; rm temp.$$
DICT="$NEMS/input/dict.txt"

sh $NEMS/scripts/checkfor.z.sh $DICT print
# --- LAYOUT portion of script
# --- Find out if want to use default or user layout file
self=0
acceptable=0
case "$reptype" in
 mcintosh ) LAYOUT="M:/rec/rsc/merc/input/layout.mc.txt" ;;
 mercury  ) LAYOUT="M:/rec/rsc/merc/input/layout.mc.txt" ;;
 jefferep ) LAYOUT="M:/rec/rsc/merc/input/layout.nox.txt" ;;
 mccainlieb ) LAYOUT="L:/main/pkc/jq5/publish/layout.txt" ;;
 *        )
while [[ $acceptable = 0 ]]
do
  echo " "
  echo -n "Do you want to use the $reptype layout file (y/n) <y>?"
  read SELLAYOT

  case $SELLAYOT in
      [Yy]        ) LAYOUT="$NEMS/input/layout.txt"
                    sh $NEMS/scripts/checkfor.z.sh $LAYOUT print
                    acceptable=1 ;;
      [Nn]        ) acceptable=1 ; self=1;;
      *        )  if [[ -z "$SELLAYOT" ]]; then
# --- Set layout file -- need to something similiar to above later
#                    grep "layout.txt input $reptype " $NEMS/logs/DEFAULTS > temp.$$
#                    read junk1 junk2 junk3 layvers junkend < temp.$$ ; rm temp.$$
                    LAYOUT="$NEMS/input/layout.txt"
                    sh $NEMS/scripts/checkfor.z.sh $LAYOUT print
                    acceptable=1 
                  else
                    echo "*** UNACCEPTABLE RESPONSE - RE-ENTER ***"
                  fi;;
  esac
done
# --- Set dollar year
acceptable=0
while [[ $acceptable = 0 ]]  
do
  echo -n "Enter dollar year (deflator) to use (1987-2050). <$DEFDOLYR> "
  read DOLARYR
  if [[ "$DOLARYR" > 1986 && "$DOLARYR" < 2051 ]]; then
    acceptable=1
  else 
    if [[ -z "$DOLARYR" ]]; then   
      DOLARYR=$DEFDOLYR
      acceptable=1
    else  
      echo " *** UNACCEPTABLE RESPONSE - RE-ENTER ***"     
    fi
  fi
done      
#
# --- get a valid name for layout file if answer was No to $reptype version
while [[ $self = 1 ]]
do
  echo -n 'Enter the name of the layout file: '
  read LAYOUT
# --- check existence, access, and file type of layout file
  sh $NEMS/scripts/checkfor.z.sh $LAYOUT
  if read_access $LAYOUT; then
    self=0
  else
    echo "$0: *** ENTER LAYOUT FILE AGAIN ***"
    echo " "
    echo " "
  fi
done
;;
esac

# --- Get STIFDATA file for old times sake
#grep "stifdata.txt input $reptype " $NEMS/logs/DEFAULTS > temp.$$
#was_there_stiffile=`wc -l < temp.$$`
#if [ $was_there_stiffile -gt 0 ]; then
#   echo "found stifdata file in DEFAULTS!"
#   read junk1 junk2 junk3 stifvers junkend < temp.$$ ; rm temp.$$
#else
   echo "using latest stifdata file"
  # sh $NEMS/scripts/fdef.sh stifdata.txt s | sed "s/.*is //g" > temp.$$ ; read stifvers < temp.$$ ; rm temp.$$
#fi
STIFDATA=$NEMS/input/stifdata.txt
# --- Get FTABXML file
#grep "ftabxml.txt input $reptype " $NEMS/logs/DEFAULTS > temp.$$
#was_there_xmlfile=`wc -l < temp.$$`
#if [ $was_there_xmlfile -gt 0 ]; then
#   echo "found xml file in DEFAULTS!"
#   read junk1 junk2 junk3 xmlvers junkend < temp.$$ ; rm temp.$$
#else
#   echo "using default xml file"
#   sh $NEMS/scripts/fdef.sh ftabxml.txt s | sed "s/.*is //g" > temp.$$ ; read xmlvers < temp.$$ ; rm temp.$$
#fi
FTABXML=$NEMS/input/ftabxml.txt

# --- Get footnote source file
#sh $NEMS/scripts/fdef.sh citation.txt s | sed "s/.*is //g" > temp.$$ ; read citevers < temp.$$ ; rm temp.$$
CITATION=$NEMS/input/citation.txt
if [ -f citation.txt ] ; then
  CITATION=citation.txt
  echo "Local footnote source citation file found, so it will be used."
fi

# --- create file ftab.sed
echo "s!^SCENARIO!${SCEN[1]}!" > ftab.sed
echo "s!^DATEKEY!${DATEKEY[1]}!" >> ftab.sed
echo "s!^REPCOMMENT!$COMMENT!" >> ftab.sed
echo "s!^REGIONS!$REGIONS!" >> ftab.sed
echo "s!^FYR!$FYR!" >> ftab.sed
echo "s!^LYR!$LYR!" >> ftab.sed
echo "s!^DOGROW!$DOGROW!" >> ftab.sed
echo "s!GROWYR!$GROWYR!" >> ftab.sed
echo "s!^Y5OPTION!$Y5OPTION!" >> ftab.sed
echo "s!SKIPYR!$SKIPYR!" >> ftab.sed
echo "s!^NUMSCEN!$NUMSCEN!" >> ftab.sed
echo "s!^DOLARYR!$DOLARYR!" >> ftab.sed
echo "s!^DOWK1!$DOWK1!" >> ftab.sed
echo "s!^TABREQ!$TABREQ!" >> ftab.sed
echo "s!^DICT!$DICT!" >> ftab.sed
echo "s!^LAYOUT!$LAYOUT!" >> ftab.sed
echo "s!^FTABCARB!$FTABCARB!" >> ftab.sed
echo "s!^FTABONUS!$FTABONUS!" >> ftab.sed
echo "s!^FTABDISC!\.08!" >> ftab.sed
let count=0
while [[ $count < $NUMSCEN ]]
do
  let count=count+1  
  echo "s!RESTART$count!${RESTART[$count]}!" >> ftab.sed
done
echo "s!FTABXML!$FTABXML!" >> ftab.sed
echo "s!CITATION!$CITATION!" >> ftab.sed
echo "s!LADYFILE!$LADYFILE!" >> ftab.sed
echo "s!CUMCAPADD!$CUMCAPADD!" >> ftab.sed
echo "s!CUMOILGAS!$CUMOILGAS!" >> ftab.sed
# --- Use ftab.sed and ftab.template to create ftab.dat
if [ -f ftab.template ] ; then
  echo "Local ftab template file found.  It will be used even if you complain."
  TEMPLATE="ftab.template"
else
  TEMPLATE="$NEMS/scripts/ftab.template"
fi
sed -f ftab.sed $TEMPLATE > ftab.dat
# --- Delete ftab.sed, since it is no longer needed
rm ftab.sed
ftabname="$NEMS/source/ftab.obj"
if [ -f ftab.obj ] ; then
  ftabname="ftab.obj"
  echo "Local ftab.obj found, so it will be used."
fi
ftab2name="$NEMS/source/ftab2.obj"
if [ -f ftab2.obj ] ; then
  ftab2name="ftab2.obj"
  echo "Local ftab2.obj found, so it will be used."
fi
filername="$NEMS/source/filer.obj"
if [ -f filer.obj ] ; then
  filername="filer.obj"
  echo "Local filer.obj found, so it will be used."
fi

  fwk1name="$NEMS/source/fwk1io.obj"
  cioname="$NEMS/source/cio4wk1.obj"

# --- Link FTAB   
echo $ftabname $ftab2name $filername
echo "Please wait, ftab executable is being created."
OBJSFTAB="\
$ftabname \
$ftab2name \
$filername \
$fwk1name \
$cioname \
$gdxdefname \
$gdxgluname "

 notthere=0
 for item in $OBJSFTAB
 do
    sh $NEMS/scripts/checkfor.z.sh $item print
    if [ $? -ne "0" ] ; then
      notthere=1
    fi
 done
 if [ $notthere -eq 1 ] ;then
   echo "not continuing because of missing object files"
   exit
 fi
echo $OBJSFTAB | sed 's/\//\\/g' > ftab.objs
# set the directory for link files to the current directory
TMP="$PWD"
export TMP
# re-set lnkinems if new terminal server
#comment out check for new servers since we have a new Intel version
#case $COMPUTERNAME in
#   NEM1|NEM2|NEM3|NEM4|NEM5|NEM6   )  echo "New terminal server alert!"
#                                      echo "   setting lnkinems to 3 (here this means running 64-bit Intel version 11.1)"
#                                      lnkinems=3
#                                      IVERS="11.1";export IVERS
#                                      . $NEMS/scripts/ifortvars64x64.sh ;;
#                          *        )  echo "lnkinems business as usual" ;;  
#esac
LIB="$LIB;$NEMS\\scripts"        # to find aimmslink library
if [[ $lnkinems = 1 ]] ; then
  IVERS=9.0;export IVERS
  . $NEMS/scripts/ifortvars.sh 9.0
fi
if [[ $lnkinems = 2 ]] ; then
  IVERS=9.1;export IVERS
  . $NEMS/scripts/ifortvars.sh 9.1
fi
if [[ $lnkinems = 0 ]] ; then
  link /out:ftab.exe /warn:0 /debug:full /FORCE @ftab.objs
fi
if [ $lnkinems = 4 ] ; then
  IVERS="11.1";export IVERS
  . $NEMS/scripts/ifortvars64x64.sh
  xilink /out:ftab.exe /debug /FORCE /NODEFAULTLIB:"libc.lib" /DELAYLOAD:"aimmslink.dll" @ftab.objs aimmslink64.lib delayimp64.lib
fi
if [ $lnkinems = 5 ] ; then
  IVERS="18.1";export IVERS
  . $NEMS/scripts/ifortvars18.sh
  xilink /out:ftab.exe /subsystem:console /debug /FORCE /NODEFAULTLIB:"libc.lib" /DELAYLOAD:"aimmslink.dll" @ftab.objs aimmslink64.lib delayimp64.lib legacy_stdio_definitions.lib
fi
# --- See if wish to run FTAB now
acceptable=0
while [[ $acceptable = 0 ]]
do
  echo " "
  echo -n "DO you want to run FTAB now (y/n)? "
  read RUNITNOW
  case $RUNITNOW in
       [Yy]    ) self=0
                 acceptable=1;;
       [Nn]    ) self=1
                 acceptable=1;;         
       *       ) if [[ -z "$RUNITNOW" ]]; then
                   echo " *** THIS ITEM HAS NO DEFAULT "
                 fi  
                 echo " *** UNACCEPTABLE RESPONSE - RE-ENTER ***";;
  esac     
done
if [[ "$lnkinems" = "4" || "$lnkinems" = "5" ]] ; then
  if [ ! -f gdxdclib64.dll ] ; then
    echo copying gdxdclib64.dll
    cp $NEMS/scripts/gdxdclib64.dll .
  fi
  if [ ! -f aimmslink.dll ] ; then
    echo copying aimmslink64.dll
    cp $NEMS/scripts/aimmslink64.dll aimmslink.dll
  fi

else
  if [ ! -f gdxdclib.dll ] ; then
    echo copying gdxdclib.dll
    cp $NEMS/scripts/gdxdclib.dll .
  fi
  if [ ! -f aimmslink.dll ] ; then
    echo copying aimmslink.dll
    cp $NEMS/scripts/aimmslink.dll .
  fi
fi
logstr="echo nftab $USER `date`"
#$logstr >> $NEMS/logs/ftablog
if [[ $self = 0 ]]; then
   ftab  < ftab.dat 
else
   echo " "
   echo " "
   echo "ENTER THE FOLLOWING FROM CURRENT DIRECTORY, WHEN YOU ARE READY TO RUN FTAB:"
   echo " "
   echo "   ftab < ftab.dat "   
   echo " "
   echo "Rumor has it that you really don't need the '<'."   
   echo " "
   echo " "
fi
