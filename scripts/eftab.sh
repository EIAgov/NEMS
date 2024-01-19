# $Header: m:/default/scripts/RCS/eftab.sh,v 1.15 2020/02/07 14:32:44 pkc Exp $
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
echo "*   NEMS Script to make ftab.exe      *"
echo "***************************************"
echo " "
echo " "
# See which version of code user wants
acceptable=0
  while [[ $acceptable = 0 ]]
  do
    echo " "
    echo "Select report writer to use:"
    echo "     1 - AEO 2000" 
    echo "     2 - AEO 2001" 
    echo "     3 - McIntosh I compatible"
    echo "     4 - McIntosh II (Mercury mission) compatible"
    echo "     5 - Jefferman/3 Republican compatible (current)"
    echo "     6 - AEO 2002"
    echo "     7 - AEO 2003"
    echo "     8 - McCain/Lieberman compatible"
    echo "     9 - AEO 2004"
    echo "    10 - AEO 2005"
    echo "    11 - AEO 2006"
    echo "    12 - AEO 2007"
    echo "    13 - 2007 Service Reports (S. 280, Inhofe)"
    echo "    14 - AEO 2008 Early Release (before Energy Bill)"
    echo "    15 - Revised AEO 2008 with Energy Bill"
    echo "    16 - AEO 2009 Early Release"
    echo "    17 - AEO 2009 Full Report"
    echo "    18 - AEO 2009 Updated Reference Case"
    echo "    19 - AEO 2010"
    echo "    20 - AEO 2011"
    echo "    21 - AEO 2012 Early Release"
    echo "    22 - AEO 2012 Full Report"
    echo "    23 - AEO 2013"
    echo "    24 - AEO 2014"
    echo "    25 - AEO 2015"
    echo "    26 - AEO 2016"
    echo "    27 - AEO 2017"
    echo "    28 - AEO 2018"
    echo "    29 - AEO 2019"
    echo "    30 - AEO 2020"
    echo "    31 - Current default"
    echo -n "Enter your choice (1-31) :<31>"
    read SELECTED

    case "$SELECTED" in
      1        ) reptype=aeo2k
                 echo "Setting print dollar year to 1998"
                 DEFDOLYR=1998
                 CUMCAPADD=1998
                 CUMOILGAS=1998
                 FTABCARB="c"
                 LYR=2020
                 lnkinems=0
                 acceptable=1 ;;
      2        ) reptype=aeo2001
                 echo "Setting print dollar year to 1999"
                 DEFDOLYR=1999
                 CUMCAPADD=1999
                 CUMOILGAS=1999
                 FTABCARB="c"
                 LYR=2020
                 lnkinems=0
                 acceptable=1 ;;
      3        ) reptype=mcintosh
                 DEFDOLYR=1999
                 CUMCAPADD=1999
                 CUMOILGAS=1999
                 FTABCARB="c"
                 LYR=2020
                 lnkinems=0
                 acceptable=1 ;;
      4        ) reptype=mercury
                 DEFDOLYR=1999
                 CUMCAPADD=1999
                 CUMOILGAS=1999
                 FTABCARB="c"
                 LYR=2020
                 lnkinems=0
                 acceptable=1 ;;
      5        ) reptype=jefferep
                 DEFDOLYR=1999
                 CUMCAPADD=1999
                 CUMOILGAS=1999
                 FTABCARB="c"
                 LYR=2020
                 lnkinems=0
                 acceptable=1 ;;
      6        ) reptype=aeo2002
                 echo "Setting print dollar year to 2000"
                 DEFDOLYR=2000
                 CUMCAPADD=2000
                 CUMOILGAS=2000
                 FTABCARB="c"
                 LYR=2020
                 lnkinems=0
                 acceptable=1 ;;
      7        ) reptype=aeo2003
                 echo "Setting print dollar year to 2001"
                 DEFDOLYR=2001
                 CUMCAPADD=2001
                 CUMOILGAS=2001
                 FTABCARB="c"
                 LYR=2025
                 lnkinems=0
                 acceptable=1 ;;
      8        ) reptype=mccainlieb
                 DEFDOLYR=2001
                 CUMCAPADD=2001
                 CUMOILGAS=2001
                 FTABCARB="c"
                 LYR=2025
                 lnkinems=0
                 acceptable=1 ;;
      9        ) reptype=aeo2004
                 echo "Setting print dollar year to 2002"
                 DEFDOLYR=2002
                 CUMCAPADD=2002
                 CUMOILGAS=2002
                 LYR=2025
                 lnkinems=0
                 acceptable=1 ;;
     10        ) reptype=aeo2005
                 echo "Setting print dollar year to 2003"
                 DEFDOLYR=2003
                 CUMCAPADD=2003
                 CUMOILGAS=2003
                 LYR=2025
                 lnkinems=0
                 acceptable=1 ;;
     11        ) reptype=aeo2006
                 echo "Setting print dollar year to 2004"
                 DEFDOLYR=2004
                 CUMCAPADD=2004
                 CUMOILGAS=2004
                 LYR=2030
                 lnkinems=1
                 acceptable=1 ;;
     12        ) reptype=aeo2007
                 echo "Setting print dollar year to 2005"
                 DEFDOLYR=2005
                 CUMCAPADD=2005
                 CUMOILGAS=2005
                 LYR=2030
                 lnkinems=1
                 acceptable=1 ;;
     13        ) reptype=serv2007
                 echo "Setting print dollar year to 2005"
                 DEFDOLYR=2005
                 CUMCAPADD=2005
                 CUMOILGAS=2005
                 LYR=2030
                 lnkinems=1
                 acceptable=1 ;;
     14        ) reptype=aeo2008er
                 echo "Setting print dollar year to 2006"
                 DEFDOLYR=2006
                 CUMCAPADD=2006
                 CUMOILGAS=2006
                 LYR=2030
                 lnkinems=2
                 acceptable=1 ;;
     15        ) reptype=aeo2008
                 echo "Setting print dollar year to 2006"
                 DEFDOLYR=2006
                 CUMCAPADD=2006
                 CUMOILGAS=2006
                 LYR=2030
                 lnkinems=2
                 acceptable=1 ;;
     16        ) reptype=aeo2009er
                 echo "Setting print dollar year to 2007"
                 DEFDOLYR=2007
                 CUMCAPADD=2007
                 CUMOILGAS=2007
                 LYR=2030
                 lnkinems=2
                 acceptable=1 ;;
     17        ) reptype=aeo2009
                 echo "Setting print dollar year to 2007"
                 DEFDOLYR=2007
                 CUMCAPADD=2007
                 CUMOILGAS=2007
                 LYR=2030
                 lnkinems=1
                 acceptable=1 ;;
     18        ) reptype=aeo2009up
                 echo "Setting print dollar year to 2007"
                 DEFDOLYR=2007
                 CUMCAPADD=2007
                 CUMOILGAS=2007
                 LYR=2030
                 lnkinems=2
                 acceptable=1 ;;
     19        ) reptype=aeo2010
                 echo "Setting print dollar year to 2008"
                 DEFDOLYR=2008
                 CUMCAPADD=2008
                 CUMOILGAS=2008
                 LYR=2035
                 lnkinems=2
                 acceptable=1 ;;
     20        ) reptype=aeo2011
                 echo "Setting print dollar year to 2009"
                 DEFDOLYR=2009
                 CUMCAPADD=2009
                 CUMOILGAS=2009
                 LYR=2035
                 lnkinems=3
                 fwk1name="$NEMS/objects/fwk1io.v1.10.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.9.obj"
                 IVERS="11.1";export IVERS
                 . $NEMS/scripts/ifortvars.sh
                 acceptable=1 ;;
     21        ) reptype=aeo2012er
                 echo "Setting print dollar year to 2010"
                 DEFDOLYR=2010
                 CUMCAPADD=2010
                 CUMOILGAS=2010
                 LYR=2035
                 lnkinems=3
                 fwk1name="$NEMS/objects/fwk1io.v1.10.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.9.obj"
                 IVERS="11.1";export IVERS
                 . $NEMS/scripts/ifortvars.sh
                 acceptable=1 ;;
     22        ) reptype=aeo2012
                 echo "Setting print dollar year to 2010"
                 DEFDOLYR=2010
                 CUMCAPADD=2010
                 CUMOILGAS=2010
                 LYR=2035
                 lnkinems=3
                 fwk1name="$NEMS/objects/fwk1io.v1.10.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.9.obj"
                 IVERS="11.1";export IVERS
                 . $NEMS/scripts/ifortvars.sh
                 acceptable=1 ;;
     23        ) reptype=aeo2013
                 echo "Setting print dollar year to 2011"
                 DEFDOLYR=2011
                 CUMCAPADD=2011
                 CUMOILGAS=2011
                 LYR=2040
                 lnkinems=3
                 fwk1name="$NEMS/objects/fwk1io.v1.10.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.9.obj"
                 IVERS="11.1";export IVERS
                 . $NEMS/scripts/ifortvars.sh
                 acceptable=1 ;;
     24        ) reptype=aeo2014
                 echo "Setting print dollar year to 2012"
                 DEFDOLYR=2012
                 CUMCAPADD=2012
                 CUMOILGAS=2012
                 LYR=2040
                 lnkinems=3
                 fwk1name="$NEMS/objects/fwk1io.v1.10.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.9.obj"
                 IVERS="11.1";export IVERS
                 . $NEMS/scripts/ifortvars.sh
                 acceptable=1 ;;
     25        ) reptype=aeo2015
                 echo "Setting print dollar year to 2013"
                 DEFDOLYR=2013
                 CUMCAPADD=2013
                 CUMOILGAS=2013
                 LYR=2040
                 lnkinems=4
                 fwk1name="$NEMS/objects/fwk1io.v1.11.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.10.obj"
                 IVERS="11.1";export IVERS
                 . $NEMS/scripts/ifortvars64x64.sh
                 acceptable=1 ;;
     26        ) reptype=aeo2016
# published late - base year was moved ahead by 2
                 echo "Setting print dollar year to 2015"
                 DEFDOLYR=2015
                 CUMCAPADD=2015
                 CUMOILGAS=2015
                 LYR=2040
                 lnkinems=4
                 fwk1name="$NEMS/objects/fwk1io.v1.11.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.10.obj"
                 IVERS="11.1";export IVERS
                 . $NEMS/scripts/ifortvars64x64.sh
                 acceptable=1 ;;
     27        ) reptype=aeo2017
                 echo "Setting print dollar year to 2016"
                 DEFDOLYR=2016
                 CUMCAPADD=2016
                 CUMOILGAS=2016
                 LYR=2050
                 lnkinems=4
                 fwk1name="$NEMS/objects/fwk1io.v1.11.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.10.obj"
                 IVERS="11.1";export IVERS
                 . $NEMS/scripts/ifortvars64x64.sh
                 acceptable=1 ;;
     28        ) reptype=aeo2018
                 echo "Setting print dollar year to 2017"
                 DEFDOLYR=2017
                 CUMCAPADD=2017
                 CUMOILGAS=2017
                 LYR=2050
                 lnkinems=4
                 fwk1name="$NEMS/objects/fwk1io.v1.11.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.10.obj"
                 IVERS="11.1";export IVERS
                 . $NEMS/scripts/ifortvars64x64.sh
                 acceptable=1 ;;
     29        ) reptype=aeo2019
                 echo "Setting print dollar year to 2018"
                 DEFDOLYR=2018
                 CUMCAPADD=2018
                 CUMOILGAS=2018
                 LYR=2050
                 lnkinems=5
                 fwk1name="$NEMS/objects/fwk1io.v1.12.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.11.obj"
                 IVERS="18.1";export IVERS
                 . $NEMS/scripts/ifortvars18.sh
                 acceptable=1 ;;
     30        ) reptype=aeo2020
                 echo "Setting print dollar year to 2019"
                 DEFDOLYR=2019
                 CUMCAPADD=2019
                 CUMOILGAS=2019
                 LYR=2050
                 lnkinems=5
                 fwk1name="$NEMS/objects/fwk1io.v1.12.obj"
                 cioname="$NEMS/objects/cio4wk1.v1.11.obj"
                 acceptable=1 ;;
     31        ) reptype=default
                 lnkinems=5
                 acceptable=1 ;;
      *        )  if [[ -z "$SELECTED" ]]; then
                    reptype=default
                    lnkinems=5
                    acceptable=1 
                  else
                    echo "*** UNACCEPTABLE RESPONSE - RE-ENTER ***"
                  fi;;
    esac
  done

# --- Get object file defaults, used to create ftab executable
case "$reptype" in
 mcintosh )
  ftabname="M:/rec/rsc/merc/src/ftab.mc.obj"
  ftab2name="M:/rec/rsc/merc/src/ftab2.obj"
  filername="M:/rec/rsc/merc/src/filer.mc.obj"
  ;;
 mercury )
  ftabname="M:/rec/rsc/merc/src/ftab.mc.obj"
  ftab2name="M:/rec/rsc/merc/src/ftab2.obj"
  filername="M:/rec/rsc/merc/src/filer.mc.obj"
  ;;
 jefferep )
  ftabname="M:/rec/rsc/merc/src/nox/ftab.nox.obj"
  ftab2name="M:/rec/rsc/merc/src/nox/ftab2.obj"
  filername="M:/rec/rsc/merc/src/nox/filer.mc.obj"
  ;;
 mccainlieb )
  ftabname="L:/main/pkc/jq5/publish/ftab.obj"
  ftab2name="L:/main/pkc/jq5/publish/ftab2.obj"
  filername="$NEMS/objects/filer.v1.88.obj"
  ;;
 * )
  grep "^ftab.obj objects $reptype " $NEMS/logs/DEFAULTS > temp.$$
    read junk1 junk2 junk3 vers1 junkend < temp.$$ ; rm temp.$$
  grep "^ftab2.obj objects $reptype " $NEMS/logs/DEFAULTS > temp.$$
    read junk1 junk2 junk3 vers2 junkend < temp.$$ ; rm temp.$$
  grep "^filer.obj objects $reptype " $NEMS/logs/DEFAULTS > temp.$$
    read junk1 junk2 junk3 vers3 junkend < temp.$$ ; rm temp.$$
ftabname="$NEMS/objects/ftab.v$vers1.obj"
if [ -f ftab.obj ] ; then
  ftabname="ftab.obj"
  echo "Local ftab.obj found, so it will be used."
fi
ftab2name="$NEMS/objects/ftab2.v$vers2.obj"
if [ -f ftab2.obj ] ; then
  ftab2name="ftab2.obj"
  echo "Local ftab2.obj found, so it will be used."
fi
filername="$NEMS/objects/filer.v$vers3.obj"
if [ -f filer.obj ] ; then
  filername="filer.obj"
  echo "Local filer.obj found, so it will be used."
fi
;;
esac
grep "^fwk1io.obj objects default" $NEMS/logs/DEFAULTS > temp.$$
  read junk1 junk2 junk3 vers4 junkend < temp.$$ ; rm temp.$$

grep "^cio4wk1.obj objects default" $NEMS/logs/DEFAULTS > temp.$$
  read junk1 junk2 junk3 vers5 junkend < temp.$$ ; rm temp.$$
  
grep "^gdxf9def.obj objects default" $NEMS/logs/DEFAULTS > temp.$$
  read junk1 junk2 junk3 vers6 junkend < temp.$$ ; rm temp.$$
  gdxf9def="$NEMS/objects/gdxf9def.v$vers6.obj"

grep "^gdxf9glu.obj objects default" $NEMS/logs/DEFAULTS > temp.$$
  read junk1 junk2 junk3 vers7 junkend < temp.$$ ; rm temp.$$
  gdxf9glu="$NEMS/objects/gdxf9glu.v$vers7.obj"
  
  
  
if [[ $lnkinems = 0 ]] ; then
  echo "CVF linker will be used."
  fwk1name="$NEMS/objects/fwk1io.v1.4.obj"
  cioname="$NEMS/objects/cio4wk1.v1.3.obj"
  gdxf9def=" "
  gdxf9glu=" "
elif [[ $lnkinems = 1 ]] ; then
  fwk1name="$NEMS/objects/fwk1io.v1.5.obj"
  cioname="$NEMS/objects/cio4wk1.v1.4.obj"
  gdxf9def=" "
  gdxf9glu=" "
elif [[ $lnkinems = 2 ]] ; then
  fwk1name="$NEMS/objects/fwk1io.v1.10.obj"
  cioname="$NEMS/objects/cio4wk1.v1.9.obj"
  gdxf9def="$NEMS/objects/gdxf9def.v1.3.obj"
  gdxf9glu="$NEMS/objects/gdxf9glu.v1.4.obj"
elif [[ $lnkinems = 3 ]] ; then
  fwk1name="$NEMS/objects/fwk1io.v1.10.obj"
  cioname="$NEMS/objects/cio4wk1.v1.9.obj"
  gdxf9def="$NEMS/objects/gdxf9def.v1.3.obj"
  gdxf9glu="$NEMS/objects/gdxf9glu.v1.4.obj"
elif [[ $lnkinems = 4 ]] ; then
  fwk1name="$NEMS/objects/fwk1io.v1.11.obj"
  cioname="$NEMS/objects/cio4wk1.v1.10.obj"
  gdxf9def="$NEMS/objects/gdxf9def.v1.4.obj"
  gdxf9glu="$NEMS/objects/gdxf9glu.v1.5.obj"
else
  fwk1name="$NEMS/objects/fwk1io.v$vers4.obj"
  cioname="$NEMS/objects/cio4wk1.v$vers5.obj"
  gdxf9def="$NEMS/objects/gdxf9def.v$vers6.obj"
  gdxf9glu="$NEMS/objects/gdxf9glu.v$vers7.obj"
fi
#
# --- Link FTAB   
echo $ftabname $ftab2name $filername
echo "Please wait, ftab executable is being created."
OBJSFTAB="\
$ftabname \
$ftab2name \
$filername \
$fwk1name \
$cioname \
$gdxf9def \
$gdxf9glu "

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
#case $COMPUTERNAME in
#   NEM1|NEM2|NEM3|NEM4|NEM5|NEM6   )  echo "New terminal server alert!  lnkinems becomes 3!"
#                                      lnkinems=3;;
#                                     IVERS="11.1";export IVERS
#                                     . $NEMS/scripts/ifortvars.sh ;;
#                          *        )  echo "lnkinems business as usual" ;;
#esac
LIB="$LIB;$NEMS\\scripts"        # to find aimmslink library
if [[ "$lnkinems" = "1" ]] ; then
  IVERS=9.0;export IVERS
  . $NEMS/scripts/ifortvars.sh 9.0
fi
if [[ "$lnkinems" = "2" ]] ; then
  IVERS=9.1;export IVERS
  . $NEMS/scripts/ifortvars.sh 9.1
fi
if [ "$lnkinems" = "0" ] ; then
  link /out:ftab.exe /warn:0 /debug:full /FORCE @ftab.objs
elif [ "$lnkinems" = "4" ] ; then
  IVERS="11.1";export IVERS
  . $NEMS/scripts/ifortvars64x64.sh
  xilink /out:ftab.exe /debug /FORCE /NODEFAULTLIB:"libc.lib" /DELAYLOAD:"aimmslink.dll" @ftab.objs aimmslink64.lib delayimp64.lib
elif [ "$lnkinems" = "5" ] ; then
  IVERS="18.1";export IVERS
  . $NEMS/scripts/ifortvars18.sh
  xilink /out:ftab.exe /subsystem:console /debug /FORCE /NODEFAULTLIB:"libc.lib" /DELAYLOAD:"aimmslink.dll" @ftab.objs aimmslink64.lib delayimp64.lib legacy_stdio_definitions.lib
else
  xilink /out:ftab.exe /debug /FORCE /NODEFAULTLIB:"libc.lib" /DELAYLOAD:"aimmslink.dll" @ftab.objs aimmslink.lib delayimp.lib
fi
# copy dll required for GDX and AIMMS interfaces
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
