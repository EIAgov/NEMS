#$Header: m:/default/scripts/RCS/nfort.sh,v 1.39 2019/02/25 15:55:50 pkc Exp $
TK_ECHO_USE_BASH_BEHAVIOR=1
  
### Script Functionality: To compile NEMS FORTRAN source file and 
###                       to update complog files.

###  First, check to see if NEMS is locked

if [[ -a "$NEMS/freeze/freeze.defaults" ]]; then
 echo "NEMS Configuration Management System is temporarily disabled by:"
 cat $NEMS/freeze/freeze.defaults
 exit
fi

FLAGB="/check:bounds"
export FLAGB
FLAGO="/debug:full /Od"  # ensure optimization default is set to debug
export FLAGO
#FLAGS1="/compile-only /nopdbfile /free /traceback /fpconstant /assume:byterecl /assume:source_include /nolist /static /Qsave /Qzero /heap-arrays0"
#export FLAGS1
#FLAGI="/include:. /include:../includes"
#export FLAGI

### functions

Header()
{
 echo " "
 echo "#################################################"
 echo "#   NEMS Fortran Compilation Utility nfort      #"
 echo "#                                               #"
 echo "#   script usage: nfort                         #"
 echo "#          or   : nfort filename.f              #"
 echo "#################################################"
 echo " "
} #End of Header()

### trap
trap " if [ -f makefile.$$ ];then mv -f  makefile.$$ makefile;fi; \
       rm -f *.$$;rm -f temp.z;exit;" 0 1 2 15
if [ "$1" != "" ] ; then
  echo "Optional Argument 1: specify a fortran file  : $1"
  if [ "$2" != "" ] ; then
    echo "Optional Argument 2: override default include path: $2"
  fi
fi
echo 

IVERS="18.1";export IVERS
COMPILER="C:/Progra~2/Intel/oneAPI/compiler/2023.2.1/windows/bin/intel64/ifort.exe";export COMPILER
. $NEMS/scripts/ifortvars18.sh
FLAGIO='/fpscomp:ldio_spacing';export FLAGIO

# check for /nocheck flag in makefile.  if not there, ask to replace
 if [ -f makefile ]; then 
    if [ $NEMS/scripts/makefile -nt makefile ]; then
       echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
       echo "We've updated the makefile.  Copying new default..."
       cp $NEMS/scripts/makefile .
       echo "If you've manually changed your makefile, you'll have to update the new one, but rest assured I'm done copying over it."
       echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    fi
 else
   echo "No makefile.  Using the default one..."
   echo -n "    Press <enter> to continue ";read notused
   cp $NEMS/scripts/makefile .
 fi

FileSelected="FALSE"
#####
#if [[ ! -z "$1" ]]; then
# case "$1" in  
#  *\.f|*\.f90)
#   grep "^$1" $NEMS/logs/DEFAULTS | fgrep source 1>/dev/null 2>/dev/null
#   if [[ "$?" -eq 0 ]]; then FileSelected="TRUE"; fi
#    ;;
# esac
#fi

if [[ "$FileSelected" = "FALSE" ]]; then

### process FORTRAN files
ls -1  *.f > Dot_F_Files.$$  2>/dev/null
ls -1  *.f90 >> Dot_F_Files.$$  2>/dev/null
awk '{n++; printf("%3.0f) %s\n",n,$1)};' Dot_F_Files.$$ > frtfiles.$$

### determine number of FORTRAN source files
wc -l frtfiles.$$ >temp2.$$; read fnumber dummy < temp2.$$

### prepare main menu
echo " " > frtmenu.$$
echo "You may compile any of the following files:" >> frtmenu.$$
echo " " >> frtmenu.$$
if [[ `cat frtfiles.$$ | wc -l` -gt 10 ]]
then
 cat frtfiles.$$ | paste - - - - >> frtmenu.$$
else
 cat frtfiles.$$ >> frtmenu.$$
fi
echo " -----------------------" >> frtmenu.$$
echo "  a) Compile all of the above" >> frtmenu.$$
echo "  v) Read error messages " >> frtmenu.$$
echo "  q) Quit" >> frtmenu.$$
echo "  \n  Type an O (letter) before any compile option to use optimization level 2," >>frtmenu.$$
echo "  e.g. O1 to compile the first source code or Oa to compile all of them." >>frtmenu.$$ 
echo " " >>frtmenu.$$
echo "  ****** Note **** B for bounds checking is now the default option." >>frtmenu.$$ 
echo " " >> frtmenu.$$
echo "  a) Compile all of the above" >> frtfiles.$$
echo "  v) Read error message " >> frtfiles.$$
echo "  q) Quit" >> frtfiles.$$
echo "  Prepend an O before any compile option to use optimization level 2" >>frtfiles.$$
echo "  e.g. O1 or Oa " >>frtfiles.$$ 
else
FileSelected="TRUE"
fi
####

### main loop
while [ 1 ]
do
# create variable NEMSINCL with the default location for nems include files . change to backslash
# format for the f90 command syntax
# 
if [ -z "$2" ] ; then
  NEMSINCLa="$NEMS/includes"
else
  echo "$2"
  NEMSINCLa="$2"
fi

echo $NEMSINCLa | sed 's/\/\//\\\\/g;s/\//\\/g' > temp.z
NEMSINCL=`cat temp.z`
rm temp.z

export NEMSINCL 
echo $NEMSINCL
####
if [[ "$FileSelected" = "FALSE" ]]; then

 ### header
 Header

 ### display main menu
 cat frtmenu.$$

 ### to prompt user to enter selection
 if [[ "$errmsg" -eq 1 ]];then echo "*** Invalid choice!";fi
 if [[ "$errmsg" -eq 2 ]];then echo "*** Error in compilation!";fi
 if [[ "$errmsg" -eq 3 ]];then echo "*** Compilation is done.";fi
 echo -n "Enter your choice (1|2|...|a|v|q)[q] ==> ";read choice
 if [[ -z "$choice" || "$choice" = "q" ]];then 
    echo " ";
    rm -f *.$$
    exit;
  fi
 case "$choice" in
  [0-9]*)
   if [[ "$choice" -gt 0 && "$choice" -le "$fnumber" ]];
   then
    errmsg=0
    FLAGB="/check:bounds"
    FLAGO="/debug:full /Od"  
    grep "$choice[)]" frtfiles.$$ > temp.$$; sed "s/[ 0-9]*[)] //" temp.$$ > temp2.$$; read item dummy < temp2.$$
    rm -f temp.$$ temp2.$$
   else
    errmsg=1
    continue
   fi
   ;;
  O[0-9]*)
   choice=${choice#O}
   if [[ "$choice" -gt 0 && "$choice" -le "$fnumber" ]];
   then
    errmsg=0
    FLAGO="/nodebug /O2"
    FLAGB=" "
    grep "$choice[)]" frtfiles.$$ > temp.$$; sed "s/[ 0-9]*[)] //" temp.$$ >temp2.$$; read item dummy < temp2.$$
    rm -f temp.$$ temp2.$$
   else
    errmsg=1
    continue
   fi
   ;;
  B[0-9]*)
   choice=${choice#B}
   if [[ "$choice" -gt 0 && "$choice" -le "$fnumber" ]];
   then
    errmsg=0
    FLAGO="/debug:full /Od"  
    FLAGB="/check:bounds"
    grep "$choice[)]" frtfiles.$$ > temp.$$; sed "s/[ 0-9]*[)] //" temp.$$ >temp2.$$; read item dummy < temp2.$$
    rm -f temp.$$ temp2.$$
   else
    errmsg=1
    continue
   fi
   ;;
 
  a)
   item="a"
    FLAGB="/check:bounds"
    FLAGO="/debug:full /Od"  
   errmsg=0
   ;;
  Oa)
   item="a"
    FLAGB=" "
   FLAGO="/nodebug /O2"  
   errmsg=0
   ;;
  Ba)
   item="a"
   FLAGB="/check:bounds"  
   FLAGO="/debug:full /Od"  
   errmsg=0
   ;;
  v)
   more -P "---More--- press q to exit;" ERROR
   continue
   ;;
  *)
   errmsg=1
   FLAGB="/check:bounds"
   FLAGO="/debug:full /Od"  
   continue
   ;;
 esac
else
 FLAGB="/check:bounds"
 echo "Compiling $1 ... "
 item=$1
fi
####

 ## process user's input
 case "$item" in  
  *\.f)
     item2="${item%.f}.obj"
     ;;
   *\.f90)
     item2="${item%.f90}.obj"
     ;;
 esac
 case "$item" in  
  *\.f|*\.f90)
   touch $item
   echo " "
   if [[ "$FLAGB" = "/check:bounds" ]] ; then
     echo "Compiling ${item} with bounds checking on"
   fi
   export FLAGB
   if [[ "$FLAGO" = "/debug:full /Od" ]] ; then
     echo "Compiling ${item} with debug and no optimization "
   elif  [[ "$FLAGO" = "/nodebug /O2" ]] ; then
     echo "Compiling ${item} with optimization level 2"
   else 
     echo "optimization is $FLAGO "
   fi 
   export FLAGO
   
#    echo "compiler" $COMPILER 
#	 echo "flagb" $FLAGB 
#	 echo "flago" $FLAGO 
#	 echo "flags1" $FLAGS1 
#	 echo "flagio" 	$FLAGIO 
#	 echo "flagi" $FLAGI 
#	 NEMSINCL="w:/AEO2021_git/includes"
#	 echo "nemsincl" /include:$NEMSINCL 
#	 echo "item2" $item2
#	 echo "item" $item
#     $COMPILER $FLAGB $FLAGO $FLAGS1 $FLAGIO $FLAGI /include:$NEMSINCL $item
   make $item2 
 

   grep -i "error" ERROR 1>/dev/null 2>/dev/null
   echo " "
   echo -n "Compilation is done, press RETURN key to continue ... ";read answer
#####
if [[ "$FileSelected" = "TRUE" ]]; then exit ; fi
#####
   ;;
  a)
   if [[ -f ERROR ]];then rm -f ERROR;fi
   export FLAGB
   export FLAGO
   let leftnumber=$fnumber-1
   while read file
   do
    touch $file
    case "$file" in
      *\.f)
        fileobj="${file%.f}.obj"
        filecomplog="complog_${file%.f}"
        ;;
      *\.f90)
        fileobj="${file%.f90}.obj"
        filecomplog="complog_${file%.f90}"
        ;;
    esac
    echo " "
    echo "Compiling $file now with $FLAGO and $FLAGB ... $leftnumber more file(s) left"
#    $COMPILER $FLAGB $FLAGO $FLAGS1 $FLAGIO $FLAGI /include:$NEMSINCL $file	
    $NEMS/scripts/make "$fileobj" 
    if [[ -f ERROR ]];then cat ERROR >> ERROR.$$;fi
     grep -i "error" ERROR 1>/dev/null 2>/dev/null
    let leftnumber=leftnumber-1
   done < Dot_F_Files.$$
   if [ -f make1.$$ ] ; then
     mv make1.$$ makefile
   fi
   mv -f ERROR.$$ ERROR
   echo " "
   echo -n "Compilations are done, press RETURN key to continue ... ";read answer
   grep -i "error" ERROR 1>/dev/null 2>/dev/null
   if [[ "$?" -eq 0 ]];then errmsg=2;else errmsg=3;fi
   continue
   ;;
  *)
   continue
   ;;
 esac
### end of main loop
done
 rm -f Dot_F_Files.$$
