# runoml:  script to compile and run an oml-based fortran program. 
# Requires a fortran file argument and compiles it in free form, and links
# to necessary oml and windows libraries
#===================================================================
 if [ "$1" = "" ] ; then
  echo "provide a fortran file name as argument to this script."
  exit
 fi
 if [ -f "$1" ] ; then
  MPS="N:\default\oml\vers4lib"
  OMLVERS="vers4lib"
  echo  "$LIB \n" | grep -i "$OMLVERS" >nul
  if [ $? -eq 1 ] ; then
    LIB="$NEMS\\OML\\$OMLVERS;$LIB"
  fi

  OMLLIBS=" OMLDB32.LIB OMLLIB32.LIB OMLLP32.LIB OMLWIN32.LIB CRP32DLL.LIB user32.lib gdi32.lib"
  pwd
  ls -l *

  echo "OMLLIBS=" $OMLLIBS
  NEMSINCL="$NEMS/includes"
  echo "NEMSINCL=" $NEMSINCL

  f90 /free /include:"${NEMSINCL} " $1  user32.lib gdi32.lib $OMLLIBS		

  exn=`basename $1`
  exn=${exn%\.*}
  exn="$exn.exe"
  echo $exn
  $exn
 else
  echo "$1 was not found."
 fi
#===================================================================
