  IVERS="11.1";export IVERS
  if [ -a "c:/program files (x86)/intel" ] ; then
    echo "64-bit windows."
    COMPILER="C:/Program Files (x86)/Intel/Compiler/11.1/065/bin/Ia32/ifort.exe";export COMPILER
    . $NEMS/scripts/ifortvars64.sh
  else
    COMPILER="C:/Program Files/Intel/Compiler/11.1/065/bin/Ia32/ifort.exe";export COMPILER
    . $NEMS/scripts/ifortvars.sh
  fi
  FLAGIO='/fpscomp:ldio_spacing';export FLAGIO

# set the directory for link files to the current directory
TMP="$PWD"
export TMP

 . $NEMS/scripts/ifortvars.sh

FLAGS="/debug:full /optimize:0  /free /check:bounds /traceback /assume:byterecl"
INCSa=$NEMS/includes/
LIBSa=$NEMS/source/f90sql.lib
 

echo $INCSa | sed 's/\/\//\\\\/g;s/\//\\/g' > temp.z
INCS=`cat temp.z`

echo $LIBSa | sed 's/\/\//\\\\/g;s/\//\\/g' > temp.z
LIBS=`cat temp.z`
rm temp.z

export INCS 
export LIBS 

"${COMPILER}" ${FLAGS}  /include:$INCS /exe:udbp.exe udbp.f $LIBS 2>&1 | tee ERROR

echo ' done with compile/link step begin execution '   