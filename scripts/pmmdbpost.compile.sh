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


. M:/default/scripts/ifortvars.sh

FLAGS="/debug:full /optimize:0  /free /check:bounds /traceback /assume:byterecl"
"${COMPILER}" ${FLAGS} /include:'M:\default\includes\'  /exe:pmmdbpost.exe pmmdb.post.f f90sql.lib 2>&1 | tee ERROR
  

print ' done with compile/link step begin execution '

