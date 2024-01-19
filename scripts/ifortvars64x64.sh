#  Sets the environment strings for Intel Fortran and MS linker
# This version is variation of ifortvars64.sh to select the "x64" version of the compiler, linker, libraries, and incldues.

# convert PATH from unix to mixed windows (ie, forward slashes, long name format)
  if [ "$SH" = "bash" ] ; then
    PATHDOS=`cygpath -pml "$PATH"`
  else
    PATHDOS=${PATH}
    export PATHDOS=$(echo -E $PATH | sed 's@\\@\/@g')
  fi

# assumea microsoft visual studo 9.0 for linker"
  export VSINSTALLDIR='C:\PROGRA~2\Microsoft Visual Studio 9.0'
  export VCINSTALLDIR=$VSINSTALLDIR'\VC'

  export PATHDOS=$VSINSTALLDIR'/Common7/IDE;'$VCINSTALLDIR'/BIN/X86_amd64;'$VSINSTALLDIR'/Common7/Tools;'$VSINSTALLDIR'/Common7/Tools/BIN;'$VCINSTALLDIR'/PlatformSDK/bin;'$PATHDOS

  export LIB=$VCINSTALLDIR'\lib\amd64;'$VCINSTALLDIR'\PlatformSDK\lib\x64;'$LIB
  export INCLUDE=$VCINSTALLDIR'\atlmfc\include;'$VCINSTALLDIR'\include;'$VCINSTALLDIR'\PlatformSDK\include;'$INCLUDE

# assumes "Intel 11.1 build 065" for fortran version
  export IFORT_COMPILER11='C:\PROGRA~2\Intel\Compiler\11.1\065'
  export INTEL_LICENSE_FILE='C:\PROGRA~2\Common Files\Intel\Licenses'

  export PATHDOS=$IFORT_COMPILER11'/mkl/em64t/bin;'$IFORT_COMPILER11'/Bin/intel64;'$PATHDOS

  export LIB=$IFORT_COMPILER11'\mkl\em64t\lib;'$IFORT_COMPILER11'\Lib\intel64;'$LIB
  export INCLUDE=$IFORT_COMPILER11'\mkl\Include;'$IFORT_COMPILER11'\Include;'$IFORT_COMPILER11'Include\intel64;'$INCLUDE

if [ "$SH" = "bash" ] ; then
# Convert from windows to unix path convention for cygwin's bash shell
  PATH=`cygpath -pu "$PATHDOS"`
else
  PATH=$(echo -E $PATHDOS | sed 's@\/@\\@g')
fi
  
