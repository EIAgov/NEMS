#  Sets the environment strings for Intel Fortran and MS Visual Studio linker
# This version selects the "intel64" version of the compiler, linker, libraries, and incldues.

# convert PATH from unix to mixed windows (ie, forward slashes, long name format)
  if [ "$SH" = "bash" ] ; then
    PATHDOS=`cygpath -pml "$PATH"`
  else
    PATHDOS=${PATH}
    export PATHDOS=$(echo -E $PATH | sed 's@\\@\/@g')
  fi

# assumes microsoft visual studo 17.7.4 (2022) for link, etc. Set path to find Visual Studio Common7/Tools and Common7/IDE
  export VSINSTALLDIR='C:\Program Files\Microsoft Visual Studio\2022\Community'
  export VCINSTALLDIR=$VSINSTALLDIR'\VC'
  export WINDOWSSDKINSTALLDIR='C:\Program Files (x86)\Windows Kits\10'
  export WINDOWSSDKVERSION='10.0.22621.0'
  export LIB=$VCINSTALLDIR'\Tools\MSVC\14.38.33130\lib\x64;'$VSINSTALLDIR'\VC\Tools\MSVC\14.38.33130\lib\x64;'$VSINSTALLDIR'\VC\Tools\MSVC\14.38.33130\atlmfc\lib\x64;'$WINDOWSSDKINSTALLDIR'\Lib\'$WINDOWSSDKVERSION'\um\x64;'$WINDOWSSDKINSTALLDIR'\Lib\'$WINDOWSSDKVERSION'\ucrt\x64;'$LIB
  export INCLUDE=$VCINSTALLDIR'\VC\Tools\MSVC\14.38.33130\atlmfc\include;'$VCINSTALLDIR'\Tools\MSVC\14.38.33130\include;'$VSINSTALLDIR'\include;'$INCLUDE
 
  export VSINSTALLDIR='C:/Program Files/Microsoft Visual Studio/2022/Community'
  export VCINSTALLDIR=$VSINSTALLDIR'/VC'
  export PATHDOS=$VSINSTALLDIR'/Common7/IDE;'$VSINSTALLDIR'/VC/Tools/MSVC/14.38.33130/lib/x64;'$VSINSTALLDIR'/Common7/Tools;'$VCINSTALLDIR'/Tools/MSVC/14.38.33130/bin/Hostx64/x64;'$WINDOWSSDKINSTALLDIR'/bin/x64;'$PATHDOS

# assumes "Intel 2021.10.0 build 20230609" for fortran version
  export IFORT_COMPILER21='C:\Program Files (x86)\Intel\oneAPI\compiler\2023.2.1\windows'
  export LIB=$IFORT_COMPILER21'\compiler\lib\intel64_win;'$LIB
# for kernel32.lib:
  export LIB=$LIB';C:\Program Files (x86)\Windows Kits\10\Lib\10.0.22621.0\um\x64'
  export INCLUDE=$IFORT_COMPILER21'\compiler\include;'$IFORT_COMPILER21'\Include;'$IFORT_COMPILER21'\compiler\include\intel64;'$INCLUDE
  export INTEL_LICENSE_FILE='C:\Program Files (x86)\Intel\oneAPI\licensing\2023.2.0'

  export IFORT_COMPILER21='C:/Program Files (x86)/Intel/oneAPI/compiler/2023.2.1/windows'
  export PATHDOS=$IFORT_COMPILER21'/bin;'$IFORT_COMPILER21'/bin/intel64;'$PATHDOS


if [ "$SH" = "bash" ] ; then
# Convert from windows to unix path convention for cygwin's bash shell
  PATH=`cygpath -pu "$PATHDOS"`
else
  PATH=$(echo -E $PATHDOS | sed 's@\/@\\@g')
fi
