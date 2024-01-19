# $Header: M:/default/scripts/RCS/ifortvars.sh,v 1.12 2011/11/17 16:25:13 dsa Exp $
# Sets the environment strings for Intel Fortran and ".net" (linker and libraries).
#  This is based on two bat files that implement the Intel Fortan command line environment,
#  vsvars32.bat and ifortvars.sh.  Naming conventions are different under Win XP 64-bit so
#  check for that.
if [ -a "c:/program files (x86)/intel" ] ; then
  . $NEMS/scripts/ifortvars64.sh
else
  if [ "$SH" = "bash" ] ; then
    PATHDOS=`cygpath -pml "$PATH"`
  else
    PATHDOS=${PATH}
    export PATHDOS=$(echo -E $PATH | sed 's@\\@\/@g')
  fi
  if [ "$IVERS" = "11.1" ] ; then
    if [ -d "c:/PROGRAM FILES/Microsoft Visual Studio .NET 2003" ] ; then
      #echo "assume .net 2003 for linker"
      export VSINSTALLDIR='C:\PROGRAM FILES\Microsoft Visual Studio .NET 2003\Common7\IDE'
      export VCINSTALLDIR='C:\PROGRAM FILES\Microsoft Visual Studio .NET 2003'
      export FrameworkDir='C:\WINDOWS\Microsoft.NET\Framework'
      export FrameworkVersion='v1.1.4322'
      export FrameworkSDKDir='C:\PROGRAM FILES\Microsoft Visual Studio .NET 2003\SDK\v1.1'
      export DevEnvDir=$VSINSTALLDIR
      export MSVCDir=$VCINSTALLDIR'\VC7'
      export PATHDOS=$DevEnvDir';'$MSVCDir'/BIN;'$VCINSTALLDIR'/Common7/Tools;'$VCINSTALLDIR'/Common7/Tools/bin/prerelease;'$VCINSTALLDIR'/Common7/Tools/bin;'$FrameworkSDKDir'/bin;'$FrameworkDir'/'$FrameworkVersion';'$PATHDOS';'
      export LIB=$MSVCDir'\ATLMFC\LIB;'$MSVCDir'\LIB;'$MSVCDir'\PlatformSDK\lib\prerelease;'$MSVCDir'\PlatformSDK\lib;'$FrameworkSDKDir'\lib;'$LIB
      export INCLUDE=$MSVCDir'\ATLMFC\INCLUDE;'$MSVCDir'\INCLUDE;'$MSVCDir'\PlatformSDK\include\prerelease;'$MSVCDir'\PlatformSDK\include;'$FrameworkSDKDir'\include;'$INCLUDE
    else
      #echo "assumed microsoft visual studo 9.0 for linker"
      export VSINSTALLDIR='C:\PROGRA~2\Microsoft Visual Studio 9.0'
      export VCINSTALLDIR=$VSINSTALLDIR'\VC'
      export PATHDOS=$VSINSTALLDIR'/Common7/IDE;'$VCINSTALLDIR'/BIN;'$VSINSTALLDIR'/Common7/Tools;'$VSINSTALLDIR'/Common7/Tools/BIN;'$VCINSTALLDIR'/PlatformSDK/bin;'$PATHDOS
      export LIB=$VCINSTALLDIR'\atlmfc\lib;'$VCINSTALLDIR'\lib;'$VCINSTALLDIR'\PlatformSDK\lib;'$LIB
      export INCLUDE=$VCINSTALLDIR'\atlmfc\include;'$VCINSTALLDIR'\include;'$VCINSTALLDIR'\PlatformSDK\include;'$INCLUDE
    fi 
    #echo "Intel 11.1 build 065"
    export IFORT_COMPILER11='C:\PROGRA~2\Intel\Compiler\11.1\065'
    export INTEL_LICENSE_FILE='C:\PROGRA~2\Common Files\Intel\Licenses'
    export PATHDOS=$IFORT_COMPILER11'/Bin/Ia32;'$PATHDOS
    export LIB=$IFORT_COMPILER11'\Lib\Ia32;'$LIB
    export INCLUDE=$IFORT_COMPILER11'\Include;'$IFORT_COMPILER11'Include\ia32;'$INCLUDE
  else
  
    #
    # 1) set environment as done in vsvars32.bat, translated to kornshell syntax
    export VSINSTALLDIR='C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE'
    export VCINSTALLDIR='C:\Program Files\Microsoft Visual Studio .NET 2003'
    export FrameworkDir='C:\WINDOWS\Microsoft.NET\Framework'
    export FrameworkVersion='v1.1.4322'
    export FrameworkSDKDir='C:\Program Files\Microsoft Visual Studio .NET 2003\SDK\v1.1'
    export DevEnvDir=$VSINSTALLDIR
    export MSVCDir=$VCINSTALLDIR'\VC7'
    export PATHDOS=$DevEnvDir';'$MSVCDir'/BIN;'$VCINSTALLDIR'/Common7/Tools;'$VCINSTALLDIR'/Common7/Tools/bin/prerelease;'$VCINSTALLDIR'/Common7/Tools/bin;'$FrameworkSDKDir'/bin;'$FrameworkDir'/'$FrameworkVersion';'$PATHDOS';'
    export LIB=$MSVCDir'\ATLMFC\LIB;'$MSVCDir'\LIB;'$MSVCDir'\PlatformSDK\lib\prerelease;'$MSVCDir'\PlatformSDK\lib;'$FrameworkSDKDir'\lib;'$LIB
    export INCLUDE=$MSVCDir'\ATLMFC\INCLUDE;'$MSVCDir'\INCLUDE;'$MSVCDir'\PlatformSDK\include\prerelease;'$MSVCDir'\PlatformSDK\include;'$FrameworkSDKDir'\include;'$INCLUDE

    # 2) set envirnoment as done in ifortvars.bat
   
    #echo 'Intel(R) Visual Fortran Compiler 9.1 Build Environment for 32-bit applications'
    #echo 'Copyright (C) 1985-2005 Intel Corporation. All rights reserved.'
    if [ "$IVERS" = "11.1" ] ; then
      #echo "Intel 11.1 build 065"
      export IFORT_COMPILER11='C:\Program Files\Intel\Compiler\11.1\065'
      export INTEL_LICENSE_FILE='C:\Program Files\Common Files\Intel\Licenses'
      export PATHDOS=$IFORT_COMPILER11'/Bin/Ia32;'$PATHDOS
      export LIB=$IFORT_COMPILER11'\Lib\Ia32;'$LIB
      export INCLUDE=$IFORT_COMPILER11'\Include;'$IFORT_COMPILER11'Include\ia32;'$INCLUDE
    elif [ "$IVERS" = "11.0" ] ; then
      #echo "Intel 11.0 build 074"
      export IFORT_COMPILER11='C:\Program Files\Intel\Compiler\11.0\074\fortran'
      export INTEL_LICENSE_FILE='C:\Program Files\Common Files\Intel\Licenses'
      export PATHDOS=$IFORT_COMPILER11'/Bin/Ia32;'$PATHDOS
      export LIB=$IFORT_COMPILER11'\Lib\Ia32;'$LIB
      export INCLUDE=$IFORT_COMPILER11'\Include;'$IFORT_COMPILER11'Include\ia32;'$INCLUDE
    elif [ "$IVERS" = "9.1" ] ; then
      #echo "Intel 9.1"
      export IFORT_COMPILER91='C:\Program Files\Intel\Compiler\Fortran\9.1'
      export INTEL_SHARED='C:\Program Files\Common Files\Intel\Shared Files'
      export INTEL_LICENSE_FILE='C:\Program Files\Common Files\Intel\Licenses'
      export PATHDOS=$IFORT_COMPILER91'/Ia32/Bin;'$INTEL_SHARED'/Ia32/Bin;'$PATHDOS
      export LIB=$IFORT_COMPILER91'\Ia32\Lib;'$LIB
      export INCLUDE=$IFORT_COMPILER91'\Ia32\Include;'$INCLUDE
    elif [ "$IVERS" = "9.0" ] ; then
      #echo "Intel 9.0"
      export IFORT_COMPILER90='C:\Program Files\Intel\Compiler\Fortran\9.0'
      export INTEL_SHARED='C:\Program Files\Common Files\Intel\Shared Files'
      export INTEL_LICENSE_FILE='C:\Program Files\Common Files\Intel\Licenses'
      export PATHDOS=$IFORT_COMPILER90'/Ia32/Bin;'$INTEL_SHARED'/Ia32/Bin;'$PATHDOS
      export LIB=$IFORT_COMPILER90'\Ia32\Lib;'$LIB
      export INCLUDE=$IFORT_COMPILER90'\Ia32\Include;'$INCLUDE
    fi
  fi
  if [ "$SH" = "bash" ] ; then
  # Convert from windows to unix path convention for cygwin's bash shell
    PATH=`cygpath -pu "$PATHDOS"`
  else
    PATH=$(echo -E $PATHDOS | sed 's@\/@\\@g')
  fi
fi