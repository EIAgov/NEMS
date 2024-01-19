MPS="C:\oml\oml64"
export MPS
OMLLIBS=" $MPS\OMLDB32.LIB $MPS\OMLLIB32.LIB $MPS\OMLLP32.LIB $MPS\OMLWIN32.LIB $MPS\OMLXPRS.LIB"
OMLLIBS=" $MPS\omldb64.lib $MPS\omllib64.lib $MPS\omllp64.lib $MPS\omlwin64.lib $MPS\omlxprs64.lib"
pack_objs="pack_all omlanal.obj"
xilink /out:pack_all.exe /incremental:no /debug /map:pack.map /FORCE $pack_objs $OMLLIBS
