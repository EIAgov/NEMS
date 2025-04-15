call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022
::C:\python_environments\aeo2025_py311_D\Scripts\activate

ifort ecp_row_col.f90 efd_row_col.f90 fsqlite.f90 util_tools_prepro.f90 gdxf9def.f filemgr.f filer.f udaf.f uread.f cio4wk1.f nemswk1.f fwk1io.f dummypp.f udat.f uldsm.f prepplt.f prepett.f uesql.f90 /free /Qzero /assume:nounderscore /include:../includes /compile-only /nopdbfile /traceback /fpconstant /assume:byterecl /assume:source_include /nolist /static /Qsave /heap-arrays0
ifort /exe:prepplt.exe gdxf9def.obj gdxf9glu.obj filemgr.obj filer.obj udat.obj udaf.obj uldsm.obj prepplt.obj util_tools_prepro.obj cio4wk1.obj nemswk1.obj fwk1io.obj uread.obj uesql.obj dummypp.obj libfsqlite.lib 
ifort /exe:prepett.exe gdxf9def.obj gdxf9glu.obj filemgr.obj filer.obj udat.obj udaf.obj uldsm.obj prepett.obj util_tools_prepro.obj cio4wk1.obj nemswk1.obj fwk1io.obj uread.obj uesql.obj dummypp.obj libfsqlite.lib 