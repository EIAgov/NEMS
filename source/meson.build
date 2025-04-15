project('NEMSpyfiler',
        ['c', 'fortran'],
        version : '2025.7',
        meson_version: '>= 1.1.0',
        default_options : [
                            'warning_level=1',
                            'buildtype=debugoptimized',
                                'debug=false',
                                'optimization=2'
                          ])

  
  add_project_arguments('/free', language: 'fortran')
  add_project_arguments('/traceback', language: 'fortran')
  add_project_arguments('/Qzero', language: 'fortran')
  add_project_arguments('/Qsave', language: 'fortran')
  add_project_arguments('/names:lowercase', language: 'fortran')
  add_project_arguments('/assume:underscore', language: 'fortran')
  add_project_arguments('/include:/includes', language: 'fortran')
  add_project_arguments('/assume:byterecl', language: 'fortran')
  add_project_arguments('/assume:source_include', language: 'fortran')
  add_project_arguments('/nolist', language: 'fortran')
  add_project_arguments('/static', language: 'fortran')
  add_project_arguments('/heap-arrays0', language: 'fortran')
  add_project_arguments('/check:bounds', language: 'fortran')
  add_project_arguments('/debug:full', language: 'fortran')
  add_project_arguments('/fpconstant', language: 'fortran')


py = import('python').find_installation(pure: false)
py_dep = py.dependency()

incdir_numpy = run_command(py,
  ['-c', 'import os; os.chdir(".."); import numpy; print(numpy.get_include())'],
  check : true
).stdout().strip()

incdir_f2py = run_command(py,
    ['-c', 'import os; os.chdir(".."); import numpy.f2py; print(numpy.f2py.get_include())'],
    check : true
).stdout().strip()

inc_np = include_directories(incdir_numpy)
includes_files =include_directories ('../includes')
src = include_directories('.')
compl = include_directories('C:/Program Files (x86)/Intel/oneAPI/compiler/2023.2.1/windows/redist/intel64_win/compiler')
xpress= include_directories('C:/xpress8_12/bin')
np_dep = declare_dependency(include_directories: inc_np)

incdir_f2py = incdir_numpy / '..' / '..' / 'f2py' / 'src'
inc_f2py = include_directories(incdir_f2py)
fortranobject_c = incdir_f2py / 'fortranobject.c'

inc_np = include_directories(incdir_numpy, incdir_f2py)

py.extension_module('pyfiler1',
'pyfiler.f90',
'pyfiler1module.c',
'pyfiler1-f2pywrappers.f',
'pyfiler1-f2pywrappers2.f90',
'util_ngpl_stat_price.f90',
'util_call_pyd1_module.f90',
'util_tools.f90',
'util_refine.f90',
'util_cvtest_relax.f90',
'util_docvfacts.f90',
'util_sumqas.f90',
'scedes_reader.f90',
'util_copyadjusted.f90',
'util_checkfor_nanq.f90',
'util_avepas_avepasa.f90',
'util_minit.f90',
'..\source/mac.f',
'..\source/resd.f',
'..\source/tranfrt.f',
'..\source/tranair.f',
'..\source/tran.f',
'..\source/comm.f',
'..\source/refine.f',
'..\source/world.f',
'..\source/ind.f',
'..\source/filemgr.f',
'..\source/gdxf9def.f',
'..\source/cio4wk1.f',
'..\source/fwk1io.f',
'..\source/filer.f',
'..\source/nemswk1.f',
					fortranobject_c,
					include_directories: [inc_np, includes_files,src,compl],
					 objects: [ 'gdxf9glu.o', 'libfsqlite.a'],
                     dependencies : [
                     py_dep,					 
                     ],
                     install : true)

py.extension_module('pyfiler2',
'pyfiler.f90',
'pyfiler2module.c',
'pyfiler2-f2pywrappers.f',
'pyfiler2-f2pywrappers2.f90',
'arraycode-re.f90',
'util_ngpl_stat_price.f90',
'util_call_pyd2_module.f90',
'util_tools.f90',
'util_refine.f90',
'util_cvtest_relax.f90',
'util_docvfacts.f90',
'util_copyadjusted.f90',
'util_checkfor_nanq.f90',
'util_sumqas.f90',
'util_avepas_avepasa.f90',
'util_minit.f90',
'scedes_reader.f90',
'..\source/refine.f',
'..\source/renew.f',
'..\source/ephrts.f90',
'..\source/refine.f',
'..\source/nemsfunct.f',
'..\source/refine.f',
'..\source/uefp.f',
'..\source/udat.f',
'..\source/util.f',
'..\source/uecp.f',
'..\source/uefd.f',
'..\source/ucape.f',
'..\source/uread.f',
'..\source/uldsm.f',
'..\source/unugs.f',
'..\source/uesql.f90',
'..\source/fsqlite.f90',
'..\source/uaimms.f',
'..\source/udaf.f',
'..\source/ecp_row_col.f90',
'..\source/efd_row_col.f90',
'..\source/filemgr.f',
'..\source/gdxf9def.f',
'..\source/cio4wk1.f',
'..\source/fwk1io.f',
'..\source/filer.f',
'..\source/nemswk1.f',
					fortranobject_c,
					include_directories: [inc_np, includes_files,src,compl,xpress],
					objects: [ 'gdxf9glu.o', 'aimmslink64.a', 'delayimp64.a', 'libfsqlite.a'],
                    dependencies : [
                     py_dep,					 
                     ],
                     install : true)

executable('tfiler', 
'..\source/resd.f',
'..\source/comm.f',
'..\source/filemgr.f',
'..\source/gdxf9def.f',
'..\source/cio4wk1.f',
'..\source/fwk1io.f',
'..\source/filer.f',
'..\source/tfiler.f',
'..\source/dummyx.f',
'..\source/nemswk1.f',
			include_directories: [includes_files,src],
			objects: [ 'gdxf9glu.o', 'delayimp64.a', 'libfsqlite.a']
			)
			
executable('intercv', 
			'intercv.f',  
			'filer.f',
			'gdxf9def.f',
			'fwk1io.f',	
			'cio4wk1.f',
			include_directories: [includes_files,src],
			objects: [ 'gdxf9glu.o', 'delayimp64.a', 'libfsqlite.a']
			)		
