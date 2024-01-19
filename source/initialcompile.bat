@echo off
set OPT1=/include:../includes
set OPT2=/compile_only /debug:full /check:bounds
set OPT3=/free /fpconstant /nopdbfile /traceback /Qsave /Qzero /nolist /static
set OPT4=/assume:byterecl /assume:source_include /fpscomp:ldio_spacing
rem the following is for filer only for speed
set OPT5=/compile_only /debug:full /O2
set OPTC=/free /nodebug

@echo on
ifort %OPT1% %OPT2% %OPT3% %OPT4%  fsqlite.f90
ifort %OPT1% %OPT2% %OPT3% %OPT4%  uesql.f90
ifort %OPT1% %OPT2% %OPT3% %OPT4%  filemgr.f
ifort %OPT1% %OPT5% %OPT3% %OPT4%  filer.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  cio4wk1.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  fwk1io.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  nemswk1.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  main.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  nemsfunct.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  epm.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  mac.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  world.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  resd.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  hem.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  comm.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  ind.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  tran.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  tranfrt.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  tranair.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  refine.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  wellogs.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  wellcost.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  wellexp.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  welloff.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  wellimp.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  welldcf.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  welleor.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  wellak.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  wellon.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  wellrep.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  gdxf9def.
ifort %OPT1% %OPT2% %OPT3% %OPT4%  dumpack.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  ecp_row_col.f90
ifort %OPT1% %OPT2% %OPT3% %OPT4%  efd_row_col.f90
ifort %OPT1% %OPT2% %OPT3% %OPT4%  uread.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  uaimms.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  util.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  udat.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  udaf.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  uecp.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  uefd.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  uefp.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  ucape.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  uldsm.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  unugs.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  renew.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  ftab.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  ftab2.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  tfiler.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  intercv.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  prepplt.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  prepett.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  prepdsmd.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  diction.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  dummypp.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  dummyx.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  gdxf9def.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  hmm.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  omlanal.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  orcltabs.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  pack_all.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  readaimlis.f90
ifort %OPT1% %OPT2% %OPT3% %OPT4%  readda.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  refchg.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  refco2.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  refeth.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  refintl.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  refrpt.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  refsln.f
ifort %OPT1% %OPT2% %OPT3% %OPT4%  wellugr.f

@echo off



