# complinkdan.s
# script to compile FLIP 

ifort  -c flipmain.f flipmgt.f  fldictry.f  flfiles.f flkeywrd.f  \
       flparse.f  flrand.f   flscreen.f  flstring.f

# script to compile ANALYZE (excluding FLIP)

ifort -c \
       analsys.f  abasis.f   aquery1.f  aquery2.f  arate1.f   arate2.f \
       areadin.f  aschema.f  asensitv.f ashow.f    asubmat.f  aedit.f \
       blkfind.f  blkio.f    blkmgt.f   blkplot.f  \
       explain.f  rbgenerl.f rbcmnd1.f  rbcmnd2.f  rbstacks.f \
       graphdo.f  graphio.f  graphsyn.f \
       gbagnda1.f gbagnda2.f gbrange.f  gbrver.f   getedit.f  getiomat.f \
       getiopck.f getiosol.f getiosub.dan.f getlib.f   getrans.f  getset.dan.f \
       gredbin.f  gredmat.f  gredexp.f  gredexp2.f gredtest.f greduce.f \
       schema.f   scheqn.f   screen.f   scrplots.f table.f

# script link ANALYZE

link /map:analyze.map /out:analyze.dan.exe \
    flipmain.obj flipmgt.obj fldictry.obj flfiles.obj flkeywrd.obj \
    flparse.obj  flrand.obj  flscreen.obj flstring.obj\
    gbagnda1.obj gbagnda2.obj gbrange.obj gbrver.obj getedit.obj getiomat.obj\
    getiopck.obj getiosol.obj getiosub.dan.obj getlib.obj getrans.obj getset.dan.obj\
    gredbin.obj  gredmat.obj gredexp.obj gredexp2.obj greduce.obj gredtest.obj\
    analsys.obj  abasis.obj  aquery1.obj aquery2.obj arate1.obj arate2.obj\
    aedit.obj    areadin.obj aschema.obj asensitv.obj ashow.obj asubmat.obj \
    blkfind.obj  blkio.obj   blkmgt.obj  blkplot.obj\
    explain.obj  rbgenerl.obj rbcmnd1.obj rbcmnd2.obj rbstacks.obj\
    graphdo.obj  graphio.obj graphsyn.obj\
    schema.obj   scheqn.obj  screen.obj   scrplots.obj table.obj
