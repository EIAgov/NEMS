# script to compile ANALYZE (excluding FLIP)

ifort -c \
       analsys.f  abasis.f   aquery1.f  aquery2.f  arate1.f   arate2.f \
       areadin.f  aschema.f  asensitv.f ashow.f    asubmat.f  aedit.f \
       blkfind.f  blkio.f    blkmgt.f   blkplot.f  \
       explain.f  rbgenerl.f rbcmnd1.f  rbcmnd2.f  rbstacks.f \
       graphdo.f  graphio.f  graphsyn.f \
       gbagnda1.f gbagnda2.f gbrange.f  gbrver.f   getedit.f  getiomat.f \
       getiopck.f getiosol.f getiosub.f getlib.f   getrans.f  getset.f \
       gredbin.f  gredmat.f  gredexp.f  gredexp2.f gredtest.f greduce.f \
       schema.f   scheqn.f   screen.f   scrplots.f table.f

