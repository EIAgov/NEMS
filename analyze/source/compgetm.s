# script to compile GETMAT under AIX

xlf -c -qsource \
     gbagnda1.f gbagnda2.f gbrange.f  gbrver.f   getedit.f  getiomat.f
     getiopck.f getiosol.f getiosub.f getlib.f   getrans.f  getset.f \
     gredbin.f  gredmat.f  gredexp.f  gredexp2.f gredtest.f greduce.f
