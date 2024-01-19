# script to compile FLIP 

ifort  -c flipmain.f flipmgt.f  fldictry.f  flfiles.f flkeywrd.f  \
       flparse.f  flrand.f   flscreen.f  flstring.f
