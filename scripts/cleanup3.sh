# check for prexisting gz files which can confuse gzip
ls -l *.gz >/dev/null
gzcheck=$?
if [ $gzcheck -eq 0 ] ; then
  echo 'compressed files found. removing them to prevent errors.'
  rm -f *.gz >/dev/null
fi
rm -f *.gms 2>/dev/null
rm -f EMMDB.mdb 2>/dev/null
rm -f PLNTTMP.daf 2>/dev/null
rm -f base??i 2>/dev/null
rm -f bascmmi 2>/dev/null
rm -f jcl.backup.dat 2>/dev/null
rm -f lffindstr.bat 2>/dev/null
rm -f comfloor.xls 2>/dev/null
rm -f mchighlo.xls 2>/dev/null
rm -f coal*txt 2>/dev/null
rm -f coal_out.mdb 2>/dev/null
rm -f nems.pdb 2>/dev/null
gzip restart.gdx 2>/dev/null
