# check for prexisting gz files which can confuse gzip
ls -l *.gz >/dev/null
gzcheck=$?
if [ $gzcheck -eq 0 ] ; then
  echo 'compressed files found. removing them to prevent errors.'
  rm -f *.gz >/dev/null
fi
rm -f collectnofiles 2>/dev/null
rm -f coal_out.mdb 2>/dev/null
rm -f coal*.txt 2>/dev/null
rm -f cts*gms 2>/dev/null
rm -f EMMDB.mdb 2>/dev/null
rm -f PLNTTMP.daf 2>/dev/null
rm -f OGUNOUT.unf 2>/dev/null
rm -f bascmm* 2>/dev/null
rm -f basemm* 2>/dev/null
rm -f basefd* 2>/dev/null
rm -f baspmm* 2>/dev/null
rm -f baxpmm* 2>/dev/null
rm -f nems.exe  2>/dev/null
echo zipping big text files
gzip K*.txt 2>/dev/null
gzip OGSMOUT.txt 2>/dev/null
gzip OGSMDBG.txt 2>/dev/null
gzip RDGENOUT.txt 2>/dev/null
gzip RESDEQP.txt 2>/dev/null
gzip RESDBOUT.txt 2>/dev/null
gzip AIRAGE.csv 2>/dev/null
gzip TRANAIR.txt 2>/dev/null
gzip TRNH2OUT.txt 2>/dev/null
echo zipping mdb, daf, pack, and gdx files
gzip lfmm_p20??.gdx 2>/dev/null
