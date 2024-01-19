# check for prexisting gz files which can confuse gzip
ls -l *.gz >/dev/null
gzcheck=$?
if [ $gzcheck -eq 0 ] ; then
  echo 'compressed files found. removing them to prevent errors.'
  rm -f *.gz >/dev/null
fi
rm -f *mat 2>/dev/null
rm -f ACT* 2>/dev/null
rm -f ECPIDAF.daf 2>/dev/null
rm -f ECPODAF.daf 2>/dev/null
rm -f RSDSM.unf 2>/dev/null
rm -f MPS*.dat 2>/dev/null
rm -f ECPDAT* 2>/dev/null
rm -f ETTTMP.daf 2>/dev/null
rm -f EFP[C-Z]* 2>/dev/null
rm -f 1 2>/dev/null
rm -f DD 2>/dev/null
rm -f EFDOUT.txt 2>/dev/null
rm -f INPTDAF.daf 2>/dev/null
rm -f SYSLOAD.daf 2>/dev/null
rm -f TRANSET.daf 2>/dev/null
rm -f WRPT.txt 2>/dev/null
rm -f SO2DAF.daf 2>/dev/null
rm -f PS*.unf 2>/dev/null
cat bas[1-2][0-9][0-9][0-9].dat > BASEMMO.dat 2>/dev/null; chmod ugo+w BASEMMO.dat ; rm bas[1-2][0-9][0-9][0-9].dat 2>/dev/null
cat efd[1-2][0-9][0-9][0-9].dat > BASEFDO.dat 2>/dev/null; chmod ugo+w BASEFDO.dat ; rm efd[1-2][0-9][0-9][0-9].dat 2>/dev/null
cat cmm[1-2][0-9][0-9][0-9].dat > BASCMMO.dat 2>/dev/null; chmod ugo+w BASCMMO.dat ; rm cmm[1-2][0-9][0-9][0-9].dat 2>/dev/null
cat baspmm*.da > BASPMM1.dat 2>/dev/null ; chmod ugo+w BASPMM1.dat
cat baxpmm*.da > BAXPMM1.dat 2>/dev/null ; chmod ugo+w BAXPMM1.dat; rm ba?pmm*.da 2>/dev/null
rm -f bascmmi.dat 2>/dev/null
rm -f basemmi.dat 2>/dev/null
rm -f basefdi.dat 2>/dev/null
rm -f baspmm2.dat 2>/dev/null
rm -f baxpmm2.dat 2>/dev/null
rm -f SYSACT.act 2>/dev/null
echo zipping nems.exe
gzip nems.exe  2>/dev/null
echo zipping .act files
gzip EFD_????.act 2>/dev/null
gzip EMM_????.act 2>/dev/null
gzip CMM_????.act 2>/dev/null
gzip PMM_????.act 2>/dev/null
gzip XPMM????.act 2>/dev/null
echo zipping big text files
gzip EFDDBUG.txt 2>/dev/null
gzip EDBPGRP*.txt 2>/dev/null
gzip EMMRP?.txt 2>/dev/null
gzip EMMDBASE.txt 2>/dev/null
gzip EMMDBUG 2>/dev/null
gzip EMMPRNT 2>/dev/null
gzip EMMREPT 2>/dev/null
gzip PMMPRNT 2>/dev/null
gzip PMMDBG.txt 2>/dev/null
gzip WDUMP.txt 2>/dev/null
gzip SYSPRNT 2>/dev/null
gzip LDSMRPT.txt 2>/dev/null
gzip K*.txt 2>/dev/null
gzip OGSMOUT.txt 2>/dev/null
gzip RDGENOUT.txt 2>/dev/null
gzip RESDEQP.txt 2>/dev/null
gzip RESDBOUT.txt 2>/dev/null
gzip CL*.txt 2>/dev/null
gzip UEFP.txt 2>/dev/null
gzip BASEMMO.DAT 2>/dev/null
gzip BASEFDO.DAT 2>/dev/null
gzip BASPMM1.DAT 2>/dev/null
gzip BAXPMM1.DAT 2>/dev/null
echo zipping mdb, daf, pack, and gdx files
gzip PMMDB.mdb 2>/dev/null
gzip EMMDB.mdb 2>/dev/null
gzip OUTDAF.daf 2>/dev/null
gzip PACK* 2>/dev/null
gzip *.PCK 2>/dev/null
gzip *.gdx 2>/dev/null
gzip ecp/*.txt 2>/dev/null
gzip ecp/*.mps 2>/dev/null
gzip ecp/log/*.lis 2>/dev/null
gzip efd/*.txt 2>/dev/null
gzip efd/*.mps 2>/dev/null
gzip efd/log/*.lis 2>/dev/null
gzip e*coeff_*.txt 2>/dev/null
gzip *_soln_*txt 2>/dev/null
gzip *.mps 2>/dev/null
