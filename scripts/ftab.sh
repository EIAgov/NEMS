# script to run an prepared ftab from the ftabwin application.
if [ -f ftab.out ] ; then
  rm ftab.out
fi
if [ -f fort.20 ] ; then
  rm fort.20
fi
if [ -f infile.wk1 ] ; then
  rm infile.wk1
fi
$NEMS/scripts/ftab < ftab.dat   
if [ -f fort.20 ] ; then
  awk -f $NEMS/scripts/shaveftab.awk fort.20 > ftab.tmp
  mv ftab.tmp ftab.out
fi
if [ -f infile.wk1 ] ; then
  mv infile.wk1 ftab.wk1
fi

