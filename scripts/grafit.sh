# $Header: M:/default/scripts/RCS/grafit.sh,v 1.12 2017/10/18 15:39:48 dsa Exp $ Grafit. 
# If a ran file exists in the current directory, this utility create a roster My_Files.txt in
# the user's graf2000 user directory.  It will then launch graf2000.  The user then must 
# click button "load user roster below" to load the files.
# warning: any previously existing My_Files.txt file will be overwritten
nran=`ls -l *.RAN 2>/dev/null | wc -l`

if [ $nran -gt 0 ] ; then
  echo " " > $NEMSDIR/graf2000/$LOGNAME/My_files1.txt
  dir=$PWD
  if [ "$OSTYPE" = "cygwin" ] ; then
    dir=`cygpath -m $PWD`
  fi
  ls -1 *.RAN | grep -v "\.[0-9]*\." > graflist.txt
  ls -1 *.RAN | sort -r | grep -v "\.[0-9]\." | grep "\.[0-9]*\.">> graflist.txt
  ls -1 *.[0-9]*.*.RAN 2>/dev/null | grep -v "\.[0-9][0-9]\." | sort -r >> graflist.txt
  for item in `cat graflist.txt`
  do
    echo $dir/$item >> $NEMSDIR/graf2000/$LOGNAME/My_files1.txt
  done
  
# oscillate program creates playback recordings of oscillations and T2/T3 convergence items  
  if [ -f osc.rec ] ; then
    cp osc.rec $NEMSDIR/graf2000/$LOGNAME/osc.rec
  fi
  if [ -f cnv.rec ] ; then
    cp cnv.rec $NEMSDIR/graf2000/$LOGNAME/cnv.rec
  fi
  cd $NEMSDIR/graf2000 > /dev/null
  if [ "$OSTYPE" = "cygwin" ] ; then
    sed 's/$'"/`echo \\\r`/" $NEMSDIR/graf2000/$LOGNAME/My_files1.txt > $NEMSDIR/graf2000/$LOGNAME/My_Files.txt
  else
    mv $NEMSDIR/graf2000/$LOGNAME/My_Files1.txt $NEMSDIR/graf2000/$LOGNAME/My_Files.txt
  fi
  
  M:/default/scripts/launch_graf2000.exe
  cd - > /dev/null
else
  echo "no RAN files in this directory"
fi
