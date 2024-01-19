# $Header: M:/default/scripts/RCS/caimms.sh,v 1.1 2016/10/21 17:19:19 dsa Exp $
# script to prepare and launch the aimms coal model in an existing nems output p2 folder.
# the script copies the input folder, uncompresses the access files, and launches aimms from the
# command line

d=$PWD
echo $d | grep -i "p2$"
r=$?
echo "p2 indicator: $r"
if [ $r -ne 0 ] ; then
   echo this command must be entered from a parallel run p2 folder
   return
fi
if [ ! -d input ] ; then
  echo copying input folder to p2
  cp -rm ../input . 2> /dev/null
  cd input
  uncompress cmm.mdb.gz cmm2.mdb.gz cps.mdb.gz coal_out.mdb.gz 2> /dev/null
  cd ..
fi
grep -i AIMMSLOC ../scedes.all | sed 's/^.*=//;s@\\@\/@g' > temp.dat
cat temp.dat
ac=`cat temp.dat`
ac="$ac/bin/aimms.exe"
echo "aimms executable used is $ac"
if [ -f "$ac" ] ; then
  echo launching aimms
  ac="$ac coal.aimms"
  echo "aimms command is $ac"
  cd coal
  $ac
fi



