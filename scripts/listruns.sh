# $Header: M:/default/scripts/RCS/listruns.sh,v 1.8 2016/04/07 13:31:24 dsa Exp $
# searches runlog for lines that contain all of the argument strings
#
if [ -z "$1" ] ; then
  echo "enter search strings as arguments"
  exit
fi
echo "This command lists the runs which have a RESTART file in the output directory."
narg=$#
argstr=$*
cmdstr="grep -v Deleted $NEMSJOBLOG/runlog | awk '{print tolower(\$0)}' | sed 's@\/\/nem2\/l\/@l:\/@' | grep -i "
i=1
for arg in $argstr   
do
  temper="$cmdstr $arg "  
  cmdstr=$temper
  if [ $i -ne $narg ] ; then
    temper="$cmdstr | grep -i "
    cmdstr=$temper
  fi
  i=i+1
done
cd
echo $cmdstr > temp.sh
sh temp.sh | sed "s/'//g" | awk '{print "if [ -f " $2 "/RESTART.unf ] ; then echo " $1 " " $2 " ; fi"}' > temp.out
sh temp.out
#rm temp.sh
#rm temp.out
