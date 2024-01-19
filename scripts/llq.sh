# Script to execute llq command to 
# provide more info if available
 cd
 pwd > tempit;read homedir < tempit; rm tempit
 cd - 
 llqfil="$homedir/llq.$$"
 ps -al | egrep -i "nems.exe| USER"  | grep -v grep > $llqfil
 date +"%m%d%y" > $homedir/tempit ; read DATE < $homedir/tempit;rm $homedir/tempit
 DATE="d$DATE"
# joblog="$NEMS/logs/@.joblog.$DATE"
# if [ -a $joblog ] ; then
#   awk -f $NEMS/scripts/llq.awk < $llqfil 
#  $NEMS/scripts/llqawk.exe < $llqfil 
# else
   cat $llqfil
# fi
 rm $llqfil

