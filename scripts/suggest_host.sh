echo " "
echo "Checking for available PCs "
echo " "
NEMQueue=$(echo "$NEMSJOBLOG" | tr '\\' '/')
ls -1 $NEMQueue/queue* | grep -iv "\.log" | awk -F [./] '{print $4";"$0}' > $HOME/queues.txt
sed 's@^@y=\"@;s@\;@\"\;x=\`cat @;s@$@\`\;echo \$y \$x@' $HOME/queues.txt > $HOME/temp1.sh;
. $HOME/temp1.sh > $HOME/temp1.dat
sleep 6
. $HOME/temp1.sh > $HOME/temp2.dat
diff $HOME/temp1.dat $HOME/temp2.dat | grep ">" > $HOME/hosts.dat 
rm $HOME/temp1.dat $HOME/temp2.dat
$NEMS/scripts/jobcontrol.exe | cut -c 1-57 > $HOME/joblog.dat
# build a script, hosts.sh, to compute the pick score for each available PC
awk '{print "own=`grep "$2" $NEMS/scripts/host_picks.txt | cut -c 14-`;nj=`grep "$2" $HOME/joblog.dat | grep -vi \"small\" | cut -c6-36 | sort -u | wc -l `;r=`awk -f $NEMS/scripts/pick.awk -v pc="$2" -v nj=\"$nj\" < $NEMS/scripts/host_picks.txt`;echo $r $own "$2" $nj "}' $HOME/hosts.dat > $HOME/hosts.sh
. $HOME/hosts.sh | sort > $HOME/hosts2.dat
awk '{printf "%-8s       %-7s    %d      %4d\n", $2, $3, $4, $1}' $HOME/hosts2.dat > $HOME/hosts3.dat

echo "Distinquished          # Big Jobs   Pick"
echo " Host/Hostess  HostPC    Running   Index"
echo "=============  ======= ========== ======="
cat $HOME/hosts3.dat
echo " "
line=1
sh $NEMS/scripts/showspace.sh
pick=`head -"$line" $HOME/hosts3.dat | tail -1 | awk '{print $2}'`
echo "The HOSTPC with the best speed index is  $pick"
rm $HOME/queues.txt $HOME/temp1.sh  $HOME/joblog.dat $HOME/hosts.dat $HOME/hosts2.dat $HOME/hosts3.dat  $HOME/hosts.sh
