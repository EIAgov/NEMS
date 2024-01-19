cd $NEMSJOBLOG
ls -1 queue* | grep -iv log | grep -iv debug | grep nem > stoplist
cat stoplist | sed 's/^/cp stoplist /;s/queue/stop/' > stopcmd.sh
#echo "Here is stopcmd.sh:"
#cat stopcmd.sh
sh stopcmd.sh
