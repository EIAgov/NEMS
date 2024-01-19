if [ "$NEMSJOBLOG" = "" ] ; then
  NEMSJOBLOG="w:/NEMSJobLog"
fi

if [ -a $NEMSJOBLOG/space_report_summary.txt ] ; then
  created=`grep "Created" $NEMSJOBLOG/space_report_summary.txt | sed 's/^.*Created //'`
  rspace=`grep -i "$LOGNAME" $NEMSJOBLOG/space_report_summary.txt | cut -c40-60 | awk '{print $1}'`
  hdir=`grep -i "$LOGNAME" $NEMSJOBLOG/space_report_summary.txt | awk '{print $2}'`
  runs=`grep -i "$LOGNAME" $NEMSJOBLOG/space_report_summary.txt | cut -c30-39 | awk '{print $1}'`
  if [ "$runs" = "" ] ; then 
    runs="0"
    rspace="0"
  fi
#  cat $NEMSJOBLOG/space_report_summary.txt
  echo  
  echo "Based on $NEMSJOBLOG/space_report_summary.txt, "
  echo "created $created, you had:"
  echo " "
  echo "         $runs   runs or output directories on the servers"
  echo " $rspace   KB space used in those directories"
  echo " "
  echo "Details:         $NEMSJOBLOG/runspace_report_detail.txt" 
  echo
fi 
echo Free space on NEMS drives in kilobytes
df -k | egrep -i "K:|L:|M:|N:|O:|P:|Q:|R:|S:|T:|U:|V:" > $HOME/tempdf
if [ "$OSTYPE" = "cygwin" ] ; then
  cat $HOME/tempdf | awk '{print $1 " " $4}' | awk '{c1=" ";c2=" ";c3=" ";L=length($2);if(L>3)c1=",";if(L>6)c2=",";if(L>9)c3=",";$2=sprintf("%12s",$2);printf("%s %3s%1s%3s%1s%3s%1s%3s\n",$1,substr($2,1,3),c3,substr($2,4,3),c2,substr($2,7,3),c1,substr($2,10,3))}' 
else
  cat $HOME/tempdf | awk '{print $1 " " $3}' | sed 's/\// /;s/\/[0123456789]*$//' | awk '{c1=" ";c2=" ";c3=" ";L=length($2);if(L>3)c1=",";if(L>6)c2=",";if(L>9)c3=",";$2=sprintf("%12s",$2);printf("%s %3s%1s%3s%1s%3s%1s%3s\n",$1,substr($2,1,3),c3,substr($2,4,3),c2,substr($2,7,3),c1,substr($2,10,3))}' 
fi
rm -f $HOME/tempdf 1>/dev/null 2>/dev/null

