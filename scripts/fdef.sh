#!sh
   
 if [[ -z "$1"  ]] ; then
   banner  "NEMS FDEF"
   echo " "
   echo " INSTRUCTIONS"
   echo "   The fdef command reports the git log of a NEMS file"
   echo "   .  It also prints part of the file's log from "
   echo " "
   echo "        fdef path/filename "
   echo "   "  
   echo "   For example:"
   echo "         fdef includes/emmcntl" 
   echo "         fdef scripts/submit.sh"
   echo "         fdef source/main.f"
   echo " "
   echo -n "Hit Return to Exit";read exit
   exit
 fi
 cd

 filename=$1
 if [ -r "$NEMS/source/$filename" ] ; then
   git --git-dir=$NEMS/.git log  --pretty=format:'%h - %cn, %cD  ::%s' $NEMS/source/$filename  >test.txt
 elif [ -r "$NEMS/input/$filename" ] ; then
   git --git-dir=$NEMS/.git log  --pretty=format:'%h - %cn, %cD  ::%s' $NEMS/input/$filename  >test.txt
 elif [ -r "$NEMS/scripts/$filename" ] ; then
   git --git-dir=$NEMS/.git log  --pretty=format:'%h - %cn, %cD  ::%s' $NEMS/scripts/$filename  >test.txt
 elif [ -r "$NEMS/includes/$filename" ] ; then
   git --git-dir=$NEMS/.git log  --pretty=format:'%h - %cn, %cD  ::%s' $NEMS/includes/$filename  >test.txt
 else
   echo "file does not exist"
 fi
cat test.txt
rm test.txt
 