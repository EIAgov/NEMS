# $Header: N:\\default\\scripts/RCS/releasejobs.sh,v 1.1 1999/11/17 13:40:00 PKC Exp $
# Script to release any of the user's jobs on hold 
   /usr/lpp/LoadL/nfs/bin/llq | grep "  *$USER" | grep -c " H " | read njobs
   llq
   echo "You have $njobs job(s) on hold"
   if [ $njobs -gt 0 ] ; then
     /usr/lpp/LoadL/nfs/bin/llq | grep "\.[0-9]  *$USER" | grep " H " | sed 's/ .*$//;s/^/llhold -r /' >release.sh
     cat release.sh
     ksh release.sh 
   fi
