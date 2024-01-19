# $Header: m:/default/scripts/RCS/addex.sh,v 1.3 2019/09/18 13:07:57 pkc Exp $
# addex.sh -- script to add model execution switches to a scedes file
#  usage:
#  addex.sh scedes.file
if [ -f $1 ] ; then
   scedesfile=$1
else
   scedesfile="scedes.$1"
fi
if [ ! -f $scedesfile ] ; then
   echo " scedes file $scedesfile not found"
   return
fi
grep "^EX.=" $NEMS/scripts/varkeys | sed "s/=0.*/=0/" >> $scedesfile
grep "^SSCRIPT=" $NEMS/scripts/varkeys | awk '{print $1}' >> $scedesfile
