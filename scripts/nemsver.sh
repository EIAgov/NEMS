# $Header: m:/default/scripts/RCS/nemsver.sh,v 1.8 2016/05/03 19:41:29 dsa Exp $
# Prompts for version of NEMS to use:

echo "The current NEMS version/platform setting is: $NEMS"

echo Select NEMS version
cat $NEMSJOBLOG/platform.txt | awk 'BEGIN{FS=","}{printf("%s) %s %s \n", $1, $2, $3)}'

echo -n "Select version [1] : ";read ver
if [ -z "$ver" ] ; then
  ver="1"
fi
grep "^$ver," $NEMSJOBLOG/platform.txt | awk 'BEGIN{FS=","}{printf("NEMS=%s \n",$3)}'  > temp1.sh
cat temp1.sh
. temp1.sh
rm temp1.sh

export NEMS
echo "The NEMS version/platform is now: $NEMS"
oldir=$PWD
cd
# reset aliases in the profile and $NEMS/scripts/commands.sh
grep -i "alias" .profile > temp.sh
. temp.sh

# re-invoke everything but the fortran script which accumulates the PATH string 
# and leads to problems if invoked repeatedly.
grep -iv "fortvars" "$NEMS/scripts/commands.sh" > temp.sh
. temp.sh
PS1='[$NEMS][$COMPUTERNAME][$PWD] '
export PS1
cd $oldir

