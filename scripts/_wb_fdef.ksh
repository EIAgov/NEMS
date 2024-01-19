#trap "exit" 1 2 15
if [[ "$1" = "" ]] ; then
read filename?"Enter filename ==> "
$NEMS/scripts/fdef.sh $filename
else
$NEMS/scripts/fdef.sh $1
fi
echo
read ans?"Press RETURN to close this window ... "
