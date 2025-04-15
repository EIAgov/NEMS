uc=`print $1 | tr '[:lower:]' '[:upper:]'`
lc=`print $1 | tr '[:upper:]' '[:lower:]'`
mc=${uc:0:1}${lc:1:20}
egrep -i -d skip  " $1[ 	(]| $1$|parameter.*$1" $NEMS/includes/* --exclude=*.mod  2> /dev/null | sed "s/!.* $lc.*$//;s/!.* $uc.*$//;s/!.* $mc.*$//" | egrep -i " $1[ 	(,]| $1$"
