p=`echo -E $@ | tr '[$]' '[.]'`
#egrep -i -d skip  "$p" $NEMS/includes/!(*.mod) | grep -i "parameter.*$p.*="
egrep -i -d  skip "$p" $NEMS/includes/* --exclude=*.mod | grep -i "parameter.*$p.*="
