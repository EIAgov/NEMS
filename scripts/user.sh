lsuser ALL | sort | awk -f $NEMS/scripts/user.awk | sort | sed "s/roles=//"
