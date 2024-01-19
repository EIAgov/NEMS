#! /bin/sh
# convert single file from dos format to unix format so it can be executed in bash
#
tempi="$1_2"
tr -d '\15' < $1 > $tempi
mv -f $tempi $1


