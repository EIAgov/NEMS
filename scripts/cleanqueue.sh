if [ -z "$NEMSJOBLOG" ] ; then
  NEMSJOBLOG="$NEMS/logs"
  export NEMSJOBLOG
fi
cd $NEMSJOBLOG
# clean out old queue files
find . -name "queue.*" -mtime +1 -exec rm "{}" ";"
# clean out old verscomp files
 find . -name "@.verscomp.*" -mtime +1 -exec rm "{}" ";"
