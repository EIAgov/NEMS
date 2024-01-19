# $Header: N:/default/scripts/RCS/combine_glf.ksh,v 1.5 2005/03/02 15:52:10 DSA Exp $
#This script uses a graphlist.txt file to generate a customized
# graph layout file
#
#
layoutv=`sh -K $NEMS/scripts/fdef.sh layout.txt s | sed "s/.* is //"`
echo "layout version: $layoutv"
layout="$NEMS/input/layout.v$layoutv.txt"
ls -al $layout
sh -K $NEMS/scripts/fdef.sh graphlist.txt s
ls -al $NEMS/scripts/graphlist.txt
ls -al $PWD/graphlist*txt
if [ -a graphlist.txt ] ; then
  echo "Local graphlist.txt file found."
  def="graphlist.txt"
else
  def="$NEMS/scripts/graphlist.txt"
fi
read ans?"Enter graphlist.txt file [$def] : "
if [ "$ans" = "" ] ; then
   ans=$def
fi
if [ "$NEMS/scripts/graphlist.txt" -nt "$ans" ] ; then    # -nt:  newer-than test
   echo "WARNING: $ans is older than the default graphlist.txt file."
   echo "Some of the graph layout files may be out of date."
   echo "Will continue anyway."
fi
if [ "$ans" -ot "$layout" ] ; then # -ot:  older-than test
  echo "WARNING:  Ftab's table layout file is newer than $ans."
  echo "Some of the graph layout files may be out of date."
  echo "Will continue anyway."
fi

pre="$NEMS/fgraph/GLFs/"
#=============================================================
# Generate SS (single Series/comparison of runs)

grep -i "COMPARE" $def | grep ^1 | sed 's/.*|//' > ss_files
first=`head -1 ss_files`
grep -v "Finish" $pre$first > ss_combine.glf
echo $first
for item in `cat ss_files`
do
  if [ "$item" != "$first" ] ; then
    echo $item
    grep -v "Finish" $pre$item | grep -v "^Df" >> ss_combine.glf
  fi
done
echo "Finish-" >> ss_combine.glf
cp ss_combine.glf glf.txt
#=============================================================
grep -i "ONE RUN" $def | grep ^1 | sed 's/.*|//' > ws_files
first=`head -1 ws_files`
grep -v "Finish" $pre$first > ws_combine.glf 2>nul
if [ "$?" -eq 0 ] ; then
  echo for item
  for item in `cat ws_files`
  do
    if [ "$item" != "$first" ] ; then
      grep -v "Finish" $pre$item | grep -v "^Df" >> ws_combine.glf
    fi
  done
  echo "Finish-" >> ws_combine.glf
else
  rm ws_combine.glf
fi
