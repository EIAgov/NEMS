#!/bin/ksh
#$Header: M:/default/scripts/RCS/makemps.sh,v 1.6 2017/03/31 15:06:15 dsa Exp $
# Create pack files (.pck) from EFD's or ECP's OML act files (*.act) using "pack_all.exe" and
# then then invokes analyze to read the pack files and write out a ".mps" file for each one.
#
# The .mps files are intended for use by Augustine OML-to-AIMMS-to-CPLEX comparison program
# to validate the LP construction.
#

model=$1
modelp=$1
year=$2
if [ "$NEMS" = "" ] ; then
  NEMS="m:/default"
fi

alias    analyze="sh $NEMS/analyze/analyze.s" ; export analyze

if [ "$model" = "" ] ; then
  echo supply the LP model name, EFD or EMM, as an argument
  return
elif [ "$model" = "ECP" ] ; then
  model="EMM"
  modelp="ECP"
fi
echo model=$model
if [ "$model" != "EFD" -a "$model" != "EMM" ] ; then
  echo model name must be EFD or EMM
  return
fi

if [ "$model" = "EMM" ] ; then
  packpre='EC'
  if [ -f basemmo.dat.gz ] ; then
    uncompress basemmo.dat.gz
  fi
elif [ "$model" = "EFD" ] ; then
  packpre='ED'
fi

if [ "$year" != "" ] ; then
  echo $year > actlist.txt
else
  ls -1 *.act* | grep -i "$model\_"  | sed 's/.*_//;s@\....@@;s@\...@@' > actlist.txt
fi
nyears=`cat actlist.txt | wc -l | sed 's/^ *//'`
echo "nyears=$nyears"
if [[ $nyears -gt 0 ]] ; then
  for y in `cat actlist.txt` ; do
    actfile=$model"_"$y".act"
    packfile=${actfile#EFD_20}
    packfile=${packfile#EMM_20}
    packfile=$packpre${packfile%.act}".pck"
    gzfile=$actfile".gz"
    if [ -f "$gzfile" ] ; then
      uncompress $gzfile
    else
      echo $gzfile not found
    fi
    if [ -f "$actfile" ] ; then
      if [ -f syswhiz.act ] ; then
        rm -f syswhiz.act
      fi
      echo "$actfile found"
      echo "running pack_all $modelp $y "
      $NEMS/scripts/pack_all.exe $modelp $y
      
      if [ -f "$packfile" ] ; then
         echo $packfile found
         mpsfile=${actfile%.act}".mps"
# create analyze input commands to read the pck file and write the mps file
         echo "dan" > analyze.mps.txt
         echo "dan" >> analyze.mps.txt
         echo "dan" >> analyze.mps.txt
         echo "read packed $packfile" >> analyze.mps.txt
         echo "submat *" >> analyze.mps.txt
         echo "submat write $mpsfile" >> analyze.mps.txt
         echo "y" >> analyze.mps.txt
         echo "quit" >> analyze.mps.txt
         cat analyze.mps.txt
         analyze < analyze.mps.txt
       fi     
    else
      echo "$actfile not found"
    fi
  done
else
  echo no matching act files found
fi
echo "done"