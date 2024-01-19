# $Header: M:/default/scripts/RCS/runitnew.sh,v 1.18 2016/02/12 19:29:46 dsa Exp $
# Script to launch job from runit.f
# needs 6 arguments
#
umask u=rwx,g=rwx,o=rwx
datec=`date`
echo "Runit.sh launched at $datec"
echo "Arguments: $1 $2 $3 $4 $5 $6 $7"
if [ -z "$NEMS" ] ; then
   NEMS="m:/default"
fi
echo "NEMS is set to $NEMS" >> nohup.out
if [ -z "$6" ] ; then
  return
fi
outdir=`echo $1 | awk '{print tolower($1)}'`
PCoutdir=`echo $2 | awk '{print tolower($1)}'`
workdir=`echo $3 | awk '{print tolower($1)}'`
PCworkdir=`echo $4 | awk '{print tolower($1)}'`
scenario=`echo $5 | awk '{print tolower($1)}'`
datecode=`echo $6 | awk '{print tolower($1)}'`
TITLEBAR="NEMS $scenario $datecode"

cd $outdir
echo "Job Dispatched by Runit" > nohup.out
echo "  Output dir=$1" >> nohup.out
echo "  outdir PC=$2" >> nohup.out
echo "  working dir=$3" >> nohup.out
echo "  workdir PC=$4" >> nohup.out
echo "  scenario=$5" >> nohup.out 
echo "  datecode=$6" >> nohup.out

compcode=0
cdstatus=1
copied=0
ok=0

cp_flg="-rfm"
cp_flg2="-fm"
if [ "$OSTYPE" = "cygwin" ] ; then
  cp_flg="-rfp"
  cp_flg2="-fp"
fi

if [ "$PCoutdir" != "$PCworkdir" ] ; then
  echo "Will try to run in $workdir on $PCworkdir instead of in $outdir on $PCoutdir" >>$outdir/nohup.out
  cd $workdir
  echo "current working directory is now:" >> $outdir/nohup.out
  pwd >>$outdir/nohup.out
  mkdir $scenario 2>/dev/null
  cd $scenario
  mkdir $datecode 2>/dev/null
  cd $datecode
  echo "current working directory is now:" >> $outdir/nohup.out
  if [ "$OSTYPE" = "cygwin" ] ; then
    wpwd=`cygpath -m $PWD`
  else
    wpwd=$PWD
  fi

  echo $wpwd >> $outdir/nohup.out
  workdir2="$workdir/$scenario/$datecode"
# lower case
  PWDLC=`echo $wpwd | awk '{print tolower($1)}'`
  
  if [ "$PWDLC" != "$workdir2" ] ; then
     echo "PWD=$PWDLC and workdir2 is $workdir2" >> $outdir/nohup.out
     echo "These should be equal, so " >> $outdir/nohup.out
     echo "unable to switch to $workdir2" >> $outdir/nohup.out
     cd $outdir
     echo "Current directory switched back to:" >> $outdir/nohup.out
     pwd >> $outdir/nohup.out
  else
     echo "CD Status ok" >> $outdir/nohup.out
     cdstatus=0
  fi
  if [ "$cdstatus" = "0" ] ; then
    echo "removing any prexisting files from working directory" >> $outdir/nohup.out
    rm -rf $workdir2/* >> $outdir/nohup.out 2>&1
    echo "copying files from output directory to working directory" >> $outdir/nohup.out
    cp "$cp_flg" $outdir/* . >> $outdir/nohup.out 2>&1
    if [ $? = 0 ] ; then
      cp -f $outdir/nohup.out . 
      copied=1
    else
      echo "error during copying, so will just use original output directory" >> $outdir/nohup.out
      echo "removing workdir" >> $outdir/nohup.out
      rm -rf $workdir2 >>  $outdir/nohup.out 2>&1
      cd $outdir
    fi
  fi
else
  cd $outdir
fi

echo "Launching cycle.sh in $PWD" >> nohup.out

sh cycle.sh >>nohup.out 2>&1    
compcode=$?
echo "Back from cycle.sh" >> nohup.out



if [ "$copied" = "1" ] ; then
  # remove input folders and other files that should be in the original folder
  if [ -d $workdir2/p1/input ] ; then
    rm -fr $workdir2/p1/input
    rm -f $workdir2/p1/*.dll
    rm -f $workdir2/p1/*.exe
  fi
  if [ -d $workdir2/p2/input ] ; then
    rm -fr $workdir2/p2/input
    rm -f $workdir2/p2/*.dll
    rm -f $workdir2/p2/*.exe
  fi
  if [ -d $workdir2/p3/input ] ; then
    rm -fr $workdir2/p3/input
    rm -f $workdir2/p3/*.dll
    rm -f $workdir2/p3/*.exe
  fi
  if [ -d $workdir2/input ] ; then
    rm -fr $workdir2/input
    rm -f scedes.all ;# don't move this file because original already exists and original may be inaccessible after being opened by runit.exe
    if [ -f $workdir2/ftab.exe ] ; then
      cp -f $workdir2/ftab.exe $workdir2/ftab.xxx
      echo $workdir2/ftab.xxx created
    fi
    rm $workdir2/*.dll $workdir2/*.exe 
  fi
  if [ -f $outdir/ftab.exe ] ; then
    cp -f $outdir/ftab.exe $outdir/ftab.xxx
    echo $outdir/ftab.xxx created
  fi

  echo "Copying files from $workdir2 to $outdir" >> nohup.out


  if [ -d $workdir2/p1 ] ; then
    echo " " >> nohup.out
    echo "command:  cp $cp_flg $workdir2/p1/* $outdir/p1" >> nohup.out
    cp "$cp_flg" $workdir2/p1/* $outdir/p1 >> nohup.out 2>&1
    cpretp1=$?
    echo "cp p1 return code=$cpretp1" >> nohup.out
    if [ "$cpretp1" = "0" ] ; then
      echo "command: rm -rf $workdir2/p1" >> nohup.out
      rm -rf $workdir2/p1 >> nohup.out 2>&1
    fi
  else 
    cpretp1=0
  fi
  if [ -d $workdir2/p2 ] ; then
    echo " " >> nohup.out
    echo "command:  cp $cp_flg $workdir2/p2/* $outdir/p2" >> nohup.out
    cp "$cp_flg" $workdir2/p2/* $outdir/p2 >> nohup.out 2>&1
    cpretp2=$?
    echo "cp p2 return code=$cpretp2" >> nohup.out
    if [ "$cpretp2" = "0" ] ; then
      echo "command: rm -rf $workdir2/p2" >> nohup.out
      rm -rf $workdir2/p2 >> nohup.out 2>&1
    fi
  else 
    cpretp2=0
  fi
  if [ -d $workdir2/p3 ] ; then
    echo " " >> nohup.out
    echo "command:  cp $cp_flg $workdir2/p3/* $outdir/p3" >> nohup.out
    cp "$cp_flg" $workdir2/p3/* $outdir/p3 >> nohup.out 2>&1
    cpretp3=$?
    echo "cp p3 return code=$cpretp3" >> nohup.out
    if [ "$cpretp3" = "0" ] ; then
      echo "command: rm -rf $workdir2/p3" >> nohup.out
      rm -rf $workdir2/p3 >> nohup.out 2>&1
    fi
  else 
    cpretp3=0
  fi
  echo " " >> nohup.out
  echo "List of files in $workdir2" >> nohup.out
  ls -al $workdir2/* >> nohup.out
  echo " " >> nohup.out
  echo "command:  cp $cp_flg $workdir2/* $outdir" >> nohup.out
  cp "$cp_flg" $workdir2/* $outdir >> nohup.out 2>&1
  cpret=$?
  echo "cp return code=$cpret" >> $outdir/nohup.out
  echo "cp return code=$cpret" >> $workdir2/nohup.out

  cpret=`expr $cpret + $cpretp1 + $cpretp2 + $cpretp3`
  

# Get job number of job and update status 
  jobno=`$NEMS/scripts/jobcontrol.exe | grep "/$scenario/$datecode" | awk '{print $1}' | head -1`
  if [ ! -z "$jobno" ] ; then
    $NEMS/scripts/jobcontrol.exe status $jobno Complete > /dev/null
    if [ "$compcode" != "0" ] ; then
# update name of output directory--in case job bombs and main.f doesn't do it
      if [ "$cpret" = "0" ] ; then
        $NEMS/scripts/jobcontrol.exe odir $jobno $outdir > /dev/null
      else
        $NEMS/scripts/jobcontrol.exe odir $jobno $workdir2 > /dev/null
      fi
    fi
  fi
  if [ "$cpret" = "0" ] ; then
    rm -rf $workdir2/* 
    cd ..
    rmdir $datecode
    if [ -f $outdir/nems.exe.gz ] ; then
      rm -f $outdir/nems.exe
    fi
  else
    echo "An error occurred while copying the files from $workdir2 to $outdir" >> $workdir2/nohup.out
    echo "so not removing working directory" >> $workdir2/nohup.out
    echo "An error occurred while copying the files from $workdir2 to $outdir" >> $outdir/nohup.out
    echo "so not removing working directory" >> $outdir/nohup.out
    jobno=`$NEMS/scripts/jobcontrol.exe | grep "/$scenario/$datecode" | awk '{print $1}' | head -1`
    if [ ! -z "$jobno" ] ; then
        $NEMS/scripts/jobcontrol.exe odir $jobno $workdir2 > /dev/null
    fi
  fi
else
  echo "copied=$copied" >> nohup.out
# Get job number of job and update status 
  jobno=`$NEMS/scripts/jobcontrol.exe | grep "/$scenario/$datecode" | awk '{print $1}' | head -1`
  if [ ! -z "$jobno" ] ; then
    $NEMS/scripts/jobcontrol.exe status $jobno Complete > /dev/null
  fi
fi
cd $outdir
rm -f *.exe 
# rm -f cycle.sh
# rm -f cleanup.sh
echo " "
if [ -d $outdir/p1/input ] ; then
  echo "removing $outdir/p1/input" >> nohup.out
  rm -fr $outdir/p1/input >> nohup.out 2>&1
  rm -f $outdir/p1/*.exe 
fi
if [ -d $outdir/p2/input ] ; then
  echo "removing $outdir/p2/input" >> nohup.out
  rm -fr $outdir/p2/input >> nohup.out 2>&1
  rm -f $outdir/p2/*.exe 
fi
if [ -d $outdir/p3/input ] ; then
#  remove unzipped polysys binary input files.  the zip is still there.
  echo "removing polysys binary input files."  >> nohup.out
  rm -f $outdir/p3/input/ps*.unf >> nohup.out 2>&1
  echo "moving $outdir/p3/input to $outdir/input"  >> nohup.out
  rm -fr $outdir/input >> nohup.out 2>&1
  mv $outdir/p3/input $outdir/input >> nohup.out 2>&1
  echo "A single copy of the input folder has been moved to $outdir/input"  >> nohup.out
  echo "g-zipping the non-text files in the input folder to save space."  >> nohup.out
  rm -f input/*.wk1 >> nohup.out 2>&1
  gzip input/*.daf  >> nohup.out 2>&1
  gzip input/*.mdb >> nohup.out 2>&1
  gzip input/*.gdx >> nohup.out 2>&1
  gzip input/*.rlx >> nohup.out 2>&1
  gzip input/*.xml >> nohup.out 2>&1
  gzip input/*.xls >> nohup.out 2>&1
  rm -f $outdir/p3/*.exe 
  echo "Done gzipping."  >> nohup.out
fi


