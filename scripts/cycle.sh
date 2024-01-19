# $Header: M:/default/scripts/RCS/cycle.sh,v 1.131 2021/04/23 18:14:19 nems_auto Exp $
a=$(grep -h "stored here" launched.from | cut -d ':' -f 2)
b=$(grep -h "stored here" launched.from | cut -d ':' -f 3)
NEMS=$a":"$b
echo "jognems, NEMS repo is:"
echo $NEMS

 if [ "$OSTYPE" = "cygwin" ] ; then
   shopt -s nocaseglob
   umask u=rwx,g=rwx,o=rwx
 fi
 grep -i "OMLVERS=" JCL.DAT > omlvers; read omlvers < omlvers; rm omlvers 
 omlvers=${omlvers#*=}
 MPS="c:/oml/$omlvers"
 export MPS
 echo $MPS
 echo "Here is the current PATH string:"
 echo -E $PATH
 echo "Here is the LIB string:"
 echo -E $LIB
 
 grep -i "NRUNS" MOREOPT > nruns; read dum nruns < nruns; rm nruns
 grep -i "DOEMALL" MOREOPT > doemall; read dum doemall < doemall; rm doemall
 grep -i "DOAMINOF" MOREOPT > doaminof; read dum doaminof < doaminof; rm doaminof
 grep -i "MINSCORE" JCL.DAT > minscore; IFS='='; read dum minscore < minscore; rm minscore;IFS=' '
 grep -i "XPRESSSW" MOREOPT > xpresssw; read dum xpresssw < xpresssw; rm xpresssw
 grep -i "EXBUILD" MOREOPT > exbuild; read dum exbuild < exbuild; rm exbuild
 grep -i "ITERDUMP" MOREOPT > iterdump; read dum iterdump < iterdump; rm iterdump
 NOTIFY=`grep -i "^NOTIFY=" scedes.all | head -1 | cut -c 8`

 echo "The XPRESS-MP Switch, XPRESSSW is " $xpresssw "(0=off, 1=on for ECP) "

 if [ "$1" = "big" ] ; then
   echo "big memory version"
 else
   echo "normal memory version"
 fi 
 n=1
 if [ $doemall -eq 1 ] ; then
   echo "There will be $nruns cycles(s) in this run."
 else
   echo "There will be $nruns run(s) in this cycle, unless NEMS converges between cycles."
 fi
 export n
 export nruns
 export doemall
 export doaminof
 export minscore
 datecode=`basename $PWD`
 dir2=`dirname $PWD`
 scenario=`basename $dir2`
 
   
 while [ "$n" -le "$nruns" ]
 do
      echo "NEMS Run $n"  

# if EXBUILD equal 1, then
# run program to invoke resd, comm in partner process with NEMS.  the 1 argument establishes a loop
# to stayopen and communicate via the file "nems.signal".  Run in minimized window, low priority
      if [ $exbuild -eq 1 ] ; then
        echo "Running tfiler coprocess for resd, comm. exbuild=$exbuild"
        echo "QUIT" > nems.signal
        sleep 2
        echo "NEMS" > nems.signal

        if [ "$OSTYPE" = "cygwin" ] ; then
           cygstart --minimize "$NEMS/scripts/sys.exe" tfiler.exe "1 > nohup.tf.out 2>&1"
        else
          start -l -m -t "TFiler $scenario $datecode" "$NEMS/scripts/sys.exe" tfiler.exe "1 > nohup.tf.out 2>&1"
        fi
      fi
# Run nems.  the argument $1 is the job class (big, small, debug).      
      echo "Run $n Start time:  " `date +%T`
      nems.exe $1
      returncode=$?
      echo "Returncode=$returncode"
      echo "QUIT" > nems.signal
      echo "Run $n End time:  " `date +%T`
      
      if [ $returncode -eq 128 ] ; then
        echo "possible dll problem. is f90sql from canaimasoft linked in?"
      fi 
      if [ $returncode -ne 0 ] ; then
        datecode=`basename $PWD`
        dir2=`dirname $PWD`
        scenario=`basename $dir2`
        jobno=`$NEMS/scripts/jobcontrol.exe | grep -i "$scenario/$datecode" | awk '{print $1}'`
        if [[ ( ! -z "$jobno" ) && ( $returncode -ne 111 ) ]] ; then
			echo "Refer to the following link for more details on runtime error"
			echo "https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2023-0/list-of-runtime-error-messages.html"
            echo -e "\n\n\n"
		  banner HAHAHA!
          $NEMS/scripts/jobcontrol.exe status $jobno HaHaHa > /dev/null
          
# adding sed to emailaddress to eliminate space in Dana Van Wagener email address as returned
          if [ "$OSTYPE" != "cygwin" ] ; then
             fromadd=`$NEMS/scripts/emailaddress.sh | sed 's/ //g'`
             echo "A return code of $returncode occurred on run $scenario/$datecode." > nmailmsg.txt
             if [ -f launched.from ] ; then
               cat launched.from >> nmailmsg.txt
               OUTDIR=`grep -i "^OUTDIR=" launched.from | head -1 | cut -c 8-`      
             else
               OUTDIR=$scenario/$datecode
	     fi
             submitter=`grep -i "/$scenario/$datecode" $NEMSJOBLOG/runlog | head -1 | awk '{print $1}'`
             if [ -n "$submitter" ] ; then
               useradd=`$NEMS/scripts/emailaddress.sh $submitter | sed 's/ //g'`
             else
               useradd=$fromadd
             fi
             toadd=`grep -i "hahaha" $NEMS/scripts/nmail.txt | awk '{printf("%s ",$1)}' `
             smtpmail -h relay.eia.gov -f $fromadd -m "nmailmsg.txt" -s "$submitter got code $returncode on run $OUTDIR" $toadd 
             if [ "$useradd" != "$fromadd" ] ; then
               if [ -n "$useradd" ] ; then
                 echo "This is an automated message from NEMS." > nmailmsg.txt
                 echo "An return code of $returncode occurred on run $scenario/$datecode." >> nmailmsg.txt
                 smtpmail -h relay.eia.gov -f $fromadd -m "nmailmsg.txt" -s "$submitter got code $returncode on run $OUTDIR" $useradd
               fi
             fi
          fi
        fi
        exit $returncode 
      fi

      grep -i "NRUNS" MOREOPT > nruns; read dum nruns < nruns; rm nruns
      grep -i "DOEMALL" MOREOPT > doemall; read dum doemall < doemall; rm doemall
      grep -i "DOAMINOF" MOREOPT > doaminof; read dum doaminof < doaminof; rm doaminof
      grep -i "MINSCORE" JCL.DAT > minscore; IFS='='; read dum minscore < minscore; rm minscore;IFS=' '
      echo "A review for cycle $n:  NRUNS is $nruns; DOEMALL is $doemall; DOAMINOF is $doaminof; MINSCORE is $minscore"

#  Do convergence testing:  this cycle compared to previous cycle
      echo "Start intercycle convergence check for cycle $n:  " `date +%T`
      intercv.exe $minscore < intercvfiles.txt > intercvout.txt
      iret=$?
      echo "Intercv return code=$iret"
      echo "End intercycle convergence check for cycle $n:  " `date +%T`
# new versions of intercv.exe exit with a return code of 1 if a good score on intercycle convergence
# testing is achieved over the three worst converged years.
# The alternative test is based on a count of mostly Census-level variables out of convergence tolerance.
# THe first grep is for show, the second for a count

#     egrep "&&&[A-Z]: [A-Z][A-Z][A-Z][A-Z][A-Z] |EMETAX|IT_WOP|intercycle 2" intercvout.txt 

      keepgoing=`egrep "&&&[A-Z]: [A-Z][A-Z][A-Z][A-Z][A-Z] |EMETAX|IT_WOP" intercvout.txt | wc -l`
      GPA=`grep "intercycle convergence GPA" intercvout.txt`
      GPA_US=`grep "US:   intercycle convergence GPA" intercvout.txt | sed "s/intercycle convergence GPA=//"`
      GPA_REG=`grep "REG:  intercycle convergence GPA" intercvout.txt | sed "s/intercycle convergence GPA=//"`

      echo "Number of instances failing convergence checks: $keepgoing"
      if [ "$GPA" != "" ] ; then
#       If using the new criteria, set the old criteria (keepgoing) outside of the acceptable range.
        keepgoing=12
        echo "GPA ($GPA_US; $GPA_REG) on a 4 point scale (averaged over 3 worst years) with $minscore considered minimally acceptable."
      fi
      tail -1 intercvout.txt | awk '{print " INTERCV MESSAGE       ", $0}'
      if [ $returncode -eq 0 -a $nruns -gt 1 ] ; then
         echo "Full intercycle convergence check stored in intercvout.$n.txt"
         cp -p intercvout.txt intercvout.$n.txt

         if [ $keepgoing -lt 11 -o $iret -eq 1 ] ; then
            if [ $doemall -eq 0 -a $n -ge $doaminof ] ; then
               echo " The convergence criteria of $minscore was met after $n cycles.  Clapclapclap."
               n=$nruns 
            else
               if [ $n -lt $nruns ] ; then
                  echo " The convergence criteria of $minscore was met in cycle $n but I must cycle on due to user request."
               fi
            fi
         fi
       fi

       if [ $iterdump -eq 1 ] ; then
         if [ -f tfiler.exe ] ; then
           ls -1 restart*unf > restartfiles.txt
           $NEMS/scripts/iterdump.exe 
         fi
       fi
# These copy commands are to cycle output files that can
# be input, or cycled, for the next run.  The primary
# file is the restart file.  Cycling the PMM basis files
# files is intended to speed solution time.
       
      if [ $n -lt $nruns ] ; then
        if [ -f RESTART.unf ] ; then cp RESTART.unf RESTART.IN ; chmod ugo+rw RESTART.IN ; fi
        if [ -f RESTART.txt ] ; then cp RESTART.txt RESTART.IN ; chmod ugo+rw RESTART.IN ; fi
        if [ -f RESTART.rlx ] ; then cp RESTART.rlx RESTART.IN ; chmod ugo+rw RESTART.IN ; fi
        if [ -f BASPMM1.dat ] ; then cp BASPMM1.dat baspmm2 ; chmod ugo+rw baspmm2 ; fi
        if [ -f BAXPMM1.dat ] ; then cp BAXPMM1.dat baxpmm2 ; chmod ugo+rw baxpmm2 ; fi
        if [ -f IMPCURV.txt ] ; then cp IMPCURV.txt SPRFLRT ; chmod ugo+rw SPRFLRT ; fi
        if [ -f COALUNITO.txt ] ; then cp COALUNITO.txt COALUNITS.txt ; chmod ugo+rw COALUNITS.txt ; fi
        cat bas[1-2][0-9][0-9][0-9].dat > basemmi
        cat efd[1-2][0-9][0-9][0-9].dat > basefdi
        cat baspmm*.da > baspmm2 ; chmod ugo+rw baspmm2
        cat baxpmm*.da > baxpmm2 ; chmod ugo+rw baxpmm2
        cat nofiles* > collectnofiles; chmod ugo+rw collectnofiles
        if [ -f epmout.txt ]  ; then cp epmout.txt epmout.$n.txt ; chmod ugo+rw epmout.$n.txt  ; fi
        if [ -f restart.unf ]  ; then cp restart.unf restart.$n.unf ; chmod ugo+rw restart.$n.unf  ; fi
        if [ -f MAINDBUG ] ; then cp MAINDBUG MAINDBUG.$n ; chmod ugo+rw MAINDBUG.$n ; fi
        if [ -f MNPQIT.daf ] ; then cp MNPQIT.daf MNPQIT.$n.daf ; chmod ugo+rw MNPQIT.$n.daf ; fi
        if [ -f MC_COMMON.csv ] ; then cp MC_COMMON.csv MC_COMMON.$n.csv ; chmod ugo+rw MC_COMMON.$n.csv ; fi
        if [ -f INTLDBG.txt ] ; then cp INTLDBG.txt ACT_INTLDBG.txt ; chmod ugo+rw ACT_INTLDBG.txt ; fi
        if [ -f MNEXPECT.txt ] ; then cp MNEXPECT.txt MNEXPECT.$n.txt ; chmod ugo+rw MNEXPECT.$n.txt ; fi
        if [ -f EDBPGRP.txt ] ; then cp EDBPGRP.txt EDBPGRP.$n.txt ; chmod ugo+rw EDBPGRP.$n.txt ; fi
#  These two are also done in hswrk.sh
        if [ -f COAL1CPS ]  ; then rm -f  COAL1CPS     ;   fi
        if [ -f EDN0SLC ]   ; then rm -f  EDN0SLC      ;   fi

# create ran files for intermediate cycles
        if [ -f restart.$n.unf ] ; then
           echo "Running ftab for cycle $n:  " `date +%T`
           sed "s/RESTART\.unf/RESTART\.$n\.unf/" ftab.dat >ftab.$n.dat
           ls -al ftab.$n.dat
           ftab < ftab.$n.dat > nohup2.out
           mv fort.20 fort.$n.20
           mv $scenario.${datecode:1:4}${datecode:7:2}.ran $scenario.$n.${datecode:1:4}${datecode:7:2}.RAN
           ls -al fort.$n.20 $scenario.$n.${datecode:1:4}${datecode:7:2}.RAN 
           echo "End ftab for cycle $n:  " `date +%T`
        fi
      fi

# this would copy individual cycle's fort.20 and ran files, but doesn't work because the the outdir variables are empty
      if [ "$PCoutdir" != "$PCworkdir" ] ; then
         if [ -n $outdir ] ; then
            cp fort.$n.20 $outdir/.
            cp $scenario.$n.${datecode:1:4}${datecode:7:2}.RAN $outdir/.
            ls -l $outdir/*.$n.*
         fi
      fi

    n=`expr $n + 1 `
  
 done
 echo "Running final ftab:  " `date +%T`
 ftab.exe < ftab.dat > nohup2.out
 echo "End final ftab:  " `date +%T`

# Date: 2020-12-28. ADR. Port below code from cycle_par.sh to here.
# Date: 2020-06-20. ADR, EDT.
# We were asked to generate all the tables for a few related purposes, where a more complete data file is needed:
#  1) future Grafnem effort
#  2) testing the performance of a NEMS run (NEMS Validator, regression)
#  3) general need to have a uniform output file for a run, in human-readable format
# Currently, a subset of tables is generated by Ftab, in the "*.api.csv".
# This is indicated in tabreq.txt by 0 next to the table name.
# The goal was to change this config to generate all the tables.
# Additionally, other changes to the ftab.dat (Ftab runtime config) were
# needed to be made, so not to overwrite or duplicate previous work:
#  line 2: use a different file name for the uniform Ftab CSV output file
#  line 6: print all regions
#  line 14: do not make WK1 file
#  line 16: specify path to the new tabreq.txt (has all the tables turned on)
#  line 28: do not generate RAN file
# note that " were used over ' as string designators because ' did not allow for $variable expansion on line 16.
#   https://stackoverflow.com/a/5156322/2236315
# ftab.uni.dat is a config file that has run-time switches for Ftab operation.
# restart.unf (path specified in the ftab.dat by default) gets processed 
# and results aggregated into the output file (uniform Ftab csv).
# If tabreq.txt gets deleted, this uniform file generation will break.
# Note the sed line that replaces the path (starts with "16s") uses # as the arg
# specification, because there was interference between / and \, when specifying a
# path.
# make a copy of tabreq (tabreq_all_tables_on) with all tables on
sed "s/0 /1 /" ./input/tabreq.txt > ./input/tabreq_all_tables_on.txt
FILETABREQ=./input/tabreq_all_tables_on.txt
echo "Running Uni Ftab CSV using $FILETABREQ"
sed "2s/.*/_unif/" ftab.dat > ftab.temp.dat
sed "6s/.*/1               IPREGD     PRINT REGIONS (1=YES)/" ftab.temp.dat > ftab.temp1.dat
sed "14s/.*/0          WK1 FILE SWITCH (1=CALL FWK1 TO CREATE IT)/" ftab.temp1.dat > ftab.temp2.dat
sed "16s#.*#$FILETABREQ#" ftab.temp2.dat > ftab.temp3.dat
sed "28s/.*/0             Switch for graf2000 .ran file/" ftab.temp3.dat > ftab.uni.dat
echo "sed commands applied."
ftab.exe < ftab.uni.dat > nohup2.out
echo "ftab.exe executed."
rm ftab.temp.dat ftab.temp1.dat ftab.temp2.dat ftab.temp3.dat
echo "End Running Uni Ftab CSV"


# check for intercycle oscillations. print messages to console (look for "OSC") and store detailed results in oscillate.csv
echo "OSC Check for intercycle oscillations:  " `date +%T`
# oscillation arguments
#  3: makes RMSDpct the measure used (3:default)
# .99 with option 3, .99 is max ratio of Skipped to Adjacent measure, meaning comparison between skipped cycles are more similar (lower measure) than adjacent
# .5  with option 3, the RMSDpct for Adj cycles must be  at least this big (or else they are too close together to be considered significant).
#  3  min number of oscillations in 3 calculations for 5 last cycles
$NEMS/scripts/oscillate 3 .99 .5 3
 
# check for ladyfile=4 (enhanced RAN file format). If there, create single and comparison RTF files
LADYFILE=`grep -i "^ladyfile=4" scedes.all | head -1 | cut -c 10`
RANTORTF=`grep -i "^RANTORTF=" scedes.all | head -1 | cut -c 10`
if [ "$LADYFILE" -eq 4 ] ; then
 if [ "$RANTORTF" -eq 1 ] ; then
   echo " "
   echo "Creating FTAB .rtf file outputs:  " `date +%T`
   $NEMS/scripts/RanToRTF.exe
   cp Controlfile_comp.txt ControlFile.txt
   $NEMS/scripts/RanToRTF.exe
   cp Controlfile_sngl.txt ControlFile.txt
   ls -l *.rtf
   echo "Done FTAB .rtf file outputs:  " `date +%T`
   echo " "
#   echo "Creating FTAB .xls file output:"
#   sh $NEMS/scripts/RanToExcel.ksh
 fi
fi

if [ -f runit.txt ] ; then
  datecode=`basename $PWD`
  dir2=`dirname $PWD`
  scenario=`basename $dir2`
  jobno=`$NEMS/scripts/jobcontrol.exe | grep -i "$scenario/$datecode" | awk '{print $1}'`
  if [ ! -z "$jobno" ] ; then
    $NEMS/scripts/jobcontrol.exe status $jobno Cleanup > /dev/null
  fi
fi

date
echo "Here is how much space is used (kilobytes) in $PWD :  " `du -ks`
echo "Now it's time to clean up and compress."

# for cyber security, remove executables from folder.
if [ -f ftab.exe ] ; then
#   qftab tries to invoke the ftab.exe in an output folder, so rename ftab.exe as ftab.xxx so qftab can find and rename it later, if necessary
  cp -p ftab.exe ftab.xxx
  echo "created ftab.xxx"
  rm -f ftab.exe
fi
if [ -f tfiler.exe ] ; then
  rm -f tfiler.exe
fi
if [ -f intercv.exe ] ; then
  rm -f intercv.exe
fi
if [ -f nems.exe ] ; then
  rm -f nems.exe
fi


if [ -f udbp.exe -a -f edbpgrp.txt ] ; then
   {
   echo "Putting edbpgrp.txt into emmdb.mdb:  " `date +%T`
   udbp.exe > udbp.out
   returncode3=$?
   echo "Returncode=$returncode3" >> udbp.out
   if [ $returncode3 -eq 0 ] ; then
      rm edbpgrp.txt
   fi 
   rm -f udpb.exe
   echo "End putting edbpgrp.txt into emmdb.mdb:  " `date +%T`
   } &
fi

echo "Start clean up and compress:  " `date +%T`
cleanup.sh
cp -p ftab.final.dat ftab.dat
if [ -f indusa.csv ] ; then
  $NEMS/scripts/trim.sh indusa.csv temp.csv;mv temp.csv indusa.csv
  $NEMS/scripts/trim.sh indreg1.csv temp.csv;mv temp.csv indreg1.csv
  $NEMS/scripts/trim.sh indreg2.csv temp.csv;mv temp.csv indreg2.csv
  $NEMS/scripts/trim.sh indreg3.csv temp.csv;mv temp.csv indreg3.csv
  $NEMS/scripts/trim.sh indreg4.csv temp.csv;mv temp.csv indreg4.csv
  $NEMS/scripts/trim.sh indt.csv temp.csv;mv temp.csv indt.csv
  $NEMS/scripts/trim.sh indm.csv temp.csv;mv temp.csv indm.csv
  $NEMS/scripts/trim.sh indn.csv temp.csv;mv temp.csv indn.csv
fi
  
echo "End clean up and compress:  " `date +%T`
date

# moving validator after clean up process
echo "Launching the Validator"
grep -i "NEMSPYENV" MOREOPT > NEMSPYENV; read dum NEMSPYENV < NEMSPYENV; rm NEMSPYENV
echo ${NEMSPYENV:5}
echo $NEMS
#$NEMS/scripts/run_validator.bat > /dev/null
#./run_validator.bat ${NEMSPYENV:5} > validator.log 2>valiatorDebug.log
cmd /C %cd%/Validator/run_validator.bat ${NEMSPYENV:5} > validator.log 2>validatorDebug.log
#$NEMS/scripts/run_validator.bat ${NEMSPYENV:5} > validator.log 2>valiatorDebug.log
#PWD = R:/workdir/m2_p/d062222k
echo $PWD



echo "Here is how much space is used now:  " `du -ks`
date
if [ ! -f runit.txt ] ; then
  datecode=`basename $PWD`
  dir2=`dirname $PWD`
  scenario=`basename $dir2`
  jobno=`$NEMS/scripts/jobcontrol.exe | grep -i "$scenario/$datecode" | awk '{print $1}'`
  if [ ! -z "$jobno" ] ; then
    $NEMS/scripts/jobcontrol.exe status $jobno Complete > /dev/null
  fi
fi


# send notification that job is done
if [ $NOTIFY -eq 1 ] ; then
  if [ "$OSTYPE" != "cygwin" ] ; then
    submitter=`grep -i "/$scenario/$datecode" $NEMSJOBLOG/runlog | head -1 | awk '{print $1}'`
    if [ -n "$submitter" ] ; then
      fromadd=`$NEMS/scripts/emailaddress.sh | sed 's/ //g'`
      useradd=`$NEMS/scripts/emailaddress.sh $submitter | sed 's/ //g'`
      echo "Sending notification from $fromadd to $useradd"
      echo "This is an automated message from NEMS." > notify.txt
      if [ -f launched.from ] ; then
        OUTDIR=`grep -i "^OUTDIR=" launched.from | head -1 | cut -c 8-`      
        cat launched.from >> notify.txt
      else
        OUTDIR=$scenario/$datecode
      fi
      echo "run $OUTDIR finished return code $returncode ."
      smtpmail -h relay.eia.gov -f $fromadd -m "notify.txt" -s "NEMS Run $OUTDIR finished" $useradd
    fi
  fi
fi
