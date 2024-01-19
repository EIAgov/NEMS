# $Header: M:/default/scripts/RCS/cycle_par.sh,v 1.41 2021/04/23 18:15:11 nems_auto Exp $
a=$(grep -h "stored here" launched.from | cut -d ':' -f 2)
b=$(grep -h "stored here" launched.from | cut -d ':' -f 3)
NEMS=$a":"$b
echo "parnems, NEMS repo is:"
echo $NEMS

if [ "$OSTYPE" = "cygwin" ] ; then
   shopt -s nocaseglob
   umask u=rwx,g=rwx,o=rwx
 fi
 grep -i "OMLVERS=" p1/JCL.DAT > omlvers; read omlvers < omlvers; rm omlvers 
 omlvers=${omlvers#*=}
 MPS="c:/oml/$omlvers"
 export MPS
 echo $MPS
 echo "Here is the current PATH string:"
 echo -E $PATH
 echo "Here is the LIB string:"
 echo -E $LIB
 
 grep -i "NRUNS" p1/MOREOPT > nruns; read dum nruns < nruns; rm nruns
 grep -i "DOEMALL" p1/MOREOPT > doemall; read dum doemall < doemall; rm doemall
 grep -i "DOAMINOF" p1/MOREOPT > doaminof; read dum doaminof < doaminof; rm doaminof
 grep -i "MINSCORE" p1/JCL.DAT > minscore; IFS='='; read dum minscore < minscore; rm minscore;IFS=' '
 grep -i "XPRESSSW" p1/MOREOPT > xpresssw; read dum xpresssw < xpresssw; rm xpresssw
 grep -i "EXCOAL" p1/MOREOPT > excoal; read dum excoal < excoal; rm excoal
 grep -i "EXBUILD" p1/MOREOPT > exbuild; read dum exbuild < exbuild; rm exbuild
 grep -i "ITERDUMP" p1/MOREOPT > iterdump; read dum iterdump < iterdump; rm iterdump
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
 
# remove any pre-existing console output files so we can use ">>" below 
 cd p1;if [ -f nohup.out ] ; then rm -f nohup.out; fi ; cd ..
 cd p2;if [ -f nohup.out ] ; then rm -f nohup.out; fi ; cd ..
 cd p3;if [ -f nohup.out ] ; then rm -f nohup.out; fi ; cd ..


 jobno=`$NEMS/scripts/jobcontrol.exe debug | cut -b 1-5,127- | grep -i "$scenario/$datecode" | awk '{print $1}' | head -1`
 if [ ! -z "$jobno" ] ; then
   $NEMS/scripts/jobcontrol.exe status $jobno nruns=$nruns > /dev/null
 fi

 
 while [ "$n" -le "$nruns" ]
 do
      echo "NEMS Run $n"  
      echo "Run $n Start time:  " `date +%T`
      startsec=`date +%s`

      if [ ! -z "$jobno" ] ; then
        $NEMS/scripts/jobcontrol.exe status $jobno irun=$n > /dev/null
      fi


# Run nems.  the argument $1 is the job class (big, small, debug).      
      cd p1
      echo "NEMS Run $n"  >> nohup.out
	     chmod 777 *
	     echo "here is a list of files in p1 folder"  >> nohup.out
	     ls -l  >> nohup.out
      nems.exe $1  >> nohup.out 2>&1 &
      pid1=$!
      cd ..
      cd p2
      echo "NEMS Run $n"  >> nohup.out
	  	 chmod 777 *
	     echo "here is a list of files in p2 folder"  >> nohup.out
	     ls -l  >> nohup.out
      nems.exe $1 >> nohup.out 2>&1 &
      pid2=$!
      cd ..
# wait until all child processes are done   
      wait $pid1;exit1=$?;echo "exit status part1=$exit1"
      wait $pid2;exit2=$?;echo "exit status part2=$exit2"
      returncode=`expr $exit1 + $exit2 `
      echo "<run $n combined p1 and p2> Returncode=$returncode"
	  
	  
      p1job=`$NEMS/scripts/jobcontrol.exe debug | cut -b 1-5,127- | grep -i "$scenario/$datecode/p1" | awk '{print $1}' | head -1`
      p2job=`$NEMS/scripts/jobcontrol.exe debug | cut -b 1-5,127- | grep -i "$scenario/$datecode/p2" | awk '{print $1}' | head -1`

      if [ ! -z "$p1job" ] ; then
        $NEMS/scripts/jobcontrol.exe status $p1job Complete > /dev/null
      fi
      if [ ! -z "$p2job" ] ; then
        $NEMS/scripts/jobcontrol.exe status $p2job Complete > /dev/null
      fi
       
      if [[ ( $returncode -eq 128 ) || ( $returncode -eq 256 ) ]] ; then
        echo "128 return code could be dll problem. is f90sql from canaimasoft linked in?"
      fi 
      if [ $returncode -ne 0 ] ; then
        datecode=`basename $PWD`
        dir2=`dirname $PWD`
        scenario=`basename $dir2`
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
                 echo "A return code of $returncode occurred on run $scenario/$datecode." >> nmailmsg.txt
                 smtpmail -h relay.eia.gov -f $fromadd -m "nmailmsg.txt" -s "$submitter got code $returncode on run $OUTDIR" $useradd
               fi
             fi
          fi

        fi
        exit $returncode 
      fi

      echo "Start tfiler for cycle $n:  " `date +%T`
      # use tfiler to create combined restart file from parallel runs
      echo "Dictionary input/dict.txt" > tfiler.files
      echo "Restarti   p1/restart.unf" >> tfiler.files
      echo "In Format  1  Unformatted" >> tfiler.files
      echo "Varlist    p1/input/varlist.txt" >> tfiler.files
      if [ $nruns -eq 1 ] ; then
        rm p3/input/restarti.*
        echo "Restarto   p3/input/restarti.unf" >> tfiler.files
        sed '/restarti/s/rlx/unf/g;/restarti/s/RLX/unf/g' p3/filelist > p3/filelist2
        mv -f p3/filelist2 p3/filelist
      else
        echo "Restarto   p3/RESTART.IN" >> tfiler.files
      fi
      echo "Out Format 1" >> tfiler.files
      echo "Filer Obj  na" >> tfiler.files
      echo "date-time-na" >> tfiler.files
      echo "===FYearSubset=0==FYearStart=51==FYearEnd=51===" >> tfiler.files
      echo "p2/restart.unf" >> tfiler.files
      echo " " >> tfiler.files ;# need extra blank line for where it tries to read a second, optional dictionary file.
      tfiler.exe
      echo "End tfiler:  " `date +%T`

#  pass3 :  run barebones nems to calc emissions and make sure totals add up. only one module turned on (ind) so emissions piece will run. 
#           set maxitr to 2 as workaround on industrial module erroneous results on first coupl of iterations.  When that
#           is fixed, change maxitr to "0" to minimize runtime.  Run starts with a merged restart file and 
#           produces an output restart file for this cycle and for copying to restart.in to initialize next parallel cycle.

      cd p3
      cp jcl.dat jcl.backup.dat
# WHEN INDUSTRIAL ITERATION PROBLEM IS FIXED, USE NEXT LINE INSTEAD OF THE ONE AFTER IT.
#     sed "/MAXITR/s/^../0 /" jcl.backup.dat > jcl.dat
      sed "/MAXITR/s/^../2 /" jcl.backup.dat > jcl.dat
      
      echo "NEMS p3 Run $n" 

      echo "NEMS Run $n"  >> nohup.out
      nems.exe small >> nohup.out 2>&1
      cd ..
      
      cp p3/restart.unf .

      p3job=`$NEMS/scripts/jobcontrol.exe debug | cut -b 1-5,127- | grep -i "$scenario/$datecode/p3" | awk '{print $1}' | head -1`
      if [ ! -z "$p3job" ] ; then
        $NEMS/scripts/jobcontrol.exe status $p3job Complete > /dev/null
      fi

      
      grep -i "NRUNS" p1/MOREOPT > nruns; read dum nruns < nruns; rm nruns
      grep -i "DOEMALL" p1/MOREOPT > doemall; read dum doemall < doemall; rm doemall
      grep -i "DOAMINOF" p1/MOREOPT > doaminof; read dum doaminof < doaminof; rm doaminof
      grep -i "MINSCORE" p1/JCL.DAT > minscore; IFS='='; read dum minscore < minscore; rm minscore;IFS=' '
      echo "A review for cycle $n:  NRUNS is $nruns; DOEMALL is $doemall; DOAMINOF is $doaminof; MINSCORE is $minscore"

#  Do convergence testing:  this cycle compared to previous cycle
      echo "Start intercycle convergence check for cycle $n:  " `date +%T`

# change input file name.
      sed 's/ RESTART.IN/ p2\/RESTART.IN/' intercvfiles.txt > intercvfilesnew.txt; mv intercvfilesnew.txt intercvfiles.txt
      intercv.exe $minscore < intercvfiles.txt > intercvout.txt
      iret=$?
      echo "Intercycle return code=$iret"
      cp restart.unf p3/restart.unf ;#  preserve intercv output in the p3 output restart file
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
              # n=$nruns 
               nruns=$n
            else
               if [ $n -lt $nruns ] ; then
                  echo " The convergence criteria of $minscore was met in cycle $n but I must cycle on due to user request."
               fi
            fi
         fi
       fi
       echo "End intercycle convergence check for cycle $n:  " `date +%T`

       if [ $iterdump -eq 1 ] ; then
# run iterdump.exe to concatenate single-iteration restart files for iteration analysis.
# do for each parallel part.
         if [ -f tfiler.exe ] ; then
           if [ ! -f p1/tfiler.exe ] ; then
              cp tfiler.exe p1
           fi
           if [ ! -f p2/tfiler.exe ] ; then
              cp tfiler.exe p2
           fi 
           if [ ! -f p1/ftab.exe ] ; then cp ftab.exe p1 ; fi
           if [ ! -f p2/ftab.exe ] ; then cp ftab.exe p2 ; fi
           cp ftab.dat p1
           cp ftab.dat p2
           cd p1
           ls -1 restart*unf > restartfiles.txt
           $NEMS/scripts/iterdump.exe 
           cd ..
           cd p2
           ls -1 restart*unf > restartfiles.txt
           $NEMS/scripts/iterdump.exe 
           cd ..
        
         fi
       fi

# These copy commands are to cycle output files that can
# be input, or cycled, for the next run.  The primary
# file is the restart file.  Cycling the PMM basis files
# files is intended to speed solution time.
       
      if [ $n -lt $nruns ] ; then
        #================================ p1 ======================================================================        
        if [ -f RESTART.unf ] ; then cp -p RESTART.unf p1/RESTART.IN ; chmod ugo+rw p1/RESTART.IN ; fi
        if [ -f RESTART.txt ] ; then cp -p RESTART.txt p1/RESTART.IN ; chmod ugo+rw p1/RESTART.IN ; fi
        if [ -f RESTART.rlx ] ; then cp -p RESTART.rlx p1/RESTART.IN ; chmod ugo+rw p1/RESTART.IN ; fi
        cd p1
        if [ -f BASPMM1.dat ] ; then cp -p BASPMM1.dat baspmm2 ; chmod ugo+rw baspmm2 ; fi
        if [ -f BAXPMM1.dat ] ; then cp -p BAXPMM1.dat baxpmm2 ; chmod ugo+rw baxpmm2 ; fi
        if [ -f IMPCURV.txt ] ; then cp -p IMPCURV.txt SPRFLRT ; chmod ugo+rw SPRFLRT ; fi
        if [ -f COALUNITO.txt ] ; then cp -p COALUNITO.txt COALUNITS.txt ; chmod ugo+rw COALUNITS.txt ; fi
        cat baspmm*.da > baspmm2 ; chmod ugo+rw baspmm2
        cat baxpmm*.da > baxpmm2 ; chmod ugo+rw baxpmm2
        cat nofiles* > collectnofiles; chmod ugo+rw collectnofiles
        if [ -f epmout.txt ]  ; then cp -p epmout.txt epmout.$n.txt ; chmod ugo+rw epmout.$n.txt  ; fi
        if [ -f restart.unf ]  ; then cp -p restart.unf restart.$n.unf ; chmod ugo+rw restart.$n.unf  ; fi
        if [ -f MAINDBUG ] ; then cp -p MAINDBUG MAINDBUG.$n ; chmod ugo+rw MAINDBUG.$n ; fi
        if [ -f MNPQIT.daf ] ; then cp -p MNPQIT.daf MNPQIT.$n.daf ; chmod ugo+rw MNPQIT.$n.daf ; fi
        if [ -f MC_COMMON.csv ] ; then cp -p MC_COMMON.csv MC_COMMON.$n.csv ; chmod ugo+rw MC_COMMON.$n.csv ; fi
        if [ -f INTLDBG.txt ] ; then cp -p INTLDBG.txt ACT_INTLDBG.txt ; chmod ugo+rw ACT_INTLDBG.txt ; fi
        if [ -f MNEXPECT.txt ] ; then cp -p MNEXPECT.txt MNEXPECT.$n.txt ; chmod ugo+rw MNEXPECT.$n.txt ; fi
#  These two are also done in hswrk.sh
        if [ -f COAL1CPS ]  ; then rm -f  COAL1CPS     ;   fi
        if [ -f EDN0SLC ]   ; then rm -f  EDN0SLC      ;   fi
        cd ..
        #================================ p2 ======================================================================        
        if [ -f RESTART.unf ] ; then cp -p RESTART.unf p2/RESTART.IN ; chmod ugo+rw p2/RESTART.IN ; fi
        if [ -f RESTART.txt ] ; then cp -p RESTART.txt p2/RESTART.IN ; chmod ugo+rw p2/RESTART.IN ; fi
        if [ -f RESTART.rlx ] ; then cp -p RESTART.rlx p2/RESTART.IN ; chmod ugo+rw p2/RESTART.IN ; fi
        cd p2
        if [ -f BASPMM1.dat ] ; then cp -p BASPMM1.dat baspmm2 ; chmod ugo+rw baspmm2 ; fi
        if [ -f BAXPMM1.dat ] ; then cp -p BAXPMM1.dat baxpmm2 ; chmod ugo+rw baxpmm2 ; fi
        if [ -f IMPCURV.txt ] ; then cp -p IMPCURV.txt SPRFLRT ; chmod ugo+rw SPRFLRT ; fi
        if [ -f COALUNITO.txt ] ; then cp -p COALUNITO.txt COALUNITS.txt ; chmod ugo+rw COALUNITS.txt ; fi
        cat bas[1-2][0-9][0-9][0-9].dat > basemmi
        cat efd[1-2][0-9][0-9][0-9].dat > basefdi
        cat baspmm*.da > baspmm2 ; chmod ugo+rw baspmm2
        cat baxpmm*.da > baxpmm2 ; chmod ugo+rw baxpmm2
        cat nofiles* > collectnofiles; chmod ugo+rw collectnofiles
        if [ -f epmout.txt ]  ; then cp -p epmout.txt epmout.$n.txt ; chmod ugo+rw epmout.$n.txt  ; fi
        if [ -f restart.unf ]  ; then cp -p restart.unf restart.$n.unf ; chmod ugo+rw restart.$n.unf  ; fi
        if [ -f MAINDBUG ] ; then cp -p MAINDBUG MAINDBUG.$n ; chmod ugo+rw MAINDBUG.$n ; fi
        if [ -f MNPQIT.daf ] ; then cp -p MNPQIT.daf MNPQIT.$n.daf ; chmod ugo+rw MNPQIT.$n.daf ; fi
        if [ -f MC_COMMON.csv ] ; then cp -p MC_COMMON.csv MC_COMMON.$n.csv ; chmod ugo+rw MC_COMMON.$n.csv ; fi
        if [ -f INTLDBG.txt ] ; then cp -p INTLDBG.txt ACT_INTLDBG.txt ; chmod ugo+rw ACT_INTLDBG.txt ; fi
        if [ -f MNEXPECT.txt ] ; then cp -p MNEXPECT.txt MNEXPECT.$n.txt ; chmod ugo+rw MNEXPECT.$n.txt ; fi
        if [ -f EDBPGRP.txt ] ; then cp EDBPGRP.txt EDBPGRP.$n.txt ; chmod ugo+rw EDBPGRP.$n.txt ; fi
#  These two are also done in hswrk.sh
        if [ -f COAL1CPS ]  ; then rm -f  COAL1CPS     ;   fi
        if [ -f EDN0SLC ]   ; then rm -f  EDN0SLC      ;   fi
        cd ..
        #================================ p3 ======================================================================   
        cd p3
        if [ -f epmout.txt ]  ; then cp -p epmout.txt epmout.$n.txt ; chmod ugo+rw epmout.$n.txt  ; fi
        if [ -f restart.unf ]  ; then cp -p restart.unf restart.$n.unf ; chmod ugo+rw restart.$n.unf  ; fi
        cd ..

# create ran files for intermediate cycles
        if [ -f p3/restart.$n.unf ] ; then
           echo $PWD
           echo "Running ftab for cycle $n:  " `date +%T`
           sed "s/RESTART\.unf/p3\/RESTART\.$n\.unf/" ftab.dat >ftab.$n.dat
           ls -al ftab.$n.dat
           ftab < ftab.$n.dat > nohup2.out
           mv fort.20 fort.$n.20
           mv $scenario.${datecode:1:4}${datecode:7:2}.xml $scenario.$n.${datecode:1:4}${datecode:7:2}.xml
           mv $scenario.${datecode:1:4}${datecode:7:2}.ran $scenario.$n.${datecode:1:4}${datecode:7:2}.RAN
           ls -al fort.$n.20 $scenario.$n.${datecode:1:4}${datecode:7:2}.xml $scenario.$n.${datecode:1:4}${datecode:7:2}.RAN 
           echo "End ftab for cycle $n:  " `date +%T`
        fi
      fi

      if [ "$PCoutdir" != "$PCworkdir" ] ; then
         if [ -n $outdir ] ; then
            cp -p fort.$n.20 $outdir/.
            cp -p $scenario.$n.${datecode:1:4}${datecode:7:2}.xml $outdir/.
            cp -p $scenario.$n.${datecode:1:4}${datecode:7:2}.RAN $outdir/.
         fi
      fi

# get elapsed time and convert to hr:min:sec
      echo "Run $n end time: " `date +%T`
      endsec=`date +%s`
      elapsedsec=`echo "$endsec - $startsec" | bc`
      elapsed=`echo "$endsec - $startsec" | bc`
      elhr=`echo "$elapsed / 3600" | bc`
      if [ $elhr -lt 10 ] ; then elhr="0$elhr" ; fi
      remain=`echo "$elapsed - $elhr * 3600" | bc`
      elmin=`echo "$remain / 60" | bc `
      if [ $elmin -lt 10 ] ; then elmin="0$elmin" ; fi
      elsec=`echo "$elapsed - $elhr * 3600 - $elmin * 60" | bc`
      if [ $elsec -lt 10 ] ; then elsec="0$elsec" ; fi     
      echo "Run $n Total Wall (hr:min:sec) =$elhr:$elmin:$elsec"

 nth=$n
 n=`expr $n + 1 `
  
 done
 
# get option to run a final, additional p1 run so that LFMM can incorporate final AB32 and other allowance prices 
 grep -i "EXTRAP1" scedes.all | sed 's/EXTRAP1=//' > extra; read extrap1 < extra; rm extra 
 echo "extrap1 " $extrap1
 
 if [ "$extrap1" = "1" ] ; then
 # create ran files for nth cycle before final p1 cycle
      if [ -f restart.unf ] ; then
         cp -p restart.unf restart.$nth.unf
         echo "Running ftab for cycle $nth:  " `date +%T`
         cp -p ftab.dat ftab.$nth.dat
         ls -al ftab.$nth.dat
         ftab < ftab.$nth.dat > nohup2.out
         mv fort.20 fort.$nth.20
         mv $scenario.${datecode:1:4}${datecode:7:2}.xml $scenario.$nth.${datecode:1:4}${datecode:7:2}.xml
         mv $scenario.${datecode:1:4}${datecode:7:2}.ran $scenario.$nth.${datecode:1:4}${datecode:7:2}.RAN
         ls -al fort.$nth.20 $scenario.$nth.${datecode:1:4}${datecode:7:2}.xml $scenario.$nth.${datecode:1:4}${datecode:7:2}.RAN 
       fi

       if [ -f RESTART.unf ] ; then cp RESTART.unf p1/RESTART.IN ; chmod ugo+rw p1/RESTART.IN ; fi
       if [ $nruns -eq 1 ] ; then
       # main.f reads restart file from restart.in when nruns>1, so change nruns in moreopt file
          sed '/NRUNS/s/1/2/g' p1/moreopt > p1/moreopt2
          mv -f p1/moreopt2 p1/moreopt
       fi
       cd p1
       if [ -f epmout.txt ]  ; then cp -p epmout.txt epmout.$nth.txt ; chmod ugo+rw epmout.$nth.txt  ; fi
       if [ -f restart.unf ]  ; then cp -p restart.unf restart.$nth.unf ; chmod ugo+rw restart.$nth.unf  ; fi
       if [ -f MAINDBUG ] ; then cp -p MAINDBUG MAINDBUG.$nth ; chmod ugo+rw MAINDBUG.$nth ; fi
       if [ -f MNPQIT.daf ] ; then cp -p MNPQIT.daf MNPQIT.$nth.daf ; chmod ugo+rw MNPQIT.$nth.daf ; fi
       if [ -f MC_COMMON.csv ] ; then cp -p MC_COMMON.csv MC_COMMON.$nth.csv ; chmod ugo+rw MC_COMMON.$nth.csv ; fi
       if [ -f INTLDBG.txt ] ; then cp INTLDBG.txt ACT_INTLDBG.txt ; chmod ugo+rw ACT_INTLDBG.txt ; fi
       if [ -f MNEXPECT.txt ] ; then cp -p MNEXPECT.txt MNEXPECT.$nth.txt ; chmod ugo+rw MNEXPECT.$nth.txt ; fi


       echo "NEMS Run $n  p1 part only"  
       echo "NEMS Run $n"  >> nohup.out
       echo "Run $n Start time:  " `date +%T`
       startsec=`date +%s`

       nems.exe $1 >> nohup.out 2>&1 
       pid1=$?
       echo "<final p1> Returncode=$pid1"

       if [ $iterdump -eq 1 ] ; then
           ls -1 restart*unf > restartfiles.txt
           $NEMS/scripts/iterdump.exe 
       fi 

       # get elapsed time and convert to hr:min:sec
       echo "Run $n end time:  " `date +%T`
       endsec=`date +%s`
       elapsedsec=`echo "$endsec - $startsec" | bc`
       elapsed=`echo "$endsec - $startsec" | bc`
       elhr=`echo "$elapsed / 3600" | bc`
       if [ $elhr -lt 10 ] ; then elhr="0$elhr" ; fi
       remain=`echo "$elapsed - $elhr * 3600" | bc`
       elmin=`echo "$remain / 60" | bc `
       if [ $elmin -lt 10 ] ; then elmin="0$elmin" ; fi
       elsec=`echo "$elapsed - $elhr * 3600 - $elmin * 60" | bc`
       if [ $elsec -lt 10 ] ; then elsec="0$elsec" ; fi     
       echo "Run $n Total Wall (hr:min:sec) =$elhr:$elmin:$elsec"

       cd ..

       cp -p p1/restart.unf .
 
       p1job=`$NEMS/scripts/jobcontrol.exe debug | cut -b 1-5,127- | grep -i "$scenario/$datecode/p1" | awk '{print $1}' | head -1`
       if [ ! -z "$p1job" ] ; then
         $NEMS/scripts/jobcontrol.exe status $p1job Complete > /dev/null
       fi

 fi

#  Do a final intercycle convergence check
      echo "Intercycle convergence check after P1 final cycle, relative to input restart file of last full cycle"

# change input file name.
      sed 's/ RESTART.IN/ p2\/RESTART.IN/' intercvfiles.txt > intercvfilesnew.txt; mv intercvfilesnew.txt intercvfiles.txt
      intercv.exe $minscore < intercvfiles.txt > intercvout.txt
      iret=$?
      echo "Intercycle return code=$iret"

      GPA=`grep "intercycle convergence GPA" intercvout.txt`
      GPA_US=`grep "US:   intercycle convergence GPA" intercvout.txt | sed "s/intercycle convergence GPA=//"`
      GPA_REG=`grep "REG:  intercycle convergence GPA" intercvout.txt | sed "s/intercycle convergence GPA=//"`
      if [ "$GPA" != "" ] ; then
        echo "GPA ($GPA_US; $GPA_REG) after final p1 job."
      fi


 
 ftab.exe < ftab.dat > nohup2.out
 echo "End ftab for final p1:  " `date +%T`
 
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
echo "OSC Check for intercycle oscillations"
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
   echo "Done creating FTAB .rtf file outputs:  " `date +%T`
#   echo "Creating FTAB .xls file output:"
#   sh $NEMS/scripts/RanToExcel.ksh
 fi
fi
 

if [ -f runit.txt ] ; then
  datecode=`basename $PWD`
  dir2=`dirname $PWD`
  scenario=`basename $dir2`
  if [ ! -z "$jobno" ] ; then
    $NEMS/scripts/jobcontrol.exe status $jobno Cleanup > /dev/null
  fi
fi

echo "Here is how much space is used (kilobytes) in $PWD :  " `du -ks`
cp ftab.final.dat ftab.dat

cd p1
if [ -f udbp.exe -a -f edbpgrp.txt ] ; then
   {
   echo "Putting edbpgrp.txt into emmdb.mdb:  " `date +%T`
   udbp.exe > udbp.out
   returncode3=$?
   echo "Returncode=$returncode3" >> udbp.out
   if [ $returncode3 -eq 0 ] ; then
      rm edbpgrp.txt
   fi 
   rm -f udbp.exe
   echo "End putting edbpgrp.txt into emmdb.mdb:  " `date +%T`
   } &
fi

echo "Start clean up and compress of p1:  " `date +%T`
if [ ! -f ../runit.txt ] ; then
  rm -fr input
fi
#if [ -f nems.exe ] ; then
#  rm nems.exe
#fi
{
sh cleanup.sh
if [ -f indusa.csv ] ; then
  sh $NEMS/scripts/trim.sh indusa.csv temp.csv;mv temp.csv indusa.csv
  sh $NEMS/scripts/trim.sh indreg1.csv temp.csv;mv temp.csv indreg1.csv
  sh $NEMS/scripts/trim.sh indreg2.csv temp.csv;mv temp.csv indreg2.csv
  sh $NEMS/scripts/trim.sh indreg3.csv temp.csv;mv temp.csv indreg3.csv
  sh $NEMS/scripts/trim.sh indreg4.csv temp.csv;mv temp.csv indreg4.csv
  sh $NEMS/scripts/trim.sh indt.csv temp.csv;mv temp.csv indt.csv
  sh $NEMS/scripts/trim.sh indm.csv temp.csv;mv temp.csv indm.csv
  sh $NEMS/scripts/trim.sh indn.csv temp.csv;mv temp.csv indn.csv
fi
echo "End clean up and compress of p1:  " `date +%T`
} &

cd ..
cd p2
if [ -f udbp.exe -a -f edbpgrp.txt ] ; then
   {
   echo "Putting edbpgrp.txt into emmdb.mdb:  " `date +%T`
   udbp.exe > udbp.out
   returncode3=$?
   echo "Returncode=$returncode3" >> udbp.out
   if [ $returncode3 -eq 0 ] ; then
      rm edbpgrp.txt
   fi 
   rm -f udbp.exe
   echo "End putting edbpgrp.txt into emmdb.mdb:  " `date +%T`
   } &
fi

echo "Start clean up and compress of p2:  " `date +%T`
if [ ! -f ../runit.txt ] ; then
  rm -rf input
fi
#if [ -f nems.exe ] ; then
#  rm nems.exe
#fi

sh cleanup.sh
if [ -f indusa.csv ] ; then
  sh $NEMS/scripts/trim.sh indusa.csv temp.csv;mv temp.csv indusa.csv
  sh $NEMS/scripts/trim.sh indreg1.csv temp.csv;mv temp.csv indreg1.csv
  sh $NEMS/scripts/trim.sh indreg2.csv temp.csv;mv temp.csv indreg2.csv
  sh $NEMS/scripts/trim.sh indreg3.csv temp.csv;mv temp.csv indreg3.csv
  sh $NEMS/scripts/trim.sh indreg4.csv temp.csv;mv temp.csv indreg4.csv
  sh $NEMS/scripts/trim.sh indt.csv temp.csv;mv temp.csv indt.csv
  sh $NEMS/scripts/trim.sh indm.csv temp.csv;mv temp.csv indm.csv
  sh $NEMS/scripts/trim.sh indn.csv temp.csv;mv temp.csv indn.csv
fi
echo "End clean up and compress of p2:  " `date +%T`

cd ..

cd p3
if [ -f udbp.exe -a -f edbpgrp.txt ] ; then
   {
   echo "Putting edbpgrp.txt into emmdb.mdb:  " `date +%T`
   udbp.exe > udbp.out
   returncode3=$?
   echo "Returncode=$returncode3" >> udbp.out
   if [ $returncode3 -eq 0 ] ; then
      rm edbpgrp.txt
   fi 
   rm -f udbp.exe
   echo "End putting edbpgrp.txt into emmdb.mdb:  " `date +%T`
   } &
fi

echo "Start clean up and compress of p3:  " `date +%T`
#if [ -f nems.exe ] ; then
#  rm nems.exe
#fi
sh cleanup.sh

if [ -f indusa.csv ] ; then
  sh $NEMS/scripts/trim.sh indusa.csv temp.csv;mv temp.csv indusa.csv
  sh $NEMS/scripts/trim.sh indreg1.csv temp.csv;mv temp.csv indreg1.csv
  sh $NEMS/scripts/trim.sh indreg2.csv temp.csv;mv temp.csv indreg2.csv
  sh $NEMS/scripts/trim.sh indreg3.csv temp.csv;mv temp.csv indreg3.csv
  sh $NEMS/scripts/trim.sh indreg4.csv temp.csv;mv temp.csv indreg4.csv
  sh $NEMS/scripts/trim.sh indt.csv temp.csv;mv temp.csv indt.csv
  sh $NEMS/scripts/trim.sh indm.csv temp.csv;mv temp.csv indm.csv
  sh $NEMS/scripts/trim.sh indn.csv temp.csv;mv temp.csv indn.csv
fi
echo "End clean up and compress of p3:  " `date +%T`

cd ..

# for cyber security, remove executables from folder.
#if [ -f ftab.exe ] ; then
#   qftab tries to invoke the ftab.exe in an output folder, so rename ftab.exe as ftab.xxx so qftab can find and rename it later, if necessary
#  cp -p ftab.exe ftab.xxx
#  rm -f ftab.exe
#fi
if [ -f tfiler.exe ] ; then
  rm tfiler.exe
fi
if [ -f intercv.exe ] ; then
  rm intercv.exe
fi


# moving validator after clean up process
echo "Launching the Validator"
grep -i "NEMSPYENV" p1/MOREOPT > NEMSPYENV; read dum NEMSPYENV < NEMSPYENV; rm NEMSPYENV
#echo ${NEMSPYENV:5}
echo $NEMS
#$NEMS/scripts/run_validator.bat > /dev/null
#./run_validator.bat ${NEMSPYENV:5} > validator.log 2>valiatorDebug.log
cmd /C %cd%/Validator/run_validator.bat ${NEMSPYENV:5} > validator.log 2>validatorDebug.log
#$NEMS/scripts/run_validator.bat ${NEMSPYENV:5} > validator.log 2>valiatorDebug.log
#PWD = R:/workdir/m2_p/d062222k
echo $PWD


echo "Here is how much space is used now:  " `du -ks`
if [ ! -f runit.txt ] ; then
  datecode=`basename $PWD`
  dir2=`dirname $PWD`
  scenario=`basename $dir2`
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
