# Sets up the input file for the beta RanToExcel program.
TK_ECHO_USE_BASH_BEHAVIOR=1
# Looks for ran file in current output directory.  Gets scenario/datekey from the
# PWD variable.  Checks if the RAN file was
# created with ftab RAN option 4.  if found, proceeds to build the RanToExcel 
# control file (called RanToExcel.txt).

if [ "$OSTYPE" = "cygwin" ] ; then
  wpwd=`cygpath -m $PWD`
else
  wpwd="$PWD"
fi
datecode=`basename $wpwd`
dir2=`dirname $wpwd`
scenario=`basename $dir2`
ran=$scenario.${datecode:1:4}${datecode:7:2}.RAN
ran=ref2022.01.0112a.RAN
echo "ran=$ran"
if [ -f $ran ] ; then
  
# if ran file name extension is in lower case, then it was not created by ftab with ladyfile=4 and
# is therefore not an enhanced ran file

  ran2=`ls -1 $ran `

#    echo $ran
    xls=${ran/.RAN}".xls"
    echo "Control file for RanToExcel program" > RanToExcel.txt
    echo "Name of the RAN input file"          >> RanToExcel.txt
    echo $ran                                  >> RanToExcel.txt
    echo "Name of the Excel output file"       >> RanToExcel.txt
    echo $xls                                  >> RanToExcel.txt

    regional=0
# find tabreq.txt file used in ftab.dat or scedes.all and use it, or prompt for it
    if [ -f ftab.dat ] ; then
      tabreq=`head -16 ftab.dat | tail -1`
      regional=`head -6 ftab.dat | tail -1`
      regional=${regional:0:1}
      ftabbone=`grep -i "bonus rows" ftab.dat`
      ftabbone=${ftabbone:0:1}
    elif [ -f scedes.all ] ; then
      tabreq=`grep -i "layoutn=" scedes.all | sed 's/.*=//'`
      regional=`grep -i "ftabreg=" scedes.all | sed 's/.*=//'`
      ftabbone=`grep -i "ftabbone=" scedes.all | sed 's/.*=//'`
    else
      tabreq=read?"Enter ftab table request file: "
      if [ ! -f "$tabreq" ] ; then
        return
      fi
      ftabbone="1"
    fi
    ntable=`grep '^1 ' $tabreq | wc -l`
 #   echo "$ntable tables requested in ftab table request file: $tabreq "
    if [ "$ntable" -gt 0 ] ; then
      echo "Number of tables to be put in the excel file"   >> RanToExcel.txt
      echo "$ntable"                                        >> RanToExcel.txt
      echo "Tables to be put into the excel file"           >> RanToExcel.txt
      grep '^1 ' $tabreq | sed 's/^1 //;s/\..*$//'          >> RanToExcel.txt
      echo "Number of years to be written out"              >> RanToExcel.txt
      echo "51"                                             >> RanToExcel.txt
      echo "Years to be written out"                        >> RanToExcel.txt      
      awk 'BEGIN{for (i=2000;i<=2050;i++) print i}'         >> RanToExcel.txt
      echo "Option to write out regional versions of tables selected (0:no, 1:yes):" >>RanToExcel.txt
      echo "$regional"                                      >> RanToExcel.txt
      echo "Option to include bonus rows (0:no, 1:yes):"    >> RanToExcel.txt
      echo "$ftabbone"                                      >> RanToExcel.txt                                   
 
      if [ "$OSTYPE" = "cygwin" ] ; then
        sed 's/$'"/`echo \\\r`/" RanToExcel.txt > RanToExcelCRLF.txt
        mv RanToExcelCRLF.txt RanToExcel.txt
      fi

      echo "RanToExcel.txt"
      cat RanToExcel.txt
      echo " "
 
      echo "creating " $xls
      $NEMS/scripts/RanToExcel.exe
    fi
fi
