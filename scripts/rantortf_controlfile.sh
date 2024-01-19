# Creates control file for RanToRTF program.  executed from nemsh.sh or qnems.sh, so it
# expects script variables scenario, datekey, OUTDIR, DATE, and tabreq to be defined.
#
unc_out=`$NEMS/scripts/getshare.exe $OUTDIR`
echo "SnglVer1 - Control File--For Single Run Format"                            > ControlFile_sngl.txt
echo "*next two lines: 1)ran file name and 2)description"                       >> ControlFile_sngl.txt
echo "$unc_out/$scenario/$datekey/$scenario.${datekey:1:4}${datekey:7:1}.RAN"   >> ControlFile_sngl.txt
echo "$scenario $datekey"                                                       >> ControlFile_sngl.txt
echo "*Output file name."                                                       >> ControlFile_sngl.txt
echo "$scenario.single.rtf"                                                     >> ControlFile_sngl.txt
echo "*Page header description:"                                                >> ControlFile_sngl.txt
echo "run $scenario/$datekey"                                                   >> ControlFile_sngl.txt
echo "*Table Numbering Letter:"                                                 >> ControlFile_sngl.txt
echo "A"                                                                        >> ControlFile_sngl.txt
echo "*Page footer description:"                                                >> ControlFile_sngl.txt
echo "Energy Information Administration -- Draft Results -- Internal Use Only"  >> ControlFile_sngl.txt
echo "*Font"                                                                    >> ControlFile_sngl.txt
echo "Arial"                                                                    >> ControlFile_sngl.txt
echo "*Default Point Size (can be half points)"                                 >> ControlFile_sngl.txt
echo "7"                                                                        >> ControlFile_sngl.txt
echo "*Page Orientation (0=Portrait,1=Landscape)"                               >> ControlFile_sngl.txt
echo "0"                                                                        >> ControlFile_sngl.txt
echo "*Top Margin (inches)"                                                     >> ControlFile_sngl.txt
echo "0.50"                                                                     >> ControlFile_sngl.txt
echo "*Bottom Margin (inches)"                                                  >> ControlFile_sngl.txt
echo "0.50"                                                                     >> ControlFile_sngl.txt
echo "*Left Margin (inches)"                                                    >> ControlFile_sngl.txt
echo "0.75"                                                                     >> ControlFile_sngl.txt
echo "*Right Margin (inches)"                                                   >> ControlFile_sngl.txt
echo "0.75"                                                                     >> ControlFile_sngl.txt
echo "*Width of data row label (including dot leader) (inches)"                 >> ControlFile_sngl.txt
echo "2.25"                                                                     >> ControlFile_sngl.txt
echo "*Minimum width for growth rate column (inches, 0=do not include)"         >> ControlFile_sngl.txt
echo "0.6"                                                                      >> ControlFile_sngl.txt
echo "*Number of Years to Display"                                              >> ControlFile_sngl.txt
echo "8"                                                                        >> ControlFile_sngl.txt
echo "*Years to Display (e.g., 2000 - number as above)"                         >> ControlFile_sngl.txt
echo "2009"                                                                     >> ControlFile_sngl.txt
echo "2010"                                                                     >> ControlFile_sngl.txt
echo "2015"                                                                     >> ControlFile_sngl.txt
echo "2020"                                                                     >> ControlFile_sngl.txt
echo "2025"                                                                     >> ControlFile_sngl.txt
echo "2030"                                                                     >> ControlFile_sngl.txt
echo "2035"                                                                     >> ControlFile_sngl.txt
echo "2040"                                                                     >> ControlFile_sngl.txt
echo "*First and Last Year for Growth Rate"                                     >> ControlFile_sngl.txt
echo "2010"                                                                     >> ControlFile_sngl.txt
echo "2040"                                                                     >> ControlFile_sngl.txt
echo "*Number of Tables to Display"                                             >> ControlFile_sngl.txt
echo "21"                                                                       >> ControlFile_sngl.txt
echo "*Tables to Display (RAN number and Publish number)"                       >> ControlFile_sngl.txt
echo "1 1    "                  >> ControlFile_sngl.txt
echo "2 2    "                  >> ControlFile_sngl.txt
echo "3 3    "                  >> ControlFile_sngl.txt
echo "4 4    "                  >> ControlFile_sngl.txt
echo "5 5    "                  >> ControlFile_sngl.txt
echo "6 6    "                  >> ControlFile_sngl.txt
echo "7 7    "                  >> ControlFile_sngl.txt
echo "8 8    "                  >> ControlFile_sngl.txt
echo "9 9    "                  >> ControlFile_sngl.txt
echo "10 10  "                  >> ControlFile_sngl.txt
echo "11 11  "                  >> ControlFile_sngl.txt
echo "12 12  "                  >> ControlFile_sngl.txt
echo "13 13  "                  >> ControlFile_sngl.txt
echo "14 14  "                  >> ControlFile_sngl.txt
echo "15 15  "                  >> ControlFile_sngl.txt
echo "16 16  "                  >> ControlFile_sngl.txt
echo "17 17  "                  >> ControlFile_sngl.txt
echo "18 18  "                  >> ControlFile_sngl.txt
echo "19 19  "                  >> ControlFile_sngl.txt
echo "22 22  "                  >> ControlFile_sngl.txt
echo "23 23  "                  >> ControlFile_sngl.txt
#
#=======================================================================================================
#
grep -i "FGR_RAN1=" scedes.all > /dev/null
iret=$?
if [ $iret == 1 ] ; then
  RNF1=" "
  numran=1
else
  RNF1=`grep "FGR_RAN1=" scedes.all | cut -c 10-100`
  numran=2
  RNL1=`basename $RNF1 .ran` 
  RNL1=${RNL1//\./ }  #change periods to space
  numran=2
fi
# Check for other comparison runs to include
grep -i "FGR_RAN2=" scedes.all > /dev/null
iret=$?
if [ $iret != 1 ] ; then
  RNF2=`grep "FGR_RAN2=" scedes.all | cut -c 10-100`
  if [ "$RNF2" != "none" ] ; then
    RNL2=`basename $RNF2 .ran` 
    RNL2=${RNL2//\./ }  #change periods to space
    numran=3
    # Check for third comparison run
    grep -i "FGR_RAN3=" scedes.all > /dev/null
    iret=$?
    if [ $iret != 1 ] ; then
      RNF3=`grep "FGR_RAN3=" scedes.all | cut -c 10-100`
      if [ "$RNF3" != "none" ] ; then
          RNL3=`basename $RNF3 .ran` 
          RNL3=${RNL3//\./ }  #change periods to space
          numran=4
        else
          #echo "3rd comparison ran file not found: $RNF3"
          RNF3=""
      fi
    else
      #echo "2nd comparison ran file not found: $RNF2"
      RNF2=""
    fi
  else
    #echo "2nd comparison ran file not found: $RNF2"
    RNF2=""
  fi
else
  #echo "2nd comparison ran file not found: $RNF2"
  RNF2=""
fi
#echo "numran=$numran"
#echo "RNF1=$RNF1"
#echo "RNL1=$RNL1"
#echo "RNF2=$RNF2"
#echo "RNL2=$RNL2"
#echo "RNF3=$RNF3"
#echo "RNL3=$RNL3"
#=======================================================================================================
echo "CompVer1 - Control File--For Comparison Format"                            > ControlFile_comp.txt
echo "*Number of input RAN files to use"                                        >> ControlFile_comp.txt
echo "$numran"                                                                  >> ControlFile_comp.txt
echo "*two lines: 1)ran file name and 2)column heading for each RAN file used:" >> ControlFile_comp.txt


if [ $numran -gt 1 ] ; then
# put first comparison file first.  this generally works better (old left, new right)
  echo "$RNF1"                                                                  >> ControlFile_comp.txt
  echo "$RNL1"                                                                  >> ControlFile_comp.txt
# put this run in second:
  echo "$unc_out/$scenario/$datekey/$scenario.${datekey:1:4}${datekey:7:1}.RAN" >> ControlFile_comp.txt
  echo "$scenario $datekey"                                                     >> ControlFile_comp.txt
# now do the rest:
  if [ $numran -gt 2 ] ; then
    echo "$RNF2"                                                                >> ControlFile_comp.txt
    echo "$RNL2"                                                                >> ControlFile_comp.txt
    if [ $numran -gt 3 ] ; then
      echo "$RNF3"                                                              >> ControlFile_comp.txt
      echo "$RNL3"                                                              >> ControlFile_comp.txt
    fi 
  fi
else
# if now comparison files set up, just enter one run to establish minimum input
  echo "$unc_out/$scenario/$datekey/$scenario.${datekey:1:4}${datekey:7:1}.RAN" >> ControlFile_comp.txt
  echo "$scenario $datekey"                                                     >> ControlFile_comp.txt
fi
echo "*Output file name."                                                       >> ControlFile_comp.txt
echo "$scenario.compare.rtf"                                                    >> ControlFile_comp.txt
echo "*Page header description:"                                                >> ControlFile_comp.txt
echo "$unc_out/$scenario/$datekey"                                              >> ControlFile_comp.txt
echo "*Table Numbering Letter:"                                                 >> ControlFile_comp.txt
echo "A"                                                                        >> ControlFile_comp.txt
echo "*Page footer description:"                                                >> ControlFile_comp.txt
echo "Energy Information Administration -- Draft Results -- Internal Use Only"  >> ControlFile_comp.txt
echo "*Font"                                                                    >> ControlFile_comp.txt
echo "Arial"                                                                    >> ControlFile_comp.txt
echo "*Default Point Size (can be half points)"                                 >> ControlFile_comp.txt
echo "7"                                                                        >> ControlFile_comp.txt
echo "*Page Orientation (0=Portrait,1=Landscape)"                               >> ControlFile_comp.txt
echo "0"                                                                        >> ControlFile_comp.txt
echo "*Top Margin (inches)"                                                     >> ControlFile_comp.txt
echo "0.50"                                                                     >> ControlFile_comp.txt
echo "*Bottom Margin (inches)"                                                  >> ControlFile_comp.txt
echo "0.50"                                                                     >> ControlFile_comp.txt
echo "*Left Margin (inches)"                                                    >> ControlFile_comp.txt
echo "0.75"                                                                     >> ControlFile_comp.txt
echo "*Right Margin (inches)"                                                   >> ControlFile_comp.txt
echo "0.75"                                                                     >> ControlFile_comp.txt
echo "*Width of data row label (including dot leader) (inches)"                 >> ControlFile_comp.txt
echo "2.25"                                                                     >> ControlFile_comp.txt
echo "*Number of Single Years to Display"                                       >> ControlFile_comp.txt
echo "1"                                                                        >> ControlFile_comp.txt
echo "*Single Years to Display (e.g., 2000 - number as above)"                  >> ControlFile_comp.txt
echo "2010"                                                                     >> ControlFile_comp.txt
echo "*Number of Combined Years to Display"                                     >> ControlFile_comp.txt
echo "3"                                                                        >> ControlFile_comp.txt
echo "*Years to Display (e.g., 2000 - number as above)"                         >> ControlFile_comp.txt
echo "2020"                                                                     >> ControlFile_comp.txt
echo "2030"                                                                     >> ControlFile_comp.txt
echo "2040"                                                                     >> ControlFile_comp.txt
echo "*Number of Tables to Display"                                             >> ControlFile_comp.txt
echo "21"                                                                       >> ControlFile_comp.txt
echo "*Tables to Display (RAN number and Publish number)"                       >> ControlFile_comp.txt
echo "1 1    "                  >> ControlFile_comp.txt
echo "2 2    "                  >> ControlFile_comp.txt
echo "3 3    "                  >> ControlFile_comp.txt
echo "4 4    "                  >> ControlFile_comp.txt
echo "5 5    "                  >> ControlFile_comp.txt
echo "6 6    "                  >> ControlFile_comp.txt
echo "7 7    "                  >> ControlFile_comp.txt
echo "8 8    "                  >> ControlFile_comp.txt
echo "9 9    "                  >> ControlFile_comp.txt
echo "10 10  "                  >> ControlFile_comp.txt
echo "11 11  "                  >> ControlFile_comp.txt
echo "12 12  "                  >> ControlFile_comp.txt
echo "13 13  "                  >> ControlFile_comp.txt
echo "14 14  "                  >> ControlFile_comp.txt
echo "15 15  "                  >> ControlFile_comp.txt
echo "16 16  "                  >> ControlFile_comp.txt
echo "17 17  "                  >> ControlFile_comp.txt
echo "18 18  "                  >> ControlFile_comp.txt
echo "19 19  "                  >> ControlFile_comp.txt
echo "22 22  "                  >> ControlFile_comp.txt
echo "23 23  "                  >> ControlFile_comp.txt
#
#======================================================================================================
cp ControlFile_Sngl.txt ControlFile.txt
                                                                                           
                                                                                           
