#$Header: m:/default/scripts/RCS/tfiler.sh,v 1.23 2018/06/29 20:23:31 pkc Exp $
TK_ECHO_USE_BASH_BEHAVIOR=1
# TFILER script
# create an input file called tfiler.files that looks something like this:

#   Dictionary /default/input/dict.v1.1
#   Restarti   /default/input/restart.v1.1
#   In Format  1
#   Varlist    /default/input/varlist.v1.1
#   Restarto   /u/dsa/trash/tfiler/d0207957/@.restart
#   Out Format 0
#   Filer Obj  /default/objects/filer.v1.1.obj
# make sure they're not in the directory for this script when they execute it.
 wpwd=$PWD
 if [ "$OSTYPE" = "cygwin" ] ; then
   wpwd=`cygpath -m $PWD` 
 fi
 if [ "$wpwd" = "$NEMS/scripts" ]
   then
   echo "     Sorry.  There is a slight problem."
   echo "     Your current directory is  $wpwd."
   echo "     Please execute this from another directory."
   exit 1
 fi
 if [ -f tfiler.files ] ; then 
   grep "Exec" tfiler.files >/dev/null
   if [ $? = "1" ] ; then
     echo -n "The tfiler setup file, tfiler.files, exists.  Remove (y/n)? [n] : ";read rem
     if [ -n "$rem" ] ; then
       case $rem in
       y|Y|yes|YES) rm tfiler.files ;;
       esac 
     fi 
   else
      rm tfiler.files
   fi 
 fi
 if [ ! -f tfiler.files ] ; then
# use the NEMS fdef script to determine the default version of each input file

   sh $NEMS/scripts/fdef.sh dict.txt s    > temp1.$$
   sed "s/.*is //g" temp1.$$ > temp2.$$
   read vers < temp2.$$
   dict="$NEMS/input/dict.v$vers.txt"
   sh $NEMS/scripts/checkfor.z.sh $dict print
   echo "Dictionary $dict" > tfiler.files

   sh $NEMS/scripts/fdef.sh restart.unf s > temp1.$$
   sed "s/.*is //g" temp1.$$ > temp2.$$
   read vers < temp2.$$
   restarti="$NEMS/input/restart.v$vers.unf"
   sh $NEMS/scripts/checkfor.z.sh $restarti print
   echo "Restarti   $restarti" >>tfiler.files
   echo "In Format  1  Unformatted" >> tfiler.files
                           
   sh $NEMS/scripts/fdef.sh varlist.txt s > temp1.$$
   sed "s/.*is //g" temp1.$$ > temp2.$$
   read vers < temp2.$$
   varlist="$NEMS/input/varlist.v$vers.txt"
   sh $NEMS/scripts/checkfor.z.sh $varlist print
   echo "Varlist    $varlist" >>tfiler.files
#                      
   restarto="$wpwd/tfiler.out"
   echo "Restarto   $restarto" >> tfiler.files
   echo "Out Format 0  Formatted" >> tfiler.files

   sh $NEMS/scripts/fdef.sh filer.obj s > temp1.$$
   sed "s/.*is //g" temp1.$$ > temp2.$$
   read vers < temp2.$$
   filer=$NEMS/objects/filer.v$vers.obj
   sh $NEMS/scripts/checkfor.z.sh $filer print
   echo "Filer Obj  $filer" >>tfiler.files
   date=`date`
   echo "$USER $date" >> tfiler.files
   echo "==================================================" >> tfiler.files
   rm temp1.$$
   rm temp2.$$ 
 else
     grep "Dictionary" tfiler.files | head -1 > temp3.$$
     read dummy file < temp3.$$
     if [ -n "$file" ] ; then
        sh $NEMS/scripts/checkfor.z.sh $file
        if [ $? -eq "0" ] ; then
           dict=$file
        else
           echo -n "$file does not exist.  Hit enter to continue.";read ans
        fi
     fi
     newdict="Dictionary $dict" ; 
     sed -e "1,/Dictionary/ s?Dictionary .*?$newdict?" tfiler.files > tfiler.temp ;
     mv tfiler.temp tfiler.files ;
     grep "Restarti" tfiler.files | head -1 > temp3.$$
     read dummy file < temp3.$$
     if [ -n "$file" ] ; then
        sh $NEMS/scripts/checkfor.z.sh $file
        if [ $? -eq "0" ] ; then
           restarti=$file
           file $restarti | awk '{print tolower($0)}' > temp1.$$
           sed "s/.*maybe/1  Unformatted/g;
                s/.*data/1  Unformatted/g;
                s/.*ascii text/0  Formatted/g" temp1.$$ > temp2.$$

           read fmt trans dummy < temp2.$$
           if [ "$fmt" -eq "0" ] ; then
	   #   check if text file is in aimms composite table format
	     echo "checking for composite table"
	     head $restarti | grep -iq "Composite table"
	     if [ $? -eq "0" ] ; then
	       fmt="7"
	     fi
	   fi
           rm temp1.$$
           rm temp2.$$
        else
           echo -n "$file does not exist.  Hit enter to continue.";read ans
        fi
     fi
     ext=""
     ext=${restarti##*.}
     echo "ext = $ext"
     
     newi="Restarti   $restarti" ;
     sed -e "1,/Restarti/ s?Restarti .*?$newi?" tfiler.files > tfiler.temp ;
     mv tfiler.temp tfiler.files  
     sed -e "1,/In Format/ s?In Format .*?In Format  $fmt  $trans?" tfiler.files > tfiler.temp ;
     echo "In Format $fmt"
     mv tfiler.temp tfiler.files ;
     
     
     grep "Varlist" tfiler.files | head -1 > temp3.$$
     read dummy file < temp3.$$
     if [ -n "$file" ] ; then
      sh $NEMS/scripts/checkfor.z.sh $file
      if [ $? -eq "0" ] ; then
           varlist=$file
        else
           echo -n "$file does not exist.  Hit enter to continue.";read ans
        fi
     fi
     newvar="Varlist    $varlist" ;
     sed -e "1,/Varlist/ s?Varlist .*?$newvar?" tfiler.files > tfiler.temp ;
     mv tfiler.temp tfiler.files ;
     
     grep "Filer Obj" tfiler.files | head -1 > temp3.$$
     read dummy dum file < temp3.$$
     if [ -n "$file" ] ; then
        sh $NEMS/scripts/checkfor.z.sh $file
        if [ $? -eq "0" ] ; then
           filer=$file
        else
           echo -n "$file does not exist.  Hit enter to continue.";read ans
        fi
     fi
     newvar="Filer Obj  $filer" 
     sed -e "1,/Filer Obj / s?Filer Obj .*?$newvar?" tfiler.files > tfiler.temp 
     mv tfiler.temp tfiler.files ;
     rm temp3.$$
 fi
   askagain="0"
   while [ "$askagain" != "8" ]
     do
    # clear  
     echo "  TFILER "
     echo " "
     echo "  The contents of the TFILER Setup File is :"
     echo " "
     cat tfiler.files | sed "s/^/    /g" 
     echo " "
     echo "    TFILER Setup Menu"  
     echo
     echo " 1> Dictionary File" 
     echo " 2> Input Restart File" 
     echo " 3> Varlist File" 
     echo " 4> Output Restart File" 
     echo " 5> Format for Output Restart File" 
     echo " 6> Filer Object "
     echo " ======================================="
     echo " 7> Begin TFILER" 
     echo " 8> Quit" 
     echo " "
     echo -n "    Enter Selected Option: [1-8] ";read askagain  
     case $askagain in
       1|D|d) echo -n "    Enter Dictionary File : ";read file  
              if [ -n "$file" ] ; then
                 sh $NEMS/scripts/checkfor.z.sh $file
                 if [ $? -eq "0" ] ; then
                    dict=$file
                 else
                    echo -n "$file does not exist.  Hit enter to continue.";read ans
                 fi
              fi
              newdict="Dictionary $dict" ; 
              sed -e "1,/Dictionary/ s?Dictionary .*?$newdict?" tfiler.files > tfiler.temp ;
              mv tfiler.temp tfiler.files ;;
       2|I|i) echo -n "    Enter Input Restart File : ";read file
              if [ -n "$file" ] ; then
                sh $NEMS/scripts/checkfor.z.sh $file
                if [ $? -eq "0" ] ; then
                    restarti=$file
                    file $restarti | awk '{print tolower($0)}' > temp1.$$
                    sed "s/.*maybe/1  Unformatted/g;
                         s/.*data/1  Unformatted/g;
                         s/.*ascii text/0  Formatted/g" temp1.$$ > temp2.$$

                    read fmt trans dummy < temp2.$$
                    if [ "$fmt" -eq "0" ] ; then
                    #   check if text file is in aimms composite table format
                      echo "checking for composite table"
                      head $restarti | grep -iq "Composite table" 
                      if [ $? -eq "0" ] ; then
                        fmt="7"
                      fi
                    fi
                    rm temp1.$$
                    rm temp2.$$
                 else
                    echo -n "$file does not exist.  Hit enter to continue.";read ans
                 fi
              fi
              newi="Restarti   $restarti" ;
              sed -e "1,/Restarti/ s?Restarti .*?$newi?" tfiler.files > tfiler.temp ;
              mv tfiler.temp tfiler.files  
              if [ "$fmt" -eq "0" ] ; then
	      #   check if text file is in aimms composite table format
	        echo "checking for composite table"
	        head $restarti | grep -iq "Composite table"
	        if [ $? -eq "0" ] ; then
	          fmt="7"
	        fi
	      fi
              sed -e "1,/In Format/ s?In Format .*?In Format  $fmt  $trans?" tfiler.files > tfiler.temp ;
              mv tfiler.temp tfiler.files ;;
       3|V|v) echo -n "    Enter Varlist File : ";read file
              if [ -n "$file" ] ; then
                 sh $NEMS/scripts/checkfor.z.sh $file
                 if [ $? -eq "0" ] ; then
                    varlist=$file
                 else
                    echo -n "$file does not exist.  Hit enter to continue.";read ans
                 fi
              fi
              newvar="Varlist    $varlist" ;
              sed -e "1,/Varlist/ s?Varlist .*?$newvar?" tfiler.files > tfiler.temp ;
              mv tfiler.temp tfiler.files ;;
       4|O|o) echo "Enter Output Restart File" ; read out ;
              newo="Restarto   $out" ;
              sed -e "1,/Restarto/ s?Restarto .*?$newo?" tfiler.files > tfiler.temp ;
              mv tfiler.temp tfiler.files ;;
       5|F|f) echo "     Specify the format for the output restart file:"
              echo "     0  Formatted (text)"
              echo "     1  Unformattted (binary)"
              echo "     2  not available yet"
              echo "     3  PC WK1 format and thus unopenable"
              echo "     4  Comma delimited (csv) format"
              echo "     5  GAMS GDX format"
              echo "     6  not used"
              echo "     7  AIMMS Composite Table format"
              echo -n "     Enter Format (0-7) [0] : ";read ans
              if [ -n "$ans" ] ; then
                fmt=$ans
                if [ $fmt = 1 ] ; then
                  newfmt="Out Format $fmt  Unformatted" ;
                else
                  newfmt="Out Format $fmt  " ;
                fi
              else
                fmt="0"
                newfmt="Out Format $fmt  Formatted" ;
              fi
              sed -e "1,/Out Format/ s?Out Format .*?$newfmt?" tfiler.files > tfiler.temp ;
              mv tfiler.temp tfiler.files ;; 
       6|E|e) echo -n "    Enter Filer Object File : ";read file
              if [ -n "$file" ] ; then
                sh $NEMS/scripts/checkfor.z.sh $file
                if [ $? -eq "0" ] ; then
                    filer=$file
                 else
                    echo -n "$file does not exist.  Hit enter to continue.";read ans
                 fi
              fi
              newvar="Filer Obj  $filer" 
              sed -e "1,/Filer Obj / s?Filer Obj .*?$newvar?" tfiler.files > tfiler.temp 
              mv tfiler.temp tfiler.files ;;

       7|B|b) cat tfiler.files >> $NEMS/logs/tfiler.log 
# link TFILER 
echo " Linking TFILER object code to create an executable"
# --- Get object file defaults, used to create executable
 sh $NEMS/scripts/fdef.sh tfiler.obj s | sed "s/.*is //g" > temp.$$ ; read vers1 < temp.$$ ; rm temp.$$
 vers1="$NEMS/objects/tfiler.v$vers1.obj"
 if [ -f tfiler.obj ] ; then
    vers1="tfiler.obj"
    echo "local tfiler.obj found so it will be used"
 fi
 
 if [ -f filer.obj ] ; then
    filer="filer.obj"
    echo "local filer.obj found so it will be used"
 fi

 sh $NEMS/scripts/fdef.sh fwk1io.obj s | sed "s/.*is //g" > temp.$$ ; read vers4 < temp.$$ ; rm temp.$$
 vers4="$NEMS/objects/fwk1io.v$vers4.obj"
 if [ -f fwk1io.obj ] ; then
   vers4="fwk1io.obj"
   echo "local fwk1io.obj found so it will be used"
 fi

 sh $NEMS/scripts/fdef.sh cio4wk1.obj s | sed "s/.*is //g" > temp.$$ ; read vers5 < temp.$$ ; rm temp.$$
 vers5="$NEMS/objects/cio4wk1.v$vers5.obj"
 if [ -f cio4wk1.obj ] ; then
   vers5="cio4wk1.obj"
   echo "local cio4wk1.obj found so it will be used"
 fi

 sh $NEMS/scripts/fdef.sh gdxf9def.obj s | sed "s/.*is //g" > temp.$$ ; read vers6 < temp.$$ ; rm temp.$$
 vers6="$NEMS/objects/gdxf9def.v$vers6.obj"
 if [ -f gdxf9def.obj ] ; then
   vers6="gdxf9def.obj"
   echo "local gdxf9def.obj found so it will be used"
 fi

 sh $NEMS/scripts/fdef.sh gdxf9glu.obj s | sed "s/.*is //g" > temp.$$ ; read vers7 < temp.$$ ; rm temp.$$
 vers7="$NEMS/objects/gdxf9glu.v$vers7.obj"
 if [ -f gdxf9glu.obj ] ; then
   vers7="gdxf9glu.obj"
   echo "local gdxf9glu.obj found so it will be used"
 fi

 rm -f tfiler.objs
#
OBJS="\
$vers1 \
$filer \
$vers4 \
$vers5 \
$vers6 \
$vers7"
 notthere=0
 for item in $OBJS
 do
    echo "$item" | sed 's/\/\//\\\\/;s/\//\\/g' >> tfiler.objs
    sh $NEMS/scripts/checkfor.z.sh $item print
    if [ $? -ne "0" ] ; then
      notthere=1
    fi
 done
 if [ $notthere -eq 1 ] ;then
   echo "not continuing because of missing object files"
   exit
 fi
   
xilink /subsystem:console /force /out:tfiler.exe /NODEFAULTLIB:"libc" @tfiler.objs 
if [ ! -f gdxdclib64.dll ] ; then
  echo copying gdxdclib64.dll
  cp $NEMS/scripts/gdxdclib64.dll .
fi

echo "Running tfiler: "
              tfiler.exe 
              exit ;;    
       8|Q|q) echo " "
              exit ;; 
       *)     echo "     Option not Available" ; 
              echo -n "     Press <Enter> to Continue";read ans 
              askagain=6 ;;
     esac
 done   
