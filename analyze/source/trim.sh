# Trim Program
# $Header: /default/scripts/RCS/trim.sh,v 1.1 1995/05/04 15:30:10 dsa Exp $
#

# sed 's/ *$//' enprod >enprod2
 if [ -z "$1" -o $# -ne 2 ] ; then
   echo " " 
   echo " INSTRUCTIONS for trim"
   echo "   The trim command removes trailing blanks from a file."
   echo "   The input and output files are specified as command "
   echo "   line arguments as follows: "
   echo " "
   echo "        trim inputfile outputfile"
   echo " "
   return 
 fi

 ifile="$1"
 ofile="$2"

 if [ ! -f "$ifile" ] ; then
   echo "The input file, $ifile, does not exist."
   return 
 fi

 if [ "$ifile" = "$ofile" ] ; then
   echo "The input file and output files must differ"
   return
 fi
 if [ -f "$ofile" ] ; then
   echo "The output file, $ofile, already exists."
   read over?"Do you want to overwrite? (y|n) [n] "
   if [ -z "$over" ] ; then
     over="n"
   fi
   if [ "$over" != "y" -a "$over" != "Y" ] ; then
     return
   fi
 fi
#
 echo "trimming $ifile to create $ofile"
 sed "s/ *$//;d//" $ifile > $ofil
