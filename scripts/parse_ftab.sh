# $Header: M:/default/scripts/RCS/parse_ftab.sh,v 1.2 2016/04/21 15:19:44 dsa Exp $
# script to parse ftab code generating a list of expressions by table/row and
# the variables that feed each expression.

# Step 1:  eliminate comment strings on lines beginning with "!" or with
# end-of-line comments beginning with "!".

grep -v "^ *!" ftab.f | sed 's/!.*$//' > ftab.parse2.txt
echo ftab.parse2.txt created

# Step 2: delete everything but the T-array assignment statements and assemble them
# on single lines
awk -f $NEMS/scripts/parse2_to_3.awk ftab.parse2.txt | sed 's/&//g;s/ //g' > ftab.parse3.txt
echo ftab.parse3.txt created

# Step 3: 
# parse_ftab.f90 reads ftab.parse3.txt and creates ftab.parse4.csv

if [ -f parse_ftab.exe ] ; then
  parse_ftab.exe
else
  $NEMS/scripts/parse_ftab.exe
fi
echo ftab.parse4.csv created

cat ftab.parse4.csv | sort > ftab.parse5.csv
echo ftab.parse5.csv created

cat ftab.parse5.csv | awk 'BEGIN {FS="|"}{printf("%s,%s,%s\n",$1,$2,$4)}' > ftab.parse6.csv
echo ftab.parse6.csv created
