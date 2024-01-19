# invoke by calling the script "gamsmps.sh"
#
# gamsmps.sh is called thusly (for example): gamsmps LFMMfixed.mps LFMMdict.txt LFMMfree.mps
#
# gamsmps.sh calls this awk script as follows:
#   awk -v MPS_In=$MPS_In -v Dict_In=$Dict_In -v MPS_Out=$MPS_Out -f gamsmps.awk fixed.mps
#
#

BEGIN{
    
  NumRows = 0
  NumCols = 0

#  while ( getline <"LFMMdict.txt" > 0 )
  while ( getline <Dict_In > 0 )
  {
    if ((substr($1,1,1)!="*") && (NF>0))   # skip comment lines, blank lines
    { 
      if ($1 == "Equations")
      { InSection = $1 }
      else if ($1=="Variables")
      { InSection = $1 }
      else if (InSection=="Equations")
      {
         RowCode = "e" substr($1,2)
         RowName[RowCode] = $2
      
         NumRows++         
      }
      else if (InSection=="Variables")
      {
         ColCode = $1
         ColName[ColCode] = $2
      
         NumCols++
      }
      else { }
    } 
  }
  close(Dict_In)
  
  RowName["obj"] = "OBJ"
  RowName["c1"] = "ObjectiveFunction"  
   
  # Sections of an MPS file
  SECTION_NAME["NAME"]    = 1
  SECTION_NAME["ROWS"]    = 1
  SECTION_NAME["COLUMNS"] = 1
  SECTION_NAME["RHS"]     = 1
  SECTION_NAME["BOUNDS"]  = 1
  SECTION_NAME["RANGES"]  = 1
  SECTION_NAME["ENDATA"]  = 1
 
  
  #OutputFile = "free_mc6.mps" 
  #print OutputFile
  
}  #end of BEGIN


{                                    # start processing fixed.mps
  if (substr($1,1,1)!="*")           # skip comment lines
  { 
    if ($1 in SECTION_NAME)
    {
      print $0 >> MPS_Out  
      InSection = $1
    }
    else if (InSection=="ROWS")      
    { print " ", $1, RowName[$2] >> MPS_Out }
    
    else if (InSection=="COLUMNS")   
    { print " ", ColName[$1], RowName[$2], $3 >> MPS_Out }
    
    else if (InSection=="RHS")   
    { print " ", $1, RowName[$2], $3 >> MPS_Out }
    
    else if (InSection=="BOUNDS")   
    { print " ", $1, $2, ColName[$3], $4 >> MPS_Out }
      
    else { }
         
 }
}

END {                               # "{" must be on this line, after "END"
#   print "\nNumber of (Rows,Columns): (" NumRows "," NumCols ")"   
}
