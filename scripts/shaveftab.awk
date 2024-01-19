# $Header: N:\\default\\scripts/RCS/shaveftab.awk,v 1.1 1999/11/17 13:40:02 PKC Exp $
# Script to rid ftab output report of overprint lines with vertical bars.
# also rids report of scenario description text at the begginning.
# This filtering makes the output easier to view on-screen.

BEGIN { i = 0 ; oldline=" ";first=0 ; printit=0;blank=" "}
{ i++   
  line=$0
  printit=0
  cc=substr(line,1,1)
  if (cc == "1")
     {temp="\f" substr(line,2)
      line=temp
     }
  if ( first==0)
     {  j= index(line,"N A T I O N")
        if( j > 1 )
           {first=i 

            } 
     }
  else
     { if (index(oldline,"+")==0 && index(oldline,"___")==0) printit=1
       if( (index(oldline,"___")==0) && (index(line,"___")==0)) printit=1
       if (index(oldline,"+")>0 && index(line,"___")==0) printit=1 
       if (substr(line,1,1)=="+"){
          l=index(line,"|")
          while (l>1)
              {s2=substr(oldline,l,1)
               if (s2==" ") oldline=substr(oldline,1,l-1) "|" substr(oldline,l+1) 
               line=substr(line,1,l-1) " " substr(line,l+1)     
               l=index(line,"|")
              }
        }
       if (match(oldline,/\+ *$/)>0 ) printit=0
        
       if (printit==1)
           {  if(substr(oldline,1,1)=="+") oldline=" " substr(oldline,2)
              print oldline
           } 
      }

  oldline=line
}
END {print oldline}

