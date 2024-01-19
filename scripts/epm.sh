echo $1

awk '
BEGIN { }

  {
   if ($2 == 1){ print $1, $2, $3 }
     else next
   new = "new"
   while (new != "")  {
   old = new
   getline new
   if (new  == "")  {
      { print old }
      }
   }
  } ' $1  | awk '
BEGIN { }

  {
   if ($2 == 1) getline new
   { print $1, $3, new }
  }' | awk ' 
   BEGIN {
 { print "Year   Begin     End" }
 }

  {
   if ($2 + $5 != 0.0) { printf ( "%4s %7s %7s \n", $1, $2, $5 ) }
  }'
