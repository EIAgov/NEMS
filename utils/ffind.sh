# script to find a file below the current directory.
TK_ECHO_USE_BASH_BEHAVIOR=1
 file=$1
 if [ -n "$file" ] ; then
   echo "Files with the string $1"
 else
   echo -n "Enter all or part of the file name to search for: ";read file
   echo "Files with the string $file"
 fi
 echo "find command is:  find . -print | fgrep -i ""$file"
 find . -print | fgrep -i "$file" 
  
