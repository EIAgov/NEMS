# $Header: M:/default/scripts/RCS/checkfor.z.sh,v 1.2 2006/09/27 12:44:32 pkc Exp $

# script to check for file existence. if found, return status 0.  If not,
# check for .Z file.  if it is there, uncompress it and return status 0.
# If not, return status 1
# if any second argument is used, print message if file not found.
 if [ -n "$1" ] ; then
   file="$1"
   if [ -r "$file" ] ; then
     exit 0
   else
     file2="$file.gz"
     if [ -r "$file2" ] ; then
       uncompress $file2
       exit 0
     else
       file2="$file.Z"
       if [ -r "$file2" ] ; then
         uncompress $file2
         exit 0
       else
         if [ -n "$2" ] ; then
            echo "$file not found"
         fi
         exit 1
       fi
     fi
   fi
 else
   echo "This script requires a file name as an argument"
   exit 0
 fi
