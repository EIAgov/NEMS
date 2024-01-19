# if userinfo.txt older than two months, replace
#ls -al l:/main/dsa/userinfo.txt

# if argument is "update", just update the text file
if [[ "$1" = "update" ]]; then
  userinfo -D eiahq > l:/main/dsa/userinfo.txt
else

#  find the user
oldone=`find l:/main/dsa -mtime +60 -name "userinfo.txt" -print`
echo $oldone
if [ "$oldone" != "" ] ; then
  echo "Slight delay of 30 seconds to update user info because it is over 60 days old"
  userinfo -D eiahq > l:/main/dsa/userinfo.txt
#  ls -al l:/main/dsa/userinfo.txt
fi
for each in `grep -in $1 l:/main/dsa/userinfo.txt | egrep -i "Comment|Full Name" | sed 's/:.*$//'` ; do
  line=`expr $each + 4`
  head -$line l:/main/dsa/userinfo.txt | tail -9 | egrep -i "^Full|^User|Comment" | sed '/\$/s/Full Name/PCID/;s/\$//;/User Comment/d;s/Comment/_Comment/' | sort
  echo " "
done

fi
