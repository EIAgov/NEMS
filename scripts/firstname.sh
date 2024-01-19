first=`net user "$USERNAME" /DOMAIN  | egrep -i "full name|^comment" | awk '{print $0}'`
echo "$first" 
