BEGIN {FS="gecos\=";
}
{
 m=match($1, " ")
m=m-1
userid=substr($1,1,m)

printf "%s %s\n",userid, $2;
n=n+1;
}
END {
printf "\nNumber of users= %d\n",n}
