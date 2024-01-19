# given a host pc name and the number of jobs running on it,
# returns a rating index on which to choose the best computer
# to run on.
# for example:
#   awk -v pc=a23812 -v nj=1 -f pick.awk < m:/default/scripts/host_picks.txt
#
# must use -v to assign variables pc (name of host pc) and nj (number of jobs running )

BEGIN{
      ipc=tolower(pc)
      inj=nj
      count=0
     }
{
 sp=$1    # speed rating of the pc
 maxj=$2  # max number of jobs normally run on this pc
 hostpc=$3
 hostpc=tolower(hostpc)
 host=$4  # owner of the PC
 if (hostpc==ipc && length(hostpc)==length(ipc) ) {
   if (substr(tolower(hostpc),1,3)=="nem" && substr(hostpc,4,1)<=6) {
     overload_penalty= 10*int(inj/maxj)
     rating= sp+inj*.6 + overloaded_penalty
   }
   else  {
     overloaded_penalty= 10*int(inj/maxj)
     rating= sp+1.5*inj + overloaded_penalty
   }
   rating=rating*2
   count=count+1
   exit
   }
}
END{
    if(count==1) printf("%04d\n" ,rating)
    else print 9999
   }
