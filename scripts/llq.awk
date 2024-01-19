BEGIN { FS=" " ; 
# $Header: N:\\default\\scripts/RCS/llq.awk,v 1.1 1999/11/17 13:39:56 PKC Exp $ 
        blank=" " 
        i=0 
        njob=0 
# read output of llq command to get todays jobs
        "date +'d%m%d%y'" | getline today
        logfile="/default/logs/@.joblog." today
# figure out yesterday, because some of yesterdays jobs may still
# be executing. 
        day=substr(today,4,2)
        mon=substr(today,2,2)
        year=substr(today,6,2)
        yesterday=day-1
        if (yesterday<1)
          {mon=mon-1
           if (mon==0)
            { mon=12
             year=year-1
             if (year<0) 
               year=99 
            } 
           daysper="31 28 31 30 31 30 31 31 30 31 30 31"
           split(daysper,dayspermon," ")
           yesterday=dayspermon[mon]
           if (mon==2 && (year==96 || year=="00"))
              yesterday="29"
          }
        mon=sprintf("%s",mon)
        if (length(mon)==1)
          mon="0" mon
        yesterday=sprintf("%s",yesterday)
        if (length(yesterday)==1)
           yesterday="0" yesterday
        yesterday=sprintf ("d%s%s%s",mon,yesterday,year)
        logfile2="/default/logs/@.joblog." yesterday       

        while (getline <logfile >0)
        { njob=njob+1
          jjob[njob]=$2 ".0"
          jdir[njob]=$10
          jend[njob]=$11
        }
        while (getline <logfile2 >0)
        { njob=njob+1
          jjob[njob]=$2 ".0"
          jdir[njob]=$10
          jend[njob]=$11
        }
# debug:  print out stuff
#        for (ijob=1; ijob<=njob; ++ijob)
#         print ijob,juser[ijob],jjob[ijob],jdir[ijob]
# end of "BEGIN" section
      }
{    if ( $1=="Id" )
     {  printf ("%-14s %-5s Start  End? %s %s %s  Class   NEMS Output Directory\n", $1, $2, $4, $7, $8)
     }
     else
     {
       if ( $1=="------------------------")
       {  printf ("-------------- ----- ----- ----- %s ---------- -------- ---------------------\n", $4)
       }
       else
       { sstr="nems" 
         n=index($1,sstr)
         if ( n>0 )  
         { job=$1
           ijob=1
           ifound=0
           dir=" "
           end=" "
           while( (ijob<=njob) && (ifound==0))
           { if ( job == jjob[ijob] ) 
             {  dir=jdir[ijob]
                end=jend[ijob]
                colon=substr(end,2,1)
                if (colon==":")
                  end="0" end
                end=substr(end,1,5)
                ifound=1
             }
             ijob=ijob+1
           }
           printf ("%-14s %-5s %-5s %-5s %-2s %-10s %-8s %s\n", $1, $2, $4, end, $5, $8, $7, dir)
         }
         else
         { printf ("%s\n", $0)
         } 
       }
     }
}

