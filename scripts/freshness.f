 program freshness
! checks for most recent access time on file(s) given as argument.
! accepts wildcards in filenames
! returns 0 errorcode if the file was recently accessed (<1800 seconds ago)
! and returns 1 if most recent file is over 1800 seconds
 use dflib
 implicit none
 type (file$info) :: filedata
 integer*4 handle
 integer*4 iret 
 integer*4 packtime
 character($MAXPATH) filen
 integer*2 iret2,narg,nfile
 INTEGER(2) iyr, imon, iday, ihr, imin, isec
 INTEGER(2) jyr, jmon, jday, jhr, jmin, jsec, jhun
 integer*2 i
 integer*4 fsecs,csecs,diff,rdiff,fsecs2
 character*8 cdiff
 integer timval(8)
 integer*4 yseconds
 
 logical(4) result
 result=setenvqq("TZ= ")  ! initialize TZ time zone because cygwin sets it to something incompatible with the windows time functions
 
 narg=nargs()
 narg=narg-1
 if(narg.eq.0) then
   write(6,*) ' enter the file name as an argument'

   stop 
 endif

 call date_and_time(VALUES=timval)
 jyr=timval(1)
 jmon=timval(2)
 jday=timval(3)
 jhr=timval(5)
 jmin=timval(6)
 jsec=timval(7)
 csecs=yseconds(jyr,jmon,jday,jhr,jmin,jsec) ! current date/time in seconds

 fsecs=0
 fsecs2=0
 rdiff=1800 ! number of seconds to be considered recent
 nfile=0

 do i=1,narg
   call getarg(i,filen)

! get the last-write time for all files matching the file specs given as first argument
 handle=file$first ! first file matching file specs in filen
 do while(.true.)
   if(handle.eq.file$last.or.handle.eq.file$error) exit
   iret=getfileinfoqq(filen,filedata,handle)  ! if error on this line, try compiling with 32-bit.  didn't work with 64-bit version.
   if(iret.gt.0) then
      nfile=nfile+1
      CALL UNPACKTIMEQQ(filedata%lastwrite, iyr, imon, iday, ihr, imin, isec) 
      if(iyr.eq.jyr) then
        fsecs2= yseconds(iyr,imon,iday,ihr,imin,isec)  ! find most recent file in bunch
      endif
      !write(6,*) trim(filen),fsecs2
      fsecs=max(fsecs,fsecs2)
   endif
   
 enddo
 enddo
 diff=csecs-fsecs
! write(6,*) 'nfile=',nfile
! write(6,*) 'most recent updated file was written ',diff, ' seconds ago'

 if(diff.gt.rdiff) then
   stop 1
 else
   stop 0
 endif
 end
!-------------------------------------------------------
 function yseconds(jyr,jmon,jday,jhr,jmin,jsec)
! returns seconds since start of jyr 
 implicit none
  integer i
 integer(4)iyr,imon,iday,ihr,imin,isec
 integer(2)jyr,jmon,jday,jhr,jmin,jsec
 integer*4 yseconds,fdays
 integer*4 days,daypmon(12)/31,28,31,30,31,30,31,31,30,31,30,31/
 iyr=jyr
 imon=jmon
 iday=jday
 ihr=jhr
 imin=jmin
 isec=jsec
 daypmon(2)=28
 if(mod(iyr,4).eq.0)daypmon(2)=29
 if(mod(iyr,100).eq.0)daypmon(2)=28
 if(mod(iyr,400).eq.0)daypmon(2)=29
 days=0
 do i=1,(min(12,imon)-1)
  days=days+daypmon(i)
 enddo
 days=days +min(31,iday)-1
 yseconds=days*86400+ihr*3600+imin*60+isec
 return
 end
