 program sys
! takes command line argument and submits it as a command.
! designed to be run by the mks start command, but redirect output to a file
! eg. 
! sys "echo hi > test.out"
 use dflib
 implicit none
 character*255 cmd
 character*127 argue(10)
 integer*2 narg,iargue,i,largue,m6/6/,lcmd
 integer*2 iclock
 character*5 cclock
 character*35 batname
 integer iret
 character*1 ans
  narg=nargs() !
  largue=1
  cmd=' '
  do i=2,narg
    iargue=i-1
    call getarg(iargue,argue(iargue))
    cmd(largue:)=trim(argue(iargue))//' '
    largue=largue+len_trim(argue(iargue))+1
    if(i.eq.2) lcmd=largue-1
  enddo
  call system_clock(count=iclock)
  write(cclock,'(i5.5)') iclock
  batname='nems'//cclock//'.bat'
  open(9,file=batname,status='unknown')

  write(9,'(a)') trim(cmd)
  close (9)
  
  write(m6,*) 'Will create a background process, '//trim(batname)//', with this command: '
  write(m6,*) trim(cmd)
  if(len_trim(cmd).gt.1) then
    call idle_priority
    !call system(cmd)
    call system(batname)
  endif
!  open(9,file=batname,status='unknown')
!  close(9,status='delete')
 end
!=========================================================================================
 subroutine idle_priority
! set priority to idle class.  This way it doesn't interfere as much with other things
 use dfwin
 implicit none
 integer iret 
 integer(HANDLE) P_Handle

 P_Handle=GetCurrentProcess() ! pseudohandle
 iret=SetPriorityClass(P_Handle,IDLE_PRIORITY_CLASS)
 if(iret.eq.0) then
   write(6,*) ' setting process priority failed'
 endif
 return
 end

