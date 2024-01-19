! $Header: M:/default/scripts/RCS/runit.cygwin.f,v 1.4 2016/04/07 18:57:21 dsa Exp $
!****************************************************************
 PROGRAM runit
 ! Program acts as a batch job queue initiator to run jobs in the job log 
 !
   use dfport
   use dflib
   IMPLICIT NONE
   character*8 cPCID









!     f90  /free  /assume:byterecl   /nodebug runit.f  getshare.f










!************************************************
!   data structure for job control record

   type jobcontrol
     integer      JobNumber ! unique jobnumber. 
     integer      RecNum     ! record on which job is stored in jobqueue.daf
     character*15 Scenario 
     character*9  Datecode 
     character*80 OutputDir 
     character*3  UserID
     character*8  PCID      ! PC Launching Job
     character*8  Hostname  ! Preferred Hostname on Job Submission 
     character*5  QueueType ! Queue type (big or small) on the PCID runnning the job
     character*4  QueueID   ! Queue ID of Queue running the job
     character*8  futureuse
     integer      Curiyr 
     integer      Curitr 
     Integer      Imodel 
     character*10 StartTime !(hhmmss.mmm)
     character*8  StartDate  !(ccyymmdd)
     real         CPU_Seconds 
     real         WALL_Seconds 
     character*10 Status ! running, completed, MIA
     character*10 StatusTime !time of last status update,
     integer      Stat_Interval ! status update interval, 
     character*9  Priority      ! job priority (high, normal), 
     character*20 Command ! job command: set priority, suspend, resume, stop).
     integer      NapTime ! not used 
     character*2  nruns   ! number of nems runs in this cycle
     character*2  irun    ! run number in this cycle from 1 to nruns
     character*5  class   ! job class (small, big, or debug) 
   end type jobcontrol
   integer islash
   integer*2 ia,i
   integer endpos
   character*2 numprocessors
   integer maxjob,numjob,jlist(25),njlist,option/0/,onlyjob,numrun,numrunt,numproc
   parameter (maxjob=150)
   integer runmax/2/  ! Could be command line argument or prompt
   integer isort(maxjob)
   character*25 asort(maxjob)
   character*80 errtext
   type (jobcontrol) :: J(maxjob)  ! array variable J is of type jobcontrol--a user-defined type
   type (jobcontrol) :: CurJ       ! single instance of a jobcontrol type to hold current job

   logical copied
   character*100 userlist,userarg*10
   character*9 PCOutdir
   character*9 workpc, workdir*80  ! working dir is place to make runs; pc on which
                                   ! workdir resides must match cPCID (computer on
                                   ! which runit.exe executes)
       
   character*90 unc_dir1,unc_dir2
   character*100 J_File,filen  ! name of job control information file
   character*80 ch_dir ! name of output directory to change to after job completes  
   character*80 tempdir,scendate*30
integer J_record_number ! record number in job control information file for this job
   integer J_num_rec       ! number of records in job control file
   integer M6/6/,m2,nidle ! output unit for messages

   logical lexist
   integer j_reclen
   integer ios ! io status

   Integer J_file_unit/500/   

   character*1 ans,ans4*4,of*4/' of '/

   character*3 User

   integer iret,ipriv,is1,is2
   integer ipick,ijob,l
   character*10 qstatus
   character*254 cmd,users*200
   character*127 testfile,cmdline*254
   LOGICAL(4) CDstatus,ok
   logical isopen

   character*5 queue,QueID*4
   common/q/queue,queid
   character*80 q_file,s_file,b_file,L_file
   character*50 bash ! bash command

   integer(2) iarg,narg
   integer OnHoldTime,itest
   character*50 NEMSJOBLOG,NEMSDIR
   character*10 curdat,curtim*8
   integer vals(8)
   character*8 ctest
   integer*4 freshness,inotbusy
   character*127 argue 
   integer*2 iargue
   integer*4 largue
   character*255 buffer1
   integer*2 number_runit ! number of runit windows going, including this one
   
   character*8 vkey,vval*80,scedes*200
!
!  End of Declarations
!  ===================================================================
!   Set low priority for runit so that all processes it opens inherit that property.  This way, the
!   work done in nems scripts, such as copying and rantortf, should be done in low priority.
   call low_prior

      
! Establish name of Job Queue File--Should be in the NEMS directory as determined from environment string
  nidle=0
  NEMSDIR=' '
  call GETENV("NEMS",NEMSDIR)
  if(NEMSDIR.eq.' ') then
    NEMSDIR='M:/default'
  endif
  
  NEMSJOBLOG=' '
  call GETENV("NEMSJOBLOG",NEMSJOBLOG)
  if(NEMSJOBLOG.eq.' ') then
    NEMSJOBLOG='W:/NEMSJobLog'
  endif
  
  J_File=trim(NEMSJOBLOG)//'/jobqueue.daf' 
  Q_File=trim(NEMSJOBLOG)//'/queue.'
  

  call getenv("USERNAME",User)
  if(User.eq.' ') then
    User='eia'
  endif
  call mlower(User)

! Get PCID (network ID of the current computer) using the built-in HOSTNAME function
  cPCID=' '
  call getenv("COMPUTERNAME",cPCID)
  if(cpcid.eq.' ') then
    iret=HOSTNAM(cPCID)
  endif
  call mlower(cpcid)
  write(m6,*) 'cpcid is = ',cpcid

! set maximum runs at a time based on the number of processors
  call getenv("NUMBER_OF_PROCESSORS",numprocessors)
  read(numprocessors,'(bn,i2)') numproc
  runmax=numproc 
  write(6,'(2a,1x,i2,1x,i2,1x,i2)') 'NUMBER_OF_PROCESSORS,numproc,runmax=',numprocessors,numproc,runmax
 
! check for required argumens queue name,QueID, and working directory
  narg=nargs() !
  write(m6,*) 'number of arguments',narg-1

  largue=1
  cmdline=' '
  do i=1,narg
    iargue=i-1
    call getarg(iargue,argue)
    cmdline(largue:)=(argue)//' '
    largue=largue+len_trim(argue)+1
  enddo
  write(m6,*) 'command line: '
  write(m6,*) trim(cmdline)
  write(m6,*) ' '
  queue=' '

! Error message for Bad Command Line Arguments:
100 format(1x,'syntax:  runit <queue-name> <queue-ID> <workdir> [user1 user2 ...]'/& 
              'where: '/ &
              '  queue-name = big or small'/ &
              '  queue-ID   = xxxx'/ &
              '  workdir    = directory on this machine for temp nems output when'/&
              '               output directory for run is on other machine' / &
              '  user1...   = optional list of 1 or more users allowed (all users by default)'//) 

  if(narg.ge.2) then 
    call GETARG(1,queue) ! get argument #1 (1st argument is #0, the command itself)
    write(m6,*) 'queue name: ',queue
  else
    write(m6,*) 'No queue name given on command line'
    write(m6,100) 
    write(m6,'(a\)') 'ENTER Queue Name (big or small): '
    read(5,'(a)') queue 
  endif
  call mlower(queue)
  if(queue.ne.'big'.and.queue.ne.'small') then
    write(m6,*) 'Invalid queue name given on command line: ',queue
    write(m6,100) 
    write(m6,'(a\)') 'ENTER Queue Name (big or small): '
    read(5,'(a)') queue 
  endif
! Check for Queue ID argument
  if(narg.ge.3) then 
    call GETARG(2,queID) ! get argument #2 
    write(m6,*) 'queue-ID: ',queID
  else
    write(m6,*) 'No queue ID given on command line'
    write(m6,100) 
    write(m6,'(a\)') 'Press ENTER 4 character queue ID: '
    read(5,'(a)') queID 
  endif
  call mlower(queID)

  if(len_trim(queID).ne.4) then
    write(m6,*) 'Invalid queue ID -- must be 4 characters: ',queID
    write(m6,100) 
    write(m6,'(a\)') 'Press ENTER 4 character queue ID: '
    read(5,'(a)') queID 
  endif
  if(narg.lt.4) then
    write(m6,*) 'No working directory given.  Expecting something like:'
    write(m6,*) 'd:/workdir, where directory workdir is shared to the LAN.'
    write(m6,*) 'Also, do not use //PCID/ naming in working directory name.'
    write(m6,'(a\)') 'Enter shared (accessible) working directory: '
    read(5,'(a)') workdir 
  else 
    call getarg(3,workdir)
  endif
  call Mreplace(workdir,'\','/')
  call mlower(workdir)
  l=len_trim(workdir)
  if(workdir(l:l).eq.'/') workdir(l:l)=' '

  unc_dir1=workdir
  call getshare(unc_dir1,1)
write(6,*) 'unc_dir1='//trim(unc_dir1)
  call Mreplace(unc_dir1,'\','/')
  
  if(unc_dir1(1:2).ne.'//') then
    write(m6,*) 'Unable to get UNC name for working directory because it is not shared' 
    write(m6,*) 'does not exist, or does not begin with a drive letter.  Here it is:'
    write(m6,*) trim(workdir)
    write(m6,100) 
    write(m6,'(a\)') 'Press ENTER key to close'
    read(5,'(a)') ans
    stop
  else
    is1=index(unc_dir1(3:),'/')+1
  endif
  workPC=unc_dir1(3:) 
  islash=index(workpc,'/')
  if(islash.gt.1) workpc=workpc(:islash-1)
  call mlower(workpc)
  call mlower(cPCID)
  if(trim(workpc).ne.trim(cPCID)) then
!    write(m6,*) 'Working directory must reside on computer that runit runs on.'
!    write(m6,*) 'Working directory: ',trim(workdir), ' is on ',trim(workpc)
!    write(m6,*) 'Current PCID is : ',cPCID 
!    write(m6,100)
!    write(m6,'(a\)') 'Press ENTER key to close'
!    read(5,'(a)') ans
!    stop 
    write(m6,*) 'Drives are differentfrom where the computer is, but it is fine.'
  endif
! check for optional list of user whose jobs will run .
  userlist=' '
  if(narg.ge.5) then 
      do i=5,narg
        ia=i-1 
        call GETARG(ia,userarg) 
        userlist=trim(userlist)//' '//trim(userarg)
      enddo
      write(m6,*)' Runit will accept jobs from the following users:'
      call mlower(userlist)
      write(m6,*) trim(userlist)
  endif



  if(queID(1:3).eq.'dum') then   ! make this a dummy queue so jobs will be dispatched elsewhere
    read(queID(4:4),'(i1)') runmax
     write(m6,*) 'this queue will run at most, this many jobs:',runmax
  endif

  Q_File=trim(NEMSJOBLOG)//'/queue.'//trim(cPCID)//'.'//trim(queue)//'.'//trim(queID)
  S_File=trim(NEMSJOBLOG)//'/stop.'//trim(cPCID)//'.'//trim(queue)//'.'//trim(queID)
  B_FILE=trim(NEMSJOBLOG)//'/busy.'//trim(cPCID)//'.dat'  ! optional busy file created by performance monitor alerts
  write(m6,*) 'Q_File=',trim(Q_File)
  write(m6,*) 'S_File=',trim(S_File)
  write(m6,*) 'B_File=',trim(B_File)

!   see how many other runits are going.  if so, exit. only one at a time, mister.
! The following command displays the running processes and searches for any with "runit.exe."  The
! command itself generates a line with runit.exe.  If one is going already, the number of lines will
! be 2
    cmd='ps | grep -i "runit.exe" | grep -i "'//trim(queue)//'" | grep -iv "grep"'
    iret=system(cmd)
!
! Sample Output of above command with 2 runit.exe running in queue big   
!  4540  0:00 "M:\default\scripts\runit.exe"  big dum1 d:\workdir
!  3752  0:00 "M:\default\scripts\runit.exe"  big dsa1 d:\workdir
  
  cmd='ps | grep -i "runit.exe" | grep -i "'//trim(queue)//'" | grep -iv "grep" | wc -l > temp.dat'
        
 
   iret=system(cmd)
   inquire(file='temp.dat',exist=lexist)
   if(lexist)then
      open(3,file='temp.dat',status='old',err=399)
      read(3,*,end=399,err=399) number_runit
      
      write(6,'(a,i3,a)') 'number of runit.exe processes for queue='//trim(queue)//' is',number_runit
      if(number_runit.ge.2) then
        close(3,status='delete')
        write(6,'(a)') ' too many runit programs going for that queue.  Press Enter'
        read(5,'(a)') ans
        stop
      endif
399   continue
      close(3,status='delete')
    endif

! Check if the queue is already running on the machine.  If so report it so and exit.
! Otherwise, enter the pcid and queue name in the queue status log
  inquire(file=Q_File,exist=lexist)
  if(lexist) then
    write(m6,*) 'WARNING: that initiator '//trim(QueID)//' may already be running.  '
    write(m6,*) Q_file
!    write(m6,'(a\)') 'Do you want to try to start anyway? (Enter y or n) :'
!    read(5,'(a)') ans
!    if(ans.ne.'y'.and.ans.ne.'Y') stop
  endif
  L_FILE=trim(q_file)//'.log'
  write(m6,*) 'L_File=',trim(L_File)
  m2=6
  open(2,file=l_file,status='replace',err=95)
  m2=2
  write(m2,*) trim(l_file)
95  continue
      
! enter initial queue status as idle and enter job watch/launch cycle
  qstatus='Idle'
  open(15,file=Q_file,status='replace')
  write(15,'(a)') qstatus
  close(15)
    
! copy the runitnew.sh script over to the working directory
  cmd='cp -f '//trim(NEMSDIR)//'/scripts/runitnew.sh '//trim(workdir) 
  write(6,*) trim(cmd)
  iret=system(cmd)
  write(6,*) 'iret=',iret

 do while(.true.)
  m6=6  
  B_FILE=trim(NEMSJOBLOG)//'/busy.'//trim(cPCID)//'.dat'
  inotbusy=freshness(b_file,120,m6) ! see if busy-file exists and if it is recent.

      inquire(file=Q_File,exist=lexist,err=997)
      if(lexist) then
        qstatus='Idle'
        open(15,file=Q_file,status='replace',err=997)
        call date_and_time(DATE=curdat,VALUES=vals) 
        write(15,'(i2.2,a,i2.2,a,i2.2,5a)') vals(5),':',vals(6),':',vals(7),' '//Queue,' '//queID,' '//qstatus
        close(15)
      endif
997   continue

! The existence of the stop file is a signal to shut down
      inquire(file=S_File,exist=lexist,err=998)  ! if queue stop file is there, exit
      if(lexist) then
        open(15,file=S_file,status='unknown',err=999)
        close(15,status='delete',err=999)
        open(15,file=Q_file,status='replace',err=999)
        close(15,status='delete',err=999)
        stop
      endif 
998   continue

! Get list of jobs awaiting execution from the job log
      m6=6
      call J_Open     ! opens the job file
      call J_ReadAll  ! reads all of the job records and updates job status as MIA for jobs that hasn't been updated recently
      call J_Close    ! closes the file

      call date_and_time(DATE=curdat,VALUES=vals) 
      
      write(m6,'(i2.2,a,i2.2,a,i2.2,5a)') vals(5),':',vals(6),':',vals(7),' '//Queue,' '//queID,' '//qstatus
      nidle=nidle+1 
      if(nidle.eq.1) then 
        write(m2,'(i2.2,a,i2.2,a,i2.2,5a)') vals(5),':',vals(6),':',vals(7),' '//Queue,' '//queID,' '//qstatus
      endif
      call J_Display_Queue


! Find the lowest numbered job on hold, if any, whose Preferred hostname matches the PCID of this Queue.

      ipick=0
      ijob=99999
      numrun=0 ! count of number running in this job queue
      numrunt=0 ! count of total nems running on this machine
      do i=1,numjob
!
        call mlower(j(i)%Hostname)

! current number of jobs running already
        if( (j(i)%Status .eq. 'Running'.or. j(i)%Status .eq. 'Cleanup' ).and. &
            j(i)%Class  .eq. Queue .and. &
            j(i)%Hostname .eq. cPCID) numrun=numrun+1
        if( (j(i)%Status .eq. 'Running'.or. j(i)%Status .eq. 'Cleanup' ) .and. &
            j(i)%Hostname .eq. cPCID) numrunt=numrunt+1
      enddo
      if(numrun.lt.runmax.and.numrunt.lt.2*runmax) then
       do i=1,numjob
! see if any jobs should be run
        if( j(i)%Status .eq. 'On Hold' .and. &
            j(i)%Class  .eq. Queue .and. &
            (userlist.eq.' ' .or. userlist.eq.'all' .or. index(userlist,j(i)%UserID).gt.1) .and. &  ! userid is among list 
           (j(i)%Class.ne.'big'.or.inotbusy.eq.1) .and. &  ! don't run bigs when busy
            j(i)%Hostname .eq. cPCID   ) then

          if(j(i)%JobNumber.lt.ijob) then
! check to make sure I have write access to the output directory. 
            testfile=trim(j(i)%OutputDir)//'/runit.txt'
            ios=0
            open(9,file=testfile,status='unknown',err=75,iostat=ios)
            write(9,'(a)',err=75) 'This job being run by runit.exe.'
            ipick=i
            call pick_job
          endif
        endif
75      continue
       enddo
! if no jobs on hold for the preferred hostname, check other jobs 
! with a preferred host of nem* that have been on hold for
! 1 minute or more 

       if(ipick.eq.0.and.numrun.lt.runmax.and.numrunt.lt.2*runmax) then
        do i=1,numjob
          OnHoldTime=0 
          call j_CheckHoldStatus(J(I))

! (OnHoldTime>0 occurs only with J%Status="On Hold" )
!  Check for preferred host
          itest=len_trim(j(i)%hostname)              ! check for blank or 
          itest=min(itest,ichar(j(i)%hostname(1:1))) ! or ascii 0--unitialized
          call mlower(j(i)%hostname)
          if( OnHoldTime .gt. 60 .and. &
              (j(i)%hostname(1:3).eq.'nem'.or. &
               itest.eq.0).and. &
               (userlist.eq.' ' .or. userlist.eq.'all' .or. index(userlist,j(i)%UserID).gt.1) .and. &  ! userid is among list 
              ( j(i)%Class.ne.'big'.or.inotbusy.eq.1) .and. & ! don't run bigs when busy
              ( j(i)%Class .eq. Queue .or. Queue.eq.'big')) then  ! don't allow small queue to run big jobs, but vice versa is okay

            if(j(i)%JobNumber.lt.ijob) then

! check to make sure I have write access to the output directory. 
              testfile=trim(j(i)%OutputDir)//'/runit.txt '
              ios=0
              open(9,file=testfile,status='unknown',err=76,iostat=ios)
              write(9,'(a)',err=76) 'This job being run by runit.exe.'
              ipick=i
              ijob=j(ipick)%JobNumber
              call pick_job
            endif
          endif
76        continue
        enddo
       endif
      endif
      if(ipick.gt.0) then
         do m6=m2,6,4
           call j_Display(CurJ) ! write display info to q_file
         enddo
         m6=6
! if using Pick option, force run to operating in workdir (which is not backed up and is less likely to have an error 
! caused by the backup-system locking a file. only for big class.
         vval=' '
         if(queue(1:3).eq.'big') then
           vkey='workdir'
           vval='0'
           scedes=trim(curj%outputdir)//'/scedes.all'
           call getkey(scedes,vkey,vval)
           if(vval(1:4).eq.'1') then
             PCOutdir=trim(PCOutdir)//'_'  ! force run to user /workdir by changing the name slightly here.
             write(m6,*)' WORKDIR=1 Option used.  running in workdir'
             inquire(unit=9,opened=isopen,err=775)
             if(isopen) then
               write(9,*,err=77) ' WORKDIR=1 Option used.  running in workdir'
77             continue               
             endif
775          continue
           endif
         else
           call getshare(curj%outputdir,2)
         endif
! by default, use bash from default folder         
         bash='c:\cygwin\bin\bash.exe'
         inquire(file=bash,exist=lexist)
         if(.not. lexist) then
! at EIA, the 64-bit bash is stored here:
           bash='c:\cygwin64\bin\bash.exe'
           inquire(file=bash,exist=lexist)
           if(.not.lexist) then
             write(6,*) trim(bash)//' not found.  Hit Enter to exit'
             read(5,'(a)') ans
             stop
           endif
         endif         
         cmd=trim(bash)//' --login ' &
                   //trim(workdir)//'/runitnew.sh'// &
                ' '//trim(curj%outputdir)// &  ! outdir
                ' '//trim(PCOutdir)//       &  ! PCoutdir
                ' '//trim(workdir)//        &  ! workdir
                ' '//trim(cPCID)//          &  ! PCworkdir
                ' '//trim(curj%scenario)//  &  ! scenario
                ' '//trim(curj%datecode)     ! datecode

         inquire(unit=9,opened=isopen,err=776)
         if(isopen) then
           write(9,'(a)',err=776) trim(cmd)      
         endif
776      continue


         call docreate(cmd,9) ! This executes cmd in background and returns immediately
         

         close(9,err=79) 
79       continue
        
         
      else
        call sleep(5) ! goto sleep for 5 seconds
      endif 
996  continue                
     
   enddo  

999 stop

!  NOTE: 
! the following subroutines "contain"ed in this one share the declarations
!  defined above in this subroutine.
    
   CONTAINS

!========================================================================
   subroutine Pick_Job
   ! Examines jobs on hold and checks if any of them match job queue class criteria.
   ! Assigns 1 to IPICK if a suitable job on hold is found

            ijob=j(ipick)%JobNumber
            curj=j(ipick)
            CurJ%Status='Running'
            CurJ%QueueType=queue
            if(curj%class.ne.queue) curj%QueueType=curj%class
            CurJ%QueueID=QueID
            CurJ%Hostname=cPCID
            Curj%irun=' '
            Curj%nruns=' '
            call Mreplace(curj%outputdir,'\','/')
            call mlower(curj%outputdir)


            ch_dir=curj%outputdir  
! extract scenario and datecode from output directory name.  Will used to create
! like-named working directory on runit PC.
            curj%scenario=' '
            curj%datecode=' '
            endpos=len_trim(ch_dir)
            do i=len_trim(ch_dir),1,-1
              if(ch_dir(i:i).eq.'/') then
                if(endpos.eq.len_trim(ch_dir)) then
                  curj%datecode=ch_dir(i+1:endpos)
                  endpos=i-1
                elseif(endpos.gt.0) then
                  curj%scenario=ch_dir(i+1:endpos)
                  endpos=0
                endif
              endif
            enddo
            do m6=m2,6,4
              write(m6,*) 'Datecode=',trim(curj%datecode)
              write(m6,*) 'Scenario=',trim(curj%scenario)
            enddo
            m6=6
            call J_Open
            call J_WriteOne(CurJ)        
            call J_Close

! make sure that the current directory is in drive letter format because nems demands that in
! when it executes(because it calls batch files that demand a drive letter default working directory)
            call getshare(curj%outputdir,2)
            unc_dir2=curj%outputdir
            call getshare(unc_dir2,1)
            call Mreplace(unc_dir2,'\','/')
            call mlower(unc_dir2)
            is2=index(unc_dir2(3:),'/')+1
            PCoutdir=unc_dir2(3:IS2)

   end subroutine Pick_Job

!======================================================================== 

   subroutine J_Display(K)
      type (jobcontrol) :: K
      write(m6,*)' '
      write(m6,*)'========================================'
      write(m6,*)'   Job Control and Status Information'
      write(m6,*)'========================================'
      write(m6,*) 'Job Number   = ',K%JobNumber
      write(m6,*) 'Record Number= ',K%RecNum
      write(m6,*) 'Scenario     = ',K%Scenario
      write(m6,*) 'Datecode     = ',K%Datecode
      write(m6,*) 'OutputDir    = ',trim(K%OutputDir)
      write(m6,*) 'UserID       = ',K%USERID
      write(m6,*) 'User''s PCID  = ',K%PCID
      write(m6,*) 'Host for Job = ',K%Hostname
      write(m6,*) 'Host Queue ID= ',K%QueueID
      write(m6,*) 'Job Class    = ',K%Class
      write(m6,*) 'Queue Type   = ',K%QueueType
      write(m6,*) 'StartDate    = ',K%StartDate(5:6)//'-'//K%StartDate(7:8)//'-'//K%StartDate(1:4)
      write(m6,*) 'StartTime    = ',K%StartTime(1:2)//':'//K%StartTime(3:4)//':'//K%StartTime(5:6)
      write(m6,'(1x,a,i9)') 'Stat_Interval=',K%Stat_Interval
      write(m6,*) 'Priority     = ',K%Priority
      write(m6,*) 'Command      = ',K%Command
      write(m6,*) 'Status       = ',K%Status 
      write(m6,'(1x,a,i4)') 'Curiyr       = ',K%curiyr
      write(m6,'(1x,a,i4)') 'Curitr       = ',K%curitr
      write(m6,'(1x,a,i4)') 'Imodel       = ',K%imodel
      write(m6,*) 'runs in cycle= ',K%nruns
      write(m6,*) 'run #        = ',K%irun
      write(m6,'(1x,a,f8.0)') 'Cpu_Seconds  = ',K%CPU_Seconds
      write(m6,'(1x,a,f8.0)') 'Wall_Seconds = ',K%Wall_Seconds
      write(m6,*) 'StatusTime   = ',K%StatusTime(1:2)//':'//K%StatusTime(3:4)//':'//K%StatusTime(5:6)
      write(m6,*)'========================================'
      write(m6,*)' '
  end subroutine J_Display
!======================================================================== 
  subroutine J_Close
      call flush(J_file_unit)
      close(J_File_Unit,err=99)

 99   continue
  end subroutine J_Close
! ===============================================
  Subroutine J_Open
    implicit none
    integer ir
    ir=-1 
    J_reclen=260
    lexist=.false.
    inquire(file=J_File, exist=lexist,err=999)
997 continue 
    if(.not. lexist) then
      write(6,'(a)') trim(j_file)//' was not found or is inaccessible'
    else
      open(J_File_Unit,FILE=J_File,STATUS='unknown',action='readwrite',access='direct', &
      SHARE='denynone',RECL=260,SHARED,IOSTAT=ios,ERR=999)
    endif

    return
999 continue
    write(6,'(a,i5,1x,a)') 'Error opening job information file:'//trim(J_File)//&
        ', iostat=',ios
! restart queue and exit. 
    open(15,file=Q_file,status='replace',err=998)
    close(15,status='delete',err=998)
998 continue
    call docreate(cmdline,6)

    if(ios.eq.37) then
! test for record length that doesn't trigger error message 
 9    continue
      ir=ir+1
      if(ir.lt.1500) then
        open(J_File_Unit,FILE=J_File,STATUS='unknown',action='readwrite',access='direct', &
        SHARE='denynone',RECL=ir,SHARED,IOSTAT=ios,ERR=999)
      endif
      write(6,*) 'test for error-free record length=',ir
      write(6,'(a\)') 'Press ENTER key to close'
      read(5,'(a)') ans
    endif
    stop
    return
 end subroutine J_Open
! ===============================================
 subroutine J_ReadAll
   integer i,ios
   logical update
   ios=0
   do i=1,maxjob
     j_record_number=i
     read(j_file_unit,rec=j_record_number,err=99,iostat=ios) J(i) ! first nonexistant record will trigger error branch
   enddo
   do m6=m2,6,4
     write(m6,*,err=99) ' maximum job records found in job information file'
   enddo
   m6=6
   j_record_number=maxjob
99 continue
   if(ios.ne.36.and.ios.ne.0) then
     do m6=m2,6,4
       write(m6,*,err=98) 'error reading '//trim(J_FILE)//' ,ios=',ios, ' j_record_number=',j_record_number
98     continue
     enddo
     m6=6
   endif
   numjob=j_record_number-1
   do i=1,numjob
     j_record_number=i
     update=.false.
     call J_CheckStat(J(i),update)
     if(update) then
       J(i)%Status='MIA'
       write(j_file_unit,rec=j_record_number,err=89) J(i)
89     continue
     endif
   enddo
 
 end subroutine J_ReadAll
!======================================================================== 

 subroutine J_WriteOne(CurJ)
   type (JobControl) :: CurJ
   integer i,ios
   logical update
   ios=0
   j_record_number=CurJ%Recnum
   write(unit=J_File_unit,rec=j_record_number,err=99,iostat=ios) CurJ 
99 continue
   if(ios.ne.36.and.ios.ne.0) then
     do m6=m2,6,4
       write(m6,*) 'error writing job record '//trim(J_FILE)//' ,ios=',ios, ' j_record_number=',j_record_number
     enddo
     m6=6
   endif
   return
 
 end subroutine J_WriteOne
!======================================================================== 

 subroutine J_CheckStat(CurJ,update)
   type (jobcontrol) :: CurJ
   logical update
   character*8 curdat
   character*10 curtim
   integer hr,min,sec,vals(8)
   integer isec 
   integer csec
   integer delay
   update=.false.
 
 
   if(CurJ%Status.eq.'Running'.or.CurJ%Status.eq.'dying'.or.Curj%Status.eq.'Cleanup') then ! last status update should be no longer than a few seconds ago
     call date_and_time(DATE=curdat,VALUES=vals)
     hr=vals(5)
     min=vals(6)
     sec=vals(7)
     csec=hr*3600+min*60+sec  ! number of seconds passed in current day
     read(CurJ%StatusTime,'(i2,i2,i2)',err=99) hr,min,sec
     isec=hr*3600+min*60+sec
  
     delay= 7200  ! after 2 hr delay, figure job has gone missing in action
  
     if(abs(csec-isec).gt. delay.and.csec.gt.delay) then  ! if status hasn't been updated recently
       update=.true.               ! set up for status of MIA
     end if
   end if

   99 continue
   return
 end subroutine J_CheckStat
!======================================================================== 

 subroutine J_CheckHoldStatus(CurJ)
   type (jobcontrol) :: CurJ
   character*8 curdat
   character*10 curtim
   integer hr,min,sec,vals(8)
   integer isec 
   integer csec

 
 
   if(CurJ%Status.eq.'On Hold') then ! last status update should be no longer than a few seconds ago
     call date_and_time(DATE=curdat,VALUES=vals)
     hr=vals(5)
     min=vals(6)
     sec=vals(7)
     csec=hr*3600+min*60+sec  ! number of seconds passed in current day
     read(CurJ%StatusTime,'(i2,i2,i2)',err=99) hr,min,sec
     isec=hr*3600+min*60+sec

     OnHoldTime=csec-isec
  
   end if

   99 continue
   return
 end subroutine J_CheckHoldStatus
!======================================================================== 

 subroutine J_Display_Queue
 character*20 oldcommand
 integer i, form,k
 real perform
 character*5 ajob
 logical recent
 numrun=0
 onlyjob=0
  do i=1,numjob
   recent=J_Recent(j(i)%StatusTime,j(i)%StartDate,300)
   if(j(i)%Status .eq. 'Running   ' .or. &
      j(i)%Status .eq. 'suspended ' .or. &
      j(i)%Status .eq. 'Cleanup   ' .or. &
     (j(i)%Status .eq. 'HaHaHa'   .and. recent) .or. &
     (j(i)%Status .eq. 'Complete' .and. recent) .or. &  ! display recently completed
          j(i)%Status .eq. 'On Hold' .or. &
!         j(i)%status .eq. 'dying' .or. &
          option .eq. '9'   ) then
     numrun=numrun+1
         if(numrun.eq.1) then
       onlyjob=j(i)%JobNumber
     endif
     isort(numrun)=i
     write(ajob,'(i5.5)') j(i)%JobNumber
     if(j(i)%QueueType.eq.' ') then
       j(i)%Queuetype = j(i)%class
     endif
     asort(numrun)=j(i)%hostname//j(i)%QueueType//ajob
   endif
 enddo
 if(numrun.gt.0) then
   write(m6,'(9a)',err=999) ' Job ','User', ' Host     ', '  Year ', 'Run #   Itr', &
   ' Queue Init',' Status  ', 'CPU/Wall',' Output Directory'
   call mnsort(asort,isort,numrun,maxjob)
 
 endif
 do k=1,numrun
   i=isort(k)
   recent=J_Recent(j(i)%StatusTime,j(i)%StartDate,300)
 !  if(j(i)%JobNumber.ne.0.and.
   if(j(i)%Status .eq. 'Running   ' .or. &
      j(i)%Status .eq. 'suspended ' .or. &
      j(i)%Status .eq. 'Cleanup   ' .or. &
     (j(i)%Status .eq. 'HaHaHa'  .and. recent) .or. &
     (j(i)%Status .eq. 'Complete' .and. recent) .or. &  ! display recently completed
          j(i)%Status .eq. 'On Hold' .or. &
!         j(i)%status .eq. 'dying' .or. &
          option .eq. '9'   ) then
      perform=0.
     if(j(i)%WALL_Seconds.gt.0.) then
       perform=j(i)%CPU_Seconds/j(i)%WALL_Seconds  
     endif
     if(j(i)%irun(1:1) .ne. ' ') then
       of=' of '
     else
       of='    '
     endif
     write(m6,'(i4,1x,2a,1x,i6,1x,a2,a4,a2,i3,1x,a,a10,f6.3,1x,a)',err=999) &
             J(i)%JobNumber, & 
             J(i)%USERID, &
       '  '//J(i)%hostname, &
             J(i)%curiyr, &
             J(i)%irun(1:2),of,j(i)%nruns(1:2), &
             J(i)%curitr, &
             J(i)%queuetype//' '//J(i)%QueueID, &
        ' '//J(i)%Status(:9), &
        perform,&
        trim(J(i)%OutputDir)
     if(option.eq.'9') then
       write(6,'(i3,1x,a,1x,a,1x,a,2f10.3,1x,a,1x,a,1x,a,1x,a)') &
           J(i)%recnum,J(i)%startdate, J(i)%starttime,J(i)%statustime, &
           J(i)%cpu_seconds,J(i)%wall_seconds, &
           J(i)%priority,J(i)%command,J(i)%nruns,J(i)%irun
     
         endif
   endif
 enddo
 if(numrun.eq.0) write(m6,*,err=999) 'no jobs running'
999 continue
 return
 end subroutine J_Display_queue
!======================================================================== 

   function J_Recent(StatTime,StatDate,delay)
 ! see if last status time was longer than acceptable delay. if so, not recent
      
     logical J_Recent
     character*8 curdat,StatDate
     character*10 curtim,StatTime
     integer hr,min,sec,vals(8)
     integer isec
     integer csec
     integer delay
     J_Recent=.true.
     

     call date_and_time(DATE=curdat,VALUES=vals)
     if(curdat.ne.StatDate) then
       J_Recent=.false.
       return
     endif
     hr=vals(5)
     min=vals(6)
     sec=vals(7)
     csec=hr*3600+min*60+sec  ! number of seconds passed in current day
     read(StatTime,'(i2,i2,i2)',err=99) hr,min,sec
     isec=hr*3600+min*60+sec
 
     if(abs(csec-isec).gt. delay.and.csec.gt.delay) then  ! if status hasn't been updated recently
       J_Recent=.false.                
     end if
     
99   continue
     return
   end function J_Recent


!========================================================================
! End of Contain ed Routines in Runit

 end program runit


!========================================================================

 function freshness(filen,seconds,m6)
 use dflib
! checks for most recent access time on file given as argument.
! returns 0 errorcode if the file was recently accessed ( < "seconds" ago)
! and returns 1 if most recent file is over "seconds" seconds
 implicit none
 type (file$info) :: filedata
 integer*4 handle
 integer*4 iret 
 integer*4 packtime
 integer freshness
 character*(*) filen
 integer*2 iret2,narg,nfile
 INTEGER(2) iyr, imon, iday, ihr, imin, isec
 INTEGER(2) jyr, jmon, jday, jhr, jmin, jsec, jhun
 integer i
 integer*4 fsecs,csecs,diff,rdiff
 character*8 cdiff
 integer timval(8)
 integer*4 yseconds,seconds
 logical lexist
 integer*4 m6

 freshness=1 ! assume the filen is not recent 

 fsecs=0
 rdiff=seconds ! number of seconds to be considered recent
 inquire(file=filen,exist=lexist,err=99)
  diff=rdiff+100
 if (lexist) then
   handle=file$first
   iret=getfileinfoqq(filen,filedata,handle) ! if error on this line, try compiling with 32-bit.  didn't work with 64-bit version.
   CALL UNPACKTIMEQQ(filedata%lastwrite, iyr, imon, iday, ihr, imin, isec) 
   call date_and_time(VALUES=timval)
   jyr=timval(1)
   jmon=timval(2)
   jday=timval(3)
   jhr=timval(5)
   jmin=timval(6)
   jsec=timval(7)
   csecs=yseconds(jyr,jmon,jday,jhr,jmin,jsec) ! current date/time in seconds
   if(iyr.eq.jyr) then
     fsecs= yseconds(iyr,imon,iday,ihr,imin,isec)  ! find most recent file in bunch
   endif
   
   diff=csecs-fsecs
 
 endif
 if(diff.gt.rdiff) then
   freshness=1  ! file is older than rdiff seconds or doesn't exist
 else
   write(m6,'(2a,i2   ,a, i2,  a,  i4, 1x,i2,  a, i2,   a, i2)') trim(filen),' last written ', &
                 iday,'/',imon,'/',iyr,   ihr,':',imin,':',isec
   freshness=0
 endif
 99 return
 end function freshness
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
 daypmon(3)=28
 if(mod(iyr,4).eq.0)daypmon(3)=29
 if(mod(iyr,100).eq.0)daypmon(3)=28
 if(mod(iyr,400).eq.0)daypmon(3)=29
 days=0
 do i=1,(min(12,imon)-1)
  days=days+daypmon(i)
 enddo
 days=days +min(31,iday)-1
 yseconds=days*86400+ihr*3600+imin*60+isec
 return
 end function yseconds
!=======================================================================
      subroutine Mlower(a)
      implicit none
      character*(*) a
      integer L,j  ,i
      L=len(a)
      do i=1,L
        j=ichar(a(i:i))
        if(j.ge.65.and.j.le.90)then
          a(i:i)=char(j+32)
        endif
      enddo
      return
      end subroutine Mlower
! ===============================================
       subroutine Mreplace(string,finds,repls)
       implicit none
       character*(*) string, finds, repls
       character*255 temp
       integer L,LF,LR,I,leng
       l=len(string)
       lf=len(finds)
       lr=len(repls)
       i=1
10     continue
         if(string(i:i+LF-1).eq.finds) then
           temp=string(i+Lf:)
           string(i:i+LR-1)=repls
           string(i+LR:)=trim(temp)
           l=len(string)
           i=i+LR-1
         endif
         i=i+1
       if(i.le.L-LF+1) goto 10
       return
       end subroutine Mreplace
!========================================================      
     SUBROUTINE MNSORT(A,IDX,N,NMAX)
      IMPLICIT NONE
! SORTS FIRST N ELEMENTS OF CHARACTER ARRAY A.  IT ALSO SORTS AN INDEXING
! ARRAY, IDX, WHICH CAN BE USED TO MAP THE SORT ORDER TO OTHER ARRAYS.  IDX
! SHOULD BE PASSED WITH THE SERIES 1, 2, ... N.
!  THE SORT METHODOLOGY IS CALLED QUICKSORT IS CONSIDERED AN EFFICIENT
!  APPROACH FOR SORTING LARGE ARRAYS

      INTEGER NMAX,N
      CHARACTER*25 A(NMAX),X,W
      INTEGER IDX(NMAX)
      INTEGER STACK(16,2),L,R,S,J,I,II,ITEMP
      S=1
      STACK(1,1)=1
      STACK(1,2)=N
10    CONTINUE
      L=STACK(S,1)
      R=STACK(S,2)
      S=S-1
20    CONTINUE
        I=L
        J=R
        II=(L+R)/2
        X=A(II)
30      CONTINUE
40        CONTINUE
            IF(A(I).LT.X) THEN
               I=I+1
               GO TO 40
            ENDIF
50        CONTINUE
            IF(X.LT.A(J)) THEN
              J=J-1
              GO TO 50
            ENDIF
          IF(I.LE.J) THEN
            W=A(I)
            A(I)=A(J)
            A(J)=W
            ITEMP=IDX(I)
            IDX(I)=IDX(J)
            IDX(J)=ITEMP
            I=I+1
            J=J-1
          ENDIF
          IF(I.LE.J) GO TO 30
        IF((J-L).LT.(R-I)) THEN
          IF(I.LT.R) THEN
            S=S+1
            STACK(S,1)=I
            STACK(S,2)=R
          ENDIF
          R=J
        ELSE
          IF(L.LT.J) THEN
            S=S+1
            STACK(S,1)=L
            STACK(S,2)=J
          ENDIF
          L=I
        ENDIF
        IF(L.LT.R) GO TO 20
      IF(S.GT.0) GO TO 10
      RETURN
      END
 
!**************************************************************************\
!*
!*  subroutine:  doCreate(cmd)
!*  calls WIN32 function CreateProcess() to run a windows executable command in
!   a sub-process.  It is 
!   similar to calling system(cmd) but runs process in the background and returns
!   immediately to calling program.
!*
!* this example drawn from the Visual Fortran Advanced Sample "PROCESS"
!* in \program files\microsoft visual studio\dv98\samples\process\
!*
!**************************************************************************/

subroutine doCreate(cmd,iunit)
use dfwin

integer, parameter :: SIZESTARTUPINFO = 68

character*(*) cmd  ! command line for new process
integer iunit
character*256               buffer  ! copy of cmd as c string

type (T_STARTUPINFO)            sui
type (T_PROCESS_INFORMATION)    pi

logical(4)          bret
logical isopen
integer f/0/
call ZeroMemory(LOC(buffer),256)
iret = lstrcpy(buffer,trim(cmd)//char(0))

!    /* set up the STARTUPINFO structure,
!     *  then call CreateProcess to try and start the new process 
!     */
sui%cb             = SIZESTARTUPINFO  ! 68 ( sizeof(StartUpInfo)
sui%lpReserved       = 0
sui%lpDesktop        = NULL
sui%lpTitle          = NULL
sui%dwX              = 0
sui%dwY              = 0
sui%dwXSize          = 0
sui%dwYSize          = 0
sui%dwXCountChars    = 0
sui%dwYCountChars    = 0
sui%dwFillAttribute  = 0
sui%dwFlags          = StartF_UseShowWindow  ! causes wShowWindow (next line) to be interpreted
sui%wShowWindow      = SW_SHOWMINNOACTIVE    ! causes window to be opened in minimized state and doesn't activate it
sui%cbReserved2      = 0
sui%lpReserved2      = 0


bret = CreateProcess (                              &
                     NULL_CHARACTER,                &  ! exe name
                     buffer,                        &  ! or cmd line   
                     NULL_SECURITY_ATTRIBUTES,      &
                     NULL_SECURITY_ATTRIBUTES,      &
                     F,                             &  ! false
                     CREATE_NEW_CONSOLE,            &
                     NULL,                          &
                     NULL_CHARACTER,                &
                     sui,                           &
                     pi)

isopen=.false.
If(iunit.ne.6.and. iunit.ne.0) then
  inquire(unit=iunit,opened=isopen,err=98)
else
  isopen=.true.
endif
    If(isopen) then
      write(iunit,*,err=99) ' bret from CreateProcess=',bret
99    continue
    endif
98  continue

end
 subroutine low_prior
 ! sets this process to low priority (like a screen saver)
 use dfwin
 integer(HANDLE) p_handle
 integer iret

 p_handle=GetCurrentProcess()
 iret=SetPriorityClass(P_Handle,IDLE_PRIORITY_CLASS)

 return
 end

subroutine getkey(scedes,vkey,vval)
implicit none
! reads scedes file and looks for a given key. if found, returns the value in lower case
character*(*) scedes
character*(*) vkey
character*(*) vval
character*125 line
character*8 key
integer iequal
logical lexist,lopen
inquire(file=scedes,exist=lexist,err=99)
if(lexist) then
  open(22,file=scedes,status='old',readonly,err=99)
  call mlower(vkey)
  do while (.not. eof(22))
    read(22,'(a)',err=99) line
    iequal=index(line,'=')
    if(iequal.gt.0) then
      key=line(1:iequal-1)
      call mlower(key)
      if(trim(vkey).eq.trim(key))then
        vval=line(iequal+1:)
        call mlower(vval)
        close(22)
        return
      endif
    endif
  enddo
  close(22)
endif
99 continue
inquire(22,opened=lopen)
if(lopen)close(22)
return
end
  
