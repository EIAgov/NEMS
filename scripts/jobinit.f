PROGRAM Jobinit
!============================================================================
! job control initializer--Creates job request record for the nems job queue
! See standalone programs jobcontrol.f and runit.f in scripts directory.
! for more on controlling jobs through these routines.

! compile with /assume:byterecl
!============================================================================
   use dflib
   use dfport
   use dfwin
  
   implicit none
  
   ! data structure for job control record
  
   type jobcontrol
     integer      JobNumber ! unique jobnumber. 
     integer      RecNum     ! record on which job is stored in jobqueue.daf
     character*15 Scenario 
     character*9  Datecode 
     character*80 OutputDir 
     character*3  UserID
     character*8  PCID 
     character*8  Hostname  ! Preferred Hostname on Job Submission 
     character*5  QueueType ! Queue type (big or small) on the PCID runnning the job
     character*4  QueueID   ! Queue ID of Queue running the job
     character*8  partition
     integer      Curiyr 
     integer      Curitr 
     Integer      Imodel 
     character*10 StartTime     !(hhmmss.mmm)
     character*8  StartDate     !(ccyymmdd)
     real         CPU_Seconds 
     real         WALL_Seconds 
     character*10 Status        ! running, completed, MIA
     character*10 StatusTime    !time of last status update,
     integer      Stat_Interval ! status update interval, 
     character*9  Priority      ! job priority (high, normal), 
     character*20 Command       ! job command: set priority, suspend, resume, stop).
     integer      NapTime       !  not used
     character*2  nruns         ! number of nems runs in this cycle
     character*2  irun          ! run number in this cycle from 1 to nruns
     character*5  class         ! job class (small or big)
     end type jobcontrol
! end of job control record fields

   type (jobcontrol) :: J  ! variable J is of type jobcontrol--a user-defined type
   
   character*100 J_File   ! job control information file
   integer J_record_number ! record number in job control information file for this job
   integer J_num_rec       ! number of records in job control file
   integer M6/6/ ! output unit for messages
   logical lexist
   integer j_reclen
   integer ios ! io status
   character*60 errtext
   Integer J_file_unit/500/   
   integer iret
   
   character*5 queue,QueID*4
   common/q/queue,queid

 
   integer call_type
   integer suspendtime     ! number of seconds job is being suspended for
   
   character*50 nemsjoblog
   integer(4) icsec,iwsec ! for calling mptim3 for cpu, wall seconds
  
   logical(4) result
    result=setenvqq("TZ= ")  ! initialize TZ time zone because cygwin sets it to something incompatible with the windows time functions
    
    queue='noneq'
    queid='none' 

    nemsjoblog=' '
    call GETENV("NEMSJOBLOG",nemsjoblog)
    if(nemsjoblog.eq.' ') then
      nemsjoblog='w:/NEMSJobLog'
      lexist=.false.
    endif
    J_File=trim(nemsjoblog)//'/jobqueue.daf' 
    

    call J_Open     ! opens the file
    call J_Init     ! fills and writes the initial job record 
    call J_Close    ! closes the file
    call j_Display 
   
!   
! the following subroutines "contain"ed in this one share the declarations
!  defined here
    
    CONTAINS
    Subroutine J_Open
  
    
      J_reclen=sizeof(J) ! get size of record for open statement
  
      inquire(file=J_File, exist=lexist)
      if(.not. lexist) then
        write(M6,'(a)') trim(j_file)//' does not exist.  will create'
      endif
      open(J_File_Unit,FILE=J_File,STATUS='unknown',action='readwrite',access='direct', &
      SHARE='denynone',RECL=J_reclen,SHARED,IOSTAT=ios,ERR=999)
     
  
      return
999   continue
      call gerror(errtext) ! portability routine to get text of last run time error
      write(m6,'(a,i5,1x,a)') 'Error opening job information file:'//trim(J_File)//&
        ', iostat=',ios, trim(errtext) 
      return
    end subroutine J_Open
! ===============================================================================
    subroutine  J_Init
      integer i,isys/501/,endpos
      TYPE (jobcontrol) :: T
      
      CHARACTER($MAXPATH) dir
      character*2 DefDrive
      INTEGER(4) ldir
      logical lexist, DoneOnce, found
      integer(4) highest_job, ios
      integer(2) narg,iarg
      character*80 arg
      character*2 numprocessors
      character*25 field
      integer pattern

      ios=0 

! check for optional Job Class argument
      j%class='debug'  ! default -- nems executed from command line such as in debug mode without job class argument
      narg=nargs()-1   ! exclude command name, get number of arguments
      if(narg.gt.0) then
         iarg=1
         call GETARG(iarg,arg)  ! get first argument # 1
         if(index(arg,'small').gt.0) j%class='small'
         if(index(arg,'big').gt.0)   j%class='big'
      endif

      !  Get current drive and directory
      Dir  = FILE$CURDRIVE      ! get default drive
      ldir = GETDRIVEDIRQQ(Dir) ! on input, Dir is just Drive; on output Dir is drive/path
      j%hostname=' '
! Check for Optional Command Line Argument for Preferred Hostname for the Job.
! If missing as a command line argument, then it will be assumed as the hostname
! of the computer submitting this job.
      if(narg.gt.1) then
         iarg=2
         call GETARG(iarg,arg)  ! get first argument # 1
         j%hostname=arg
      endif
      if(len_trim(j%hostname).eq.0) then
        iret = system('hostname > hostname.out')
        inquire(exist=lexist,file='hostname.out')
        if(lexist) then
          open(isys,file='hostname.out',status='old',err=22)
          read(isys,'(a)',end=24) j%hostname
24        close(isys,status='delete')
22        continue
        endif
      endif
      call Mlower(j%hostname)

      j%curiyr=0
      j%curitr=0
      j%imodel=0
      j%QueueType= ' '
      j%QueueID=' '
      j%nruns=' '
      j%irun=' '
      
      do i=1,len_trim(dir)
        if(dir(i:i).eq.'\') then
          dir(i:i)='/'
        endif
      enddo
      endpos=len_trim(dir)
      j%datecode=' '
      j%scenario=' '           
      j%partition=' '   

      do i=len_trim(dir),1,-1
        if(dir(i:i).eq.'/') then
          field=dir(i+1:endpos)
          call j_pattern(field,pattern)
          if(pattern.eq.1) then
            j%datecode=field
            endpos=i-1
          elseif(pattern.eq.2) then
            j%partition=field
            endpos=i-1
          elseif(pattern.eq.3) then
            j%scenario=field
            endpos=i-1
            exit
          endif
        endif
      enddo

      write(6,*) 'Datecode=',trim(j%datecode)  
      write(6,*) 'Scenario=',trim(j%scenario)             
      if(len_trim(j%partition).gt.0)then
        write(6,*) 'Partition=',trim(j%partition)          
      endif

      J%OutputDir = trim(Dir)

! if outputdir name is drive:path format, the drive might be
! a mapped network drive.  Convert to UNC notation  (//node/path) 
! so it can be better handled by the runit program.
      if(j%OutputDir(1:2).ne.'//') then
        call getshare(J%OutputDir,1)     
      endif
      j%priority='normal'

      call getenv("USERNAME",J%UserID)
      if(J%UserID.eq.' ') then
         J%UserID='eia'
      endif
      call mlower(j%UserId)
      call getenv("COMPUTERNAME",J%PCID)
      call mlower(j%PCID)

      call date_and_time(J%StartDate,J%StartTime)
      J%CPU_Seconds= 0.
      J%WALL_Seconds=0.
      J%Status = 'On Hold'
      call date_and_time(TIME=J%StatusTime) !time of last status update 
      J%Stat_Interval=5 ! status update interval  in seconds  
      J%NapTime = 0          ! not used now 
      
      J%Command=' ' ! job command: set priority for example)
! see if this is a resubmitted job by checking for pre-existing job record with
! same scenario datecode.  if so, re-use it
      found=.false.
      ios=0
      do i=1,1000
        read(j_file_unit,rec=i,err=89,iostat=ios) T ! first nonexistant record will trigger error branch
        highest_job=max(T%JobNumber,Highest_job)
        call mlower(T%scenario)
        if(T%Scenario.eq.J%scenario .and. T%Datecode.eq.J%Datecode .and. T%Partition.eq.J%Partition) then

           found=.true.
           j_record_number=i
           j%jobnumber=T%jobnumber
           j%recnum=i
           exit ! breakout of do loop
        endif
      enddo
 89   continue
      if(ios.ne.0.and.ios.ne.36) then
        write(6,*) 'job record read error, iostat=',ios
      endif


      if(.not.found) then  ! if no pre-existing job record for this scenario/datecode...
!  determine last record in direct access file or any record that can be reused.
!  reusable records have a status of "complete" or "MIA"

        highest_job=0
        J_record_number=0
        doneonce=.false.
        do i=1,1000
          read(j_file_unit,rec=i,err=99,iostat=ios) T ! first nonexistant record will trigger error branch
          highest_job=max(T%JobNumber,Highest_job)
          if(.not.doneonce) then
            if (T%Status.eq.'complete' .or. &
                T%Status.eq.'Complete' .or. &
                T%Status.eq.'HaHaHa' .or. &
                T%jobnumber.lt.1 .or. &
                T%Status.eq.' ') then
                if(.not. J_Recent(T%StatusTime,T%StartDate,600)) then
                     j_record_number=i
                     doneonce=.true.
                endif
            elseif(T%Status.eq.'MIA') then
                if(.not. J_Recent(T%StatusTime,T%StartDate,14000)) then
                     j_record_number=i
                     doneonce=.true.
                endif
            endif
          endif
        enddo
 99     continue
        if(ios.ne.0.and.ios.ne.36) then
          write(6,*) 'job record read error, iostat=',ios
        endif
        if(.not. doneonce) then
           j_record_number=i
          write(6,*) 'new job record to be created, j_record_number=',j_record_number
        endif
        J%JobNumber=highest_job+1  ! assign unique job number as highest previous plus 1
        J%RecNum=J_record_number
      endif 
      ios=0 
      write(j_file_unit,rec=J_record_number,err=999,iostat=ios) J ! writes the job information record out
 999  continue
      if(ios.ne.0) then
         write(6,*) 'error writing job record, ios=',ios,' record number:', j_record_number
      endif
    end subroutine J_Init
!========================================================================
    subroutine J_Pattern(field,pattern)
    implicit none
! determine if the directory field is the datekey, parallel partition name, or scenario
    character*(*) field
    INTEGER, INTENT(OUT) :: pattern   ! 1: datecode pattern, 2: partition pattern, 3: scenario
    character*27 alpha/'abcdefghijklmnopqrstuvwxyz '/
    character*10 digit/'0123456789'/
    character a1,a8,a9
    logical alldigit
    integer lfld,i
    pattern=0
    lfld=len_trim(field)
    if(lfld.lt.1) return

    if(lfld.eq.1) then
      pattern=3
      return
    endif
! partition pattern is length 2, starting with p and ending in a digit
    a1=field(1:1)
    if(lfld.eq.2 .and. (a1.eq.'p' .or. a1.eq.'P') .and. index(digit,field(2:2)).gt.0) then
      pattern=2
      return
    endif
! any field that is not 8 or 9 in length is not a datecode so classify as a scenario name
    if(lfld.ne.8 .and. lfld .ne. 9) then
      pattern=3
      return
    endif
    a8=field(8:8)
    a9=' '
    if(lfld.eq.9) a9=field(9:9)

! check that fields of 8 or 9 have this pattern "d[0-9][0-9][0-9][0-9][0-9][0-9][a-z][a-z]" 
    if(a1.eq.'d'.and.index(alpha,a8).gt.0 .and.index(alpha,a9).gt.0) then
      alldigit=.true.
      do i=2,7
        if(index(digit,field(i:i)).eq.0) then
          alldigit=.false.
          exit
        endif
      enddo
      if(alldigit) then
        pattern=1
      else
        pattern=3
      endif
    else
      pattern=3
    endif
    
    end subroutine J_Pattern

!======================================================================== 
    subroutine J_Display
           write(m6,*)' '
           write(m6,*)'========================================'
           write(m6,*)' NEMS Job Control and Status Information'
           write(m6,*)'========================================'
           write(m6,*) 'Job Number   = ',J%JobNumber
           write(m6,*) 'Record Number= ',J%RecNum
           write(m6,*) 'Scenario     = ',J%Scenario
           write(m6,*) 'Datecode     = ',J%Datecode
           write(m6,*) 'OutputDir    = ',trim(J%OutputDir)
           write(m6,*) 'UserID       = ',J%USERID
           write(m6,*) 'User''s PCID  = ',J%PCID
           write(m6,*) 'Host for Job = ',J%Hostname
           write(m6,*) 'Host Queue ID= ',J%QueueID
           write(m6,*) 'Job Class    = ',J%Class
           write(m6,*) 'Queue Type   = ',J%QueueType
           write(m6,*) 'StartDate    = ',J%StartDate(5:6)//'-'//J%StartDate(7:8)//'-'//J%StartDate(1:4)
           write(m6,*) 'StartTime    = ',J%StartTime(1:2)//':'//J%StartTime(3:4)//':'//J%StartTime(5:6)
           write(m6,'(1x,a,i9)') 'Stat_Interval=',J%Stat_Interval
           write(m6,*) 'Priority     = ',J%Priority
           write(m6,*) 'Command      = ',J%Command
           write(m6,*) 'Status       = ',J%Status 
           write(m6,'(1x,a,i4)') 'Curiyr       = ',J%curiyr
           write(m6,'(1x,a,i4)') 'Curitr       = ',J%curitr
           write(m6,'(1x,a,i4)') 'Imodel       = ',J%imodel
           write(m6,*) 'runs in cycle= ',J%nruns
           write(m6,*) 'run #        = ',J%irun
           write(m6,'(1x,a,f8.0)') 'Cpu_Seconds  = ',J%CPU_Seconds
           write(m6,'(1x,a,f8.0)') 'Wall_Seconds = ',J%Wall_Seconds
           write(m6,*) 'StatusTime   = ',J%StatusTime(1:2)//':'//J%StatusTime(3:4)//':'//J%StatusTime(5:6)
           write(m6,*)'========================================'
           write(m6,*)' '
    end subroutine J_Display
!======================================================================== 
    subroutine J_Close
      call flush(j_file_unit)
      close(J_File_Unit,err=99)

 99   continue
    end subroutine J_Close
!========================================================================================
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
!=======================================================================

end program jobinit
!====================================================================
!    end of subroutine job which CONTAINs someother subroutines
!====================================================================

      SUBROUTINE MPTIM2(ICSEC)
      IMPLICIT NONE
      integer*4 icsec ! cpu seconds (100ths of secs) since start of program
      common/mptims/start_sec,ionce,curcpu,wstart_sec,wonce,cursec
      integer*4 ionce,wonce,wstart_sec,iwsec
      real*4 rsecs
      integer*4 start_sec
      integer*4 curcpu,cursec
      integer*4 time ! portability function time

      call cpu_time(rsecs)   ! processor dependent: seconds on cpu clock 

      icsec=rsecs*100.       ! convert to hundreths of seconds
      if(ionce.ne.1) then
         ionce=1
         start_sec=icsec     ! store first result as start of program 
      endif
      icsec=icsec-start_sec  ! return time since the start of program
      curcpu=icsec           ! store in common mptims

      iwsec=time()         ! number of wall clock seconds since 1970  
      if(wonce.ne.1) then
         wonce=1
         wstart_sec=iwsec
      endif
      iwsec=iwsec-wstart_sec   ! wall clock seconds since start of program
      cursec=iwsec*100     ! store in common, in hundredths


      RETURN
      END
      SUBROUTINE MPTIM3(ICSEC,ICWALL)
      IMPLICIT NONE

      integer*4 icsec ! cpu seconds (100ths of secs) since start of program
      integer*4 icwall! wall seconds (100ths of secs) since start of program
      common/mptims/start_sec,ionce,curcpu,wstart_sec,wonce,cursec
      integer*4 ionce,wonce,wstart_sec,iwsec
      real*4 rsecs
      integer*4 start_sec
      integer*4 curcpu,cursec
      call mptim2(icsec)
      icwall=cursec
      RETURN
      END

! ===============================================
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

