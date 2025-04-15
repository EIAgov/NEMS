!module util_tools
!contains


!******************************************************************
!*    Subroutine MPTIM3(ICSEC,ICWALL)
!*
!*    Same as MPTIM2, above, and also returns the wall clock time.
!******************************************************************
      SUBROUTINE MPTIM3(ICSEC,ICWALL)
      IMPLICIT NONE

      integer*4 icsec         ! cpu seconds (hundredths) since start of program
      integer*4 icwall        ! wall seconds (hundredths) since start of program
      common/mptims/istart_sec,curcpu,wstart_sec,cursec,crate4,cmax
      integer*4     istart_sec,curcpu,wstart_sec,cursec,crate4,cmax
      real*4 rsecs

      call mptim2(icsec)
      icwall=cursec

      RETURN
      END
	  
	  
!=====================================================================================================
! OSCall--Used to invoke a child process using the Win32 "CreateProcess" routine and optionally
! wait for it to finish using Win32 "WaitForSingleProcess"
! NEMS use:  to possible invoke Eviews in minimized state (from mac.f)
!
!=====================================================================================================
! Copyright (C) 2001 by Fortran Library
!
! This source may be freely copied, modified, or distributed so long as the original
! copyright statement remains intact.
!
! Suggestions for improvment to the original posted version are welcome.  Comments
! should be sent to mailto:webmaster@fortranlib.com
!
! Version:  2.0, 4 August 2001, 21:20:00
!
! Purpose:  OS Command line interface utility with immediate return or wait specified
!           in milliseconds (routine automatically quotes the command string)
!
! System Requirements:  Written for Digital/Compaq/Intel) Visual Fortran (x86)
!
! Routine Name:  OSCall
!
! iWaitMS:  default 32-bit (unsigned) integer wait value in milliseconds
!           0 = do not wait for completion
!          >0 = Number of milliseconds to wait for completion of initiated process
!          -1 = Infinite wait (wait for process completion)
!
! Command:  character command string
!
! Args:     optional character argument list string
!
! iRet:     default integer return code
!          -1 = Unable to initiate process
!           0 = Successful process initiate (if iWait = 0)
!           1 = Successful process initiate (Process terminated normally prior to wait period)
!           2 = Successful process initiate (Wait timeout occurred prior to process termination)
!           3 = Successful process initiate (Wait abandoned (unreleased mutex object))
!           4 = Successful process initiate (Wait failed)
!
! EIA Modification: have window open in minimized state.   DS 9/9/2004
!
 subroutine OSCall(iWaitMS,Command,Args,iRet)

   use ifport
   use ifwin
   use ifwinty
   implicit none
   interface
!     function GetLastE () bind (C, name='GetLastError@0')
      function GetLastE ()
      !dec$attributes stdcall, decorate, alias:'GetLastError' :: GetLastE
      import
         integer (DWORD):: GetLastE
      end function
   end interface


   character(*), intent(in)     :: Command        !Command portion of the command line (i.e. the program name)
   character(*), intent(in)     :: Args           !Argument portion of the command line

   character(256)               :: CmdLine        !Work area for the command line

   integer, intent(in)          :: iWaitMS        !Process completion wait value in milliseconds
   integer, intent(out)         :: iRet           !Main return code

   integer                      :: iWRC           !Return code for WaitForSingleObject
   integer                      :: iCRC           !Return code for CreateProcess
   integer                      :: iTRC           !Return code for TerminateProcess

   type (T_StartupInfo)         :: StartInfo      !CreatProcess parms
   type (T_Process_Information) :: ProcInfo       !CreatProcess parms (created process info)
   integer f/0/

   integer*4 err
   integer*4 len_cmd
!
! Initialize return code
!
   iRet = 0
!
! Insure console window is minimized
!
   StartInfo%cb               = 68
   StartInfo%lpReserved       = 0
   StartInfo%lpDesktop        = NULL
   StartInfo%lpTitle          = NULL
   StartInfo%dwX              = 0
   StartInfo%dwY              = 0
   StartInfo%dwXSize          = 0
   StartInfo%dwYSize          = 0
   StartInfo%dwXCountChars    = 0
   StartInfo%dwYCountChars    = 0
   StartInfo%dwFillAttribute  = 0
   StartInfo%dwFlags          = StartF_UseShowWindow
!  StartInfo%wShowWindow      = SW_FORCEMINIMIZE           ! DSA Changed from "SW_HIDE"
   StartInfo%wShowWindow      = SW_SHOWMINNOACTIVE           ! DSA Changed from "SW_HIDE"
   StartInfo%cbReserved2      = 0
   StartInfo%lpReserved2      = NULL
!
! Prepare the command line string and arguments
!
!   cmdLine = '"' // trim(command) // '" ' // args // char(0)  ! original
   cmdLine = trim(command) // ' '// trim(args) // char(0)   ! DSA change
   len_cmd = len_trim(cmdLine)
!
! Initiate process
!
   iCRC = CreateProcess(null, &
          cmdLine, &
          null, &
          null, &
          f, &                          ! previously, .false. worked
          Null, &
          Null, &
          null, & ! Null_Character, &
          StartInfo, &
          ProcInfo)
!
! Check return code from CreateProcess
!
   if (iCRC .eq. 0) then !Nonzero means success (i.e. the process id)
      err=getlaste()
      write(6,'(a,I4,":  ",a)')' error code from CreateProcess=',err,trim(cmdLine)
      iRet = -1
      return
   end if
!
! If user specified to wait
!
   if (iWaitMS .ne. 0) then

      iWRC = WaitForSingleObject(ProcInfo%hProcess,iWaitMS) !Wait for completion
      if (iWRC .eq. Wait_Failed) iRet = 4    !Wait failed
      if (iWRC .eq. Wait_Abandoned) iRet = 3 !Timeout abandoned
      if (iWRC .eq. Wait_Timeout) then       !Timeout occurred
          iRet = 2
          write(6,'(a,a,a)') "Maximum wait time has elapsed since ",cmdLine(1:len_cmd-1)," was called.  Giving up and continuing run."
!  the previous was changed to not print the char(0) to nohup.out
!  The following is for use if we want to terminate the EViews process:
         write(6,*) "Attempting to terminate process number",ProcInfo%hProcess
         iTRC = TerminateProcess(ProcInfo%hProcess,0)
      end if
      if (iWRC .eq. Wait_Object_0) iRet = 1  !Normal termination (signaled)

   end if

   return

end subroutine

!******************************************************************
      SUBROUTINE MNFLUSH
      use dfport
      implicit none
! flushes all files so in the event of an error, the most updated stuff will appear
      integer low/1/,high/200/
      integer i
      logical opn
      call FLUSH(6)
      do i=low,high
         if(i.ne.6.and.i.ne.5) then
           inquire(unit=i,opened=opn)
           if(opn) call FLUSH(i)
        endif
      enddo
      return
      end
!end module

!******************************************************************
! add the following not in module scope
! Claire duplicates unitunopened(), OSCall_PID() and OSCall() on purpose.
! We shall take out the filemgr.f90 unit_open module and move to here.
!******************************************************************
! ------ beg of subroutine OSCALL_PID()---------------
 subroutine OSCall_PID(iWaitMS,Command,Args,iRet,iPID)
! this version returns the process ID (iPID)
   use ifport
   use ifwin
   use ifwinty
   implicit none
   interface
!     function GetLastE () bind (C, name='GetLastError@0')
      function GetLastE ()
      !dec$attributes stdcall, decorate, alias:'GetLastError' :: GetLastE
      import
         integer (DWORD):: GetLastE
      end function
   end interface


   character(*), intent(in)     :: Command        !Command portion of the command line (i.e. the program name)
   character(*), intent(in)     :: Args           !Argument portion of the command line

   character(256)               :: CmdLine        !Work area for the command line

   integer, intent(in)          :: iWaitMS        !Process completion wait value in milliseconds
   integer, intent(out)         :: iRet           !Main return code
   integer, intent(out)         :: iPID           ! process id

   integer                      :: iWRC           !Return code for WaitForSingleObject
   integer                      :: iCRC           !Return code for CreateProcess
   integer                      :: iTRC           !Return code for TerminateProcess

   type (T_StartupInfo)         :: StartInfo      !CreatProcess parms
   type (T_Process_Information) :: ProcInfo       !CreatProcess parms (created process info)
   integer f/0/

   integer*4 err
   integer*4 len_cmd
!
! Initialize return code
!
   iRet = 0
!
! Insure console window is minimized
!
   StartInfo%cb               = 68
   StartInfo%lpReserved       = 0
   StartInfo%lpDesktop        = NULL
   StartInfo%lpTitle          = NULL
   StartInfo%dwX              = 0
   StartInfo%dwY              = 0
   StartInfo%dwXSize          = 0
   StartInfo%dwYSize          = 0
   StartInfo%dwXCountChars    = 0
   StartInfo%dwYCountChars    = 0
   StartInfo%dwFillAttribute  = 0
   StartInfo%dwFlags          = StartF_UseShowWindow
!  StartInfo%wShowWindow      = SW_FORCEMINIMIZE           ! DSA Changed from "SW_HIDE"
   StartInfo%wShowWindow      = SW_SHOWMINNOACTIVE           ! DSA Changed from "SW_HIDE"
   StartInfo%cbReserved2      = 0
   StartInfo%lpReserved2      = NULL
!
! Prepare the command line string and arguments
!
!   cmdLine = '"' // trim(command) // '" ' // args // char(0)  ! original
   cmdLine = trim(command) // ' '// trim(args) // char(0)   ! DSA change
   len_cmd = len_trim(cmdLine)

   iPID=0
!
! Initiate process
!
   iCRC = CreateProcess(null, &
          cmdLine, &
          null, &
          null, &
          f, &                          ! previously, .false. worked
          Null, &
          Null, &
          null, & ! Null_Character, &
          StartInfo, &
          ProcInfo)
!
! Check return code from CreateProcess
!
   if (iCRC .eq. 0) then !Nonzero means success (i.e. the process id)
      err=getlaste()
      write(6,'(a,I4,":  ",a)')' error code from CreateProcess=',err,trim(cmdLine)
      iRet = -1
      return
   end if
!
! If user specified to wait
!
   if (iWaitMS .ne. 0) then

      iWRC = WaitForSingleObject(ProcInfo%hProcess,iWaitMS) !Wait for completion
      if (iWRC .eq. Wait_Failed) iRet = 4    !Wait failed
      if (iWRC .eq. Wait_Abandoned) iRet = 3 !Timeout abandoned
      if (iWRC .eq. Wait_Timeout) then       !Timeout occurred
          iRet = 2
          write(6,'(a,a,a)') "Maximum wait time has elapsed since ",cmdLine(1:len_cmd-1)," was called.  Giving up and continuing run."
!  the previous was changed to not print the char(0) to nohup.out
!  The following is for use if we want to terminate the EViews process:
         write(6,*) "Attempting to terminate process number",ProcInfo%hProcess
         iTRC = TerminateProcess(ProcInfo%hProcess,0)
      end if
      if (iWRC .eq. Wait_Object_0) iRet = 1  !Normal termination (signaled)

   end if

   iPID=procInfo%dwProcessID

   return

end subroutine
! ------ end of subroutine OSCALL_PID()---------------
! *****************************************************************
! *****************************************************************

!******************************************************************
!*  Subroutine NDATOT
!*
!*  uses FILER to write RESTART file. At
!*  this point DICTIONARY is in memory (loaded in Subroutine NDATIN).
!******************************************************************
      SUBROUTINE NDATOT(opt)
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'fdict'
      include 'emmparm'
      include 'cdsparms'
      include 'coalemm'
      character*3 opt
      integer*4 iret, I, J, K, ENDUNIT
      CHARACTER*10 TEMP_CL_UNITS(MX_UNITS)
      LOGICAL FIRSTSPACE
      LOGICAL DOONCE /.FALSE./
! +++ FILER Declarations:
      INTEGER*4 FUNITI,FUNITO,FRETCD,FRTYPE,FSOURC,FUNFMT
      CHARACTER*100 FNAMEI,FNAMEO
      CHARACTER*18 FNAMEI18,FNAMEO18
! +++ End FILER Declarations
      CHARACTER*11 FUNFMTC  ! FROM INQUIRE STATEMENT--USED TO SET FUNFMT

      LOGICAL NEW
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      FRETCD=0
      FSOURC=1          ! Get variable list from a file
      FRTYPE=1          ! Output request

      FNAMEI18='VARLIST'  ! input file with list of variable names to write
      NEW=.FALSE.
      FUNITI=FILE_MGR('O',FNAMEI18,NEW)


      if(opt.eq.'GDX') then
        FNAMEO='restart.gdx'
        funfmt=5 ! gdx format
        CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
        WRITE(6,*) 'restart.gdx created'
        iret=FILE_MGR('C',FNAMEI18,NEW)
      elseif(opt.eq.'ITR') then
        write(fnameo,'(a,i2.2,a,i2.2,a,i4,a)') 'restart_',curirun,'_',curitr,'_',curcalyr,'.unf'
        WRITE(6,*) 'creating '//trim(fnameo)
      !   get unused unit number and open
        call unitunopened(100,999,FUNITO)
        open(funito,name=fnameo,status='unknown',form='unformatted',access='sequential',CONVERT='BIG_ENDIAN')
        rewind funito

        FYearSubset=1
        FYearStart=curiyr
        FYearEnd=curiyr

        FUNFMT=1 ! unformatted
        FNAMEI=' '  ! varlist file.  already open
        FNAMEO= ' ' ! restart file.  already open

        CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
        iret=FILE_MGR('C',FNAMEI18,NEW)

        FYearSubset=0
        close(funito)
        WRITE(6,*) trim(fnameo)//' created'
      elseif(opt.eq.'   ') then
        FNAMEO18='RESTART'  ! DD name for output file
        NEW=.TRUE.
        FUNITO=FILE_MGR('O',FNAMEO18,NEW)
222     FUNFMT=0
! +++ DETERMINE WHETHER FILE WAS OPENED AS FORMATTED OR UNFORMATTED BY FILEMGR AND SET FUNFMT VARIABLE ACCORDINGLY
        INQUIRE(FUNITO,FORM=FUNFMTC)
        IF(FUNFMTC.EQ.'FORMATTED') FUNFMT=0
        IF(FUNFMTC.EQ.'UNFORMATTED') FUNFMT=1

        FNAMEI=' '
        FNAMEO= ' '

        CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
        iret=FILE_MGR('C',FNAMEI18,NEW)
        iret=FILE_MGR('C',FNAMEO18,NEW)
        WRITE(6,*) 'restart file created'
      endif

      FUNITI=FILE_MGR('C',FNAMEI18,NEW)

      IF (CURIYR .EQ. LASTYR .AND. .NOT. DOONCE) THEN
         DOONCE=.TRUE.
         FNAMEI18='COALUNITO'
         NEW=.TRUE.
         FUNITI=FILE_MGR('O',FNAMEI18,NEW)
      IF (NUM_CMM_UNITS .GT. 0) THEN
         TEMP_CL_UNITS='          '
! replace space in name with '_'; i.e., "3 3" becomes "3_3"
         DO I=1,NUM_CMM_UNITS
            ENDUNIT=LEN_TRIM(EMM_CL_UNITS(I))
            FIRSTSPACE=.FALSE.
            K=0
            DO J=1,ENDUNIT
               IF (EMM_CL_UNITS(I)(J:J) .EQ. ' ' .AND. .NOT. FIRSTSPACE)  THEN
                   K=K+1
                   TEMP_CL_UNITS(I)(K:K)='_'
                   FIRSTSPACE=.TRUE.
               ELSEIF (EMM_CL_UNITS(I)(J:J) .NE. ' ') THEN
                   K=K+1
                   TEMP_CL_UNITS(I)(K:K)=EMM_CL_UNITS(I)(J:J)
               ENDIF
            ENDDO
         ENDDO
!  write output coal unit file for cycling.  this is in AIMMS data statement format
         WRITE(FUNITI,'(A)') 'Plantid_unitid := Data {'
         WRITE(FUNITI,'(A)') TRIM(TEMP_CL_UNITS(1))
         DO I=2,NUM_CMM_UNITS
            WRITE(FUNITI,'(A,A)') ',',TRIM(TEMP_CL_UNITS(I))
         ENDDO
         WRITE(FUNITI,'(A/)') '} ;'
         WRITE(FUNITI,'(A)') 'COALEMM_EMM_CL_UNITS := Data {'
         WRITE(FUNITI,'(A,A,A)') '1:"',TRIM(TEMP_CL_UNITS(1)),'"'
         DO I=2,NUM_CMM_UNITS
            WRITE(FUNITI,'(A,I4,A,A,A)') ',',I,':"',TRIM(TEMP_CL_UNITS(I)),'"'
         ENDDO
         WRITE(FUNITI,'(A/)') '} ;'
      ELSE
         WRITE(6,'("  Warning:  No coal units to output to cycle file COALUNITO.txt")')
         WRITE(FUNITI,'(A)') 'Plantid_unitid := Data { };'
         WRITE(FUNITI,'(A)') 'COALEMM_EMM_CL_UNITS := Data { };'
      ENDIF
         FUNITI=FILE_MGR('C',FNAMEI18,NEW)
      ENDIF

      RETURN
      END
	  
	  
	  
	  
!******************************************************************
!main.py
! extract from main.f globalmain and CALLING_AIMMS modules for AIMMS_RESTORE() to use
!******************************************************************
!****************************************************************
!*    NEMS "Main" Integrating  Module                           *
!****************************************************************
module globmain
! Using instead of common to pass info within integrating routines

   integer iterdump  ! Option(1=on) to creates single-year restart file after every iteration.
   integer aimmsver  ! option (3 or 4) to select which AIMMS version for coal module
end module

module scedes_keys
   INTEGER*4 CLBASEYR
   INTEGER*1 LEGIRA
end module
!******************************************************************

module CALLING_AIMMS
! declares data shared among AIMMS invocation routines:  AIMMS_Coal, AIMMS_NG, Read_AIMMS_Link, write_to_AIMMS_txt_cmd, read_from_AIMMS_txt, and End_AIMMS_cmd
      include 'parametr'
      include 'ncntrl'
      include 'fdict'
      include 'cdsparms'
      include 'coalout'
      include 'ngrpt'
      include 'continew'

!  Need to retrieve run-time options to see if we want to keep AIMMS open for the duration
      external rtovalue
      integer rtovalue


! AIMMS-related variables
     integer Num_AIMMS_modules,Num_AIMMS_modules_d,coalmod,ngasmod,restore
     parameter(Num_AIMMS_modules=3)
     parameter(coalmod=1)   ! coalmod, ngasmod, restore numbered as in aimms_module
     parameter(ngasmod=2)
     parameter(restore=3)
     parameter(Num_AIMMS_modules_d=Num_AIMMS_modules+1)
     integer keepopen(Num_AIMMS_modules)                              ! keep AIMMS open for each module
     integer(KIND=4) projectHandle(0:Num_AIMMS_modules)
     integer aimms_module ! 1 for coal, 2 for natural gas, 3 for ReStore used as index of projectHandle

     character*5 modfolder(Num_AIMMS_modules)/'coal','ngas','rest'/

     logical aimms_opened(0:Num_AIMMS_modules)                        ! Status of aimms for a given module. coal remains open. others may close between invocations.
     integer monitordbg(Num_AIMMS_modules)                            ! unit number for monitor.debug.txt
     logical opened_yet(Num_AIMMS_modules)/.false.,.false.,.false./   ! has monitordbg been opened for each module
     logical closed(0:Num_AIMMS_modules)                              ! Status of aimms for a a given module. if closed, it stays closed due to error.


     integer(KIND=4) ret
     character*80 aimmsFolder(Num_AIMMS_modules)
     logical lexist, l_a_result, no_aimms/.false./
     real*4 timer,timer2,mtimer

    integer file_mgr
    external file_mgr
    logical new,old

    integer iret                                                       ! integer return code

 ! declaration for subroutine filer arguments
    integer funiti,funito,fretcd,funfmt,frtype,fsourc
    character*100 fnamei,fnameo
    character*100 filen,filen2

    logical lopened

  integer O/6/     ! unit number for status/error messages

  Character*80 restarto,restarti
  character*18 varlistfile  ! such as coalputvars.txt or coalgetvars.txt
  integer fmt/6/

end module CALLING_AIMMS
!******************************************************************
! end of module CALLING_AIMMS
!******************************************************************


!===================================================================================================================

Subroutine Write_to_AIMMS_txt_cmd
! invokes aimms through command line. assumed curcalyr and curitr send via text file (monitor.in.txt)
  use ifcore,only:commitqq
  use ifport,only:timef,sleepqq
  use CALLING_AIMMS
  implicit none


      character*60 putFileName ! name of text file output by this routine
      INTEGER*4 IR,ICL,folder_start,folder_end
      integer*4 numerr,L,iwait,eunit,counter,ntries,ntriesmax/10/
      character*180 line,cmd*200,args*200
      logical license_issue
      character curdate*8,curtime*10
      integer iPID, iPIDlist, iPID2 ! process ID from oscall_pid or tasklist command
      integer nmessage,nchar ! counts of lines and characters in message.log
      integer ikill  ! set to 1 if aimms.exe has to be killed

! time limit to wait for AIMMS to return:
      integer aimms_time_limit /1200/         ! in half seconds, 1200 is 10 minutes (600 seconds)

     ! doing this to pass curcalyr to filer so it can incorporate it in a file name
      INTEGER   PASSCALYR
      COMMON /PassToFiler/ PASSCALYR
      PASSCALYR=CURCALYR


      ntries=1
100   continue ! return to here from below if license error occurs to retry up to "ntriesmax-ntries" more times.


if (aimms_module .eq. restore) goto 26       ! skip passing data for restore as this is done in uecp.f
      folder_start=index(restarto,'\\')+2         ! character after the first "\\"
      folder_end  =index(restarto,'\\',.true.)-1  ! character before the last "\\"
      write(putFileName,'(a,i4,a,i2.2,a)') '.\'//restarto(folder_start:folder_end)//'\toAIMMS\GlobalDataToAIMMS_',curcalyr,'_',curitr,'.txt'
      call unitunopened(100,999,FUNITO)
      open(funito,file=putFileName,status='unknown',buffered='YES')
      rewind funito

      write(O,'(a,i4,2a)') 'AIMMS ',curcalyr,'    List of variables to be sent: ',trim(varlistfile)
      write(O,'(a,i4,2a)') 'AIMMS ',curcalyr,'    Text file with sent data: ',trim(putFileName)
      write(O,'(a,i4,2a)') 'AIMMS ',curcalyr,'    Name of AIMMS project:     ',trim(restarto)

! write out variables listed in output request file
      FRTYPE=1

! get unit number and open varlist file using filemgr
      NEW=.false.
      FUNITI = FILE_MGR('O',varlistfile,NEW)
      rewind funiti    ! in case it is already open, position at beginning
      FNAMEI=' '       ! blank when the file is opened outside of filer
      FSOURC=1         ! obtain list of variables from file
      FNAMEO=' '  ! blank when the file is open outside of filer
      FUNFMT=7    ! text data transfer via AIMMS composite tables
26 continue

      ! inititalize message file used to direct actions of the aimms project monitor to wait so it doesn't start early.
      call unitunopened(100,999,EUNIT)
      open(eunit,file=trim(modfolder(aimms_module))//'\monitor.in.txt',status='unknown')
      rewind eunit
      write(eunit,'(a)')      'sAction             := "Wait";'
if (aimms_module .ne. restore) then
      write(eunit,'(a,i4,a)') "ncntrl_curcalyr('1'):=",curcalyr,';'
      write(eunit,'(a,i2,a)') "ncntrl_curitr('1')  :=",curitr,  ';'
else
      write(eunit,'(a,i4,a)') "Year:=data {",curcalyr," };"
endif
      l_a_result=commitqq(eunit)  ! use ifcore: force data to be written to file immediately
      close(eunit)
      call date_and_time(curdate,curtime)
      write(monitordbg(aimms_module),'(a,t45,3a,i5,i3)') 'monitor.in.txt  : Wait', curtime(1:2),':',curtime(3:10),curcalyr,curitr

      if(.not. aimms_opened(aimms_module) ) then

        timer=timef()
        l=len_trim(aimmsFolder(aimms_module))-1
        cmd=aimmsFolder(aimms_module)(:l)//'\bin\aimms.exe '

        args='--hidden --license-wait-seconds 120 -RNEMS_monitor '//trim(restarto)
        write(6,'(2a)') 'AIMMS Command line is ',trim(cmd)//' '//trim(args)
        write(O,'(a,i4,a)') 'AIMMS ',curcalyr,'    Opening AIMMS project, procedure NEMS_monitor'
        call date_and_time(curdate,curtime)
        write(monitordbg(aimms_module),'(a,t45,3a,i5,i3,i2)') '                : Opening Aimms Project', curtime(1:2),':',curtime(3:10),curcalyr,curitr

        iwait=0
        call oscall_pid(iwait,cmd,args,iret,iPID)

        if(iret.eq.0) then
          aimms_opened(aimms_module)=.true.
        else
          write(O,'(a,i4,a,i4)') 'AIMMS error running '//trim(restarto)//', return code=',iret,'  in ',curcalyr
          aimms_opened(aimms_module)=.true.
          closed(aimms_module)=.true.
        endif

        write(O,'(a,i4,a,f8.1)') 'AIMMS ',curcalyr,'    Time to open project:',timef()-timer

      endif

if (aimms_module .eq. restore) goto 27       ! skip passing data for restore as this is done in uecp.f
! send variables to AIMMS project
      timer=timef()

      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      close(funito)
      write(6,'(a,i4,a,i4)') 'AIMMS ',curcalyr,'    filer put return code=',fretcd
      write(6,'(a,i4,a,i5)') 'AIMMS ',curcalyr,'    filer FYearSubset=',FYearSubset


      write(O,'(a,i4,a,f8.1)') 'AIMMS ',curcalyr,'    Time to output transfer data to text file=',timef()-timer
27 continue

! Initialize AIMMS output message file so this routine won't see old one and think AIMMS is finished
      call unitunopened(100,999,EUNIT)
      open(eunit,file=trim(modfolder(aimms_module))//'\monitor.out.txt',status='unknown',SHARED)
      rewind eunit
      write(eunit,'(a)') 'executing' !  so loop below won't think it is already finished in loop below.
      l_a_result=commitqq(eunit)
      close(eunit)

! Tell AIMMS to invoke MainExecution via its input message file
      call unitunopened(100,999,EUNIT)
      open(eunit,file=trim(modfolder(aimms_module))//'\monitor.in.txt',status='unknown',SHARED)
      rewind eunit

!  writing instructions to AIMMS module
!  moving sAction to last so the other variables will be in the file before it is told to run
if (aimms_module .ne. restore) then
      write(eunit,'(a,i4,a)') "ncntrl_curcalyr('1'):=",curcalyr,';'
      write(eunit,'(a,i2,a)') "ncntrl_curitr('1')  :=",curitr,  ';'
      write(eunit,'(a,i2,a)') "ncntrl_ncrl('1')  :=",ncrl,  ';'
else
      write(eunit,'(a,i4,a)') "Year:=data {",curcalyr," };"
endif
      write(eunit,'(a)')      'sAction             := "MainExecution";'

      l_a_result=commitqq(eunit)  ! use ifcore: force data to be written to file immediately
      close(eunit)
      call date_and_time(curdate,curtime)
      write(monitordbg(aimms_module),'(a,t45,3a)') 'monitor.in.txt  : MainExecution', curtime(1:2),':',curtime(3:10)
      write(monitordbg(aimms_module),'(t20,a)') 'Here is the monitor.in.txt file:'
if (aimms_module .ne. restore) then
      write(monitordbg(aimms_module),'(t25,a,i4,a)') "ncntrl_curcalyr('1'):=",curcalyr,';'
      write(monitordbg(aimms_module),'(t25,a,i2,a)') "ncntrl_curitr('1')  :=",curitr,  ';'
      write(monitordbg(aimms_module),'(t25,a,i2,a)') "ncntrl_ncrl('1')  :=",ncrl,  ';'
else
      write(monitordbg(aimms_module),'(t25,a,i4,a)') "Year:=data {",curcalyr,"};"
endif

      counter=0
      line=' '
! when AIMMS is done, the monitor procedure writes out a keyword "Completed" to monitor.out.txt. Wait for
      do while (counter.lt.aimms_time_limit .and. (index(line,'Completed')+index(line,'Quit')+index(line,'Exited')).eq.0)
     !  setting back to original sleep 500 and increment 1 as sleeping 1000 increment 2 did not help
        call sleepqq(500)
        counter=counter+1
        iret=1
        open(eunit,file=trim(modfolder(aimms_module))//'\monitor.out.txt',status='old',SHARED,action='READ',err=99)
        line=' '
        read(eunit,'(a)',err=99,end=99) line
!        write(6,'(a)') 'line='//trim(line)
        iret=0
        close(eunit)
99      continue

      enddo
      call date_and_time(curdate,curtime)

      write(O,'(a,i4,a,f8.1)') 'AIMMS ',curcalyr,'    Time to run AIMMS MainExecution:',timef()-timer

      if(counter.ge.aimms_time_limit) then ! Aimms may have stopped running a procedure or in start up.
        write(monitordbg(aimms_module),'(a,t75,3a,i5,i3,i2)') 'monitor.out.txt  : wait time exceeded', curtime(1:2),':',curtime(3:10),curcalyr,curitr
        write(6,'(a)') 'AIMMS wait time exceeded. Showing AIMMS messages.log and checking for license issue.'
        license_issue=.false.
        inquire(file=trim(modfolder(aimms_module))//'\log\messages.log',exist=lexist)
        nmessage=0
        nchar=0
        if(lexist) then
          open(eunit,file=trim(modfolder(aimms_module))//'\log\messages.log',status='old',SHARED,READONLY,err=1999)
1000      continue
            read(eunit,'(a)',end=1998,err=1999) line
            write(6,'(a,I4,2a)') 'AIMMS  ',curcalyr,'  AIMMS messages.log:  ',trim(line)
            nmessage=nmessage+1
            nchar=nchar+len_trim(line)  ! count characters. if only 3 in file, aimms didn't start gracefully. sign of intermittant error.
            if(index(line,'license') .gt. 0 .or. index(line,'SaveAll') .gt. 0 .or. &
               index(line,'invalid window handle') .gt. 0 .or. &
               index(line,'RHS of DemandMassBalance constraint is negative') .gt. 0) then
              license_issue=.true.
            endif
            goto 1000  ! above
1998      continue
          close(eunit)
1999      continue
          write(6,'(a,I4,a,i5)') 'AIMMS  ',curcalyr,'  AIMMS messages.log, number of lines read:',nmessage
        endif
! if an aimms.exe is still executing with the same ProcessID (iPID), kill it.
        args=' '
        iWait=-1
        ikill=0
        cmd='cmd /c tasklist -v -FI "IMAGENAME eq aimms.exe" > tasklist.txt'
        call oscall_PID(iWait,cmd,Args,iRet,iPID2)
        inquire(exist=lexist,file='tasklist.txt')
        if(lexist) then
          open(eunit, file='tasklist.txt',status='old',READONLY)
        19 continue
           read(eunit,'(a)',end=21) line
           if (index(line,'aimms.exe') .gt. 0) then
             line(:20)=''
             read(line,*,end=21,err=21) iPIDlist
             if (iPIDlist .eq. iPID) then
                ikill=1
                write(6,'(a,i6,a)') 'The aimms.exe is still executing.  Will kill the process ID ',iPID
                write(cmd,'(a,i6)')  'taskkill /F /PID ',iPID
                iWait=-1
                call oscall_PID(iWait,cmd,Args,iRet,iPID2)
                goto 21
             else
               goto 19
             endif
           else
             goto 19
           endif
        21 continue
           close(eunit)
        endif
        if (ikill .eq. 0) then
          write(6,'(a)') 'AIMMS seems to have closed early. No aimms.exe process found with matching process ID.'
        endif
        if (license_issue .and. ntries .lt. 3) then  ! for time out errors, only repeat twice more, or 3 total
          write(6,'(a,i2)') 'AIMMS license issue.  Will retry. Number of tries so far: ',ntries
          ntries=ntries+1
          aimms_opened(aimms_module)=.false.
          go to 100  ! above at subroutine start
        elseif (ntries .lt. 3 .and. ikill .eq. 1 .and. nchar .eq. 3) then  ! when AIMMS stays open and writes only 3 non-printable characters in message.log.
          write(6,'(a,i2)') 'AIMMS start up failed, funny message.log.  Will retry. Number of tries so far: ',ntries
          ntries=ntries+1
          aimms_opened(aimms_module)=.false.
          go to 100  ! above at subroutine start
        else
          iret=900
          aimms_opened(aimms_module)=.false.
          closed(aimms_module)=.true.
        endif
      else
        if(index(line,'Exited').gt.0) then  ! error or licensing issue
          write(6,'(a)') 'AIMMS Exited early. Likely a licensing issue or error in the AIMMS code or transfer data. Displaying log/messages.log:'
          license_issue=.false.
          inquire(file=trim(modfolder(aimms_module))//'\log\messages.log',exist=lexist)
          if(lexist) then
            open(eunit,file=trim(modfolder(aimms_module))//'\log\messages.log',status='old',SHARED,READONLY,err=2999)
2000        continue
              read(eunit,'(a)',end=2998,err=2999) line
              write(6,'(2a)')'AIMMS messages.log:  ',trim(line)
              if(index(line,'license').gt.0) then
                license_issue=.true.
              endif
              goto 2000  ! above
2998        continue
            close(eunit)
2999        continue
            if (aimms_module .eq. 2) then       ! flag for notification via intercv
               CONTING=0
               REASONG='AIMMS early exit'
            endif
          else
            write(6,'(a)') 'AIMMS message log, log/messages.log, does not exist.'
          endif
          if (license_issue .and. ntries .lt. ntriesmax) then
            if (aimms_module .eq. 2) then       ! reset flag for notification via intercv for next try
               CONTING=1
               REASONG=''
            endif
            write(6,'(a,i2)') 'AIMMS license issue.  Will retry. Number of tries so far: ',ntries
            ntries=ntries+1
            aimms_opened(aimms_module)=.false.
            go to 100   ! above at subroutine start
            iret=900
            aimms_opened(aimms_module)=.false.
            closed(aimms_module)=.true.
          endif
        elseif (index(line,'Completed') .gt. 0) then
          write(monitordbg(aimms_module),'(a,t75,3a,i5,i3,i2)') 'monitor.out.txt  : '//trim(line(5:)), curtime(1:2),':',curtime(3:10),curcalyr,curitr
          ! inititalize message file used to direct actions of the aimms project monitor to wait so it doesn't start early.
          call unitunopened(100,999,EUNIT)
          open(eunit,file=trim(modfolder(aimms_module))//'\monitor.in.txt',status='unknown')
          rewind eunit
          write(eunit,'(a)')      'sAction             := "Wait";'
          write(eunit,'(a,i4,a)') "ncntrl_curcalyr('1'):=",curcalyr,';'
          write(eunit,'(a,i2,a)') "ncntrl_curitr('1')  :=",curitr,  ';'
          l_a_result=commitqq(eunit)  ! use ifcore: force data to be written to file immediately
          close(eunit)
          call date_and_time(curdate,curtime)
          write(monitordbg(aimms_module),'(a,t45,3a,i5,i3)') 'monitor.in.txt  : Wait', curtime(1:2),':',curtime(3:10),curcalyr,curitr
        elseif(index(line,'Quit').gt.0) then
          call date_and_time(curdate,curtime)
          write(monitordbg(aimms_module),'(a,t45,3a,i5,i3)') 'monitor.out.txt  : Quit', curtime(1:2),':',curtime(3:10),curcalyr,curitr
        else
        endif


     endif
   return

End Subroutine Write_to_AIMMS_txt_cmd


!===================================================================================================================



!===================================================================================================================

Subroutine Read_from_AIMMS_txt
  use ifport,only:timef
  use CALLING_AIMMS
  implicit none
  integer folder_start,folder_end
  character*60 putFileName ! name of text file output by this routine

      folder_start=index(restarto,'\\')+2         ! character after the first "\\"
      folder_end  =index(restarto,'\\',.true.)-1  ! character before the last "\\"
      write(putFileName,'(a,i4,a,i2.2,a)') '.\'//restarto(folder_start:folder_end)//'\fromAIMMS\GlobalDataToNEMS_',curcalyr,'_',curitr,'.txt'
      write(6,'(a,i4,2a)') 'AIMMS ',curcalyr,'    Reading text file with data from AIMMS: ',trim(putFileName)
      inquire(file=putFileName,exist=lexist)
      if(.not. lexist) then
         write(6,'(3a)')  'AIMMS ERROR: text data from AIMMS not found: '//trim(putFileName)
         Call End_AIMMS_cmd
         closed(aimms_module)=.true.
         write(6,'(3a)')  'AIMMS ERROR: stopping now to avoid confusion.'
         if (aimms_module .eq. 1 .and. CONTINC .eq. 1) then
            CONTINC=0
            REASONC='AIMMS error'
         endif
         if (aimms_module .eq. 2 .and. CONTING .eq. 1) then
            CONTING=0
            REASONG='AIMMS error'
         endif
         CALL NDATOT('   ')
         CALL NDATOT('GDX')
         stop 192                  !  flag for Returncode
      endif
      call unitunopened(100,999,FUNITI)
      open(FUNITI,file=putFileName,status='old')

      FRTYPE=2           ! read from restart file/aimms project
      FSOURC=1           ! variables identified in the file
      FNAMEI=' '         ! blank when opened outside of filer
      FUNFMT=7           ! format indicator for AIMMS text format, composite tables

! Read global variables from AIMMS

      timer=timef()
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      close(FUNITI)

      write(6,'(a,i4,a,i4)')   'AIMMS ',curcalyr,'    filer put return code=',fretcd
      write(6,'(a,i4,a,i5)')   'AIMMS ',curcalyr,'    filer FYearSubset=',FYearSubset
      write(6,'(a,i4,a,f8.1)') 'AIMMS ',curcalyr,'    Time to retrieve variables=',timef()-timer


   return

end subroutine Read_from_AIMMS_txt

!===================================================================================================================

Subroutine End_AIMMS_cmd
  use ifport,only:sleepqq
  use CALLING_AIMMS
  implicit none
  integer eunit,ntries,nmax/240/
  character*160 line
  character curdate*8,curtime*10

! write Quit to message file to tell aimms project monitor to exit
      call unitunopened(100,999,EUNIT)
      open(eunit,file=trim(modfolder(aimms_module))//'\monitor.in.txt',status='unknown',SHARED)
      rewind eunit
      write(eunit,'(a)')      'sAction             := "Quit";'
      close(eunit)

      call date_and_time(curdate,curtime)
      write(monitordbg(aimms_module),'(a,t45,3a,i5,i3,i2)') 'monitor.in.txt  : Quit', curtime(1:2),':',curtime(3:10),curcalyr,curitr


!   need to wait until project is closed. May take 30 seconds to close for some reason. error trapping?
!   Added MainTermination statement to write "Exited" to monitor.out.txt, so use this as signal to stop
      ntries=1
      do while (ntries .lt. nmax)
        open(eunit,file=trim(modfolder(aimms_module))//'\monitor.out.txt',status='old',SHARED,READONLY)
        flush eunit
        read(eunit,'(a)',err=99,end=99) line
        close(eunit)
        if(index(line,'Exited').gt.0) then
          exit  ! exit do while
        endif
99      ntries=ntries+1
        call sleepqq(500) ! wait 1/2 second
      enddo
      write(6,'(a,i4,a,i4)') 'AIMMS Exiting, number of monitor.out.txt checks ',ntries,', max number set to ',nmax
      call date_and_time(curdate,curtime)
      if(ntries.eq.nmax) then
        write(monitordbg(aimms_module),'(a,t45,3a,i5,i3,i2)') 'monitor.out.txt : wait for Exited exceeded', curtime(1:2),':',curtime(3:10),curcalyr,curitr
      else
        write(monitordbg(aimms_module),'(a,t45,3a,i5,i3,i2)') 'monitor.out.txt : '//trim(line(5:)), curtime(1:2),':',curtime(3:10),curcalyr,curitr
      endif
      return

End Subroutine End_AIMMS_cmd

!===================================================================================================================
!******************************************************************
!*  Subroutine NXPECT
!*
!*  Updates the expectation variables for CURIYR and beyond; copies
!*  previous year value to start.
!*
!*  Inputs:
!*    MQTY   (PQ)     Vector of QUANTITY products, current iteration
!*    MPRC   (PQ)     Vector of PRICE products, current iteration
!*    MXQTY  (XPQ)    Vector of QUANTITY products for expectations
!*    MXPRC  (XPQ)    Vector of PRICE products for expectations
!*    I4SITE (NCNTRL) Foresight (1: Myopic, 2: Adaptive, 3: Perfect)
!*    CURIYR (NCNTRL) Index for current year
!******************************************************************
      SUBROUTINE NXPECT(IMODEL)
!	  use scedes_reader
      IMPLICIT NONE
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'qblk'
      include 'ampblk'
      include 'mxqblk'
      include 'mxpblk'
      include 'intout'
      include 'ngtdmrep'
      include 'pmmout'
      include 'pmmrpt'
      include 'control'
      include 'ecpcntl'
      include 'convfact'
      include 'tranrep'
      include 'pmmftab'
      INTEGER IMODEL     ! MODEL INDEX, 7=UTIL
      INTEGER IR         ! Subscript index for REGION
      INTEGER IV         ! Subscript index for VARIABLE
      INTEGER IY         ! Subscript index for Year
      INTEGER IDXGY_P    ! Subscript index for Growth Year, Price Variable
      INTEGER IDXGY_Q    ! Subscript index for Growth Year, Quantity Variable
      INTEGER IDXSY      ! Subscript index for Previous or Starting Year
      INTEGER IDXSY_SV   ! Subscript index for Previous or Starting Year Save Value
      INTEGER IDXEY      ! Subscript index for End Year
      INTEGER NYRS_P     ! Number of years for growth rate, prices
      INTEGER NYRS_Q     ! Number of years for growth rate, quantities
      INTEGER NXPVEC(MNUMPX) ! Expectation Price Array
      INTEGER NXQVEC(MNUMQX) ! Expectation Quantity Array
      DATA NXPVEC/ 2, 4,10,15,16,19,  & !XPELCM,ELIN,GFEL,GIIN,GIEL,NGCM
       22,29,39,42,56,59,64,81,       & !XPNGEL,CLEL,XPDSCM, XPDSEL, XPRLEL, XPRHEL, RS,UREL
       36,37,38,40,41,47,52,57,       & !XPMGAS, XPJFTR, XPDSRS, XPDSTR, XPDSIN, XPKSAS, XPLGAS, XPRLAS
       60,66,67,70,78,79/               !XPRHAS, XPPFIN, XPASIN, XPOTAS, XPMETR, XPETTR
      DATA NXQVEC/ 1, 2, 3, 4, 7, 8,  & !XQELRS,ELCM,ELTR,ELIN,ELAS,GFRS
        9,10,11,13,16,17,18,19,21,    & !XQGFCM,TR,IN,EL, GIRS,CM,TR,IN,EL
       46,47,48,49,50,51,53,58,       & !XQMGAS, XQJFTR, XQDSRS, XQDSCM, XQDSTR, XQDSIN, XQDSEL, XQKSAS
       63,64,68,69,70,72,73,80,       & !XQLGRF, XQLGAS, XQRLRF, XQRLEL, XQRLAS, XQRHEL, XQRHAS, XQPFIN
       81,82,84,86,87,90,91,99,       & !XQSGIN, XQSGRF, XQPCRF, XQPCAS, XQASIN, XQOTRF, XQOTAS, XQMETR
       100/                             !XQETTR
      INTEGER BMYREXP/0/ ! Subscript index for Benchmark Year
      INTEGER EUEDEXP    ! End Use Electricity Demand EXPectation switch
      REAL ELGREXP       ! Holds maximum growth rate for elec demand expect.
      REAL GDEM          ! Holds growth rate for QUANTITY variable
      REAL GPRC          ! Holds growth rate for PRICE variable
      EXTERNAL GROWTH    ! Growth Rate Function
      REAL GROWTH
      INTEGER RTOVALUE           ! FUNCTION TO READ RUN TIME OPTIONS
      EXTERNAL RTOVALUE
      INTEGER*4 AEOLSTYR,AYR   ! run-time option set to calendar year for last year of AEO projection period

! RETURN if beyond biggest NEMS year (how did THAT happen?)
      IF(CURIYR.GT.MNUMYR) RETURN

!  Determine if this is a "perfect" End Use Electricity Demand EXPectation (EUEDEXP) run
      EUEDEXP=RTOVALUE('EUEDEXP ',0)

      AEOLSTYR = RTOVALUE("AEOLSTYR",0)  ! Get calendar year for last year of AEO projection period.
      AYR = AEOLSTYR - 1989
!      Write(6,*)'aeolstyr ', curiyr,aeolstyr,AYR

! GET RUNTIME OPTION FOR BENCHMARK YEAR.  This option sets expecation
! variables to the values in the standard variables up to and including
! the benchmark year, such as 1995 (year index 6).  In addition, any
! extrapolation or myopia starts in the following year (eg 1996).

      IF(BMYREXP.EQ.0) BMYREXP=RTOVALUE('BMYREXP ',1)
      IF(BMYREXP.GT.1989) BMYREXP=BMYREXP-1989
! get runtime option for electricity demand growth maximum. divide by
! 10 to get percentage increase, by 100 to get fractional increase, then
!  add 1 to convert to growth multiplier.  That is
! the value 15 gets converted to 1.015.
      ELGREXP = 1. + (float(RTOVALUE('ELGREXP ',15))/10./100.)

!    Set up year indices for computing growth rates.  The way that year indices are
!    defined depends on expectations method.  This is done so that relatively generic
!    code can be used to fill in the expectations variables.

      IF(BMYREXP.EQ.0) BMYREXP=1
      IDXSY = MAX(CURIYR-1,BMYREXP)   !INDEX FOR PREVIOUS OR STARTING YEAR

      IF(I4SITE.EQ.3) IDXSY=CURIYR    ! (SET TO CURRENT YEAR, PERFECT FORESIGHT)

      IDXEY = MNXYR                   !INDEX FOR END YEAR
      IF(I4SITE.EQ.3.AND.CURIYR.NE.LASTYR) THEN
        IDXEY=CURIYR                  ! (SET TO CURRENT YEAR, PERFECT FORESIGHT)
      ENDIF

      IDXGY_Q= MAX(IDXSY-INT(NYRS),1) !INDEX FOR START OF GROWTH PERIOD, QUANT
      IDXGY_P= MAX(IDXSY-INT(NYRS),1) !INDEX FOR START OF GROWTH PERIOD, PRICE
      IF(I4SITE.EQ.1) IDXGY_P=IDXSY   ! (SET TO START YEAR, MYOPIC FORESIGHT)

      NYRS_P = IDXSY-IDXGY_P          !NUMBER OF GROWTH YEARS, PRICES
      NYRS_Q = IDXSY-IDXGY_Q          !NUMBER OF GROWTH YEARS, QUANTITIES

      DO IY=3,MNXYR
        DO IR=1,MNUMCR
        IF (IY .LE. CURIYR) THEN
          XPCLGAS(IR,IY) = PCLGAS(IR,IY)
        ELSE IF (CURCALYR .GT. 2009 .AND. XPCLGAS(IR,IY-2) .NE. 0) THEN
          XPCLGAS(IR,IY) = XPCLGAS(IR,IY-1) * XPCLGAS(IR,IY-1) / XPCLGAS(IR,IY-2)
        ELSE
          XPCLGAS(IR,IY) = PCLGAS(IR,MIN(IY,IJUMPYR))
        ENDIF
        ENDDO
      ENDDO

       DO IR=1,MNUMCR
        DO IY=1,AYR
           XPALMG(IR,IY) = PALMG(IR,IY)
         ENDDO
           GDEM = 1.0 + GROWTH(PALMG(IR,AYR-21),PALMG(IR,AYR-1),20)
           IF(GPRC.GT.1.05) GPRC=1.05
           IF(GPRC.LT.1.00) GPRC=1.00
         DO IY=AYR+1,MNXYR
           XPALMG(IR,IY) = XPALMG(IR,IY-1)*GDEM
         ENDDO
        ENDDO

!    For each variable:
!      1) COPY START YEAR VALUE
!      2) COMPUTE GROWTH RATE OVER PAST PERIOD
!      3) APPLY GROWTH PERIOD THROUGH TO END YEAR

      DO IR=1,MNUMCR-2  ! NINE CENSUS REGIONS
! MAIN PRICES
         DO IV = 1,MNUMPX
           DO IY=1,IDXSY
             MXPRC(IR,IY,IV) = MPRC(IR,IY,NXPVEC(IV))
           ENDDO
           GPRC = 1. +   GROWTH(MPRC(IR,IDXGY_P,NXPVEC(IV)), &
                                MPRC(IR,IDXSY, NXPVEC(IV)), NYRS_P)
           IF(GPRC.GT.1.05) GPRC=1.05
           IF(GPRC.LT.1.00) GPRC=1.00
           DO IY=IDXSY+1,IDXEY
              MXPRC(IR,IY,IV) = MXPRC(IR,IY-1,IV)*GPRC
           ENDDO
         ENDDO
! MAIN QUANTITIES
         DO IV = 1,MNUMQX

!  For Electricity Demand Use Perfect Foresight Through STEO Years Plus One, (LAST_STEO_YR) Then Grow Demands Forward

           IF (IV .LE. 5) THEN
              IDXSY_SV = IDXSY
              IDXSY = MAX(IDXSY , LAST_STEO_YR)
              IDXGY_Q= MAX(IDXSY-INT(NYRS),1) !INDEX FOR START OF GROWTH PERIOD, QUANT
              NYRS_Q = IDXSY-IDXGY_Q          !NUMBER OF GROWTH YEARS, QUANTITIES
           END IF
           DO IY=1,IDXSY
             MXQTY(IR,IY,IV) = MQTY(IR,IY,NXQVEC(IV))
           ENDDO

           GDEM = 1. +   GROWTH(MQTY(IR,IDXGY_Q,NXQVEC(IV)), &
                                MQTY(IR,IDXSY, NXQVEC(IV)), NYRS_Q)
           IF (GDEM .GT. 1.05) GDEM=1.05
           IF (IV .LE. 5) THEN
               GDEM = MIN(GDEM,ELGREXP)

!   THROUGH HISTORICAL YEARS, CAP ELEC GROWTH RATE AT 1.7 PERCENT

               IF(CURIYR + UHBSYR .LE. 2000)GDEM = MIN(GDEM,1.017)
           END IF

!  NOTE IV=1,5 ARE 5 ELECTRICITY DEMAND VARIABLES.

           IF (IV .LE. 5 .AND. EXE .EQ. 1 .AND. GRW_XYR .GT. 0) THEN
              IF (IMODEL .EQ. 7 .AND. (CURIYR + UHBSYR) .GE. UPSTYR) THEN
                 IF (GRW_XYR .LE. UHBSYR) GRW_XYR = 2002
                 IF (CURIYR + UHBSYR .GE. GRW_XYR ) THEN
                    GDEM = 1.0 + &
                       GROWTH(MQTY(IR,CURIYR-2,NXQVEC(IV)), &
                       MQTY(IR,CURIYR-1,NXQVEC(IV)),1)
                    GDEM = DEM_WGHT(IV,IR) * GDEM + &
                       (1.0 - DEM_WGHT(IV,IR)) * DEM_GRW(IV,IR)
                    DEM_GRW(IV,IR) = GDEM
                 ELSE
                    GDEM = DEM_GRW(IV,IR)
                 END IF
              ELSE
                 GDEM = DEM_GRW(IV,IR)
              END IF
           END IF
           DO IY=IDXSY+1,IDXEY
!   USE RESTART VALUES FOR "PERFECT" FORESIGHT
             IF(EUEDEXP.EQ.1)THEN
             IF(IY.LE.AYR)THEN
               IF(IV.EQ.1)GDEM = QELRS(IR,IY) / QELRS(IR,IY-1)
               IF(IV.EQ.2)GDEM = QELCM(IR,IY) / QELCM(IR,IY-1)
               IF(IV.EQ.3)GDEM = QELTR(IR,IY) / QELTR(IR,IY-1)
               IF(IV.EQ.4)GDEM = QELIN(IR,IY) / QELIN(IR,IY-1)
               IF(IV.EQ.5)GDEM = QELAS(IR,IY) / QELAS(IR,IY-1)
             ELSE
               IF(IV.EQ.1)GDEM = QELRS(IR,AYR) / QELRS(IR,AYR-1)
               IF(IV.EQ.2)GDEM = QELCM(IR,AYR) / QELCM(IR,AYR-1)
               IF(IV.EQ.3)GDEM = QELTR(IR,AYR) / QELTR(IR,AYR-1)
               IF(IV.EQ.4)GDEM = QELIN(IR,AYR) / QELIN(IR,AYR-1)
               IF(IV.EQ.5)GDEM = QELAS(IR,AYR) / QELAS(IR,AYR-1)
             END IF
             END IF
             MXQTY(IR,IY,IV) = MXQTY(IR,IY-1,IV)*GDEM
           ENDDO

           IF (IV .LE. 5) THEN
              IDXSY = IDXSY_SV
              IDXGY_Q= MAX(IDXSY-INT(NYRS),1) !INDEX FOR START OF GROWTH PERIOD, QUANT
              NYRS_Q = IDXSY-IDXGY_Q          !NUMBER OF GROWTH YEARS, QUANTITIES
           END IF

!load expectations for new q arrays
           IF (IV.GE.16.AND.IV.LE.40) THEN
              DO IY=1,AYR
                MXQTY(IR,IY,IV) = MQTY(IR,IY,NXQVEC(IV))
              ENDDO
                GDEM = 1.0 + &
                  GROWTH(MQTY(IR,AYR-21,NXQVEC(IV)), &
                  MQTY(IR,AYR-1,NXQVEC(IV)),20)
                IF(GDEM.GT.1.05) GDEM=1.05
                IF(GDEM.LT.1.00) GDEM=1.00
              DO IY=AYR+1,MNXYR
                MXQTY(IR,IY,IV) = MXQTY(IR,IY-1,IV)*GDEM
!                If (CURIYR.EQ.11) Write(6,*)'gdem ',IR,IV,IY,GDEM,MXQTY(IR,IY,IV)
              ENDDO
           ENDIF

         ENDDO
! load expectations for new xqagtr
         DO IY=1,AYR
           XQAGTR(IR,IY) = QAGTR(IR,IY)
         ENDDO
           GDEM = 1.0 + GROWTH(QAGTR(IR,AYR-21),QAGTR(IR,AYR-1),20)
           IF(GDEM.GT.1.05) GDEM=1.05
           IF(GDEM.LT.1.00) GDEM=1.00
         DO IY=AYR+1,MNXYR
           XQAGTR(IR,IY) = XQAGTR(IR,IY-1)*GDEM
         ENDDO
! load expectations for new xqlutr
         DO IY=1,AYR
           XQLUTR(IR,IY) = QLUTR(IR,IY)
         ENDDO
           GDEM = 1.0 + GROWTH(QLUTR(IR,AYR-21),QLUTR(IR,AYR-1),20)
           IF(GDEM.GT.1.05) GDEM=1.05
           IF(GDEM.LT.1.00) GDEM=1.00
         DO IY=AYR+1,MNXYR
           XQLUTR(IR,IY) = XQLUTR(IR,IY-1)*GDEM
         ENDDO
! load expectations for new xqluin
         DO IY=1,AYR
           XQLUIN(IR,IY) = QLUIN(IR,IY)
         ENDDO
           GDEM = 1.0 + GROWTH(QLUIN(IR,AYR-21),QLUIN(IR,AYR-1),20)
           IF(GDEM.GT.1.05) GDEM=1.05
           IF(GDEM.LT.1.00) GDEM=1.00
         DO IY=AYR+1,MNXYR
           XQLUIN(IR,IY) = XQLUIN(IR,IY-1)*GDEM
         ENDDO
! load expectations for new xqffv
         DO IY=1,AYR
           XQFFV(IR,IY) = QFFV(IR,IY)
         ENDDO
           GDEM = 1.0 + GROWTH(QFFV(IR,AYR-21),QFFV(IR,AYR-1),20)
           IF(GDEM.GT.1.05) GDEM=1.05
           IF(GDEM.LT.1.00) GDEM=1.00
         DO IY=AYR+1,MNXYR
           XQFFV(IR,IY) = XQFFV(IR,IY-1)*GDEM
         ENDDO
! load expectations for new xtrqldv
         DO IV=1,8
         DO IY=1,AYR
           XTRQLDV(IV,IR,IY) = TRQLDV(IV,IR,IY)
         ENDDO
           GDEM = 1.0 + GROWTH(TRQLDV(IV,IR,AYR-21),TRQLDV(IV,IR,AYR-1),20)
           IF(GDEM.GT.1.05) GDEM=1.05
           IF(GDEM.LT.1.00) GDEM=1.00
         DO IY=AYR+1,MNXYR
           XTRQLDV(IV,IR,IY) = XTRQLDV(IV,IR,IY-1)*GDEM
         ENDDO
         ENDDO

! load expectations for new xQOTTR
         DO IY=1,AYR
           XQOTTR(IR,IY) = QOTTR(IR,IY)
         ENDDO
           GDEM = 1.0 + GROWTH(QOTTR(IR,AYR-21),QOTTR(IR,AYR-1),20)
                IF(GDEM.GT.1.05) GDEM=1.05
                IF(GDEM.LT.1.00) GDEM=1.00
         DO IY=AYR+1,MNXYR
           XQOTTR(IR,IY) = XQOTTR(IR,IY-1)*GDEM
         ENDDO
! load expectations for new xQOTIN
! load expectations for new xQOTRF
         DO IY=1,AYR
           XQOTIN(IR,IY) = QOTIN(IR,IY)
           XQOTRF(IR,IY) = QOTRF(IR,IY)
         ENDDO
           GDEM = 1.0 + GROWTH(QOTIN(IR,AYR-21),QOTIN(IR,AYR-1),20)
                IF(GDEM.GT.1.05) GDEM=1.05
                IF(GDEM.LT.1.00) GDEM=1.00
         DO IY=AYR+1,MNXYR
           XQOTIN(IR,IY) = XQOTIN(IR,IY-1)*GDEM
         ENDDO
           GDEM = 1.0 + GROWTH(QOTRF(IR,AYR-21),QOTRF(IR,AYR-1),20)
                IF(GDEM.GT.1.05) GDEM=1.05
                IF(GDEM.LT.1.00) GDEM=1.00
         DO IY=AYR+1,MNXYR
           XQOTRF(IR,IY) = XQOTRF(IR,IY-1)*GDEM
         ENDDO

! load expectations for new xQNGTR
         DO IY=1,AYR
           XQNGTR(IR,IY) = QNGTR(IR,IY)
         ENDDO
           GDEM = 1.0 + GROWTH(QNGTR(IR,AYR-21),QNGTR(IR,AYR-1),20)
                IF(GDEM.GT.1.05) GDEM=1.05
                IF(GDEM.LT.1.00) GDEM=1.00
         DO IY=AYR+1,MNXYR
           XQNGTR(IR,IY) = XQNGTR(IR,IY-1)*GDEM
         ENDDO

! load expectations for new xQLGTR
         DO IY=1,AYR
           XQLGTR(IR,IY) = QLGTR(IR,IY)
         ENDDO
           GDEM = 1.0 + GROWTH(QLGTR(IR,AYR-21),QLGTR(IR,AYR-1),20)
                IF(GDEM.GT.1.05) GDEM=1.05
                IF(GDEM.LT.1.00) GDEM=1.00
         DO IY=AYR+1,MNXYR
           XQLGTR(IR,IY) = XQLGTR(IR,IY-1)*GDEM
         ENDDO

      ENDDO

! WORLD OIL PRICES (SECOND DIMENSION IS ALTERNATE UNITS--1:$/BARREL,2:$/MMBTU)
      IF (CFCRDIMP(CURIYR) .NE. 0.0) IT_WOP(CURIYR,2)  = IT_WOP(CURIYR,1)/CFCRDIMP(CURIYR)
      DO IY=1,IDXSY
         XIT_WOP(IY,1) = IT_WOP(IY,1)
         XIT_WOP(IY,2) = IT_WOP(IY,2)
      ENDDO
      GPRC = 1. +  GROWTH(IT_WOP(IDXGY_P,1),IT_WOP(IDXSY,1),NYRS_P)
      IF(GPRC.GT.1.05) GPRC=1.05
      IF(GPRC.LT.1.00) GPRC=1.00
      DO IY=IDXSY+1,IDXEY
         XIT_WOP(IY,1) = XIT_WOP(IY-1,1)*GPRC
         XIT_WOP(IY,2) = XIT_WOP(IY,1)/CFCRDIMP(min(IY,MNUMYR))
      ENDDO
      IF (BRENT_PRICE(IDXGY_P) .NE. 0.0) THEN
        GPRC = 1. +  GROWTH(BRENT_PRICE(IDXGY_P),BRENT_PRICE(IDXSY),NYRS_P)
      ELSE
        GPRC = GPRC
      ENDIF
      DO IY=IDXSY+1,IDXEY
         XBRENT_PRICE(IY) = XBRENT_PRICE(IY-1)*GPRC
      ENDDO
      IF (START_PRICE(IDXGY_P) .NE. 0.0) THEN
        GPRC = 1. +  GROWTH(START_PRICE(IDXGY_P),START_PRICE(IDXSY),NYRS_P)
      ELSE
        GPRC = GPRC
      ENDIF
      DO IY=IDXSY+1,IDXEY
         XSTART_PRICE(IY) = XSTART_PRICE(IY-1)*GPRC
      ENDDO
      IF (WTI_PRICE(IDXGY_P) .NE. 0.0) THEN
        GPRC = 1. +  GROWTH(WTI_PRICE(IDXGY_P),WTI_PRICE(IDXSY),NYRS_P)
      ELSE
        GPRC = GPRC
      ENDIF
      DO IY=IDXSY+1,IDXEY
         XWTI_PRICE(IY) = XWTI_PRICE(IY-1)*GPRC
      ENDDO
! CALL UTILITY PRICE EXPECTATIONS HERE SO THAT EXPECTATION VARIABLES DON'T
! GET OVERWRITTEN.  AS A RESULT, EXPECTATION VARIABLES IN THE DATABASE
! SHOULD MATCH THOSE ACTUALLY USED ON THE LAST YEAR OF THE RUN.
      IF(I4SCNT.EQ.2) THEN
        CALL EXNGUTIL(IMODEL)
        CALL EXOLUTIL(IMODEL)
      ENDIF

      RETURN
      END
!******************************************************************
!*  Subroutine EXNGUTIL
!******************************************************************
      SUBROUTINE EXNGUTIL(IMODEL)
      IMPLICIT NONE
! ROUTINE TO SET UTILITY NATURAL GAS PRICE EXPECTATIONS BY CENSUS REGION.

! INPUTS--
! FROM NEMS:
!  D  = PREVIOUS YEAR GAS PRODUCTION IN Tcf              (FROM OGPRDNG/1000)
!  QS = CUMULATIVE PRODUCTION THRU PREVIOUS YEAR (Tcf)   (CUMULATIVE OGPRDNG)
!  PS = WELLHEAD GAS PRICE IN PREVIOUS FORECAST YEAR     (FROM OGWPRNG)

! EXOGENOUS:
!  QF = CUMULATIVE PRODUCTION AT WHICH AVERAGE WELL-HEAD PRICE IS PRESUMED "KNOWN"
!  PF = EXOGENOUS EXPECTED WELLHEAD GAS PRICE WHEN QF CUMULATIVE PRODUCTION HAS BEEN ACHIEVED

      include 'parametr' ! dimensions
      include 'ncntrl'   ! curiyr, etc
      include 'ngtdmrep' ! well head price, domestic dry gas production
      include 'qblk'     ! quantities
      include 'ampblk'   ! adjusted prices
      include 'mxpblk'   ! expected prices
      include 'mxqblk'   ! expected quantities
      include 'convfact' ! conversion factor, cfngu, for wellhead prices
      include 'macout'   ! for deflating run-time option prices
      include 'emmparm'  ! emm parameters
      include 'ecpcntl'  ! ecp variables
      include 'ogsmout'  ! for natural gas resource base

      INTEGER IMODEL
      INTEGER IY,ISTART,IR,INTERVAL
      LOGICAL ONCE/.FALSE./
      LOGICAL ONCEPF/.FALSE./
      REAL D,PS,QS,QF,QG,PF,UPF
      REAL GR,A,B,C,EXPEXP,EXPEXP2, EXPARG
      REAL EXPOUTA(MNUMYR-(1999-1989),MNXYR-(1999-1989))
      INTEGER RTOVALUE
      EXTERNAL RTOVALUE
      CHARACTER*18 XFILE
      LOGICAL NEW/.TRUE./,OLD/.FALSE./
      INTEGER UNITX
      REAL WHDRTE(MNXYR)
      REAL STEOXPR(10)
      REAL WHATWGTPF, WHATWGTLNG
      INTEGER ISITPF
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
! GET FUTURE TARGET VALUE FOR CUMULATIVE PRODUCTION (QF), WELLHEAD PRICE (PF)
! FROM GENERAL PURPOSE OPTIONS FILE ("MOREDATA"), WITH DEFAULT VALUES PASSED
! IN CASE THE OPTIONS ARE NOT FOUND
      IF (CURIYR .EQ. FIRSYR .AND. CURITR .EQ. 1) THEN
        IF (.NOT. ONCE) THEN
            ONCE = .TRUE.
! STEO WELLHEAD PRICES
              IY=RTOVALUE('EXPNGPRD',2000) - 1989      !  10/01/10  changing this option to a year
              QF = sum(OGEOYURR( 1:MNUMOR-1, 2,IY)) + sum(OGEOYINF( 1:MNUMOR-1, 2,IY)) + &
                   OGEOYRSV(MNUMOR,2,IY) + &                     ! MNUMOR filled for this one
                   sum(OGEOYUGR(:,:,IY)) + sum(OGEOYAD(:,IY))    ! these two are not MNUMOR
              write(6,'(a,F9.2,a,I4)') " Resource base = ",QF," for year ",IY+1989
              PF=REAL(RTOVALUE('EXPNGPRC',675))*.01/MC_JPGDP(1998-BASEYR+1)
              QG=REAL(RTOVALUE('EXPNGGRW',20)) * .001
              UPF=REAL(RTOVALUE('EXPNGIPT',325))*.01/MC_JPGDP(1998-BASEYR+1)
              EXPEXP=REAL(RTOVALUE('EXPNGEXP',150))/100.
              EXPEXP2=REAL(RTOVALUE('EXPNGEX2',150))/100.
              ISITPF=RTOVALUE('WELLPF  ',0)
              WHATWGTPF =REAL(RTOVALUE('PFGUESS ',50))/100.
              WHATWGTLNG=REAL(RTOVALUE('LNGGUESS',50))/100.
            IF (UPCRVSW .GT. 0) THEN
! USE 2005 - 2040 GROWTH RATE FROM RESTART FILE
!             UPNGGRWT = (OGPRDNG(MNUMOR,CURIYR - 1) / OGPRDNG(MNUMOR,CURIYR - 4)) ** (1.0 / 3.0)
              UPNGGRWT = (OGPRDNG(MNUMOR,LASTYR) / OGPRDNG(MNUMOR,2005-BASEYR+1)) **  &
                         (1.0 / FLOAT(LASTYR - (2005-BASEYR+1)))
              UPNGGRWT = MAX(UPNGGRWT,1.0)
!             UPNGGRWE = (QNGEL(MNUMCR,CURIYR - 1) / QNGEL(MNUMCR,CURIYR - 4)) ** (1.0 / 3.0)
              UPNGGRWE = (QNGEL(MNUMCR,LASTYR) / QNGEL(MNUMCR,2005-BASEYR+1)) **  &
                         (1.0 / FLOAT(LASTYR - (2005-BASEYR+1)))
              UPNGGRWE = MAX(UPNGGRWE,1.0)
            ELSE
              UPNGGRWT = 1.0 + QG
              UPNGGRWE = 1.0 + 2.0 * QG
            END IF
! IF PERFECT FORESIGHT, SMOOTH PREVIOUS WELLHEAD PRICE GUESS AND
! WELLHEAD PRICE FROM RESTART FILE TO OBTAIN NEW GUESS
          IF (ISITPF .EQ. 1) THEN
            DO IY = CURIYR , LASTYR
! IF NO PREVIOUS GUESS EXISTS, JUST STORE RESTART PRICE
                DO IR=1,MNUMOR
                   IF (XOGWPRNGPF(2,IR,IY) .LE. 0.0) XOGWPRNGPF(2,IR,IY) = OGWPRNG(IR,IY)
                ENDDO
! SMOOTH PRIOR GUESS AND RESTART WELLHEAD PRICES
                DO IR=1,MNUMOR
                   XOGWPRNGPF(1,IR,IY) = OGWPRNG(IR,IY) * (1.0 - WHATWGTPF) + XOGWPRNGPF(2,IR,IY) * WHATWGTPF
                   XOGWPRNGPF(2,IR,IY) = XOGWPRNGPF(1,IR,IY)
                ENDDO
            END DO
          END IF
        ENDIF
      ENDIF
      IF (CURIYR .LE. 2)RETURN

!   SUM CUMULATIVE PRODUCTION (QS)  FROM 1991 TO LAST YEAR
      QS=0.
      ISTART=1991-BASEYR+1
      IF(ISTART.LT.2) ISTART=2
      DO IY=ISTART,CURIYR-1
        QS=QS+OGPRDNG(MNUMOR,IY)*.001
      ENDDO

!       PROGRAM SOLVES FOR A,B,C IN THE EQUATION
!               P = A*Q**1.5 + C USING TWO PAIRS
!       COMPUTE THE PARAMETERS A, C BY SOLVING OLS

      EXPARG=EXPEXP
      INTERVAL=1
      D =OGPRDNG(MNUMOR,CURIYR-1)*.001 ! LAST YEAR'S DOMESTIC DRY GAS PRODUCTION
      PS=OGWPRNG(MNUMOR,CURIYR-1)      ! LAST YEAR'S WELLHEAD PRICE
	  
      XOGWPRNG(MNUMOR,CURIYR-1) = PS     ! LAST YEAR'S WELLHEAD PRICE
      IF (PS .GE. UPF) THEN
        EXPARG=EXPEXP2
        INTERVAL=2
        IF (IMODEL .EQ. 7) write(6,'(a,f7.3,a,f7.3,a,i4)') &
          ' The natural gas price of',ogwprng(mnumor,curiyr-1), &
          ' is above the the economic point of unconventionals (',upf,') in ',curcalyr
      ENDIF
      CALL EXNGINVERT(QS,QF,PS,PF,A,B,C,EXPARG)
!      IF((CURIYR.EQ. 6.OR.CURIYR.EQ.11.OR.
!     +   CURIYR.EQ.16.OR.CURIYR.EQ.21).AND.IMODEL.EQ.7) THEN
!      WRITE(6,*)'#&UTILITY GAS PRICE EXPECTATIONS--DEBUG PRINTS'
!      write(6,*)'#& Dry Gas Production, last year         (D) =',D
!      WRITE(6,*)'#& Cumulative production, 1991-last year (QS)=',QS
!      write(6,*)'#& Wellhead price, last year (87$/mcf)   (PS)=',PS
!      WRITE(6,*)'#& Cumulative production, future target  (QF)=',QF
!      write(6,*)'#& Wellhead price, futur target year     (PF)=',PF
!      write(6,*)'#& Coefficients for A*QS**1.5+C         (A,C)=',A,C
!      ENDIF

!   COMPUTE GAS PRICE EXPECTATIONS FOR THE NEXT 40 YEARS: ASSUMES EXPECTED
!   GROWTH IN DOMESTIC PRODUCTION, D.  IF USING "PERFECT FORESIGHT" SMOOTH
!   PREVIOUS GUESS WITH RESTART VALUE FROM PREVIOUS CYCLE

      DO IY = CURIYR,MNXYR
       IF ((ISITPF .EQ. 0) .OR. (.NOT. ONCEPF)) THEN
        IF (ISITPF .EQ. 0)THEN
           QS=QS+D * UPNGGRWT ** FLOAT(IY - (CURIYR - 1))
        ELSE
         IF (IY .LE. LASTYR)THEN
           QS=QS+OGPRDNG(MNUMOR,IY)*0.001
         ELSE
           QS=QS+(OGPRDNG(MNUMOR,LASTYR)*0.001) * UPNGGRWT ** FLOAT(IY - LASTYR)
         END IF
        END IF

           XOGWPRNG(MNUMOR,IY) = A*(QS**EXPARG) + B*QS + C
          IF ((XOGWPRNG(MNUMOR,IY) .GT. UPF) .AND. (INTERVAL .EQ. 1)) THEN
           EXPARG=EXPEXP2
           CALL EXNGINVERT(QS,QF,XOGWPRNG(MNUMOR,IY),PF,A,B,C,EXPARG)
           IF (IMODEL .EQ. 7) &
           write(6,'(a,f7.3,a,f7.3,a,i4)') ' The natural gas price of',xogwprng(mnumor,iy), &
            ' has exceeded the the economic point of unconventionals (',upf, &
            ') in expectation year ',iy+1989
           INTERVAL=2
          ENDIF
        
!   IF USING "PERFECT FORESIGHT", USE COMPUTED EXPECTATIONS TO COMPUTE ANNUAL CHANGE
!   FOR POST-2040 AND THEN OVERWRITE THRU 2040 WITH WELLHEAD PRICES FROM "SMOOTHING"
        IF (ISITPF .EQ. 1) THEN
!   STORE ADAPTIVE EXPECTATIONS TO USE AS POST-2040 GROWTH INDEX
          WHDRTE(IY) = XOGWPRNG(MNUMOR,IY)
         IF (IY .LE. LASTYR) THEN
          DO IR=1,MNUMOR
             XOGWPRNG(IR,IY) = XOGWPRNGPF(1,IR,IY)
          ENDDO
         ELSE
          DO IR=1,MNUMOR
             XOGWPRNG(IR,IY) = XOGWPRNGPF(1,IR,LASTYR) * WHDRTE(IY) / WHDRTE(LASTYR)
          ENDDO
         END IF
        END IF
          IF (IY .EQ. MNXYR)ONCEPF = .TRUE.
       END IF
       IF (IMODEL .EQ. 7 .AND. CURIYR .GE. 21) &
          EXPOUTA(CURIYR-(2009-1989),IY-(2009-1989)) = XOGWPRNG(MNUMOR,IY)
      ENDDO
      IF (IMODEL .EQ. 7 .AND. CURIYR .GE. 21 .AND. CURIYR .EQ. LASTYR) THEN
        XFILE="MNEXPECT"
        UNITX = FILE_MGR('O',XFILE,NEW)
        IF (ISITPF .EQ. 1) THEN
           WRITE(UNITX,'(<MNXYR>I7)') (IY,IY=2010,(MNXYR+BASEYR-1))
           WRITE(UNITX,'(<MNXYR>F7.3)') (EXPOUTA(1,IY)*SCALPR,IY=1,MNXYR-(2009-1989))
        ELSE
          WRITE(UNITX,'(1x,A4,<IJUMPYR>I6)') 'Year',(IY,IY=2010,LASTYR+BASEYR-1)
          DO IY = 1,MNXYR-(2009-1989)
            WRITE(UNITX,'(1x,I4,<IJUMPYR>F6.2)') IY+2009,(EXPOUTA(IR,IY)*SCALPR,IR=1,LASTYR-20)
          ENDDO
        ENDIF
        UNITX = FILE_MGR('C',XFILE,OLD)
      ENDIF

! FILL EXPECTED DELIVERED GAS PRICES FROM WELLHEAD PRICE ASSUMING
! CONSTANT PRICE DIFFERENTIAL BETWEEN WELLHEAD AND DELIVERED PRICES

!     DO IY=CURIYR,MNXYR
!       DO IR=1,MNUMCR-2
!         XPGIEL(IR,IY)=XOGWPRNG(MNUMOR,IY)/CFNGU(MIN(IY,LASTYR))+ &
!            (XPGIEL(IR,CURIYR-1)-XOGWPRNG(MNUMOR,CURIYR-1)/CFNGU(MIN(IY,LASTYR)))
!         XPGFEL(IR,IY)=XOGWPRNG(MNUMOR,IY)/CFNGU(MIN(IY,LASTYR))+ &
!            (XPGFEL(IR,CURIYR-1)-XOGWPRNG(MNUMOR,CURIYR-1)/CFNGU(MIN(IY,LASTYR)))
!         XPNGEL(IR,IY)=XOGWPRNG(MNUMOR,IY)/CFNGU(MIN(IY,LASTYR))+ &
!            (XPNGEL(IR,CURIYR-1)-XOGWPRNG(MNUMOR,CURIYR-1)/CFNGU(MIN(IY,LASTYR)))
!       ENDDO
!     ENDDO
! TEMPORARY DEBUG WRITES
!      IF((CURIYR.EQ. 6.OR.CURIYR.EQ.11.OR.
!     +   CURIYR.EQ.16.OR.CURIYR.EQ.21).AND.IMODEL.EQ.7) THEN
!         WRITE(6,*) '#&WELLHEAD (87$/MCF) AND CENSUS REG 1 (87$/MMBTU)'
!         WRITE(6,*) '#&YEAR  XOGWPRNG   XPGIEL     XPGFEL    XPNGEL'
!         WRITE(6,*) '#&---- --------- --------- --------- ---------'
!         DO IY=CURIYR,MNXYR
!           WRITE(6,'(1X,A,i4,4F10.3)') '#&',iy+baseyr-1,
!     +      XOGWPRNG(MNUMOR,IY),XPGIEL(1,IY),XPGFEL(1,IY),XPNGEL(1,IY)
!         ENDDO
!      ENDIF
      RETURN
      END
!******************************************************************
!*  Subroutine EXNGINVERT
!******************************************************************
      SUBROUTINE EXNGINVERT(QS,QF,PS,PF,A,B,C,EXPEXP)
      IMPLICIT NONE
      REAL QS,QF,PS,PF,A,B,C,EXPEXP
!   CALLED BY EXNGUTIL TO
!       CALCULATE THE COEFFICIENTS OF THE EQUATION:
!       P = A*Q**1.5 + C
!       USING THE GENERALIZED INVERSE WITH TWO POINTS
!         PS = A*QS**1.5 +  C
!         PF = A*QF**1.5 +  C

      A = (PF-PS)/(QF**EXPEXP-QS**EXPEXP)
      B = 0.
      C = PF - A*QF**EXPEXP

      RETURN
      END
!******************************************************************
!*  Subroutine EXOLUTIL
!******************************************************************
      SUBROUTINE EXOLUTIL(IMODEL)
!	  use fm
!	  use scedes_reader
      IMPLICIT NONE
! ROUTINE TO SET UTILITY OIL PRICE EXPECTATIONS BY CENSUS REGION.
! THIS ROUTINE TEMPORARILY SETS EXECTED WOP PRICES FROM WOP PRICES
! AS READ IN THE INTERNATIONAL MODEL ("THE GOING-IN WORLD OIL PRICES").
      include 'parametr' ! dimensions
      include 'ncntrl'   ! curiyr, etc
      include 'intout'   ! wop
      include 'macout'   ! gdp92c deflator
      include 'ampblk'    ! delivered prices
      include 'mxpblk'   ! expected prices
      include 'convfact'

      INTEGER IMODEL,UNITX
      INTEGER RTOVALUE,FILE_MGR
      EXTERNAL RTOVALUE,FILE_MGR
      CHARACTER*18 XFILE
      REAL WOPA_ONE,WOPA_TWO,WOPLIMIT ! ASSUMPTIONS FOR WOP PRICE INCREASE BEYOND 2015
      REAL EXPOUTA(MNUMYR-(1999-1989),MNXYR-(1999-1989),5)
      INTEGER IY,IV,IR
      LOGICAL ONCE/.FALSE./,NEW/.TRUE./,OLD/.FALSE./

      IF(CURIYR.LT.2)RETURN

! COPY PRIOR YEAR'S "ACTUAL" PRICES INTO PRIOR YEAR EXPECTATION VARIABLES.
!  ASSIGN $/MMBTU PRICE FROM $/BARREL PRICE USING 5.8 MMBTU/BARREL (M.E.R)
      XIT_WOP(CURIYR-1,1)=IT_WOP(CURIYR-1,1)
      XIT_WOP(CURIYR-1,2)=IT_WOP(CURIYR-1,1)/CFCRDIMP(CURIYR-1)
      XBRENT_PRICE(CURIYR-1) = BRENT_PRICE(CURIYR-1)
      XSTART_PRICE(CURIYR-1) = START_PRICE(CURIYR-1)
      XWTI_PRICE(CURIYR-1) = WTI_PRICE(CURIYR-1)
      DO IY=CURIYR,LASTYR
        XIT_WOP(IY,1) = IT_WOP(IY,1)
        XIT_WOP(IY,2) = IT_WOP(IY,1)/CFCRDIMP(IY)
        XBRENT_PRICE(IY) = BRENT_PRICE(IY)
        XSTART_PRICE(IY) = START_PRICE(IY)
        XWTI_PRICE(IY) = WTI_PRICE(IY)
      ENDDO

! GET EXPECTATION ASSUMPTIONS FOR POST-2015 WOP INCREASE PER BARREL.
! FIRST ASSUMPTION APPLIES FOR 2016-2030, SECOND FOR 2031 ON.
! GET VALUES FROM GENERAL PURPOSE OPTIONS FILE ("MOREDATA"),
! WITH DEFAULT VALUES PASSED IN CASE THE OPTIONS ARE NOT FOUND.
! ASSUMPTIONS ARE ASSUMED TO BE IN 92CENTS/BARREL SO ARE CONVERTED TO 87$.
      IF(.NOT.ONCE) THEN
       ONCE=.TRUE.
       WOPA_ONE= REAL(RTOVALUE('WOPA_ONE',40))*.01/MC_JPGDP(1992-BASEYR+1)
       WOPA_TWO= REAL(RTOVALUE('WOPA_TWO',10))*.01/MC_JPGDP(1992-BASEYR+1)
       WOPLIMIT= REAL(RTOVALUE('WOPLIMIT',2602))*.01/MC_JPGDP(1992-BASEYR+1)
      ENDIF
      DO IY=LASTYR+1,MNXYR
        IF (CFCRDIMP(MIN(IY,MNUMYR)) .EQ. 0.0) CFCRDIMP(MIN(IY,MNUMYR))=5.8
        XIT_WOP(IY,1)=XIT_WOP(IY-1,1)+WOPA_ONE
        XIT_WOP(IY,2)=XIT_WOP(IY,1)/CFCRDIMP(MIN(IY,MNUMYR))
        XBRENT_PRICE(IY) = XBRENT_PRICE(IY-1) + WOPA_ONE
        XSTART_PRICE(IY) = XSTART_PRICE(IY-1) + WOPA_ONE
        XWTI_PRICE(IY) = XWTI_PRICE(IY-1) + WOPA_ONE
      ENDDO

! FILL DELIVERED PRICE EXPECTATION VARIABLES FROM WOP ($/MMBTU).
! ASSUME DELIVERED PRICES REMAIN AT FIXED DIFFERENTIAL TO WOP PRICE.
      DO IY=CURIYR,MNXYR
        DO IR=1,MNUMCR
          IF (XBRENT_PRICE(MNXYR) .EQ. 0.0) THEN
            XPDSEL(IR,IY)=XIT_WOP(IY,2) + PDSEL(IR,CURIYR-1)-XIT_WOP(CURIYR-1,2)
            XPRLEL(IR,IY)=XIT_WOP(IY,2) + PRLEL(IR,CURIYR-1)-XIT_WOP(CURIYR-1,2)
            XPRHEL(IR,IY)=XIT_WOP(IY,2) + PRHEL(IR,CURIYR-1)-XIT_WOP(CURIYR-1,2)
            XPRSEL(IR,IY)=XIT_WOP(IY,2) + PRSEL(IR,CURIYR-1)-XIT_WOP(CURIYR-1,2)
          ELSE
            XPDSEL(IR,IY)=XBRENT_PRICE(IY)/5.8 + PDSEL(IR,CURIYR-1)-XBRENT_PRICE(CURIYR-1)/5.8
            XPRLEL(IR,IY)=XBRENT_PRICE(IY)/5.8 + PRLEL(IR,CURIYR-1)-XBRENT_PRICE(CURIYR-1)/5.8
            XPRHEL(IR,IY)=XBRENT_PRICE(IY)/5.8 + PRHEL(IR,CURIYR-1)-XBRENT_PRICE(CURIYR-1)/5.8
            XPRSEL(IR,IY)=XBRENT_PRICE(IY)/5.8 + PRSEL(IR,CURIYR-1)-XBRENT_PRICE(CURIYR-1)/5.8
          ENDIF
        ENDDO
      ENDDO

      IF (CURIYR .GT. 1999-1989) THEN
      DO IY=CURIYR,MNXYR
        IF (XBRENT_PRICE(MNXYR) .EQ. 0.0) THEN
          EXPOUTA(CURIYR-(1999-1989),IY-(1999-1989),1) = XIT_WOP(IY,1)
          EXPOUTA(CURIYR-(1999-1989),IY-(1999-1989),2) = XIT_WOP(IY,2)
        ELSE
          EXPOUTA(CURIYR-(1999-1989),IY-(1999-1989),1) = XBRENT_PRICE(IY)
          EXPOUTA(CURIYR-(1999-1989),IY-(1999-1989),2) = XWTI_PRICE(IY)
        ENDIF
        EXPOUTA(CURIYR-(1999-1989),IY-(1999-1989),3) = XPDSEL(MNUMCR,IY)
        EXPOUTA(CURIYR-(1999-1989),IY-(1999-1989),4) = XPRLEL(MNUMCR-2,IY)
        EXPOUTA(CURIYR-(1999-1989),IY-(1999-1989),5) = XPRHEL(MNUMCR-2,IY)
      ENDDO
      ENDIF
      IF (IMODEL .EQ. 7 .AND. CURIYR .GE. 11 .AND. CURIYR .EQ. LASTYR) THEN
        XFILE="MOEXPECT"
        UNITX = FILE_MGR('O',XFILE,NEW)
        WRITE(UNITX,'(" World oil price expectations in dollars per barrel")')
        WRITE(UNITX,'(1x,A4,<MNUMYR>I7)') 'Year',(IY,IY=2000,LASTYR+BASEYR-1)
        DO IY = 1,MNXYR-(1999-1989)
          WRITE(UNITX,'(1x,I4,<MNUMYR>F7.2)') IY+1999,(EXPOUTA(IR,IY,1)*SCALPR,IR=1,LASTYR-10)
        ENDDO
        WRITE(UNITX,'(///" World oil price expectations in dollars per million Btu")')
        WRITE(UNITX,'(1x,A4,<MNUMYR>I7)') 'Year',(IY,IY=2000,LASTYR+BASEYR-1)
        DO IY = 1,MNXYR-(1999-1989)
          WRITE(UNITX,'(1x,I4,<MNUMYR>F7.2)') IY+1999,(EXPOUTA(IR,IY,2)*SCALPR,IR=1,LASTYR-10)
        ENDDO
        WRITE(UNITX,'(///" Distillate price expectations in dollars per million Btu")')
        WRITE(UNITX,'(1x,A4,<MNUMYR>I7)') 'Year',(IY,IY=2000,LASTYR+BASEYR-1)
        DO IY = 1,MNXYR-(1999-1989)
          WRITE(UNITX,'(1x,I4,<MNUMYR>F7.2)') IY+1999,(EXPOUTA(IR,IY,3)*SCALPR,IR=1,LASTYR-10)
        ENDDO
        WRITE(UNITX,'(///" Low sulfur residual price expectations in dollars per million Btu")')
        WRITE(UNITX,'(1x,A4,<MNUMYR>I7)') 'Year',(IY,IY=2000,LASTYR+BASEYR-1)
        DO IY = 1,MNXYR-(1999-1989)
          WRITE(UNITX,'(1x,I4,<MNUMYR>F7.2)') IY+1999,(EXPOUTA(IR,IY,4)*SCALPR,IR=1,LASTYR-10)
        ENDDO
        WRITE(UNITX,'(///" High sulfur residual price expectations in dollars per million Btu")')
        WRITE(UNITX,'(1x,A4,<MNUMYR>I7)') 'Year',(IY,IY=2000,LASTYR+BASEYR-1)
        DO IY = 1,MNXYR-(1999-1989)
          WRITE(UNITX,'(1x,I4,<MNUMYR>F7.2)') IY+1999,(EXPOUTA(IR,IY,5)*SCALPR,IR=1,LASTYR-10)
        ENDDO
        UNITX = FILE_MGR('C',XFILE,OLD)
      ENDIF

      RETURN
      END
!*****************************************************************
!*    Function GROWTH(Q1,QN,N)
!*
!*    Returns average annual growth rate.  Returns 0 if growth rate
!*    undefined due to zero base value or change in sign.
!*
!*    Input:
!*      Q1:  Value in First Year
!*      QN:  Value in Last Year
!*      N:   Number of Years
!*****************************************************************
      FUNCTION GROWTH(Q1,QN,N)
      IMPLICIT NONE

      REAL GROWTH
      REAL Q1,QN,RATIO,RN
      INTEGER N

      GROWTH=0.
      IF (Q1.EQ.0.0.OR.N.LE.0) RETURN

      RN=N
      RATIO=QN/Q1

      IF (RATIO.GT.0.) GROWTH=(RATIO**(1./RN))-1.

      IF (GROWTH.GT. 0.10) THEN
        GROWTH =  0.10
      ELSEIF (GROWTH.LT.-0.10) THEN
        GROWTH = -0.10
      ENDIF

      RETURN
      END

!===================================================================================================================
!******************************************************************
! MODULE EPHRTS_SWTICHES
!******************************************************************
! EPHRTS - SCEDES SWITCH FOR THE EPHRTS MODEL, RUNS THE MODEL BY USING A SWTICH ON THE AIMMS WRAPPER
MODULE EPHRTS_SWTICHES
      !LOGICAL :: TURN_ON_EPHRTS
      LOGICAL :: TURN_ON_DEBUGGING = .FALSE.
      INTEGER :: EPHRTS_DEBUG_SWITCH
      LOGICAL :: EPHRTS_READ_PARAMETER_FILE = .FALSE.
      INTEGER :: EPHRTS
END MODULE
!******************************************************************

subroutine coal_expect
implicit none
! fill ecp's coal expectations horizon for current year using prior run values, or perfect foresight
   include 'parametr'
   include 'ncntrl'
   include 'cdsparms'
   include 'emmparm'
   include 'uso2grp'
   integer icl,j,iy,ilast,AEOLSTYR

   integer rtovalue ! run time option for last year of the current AEO, like 2040 for AEO2014
   external rtovalue

   if(ilast.eq.0) then
      AEOLSTYR=rtovalue('AEOLSTYR',2050)
      ilast=AEOLSTYR-BASEYR+1
   endif
   do icl=1,mx_ncoals
     do j=1,ndreg
       do iy=1,ECP_D_FPH  ! expectations horizon indexed 0:ECP_D_FPH with 0 representing current model year, 1 next, etc.
         if(curiyr+iy.le.ilast) then
           xcl_1tesc(icl,iy,curiyr,j)= xcl_1tesc(icl,0,curiyr+iy,j)
           xcl_2tesc(icl,iy,curiyr,j)= xcl_2tesc(icl,0,curiyr+iy,j)
         else
           xcl_1tesc(icl,iy,curiyr,j)= xcl_1tesc(icl,0,ilast,j)
           xcl_2tesc(icl,iy,curiyr,j)= xcl_2tesc(icl,0,ilast,j)
         endif
       enddo
     enddo
   enddo
   return
end subroutine coal_expect


SUBROUTINE HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)

      IMPLICIT NONE

      include'parametr'
      include'emmparm'
      include'ncntrl'
      include'control'
      include'ecpcntl'
      include'entcntl'
      include'bildin'
      include'wrenew'
      include'wwdcomon'
      include'uefdout'
      include'elcntl'
      include'elout'

      REAL*8 Load_Level, HTRT_ADJ
      REAL*8 EH, EL, LH, LL, Target_EFF, Max_EFF

      INTEGER*4 FUEL_RGN, ECPt
      INTEGER*4 KNOT, N_KNOT
      CHARACTER*12 FROM_LABEL

      N_KNOT = FLRG_HR_KNOTS(FUEL_RGN,ECPt)
      IF (FLRG_HR_KNOTS(FUEL_RGN,ECPt) .EQ. 0) THEN
         HTRT_ADJ = 1.0
         Target_EFF = 1.0
         Max_EFF = 1.0
      ELSE
         Load_Level = MIN(Load_Level , 1.0)
         Max_EFF = FLRG_HR_EFF(FUEL_RGN,ECPt,N_KNOT)
         IF (Load_Level .LE. FLRG_HR_LL(FUEL_RGN,ECPt,1)) THEN
            Target_EFF = FLRG_HR_EFF(FUEL_RGN,ECPt,1)
            HTRT_ADJ =  Max_EFF / Target_EFF
         ELSE
            DO KNOT = 2 , FLRG_HR_KNOTS(FUEL_RGN,ECPt)
               IF (Load_Level .LE. FLRG_HR_LL(FUEL_RGN,ECPt,KNOT)) THEN
                  LL = FLRG_HR_LL(FUEL_RGN,ECPt,KNOT-1)
                  EL = FLRG_HR_EFF(FUEL_RGN,ECPt,KNOT-1)
                  LH = FLRG_HR_LL(FUEL_RGN,ECPt,KNOT)
                  EH = FLRG_HR_EFF(FUEL_RGN,ECPt,KNOT)
                  Target_EFF = EL + ((EH - EL) / (LH - LL)) * (Load_Level - LL)
                  HTRT_ADJ =  Max_EFF / Target_EFF

!                 IF (CURIYR+1989 .LE. 2017 .AND. FCRL .EQ. 1) THEN
!                    WRITE(18,5751) CURIRUN, CURIYR+1989, CURITR, FUEL_RGN, ECPt, KNOT, FLRG_HR_KNOTS(FUEL_RGN,ECPt), FROM_LABEL, LL, Load_Level, LH, EL, Target_EFF, EH, Max_EFF, HTRT_ADJ
!5751                FORMAT(1X,"HTRT_ADJ_INFO",7(":",I4),":",A12,8(":",F21.6))
!                 END IF

                  EXIT
               END IF
            END DO
            IF (KNOT .GT. FLRG_HR_KNOTS(FUEL_RGN,ECPt)) THEN
               HTRT_ADJ = 1.0
               Target_EFF = Max_EFF
            END IF
         END IF
      END IF

      RETURN
      END
	  
SUBROUTINE ECP_AVG_HTRT(FROM_LABEL, EMM_RGN, FUEL_RGN, ECPt, XYR, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, ECP_GEN, ECP_GEN_MR, ECP_GEN_MOD, ECP_GEN_MR_MOD)

      IMPLICIT NONE

      include'parametr'
      include'emmparm'
      include'ncntrl'
      include'control'
      include'ecpcntl'
      include'entcntl'
      include'wrenew'
      include'wwdcomon'
      include'uefdout'
      include'elcntl'
      include'elout'
      include'bildin'

      REAL*8 AVG_HTRT(0:ECP_D_CAP),     AVG_HTRT_MR(0:ECP_D_CAP),     ECP_CAP(0:ECP_D_CAP),     ECP_CAP_MR(0:ECP_D_CAP),     ECP_GEN(0:ECP_D_CAP),     ECP_GEN_MR(0:ECP_D_CAP)
      REAL*8 AVG_HTRT_MOD(0:ECP_D_CAP), AVG_HTRT_MR_MOD(0:ECP_D_CAP), ECP_CAP_MOD(0:ECP_D_CAP), ECP_CAP_MR_MOD(0:ECP_D_CAP), ECP_GEN_MOD(0:ECP_D_CAP), ECP_GEN_MR_MOD(0:ECP_D_CAP)
    
      REAL*8 YRSPAN,YRDIFF

      INTEGER*4 EMM_RGN, FUEL_RGN, ECPt, I_ECP, ECP_GRP, XYR, ECP_TYPE, IGRP, CYR_INDEX, LYR_INDEX, S_FLRG, L_FLRG, I_FLRG ,YRADJ, GRP_ECP_TYPE, HTRT_QRT, INIT_GRP, TEST
      CHARACTER*12 FROM_LABEL

      AVG_HTRT = 0.0
      AVG_HTRT_MR = 0.0
      ECP_CAP = 0.0
      ECP_CAP_MR = 0.0
      ECP_GEN = 0.0
      ECP_GEN_MR = 0.0
      AVG_HTRT_MOD = 0.0
      AVG_HTRT_MR_MOD = 0.0
      ECP_CAP_MOD = 0.0
      ECP_CAP_MR_MOD = 0.0
      ECP_GEN_MOD = 0.0
      ECP_GEN_MR_MOD = 0.0

      ECP_GRP = UPTTYP(ECPt)

!     If heatrate overwrite switch is on use overwrite values
 
      IF (ECP_D_OVHR(ECPt) .GT. 0) THEN

         AVG_HTRT(ECPt) = UPOVHR(ECPt,MIN(MNUMYR,CURIYR+XYR))
         AVG_HTRT(0) = UPOVHR(ECPt,MIN(MNUMYR,CURIYR+XYR))
         AVG_HTRT_MR(ECPt) = UPOVHR(ECPt,MIN(MNUMYR,CURIYR+XYR))
         AVG_HTRT_MR(0) = UPOVHR(ECPt,MIN(MNUMYR,CURIYR+XYR))
         AVG_HTRT_MOD(ECPt) = UPOVHR(ECPt,MIN(MNUMYR,CURIYR+XYR))
         AVG_HTRT_MOD(0) = UPOVHR(ECPt,MIN(MNUMYR,CURIYR+XYR))
         AVG_HTRT_MR_MOD(ECPt) = UPOVHR(ECPt,MIN(MNUMYR,CURIYR+XYR))
         AVG_HTRT_MR_MOD(0) = UPOVHR(ECPt,MIN(MNUMYR,CURIYR+XYR))

!     For Existing Only Plant Types Calaculate the Average Heatrattes

      ELSE IF (UPVTYP(ECPt) .EQ. 0) THEN

         IF (FUEL_RGN .EQ. 0) THEN
            S_FLRG = 0
            L_FLRG = UNFRGN
         ELSE
            S_FLRG = FUEL_RGN
            L_FLRG = FUEL_RGN
         END IF
         CYR_INDEX = (CURIYR + UHBSYR + XYR - 1) * 100
         IF (XYR .LT. UNXPH) THEN
            LYR_INDEX = (CURIYR + UHBSYR + XYR - 1) * 100 + 12
         ELSE
            LYR_INDEX = (CURIYR + UHBSYR + UNFPH - 1) * 100 + 12
         END IF
          
         DO IGRP = 1 , EMM_D_GRP
            I_ECP = ULECPT(IGRP)
            HTRT_QRT = ULHRQ(IGRP)
            INIT_GRP = ULIGRP(IGRP)
            IF (I_ECP .GT. 0) THEN
               GRP_ECP_TYPE = UPTTYP(I_ECP)
               IF (ECPt .EQ. WING .AND. GRP_ECP_TYPE .LE. EX_COAL) THEN
                  GRP_ECP_TYPE = ECP_GRP
               END IF
               IF (ECPt .EQ. WIA2 .AND. I_ECP .EQ. WIEC) THEN
                  GRP_ECP_TYPE = ECP_GRP
               END IF

!              IF (I_ECP .EQ. ECPt .AND. ULORGN(IGRP) .EQ. 1 .AND. EMM_RGN .EQ. 1) THEN
!                 WRITE(18,3390) CURIRUN, CURIYR+UHBSYR, CURIYR+UHBSYR+XYR-1, EMM_RGN, FUEL_RGN, ECPt, I_ECP, ECP_GRP, GRP_ECP_TYPE, IGRP, ULSINDX(IGRP), LYR_INDEX, ULRINDX(IGRP), CYR_INDEX, &
!                     ULMRUN(IGRP), UPLNTCD(ECPt), ULSCAP_ECP(IGRP,XYR), ULHTRT_ECP(IGRP,XYR), UECP_HTRT_ADJ(I_ECP), ULTGEN(IGRP), ULTGEN_ECP(IGRP,XYR)
!3390             FORMAT(1X,"HTRT0_UECP",15(":",I6),":",A2,5(":",F21.6))
!              END IF

               IF (GRP_ECP_TYPE .EQ. ECP_GRP .AND. ULORGN(IGRP) .EQ. EMM_RGN) THEN
                  IF (ULSINDX(IGRP) .LE. LYR_INDEX .AND. ULRINDX(IGRP) .GE. CYR_INDEX .AND. ULTGEN_ECP(IGRP,XYR) .GT. 0.0) THEN
                     DO I_FLRG = S_FLRG , L_FLRG
                        IF (I_FLRG .EQ. ULFRGN(IGRP)) THEN

!                          IF (HTRT_RESULTS(ULIGRP(IGRP)) .EQ. 0 .AND. CURIYR + 1989 .EQ. 2015) THEN
!                             WRITE(18,4390) CURIRUN, CURIYR+UHBSYR, CURIYR+UHBSYR+XYR-1, EMM_RGN, FUEL_RGN, ECPt, I_ECP, ECP_GRP, GRP_ECP_TYPE, IGRP, ULSINDX(IGRP), LYR_INDEX, ULRINDX(IGRP), CYR_INDEX, &
!                                ULMRUN(IGRP), UPLNTCD(ECPt), ULSCAP_ECP(IGRP,XYR), ULHTRT_ECP(IGRP,XYR), UECP_HTRT_ADJ(I_ECP), &
!                                ULTGEN(IGRP), ULTGEN_ECP(IGRP,XYR), HTRT_OVR_CST(ECPt), HTRT_FLOOR(ECPt), HTRT_REDUCTION(ECPt)
!4390                         FORMAT(1X,"HTRT1_UECP",15(":",I6),":",A2,8(":",F21.6))
!                          END IF

                           IF (ULMRUN(IGRP) .LT. CURIYR + UHBSYR + XYR - 1) THEN
                              ECP_CAP(I_ECP) = ECP_CAP(I_ECP) + ULSCAP_ECP(IGRP,XYR)
                              ECP_CAP(0) = ECP_CAP(0) + ULSCAP_ECP(IGRP,XYR)
                              AVG_HTRT(I_ECP) = AVG_HTRT(I_ECP) + ULTGEN_ECP(IGRP,XYR) * ULHTRT_ECP(IGRP,XYR) / UECP_HTRT_ADJ(I_ECP)
                              AVG_HTRT(0) = AVG_HTRT(0) + ULTGEN_ECP(IGRP,XYR) * ULHTRT_ECP(IGRP,XYR) / UECP_HTRT_ADJ(I_ECP)
                              ECP_GEN(I_ECP) = ECP_GEN(I_ECP) + ULTGEN_ECP(IGRP,XYR)
                              ECP_GEN(0) = ECP_GEN(0) + ULTGEN_ECP(IGRP,XYR)

                              IF (HTRT_RESULTS(INIT_GRP) .EQ. 0 .AND. HTRT_OVRQ(I_ECP,HTRT_QRT) .GT. 0.0) THEN
                                 ECP_CAP_MOD(I_ECP) = ECP_CAP_MOD(I_ECP) + ULSCAP_ECP(IGRP,XYR)
                                 ECP_CAP_MOD(0) = ECP_CAP_MOD(0) + ULSCAP_ECP(IGRP,XYR)
                                 AVG_HTRT_MOD(I_ECP) = AVG_HTRT_MOD(I_ECP) + MAX( HTRT_FLOOR(I_ECP) , ULHTRT_ECP(IGRP,XYR) * (1.0 - HTRT_REDUCTION(I_ECP)) ) * ULTGEN_ECP(IGRP,XYR) / UECP_HTRT_ADJ(I_ECP)
                                 AVG_HTRT_MOD(0) = AVG_HTRT_MOD(0) + MAX(HTRT_FLOOR(I_ECP) , ULHTRT_ECP(IGRP,XYR) * (1.0 - HTRT_REDUCTION(I_ECP))) * ULTGEN_ECP(IGRP,XYR) / UECP_HTRT_ADJ(I_ECP)
                                 ECP_GEN_MOD(I_ECP) = ECP_GEN_MOD(I_ECP) + ULTGEN_ECP(IGRP,XYR)
                                 ECP_GEN_MOD(0) = ECP_GEN_MOD(0) + ULTGEN_ECP(IGRP,XYR)
                              END IF
                           ELSE
                              ECP_CAP_MR(I_ECP) = ECP_CAP_MR(I_ECP) + ULSCAP_ECP(IGRP,XYR)
                              ECP_CAP_MR(0) = ECP_CAP_MR(0) + ULSCAP_ECP(IGRP,XYR)
                              AVG_HTRT_MR(I_ECP) = AVG_HTRT_MR(I_ECP) + ULTGEN_ECP(IGRP,XYR) * ULHTRT_ECP(IGRP,XYR) / UECP_HTRT_ADJ(I_ECP)
                              AVG_HTRT_MR(0) = AVG_HTRT_MR(0) + ULTGEN_ECP(IGRP,XYR) * ULHTRT_ECP(IGRP,XYR) / UECP_HTRT_ADJ(I_ECP)
                              ECP_GEN_MR(I_ECP) = ECP_GEN_MR(I_ECP) + ULTGEN_ECP(IGRP,XYR)
                              ECP_GEN_MR(0) = ECP_GEN_MR(0) + ULTGEN_ECP(IGRP,XYR)

                              IF (HTRT_RESULTS(INIT_GRP) .EQ. 0 .AND. HTRT_OVRQ(I_ECP,HTRT_QRT) .GT. 0.0) THEN
                                 ECP_CAP_MR_MOD(I_ECP) = ECP_CAP_MR_MOD(I_ECP) + ULSCAP_ECP(IGRP,XYR)
                                 ECP_CAP_MR_MOD(0) = ECP_CAP_MR_MOD(0) + ULSCAP_ECP(IGRP,XYR)
                                 AVG_HTRT_MR_MOD(I_ECP) = AVG_HTRT_MR_MOD(I_ECP) + MAX( HTRT_FLOOR(I_ECP) , ULHTRT_ECP(IGRP,XYR) * (1.0 - HTRT_REDUCTION(I_ECP)) ) * ULTGEN_ECP(IGRP,XYR) / UECP_HTRT_ADJ(I_ECP)
                                 AVG_HTRT_MR_MOD(0) = AVG_HTRT_MR_MOD(0) + MAX(HTRT_FLOOR(I_ECP) , ULHTRT_ECP(IGRP,XYR) * (1.0 - HTRT_REDUCTION(I_ECP))) * ULTGEN_ECP(IGRP,XYR) / UECP_HTRT_ADJ(I_ECP)
                                 ECP_GEN_MR_MOD(I_ECP) = ECP_GEN_MR_MOD(I_ECP) + ULTGEN_ECP(IGRP,XYR)
                                 ECP_GEN_MR_MOD(0) = ECP_GEN_MR_MOD(0) + ULTGEN_ECP(IGRP,XYR)
                              END IF
                           END IF
                        END IF
                     END DO
                  END IF
               END IF
            END IF
         END DO

         IF (ECP_GEN(0) .GT. 0.0) THEN
            AVG_HTRT(0) = AVG_HTRT(0) / ECP_GEN(0)
            TEST = 0
            DO I_ECP = 1 , ECP_D_CAP
               ECP_TYPE = UPTTYP(I_ECP)
               IF (ECPt .EQ. WING .AND. ECP_TYPE .LE. EX_COAL) THEN
                  ECP_TYPE = ECP_GRP
               END IF
               IF (ECP_TYPE .EQ. ECP_GRP) THEN
                  IF (ECP_GEN(I_ECP) .GT. 0.0) THEN
                     AVG_HTRT(I_ECP) = (AVG_HTRT(I_ECP) / ECP_GEN(I_ECP)) * UECP_HTRT_ADJ(I_ECP)
                     TEST = 1
                  ELSE IF (TEST .GT. 0) THEN
                     AVG_HTRT(I_ECP) = AVG_HTRT(0) * UECP_HTRT_ADJ(I_ECP)
                  END IF
               END IF
            END DO
         END IF

         IF (ECP_GEN_MR(0) .GT. 0.0) THEN
            AVG_HTRT_MR(0) = AVG_HTRT_MR(0) / ECP_GEN_MR(0)
            TEST = 0
            DO I_ECP = 1 , ECP_D_CAP
               ECP_TYPE = UPTTYP(I_ECP)
               IF (ECPt .EQ. WING .AND. ECP_TYPE .LE. EX_COAL) THEN
                  ECP_TYPE = ECP_GRP
               END IF
               IF (ECP_TYPE .EQ. ECP_GRP) THEN
                  IF (ECP_GEN_MR(I_ECP) .GT. 0.0) THEN
                     AVG_HTRT_MR(I_ECP) = (AVG_HTRT_MR(I_ECP) / ECP_GEN_MR(I_ECP)) * UECP_HTRT_ADJ(I_ECP)
                     TEST = 1
                  ELSE IF (TEST .GT. 0) THEN
                     AVG_HTRT_MR(I_ECP) = AVG_HTRT_MR(0) * UECP_HTRT_ADJ(I_ECP)
                  END IF
               END IF
            END DO
         END IF

         IF (ECP_GEN_MOD(0) .GT. 0.0) THEN
            AVG_HTRT_MOD(0) = AVG_HTRT_MOD(0) / ECP_GEN_MOD(0)
            TEST = 0
            DO I_ECP = 1 , ECP_D_CAP
               ECP_TYPE = UPTTYP(I_ECP)
               IF (ECPt .EQ. WING .AND. ECP_TYPE .LE. EX_COAL) THEN
                  ECP_TYPE = ECP_GRP
               END IF
               IF (ECP_TYPE .EQ. ECP_GRP) THEN
                  IF (ECP_GEN_MOD(I_ECP) .GT. 0.0) THEN
                     AVG_HTRT_MOD(I_ECP) = (AVG_HTRT_MOD(I_ECP) / ECP_GEN_MOD(I_ECP)) * UECP_HTRT_ADJ(I_ECP)
                     TEST = 1
                  ELSE IF (TEST .GT. 0) THEN
                     AVG_HTRT_MOD(I_ECP) = AVG_HTRT_MOD(0) * UECP_HTRT_ADJ(I_ECP)
                  END IF
               END IF
            END DO
         END IF

         IF (ECP_GEN_MR_MOD(0) .GT. 0.0) THEN
            AVG_HTRT_MR_MOD(0) = AVG_HTRT_MR_MOD(0) / ECP_GEN_MR_MOD(0)
            TEST = 0
            DO I_ECP = 1 , ECP_D_CAP
               ECP_TYPE = UPTTYP(I_ECP)
               IF (ECPt .EQ. WING .AND. ECP_TYPE .LE. EX_COAL) THEN
                  ECP_TYPE = ECP_GRP
               END IF
               IF (ECP_TYPE .EQ. ECP_GRP) THEN
                  IF (ECP_GEN_MR_MOD(I_ECP) .GT. 0.0) THEN
                     AVG_HTRT_MR_MOD(I_ECP) = (AVG_HTRT_MR_MOD(I_ECP) / ECP_GEN_MR_MOD(I_ECP)) * UECP_HTRT_ADJ(I_ECP)
                     TEST = 1
                  ELSE IF (TEST .GT. 0) THEN
                     AVG_HTRT_MR_MOD(I_ECP) = AVG_HTRT_MR_MOD(0) * UECP_HTRT_ADJ(I_ECP)
                  END IF
               END IF
            END DO
         END IF

!     For Biomass Units use heatrate determined by the renewable module - changing to use UPHTRT instead

!      ELSE IF (ECPt .EQ. WIWD .OR. ECPt .EQ. WIBI) THEN ! BECCS
!         AVG_HTRT(ECPt) = WHRIGCC(MIN(CURIYR + XYR - 1 , UNYEAR))
!         AVG_HTRT(0) = WHRIGCC(MIN(CURIYR + XYR - 1 , UNYEAR))
!         AVG_HTRT_MR(ECPt) = WHRIGCC(MIN(CURIYR + XYR - 1 , UNYEAR))
!         AVG_HTRT_MR(0) = WHRIGCC(MIN(CURIYR + XYR - 1 , UNYEAR))

!     For New Dispatchable with heatrate learning determine heatrate for current planning year

      ELSE IF (ECPt .LE. ECP_D_DSP) THEN
         IF (ECP_D_LFHR .GT. 0 .AND. UPDHRSW(ECPt) .GT. 1) THEN

            YRADJ = UPPLYR(ECPt)
            IF (CURIYR + UHBSYR + YRADJ .LE. UPDHRY0(ECPt)) THEN

               AVG_HTRT(ECPt) = UPPHRT0(ECPt)

            ELSE IF (CURIYR + UHBSYR + YRADJ .LE. UPDHRYN(ECPt)) THEN
               YRDIFF = DBLE((CURIYR + UHBSYR + YRADJ) - UPDHRY0(ECPt))
               YRSPAN = DBLE(UPDHRYN(ECPt) - UPDHRY0(ECPt))

               AVG_HTRT(ECPt) = UPPHRT0(ECPt) + ((UPPHRTN(ECPt) - UPPHRT0(ECPt)) * YRDIFF / YRSPAN)

            ELSE
               AVG_HTRT(ECPt) = UPPHRTN(ECPt)
            END IF

         ELSE
            AVG_HTRT(ECPt) = UPHTRT(ECPt)
         END IF
         AVG_HTRT(0) = AVG_HTRT(ECPt)
         IF (ECPt .EQ. WIFC .AND. EPECAP_MR(FUEL_RGN,ECPt,XYR) .GT. 0.0) THEN   ! existing fuel cell is must-run
           AVG_HTRT_MR(ECPt) = AVG_HTRT(ECPt) 
           AVG_HTRT_MR(0) = AVG_HTRT(ECPt)
         ELSE     ! most new DISP is not made must-run
           AVG_HTRT_MR(ECPt) = 0.0 
           AVG_HTRT_MR(0) = 0.0 
         ENDIF
!     For all other units use input heatrate - for renewables this has now been set to the historical average from the AER (EPHTRT_AER())

      ELSE
         AVG_HTRT(ECPt) = UPHTRT(ECPt)
         AVG_HTRT(0) = UPHTRT(ECPt)
         AVG_HTRT_MR(ECPt) = UPHTRT(ECPt)
         AVG_HTRT_MR(0) = UPHTRT(ECPt)
      END IF

!     IF (ECP_CAP(0) .GT. 0.0 .OR. ECP_CAP_MR(0) .GT. 0.0 .OR. UPVTYP(ECPt) .GT. 0) THEN

!      IF (FROM_LABEL .EQ. "EP_PM_LF" .AND. EMM_RGN .EQ. 3 .AND. FUEL_RGN .EQ. 5 .AND. ECPt .GE. 46 .AND. ECPt .LE. 49) THEN
!      IF (CURIYR.EQ.33 .or. curiyr.eq.36.or.curiyr.eq.41) then
!      WRITE(18,3391) FROM_LABEL, CURIRUN, CURIYR+UHBSYR, CURIYR+UHBSYR+XYR-1, EMM_RGN, FUEL_RGN, ECPt, ECP_GRP, UPLNTCD(ECPt), UPHTRT(ECPt), &
!            AVG_HTRT(0) * UECP_HTRT_ADJ(ECPt),     AVG_HTRT_MR(0) * UECP_HTRT_ADJ(ECPt),     ECP_CAP(0),     ECP_CAP_MR(0),     ECP_GEN(0),     ECP_GEN_MR(0), &
!            AVG_HTRT(ECPt), AVG_HTRT_MR(ECPt), ECP_CAP(ECPt),     ECP_CAP_MR(ECPt),     ECP_GEN(ECPt),     ECP_GEN_MR(ECPt), &
!            AVG_HTRT_MOD(0) * UECP_HTRT_ADJ(ECPt), AVG_HTRT_MR_MOD(0) * UECP_HTRT_ADJ(ECPt), ECP_CAP_MOD(0), ECP_CAP_MR_MOD(0), ECP_GEN_MOD(0), ECP_GEN_MR_MOD(0), &
!            AVG_HTRT_MOD(ECPt), AVG_HTRT_MR_MOD(ECPt), ECP_CAP_MOD(ECPt), ECP_CAP_MR_MOD(ECPt), ECP_GEN_MOD(ECPt), ECP_GEN_MR_MOD(ECPt), &
!            UECP_HTRT_ADJ(ECPt)
!      END IF

 3391 FORMAT(1X,"HTRT_UECP",":",A12,7(":",I6),":",A2,26(":",F21.6))

      RETURN
      END