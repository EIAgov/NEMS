! $Header: M:/default/source/RCS/main.f,v 1.416 2020/09/30 15:07:57 nems_auto Exp $
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

      PROGRAM NEMS
      use dfport, ONLY:setenvqq
      use globmain
      use scedes_keys
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'maindbg'
      include 'convfact'
      include 'emmparm'
      include 'uecpout'
      include 'efpout'
      include 'xprsparm'
      include 'continew'
      include 'steoblock'

      INTEGER*4 WALL_TIME_BEGIN,WALL_TIME_END
      INTEGER*4 CPU_TIME_BEGIN,CPU_TIME_END,cpubeg2,cpuend2
      INTEGER IR, IYR
      real wallhour,wallmin,wallsec,cpuhour,cpumin,cpusec
      REAL DUMMY(MNUMYR), RDUMMY(MNUMYR)       ! dummy variables for sending to R program for summary
      include 'fmgr'

      INTEGER RUNRNGPL
      INTEGER NRETCD
	  CHARACTER*40 NEMSPYENV
	  LOGICAL :: dir_e
! +++ Variables to Call FILE_MGR for Initialization
      CHARACTER*18 FM_NAME/' '/
      INTEGER ISTATUS
      LOGICAL NEW/.FALSE./
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      integer iret1
      include 'emission'
      external getindex,rtovalue
      integer getindex,rtovalue
      character*14 cmd
      character*22 cmd2
      CHARACTER*18 UNIQUE_NAME
      CHARACTER*18 DD_NAME
      CHARACTER*10 PDS_NAME
      LOGICAL      PDS_FLAG/.false./
      CHARACTER*1  FUNCTION
      CHARACTER*8  RUN_CHAR
      INTEGER*4 AEOLSTYR   ! run-time option set to calendar year for last year of AEO projection period
      logical(4) lResult

! for calling benchmark R function for summary report generation
      integer BENCH_R
      external BENCH_R

! AIMMS runtime options
      integer AIMMSEFD  ! run-time option to invoke AIMMS EFD LP (if 1)
      integer AIMMSECP  ! run-time option to invoke AIMMS ECP LP (if 1)
      integer AIMMSNG   ! run-time option to invoke AIMMS NGMM (if 1)
      integer EXCOAL    ! run-time option to invoke AIMMS coal (if 1)

! OML STUFF
      include 'omlall.fi'
      INTEGER*4    IRET          ! OML RETURN CODE
      integer*4    IOML/0/       ! oml indicator: 1 if needed
!     Null pointer(s) for passing to functions that do not populate them.
      REAL*8, POINTER :: NULLPD=>NULL()   ! for wfinit - database memory pointer
      INTEGER*4 OMLDBKB   ! for wfinit - KB of memory to allocate for OML database
!     OMLDBKB = 73*1024   ! = 8*9500000 bytes rounded up to nearest megabyte
      OMLDBKB = 0*1024   ! = 8*9500000 bytes rounded up to nearest megabyte
! END OML STUFF


! for cygwin compatibility, clear TZ variable that interferes with time functions
      lResult=setenvqq("TZ= ")

      CALL MPTIMINIT
      call job(0) ! call job status/job control routine - 0 is for initialization
      CALL MPTIM3(CPUBEG2,WALL_TIME_BEGIN)

!   ADD system call
      cmd='hswrk.bat'
      call callsys(iret1,cmd)
! +++ Call Routine Functioning as Block Data
      CALL MBLOCKD

      CALL MPTIM2(CPU_TIME_BEGIN)
      WRITE(6,60) '***Calling ','NINIT',FLOAT(CPU_TIME_BEGIN)/100.
60    FORMAT(1X,A,A,', CPU Time (Seconds)=',F9.1,A,F9.1)
61    FORMAT(1X,A,A,', WALL Time (Seconds)=',F9.1,A,F9.1)

      NEW=.TRUE.
! +++ Call FILE_MGR First Time to Open up and Read the List of Files
      ISTATUS=FILE_MGR('I',FM_NAME,NEW)

!     Get last year index and year from moreopt

      IJUMPYR = RTOVALUE('IJUMPYR ',41)
      IJUMPCALYR = RTOVALUE('IJUMPCAL',2030)

! If this run is part of a multi-run cycle, override the name of the restart file
! with the local copy, which would be the restart file from the prior cycle.
      NUMIRUNS=RTOVALUE('NRUNS   ',1)
      if(NUMIRUNS.gt.1) THEN
        UNIQUE_NAME='RESTARTI'
        DD_NAME='RESTARTI'
        PDS_NAME=' '
        FUNCTION='O'
        IRET1=GETINDEX(UNIQUE_NAME,DD_NAME,PDS_NAME,PDS_FLAG,FUNCTION)
        F_NAME(IRET1)='RESTART.IN'
      ENDIF
      call getirun(RUN_CHAR)
      READ (RUN_CHAR,"(I2)") CURIRUN
      WRITE (6,*) "This is run ",CURIRUN," out of ",NUMIRUNS
	  WRITE(*,*)'TEST OF LEGIRA'	
	  LEGIRA=RTOVALUE('LEGIRA  ', 1)
	  WRITE(*,*)'The Value of LEGIRA is : ',LEGIRA

! +++ Call Initialization Routine to Zero Out Variables
      OPEN(15,FILE='MAINDBUG')
      CALL NINIT

      IF (ITIMNG.EQ.1) THEN
         CALL MPTIM2(CPU_TIME_END)
         WRITE(6,60) '***Back from ','NINIT',FLOAT(CPU_TIME_END)/100., &
          ', Elapsed Time=',FLOAT(CPU_TIME_END - CPU_TIME_BEGIN)/100.
      ENDIF

! +++ Read RESTART Datafile using Prototype Version of FILER
      IF (ITIMNG.EQ.1) THEN
         CALL MPTIM2(CPU_TIME_BEGIN)
         WRITE(6,60) '***Calling ','NDATIN',FLOAT(CPU_TIME_BEGIN)/100.
      ENDIF
      CLBASEYR=RTOVALUE('CLBASEYR',2018)
      WRITE(*,*)'Coal Base Year= ',CLBASEYR
      CALL NDATIN
      CALL MBLOCKD   ! call again after restart file is read to enable changes to variables stored in the restart file

! reset these two now that they are in the restart file and get overwritten when NDATIN is called
      NUMIRUNS=RTOVALUE('NRUNS   ',1)
      READ (RUN_CHAR,"(I2)") CURIRUN

      IF (ITIMNG.EQ.1) THEN
         CALL MPTIM2(CPU_TIME_END)
         WRITE(6,60) '***Back from ','NDATIN',FLOAT(CPU_TIME_END)/100., &
           ', Elapsed Time=',FLOAT(CPU_TIME_END - CPU_TIME_BEGIN)/100.
      ENDIF

	  WRITE(*,*)'Loc 217'
	  call RTOSTRING('NEMSPYENV',NEMSPYENV)
	  WRITE(*,*)'Loc 219'
	  inquire(file='PyFiler\run_PyFiler.bat', exist = dir_e ) 
	     IF ( dir_e ) then
			WRITE(*,*) 'PyFiler Folder Exists'
			WRITE(*,*) dir_e
	        CALL execute_command_line ("PyFiler\run_PyFiler.bat" // " " // NEMSPYENV)
	     else
			WRITE(*,*) 'PyFiler Folder DNE, must be a parallel run'
			WRITE(*,*) dir_e
	        CALL execute_command_line ("..\PyFiler\run_PyFiler.bat" // " " // NEMSPYENV)
	     end if
      WRITE(*,*)'Loc 221'


! +++ Read run-time options from jcl.dat
      CALL NRUSER
      CALL MPTIM2(CPU_TIME_BEGIN)
      WRITE(6,60) '***Calling ','NRUSER',FLOAT(CPU_TIME_BEGIN)/100.

      IF (EXO .EQ. 1 .AND. NUMIRUNS .GT. 1) THEN
        UNIQUE_NAME='SPRFLRT'
        DD_NAME='SPRFLRT'
        PDS_NAME=' '
        FUNCTION='O'
        IRET1=GETINDEX(UNIQUE_NAME,DD_NAME,PDS_NAME,PDS_FLAG,FUNCTION)
        F_NAME(IRET1)='SPRFLRT'
      ENDIF

!  Connect to Oracle
      IF (EXO .EQ. 0) CALL RESETOSW("ORCLPMM ",0)
      IF (EXL .EQ. 0) CALL RESETOSW("ORCLOGSM",0)
      IF (RTOVALUE('ORACLESW',0) .GE. 1) THEN
!         CALL ORCLTABS
      ELSE
          CALL RESETOSW("ORCLECP ",0)
          CALL RESETOSW("ORCLEFP ",0)
          CALL RESETOSW("ORCLEFD ",0)
          CALL RESETOSW("ORCLCL  ",0)
          CALL RESETOSW("ORCLPMM ",0)
          CALL RESETOSW("ORCLOGSM",0)
      ENDIF

      XPRESSSW=0
! get aimms version to use for coal module (3: aeo2015, 4:aeo2016)
      AIMMSVER=RTOVALUE('AIMMSVER',3)
      if(exc.eq.1) excoal=rtovalue('EXCOAL  ',1)

! If OML needed, call memory initialization routine
      if( exe.eq.1 .or. (exc.eq.1.and.excoal.eq.0) .or. exh .eq. 1 ) IOML=1
      if(IOML.eq.1) then

         call mnflush

         XPRESSSW=RTOVALUE('XPRESSSW',0)
         write(6,*) 'calling wfinit'
         iret = wfinit('NEMS'//char(0),NULLPD,OMLDBKB)

         IF (IRET .NE. 0) THEN
           WRITE(6,'(1x,a,i5/1x,a)') &
           ' MAIN: OML WFINIT ERROR--DATABASE BUFFER SPACE ALLOCATION PROBLEM, RETURN CODE=',IRET, &
           'CONT--OML WFINIT ERROR, RESET EXE, EXC, EXN = 0; EXECUTION CONTINUES'
           EXE = 0                  ! TURN OFF EMM
           EXN = 0                  ! TURN OFF RENEW (EMM dependent)
           EXC = 0                  ! TURN OFF COAL
           EXH = 0                  ! TURN OFF Hydrogen
           RUNMOD( 7)=EXE ! UTIL
           RUNMOD(12)=EXN ! RENEW
           RUNMOD( 8)=EXC ! COAL
           RUNMOD(13)=EXH ! Hydrogen
           XPRESSSW=0               ! Turn off Xpress as well
         ELSE
           WRITE(6,'(1X,a)') 'MAIN: OML WFINIT SUCCESSFUL--DATABASE BUFFER SPACE ALLOCATED'
!          Obtain access to the optimization subset of the MPSIII "Communication Region" (CR).
!          Include file wcr.fi (as adapted for NEMS) declares OML as a pointer to a structure of type FWCR.
!          OML is placed in named common, so wfgetpwcr needs to be called only once.
           CALL WFGETPWCR(OML)
         ENDIF
      ENDIF ! if oml needed
      IF (ITIMNG.EQ.1) THEN
         CALL MPTIM2(CPU_TIME_END)
         WRITE(6,60) '***Back from ','NRUSER',FLOAT(CPU_TIME_END)/100., &
           ', Elapsed Time=',FLOAT(CPU_TIME_END - CPU_TIME_BEGIN)/100.
      ENDIF

! +++ Read any remaining data specific to the Integrating Model
      CALL NROTHR
      CALL INITIALIZE_API
      ITERDUMP=RTOVALUE('ITERDUMP',0) ! if 1, creates single-year restart file after every iteration.
! ++++OPEN DEBUG FILE IF DBDUMP SWITCH IS ON
      IF(DBDUMP.EQ.1) THEN
         MNPQIT='MNPQIT'
         NEW=.TRUE.
         IMNPQIT=FILE_MGR('O',MNPQIT,NEW)  ! IMNPQIT IS IN /MAINDBG/
         CNVRTOUT='CNVRTOUT'
         ICNVRTOUT=FILE_MGR('O',CNVRTOUT,NEW)  ! ICNVRTOUT IS IN /CONVFACT/
      ENDIF
! +++ DEBUG INFORMATION HEADER
      CALL MAINDBGRPT(1)


! if RUNRNGPL=1, call routine to estimate ethane and propane prices with external R program
      RUNRNGPL=RTOVALUE('RUNRNGPL',1)
      IF (RUNRNGPL .EQ. 1) THEN
          call stat_price
      ELSE
          write(6,'("  Not running R program for propane and ethane pricing by request")')
      ENDIF


! +++ Call Solution Algorithm.  A nonzero return code causes the Final Output Step to be omitted.
      NRETCD=0
      IF (ITIMNG.EQ.1) THEN
         CALL MPTIM2(CPU_TIME_BEGIN)
         WRITE(6,60) '***Calling ','NSOLVE',FLOAT(CPU_TIME_BEGIN)/100.
      ENDIF
      CALL NSOLVE(NRETCD)
      IF (ITIMNG.EQ.1) THEN
         CALL MPTIM2(CPU_TIME_END)
         WRITE(6,60) '***Back from ','NSOLVE',FLOAT(CPU_TIME_END)/100., &
           ', Elapsed Time=',FLOAT(CPU_TIME_END - CPU_TIME_BEGIN)/100.
      ENDIF

! Print out helpful message summarizing the convergence of the reserve margin and the reliability
! component of price within EMM.  It's all very complicated.
!     WRITE(6,'(A,A)')    " CNV_EMM:  NERC Region 01    02    03    04    05    06    07    08", &
!                                            "    09    10    11    12    13    14    15    16"
!     WRITE(6,'(A,<MNUMNR>F6.2)') " CNV_EMM:  Target  ",(URELTGT(IR),IR=1,MNUMNR)
!     WRITE(6,'(A,<MNUMNR>F6.2)') " CNV_EMM:  Result  ",(PECASRLN(IR,LASTYR),IR=1,MNUMNR)

! +++ Create Datafile for Future RESTART, REPORTING, and DATABASE loading.

      IF (NRETCD.EQ.0) THEN
         IF (ITIMNG.EQ.1) THEN
            CALL MPTIM2(CPU_TIME_BEGIN)
            WRITE(6,60) '***Calling ','NDATOT',FLOAT(CPU_TIME_BEGIN)/100.
         ENDIF
         DO IYR=FIRSYR,LASTYR  ! sum up everything to catch retroactive history updates, etc.
           CURIYR=IYR
           CURCALYR=CURIYR + BASEYR - 1
           CALL SUMQAS
           CALL AVEPAS
           CALL COPY_ADJUSTED
           CALL AVEPASA
         ENDDO
         CALL NDATOT('   ')
         CALL NDATOT('GDX') ! call with argument to write GDX format restart file also
      ENDIF
      CALL MPTIM2(CPU_TIME_END)
      WRITE(6,60) '***Back from ','NDATOT',FLOAT(CPU_TIME_END)/100., &
         ', Elapsed Time=',FLOAT(CPU_TIME_END - CPU_TIME_BEGIN)/100.

      CALL MAINDBGRPT(3)  ! CALL TO WRITE OUT OPTIONS TO END OF REPORT
      IRET = BENCH_R(DUMMY,DUMMY(1:LASTSTEOYEAR-1989),'DUMMYDUMMY  ',RDUMMY,0)   ! call to summarize (last argument is 0)

      CALL MPTIM3(CPUEND2,WALL_TIME_END)
        WRITE(6,60) '#*** Done with ','all of NEMS ', &
         FLOAT(cpuend2)/100.,', Elapsed Time=',FLOAT(cpuend2)/100.

        cpuhour=aint(float(cpuend2)/100./3600.)
        cpumin=aint(float(cpuend2)/100./60) - (cpuhour*60.)
        cpusec=aint(float(cpuend2)/100.)-cpuhour*3600.-cpumin*60.
        write(6,'(1x,a,i2.2,a,i2.2,a,i2.2)') '#*** Total CPU '// &
         '(hr:min:sec) =',int(cpuhour),':',int(cpumin),':',int(cpusec)

        WRITE(6,61) '#*** Done with ','all of NEMS ', &
           FLOAT(WALL_TIME_END)/100., &
           ', Elapsed Time=',FLOAT(WALL_TIME_END)/100.
        wallhour=aint(float(WALL_TIME_END)/100./3600.)
        wallmin=aint(float(WALL_TIME_END)/100./60)-wallhour*60.
        wallsec=aint(float(WALL_TIME_END)/100.)- &
           wallhour*3600.-wallmin*60.
        write(6,'(1x,a,i2.2,a,i2.2,a,i2.2)') '#*** Total Wall '// &
           '(hr:min:sec)=',int(wallhour),':',int(wallmin),':',int(wallsec)
      call mnclose

! if oml is being used, close it out
      if(ioml.eq.1) then
        call wfend
        write (6,*) "OML closed."
      endif

! AIMMS_EFD was run, close it out
      AIMMSEFD=RTOVALUE('AIMMSEFD',0)
      IF (AIMMSEFD .EQ. 1 .and. EXE.eq.1) THEN
         CALL AIMMS_EFD('end')  ! invokes subroutine in uefd.f to invoke maintermination routine and close the aimms project.
      ENDIF

! AIMMS_ECP was run, close it out
      AIMMSECP=RTOVALUE('AIMMSECP',0)
      IF (AIMMSECP .EQ. 1 .and. EXE.eq.1) THEN
         CALL AIMMS_ECP('end')  ! invokes subroutine in uecp.f to invoke maintermination routine and close the aimms project.
      ENDIF

      IF ((CONTINW + CONTINM + CONTINR + CONTINK + CONTINI + CONTINT + CONTINE +    &
           CONTINC + CONTINL + CONTING + CONTINO + CONTINN + CONTINH) .LT. 13)      & ! should all be 1
            write(6,'("  The following NEMS modules have experienced problems in cycle ",I4)') CURIRUN
      IF (CONTINW .EQ. 0) THEN
         IF (EXW .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 1),REASONW
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 1),REASONW
         ENDIF
      ENDIF
      IF (CONTINM .EQ. 0) THEN
         IF (EXM .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 2),REASONM
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 2),REASONM
         ENDIF
      ENDIF
      IF (CONTINR .EQ. 0) THEN
         IF (EXR .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 3),REASONR
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 3),REASONR
         ENDIF
      ENDIF
      IF (CONTINK .EQ. 0) THEN
         IF (EXK .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 4),REASONK
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 4),REASONK
         ENDIF
      ENDIF
      IF (CONTINI .EQ. 0) THEN
         IF (EXI .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 5),REASONI
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 5),REASONI
         ENDIF
      ENDIF
      IF (CONTINT .EQ. 0) THEN
         IF (EXT .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 6),REASONT
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 6),REASONT
         ENDIF
      ENDIF
      IF (CONTINE .EQ. 0) THEN
         IF (EXE .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 7),REASONE
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 7),REASONE
         ENDIF
      ENDIF
      IF (CONTINC .EQ. 0) THEN
         IF (EXC .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 8),REASONC
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 8),REASONC
         ENDIF
      ENDIF
      IF (CONTINL .EQ. 0) THEN
         IF (EXL .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 9),REASONL
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES( 9),REASONL
         ENDIF
      ENDIF
      IF (CONTING .EQ. 0) THEN
         IF (EXG .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES(10),REASONG
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES(10),REASONG
         ENDIF
      ENDIF
      IF (CONTINO .EQ. 0) THEN
         IF (EXO .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES(11),REASONO
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES(11),REASONO
         ENDIF
      ENDIF
      IF (CONTINN .EQ. 0) THEN
         IF (EXN .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES(12),REASONN
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES(12),REASONN
         ENDIF
      ENDIF
      IF (CONTINH .EQ. 0) THEN
         IF (EXH .EQ. 1) THEN
            write(6,'("NEMS module:  ",A6,"  Problem: ",A20)') SUBR_NAMES(13),REASONH
         ELSE
            write(6,'("From input restart file, module off for this execution:  ",A6,"  Problem: ",A20)') SUBR_NAMES(13),REASONH
         ENDIF
      ENDIF

      write (6,'(a,I2,a)') ' NEMS run ',CURIRUN, ' completed.'
      call job(2) ! call job status reporting routine. 2 means set status 'complete'
      STOP ' '
      END
      subroutine callsys(iret,cmd)
      use dfport
      character*(*) cmd
      integer iret
      write(6,'(a,a)') ' Calling system to do this:  ',trim(cmd)
      iret=system(cmd)
      if(iret.ne.0) then
        write(6,'(a,I2,a,a)') '   Command failed with return code of ',iret,':  ',trim(cmd)
      endif
      return
      end
      subroutine mnclose
      implicit none
! closes all files
      integer low/1/,high/999/
      integer i
      logical opn
      do i=low,high
         if (i .ne. 6 .and. i .ne. 5) then
           inquire(unit=i,opened=opn)
           if (opn) close(i)
         endif
      enddo
      end
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
! *****************************************************************
      subroutine mainomlinit
      implicit none
      include 'omlall.fi'
!     Calls are now in coalcds.f, uecp.f, and uefd.f so as to ensure
!       that this routine is called when needed and only when needed
!       (and when the OML pointer is properly associated).
      OML.XOBJ='*'
      OML.XRHS='*'
      oml.XBOUND='*'
      oml.XRANGE='*'
!     set OML parameters to limit output to SYSPRNT
      OML.XFREQLOG=0
      OML.XFREQSUM=0
!     NEMS is not now using in-memory matrix modifications.
!     A negative value for XSLPNZ causes the "whiz directory" to be
!        maintained on a temporary actfile rather than in memory.
!        This works MUCH better for solution retrievals based on masks.
      OML.XSLPNZ=-1
      return
      end
!******************************************************************
!*  Subroutine NSOLVE(NRETCD)
!*
!*  Solves the NEMS Modules in an Interative Convergence Algorithm
!******************************************************************
      SUBROUTINE NSOLVE(NRETCD)
      use globmain
      IMPLICIT NONE

      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'epmmpblk'    ! use adjusted prices  and regular pw2
      include 'macout'
      include 'intout'
      include 'indout'     ! used for INQLGHP only.  when switch to QPRIN, remove this
      include 'pmmrpt'
      include 'pmmout'
      include 'uefdout'
      include 'ngtdmout'
      include 'ngtdmrep'
      include 'cdsparms'
      include 'coalemm'
      include 'coalprc'
      include 'epmclprc'
      include 'pqchar'
      include 'epmngtdm'
      include 'maindbg'
      include 'epmcntl'      ! emission policy control options
      include 'emission'     ! convergence test on tax EMETAX
      include 'convfact'
      include 'uso2grp'
      include 'ogsmout'
      include 'wrenew'
      include 'tranrep'
      common/passmodel/modelno
      integer modelno

! +++ Argument:
      INTEGER NRETCD             ! Return Code. Set to NONZERO to omit further
                                 ! Output.
! +++ Local Variables:
      INTEGER IMODEL             ! Do Loop Index for Looping over Submodels
      INTEGER IR                 ! Subscript Index for REGION
      INTEGER IV                 ! Subscript Index for VARIABLE
      INTEGER IYR                ! Subscript Index for Year to keep CURIYR at LASTYR after loop
      INTEGER I,J                ! Subscript Index for temp use
      INTEGER MOD_CONV           ! Flag for testing whether a model's converged
      INTEGER EMODEL,IM          ! Integers for matching Model Order to Model
                                 ! Number
      INTEGER FILE_MGR           ! File manager function
      INTEGER IRESTRP            ! RUNTIME OPTION FOR ADJUSTMENT TO RESTART Ps
      INTEGER MAXITRJR           ! RUNTIME OPTION FOR MAXITR IN 90,91
      INTEGER MAXITRSR           ! RUNTIME OPTION FOR MAXITR 2001-ON
      INTEGER MAXITRDF           ! STORE ORIGINAL MAXITR
      INTEGER RTOVALUE           ! FUNCTION TO READ RUN TIME OPTIONS
      EXTERNAL RTOVALUE, FILE_MGR

      INTEGER NTIMITR
      PARAMETER (NTIMITR=11)
      REAL CTIMES(NTIMITR+1,NMODEL,MNUMYR)
      REAL WTIMES(NTIMITR+1,NMODEL,MNUMYR)
      COMMON/MAINTIME/CTIMES,WTIMES
      INTEGER WALL_TIME_BEGIN, WALL_TIME_END
      INTEGER WALLSW
      COMMON /MNWALLSW/ WALLSW

      COMMON/MAXITRSR/MAXITRSR
      CHARACTER*132 TLINE
      INTEGER NKEEP,ITOT
      REAL TOTTIME, TOTWALL

      LOGICAL NEW/.TRUE./
      LOGICAL OLD/.FALSE./
      LOGICAL STABLE             ! True if Convergence Achieved on FCRL Loop
      INTEGER*4 MIDTERM       !RESTART YR/FLAG (0=OFF,>1990=ON)
      INTEGER*4 MIDITR        !MAXITR PRIOR to RESTART YR
      integer*4 cutitr,doaminof
      integer inanq ! Return code from Check_For_NaNq.  1 indicates a NaN found among convergence variables.
      INTEGER IY

! GET LOCAL RUNTIME OPTIONS FOR GENERUAL PURPOSE MOREOPT FILE USING RTOVALUE FUNCTION
      IRESTRP =RTOVALUE('IRESTRP ',0) ! 1 IS YES, 0 IS NO
      MAXITRJR=RTOVALUE('MAXITRJR',1) !  MAXITR OVERWRITE FOR 90,91,92,93
      MAXITRSR=RTOVALUE('MAXITRSR',5) !  MAXITR OVERWRITE FOR 2001 ON
      RUNEPM  =RTOVALUE('RUNEPM  ',0) !  SWITCH TO RUN EMISSIONS MOD
      WALLSW  =RTOVALUE('WALLSW  ',0) !  SWITCH for wall timing
      CUTITR  =RTOVALUE('CUTITR  ',0) !  SWITCH to cut maxitr on early cycles
      DOAMINOF=RTOVALUE('DOAMINOF',1) !  Minimum number of cycles requested

! If switch is on, cut maxitr on early cycles (defined as those less than the minimum number of cycles requested)
      if(CUTITR.eq.1 .and. curirun.lt.DoAMinOf) then
         write(6,*)'CUTITR=1 so cutting MAXITR on this early cycle to 2 to save time'
         maxitr=2
      endif

      MAXITRDF=MAXITR                 !  STORE ORIGINAL MAXITR
      IF(MAXITR.LT.MAXITRSR) MAXITRSR=MAXITR
      IF(MAXITR.LT.MAXITRJR) MAXITRJR=MAXITR
      MIDTERM=RTOVALUE('MIDTERM ',0)  !RESTART YR/FLAG (0=OFF, >1990=ON)
      MIDITR=RTOVALUE('MIDITR  ',2) !MAXITR PRIOR tO RESTART YR
      modelno=0
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+  MORDER holds the Module execution position for the modules in the order:
!+         WORLD, MACRO, RESIDENTIAL, COMMERCIAL, INDUSTRIAL, TRANSPORTATION,
!+         ELECTRIC UTILITY COAL, WELLHEAD, PIPELINE, REFINERY, & RENEWABLES.
!+
!+  To change the order of execution, change the numbers in the array.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      WRITE(*,'(a)') ' #Starting NSOLVE'  !DEBUG
      CONVOTH = 0.0
      CALL NDEBUG(0,6)
      CURIYR=FIRSYR
      CURCALYR=CURIYR + BASEYR - 1

      inanq=0 !  Controls NaN testing

      DO WHILE (CURIYR.LE.LASTYR)
         WRITE(*,'(a,i3)') ' ## Starting New Year Loop, CURIYR=',CURIYR
         CURITR=1                       ! Initialize Current Iteration

         MAXITR=MAXITRDF
         IF(CURIYR.LE.(2000-(BASEYR-1))) THEN
            MAXITR=MAXITRJR
         ELSE
            IF(CURIYR.GE.(2001-(BASEYR-1)))MAXITR=MAXITRSR
            IF ((MIDTERM .GT. BASEYR) .AND. (CURIYR .LT. (MIDTERM-(BASEYR-1)))) MAXITR = MIDITR
         ENDIF
         FCRL=0
         NCRL=0
         STABLE=.FALSE.
         IF (IRESTRP .EQ. 1) CALL NSTOREP
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+  Restart Point for Final Convergence and Reporting Loop:
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         DO WHILE ((.NOT.STABLE).AND.CURITR.LE.MAXITR+1)
            CTEST=0                     ! Initialize Global Convergence Flag
            DO IMODEL=1,NMODEL
               CNVTST(IMODEL,CURIYR)=0  ! Initialize Submodel Convergence Flags
            ENDDO

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+  Convergence/Iteration Loop:
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            DO WHILE (CTEST.EQ.0 .AND. (CURITR.LE.MAXITR.OR. &
              (CURITR.EQ.MAXITR+1.AND.FCRL.EQ.1)) .AND. NCRL .EQ. 0)
               WRITE(*,'(A,I2)') ' ##   Starting Iteration Loop,CURITR=',CURITR
               DO I = 1,MNDBGVARS
                 DO J=1,3
                   DTBAC(I,J) = 0.
                 ENDDO
                 DTREG(I)=0
                 DTVAR(I)=' '
               ENDDO
! the following commented call could be used to implement checks/resetting of initial prices to make sure
! the demand models are not exposed to errant prices that might be lingering in the the restart file.
!!!!!          IF(CURITR.EQ.1) CALL MNPRCHK

! +++ Initialize Global Convergence Flag ON - It will be turned OFF below when submodels do not meet convergence criteria.
               CTEST=1

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+  Submodel LOOP:
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++ Store End-Use and Other (GEO/MSW) Carbon Emissions to Use in ECP Row
               IF (CURIYR .EQ. 1 .AND. CURITR .EQ. 1)THEN
                 DO IY = 1 , MNUMYR
                   ECP_SCAR(IY) = EMRSC(11,1,IY) + EMCMC(11,1,IY) + EMINCC(11,1,IY) +  &
                                  EMTRC(11,1,IY) + EMNT(11,1,IY) * 0.001
                   ECP_OCAR(IY) = EMEL(4,1,IY)
                 END DO
!+++ Read in emissions data before executing models
                   CALL EPM_READ
               END IF
               DO EMODEL=1,NMODEL
                  DO IM=1,NMODEL
                     IF (EMODEL.EQ.MORDER(IM)) IMODEL=IM
                  ENDDO

! +++ Submodel Conditional: Check if this Model converged for all inner years.   If so, don't rerun it.
                  IF (RUNMOD(IMODEL).EQ.1) THEN
                   modelno=imodel
                   WRITE(*,'(A,I2,4A)')' #      Submodel Loop, IMODEL=', &
                        IMODEL,',',SUBR_NAMES(IMODEL),' -- ',SUBR_DESCR(IMODEL)
                     MOD_CONV=1
                     IF(CNVTST(IMODEL,CURIYR).EQ.0) MOD_CONV=0

                     IF (MOD_CONV.EQ.0.OR.CURITR.LE.2.OR.MODELON.EQ.1) THEN
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+  Update expectations variables for the current year.
!+  (Copy current P,Q to expectations variables.)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                        IF(CURITR.EQ.1) CALL NXPECT(IMODEL)

! +++ Store previous iteration values for next convergence check.
! +++ MUST USE UNADJUSTED PRICES, SINCE WE TEST THEM
                        DO IR=1,MNUMCR
                          DO IV = 1,MNUMP
                            PREVP(IR,CURIYR,IV) = MPRC(IR,CURIYR,IV)
                          ENDDO
                          DO IV = 1,MNUMQ
                            PREVQ(IR,CURIYR,IV) = MQTY(IR,CURIYR,IV)
                          ENDDO
                        ENDDO
                        DO IR=1,NNGEM  !NGMM  REGION
                          DO IV = 1,3 ! UTIL GAS P,Q
                            PREVUP(IR,CURIYR,IV) = MUPRC(IR,CURIYR,IV)
                            PREVUQ(IR,CURIYR,IV) = MUQTY(IR,CURIYR,IV)
                          ENDDO
                          DO IV = 4,7
                            PREVUP(IR,CURIYR,IV) = SMUPRC(IR,CURIYR,IV-3)
                            PREVUQ(IR,CURIYR,IV) = SMUQTY(IR,CURIYR,IV-3)
                          ENDDO
                          DO IV = 1,3 ! seasonal (3 of them) electric power GAS P,Q
                            PREVGASP(IR,CURIYR,IV) = SGASPRICE(IR,CURIYR,IV)
                            PREVGASQ(IR,CURIYR,IV) = SGASQUANT(IR,CURIYR,IV)
                          ENDDO
                        ENDDO

                        DO IR=1,NDRGG   !COAL REGION
                          PREVCPS(1,IR,CURIYR) = PCLELCDR(1,IR,CURIYR)
                          PREVCPS(2,IR,CURIYR) = PCLELCDR(2,IR,CURIYR)
                          DO IV = 1,NCLUT1   ! UTIL COAL QUANTS
                            PREVCQS(IR,CURIYR,IV) = QCLCLNR(IR,CURIYR,IV)
                          ENDDO
                        ENDDO
                        DO IV = 1,MNOTH
                          CONVOTH(1,IV,CURIYR)=CONVOTH(2,IV,CURIYR)
                        ENDDO
                        DO IR=1,MX_NCOALS   !COAL SUPPLY CURVES
                            PREVPCAP(IR,CURIYR) = XCL_PCAP(IR,CURIYR)
                        ENDDO
                        DO IR=1,MX_UNITS   !COAL GENERATION UNITS
                            PREV_CL_CF(IR,CURIYR) = EMM_CL_CF(IR,CURIYR)
                            PREV_CL_BTUs(IR,CURIYR) = EMM_CL_BTUs(IR,CURIYR)
                        ENDDO
! call nems module IMODEL
                        CALL NEXEC(IMODEL,NRETCD)

                        IF (NRETCD.NE.0) RETURN

! Recalculate national totals for PBMET and QBMET to ensure they add up
                        DO IV=1,NUMETHQ
                        QBMET(MNUMCR,CURIYR,IV) = sum(QBMET(1:9,CURIYR,IV))
                        PBMET(MNUMCR,CURIYR,IV) = 0.0
                        DO IR=1,9
                           IF (QBMET(MNUMCR,CURIYR,IV) .NE. 0.0) &
                           PBMET(MNUMCR,CURIYR,IV) = PBMET(MNUMCR,CURIYR,IV) + &
                               QBMET(IR,CURIYR,IV) * PBMET(IR,CURIYR,IV) / QBMET(MNUMCR,CURIYR,IV)
                        ENDDO
                        ENDDO

!  Set up the array for convergence testing of non-PQ items.
!  Assign new variables to be tested at this point.

                        CONVOTH(2,1,CURIYR) = MC_GDPR(CURIYR)
                        CONVOTH(2,2,CURIYR) = IT_WOP(CURIYR,1)
                        CONVOTH(2,3,CURIYR) = RFQICRD(MNUMPR,CURIYR)
                        CONVOTH(2,4,CURIYR) = RFPQIPRDT(MNUMPR,CURIYR,2)
                        CONVOTH(2,5,CURIYR) = EMETAX(1,CURIYR)
                        CONVOTH(2,6,CURIYR) = EMELPSO2(CURIYR,1)
                        CONVOTH(2,7,CURIYR) = ECP_PHG(1,CURIYR)
                        CONVOTH(2,8,CURIYR) = OGWPRNG(MNUMOR,CURIYR)
                        CONVOTH(2,9,CURIYR) = OGCNPPRD(1,CURIYR)
                        CONVOTH(2,10,CURIYR) = OGCNPPRD(2,CURIYR)
                        CONVOTH(2,21:29,CURIYR) = PBMET(1:9,CURIYR,1)
                        CONVOTH(2,30,CURIYR) = PBMET(MNUMCR,CURIYR,1)
                        CONVOTH(2,31:39,CURIYR) = QBMET(1:9,CURIYR,1)
                        CONVOTH(2,40,CURIYR) = QBMET(MNUMCR,CURIYR,1)
                        CONVOTH(2,43,CURIYR) = GLBCRDDMD(CURIYR)
                        CONVOTH(2,44:54,CURIYR) = ECP_PSO2(0:10,CURIYR,1)
                        CONVOTH(2,55:65,CURIYR) = ECP_PSO2(0:10,CURIYR,2)


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+  Set convergence flags for the moduel: CNVTST(IMODEL,CURIYR) in
!+  CVTEST.  CVTEST computes absolute and percentages changes for
!+  the key convergence variables, and sets CNVTST(IMODEL,CURIYR)
!+  to 1 if changes are less than the tolerance. It sets global
!+  convergence flag, CTEST, to 0 if any submodel does not converge.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                        IF (CURITR.GE.2) CALL CVTEST(IMODEL)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+  If the relaxation option is ON, call heuristic routine to limit
!+  or reset certain model outputs between iterations to speed
!+  overall convergence.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                        IF ((CURITR.GE.2).AND.(IRELAX.GE.1) ) CALL RELAX

! +++ Compute ALL-SECTOR totals for NEMS QUANTITY variables
                        CALL SUMQAS

! +++ Compute ALL-SECTOR averages for NEMS PRICE variables
                        CALL AVEPAS
! +++ COMPUTE ALL-SECTOR AVERAGES FOR NEMS ADJUSTED PRICE VARIABLES FOR EPM
! +++ COPY UNADJUSTED PRICES to ADJUSTED PRICES, ADDING ANY POLICY-RELATED TAXES (eg CARBON)
                        CALL COPY_ADJUSTED
                        CALL AVEPASA
                        CALL DOCVFACTS

! +++ Write DEBUG information
                        CALL NDEBUG(IMODEL,6)

! Check for NaN (Not a Number) values in convergence variables.  If found, write restart file,
! since standalone debugging will be easier after the crash using this output restart file.
                        if(inanq.eq.0) then
                          CALL Check_for_NaNq(inanq)
                          IF (inanq.eq.1) then
                           write(6,*)' NaNq detected after return of '// &
                             trim(SUBR_NAMES(IMODEL))//'.  Writing restart file.'
                           CALL NDATOT('   ')
                          endif
                        endif

                     ENDIF        ! End of IF Model already converged
                  ELSE                     ! Else model not being run

! +++ If module not being run, set its convergence flag
                     CNVTST(IMODEL,CURIYR)=1
                  ENDIF                     ! (RUNMOD(IMODEL).EQ.1)
               ENDDO                        ! of Model Loop
               modelno=0

! call routine to overwrite NEMS quantities with seds values through "msedyr" year if HISTORY=1

               CALL SEDSHIST

!  CALL EMISSION MODULE.  Computes carbon emission and implements some policy options.
               IF(RUNEPM.EQ.1) CALL EPM

! +++ Check if all modules have converged for all years in Inner Year Loop
               CTEST=1
               DO IMODEL=1,NMODEL
                  IF (CNVTST(IMODEL,CURIYR).EQ.0) CTEST=0
               ENDDO
               IF (CTEST.EQ.0) THEN
                  IF (FCRL.EQ.1.AND.CURITR.LE.MAXITR) THEN
                     WRITE(*,'(2a,I4,A,I4)') ' ##  Out of convergence on', &
                         ' Final Convergence and Reporting Loop:  Year= ',CURCALYR, &
                         '  , Iteration= ',CURITR
                     WRITE(*,'(a)') ' ##  Resetting FCRL=0 and continuing.'
                     FCRL=0
                  ELSEIF (FCRL.EQ.1) THEN
                     WRITE(*,'(1x,3A,I2,I5)') '##  Not converged', &
                         ' on Final Convergence and Reporting Loop.', &
                         '  CURIYR=',CURIYR,CURIYR+BASEYR-1
                     WRITE(*,'(2a)') ' ##  Will not continue because '// &
                         'the maximum iterations have been performed.'
                  ENDIF
               ELSE
                  IF (FCRL.EQ.1) THEN
                     WRITE(*,'(1x,3A,I2,I5)')'##   Convergence ', &
                         'maintained on Final Convergence and Reporting', &
                         ' Loop.  CURIYR=',CURIYR,CURIYR+BASEYR-1
                     STABLE=.TRUE.
                  ENDIF
                  IF (FCRL.EQ.0) THEN
                     WRITE(*,'(1X,A,I2)') '##   Convergence achieved on iteration',CURITR
                     WRITE(*,'(a)') &
                         ' ##   Entering Final Convergence and Reporting Loop.  Setting FCRL=1'
                     FCRL=1
                  ENDIF
               ENDIF
!  STORE THIS ITERATION'S VALUES ON DEBUG FILE
               IF(DBDUMP.EQ.1) CALL STMNPQIT
               IF(ITERDUMP .EQ. 1 .and. curcalyr.gt.2010) CALL NDATOT('ITR')  !
               CURITR=CURITR+1           ! Increment Iteration Count

            ENDDO              ! End of Convergence/Iteration Loop

            IF (CTEST.EQ.0.AND.CURITR.LE.MAXITR+1.AND.FCRL.EQ.0) THEN
               WRITE(*,'(1X,A,I2)') &
                   '##   Maximum iterations reached without converging.'
               WRITE(*,'(a)') ' ##   Entering Final Convergence and Reporting Loop.  Setting FCRL=1'
               FCRL=1
            ENDIF

         ENDDO                  ! End of Stability/FCRL loop

         CALL MAINDBGRPT(2) ! PRINT SUMMARY OF CONVERGENCE STATISTICS

!   Do the modules that reference NCRL for the final pass.

         NCRL = 1
         WRITE (6,'(a,i2)') ' ## Executing modules set for NCRL reporting loop, CURITR=',CURITR
! Industrial, WELL, NGMM, AND REFINE
         IF(RUNMOD(5).EQ.1) CALL NEXEC2(5,NRETCD)
         IF(RUNMOD(9).EQ.1) CALL NEXEC2(9,NRETCD)
         IF(RUNMOD(10).EQ.1) CALL NEXEC2(10,NRETCD)
!        IF(RUNMOD(11).EQ.1) CALL NEXEC2(11,NRETCD)
         IF (RUNMOD(3).EQ.1.AND.CURIYR.EQ.LASTYR) CALL HEM
         IF(RUNEPM.EQ.1) CALL EPM   ! will just calculate emissions. If last year, writes some debug

! Sum up arrays one more time, as industrial redoes their Q variables with the revised refinery consumption.
         CALL SUMQAS
         CALL AVEPAS
         CALL COPY_ADJUSTED
         CALL AVEPASA
         CALL DOCVFACTS
!  WRITE OUT RESTART FILE AT END OF YEAR IF DESIRED
         IF(DBDUMP .EQ. 1) CALL NDATOT('   ')


         IF(ITIMNG.EQ.1) THEN
           WRITE(6,'(A,I4)') ' #*** CPU Time Summary for ',CURIYR+BASEYR-1
           NKEEP=MIN(CURITR-1,NTIMITR-1)
           TLINE=' '
           TOTTIME=0.
           TOTWALL=0.
           WRITE(TLINE,'(A11,9(2x,A4,i1),9(1x,a4,i2))') '#***MODULE ',('Itr-',I,I=1,NKEEP)
           TLINE(12+NKEEP*7:)=' Report  Total'
           WRITE(6,'(1x,a)') TLINE
           ITOT=NTIMITR+1
           DO IMODEL=1,NMODEL
             IF(RUNMOD(IMODEL).EQ.1) THEN
               CTIMES(ITOT,IMODEL,CURIYR)=0.0
               WTIMES(ITOT,IMODEL,CURIYR)=0.0
               DO CURITR=1,NKEEP
                 CTIMES(ITOT,IMODEL,CURIYR)=CTIMES(ITOT,IMODEL,CURIYR)+CTIMES(CURITR,IMODEL,CURIYR)
                 WTIMES(ITOT,IMODEL,CURIYR)=WTIMES(ITOT,IMODEL,CURIYR)+WTIMES(CURITR,IMODEL,CURIYR)
               ENDDO
               CTIMES(ITOT,IMODEL,CURIYR)=CTIMES(ITOT,IMODEL,CURIYR)+CTIMES(NTIMITR,IMODEL,CURIYR)
               WTIMES(ITOT,IMODEL,CURIYR)=WTIMES(ITOT,IMODEL,CURIYR)+WTIMES(NTIMITR,IMODEL,CURIYR)
               TOTTIME=TOTTIME+CTIMES(ITOT,IMODEL,CURIYR)
               TOTWALL=TOTWALL+WTIMES(ITOT,IMODEL,CURIYR)
               WRITE(6,'(1X,A5,A6,20F7.2)') '#*** ', &
                  SUBR_NAMES(IMODEL),(CTIMES(I,IMODEL,CURIYR),I=1,NKEEP), &
                  CTIMES(NTIMITR,IMODEL,CURIYR),CTIMES(ITOT,IMODEL,CURIYR)
               IF (WALLSW .EQ. 1) &
                 WRITE(6,'(1X,A5,A6,20F7.2)') '#*** ',SUBR_NAMES(IMODEL), &
                   (WTIMES(I,IMODEL,CURIYR),I=1,NKEEP), &
                    WTIMES(NTIMITR,IMODEL,CURIYR),WTIMES(ITOT,IMODEL,CURIYR)
             ENDIF
           ENDDO
           WRITE(6,'(A,I4,A,F9.1)') ' #*** Total CPU Seconds for ',CURIYR+BASEYR-1,' all modules:',TOTTIME
           IF (WALLSW .EQ. 1) &
           WRITE(6,'(A,I4,A,F9.1)') ' #*** Total Wall Seconds for ',CURIYR+BASEYR-1,' all modules:',TOTWALL
         ENDIF

         CURIYR=CURIYR+1
         CURCALYR=CURIYR + BASEYR - 1

      ENDDO                               ! End of Outer Year Loop

      IF(ITIMNG.EQ.1) THEN
        WRITE(6,'(a)') ' #*** CPU Time by Module'
        DO IMODEL=1,NMODEL
          IF(RUNMOD(IMODEL).EQ.1) THEN
            WRITE(6,'(a,a)') ' #*** ',SUBR_DESCR(IMODEL)
            WRITE(6,'(1X,A5,4X,9(2X,A4,I1),1X,A4,I2,2A7)') '#*** ',('Itr-',I,I=1,10),' Report','  Total'
            TOTTIME=0.
            DO IYR=FIRSYR,LASTYR
              CURIYR = IYR
              CURCALYR=CURIYR + BASEYR - 1
              WRITE(6,'(1X,A5,I4,11F7.2,F7.1)') '#*** ',CURIYR+BASEYR-1,(CTIMES(I,IMODEL,CURIYR),I=1,ITOT)
              TOTTIME=TOTTIME+CTIMES(ITOT,IMODEL,CURIYR)
            ENDDO
            WRITE(6,'(3A,F9.1)') ' #*** Total CPU Seconds for ',SUBR_NAMES(IMODEL),' all years:',TOTTIME
          ENDIF
        ENDDO
        IF (WALLSW .EQ. 1) THEN
          WRITE(6,'(a)') ' #*** Wall Clock Time by Module'
          DO IMODEL=1,NMODEL
            IF(RUNMOD(IMODEL).EQ.1) THEN
              WRITE(6,'(a,a)') ' #*** ',SUBR_DESCR(IMODEL)
              WRITE(6,'(1X,A5,4X,9(2X,A4,I1),1X,A4,I2,2A7)') '#*** ',('Itr-',I,I=1,10),' Report','  Total'
              TOTWALL=0.
              DO IYR=FIRSYR,LASTYR
                CURIYR = IYR
                CURCALYR=CURIYR + BASEYR - 1
                WRITE(6,'(1X,A5,I4,11F7.2,F7.1)') '#*** ', &
                CURIYR+BASEYR-1,(WTIMES(I,IMODEL,CURIYR),I=1,ITOT)
                TOTWALL=TOTWALL+WTIMES(ITOT,IMODEL,CURIYR)
              ENDDO
              WRITE(6,'(3A,F9.1)') ' #*** Total Wall Seconds for ',SUBR_NAMES(IMODEL),' all years:',TOTWALL
            ENDIF
          ENDDO
        ENDIF
      ENDIF

      MAXITR=MAXITRDF
      RETURN
      END
!****************************************************************
!*    Subroutine NSTOREP
!*
!*    Adjusts the current year prices by the amount of change in the previous year
!****************************************************************
      SUBROUTINE NSTOREP
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'cdsparms'
      include 'coalemm'
      include 'mpblk'
      include 'ngtdmout'
      include 'coalprc'
      include 'epmmpblk'
      include 'epmngtdm'
      include 'epmclprc'
      include 'ponroad'
      include 'epmonrd'

      INTEGER IR,IV
      REAL RSTRTP(MNUMCR,MNUMP),RSTRTUP(NNGEM,3)
      REAL RSTRTSEAS(NNGEM,2,2),RSTRTVEH(MNUMCR,2,2),RSTRTFRT(4,MNUMCR,2,2)
      REAL RSTRTNGSEAS(NNGEM,4)     !  first three are seasonal variable, 4 is annual
      REAL RSTRTONRD(MNUMCR),RSTRTCDR(2,NDRGG)
      REAL TEMP,TEMPRAILSHIP(4)

      DO IR=1,MNUMCR
        DO IV=1,MNUMP
          TEMP=MPRC(IR,CURIYR,IV)
          IF(CURIYR.GT.3) THEN
             MPRC(IR,CURIYR,IV)= MPRC(IR,CURIYR,IV)+MPRC(IR,CURIYR-1,IV)-RSTRTP(IR,IV)
            AMPRC(IR,CURIYR,IV)=AMPRC(IR,CURIYR,IV)+MPRC(IR,CURIYR-1,IV)-RSTRTP(IR,IV)
          ENDIF
          RSTRTP(IR,IV)=TEMP
        ENDDO
        TEMP=PDSTRHWY(IR,CURIYR)   ! squeeze on-road distillate in here
        IF(CURIYR.GT.3) THEN
          PDSTRHWY(IR,CURIYR)=PDSTRHWY(IR,CURIYR)+PDSTRHWY(IR,CURIYR-1)-RSTRTONRD(IR)
          ADSTRHWY(IR,CURIYR)=ADSTRHWY(IR,CURIYR)+PDSTRHWY(IR,CURIYR-1)-RSTRTONRD(IR)
        ENDIF
        RSTRTONRD(IR)=TEMP
      ENDDO

!     ADJUST natural GAS prices by EMM/NGMM REGIONS
      DO IR=1,NNGEM
        DO IV=1,3
          TEMP=MUPRC(IR,CURIYR,IV)
          IF(CURIYR.GT.3) THEN
             MUPRC(IR,CURIYR,IV)= MUPRC(IR,CURIYR,IV)+MUPRC(IR,CURIYR-1,IV)-RSTRTUP(IR,IV)
            AMUPRC(IR,CURIYR,IV)=AMUPRC(IR,CURIYR,IV)+MUPRC(IR,CURIYR-1,IV)-RSTRTUP(IR,IV)
          ENDIF
          RSTRTUP(IR,IV)=TEMP
        ENDDO
      ENDDO
! seasonal:
!   new variables have three seasons
      DO IR = 1,NNGEM
        do IV=1,3
          TEMP=SPNGELGR(IR,CURIYR,IV)
          IF(CURIYR.GT.3) THEN
            SPNGELGR(IR,CURIYR,IV)= SPNGELGR(IR,CURIYR,IV)+SPNGELGR(IR,CURIYR-1,IV)-RSTRTNGSEAS(IR,IV)
           ASPNGELGR(IR,CURIYR,IV)=ASPNGELGR(IR,CURIYR,IV)+SPNGELGR(IR,CURIYR-1,IV)-RSTRTNGSEAS(IR,IV)
          ENDIF
          RSTRTNGSEAS(IR,IV)=TEMP
        enddo
        TEMP=PNGELGR(IR,CURIYR)
        IF(CURIYR.GT.3) THEN
          PNGELGR(IR,CURIYR)= PNGELGR(IR,CURIYR)+PNGELGR(IR,CURIYR-1)-RSTRTNGSEAS(IR,4)
         APNGELGR(IR,CURIYR)=APNGELGR(IR,CURIYR)+PNGELGR(IR,CURIYR-1)-RSTRTNGSEAS(IR,4)
        ENDIF
        RSTRTNGSEAS(IR,4)=TEMP
      ENDDO
!  old variables represent firm and interruptible with two seasons
      DO IR = 1,NNGEM
        do IV=1,2
          TEMP=SPGFELGR(IR,CURIYR,IV)
          IF(CURIYR.GT.3) THEN
            SPGFELGR(IR,CURIYR,IV)= SPGFELGR(IR,CURIYR,IV)+SPGFELGR(IR,CURIYR-1,IV)-RSTRTSEAS(IR,IV,1)
           ASPGFELGR(IR,CURIYR,IV)=ASPGFELGR(IR,CURIYR,IV)+SPGFELGR(IR,CURIYR-1,IV)-RSTRTSEAS(IR,IV,1)
          ENDIF
          RSTRTSEAS(IR,IV,1)=TEMP
        enddo
      ENDDO
      DO IR = 1,NNGEM
        do IV=1,2
          TEMP=SPGIELGR(IR,CURIYR,IV)
          IF(CURIYR.GT.3) THEN
            SPGIELGR(IR,CURIYR,IV)= SPGIELGR(IR,CURIYR,IV)+SPGIELGR(IR,CURIYR-1,IV)-RSTRTSEAS(IR,IV,2)
           ASPGIELGR(IR,CURIYR,IV)=ASPGIELGR(IR,CURIYR,IV)+SPGIELGR(IR,CURIYR-1,IV)-RSTRTSEAS(IR,IV,2)
          ENDIF
          RSTRTSEAS(IR,IV,2)=TEMP
        enddo
      ENDDO

      DO IR=1,NDRGG
        DO IV=1,2
          TEMP=PCLELCDR(IV,IR,CURIYR)
          IF (CURIYR .GT. 3) THEN
            PCLELCDR(IV,IR,CURIYR) =  PCLELCDR(IV,IR,CURIYR)+PCLELCDR(IV,IR,CURIYR-1)-RSTRTCDR(IV,IR)
           APCLELCDR(IV,IR,CURIYR) = APCLELCDR(IV,IR,CURIYR)+PCLELCDR(IV,IR,CURIYR-1)-RSTRTCDR(IV,IR)
          ENDIF
          RSTRTCDR(IV,IR)=TEMP
        ENDDO
      ENDDO

! compressed natural gas fleet/personal vehicles
      DO IR = 1,MNUMCR
          TEMP=PGFTRFV(IR,CURIYR)
          IF(CURIYR.GT.3) THEN
            PGFTRFV(IR,CURIYR) = PGFTRFV(IR,CURIYR) + PGFTRFV(IR,CURIYR-1)-RSTRTVEH(IR,1,1)
            AGFTRFV(IR,CURIYR) = AGFTRFV(IR,CURIYR) + PGFTRFV(IR,CURIYR-1)-RSTRTVEH(IR,1,1)
          ENDIF
          RSTRTVEH(IR,1,1)=TEMP
      ENDDO
      DO IR = 1,MNUMCR
          TEMP=PGFTRPV(IR,CURIYR)
          IF(CURIYR.GT.3) THEN
            PGFTRPV(IR,CURIYR) = PGFTRPV(IR,CURIYR) + PGFTRPV(IR,CURIYR-1)-RSTRTVEH(IR,2,1)
            AGFTRPV(IR,CURIYR) = AGFTRPV(IR,CURIYR) + PGFTRPV(IR,CURIYR-1)-RSTRTVEH(IR,2,1)
          ENDIF
          RSTRTVEH(IR,2,1)=TEMP
      ENDDO

! liquefied natural gas fleet/personal vehicles
      DO IR = 1,MNUMCR
          TEMP=PGLTRFV(IR,CURIYR)
          IF(CURIYR.GT.3) THEN
            PGLTRFV(IR,CURIYR) = PGLTRFV(IR,CURIYR) + PGLTRFV(IR,CURIYR-1)-RSTRTVEH(IR,1,2)
            AGLTRFV(IR,CURIYR) = AGLTRFV(IR,CURIYR) + PGLTRFV(IR,CURIYR-1)-RSTRTVEH(IR,1,2)
          ENDIF
          RSTRTVEH(IR,1,2)=TEMP
      ENDDO
      DO IR = 1,MNUMCR
          TEMP=PGLTRPV(IR,CURIYR)
          IF(CURIYR.GT.3) THEN
            PGLTRPV(IR,CURIYR) = PGLTRPV(IR,CURIYR) + PGLTRPV(IR,CURIYR-1)-RSTRTVEH(IR,2,2)
            AGLTRPV(IR,CURIYR) = AGLTRPV(IR,CURIYR) + PGLTRPV(IR,CURIYR-1)-RSTRTVEH(IR,2,2)
          ENDIF
          RSTRTVEH(IR,2,2)=TEMP
      ENDDO
! compressed natural gas, rail and shipping
      DO IR = 1,MNUMCR
          TEMPRAILSHIP(1:4)=PGFTRRAIL(1:4,IR,CURIYR)
          IF(CURIYR.GT.3) THEN
            PGFTRRAIL(1:4,IR,CURIYR) = PGFTRRAIL(1:4,IR,CURIYR) + PGFTRRAIL(1:4,IR,CURIYR-1)-RSTRTFRT(1:4,IR,1,1)
            AGFTRRAIL(1:4,IR,CURIYR) = AGFTRRAIL(1:4,IR,CURIYR) + PGFTRRAIL(1:4,IR,CURIYR-1)-RSTRTFRT(1:4,IR,1,1)
          ENDIF
          RSTRTFRT(1:4,IR,1,1)=TEMPRAILSHIP(1:4)
      ENDDO
      DO IR = 1,MNUMCR
          TEMPRAILSHIP(1:3)=PGFTRSHIP(1:3,IR,CURIYR)
          IF(CURIYR.GT.3) THEN
            PGFTRSHIP(1:3,IR,CURIYR) = PGFTRSHIP(1:3,IR,CURIYR) + PGFTRSHIP(1:3,IR,CURIYR-1)-RSTRTFRT(1:3,IR,2,1)
            AGFTRSHIP(1:3,IR,CURIYR) = AGFTRSHIP(1:3,IR,CURIYR) + PGFTRSHIP(1:3,IR,CURIYR-1)-RSTRTFRT(1:3,IR,2,1)
          ENDIF
          RSTRTFRT(1:3,IR,2,1)=TEMPRAILSHIP(1:3)
      ENDDO

! liquefied natural gas, rail and shipping
      DO IR = 1,MNUMCR
          TEMPRAILSHIP(1:4)=PGLTRRAIL(1:4,IR,CURIYR)
          IF(CURIYR.GT.3) THEN
            PGLTRRAIL(1:4,IR,CURIYR) = PGLTRRAIL(1:4,IR,CURIYR) + PGLTRRAIL(1:4,IR,CURIYR-1)-RSTRTFRT(1:4,IR,1,2)
            AGLTRRAIL(1:4,IR,CURIYR) = AGLTRRAIL(1:4,IR,CURIYR) + PGLTRRAIL(1:4,IR,CURIYR-1)-RSTRTFRT(1:4,IR,1,2)
          ENDIF
          RSTRTFRT(1:4,IR,1,2)=TEMPRAILSHIP(1:4)
      ENDDO
      DO IR = 1,MNUMCR
          TEMPRAILSHIP(1:3)=PGLTRSHIP(1:3,IR,CURIYR)
          IF(CURIYR.GT.3) THEN
            PGLTRSHIP(1:3,IR,CURIYR) = PGLTRSHIP(1:3,IR,CURIYR) + PGLTRSHIP(1:3,IR,CURIYR-1)-RSTRTFRT(1:3,IR,2,2)
            AGLTRSHIP(1:3,IR,CURIYR) = AGLTRSHIP(1:3,IR,CURIYR) + PGLTRSHIP(1:3,IR,CURIYR-1)-RSTRTFRT(1:3,IR,2,2)
          ENDIF
          RSTRTFRT(1:3,IR,2,2)=TEMPRAILSHIP(1:3)
      ENDDO

      RETURN
      END SUBROUTINE NSTOREP
!****************************************************************
!*    Subroutine NEXEC(IMODEL,NRETCD)
!*
!*    Executes one of the NEMS submodels for the current year
!****************************************************************

      SUBROUTINE NEXEC(IMODEL,NRETCD)
  use ifport, only : timef,sleepqq
  use ifcore, only : commitqq
      use CALLING_AIMMS
      use scedes_keys

      IMPLICIT NONE

      include 'emmparm'
      include 'macout'
      include 'qblk'
      include 'bifurc' ! for qe2xxIN variables
      include 'indrep' ! for REFCON variable
      include 'wrenew' ! qeleth, qngeth, qcleth
      include 'cogen'  ! to fill new 10-fuel refinery CHaP variables
      include 'convfact' ! for debugging
      include 'pmmout'
      include 'lfmmout'
      include 'uso2grp'   !  this is temporary to fill in the new, year-dimensioned variables from the old, non-year ones

      real trefcon(18,11)
      INTEGER IMODEL,NRETCD,CPU_TIME_BEGIN,CPU_TIME_END,ir,IVAR
      INTEGER EXBUILD/-1/,EXCOAL/-1/
      integer AIMMSNG/-1/   ! run-time option to invoke AIMMS NGMM (if 1)

      INTEGER NTIMITR
      PARAMETER (NTIMITR=11)
      REAL CTIMES(NTIMITR+1,NMODEL,MNUMYR)
      REAL WTIMES(NTIMITR+1,NMODEL,MNUMYR)
      COMMON/MAINTIME/CTIMES,WTIMES
      INTEGER WALL_TIME_BEGIN, WALL_TIME_END
      INTEGER WALLSW
      COMMON /MNWALLSW/ WALLSW

      character*50 MPS
      real sELeth(9),sNGeth(9),sCLeth(9)  ! backup regional shares for missing regional ethanol quantities
      integer FYearSubset_Coal
      integer eunit, emmbench ! to pass moreopt parameter EMMBENCH to the coal module via a text data file.
!  The following are to pass MOREOPT run-time options to AIMMS Coal using eunit
      character(80) CLUSEXPORTICMMN,ICMMCOMMODITYMAPN,CLOCEANDISTN,CLAGGEXPORTMAPN,CLEXPORTLIMITSN,CLINTLDEMANDN,CLINTLQUALITYN,CLINTLSUPPLYN,CLDISTANCEN,CLFEASIBLEROUTN,CLFREIGHTN,CLRATESELECN,CLRATESNONELECN,CLTONRAILMILEN,CLTONSPERCARN,CLINTLUSEXPORTN,CLFLAGSN
      character(80) CLIMPORTLIMITSN

!  The following are to pass MOREOPT run-time options to AIMMS NGMM using eunit
      INTEGER NGA_STEOBM, NGA_STEONG, NGA_STSCALNG, NGA_OGTECH, NGA_OGTECADJ, NGA_KEEPOPEN
      character(80) NGASSUMPTIONSN, NGMARKUPSN, NGLNGEXPN, NGMEXICON, NGSPOTPRCN, NGCAPACITYN, NGCANADAN, NGEIAN, NGSETMAPN, NGVARTARN, NGSTEOFACTINN, NGTEXASN, NGTXCAPAN
      INTEGER NGA_LNGGAMLV, NGA_LNGGAMCN

      INTEGER DIVS(5,2)/1,3,5,8,11, &  ! Census Division start for each Region and US
                        2,4,7,9,11/    ! Census Division end   for each Region and US

      integer Year_Slice_Start, Year_Slice_End
      integer Year_Slice_Start_Coal

!  map_refreg_to_cenreg maps refinery regions  to Census divisions through coal regions via CTL_CDSL1
!  array in clshare.txt (search for "PADD Region"; lines are coal regions, columns are refinery regions)
!  then to Census divisions using code that had been in coalcds.f (and is now gone):
!      IF (K .EQ. 1)J = 1
!      IF (K .EQ. 2)J= 2                      !J IS CENSUS REGION, K IS COAL DEMAND REGION
!      IF (K .EQ. 3 .OR. K .EQ. 4 .OR. K .EQ. 5)J= 5
!      IF (K .EQ. 6 .OR. K .EQ. 7)J = 3
!      IF (K .EQ. 8 .OR. K .EQ. 9)J = 6
!      IF (K .EQ. 10 .OR. K .EQ. 11)J= 4
!      IF (K .EQ. 12) J = 7
!      IF (K .EQ. 13 .OR. K .EQ. 14 .OR. K .EQ. 15)J = 8
!      IF (K .EQ. 16)J = 9
!  this translates to the following refinery region to Census division map:
      integer map_refreg_to_cenreg(refreg)     !  final refreg is Caribbean/Canada, but just in case...
      data map_refreg_to_cenreg / 2, 7, 3, 7, 7, 8, 9, 9, 9/
      integer retry ! counts number of retries after trapping error from AIMMS
! +++ Set Return Code to False--CURRENTLY UNUSED
      Year_Slice_Start=curiyr
      Year_Slice_End=curiyr
      if(curitr.eq.1) year_slice_start=curiyr-1  ! on 1st iteration, pass lagged values that can change with models invoked later in execution sequence

! get run time switches for external buildings execution (exbuild), external aimms-coal execution (excoal)
      if(exbuild.lt.0) exbuild=rtovalue('EXBUILD ',0)
      if(excoal.lt.0) excoal=rtovalue('EXCOAL  ',0)
      if(AIMMSNG.lt.0) AIMMSNG=rtovalue('AIMMSNG ',0)

      NRETCD = 0
      if(CURIYR.eq.LASTYR .and. FCRL.eq.1) FYearSubset=0  ! last time through, write/read all years
      IF (ITIMNG.EQ.1) THEN
         IF (WALLSW .EQ. 1) THEN
           CALL MPTIM3(CPU_TIME_BEGIN,WALL_TIME_BEGIN)
         ELSE
          CALL MPTIM2(CPU_TIME_BEGIN)
         ENDIF
      ENDIF
   60 FORMAT(10X,A,A6,', CPU Time (Seconds)=',F9.2,A,F7.2)
      call job(1) ! call job status and control routine

      IF (IMODEL.EQ.1) THEN
         CALL WORLD
      ELSEIF (IMODEL.EQ.2) THEN
         CALL MAC
      ELSEIF (IMODEL.EQ.3) THEN
        if(exbuild.eq.1) then      ! invoke resd and comm externally

          ! Invoke both RESD and COMM, if both are turned on, to reduce communications overhead
          !  of running them separately.  But if commerical turned off, just run RESD
          if(EXK.eq.0) then     ! If COMM off, just invoke RESD.

            if(CurCalYr.ge.2005) then !RESD first year is 2005, matches recs

             FYearSubset=0 ! for now
             call M_Launch('RESD  ','DEMPUTVARS','DEMGETVARS',FYearSubset,Year_Slice_Start,Year_Slice_End,iret)

             ! first time through FYearSubset is 0 so data for all years written to first transfer file.  Then transfer year subsets.
             !FYearSubset=1
             if(iret.eq.1) then
               write(6,*) 'ERROR running RESD.  Turning it off'
               runmod(3)=0
               EXR=0
             endif

            endif

          else  ! invoke both in one request

            if(CurCalYr.ge.2004) Then ! Comm's first year is CBECSYEAR+1 (2004)

             FYearSubset=0 ! for now
             call M_Launch('BUILD ','DEMPUTVARS','DEMGETVARS',FYearSubset,Year_Slice_Start,Year_Slice_End,iret)

             ! first time through FYearSubset is 0 so data for all years written to first transfer file.  Then transfer year subsets.
            ! FYearSubset=1

             if(iret.eq.1) then
               write(6,*) 'ERROR running RESD and COMM.  Turning them off'
               EXR=0
               EXK=0
               runmod(3)=0
               runmod(4)=0
             endif

            endif

          endif
        else   ! invoke resd internally
          CALL RESD
        endif
      ELSEIF (IMODEL.EQ.4) THEN
        if(exbuild.eq.1) then  ! invoke comm externally

          if(CurCalYr.ge.2004) then ! Comm's first year is CBECSYEAR+1 (2004)

          !invoke COMM if it wasn't called with RESD above (i.e., only when resd off, comm on)
            if(EXR.eq.0) then

              FYearSubset=0 ! for now
              call M_Launch('COMM  ','DEMPUTVARS','DEMGETVARS',FYearSubset,Year_Slice_Start,Year_Slice_End,iret)

! first time through FYearSubset is 0 so data for all years written to first transfer file.
             ! FYearSubset=1

              if(iret.eq.1) then
                write(6,*) 'ERROR running COMM.  Turning it off'
                EXK=0
                runmod(4)=0
              endif
            endif
          endif
        else ! invoke comm internally
          CALL COMM
        endif
      ELSEIF (IMODEL.EQ.5) THEN
         CALL IND
      ELSEIF (IMODEL.EQ.6) THEN
         CALL TRAN
      ELSEIF (IMODEL.EQ.7) THEN
         if (.not. opened_yet(restore)) then
           opened_yet(restore)=.true.
           call unitunopened(100,999,monitordbg(restore))
           open(monitordbg(restore),file='rest\monitor.debug.txt',status='unknown')
           rewind monitordbg(restore)
         endif
         CALL UTIL
      ELSEIF (IMODEL.EQ.8) THEN

        if (excoal .eq. 0) then
          CALL COAL
        else                                            ! Invoke external AIMMS Coal module
          if (CurCalYr .ge. CLBASEYR) then ! first coal year
             if (.not. opened_yet(1)) then
               opened_yet(1)=.true.
               call unitunopened(100,999,monitordbg(1))
               open(monitordbg(1),file='coal\monitor.debug.txt',status='unknown')
               rewind monitordbg(1)
             endif
            FYearStart=curiyr
            FYearEnd=curiyr
            FYearSubset=1

            if (CurCalYr .eq. CLBASEYR) FYearSubset=0
            iret=0

            EMMBENCH=rtovalue('EMMBENCH',1)

            ! now open emmbench.txt and write the content in
            call unitunopened(100,999,EUNIT)
            open(eunit,file='coal\emmbench.txt',status='unknown')
            rewind eunit
            write(eunit,'(a,i1,a)') 'EMMBENCH := ',emmbench,';'

            close (eunit)

!  Retrieve and pass run-time options to AIMMS via text file cl_runval.txt
            ! read in coal AIMMS input files path (defined in scedes file) and assgin the values to the corresponding variables, to output to cl_runval.txt
            call RTOSTRING('CLUSEXPO',CLUSEXPORTICMMN)
            call RTOSTRING('ICMMCOMM',ICMMCOMMODITYMAPN)
            call RTOSTRING('CLOCEAND',CLOCEANDISTN)
            call RTOSTRING('CLAGGEXP',CLAGGEXPORTMAPN)
            call RTOSTRING('CLEXPORT',CLEXPORTLIMITSN)
            call RTOSTRING('CLIMPORT',CLIMPORTLIMITSN)
            call RTOSTRING('CLINTLDE',CLINTLDEMANDN)
            call RTOSTRING('CLINTLQU',CLINTLQUALITYN)
            call RTOSTRING('CLINTLSU',CLINTLSUPPLYN)
            call RTOSTRING('CLDISTAN',CLDISTANCEN)
            call RTOSTRING('CLFEASIB',CLFEASIBLEROUTN)
            call RTOSTRING('CLFREIGH',CLFREIGHTN)
            call RTOSTRING('CLRATESE',CLRATESELECN)
            call RTOSTRING('CLRATESN',CLRATESNONELECN)
            call RTOSTRING('CLTONRAI',CLTONRAILMILEN)
            call RTOSTRING('CLTONSPE',CLTONSPERCARN)
            call RTOSTRING('CLINTLUS',CLINTLUSEXPORTN)
            call RTOSTRING('CLFLAGSN',CLFLAGSN)
            
            ! for debugging - jognems output in nohup.out
            !WRITE(*,*)CLUSEXPORTICMMN
            
            ! now open cl_runval.txt and write the content in
            call unitunopened(100,999,EUNIT)
            open(eunit,file='coal\cl_runval.txt',status='unknown')
            rewind eunit
            
            ! write out paths about Coal AIMMS input files
            write(eunit,'(a,a,a)') 'CLUSEXPORTICMMN := "',trim(CLUSEXPORTICMMN),'";'
            write(eunit,'(a,a,a)') 'ICMMCOMMODITYMAPN := "',trim(ICMMCOMMODITYMAPN),'";'
            write(eunit,'(a,a,a)') 'CLOCEANDISTN := "',trim(CLOCEANDISTN),'";'
            write(eunit,'(a,a,a)') 'CLAGGEXPORTMAPN := "',trim(CLAGGEXPORTMAPN),'";'
            write(eunit,'(a,a,a)') 'CLEXPORTLIMITSN := "',trim(CLEXPORTLIMITSN),'";'
            write(eunit,'(a,a,a)') 'CLIMPORTLIMITSN := "',trim(CLIMPORTLIMITSN),'";'
            write(eunit,'(a,a,a)') 'CLINTLDEMANDN := "',trim(CLINTLDEMANDN),'";'
            write(eunit,'(a,a,a)') 'CLINTLQUALITYN := "',trim(CLINTLQUALITYN),'";'
            write(eunit,'(a,a,a)') 'CLINTLSUPPLYN := "',trim(CLINTLSUPPLYN),'";'
            write(eunit,'(a,a,a)') 'CLDISTANCEN := "',trim(CLDISTANCEN),'";'
            write(eunit,'(a,a,a)') 'CLFEASIBLEROUTN := "',trim(CLFEASIBLEROUTN),'";'
            write(eunit,'(a,a,a)') 'CLFREIGHTN := "',trim(CLFREIGHTN),'";'
            write(eunit,'(a,a,a)') 'CLRATESELECN := "',trim(CLRATESELECN),'";'
            write(eunit,'(a,a,a)') 'CLRATESNONELECN := "',trim(CLRATESNONELECN),'";'
            write(eunit,'(a,a,a)') 'CLTONRAILMILEN := "',trim(CLTONRAILMILEN),'";'
            write(eunit,'(a,a,a)') 'CLTONSPERCARN := "',trim(CLTONSPERCARN),'";'
            write(eunit,'(a,a,a)') 'CLINTLUSEXPORTN := "',trim(CLINTLUSEXPORTN),'";'
            write(eunit,'(a,a,a)') 'CLFLAGSN := "',trim(CLFLAGSN),'";'
            
            close (eunit)
            
            call AIMMS_Coal(iret)
            FYearsubset=0
            if(iret.ne.0) then
              write(6,*) 'ERROR running AIMMS Coal.  Stopping now to avoid confusion.'
              CALL NDATOT('   ')
              CALL NDATOT('GDX') ! call with argument to write GDX format restart file also
              write(6,*) 'ERROR return from AIMMS Coal is ',iret
              stop 999
            endif

! fill ecp's coal expectations horizon for current year using prior run values, or perfect foresight
            call coal_expect
           endif

          endif
      ELSEIF (IMODEL.EQ.9) THEN
         CALL WELL
      ELSEIF (IMODEL.EQ.10) THEN
!         if(AIMMSNG.eq.0) then ! fortran version, in ngtdm.f
!           CALL NGMAIN

!         else                                     ! invoke external AIMMS natural gas module, in ./ngas/
           if (CurCalYr .ge. 2016) then           ! first model year
             if (.not. opened_yet(2)) then
               opened_yet(2)=.true.
               call unitunopened(100,999,monitordbg(2))
               open(monitordbg(2),file='ngas\monitor.debug.txt',status='unknown')
               rewind monitordbg(2)
             endif
             FYearStart=curiyr
             FYearEnd=curiyr
             FYearSubset=1                 ! send year subset as specified by FYearStart and FYearEnd
             FYearSubset=0                 ! send all years each time
             if (CurCalYr .le. 2016 .and. curitr .eq. 1) FYearSubset=0

!  Retrieve and pass run-time options to AIMMS via text file
             NGA_STEOBM   = RTOVALUE('STEOBM  ',0)
             NGA_STEONG   = RTOVALUE('STEONG  ',0)
             NGA_STSCALNG = RTOVALUE('STSCALNG',0)
             NGA_OGTECH   = RTOVALUE('OGTECH  ',0)
             NGA_OGTECADJ = RTOVALUE('OGTECADJ',0)
             NGA_LNGGAMLV = RTOVALUE('LNGGAMLV',450)
             NGA_LNGGAMCN = RTOVALUE('LNGGAMCN',-425)
             NGA_KEEPOPEN = RTOVALUE('KEEPOPEN',0)
             IF (NGA_KEEPOPEN .EQ. 0) NGA_KEEPOPEN = RTOVALUE('KEEPNG  ',0)
             call unitunopened(100,999,EUNIT)
             
             ! read in ngmm AIMMS input files path and assgin the values to the corresponding variables, to output to ng_runval.txt
             ! for debugging - jognems output in nohup.out
             !WRITE(*,*)NGASSUMPTIONSN
             call RTOSTRING('NGASSUMP',NGASSUMPTIONSN)  !  e.g., '$NEMS/models/ngas/input/ngassumptions.txt      '
             call RTOSTRING('NGMARKUP',NGMARKUPSN)
             call RTOSTRING('NGLNGEXP',NGLNGEXPN)
             call RTOSTRING('NGMEXICO',NGMEXICON)
             call RTOSTRING('NGSPOTPR',NGSPOTPRCN)
             call RTOSTRING('NGCAPACI',NGCAPACITYN)
             call RTOSTRING('NGCANADA',NGCANADAN)
             call RTOSTRING('NGEIAN  ',NGEIAN)
             call RTOSTRING('NGSETMAP',NGSETMAPN)
             call RTOSTRING('NGVARTAR',NGVARTARN)
             call RTOSTRING('NGSTEOFA',NGSTEOFACTINN)
             call RTOSTRING('NGTEXAS ',NGTEXASN)
             call RTOSTRING('NGTXCAPA',NGTXCAPAN)

             open(eunit,file='ngas\ng_runval.txt',status='unknown')
             rewind eunit
             write(eunit,'(a,i2,a)') 'STEOBM   := ',nga_steobm,';'
             write(eunit,'(a,i2,a)') 'STEONG   := ',nga_steong,';'
             write(eunit,'(a,i2,a)') 'STSCALNG := ',nga_stscalng,';'
             write(eunit,'(a,i2,a)') 'OGTECH   := ',nga_ogtech,';'
             write(eunit,'(a,i2,a)') 'OGTECADJ := ',nga_ogtecadj,';'
             write(eunit,'(a,i2,a)') 'KEEPOPEN := ',nga_keepopen,';'
             write(eunit,'(a,F6.1,a)') 'x_LNG_Gamma2 := ',float(nga_lnggamlv)/10.,';'
             write(eunit,'(a,F6.1,a)') 'x_LNG_Gamma1 := ',float(nga_lnggamcn)/10.,';'

             ! write out paths about ngmm AIMMS input files
             write(eunit,'(a,a,a)') 'NGASSUMPTIONSN := "',trim(NGASSUMPTIONSN),'";'
             write(eunit,'(a,a,a)') 'NGMARKUPSN     := "',trim(NGMARKUPSN),'";'
             write(eunit,'(a,a,a)') 'NGLNGEXPN      := "',trim(NGLNGEXPN),'";'
             write(eunit,'(a,a,a)') 'NGMEXICON      := "',trim(NGMEXICON),'";'
             write(eunit,'(a,a,a)') 'NGSPOTPRCN     := "',trim(NGSPOTPRCN),'";'
             write(eunit,'(a,a,a)') 'NGCAPACITYN    := "',trim(NGCAPACITYN),'";'
             write(eunit,'(a,a,a)') 'NGCANADAN      := "',trim(NGCANADAN),'";'
             write(eunit,'(a,a,a)') 'NGEIAN         := "',trim(NGEIAN),'";'
             write(eunit,'(a,a,a)') 'NGSETMAPN      := "',trim(NGSETMAPN),'";'
             write(eunit,'(a,a,a)') 'NGVARTARN      := "',trim(NGVARTARN),'";'
             write(eunit,'(a,a,a)') 'NGSTEOFACTINN  := "',trim(NGSTEOFACTINN),'";'
             write(eunit,'(a,a,a)') 'NGTEXASN       := "',trim(NGTEXASN),'";'
             write(eunit,'(a,a,a)') 'NGTXCAPAN      := "',trim(NGTXCAPAN),'";'
             
             close(eunit)

             retry=0
             call AIMMS_NG(iret)
             FYearsubset=0
             if(iret.ne.0) then
               write(6,*) 'ERROR running AIMMS natural gas module.  Stopping now to avoid confusion.'
               CALL NDATOT('   ')
               CALL NDATOT('GDX') ! call with argument to write GDX format restart file also
               write(6,*) 'ERROR return from AIMMS natural gas module is ',iret
               stop 999
             endif
           endif
!         endif
      ELSEIF (IMODEL.EQ.11) THEN
! store current refinery consumption so the QxxIN variables, which include QxxRF, can be reassigned after refinery model runs.

         do ir=1,11
           TREFCON(1,ir)= QELRF(ir,curiyr)+qELeth(curiyr,ir) ! electricity
           TREFCON(2,ir)= QNGRF(ir,curiyr)+qNGeth(curiyr,ir)+qGTLRF(ir,curiyr) ! nat gas h&p&GTL losses
           TREFCON(3,ir)= QCLRF(ir,curiyr)+qCLeth(curiyr,ir) ! +qCTLRF(ir,curiyr) ! steam coal and CTL losses
           TREFCON(6,ir)= QRLRF(ir,curiyr) ! residual
           TREFCON(7,ir)= QDSRF(ir,curiyr) ! distillate
           TREFCON(8,ir)= QLGRF(ir,curiyr) ! lpg h&p
           TREFCON(10,ir)=QSGRF(ir,curiyr) ! still gas
           TREFCON(11,ir)=QPCRF(ir,curiyr)+qCCRF(ir,curiyr) ! petroleum coke
           TREFCON(15,ir)=QOTRF(ir,curiyr) ! other petroleum
           TREFCON(18,ir)=QBMRF(ir,curiyr) ! biomass
           QGFIN(ir,curiyr) = QGFIN(ir,curiyr) - QGTLRF(ir,curiyr)
         enddo

!        QCLRFPD(:,CURIYR) = 0.0
!        QNGRFPD(:,CURIYR) = 0.0

         CALL REFINE

        sELeth(1:9)=(/0.,0.,0.31,0.69,0.,0.,0.,0.,0./)  !  backup regional shares
        sNGeth(1:9)=(/0.,0.,0.30,0.70,0.,0.,0.,0.,0./)
        sCLeth(1:9)=(/0.,0.,0.28,0.72,0.,0.,0.,0.,0./)

        if(sum(qNGeth(curiyr,1:9)).eq.0.0) qNGeth(curiyr,1:9)=qNGeth(curiyr,11)*sNGeth(1:9)
        if(sum(qELeth(curiyr,1:9)).eq.0.0) qELeth(curiyr,1:9)=qELeth(curiyr,11)*sELeth(1:9)
        if(sum(qCLeth(curiyr,1:9)).eq.0.0) qCLeth(curiyr,1:9)=qCLeth(curiyr,11)*sCLeth(1:9)

! assign new national totals
         DO IVAR=1,MNUMQ
            CALL SUMARY(MQTY(1,CURIYR,IVAR),MNUMCR)
         ENDDO

! Fischer-Tropsch conversion factors, this code is also in DOCVFACTS
!  GTL:
      IF (sum(GTLFRAC(1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFGTLLIQ(CURIYR) =(GTLFRAC(1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                            GTLFRAC(2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                            GTLFRAC(3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                            GTLFRAC(4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                        sum(GTLFRAC(1:4,MNUMPR,CURIYR))
      ELSE
         CFGTLLIQ(CURIYR) = CFDSQ
      ENDIF
!  CTL:
      IF (sum(CTLFRAC(1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFCTLLIQ(CURIYR) =(CTLFRAC(1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                            CTLFRAC(2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                            CTLFRAC(3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                            CTLFRAC(4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                        sum(CTLFRAC(1:4,MNUMPR,CURIYR))
      ELSE
         CFCTLLIQ(CURIYR) = CFDSQ
      ENDIF
!  BTL:
      IF (sum(BTLFRAC(1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFBTLLIQ(CURIYR) =(BTLFRAC(1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                            BTLFRAC(2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                            BTLFRAC(3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                            BTLFRAC(4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                        sum(BTLFRAC(1:4,MNUMPR,CURIYR))
      ELSE
         CFBTLLIQ(CURIYR) = CFDSQ
      ENDIF
!  CBTL:
      IF (sum(CBTLFRAC(1,1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFCBTLLIQ(1,CURIYR) =(CBTLFRAC(1,1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                               CBTLFRAC(1,2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                               CBTLFRAC(1,3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                               CBTLFRAC(1,4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                           sum(CBTLFRAC(1,1:4,MNUMPR,CURIYR))
      ELSE
         CFCBTLLIQ(1,CURIYR) = CFDSQ
      ENDIF
      IF (sum(CBTLFRAC(2,1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFCBTLLIQ(2,CURIYR) =(CBTLFRAC(2,1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                               CBTLFRAC(2,2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                               CBTLFRAC(2,3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                               CBTLFRAC(2,4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                           sum(CBTLFRAC(2,1:4,MNUMPR,CURIYR))
      ELSE
         CFCBTLLIQ(2,CURIYR) = CFDSQ
      ENDIF
      IF ((sum(CBTLFRAC(1,:,MNUMPR,CURIYR)) + sum(CBTLFRAC(2,:,MNUMPR,CURIYR))) .NE. 0.0) THEN
          CFCBTLLIQ(3,CURIYR) =(sum(CBTLFRAC(1,:,MNUMPR,CURIYR)) * CFCBTLLIQ(1,CURIYR) + &
                                sum(CBTLFRAC(2,:,MNUMPR,CURIYR)) * CFCBTLLIQ(2,CURIYR)) / &
                               (sum(CBTLFRAC(1,:,MNUMPR,CURIYR)) + sum(CBTLFRAC(2,:,MNUMPR,CURIYR)))
      ELSE
         CFCBTLLIQ(3,CURIYR) = CFDSQ
      ENDIF
! end Fischer-Tropsch conversion factors

         QCTLRF(:,CURIYR) = 0.0
         QGTLRF(:,CURIYR) = 0.0
         QGTLSN(:,CURIYR) = 0.0
!  skip over region 10, as I worry about affecting AB32 code:
         QCLSN(1:9,CURIYR) = 0.0
         QCLSN(MNUMCR,CURIYR) = 0.0
!  calculate non-liquids portion of CTL, GTL feedstock
         DO IR=1,REFREG-1              ! Region REFREG is Caribbean/Canada
            QCLSN(map_refreg_to_cenreg(IR),CURIYR) = QCLSN(map_refreg_to_cenreg(IR),CURIYR) + QCLRFPD(IR,CURIYR)
            QCTLRF(map_refreg_to_cenreg(IR),CURIYR) = QCTLRF(map_refreg_to_cenreg(IR),CURIYR) + QCLRFPD(IR,CURIYR) - &
              (sum(CTLFRAC(1:4,IR,CURIYR)) * CFCTLLIQ(CURIYR) + sum(CBTLFRAC(1,1:4,IR,CURIYR)) * CFCBTLLIQ(1,CURIYR)) * 365. / 1000.
            QGTLSN(map_refreg_to_cenreg(IR),CURIYR) = QGTLSN(map_refreg_to_cenreg(IR),CURIYR) + QNGRFPD(IR,CURIYR)
            QGTLRF(map_refreg_to_cenreg(IR),CURIYR) = QGTLRF(map_refreg_to_cenreg(IR),CURIYR) + QNGRFPD(IR,CURIYR) - &
              (sum(GTLFRAC(1:4,IR,CURIYR)) * CFGTLLIQ(CURIYR)) * 365. / 1000.
!write(6,'(I4,":  mapped from REFREG ",I2," to CenDiv ",I2," this amount:  heat and power=",F12.6,"; liquid=",F12.6,"; total=",F12.6,";  running total=",F12.6)') &
!curcalyr,IR,map_refreg_to_cenreg(IR), &
!QGTLRF(map_refreg_to_cenreg(IR),CURIYR), &
!(sum(GTLFRAC(1:4,IR,CURIYR)) * CFGTLLIQ(CURIYR)) * 365. / 1000., &
!QNGRFPD(IR,CURIYR), &
!QGTLSN(map_refreg_to_cenreg(IR),curiyr)
         ENDDO

         QCTLRF(MNUMCR,CURIYR) = sum(QCTLRF(1:MNUMCR-2,CURIYR))
         QGTLRF(MNUMCR,CURIYR) = sum(QGTLRF(1:MNUMCR-2,CURIYR))
         QGTLSN(MNUMCR,CURIYR) = sum(QGTLSN(1:MNUMCR-2,CURIYR))
         QCLSN(MNUMCR,CURIYR) = sum(QCLSN(1:MNUMCR-2,CURIYR))

! 1) Subtract what was previously added to Q*IN from Last iteration
! 2) Add back in current values for Q*RF
! 3) Reset reporting variables
         DO IR=1,11
           IF (IR .EQ. 10) CYCLE
           QCLRF(IR,CURIYR) = QCTLRF(IR,CURIYR)

!     1)
           QELIN(IR,curiyr)=QELIN(IR,curiyr)-TREFCON(1,IR)
           QNGIN(IR,curiyr)=QNGIN(IR,curiyr)-TREFCON(2,IR)
           QCLIN(IR,curiyr)=QCLIN(IR,curiyr)-TREFCON(3,IR)
           QRLIN(IR,curiyr)=QRLIN(IR,curiyr)-TREFCON(6,IR)  ! low sulfur reside
           QRSIN(IR,curiyr)=QRSIN(IR,curiyr)-TREFCON(6,IR)  ! total resid
           QDSIN(IR,curiyr)=QDSIN(IR,curiyr)-TREFCON(7,IR)
           QLGIN(IR,curiyr)=QLGIN(IR,curiyr)-TREFCON(8,IR)
           QSGIN(IR,curiyr)=QSGIN(IR,curiyr)-TREFCON(10,IR)
           QPCIN(IR,curiyr)=QPCIN(IR,curiyr)-TREFCON(11,IR)
           QOTIN(IR,curiyr)=QOTIN(IR,curiyr)-TREFCON(15,IR)
           QBMIN(IR,curiyr)=QBMIN(IR,curiyr)-TREFCON(18,IR)
!     2)
           QELIN(IR,curiyr)=QELIN(IR,curiyr)+QELRF(IR,curiyr)+qELeth(curiyr,ir)
           QGFIN(ir,curiyr) = QGFIN(ir,curiyr) + QGTLRF(ir,curiyr)
           QNGIN(IR,curiyr)=QNGIN(IR,curiyr)+QNGRF(IR,curiyr)+qNGeth(curiyr,ir)+QGTLRF(IR,curiyr)
           QGIIN(ir,curiyr)=QNGIN(IR,curiyr)-QGFIN(ir,curiyr)                    ! so they add up
           QCLIN(IR,curiyr)=QCLIN(IR,curiyr)+QCLRF(IR,curiyr)+qCLeth(curiyr,ir) ! +QCTLRF(IR,curiyr)
           QRLIN(IR,curiyr)=QRLIN(IR,curiyr)+QRLRF(IR,curiyr)
           QRSIN(IR,curiyr)=QRSIN(IR,curiyr)+QRSRF(IR,curiyr)
           QDSIN(IR,curiyr)=QDSIN(IR,curiyr)+QDSRF(IR,curiyr)
           QLGIN(IR,curiyr)=QLGIN(IR,curiyr)+QLGRF(IR,curiyr)
           QSGIN(IR,curiyr)=QSGIN(IR,curiyr)+QSGRF(IR,curiyr)
           QPCIN(IR,curiyr)=QPCIN(IR,curiyr)+QPCRF(IR,curiyr)  +qCCRF(IR,curiyr)
           QOTIN(IR,curiyr)=QOTIN(IR,curiyr)+QOTRF(IR,curiyr)
           QBMIN(IR,curiyr)=QBMIN(IR,curiyr)+QBMRF(IR,curiyr)
         ENDDO
!
         Qe2NGIN(11,curiyr,1)=Qe2NGIN(11,curiyr,1)-TREFCON( 2,11)
         Qe2CLIN(11,curiyr,1)=Qe2CLIN(11,curiyr,1)-TREFCON( 3,11)
         Qe2RSIN(11,curiyr,1)=Qe2RSIN(11,curiyr,1)-TREFCON( 6,11)
         Qe2DSIN(11,curiyr,1)=Qe2DSIN(11,curiyr,1)-TREFCON( 7,11)
         Qe2LGIN(11,curiyr,1)=Qe2LGIN(11,curiyr,1)-TREFCON( 8,11)
         Qe2SGIN(11,curiyr,1)=Qe2SGIN(11,curiyr,1)-TREFCON(10,11)
         Qe2PCIN(11,curiyr,1)=Qe2PCIN(11,curiyr,1)-TREFCON(11,11)
         Qe2OTIN(11,curiyr,1)=Qe2OTIN(11,curiyr,1)-TREFCON(15,11)

         Qe2NGIN(11,curiyr,1)=Qe2NGIN(11,curiyr,1)+QNGRF(11,curiyr)+qNGeth(curiyr,11)+QGTLRF(11,curiyr)
         Qe2CLIN(11,curiyr,1)=Qe2CLIN(11,curiyr,1)+QCLRF(11,curiyr)+qCLeth(curiyr,11) ! +QCTLRF(11,curiyr)
         Qe2RSIN(11,curiyr,1)=Qe2RSIN(11,curiyr,1)+QRSRF(11,curiyr)
         Qe2DSIN(11,curiyr,1)=Qe2DSIN(11,curiyr,1)+QDSRF(11,curiyr)
         Qe2LGIN(11,curiyr,1)=Qe2LGIN(11,curiyr,1)+QLGRF(11,curiyr)
         Qe2SGIN(11,curiyr,1)=Qe2SGIN(11,curiyr,1)+QSGRF(11,curiyr)
         Qe2PCIN(11,curiyr,1)=Qe2PCIN(11,curiyr,1)+QPCRF(11,curiyr)  +qCCRF(11,curiyr)
         Qe2OTIN(11,curiyr,1)=Qe2OTIN(11,curiyr,1)+QOTRF(11,curiyr)

!    3) update Table 35 values
         DO IR=1,5            ! 4 census regions and 5:US
           REFCON(1,ir,curiyr)= SUM(QELRF(DIVS(ir,1):DIVS(ir,2),curiyr))+SUM(QELeth(curiyr,DIVS(ir,1):DIVS(ir,2))) ! electricity
           REFCON(2,ir,curiyr)= SUM(QNGRF(DIVS(ir,1):DIVS(ir,2),curiyr))+SUM(QNGeth(curiyr,DIVS(ir,1):DIVS(ir,2)))+SUM(QGTLRF(DIVS(ir,1):DIVS(ir,2),curiyr)) ! nat gas h&p
           REFCON(3,ir,curiyr)= SUM(QCLRF(DIVS(ir,1):DIVS(ir,2),curiyr))+SUM(QCLeth(curiyr,DIVS(ir,1):DIVS(ir,2))) ! +SUM(QCTLRF(DIVS(ir,1):DIVS(ir,2),curiyr)) ! steam coal
           REFCON(6,ir,curiyr)= SUM(QRLRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! residual
           REFCON(7,ir,curiyr)= SUM(QDSRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! distillate
           REFCON(8,ir,curiyr)= SUM(QLGRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! lpg h&p
           REFCON(10,ir,curiyr)=SUM(QSGRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! still gas
           REFCON(11,ir,curiyr)=SUM(QPCRF(DIVS(ir,1):DIVS(ir,2),curiyr)) + &  !        petroleum coke, Marketable
                                SUM(QCCRF(DIVS(ir,1):DIVS(ir,2),curiyr))      !    petroleum coke, cat coke
           REFCON(15,ir,curiyr)=SUM(QOTRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! other petroleum
           REFCON(18,ir,curiyr)=SUM(QBMRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! biomass
         ENDDO
      ELSEIF (IMODEL.EQ.12) THEN
         CALL RENEW
      ELSEIF (IMODEL.EQ.13) THEN
         CALL HMM_MAIN
      ELSE
         WRITE(*,'(a,i4)') ' IN SUBROUTINE NEXEC: IMODEL TOO HIGH: ',IMODEL
      ENDIF

      IF (ITIMNG.EQ.1) THEN
         IF (WALLSW .EQ. 1) THEN
           CALL MPTIM3(CPU_TIME_END,WALL_TIME_END)
          ! IF (WALL_TIME_END .LT. WALL_TIME_BEGIN) WALL_TIME_END = WALL_TIME_END + 24*60*60*100
           WRITE(6,60) '***Back from ',SUBR_NAMES(IMODEL), &
             FLOAT(CPU_TIME_END)/100., &
      ', Elapsed Time=',FLOAT(CPU_TIME_END - CPU_TIME_BEGIN)/100.
           IF(CURITR.LT.NTIMITR) WTIMES(CURITR,IMODEL,CURIYR)= &
           (FLOAT(WALL_TIME_END)-FLOAT(WALL_TIME_BEGIN))/100.
         ELSE
           CALL MPTIM2(CPU_TIME_END)
           WRITE(6,60) '***Back from ',SUBR_NAMES(IMODEL), &
           FLOAT(CPU_TIME_END)/100., &
      ', Elapsed Time=',(FLOAT(CPU_TIME_END) - FLOAT(CPU_TIME_BEGIN))/100.
         ENDIF
         IF(CURITR.LT.NTIMITR) CTIMES(CURITR,IMODEL,CURIYR)= &
         (FLOAT(CPU_TIME_END)-FLOAT(CPU_TIME_BEGIN))/100.
      ENDIF
      RETURN
      END
!****************************************************************
!*    Subroutine NEXEC2(IMODEL,NRETCD)
!*
!*    Executes one of the NEMS submodels for a reporting
!*    execution that does not affect current year results
!****************************************************************
      SUBROUTINE NEXEC2(IMODEL,NRETCD)
  use ifport, only : timef,sleepqq
  use ifcore, only : commitqq
      use CALLING_AIMMS

      IMPLICIT NONE

      INTEGER IMODEL,NRETCD,CPU_TIME_BEGIN,CPU_TIME_END

      INTEGER NTIMITR
      PARAMETER (NTIMITR=11)
      REAL CTIMES(NTIMITR+1,NMODEL,MNUMYR)
      REAL WTIMES(NTIMITR+1,NMODEL,MNUMYR)
      COMMON/MAINTIME/CTIMES,WTIMES
      INTEGER WALL_TIME_BEGIN, WALL_TIME_END
      INTEGER WALLSW
      COMMON /MNWALLSW/ WALLSW

      integer AIMMSNG/-1/,eunit

      integer nga_keepopen
      integer retry               ! count number of retries after aimms error

      if(AIMMSNG.lt.0) AIMMSNG=rtovalue('AIMMSNG ',0)

! +++ Set Return Code to False--CURRENTLY UNUSED
      NRETCD = 0
      IF (ITIMNG.EQ.1) THEN
         IF (WALLSW .EQ. 1) THEN
           CALL MPTIM3(CPU_TIME_BEGIN,WALL_TIME_BEGIN)
         ELSE
          CALL MPTIM2(CPU_TIME_BEGIN)
         ENDIF
      ENDIF
   60 FORMAT(10X,A,A6,', CPU Time (Seconds)=',F9.2,A,F7.2)

      call job(1) ! call job status and control routine

      IF (IMODEL.EQ.1) THEN
         CALL WORLD
      ELSEIF (IMODEL.EQ.2) THEN
         CALL MAC
      ELSEIF (IMODEL.EQ.3) THEN
        ! CALL RESD
      ELSEIF (IMODEL.EQ.4) THEN
        ! CALL COMM
      ELSEIF (IMODEL.EQ.5) THEN
         CALL IND
      ELSEIF (IMODEL.EQ.6) THEN
         CALL TRAN
      ELSEIF (IMODEL.EQ.7) THEN
!        CALL COAL_SMOOTH
         CALL UTIL
      ELSEIF (IMODEL.EQ.8) THEN
         CALL COAL
      ELSEIF (IMODEL.EQ.9) THEN
         CALL WELL
      ELSEIF (IMODEL.EQ.10) THEN
!         if(AIMMSNG.eq.0) then ! fortran version, in ngtdm.f
!           CALL NGMAIN
!         else ! invoke external AIMMS natural gas module, in ./ngas/
           if(CurCalYr.ge.2016) then ! first model year

             FYearStart=curiyr
             FYearEnd=curiyr
             FYearSubset=0
             NGA_KEEPOPEN = RTOVALUE('KEEPOPEN',0)
             IF (NGA_KEEPOPEN .EQ. 0) NGA_KEEPOPEN = RTOVALUE('KEEPNG  ',0)

             retry=0
             call AIMMS_NG(iret)

             FYearsubset=0
             if(iret.ne.0) then
               write(6,*) 'ERROR running AIMMS NGMM in NEXEC2, Reporting Loop.  Stopping now to avoid confusion.'
               CALL NDATOT('   ')
               CALL NDATOT('GDX') ! call with argument to write GDX format restart file also
               write(6,*) 'ERROR return from AIMMS NGMM is ',iret
               stop 999
             endif
           endif
!         endif

      ELSEIF (IMODEL.EQ.11) THEN
         CALL REFINE
      ELSEIF (IMODEL.EQ.12) THEN
         CALL RENEW
      ELSEIF (IMODEL.EQ.13) THEN
         CALL HMM_MAIN
      ELSE
         WRITE(*,'(a,i4)') ' IN SUBROUTINE NEXEC2: IMODEL TOO HIGH: ',IMODEL
      ENDIF

      IF (ITIMNG.EQ.1) THEN
         IF (WALLSW .EQ. 1) THEN
           CALL MPTIM3(CPU_TIME_END,WALL_TIME_END)
          ! IF (WALL_TIME_END .LT. WALL_TIME_BEGIN) WALL_TIME_END = WALL_TIME_END + 24*60*60*100
           WRITE(6,60) '***Back from ',SUBR_NAMES(IMODEL), &
             FLOAT(CPU_TIME_END)/100., &
      ', Elapsed Time=',FLOAT(CPU_TIME_END - CPU_TIME_BEGIN)/100.
           WTIMES(NTIMITR,IMODEL,CURIYR)= &
           (FLOAT(WALL_TIME_END)-FLOAT(WALL_TIME_BEGIN))/100.
         ELSE
           CALL MPTIM2(CPU_TIME_END)
           WRITE(6,60) '***Back from ',SUBR_NAMES(IMODEL), &
           FLOAT(CPU_TIME_END)/100., &
      ', Elapsed Time=',(FLOAT(CPU_TIME_END) - FLOAT(CPU_TIME_BEGIN))/100.
         ENDIF
         CTIMES(NTIMITR,IMODEL,CURIYR)= &
         (FLOAT(CPU_TIME_END)-FLOAT(CPU_TIME_BEGIN))/100.
      ENDIF

      RETURN
      END
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
!* ******************************************************************
!*  Subroutine CVTEST(IMODEL)
!*
!*  Sets convergence flags for the module: CNVTST(IMODEL,CURIYR) in
!*  CVTEST.  CVTEST computes absolute and percentage changes for the
!*  key convergence variables, and sets CNVTST(IMODEL,CURIYR) to 1
!*  if changes are less than the required tolerance.
!*
!*  Inputs:
!*    IMODEL ARGUMENT Index of Model just executed
!*    CURIYR (NCNTRL) Current year being executed
!*    FRCTOL (NCNTRL) Minimum fractional convergence tolerance
!*    ABSTOL (NCNTRL) Minimum absolute convergence tolerance
!*    MQTY   (PQ)     Vector of QUANTITY products, current iteration
!*    MPRC   (PQ)     Vector of PRICE products, current iteration
!*    PREVQ  /PREV/   Vector of QUANTITY products, previous iteration
!*    PREVP  /PREV/   Vector of PRICE products, previous iteration
!*    ALSO, 3 NGMM  NG PRICE VARIABLES AT THE NERC REGION LEVEL
!*          3 EMM   NG QUANT VARIABLES AT THE NERC REGION LEVEL
!*         12 CMM COAL PRICE VARIABLES AT THE COAL REGION LEVEL
!*         15 EMM COAL QUANT VARIABLES AT THE COAL REGION LEVEL
!*
!*  QUANTITY CONVERGENCE TESTS:
!*     THE ABSOLUTE CHANGE MUST EXCEED A MINIMUM, AND THE RELATIVE
!*     CHANGE, COMPUTED AS:  (ABSOLUTE CHANGE) / ((CURRENT+PREVIOUS)/2)
!*     MUST EXCEED MINIMUM.
!*  PRICE CONVERGENCE TESTS:
!*     SAME AS QUANTITY TEST, BUT ALSO THE CORRESPONDING QUANTITY FOR
!*     THE PRICE MUST EXCEED A MINIMUM.  CURRENTLY, THE MINIMUM
!*     USED IS THE SAME MINIMUM USED FOR THE QUANTITY CONVERGENCE TEST
!*  Outputs:
!*    CNVTST(IMODEL,CURIYR)   Flag set to 0 if changes exceed minimum
!*                            tolerance.
!*    NOW USES ADJUSTED PRICES PW2
!*******************************************************************
      SUBROUTINE CVTEST(IMODEL)
      IMPLICIT NONE

      INTEGER IMODEL
      include 'parametr'
      include 'emmparm'
      include 'cdsparms'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'uefdout'
      include 'ngtdmout'
      include 'coalemm'
      include 'coalprc'
      include 'pqchar'
      include 'uso2grp'

      REAL FRCCHG                      ! Holds fractional change
      REAL ABSCHG                      ! Holds absolute change
      INTEGER IR                       ! Subscript index for REGION
      INTEGER IV                       ! Subscript index for VARIABLE
      INTEGER IQ                       ! Subscript index for Q VARIABLE
      INTEGER IY                       ! Subscript index for YEAR (IY=CURIYR)
      INTEGER NCHECK                   ! Flag for tolerance
      CHARACTER*9 VARNAM
      INTEGER NUMQAS,NUMQ_AS,NUMPAS,NUMP_AS,PQMATCH
      EXTERNAL NUMQ_AS,NUMP_AS,PQMATCH,RTOVALUE
      INTEGER MNDBGCV,RTOVALUE
      REAL TRIGGER
      LOGICAL DONEYET/.FALSE./
      NCHECK=1
      IY=CURIYR

! DEBUG CODE TO TRIGGER CVTESTS.
! GET RUNTIME PARAMETER MNDBGCV TO SEE IF DEBUG CODE SHOULD EXECUTE.
! PERTURB VALUES ON EVEN ITERATIONS TO TRIGGER NONCONVERGENCE
      IF(.NOT. DONEYET) MNDBGCV=RTOVALUE('MNDBGCV ',0)
      DONEYET=.TRUE.
      IF(MNDBGCV.EQ.1) THEN
        IF(CURITR.EQ.((CURITR/2)*2)) THEN
          TRIGGER=1.11
        ELSE
          TRIGGER=1/1.11
        ENDIF
        IR=7
        DO IV = 1,MNUMP
          NUMPAS=NUMP_AS(IV,MNUMP)
          IF(NUMPAS.EQ.0) THEN
            PREVP(IR,IY,IV)=PREVP(IR,IY,IV)*TRIGGER
          ENDIF
        ENDDO
        DO IV = 1,MNUMQ
          NUMQAS=NUMQ_AS(IV,MNUMQ)
          IF(NUMQAS.EQ.0) THEN
            PREVQ(IR,IY,IV)=PREVQ(IR,IY,IV)*TRIGGER
          ENDIF
        ENDDO
        DO IV = 1,7
          PREVUP(IR,IY,IV)=PREVUP(IR,IY,IV)*TRIGGER
        ENDDO
        DO IV = 1,7
          PREVUQ(IR,IY,IV)=PREVUQ(IR,IY,IV)*TRIGGER
        ENDDO
        PREVCPS(1,IR,IY)=PREVCPS(1,IR,IY)*TRIGGER
        PREVCPS(2,IR,IY)=PREVCPS(2,IR,IY)*TRIGGER
        DO IV = 1,NCLUT1
          PREVCQS(IR,IY,IV)=PREVCQS(IR,IY,IV)*TRIGGER
        ENDDO
        IR=11
        DO IV = 1,MNOTH
          CONVOTH(2,IV,IY)=CONVOTH(2,IV,IY)*TRIGGER
        ENDDO
        PREVPCAP(IR,IY)=PREVPCAP(IR,IY)*TRIGGER
        PREV_CL_CF(IR,IY)=PREV_CL_CF(IR,IY)*TRIGGER
        PREV_CL_BTUs(IR,IY)=PREV_CL_BTUs(IR,IY)*TRIGGER
      ENDIF
! END DEBUG CODE

! NOTE: NCHECK GETS SET TO 0 IN MNPRTCV IF ANY VARIABLE FAILS CONVERGENCE TESTS.
      DO IR=1,MNUMCR-2  ! 9 census divisons.  skip 10 (unused) and 11 (national total)
        DO IV = 1,MNUMP
          NUMPAS=NUMP_AS(IV,MNUMP)
          IF(NUMPAS.EQ.0) THEN
            CALL MNCHKCV(MPRC(IR,IY,IV),PREVP(IR,IY,IV),FRCCHG,ABSCHG)
            IQ=PQMATCH(IV,MNUMQ,MNUMP)     ! Get matching QUANTITY product
            IF(FRCCHG.GE.MNPCNV(1,IV).AND.ABSCHG.GE.MNPCNV(2,IV) .AND.  & ! CNV TEST
                   (MQTY(IR,IY,IQ).GE.MNQCNV(2,IQ)                      & ! CNV TEST
               .OR.PREVQ(IR,IY,IQ).GE.MNQCNV(2,IQ))  ) THEN               ! CNV TEST
               VARNAM=MPVARS(IV)//'   '
               CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                   MPRC(IR,IY,IV),PREVP(IR,IY,IV),FRCCHG,MNPCNV(1,IV), &
                   VARNAM,SUBR_NAMES(IMODEL),'&&&P')
            ENDIF
          ENDIF
        ENDDO
        DO IV = 1,MNUMQ
          NUMQAS=NUMQ_AS(IV,MNUMQ)
          IF(NUMQAS.EQ.0) THEN
            CALL MNCHKCV(MQTY(IR,IY,IV),PREVQ(IR,IY,IV),FRCCHG,ABSCHG)
            IF(FRCCHG.GE.MNQCNV(1,IV).AND.ABSCHG.GE.MNQCNV(2,IV))THEN ! CNV TEST
              VARNAM=MQVARS(IV)//'   '
              CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                  MQTY(IR,IY,IV),PREVQ(IR,IY,IV),FRCCHG,MNQCNV(1,IV), &
                  VARNAM,SUBR_NAMES(IMODEL),'&&&Q')
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      DO IR=1,NNGEM
        DO IV = 1,3
          CALL MNCHKCV(MUPRC(IR,IY,IV),PREVUP(IR,IY,IV),FRCCHG,ABSCHG)
          IF (FRCCHG.GE.MNUPCNV(1,IV).AND.ABSCHG.GE.MNUPCNV(2,IV) .AND. & ! CNV TEST
             (MUQTY(IR,IY,IV).GE.MNUQCNV(2,IV)                          & ! CNV TEST
         .OR.PREVUQ(IR,IY,IV).GE.MNUQCNV(2,IV))       )THEN               ! CNV TEST
             VARNAM=MUPVARS(IV)
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 MUPRC(IR,IY,IV),PREVUP(IR,IY,IV),FRCCHG,MNUPCNV(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&P')
          ENDIF
        ENDDO
        DO IV = 1,3
          CALL MNCHKCV(MUQTY(IR,IY,IV),PREVUQ(IR,IY,IV),FRCCHG,ABSCHG)
          IF(FRCCHG.GE.MNUQCNV(1,IV).AND.ABSCHG.GE.MNUQCNV(2,IV))THEN ! CNV TEST
             VARNAM=MUQVARS(IV)
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 MUQTY(IR,IY,IV),PREVUQ(IR,IY,IV),FRCCHG,MNUQCNV(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Q')
          ENDIF
        ENDDO
        DO IV = 1,3
          CALL MNCHKCV(SGASPRICE(IR,IY,IV),PREVGASP(IR,IY,IV),FRCCHG,ABSCHG)
          IF (FRCCHG.GE.MNGASPCNV(1,IV).AND.ABSCHG.GE.MNGASPCNV(2,IV) .AND. & ! CNV TEST
             (SGASQUANT(IR,IY,IV).GE.MNGASQCNV(2,IV)                          & ! CNV TEST
         .OR.PREVGASQ(IR,IY,IV).GE.MNGASQCNV(2,IV))       )THEN               ! CNV TEST
             VARNAM=SGASPVARS(IV)
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 SGASPRICE(IR,IY,IV),PREVGASP(IR,IY,IV),FRCCHG,MNGASPCNV(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&P')
          ENDIF
        ENDDO
        DO IV = 1,3
          CALL MNCHKCV(SGASQUANT(IR,IY,IV),PREVGASQ(IR,IY,IV),FRCCHG,ABSCHG)
          IF(FRCCHG.GE.MNGASQCNV(1,IV).AND.ABSCHG.GE.MNGASQCNV(2,IV))THEN ! CNV TEST
             VARNAM=SGASQVARS(IV)
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 SGASQUANT(IR,IY,IV),PREVGASQ(IR,IY,IV),FRCCHG,MNGASQCNV(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Q')
          ENDIF
        ENDDO
      ENDDO

!   Added check for new coal/utility prices and quantities
      DO IR=1,NDRGG
        DO IV = 1,2
          CALL MNCHKCV(PCLELCDR(IV,IR,IY),PREVCPS(IV,IR,IY),FRCCHG,ABSCHG)
          IF(FRCCHG.GE.MNCPCNVS(1).AND.ABSCHG.GE.MNCPCNVS(2)   ) THEN ! CNV TEST
!           .AND. (QCLCLNR(IR,IY,IV).GE.MNCQCNVS(2,IV)              & ! CNV TEST
!            .OR. PREVCQS(IR,IY,IV).GE.MNCQCNVS(2,IV))         )THEN  ! CNV TEST
               VARNAM=MCPVARSS(IV)
               CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                   PCLELCDR(IV,IR,IY),PREVCPS(IV,IR,IY),FRCCHG,MNCPCNVS(1), &
                   VARNAM,SUBR_NAMES(IMODEL),'&&&P')
          ENDIF
        ENDDO
        DO IV = 1,NCLUT1
          CALL MNCHKCV(QCLCLNR(IR,IY,IV),PREVCQS(IR,IY,IV),FRCCHG,ABSCHG)
          IF(FRCCHG.GE.MNCQCNVS(1,IV).AND.ABSCHG.GE.MNCQCNVS(2,IV))THEN ! CNV TEST
             VARNAM=MCQVARSS(IV)//'  '
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 QCLCLNR(IR,IY,IV),PREVCQS(IR,IY,IV),FRCCHG,MNCQCNVS(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Q')
          ENDIF
        ENDDO
      ENDDO
      IR=11
      IR=11
      DO IV = 1,MNOTH
         CALL MNCHKCV(CONVOTH(2,IV,IY),CONVOTH(1,IV,IY),FRCCHG,ABSCHG)
         IF(FRCCHG.GE.MNOCNV(1,IV).AND.ABSCHG.GE.MNOCNV(2,IV))THEN   ! CNV TEST
             VARNAM=MOVARS(IV)
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 CONVOTH(2,IV,IY),CONVOTH(1,IV,IY),FRCCHG,MNOCNV(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&O')
         ENDIF
      ENDDO

!     Added check for new coal productive capacity

      DO IR=1,MX_NCOALS
        CALL MNCHKCV(XCL_PCAP(IR,IY),PREVPCAP(IR,IY),FRCCHG,ABSCHG)
        IF(FRCCHG.GE.MNZCNV(1,1).AND.ABSCHG.GE.MNZCNV(2,1))  THEN  ! CNV TEST
             VARNAM=MZVARS(1)//' '
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 XCL_PCAP(IR,IY),PREVPCAP(IR,IY),FRCCHG,MNZCNV(1,1), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Z')
        ENDIF
      ENDDO

!     Added check for coal generation unit utilization factors

      DO IR=1,MX_UNITS
        CALL MNCHKCV(EMM_CL_CF(IR,IY),PREV_CL_CF(IR,IY),FRCCHG,ABSCHG)
        IF(FRCCHG.GE.MNZCNV(1,2).AND.ABSCHG.GE.MNZCNV(2,2))  THEN  ! CNV TEST
             VARNAM=MZVARS(2)//' '
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 EMM_CL_CF(IR,IY),PREV_CL_CF(IR,IY),FRCCHG,MNZCNV(1,2), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Z')
        ENDIF
        CALL MNCHKCV(EMM_CL_BTUs(IR,IY),PREV_CL_BTUs(IR,IY),FRCCHG,ABSCHG)
        IF(FRCCHG.GE.MNZCNV(1,3).AND.ABSCHG.GE.MNZCNV(2,3))  THEN  ! CNV TEST
             VARNAM=MZVARS(3)//' '
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 EMM_CL_BTUs(IR,IY),PREV_CL_BTUs(IR,IY),FRCCHG,MNZCNV(1,3), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Z')
        ENDIF
      ENDDO

      IF (NCHECK.EQ.1) THEN
          CNVTST(IMODEL,CURIYR)=1
      ELSE
          CNVTST(IMODEL,CURIYR)=0
      ENDIF
      RETURN
      END
!******************************************************************
      SUBROUTINE MNCHKCV(CUR,OLD,FRCCHG,ABSCHG)
      IMPLICIT NONE
! SETS FRACTIONAL AND ABSOLUTE CHANGE VARIABLES, GIVEN CURRENT AND PREVIOUS VALUES.

! CALLED BY CVTEST

      REAL CUR    ! CURRENT VALUE              (INPUT)
      REAL OLD    ! PREVIOUS VALUE             (INPUT)
      REAL FRCCHG ! FRACTIONAL RELATIVE CHANGE (OUTPUT)
      REAL ABSCHG ! ABSOLUTE VALUE OF CHANGE   (OUTPUT)
      REAL DENOM  ! DENOMINATOR FOR RELATIVE CHANGE (LOCAL)
      FRCCHG=0.0
      ABSCHG=ABS(CUR-OLD)
      DENOM=(CUR+OLD)/2.
      IF (DENOM.NE.0.0) FRCCHG = ABSCHG/DENOM
      RETURN
      END
!******************************************************************
      SUBROUTINE MNPRTCV(NCHECK,IY,IT,IR,CUR,OLD,FRCCHG,LIMIT,VARNAM,SUBNAM,TYPSTR)
      IMPLICIT NONE

! PRINTS MESSAGE FOR VARIABLES NOT MEETING CONVERGENCE CRITERIA.  THE
! MESSAGE DISPLAYS THE SUBROUTINE, VARIABLE NAME, CURRENT AND PRIOR VALUES,
! PERCENTAGE CHANGE, TOLERANCE,
! ITERATION COUNT, REGION, YEAR.  THE ROUTINE ALSO CALLS ROUTINE TO
! PUT THE ENTRY INTO THE SORTED SUMMARY LIST OF VARIABLES NOT CONVERGED.

! CALLED BY CVTEST
      INTEGER NCHECK  ! FLAG FOR WHETHER HEADING HAS BEEN PRINTED FOR GROUP
      INTEGER IY      ! NEMS YEAR INDEX
      INTEGER IT      ! NEMS ITERATION
      INTEGER IR      ! REGION INDEX
      REAL    CUR     ! CURRENT VALUE
      REAL    OLD     ! PREVIOUS VALUE
      REAL    FRCCHG  ! FRACTIONAL RELATIVE CHANGE
      REAL    LIMIT   ! TOLERANCE LIMIT ON FRACTIONAL RELATIVE CHANGE
      CHARACTER*9 VARNAM  ! VARIABLE NAME
      CHARACTER*6 SUBNAM  ! SHORT SUBROUTINE NAME FOR NEMS ROUTINE
      CHARACTER*4 TYPSTR  ! SEARCH STRING ('&&&Q', '&&&P' OR '&&&O')

      IF(NCHECK.EQ.1 .AND. IT.GE.3) WRITE(*,'(A)') ' &&&   MODULE' // &
           '  VARIABLE  YR IT RG   CURRENT  PREVIOUS %CHANGE %TOLER'
      NCHECK=0

      IF(IT .GE. 3 .AND. TYPSTR .NE. '&&&Z') WRITE(*,'(A,3I3,2F10.3,F7.1,F7.1,F9.3)') ' ' // &
             TYPSTR //': '// SUBNAM // '  ' // VARNAM,IY,IT,IR,CUR,OLD,FRCCHG*100.,LIMIT*100.

      CALL SORTRPT(FRCCHG,IR,OLD,CUR,VARNAM)

      RETURN
      END
!******************************************************************
!*    Subroutine RELAX
!*    PW2 USE UNADJUSTED PRICES FOR RELAXATION, TOO
!*    Heuristic routine to limit or reset certain model outputs
!*    between iterations to speed overall convergence.
!******************************************************************

      SUBROUTINE RELAX

      IMPLICIT NONE

      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'uefdout'
      include 'ngtdmout'
      include 'ngtdmrep'
      include 'cdsparms'
      include 'coalemm'
      include 'coalprc'
      include 'pqchar'
      include 'intout'
      include 'macout'
      include 'pmmrpt'
      include 'emission'
      include 'uso2grp'
      include 'ogsmout'
      include 'wrenew'
      include 'pmmout'

      INTEGER IR         ! SUBSCRIPT INDEX FOR REGION
      INTEGER IV         ! SUBSCRIPT INDEX FOR VARIABLE
      INTEGER IX         ! readjustment subscript

! +++ Reset PRICES and QUANTITIES to points 2/3 between previous and current iteration.
      DO IR=1,MNUMCR-2
         DO IV = 1,MNUMP
            MPRC(IR,CURIYR,IV)=PREVP(IR,CURIYR,IV)+ &
                RLXP(CURITR,IV)*(MPRC(IR,CURIYR,IV)-PREVP(IR,CURIYR,IV))
         ENDDO
         DO IV = 1,MNUMQ
            MQTY(IR,CURIYR,IV)=PREVQ(IR,CURIYR,IV)+ &
                RLXQ(CURITR,IV)*(MQTY(IR,CURIYR,IV)-PREVQ(IR,CURIYR,IV))
         ENDDO
      ENDDO
      DO IR=1,NNGEM
         DO IV = 1,3
            MUPRC(IR,CURIYR,IV)=PREVUP(IR,CURIYR,IV)+ &
                RLXUP(CURITR,IV)*(MUPRC(IR,CURIYR,IV)-PREVUP(IR,CURIYR,IV))
            MUQTY(IR,CURIYR,IV)=PREVUQ(IR,CURIYR,IV)+ &
                RLXUQ(CURITR,IV)*(MUQTY(IR,CURIYR,IV)-PREVUQ(IR,CURIYR,IV))
         ENDDO
         DO IV = 1,4
            IF (IV.LE.2) THEN
              IX = 1
            ELSE
              IX = 2
            ENDIF
            SMUPRC(IR,CURIYR,IV)=PREVUP(IR,CURIYR,IV+3)+ &
                RLXUP(CURITR,IX)*(SMUPRC(IR,CURIYR,IV)-PREVUP(IR,CURIYR,IV+3))
            SMUQTY(IR,CURIYR,IV)=PREVUQ(IR,CURIYR,IV+3)+ &
                RLXUQ(CURITR,IX)*(SMUQTY(IR,CURIYR,IV)-PREVUQ(IR,CURIYR,IV+3))
         ENDDO
         DO IV = 1,3
            SGASPRICE(IR,CURIYR,IV)=PREVGASP(IR,CURIYR,IV)+ &
                RLXGASP(CURITR,IV)*(SGASPRICE(IR,CURIYR,IV)-PREVGASP(IR,CURIYR,IV))
            SGASQUANT(IR,CURIYR,IV)=PREVGASQ(IR,CURIYR,IV)+ &
                RLXGASQ(CURITR,IV)*(SGASQUANT(IR,CURIYR,IV)-PREVGASQ(IR,CURIYR,IV))
         ENDDO
      ENDDO

!  New coal regions
      DO IR=1,NDRGG
         PCLELCDR(1,IR,CURIYR)=PREVCPS(1,IR,CURIYR)+ &
                RLXCPS(CURITR)*(PCLELCDR(1,IR,CURIYR)-PREVCPS(1,IR,CURIYR))
         PCLELCDR(2,IR,CURIYR)=PREVCPS(2,IR,CURIYR)+ &
                RLXCPS(CURITR)*(PCLELCDR(2,IR,CURIYR)-PREVCPS(2,IR,CURIYR))
         DO IV = 1,NCLUT1
            QCLCLNR(IR,CURIYR,IV)=PREVCQS(IR,CURIYR,IV)+ &
                RLXCQS(CURITR,IV)*(QCLCLNR(IR,CURIYR,IV)-PREVCQS(IR,CURIYR,IV))
         ENDDO
      ENDDO
      DO IV = 1,MNOTH
         CONVOTH(2,IV,CURIYR)=CONVOTH(1,IV,CURIYR)+ &
            RLXOTH(CURITR,IV)*(CONVOTH(2,IV,CURIYR)-CONVOTH(1,IV,CURIYR))
      ENDDO

!  Coal Supply Curce Productive Capacity
      DO IR=1,MX_NCOALS
         XCL_PCAP(IR,CURIYR)=PREVPCAP(IR,CURIYR)+ &
             RLXPCAP(CURITR,1)*(XCL_PCAP(IR,CURIYR)-PREVPCAP(IR,CURIYR))
      ENDDO

!  Coal generation unit utilization factors
      DO IR=1,MX_UNITS
         EMM_CL_CF(IR,CURIYR) = PREV_CL_CF(IR,CURIYR)+ &
             RLXPCAP(CURITR,2) * (EMM_CL_CF(IR,CURIYR) - PREV_CL_CF(IR,CURIYR))
         EMM_CL_BTUs(IR,CURIYR) = PREV_CL_BTUs(IR,CURIYR)+ &
             RLXPCAP(CURITR,3) * (EMM_CL_BTUs(IR,CURIYR) - PREV_CL_BTUs(IR,CURIYR))
      ENDDO

!     SINCE WE RELAX THE OTHERS, WE NEED TO COPY THEM BACK

      MC_GDPR(CURIYR)         = CONVOTH(2,1,CURIYR)
      IT_WOP(CURIYR,1)        = CONVOTH(2,2,CURIYR)
      RFQICRD(MNUMPR,CURIYR)  = CONVOTH(2,3,CURIYR)
      RFPQIPRDT(MNUMPR,CURIYR,2) = CONVOTH(2,4,CURIYR)
      EMETAX(1,CURIYR)        = CONVOTH(2,5,CURIYR)
      EMELPSO2(CURIYR,1)      = CONVOTH(2,6,CURIYR)
      ECP_PHG(1,CURIYR)       = CONVOTH(2,7,CURIYR)
      OGWPRNG(MNUMOR,CURIYR)  = CONVOTH(2,8,CURIYR)
      OGCNPPRD(1,CURIYR)      = CONVOTH(2,9,CURIYR)
      OGCNPPRD(2,CURIYR)      = CONVOTH(2,10,CURIYR)
      GLBCRDDMD(CURIYR)       = CONVOTH(2,43,CURIYR)
      ECP_PSO2(0:10,CURIYR,1) = CONVOTH(2,44:54,CURIYR)
      ECP_PSO2(0:10,CURIYR,2) = CONVOTH(2,55:65,CURIYR)


! Comment out the following, as it would only mess things up
!     PBMET(1:9,CURIYR,1)   = CONVOTH(2,21:29,CURIYR)
!     PBMET(MNUMCR,CURIYR,1)= CONVOTH(2,30,CURIYR)
!     QBMET(1:9,CURIYR,1)   = CONVOTH(2,31:39,CURIYR)
!     QBMET(MNUMCR,CURIYR,1)= CONVOTH(2,40,CURIYR)

! Recalculate national totals for PBMET and QBMET to ensure they add up
!     DO IV=1,NUMETHQ
!     QBMET(MNUMCR,CURIYR,IV) = sum(QBMET(1:9,CURIYR,IV))
!     PBMET(MNUMCR,CURIYR,IV) = 0.0
!     DO IR=1,9
!        IF (QBMET(MNUMCR,CURIYR,IV) .NE. 0.0) &
!        PBMET(MNUMCR,CURIYR,IV) = PBMET(MNUMCR,CURIYR,IV) + &
!            QBMET(IR,CURIYR,IV) * PBMET(IR,CURIYR,IV) / QBMET(MNUMCR,CURIYR,IV)
!     ENDDO
!     ENDDO

      RETURN
      END


! DOCVFACTS:  this subroutine calculates some weighted average conversion factors

      SUBROUTINE DOCVFACTS
      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'intout'
      include 'indout'     ! used for INQLGHP only.  when switch to QPRIN, remove this
      include 'pmmrpt'
      include 'pmmout'
      include 'lfmmout'
      include 'ogsmout'
      include 'convfact'
      include 'tranrep'

      INTEGER HISTYR, ICV, IV
      COMMON /CVHISTYR/ HISTYR           ! last history year for conversion factors
      REAL API_TO_BTU
      EXTERNAL API_TO_BTU

      IF (CURIYR .LE. HISTYR) THEN
          IF (DBDUMP .EQ. 1) WRITE(ICNVRTOUT,'(A)')  "   Historical year.  Not updating conversion factors!"
          RETURN
      ENDIF
! Calculate overall petroleum conversion factor
      IF (DBDUMP .EQ. 1 .AND. CURITR .EQ. 1) &
          WRITE(ICNVRTOUT,'(A,I4,A,F6.3)')  &
        " Petroleum consumption conversion factor from file in year ", &
          CURIYR+1989, " is ",CFTPQ(CURIYR)
!  unfinished oils
      IF ((RFIPQAR3(MNUMPR,CURIYR,2)+RFIPQGO3(MNUMPR,CURIYR,2)+RFIPQMN3(MNUMPR,CURIYR,2)) .NE. 0.0) THEN
          CFIMUO(CURIYR) =(RFIPQAR3(MNUMPR,CURIYR,2)*CFAR3(CURIYR)+   &
                           RFIPQGO3(MNUMPR,CURIYR,2)*CFGO3(CURIYR)+   &
                           RFIPQMN3(MNUMPR,CURIYR,2)*CFMN3(CURIYR))/  &
                     (RFIPQAR3(MNUMPR,CURIYR,2)+RFIPQGO3(MNUMPR,CURIYR,2)+RFIPQMN3(MNUMPR,CURIYR,2))
      ELSE
          CFIMUO(CURIYR) = CFDSQ
      ENDIF
      CFE85Q(CURIYR) = ETHNE85 * CFETQ(CURIYR) + TRGNE85 * CFRBOB(CURIYR)
!  uncomment the following if we switch to OGSM heat contents.  (will also need lfreport change)
!     CFCRDLTSWT(CURIYR)   = OGCRDHEAT( 1,CURIYR)
!     CFCRDLTSOUR(CURIYR)  = OGCRDHEAT( 2,CURIYR)
!     CFCRDMD2SOUR(CURIYR) = OGCRDHEAT( 3,CURIYR)
!     CFCRDMDSOUR(CURIYR)  = OGCRDHEAT( 4,CURIYR)
!     CFCRDHVSWT(CURIYR)   = OGCRDHEAT( 5,CURIYR)
!     CFCRDHVSOUR(CURIYR)  = OGCRDHEAT( 6,CURIYR)
!     CFCRDCA(CURIYR)      = OGCRDHEAT( 7,CURIYR)
!     CFCRDSYN(CURIYR)     = OGCRDHEAT( 8,CURIYR)
!     CFCRDDILBIT(CURIYR)  = OGCRDHEAT( 9,CURIYR)
!     CFCRDLT2SWT(CURIYR)  = OGCRDHEAT(10,CURIYR)
!     CFCRDLSCOND(CURIYR)  = OGCRDHEAT(11,CURIYR)
      IF (sum(OGCRDPRD(1:MNUMOR-1, 1:MNCRUD,CURIYR)) .GT. 0.0) THEN
         CFCRDDOM(CURIYR) =                                           &
              (sum(OGCRDPRD(1:MNUMOR-1, 1,CURIYR)) * CFCRDLTSWT(CURIYR)      + &
               sum(OGCRDPRD(1:MNUMOR-1, 2,CURIYR)) * CFCRDLTSOUR(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1, 3,CURIYR)) * CFCRDMD2SOUR(CURIYR)    + &
               sum(OGCRDPRD(1:MNUMOR-1, 4,CURIYR)) * CFCRDMDSOUR(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1, 5,CURIYR)) * CFCRDHVSWT(CURIYR)      + &
               sum(OGCRDPRD(1:MNUMOR-1, 6,CURIYR)) * CFCRDHVSOUR(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1, 7,CURIYR)) * CFCRDCA(CURIYR)         + &
               sum(OGCRDPRD(1:MNUMOR-1, 8,CURIYR)) * CFCRDSYN(CURIYR)        + &
               sum(OGCRDPRD(1:MNUMOR-1, 9,CURIYR)) * CFCRDDILBIT(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1,10,CURIYR)) * CFCRDLT2SWT(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1,11,CURIYR)) * CFCRDLSCOND(CURIYR)) /    &
                  sum(OGCRDPRD(1:MNUMOR-1, 1:MNCRUD,CURIYR))
      ENDIF
      IF (sum(Q_CRUDE_IMPORTA(MNUMPR, 1:11,CURCALYR)) .GT. 0.0) THEN
         CFCRDIMP(CURIYR) =                                                    &
              (Q_CRUDE_IMPORTA(MNUMPR, 1,CURCALYR) * CFCRDLTSWT(CURIYR)      + &
               Q_CRUDE_IMPORTA(MNUMPR, 2,CURCALYR) * CFCRDLTSOUR(CURIYR)     + &
               Q_CRUDE_IMPORTA(MNUMPR, 3,CURCALYR) * CFCRDMD2SOUR(CURIYR)    + &
               Q_CRUDE_IMPORTA(MNUMPR, 4,CURCALYR) * CFCRDMDSOUR(CURIYR)     + &
               Q_CRUDE_IMPORTA(MNUMPR, 5,CURCALYR) * CFCRDHVSWT(CURIYR)      + &
               Q_CRUDE_IMPORTA(MNUMPR, 6,CURCALYR) * CFCRDHVSOUR(CURIYR)     + &
               Q_CRUDE_IMPORTA(MNUMPR, 7,CURCALYR) * CFCRDCA(CURIYR)         + &
               Q_CRUDE_IMPORTA(MNUMPR, 8,CURCALYR) * CFCRDSYN(CURIYR)        + &
               Q_CRUDE_IMPORTA(MNUMPR, 9,CURCALYR) * CFCRDDILBIT(CURIYR)     + &
               Q_CRUDE_IMPORTA(MNUMPR,10,CURCALYR) * CFCRDLT2SWT(CURIYR)     + &
               Q_CRUDE_IMPORTA(MNUMPR,11,CURCALYR) * CFCRDLSCOND(CURIYR)) /    &
                  sum(Q_CRUDE_IMPORTA(MNUMPR, 1:11,CURCALYR))
      ENDIF
      IT_WOP(CURIYR,2) = IT_WOP(CURIYR,1) / CFCRDIMP(CURIYR)
      IF (sum(Q_CRUDE_EXPORTS(MNUMPR, 1:11,CURCALYR)) .GT. 0.0) THEN
         CFCRDEXP(CURIYR) =                                                    &
              (Q_CRUDE_EXPORTS(MNUMPR, 1,CURCALYR) * CFCRDLTSWT(CURIYR)      + &
               Q_CRUDE_EXPORTS(MNUMPR, 2,CURCALYR) * CFCRDLTSOUR(CURIYR)     + &
               Q_CRUDE_EXPORTS(MNUMPR, 3,CURCALYR) * CFCRDMD2SOUR(CURIYR)    + &
               Q_CRUDE_EXPORTS(MNUMPR, 4,CURCALYR) * CFCRDMDSOUR(CURIYR)     + &
               Q_CRUDE_EXPORTS(MNUMPR, 5,CURCALYR) * CFCRDHVSWT(CURIYR)      + &
               Q_CRUDE_EXPORTS(MNUMPR, 6,CURCALYR) * CFCRDHVSOUR(CURIYR)     + &
               Q_CRUDE_EXPORTS(MNUMPR, 7,CURCALYR) * CFCRDCA(CURIYR)         + &
               Q_CRUDE_EXPORTS(MNUMPR, 8,CURCALYR) * CFCRDSYN(CURIYR)        + &
               Q_CRUDE_EXPORTS(MNUMPR, 9,CURCALYR) * CFCRDDILBIT(CURIYR)     + &
               Q_CRUDE_EXPORTS(MNUMPR,10,CURCALYR) * CFCRDLT2SWT(CURIYR)     + &
               Q_CRUDE_EXPORTS(MNUMPR,11,CURCALYR) * CFCRDLSCOND(CURIYR)) /    &
                  sum(Q_CRUDE_EXPORTS(MNUMPR, 1:11,CURCALYR))
      ENDIF
!  calculation option 12 in API_TO_BTU function converts API to Btu (array element 1 converted to array element 2)
!  calculation option 21 in API_TO_BTU function converts Btu to API (array element 2 converted to array element 1)
      APICAMG(1,CURIYR) = API_TO_BTU(APICAMG(2,CURIYR),21)
      APILTSW(2,CURIYR) = API_TO_BTU(APILTSW(1,CURIYR),12)
      APILTSO(2,CURIYR) = API_TO_BTU(APILTSO(1,CURIYR),12)
      APIMMSO(2,CURIYR) = API_TO_BTU(APIMMSO(1,CURIYR),12)
      APIMDSO(2,CURIYR) = API_TO_BTU(APIMDSO(1,CURIYR),12)
      APIHVSW(2,CURIYR) = API_TO_BTU(APIHVSW(1,CURIYR),12)
      APIHVSO(2,CURIYR) = API_TO_BTU(APIHVSO(1,CURIYR),12)
      APICA(2,CURIYR)   = API_TO_BTU(APICA(1,CURIYR),12)
      APISYN(2,CURIYR)  = API_TO_BTU(APISYN(1,CURIYR),12)
      APIDIL(2,CURIYR)  = API_TO_BTU(APIDIL(1,CURIYR),12)
      APILLSW(2,CURIYR) = API_TO_BTU(APILLSW(1,CURIYR),12)
      API50PL(2,CURIYR) = API_TO_BTU(API50PL(1,CURIYR),12)
      APICRDDOM(2,CURIYR) = API_TO_BTU(APICRDDOM(1,CURIYR),12)
      APICRDIMP(2,CURIYR) = API_TO_BTU(APICRDIMP(1,CURIYR),12)
      APICRDEXP(2,CURIYR) = API_TO_BTU(APICRDEXP(1,CURIYR),12)

!  but first somebody needs to calculate a total distillate conversion factor:
      CFDSQT(CURIYR) =(CFDSRS(CURIYR) * QDSRS(MNUMCR,CURIYR) + &
                        CFDSCM(CURIYR) * QDSCM(MNUMCR,CURIYR) + &
                        CFDSIN(CURIYR) * QDSIN(MNUMCR,CURIYR) + &
                        CFDSTR(CURIYR) * QDSTR(MNUMCR,CURIYR) + &
                        CFDSEL(CURIYR) * QDSEL(MNUMCR,CURIYR)) / &
                   (QDSRS(MNUMCR,CURIYR) + QDSCM(MNUMCR,CURIYR) + QDSIN(MNUMCR,CURIYR) + &
                    QDSTR(MNUMCR,CURIYR) + QDSEL(MNUMCR,CURIYR))

!  now it is all clear for total petroleum:
      CFTPQ(CURIYR) =(QMGAS(11,CURIYR) + QJFTR(11,CURIYR) + &
                      QDSAS(11,CURIYR) + QLGAS(11,CURIYR) + QPFIN(11,CURIYR) + &
                      QRLAS(11,CURIYR) + QRHAS(11,CURIYR) + &
! replacing other with the pieces - ind (lubricants, pentanes plus) and tran (av gas, lubricants)
                      QPPIN(MNUMCR,CURIYR) + QPPINPF(MNUMCR,CURIYR) + &
                      QLUIN(MNUMCR,CURIYR) + QOTIN(MNUMCR,CURIYR) + &
                      QAGTR(MNUMCR,CURIYR) + QLUTR(MNUMCR,CURIYR) + &
                                         QKSAS(11,CURIYR) + &
                      QASIN(11,CURIYR) + QPCAS(11,CURIYR) + QSGIN(11,CURIYR)) / &
                     (QMGAS(11,CURIYR)/CFMGQ(CURIYR) + &
                      QJFTR(11,CURIYR)/CFJFQ(CURIYR) + QDSAS(11,CURIYR)/CFDSQT(CURIYR) + &
                      QRLAS(11,CURIYR)/CFRSQ + QRHAS(11,CURIYR)/CFRSQ + &
!                       QOTAS(11,CURIYR)/CFOTQ(CURIYR) +
                     (QPPIN(MNUMCR,CURIYR)+QPPINPF(MNUMCR,CURIYR))/CFPPQ + &
                      QOTIN(MNUMCR,CURIYR)/CFOTQ(CURIYR) + &
                      QLUIN(MNUMCR,CURIYR)/CFLUQ + QLUTR(MNUMCR,CURIYR)/CFLUQ + &
                      QAGTR(MNUMCR,CURIYR)/CFAVQ + QKSAS(11,CURIYR)/CFKSQ + &
                      QLGAS(11,CURIYR)/CFLGQ(CURIYR) + QPCAS(11,CURIYR)/CFPCQ + &
                      QPFIN(11,CURIYR)/CFPFQ(CURIYR) + QASIN(11,CURIYR)/CFASQ + &
                      QSGIN(11,CURIYR)/CFSGQ)
      IF (DBDUMP .EQ. 1) &
          WRITE(ICNVRTOUT,'(A,I2,A,F6.3)')  &
        " Petroleum conversion factor after iteration ",CURITR," is ",CFTPQ(CURIYR)

         QPRDEX(30,CURIYR) = sum(QPRDEX(1:29,CURIYR))
! Fischer-Tropsch conversion factors
!  GTL:
      IF (sum(GTLFRAC(1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFGTLLIQ(CURIYR) =(GTLFRAC(1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                            GTLFRAC(2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                            GTLFRAC(3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                            GTLFRAC(4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                        sum(GTLFRAC(1:4,MNUMPR,CURIYR))
      ELSE
         CFGTLLIQ(CURIYR) = CFDSQ
      ENDIF
!  CTL:
      IF (sum(CTLFRAC(1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFCTLLIQ(CURIYR) =(CTLFRAC(1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                            CTLFRAC(2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                            CTLFRAC(3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                            CTLFRAC(4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                        sum(CTLFRAC(1:4,MNUMPR,CURIYR))
      ELSE
         CFCTLLIQ(CURIYR) = CFDSQ
      ENDIF
!  BTL:
      IF (sum(BTLFRAC(1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFBTLLIQ(CURIYR) =(BTLFRAC(1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                            BTLFRAC(2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                            BTLFRAC(3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                            BTLFRAC(4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                        sum(BTLFRAC(1:4,MNUMPR,CURIYR))
      ELSE
         CFBTLLIQ(CURIYR) = CFDSQ
      ENDIF
!  CBTL:
      IF (sum(CBTLFRAC(1,1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFCBTLLIQ(1,CURIYR) =(CBTLFRAC(1,1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                               CBTLFRAC(1,2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                               CBTLFRAC(1,3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                               CBTLFRAC(1,4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                           sum(CBTLFRAC(1,1:4,MNUMPR,CURIYR))
      ELSE
         CFCBTLLIQ(1,CURIYR) = CFDSQ
      ENDIF
      IF (sum(CBTLFRAC(2,1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFCBTLLIQ(2,CURIYR) =(CBTLFRAC(2,1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                               CBTLFRAC(2,2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                               CBTLFRAC(2,3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                               CBTLFRAC(2,4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                           sum(CBTLFRAC(2,1:4,MNUMPR,CURIYR))
      ELSE
         CFCBTLLIQ(2,CURIYR) = CFDSQ
      ENDIF
      IF ((sum(CBTLFRAC(1,:,MNUMPR,CURIYR)) + sum(CBTLFRAC(2,:,MNUMPR,CURIYR))) .NE. 0.0) THEN
          CFCBTLLIQ(3,CURIYR) =(sum(CBTLFRAC(1,:,MNUMPR,CURIYR)) * CFCBTLLIQ(1,CURIYR) + &
                                sum(CBTLFRAC(2,:,MNUMPR,CURIYR)) * CFCBTLLIQ(2,CURIYR)) / &
                               (sum(CBTLFRAC(1,:,MNUMPR,CURIYR)) + sum(CBTLFRAC(2,:,MNUMPR,CURIYR)))
      ELSE
         CFCBTLLIQ(3,CURIYR) = CFDSQ
      ENDIF
! end Fischer-Tropsch conversion factors
      IF ((RFIPQAS(MNUMPR,CURIYR,2) + RFIPQAG(MNUMPR,CURIYR,2) + RFIPQCD(MNUMPR,CURIYR,2) + &
           RFIPQMG(MNUMPR,CURIYR,2) + RFIPQRG(MNUMPR,CURIYR,2) + RFIPQDL(MNUMPR,CURIYR,2) + &
           RFIPQDU(MNUMPR,CURIYR,2) + RFIPQJF(MNUMPR,CURIYR,2) + RFIPQPF(MNUMPR,CURIYR,2) + &
           RFIPQPR(MNUMPR,CURIYR,2) + RFIPQPY(MNUMPR,CURIYR,2) + RFIPQPP(MNUMPR,CURIYR,2) + &
           RFIPQET(MNUMPR,CURIYR,2) + RFIPQBU(MNUMPR,CURIYR,2) + RFIPQIS(MNUMPR,CURIYR,2) + &
           RFIPQLU(MNUMPR,CURIYR,2) + RFIPQDS(MNUMPR,CURIYR,2) + RFIPQRL(MNUMPR,CURIYR,2) + &
           RFIPQRH(MNUMPR,CURIYR,2) + RFIPQPC(MNUMPR,CURIYR,2)) .NE. 0.0) THEN
               CFIMPRD(CURIYR) = ( &
                        RFIPQAS(MNUMPR,CURIYR,2) * CFASQ + &
                        RFIPQAG(MNUMPR,CURIYR,2) * CFAVQ + &
                        RFIPQCD(MNUMPR,CURIYR,2) * CFDSCQ(CURIYR) + &
                        RFIPQMG(MNUMPR,CURIYR,2) * CFTGQ(CURIYR) + &
                        RFIPQRG(MNUMPR,CURIYR,2) * CFRGQ(CURIYR) + &
                        RFIPQDL(MNUMPR,CURIYR,2) * CFDSLQ(CURIYR) + &
                        RFIPQDU(MNUMPR,CURIYR,2) * CFDSUQ(CURIYR) + &
                        RFIPQJF(MNUMPR,CURIYR,2) * CFJFQ(CURIYR) + &
                        RFIPQPF(MNUMPR,CURIYR,2) * CFPFQ(CURIYR) + &
                        RFIPQPR(MNUMPR,CURIYR,2) * CFPRQ + &
                        RFIPQPY(MNUMPR,CURIYR,2) * CFPRQ + &
                        RFIPQET(MNUMPR,CURIYR,2) * CFEEQ + &
                        RFIPQBU(MNUMPR,CURIYR,2) * CFBUQ + &
                        RFIPQIS(MNUMPR,CURIYR,2) * CFIBQ + &
                        RFIPQPP(MNUMPR,CURIYR,2) * CFPPQ + &
                        RFIPQLU(MNUMPR,CURIYR,2) * CFLUQ + &
                        RFIPQDS(MNUMPR,CURIYR,2) * CFDSQ + &
                        RFIPQRL(MNUMPR,CURIYR,2) * CFRSQ + &
                        RFIPQRH(MNUMPR,CURIYR,2) * CFRSQ + &
                        RFIPQPC(MNUMPR,CURIYR,2) * CFPCQ) / &
          (RFIPQAS(MNUMPR,CURIYR,2) + RFIPQAG(MNUMPR,CURIYR,2) + RFIPQCD(MNUMPR,CURIYR,2) + &
           RFIPQMG(MNUMPR,CURIYR,2) + RFIPQRG(MNUMPR,CURIYR,2) + RFIPQDL(MNUMPR,CURIYR,2) + &
           RFIPQDU(MNUMPR,CURIYR,2) + RFIPQJF(MNUMPR,CURIYR,2) + RFIPQPF(MNUMPR,CURIYR,2) + &
           RFIPQPR(MNUMPR,CURIYR,2) + RFIPQPY(MNUMPR,CURIYR,2) + RFIPQPP(MNUMPR,CURIYR,2) + &
           RFIPQET(MNUMPR,CURIYR,2) + RFIPQBU(MNUMPR,CURIYR,2) + RFIPQIS(MNUMPR,CURIYR,2) + &
           RFIPQLU(MNUMPR,CURIYR,2) + RFIPQDS(MNUMPR,CURIYR,2) + &
           RFIPQRL(MNUMPR,CURIYR,2) + RFIPQRH(MNUMPR,CURIYR,2) + RFIPQPC(MNUMPR,CURIYR,2))
      ELSE
               CFIMPRD(CURIYR) = 5.8
      ENDIF
      IF (QPRDEX(30,CURIYR) .NE. 0.0) THEN
         CFEXPRD(CURIYR) = ( &
               QPRDEX( 1,CURIYR) * CFPRQ + &            ! use straight propane factor
               QPRDEX( 2,CURIYR) * CFTGQ(CURIYR) + &
               QPRDEX( 3,CURIYR) * CFRGQ(CURIYR) + &
               QPRDEX( 4,CURIYR) * CFTGQ(CURIYR) + &
               QPRDEX( 5,CURIYR) * CFRGQ(CURIYR) + &
               QPRDEX( 6,CURIYR) * CFJFQ(CURIYR) + &
               QPRDEX( 7,CURIYR) * CFDSQ + &
               QPRDEX( 8,CURIYR) * CFRSQ + &
               QPRDEX( 9,CURIYR) * CFRSQ + &
               QPRDEX(10,CURIYR) * CFGO3(CURIYR) + &
               QPRDEX(11,CURIYR) * CFPFQ(CURIYR) + &
               QPRDEX(12,CURIYR) * CFASQ + &
               QPRDEX(13,CURIYR) * CFDSLQ(CURIYR) + &
               QPRDEX(14,CURIYR) * CFPRQ + &
               QPRDEX(15,CURIYR) * CFBUQ + &
               QPRDEX(16,CURIYR) * CFPCQ + &
               QPRDEX(17,CURIYR) * CFE85Q(CURIYR) + &
               QPRDEX(18,CURIYR) * CFAVQ + &
               QPRDEX(19,CURIYR) * CFLUQ + &
               QPRDEX(20,CURIYR) * CFAR3(CURIYR) + &
               QPRDEX(21,CURIYR) * CFMN3(CURIYR) + &
               QPRDEX(22,CURIYR) * CFMEQT + &
               QPRDEX(23,CURIYR) * CFGOP(CURIYR) + &
               QPRDEX(24,CURIYR) * CFDSUQ(CURIYR) + &
               QPRDEX(25,CURIYR) * CFDSCQ(CURIYR) + &   !  CarbDSUout; next (26) is CaRBOBout
               QPRDEX(26,CURIYR) * CFRGQ(CURIYR) + &
               QPRDEX(27,CURIYR) * CFEEQ + &
               QPRDEX(28,CURIYR) * CFIBQ + &
               QPRDEX(29,CURIYR) * CFPPQ) / QPRDEX(30,CURIYR)
      ELSE
         CFEXPRD(CURIYR) = 5.8
      ENDIF

      RETURN
      END SUBROUTINE DOCVFACTS
!*******************************************************************
!* Check_for_NaNq
!*
!* Check's convergence variables for NaNq.  Returns 1 if found.
!*******************************************************************
      SUBROUTINE Check_for_NaNq(inanq)
      IMPLICIT NONE

      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'uefdout'
      include 'ngtdmout'
      include 'cdsparms'
      include 'coalemm'
      include 'coalprc'
      include 'pqchar'
      include 'intout'
      include 'macout'
      include 'pmmrpt'
      include 'emission'
      include 'uso2grp'
      integer ivar,ireg,inanq
      inanq=0 ! initialize to false

! Basic PQ variables
      do ivar=1,mnump
        do ireg=1,mnumcr
          call write_NaNq(mprc(ireg,curiyr,ivar),curiyr,curitr,ireg,mpvars(ivar),inanq)
        enddo
      enddo
      do ivar=1,mnumq
        do ireg=1,mnumcr
          call write_NaNq(mqty(ireg,curiyr,ivar),curiyr,curitr,ireg,mqvars(ivar),inanq)
        enddo
      enddo

! Utility <---> Original Gas variables (firm/itnteruptible/competitive), plus new firm/interuptible variables for two seasons
! original (obsolete)
      do ivar=1,3
        do ireg=1,nngem
           call write_NaNq(muprc(ireg,curiyr,ivar),curiyr,curitr,ireg,mupvars(ivar),inanq)
        enddo
      enddo

      do ivar=1,3
        do ireg=1,nngem
          call write_NaNq(muqty(ireg,curiyr,ivar),curiyr,curitr,ireg,muqvars(ivar),inanq)
        enddo
      enddo
! newer
      do ivar=1,4
        do ireg=1,nngem
           call write_NaNq(smuprc(ireg,curiyr,ivar),curiyr,curitr,ireg,smupvars(ivar),inanq)
        enddo
      enddo

      do ivar=1,4
        do ireg=1,nngem
           call write_NaNq(smuqty(ireg,curiyr,ivar),curiyr,curitr,ireg,smuqvars(ivar),inanq)
        enddo
      enddo


! Utility <---> Coal variables
      do ivar=1,2
        do ireg=1,NDRG2
          call write_NaNq(pclelcdr(ivar,ireg,curiyr),curiyr,curitr,ireg,mcpvarss,inanq)
        enddo
      enddo

      do ivar=1,NCLUT1
        do ireg=1,NDRGG
          call write_NaNq(qclclnr(ireg,curiyr,ivar),curiyr,curitr,ireg,mcqvarss(ivar),inanq)
        enddo
      enddo

      do ivar=1,MNOTH
        ireg=11
        call write_NaNq(convoth(2,ivar,curiyr),curiyr,curitr,ireg,movars(ivar),inanq)
      enddo

      do ireg=1,MX_NCOALS
        call write_NaNq(XCL_PCAP(ireg,curiyr),curiyr,curitr,ireg,mzvars(1),inanq)
      enddo

      do ireg=1,MX_UNITS
        call write_NaNq(EMM_CL_CF(ireg,curiyr),curiyr,curitr,ireg,mzvars(2),inanq)
        call write_NaNq(EMM_CL_BTUs(ireg,curiyr),curiyr,curitr,ireg,mzvars(3),inanq)
      enddo

      RETURN
      END SUBROUTINE Check_for_NaNq
!********************************************************************
! Write_NaNq
! Checks for a nanq and if found, sets inanq to 1 and writes a message
! to standard output with the year, iteration, region, and variable name.
! The intrinsic function ISNAN returns true if the real argument is
! an IEEE NaN value.
!*********************************************************************
      subroutine WRITE_NaNq(Rvalue,curiyr,curitr,ireg,vname,inanq)
      implicit none
      character*(*) vname
      real Rvalue, test/100./, zero/0./,testvalue

      integer curiyr,curitr,ireg ! input arguments for year index, iteration, region
      Integer inanq ! output argument:  1 if nanq detected, unchanged otherwise

      if(isnan(rvalue)) then
         inanq=1
         write(6,100)'NaN in '//trim(vname)//', curiyr/curitr/region=', &
         curiyr,curitr,ireg
100 format(1x,a,3i3)
      endif
      return
      end subroutine WRITE_NaNq
!*******************************************************************
!*  Subroutine NDEBUG(IMODEL,U)
!*
!*  Writes out debug information for the current iteration and year.
!*
!*******************************************************************

      SUBROUTINE NDEBUG(IMODEL,U)

      IMPLICIT NONE
      integer i
      integer nobjects
      character*127 objects(50)
      character*16 omlvers
      character*50 MPS,NEMSDIR
      character*4 Ordinal(16)/' 1st',' 2nd',' 3rd',' 4th',' 5th',' 6th',' 7th',' 8th',  &
                              ' 9th','10th','11th','12th','13th','14th','15th','16th'/
      common/holduser/nobjects,objects,omlvers,MPS,NEMSDIR

      INTEGER IMODEL,U   ! MODEL INDEX AND OUTPUT UNIT NUMBER
      CHARACTER*3 OFFON(0:2)/'Off','On ','On '/
      CHARACTER*2 LBC/'##'/,L1C/'# '/,LB,L1
      include 'parametr'
      include 'ncntrl'
      INTEGER IM
      LB=LBC
      L1=L1C
      IF(U.NE.6) THEN
        LB='  '
        L1='  '
      ENDIF
      IF (IMODEL.EQ.0) THEN
         WRITE(U,*) LB//'NEMS Integrated Run: "',SCEN_DATE,'" -- Control Information'
         WRITE(U,*) L1,'Run Comment:'
         WRITE(U,*) LB,'"',COMMENT,'"'
         WRITE(U,*) L1,'NEMS Submodel Selections for this Run: '
200   FORMAT(1X,A,3X,A,I1,1X,4A,' DEBUG= ',I1,'   Runs ',A5)
         WRITE(U,200) LB,'EXW=RUNMOD( 1)=',EXW,'(',OFFON(EXW),') ',SUBR_DESCR( 1), PRTDBGW,Ordinal(MORDER( 1))
         WRITE(U,200) LB,'EXM=RUNMOD( 2)=',EXM,'(',OFFON(EXM),') ',SUBR_DESCR( 2), PRTDBGM,Ordinal(MORDER( 2))
         WRITE(U,200) LB,'EXR=RUNMOD( 3)=',EXR,'(',OFFON(EXR),') ',SUBR_DESCR( 3), PRTDBGR,Ordinal(MORDER( 3))
         WRITE(U,200) LB,'EXK=RUNMOD( 4)=',EXK,'(',OFFON(EXK),') ',SUBR_DESCR( 4), PRTDBGK,Ordinal(MORDER( 4))
         WRITE(U,200) LB,'EXI=RUNMOD( 5)=',EXI,'(',OFFON(EXI),') ',SUBR_DESCR( 5), PRTDBGI,Ordinal(MORDER( 5))
         WRITE(U,200) LB,'EXT=RUNMOD( 6)=',EXT,'(',OFFON(EXT),') ',SUBR_DESCR( 6), PRTDBGT,Ordinal(MORDER( 6))
         WRITE(U,200) LB,'EXE=RUNMOD( 7)=',EXE,'(',OFFON(EXE),') ',SUBR_DESCR( 7), PRTDBGE,Ordinal(MORDER( 7))
         WRITE(U,200) LB,'EXC=RUNMOD( 8)=',EXC,'(',OFFON(EXC),') ',SUBR_DESCR( 8), PRTDBGC,Ordinal(MORDER( 8))
         WRITE(U,200) LB,'EXL=RUNMOD( 9)=',EXL,'(',OFFON(EXL),') ',SUBR_DESCR( 9), PRTDBGL,Ordinal(MORDER( 9))
         WRITE(U,200) LB,'EXG=RUNMOD(10)=',EXG,'(',OFFON(EXG),') ',SUBR_DESCR(10), PRTDBGG,Ordinal(MORDER(10))
         WRITE(U,200) LB,'EXO=RUNMOD(11)=',EXO,'(',OFFON(EXO),') ',SUBR_DESCR(11), PRTDBGO,Ordinal(MORDER(11))
         WRITE(U,200) LB,'EXN=RUNMOD(12)=',EXN,'(',OFFON(EXN),') ',SUBR_DESCR(12), PRTDBGN,Ordinal(MORDER(12))
         WRITE(U,200) LB,'EXH=RUNMOD(13)=',EXH,'(',OFFON(EXH),') ',SUBR_DESCR(13), PRTDBGH,Ordinal(MORDER(13))

         WRITE(U,*) L1
         WRITE(U,*) L1,'Other User-Specified Control Variables: '
         WRITE(U,100) LB,'NYRS    =',(INT(NYRS)),'Number of growth years, expectations'
         WRITE(U,100) LB,'WWOP    =',WWOP    ,'World Oil Price Case'
         WRITE(U,100) LB,'MMAC    =',MMAC    ,'Macro Case'
         WRITE(U,100) LB,'History =',HISTORY ,'Option to overwrite with SEDS data'
         WRITE(U,100) LB,'I4SITE  =',I4SITE  ,'Foresight is ',FORE_SITE_TYPE(I4SITE)
         WRITE(U,100) LB,'I4SCNT  =',I4SCNT  ,'Foresight Control is by ',FORE_SITE_CNTL(I4SCNT)
         WRITE(U,100) LB,'MAXITR  =',MAXITR  ,'Maximum Iterations'
         WRITE(U,100) LB,'FIRSYR  =',FIRSYR  ,'Index for Start Year'
         WRITE(U,100) LB,'LASTYR  =',LASTYR  ,'Index for Last Year'
         WRITE(U,100) LB,'BASEYR  =',BASEYR  ,'Base year for Year Index'
         WRITE(U,100) LB,'DSMSWTCH=',DSMSWTCH,'DSM Switch (1=Do DSM)'
         WRITE(U,100) LB,'MODELON =',MODELON ,'Models are ',OFFON(MODELON),' after converging'
         WRITE(U,100) LB,'DBDUMP  =',DBDUMP  ,'Write RESTART yearly is ',OFFON(DBDUMP)
         WRITE(U,100) LB,'MACFDBK =',MACFDBK ,'Macroeconomic feedback is ',OFFON(MACFDBK)
         WRITE(U,100) LB,'ECPSTART=',ECPSTART,'EMM capacity planning is ',OFFON(ECPSTART)
         WRITE(U,100) LB,'IRELAX  =',IRELAX  ,'Relaxation is ',OFFON(IRELAX)
         WRITE(U,100) LB,'ITIMNG  =',ITIMNG  ,'CPU time reporting is ',OFFON(ITIMNG)
         write(u,*) ' '
         if(nobjects.gt.0) then
           write(u,*) ' List of NEMS components used in this run'
           do i=1,nobjects
             write(u,'(a)') trim(objects(i))
           enddo
         endif
      ELSE
         WRITE(U,101) LB,curiyr+baseyr-1,'   CURIYR=',CURIYR, &
             ',  CURITR=',CURITR,',  IMODEL=',IMODEL, &
             ',  CNVTST=',CNVTST(IMODEL,CURIYR)
      ENDIF

100   FORMAT(1X,A,1X,A,I7,1X,4A)
101   FORMAT(1X,A,7X,'Model Year=',i4,4(A,I2))
107   FORMAT(1X,A,1X,A,F7.3,1X,2A)
201   FORMAT(1X,A,' Execution order:  ',12(I2,', '),I2)

      RETURN
      END
!*******************************************************************
!*    Subroutine NDATIN
!*
!*    Reads RESTART data file using prototype version of FILER.
!*******************************************************************
      SUBROUTINE NDATIN
      IMPLICIT NONE

! +++ FILER Declarations:
      INTEGER*4 FUNITI,FUNITO,FRETCD,FRTYPE,FSOURC,FUNFMT
      CHARACTER*100 FNAMEI,FNAMEO,FUNCTION*1
      CHARACTER*18 FNAMEI18,FNAMEO18
! +++ End FILER Declarations
      CHARACTER*11 FUNFMTC  ! FROM INQUIRE STATEMENT--USED TO SET FUNFMT

      LOGICAL NEW
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

! +++ Read DICTIONARY
      WRITE(*,*) 'Reading FILER Dictionary'
      FRTYPE=3
      FRETCD=0
      FUNITI=1

! +++ Open File
      FNAMEI18='DICT'
      NEW=.FALSE.
      FUNITI=FILE_MGR('O',FNAMEI18,NEW)  ! OPEN FILE AND GET UNIT #
      FNAMEI= ' '
      FUNITO=6
      FNAMEO=' '
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      FUNITI=FILE_MGR('C',FNAMEI18,NEW)
      WRITE(*,*) ' Filer Dictionary Read.'
      WRITE(*,*) ' Filer Return Code=',FRETCD

! +++ Read the Restart file containing global data and starting solution values
! +++ from a prior run.
      FRTYPE=2
      FSOURC=1
      FUNITI=1
      FNAMEI18='RESTARTI'
      NEW=.FALSE.
      IF (FNAMEI .EQ. ' ') THEN
        WRITE(*,*) 'FILER Input Request From ',FNAMEI18
      ELSE
        WRITE(*,*) 'FILER Input Request From ',FNAMEI
      ENDIF
      FUNITI=FILE_MGR('O',FNAMEI18,NEW)
      FNAMEI=' '                              ! Blank Input file name
                                              ! so FILER won't open it.
      FNAMEO=' '
      FUNFMT=0
! +++ DETERMINE WHETHER FILE WAS OPENED AS FORMATTED OR UNFORMATTED BY FILEMGR AND SET FUNFMT VARIABLE ACCORDINGLY
      INQUIRE(FUNITI,FORM=FUNFMTC)
      IF(FUNFMTC.EQ.'FORMATTED') FUNFMT=0
      IF(FUNFMTC.EQ.'UNFORMATTED') FUNFMT=1

      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      FUNITI=FILE_MGR('C',FNAMEI18,NEW)
      WRITE(*,*) 'FILER Input Request Completed.'

      RETURN
      END

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
!*******************************************************************
!*     Subroutine SEDSHIST
!*
!*     if HISTORY=1, overwrites history quantities with corresponding State Energy
!*     Data System (SEDS) values
!*     for 1990-to the year corresponding to NEMS YEar Index Parameter MSEDYR.
!*     This routine has not been used in the last decade, but archeologists
!*     may have some interest in the future.
!*******************************************************************
      SUBROUTINE SEDSHIST
      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'qsblk'
      INTEGER REG, VAR, YR

      IF(curiyr.le.msedyr.and.history.eq.1) THEN
        yr=curiyr
        DO REG = 1, MNUMCR
          DO VAR = 1,MNUMQ
            IF (MQCEN(REG,YR,VAR) .NE. 0.) THEN
              MQTY(REG,YR,VAR) = MQCEN(REG,YR,VAR)
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      RETURN
      END
!*******************************************************************
!*    Subroutine NINIT
!*
!*    Initialize global QUANTITY and PRICE variables to zero
!*******************************************************************
      SUBROUTINE NINIT
      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'converge' ! for oscor and CVSCOREHIST
      integer i,j

      CALL ZEROR(MQARRY,MQSIZE)
      CALL ZEROR(MPARRY,MPSIZE)

      RETURN
      END
!******************************************************************
!*  Subroutine SUMQAS
!*
!*  Sums NEMS ACROSS-SECTOR (...AS) QUANTITY variables; sums TOTAL
!*  PETROLEUM, TOTAL RENEWABLE, and TOTAL SECTOR QUANTITY variables.
!******************************************************************
      SUBROUTINE SUMQAS
      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'ngtdmrep'
      include 'indrep'
      include 'indout'
      include 'convfact'
      include 'tranrep'
      include 'qsblk'
      include 'calshr'
      include 'qonroad'
      INTEGER IY,IC,IVAR,isedyr

! +++ FIRST sum NEMS QUANTITIES Across Regions for the current year.
      DO IVAR=1,MNUMQ
         CALL SUMARY(MQTY(1,CURIYR,IVAR),MNUMCR)
      ENDDO
! share Pacific (Division 9) to California based on SEDS
      isedyr=min(curiyr,msedyr)
      mqty(10,curiyr,:)=0.  ! initialize all California
      if(curiyr.ge.msedyr) then
        QELRS(10,curiyr) = QELRS(9,curiyr) * ELRS_SHR(curiyr) !  1 Purchased Electricity - Residential
        QELCM(10,curiyr) = QELCM(9,curiyr) * ELCM_SHR(curiyr) !  2 Purchased Electricity - Commercial
        QELTR(10,curiyr) = QELTR(9,curiyr) * ELTR_SHR(curiyr) !  3 Purchased Electricity - Transportation
        QELIN(10,curiyr) = QELIN(9,curiyr) * ELIN_SHR(curiyr) !  4 Purchased Electricity - Industrial
        QELRF(10,curiyr) = QELRF(9,curiyr) * ELRF_SHR(curiyr) !  5 Purchased Electricity - Refinery
        QELHM(10,curiyr) = QELHM(9,curiyr) * ELHM_SHR(curiyr) !  6 Purchased Electricity - Hydrogen

        QNGRS(10,curiyr) = QNGRS(9,curiyr) * NGRS_SHR(curiyr) ! 24 Natural Gas - Residential
        QNGCM(10,curiyr) = QNGCM(9,curiyr) * NGCM_SHR(curiyr) ! 25 Natural Gas - Commercial
        QNGTR(10,curiyr) = QNGTR(9,curiyr) * NGTR_SHR(curiyr) ! 26 Natural Gas - Transportation
        QNGIN(10,curiyr) = QNGIN(9,curiyr) * NGIN_SHR(curiyr) ! 27 Natural Gas - Industrial
        QNGRF(10,curiyr) = QNGRF(9,curiyr) * NGRF_SHR(curiyr) ! 28 Natural Gas - Refinery
        QNGEL(10,curiyr) = QNGEL(9,curiyr) * NGEL_SHR(curiyr) ! 29 Natural Gas - Electricity
        QNGHM(10,curiyr) = QNGHM(9,curiyr) * NGHM_SHR(curiyr) ! 30 Natural Gas - Hydrogen
        QGPTR(10,curiyr) = QGPTR(9,curiyr) * GPTR_SHR(curiyr) ! 32 Natural Gas - Pipeline
        QLPIN(10,curiyr) = QLPIN(9,curiyr) * LPIN_SHR(curiyr) ! 33 Natural Gas - Lease and Plant Fuel
        QNGLQ(10,curiyr) = QNGLQ(9,curiyr) * LPIN_SHR(curiyr) !    Natural Gas - Liquefaction

        QCLRS(10,curiyr) = QCLRS(9,curiyr) * CLRS_SHR(curiyr) ! 34 Coal - Residential
        QCLCM(10,curiyr) = QCLCM(9,curiyr) * CLCM_SHR(curiyr) ! 35 Coal - Commercial
        QCLIN(10,curiyr) = QCLIN(9,curiyr) * CLIN_SHR(curiyr) ! 36 Coal - Industrial
        QCLRF(10,curiyr) = QCLRF(9,curiyr) * CLRF_SHR(curiyr) ! 37 Coal - Refinery
        QCLEL(10,curiyr) = QCLEL(9,curiyr) * CLEL_SHR(curiyr) ! 38 Coal - Electricity
        QCLSN(10,curiyr) = QCLSN(9,curiyr) * CLSN_SHR(curiyr) ! 39 Coal - Synthetics
        QCLHM(10,curiyr) = QCLHM(9,curiyr) * CLHM_SHR(curiyr) ! 40 Coal - Hydrogen

        QMCIN(10,curiyr) = QMCIN(9,curiyr) * MCIN_SHR(curiyr) ! 42 Metallurgical Coal - Industrial
        QMGCM(10,curiyr) = QMGCM(9,curiyr) * MGCM_SHR(curiyr) ! 43 Motor Gasoline - Commercial
        QMGTR(10,curiyr) = QMGTR(9,curiyr) * MGTR_SHR(curiyr) ! 44 Motor Gasoline - Transportation
        QMGIN(10,curiyr) = QMGIN(9,curiyr) * MGIN_SHR(curiyr) ! 45 Motor Gasoline - Industrial

        QJFTR(10,curiyr) = QJFTR(9,curiyr) * JFTR_SHR(curiyr) ! 47 Jet Fuel - Transportation
        QDSRS(10,curiyr) = QDSRS(9,curiyr) * DSRS_SHR(curiyr) ! 48 Distillate - Residential
        QDSCM(10,curiyr) = QDSCM(9,curiyr) * DSCM_SHR(curiyr) ! 49 Distillate - Commercial
        QDSTR(10,curiyr) = QDSTR(9,curiyr) * DSTR_SHR(curiyr) ! 50 Distillate - Transportation
        QDSIN(10,curiyr) = QDSIN(9,curiyr) * DSIN_SHR(curiyr) ! 51 Distillate - Industrial
        QDSRF(10,curiyr) = QDSRF(9,curiyr) * DSRF_SHR(curiyr) ! 52 Distillate - Refinery
        QDSEL(10,curiyr) = QDSEL(9,curiyr) * DSEL_SHR(curiyr) ! 53 Distillate - Electricity (+petroleum coke)

        QKSRS(10,curiyr) = QKSRS(9,curiyr) * KSRS_SHR(curiyr) ! 55 Kerosene - Residential
        QKSCM(10,curiyr) = QKSCM(9,curiyr) * KSCM_SHR(curiyr) ! 56 Kerosene - Commercial
        QKSIN(10,curiyr) = QKSIN(9,curiyr) * KSIN_SHR(curiyr) ! 57 Kerosene - Industrial

        QLGRS(10,curiyr) = QLGRS(9,curiyr) * LGRS_SHR(curiyr) ! 59 Liquid Petroleum Gases - Residential
        QLGCM(10,curiyr) = QLGCM(9,curiyr) * LGCM_SHR(curiyr) ! 60 Liquid Petroleum Gases - Commercial
        QLGTR(10,curiyr) = QLGTR(9,curiyr) * LGTR_SHR(curiyr) ! 61 Liquid Petroleum Gases - Transportation
        QLGIN(10,curiyr) = QLGIN(9,curiyr) * LGIN_SHR(curiyr) ! 62 Liquid Petroleum Gases - Industrial
        QLGRF(10,curiyr) = QLGRF(9,curiyr) * LGRF_SHR(curiyr) ! 63 Liquid Petroleum Gases - Refinery

        QRSCM(10,curiyr) = QRSCM(9,curiyr) * RSCM_SHR(curiyr) ! 74 Residual Fuel - Commercial
        QRSTR(10,curiyr) = QRSTR(9,curiyr) * RSTR_SHR(curiyr) ! 75 Residual Fuel - Transportation
        QRSIN(10,curiyr) = QRSIN(9,curiyr) * RSIN_SHR(curiyr) ! 76 Residual Fuel - Industrial
        QRSRF(10,curiyr) = QRSRF(9,curiyr) * RSRF_SHR(curiyr) ! 77 Residual Fuel - Refinery
        QRSEL(10,curiyr) = QRSEL(9,curiyr) * RSEL_SHR(curiyr) ! 78 Residual Fuel - Electricity

        QPFIN(10,curiyr) = QPFIN(9,curiyr) * PFIN_SHR(curiyr) ! 80 Petrochemical Feedstocks - Industrial
        QSGIN(10,curiyr) = QSGIN(9,curiyr) * SGIN_SHR(curiyr) ! 81 Still Gas - Industrial
        QSGRF(10,curiyr) = QSGRF(9,curiyr) * SGRF_SHR(curiyr) ! 82 Still Gas - Refinery
        QPCIN(10,curiyr) = QPCIN(9,curiyr) * PCIN_SHR(curiyr) ! 83 Petroleum Coke - Industrial
        QPCRF(10,curiyr) = QPCRF(9,curiyr) * PCRF_SHR(curiyr) ! 84 Petroleum Coke - Refinery
        QPCEL(10,curiyr) = QPCEL(9,curiyr) * PCEL_SHR(curiyr) ! 85 Petroleum Coke - Electricity
        QPCAS(10,curiyr) = QPCAS(9,curiyr) * PCAS_SHR(curiyr) ! 86 Petroleum Coke - All Sectors
        QASIN(10,curiyr) = QASIN(9,curiyr) * ASIN_SHR(curiyr) ! 87 Asphalt and Road Oil - Industrial
        QOTTR(10,curiyr) = QOTTR(9,curiyr) * OTTR_SHR(curiyr) ! 88 Other Petroleum - Transportation
        QOTIN(10,curiyr) = QOTIN(9,curiyr) * OTIN_SHR(curiyr) ! 89 Other Petroleum - Industrial
        QOTRF(10,curiyr) = QOTRF(9,curiyr) * OTRF_SHR(curiyr) ! 90 Other Petroleum - Refinery

        QMETR(10,curiyr) = QMETR(9,curiyr) * METR_SHR(curiyr) ! 99 Methanol - Transporation
        QETTR(10,curiyr) = QETTR(9,curiyr) * ETTR_SHR(curiyr) !100 Ethanol - Transporation
        QETHM(10,curiyr) = QETHM(9,curiyr) * ETHM_SHR(curiyr) !101 Ethanol - Hydrogen
        QHYTR(10,curiyr) = QHYTR(9,curiyr) * HYTR_SHR(curiyr) !102 Liquid Hydrogen - Transportation
        QUREL(10,curiyr) = QUREL(9,curiyr) * UREL_SHR(curiyr) !103 Uranium - Electricity
        QURHM(10,curiyr) = QURHM(9,curiyr) * URHM_SHR(curiyr) !104 Uranium - Hydrogen
        QHOIN(10,curiyr) = QHOIN(9,curiyr) * HOIN_SHR(curiyr) !105 Hydropower - Industrial
        QHOEL(10,curiyr) = QHOEL(9,curiyr) * HOEL_SHR(curiyr) !106 Hydropower - Electricity

        QGERS(10,curiyr) = QGERS(9,curiyr) * GERS_SHR(curiyr) !108 Geothermal - Residential
        QGEIN(10,curiyr) = QGEIN(9,curiyr) * GEIN_SHR(curiyr) !109 Geothermal - Industrial
        QGEEL(10,curiyr) = QGEEL(9,curiyr) * GEEL_SHR(curiyr) !110 Geothermal - Electricity

        QBMRS(10,curiyr) = QBMRS(9,curiyr) * BMRS_SHR(curiyr) !112 Biomass - Residential
        QBMCM(10,curiyr) = QBMCM(9,curiyr) * BMCM_SHR(curiyr) !113 Biomass - Commercial
        QBMIN(10,curiyr) = QBMIN(9,curiyr) * BMIN_SHR(curiyr) !114 Biomass - Industrial
        QBMRF(10,curiyr) = QBMRF(9,curiyr) * BMRF_SHR(curiyr) !115 Biomass - Refinery
        QBMEL(10,curiyr) = QBMEL(9,curiyr) * BMEL_SHR(curiyr) !116 Biomass - Electricity
        QBMSN(10,curiyr) = QBMSN(9,curiyr) * BMSN_SHR(curiyr) !117 Biomass - Synthetics
        QBMHM(10,curiyr) = QBMHM(9,curiyr) * BMHM_SHR(curiyr) !118 Biomass - Hydrogen

        QMSIN(10,curiyr) = QMSIN(9,curiyr) * MSIN_SHR(curiyr) !120 Municipal Solid Waste - Industrial
        QMSEL(10,curiyr) = QMSEL(9,curiyr) * MSEL_SHR(curiyr) !121 Municipal Solid Waste - Electricity

        QSTRS(10,curiyr) = QSTRS(9,curiyr) * STRS_SHR(curiyr) !123 Solar Thermal - Residential
        QSTCM(10,curiyr) = QSTCM(9,curiyr) * STCM_SHR(curiyr) !124 Solar Thermal - Commercial
        QSTIN(10,curiyr) = QSTIN(9,curiyr) * STIN_SHR(curiyr) !125 Solar Thermal - Industrial
        QSTEL(10,curiyr) = QSTEL(9,curiyr) * STEL_SHR(curiyr) !126 Solar Thermal - Electricity

        QPVRS(10,curiyr) = QPVRS(9,curiyr) * PVRS_SHR(curiyr) !128 Photovoltaic - Residential
        QPVCM(10,curiyr) = QPVCM(9,curiyr) * PVCM_SHR(curiyr) !129 Photovoltaic - Commercial
        QPVIN(10,curiyr) = QPVIN(9,curiyr) * PVIN_SHR(curiyr) !130 Photovoltaic - Industrial
        QPVEL(10,curiyr) = QPVEL(9,curiyr) * PVEL_SHR(curiyr) !131 Photovoltaic - Electricity

        QWIIN(10,curiyr) = QWIIN(9,curiyr) * WIIN_SHR(curiyr) !133 Wind - Industrial
        QWIEL(10,curiyr) = QWIEL(9,curiyr) * WIEL_SHR(curiyr) !134 Wind - Electricity

        QEIEL(10,curiyr) = QEIEL(9,curiyr) * EIEL_SHR(curiyr) !144 Net Electricity Imports - Electricity
        QCIIN(10,curiyr) = QCIIN(9,curiyr) * CIIN_SHR(curiyr) !145 Net Coal Coke Imports - Industrial

      endif
      DO 20 IY=1,MNUMYR
         QPRRS(MNUMCR,IY) = 0.0
         QPRCM(MNUMCR,IY) = 0.0
         QPRTR(MNUMCR,IY) = 0.0
         QETIN(MNUMCR,IY) = 0.0
         QBUIN(MNUMCR,IY) = 0.0
         QPRIN(MNUMCR,IY) = 0.0
         QISIN(MNUMCR,IY) = 0.0
         QETINPF(MNUMCR,IY) = 0.0
         QBUINPF(MNUMCR,IY) = 0.0
         QPRINPF(MNUMCR,IY) = 0.0
         QISINPF(MNUMCR,IY) = 0.0
         INQLGHP(MNUMCR,IY) = 0.0
         INQLGPF(MNUMCR,IY) = 0.0
         QPROLENERF(MNUMCR,IY) = 0.0
         QAGTR(MNUMCR,IY) = 0.0
         QLUTR(MNUMCR,IY) = 0.0
         QPPIN(MNUMCR,IY) = 0.0
         QPPINPF(MNUMCR,IY) = 0.0
         QLUIN(MNUMCR,IY) = 0.0
         QLGIN(MNUMCR,IY) = 0.0
         QMGBS(MNUMCR,IY) = 0.0
         QJFBS(MNUMCR,IY) = 0.0
         QDSBS(MNUMCR,IY) = 0.0
         QDSTRHWY(MNUMCR,IY) = 0.0
         DO IC=1,MNUMCR-2
            QPRRS(MNUMCR,IY) = QPRRS(MNUMCR,IY) + QPRRS(IC,IY)
            QPRCM(MNUMCR,IY) = QPRCM(MNUMCR,IY) + QPRCM(IC,IY)
            QPRTR(MNUMCR,IY) = QPRTR(MNUMCR,IY) + QPRTR(IC,IY)
            QETIN(MNUMCR,IY) = QETIN(MNUMCR,IY) + QETIN(IC,IY)
            QBUIN(MNUMCR,IY) = QBUIN(MNUMCR,IY) + QBUIN(IC,IY)
            QPRIN(MNUMCR,IY) = QPRIN(MNUMCR,IY) + QPRIN(IC,IY)
            QISIN(MNUMCR,IY) = QISIN(MNUMCR,IY) + QISIN(IC,IY)
            QETINPF(MNUMCR,IY) = QETINPF(MNUMCR,IY) + QETINPF(IC,IY)
            QBUINPF(MNUMCR,IY) = QBUINPF(MNUMCR,IY) + QBUINPF(IC,IY)
            QPRINPF(MNUMCR,IY) = QPRINPF(MNUMCR,IY) + QPRINPF(IC,IY)
            QISINPF(MNUMCR,IY) = QISINPF(MNUMCR,IY) + QISINPF(IC,IY)
            INQLGHP(MNUMCR,IY) = INQLGHP(MNUMCR,IY) + INQLGHP(IC,IY)
            INQLGPF(MNUMCR,IY) = INQLGPF(MNUMCR,IY) + INQLGPF(IC,IY)
            QPROLENERF(MNUMCR,IY) = QPROLENERF(MNUMCR,IY) + QPROLENERF(IC,IY)
            QLGIN(MNUMCR,IY) = QLGIN(MNUMCR,IY) + QLGIN(IC,IY)
            QAGTR(MNUMCR,IY) = QAGTR(MNUMCR,IY) + QAGTR(IC,IY)
            QLUTR(MNUMCR,IY) = QLUTR(MNUMCR,IY) + QLUTR(IC,IY)
            QPPIN(MNUMCR,IY) = QPPIN(MNUMCR,IY) + QPPIN(IC,IY)
            QPPINPF(MNUMCR,IY) = QPPINPF(MNUMCR,IY) + QPPINPF(IC,IY)
            QLUIN(MNUMCR,IY) = QLUIN(MNUMCR,IY) + QLUIN(IC,IY)
            QMGBS(MNUMCR,IY) = QMGBS(MNUMCR,IY) + QMGBS(IC,IY)
            QJFBS(MNUMCR,IY) = QJFBS(MNUMCR,IY) + QJFBS(IC,IY)
            QDSBS(MNUMCR,IY) = QDSBS(MNUMCR,IY) + QDSBS(IC,IY)
            QDSTRHWY(MNUMCR,IY) = QDSTRHWY(MNUMCR,IY) + QDSTRHWY(IC,IY)
         ENDDO
         DO 10 IC=1,MNUMCR
            QELAS(IC,IY)=QELRS(IC,IY)+QELCM(IC,IY)+QELTR(IC,IY)+QELIN(IC,IY)+QELHM(IC,IY)
            QGFAS(IC,IY)=QGFRS(IC,IY)+QGFCM(IC,IY)+QGFTR(IC,IY)+QGFIN(IC,IY)+QGFEL(IC,IY)+QGFHM(IC,IY)
            QGIAS(IC,IY)=QGIRS(IC,IY)+QGICM(IC,IY)+QGITR(IC,IY)+QGIIN(IC,IY)+QGIEL(IC,IY)+QGIHM(IC,IY)
            QNGAS(IC,IY)=QNGRS(IC,IY)+QNGCM(IC,IY)+QNGTR(IC,IY)+QNGIN(IC,IY)+QNGEL(IC,IY)+QNGHM(IC,IY)
            QCLAS(IC,IY)=QCLRS(IC,IY)+QCLCM(IC,IY)+QCLIN(IC,IY)+QCLEL(IC,IY)+QCLHM(IC,IY)
            QMGAS(IC,IY)=QMGCM(IC,IY)+QMGTR(IC,IY)+QMGIN(IC,IY)
            QDSAS(IC,IY)=QDSRS(IC,IY)+QDSCM(IC,IY)+QDSTR(IC,IY)+QDSIN(IC,IY)+QDSEL(IC,IY)
            QKSAS(IC,IY)=QKSRS(IC,IY)+QKSCM(IC,IY)+QKSIN(IC,IY)
            QLGAS(IC,IY)=QLGRS(IC,IY)+QLGCM(IC,IY)+QLGTR(IC,IY)+QLGIN(IC,IY)
            QRLAS(IC,IY)=QRLCM(IC,IY)+QRLTR(IC,IY)+QRLIN(IC,IY)+QRLEL(IC,IY)
            QRHAS(IC,IY)=QRHTR(IC,IY)+QRHEL(IC,IY)
!            QRSCM(IC,IY)=QRLCM(IC,IY)
!            QRSIN(IC,IY)=QRLIN(IC,IY)
            QRSTR(IC,IY)=QRLTR(IC,IY)+QRHTR(IC,IY)
            QRSEL(IC,IY)=QRLEL(IC,IY)+QRHEL(IC,IY)
            QRSAS(IC,IY)=QRSCM(IC,IY)+QRSTR(IC,IY)+QRSIN(IC,IY)+QRSEL(IC,IY)
            QPCAS(IC,IY)=QPCIN(IC,IY)+QPCEL(IC,IY)
            QOTAS(IC,IY)=QOTTR(IC,IY)+QOTIN(IC,IY)
            QHOAS(IC,IY)=QHOIN(IC,IY)+QHOEL(IC,IY)
            QGEAS(IC,IY)=QGEIN(IC,IY)+QGEEL(IC,IY)
            QBMAS(IC,IY)=QBMRS(IC,IY)+QBMCM(IC,IY)+QBMIN(IC,IY)+QBMEL(IC,IY)+QBMSN(IC,IY)+QBMHM(IC,IY)
            QMSAS(IC,IY)=QMSIN(IC,IY)+QMSEL(IC,IY)
            QSTAS(IC,IY)=QSTRS(IC,IY)+QSTCM(IC,IY)+QSTIN(IC,IY)+QSTEL(IC,IY)
            QPVAS(IC,IY)=QPVRS(IC,IY)+QPVCM(IC,IY)+QPVIN(IC,IY)+QPVEL(IC,IY)
            QWIAS(IC,IY)=QWIIN(IC,IY)+QWIEL(IC,IY)

! +++ Total Residential:
            QTPRS(IC,IY)=QDSRS(IC,IY)+QKSRS(IC,IY)+QLGRS(IC,IY)
            QTRRS(IC,IY)=QBMRS(IC,IY)+QSTRS(IC,IY)+QPVRS(IC,IY)+QGERS(IC,IY)
            QTSRS(IC,IY)=QELRS(IC,IY)+QNGRS(IC,IY)+QCLRS(IC,IY)+QTPRS(IC,IY)+QTRRS(IC,IY)

! +++ Total Commercial:
            QTPCM(IC,IY)=QMGCM(IC,IY)+QDSCM(IC,IY)+QKSCM(IC,IY)+QLGCM(IC,IY)+QRSCM(IC,IY)
            QTRCM(IC,IY)=QBMCM(IC,IY)+QSTCM(IC,IY)+QPVCM(IC,IY)
            QTSCM(IC,IY)=QELCM(IC,IY)+QNGCM(IC,IY)+QCLCM(IC,IY)+QTPCM(IC,IY)+QTRCM(IC,IY)

! +++ Total Transportation:
            QTPTR(IC,IY)=QMGTR(IC,IY)+QJFTR(IC,IY)+QDSTR(IC,IY)+QLGTR(IC,IY)+QRSTR(IC,IY)+QOTTR(IC,IY)
            QTRTR(IC,IY)=QETTR(IC,IY)
            QTSTR(IC,IY)=QELTR(IC,IY)+QNGTR(IC,IY)+QGPTR(IC,IY)+QTPTR(IC,IY)+ &
                         QMETR(IC,IY)+QETTR(IC,IY)+QHYTR(IC,IY)

! +++ Total Refinery:
            QTPRF(IC,IY)=QDSRF(IC,IY)+QLGRF(IC,IY)+QRSRF(IC,IY)+QSGRF(IC,IY)+QPCRF(IC,IY)+QCCRF(IC,IY)+QOTRF(IC,IY)
            QTSRF(IC,IY)=QELRF(IC,IY)+QNGRF(IC,IY)+QCLRF(IC,IY)+QTPRF(IC,IY)+QBMRF(IC,IY)

! +++ Total Industrial:
            QTPIN(IC,IY)=QMGIN(IC,IY)+QDSIN(IC,IY)+QKSIN(IC,IY)+QLGIN(IC,IY)+QRSIN(IC,IY)+ &
                         QPFIN(IC,IY)+QSGIN(IC,IY)+QPCIN(IC,IY)+QASIN(IC,IY)+QOTIN(IC,IY)
            QTRIN(IC,IY)=QHOIN(IC,IY)+QGEIN(IC,IY)+QBMIN(IC,IY)+QMSIN(IC,IY)+QSTIN(IC,IY)+ &
                         QPVIN(IC,IY)+QWIIN(IC,IY)
            QTSIN(IC,IY)=QELIN(IC,IY)+QNGIN(IC,IY)+QLPIN(IC,IY)+QNGLQ(IC,IY)+  &
                         QCLIN(IC,IY)+QMCIN(IC,IY)+ &
                         QCIIN(IC,IY)+QTPIN(IC,IY)+QTRIN(IC,IY) - OGSUPGAS(1,IC,IY) * CFNGC(IY)

! +++ Total Electric Utility:
            QTPEL(IC,IY)=QDSEL(IC,IY)+QRSEL(IC,IY)+QPCEL(IC,IY)
            QTREL(IC,IY)=QHOEL(IC,IY)+QGEEL(IC,IY)+QBMEL(IC,IY)+QMSEL(IC,IY)+QSTEL(IC,IY)+ &
                         QPVEL(IC,IY)+QWIEL(IC,IY)
            QTSEL(IC,IY)=QNGEL(IC,IY)+QCLEL(IC,IY)+QTPEL(IC,IY)+QUREL(IC,IY)+QTREL(IC,IY)+QEIEL(IC,IY)

! +++ Total Hydrogen Conversion:
            QTRHM(IC,IY)=QETHM(IC,IY)+QBMHM(IC,IY)
            QTSHM(IC,IY)=QELHM(IC,IY)+QNGHM(IC,IY)+QCLHM(IC,IY)+QETHM(IC,IY)+QURHM(IC,IY)+QBMHM(IC,IY)

! +++ Total Synthetics:
            QTRSN(IC,IY)=QBMSN(IC,IY)
            QTSSN(IC,IY)=QTRSN(IC,IY)

! +++ Sum ALL-SECTOR variables for TOTAL PETROLEUM, TOTAL RENEWABLE, and TOTAL SECTOR
            QTPAS(IC,IY)=QTPRS(IC,IY)+QTPCM(IC,IY)+QTPTR(IC,IY)+QTPIN(IC,IY)+QTPEL(IC,IY)
            QTRAS(IC,IY)=QTRRS(IC,IY)+QTRCM(IC,IY)+QTRTR(IC,IY)+QTRIN(IC,IY)+QTREL(IC,IY)+QTRSN(IC,IY)
            QTSAS(IC,IY)=QTSRS(IC,IY)+QTSCM(IC,IY)+QTSTR(IC,IY)+QTSIN(IC,IY)+QTSEL(IC,IY)+ &
                         QTSSN(IC,IY)-QELAS(IC,IY)-QHYTR(IC,IY)
10       CONTINUE
20    CONTINUE

! +++ Sum NEMS QUANTITIES Across Regions for the current year.
      DO IVAR=1,MNUMQ
         CALL SUMARY(MQTY(1,CURIYR,IVAR),MNUMCR)
      ENDDO

      RETURN
      END
!******************************************************************
!*  Subroutine SUMARY(ARRAY,N)
!*
!*  Sums the N-1 elements of an array into a total stored as the nth
!*  element.  The length of the array is set by the calling program.
!******************************************************************
      SUBROUTINE SUMARY(ARRAY,N)
      IMPLICIT NONE

      INTEGER N,I
      REAL ARRAY(N)

      ARRAY(N)=0.
      DO 10 I=1,N-2
         ARRAY(N)=ARRAY(N)+ARRAY(I)
10    CONTINUE

      RETURN
      END
!******************************************************************
!*    Subroutine AVEPAS
!*
!*    Averages prices across sector and region
!******************************************************************
      SUBROUTINE AVEPAS
      IMPLICIT NONE

      include 'parametr'
      include 'qblk'
      include 'mpblk'
      include 'ncntrl'
      include 'cdsparms'
      include 'coalout'
      include 'indout'
      include 'ponroad'
      include 'qonroad'
      include 'ngtdmout'

      INTEGER IY          ! INDEX FOR YEAR
      INTEGER IC          ! INDEX FOR CENSUS REGION
      INTEGER IV,IP,IPRC  ! INDICES FOR PRICE VARIABLE
      INTEGER IQ,IQTY     ! INDEX FOR QUANTITY VARIABLE
      INTEGER NUMPAS,IR
      EXTERNAL NUMP_AS
      INTEGER NUMP_AS
      EXTERNAL PQMATCH
      INTEGER PQMATCH

      IY = CURIYR
! temporarily set PCLGAS here until it is set for real somewhere
      PCLGAS(:,IY) = PCLEL(:,IY)

      INQLGPF(MNUMCR,CURIYR) = 0.0
      PLGINPF(MNUMCR,CURIYR) = 0.0
      QCLGAS(MNUMCR,CURIYR) = 0.0
      PCLGAS(MNUMCR,CURIYR) = 0.0
      PDSTRHWY(MNUMCR,CURIYR) = 0.0
      QGFTRFV(MNUMCR,CURIYR) = 0.0
      QGFTRPV(MNUMCR,CURIYR) = 0.0
      QGLTRFV(MNUMCR,CURIYR) = 0.0
      QGLTRPV(MNUMCR,CURIYR) = 0.0
      QGFTRRAIL(:,MNUMCR,CURIYR) = 0.0
      QGLTRRAIL(:,MNUMCR,CURIYR) = 0.0
      QGFTRSHIP(:,MNUMCR,CURIYR) = 0.0
      QGLTRSHIP(:,MNUMCR,CURIYR) = 0.0
      PGFTRFV(MNUMCR,CURIYR) = 0.0
      PGFTRPV(MNUMCR,CURIYR) = 0.0
      PGLTRFV(MNUMCR,CURIYR) = 0.0
      PGLTRPV(MNUMCR,CURIYR) = 0.0
      PGFTRRAIL(:,MNUMCR,CURIYR) = 0.0
      PGLTRRAIL(:,MNUMCR,CURIYR) = 0.0
      PGFTRSHIP(:,MNUMCR,CURIYR) = 0.0
      PGLTRSHIP(:,MNUMCR,CURIYR) = 0.0
      DO IC=1,MNUMCR-2
         INQLGPF(MNUMCR,CURIYR) = INQLGPF(MNUMCR,CURIYR) + INQLGPF(IC,CURIYR)
         PLGINPF(MNUMCR,CURIYR) = PLGINPF(MNUMCR,CURIYR) + PLGINPF(IC,CURIYR) * INQLGPF(IC,CURIYR)
         QCLGAS(MNUMCR,CURIYR) = QCLGAS(MNUMCR,CURIYR) + QCLGAS(IC,CURIYR)
         PCLGAS(MNUMCR,CURIYR) = PCLGAS(MNUMCR,CURIYR) + QCLGAS(IC,CURIYR) * PCLGAS(IC,CURIYR)
         PDSTRHWY(MNUMCR,CURIYR) = PDSTRHWY(MNUMCR,CURIYR) + QDSTRHWY(IC,CURIYR) * PDSTRHWY(IC,CURIYR)
         QGFTRFV(MNUMCR,CURIYR) = QGFTRFV(MNUMCR,CURIYR) + QGFTRFV(IC,CURIYR)
         QGFTRPV(MNUMCR,CURIYR) = QGFTRPV(MNUMCR,CURIYR) + QGFTRPV(IC,CURIYR)
         QGLTRFV(MNUMCR,CURIYR) = QGLTRFV(MNUMCR,CURIYR) + QGLTRFV(IC,CURIYR)
         QGLTRPV(MNUMCR,CURIYR) = QGLTRPV(MNUMCR,CURIYR) + QGLTRPV(IC,CURIYR)
         QGFTRRAIL(1,MNUMCR,CURIYR) = QGFTRRAIL(1,MNUMCR,CURIYR) + QGFTRRAIL(1,IC,CURIYR)
         QGFTRRAIL(2,MNUMCR,CURIYR) = QGFTRRAIL(2,MNUMCR,CURIYR) + QGFTRRAIL(2,IC,CURIYR)
         QGFTRRAIL(3,MNUMCR,CURIYR) = QGFTRRAIL(3,MNUMCR,CURIYR) + QGFTRRAIL(3,IC,CURIYR)
         QGFTRRAIL(4,MNUMCR,CURIYR) = QGFTRRAIL(4,MNUMCR,CURIYR) + QGFTRRAIL(4,IC,CURIYR)
         QGLTRRAIL(1,MNUMCR,CURIYR) = QGLTRRAIL(1,MNUMCR,CURIYR) + QGLTRRAIL(1,IC,CURIYR)
         QGLTRRAIL(2,MNUMCR,CURIYR) = QGLTRRAIL(2,MNUMCR,CURIYR) + QGLTRRAIL(2,IC,CURIYR)
         QGLTRRAIL(3,MNUMCR,CURIYR) = QGLTRRAIL(3,MNUMCR,CURIYR) + QGLTRRAIL(3,IC,CURIYR)
         QGLTRRAIL(4,MNUMCR,CURIYR) = QGLTRRAIL(4,MNUMCR,CURIYR) + QGLTRRAIL(4,IC,CURIYR)
         QGFTRSHIP(1,MNUMCR,CURIYR) = QGFTRSHIP(1,MNUMCR,CURIYR) + QGFTRSHIP(1,IC,CURIYR)
         QGFTRSHIP(2,MNUMCR,CURIYR) = QGFTRSHIP(2,MNUMCR,CURIYR) + QGFTRSHIP(2,IC,CURIYR)
         QGFTRSHIP(3,MNUMCR,CURIYR) = QGFTRSHIP(3,MNUMCR,CURIYR) + QGFTRSHIP(3,IC,CURIYR)
         QGLTRSHIP(1,MNUMCR,CURIYR) = QGLTRSHIP(1,MNUMCR,CURIYR) + QGLTRSHIP(1,IC,CURIYR)
         QGLTRSHIP(2,MNUMCR,CURIYR) = QGLTRSHIP(2,MNUMCR,CURIYR) + QGLTRSHIP(2,IC,CURIYR)
         QGLTRSHIP(3,MNUMCR,CURIYR) = QGLTRSHIP(3,MNUMCR,CURIYR) + QGLTRSHIP(3,IC,CURIYR)
         PGFTRFV(MNUMCR,CURIYR) = PGFTRFV(MNUMCR,CURIYR) + PGFTRFV(IC,CURIYR) * QGFTRFV(IC,CURIYR)
         PGFTRPV(MNUMCR,CURIYR) = PGFTRPV(MNUMCR,CURIYR) + PGFTRPV(IC,CURIYR) * QGFTRPV(IC,CURIYR)
         PGLTRFV(MNUMCR,CURIYR) = PGLTRFV(MNUMCR,CURIYR) + PGLTRFV(IC,CURIYR) * QGLTRFV(IC,CURIYR)
         PGLTRPV(MNUMCR,CURIYR) = PGLTRPV(MNUMCR,CURIYR) + PGLTRPV(IC,CURIYR) * QGLTRPV(IC,CURIYR)
         PGFTRRAIL(1,MNUMCR,CURIYR) = PGFTRRAIL(1,MNUMCR,CURIYR) + PGFTRRAIL(1,IC,CURIYR) * QGFTRRAIL(1,IC,CURIYR)
         PGFTRRAIL(2,MNUMCR,CURIYR) = PGFTRRAIL(2,MNUMCR,CURIYR) + PGFTRRAIL(2,IC,CURIYR) * QGFTRRAIL(2,IC,CURIYR)
         PGFTRRAIL(3,MNUMCR,CURIYR) = PGFTRRAIL(3,MNUMCR,CURIYR) + PGFTRRAIL(3,IC,CURIYR) * QGFTRRAIL(3,IC,CURIYR)
         PGFTRRAIL(4,MNUMCR,CURIYR) = PGFTRRAIL(4,MNUMCR,CURIYR) + PGFTRRAIL(4,IC,CURIYR) * QGFTRRAIL(4,IC,CURIYR)
         PGLTRRAIL(1,MNUMCR,CURIYR) = PGLTRRAIL(1,MNUMCR,CURIYR) + PGLTRRAIL(1,IC,CURIYR) * QGLTRRAIL(1,IC,CURIYR)
         PGLTRRAIL(2,MNUMCR,CURIYR) = PGLTRRAIL(2,MNUMCR,CURIYR) + PGLTRRAIL(2,IC,CURIYR) * QGLTRRAIL(2,IC,CURIYR)
         PGLTRRAIL(3,MNUMCR,CURIYR) = PGLTRRAIL(3,MNUMCR,CURIYR) + PGLTRRAIL(3,IC,CURIYR) * QGLTRRAIL(3,IC,CURIYR)
         PGLTRRAIL(4,MNUMCR,CURIYR) = PGLTRRAIL(4,MNUMCR,CURIYR) + PGLTRRAIL(4,IC,CURIYR) * QGLTRRAIL(4,IC,CURIYR)
         PGFTRSHIP(1,MNUMCR,CURIYR) = PGFTRSHIP(1,MNUMCR,CURIYR) + PGFTRSHIP(1,IC,CURIYR) * QGFTRSHIP(1,IC,CURIYR)
         PGFTRSHIP(2,MNUMCR,CURIYR) = PGFTRSHIP(2,MNUMCR,CURIYR) + PGFTRSHIP(2,IC,CURIYR) * QGFTRSHIP(2,IC,CURIYR)
         PGFTRSHIP(3,MNUMCR,CURIYR) = PGFTRSHIP(3,MNUMCR,CURIYR) + PGFTRSHIP(3,IC,CURIYR) * QGFTRSHIP(3,IC,CURIYR)
         PGLTRSHIP(1,MNUMCR,CURIYR) = PGLTRSHIP(1,MNUMCR,CURIYR) + PGLTRSHIP(1,IC,CURIYR) * QGLTRSHIP(1,IC,CURIYR)
         PGLTRSHIP(2,MNUMCR,CURIYR) = PGLTRSHIP(2,MNUMCR,CURIYR) + PGLTRSHIP(2,IC,CURIYR) * QGLTRSHIP(2,IC,CURIYR)
         PGLTRSHIP(3,MNUMCR,CURIYR) = PGLTRSHIP(3,MNUMCR,CURIYR) + PGLTRSHIP(3,IC,CURIYR) * QGLTRSHIP(3,IC,CURIYR)
      ENDDO
      IF (INQLGPF(MNUMCR,CURIYR) .NE. 0.0) THEN
        PLGINPF(MNUMCR,CURIYR) = PLGINPF(MNUMCR,CURIYR) / INQLGPF(MNUMCR,CURIYR)
      ELSE
        PLGINPF(MNUMCR,CURIYR) = sum(PLGINPF(1:9,CURIYR)) / 9.
      ENDIF
      IF (QCLGAS(MNUMCR,CURIYR) .NE. 0.0) THEN
        PCLGAS(MNUMCR,CURIYR) = PCLGAS(MNUMCR,CURIYR) / QCLGAS(MNUMCR,CURIYR)
      ELSE
        PCLGAS(MNUMCR,CURIYR) = sum(PCLGAS(1:9,CURIYR)) / 9.
      ENDIF
      IF (QDSTRHWY(MNUMCR,CURIYR) .NE. 0.0) THEN
        PDSTRHWY(MNUMCR,CURIYR) = PDSTRHWY(MNUMCR,CURIYR) / QDSTRHWY(MNUMCR,CURIYR)
      ELSE
        PDSTRHWY(MNUMCR,CURIYR) = sum(PDSTRHWY(1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRFV(MNUMCR,CURIYR) .NE. 0.0) THEN
        PGFTRFV(MNUMCR,CURIYR) = PGFTRFV(MNUMCR,CURIYR) / QGFTRFV(MNUMCR,CURIYR)
      ELSE
        PGFTRFV(MNUMCR,CURIYR) = sum(PGFTRFV(1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRPV(MNUMCR,CURIYR) .NE. 0.0) THEN
        PGFTRPV(MNUMCR,CURIYR) = PGFTRPV(MNUMCR,CURIYR) / QGFTRPV(MNUMCR,CURIYR)
      ELSE
        PGFTRPV(MNUMCR,CURIYR) = sum(PGFTRPV(1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRFV(MNUMCR,CURIYR) .NE. 0.0) THEN
        PGLTRFV(MNUMCR,CURIYR) = PGLTRFV(MNUMCR,CURIYR) / QGLTRFV(MNUMCR,CURIYR)
      ELSE
        PGLTRFV(MNUMCR,CURIYR) = sum(PGLTRFV(1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRPV(MNUMCR,CURIYR) .NE. 0.0) THEN
        PGLTRPV(MNUMCR,CURIYR) = PGLTRPV(MNUMCR,CURIYR) / QGLTRPV(MNUMCR,CURIYR)
      ELSE
        PGLTRPV(MNUMCR,CURIYR) = sum(PGLTRPV(1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRRAIL(1,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGFTRRAIL(1,MNUMCR,CURIYR) = PGFTRRAIL(1,MNUMCR,CURIYR) / QGFTRRAIL(1,MNUMCR,CURIYR)
      ELSE
        PGFTRRAIL(1,MNUMCR,CURIYR) = sum(PGFTRRAIL(1,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRRAIL(2,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGFTRRAIL(2,MNUMCR,CURIYR) = PGFTRRAIL(2,MNUMCR,CURIYR) / QGFTRRAIL(2,MNUMCR,CURIYR)
      ELSE
        PGFTRRAIL(2,MNUMCR,CURIYR) = sum(PGFTRRAIL(2,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRRAIL(3,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGFTRRAIL(3,MNUMCR,CURIYR) = PGFTRRAIL(3,MNUMCR,CURIYR) / QGFTRRAIL(3,MNUMCR,CURIYR)
      ELSE
        PGFTRRAIL(3,MNUMCR,CURIYR) = sum(PGFTRRAIL(3,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRRAIL(4,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGFTRRAIL(4,MNUMCR,CURIYR) = PGFTRRAIL(4,MNUMCR,CURIYR) / QGFTRRAIL(4,MNUMCR,CURIYR)
      ELSE
        PGFTRRAIL(4,MNUMCR,CURIYR) = sum(PGFTRRAIL(4,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRRAIL(1,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGLTRRAIL(1,MNUMCR,CURIYR) = PGLTRRAIL(1,MNUMCR,CURIYR) / QGLTRRAIL(1,MNUMCR,CURIYR)
      ELSE
        PGLTRRAIL(1,MNUMCR,CURIYR) = sum(PGLTRRAIL(1,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRRAIL(2,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGLTRRAIL(2,MNUMCR,CURIYR) = PGLTRRAIL(2,MNUMCR,CURIYR) / QGLTRRAIL(2,MNUMCR,CURIYR)
      ELSE
        PGLTRRAIL(2,MNUMCR,CURIYR) = sum(PGLTRRAIL(2,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRRAIL(3,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGLTRRAIL(3,MNUMCR,CURIYR) = PGLTRRAIL(3,MNUMCR,CURIYR) / QGLTRRAIL(3,MNUMCR,CURIYR)
      ELSE
        PGLTRRAIL(3,MNUMCR,CURIYR) = sum(PGLTRRAIL(3,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRRAIL(4,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGLTRRAIL(4,MNUMCR,CURIYR) = PGLTRRAIL(4,MNUMCR,CURIYR) / QGLTRRAIL(4,MNUMCR,CURIYR)
      ELSE
        PGLTRRAIL(4,MNUMCR,CURIYR) = sum(PGLTRRAIL(4,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRSHIP(1,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGFTRSHIP(1,MNUMCR,CURIYR) = PGFTRSHIP(1,MNUMCR,CURIYR) / QGFTRSHIP(1,MNUMCR,CURIYR)
      ELSE
        PGFTRSHIP(1,MNUMCR,CURIYR) = sum(PGFTRSHIP(1,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRSHIP(2,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGFTRSHIP(2,MNUMCR,CURIYR) = PGFTRSHIP(2,MNUMCR,CURIYR) / QGFTRSHIP(2,MNUMCR,CURIYR)
      ELSE
        PGFTRSHIP(2,MNUMCR,CURIYR) = sum(PGFTRSHIP(2,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRSHIP(3,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGFTRSHIP(3,MNUMCR,CURIYR) = PGFTRSHIP(3,MNUMCR,CURIYR) / QGFTRSHIP(3,MNUMCR,CURIYR)
      ELSE
        PGFTRSHIP(3,MNUMCR,CURIYR) = sum(PGFTRSHIP(3,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRSHIP(1,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGLTRSHIP(1,MNUMCR,CURIYR) = PGLTRSHIP(1,MNUMCR,CURIYR) / QGLTRSHIP(1,MNUMCR,CURIYR)
      ELSE
        PGLTRSHIP(1,MNUMCR,CURIYR) = sum(PGLTRSHIP(1,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRSHIP(2,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGLTRSHIP(2,MNUMCR,CURIYR) = PGLTRSHIP(2,MNUMCR,CURIYR) / QGLTRSHIP(2,MNUMCR,CURIYR)
      ELSE
        PGLTRSHIP(2,MNUMCR,CURIYR) = sum(PGLTRSHIP(2,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRSHIP(3,MNUMCR,CURIYR) .NE. 0.0) THEN
        PGLTRSHIP(3,MNUMCR,CURIYR) = PGLTRSHIP(3,MNUMCR,CURIYR) / QGLTRSHIP(3,MNUMCR,CURIYR)
      ELSE
        PGLTRSHIP(3,MNUMCR,CURIYR) = sum(PGLTRSHIP(3,1:9,CURIYR)) / 9.
      ENDIF

! +++ Zero Out US Average Price for all products
      DO IV=1,MNUMP
         MPRC(MNUMCR,IY,IV)=0.
      ENDDO

!   Average the Petroleum Products for each sector.

      DO 111 IR=1,MNUMCR-1
        PRSCM(IR,IY)=PRLCM(IR,IY)
        PRSIN(IR,IY)=PRLIN(IR,IY)
        IF(QRSEL(IR,IY).NE.0.) &
        PRSEL(IR,IY)=PRLEL(IR,IY)  ! ignore high sulfur
        IF(QRSTR(IR,IY).NE.0.) &
        PRSTR(IR,IY)=(PRLTR(IR,IY)*QRLTR(IR,IY)+PRHTR(IR,IY)*QRHTR(IR,IY))/QRSTR(IR,IY)
        IF((QDSRS(IR,IY)+QKSRS(IR,IY)+QLGRS(IR,IY))   .NE.0.0) &
        PTPRS(IR,IY) = (PDSRS(IR,IY)*QDSRS(IR,IY)+PKSRS(IR,IY)* &
           QKSRS(IR,IY)+PLGRS(IR,IY)*QLGRS(IR,IY))/(QDSRS(IR,IY)+ &
           QKSRS(IR,IY)+QLGRS(IR,IY))
        IF((QMGCM(IR,IY)+QDSCM(IR,IY)+QKSCM(IR,IY)+QLGCM(IR,IY)+QRLCM(IR,IY)) .NE. 0.0) &
        PTPCM(IR,IY) = (PMGCM(IR,IY)*QMGCM(IR,IY)+PDSCM(IR,IY)* &
           QDSCM(IR,IY)+PKSCM(IR,IY)*QKSCM(IR,IY)+PLGCM(IR,IY)* &
           QLGCM(IR,IY)+PRLCM(IR,IY)*QRLCM(IR,IY))/(QMGCM(IR,IY)+ &
           QDSCM(IR,IY)+QKSCM(IR,IY)+QLGCM(IR,IY)+QRLCM(IR,IY))
        IF((QMGTR(IR,IY)+QDSTR(IR,IY)+QJFTR(IR,IY)+QLGTR(IR,IY)+ &
            QRLTR(IR,IY)+QRHTR(IR,IY)+QOTTR(IR,IY))    .NE.0.0) &
        PTPTR(IR,IY) = (PMGTR(IR,IY)*QMGTR(IR,IY)+PDSTR(IR,IY)* &
           QDSTR(IR,IY)+PJFTR(IR,IY)*QJFTR(IR,IY)+PLGTR(IR,IY)* &
           QLGTR(IR,IY)+PRLTR(IR,IY)*QRLTR(IR,IY)+PRHTR(IR,IY)* &
           QRHTR(IR,IY)+POTTR(IR,IY)*QOTTR(IR,IY))/(QMGTR(IR,IY)+ &
           QDSTR(IR,IY)+QJFTR(IR,IY)+QLGTR(IR,IY)+QRLTR(IR,IY)+ &
           QRHTR(IR,IY)+QOTTR(IR,IY))
        IF((QMGIN(IR,IY)+QDSIN(IR,IY)+QKSIN(IR,IY)+QLGIN(IR,IY)+ &
            QRLIN(IR,IY)+QPFIN(IR,IY)+QASIN(IR,IY)+QOTIN(IR,IY)).NE.0.0) &
        PTPIN(IR,IY) = (PMGIN(IR,IY)*QMGIN(IR,IY) + PDSIN(IR,IY)*QDSIN(IR,IY) + &
                        PKSIN(IR,IY)*QKSIN(IR,IY)+ &
                        PLGIN(IR,IY)*(QLGIN(IR,IY)-INQLGPF(IR,IY)) + PLGINPF(IR,IY)*INQLGPF(IR,IY) + &
                        PRLIN(IR,IY)*QRLIN(IR,IY) + PPFIN(IR,IY)*QPFIN(IR,IY) + &
                        PASIN(IR,IY)*QASIN(IR,IY) + POTIN(IR,IY)*QOTIN(IR,IY)) / &
                       (QMGIN(IR,IY) + QDSIN(IR,IY) + QKSIN(IR,IY) + QLGIN(IR,IY) + &
                        QRLIN(IR,IY) + QPFIN(IR,IY) + QASIN(IR,IY) + QOTIN(IR,IY))
! recalculate this to exclude PLGINPF at least until we know more about it
        IF((QMGIN(IR,IY)+QDSIN(IR,IY)+QKSIN(IR,IY)+QLGIN(IR,IY)+ &
            QRLIN(IR,IY)+QPFIN(IR,IY)+QASIN(IR,IY)+QOTIN(IR,IY)).NE.0.0) &
        PTPIN(IR,IY) = (PMGIN(IR,IY)*QMGIN(IR,IY) + PDSIN(IR,IY)*QDSIN(IR,IY) + &
                        PKSIN(IR,IY)*QKSIN(IR,IY)+ &
                        PLGIN(IR,IY)*QLGIN(IR,IY)+ &
                        PRLIN(IR,IY)*QRLIN(IR,IY) + PPFIN(IR,IY)*QPFIN(IR,IY) + &
                        PASIN(IR,IY)*QASIN(IR,IY) + POTIN(IR,IY)*QOTIN(IR,IY)) / &
                       (QMGIN(IR,IY) + QDSIN(IR,IY) + QKSIN(IR,IY) + QLGIN(IR,IY) + &
                        QRLIN(IR,IY) + QPFIN(IR,IY) + QASIN(IR,IY) + QOTIN(IR,IY))
        IF((QDSEL(IR,IY)+QRLEL(IR,IY)+QRHEL(IR,IY))   .NE.0.0) &
        PTPEL(IR,IY) = (PDSEL(IR,IY)*QDSEL(IR,IY) + PRSEL(IR,IY)*QRSEL(IR,IY)) / &  !  Using average residual prices
                       (QDSEL(IR,IY)+QRSEL(IR,IY))
 111  CONTINUE

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+    Average Prices Across Sectors--Fill the P....AS arrays.
!+
!+    The FUNCTION NUMP_AS(IP,MNUMP) returns the number of component prices for
!+    any of the MNUMP price arrays.  Prices arrays with names not ending in
!+    "AS" (for Across Sector) have 0 as the number of component prices. For
!+    the "AS" arrays, NUMP_AS(IV) returns NUMPAS, the number of prices defined
!+    for product IV.  These NUMPAS component prices arrays are stored as the
!+    NUMPAS consecutive arrays before IV, or from (IV-NUMPAS) to (IV-1).
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DO IV=1,MNUMP
         NUMPAS=NUMP_AS(IV,MNUMP)
         IF (NUMPAS.GT.0) THEN
            IQ=PQMATCH(IV,MNUMQ,MNUMP)         ! Get matching QUANTITY product
            DO IC=1,MNUMCR-1
               MPRC(IC,IY,IV)=0.
               IF (MQTY(IC,IY,IQ).GT.0.) THEN  ! Prevent divide by zero
                  DO IP=1,NUMPAS               ! Loop over "NUMPAS" preceeding
                     IPRC=IV-IP                ! products.
                     IQTY=PQMATCH(IPRC,MNUMQ,MNUMP)
                     IF (MPRC(IC,IY,IPRC) .NE. MPRC(IC,IY,IPRC) .OR. MPRC(IC,IY,IPRC) .LT. 0.0000000000001) THEN
                        IF (MPRC(IC,IY,IPRC) .NE. 0.0) MPRC(IC,IY,IPRC) = 0.0000000000001
                     END IF
                     IF (MQTY(IC,IY,IQTY) .NE. MQTY(IC,IY,IQTY) .OR. &
                        (MQTY(IC,IY,IQTY) .LT. 0.0000000000001 .AND. MQTY(IC,IY,IQTY) .GT. -0.0000000000001)) THEN
                        IF (MQTY(IC,IY,IQTY) .LT. 0.0) THEN
                           MQTY(IC,IY,IQTY) = -0.0000000000001
                        ELSE IF (MQTY(IC,IY,IQTY) .GT. 0.0) THEN
                           MQTY(IC,IY,IQTY) = 0.0000000000001
                        END IF
                     END IF
                     MPRC(IC,IY,IV)=MPRC(IC,IY,IV)+MPRC(IC,IY,IPRC)*MQTY(IC,IY,IQTY)
                  ENDDO
                  MPRC(IC,IY,IV)=MPRC(IC,IY,IV)/MQTY(IC,IY,IQ)
               ENDIF
            ENDDO
         ENDIF
      ENDDO

! Recalculate LPG average price to include feedstock price (PLGIN represents heat & power price)
      DO IC = 1,MNUMCR-1
        IF (QLGRS(IC,IY) + QLGCM(IC,IY) + QLGIN(IC,IY) + QLGTR(IC,IY) .NE. 0.0)     PLGAS(IC,IY) = &
                 (PLGIN(IC,IY)*(QLGIN(IC,IY)-INQLGPF(IC,IY)) + PLGINPF(IC,IY)*INQLGPF(IC,IY) + &
                  PLGRS(IC,IY)*QLGRS(IC,IY) + PLGCM(IC,IY)*QLGCM(IC,IY) + PLGTR(IC,IY)*QLGTR(IC,IY)) / &
                 (QLGRS(IC,IY) + QLGCM(IC,IY) + QLGIN(IC,IY) + QLGTR(IC,IY))
      ENDDO

! +++ Average Prices Across Regions, using the corresponding regional quantities as the weights.
      DO IC = 1,MNUMCR - 2
         DO IV=1,MNUMP
            IQ=PQMATCH(IV,MNUMQ,MNUMP)
            IF (MQTY(MNUMCR,IY,IQ).NE.0.) THEN
               MPRC(MNUMCR,IY,IV)=MPRC(MNUMCR,IY,IV)+ &
                   MPRC(IC,IY,IV)*(MQTY(IC,IY,IQ)/MQTY(MNUMCR,IY,IQ))
            ENDIF
         ENDDO
      ENDDO
! Recalculate LPG average price to include feedstock price (PLGIN represents heat & power price)
      IF ((sum(QLGIN(1:9,IY))-sum(INQLGPF(1:9,IY))) .NE. 0.0) &
      PLGIN(MNUMCR,IY) = (PLGIN(1,IY) * (QLGIN(1,IY) - INQLGPF(1,IY)) + &
                          PLGIN(2,IY) * (QLGIN(2,IY) - INQLGPF(2,IY)) + &
                          PLGIN(3,IY) * (QLGIN(3,IY) - INQLGPF(3,IY)) + &
                          PLGIN(4,IY) * (QLGIN(4,IY) - INQLGPF(4,IY)) + &
                          PLGIN(5,IY) * (QLGIN(5,IY) - INQLGPF(5,IY)) + &
                          PLGIN(6,IY) * (QLGIN(6,IY) - INQLGPF(6,IY)) + &
                          PLGIN(7,IY) * (QLGIN(7,IY) - INQLGPF(7,IY)) + &
                          PLGIN(8,IY) * (QLGIN(8,IY) - INQLGPF(8,IY)) + &
                          PLGIN(9,IY) * (QLGIN(9,IY) - INQLGPF(9,IY))) / &
                         (sum(QLGIN(1:9,IY))-sum(INQLGPF(1:9,IY)))

      RETURN
      END
!******************************************************************
!*    SUBROUTINE AVEPASA
!*
!*    AVERAGES ADJUSTED PRICES ACROSS SECTOR AND REGION FOR EPM
!******************************************************************
      SUBROUTINE AVEPASA
      IMPLICIT NONE

      include 'parametr'
      include 'qblk'
      include 'ampblk'
      include 'ncntrl'
      include 'cdsparms'
      include 'coalout'
      include 'indout'
      include 'qonroad'
      include 'aponroad'
!  this is a mistake:  the 'Q' variables are in ngtdmout, but not in angtdm
!  tran.f does indeed use angtdm when it wants the price
!  doing this include combo to get both 'P' and 'Q' variables
!  because of this include combo, need to use 'A' variables for adjusted prices below
      include 'ngtdmout'
      include 'epmngtdm'

      INTEGER IY          ! INDEX FOR YEAR
      INTEGER IC          ! INDEX FOR CENSUS REGION
      INTEGER IV,IP,IPRC  ! INDICES FOR PRICE VARIABLE
      INTEGER IQ,IQTY     ! INDEX FOR QUANTITY VARIABLE
      INTEGER NUMPAS,IR
      EXTERNAL NUMP_AS
      INTEGER NUMP_AS
      EXTERNAL PQMATCH
      INTEGER PQMATCH

      IY = CURIYR

      PCLGAS(:,IY) = PCLEL(:,IY)    !temporary only!

      INQLGPF(MNUMCR,CURIYR) = 0.0
      PLGINPF(MNUMCR,CURIYR) = 0.0
      QCLGAS(MNUMCR,CURIYR) = 0.0
      PCLGAS(MNUMCR,CURIYR) = 0.0
      PDSTRHWY(MNUMCR,CURIYR) = 0.0
      QGFTRFV(MNUMCR,CURIYR) = 0.0
      QGFTRPV(MNUMCR,CURIYR) = 0.0
      QGLTRFV(MNUMCR,CURIYR) = 0.0
      QGLTRPV(MNUMCR,CURIYR) = 0.0
      QGFTRRAIL(:,MNUMCR,CURIYR) = 0.0
      QGLTRRAIL(:,MNUMCR,CURIYR) = 0.0
      QGFTRSHIP(:,MNUMCR,CURIYR) = 0.0
      QGLTRSHIP(:,MNUMCR,CURIYR) = 0.0
!  because of include combo above, need to use 'A' variables for adjusted prices
      AGFTRFV(MNUMCR,CURIYR) = 0.0
      AGFTRPV(MNUMCR,CURIYR) = 0.0
      AGLTRFV(MNUMCR,CURIYR) = 0.0
      AGLTRPV(MNUMCR,CURIYR) = 0.0
      AGFTRRAIL(:,MNUMCR,CURIYR) = 0.0
      AGLTRRAIL(:,MNUMCR,CURIYR) = 0.0
      AGFTRSHIP(:,MNUMCR,CURIYR) = 0.0
      AGLTRSHIP(:,MNUMCR,CURIYR) = 0.0
      DO IC=1,MNUMCR-2
         INQLGPF(MNUMCR,CURIYR) = INQLGPF(MNUMCR,CURIYR) + INQLGPF(IC,CURIYR)
         PLGINPF(MNUMCR,CURIYR) = PLGINPF(MNUMCR,CURIYR) + PLGINPF(IC,CURIYR) * INQLGPF(IC,CURIYR)
         QCLGAS(MNUMCR,CURIYR) = QCLGAS(MNUMCR,CURIYR) + QCLGAS(IC,CURIYR)
         PCLGAS(MNUMCR,CURIYR) = PCLGAS(MNUMCR,CURIYR) + QCLGAS(IC,CURIYR) * PCLGAS(IC,CURIYR)
         PDSTRHWY(MNUMCR,CURIYR) = PDSTRHWY(MNUMCR,CURIYR) + QDSTRHWY(IC,CURIYR) * PDSTRHWY(IC,CURIYR)
         QGFTRFV(MNUMCR,CURIYR) = QGFTRFV(MNUMCR,CURIYR) + QGFTRFV(IC,CURIYR)
         QGFTRPV(MNUMCR,CURIYR) = QGFTRPV(MNUMCR,CURIYR) + QGFTRPV(IC,CURIYR)
         QGLTRFV(MNUMCR,CURIYR) = QGLTRFV(MNUMCR,CURIYR) + QGLTRFV(IC,CURIYR)
         QGLTRPV(MNUMCR,CURIYR) = QGLTRPV(MNUMCR,CURIYR) + QGLTRPV(IC,CURIYR)
         QGFTRRAIL(1,MNUMCR,CURIYR) = QGFTRRAIL(1,MNUMCR,CURIYR) + QGFTRRAIL(1,IC,CURIYR)
         QGFTRRAIL(2,MNUMCR,CURIYR) = QGFTRRAIL(2,MNUMCR,CURIYR) + QGFTRRAIL(2,IC,CURIYR)
         QGFTRRAIL(3,MNUMCR,CURIYR) = QGFTRRAIL(3,MNUMCR,CURIYR) + QGFTRRAIL(3,IC,CURIYR)
         QGFTRRAIL(4,MNUMCR,CURIYR) = QGFTRRAIL(4,MNUMCR,CURIYR) + QGFTRRAIL(4,IC,CURIYR)
         QGLTRRAIL(1,MNUMCR,CURIYR) = QGLTRRAIL(1,MNUMCR,CURIYR) + QGLTRRAIL(1,IC,CURIYR)
         QGLTRRAIL(2,MNUMCR,CURIYR) = QGLTRRAIL(2,MNUMCR,CURIYR) + QGLTRRAIL(2,IC,CURIYR)
         QGLTRRAIL(3,MNUMCR,CURIYR) = QGLTRRAIL(3,MNUMCR,CURIYR) + QGLTRRAIL(3,IC,CURIYR)
         QGLTRRAIL(4,MNUMCR,CURIYR) = QGLTRRAIL(4,MNUMCR,CURIYR) + QGLTRRAIL(4,IC,CURIYR)
         QGFTRSHIP(1,MNUMCR,CURIYR) = QGFTRSHIP(1,MNUMCR,CURIYR) + QGFTRSHIP(1,IC,CURIYR)
         QGFTRSHIP(2,MNUMCR,CURIYR) = QGFTRSHIP(2,MNUMCR,CURIYR) + QGFTRSHIP(2,IC,CURIYR)
         QGFTRSHIP(3,MNUMCR,CURIYR) = QGFTRSHIP(3,MNUMCR,CURIYR) + QGFTRSHIP(3,IC,CURIYR)
         QGLTRSHIP(1,MNUMCR,CURIYR) = QGLTRSHIP(1,MNUMCR,CURIYR) + QGLTRSHIP(1,IC,CURIYR)
         QGLTRSHIP(2,MNUMCR,CURIYR) = QGLTRSHIP(2,MNUMCR,CURIYR) + QGLTRSHIP(2,IC,CURIYR)
         QGLTRSHIP(3,MNUMCR,CURIYR) = QGLTRSHIP(3,MNUMCR,CURIYR) + QGLTRSHIP(3,IC,CURIYR)
         AGFTRFV(MNUMCR,CURIYR) = AGFTRFV(MNUMCR,CURIYR) + AGFTRFV(IC,CURIYR) * QGFTRFV(IC,CURIYR)
         AGFTRPV(MNUMCR,CURIYR) = AGFTRPV(MNUMCR,CURIYR) + AGFTRPV(IC,CURIYR) * QGFTRPV(IC,CURIYR)
         AGLTRFV(MNUMCR,CURIYR) = AGLTRFV(MNUMCR,CURIYR) + AGLTRFV(IC,CURIYR) * QGLTRFV(IC,CURIYR)
         AGLTRPV(MNUMCR,CURIYR) = AGLTRPV(MNUMCR,CURIYR) + AGLTRPV(IC,CURIYR) * QGLTRPV(IC,CURIYR)
         AGFTRRAIL(1,MNUMCR,CURIYR) = AGFTRRAIL(1,MNUMCR,CURIYR) + AGFTRRAIL(1,IC,CURIYR) * QGFTRRAIL(1,IC,CURIYR)
         AGFTRRAIL(2,MNUMCR,CURIYR) = AGFTRRAIL(2,MNUMCR,CURIYR) + AGFTRRAIL(2,IC,CURIYR) * QGFTRRAIL(2,IC,CURIYR)
         AGFTRRAIL(3,MNUMCR,CURIYR) = AGFTRRAIL(3,MNUMCR,CURIYR) + AGFTRRAIL(3,IC,CURIYR) * QGFTRRAIL(3,IC,CURIYR)
         AGFTRRAIL(4,MNUMCR,CURIYR) = AGFTRRAIL(4,MNUMCR,CURIYR) + AGFTRRAIL(4,IC,CURIYR) * QGFTRRAIL(4,IC,CURIYR)
         AGLTRRAIL(1,MNUMCR,CURIYR) = AGLTRRAIL(1,MNUMCR,CURIYR) + AGLTRRAIL(1,IC,CURIYR) * QGLTRRAIL(1,IC,CURIYR)
         AGLTRRAIL(2,MNUMCR,CURIYR) = AGLTRRAIL(2,MNUMCR,CURIYR) + AGLTRRAIL(2,IC,CURIYR) * QGLTRRAIL(2,IC,CURIYR)
         AGLTRRAIL(3,MNUMCR,CURIYR) = AGLTRRAIL(3,MNUMCR,CURIYR) + AGLTRRAIL(3,IC,CURIYR) * QGLTRRAIL(3,IC,CURIYR)
         AGLTRRAIL(4,MNUMCR,CURIYR) = AGLTRRAIL(4,MNUMCR,CURIYR) + AGLTRRAIL(4,IC,CURIYR) * QGLTRRAIL(4,IC,CURIYR)
         AGFTRSHIP(1,MNUMCR,CURIYR) = AGFTRSHIP(1,MNUMCR,CURIYR) + AGFTRSHIP(1,IC,CURIYR) * QGFTRSHIP(1,IC,CURIYR)
         AGFTRSHIP(2,MNUMCR,CURIYR) = AGFTRSHIP(2,MNUMCR,CURIYR) + AGFTRSHIP(2,IC,CURIYR) * QGFTRSHIP(2,IC,CURIYR)
         AGFTRSHIP(3,MNUMCR,CURIYR) = AGFTRSHIP(3,MNUMCR,CURIYR) + AGFTRSHIP(3,IC,CURIYR) * QGFTRSHIP(3,IC,CURIYR)
         AGLTRSHIP(1,MNUMCR,CURIYR) = AGLTRSHIP(1,MNUMCR,CURIYR) + AGLTRSHIP(1,IC,CURIYR) * QGLTRSHIP(1,IC,CURIYR)
         AGLTRSHIP(2,MNUMCR,CURIYR) = AGLTRSHIP(2,MNUMCR,CURIYR) + AGLTRSHIP(2,IC,CURIYR) * QGLTRSHIP(2,IC,CURIYR)
         AGLTRSHIP(3,MNUMCR,CURIYR) = AGLTRSHIP(3,MNUMCR,CURIYR) + AGLTRSHIP(3,IC,CURIYR) * QGLTRSHIP(3,IC,CURIYR)
      ENDDO
      IF (INQLGPF(MNUMCR,CURIYR) .NE. 0.0) THEN
        PLGINPF(MNUMCR,CURIYR) = PLGINPF(MNUMCR,CURIYR) / INQLGPF(MNUMCR,CURIYR)
      ELSE
        PLGINPF(MNUMCR,CURIYR) = sum(PLGINPF(1:9,CURIYR)) / 9
      ENDIF
      IF (QCLGAS(MNUMCR,CURIYR) .NE. 0.0) THEN
        PCLGAS(MNUMCR,CURIYR) = PCLGAS(MNUMCR,CURIYR) / QCLGAS(MNUMCR,CURIYR)
      ELSE
        PCLGAS(MNUMCR,CURIYR) = sum(PCLGAS(1:9,CURIYR)) / 9
      ENDIF
      IF (QDSTRHWY(MNUMCR,CURIYR) .NE. 0.0) THEN
        PDSTRHWY(MNUMCR,CURIYR) = PDSTRHWY(MNUMCR,CURIYR) / QDSTRHWY(MNUMCR,CURIYR)
      ELSE
        PDSTRHWY(MNUMCR,CURIYR) = sum(PDSTRHWY(1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRFV(MNUMCR,CURIYR) .NE. 0.0) THEN
        AGFTRFV(MNUMCR,CURIYR) = AGFTRFV(MNUMCR,CURIYR) / QGFTRFV(MNUMCR,CURIYR)
      ELSE
        AGFTRFV(MNUMCR,CURIYR) = sum(AGFTRFV(1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRPV(MNUMCR,CURIYR) .NE. 0.0) THEN
        AGFTRPV(MNUMCR,CURIYR) = AGFTRPV(MNUMCR,CURIYR) / QGFTRPV(MNUMCR,CURIYR)
      ELSE
        AGFTRPV(MNUMCR,CURIYR) = sum(AGFTRPV(1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRFV(MNUMCR,CURIYR) .NE. 0.0) THEN
        AGLTRFV(MNUMCR,CURIYR) = AGLTRFV(MNUMCR,CURIYR) / QGLTRFV(MNUMCR,CURIYR)
      ELSE
        AGLTRFV(MNUMCR,CURIYR) = sum(AGLTRFV(1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRPV(MNUMCR,CURIYR) .NE. 0.0) THEN
        AGLTRPV(MNUMCR,CURIYR) = AGLTRPV(MNUMCR,CURIYR) / QGLTRPV(MNUMCR,CURIYR)
      ELSE
        AGLTRPV(MNUMCR,CURIYR) = sum(AGLTRPV(1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRRAIL(1,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGFTRRAIL(1,MNUMCR,CURIYR) = AGFTRRAIL(1,MNUMCR,CURIYR) / QGFTRRAIL(1,MNUMCR,CURIYR)
      ELSE
        AGFTRRAIL(1,MNUMCR,CURIYR) = sum(AGFTRRAIL(1,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRRAIL(2,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGFTRRAIL(2,MNUMCR,CURIYR) = AGFTRRAIL(2,MNUMCR,CURIYR) / QGFTRRAIL(2,MNUMCR,CURIYR)
      ELSE
        AGFTRRAIL(2,MNUMCR,CURIYR) = sum(AGFTRRAIL(2,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRRAIL(3,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGFTRRAIL(3,MNUMCR,CURIYR) = AGFTRRAIL(3,MNUMCR,CURIYR) / QGFTRRAIL(3,MNUMCR,CURIYR)
      ELSE
        AGFTRRAIL(3,MNUMCR,CURIYR) = sum(AGFTRRAIL(3,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRRAIL(4,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGFTRRAIL(4,MNUMCR,CURIYR) = AGFTRRAIL(4,MNUMCR,CURIYR) / QGFTRRAIL(4,MNUMCR,CURIYR)
      ELSE
        AGFTRRAIL(4,MNUMCR,CURIYR) = sum(AGFTRRAIL(4,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRRAIL(1,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGLTRRAIL(1,MNUMCR,CURIYR) = AGLTRRAIL(1,MNUMCR,CURIYR) / QGLTRRAIL(1,MNUMCR,CURIYR)
      ELSE
        AGLTRRAIL(1,MNUMCR,CURIYR) = sum(AGLTRRAIL(1,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRRAIL(2,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGLTRRAIL(2,MNUMCR,CURIYR) = AGLTRRAIL(2,MNUMCR,CURIYR) / QGLTRRAIL(2,MNUMCR,CURIYR)
      ELSE
        AGLTRRAIL(2,MNUMCR,CURIYR) = sum(AGLTRRAIL(2,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRRAIL(3,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGLTRRAIL(3,MNUMCR,CURIYR) = AGLTRRAIL(3,MNUMCR,CURIYR) / QGLTRRAIL(3,MNUMCR,CURIYR)
      ELSE
        AGLTRRAIL(3,MNUMCR,CURIYR) = sum(AGLTRRAIL(3,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRRAIL(4,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGLTRRAIL(4,MNUMCR,CURIYR) = AGLTRRAIL(4,MNUMCR,CURIYR) / QGLTRRAIL(4,MNUMCR,CURIYR)
      ELSE
        AGLTRRAIL(4,MNUMCR,CURIYR) = sum(AGLTRRAIL(4,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRSHIP(1,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGFTRSHIP(1,MNUMCR,CURIYR) = AGFTRSHIP(1,MNUMCR,CURIYR) / QGFTRSHIP(1,MNUMCR,CURIYR)
      ELSE
        AGFTRSHIP(1,MNUMCR,CURIYR) = sum(AGFTRSHIP(1,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRSHIP(2,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGFTRSHIP(2,MNUMCR,CURIYR) = AGFTRSHIP(2,MNUMCR,CURIYR) / QGFTRSHIP(2,MNUMCR,CURIYR)
      ELSE
        AGFTRSHIP(2,MNUMCR,CURIYR) = sum(AGFTRSHIP(2,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGFTRSHIP(3,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGFTRSHIP(3,MNUMCR,CURIYR) = AGFTRSHIP(3,MNUMCR,CURIYR) / QGFTRSHIP(3,MNUMCR,CURIYR)
      ELSE
        AGFTRSHIP(3,MNUMCR,CURIYR) = sum(AGFTRSHIP(3,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRSHIP(1,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGLTRSHIP(1,MNUMCR,CURIYR) = AGLTRSHIP(1,MNUMCR,CURIYR) / QGLTRSHIP(1,MNUMCR,CURIYR)
      ELSE
        AGLTRSHIP(1,MNUMCR,CURIYR) = sum(AGLTRSHIP(1,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRSHIP(2,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGLTRSHIP(2,MNUMCR,CURIYR) = AGLTRSHIP(2,MNUMCR,CURIYR) / QGLTRSHIP(2,MNUMCR,CURIYR)
      ELSE
        AGLTRSHIP(2,MNUMCR,CURIYR) = sum(AGLTRSHIP(2,1:9,CURIYR)) / 9.
      ENDIF
      IF (QGLTRSHIP(3,MNUMCR,CURIYR) .NE. 0.0) THEN
        AGLTRSHIP(3,MNUMCR,CURIYR) = AGLTRSHIP(3,MNUMCR,CURIYR) / QGLTRSHIP(3,MNUMCR,CURIYR)
      ELSE
        AGLTRSHIP(3,MNUMCR,CURIYR) = sum(AGLTRSHIP(3,1:9,CURIYR)) / 9.
      ENDIF

! +++ Zero Out US Average Price for all products
      DO IV=1,MNUMP
         MPRC(MNUMCR,IY,IV)=0.
      ENDDO

!   Average the Petroleum Products for each sector.

      DO 111 IR=1,MNUMCR-1
        PRSCM(IR,IY)=PRLCM(IR,IY)
        PRSIN(IR,IY)=PRLIN(IR,IY)
        IF(QRSEL(IR,IY).NE.0.) &
        PRSEL(IR,IY)=PRLEL(IR,IY)  ! ignore high sulfur
        IF(QRSTR(IR,IY).NE.0.) &
        PRSTR(IR,IY)=(PRLTR(IR,IY)*QRLTR(IR,IY)+PRHTR(IR,IY)*QRHTR(IR,IY))/QRSTR(IR,IY)
        IF((QDSRS(IR,IY)+QKSRS(IR,IY)+QLGRS(IR,IY))   .NE.0.0) &
        PTPRS(IR,IY) = (PDSRS(IR,IY)*QDSRS(IR,IY)+PKSRS(IR,IY)* &
           QKSRS(IR,IY)+PLGRS(IR,IY)*QLGRS(IR,IY))/(QDSRS(IR,IY)+ &
           QKSRS(IR,IY)+QLGRS(IR,IY))
        IF((QMGCM(IR,IY)+QDSCM(IR,IY)+QKSCM(IR,IY)+QLGCM(IR,IY)+QRLCM(IR,IY)) .NE. 0.0) &
        PTPCM(IR,IY) = (PMGCM(IR,IY)*QMGCM(IR,IY)+PDSCM(IR,IY)* &
           QDSCM(IR,IY)+PKSCM(IR,IY)*QKSCM(IR,IY)+PLGCM(IR,IY)* &
           QLGCM(IR,IY)+PRLCM(IR,IY)*QRLCM(IR,IY))/(QMGCM(IR,IY)+ &
           QDSCM(IR,IY)+QKSCM(IR,IY)+QLGCM(IR,IY)+QRLCM(IR,IY))
        IF((QMGTR(IR,IY)+QDSTR(IR,IY)+QJFTR(IR,IY)+QLGTR(IR,IY)+ &
            QRLTR(IR,IY)+QRHTR(IR,IY)+QOTTR(IR,IY))    .NE.0.0) &
        PTPTR(IR,IY) = (PMGTR(IR,IY)*QMGTR(IR,IY)+PDSTR(IR,IY)* &
           QDSTR(IR,IY)+PJFTR(IR,IY)*QJFTR(IR,IY)+PLGTR(IR,IY)* &
           QLGTR(IR,IY)+PRLTR(IR,IY)*QRLTR(IR,IY)+PRHTR(IR,IY)* &
           QRHTR(IR,IY)+POTTR(IR,IY)*QOTTR(IR,IY))/(QMGTR(IR,IY)+ &
           QDSTR(IR,IY)+QJFTR(IR,IY)+QLGTR(IR,IY)+QRLTR(IR,IY)+ &
           QRHTR(IR,IY)+QOTTR(IR,IY))
        IF((QMGIN(IR,IY)+QDSIN(IR,IY)+QKSIN(IR,IY)+QLGIN(IR,IY)+ &
            QRLIN(IR,IY)+QPFIN(IR,IY)+QASIN(IR,IY)+QOTIN(IR,IY)).NE.0.0) &
        PTPIN(IR,IY) = (PMGIN(IR,IY)*QMGIN(IR,IY) + PDSIN(IR,IY)*QDSIN(IR,IY) + &
                        PKSIN(IR,IY)*QKSIN(IR,IY)+ &
                        PLGIN(IR,IY)*(QLGIN(IR,IY)-INQLGPF(IR,IY)) + PLGINPF(IR,IY)*INQLGPF(IR,IY) + &
                        PRLIN(IR,IY)*QRLIN(IR,IY) + PPFIN(IR,IY)*QPFIN(IR,IY) + &
                        PASIN(IR,IY)*QASIN(IR,IY) + POTIN(IR,IY)*QOTIN(IR,IY)) / &
                       (QMGIN(IR,IY) + QDSIN(IR,IY) + QKSIN(IR,IY) + QLGIN(IR,IY) + &
                        QRLIN(IR,IY) + QPFIN(IR,IY) + QASIN(IR,IY) + QOTIN(IR,IY))
! recalculate this to exclude PLGINPF at least until we know more about it
        IF((QMGIN(IR,IY)+QDSIN(IR,IY)+QKSIN(IR,IY)+QLGIN(IR,IY)+ &
            QRLIN(IR,IY)+QPFIN(IR,IY)+QASIN(IR,IY)+QOTIN(IR,IY)).NE.0.0) &
        PTPIN(IR,IY) = (PMGIN(IR,IY)*QMGIN(IR,IY) + PDSIN(IR,IY)*QDSIN(IR,IY) + &
                        PKSIN(IR,IY)*QKSIN(IR,IY)+ &
                        PLGIN(IR,IY)*QLGIN(IR,IY)+ &
                        PRLIN(IR,IY)*QRLIN(IR,IY) + PPFIN(IR,IY)*QPFIN(IR,IY) + &
                        PASIN(IR,IY)*QASIN(IR,IY) + POTIN(IR,IY)*QOTIN(IR,IY)) / &
                       (QMGIN(IR,IY) + QDSIN(IR,IY) + QKSIN(IR,IY) + QLGIN(IR,IY) + &
                        QRLIN(IR,IY) + QPFIN(IR,IY) + QASIN(IR,IY) + QOTIN(IR,IY))
        IF((QDSEL(IR,IY)+QRLEL(IR,IY)+QRHEL(IR,IY))   .NE.0.0) &
        PTPEL(IR,IY) = (PDSEL(IR,IY)*QDSEL(IR,IY) + PRSEL(IR,IY)*QRSEL(IR,IY)) / &  !  Using average residual prices
                       (QDSEL(IR,IY)+QRSEL(IR,IY))
 111  CONTINUE

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+    Average Prices Across Sectors--Fill the P....AS arrays.
!+
!+    The FUNCTION NUMP_AS(IP,MNUMP) returns the number of component prices for
!+    any of the MNUMP price arrays.  Prices arrays with names not ending in
!+    "AS" (for Across Sector) have 0 as the number of component prices. For
!+    the "AS" arrays, NUMP_AS(IV) returns NUMPAS, the number of prices defined
!+    for product IV.  These NUMPAS component prices arrays are stored as the
!+    NUMPAS consecutive arrays before IV, or from (IV-NUMPAS) to (IV-1).
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DO IV=1,MNUMP
         NUMPAS=NUMP_AS(IV,MNUMP)
         IF (NUMPAS.GT.0) THEN
            IQ=PQMATCH(IV,MNUMQ,MNUMP)         ! Get matching QUANTITY product
            DO IC=1,MNUMCR-1
               MPRC(IC,IY,IV)=0.
               IF (MQTY(IC,IY,IQ).GT.0.) THEN  ! Prevent divide by zero
                  DO IP=1,NUMPAS               ! Loop over "NUMPAS" preceeding
                     IPRC=IV-IP                ! products.
                     IQTY=PQMATCH(IPRC,MNUMQ,MNUMP)
                     MPRC(IC,IY,IV)=MPRC(IC,IY,IV)+MPRC(IC,IY,IPRC)*MQTY(IC,IY,IQTY)
                  ENDDO
                  MPRC(IC,IY,IV)=MPRC(IC,IY,IV)/MQTY(IC,IY,IQ)
               ENDIF
            ENDDO
         ENDIF
      ENDDO

! Recalculate LPG average price to include feedstock price (PLGIN represents heat & power price)
      DO IC = 1,MNUMCR-1
        IF (QLGRS(IC,IY) + QLGCM(IC,IY) + QLGIN(IC,IY) + QLGTR(IC,IY) .NE. 0.0)    PLGAS(IC,IY) = &
                (PLGIN(IC,IY)*(QLGIN(IC,IY)-INQLGPF(IC,IY)) + PLGINPF(IC,IY)*INQLGPF(IC,IY) + &
                 PLGRS(IC,IY)*QLGRS(IC,IY) + PLGCM(IC,IY)*QLGCM(IC,IY) + PLGTR(IC,IY)*QLGTR(IC,IY)) / &
                (QLGRS(IC,IY) + QLGCM(IC,IY) + QLGIN(IC,IY) + QLGTR(IC,IY))
      ENDDO

! +++ Average Prices Across Regions, using the corresponding regional quantities as the weights.
      DO IC = 1,MNUMCR - 2
         DO IV=1,MNUMP
            IQ=PQMATCH(IV,MNUMQ,MNUMP)
            IF (MQTY(MNUMCR,IY,IQ).NE.0.) THEN
               MPRC(MNUMCR,IY,IV)=MPRC(MNUMCR,IY,IV)+ &
                   MPRC(IC,IY,IV)*(MQTY(IC,IY,IQ)/MQTY(MNUMCR,IY,IQ))
            ENDIF
         ENDDO
      ENDDO
! Recalculate LPG average price to include feedstock price (PLGIN represents heat & power price)
      IF ((sum(QLGIN(1:9,IY))-sum(INQLGPF(1:9,IY))) .NE. 0.0) &
      PLGIN(MNUMCR,IY) = (PLGIN(1,IY) * (QLGIN(1,IY) - INQLGPF(1,IY)) + &
                          PLGIN(2,IY) * (QLGIN(2,IY) - INQLGPF(2,IY)) + &
                          PLGIN(3,IY) * (QLGIN(3,IY) - INQLGPF(3,IY)) + &
                          PLGIN(4,IY) * (QLGIN(4,IY) - INQLGPF(4,IY)) + &
                          PLGIN(5,IY) * (QLGIN(5,IY) - INQLGPF(5,IY)) + &
                          PLGIN(6,IY) * (QLGIN(6,IY) - INQLGPF(6,IY)) + &
                          PLGIN(7,IY) * (QLGIN(7,IY) - INQLGPF(7,IY)) + &
                          PLGIN(8,IY) * (QLGIN(8,IY) - INQLGPF(8,IY)) + &
                          PLGIN(9,IY) * (QLGIN(9,IY) - INQLGPF(9,IY))) / &
                         (sum(QLGIN(1:9,IY))-sum(INQLGPF(1:9,IY)))

      RETURN
      END
!******************************************************************************
!*    Function NUMP_AS(IPRICE,MNUMP)
!*
!*    The function NUMP_AS(IPRICE) returns the number of component prices for
!*    any of the MNUMP price arrays.  Prices arrays with names not ending in
!*    "AS" (for Across Sector) have 0 as the number of component prices. For
!*    the "AS" arrays, NUMP_AS(IV,MNUMP) returns NUMPAS, the number of prices
!*    defined for product IV. These NUMPAS component prices arrays are stored
!*    as the NUMPAS consecutive arrays before IV, or from (IV-NUMPAS) to (IV-1).
!******************************************************************************
      FUNCTION NUMP_AS(IPRICE,MNUMP)
      IMPLICIT NONE

      INTEGER NUMP_AS
      INTEGER MNUMP,IPRICE
      INTEGER NP
      PARAMETER (NP=84)
      INTEGER NUMPAS(NP)

!  THE NUMPAS VARIABLE IS A KEY FOR BOTH CONVERGENCE TESTING AND
!     SUMMING OVER THE SECTORS INTO THE 'AS' VARIABLES AS FOLLOWS:

!           VALUE   EFFECT
!            -1     NO CONVERGENCE TESTING OR SUMMING
!             0     NO SUMMING
!             N(>0) SUM PREVIOUS N VARIABLES;  NO CONVERGENCE TESTING

      DATA NUMPAS/ &
            0, 0, 0, 0, 4,       & ! PELRS,PELCM,PELTR,PELIN,PELAS,
           -1,-1,-1, 0, 0, 5,    & ! PGFRS,PGFCM,PGFTR,PGFIN,PGFEL,PGFAS,
           -1,-1,-1, 0, 0, 5,    & ! PGIRS,PGICM,PGITR,PGIIN,PGIEL,PGIAS,
            0, 0, 0, 0, 0, 5,    & ! PNGRS,PNGCM,PNGTR,PNGIN,PNGEL,PNGAS,
            0, 0,                & ! PGPTR,PLPIN,
            0, 0, 0, 0, 0, 5,    & ! PCLRS,PCLCM,PCLIN,PCLEL,PCLSN,PCLAS,
            0,                   & ! PMCIN,
            0, 0, 0, 3,          & ! PMGCM,PMGTR,PMGIN,PMGAS,
            0,                   & ! PJFTR,
            0, 0, 0, 0, 0, 5,    & ! PDSRS,PDSCM,PDSTR,PDSIN,PDSEL,PDSAS,
            0, 0, 0, 3,          & ! PKSRS,PKSCM,PKSIN,PKSAS,
            0, 0, 0, 0, 4,       & ! PLGRS,PLGCM,PLGTR,PLGIN,PLGAS,
            0, 0, 0, 0, 4,       & ! PRLCM,PRLTR,PRLIN,PRLEL,PRLAS,
            0, 0, 2,             & ! PRHTR,PRHEL,PRHAS,
            0, 0, 0, 0, 4,       & ! PRSCM,PRSTR,PRSIN,PRSEL,PRSAS,
            0, 0,                & ! PPFIN,PASIN,
            0, 0, 2,             & ! POTTR,POTIN,POTAS,
           -1,-1,-1,-1,-1,-1, 6, & ! PTPRS,PTPCM,PTPTR,PTPIN,PTPRF,PTPEL,PTPAS
            0, 0, 0, 0,          & ! PMETR,PETTR,PHYTR,PUREL
           -1,-1,-1/               ! PH1TR,PH2TR,PH3TR

      IF (NP.NE.MNUMP) THEN
         WRITE(*,*) ' PARAMETER MNUMP (DIMENSION OF MAIN PRICE ARRAY MPRC) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION NUMP_AS IN MAIN.'
      ENDIF

      NUMP_AS=NUMPAS(IPRICE)
      RETURN
      END
!******************************************************************************
!*    Function NUMQ_AS(IQUAN,MNUMQ)
!*
!*    The function NUMQ_AS(IPRICE) returns the number of component quantities
!*    for the MNUMQ array.  Quantity arrays with names not ending in "AS"
!*    (for Across Sector) have 0 as the number of components.  It is currently
!*    (5/7/92) being used to skip these "AS" series for the convergence test.
!******************************************************************************
      FUNCTION NUMQ_AS(IQUAN,MNUMQ)
      IMPLICIT NONE

      INTEGER NUMQ_AS
      INTEGER MNUMQ,IQUAN
      INTEGER NQ
      PARAMETER (NQ=157)
      INTEGER NUMQAS(NQ)

      DATA NUMQAS/ &
            0, 0, 0, 0, 0, 0, 6,        & ! QELRS,QELCM,QELTR,QELIN,QELRF,QELHM,QELAS,
           -1,-1,-1, 0,-1, 0,-1, 7,     & ! QGFRS,QGFCM,QGFTR,QGFIN,QGFRF,QGFEL,QGFHM,QGFAS,
           -1,-1,-1, 0,-1, 0,-1, 7,     & ! QGIRS,QGICM,QGITR,QGIIN,QGIRF,QGIEL,QGIHM,QGIAS,
            0, 0, 0, 0, 0, 0, 0, 7,     & ! QNGRS,QNGCM,QNGTR,QNGIN,QNGRF,QNGEL,QNGHM,QNGAS,
            0, 0,                       & ! QGPTR,QLPIN,
            0, 0, 0, 0, 0, 0, 0, 7,     & ! QCLRS,QCLCM,QCLIN,QCLRF,QCLEL,QCLSN,QCLHM,QCLAS,
            0,                          & ! QMCIN,
            0, 0, 0, 3,                 & ! QMGCM,QMGTR,QMGIN,QMGAS,
            0,                          & ! QJFTR,
            0, 0, 0, 0, 0, 0, 6,        & ! QDSRS,QDSCM,QDSTR,QDSIN,QDSRF,QDSEL,QDSAS,
            0, 0, 0, 3,                 & ! QKSRS,QKSCM,QKSIN,QKSAS,
            0, 0, 0, 0, 0, 5,           & ! QLGRS,QLGCM,QLGTR,QLGIN,QLGRF,QLGAS,
            0, 0, 0, 0, 0, 5,           & ! QRLCM,QRLTR,QRLIN,QRLRF,QRLEL,QRLAS,
            0, 0, 2,                    & ! QRHTR,QRHEL,QRHAS,
           -1,-1,-1,-1,-1, 5,           & ! QRSCM,QRSTR,QRSIN,QRSRF,QRSEL,QRSAS,
            0, 0, 0,                    & ! QPFIN,QSGIN,QSGRF,
            0, 0, 0, 3,                 & ! QPCIN,QPCRF,QPCEL,QPCAS,
            0,                          & ! QASIN,
            0, 0, 0, 3,                 & ! QOTTR,QOTIN,QOTRF,QOTAS,
           -1,-1,-1,-1,-1,-1, 6,        & ! QTPRS,QTPCM,QTPTR,QTPIN,QTPRF,QTPEL,QTPAS,
            0, 0, 0, 0, 0, 0,           & ! QMETR,QETTR,QETHM,QHYTR,QUREL,QURHM,
            0, 0, 2,                    & ! QHOIN,QHOEL,QHOAS,
            0, 0, 0, 3,                 & ! QGERS,QGEIN,QGEEL,QGEAS,
            0, 0, 0, 0, 0, 0, 0, 7,     & ! QBMRS,QBMCM,QBMIN,QBMRF,QBMEL,QBMSN,QBMHM,QBMAS,
            0, 0, 2,                    & ! QMSIN,QMSEL,QMSAS,
            0, 0, 0, 0, 4,              & ! QSTRS,QSTCM,QSTIN,QSTEL,QSTAS,
            0, 0, 0, 0, 4,              & ! QPVRS,QPVCM,QPVIN,QPVEL,QPVAS,
            0, 0, 2,                    & ! QWIIN,QWIEL,QWIAS,
           -1,-1,-1,-1,-1,-1,-1, 7,     & ! QTRRS,QTRCM,QTRTR,QTRIN,QTREL,QTRSN,QTRHM,QTRAS,
            0, 0,                       & ! QEIEL,QCIIN,
           -1,-1,-1,-1,-1,-1,-1,-1, 8,  & ! QTSRS,QTSCM,QTSTR,QTSIN,QTSRF,QTSEL,QTSSN,QTSHM,QTSAS
            0, 0, 0/                      ! QH1TR,QH2TR,QH3TR

      IF (NQ.NE.MNUMQ) THEN
         WRITE(*,*) ' PARAMETER MNUMQ (DIMENSION OF MAIN QUANTITY ARRAY MPRC) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION NUMQ_AS IN MAIN.'
      ENDIF

      NUMQ_AS=NUMQAS(IQUAN)

      RETURN
      END
!******************************************************************************
!*    Function PQMATCH(IPRICE,MNUMQ,MNUMP)
!*
!*    Returns the subscript of the corresponding quantity-variable for any
!*    PRICE subscript.  Checks for changes in price/quantity variable dimen-
!*    sions using MNUMQ and MNUMP.
!******************************************************************************
      FUNCTION PQMATCH(IPRICE,MNUMQ,MNUMP)
      IMPLICIT NONE

      INTEGER PQMATCH        ! Returns product code subscript for MQTY
      INTEGER NP             ! Dimension of map -- MUST MATCH MNUMP
      PARAMETER(NP=84)
      INTEGER MAP(NP)        ! Mapping of PRICE products to QUANTITY products
      INTEGER IPRICE         ! Product code subscript for MPRC
      INTEGER MNUMQ,MNUMP    ! Dimensions of MQTY and MPRC to insure against
                             ! changes.

      DATA MAP/ &
         1,  2,  3,  4,  7,          & ! PELRS,PELCM,PELTR,PELIN,PELAS,
         8,  9, 10, 11, 13, 15,      & ! PGFRS,PGFCM,PGFTR,PGFIN,PGFEL,PGFAS,
        16, 17, 18, 19, 21, 23,      & ! PGIRS,PGICM,PGITR,PGIIN,PGIEL,PGIAS,
        24, 25, 26, 27, 29, 31,      & ! PNGRS,PNGCM,PNGTR,PNGIN,PNGEL,PNGAS,
        32, 33,                      & ! PGPTR,PLPIN,
        34, 35, 36, 38, 39, 41, 42,  & ! PCLRS,PCLCM,PCLIN,PCLEL,PCLSN,PCLAS,PMCIN,
        43, 44, 45, 46, 47,          & ! PMGCM,PMGTR,PMGIN,PMGAS,PJFTR,
        48, 49, 50, 51, 53, 54,      & ! PDSRS,PDSCM,PDSTR,PDSIN,PDSEL,PDSAS,
        55, 56, 57, 58,              & ! PKSRS,PKSCM,PKSIN,PKSAS,
        59, 60, 61, 62, 64,          & ! PLGRS,PLGCM,PLGTR,PLGIN,PLGAS,
        65, 66, 67, 69, 70,          & ! PRLCM,PRLTR,PRLIN,PRLEL,PRLAS,
        71, 72, 73,                  & ! PRHTR,PRHEL,PRHAS,
        74, 75, 76, 78, 79,          & ! PRSCM,PRSTR,PRSIN,PRSEL,PRSAS,
        80, 87,                      & ! PPFIN,PASIN,
        88, 89, 91,                  & ! POTTR,POTIN,POTAS,
        92, 93, 94, 95, 96, 97, 98,  & ! PTPRS,PTPCM,PTPTR,PTPIN,PTPRF,PTPEL,PTPAS,
        99,100,102,103,              & ! PMETR,PETTR,PHYTR,PUREL
       155,156,157/                    ! PH1TR,PH2TR,PH3TR

      PQMATCH=MAP(IPRICE)
      IF (NP.NE.MNUMP) THEN
         WRITE(*,*) ' PARAMETER MNUMP (DIMENSION OF MAIN PRICE ARRAY MPRC) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION PQMATCH IN MAIN.'
      ENDIF

      IF (PQMATCH.GT.MNUMQ) THEN
         WRITE(*,*) ' PARAMETER MNUMQ (DIMENSION OF MAIN QTY ARRAY MQTY) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION PQMATCH IN MAIN.'
      ENDIF

      RETURN
      END
      subroutine GETIRUN(run_char)
      use dfport
      implicit none
      character*(*) run_char
      call getenv("n",run_char)
      return
      end
      subroutine GETMPS(mps)
      use dfport
      implicit none
      character*50 mps
      call getenv("MPS",mps)
      return
      end
!*******************************************************************
!*  Subroutine NRUSER
!*
!*  Read run-time options from user interface file (JCL.DAT)
!*******************************************************************
      SUBROUTINE NRUSER
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'macout'

      integer nobjects
      character*127 objects(50)
      character*20 omlvers
      character*50 MPS,NEMSDIR
      common/holduser/nobjects,objects,omlvers,MPS,NEMSDIR

      character*1 pound ! used for reading #
      integer iret

      INTEGER IMODEL
      INTEGER RTOVALUE
      EXTERNAL RTOVALUE
      CHARACTER*18 JCLDAT
      LOGICAL NEW,OLD
      INTEGER UNITI
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      logical success,lexist
      integer i

      OLD=.FALSE.
      JCLDAT='JCLDAT'
      UNITI=FILE_MGR('O',JCLDAT,OLD)

      IF (UNITI .EQ. (-12)) UNITI=5

      READ(UNITI,'(A72)') COMMENT
      READ(UNITI,'(A)')  SCEN
      READ(UNITI,'(A)')  DATE
      SCEN_DATE = trim(SCEN) // '.' // DATE
      READ(UNITI,*) EXW,MORDER(1),PRTDBGW    ! RUNMOD( 1) WORLD
      READ(UNITI,*) EXM,MORDER(2),PRTDBGM    ! RUNMOD( 2) MAC
      READ(UNITI,*) EXR,MORDER(3),PRTDBGR    ! RUNMOD( 3) RESD
      READ(UNITI,*) EXK,MORDER(4),PRTDBGK    ! RUNMOD( 4) COMM
      READ(UNITI,*) EXI,MORDER(5),PRTDBGI    ! RUNMOD( 5) IND
      READ(UNITI,*) EXT,MORDER(6),PRTDBGT    ! RUNMOD( 6) TRAN
      READ(UNITI,*) EXE,MORDER(7),PRTDBGE    ! RUNMOD( 7) UTIL
      READ(UNITI,*) EXC,MORDER(8),PRTDBGC    ! RUNMOD( 8) COAL
      READ(UNITI,*) EXL,MORDER(9),PRTDBGL    ! RUNMOD( 9) WELL (OIL AND GAS SUPPLY)
      READ(UNITI,*) EXG,MORDER(10),PRTDBGG   ! RUNMOD(10) GAS TRANSP & DISTRIBUTION
      READ(UNITI,*) EXO,MORDER(11),PRTDBGO   ! RUNMOD(11) REFINE
      READ(UNITI,*) EXN,MORDER(12),PRTDBGN   ! RUNMOD(12) RENEW
! go out and get hydrogen module (RUNMOD(13) execution, order, print debug switches
      EXH=RTOVALUE('EXH     ',0)
      MORDER(13)=RTOVALUE('ORDH    ',13)
      PRTDBGH=RTOVALUE('PRTDBGH ',0)
! put model execution switches in array used in NEXEC
      RUNMOD( 1)=EXW
      RUNMOD( 2)=EXM
      RUNMOD( 3)=EXR
      RUNMOD( 4)=EXK
      RUNMOD( 5)=EXI
      RUNMOD( 6)=EXT
      RUNMOD( 7)=EXE
      RUNMOD( 8)=EXC
      RUNMOD( 9)=EXL
      RUNMOD(10)=EXG
      RUNMOD(11)=EXO
      RUNMOD(12)=EXN
      RUNMOD(13)=EXH
! more reading to do
      READ(UNITI,*) FIRSYR      ! START YEAR
      READ(UNITI,*) LASTYR      ! LAST YEAR
      READ(UNITI,*) MAXITR      ! MAXIMUM ITERATIONS
      READ(UNITI,*) FRCTOL      ! FRACTIONAL convergence TOLERANCE
      READ(UNITI,*) ABSTOL      ! ABSOLUTE quantity convergence TOLERANCE

      READ(UNITI,*) RLXPC       ! RELAXATION PERCENTAGE - this is used if no variable-specific relaxation is found
      IF ((RLXPC .LT. .5) .OR. (RLXPC .GT. 1)) RLXPC = .66666

      READ(UNITI,*) NYRS        ! NUMBER OF GROWTH YEARS for expectations
      IF ((NYRS .LT. 1) .OR. (NYRS .GT. 6)) NYRS = 3

      READ(UNITI,*) I4SITE      ! FORESIGHT (1:MYOPIC, 2: ADAPTIVE, 3: PERFECT)
      READ(UNITI,*) I4SCNT      ! FORESIGHT CONTROL: (1: MAIN, 2: SUBMODULE)
      READ(UNITI,*) IRELAX      ! OPTION TO RUN RELAXATION ROUTINE IN MAIN
      READ(UNITI,*) ITIMNG      ! OPTION TO TIME MODULES
      READ(UNITI,*) WWOP        ! WORLD OIL PRICE CASE
      READ(UNITI,*) MMAC        ! MACRO CASE
      READ(UNITI,*) HISTORY     ! 1990 SEDS DATA FLAG
      READ(UNITI,*) MACFDBK     ! MACRO FEEDBACK SWITCH (1 = ON, 0 = OFF)
      READ(UNITI,*) ELASSW      ! ELASTICITY SWITCH (1 = ON, 0 = OFF)
      READ(UNITI,*) DSMSWTCH    ! DSM LOGIC SWITCH (1 = DO DSM, 0 = DON'T)
      READ(UNITI,*) MODELON     ! LEAVE MODELS ON ONCE CONVERGED = 1
      READ(UNITI,*) DBDUMP      ! WRITE RESTART FILE EVERY YEAR (1=YES)
      READ(UNITI,*) ECPSTART    ! START YEAR FOR UTIL CAP PLANS

      DO IMODEL=1,15
         READ(UNITI,'(A)')
      ENDDO

      IF (FIRSYR.LT.BASEYR.OR.FIRSYR.GT.ENDYR) FIRSYR=BASEYR
      IF (LASTYR.LT.BASEYR.OR.LASTYR.GT.ENDYR) LASTYR=ENDYR
      FIRSYR=FIRSYR-BASEYR+1
      LASTCALYR=LASTYR
      LASTYR=LASTYR-BASEYR+1

! +++ Temporary assignments for two FTAB variables
      YEARPR=RTOVALUE('YEARPR  ',1993)  ! REPORTING YEAR FOR REAL $
      SCALPR=MC_JPGDP(YEARPR-1989) !PCWGDP SCALING FROM 1987$ TO YEARPR $

! read complete list of objects linked with NEMS
      pound=' '
      nobjects=0
      read(uniti,'(a)',end=25) pound
      if(pound.eq.'#'  ) then
        pound=' '
        do while (pound.ne.'#')
          nobjects=nobjects+1
          read(uniti,'(a)',end=25) objects(nobjects)
          pound=objects(nobjects)(1:1)
        enddo
      endif
25    continue
      if(nobjects.gt.1.and.pound.eq.'#') nobjects=nobjects-1

! read oml version identifier used in setting the MPS environment string in cycle.sh
      omlvers=' '
      read(uniti,'(a)',end=30) omlvers
      write(6,*) 'The following MPS environment string should match this OML version: ',omlvers

      call getmps(mps)
      write(6,*) 'MPS=',trim(mps)
30    continue

      IF (UNITI .NE. 5) THEN
        UNITI=FILE_MGR('C',JCLDAT,OLD)
      ENDIF
! call routine to read wk1 record id codes
      CALL wk1init

!     Now that the restart has been read re-establish IJUMPYR and IJUMPCALYR

      IJUMPYR = RTOVALUE('IJUMPYR ',41)
      IJUMPCALYR = RTOVALUE('IJUMPCAL',2030)

      RETURN

      END
!*******************************************************************
!*    Subroutine NROTHR
!*
!*    Reads any remaining data specific to the integrating model, but
!*    not included in the RESTART file or the user interface file.
!*******************************************************************
      SUBROUTINE NROTHR
      IMPLICIT NONE

      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'cdsparms'
      include 'coalemm'
      include 'coalprc'
      include 'pqchar'
      include 'convfact'
      include 'emission'
      include 'tcs45q'
      include 'macout'
      include 'continew'

      INTEGER FUNITI,FRETCD,FUNITO,FRTYPE,FUNFMT,FSOURC,HISTYR,I_S,I_L,I_R
      CHARACTER*100 FNAMEI,FNAMEO,FUNCTION*1
      CHARACTER*18 FNAMEI18,FNAMEO18,FUNFMTC*11
      CHARACTER CHECKQ*6,CHECKP*6,CHECKUQ*9,CHECKUP*9,CHECKO*9
      REAL MNCNVF,MNCNVA,RLXT(20),RLXX(20),REDUCE, WEIGHTS(5,MNUMYR)
      CHARACTER*165 CONV_LINE
      CHARACTER*1 CONV_SET, STARTHERE
      CHARACTER*9 CHECK
      CHARACTER*16 RNAME
      INTEGER WHATTODO,ITR_PER_RLX,HOWMANYITR
      INTEGER RISKYEAR
      REAL    NUMBER
      INTEGER IV,II
      LOGICAL KEEPGOING/.TRUE./
      LOGICAL MATCH
      LOGICAL NEW
      INTEGER RTOVALUE,RDCVFACT
      EXTERNAL RTOVALUE
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      COMMON/MAXITRSR/MAXITRSR
      INTEGER MAXITRSR           ! RUNTIME OPTION FOR MAXITR 2001-ON
      REAL API_IN(3),CRUDEQ(5,4)
      COMMON /CVHISTYR/ HISTYR

      REASONYES=1            !  no reason yet to not stop on this cycle (intercv sees '1' as PASS)
      REASONCHR="Other"      ! general category name
      IF (EXW .EQ. 1) THEN
         CONTINW=1
         REASONW="None"
      ENDIF
      IF (EXM .EQ. 1) THEN
         CONTINM=1
         REASONM="None"
      ENDIF
      IF (EXR .EQ. 1) THEN
         CONTINR=1
         REASONR="None"
      ENDIF
      IF (EXK .EQ. 1) THEN
         CONTINK=1
         REASONK="None"
      ENDIF
      IF (EXI .EQ. 1) THEN
         CONTINI=1
         REASONI="None"
      ENDIF
      IF (EXT .EQ. 1) THEN
         CONTINT=1
         REASONT="None"
      ENDIF
      IF (EXE .EQ. 1) THEN
         CONTINE=1
         REASONE="None"
      ELSE
         IF (CONTINE .EQ. 0 .AND. REASONE .EQ. 'None') REASONE = 'see p2'
      ENDIF
      IF (EXC .EQ. 1) THEN
         CONTINC=1
         REASONC="None"
      ELSE
         IF (CONTINC .EQ. 0 .AND. REASONC .EQ. 'None') REASONC = 'see p2'
      ENDIF
      IF (EXL .EQ. 1) THEN
         CONTINL=1
         REASONL="None"
      ENDIF
      IF (EXG .EQ. 1) THEN
         CONTING=1
         REASONG="None"
      ENDIF
      IF (EXO .EQ. 1) THEN
         CONTINO=1
         REASONO="None"
      ENDIF
      IF (EXN .EQ. 1) THEN
         CONTINN=1
         REASONN="None"
      ENDIF
      IF (EXH .EQ. 1) THEN
         CONTINH=1
         REASONH="None"
      ENDIF

!   Open file containing convergence tolerances.

      NEW = .FALSE.
      FUNITI=1
      FNAMEI18='MNCNVRG'
      FUNITI=FILE_MGR('O',FNAMEI18,NEW)

!  Initialize convergence arrays to FRCTOL and ABSTOL.
!  Initialize relaxation arrays to RLXPC.

      DO 900 IV=1,MNUMQ
        MNQCNV(1,IV) = FRCTOL
        MNQCNV(2,IV) = ABSTOL
        RLXQ(1:20,IV) = RLXPC
900   CONTINUE
      DO 901 IV=1,MNUMP
        MNPCNV(1,IV) = FRCTOL
        MNPCNV(2,IV) = ABSTOL
        RLXP(1:20,IV) = RLXPC
901   CONTINUE
      DO 902 IV=1,3
        MNUQCNV(1,IV) = FRCTOL
        MNUQCNV(2,IV) = ABSTOL
        MNUPCNV(1,IV) = FRCTOL
        MNUPCNV(2,IV) = ABSTOL
        MNGASQCNV(1,IV) = FRCTOL
        MNGASQCNV(2,IV) = ABSTOL
        MNGASPCNV(1,IV) = FRCTOL
        MNGASPCNV(2,IV) = ABSTOL
        RLXUP(1:20,IV) = RLXPC
        RLXUQ(1:20,IV) = RLXPC
        RLXGASP(1:20,IV) = RLXPC
        RLXGASQ(1:20,IV) = RLXPC
902   CONTINUE
      DO IV=1,15
        MNCQCNV(1,IV) = FRCTOL
        MNCQCNV(2,IV) = ABSTOL
        RLXCQ(1:20,IV) = RLXPC
      ENDDO
      DO IV=1,12
        MNCPCNV(1,IV) = FRCTOL
        MNCPCNV(2,IV) = ABSTOL
        RLXCP(1:20,IV) = RLXPC
      ENDDO
! New coal stuff
      DO IV=1,NCLUT1
        MNCQCNVS(1,IV) = FRCTOL
        MNCQCNVS(2,IV) = ABSTOL
        RLXCQS(1:20,IV) = RLXPC
      ENDDO
      MNCPCNVS(1) = FRCTOL
      MNCPCNVS(2) = ABSTOL
      RLXCPS(1:20) = RLXPC
      RLXCPS(1:20) = RLXPC
      DO 903 IV=1,MNOTH
        MNOCNV(1,IV) = FRCTOL
        MNOCNV(2,IV) = ABSTOL
        RLXOTH(1:20,IV) = RLXPC
903   CONTINUE
      DO IV = 1 , 3
        MNZCNV(1,IV) = FRCTOL
        MNZCNV(2,IV) = ABSTOL
        RLXPCAP(1:20,IV) = RLXPC
      END DO

!  Read in the convergence values - percent and absolute, relaxation percentages,
!  and the variable name to check against M?VARS name array.  If there's a non-match,
!  don't transfer to the MN?CNV, and RELX arrays.
      IF (FUNITI .EQ. 1) THEN
        WRITE(6,*) ' Unable to open convergence file MNCNVRG.  Using global relaxation value.'
        RETURN
      ENDIF
      HOWMANYITR=MAX(MAXITR,MAXITRSR)+1
      DO WHILE (KEEPGOING)
        READ (FUNITI,'(A165)',END=999) CONV_LINE
        IF (CONV_LINE(1:1) .EQ. '!') CYCLE
        IF (CONV_LINE(1:1) .EQ. 'Q' .OR. CONV_LINE(1:1) .EQ. 'P' .OR. &
            CONV_LINE(1:1) .EQ. 'G' .OR. CONV_LINE(1:1) .EQ. 'S' .OR. &
            CONV_LINE(1:1) .EQ. 'C' .OR. &
            CONV_LINE(1:1) .EQ. 'O' .OR. CONV_LINE(1:1) .EQ. 'Z') THEN
                CONV_SET=CONV_LINE(1:1)
                CYCLE
        ENDIF
        READ (CONV_LINE,'(I2)') WHATTODO
        IF (WHATTODO .EQ. 99) THEN
          READ (CONV_LINE,*) WHATTODO,MNCNVF,MNCNVA,RLXT(1),REDUCE,CHECK
          DO II=2,20
            RLXT(II)=RLXT(1)*(REDUCE**REAL(II-1))
          ENDDO
        ELSE
          READ (CONV_LINE,*) II,MNCNVF,MNCNVA,(RLXX(IV),IV=1,WHATTODO),CHECK
          IF (HOWMANYITR .GT. WHATTODO) THEN
            ITR_PER_RLX=HOWMANYITR/WHATTODO
!  Initialize entire array to last value.
!  This loop allocates the relaxation parameters to the iterations; if there are
!  4 paprameters and 8 iterations, the first two iterations get the first parameter,
!  etc.  In cases of uneven divides, (3 parameters, 8 iterations), this loop only
!  would allocate parameters to the first 6 iterations.  This initialization will
!  fill the remaining iterations with the last parameter.
            RLXT=RLXX(WHATTODO)
            DO II=1,ITR_PER_RLX*WHATTODO
              RLXT(II)=RLXX(((II-1)/ITR_PER_RLX)+1)
            ENDDO
          ELSE
            DO II=1,HOWMANYITR
              RLXT(II)=RLXX(II)
            ENDDO
          ENDIF
        ENDIF
        MATCH = .FALSE.
        IF (CONV_SET .EQ. 'Q') THEN
          DO IV=1,MNUMQ
            IF (TRIM(CHECK) .EQ. TRIM(MQVARS(IV))) THEN
               MNQCNV(1,IV) = MNCNVF
               MNQCNV(2,IV) = MNCNVA
               RLXQ(1:20,IV) = RLXT(1:20)
               MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'P') THEN
          DO IV=1,MNUMP
            IF (TRIM(CHECK) .EQ. TRIM(MPVARS(IV))) THEN
               MNPCNV(1,IV) = MNCNVF
               MNPCNV(2,IV) = MNCNVA
               RLXP(1:20,IV) = RLXT(1:20)
               MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'G') THEN
          DO IV=1,3
            IF (TRIM(CHECK) .EQ. TRIM(MUQVARS(IV))) THEN
               MNUQCNV(1,IV) = MNCNVF
               MNUQCNV(2,IV) = MNCNVA
               RLXUQ(1:20,IV) = RLXT(1:20)
               MATCH = .TRUE.
            ENDIF
            IF (TRIM(CHECK) .EQ. TRIM(MUPVARS(IV))) THEN
               MNUPCNV(1,IV) = MNCNVF
               MNUPCNV(2,IV) = MNCNVA
               RLXUP(1:20,IV) = RLXT(1:20)
               MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'S') THEN                    ! seasonal natural gas, each season separate
          DO IV=1,3
            IF (TRIM(CHECK) .EQ. TRIM(SGASQVARS(IV))) THEN
               MNGASQCNV(1,IV) = MNCNVF
               MNGASQCNV(2,IV) = MNCNVA
               RLXGASQ(1:20,IV) = RLXT(1:20)
               MATCH = .TRUE.
            ENDIF
            IF (TRIM(CHECK) .EQ. TRIM(SGASPVARS(IV))) THEN
               MNGASPCNV(1,IV) = MNCNVF
               MNGASPCNV(2,IV) = MNCNVA
               RLXGASP(1:20,IV) = RLXT(1:20)
               MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'C') THEN
          DO IV=1,NCLUT1
            IF (TRIM(CHECK) .EQ. TRIM(MCQVARSS(IV))) THEN
               MNCQCNVS(1,IV) = MNCNVF
               MNCQCNVS(2,IV) = MNCNVA
               RLXCQS(1:20,IV) = RLXT(1:20)
               MATCH = .TRUE.
            ENDIF
          ENDDO
          DO IV=1,2
            IF (TRIM(CHECK) .EQ. TRIM(MCPVARSS(IV))) THEN
               MNCPCNVS(1) = MNCNVF
               MNCPCNVS(2) = MNCNVA
               RLXCPS(1:20) = RLXT(1:20)
               MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'O') THEN
          DO IV=1,MNOTH
            IF (TRIM(CHECK) .EQ. TRIM(MOVARS(IV))) THEN
               MNOCNV(1,IV) = MNCNVF
               MNOCNV(2,IV) = MNCNVA
               RLXOTH(1:20,IV) = RLXT(1:20)
               MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE IF (CONV_SET .EQ. 'Z') THEN
          DO IV=1,3
            IF (TRIM(CHECK) .EQ. TRIM(MZVARS(IV))) THEN
               MNZCNV(1,IV) = MNCNVF
               MNZCNV(2,IV) = MNCNVA
               RLXPCAP(1:20,IV) = RLXT(1:20)
               MATCH = .TRUE.
            ENDIF
          ENDDO
        ELSE
          WRITE(6,*) ' Unknown convergence set type = ',CONV_SET
        ENDIF
        IF (MATCH .EQ. .FALSE.) WRITE(6,*) '  No match found for convergence variable ',CHECK
        CYCLE
999     KEEPGOING=.FALSE.
      ENDDO

!   Close the convergence values file.
      FUNITI=FILE_MGR('C',FNAMEI18,NEW)

!  Read in conversion factors from excel workbook in XML format
      NEW=.FALSE.
      FNAMEI18='MNFACTORX'
      FUNITI = FILE_MGR('O',FNAMEI18,NEW)

      RDCVFACT = RTOVALUE('RDCVFACT',1)

      IF (FUNITI .EQ. 0) THEN
        WRITE(6,'(2a)') ' Unable to open conversion factor file.', &
           '  You are stuck with the values from the previous run.'
      ELSE IF (RDCVFACT .EQ. 0) THEN
        WRITE(6,'(//,a)') '  No reading of conversion factors requested this run, but reading last conversion factor year.'
!  But read the historical year, because we use that in DOCVFACTS
        CALL ReadRngXLSX(FUNITI,'mncvfact')  ! sheet name is mncvfact
        CLOSE(FUNITI)
        RNAME = 'HISTYR'
        CALL GETRNGI(RNAME,HISTYR,1,1,1)
        WRITE(6,'("  The historical year for conversion factors is ",I4,//)') HISTYR
        HISTYR=HISTYR-BASEYR+1
      ELSE
!  Copy each range from worksheet data area to variables
!  GETRNGI : Copies an Integer*2 variable from the worksheet
!            data area into the variable.  The variable
!            dimensions are passed as the 3rd,4th,&5th
!            arguments, (eg, ... 1,1,1).
!            A variable with dimesions of 1,1,1 is a scalar.
!            A variable with dimensions of 26,1,1 is a one-
!            dimensional array with 26 elements.
!  GETRNGR:  Copies a REAL variable from the worksheet
!            data area into the variable.

!  Call subroutine to read all defined ranges from worksheet
        CALL ReadRngXLSX(FUNITI,'mncvfact')  ! sheet name is mncvfact
        CLOSE(FUNITI)
        RNAME = 'HISTYR'
        CALL GETRNGI(RNAME,HISTYR,1,1,1)
        HISTYR=HISTYR-BASEYR+1
!  First, we've got some scalars to make into yearly variables.  We'll use CFASQ as the read-in variable.
        RNAME = 'CFDSLQ'
        CALL GETRNGR(RNAME,CFASQ,1,1,1)
        DO II = 1,HISTYR
          CFDSLQ(II)=CFASQ
        ENDDO
        RNAME = 'CFDSUQ'
        CALL GETRNGR(RNAME,CFASQ,1,1,1)
        DO II = 1,HISTYR
          CFDSUQ(II)=CFASQ
        ENDDO
        RNAME = 'CFTGQ'
        CALL GETRNGR(RNAME,CFASQ,1,1,1)
        DO II = 1,HISTYR
          CFTGQ(II)=CFASQ
        ENDDO
        RNAME = 'CFRGQ'
        CALL GETRNGR(RNAME,CFASQ,1,1,1)
        DO II = 1,HISTYR
          CFRGQ(II)=CFASQ
        ENDDO
        RNAME = 'CFE85Q'
        CALL GETRNGR(RNAME,CFE85Q,HISTYR,1,1)
        RNAME = 'CFM85Q'
        CALL GETRNGR(RNAME,CFASQ,1,1,1)
        DO II = 1,HISTYR
          CFM85Q(II)=CFASQ
        ENDDO
        RNAME = 'CFBMQ'
        CALL GETRNGR(RNAME,CFASQ,1,1,1)
        DO II = 1,HISTYR
          CFBMQ(II)=CFASQ
        ENDDO
        RNAME = 'CFJOULE'
        CALL GETRNGR(RNAME,CFJOULE,1,1,1)
        RNAME = 'CFCORN'
        CALL GETRNGR(RNAME,CFCORN,1,1,1)
        RNAME = 'CFCELL'
        CALL GETRNGR(RNAME,CFCELL,1,1,1)
! Setting CFASQ after using it as the placeholder for scalars above.
        RNAME = 'CFASQ'
        CALL GETRNGR(RNAME,CFASQ,1,1,1)
        RNAME = 'CFBUQ'
        CALL GETRNGR(RNAME,CFBUQ,1,1,1)
        RNAME = 'CFDSQ'
        CALL GETRNGR(RNAME,CFDSQ,1,1,1)
        DO II = 1,HISTYR                                      ! Carb rules phase in as follows:
          CFDSCQ(II) = CFDSLQ(II)                             !      low sulfur July 1993-July 2006
          IF (II+BASEYR-1 .LE. 1993) CFDSCQ(II) = CFDSQ       !      start in July 1993
          IF (II+BASEYR-1 .GT. 2006) CFDSCQ(II) = CFDSUQ(II)  !      ultra low sulfur after July 2006
        ENDDO
        RNAME = 'CFEEQ'
        CALL GETRNGR(RNAME,CFEEQ,1,1,1)
        RNAME = 'CFIBQ'
        CALL GETRNGR(RNAME,CFIBQ,1,1,1)
        RNAME = 'CFJFK'
        CALL GETRNGR(RNAME,CFJFK,1,1,1)
        RNAME = 'CFJFN'
        CALL GETRNGR(RNAME,CFJFN,1,1,1)
        RNAME = 'CFKSQ'
        CALL GETRNGR(RNAME,CFKSQ,1,1,1)
        RNAME = 'CFPPQ'
        CALL GETRNGR(RNAME,CFPPQ,1,1,1)
        RNAME = 'CFPCQ'
        CALL GETRNGR(RNAME,CFPCQ,1,1,1)
        RNAME = 'CFCCQ'
        CALL GETRNGR(RNAME,CFCCQ,HISTYR,1,1)
        DO II = HISTYR+1,LASTYR
          CFCCQ(II)=CFCCQ(HISTYR)
        ENDDO
        RNAME = 'CFPRQ'
        CALL GETRNGR(RNAME,CFPRQ,1,1,1)
        RNAME = 'CFRSQ'
        CALL GETRNGR(RNAME,CFRSQ,1,1,1)
        RNAME = 'CFSGQ'
        CALL GETRNGR(RNAME,CFSGQ,1,1,1)
        RNAME = 'CFAVQ'
        CALL GETRNGR(RNAME,CFAVQ,1,1,1)
        RNAME = 'CFLUQ'
        CALL GETRNGR(RNAME,CFLUQ,1,1,1)
        RNAME = 'CFNPQ'
        CALL GETRNGR(RNAME,CFNPQ,1,1,1)
        RNAME = 'CFUSQ'
        CALL GETRNGR(RNAME,CFUSQ,1,1,1)
        RNAME = 'CFWXQ'
        CALL GETRNGR(RNAME,CFWXQ,1,1,1)
        RNAME = 'CFMSQ'
        CALL GETRNGR(RNAME,CFMSQ,1,1,1)
        RNAME = 'CFMEQT'
        CALL GETRNGR(RNAME,CFMEQT,1,1,1)
        RNAME = 'CFPET'
        CALL GETRNGR(RNAME,CFPET,1,1,1)
        RNAME = 'CFOGQ'
        CALL GETRNGR(RNAME,CFOGQ,1,1,1)
        RNAME = 'CFLGQ'
        CALL GETRNGR(RNAME,CFLGQ,HISTYR,1,1)
        RNAME = 'CFCBOB'
        CALL GETRNGR(RNAME,CFCBOB,HISTYR,1,1)
        RNAME = 'CFRBOB'
        CALL GETRNGR(RNAME,CFRBOB,HISTYR,1,1)
        RNAME = 'CFCBQ'
        CALL GETRNGR(RNAME,CFCBQ,HISTYR,1,1)
        RNAME = 'CFMGQ'
        CALL GETRNGR(RNAME,CFMGQ,HISTYR,1,1)
        RNAME = 'CFAR3'
        CALL GETRNGR(RNAME,CFAR3,HISTYR,1,1)
        RNAME = 'CFMN3'
        CALL GETRNGR(RNAME,CFMN3,HISTYR,1,1)
        RNAME = 'CFGO3'
        CALL GETRNGR(RNAME,CFGO3,HISTYR,1,1)
        RNAME = 'CFOTQ'
        CALL GETRNGR(RNAME,CFOTQ,HISTYR,1,1)
        RNAME = 'CFPFQ'
        CALL GETRNGR(RNAME,CFPFQ,HISTYR,1,1)
        RNAME = 'CFDSRS'
        CALL GETRNGR(RNAME,CFDSRS,HISTYR,1,1)
        RNAME = 'CFDSCM'
        CALL GETRNGR(RNAME,CFDSCM,HISTYR,1,1)
        RNAME = 'CFDSIN'
        CALL GETRNGR(RNAME,CFDSIN,HISTYR,1,1)
        RNAME = 'CFDSTR'
        CALL GETRNGR(RNAME,CFDSTR,HISTYR,1,1)
        RNAME = 'CFDSEL'
        CALL GETRNGR(RNAME,CFDSEL,HISTYR,1,1)
        RNAME = 'CFDSQT'
        CALL GETRNGR(RNAME,CFDSQT,HISTYR,1,1)
        RNAME = 'CFNGU'
        CALL GETRNGR(RNAME,CFNGU,HISTYR,1,1)
        RNAME = 'CFNGN'
        CALL GETRNGR(RNAME,CFNGN,HISTYR,1,1)
        RNAME = 'CFNGP'
        CALL GETRNGR(RNAME,CFNGP,HISTYR,1,1)
        RNAME = 'CFNGC'
        CALL GETRNGR(RNAME,CFNGC,HISTYR,1,1)
        RNAME = 'CFNGI'
        CALL GETRNGR(RNAME,CFNGI,HISTYR,1,1)
        RNAME = 'CFNGE'
        CALL GETRNGR(RNAME,CFNGE,HISTYR,1,1)
!       RNAME = 'CFNGL'
!       CALL GETRNGR(RNAME,CFNGL,HISTYR,1,1)
        RNAME = 'CFCNGQ'
        CALL GETRNGR(RNAME,CFCNGQ,HISTYR,1,1)
        RNAME = 'CFNGCL'
        CALL GETRNGR(RNAME,CFNGCL,HISTYR,1,1)
        RNAME = 'CFIMPRD'
        CALL GETRNGR(RNAME,CFIMPRD,HISTYR,1,1)
        RNAME = 'CFEXPRD'
        CALL GETRNGR(RNAME,CFEXPRD,HISTYR,1,1)
        RNAME = 'CFTPQ'
        CALL GETRNGR(RNAME,CFTPQ,HISTYR,1,1)
        RNAME = 'CFETQ'
        CALL GETRNGR(RNAME,CFETQ,HISTYR,1,1)
        RNAME = 'CFBIOBUTE'
        CALL GETRNGR(RNAME,CFBIOBUTE,HISTYR,1,1)
        DO II = HISTYR+1,LASTYR
          CFETQ(II)=CFETQ(HISTYR)
          CFBIOBUTE(II)=CFBIOBUTE(HISTYR)
        ENDDO
! have not created CFDLTSWT, CFFLTSWT variables yet, but reading in to test read
        RNAME = 'CFFLTSWT'
        CALL GETRNGR(RNAME,CFCRDEXP,HISTYR,1,1)
        RNAME = 'CFDLTSWT'
        CALL GETRNGR(RNAME,CFCRDEXP,HISTYR,1,1)
        RNAME = 'CFCRDLTSWT'
        CALL GETRNGR(RNAME,CFCRDLTSWT,HISTYR,1,1)
        RNAME = 'CFCRDLTSOUR'
        CALL GETRNGR(RNAME,CFCRDLTSOUR,HISTYR,1,1)
        RNAME = 'CFCRDMD2SOUR'
        CALL GETRNGR(RNAME,CFCRDMD2SOUR,HISTYR,1,1)
        RNAME = 'CFCRDMDSOUR'
        CALL GETRNGR(RNAME,CFCRDMDSOUR,HISTYR,1,1)
        RNAME = 'CFCRDHVSWT'
        CALL GETRNGR(RNAME,CFCRDHVSWT,HISTYR,1,1)
        RNAME = 'CFCRDHVSOUR'
        CALL GETRNGR(RNAME,CFCRDHVSOUR,HISTYR,1,1)
        RNAME = 'CFCRDCA'
        CALL GETRNGR(RNAME,CFCRDCA,HISTYR,1,1)
        RNAME = 'CFCRDSYN'
        CALL GETRNGR(RNAME,CFCRDSYN,HISTYR,1,1)
        RNAME = 'CFCRDDILBIT'
        CALL GETRNGR(RNAME,CFCRDDILBIT,HISTYR,1,1)
        RNAME = 'CFCRDLT2SWT'
        CALL GETRNGR(RNAME,CFCRDLT2SWT,HISTYR,1,1)
        RNAME = 'CFCRDLSCOND'
        CALL GETRNGR(RNAME,CFCRDLSCOND,HISTYR,1,1)
        RNAME = 'CFCRDDOM'
        CALL GETRNGR(RNAME,CFCRDDOM,HISTYR,1,1)
        RNAME = 'CFCRDIMP'
        CALL GETRNGR(RNAME,CFCRDIMP,HISTYR,1,1)
        RNAME = 'CFCRDEXP'
        CALL GETRNGR(RNAME,CFCRDEXP,HISTYR,1,1)
!  for CFVEGGIE and CFBIOD, carry values throughout forecast period
        RNAME = 'CFBIOD'
        CALL GETRNGR(RNAME,CFBIOD,HISTYR,1,1)
        DO II = HISTYR+1,LASTYR
          CFBIOD(II)=CFBIOD(HISTYR)
        ENDDO
        RNAME = 'CFVEGGIE'
        CALL GETRNGR(RNAME,CFVEGGIE,HISTYR,1,1)
        DO II = HISTYR+1,LASTYR
          CFVEGGIE(II)=CFVEGGIE(HISTYR)
        ENDDO
        RNAME = 'CFETFAC'
        CALL GETRNGR(RNAME,CFETFAC,HISTYR,1,1)
        DO II = HISTYR+1,LASTYR
          CFETFAC(II)=CFETFAC(HISTYR)
        ENDDO
        RNAME = 'CFELQ'
        CALL GETRNGR(RNAME,CFELQ,1,1,1)
        DO II = 1,LASTYR
          CFFTLIQ(1,II)=CFNPQ * 0.92
          CFFTLIQ(2,II)=CFJFN * 0.92
          CFFTLIQ(3,II)=CFJFK * 0.92
          CFFTLIQ(4,II)=CFBIOD(II)       !  CFBIOD is about 8% less than distillate
!  overriding what was read in for these (input file has values extracted from LFMM, these are as below):
          CFAR3(II) = CFRSQ      !  set atmospheric residuum to residual fuel oil
          CFMN3(II) = CFJFN      !  set medium naphtha to naphtha jet fuel
          CFGO3(II) = CFDSQ      !  set gas oil to straight distillate
          CFGOP(II) = CFDSQ      !  set gas oil to straight distillate
        ENDDO

        WEIGHTS=0.0
        RNAME = 'WEIGHTLPG1'
        CALL GETRNGR(RNAME,WEIGHTS,5,HISTYR,1)
        DO II=1,HISTYR
           IF ((WEIGHTS(1,II)+WEIGHTS(2,II)+WEIGHTS(3,II)+WEIGHTS(4,II)+WEIGHTS(5,II)) .NE. 0.0) &
           CFNGL(II) = (WEIGHTS(5,II)*CFPPQ+WEIGHTS(1,II)*CFEEQ+WEIGHTS(2,II)*CFPRQ+ &
                        WEIGHTS(3,II)*CFBUQ+WEIGHTS(4,II)*CFIBQ) / (WEIGHTS(1,II)+ &
                        WEIGHTS(2,II)+WEIGHTS(3,II)+WEIGHTS(4,II)+WEIGHTS(5,II))
        ENDDO
        WEIGHTS=0.0
        RNAME = 'WEIGHTLPG2'
        CALL GETRNGR(RNAME,WEIGHTS,5,HISTYR,1)
        DO II=1,HISTYR
           IF ((WEIGHTS(1,II)+WEIGHTS(2,II)+WEIGHTS(3,II)+WEIGHTS(4,II)) .NE. 0.0) &
           CFLGQ(II) = (WEIGHTS(1,II)*CFEEQ+WEIGHTS(2,II)*CFPRQ+ &
                        WEIGHTS(3,II)*CFBUQ+WEIGHTS(4,II)*CFIBQ) / &
                       (WEIGHTS(1,II)+WEIGHTS(2,II)+WEIGHTS(3,II)+WEIGHTS(4,II))
        ENDDO
        WEIGHTS=0.0
        RNAME = 'WEIGHTPETF'
        CALL GETRNGR(RNAME,WEIGHTS,5,HISTYR,1)
        DO II=1,HISTYR
           IF ((WEIGHTS(1,II)+WEIGHTS(2,II)) .NE. 0.0) &
           CFPFQ(II) = (WEIGHTS(1,II)*CFNPQ+WEIGHTS(2,II)*CFDSQ) / &
                       (WEIGHTS(1,II)+WEIGHTS(2,II))
        ENDDO
        WEIGHTS=0.0
        RNAME = 'WEIGHTJETF'
        CALL GETRNGR(RNAME,WEIGHTS,5,HISTYR,1)
        DO II=1,HISTYR
           IF ((WEIGHTS(1,II)+WEIGHTS(2,II)) .NE. 0.0) &
           CFJFQ(II) = (WEIGHTS(1,II)*CFJFN+WEIGHTS(2,II)*CFJFK) / &
                       (WEIGHTS(1,II)+WEIGHTS(2,II))
        ENDDO
        WEIGHTS=0.0
        RNAME = 'WEIGHTUOIL'
        CALL GETRNGR(RNAME,WEIGHTS,5,HISTYR,1)
        DO II=1,HISTYR
           IF ((WEIGHTS(1,II)+WEIGHTS(2,II)+WEIGHTS(3,II)+WEIGHTS(4,II)) .NE. 0.0) &
           CFIMUO(II) = (WEIGHTS(1,II)*CFNPQ+WEIGHTS(2,II)*CFKSQ+WEIGHTS(3,II)*CFRSQ+ &
                         WEIGHTS(4,II)*CFRSQ) / &
                        (WEIGHTS(1,II)+WEIGHTS(2,II)+WEIGHTS(3,II)+WEIGHTS(4,II))
        ENDDO
        RNAME = 'API'
        CALL GETRNGR(RNAME,API_IN,3,1,1)
        RNAME = 'CRUDEQ'
        CALL GETRNGR(RNAME,CRUDEQ,5,4,1)

        DO II=1,HISTYR
           APILTSW(2,II) = CFCRDLTSWT(II)
           APILTSO(2,II) = CFCRDLTSOUR(II)
           APIMMSO(2,II) = CFCRDMD2SOUR(II)
           APIMDSO(2,II) = CFCRDMDSOUR(II)
           APIHVSW(2,II) = CFCRDHVSWT(II)
           APIHVSO(2,II) = CFCRDHVSOUR(II)
           APICA(2,II)   = CFCRDCA(II)
           APISYN(2,II)  = CFCRDSYN(II)
           APIDIL(2,II)  = CFCRDDILBIT(II)
           APILLSW(2,II) = CFCRDLT2SWT(II)
           API50PL(2,II) = CFCRDLSCOND(II)
        ENDDO
! Put the last historical value in all of the forecast years, except for stuff LFMM calculates
! (like CFIMPRD, CFEXPRD, CFMGQ)
! This uses the last historical year for the projections.
! If we didn't do this, the value in the projections would be an old number (providing history changes).
        DO II=HISTYR+1,MNUMYR
          IF (EXO .EQ. 1) THEN
            CFPFQ(II)   = CFPFQ(HISTYR)
            CFJFQ(II)   = CFJFQ(HISTYR)
            CFCBQ(II)   = CFCBQ(HISTYR)
            CFCBOB(II)  = CFCBOB(HISTYR)
            CFRBOB(II)  = CFCBOB(HISTYR)
            CFDSLQ(II)  = CFDSLQ(HISTYR)
            CFDSUQ(II)  = CFDSUQ(HISTYR)
            CFDSCQ(II)  = CFDSCQ(HISTYR)
            CFE85Q(II)  = CFE85Q(HISTYR)
! Don't do these, they are calculated each year as weighted averages.
!           CFNGL(II)   = CFNGL(HISTYR)
!           CFLGQ(II)   = CFLGQ(HISTYR)
!           CFOTQ(II)   = CFOTQ(HISTYR)
!           CFTPQ(II)   = CFTPQ(HISTYR)
!           CFIMUO(II)  = CFIMUO(HISTYR)
!           CFAR3(II)   = CFAR3(HISTYR)
!           CFMN3(II)   = CFMN3(HISTYR)
!           CFGO3(II)   = CFGO3(HISTYR)
!           CFCRDDOM(II)= CFCRDDOM(HISTYR)
!           CFCRDIMP(II)= CFCRDIMP(HISTYR)
!           CFCRDEXP(II)= CFCRDEXP(HISTYR)
!           CFDSRS(II)  = CFDSRS(HISTYR)
!           CFDSCM(II)  = CFDSCM(HISTYR)
!           CFDSIN(II)  = CFDSIN(HISTYR)
!           CFDSTR(II)  = CFDSTR(HISTYR)
!           CFDSEL(II)  = CFDSEL(HISTYR)
!           CFDSQT(II)  = CFDSQT(HISTYR)
!           CFM85Q(II)  = CFM85Q(HISTYR)
!           CFETFAC(II) = CFETFAC(HISTYR)      uncomment this loop when the variable gets created!
          ENDIF
          IF (EXG .EQ. 1) THEN
             CFNGU(II)=CFNGU(HISTYR)
             CFNGN(II)=CFNGN(HISTYR)
             CFNGC(II)=CFNGC(HISTYR)
             CFNGP(II)=CFNGP(HISTYR)
             CFNGI(II)=CFNGI(HISTYR)
             CFNGE(II)=CFNGE(HISTYR)
             CFCNGQ(II)=CFCNGQ(HISTYR)
             CFNGCL(II)=CFNGCL(HISTYR)
          ENDIF
        ENDDO
      ENDIF              ! if file was opened

! Use fall-back values for these if 0
      DO II=1,MNUMYR
         IF (CFUBAQ(II) .EQ. 0.0) CFUBAQ(II) = CFDSQ
         IF (CFDSCQ(II) .EQ. 0.0) CFDSCQ(II) = CFDSUQ(II)
         IF (CFFTLIQ(1,II) .EQ. 0.0) CFFTLIQ(1,II) = CFNPQ
         IF (CFFTLIQ(2,II) .EQ. 0.0) CFFTLIQ(2,II) = CFNPQ
         IF (CFFTLIQ(3,II) .EQ. 0.0) CFFTLIQ(3,II) = CFKSQ
         IF (CFFTLIQ(4,II) .EQ. 0.0) CFFTLIQ(4,II) = CFDSQ
         IF (APICAMG(2,II) .EQ. 0.0) APICAMG(2,II) = CFRGQ(II)
         IF (CFCBOB(II) .EQ. 0.0) CFCBOB(II) = 5.22228
         IF (CFRBOB(II) .EQ. 0.0) CFRBOB(II) = CFCBOB(II)
         IF (CFCBQ(II) .EQ. 0.0) CFCBQ(II) = CFCBOB(II)
         IF (CFAR3(II) .EQ. 0.0) CFAR3(II) = CFRSQ
         IF (CFMN3(II) .EQ. 0.0) CFMN3(II) = CFNPQ
         IF (CFGO3(II) .EQ. 0.0) CFGO3(II) = CFDSQ
         IF (CFNGP(II) .EQ. 0.0) CFNGP(II) = CFNGC(II)
      ENDDO

! Read in Regional Historical values for energy consumption based on
! the State Energy Data System (SEDS). This global data is formatted for
! input via the filer routines.  Therefore, other global data can be
! input through this mechanism to override or supplement the data in
! the Restart File.

      NEW=.FALSE.
      FNAMEI18='QSBLK'  ! name based on the common block and include file used
      FUNITI = FILE_MGR('O',FNAMEI18,NEW)

      IF (FUNITI .EQ. 0) THEN
        WRITE(6,*) ' Unable to open the SEDS/Supplementary Global Data file,'
        WRITE(6,*) ' so the global data already read from the restart file will not be changed.'
      ELSE
        FNAMEI= ' '
        FNAMEO=' '
        FUNITO=6
        FRTYPE=2
        FSOURC=1
        FUNFMT=0
! +++   DETERMINE WHETHER FILE WAS OPENED AS FORMATTED OR UNFORMATTED BY FILEMGR AND SET FUNFMT VARIABLE ACCORDINGLY
        INQUIRE(FUNITI,FORM=FUNFMTC)
        IF(FUNFMTC.EQ.'FORMATTED') FUNFMT=0
        IF(FUNFMTC.EQ.'UNFORMATTED') FUNFMT=1

        CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

        FUNITI=FILE_MGR('C',FNAMEI18,NEW)

        if(fretcd.eq.0) then
          write(6,'(A)') 'Regional fuel consumption history (from SEDS) input completed.'
        else
          write(6,'(A)') 'Error reading SEDS input file. Filer return code, FRETCD, =',fretcd
        endif

      ENDIF

!  read U.S. Tax Code Section 45Q input file in the form of AIMMS composite table
      FNAMEI18='TCS45QIN'      ! U.S. Tax Code Section 45Q input file for tax credits
      FUNITI = FILE_MGR('O',FNAMEI18,NEW)
      IF (FUNITI .EQ. 0) THEN
        WRITE(6,'(A)') ' Unable to open the 45Q tax credit file, so RESTART file values are used'
      ELSE
        FRTYPE=2           ! read from restart file/aimms project
        FSOURC=1           ! variables identified in the file
        FNAMEI=' '         ! blank when opened outside of filer
        FUNFMT=7           ! format indicator for AIMMS text format, composite tables
        CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      ENDIF
      FUNITI=FILE_MGR('C',FNAMEI18,NEW)
! convert tax credits from nominal to 1987 dollars, from start year till 2026, then values are in real 2026 dollars
      I_S = I_45Q_SYR - BASEYR + 1
      I_R = 2026 - BASEYR + 1
      DO II=I_S,I_R

        WRITE(6,2317) CURIRUN, CURCALYR, CURITR, I_45Q_SYR, I_45Q_DURATION, I_45Q_LYR_NEW, I_45Q_LYR_RET, BASEYR, I_S, I_R, II, &
           CCS_EOR_45Q(II), CCS_EOR_45Q(II) * MC_JPGDP(-2) / MC_JPGDP(II), CCS_SALINE_45Q(II), CCS_SALINE_45Q(II) * MC_JPGDP(-2) / MC_JPGDP(II), MC_JPGDP(-2), MC_JPGDP(II)
 2317   FORMAT(1X,"45Q_Info",11(",",I5),6(",",F21.6))

        CCS_EOR_45Q(II)= CCS_EOR_45Q(II) * MC_JPGDP(-2) / MC_JPGDP(II)
        CCS_SALINE_45Q(II) = CCS_SALINE_45Q(II)  * MC_JPGDP(-2) / MC_JPGDP(II)
      ENDDO
      I_L = MIN(I_45Q_LYR_NEW + I_45Q_DURATION - BASEYR , MNUMYR)
      Do II = I_R + 1 , I_L

        WRITE(6,2317) CURIRUN, CURCALYR, CURITR, I_45Q_SYR, I_45Q_DURATION, I_45Q_LYR_NEW, I_45Q_LYR_RET, BASEYR, I_R, I_L, II, &
           CCS_EOR_45Q(II), CCS_EOR_45Q(II) * MC_JPGDP(-2) / MC_JPGDP(I_R), CCS_SALINE_45Q(II), CCS_SALINE_45Q(II) * MC_JPGDP(-2) / MC_JPGDP(I_R), MC_JPGDP(-2), MC_JPGDP(I_R)

        CCS_EOR_45Q(II) = CCS_EOR_45Q(II) * MC_JPGDP(-2) / MC_JPGDP(I_R)
        CCS_SALINE_45Q(II) = CCS_SALINE_45Q(II)  * MC_JPGDP(-2) / MC_JPGDP(I_R)
      ENDDO
      Do II = I_L + 1 , MNUMYR
        CCS_EOR_45Q(II) = 0.0
        CCS_SALINE_45Q(II) = 0.0
      ENDDO

      FNAMEI18='MNRISK  '
      FUNITI=FILE_MGR('O',FNAMEI18,NEW)
      IF (FUNITI .GT. 0) THEN
        DO WHILE (STARTHERE .NE. '@')
          READ(FUNITI,'(A1)') STARTHERE
        ENDDO
        extrarisk=0.0
        DO WHILE (RISKYEAR .NE. 9999)
          READ(FUNITI,'(I4,1X,F12.8)') RISKYEAR, number
          IF (RISKYEAR .GE. BASEYR .AND. RISKYEAR .LE. ENDYR) THEN
              extrarisk(riskyear) = number
          ELSEIF (RISKYEAR .NE. 9999) THEN
              write(6,'( "   Bad risk year=",I4)') RISKYEAR
          ELSE
              write(6,'(" ending read of mnrisk")')
          ENDIF
        ENDDO
        FUNITI=FILE_MGR('C',FNAMEI18,NEW)
      ELSE
        WRITE(6,*) ' Open of file MNRISK failed.'
      ENDIF

!  call routine to read in variables from STEO file
      CALL READ_ALL_STEO_VARS

      RETURN
      END

! Initialize_API reads the table assigning Btu conversion factors to API gravities from:
!   Thermal Properties of Petroleum Products    November 9, 1929
!  input file is apitable.txt   (comma-delimited)
!  this is for function API_TO_BTU to look through and interpolate from

      SUBROUTINE INITIALIZE_API
      IMPLICIT NONE

      include 'apiblk'

      CHARACTER*80 LINE
      LOGICAL STILL_COMMENTS, NOT_END_OF_DATA
      REAL API_HOLD, BTU_PER_GAL_HOLD
      INTEGER I, API_UNIT

      CHARACTER*18 API_FILENAME/'APITABLE          '/
      LOGICAL NEW/.FALSE./
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      STILL_COMMENTS = .TRUE.
      NOT_END_OF_DATA = .TRUE.
      API_GRAV=0.0
      BTU_PER_GAL=0.0
      API_UNIT=FILE_MGR('O',API_FILENAME,NEW)
!  read comments at top of file ending with line that has an @ in column 1
      DO WHILE (STILL_COMMENTS)
         READ (API_UNIT,'(A80)') LINE
         IF (LINE(1:1) .EQ. '@') STILL_COMMENTS = .FALSE.
      ENDDO
!  and now read numbers in comma-delimited format (can read list-directed)
      API_COUNT=0
      DO WHILE (NOT_END_OF_DATA)
         READ (API_UNIT,*) API_HOLD, BTU_PER_GAL_HOLD
         IF (API_HOLD .NE. 9999) THEN
             API_COUNT=API_COUNT+1
             API_GRAV(API_COUNT) = API_HOLD
             BTU_PER_GAL(API_COUNT) = BTU_PER_GAL_HOLD
         ELSE
             NOT_END_OF_DATA = .FALSE.
         ENDIF
         IF (API_COUNT .GT. MAX_API_LINES) THEN
             NOT_END_OF_DATA = .FALSE.
!   "eg" command should flag "Encountered" in the following write
             WRITE(6,'(" Encountered possibly early end of read:  MAX_API_LINES exceeded.")')
         ENDIF
      ENDDO
      API_UNIT=FILE_MGR('C',API_FILENAME,NEW)
      DO I=1,API_COUNT
         MM_BTU_PER_BBL = BTU_PER_GAL(I) * 42 / 1000000.
      ENDDO
      RETURN
      END

!*****************************************************************
!*    Subroutine ZEROR(X,N)
!*
!*    Zeroes array X with adjustable dimension size N
!*****************************************************************
      SUBROUTINE ZEROR(X,N)
      IMPLICIT NONE

      INTEGER I,N
      REAL X(N)

      DO 10 I=1,N
         X(I)=0.
10    CONTINUE
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
!*****************************************************************
!*    Subroutine MBLOCKD
!*
!*    This routine functions as Block Data
!*****************************************************************
      SUBROUTINE MBLOCKD
      IMPLICIT NONE
      include 'parametr'
      include 'emmparm'
      include 'cdsparms'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'coalemm'
      include 'coalprc'
      include 'pqchar'

! +++ MPVARS, MQVARS, MUPVARS, MUQVARS declared in (PQCHAR)
      CHARACTER*6 MPVARSL(MNUMP),MQVARSL(MNUMQ)
      CHARACTER*9 MUPVARSL(3),MUQVARSL(3)
      CHARACTER*7 MCQVARSLS(NCLUT1)

!  MOVARS holds the variable names (approximately) for the additional convergence tests.  If more than
!  30 variables get tested, then increase the size of the array (MNOTH), and update the DO loop below.

      CHARACTER*9  MOVARSL(MNOTH)
      DATA MOVARSL/'MC_GDPR','IT_WOP','RFQICRD','RFPQIPRDT', &                        ! 1- 4
         'EMETAX','EMELPSO2','ECP_PHG','OGWPRNG', &                                   ! 5- 8
         'OGCNPPRD1','OGCNPPRD2','PLNGIMP03','PLNGIMP04','PLNGIMP05','PLNGIMP06', &   ! 9-14
         'PLNGIMP07','PLNGIMP08','PLNGIMP09','PLNGIMP10','PLNGIMP11','PLNGIMP12', &   !15-20
         'PBMET01'  ,'PBMET02'  ,'PBMET03'  ,'PBMET04'  ,'PBMET05'  ,             &   !21-25
         'PBMET06'  ,'PBMET07'  ,'PBMET08'  ,'PBMET09'  ,'PBMET10'  ,             &   !26-30
         'QBMET01'  ,'QBMET02'  ,'QBMET03'  ,'QBMET04'  ,'QBMET05'  ,             &   !31-35
         'QBMET06'  ,'QBMET07'  ,'QBMET08'  ,'QBMET09'  ,'QBMET10'  ,             &   !36-40
         'Unused01' ,'Unused02' ,'GLBCRDDMD',                                     &   !41-43
         'PSO2_00_1','PSO2_01_1','PSO2_02_1','PSO2_03_1','PSO2_04_1','PSO2_05_1', &   !44-49
         'PSO2_06_1','PSO2_07_1','PSO2_08_1','PSO2_09_1','PSO2_10_1',             &   !50-54
         'PSO2_00_2','PSO2_01_2','PSO2_02_2','PSO2_03_2','PSO2_04_2','PSO2_05_2', &   !55-60
         'PSO2_06_2','PSO2_07_2','PSO2_08_2','PSO2_09_2','PSO2_10_2'/                 !61-65

!  Coal Productive Capacity
      CHARACTER*8  MZVARSL(3)
      DATA MZVARSL/'XCL_PCAP','EM_CL_CF','E_CLBTUs'/

! +++ QUANTITY Variables:
      DATA MQVARSL/ &
       'QELRS','QELCM','QELTR','QELIN','QELRF','QELHM','QELAS', &
       'QGFRS','QGFCM','QGFTR','QGFIN','QGFRF','QGFEL','QGFHM','QGFAS', &
       'QGIRS','QGICM','QGITR','QGIIN','QGIRF','QGIEL','QGIHM','QGIAS', &
       'QNGRS','QNGCM','QNGTR','QNGIN','QNGRF','QNGEL','QNGHM','QNGAS', &
       'QGPTR','QLPIN', &
       'QCLRS','QCLCM','QCLIN','QCLRF','QCLEL','QCLSN','QCLHM','QCLAS', &
       'QMCIN', &
       'QMGCM','QMGTR','QMGIN','QMGAS', &
       'QJFTR', &
       'QDSRS','QDSCM','QDSTR','QDSIN','QDSRF','QDSEL','QDSAS', &
       'QKSRS','QKSCM','QKSIN','QKSAS', &
       'QLGRS','QLGCM','QLGTR','QLGIN','QLGRF','QLGAS', &
       'QRLCM','QRLTR','QRLIN','QRLRF','QRLEL','QRLAS', &
       'QRHTR','QRHEL','QRHAS', &
       'QRSCM','QRSTR','QRSIN','QRSRF','QRSEL','QRSAS', &
       'QPFIN','QSGIN','QSGRF', &
       'QPCIN','QPCRF','QPCEL','QPCAS', &
       'QASIN', &
       'QOTTR','QOTIN','QOTRF','QOTAS', &
       'QTPRS','QTPCM','QTPTR','QTPIN','QTPRF','QTPEL','QTPAS', &
       'QMETR','QETTR','QETHM','QHYTR','QUREL','QURHM', &
       'QHOIN','QHOEL','QHOAS', &
       'QGERS','QGEIN','QGEEL','QGEAS', &
       'QBMRS','QBMCM','QBMIN','QBMRF','QBMEL','QBMSN','QBMHM','QBMAS', &
       'QMSIN','QMSEL','QMSAS', &
       'QSTRS','QSTCM','QSTIN','QSTEL','QSTAS', &
       'QPVRS','QPVCM','QPVIN','QPVEL','QPVAS', &
       'QWIIN','QWIEL','QWIAS', &
       'QTRRS','QTRCM','QTRTR','QTRIN','QTREL','QTRSN','QTRHM','QTRAS', &
       'QEIEL','QCIIN', &
       'QTSRS','QTSCM','QTSTR','QTSIN','QTSRF','QTSEL','QTSSN','QTSHM','QTSAS', &
       'QH1TR','QH2TR','QH3TR'/

! +++ PRICES Variables:
      DATA MPVARSL/ &
       'PELRS','PELCM','PELTR','PELIN','PELAS', &
       'PGFRS','PGFCM','PGFTR','PGFIN','PGFEL','PGFAS', &
       'PGIRS','PGICM','PGITR','PGIIN','PGIEL','PGIAS', &
       'PNGRS','PNGCM','PNGTR','PNGIN','PNGEL','PNGAS', &
       'PGPTR','PLPIN', &
       'PCLRS','PCLCM','PCLIN','PCLEL','PCLSN','PCLAS', &
       'PMCIN', &
       'PMGCM','PMGTR','PMGIN','PMGAS', &
       'PJFTR', &
       'PDSRS','PDSCM','PDSTR','PDSIN','PDSEL','PDSAS', &
       'PKSRS','PKSCM','PKSIN','PKSAS', &
       'PLGRS','PLGCM','PLGTR','PLGIN','PLGAS', &
       'PRLCM','PRLTR','PRLIN','PRLEL','PRLAS', &
       'PRHTR','PRHEL','PRHAS', &
       'PRSCM','PRSTR','PRSIN','PRSEL','PRSAS', &
       'PPFIN','PASIN', &
       'POTTR','POTIN','POTAS', &
       'PTPRS','PTPCM','PTPTR','PTPIN','PTPRF','PTPEL','PTPAS', &
       'PMETR','PETTR','PHYTR','PUREL', &
       'PH1TR','PH2TR','PH3TR'/

! +++ QUANTITY variables for natural gas and coal from utilities:
      DATA MUQVARSL/'QGFELGR','QGIELGR','QGCELGR'/
      character*9 SMUQVARSL(4)/'SQGFELGR1','SQGFELGR2','SQGIELGR1','SQGIELGR2'/
      CHARACTER*9 SGASQVARSL(3)/'SQNGELGR1','SQNGELGR2','SQNGELGR3'/

!  New coal
      DATA MCQVARSLS/ 'QCLB1NR', 'QCLB2NR', 'QCLB3NR', 'QCLB4NR', 'QCLB5NR', 'QCLB6NR', 'QCLB7NR', 'QCLB8NR', &
           'QCLC1NR', 'QCLC2NR', 'QCLC3NR', 'QCLC4NR', 'QCLC5NR', 'QCLC6NR', 'QCLC7NR', 'QCLC8NR', 'QCLC9NR', &
           'QCLCXNR', 'QCLCYNR', 'QCLCZNR', 'QCLH1NR', 'QCLH2NR', 'QCLH3NR', 'QCLH4NR', 'QCLH5NR', 'QCLH6NR', &
           'QCLH7NR', 'QCLH8NR', 'QCLH9NR', 'QCLHANR', 'QCLHBNR', 'QCLHCNR',                                  &
           'QCLPCNR', 'QCLOCNR', 'QCLIGNR', 'QCLI2NR', 'QCLPQNR', 'QCLISNR'/

! +++ PRICE variables for natural gas and coal to utilities:
      DATA MUPVARSL/'PGFELGR','PGIELGR','PGCELGR'/
      CHARACTER*9 SMUPVARSL(4)/'SPGFELGR1','SPGFELGR2','SPGIELGR1','SPGIELGR2'/
      CHARACTER*9 SGASPVARSL(3)/'SPNGELGR1','SPNGELGR2','SPNGELGR3'/

      CHARACTER*9 MCPVARSLS(2) / 'PCLELCDR1' , 'PCLELCDR2' /

! +++ The variables SUBR_NAMES, SUBR_DESCR, FORE_SITE_CNTL, and FORE_SITE_TYPE are declared in NCNTRL
      CHARACTER*6 SUBR_NAMESL(NMODEL),SUBR_DESCRL(NMODEL)*18

      DATA SUBR_NAMESL/'WORLD ','MAC   ','RESD  ','COMM  ','IND   ','TRAN  ', &
              'UTIL  ','COAL  ','WELL  ','NGMM  ','REFINE','RENEW ','HYDRGN'/

      DATA SUBR_DESCRL/'International', &
       'Macroeconomic', &
       'Residential', &
       'Commercial', &
       'Industrial', &
       'Transportation', &
       'Electric Utility', &
       'Coal Supply', &
       'Oil and Gas Supply', &
       'Natural Gas Market', &
       'Petroleum Refinery', &
       'Renewables', &
       'Hydrogen'/

      CHARACTER FORE_SITE_CNTLL(2)*10
      CHARACTER FORE_SITE_TYPEL(3)*8
      CHARACTER LOOPL(2)*19

      DATA FORE_SITE_CNTLL/'Central','Submodules'/
      DATA FORE_SITE_TYPEL/'Myopic','Adaptive','Perfect'/
      DATA LOOPL/'One-Year-At-A-Time','All-Years-At-A-Time'/
      INTEGER I,J

      DO I=1,MNUMP
         MPVARS(I)=MPVARSL(I)
      ENDDO

      DO I=1,MNUMQ
         MQVARS(I)=MQVARSL(I)
      ENDDO

      DO I=1,3
         MUQVARS(I)=MUQVARSL(I)
         MUPVARS(I)=MUPVARSL(I)
      ENDDO
      SMUQVARS(:)=SMUQVARSL(:)
      SMUPVARS(:)=SMUPVARSL(:)
      SGASQVARS=SGASQVARSL
      SGASPVARS=SGASPVARSL

!  new coal
      DO I=1,NCLUT1
         MCQVARSS(I)=MCQVARSLS(I)
      ENDDO
      MCPVARSS(1)=MCPVARSLS(1)
      MCPVARSS(2)=MCPVARSLS(2)
      DO I=1,MNOTH
         MOVARS(I)=MOVARSL(I)
      ENDDO

      DO I=1,3
         MZVARS(I)=MZVARSL(I)
      ENDDO

      DO J=1,NMODEL
         SUBR_NAMES(J)=SUBR_NAMESL(J)
         SUBR_DESCR(J)=SUBR_DESCRL(J)
      ENDDO

      DO I=1,2
         FORE_SITE_CNTL(I)=FORE_SITE_CNTLL(I)
         LOOP(I)=LOOPL(I)
      ENDDO

      DO I=1,3
         FORE_SITE_TYPE(I)=FORE_SITE_TYPEL(I)
      ENDDO

      RETURN
      END
!******************************************************************
!*    Subroutine MPTIM2(ICSEC)
!*
!*    Returns CPU seconds, in hundredths, passed since start of job.
!*    Calls external routines cpu_time, system_clock
!******************************************************************
      SUBROUTINE MPTIM2(ICSEC)
      use dfport
      IMPLICIT NONE

      integer*4 icsec               ! cpu seconds (hundredths) since start of program
      common/mptims/istart_sec,curcpu,wstart_sec,cursec,crate4,cmax
      integer*4     istart_sec,curcpu,wstart_sec,cursec,crate4,cmax
      real*4 rsecs                  ! cpu_time takes a real argument
      integer*4 iwsec
      real*4 elapsed

      call cpu_time(rsecs)        ! processor dependent: seconds on cpu clock
      icsec=int(rsecs*100.)       ! convert to hundredths of seconds
      icsec=icsec-istart_sec      ! calculate time since the start of program
      curcpu=icsec                ! also save cpu time in common, curcpu

      call system_clock(iwsec)
      if (iwsec .ge. wstart_sec) then
         elapsed=float(iwsec-wstart_sec)/float(crate4)
      else
         write (6,'(a,I8,I16,I24,I8)') " Negative time prevention: ",iwsec,wstart_sec,cmax,crate4
         elapsed=float(iwsec-wstart_sec+cmax)/float(crate4)
         call system_clock(count=wstart_sec,count_rate=crate4,count_max=cmax)   ! reinitialize
      endif
      cursec=int(elapsed*100.)    ! hundredths of seconds

      RETURN
      END
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
!******************************************************************
!*    Subroutine MPTIMINIT
!*
!*    Initializes wall clock time keeping (system_clock calls)
!******************************************************************
      SUBROUTINE MPTIMINIT
      use dfport
      IMPLICIT NONE

      common/mptims/istart_sec,curcpu,wstart_sec,cursec,crate4,cmax
      integer*4     istart_sec,curcpu,wstart_sec,cursec,crate4,cmax
      real*4 rsecs                  ! cpu_time takes a real argument

      call cpu_time(rsecs)          ! processor dependent: seconds on cpu clock
      istart_sec=int(rsecs*100.)    ! store first result as start of program in hundredths of a second
      call system_clock(count=wstart_sec,count_rate=crate4,count_max=cmax)

      RETURN
      END
!******************************************************************
!     ROUTINE TO STORE AN ITERATION'S WORTH OF CONVERGENCE VARIABLE
!     VALUES TO DIRECT ACCESS DEBUG FILE.  FILE IS LATER USED BY
!     ROUTINE MAINDBGRPT.  THIS ROUTINE CALLS WRMNPQIT TO DO WORK
!******************************************************************
      SUBROUTINE STMNPQIT
      IMPLICIT NONE
      include 'parametr'
      include 'emmparm'
      include 'cdsparms'
      include 'ncntrl'
      include 'qblk'
      include 'ampblk'
      include 'uefdout'
      include 'angtdm'
      include 'coalemm'
      include 'acoalprc'
      include 'pqchar'
      include 'uso2grp'
      include 'maindbg'  ! <-- working with these variables primarily

      INTEGER IR        ! Subscript index for REGION
      INTEGER IV        ! Subscript index for VARIABLE
      INTEGER IY        ! Subscript index for YEAR (IY=CURIYR)
      INTEGER IREC      ! HOLDS RECORD NUMBER FOR D/A FILE
      INTEGER FLAGWR    ! FLAG TO TELL SUBROUTINE TO WRITE BLOCK ON LAST CALL
      INTEGER NUMQAS,NUMQ_AS,NUMPAS,NUMP_AS,ILAST
      EXTERNAL NUMQ_AS,NUMP_AS
! SET POINTERS TO FIRST RECORD FOR EACH YEAR GROUP
      IY=CURIYR
      DTCURBLK=0  ! INIT THE # OF THE CURRENT BLOCK OF RECORDS IN MEMORY
      IF(CURITR.EQ.1.AND.CURIYR.EQ.FIRSYR) THEN
        DTSTART(CURIYR)=1
      ELSEIF(CURITR.EQ.1.AND.CURIYR.GT.FIRSYR) THEN
        DTSTART(CURIYR)=DTSTART(CURIYR-1)+DTNVAR
      ENDIF
      DTNVAR=0  ! INITIALIZE COUNTER FOR NUMBER OF CONV VARIABLES OR RECORDS
      FLAGWR=0  ! TURNOFF FLAG THAT TELLS WRMNPQIT TO WRITE A BLOCK
! PROCESS PRICE VARIABLES, THEN QTY VARIABLES, THEN OTHER VARIABLES, LOOPING THROUGH REGIONS
      DO IV = 1,MNUMP
! SEE IF THE VARIABLE IS A TOTAL VARIABLE OR NOT--WE SKIP TOTAL VARIABLES
        NUMPAS=NUMP_AS(IV,MNUMP)
        IF (NUMPAS.EQ.0) THEN
          DO IR=1,MNUMCR-2
            CALL WRMNPQIT(MPVARS(IV),MPRC(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
        ENDIF
      ENDDO
! GO THROUGH LIST OF QUANTITIES--DO SAME THING
      DO IV = 1,MNUMQ
        NUMQAS=NUMQ_AS(IV,MNUMQ)
        IF (NUMQAS.EQ.0) THEN
          DO IR=1,MNUMCR-2
            CALL WRMNPQIT(MQVARS(IV),MQTY(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
        ENDIF
      ENDDO

! GO THROUGH LIST OF PRICES OF NATURAL GAS, COAL TO UTILITIES
      DO IV = 1,3
          DO IR=1,NNGEM
            CALL WRMNPQIT(MUPVARS(IV),MUPRC(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
      DO IV = 1,3
          DO IR=1,NNGEM
            CALL WRMNPQIT(SGASPVARS(IV),SGASPRICE(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
!  Coal prices
      DO IV = 1,2
          DO IR=1,NDRGG
            CALL WRMNPQIT(MCPVARSS(IV),PCLELCDR(IV,IR,IY),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
! GO THROUGH LIST OF QUANTITIES OF NATURAL GAS, COAL FROM UTILITIES
      DO IV = 1,3
          DO IR=1,NNGEM
            CALL WRMNPQIT(MUQVARS(IV),MUQTY(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
      DO IV = 1,3
          DO IR=1,NNGEM
            CALL WRMNPQIT(SGASQVARS(IV),SGASQUANT(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
! Coal quantities
      DO IV = 1,NCLUT1
          DO IR=1,NDRGG
            CALL WRMNPQIT(MCQVARSS(IV),QCLCLNR(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
! GO THROUGH LIST OF OTHER CONVERGENCE VARIABLES AND DO SAME THING ON LAST CALL, SET FLAG
      FLAGWR=0
      IR=11
      DO IV = 1,MNOTH
        IF(IV.EQ.MNOTH) FLAGWR=1
        CALL WRMNPQIT(MOVARS(IV),CONVOTH(2,IV,IY),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
      ENDDO
      IF(DTNVAR.EQ.MXDTNVAR) WRITE(6,*) 'IN STMNPQIT, DTNVAR>MXDTNVAR'
      IF(DTNVAR.EQ.MXDTNVAR) WRITE(6,*) 'INCREASE MXDTNVAR IN /MAINDBG/'
99    RETURN
      END
!******************************************************************
!*    ROUTINE TO STORE/RETRIEVE BLOCKS OF CONVERGENCE VARIABLES
!     ON DIRECT ACCESS DEBUG FILE.  CALL BY STMNPQIT
!******************************************************************
      SUBROUTINE WRMNPQIT(CVAR,QVAR,IR,CURIYR,CURITR,FIRSYR,FLAGWR)
      IMPLICIT NONE
! INPUT ARGUMENTS:
!    CVAR   -- NAME OF VARIABLE TO STORE ON DATA FILE
!    QVAR   -- VALUE OF VARIABLE
!    IR     -- REGION NUMBER (1-11)
!    CURIYR -- NEMS YEAR INDEX
!    CURITR -- NEMS ITERATION COUNT
!    FLAGWR -- INTEGER FLAG (1=YES) TO SAY WHETHER TO WRITE OUT
!              A GROUP OF 100 ITEMS ON THE FINAL CALL
      include 'parametr'
      include 'maindbg'
      CHARACTER*1 AREG(23)/'1','2','3','4','5','6','7','8','9','0', &
       'A','B','C','D','E','F','G','H','I','J','K','L','M'/
      CHARACTER*2 AYEAR(MNUMYR)/ &
       '90','91','92','93','94','95','96','97','98','99', &
       '00','01','02','03','04','05','06','07','08','09', &
       '10','11','12','13','14','15','16','17','18','19', &
       '20','21','22','23','24','25','26','27','28','29', &
       '30','31','32','33','34','35','36','37','38','39', &
       '40','41','42','43','44','45','46','47','48','49', &
       '50'/
      CHARACTER*(*) CVAR
      CHARACTER*12 DTTMP  ! HOLDS KEY READ FROM D/A FILE
      REAL*4 QVAR
      INTEGER IR,CURIYR,CURITR,FIRSYR,IREC,IPOS,IBLK,I,J,FLAGWR
      IF(DTNVAR.LT.MXDTNVAR) DTNVAR=DTNVAR+1
! CONSTRUCT KEY FOR DIRECT ACCESS FILE. KEY CONSISTS OF VAR NAME, REGION, YEAR
      DTNAM=CVAR           ! VAR NAME
      DTAREG=AREG(IR)      ! REGION
      DTYEAR=AYEAR(CURIYR) ! YEAR
      DTTMP=DTNAM//DTAREG//DTYEAR
! ASSIGN RECORD NUMBER FOR EACH KEY--FOR USE AFTER SORTING TO GO TO PARTICULAR RECORD
      DTOFFSET(DTNVAR)=DTNVAR
      DTKEYS(DTNVAR)=DTTMP
! IREC: logical record or item--one for each variable in each region, year
! IBLK: actual file record or block, as records are accessed in blocks of 100
! IPOS: relative position (from 1 to 100) within the block of the logical records
      IREC=DTSTART(CURIYR)+DTOFFSET(DTNVAR)-1
      IBLK=1+(IREC-1)/100
      IPOS=IREC-(IBLK-1)*100
! Read values from previous iterations.  The current record may not be at the
! last written because the reporting step reads records out-of-sequence.  If
! this is the first iteration of a year, make sure that the current record is
! the last record from the previous year.
      IF(DTCURBLK.NE.IBLK) THEN
        IF(CURITR.GT.1.OR. (DTNVAR.EQ.1.AND.IREC.GT.1.AND.IPOS.NE.1)) THEN
! IF BLOCK OF DATA NOT ALREADY IN MEMORY, LOAD IT IN
          READ(IMNPQIT,REC=IBLK,ERR=99) (DTKEY(I),(DTPQIT(J,I),J=1,MNDBGITRS),I=1,100)
          DTCURBLK=IBLK
        ENDIF
      ENDIF
      IF(DTTMP.NE.DTKEY(IPOS).AND.CURITR.GT.1) THEN
        WRITE(6,*) ' MAIN DEBUG KEY MISMATCH, EXPECTED KEY=',DTTMP
        WRITE(6,*) ' BUY DTKEY(IPOS)=',DTKEY(IPOS), &
            ' ON RECORD BLOCK:',IBLK,', POSITION=',IPOS,', ITEM NUMBER=',IREC
      ENDIF
! INITIALIZE ARRAY OF VALUES ACROSS ITERATIONS IF ON FIRST ITERATION
      IF(CURITR.EQ.1) CALL ZEROR(DTPQIT(1,IPOS),MNDBGITRS)
! ASSIGN CURRENT ITERATION'S VALUE
      DTKEY(IPOS)=DTTMP
      DTPQIT(min(mndbgitrs,CURITR),IPOS)=QVAR
! REWRITE BLOCK OF 100 ITEMS IF ON THE 100TH OR FLAG IS SET.
      IF(IPOS.EQ.100.OR.FLAGWR.EQ.1) THEN
         IF(IPOS.LT.100)THEN
           DO I=IPOS+1,100
             DTKEY(I)=' '
             DO J=1,MNDBGITRS
               DTPQIT(J,I)=0.
             ENDDO
           ENDDO
         ENDIF
         WRITE(IMNPQIT,REC=IBLK,ERR=99) (DTKEY(I),(DTPQIT(J,I),J=1,MNDBGITRS),I=1,100)
        DTCURBLK=IBLK
      ENDIF
99    RETURN
      END

!******************************************************************
!*    Report general debugging information for a run.
!******************************************************************
      SUBROUTINE MAINDBGRPT(ITASK)
      IMPLICIT NONE
      include 'parametr'
      include 'emmparm'
      include 'cdsparms'
      include 'ncntrl'
      include 'qblk'
      include 'ampblk'
      include 'coalemm'
      include 'coalprc'
      include 'pqchar'
      include 'maindbg'
      CHARACTER*35 DEFIN
      INTEGER I,J,K,ITASK,IM,YR,IMODEL,IFOUND,NPL,IV,ITR
      INTEGER IREC,IBLK,IPOS
      CHARACTER*3 TXT
      CHARACTER*3 OFFON(0:1)/'Off','On '/
      CHARACTER*1 AREG(23)/'1','2','3','4','5','6','7','8','9','0', &
       'A','B','C','D','E','F','G','H','I','J','K','L','M'/
      CHARACTER*2 AYEAR(MNUMYR)/ &
       '90','91','92','93','94','95','96','97','98','99', &
       '00','01','02','03','04','05','06','07','08','09', &
       '10','11','12','13','14','15','16','17','18','19', &
       '20','21','22','23','24','25','26','27','28','29', &
       '30','31','32','33','34','35','36','37','38','39', &
       '40','41','42','43','44','45','46','47','48','49', &
       '50'/
      CHARACTER*12 KEY

      INTEGER NUMQAS,NUMQ_AS,NUMPAS,NUMP_AS
      EXTERNAL NUMQ_AS,NUMP_AS
      COMMON/MAXITRSR/MAXITRSR
      INTEGER MAXITRSR,NUM_ITR           ! RUNTIME OPTION FOR MAXITR 2001-ON
      NUM_ITR=MAX(MAXITR,MAXITRSR)+1
      YR = CURIYR+ 1989
      IF (ITASK .EQ. 1) THEN

        WRITE(15,*)  '  Convergence Summary for NEMS Integrated ','Run:'
        WRITE(15,*) '  Scenario: ',SCEN,'     Version: ',DATE
        WRITE(15,*)  ' '
        CALL MNINFO
! PRINT CONVERGENCE, RELAXATION PARAMETERS, ALONG WITH DEFINITIONS
        WRITE(15,201)
        DO IV = 1,MNUMP
          NUMPAS=NUMP_AS(IV,MNUMP)
          IF(NUMPAS.EQ.0) THEN
            DEFIN=' '
            CALL MNDEFN('MPBLK',MPVARS(IV),DEFIN)
            WRITE(15,200) MPVARS(IV),(MNPCNV(I,IV),I=1,2),(RLXP(ITR,IV),ITR=1,NUM_ITR),DEFIN
          ENDIF
        ENDDO
        WRITE(15,201)
        DO IV = 1,MNUMQ
          NUMQAS=NUMQ_AS(IV,MNUMQ)
          IF(NUMQAS.EQ.0) THEN
            CALL MNDEFN('QBLK',MQVARS(IV),DEFIN)
            WRITE(15,200) MQVARS(IV),(MNQCNV(I,IV),I=1,2),(RLXQ(ITR,IV),ITR=1,NUM_ITR),DEFIN
          ENDIF
        ENDDO
        WRITE(15,201)
        DO IV = 1,3
            CALL MNDEFN(' ',MUPVARS(IV),DEFIN)
            WRITE(15,200) MUPVARS(IV),(MNUPCNV(I,IV),I=1,2),(RLXUP(ITR,IV),ITR=1,NUM_ITR),DEFIN
        ENDDO
        DO IV = 1,3
            CALL MNDEFN(' ',MUQVARS(IV),DEFIN)
            WRITE(15,200) MUQVARS(IV),(MNUQCNV(I,IV),I=1,2),(RLXUQ(ITR,IV),ITR=1,NUM_ITR),DEFIN
        ENDDO
        DO IV = 1,3
            CALL MNDEFN('NGTDMOUT',SGASPVARS(IV)(1:8),DEFIN)    !  trim last character for lookup
            WRITE(15,200) SGASPVARS(IV),(MNGASPCNV(I,IV),I=1,2),(RLXGASP(ITR,IV),ITR=1,NUM_ITR),DEFIN
        ENDDO
        DO IV = 1,3
            CALL MNDEFN('UEFDOUT',SGASQVARS(IV)(1:8),DEFIN)     !  trim last character for lookup
            WRITE(15,200) SGASQVARS(IV),(MNGASQCNV(I,IV),I=1,2),(RLXGASQ(ITR,IV),ITR=1,NUM_ITR),DEFIN
        ENDDO
! New coal
        DO IV = 1,2
            CALL MNDEFN(' ',MCPVARSS(IV),DEFIN)
            WRITE(15,200) MCPVARSS(IV),(MNCPCNVS(I),I=1,2),(RLXCPS(ITR),ITR=1,NUM_ITR),DEFIN
        ENDDO
        DO IV = 1,NCLUT1
            CALL MNDEFN(' ','QCLCLNR',DEFIN)          !  QCLCLNR is name in dictionary, so look that up
            WRITE(15,200) MCQVARSS(IV),(MNCQCNVS(I,IV),I=1,2),(RLXCQS(ITR,IV),ITR=1,NUM_ITR),DEFIN
        ENDDO

        DO IV = 1,MNOTH
            CALL MNDEFN(' ',MOVARS(IV),DEFIN)
            WRITE(15,200) MOVARS(IV),(MNOCNV(I,IV),I=1,2),(RLXOTH(ITR,IV),ITR=1,NUM_ITR),DEFIN
        ENDDO

200   FORMAT(1X,A,T12,1X,2P,F10.3,0P,F10.3,<NUM_ITR>F12.3,2X,A35)
201   FORMAT(// &
       1X,'   Convergence Variables and Definitions   '// &
       1X,' Variable     Maximum  Absolute  Relaxation'/ &
       1X,'   Name      % Change    Change    Fraction',<(NUM_ITR-1)*12>X,'  Definition'/ &
       1X,'=========    ========  ========  ==========  ',<(NUM_ITR-1)*12>X,5('======='))

        WRITE(15,'(//)')
        WRITE(15,*)  '            Year by Year Summary '

      ELSEIF (ITASK .EQ. 2) THEN
! SORT THE DIRECT ACCESS RECORD KEYS TO SPEED SEARCHES
        IF(DBDUMP.EQ.1) CALL MNSORT(DTKEYS,DTOFFSET,DTNVAR,MXDTNVAR)
        TXT = '   '
        IF (CTEST.EQ.0.AND.CURITR.GE.MAXITR+1) TXT = 'Not'
        WRITE(15,*) ' '
        WRITE(15,*) '  Year ',YR,'     ',TXT,' Converged ', &
                    '  Iterations ',CURITR - 1
        IF (CTEST.EQ.0.AND.CURITR.GE.MAXITR+1) THEN
          WRITE(15,*) 'Modules that did not converge: '
          DO IMODEL=1,NMODEL
            IF (CNVTST(IMODEL,CURIYR) .EQ. 0) WRITE(15,*) ' ', &
            SUBR_NAMES(IMODEL),'  ',SUBR_DESCR(IMODEL)
          ENDDO
          IF (DTBAC(1,3) .NE. 0.) THEN
            IF(DBDUMP.NE.1) THEN
              WRITE(15,*) '             Variables Not Converged'
              WRITE(15,*) ' Variable  Region  Change    Final  Previous'
              WRITE(15,*) '                    (%)      Value    Value '
            ELSE
              NPL=MIN((CURITR-1),MNDBGITRS,10)
              WRITE(15,*) '             Variables Not Converged--by Iteration'
              WRITE(15,102) ' Variable  Region  Change',('    Iter. ',J=1,NPL)
102           FORMAT(1X,11A)
              WRITE(15,103) '                    (%)  ',(J,J=NPL,1,-1)
103           FORMAT(1X,A,I7,9I10)
            ENDIF
          ENDIF
          DO I=1,MNDBGVARS
            IF (DTBAC(I,3) .NE. 0. .AND. DTREG(I) .LE. 23) THEN
              IFOUND=0
              KEY=DTVAR(I)//AREG(DTREG(I))//AYEAR(CURIYR)
              IF(DBDUMP.EQ.1) CALL MNSRCH(DTKEYS,DTNVAR,KEY,IFOUND)
              IF(DBDUMP.EQ.1.AND.IFOUND.EQ.0) THEN
                 WRITE(15,*) 'KEY NOT FOUND: ',KEY
              ENDIF
              IF(DBDUMP.EQ.1.AND.IFOUND.NE.0) THEN
                IREC=DTSTART(CURIYR)+DTOFFSET(IFOUND)-1
                IBLK=1+(IREC-1)/100
                IPOS=IREC-(IBLK-1)*100
                IF(DTCURBLK.NE.IBLK) THEN
!    IF BLOCK OF DATA NOT ALREADY IN MEMORY, LOAD IT IN
                  READ(IMNPQIT,REC=IBLK,ERR=99) (DTKEY(K),(DTPQIT(J,K),J=1,MNDBGITRS),K=1,100)
                  DTCURBLK=IBLK
                ENDIF
99              WRITE(15,100) DTVAR(I),DTREG(I),DTBAC(I,3)*100,(DTPQIT(J,IPOS),J=NPL,1,-1)
100             FORMAT(3X,A,I5,F8.2,10F10.2)
              ELSE
                WRITE(15,101) DTVAR(I),DTREG(I),DTBAC(I,3)*100,DTBAC(I,2),DTBAC(I,1)
101             FORMAT(3X,A,I5,F8.2,10F10.2)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ELSEIF (ITASK .EQ. 3) THEN
        CALL NDEBUG(0,15)
      ENDIF
      RETURN
      END
!******************************************************************
      SUBROUTINE MNINFO
      IMPLICIT NONE
! ROUTINE TO DISPLAY INFORMATION ON THE MAIN CONVERGENCE DEBUG REPORT
      CHARACTER*63 MESSAGE(24)
      INTEGER I
      DATA (MESSAGE(I),I=1,10)/ &
      '***************************************************************', &
      'This report lists variables not meeting the convergence        ', &
      'criteria on the final iteration.  The percentage changes       ', &
      'reported are now calculated as the absolute change relative to ', &
      'the average of the current and previous values.  That is:      ', &
      '      % Change = 100 * ( ABS(CUR-PREV) / ((CUR+PREV)/2) )      ', &
      '                                                               ', &
      'Up to 25 variables failing to converge are reported.  More     ', &
      'detailed diagnostics are printed in the execution log (unit 6).', &
      '                                                               '/
      DATA (MESSAGE(I),I=11,24)/ &
      'If runtime option DBDUMP=1 is specified, values by iteration   ', &
      'are reported.  If so, the values are those after all models    ', &
      'have executed and AFTER any relaxation has been applied.  If   ', &
      'a variable is reset by the relaxation routine, the percentage  ', &
      'changes reported will be inconsistent with the values reported.', &
      '                                                               ', &
      'By default, only the values from the final iteration and       ', &
      'and the prior iteration are reported.  If so, the "Final"      ', &
      'values are those BEFORE execution of the relaxation routine.   ', &
      '                                                               ', &
      'The following convergence criteria and relaxation assumptions  ', &
      'are obtained from the MAIN.MNCNVRG input file.  The variable   ', &
      'name definitions are found in the NEMS.FDICT input file.       ', &
      '***************************************************************'/
      do i=1,24
        write(15,'(10X,A)') message(i)
      enddo
      RETURN
      END
!******************************************************************
      SUBROUTINE MNDEFN(BLKNAM,VARNAM,DEFIN)
      IMPLICIT NONE
! SEARCHES FILER DICTIONARY TO GET DEFINITION FOR A VARIABLE.
! IF COMMON BLOCK NAME SUPPLIED, THE SEARCH GOES FASTER.  IF
! NOT, THE WHOLE DICTIONARY (~1500 VARIABLES) IS SEARCHED.

! INPUT ARGUMENTS:
!     BLKNAM  COMMON BLOCK NAME
!     VARNAM  VARIABLE NAME
! OUTPUT ARGUMENT:
!     DEFIN   DEFINITION OF VARIABLE

      include 'fdict'
      CHARACTER*(*) BLKNAM,VARNAM,DEFIN
      CHARACTER*(FMAXCHARVAR) VAR
      CHARACTER*(FMAXCHARCOM) BLK
      INTEGER IBLOCK,IVAR,I,ISTART,IEND

      VAR=' '
      BLK=' '
      DEFIN=' '
      VAR=VARNAM
      BLK=BLKNAM
      IBLOCK=0
      IVAR=0
      IF(BLK.NE.' ') THEN
        DO I=1,FNBLOC
          IF(BLK.EQ.FBLOCK(I)) THEN
            IBLOCK=I
            GOTO 10
          ENDIF
        ENDDO
10      CONTINUE
      ENDIF
      ISTART=1
      IEND=FNDICT                     ! TOTAL ENTRIES IN DICTIONARY
      IF(IBLOCK.GT.0) THEN
        ISTART=FINDEX(IBLOCK)         ! START OF ENTRIES FOR COMMON # IBLOCK
        IEND=ISTART+FNVARS(IBLOCK)-1  ! END OF ENTRIES GIVEN # VARS IN COMMON
      ENDIF
      DO I=ISTART,IEND
        IF(VAR.EQ.FVARNM(I)) THEN     ! SEARCH FOR VARIABLE NAME
          IVAR=I
          DEFIN=FDESCR(IVAR)          ! GET DICTIONARY DEFINITION
          RETURN
        ENDIF
      ENDDO
      If(VAR(1:7).eq.'PLNGIMP') then
        DEFIN='LNG Import Price by LNG region'
      ELSE If(VAR(1:5).eq.'PBMET') then
        DEFIN='Price of Biomass for Ethanol'
      ELSE If(VAR(1:5).eq.'QBMET') then
        DEFIN='Quantity of Biomass for Ethanol'
      ELSE If(VAR(1:5).eq.'PSO2_') then
        DEFIN='ECP Sulfur dioxide allowance price'
      ELSE If(VAR(1:6).eq.'Unused') then
        DEFIN='Unused'
      ENDIF
      RETURN
      END
!******************************************************************
!*    FILL ARRAYS CONTAINING debugging information for a run.  THE
!*    ROUTINES MAINTAINS A LIST, SORTED BY PERCENTAGE CHANGE, OF THE
!*    THE TOP n VARIABLES FAILING TO CONVERGE.
!******************************************************************
      SUBROUTINE SORTRPT(FRCCHG,IR,PREVV,MRC,MVARS)
      IMPLICIT NONE
      include 'parametr'
      include 'maindbg'

      REAL PREVV,MRC,FRCCHG
      INTEGER IR
      CHARACTER*(*) MVARS

      INTEGER I,IDX,TMP1,TMP2
      I = 1
      IDX = 0
      DO WHILE ((I .LE. MNDBGVARS) .AND. (IDX .EQ. 0))
        IF (FRCCHG .GT. DTBAC(I,3)) IDX = I
        I = I + 1
      ENDDO
      IF (IDX .GT. 0) THEN
        DO I = (MNDBGVARS-1), IDX , -1
          DTBAC(I + 1,1) = DTBAC(I,1)
          DTBAC(I + 1,2) = DTBAC(I,2)
          DTBAC(I + 1,3) = DTBAC(I,3)
          DTREG(I + 1) = DTREG(I)
          DTVAR(I + 1) = DTVAR(I)
        ENDDO
        DTBAC(IDX,1) = PREVV
        DTBAC(IDX,2) = MRC
        DTBAC(IDX,3) = FRCCHG
        DTREG(IDX)   = IR
        DTVAR(IDX)   = MVARS
      ENDIF
      RETURN
      END
!*******************************************************************
      SUBROUTINE MNSRCH(A,N,STRING,IFOUND)
      IMPLICIT NONE
!  SEARCHS FOR "STRING" IN SORTED ARRAY A.  WHEN FOUND, RETURNS
!  ARRAY INDEX IN IFOUND
      INTEGER N,IFOUND,ILOW,IHIGH,ITRY
      CHARACTER*12 A(N),STRING*12
      IFOUND=0
      ILOW=1
      IHIGH=N
      DO WHILE(IFOUND.EQ.0.AND.IHIGH.GE.ILOW)
         ITRY=(ILOW+IHIGH)/2
         IF(STRING.EQ.A(ITRY)) THEN
           IFOUND=ITRY
         ELSEIF(STRING.GT.A(ITRY)) THEN
           ILOW=ITRY+1
         ELSEIF(STRING.LT.A(ITRY)) THEN
           IHIGH=ITRY-1
         ENDIF
      ENDDO
      RETURN
      END
!*******************************************************************
      SUBROUTINE MNSORT(A,IDX,N,NMAX)
      IMPLICIT NONE
! SORTS FIRST N ELEMENTS OF CHARACTER ARRAY A.  IT ALSO SORTS AN INDEXING
! ARRAY, IDX, WHICH CAN BE USED TO MAP THE SORT ORDER TO OTHER ARRAYS.  IDX
! SHOULD BE PASSED WITH THE SERIES 1, 2, ... N.
!  THE SORT METHODOLOGY IS CALLED QUICKSORT IS CONSIDERED AN EFFICIENT
!  APPROACH FOR SORTING LARGE ARRAYS

      INTEGER NMAX,N
      CHARACTER*12 A(NMAX),X,W
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
!===============================================================
      SUBROUTINE MNPRCHK
      IMPLICIT NONE
! ROUTINE TO CHECK SETTINGS ON INITIAL PRICES OR QUANTITIES.  CURRENTLY
! USED TO CHECK THAT PRHEL<PRLEL
      include 'parametr'
      include 'emmparm'
      include 'cdsparms'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'coalemm'
      include 'coalprc'
      include 'pqchar'
      INTEGER ICHANGE            ! FLAG TO INDICATE WHETHER CHANGE MADE
      INTEGER IR                 ! Subscript Index for REGION
      INTEGER IY                 ! Subscript Index for YEAR
      IY=CURIYR
      ICHANGE=0
      DO IR=1,MNUMCR-1
        IF(PRLEL(IR,IY).LT.PRHEL(IR,IY)) THEN
          IF(ICHANGE.EQ.0) &
          WRITE(6,*) ' RESETTING INITIAL LOW SULFUR RESID ', &
                      'PRICE TO 1.1*HIGH SULFUR PRICE'
          ICHANGE=1
          PRLEL(IR,IY)=1.1*PRHEL(IR,IY)
        ENDIF
! MAKE SURE INITIAL AVERAGE RESID PRICE IS CORRECT
        QRSEL(IR,IY)=QRLEL(IR,IY)+QRHEL(IR,IY)
        IF(QRSEL(IR,IY).GT.0.) THEN
          PRSEL(IR,IY)=PRLEL(IR,IY)*.92+PRHEL(IR,IY)*.08   !Calculate Average resid price based on 95-03 average split

        ENDIF
      ENDDO
      CALL SUMQAS
      CALL AVEPAS

! +++ CALL COPY_ADJUSTENT EVEN IF EPM IS OFF
      CALL COPY_ADJUSTED
! +++ COMPUTE ALL-SECTOR AVERAGES FOR NEMS ADJUSTED PRICE VARIABLES FOR EPM
      CALL AVEPASA
      RETURN
      END
!
      SUBROUTINE RESETOSW(OPT_NAME,NEW_VALUE)
!This resets the value for a run-time option integer switch from the value read from the "moreopt" file
      IMPLICIT NONE
      INTEGER I,NEW_VALUE
      INTEGER MAXRTOPTS
      PARAMETER (MAXRTOPTS=200)
      CHARACTER*8 RTOPTS(MAXRTOPTS),OPT_NAME,RESTOFLINE(MAXRTOPTS)*80
      INTEGER RTOPTSV(MAXRTOPTS),NUMRTOPTS
      COMMON /RTOOPTIONS/ RTOPTS,RTOPTSV,RESTOFLINE,NUMRTOPTS

      DO I=1,NUMRTOPTS
         IF (RTOPTS(I) .EQ. OPT_NAME) THEN
             RTOPTSV(I) = NEW_VALUE
             WRITE(6,'(" Setting option ",A8," to ",I2)') RTOPTS(I),RTOPTSV(I)
             exit
         ENDIF
      ENDDO

      END
      FUNCTION RTOVALUE(RTONAME,RTODEFVAL)

! ---------------------------------------------------------------*

!   THIS FUNCTION SEARCHES THE MOREOPT FILE FOR THE
!   RUN TIME OPTION (RTO) NAME SET IN THE FIRST FUNCTION INVOCATION,
!   AND RETURNS THE VALUE SET FOR THE RUN.
!   Used for obtaining integer values from options in the scedes file via the
!   moreopt options file.
! IF RTONAME NOT FOUND, THE
!   FUNCTION RETURNS the default value.

! ---------------------------------------------------------------*

      IMPLICIT NONE
      INTEGER UNIT,VVALUE,RTOVALUE,FILE_MGR,RTODEFVAL
      CHARACTER FILENM*18,RTONAME*8
      INTEGER MAXRTOPTS,NUMRTOPTS,I
      PARAMETER (MAXRTOPTS=200)
      CHARACTER*8 RTOPTS(MAXRTOPTS),RESTOFLINE(MAXRTOPTS)*80
      INTEGER RTOPTSV(MAXRTOPTS),OPT_USED(MAXRTOPTS),IFOUND
      LOGICAL READYET/.FALSE./
      LOGICAL NEW,FINDRTO
      EXTERNAL FILE_MGR
      COMMON /RTOOPTIONS/ RTOPTS,RTOPTSV,RESTOFLINE,NUMRTOPTS
      FINDRTO=.FALSE.
      IF (.NOT. READYET) THEN
         READYET=.TRUE.
         NEW=.FALSE.
         FILENM = 'MOREOPT'
         UNIT = FILE_MGR('O',FILENM,NEW)
         NUMRTOPTS=1
         WRITE(6,*) '##  Moreopt Runtime Options File'
         WRITE(6,*) '##  Name    Value and  Description or String Value'
         WRITE(6,*) '##========  ============================================================'
10       CONTINUE
         READ(UNIT,900,END=100)RTOPTS(NUMRTOPTS),RTOPTSV(NUMRTOPTS),RESTOFLINE(NUMRTOPTS)
900      FORMAT(A8,1X,I4,1X,A)
         WRITE(6,'(1X,A2,A8,I8,1x,A)')'##',RTOPTS(NUMRTOPTS),RTOPTSV(NUMRTOPTS),TRIM(RESTOFLINE(NUMRTOPTS))
         NUMRTOPTS=NUMRTOPTS+1
         OPT_USED(NUMRTOPTS)=0
         IF(NUMRTOPTS.LE.MAXRTOPTS) GOTO 10
100      NUMRTOPTS=NUMRTOPTS-1
         UNIT = FILE_MGR('C',FILENM,NEW)
      ENDIF
      FINDRTO=.FALSE.
      IFOUND=0
      DO I=1,NUMRTOPTS
         IF (RTONAME .EQ. RTOPTS(I)) THEN
             RTOVALUE = RTOPTSV(I)
             FINDRTO = .TRUE.
             OPT_USED(I)=OPT_USED(I)+1
             IFOUND=I
             GOTO 101
         ENDIF
      ENDDO
101   CONTINUE
      IF(FINDRTO) THEN
         IF(OPT_USED(IFOUND).EQ.1) &
         WRITE(6,*) '##RUN TIME OPTION ',RTONAME,' SET TO ',RTOPTSV(IFOUND)
      ELSE
         WRITE(6,*) '##RUN TIME OPTION ',RTONAME,' NOT FOUND IN LIST'
         RTOVALUE = RTODEFVAL
      ENDIF
      RETURN
      END
!
      SUBROUTINE RTOSTRING(RTONAME,RTOVALUE)

! ---------------------------------------------------------------*

!   THIS SUBROUTINE SEARCHES THE MOREOPT FILE FOR THE RUN TIME OPTION (RTO) NAME SENT
!   AS THE FIRST ARGUMENT RETURNS THE VALUE, WHICH IS TECHNICALLY IN THE DESCRIPTION FIELD
!   Used for obtaining string values from the scedes file.
! ---------------------------------------------------------------*

      IMPLICIT NONE
      CHARACTER*(*) RTOVALUE
      CHARACTER RTONAME*8
      INTEGER MAXRTOPTS,NUMRTOPTS,I
      PARAMETER (MAXRTOPTS=200)
      CHARACTER*8 RTOPTS(MAXRTOPTS),RESTOFLINE(MAXRTOPTS)*80
      INTEGER RTOPTSV(MAXRTOPTS),OPT_USED(MAXRTOPTS),IFOUND

      LOGICAL NEW,FINDRTO

      COMMON /RTOOPTIONS/ RTOPTS,RTOPTSV,RESTOFLINE,NUMRTOPTS
      FINDRTO=.FALSE.
      IFOUND=0


      RTOVALUE=' '
      DO I=1,NUMRTOPTS
         IF (RTONAME .EQ. RTOPTS(I)) THEN
             RTOVALUE = RESTOFLINE(I)
             FINDRTO = .TRUE.
             OPT_USED(I)=OPT_USED(I)+1
             IFOUND=I
             EXIT
         ENDIF
      ENDDO
      IF(FINDRTO) THEN
         IF(OPT_USED(IFOUND).EQ.1) &
         WRITE(6,'(4a)') '##RUN TIME OPTION ',RTONAME,' SET TO ',trim(RTOVALUE)
      ELSE
         WRITE(6,'(4a)') '##RUN TIME OPTION ',RTONAME,' NOT FOUND IN LIST'

      ENDIF
      RETURN
      END
!




!  function to return a million Btu/barrel conversion factor when sent an API gravity, any API gravity.
!  note that the API_OUT formulas can be simplified, but here parallel the BTU_OUT formulas

      REAL FUNCTION API_TO_BTU(API_IN,API_CALC)
      IMPLICIT NONE

      include 'apiblk'

      REAL API_IN, API_OUT, BTU_OUT, BTU_IN

      INTEGER I, API_CALC

      IF (API_CALC .EQ. 21) THEN
         BTU_IN = API_IN / 42 * 1000000.
         IF (BTU_IN .GT. BTU_PER_GAL(1)) THEN
             BTU_OUT = (BTU_IN-BTU_PER_GAL(1))/(BTU_PER_GAL(1)-BTU_PER_GAL(2))*(BTU_PER_GAL(1)-BTU_PER_GAL(2))+BTU_PER_GAL(1)
             API_OUT = (BTU_IN-BTU_PER_GAL(1))/(BTU_PER_GAL(1)-BTU_PER_GAL(2))*(API_GRAV(1)-API_GRAV(2))+API_GRAV(1)
             API_TO_BTU = API_OUT
             RETURN
         ELSE IF (BTU_IN .LT. BTU_PER_GAL(API_COUNT)) THEN
             BTU_OUT = (BTU_IN-BTU_PER_GAL(API_COUNT))/(BTU_PER_GAL(API_COUNT-1)-BTU_PER_GAL(API_COUNT))*(BTU_PER_GAL(API_COUNT-1)-BTU_PER_GAL(API_COUNT))+BTU_PER_GAL(API_COUNT)
             API_OUT = (BTU_IN-BTU_PER_GAL(API_COUNT))/(BTU_PER_GAL(API_COUNT-1)-BTU_PER_GAL(API_COUNT))*(API_GRAV(API_COUNT-1)-API_GRAV(API_COUNT))+API_GRAV(API_COUNT)
             API_TO_BTU = API_OUT
             RETURN
         ENDIF
         DO I=1,API_COUNT
            IF (BTU_IN .LE. BTU_PER_GAL(I) .AND. BTU_IN .GT. BTU_PER_GAL(I+1)) THEN
                BTU_OUT = (BTU_IN-BTU_PER_GAL(I+1))/(BTU_PER_GAL(I)-BTU_PER_GAL(I+1))*(BTU_PER_GAL(I)-BTU_PER_GAL(I+1))+BTU_PER_GAL(I+1)
                API_OUT = (BTU_IN-BTU_PER_GAL(I+1))/(BTU_PER_GAL(I)-BTU_PER_GAL(I+1))*(API_GRAV(I)-API_GRAV(I+1))+API_GRAV(I+1)
                API_TO_BTU = API_OUT
                RETURN
            ENDIF
         ENDDO
      ENDIF
      IF (API_CALC .EQ. 12) THEN
         IF (API_IN .LT. API_GRAV(1)) THEN
!  off table, extrapolate off low end
             API_OUT = (API_GRAV(1)-API_IN)/(API_GRAV(2)-API_GRAV(1))*  &
                       (API_GRAV(1)-API_GRAV(2))+API_GRAV(1)
             BTU_OUT = (API_GRAV(1)-API_IN)/(API_GRAV(2)-API_GRAV(1))*  &
                       (BTU_PER_GAL(1)-BTU_PER_GAL(2))+BTU_PER_GAL(1)
             API_TO_BTU = BTU_OUT * 42 / 1000000
             RETURN
         ELSE IF (API_IN .GT. API_GRAV(API_COUNT)) THEN
!  off table, extrapolate off high end
             API_OUT = (API_IN-API_GRAV(API_COUNT))/(API_GRAV(API_COUNT)-API_GRAV(API_COUNT-1))*  &
                       (API_GRAV(API_COUNT)-API_GRAV(API_COUNT-1))+API_GRAV(API_COUNT)
             BTU_OUT = (API_IN-API_GRAV(API_COUNT))/(API_GRAV(API_COUNT)-API_GRAV(API_COUNT-1))*  &
                        (BTU_PER_GAL(API_COUNT)-BTU_PER_GAL(API_COUNT-1))+BTU_PER_GAL(API_COUNT)
             API_TO_BTU = BTU_OUT * 42 / 1000000
             RETURN
         ENDIF
         DO I=1,API_COUNT
            IF (API_IN .GE. API_GRAV(I) .AND. API_IN .LT. API_GRAV(I+1)) THEN
!  within bounds of table, find place and interpolate
                API_OUT = (API_GRAV(I+1)-API_IN)/(API_GRAV(I+1)-API_GRAV(I))*  &
                          (API_GRAV(I)-API_GRAV(I+1))+API_GRAV(I+1)
                BTU_OUT = (API_GRAV(I+1)-API_IN)/(API_GRAV(I+1)-API_GRAV(I))*  &
                          (BTU_PER_GAL(I)-BTU_PER_GAL(I+1))+BTU_PER_GAL(I+1)
                API_TO_BTU = BTU_OUT * 42 / 1000000
                RETURN
            ENDIF
         ENDDO
      ENDIF

      RETURN
      END


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
      INTEGER UNITX,STEOFYR, STEOLYR
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
! OPEN AND READ FILE WITH STEO WELLHEAD PRICES
              XFILE="MNEXSTEO"
              UNITX=FILE_MGR('O',XFILE,OLD)
          IF (UNITX .GT. 0) THEN
              READ(UNITX,*) STEOFYR , STEOLYR
            DO IY=STEOFYR , STEOLYR
              READ(UNITX,*) STEOXPR(IY - STEOFYR + 1)
              STEOXPR(IY - STEOFYR + 1) = STEOXPR(IY - STEOFYR + 1) / SCALPR
            ENDDO
              UNITX=FILE_MGR('C',XFILE,OLD)
          ELSE
              WRITE(6,*) ' Open of file MNEXSTEO failed.'
          ENDIF
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
      IF (((CURIYR+BASEYR-2) .GE. STEOFYR) .AND. &
          ((CURIYR+BASEYR-2) .LE. STEOLYR)) THEN
         PS = STEOXPR((CURIYR + BASEYR - 1) - STEOFYR)
      END IF
      XOGWPRNG(MNUMOR,CURIYR-1) = PS     ! LAST YEAR'S WELLHEAD PRICE
      IF ((PS .GE. UPF) .AND. ((CURIYR+BASEYR-1) .GT. STEOLYR)) THEN
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
        IF (((IY+BASEYR-1) .GE. STEOFYR) .AND. &
            ((IY+BASEYR-1) .LE. STEOLYR)) THEN
           XOGWPRNG(MNUMOR,IY) = STEOXPR((IY + BASEYR) - STEOFYR)
           IF ((IY+BASEYR-1) .EQ. STEOLYR)  &
           CALL EXNGINVERT(QS,QF,XOGWPRNG(MNUMOR,IY),PF,A,B,C,EXPARG)
        ELSE
           XOGWPRNG(MNUMOR,IY) = A*(QS**EXPARG) + B*QS + C
          IF ((XOGWPRNG(MNUMOR,IY) .GT. UPF) .AND. (INTERVAL .EQ. 1) &
             .AND. ((IY+BASEYR-1) .GT. STEOLYR)) THEN
           EXPARG=EXPEXP2
           CALL EXNGINVERT(QS,QF,XOGWPRNG(MNUMOR,IY),PF,A,B,C,EXPARG)
           IF (IMODEL .EQ. 7) &
           write(6,'(a,f7.3,a,f7.3,a,i4)') ' The natural gas price of',xogwprng(mnumor,iy), &
            ' has exceeded the the economic point of unconventionals (',upf, &
            ') in expectation year ',iy+1989
           INTERVAL=2
          ENDIF
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
      SUBROUTINE EXOLUTIL(IMODEL)
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
!*******************************************************************C

      SUBROUTINE COPY_ADJUSTED

!  THIS SUBROUTINE COPIES PRICES FOR THE VARIABLES IN THE MPBLK
!  COMMON TO THE VARIABLES IN THE EPMMPBLK COMMON.  IF THE EPM IS
!  TURNED ON SUBROUTINE PRICE_ADJUST IS CALLED.  THIS SUBROUTINE
!  CALCULATES THE PRICE ADJUSTMENT OF END-USE FUELS. IN EACH CASE WE
!  FIND THE PRICE THAT THE DEMAND SECTORS RESPONT TO, AND ADJUST THE
!  PRICE BASED ON THE CARBON CONTENT OF THE FUEL AND THE TAX.
!     THIS VERSION WILL USE THE NEW ADJUSTED PRICE BLOCKS
!*******************************************************************

      IMPLICIT NONE
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'mpblk'
      include 'qblk'
      include 'qsblk'
      include 'indout'
      include 'ngtdmout'
      include 'cdsparms'
      include 'uefdout'
      include 'epmmpblk'
      include 'epmngtdm'
      include 'coalprc'
      include 'epmclprc'
      include 'convfact'
      include 'emablk'
      include 'epmcntl'
      include 'emoblk'
      include 'epmbank'
      include 'ponroad'
      include 'epmonrd'
      include 'epmeusprc'
      include 'eusprc'
      include 'ab32'
      include 'emeblk'
      include 'ghgrep'
      include 'calshr'
      include 'lfmmout'
      include 'pmmout'

      INTEGER I,J,K,IR,IYR,IV
      INTEGER X,Y,Z
      integer rtovalue
      external rtovalue ! to get run time option AB32SW
      integer AB32SW
      real AB32P9 ! function to get AB32 adjusted price in Pacific Division (#9)
      external AB32P9
      real emfac_qmgtr ! carbon emission factor for motor gasoline, adjusted for blended ethanol btu content which doesn't count
      real emfac_qettr ! carbon emission factor for E85, adjusted for gasoline content only
      real emfac_qdstr ! carbon emission factor for distillate, adjusted for biodiesel content which doesn't count
      real cal_ind_emis ! industrial carbon emissions for california, total, then net of covered industrial and refining.
      integer nseas
      integer CO2PLFMM  ! if 1, assum LFMM has included CO2 price adjustments in prices.  if 0, add CO2 adjustments here.
      integer file_mgr
      external file_mgr
      logical new,lexist
      integer pacific/0/
      logical done/.false./, doneyet/.false./
      character*16 rname

!     HERE WE COPY THE ADJUSTED PRICES FOR THIS YEAR ONLY

      DO IR=1,MNUMCR
        DO IV=1,MNUMP
           AMPRC(IR,CURIYR,IV)=MPRC(IR,CURIYR,IV)
        ENDDO
      ENDDO

!  COPY NGTDM variables other than PRICES TO ADJUSTED BLOCK
!  use array assignments--allowed in fortran 90
!     AGPRDNGON=OGPRDNGON
!     AGPRDNGOF=OGPRDNGOF
!     ARNG_PADD=PRNG_PADD
!     ALSYNGWP =CLSYNGWP
      APBAJA   =PBAJA
      AQBAJA   =QBAJA

      J=CURIYR


  if( (bank_flag.and.curcalyr.ge.bank_startyr) .or. .not. bank_flag) then

      DO I=1, MNUMCR
!    NATURAL GAS, CORE (RESID,COMM,TRANS,IND,UTIL)
         AGFRS(I,J) = PGFRS(I,J) +  JGFRS(J)
         AGFCM(I,J) = PGFCM(I,J) +  JGFCM(J)
         AGFTR(I,J) = PGFTR(I,J) +  JGFTR(J)
         AGFIN(I,J) = PGFIN(I,J) +  JGFIN(J)
       IF(QGFIN(I,J) .GT. 0.0) THEN
         AGFIN(I,J) = PGFIN(I,J) + (((QGFIN(I,J) - INQNGPF(I,J)) * &
               JGFIN(J) + JNQNGPF(J) * INQNGPF(I,J)) / QGFIN(I,J))
       ELSE
         AGFIN(I,J) = PGFIN(I,J) + JGFIN(J)
       END IF
         AGFEL(I,J) = PGFEL(I,J) +  JGFEL(J)

!    NATURAL GAS, NONCORE (RESID,COMM,TRANS,IND,UTIL)
         AGIRS(I,J) = PGIRS(I,J) +  JGIRS(J)
         AGICM(I,J) = PGICM(I,J) +  JGICM(J)
         AGITR(I,J) = PGITR(I,J) +  JGITR(J)
         AGIIN(I,J) = PGIIN(I,J) +  JGIIN(J)
         AGIEL(I,J) = PGIEL(I,J) +  JGIEL(J)

!    NATURAL GAS, TOTAL (RESID,COMM,TRANS,IND,UTIL)
         ANGRS(I,J) = PNGRS(I,J) +  JNGRS(J)
         ANGCM(I,J) = PNGCM(I,J) +  JNGCM(J)
         ANGTR(I,J) = PNGTR(I,J) +  JNGTR(J)
!    COMPRESSED and liquefied natural gas
         AGFTRFV(I,J) = PGFTRFV(I,J) +  JNGTR(J)
         AGFTRPV(I,J) = PGFTRPV(I,J) +  JNGTR(J)
         AGLTRFV(I,J) = PGLTRFV(I,J) +  JNGTR(J)
         AGLTRPV(I,J) = PGLTRPV(I,J) +  JNGTR(J)
         AGFTRRAIL(:,I,J) = PGFTRRAIL(:,I,J) + JNGTR(J)      ! rail use of natural gas
         AGLTRRAIL(:,I,J) = PGLTRRAIL(:,I,J) + JNGTR(J)
         AGFTRSHIP(:,I,J) = PGFTRSHIP(:,I,J) + JNGTR(J)      ! shipping use of natural gas
         AGLTRSHIP(:,I,J) = PGLTRSHIP(:,I,J) + JNGTR(J)

         ANGIN(I,J) = PNGIN(I,J) +  JNGIN(J)
       IF(QNGIN(I,J) .GT. 0.0) THEN
         ANGIN(I,J) = PNGIN(I,J) + (((QNGIN(I,J) - INQNGPF(I,J)) * &
              JNGIN(J) + JNQNGPF(J) * INQNGPF(I,J)) / QNGIN(I,J))
       ELSE
         ANGIN(I,J) = PNGIN(I,J) + JNGIN(J)
       END IF
         ANGEL(I,J) = PNGEL(I,J) +  JNGEL(J)

         AGPTR(I,J) = PGPTR(I,J) +  JGPTR(J)
         ALPIN(I,J) = PLPIN(I,J) +  JLPIN(J)

         IF(PCLRS(I,J).GE.15.0) PCLRS(I,J)=15.
         IF(PCLCM(I,J).GE.15.0) PCLCM(I,J)=15.
         IF(PCLIN(I,J).GE.15.0) PCLIN(I,J)=15.
         IF(PCLEL(I,J).GE.15.0) PCLEL(I,J)=15.
         ACLRS(I,J) = PCLRS(I,J) +  JCLRS(J)
         ACLCM(I,J) = PCLCM(I,J) +  JCLCM(J)
         ACLIN(I,J) = PCLIN(I,J) +  JCLIN(J)
         ACLEL(I,J) = PCLEL(I,J) +  JCLEL(J)
         ACLSN(I,J) = PCLSN(I,J) +  JCLIN(J)
         ACLGAS(I,J) = PCLGAS(I,J) +  JCLIN(J)
         AMCIN(I,J) = PMCIN(I,J) +  JMCIN(J)

! LFMM may include CO2 price adjustments in petroleum prices.  If run-time option
! CO2PLFMM=0, assume LFMM has not done so.  If =1, just copy prices.
       CO2PLFMM=rtovalue('CO2PLFMM',0)
       IF(CO2PLFMM.eq.0) then
         AMGCM(I,J) = PMGCM(I,J) +  JMGCM(J)
         AMGTR(I,J) = PMGTR(I,J) +  JMGTR(J)
         AMGIN(I,J) = PMGIN(I,J) +  JMGIN(J)
         AJFTR(I,J) = PJFTR(I,J) +  JJFTR(J)
         ADSRS(I,J) = PDSRS(I,J) +  JDSRS(J)
         ADSCM(I,J) = PDSCM(I,J) +  JDSCM(J)
         ADSTR(I,J) = PDSTR(I,J) +  JDSTR(J)
         ADSTRHWY(I,J) = PDSTRHWY(I,J) +  JDSTR(J)   ! slip on-road distillate in here
         ADSIN(I,J) = PDSIN(I,J) +  JDSIN(J)
         ADSEL(I,J) = PDSEL(I,J) +  JDSEL(J)
         AKSRS(I,J) = PKSRS(I,J) +  JKSRS(J)
         AKSCM(I,J) = PKSCM(I,J) +  JKSCM(J)
         AKSIN(I,J) = PKSIN(I,J) +  JKSIN(J)
         ALGRS(I,J) = PLGRS(I,J) +  JLGRS(J)
         ALGCM(I,J) = PLGCM(I,J) +  JLGCM(J)
         ALGTR(I,J) = PLGTR(I,J) +  JLGTR(J)
         ALGIN(I,J) = PLGIN(I,J) +  JLGIN(J)
         ALGINPF(I,J) = PLGINPF(I,J) + JNQLGPF(J)
         APRRS(I,J) = PPRRS(I,J) + JPRRS(J)
         APRCM(I,J) = PPRCM(I,J) + JPRCM(J)
         APRTR(I,J) = PPRTR(I,J) + JPRTR(J)
         AETIN(I,J) = PETIN(I,J) + JETIN(J)
         APRIN(I,J) = PPRIN(I,J) + JPRIN(J)
         ABUIN(I,J) = PBUIN(I,J) + JBUIN(J)
         AISIN(I,J) = PISIN(I,J) + JISIN(J)
         AETINPF(I,J) = PETINPF(I,J) + JETINPF(J)
         APRINPF(I,J) = PPRINPF(I,J) + JPRINPF(J)
         ABUINPF(I,J) = PBUINPF(I,J) + JBUINPF(J)
         AISINPF(I,J) = PISINPF(I,J) + JISINPF(J)
         APROLENERF(I,J) = PPROLENERF(I,J)              ! not adjusting
         ARLCM(I,J) = PRLCM(I,J) +  JRLCM(J)
         ARLTR(I,J) = PRLTR(I,J) +  JRLTR(J)
         ARLIN(I,J) = PRLIN(I,J) +  JRLIN(J)
         ARLEL(I,J) = PRLEL(I,J) +  JRLEL(J)
         ARHTR(I,J) = PRHTR(I,J) +  JRHTR(J)
         ARHEL(I,J) = PRHEL(I,J) +  JRHEL(J)
         ARSCM(I,J) = PRSCM(I,J) +  JRSCM(J)
         ARSTR(I,J) = PRSTR(I,J) +  JRSTR(J)
         ARSIN(I,J) = PRSIN(I,J) +  JRSIN(J)
         ARSEL(I,J) = PRSEL(I,J) +  JRSEL(J)
         APFIN(I,J) = PPFIN(I,J) +  JPFIN(J)
         APCIN(I,J) = PPCIN(I,J) +  JPCIN(J)
         APPIN(I,J) = PPPIN(I,J) +  JPPIN(J)
         APPINPF(I,J) = PPPINPF(I,J) +  JPPINPF(J)
         ALUIN(I,J) = PLUIN(I,J) +  JLUIN(J)
         AOTIN(I,J) = POTIN(I,J) +  JOTIN(J)
         AOTTR(I,J) = POTTR(I,J) +  JOTTR(J)
         AMETR(I,J) = PMETR(I,J) +  JMETR(J)
         AETTR(I,J) = PETTR(I,J) +  JETTR(J)
       ELSE
         AMGCM(I,J) = PMGCM(I,J)
         AMGTR(I,J) = PMGTR(I,J)
         AMGIN(I,J) = PMGIN(I,J)
         AJFTR(I,J) = PJFTR(I,J)
         ADSRS(I,J) = PDSRS(I,J)
         ADSCM(I,J) = PDSCM(I,J)
         ADSTR(I,J) = PDSTR(I,J)
         ADSTRHWY(I,J) = PDSTRHWY(I,J)
         ADSIN(I,J) = PDSIN(I,J)
         ADSEL(I,J) = PDSEL(I,J)
         AKSRS(I,J) = PKSRS(I,J)
         AKSCM(I,J) = PKSCM(I,J)
         AKSIN(I,J) = PKSIN(I,J)
         ALGRS(I,J) = PLGRS(I,J)
         ALGCM(I,J) = PLGCM(I,J)
         ALGTR(I,J) = PLGTR(I,J)
         ALGIN(I,J) = PLGIN(I,J)
         ALGINPF(I,J) = PLGINPF(I,J)
         APRRS(I,J) = PPRRS(I,J)
         APRCM(I,J) = PPRCM(I,J)
         APRTR(I,J) = PPRTR(I,J)
         AETIN(I,J) = PETIN(I,J)
         APRIN(I,J) = PPRIN(I,J)
         ABUIN(I,J) = PBUIN(I,J)
         AISIN(I,J) = PISIN(I,J)
         AETINPF(I,J) = PETINPF(I,J)
         APRINPF(I,J) = PPRINPF(I,J)
         ABUINPF(I,J) = PBUINPF(I,J)
         AISINPF(I,J) = PISINPF(I,J)
         APROLENERF(I,J) = PPROLENERF(I,J)
         ARLCM(I,J) = PRLCM(I,J)
         ARLTR(I,J) = PRLTR(I,J)
         ARLIN(I,J) = PRLIN(I,J)
         ARLEL(I,J) = PRLEL(I,J)
         ARHTR(I,J) = PRHTR(I,J)
         ARHEL(I,J) = PRHEL(I,J)
         ARSCM(I,J) = PRSCM(I,J)
         ARSTR(I,J) = PRSTR(I,J)
         ARSIN(I,J) = PRSIN(I,J)
         ARSEL(I,J) = PRSEL(I,J)
         APFIN(I,J) = PPFIN(I,J)
         APCIN(I,J) = PPCIN(I,J)
         APPIN(I,J) = PPPIN(I,J)
         APPINPF(I,J) = PPPINPF(I,J)
         ALUIN(I,J) = PLUIN(I,J)
         AOTIN(I,J) = POTIN(I,J)
         AOTTR(I,J) = POTTR(I,J)
         AMETR(I,J) = PMETR(I,J) + JMETR(J)
         AETTR(I,J) = PETTR(I,J)
       ENDIF
! electricity prices at end use level (no tax)
         AELSHRS(I,J) = PELSHRS(I,J)
         AELCLRS(I,J) = PELCLRS(I,J)
         AELWHRS(I,J) = PELWHRS(I,J)
         AELCKRS(I,J) = PELCKRS(I,J)
         AELCDRS(I,J) = PELCDRS(I,J)
         AELRFRS(I,J) = PELRFRS(I,J)
         AELFZRS(I,J) = PELFZRS(I,J)
         AELLTRS(I,J) = PELLTRS(I,J)
         AELOTRS(I,J) = PELOTRS(I,J)
         AELH2RS(I,J) = PELH2RS(I,J)
         AELSHCM(I,J) = PELSHCM(I,J)
         AELSCCM(I,J) = PELSCCM(I,J)
         AELWHCM(I,J) = PELWHCM(I,J)
         AELVTCM(I,J) = PELVTCM(I,J)
         AELCKCM(I,J) = PELCKCM(I,J)
         AELLTCM(I,J) = PELLTCM(I,J)
         AELRFCM(I,J) = PELRFCM(I,J)
         AELOPCM(I,J) = PELOPCM(I,J)
         AELONCM(I,J) = PELONCM(I,J)
         AELOTCM(I,J) = PELOTCM(I,J)
         AELINP(I,J) = PELINP(I,J)
         AELINS(I,J) = PELINS(I,J)
         AELINM(I,J) = PELINM(I,J)
         AELLTTR(I,J) = PELLTTR(I,J)
         AELVHTR(I,J) = PELVHTR(I,J)
      ENDDO

!     ADJUST MARGINAL PRICES BY COAL REGIONS

      DO I = 1,NDRG2
        APCLELCDR(1,I,J) = PCLELCDR(1,I,J) + JCLEL(J)
        APCLELCDR(2,I,J) = PCLELCDR(2,I,J) + JCLEL(J)
      END DO

!     ADJUST natural GAS prices by EMM/NGMM REGIONS

      DO I = 1,NNGEM
         AGFELGR(I,J) = PGFELGR(I,J) +  JGFELGR(J)
         AGIELGR(I,J) = PGIELGR(I,J) +  JGIELGR(J)
         AGCELGR(I,J) = PGCELGR(I,J) +  JGCELGR(J)
      ENDDO
! seasonal:
      DO I = 1,NNGEM
        do nseas=1,3
         ASPNGELGR(I,J,nseas) = SPNGELGR(I,J,nseas) +  JNGEL(J)
        enddo
        APNGELGR(I,J) = PNGELGR(I,J) +  JNGEL(J)
      ENDDO
      DO I = 1,NNGEM
        do nseas=1,2
         ASPGFELGR(I,J,nseas) = SPGFELGR(I,J,nseas) +  JGFELGR(J)
         ASPGIELGR(I,J,nseas) = SPGIELGR(I,J,nseas) +  JGIELGR(J)
        enddo
      ENDDO
   else
! If before compliance period in a banking case, to not pass the allowance price
! on to end use prices.  They will, however, be used in capacity decisions.
      DO I=1, MNUMCR
!    NATURAL GAS, CORE (RESID,COMM,TRANS,IND,UTIL)
         AGFRS(I,J) = PGFRS(I,J)
         AGFCM(I,J) = PGFCM(I,J)
         AGFTR(I,J) = PGFTR(I,J)
         AGFIN(I,J) = PGFIN(I,J)
         AGFIN(I,J) = PGFIN(I,J)
         AGFEL(I,J) = PGFEL(I,J)

!    NATURAL GAS, NONCORE (RESID,COMM,TRANS,IND,UTIL)
         AGIRS(I,J) = PGIRS(I,J)
         AGICM(I,J) = PGICM(I,J)
         AGITR(I,J) = PGITR(I,J)
         AGIIN(I,J) = PGIIN(I,J)
         AGIEL(I,J) = PGIEL(I,J)

!    NATURAL GAS, TOTAL (RESID,COMM,TRANS,IND,UTIL)
         ANGRS(I,J) = PNGRS(I,J)
         ANGCM(I,J) = PNGCM(I,J)
         ANGTR(I,J) = PNGTR(I,J)
         AGFTRFV(I,J) = PGFTRFV(I,J)     ! these next two are compressed natural gas
         AGFTRPV(I,J) = PGFTRPV(I,J)
         AGLTRFV(I,J) = PGLTRFV(I,J)     ! these next two are liquefied natural gas
         AGLTRPV(I,J) = PGLTRPV(I,J)
         AGFTRRAIL(:,I,J) = PGFTRRAIL(:,I,J)      ! rail use of natural gas
         AGLTRRAIL(:,I,J) = PGLTRRAIL(:,I,J)
         AGFTRSHIP(:,I,J) = PGFTRSHIP(:,I,J)      ! shipping use of natural gas
         AGLTRSHIP(:,I,J) = PGLTRSHIP(:,I,J)
         ANGIN(I,J) = PNGIN(I,J)
         ANGIN(I,J) = PNGIN(I,J)
         ANGEL(I,J) = PNGEL(I,J)

         AGPTR(I,J) = PGPTR(I,J)
         ALPIN(I,J) = PLPIN(I,J)

! Coal
         ACLRS(I,J) = PCLRS(I,J)
         ACLCM(I,J) = PCLCM(I,J)
         ACLIN(I,J) = PCLIN(I,J)
         ACLEL(I,J) = PCLEL(I,J)
         ACLSN(I,J) = PCLSN(I,J)
         ACLGAS(I,J) = PCLGAS(I,J)
         AMCIN(I,J) = PMCIN(I,J)
         AMGCM(I,J) = PMGCM(I,J)
         AMGTR(I,J) = PMGTR(I,J)
         AMGIN(I,J) = PMGIN(I,J)
         AJFTR(I,J) = PJFTR(I,J)
         ADSRS(I,J) = PDSRS(I,J)
         ADSCM(I,J) = PDSCM(I,J)
         ADSTR(I,J) = PDSTR(I,J)
         ADSTRHWY(I,J) = PDSTRHWY(I,J)               ! slip on-road distillate in here
         ADSIN(I,J) = PDSIN(I,J)
         ADSEL(I,J) = PDSEL(I,J)
         AKSRS(I,J) = PKSRS(I,J)
         AKSCM(I,J) = PKSCM(I,J)
         AKSIN(I,J) = PKSIN(I,J)
         ALGRS(I,J) = PLGRS(I,J)
         ALGCM(I,J) = PLGCM(I,J)
         ALGTR(I,J) = PLGTR(I,J)
         ALGIN(I,J) = PLGIN(I,J)
         ALGINPF(I,J) = PLGINPF(I,J)
         APRRS(I,J) = PPRRS(I,J)
         APRCM(I,J) = PPRCM(I,J)
         APRTR(I,J) = PPRTR(I,J)
         AETIN(I,J) = PETIN(I,J)
         APRIN(I,J) = PPRIN(I,J)
         ABUIN(I,J) = PBUIN(I,J)
         AISIN(I,J) = PISIN(I,J)
         AETINPF(I,J) = PETINPF(I,J)
         APRINPF(I,J) = PPRINPF(I,J)
         ABUINPF(I,J) = PBUINPF(I,J)
         AISINPF(I,J) = PISINPF(I,J)
         APROLENERF(I,J) = PPROLENERF(I,J)
         ARLCM(I,J) = PRLCM(I,J)
         ARLTR(I,J) = PRLTR(I,J)
         ARLIN(I,J) = PRLIN(I,J)
         ARLEL(I,J) = PRLEL(I,J)
         ARHTR(I,J) = PRHTR(I,J)
         ARHEL(I,J) = PRHEL(I,J)
         ARSCM(I,J) = PRSCM(I,J)
         ARSTR(I,J) = PRSTR(I,J)
         ARSIN(I,J) = PRSIN(I,J)
         ARSEL(I,J) = PRSEL(I,J)
         APFIN(I,J) = PPFIN(I,J)
         APCIN(I,J) = PPCIN(I,J)
         APPIN(I,J) = PPPIN(I,J)
         APPINPF(I,J) = PPPINPF(I,J)
         ALUIN(I,J) = PLUIN(I,J)
         AOTIN(I,J) = POTIN(I,J)
         AOTTR(I,J) = POTTR(I,J)
         AMETR(I,J) = PMETR(I,J)
         AETTR(I,J) = PETTR(I,J)

! electricity prices at end use level
         AELSHRS(I,J) = PELSHRS(I,J)
         AELCLRS(I,J) = PELCLRS(I,J)
         AELWHRS(I,J) = PELWHRS(I,J)
         AELCKRS(I,J) = PELCKRS(I,J)
         AELCDRS(I,J) = PELCDRS(I,J)
         AELRFRS(I,J) = PELRFRS(I,J)
         AELFZRS(I,J) = PELFZRS(I,J)
         AELLTRS(I,J) = PELLTRS(I,J)
         AELOTRS(I,J) = PELOTRS(I,J)
         AELH2RS(I,J) = PELH2RS(I,J)
         AELSHCM(I,J) = PELSHCM(I,J)
         AELSCCM(I,J) = PELSCCM(I,J)
         AELWHCM(I,J) = PELWHCM(I,J)
         AELVTCM(I,J) = PELVTCM(I,J)
         AELCKCM(I,J) = PELCKCM(I,J)
         AELLTCM(I,J) = PELLTCM(I,J)
         AELRFCM(I,J) = PELRFCM(I,J)
         AELOPCM(I,J) = PELOPCM(I,J)
         AELONCM(I,J) = PELONCM(I,J)
         AELOTCM(I,J) = PELOTCM(I,J)
         AELINP(I,J) = PELINP(I,J)
         AELINS(I,J) = PELINS(I,J)
         AELINM(I,J) = PELINM(I,J)
         AELLTTR(I,J) = PELLTTR(I,J)
         AELVHTR(I,J) = PELVHTR(I,J)
      ENDDO

!  MARGINAL PRICES BY COAL REGIONS
      DO I = 1,NDRG2
        APCLELCDR(1,I,J) = PCLELCDR(1,I,J)
        APCLELCDR(2,I,J) = PCLELCDR(2,I,J)
      END DO

!  natural GAS prices by EMM/NGMM REGIONS
      DO I = 1,NNGEM
         AGFELGR(I,J) = PGFELGR(I,J)
         AGIELGR(I,J) = PGIELGR(I,J)
         AGCELGR(I,J) = PGCELGR(I,J)
      ENDDO
! seasonal natural gas prices
      DO I = 1,NNGEM
        do nseas=1,3
         ASPNGELGR(I,J,nseas) = SPNGELGR(I,J,nseas)
        enddo
        APNGELGR(I,J) = PNGELGR(I,J)
      ENDDO
      DO I = 1,NNGEM
        do nseas=1,2
         ASPGFELGR(I,J,nseas) = SPGFELGR(I,J,nseas)
         ASPGIELGR(I,J,nseas) = SPGIELGR(I,J,nseas)
        enddo
      ENDDO

   endif

! Read California shares of
     if(.not. doneyet) then
        doneyet=.true.
        pacific=file_mgr('O','CAFSHAREX         ',.false.)

!  read all defined ranges from worksheet
        CALL ReadRngXLSX(pacific,'cafshare')  ! sheet name is cafshare
        CLOSE(pacific)
!  fill arrays from worksheet range data
!  37 columns for each range: first column of range put in position 25 of *_SHR (for 2014 normally), but holds seds-based 2015 share (not 2014).
!  Second column goes into position 26, for 2015) and holds share computed for 2015 which should match 1st column.  37th column is 2050
!  Shares should change over time unless the spreadsheet option to holds shares constant overt time is on.

        rname='ELRS_SHR';call getrngr(rname, ELRS_SHR(25),1,37,1)
        rname='ELCM_SHR';call getrngr(rname, ELCM_SHR(25),1,37,1)
        rname='ELTR_SHR';call getrngr(rname, ELTR_SHR(25),1,37,1)
        rname='ELIN_SHR';call getrngr(rname, ELIN_SHR(25),1,37,1)
        rname='NGRS_SHR';call getrngr(rname, NGRS_SHR(25),1,37,1)
        rname='NGCM_SHR';call getrngr(rname, NGCM_SHR(25),1,37,1)
        rname='NGTR_SHR';call getrngr(rname, NGTR_SHR(25),1,37,1)
        rname='NGIN_SHR';call getrngr(rname, NGIN_SHR(25),1,37,1)
        rname='NGEL_SHR';call getrngr(rname, NGEL_SHR(25),1,37,1)
        rname='GPTR_SHR';call getrngr(rname, GPTR_SHR(25),1,37,1)
        rname='LPIN_SHR';call getrngr(rname, LPIN_SHR(25),1,37,1)
        rname='CLCM_SHR';call getrngr(rname, CLCM_SHR(25),1,37,1)
        rname='CLIN_SHR';call getrngr(rname, CLIN_SHR(25),1,37,1)
        rname='CLEL_SHR';call getrngr(rname, CLEL_SHR(25),1,37,1)
        rname='MGCM_SHR';call getrngr(rname, MGCM_SHR(25),1,37,1)
        rname='MGTR_SHR';call getrngr(rname, MGTR_SHR(25),1,37,1)
        rname='MGIN_SHR';call getrngr(rname, MGIN_SHR(25),1,37,1)
        rname='JFTR_SHR';call getrngr(rname, JFTR_SHR(25),1,37,1)
        rname='DSRS_SHR';call getrngr(rname, DSRS_SHR(25),1,37,1)
        rname='DSCM_SHR';call getrngr(rname, DSCM_SHR(25),1,37,1)
        rname='DSTR_SHR';call getrngr(rname, DSTR_SHR(25),1,37,1)
        rname='DSIN_SHR';call getrngr(rname, DSIN_SHR(25),1,37,1)
        rname='DSEL_SHR';call getrngr(rname, DSEL_SHR(25),1,37,1)
        rname='KSRS_SHR';call getrngr(rname, KSRS_SHR(25),1,37,1)
        rname='KSCM_SHR';call getrngr(rname, KSCM_SHR(25),1,37,1)
        rname='KSIN_SHR';call getrngr(rname, KSIN_SHR(25),1,37,1)
        rname='LGRS_SHR';call getrngr(rname, LGRS_SHR(25),1,37,1)
        rname='LGCM_SHR';call getrngr(rname, LGCM_SHR(25),1,37,1)
        rname='LGTR_SHR';call getrngr(rname, LGTR_SHR(25),1,37,1)
        rname='LGIN_SHR';call getrngr(rname, LGIN_SHR(25),1,37,1)
        rname='RSCM_SHR';call getrngr(rname, RSCM_SHR(25),1,37,1)
        rname='RSTR_SHR';call getrngr(rname, RSTR_SHR(25),1,37,1)
        rname='RSIN_SHR';call getrngr(rname, RSIN_SHR(25),1,37,1)
        rname='RSEL_SHR';call getrngr(rname, RSEL_SHR(25),1,37,1)
        rname='SGIN_SHR';call getrngr(rname, SGIN_SHR(25),1,37,1)
        rname='PCIN_SHR';call getrngr(rname, PCIN_SHR(25),1,37,1)
!        rname='PCEL_SHR';call getrngr(rname, PCEL_SHR(25),1,37,1)
        rname='PCAS_SHR';call getrngr(rname, PCAS_SHR(25),1,37,1)
        rname='ASIN_SHR';call getrngr(rname, ASIN_SHR(25),1,37,1)
        rname='OTTR_SHR';call getrngr(rname, OTTR_SHR(25),1,37,1)
        rname='OTIN_SHR';call getrngr(rname, OTIN_SHR(25),1,37,1)
        rname='ETTR_SHR';call getrngr(rname, ETTR_SHR(25),1,37,1)
        rname='UREL_SHR';call getrngr(rname, UREL_SHR(25),1,37,1)
        rname='HOIN_SHR';call getrngr(rname, HOIN_SHR(25),1,37,1)
        rname='HOEL_SHR';call getrngr(rname, HOEL_SHR(25),1,37,1)
        rname='GERS_SHR';call getrngr(rname, GERS_SHR(25),1,37,1)
        rname='GEIN_SHR';call getrngr(rname, GEIN_SHR(25),1,37,1)
        rname='GEEL_SHR';call getrngr(rname, GEEL_SHR(25),1,37,1)
        rname='BMRS_SHR';call getrngr(rname, BMRS_SHR(25),1,37,1)
        rname='BMCM_SHR';call getrngr(rname, BMCM_SHR(25),1,37,1)
        rname='BMIN_SHR';call getrngr(rname, BMIN_SHR(25),1,37,1)
        rname='BMEL_SHR';call getrngr(rname, BMEL_SHR(25),1,37,1)
        rname='MSIN_SHR';call getrngr(rname, MSIN_SHR(25),1,37,1)
        rname='STRS_SHR';call getrngr(rname, STRS_SHR(25),1,37,1)
        rname='STCM_SHR';call getrngr(rname, STCM_SHR(25),1,37,1)
        rname='STIN_SHR';call getrngr(rname, STIN_SHR(25),1,37,1)
        rname='STEL_SHR';call getrngr(rname, STEL_SHR(25),1,37,1)
        rname='WIIN_SHR';call getrngr(rname, WIIN_SHR(25),1,37,1)
        rname='WIEL_SHR';call getrngr(rname, WIEL_SHR(25),1,37,1)
        rname='EIEL_SHR';call getrngr(rname, EIEL_SHR(25),1,37,1)

     endif


     if(curiyr.eq.lastyr.and. FCRL.eq.1 .and.  .not. done) then
! write out Pacific Division energy consumption to csv file. Is used to compile California energy sharing assumptions
! read from input spreadsheet cafshare.xml for ab32 calculations
       done=.true.
925    format(a,',',f8.5,',',<MNUMYR>(F10.3,','))   ! csv format
       new=.true.
       pacific=file_mgr('O','PACIFIC           ',new)


       if(QSELRS(9,MSEDYR).gt.0.) write(pacific,925) 'ELrs', QSELRS(10,MSEDYR) / QSELRS(9,MSEDYR), (QELRS(9,I),i=msedyr,lastyr)!  1 Purchased Electricity - Residential
       if(QSELCM(9,MSEDYR).gt.0.) write(pacific,925) 'ELcm', QSELCM(10,MSEDYR) / QSELCM(9,MSEDYR), (QELCM(9,I),i=msedyr,lastyr)!  2 Purchased Electricity - Commercial
       if(QSELTR(9,MSEDYR).gt.0.) write(pacific,925) 'ELtr', QSELTR(10,MSEDYR) / QSELTR(9,MSEDYR), (QELTR(9,I),i=msedyr,lastyr)!  3 Purchased Electricity - Transportation
       if(QSELIN(9,MSEDYR).gt.0.) write(pacific,925) 'ELin', QSELIN(10,MSEDYR) / QSELIN(9,MSEDYR), (QELIN(9,I),i=msedyr,lastyr)!  4 Purchased Electricity - Industrial
       if(QSELRF(9,MSEDYR).gt.0.) write(pacific,925) 'ELrf', QSELRF(10,MSEDYR) / QSELRF(9,MSEDYR), (QELRF(9,I),i=msedyr,lastyr)!  5 Purchased Electricity - Refinery
       if(QSELHM(9,MSEDYR).gt.0.) write(pacific,925) 'ELhm', QSELHM(10,MSEDYR) / QSELHM(9,MSEDYR), (QELHM(9,I),i=msedyr,lastyr)!  6 Purchased Electricity - Hydrogen

       if(QSNGRS(9,MSEDYR).gt.0.) write(pacific,925) 'NGrs', QSNGRS(10,MSEDYR) / QSNGRS(9,MSEDYR), (QNGRS(9,I),i=msedyr,lastyr)! 24 Natural Gas - Residential
       if(QSNGCM(9,MSEDYR).gt.0.) write(pacific,925) 'NGcm', QSNGCM(10,MSEDYR) / QSNGCM(9,MSEDYR), (QNGCM(9,I),i=msedyr,lastyr)! 25 Natural Gas - Commercial
       if(QSNGTR(9,MSEDYR).gt.0.) write(pacific,925) 'NGtr', QSNGTR(10,MSEDYR) / QSNGTR(9,MSEDYR), (QNGTR(9,I),i=msedyr,lastyr)! 26 Natural Gas - Transportation
       if(QSNGIN(9,MSEDYR).gt.0.) write(pacific,925) 'NGin', QSNGIN(10,MSEDYR) / QSNGIN(9,MSEDYR), (QNGIN(9,I),i=msedyr,lastyr)! 27 Natural Gas - Industrial
       if(QSNGRF(9,MSEDYR).gt.0.) write(pacific,925) 'NGrf', QSNGRF(10,MSEDYR) / QSNGRF(9,MSEDYR), (QNGRF(9,I),i=msedyr,lastyr)! 28 Natural Gas - Refinery
       if(QSNGEL(9,MSEDYR).gt.0.) write(pacific,925) 'NGel', QSNGEL(10,MSEDYR) / QSNGEL(9,MSEDYR), (QNGEL(9,I),i=msedyr,lastyr)! 29 Natural Gas - Electricity
       if(QSNGHM(9,MSEDYR).gt.0.) write(pacific,925) 'NGhm', QSNGHM(10,MSEDYR) / QSNGHM(9,MSEDYR), (QNGHM(9,I),i=msedyr,lastyr)! 30 Natural Gas - Hydrogen
       if(QSGPTR(9,MSEDYR).gt.0.) write(pacific,925) 'GPtr', QSGPTR(10,MSEDYR) / QSGPTR(9,MSEDYR), (QGPTR(9,I),i=msedyr,lastyr)! 32 Natural Gas - Pipeline
       if(QSLPIN(9,MSEDYR).gt.0.) write(pacific,925) 'LPin', QSLPIN(10,MSEDYR) / QSLPIN(9,MSEDYR), (QLPIN(9,I),i=msedyr,lastyr)! 33 Natural Gas - Lease and Plant Fuel

       if(QSCLRS(9,MSEDYR).gt.0.) write(pacific,925) 'CLrs', QSCLRS(10,MSEDYR) / QSCLRS(9,MSEDYR), (QCLRS(9,I),i=msedyr,lastyr)! 34 Coal - Residential
       if(QSCLCM(9,MSEDYR).gt.0.) write(pacific,925) 'CLcm', QSCLCM(10,MSEDYR) / QSCLCM(9,MSEDYR), (QCLCM(9,I),i=msedyr,lastyr)! 35 Coal - Commercial
       if(QSCLIN(9,MSEDYR).gt.0.) write(pacific,925) 'CLin', QSCLIN(10,MSEDYR) / QSCLIN(9,MSEDYR), (QCLIN(9,I),i=msedyr,lastyr)! 36 Coal - Industrial
       if(QSCLRF(9,MSEDYR).gt.0.) write(pacific,925) 'CLrf', QSCLRF(10,MSEDYR) / QSCLRF(9,MSEDYR), (QCLRF(9,I),i=msedyr,lastyr)! 37 Coal - Refinery
       if(QSCLEL(9,MSEDYR).gt.0.) write(pacific,925) 'CLel', QSCLEL(10,MSEDYR) / QSCLEL(9,MSEDYR), (QCLEL(9,I),i=msedyr,lastyr)! 38 Coal - Electricity
       if(QSCLSN(9,MSEDYR).gt.0.) write(pacific,925) 'CLsn', QSCLSN(10,MSEDYR) / QSCLSN(9,MSEDYR), (QCLSN(9,I),i=msedyr,lastyr)! 39 Coal - Synthetics
       if(QSCLHM(9,MSEDYR).gt.0.) write(pacific,925) 'CLhm', QSCLHM(10,MSEDYR) / QSCLHM(9,MSEDYR), (QCLHM(9,I),i=msedyr,lastyr)! 40 Coal - Hydrogen

       if(QSMCIN(9,MSEDYR).gt.0.) write(pacific,925) 'MCin', QSMCIN(10,MSEDYR) / QSMCIN(9,MSEDYR), (QMCIN(9,I),i=msedyr,lastyr)! 42 Metallurgical Coal - Industrial
       if(QSMGCM(9,MSEDYR).gt.0.) write(pacific,925) 'MGcm', QSMGCM(10,MSEDYR) / QSMGCM(9,MSEDYR), (QMGCM(9,I),i=msedyr,lastyr)! 43 Motor Gasoline - Commercial
       if(QSMGTR(9,MSEDYR).gt.0.) write(pacific,925) 'MGtr', QSMGTR(10,MSEDYR) / QSMGTR(9,MSEDYR), (QMGTR(9,I),i=msedyr,lastyr)! 44 Motor Gasoline - Transportation
       if(QSMGIN(9,MSEDYR).gt.0.) write(pacific,925) 'MGin', QSMGIN(10,MSEDYR) / QSMGIN(9,MSEDYR), (QMGIN(9,I),i=msedyr,lastyr)! 45 Motor Gasoline - Industrial

       if(QSJFTR(9,MSEDYR).gt.0.) write(pacific,925) 'JFtr', QSJFTR(10,MSEDYR) / QSJFTR(9,MSEDYR), (QJFTR(9,I),i=msedyr,lastyr)! 47 Jet Fuel - Transportation
       if(QSDSRS(9,MSEDYR).gt.0.) write(pacific,925) 'DSrs', QSDSRS(10,MSEDYR) / QSDSRS(9,MSEDYR), (QDSRS(9,I),i=msedyr,lastyr)! 48 Distillate - Residential
       if(QSDSCM(9,MSEDYR).gt.0.) write(pacific,925) 'DScm', QSDSCM(10,MSEDYR) / QSDSCM(9,MSEDYR), (QDSCM(9,I),i=msedyr,lastyr)! 49 Distillate - Commercial
       if(QSDSTR(9,MSEDYR).gt.0.) write(pacific,925) 'DStr', QSDSTR(10,MSEDYR) / QSDSTR(9,MSEDYR), (QDSTR(9,I),i=msedyr,lastyr)! 50 Distillate - Transportation
       if(QSDSIN(9,MSEDYR).gt.0.) write(pacific,925) 'DSin', QSDSIN(10,MSEDYR) / QSDSIN(9,MSEDYR), (QDSIN(9,I),i=msedyr,lastyr)! 51 Distillate - Industrial
       if(QSDSRF(9,MSEDYR).gt.0.) write(pacific,925) 'DSrf', QSDSRF(10,MSEDYR) / QSDSRF(9,MSEDYR), (QDSRF(9,I),i=msedyr,lastyr)! 52 Distillate - Refinery
       if(QSDSEL(9,MSEDYR).gt.0.) write(pacific,925) 'DSel', QSDSEL(10,MSEDYR) / QSDSEL(9,MSEDYR), (QDSEL(9,I),i=msedyr,lastyr)! 53 Distillate - Electricity (+petroleum coke)

       if(QSKSRS(9,MSEDYR).gt.0.) write(pacific,925) 'KSrs', QSKSRS(10,MSEDYR) / QSKSRS(9,MSEDYR), (QKSRS(9,I),i=msedyr,lastyr)! 55 Kerosene - Residential
       if(QSKSCM(9,MSEDYR).gt.0.) write(pacific,925) 'KScm', QSKSCM(10,MSEDYR) / QSKSCM(9,MSEDYR), (QKSCM(9,I),i=msedyr,lastyr)! 56 Kerosene - Commercial
       if(QSKSIN(9,MSEDYR).gt.0.) write(pacific,925) 'KSin', QSKSIN(10,MSEDYR) / QSKSIN(9,MSEDYR), (QKSIN(9,I),i=msedyr,lastyr)! 57 Kerosene - Industrial

       if(QSLGRS(9,MSEDYR).gt.0.) write(pacific,925) 'LGrs', QSLGRS(10,MSEDYR) / QSLGRS(9,MSEDYR), (QLGRS(9,I),i=msedyr,lastyr)! 59 Liquid Petroleum Gases - Residential
       if(QSLGCM(9,MSEDYR).gt.0.) write(pacific,925) 'LGcm', QSLGCM(10,MSEDYR) / QSLGCM(9,MSEDYR), (QLGCM(9,I),i=msedyr,lastyr)! 60 Liquid Petroleum Gases - Commercial
       if(QSLGTR(9,MSEDYR).gt.0.) write(pacific,925) 'LGtr', QSLGTR(10,MSEDYR) / QSLGTR(9,MSEDYR), (QLGTR(9,I),i=msedyr,lastyr)! 61 Liquid Petroleum Gases - Transportation
       if(QSLGIN(9,MSEDYR).gt.0.) write(pacific,925) 'LGin', QSLGIN(10,MSEDYR) / QSLGIN(9,MSEDYR), (QLGIN(9,I),i=msedyr,lastyr)! 62 Liquid Petroleum Gases - Industrial
       if(QSLGRF(9,MSEDYR).gt.0.) write(pacific,925) 'LGrf', QSLGRF(10,MSEDYR) / QSLGRF(9,MSEDYR), (QLGRF(9,I),i=msedyr,lastyr)! 63 Liquid Petroleum Gases - Refinery

       if(QSRSCM(9,MSEDYR).gt.0.) write(pacific,925) 'RScm', QSRSCM(10,MSEDYR) / QSRSCM(9,MSEDYR), (QRSCM(9,I),i=msedyr,lastyr)! 74 Residual Fuel - Commercial
       if(QSRSTR(9,MSEDYR).gt.0.) write(pacific,925) 'RStr', QSRSTR(10,MSEDYR) / QSRSTR(9,MSEDYR), (QRSTR(9,I),i=msedyr,lastyr)! 75 Residual Fuel - Transportation
       if(QSRSIN(9,MSEDYR).gt.0.) write(pacific,925) 'RSin', QSRSIN(10,MSEDYR) / QSRSIN(9,MSEDYR), (QRSIN(9,I),i=msedyr,lastyr)! 76 Residual Fuel - Industrial
       if(QSRSRF(9,MSEDYR).gt.0.) write(pacific,925) 'RSrf', QSRSRF(10,MSEDYR) / QSRSRF(9,MSEDYR), (QRSRF(9,I),i=msedyr,lastyr)! 77 Residual Fuel - Refinery
       if(QSRSEL(9,MSEDYR).gt.0.) write(pacific,925) 'RSel', QSRSEL(10,MSEDYR) / QSRSEL(9,MSEDYR), (QRSEL(9,I),i=msedyr,lastyr)! 78 Residual Fuel - Electricity

       if(QSPFIN(9,MSEDYR).gt.0.) write(pacific,925) 'PFin', QSPFIN(10,MSEDYR) / QSPFIN(9,MSEDYR), (QPFIN(9,I),i=msedyr,lastyr)! 80 Petrochemical Feedstocks - Industrial
       if(QSSGIN(9,MSEDYR).gt.0.) write(pacific,925) 'SGin', QSSGIN(10,MSEDYR) / QSSGIN(9,MSEDYR), (QSGIN(9,I),i=msedyr,lastyr)! 81 Still Gas - Industrial
       if(QSSGRF(9,MSEDYR).gt.0.) write(pacific,925) 'SGrf', QSSGRF(10,MSEDYR) / QSSGRF(9,MSEDYR), (QSGRF(9,I),i=msedyr,lastyr)! 82 Still Gas - Refinery
       if(QSPCIN(9,MSEDYR).gt.0.) write(pacific,925) 'PCin', QSPCIN(10,MSEDYR) / QSPCIN(9,MSEDYR), (QPCIN(9,I),i=msedyr,lastyr)! 83 Petroleum Coke - Industrial
       if(QSPCRF(9,MSEDYR).gt.0.) write(pacific,925) 'PCrf', QSPCRF(10,MSEDYR) / QSPCRF(9,MSEDYR), (QPCRF(9,I),i=msedyr,lastyr)! 84 Petroleum Coke - Refinery
       if(QSPCEL(9,MSEDYR).gt.0.) write(pacific,925) 'PCel', QSPCEL(10,MSEDYR) / QSPCEL(9,MSEDYR), (QPCEL(9,I),i=msedyr,lastyr)! 85 Petroleum Coke - Electricity
       if(QSPCAS(9,MSEDYR).gt.0.) write(pacific,925) 'PCas', QSPCAS(10,MSEDYR) / QSPCAS(9,MSEDYR), (QPCAS(9,I),i=msedyr,lastyr)! 86 Petroleum Coke - All Sectors
       if(QSASIN(9,MSEDYR).gt.0.) write(pacific,925) 'ASin', QSASIN(10,MSEDYR) / QSASIN(9,MSEDYR), (QASIN(9,I),i=msedyr,lastyr)! 87 Asphalt and Road Oil - Industrial
       if(QSOTTR(9,MSEDYR).gt.0.) write(pacific,925) 'OTtr', QSOTTR(10,MSEDYR) / QSOTTR(9,MSEDYR), (QOTTR(9,I),i=msedyr,lastyr)! 88 Other Petroleum - Transportation
       if(QSOTIN(9,MSEDYR).gt.0.) write(pacific,925) 'OTin', QSOTIN(10,MSEDYR) / QSOTIN(9,MSEDYR), (QOTIN(9,I),i=msedyr,lastyr)! 89 Other Petroleum - Industrial
       if(QSOTRF(9,MSEDYR).gt.0.) write(pacific,925) 'OTrf', QSOTRF(10,MSEDYR) / QSOTRF(9,MSEDYR), (QOTRF(9,I),i=msedyr,lastyr)! 90 Other Petroleum - Refinery

       if(QSMETR(9,MSEDYR).gt.0.) write(pacific,925) 'MEtr', QSMETR(10,MSEDYR) / QSMETR(9,MSEDYR), (QMETR(9,I),i=msedyr,lastyr)! 99 Methanol - Transporation
       if(QSETTR(9,MSEDYR).gt.0.) write(pacific,925) 'ETtr', QSETTR(10,MSEDYR) / QSETTR(9,MSEDYR), (QETTR(9,I),i=msedyr,lastyr)!100 Ethanol - Transporation
       if(QSETHM(9,MSEDYR).gt.0.) write(pacific,925) 'EThm', QSETHM(10,MSEDYR) / QSETHM(9,MSEDYR), (QETHM(9,I),i=msedyr,lastyr)!101 Ethanol - Hydrogen
       if(QSHYTR(9,MSEDYR).gt.0.) write(pacific,925) 'HYtr', QSHYTR(10,MSEDYR) / QSHYTR(9,MSEDYR), (QHYTR(9,I),i=msedyr,lastyr)!102 Liquid Hydrogen - Transportation
       if(QSUREL(9,MSEDYR).gt.0.) write(pacific,925) 'URel', QSUREL(10,MSEDYR) / QSUREL(9,MSEDYR), (QUREL(9,I),i=msedyr,lastyr)!103 Uranium - Electricity
       if(QSURHM(9,MSEDYR).gt.0.) write(pacific,925) 'URhm', QSURHM(10,MSEDYR) / QSURHM(9,MSEDYR), (QURHM(9,I),i=msedyr,lastyr)!104 Uranium - Hydrogen
       if(QSHOIN(9,MSEDYR).gt.0.) write(pacific,925) 'HOin', QSHOIN(10,MSEDYR) / QSHOIN(9,MSEDYR), (QHOIN(9,I),i=msedyr,lastyr)!105 Hydropower - Industrial
       if(QSHOEL(9,MSEDYR).gt.0.) write(pacific,925) 'HOel', QSHOEL(10,MSEDYR) / QSHOEL(9,MSEDYR), (QHOEL(9,I),i=msedyr,lastyr)!106 Hydropower - Electricity

       if(QSGERS(9,MSEDYR).gt.0.) write(pacific,925) 'GErs', QSGERS(10,MSEDYR) / QSGERS(9,MSEDYR), (QGERS(9,I),i=msedyr,lastyr)!108 Geothermal - Residential
       if(QSGEIN(9,MSEDYR).gt.0.) write(pacific,925) 'GEin', QSGEIN(10,MSEDYR) / QSGEIN(9,MSEDYR), (QGEIN(9,I),i=msedyr,lastyr)!109 Geothermal - Industrial
       if(QSGEEL(9,MSEDYR).gt.0.) write(pacific,925) 'GEel', QSGEEL(10,MSEDYR) / QSGEEL(9,MSEDYR), (QGEEL(9,I),i=msedyr,lastyr)!110 Geothermal - Electricity

       if(QSBMRS(9,MSEDYR).gt.0.) write(pacific,925) 'BMrs', QSBMRS(10,MSEDYR) / QSBMRS(9,MSEDYR), (QBMRS(9,I),i=msedyr,lastyr)!112 Biomass - Residential
       if(QSBMCM(9,MSEDYR).gt.0.) write(pacific,925) 'BMcm', QSBMCM(10,MSEDYR) / QSBMCM(9,MSEDYR), (QBMCM(9,I),i=msedyr,lastyr)!113 Biomass - Commercial
       if(QSBMIN(9,MSEDYR).gt.0.) write(pacific,925) 'BMin', QSBMIN(10,MSEDYR) / QSBMIN(9,MSEDYR), (QBMIN(9,I),i=msedyr,lastyr)!114 Biomass - Industrial
       if(QSBMRF(9,MSEDYR).gt.0.) write(pacific,925) 'BMrf', QSBMRF(10,MSEDYR) / QSBMRF(9,MSEDYR), (QBMRF(9,I),i=msedyr,lastyr)!115 Biomass - Refinery
       if(QSBMEL(9,MSEDYR).gt.0.) write(pacific,925) 'BMel', QSBMEL(10,MSEDYR) / QSBMEL(9,MSEDYR), (QBMEL(9,I),i=msedyr,lastyr)!116 Biomass - Electricity
       if(QSBMSN(9,MSEDYR).gt.0.) write(pacific,925) 'BMsn', QSBMSN(10,MSEDYR) / QSBMSN(9,MSEDYR), (QBMSN(9,I),i=msedyr,lastyr)!117 Biomass - Synthetics
       if(QSBMHM(9,MSEDYR).gt.0.) write(pacific,925) 'BMhm', QSBMHM(10,MSEDYR) / QSBMHM(9,MSEDYR), (QBMHM(9,I),i=msedyr,lastyr)!118 Biomass - Hydrogen

       if(QSMSIN(9,MSEDYR).gt.0.) write(pacific,925) 'MSin', QSMSIN(10,MSEDYR) / QSMSIN(9,MSEDYR), (QMSIN(9,I),i=msedyr,lastyr)!120 Municipal Solid Waste - Industrial
       if(QSMSEL(9,MSEDYR).gt.0.) write(pacific,925) 'MSel', QSMSEL(10,MSEDYR) / QSMSEL(9,MSEDYR), (QMSEL(9,I),i=msedyr,lastyr)!121 Municipal Solid Waste - Electricity

       if(QSSTRS(9,MSEDYR).gt.0.) write(pacific,925) 'STrs', QSSTRS(10,MSEDYR) / QSSTRS(9,MSEDYR), (QSTRS(9,I),i=msedyr,lastyr)!123 Solar Thermal - Residential
       if(QSSTCM(9,MSEDYR).gt.0.) write(pacific,925) 'STcm', QSSTCM(10,MSEDYR) / QSSTCM(9,MSEDYR), (QSTCM(9,I),i=msedyr,lastyr)!124 Solar Thermal - Commercial
       if(QSSTIN(9,MSEDYR).gt.0.) write(pacific,925) 'STin', QSSTIN(10,MSEDYR) / QSSTIN(9,MSEDYR), (QSTIN(9,I),i=msedyr,lastyr)!125 Solar Thermal - Industrial
       if(QSSTEL(9,MSEDYR).gt.0.) write(pacific,925) 'STel', QSSTEL(10,MSEDYR) / QSSTEL(9,MSEDYR), (QSTEL(9,I),i=msedyr,lastyr)!126 Solar Thermal - Electricity

       if(QSPVRS(9,MSEDYR).gt.0.) write(pacific,925) 'PVrs', QSPVRS(10,MSEDYR) / QSPVRS(9,MSEDYR), (QPVRS(9,I),i=msedyr,lastyr)!128 Photovoltaic - Residential
       if(QSPVCM(9,MSEDYR).gt.0.) write(pacific,925) 'PVcm', QSPVCM(10,MSEDYR) / QSPVCM(9,MSEDYR), (QPVCM(9,I),i=msedyr,lastyr)!129 Photovoltaic - Commercial
       if(QSPVIN(9,MSEDYR).gt.0.) write(pacific,925) 'PVin', QSPVIN(10,MSEDYR) / QSPVIN(9,MSEDYR), (QPVIN(9,I),i=msedyr,lastyr)!130 Photovoltaic - Industrial
       if(QSPVEL(9,MSEDYR).gt.0.) write(pacific,925) 'PVel', QSPVEL(10,MSEDYR) / QSPVEL(9,MSEDYR), (QPVEL(9,I),i=msedyr,lastyr)!131 Photovoltaic - Electricity

       if(QSWIIN(9,MSEDYR).gt.0.) write(pacific,925) 'WIin', QSWIIN(10,MSEDYR) / QSWIIN(9,MSEDYR), (QWIIN(9,I),i=msedyr,lastyr)!133 Wind - Industrial
       if(QSWIEL(9,MSEDYR).gt.0.) write(pacific,925) 'WIel', QSWIEL(10,MSEDYR) / QSWIEL(9,MSEDYR), (QWIEL(9,I),i=msedyr,lastyr)!134 Wind - Electricity

       if(QSEIEL(9,MSEDYR).gt.0.) write(pacific,925) 'EIel', QSEIEL(10,MSEDYR) / QSEIEL(9,MSEDYR), (QEIEL(9,I),i=msedyr,lastyr)!144 Net Electricity Imports - Electricity
       if(QSCIIN(9,MSEDYR).gt.0.) write(pacific,925) 'CIIN', QSCIIN(10,MSEDYR) / QSCIIN(9,MSEDYR), (QCIIN(9,I),i=msedyr,lastyr)!145 Net Coal Coke Imports - Industrial


       pacific=file_mgr('C','PACIFIC           ',new)
     endif



! check AB32 switch.  If on, apply prices and calculate combustion emissions for fuel sold by fuel producers to
! entities not otherwise explicitly covered.  For petroleum pricing, price adjustments will be added in LFMM so are omitted here by
! commenting out the relevant lines with "!!!!"

   AB32SW=rtovalue('AB32SW  ',1)

   if(curcalyr.ge.2015) then
    if(AB32SW.eq.1) then

     if(ab_allow_p(curiyr) .gt. 0.0) then

!      add ab32 carbon value to california's prices beginning 2015 for resid, commerc, and trans sectors.
!      re-weight Division 9 price using last year of SEDS data
       ANGRS(9,CURIYR)=AB32P9(ANGRS(9,CURIYR),QSNGRS(9,MSEDYR),QSNGRS(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGRS(CURIYR))
       ADSRS(9,CURIYR)=AB32P9(ADSRS(9,CURIYR),QSDSRS(9,MSEDYR),QSDSRS(10,MSEDYR),AB_ALLOW_P(CURIYR),EDSRS(CURIYR))
       ALGRS(9,CURIYR)=AB32P9(ALGRS(9,CURIYR),QSLGRS(9,MSEDYR),QSLGRS(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRRS(CURIYR))
       APRRS(9,CURIYR)=AB32P9(APRRS(9,CURIYR),QSLGRS(9,MSEDYR),QSLGRS(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRRS(CURIYR))
       AKSRS(9,CURIYR)=AB32P9(AKSRS(9,CURIYR),QSKSRS(9,MSEDYR),QSKSRS(10,MSEDYR),AB_ALLOW_P(CURIYR),EKSRS(CURIYR))

       ANGCM(9,CURIYR)=AB32P9(ANGCM(9,CURIYR),QSNGCM(9,MSEDYR),QSNGCM(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGCM(CURIYR))
       ADSCM(9,CURIYR)=AB32P9(ADSCM(9,CURIYR),QSDSCM(9,MSEDYR),QSDSCM(10,MSEDYR),AB_ALLOW_P(CURIYR),EDSCM(CURIYR))
       ALGCM(9,CURIYR)=AB32P9(ALGCM(9,CURIYR),QSLGCM(9,MSEDYR),QSLGCM(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRCM(CURIYR))
       APRCM(9,CURIYR)=AB32P9(APRCM(9,CURIYR),QSLGCM(9,MSEDYR),QSLGCM(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRCM(CURIYR))
       AMGCM(9,CURIYR)=AB32P9(AMGCM(9,CURIYR),QSMGCM(9,MSEDYR),QSMGCM(10,MSEDYR),AB_ALLOW_P(CURIYR),EMGCM(CURIYR))
       AKSCM(9,CURIYR)=AB32P9(AKSCM(9,CURIYR),QSKSCM(9,MSEDYR),QSKSCM(10,MSEDYR),AB_ALLOW_P(CURIYR),EKSCM(CURIYR))

! for natural gas use by trains (1=freight; 2=intercity; 3=transit; 4=commuter)
!                     and boats (1=domestic shipping; 2=international shipping; 3=recreational)
! the CO2 calculation assumes all of this is covered, despite some being international shipping, so to be consistent with that,
! we are applying the allowance price adder to each price.  this makes ANGTR the same, rather than have to be recalculated
! based on all the pieces, whew.
       AGFTRFV(9,CURIYR)=AB32P9(AGFTRFV(9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRPV(9,CURIYR)=AB32P9(AGFTRPV(9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRFV(9,CURIYR)=AB32P9(AGLTRFV(9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRPV(9,CURIYR)=AB32P9(AGLTRPV(9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRRAIL(1,9,CURIYR)=AB32P9(AGFTRRAIL(1,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRRAIL(2,9,CURIYR)=AB32P9(AGFTRRAIL(2,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRRAIL(3,9,CURIYR)=AB32P9(AGFTRRAIL(3,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRRAIL(4,9,CURIYR)=AB32P9(AGFTRRAIL(4,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRRAIL(1,9,CURIYR)=AB32P9(AGLTRRAIL(1,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRRAIL(2,9,CURIYR)=AB32P9(AGLTRRAIL(2,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRRAIL(3,9,CURIYR)=AB32P9(AGLTRRAIL(3,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRRAIL(4,9,CURIYR)=AB32P9(AGLTRRAIL(4,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRSHIP(1,9,CURIYR)=AB32P9(AGFTRSHIP(1,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRSHIP(2,9,CURIYR)=AB32P9(AGFTRSHIP(2,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRSHIP(3,9,CURIYR)=AB32P9(AGFTRSHIP(3,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRSHIP(1,9,CURIYR)=AB32P9(AGLTRSHIP(1,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRSHIP(2,9,CURIYR)=AB32P9(AGLTRSHIP(2,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRSHIP(3,9,CURIYR)=AB32P9(AGLTRSHIP(3,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       ANGTR(9,CURIYR)=AB32P9(ANGTR(9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       ALGTR(9,CURIYR)=AB32P9(ALGTR(9,CURIYR),QSLGTR(9,MSEDYR),QSLGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRTR(CURIYR))
       APRTR(9,CURIYR)=AB32P9(APRTR(9,CURIYR),QSLGTR(9,MSEDYR),QSLGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRTR(CURIYR))

       emfac_qmgtr=emgtr(curiyr)
       emfac_qdstr=edstr(curiyr)
       if(qmgtr(9,curiyr).gt.0.) emfac_qmgtr=(em_tran(1,9,curcalyr)+em_tran(3,9,curcalyr))/qmgtr(9,curiyr)
       if(qmgtr(9,curiyr).gt.0.) emfac_qettr= em_tran(2,9,curcalyr)/qmgtr(9,curiyr)
       if(qdstr(9,curiyr).gt.0.) emfac_qdstr=(em_tran(6,9,curcalyr)+em_tran(7,9,curcalyr))/qdstr(9,curiyr)
       emfac_qettr=emgtr(curiyr) * (TRGNE85*CFRBOB(J)) / (TRGNE85*CFRBOB(J)+ETHNE85*CFETQ(J))


       AMGTR(9,CURIYR)=AB32P9(AMGTR(9,CURIYR),QSMGTR(9,MSEDYR),QSMGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),emfac_qmgtr)
       AETTR(9,CURIYR)=AB32P9(AETTR(9,CURIYR),QSMGTR(9,MSEDYR),QSMGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),emfac_qettr)
       ADSTR(9,CURIYR)=AB32P9(ADSTR(9,CURIYR),QSDSTR(9,MSEDYR),QSDSTR(10,MSEDYR),AB_ALLOW_P(CURIYR),emfac_qdstr)
       AJFTR(9,CURIYR)=AB32P9(AJFTR(9,CURIYR),QSJFTR(9,MSEDYR),QSJFTR(10,MSEDYR)*AB32JETCOVER(CURIYR),AB_ALLOW_P(CURIYR),EJFTR(CURIYR))

       ADSTRHWY(9,CURIYR)=AB32P9(ADSTRHWY(9,CURIYR),QSDSTR(9,MSEDYR),QSDSTR(10,MSEDYR),AB_ALLOW_P(CURIYR),EDSTR(CURIYR))
       AGFTRPV (9,CURIYR)=AB32P9(AGFTRPV (9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
     endif
    endif


! calculate california share of emissions covered by fuel providers for non-covered entities
     ab_covd_em_fue(curiyr)=0.
     ab_covd_em_fue(curiyr)=ab_covd_em_fue(curiyr)+ sum(em_resd(1:5,10,curcalyr))

     ab_covd_em_fue(curiyr)=ab_covd_em_fue(curiyr)+ sum(em_comm(1:7,10,curcalyr))

     ab_covd_em_fue(curiyr)=ab_covd_em_fue(curiyr)+sum(em_tran(1:11,10,curcalyr))

     ab_covd_em_fue(curiyr)=ab_covd_em_fue(curiyr)*.001

! For industrial, we are essentially adding gas and oil CO2 emissions that are not explicitly covered.  So this amounts to total industrial
! emissions minus the covered california emissions as calculated by industrial and refining.
! So first calculate total industrial CO2 emissions for California

     cal_ind_emis=0.
     cal_ind_emis=cal_ind_emis+ sum(em_indy( 1:16,10,curcalyr))

     write(6,'(a,i5,i3,4F12.3)') 'curcalyr,curitr,ab_covd_em_fue/ind/ref,cal_ind_emis: ', &
     curcalyr,curitr, &
     ab_covd_em_fue(curiyr), &
     ab_covd_em_ind(curiyr), &
     ab_covd_em_ref(curiyr), &
     cal_ind_emis*.001
! subtract out emissions from narrow scope covered entities to get broad scope fuel emissions
     cal_ind_emis=cal_ind_emis*.001 - ab_covd_em_ind(curiyr) - ab_covd_em_ref(curiyr)
     if(cal_ind_emis.lt.0) then
       cal_ind_emis=0.
     endif

     ab_covd_em_fue(curiyr)=ab_covd_em_fue(curiyr)+cal_ind_emis

   endif

! calculate the total covered emissions
     ab_covd_em_tot(curiyr)=ab_covd_em_ele(curiyr)+ &
                            ab_covd_em_ref(curiyr)+ &
                            ab_covd_em_ind(curiyr)+ &
                            ab_covd_em_fue(curiyr)+ &
                            ab_covd_em_oth(curiyr)

   RETURN
   END
   FUNCTION AB32P9(Pdiv9,Qsdiv9,Qsca,allow_p,emfact)
! function to increment Division 9 (Pacific) price after adding allowance price to CA price
   implicit none
   real AB32P9 ! function to increment Division 9 (Pacific) price after adding allowance price to CA price
   real Pdiv9  ! Division 9 price
   real Qsdiv9 ! SEDS history consumption for Div 9
   real Qsca   ! SEDS history consumption for california
   real allow_p ! allowance price in 87$/kg-C
   real emfact  ! emission factor kgC/mmbtu

   real Pca     ! california price after adding allowance cost

   Pca = Pdiv9 + (allow_p*emfact)

   AB32P9= Pdiv9  ! default value if quantity zero

   if(Qsdiv9.gt.0.) then
     AB32P9= (Pdiv9*(Qsdiv9-Qsca) + Pca*Qsca) / Qsdiv9
   endif

   return
   end

!============================================================================
! Program to invoke RESD and COMM in separate process to reduce memory
!
  subroutine M_Launch(modstr,putvars,getvars,FYearSubset,FYearStart,FYearEnd,iret)
  use ifport, only : timef,sleepqq
  implicit none
  include 'parametr'
  include 'ncntrl'
  character*6 modstr
  character*(*) putvars,getvars
  integer FYearSubset, FYearStart, FYearEnd,iret
  integer tunit
  character*120 fname,filen,filen2,msg
  integer file_mgr
  external file_mgr
  logical new,lexist
  character*5 tsignal
  real time_beg,time_limit/360./
! declaration for subroutine filer' arguments
  integer funiti,funito,fretcd,funfmt,frtype,fsourc
  character*100 fnamei,fnameo
  integer ntries
  integer iretcopy
  character*50 cmd
   iret=0





     write(6,*) 'm_launch, modstr=',modstr,' putvars='//trim(putvars),', getvars='//trim(getvars)//', FYearSubset=',FYearSubset,FYearSTart,FYearEnd

! Store global data variables listed in "putvars" in unformatted file (nemsout.unf) using FILER.
!   open putvars with file_mgr
        NEW=.false.
        FUNITI = FILE_MGR('O',putvars,NEW)
        inquire(unit=funiti,name=filen2)  ! full name of putvars file
        FNAMEI=' '                ! blank since the unit is opened
        FSOURC=1                  ! variables from file
        funito=11
        FNAMEO='nemsout.unf'
        FUNFMT=1                  ! Unformatted
        FRTYPE=1                  ! output request

        CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

        funiti=file_mgr('C',putvars,NEW)


! get the name of the "getvars" file by opening in filemgr, then doing an inquire to get the file name in filen
        if(modstr.ne.'TFILER') then
          NEW=.false.
          FUNITI = FILE_MGR('O',getvars,NEW)
          inquire(unit=funiti,name=filen)
          funiti=file_mgr('C',getvars,NEW)
        endif

! create data transfer file (tfiler.files)
        FNAME='TFILER'
        new=.true.
        tunit=file_mgr('O',FNAME,new)  ! open file and get unit number tunit

        if(modstr.eq.'RESD  ' .or. modstr.eq.'COMM  ' .or. modstr.eq.'BUILD ') then
          time_limit=360   !  seconds to run resd and comm.  On last year it takes longer, for comm especially.
          write(tunit,'(a)') 'Dictionary input/dict.txt'
          write(tunit,'(a)') 'Restarti   nemsout.unf'        ! the file written by filer above will be the input restart file to tfiler
          write(tunit,'(a)') 'In Format  1  Unformatted'
          write(tunit,'(a)') 'Varlist    '//trim(filen)       ! getvars file name
          write(tunit,'(a)') 'Restarto   nemsin.unf'         ! the file written by tfiler will be read input by filer below.
          write(tunit,'(a)') 'Out Format 1'
          write(tunit,'(a)') 'Filer Obj  na'
          write(tunit,'(a)') 'date-time-na'
          write(tunit,'(a,i1,a,i2,a,i2,a)') '===FYearSubset=',FYearSubset,'==FYearStart=',FYearStart,'==FYearEnd=',FYearEnd,'==='
        else
          write(6,*) 'error: in m_launch, modstr not recognized. modstr=',trim(modstr)
        endif
        tunit=file_mgr('C',fname,new) ! close unit
!
!    delete any preexisting nemsin.unf
        inquire(file='nemsin.unf',exist=lexist)
        if(lexist) then
          open(11, file='nemsin.unf')
          close(11,status='DELETE')
        endif


! Send signal to tfiler via "nems.signal"
     ntries=0
     do while(ntries.lt.25)
        ntries=ntries+1
        open(11,file='nems.signal',status='unknown',SHARE='denynone',SHARED,ACTION='readwrite',err=56)
        write(11,'(a)',err=55) trim(modstr)
        ntries=26
55      continue
        close(11)
56      continue
        if(ntries.le.25) write(6,*)'Unable to write to nems.signal, ntries=',ntries
     enddo
     tsignal=modstr
     time_beg=timef()
     do while(timef()-time_beg .lt. time_limit  .and. tsignal .ne. 'NEMS' .and. tsignal .ne. 'QUIT')
       call sleepqq(100)  ! sleep for 100 miliseconds
       inquire(file='nems.signal',exist=lexist)
       if(lexist) then
         open(11,file='nems.signal',status='old',readonly,err=5)
         read(11,'(a)',err=3,end=3) tsignal
  3      continue
         close(11,err=5)
  5      continue
       else
         tsignal='QUIT'
       endif
     enddo
     if(tsignal.ne.'NEMS') then
       if(timef()-time_beg .gt. time_limit ) then
         write(6,*)' in m_launch, time_limit exceeded waiting for nems.signal. time_limit(seconds)=',time_limit
       endif
! wait time exceeded or someone entered QUIT into the signal file.
! turn model off.
       iret=1
       return
     endif

     FNAMEI='nemsin.unf'
     FUNFMT=1

! read nemsin.unf back in
    inquire(file=fnamei,exist=lexist)
    if(lexist) then
      FUNITI=11
      FNAMEO=' '
      FRTYPE=2  ! input request

      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      if(modstr.eq.'TFILER'.and.prtdbgc.ge.1) then
        funiti=file_mgr('C',getvars,NEW)
        close(11)
        cmd=' '
        write(cmd,'(a,i4,a)') 'copy nemsin.gdx coalin',curcalyr,'.gdx'
        call callsys(iretcopy,cmd)
      endif
    else
      write(6,*) ' in m_launch, interface file nemsin.unf not found so model did not run to completion. '
      iret=1
      return
    endif
    write(6,*) 'm_launch...returning normally'
   return
   end

!============================================================================
! job control routines.  see standalone programs jobinit.f, runit.f, and
! jobcontrol.f in scripts for controlling jobs through these routines.
!============================================================================
module JD_
   use IFWIN
   ! data structure for job control record

   type jobcontrol
     integer      JobNumber ! unique jobnumber.
     integer      RecNum     ! record on which job is stored in jobinfo.daf
     character*15 Scenario
     character*9  Datecode
     character*80 OutputDir
     character*3  UserID
     character*8  PCID      ! PC Launching Job
     character*8  Hostname  ! Preferred Hostname on Job Submission
     character*5  QueueType ! Queue type (big or small) on the PCID runnning the job
     character*4  QueueID   ! Queue ID of Queue running the job
     character*8  partition ! name of partition for parallel job
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
   character*50 NEMSDIR
   character*80 outdir
   character*60 errtext
   Integer J_file_unit/500/
   integer iret

   integer(HANDLE) P_Handle
   integer suspendtime     ! number of seconds job is being suspended for


   integer(4) icsec,iwsec ! for calling mptim3 for cpu, wall seconds
end module JD_



 subroutine Job(call_type)
   use JD_
   use dflib
   use dfport
   use dfwin
   implicit none

   integer call_type
   logical j_recent
   external j_recent
   character*5 queue,QueID*4
   common/q/queue,queid

    NEMSDIR=' '
    call GETENV("NEMSJOBLOG",NEMSDIR) ! new location for NEMS runlog and jobqueue.daf in multi-platform/vintage set up.
    if(NEMSDIR.eq.' ') then
      call GETENV("NEMS",NEMSDIR)   ! try using old, platform-specific location
      if (NEMSDIR .ne. ' ') then
        NEMSDIR=trim(NEMSDIR)//'/logs/'
      endif
    else
      NEMSDIR=trim(nemsdir) //  "/"   ! add slash to end
    endif
    if(NEMSDIR.eq.' ') then
      NEMSDIR='m:/NEMSJobLog/'
    endif
    lexist=.false.
!   inquire(file=NEMSDIR,exist=lexist)   ! compaq visual fortran
    inquire(directory=NEMSDIR,exist=lexist) ! intel fortran
    if(.not. lexist) then
      NEMSDIR=' '  ! new, dummy jobqueue.daf will be put in local output directory
    endif

    J_File=trim(NEMSDIR)//'jobqueue.daf'

    if(call_type.eq.0) then   ! initial call
      call J_Open     ! opens the file
      call J_Init     ! find existing job record or creates a new job record
      call J_Close    ! closes the file
      j%status='Running'
      call j_Display
      call J_Update   ! opens file, Reads and updates the job record with current status, closes file
    elseif(call_type.eq. 1 .or. call_type.eq.2) then
      call J_Update
      if(call_type.eq.2) call J_Display
    endif
    return
!
! the following subroutines "contain"ed in this one share the declarations
!  defined here

    CONTAINS
    Subroutine J_Open


      J_reclen=260

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
    subroutine J_Init
      integer i,isys/501/,endpos
      TYPE (jobcontrol) :: T

      CHARACTER($MAXPATH) dir
      character*2 DefDrive,c2,c3*3,c8*8
      INTEGER(4) ldir
      logical lexist, found, DoneOnce
      integer(4) highest_job, ios
      character*25 field,line*150
      integer pattern
      ios=0


      !  Get current drive and directory
      Dir  = FILE$CURDRIVE      ! get default drive
      ldir = GETDRIVEDIRQQ(Dir) ! on input, Dir is just Drive; on output Dir is drive/path


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

      found=.false.
      ios=0
      outdir=trim(dir)
      call mlower(dir)
      call mlower(j%scenario)
      do i=1,1000
        read(j_file_unit,rec=i,err=89,iostat=ios) T ! first nonexistant record will trigger error branch
        highest_job=max(T%JobNumber,Highest_job)
        call mlower(T%scenario)
        if(T%Scenario.eq.J%scenario .and. T%Datecode.eq.J%Datecode .and. T%Partition.eq.J%Partition) then
           found=.true.
           J = T  ! initialize Job record from file
           j_record_number=i
           outdir=j%outputdir  ! save original outputdir in case this run uses a temp working directory
           call mlower(outdir)
           exit ! breakout of do loop
        endif
      enddo
 89   continue
      if(ios.ne.0.and.ios.ne.36) then
        write(6,*) 'job record read error, iostat=',ios
      endif


! check for use of temporary working directory.
      call getshare(outdir,2)
      if(outdir.ne.dir) then
        call getshare(dir,1)
        j%OutputDir='for now: '//trim(Dir)
      else
        call getshare(dir,1)
        J%OutputDir = trim(Dir)  ! during the run, the job log will point to working directory even if diff from orig
      endif
      call getshare(outdir,1)

      queue=j%QueueType
      queid=j%QueueID
      if(ichar(queue(1:1)).eq.0) queue=' '
      if(ichar(queid(1:1)).eq.0) queid=' '
      j%curiyr=0
      j%curitr=0
      j%imodel=0
      ! the following two environment strings are defined if nems is submitted
      ! in batch mode via the cycle.sh script and enable multiple runs in the
      ! same directory, recycling the restart file each time

      call getenv("nruns",c2)
      j%nruns=c2
      call getenv("n",c2)
      j%irun=c2

      !   since nruns would only be defined normally if NEMS is run in batch, the
      !   we can use this as an indication that this job can be a low priority batch
      !   mode job.  Jobs run through the debugger or by direct invocation of
      !   nems at the command line would be run at normal priority.

! set priority to normal if run batch.  this way it doesn't interfere with other things
      J%Priority='high'      ! job priority (high, normal)
      if(j%nruns.ne.' ') then     ! must be batch mode.  switch to low priority
        j%priority='normal'
        P_Handle=GetCurrentProcess() ! pseudohandle
        iret=SetPriorityClass(P_Handle,IDLE_PRIORITY_CLASS)
        if(iret.eq.0) then
          write(6,*) ' setting process priority failed'
        endif
      else
        j%class='debug'  ! default -- nems executed from command line such as in debug mode without job class argument
        j%queuetype='debug'
        j%queueid='0000'
        j%nruns='1'
        j%irun='1'
      endif
      call date_and_time(J%StartDate,J%StartTime)
      call MPTIM3(icsec,iwsec)
      J%CPU_Seconds= real(icsec)/100.
      J%WALL_Seconds=real(iwsec)/100.
      J%Status = 'Running'
      call date_and_time(TIME=J%StatusTime) !time of last status update
      J%Stat_Interval=5 ! status update interval  in seconds
      J%NapTime = 0          ! not used now

! If no pre-existing job record, initialize other required fields in job record

      if(.not. found) then  !
        j%class='debug'  ! default -- nems executed from command line such as in debug mode without job class argument
        j%PCID=' '
        j%hostname=' '
        j%nruns='1'
        j%irun='1'
        call getenv("COMPUTERNAME",c8)
        j%pcid=c8
        if(j%PCID.eq.' ') then

! hostname requires that the MKS/MKSNT is in the path
          iret = system('hostname > hostname.out')
          inquire(exist=lexist,file='hostname.out')
          if(lexist) then
            open(isys,file='hostname.out',status='old',err=22)
            read(isys,'(a)',end=24) j%pcid
24          close(isys,status='delete')
22          continue
          endif
        endif
        call mlower(j%PCID)
        j%hostname=j%PCID
        j%queuetype='debug'
        j%queueid='0000'

        call getenv("USER",c3)
! get user id of submitter from launched.from if it exists.
        inquire(file='launched.from',exist=lexist)
        if(lexist) then
           open(isys,file='launched.from',status='old')
           read(isys,'(a)') line
           close(isys)
           i=index(line,' by ')+4
           c3=line(i:i+2)
        else
           inquire(file='..\launched.from',exist=lexist)
           if(lexist) then
              open(isys,file='..\launched.from',status='old')
             read(isys,'(a)') line
             close(isys)
             i=index(line,' by ')+4
             c3=line(i:i+2)
           endif
        endif

        j%UserID=c3
        if(J%UserID.eq.' ') then
          iret = system('who > who.out')
          inquire(exist=lexist,file='who.out')
          if(lexist) then
            open(isys,file='who.out',status='old',err=26)
            read(isys,'(a)',end=28) j%UserId
26          close(isys,status='delete')
28          continue
          endif
        endif
        call mlower(j%UserID)

        J%Command=' ' ! job command: set priority for example)

!  determine last record in direct access file or any record that can be reused.
!  reusable records have a status of "complete" or "MIA"

        highest_job=0
        J_record_number=0
        doneonce=.false.
        do i=1,1000
          read(j_file_unit,rec=i,err=99,iostat=ios) T ! first nonexistant record will trigger error branch
          highest_job=max(T%JobNumber,Highest_job)
          if(.not.doneonce) then
            if(T%Status.eq.'Complete' .or. T%Status.eq.'complete' .or. &
              T%Status.eq.'HaHaHa' .or. &
              t%jobnumber.lt.1 .or. &
              T%Status.eq.' ') then
              if(.not.j_recent(T%statustime, T%startdate,1200)) then
                j_record_number=i
                doneonce=.true.
              endif
            elseif(t%Status.eq.'MIA') then
              if(.not.j_recent(T%statustime, T%startdate,14000)) then
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


! pre-existing or new job record can now be written out
      ios=0
      write(j_file_unit,rec=J_record_number,err=999,iostat=ios) J ! writes the job information record out
999   continue
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
    subroutine J_Update
      character*20 oldcommand
      character*10 oldstatus
      logical update, repeat

      repeat=.true.
      suspendtime=0
      do while (repeat)
        oldcommand=J%command
        oldstatus=j%status
        update=.true.
        if(call_type.eq.1) then ! see if sufficient time has passed to warrent stat update
          call j_checkstat(j%StatusTime,J%Stat_Interval,update)
        endif
        if(update) then
          call J_Open
          read(J_file_unit,rec=J_record_number,err=999) J ! reads the job information record
          ! see if job command changed.  if so, process command
          if(oldcommand.ne.J%Command) then
             call J_Process_Command
          else
             call J_Sched(J%Stat_Interval) ! invokes alarm to call J_update in j%stat_interval seconds

          endif

          ! update status variables

          call date_and_time(TIME=J%StatusTime) !time of last status update
          call MPTIM3(icsec,iwsec)
          J%CPU_Seconds= real(icsec)/100.
          J%WALL_Seconds=real(iwsec)/100.
          call jobdata(J%curiyr,j%curitr,J%imodel)

          if(call_type.eq.1.and.suspendtime.le.0) then
            J%status = 'Running'
          elseif(call_type.eq.2) then
            if(outdir.ne.j%outputdir) j%outputdir=outdir
            inquire(exist=lexist,file='runit.txt')  ! file created by runit.exe. if there, don't need to update status
            if(j%irun.eq.j%nruns.and. .not.lexist) then
              J%status = 'Complete'
            endif
          else
            j%status=oldstatus  ! eg. was on hold in log and now is running
          endif
          write(j_file_unit,rec=J_record_number,err=999) J ! writes the job information record out
          999 continue
          call J_Close
! for a suspension, sleep one minute at a time until suspension has been served
          if(suspendtime.gt.0) then
            call sleepqq( 1000*min(60,suspendtime) )
            suspendtime=suspendtime-60
            if(suspendtime.lt.0 ) suspendtime=0
          endif
        endif
        if(suspendtime.gt.0) then
          repeat=.true.
        else
          repeat=.false.
        endif
      enddo
    end subroutine J_Update
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
!=======================================================================
    subroutine J_Process_Command
      integer k

      if(index(J%Command,        'kill'         )) then

        write(6,*) 'stopping at direction of job control'
        j%Status='Complete'
        j%Command=' '
        write(j_file_unit,rec=J_record_number,err=990) J ! writes the job information record out
990     continue
        call J_Close
        call mnclose
        stop 111

      elseif(index(J%Command,    'high'       )) then

        J%command=' '
        J%Priority='high'
        J%NapTime=0
        write(j_file_unit,rec=J_record_number,err=992) J ! writes the job information record out
992      continue
        P_Handle=GetCurrentProcess() ! pseudohandle
        iret=SetPriorityClass(P_Handle,NORMAL_PRIORITY_CLASS) ! we call normal "high"
        if(iret.eq.0) then
          write(6,*) ' setting process priority failed'
        endif

      elseif(index(J%Command,    'normal'         )) then

        J%command=' '
        J%Priority='normal'
        write(j_file_unit,rec=J_record_number,err=993) J ! writes the job information record out
993     continue
        P_Handle=GetCurrentProcess() ! pseudohandle
        iret=SetPriorityClass(P_Handle,IDLE_PRIORITY_CLASS)   ! we call idle_priority "normal"
        if(iret.eq.0) then
          write(6,*) ' setting process priority failed'
        endif

      elseif(index(J%Command,      'suspend'       )) then

        k=index(J%command,'suspend')
        j%command(k:k+6)=' '
        suspendtime=7200
        read(J%Command,*,err=99,end=99) suspendtime ! seconds
        j%status='suspended'
        j%command=' '
        write(j_file_unit,rec=J_record_number,err=999) J ! writes the job information record out
999     continue
        call j_display
        write(m6,*) ' =============================================='
        write(m6,*) ' Job suspended, number of seconds: ',suspendtime
        write(m6,*) ' To cancel suspension, reissue job control command'
        write(m6,*) ' =============================================='


99      continue
      endif

      call J_Sched(J%Stat_Interval)

      return

    end subroutine J_Process_Command
!=======================================================================
    subroutine J_Sched(stat_interval)
! invokes the alarm timer to call the job routine every "stat_interval" seconds
     use dfport
     implicit none
     integer stat_interval
     integer iret

!     external calljob
   ! iret = alarm(stat_interval,calljob)  ! couldn't get this to work reliably

     if(J%NapTime.gt.0) then
       call sleepqq(J%NapTime*1000) ! sleep for time specified in suspend command
     endif

     return
   end subroutine J_Sched
!========================================================================================
   subroutine J_CheckStat(StatTime,delay,update)
 ! see if last status time was longer than acceptable delay. if so, update in order

     logical update
     character*8 curdat
     character*10 curtim,StatTime
     integer hr,min,sec,vals(8)
     integer isec
     integer csec
     integer delay
     update=.false.


     call date_and_time(DATE=curdat,VALUES=vals)

     hr=vals(5)
     min=vals(6)
     sec=vals(7)
     csec=hr*3600+min*60+sec  ! number of seconds passed in current day
     read(StatTime,'(i2,i2,i2)',err=99) hr,min,sec
     isec=hr*3600+min*60+sec

     if(abs(csec-isec).gt. delay.and.csec.gt.delay) then  ! if status hasn't been updated recently
       update=.true.               ! set update to true
     end if

99   continue
     return
   end subroutine J_CheckStat
end subroutine job
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

!====================================================================
!    end of subroutine job which CONTAINs someother subroutines
!====================================================================

 subroutine jobdata(j_curiyr,J_curitr,j_imodel)
!pass data to job subroutines
   include 'parametr'
   include 'ncntrl'
   integer(4) j_curiyr,j_curitr,j_imodel
   common/passmodel/modelno
   integer modelno
   if(baseyr.gt.0) then  ! before options have been read, baseyr is 0
     j_curiyr=baseyr+curiyr-1
     j_curitr=curitr
     j_imodel=modelno
   else
     j_curiyr=0
     j_curitr=0
     j_imodel=0
   endif
   return
end

 subroutine getshare(filen,option)
!  tries to convert between DRIVE:PATH name to UNC naming based
! on mapped network drives in use and network shares assigned.
!  OPTION=1 tries to convert to UNC (//node/...)
!  OPTION=2 tries to convert to DRIVE:PATH
!
!
      use dflib
      use dfport
      implicit none
      character*(*) filen
      character*100 cmd,line*200,H*70,dum*80
      character*3 drive,ext*3,fname*35
      character*8 pcid
      integer iret,imatch,lmatch,lp,lpf,lc,ls
      character*31 shares(100),shpath(100)*31
          integer nshares,i
      character*31 shrname
      character*4 drives(26)
      integer(4) istat, option
      character*100 cfile
      logical lexist

      character*5 queue,QueID*4
      common/q/queue,queid

      call Mlower(filen)
      call Mreplace(filen,'\','/')

      IRET = HOSTNAM (pcid) ! portability routine
      !write(*,*) 'HOSTNAM:',pcid
      call Mlower(pcid)

      if(len_trim(queue).eq.0.or.ichar(queue(1:1)).eq.0) queue='small'
      if(len_trim(queid).eq.0.or.ichar(queid(1:1)).eq.0) queid='dum0'
      cfile=trim(queue)//'.'//trim(queid)//'.out'


! run "net use" command to generate
! a list of network drives

      cmd='net use > '//trim(cfile)
      iret=SYSTEM(cmd)

      open(8,file=cfile,status='old',err=92)

      nshares=1
      do while (.not. eof(8))
         read(8,'(a)',err=97) line
         lc=index(line,':')
         if(lc.gt.0) then
           if(lc.gt.2) line=line(lc-1:)
           read(line,'(a9,1x,a24)',err=97) shpath(nshares), shares(nshares)
           if(index(shares(nshares),':').eq.0) then
             call Mlower(shpath(nshares))
             call Mlower(shares(nshares))
             call Mreplace(shares(nshares),'\','/')
             call Mreplace(shares(nshares),')',' ')
             !write(*,'(a,1x,a)') trim(shpath(nshares)),trim(shares(nshares))
             nshares=nshares+1
           endif
         endif
97       continue
      enddo
      close(8,status='delete')

      !write(*,*)' '
      !write(*,*)'List of network shares on '//trim(pcid)

! get list of file shares on this PC
      h='HKEY_LOCAL_MACHINE\SYSTEM\ControlSet001\Services\LanmanServer\Shares'
      cmd='reg query '//trim(h)//' > '//trim(cfile)
      iret=SYSTEM(cmd)
      open(8,file=cfile,status='old',err=92)

      do while (.not. eof(8))
         read(8,'(a)') line
         lc=index(line,'Path=')
         if(lc.gt.0) then

! share name is first field, space delimited
           line=adjustl(line)
           ls=index(line,' ')
           shrname=line(1:ls)

! path is found after string "Path=", delimited by "\0"
           lc=index(line,'Path=')
           line=line(lc+5:)

           lc=index(line,'\0')
           if(lc.gt.0) line(lc:)=' '
           call mlower(line)
           call mreplace(line,'\','/')
           nshares=nshares+1

           shpath(nshares)=trim(line)
           shares(nshares)='//'//trim(pcid)//'/'//trim(shrname)
           !write(*,*) trim(shpath(nshares)),' ',trim(shares(nshares))
         endif
       end do
       close(8,status='delete')
! substitute UNC name for drive letter/path prefix if possible
        if(nshares.ge.1) then
          imatch=0
          lmatch=0
          lpf=len_trim(filen)
          do i=1,nshares

            if(filen(lpf:lpf).eq.'/') then
              lp=len_trim(shpath(i))
              if(shpath(i)(lp:lp).ne.'/') then
                shpath(i)(lp+1:)='/'
              endif
            else
              lp=len_trim(shpath(i))
              if(shpath(i)(lp:lp).eq.'/') then
                shpath(i)(lp:lp)=' '
              endif
            endif

            if(option.eq.1) then
              if(index(filen,trim(shpath(i))).eq.1.and.len_trim(shpath(i)).gt.lmatch) then
                lmatch=len_trim(shpath(i))
                imatch=i
              endif
            else
              if(index(filen,trim(shares(i))).eq.1.and.len_trim(shpath(i)).gt.lmatch) then
                lmatch=len_trim(shpath(i))
                imatch=i
              endif
            endif
          enddo
          if(imatch.ne.0) then
            if(option.eq.1) then
              call Mreplace(filen,trim(shpath(imatch)),trim(shares(imatch)))
              call Mreplace(filen(3:),'//','/')
             else
              call slashslash(shares(imatch),shpath(imatch))
              call Mreplace(filen,trim(shares(imatch)),trim(shpath(imatch)))
              call Mreplace(filen(3:),'//','/')
            endif
          endif
        endif
        return
92    continue
      end subroutine getshare

! ===============================================

      subroutine slashslash(a,b)
      implicit none
      character*(*)a
      character*(*)b
      integer la,lb
      la=len_trim(a)
      lb=len_trim(b)
      if(a(la:la).eq.'/'.and.b(lb:lb).ne.'/') then
        b(lb+1:lb+1)='/'
      elseif(a(la:la).ne.'/'.and.b(lb:lb).eq.'/') then
        a(la+1:la+1)='/'
      endif
      return
      end

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

! EIA Modification: acquire and return the process ID
!
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

! =============================================================================================================================

Subroutine AIMMS_Coal(ireturn)
   use ifport,only:timef
   use globmain
   use CALLING_AIMMS
   implicit none
   integer ireturn ! return code
   logical done_yet/.false./
   character*20 coal_issue(10)  ! describes issues associcated with integer coalcode return code from aimms

! Handles data exchanges and invocation of the AIMMS Coal module.
! Data exchanges of global variables to and from AIMMS are handled by calls to filer routines.


!  Each time through:
!  1) send NEMS input/output global data to AIMMS NGMM project
!  2) Open AIMMS project via the command line
!  3) invoke its MainExecution procedure and wait for it to finish.
!  4) retrieve output global data from AIMMS project
  ireturn=0
  aimms_module=1  ! index to projectHandle to keep coal project separate from natural gas project

  restarto='.\\coal\\coal.aimms'

! first time through:
 if(.not. done_yet) then
     keepopen(aimms_module)=RTOVALUE('KEEPOPEN',0)
     if (keepopen(aimms_module) .eq. 0) keepopen(aimms_module)=RTOVALUE('KEEPCOAL',0)

!  get AIMMS Link foldername
     call Read_AIMMS_Link

     done_yet=.true.

     if(no_AIMMS) then
       write(6,*) 'AIMMS Software not found in expected location so stopping'
       ireturn=221
       return
     endif
   endif

! send input/output global data to AIMMS project and invoke its MainExecution procedure
  if(.not. closed(aimms_module)) then   ! once closed, don't reopen
     restarto='.\\coal\\coal.aimms'
     varlistfile='COALPUTVARS'
     call write_to_AIMMS_txt_cmd
     ireturn=iret
     if (iret .ne. 0) then
         ! call termination procedure and close AIMMS project
         Call End_AIMMS_cmd
         closed(aimms_module)=.true.
       return
     endif
  endif

  coal_issue(1)='Infeasible'
  coal_issue(2)='Unbounded'
  coal_issue(3)='Solver error'
  coal_issue(4)='Other'
  coal_issue(5:10)='No info'
  if(.not. closed(aimms_module)) then  ! once closed, don't reopen
     restarti='.\\coal\\coal.aimms'
     varlistfile='COALGETVARS'  ! no longer used with text data transfer
     call read_from_AIMMS_txt
     if (COALCODE .ne. 0) then
       if(COALCODE.le.10) then
         write(6,'(a,i4,a,I2,a,i2,2a)') 'AIMMS ',curcalyr,'  Iteration',CURITR,'    Coal problem, coalcode=',coalcode,' '//coal_issue(coalcode)
       else
         write(6,'(a,i4,a,I2,a,i2)') 'AIMMS ',curcalyr,'  Iteration',CURITR,'    Coal problem, coalcode=',coalcode
       endif
       IF (CONTINC .EQ. 1) THEN
         CONTINC=0
         write(REASONC,'(A10,2x,I4)') coal_issue(coalcode)(1:10),CURCALYR
       ENDIF
     endif

     ireturn=iret
     if (iret .ne. 0) then
! call termination procedure and close AIMMS project. don't reopen after error.
       Call End_AIMMS_cmd
       closed(aimms_module)=.true.
       aimms_opened(aimms_module)=.false.
     else
! call termination procedure and close AIMMS project unless option to keep aimms open after each invocation is in effect
       if (keepopen(aimms_module) .eq. 0) then
          Call End_AIMMS_cmd
          aimms_opened(aimms_module)=.false.
       endif
     endif
  endif

  if (curiyr.eq.lastyr .and. fcrl.eq.1 .and. aimms_opened(aimms_module) ) then
    ! call termination procedure and close AIMMS project
    Call End_AIMMS_cmd
    closed(aimms_module)=.true.
    aimms_opened(aimms_module)=.false.
  endif

  return

  end subroutine AIMMS_Coal

Subroutine AIMMS_NG(ireturn)
   use ifport,only:timef
   use globmain
   use CALLING_AIMMS
   implicit none
   integer ireturn ! return code
   logical done_yet/.false./

! Handles data exchanges and invocation of the AIMMS NGMM module.
! Data exchanges of global variables to and from AIMMS are handled by calls to filer routines.


!  Each time through:
!  1) send NEMS input/output global data to AIMMS NGMM project
!  2) Open AIMMS project via the command line
!  3) invoke its MainExecution procedure and wait for it to finish.
!  4) retrieve output global data from AIMMS project
  ireturn=0
  aimms_module=2    ! index to indicate the natural gas module

  restarto='.\\ngas\\natgas.aimms'

! first time through:
   if (.not. done_yet) then
     keepopen(aimms_module)=RTOVALUE('KEEPOPEN',0)
     if (keepopen(aimms_module) .eq. 0) keepopen(aimms_module)=RTOVALUE('KEEPNG  ',0)

!  get AIMMS foldername
     call Read_AIMMS_Link

     done_yet=.true.

     if(no_AIMMS) then
       write(6,*) 'AIMMS Software not found in expected location so exiting AIMMS_NG'
       ireturn=221
       return
     endif
   endif

! send input/output global data to AIMMS project and invoke its MainExecution procedure
  if(.not. closed(aimms_module)) then     ! once closed, don't reopen
     restarto='.\\ngas\\natgas.aimms'
     varlistfile='NGPUTVARS'
     call write_to_AIMMS_txt_cmd
     ireturn=iret
     if (iret .ne. 0) then
         ! call termination procedure and close AIMMS project
         Call End_AIMMS_cmd
         closed(aimms_module)=.true.
         return
     endif
  endif

  if(.not. closed(aimms_module)) then      ! once closed, don't invoke or reclose
     restarti='.\\ngas\\natgas.aimms'
     varlistfile='NGGETVARS'   ! no longer used with text data transfer
     call read_from_AIMMS_txt
     if (NGCODE .ne. 0) then
       write(6,'(a,i4,a,i2,2a)') 'AIMMS ',curcalyr,'    NGMM problem, nccode=',ngcode
       CONTING=0
       write(REASONG,'(A10,2x,I4)') 'Infeasible',CURCALYR
     endif

     ireturn=iret
     if (iret .ne. 0) then
! don't reopen after error
       Call End_AIMMS_cmd
       closed(aimms_module)=.true.
       aimms_opened(aimms_module)=.false.
     else
! call termination procedure and close AIMMS project unless option to keep aimms open after each invocation is in effect
       if (keepopen(aimms_module) .eq. 0) then
         Call End_AIMMS_cmd
         aimms_opened(aimms_module)=.false.
       endif
     endif
  endif

  if (curiyr.eq.lastyr .and. NCRL.eq.1 .and. aimms_opened(aimms_module) ) then  ! for coal, never true.  but may want to keep natural gas open until the end
    ! call termination procedure and close AIMMS project
    write(6,'(a,i4,a)') 'AIMMS ',curcalyr,' Closing AIMMS'
    Call End_AIMMS_cmd
    closed(aimms_module)=.true.
    aimms_opened(aimms_module)=.false.
  endif

  return

  end subroutine AIMMS_NG


Subroutine AIMMS_ReStore(ireturn)
   use ifport,only:timef
   use globmain
   use CALLING_AIMMS
   implicit none
   integer ireturn ! return code
   logical done_yet/.false./
   character*20 restore_issue(10)  ! assigns issues to return codes 1 - 10 from aimms
   integer i

! No data exchanges.  These are done in EMM (uecp.f)
! Moving the call here from uecp.f so that it retries AIMMS connection when appropriate

!  Each time through:
!  1) Open AIMMS project via the command line
!  2) invoke its MainExecution procedure and wait for it to finish.
  ireturn=0
  aimms_module=restore  ! index to projectHandle to keep ReStore separate from coal and natural gas

  restarto='.\\rest\\restore.aimms'
  restore_issue(1) ='Rest: Infeasible    '
  restore_issue(2) ='Rest: Unbounded     '
  restore_issue(3) ='Rest: Internal Solve'
  restore_issue(4) ='Rest: Solve UnCalled'
  restore_issue(5) ='Rest: Solver Failure'
  restore_issue(6) ='Rest: Setup Failure '
  restore_issue(7) ='Rest: Preprocessor  '
  restore_issue(8) ='Rest: Post Processor'
  restore_issue(9) ='Rest: No Solution   '
  restore_issue(10)='Rest: Other         '

! first time through (keepopen does not apply to restore):
 if (.not. done_yet) then
     call Read_AIMMS_Link           !  get AIMMS Link foldername
     done_yet=.true.
     if (no_AIMMS) then
       write(6,'(a)') 'AIMMS Software not found in expected location so exiting AIMMS_ReStore'
       ireturn=221
       return
     endif
 endif

! send input/output global data to AIMMS project and invoke its MainExecution procedure
  if (.not. closed(aimms_module)) then   ! once closed, don't reopen
     restarto='.\\rest\\restore.aimms'
     varlistfile=''
     call write_to_AIMMS_txt_cmd
     ireturn=iret
     if (iret .ne. 0) then          ! if fails, call termination procedure and close AIMMS project
         Call End_AIMMS_cmd
         closed(aimms_module)=.true.
         return
     endif
  endif

  if (.not. closed(aimms_module)) then  ! once closed, don't reopen
!    restarti='.\\rest\\restore.aimms'
!    varlistfile=''     ! no longer used with text data transfer.  also, not used with restore.
!    call read_from_AIMMS_txt
!    if (iret .ne. 0) then
!      if(iret .le. 10) then
!        write(6,'(a,i4,a,I2,a,i2,2a)') 'AIMMS ',curcalyr,'  Iteration',CURITR,'    ReStore problem, return code=',iret,' '//restore_issue(iret)
!      else
!        write(6,'(a,i4,a,I2,a,i2)') 'AIMMS ',curcalyr,'  Iteration',CURITR,'   ReStore problem, code=',iret
!      endif
!      CONTINE=0
!      write(REASONE,'(A10,2x,I4)') 'ReStore   ',CURCALYR
!    endif

     ireturn=iret
     if (iret .ne. 0) then
! call termination procedure and close AIMMS project. don't reopen after error.
       Call End_AIMMS_cmd
       closed(aimms_module)=.true.
       aimms_opened(aimms_module)=.false.
     else
! call termination procedure and close AIMMS project
          Call End_AIMMS_cmd
          aimms_opened(aimms_module)=.false.
     endif
  endif

  if (curiyr .eq. lastyr .and. fcrl .eq. 1 .and. aimms_opened(aimms_module)) then
    ! call termination procedure and close AIMMS project
    Call End_AIMMS_cmd
    closed(aimms_module)=.true.
    aimms_opened(aimms_module)=.false.
  endif

  return

  end subroutine AIMMS_ReStore

!===================================================================================================================

Subroutine Read_AIMMS_Link
  use globmain
  use CALLING_AIMMS
  implicit none

character(80) aimms32,aimms64,AIMMSLOC*66

 no_aimms=.false.

 call RTOSTRING('AIMMSLOC',AIMMSLOC) !  e.g., 'C:\AIMMS_Installation_Free_Releases\4.8.1.299-x64'
 aimmsFolder(aimms_module) = trim(AIMMSLOC)//char(0)
 aimms32 = trim(AIMMSLOC)//'\bin\aimms.exe'
 aimms64 = trim(AIMMSLOC)//'\bin\aimms.exe'


 inquire(file=aimms64,exist=lexist)
 if(lexist) then
    write(O,'(A)') 'AIMMS executable found:   '//trim(aimms64)
 else
    inquire(file=aimms32,exist=lexist)
    if(lexist) then
       write(O,'(A)') 'AIMMS executable found:   '//trim(aimms32)
    else
      no_AIMMS=.true.
    endif
 endif

 return

End Subroutine Read_AIMMS_Link

!===================================================================================================================

Subroutine Write_to_AIMMS_txt_cmd
! invokes aimms through command line. assumed curcalyr and curitr send via text file (monitor.in.txt)
  use ifcore,only:commitqq
  use ifport,only:timef
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

!===================================================================================================================

subroutine unitunopened(start,end,iunit)
implicit none
logical :: lopen
integer :: start,end,iunit
integer i
iunit=-1
do i= start,end
  iunit=i
  inquire(unit=iunit,opened=lopen)
  if(.not. lopen) then
     return
  endif
enddo
return
end

subroutine stat_price
use ifport, only: GETDRIVEDIRQQ,FILE$CURDRIVE, $MAXPATH
implicit none
! invokes an R procedure to estimate propane and ethane prices, aka NGPLs
    include 'parametr'
    include 'ncntrl'
    include 'mpblk'
    include 'qblk'
    include 'macout'
    include 'ngtdmrep'
    include 'intout' ! wti_price
    include 'pmmout'
    include 'convfact'
    integer :: runit, i, ir, iy, iyn, nemsyr, iy2011

    integer regyear ! for regionalizing historical propane price data

    character*80 line
    character*120 rcmd
    character*12 NGPL_Price
    character*15 RLOC
    character*80 RTERM
    character*15 dummy

    integer :: iyear,colA,iret
    real :: colB,colC,colD,colE,colF,colG,colH,colI,colJ,colK,colL,colM,colN,colO,colP,colQ

! variables and data in 4 lines below are for breaking out CD-level prices
      real resiweights(1:9)/0.172,0.161,-0.102,-0.183,0.111,0.0582,0.0385,-0.0416,0.120/
      real commweights(1:9)/0.0188,0.0753,-0.0797,-0.121,0.0369,-0.0164,-0.0210,-0.00942,0.0487/
      real tranweights(1:9)/-0.0233,-0.0159,0.00396,-0.0106,0.0254,0.0200,-0.00514,-0.0240,-0.000260/
      real resinat, commnat, trannat

      real lpgbase ! base propane feedstock (Mt. Belvieu) price = PPRINPF
      real rs_prcoef(3), cm_prcoef(3), in_prcoef(3), tr_prcoef(3)
      integer ipropco
      integer icd  ! CD counter for Census Districts

    real :: bar_per_gal

    CHARACTER($MAXPATH) dir
    INTEGER(4) ldir

! +++ Variables to Call FILE_MGR for Initialization
      INTEGER IROUTDBG
      LOGICAL NEW/.TRUE./
      INTEGER FILE_MGR
      INTEGER RTOVALUE           ! FUNCTION TO READ RUN TIME OPTIONS
      EXTERNAL FILE_MGR, RTOVALUE
      INTEGER*4 AEOLSTYR   ! run-time option set to calendar year for last year of AEO projection

      iy2011=2011-baseyr+1

!  put in most recent historical years this way to take precedence over R model output in subsequent cycles
       petin(mnumcr,26:31)   = (/2.777,2.980,3.740,4.958,3.231,2.843/)      &  ! 2015-2020, nominal dollars/mmBtu, ethane
                                     / MC_JPGDP(26:31)       ! convert to 1987 dollars per million Btu
       pprinpf(mnumcr,26:31) = (/0.452,0.484,0.762,0.880,0.531,0.463/)  &  ! 2015-2020, nominal dollars/gal, propane
                       * 42. / cfprq / MC_JPGDP(26:31)       ! convert to 1987 dollars per million Btu

!  Get current drive and directory
    dir  = FILE$CURDRIVE      ! get default drive
    ldir = GETDRIVEDIRQQ(Dir) ! on input, Dir is just Drive; on output Dir is drive/path

! create the input file to the external R program
!   get unused unit number
    call unitunopened(100,999,runit)

! create a RunName with the cycle number as part of the name:
    write(NGPL_Price,'(a,i2.2)') 'NGPL_Price',curirun  ! include the cycle number in the run name to save each invocation

!   Write R program's input file
    open(unit=runit,file=trim(NGPL_Price)//'_RInputData.csv',status='unknown')
    rewind runit    ! it may exist, so this causes the output to overwrite rather than append

! create the input file in csv format, beginning with the header line
    write(runit,  '(  25(a,",") )' ) &
        'Year',                                          &     !  colA
        'total NGPL production (Mbbl/d)',                &     !  colB
        'ethane annual NGPL production (Mbbl/d)',        &     !  colC
        'propane annual NGPL production (Mbbl/d)',       &     !  colD
        'ethane annual cents/gal (nominal)',             &     !  colE
        'ethane annual cents/gal (real 2011$)',          &     !  colF
        'propane annual cents/gal (nominal)',            &     !  colG
        'propane annual cents/gal (real 2011$)',         &     !  colH
        'organic shipment demand (billion 2009$)',       &     !  colI
        'resin shipment demand (billion 2009$)',         &     !  colJ
        'total chemical demand (billion 2009$)',         &     !  colK
        'WTI (2011$/ barrel)',                           &     !  colL
        'HH (2011$/MMBTU)',                              &     !  colM
        'deflator to 1987$',                             &     !  colN
        'Brent (2011$)',                                 &     !  colO
		'ethane consumed (trils)',                       &     !  colP
		'propane consumed (trils)'                             !  colQ


    IROUTDBG = FILE_MGR('O','ROUTDBG           ',NEW)
    AEOLSTYR = RTOVALUE("AEOLSTYR",0)  ! Get calendar year for last year of AEO projection period.
    do iy=2002,AEOLSTYR
      iyn=iy-baseyr+1  ! convert calendar year to NEMS year index, 1 for 1990 (NEMS baseyr)
      iy2011=2011-baseyr+1
      bar_per_gal=1./42.
      colB =  sum(RFQNGPL(MNUMPR,iyn,1:5))
      colC =  RFQNGPL(MNUMPR,iyn,1)
      colD =  RFQNGPL(MNUMPR,iyn,2)
      colE =  PETIN(11,iyn) * MC_JPGDP(iyn)   * 100. * CFEEQ * bar_per_gal  ! nominal
      colF =  PETIN(11,iyn) * MC_JPGDP(iy2011)* 100. * CFEEQ * bar_per_gal  ! $2011
      colG =  PPRINPF(11,iyn) * MC_JPGDP(iyn)   * 100. * CFPRQ * bar_per_gal       ! nominal
      colH =  PPRINPF(11,iyn) * MC_JPGDP(iy2011)* 100. * CFPRQ * bar_per_gal       ! $2011
      colI =  MC_REVIND(11,16,iyn)
      colJ =  MC_REVIND(11,18,iyn)
      colK =  (MC_REVIND(11,15,iyn) + MC_REVIND(11,16,iyn) + MC_REVIND(11,18,iyn) + MC_REVIND(11,19,iyn))
      colL =  WTI_PRICE(iyn) * MC_JPGDP(iy2011)
      colM =  OGHHPRNG(iyn)  * MC_JPGDP(iy2011)
      colN =  MC_JPGDP(iyn)     ! GDP Implicit price deflator indexed to 1987
      colO =  BRENT_PRICE(iyn) * MC_JPGDP(iy2011)
	  colP =  QETINPF(11, iyn)
	  colQ =  QPRINPF(11, iyn)


      write (IROUTDBG,*) 'iyn = ',iyn
      write (IROUTDBG,*) 'iy2011 = ',iy2011
      write (IROUTDBG,*) 'baseyr = ',baseyr
      write (IROUTDBG,*) 'PPRINPF(11,iyn) in 1987$/MMBTU = ',PPRINPF(11,iyn)
      write (IROUTDBG,*) 'PPRINPF(11,iyn) in 2011 cents/gallon = ',PPRINPF(11,iyn) * MC_JPGDP(iy2011) * 100.0 * CFPRQ * bar_per_gal
      write (IROUTDBG,*) 'DLM (1)'
      write (IROUTDBG,*)


100 format(i4,',',25(F15.6,',') )
      write(runit,100) iy,colB,colC,colD,colE,colF,colG,colH,colI,colJ,colK,colL,colM,colN,colO,colP,colQ

    end do

    close(runit)

      IPROPCO = FILE_MGR('O','PROPCO            ',.FALSE.)
      if (ipropco .gt. 0) then
write(6,'(" Reading propane price coefficients from file!")')
         read (ipropco,*)  dummy
         read (ipropco,*)  dummy,(rs_prcoef(i),i=1,3)
         read (ipropco,*)  dummy,(cm_prcoef(i),i=1,3)
         read (ipropco,*)  dummy,(in_prcoef(i),i=1,3)
         read (ipropco,*)  dummy,(tr_prcoef(i),i=1,3)
      else   ! if file not read properly, fill them with the AEO2020 values
write(6,'(" NOT reading propane price coefficients from file!")')
         rs_prcoef=(/0.69960,0.30784,0.61500/)
         cm_prcoef=(/1.58368,0.51461,0.23796/)
         in_prcoef=(/0.71060,0.75976,0.14961/)
         tr_prcoef=(/1.80588,0.51702,0.18045/)
      endif

    ! create command like this to invoke R using the RTERM executable:
    !
    !        rterm.exe -e "RunName <- 'NGPL_Price01'" -e "source('input/ngplprice.r')"
    !

    call RTOSTRING('RLOC    ',RLOC)        !  R version node, such as 'R-2.15.1'
    RTERM = 'C:\R\' // trim(RLOC) // '\bin\x64\RTerm.exe'
    rcmd=trim(RTERM) // ' -e "RunName <- '''  //  trim(NGPL_Price)  // '''" -e "source(''input/ngplprice.r'')"  '
    call callsys(iret,rcmd)


    if(iret .eq. 0) then
       open(unit=runit,file=trim(NGPL_Price)//'_ROutputData.csv',status='old',readonly)

       ! skip a line
       read(runit,'()')
       do while (.not. eof(runit))
         read(runit,'(a)') line
         read(line,'(1x,i4,1x)') iyear
         line(1:7)=' '
         read(line,*,err=99,end=99) colB,colC

         nemsyr=iyear-baseyr+1

         if(iyear.gt.2017) then
         petin(mnumcr,nemsyr) = colB
         pprinpf(mnumcr,nemsyr) = colC

         write (IROUTDBG,*) 'nemsyr = ',nemsyr
         write (IROUTDBG,*) 'iyear = ',iyear
         write (IROUTDBG,*) 'iy2011 = ',iy2011
         write (IROUTDBG,*) 'PPRINPF(11,nemsyr) in 2011 cents/gallon = ',PPRINPF(11,nemsyr)
         write (IROUTDBG,*) 'PPRINPF(11,nemsyr) in 1987$/MMBTU = ',PPRINPF(11,nemsyr) * 0.01 * (1.0/CFPRQ) * (1.0/bar_per_gal) * (1.0/MC_JPGDP(iy2011))
         write (IROUTDBG,*) 'DLM (2)'
         write (IROUTDBG,*)

! convert ethane price from 2011 cents/gallon to 1987$/MMBTU
         PETIN(mnumcr,nemsyr) = PETIN(mnumcr,nemsyr) * 0.01 * (1.0/CFEEQ) * (1.0/bar_per_gal) * (1.0/MC_JPGDP(iy2011))
         PETINPF(mnumcr,nemsyr) = PETIN(mnumcr,nemsyr) ! ethane feedstock = ethane industrial
                                                       ! since it's only used for feedstock after all

         do icd = 1,9                                  ! assume CD prices = national price for ethane feedstock...
            petin(icd,nemsyr) = petin(mnumcr,nemsyr)   ! 95+% of ethane cracked/used in CD 7 anyway
            petinpf(icd,nemsyr) = petinpf(mnumcr,nemsyr)
         end do

! base (Mt. Belvieu price) for sectoral compuations below (lpgbase must be in 1987 cents/gallon)
! see LFMM documentation sent to Beth May, Mike Cole, Mindi, Kelly, Janice on 08/27/2012
!         lpgbase = log(pprinpf(mnumcr,nemsyr) * (1.0/MC_JPGDP(iy2011)))
          lpgbase = pprinpf(mnumcr,nemsyr)

! set up sectoral prices based on regression relationships to Mt. Belvieu price (aka petchem feedstock price)
! these sectoral prices are computed as 1987$/MMBTU, as they should be for use NEMS wide
!            plgrs(mnumcr,nemsyr) = exp(0.2425 * lpgbase + 3.827) * 0.01 * 42.0 / CFPRQ
!            plgcm(mnumcr,nemsyr) = exp(0.3501 * lpgbase + 3.237) * 0.01 * 42.0 / CFPRQ
!            plgin(mnumcr,nemsyr) = exp(0.3656 * lpgbase + 3.190) * 0.01 * 42.0 / CFPRQ
!            plginpf(mnumcr,nemsyr) = exp(0.9986 * lpgbase) * 0.01 * 42.0 / CFPRQ
            plgrs(mnumcr,nemsyr) = exp(rs_prcoef(3) * log(plgrs(mnumcr,nemsyr-1)*MC_JPGDP(iy2011)*CFPRQ*100./42.) + rs_prcoef(2) * log(lpgbase) + rs_prcoef(1)) * 0.01 * 42.0 / CFPRQ / MC_JPGDP(iy2011)
            plgcm(mnumcr,nemsyr) = exp(cm_prcoef(3) * log(plgcm(mnumcr,nemsyr-1)*MC_JPGDP(iy2011)*CFPRQ*100./42.) + cm_prcoef(2) * log(lpgbase) + cm_prcoef(1)) * 0.01 * 42.0 / CFPRQ / MC_JPGDP(iy2011)
            plgin(mnumcr,nemsyr) = exp(in_prcoef(3) * log(plgin(mnumcr,nemsyr-1)*MC_JPGDP(iy2011)*CFPRQ*100./42.) + in_prcoef(2) * log(lpgbase) + in_prcoef(1)) * 0.01 * 42.0 / CFPRQ / MC_JPGDP(iy2011)
            plgtr(mnumcr,nemsyr) = exp(tr_prcoef(3) * log(plgtr(mnumcr,nemsyr-1)*MC_JPGDP(iy2011)*CFPRQ*100./42.) + tr_prcoef(2) * log(lpgbase) + tr_prcoef(1)) * 0.01 * 42.0 / CFPRQ / MC_JPGDP(iy2011)
            plginpf(mnumcr,nemsyr) = (QETIN(MNUMCR,nemsyr)*PETIN(mnumcr,nemsyr)+QPRIN(MNUMCR,nemsyr)*PPRIN(mnumcr,nemsyr))/(QETIN(MNUMCR,nemsyr)+QPRIN(MNUMCR,nemsyr))


            pprrs(mnumcr,nemsyr) = plgrs(mnumcr,nemsyr) ! fill in national propane prices as "LPG"
            pprcm(mnumcr,nemsyr) = plgcm(mnumcr,nemsyr)
            pprin(mnumcr,nemsyr) = plgin(mnumcr,nemsyr)
            pprinpf(mnumcr,nemsyr) = (1.0 * lpgbase) * 0.01 * 42.0 / CFPRQ / MC_JPGDP(iy2011) 
            pprtr(mnumcr,nemsyr) = plgtr(mnumcr,nemsyr)

! set up regionality for propane prices
! only do residential, commercial, and transportation (industrial all in CD 7 anyway)
            do icd = 1,9
               plgrs(icd,nemsyr) = plgrs(mnumcr,nemsyr) * (1.0 + resiweights(icd))
               plgcm(icd,nemsyr) = plgcm(mnumcr,nemsyr) * (1.0 + commweights(icd))
               plgtr(icd,nemsyr) = plgtr(mnumcr,nemsyr) * (1.0 + tranweights(icd))
               plgin(icd,nemsyr) = plgin(mnumcr,nemsyr)     ! CD's same as national (not going to fuss with trivial amounts
               plginpf(icd,nemsyr) = plginpf(mnumcr,nemsyr) ! CD's same as national (they only crack it in CD)
            end do

! store national weighted price from just computed regional prices
            resinat = 0.0
            commnat = 0.0
            trannat = 0.0
            do icd = 1,9
               resinat = resinat + plgrs(icd,nemsyr) * qlgrs(icd,nemsyr) / qlgrs(11,nemsyr)
               commnat = commnat + plgcm(icd,nemsyr) * qlgcm(icd,nemsyr) / qlgcm(11,nemsyr)
               trannat = trannat + plgtr(icd,nemsyr) * qlgtr(icd,nemsyr) / qlgtr(11,nemsyr)
            end do

! renomalize CD prices so that their average-weighted sum is = to the desired national price
            do icd = 1,9
               plgrs(icd,nemsyr) = plgrs(icd,nemsyr) * plgrs(11,nemsyr) / resinat
               plgcm(icd,nemsyr) = plgcm(icd,nemsyr) * plgcm(11,nemsyr) / commnat
               plgtr(icd,nemsyr) = plgtr(icd,nemsyr) * plgtr(11,nemsyr) / trannat
            end do

            do icd = 1,9 ! fill in CD-level propane prices as LPG; feedstock and industrial are same as national (see above)
               pprrs(icd,nemsyr) = plgrs(icd,nemsyr)
               pprcm(icd,nemsyr) = plgcm(icd,nemsyr)
               pprin(icd,nemsyr) = plgin(icd,nemsyr)
               pprinpf(icd,nemsyr) = plginpf(icd,nemsyr)
               pprtr(icd,nemsyr) = plgtr(icd,nemsyr)
            end do

! set up historical regional prices for 2011 - 2013
! set up regionality for propane prices
! only do residential, commercial, and transportation (industrial all in CD 7 anyway)
            if (nemsyr.eq.25) then ! only set up historical regional prices when first projection year worked
             do regyear = 22,24    ! loop over 2011 - 2013

              do icd = 1,9
                 plgrs(icd,regyear) = plgrs(mnumcr,regyear) * (1.0 + resiweights(icd))
                 plgcm(icd,regyear) = plgcm(mnumcr,regyear) * (1.0 + commweights(icd))
                 plgtr(icd,regyear) = plgtr(mnumcr,regyear) * (1.0 + tranweights(icd))
                 plgin(icd,regyear) = plgin(mnumcr,regyear)     ! CD's same as national (not going to fuss with trivial amounts
                 plginpf(icd,regyear) = plginpf(mnumcr,regyear) ! CD's same as national (they only crack it in CD)
              end do

! store national weighted price from just computed regional prices
              resinat = 0.0
              commnat = 0.0
              trannat = 0.0
              do icd = 1,9
                 resinat = resinat + plgrs(icd,regyear) * qlgrs(icd,regyear) / qlgrs(11,regyear)
                 commnat = commnat + plgcm(icd,regyear) * qlgcm(icd,regyear) / qlgcm(11,regyear)
                 trannat = trannat + plgtr(icd,regyear) * qlgtr(icd,regyear) / qlgtr(11,regyear)
              end do

! renomalize CD prices so that their average-weighted sum is = to the desired national price
              do icd = 1,9
                 plgrs(icd,regyear) = plgrs(icd,regyear) * plgrs(11,regyear) / resinat
                 plgcm(icd,regyear) = plgcm(icd,regyear) * plgcm(11,regyear) / commnat
                 plgtr(icd,regyear) = plgtr(icd,regyear) * plgtr(11,regyear) / trannat
              end do

              do icd = 1,9 ! fill in CD-level propane prices as LPG; feedstock and industrial are same as national (see above)
                 pprrs(icd,regyear) = plgrs(icd,regyear)
                 pprcm(icd,regyear) = plgcm(icd,regyear)
                 pprin(icd,regyear) = plgin(icd,regyear)
                 pprinpf(icd,regyear) = plginpf(icd,regyear)
                 pprtr(icd,regyear) = plgtr(icd,regyear)
              end do
             end do        ! loop over historical years 2011 - 2013
            end if         ! end set up historical regional prices for 2011 - 2013

         write (IROUTDBG,*) 'residential CD1 2011 = ',pprrs(1,22)
         write (IROUTDBG,*) 'residential CD2 2011 = ',pprrs(2,22)

            write (IROUTDBG,*) 'In 1987$/MMBTU '
            write (IROUTDBG,*) 'DLM ethane', petin(mnumcr,nemsyr)
            write (IROUTDBG,*) 'DLM PPRINPF', pprinpf(mnumcr,nemsyr)
            write (IROUTDBG,*) 'DLM PPRIN', pprin(mnumcr,nemsyr)
            write (IROUTDBG,*) 'DLM PPRRS', pprrs(mnumcr,nemsyr)
            write (IROUTDBG,*) 'DLM PPRCM', pprcm(mnumcr,nemsyr)
            write (IROUTDBG,*) 'DLM PPRTR', pprtr(mnumcr,nemsyr)

            write (IROUTDBG,*) 'In 2011$/MMBTU '
            write (IROUTDBG,*) 'DLM ethane', petin(mnumcr,nemsyr) * MC_JPGDP(iy2011)
            write (IROUTDBG,*) 'DLM PPRINPF', pprinpf(mnumcr,nemsyr) * MC_JPGDP(iy2011)
            write (IROUTDBG,*) 'DLM PPRIN', pprin(mnumcr,nemsyr) * MC_JPGDP(iy2011)
            write (IROUTDBG,*) 'DLM PPRRS', pprrs(mnumcr,nemsyr) * MC_JPGDP(iy2011)
            write (IROUTDBG,*) 'DLM PPRCM', pprcm(mnumcr,nemsyr) * MC_JPGDP(iy2011)
            write (IROUTDBG,*) 'DLM PPRTR', pprtr(mnumcr,nemsyr) * MC_JPGDP(iy2011)

         end if

       end do  ! end of do while .not. eof(runit)
       close(runit)
    end if
    return

99  write(6,'(a)')  'Error reading R output file in subroutine stat_price'
    close(runit)
    end subroutine stat_price



!  subroutine READ_ALL_STEO_VARS reads all the STEO variables for possible module benchmarking
!  it reads them all, but retains only those that we think we can benchmark to
!  "retains" means hold in common block memory for now, not in the global data structure
!  the file it reads is the a15bbb.csv file from the STEO EViews directory

SUBROUTINE READ_ALL_STEO_VARS
IMPLICIT NONE

include 'parametr'
include 'steoblock'
include 'ncntrl'

INTEGER I, I1, I2, ISP, IC, IYEAR, STEO_YEARS(MNUMYR), STEO_VARS_FOUND(STEOVR)
INTEGER STEO_BOL, STEO_EOL, STEO_COUNT, UNIT_STEO_IN, UNIT_STEO_OUT

! For writing out clsteo.txt file:
INTEGER EUNIT, EXCOAL, EXNGAIM
!  End variables for writing out clsteo.txt file

REAL STEO_IN(MNXYR)    !  using MNXYR to accomodate extra years before 1990; current file starts in 1975
REAL STEO_DEF_1987     !  STEO 1987 defalator for converting prices from nominal dollars to 1987 dollars

LOGICAL READ_STEO_ONCE /.TRUE./, READ_LIST_ONCE /.TRUE./
LOGICAL COMPILE_LIST /.FALSE./     !  start false, set to true when find the beginning of the read range
LOGICAL KEEP_CHECKING
LOGICAL FOUND_END_OF_LIST

INTEGER FILE_MGR, RTOVALUE
EXTERNAL FILE_MGR, RTOVALUE
LOGICAL NEW

!  currently, the years run from 1975-2016.  add in the labels and the commas and you get around
!  516 characters for a line.  STEO_LINE is bigger than that for room for growth as STEO adds years.
!  this 999 appears in an IF block after STEO_LINE is read checking if a line is approaching the limit.
!  so if this gets changed from 999, change it there, too, and the slightly smaller number in the IF
!  and also the formats where STEO_LINE is read
CHARACTER*999 STEO_LINE     !  if this changes size, the read below also needs to change
CHARACTER*25 STEO_VARS_KEEP(STEOVR), FIRST_COL, SECOND_COL
CHARACTER*6  STEO_MONTH


!   writes will go to STEODBUG.txt except ones with "Problem" (or "problem") in string

STEO_VARS_KEEP=''

NEW=.true.
UNIT_STEO_OUT = FILE_MGR('O','STEODBUG          ',NEW)
NEW=.false.
UNIT_STEO_IN  = FILE_MGR('O','STEOLIST          ',NEW)

! reading in variable list from a copy of the include file.  this requires the include file variable
! declarations be in the same order as they are in the common block
! note that the steoblock include file is the same as the steolist.txt input file.
! one can be copied to the other, usually the include file to the input file.
      STEO_COUNT=0
write(UNIT_STEO_OUT,'("  Reading file with list of variables")')
      DO WHILE (READ_LIST_ONCE)
         READ(UNIT_STEO_IN,'(A999)') STEO_LINE
!write(UNIT_STEO_OUT,'("    Line:  ",A)') trim(STEO_LINE)
! start of variable list is !@ in first two columns (the ! is because it is code, too; this comments)
         IF (STEO_LINE(1:2) .EQ. '!@') COMPILE_LIST=.TRUE.
         IF (.NOT. COMPILE_LIST) CYCLE
write(UNIT_STEO_OUT,'("  Reading list of variables:  found beginning of list")')
         FOUND_END_OF_LIST=.FALSE.
         DO WHILE (.NOT. FOUND_END_OF_LIST)
            READ(UNIT_STEO_IN,'(A80)',END=876) STEO_LINE     !  80 characters should get the variable name
            I2=LEN_TRIM(STEO_LINE)
            DO IC=1,I2      !  this loop converts the line to upper case
               IF (ICHAR(STEO_LINE(IC:IC)) .GE. 97 .AND. ICHAR(STEO_LINE(IC:IC)) .LE. 122) &
                   STEO_LINE(IC:IC)=CHAR(ICHAR(STEO_LINE(IC:IC))-32)
            ENDDO
            IF (STEO_LINE(1:2) .EQ. '!@') THEN       !  no processing, at end of list, exit loop
               FOUND_END_OF_LIST=.TRUE.
write(UNIT_STEO_OUT,'(" End of list found after ",I4," variable names found.")') STEO_COUNT
               EXIT
            ENDIF
            I2=INDEX(STEO_LINE,'(MNUMYR)')-1         !  this all falls apart if variables are not all dimensioned by mnumyr
            IF (I2 .GT. 0) THEN                      !  presence of mnumyr dimensioning indicates variable
               STEO_COUNT=STEO_COUNT+1               !  count the variables, why not
               IF (STEO_COUNT .GT. STEOVR) THEN
write(6,'("  Problem:  Number of variables in common block greater than STEOVR(",I4,"): ",I6," variables read.")') STEOVR, STEO_COUNT
                  EXIT
               ENDIF
               I1=0
               DO IC=I2,1,-1                         !  find non-space working backwards (allows for spaces between variable name and dimension
                  IF (STEO_LINE(IC:IC) .NE. ' ') THEN
!write(UNIT_STEO_OUT,'("  Reading list of variables:  found end position of variable name:  ",I4)') IC
                     EXIT
                  ENDIF
               ENDDO
               DO ISP=IC,1,-1                        !  now find space before variable name
                  IF (STEO_LINE(ISP:ISP) .EQ. ' ') THEN
                     I1=ISP+1                        !  beginning of variable name
                     STEO_VARS_KEEP(STEO_COUNT)=STEO_LINE(I1:I2)   !  store variable name
write(UNIT_STEO_OUT,'("  Reading list of variables:  found variable name:  ",A)') trim(STEO_VARS_KEEP(STEO_COUNT))
                     EXIT
                  ENDIF
               ENDDO
!  there can be blank lines, comments, etc, scattered throughout the variable name list:
               IF (I1 .EQ. 0) write(6,'("  Possible problem:  variable name not found ",I4," lines into range:  ",A)') I,trim(STEO_LINE)
            ENDIF
            CYCLE
876         write(6,'("  Problem:  End of file before end of STEO list after ",I6," variables read.")') STEO_COUNT
            EXIT
         ENDDO
         READ_LIST_ONCE=.FALSE.
      ENDDO

!  close file with list of variables; open file with values for variables
UNIT_STEO_IN  = FILE_MGR('C','STEOLIST          ',NEW)
NEW=.false.
UNIT_STEO_IN  = FILE_MGR('O','STEOVARS          ',NEW)

      STEO_COUNT=0
      STEO_VARS_FOUND=1                   !  will get turned to 0 when the variable is filled
      DO WHILE (READ_STEO_ONCE)
         STEO_COUNT=STEO_COUNT+1
         READ(UNIT_STEO_IN,'(A999)',END=987) STEO_LINE
         FIRST_COL=''
         STEO_EOL=LEN_TRIM(STEO_LINE)
!  some data ends before the last STEO year.  this, putting a 0 at the end of lines ending with a comma,
!  is an attempt to read those lines without hitting end-of-line early
!  (filled cells in the final column don't have a comma following them when saved comma-delimited)
         IF (STEO_LINE(STEO_EOL:STEO_EOL) .EQ. ',') THEN
            STEO_LINE(STEO_EOL+1:STEO_EOL+1) = "0"
            STEO_EOL=LEN_TRIM(STEO_LINE)
         ENDIF
         IF (STEO_EOL .GT. 990) THEN
write(6,'("  Possible problem:  end of line=",I6," approaching maximum length of line read=",I6)') STEO_EOL,999
         ENDIF
         STEO_BOL=1
         IF (STEO_EOL .LT. STEO_BOL) THEN
write(6,'("  Interesting problem in which the end of line is before the beginning of line.  Line=",I6)') STEO_COUNT
            CYCLE
         ENDIF
         DO I=1,STEO_EOL
            IF (STEO_LINE(I:I) .EQ. ',') THEN
               STEO_BOL=I+1                                 !  to get index point after first comma
               FIRST_COL=''
               IF (I .GT. 1) FIRST_COL=STEO_LINE(1:I-1)     !  to get index point before first comma
write(UNIT_STEO_OUT,'(" STEO variable #",A,"# data begins at position=",I4,"  ends at position=",I4)') trim(FIRST_COL),STEO_BOL,STEO_EOL
               EXIT                        !  assigned STEO beginning of line for data reads, exit loop
            ENDIF
         ENDDO
         IF (trim(FIRST_COL) .EQ. 'TIMESTAMP' .OR. trim(FIRST_COL) .EQ. 'AAA_OFFSET' .OR. trim(FIRST_COL) .EQ. 'AAAA_DATEX') THEN
write(UNIT_STEO_OUT,'(" skip:  ",A)') trim(STEO_LINE)
            CYCLE   !  ignore these lines
         ELSE IF (FIRST_COL .EQ. '') THEN   ! unlabelled line has years ('AAA_DATEX' also has years)
            READ(STEO_LINE(STEO_BOL:STEO_EOL),*) FIRSTSTEOYEAR
write(UNIT_STEO_OUT,'(" dates, first year = ",I4)') FIRSTSTEOYEAR
            SKIPYEARS = BASEYR - FIRSTSTEOYEAR
            LASTSTEOYEAR=0
            DO I=STEO_EOL,STEO_BOL,-1
               IF (STEO_LINE(I:I) .EQ. ',') THEN
                  READ (STEO_LINE(I+1:STEO_EOL),'(I)') LASTSTEOYEAR
write(UNIT_STEO_OUT,'(" dates, last year = ",I4)') LASTSTEOYEAR
                  EXIT                        !  assigned lastSTEOyear, exit loop
               ENDIF
            ENDDO
            IF (LASTSTEOYEAR .EQ. 0) THEN
               write(6,'("  Problem:  Failure to find last STEO year, so I do not know how many numbers to read.  So I am giving up.")')
               RETURN
            ENDIF
         ELSE IF (FIRST_COL .EQ. 'STEOMONTH') THEN   ! in mmm-yy format such as "Oct-15"; current plan is to check in STEO file a15bbb as steovars.csv every month
            READ(STEO_LINE(STEO_BOL:STEO_EOL),*) SECOND_COL
            STEO_MONTH=SECOND_COL(1:6)
write(UNIT_STEO_OUT,'(" STEO month found = ",A)') STEO_MONTH
         ELSE                       !  variable line, read in variable name and data
!  initialize read variable so lack of entry is 0 rather than the last read value for that year
!  this variable is dimensioned to cover the years in the STEO file, which begins before 1990
!  all the years are read.  it will only transfer over the applicable years
            STEO_IN=0.0
            READ(STEO_LINE(STEO_BOL:STEO_EOL),*) (STEO_IN(I),I=1,LASTSTEOYEAR-FIRSTSTEOYEAR+1)
!write(UNIT_STEO_OUT,'(" echo of input line to STEO_IN:  ",<LASTSTEOYEAR-FIRSTSTEOYEAR+1>F12.5)') (STEO_IN(I),I=1,LASTSTEOYEAR-FIRSTSTEOYEAR+1)
! determine the location of the variable in the equivalenced array and copy values from input variable
! to STEO common block variable
            KEEP_CHECKING=.TRUE.
            DO I=1,STEOVR
!write(UNIT_STEO_OUT,'( "checking ",A," against ",A)') trim(FIRST_COL), trim(steo_vars_keep(I))
               IF (FIRST_COL .EQ. steo_vars_keep(I) .AND. KEEP_CHECKING) THEN
                  write(UNIT_STEO_OUT,'("  STEO variable ",A," found at equivalenced position ",I4)') FIRST_COL, I
!  keep track of variables found, so we can report a list of those not matched up at the end
!  defining "not matched up" as in the include file but not in the STEO data file and thus not filled
                  STEO_VARS_FOUND(I) = 0
!write(UNIT_STEO_OUT,'("  variable number ",I6,";  start at ",I6,";  end at ",I6,";  translated start=",I6)') I, (I-1)*MNUMYR+FIRSTSTEOYEAR+SKIPYEARS-BASEYR+1, (I-1)*MNUMYR+LASTSTEOYEAR-BASEYR+1, (I-1)*MNUMYR+FIRSTSTEOYEAR+SKIPYEARS-BASEYR+1
!  assign the values to the include file variable using the equivalence location determined above:
                  DO IYEAR=FIRSTSTEOYEAR+SKIPYEARS,LASTSTEOYEAR
                     EQ_STEO_VARS((I-1)*MNUMYR+IYEAR-BASEYR+1) = STEO_IN(IYEAR-FIRSTSTEOYEAR+1)
                  ENDDO
                  KEEP_CHECKING=.FALSE.
                  IF (FIRST_COL .EQ. 'GDPDIUS') THEN
                      STEO_DEF_1987 = STEO_IN(1987-FIRSTSTEOYEAR+1)
write(UNIT_STEO_OUT,'("  The 1987 implicit price deflator=",F8.3)') STEO_DEF_1987
                  ENDIF
! we could probably save some time and exit the loop at this point...
               ENDIF
            ENDDO
         ENDIF
         CYCLE
987      write(UNIT_STEO_OUT,'("  End of STEO file reached after ",I6, " variables read.")') STEO_COUNT
         READ_STEO_ONCE=.FALSE.
      ENDDO
      IF (sum(STEO_VARS_FOUND) .GT. 0) THEN       !  means that something was not filled from file
         DO I=1,STEOVR
            IF (STEO_VARS_FOUND(I) .EQ. 1) write (UNIT_STEO_OUT,'("  variable ",A," not Encountered when it was an expected STEO variable")') STEO_VARS_KEEP(I)
         ENDDO
      ENDIF

UNIT_STEO_IN  = FILE_MGR('C','STEOVARS          ',NEW)
! doing some conversions to make things a little easier for the models
! STEO has natural gas and electricity in units per day while AEO has units per year
! also:  using STEO days and STEO deflator to do these conversions for enhanced STEO consistency
DO I=1,LASTSTEOYEAR-1989
!   convert these from billion cubic feet per day to trillion cubic feet
    BALIT(I)   =  BALIT(I)   * ZSAJQUS(I) / 1000.   !  natural gas balancing item
    NGEXPUS_LNG(I)  =  NGEXPUS_LNG(I)  * ZSAJQUS(I) / 1000. !  liquefied natural gas exports
    NGEXPUS_PIPE(I) =  NGEXPUS_PIPE(I) * ZSAJQUS(I) / 1000. !  pipeline natural gas exports
    NGEXPUS(I) =  NGEXPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas exports
    NGIMPUS(I) =  NGIMPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas imports
    NGIMPUS_LNG(I)  =  NGIMPUS_LNG(I)  * ZSAJQUS(I) / 1000. !  liquefied natural gas imports
    NGIMPUS_PIPE(I) =  NGIMPUS_PIPE(I) * ZSAJQUS(I) / 1000. !  pipeline natural gas imports
    NGNWPUS(I) =  NGNWPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas net withdrawals from inventory
    NGPRPUS(I) =  NGPRPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas total dry production
    NGRCPUS(I) =  NGRCPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas consumption, residential
        NGRCP_ENC(I) =  NGRCP_ENC(I) * ZSAJQUS(I) / 1000.
        NGRCP_ESC(I) =  NGRCP_ESC(I) * ZSAJQUS(I) / 1000.
        NGRCP_MAC(I) =  NGRCP_MAC(I) * ZSAJQUS(I) / 1000.
        NGRCP_MTN(I) =  NGRCP_MTN(I) * ZSAJQUS(I) / 1000.
        NGRCP_NEC(I) =  NGRCP_NEC(I) * ZSAJQUS(I) / 1000.
        NGRCP_PAC(I) =  NGRCP_PAC(I) * ZSAJQUS(I) / 1000.
        NGRCP_SAC(I) =  NGRCP_SAC(I) * ZSAJQUS(I) / 1000.
        NGRCP_WNC(I) =  NGRCP_WNC(I) * ZSAJQUS(I) / 1000.
        NGRCP_WSC(I) =  NGRCP_WSC(I) * ZSAJQUS(I) / 1000.
    NGCCPUS(I) =  NGCCPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas consumption, commercial
        NGCCP_ENC(I) =  NGCCP_ENC(I) * ZSAJQUS(I) / 1000.
        NGCCP_ESC(I) =  NGCCP_ESC(I) * ZSAJQUS(I) / 1000.
        NGCCP_MAC(I) =  NGCCP_MAC(I) * ZSAJQUS(I) / 1000.
        NGCCP_MTN(I) =  NGCCP_MTN(I) * ZSAJQUS(I) / 1000.
        NGCCP_NEC(I) =  NGCCP_NEC(I) * ZSAJQUS(I) / 1000.
        NGCCP_PAC(I) =  NGCCP_PAC(I) * ZSAJQUS(I) / 1000.
        NGCCP_SAC(I) =  NGCCP_SAC(I) * ZSAJQUS(I) / 1000.
        NGCCP_WNC(I) =  NGCCP_WNC(I) * ZSAJQUS(I) / 1000.
        NGCCP_WSC(I) =  NGCCP_WSC(I) * ZSAJQUS(I) / 1000.
    NGINX(I)   =  NGINX(I)   * ZSAJQUS(I) / 1000.   !  natural gas consumption, industrial (including lease and plant fuel)
    NGLPPUS(I) =  NGLPPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas lease and plant fuel
    NGVHPUS(I) =  NGVHPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas vehicle use
    NGACPUS(I) =  NGACPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas pipeline and distribution use
    NGEPCON(I) =  NGEPCON(I) * ZSAJQUS(I) / 1000.   !  natural gas consumption, electric power
    NGSFPUS(I) =  NGSFPUS(I) * ZSAJQUS(I) / 1000.   !  supplemental gaseous fuels supply
    NGTCPUS(I) =  NGTCPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas consumption
!   convert these from billion kilowatthours per day to billion kilowatthours
    EPEOPUS(I) =  EPEOPUS(I) * ZSAJQUS(I)      ! total electric power sector generation
    CLTOPUS(I) =  CLTOPUS(I) * ZSAJQUS(I)      !   from coal
    NGTOPUS(I) =  NGTOPUS(I) * ZSAJQUS(I)      !   from natural gas
    NUTOPUS(I) =  NUTOPUS(I) * ZSAJQUS(I)      !   from nuclear power
    PATOPUS(I) =  PATOPUS(I) * ZSAJQUS(I)      !   from petroleum
    HPTOPUS(I) =  HPTOPUS(I) * ZSAJQUS(I)      !   from pumped storage
    HVTOPUS(I) =  HVTOPUS(I) * ZSAJQUS(I)      !   from conventional hydropower
    GETOPUS(I) =  GETOPUS(I) * ZSAJQUS(I)      !   from geothermal
    SOTOPUS(I) =  SOTOPUS(I) * ZSAJQUS(I)      !   from all solar
    WWTOPUS(I) =  WWTOPUS(I) * ZSAJQUS(I)      !   from wood and other biomass
    TSEOPUS(I) =  TSEOPUS(I) * ZSAJQUS(I)      ! total electricity generation
    ELDUPUS(I) =  ELDUPUS(I) * ZSAJQUS(I)      ! direct use of electricity by non-electric power sector
    ELNIPUS(I) =  ELNIPUS(I) * ZSAJQUS(I)      ! net imports of electricity
    EXRCPUS(I) =  EXRCPUS(I) * ZSAJQUS(I)      ! residential electricity sales
        EXRCP_ENC(I) =  EXRCP_ENC(I) * ZSAJQUS(I) / 1000.
        EXRCP_ESC(I) =  EXRCP_ESC(I) * ZSAJQUS(I) / 1000.
        EXRCP_HAK(I) =  EXRCP_HAK(I) * ZSAJQUS(I) / 1000.
        EXRCP_MAC(I) =  EXRCP_MAC(I) * ZSAJQUS(I) / 1000.
        EXRCP_MTN(I) =  EXRCP_MTN(I) * ZSAJQUS(I) / 1000.
        EXRCP_NEC(I) =  EXRCP_NEC(I) * ZSAJQUS(I) / 1000.
        EXRCP_PAC(I) =  EXRCP_PAC(I) * ZSAJQUS(I) / 1000.
        EXRCP_SAC(I) =  EXRCP_SAC(I) * ZSAJQUS(I) / 1000.
        EXRCP_WNC(I) =  EXRCP_WNC(I) * ZSAJQUS(I) / 1000.
        EXRCP_WSC(I) =  EXRCP_WSC(I) * ZSAJQUS(I) / 1000.
    EXCCPUS(I) =  EXCCPUS(I) * ZSAJQUS(I)      ! commercial electricity sales
        EXCCP_ENC(I) =  EXCCP_ENC(I) * ZSAJQUS(I) / 1000.
        EXCCP_ESC(I) =  EXCCP_ESC(I) * ZSAJQUS(I) / 1000.
        EXCCP_HAK(I) =  EXCCP_HAK(I) * ZSAJQUS(I) / 1000.
        EXCCP_MAC(I) =  EXCCP_MAC(I) * ZSAJQUS(I) / 1000.
        EXCCP_MTN(I) =  EXCCP_MTN(I) * ZSAJQUS(I) / 1000.
        EXCCP_NEC(I) =  EXCCP_NEC(I) * ZSAJQUS(I) / 1000.
        EXCCP_PAC(I) =  EXCCP_PAC(I) * ZSAJQUS(I) / 1000.
        EXCCP_SAC(I) =  EXCCP_SAC(I) * ZSAJQUS(I) / 1000.
        EXCCP_WNC(I) =  EXCCP_WNC(I) * ZSAJQUS(I) / 1000.
        EXCCP_WSC(I) =  EXCCP_WSC(I) * ZSAJQUS(I) / 1000.
    EXICPUS(I) =  EXICPUS(I) * ZSAJQUS(I)      ! industrial electricity sales
    EXACPUS(I) =  EXACPUS(I) * ZSAJQUS(I)      ! transportation electricity sales
    EXTCPUS(I) =  EXTCPUS(I) * ZSAJQUS(I)      ! total electricity sales
    ESTXPUS(I) =  ESTXPUS(I) * ZSAJQUS(I)      ! total electricity consumption
    TDLOPUS(I) =  TDLOPUS(I) * ZSAJQUS(I)      ! transmission & distribution losses and unaccounted for
!   convert these from million kilowatthours per day to billion kilowatthours
       ! electric power sector generation
    CLEP_US(I) =  CLEP_US(I) * ZSAJQUS(I) / 1000.   !  from coal
    NGEP_US(I) =  NGEP_US(I) * ZSAJQUS(I) / 1000.   !  from natural gas
    OGEP_US(I) =  OGEP_US(I) * ZSAJQUS(I) / 1000.   !  from other gaseous fuels
    OTEP_US(I) =  OTEP_US(I) * ZSAJQUS(I) / 1000.   !  from other (unlisted in MER)
    PAEP_US(I) =  PAEP_US(I) * ZSAJQUS(I) / 1000.   !  from petroleum
    NUEP_US(I) =  NUEP_US(I) * ZSAJQUS(I) / 1000.   !  from nuclear power
    HPEP_US(I) =  HPEP_US(I) * ZSAJQUS(I) / 1000.   !  from pumped storage
    HVEP_US(I) =  HVEP_US(I) * ZSAJQUS(I) / 1000.   !  from conventional hydropower
    RNEP_US(I) =  RNEP_US(I) * ZSAJQUS(I) / 1000.   !  from renewables excluding hydropower
    WNEP_US(I) =  WNEP_US(I) * ZSAJQUS(I) / 1000.   !  from wind
    GEEP_US(I) =  GEEP_US(I) * ZSAJQUS(I) / 1000.   !  from geothermal
    SOCH_US(I) =  SOCH_US(I) * ZSAJQUS(I) / 1000.   !  from end use solar
    SOEP_US(I) =  SOEP_US(I) * ZSAJQUS(I) / 1000.   !  from electric power solar
!  no longer convert small scale solar, STEO changed units
!   SODTP_US(I) =  SODTP_US(I) * ZSAJQUS(I) / 1000. !  from small scale solar
    SOPV_US(I) =  SOPV_US(I) * ZSAJQUS(I) / 1000.   !  from solar photovoltaic
    SOTH_US(I) =  SOTH_US(I) * ZSAJQUS(I) / 1000.   !  from solar thermal
    BMEP_US(I) =  BMEP_US(I) * ZSAJQUS(I) / 1000.   !  from all biomass
    MLEP_US(I) =  MLEP_US(I) * ZSAJQUS(I) / 1000.   !  from municipal waste
    WWEP_US(I) =  WWEP_US(I) * ZSAJQUS(I) / 1000.   !  from wood and other biomass
       ! end-use generation
    RNCH_US(I) =  RNCH_US(I) * ZSAJQUS(I) / 1000.   !  from renewables excluding hydropower
    WNCH_US(I) =  WNCH_US(I) * ZSAJQUS(I) / 1000.   !  from wind
    WWCH_US(I) =  WWCH_US(I) * ZSAJQUS(I) / 1000.   !  from wood and other biomass
! convert from thousand Btu per kilowatthour to Btu per kilowatthour
    FFEOKUS(I) = FFEOKUS(I) * 1000.             ! average fossil fuel heat rate

! price conversions
!   convert these from nominal dollars per barrel to 1987 dollars per barrel
    BREPUUS(I) =  BREPUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! brent crude oil
    WTIPUUS(I) =  WTIPUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! west texas intermediate crude oil
    RAIMUUS(I) =  RAIMUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! average imported refiner inquisition
!   convert these from nominal cents per gallon to 1987 dollars per gallon
    D2RCAUS(I) =  D2RCAUS(I) * STEO_DEF_1987 / GDPDIUS(I) / 100. ! retail heating oil incl taxes
    DSRTUUS(I) =  DSRTUUS(I) * STEO_DEF_1987 / GDPDIUS(I) / 100. ! retail diesel fuel incl taxes
    JKTCUUS(I) =  JKTCUUS(I) * STEO_DEF_1987 / GDPDIUS(I) / 100. ! jet fuel from refiner to end user
    MGEIAUS(I) =  MGEIAUS(I) * STEO_DEF_1987 / GDPDIUS(I) / 100. ! retail gasoline all grades incl taxes
    MGRARUS(I) =  MGRARUS(I) * STEO_DEF_1987 / GDPDIUS(I) / 100. ! retail regular motor gasoline incl taxes
    RFTCUUS(I) =  RFTCUUS(I) * STEO_DEF_1987 / GDPDIUS(I) / 100. ! no. 6 residual fuel oil
!   convert these from nominal dollars per million Btu to 1987 dollars per million Btu
    CLEUDUS(I) =  CLEUDUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! coal, electric power
    NGEUDUS(I) =  NGEUDUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, electric power
    NGHHUUS(I) =  NGHHUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas at henry hub
!   convert these from nominal dollars per thousand cubic feet to 1987 dollars per thousand cubic feet
    NGCCU_ENC(I) =  NGCCU_ENC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, commercial, east north central
    NGCCU_ESC(I) =  NGCCU_ESC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, commercial, east south central
    NGCCU_MAC(I) =  NGCCU_MAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, commercial, middle atlantic
    NGCCU_MTN(I) =  NGCCU_MTN(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, commercial, mountain
    NGCCU_NEC(I) =  NGCCU_NEC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, commercial, new england
    NGCCU_PAC(I) =  NGCCU_PAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, commercial, pacific
    NGCCU_SAC(I) =  NGCCU_SAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, commercial, south atlantic
    NGCCU_WNC(I) =  NGCCU_WNC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, commercial, west north central
    NGCCU_WSC(I) =  NGCCU_WSC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, commercial, west south central
    NGCCUUS(I) =  NGCCUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, commercial
    NGICU_ENC(I) =  NGICU_ENC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, industrial, east north central
    NGICU_ESC(I) =  NGICU_ESC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, industrial, east south central
    NGICU_MAC(I) =  NGICU_MAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, industrial, middle atlantic
    NGICU_MTN(I) =  NGICU_MTN(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, industrial, mountain
    NGICU_NEC(I) =  NGICU_NEC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, industrial, new england
    NGICU_PAC(I) =  NGICU_PAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, industrial, pacific
    NGICU_SAC(I) =  NGICU_SAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, industrial, south atlantic
    NGICU_WNC(I) =  NGICU_WNC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, industrial, west north central
    NGICU_WSC(I) =  NGICU_WSC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, industrial, west south central
    NGICUUS(I) =  NGICUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, industrial
    NGRCU_ENC(I) =  NGRCU_ENC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, residential, east north central
    NGRCU_ESC(I) =  NGRCU_ESC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, residential, east south central
    NGRCU_MAC(I) =  NGRCU_MAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, residential, middle atlantic
    NGRCU_MTN(I) =  NGRCU_MTN(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, residential, mountain
    NGRCU_NEC(I) =  NGRCU_NEC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, residential, new england
    NGRCU_PAC(I) =  NGRCU_PAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, residential, pacific
    NGRCU_SAC(I) =  NGRCU_SAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, residential, south atlantic
    NGRCU_WNC(I) =  NGRCU_WNC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, residential, west north central
    NGRCU_WSC(I) =  NGRCU_WSC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, residential, west south central
    NGRCUUS(I) =  NGRCUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! natural gas, residential
!   convert these from nominal cents per kilowatthour Btu to 1987 cents per kilowatthour
    ESCMU_ENC(I)  =  ESCMU_ENC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! commercial electricity, east north central
    ESCMU_ESC(I)  =  ESCMU_ESC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! commercial electricity, east south central
    ESCMU_MAC(I)  =  ESCMU_MAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! commercial electricity, middle atlantic
    ESCMU_MTN(I)  =  ESCMU_MTN(I) * STEO_DEF_1987 / GDPDIUS(I)        ! commercial electricity, mountain
    ESCMU_NEC(I)  =  ESCMU_NEC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! commercial electricity, new england
    ESCMU_PAC(I)  =  ESCMU_PAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! commercial electricity, pacific
    ESCMU_SAC(I)  =  ESCMU_SAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! commercial electricity, south atlantic
    ESCMU_WNC(I)  =  ESCMU_WNC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! commercial electricity, west north central
    ESCMU_WSC(I)  =  ESCMU_WSC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! commercial electricity, west south central
    ESCMUUS(I) =  ESCMUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! commercial electricity
    ESICU_ENC(I)  =  ESICU_ENC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! industrial electricity, east north central
    ESICU_ESC(I)  =  ESICU_ESC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! industrial electricity, east south central
    ESICU_MAC(I)  =  ESICU_MAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! industrial electricity, middle atlantic
    ESICU_MTN(I)  =  ESICU_MTN(I) * STEO_DEF_1987 / GDPDIUS(I)        ! industrial electricity, mountain
    ESICU_NEC(I)  =  ESICU_NEC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! industrial electricity, new england
    ESICU_PAC(I)  =  ESICU_PAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! industrial electricity, pacific
    ESICU_SAC(I)  =  ESICU_SAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! industrial electricity, south atlantic
    ESICU_WNC(I)  =  ESICU_WNC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! industrial electricity, west north central
    ESICU_WSC(I)  =  ESICU_WSC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! industrial electricity, west south central
    ESICUUS(I) =  ESICUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! industrial electricity
    ESRCU_ENC(I)  =  ESRCU_ENC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! residential electricity, east north central
    ESRCU_ESC(I)  =  ESRCU_ESC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! residential electricity, east south central
    ESRCU_MAC(I)  =  ESRCU_MAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! residential electricity, middle atlantic
    ESRCU_MTN(I)  =  ESRCU_MTN(I) * STEO_DEF_1987 / GDPDIUS(I)        ! residential electricity, mountain
    ESRCU_NEC(I)  =  ESRCU_NEC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! residential electricity, new england
    ESRCU_PAC(I)  =  ESRCU_PAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! residential electricity, pacific
    ESRCU_SAC(I)  =  ESRCU_SAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! residential electricity, south atlantic
    ESRCU_WNC(I)  =  ESRCU_WNC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! residential electricity, west north central
    ESRCU_WSC(I)  =  ESRCU_WSC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! residential electricity, west south central
    ESRCUUS(I) =  ESRCUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! residential electricity
    ESTCU_ENC(I)  =  ESTCU_ENC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! electricity, east north central
    ESTCU_ESC(I)  =  ESTCU_ESC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! electricity, east south central
    ESTCU_MAC(I)  =  ESTCU_MAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! electricity, middle atlantic
    ESTCU_MTN(I)  =  ESTCU_MTN(I) * STEO_DEF_1987 / GDPDIUS(I)        ! electricity, mountain
    ESTCU_NEC(I)  =  ESTCU_NEC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! electricity, new england
    ESTCU_PAC(I)  =  ESTCU_PAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! electricity, pacific
    ESTCU_SAC(I)  =  ESTCU_SAC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! electricity, south atlantic
    ESTCU_WNC(I)  =  ESTCU_WNC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! electricity, west north central
    ESTCU_WSC(I)  =  ESTCU_WSC(I) * STEO_DEF_1987 / GDPDIUS(I)        ! electricity, west south central
    ESTCUUS(I) =  ESTCUUS(I) * STEO_DEF_1987 / GDPDIUS(I)        ! electricity, all sector average
ENDDO
!  some random write statements to check versus input file:
 write(UNIT_STEO_OUT,'("ARTCBUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ARTCBUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("BALIT  ",<LASTSTEOYEAR-BASEYR+1>F12.6)') (BALIT(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("BREPUUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (BREPUUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("C4TCPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (C4TCPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("CLEP_US",<LASTSTEOYEAR-BASEYR+1>F12.6)') (CLEP_US(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("CLEPCONB",<LASTSTEOYEAR-BASEYR+1>F12.6)') (CLEPCONB(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("CLIMPUS_TON",<LASTSTEOYEAR-BASEYR+1>F12.6)') (CLIMPUS_TON(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("CLPRPUS_TON",<LASTSTEOYEAR-BASEYR+1>F12.6)') (CLPRPUS_TON(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("CLYCPUS_TON",<LASTSTEOYEAR-BASEYR+1>F12.6)') (CLYCPUS_TON(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("COSX_DRAW",<LASTSTEOYEAR-BASEYR+1>F12.6)') (COSX_DRAW(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("DFTCBUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (DFTCBUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("EOPRPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (EOPRPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ESCMU_ENC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ESCMU_ENC(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ESCMUUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ESCMUUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ESICU_PAC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ESICU_PAC(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ESICUUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ESICUUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ESRCU_NEC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ESRCU_NEC(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ESRCUUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ESRCUUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ESTCU_WNC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ESTCU_WNC(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ESTCUUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ESTCUUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ETROPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ETROPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("EXRCP_HAK",<LASTSTEOYEAR-BASEYR+1>F12.6)') (EXRCP_HAK(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("EXRCPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (EXRCPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("EXCCP_NEC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (EXCCP_NEC(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("EXCCPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (EXCCPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("FFEOKUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (FFEOKUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("GDPDIUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (GDPDIUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("HPEP_US",<LASTSTEOYEAR-BASEYR+1>F12.6)') (HPEP_US(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("JFROPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (JFROPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("JKTCUUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (JKTCUUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("MGRARUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (MGRARUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("MLEPCAP_US",<LASTSTEOYEAR-BASEYR+1>F12.6)') (MLEPCAP_US(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("LUTCBUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (LUTCBUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("MSTCPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (MSTCPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGACPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGACPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGEPCONB",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGEPCONB(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGHHUUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGHHUUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGCCP_MAC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGCCP_MAC(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGCCPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGCCPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGRCP_WSC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGRCP_WSC(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGRCPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGRCPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGICU_MTN",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGICU_MTN(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGICUUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGICUUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGRCU_MAC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGRCU_MAC(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGRCUUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGRCUUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGIMPUS_PIPE",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGIMPUS_PIPE(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGNWPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGNWPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NGTCCO2",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NGTCCO2(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("NUEP_US",<LASTSTEOYEAR-BASEYR+1>F12.6)') (NUEP_US(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("OPEPCONB",<LASTSTEOYEAR-BASEYR+1>F12.6)') (OPEPCONB(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("MBFPPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (MBFPPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("MBRIPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (MBRIPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("KSTCPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (KSTCPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("LUTCPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (LUTCPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("WXTCPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (WXTCPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("OHNIPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (OHNIPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("OHRIPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (OHRIPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("PAGLPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (PAGLPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("PARIPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (PARIPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("PAEPCONB",<LASTSTEOYEAR-BASEYR+1>F12.6)') (PAEPCONB(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("PPTCBUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (PPTCBUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("PRTCBUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (PRTCBUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("RFNPPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (RFNPPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("RAIMUUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (RAIMUUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("SOCMGEN_US",<LASTSTEOYEAR-BASEYR+1>F12.6)') (SOCMGEN_US(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("SNTCBUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (SNTCBUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("UORIPUS",<LASTSTEOYEAR-BASEYR+1>F12.6)') (UORIPUS(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("WNEP_US",<LASTSTEOYEAR-BASEYR+1>F12.6)') (WNEP_US(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("WWCH_US",<LASTSTEOYEAR-BASEYR+1>F12.6)') (WWCH_US(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ZWCD_ENC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ZWCD_ENC(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ZWCD_SAC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ZWCD_SAC(I),I=1,LASTSTEOYEAR-BASEYR+1)
!write(UNIT_STEO_OUT,'("ZWHD_MAC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ZWHD_MAC(I),I=1,LASTSTEOYEAR-BASEYR+1)
 write(UNIT_STEO_OUT,'("ZWHD_WSC",<LASTSTEOYEAR-BASEYR+1>F12.6)') (ZWHD_WSC(I),I=1,LASTSTEOYEAR-BASEYR+1)

!  write out clsteo.txt file:  for now, emulate exact file format
        IF (EXC .EQ. 1) THEN
        EXCOAL=RTOVALUE('EXCOAL  ',0)
        if(excoal.ne.0) then           !  but only if AIMMS coal model is on
            NEW=.false.
            EUNIT = FILE_MGR('O','CLSTEO            ',NEW)
            close(eunit)
            open(eunit,file='coal\clsteo.txt',status='unknown')
            write(eunit,'("  COMPOSITE TABLE:")')
!  writing STEO variables for Coal model in AIMMS
            write(eunit,'("  steoyr  AppalachiaLimit   InteriorLimit   WestLimit  ElecPriceSTEO   ElecTonsSTEO  WasteCoalSTEO  CokeTonsSTEO  IndustrialTonsSTEO CokingExpSTEO SteamExpSTEO ImportsSTEO EOYStocksEPEast EOYStocksEPMidW EOYStocksEPSouth EOYStocksEPWest EOYStocksPrim EOYStocksSecd")')
            write(eunit,'("!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")')
            do I=1990,LASTSTEOYEAR
                write(eunit,'(I6,F16.3,F16.3,F14.3,F13.4,F16.3,F13.2,F16.3,F16.3,F18.3,F13.3,F12.3,F16.3,F16.3,F17.3,F16.3,F14.3,F14.3)') &
                   I, CLPRPAR_TON(I-1989), CLPRPIR_TON(I-1989), CLPRPWR_TON(I-1989),                  &
                      CLEUDUS(I-1989)/STEO_DEF_1987*GDPDIUS(I-1989), CLEPCON_TON(I-1989),             &
                      CLWCPUS_TON(I-1989), CLKCPUS_TON(I-1989), CLYCPUS_TON(I-1989),                  &
                      CLEXPMC_TON(I-1989), CLEXPSC_TON(I-1989), CLIMPUS_TON(I-1989),                  &
                      CLPS_EP_NE(I-1989), CLPS_EP_MW(I-1989), CLPS_EP_SO(I-1989), CLPS_EP_WE(I-1989), &
                      CLSDPUS(I-1989), CLSTPUS(I-1989)
            enddo
            EUNIT = FILE_MGR('C','CLSTEO            ',NEW)
        endif
        ENDIF
!
!  List of variables to write to AIMMS NGMM:
        IF (EXG .EQ. 1) THEN
        EXNGAIM=RTOVALUE('AIMMSNG ',0)
        if(exngaim.ne.0) then           !  but only if AIMMS coal model is on
            NEW=.false.
            EUNIT = FILE_MGR('O','STEOLIST          ',NEW)      !  open STEOLIST file to get unit number
            close(eunit)
            open(eunit,file='ngas\ngsteo.txt',status='unknown')
!  Format to write them in:
!  NGCCU_ENC:= data { 1.0, 1.1, 1.2, etc: put the entire list of data elements separated by comments };
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGCCU_ENC(YEAR)   ", LASTSTEOYEAR-1989,NGCCU_ENC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGCCU_ESC(YEAR)   ", LASTSTEOYEAR-1989,NGCCU_ESC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGCCU_MAC(YEAR)   ", LASTSTEOYEAR-1989,NGCCU_MAC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGCCU_MTN(YEAR)   ", LASTSTEOYEAR-1989,NGCCU_MTN(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGCCU_NEC(YEAR)   ", LASTSTEOYEAR-1989,NGCCU_NEC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGCCU_PAC(YEAR)   ", LASTSTEOYEAR-1989,NGCCU_PAC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGCCU_SAC(YEAR)   ", LASTSTEOYEAR-1989,NGCCU_SAC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGCCU_WNC(YEAR)   ", LASTSTEOYEAR-1989,NGCCU_WNC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGCCU_WSC(YEAR)   ", LASTSTEOYEAR-1989,NGCCU_WSC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGCCUUS(YEAR)     ", LASTSTEOYEAR-1989,NGCCUUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGICU_ENC(YEAR)   ", LASTSTEOYEAR-1989,NGICU_ENC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGICU_ESC(YEAR)   ", LASTSTEOYEAR-1989,NGICU_ESC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGICU_MAC(YEAR)   ", LASTSTEOYEAR-1989,NGICU_MAC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGICU_MTN(YEAR)   ", LASTSTEOYEAR-1989,NGICU_MTN(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGICU_NEC(YEAR)   ", LASTSTEOYEAR-1989,NGICU_NEC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGICU_PAC(YEAR)   ", LASTSTEOYEAR-1989,NGICU_PAC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGICU_SAC(YEAR)   ", LASTSTEOYEAR-1989,NGICU_SAC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGICU_WNC(YEAR)   ", LASTSTEOYEAR-1989,NGICU_WNC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGICU_WSC(YEAR)   ", LASTSTEOYEAR-1989,NGICU_WSC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGICUUS(YEAR)     ", LASTSTEOYEAR-1989,NGICUUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGRCU_ENC(YEAR)   ", LASTSTEOYEAR-1989,NGRCU_ENC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGRCU_ESC(YEAR)   ", LASTSTEOYEAR-1989,NGRCU_ESC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGRCU_MAC(YEAR)   ", LASTSTEOYEAR-1989,NGRCU_MAC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGRCU_MTN(YEAR)   ", LASTSTEOYEAR-1989,NGRCU_MTN(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGRCU_NEC(YEAR)   ", LASTSTEOYEAR-1989,NGRCU_NEC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGRCU_PAC(YEAR)   ", LASTSTEOYEAR-1989,NGRCU_PAC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGRCU_SAC(YEAR)   ", LASTSTEOYEAR-1989,NGRCU_SAC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGRCU_WNC(YEAR)   ", LASTSTEOYEAR-1989,NGRCU_WNC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGRCU_WSC(YEAR)   ", LASTSTEOYEAR-1989,NGRCU_WSC(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGRCUUS(YEAR)     ", LASTSTEOYEAR-1989,NGRCUUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGEUDUS(YEAR)     ", LASTSTEOYEAR-1989,NGEUDUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGHHUUS(YEAR)     ", LASTSTEOYEAR-1989,NGHHUUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGSFPUS(YEAR)     ", LASTSTEOYEAR-1989,NGSFPUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGLPPUS(YEAR)     ", LASTSTEOYEAR-1989,NGLPPUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGACPUS(YEAR)     ", LASTSTEOYEAR-1989,NGACPUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGTCPUS(YEAR)     ", LASTSTEOYEAR-1989,NGTCPUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"BALIT(YEAR)       ", LASTSTEOYEAR-1989,BALIT(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGNWPUS(YEAR)     ", LASTSTEOYEAR-1989,NGNWPUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGEXPUS_LNG(YEAR) ", LASTSTEOYEAR-1989,NGEXPUS_LNG(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGEXPUS_PIPE(YEAR)", LASTSTEOYEAR-1989,NGEXPUS_PIPE(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGEXPUS(YEAR)     ", LASTSTEOYEAR-1989,NGEXPUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGIMPUS(YEAR)     ", LASTSTEOYEAR-1989,NGIMPUS(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGIMPUS_LNG(YEAR) ", LASTSTEOYEAR-1989,NGIMPUS_LNG(1:LASTSTEOYEAR-1989),1990)
CALL WRITE_AIMMS_VARIABLE(EUNIT,"NGIMPUS_PIPE(YEAR)", LASTSTEOYEAR-1989,NGIMPUS_PIPE(1:LASTSTEOYEAR-1989),1990)
            close(eunit)
        endif
        ENDIF

RETURN
END SUBROUTINE READ_ALL_STEO_VARS
SUBROUTINE WRITE_AIMMS_VARIABLE(WUNIT,WSTRING,WNUMNUMS,WNUMS,WSTARTI)
implicit none

!   this is a subroutine to write out variable WSTRING of dimension WNUMNUMS to an AIMMS file unit WUNIT

INTEGER WNUMNUMS         !  number of elements to write
REAL WNUMS(WNUMNUMS)     !  array to be written
INTEGER WUNIT            !  unit to write to
INTEGER WSTARTI          !  identifier start, such as 1 or 1990; i.e., the array position of the first number (MNUMYR can start at 1990 for AIMMS)
CHARACTER*(*) WSTRING    !  variable name
INTEGER I                !  boring local index

write (WUNIT,'(A,A,<WNUMNUMS-1>(I6," : ",F14.5,","),I6," : ",F14.5,A)') trim(WSTRING)," := data { ",((WSTARTI+I-1,WNUMS(I)),I=1,WNUMNUMS)," };"

RETURN
END SUBROUTINE WRITE_AIMMS_VARIABLE

!============================================================================================

! invokes an R procedure to help models benchmark a series to STEO
!  the calling module sends the NEMS series (dimensioned by MNUMYR),
!  the STEO series (1990 through laststeoyear),
!  and an array dimensioned by MNUMYR to receive the spliced series result
!  also specified is splice option 1 (send a 1) or splice option 2 (send a 2)
!  the R program is input file benchmark.r

!  the calling module can either accept default parameters for splicing
!  or write their desired parameters to the file RBM_PARAM.csv (overwriting should be fine)
!  if defaults are desired, this function can be left in charge of setting up the parameter file

!  the R benchmarking program parameter file contains (each of which can be empty):
!      a. Series name
!      b. Option 1 or Option 2, though both are computed and sent back
!      c. Change tolerance multiplier factor (default is 1)
!      d. Alpha parameter for the Holt-Winters projection (default is NULL, R computes)
!      e. Beta parameter for the Holt-Winters projection (default is NULL, R computes)
!      f. Series minimum value (default is series minimum value for NEMS and STEO)
!      g. Series maximum value (default is series maximum value for NEMS and STEO)
!  Valid values for (d) and (e) are between 0 and 1.
!  naming the series and requesting default settings:
!  SERIESNAME,,,,,,      (the series name with 6 commas)

      integer function bench_r(NEMS_series,STEO_series,variable_name,return_series,in_opt)
      implicit none
! invokes an R procedure to
      include 'parametr'
      include 'ncntrl'
      include 'steoblock'

      INTEGER MAX_BENCH, NRPARM
      PARAMETER (MAX_BENCH=400)                 ! maximum number of series that can be kept
      PARAMETER (NRPARM=6)                      ! number of parameters read into R program

      logical found_var, bm_once/.FALSE./, parm_exist

      character*99 rcmd
      character*12 variable_name, keep_track_vars(MAX_BENCH), vn
      character*13 variable_search        ! variable_name followed by ',' to avoid duplicate substrings
      character*20 CHAR2WRITE

      integer bun_opt,in_opt, rret
!  calling program sends the values from the NEMS variable and the values from the STEO variable
      real NEMS_series(MNUMYR), STEO_series(MNUMYR)
!  this is the benchmarked series allowing the calling module to work with it and the original values
      real return_series(MNUMYR)
! these arrays are to store what we did for writing out in one big, incomprehensible file
      real keep_track_NEMS(MAX_BENCH,MNUMYR), keep_track_STEO(MAX_BENCH,MNUMYR)
      real keep_track_back(MAX_BENCH,MNUMYR)
!  COUNT_BENCH counts how many different requests there are, NUM_BENCH is the current request number
      integer COUNT_BENCH, NUM_BENCH, INEMSUNIT, ISTEOUNIT, IBACKUNIT, IPARMUNIT, IBENDBGUNIT, PLINES
      integer SAVE_NUM_BENCH
      integer IBENALLSUNIT, IBENALLNUNIT, IBENALLRUNIT, IBENALLPUNIT
      integer iret, iyr, i
      character*15 RLOC
      character*80 RTERM
      CHARACTER*80 PARM_LINES(MAX_BENCH), LINE

! +++ Variables to Call FILE_MGR for Initialization
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      return_series=0.0
      BENCH_R = 0                       !  this represents a return code
      BUN_OPT = IN_OPT                  !  because it could change, and a non-variable could be sent
      IF (.NOT. BM_ONCE) THEN
         IBENDBGUNIT = FILE_MGR('O','RBENDBG           ',.TRUE.)
         IF (BUN_OPT .NE. 0) THEN 
            write(IBENDBGUNIT,'("  first call to R benchmark routine:  ",a)') trim(variable_name)
            BM_ONCE = .TRUE.
            COUNT_BENCH=0
            PARM_EXIST = .FALSE.
! check if parameter file exists.  perhaps it is asking for trouble to do this only once?
            INQUIRE(FILE='RBM_PARAM.csv',EXIST=PARM_EXIST)
         ENDIF
      ENDIF

      IF (BUN_OPT .NE. 0) THEN                 !  benchmark a series
         IF (BUN_OPT .NE. 2) BUN_OPT = 1       !  default is option 1, 2 is only other choice
         PLINES = 0
         NUM_BENCH = 0
         VARIABLE_SEARCH=trim(variable_name)//','
         DO I=1,MAX_BENCH
            IF (INDEX(keep_track_vars(I),variable_name) .GT. 0) NUM_BENCH = I
         ENDDO
         IF (NUM_BENCH .EQ. 0) THEN            !  if it wasn't already processed, then add it to the list
            COUNT_BENCH = COUNT_BENCH + 1
            IF (NUM_BENCH .GT. MAX_BENCH) THEN
               write(6,'(" Preventing subscript out of bounds.  More requests than MAX_BENCH:  ",A,2I4)') trim(variable_name),NUM_BENCH,MAX_BENCH
               COUNT_BENCH=MAX_BENCH
            ENDIF
            NUM_BENCH = COUNT_BENCH
            KEEP_TRACK_VARS(COUNT_BENCH) = variable_name
         ENDIF
! create the input files to the external R program, one for NEMS data, one for STEO data
         INEMSUNIT = FILE_MGR('O','NEMS_TO_R         ',.TRUE.)
         ISTEOUNIT = FILE_MGR('O','STEO_TO_R         ',.TRUE.)

!  see if variable_name is in the parameter file.  first, see if parameter file exists
         IF (PARM_EXIST) THEN
            IPARMUNIT = FILE_MGR('O','RBM_PARAM         ',.FALSE.)
            REWIND IPARMUNIT
            FOUND_VAR = .FALSE.
            DO WHILE (.NOT. FOUND_VAR)
               READ(IPARMUNIT,'(A80)',END=9696,ERR=9696) LINE
               write(IBENDBGUNIT,'(" Looking for string:  ",A," in ",A)') trim(variable_search),trim(line)
               PLINES=PLINES+1
               IF (INDEX(LINE,trim(variable_search)) .GT. 0) THEN
!  this means that the variable_name sent via argument matches the variable name in the parameter file
                  FOUND_VAR = .TRUE.
                  PARM_LINES(NUM_BENCH) = LINE
                  write(IBENDBGUNIT,'(" Benchmark routine.  Found ",a," in parameter file!  Parameter request=",a)') trim(variable_name),trim(PARM_LINES(NUM_BENCH))
               ENDIF
            ENDDO
            IPARMUNIT = FILE_MGR('C','RBM_PARAM         ',.FALSE.)
         ENDIF
9696     CONTINUE
!  if the variable name sent via argument does not match what is in parameter file if there is one
         IF (.NOT. FOUND_VAR .OR. .NOT. PARM_EXIST) THEN
            IF (BUN_OPT .EQ. 2) THEN
               PARM_LINES(NUM_BENCH) = TRIM(variable_name) // ",2,,,,,"
            ELSE
               PARM_LINES(NUM_BENCH) = TRIM(variable_name) // ",,,,,,"
            ENDIF
!     if NOT already in file, add with default (blank) settings for each option
            IF (.NOT. PARM_EXIST) THEN
               IPARMUNIT = FILE_MGR('O','RBM_PARAM         ',.TRUE.)
               WRITE(IPARMUNIT,'(a)') "SeriesName,Option,ChMult,alpha,beta,SeriesMin,SeriesMax"
               PARM_EXIST=.TRUE.
            ENDIF
            IF (.NOT. FOUND_VAR) THEN
               write(IPARMUNIT,'(a)') trim(PARM_LINES(NUM_BENCH))
            ENDIF
            IPARMUNIT = FILE_MGR('C','RBM_PARAM         ',.FALSE.)
         ENDIF

         WRITE(INEMSUNIT,'("Year,",a)') trim(variable_name)
         WRITE(ISTEOUNIT,'("Year,",a)') trim(variable_name)
         DO IYR = 1, MNUMYR
            IF (.NOT. ISNAN(NEMS_series(iyr))) THEN
               write(CHAR2WRITE,'(F20.6)') NEMS_series(iyr)
               WRITE(INEMSUNIT,'(I4,a,a)') IYR+1989,",",TRIM(ADJUSTL(CHAR2WRITE))
            ENDIF
            write(CHAR2WRITE,'(F20.6)') STEO_series(iyr)
            IF (IYR .LE. LASTSTEOYEAR-1989) WRITE(ISTEOUNIT,'(I4,a,a)') IYR+1989,",",TRIM(ADJUSTL(CHAR2WRITE))
         ENDDO

         INEMSUNIT = FILE_MGR('C','NEMS_TO_R         ',.FALSE.)
         ISTEOUNIT = FILE_MGR('C','STEO_TO_R         ',.FALSE.)

    ! create command like this to invoke R using the RTERM executable:
    !
    !          rterm.exe -e "RunName <- 'QNGRS'" -e "source('input/benchmark.r')"
    !
         call RTOSTRING('RLOC    ',RLOC)        !  R version node, such as 'R-2.15.1'
         RTERM = 'C:\R\' // trim(RLOC) // '\bin\x64\RTerm.exe'
         rcmd=trim(RTERM) // ' -e "RunName <- '''  //  trim(variable_name)  // '''" -e "source(''input/benchmark.r'')"  '
         write(IBENDBGUNIT,'(a,a)') "command:  ",trim(rcmd)
         call callsys(IRET,rcmd)

!  if it worked (iret equal to 0), then read the return file
         if (IRET .eq. 0) then
            IF (BUN_OPT .EQ. 1) THEN
               rcmd='cp R_TO_NEMS1.csv R_TO_NEMS.csv'
            ELSE
               rcmd='cp R_TO_NEMS2.csv R_TO_NEMS.csv'
            ENDIF
            call callsys(IRET,rcmd)
            IBACKUNIT = FILE_MGR('O','R_TO_NEMS         ',.FALSE.)
            read(IBACKUNIT,'(1x)')                         ! skip header line
            do while (.not. eof(IBACKUNIT))
               read(IBACKUNIT,*) iyr, return_series(iyr-1989)
               keep_track_nems(num_bench,iyr-1989) = NEMS_series(iyr-1989)
               keep_track_steo(num_bench,iyr-1989) = STEO_series(iyr-1989)
               keep_track_back(num_bench,iyr-1989) = return_series(iyr-1989)
               keep_track_vars(num_bench) = variable_name
            enddo
            IBACKUNIT = FILE_MGR('C','R_TO_NEMS         ',.FALSE.)
         else
            write(6,'(a,a,a,I2)') "  Attempted ",trim(variable_name)," benchmarking failed!  Code=",iret
            return_series = NEMS_series           !  for safety, set what is returned to what was sent
            BENCH_R = 1
         endif

! do this so subsequent invocation writes over file
         REWIND INEMSUNIT
         REWIND ISTEOUNIT
         IF (PLINES .LE. 2) CLOSE(IPARMUNIT,STATUS='DELETE')

      ELSE                             ! write a summary report file
         IF (COUNT_BENCH .GT. 0) THEN    ! but only if there was a request

            IBENALLSUNIT = FILE_MGR('O','RBENALLS          ',.TRUE.)
            IBENALLNUNIT = FILE_MGR('O','RBENALLN          ',.TRUE.)
            IBENALLRUNIT = FILE_MGR('O','RBENALLR          ',.TRUE.)
            IBENALLPUNIT = FILE_MGR('O','RBENALLP          ',.TRUE.)

            WRITE(IBENALLSUNIT,'(a,<COUNT_BENCH>(",",a))') "Year",(keep_track_vars(I),I=1,COUNT_BENCH)
            WRITE(IBENALLNUNIT,'(a,<COUNT_BENCH>(",",a))') "Year",(keep_track_vars(I),I=1,COUNT_BENCH)
            WRITE(IBENALLRUNIT,'(a,<COUNT_BENCH>(",",a))') "Year",(keep_track_vars(I),I=1,COUNT_BENCH)
            WRITE(IBENALLPUNIT,'(a)') "SeriesName,Option,ChMult,alpha,beta,SeriesMin,SeriesMax"
            DO IYR = 1, MNUMYR
               WRITE(IBENALLSUNIT,'(I4,<COUNT_BENCH>(",",F12.4))') IYR+1989,(keep_track_steo(I,IYR),I=1,COUNT_BENCH)
               WRITE(IBENALLNUNIT,'(I4,<COUNT_BENCH>(",",F12.4))') IYR+1989,(keep_track_nems(I,IYR),I=1,COUNT_BENCH)
               WRITE(IBENALLRUNIT,'(I4,<COUNT_BENCH>(",",F12.4))') IYR+1989,(keep_track_back(I,IYR),I=1,COUNT_BENCH)
            ENDDO
            DO I = 1, COUNT_BENCH
               WRITE(IBENALLPUNIT,'(a)') trim(PARM_LINES(I))
            ENDDO

            IBENALLSUNIT = FILE_MGR('C','RBENALLS          ',.FALSE.)
            IBENALLNUNIT = FILE_MGR('C','RBENALLN          ',.FALSE.)
            IBENALLRUNIT = FILE_MGR('C','RBENALLR          ',.FALSE.)
            IBENALLPUNIT = FILE_MGR('C','RBENALLP          ',.FALSE.)
         ELSE
            WRITE(IBENDBGUNIT,'("The R benchmark routine was not called during this NEMS run")')
         ENDIF

      ENDIF

      RETURN
      END
