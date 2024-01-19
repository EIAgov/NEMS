! $Header: M:/default/source/RCS/tfiler.f,v 1.31 2018/04/04 17:35:48 dsa Exp $
      PROGRAM TFILER
      use ifport,only:timef, sleepqq
      implicit none
! FRONT-END PROGRAM TO EXECUTE FILER STANDALONE.  READS
! DICTIONARY, READS A RESTART FILE, AND EXECUTES AN OUTPUT REQUEST
! FROM A USER-SPECIFIED FILE.

! Tfiler can also be executed by NEMS scripts to run the buildings
! modules. This is implemented through a signal file (nems.signal) with entries
! to designate which module to run, and a command line option ("1") to remain open
! after executing. (eg. "tfiler.exe 1")
!
! To have tfiler execute the RESD and/or the COMM modules, the entry in nems.signal 
! should be "RESD", "COMM", or "BUILD" to run one (RESD or COMM) or both (BUILD) of the
! buildings demand modules. 
!
! After processing input/output request in tfiler.files, tfiler will replace the entry in
! nems.signal with "NEMS" to signal to NEMS to resume and read the restart file created by tfiler.
!
! TFILER then stays open until it receives a "QUIT" signal, via nems.signal, or a signal to run again
! (TFILER, RESD, COMM, or BUILD). It remains open because RESD and COMM need to retain intermediate
! calculations for vintaging and stocks as it moves through the modeled time horizon.  Also, 
! keeping the AIMMS module open reduces overhead for opening and closing the project repeatedly
! through the iterative NEMS executions.
!
!  Normally, when executed independently of NEMS, tfiler will execute once (by reading the tfiler.files 
!  settings once, then exit.

      include 'parametr'
      include 'aimms_c_face'
      include 'fdict'
      include 'ncntrl'
      
      INTEGER*4 FUNITI,FUNITO,FRETCD,FRTYPE,FSOURC,funfmt,fmt,infmt,I,L
      integer*4 leng
      external leng
      CHARACTER*11 trash
      character*5 argument/'0    '/
      integer*4 nargument,iargument, O/6/
      logical stayopen/.false./
      character*6 tsignal
! gdx-related variables      
      logical ok,gdxCreate,gdxClose
      external gdxCreate,gdxClose, gdxGetDLLVersion, gdxOpenRead, gdxOpenWrite, gdxOpenWriteEx
      integer(KIND=4) gdxGetDLLVersion, gdxOpenRead, gdxOpenWrite, gdxOpenWriteEx
      integer(KIND=4) errnr
      character*100 errMsg
      integer(kind=8) pgdx
      COMMON/GDX/pgdx
      
! other declarations
     logical lexist,no_aimms/.false./,aimms_error
     integer(KIND=4) ret,procedureRet,iret
     character*80 projectName,aimmsFolder

      real*4 timer,timer2,mtimer
!--------------------------      
      character*130 infile
      CHARACTER*260 line,column_title
      CHARACTER*260 dict,restarti,var,restarto,restart2,dict2
      character*132 TFILER_LINE
      character*80 blnkfile
      CHARACTER*100 FNAMEI,FNAMEO
      CHARACTER ANS
      character*80 filervers,rundate,FYearLine
! vars for csv output file closing !
      integer irecl,irow           !
      character*1 nul              !
      parameter (irecl=400)        !
      character*irecl longline     !
!----------------------------------!
!  Variables to Call FILE_MGR for Initialization
      CHARACTER*18 FM_NAME/' '/
      INTEGER ISTATUS
      LOGICAL NEW/.true./
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
!----------------------------------!
      integer ncount/0/ ! count errors trying to open nems.signal
    
      integer do_cycle/0/ ! 1 signals control to pass to end of the do-while on return from subroutine

    logical quiet/.false./  ! if true, turns off console output when reading tfiler.files

 ! get optional integer argument.  If nonzero, stay open and wait for tfiler.files to appear. 
 ! After tfiler.files processed, stay open.
 infile='tfiler.files'
 nargument=nargs()
 iargument=0
 if(nargument.ge.2) then
   call getarg(1,argument)
   read(argument,'(bn,i5)',err=9,end=9) iargument
9  if(iargument.ne.0) stayopen=.true.
 endif
 if(nargument.ge.3) then
   call getarg(2,infile)
   inquire(file=infile,exist=lexist)
   if(.not.lexist) then
      write(6,'(a)') '2nd argument for input file found, but file not found: ',trim(infile)
      write(6,'(a)') 'Trying default file name, tfiler.files'
      infile='tfiler.files'
   else
      write(6,'(a)') '2nd argument for alternate input file found. Will read input from ',trim(infile),' instead of tfiler.files'
   endif
 endif
 
 
 
 if(stayopen) then
   O=15
   open(15,file='tfiler.out',status='unknown')
   write(O,*)'TFiler: stay open option'
   
! +++ Call FILE_MGR First Time to Open up and Read the List of Files for possibly running NEMS modules
      new=.false.  ! will cause it to open fmgrout.tfiler.txt rather than fmgrout.txt
      ISTATUS=FILE_MGR('I',FM_NAME,NEW)
      new=.true.
 else
   O=6
   write(O,*)'TFiler'
 endif

  quiet=.false.

tsignal='TFILER'  ! this is the signal to run TFILER
do while (tsignal .ne. 'QUIT' ) 

  if(stayopen) then  ! then read the signal file to see if NEMS wants TFILER to go.
    inquire(file='nems.signal',exist=lexist)
    if(lexist) then
      open(12,file='nems.signal',status='old',readonly,err=5)
      read(12,'(a)',err=3,end=3) tsignal
3     continue
      close(12,err=5)
5     continue
    else
      tsignal='QUIT'
    endif 
    if(tsignal.eq.'QUIT') cycle 
  endif
  
  if(tsignal.eq.'NEMS ' .and. stayopen) then
  
    !call sleep(1)  
    call sleepqq(100) ! sleep for .1 second until next check.
  else 
  
    if(.not. stayopen)then
      tsignal='QUIT'
    endif
    timer=timef()
    timer2=timer
    open(8,file=infile,status='old',readonly)
      
    Do while (.not. eof(8))

      do_cycle=0
      
      Call Read_Tfiler_Files
      
      if(do_cycle) cycle ! go to end of do-while
      
      
      call Input_Request
      
      if(istatus .ge.0) then ! ISTATUS of 0 from file_mgr initialization
        if(tsignal.eq.'RESD'.or.tsignal.eq.'BUILD') then
         write(O,*)'    Calling RESD, CurCalYr, CurItr=',CurCalYr,CurItr
         write(6,*)'Calling RESD, CurCalYr, CurItr=',CurCalYr,CurItr
         mtimer=timef()
         call resd
         write(6,*)'Back from RESD, elapsed time=',timef()-mtimer
        endif
        if(tsignal.eq.'COMM'.or.tsignal.eq.'BUILD') then
          write(O,*)'    Calling COMM, CurCalYr, CurItr=',CurCalYr,CurItr
          write(6,*)'Calling COMM, CurCalYr, CurItr=',CurCalYr,CurItr
          mtimer=timef()
          call comm
          write(6,*)'Back from COMM, elapsed time=',timef()-mtimer
        endif
      endif
      
      call Output_Request
      
    end do !  of while .not. eof(8)
    close(8) ! ' tfiler.files'
    
    write(O,'(a,f8.1)') '   Total TFiler time (seconds)=',timef()-timer2
 
    ncount=0
    do while(stayopen.and.ncount.lt.200) 
    ! send NEMS a signal to resume'
      open(2,file='nems.signal',status='unknown',SHARE='denynone',SHARED,ACTION='readwrite',err=56)
      tsignal='NEMS'
      write(2,'(a)',err=55) tsignal
55    continue
      close(2)
      ncount=200
56    ncount=ncount+1
      if(ncount.eq.200) then
        write(6,*) 'ERROR.  could not open nems.signal file to update status after 200 tries.'
        write(6,*) 'might as well stop'
        stop ' '
      endif
    enddo    
  endif
  
  
enddo

write(O,*)':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::'


STOP 'Tfiler end '

CONTAINS
! returns part of string after delimiter
! used to get the value after a key in file
FUNCTION split_string(instring, delim)
  CHARACTER(*) :: instring,delim
  CHARACTER(260) :: split_string
  INTEGER :: index

  instring = TRIM(instring)

  index = SCAN(instring,delim)
  split_string = instring(index+1:)

END FUNCTION split_string

Subroutine Read_Tfiler_Files

      if(.not.quiet) then
      write(O,*)':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::'
      write(O,*)'  Reading '//trim(infile)//' to get file names and options:'
      endif

      read(8,'(a)') TFILER_LINE   ! can now run through multiple tfiler operations by separating instances with a line containing "TFILER" 
      if(index(TFILER_LINE,'TFILER').eq.0) then         
        backspace(8)
      endif
      
      if(.not. eof(8)) read(8,'(A)') line
      dict = split_string(line, ' ')
      write(*,*) line
      if(.not. eof(8)) read(8,'(A)') line
      restarti = split_string(line, ' ')
      if(.not. eof(8)) read(8,'(11x,i1)') infmt
      if(.not. eof(8)) read(8,'(A)') line
      var = split_string(line, ' ')
      if(.not. eof(8)) read(8,'(A)') line
      restarto = split_string(line, ' ')
      if(.not. eof(8)) read(8,'(11x,i1)') fmt
      if(eof(8)) then
        do_cycle=1
        return
      endif
      restart2=' '
      dict2=' '
      rundate=' '
      filervers=' '
      FYearLine=' '
      FYearSubset=0
      FYearStart=0
      FYearEnd=0
      TFILER_LINE=' '
      if(.not. eof(8)) read(8,'(11x,a)') filervers
      if(.not. eof(8)) read(8,'(a)') rundate
      if(.not. eof(8)) read(8,'(a)') FYearline
      if(.not.quiet) then
        write(O,'(a,a)') '     Dict.txt:            ',dict
        write(O,'(a,a)') '     Restart input:       ',restarti
        write(O,'(a,i1)')'       In format:         ',infmt
        write(O,'(a,a)') '     Output request:      ',var
        write(O,'(a,a)') '     Restart output:      ',restarto
        write(O,'(a,i1)')'       Out format:        ',fmt
        write(O,'(a,a)') '     Filer version:       ',trim(filervers)
        write(O,'(a,a)') '     Set-up date:         ',trim(rundate)
        write(O,'(a,a)') '     FYear slice options: ',trim(FYearline)
      endif
      if(index(FYearLine,'FYearSubset=1').gt.0)then
         i=index(FYearLine,'FYearStart=')
         if(i.gt.0) then
            i=i+11
            read(Fyearline(i:i+1),'(i2)') FYearStart
         endif
         i=index(FYearLine,'FYearEnd=')
         if(i.gt.0) then
            i=i+9
            read(Fyearline(i:i+1),'(i2)') FYearEnd
         endif
         if(FYearstart.gt.0 .and. FYearend.gt.0 .and. FYearEnd.ge.FYearStart .and. FYearEnd.le.mnumyr) then
            FYearSubset=1
         endif
      endif
      if(.not.quiet) then
        write(O,'(a,i2)') '     FYearSubset Option: ',FYearSubset
        write(O,'(a,i2)') '     FYearStart:         ',FYearStart
        write(O,'(a,i2)') '     FYearEnd:           ',FYearEnd
      endif

!      see if there is an optional secondary restart file
      if(.not. eof(8)) read(8,'(a)') TFILER_LINE
      if(index(TFILER_LINE,'TFILER').gt.0 .or. len_trim(TFILER_LINE).eq.0) then
!        write(O,*) '    Reading tfiler.files, TFILER_LINE=',trim(tfiler_line)
      else
        if(.not.quiet) then
          write(O,*) '    Reading '//trim(infile)//', extra restart file, TFILER_LINE=',trim(tfiler_line)
        endif
         restart2=TFILER_LINE

!      see if there is an optional secondary dict file (to write out data with different dimensions, etc.
         if(.not. eof(8)) read(8,'(a)') TFILER_LINE
         if(index(TFILER_LINE,'TFILER').gt.0 .or. len_trim(TFILER_LINE).eq.0) then
         else
           if(.not.quiet) then
             write(O,*) '    Reading '//trim(infile)//', extra dict.txt file, TFILER_LINE=',trim(tfiler_line)
           endif
           dict2=TFILER_LINE
         endif      
      endif
  return

End Subroutine Read_Tfiler_files
!===================================================================================================================

Subroutine Input_Request
      logical lopened
      write(O,*) ' '
      write(O,*) '  Processing TFILER request'
      write(O,*) '    Reading dictionary file: ',trim(dict)
      write(O,*) '    Dictionary debug file: dictout.txt'
! READ DICTIONARY
      FRTYPE=3
      FRETCD=0
      FUNITI=1
      FNAMEI=' '
      open(1,file=dict,status='old') 
      FUNITO=12
      FNAMEO='dictout.txt'
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD, &
       FUNFMT)



      write(O,*) '    Opening, reading input: ',restarti,' infmt=',infmt

      funito=2
! READ RESTART FILE
      FRTYPE=2
      FSOURC=0
       
!**************
      FNAMEI=' '
      if(infmt.eq.0) then
        open(1,file=restarti,status='old')
        FUNFMT=0
      elseif(infmt.eq.7) then
           open(1,file=restarti,status='old')
           funfmt=infmt
      elseif(infmt.eq.1) then
        open(1,file=restarti,status='old',form='unformatted', &
         CONVERT='Big_Endian',recl=16384,access='sequential')
        FUNFMT=1  
      elseif(infmt.eq.5) then
        FUNFMT=infmt
       !create a pointer to the gdx object
        ok = gdxCreate(pgdx,errMsg)
        IF(.not.ok) then
          write(O,*) 'Error in gdxCreate'
          write(O,*) errMsg
          stop ! use return in subroutine
        ENDIF

        ok = gdxOpenRead(pgdx,restarti,ErrNr)
        IF(.not.ok) then
          write(O,*) 'Error in gdxOpenRead'
          write(O,*) 'error number: ',errNr
          stop ! return in subroutine
        ENDIF
! use varlist file to control which variables are read from GDX file
        open(10,file=var,status='old')
        rewind(10)  ! if file is open already, the open statement leaves file position the same, like at end of file. this resets the file position at the beginning. 
        FUNITO=10
      endif

! Process restart input request (returns if gdx format)
      FNAMEO=' '
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      inquire(unit=funito,opened=lopened)
      if(lopened) close(funito)
      inquire(unit=funiti,opened=lopened)
      if(lopened) close(funiti)
! 
! if there is an option secondary restart file, open it and read
! it in.  It is assumed to be formatted and would be used
! to input additional data , as when we are overwriting or
! adding new variables.  
331   continue
      do while (restart2.ne.' ') 

        write(O,*) ' Additional restart file to be opened: ',restart2
        FNAMEI=' '
        FUNFMT=0  ! assumes secondary restart file is text/formatted
        if(index(restart2,'.unf').gt.0) then
          funfmt=1  ! if ends in .unf, assume unformatted
          open(1,file=restart2,status='old',form='unformatted', &
          CONVERT='Big_Endian',recl=16384,access='sequential')
          FUNFMT=1  

          funiti=1
        else
          fnamei=' '
          funfmt=0
          open(1,file=restart2,status='old')
          funiti=1
        endif
        FNAMEO=' '
        CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
        close (1,status='keep')
        restart2= ' '
        TFILER_LINE=' '
        if(.not. eof(8)) then
          read(8,'(a)') TFILER_LINE
          write(O,'(a)') 'Reading '//trim(infile)//':',trim(TFILER_LINE)
        endif
        if(index(TFILER_LINE,'TFILER').eq.0 .and. len_trim(TFILER_LINE).gt.0) then
          restart2=TFILER_LINE
        endif

      end do
      
          
      
      if(dict2.ne.' ') then
         write(O,*) '    Secondary dictionary file to be opened: ', dict2
        FRTYPE=3
        FRETCD=0
        FUNITI=1
        FNAMEI=' '
        open(1,file=dict2,status='old') 
        FUNITO=12
        FNAMEO='dictout.txt'
        CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      endif
         
      write(O,'(a,f8.1)') '     Input time (seconds)=',timef()-timer
      timer=timef()

   return
   
end subroutine Input_request
!===================================================================================================================

Subroutine Output_Request
      logical lopened
      write(O,*) '    Output request from: ',trim(var)
      write(O,*) '    Output file        :   ',trim(restarto)
      
            
! write out variables listed in output request file
      FRTYPE=1
      FSOURC=1
      FNAMEI=' '
      open(1,file=var,status='old')
      FUNITI=1
      FUNITO=2
      FNAMEO=' '
      FUNFMT=fmt
      if(funfmt.eq.1) then
        open(2,file=restarto,form='unformatted',status='unknown', &
         CONVERT='Big_Endian',buffered='YES')
      elseif(funfmt.eq.0) then
        open(2,file=restarto,form='formatted',status='unknown',recl=512,buffered='YES')
      elseif(funfmt.eq.3) then
        l=len_trim(restarto)
        blnkfile=restarto(1:l)
        do i=l+1,len(restarto)
          blnkfile(i:i)=char(0)
        enddo 
        write(O,*) ' blnkfile=',trim(blnkfile)
        call wkopen(funito,blnkfile)
      ELSEIF(FUNFMT.EQ.4 ) THEN ! CSV
         FNAMEO=RESTARTO
      ELSEIF(FUNFMT.EQ.5) then ! GDX
         FNAMEO=' '  
        !create the gdx file
        ok = gdxCreate(pgdx,errMsg)
        IF(.not.ok) then
          write(O,*) 'Error in gdxCreate: '//trim(errMsg)
          write(O,*) errMsg
          stop 
        ENDIF
!       Return the version of GAMS we're calling
        errMsg=' '
        ok = gdxgetdllversion(pgdx, errMsg)
        write(6,*) 'gams dll version ',trim(errMsg)
        
        
        ok = gdxOpenWrite(pgdx,restarto,'filer.f',ErrNr)
        if(.not.ok) then
          write(O,*) 'Error in gdxCreate: ',ErrNr
          stop
        endif
        
      elseif(funfmt.eq.7) then ! AIMMS composite table format text file
         open(2,file=restarto,form='formatted',status='unknown',buffered='YES')
      endif
      
! perform output request      
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      inquire(unit=funito,opened=lopened)
      if(lopened) close(funito)
      inquire(unit=funiti,opened=lopened)
      if(lopened) close(funiti)
      
      if(funfmt.eq.3) then
        call wkclose(funito)
      endif
      if(funfmt.eq.4) then
        OPEN(UNIT=funito,FILE=fnameo,FORM='FORMATTED',ACCESS='DIRECT',recl=irecl,buffered='yes')

         irow=1
         read(funito,'(a)',rec=irow) longline
         nul=char(0)
         do while (.not. eof(funito))
            read(funito,'(a)',rec=irow,err=999) longline
            do i=1,irecl-2
              if(longline(i:i).eq.nul) then
                longline(i:i)=' '
              endif
            enddo
            do i=irecl-2,1,-1
              if(longline(i:i).ne.','.and.longline(i:i).ne.' ') then
!                if(i.lt.irecl-2) longline(i+1:i+1)=','
                exit
              else
                if(longline(i:i).eq.',') then
                  longline(i:i)=' '
                endif
              endif 
            enddo
            longline(irecl-1:irecl)=char(13)//char(10)
!            if(len_trim(longline(:irecl-2)).eq.0) then
!              longline(1:4)='" ",'
!            endif
            write(funito,'(a)',rec=irow) longline
999         continue  
            irow=irow+1 
         enddo
       CLOSE(funito)
       OPEN(UNIT=funito,FILE=fnameo,FORM='FORMATTED',status='unknown')
       rewind funito
       OPEN(UNIT=funito+20,FILE=trim(fnameo)//'2',FORM='FORMATTED',buffered='YES')
         do while (.not. eof(funito))
           read(funito,'(a)') longline
           write(funito+20,'(a)') trim(longline)
         enddo
       close(unit=funito)
       OPEN(UNIT=funito,FILE=fnameo,FORM='FORMATTED',status='unknown')
       rewind funito
       rewind(funito+20)
         do while (.not. eof(funito+20))
           read(funito+20,'(a)') longline
           write(funito,'(a)') trim(longline)
         enddo
         close(funito+20,status='delete')
         close(funito)        
      endif
      if(funfmt.eq.5) then
        ok = gdxClose(pgdx,errMsg)
      endif
  
      write(O,'(a,f8.1)') '     Output time (seconds)=',timef()-timer
      timer=timef()
      

   return

End Subroutine Output_Request
!===================================================================================================================

END program tfiler
!===================================================================================================================

      FUNCTION RTOVALUE(RTONAME,RTODEFVAL)

! ---------------------------------------------------------------*

!   THIS FUNCTION SEARCHES THE MOREOPT UNIT IN THE JCL FOR THE   *
!   RUN TIME OPTION (RTO) NAME SENT IN THE FUNCTION INVOCATION,  *
!   AND RETURNS THE VALUE SET FOR THE RUN.  IF NOT FOUND, THE    *
!   FUNCTION RETURNS 9999.  THE UNIT IS REWOUND AT THE END.      *

! ---------------------------------------------------------------*

      IMPLICIT NONE
      INTEGER UNIT,VVALUE,RTOVALUE,FILE_MGR,RTODEFVAL
      CHARACTER FILENM*18,RTONAME*8
      INTEGER MAXRTOPTS,NUMRTOPTS,I
      PARAMETER (MAXRTOPTS=200)
      CHARACTER*8 RTOPTS(MAXRTOPTS),RESTOFLINE*66
      INTEGER RTOPTSV(MAXRTOPTS),OPT_USED(MAXRTOPTS),IFOUND
      LOGICAL READYET/.FALSE./
      LOGICAL NEW,FINDRTO
      EXTERNAL FILE_MGR
      COMMON /RTOOPTIONS/ RTOPTS,RTOPTSV
      FINDRTO=.FALSE.
      IF (.NOT. READYET) THEN
         READYET=.TRUE.
         NEW=.FALSE.
         FILENM = 'MOREOPT'
         UNIT = FILE_MGR('O',FILENM,NEW)
         NUMRTOPTS=1
         WRITE(6,*) '##Integer Runtime Options File'
         WRITE(6,*) '##    Name   Value  Description'
         WRITE(6,*) '##========   ==============================='
10       CONTINUE
         READ(UNIT,900,END=100)RTOPTS(NUMRTOPTS),RTOPTSV(NUMRTOPTS),RESTOFLINE
900      FORMAT(A8,1X,I4,1X,A)
         WRITE(6,'(1X,A2,A8,I8,A)')'##', &
                   RTOPTS(NUMRTOPTS),RTOPTSV(NUMRTOPTS),RESTOFLINE
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
!===================================================================================================================

      subroutine unitunopened(start,end,iunit)
! Find next unopened input/output unit beginning with start, ending with end. Return iunit as the unit number.
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
!===================================================================================================================

      subroutine callsys(iret,cmd)
! call the system command, cmd, to invoke a child process and wait for it to return. The return code from the
! sysetm command is iret.
      use dfport
      character*(*) cmd
      integer iret
      write(6,*) ' Calling system to do this:  ',cmd
      iret=system(cmd)
      if(iret.ne.0) then
        write(6,'(a,I2,a,a)') '   Command failed with return code of ',iret,':  ',cmd
      endif
      return
      end
