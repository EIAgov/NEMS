! Oscillate:  reads nems/ftab ran files from a series of nems cycles.  
! Calculates correlations for each row across successive and every-other cycles.  Searches for
! an oscillation pattern on each row.  Writes message for table/rows with the oscillation pattern evident.
! outputs a csv file with all the results.
!
! data declaration module r_vars stores common block declarations to avoid repeating and avoid include files
 module r_vars
    implicit none
    
    integer maxcyc
    parameter (maxcyc=6)

    integer mTab,mRow
    parameter (mTab=500) ! maximum number of tables stored in ran
    parameter (mRow=400) ! maximum number of rows on any single table
   
 
 ! RANVar1   
   real*4    RGData(56,mTab,mRow,maxcyc+1)
   integer*4 DataRow(mTab,mRow) ! data row indicator: 1: yes, 0: no 
   integer*4 TotNote(mTab)
   common/RANVar1/RGData,DataRow,TotNote
   
 ! RANVar2   
   character*120  TabTitle(mTab,4)
   character*80   RowTitle(mTab,mRow)
   character*200  FootNote(mTab,100)
   character*128  Subject(mTab,4)

   common/RANVar2/TabTitle,RowTitle,FootNote,Subject
 
 ! RANVar3  Variables in headers.
   common/RANVar3/RSFile,RSScen

  
! Ran file Main header 
   character*4   RMVer             ! main version keyword for version detection
   integer*4     RMLen             ! length of the main header
   integer*4     RSLen             ! length of the scenario header
   integer*4     RTLen             ! Length of the table location header
   integer*4     RDLen             ! length of the data/row location header

! Scenario Header
   character*4   RSVer             ! main version keyword for version detection
   integer*2     RSFLen            ! text length of the restart file name
   character*150 RSFile            ! restart file name
   integer*2     RSSLen            ! scenario description text length
   character*150 RSScen            ! scenario description text

! Table Location Header   
   integer*2     RTNum             ! total number of tables (appears just once)
   character*6   RTKey(mTab)       ! Table keyword
   integer*2     RTTRow(mTab)      ! total number of all types of rows for this table
   integer*2     RTDRow(mTab)      ! total number of data rows for this table
   integer*4     RTDLoc(mTab)      ! file data section starting byte location for all rows for this table
   integer*4     RTILoc(mTab)      ! file index data section starting byte lcoation for data rows for this table
   
! Data Row Location Header   
   character*16  RDKey(mTab,500)   ! data row keyword: a unique string identifying this row
   integer*4     RDLoc(mTab,500)   ! data row byte location in the data section

! General Data Section
   integer*2     RGType            ! record type (1:heading, 2:floating row, 3:datarow, 4:footnote)
   integer*2     RGSTyp            ! Sub record type (not used)
   integer*2     RGLRem            ! Length of remainder of record:, i.e., not including first 3 fields
   integer*2     RGTNum            ! Sequence number of tables, incremented for each U.S. table and regional table 
   character*6   RGTKey(mTab)      ! table keyword 
   integer*2     RGTAct(mTab)      ! Actual table number as defined in ftab (all regional tables have the same actual table number in ftab)
   integer*2     RGTReg(mTab)      ! region number: 0 for national table, or >0 for regional instances of tables
   character*6   RGForm(mTab,mRow) ! formatting string to specify bolding/plain, indent, and bonus/not.
   integer*2     RGRNum            ! data row sequence number for all rows, whether data rows or not
   character*16  RGRKey(mTab,mRow) ! data row keyword: a unique (within table) string identifying this row
   integer*2     RGTTyp            ! title record type (1,2, or 3 for main, subheading, ..)
   integer*2     RGTLen            ! length of a title string
   character*800 RGText            ! Text on rows or title
   integer*2     RGSLen            ! length of the subject field

   integer*2     RGDTyp(mTab,mRow) ! row data format or type: 1-8 for digits to right of decimal, or 9 for integer format
   integer*2     RGDFYr(mTab,mRow) ! first year of row data (usually 1995)
   integer*2     RGDLYr(mTab,mRow) ! last year of row data (usually 2050)
   integer*2     LenRowStr(8)      ! length of 8 row strings
   character(LEN=1500),allocatable :: RowStrings(:,:,:) ! RowStrings(8,mTab,mRow)     ! additional descriptive strings
 !  common/RANVar4/ RMLen,RSLen,RTLen,RDLen,RTDLoc,RTILoc,RDLoc, &
  !  RSFLen,RSSLen,RTNum,RTTRow,RTDRow, &
 !   RMVer,RSVer,RDKey,RTKey,errplace  
  
 !  common/RANVar5/RGType,RGSTyp,RGLRem,RGTNum,RGTAct,RGTReg,RGRNum,RGTTyp,RGTLen,RGDTyp,RGDFYr,RGDLYr, &
 !   RGForm,RGTKey,RGRKey

   integer*4 ncyc ! number of cycles and input files
   character*256 InFileNamet(20),InFileName(7),OutFileName
  ! common/files/InFileName,OutFileName,ncyc
   character*100 errplace  


   real :: RMSDPctAve(7,2)   ! do sum, then average, of all rows as possible metric
   integer :: RMSDPctCt(7,2) ! count of all rows
   
   integer, parameter :: maxotabs=150
   integer :: otabs(maxotabs),notabs
   
   
end module r_vars
!==================================================================================================
Program Oscillate
  use r_vars
  implicit none
   integer oscor
   common/score/oscor
!
    character*150 retry
    logical lexist
    integer basename,ftabbone

    integer TLen,y
    character*200 TextLine

   integer Align,Apper,Cont,Skip
   real PtSize,IndNum
   integer f,t,r,i,n,tr,xf,idot,ndot,icyc
   character*2000 TextA
   character*80 lastran
   character*60 cmd
   integer*2 narg
   integer :: IOpt=3          ! IOpt=1 uses concordance correlation (R), iopt=2 uses pearson correlation (R), iopt=3 uses root mean squard deviation as percent
   real    :: minRRatio=1.01  ! min R ratio between Skipped and Adjacent cycles to count it as an oscillation indicator
   real    :: minSkipR=0.99   ! min R between a cycle and the skipped cycle (prior prior) to count it as oscillation indicator
   integer :: minNos=4        ! min number of oscillation indicators for the row to count as a likely oscillation row
   real    :: minConvR=0.95   ! min R between a cycle and the prior cycle to count it as poorly converged 
   real    :: minRMSDpct=1.5  ! min RMSDpct to be considered to screen small variations
   real    :: maxRRatio=.85   ! max ratio of the Skipped to Adjacent RMSDpct values that will be considered to screen nuisance instances
   character*20 option(3)/'concordance','Pearson','RMSDpct'/
   integer nshift,ios
   logical eof
   logical parallel
   integer :: cnt,j
   allocate(RowStrings(8,mTab,mRow))
   RMSDPctAve(1:7,1:2)=0.
   RMSDPctCt(1:7,1:2)=0
    InFileNamet=' '
! check for command line argument IOpt. Default is 1 if argument not found.
   narg=command_argument_count()
   if(narg.ge.1) then
     call get_command_argument(1,cmd)
     read(cmd,'(BN,i1)',err=20,end=20) IOpt
20   continue
     if(IOpt.lt.1 .or. IOpt.gt.3) IOpt=3
   endif
   if(narg.ge.2) then
     call get_command_argument(2,cmd)
     read(cmd,*,err=30,end=30) minRRatio
     maxRRatio=minRRatio
30   continue
   endif
   if(narg.ge.3) then
     call get_command_argument(3,cmd)
     read(cmd,*,err=40,end=40) minSkipR
     minRMSDpct=minSkipR
40   continue
   endif
   if(narg.ge.4) then
     call get_command_argument(4,cmd)
     read(cmd,*,err=50,end=50) minNos
50   continue
   endif
   write(6,'(a,i1,a)') 'OSC Measurement option: ',IOpt,', '//trim(option(iopt))
   if(iopt.eq.3) then
     write(6,'(a,F6.4)') 'OSC MaxRRatio  option:   ',MaxRRatio
     write(6,'(a,F6.4)') 'OSC MinRMSDpct option:    ',MinRMSDpct
   else
     write(6,'(a,F6.4)') 'OSC MinRRatio option:   ',MinRRatio
     write(6,'(a,F6.4)') 'OSC MinSkipR option:    ',MinSkipR
   endif
   write(6,'(a,i1)')   'OSC MinNos option:      ',MinNos

   
    ncyc=0


! Open and read optional list of tables to process
   inquire(file='$NEMS/scripts/oscillate_tables.txt',exist=lexist)
   if(lexist) then
     otabs(:)=0
     notabs=1
     open(unit=10,file='$NEMS/scripts/oscillate_tables.txt',status='unknown',action='READ',share='denynone',SHARED )
     eof=.false.
     do while (.not. eof)
        read(10,*,iostat=ios) otabs(notabs)
        if(ios.eq.-1) then
          eof=.true.
          exit
        endif
        notabs=notabs+1
     enddo
     notabs=notabs-1
     close(10)
   else

     do notabs=1,maxotabs
       otabs(notabs)=notabs
     enddo
     notabs=maxotabs
   endif

! check to see if this was a parallel soluton run
   inquire(directory='p1',exist=parallel)

!Open and read the control file to get input, output file names
   inquire(file='oscillate.txt',exist=lexist)
   if(lexist) then
     open(unit=10,file='oscillate.txt')
     call ReadFileNames
     close(10)
     open(unit=20,file=trim(OutFileName),status='unknown')
   else
     open(unit=20,file='oscillate.csv',status='unknown')
     cmd='dir /b *.RAN  | sort -r > ranfiles.txt'
     call system(cmd)
     inquire(file='ranfiles.txt',exist=lexist)
     if(lexist)then
       open(unit=10,file='ranfiles.txt',status='old')
     else
       write(6,*) 'OSC no ran files to use'
       stop
     endif
     eof=.false.
     lastran=' '
     do while (.not. eof)
       read(10,'(a)',iostat=ios) textline
       if(ios.eq.-1)then
         eof=.true.
         exit
       endif
       ndot=0
       do i=1,len_trim(textline)
       if(textline(i:I).eq.'.') ndot=ndot+1
       enddo
       if(ndot.eq.2) then             ! the last ran file has two dots in its name.  the others have 3.
         lastran=textline
         ncyc=ncyc+1
       elseif(ndot.eq.3) then
         idot=index(textline,'.')+1
         if(textline(idot+1:idot+1).eq.'.') then
           read(textline(idot:idot),'(i1)') icyc
         elseif(textline(idot+2:idot+2).eq.'.') then
           read(textline(idot:idot+1),'(i2)') icyc
         endif
         if(icyc.lt.20) then
           InFileNamet(icyc)=trim(textline)
         endif
         ncyc=ncyc+1
       endif  
     enddo
     if(ncyc.gt.20) ncyc=20
     if(ncyc.ge.1) then
        InFileNamet(ncyc)=lastran
     endif
     nshift=0

     if(len_trim(infilenamet(ncyc)).eq.0.and.ncyc.gt.1) then
       ncyc=ncyc-1
     endif
     if(parallel .and. ncyc.gt.1) then
       write(6,*)'before, ncyc=',ncyc
       ncyc = ncyc - 1
       write(6,*)'after, ncyc=',ncyc
     endif
     if(ncyc.gt.maxcyc) then
       nshift=ncyc-maxcyc
       ncyc=maxcyc
     endif
 
     do icyc=1,ncyc
       InFileName(icyc)=InfileNamet(icyc+nshift)
     enddo
     if(parallel) then
       InFileName(ncyc+1)=InfileNamet(ncyc+nshift+1)
     endif
   endif
   if(ncyc.lt.3) then
     stop 'OSC not enough ran files to check for oscillation'
   endif

!Open and read the RAN input files
do xf=ncyc,1,-1

  write(6,'(3a)') 'OSC Reading RAN input file ',trim(InFileName(xf))
  inquire(file=trim(InFileName(xf)),exist=lexist)

  if(.not. lexist) then
    basename=max(index(infilename(xf),'/',BACK=.true.),index(infilename(xf),'\',BACK=.true.))+1
    if(basename.gt.1) then
      retry=InFileName(xf)(basename:)
      inquire(file=trim(retry),exist=lexist)
      if(lexist) then
       InFileName(xf)=retry
      endif
    endif
  endif
  if(lexist) then
    open(unit=10,file=trim(InFileName(xf)),access='stream',action='READ' ) ! &
!intel    recl=1,access='direct',form='binary',READONLY,buffered='yes')
    call ReadRan(xf)
  else
    write(6,'(a,a)') 'Ran File not found: ',trim(InFileName(xf))
    stop 
  endif

  close(10)
enddo

! Open grafnem recording file to create slideshow of the oscillation rows:
open(25,file='osc.rec',status='unknown')
rewind 25
! write header stuff 
write(25,'(a)')  'Start|0|0|00:00:00|0'
write(25,'(a)')  'dataform.normal|1|6|00:00:00|1'
write(25,'(a)')  'regdata.normal|1|6|00:00:00|2'
write(25,'(a)')  'graf98.activate|1|6|00:00:00|3'
write(25,'(a)')  'graf98.command4: Compare Results Across Runs|1|1|00:00:00|4'
write(25,'(a)')  'graf98.minimized|1|6|00:00:00|4'


! Open grafnem recording file to create slideshow of the Table 2/3 poorly-converged rows:
open(26,file='cnv.rec',status='unknown')
rewind 26
! write header stuff 
write(26,'(a)')  'Start|0|0|00:00:00|0'
write(26,'(a)')  'dataform.normal|1|6|00:00:00|1'
write(26,'(a)')  'regdata.normal|1|6|00:00:00|2'
write(26,'(a)')  'graf98.activate|1|6|00:00:00|3'
write(26,'(a)')  'graf98.command4: Compare Results Across Runs|1|1|00:00:00|4'
write(26,'(a)')  'graf98.minimized|1|6|00:00:00|4'



if(iopt.eq.3) then
  call check(IOpt,maxRRatio,minRMSDpct,minNos) 
  do i=ncyc,3,-1
    do j=1,2
      if(RMSDPctCt(i,j).gt.0) then
        RMSDPctAve(i,j)=RMSDPctAve(i,j)/float(RMSDPctCt(i,j))
      endif
    enddo
  enddo
  write(6,'(a,4(2f8.2,a1,2x))') 'OSC RMSDPctAve', ((RMSDPctAve(i,j),j=1,2),'|',i=ncyc,3,-1)
  cnt=0
  do i=ncyc,3,-1
    if(RMSDPctAve(i,2).lt.RMSDpctAve(i,1)) then
      cnt=cnt+1
    endif
  enddo
  write(6,'(a,i1)') 'OSC Oscillation score based on aggregate scores oscillating (4 is bad, 0 good):',cnt
  oscor=cnt
else
  call check(IOpt,minRRatio,minSkipR,minNos)
endif
close(20)
open(20,file='converge.csv',status='unknown')
if(iopt.eq.3) then
  minConvR=5.0  ! minimum root mean squared deviation, expressed as a percent, to qualify as poor convergence.  Because deviations are squared, this is sensitive and
                ! definitely not be interpreted as if it were an average percentage change
endif

 call check2(IOpt,minConvR)

! Store summary oscillation count,OSCOR, into a Table 147 (Convergence Indicators), row 47 in
! the final cycle.  If a parallel run, the ran created after the final p1 run
if(parallel) ncyc=ncyc+1
  xf=ncyc
  write(6,'(3a)') 'OSC Storing oscillation score in RAN file ',trim(InFileName(xf))
  inquire(file=trim(InFileName(xf)),exist=lexist)

  if(.not. lexist) then
    basename=max(index(infilename(xf),'/',BACK=.true.),index(infilename(xf),'\',BACK=.true.))+1
    if(basename.gt.1) then
      retry=InFileName(xf)(basename:)
      inquire(file=trim(retry),exist=lexist)
      if(lexist) then
       InFileName(xf)=retry
      endif
    endif
  endif
  if(lexist) then
    open(unit=10,file=trim(InFileName(xf)),access='stream',action='READWRITE',share='denynone',SHARED ) ! &
    call ReadRanTab(xf,147)
    close(10)
  else
    write(6,'(a,a)') 'Ran File not found: ',trim(InFileName(xf))
    stop 
  endif





stop
end

!*******************************************************
subroutine ReadRANTab(xf,itab)
   use r_vars
   implicit none
   integer oscor
   common/score/oscor  
   character*6 RGForm_skip

   integer*4 j,i,ByteLoc,XLoc,xf,irow,itab
   integer ios
   logical eof
   subject=' '
   RowTitle=' '
   RowStrings=' '
!  Read the main header.
   errplace='reading header'
   ByteLoc=1
    read(10,pos=ByteLoc,err=99,iostat=ios) RMVer,RMLen,RSLen,RTLen,RDLen
!   read(10,rec=ByteLoc,err=99,iostat=ios) RMVer,RMLen,RSLen,RTLen,RDLen

!  Read the scenario header.
   ByteLoc=ByteLoc+RMLen
   RSFile=repeat(' ',150)
   RSScen=repeat(' ',150)
   errplace='reading scenario'
    read(10,pos=ByteLoc,err=99,iostat=ios) RSVer,RSFLen,RSFile(1:RSFLen),RSSLen,RSScen(1:RSSLen)
 !  read(10,rec=ByteLoc,err=99,iostat=ios) RSVer,RSFLen,RSFile(1:RSFLen),RSSLen,RSScen(1:RSSLen)

!Read the table location header.
   ByteLoc=ByteLoc+RSLen
   errplace='reading table location header'
    read(10,pos=ByteLoc,err=99,iostat=ios) RTNum,(RTKey(i),RTTRow(i),RTDRow(i),RTDLoc(i),RTILoc(i),i=1,RTNum)
!   read(10,rec=ByteLoc,err=99,iostat=ios) RTNum,(RTKey(i),RTTRow(i),RTDRow(i),RTDLoc(i),RTILoc(i),i=1,RTNum)

!Read the data location header.
   do i=1,RTNum
     ByteLoc=RTILoc(i)
      read(10,pos=ByteLoc,err=99,iostat=ios) (RDKey(i,j),RDLoc(i,j),j=1,RTDRow(i))
!     read(10,rec=ByteLoc,err=99,iostat=ios) (RDKey(i,j),RDLoc(i,j),j=1,RTDRow(i))
   end do

!Read table ITAB 
   i=itab
     write(errplace,'(a,i4,a,i4)') 'reading table',i,' irow=',irow

     ByteLoc=RTDLoc(i)
     XLoc=RTDLoc(i+1)
     if(i.eq.RTNum)then
       XLoc=RTDLoc(i)*2
     endif
     TotNote(i)=0
     irow=0
     eof=.false.
     do while (.not.eof.and.ByteLoc.lt.XLoc)
       write(errplace,'(a,i4,a,i4)') 'reading table',i,' row header'
        read(10,pos=ByteLoc,err=999,iostat=ios) RGType,RGSTyp,RGLRem
 !      read(10,rec=ByteLoc,err=999,iostat=ios) RGType,RGSTyp,RGLRem
       if(ios.eq.-1) then
         eof=.true.
         exit
       endif
       ByteLoc=ByteLoc+6

       if(RGType.eq.1) then
         write(errplace,'(a,i4,a,i4)') 'reading table',i,' Title'
          read(10,pos=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGTAct(i),RGTReg(i),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
!         read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGTAct(i),RGTReg(i),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
         if(RGTTyp.ne.4) Then
           ByteLoc=Byteloc+2+6+2+2+2+6+2+RGTLen
            read(10,pos=Byteloc) RGSLen,Subject(i,RGTTyp)(1:RGSLen)
  !         read(10,rec=Byteloc) RGSLen,Subject(i,RGTTyp)(1:RGSLen)
           ByteLoc=ByteLoc+2+RGSLen
         else
           ByteLoc=ByteLoc+RGLRem
         endif
         TabTitle(i,RGTTyp)=RGText(1:RGTLen)
      
       elseif(RGType.eq.2) then
         irow=irow+1
         RowTitle(i,irow)=' '
         write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 2, row',irow
          read(10,pos=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen)
!         read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen)
         ByteLoc=ByteLoc+RGLRem
         RowTitle(i,irow)=RGText(1:RGTLen)
         DataRow(i,irow)=0

       elseif(RGType.eq.3) then
         irow=irow+1
         write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 3, row',irow
          read(10,pos=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen), &
 !        read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen), &
          RGDTyp(i,irow),RGDFYr(i,irow),RGDLYr(i,irow), & 
          RGData(  1:(RGDLYr(i,irow)-RGDFYr(i,irow)+1),   i,irow,xf),&
              (LenRowStr(j), RowStrings(j,i,irow)(1:LenRowStr(j))  , j=1,8)    ! each string, preceded by its length to allow variable-length storage

! Enter oscillation score into all year positions of row 47 in table 147
         if(RGRKEY(i,irow).eq.'ga_Oscor') then
            do j=1,(RGDLYr(i,irow)-RGDFYr(i,irow)+1)
              RGData(j,i,irow,xf)=oscor
            enddo
            write(errplace,'(a,i4,a,i4)') 'writing table',i,'RGtype 3, row',irow
            write(10,pos=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen), &
            RGDTyp(i,irow),RGDFYr(i,irow),RGDLYr(i,irow), & 
            RGData(  1:(RGDLYr(i,irow)-RGDFYr(i,irow)+1),   i,irow,xf)          
         endif




         ByteLoc=ByteLoc+RGLRem
         RowTitle(i,irow)=RGText(1:RGTLen)
         DataRow(i,irow)=1
         if(irow.ne.RGRNum) then
            write(*,*) 'irow.ne.RGRNum: i,RGRNum=',i,RGRNum
         endif
         if(i.ne.RGTNum) then
            write(*,*) 'i.ne.RGTNum: i,RGTNum=',i,RGTNum
         endif
         

         
         
         
       elseif(RGType.eq.4) then
         write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 4, Footnote',totnote(i)+1
          read(10,pos=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
!         read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
         ByteLoc=ByteLoc+RGLRem
         TotNote(i)=TotNote(i)+1
         FootNote(i,TotNote(i))=RGText(1:RGTLen)
       else
         errplace='other'
        !Anything greater than 4 is an error.
         write(*,*) 'ERROR IN FILE'
         goto 99
       endif
     end do
!  =========================   

   return
999 continue
    if(ios.eq.36 .and. i.eq.RTNum) then
      return
    endif

99 Continue
   write(6,*) 'OSC ==========================================================================='
   write(6,*) 'OSC  Error in Oscillate program.'
   write(6,*) 'OSC  Error reading RAN file. The input RAN file is invalid in some way.'
   write(6,*) 'OSC  IOSTAT=',ios
   write(6,*) 'OSC  RGType=',rgtype
   write(6,*) 'OSC  ByteLoc=',byteloc
   write(6,*) 'OSC  RMVer=',RMVer
   write(6,*) 'OSC  errplace=',trim(errplace)
   write(6,*) 'OSC ==========================================================================='
   stop ' '
end
!********************************************************
!*******************************************************
subroutine ReadRAN(xf)
   use r_vars
   implicit none
  
   character*6 RGForm_skip

   integer*4 j,i,ByteLoc,XLoc,xf,irow
   integer ios
   logical eof
   subject=' '
   RowTitle=' '
   RowStrings=' '
!  Read the main header.
   errplace='reading header'
   ByteLoc=1
    read(10,pos=ByteLoc,err=99,iostat=ios) RMVer,RMLen,RSLen,RTLen,RDLen
!   read(10,rec=ByteLoc,err=99,iostat=ios) RMVer,RMLen,RSLen,RTLen,RDLen

!  Read the scenario header.
   ByteLoc=ByteLoc+RMLen
   RSFile=repeat(' ',150)
   RSScen=repeat(' ',150)
   errplace='reading scenario'
    read(10,pos=ByteLoc,err=99,iostat=ios) RSVer,RSFLen,RSFile(1:RSFLen),RSSLen,RSScen(1:RSSLen)
 !  read(10,rec=ByteLoc,err=99,iostat=ios) RSVer,RSFLen,RSFile(1:RSFLen),RSSLen,RSScen(1:RSSLen)

!Read the table location header.
   ByteLoc=ByteLoc+RSLen
   errplace='reading table location header'
    read(10,pos=ByteLoc,err=99,iostat=ios) RTNum,(RTKey(i),RTTRow(i),RTDRow(i),RTDLoc(i),RTILoc(i),i=1,RTNum)
!   read(10,rec=ByteLoc,err=99,iostat=ios) RTNum,(RTKey(i),RTTRow(i),RTDRow(i),RTDLoc(i),RTILoc(i),i=1,RTNum)

!Read the data location header.
   do i=1,RTNum
     ByteLoc=RTILoc(i)
      read(10,pos=ByteLoc,err=99,iostat=ios) (RDKey(i,j),RDLoc(i,j),j=1,RTDRow(i))
!     read(10,rec=ByteLoc,err=99,iostat=ios) (RDKey(i,j),RDLoc(i,j),j=1,RTDRow(i))
   end do

!Read through all the tables and write out a debug.
   do i=1,RTNum
     write(errplace,'(a,i4,a,i4)') 'reading table',i,' irow=',irow

     ByteLoc=RTDLoc(i)
     XLoc=RTDLoc(i+1)
     if(i.eq.RTNum)then
       XLoc=RTDLoc(i)*2
     endif
     TotNote(i)=0
     irow=0
     eof=.false.
     do while (.not.eof.and.ByteLoc.lt.XLoc)
       write(errplace,'(a,i4,a,i4)') 'reading table',i,' row header'
        read(10,pos=ByteLoc,err=999,iostat=ios) RGType,RGSTyp,RGLRem
 !      read(10,rec=ByteLoc,err=999,iostat=ios) RGType,RGSTyp,RGLRem
       if(ios.eq.-1) then
         eof=.true.
         exit
       endif
       ByteLoc=ByteLoc+6

       if(RGType.eq.1) then
         write(errplace,'(a,i4,a,i4)') 'reading table',i,' Title'
          read(10,pos=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGTAct(i),RGTReg(i),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
!         read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGTAct(i),RGTReg(i),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
         if(RGTTyp.ne.4) Then
           ByteLoc=Byteloc+2+6+2+2+2+6+2+RGTLen
            read(10,pos=Byteloc) RGSLen,Subject(i,RGTTyp)(1:RGSLen)
  !         read(10,rec=Byteloc) RGSLen,Subject(i,RGTTyp)(1:RGSLen)
           ByteLoc=ByteLoc+2+RGSLen
         else
           ByteLoc=ByteLoc+RGLRem
         endif
         TabTitle(i,RGTTyp)=RGText(1:RGTLen)
      
       elseif(RGType.eq.2) then
         irow=irow+1
         RowTitle(i,irow)=' '
         write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 2, row',irow
          read(10,pos=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen)
!         read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen)
         ByteLoc=ByteLoc+RGLRem
         RowTitle(i,irow)=RGText(1:RGTLen)
         DataRow(i,irow)=0

       elseif(RGType.eq.3) then
         irow=irow+1
         write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 3, row',irow
          read(10,pos=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen), &
 !        read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen), &
          RGDTyp(i,irow),RGDFYr(i,irow),RGDLYr(i,irow), & 
          RGData(  1:(RGDLYr(i,irow)-RGDFYr(i,irow)+1),   i,irow,xf),&
              (LenRowStr(j), RowStrings(j,i,irow)(1:LenRowStr(j))  , j=1,8)    ! each string, preceded by its length to allow variable-length storage

         ByteLoc=ByteLoc+RGLRem
         RowTitle(i,irow)=RGText(1:RGTLen)
         DataRow(i,irow)=1
         if(irow.ne.RGRNum) then
            write(*,*) 'irow.ne.RGRNum: i,RGRNum=',i,RGRNum
         endif
         if(i.ne.RGTNum) then
            write(*,*) 'i.ne.RGTNum: i,RGTNum=',i,RGTNum
         endif
         
       elseif(RGType.eq.4) then
         write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 4, Footnote',totnote(i)+1
          read(10,pos=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
!         read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
         ByteLoc=ByteLoc+RGLRem
         TotNote(i)=TotNote(i)+1
         FootNote(i,TotNote(i))=RGText(1:RGTLen)
       else
         errplace='other'
        !Anything greater than 4 is an error.
         write(*,*) 'ERROR IN FILE'
         goto 99
       endif
     end do
   end do

   return
999 continue
    if(ios.eq.36 .and. i.eq.RTNum) then
      return
    endif

99 Continue
   write(6,*) 'OSC ==========================================================================='
   write(6,*) 'OSC  Error in Oscillate program.'
   write(6,*) 'OSC  Error reading RAN file. The input RAN file is invalid in some way.'
   write(6,*) 'OSC  IOSTAT=',ios
   write(6,*) 'OSC  RGType=',rgtype
   write(6,*) 'OSC  ByteLoc=',byteloc
   write(6,*) 'OSC  RMVer=',RMVer
   write(6,*) 'OSC  errplace=',trim(errplace)
   write(6,*) 'OSC ==========================================================================='
   stop ' '
end


!*******************************************************
subroutine ReadFileNames
use r_vars
implicit none

integer i

read(10,'(1x)')
read(10,'(1x)')
!Output file name.
read(10,'(a)') OutFileName
read(10,'(BN,i2)') ncyc ! Number of cycle files to read
if(ncyc.gt.maxcyc) then
  write(6,*)'OSC Can only process ',maxcyc,' files.'
  ncyc=maxcyc
endif
if(ncyc.lt.3) then
  write(6,*)'OSC Need 3 cycles for oscillation detection.'
endif
!Input  ran file for each cycle, beginning with last cycle.

do i=ncyc,1,-1
read(10,'(a)') InFileName(i)
enddo

close(10)


return
end
!==================================================================
subroutine check(IOpt,limit1,limit2,minNos)
use r_vars
implicit none

   integer oscor  ! oscillation count to be stored in table 150 row
   common/score/oscor

integer iTNum,iTRow,nos,tnos,tcnt
integer :: IOpt          ! IOpt=1 uses concordance correlation (R), iopt=2 uses pearson correlation (R), iopt=3 uses root mean squard deviation as percent
real minRRatio  ! min R ratio between Skipped and Adjacent cycles to count it as an oscillation indicator
real minSkipR   ! min R between a cycle and the skipped cycle (prior prior) to count it as oscillation indicator
integer minNos   ! min number of oscillation indicators for the row to count as a likely oscillation row
real radj(maxcyc),rskip(maxcyc),RRatio
real maxRRatio ! max RRatio of RMSDpcts of between the Skipped and Adjacent solutions to be reported
real minRMSDpct ! min RMSDpct of the current solution to be reported to screen small changes
real limit1,limit2 ! two limits passed as argument. usage depends on IOPT value
character*1 osc ! Y in csv output column if the row is a possible oscillator; blank otherwise
character*128 graphlabel,labels(8)
integer icyc,i
logical process,first_in_table
integer(4) cumultime

if(iopt.eq.3) then
  maxRRatio=limit1
  minRMSDpct=limit2
else
  minRRatio=limit1
  minSkipR=limit2
endif
tnos=0
cumultime=1000
do iTNum=1,RTNum              ! go through list of tables
  tcnt=0
  
  if(RGTReg(iTNUM).eq.0) then ! only national tables for now

!   check to see if the ftab table number is among those selected to process (from optional $NEMS/scripts/oscillate_tables.txt)
    process=.false.
    do i=1,notabs
      if(otabs(i).eq.RGTAct(iTNum)) then
        process=.true.
        exit ! exit loop
      endif
    enddo
    if(process) then    
     first_in_table=.true.  
     do iTRow=1,RTTRow(iTNum)
  
      graphlabel=' '
      if(DataRow(iTNum,iTRow).eq.1) then
         graphlabel=rowstrings(2,ITnum,iTRow)
         labels(1:8)=rowstrings(1:8,ITnum,iTRow)
         radj(:)=0
         rskip(:)=0
         nos=0
         
         if(iopt.eq.1 .or. iopt.eq.2) then
! calculate correlation between last cycle and adjacent cycle and between last cycle 
! and the cycle two back.  If results of latter are more similar (higher R), may be an oscillation.  the
! probability is higher when the ratio of the R values is > 1.05.  Also, If we have 4 or 5 runs, and the pattern 
! repeats (nos will equal 2 or 3), there is also a good chance it is oscillating.
           do icyc=ncyc,3,-1
             call correlation(RGData(21,iTNum,iTRow,icyc),RGData(21,iTNum,iTRow,icyc-1),26,radj(icyc),IOpt)  ! correlation with adjacent (prior) cycle
             call correlation(RGData(21,iTNum,iTRow,icyc),RGData(21,iTNum,iTRow,icyc-2),26,rskip(icyc),IOpt) ! correlation skipping a cycle (prior prior) cycle
             RRatio=0.
             if(radj(icyc).gt. 0.0001) RRatio=rskip(icyc)/radj(icyc)
             if(rskip(icyc).ne.0) then
                if(RRatio.gt.MinRRatio.and.rskip(Ncyc).gt.MinSkipR) nos=nos+1  ! to eliminiate volatile supply curve rows, only track relatively well-behaved values with R>minRRatio
             endif
           enddo
           osc=' '
           if(nos.eq.ncyc-2.or.nos.ge.minNos) then  ! looking for persistent pattern across all cycles examined, or min number specified
              osc='Y'
              tnos=tnos+1
              tcnt=tcnt+1
              RRatio=0.
              if(radj(ncyc).gt. 0.0001) RRatio=rskip(ncyc)/radj(ncyc)
              if(tnos.eq.1) then
              write(6,'(a)')                             'OSC Table Row R-Ratio    RAdj   RSkip Nos  Graph Label'
              endif
              write(6,'(a,i5,i5,3f8.4,i4,1x,a)')  'OSC',RGTAct(iTNum),iTRow,RRatio,radj(ncyc),rskip(ncyc),nos,' '//trim(graphlabel)

 ! write grafnem playback instructions for this table, row
              if(first_in_table) then
                write(25,'(a,i3,a,i6)')  'plotform.tablelistbox|',  iTNum , '|3|00:00:00|',cumultime
                first_in_table=.false.
              endif
              cumultime=cumultime+3000
              write(25,'(a,i3,a,i6)') 'plotform.list1|', iTRow, '|3|00:00:03|',cumultime

           endif
           write(20,'(i3,a,i3,1x,2a,i1,a,20(a,f8.5,a,f8.5))') RGTAct(iTNum),',',iTROW,',"'//trim(graphlabel),'",'//osc//',',nos,',',(',',radj(icyc),',',rskip(icyc),icyc=ncyc,3,-1)
         else
! calculate root mean squared deviation between last cycle and adjacent cycle and between last cycle 
! and the cycle two back.  If results of latter are more similar (smaller RMSD), may be an oscillation.  the
! probability is higher when the ratio of the RMSD values is < .80.  Ignore small deviations (RMSDpct<3).
!  Also, If we have 4 or 5 runs, and the pattern repeats (nos will equal 2 or 3), there is also a good chance it is oscillating.
           do icyc=ncyc,3,-1
             call RMSDpct(RGData(21,iTNum,iTRow,icyc),RGData(21,iTNum,iTRow,icyc-1),26,radj(icyc))  ! RMSD with adjacent (prior) cycle
             call RMSDpct(RGData(21,iTNum,iTRow,icyc),RGData(21,iTNum,iTRow,icyc-2),26,rskip(icyc)) ! RMSD skipping a cycle (prior prior) cycle
             RRatio=1.
             if(radj(icyc).gt. 0.0001)then
               RRatio=rskip(icyc)/radj(icyc)
               
               if(itnum.ne.111.and.itnum.ne.112.and.itnum.ne.150) then
                 RMSDPctAve(icyc,1)=RMSDPctAve(icyc,1)+min(radj(icyc),25.)
                 RMSDPctCt(icyc,1)=RMSDPctCt(icyc,1)+1
                 RMSDPctAve(icyc,2)=RMSDPctAve(icyc,2)+min(rskip(icyc),25.)
                 RMSDPctCt(icyc,2)=RMSDPctCt(icyc,2)+1
               endif
             endif
             if(rskip(icyc).ne.0) then
                if(RRatio.lt.MaxRRatio.and.radj(Ncyc).gt.MinRMSDpct) then
                 nos=nos+1  
                endif
             endif
           enddo
           osc=' '
           if(nos.eq.ncyc-2.or.nos.ge.minNos) then  ! looking for persistent pattern across all cycles examined, or min number specified
              osc='Y'
              tnos=tnos+1
              tcnt=tcnt+1
              RRatio=1.
              if(radj(ncyc).gt. 0.0001) RRatio=rskip(ncyc)/radj(ncyc)
              if(tnos.eq.1) then
              write(6,'(a)')                             'OSC Table Row R-Ratio RMSDAdj RMSDSkip Nos  Graph Label'
              endif
              write(6,'(a,i5,i5,3f8.2,i4,1x,a)')  'OSC',RGTAct(iTNum),iTRow,RRatio,radj(ncyc),rskip(ncyc),nos,' '//trim(graphlabel)

! write grafnem playback instructions for this table, row
              if(first_in_table) then
                write(25,'(a,i3,a,i6)')  'plotform.tablelistbox|',  iTNum , '|3|00:00:00|',cumultime
                first_in_table=.false.
              endif
              cumultime=cumultime+3000
              write(25,'(a,i3,a,i6)') 'plotform.list1|', iTRow, '|3|00:00:03|',cumultime

           endif
           write(20,'(i3,a,i3,1x,2a,i1,a,20(a,f8.2,a,f8.2))') RGTAct(iTNum),',',iTROW,',"'//trim(graphlabel),'",'//osc//',',nos,',',(',',radj(icyc),',',rskip(icyc),icyc=ncyc,3,-1)

         endif


      else
        write(20,'(i3,a,i3,1x,2a)') RGTAct(iTNum),',',iTROW,',"'//trim(graphlabel)//'"'
      endif

     enddo
    endif 
   endif
enddo
write(6,'(a,i5)')'OSC Number rows in national tables with intercycle oscillation:',tnos
return
end
!==============================================================================
subroutine check2(IOpt,minAdjR)
use r_vars
implicit none
integer iTNum,iTRow,nos,tnos,tcnt
integer IOpt ! IOpt=1 uses concordance correlation, iopt=2 uses pearson correlation
real minAdjR   ! min R between the last cycle and prior cycle to count as not converged.
real radj
character*128 graphlabel,labels(8)
integer icyc
real sumr,nrow
logical process,first_in_table
integer(4) cumultime
tnos=0
sumr=0.
cumultime=1000
do iTNum=1,RTNum              ! go through list of tables
  if(RGTAct(ITnum).ge. 2 .and. RGTAct(ITnum).le.3 .and.   RGTReg(iTNUM).gt.0.and.RGTReg(iTNUM).le.9) then ! Only Regional Tables 2, 3

    first_in_table=.true.

    do iTRow=1,RTTRow(iTNum)
      graphlabel=' '
      if(DataRow(iTNum,iTRow).eq.1) then
         graphlabel=rowstrings(2,ITnum,iTRow)
         labels(1:8)=rowstrings(1:8,ITnum,iTRow)
         radj=0
         icyc=ncyc
! calculate correlation between last cycle prior cycle.  Print message if correlation less than threshhold,
! mean it poor intercycle convergence
         if(iopt.eq.1.or.iopt.eq.2) then
           call correlation(RGData(21,iTNum,iTRow,icyc),RGData(21,iTNum,iTRow,icyc-1),26,radj,IOpt)  ! correlation with adjacent (prior) cycle
         else
           call RMSDpct(RGData(21,iTNum,iTRow,icyc),RGData(21,iTNum,iTRow,icyc-1),26,radj)  ! RMSDpct between last two cycles
         endif
         if(radj.gt.0) then
           nrow=nrow+1
           sumr=sumr+radj
 ! use minimum RMSDpct threshhold for opt 3. use maximum correlation threshhold for opt 1 or 2
           if(iopt.eq.3) then
            if(radj.gt.minAdjR) then  
             if(tnos.eq.0) then
               write(6,'(a)')                             'CNV Reg Table Row  RMSDpct Graph Label'
             endif
             write(6,'(a,i4,i5,i5,f8.2,1x,a)')  'CNV',RGTReg(iTNUM),RGTAct(iTNum),iTRow,radj,trim(graphlabel)
             tnos=tnos+1

! write grafnem playback instructions for this table, row
             if(first_in_table) then
                write(26,'(a,i3,a,i6)')  'plotform.tablelistbox|',  iTNum , '|3|00:00:00|',cumultime
                first_in_table=.false.
             endif
             cumultime=cumultime+3000
             write(26,'(a,i3,a,i6)') 'plotform.list1|', iTRow, '|3|00:00:03|',cumultime
            endif
           else
            if(radj.lt.minAdjR) then  
             if(tnos.eq.0) then
               write(6,'(a)')                             'CNV Reg Table Row    RAdj  Graph Label'
             endif
             write(6,'(a,i4,i5,i5,f8.4,1x,a)')  'CNV',RGTReg(iTNUM),RGTAct(iTNum),iTRow,radj,trim(graphlabel)
             tnos=tnos+1
! write grafnem playback instructions for this table, row
             if(first_in_table) then
                write(26,'(a,i3,a,i6)')  'plotform.tablelistbox|',  iTNum , '|3|00:00:00|',cumultime
                first_in_table=.false.
             endif
             cumultime=cumultime+3000
             write(26,'(a,i3,a,i6)') 'plotform.list1|', iTRow, '|3|00:00:03|',cumultime

            endif
           endif
           write(20,'(i2,a,i3,a,i3,1x,2a,f8.5)') RGTReg(iTNUM),',',RGTAct(iTNum),',',iTROW,',"'//trim(graphlabel),'",',radj
         endif
      else
        write(20,'(i2,a,i3,a,i3,1x,2a)') RGTReg(iTNUM),',',RGTAct(iTNum),',',iTROW,',"'//trim(graphlabel)//'"'
      endif
     
    enddo
 
  endif
enddo
if(iopt.eq.1.or.iopt.eq.2) then
  write(6,'(a,f8.3)')'CNV Average correlation of regional 2 and 3 rows',sumr/nrow
 else
  write(6,'(a,f8.2)')'CNV Average RMSDpct of regional 2 and 3 rows',sumr/nrow
 endif
if(tnos.gt.0) then
  write(6,'(a,i5)')'CNV Number rows in regional tables 2 and 3 with poor convergence:',tnos
else
  write(6,'(a)')'CNV No intercycle convergence problems found in regional tables 2 and 3'
endif
return
end
!+++++++++++++++++++++++++++++++++++++++++++
subroutine mean(x,n,result)
implicit none
integer n
real x(n),result
result=sum(x(1:n))/float(n)
return
end
!+++++++++++++++++++++++++++++++++++++++++++
subroutine stddev(x,n,result)
! returns the population standard deviation of x in result
implicit none
integer n,i
real x(n),result,xmean

call mean(x,n,xmean)

result=0.
do i=1,n
result=result+(x(i)-xmean)**2    ! sum of squared deviations
enddo
result=result/float(n)           ! variance: average squared deviation

result=result**0.5  


return
end
!+++++++++++++++++++++++++++++++++++++++++++
subroutine rmsd(x1,x2,n,result)
! returns the root mean squared deviation between two series with n items
real x1(n),x2(n),result
integer i
result=0.
do i=1,n
  result=result+(x1(i)-x2(i))*(x1(i)-x2(i))
enddo
result=result/float(n)
result=result**(0.5)

return
end
!===============================================================
subroutine filter(x,n,useit)
! checks if series is all zero or a mix of positive and negative.  If so, return useit=false.  Otherwise, useit=true
implicit none
integer n,i,npos,nneg,nzer
real x(n)
logical useit
! screen out series with positive and negatives
npos=0
nneg=0
nzer=0
useit=.true.
do i=1,n
  if(npos.eq.0) then
     if(x(i).gt.0.0) npos=npos+1
  endif
  if(nneg.eq.0)then
    if(x(i).lt.0.0) nneg=nneg+1
  endif
  if(npos.gt.0.and.nneg.gt.0) then
    useit=.false.
    exit
  endif
enddo
if(sum(x(1:n)).eq.0.0) then
  useit=.false.
endif
return
end

Subroutine correlation(x,y,n,ResultR,Iopt)
! computes concordance correlation coefficient: measure of agreement between two variables
! and pearson correlation.  Iopt=1 returns concordance, iopt=2 returns pearson
implicit none
real x(n),y(n),ConcordR,PearsonR,accuracy,resultR
integer n, i,iopt
logical useit
real sx,sy,mux,muy,covar,varX,varY,mudiff,denom
ResultR=0.
covar=0.
call filter(x,n,useit)
if(useit) then
  call filter(y,n,useit)
  if(useit) then
     call mean(x,n,mux)
     call mean(y,n,muy)
     call stddev(x,n,sx)
     call stddev(y,n,sy)
     do i=1,n
       covar=covar+(x(i)-mux)*(y(i)-muy)
     enddo
     covar=covar/float(n)
     PearsonR=covar/(sx*sy)
     varX=sx*sx
     varY=sy*sy
     mudiff=(mux-muy)*(mux-muy)
     denom=varX+varY+mudiff
     if(denom.gt.  0.00001) then
       ConcordR=2.*covar/( (sx*sx) + (sy*sy) + ((mux-muy)*(mux-muy))  )
     else
       ConcordR=1.
     endif
     accuracy=ConcordR/PearsonR
  endif
else
  ConcordR=0.  ! dummy result for filtered rows
  PearsonR=0.
endif
if(iopt.eq.1)then
  resultR=ConcordR
elseif(iopt.eq.2) then
  resultR=PearsonR
endif
return
end

Subroutine RMSDpct(x,y,n,ResultR)
! computes root mean squared deviation as percent of mean of first series.
implicit none
real x(n),y(n),resultR
integer n, i,iopt
logical useit
real mux
ResultR=0.
call filter(x,n,useit)
if(useit) then
  call filter(y,n,useit)
  if(useit) then
     call mean(x,n,mux)
     call rmsd(x,y,n,resultR)
     if(mux.ne.0) then
       resultr=100.*resultr/mux
     endif
    
  endif
endif

return
end


