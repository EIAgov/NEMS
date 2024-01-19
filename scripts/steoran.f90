! STEORan:  reads a STEO comparison file and a nems/ftab ran file.  Writes a new ran file with steo values 
! for matching rows in the comparison file.  Values in all other data rows are reset to whatever
! value the 2010 value is (the last history year) so that it is obvious in horizontal line graphs that there
! are no STEO values available.
!
! data declaration module r_vars stores common block declarations to avoid repeating and avoid include files
 module r_vars
    implicit none
    
    integer maxsteo    
    parameter(maxsteo=2000)

    integer nsteo

! Program Maintenance ======================================================================
    integer msteoyrs, lasthist, numranyrs                                                 !=
    parameter (msteoyrs=2020-1995+1)    !  Number of STEO years starting 1i 1995          !=
    parameter (lasthist=2018-1995+1)    !  Last STEO historical year                      !=
    parameter (numranyrs=2050-1995+1)   !  number of years in RAN file starting with 1995 !=
    integer maxfield                    !  maximum number of fields in STEO file          !=
    parameter (maxfield=msteoyrs+2)     !  number of fields = 2 name field plus msteoyrs  !=
!===========================================================================================

    real steod(msteoyrs,maxsteo) 
    character*30 steom(maxsteo)
    common/steo/nsteo,steom,steod

    integer mTab,mRow
    parameter (mTab=500) ! maximum number of tables stored in ran
    parameter (mRow=400) ! maximum number of rows on any single table, MAXROW is 400 in ftable
   
 
 ! RANVar1
   real*4    RGData(numranyrs,mTab,mRow)
   integer*4 DataRow(mTab,mRow)             ! data row indicator: 1: yes, 0: no 
   integer*4 TotNote(mTab)
   common/RANVar1/RGData,DataRow,TotNote
   
 ! RANVar2   
   character*130  TabTitle(mTab,4)          ! i extended this to equal TABHED
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
   character*16  RDKey(mTab,mTab)  ! data row keyword: a unique string identifying this row
   integer*4     RDLoc(mTab,mTab)  ! data row byte location in the data section

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
   integer*2     LenRowStr(8,mTab,mRow)  ! length of 8 row strings which we need to preserve as input
! additional descriptive strings: RowStringy(8) read in, sized as in ftab2.f
!                                 RowStrings(8,mTab,mRow) assigned first 256 charcters of what is read in
!      doing this because 1536 by 500 by 400 exceed maximum variable size
   character*1536 RowStringy(8)
   character*512  RowStrings(8,mTab,mRow)
   character*1024 DumString(8)
   common/RANVar4/ RMLen,RSLen,RTLen,RDLen,RTDLoc,RTILoc,RDLoc, &
    RSFLen,RSSLen,RTNum,RTTRow,RTDRow, &
    RMVer,RSVer,RDKey,RTKey,errplace  
  
   common/RANVar5/RGType,RGSTyp,RGLRem,RGTNum,RGTAct,RGTReg,RGRNum,RGTTyp,RGTLen,RGDTyp,RGDFYr,RGDLYr, &
    RGForm,RGTKey,RGRKey

   character*256 InFileName(2),OutFileName
   common/files/InFileName,OutFileName
   character*100 errplace  

end module r_vars
!==================================================================================================
Program RanToRtf
  use r_vars
  implicit none

!
    character*150 retry
    logical lexist
    integer basename,ftabbone

    integer TLen,y
    character*200 TextLine

   integer Align,Apper,Cont,Skip
   real PtSize,IndNum
   integer f,t,r,i,n,tr,xf
   character*2000 TextA


!Open and read the control file to get input, output file names
   open(unit=10,file='steoran.txt')
   call ReadSngl
   close(10)
! Open and read the steo csv file generated from the aeo-steo comparison spreadsheet
   open(unit=10,file=infilename(2),status='old',readonly)
   call readcsv(10)
   

!Open and read the RAN input file
xf=1
write(*,'(2a)') 'Reading RAN input file ',trim(InFileName(xf))
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
   open(unit=10,file=trim(InFileName(xf)),access='direct',form='binary',recl=1,action='READ',buffered='yes')
   inquire(file=OutFileName,exist=lexist)
   if(lexist) then
     open(unit=20,file=trim(OutFileName),status='old')
     close(20,status='delete')
   endif
   open(unit=20,file=trim(OutFileName),status='new',access='direct',form='binary',recl=1,action='READWRITE',buffered='yes')
   call ReadRAN(xf) 
   write(*,'(2a)') 'Writing Steo RAN output file ',trim(OutFileName)
   call WriteRan
 else
   write(*,'(a,a)') 'Ran File not found: ',trim(InFileName(xf))
   stop 
 endif

 close(10)



write(*,'(a)') 'Finished.'

stop
end

!*******************************************************
subroutine ReadRAN(xf)
   use r_vars
   implicit none
  
   character*6 RGForm_skip

   integer*4 j,i,t8,ByteLoc,XLoc,xf,irow
   integer ios
   subject=' '
   RowTitle=' '
   DumString=' '
   RowStringy=' '
   RowStrings=' '
!  Read the main header.
   errplace='reading header'
   ByteLoc=1
   read(10,rec=ByteLoc,err=99,iostat=ios) RMVer,RMLen,RSLen,RTLen,RDLen

!  Read the scenario header.
   ByteLoc=ByteLoc+RMLen
   RSFile=repeat(' ',150)
   RSScen=repeat(' ',150)
   errplace='reading scenario'
   read(10,rec=ByteLoc,err=99,iostat=ios) RSVer,RSFLen,RSFile(1:RSFLen),RSSLen,RSScen(1:RSSLen)

!Read the table location header.
   ByteLoc=ByteLoc+RSLen
   errplace='reading table location header'
   read(10,rec=ByteLoc,err=99,iostat=ios) RTNum,(RTKey(i),RTTRow(i),RTDRow(i),RTDLoc(i),RTILoc(i),i=1,RTNum)

!Read the data location header.
   do i=1,RTNum
     ByteLoc=RTILoc(i)
     read(10,rec=ByteLoc,err=99,iostat=ios) (RDKey(i,j),RDLoc(i,j),j=1,RTDRow(i))
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

     do while (.not.eof(10).and.ByteLoc.lt.XLoc)
       write(errplace,'(a,i4,a,i4)') 'reading table',i,' row header'
       read(10,rec=ByteLoc,err=999,iostat=ios) RGType,RGSTyp,RGLRem
       ByteLoc=ByteLoc+2+2+2      ! move byte location by what we just read

       if(RGType.eq.1) then
         write(errplace,'(a,i4,a,i4)') 'reading table',i,' Title'
         read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGTAct(i),RGTReg(i),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
         if(RGTTyp.ne.4) Then
           ByteLoc=Byteloc+2+6+2+2+2+6+2+RGTLen
           read(10,rec=Byteloc) RGSLen,Subject(i,RGTTyp)(1:RGSLen)
           ByteLoc=ByteLoc+2+RGSLen
         else
           ByteLoc=ByteLoc+RGLRem
         endif
         TabTitle(i,RGTTyp)=RGText(1:RGTLen)
      
       elseif(RGType.eq.2) then
         irow=irow+1
         RowTitle(i,irow)=' '
         write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 2, row',irow
         read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen)
         ByteLoc=ByteLoc+RGLRem
         RowTitle(i,irow)=RGText(1:RGTLen)
         DataRow(i,irow)=0

       elseif(RGType.eq.3) then
         irow=irow+1
         write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 3, row',irow
         read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGRNum,RGRKey(i,irow),RGTTyp,RGForm(i,irow),RGTLen,RGText(1:RGTLen), &
          RGDTyp(i,irow),RGDFYr(i,irow),RGDLYr(i,irow), & 
          RGData(  1:(RGDLYr(i,irow)-RGDFYr(i,irow)+1),   i,irow),&
              (LenRowStr(j,i,irow), RowStringy(j)(1:LenRowStr(j,i,irow)) , j=1,8)    ! each string, preceded by its length to allow variable-length storage

         do t8=1,8
            RowStrings(t8,i,irow) = RowStringy(t8)(1:512)
         enddo
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
         read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey(i),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
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
   write(6,*) '==========================================================================='
   write(6,*) ' Error in STEORan program.'
   write(6,*) ' Error reading RAN file. The input RAN file is invalid in some way.'
   write(6,*) ' IOSTAT=',ios
   write(6,*) ' RGType=',rgtype
   write(6,*) ' ByteLoc=',byteloc
   write(6,*) ' RMVer=',RMVer
   write(6,*) ' errplace=',trim(errplace)
   write(6,*) '==========================================================================='
   stop ' '
end

!*******************************************************
subroutine WriteRAN
use r_vars
implicit none

   character*6 RGForm_skip,varname*30

  
   integer*4 j,i,t8,ByteLoc,XLoc,irow,LenRowStrs,itab,RGPtr,RDPtr,isteo
  
   integer ios
! Substitute STEO/NEMS comparison for read file name
   RSFile=OutFileName
   RSSCEN=InFileName(2)//' and '//trim(RSScen)
! Write the main header.
    errplace='reading header'
    ByteLoc=1
    write(20,rec=ByteLoc,err=99,iostat=ios) RMVer,RMLen,RSLen,RTLen,RDLen
  
! Write the scenario header.
    ByteLoc=ByteLoc+RMLen
    errplace='writing scenario'
    write(20,rec=ByteLoc,err=99,iostat=ios) RSVer,RSFLen,RSFile(1:RSFLen),RSSLen,RSScen(1:RSSLen)
    
!   Write the table location header.
    ByteLoc=ByteLoc+RSLen
    errplace='writing table location header'
    write(20,rec=ByteLoc,err=99,iostat=ios) RTNum,(RTKey(i),RTTRow(i),RTDRow(i),RTDLoc(i),RTILoc(i),i=1,RTNum)

!   Write the data location header.
    do i=1,RTNum
      ByteLoc=RTILoc(i)
      write(20,rec=ByteLoc,err=99,iostat=ios) (RDKey(i,j),RDLoc(i,j),j=1,RTDRow(i))
    end do
  

    RGTNum=0
    RGForm_skip=repeat(' ',6)                      ! heading format (possible future use?) fill with blanks

    do itab=1,RTNum
!       write(6,*) 'Writing table ',itab
        RGSTyp=1
        RGTNum=itab  ! table sequence number

        RGPtr=RTDLoc(itab)     ! byte pointer to the start of the table within the file
        RDPtr=RTILoc(itab)     ! byte pointer to the start of the data
        RGType=1
        do i=1,4
          RGTTyp=i                                  ! heading Label index (4 heading records per table) 
          RGTLen=len_trim(TabTitle(RGTNum,RGTTyp))  ! length of this heading
          RGText=         TabTitle(RGTNum,RGTTyp)   ! heading text
          RGSLen=max(1,len_trim(Subject(itab,i))) ! length of table subject (short heading for this table)
          if(RGSlen.eq.0) RGSlen=1
          if(i.le.3) then
            RGLRem=RGTLen + 5*2 + 6 + 6 + 2+ RGSLen         ! Length of this record
            write(20,rec=RGPtr) RGType,RGSTyp,RGLRem,RGTNum,RGTKey(itab),RGTAct(itab),RGTReg(itab),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen),RGSLen,  Subject(itab,i)(1:RGSLen)
          else
            RGLRem=RGTLen + 5*2 + 6 + 6 
            write(20,rec=RGPtr) RGType,RGSTyp,RGLRem,RGTNum,RGTKey(itab),RGTAct(itab),RGTReg(itab),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
          endif

          RGPtr=RGPtr + 6 + RGLRem
        end do

        do i=1,RTTRow(itab)
          varname=' '
          RGTNum=itab

          if(DataRow(itab,i).eq.0) then
            !Write out DataType 2 record (Mid-Table Text).
            RGType=2         ! Record type (2 for mid-table text without data (stub only)
            RGSTyp=1
            RGRNum=i      ! row number
            RGTTyp=1
            RGTLen=len_trim(RowTitle(itab,i))
            RGText=RowTitle(itab,i)(1:RGTLen)

            RGLRem=4*2 + 6 + 6 + 16 + RGTLen
            !length of fields   2      2      2      2      6            2      16             2       6             2      RGTLen
            write(20,rec=RGPtr) RGType,RGSTyp,RGLRem,RGTNum,RGTKey(itab),RGRNum,RGRKey(itab,i),RGTTyp,RGForm(itab,i),RGTLen,RGText(1:RGTLen)
            RGPtr=RGPtr + 6 + RGLRem    ! next byte location (number of bytes written so far)
          else
            !Write out DataType 3 record (Data).
            RGType=3        ! Record type (3 for mid-table text with data)
            RGSTyp=1
            RGRNum=i
            RGTTyp=1
            RGTLen=len_trim(RowTitle(itab,i))
            RGText=RowTitle(itab,i)(1:RGTLen)

            LenRowStrs=sum(LenRowStr(1:8,itab,i))          

            RGLRem=4*2 + 6 + 6 + 16 + RGTLen + 3*2 + (RGDLYr(itab,i)-RGDFYr(itab,i)+1)*4 + LenRowStrs + 2*8
            RowStringy(8) = RowStrings(8,itab,i) // DumString(8)
  
 ! Replace NEMS with STEO Data if there is a match
           varname=trim(RGTkey(itab))//':'//trim(RGRKey(itab,i))
           call getsteo(varname,isteo)
            if(isteo.ne.0) then
              do j=1,msteoyrs
                RGData(j,itab,i)=steod(j,isteo)
              enddo
            else
              do j=1,msteoyrs
                RGData(j,itab,i)=RGData(lasthist,itab,i)  ! create dummy values using last history value; line graphs will then display as horizontal
              enddo
            endif

! Hold data constant after last STEO year
            do j=msteoyrs+1,numranyrs
              rgdata(j,itab,i)=rgdata(j-1,itab,i)
            enddo

            write(20,rec=RGPtr) RGType,RGSTyp,RGLRem,RGTNum,RGTKey(itab),RGRNum,RGRKey(itab,i),RGTTyp,RGForm(itab,i), &
             RGTLen,RGText(1:RGTLen), &
             RGDTyp(itab,i),RGDFYr(itab,i),RGDLYr(itab,i), &
             RGData(    1:(RGDLYr(itab,i)-RGDFYr(itab,i)+1)    ,itab,i), &
    ! each string, preceded by its length to allow variable-length storage:
             (LenRowStr(j,itab,i),RowStrings(j,itab,i)(1:LenRowStr(j,itab,i))  , j=1,7), &
    ! do the eighth individually in case it was truncated on entry (caused by extreme array size)
              LenRowStr(8,itab,i),RowStringy(8)(1:LenRowStr(8,itab,i))

            RGPtr=RGPtr + 6 + RGLRem   ! next byte location (number of bytes written so far)
          endif
        end do

        !Add footnotes to the new RAN file.
        do i=1,TotNote(itab)
          !Write out DataType 4 record (Footnote).
          RGType=4        ! Record type (4 for footnotes)
          RGSTyp=1
          RGTTyp=1
          RGTLen=len_trim(FootNote(itab,i))
          RGText=FootNote(itab,i)(1:RGTLen)
          RGLRem=3*2 + 6 + 6 + RGTLen
          write(20,rec=RGPtr) RGType,RGSTyp,RGLRem,RGTNum,RGTKey(itab),RGTTyp,RGForm_skip,RGTLen,RGText(1:RGTLen)
          RGPtr=RGPtr + 6 + RGLRem   ! next byte location (number of bytes written so far)
        end do

    end do ! do itab=1,RTNum
  return
  999 continue
      if(ios.eq.36 .and. i.eq.RTNum) then
        return
      endif
  
  99 Continue
     write(6,*) '==========================================================================='
     write(6,*) ' Error in STEORan program.'
     write(6,*) ' Error writing RAN file. '
     write(6,*) ' IOSTAT=',ios
     write(6,*) ' RGType=',rgtype
     write(6,*) ' ByteLoc=',byteloc
     write(6,*) ' RMVer=',RMVer
     write(6,*) ' errplace=',trim(errplace)
     write(6,*) '==========================================================================='
     stop ' '
end
Subroutine ReadCSV(iunit)
    use r_vars
    implicit none

! CSV file variables--start      
    integer fields(2,maxfield)          ! starting and ending position of each column-field
    character*30 afields(maxfield)      ! character contents of each column-field
    integer cnt,nfield,ifield  
! CSV file variables--end 


    character*500 line
    integer iunit,i

    nsteo=0
    STEOM=' '
    STEOD=0.
    do while (.not. eof(iunit))

      read(iunit,'(a)') line

! Separate the CSV line into the columns-fields.
! 1) determine starting and ending position of each column-field on the CSV line.
        afields(1:maxfield)=' '
        call csv_field(line,fields,maxfield,cnt)  
! 2) collect contents of each column-field into character array AFIELDS().  The Ith field is put
!    in afields(i). The fields are placed in appropriate variables based on the row type (RTYPE).

      nfield=cnt
      do ifield=1,nfield
        call get_csv_field(line,fields,maxfield,ifield,afields(ifield))
      enddo
      if(len_trim(afields(1)).gt.0) then
        nsteo=nsteo+1
        if(nsteo.le.maxsteo) then
          STEOM(nsteo)=afields(1)
          do i=1,msteoyrs
            STEOD(i,nsteo)=0.
            read(afields(i+2),*,end=99,err=99) STEOD(i,nsteo)
          enddo
        else
          write(6,*) 'Too many entries in input file. Increase parameter maxsteo'
          stop ' '
        endif
      endif 
99      continue           
    enddo
return
end subroutine ReadCSV
!*******************************************************
subroutine ReadSngl
use r_vars
implicit none


integer n,XYr

!Input ran file name.
read(10,'(1x)')
read(10,'(1x)')
read(10,'(a)') InFileName(1)

!input a steo file name
read(10,'(1x)')
read(10,'(a)') InFileName(2)

!Output ran file name.
read(10,'(1x)')
read(10,'(a)') OutFileName


return
end
 !==========================================================================
 subroutine csv_field(line,fields,maxfield,cnt)
 implicit none

!  This subroutine counts the comma-delimited fields on a line
! and sets the starting and ending position of each field number 
! 
! line:  character string holding the line of input (intent::in)
! fields: starting and ending position of each of the fields (intent::out)
! maxfield: size of array fields (intent::in)
! cnt:  number of fields found on the line (intent::out)

 integer cnt,maxfield
 integer fields(2,maxfield)
 character*(*) line
 integer line_len,ipos,icomma,iquote
 
 cnt=1
 line_len=len_trim(line)
 ipos=0
 fields=0
 if(line_len.eq.0) then
   cnt=0
   return
 endif
 
 do while (ipos.le.(line_len) .and. cnt .le. maxfield)   ! .and. cnt.le.maxfield)

! set field count, position, and starting position of field

   ipos=ipos+1
   fields(1,cnt)=ipos

   ! if field starts with a " find next "
   if(line(ipos:ipos).eq.'"') then
      iquote=index(line(ipos+1:line_len),'"')
      fields(1,cnt)=ipos+1
      do while(iquote.gt.0)
        if(line(ipos+iquote+1:ipos+iquote+1).eq.'"')then
          ipos=ipos+iquote+2  ! skip double quotes which signify
          iquote=index(line(ipos+1:line_len),'"')
        else
          ipos=ipos+iquote
          iquote=0
        endif
      enddo  
   endif
   ! find next comma
   icomma=index(line(ipos:line_len),',')
   if(icomma.ne.0) then
     fields(2,cnt)=ipos+icomma-2
     if(fields(2,cnt).lt.fields(1,cnt)) fields(2,cnt)=fields(1,cnt)
     cnt=cnt+1
     ipos=ipos+icomma-1

   elseif(line(line_len:line_len).eq.'"') then  ! last field, ends in a "
     fields(2,cnt)=line_len-1
     if(fields(2,cnt).lt.fields(1,cnt)) fields(2,cnt)=fields(1,cnt)
!     cnt=cnt+1
     ipos=line_len+1
   else                                         ! las field, ends in a character is empty 
     fields(2,cnt)=line_len
     if(fields(2,cnt).lt.fields(1,cnt)) fields(2,cnt)=fields(1,cnt)
!     cnt=cnt+1
     ipos=line_len+1
   endif
 
 enddo
 return
 end subroutine csv_field

!*********************************************************************

 subroutine get_csv_field(line,fields,maxfield,ifield,afield)
 implicit none
  character*(*) afield
  character*(*) line
  character*256 a
  integer maxfield
  integer fields(2,maxfield)
  
  integer ifield,i,j,n
  a=' '
  afield=' '
  afield=line(fields(1,ifield):fields(2,ifield))
  if(fields(1,ifield).eq.fields(2,ifield).and. &
    afield(1:1).eq.',') then
      afield=' '
      return
  endif

  n=len_trim(afield)
  if(n.gt.2) then
    if(afield(n:n).eq.'"'.and.(afield(n-1:n-1).ne.'"'.or.afield(n-2:n-1).eq.'""')) then
      afield(n:n)=' '
    endif
    if(afield(1:1).eq.'"'.and.afield(2:2).ne.'"') then
      afield=afield(2:)
    endif
  endif
  i=index(afield,'""')
  do while (i.gt.0)
    if(i.gt.1) then
      afield=afield(:i-1)//afield(i+1:)
    else
      afield=afield(2:)
    endif
    i=index(afield,'""')
  end do
  return
 end subroutine get_csv_field

!*********************************************************************

 subroutine getsteo(varname,isteo)
 use r_vars
 implicit none
 character*(*) varname
 integer i,isteo
 isteo=0
 do i=1,nsteo
   if (trim(varname) .eq. trim(steom(i))) then
     isteo=i
     return
   endif
 enddo
 return
 end
 
