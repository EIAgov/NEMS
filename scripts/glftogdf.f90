! GLFtoGDF
! Part of FGRAPH system.  Used to create FGRAPH/BatchGr "graph data files" (gdf) from
! graph layout files (GLF). The program reads the GLF file and whatever RAN files are
! specified and fills in the data to create the gdf file.
! The output of this program is also used by the FGRAPH.XLS spreadsheet.
!  Program written by John Holte of OnLocation, Inc.
implicit none

real SrData(10,56),SrMax,SrMin
integer SrRANNum(10),SrTNum(10),SrRNum(10)
integer SrNum,SrFYr,SrLYr,SrTyp
integer i35
character*6 SrTKey(10)
character*16 SrRKey(10)
character*100 SrLegd(10)
character*150 DataLine
common/SrVars/SrData,SrMax,SrMin,SrRANNum,SrTNum,SrRNum,SrNum,SrFYr,SrLYr,SrTyp,SrTKey,SrRKey,SrLegd,DataLine

integer*2 Size

real XArray(10,500,400,56)
integer TotTab(10),TotRow(10,500)
character*6 TabKey(10,500)
character*16 RowKey(10,500,400)
common/RANVar/XArray,TotTab,TotRow,TabKey,RowKey

real SrBase(56)
integer FTag,TLen,y,RANNum,RANNumIn,i,XLen,TLoc,LocA,LocB,RMode,RUse(10),XRan,XTNum,XRNum,t,r
character*500 TextLine
character*200 RANAll
character*150 RANFile(10)
character*150 GLFParmFile,GLFName,GDFName,RLegend(10)
character*50 formatt
logical lexist

!Get the parameter input file name.
call GetArg(1,GLFParmFile,Size)

!Open and read the initialization file.
TLen=len_trim(GLFParmFile)
inquire(file=GLFParmFile(1:TLen),exist=lexist)
if(lexist) then
  open(unit=11,file=GLFParmFile(1:TLen),status='old',action='read')
else
  write(*,'(a)') 'missing input file: ' ,GLFParmFile(1:TLen)
  stop
endif
read(11,'(a)') GLFName
read(11,'(a)') GDFName
read(11,*) RMode
do i=1,10
 RUse(i)=0
end do
i=0
11 continue
read(11,'(a)',end=12) TextLine
TLen=len_trim(TextLine)
if(TLen.gt.0) then
 i=i+1
 RUse(i)=1
 RANFile(i)=TextLine(1:TLen)
 read(11,'(a)') RLegend(i)
endif
goto 11
12 continue
close(11)

!Open the graph layout input file.
TLen=len_trim(GLFName)
inquire(file=GLFName(1:TLen),exist=lexist)
if(lexist) then
  open(unit=15,file=GLFName(1:TLen),status='old',action='read')
else
  write(*,'(a)') 'missing input file: ' ,GLFName(1:TLen)
  stop
endif
i35=35
open(unit=i35,file='glfout.txt',status='unknown')  ! New glf with resolved row, table numbers and problem graphs
!i35=6
!Open the graph data file output.
FTag=17
TLen=len_trim(GDFName)
open(unit=FTag,file=GDFName(1:TLen))
inquire(file=GDFName(1:TLen),exist=lexist)
if(lexist) then
  open(unit=FTag,file=GDFName(1:TLen),status='old')
  close (unit=FTag,status='delete')
endif
open(unit=FTag,file=GDFName(1:TLen),status='new')

!Read and write each line of the graph layout file.  If the line starts with the
!string SrData, then write out the actual data.
!Initially set the series type variable to 0.
SrTyp=0
31 read(15,'(a)',end=39) TextLine
32 TLen=len_trim(TextLine)
if(TextLine(1:6).eq.'DfFile') then
 write(i35,'(a)') trim(textline)
 !If RMode=0 then follow the GLF file, if RMode=1 then open RAN file for comparison.
 if(RMode.eq.0) then
  write(i35,*) 'dbg rmode=0'
  TLoc=index(TextLine,";")
  read(TextLine(11:11),*) RANNum
  !If RUse=0 use the name from the GLF file. If RUse=1 use the name from the parameter file.
  if(RUse(RANNum).eq.0) RANFile(RANNum)=TextLine(TLoc+1:TLen)
  XLen=len_trim(RANFile(RANNum))
  inquire(file=RANFile(RANNum)(1:XLen),exist=lexist)
  if(lexist) then
    write(i35,*) 'reading '//trim(ranfile(rannum))
    open(unit=10,file=RANFile(RANNum)(1:XLen),access='direct',form='binary',recl=1,buffered='yes')
    call ReadRAN(RANNum)
    close(10)
  else
    write(*,'(a)') 'RAN input file not found: ',RANFile(RANNum)(1:XLen)
    stop
  endif
 else
  write(i35,*) 'dbg rmode=',rmode
  do i=1,10
   if(RUse(i).ne.0) then
    XLen=len_trim(RANFile(i))
    inquire(file=RANFile(i)(1:XLen),exist=lexist)
    if(lexist) then
      open(unit=10,file=RANFile(i)(1:XLen),access='direct',form='binary',recl=1,buffered='yes')
      write(i35,*) 'reading '//trim(ranfile(i))
      call ReadRAN(i)
      close(10)
    else
      write(*,'(a)') 'RAN input file not found: ',RANFile(i)(1:XLen)
      stop
    endif
   endif
  end do
 endif
elseif(TextLine(1:6).eq.'DfPgFt') then
 write(i35,'(a)') trim(textline)
 XLen=len_trim(RANFile(1))
 RANAll(1:XLen)=RANFile(1)(1:XLen)
 LocB=XLen
 if(RANNum.gt.1) then
  do i=2,RANNum
   LocA=LocB+1
   RANAll(LocA:LocA)=";"
   XLen=len_trim(RANFile(i))
   LocB=LocA+XLen
   LocA=LocA+1
   RANAll(LocA:LocB)=RANFile(i)(1:XLen)
  end do
 endif
 write(FTag,'(a)') 'DfPgFt-NEMS Batch Graph ('//RANAll(1:LocB)//') - Page @'
elseif(TextLine(1:6).eq.'GrVals') then
 write(i35,'(a)') trim(textline)
 TextLine=TextLine(8:TLen)
 TLen=len_trim(TextLine)
 if(TLen.gt.0) then
  read(TextLine(1:TLen),*) SrTyp
 endif
elseif(TextLine(1:6).eq.'SrNext') then
 write(i35,'(a)') trim(textline)
 SrNum=1
 read(15,'(7x,a)') SrLegd(SrNum)
 write(i35,'(a)') 'SrName-'//trim(SrLegd(SrNum))
 read(15,'(a)') DataLine
 Call GetData
 33 read(15,'(a)') TextLine
 if(TextLine(1:6).ne.'SrNext') goto 35
 SrNum=SrNum+1
 if(SrNum.gt.10) goto 35
 read(15,'(7x,a)') SrLegd(SrNum)
 read(15,'(a)') DataLine
 Call GetData
 goto 33
 35 continue

 if(RMode.eq.0) then
  do i=1,10
   XRNum=0
   XTNum=0
   XRan=SrRANNum(i)
   !Check to make sure this lines up with the table keyword and row keyword...
   if(SrTKey(i).eq.TabKey(XRan,SrTNum(i))) then
    XTNum=SrTNum(i)
   else
    do t=1,TotTab(i)
     if(SrTKey(i).eq.TabKey(XRan,t)) then
      XTNum=t
      exit
     endif
    end do
   endif
   if(SrRKey(i).eq.RowKey(XRan,XTNum,SrRNum(i))) then
    XRNum=SrRNum(i)
   else
    do r=1,TotRow(XRan,XTNum)
     if(SrRKey(i).eq.RowKey(XRan,XTNum,r)) then
      XRNum=r
      exit
     endif
    end do
!    write(6,*) 'SrRKey(i)='//SrRKey(i)//' xtnum=',xtnum
   end if
   if(XRNum.ne.0.and.XTNum.ne.0) then
     do y=1,56
      SrData(i,y)=XArray(XRan,XTNum,XRNum,y)
     end do
   endif
  end do
 endif

 if(RMode.ne.0) then ! comparison /multirun 
  do i=1,10
   if(RUse(i).ne.0)then
    XRNum=0
    XTNum=0
    XRan=i
    !Check to make sure this lines up with the table keyword and row keyword...
    if(SrTKey(1).eq.TabKey(XRan,SrTNum(1))) then
      XTNum=SrTNum(1)
    else
      do t=1,TotTab(XRan)
        if(SrTKey(1).eq.TabKey(XRan,t)) then
          XTNum=t
          exit
        endif
      end do
    endif
    if(SrRKey(1).eq.RowKey(XRan,XTNum,SrRNum(1))) then
      XRNum=SrRNum(1)
    else
      do r=1,TotRow(XRan,XTNum)
        if(SrRKey(1).eq.RowKey(XRan,XTNum,r)) then
          XRNum=r
          exit
        endif
      end do
! debug
!      write(6,*) 'SrRKey(1)='//SrRKey(1)//' xtnum=',xtnum
!      do r=1,TotRow(XRan,XTNum)
!        write(6,*) RowKey(XRan,XTNum,r) 
!      end do
      
   end if
    SrNum=i  ! series count
    SrLegd(i)=RLegend(i)
    if(XRNum.ne.0.and.XTNum.ne.0) then
      do y=1,56
        SrData(i,y)=XArray(XRan,XTNum,XRNum,y)
      end do
    else
      if(XRnum.eq.0) write(i35,'(a,i1)') ' Missing Row '//trim(SrRkey(1))//' Table '//SrTKey(1)//', Row '//SrRKey(1)//', Ran# ',XRan
      if(XTnum.eq.0) write(i35,'(a,i1)') ' Missing Table '//SrTKey(1)//', Ran# ',XRan
    endif
   endif
  end do
 endif

 !The series type determines the relationship between the various series in the graph.
 !If SrTyp=1 then make all the series a difference from the first series in each year.
 if(SrTyp.eq.1.and.SrNum.gt.1) then
  do y=SrFYr,SrLYr
   SrBase(y)=SrData(1,y)
  end do
  do i=1,SrNum
   do y=SrFYr,SrLYr
    SrData(i,y)=SrData(i,y)-SrBase(y)
   end do
  end do
 endif
 !If SrTyp=2 then make all the series a percent change from the first series in each year.
 if(SrTyp.eq.2.and.SrNum.gt.1) then
  do y=SrFYr,SrLYr
   SrBase(y)=SrData(1,y)
  end do
  do i=1,SrNum
   do y=SrFYr,SrLYr
    if(SrBase(y).ne.0.0) then
     SrData(i,y)=((SrData(i,y)-SrBase(y))/SrBase(y))*100.0
    else
     SrData(i,y)=0.0
    endif
   end do
  end do
 endif
 !If there are negative values, then set the min graph value to data.
 !(Might also find out how big numbers are so can write out in the correct format.)
 SrMax=-9999999.0
 SrMin=9999999.0
 do i=1,SrNum
  do y=SrFYr,SrLYr
   if(SrData(i,y).gt.SrMax) SrMax=SrData(i,y)
   if(SrData(i,y).lt.SrMin) SrMin=SrData(i,y)
  end do
 end do
 if(SrMin.lt.0.0) write(FTag,'(a)') 'GrYMin-Data'
 !Write out all the series and their legends and data.
 formatt='(a,i'
 if(xtnum.le.9) then
   formatt=trim(formatt)//'1'
 elseif(xtnum.ge.10.and.xtnum.le.99) then
   formatt=trim(formatt)//'2'
 else
   formatt=trim(formatt)//'3'
 endif
 formatt=trim(formatt)//',a,i'
 if(xrnum.le.9) then
   formatt=trim(formatt)//'1'
 elseif(xrnum.ge.10.and.xrnum.le.99) then
   formatt=trim(formatt)//'2'
 else
   formatt=trim(formatt)//'3'
 endif
 formatt=trim(formatt)//',a,i4,a,i4,5a)'
 write(i35,formatt) 'SrData-RAN1;',XTNum,';',XRNum,';',1994+SrFYr,'-',1994+SrLyr,';',SrTKey(1),';',SrRKey(1)
 do i=1,SrNum
  write(FTag,'(a)') 'SrNext-'
  XLen=len_trim(SrLegd(i))
  write(FTag,'(a,a)') 'SrName-',SrLegd(i)(1:XLen)
  write(FTag,'(a,61f13.3)') 'SrVals-',(SrData(i,y),y=SrFYr,SrLYr)
 end do
 !The series for this graph have been written, so reset the series type variable to 0.
 SrTyp=0
 !TextLine was already read so we go directly to label 32.
 goto 32
else
 write(FTag,'(a)') TextLine(1:TLen)
 write(i35,'(a)') textline(1:TLen)
endif

goto 31
39 continue
close(15)
close(FTag)
!close(35,status='delete')
stop
end

!*******************************************************
subroutine ReadRAN(RANNum)
implicit none
integer RANNum

integer*4 j,i,ByteLoc,XLoc

!Variables in headers.
character*4 RMVer,RSVer
integer*4 RMLen,RSLen,RTLen,RDLen,RTDLoc(500),RTILoc(500),RDLoc(500,400)
integer*2 RSFLen,RSSLen,RTNum,RTTRow(500),RTDRow(500)
character*6 RTKey(500)
character*16 RDKey(500,400)
character*150 RSFile,RSScen
!Variables in data section.
integer*2 RGType,RGSTyp,RGLRem,RGTNum,RGTAct,RGTReg,RGRNum,RGTTyp,RGTLen,RGDTyp,RGDFYr,RGDLYr
character*6 RGTKey,RGForm
character*16 RGRKey
character*500 RGText
real*4 RGData(50)

real XArray(10,500,400,56)
integer TotTab(10),TotRow(10,500)
character*6 TabKey(10,500)
character*16 RowKey(10,500,400)
common/RANVar/XArray,TotTab,TotRow,TabKey,RowKey
integer ios

!Read the main header.
ByteLoc=1
read(10,rec=ByteLoc) RMVer,RMLen,RSLen,RTLen,RDLen
!Read the scenario header.
ByteLoc=ByteLoc+RMLen
read(10,rec=ByteLoc) RSVer,RSFLen,RSFile(1:RSFLen),RSSLen,RSScen(1:RSSLen)
!Read the table location header.
ByteLoc=ByteLoc+RSLen
read(10,rec=ByteLoc) RTNum,(RTKey(i),RTTRow(i),RTDRow(i),RTDLoc(i),RTILoc(i),i=1,RTNum)
TotTab(RANNum)=RTNum
!Read the data location header.
do i=1,RTNum
 ByteLoc=RTILoc(i)
 read(10,rec=ByteLoc) (RDKey(i,j),RDLoc(i,j),j=1,RTDRow(i))
end do

!Read through all the tables and write out a debug.
do i=1,RTNum
 TotRow(RANNum,i)=0
 ByteLoc=RTDLoc(i)
 XLoc=RTDLoc(i+1)
 if(i.eq.RTNum) XLoc=RTDLoc(i)*2
 do while (.not.eof(10).and.ByteLoc.lt.XLoc)
  read(10,rec=ByteLoc,err=999,iostat=ios) RGType,RGSTyp,RGLRem
  ByteLoc=ByteLoc+6
  if(RGType.eq.1) then
   read(10,rec=ByteLoc,err=999,iostat=ios) RGTNum,RGTKey,RGTAct,RGTReg,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen)
   ByteLoc=ByteLoc+RGLRem
   if(RGTTyp.eq.1) TabKey(RANNum,RGTNum)=RGTKey
  elseif(RGType.eq.2) then
   read(10,rec=ByteLoc,err=999,iostat=ios) RGTNum,RGTKey,RGRNum,RGRKey,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen)
   ByteLoc=ByteLoc+RGLRem
   if(RGRNum.gt.TotRow(RANNum,i)) TotRow(RANNum,i)=RGRNum
  elseif(RGType.eq.3) then
   read(10,rec=ByteLoc,err=999,iostat=ios) RGTNum,RGTKey,RGRNum,RGRKey,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen), &
    RGDTyp,RGDFYr,RGDLYr,(RGData(j),j=1,RGDLYr-RGDFYr+1)
   ByteLoc=ByteLoc+RGLRem
   if(RGRNum.gt.TotRow(RANNum,i)) TotRow(RANNum,i)=RGRNum
   RowKey(RANNum,RGTNum,RGRNum)=RGRKey
   do j=1,RGDLYr-RGDFYr+1
    XArray(RANNum,RGTNum,RGRNum,j)=RGData(j)
   end do
  elseif(RGType.eq.4) then
   read(10,rec=ByteLoc,err=999,iostat=ios) RGTNum,RGTKey,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen)
   ByteLoc=ByteLoc+RGLRem
  else
   !For now, anything greater than 4 is an error.
   write(*,*) 'ERROR IN FILE'
  endif
 end do
end do

return
999 continue

if(ios.eq.36 .and. i.eq.RTNum) return
Write(6,*) 'error reading ran file,i, RGType, ByteLoc=',i,RGType,ByteLoc
end

!*******************************************************
subroutine GetData
implicit none

real SrData(10,56),SrMax,SrMin
integer SrRANNum(10),SrTNum(10),SrRNum(10)
integer SrNum,SrFYr,SrLYr,SrTyp
character*6 SrTKey(10)
character*16 SrRKey(10)
character*100 SrLegd(10)
character*150 DataLine
common/SrVars/SrData,SrMax,SrMin,SrRANNum,SrTNum,SrRNum,SrNum,SrFYr,SrLYr,SrTyp,SrTKey,SrRKey,SrLegd,DataLine

integer TLen,LocA,LocB,LocC,LocD,LocE,LocF,FYear,LYear

TLen=len_trim(DataLine)
LocA=index(DataLine,";")
LocB=LocA+index(DataLine(LocA+1:TLen),";")
LocC=LocB+index(DataLine(LocB+1:TLen),";")
LocD=LocC+index(DataLine(LocC+1:TLen),"-")
LocE=LocD+index(DataLine(LocD+1:TLen),";")
LocF=LocE+index(DataLine(LocE+1:TLen),";")

read(DataLine(11:11),*) SrRANNum(SrNum)
read(DataLine(LocA+1:LocB-1),*) SrTNum(SrNum)
read(DataLine(LocB+1:LocC-1),*) SrRNum(SrNum)
read(DataLine(LocC+1:LocD-1),*) FYear
read(DataLine(LocD+1:LocE-1),*) LYear
read(DataLine(LocE+1:LocF-1),*) SrTKey(SrNum)
read(DataLine(LocF+1:TLen),'(a)') SrRKey(SrNum)

if(SrNum.eq.1) then
 SrFYr=FYear-1994
 if(SrFYr.lt.1) SrFYr=1
 if(SrFYr.gt.56) SrFYr=56
 SrLYr=LYear-1994
 if(SrLYr.lt.1) SrLYr=1
 if(SrLYr.gt.56) SrLYr=56
 if(SrFYr.gt.SrLYr) SrFYr=SrLYr
endif

return
end
