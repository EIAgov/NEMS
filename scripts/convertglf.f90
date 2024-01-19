implicit none

real SrData(10,31),SrMax,SrMin
integer SrRANNum(10),SrTNum(10),SrRNum(10)
integer SrNum,SrFYr,SrLYr,SrTyp
character*100 SrLegd(10)
character*150 DataLine
common/SrVars/SrData,SrMax,SrMin,SrRANNum,SrTNum,SrRNum,SrNum,SrFYr,SrLYr,SrTyp,SrLegd,DataLine

integer*2 Size
real XArray(10,300,300,31)
common/RANVar/XArray

real SrBase(31)
integer FTag,TLen,y,RANNum,RANNumIn,i,XLen,TLoc,LocA,LocB,RMode,RUse(10)
character*500 TextLine
character*200 RANAll
character*150 RANFile(10)
character*150 GLFParmFile,GLFName,GDFName,RLegend(10)

!Get the parameter input file name.
call GetArg(1,GLFParmFile,Size)

!Open and read the initialization file.
TLen=len_trim(GLFParmFile)
open(unit=11,file=GLFParmFile(1:TLen))
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
!do i=1,5
! read(11,'(a)') RANFile(i)
! RUse(i)=0
! if(len_trim(RANFile(i)).gt.0) RUse(i)=1
! read(11,'(a)') RLegend(i)
!end do
close(11)

!Open the graph layout input file.
TLen=len_trim(GLFName)
open(unit=15,file=GLFName(1:TLen))
!Open the graph data file output.
FTag=17
TLen=len_trim(GDFName)
open(unit=FTag,file=GDFName(1:TLen))

!Read and write each line of the graph layout file.  If the line starts with the
!string SrData, then write out the actual data.
!Initially set the series type variable to 0.
SrTyp=0
31 read(15,'(a)',end=39) TextLine
32 TLen=len_trim(TextLine)

if(TextLine(1:6).eq.'DfFile') then
 !If RMode=0 then follow the GLF file, if RMode=1 then open RAN file for comparison.
 if(RMode.eq.0) then
  TLoc=index(TextLine,";")
  read(TextLine(11:11),*) RANNum
  !If RUse=0 use the name from the GLF file. If RUse=1 use the name from the parameter file.
  if(RUse(RANNum).eq.0) RANFile(RANNum)=TextLine(TLoc+1:TLen)
  XLen=len_trim(RANFile(RANNum))
  open(unit=10,file=RANFile(RANNum)(1:XLen),form='binary')
  call ReadRAN(RANNum)
  close(10)
 else
  do i=1,10
   if(RUse(i).ne.0) then
    XLen=len_trim(RANFile(i))
    open(unit=10,file=RANFile(i)(1:XLen),form='binary')
    call ReadRAN(i)
    close(10)
   endif
  end do
 endif
elseif(TextLine(1:6).eq.'DfPgFt') then
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
 TextLine=TextLine(8:TLen)
 TLen=len_trim(TextLine)
 if(TLen.gt.0) then
  read(TextLine(1:TLen),*) SrTyp
 endif
elseif(TextLine(1:6).eq.'SrNext') then
 SrNum=1
 read(15,'(7x,a)') SrLegd(SrNum)
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
  do i=1,SrNum
   !if(RUse(i).ne.0) then
    !SrLegd(i)=RLegend(i)
    do y=1,31
     SrData(i,y)=XArray(SrRANNum(i),SrTNum(i),SrRNum(i),y)
    end do
   !endif
  end do
 endif

 if(RMode.ne.0) then
  do i=1,10
   if(RUse(i).ne.0) then
    SrNum=i
    SrLegd(i)=RLegend(i)
    do y=1,31
     SrData(i,y)=XArray(i,SrTNum(1),SrRNum(1),y)
    end do
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
 do i=1,SrNum
  write(FTag,'(a)') 'SrNext-'
  XLen=len_trim(SrLegd(i))
  write(FTag,'(a,a)') 'SrName-',SrLegd(i)(1:XLen)
  write(FTag,'(a,31f13.5)') 'SrVals-',(SrData(i,y),y=SrFYr,SrLYr)
 end do
 !The series for this graph have been written, so reset the series type variable to 0.
 SrTyp=0
 !TextLine was already read so we go directly to label 32.
 goto 32
else
 write(FTag,'(a)') TextLine(1:TLen)
endif

goto 31
39 continue
close(15)
close(FTag)

stop
end

!*******************************************************
subroutine ReadRAN(RANNum)
implicit none
integer RANNum
integer i,j,NLines,DLines,g,s
integer*2 RecType,GTable,FTableNum,Region,HeadNum,RowNum
character*80 Header
character*90 ResName
character*2 DataRow
character*32 RowLab
real RowData(31)
character*166 RestRec0
character*76 RestRec1
character*78 RestRec2
character*124 RestRec3a
character*20 RestRec3b
real XArray(10,300,300,31)
common/RANVar/XArray

DLines=0
do while (.not.eof(10))
 read(10,err=50) RecType
 NLines=NLines+1
 if(RecType.eq.0) then
  read(10) RestRec0
 elseif(RecType.eq.1) then
  read(10) ResName,RestRec1
 elseif(RecType.eq.2) then
  read(10) GTable,FTableNum,Region,HeadNum,Header,RestRec2
 elseif(RecType.eq.3) then
  read(10) GTable,FTableNum,Region,RowNum,DataRow,RowLab
  if(DataRow(1:1).eq.char(255)) then
   read(10) (RowData(j),j=1,31)
  else
   read(10) RestRec3a
  end if
  if(ichar(DataRow(1:1)).ne.0) then
   DLines=DLines+1
   do j=1,31
    XArray(RANNum,GTable,RowNum,j)=RowData(j)
   end do
  end if
 end if
end do
50 continue

return
end

!*******************************************************
subroutine GetData
implicit none
real SrData(10,31),SrMax,SrMin
integer SrRANNum(10),SrTNum(10),SrRNum(10)
integer SrNum,SrFYr,SrLYr,SrTyp
character*100 SrLegd(10)
character*150 DataLine
common/SrVars/SrData,SrMax,SrMin,SrRANNum,SrTNum,SrRNum,SrNum,SrFYr,SrLYr,SrTyp,SrLegd,DataLine

integer TLen,LocA,LocB,LocC,LocD,FYear,LYear

TLen=len_trim(DataLine)
LocA=index(DataLine,";")
LocB=LocA+index(DataLine(LocA+1:TLen),";")
LocC=LocB+index(DataLine(LocB+1:TLen),";")
LocD=LocC+index(DataLine(LocC+1:TLen),"-")
read(DataLine(11:11),*) SrRANNum(SrNum)
read(DataLine(LocA+1:LocB-1),*) SrTNum(SrNum)
read(DataLine(LocB+1:LocC-1),*) SrRNum(SrNum)
read(DataLine(LocC+1:LocD-1),*) FYear
read(DataLine(LocD+1:TLen),*) LYear
if(SrNum.eq.1) then
 SrFYr=FYear-1994
 if(SrFYr.lt.1) SrFYr=1
 if(SrFYr.gt.31) SrFYr=31
 SrLYr=LYear-1994
 if(SrLYr.lt.1) SrLYr=1
 if(SrLYr.gt.31) SrLYr=31
 if(SrFYr.gt.SrLYr) SrFYr=SrLYr
endif

return
end
