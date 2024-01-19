! RanToRtf:  reads a nems/ftab ran file and creates publication quality tables in RTF format
!
! data declaration module r_vars stores common block declartions to avoid repeating and avoid include files
 module r_vars
    implicit none

    integer mTab,mRow
    parameter (mTab=500) ! maximum number of tables stored in ran
    parameter (mRow=400) ! maximum number of rows on any single table
   
 
 ! RANVar1   
   real XArray(5,mTab,mRow,56)
   integer TotRow(mTab),DataRow(mTab,mRow),DataFmt(mTab,mRow),TotNote(mTab),FNum
   common/RANVar1/XArray,TotRow,DataRow,DataFmt,TotNote,FNum
   
 ! RANVar2   
   character*6 RowForm(mTab,mRow)
   character*120 TabTitle(mTab,4)
   character*80 RowTitle(mTab,mRow)
   character*200 FootNote(mTab,100)
   character*2000 FNote(100)
   common/RANVar2/RowForm,TabTitle,RowTitle,FootNote,FNote
 
 ! RANVar3  Variables in headers.
   character*150 RSFile,RSScen
   common/RANVar3/RSFile,RSScen

  
 ! Table1
   integer LWidth,LType,YTop,YMid,YMid2,YBot
   integer XLeft,XRgt,XNVert,XVert(25)
   common/Table1/LWidth,LType,YTop,YMid,YMid2,YBot,XLeft,XRgt,XnVert,XVert
   
   
 ! RTFa
   integer BFtSize
   common/RTFa/BFtSize

 ! RTFb  Variables for printing  
   integer pRTFTabJ(10,25),pRTFTabN(10)
   real pRTFTabI(10,25)
   common/RTFb/pRTFTabI,pRTFTabJ,pRTFTabN
   
 ! RTFc
   integer TabPrs(25),TabTxtN
   character*80 TabTxt(25)
   common/RTFc/TabPrs,TabTxtN,TabTxt
   
 ! RTFd   
   integer PageOri,DoType,NCols
   real TMargIn,BMargIn,LMargIn,RMargIn,BPtSize,PageWidth,RLWidth,GRWidth,TabIncr
   character*100 FontName
   common/RTFd/PageOri,DoType,NCols,TMargIn,BMargIn,LMargIn,RMargIn,BPtSize,PageWidth,RLWidth, &
    GRWidth,TabIncr,FontName
   
  ! RTFe
   integer NFile,NYrs,DispYrs(56),NSngl,SnglYrs(56),FGYr,LGYr,NTabs,RANTab(mTab),PubTab(mtab)
   character*10 TablePre
   character*50 CaseName(5),PageHead
   character*150 InFileName(5),OutFileName,PageFoot
   common/RTFe/NFile,NYrs,DispYrs,NSngl,SnglYrs,FGYr,LGYr,NTabs,RANTab,PubTab, &
    TablePre,CaseName,PageHead,InFileName,OutFileName,PageFoot
  
! Ran file header (not in common)
   character*4 RMVer,RSVer
   integer*4 RMLen,RSLen,RTLen,RDLen,RTDLoc(mTab),RTILoc(mTab),RDLoc(mTab,500)
   integer*2 RSFLen,RSSLen,RTNum,RTTRow(mTab),RTDRow(mTab)
   character*6 RTKey(mTab)
   character*16 RDKey(mTab,500)
   
   character*100 errplace  


end module r_vars

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

open(unit=9,file='aDebug.txt')

!Open and read the control file and use it to set up a number of variables.
open(unit=10,file='controlfile.txt')
read(10,'(a)') TextLine
if(TextLine(1:8).eq.'SnglVer1') then
 call ReadSngl
 DoType=0
 NFile=1
elseif(TextLine(1:8).eq.'CompVer1') then
 call ReadComp
 DoType=1
else
 write(*,'(a)') 'Control file was not recognized.'
endif
close(10)
! Check settings in ftab.dat to see if the ftabbone option (bonus rows) is on (1) or off (0)
ftabbone=1
inquire(file='ftab.dat',exist=lexist)
if(lexist) then
 open(10,file='ftab.dat',status='old',action='read')
 lexist=.false.
 do while (.not. eof(10).and. .not.lexist)
  read(10,'(a)') TextLine
  if(index(TextLine,'bonus rows').gt.0) then
    lexist=.true.
  endif
 end do
 if(lexist)then
  read(TextLine,*,end=77,err=77) ftabbone
  77 continue
 endif
endif
!Open and read the RAN input file(s).
do xf=1,NFile
 write(*,'(a,i1,2a)') 'Read RAN input file number ',xf,' ',trim(InFileName(xf))
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
   open(unit=10,file=trim(InFileName(xf)),access='direct',form='binary',recl=1,action='READ')
   call ReadRAN(xf) 
 else
   write(*,'(a,a)') 'Ran File not found: ',trim(InFileName(xf))
   stop 
 endif

 close(10)
end do
!write(*,*)' Done reading.  Creating Output'
!Open the RTF output file.
open(unit=21,file=trim(OutFileName))

!Calculate a variety of spacing - tabs, table headings, etc.
call DoSpacing

!sRTFHeader writes out the rtf file header information.
call sRTFHeader
call DoPageHead

!Write out the list of tables from the control file.
do t=1,NTabs
 tr=RANTab(t)


 !For all pages except the first, write a new page.
 if(t.ne.1) call sRTFNewPage
 call DrawLine(XLeft,YTop,XRgt,YTop,LWidth)

 !Write the titles at the top of the table on each page.
 call DoTitles(t,tr,0)

 !Write the table headings at the top of the table on each page.
 call DoTHead(t,tr)

 !Go through all the rows and write out according to the row formats.
 do r=1,TotRow(tr)
  if(ftabbone.eq.0 .and. RowForm(tr,r)(6:6).eq.'1') cycle
  !Skip lines or go to new page based upon the row format.
  if(RowForm(tr,r)(4:4).eq.'1') then
   call sRTFSkipLn(1)
  elseif(RowForm(tr,r)(4:4).eq.'2') then
   call sRTFSkipLn(2)
  elseif(RowForm(tr,r)(4:4).eq.'8') then
   call sRTFNewPage
   call DrawLine(XLeft,YTop,XRgt,YTop,LWidth)
   call DoTitles(t,tr,1)
   call DoTHead(t,tr)
  endif

  !Print items where the row format says to print them.
  Skip=0
  if(RowForm(tr,r)(1:1).eq.'x'.or.RowForm(tr,r)(1:1).eq.'X') Skip=1  !Skip always.
  if(RowForm(tr,r)(1:1).eq.'y'.or.RowForm(tr,r)(1:1).eq.'Y') Skip=1  !Skip in formal tables.
  if(Skip.eq.0) then
   TextA=RowTitle(tr,r)
   !Check TextA for footnote numbers - if so put in superscript tags.
   call sNEMFootNum(TextA)
   !sRTFWrText writes out a line of text.  The text is TextA, the
   !point size is given by PtSize, the text alignment is given by
   !Align (1=left, 2=centered, 3=right, 4=indent), the text appearance is given by Apper
   !(1=plain, 2=bold, 3=italic), more to come by Cont (0=no, 1=more), and
   !IndNum is number of inches to indent if applicable.
   !Determine the point size.
   if(RowForm(tr,r)(3:3).eq.'2') PtSize=BPtSize*0.7
   if(RowForm(tr,r)(3:3).eq.'3') PtSize=BPtSize*0.8
   if(RowForm(tr,r)(3:3).eq.'4') PtSize=BPtSize*0.9
   if(RowForm(tr,r)(3:3).eq.'5') PtSize=BPtSize
   if(RowForm(tr,r)(3:3).eq.'6') PtSize=BPtSize*1.11
   if(RowForm(tr,r)(3:3).eq.'7') PtSize=BPtSize*1.25
   if(RowForm(tr,r)(3:3).eq.'8') PtSize=BPtSize*1.43
   !Determine the appearance.
   Apper=1
   if(RowForm(tr,r)(1:1).eq.'b'.or.RowForm(tr,r)(1:1).eq.'B') Apper=2
   if(RowForm(tr,r)(1:1).eq.'i'.or.RowForm(tr,r)(1:1).eq.'I') Apper=3
   !Determine indentation if any (at some point this may also determine alignment).
   write(9,'(a,a)') 'RowForm: ',RowForm(tr,r)
   read(RowForm(tr,r)(2:2),'(f1.0)') IndNum
   if(IndNum.gt.0.0) then
    !Apper=4
    IndNum=IndNum*0.08
   endif
   Align=1
   Cont=0
   if(DataRow(tr,r).eq.0) then  !This is a row with just a label.
    call sRTFWrText(TextA,PtSize,Align,Apper,Cont,IndNum)
   elseif(DataRow(tr,r).eq.1) then  !This is a row with both a label and data.
    call DoDataRow(TextA,PtSize,Align,Apper,IndNum,tr,r)
   endif
  endif
 end do

 call DrawLine(XLeft,YTop,XRgt,YTop,LWidth)
 call CleanFoot(tr) 
 !do r=1,TotNote(t)
 call sRTFSkipLn(1)
 do r=1,FNum
  !If the row title is blank then simply skip a line.
  TLen=len_trim(FNote(r))
  if(TLen.eq.0) then
   call sRTFSkipLn(1)
  else
   !sRTFWrText writes out a line of text.  The text is TextA, the
   !point size is given by PtSize, the text alignment is given by
   !Align (1=left, 2=centered, 3=right, 4=indent), the text appearance is given by Apper
   !(1=plain, 2=bold, 3=italic), more to come by Cont (0=no, 1=more), and
   !IndNum is number of inches to indent if applicable.
   TextA=FNote(r)
   !PtSize=8.0
   PtSize=BPtSize*0.89
   Align=4
   Apper=1
   Cont=0
   IndNum=0.08
   call sRTFWrText(TextA,PtSize,Align,Apper,Cont,IndNum)
  endif
 end do

end do

!Write out the footer for this RTF document.
call sRTFFooter      

close(21)
close(9)
write(*,'(a)') 'Finished.'

stop
end

!*******************************************************
subroutine ReadRAN(xf)
use r_vars
implicit none

 ! Variables from the data section of the ran file (not in common block)
   integer*2 RGType,RGSTyp,RGLRem,RGTNum,RGTAct,RGTReg,RGRNum,RGTTyp,RGTLen,RGDTyp,RGDFYr,RGDLYr
   character*6 RGForm
   character*6 RGTKey
   character*16 RGRKey
   character*800 RGText
   real*4 RGData(56)


integer*4 j,i,ByteLoc,XLoc,xf,irow

integer ios
!Read the main header.
errplace='reading header'
ByteLoc=1
read(10,rec=ByteLoc,err=99,iostat=ios) RMVer,RMLen,RSLen,RTLen,RDLen

!Read the scenario header.
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
 TotRow(i)=RTTRow(i)
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
  ByteLoc=ByteLoc+6
  if(RGType.eq.1) then
   write(errplace,'(a,i4,a,i4)') 'reading table',i,' Title'
   read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey,RGTAct,RGTReg,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen)
   ByteLoc=ByteLoc+RGLRem
   TabTitle(RGTNum,RGTTyp)=RGText(1:RGTLen)
  elseif(RGType.eq.2) then
   irow=irow+1
   write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 2, row',irow
   read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey,RGRNum,RGRKey,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen)
   ByteLoc=ByteLoc+RGLRem
   !write(9,'(a,3i6,2x,a)') 'RowForm: ',RGTNum,RGTAct,irow,RGForm
   RowForm(RGTNum,irow)=RGForm
   RowTitle(RGTNum,RGRNum)=RGText(1:RGTLen)
   DataRow(RGTNum,RGRNum)=0
  elseif(RGType.eq.3) then
   irow=irow+1
   write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 3, row',irow
   read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey,RGRNum,RGRKey,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen), &
    RGDTyp,RGDFYr,RGDLYr,(RGData(1:RGDLYr-RGDFYr+1))
   ByteLoc=ByteLoc+RGLRem
   RowForm(RGTNum,irow)=RGForm
   RowTitle(RGTNum,RGRNum)=RGText(1:RGTLen)
   DataRow(RGTNum,RGRNum)=1
   DataFmt(RGTNum,RGRNum)=RGDTyp
   do j=1,RGDLYr-RGDFYr+1
    XArray(xf,RGTNum,RGRNum,j)=RGData(j)
   end do
  elseif(RGType.eq.4) then
   write(errplace,'(a,i4,a,i4)') 'reading table',i,'RGtype 4, Footnote',totnote(i)+1
   read(10,rec=ByteLoc,err=99,iostat=ios) RGTNum,RGTKey,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen)
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
   write(6,*) ' Error in RanToRTF program.'
   write(6,*) ' Error reading RAN file.  Possibly the old format. To work with this'
   write(6,*) ' program, the ran file must be created by FTAB with option LADYFILE=4.'
   write(6,*) ' Other read errors are possible: the input RAN file is invalid in some way.'
   write(6,*) ' IOSTAT=',ios
   write(6,*) ' RGType=',rgtype
   write(6,*) ' ByteLoc=',byteloc
   write(6,*) ' RMVer=',RMVer
   write(6,*) ' errplace=',trim(errplace)
   write(6,*) '==========================================================================='
   stop ' '
end

!*******************************************************
subroutine DoSpacing
use r_vars
implicit none
real PrintSpace,DataSpace
integer n





!Set up some tab stops ahead of time so they don't have to be done each time.
!pRTFTabI are tab settings in inches from the edge of the paper, and
!there can be up to 25.  pRTFTabJ holds the justification information
!for each setting, with 1=left, 2=center, 3=right, and 4=left with dot
!leaders.  pRTFTabN is the number of tabs set.  Note that these should be
!consistent with printing in portrait or landscape mode (PageOri).
!The dot leaders will stop at RLWidth inches from the margin, and the paper width is
!PageWidth inches minus the right margin and minus the left margin.


!Set up the spacing for tabs and for the table headers. First, determine the data space.
PrintSpace=PageWidth-LMargIn-RMargIn
DataSpace=PrintSpace-RLWidth
!Next, determine how many columns of data are in the table and the increment for each of
!the columns. This depends upon whether it is a single RAN file table (DoType=0) or a
!compare RAN file table (DoType=1). Also, if single, whether a growth rate is to be done (GRWidth<>0).
if(DoType.eq.0) then
 if(GRWidth.ne.0) then
  !Check to see if dividing the space evenly leaves the minimum amount for the growth rate column.
  NCols=NYrs+1
  TabIncr=DataSpace/NCols
  if(TabIncr.lt.GRWidth) TabIncr=(DataSpace-GRWidth)/NYrs
 else
  NCols=NYrs
  TabIncr=DataSpace/NCols
 endif
elseif(DoType.eq.1) then
 NCols=NSngl+(NYrs*NFile) 
 TabIncr=DataSpace/NCols
endif 

!Set up tab #1 for the table headings.
pRTFTabI(1,1)=1.0
pRTFTabJ(1,1)=1

!Set up tab #3 for the lines that include the row labels and dot leaders.
pRTFTabI(3,1)=0.08
pRTFTabJ(3,1)=1
pRTFTabI(3,2)=RLWidth
pRTFTabJ(3,2)=4
do n=1,NCols
 pRTFTabI(3,n+2)=RLWidth+(n*TabIncr)
 pRTFTabJ(3,n+2)=3
end do
pRTFTabN(3)=NCols+2
if(DoType.eq.0.and.GRWidth.gt.0) then
 pRTFTabI(3,NCols+2)=PrintSpace
 pRTFTabJ(3,NCols+2)=3
endif

!Set up the line drawing values for the "header" for each of the tables.
LWidth=12000
LType=20
if(DoType.eq.0) then
 YTop=0
 YMid=300
 YBot=600
else
 YTop=0
 YMid=175
 YMid2=mTab
 YBot=700
endif
XLeft=0  !(Left margin)
if(PageOri.eq.0) XRgt=12240-LMargIn*1440-RMargIn*1440  !(Right margin)
if(PageOri.eq.1) XRgt=15840-LMargIn*1440-RMargIn*1440  !(Right margin)
XNVert=NCols
write(9,'(a,i5)') 'NCols: ',NCols
do n=1,XNVert
 XVert(n)=(RLWidth+((float(n-1)+0.15)*TabIncr))*1440
end do

return
end

!*******************************************************
subroutine DoPageHead
use r_vars
implicit none

integer t,tr,NCt,NRt
character*8 SCt,SRt


write(SCt,'(i8)') (XRgt-XLeft)/2
NCt=index(SCt,' ',back=.true.)+1
write(SRt,'(i8)') XRgt
NRt=index(SRt,' ',back=.true.)+1

write(21,'(a)') '{\headerl \pard\tql\tx1 \fs24\b\tab '//trim(PageHead)//'\par}'
write(21,'(a)') '{\headerr \pard\tqr\tx'//SRt(NRt:8)//' \fs24\b\tab '//trim(PageHead)//'\par}'

write(21,'(a)') '{\footerl \pard\tql\tx1\tqc\tx'//SCt(NCt:8)//' \fs16\b\tab{\field{\fldinst PAGE }{\fldrslt }}'
write(21,'(a)') '\tab '//trim(PageFoot)//'\par}'

write(21,'(a)') '{\footerr \pard\tqc\tx'//SCt(NCt:8)//'\tqr\tx'//SRt(NRt:8)//' \fs16\b\tab '//trim(PageFoot)
write(21,'(a)') '\tab{\field{\fldinst PAGE }{\fldrslt }} \par}'

return
end

!*******************************************************
subroutine DoTitles(t,tr,cnt)
use r_vars
implicit none
integer t,tr,cnt
character*8 STable,STab,SFont1,SFont2
integer NTable,TabSet,NTab,Font1,NFont1,Font2,NFont2

write(STable,'(i8)') PubTab(t)
NTable=index(STable,' ',back=.true.)+1
TabSet=1440
write(STab,'(i8)') TabSet
NTab=index(STab,' ',back=.true.)+1
Font1=(10.0*2.0)+0.5
Font2=(8.5*2.0)+0.5
write(SFont1,'(i8)') Font1
NFont1=index(SFont1,' ',back=.true.)+1
write(SFont2,'(i8)') Font2
NFont2=index(SFont2,' ',back=.true.)+1

if(cnt.eq.0) then
 write(21,'(a,a)') '{\pard \tql\tx'//STab(NTab:8)//' \b\fs'//SFont1(NFont1:8)//' Table '//trim(TablePre), &
  STable(NTable:8)//'.\tab '//trim(TabTitle(tr,1))//'\par}'
else
 write(21,'(a,a)') '{\pard \tql\tx'//STab(NTab:8)//' \b\fs'//SFont1(NFont1:8)//' Table '//trim(TablePre), &
  STable(NTable:8)//'.\tab '//trim(TabTitle(tr,1))//' (Continued)\par}'
endif
write(21,'(a)') '{\pard \tql\tx'//STab(NTab:8)//' \b\fs'//SFont2(NFont2:8)//' \tab '//trim(TabTitle(tr,2))//'\par}'

return
end

!*******************************************************
subroutine sRTFHeader
use r_vars
implicit none

character*8 Buffer
integer mn,dy,yr
character*4 SYr
character*2 SMn,SDy,SHr,SMi


character*8 TMargC,BMargC,LMargC,RMargC
integer TBeg,BBeg,LBeg,RBeg,TInt,BInt,LInt,RInt


!Calculate the RTF base font size based on the point size.
BFtSize=(BPtSize*2.0)+0.5

!Write the header with the font table and font name.
write(21,'(a)') '{\rtf1\ansi\deff0'
write(21,'(a)') '{\fonttbl{\f0\froman '//trim(FontName)//';}}'
!Write header information about the author and document.
write(21,'(a)') '{\info'
write(21,'(a)') '{\title NEMS Report Tables}'
write(21,'(a)') '{\author NEMS RTF Report Writer}'
write(21,'(a)') '{\company Office of Integrated Analysis and Forecasting, EIA}'
!Create dates and times for the header.
call idate(mn,dy,yr)
if(mn.lt.10) write(SMn,'(a,i1)') '0',mn
if(mn.ge.10) write(SMn,'(i2)') mn
if(dy.lt.10) write(SDy,'(a,i1)') '0',dy
if(dy.ge.10) write(SDy,'(i2)') dy
write(SYr,'(i4)') yr+2000
call time(Buffer)
SHr=Buffer(1:2)
SMi=Buffer(4:5)
write(21,'(a)') '{\creatim\yr'//SYr//'\mo'//SMn//'\dy'//SDy//'\hr'//SHr//'\min'//SMi//'}'
write(21,'(a)') '{\doccomm '//trim(RSScen)//'}}'

!If BPgType=0 then portrait orientation, otherwise landscape.
if(PageOri.eq.1) then
 write(21,'(a)') '\paperw15840\paperh12240'
 write(21,'(a)') '\landscape'
else
 write(21,'(a)') '\paperw12240\paperh15840'
end if

!Determine page margins.
TInt=TMargIn*1440
write(TMargC,'(i8)') TInt
TBeg=index(TMargC,' ',back=.true.)+1
BInt=BMargIn*1440
write(BMargC,'(i8)') BInt
BBeg=index(BMargC,' ',back=.true.)+1
LInt=LMargIn*1440
write(LMargC,'(i8)') LInt
LBeg=index(LMargC,' ',back=.true.)+1
RInt=RMargIn*1440
write(RMargC,'(i8)') RInt
RBeg=index(RMargC,' ',back=.true.)+1
write(21,'(a,/)') '\margt'//TMargC(TBeg:8)//'\margb'//BMargC(BBeg:8)//'\margl'//LMargC(LBeg:8)//'\margr'//RMargC(RBeg:8)

!Write this to have "facing" pages so that the headers and footers for odd and even numbered pages are different.
write(21,'(a)') '\facingp'

return
end

!*******************************************************
subroutine sRTFFooter
implicit none

write(21,'(a)') '}'

return
end

!*******************************************************
subroutine sRTFNewPage
implicit none

write(21,'(a)') '\page'

return
end

!*******************************************************
subroutine sRTFSkipLn(XLine)
implicit none

integer XLine,n

do n=1,XLine
 write(21,'(a,i2,a)') '{\pard \par}'
end do

return
end

!*******************************************************
subroutine sRTFWrText(Text,PtSize,Align,Apper,Cont,IndNum)
use r_vars
implicit none
character*(*) Text
character*10 AppChr,AlnChr
character*2000 WText
character*5 FntChr
character*8 IndChr
character*20 FmtChr
integer Align,Apper,Cont,FtSize,TLen,BLen,BLoc,ELoc,TLoc
real PtSize,IndNum

!Set the text alignment.
if(Align.eq.1) AlnChr='\ql'
if(Align.eq.2) AlnChr='\qc'
if(Align.eq.3) AlnChr='\qr'
if(Align.eq.4) then
 write(IndChr,'(i8)') int(1440*IndNum)
 TLoc=index(IndChr,' ',BACK=.true.)+1
 AlnChr='\fi'//IndChr(TLoc:8)
endif
FmtChr=AlnChr

!Set the text appearance.
if(Apper.eq.1) AppChr=''
if(Apper.eq.2) AppChr='\b'
if(Apper.eq.3) AppChr='\i'
if(len_trim(AppChr).gt.0) FmtChr=trim(FmtChr)//trim(AppChr)

!If the point size is not designated use the default point size.
!Calculate the RTF font size, based upon the point size.
if(PtSize.le.0.0) then
 FtSize=BFtSize
else
 FtSize=(PtSize*2.0)+0.5
end if
if(FtSize.lt.10) write(FntChr,'(a,i1)') '\fs0',FtSize
if(FtSize.ge.10) write(FntChr,'(a,i2)') '\fs',FtSize
if(len_trim(FntChr).gt.0) FmtChr=trim(FmtChr)//trim(FntChr)

!Set the working text with text.
WText=Text
!If there are hidden codes in the text, then do the substitutions.
BLoc=index(Text,'^')
if(BLoc.gt.0) then
 ELoc=index(Text(BLoc+1:),'^')+BLoc
 BLen=len_trim(AppChr)+1
 WText=Text(1:BLoc-1)//'}{'//AppChr(:BLen)//'\i '//Text(BLoc+1:ELoc-1)//'}{'//AppChr(:BLen)//Text(ELoc+1:)
end if

!Write out the text.
TLen=len_trim(WText)
write(21,'(a)') '{\pard'//trim(FmtChr)//' '//WText(:TLen)//'\par}'

return
end

!*******************************************************
subroutine DoDataRow(TextA,PtSize,Align,Apper,IndNum,tr,r)
use r_vars
implicit none

integer tr,r,Align,Apper,is,id,y,NDigit,n,xf,IData
real PtSize,IndNum,XBeg,XEnd,XNum,FData,GrRate
character*(*) TextA
character*10 XFmt


!Reset the first tab for the indentation, then write out the tab settings.
pRTFTabI(3,1)=IndNum
if(IndNum.eq.0.0) pRTFTabI(3,1)=0.001
call sRTFTabSet(3)

!Remove leading spaces and trailing dots and use the row title.
is=0
23 is=is+1
if(TextA(is:is).eq.' ') goto 23
id=len_trim(TextA)+1
25 id=id-1
if(TextA(id:id).eq.'.') goto 25
TabTxt(1)=TextA(is:id)
TabTxt(2)=""


!Add the row data to the line of text.
y=0
NDigit=8
if(DataFmt(tr,r).eq.9) then
 if(NDigit.lt.10) write(XFmt,'(a,i1,a)') '(i',NDigit,')'
 if(NDigit.ge.10) write(XFmt,'(a,i2,a)') '(i',NDigit,')'
 if(DoType.eq.0) then
  do n=1,56
   if(DispYrs(n).eq.1) then
   y=y+1
   IData=XArray(1,tr,r,n)
    write(TabTxt(y+2),XFmt) IData
    TabPrs(y+2)=1
   endif
  end do
 elseif(DoType.eq.1) then
  do n=1,56
   if(SnglYrs(n).eq.1) then
    y=y+1       
    IData=XArray(1,tr,r,n)
    write(TabTxt(y+2),XFmt) IData
    TabPrs(y+2)=1
   endif
  end do
  do n=1,56
   if(DispYrs(n).eq.1) then
    do xf=1,NFile
     y=y+1       
     IData=XArray(xf,tr,r,n)
     write(TabTxt(y+2),XFmt) IData
     TabPrs(y+2)=1
    enddo
   endif
  end do
 endif
else
 if(NDigit.lt.10) write(XFmt,'(a,i1,a,i1,a)') '(f',NDigit,'.',DataFmt(tr,r),')'
 if(NDigit.ge.10) write(XFmt,'(a,i2,a,i1,a)') '(f',NDigit,'.',DataFmt(tr,r),')'
 if(DoType.eq.0) then
  do n=1,56
   if(DispYrs(n).eq.1) then
    y=y+1       
    FData=XArray(1,tr,r,n)
    write(TabTxt(y+2),XFmt) FData
    TabPrs(y+2)=1
   endif
  end do
 elseif(DoType.eq.1) then
  do n=1,56
   if(SnglYrs(n).eq.1) then
    y=y+1       
    FData=XArray(1,tr,r,n)
    write(TabTxt(y+2),XFmt) FData
    TabPrs(y+2)=1
   endif
  end do
  do n=1,56
   if(DispYrs(n).eq.1) then
    do xf=1,NFile
     y=y+1       
     FData=XArray(xf,tr,r,n)
     write(TabTxt(y+2),XFmt) FData
     TabPrs(y+2)=1
    enddo
   endif
  end do
 endif
endif

!Add growth rates to the text line if they are supposed to be there.
if(DoType.eq.0.and.GRWidth.ne.0) then
 !Calculate growth rates.
 XBeg=XArray(1,tr,r,FGYr-1994)
 XEnd=XArray(1,tr,r,LGYr-1994)
 XNum=float(LGYr-FGYr)
 y=y+1
 if(XBeg.le.0.0.or.XEnd.le.0.0.or.XNum.le.0.0) then
  TabTxt(y+2)='n/a'
 else
  GrRate=(((XEnd/XBeg)**(1.0/XNum))-1.0)*100.0
  write(TabTxt(y+2),'(f6.1,a1)') GrRate,'%'
 endif
endif
TabTxtN=NCols+2

!Write out the text line.
call sRTFTabText(3,PtSize,Apper)

return
end

!*******************************************************
subroutine sRTFTabSet(tn)
use r_vars
implicit none

integer n,TLoc,tn
character*500 TLine
character*8 TabChr


TLine=repeat(' ',500)
TLine='{\pard'
do n=1,pRTFTabN(tn)
 write(TabChr,'(i8)') int(1440*pRTFTabI(tn,n))
 TLoc=index(TabChr,' ',BACK=.true.)+1
 if(pRTFTabJ(tn,n).eq.1) TLine=trim(TLine)//'\tql\tx'//TabChr(TLoc:8)
 if(pRTFTabJ(tn,n).eq.2) TLine=trim(TLine)//'\tqc\tx'//TabChr(TLoc:8)
 if(pRTFTabJ(tn,n).eq.3) TLine=trim(TLine)//'\tqr\tx'//TabChr(TLoc:8)
 if(pRTFTabJ(tn,n).eq.4) TLine=trim(TLine)//'\tqr\tldot\tx'//TabChr(TLoc:8)
end do

write(21,'(a)') trim(TLine)

return
end

!*******************************************************
subroutine sRTFTabText(tn,PtSize,Apper)
use r_vars
implicit none


integer n,TLen,tn,TLoc
character*8 FntChr
character*2 AppChr
integer Apper,FtSize
real PtSize

!Set the text appearance.
if(Apper.eq.1) AppChr='\p'
if(Apper.eq.2) AppChr='\b'
if(Apper.eq.3) AppChr='\i'

!If the point size is not designated use the default point size.
!Calculate the RTF font size, based upon the point size.
if(PtSize.le.0.0) then
 FtSize=BFtSize
else
 FtSize=(PtSize*2.0)+0.5
end if

write(FntChr,'(i8)') FtSize
TLoc=index(FntChr,' ',BACK=.true.)+1
write(21,'(a,\)') '\fs'//FntChr(TLoc:8)//AppChr//' '

do n=1,TabTxtN
 !Get rid of leading and trailing spaces in the text.
 do TLoc=1,50
  if(TabTxt(n)(TLoc:TLoc).ne.' ') exit
 end do
 TLen=len_trim(TabTxt(n))
 if(pRTFTabJ(tn,n).ne.4) then
  write(21,'(a,\)') '\tab '//TabTxt(n)(TLoc:TLen)
 else
  write(21,'(a,\)') '\tab '
 end if
end do
write(21,'(a)') '\par}'

return
end

!*******************************************************
subroutine DrawLine(XB,YB,XE,YE,LW)
integer XB,YB,XE,YE,LW
character*8 SXB,SYB,SXE,SYE,SLW
integer NXB,NYB,NXE,NYE,NLW

write(SXB,'(i8)') XB
NXB=index(SXB,' ',BACK=.true.)+1
write(SYB,'(i8)') YB
NYB=index(SYB,' ',BACK=.true.)+1
write(SXE,'(i8)') XE
NXE=index(SXE,' ',BACK=.true.)+1
write(SYE,'(i8)') YE
NYE=index(SYE,' ',BACK=.true.)+1
write(SLW,'(i8)') LW
NLW=index(SLW,' ',BACK=.true.)+1

write(21,'(a,a)') '{\shp{\*\shpinst\shpwr3\shpwrk0\shpbypara\shptop'//SYB(NYB:8)//'\shpbottom' &
 //SYE(NYE:8)//'\shpbxmargin\shpleft'//SXB(NXB:8)//'\shpright'//SXE(NXE:8)
write(21,'(a)') '{\sp{\sn lineWidth}{\sv '//SLW(NLW:8)//'}}{\sp{\sn shapeType}{\sv 20}}}}'

return
end

!*******************************************************
subroutine DoTHead(x,xr)
use r_vars
implicit none
integer x,xr

if(DoType.eq.0) then
 if(GRWidth.gt.0) then
  call DoTHead1(x,xr)
 else
  call DoTHead2(x,xr)
 endif
else
 call DoTHead3(x,xr)
endif

return
end

!*******************************************************
subroutine DoTHead1(x,xr)
use r_vars
implicit none
integer x,xr

integer n,i,TabN,TabR(20),TabW(20),XMarL,XMarR,NR(20),NW(20),NTextY,FontN,FtSize
character*8 FontS,BWidth,TMarL,TMarR,TR(20),TW(20)
character*50 TTextL,TTextM,TTextR
character*4 TTextY(25)
character*400 WText
character*4 FXYr,LXYr
character*2 CRLF

CRLF=char(13)//char(10)
FtSize=BPtSize*2.0
write(FontS,'(i8)') FtSize
FontN=index(FontS,' ',back=.true.)+1
TTextL=TabTitle(xr,4)
TTextM=CaseName(1)
write(FXYr,'(i4)') FGYr
write(LXYr,'(i4)') LGYr
TTextR='Annual Growth '//FXYr//'-'//LXYr//' (percent)'
!TTextR='Annual'//CRLF//'Growth'//CRLF//FXYr//'-'//LXYr//CRLF//'(percent)'

i=1
TTextY(i)=''
do n=1,56
 if(DispYrs(n).eq.1) then
  i=i+1
  write(TTextY(i),'(i4)') n+1994
 endif
end do
i=i+1
TTextY(i)=''
NTextY=NYrs+2

BWidth='15'
XMarL=0
TMarL='0'
XMarR=0
TMarR='0'

!Build the top half of the table.
TabN=3
TabR(1)=XVert(1)
TabW(1)=TabR(1)-XMarL
TabR(2)=XVert(XNVert)
TabW(2)=TabR(2)-TabR(1)
TabR(3)=XRgt
TabW(3)=TabR(3)-TabR(2)
do n=1,TabN
 write(TR(n),'(i8)') TabR(n)
 NR(n)=index(TR(n),' ',back=.true.)+1
 write(TW(n),'(i8)') TabW(n)
 NW(n)=index(TW(n),' ',back=.true.)+1
end do

write(21,'(/,a)') '\pard \intbl {\qc\b\fs'//FontS(FontN:8)//' '//trim(TTextL)//'\cell '//trim(TTextM)//'\cell '//trim(TTextR)//'\cell }'

write(21,'(a)') '{\trowd \trgaph'//trim(TMarR)//'\trleft'//trim(TMarL)
write(21,'(a,a)') '\clvmgf\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrnone \clbrdrb\brdrs\brdrw', &
 trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(1)(NW(1):8)//' \cellx'//TR(1)(NR(1):8)
write(21,'(a,a)') '\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
 trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(2)(NW(2):8)//' \cellx'//TR(2)(NR(2):8)
write(21,'(a,a)') '\clvmgf\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
 trim(BWidth)//' \clbrdrr\brdrnone \clwWidth'//TW(3)(NW(3):8)//' \cellx'//TR(3)(NR(3):8)
write(21,'(a)') '\row }'

!Build the bottom half of the table.
do n=1,NTextY
 if(n.eq.NTextY) then
  TabR(n)=XRgt
 else
  TabR(n)=XVert(n)
 endif
 if(n.eq.1) then
  TabW(n)=TabR(n)-XMarL
 else
  TabW(n)=TabR(n)-TabR(n-1)
 endif
 write(TR(n),'(i8)') TabR(n)
 NR(n)=index(TR(n),' ',back=.true.)+1
 write(TW(n),'(i8)') TabW(n)
 NW(n)=index(TW(n),' ',back=.true.)+1
end do

WText='\pard \intbl {\qc\b\fs'//FontS(FontN:8)
do n=1,NTextY
 WText=trim(WText)//' '//trim(TTextY(n))//'\cell'
end do
WText=trim(WText)//'}'
write(21,'(a)') trim(WText)

write(21,'(a)') '{\trowd \trgaph'//trim(TMarR)//'\trleft'//trim(TMarL)
do n=1,NTextY
 if(n.eq.1) then
  write(21,'(a,a)') '\clvmrg\clvertalt\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrnone \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 elseif(n.eq.NTextY) then
  write(21,'(a,a)') '\clvmrg\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth), &
   ' \clbrdrb\brdrs\brdrw'//trim(BWidth)//' \clbrdrr\brdrnone \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 else
  write(21,'(a,a)') '\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 endif
end do
write(21,'(a)') '\row }'

return
end

!*******************************************************
subroutine DoTHead2(x,xr)
use r_vars
implicit none
integer x,xr


integer n,i,TabN,TabR(20),TabW(20),XMarL,XMarR,NR(20),NW(20),NTextY,FontN,FtSize
character*8 FontS,BWidth,TMarL,TMarR,TR(20),TW(20)
character*50 TTextL,TTextM,TTextR
character*4 TTextY(25)
character*400 WText
character*4 FXYr,LXYr

FtSize=BPtSize*2.0
write(FontS,'(i8)') FtSize
FontN=index(FontS,' ',back=.true.)+1
TTextL=TabTitle(xr,4)
TTextM=CaseName(1)

i=1
TTextY(i)=''
do n=1,56
 if(DispYrs(n).eq.1) then
  i=i+1
  write(TTextY(i),'(i4)') n+1994
 endif
end do
i=i+1
NTextY=NYrs+1

BWidth='15'
XMarL=0
TMarL='0'
XMarR=0
TMarR='0'

!Build the top half of the table.
TabN=2
TabR(1)=XVert(1)
TabW(1)=TabR(1)-XMarL
TabR(2)=XRgt
TabW(2)=TabR(2)-TabR(1)
do n=1,TabN
 write(TR(n),'(i8)') TabR(n)
 NR(n)=index(TR(n),' ',back=.true.)+1
 write(TW(n),'(i8)') TabW(n)
 NW(n)=index(TW(n),' ',back=.true.)+1
end do

write(21,'(/,a)') '\pard \intbl {\qc\b\fs'//FontS(FontN:8)//' '//trim(TTextL)//'\cell '//trim(TTextM)//'\cell }'

write(21,'(a)') '{\trowd \trgaph'//trim(TMarR)//'\trleft'//trim(TMarL)
write(21,'(a,a)') '\clvmgf\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrnone \clbrdrb\brdrs\brdrw', &
 trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(1)(NW(1):8)//' \cellx'//TR(1)(NR(1):8)
write(21,'(a,a)') '\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
 trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(2)(NW(2):8)//' \cellx'//TR(2)(NR(2):8)
write(21,'(a)') '\row }'

!Build the bottom half of the table.
do n=1,NTextY
 if(n.eq.NTextY) then
  TabR(n)=XRgt
 else
  TabR(n)=XVert(n)
 endif
 if(n.eq.1) then
  TabW(n)=TabR(n)-XMarL
 else
  TabW(n)=TabR(n)-TabR(n-1)
 endif
 write(TR(n),'(i8)') TabR(n)
 NR(n)=index(TR(n),' ',back=.true.)+1
 write(TW(n),'(i8)') TabW(n)
 NW(n)=index(TW(n),' ',back=.true.)+1
end do

WText='\pard \intbl {\qc\b\fs'//FontS(FontN:8)
do n=1,NTextY
 WText=trim(WText)//' '//trim(TTextY(n))//'\cell'
end do
WText=trim(WText)//'}'
write(21,'(a)') trim(WText)

write(21,'(a)') '{\trowd \trgaph'//trim(TMarR)//'\trleft'//trim(TMarL)
do n=1,NTextY
 if(n.eq.1) then
  write(21,'(a,a)') '\clvmrg\clvertalt\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrnone \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 else
  write(21,'(a,a)') '\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 endif
end do
write(21,'(a)') '\row }'

return
end

!*******************************************************
subroutine DoTHead3(x,xr)
use r_vars
implicit none
integer x,xr


integer n,i,ii,TabN,TabR(20),TabW(20),XMarL,XMarR,NR(20),NW(20),NTextY,FontN,FtSize
character*8 FontS,BWidth,TMarL,TMarR,TR(20),TW(20)
character*50 TTextL,TTextM,TTextR
character*50 TTextY(25)
character*400 WText
character*4 FXYr,LXYr

FtSize=BPtSize*2.0
write(FontS,'(i8)') FtSize
FontN=index(FontS,' ',back=.true.)+1

BWidth='15'
XMarL=0
TMarL='0'
XMarR=0
TMarR='0'

!Build the top third of the table.
do n=1,NSngl+1
 TabR(n)=XVert(n)
 if(n.eq.1) then
  TabW(n)=TabR(n)-XMarL
 else
  TabW(n)=TabR(n)-TabR(1)
 endif
end do
TabN=NSngl+2
TabR(TabN)=XRgt
TabW(TabN)=TabR(TabN)-TabR(TabN-1)
do n=1,TabN
 write(TR(n),'(i8)') TabR(n)
 NR(n)=index(TR(n),' ',back=.true.)+1
 write(TW(n),'(i8)') TabW(n)
 NW(n)=index(TW(n),' ',back=.true.)+1
end do

do n=1,25
 TTextY(n)=repeat(' ',50)
end do
i=1
TTextY(i)=TabTitle(xr,4)
do n=1,56
 if(SnglYrs(n).eq.1) then
  i=i+1
  write(TTextY(i)(1:4),'(i4)') n+1994
 endif
end do
i=i+1
TTextY(i)='Projections'
NTextY=i

WText='\pard \intbl {\qc\b\fs'//FontS(FontN:8)
do n=1,NTextY
 WText=trim(WText)//' '//trim(TTextY(n))//'\cell'
end do
WText=trim(WText)//'}'
write(21,'(a)') trim(WText)

write(21,'(a)') '{\trowd \trgaph'//trim(TMarR)//'\trleft'//trim(TMarL)
do n=1,NTextY
 if(n.eq.1) then
  write(21,'(a,a)') '\clvmgf\clvertalt\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrnone \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 elseif(n.eq.NTextY) then
  write(21,'(a,a)') '\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrnone'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 else
  write(21,'(a,a)') '\clvmgf\clvertalt\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 endif
end do
write(21,'(a)') '\row }'

!Build the middle third of the table.
do n=1,25
 TTextY(n)=repeat(' ',50)
end do
i=1+NSngl
do n=1,56
 if(DispYrs(n).eq.1) then
  i=i+1
  write(TTextY(i)(1:4),'(i4)') n+1994
 endif
end do
NTextY=i
i=0
do n=1,NTextY
 if(n.eq.NTextY) then
  TabR(n)=XRgt
 elseif(n.gt.NSngl+1) then
  i=i+NFile
  TabR(n)=XVert(i)
 else
  i=i+1
  TabR(n)=XVert(i)
 endif
 if(n.eq.1) then
  TabW(n)=TabR(n)-XMarL
 else
  TabW(n)=TabR(n)-TabR(n-1)
 endif
end do
TabN=NTextY
do n=1,TabN
 write(TR(n),'(i8)') TabR(n)
 NR(n)=index(TR(n),' ',back=.true.)+1
 write(TW(n),'(i8)') TabW(n)
 NW(n)=index(TW(n),' ',back=.true.)+1
end do

WText='\pard \intbl {\qc\b\fs'//FontS(FontN:8)
do n=1,NTextY
 WText=trim(WText)//' '//trim(TTextY(n))//'\cell'
end do
WText=trim(WText)//'}'
write(21,'(a)') trim(WText)

write(21,'(a)') '{\trowd \trgaph'//trim(TMarR)//'\trleft'//trim(TMarL)
do n=1,TabN
 if(n.le.NSngl+1) then
  write(21,'(a,a)') '\clvmrg\clvertalt\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrnone \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 elseif(n.eq.TabN) then
  write(21,'(a,a)') '\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrnone \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 else
  write(21,'(a,a)') '\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 endif
end do
write(21,'(a)') '\row }'

!Build the bottom third of the table.
do n=1,25
 TTextY(n)=repeat(' ',50)
end do
i=1+NSngl
do n=1,56
 if(DispYrs(n).eq.1) then
  do ii=1,NFile
   i=i+1
   TTextY(i)=CaseName(ii)
  end do
 endif
end do
NTextY=i

do n=1,NTextY
 if(n.eq.NTextY) then
  TabR(n)=XRgt
 else
  TabR(n)=XVert(n)
 endif
 if(n.eq.1) then
  TabW(n)=TabR(n)-XMarL
 else
  TabW(n)=TabR(n)-TabR(n-1)
 endif
end do
TabN=NTextY
do n=1,TabN
 write(TR(n),'(i8)') TabR(n)
 NR(n)=index(TR(n),' ',back=.true.)+1
 write(TW(n),'(i8)') TabW(n)
 NW(n)=index(TW(n),' ',back=.true.)+1
end do

WText='\pard \intbl {\qc\b\fs'//FontS(FontN:8)
do n=1,NTextY
 WText=trim(WText)//' '//trim(TTextY(n))//'\cell'
end do
WText=trim(WText)//'}'
write(21,'(a)') trim(WText)

write(21,'(a)') '{\trowd \trgaph'//trim(TMarR)//'\trleft'//trim(TMarL)
do n=1,TabN
 if(n.le.NSngl+1) then
  write(21,'(a,a)') '\clvmrg\clvertalt\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrnone \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 elseif(n.eq.TabN) then
  write(21,'(a,a)') '\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrnone \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 else
  write(21,'(a,a)') '\clvertalc\clbrdrt\brdrs\brdrw'//trim(BWidth)//' \clbrdrl\brdrs\brdrw'//trim(BWidth)//' \clbrdrb\brdrs\brdrw', &
   trim(BWidth)//' \clbrdrr\brdrs\brdrw'//trim(BWidth)//' \clwWidth'//TW(n)(NW(n):8)//' \cellx'//TR(n)(NR(n):8)
 endif
end do
write(21,'(a)') '\row }'

return
end

!*******************************************************
subroutine sNEMFootNum(Text)
implicit none
character*(*) Text
character*15 SupChr
integer TLoc,XLen,XLoc,AVal,NLoc

NLoc=0
21 continue
!Find an instance of "/"
TLoc=len_trim(Text)
if(NLoc.lt.TLoc) then
 XLoc=index(Text(NLoc+1:TLoc),"/")
 if(XLoc.gt.0) then
  XLoc=XLoc+NLoc
  NLoc=XLoc
 endif
else
 XLoc=0
endif
!Check to see if one or two of the digits in front are numbers.
if(XLoc.gt.1) then
 XLen=0
 !write(9,'(i4,2x,a,a,a)') XLoc,'>',Text(1:TLoc),'<'
 if(Text(XLoc-1:XLoc-1).ne.' ') then
  AVal=ichar(Text(XLoc-1:XLoc-1))
  if(AVal.ge.48.and.AVal.le.57) then
   XLen=1
   if(XLoc.gt.2) then
    if(Text(XLoc-2:XLoc-2).ne.' ') then
     AVal=ichar(Text(XLoc-2:XLoc-2))
     if(AVal.ge.48.and.AVal.le.57) then
      XLen=2
     end if
    end if
   end if
  end if
 end if
 !Add the tag if XLen>0.
 if(XLen.gt.0) then
  !write(9,'(a,i2)') 'Number Size: ',XLen
  if(XLen.eq.1) then
   SupChr='{\super '//Text(XLoc-1:XLoc-1)//'}'
   if(Text(XLoc-2:XLoc-2).eq.' ') then
    Text=Text(1:XLoc-3)//trim(SupChr)//Text(XLoc+1:TLoc)
   else
    Text=Text(1:XLoc-2)//trim(SupChr)//Text(XLoc+1:TLoc)
   endif
  elseif(XLen.eq.2) then
   SupChr='{\super '//Text(XLoc-2:XLoc-1)//'}'
   if(Text(XLoc-3:XLoc-3).eq.' ') then
    Text=Text(1:XLoc-4)//trim(SupChr)//Text(XLoc+1:TLoc)
   else
    Text=Text(1:XLoc-3)//trim(SupChr)//Text(XLoc+1:TLoc)
   endif
  endif
  !write(9,'(a)') trim(Text)
 end if
endif
!Check to see if there is another slash.
if(XLoc.gt.0) goto 21

return
end

!*******************************************************
subroutine CleanFoot(t)
use r_vars
implicit none
!RAN file variables to be passed to the main routine.

integer r,t,TLoc,BLoc

FNum=0
do r=1,TotNote(t)
 !write(9,'(a,a)') 'Old>',trim(FootNote(t,r))
 if(FootNote(t,r)(1:1).eq.' ') then
  FNum=FNum+1
  BLoc=1
  21 BLoc=BLoc+1
  if(FootNote(t,r)(BLoc:BLoc).eq.' ') goto 21
  TLoc=len_trim(FootNote(t,r))
  FNote(FNum)=FootNote(t,r)(BLoc:TLoc)
 else
  FNote(FNum)=trim(FNote(FNum))//' '//trim(FootNote(t,r))
 endif
 !write(9,'(a,a)') 'New>',trim(FNote(FNum))
end do

do r=1,FNum
 TLoc=len_trim(FNote(r))
 if(FNote(r)(1:5).eq.'Note:') FNote(r)='{\b Notes:}'//FNote(r)(6:TLoc)
 if(FNote(r)(1:6).eq.'Notes:') FNote(r)='{\b Notes:}'//FNote(r)(7:TLoc)
 if(FNote(r)(1:7).eq.'Source:') FNote(r)='{\b Sources:}'//FNote(r)(8:TLoc)
 if(FNote(r)(1:8).eq.'Sources:') FNote(r)='{\b Sources:}'//FNote(r)(9:TLoc)
 if(FNote(r)(1:11).eq.'Projection:') FNote(r)='{\b Projections:}'//FNote(r)(12:TLoc)
 if(FNote(r)(1:12).eq.'Projections:') FNote(r)='{\b Projections:}'//FNote(r)(13:TLoc)
 if(FNote(r)(2:2).eq.'/') then
  if(ichar(FNote(r)(1:1)).ge.48.and.ichar(FNote(r)(1:1)).le.57) then
   if(FNote(r)(3:3).eq.' ') then
    FNote(r)='{\super '//FNote(r)(1:1)//'}'//FNote(r)(4:TLoc)
   else
    FNote(r)='{\super '//FNote(r)(1:1)//'}'//FNote(r)(3:TLoc)
   endif
  endif
 endif
 if(FNote(r)(3:3).eq.'/') then
  if(ichar(FNote(r)(1:1)).ge.48.and.ichar(FNote(r)(1:1)).le.57) then
   if(ichar(FNote(r)(2:2)).ge.48.and.ichar(FNote(r)(2:2)).le.57) then
    if(FNote(r)(4:4).eq.' ') then
     FNote(r)='{\super '//FNote(r)(1:2)//'}'//FNote(r)(5:TLoc)
    else
     FNote(r)='{\super '//FNote(r)(1:2)//'}'//FNote(r)(4:TLoc)
    endif
   endif
  endif
 endif
end do

return
end

!*******************************************************
subroutine ReadSngl
use r_vars
implicit none


integer n,XYr

!Input file name.
read(10,'(1x)')
read(10,'(a)') InFileName(1)
read(10,'(a)') CaseName(1)
!Output file name.
read(10,'(1x)')
read(10,'(a)') OutFileName
!Page header description.
read(10,'(1x)')
read(10,'(a)') PageHead
!Prefix (letter) that prefixes the table number.
read(10,'(1x)')
read(10,'(a)') TablePre
!Page footer description.
read(10,'(1x)')
read(10,'(a)') PageFoot
!Font name.
read(10,'(1x)')
read(10,'(a)') FontName
!Default point size.
read(10,'(1x)')
read(10,*) BPtSize
!Page orientation (0=portrait,1=landscape).
read(10,'(1x)')
read(10,*) PageOri
if(PageOri.eq.0) PageWidth=8.5
if(PageOri.eq.1) PageWidth=11.0
!Top margin in inches.
read(10,'(1x)')
read(10,*) TMargIn
!Bottom margin in inches.
read(10,'(1x)')
read(10,*) BMargIn
!Left margin in inches.
read(10,'(1x)')
read(10,*) LMargIn
!Right margin in inches.
read(10,'(1x)')
read(10,*) RMargIn
!Row label width in inches.
read(10,'(1x)')
read(10,*) RLWidth
!Minimum growth rate column width in inches (0=none).
read(10,'(1x)')
read(10,*) GRWidth
!Number of years to display.
read(10,'(1x)')
read(10,*) NYrs
write(*,'(i5)') NYrs
!Years to display.
read(10,'(1x)')
do n=1,56
 DispYrs(n)=0
end do
do n=1,NYrs
 read(10,*) XYr
 DispYrs(XYr-1994)=1
end do
!First and last year for growth rate.
read(10,'(1x)')
read(10,*) FGYr
read(10,*) LGYr
!Number of tables to display.
read(10,'(1x)')
read(10,*) NTabs
!Tables to display (RAN number and Publish number)
read(10,'(1x)')
do n=1,NTabs
 read(10,*) RANTab(n),PubTab(n)
end do

return
end

!*******************************************************
subroutine ReadComp
use r_vars
implicit none


integer n,XYr

!Number of input files.
read(10,'(1x)')
read(10,*) NFile
!Input file names.
read(10,'(1x)')
do n=1,NFile
 read(10,'(a)') InFileName(n)
 read(10,'(a)') CaseName(n)
enddo
!Output file name.
read(10,'(1x)')
read(10,'(a)') OutFileName
!Page header description.
read(10,'(1x)')
read(10,'(a)') PageHead
!Prefix (letter) that prefixes the table number.
read(10,'(1x)')
read(10,'(a)') TablePre
!Page footer description.
read(10,'(1x)')
read(10,'(a)') PageFoot
!Font name.
read(10,'(1x)')
read(10,'(a)') FontName
!Default point size.
read(10,'(1x)')
read(10,*) BPtSize
!Page orientation (0=portrait,1=landscape).
read(10,'(1x)')
read(10,*) PageOri
if(PageOri.eq.0) PageWidth=8.5
if(PageOri.eq.1) PageWidth=11.0
!Top margin in inches.
read(10,'(1x)')
read(10,*) TMargIn
!Bottom margin in inches.
read(10,'(1x)')
read(10,*) BMargIn
!Left margin in inches.
read(10,'(1x)')
read(10,*) LMargIn
!Right margin in inches.
read(10,'(1x)')
read(10,*) RMargIn
!Row label width in inches.
read(10,'(1x)')
read(10,*) RLWidth
!Number of single years to display.
read(10,'(1x)')
read(10,*) NSngl
!Single years to display.
read(10,'(1x)')
do n=1,56
 SnglYrs(n)=0
end do
do n=1,NSngl
 read(10,*) XYr
 SnglYrs(XYr-1994)=1
end do
!Number of combination years to display.
read(10,'(1x)')
read(10,*) NYrs
!Combination years to display.
read(10,'(1x)')
do n=1,56
 DispYrs(n)=0
end do
do n=1,NYrs
 read(10,*) XYr
 DispYrs(XYr-1994)=1
end do
!Number of tables to display.
read(10,'(1x)')
read(10,*) NTabs
!Tables to display (RAN number and Publish number)
read(10,'(1x)')
do n=1,NTabs
 read(10,*) RANTab(n),PubTab(n)
end do

return
end
