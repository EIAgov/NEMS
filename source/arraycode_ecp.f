! $Header: M:/default/source/RCS/arraycode_ecp.f,v 1.11 2017/04/14 20:46:16 dsa Exp $
  program arraycode
  implicit none

! Writes "parameter_ecp.csv" which can be opened in Excel, along with aimecp.xlsx and copied to worksheet "parameter" to 
! prepare aimecp.xlsx for input.  After copying the new parameters and set names.  Highlight the input area, along with the column headings,
! then select from excel menu "formula...

! Reads an existing ecp.ams code file and writes out new code, auto_ecp.ams with the LP,
! specifications, including set and coefficient declarations, Objective function, Constraints, 
! Variables, Bounds, and RHS coefficients.

integer,parameter::maxarray=1000
integer,parameter::maxdims=400
integer,parameter::maxdim=9
integer,parameter::max_set=6
integer nlist
integer narray
integer ifound

character*1 apy(3)

! 
character*31 allArrays(maxarray)
character*31 allSets(maxdim,maxarray)
integer allArraysDim(maxarray)
integer array_col_id(maxarray)
integer array_row_id(maxarray)

character*50 spaces/'                                                   '/
character*50 array,colnam*12,rownam*12
integer ndim ,i,j,ir,ic, ie,is,la,imatch,rmatch, maxArrayLength
character*31 dim(maxdim),   adim*1, dim_order(maxdim),sumsets(maxdim)
character*10 ans
character*31 alldims(maxdims)
character*300 line

logical lexist

integer num_rows/0/,max_rows
integer num_cols/0/,max_cols
parameter(max_rows=300,max_cols=200)
character*50 rowsets(max_rows,max_set),colsets(max_cols,max_set)
integer numColSets(max_cols)      ! number of sets in the column index space
integer needsol_cols(max_cols)    ! indicator (if <>0) that solution values for the column is needed
integer numRowSets(max_rows)
integer needsol_rows(max_rows)    ! indicator (if <>0) that solution values for the row is needed
logical isBounded(max_cols)
integer numBounds(max_cols)        ! count 1 for a nonzero lower and an upper bound.  if count is 2 (both bounds found, such as with a fixed column), set AIMMS attribute nonvarStatus to -1 to force appearance in mps file
logical First_Time,DoSafety

integer row_id,col_id,L,nsumsets,LT,k,nj,ibm
integer nsets
character*50 setnames(maxdim)

character*20 rowname(max_rows),colname(max_cols)
character*30 rowmask(max_rows),colmask(max_cols) ! OML row and column masks to pass to AIMMS code as comments
character*1 rowtype(max_rows)
integer ireplace, nreplace
type scard
  character*50 sname
  integer card
end type 

integer norder,dim_card(maxdim)

type (scard) :: setcard(47)
logical first,reorder/.false./
integer num_inter(max_rows)

! declarations for local aimecp.xlsx inputs
  integer*2 c_count,r_count,p_count                   ! number of row items for columns, rows, and parameters
  character(LEN=24), allocatable :: xcolnam(:)        ! xlsx AIMMS column    names for colnam_aimms assignments
  character(LEN=30), allocatable :: xcolsetnam(:,:)   ! xlsx AIMMS column    names for colnam_aimms assignments
  character(LEN=30), allocatable :: xcolmask(:)       ! xlsx oml column mask for an AIMMS column
  real(4), allocatable           :: xcolneedsol(:)    ! xlsx AIMMS Column    need solution if =1
  character(LEN=128),allocatable :: xc_description(:) ! xlsx definition/description of AIMMS column
 
  character(LEN=24), allocatable :: xrownam(:)        ! xlsx AIMMS row       names for rownam_aimms assignments
  character(LEN=30), allocatable :: xrowsetnam(:,:)   ! xlsx AIMMS row       names for rownam_aimms assignments
  character(LEN=30), allocatable :: xrowmask(:)       ! xlsx oml row    mask for an AIMMS row
  real(4), allocatable           :: xrowneedsol(:)    ! xlsx AIMMS Column    need solution if =1
  character(LEN=30), allocatable :: xrow_type(:)      ! xlsx row type bound for AIMMS row: E =, L <=, G >=, or N nonconstrained/free
  character(LEN=128),allocatable :: xr_description(:) ! xlsx definition/description of AIMMS row
  
  character(LEN=38), allocatable :: xparamname(:)     ! xlsx AIMMS parameter names 
  character(LEN=30), allocatable :: xparsetnam(:,:)   ! xlsx AIMMS parameter set names 
  character(LEN=16) rname
  integer ixlsx
! end of local xlsx data  

isBounded=.false.
apy=(/'1','2','3'/)
numBounds=0

! there are no ecp rows and columns thatactually use the 4-letter SupplyRegion, so initialize the
! set list with it here.
alldims(1)='SupplyRegion'
nlist=1

 DoSafety=.false. 
 write(6,'(a\)')' Do you want to add safety variables to all constraints? (n) '
 read(5,'(a)') ans
 if(ans.eq.'Y'.or.ans.eq.'y') then
   DoSafety=.true.
 endif

! read AIMMS row and column settings from aimecp.xlsx, also used as an input file for nems/uecp.f
     open(unit=ixlsx,file='aimecp.xlsx',status='old') ! this is just to pass the file name via the unit numbeer to the next subroutine
     call readRngXLSX(ixlsx,'col_row')  ! read sheet "col_row" range names and the data in them
     close(ixlsx)
     rname='C_COUNT'
     call getrngi(rname,c_count,1,1,1)
     rname='R_COUNT'
     call getrngi(rname,r_count,1,1,1)
! allocate arrays to hold aimms column and row definitions
     allocate (xcolnam(       c_count))
     allocate (xcolsetnam(    c_count,max_set))
     allocate (xcolmask(      c_count))
     allocate (xcolneedsol(   c_count))
     allocate (xc_description(c_count))
     allocate (xrownam(       r_count))
     allocate (xrowsetnam(    r_count,max_set))
     allocate (xrowneedsol(   r_count))
     allocate (xrowmask(      r_count))
     allocate (xrow_type(     r_count))
     allocate (xr_description(r_count))
!                                           range,destination,     cols,rows,groups
     rname='COLNAM_AIMMS    '; call getrngc(rname,xcolnam,            1,c_count,1)
     rname='COLNAM_MASK     '; call getrngc(rname,xcolmask,           1,c_count,1)
     rname='CNEEDSOL        '; call getrngr(rname,xcolneedsol,        1,c_count,1)
     rname='CSETNAM1        '; call getrngc(rname,xcolsetnam(1,1),    1,c_count,1)
     rname='CSETNAM2        '; call getrngc(rname,xcolsetnam(1,2),    1,c_count,1)
     rname='CSETNAM3        '; call getrngc(rname,xcolsetnam(1,3),    1,c_count,1)
     rname='CSETNAM4        '; call getrngc(rname,xcolsetnam(1,4),    1,c_count,1)
     rname='CSETNAM5        '; call getrngc(rname,xcolsetnam(1,5),    1,c_count,1)
     rname='CSETNAM6        '; call getrngc(rname,xcolsetnam(1,6),    1,c_count,1)
     rname='C_DESCRIPTION   '; call getrngc(rname,xc_description,     1,c_count,1)
     rname='C_DESCRIPTION   '; call getrngc(rname,xc_description,     1,c_count,1)

     rname='ROWNAM_AIMMS    '; call getrngc(rname,xrownam,            1,r_count,1)
     rname='ROWNAM_MASK     '; call getrngc(rname,xrowmask,           1,r_count,1)
     rname='RNEEDSOL        '; call getrngr(rname,xrowneedsol,        1,r_count,1)
     rname='ROW_TYPE        '; call getrngc(rname,xrow_type,          1,r_count,1)
     rname='RSETNAM1        '; call getrngc(rname,xrowsetnam(1,1),    1,r_count,1)
     rname='RSETNAM2        '; call getrngc(rname,xrowsetnam(1,2),    1,r_count,1)
     rname='RSETNAM3        '; call getrngc(rname,xrowsetnam(1,3),    1,r_count,1)
     rname='RSETNAM4        '; call getrngc(rname,xrowsetnam(1,4),    1,r_count,1)
     rname='RSETNAM5        '; call getrngc(rname,xrowsetnam(1,5),    1,r_count,1)
     rname='RSETNAM6        '; call getrngc(rname,xrowsetnam(1,6),    1,r_count,1)
     rname='R_DESCRIPTION   '; call getrngc(rname,xr_description,     1,r_count,1)

     do num_cols=1,c_count
         colsets(num_cols,1:max_set)=' '
         colname(num_cols)  =xcolnam(num_cols)
         colmask(num_cols)  =xcolmask(num_cols)
         needsol_cols(num_cols)=xcolneedsol(num_cols)
		 write(6,'(1x,a,1x,i5)') colname(num_cols),needsol_cols(num_cols)
         colsets(num_cols,1)=xcolsetnam(num_cols,1)
         colsets(num_cols,2)=xcolsetnam(num_cols,2)
         colsets(num_cols,3)=xcolsetnam(num_cols,3)
         colsets(num_cols,4)=xcolsetnam(num_cols,4)
         colsets(num_cols,5)=xcolsetnam(num_cols,5)
         colsets(num_cols,6)=xcolsetnam(num_cols,6)
         numColSets(num_cols)=0
         do i=1,max_set
           if(len_trim(colsets(num_cols,i)).gt.0) then
             numColSets(num_cols)=numColSets(num_cols)+1
           endif
         enddo
         dim=' '
         ndim=NumColSets(num_cols)
         dim(1:ndim)=colsets(num_cols,1:ndim)
         call dimlist(alldims,maxdims,nlist,dim,ndim)
     enddo
     num_cols=c_count
     
     do num_rows=1,r_count

        rowsets(num_rows,1:max_set)=' '
        rowtype(num_rows)= xrow_type(num_rows)   
        rowname(num_rows)= xrownam(num_rows)
        rowmask(num_rows)=xrowmask(num_rows)
        needsol_rows(num_rows)=xrowneedsol(num_rows)
	    write(6,'(1x,a,1x,i5)') rowname(num_rows),needsol_rows(num_rows)
        rowsets(num_rows,1)=xrowsetnam(num_rows,1)
        rowsets(num_rows,2)=xrowsetnam(num_rows,2)
        rowsets(num_rows,3)=xrowsetnam(num_rows,3)
        rowsets(num_rows,4)=xrowsetnam(num_rows,4)
        rowsets(num_rows,5)=xrowsetnam(num_rows,5)
        rowsets(num_rows,6)=xrowsetnam(num_rows,6)

        numRowSets(num_rows)=0
        
        do i=1,max_set
          if(len_trim(rowsets(num_rows,i)).gt.0) then
            numRowSets(num_rows)=numRowSets(num_rows)+1
          endif
        enddo
        do i=1,NumRowSets(num_rows)
          dim=' '
          ndim=NumRowSets(num_rows)
          dim(1:ndim)=rowsets(num_rows,1:ndim)
          call dimlist(alldims,maxdims,nlist,dim,ndim)
        enddo
     enddo
     num_rows=r_count

    
! Unit 8 is the input file: a list of arrays and their dimensions/sets

! Current: 
! this next file was generated from lines in the debug file ecpcoeff_*.txt (with "missing")
! using the commands in get_ecp_array_list.sh
!
  open(8,file='ecpArrays_all.txt',status='unknown')
  rewind 8
! pre-read the array file to determine the one with the longest length.  This length will be used for formatting.  
9 read(8,*,end=299) array 
    la=len_trim(array)
    if (la.gt. maxArrayLength) then
       maxArrayLength=la
    endif
    goto 9
299 continue

open(19,file='parameter_ecp.csv',status='unknown')
rewind 19

rewind 8
narray=0
10 continue
  read(8,*,end=99) array
  
  if(narray.le.maxarray) then
    narray=narray+1
    allArrays(narray)=trim(array)
  else
    write(6,*)' increase maxarray'
    stop
  endif
  
  if(array(1:3).eq.'RHS') then
    is=1
    ie=index(array(is:),'_')+is-2
  elseif(index(array,'BOUND').gt.0) then
    is=index(array,'_')+1
    ie=index(array(is:),'_LBOUND') + index(array(is:),'_UBOUND') +is-2
    do i=1,num_cols
      if(trim(colname(i)).eq.array(is:ie)) then
        isBounded(i) = .true.
        numBounds(i)=numBounds(i)+1
        exit
      endif
    enddo
    
  elseif(index(array,'_OBJ').gt.0) then
    is=index(array,'_')+1
    ie=index(array(is:),'_OBJ')+is-2
  elseif(index(array,'_ECPCOSTS').gt.0) then
    is=index(array,'_')+1
    ie=index(array(is:),'_ECPCOSTS')+is-2
  else
    is=index(array,'_')+1
    ie=index(array(is:),'_r')+is-2
  endif
  colnam=array(is:ie)
  rownam=array(ie+2:)

  col_id=0
  do i=1,num_cols
    if(colnam.eq.colname(i)) then
      col_id=i
      exit
    endif
  enddo
  if(col_id.eq.0) then
     write(6,'(2a)') 'this colnam not found in colname list:',trim(colnam),' so skipping'
     goto 10
  endif
  array_col_ID(narray)=col_id
  
  row_id=0
  do i=1,num_rows
    if(rownam.eq.rowname(i)) then
      row_id=i
      exit
    endif
  enddo
  if(row_id.eq.0) then
     write(6,'(2a)') 'this rownam not found in rowname list:',trim(rownam),' so skipping'
     goto 10
  endif
  array_row_ID(narray)=row_id
 
  dim(1:maxdim)=' '
  dim_order(1:maxdim)= ' '  ! reordered list.
  dim_card(1:maxdim)=0

  ndim=0
  do i=1,max_set
    if(len_trim(rowsets(row_id,i)).gt.0) then
      ndim=ndim+1
      dim(ndim)=rowsets(row_id,i)
    else
      exit
    endif
  enddo
  do i=1,max_set
    lt=len_trim(colsets(col_id,i))
    if(lt.gt.0) then
      imatch=0
      do j=1,ndim
        if(colsets(col_id,i).eq.dim(j))then
          imatch=1
        endif
      enddo
      if(imatch.eq.0) then
        ndim=ndim+1
        if(ndim.le.maxdim) then
          dim(ndim)=colsets(col_id,i)
        else
          write(6,'(a,i4)') 'too many sets: '//trim(rownam)//',  '//trim(colnam), ndim
          exit
        endif
      endif
    else
      exit
    endif
  enddo
 
 !  reorder list of dims in the array so PlanYear is last
   norder=ndim
   do i=1,ndim
     if (dim(i).eq.'PlanYear') then
       dim_order(norder)=dim(i)
       dim_card(norder)=3
       dim(i)=' '
       exit
     endif
   enddo

   if(dim_order(norder).eq.'PlanYear') then
     j=1
     do i=1,ndim
       if(len_trim(dim(i)).gt.0) then
         dim_order(j)=dim(i)
          j=j+1
       endif
     enddo
     dim=dim_order
   endif

  if(ndim.gt.0) then
    write(19,'(20a)') trim(array),',',(trim(dim(j)),',',j=1,ndim-1),trim(dim(ndim))
  else
    write(19,'(a)') trim(array)
  endif
  allSets(1:ndim,narray)=dim(1:ndim)
  allArraysDim(narray)=ndim
  call dimlist(alldims,maxdims,nlist,dim,ndim)



  
!===================================================================================================================================

  goto 10
 99 continue
  
 close(19)

!
!==============================================================================================================================
! Write AIMMS code to hold the arrays, variables, constraints, objective function, safety values, bounds, etc.
!==============================================================================================================================
 inquire(file='ecp.ams',exist=lexist) 
 ans='y'
 if(lexist) then
   write(6,'(a\)') 'A file ecp.ams was found.  Do you want to copy its front section into auto_ecp.ams (y/n)? [y] '
   read(5,'(a)') ans
   if(ans.eq.' ') then
     ans='y'
   endif
   if(ans.eq.'y') then
     open(8,file='ecp.ams',status='old')
   endif  
 endif
 
 open(9,file='auto_ecp.ams',status='unknown')
 rewind 9
 
  if(ans.eq.'y') then
 5   continue
     read(8,'(a)',end=9) line
     if(index(line,' DeclarationSection Sections_below_this').eq.0) then
       write(9,'(a)') trim(line)
       goto 5
     endif
8    close(8) 
     write(9,'(a)') trim(line)
    endif
 
! write AIMMS code sections
!   DeclarationSection SetDeclare 
  write(9,'(5a)')      '    DeclarationSection SetDeclare {'
  do i=1,nlist
    j=index(alldims(i),'_ALT')
    if(j.eq.0) then
      write(9,'(3a)' )   '     Set ',trim(alldims(i)),'_ {'
      write(9,'(2a,$)' )   '         Index: ',trim(alldims(i)) ! start index line but write without CR to possibly add aliases
      do k=1,nlist  ! look for any alias names and add them to the Index line
        if (k.ne.i) then
          j=index(alldims(k),'_ALT')
          if(j.gt.0) then
            if(alldims(k)(1:j-1).eq.alldims(i)(1:j-1).and. j.ge. len_trim(alldims(i))) then
              write(9,'(2a,$)')   ', ',   trim(alldims(k))
            endif
          endif
        endif
      enddo
      write(9,'(a)') ';'  ! finish Index line
      write(9,'(3a)' )   '     }'  
    endif
  enddo
  write(9,'(5a)')      '   }'

!   DeclarationSection ParameterDeclare
  write(9,'(5a)')         '   DeclarationSection ParameterDeclare {'
  do i=1,narray
    if(index(allArrays(i),'_ECPCOSTS').eq.0 .and. index(allArrays(i),'RHS').ne.1  .and.  index(allArrays(i),'_LBOUND').eq.0 .and. index(allArrays(i),'_UBOUND').eq.0) then
      if(allArraysDim(i).gt.0) then
        write(9,'(30a)' )    '     Parameter ',trim(allArrays(i)),' {'
        write(9,'(30a)' )    '         IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'
        col_id=array_col_ID(i)
        row_id=array_row_ID(i)
        if(col_id.gt.0 .and. row_id.gt.0) then
          write(9,'(30a)')     '         Text: "OML Columns: ',trim(colmask(col_id)),' and OML Rows: ',trim(rowmask(row_id)),'";'
        endif
        write(9,'(30a)' )    '     }'  
      else
        write(6,'(5a)') 'NO set names found for ', trim(allArrays(i)),' so skipping '
      endif
    endif
  enddo
  write(9,'(5a)')         '   }'
!   DeclarationSection ObjectiveCoefficients
  write(9,'(5a)')         '   DeclarationSection ObjectiveCoefficients {'
  do i=1,narray
    if(index(allArrays(i),'_ECPCOSTS').gt.0) then
      if(allArraysDim(i).gt.0) then
        write(9,'(30a)' )    '     Parameter ',trim(allArrays(i)),' {'
        write(9,'(30a)' )    '         IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'
        col_id=array_col_ID(i)
        if(col_id.gt.0) then
          write(9,'(30a)')   '         Text: "OML Columns: ',trim(colmask(col_id)),'";'
        endif
        write(9,'(30a)' )    '     }'  
      else
        write(6,'(5a)') 'NO set names found for ', trim(allArrays(i)),' so skipping '
      endif
    endif
  enddo
  write(9,'(5a)')         '   }'
!   DeclarationSection RHSCoefficients
  write(9,'(5a)')         '   DeclarationSection RHSCoefficients {'
  do i=1,narray
    if(index(allArrays(i),'RHS').eq.1) then
      if(allArraysDim(i).gt.0) then
        write(9,'(30a)' )    '     Parameter ',trim(allArrays(i)),' {'
        write(9,'(30a)' )    '         IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'
        col_id=array_col_ID(i)
        row_id=array_row_ID(i)
        if(len_trim(rowmask(row_id)).gt.0) then 
           write(9,'(30a)' )  '         Text: "OML Rows: ',trim(rowmask(row_id)),'";'
        endif
        write(9,'(30a)' )    '     }'  
      else
        write(6,'(5a)') 'NO set names found for ', trim(allArrays(i)),' so skipping '
      endif
    endif
  enddo
  write(9,'(5a)')         '   }'
!   DeclarationSection VariableDeclare
  write(9,'(5a)')          '   DeclarationSection VariableDeclare {'
  do i=1,num_cols
    nj=numColSets(i)
    if(index(colname(i),'RHS').eq.0) then
      write(9,'(30a)' )    '     Variable ',trim(colname(i)),' {'
      if(nj.gt.0) then
        write(9,'(30a)' )  '         IndexDomain: (',                     (trim(colSets(i,j)),',',j=1,nj-1),trim(colSets(i,nj)),') ;'
      endif
      if(len_trim(colmask(i)).gt.0) then 
         write(9,'(30a)' ) '         Text: "OML Columns: ',trim(colmask(i)),'";'
      endif
      if(isBounded(i)) then
        if(nj.gt.0) then
         write(9,'(40a)' ) '         Range: [','coeff_',trim(colname(i)),'_LBOUND(',(trim(colSets(i,j)),',',j=1,nj-1),trim(colSets(i,nj)),'),', &
                                               'coeff_',trim(colname(i)),'_UBOUND('    ,(trim(colSets(i,j)),',',j=1,nj-1),trim(colSets(i,nj)),')] ;'
        else
         write(9,'(40a)' ) '         Range: [','coeff_',trim(colname(i)),'_LBOUND,', &
                                               'coeff_',trim(colname(i)),'_UBOUND] ;'
        endif
! make sure non-variable, bounded columns (where lower bound = upper bound) show up in cplex mps file instead of being substracted from the RHS of constraints. this is for validation
! comparison to OML mps.
       ibm=index(colname(i),'cCON')+index(colname(i),'cGEN')+index(colname(i),'cCAR')
        if(numBounds(i).eq.2.and.ibm.eq.0) then
!          write(9,'(a)')  '         NonvarStatus: -1;'  ! this has the side effect of AIMMS setting the bounds to zero, so comment out
        endif
      else
         write(9,'(30a)')  '         Range: nonnegative;'
      endif
!!!      if(needsol_cols(i).ne.0) then
        write(9,'(30a)' )  '         Property: ReducedCost, CoefficientRange, Basic;' ! the coefficient turns on .nominalcoeficient, .SmallestCoefficient, and .LargestValue.
!!!      endif
      write(9,'(30a)' )    '     }'  
      if(isBounded(i)) then 
        write(9,'(30a)' )    '     Parameter ','coeff_',trim(colname(i)),'_LBOUND {'
        if(nj.gt.0) then
          write(9,'(30a)' )    '         IndexDomain: (',(trim(colSets(i,j)),',',j=1,nj-1),trim(colSets(i,nj)),') ;'
        endif
        write(9,'(30a)' )    '         Range: nonnegative;'  
        write(9,'(30a)' )    '     }'  
        write(9,'(30a)' )    '     Parameter ','coeff_',trim(colname(i)),'_UBOUND {'
        if(nj.gt.0) then
          write(9,'(30a)' )    '         IndexDomain: (',(trim(colSets(i,j)),',',j=1,nj-1),trim(colSets(i,nj)),') ;'
        endif
        write(9,'(30a)' )    '         Range: nonnegative;'  
        write(9,'(30a)' )    '         Default: inf;'  
        write(9,'(30a)' )    '     }'  
      endif
    endif
  enddo
  write(9,'(5a)')         '   }'


! Declare Safety Valve Variables.  The section name is used to define the set of all safety valve variables
! so they can be included or excluded from the model
write(9,'(a)')  '      DeclarationSection LP_safety_valve_variables {'
if(doSafety) then  
  
  do i=1,num_rows
    nj=numRowSets(i)
    if(trim(rowname(i)).ne.'ECPCOSTS'.and.rowtype(i).ne.'N' .and. rowname(i)(2:6).ne.'BOUND') then
! create one or two safety variable for each row. Skip free/Nonconstrained rows 
      if(rowtype(i).ne.'E') then
        write(9,'(30a)') '            Variable safety_',trim(rowname(i)),' {'
        if(numRowSets(i).gt.0) then
          write(9,'(30a)') '                IndexDomain: (',(trim(rowSets(i,j)),',',j=1,nj-1),trim(rowSets(i,nj)),') ;' 
        endif
        write(9,'(30a)')   '                Range: nonnegative;'
        write(9,'(30a)')   '                Property: ReducedCost, Basic;'
        write(9,'(30a)')   '            }'   
      else       
        write(9,'(30a)') '            Variable safety_',trim(rowname(i)),'_plus {'
        if(numrowSets(i).gt.0) then
          write(9,'(30a)') '                IndexDomain: (',(trim(rowSets(i,j)),',',j=1,nj-1),trim(rowSets(i,nj)),') ;' 
        endif
        write(9,'(30a)')   '                Range: nonnegative;'
        write(9,'(30a)')   '                Property: ReducedCost, Basic;'
        write(9,'(30a)')   '            }'   
        write(9,'(30a)') '            Variable safety_',trim(rowname(i)),'_minus {'
        if(numrowSets(i).gt.0) then
          write(9,'(30a)') '                IndexDomain: (',(trim(rowSets(i,j)),',',j=1,nj-1),trim(rowSets(i,nj)),') ;' 
        endif
        write(9,'(30a)')   '                Range: nonnegative;'
        write(9,'(30a)')   '                Property: ReducedCost, Basic;'
        write(9,'(30a)')   '            }'   

      endif
    endif
  enddo
endif
write(9,'(a)')  '      }'

!   DeclarationSection ObjectiveDeclare 
  write(9,'(a)') '   DeclarationSection Objective_Declaration {                                             '
  write(9,'(a)') '       Parameter K {'
  write(9,'(a)') '       Text: "safety valve OBJ coefficient";'
  write(9,'(a)') '       Range: nonnegative;'
  write(9,'(a)') '       InitialData: 9999;'
  write(9,'(a)') '       }'

  write(9,'(a)')         '       Variable ECPCOSTS {                                                                  '
  write(9,'(a)')         '           Range: free;                                                                   '
  write(9,'(a)')         '           Definition: {                                                                  '
  first_time=.true.
  do i=1,narray
    if(index(allArrays(i),'_ECPCOSTS').gt.0.and.allArraysDim(i).gt.0) then
       array=allArrays(i)
       L=len_trim(array)
       is=index(array,'coeff_')+6   ! 7  
       ie=is+index(array(is:),'_ECPCOSTS')-2
       colnam=array(is:ie)
    
       if(first_time) then
         first_time=.false.
       else
         write(9,'(/a/)')'           +'
       endif
       write(9,'(30a)')  '           ',spaces(:L-4),                              'sum((',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),'),'
       write(9,'(30a)')  '           ',                                  trim(array), '(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),')'
       write(9,'(30a)')  '           ','*',spaces(:L-len_trim(colnam)-1),trim(colnam),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),'))'
    endif
  enddo
 ! add safety terms
if(doSafety) then
  do i=1,num_rows
    nj=numRowSets(i)
    if(trim(rowname(i)).ne.'ECPCOSTS' .and. rowtype(i).ne.'N' .and. rowname(i)(2:6).ne.'BOUND') then
      if(rowtype(i).ne.'E') then
        write(9,'(a)')'           +'
        if(nj.gt.0) then
          write(9,'(30a)')  '              sum((',(trim(rowSets(i,j)),',',j=1,nj-1),trim(rowSets(i,nj)),'), K * safety_',trim(rowname(i)),&
                                           '(',(trim(rowSets(i,j)),',',j=1,nj-1),trim(rowSets(i,nj)),'))'
        else
          write(9,'(30a)')  '              (K * Safety_',trim(rowname(i)),')'
        endif
      elseif(rowtype(i).eq.'E') then
! for equality constraints, include terms for the positive and the negative safety.     
        write(9,'(a)')      '           +'  
        if(nj.gt.0) then
          write(9,'(30a)')  '              sum((',(trim(rowSets(i,j)),',',j=1,nj-1),trim(rowSets(i,nj)),'), K * safety_',trim(rowname(i))//'_plus',&
                                           '(',(trim(rowSets(i,j)),',',j=1,nj-1),trim(rowSets(i,nj)),'))'
        else
          write(9,'(30a)')  '              K * Safety_',trim(rowname(i))//'_plus'
        endif

        write(9,'(a)')      '           +'
        if(nj.gt.0) then
          write(9,'(30a)')  '              sum((',(trim(rowSets(i,j)),',',j=1,nj-1),trim(rowSets(i,nj)),'), K * safety_',trim(rowname(i))//'_minus',&
                                           '(',(trim(rowSets(i,j)),',',j=1,nj-1),trim(rowSets(i,nj)),'))'
        else
          write(9,'(30a)')  '              K * Safety_',trim(rowname(i))//'_minus'
        endif
      else
        write(6,*)' error, i,rowtype,rowname=',i,' ',rowtype(i),' ',trim(rowname(i))
      endif
    endif
  enddo
endif
  write(9,'(a)')         '           }' ! end of "Definition :
  write(9,'(a)')         '       }'     ! end of "Variable ECPCOSTS"
  write(9,'(a)') '   }'                 ! end of "DeclarationSection Objective_Declaration"


! sample
!        Constraint rDV {
!            IndexDomain: (CoalDemandRegion,PlantType_ECP,CoalDiversityType);
! !          Property: Bound, ShadowPrice, Level;
!            Definition: {
!                sum((CoalSupplyCurve,ACI),
!                  coeff_cCT_rDV(CoalSupplyCurve,PlantType_ECP,CoalDiversityType) *
!                  cCT(CoalSupplyCurve,CoalDemandRegion,PlantType_ECP,ACI))
!               
!                +
!                
!                coeff_cDX_rDV(CoalDemandRegion,PlantType_ECP,CoalDiversityType)
!                * cDX(CoalDemandRegion,PlantType_ECP,CoalDiversityType)
!                
!                - safety_rDV(CoalDemandRegion,PlantType_ECP,CoalDiversityType)
!                
!                <=
!                
!                RHS_rDV(CoalDemandRegion,PlantType_ECP,CoalDiversityType)
!            }
!        }

write(9,'(a)') '   DeclarationSection ConstraintDeclare {'
 
  do i=1,num_rows
    if(trim(rowname(i)).ne.'ECPCOSTS' .and. rowtype(i).ne.'N' .and. rowname(i)(2:6).ne.'BOUND') then
      first=.true.   
      num_inter(i)=0     ! number of intersecting columns.  if none found, do not include any definition.
! check for intersecting columns by looking for coefficient arrays with a matching row_id
      do j=1,narray
        col_id=0
        if(array_row_id(j).eq.i) then
          col_id=array_col_id(j)
          if(index(colname(col_id),'RHS').eq.0) then
            num_inter(i)=num_inter(i)+1
            if(num_inter(i).eq.1)then
               write(9,'(30a)') '       Constraint ',trim(rowname(i)),' {'
               if(numrowSets(i).gt.0) then
                 write(9,'(30a)' )'         IndexDomain: (', (trim(rowSets(i,k)),',',k=1,numrowSets(i)-1),trim(rowSets(i,numrowSets(i))),') ;'
               endif
               if(len_trim(rowmask(i)).gt.0) then 
                  write(9,'(30a)' )  '         Text: "OML Rows: ',trim(rowmask(i)),'";'
               endif
!!!            if(needsol_rows(i).ne.0) then
                 write(9,'(30a)')  '        Property: Bound, ShadowPrice, Level;' ! writing as comment since type of constraint will dictate need
!!!            endif
               write(9,'(30a)')  '          Definition: {'
            endif
! check number of sets to include in the sum term.  these are sets in the column that aren't in the row
          nsumsets=0
          sumsets=' '
          do ic=1,numcolsets(col_id)
            rmatch=0
            do ir=1,numrowsets(i)
              if(trim(rowsets(i,ir)).eq.trim(colsets(col_id,ic))  ) then
                rmatch=ic
                exit
              endif
            enddo
            if(rmatch.eq.0) then
              nsumsets=nsumsets+1
              sumsets(nsumsets)=colsets(col_id,ic)
            endif
          enddo
          if(first) then
            first=.false.
          else
            write(9,'(a)')     '              +'
          endif
          if(nsumsets.gt.0) then  ! include a sum statement and lists the sets
             write(9,'(30a)')  '              sum((',(trim(sumsets(ic)),',',ic=1,nsumsets-1),trim(sumsets(nsumsets)),'),'  
             if(allArraysDim(j).gt.0) then
               write(9,'(30a)')  '              ',trim(allArrays(j)),'(', (trim(allSets(ic,j)),',',ic=1,allArraysDim(j)-1),trim(allSets(allArraysDim(j),j)), ') *'
             else
               write(9,'(30a)')  '              ',trim(allArrays(j)),' *'
             endif
          else                    ! no need for any sum statement.  just repeat the column indexdomain
             if(allArraysDim(j).gt.0) then ! write coefficient array and its indexdomain
               write(9,'(30a)')  '              ',trim(allArrays(j)),'(', (trim(allSets(ic,j)),',',ic=1,allArraysDim(j)-1),trim(allSets(allArraysDim(j),j)), ') *'
             else
               write(9,'(30a)')  '              ',trim(allArrays(j)),' *'
             endif
          endif
          if(numcolSets(col_id).gt.0 ) then
            if(nsumsets.gt.0) then
              write(9,'(30a)' )    '                    ',trim(colname(col_id)),'(',(trim(colSets(col_id,ic)),',',ic=1,numColSets(col_id)-1),trim(colSets(col_id,numcolSets(col_id))),'))'
            else
              write(9,'(30a)' )    '                    ',trim(colname(col_id)),'(',(trim(colSets(col_id,ic)),',',ic=1,numColSets(col_id)-1),trim(colSets(col_id,numcolSets(col_id))),')'
            endif
          else
            write(9,'(30a)' )    '                    ',trim(colname(col_id))
          endif
         endif
        endif
      enddo ! of matching columns
! now write the safety term(s), relationship type, and RHS
      if(num_inter(i).gt.0) then
        if(rowtype(i).eq.'E') then ! add plus and minus safety terms for equality constraints
          write(9,'(a)') ' '
          if(doSafety) then  
            if(numrowSets(i).gt.0) then
              write(9,'(30a)')       '              + safety_',trim(rowname(i)),'_plus(' , (trim(rowSets(i,ir)),',',ir=1,numRowSets(i)-1),trim(rowSets(i,numrowSets(i))),')' 
              write(9,'(30a)')       '              - safety_',trim(rowname(i)),'_minus(', (trim(rowSets(i,ir)),',',ir=1,numRowSets(i)-1),trim(rowSets(i,numrowSets(i))),')' 
            else
              write(9,'(30a)')       '              + safety_',trim(rowname(i)),'_plus'
              write(9,'(30a)')       '              - safety_',trim(rowname(i)),'_minus'
            endif
          endif
          write(9,'(a)')           '              ='
        elseif(rowtype(i).eq.'L') then  ! minus safety 
       
          write(9,'(a)') ' '
          if(doSafety) then  
            if(numrowSets(i).gt.0) then
              write(9,'(30a)')       '              - safety_',trim(rowname(i)),'(',(trim(rowSets(i,ir)),',',ir=1,numRowSets(i)-1),trim(rowSets(i,numrowSets(i))),')' 
            else
              write(9,'(30a)')       '              - safety_',trim(rowname(i))
            endif
            write(9,'(a)') ' '
          endif
          write(9,'(a)')           '              <='
          write(9,'(a)') ' '
        elseif(rowtype(i).eq.'G') then  ! plus safety 
          write(9,'(a)') ' '
          if(doSafety) then  
            if(numrowSets(i).gt.0) then
              write(9,'(30a)')       '              + safety_',trim(rowname(i)),'(',(trim(rowSets(i,ir)),',',ir=1,numRowSets(i)-1),trim(rowSets(i,numrowSets(i))),')' 
            else
              write(9,'(30a)')       '              + safety_',trim(rowname(i))
            endif
            write(9,'(a)') ' '
            endif
          write(9,'(a)')           '              >='
          write(9,'(a)') ' '
        endif

! now write the RHS.  Check for a matching RHS and row.  Otherwise write a zero
        imatch=0
        do j=1,narray
          col_id=0
          if(array_row_id(j).eq.i) then ! the row for this coefficient array matches
           col_id=array_col_id(j)
           if(index(colname(col_id),'RHS').gt.0) then
             if(allArraysDim(j).gt.0) then
               write(9,'(30a)')  '              ',trim(allArrays(j)),'(', (trim(allSets(ic,j)),',',ic=1,allArraysDim(j)-1),trim(allSets(allArraysDim(j),j)), ')'
             else
               write(9,'(30a)')  '              ',trim(allArrays(j))
             endif
             imatch=1
             exit
           endif
          endif
        enddo
        if(imatch.eq.0) then ! no RHS coefficient array found.  write zero.
           write(9,'(a)')  '              0.'
        endif
        if(num_inter(i).gt.0) then
          write(9,'(30a)')  '          }'  ! end of Definition: {
        endif
      endif
      if(num_inter(i).gt.0) then
        write(9,'(30a)') '       }' ! end of Constraint: {
      endif
    endif
  enddo

! free rows treated as parameters
  do i=1,num_rows
    if(trim(rowname(i)).ne.'ECPCOSTS'.and.rowtype(i).eq.'N') then ! add free rows as parameters; retain the "r" prefix denoting row in the name  
      num_inter(i)=num_inter(i)+1 
!     write(9,'(30a)' )    '     Variable ',trim(rowname(i)),' {'
      write(9,'(30a)' )    '     Parameter ',trim(rowname(i)),' {'
      if(numrowSets(i).gt.0) then
        write(9,'(30a)' )  '         IndexDomain: (',                     (trim(rowSets(i,j)),',',j=1,numrowSets(i)-1),trim(rowSets(i,numrowSets(i))),') ;'
      endif
      if(len_trim(rowmask(i)).gt.0) then 
        write(9,'(30a)' )  '         Text: "Free row in OML, Rows: ',trim(rowmask(i)),'";'
      endif
 !    write(9,'(30a)' )    '         Property: ReducedCost, CoefficientRange, Basic;' ! the coefficient turns on .nominalcoeficient, .SmallestCoefficient, and .LargestValue.
      write(9,'(30a)')     '         Definition: {'
      first=.true.   
            
! check for intersecting columns by looking for coefficient arrays with a matching row_id
      do j=1,narray
        col_id=0
        if(array_row_id(j).eq.i) then
         col_id=array_col_id(j)
         if(index(colname(col_id),'RHS').eq.0) then
! check number of sets to include in the sum term.  these are sets in the column that aren't in the row
          nsumsets=0
          sumsets=' '
          do ic=1,numcolsets(col_id)
            rmatch=0
            do ir=1,numrowsets(i)
              if(trim(rowsets(i,ir)).eq.trim(colsets(col_id,ic))  ) then
                rmatch=ic
                exit
              endif
            enddo
            if(rmatch.eq.0) then
              nsumsets=nsumsets+1
              sumsets(nsumsets)=colsets(col_id,ic)
            endif
          enddo
          if(first) then
            first=.false.
          else
            write(9,'(a)')     '              +'
          endif
          if(nsumsets.gt.0) then  ! include a sum statement and lists the sets
             write(9,'(30a)')  '              sum((',(trim(sumsets(ic)),',',ic=1,nsumsets-1),trim(sumsets(nsumsets)),'),'  
             if(allArraysDim(j).gt.0) then
               write(9,'(30a)')  '              ',trim(allArrays(j)),'(', (trim(allSets(ic,j)),',',ic=1,allArraysDim(j)-1),trim(allSets(allArraysDim(j),j)), ') *'
             else
               write(9,'(30a)')  '              ',trim(allArrays(j)),' *'
             endif
          else                    ! no need for any sum statement.  just repeat the column indexdomain
             if(allArraysDim(j).gt.0) then ! write coefficient array and its indexdomain
               write(9,'(30a)')  '              ',trim(allArrays(j)),'(', (trim(allSets(ic,j)),',',ic=1,allArraysDim(j)-1),trim(allSets(allArraysDim(j),j)), ') *'
             else
               write(9,'(30a)')  '              ',trim(allArrays(j)),' *'
             endif
          endif
          if(numcolSets(col_id).gt.0 ) then
            if(nsumsets.gt.0) then
              write(9,'(30a)' )    '                    ',trim(colname(col_id)),'(',(trim(colSets(col_id,ic)),',',ic=1,numColSets(col_id)-1),trim(colSets(col_id,numcolSets(col_id))),'))'
            else
              write(9,'(30a)' )    '                    ',trim(colname(col_id)),'(',(trim(colSets(col_id,ic)),',',ic=1,numColSets(col_id)-1),trim(colSets(col_id,numcolSets(col_id))),')'
            endif
          else
            write(9,'(30a)' )    '                    ',trim(colname(col_id))
          endif
         endif
        endif
      enddo ! of matching columns
      
      write(9,'(30a)')  '          }'  ! end of "Definition: {"
      write(9,'(30a)') '       }' ! end of "Parameter {"
    
    endif
  enddo
  
  
  write(9,'(a)') '   }' ! end of "Declaration Section ConstraintDeclare
 
 ! Produce WriteToNEMS procedure with display statements for all columns and rows for which solution values are needed by ecp first; then display the others
 ! needed for Augustine's validation work
   write(9,'(a)') '  Procedure WriteToNEMS {' 
   write(9,'(a)') '    Body: {'
   write(9,'(a)') '      OutToNEMS_FileName:="OutToNEMS_"+formatstring("%i",curcalyr(1))+".txt";'
   write(9,'(a)') '      put OutToNEMS; ! opens the file and sets stage for subsequent display and put statements'
   write(9,'(a)') '      display ECP_WithoutSafety.ProgramStatus ;'
   write(9,'(a)') '      display ECP_WithSafety.ProgramStatus ;'
  
     do i=1,num_cols
       if(needsol_cols(i).ne.0) then
         if(Colname(i).ne.'RHS') then
           if(isBounded(i)) then
             write(9,'(20a)') '      display {',trim(colname(i)),'.level, ',               &
                                                trim(colname(i)),'.NominalCoefficient, ',  &  
                                                'coeff_'//trim(colname(i))//'_LBOUND, ',  &
                                                'coeff_'//trim(colname(i))//'_UBOUND, ',   &
                                                trim(colname(i)),'.ReducedCost} where decimals := 10 ;'
           else
             write(9,'(20a)') '      display {',trim(colname(i)),'.level, ',               &
                                                trim(colname(i)),'.NominalCoefficient, ',  &  
                                                trim(colname(i)),'.lower, ',  &
                                                trim(colname(i)),'.upper, ',   &
                                                trim(colname(i)),'.ReducedCost} where decimals := 10 ;'
           endif
         endif
       endif
     enddo
     
     do i=1,num_rows
       if(needsol_rows(i).ne.0 .and. num_inter(i).gt.0) then
         if(rowtype(i).eq.'N') then ! for free rows implemented as AIMMS Parameters. just write value
              write(9,'(20a)') '      display {',trim(rowname(i)),'} where decimals := 10 ;'  ! free rows implemented as parameters, so just display parameter values
         else
           write(9,'(20a)') '      display {',trim(rowname(i)),'.level, ',  &
                                            trim(rowname(i)),'.lower, ',  &
                                            trim(rowname(i)),'.upper, ',  &
                                            trim(rowname(i)),'.ShadowPrice} where decimals := 10  ;'
         endif
       endif
     enddo
! now repeat the list with the other colums and rows     

     write(9,'(20a)') '  put "! ==========================================================================" / ;'
     write(9,'(20a)') '  put "! Remainder of solution used for validation only" / ;'
     write(9,'(20a)') '  put "! ==========================================================================" / ;'
     write(9,'(20a)') '  put " " / ;' ! need blank line if next is scalar
     write(9,'(20a)')        '  if (AIMECPBG = 1) then'
     do i=1,num_cols
       if(needsol_cols(i).eq.0) then
         if(Colname(i).ne.'RHS') then
           if(isBounded(i)) then
             write(9,'(20a)') '      display {',trim(colname(i)),'.level, ',               &
                                                trim(colname(i)),'.NominalCoefficient, ',  &  
                                                'coeff_'//trim(colname(i))//'_LBOUND, ',  &
                                                'coeff_'//trim(colname(i))//'_UBOUND, ',   &
                                                trim(colname(i)),'.ReducedCost} where decimals := 10 ;'
           else
             write(9,'(20a)') '      display {',trim(colname(i)),'.level, ',               &
                                                trim(colname(i)),'.NominalCoefficient, ',  &  
                                                trim(colname(i)),'.lower, ',  &
                                                trim(colname(i)),'.upper, ',   &
                                                trim(colname(i)),'.ReducedCost} where decimals := 10 ;'
           endif
         endif
       endif
     enddo
     
     do i=1,num_rows
        if(needsol_rows(i).eq.0 .and. num_inter(i).gt.0 .or. rowname(i).eq.'ECPCOSTS') then
          if(rowtype(i).eq.'N') then ! for free rows implemented as AIMMS parameters, add 0s as dummy filler fields
            if(  index('UBOUND LBOUND ',trim(rowname(i))  ) .eq. 0) then
              write(9,'(20a)') '      display {',trim(rowname(i)),'} where decimals := 10 ;'  ! free rows implemented as parameters, so just display parameter values
            endif
          else
            if(index('UBOUND LBOUND ',trim(rowname(i))) .eq. 0) then
              write(9,'(20a)') '      display {',trim(rowname(i)),'.level, ',  &
                                                 trim(rowname(i)),'.lower, ',  &
                                                 trim(rowname(i)),'.upper, ',  &
                                                 trim(rowname(i)),'.ShadowPrice} where decimals := 10  ;'
            endif
          endif
        endif
     
     enddo
     write(9,'(a)'  ) '      endif;'
     write(9,'(20a)') '      putclose;'          
     write(9,'(a)') '    }' 
     write(9,'(a)') '  }' 

   write(6,'(a)') ' '
   write(6,'(a)') 'DIRECTIONS:'
   write(6,'(a)') ' '
   if(ans.eq.'y') then
     write(9,'(a)') '}' 
     write(6,'(a)')'1)  Auto_ecp.ams can now replace ecp.ams in the aimms project folder "MainProject" in ecp.zip.'
     write(6,'(a)')'    So copy auto_ecp.ams to ecp.ams, open ecp.zip in windows explorer, and '
     write(6,'(a)')'    copy ecp.ams into the ecp.zip folder "MainProject"'
   else
     write(6,'(a)')'1)  Auto_ecp.ams should replace the section of ecp.ams beginning with "  DeclarationSection SetDeclare {"'
   endif
   write(6,'(a)')  ' '
   write(6,'(a)')  '2)  A new list of parameter arrays has been created: parameter_ecp.csv'
   write(6,'(a)')  '    Open aimecp.xlxs and parameter_ecp.csv in excel. copy all of parameter_ecp.csv to'
   write(6,'(a)')  '    the area beneath the bolded headers in worksheet parameter in aimecp.xlsx.'
   write(6,'(a)')  ' '
   write(6,'(a)')  '3)  Reset the defined names, indicated by the bolded column headers, in worksheet parameter.'
   write(6,'(a)')  '    An easy way to do it is to highlight the parameter/set names AND the column headers, then'
   write(6,'(a)')  '    choose menu "formulas"...[look in tab "defined names"]..."create from selection"...select "top row".'
      

!========================================================================================================================= 
 stop
 end
 
 subroutine dimlist(alldims,maxdims,nlist,dim,ndim)
 implicit none
 integer maxdims,ndim,nlist
 character*31 alldims(maxdims),dim(ndim)
 integer i,j
 integer ifound
 
 do i=1,ndim
   ifound=0
   do j=1,nlist
     if(alldims(j).eq.dim(i)) then
       ifound=1
       exit
     endif
   enddo
   if(ifound.eq.0)then
      nlist=nlist+1
      alldims(nlist)=dim(i)
   endif
 enddo
 return
 end
 subroutine callsys(iret,cmd)
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
 include 'nemswk1.f'
 include 'cio4wk1.f'

  