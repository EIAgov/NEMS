! $Header: m:/default/source/RCS/arraycode.f,v 1.34 2021/05/25 18:33:55 AKN Exp $
  program arraycode
  implicit none
! Prepares AIMMS interface data: list of coefficient arrays and AIMMS LP code for efd or ecp.
!
! NOTE: In this code "VER" means 'ecp' or 'efd' depending on version user selects.
! 
! Writes "parameter_VER.csv" which can be opened in Excel, along with aimVER.xlsx and copied to worksheet "parameter" to 
! prepare aimVER.xlsx for input.  After copying the new parameters and set names.  Highlight the input area, along with the column headings,
! then select from excel menu "formula...

! Reads an existing VER.ams code file and writes out new code, auto_VER.ams with the LP,
! specifications, including set and coefficient declarations, Objective function, Constraints, 
! Variables, Bounds, and RHS coefficients.

integer :: iecp=1
integer :: iefd=2
integer :: iver

integer,parameter::maxarray=3000
integer,parameter::maxdims=500
integer,parameter::maxdim=11
integer,parameter::maxsub=11  ! maximum number of subsets for a superset like PlantType
integer,parameter::max_set(2)=(/6,5/) ! 6 for ecp, 5 for efd
integer nlist
integer narray, ArrayIndex
integer ifound,Laimdim
integer t(7)

character*1 apy(3)
character*3 ver
! 
character*50 allArrays(maxarray), &
             allArrays_coeff(maxarray), &
             allArrays_pass(maxarray),  &
             allArrays_calc(maxarray),  &
             allArrays_compare(maxarray),  &
             allArrays_Percent(maxarray),  &
             allArrays_cdiffPct(maxarray), &
             allArrays_diff(maxarray),  param

character*50 allSets(maxdim,maxarray)
integer allArraysDim(maxarray)
integer array_col_id(maxarray)
integer array_row_id(maxarray)
integer array_declared(maxarray) ! 1 if declaration written

character*50 spaces/'                                                   '/
character*50 array,colnam*25,rownam*25
integer ndim ,i,j,ir,ic, ie,is,la,imatch,rmatch, maxArrayLength
character*50 dim(maxdim),empty(maxdim),   adim*1, dim_order(maxdim),sumsets(maxdim)
character*10 ans
character*50 alldims(maxdims)         ! dimensioning sets
character*50 superset(maxdims)        ! superset(i), if alldims(i) is a subset
character*50 subsets(maxsub,maxdims)  ! list of subsets for a given set alldims(i) (nsubsets(i) counts number in the list, up to maxsub)
integer      nsubsets(maxdims)        ! number of subsets for a given set alldims(i)
character*900 line
character*900 lineleftadj

character*6 prefix(3)/'coeff_','cpass_','ccalc_'/  ! prefix for coefficient arrays: 1:for LP formulation; 2: for coefficients passed from NEMS to AIMMS; 3: for coefficients calculated in AIMMS
character*5 posttag(0:3)/' ','_pass','_pass','_pass'/       ! prefix for text string used in transfer_out calls based on tCodeUsage string (0:RHS, 1:LHS, 2:LHS)
character*5 post 

integer passed(maxarray)! indicator whether to use the passed coefficient array (cpass_ or the calculated array in assigning values to coeff_ array
integer ipass ! moved to passed(numarray)
 
logical lexist, USE_AIM_SLNADJ

integer num_rows/0/,max_rows
integer num_cols/0/,max_cols
parameter(max_rows=400,max_cols=400)
character*50 rowsets(max_rows,max_set(1)),colsets(max_cols,max_set(1))
integer numColSets(max_cols)      ! number of sets in the column index space
integer needsol_cols(max_cols)    ! indicator (if <>0) that solution values for the column is needed
integer numRowSets(max_rows)
integer nRowSets  ! used for counting the number of sets for a single row
integer nextDim   ! used to indicate next position to use in parameter set list
integer needsol_rows(max_rows)    ! indicator (if <>0) that solution values for the row is needed
logical isBounded(max_cols)
integer numBounds(max_cols)        ! count 1 for a nonzero lower and an upper bound.  if count is 2 (both bounds found, such as with a fixed column), set AIMMS attribute nonvarStatus to -1 to force appearance in mps file
logical First_Time,DoSafety

integer row_id,col_id,L,nsumsets,LT,k,nj,ibm,m,itest
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

! declarations for local aimVER.xlsx inputs
  integer*4 c_count,r_count,p_count                   ! number of row items for columns, rows, and parameters
  character(LEN=24), allocatable :: xcolnam(:)        ! xlsx AIMMS column    names for colnam_aimms assignments
  character(LEN=30), allocatable :: xcolsetnam(:,:)   ! xlsx AIMMS column    names for colnam_aimms assignments
  character(LEN=30), allocatable :: xcolmask(:)       ! xlsx oml column mask for an AIMMS column
  real(4), allocatable           :: xcolneedsol(:)    ! xlsx AIMMS Column    need solution if =1
  character(LEN=150),allocatable :: xc_comment(:)     ! xlsx comment of AIMMS column
 
  character(LEN=24), allocatable :: xrownam(:)        ! xlsx AIMMS row       names for rownam_aimms assignments
  character(LEN=30), allocatable :: xrowsetnam(:,:)   ! xlsx AIMMS row       names for rownam_aimms assignments
  character(LEN=30), allocatable :: xrowmask(:)       ! xlsx oml row    mask for an AIMMS row
  real(4), allocatable           :: xrowneedsol(:)    ! xlsx AIMMS Column    need solution if =1
  character(LEN=30), allocatable :: xrow_type(:)      ! xlsx row type bound for AIMMS row: E =, L <=, G >=, or N nonconstrained/free
  character(LEN=150),allocatable :: xr_comment(:)     ! xlsx comment of AIMMS row
  
  character(LEN=38), allocatable :: xparamname(:)     ! xlsx AIMMS parameter names 
  character(LEN=30), allocatable :: xparsetnam(:,:)   ! xlsx AIMMS parameter set names 
  character(LEN=16) rname
  character(LEN=32) setZero_passback
  integer ixlsx
  integer SetLookCount
  character(LEN=50), allocatable :: xSetLook(:,:)     ! xlsx Additional sets in dimension look up table for transfer variables
  
! VERTransfer WorkSheet 
  integer TransferCount  ! size of arrays to allocate
  character(LEN=35), allocatable :: tFortranVariable(:)     ! xlsx 
  character(LEN=30), allocatable :: tIncludeFile(:)     ! xlsx 
  character(LEN=30), allocatable :: tCodeUsage(:)     ! xlsx 
  character(LEN=30), allocatable :: tHistoricalYears(:)     ! xlsx 
  character(LEN=30), allocatable :: tFortType(:)     ! xlsx 	
  character(LEN=30), allocatable :: tNoGenCode(:)     ! xlsx    !added by AKN 5/25/22
  character(LEN=150),allocatable :: tFortDescription(:)     ! xlsx 	
  character(LEN=35), allocatable :: tFortDim1(:)     ! xlsx 	
  character(LEN=35), allocatable :: tFortDim2(:)     ! xlsx 	
  character(LEN=35), allocatable :: tFortDim3(:)     ! xlsx 	
  character(LEN=35), allocatable :: tFortDim4(:)     ! xlsx 	
  character(LEN=35), allocatable :: tFortDim5(:)     ! xlsx 	
  character(LEN=35), allocatable :: tDAFDim1(:)     ! xlsx 	
  character(LEN=35), allocatable :: tDAFDim2(:)     ! xlsx 	
  character(LEN=35), allocatable :: tAimDim1(:)     ! xlsx 	
  character(LEN=35), allocatable :: tAimDim2(:)     ! xlsx 	
  character(LEN=35), allocatable :: tAimDim3(:)     ! xlsx 	
  character(LEN=35), allocatable :: tAimDim4(:)     ! xlsx 	
  character(LEN=35), allocatable :: tAimDim5(:)     ! xlsx 	
  character(LEN=35), allocatable :: tAimDAF1(:)     ! xlsx 	
  character(LEN=35), allocatable :: tAimDAF2(:)     ! xlsx 	
  character(LEN=90), allocatable :: tAimmsVariable(:)     ! xlsx 	
  character(LEN=120), allocatable :: tAimSetDomain(:)     ! xlsx 
  character(LEN=35), allocatable :: emmvar_start(:,:) ! starting index argument to pass. usually "1"; sometimes "0"; or "curiyr" or "ireg" for daf dims
  character(LEN=35), allocatable :: emmvar_end(:,:)   ! ending index argument to pass. usually emm dim parameter; curiyr or ireg for daf dims
  character(LEN=2),  allocatable :: emmtype(:)  ! type of data: "r4" "r8" "i4" "i2" or "logical"
  integer,allocatable :: NumEMMDim(:,:)
  integer numRegularDim,numDAFDim
  character(LEN=150),allocatable :: AimDimNames(:)
! ParamBySub WorkSheet
  character(LEN=50), allocatable :: ParamBySub_Subr(:)     ! xlsx 
  character(LEN=50), allocatable :: ParamBySub_Parm(:)     ! xlsx 
  integer SubCount
  
  integer,parameter :: max_subs=100
  character(LEN=30) SubList(max_subs)
  integer Sub_Declared(max_subs) ! 1 if declaration added; to avoid repeated declarations in sub sections
  integer num_subs
  
  integer,parameter :: max_trans=3000
  integer nuniq
  character(LEN=50) transfer(max_trans)    ! list of unique arrays in tFortranVariable
  integer           tUnique(max_trans)     ! index to each unique fortran variable in tFortranVariable
  integer           transfer_usage(max_trans)  ! indicator for how to treat a variable, 0: RHS usage only so only transfered to AIMMS, 1: LHS usage, to be passed back to NEMS, 2: LHS usage, omit calculated version declaration because aimms code already has declaration
  integer           DoNotGenerate_Code(max_trans)
  logical           write_historicalyears(max_trans) !flag to write out all historical years up to the current year
  transfer=' '
  
  
! end of local xlsx data  

isBounded=.false.
apy=(/'1','2','3'/)
numBounds=0
empty(:)=' '
subsets(:,:)=' '
superset(:)=' '
nsubsets(:)=0
allsets=' '

1  write(6,'(a\)')' Enter version, ecp or efd [ecp]: '
read(5,'(a)') ver
  if(ver.eq.' ') ver='ecp'
  if(ver.ne.'ecp'.and.ver.ne.'efd') then
     write(6,'(a)') ' version must be ecp or efd in lower case.'
     go to 1
  endif
  if(ver.eq.'ecp')then
    iver=1
  else
    iver=2
  endif
! there are no ecp rows and columns that actually use the 4-letter SupplyRegion, so initialize the
! set list with it here.
  if(ver.eq.'ecp') then
    alldims(1)='SupplyRegion'
   nlist=1
  else
    nlist=0
  endif


 DoSafety=.false. 
 !write(6,'(a\)')' Do you want to add safety variables to all constraints? (n) '
 !read(5,'(a)') ans
 if(ans.eq.'Y'.or.ans.eq.'y') then
   DoSafety=.true.
 endif
! read AIMMS row and column settings from aimVER.xlsx, also used as an input file for nems/uVER.f
     open(unit=ixlsx,file ='aim'//ver//'.xlsx',status='old') ! this is just to pass the file name via the unit number to the next subroutine
     call readRngXLSX(ixlsx,'col_row')  ! read sheet "col_row" range names and the data in them
     close(ixlsx)
     rname='C_COUNT'
     call getrngi(rname,c_count,1,1,1)
     rname='R_COUNT'
     call getrngi(rname,r_count,1,1,1)
! allocate arrays to hold aimms column and row definitions
     allocate (xcolnam(       c_count))
     allocate (xcolsetnam(    c_count,max_set(iver)))
     allocate (xcolmask(      c_count))
     allocate (xcolneedsol(   c_count))
     allocate (xc_comment(    c_count))
     allocate (xrownam(       r_count))
     allocate (xrowsetnam(    r_count,max_set(iver)))
     allocate (xrowneedsol(   r_count))
     allocate (xrowmask(      r_count))
     allocate (xrow_type(     r_count))
     allocate (xr_comment(    r_count))
!                                           range,destination,     cols,rows,groups
     rname='COLNAM_AIMMS    '; call getrngc(rname,xcolnam,            1,c_count,1)
     rname='COLNAM_MASK     '; call getrngc(rname,xcolmask,           1,c_count,1)
     rname='CNEEDSOL        '; call getrngr(rname,xcolneedsol,        1,c_count,1)
     rname='CSETNAM1        '; call getrngc(rname,xcolsetnam(1,1),    1,c_count,1)
     rname='CSETNAM2        '; call getrngc(rname,xcolsetnam(1,2),    1,c_count,1)
     rname='CSETNAM3        '; call getrngc(rname,xcolsetnam(1,3),    1,c_count,1)
     rname='CSETNAM4        '; call getrngc(rname,xcolsetnam(1,4),    1,c_count,1)
     rname='CSETNAM5        '; call getrngc(rname,xcolsetnam(1,5),    1,c_count,1)
     if ( ver.eq.'ecp') then
     rname='CSETNAM6        '; call getrngc(rname,xcolsetnam(1,6),    1,c_count,1)
     endif
     rname='C_COMMENT       '; call getrngc(rname,xc_comment,             1,c_count,1)

     rname='ROWNAM_AIMMS    '; call getrngc(rname,xrownam,            1,r_count,1)
     rname='ROWNAM_MASK     '; call getrngc(rname,xrowmask,           1,r_count,1)
     rname='RNEEDSOL        '; call getrngr(rname,xrowneedsol,        1,r_count,1)
     rname='ROW_TYPE        '; call getrngc(rname,xrow_type,          1,r_count,1)
     rname='RSETNAM1        '; call getrngc(rname,xrowsetnam(1,1),    1,r_count,1)
     rname='RSETNAM2        '; call getrngc(rname,xrowsetnam(1,2),    1,r_count,1)
     rname='RSETNAM3        '; call getrngc(rname,xrowsetnam(1,3),    1,r_count,1)
     rname='RSETNAM4        '; call getrngc(rname,xrowsetnam(1,4),    1,r_count,1)
     rname='RSETNAM5        '; call getrngc(rname,xrowsetnam(1,5),    1,r_count,1)
     if (ver.eq.'ecp')then
     rname='RSETNAM6        '; call getrngc(rname,xrowsetnam(1,6),    1,r_count,1)
     endif
     rname='R_COMMENT       '; call getrngc(rname,xr_comment,             1,r_count,1)

     do num_cols=1,c_count
         colsets(num_cols,1:max_set(iver))=' '
         colname(num_cols)  =xcolnam(num_cols)
         colmask(num_cols)  =xcolmask(num_cols)
         needsol_cols(num_cols)=xcolneedsol(num_cols)
!		 write(6,'(1x,a,1x,i5)') colname(num_cols),needsol_cols(num_cols)
         colsets(num_cols,1)=xcolsetnam(num_cols,1)
         colsets(num_cols,2)=xcolsetnam(num_cols,2)
         colsets(num_cols,3)=xcolsetnam(num_cols,3)
         colsets(num_cols,4)=xcolsetnam(num_cols,4)
         colsets(num_cols,5)=xcolsetnam(num_cols,5)
         if(ver.eq.'ecp')colsets(num_cols,6)=xcolsetnam(num_cols,6)
         numColSets(num_cols)=0
         do i=1,max_set(iver)
           if(len_trim(colsets(num_cols,i)).gt.0) then
             numColSets(num_cols)=numColSets(num_cols)+1
           endif
         enddo
         dim=' '
         ndim=NumColSets(num_cols)
         dim(1:ndim)=colsets(num_cols,1:ndim)
         call dimlist(alldims,superset,maxdims,nlist,dim,ndim,empty)
         call mreplace(xc_comment(num_cols),'"',"'")
     enddo
     num_cols=c_count
     
     do num_rows=1,r_count

        rowsets(num_rows,1:max_set(iver))=' '
        rowtype(num_rows)= xrow_type(num_rows)   
        rowname(num_rows)= xrownam(num_rows)
        rowmask(num_rows)=xrowmask(num_rows)
        needsol_rows(num_rows)=xrowneedsol(num_rows)
!	    write(6,'(1x,a,1x,i5)') rowname(num_rows),needsol_rows(num_rows)
        rowsets(num_rows,1)=xrowsetnam(num_rows,1)
        rowsets(num_rows,2)=xrowsetnam(num_rows,2)
        rowsets(num_rows,3)=xrowsetnam(num_rows,3)
        rowsets(num_rows,4)=xrowsetnam(num_rows,4)
        rowsets(num_rows,5)=xrowsetnam(num_rows,5)
        if(ver.eq.'ecp')then
        rowsets(num_rows,6)=xrowsetnam(num_rows,6)
        endif

        numRowSets(num_rows)=0
        
        do i=1,max_set(iver)
          if(len_trim(rowsets(num_rows,i)).gt.0) then
            numRowSets(num_rows)=numRowSets(num_rows)+1
          endif
        enddo
        do i=1,NumRowSets(num_rows)
          dim=' '
          ndim=NumRowSets(num_rows)
          dim(1:ndim)=rowsets(num_rows,1:ndim)
          call dimlist(alldims,superset,maxdims,nlist,dim,ndim,empty)
        enddo
        call mreplace(xr_comment(num_rows),'"',"'")

     enddo
     num_rows=r_count

     open(unit=ixlsx,file='aim'//ver//'.xlsx',status='old') ! this is just to pass the file name via the unit number to the next subroutine
     call readRngXLSX(ixlsx,ver//'SetMatch')  ! read sheet "ecpSetMatch" or "efdSetMatch" range names and the data in them
     close(ixlsx)

     rname=(ver)//'LOOKCOUNT'
     call upper(rname)
     call getrngi4(rname,SetLookCount,1,1,1)
     allocate (xSetLook(SetLookCount,5))
     rname=(ver)//'LOOK         '; 
     call upper(rname)
     call getrngc(rname,xSetLook,SetLookCount,5,1)
     call dimlist(alldims,superset,maxdims,nlist,xSetLook(1:SetLookCount,2),SetLookCount,xSetLook(1:SetLookCount,5))
     call dimlist(alldims,superset,maxdims,nlist,xSetLook(1:SetLookCount,3),SetLookCount,xSetLook(1:SetLookCount,5))
     
     
     
     
     
     
     
     
     
! read AIMMS row and column settings from aimVER.xlsx, also used as an input file for nems/uVER.f
     open(unit=ixlsx,file='aim'//ver//'.xlsx',status='old') ! this is just to pass the file name via the unit number to the next subroutine
     call readRngXLSX(ixlsx,ver//'Transfer')  ! read sheet "EFDTransfer" range names and the data in them
     close(ixlsx)
     rname='TransferCount'
     call getrngi4(rname,TransferCount,1,1,1)
    allocate(tFortranVariable(TransferCount))  
    allocate(tIncludeFile(TransferCount)    )  
    allocate(tCodeUsage(TransferCount)     ) 
    allocate(tHistoricalYears(TransferCount)     )
    allocate(tFortType(TransferCount)       ) 	
    allocate(tNoGenCode(TransferCount)       ) 
    allocate(tFortDescription(TransferCount))  	
    allocate(tFortDim1(TransferCount)       ) 	
    allocate(tFortDim2(TransferCount)       ) 	
    allocate(tFortDim3(TransferCount)       ) 	
    allocate(tFortDim4(TransferCount)       ) 	
    allocate(tFortDim5(TransferCount)       ) 	
    allocate(tDAFDim1(TransferCount)        ) 
    allocate(tDAFDim2(TransferCount)        ) 
    allocate(tAimDim1(TransferCount)        ) 
    allocate(tAimDim2(TransferCount)        ) 
    allocate(tAimDim3(TransferCount)        ) 
    allocate(tAimDim4(TransferCount)        ) 
    allocate(tAimDim5(TransferCount)        ) 
    allocate(tAimDAF1(TransferCount)        ) 
    allocate(tAimDAF2(TransferCount)        ) 
    allocate(tAimmsVariable(TransferCount)  ) 
    allocate(tAimSetDomain(TransferCount)   ) 
    allocate(emmvar_start(7,TransferCount) )
    allocate(emmvar_end(7,TransferCount) )
    allocate(emmtype(TransferCount))
    allocate(NumEMMDim(2,TransferCount))
    allocate(AimDimNames(TransferCount))

    rname='FortranVariable ';call getrngc(rname,  tFortranVariable       , 1,TransferCount,1)
    rname='INCLUDEFILE     ';call getrngc(rname,  tIncludeFile           , 1, TransferCount , 1 )
    rname='CODEUSAGE       ';call getrngc(rname,  tCodeUsage             , 1, TransferCount , 1 )
    rname='HISTORICALYEARS ';call getrngc(rname,  tHistoricalYears       , 1, TransferCount , 1 )
    rname='FORTTYPE        ';call getrngc(rname,  tFortType              , 1, TransferCount , 1 )
    rname='NOGENCODE       ';call getrngc(rname,  tNoGenCode             , 1, TransferCount , 1 )   !added by  AKN on 5/25/22
    rname='FORTDESCRIPTION ';call getrngc(rname,  tFortDescription       , 1, TransferCount , 1 )
    rname='FORTDIM1        ';call getrngc(rname,  tFortDim1              , 1, TransferCount , 1 )
    rname='FORTDIM2        ';call getrngc(rname,  tFortDim2              , 1, TransferCount , 1 )
    rname='FORTDIM3        ';call getrngc(rname,  tFortDim3              , 1, TransferCount , 1 )
    rname='FORTDIM4        ';call getrngc(rname,  tFortDim4              , 1, TransferCount , 1 )
    rname='FORTDIM5        ';call getrngc(rname,  tFortDim5              , 1, TransferCount , 1 )
    rname='DAFDIM1         ';call getrngc(rname,  tDAFDim1               , 1, TransferCount , 1 )
    rname='DAFDIM2         ';call getrngc(rname,  tDAFDim2               , 1, TransferCount , 1 )
    rname='AIMDIM1         ';call getrngc(rname,  tAimDim1               , 1, TransferCount , 1 )
    rname='AIMDIM2         ';call getrngc(rname,  tAimDim2               , 1, TransferCount , 1 )
    rname='AIMDIM3         ';call getrngc(rname,  tAimDim3               , 1, TransferCount , 1 )
    rname='AIMDIM4         ';call getrngc(rname,  tAimDim4               , 1, TransferCount , 1 )
    rname='AIMDIM5         ';call getrngc(rname,  tAimDim5               , 1, TransferCount , 1 )
    rname='AIMDAF1         ';call getrngc(rname,  tAimDAF1               , 1, TransferCount , 1 )
    rname='AIMDAF2         ';call getrngc(rname,  tAimDAF2               , 1, TransferCount , 1 )
    rname='AIMMSVARIABLE   ';call getrngc(rname,  tAimmsVariable         , 1, TransferCount , 1 )
    rname='AIMSETDOMAIN    ';call getrngc(rname,  tAimSetDomain          , 1, TransferCount , 1 )
    

    do j=1,TransferCount
         call mreplace(tFortDescription(j),'&amp;',"&")
         call mreplace(tFortDescription(j),'&gt;',">")
         call mreplace(tFortDescription(j),'&lt;',"<")
         call mreplace(tAimmsVariable(j),'.',"_")
         call mreplace(tAimmsVariable(j),'$',"_")
         if(tFortType(j).eq.'REAL ') then
           emmtype(j)='R4'
         elseif(tFortType(j).eq.'REAL*4') then
           emmtype(j)='R4'
         elseif(tFortType(j).eq.'REAL(4)') then
           emmtype(j)='R4'
         elseif(tFortType(j).eq.'REAL*8') then
           emmtype(j)='R8'
         elseif(tFortType(j).eq.'REAL(8)') then
           emmtype(j)='R8'
         elseif(tFortType(j).eq.'INTEGER') then
           emmtype(j)='I4'
         elseif(tFortType(j).eq.'INTEGER*4') then
           emmtype(j)='I4'
         elseif(tFortType(j).eq.'INTEGER(4)') then
           emmtype(j)='I4'
         elseif(tFortType(j).eq.'INTEGER*2') then
           emmtype(j)='I2'
         elseif(tFortType(j).eq.'INTEGER(2)') then
           emmtype(j)='I2'
         elseif(tFortType(j).eq.'LOGICAL') then
           emmtype(j)='I1'
         else
           emmtype(j)='R4'
         endif
         
         call mreplace(tFortDim1(j),"'","")
         call mreplace(tFortDim2(j),"'","")
         call mreplace(tFortDim3(j),"'","")
         call mreplace(tFortDim4(j),"'","")
         call mreplace(tFortDim5(j),"'","")
         
         call Mreplace(tFortranVariable(j),char(0)," ")
    enddo

! read AIMMS list of ParamBySub from aimVER.xlsx, also used as an input file for nems/uVER.f
     open(unit=ixlsx,file='aim'//ver//'.xlsx',status='old') ! this is just to pass the file name via the unit number to the next subroutine
     call readRngXLSX(ixlsx,'ParamBySub')  ! read sheet "'ParamBySub" range names and the data in them
     close(ixlsx)
     rname='SubCount'
     call getrngi4(rname,SubCount,1,1,1)
    allocate(ParamBySub_Subr(SubCount))  
    allocate(ParamBySub_Parm(SubCount))  
    rname='SubList         ';call getrngc(rname,  ParamBySub_Subr    , 1, SubCount , 1 )
    rname='ParamList       ';call getrngc(rname,  ParamBySub_Parm    , 1, SubCount , 1 )
    
    num_subs=0
    do j=1,SubCount
      call mreplace(ParamBySub_Subr(j),'$',"_")

! get list of subroutines for organizing parameters in the aimms project     
      ifound=0
      do i=1,num_subs
        if(ParamBySub_Subr(j).eq.SubList(i)) then
          ifound=1
          exit
       endif
      enddo
      if(ifound.eq.0) then
        num_subs=num_subs+1
        SubList(Num_subs)= ParamBySub_Subr(j)      
      endif
    enddo
    


! create list of unique tAIMMSVariables to eliminate possible duplicates (from an earlier veresion of the worksheet that listed variables by subroutine)
    nuniq=0
    tunique=0
    do j=1,TransferCount
      ifound=0
      do i=1,nuniq
        if (tAIMMSVariable(j).eq. transfer(i)) then
          ifound=i
          exit
        endif
      enddo
      if(ifound.eq.0) then
        nuniq=nuniq+1
        transfer(nuniq)=tAIMMSVariable(j)
        transfer_usage(nuniq)=0
        DoNotGenerate_Code(nuniq)=0
        call upper(tCodeUsage(j))
        if(tCodeUsage(j).eq.'RHS') then  ! RHS is abbreviation for RHS of fortran assignment statement. If so, it is an INPUT to LP coefficient deriviation.
          transfer_usage(nuniq)=0
        elseif(tCodeUsage(j)(1:3).eq.'LHS' .and. len_trim(tCodeUsage(j)).eq.3) then  ! LHS means the variable is used on the LHS of an assignement statement based on the LP solution usually.
          transfer_usage(nuniq)=1
        elseif(tCodeUsage(j)(1:3).eq.'LHS' .and. len_trim(tCodeUsage(j)).gt.3) then  ! A suffix to LHS is used to designate a variable that has been declared/coded in AIMMS and so we omit the declaration when writing the AIMMS code
            if(trim(tCodeUsage(j)) .eq. 'LHS_CODED') then            
               transfer_usage(nuniq)=2
             elseif (trim(tCodeUsage(j)) .eq. 'LHS_DONE') then
               transfer_usage(nuniq)=3
             else
               transfer_usage(nuniq)=1
             endif
         endif
         tUnique(nuniq)=j
      else ! variable may appear twice in list. if first had "RHS", second has "LHS". if LHS in either, treat as LHS
        if(tCodeUsage(j)(1:3).eq.'LHS' .and. len_trim(tCodeUsage(j)).eq.3) then 
          transfer_usage(ifound)=1
        elseif(tCodeUsage(j)(1:3).eq.'LHS' .and. len_trim(tCodeUsage(j)).gt.3) then
           if(trim(tCodeUsage(j)) .eq. 'LHS_CODED') then            
               transfer_usage(nuniq)=2
             elseif (trim(tCodeUsage(j)) .eq. 'LHS_DONE') then
               transfer_usage(nuniq)=3
             else
               transfer_usage(nuniq)=1
             endif
         endif
       endif
       if ((trim(tNoGenCode(j)).eq.'Yes' .or. trim(tNoGenCode(j)).eq.'YES' .or. trim(tNoGenCode(j)).eq.'yes') .and. len_trim(tNoGenCode(j)).eq.3 ) then
          DoNotGenerate_Code(nuniq) = 1
       else
          DoNotGenerate_Code(nuniq) = 0
       endif
      if ((trim(tHistoricalYears(j)).eq.'Yes' .or. trim(tHistoricalYears(j)).eq.'YES' .or. trim(tHistoricalYears(j)).eq.'yes') .and. len_trim(tHistoricalYears(j)).eq.3 ) then
         write_historicalyears(j) = .TRUE.
      else
         write_historicalyears(j) = .FALSE.
      endif
    enddo

      USE_AIM_SLNADJ = .TRUE.
      do ArrayIndex=1,TransferCount
            if(transfer_usage(Arrayindex).eq.1 .or. transfer_usage(Arrayindex).eq.2 ) then  
                USE_AIM_SLNADJ = .FALSE.
            endif
      enddo
    
    do j=1,TransferCount
      emmvar_start(1:7,j)='1'
      emmvar_end(1:7,j)  ='1'  
      numRegularDim=0
      numDafDim=0    
      AimDimNames(j)=' '
      if(tAimSetDomain(j).eq.'(SCALAR)') then
        AimDimNames(j)='"SCALAR","","","","","","",'
        numRegularDIm=1
        numDafDim=0
        numEmmDim(1,j)=numRegularDim
        numEMMDim(2,j)=numDAFDim

      else
        if(tAimDim1(j).ne.' ') then
          dim(1)=tAimDim1(j)
          call dimlist(alldims,superset,maxdims,nlist,dim,1,empty)
          numRegularDim=1
          call vrange(tFortranVariable(j),tFortDim1(j),tAimDim1(j),ver,emmvar_start(1,j),emmvar_end(1,j),0,NumRegularDim+numDAFDIM)
          Laimdim=1
          AimDimNames(j)(Laimdim:)='"'//trim(tAimDim1(j))//'",'
        endif
         if(tAimDim2(j).ne.' ') then
          dim(1)=tAimDim2(j)
          call dimlist(alldims,superset,maxdims,nlist,dim,1,empty)
          numRegularDim=2
          call vrange(tFortranVariable(j),tFortDim2(j),tAimDim2(j),ver,emmvar_start(1,j),emmvar_end(1,j),0,NumRegularDim+numDAFDIM)
          Laimdim=max(len_trim(AimDimNames(j))+2,15)
          AimDimNames(j)(Laimdim:)='"'//trim(tAimDim2(j))//'",'
        endif
        if(tAimDim3(j).ne.' ') then
          dim(1)=tAimDim3(j)
          call dimlist(alldims,superset,maxdims,nlist,dim,1,empty)
          numRegularDim=3
          call vrange(tFortranVariable(j),tFortDim3(j),tAimDim3(j),ver,emmvar_start(1,j),emmvar_end(1,j),0,NumRegularDim+numDAFDIM)
          Laimdim=max(len_trim(AimDimNames(j))+2,30)
          AimDimNames(j)(Laimdim:)='"'//trim(tAimDim3(j))//'",'
        endif
        if(tAimDim4(j).ne.' ') then
          dim(1)=tAimDim4(j)
          call dimlist(alldims,superset,maxdims,nlist,dim,1,empty)
          numRegularDim=4
          call vrange(tFortranVariable(j),tFortDim4(j),tAimDim4(j),ver,emmvar_start(1,j),emmvar_end(1,j),0,NumRegularDim+numDAFDIM)
          Laimdim=max(len_trim(AimDimNames(j))+2,45)
          AimDimNames(j)(Laimdim:)='"'//trim(tAimDim4(j))//'",'
        endif
        if(tAimDim5(j).ne.' ') then
          dim(1)=tAimDim5(j)
          call dimlist(alldims,superset,maxdims,nlist,dim,1,empty)
          numRegularDim=5
          call vrange(tFortranVariable(j),tFortDim5(j),tAimDim5(j),ver,emmvar_start(1,j),emmvar_end(1,j),0,NumRegularDim+numDAFDIM)
          Laimdim=max(len_trim(AimDimNames(j))+2,60)
          AimDimNames(j)(Laimdim:)='"'//trim(tAimDim5(j))//'",'
        endif
        if(tAimDAF1(j).ne.' ') then
          dim(1)=tAimDAF1(j)
          call dimlist(alldims,superset,maxdims,nlist,dim,1,empty)
          numDafDim=numDafDim+1
          call vrange(tFortranVariable(j),tDAFDim1(j),tAimDAF1(j),ver,emmvar_start(1,j),emmvar_end(1,j),1,NumRegularDim+numDAFDIM)
          Laimdim=max(len_trim(AimDimNames(j))+2,(numRegularDim-1)*15)
          AimDimNames(j)(Laimdim:)='"'//trim(tAimDAF1(j))//'",'
        endif
        if(tAimDAF2(j).ne.' ') then
          dim(1)=tAimDAF2(j)
          call dimlist(alldims,superset,maxdims,nlist,dim,1,empty)
          numDafDim=numDafDim+1
          call vrange(tFortranVariable(j),tDAFDim2(j),tAimDAF2(j),ver,emmvar_start(1,j),emmvar_end(1,j),1,NumRegularDim+numDAFDIM)
          Laimdim=max(len_trim(AimDimNames(j))+2,(numRegularDim+numDAFDim-1)*15)
          AimDimNames(j)(Laimdim:)='"'//trim(tAimDAF2(j))//'",'
        endif
        numEmmDim(1,j)=numRegularDim
        numEMMDim(2,j)=numDAFDim
        do k=numRegularDim+numDafDim+1,7
          laimdim=max(len_trim(AimDimNames(j))+2,(k-1)*15)
          AimDimNames(j)(laimdim:)='"",'
        enddo
      endif
    enddo
    open(29,file='transfer_code_'//ver//'.f',status='unknown')
 ! write out calls to subroutine to output emm variables to aimms:
 ! example
 ! call AIMMS_Transfer_Out(Curtailment_Factor_WN,  Curtailment_Factor_WN,  Curtailment_Factor_WN, Curtailment_Factor_WN, & ! Fortran variable, listed 4 times for each of 4 possible data types
 ! 3,'Curtailment_Factor_WN', &                                ! number of sets, or 1, which iver is higher, name of aimms variable,
 !  1,1,1,1,1,1,1,       &                                     ! starting index for each of up to 5 regular dimensions and up to 2 DAF implicit dimensions
 !  MAXECPB,ECP_D_VLS,MNUMNR,1,1,1,1,      &                     ! ending index for each of up to 5 dimensions and up to 2 DAF implicit dimensions
 ! 'DSMLoadGroup','VLoadSegment','SupplyRegion',' ',' ',' ',' ',  ! names of aimms sets for each dimension; 
 !  'R8',iOutTxt,'efd')  

    rewind 29
    write(29,'(20a)') '!', ('=============',i=1,10)
    write(29,'(a)')   '!  Transfer the variables not stored in DAF files'
    write(29,'(20a)') '!', ('=============',i=1,10)
    do i=1,nuniq
      j=tunique(i)
       if(tDAFDim1(j).ne.'mnumnr'.and.tDAFDim2(j).ne.'mnumyr' .and. (tIncludeFile(j).ne.'wwind') .and.DoNotGenerate_Code(i).eq. 0) then
        t(1)=11
        do k=2,7
          t(k)=t(k-1)+ max(15,len_trim(emmvar_end(k-1,j))+2)
        enddo
        post=' '
        
        if(transfer_usage(i).gt.0.and.transfer_usage(i).le.3) then
          post=posttag(Transfer_Usage(i))
            write(29,'(5x,11a,i1,3a)')       '  call AIMMS_TransArray_out_'//ver//'(',(trim(tFortranVariable(j)),',',k=1,5),numEmmDim(1,j)+numEMMDim(2,j), &
                ',"',trim(tAIMMSVariable(j)),'", &'  ! AKN this is to creates AIMMS output variable names containing post LP solve values .
            write(29,'(11x,t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_start(k,j)),',',k=1,7),'  &'
            write(29,'(11x,t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_end  (k,j)),',',k=1,7),'  &'
            write(29,'(10x,2a)')     trim(AimDimNames(j)),'  &'
            !if (ver.eq.'ecp')  then
                if (write_historicalyears(j) .eq. .TRUE.) then
                     write(29,'(10x,30a)')     '"',emmtype(j),'",iyr,.TRUE.)'
                else
                     write(29,'(10x,30a)')     '"',emmtype(j),'",iyr,.FALSE.)'
                endif
             !else
                !write(29,'(10x,30a)')     '"',emmtype(j),'",iyr)'
             !endif
        elseif(transfer_usage(i).gt.3.or.transfer_usage(i).lt.0) then
          write(6,*)' error in transfer_usage, i=',i,trim(tAIMMSVariable(j))
        endif
        write(29,'(5x,11a,i1,3a)')       '  call AIMMS_TransArray_out_'//ver//'(',(trim(tFortranVariable(j)),',',k=1,5),numEmmDim(1,j)+numEMMDim(2,j), &
            ',"',trim(tAIMMSVariable(j))//trim(post),'", &'  ! AKN this appends "pass_" to the AIMMS output variable names to pass values obtains only durig pre-LP solve stage.
        write(29,'(11x,t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_start(k,j)),',',k=1,7),'  &'
        write(29,'(11x,t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_end  (k,j)),',',k=1,7),'  &'
        write(29,'(10x,2a)')     trim(AimDimNames(j)),'  &'
        !if (ver.eq.'ecp')  then
            if (write_historicalyears(j) .eq. .TRUE.) then
                    write(29,'(10x,30a)')     '"',emmtype(j),'",iyr,.TRUE.)'
            else
                    write(29,'(10x,30a)')     '"',emmtype(j),'",iyr,.FALSE.)'
            endif
        !else
            !write(29,'(10x,30a)')     '"',emmtype(j),'",iyr)'
        !endif
      endif
    enddo
    write(29,'(20a)') '!', ('=============',i=1,10)
    write(29,'(20a)') '!  Transfer the variables stored in DAF files: read the DAF files in region, year, or plant loops'
    write(29,'(20a)') '!',('=============',i=1,10)
    if (ver.eq.'ecp') then
         write(29,'(a)') 'do iyr =curiyr,curiyr + ECP_D_XPH - 1'     !to accomodate look-ahead years   by AKN
    endif
    write(29,'(a)') '   call geteij(iyr)'


! place calls to dump only dispett3 variables here
    do i=1,nuniq
      j=tunique(i) 
      if(tIncludeFile(j).eq.'dispett3'.and.DoNotGenerate_Code(i).eq. 0) then
        t(1)=13
        do k=2,7
          t(k)=t(k-1)+ max(15,len_trim(emmvar_end(k-1,j))+2)
        enddo
        post=' '
        if(transfer_usage(i).gt.0.and.transfer_usage(i).le.3) then
          post=posttag(Transfer_Usage(i))
          write(29,'(7x,11a,i1,3a)')       '  call AIMMS_TransArray_out_'//ver//'(',(trim(tFortranVariable(j)),',',k=1,5),numEmmDim(1,j)+numEMMDim(2,j), & 
            ',"',trim(tAIMMSVariable(j)),'", &'  ! ! AKN this is to creates AIMMS output variable names containing post LP solve values .
          write(29,'(13x,t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_start(k,j)),',',k=1,7),'  &'
          write(29,'(13x,t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_end  (k,j)),',',k=1,7),'  &'
          write(29,'(12x,2a)')     trim(AimDimNames(j)),'  &'
              !if (ver.eq.'ecp')  then
                    if (write_historicalyears(j) .eq. .TRUE.) then
                         write(29,'(10x,30a)')     '"',emmtype(j),'",iyr,.TRUE.)'
                    else
                         write(29,'(10x,30a)')     '"',emmtype(j),'",iyr,.FALSE.)'
                    endif
            !else
                !write(29,'(10x,30a)')     '"',emmtype(j),'",iyr)'
            !endif
        elseif(transfer_usage(i).gt.3.or.transfer_usage(i).lt.0) then
          write(6,*)' error in transfer_usage, i=',i,trim(tAIMMSVariable(j))
        endif
        write(29,'(7x,11a,i1,3a)')       '  call AIMMS_TransArray_out_'//ver//'(',(trim(tFortranVariable(j)),',',k=1,5),numEmmDim(1,j)+numEMMDim(2,j), & 
            ',"',trim(tAIMMSVariable(j))//trim(post),'", &'  ! AKN this appends "pass_" to the AIMMS output variable names to pass values obtains only durig pre-LP solve stage.
        write(29,'(13x,t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_start(k,j)),',',k=1,7),'  &'
        write(29,'(13x,t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_end  (k,j)),',',k=1,7),'  &'
        write(29,'(12x,2a)')     trim(AimDimNames(j)),'  &'
            !if (ver.eq.'ecp')  then
                    if (write_historicalyears(j) .eq. .TRUE.) then
                         write(29,'(10x,30a)')     '"',emmtype(j),'",iyr,.TRUE.)'
                    else
                         write(29,'(10x,30a)')     '"',emmtype(j),'",iyr,.FALSE.)'
                    endif
            !else
                !write(29,'(10x,30a)')     '"',emmtype(j),'",iyr)'
            !endif
      endif
    enddo
    
    if (ver.eq.'ecp') then
        write(29,'(a)') '   enddo'    !to accomodate look-ahead years   by AKN
        write(29,'(a)') '   iyr = curiyr'
        write(29,'(a)') '     '
    endif
    write(29,'(a)') '   do irg=1,UNRGNS'
    write(29,'(a)') '     call getin(1,irg)   ! year argument is always 1 for getin'
    if(ver.eq.'ecp')then
      write(29,'(a)') '     call getout(iyr-1,irg)'
      write(29,'(a)') '     call getbld(1,irg)'
      write(29,'(a)') '     call getbout(iyr,irg)'
    else
     write(29,'(a)') '     call getout(iyr,irg)'  !for efd DAF
    endif
    write(29,'(a)') '     '
! generate calls for all other daf-stored variables, invoked once for each region and for the current year
    do i=1,nuniq
      j=tunique(i) 
      if((tDAFDim1(j).eq.'mnumnr'.or.tDAFDim2(j).eq.'mnumyr') .and. tIncludeFile(j).ne.'dispett3'.and.DoNotGenerate_Code(i).eq. 0) then
        t(1)=13
        do k=2,7
          t(k)=t(k-1)+ max(15,len_trim(emmvar_end(k,j))+2)
        enddo
        post=' '
        if(transfer_usage(i).gt.0.and.transfer_usage(i).le.3) then
          post=posttag(Transfer_Usage(i))
        elseif(transfer_usage(i).gt.3.or.transfer_usage(i).lt.0) then
          write(6,*)' error in transfer_usage, i=',i,trim(tAIMMSVariable(j))
        endif
        write(29,'(7x,11a,i1,3a)')       '  call AIMMS_TransArray_out_'//ver//'(',(trim(tFortranVariable(j)),',',k=1,5),numEmmDim(1,j)+numEMMDim(2,j), &
            ',"',trim(tAIMMSVariable(j))//trim(post),'", &'
        write(29,'(13x,t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_start(k,j)),',',k=1,7),'  &'
        write(29,'(13x,t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_end  (k,j)),',',k=1,7),'  &'
        write(29,'(12x,2a)')     trim(AimDimNames(j)),'  &'
            !if (ver.eq.'ecp')  then
                    if (write_historicalyears(j) .eq. .TRUE.) then
                         write(29,'(10x,30a)')     '"',emmtype(j),'",iyr,.TRUE.)'
                    else
                         write(29,'(10x,30a)')     '"',emmtype(j),'",iyr,.FALSE.)'
                    endif
            !else
                !write(29,'(10x,30a)')     '"',emmtype(j),'",iyr)'
            !endif
      endif
    enddo
    write(29,'(a)') '   enddo'
    close(29)

    open(29,file='passback_code_'//ver//'.f',status='unknown')
    rewind 29
   
    write(29,'(20a)') '!', ('=============',i=1,10)
    write(29,'(20a)') '! Case Select Statements to read pass-back variables'
    write(29,'(20a)') '!', ('=============',i=1,10)

    write(29,'(a)') '   Select case(identifier)'
    do i=1,nuniq
      j=tunique(i) 
      if(transfer_usage(i).gt.0 .and.DoNotGenerate_Code(i).eq. 0) then
        if(tDAFDim1(j).ne.'mnumnr'.and.tAimDAF2(j).ne.'mnumyr' .and. (tIncludeFile(j).ne.'wwind')) then
          t(1)=21
          do k=2,7
            t(k)=t(k-1)+ max(15,len_trim(emmvar_end(k-1,j))+2)
          enddo
          !write(29,'(30a)') "     CASE('",trim(tFortranVariable(j)),"')"  
          if(transfer_usage(i).eq.3) then
             write(29,'(30a)') "     CASE('",trim(tAIMMSVariable(j))//'_calc',"')" 
              !**************************************************
              if (trim(ADJUSTL(emmvar_end  (1,j))).eq. 'SCALAR') then
                  setZero_passback = trim(tFortranVariable(j))//'=0.0'
              else
                setZero_passback = trim(tFortranVariable(j))//'('
                do k=1,7
                    if (trim(ADJUSTL(emmvar_end  (k,j))) .ne. '1') then
                        if (trim(ADJUSTL(emmvar_end  (k,j))) .ne. 'MNUMYR') then
                            if (k .eq. 1) then
                                setZero_passback = trim(setZero_passback)//':'
                            else
                                setZero_passback = trim(setZero_passback)//',:'
                            endif
                        else
                            if (k .eq. 1) then
                                setZero_passback = trim(setZero_passback)//'iyr'
                            else
                                setZero_passback = trim(setZero_passback)//',iyr'
                            endif
                        endif
                    else
                        exit
                    endif
                enddo
                setZero_passback = trim(setZero_passback)//')=0.0'
              endif
              write(29,'(20x,32a)') setZero_passback
              write(29,'(20x,32a)') 'if (emptyTable) goto 10'
              !*************************************************
          else
             write(29,'(30a)') "     CASE('",trim(tAIMMSVariable(j)),"')" 
          endif
          write(29,'(20x,32a)')  'call AIMMS_Transfer_in_'//ver//'(',(trim(tFortranVariable(j)),',',k=1,5),' &'  
          write(29,'(t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_start(k,j)),',',k=1,7),'  &'
          write(29,'(t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_end  (k,j)),',',k=1,7),'  &'
          write(29,'(20x,30a)')     '"',emmtype(j),'",LcolumnStart,LcolumnEnd,FieldName,nfields,iunit)'
         endif
       endif
    enddo
    write(29,'(20a)') '!', ('=============',i=1,10)
    write(29,'(20a)') '!  Read pass-back variables to be stored in DAF files: read the DAF record for the region/year; read the data; store the DAF record'
    write(29,'(20a)') '!',('=============',i=1,10)
! generate calls for all other daf-stored variables, invoked once for each region and for the current year
    do i=1,nuniq
      j=tunique(i) 
      if(transfer_usage(i).gt.0 .and. (tDAFDim1(j).eq.'mnumnr'.or.tDAFDim2(j).eq.'mnumyr') .and. tIncludeFile(j).ne.'dispett3'.and.DoNotGenerate_Code(i).eq. 0) then

! remove the dummy dimensions used for looping over DAF region or yearly records to create the AIMMS variables. Not used when reading in to fortran variables.        
        do k=1,7
          if(emmvar_start(k,j).eq.'irg') then
             emmvar_start(k,j)='1'
          endif
          if(emmvar_end(k,j).eq.'irg') then
            emmvar_end(k,j)='1'          
          endif
          if(emmvar_start(k,j).eq.'iyr') then
             emmvar_start(k,j)='1'
          endif
          if(emmvar_end(k,j).eq.'iyr') then
            emmvar_end(k,j)='1'          
          endif
        
        enddo
        t(1)=23
        do k=2,7
          t(k)=t(k-1)+ max(15,len_trim(emmvar_end(k,j))+2)
        enddo
        !write(29,'(30a)') "     CASE('",trim(tFortranVariable(j)),"')" ! assume supply region number IRG is read separately and that each array is written for a single region at a time  
        if(transfer_usage(i).eq.3) then
        
          write(29,'(30a)') "     CASE('",trim(tAIMMSVariable(j))//'_calc',"')"
          !**************************************************
          if (trim(ADJUSTL(emmvar_end  (1,j))).eq. 'SCALAR') then
              setZero_passback = trim(tFortranVariable(j))//'=0.0'
          else
            setZero_passback = trim(tFortranVariable(j))//'('
            do k=1,7
                if (trim(ADJUSTL(emmvar_end  (k,j))) .ne. '1') then
                    if (trim(ADJUSTL(emmvar_end  (k,j))) .ne. 'MNUMYR') then
                        if (k .eq. 1) then
                            setZero_passback = trim(setZero_passback)//':'
                        else
                            setZero_passback = trim(setZero_passback)//',:'
                        endif
                    else
                        if (k .eq. 1) then
                            setZero_passback = trim(setZero_passback)//'iyr'
                        else
                            setZero_passback = trim(setZero_passback)//',iyr'
                        endif
                    endif
                else
                    exit
                endif
            enddo
            setZero_passback = trim(setZero_passback)//')=0.0'
          endif
          write(29,'(20x,32a)') setZero_passback
          write(29,'(20x,32a)') 'if (emptyTable) goto 10'
          !*************************************************
        else
          write(29,'(30a)') "     CASE('",trim(tAIMMSVariable(j)),"')"
        endif
        write(29,'(20x,32a)')  'call AIMMS_Transfer_in_'//ver//'(',(trim(tFortranVariable(j)),',',k=1,5),' &'
        write(29,'(t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_start(k,j)),',',k=1,7),'  &'
        write(29,'(t<t(1)>,2a,t<t(2)>,2a,t<t(3)>,2a,t<t(4)>,2a,t<t(5)>,2a,t<t(6)>,2a,t<t(7)>,3a)')     (trim(emmvar_end  (k,j)),',',k=1,7),'  &'
        write(29,'(20x,30a)')     '"',emmtype(j),'",LcolumnStart,LcolumnEnd,FieldName,nfields,iunit)'

      endif
    enddo
    write(29,'(a)') '   END SELECT'
    close (29)
    
! Unit 8 is the input file: a list of arrays and their dimensions/sets

! Current: 
! this next file was generated from lines in the debug file VERcoeff_yyyy[_ii].txt (with "missing")
! using the commands in get_VER_array_list.sh
!
  open(8,file=ver//'Arrays_all.txt',status='unknown')
  rewind 8
! pre-read the array file to determine the one with the longest length.  This length will be used for formatting.  
9 read(8,*,end=299) array 
  if(array(1:1).eq.'#') go to 9
    la=len_trim(array)
    if (la.gt. maxArrayLength) then
       maxArrayLength=la
    endif
    goto 9
299 continue

!write(6,'(a,i4)') 'MaxArrayLength in '//ver//'Arrays_all.txt is:',maxArrayLength

open(19,file='parameter_'//ver//'.csv',status='unknown')
rewind 19

rewind 8
narray=0
10 continue
  read(8,*,end=99) array
  if(array(1:1).eq.'#') go to 10
  backspace(8)
  read(8,*) array,ipass
  
  if(narray.le.maxarray) then
    narray=narray+1
    allArrays(narray)=trim(array)
    passed(narray)=ipass
    allArrays_coeff(narray)=trim(array)    
    allArrays_calc(narray)=trim(array)    
    allArrays_pass(narray)=trim(array) 
    allArrays_diff(narray)=trim(array) 
    allArrays_compare(narray)=trim(array)
    allArrays_Percent(narray)=trim(array)
    allArrays_cdiffPct(narray)=trim(array)
    call mreplace(allArrays_coeff(narray),'cpass_','coeff_')   
    call mreplace(allArrays_calc(narray), 'cpass_','ccalc_')   
    call mreplace(allArrays_diff(narray), 'cpass_','cdiff_')   
    call mreplace(allArrays_compare(narray),'cpass_','compare_')   
    call mreplace(allArrays_Percent(narray), 'cpass_','Percent_')   
    call mreplace(allArrays_cdiffPct(narray), 'cpass_','cdiffPct_')   
    call mreplace(allArrays(narray),      'cpass_','coeff_')   
  else
    write(6,*)' increase maxarray'
    stop
  endif
  is=index(array,'cRHS_')
  if(is.gt.0) then
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
 elseif(index(array,'_EFDCOSTS').gt.0) then
    is=index(array,'_')+1
    ie=index(array(is:),'_EFDCOSTS')+is-2
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
! find the number of unique sets in the rows and columns.
  do i=1,max_set(iver)
    if(len_trim(rowsets(row_id,i)).gt.0) then
      ndim=ndim+1
      dim(ndim)=rowsets(row_id,i)
    else
      exit
    endif
  enddo
  nrowsets=ndim
! add in the number of unique column sets  
  do i=1,max_set(iver)
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
! recreate the list of parameter dimensions
  dim(:)=' '

  nextDim = 1
!  nextDim = ndim - NRowSets + 1  ! uncomment this, and "nextDim=1" below, to have row dimensions on outside of parameter domain list. an experiement showed this increased time slightly, though.

! add row sets to the parameter domain, placing them on the higher order positions so unique column
! sets will show up in the first positions
  do i=1,max_set(iver)
    if(len_trim(rowsets(row_id,i)).gt.0) then
      dim(nextdim)=rowsets(row_id,i)
      nextDim=nextDim+1
    else
      exit
    endif
  enddo
! to add unique column sets starting in position 1, uncomment following
!  nextDim=1
  do i=1,max_set(iver)
    lt=len_trim(colsets(col_id,i))
    if(lt.gt.0) then
      imatch=0
      do j=1,ndim
        if(colsets(col_id,i).eq.dim(j))then
          imatch=1
        endif
      enddo
      if(imatch.eq.0) then
        dim(nextDim)=colsets(col_id,i)
        nextDim=nextDim+1
      endif
    else
      exit
    endif
  enddo
 
!  to reorder list of dims in the array so PlanYear is last, uncomment this next section.  Should be faster if row dimensions are first
!   norder=ndim
!   do i=1,ndim
!     if (dim(i).eq.'PlanYear') then
!       dim_order(norder)=dim(i)
!       dim_card(norder)=3
!       dim(i)=' '
!       exit
!     endif
!   enddo

!   if(dim_order(norder).eq.'PlanYear') then
!     j=1
!     do i=1,ndim
!       if(len_trim(dim(i)).gt.0) then
!         dim_order(j)=dim(i)
!          j=j+1
!       endif
!     enddo
!     dim=dim_order
!   endif

  if(ndim.gt.0) then
    write(19,'(30a)') trim(array),',',(trim(dim(j)),',',j=1,ndim-1),trim(dim(ndim))
  else
    write(19,'(a)') trim(array)
  endif
  allSets(1:ndim,narray)=dim(1:ndim)
  allArraysDim(narray)=ndim
  call dimlist(alldims,superset,maxdims,nlist,dim,ndim,empty)



  
!===================================================================================================================================

  goto 10
 99 continue
  
 close(19)

! compile the list of subsets for each set. 

 do i=1,nlist 
   if(superset(i).ne.' ') then  ! this set, ( alldims(i) ), is a subset of superset(i)
      ifound=0
      do j=1,nlist
        if(i.ne.j   .and. alldims(j).eq.superset(i)) then
          ifound=j
          exit
        endif
      enddo
      if(ifound.ne.0) then
        nsubsets(ifound)=nsubsets(ifound)+1
        subsets(nsubsets(ifound),ifound)=alldims(i)
      endif
     continue
   endif
enddo
 
 
!
!==============================================================================================================================
! Write AIMMS code to hold the arrays, variables, constraints, objective function, safety values, bounds, etc.
!==============================================================================================================================
 inquire(file=ver//'.ams',exist=lexist) 
 ans='n'
 if(lexist) then
   write(6,'(a\)') 'A file '//ver//'.ams was found.  Do you want to copy its front section into auto_'//ver//'.ams (y/n)? [y] '
   read(5,'(a)') ans
   if(ans.eq.' ') then
     ans='y'
   endif
   if(ans.eq.'y') then
     open(8,file=ver//'.ams',status='old')
   endif  
 endif
 
 open(9,file='auto_'//ver//'.ams',status='unknown')
 rewind 9

  if(ans.eq.'y') then
 5   continue
     read(8,'(a)',end=8) line
     if(index(line,'WriteToNEMS();').gt.0 .AND. USE_AIM_SLNADJ) then
         call mreplace(line,' WriteToNEMS();','!WriteToNEMS();')
       elseif (index(line,'!WriteToNEMS();').gt.0 .AND. .NOT. USE_AIM_SLNADJ) THEN
         call mreplace(line,'!WriteToNEMS();',' WriteToNEMS();')
     endif

     if(index(line,'Section Sections_below_this').eq.0) then
       
       if (index(line,'if (PlantType >= ''B1X'') and (PlantType <= ''ISX'') then  !coals').gt.0) then
          i= len(line)
       endif
11       lineleftadj = adjustl(line)
      if (index(line,lineleftadj(1:1)).gt.50) then 
          do i = 1, (len(line)-50)
            lineleftadj (i:i) = line (i+50:i+50)
          enddo
          !lineleftadj = ' '
          !lineleftadj(1:len(line)-50) = line(51:len(line))
          do i=(len(line)-50+1),len(line)
            lineleftadj(i:i) = ' '
          enddo
          line = lineleftadj   
          goto 11
       endif
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
      if(superset(i).ne.' ') then
         write(9,'(3a)' )'       SubsetOf: ',trim(superset(i)),'_;'        
      endif
     
      write(9,'(2a,$)' )   '         Index: ',trim(alldims(i)) ! start index line but write without CR to possibly add aliases
      do k=1,nlist  ! look for any alias names and add them to the Index line
        if (k.ne.i) then
          j=index(alldims(k),'_ALT')
          if(j.gt.0) then
            if(alldims(k)(1:j-1).eq.trim(alldims(i)) .and. j.ge. len_trim(alldims(i))) then
              write(9,'(2a,$)')   ', ',   trim(alldims(k))
            endif
          endif
        endif
      enddo
      write(9,'(a)') ';'  ! finish Index line
      write(9,'(a)') '         OrderBy: user;'
      write(9,'(3a)' )   '     }'  
    endif
  enddo
  write(9,'(5a)')      '   }'


  write(9,'(5a)')         '   DeclarationSection TransferVariable_Input {'
  do i=1,nuniq
    J=tUnique(i)
    if(transfer_usage(i).eq.0) then
      write(9,'(30a)' )    '     Parameter ',trim(tAIMMSVariable(J)),' {'
      if(tAIMSetDomain(J).ne.'(SCALAR)') then
        write(9,'(30a)' )    '         IndexDomain: ',trim(tAIMSetDomain(J)),' ;'
      endif
      if( len_trim(    tFortDescription(J)  ).gt.0 ) then
        write(9,'(30a)' )    '         Text: "',trim(tFortDescription(J)),'" ;'
      else
        write(6,'(a)') 'no description for '//tFortranVariable(j)
      endif
      write(9,'(30a)' )    '     }' 
    endif
  enddo
  write(9,'(5a)')         '   }'


  write(9,'(5a)')         '   DeclarationSection TransferVariable_Output {'
  do i=1,nuniq
    J=tUnique(i)
    if(transfer_usage(i).gt.0) then 
      write(9,'(30a)' )    '     Parameter ',trim(tAIMMSVariable(J)),' {'
      if(tAIMSetDomain(J).ne.'(SCALAR)') then
        write(9,'(30a)' )    '         IndexDomain: ',trim(tAIMSetDomain(J)),' ;'
      endif
      if( len_trim(    tFortDescription(J)  ).gt.0 ) then
        write(9,'(30a)' )    '         Text: "',trim(tFortDescription(J)),'" ;'
      else
        write(6,'(a)') 'no description for '//tFortranVariable(j)
      endif
      !if(transfer_usage(i).eq.1) then
        !write(9,'(30a)')     '         Definition:  ',trim(tFortranVariable(j))//'_pass',trim(tAIMSetDomain(j)),' ;'
      !else
      !if(transfer_usage(i).ne.1) then  !modfied by AKN to prevent variables with _pass from being overwritten during the post LP solve phase
        !write(9,'(30a)')     '         Definition:  ',trim(tFortranVariable(j))//'_calc',trim(tAIMSetDomain(j)),' ;'
      !endif
      write(9,'(30a)' )    '     }' 

      write(9,'(30a)' )    '     Parameter ',trim(tAIMMSVariable(J))//'_pass {'
      if(tAIMSetDomain(J).ne.'(SCALAR)') then
        write(9,'(30a)' )    '         IndexDomain: ',trim(tAIMSetDomain(J)),' ;'
      endif
      if( len_trim(    tFortDescription(J)  ).gt.0 ) then
        write(9,'(30a)' )    '         Text: "',trim(tFortDescription(J)),'" ;'
      else
        write(6,'(a)') 'no description for '//tFortranVariable(j)
      endif
      write(9,'(30a)' )    '     }' 
    endif        
    if(transfer_usage(i).eq.1) then  ! generate the "calc_" version of the parameter, unless marked as already declared above the auto-generated code line
      write(9,'(30a)' )    '     Parameter ',trim(tAIMMSVariable(J))//'_calc {'
      if(tAIMSetDomain(J).ne.'(SCALAR)') then
        write(9,'(30a)' )    '         IndexDomain: ',trim(tAIMSetDomain(J)),' ;'
      endif
      if( len_trim(    tFortDescription(J)  ).gt.0 ) then
        write(9,'(30a)' )    '         Text: "',trim(tFortDescription(J)),'" ;'
      else
        write(6,'(a)') 'no description for '//tFortranVariable(j)
      endif
      write(9,'(30a)' )    '     }' 
    endif
  enddo
  write(9,'(5a)')         '   }'

  write(9,'(5a)')         '   DeclarationSection TransferVariable_Output_Check {'
  write(9,'(5a)')         '     Parameter Delta_diff;'  
  do i=1,nuniq
    J=tUnique(i)
    if(transfer_usage(i).gt.0) then 
      write(9,'(30a)' )    '     Parameter ',trim(tAIMMSVariable(J))//'_diff {'
      if(tAIMSetDomain(J).ne.'(SCALAR)') then
        write(9,'(30a)' )    '         IndexDomain: ',trim(tAIMSetDomain(J)),' ;'
      endif
      if( len_trim(    tFortDescription(J)  ).gt.0 ) then
        write(9,'(30a)' )    '         Text: "',trim(tFortDescription(J)),'" ;'
      else
        write(6,'(a)') 'no description for '//tFortranVariable(j)
      endif
      write(9,'(30a)')     '         Definition: { '
       write(line,'(9x,30a)')    'if ',trim(tAIMMSVariable(J))//'_calc',trim(tAIMSetDomain(J)),' <> inf '; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
       write(line,'(11x,30a)')   ' and ', trim(tAIMMSVariable(J))//'_pass',trim(tAIMSetDomain(J)),' <> inf then' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
       write(9,'(30a)')     '           abs(',trim(tAIMMSVariable(J))//'_calc',trim(tAIMSetDomain(J)),' - ',trim(tAIMMSVariable(J))//'_pass',trim(tAIMSetDomain(J)),') > Delta_diff'         
       write(9,'(30a)')          '         else'  
       write(9,'(30a)')          '             0'  
       write(9,'(30a)')          '         endif'  
      write(9,'(30a)' )    '                     }' 
      write(9,'(30a)' )    '      }' 
    endif        
  enddo
  write(9,'(5a)')         '   }'
  
  
  
!   Section Calc_Parameter_Declare--by Subroutine
  write(9,'(5a)')         '   Section ccalc_parameter_declare {'
  array_declared(:)=0
  do k=1,num_subs
    write(9,'(5a)')       '     DeclarationSection ',trim(SubList(k)), ' {'
    do i=1,narray
      if(array_declared(i).eq.0.and.passed(i).eq.1.and.index(allArrays_calc(i),'!').eq.0) then
        ifound=0
        do m=1,SubCount
          if(allArrays_pass(i).eq.ParamBySub_Parm(m) .and. ParamBySub_Subr(m).eq.SubList(k) ) then
            array_declared(i)=1
            ifound=1
          endif
        enddo
        if(ifound.eq.1) then
          write(9,'(30a)' )    '       Parameter ',trim(allArrays_calc(i)),' {'
          if(allArraysDim(i).gt.0) then
            write(9,'(30a)' )    '           IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'
          endif
          col_id=array_col_ID(i)
          row_id=array_row_ID(i)
          if(col_id.gt.0 .and. row_id.gt.0) then
            write(9,'(30a)')     '           Text: "OML Columns: ',trim(colmask(col_id)),' and OML Rows: ',trim(rowmask(row_id)),'";'
          endif
          if(index(allArrays_calc(i),'UBOUND').gt.0) then  ! trying to avoid having coeff_ version add 0 for all combinations, since it is default: inf
              write(9,'(30a)' )    '         Range: nonnegative;'  
              write(9,'(30a)' )    '         Default: inf;'  
          endif
          write(9,'(30a)' )    '      }'  
        endif
      endif
    enddo
    write(9,'(a)')       '     }'
  enddo
  write(9,'(5a)')       '     DeclarationSection OtherParams',' {'
  do i=1,narray
    if(array_declared(i).eq.0.and.passed(i).eq.1.and.index(allArrays_calc(i),'!').eq.0) then
      array_declared(i)=1 ! shouldn't matter at this point but just in case
      write(9,'(30a)' )    '       Parameter ',trim(allArrays_calc(i)),' {'
      if(allArraysDim(i).gt.0) then
        write(9,'(30a)' )    '           IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'
      endif
      col_id=array_col_ID(i)
      row_id=array_row_ID(i)
      if(col_id.gt.0 .and. row_id.gt.0) then
        write(9,'(30a)')     '           Text: "OML Columns: ',trim(colmask(col_id)),' and OML Rows: ',trim(rowmask(row_id)),'";'
      endif
      if(index(allArrays_calc(i),'UBOUND').gt.0) then  ! trying to avoid having coeff_ version add 0 for all combinations, since it is default: inf
        write(9,'(30a)' )    '         Range: nonnegative;'  
        write(9,'(30a)' )    '         Default: inf;'  
      endif
      write(9,'(30a)' )    '       }'  
    endif
  enddo
  write(9,'(a)')       '     }'
  write(9,'(a)')       '   }'


!   Section cpass_Parameter_Declare--Order as input from VERarrays_all.txt in one big section
    write(9,'(5a)')         '   DeclarationSection cpass_parameter_declare {'
    do i=1,narray
      if(index(allArrays_pass(i),'!').eq.0  ) Then
        write(9,'(30a)' )    '       Parameter ',trim(allArrays_pass(i)),' {'
        if(allArraysDim(i).gt.0) then
          write(9,'(30a)' )    '           IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'
        endif
        col_id=array_col_ID(i)
        row_id=array_row_ID(i)
        if(col_id.gt.0 .and. row_id.gt.0) then
           write(9,'(30a)')     '           Text: "OML Columns: ',trim(colmask(col_id)),' and OML Rows: ',trim(rowmask(row_id)),'";'
        endif
        if(index(allArrays_pass(i),'UBOUND').gt.0) then  ! trying to avoid having coeff_ version add 0 for all combinations, since it is default: inf.
          write(9,'(30a)' )    '         Range: nonnegative;'  
          write(9,'(30a)' )    '         Default: inf;'  
        endif
        write(9,'(30a)' )    '       }'  
      endif
    enddo
    write(9,'(a)')       '     }'

!   Section cdiff_Parameter_Declare--Order as input from VERarrays_all.txt in one big section
    write(9,'(5a)')         '   DeclarationSection check_parameter_declare {'
    do i=1,narray
      if(index(allArrays_diff(i),'!').eq.0  ) Then
        write(9,'(30a)' )    '       Parameter ',trim(allArrays_diff(i)),' {'
        if(allArraysDim(i).gt.0) then
          write(9,'(30a)' )    '           IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'
        endif
        col_id=array_col_ID(i)
        row_id=array_row_ID(i)
        if(col_id.gt.0 .and. row_id.gt.0) then
           write(9,'(30a)')     '           Text: "OML Columns: ',trim(colmask(col_id)),' and OML Rows: ',trim(rowmask(row_id)),'";'
        endif
        !if(index(allArrays_diff(i),'UBOUND').gt.0) then  ! trying to avoid having diff_ version add 0 for all combinations, since it is default: inf.
         !write(9,'(30a)' )    '         Range: nonnegative;'  
         !write(9,'(30a)' )    '         Default: inf;'  
        !endif
! add defintion as absolute difference between calculated and passed values, 
! but include a commented line that could convert the expression to a relative difference
        write(9,'(30a)')     '           Definition: { '
         if(index(allArrays_diff(i),'UBOUND').gt.0) then
            write(line,'(9x,30a)')    'if ( ',trim(allArrays_calc(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') <> inf '     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
            write(line,'(11x,30a)')   'and ', trim(allArrays_pass(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') <> inf ) then' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
         endif
        write(line,'(12x,30a)')    '( ',trim(allArrays_calc(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') -'     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(14x,30a)')         trim(allArrays_pass(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') )'     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(9x, 30a)') '! /$ ',trim(allArrays_calc(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') *100'  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
         
         if(index(allArrays_diff(i),'UBOUND').gt.0) then
             write(line,'(9x,30a)')    'elseif ( ',trim(allArrays_calc(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') = inf '     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
             write(line,'(11x,30a)')   'and ', trim(allArrays_pass(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') = inf ) then' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
             write(9,'(30a)')     '                 0'
             write(line,'(9x,30a)')    'elseif ( ',trim(allArrays_calc(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') = inf ) then'     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
             write(9,'(30a)')     '                 inf'
             write(line,'(9x,30a)')    'elseif ( ', trim(allArrays_pass(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') = inf ) then'     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
             write(9,'(30a)')     '                 -inf'
             write(9,'(30a)')     '         endif'
         endif
        write(9,'(30a)')     '           }'

        write(9,'(30a)' )    '       }'  
      endif
    enddo
    write(9,'(a)')       '     }'
    

!   Section Comparison_Parameter_Declaration-Order as input from VERarrays_all.txt in one big section
    write(9,'(5a)')         '   DeclarationSection Comparison_Parameter_Declaration {'
    write(9,'(5a)')         '       Parameter cdiff_Delta;'
    write(9,'(5a)')         '       Parameter cdiffPct_Delta;'
    do i=1,narray
      if(index(allArrays_compare(i),'!').eq.0  ) Then
        write(9,'(30a)' )    '       Parameter ',trim(allArrays_compare(i)),' {'
        if(allArraysDim(i).gt.0) then
          write(9,'(30a)' )    '           IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'
        endif
        col_id=array_col_ID(i)
        row_id=array_row_ID(i)
        if(col_id.gt.0 .and. row_id.gt.0) then
           write(9,'(30a)')     '           Text: "OML Columns: ',trim(colmask(col_id)),' and OML Rows: ',trim(rowmask(row_id)),'";'
        endif
        !if(index(allArrays_diff(i),'UBOUND').gt.0) then  ! trying to avoid having diff_ version add 0 for all combinations, since it is default: inf.
         !write(9,'(30a)' )    '         Range: nonnegative;'  
         !write(9,'(30a)' )    '         Default: inf;'  
        !endif
! add defintion as show cdiff values only if abs(cdiff) > cdiff_delta  
        write(9,'(30a)')     '           Definition: { '

        write(line,'(12x,30a)')    trim(allArrays_diff(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') $'     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(14x,30a)')    '(Abs(',trim(allArrays_cdiffPct(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),')) > cdiffPct_Delta'     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(14x,30a)')    ' and Abs(',trim(allArrays_diff(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),')) > cdiff_Delta)'     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)

        write(9,'(30a)')     '           }'

        write(9,'(30a)' )    '       }'  
      endif
    enddo
    write(9,'(a)')       '     }'

    
        

!   Section Percent_Comparison_Declaration as input from VERarrays_all.txt in one big section
    write(9,'(5a)')         '   DeclarationSection Percent_Comparison_Declaration {'
    do i=1,narray
      if(index(allArrays_Percent(i),'!').eq.0  ) Then
        write(9,'(30a)' )    '       Parameter ',trim(allArrays_Percent(i)),' {'
        if(allArraysDim(i).gt.0) then
          write(9,'(30a)' )    '           IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'
        endif
        col_id=array_col_ID(i)
        row_id=array_row_ID(i)
        if(col_id.gt.0 .and. row_id.gt.0) then
           write(9,'(30a)')     '           Text: "OML Columns: ',trim(colmask(col_id)),' and OML Rows: ',trim(rowmask(row_id)),'";'
        endif
        !if(index(allArrays_diff(i),'UBOUND').gt.0) then  ! trying to avoid having diff_ version add 0 for all combinations, since it is default: inf.
         !write(9,'(30a)' )    '         Range: nonnegative;'  
         !write(9,'(30a)' )    '         Default: inf;'  
        !endif
! add defintion as show cdiffPct values only if abs(cdiffPct) > cdiffPct_delta and abs(cdiff) > cdiff_Delta
        write(9,'(30a)')     '           Definition: { '

        write(line,'(12x,30a)')    trim(allArrays_cdiffPct(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') $'     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(14x,30a)')    '(Abs(',trim(allArrays_cdiffPct(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),')) > cdiffPct_Delta'     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
         write(line,'(14x,30a)')    'and Abs(',trim(allArrays_diff(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),')) > cdiff_Delta)'     ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(9,'(30a)')     '           }'

        write(9,'(30a)' )    '       }'  
      endif
    enddo
    write(9,'(a)')       '     }'
    
  

!   Section cdiffPct_parameter_declare--Order as input from VERarrays_all.txt in one big section
    write(9,'(5a)')         '   DeclarationSection cdiffPct_parameter_declare {'
    do i=1,narray
      if(index(allArrays_cdiffPct(i),'!').eq.0  ) Then
        write(9,'(30a)' )    '       Parameter ',trim(allArrays_cdiffPct(i)),' {'
        if(allArraysDim(i).gt.0) then
          write(9,'(30a)' )    '           IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'
        endif
        col_id=array_col_ID(i)
        row_id=array_row_ID(i)
        if(col_id.gt.0 .and. row_id.gt.0) then
           write(9,'(30a)')     '           Text: "OML Columns: ',trim(colmask(col_id)),' and OML Rows: ',trim(rowmask(row_id)),'";'
        endif
        !if(index(allArrays_diff(i),'UBOUND').gt.0) then  ! trying to avoid having diff_ version add 0 for all combinations, since it is default: inf.
         !write(9,'(30a)' )    '         Range: nonnegative;'  
         !write(9,'(30a)' )    '         Default: inf;'  
        !endif
! add defintion as show cdiff values only if cdiff values > 0 
        write(9,'(30a)')     '           Definition: { '
        write(line,'(12x,30a)')    'if (',trim(allArrays_calc(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') <> inf '  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(12x,30a)')    ' and ', trim(allArrays_pass(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') <> inf ) then'  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(15x,30a)')    'if (', trim(allArrays_calc(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') <> 0 ) then'  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(17x,30a)')    trim(allArrays_diff(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') /$ '  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(18x,30a)')    trim(allArrays_calc(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') *100' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(15x,30a)')    'elseif (', trim(allArrays_pass(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') <> 0 ) then'  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(17x,30a)')    trim(allArrays_diff(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') /$ '  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(18x,30a)')    trim(allArrays_pass(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') *100' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(15x,30a)')     'endif';write(9,'(a)') trim(line)
        write(line,'(12x,30a)')     'else';write(9,'(a)') trim(line)
        write(line,'(15x,30a)')    trim(allArrays_diff(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') '  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        write(line,'(12x,30a)')     'endif';write(9,'(a)') trim(line)
        write(9,'(30a)')     '           }'

        write(9,'(30a)' )    '       }'  
      endif
    enddo
    write(9,'(a)')       '     }'
    
    
    
!   Section coeff_Parameter_Declare--Order as input from VERarrays_all.txt in one big section
    write(9,'(5a)')         '   DeclarationSection coeff_parameter_declare {'
    do i=1,narray
      if(index(allArrays_pass(i),'!').eq.0  .and. index(allArrays_pass(i),'_LBOUND').eq.0  .and. index(allArrays_pass(i),'_UBOUND').eq.0) Then

        write(9,'(30a)' )    '       Parameter ',trim(allArrays_coeff(i)),' {'
        write(line,'(30a)' )    '           IndexDomain: (',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;'  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        col_id=array_col_ID(i)
        row_id=array_row_ID(i)
        if(col_id.gt.0 .and. row_id.gt.0) then
          write(9,'(30a)')     '           Text: "OML Columns: ',trim(colmask(col_id)),' and OML Rows: ',trim(rowmask(row_id)),'";'
        endif
        if(passed(i).ge.1) then
          write(line,'(30a)')     '           Definition: ',trim(allArrays_pass(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        else
          write(line,'(30a)')     '           Definition: ',trim(allArrays_calc(i)),'(',(trim(allSets(j,i)),',',j=1,allArraysDim(i)-1),trim(allSets(allArraysDim(i),i)),') ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        endif
      
        write(9,'(30a)' )    '       }'  
      endif
    enddo
    write(9,'(a)')       '     }'

!   DeclarationSection VariableDeclare
  write(9,'(5a)')          '   DeclarationSection Decision_Variable_Declare {'
  do i=1,num_cols
    nj=numColSets(i)
    if(index(colname(i),'RHS').eq.0 .and. index(colname(i),'!').eq. 0) then
      write(9,'(30a)' )    '     Variable ',trim(colname(i)),' {'
      if(nj.gt.0) then
        write(line,'(30a)' )  '         IndexDomain: (',                     (trim(colSets(i,j)),',',j=1,nj-1),trim(colSets(i,nj)),') ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
      endif
      if(len_trim(colmask(i)).gt.0) then 
         write(9,'(30a)' ) '         Text: "OML Columns: ',trim(colmask(i)),'";'
      endif
      if(isBounded(i)) then
        if(nj.gt.0) then
         write(line,'(40a)' ) '         Range: [','coeff_',trim(colname(i)),'_LBOUND(',(trim(colSets(i,j)),',',j=1,nj-1),trim(colSets(i,nj)),'),', &
                                               'coeff_',trim(colname(i)),'_UBOUND('    ,(trim(colSets(i,j)),',',j=1,nj-1),trim(colSets(i,nj)),')] ;'  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        else
         write(line,'(40a)' ) '         Range: [','coeff_',trim(colname(i)),'_LBOUND,', &
                                               'coeff_',trim(colname(i)),'_UBOUND] ;'  ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
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
      if(len_trim(xc_comment(i)).gt.0) then
        write(9,'(30a)')   '         Comment: "',trim(xc_comment(i)),'";'
      endif


      write(9,'(30a)' )    '     }'  
      if(isBounded(i)) then 
        write(9,'(30a)' )    '     Parameter ','coeff_',trim(colname(i)),'_LBOUND {'
        if(nj.gt.0) then
          write(line,'(30a)' )    '         IndexDomain: (',(trim(colSets(i,j)),',',j=1,nj-1),trim(colSets(i,nj)),') ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        endif
        write(9,'(30a)' )    '         Range: nonnegative;'  

! declare coefficient parameters that hold the bounds
        ifound=0
        param='coeff_'//trim(colname(i))//'_LBOUND'
        do k=1,narray
          if(param.eq.allArrays_coeff(k)) then
            ifound=k
            exit
          endif
        enddo
        if(ifound.gt.0) then
          k=ifound
          if(passed(ifound).ge.1) then
            if(nj.gt.0) then
              write(line,'(30a)')     '           Definition: ',trim(allArrays_pass(k)),'(',(trim(allSets(j,k)),',',j=1,allArraysDim(k)-1),trim(allSets(allArraysDim(k),k)),') ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
            else
              write(line,'(30a)')     '           Definition: ',trim(allArrays_pass(k)),' ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
            endif
          else
            if(nj.gt.0) then
              write(line,'(30a)')     '           Definition: ',trim(allArrays_calc(k)),'(',(trim(allSets(j,k)),',',j=1,allArraysDim(k)-1),trim(allSets(allArraysDim(k),k)),') ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
            else
              write(line,'(30a)')     '           Definition: ',trim(allArrays_calc(k)),' ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
            endif
          endif
        endif
        write(9,'(30a)' )    '     }'  
        write(9,'(30a)' )    '     Parameter ','coeff_',trim(colname(i)),'_UBOUND {'
        if(nj.gt.0) then
          write(line,'(30a)' )    '         IndexDomain: (',(trim(colSets(i,j)),',',j=1,nj-1),trim(colSets(i,nj)),') ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
        endif
        write(9,'(30a)' )    '         Range: nonnegative;'  
        write(9,'(30a)' )    '         Default: inf;'  
        ifound=0
        param='coeff_'//trim(colname(i))//'_UBOUND'
        do k=1,narray
          if(param.eq.allArrays_coeff(k)) then
            ifound=k
            exit
          endif
        enddo
        if(ifound.gt.0) then
          k=ifound
          if(passed(ifound).ge.1) then
            if(nj.gt.0) then
              write(line,'(30a)')     '         Definition: ',trim(allArrays_pass(k)),'(',(trim(allSets(j,k)),',',j=1,allArraysDim(k)-1),trim(allSets(allArraysDim(k),k)),') ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
            else
              write(line,'(30a)')     '         Definition: ',trim(allArrays_pass(k)),' ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
            endif
          else
            if(nj.gt.0) then
              write(line,'(30a)')     '         Definition: ',trim(allArrays_calc(k)),'(',(trim(allSets(j,k)),',',j=1,allArraysDim(k)-1),trim(allSets(allArraysDim(k),k)),') ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
            else
              write(line,'(30a)')     '         Definition: ',trim(allArrays_calc(k)),' ;' ; call Mreplace(line,'()',''); write(9,'(a)') trim(line)
            endif
          endif
        endif
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
    if( index(rowname(i),'!').eq. 0 .and. trim(rowname(i)).ne.'ECPCOSTS'.and.trim(rowname(i)).ne.'EFDCOSTS'.and.rowtype(i).ne.'N' .and. rowname(i)(2:6).ne.'BOUND') then
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

  if(ver.eq.'ecp') write(9,'(a)')         '       Variable ECPCOSTS {                                                                  '
  if(ver.eq.'efd') write(9,'(a)')         '       Variable EFDCOSTS {                                                                  '
  write(9,'(a)')         '           Range: free;                                                                   '
  write(9,'(a)')         '           Definition: {                                                                  '
  first_time=.true.
  do i=1,narray
    if((index(allArrays(i),'_ECPCOSTS').gt.0 .or. index(allArrays(i),'_EFDCOSTS').gt.0) & 
       .and. allArraysDim(i).gt.0 .and. index(allArrays(i),'!').eq.0) then
       array=allArrays(i)
       L=len_trim(array)
       is=index(array,'coeff_')+6   ! 7  
       ie=is+  (index(array(is:),'_EFDCOSTS')+index(array(is:),'_ECPCOSTS'))   -2
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
    if(index(rowname(i),'!').eq. 0 .and. &
      trim(rowname(i)).ne.'ECPCOSTS' .and. trim(rowname(i)).ne.'EFDCOSTS' .and. &
      rowtype(i).ne.'N' .and. rowname(i)(2:6).ne.'BOUND') then
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
  write(9,'(a)')         '       }'     ! end of "Variable VERCOSTS"
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
!            comment: "this is a comment";
!        }

write(9,'(a)') '   DeclarationSection ConstraintDeclare {'
 
  do i=1,num_rows
    if(trim(rowname(i)).ne.'ECPCOSTS' .and. trim(rowname(i)).ne.'EFDCOSTS' .and. &
      rowtype(i).ne.'N' .and. rowname(i)(2:6).ne.'BOUND'.and. index(rowname(i),'!').eq.0) then
      first=.true.   
      num_inter(i)=0     ! number of intersecting columns.  if none found, do not include any definition.
! check for intersecting columns by looking for coefficient arrays with a matching row_id
      do j=1,narray
        col_id=0
        if(array_row_id(j).eq.i) then
          col_id=array_col_id(j)
          if(index(colname(col_id),'RHS').eq.0 .and. index(colname(col_id),'!').eq.0) then
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
                 write(9,'(30a)')  '        Property: Bound, ShadowPrice, Level;' 
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
        if(len_trim(xr_comment(i)).gt.0) then
          write(9,'(30a)')  '          Comment: "',trim(xr_comment(i)),'";'
        endif
        
      endif
      if(num_inter(i).gt.0) then
        write(9,'(30a)') '       }' ! end of Constraint: {
      endif
    endif
  enddo

! free rows treated as parameters
  do i=1,num_rows
    if(index(rowname(i),'!').eq.0.and.trim(rowname(i)).ne.'ECPCOSTS' .and. rowname(i).ne.'EFDCOSTS' .and. rowtype(i).eq.'N') then ! add free rows as parameters; retain the "r" prefix denoting row in the name  
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
         if(index(colname(col_id),'RHS').eq.0 .and. index(colname(col_id),'!').eq.0) then
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
 
 ! Produce WriteToNEMS procedure with display statements for all columns and rows for which solution values are needed by VER first; then display the others
 ! needed for Augustine's validation work
   write(9,'(a)') '  Procedure WriteToNEMS {' 
   write(9,'(a)') '    Body: {'
   if(ver.eq.'ecp') then
     write(9,'(a)') '      OutToNEMS_FileName:="OutToNEMS_"+formatstring("%i",curcalyr(1))+".txt";'
     write(9,'(a)') '      put OutToNEMS; ! opens the file and sets stage for subsequent display and put statements'
     write(9,'(a)') '      display ECP_WithoutSafety.ProgramStatus ;'
     write(9,'(a)') '      display ECP_WithSafety.ProgramStatus ;'
   else
     write(9,'(a)') '      OutToNEMS_FileName:="OutToNEMS_"+formatstring("%i",curcalyr(1))+"_"+formatstring("%>02i",curitr(1))+".txt";'
     write(9,'(a)') '      put OutToNEMS; ! opens the file and sets stage for subsequent display and put statements'
     write(9,'(a)') '      display EFD_WithoutSafety.ProgramStatus ;'
     write(9,'(a)') '      display EFD_WithSafety.ProgramStatus ;'
   endif
     do i=1,num_cols
       if(needsol_cols(i).ne.0 .and. index(colname(i),'!').eq.0) then
         if(Colname(i).ne.'cRHS') then
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
       if( (needsol_rows(i).ne.0 .and. num_inter(i).gt.0 .and. index(rowname(i),'!').eq.0) &
       .or. rowname(i).eq.'ECPCOSTS'.or. rowname(i).eq.'EFDCOSTS') then
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
     write(9,'(20a)') '  put " " ;' ! need blank line if next is scalar
     if(ver.eq.'ecp') then
       write(9,'(20a)')        '  if (AIMECPBG = 1) then'
     else
       write(9,'(20a)')        '  if (AIMEFDBG = 1) then'
     endif
     do i=1,num_cols
       if(needsol_cols(i).eq.0) then
         if(Colname(i).ne.'cRHS'.and. index(colname(i),'!').eq.0) then
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
        if( needsol_rows(i).eq.0 .and. num_inter(i).gt.0 .and. index(rowname(i),'!').eq.0)  then
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
!    create declaration section and procedure to pass back derived solution variables to NEMS
     write(9,'(a)') 'DeclarationSection PassBacktoNEMSDeclare {' 
     write(9,'(a)') '    ElementParameter NEMSYearIndex {'
     write(9,'(a)') '        Range: MNUMYR_;'
     write(9,'(a)') '    }'
     write(9,'(a)') '    Parameter SupplyRegionNumber {'
     write(9,'(a)') '        Range: integer;'
     write(9,'(a)') '    }'
     write(9,'(a)') '}'
     write(9,'(a)') '  Procedure PassBackToNEMS {' 
     write(9,'(a)') '    Body: {'
     if(ver.eq.'ecp') then
       write(9,'(a)') '      OutToNEMS_FileName:="PassBack_"+formatstring("%i",curcalyr(1))+".txt";'
       write(9,'(a)') '      put OutToNEMS; ! opens the file and sets stage for subsequent display and put statements'
     else
       write(9,'(a)') '      OutToNEMS_FileName:="PassBack_"+formatstring("%i",curcalyr(1))+"_"+formatstring("%>02i",curitr(1))+".txt";'
       write(9,'(a)') '      put OutToNEMS; ! opens the file and sets stage for subsequent display and put statements'
     endif
!     following do loop was added to permanently map transfer variables with their _calc versions  commented out by AKN on 5/25/22
!     do i=1,nuniq
!       J=tUnique(i)
!       if(transfer_usage(i).eq.3) then
!          if(tAIMSetDomain(J).ne.'(SCALAR)') then
!              write(9,'(30a)' )    '      ',trim(tAIMMSVariable(J)),trim(tAIMSetDomain(J)),':= ', trim(tAIMMSVariable(J)),'_calc',trim(tAIMSetDomain(J)),';'
!          else
!              write(9,'(30a)' )    '      ',trim(tAIMMSVariable(J)),':= ',trim(tAIMMSVariable(J)),'_calc;'
!          endif
!       endif
!     enddo
     write(9,'(a)'  ) '! output the EMM variables derived from the LP solution values'
     do i=1,nuniq
       J=tUnique(i)
       if(transfer_usage(i).gt.0   .and.(tDAFDim1(j).ne.'mnumnr'.and.tDAFDim2(j).ne.'mnumyr')) then
         if(transfer_usage(i).eq.3) then
            write(9,'(30a)' )    '      display {',trim(tAIMMSVariable(J))//'_calc','} where decimals := 10 ;'
         else
            write(9,'(30a)' )    '      display {',trim(tAIMMSVariable(J)),'} where decimals := 10 ;'
         endif  
       endif
     enddo

     ! Construct a for loop over SupplyRegion so that all the data for each region is displayed in groups.
     write(9,'(a)') '! write variables with implicit year and region indices as implemented via direct access files in nems.'
     write(9,'(a)') '! use NEMSYearIndex to set the Year and SupplyRegionNumber to set the Region   '   
     write(9,'(a)') '      NEMSYearIndex:=stringtoelement(mnumyr_,FormatString("%02i",' // " CURCALYR('1')-1989));"
     write(9,'(a)') '      for (SupplyRegion in SupplyRegion_) do'
     write(9,'(a)') '        SupplyRegionNumber:=ord(SupplyRegion);'
     write(9,'(a)') '        display SupplyRegionNumber;'
     do i=1,nuniq
       j=tUnique(i)
       if(transfer_usage(i).gt.0   .and.(tDAFDim1(j).eq.'mnumnr'.or.tDAFDim2(j).eq.'mnumyr')) then
           if(index(tAIMSetDomain(j),'MNUMYR').gt.0) then
              call Mreplace(tAIMSetDomain(j),'MNUMYR','NEMSYearIndex')
           endif
           if(transfer_usage(i).eq.3) then
              write(9,'(30a)' )    '        display {',trim(tAIMMSVariable(J))//'_calc',trim(tAIMSetDomain(j)),'} where decimals := 10 ;'
           else
              write(9,'(30a)' )    '        display {',trim(tAIMMSVariable(J)),trim(tAIMSetDomain(j)),'} where decimals := 10 ;'
           endif
       endif
     enddo
     write(9,'(a)') '      endfor;'
     write(9,'(20a)') '      putclose;'          
     write(9,'(a)') '    }' 
     write(9,'(a)') '  }' 
     write(9,'(a)') '}' 
     close(9)
    !  done with the AIMMS code. Tell user what to do next:
     
   write(6,'(a)') ' '
   write(6,'(a)') 'DIRECTIONS:'
   write(6,'(a)') ' '
   if(ans.eq.'y') then
     write(6,'(a)')'1)  Auto_'//ver//'.ams can now replace '//ver//'.ams in the aimms project folder "MainProject" in '//ver//'.zip.'
     write(6,'(a)')'    So copy auto_'//ver//'.ams to '//ver//'.ams, open '//ver//'.zip in windows explorer, and '
     write(6,'(a)')'    copy '//ver//'.ams into the '//ver//'.zip folder "MainProject"'
   else
     write(6,'(a)')'1)  Auto_'//ver//'.ams should replace the section of '//ver//'.ams beginning with "  DeclarationSection SetDeclare {"'
   endif
   write(6,'(a)')  ' '
   write(6,'(a)')  '2)  A new list of parameter arrays has been created: parameter_'//ver//'.csv'
   write(6,'(a)')  '    Open aim'//ver//'.xlxs and parameter_'//ver//'.csv in excel. copy all of parameter_'//ver//'.csv to'
   write(6,'(a)')  '    the area beneath the bolded headers in worksheet parameter in aim'//ver//'.xlsx.'
   write(6,'(a)')  ' '
   write(6,'(a)')  '3)  Reset the defined names, indicated by the bolded column headers, in worksheet parameter.'
   write(6,'(a)')  '    An easy way to do it is to highlight the parameter/set names AND the column headers, then'
   write(6,'(a)')  '    choose menu "formulas"...[look in tab "defined names"]..."create from selection"...select "top row".'
   write(6,'(a)')  ' '
   write(6,'(a)')  '4)  Replace a fortran code section in uaimms.f with the code generated in this file: transfer_code_'//ver//'.f'
   write(6,'(a)')  '    The section to replace is a series of calls to "AIMMS_TransArray_out_'//ver//'" that appear in in subroutine AIMMS_Transfer_Out_'//ver//'.'
   write(6,'(a)')  '    '
   write(6,'(a)')  '5)  Replace a fortran code section in uaimms.f with the code generated in this file: passback_code_'//ver//'.f'
   write(6,'(a)')  '    The section to replace is a series of select CASE statements with calls to "AIMMS_Transfer_in_'//ver//'"' 
   write(6,'(a)')  '    that appear in in subroutine AIMMS_InTxtVar_'//ver//'.'
   write(6,'(a)')  '    '
      

!========================================================================================================================= 

 stop
 end
 
 subroutine dimlist(alldims,superset,maxdims,nlist,dim,ndim,super)
 implicit none
 integer maxdims,ndim,nlist
 character*50 alldims(maxdims),superset(maxdims),dim(ndim),super(ndim)
 integer i,j
 integer ifound
 ! adds any new strings in dim(1:ndim) to alldim(1:maxdims)and increases nlist to reflect the count
 do i=1,ndim
   ifound=0
   do j=1,nlist
     if(alldims(j).eq.dim(i)) then
       ifound=j
       exit
     endif
   enddo
   if(ifound.eq.0)then
      nlist=nlist+1
      alldims(nlist)=dim(i)
      ifound=nlist
   endif
   if(super(i).ne.' ') then
     superset(ifound)=super(i)
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

!
          subroutine vrange(FortranVariable,fortdim,aimdim,ver,emmvar_start,emmvar_end,daf,ipos)
! set up starting and ending arguments for call to transfer_out
          implicit none
          character(len=*) FortranVariable,fortdim,aimdim,ver,emmvar_start(7),emmvar_end(7)
          integer daf,ipos
          if(FortDim(1:2).eq.'0:')then
             emmvar_start(ipos)='0'
             emmvar_end(ipos)=Fortdim(3:)
             fortdim=fortdim(3:)
          endif
          if(FortDim(1:2).eq.'2:')then
             emmvar_start(ipos)='2'
             emmvar_end(ipos)=Fortdim(3:)
             fortdim=fortdim(3:)
          endif
          if(FortDim(1:3).eq.'-2:')then
             emmvar_start(ipos)='-2'
             emmvar_end(ipos)=Fortdim(4:)
             fortdim=fortdim(4:)
          endif
          if(FortDim(1:3).eq.'-1:')then    ! added by  AKN to handle OGSMRegion_SUP with set element starting from '-1'
             emmvar_start(ipos)='-1'
             emmvar_end(ipos)=Fortdim(4:)
             fortdim=fortdim(4:)
          endif
          if(aimdim.eq.'MNUMYR'.and.daf.eq.1)then
            emmvar_start(ipos)='iyr'
            emmvar_end(ipos)='iyr'
          elseif(aimdim.eq.'SupplyRegion'.and.daf.eq.1) then
            emmvar_start(ipos)='irg'
            emmvar_end(ipos)='irg'
          else
            emmvar_end(ipos)=fortdim
          endif
          return
          end
! ********************************************************************************
      subroutine lower(a)
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
      end
! ********************************************************************************
      subroutine upper(a)
      implicit none
      character*(*) a
      integer L,j  ,i
      L=len(a)
      do i=1,L
        j=ichar(a(i:i))
        if(j.ge.97.and.j.le.122)then
          a(i:i)=char(j-32)
        endif
      enddo
      return
      end
! ********************************************************************************

include 'nemswk1.f'
include 'cio4wk1.f'

  