! $Header: m:/default/source/RCS/readaimlis.f90,v 1.6 2020/10/28 17:38:55 AKN Exp $
 ! ReadAIMLis is designed to translate the AIMMS/CPlex MPS file
 ! into a form that can be readily compared to a corresponding OML-based MPS file.
 
 ! This program first reads an AIMMS ecp.lis or efd.lis containing
 ! the aimms-to-cplex crosswalk. The AIMMS project settings are set to
 ! generate this information, along with a cplex mps file.
 !
 ! It also reads the oml-AIMMS interface spreadsheet (aimefd.xlsx or aimecp.xlsx)
 ! that provides the structure of the AIMMS row and column identifiers.
 ! It also reads the set data file that lists elements of all the AIMMS sets used.
 !
 ! It then:
 ! 1) creates a new version of the (.lis) listing file with the corresponding oml row and
 !    column names added to the aimms/cplex rows.  It also stores the
 !    cplex-oml name pairs in memory, indexed by cplex row and column number.
 ! 2) Reads the cplex mps file (such as cpx.mps) and creates a new version
 !    with the cplex row/column names replaced with the OML row and column 
 !    names.
 ! 3) Writes the coefficients (equivalent to the MPS COLUMNS section) to
 !    a file "aimmat_unsorted_yyyy.txt" in the output folder. After sorting
 !    that to aimmat.txt, it can be easily compared to equivalent output from the
 !    pack_all program, in files like "EC_2016_mat.txt" and "ED_2016_mat.txt".
 !    
 !    After running this program, run the program "readmat" to generate a
 !    side-by-side comparison of the aimms/cplex and oml coefficients. 
   
 !  To compile/link this program, use the command:
 !
 !   ifort /include='m:\\default\\includes' readaimlis.f90 /link /force
 !
 ! When compiling, it expects the following two module definition files in the same folder 
 !
 ! Three input files are needed. They are expected to be found in the folder where this is run (ie.,  from folder /ecp/log),
 ! or its parent folder. The parent folder is assumed to be a copy of the AIMMS project folder (ecp or efd) in a NEMS output folder
 !  1) an aimms  .lis file from the efd or ecp project, in current log folder
 !  2) set data file (found in parent folder):  either ../efdsetdata.dat or ../ecpsetdata.dat
 !  3) ../../input/aimefd.xlsx or ../../input/aimecp.xlsx 
 !********************* readaimlis_efd.exe******************************************************************************
 !*  readaimlis_efd can take three command line arguements 
 !*  first argument is four digit CALENDAR year .e.g 2016 
 !*  second argument is a three letter code in all lower case   .e.g. efd or ecp
 !*  third argument is two digit ITERATION number .e.g. 01, 02, 03, etc  note: put any two digit number for ecp
 !***********************************************************************************************************************
 include 'nemswk1.f'
 include 'cio4wk1.f'
 include 'filemgr.f'
 include 'uaimms_readaim.f'
 
 program readaimlis_efd
 use ifport
 use ecp_row_col
 use efd_row_col
 implicit none
 
 character*3 aimname ! 'ecp' or efd'
 character*4 cyear
 character*90 lisname,outname
 character*90 mpsname,outmps,unsort
 character*2 iteration
 character*90 cmd
 character*2 citer
 integer iter
 logical lexist, tryagain
 integer iret
 
 cyear='2016'
 aimname='efd' 
 tryagain = .FALSE.
5 continue
 if (COMMAND_ARGUMENT_COUNT() .NE. 3 .OR. tryagain) THEN
    write(6,'(a\)') 'Enter 4-digit model year, like 2016: '
    read(5,'(a)')    cyear
     write(6,'(a\)') 'AIMMS project name. Enter ecp or efd: '
     read(5,'(a)')   aimname
     if(aimname.eq.'efd') then
       write(6,'(a\)')  'Enter iteration number (two digits e.g. 01 for 1, usually the final one): '
       read(5,*)   iter
       write(citer,'(i2.2)') iter
     endif
else
    CALL GET_COMMAND_ARGUMENT(1, cyear)
    CALL GET_COMMAND_ARGUMENT(2, aimname)
    CALL GET_COMMAND_ARGUMENT(3, citer)
endif
 ! fill input file name, lisname, for example: 'ecp_2016.lis'
 if (aimname.eq.'ecp') then
   lisname=aimname//'_'//cyear//'.lis'
   outname=aimname//'_'//cyear//'_oml.lis'
   inquire(file='aimecp.xlsx',exist=lexist)
   if (.not. lexist) then
     cmd='copy ..\..\input\aimecp.xlsx aimecp.xlsx'
     iret=system(cmd)
   endif
 elseif(aimname.eq.'efd') then

   lisname=aimname//'_'//cyear//'_'//citer//'.lis'
   outname=aimname//'_'//cyear//'_'//citer//'_oml.lis'
   if (.not. lexist) then
     cmd='copy ..\..\input\aimefd.xlsx aimefd.xlsx'
     iret=system(cmd)
   endif
 else
   write(6,*) 'aimname must be in lower case: ecp or efd'
   stop
 endif  
 inquire(file=lisname,exist=lexist)
 if(.not. lexist) then
   write(6,'(a)') trim(lisname)//' not found. try another year. ctrl-c to cancel'
   tryagain = .TRUE.
   goto 5
 endif
 open(8,file=lisname,status='old',readonly)
 open(9,file=outname,status='unknown')
 rewind 9
 
 if(aimname.eq.'ecp') then
    write(6,*)'call addomlecp'
   call addomlecp
 elseif(aimname.eq.'efd') then
   write(6,*)'call addomlefd'
   call addomlefd
 endif
 close(8)
 close(9)
 
 unsort='../../aimmat_unsorted_'//cyear//'.txt'
 if(aimname.eq.'efd') then
   mpsname='../cpx_'//cyear//'_'//citer//'.mps'
   outmps ='../cpx_'//cyear//'_'//citer//'_oml.mps'
   open(8,file=mpsname,status='old',readonly)
   open(9,file=outmps,status='unknown')
   open(10,file=unsort,status='unknown')  ! input for readmat.f90 program, after sorting, to compare to oml/analyze mps from makemps
   rewind 9
   rewind 10
   call mpsomlefd
   write(6,'(a)') trim(outname)//' created'
 else
   mpsname='../cpx_'//cyear//'.mps'
   outmps ='../cpx_'//cyear//'_oml.mps'
   open(8,file=mpsname,status='old',readonly)
   open(9,file=outmps,status='unknown')
   open(10,file=unsort,status='unknown')  ! input for readmat.f90 program, after sorting, to compare to oml/analyze mps from makemps
   rewind 9
   rewind 10
   call mpsomlecp
   write(6,'(a)') trim(outmps)//' created'
 endif


 stop
 end
 subroutine mpsomlecp
 ! read aimms/cplex mps and substitute names for oml names.
 ! convert to free rows.
 use ecp_row_col
 implicit none
 character*100 line,cpxnam
 integer rectype ! 0: ignore, 1:column, 2:row 
 integer icol,irow,j
 logical lnew
  rectype=0
 10 continue
   read(8,'(a)',end=99) line
   if(line(1:4).eq.'ROWS') then
     rectype=1
     write(9,'(a)') trim(line)
     goto 10

   elseif(line(1:7).eq.'COLUMNS') then
      rectype=2
       write(9,'(a)') trim(line)
     goto 10
    
   elseif(line(1:3).eq.'RHS') then
      rectype=3
       write(9,'(a)') trim(line)
     goto 10

   elseif(line(1:6).eq.'BOUNDS') then
      rectype=4
      write(9,'(a)') trim(line)
     goto 10
   elseif(line(1:6).eq.'ENDATA') then
      rectype=5
      write(9,'(a)') trim(line)
      goto 99
   endif    

   if(rectype.eq.0) then
     write(9,'(a)') trim(line)
     goto 10
   elseif(rectype.gt.0 .and.len_trim(line).eq.0) then
     write(9,'(a)') trim(line)
     goto 10
   endif
    
   if(rectype.eq.1) then                  ! ROWS
      ecprownam=' '
      cpxnam=line(5:12)
      row_type=line(2:2)
      if(cpxnam(1:3).eq.'obj') then
        ecprownam='ECPCOSTS'
      else

        read(cpxnam(2:8),'(i7)') irow
        if(irow.gt.cnt_ecp_row) then
          cnt_ecp_row=irow
        endif
        ecprownam=ecp_row_sol(irow).ecprownam
        ecp_row_sol(irow).ecprownam=ecprownam
        ecp_row_sol(irow).row_type=row_type

      endif
      line(5:12)=ecprownam
      lnew=.true.
      do j=1,cnt_ecp_row
        if(j.ne.irow) then
           if( ecp_row_sol(j).ecprownam .eq. ecprownam) then
              lnew=.false.
              exit
           endif
        endif
      enddo
      if(lnew) then
        write(9,'(a)') trim(line)
      else
        write(6,'(a)') 'warning, duplicate row found: '//ecprownam
      endif
   elseif(rectype.eq.2) then              ! COLUMNS (coefficients at column/row intersections)
      ecprownam=' '
      ecpcolnam=' '
      cpxnam=line(5:12)
      read(cpxnam(2:8),'(i7)') icol
      ecpcolnam=ecp_col_sol(icol).ecpcolnam
      cpxnam=line(15:22)
      if(cpxnam(1:3).eq.'obj') then
        ecprownam='ECPCOSTS'
      else
        read(cpxnam(2:8),'(i7)') irow
        if(irow.gt.cnt_ecp_row) then
          cnt_ecp_row=irow
        endif
        ecprownam=ecp_row_sol(irow).ecprownam
      endif
      if(  len_trim(ecpcolnam).eq.0 .or. len_trim(ecprownam).eq.0 ) then
        write(6,'(a8,i7,a8,5x,a8,i7,a8)') 'ecpcolnam=',icol,ecpcolnam,line(5:12),'ecprownam=',irow,ecprownam,line(15:22)
      endif
      line( 5:12)=ecpcolnam
      line(15:22)=ecprownam
      write(9,'(a)') trim(line)

      if(ecp_col_sol(icol).aimms_col_ID_num .gt. 0) then ! skipping columns added as free rows, which have no AIMMS_COL_ID_num
        write(10,'(7x,a8,a1,1x,a8,1x,a)') ecpcolnam,':',ecprownam,line(23:47)
     endif
 
   elseif(rectype.eq.3) then              ! RHS
      ecprownam=' '
      ecpcolnam='RHSECP'
      cpxnam=line(15:22)
      read(cpxnam(2:8),'(i7)') irow
      if(irow.gt.cnt_ecp_row) then
        cnt_ecp_row=irow
      endif
      ecprownam=ecp_row_sol(irow).ecprownam
      line( 5:12)=ecpcolnam
      line(15:22)=ecprownam
      lnew=.true.
      do j=1,cnt_ecp_row
        if(j.ne.irow) then
           if( ecp_row_sol(j).ecprownam .eq. ecprownam) then
              lnew=.false.
              exit
           endif
        endif
      enddo
      if(lnew) then
         write(9,'(a)') trim(line)
         write(10,'(7x,a8,a1,1x,a8,1x,a)') ecpcolnam,':',ecprownam,line(23:47)   !added by AKN to write out RHS
      else
        write(6,'(a)') 'warning, duplicate RHS row found: '//ecprownam
      endif

   elseif(rectype.eq.4) then              ! BOUNDS
      ecprownam=' '
      cpxnam=line(15:22)
      read(cpxnam(2:8),'(i7)') icol
      ecpcolnam=ecp_col_sol(icol).ecpcolnam
      line( 5:12)='BOUND   '
      line(15:22)=ecpcolnam
      write(9,'(a)') trim(line)
      write(10, '(7x,a8,a1,1x,a8,1x,a)') ecpcolnam,':',line(2:4)//'     ',line(23:47)   !added by AKN to write out bounds
   endif
   
   goto 10
    
    99 continue

 return
 end
  subroutine mpsomlefd
 ! read aimms/cplex mps and substitute names for oml names.
 ! convert to free rows.
 use efd_row_col
 implicit none
 character*100 line,cpxnam
 integer rectype ! 0: ignore, 1:column, 2:row 
 integer icol,irow,j
 logical lnew
  rectype=0
 10 continue
   read(8,'(a)',end=99) line
   if(line(1:4).eq.'ROWS') then
     rectype=1
     write(9,'(a)') trim(line)
     goto 10

   elseif(line(1:7).eq.'COLUMNS') then
      rectype=2
       write(9,'(a)') trim(line)
     goto 10
    
   elseif(line(1:3).eq.'RHS') then
      rectype=3
       write(9,'(a)') trim(line)
     goto 10

   elseif(line(1:6).eq.'BOUNDS') then
      rectype=4
      write(9,'(a)') trim(line)
     goto 10
   elseif(line(1:6).eq.'ENDATA') then
      rectype=5
      write(9,'(a)') trim(line)
      goto 99
   endif    

   if(rectype.eq.0) then
     write(9,'(a)') trim(line)
     goto 10
   elseif(rectype.gt.0 .and.len_trim(line).eq.0) then
     write(9,'(a)') trim(line)
     goto 10
   endif
    
   if(rectype.eq.1) then                  ! ROWS
      efdrownam=' '
      cpxnam=line(5:12)
      row_type=line(2:2)
      if(cpxnam(1:3).eq.'obj') then
        efdrownam='EFDCOSTS'
      else
        read(cpxnam(2:8),'(i7)') irow
        if(irow.gt.cnt_efd_row) then
          cnt_efd_row=irow
        endif
        efdrownam=efd_row_sol(irow).efdrownam
        efd_row_sol(irow).efdrownam=efdrownam
        efd_row_sol(irow).row_type=row_type
      endif
      line(5:12)=efdrownam
      lnew=.true.
      do j=1,cnt_efd_row
        if(j.ne.irow) then
           if( efd_row_sol(j).efdrownam .eq. efdrownam) then
              lnew=.false.
              exit
           endif
        endif
      enddo
      if(lnew) then
        write(9,'(a)') trim(line)
      else
        write(6,'(a)') 'warning, duplicate row found: '//efdrownam
      endif

   elseif(rectype.eq.2) then              ! COLUMNS (coefficients at column/row intersections)
      efdrownam=' '
      efdcolnam=' '
      cpxnam=line(5:12)
      read(cpxnam(2:8),'(i7)') icol
      efdcolnam=efd_col_sol(icol).efdcolnam
      cpxnam=line(15:22)
      if(cpxnam(1:3).eq.'obj') then
        efdrownam='EFDCOSTS'
      else
        read(cpxnam(2:8),'(i7)') irow
        if(irow.gt.cnt_efd_row) then
          cnt_efd_row=irow
        endif
        efdrownam=efd_row_sol(irow).efdrownam
        row_type=efd_row_sol(irow).row_type
      endif
      if(  len_trim(efdcolnam).eq.0 .or. len_trim(efdrownam).eq.0 ) then
        write(6,'(a8,i7,a8,5x,a8,i7,a8)') 'efdcolnam=',icol,efdcolnam,line(5:12),'efdrownam=',irow,efdrownam,line(15:22)
      endif
      line( 5:12)=efdcolnam
      line(15:22)=efdrownam
      write(9,'(a)') trim(line)
      if(efd_col_sol(icol).aimms_col_ID_num .gt. 0) then ! skipping columns added as free rows, which have not AIMMS_COL_ID_num
        write(10,'(7x,a8,a1,1x,a8,1x,a16)') efdcolnam,':',efdrownam,line(32:47)
      endif

   elseif(rectype.eq.3) then              ! RHS
      efdrownam=' '
      efdcolnam='RHSEFD'
      cpxnam=line(15:22)
      read(cpxnam(2:8),'(i7)') irow
      if(irow.gt.cnt_efd_row) then
        cnt_efd_row=irow
      endif
      efdrownam=efd_row_sol(irow).efdrownam
      line( 5:12)=efdcolnam
      line(15:22)=efdrownam
      lnew=.true.
      do j=1,cnt_efd_row
        if(j.ne.irow) then
           if( efd_row_sol(j).efdrownam .eq. efdrownam) then
              lnew=.false.
              exit
           endif
        endif
      enddo
      if(lnew) then
        write(9,'(a)') trim(line)
        write(10,'(7x,a8,a1,1x,a8,1x,a16)') efdcolnam,':',efdrownam,line(32:47)
      else
        write(6,'(a)') 'warning, duplicate row with RHS found: '//efdrownam
      endif
   elseif(rectype.eq.4) then              ! BOUNDS
      efdrownam=' '
      cpxnam=line(15:22)
      read(cpxnam(2:8),'(i7)') icol
      efdcolnam=efd_col_sol(icol).efdcolnam
      line( 5:12)='BOUND   '
      line(15:22)=efdcolnam
      write(9,'(a)') trim(line)
      write(10, '(7x,a8,a1,1x,a8,1x,a)') efdcolnam,':',line(2:4)//'     ',line(23:47)   !added by AKN to write out bounds
   endif
   
   goto 10
    
    99 continue

 return
 end

 subroutine addomlecp
 ! add oml names to end of cplex column and row numbers
 ! unit 8: lis file
 ! unit 9: output file with oml names added
 use ecp_row_col
 implicit none
 character*100 line
 integer rectype ! 0: ignore, 1:column, 2:row 
 integer icol,irow
 integer ncol, nrow
! read aimecp.xlsx to get row/column names, masks, etc
   call ecp_aimms_init
 ! read set names
   call read_ecp_sets
  
 ncol=0
 nrow=0
 rectype=0
 10 continue
    read(8,'(a)',end=99) line
    if(index(line,'Column number').gt.0) then
      rectype=1
      write(9,'(a)') trim(line)
      goto 10
    endif
    if(index(line,'Row number').gt.0) then
      rectype=2
      write(9,'(a)') trim(line)
      goto 10
    endif

    if(rectype.eq.0) then
      write(9,'(a)') trim(line)
      goto 10
    elseif(rectype.eq.1 .and.len_trim(line).eq.0.and.ncol.gt.0) then
      write(9,'(a)') trim(line)
      rectype=0
      goto 10
    elseif(rectype.eq.2 .and.len_trim(line).eq.0.and.nrow.gt.0) then
      write(9,'(a)') trim(line)
      rectype=0
      goto 10
    elseif(len_trim(line).eq.0) then
      write(9,'(a)') trim(line)
      goto 10   
    endif
    
    if(rectype.eq.1) then
      ecpcolnam=' '
      if(line(19:19).eq.'c') then
        call get_oml_colnam
        write(9,'(a,t60,a)') trim(line),ecpcolnam
        read(line(1:10),'(i10)') icol
        ecp_col_sol(icol).ecpcolnam=ecpcolnam
        ecp_col_sol(icol).aimms_col_ID_num=aimms_col_ID_num
      elseif(line(19:19).eq.'r') then
        ecprownam=' '
        call get_oml_rownam
        ecpcolnam=ecprownam ! free row entered as column/variable
        write(9,'(a,t60,a)') trim(line),ecpcolnam
        read(line(1:10),'(i10)') icol
        ecp_col_sol(icol).ecpcolnam=ecpcolnam
        ecp_col_sol(icol).aimms_col_ID_num=0
      endif  
      ncol=ncol+1
    elseif(rectype.eq.2) then
      ecprownam=' '
      call get_oml_rownam
      write(9,'(a,t60,a)') trim(line),ecprownam
      read(line(1:10),'(i10)') irow
      ecp_row_sol(irow).ecprownam=ecprownam
      ecp_row_sol(irow).aimms_row_ID_num=aimms_row_ID_num
      nrow=nrow+1
    else
      write(9,'(a)') trim(line)
    endif

    
    goto 10
    
    99 continue
    
    return
    
  CONTAINS
 
  subroutine get_oml_colnam
  implicit none
  integer ileftparen,nsets,is,i,ic,j,lm,la,ifound,icode,lf
  character*25 sets
  character*30 setname
  character*5 fields(max_set),afield
  character*5 zeroes/'00000'/
  character*24 identifier
      fields=' '
      ileftparen=index(line,'(')
       if(ileftparen.eq.0) then
         ileftparen=len_trim(line)
         nsets=0
       else
         sets=line(ileftparen+1:len_trim(line)-1)  ! sets is = to stuff between the (). if more than one set, then separated by comma
         nsets=1
         is=1
         do i=1,len_trim(sets)
           if(sets(i:i).eq.',') then
            fields(nsets)=sets(is:i-1)
            nsets=nsets+1
            is=i+1  ! next field starts after this current character which is a comma
           endif
         enddo
         fields(nsets)=sets(is:len_trim(sets))
         
       endif
       identifier=line(19:ileftparen-1)
! construct OML column name from the mask and set elements. If not an alias column, just use the elements as they are
       ecpcolnam=' '
       aimms_col_ID_num=0
       do ic=1, max_col_aimms
         if (identifier  .eq. col_aimms(ic).colnam_aimms ) then 
           aimms_col_ID_num=ic
           exit
         endif
       enddo
       if(aimms_col_id_num .eq.0) then
         write(6,*) 'aimms_col_id_num 0, line=',trim(line)
         return
       endif
 !============================================================================================

       lm=index(col_aimms(aimms_col_ID_num).colnam_mask, '!')  ! '!' is used to tag versions of the same mask, so use that as the length
       if(lm.gt.1) then
         lm=lm-1
       else
         lm=len_trim(col_aimms(aimms_col_ID_num).colnam_mask)
       endif
       j=0
       do i=1, lm
         if( index('()', col_aimms(aimms_col_ID_num).colnam_mask(i:i)) .eq.0) then  ! pass over any special masking characters in search of the constants
           j=j+1
           ecpcolnam(j:j)=col_aimms(aimms_col_ID_num).colnam_mask(i:i)         
         endif
       enddo
       cindstt(1:max_set)=col_aimms(aimms_col_ID_num).indstt(1:max_set)
       cindend(1:max_set)=col_aimms(aimms_col_ID_num).indend(1:max_set)
       do i=1,nsets
         afield=fields(i)  ! this field is a set element. 
           lf=index(col_aimms(aimms_col_ID_num).setnam(i),'_ALT')
           if(lf.gt.1) then  ! check for alias set name. if so, need to use primary set name to decode the element, then convert to the alternate alias element
           la=len_trim(afield)

           setname=col_aimms(aimms_col_ID_num).setnam(i)(1:lf-1)  ! strip off _ALT stuff to get AIMMS primary set name used in the composite table fields
           call get_ecp_set_element(setname,afield(:la),icode)         ! get the integer (ordinal) code for the set element 
           if(icode.gt.0) then
              call get_ecp_char_element(col_aimms(aimms_col_ID_num).setnam(i),icode,afield,ifound)
              la=len_trim(afield)
           endif
           if(icode.eq.0 .or. ifound.eq.0 .or. la.ne.(cindend(i)-cindstt(i)+1)) then
             write(6,'(3a)') 'AIMMS Interface error ECP with '//trim(identifier)// &
              ' solution. Could not get aliased set element for field: '//trim(fields(i))//' for set: '//trim(col_aimms(aimms_col_ID_num).setnam(i))//' and '//trim(setname)
             return
           endif
         endif
         if(cindstt(i).gt.0) then
           lf=cindend(i)-cindstt(i)+1
           la=len_trim(afield)
           if(len_trim(afield).lt.lf) then ! left pad integer set elements with 0
             afield=zeroes(1:lf-la)//afield
           endif  
           ecpcolnam(cindstt(i):cindend(i)) = afield(:lf)
         endif
       enddo
       return
       end subroutine get_oml_colnam
!================================================================================================================
  subroutine get_oml_rownam
  implicit none
  integer ileftparen,nsets,is,i,ic,j,lm,la,ifound,icode,lf
  character*25 sets
  character*30 setname
  character*5 fields(max_set),afield
  character*5 zeroes/'00000'/
  character*24 identifier
      fields=' '
      ileftparen=index(line,'(')
       if(ileftparen.eq.0) then
         ileftparen=len_trim(line)+1  ! pretend there is ( at the end of the line
         nsets=0
       else
         sets=line(ileftparen+1:len_trim(line)-1)  ! sets is = to stuff between the (). if more than one set, then separated by comma
         nsets=1
         is=1
         do i=1,len_trim(sets)
           if(sets(i:i).eq.',') then
            fields(nsets)=sets(is:i-1)
            nsets=nsets+1
            is=i+1  ! next field starts after this current character which is a comma
           endif
         enddo
         fields(nsets)=sets(is:len_trim(sets))
         
       endif
       identifier=line(19:ileftparen-1)
       lf=index(identifier,'_definition')
       if(lf.gt.0) then
         identifier(lf:)= ' '
       endif
! construct OML row name from the mask and set elements. If not an alias row, just use the elements as they are
       ecprownam=' '
       aimms_row_ID_num=0
       do ic=1, max_row_aimms
         if (identifier  .eq. row_aimms(ic).rownam_aimms ) then 
           aimms_row_ID_num=ic
           exit
         endif
       enddo
       if(aimms_row_id_num .eq.0) then
         write(6,*) 'aimms_row_id_num 0, line=',trim(line)
         return
       endif
 !============================================================================================

       lm=index(row_aimms(aimms_row_ID_num).rownam_mask, '!')  ! '!' is used to tag versions of the same mask, so use that as the length
       if(lm.gt.1) then
         lm=lm-1
       else
         lm=len_trim(row_aimms(aimms_row_ID_num).rownam_mask)
       endif
       j=0
       do i=1, lm
         if( index('()', row_aimms(aimms_row_ID_num).rownam_mask(i:i)) .eq.0) then  ! pass over any special masking characters in search of the constants
           j=j+1
           ecprownam(j:j)=row_aimms(aimms_row_ID_num).rownam_mask(i:i)         
         endif
       enddo
       rindstt(1:max_set)=row_aimms(aimms_row_ID_num).indstt(1:max_set)
       rindend(1:max_set)=row_aimms(aimms_row_ID_num).indend(1:max_set)
       do i=1,nsets
         afield=fields(i)  ! this field is a set element. 
           lf=index(row_aimms(aimms_row_ID_num).setnam(i),'_ALT')
           if(lf.gt.1) then  ! check for alias set name. if so, need to use primary set name to decode the element, then convert to the alternate alias element
           la=len_trim(afield)

           setname=row_aimms(aimms_row_ID_num).setnam(i)(1:lf-1)  ! strip off _ALT stuff to get AIMMS primary set name used in the composite table fields
           call get_ecp_set_element(setname,afield(:la),icode)         ! get the integer (ordinal) code for the set element 
           if(icode.gt.0) then
              call get_ecp_char_element(row_aimms(aimms_row_ID_num).setnam(i),icode,afield,ifound)
              la=len_trim(afield)
           endif
           if(icode.eq.0 .or. ifound.eq.0 .or. la.ne.(rindend(i)-rindstt(i)+1)) then
             write(6,'(3a)') 'AIMMS Interface error ECP with '//trim(identifier)// &
              ' solution. Could not get aliased set element for field: '//trim(fields(i))//' for set: '//trim(row_aimms(aimms_row_ID_num).setnam(i))//' and '//trim(setname)
             return
           endif
         endif
         if(rindstt(i).gt.0) then
           lf=rindend(i)-rindstt(i)+1
           la=len_trim(afield)
           if(len_trim(afield).lt.lf) then ! left pad integer set elements with 0
             afield=zeroes(1:lf-la)//afield
           endif  
           ecprownam(rindstt(i):rindend(i)) = afield(:lf)
         endif
       enddo
       return
       end subroutine get_oml_rownam
 end subroutine addomlecp

 subroutine addomlefd
 ! add oml names to end of cplex column and row numbers
 ! unit 8: lis file
 ! unit 9: output file with oml names added
 use efd_row_col
 implicit none
 character*100 line
 integer rectype ! 0: ignore, 1:column, 2:row 
 integer icol,irow
 integer ncol, nrow
! read aimefd.xlsx to get row/column names, masks, etc
   call efd_aimms_init
 ! read set names
   call read_efd_sets
  
 ncol=0
 nrow=0
 rectype=0
 10 continue
    read(8,'(a)',end=99) line
    if(index(line,'Column number').gt.0) then
      rectype=1
      write(9,'(a)') trim(line)
      goto 10
    endif
    if(index(line,'Row number').gt.0) then
      rectype=2
      write(9,'(a)') trim(line)
      goto 10
    endif

    if(rectype.eq.0) then
      write(9,'(a)') trim(line)
      goto 10
    elseif(rectype.eq.1 .and.len_trim(line).eq.0.and.ncol.gt.0) then
      write(9,'(a)') trim(line)
      rectype=0
      goto 10
    elseif(rectype.eq.2 .and.len_trim(line).eq.0.and.nrow.gt.0) then
      write(9,'(a)') trim(line)
      rectype=0
      goto 10
    elseif(len_trim(line).eq.0) then
      write(9,'(a)') trim(line)
      goto 10   
    endif
    
    if(rectype.eq.1) then
      efdcolnam=' '
      if(line(19:19).eq.'c') then
        call get_oml_colnam
        write(9,'(a,t60,a)') trim(line),efdcolnam
        read(line(1:10),'(i10)') icol
        efd_col_sol(icol).efdcolnam=efdcolnam
        efd_col_sol(icol).aimms_col_ID_num=aimms_col_ID_num
      elseif(line(19:19).eq.'r') then
        efdrownam=' '
        call get_oml_rownam
        efdcolnam=efdrownam ! free row entered as column/variable
        write(9,'(a,t60,a)') trim(line),efdcolnam
        read(line(1:10),'(i10)') icol
        efd_col_sol(icol).efdcolnam=efdcolnam
        efd_col_sol(icol).aimms_col_ID_num=0
      endif  
      ncol=ncol+1
    elseif(rectype.eq.2) then
      efdrownam=' '
      call get_oml_rownam
      write(9,'(a,t60,a)') trim(line),efdrownam
      read(line(1:10),'(i10)') irow
      efd_row_sol(irow).efdrownam=efdrownam
      efd_row_sol(irow).aimms_row_ID_num=aimms_row_ID_num
      nrow=nrow+1
    else
      write(9,'(a)') trim(line)
    endif

    
    goto 10
    
    99 continue
    
    return
    
  CONTAINS
 
  subroutine get_oml_colnam
  implicit none
  integer ileftparen,nsets,is,i,ic,j,lm,la,ifound,icode,lf
  character*25 sets
  character*30 setname
  character*5 fields(max_set),afield
  character*5 zeroes/'00000'/
  character*24 identifier
      fields=' '
      ileftparen=index(line,'(')
       if(ileftparen.eq.0) then
         ileftparen=len_trim(line)+1  ! pretend there is a ( at the end of the line
         nsets=0
       else
         sets=line(ileftparen+1:len_trim(line)-1)  ! sets is = to stuff between the (). if more than one set, then separated by comma
         nsets=1
         is=1
         do i=1,len_trim(sets)
           if(sets(i:i).eq.',') then
            fields(nsets)=sets(is:i-1)
            nsets=nsets+1
            is=i+1  ! next field starts after this current character which is a comma
           endif
         enddo
         fields(nsets)=sets(is:len_trim(sets))
         
       endif
       identifier=line(19:ileftparen-1)
! construct OML column name from the mask and set elements. If not an alias column, just use the elements as they are
       efdcolnam=' '
       aimms_col_ID_num=0
       do ic=1, max_col_aimms
         if (identifier  .eq. col_aimms(ic).colnam_aimms ) then 
           aimms_col_ID_num=ic
           exit
         endif
       enddo
       if(aimms_col_id_num .eq.0) then
         write(6,*) 'aimms_col_id_num 0, line=',trim(line)
         return
       endif
 !============================================================================================

       lm=index(col_aimms(aimms_col_ID_num).colnam_mask, '!')  ! '!' is used to tag versions of the same mask, so use that as the length
       if(lm.gt.1) then
         lm=lm-1
       else
         lm=len_trim(col_aimms(aimms_col_ID_num).colnam_mask)
       endif
       j=0
       do i=1, lm
         if( index('()', col_aimms(aimms_col_ID_num).colnam_mask(i:i)) .eq.0) then  ! pass over any special masking characters in search of the constants
           j=j+1
           efdcolnam(j:j)=col_aimms(aimms_col_ID_num).colnam_mask(i:i)         
         endif
       enddo
       cindstt(1:max_set)=col_aimms(aimms_col_ID_num).indstt(1:max_set)
       cindend(1:max_set)=col_aimms(aimms_col_ID_num).indend(1:max_set)
       do i=1,nsets
         afield=fields(i)  ! this field is a set element. 
           lf=index(col_aimms(aimms_col_ID_num).setnam(i),'_ALT')
           if(lf.gt.1) then  ! check for alias set name. if so, need to use primary set name to decode the element, then convert to the alternate alias element
           la=len_trim(afield)

           setname=col_aimms(aimms_col_ID_num).setnam(i)(1:lf-1)  ! strip off _ALT stuff to get AIMMS primary set name used in the composite table fields
           call get_efd_set_element(setname,afield(:la),icode)         ! get the integer (ordinal) code for the set element 
           if(icode.gt.0) then
              call get_efd_char_element(col_aimms(aimms_col_ID_num).setnam(i),icode,afield,ifound)
              la=len_trim(afield)
           endif
           if(icode.eq.0 .or. ifound.eq.0 .or. la.ne.(cindend(i)-cindstt(i)+1)) then
             write(6,'(3a)') 'AIMMS Interface error efd with '//trim(identifier)// &
              ' solution. Could not get aliased set element for field: '//trim(fields(i))//' for set: '//trim(col_aimms(aimms_col_ID_num).setnam(i))//' and '//trim(setname)
             return
           endif
         endif
         if(cindstt(i).gt.0) then
           lf=cindend(i)-cindstt(i)+1
           la=len_trim(afield)
           if(len_trim(afield).lt.lf) then ! left pad integer set elements with 0
             afield=zeroes(1:lf-la)//afield
           endif  
           efdcolnam(cindstt(i):cindend(i)) = afield(:lf)
         endif
       enddo
       return
       end subroutine get_oml_colnam
!================================================================================================================
  subroutine get_oml_rownam
  implicit none
  integer ileftparen,nsets,is,i,ic,j,lm,la,ifound,icode,lf
  character*25 sets
  character*30 setname
  character*5 fields(max_set),afield
  character*5 zeroes/'00000'/
  character*24 identifier
      fields=' '
      ileftparen=index(line,'(')
       if(ileftparen.eq.0) then
         ileftparen=len_trim(line)+1 ! pretend there is ( at the end of the line
         nsets=0
       else
         sets=line(ileftparen+1:len_trim(line)-1)  ! sets is = to stuff between the (). if more than one set, then separated by comma
         nsets=1
         is=1
         do i=1,len_trim(sets)
           if(sets(i:i).eq.',') then
            fields(nsets)=sets(is:i-1)
            nsets=nsets+1
            is=i+1  ! next field starts after this current character which is a comma
           endif
         enddo
         fields(nsets)=sets(is:len_trim(sets))
         
       endif
       identifier=line(19:ileftparen-1)
       lf=index(identifier,'_definition')
       if(lf.gt.0) then
         identifier(lf:)= ' '
       endif
! construct OML row name from the mask and set elements. If not an alias row, just use the elements as they are
       efdrownam=' '
       aimms_row_ID_num=0
       do ic=1, max_row_aimms
         if (identifier  .eq. row_aimms(ic).rownam_aimms ) then 
           aimms_row_ID_num=ic
           exit
         endif
       enddo
       if(aimms_row_id_num .eq.0) then
         write(6,*) 'aimms_row_id_num 0, line=',trim(line)
         return
       endif
 !============================================================================================

       lm=index(row_aimms(aimms_row_ID_num).rownam_mask, '!')  ! '!' is used to tag versions of the same mask, so use that as the length
       if(lm.gt.1) then
         lm=lm-1
       else
         lm=len_trim(row_aimms(aimms_row_ID_num).rownam_mask)
       endif
       j=0
       do i=1, lm
         if( index('()', row_aimms(aimms_row_ID_num).rownam_mask(i:i)) .eq.0) then  ! pass over any special masking characters in search of the constants
           j=j+1
           efdrownam(j:j)=row_aimms(aimms_row_ID_num).rownam_mask(i:i)         
         endif
       enddo
       rindstt(1:max_set)=row_aimms(aimms_row_ID_num).indstt(1:max_set)
       rindend(1:max_set)=row_aimms(aimms_row_ID_num).indend(1:max_set)
       do i=1,nsets
         afield=fields(i)  ! this field is a set element. 
           lf=index(row_aimms(aimms_row_ID_num).setnam(i),'_ALT')
           if(lf.gt.1) then  ! check for alias set name. if so, need to use primary set name to decode the element, then convert to the alternate alias element
           la=len_trim(afield)

           setname=row_aimms(aimms_row_ID_num).setnam(i)(1:lf-1)  ! strip off _ALT stuff to get AIMMS primary set name used in the composite table fields
           call get_efd_set_element(setname,afield(:la),icode)         ! get the integer (ordinal) code for the set element 
           if(icode.gt.0) then
              call get_efd_char_element(row_aimms(aimms_row_ID_num).setnam(i),icode,afield,ifound)
              la=len_trim(afield)
           endif
           if(icode.eq.0 .or. ifound.eq.0 .or. la.ne.(rindend(i)-rindstt(i)+1)) then
             write(6,'(3a)') 'AIMMS Interface error efd with '//trim(identifier)// &
              ' solution. Could not get aliased set element for field: '//trim(fields(i))//' for set: '//trim(row_aimms(aimms_row_ID_num).setnam(i))//' and '//trim(setname)
             return
           endif
         endif
         if(rindstt(i).gt.0) then
           lf=rindend(i)-rindstt(i)+1
           la=len_trim(afield)
           if(len_trim(afield).lt.lf) then ! left pad integer set elements with 0
             afield=zeroes(1:lf-la)//afield
           endif  
           efdrownam(rindstt(i):rindend(i)) = afield(:lf)
         endif
       enddo
       return
       end subroutine get_oml_rownam
 end subroutine addomlefd
!================================================================================================================


! for reading xlsx
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
      INTEGER RTOPTSV(MAXRTOPTS),OPT_USED(MAXRTOPTS),IFOUND,IRESULT
      LOGICAL READYET/.FALSE./
      LOGICAL NEW,FINDRTO
      EXTERNAL FILE_MGR
      COMMON /RTOOPTIONS/ RTOPTS,RTOPTSV,RESTOFLINE,NUMRTOPTS
      
      IF (IRESULT .EQ. 1) THEN 
         IRESULT = INDEX(RTONAME, "EPHRTS")
         RTONAME = "EPHRTS"
      ENDIF
      
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

!=====================================================================================================================================
  SUBROUTINE DWFSCOL(COLUMN,COLSOL,STAT,SOLVAL,colmask,RETCD)
  ! invokes OML function WFSCOL to retrieve column solution.
  ! For AIMMSEFD option, substitutes AIMMS solution for the column
    use efd_row_col  ! declarations/storage for AIMMS interface LP coefficients
    IMPLICIT NONE
    include 'omlall.fi'
    character*16 COLUMN
    character*8  COLSOL
    character*2  STAT,STAT2
    real(8)      SOLVAL(5),SOLVAL2(5)
    character*24 SOLVAL5(5)
    character(Len=*) :: colmask
    integer i,j,isol
    integer RETCD,RETCD2
    character*6  scodes/'ACLUD '/
    
    IF (AIMMSEFD .ne. 1 .or. make_efd_aimms) THEN  ! if running efd to generate data for arraycode_efd, will need to use OML solution
      stat=' '
      solval=0.
      RETCD = WFSCOL(COLUMN,COLSOL,STAT,SOLVAL)
    ENDIF 

    efdcolnam=column
    if(AIMMSEFD.eq.1) then

      if(make_efd_aimms) return ! there is no AIMMS solution if this is a make_efd_aimms run, so return

! look up OML col names in the AIMMS solution list, efd_col_sol.  
! use hash table for col names for fast lookup of solution record "isol"
! based on the col name.
      call usehash(efdcolnam, efd_col_name, max_efd_col_hash, isol, initial_store)
      solval(:)=0.    
      colnam_aimms=' '
      if (isol.ge.0 .and. .not. initial_store) then 
        do i=1,len_trim(colsol)                  ! for each non-blank character in COLSOL (eg., 'A  
          j=index(scodes,colsol(i:i))               ! get its position (1-5) in "ACLUD" to match the AIMMS solution field wanted                       
          solval(i)=efd_col_sol(isol).solval(j) 
        enddo
        retcd=0
        stat=efd_col_sol(isol).status
        aimms_col_ID_num=efd_col_sol(isol).aimms_col_ID_num
        if(aimms_col_ID_num.ge.0) colnam_aimms=col_aimms(aimms_col_ID_num).colnam_aimms
      else 
        if(isol.ge.0) then
          efd_col_name(isol)= ' '  ! initial_store was .true. re-initialize so repeated calls to solution retrieval generate initial_store=.true.
        endif
        stat=' '
        retcd=1
      endif
      stat2=' '
      solval2=0.
      solval5='       null'
      if(AIMEFDBG.eq.1) then
        RETCD2 = WFSCOL(COLUMN,COLSOL,STAT2,SOLVAL2)
        do i=1,len_trim(colsol)                  ! for each non-blank character in COLSOL (eg., 'A  
         j=index(scodes,colsol(i:i))              ! get its position (1-5) in "ACLUD" to match the 5 AIMMS solution fields              
         write(solval5(j),'(1PG24.15E3)') solval2(i)  
        enddo
      endif
  
      if(AIMEFDBG.eq.1) then
        if(retcd.eq.0 .or. retcd2.eq.0) then
          write(colunit,2000) column(1:8), colnam_aimms, retcd, retcd2, colsol, &
          stat, stat2, (efd_col_sol(isol).solval(j),j=1,5), (solval5(j),j=1,5)
        endif
!      else
!        if(retcd.eq.0) then
!          write(colunit,2001) column(1:8), colnam_aimms, retcd, &
!          colsol, stat, (efd_col_sol(isol).solval(j),j=1,5)
!        endif
      endif
 2000 format(a8,'|',a,'|',2(i2,'|'),a8,'|',a2,'|',a2,'|AIM:|',5(1PG24.15E3,'|'),'OML:|',5(a24,'|'))
 2001 format(a8,'|',a,'|',i2,'|',a8,'|',a2,5('|',1PG24.15E3))
  
   endif
!=============================================================================================================
  END SUBROUTINE DWFSCOL
!=====================================================================================================================================
  SUBROUTINE DWFSROW(ROW,ROWSOL,STAT,SOLVAL,rowmask,RETCD)
  ! invokes OML function WFSROW to retrieve row solution
  ! For AIMMSEFD option, substitutes AIMMS solution for the row 
    use efd_row_col  ! declarations/storage for AIMMS interface LP coefficients
    IMPLICIT NONE
    include 'omlall.fi'  
    character*16 ROW
    character*8  ROW8UPPER
    character*8  ROWSOL
    character*2  STAT,STAT2
    real(8)      SOLVAL(5),SOLVAL2(5)
    character*24 SOLVAL5(5)
    character(Len=*) :: rowmask
    integer i,j,isol
    integer RETCD,RETCD2
    character*6  scodes/'ASLUP '/
    
    IF (AIMMSEFD .ne. 1 .or. make_efd_aimms) THEN   ! if running efd to generate data for arraycode_efd, will need to use OML solution
      stat=' '
      solval=0.
      RETCD = WFSROW(ROW,ROWSOL,STAT,SOLVAL)
    endif
    efdrownam=row
    if(AIMMSEFD.eq.1) then

    if(make_efd_aimms) return ! there is no AIMMS solution if this is a make_efd_aimms run. so return

      ! look up OML row names in the AIMMS solution list, efd_row_sol. 
! use hash table for row names for fast lookup of solution record "isol"
! based on the row name.
      call usehash(efdrownam, efd_row_name, max_efd_row_hash, isol, initial_store)
      solval(:)=0.
      rownam_aimms=' '
      if(efd_row_sol(isol).efdrownam .ne. efdrownam) then
         efd_row_sol(isol).efdrownam=' '
         efd_row_sol(isol).row_type= ' '
         efd_row_sol(isol).solval=0
         efd_row_sol(isol).status=' '
         initial_store=.true.
      endif         
      
      if (isol.ge.0 .and. .not. initial_store) then                           
        do i=1,len_trim(rowsol)                  ! for each non-blank character in ROWSOL (eg., 'A  
          j=index(scodes,rowsol(i:i))               ! get its position (1-5) in "ACLUP" to match the AIMMS solution field wanted                       
          solval(i)=efd_row_sol(isol).solval(j)
          if(j.eq.5) then
            solval(i)=-solval(i)
          endif 
        enddo
        retcd=0
        stat=efd_row_sol(isol).status
        row_type=efd_row_sol(isol).row_type
        aimms_row_ID_num=efd_row_sol(isol).aimms_row_ID_num 
        if(aimms_row_ID_num.ge.0) then
          rownam_aimms=row_aimms(aimms_row_ID_num).rownam_aimms
          row_type=row_aimms(aimms_row_ID_num).row_type
        endif
      else
        if(isol.ge.0) then
          efd_row_name(isol)= ' '  ! initial_store true. re-initialize so repeated calls to solution retrieval generate initial_store
          efd_row_sol(isol).row_type=' '
          efd_row_sol(isol).efdrownam=' '          
        endif
        row_type=' '
        stat=' '
        retcd=1
      endif
      stat2=' '
      solval2=0.
      solval5='       null'
      if(AIMEFDBG.eq.1) then   ! then get OML solution for comparison      
        RETCD2 = WFSROW(ROW,ROWSOL,STAT2,SOLVAL2)
        do i=1,len_trim(rowsol)                  ! for each non-blank character in COLSOL (eg., 'A  
          j=index(scodes,rowsol(i:i))              ! get its position (1-5) in "ACLUD" to match the 5 AIMMS solution fields              
          write(solval5(j),'(1PG24.15E3)') solval2(i)  
        enddo
      endif

      if(AIMEFDBG.eq.1) then  ! write out comparison of AIMMS and OML solution fields
        if(retcd.eq.0 .or. retcd2.eq.0) then
          write(rowunit,2000) row(1:8), row_type, rownam_aimms, retcd, retcd2, rowsol, &
           stat, stat2, (efd_row_sol(isol).solval(j),j=1,5), (solval5(j),j=1,5)
        endif
!      else
!        if(retcd.eq.0) then
!          write(rowunit,2001) row(1:8), row_type, rownam_aimms, retcd, rowsol, stat, &
!          (efd_row_sol(isol).solval(j),j=1,5)
!        endif
      endif         
 2000 format(a8,'|',a1,'|',a,'|',2(i2,'|'),a8,'|',a2,'|',a2,'|AIM:|',5(1PG24.15E3,'|'),'|OML:|',5(a24,'|'))
 2001 format(a8,'|',a1,'|',a,'|',i2,'|',a8,'|',a2,5('|',1PG24.15E3))

    ENDIF
   END SUBROUTINE DWFSROW

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

 
 