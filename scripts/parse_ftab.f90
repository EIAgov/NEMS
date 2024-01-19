! $Header: M:/default/scripts/RCS/parse_ftab.f90,v 1.3 2016/04/29 14:02:24 dsa Exp $
! Helps parse ftab. invoked from /scripts/parse_ftab.sh
implicit none
character*2600 line
character*30 var
integer is, ie, tablenum,rownum,nvars,nline/0/,i,j,istart,iend,ivars
integer nmax
parameter(nmax=100)
character*30 vars(nmax)


open(8,file='ftab.parse3.txt',status='old')  ! input
open(9,file='ftab.parse4.csv',status='unknown')  !output

do while (.not. eof(8))

    read(8,'(a)') line
    call upper(line)
    nline=nline+1
    if(line(1:3).eq.'IF(') then
      j=index(line,')T')
      if(j.gt.0) then
         line=line(j+1:)
      endif
    endif
    is=2
    ie=index(line,'=')
    tablenum=0
    if(ie.gt.is) then
      ie=index(line,'(')
      if(ie.gt.is) then
        var=line(is:ie-1)      
        read(var,'(i3)',err=25) tablenum
25    continue
        is=ie+1
      endif
    endif
    
    ie=index(line,',')
    rownum=0
    if(ie.gt.is) then
      var=line(is:ie-1)
      if(index(var,'+').eq.0) then
        read(var,'(i<len_trim(var)>)',err=26) rownum
26      continue
      endif
    endif
    
    if(tablenum.gt.0) then
      nvars=0
      is=index(line,'=')
      if(is.gt.0) then
        vars(1:nmax)=" "
        nvars=0
      !!!!!  line=line(is+1:)
        call getvars(line,vars,nmax,nvars,nline)
        if(nvars.eq.nmax) then
          write(6,*) ' nmax dimension may need increasing due to line',nline
        endif
        if(nvars.gt.0) then
          if(rownum.gt.0) then
             write(9,'(i3.3,a,i3.3,a,i2.2,a,<3*nmax>a)')      tablenum,'|',rownum,   '|',nvars,'|"'//trim(line)//'"',('|"',trim(vars(ivars)),'"',ivars=1,nvars)
          else
             call colon_range(var,istart,iend)
             if(istart.eq.0 .or. iend.eq.0) then
               write(9,'(i3.3,a,i3.3,a,i2.2,a,<3*nmax>a)')    tablenum,'|',rownum,   '|',nvars,'|"'//trim(line)//'"',('|"',trim(vars(ivars)),'"',ivars=1,nvars)
!              write(9,'(i3.3,a,   A,a,i2.2,a,<3*nmax>a)')    tablenum,'|',trim(var),'|',nvars,'|"'//trim(line)//'"',('|"',trim(vars(ivars)),'"',ivars=1,nvars)
             else
              ! write(6,'(a,i10)')  'colon expression '//trim(var)//' found on line',nline
               do i=istart,iend
                  rownum=i
                  write(9,'(i3.3,a,i3.3,a,i2.2,a,<3*nmax>a)') tablenum,'|',rownum,   '|',nvars,'|"'//trim(line)//'"',('|"',trim(vars(ivars)),'"',ivars=1,nvars)
               enddo
             endif
          endif
        endif
      endif
    endif
 enddo
 end
 !======================================================================
 subroutine getvars(iline,vars,nmax,nvars,nline)
 implicit none
 character*(*) iline
 character*2600 line
 integer nmax
 character*(*) vars(nmax)
 integer nvars,nest,i,is,ie,nline,nfound,itoken,j
 line=iline
 call upper(line)

 nest=0
 
 
 do i=1,len_trim(line)
   if(line(i:i).eq.'(') then
     nest=nest+1
   endif
   if(line(i:i).eq.')') then
     nest=nest-1
   endif
 enddo
 if(nest.ne.0) then
   write(6,*) 'unbalanced parentheses, line=',nline
 endif

nfound=1

do while (nfound.eq.1)
 nfound=0
 is=0
 ie=0
 do i=1,len_trim(line)
 ! find first non-blank, non-operator character--the start of a name
    itoken=index('()+-/*= ',line(i:i))
    if(itoken.eq.0) then
      is=i
      exit
    endif
 enddo
 if(is.gt.0) then
 ! find next operator character that delimits the name
   ie=0
   do i=is+1,len_trim(line)
     itoken=index('()+-/*=',line(i:i))
     if(itoken.gt.0) then
       ie=i-1
       exit
     endif
   enddo
   if(ie.eq.0) ie=len_trim(line)
! add name to list of variables blank out that portion of the line.   
   nfound=1
   j=index(line(is:ie),')(')
   if(j.gt.0) then
     line(j+1:ie)=' '
   endif
   if(line(is:ie).ne.'SUM' .and. line(is:ie).ne.'FSUM'.and.nvars.lt.nmax) then
     if(index(line(is:ie),',').gt.0 .and. nvars.gt.0) then
     !  names with "," are subscripts to prior name.  exception: two in a row
        if(index(vars(nvars),',').eq.0) then
          vars(nvars)=trim(vars(nvars))//'('//line(is:ie)//')'
        endif
     elseif(line(is:ie).eq.'IY'.and.ie.eq.is+1) then
     ! skip 
     else
       nvars=nvars+1
       vars(nvars)=line(is:ie)
     endif
   endif
   line(:ie)=' '
 endif
enddo

return
end
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

   
   subroutine colon_range(a,istart,iend)
   implicit none
   character*(*) a
   integer istart, iend,il, il1,il2,icolon
   character*10 field1,field2
   logical isinteger
   external isinteger
   
   istart=0
   iend=0
   
   icolon=index(a,':')
   
   if(icolon.gt.0) then
     il1=icolon-1
     
     il2=len_trim(a)-icolon
     
     field1=a(1:il1)
     field2=a(icolon+1:icolon+il2)
   
     if( isinteger(field1) .and. isinteger(field2) ) then

       read(field1,*) istart
       read(field2,*) iend
       !write(6,'(a,a1,a,a1,a,2i10)') trim(a),',',trim(field1),',',trim(field2), istart,iend

     endif
     
  endif
  end
  
  function isnumber(a)
  implicit none
  logical isnumber
  character*(*) a
  character*14 numchars/' 0123456789.,-'/
  integer L,i
  l=len_trim(a)
  isnumber=.false.
  
  if(l.gt.0) then
     isnumber=.true.
     do i=1,l
      !  if any of the characters is not something that should be part of a decimal number input field, then it is not a number
       if(index(numchars,a(i:i)).eq.0) then
          isnumber=.false.
          exit
       endif
    enddo
  endif
  end
  
  function isinteger(a)
  implicit none
  logical isinteger
  character*(*) a

  character*12 numchars/' 0123456789-'/
  integer L,i
  l=len_trim(a)
  isinteger=.false.
  
  if(l.gt.0) then
     isinteger=.true.
     do i=1,l
     ! if any of the characters is not something that should be part of an integer input field, then it is not a integer
       if(index(numchars,a(i:i)).eq.0) then
          isinteger=.false.
          exit
       endif
    enddo
  endif
  end
    
   
   
   
 
    