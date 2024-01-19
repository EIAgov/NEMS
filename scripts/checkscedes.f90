program checkscedes
implicit none

character*127 line, newline
character*127 scedes
character*127 nemval
logical lexist
integer*2 narg,l,ieq,isl
integer countm, countf, pos
narg=nargs()
narg=narg-1
if(narg.eq.0) then
  write(6,'(a\)') ' enter the full name or suffix of the scedes file: '
  read(5,'(a)') scedes
else
  call getarg(1,scedes)
endif             
inquire(file=scedes,exist=lexist)
line=scedes
if(.not. lexist) then
  scedes='scedes.'//scedes
  inquire(file=scedes,exist=lexist)
  if(.not.lexist) then
   write(6,*) 'scedes file "'//trim(line)//'" not found'
   stop
  endif
endif

open(8,file=scedes,status='old',readonly)
countf=0
countm=0
 ! substitute $NEMS with actual environment value
   CALL GETENV( 'NEMS',nemval )
 
do while (.not. eof(8))
  read(8,'(a)') line
 
!substitute nemval for $NEMS in the line
pos=index(line,'$NEMS')
if (pos>0) then
  newline=line(1:pos-1)//trim(nemval)//line(pos+5:)
  line=newline
else 
  pos=index(line,'$nems')
  if (pos>0) then
     newline=line(1:pos-1)//trim(nemval)//line(pos+5:)
     line=newline
  endif
endif

  if(index(line,'hostpc=').gt.0) write(6,'(a)') trim(line)
  if(index(line,'HOSTPC=').gt.0) write(6,'(a)') trim(line)
  ieq=index(line,'=')
  if(ieq.ne.0) then
    line=line(ieq+1:)
    isl=index(line,'/')
    if(isl.ne.0) then
      countf=countf+1
      inquire(file=line,exist=lexist)
      if(.not.lexist) then
         line=trim(line)//'.gz'
         inquire(file=line,exist=lexist)
         if(.not.lexist) then
            l=len_trim(line)
            l=l-2
            line(l:)=' '
            line=trim(line)//'.obj'
            inquire(file=line,exist=lexist)
            if(.not.lexist) then
              l=len_trim(line)-3
              line(l:)=' '
              write(6,'(a)') 'not found: '//trim(line)
              countm=countm+1
            endif
         endif
      endif
    endif
  endif
enddo
write(6,'(i3,a,i2)') countf,' files indentified in '//trim(scedes)//' and number missing=',countm
end