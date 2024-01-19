! $Header: N:/default/scripts/RCS/makeit2025.f,v 1.2 2001/05/08 20:41:40 DSA Exp $
      PROGRAM Makeit2025
  use dflib

! reads a 2020 restart file, aeo2001 vintage, and writes a comparable restart file
! with the year range specified on the labels so it can be read by a 2025 version


!
! link /force makeit2025.obj filer.obj 
!
      implicit none
  include 'parametr'
  include 'ncntrl'
!
      INTEGER*4 FUNITI,FUNITO,FRETCD,FRTYPE,FSOURC,funfmt,stat,fmt, &
       infmt,I,L
      CHARACTER*60 dict,restarti,var1,var2,restarto,restart2
      CHARACTER*100 FNAMEI,FNAMEO
      character*30 nems
      logical lexist
 integer*2 narg
 narg=nargs()-1
 if(narg.ge.1) then
   call getarg(1,restarti)
 else
   write(6,*) ' no command line arguments given. provide input and output file names'
   stop
 endif
 if(narg.ge.2) then
   call getarg(2,restarto)
 else
   write(6,*) ' second argument, output file name, not given.'
   stop
 endif
 inquire(file=restarti,exist=lexist)
 if(.not.lexist) then
   write(6,*) trim(restarti)//' not found'
   stop
 endif
 call getenv('NEMS',nems) 
 call mlower(nems)
if(nems.eq.' ') then 
  nems='n:/default'
endif

dict=trim(nems)//'/input/dict.v1.16.1.1.txt'
infmt=1
var1=trim(nems)//'/scripts/varto2020.txt'
var2=trim(nems)//'/scripts/varcopy2020.txt'
fmt=1
      restart2=' '
      FRTYPE=3
      FRETCD=0
      FUNITI=1
      FNAMEI=' '
      open(1,file=dict,status='old',READONLY)
      FUNITO=2
      FNAMEO='dictout'
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD, &
       FUNFMT)
      close(2)
! READ RESTART FILE
! THIS TEST READS ALL OF THE DATA IN THE CURRENT DICTIONARY
      FRTYPE=2
      FSOURC=0

!**************
      FNAMEI=' '
      if(infmt.eq.0) then
        open(1,file=restarti,status='old')
        FUNFMT=0
      elseif(infmt.eq.1) then
        open(1,file=restarti,status='old',form='unformatted', &
         CONVERT='Big_Endian',recl=16384,access='sequential')
        FUNFMT=1   
      endif
      FNAMEO=' '
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD, &
       FUNFMT)

!
      lastyr=36

! IMPLEMENT OUTPUT REQUEST OF VARIABLES LISTED IN varlist FILE
      FRTYPE=1
      FSOURC=1
      FNAMEI=' '
      open(1,file=var1,status='old',readonly)
      FUNITI=1
      FUNITO=2
      FNAMEO=' '
      FUNFMT=fmt
      if(funfmt.eq.1) then
      open(2,file=restarto,form='unformatted',status='unknown', &
        CONVERT='Big_Endian')
        else
        open(2,file=restarto,form='formatted',status='unknown')
        endif
      rewind 2
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

      open(1,file=var2,status='old',readonly)
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      close(1)
      close(2) 
      STOP
      END
! ===============================================
      subroutine Mlower(a)
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
      end subroutine Mlower

