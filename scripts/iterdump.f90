program iterdump
implicit none


character*80 line,fname,rname
character*166 cmd
character*80 dellist(1990:2050,15)
logical lexist
integer maxcycle,maxiter(15),maxyear(15),maxitryr(1990:2050,15)

integer cycle,iter,year,useiter,lastiter,iret,allret,ifirst

integer LN

maxcycle=0
maxiter=0
maxyear=0
maxitryr(:,:)=0

open(8,file='restartfiles.txt',status='old',readonly)

do while (.not. eof(8))
   read(8,'(a)') line
   read(line,'(a)')    ! lines ave restart names prestent, like "restart_01_02_2040.unf"
   LN=len_trim(line)
   
   if (LN .eq. 22) then
     if(line(1:8).eq.'restart_') then
       read(line,'(8x,i2,1x,i2,1x,i4)') cycle,iter,year
       if(cycle.gt.maxcycle) maxcycle=cycle
       if(iter.gt.maxiter(cycle)) maxiter(cycle)=iter
       if(year.gt.maxyear(cycle)) maxyear(cycle)=year
       if(iter.gt.maxitryr(year,cycle)) maxitryr(year,cycle)=iter
     endif
   endif
enddo

! create a tfiler.files for each cycle

do cycle=1,maxcycle
   if(maxiter(cycle).gt.0) then
    rname='input/restarti.unf'
    inquire(file=rname,exist=lexist)
    if(.not. lexist) then
      rname='input/restarti.rlx'
      inquire(file=rname,exist=lexist)
    endif
    if(lexist) then
      rname='Restarti   '//trim(rname)
     
      dellist=' '  
      allret=0 
      do iter=1,maxiter(cycle)
     
       
        write(fname,'(a,i2.2,a,i2.2)') 'tfiler.files.c',cycle,'.i',iter
        open(9,file=fname,status='unknown')
        rewind 9
 
        write(9,'(a)')  'Dictionary input/dict.txt' 
        write(9,'(a)')  trim(rname)
        write(9,'(a)')  'In Format  1  Unformatted'
        ! if this is a p2 run, there will be a copy of the full varlist.txt file in varlistall.txt. use that so the concatenated restart file
        ! will be a complete copy that can produce ftab output.
        inquire(file='input/varlistall.txt',exist=lexist)
        if(lexist) then
!          for p2 run
          write(9,'(a)')  'Varlist    input/varlistall.txt'
        else
!          for most runs
          write(9,'(a)')  'Varlist    input/varlist.txt'
        endif
        write(9,'(a,i2.2,a,i2.2,a)')  'Restarto   restart_',cycle,'_',iter,'.unf'
        write(9,'(a)')  'Out Format 1 Unformatted'
        write(9,'(a)')  'Filer Obj  filer.obj'
        write(9,'(a)')  'comment line '
        write(9,'(a)')  '=============================================='
        
     
        ifirst=0
        do year=1990,maxyear(cycle)
        
           lastiter=maxitryr(year,cycle)     
           useiter=iter
           if(iter.gt.lastiter) useiter=lastiter
           if(useiter.gt.0) then
             write(dellist(year,iter),'(a,i2.2,a,i2.2,a,i4,a)')  'restart_',cycle,'_',useiter,'_',year,'.unf'
             write(9,'(a,i2.2,a,i2.2,a,i4,a)')  'restart_',cycle,'_',useiter,'_',year,'.unf'
             if(ifirst.eq.0) then
               write(9,*) ! skip a line for output dictionary
               ifirst=1
             endif
           endif          
        enddo
        close(9)
! run tfiler to concatenate the single-year restart files for this iteration

        cmd='tfiler.exe 0 '//trim(fname)
        call callsys(iret,cmd)
        allret=allret+iret
      enddo
      if(allret.eq.0) then   ! delete the single-year restart files
        do year=1990,maxyear(cycle)
          do iter=1,maxiter(cycle)
             fname=dellist(year,iter)
             inquire(file=fname,exist=lexist)
             if(lexist) then
               open(10,file=fname,status='old')
               close(10,status='delete')
             endif
          enddo
        enddo
        
      endif
    endif  
   endif
enddo
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
