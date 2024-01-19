      use ifport, only:getenv,getdrivedirqq,file$curdrive
      implicit none
      character*80 cmd
      integer iret
      integer*2 system
      character*10 scenario
      character*8 datecode
      character*5 user
      character*50 dir
      character*100 runname,filen*80,restart(100),ext*80,cycleno(100)*2,iterno(100)*2
      character*80 ftabdat(100)
      character*50 runlist,runlistdef,runs(3,100)*50
      character*25 commands
      character*128 runline,line
      character*128 inddir
      character*256 ldir

      
      integer nruns,i,i1,j,nftab,leng,ib
      external leng
      logical lexist
      character*12 ttoday 
      character*5 argument/'0    '/
      integer*4 nargument,iterdump
      integer iyr,imon,iday
      cycleno=' '
      iterno=' '
      ldir = FILE$CURDRIVE
      iret = GETDRIVEDIRQQ(ldir)
      
       nargument=nargs()
       iterdump=0
       if(nargument.ge.2) then
         call getarg(1,argument)
         read(argument,'(bn,i5)',err=9,end=9) iterdump   ! if 1, process iterdump files
      9  continue
       endif

    if(iterdump.eq.0) then

      
      call getdat(iyr,imon,iday)
      user=' '
      call getenv("USER",USER)
      ttoday=' '
      !write(ttoday,'(i2.2,a,i2.2,a,i4)') imon,'/',iday,'/',iyr
      !cmd='echo '//trim(USER)//' '//ttoday//' >> m:/default/logs/muchoftab.log'
!      write(6,*) 'cmd: ',cmd
     ! iret=system(cmd)

      write(6,*) '        This is the MuchoFtab Program'
      write(6,*) '               '
      write(6,*) ' This reads a list of runs, reads an ftab.dat file, and'
      write(6,*) ' then creates a bunch of similar ftab.dat files with'
      write(6,*) ' the run names inserted.  The ftab.dat files created'
      write(6,*) ' are named with the appendix ".scenario.datecode"'
      write(6,*) ' Finally, it creates a list of ftab commands in ftab.sh'
      write(6,*) ' which you execute.  After running this, you will have'
      write(6,*) ' a bunch of .wk1 or .ran files from ftab.  '
      write(6,*) ' You can use MY run list: L:/main/dsa/runs.dat or name'
      write(6,*) ' your own version.  If you have your own version, make'
      write(6,*) ' sure its format is the same as my version.'
      runlistdef='runs.dat'
      inquire(file=runlistdef,exist=lexist)
      if(.not. lexist) then
         runlistdef='l:/main/dsa/runs.dat'
      endif
      inquire(file=runlistdef,exist=lexist)     
      if(.not. lexist) then
         runlistdef='nodefault list'
      endif   
10    write(6,'(a,$)') 'Enter the name of the file '// &
       'with the run ids ['//trim(runlistdef)//'] :'
      read(5,'(a)') runlist
      if(runlist.eq.' ') then
        runlist=runlistdef
      endif
11    continue
      inquire(file=runlist,exist=lexist)
      if(.not.lexist) then
       write(6,*) 'whoops.  '//runlist(:leng(runlist))//' does not exist!'
       goto 10
      endif
      open(8,file=runlist,status='old')

      j=1
20    read(8,'(a)',end=21) runline
      i=index(runline,' ')
      runs(1,j)=adjustl(runline(1:i))
      runline(1:i)=' '
      runline=adjustl(runline)

      i=index(runline,' ')
      runs(3,j)=adjustl(runline(1:i))
      runline(1:i)=' '
      runline=adjustl(runline)

      runs(2,j)=trim(runline)

      write(6,*) 'run ',j,' ',trim(runs(1,j)),' ',trim(runs(2,j)),' ',trim(runs(3,j))
      j=j+1
      if(j.lt.100) goto 20
      goto 20
21    j=j-1
      nruns=j
      close(8)


      i=0
      do j=1,nruns
        filen=' ' &
            //runs(2,j)(:leng(runs(2,j)))//'/' &
            //runs(1,j)(:leng(runs(1,j)))//'/' &
            //runs(3,j)(:leng(runs(3,j)))//'/' &
            //'RESTART.unf'
        inddir=runs(2,j)(:leng(runs(2,j)))//'/' &
             //runs(1,j)(:leng(runs(1,j)))//'/' &
             //runs(3,j)(:leng(runs(3,j)))//'/input/' 
        inquire(file=filen,exist=lexist)
        if(lexist) then
           i=i+1
           restart(i)=filen
        else
          write(6,*) 'whoops. '//filen(:leng(filen))//' does not exist!'
        endif
        nruns=i
      enddo
    else
      nruns=0
      write(cmd,'(a,i2.2,a)') 'DIR /B restart_',iterdump,'_??.unf > restartfiles.txt'
      iret=system(cmd)
      if(iret.ne.0) stop ' '
      inquire(file='restartfiles.txt',exist=lexist)
      if(lexist) then
         open(10,file='restartfiles.txt',status='old',readonly) 
         j=0
         do while (.not. eof(10))
           j=j+1
           read(10,'(a)') restart(j)
           if(restart(j)(8:8).eq.'_') then
              cycleno(j)=restart(j)(9:10)
           endif
           if(restart(j)(11:11).eq.'_') then
              iterno(j)=restart(j)(12:13)
           endif
           restart(j)=trim(ldir)//'\'//restart(j)
           if(len_trim(restart(j)).eq.0) then
             j=j-1
           endif
         
         end do
         nruns=j
      else
         write(6,'(a,i2.2)') ' no iterdump restart files found for cycle',iterdump
      endif

    endif


      filen='ftab.dat'
      goto 31
30    write(*,'(a\)')'Enter the name of the "ftab.dat" generic file:'
      read(5,'(a)') filen
31    inquire(file=filen,exist=lexist)
      if(.not.lexist) then
       write(6,*) 'whoops. '//filen(:leng(filen))//' does not exist!'
       goto 30
      endif

      open(8,file=filen,status='old')
      j=1
40    read(8,'(a)',end=41) ftabdat(j)
      j=j+1
      if(j.lt.100) goto 40
      stop
41    j=j-1
      nftab=j
      close(8)

      open(11,file='ftab.sh',status='replace')
      do j=1,nruns
        filen=restart(j)
        ib=scan(filen,'/\',.true.)
        if(ib.gt.0) then
          inddir=restart(j)(:ib)//'input\'
          line=filen(:ib-1)
          ib=scan(filen,'\/',.true.)
          filen(ib:)=' '
          ib=scan(filen,'\/',.true.)  
          if((filen(ib+1:ib+1).eq.'p' .or. filen(ib+1:ib+1).eq.'P') .and. scan(filen(ib+2:ib+2),'0123456789').gt.0 .and. filen(ib+3:ib+3).eq.' ' ) then
             filen(ib:)=' '
             ib=scan(filen,'\/',.true.)  
          endif
          runs(3,j)=filen(ib+1:)
          filen(ib:)=' '
          ib=scan(filen,'\/',.true.)
          runs(1,j)=filen(ib+1:)
          runs(2,j)=filen(:ib-1)
       
        else
          inddir='./input'
        endif
        if(iterdump.eq.0) then
          filen='ftab.dat.'//runs(1,j)(:leng(runs(1,j)))//'.'//runs(3,j)
        else
          filen='ftab.dat.'
          ib=index(restart(j),'restart')
          if(ib.eq.0) ib=index(restart(j),'RESTART')
           
          if(ib.gt.0) then
             ib=ib+7
             ext=restart(j)(ib:)
             ib=scan(ext,'.')
             ext(ib:)=' '
          endif
          filen='ftab.dat'//ext
         
        endif


          write(11,'(a)') 'ftab.exe <'//filen(:leng(filen))
          open(9,file=filen,status='replace')
          do i=1,nftab
            if(i.eq.1) then
                 write(9,'(a)') trim(runs(1,j))//trim(cycleno(j))//trim(iterno(j))
            elseif(i.eq.2) then
              write(9,'(a)') trim(runs(3,j))
            elseif(i.eq.3) then
              write(9,'(a)') trim(runs(2,j))
            elseif(i.eq.19) then
              if(iterdump.eq.0) then
                filen=runs(2,j)(:leng(runs(2,j)))//'/' &
                //runs(1,j)(:leng(runs(1,j)))//'/' &
                //runs(3,j)(:leng(runs(3,j)))//'/' &
                //'RESTART.unf'
              else
                filen=' '//restart(j)
              endif
              write(9,'(a)') trim(filen(2:))
            elseif(i.eq.14 .and. iterdump.gt.0) then ! turn off excel output for iterdump...only need RANs
              write(9,'(a)') '0          WK1 FILE SWITCH (1=CALL FWK1 TO CREATE IT)'
            elseif(index(ftabdat(i),'./input').gt.1) then

              write(9,'(3a)') ftabdat(i)(:index(ftabdat(i),'./input/')-1),trim(inddir), &
                             ftabdat(i)(index(ftabdat(i),'./input/')+8:)
              write(6,'(3a)') ftabdat(i)(:index(ftabdat(i),'./input/')-1),trim(inddir), &
                             ftabdat(i)(index(ftabdat(i),'./input/')+8:)
            elseif(index(ftabdat(i),'./input').eq.1) then
              write(9,'(3a)') trim(inddir), &
                             ftabdat(i)(index(ftabdat(i),'./input/')+8:)
              write(6,'(3a)') trim(inddir), &
                             ftabdat(i)(index(ftabdat(i),'./input/')+8:)
            else
              write(9,'(a)') ftabdat(i)(:leng(ftabdat(i)))
            endif
          enddo

        close(9)
      enddo
      close(11,status='keep')
      inquire(file='ftab.exe',exist=lexist)
      if(.not.lexist) then
         write(6,*) ' you need a copy of the ftab executable'
         write(6,*) ' you can copy it from the output dir of'
         write(6,*) ' any of todays runs.  when you have it,'
      endif
      write(6,*)' do this:     . ftab.sh'
      stop ' '
      end
      FUNCTION LENG(A)
      IMPLICIT NONE
!     FINDS POSITION OF LAST NON-BLANK CHARACTER IN CHARACTER VARIABLE A
      CHARACTER A*(*)
      INTEGER*4 LENG,I,N
      N=LEN(A)
      LENG=0
      DO 10 I=N,1,-1
        IF(A(I:I).NE.' ') THEN
          LENG=I
          RETURN
        ENDIF
   10 CONTINUE
      RETURN
      END
      subroutine getdat( iyr,imon,iday)
      integer iyr,imon,iday
      integer values(8)
      character zone*5,date*8,time*10
      call date_and_time(date,time,zone,values)
      iyr=values(1)
      imon=values(2)
      iday=values(3)
      return
      end


