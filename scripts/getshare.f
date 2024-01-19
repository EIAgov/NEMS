 subroutine getshare(filen,option)
!  tries to convert between DRIVE:PATH name to UNC naming based
! on mapped network drives in use and network shares assigned.
!  OPTION=1 tries to convert to UNC (//node/...)
!  OPTION=2 tries to convert to DRIVE:PATH
!
!
      use dflib
      use dfport
      implicit none
      character*(*) filen
      character*100 cmd,line*200,H*70,dum*80
      character*3 drive,ext*3,fname*35

      character*8 pcid 
      integer iret,imatch,lmatch,lp,lpf,lc,ls
      character*31 shares(100),shpath(100)*31
          integer nshares,i
      character*31 shrname
      character*4 drives(26)
      integer(4) istat, option
      character*100 cfile
      logical lexist
      
      character*5 queue,QueID*4
      common/q/queue,queid

      call Mlower(filen)
      call Mreplace(filen,'\','/')

      
      IRET = HOSTNAM (pcid) ! portability routine
      !write(*,*) 'HOSTNAM:',pcid
      call Mlower(pcid)
      
      if(len_trim(queue).eq.0.or.ichar(queue(1:1)).eq.0) queue='small'
      if(len_trim(queid).eq.0.or.ichar(queid(1:1)).eq.0) queid='dum0'
      cfile=trim(queue)//'.'//trim(queid)//'.out'
      
! get list of all drives letters
! may not be needed  
      ! cmd ='fsutil fsinfo drives > '//trim(cfile)
      ! iret=SYSTEM(cmd)

      ! open(8,file=cfile,status='old')
      ! read(8,*)
      ! read(8,'(a)') line
      ! if(line(1:8).eq.'Drives: ') line=line(9:)
      ! call Mlower(line)
      !! write(*,*)' '
      !! write(*,*)'List of drives'
      !! write(*,*) trim(line)           
      ! read(line,'(26(a2,2x))') drives(1:26)
      ! close(8,status='delete')
       
! run "net use" command to generate
! a list of network drives
      !write(*,*)' '
      !write(*,*)'List of network drives'

      cmd='net use > '//trim(cfile)
      iret=SYSTEM(cmd)

      open(8,file=cfile,status='old',err=92)

      nshares=1
      do while (.not. eof(8))
         read(8,'(a)',err=97) line
         lc=index(line,':')
         if(lc.gt.0) then
           if(lc.gt.2) line=line(lc-1:)
           read(line,'(a9,1x,a24)',err=97) shpath(nshares), shares(nshares)
           if(index(shares(nshares),':').eq.0) then
             call Mlower(shpath(nshares))
             call Mlower(shares(nshares))
             call Mreplace(shares(nshares),'\','/')
             call Mreplace(shares(nshares),')',' ')
             !write(*,'(a,1x,a)') trim(shpath(nshares)),trim(shares(nshares))
             nshares=nshares+1
           endif
         endif
97       continue
      enddo
      close(8,status='delete') 

      !write(*,*)' '
      !write(*,*)'List of network shares on '//trim(pcid)
       
! get list of file shares on this PC
      h='HKEY_LOCAL_MACHINE\SYSTEM\ControlSet001\Services\LanmanServer\Shares'
      cmd='reg query '//trim(h)//' > '//trim(cfile)
      iret=SYSTEM(cmd)
      open(8,file=cfile,status='old',err=92)

      do while (.not. eof(8))
         read(8,'(a)') line
         lc=index(line,'Path=')
         if(lc.gt.0) then

! share name is first field, space delimited           
           line=adjustl(line)
           ls=index(line,' ')
           shrname=line(1:ls)

! path is found after string "Path=", delimited by "\0"            
           lc=index(line,'Path=')
           line=line(lc+5:)
       
           lc=index(line,'\0')
           if(lc.gt.0) line(lc:)=' '
           call mlower(line)
           call mreplace(line,'\','/')
           nshares=nshares+1

           shpath(nshares)=trim(line)
           shares(nshares)='//'//trim(pcid)//'/'//trim(shrname)
           !write(*,*) trim(shpath(nshares)),' ',trim(shares(nshares))
         endif
       end do
       close(8,status='delete')       
! substitute UNC name for drive letter/path prefix if possible
        if(nshares.ge.1) then
          imatch=0
          lmatch=0
          lpf=len_trim(filen)      
          do i=1,nshares

            if(filen(lpf:lpf).eq.'/') then
              lp=len_trim(shpath(i))
              if(shpath(i)(lp:lp).ne.'/') then
                shpath(i)(lp+1:)='/'
              endif
            else
              lp=len_trim(shpath(i))
              if(shpath(i)(lp:lp).eq.'/') then
                shpath(i)(lp:lp)=' '
              endif
            endif
      
            if(option.eq.1) then    
              if(index(filen,trim(shpath(i))).eq.1.and.len_trim(shpath(i)).gt.lmatch) then
                lmatch=len_trim(shpath(i))
                imatch=i
              endif
            else
              if(index(filen,trim(shares(i))).eq.1.and.len_trim(shpath(i)).gt.lmatch) then
                lmatch=len_trim(shpath(i))
                imatch=i
              endif
            endif
          enddo 
          if(imatch.ne.0) then
            if(option.eq.1) then
              call Mreplace(filen,trim(shpath(imatch)),trim(shares(imatch)))
              call Mreplace(filen(3:),'//','/')
             else
              call slashslash(shares(imatch),shpath(imatch))
              call Mreplace(filen,trim(shares(imatch)),trim(shpath(imatch)))
              call Mreplace(filen(3:),'//','/')
            endif
          endif
        endif
        return 
92    continue
      end subroutine getshare
      subroutine slashslash(a,b)
      implicit none
      character*(*)a
      character*(*)b
      integer la,lb
      la=len_trim(a)
      lb=len_trim(b)
      if(a(la:la).eq.'/'.and.b(lb:lb).ne.'/') then
        b(lb+1:lb+1)='/'
      elseif(a(la:la).ne.'/'.and.b(lb:lb).eq.'/') then
        a(la+1:la+1)='/'
      endif
      return
      end


