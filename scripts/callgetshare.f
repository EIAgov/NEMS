      program callgetshare
!  determines if the current working directory (or optional file name argument)
!  is located on a shared drive on the current computer.
!  If it is, it returns the name of the directory or file as a fully quallified
!  net work file name, such as \\dlra\sharename\filename
!  if not, then check to see if the drive is a net work drive. if it is, substitute
!  the network drive name 
      use dflib
      use dfport
      implicit none

      character*5 queue,QueID*4
      common/q/queue,queid

      character*100 cmd,line*200
      character*80 arg1,pwd,oldpwd
      character*3 drive,ext*3,fname*35
      character*7 pcid 
      integer iret,imatch,lmatch,lp
      character*31 shares(100),shpath(100)*31
          integer nshares,i
      integer(4) istat
      CHARACTER($MAXPATH) dir,filen,pathbuf
          character*30 username,hostname
      INTEGER(4) length!  Get current directory
      integer(2) iarg,narg
       
      queue=' '
      queid=' '
      dir = FILE$CURDRIVE           ! get current drive
      length = GETDRIVEDIRQQ(dir)   ! get current directory
      IF (length .GT. 0) THEN
      !  WRITE (*,*) 'GETDRIVEDIRQQ: Current directory is:', &
      ! dir(:len_trim(dir))
      ELSE
        WRITE (*,*) 'Failed to get current directory'
      ENDIF
      pwd=dir
      filen=pwd

      IRET = HOSTNAM (pcid)
!      write(*,*) 'HOSTNAM:',pcid
      call GETLOG(username)
!      write(*,*) 'LOGNAME:',username

! check for optional file name argument
      narg=nargs() ! 
     ! write(*,*) 'number of arguments',narg
      do iarg=1,narg-1
        call GETARG(iarg,cmd)
        iret=FULLPATHQQ(cmd,filen)
     !   write(*,'(1x,i3,a,a,1x,a)') iarg,') ',trim(cmd),trim(filen)
      enddo
      call mlower(filen)
      call mreplace(filen,'\','/')
      call getshare(filen,1)
   !   write(6,'(a)') trim(filen)
      call getshare(filen,1)
      write(6,'(a)') trim(filen)

      stop
      end
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
! ===============================================
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
!========================================================
