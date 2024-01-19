! $Header: m:/default/source/RCS/nemswk1.f,v 1.54 2019/10/01 20:31:41 pkc Exp $
      subroutine wk1init
      implicit none
! this routine reads a file containing various wk1 codes for
! various record types.  It must be called before the other routines
! here get called.  next open the file. then call readrng to read the
! spreadsheet.  then call getrngi and getrngr to move the data from
! ranges to arrays in your code.
      include 'wk1block'

      integer i
      LOGICAL NEW
      CHARACTER*18 FNAME
      integer FILE_MGR
      EXTERNAL FILE_MGR
      integer funit


!  open worksheet record-codes file using file manager; read it in.
      NEW=.FALSE.
      FNAME='CODES123'
      funit = FILE_MGR('O',FNAME,NEW)

5     read(funit,'(i3,1x,a9,1x,a26)',end=6) i,record(i),recnam(i)
      goto 5
6     continue

!  close file
      funit = FILE_MGR('C',FNAME,NEW)

      return
      end

! -----------------------------------------------------------
      subroutine getrngi(namedin,Vname,ix,iy,iz)
      implicit none
! loads integer*2 array Vname from spreadsheet range NAMEDIN with ix rows and iy columns and iz groups

      include 'wk1block'
      common/nemswk1/xmlout
      integer xmlout
      integer*4 n,ix,iy,iz
      integer*4 x,y,z
      integer*4 i,j,k
      integer*4 dimflag   ! set based upon range/variable dimension conflict
      integer*4 ifindrng  ! function to find range number
      integer*4 isetdflg  ! function to set the dimension conflict variable
      integer*2 Vname(ix,iy,iz)
      character*16 namedin
      character*16 named

      named=namedin
!  make sure range name is in upper case
      n=len_trim(named) ! len_trim is in filer.f and is the same as leng in other places
      call low2up(named,N)

!  determine if range was input; search the range names vector for (named)
      irng = ifindrng(named)
      if (irng.lt.0) goto 990   ! range not found

!  dimflag indicates the dimension conflicts that may exist between the
!  defined range dimensions and the user variable dimensions
      dimflag = isetdflg(irng,ix,iy,iz)

!  determine looping variables needed to assign user variable based upon
!  the dimflag.
      call fsetxyz(irng,ix,iy,iz,x,y,z,dimflag)

!  begin loop to assign user specified variable
      ival = rngloc(irng)
      do k = 1,z
         do i = 1,x
            do j = 1,y
               Vname(i,j,k) = inint(rngval(ival))
               ival = ival + 1
            enddo
         enddo
      enddo
      if(xmlout.gt.0) then
        write(xmlout,'(a)')'========================================================='
        do k=1,z
          do i=1,x
             write(xmlout,'(a,2i4, <y>I10.3)') trim(namedin),k,i,(vname(i,j,k),j=1,y)
          enddo
        enddo
      endif
990   continue

      return
      end
! -----------------------------------------------------------
      subroutine getrngi4(namedin,Vname,ix,iy,iz)
      implicit none
! loads integer*4 array Vname from spreadsheet range NAMEDIN with ix rows and iy columns and iz groups
      include 'wk1block'
      common/nemswk1/xmlout
      integer xmlout
      integer*4 n,ix,iy,iz
      integer*4 x,y,z
      integer*4 i,j,k
      integer*4 dimflag   ! set based upon range/variable dimension conflict
      integer*4 ifindrng  ! function to find range number
      integer*4 isetdflg  ! function to set the dimension conflict variable
      integer*4 Vname(ix,iy,iz)
      character*16 namedin
      character*16 named

      named=namedin
!  make sure range name is in upper case
      n=len_trim(named) ! len_trim is in filer.f and is the same as leng in other places
      call low2up(named,N)

!  determine if range was input; search the range names vector for (named)
      irng = ifindrng(named)
      if (irng.lt.0) goto 990   ! range not found

!  dimflag indicates the dimension conflicts that may exist between the
!  defined range dimensions and the user variable dimensions
      dimflag = isetdflg(irng,ix,iy,iz)

!  determine looping variables needed to assign user variable based upon the dimflag.
      call fsetxyz(irng,ix,iy,iz,x,y,z,dimflag)

!  begin loop to assign user specified variable
      ival = rngloc(irng)
      do k = 1,z
         do i = 1,x
            do j = 1,y
               Vname(i,j,k) = nint(rngval(ival))
               ival = ival + 1
            enddo
         enddo
      enddo
      if(xmlout.gt.0) then
        write(xmlout,'(a)')'========================================================='
        do k=1,z
          do i=1,x
             write(xmlout,'(a,2i4, <y>I10.3)') trim(namedin),k,i,(vname(i,j,k),j=1,y)
          enddo
        enddo
      endif
990   continue

      return
      end

! -----------------------------------------------------------
      subroutine getrngr(namedin,Vname,ix,iy,iz)
      implicit none
! loads real*4 array Vname from spreadsheet range NAMEDIN with ix rows and iy columns and iz groups

      include 'wk1block'
      common/nemswk1/xmlout
      integer xmlout
      integer*4 n,ix,iy,iz
      integer*4 x,y,z
      integer*4 i,j,k
      integer*4 dimflag   ! set based upon range/variable dimension conflict
      integer*4 igetname  ! function to get range identifier number irng
      integer*4 len_trim     ! function to compute string length
      integer*4 ifindrng  ! function to find range number
      integer*4 isetdflg  ! function to set the dimension conflict variable
      real*4 Vname(ix,iy,iz)
      character*16 namedin
      character*16 named
      named=namedin
!  make sure range name is in upper case
      n=len_trim(named)
      call low2up(named,N)

!  determine if range was input; search the range names vector for (named)
      irng = ifindrng(named)
      if (irng.lt.0) goto 990   ! range not found

!  dimflag indicates the dimension conflicts that may exist between the
!  defined range dimensions and the user variable dimensions
      dimflag = isetdflg(irng,ix,iy,iz)

!  determine looping variables needed to assign user variable based upon the dimflag.
      call fsetxyz(irng,ix,iy,iz,x,y,z,dimflag)

!  begin loop to assign user specified variable
      ival = rngloc(irng)
      do k = 1,z
         do i = 1,x
            do j = 1,y
               Vname(i,j,k) = rngval(ival)
               ival = ival + 1
            enddo
         enddo
      enddo
      if(xmlout.gt.0) then
        write(xmlout,'(a)')'========================================================='
        do k=1,z
          do i=1,x
             write(xmlout,'(a,2i4, <y>F10.3)') trim(namedin),k,i,(vname(i,j,k),j=1,y)
          enddo
        enddo
      endif
990   continue

      return
      end

! -----------------------------------------------------------
      subroutine getrngc(namedin,Vname,ix,iy,iz)
      implicit none

      include 'wk1block'
      common/nemswk1/xmlout
      integer xmlout
      integer*4 ipoint
      integer*4 n,ix,iy,iz
      integer*4 x,y,z
      integer*4 i,j,k
      integer*4 dimflag   ! set based upon range/variable dimension conflict
      integer*4 ifindrng  ! function to find range number
      integer*4 isetdflg  ! function to set the dimension conflict variable
      character*(*) Vname(ix,iy,iz)
      character*16 namedin
      character*16 named

      named=namedin
!  make sure range name is in upper case
      n=len_trim(named)
      call low2up(named,N)

!  determine if range was input; search the range names vector for (named)
      irng = ifindrng(named)
      if (irng.lt.0) goto 990   ! range not found

!  dimflag indicates the dimension conflicts that may exist between the
!  defined range dimensions and the user variable dimensions
      dimflag = isetdflg(irng,ix,iy,iz)

!  determine looping variables needed to assign user variable based upon the dimflag.
      call fsetxyz(irng,ix,iy,iz,x,y,z,dimflag)

!  begin loop to assign user specified variable
      ival = rngloc(irng)
      do k = 1,z
         do i = 1,x
            do j = 1,y
               ipoint=nint(rngval(ival))
               if(ipoint.le.maxstrings.and.ipoint.gt.0) then
                 Vname(i,j,k) = rnglab(ipoint)
               else
                 Vname(i,j,k) = ' '
               endif
               ival = ival + 1
            enddo
         enddo
      enddo

      if(xmlout.gt.0) then
        write(xmlout,'(a)')'========================================================='
        do k=1,z
          do i=1,x
            do j=1,y
             write(xmlout,'(a,3i5,4x,a)') trim(namedin),k,i,j,trim(vname(i,j,k))
            enddo
          enddo
        enddo
      endif

990   continue

      return
      end

! -----------------------------------------------------------
      subroutine readrng(funit)
      implicit none

!  This subroutine reads all defined ranges from the worksheet
!  file funit.  The values are stored into a vector called rngval.
!  the user can then call the function getrngi or getrngr to extract the
!  desired ranges

      include 'wk1block'

      integer*4 funit    ! worksheet file unit number
      integer*4 i,j
      integer*4 irow,icol
      integer*4 iscol,isrow,iecol,ierow
      integer*4 isc,isr,ier,iec
      integer*4 nskip
      integer*4 nread
      character*1 CMODE,CORD,RANGE*16,BYTE,LAB(254),string*16
      character*1 format
      integer*2 formati
      equivalence(formati,format)
      character*127 blank(2)
      equivalence (blank(1),lab(1))
      integer*4 ICMODE,ICORD
      integer rectyp*2,reclen*4                                         !* JCS: as arg to cwkreadi, rectyp must be int*2 *!
      integer*4 nsize
      real*8 RVALUE
      integer*2 IVALUE
      character*9 type
      DATA ISC/-1/,ISR/-1/,IER/-1/,IEC/-1/
! for RS6000:
      integer retcode,in_file,l
      integer*4 eofstat,n2/2/,n1/1/,n16/16/,n4/4/,n8/8/
      character*259 fname
      integer cwkopen2,cwkclosin,cwkreadi,cwkreada, &
       cwkreadf
      external cwkopen2,cwkclosin,cwkreadi,cwkreada, &
       cwkreadf

      real*8 rval8
      real*4 rval4
      character*8 cval8,cval4*4
      equivalence (cval8,rval8)
      equivalence (cval4,rval4)

!  The RS6000 fortran can't read binary files (only "unformatted").  Find
!  out the name of the file, close it, then reopen it using a "c" function.
      fname=' '
      inquire(unit=funit,name=fname)
      write(6,*) ' Reading wk1 file ',fname
      close(funit,status='keep')
      l=len_trim(fname)
      do i=l+1,len(fname)
        fname(i:i)=char(0)
      enddo

      in_file=cwkopen2(fname)
! end RS6000

!  initialize range ralue storage arrays
      do i = 1,maxrng
         rngname(i) = ' '
         rngstr(i) =  ' '
         do j = 1,locdim
            rngdim(i,j) = 0
         enddo
      enddo
      do i = 1, maxstrings
         rnglab(i)=' '
      enddo
      do i = 1,maxval
         rngval(i) = 0.0
      enddo

      icount = 0
      ivcount = 0
      istrcount = 0
      irng = 0
      ival = 1   ! set to point to top of rgnval vector
      nread=0
      nsize=0
      STRING=' '
10    continue

      retcode=cwkreadi(rectyp,n2,eofstat)
      if(eofstat.eq.1) goto 900
      retcode=cwkreadi(reclen,n2,eofstat)
    ! call rs2pci2(rectyp) ! this should work in either direction since it just flips bytes
    ! call rs2pci2(reclen)
        IF (RECTYP.LT.0.OR.RECTYP.GT.150.or.eofstat.ne.0) THEN
           WRITE(6,*) ' ERROR: reading worksheet file '
           WRITE(6,*) ' *** RECTYP= ',RECTYP
           WRITE(6,*) ' *** RECLEN= ',RECLEN
           write(6,*) ' *** eofstat=',eofstat,' ( 0=not eof, 1=eof)'
           GOTO 1000
!        else
!          write(6,*) ' rectyp,reclen=',rectyp,reclen,' ',record(rectyp)

        ENDIF

        type=record(rectyp)

        if (TYPE.EQ.'EOF') then
          goto 900

        elseif (type.eq.'INTEGER')then
           retcode=cwkreada(FORMAT,n1,eofstat)
           retcode=cwkreadi(ICOL,  n2,eofstat)
           retcode=cwkreadi(IROW,  n2,eofstat)
           retcode=cwkreadi(IVALUE,n2,eofstat)
!  call setrng to assign element to rngval if it belongs to any ranges
           call setrng(irow,icol,rvalue,ivalue,'I')

        elseif (type.eq.'NUMBER')then
           retcode=cwkreada(FORMAT,n1,eofstat)
           retcode=cwkreadi(ICOL,  n2,eofstat)
           retcode=cwkreadi(IROW,  n2,eofstat)
           retcode=cwkreadf(RVALUE,n8,eofstat)
!  call setrng to assign element to rngval if it belongs to any ranges
           call setrng(irow,icol,rvalue,ivalue,'R')

        elseif (type.eq.'FORMULA')then
           retcode=cwkreada(FORMAT,n1,eofstat)
           retcode=cwkreadi(ICOL,  n2,eofstat)
           retcode=cwkreadi(IROW,  n2,eofstat)
           retcode=cwkreadf(RVALUE,n8,eofstat)
           retcode=cwkreadi(NSKIP, n2,eofstat)
           call SKIPC(NSKIP)
!  call setrng to assign element to rngval if it belongs to any ranges
           call setrng(irow,icol,rvalue,ivalue,'R')

        elseif (type.eq.'SNRANGE'.or.RECTYP.eq.11) then
           retcode=cwkreada(RANGE,n16,eofstat)
           retcode=cwkreadi(ISCOL, n2,eofstat)
           retcode=cwkreadi(ISROW, n2,eofstat)
           retcode=cwkreadi(IECOL, n2,eofstat)
           retcode=cwkreadi(IEROW, n2,eofstat)
           if(reclen.gt.24) retcode=cwkreada(BYTE,n1,eofstat)
           if(reclen.gt.25) retcode=cwkreada(BYTE,n1,eofstat)


!  convert null characters off end of range name
          call stripz(range,16)
          call trimrng(range,16)
          call low2up(range,16)

!  create the worksheet range string ( ie C5..F30 )
          call strrng(iscol,isrow,iecol,ierow,byte,string)
!  set up range storage variables
          icount = icount + 1
          if (icount.gt.maxrng) then
             write(6,*)
             write(6,*) ' -------------------------------------------'
             write(6,*) ' ERROR: Maximum number of ranges in an input'
             write(6,*) '        worksheet was exceeded.'
             write(6,*) '        worksheet name: ',trim(fname)
             write(6,*) '        range limit: ',maxrng
             write(6,*) ' -------------------------------------------'
             write(6,*)
             goto 900   ! stop reading any further
          endif
          irng = irng + 1
          rngname(irng) = range
          rngstr(irng) = string
          rngdim(irng,1) = iscol
          rngdim(irng,2) = isrow
          rngdim(irng,3) = iecol
          rngdim(irng,4) = ierow
          rngdim(irng,5) = (ierow-isrow+1)*(iecol-iscol+1) ! total elements
          rngloc(irng) = ival   ! assign start location for this range
          ival = ival + rngdim(irng,5)  ! set start location for next range
          if (ival.gt.maxval) then
             write(6,*)
             write(6,*) ' -------------------------------------------'
             write(6,*) ' ERROR: Maximum number of range elements in'
             write(6,*) '        an input worksheet was exceeded.'
             write(6,*) '        worksheet range name: ',range
             write(6,*) '        elements limit: ',maxval
             write(6,*) ' -------------------------------------------'
             write(6,*)
             goto 900   ! stop reading any further
          endif
        elseif(type.eq.'LABEL')then
           retcode=cwkreada(FORMAT,n1,eofstat)
           retcode=cwkreadi(ICOL,  n2,eofstat)
         ! call rs2pci2(ICOL)
           retcode=cwkreadi(IROW,  n2,eofstat)
         ! call rs2pci2(IROW)
          L=RECLEN-5
          blank(1)=' '
          blank(2)= ' '
          retcode=cwkreada(LAB,l,eofstat)
!         WRITE(6,'(a,3i5,254a)')  ' LABEL=',
!     1     ICOL,IROW,L,(LAB(I),I=2,L-1)

!  call setrngc to assign element to rnglab if it belongs to any ranges. drop the leading
!  punctuation and the ending character (ascii 0).
           call setrngc(irow,icol,LAB(2:L-1),L-2)
        elseif(type.eq.'STRING') then
           retcode=cwkreada(FORMAT,n1,eofstat)
           retcode=cwkreadi(ICOL,  n2,eofstat)
         ! call rs2pci2(ICOL)
           retcode=cwkreadi(IROW,  n2,eofstat)
         ! call rs2pci2(IROW)
           L=RECLEN-5
           blank(1)=' '
           blank(2)= ' '
           retcode=cwkreada(LAB,l,eofstat)

!          WRITE(6,'(a,3i5,254a)')  ' string=',
!     1     ICOL,IROW,L,(LAB(I),I=1,L-1)
!  call setrngc to assign element to rnglab if it belongs to any ranges. drop the ending
!  character (ascii 0)
           call setrngc(irow,icol,LAB(1:L-1),L-1)

        else
          call SKIPC(reclen)
        endif

        nread=nread+1
!       nsize=nsize+int4(4)+int4(reclen)
      goto 10

!  branch here when end of file is hit
900   continue

!  branch here when error in reading worksheet occurs
1000  continue
!     nsize=nsize+int4(4)+int4(reclen)
!     write(6,*) ' records read=',nread
!     write(6,*) ' bytes read=',nsize

!  list of the stuff for debug
!     write(6,*) ' debug output from readrng'
!     write(6,*) ' number of ranges (icount):',icount
!     write(6,*) ' next range location (ivcount):',ivcount
!     do i = 1,icount
!        write(6,*) '  rngname ',rngname(i)
!        write(6,*) '   rngstr ',rngstr(i)
!        write(6,*) '   rngloc ',rngloc(i)
!        write(6,*) ' rngdim 1 ',rngdim(i,1)
!        write(6,*) '        2 ',rngdim(i,2)
!        write(6,*) '        3 ',rngdim(i,3)
!        write(6,*) '        4 ',rngdim(i,4)
!        write(6,*) '        5 ',rngdim(i,5)
!     enddo

!     write(6,*)
!     write(6,*) ' list of the data in rngval'
!     do i = 1,ivcount
!        write(6,*) ' rngval: ',rngval(i)
!     enddo
      retcode = cwkclosin()
! reopen the file so file_mgr doesn't get angry when calling
! routine tries to close the file
      open(funit,file=fname,status='old',action='read')

      return
      end

! -----------------------------------------------------------
      subroutine mreadrng(funit)
      implicit none

!  This subroutine reads all defined ranges from the worksheet
!  file funit.  The values are stored into a vector called rngval.
!  the user can then call the function getrngi or getrngr to extract the
!  desired ranges

      include 'wk1block'

      integer*4 funit    ! worksheet file unit number
      integer*4 i,j
      integer*4 irow,icol
      integer*4 iscol,isrow,iecol,ierow
      integer*4 isc,isr,ier,iec
      integer*4 nskip
      integer*4 nread
      character*1 CMODE,CORD,RANGE*16,BYTE,LAB(254),string*16
      character*1 format
      integer*2 formati
      equivalence(formati,format)
      character*127 blank(2)
      equivalence (blank(1),lab(1))
      integer*4 ICMODE,ICORD
      integer rectyp*2,reclen*4                                         !* JCS: as arg to cwkreadi, rectyp must be int*2 *!
      integer*4 nsize
      real*8 RVALUE
      integer*2 IVALUE
      character*9 type
      DATA ISC/-1/,ISR/-1/,IER/-1/,IEC/-1/
! for RS6000:
      integer retcode,in_file,l
      integer*4 eofstat,n2/2/,n1/1/,n16/16/,n4/4/,n8/8/
      character*259 fname
      integer cwkopen2,cwkclosin,cwkreadi,cwkreada, &
       cwkreadf
      external cwkopen2,cwkclosin,cwkreadi,cwkreada, &
       cwkreadf

      real*8 rval8
      real*4 rval4
      character*8 cval8,cval4*4
      equivalence (cval8,rval8)
      equivalence (cval4,rval4)

!  The RS6000 fortran can't read binary files (only "unformatted").  Find
!  out the name of the file, close it, then reopen it using a "c" function.
      fname=' '
      inquire(unit=funit,name=fname)
      write(6,*) ' Reading wk1 file ',fname
      close(funit,status='keep')
      l=len_trim(fname)
      do i=l+1,len(fname)
        fname(i:i)=char(0)
      enddo

      in_file=cwkopen2(fname)
! end RS6000

!  initialize range ralue storage arrays
      do i = 1,maxrng
         rngname(i) = ' '
         rngstr(i) =  ' '
         do j = 1,locdim
            rngdim(i,j) = 0
         enddo
      enddo
      do i = 1, maxstrings
         rnglab(i)=' '
      enddo
      do i = 1,maxval
         rngval(i) = 0.0
      enddo

      icount = 0
      ivcount = 0
      istrcount = 0
      irng = 0
      ival = 1   ! set to point to top of rgnval vector
      nread=0
      nsize=0
      STRING=' '

! If mac.f is calling this subroutine then there is no defined rangename
!  in the worksheet.  These are the arguments if there were a rangename.
!  Mac.f uses funit equal to 2 for epmac.wk1.
      if (funit .eq. 2) then
        RANGE = "MYRANGE"       ! a range name
        ISCOL = 1               ! start column, col B in wk1 = 1
        ISROW = 1               ! start row, row 2 in wk1 = 1
        IECOL = 41              ! end column, col ? in wk1 = 41
        IEROW = 179             ! end row, row 179 in wk1 = 179
        irng = irng + 1         ! rangename counter
        rngname(irng) = range   ! array containing rangename
        rngstr(irng)  = string  ! blank space
        rngdim(irng,1) = iscol  ! defining start column for irng range
        rngdim(irng,2) = isrow  ! defining start row for irng row
        rngdim(irng,3) = iecol  ! defining end column for irng range
        rngdim(irng,4) = ierow  ! defining end row for irng range
        rngdim(irng,5) = (ierow-isrow+1)*(iecol-iscol+1) ! total number of elements
        rngloc(irng) = ival     ! range location
        ival = ival + rngdim(irng,5)  ! set start location for next range
        icount=irng
      end if

! If mac.f is calling this subroutine then there is no defined rangename
!  in the worksheet.  These are the arguments if there were a rangename.
!  Mac.f uses funit equal to 3 for mc_detail.wk1 and mc_vehicles.wk1.
      if (funit .eq. 3) then
        RANGE = "MYRANGE"      ! a range name
        ISCOL = 1              ! start column, col B in wk1 = 1
        ISROW = 1              ! start row, row 2 in wk1 = 1
        IECOL = 41             ! end column, col AP in wk1 = 41
        IEROW = 101            ! end row, row 102 in wk1 = 101
        irng = irng + 1        ! rangename counter
        rngname(irng) = range  ! array containing rangename
        rngstr(irng)  = string ! blank space
        rngdim(irng,1) = iscol ! defining start column for irng range
        rngdim(irng,2) = isrow ! defining start row for irng row
        rngdim(irng,3) = iecol ! defining end column for irng range
        rngdim(irng,4) = ierow ! defining end row for irng range
        rngdim(irng,5) = (ierow-isrow+1)*(iecol-iscol+1) ! total number of elements
        rngloc(irng) = ival    ! range location
        ival = ival + rngdim(irng,5)  ! set start location for next range
        icount=irng
      end if

! If mac.f is calling this subroutine then there is no defined rangename
!  in the worksheet.  These are the arguments if there were a rangename.
!  Mac.f uses funit equal to 14 for mc_commflr.wk1, mc_regmac.wk1, mc_regio.wk1
!  and mc_regemp.
      if (funit .eq. 14) then
        RANGE = "MYRANGE"       ! a range name
        ISCOL = 1               ! start column, col B in wk1 = 1
        ISROW = 1               ! start row, row 2 in wk1 = 1
        IECOL = 41              ! end column, col AP in wk1 = 41
        IEROW = 414             ! end row, row 316 in wk1 = 315
        irng = irng + 1         ! rangename counter
        rngname(irng) = range   ! array containing rangename
        rngstr(irng)  = string  ! blank space
        rngdim(irng,1) = iscol  ! defining start column for irng range
        rngdim(irng,2) = isrow  ! defining start row for irng row
        rngdim(irng,3) = iecol  ! defining end column for irng range
        rngdim(irng,4) = ierow  ! defining end row for irng range
        rngdim(irng,5) = (ierow-isrow+1)*(iecol-iscol+1) ! total number of elements
        rngloc(irng) = ival     ! range location
        ival = ival + rngdim(irng,5)  ! set start location for next range
        icount=irng
      end if

10    continue

      retcode=cwkreadi(rectyp,n2,eofstat)
      if(eofstat.eq.1) goto 900
      retcode=cwkreadi(reclen,n2,eofstat)
        IF (RECTYP.LT.0.OR.RECTYP.GT.150.or.eofstat.ne.0) THEN
           WRITE(6,*) ' ERROR: reading worksheet file '
           WRITE(6,*) ' *** RECTYP= ',RECTYP
           WRITE(6,*) ' *** RECLEN= ',RECLEN
           write(6,*) ' *** eofstat=',eofstat,' ( 0=not eof, 1=eof)'
           GOTO 1000

        ENDIF

        type=record(rectyp)

        if (TYPE.EQ.'EOF') then
          goto 900

        elseif (type.eq.'INTEGER')then
           retcode=cwkreada(FORMAT,n1,eofstat)
           retcode=cwkreadi(ICOL,  n2,eofstat)
           retcode=cwkreadi(IROW,  n2,eofstat)
           retcode=cwkreadi(IVALUE,n2,eofstat)
!  call setrng to assign element to rngval if it belongs to any ranges
           call setrng(irow,icol,rvalue,ivalue,'I')

        elseif (type.eq.'NUMBER')then
           retcode=cwkreada(FORMAT,n1,eofstat)
           retcode=cwkreadi(ICOL,  n2,eofstat)
           retcode=cwkreadi(IROW,  n2,eofstat)
           retcode=cwkreadf(RVALUE,n8,eofstat)
!  call setrng to assign element to rngval if it belongs to any ranges
           call setrng(irow,icol,rvalue,ivalue,'R')

        elseif (type.eq.'FORMULA')then
           retcode=cwkreada(FORMAT,n1,eofstat)
           retcode=cwkreadi(ICOL,  n2,eofstat)
           retcode=cwkreadi(IROW,  n2,eofstat)
           retcode=cwkreadf(RVALUE,n8,eofstat)
           retcode=cwkreadi(NSKIP, n2,eofstat)
           call SKIPC(NSKIP)
!  call setrng to assign element to rngval if it belongs to any ranges
           call setrng(irow,icol,rvalue,ivalue,'R')

!  This elseif block is not used if mac.f calls the subroutine.  Mac.f is
!   the only module that sets file unit equal to 2.
        elseif (type.eq.'SNRANGE'.or.RECTYP.eq.11.and.funit.ne.2) then
           retcode=cwkreada(RANGE,n16,eofstat)
           retcode=cwkreadi(ISCOL, n2,eofstat)
           retcode=cwkreadi(ISROW, n2,eofstat)
           retcode=cwkreadi(IECOL, n2,eofstat)
           retcode=cwkreadi(IEROW, n2,eofstat)
           if(reclen.gt.24) retcode=cwkreada(BYTE,n1,eofstat)
           if(reclen.gt.25) retcode=cwkreada(BYTE,n1,eofstat)


!  convert null characters off end of range name
          call stripz(range,16)
          call trimrng(range,16)
          call low2up(range,16)

!  create the worksheet range string ( ie C5..F30 )
          call strrng(iscol,isrow,iecol,ierow,byte,string)
!  set up range storage variables
          icount = icount + 1
          if (icount.gt.maxrng) then
             write(6,*)
             write(6,*) ' -------------------------------------------'
             write(6,*) ' ERROR: Maximum number of ranges in an input'
             write(6,*) '        worksheet was exceeded.'
             write(6,*) '        worksheet name: ',range
             write(6,*) '        range limit: ',maxrng
             write(6,*) ' -------------------------------------------'
             write(6,*)
             goto 900   ! stop reading any further
          endif
          irng = irng + 1
          rngname(irng) = range
          rngstr(irng) = string
          rngdim(irng,1) = iscol
          rngdim(irng,2) = isrow
          rngdim(irng,3) = iecol
          rngdim(irng,4) = ierow
          rngdim(irng,5) = (ierow-isrow+1)*(iecol-iscol+1) ! total elements
          rngloc(irng) = ival   ! assign start location for this range
          ival = ival + rngdim(irng,5)  ! set start location for next range
          if (ival.gt.maxval) then
             write(6,*)
             write(6,*) ' -------------------------------------------'
             write(6,*) ' ERROR: Maximum number of range elements in'
             write(6,*) '        an input worksheet was exceeded.'
             write(6,*) '        worksheet range name: ',range
             write(6,*) '        elements limit: ',maxval
             write(6,*) ' -------------------------------------------'
             write(6,*)
             goto 900   ! stop reading any further
          endif
        elseif(type.eq.'LABEL')then
           retcode=cwkreada(FORMAT,n1,eofstat)
           retcode=cwkreadi(ICOL,  n2,eofstat)
           retcode=cwkreadi(IROW,  n2,eofstat)
          L=RECLEN-5
          blank(1)=' '
          blank(2)= ' '
          retcode=cwkreada(LAB,l,eofstat)

!  call setrngc to assign element to rnglab if it belongs to any ranges. drop the leading
!  punctuation and the ending character (ascii 0).
           call setrngc(irow,icol,LAB(2:L-1),L-2)
        elseif(type.eq.'STRING') then
           retcode=cwkreada(FORMAT,n1,eofstat)
           retcode=cwkreadi(ICOL,  n2,eofstat)
           retcode=cwkreadi(IROW,  n2,eofstat)
           L=RECLEN-5
           blank(1)=' '
           blank(2)= ' '
           retcode=cwkreada(LAB,l,eofstat)

!  call setrngc to assign element to rnglab if it belongs to any ranges. drop the ending
!  character (ascii 0)
           call setrngc(irow,icol,LAB(1:L-1),L-1)

        else
          call SKIPC(reclen)
        endif

        nread=nread+1
      goto 10

!  branch here when end of file is hit
900   continue

!  branch here when error in reading worksheet occurs
1000  continue

!  list of the stuff for debug
!     write(6,*) ' debug output from readrng'
!     write(6,*) ' number of ranges (icount):',icount
!     write(6,*) ' next range location (ivcount):',ivcount
!     do i = 1,icount
!        write(6,*) '  rngname ',rngname(i)
!        write(6,*) '   rngstr ',rngstr(i)
!        write(6,*) '   rngloc ',rngloc(i)
!        write(6,*) ' rngdim 1 ',rngdim(i,1)
!        write(6,*) '        2 ',rngdim(i,2)
!        write(6,*) '        3 ',rngdim(i,3)
!        write(6,*) '        4 ',rngdim(i,4)
!        write(6,*) '        5 ',rngdim(i,5)
!     enddo
!
!     write(6,*)
!     write(6,*) ' list of the data in rngval'
!     do i = 1,ivcount
!        write(6,*) ' rngval: ',rngval(i)
!     enddo
      retcode = cwkclosin()
! reopen the file so file_mgr doesn't get angry when calling
! routine tries to close the file
      open(funit,file=fname,status='old',action='read')

      return
      end

! -----------------------------------------------------------
  subroutine ReadRngXML(funit,Sheet)
  implicit none

!  This subroutine reads all defined ranges from the XML workbook that apply to worksheet "Sheet"
!  in open file number funit.  The values are stored into a vector called rngval.
!  the user can then call the function getrngi or getrngr to extract the
!  data from the desired ranges into integer or real arrays.
! this routine once for each worksheet to be processed.

    include 'wk1block'
    character*(*) Sheet ! name of worksheet to read.  Only 1 per call.
    integer*4 funit    ! worksheet file unit number
    integer*4 i,j,iret,iline,is,ie
    integer*4 irow,icol,numlines,oldcol
    integer*4 iscol,isrow,iecol,ierow
    character*1 byte1,RANGE*16,LAB*254,string*16,rowjmp*16
    real*8 RVALUE
    integer IVALUE*2, L*4                                               !* JCS: as arg to setrngc, L must be int*4 *!
    character*9 typedata
    integer maxlines
    parameter(maxlines=1024)
    character*512 line,CellData*512,RANGESTR*127,WorkSheet*31,xml_section(maxlines)
    character*259 fname
    logical found,skip
    fname=' '
    inquire(unit=funit,name=fname)
    write(6,*) ' Reading XML workbook file ',trim(fname)
    rewind(funit)
!  initialize range ralue storage arrays
    rngname(1:maxrng) = ' '
    rngdim(1:maxrng,1:locdim) = 0
    rnglab(1:maxstrings)=' '
    rngval(1:maxval) = 0.0
    ivalue=0
    icount = 0
    ivcount = 0
    istrcount = 0
    irng = 0
    ival = 1   ! set to point to top of rgnval vector
    STRING=' '
    iret=0
!    write(6,*)'      NamedRange  ISROW  ISCOL  IEROW  IECOL  #Cells'
! read NamedRanges applicable to the designated worksheet (designated via argument "Sheet")
    do while (iret.eq.0)
      call FindNextXML(funit,'NamedRange','Names',line,iret)
      if(iret.eq.0) then
        call FindAttrXML(line,'ss:Name=',RANGE)
        call low2up(RANGE,16)
        call FindAttrXML(line,'ss:RefersTo=',RANGESTR)
        if(index(RANGESTR,Sheet).gt.0 .and. index(RANGESTR,'#REF').eq.0) then
          call Parse_RangeStr(rangestr,iscol,isrow,iecol,ierow)
!  set up range storage variables
          icount = icount + 1
          if (icount.gt.maxrng) then
             write(6,*) ' ERROR: Maximum number of ranges in an input exceeded.'
             write(6,*) '        workbook name: ',trim(fname)
             write(6,*) '        range limit: ',maxrng
             return
          endif
          irng = irng + 1
          rngname(irng) = range
          rngdim(irng,1) = iscol
          rngdim(irng,2) = isrow
          rngdim(irng,3) = iecol
          rngdim(irng,4) = ierow
          rngdim(irng,5) = (ierow-isrow+1)*(iecol-iscol+1) ! total elements
!          write(6,'(a16,5I7)') range,isrow,iscol,ierow,iecol,rngdim(irng,5)
          rngloc(irng) = ival   ! assign start location for this range
          ival = ival + rngdim(irng,5)  ! set start location for next range
          if (ival.gt.maxval) then
            write(6,*) ' ERROR: Maximum number of range elements in'
            write(6,*) '        an input worksheet was exceeded.'
            write(6,*) '        worksheet range name: ',range
            write(6,*) '        elements limit: ',maxval
            return
          endif
        endif
      elseif(iret.eq.1) then
        write(6,*) ' Number of globally named-ranges for this sheet:',irng
      endif
    enddo

! rewind the file and begin reading the row and column contents of each worksheet
    if(icount.eq.0) then
      write(6,*) 'ERROR: no named ranges identified in the workbook for sheet ' ,trim(Sheet)
      return
    endif
    rewind(funit)

    found=.false.
    do while (.not. found .or. iret .ne. 0)
      call FindNextXML(funit,'Worksheet','Workbook',line,iret)
      call FindAttrXML(line,'ss:Name=',Worksheet)
      if(trim(Worksheet).eq.trim(Sheet)) then
        found=.true.
        write(6,*)' Identified Worksheet, name: ',trim(Sheet), ' in the workbook'
      endif
    enddo
    if (.not. found) then
       write(6,*) 'Error: Worksheet not found in workbook.  Sheet=',trim(sheet)
       return
    endif
    IROW=0
    do while (.not. eof(funit).and.iret.eq.0)
      call FindNextXML(funit,'Row','Worksheet',line,iret) ! find next Row tag in current Worksheet (parent)
      if(iret.eq.0) then
        call findAttrXML(line,'ss:Index=',rowjmp)
        if(len_trim(rowjmp).gt.0) then
          read(rowjmp,*,err=999,end=999) IROW
        else
          IROW=IROW+1
        endif
        ! check for empty row, like "<Row/>" or "<Row ss:AutoFitHeight="0"/>
        skip=.false.
        is=index(line,'<Row')
        if(is.eq.0) then
          skip=.true.
        else
          is=is+4
          ie=index(line(is:),'/>')
          if(ie.ne.0) then
            ie=is+ie-1
            if(index(line(is:ie),'<').eq.0) then
              skip=.true.
            endif
          endif
        endif
        if(.not.skip) then ! if not skipping empty row designated by "<row.../>"
! get all the cells on this row by pulling in the contents of this multiline row section
          call FindNextXMLSect(funit,'Row',XML_Section,maxlines,numlines)
!          if(irow.gt.2564.and.irow.lt.2600) then
!             write(6,'(a,i5,a)')"XML_Section, irow=",irow,"  Start----------------------------------------"
!             do i=1,numlines
!               write(6,'(2a)')'    ', trim(xml_section(i))
!             enddo
!             write(6,'(a,i5,a)')"XML_Section, irow=",irow,"  End------------------------------------------"
!          endif
          ICOL=0
          iline=0
          do while (iline.le.numlines)
            oldcol=icol
            call findNextXMLCell(XML_Section,numlines,iline,ICOL,CellData)
!            if(irow.gt.2564.and.irow.lt.2600) then
!              write(6,'(3i5,3a)') irow,icol,iline-1,trim(celldata)
!            endif
            if(icol.gt.0.and.icol.ne.oldcol) then
              if(len_trim(CellData).gt.0) then
                call Parse_CellData(CellData,rvalue,LAB,typedata)
                if (typedata.eq.'Number')then
!  call setrng to assign element to rngval if it belongs to any ranges
                  irng=1
                  do while(irng.le.icount)
                    call setrng2(irow,icol,rvalue,ivalue,'R',irng)
                  enddo
                elseif(typedata.eq.'String')then
!  call setrngc to assign element to rnglab if it belongs to any character ranges
                  l=len_trim(lab)
                  call setrngc(irow,icol,LAB(:L),L)
                endif
              else ! emtpy cell with no data
                rvalue=0.
                ivalue=0
                call setrng2(irow,icol,rvalue,ivalue,'R',irng)
              endif
            endif
          enddo
        endif
      endif
    end do

!    write(6,*) ' debug output from readRngXML'
!    write(6,*) ' number of ranges (icount):',icount
!    write(6,*) ' next range location (ivcount):',ivcount
!    write(6,*) '    i         rngname    rngloc dim1 dim2 dim3 dim4 dim5'
!    do i = 1,icount
!       write(6,'(1x,i4,1x,a,i10,5i5)') i, rngname(i),rngloc(i),rngdim(i,1:5)
!    enddo

!    write(6,*)
!    write(6,*) ' rngval:'
!    do i = 1,ivcount
!       write(6,'(1x,i4,f16.6)') i,rngval(i)
!    enddo

    return
999 write(6,*)'Error: Read Error, irow= ',irow, ' line=',trim(line)
    return
    end
! -----------------------------------------------------------
    subroutine findAttrXML(line,attr,value)
      implicit none
      character*(*) line,attr,value
      integer is,ie
      value=' '
      is=index(line,trim(attr)//'"')
      if(is.gt.0) then
        is=is+len_trim(attr)+1 ! add starting "
        ie=is+index(line(is:),'"')-2
        if(ie.ge.is) value=line(is:ie)
      endif
    return
    end
! -----------------------------------------------------------
    subroutine Parse_RangeStr(rangestr,iscol,isrow,iecol,ierow)
    implicit none
! extracts the row and column pairs from a range string that uses RC format
    character*(*) rangestr
    integer iscol,isrow,iecol,ierow
    integer is,ie
    isrow=0
    iscol=0
    ierow=0
    iecol=0
    is=index(rangestr,'!R')+2
    ie=is+index(rangestr(is:),'C')-2
    if(is.gt.0.and.ie.ge.is) then
      read(rangestr(is:ie),*) isrow
      is=ie+2
      ie=is+scan(Rangestr(is:),': ')-2
      if(is.gt.0.and.ie.ge.is) then
        read(rangestr(is:ie),*) iscol
        ie=ie+2
        if(scan(rangestr(ie:ie),'" ').gt.0) then
          ierow=isrow
          iecol=iscol
        else
          is=ie+1
          ie=is+index(rangestr(is:),'C')-2
          read(rangestr(is:ie),*) ierow
          is=ie+2
          ie=len_trim(rangestr)
          read(rangestr(is:ie),*) iecol
        endif
      endif
    endif
    return
    end
    subroutine Parse_RangeStr2(rangestr,iscol,isrow,iecol,ierow)
    implicit none
! extracts the row and column pairs from a range string that uses letter Column/numeric row format
    character*(*) rangestr
    character*10 digits/'0123456789'/
    character*26 letters/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
    character*1 col
    integer iscol,isrow,iecol,ierow
    integer is,ie,icol
    integer nColLtrs,i,j
    isrow=0
    iscol=0
    ierow=0
    iecol=0
! look for start of range at end of sheet name, after the ! delimiter
! then look between $ characters for column leters and row digits
    is=index(rangestr,'!$')+2
    ie=is+scan(rangestr(is:),'$')-2
    if(is.gt.0.and.ie.ge.is) then
      nColLtrs=ie-is+1
      j=0
      do i=nColLtrs,1,-1
        col=rangestr(is+i-1:is+i-1)
        icol=index(letters,col) ! convert from A-Z to 1-26
        iscol=iscol+icol*(26**j)
        j=j+1
      enddo
      is=ie+2
      ie=is+scan(Rangestr(is:),':"$ ')-2
      read(rangestr(is:ie),*) isrow
      is=ie+3
      ie=is+scan(Rangestr(is:),'"$ ')-2
      if(ie.ge.is) then

        nColLtrs=ie-is+1
        j=0
        do i=nColLtrs,1,-1
          col=rangestr(is+i-1:is+i-1)
          icol=index(letters,col)
          iecol=iecol+icol*(26**j)
          j=j+1
        enddo
        is=ie+2
        ie=is+scan(Rangestr(is:),':"$ ')-2
        read(rangestr(is:ie),*) ierow

      else
        ierow=isrow
        iecol=iscol
      endif
    endif
    return
    end
! -----------------------------------------------------------
    subroutine FindNextXML(funit,tag,parent,line,iret)
    implicit none
 ! finds the value contents for the next single line tag in an xml file, unit=funit, containing a given start tag (case sensitive tag name)
 ! and returns the contents in "line" left justfied.
 ! for example, if the tag is "NamedRange" and the next line with such a tag is:
 !
 ! <NamedRange ss:Name="steamseg_steel" ss:RefersTo="=icogen!R241C2:R241C9"/>
 !
 ! then on return, the contents of "line" will be: 'ss:Name="steamseg_steel" ss:RefersTo="=icogen!R241C2:R241C9"'
 !
 ! if no such lines are found before the given "parent" end tag is found, iret is set to 1.
 ! if no such lines are found before the end of file is reached, iret is set to 2
 ! if the next line with the tag doesn't contain the full contents of the tag because the "/>" is not found, iret is set to 3

      character*(*) tag
      character*(*) line
      character*(*) parent
      character*20 search
      integer funit,L,LL,iret,ipos,nest_level,LP

      iret=0
      ipos=0
      L=len_trim(tag)
      LP=len_trim(parent)
      search='</'//parent(:LP)//'>'
      line=' '
      do while (.not. eof(funit))
        ipos=0
        read(funit,'(a)',err=99) line
        LL=len_trim(line)
        if(index(line,trim(search)).gt.0 ) then
          if(tag.ne.parent) then ! see if the end of the parent has been reached
            iret=1
            return
          else
            return
          endif
        endif
        ipos=index(line,'<'//tag(:L)//'>')
        if(ipos.eq.0) then
          ipos=index(line,'<'//tag(:L))
        endif
        if(ipos.gt.0) then
          ipos=ipos+l+1
          nest_level=1
          do while (nest_level .gt.0 .and. ipos .le. LL)
            if(line(ipos:ipos).eq.'>') then
              nest_level=nest_level-1
            elseif(line(ipos:ipos).eq.'<') then
              nest_level=nest_level+1
            endif
            ipos=ipos+1
          enddo
          if(nest_level.gt.0) then
            iret=3
          elseif(ipos.lt.LL) then
            line(ipos+1:LL)=' '
          endif
          line=adjustl(line)
          return
        endif
      enddo
      iret=2 ! end of file
      return
99    iret=99 ! other read error
    return
    end
! -----------------------------------------------------------
    subroutine FindNextXMLSect(funit,tag,section,maxlines,numlines)
      implicit none
! get multiline section of xml file until the end tag is found
      integer funit,numlines,maxlines
      character*(*) tag,section(maxlines)
      character*512 line
      numlines=0 ! number of lines in the file section
      section=' '

      do while (.not. eof(funit).and.numlines.lt.maxlines)
        read(funit,'(a)') line
        if(index(line,'</'//trim(tag)).eq.0) then
          numlines=numlines+1
          section(numlines)=line
        else
          return
        endif
      enddo
      if(numlines.ge.maxlines) then
         write(6,'(a)')' error: in subroutine FindNextXMLSect, maxlines reached when reading this tag:'//trim(tag)//'.'
         write(6,'(a)')' The xml file may be malformed or limits have been reached. '
         write(6,'(a)')' May need to increase the size of array xlm_section() in the calling routine, ReadRngXLSX or ReadRngXLM,'
         write(6,'(a)')' if you are have worksheet rows with lots of data or lots of columns.'
      endif
    return
    end
 ! -----------------------------------------------------------
    subroutine findNextXMLCell(Section,numlines,iline,ICOL,CellData)
      implicit none
      integer numlines,iline,ICOL
      logical found
      character*(*) Section(numlines)
      character*(*) CellData
      character*16 coljmp
      integer ipos,is,itok
      CellData=' '
      found=.false.
      if(iline.lt.1) iline=1
      do while(iline.le.numlines)
        is=index(section(iline),'<Cell')
        if(is.gt.0) then
          found=.true.
          call findAttrXML(section(iline)(is:),'ss:Index=',coljmp)
          if(len_trim(coljmp).gt.0) then
            read(coljmp,*,err=999,end=999) ICOL
            itok=scan(section(iline)(is+1:),'</>') ! find next token
            if(itok.gt.0) then
              if(section(iline)(is+itok:is+1+itok).eq.'/>') then  ! checking for <cell ss:Index=12/> exception. normally there is an explicit end tag, "</cell>"
                iline=iline+1  !skip col
                cycle          !skip col
              endif
            endif
999         continue
          else
            ICOL=ICOL+1
          endif
! find next token to check for empty cell (cell ends with no data field)
          itok=scan(section(iline)(is+1:),'</>')
          if(itok.gt.0) then
            if(section(iline)(is+itok:is+1+itok).eq.'/>') then  ! checking for "<cell attribute=value/>", an empty cell situation. normally there is an explicit end tag, "</cell>"
!             write(6,*) iline,trim(section(iline))  ! debug
              iline=iline+1
              cycle           ! skip col
            endif
          endif

! look for the cell's data section and assign it to the variable "CellData"
          is=is+5
          do while ((index(section(iline),'</Data>').eq.0) .and.  &
                    (index(section(iline)(is:),'<Cell')  .eq.0) .and.  iline.lt.numlines)
            iline=iline+1
            is=1
          enddo

            ipos=index(section(iline),'</Data>')
            if(ipos.gt.0) then
              CellData=section(iline)(:ipos-1)
              iline=iline+1
              return
            endif
            iline=iline+1
            return  ! should be here if the cell is empty of data (formatting only)

        endif
        iline=iline+1 ! here when cell ends on line after the data tag
      enddo
    return
    end subroutine findNextXMLCell
! -----------------------------------------------------------
    subroutine Parse_CellData(CellData,rvalue,LAB,TypeData)
      implicit none
      character*(*) CellData,LAB,TypeData
      real*8 rvalue
      integer ipos
      rvalue=0.0
      LAB=' '
      TypeData=' '
      call FindAttrXML(CellData,'ss:Type=',TypeData)
      ipos=index(CellData,'>',.true.)+1 ! Start of data field by searching backwards
      if(TypeData.eq.'Number'.and.ipos.gt.1) then
        read(CellData(ipos:),*) rvalue
      else
        LAB=CellData(ipos:)
      endif
    return
    end subroutine Parse_CellData
! -----------------------------------------------------------
    subroutine FindValueXML(Line,cValue,iret)
    implicit none
! finds data between first ">" and subsequent "<\"
! for example, if line is:
!   <definedName name="BL_CH4COAL">offsets!$C$52:$BK$52</definedName>
!   it returns                    "offsets!$C$52:$BK$52"
    character*(*) line
    character*(*) cValue
    integer iret
    integer is,ie
    iret=0
    is=0
    ie=0
    is=index(line,'>')+1
    cValue=' '
    if(is.gt.1) then
      ie=index(line(is:),'</')
      if(ie.gt.0) then
        ie=is+ie-2
      else
        ie=is
      endif
      cValue=line(is:ie)
    else
      iret=1
    endif

    return
    end subroutine FindValueXML
! -----------------------------------------------------------
   subroutine findNextXMLCellC(Section,numlines,iline,ICOL,CellData,TypeData)
! examines the group of tags in the row section and finds the next
! cell, identifies the column name and returns a string with the numeric data in the cell
!
      implicit none
      integer numlines,iline,ICOL
      character*(*) Section(numlines)
      character*(*) CellData
      character*(*) TypeData
      character*16 coljmp
      integer is,is2,iret,ic,ie,i,j,nColLtrs,itok
      character*10 digits/'0123456789'/
      character*26 letters/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      character*1 col,dattyp

      CellData=' '
      TypeData=' '

      if(iline.lt.1) iline=1
      do while(iline.le.numlines)
        is=index(section(iline),'<c ')
        is2=index(section(iline),'<v>')
        if(is.gt.0 .or.is2.gt.0) then
          if(is.gt.0) then
            call findAttrXML(section(iline)(is:),'t=',TypeData)
            call findAttrXML(section(iline)(is:),'r=',coljmp)
            is=1
            ie=scan(coljmp,digits)-1
            if(ie.ge.is) then
              icol=0
              j=0
              nColLtrs=ie-is+1
              do i=nColLtrs,1,-1
                col=coljmp(is+i-1:is+i-1)
                ic=index(letters,col)
                icol=icol+ic*(26**j)
                j=j+1
              enddo
            endif
          else
            call FindValueXML(section(iline),CellData,iret)
          endif
! find next token to check for empty or string-filled cell (cell ends with no value field)
          itok=scan(section(iline)(is+1:),'</>')
          if(itok.gt.0 .and. index(section(iline),'<c').gt.0) then
            if(section(iline)(is+itok:is+1+itok).eq.'/>') then  ! checking for string-filled cells, like "<c r="A496" s="66"/> ", which end without a <v>...</v> value.
!             write(6,*) iline,trim(section(iline))  ! debug
              iline=iline+1
              cycle           ! skip col, go to end of do-while and on to next line
            endif
          endif

          if(iline.lt.numlines) iline=iline+1
          if(index(section(iline),'<v>')) then
            call FindValueXML(section(iline),CellData,iret)
          endif
        endif
        if(index(section(iline),'</c>')) then
          iline=iline+1
          return
        endif
        iline=iline+1
      enddo
    return
    end subroutine findNextXMLCellC

      subroutine skipc(reclenin)
      implicit none
!  skips "reclen" characters in binary file iunit
      integer*4 n256,c256/256/
      integer*4 reclenin
      integer*4 iunit,i
      external cwkreada
      integer*4 cwkreada,retcode  !*4
      integer*4 eofstat,reclen
      character*256 buf,byte*1
      reclen=reclenin
      n256=reclen/c256
      reclen=mod(reclen,c256)
      do i=1,n256
        retcode=cwkreada(buf,c256,eofstat)
      enddo
      if(reclen.gt.0) retcode=cwkreada(buf,reclen,eofstat)
      return
      end

! -----------------------------------------------------------
      subroutine strrng(isc,isr,iec,ier,byte,string)
      implicit none

! subroutine to convert lotus range specs into alpha col/row type range
      character*2 collet(2),string*16,byte*1,csrow*4,cerow*4
      integer*4 isc,isr,iec,ier
      integer*4 ibyte,iscol,iecol,isrow,ierow
      integer*4 lscol,lecol,lsrow,lerow

      ibyte=ichar(byte)
      iscol=isc+1
      iecol=iec+1
      isrow=isr+1
      ierow=ier+1
      call wk1_col(iscol,collet(1),lscol)
      call wk1_col(iecol,collet(2),lecol)
      call lefti(csrow,isrow,lsrow)
      call lefti(cerow,ierow,lerow)
      call wrtstr(string,collet(1),lscol,csrow,lsrow, &
            collet(2),lecol,cerow,lerow,ibyte)
      return
      end

! -----------------------------------------------------------
      subroutine wrtstr(string,scol,lscol,csrow,lsrow, &
          ecol,lecol,cerow,lerow,ibyte)
      implicit none

      integer*4 i,lscol,lsrow,lecol,lerow,ibyte
      character*1 scol(lscol),csrow(lsrow),ecol(lecol), &
       cerow(lerow),string*16
!
      string=' '
      if(ibyte.eq.1)then
         write(string,'(16a)'  ) (scol(i),i=1,lscol), &
         (csrow(i),i=1,lsrow),'..', &
         (ecol(i),i=1,lecol),(cerow(i),i=1,lerow)
      else
         write(string,'(16a)'  ) (scol(i),i=1,lscol), &
         (csrow(i),i=1,lsrow)
      endif
      return
      end


! -----------------------------------------------------------
      subroutine wk1_col(icol,col,j)
      implicit none

!    creates lotus column from column number 1-256
      character*2 col
      integer*4 icol
      integer*4 j,n,n2
      integer*4 icol4
      icol4=icol
      col=' '
      n=(icol-1)/26
      n2=mod(icol4,26)
      if(n2.eq.0)n2=26
      j=1
      if(n.gt.0)then
         col(1:1)=char(64+n)
         j=2
      endif
      col(j:j)=char(64+n2)
      return
      end

! -----------------------------------------------------------
      subroutine stripz(zstring,n)
      implicit none

! converts nul characters at end of stringz to blanks
      integer i,n
      character*1 zstring(n)
      character*1 znul
      character*1 znul2

      znul=char(0)
      znul2=char(255)
      do 10 i=n,1,-1
        if(zstring(i).eq.znul.or.zstring(i).eq.znul2)then
           zstring(i)=' '
         else
           return
         endif
10    continue
      return
      end

! -----------------------------------------------------------
      subroutine trimrng(zstring,n)
      implicit none

!  converts all characters to blank after first blank is reached

      integer*4 hitblank  ! signals first blank reached
      integer*4 n,i
      character*1 zstring(n)
      character*1 znul
      character*1 znul2

      znul=char(0)
      znul2=char(255)
      hitblank = 0
      do i = 1,n
         if (hitblank.eq.1) then
            zstring(i) = ' '
         else
            if (zstring(i).eq.' '.or.zstring(i).eq.znul.or. &
                zstring(i).eq.znul2) then
               zstring(i) = ' '
               hitblank = 1
            endif
         endif
      enddo

      return
      end

! -----------------------------------------------------------
       subroutine lefti(c,i,l)
       implicit none

! left justifies integer of length 4 into c. l is length
       character*4 c
       integer*4 i,l

       c=' '
       if(i.lt.10) then
          write(c,'(i1)') i
          l=1
       elseif(i.gt.9.and.i.lt.100) then
          write(c,'(i2)') i
          l=2
       elseif(i.gt.99.and.i.lt.1000) then
          write(c,'(i3)') i
          l=3
       else
          write(c,'(i4)') i
          l=4
       endif
       return
       end
! -----------------------------------------------------------
       SUBROUTINE LOW2UP(A,N)
       implicit none
!     CONVERTS STRING A TO UPPER CASE
      integer N,I,J
      CHARACTER*1 A(N)
      DO 10 I=1,N
        J=ICHAR(A(I))
        IF(J.GT.96.AND.J.LT.123) A(I)=CHAR(J-32)
   10 CONTINUE
      RETURN
      END

! -----------------------------------------------------------
      integer function igetname(rng)
      implicit none

!  this function locates the range name (rng) in the list of ranges
!  contained in array (rngname).  return -1 if range not found

      include  'wk1block'
      character*16 rng

      igetname = -1
      do irng=1,maxrng
        if (rng.eq.rngname(irng)) igetname = irng
      enddo

      return
      end

! -----------------------------------------------------------
      subroutine setrng(irow,icol,rvalue,ivalue,type)
      implicit none

!  this function will assign the value in rvalue to rngval for all ranges in which it was defined.

      include  'wk1block'

      integer*4 igetloc     ! function to get element offset in rngval
      integer*4 irow,icol   ! location of element
      real*8 rvalue
      integer*2 ivalue
      character*1 type    ! which type of variable we are setting

      do irng = 1,max(1,icount)
!  test if element is part of current range (irng)
         if (ICOL.GE.rngdim(irng,1).AND.ICOL.LE.rngdim(irng,3).and. &
             IROW.GE.rngdim(irng,2).AND.IROW.LE.rngdim(irng,4)) then
             ivcount = ivcount + 1  ! increment range element counter

!  determine the location of the data element in vector rngval
             ival = igetloc(irng,irow,icol)

             if (type.eq.'r'.or.type.eq.'R') then
                rngval(ival) = sngl(rvalue)
             else
                rngval(ival) = float(ivalue)
             endif

!            write(6,*) ' element in range: ',irng
!            write(6,*) ' icol,irow ',icol,irow
!            write(6,*) ' isc,isr,iec,ier ',rngdim(irng,1),
!    1                  rngdim(irng,2),rngdim(irng,3),rngdim(irng,4)
        endif
      enddo

      return
      end
! -----------------------------------------------------------
      subroutine setrngc(irow,icol,LAB,L)
      implicit none

!  this function will assign the value LAB to rnglab for all ranges in which it was defined.

      include  'wk1block'

      integer*4 igetloc     ! function to get element offset in rngval
      integer*4 irow,icol   ! location of element
      integer L,IFUNNY
      character*(L) LAB
      do irng = 1,icount
!  test if element is part of current range (irng)
         if (ICOL .GE. rngdim(irng,1) .AND. ICOL .LE. rngdim(irng,3) .and. &
             IROW .GE. rngdim(irng,2) .AND. IROW .LE. rngdim(irng,4)) then
!  determine the location of the data element in vector rngval
           ival = igetloc(irng,irow,icol)
           if(ival.gt.0) then
             rngval(ival) = float(istrcount) ! assign a pointer instead of a value
             if(istrcount .lt. maxstrings) then
               rnglab(istrcount)= LAB
               rnglen(istrcount)= L
               ifunny=index(LAB,'0.0')
               if(ifunny.gt.0) then
                 write(6,'("  irng,L= ",2i5)') irng,L
                 write(6,'(1x,a,a,1x,i5,a)') 'Warning: 0.0 as string in range:  ',  &
                              rngname(irng), istrcount,LAB(:L)
               endif
             elseif(istrcount.eq.maxstrings) then
               rnglab(istrcount)= LAB
               rnglen(istrcount)= L
               write(6,*) ' maximum number of strings reached : ', maxstrings
               write(6,*) ' I guess I''ll just have to ignore the rest'
             endif
           endif
        endif
      enddo

      return
      end
! -----------------------------------------------------------
      subroutine setrng2(irow,icol,rvalue,ivalue,type,irng)
      implicit none

!  this function will assign the value in rvalue to rngval for all ranges in which it was defined.

      include  'wk1block'

      integer*4 igetloc     ! function to get element offset in rngval
      integer*4 irow,icol   ! location of element
      real*8 rvalue
      integer*2 ivalue
      character*1 type    ! which type of variable we are setting
      integer i
      if(irng.eq.0) irng=1
      ival=0
      do i=irng,icount
!  test if element is part of current range (i)
         if (ICOL.GE.rngdim(i,1).AND.ICOL.LE.rngdim(i,3).and. &
             IROW.GE.rngdim(i,2).AND.IROW.LE.rngdim(i,4)) then
             ivcount = ivcount + 1  ! increment range element counter

!  determine the location of the data element in vector rngval
             ival = igetloc(i,irow,icol)

             if (type.eq.'r'.or.type.eq.'R') then
                rngval(ival) = sngl(rvalue)
             else
                rngval(ival) = float(ivalue)
             endif
             irng=i+1 ! next call will check other ranges
             return
        endif
      enddo
      irng=icount+1
      return
      end

! -----------------------------------------------------------
      integer*4 function igetloc(i,irow,icol)
      implicit none

!  this function determins the location of a data element within
!  the value storage vector (rngval).  returns the location

      include  'wk1block'
      integer*4 i
      integer*4 offset
      integer*4 irow,icol   ! location of element

!  compute offset = number of full rows * row width + left over elements
      offset =  ((irow-rngdim(i,2))*(rngdim(i,3)-rngdim(i,1)+1)) &
                + (icol-rngdim(i,1))
      igetloc = rngloc(i) + offset

      return
      end

      integer function ifindrng(named)
      implicit none

!  this function tries to locate the range (named) in the list of
!  defined ranges.  it returns the range number if it was found
!  otherwise -1.

      include  'wk1block'

      integer igetname   ! function that looks up range name in range list
      character*16 named

      ifindrng = -1

      irng = igetname(named)
      if (irng.lt.0) then   ! range name not found
         write(6,*)
         write(6,*) ' ----------------------------------------------'
         write(6,*) ' ERROR: The following range name was not found'
         write(6,*) '        in the worksheet file: ',named
         write(6,*) ' ----------------------------------------------'
         write(6,*)
         ifindrng = -1
      else
        ifindrng = irng
      endif

      return
      end

! -----------------------------------------------------------
      integer function isetdflg(irng,ix,iy,iz)
      implicit none

!  this function determines if there are any conflicts between
!  the defined range and the user specified variable dimensions.

      include  'wk1block'

      integer*4 ix,iy,iz
      integer*2 returnValue
      integer*4 rheight,rwidth   ! defined range height and width
      integer*4 vheight,vwidth   ! user variable height and width
      integer*4 onedim

      rheight = rngdim(irng,4) - rngdim(irng,2) + 1
      rwidth = rngdim(irng,3) - rngdim(irng,1) + 1
      onedim=0
! if the spreadsheet range is scalar or one dimensional, then relax order
! set onedim flag to allow for row or column range shape to be used.
      if(rheight.eq.1 .or.rwidth.eq.1) then
        onedim=1
      endif
      if ((iy.eq.1.and.iz.eq.1).or.(ix.eq.1.and.iz.eq.1)) then
         if(onedim.eq.0) then
           vheight = 1
           vwidth =  ix
         else
           if(rheight.ge.rwidth) then
             vheight = max(ix,iy)
             vwidth = 1
           else
             vheight = 1
             vwidth  = max(ix,iy)
           endif
         endif
      else
         vheight = ix * iz
         vwidth = iy
      endif

      returnValue = 0 ! initialize to no conflicts

!  check if range dimensions greater than user variable dimensions
      if ((rheight.gt.vheight).or.(rwidth.gt.vwidth)) then
         write(6,*) ' WARNING: worksheet input range ', &
                    'dimension was larger than the'
         write(6,*) '          specified variable range.'
         write(6,*) '          range name: ',rngname(irng)
         write(6,*) '          worksheet range dimensions:', &
                    rheight,',',rwidth
         write(6,*) '          variable dimensions:', &
                    vheight,',',vwidth
         write(6,*) '          note: dimensions are listed as x*z,y ', &
                    'for 3 dimensional arrays'
         write(6,*) ' ACTION:  the specified variable will ', &
                    'be assigned until dimension'
         write(6,*) '          limits are reached.'
         returnValue = 1
      endif

!  check if user variable dimensions greater than range dimensions
      if ((vheight.gt.rheight).or.(vwidth.gt.rwidth)) then
         write(6,*) ' WARNING: specified variable ', &
                    'dimension was larger than the'
         write(6,*) '          worksheet input range.'
         write(6,*) '          range name: ',rngname(irng)
         write(6,*) '          worksheet range dimensions:', &
                    rheight,',',rwidth
         write(6,*) '          variable dimensions:', &
                    vheight,',',vwidth
         write(6,*) '          note: dimensions are listed as x*z,y ', &
                    'for 3 dimensional arrays'
         write(6,*) ' ACTION:  the specified variable will ', &
                    'be assigned using the worksheet'
         write(6,*) '          range data that was available'
         if (returnValue.gt.0) then
            returnValue = 3   ! multiple conflicts
         else
            returnValue = 2
         endif
      endif

      isetdflg = returnValue

!     write(6,*) ' dimensions flag (dimflag): ',returnValue

      return
      end

! -----------------------------------------------------------
      subroutine fsetxyz(irng,ix,iy,iz,x,y,z,dimflag)
      implicit none

!  this function determines if there are any conflicts between
!  the defined range and the user specified variable dimensions.

      include  'wk1block'

      integer*4 ix,iy,iz,x,y,z,dimflag
      integer*4 itemp

!  assign the data for given range to the user specfied varible
      if (dimflag.eq.0) then
         x = ix
         y = iy
         z = iz
      elseif (dimflag.eq.1) then  ! range vars > user vars
         x = ix
         y = iy
         z = iz
      elseif (dimflag.eq.2) then  ! user vars > range vars

         write(6,*)
         write(6,*) ' in dimflag = 2'
         write(6,*) ' rngdim 4 & 2 ',rngdim(irng,4),rngdim(irng,2)
         write(6,*) ' rngdim 3 & 1 ',rngdim(irng,3),rngdim(irng,1)
         write(6,*) ' ix,iy,iz ',ix,iy,iz
         write(6,*)



!  check if range data is contained in a single row - in this case ix,iy are switched
         if (rngdim(irng,4)-rngdim(irng,2)+1.eq.1) then
!  check if user variable is one row also
            if (iy.eq.1.and.iz.eq.1) then ! single row
               x = rngdim(irng,3)-rngdim(irng,1)+1
               y = 1
               z = 1   ! no groups
            else   ! not single row
               x = 1
               y = rngdim(irng,3)-rngdim(irng,1)+1
               z = 1   ! no groups
            endif
         else  ! range data not in single row
            y = rngdim(irng,3) - rngdim(irng,1) + 1
!  check for insufficient number of rows in defined range
            if (ix.gt.(rngdim(irng,4)-rngdim(irng,2)+1)) then
               x = rngdim(irng,4)-rngdim(irng,2)+1
            else
               x = ix
            endif
            if (iz.eq.1) then
               z = 1
            else
               z = (rngdim(irng,4)-rngdim(irng,2)+1)/x
            endif
         endif ! range data not in single row

      elseif (dimflag.eq.3) then ! combo range var > user var & user > range

!  check if range data is contained in a single row - in this case ix,iy are switched
         if (rngdim(irng,4)-rngdim(irng,2)+1.eq.1) then
!  check if user variable is one row also
            if (iy.ne.1.or.iz.ne.1) then
               itemp = rngdim(irng,3)-rngdim(irng,1)+1
               x = min(iy,itemp)  ! x holds the y value for rowed data
               y = 1   ! set to 1
               z = 1   ! no groups
            else
               itemp = rngdim(irng,3)-rngdim(irng,1)+1
               x = min(ix,itemp)
               y = 1
               z = 1   ! no groups
            endif
         else  ! range data not in single row
!  test if user variable is one row
            if (iy.eq.1.and.iz.eq.1) then
               itemp = rngdim(irng,3)-rngdim(irng,1)+1
               x = min(ix,itemp)
               y = 1
               z = 1   ! no groups
            else
               itemp = rngdim(irng,4)-rngdim(irng,2)+1
               x = min(ix,itemp)
               itemp = rngdim(irng,3)-rngdim(irng,1)+1
               y = min(iy,itemp)
               z = 1   ! no groups
            endif
         endif

      endif   ! end if dimflag = ...

!     write(6,*) ' Loop variables -> x,y,z: ',x,y,z

      return
      end
!
Subroutine Unwrap_xlsx(filen)
use dfport
implicit none
! Routine creates a bat file and invokes it to unzip the contents of an excel file, like workbook.xlsx,
! into a subfolder called workbook.  The folder contains various xml-format files,
! including one with the names & index numbers of each worksheet and named ranges, one
! with the shared strings, and the worksheet files with the numeric data.
! This routine unzips the xlsx file so its component files can be process by xml-processing code.
include 'wk1block'
character*(*) filen
logical bat_written/.false./
integer b_unit
logical lexist
character*300 cmd
integer iret,i
! Below is the text of a bat file to unzip an xlsx file.
! The bat file uses the powershell command line program to unzip files.
! To overwride this bat file, put an alternative version of it in the output folder.
!
integer, parameter :: nlines=42
character*100 unwrap_bat(nlines)/  &
'@echo off                                                                                          ', &
'REM   Uses powershell command line program powershell to unzip an .xlsx file after copying it to   ', &
'REM   a file with the zip extension. The name can include a child folder, "input\aimefd.xlsx" ,    ', &
'REM   or it may be the name of the xlsx file in the current folder, as in "aimefd.xlsx".           ', &
'REM   A child folder using the worksheet basename, like "aimefd" is created to hold the            ', &
'REM   unzipped files.                                                                              ', &
'REM   If a second argument "replace" is found, the files are extracted even if the folder          ', &
'REM   exists already.  "replace" can be omitted to eliminate repeated unzipping.                   ', &
'                                                                                                   ', &
'if "%1" == "" goto noarg                                                                           ', &
'if not exist %1 goto notfound                                                                      ', &
'                                                                                                   ', &
'set xlsx=%1                                                                                        ', &
'set folder=%xlsx:~0,-5%                                                                            ', &
'if "%2" == "replace" goto replace                                                                  ', &
'  if exist %folder% goto existsalready                                                             ', &
'                                                                                                   ', &
':replace                                                                                           ', &
'  if exist %folder% rd /s /q %folder%                                                              ', &
'mkdir %folder%                                                                                     ', &
'REM copying %xslx% to %folder%_copy.zip to make powershell recognize it as a zip file.             ', &
'set cname=%folder%_copy.zip                                                                        ', &
'copy %xlsx% %cname% > nul                                                                          ', &
'set xlsx=%cname%                                                                                   ', &
'echo Unzipping %xlsx% into %folder% using this command:                                            ', &
'echo powershell -Command "Expand-Archive /od %xlsx% %folder%                                       ', &
'powershell -Command "Expand-Archive %xlsx% -DestinationPath %folder%"  > nul                       ', &
'rem echo removing %xlsx%                                                                           ', &
'del %xlsx%                                                                                         ', &
'goto end                                                                                           ', &
':noarg                                                                                             ', &
'echo "no xlxs file name argument given"                                                            ', &
'goto end                                                                                           ', &
'                                                                                                   ', &
':existsalready                                                                                     ', &
'echo %folder% exists already, so not unzipping. add "replace" as argument to replace the old folder', &
'goto end                                                                                           ', &
'                                                                                                   ', &
':notfound                                                                                          ', &
'echo %1 not found                                                                                  ', &
'                                                                                                   ', &
':end                                                                                               '/




inquire(file='unwrap.bat',exist=lexist)
if( .not. lexist) then                  ! may want to have unwrap.bat modified outside this program to implement other unzip command.  so don't overwrite it.
  if (.not. bat_written) then
  !   get unused unit number
      call unitunopened(100,999,b_unit)
  !   Write batfile
    open(unit=b_unit,file='unwrap.bat',status='unknown')
    rewind b_unit    ! it may exist, so this causes the output to overwrite rather than append
    do i=1,nlines
      write(b_unit,'(a)') trim(unwrap_bat(i))
    enddo
    close(b_unit)
    bat_written=.true.
  endif
endif

! if files exists in a subfolder \input\, then use that one. if file exists
! in current working folder, use that one. set xsubfolder to one or the other.

xsubfolder=' '  !
inquire(file=trim(filen),exist=lexist)
if(.not. lexist) then
  inquire(file='input\'//trim(filen),exist=lexist)
  if(lexist) then
    xsubfolder='input\'
  else
    write(6,'(a)') ' Error. '//trim(filen)//' not found in current folder or in input subfolder.'
    return
  endif
endif

if(len_trim(xsubfolder).gt.0) then
  cmd='unwrap.bat '//trim(xsubfolder)//trim(filen)  ! if folder already exists, the file will not be re-unzipped
else
  cmd='unwrap.bat '//trim(filen)//' replace'  ! replace argument forces replacement.
endif
call callsys(iret,cmd)
if(iret .ne. 0) then
   write(6,'(a,i4)') ' Error unzipping the xlsx file '//trim(xsubfolder)//trim(filen)//' using unwrap.bat'
endif

return
end subroutine Unwrap_xlsx
! -----------------------------------------------------------
  subroutine ReadRngXLSX(funit_in,Sheet)
  implicit none

!  This subroutine reads all defined ranges from the XLSX WorkBook that apply to worksheet "Sheet".
!  1) Calls Unwrap_xlsx to unzip the xlsx file to a subfolder (happens once)
!  2) Reads the range names and sheet # of the desired sheet named as an argument from /xl/workbook.xml
!  3) reads the numeric values from      /xl/worksheets/sheet#.xml that fall within the range of any
!     named sheet-ranges.
!  The values are stored into a vector called rngval.
!  the user can then call the function getrngi or getrngr to extract the
!  data from the desired ranges into integer or real arrays.
!  This routine is called once for each worksheet to be processed.

    include 'wk1block'
    character*(*) Sheet ! name of worksheet to read.  Only 1 per call.
    character*80 sheetname ! excel sheetname generated as sheet//sheetid
    character*4 sheetID,rID*8
    integer*4 funit_in,funit,w_unit    ! worksheet file unit number and its flat working file unit number
    integer*4 i,j,iret,iline,is,ie
    integer*4 irow,icol,numlines,oldcol
    integer*4 iscol,isrow,iecol,ierow,L1,L2
    character*1 byte1,RANGE*16,LAB*254,string*16,rowjmp*16
    integer,parameter :: MaxXMLStringSpace=150000  ! bytes of string storage space
    integer NumXMLStrings/-1/  ! number of shared strings in this workbook
    integer XMLNextStringLoc   ! location of the next string in the string storage area
    character*(MaxXMLStringSpace) XMLStrings
    integer XMLStringStart(0:MaxStrings)     ! starting location of each string in XMLStrings
    integer XMLStringEnd(0:MaxStrings)       ! ending   location of each string in XMLStrings
    integer end_of_string                    ! flag to note when string tag ends in cases of continuation

    real*8 rvalue
    integer ivalue*2, L*4                    !* JCS: as arg to setrngc, L must be int*4 *!
    character*3 TypeData,hidden*1
    integer maxlines
    parameter(maxlines=1024)
    integer, parameter :: maxLineLength=2048
    character*maxLineLength line,CellData*maxLineLength,RANGESTR*127,xml_section(maxlines)

    character*259 fname,folder ! names of the .xlsx file and the folder in which it has been unzipped to.  The folder name is the .xlsx file prefix
    character*259 workbook,workbook2            ! stores the workbook.xml      file name within the unzipped .xlsx XL folder
    character*259 worksheet,worksheet2          ! stores the sheet#.xml        file name within the unzipped .xlsx XL/worksheets folder
    character*259 sharedStrings,SharedStrings2  ! stores the sharedStrings.xml file name within the unzipped .xlsx XL folder
    character*259 lastStrFileRead
    character*259 basename

    logical lexist
    logical skip
    integer lline
    integer islash ! file name character position of the last slash in the file name
    integer dot ! file name character position of the dot in ".xlsx" or ".xlsm"
    funit=funit_in
    call unitunopened(funit_in,999,funit)

    fname=' '

    inquire(unit=funit_in,name=fname) ! determine the file name from the unit number as established in filemgr

    islash=0
    islash=scan(fname,'/\',.true.)  ! scan backwards for position of last slash
    basename=fname(islash+1:)       ! file name without the path

    call unwrap_xlsx(basename)

    dot=index(basename,'.xlsx')+index(basename,'.xlsm') ! can be either type
    if(dot.gt.1) then

       folder=basename(1:dot-1)
       workbook=trim(xsubfolder)//trim(folder)//'\XL\workbook.xml'
       inquire(file=workbook,exist=lexist)
       if(.not.lexist) then
         write(6,*) trim(workbook)//' not found. returning.'
         return
       endif
    else
       write(6,*) trim(basename),' does not have the .xlsx or .xlsm extension expected'
       return
    endif

!   creating flatter file with end-of-line characters between tags, These tag changes occur with "><".
!   This flatter working file in a record-oriented text file can be processed with some of the existing
!   xml processing code.

    workbook2=trim(workbook)//'.txt'
    inquire(file=workbook2,exist=lexist)
    if(.not.lexist) then  ! only write it once. after that, re-use it.
      call flattenXML(funit,w_unit,workbook,workbook2,iret)

    else ! flat text file already created, so just reopen it
!     get unused unit number for temporary text file, workbook2
      call unitunopened(100,999,w_unit)
      open(unit=w_unit,file=workbook2,status='old')
      rewind(w_unit)
    endif

    funit=w_unit

    write(6,*) ' Reading  workbook component file to get named ranges and worksheet ID numbers ',trim(workbook2)

! get SheetID number associated with the designated worksheet
    sheetID=' '
    rID=' '
    call FindNextXML(funit,'sheets','workbook',line,iret)
    do while (iret.eq.0 .and. trim(sheetname).ne.sheet)
      call FindNextXML(funit,'sheet','workbook',line,iret)
      call FindAttrXML(line,'name=',sheetname)
      call FindAttrXML(line,'sheetId=',sheetID)
      call FindAttrXML(line,'r:id=',rID)
    enddo

    L1=len_trim(sheetID)
    if(L1.eq.0) then
      write(6,*)'ERROR: worksheet '//trim(sheet)//' not found in '//trim(workbook2)
      return
    else
      write(6,'(10a)') ' Identified Worksheet, name: ',trim(Sheet), ' in the workbook with sheet ID ',sheetID,' and r:id ',rID
      L2=len_trim(rID)
      if(sheetID(:L1).ne.rID(4:L2)) then
        sheetID=rID(4:L2)
        write(6,'(10a)') ' so assuming the unzipped xlsx worksheet file name is "sheet'//rid(4:L2)//'.xml"'
      endif
    endif




!  initialize range ralue storage arrays
    rngdim(1:maxrng,1:locdim) = 0
    rngname(1:maxrng) = ' '
    rngloc(1:maxrng) = 0
    rngval(1:maxval) = 0.0
    rnglab(1:maxstrings) = ' '
    ivalue=0
    icount = 0
    ivcount = 0
    istrcount = 0
    irng = 0
    ival = 1   ! set to point to top of rngval vector
    STRING=' '
    iret=0
    rewind(funit)
!    write(6,*)'      NamedRange  ISROW  ISCOL  IEROW  IECOL  #Cells' !debug
! read NamedRanges applicable to the designated worksheet (designated via argument "Sheet")
    call FindNextXML(funit,'definedNames','workbook',line,iret)
    do while (iret.eq.0)
      call FindNextXML(funit,'definedName','definedName',line,iret)
      if(iret.eq.0) then
       hidden=' '
       call FindAttrXML(line,'hidden=',hidden)
       if(hidden.ne.'1') then
        call FindAttrXML(line,'name=',RANGE)
        call low2up(RANGE,16)

        call FindValueXML(line,RANGESTR,iret)
        if(index(RANGESTR,Sheet).gt.0 .and. index(RANGESTR,'#REF').eq.0 .and. iret.eq.0) then
          call Parse_RangeStr2(rangestr,iscol,isrow,iecol,ierow)
!  set up range storage variables
          icount = icount + 1
          if (icount.gt.maxrng) then
             write(6,*) ' ERROR: Maximum number of ranges in an input exceeded.'
             write(6,*) '        workbook name: ',trim(fname)
             write(6,*) '        range limit: ',maxrng
             return
          endif
          irng = irng + 1
          rngname(irng) = range
          rngdim(irng,1) = iscol
          rngdim(irng,2) = isrow
          rngdim(irng,3) = iecol
          rngdim(irng,4) = ierow
          rngdim(irng,5) = (ierow-isrow+1)*(iecol-iscol+1) ! total elements
!          write(6,'(a16,5I7)') range,isrow,iscol,ierow,iecol,rngdim(irng,5) !debug
          rngloc(irng) = ival   ! assign start location for this range
          ival = ival + rngdim(irng,5)  ! set start location for next range
          if (ival.gt.maxval) then
            write(6,*) ' ERROR: Maximum number of range elements in'
            write(6,*) '        an input worksheet was exceeded.'
            write(6,*) '        worksheet range name: ',range
            write(6,*) '        elements limit: ',maxval
            return
          endif
        endif
       endif
      elseif(iret.eq.1) then
        write(6,*) ' Number of globally named-ranges for this sheet:',irng
      endif
    enddo

! rewind the file and begin reading the row and column contents of each worksheet
    if(icount.eq.0) then
      write(6,*) 'ERROR: no named ranges identified in the workbook for sheet ' ,trim(Sheet)
      return
    endif
    close(funit)


! open sharedStrings.xml and flatten into a record-formated working file
    sharedStrings=trim(xsubfolder)//trim(folder)//'\XL\sharedStrings.xml'
    sharedStrings2=trim(sharedStrings)//'.txt'
    inquire(file=sharedStrings,exist=lexist)
    if(.not.lexist) then
      write(6,*) trim(sharedStrings)//' not found. returning.'
      return
    endif

    if(laststrfileread.ne.sharedstrings2) then  ! only read once. but can re-read if called again after read another workbook.
      XMLStrings = ' '
      inquire(file=sharedStrings2,exist=lexist)
      if(.not.lexist) then  ! only write it once. after that, re-use it.
        call flattenXML(funit,w_unit,sharedStrings,sharedStrings2,iret)
      else ! flat text file already created, so just reopen it
        call unitunopened(100,999,w_unit)
        open(unit=w_unit,file=sharedStrings2,status='old')
        rewind(w_unit)
      endif

      funit=w_unit
 !    read the strings between <t ...> and </t> and copy them into the string storage area, keeping track of the location of each one

      XMLNextStringLoc=1
      numXMLStrings=-1
      end_of_string = 1
      iret=0
      do while (.not. eof(funit).and.iret.eq.0)
        if (XMLNextStringLoc .ge. MaxXMLStringSpace) then
           write(6,'("ERROR in nemswk1.f, subroutine ReadRngXLSX: XMLNextStringLoc > MaxXMLStringSpace.  Please increase MaxXMLStringSpace")')
           stop
        endif
        read(funit,'(a)') line
!  need to keep in mind that strings with formatting changes result in multiple '<t>' lines
!  we will daringly attempt to concatenate them
        if (index(line,'</si>') .gt.0) then          !  this marks the end of a string packet
           end_of_string=1
           cycle
        endif
! In this file, the strings can start with a space, which leads to something like this in the string tag:
!    <t xml:space="preserve"> per unit of landfill capacity</t>
! instead of just something like:  <t>SPNO</t>
! so we need to count all of these and treat them the same
        if (index(line,'<t>') .gt. 0 .or. index(line,'<t ') .gt. 0) then
        if (end_of_string .eq. 1) then
          call FindValueXML(line,LAB,iret)
          if(iret.eq.0) then
            numXMLStrings = numXMLStrings + 1
            L=len_trim(LAB)
            if (numXMLStrings.gt.MaxStrings) then
              write(6,'(a)') 'ERROR in nemskw1.f, subroutine ReadRngXLSX: numXMLStrings > MaxStrings. Increase MaxStrings and recompile, or reduce unique strings in workbook'
              stop
            endif
            end_of_string = 0                                    !  found a string, don't know if it is continued until it sees a '</si>'
            XMLStringStart(numXMLStrings) = XMLNextStringLoc
            XMLStringEnd(numXMLStrings) = min( XMLNextStringLoc + L - 1, MaxXMLStringSpace)
            XMLNextStringLoc = XMLStringEnd(numXMLStrings) + 1
! These next two statements (one commented) should be equivalent (unless we've hit MaxXMLStringSpace):
            XMLStrings( XMLStringStart(numXMLStrings) : XMLStringEnd(numXMLStrings) ) = LAB(:L)
!           XMLStrings((XMLStringEnd(numXMLStrings)-L+1): XMLStringEnd(numXMLStrings) ) = LAB(:L)
!           write(6,'(3i6,1x,a)') numXMLStrings,XMLStringStart(numXMLStrings),XMLStringEnd(numXMLStrings),trim(LAB)
          endif
        else
!  do much the same thing except where different
          call FindValueXML(line,LAB,iret)
          if(iret.eq.0) then
            L=len_trim(LAB)
!           XMLStringStart remains what it was
            XMLStringEnd(numXMLStrings) = min( XMLStringEnd(numXMLStrings) + L, MaxXMLStringSpace)     !  tack new string on to end
            XMLNextStringLoc = XMLStringEnd(numXMLStrings) + 1
! These next two statements (one commented) should be equivalent (unless we've hit MaxXMLStringSpace):
            XMLStrings( XMLStringEnd(numXMLStrings) - L + 1 : XMLStringEnd(numXMLStrings) ) = LAB(:L)
          endif
        endif        !  end of string
        endif
      enddo

      laststrfileread=sharedStrings2
      close(funit)
      if(XMLNextStringLoc .gt. MaxXMLStringSpace) then
        write(6,*) 'ERROR: Ran out of string space reading '//trim(sharedStrings2)//' so increase MaxXMLStringSpace in subroutine ReadRngXLSX'
      endif
    endif



    worksheet=trim(xsubfolder)//trim(folder)//'\XL\worksheets\sheet'//trim(sheetID)//'.xml'
    worksheet2=trim(worksheet)//'.txt'
    inquire(file=worksheet,exist=lexist)
    if(.not.lexist) then
      write(6,*) trim(worksheet)//' not found. returning.'
      return
    endif


    inquire(file=worksheet2,exist=lexist)
    if(.not.lexist) then  ! only write it once. after that, re-use it.
      call flattenXML(funit,w_unit,worksheet,worksheet2,iret)

    else ! flat text file already created, so just reopen it
      call unitunopened(100,999,w_unit)
      open(unit=w_unit,file=worksheet2,status='old')
      rewind(w_unit)
    endif

    funit=w_unit





    IROW=0
    iret=0
    do while (.not. eof(funit).and.iret.eq.0)
      call FindNextXML(funit,'row','sheeetData',line,iret) ! find next Row tag in current Worksheet (parent)
      if(iret.eq.0) then
        call findAttrXML(line,'r=',rowjmp)
        if(len_trim(rowjmp).gt.0) then
          read(rowjmp,*,err=999,end=999) IROW
        else
          IROW=IROW+1
        endif
        ! check for empty row, like "<row/>" or "<row ss:AutoFitHeight="0"/>
        skip=.false.
        is=index(line,'<row')
        if(is.eq.0) then
          skip=.true.
        else
          is=is+4
          ie=index(line(is:),'/>')
          if(ie.ne.0) then
            ie=is+ie-1
            if(index(line(is:ie),'<').eq.0) then
              skip=.true.
            endif
          endif
        endif
        if(.not.skip) then ! if not skipping empty row designated by "<row.../>"
! get all the cells on this row by pulling in the contents of this multiline row section
          call FindNextXMLSect(funit,'row',XML_Section,maxlines,numlines)
          ICOL=0
          iline=0
          do while (iline.le.numlines)
            oldcol=icol
            call findNextXMLCellC(XML_Section,numlines,iline,ICOL,CellData,TypeData)
            if(TypeData.ne.'str') then
              if(len_trim(CellData).gt.0 .and. index(CellData,'#') .eq. 0) then
                read(CellData,*) rvalue
              else
                rvalue=0.
              endif
              ivalue=int(rvalue)
            else ! string resulting from a formula
              LAB=CellData
              L=len_trim(LAB)
              numXMLStrings = numXMLStrings + 1           ! add one to total number of strings from sharedstrings file
              istrcount = numXMLStrings - 1               ! and use this identify it
              call setrngc(irow,icol,LAB,L)
              cycle
            endif
            if(icol.gt.0.and.icol.ne.oldcol) then
              if(TypeData.ne.'s') then   ! if not a shared String
                if(len_trim(CellData).gt.0) then
!  call setrng to assign element to rngval if it belongs to any ranges
                  irng=1
                  do while(irng.le.icount)
                    call setrng2(irow,icol,rvalue,ivalue,'R',irng)
                  enddo
                else ! empty cell with no numeric data; string maybe
                  rvalue=0.
                  ivalue=0
                  call setrng2(irow,icol,rvalue,ivalue,'R',irng)
                endif
              elseif(TypeData.eq.'s') then  ! if a shared string
                    read (CellData,'(I4)') istrcount      ! value has string number
                    LAB=XMLStrings(XMLStringStart(istrcount):XMLStringEnd(istrcount))
                    L=len_trim(LAB)
                    call setrngc(irow,icol,LAB,L)
              endif
            endif
          enddo
        endif
      endif
    end do

!    write(6,*) ' debug output from readRngXLSX'
!    write(6,*) ' number of ranges (icount):',icount
!    write(6,*) ' next range location (ivcount):',ivcount
!    write(6,*) '    i         rngname    rngloc dim1 dim2 dim3 dim4 dim5'
!    do i = 1,icount
!       write(6,'(1x,i4,1x,a,i10,5i5)') i, rngname(i),rngloc(i),rngdim(i,1:5)
!    enddo

!    write(6,*)
!    write(6,*) ' rngval:'
!    do i = 1,ivcount
!       write(6,'(1x,i4,f16.6)') i,rngval(i)
!    enddo

    return
999 write(6,*)'Error: Read Error, irow= ',irow, ' line=',trim(line)
    return
    end


    subroutine FlattenXML(funit,w_unit,original,flatter,iret)
    implicit none
! reads non-record-formatted XML file and writes a new one with tags on new lines
    integer funit            ! input unit
    integer w_unit           ! output unit
    character*(*) original   ! input file
    character*(*) flatter    ! output file
    integer iret    ! return code of 1 if problem
    character*1 bchar,lf,cr,oldchar
    integer ByteLoc
    integer lline
    integer, parameter :: maxLineLength=2048
    character*(maxLineLength) line
    iret=0
      open(funit,file=original,status='old',access='direct',form='binary',recl=1,buffered='yes')
      call unitunopened(100,999,w_unit)
      open(unit=w_unit,file=flatter,status='unknown')
      rewind(w_unit)

      lf=char(10)
      cr=char(13)
      ByteLoc=0
      line=' '
      lline=0
      oldchar=' '

! read the binary xml file character by character.  Write out a record with end of line is found or a tag end/start ("><"is found.
900   ByteLoc=ByteLoc+1
        read(funit,rec=ByteLoc,err=998) bchar
        if(bchar.eq.cr) goto 900  ! skip carriage return
        if(bchar.eq.lf .or.  (bchar.eq.'<'.and.oldchar.eq.'>')) then
          if(lline.eq.0) lline=1
          write(w_unit,'(a)') line(:lline)   ! write out the current line
          if(bchar.eq.lf) then    ! line feed marks end of a line
            line=' '
            lline=0
            oldchar=' '
          elseif(bchar.eq.'<') then   ! store the tag character as the first character for the next line
            lline=1
            line=' '
            line(lline:lline)=bchar
            oldchar=bchar
          endif
          goto 900  ! go read the next character
        endif

 ! add the character to the current line, increment character count, and save to check for cr/lf combo
        if(lline.lt. maxLineLength) then
          lline=lline+1
          line(lline:lline)=bchar
          oldchar=bchar

        else
          write(6,'(A)') 'ERR:  Maximum xml line length (maxLineLength) exceeded in FlattenXML.  Line:'
          write(6,'(A)') trim(line)
          iret=1
          return
        endif
        goto 900


998 continue

    if(lline.eq.0) lline=1
    write(w_unit,'(a)') line(:lline) ! write the last line in the file
    rewind(w_unit)
    close(funit)
    iret=0

    return
    end


