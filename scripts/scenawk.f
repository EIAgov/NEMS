      Program scenawk
      use dflib
      implicit none
!  This program reads $NEMS/scripts/varkeys, and the scedes file 
!  and creates a set of change instructions which it stores
!  in keys.sed (unit 6, redirected).

! some formulas for character variable size:
!     line = uniq + scen + subdir + val + 5 (variables from varkeysawk.f)
!     fld = line - uniqfile - uniqscen
!     uniqvers = fld
!     vers = uniqvers
!     var = NEMS + 20
!     val = uniqvers
! uniqscen and uniqvers refer to the old RCS system
      integer num_def
      parameter(num_def=25000)
      character*20 uniqfile(num_def),uniq_shrt,uniq
      character*15 scen
      character*99 fld(4), vers, val
      character*100 NEMS, NEMSPC, USERPC, NEMSSHARE, type
      character*100 NEMSPCDIR
      character*20 key, swtch, uniqswitch(1000)
      character*40 def
      character*145 var
      integer nline, l, ic, idef, ndef, start,i,ieq,nv, &
       ifile,lngth,first,ifound, imatch, iexe, is_mod_sw,ndefs
      integer iend, iscedes
      character*115 line
      character*99 scedes
      logical lexist
      
       
      nems=' '
      scedes=' '
! this next section is not used but could possibly be used to
! change local drive names to network drive names for file specified
! in the scedes.
      i=getenvqq('NEMS',NEMS)
      i=getenvqq('NEMSPC',NEMSPC)
      i=getenvqq('USERPC',USERPC)
      i=getenvqq('NEMSSHARE',NEMSSHARE)
      def=trim(NEMS)//'\logs\DEFAULTS'               ! use this for regular d:/default name
      NEMSPCDIR= '/' // trim(NEMSPC) // '/default'   ! use this for unc or lan names
      NEMSPCDIR=def                                  ! currently going with d:/default  
! end of this next section 

      CALL GETARG (1, scedes) 
      if(len_trim(scedes).eq.0) then
        iscedes=5
      else
        iscedes=4
        open(4,file=scedes,status='old',readonly)
      endif
       ndef=0

!   open file for invalid switch warnings
      inquire(exist=lexist,file='warn')
      if(lexist) then
       open(10,file='warn',status='old',access='append')
      else
       open(10,file='warn',status='new')
      endif

! read non-file key names from varkeys
      var=trim(nems)//'/scripts/varkeys'
      open(8,file=var,status='old',readonly)
      ndefs=0
991   read(8,'(a)',end=992) line
      call field(line,1,start,iend,' ')
      ieq=index(line,'=')
      if(ieq.eq.0) goto 991
      key=line(start:(ieq-1))
      val=line((ieq+1):iend)
      if(trim(val).ne.'?') then !"?" designates file
        ndefs=ndefs+1
        uniqswitch(ndefs)=key
      else
       ndef=ndef+1
       uniqfile(ndef)=line(start:(ieq-2))
      endif
      goto 991
992   close(8)
      i=0 
      nv=0
! Read scedes.file with lines like "keyname=value" where keyname is a key defined
! in varkeys read above.  For each key,
! create a string replacement command for sed.
! A line with a question mark indicate strings that need to be
! "looked up" in a version file.  They also flag the key as an
! input file key.  A corresponding path variable is
! created and assigned a substitute string
20    read(iscedes,'(a)',end=98) line
      ieq=index(line,'=')
      if(ieq.eq.0) write(10,'(3a)') '#WARNING, no key: ',trim(line)
      if(ieq.eq.0) goto 20
   
      key=line(1:(ieq-1)) 
      val=line((ieq+1):) 
      L=len_trim(key)-1
      uniq=key(1:L)
      call upper(uniq)
      ifile=0
      do idef=1,ndef
        if(uniq.eq.uniqfile(idef).and.len_trim(uniqfile(idef)).eq.L) then
          ifile=1
          exit
         endif
      enddo

      if(ifile.eq.1) then

!  if the Name key contains a slash, it is assumed to be a fully-qualified
!    file name, so the default version names are changed to null strings.                                                

        L=L+1
        type=key(L:L)
        CALL UPPER(TYPE)
        if(type.eq.'N') then
          lngth=len_trim(val)
          first=index(val,'/')
          call upper(uniq)
          if(first.gt.0) then
            write(6,'(2a)') trim(uniq),'D=nullstr'
          else
            ifound=0
            do idef=1,ndef
              if(uniq.eq.uniqfile(idef)) then
                ifound=1
              
              ! change the D string to null         
                write(6,'(2a)') trim(uniq),'D=nullstr'
                exit 
              endif
            enddo
            if (ifound.eq.0) then
! Symbolic name not found for this file - write a warning
             write (10,'(4a)') ' Symbolic name not found for file ',trim(uniq)
             val=''
            endif
          endif
! Strip a .obj suffix, as we add it from the shell
          if(index(val,'.obj').gt.0) then
            val=val(:(len_trim(val)-4))
          endif
          write(6,'(3a)') trim(uniq),'N=',trim(val) 
        endif !N
      
      else     ! of file=1
        iexe=0
        l=len_trim(key)       
        is_mod_sw=0
        if (l .eq. 3) then    
          ic=ichar(key(3:3))
 
          if(key(1:2).eq.'EX'.and. &
             (ic.ge.65.and.ic.le.90).or. &
             (ic.ge.97.and.ic.le.122)) is_mod_sw=1 
        endif 
        if(is_mod_sw.eq.1)then
       
           call upper(key)
           if(val.eq.'nulline') then
             write(6,'(2a)') trim(key),'=nulline'
           elseif(val.eq.'nullstr') then 
             write(6,'(3a)') trim(key),'=',trim(val)
           else
             write(6,'(3a)') trim(key),'=',trim(val)
           endif   
           key=key(3:3)
           if(val.eq.'1') then
             val='nullstr'
           else
             val='nulline'
           endif
           ifile=1
        else
           swtch=key
           call upper(swtch)
           ifile=0
           do idef=1,ndefs
             if(swtch.eq.uniqswitch(idef)) then
               ifile=1
               exit
             endif
           enddo
           if(ifile.eq.0) then
             write(10,'(3a)') '#WARNING: ',trim(swtch),' is not a recognized key'
           endif  
        endif

        if (ifile .eq. 1) then
          call upper(key)
          if(val.eq.'nulline'.or.val.eq.'NULLINE') then
            write(6,'(2a)') trim(key),'=nulline'
          elseif (val .eq.'nullstr'.or.val.eq.'NULLSTR') then
            write(6,'(2a)') trim(key),'=nullstr'
          else
            write(6,'(3a)') trim(key),'=',trim(val)
          endif
        endif
      endif
      goto 20  ! go back and read another line
98    continue
      stop
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
      subroutine field(a,n,start,end,fldsep)
      implicit none
! find starting and end column position of Nth field in a string, comma,elimited
      integer ib(10),ie(10)
      character*(*) a
      character*1 fldsep
      integer i ,n, fldnum, j, L
      logical inblank
      integer start,end
      L=len(a)
      start=1
      end=L
      inblank=.true.
      fldnum=1
      do j=1,L
        
        if(inblank .and.a(j:j).ne.fldsep) then
           start=j
           inblank=.false.
        elseif(.not.inblank .and. a(j:j).eq.fldsep) then
           end=j-1
           inblank=.true.
           if(fldnum.eq.n) return
           fldnum=fldnum+1
        elseif(j.eq.l.and. .not. inblank) then   
           end=j
              
        endif
       enddo
       return
       end
! ===============================================
       subroutine replace(string,finds,repls)
       implicit none
       character*(*) string, finds, repls
       character*255 temp
       integer L,LF,LR,I
       l=len(string)
       lf=len(finds)
       lr=len(repls)
       i=1
10     continue
         if(string(i:(i+LF-1)).eq.finds) then
           temp=string((i+Lf):)
           string(i:(i+LR-1))=repls
           string((i+LR):)=trim(temp)
           l=len(string)
           i=i+LR-1
         endif
         i=i+1
       if(i.le.L-LF+1) goto 10
       return
       end
