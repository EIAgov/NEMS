! $Header: M:\\default\\source/RCS/cio4wk1.f,v 1.5 2009/02/18 17:14:26 dgr Exp $
!******************************************************
      function cwkopen (wkfile)
      character*(*) wkfile

      integer cwk1unit
      common/ciowk1/cwk1unit
      INTEGER  CWKOPEN   !FOR RS6000
      cwkopen=0
      cwk1unit=899
      OPEN(UNIT=cwk1unit,FILE=wkfile,FORM='BINARY', &
           ACCESS='SEQUENTIAL')
      end
!******************************************************
      function cwkopen2(wkfile)
      character*(*) wkfile

      integer cwk1unit
      common/ciowk1/cwk1unit
      INTEGER  CWKOPEN2   !FOR RS6000
      cwkopen2=0
      cwk1unit=890
      OPEN(UNIT=cwk1unit,FILE=wkfile,FORM='BINARY', &
           ACCESS='SEQUENTIAL',action='read')
      end
!******************************************************
      function cwkclose()
      INTEGER  CWKCLOSE   !FOR RS6000
      integer cwk1unit
      common/ciowk1/cwk1unit
      cwkclose=0
      close(cwk1unit)
      end
!******************************************************
      function cwkclosin()
      integer cwkclosin
      INTEGER  CWKCLOSE   !FOR RS6000
      integer cwk1unit
      common/ciowk1/cwk1unit
      cwkclosin=0
      close(cwk1unit)
      end
!******************************************************
      function cwkwrite(a1)
      character*1 a1
      integer cwk1unit
      common/ciowk1/cwk1unit
      INTEGER  CWKWRITE   !FOR RS6000
      cwkwrite=0
      write(cwk1unit) a1
      end
!******************************************************
      function cwkread(a1,n,eofstat)
      character*1 a1(n)
      integer cwk1unit,eofstat
      common/ciowk1/cwk1unit
      INTEGER  CWKread   !FOR RS6000
      cwkread=0
      eofstat=0
      read(cwk1unit,end=99,err=99,iostat=eofstat) a1
99    continue
      end
!******************************************************
      function cwkreada(a,n,eofstat)
      integer cwkreada,eofstat
      character*1 a(n)
      integer cwk1unit
      common/ciowk1/cwk1unit
      cwkreada=0
      eofstat=0
      read(cwk1unit,end=99,err=99,iostat=eofstat) a
99    continue
      end
!******************************************************
      function cwkreadi(IVAL,n,eofstat)
      integer cwkreadi,eofstat
      integer*2 ival
      integer cwk1unit
      common/ciowk1/cwk1unit
      INTEGER  CWKread   !FOR RS6000
      cwkreadi=0
      eofstat=0
      read(cwk1unit,end=99,err=99,iostat=eofstat) ival
99    continue
      end
!******************************************************
      function cwkreadf(RVAL,n,eofstat)
      integer cwkreadf,eofstat
      real*8 rval
      integer cwk1unit
      common/ciowk1/cwk1unit
      INTEGER  CWKread   !FOR RS6000
      cwkread=0
      eofstat=0
      cwkreadf=0
      read(cwk1unit,end=99,err=99,iostat=eofstat) rval
99    continue
      end
!******************************************************
      function cwkwrite2(a)
      character*(*) a
      integer ilen, iweird
      integer cwk1unit
      common/ciowk1/cwk1unit
      INTEGER  CWKWRITE2   !FOR RS6000
      cwkwrite2=0
      ilen=len(a)
      iweird=index(a,'\0')
      if(iweird.gt.0) then
        write(cwk1unit) a(1:iweird-1)
!     1 ,a(iweird+2:ilen)
      else
        write(cwk1unit) a
      endif
      end
