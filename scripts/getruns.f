      program getruns
      use dflib
      implicit none
!  reads runlog to find latest carbon runs from paul, copies the csv
!  files to a new, daily directory.
      integer i,nruns,l,leng,allday,idat,j,system,iret,idrive,ncsv
      character*133 line,dvers*5,drivelet*1,drive(3)*1
      external leng
      character*70 cmd,runs(250),rundat(250)*6,newdir*5,filen*80
      character*25 nems
      character*36 logfilename
      character*20 csvnames(0:50),ans*1,ans4*4
      integer order(250),islash,runn(0:50)
      integer iyr,imon,iday,jmon,jday,n
      character*4 today,yesterday
      logical goodorder

      call get_dat(iyr,imon,iday)
      write(today,'(2i2.2)') imon,iday
      write(6,*) 'Today: ',today
      jday=iday-1
      jmon=imon
      if(jday.eq.0) jmon=jmon-1
      if(jday.eq.0) then
         jday=31
         if(jmon.eq.2) jday=28
         if(jmon.eq.4) jday=30
         if(jmon.eq.6) jday=30
         if(jmon.eq.9) jday=30
         if(jmon.eq.11) jday=30
      endif 
      write(yesterday,'(2i2.2)') jmon,jday
      write(6,*) 'Yesterday: ',yesterday
      ans4=' '
      write(6,*) &
        'Normally, yesterday runs copied. Override, enter mmdd:'
      read(5,'(a4)') ans4
      if(ans4.ne.' ') yesterday=ans4 
      write(6,*) 'RunDay: ',yesterday

      runn(0)=0
      csvnames(0)=' '
      drive(1)='C'
      drive(2)='D'
      drive(3)='E'
      i=getenvqq('NEMS',nems)
      logfilename= nems(1:leng(nems)) // '/logs/runlog'
      cmd='grep pkc ' // logfilename // '| tail -250 >cmd.out'
      iret=system(cmd)
      open(8,file='cmd.out',status='old')
      nruns=0     
      do i=1,250
        read(8,'(4x,a)',end=99) runs(i)
        nruns=nruns+1
      enddo
99    continue
      close(8,status='delete')
!    look for most recent runs
      allday=1
      do i=nruns,1,-1 
         l=leng(runs(i))
         idat=l
         do j=l,1,-1
           if(runs(i)(j:j).eq.'/') then
            go to 98
           endif
           idat=j
         enddo
98         continue
         if(idat.lt.l) then
           idat=idat+1
           rundat(i)=runs(i)(idat:idat+6)
           
         endif
         if(i.lt.nruns) then
           if(rundat(i).ne.rundat(i+1).and. &
              allday.eq.1.and.rundat(i+1)(1:4).eq.yesterday) then
             allday=i+1
             write(6,*) ' allday=',allday
             write(6,*) '  first run=',runs(allday)
           endif
         endif
      enddo
      n=nruns
      do i=n,allday,-1
        if(rundat(i)(1:4).ne.yesterday) then
          nruns=nruns-1
        endif
      enddo
      
      write(6,*) 'Copy commands for csv files'
      newdir='d'//rundat(nruns)(1:4)
      do i=allday,nruns
        l=leng(runs(i))
        runs(i)='cp '//runs(i)(1:l)//'/*.ran '//newdir
        write(6,'(2a)') runs(i)(:leng(runs(i)))
      enddo
      write(6,*) ' '
      write(6,'(a\)') 'Hit Enter to Copy Files; Ctrl-C to break: '
! make new directory off root
 
      read(5,'(a)')
      cmd='mkdir '//newdir
      write(6,*) cmd(:leng(cmd))
      iret=system(cmd)
      do i=allday,nruns
        cmd=runs(i)
        write(6,*) cmd(:leng(cmd))
        iret=system(cmd)
      enddo
! 
      dvers=newdir
      cmd='ls '//newdir//'/*.ran > '//newdir//'/csvfiles'
      iret=system(cmd)
      write(6,'(a,i4,a,$)') 'Return code:',iret,' (hit enter): '
      read(5,'(a)')ans
      filen=newdir//'/csvfiles'
      open(8,file=filen,status='old')
      ncsv=0
      islash=1
      do i=1,100
        read(8,'(a)',end=998) line
        do j=leng(line),1,-1
          if(line(j:j).eq.'/') then
            islash=j+1
            exit
          endif
        enddo
        csvnames(i)=line(islash:)
        
        ncsv=ncsv+1
        runn(i)=i
        order(i)=i
      enddo
998   continue
      close(8,status='keep')    
      goodorder=.false.
      do while(.not. goodorder) 
        write(6,*) 'Order of CSV Files'
        write(6,*) 'Run#   Csv File     Order'
        do i=1,ncsv
          write(6,'(i3,1x,a,i2)') runn(order(i)), &
                              csvnames(order(i)), &
                                       i
        enddo
        write(6,'(a,$)') 'Is this order ok [y/n]: '
        read(5,'(a)') ans
        if(ans.eq.'y'.or.ans.eq.'Y') then
           goodorder=.true.
        else
           do i=1,ncsv
             write(6,'(a,i3,a,$)') 'What run number for place ',i,'? :'
             read(5,'(i3)') order(i)
           enddo
        endif
      enddo
      filen=newdir//'/filelist.txt'
      open(9,file=filen,status='replace')
      do i=1,ncsv
        if(order(i).ne.0) then
          write(9,'(a)') newdir//'\'//csvnames(order(i))
        endif
      enddo
      filen=newdir//'/copydir.bat'
      open(9,file=filen,status='replace')
      write(9,'(a)') 'set copydir='//newdir
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
      subroutine replace(line,find,repl)
! replaces string find with repl in line
      implicit none
      integer i,lf,lr,leng,ib,ie
      character*133 newline
      character*(*) line
      character*(*) find
      character*(*) repl
      lf=leng(find)
      lr=leng(repl)
      i=1
      do while(i.ne.0.and.find.ne.repl)
        i=index(line,find)
        ib=i-1
        ie=i+lf
        if(i.gt.0) then
          if(ib.gt.0) then  
            newline=line(1:ib)//repl//line(ie:)
          else
            newline=repl//line(ie:)
          endif
          line=newline
        endif
       enddo
       return
       end
      subroutine get_dat( iyr,imon,iday)
      integer iyr,imon,iday
      integer values(8)
      character zone*5,date*8,time*10
      call date_and_time(date,time,zone,values)
      iyr=values(1)
      imon=values(2)
      iday=values(3)
      return
      end
