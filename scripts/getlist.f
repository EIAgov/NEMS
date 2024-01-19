      program getruns
      implicit none
!  reads runlog to find latest carbon runs from paul, copies the csv
!  files to a new, daily directory.
      integer i,nruns,l,leng,allday,idat,j,system,iret,idrive,ncsv
      character*133 line,dvers*15,drivelet*1,drive(3)*1
      external leng
      character*70 cmd,runs(250),rundat(250)*6,newdir*15,filen*80
      character*20 csvnames(0:50),ans*1,ans4*4
      integer order(250),islash,runn(0:50)
      integer iyr,imon,iday,jmon,jday,n
      character*4 today,yesterday
      logical goodorder

      drive(1)='C'
      drive(2)='D'
      drive(3)='E'
! 
      write(6,'(a,$)') ' Enter Name of Directory with RANs: '
      read(5,'(a)') newdir
      write(6,'(a,$)') ' Enter Name of Directory on PC: '
      read(5,'(a)') dvers 
      cmd='ls '//newdir(:leng(newdir))//'/*.ran > ' &
               //newdir(:leng(newdir))//'/csvfiles'
      iret=system(cmd)
      write(6,'(a,i4,a,$)') 'Return code:',iret,' (hit enter): '
      read(5,'(a)')ans
      filen=newdir(:leng(newdir))//'/csvfiles'
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
      filen=newdir(:leng(newdir))// &
      '/filelist.txt'
      open(9,file=filen,status='replace')
      do i=1,ncsv
        if(order(i).ne.0) then
          write(9,'(a)') dvers(:leng(dvers))// &
          '\'//csvnames(order(i))
        endif
      enddo
      close(9)
      filen=newdir(:leng(newdir))//'/copydir.bat'
      open(9,file=filen,status='replace')
      write(9,'(a)') 'set copydir='//dvers(:leng(dvers))
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

