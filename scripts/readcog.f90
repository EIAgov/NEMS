! this reads output of a industrial model run from nohup.out when run with "prtdbgi=3".
! it is best just to name the nohup file as the input to this program, rather than to
! run the listcog.sh script
      implicit none
      integer maxl,i, imx, ibyr, ieyr
      parameter(ibyr=2010) ! Base year
      parameter(ieyr=2040) ! End year
      parameter(imx=21)
      parameter(maxl=9) ! number of steam load segments + 1 for total
      integer year, ind, reg, il
!                          1  2  3  4  5  6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
      integer igroup(imx)/ 7, 7, 7, 7, 7, 7,1,2,3, 7, 7, 4, 7, 5, 5, 5, 5, 5, 6, 6, 6/
      character*15 gname(8)
      character*11 rname(5)/ '"Northeast"','"Midwest"','"South"', &
       '"West"','"US"'/

      real sumr(5)
      real suml(maxl)
      real sumpot(maxl,ibyr:ieyr)
      real sumpot2(maxl,ibyr:ieyr)
      real spayback(maxl,ibyr:ieyr)
      real sumepot(maxl,ibyr:ieyr)
      real sumepot2(maxl,ibyr:ieyr)
      real spayback2(maxl,ibyr:ieyr)

      real techpot(maxl-1,4,imx,ibyr:ieyr),tp
      real econpot(maxl-1,4,imx,ibyr:ieyr),ep
      real econfrac(maxl-1,4,imx,ibyr:ieyr),ef
      real payback(maxl-1,4,imx,ibyr:ieyr),pb

      real ssteam(8,ibyr-5:ieyr),stemcur
      real scogstm(8,ibyr-5:ieyr),cogsteam
      real bsteam(8,ibyr-5:ieyr),biosteam
      real snoncog(8,ibyr-5:ieyr),noncogfossteam

      real ssteamR(5,ibyr-5:ieyr)
      real scogstmR(5,ibyr-5:ieyr)
      real bsteamR(5,ibyr-5:ieyr)
      real snoncogR(5,ibyr-5:ieyr)
      character*133 line,filen*60,newline
      integer leng

      gname( 1)='"Food"'
      gname( 2)='"Paper"'
      gname( 3)='"Chem"'
      gname( 4)='"Steel"'
      gname( 5)='"Metal-Based"'
      gname( 6)='"Other nonInt"'
      gname( 7)='"NonManufact"'
      gname( 8)='"  Total"'
      write(*,100) ' Enter input file: '
      read(5,101) filen
      open(8,file=filen,status='old')
      write(*,100) ' Enter output file: '
      read(5,101) filen
      open(9,file=filen,status='unknown')
      sumpot=0.
      sumepot=0.
      sumpot2=0.
      sumepot2=0.
      spayback=0.
      spayback2=0.
      techpot=0.
      econpot=0.
      econfrac=0.
      payback=0.
      ssteam=0.
      scogstm=0.
      snoncog=0.
      ssteamR=0.
      scogstmR=0.
      snoncogR=0.
10    continue
        read(8,'(a)',end=99) line
        if(index(line,'steam').eq.1) then

          read(line,'(5x,i5,i3,1x,i1,4f10.2)') year,ind,reg,  &
           stemcur,cogsteam,biosteam,noncogfossteam
           if(cogsteam.gt.stemcur) then
              !write(6,'(a)') line
              cogsteam=stemcur
              biosteam=0.
              noncogfossteam=0. 
           endif
           if(cogsteam+biosteam+noncogfossteam.gt.stemcur+2.) then
              write(6,'(a,2f10.2)') 'steam components>stemcur:',cogsteam+biosteam+noncogfossteam,stemcur
              write(6,'(a)') line
           endif
           ssteam(igroup(ind),year)=ssteam(igroup(ind),year)+stemcur
           scogstm(igroup(ind),year)=scogstm(igroup(ind),year)+cogsteam
           bsteam(igroup(ind),year)=bsteam(igroup(ind),year)+biosteam
           snoncog(igroup(ind),year)=snoncog(igroup(ind),year)+ noncogfossteam

           ssteam(8,year)=ssteam(8,year)+stemcur
           scogstm(8,year)=scogstm(8,year)+cogsteam
           bsteam(8,year)=bsteam(8,year)+biosteam
           snoncog(8,year)=snoncog(8,year)+noncogfossteam

           ssteamR(reg,year)=ssteamR(reg,year)+stemcur
           scogstmR(reg,year)=scogstmR(reg,year)+cogsteam
           bsteamR(reg,year)=bsteamR(reg,year)+biosteam
           snoncogR(reg,year)=snoncogR(reg,year)+noncogfossteam

           ssteamR(5,year)=ssteamR(5,year)+stemcur
           scogstmR(5,year)=scogstmR(5,year)+cogsteam
           bsteamR(5,year)=bsteamR(5,year)+biosteam
           snoncogR(5,year)=snoncogR(5,year)+noncogfossteam
        endif
        if(line(1:6).ne.'cogen2') then
           goto 10
        endif
        read(line,'(5x,i4,i3,2x,i1,i2,4f10.3)') year, ind,reg,il,tp,ep,ef,pb
        if(il.ne.maxl) then
           techpot(il,reg,ind,year)=tp
           sumpot(il,year)=sumpot(il,year)+tp
           sumpot( maxl,year)=sumpot(maxl,year)+tp
           sumpot2(reg,year)=sumpot2(reg,year)+tp
           sumpot2(5,year)=sumpot2(5,year)+tp
           econpot(il,reg,ind,year)=ep
           sumepot(il,year)=sumepot(il,year)+ep
           sumepot( maxl,year)=sumepot(maxl,year)+ep
           sumepot2(reg,year)=sumepot2(reg,year)+ep
           sumepot2(5,year)=sumepot2(5,year)+ep
           econfrac(il,reg,ind,year)=ef
           payback(il,reg,ind,year)=pb
           spayback(il,year)=spayback(il,year)+pb*tp
           spayback( maxl,year)=spayback(maxl,year)+pb*tp
           spayback2(reg,year)=spayback2(reg,year)+pb*tp
           spayback2(  5,year)=spayback2(  5,year)+pb*tp
        endif
        newline=line
        go to 10
99    continue
100   format(a,$)
101   format(a)

      write(9,*)' "Average CHP Payback Periods by Load Segment"'
      write(9,'(1x,a4,9a11)') '" "','"1.5--3"','"3--6"','"6--10"', '"10--50"','"50--100"','"100--250"', '"250--500"','"+500"','"All"'
      write(9,'(1x,a4,9a11)') '" "','"800"','" 3,000"','" 3,000"','" 5,000"','" 10,000"','" 25,000"','" 40,000"','" 100,000"'
      do year=ibyr,ieyr
        write(9,'(i5,9f11.1)') year, (spayback(il,year)/max(.001,sumpot(il,year)),il=1,maxl)
      enddo
      write(9,*)' "Average CHP Payback Periods by Census Region"'
       write(9,'(1x,a4,5a11)') '" " ','"Northeast"','"Midwest"', '"South"','"West"','"US"'
      do year=ibyr,ieyr
        write(9,'(i5,5f11.1)') year, (spayback2(reg,year)/max(.001,sumpot2(reg,year)),reg=1,5)
      enddo

      write(9,*)' "CHP Technical Potential by Load Segment"'
      write(9,'(1x,a4,9a11)') '" "','"1.5--3"','"3--6"','"6--10"', '"10-5-0"','"50--100"','"100--250"', '"250-5-00"','"+500"','"All"'
      write(9,'(1x,a4,9a11)') '" "','"800"','" 3,000"','" 3,000"','" 5,000"','" 10,000"','" 25,000"','" 40,000"','" 100,000"'
      do year=ibyr,ieyr
        write(9,'(i5,9f11.0)') year, (sumpot(il,year),il=1,maxl)
      enddo

      write(9,*)' "CHP Technical Potential by Census Region"'
       write(9,'(1x,a4,5a11)') '" " ','"Northeast"','"Midwest"', '"South"','"West"','"US"'
      do year=ibyr,ieyr
        write(9,'(i5,5f11.0)') year, (sumpot2(reg,year),reg=1,5)
      enddo

      write(9,*)' "CHP Econonmic Potential by Load Segment"'
      write(9,'(1x,a4,9a11)') '" "','"1.5--3"','"3--6"','"6--10"', '"10-5-0"','"50--100"','"100--250"', '"250-5-00"','"+500"','"All"'
      write(9,'(1x,a4,9a11)') '" "','"800"','" 3,000"','" 3,000"','" 5,000"','" 10,000"','" 25,000"','" 40,000"','" 100,000"'
      do year=ibyr,ieyr
        write(9,'(i5,9f11.0)') year, (sumepot(il,year),il=1,maxl)
      enddo
     
      write(9,*)' "CHP Economic Potential by Census Region"'
       write(9,'(1x,a4,5a11)') '" " ','"Northeast"','"Midwest"', '"South"','"West"','"US"'
      do year=ibyr,ieyr
        write(9,'(i5,5f11.0)') year, (sumepot2(reg,year),reg=1,5)
      enddo

      write(9,*)' "CHP Cumulative Additions by Load Segment"'
      write(9,'(1x,a4,9a11)') '" "','"1.5--3"','"3--6"','"6--10"', '"10-5-0"','"50--100"','"100--250"', '"250-5-00"','"+500"','"All"'
      write(9,'(1x,a4,9a11)') '" "','"800"','" 3,000"','" 3,000"','" 5,000"','" 10,000"','" 25,000"','" 40,000"','" 100,000"'
      do year=ibyr,ieyr
        suml=0.
        do il=1,maxl
          do i=ibyr,year
            suml(il)=suml(il)+sumepot(il,i)*.05
          enddo
        enddo
        write(9,'(i5,9f11.0)') year,(suml(il),il=1,maxl)
      enddo

      write(9,*)' "CHP Cumulative Additions by Census Regions"'
       write(9,'(1x,a4,5a11)') '" " ','"Northeast"','"Midwest"', '"South"','"West"','"US"'
      do year=ibyr,ieyr
        sumr=0.
        do reg=1,5
        do i=ibyr,year
          sumr(reg)=sumr(reg)+sumepot2(reg,i)*.05
        enddo
        enddo
        write(9,'(i5,9f10.0)') year, (sumr(reg),reg=1,5)
      enddo
      
       write(9,'(/a)') ' "Industrial Steam by Industry"'
         write(9,'(a,12X,32i10)') '" "',(year,year=ibyr,ieyr)
        do ind=1,8
          write(9,'(a15,32f10.1)') gname(ind),(ssteam(ind,year), year=ibyr,ieyr)
        enddo
      write(9,'(/a)') ' "Industrial Cogenerated Steam by Industry"'
      write(9,'(15X,32i10)') (year,year=ibyr,ieyr)
         write(9,'(a,12X,32i10)') '" "',(year,year=ibyr,ieyr)
        do ind=1,8
          write(9,'(a15,32f10.1)') gname(ind),(scogstm(ind,year), year=ibyr,ieyr)
        enddo
      write(9,'(/a)') &
       ' "Industrial NonCogen Biofueled Steam by Industry"'
         write(9,'(a,12X,32i10)') '" "',(year,year=ibyr,ieyr)
        do ind=1,8
          write(9,'(a15,32f10.1)') gname(ind),(bsteam(ind,year), year=ibyr,ieyr)
        enddo
      write(9,'(/a)') ' "Industrial NonCogen Fossil Steam by Industry"'
         write(9,'(a,12X,32i10)') '" "',(year,year=ibyr,ieyr)
        do ind=1,8
          write(9,'(a15,32f10.1)') gname(ind),(snoncog(ind,year), year=ibyr,ieyr)
        enddo
      write(9,'(/a)') ' "Industrial Steam by Region"'
         write(9,'(a,12X,32i10)') '" "',(year,year=ibyr,ieyr)
        do reg=1,5
          write(9,'(a15,32f10.1)') rname(reg),(ssteamR(reg,year), year=ibyr,ieyr)
        enddo
      write(9,'(/a)') ' "Industrial Cogenerated Steam by Region"'
         write(9,'(a,12X,32i10)') '" "',(year,year=ibyr,ieyr)
        do reg=1,5
          write(9,'(a15,32f10.1)') rname(reg),(scogstmR(reg,year), year=ibyr,ieyr)
        enddo
      write(9,'(/a)') ' "Industrial NonCogen Biofueled Steam by Region"'
         write(9,'(a,12X,32i10)') '" "',(year,year=ibyr,ieyr)
        do reg=1,5
          write(9,'(a15,32f10.1)') rname(reg),(bsteamR(reg,year), year=ibyr,ieyr)
        enddo
      write(9,'(/a)') ' "Industrial NonCogen Fossil Steam by Region"'
         write(9,'(a,12X,32i10)') '" "',(year,year=ibyr,ieyr)
        do reg=1,5
          write(9,'(a15,32f10.1)') rname(reg),(snoncogR(reg,year), year=ibyr,ieyr)
        enddo
      end
      FUNCTION LENG(A)
       IMPLICIT NONE
!     FINDS POSITION OF LAST NON-BLANK CHARACTER IN CHARACTER VARIABLE A
      CHARACTER A*(*)
      INTEGER LENG,I,N
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

