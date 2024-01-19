! $Header: m:/default/source/RCS/readmat.f90,v 1.3 2017/12/10 14:25:53 DSA Exp AKN $
! reads coefficients from pack_all files like "EC_2016_MAT.TXT and AIMMS interface debug files like ecpcoeff_2016.txt and compares side by side.
! files are assumed to be sorted by oml column, then oml row, and copied to generically-named files.
! 
!
! cmd to sort ec_2016_mat.txt:               sort -ad -t":" -k 3 -k 4 ec_2016_mat.txt > omlmat.txt
! cmd to sort ecpcoeff_2016.txt:             grep COEFF1 ecpcoeff_2016.txt | sort -ad -t":" -k 2 -k 3 > aimmat.txt
! or if  using output of readaimlis program: cat aimmat_unsorted.txt | sort -ad > aimmat.txt

! to exclude OML free rows, which are not in the aimms file, provide a list from the OML mps file using 
! this command "grep '^ N  ' emm_2016.mps > freerows.txt"

! this version only writes differnces after rounding
implicit none
integer,parameter :: mc=2500000
real*8 matoml(mc),mataim(mc)
character*17 coloml(0:mc),colaim(0:mc)
integer cntoml,i,cntaim,j,iunit,namfield,ifree,nfree,k,sngdgt
logical lexist
real(8) digits2 ! function

character*200 line,fmt*25
integer,parameter :: maxfree=20000
character*8 free(maxfree)
write(6,'(a\)') 'Enter number of digits to round coefficients, from to 3-9, or enter 10 to not round:'
read(5,*) sngdgt
if(sngdgt.eq.0) sngdgt=10
if(sngdgt.le.2) sngdgt=3
write(6,'(a,i3)') 'sngdgt=',sngdgt






open(8,file='aimmat.txt',status='old')
read(8,'(a)') line
rewind 8
if(len_trim(line).gt.165) then
  fmt='(7x,a9,1x,a8,1x,f16.0)'
else
  fmt='(7x,a9,1x,a8,1x,f26.0)'
! if reading cplex mps file, won't have free rows. read list to ignore  
  inquire(file='freerows.txt',exist=lexist)
  if(lexist) then
    write(6,'(a)')' reading list of free rows to ignore from freerows.txt, excluding ECPCOSTS or EFDCOSTS'
    open(3,file='freerows.txt',status='old')
    40 continue
       nfree=nfree+1
       read(3,'(4x,a8)',end=48) free(nfree)
       if(free(nfree).eq.'ECPCOSTS'.or.free(nfree).eq.'EFDCOSTS') nfree=nfree-1
       if(nfree.lt.maxfree) go to 40
       nfree=nfree+1
       write(6,'(a,i7,1x,a)') ' more than ',maxfree,' free rows. ignoring rest'
    48 continue
      nfree=nfree-1
      write(6,*)'number of free rows in freerows.txt: ',nfree
  else
    write(6,*)' no freerows.txt file found, so will compare all coefficients'
    nfree=0
  endif

endif  
colaim(0)='dummy'
! read aimmat.txt with oml name translation
i=1
20 continue
    read(8,fmt,end=98) colaim(i)(1:9),colaim(i)(10:17), mataim(i)
    !if(mataim(i).ne.0.  .and. colaim(i).ne.colaim(i-1) .and. colaim(i)(10:14).ne.'BOUND' .and. colaim(i)(1:4).ne.'RHSE' )then !omit zeroes, duplicates, bounds, and RHSECP column
    if(mataim(i).ne.0.  .and. colaim(i).ne.colaim(i-1) .and. colaim(i)(10:14).ne.'BOUND' )then !omit zeroes, duplicates, and bounds   modified by AKN to read RHSECP and RHSEFD 
      i=i+1
      if(i.gt.mc) then
        write(6,*) ' too many coefficients. increase parameter "mc" in readmat.f90'
        stop
      endif
    endif
    goto 20
98 continue
   cntaim=i-1
close(8)

open(8,file='omlmat.txt',status='old')
i=1
10 continue
    read(8,'(8x,a8,8x,a9,9x,E30.3)',end=99) coloml(i)(1:8),coloml(i)(9:17), matoml(i)
    if (trim(coloml(i)(9:17)) .eq. 'UP' .OR. trim(coloml(i)(9:17)) .eq. 'LO' .OR. trim(coloml(i)(9:17)) .eq. 'FX') then
        backspace(8)
        read(8,'(8x,a8,7x,a9,9x,E30.3)',end=99) coloml(i)(1:8),coloml(i)(9:17), matoml(i)
    endif
    ifree=0
    do k=1,nfree
      if(coloml(i)(10:17).eq.free(k)) then
        ifree=1
        exit
      endif
    enddo
    if(ifree.eq.0) then
      i=i+1
      if(i.gt.mc) then
        write(6,*) ' too many coefficients. increase parameter "mc" in readmat.f90'
        stop
      endif
    endif
   goto 10
99 continue
   cntoml=i-1
close(8)

write(6,'(a,i8)') 'number of coefficients in omlmat.txt=',cntoml
write(6,'(a,i8)') 'number of coefficients in aimmat.txt=',cntaim



iunit=10
open(10,file='matout.txt',status='unknown')
rewind 10
write(10,'(a17,3x,a17,a20,a20)')  'OML Column:row','AIMMS Column:row',' OML Coefficient','AIMMS Coefficient'

    i=1
    j=1
100 continue
    if(       coloml(j).lt.colaim(i)   ) then
      if(matoml(j).ne.0.) then
         matoml(j)=digits2(matoml(j),sngdgt)
         write(iunit,'(a17,3x,17x,f20.8)')     coloml(j),matoml(j)
 !        write(    6,'(a17,3x,17x,f20.8)')     coloml(j),matoml(j)
      endif
      if(j.lt.cntoml) j=j+1
    elseif(   coloml(j).gt.colaim(i)  )then
      if(mataim(i).ne.0.) then
        mataim(i)=digits2(mataim(i),sngdgt)
         write(iunit,'(17x,3x,a17,20x,F20.8)') colaim(i),mataim(i)
 !        write(    6,'(17x,3x,a17,20x,F20.8)') colaim(i),mataim(i)
      endif
      if(i.lt.cntaim) i=i+1
    elseif(coloml(j).eq.colaim(i)) then
      if(matoml(j).ne. 0.0 .or. mataim(i).ne. 0.0) then
        matoml(j)=digits2(matoml(j),sngdgt)
        mataim(i)=digits2(mataim(i),sngdgt)
        if(matoml(j)-mataim(i).gt. 0.00000001) then
          write(iunit,'(a17,3x,a17,2f20.8)')    coloml(j),colaim(i),matoml(j),mataim(i)
        endif
      endif
!      if(mod(j,10000).eq.0) then
!        write(6,  '(a17,3x,a17,2f20.8)')    coloml(j),colaim(i),matoml(j),mataim(i)
!      endif
      if(i.lt.cntaim) i=i+1
      if(j.lt.cntoml) j=j+1
    else
      write(6,  '(a,a17,3x,a17,2f20.8)')   'err? ', coloml(j),colaim(i),matoml(j),mataim(i)
	
	endif

    if(i.ne.cntaim  .and. j.ne.cntoml) goto 100
    
    
    if(i.lt.cntaim) then
      do i=i+1,cntaim
         write(iunit,'(17x,3x,a17,20x,F20.8)') colaim(i),mataim(i)
      enddo
    endif
    if(j.lt.cntoml) then
      do j=j+1,cntoml
         write(iunit,'(a17,3x,17x,f20.8)')     coloml(j),matoml(j)
      enddo
    endif

    write(6,*) 'matout.txt with differences created'
    stop
    end
    
    
    !
!
!     DIGITS2 SETS SIGNIFICANT DIGITS OF REAL*8 NUMBER
!
      FUNCTION DIGITS2(VALUE,SGNDGT)
!
      REAL*8 DIGITS2,VALUE,FACTOR
      INTEGER*4 SGNDGT
      INTEGER*4 DEC
      INTEGER*4 IPOWER
!
!  test to omit these conversions
!  this is normally called with sgndgt=5 from ecp, =6 from efd:
      if (sgndgt .ge. 10) then
        digits2=value
      else
    
        FACTOR = DBLE(0.0)
        IF (VALUE .EQ. FACTOR) THEN
           DIGITS2 = VALUE
        ELSE
          IF (ABS(VALUE) .GT. 1) THEN
            IPOWER = LOG10( ABS( VALUE ))
            DEC = MAX( 0 , SGNDGT - IPOWER - 1 )
          ELSE
            DEC = SGNDGT
          END IF
          FACTOR = DBLE(10) ** DEC
          DIGITS2 = DBLE( ANINT( VALUE * FACTOR )) / FACTOR
        END IF
!
      end if
      END
