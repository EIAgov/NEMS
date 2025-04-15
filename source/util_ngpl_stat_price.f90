subroutine NGPL_STAT_PRICE
use ifport, only: GETDRIVEDIRQQ,FILE$CURDRIVE, $MAXPATH
implicit none
! invokes a python (previously used R) procedure to estimate propane and ethane prices, aka NGPLs
    include 'parametr'
    include 'ncntrl'
    include 'mpblk'
    include 'qblk'
    include 'macout'
    include 'ngtdmrep'
    include 'intout' ! wti_price
    include 'pmmout'
    include 'convfact'
    integer :: runit, i, ir, iy, iyn, nemsyr, iy2011

    integer regyear ! for regionalizing historical propane price data

    character*80 line
    character*120 rcmd
    character*12 NGPL_Price
    character*15 RLOC
    character*80 RTERM
    character*15 dummy
    CHARACTER*40 NEMSPYENV
    CHARACTER*40 CURIRUNSTR


    integer :: iyear,colA,iret
    real :: colB,colC,colD,colE,colF,colG,colH,colI,colJ,colK,colL,colM,colN,colO,colP,colQ

! variables and data in 4 lines below are for breaking out CD-level prices
    real resiweights(1:9)/0.172,0.161,-0.102,-0.183,0.111,0.0582,0.0385,-0.0416,0.120/
    real commweights(1:9)/0.0188,0.0753,-0.0797,-0.121,0.0369,-0.0164,-0.0210,-0.00942,0.0487/
    real tranweights(1:9)/-0.0233,-0.0159,0.00396,-0.0106,0.0254,0.0200,-0.00514,-0.0240,-0.000260/
    real resinat, commnat, trannat

    real lpgbase ! base propane feedstock (Mt. Belvieu) price = PPRINPF
    real rs_prcoef(3), cm_prcoef(3), in_prcoef(3), tr_prcoef(3)
    integer ipropco
    integer icd  ! CD counter for Census Districts

    real :: bar_per_gal

    CHARACTER($MAXPATH) dir
    INTEGER(4) ldir

! +++ Variables to Call FILE_MGR for Initialization
    INTEGER IROUTDBG
    LOGICAL NEW/.TRUE./
    INTEGER FILE_MGR
    INTEGER RTOVALUE           ! FUNCTION TO READ RUN TIME OPTIONS
    EXTERNAL FILE_MGR, RTOVALUE
    INTEGER*4 AEOLSTYR   ! run-time option set to calendar year for last year of AEO projection

    iy2011=2011-baseyr+1

!  put in most recent historical years this way to take precedence over R model output in subsequent cycles
    petin(mnumcr,26:31)   = (/2.777,2.980,3.740,4.958,3.231,2.843/) /      &  ! 2015-2020, nominal dollars/mmBtu, ethane
                            MC_JPGDP(26:31)                                   ! convert to 1987 dollars per million Btu
    pprinpf(mnumcr,26:31) = (/0.452,0.484,0.762,0.880,0.531,0.463/) *      &  ! 2015-2020, nominal dollars/gal, propane
                            42. / cfprq / MC_JPGDP(26:31)                     ! convert to 1987 dollars per million Btu

!  Get current drive and directory
    dir  = FILE$CURDRIVE      ! get default drive
    ldir = GETDRIVEDIRQQ(Dir) ! on input, Dir is just Drive; on output Dir is drive/path

!  create the input file to the external python (previously used R) program
!   get unused unit number
    call unitunopened(100,999,runit)

!  create a RunName with the cycle number as part of the name:
    write(NGPL_Price,'(a,i2.2)') 'NGPL_Price',curirun  ! include the cycle number in the run name to save each invocation

!   Write python (previously used R) program's input file
    open(unit=runit,file=trim(NGPL_Price)//'_RInputData.csv',status='unknown')
    rewind runit    ! it may exist, so this causes the output to overwrite rather than append

!  create the input file in csv format, beginning with the header line
    write(runit,  '(  25(a,",") )' ) &
        'Year',                                          &     !  colA
        'total NGPL production (Mbbl/d)',                &     !  colB
        'ethane annual NGPL production (Mbbl/d)',        &     !  colC
        'propane annual NGPL production (Mbbl/d)',       &     !  colD
        'ethane annual cents/gal (nominal)',             &     !  colE
        'ethane annual cents/gal (real 2011$)',          &     !  colF
        'propane annual cents/gal (nominal)',            &     !  colG
        'propane annual cents/gal (real 2011$)',         &     !  colH
        'organic shipment demand (billion 2009$)',       &     !  colI
        'resin shipment demand (billion 2009$)',         &     !  colJ
        'total chemical demand (billion 2009$)',         &     !  colK
        'WTI (2011$/ barrel)',                           &     !  colL
        'HH (2011$/MMBTU)',                              &     !  colM
        'deflator to 1987$',                             &     !  colN
        'Brent (2011$)',                                 &     !  colO
        'ethane consumed (trils)',                       &     !  colP
        'propane consumed (trils)'                             !  colQ


    IROUTDBG = FILE_MGR('O','ROUTDBG           ',NEW)
    AEOLSTYR = RTOVALUE("AEOLSTYR",0)  ! Get calendar year for last year of AEO projection period.
    call RTOSTRING('NEMSPYENV',NEMSPYENV)
    write(CURIRUNSTR,10439) CURIRUN
10439  format(I2)
    do iy=2002,AEOLSTYR
      iyn=iy-baseyr+1  ! convert calendar year to NEMS year index, 1 for 1990 (NEMS baseyr)
      iy2011=2011-baseyr+1
      bar_per_gal=1./42.
      colB =  sum(RFQNGPL(MNUMPR,iyn,1:5))
      colC =  RFQNGPL(MNUMPR,iyn,1)
      colD =  RFQNGPL(MNUMPR,iyn,2)
      colE =  PETIN(11,iyn) * MC_JPGDP(iyn)   * 100. * CFEEQ * bar_per_gal    ! nominal
      colF =  PETIN(11,iyn) * MC_JPGDP(iy2011)* 100. * CFEEQ * bar_per_gal    ! $2011
      colG =  PPRINPF(11,iyn) * MC_JPGDP(iyn)   * 100. * CFPRQ * bar_per_gal  ! nominal
      colH =  PPRINPF(11,iyn) * MC_JPGDP(iy2011)* 100. * CFPRQ * bar_per_gal  ! $2011
      colI =  MC_REVIND(11,16,iyn)
      colJ =  MC_REVIND(11,18,iyn)
      colK =  (MC_REVIND(11,15,iyn) + MC_REVIND(11,16,iyn) + MC_REVIND(11,18,iyn) + MC_REVIND(11,19,iyn))
      colL =  WTI_PRICE(iyn) * MC_JPGDP(iy2011)
      colM =  OGHHPRNG(iyn) * MC_JPGDP(iy2011)
      colN =  MC_JPGDP(iyn)             ! GDP Implicit price deflator indexed to 1987
      colO =  BRENT_PRICE(iyn) * MC_JPGDP(iy2011)
      colP =  QETINPF(11, iyn)
      colQ =  QPRINPF(11, iyn)


      write (IROUTDBG,*) 'iyn = ',iyn
      write (IROUTDBG,*) 'iy2011 = ',iy2011
      write (IROUTDBG,*) 'baseyr = ',baseyr
      write (IROUTDBG,*) 'PPRINPF(11,iyn) in 1987$/MMBTU = ',PPRINPF(11,iyn)
      write (IROUTDBG,*) 'PPRINPF(11,iyn) in 2011 cents/gallon = ',PPRINPF(11,iyn) * MC_JPGDP(iy2011) * 100.0 * CFPRQ * bar_per_gal
      write (IROUTDBG,*) 'DLM (1)'
      write (IROUTDBG,*)


100 format(i4,',',25(F15.6,',') )
    write(runit,100) iy,colB,colC,colD,colE,colF,colG,colH,colI,colJ,colK,colL,colM,colN,colO,colP,colQ

    end do

    close(runit)

!    IPROPCO = FILE_MGR('O','PROPCO            ',.FALSE.)
!    if (ipropco .gt. 0) then
!      write(*,*) " Reading propane price coefficients from file!"
!      read (ipropco,*)  dummy
!      read (ipropco,*)  dummy,(rs_prcoef(i),i=1,3)
!      read (ipropco,*)  dummy,(cm_prcoef(i),i=1,3)
!      read (ipropco,*)  dummy,(in_prcoef(i),i=1,3)
!      read (ipropco,*)  dummy,(tr_prcoef(i),i=1,3)
!    else   ! if file not read properly, fill them with the AEO2020 values
!      write(*,*) " NOT reading propane price coefficients from file!"
!      rs_prcoef=(/0.69960,0.30784,0.61500/)
!      cm_prcoef=(/1.58368,0.51461,0.23796/)
!      in_prcoef=(/0.71060,0.75976,0.14961/)
!      tr_prcoef=(/1.80588,0.51702,0.18045/)
!    end if

! create command like this to invoke R using the RTERM executable:
!
!        rterm.exe -e "RunName <- 'NGPL_Price01'" -e "source('input/ngplprice.r')"
!

!    call RTOSTRING('RLOC    ',RLOC)        !  R version node, such as 'R-2.15.1'
!    RTERM = 'C:\R\' // trim(RLOC) // '\bin\x64\RTerm.exe'
!    rcmd=trim(RTERM) // ' -e "RunName <- '''  //  trim(NGPL_Price)  // '''" -e "source(''input/ngplprice.r'')"  '
!    call callsys(iret,rcmd)

!   WRITE(*,*) '***Calling run_NGPL.bat'
!   CALL execute_command_line ("ngpl\run_ngpl.bat" // " " // NEMSPYENV // " " // CURIRUNSTR)

!   we note that ngplprice.py is now called from nems_flow.py

end subroutine NGPL_STAT_PRICE

subroutine NGPL_STAT_PRICE_REPORTING
use ifport, only: GETDRIVEDIRQQ, FILE$CURDRIVE, $MAXPATH
implicit none
! we report out results from estimatation of propane and ethane prices, aka NGPLs
    include 'parametr'
    include 'ncntrl'
    include 'mpblk'
    include 'qblk'
    include 'macout'
    include 'convfact'
    integer :: runit, i, ir, iy, iyn, nemsyr, iy2011

    integer regyear ! for regionalizing historical propane price data

    character*80 line
    character*12 NGPL_Price

    integer :: iyear,colA,iret
    real :: colB,colC


! variables and data in 4 lines below are for breaking out CD-level prices
    real resiweights(1:9)/0.172,0.161,-0.102,-0.183,0.111,0.0582,0.0385,-0.0416,0.120/
    real commweights(1:9)/0.0188,0.0753,-0.0797,-0.121,0.0369,-0.0164,-0.0210,-0.00942,0.0487/
    real tranweights(1:9)/-0.0233,-0.0159,0.00396,-0.0106,0.0254,0.0200,-0.00514,-0.0240,-0.000260/
    real resinat, commnat, trannat

    real lpgbase ! base propane feedstock (Mt. Belvieu) price = PPRINPF
    real rs_prcoef(3), cm_prcoef(3), in_prcoef(3), tr_prcoef(3)
    integer icd  ! CD counter for Census Divisions

    real :: bar_per_gal

    CHARACTER($MAXPATH) dir
    INTEGER(4) ldir

! +++ Variables to Call FILE_MGR for Initialization
    INTEGER IROUTDBG
    LOGICAL NEW/.TRUE./
    INTEGER FILE_MGR
    EXTERNAL FILE_MGR

	integer ipropco
	character*15 dummy
	
	bar_per_gal=1./42.
	
    IPROPCO = FILE_MGR('O','PROPCO            ',.FALSE.)
    if (ipropco .gt. 0) then
      write(*,*) " Reading propane price coefficients from file! REPORTING"
      read (ipropco,*)  dummy
      read (ipropco,*)  dummy,(rs_prcoef(i),i=1,3)
      read (ipropco,*)  dummy,(cm_prcoef(i),i=1,3)
      read (ipropco,*)  dummy,(in_prcoef(i),i=1,3)
      read (ipropco,*)  dummy,(tr_prcoef(i),i=1,3)
    else   ! if file not read properly, fill them with the AEO2020 values
      write(*,*) " NOT reading propane price coefficients from file! REPORTING"
      rs_prcoef=(/0.69960,0.30784,0.61500/)
      cm_prcoef=(/1.58368,0.51461,0.23796/)
      in_prcoef=(/0.71060,0.75976,0.14961/)
      tr_prcoef=(/1.80588,0.51702,0.18045/)
    end if
	
	
    iy2011=2011-baseyr+1
    
    IROUTDBG = FILE_MGR('O','ROUTDBG           ',.FALSE.) ! MSI: Add unit for reporting out...should it be closed

    if(iret.eq.0) then
    
        !  create a RunName with the cycle number as part of the name:
       write(NGPL_Price,'(a,i2.2)') 'NGPL_Price',curirun  ! include the cycle number in the run name to save each invocation
		
		
       open(unit=runit,file=trim(NGPL_Price)//'_ROutputData.csv',status='old',readonly)

       ! skip a line
       read(runit,'()')
       do while ( .not. eof(runit))
          read(runit,'(a)') line
          read(line,'(i4)') iyear
          line(1:5)=' '
          read(line,*,err=99,end=99) colB,colC
		  
		  
		  nemsyr=iyear-baseyr+1
			
          if(iyear.gt.2017) then

             petin(mnumcr,nemsyr) = colB ! ethane
             pprinpf(mnumcr,nemsyr) = colC 

             write (IROUTDBG,*) 'nemsyr = ',nemsyr
             write (IROUTDBG,*) 'iyear = ',iyear
             write (IROUTDBG,*) 'iy2011 = ',iy2011
             write (IROUTDBG,*) 'PPRINPF(11,nemsyr) in 2011 cents/gallon = ',PPRINPF(11,nemsyr)
             write (IROUTDBG,*) 'PPRINPF(11,nemsyr) in 1987$/MMBTU = ',PPRINPF(11,nemsyr) * 0.01 * (1.0/CFPRQ) * (1.0/bar_per_gal) * (1.0/MC_JPGDP(iy2011))
             write (IROUTDBG,*) 'DLM (2)'
             write (IROUTDBG,*)

! convert ethane price from 2011 cents/gallon to 1987$/MMBTU
             PETIN(mnumcr,nemsyr) = PETIN(mnumcr,nemsyr) * 0.01 * (1.0/CFEEQ) * (1.0/bar_per_gal) * (1.0/MC_JPGDP(iy2011))
             PETINPF(mnumcr,nemsyr) = PETIN(mnumcr,nemsyr) ! ethane feedstock = ethane industrial
                                                           ! since it is only used for feedstock after all

             do icd = 1,9                                  ! assume CD prices = national price for ethane feedstock...
                petin(icd,nemsyr) = petin(mnumcr,nemsyr)   ! 95+% of ethane cracked/used in CD 7 anyway
                petinpf(icd,nemsyr) = petinpf(mnumcr,nemsyr)
             end do

! base (Mt. Belvieu price) for sectoral compuations below (lpgbase must be in 1987 cents/gallon)
! see LFMM documentation sent to Beth May, Mike Cole, Mindi, Kelly, Janice on 08/27/2012
!            lpgbase = log(pprinpf(mnumcr,nemsyr) * (1.0/MC_JPGDP(iy2011)))
             lpgbase = pprinpf(mnumcr,nemsyr)

! set up sectoral prices based on regression relationships to Mt. Belvieu price (aka petchem feedstock price)
! these sectoral prices are computed as 1987$/MMBTU, as they should be for use NEMS wide
!            plgrs(mnumcr,nemsyr) = exp(0.2425 * lpgbase + 3.827) * 0.01 * 42.0 / CFPRQ
!            plgcm(mnumcr,nemsyr) = exp(0.3501 * lpgbase + 3.237) * 0.01 * 42.0 / CFPRQ
!            plgin(mnumcr,nemsyr) = exp(0.3656 * lpgbase + 3.190) * 0.01 * 42.0 / CFPRQ
!            plginpf(mnumcr,nemsyr) = exp(0.9986 * lpgbase) * 0.01 * 42.0 / CFPRQ
             plgrs(mnumcr,nemsyr) = exp(rs_prcoef(3) * log(plgrs(mnumcr,nemsyr-1)*MC_JPGDP(iy2011)*CFPRQ*100./42.) + rs_prcoef(2) * log(lpgbase) + rs_prcoef(1)) * 0.01 * 42.0 / CFPRQ / MC_JPGDP(iy2011)
             plgcm(mnumcr,nemsyr) = exp(cm_prcoef(3) * log(plgcm(mnumcr,nemsyr-1)*MC_JPGDP(iy2011)*CFPRQ*100./42.) + cm_prcoef(2) * log(lpgbase) + cm_prcoef(1)) * 0.01 * 42.0 / CFPRQ / MC_JPGDP(iy2011)
             plgin(mnumcr,nemsyr) = exp(in_prcoef(3) * log(plgin(mnumcr,nemsyr-1)*MC_JPGDP(iy2011)*CFPRQ*100./42.) + in_prcoef(2) * log(lpgbase) + in_prcoef(1)) * 0.01 * 42.0 / CFPRQ / MC_JPGDP(iy2011)
             plgtr(mnumcr,nemsyr) = exp(tr_prcoef(3) * log(plgtr(mnumcr,nemsyr-1)*MC_JPGDP(iy2011)*CFPRQ*100./42.) + tr_prcoef(2) * log(lpgbase) + tr_prcoef(1)) * 0.01 * 42.0 / CFPRQ / MC_JPGDP(iy2011)
             plginpf(mnumcr,nemsyr) = (QETIN(MNUMCR,nemsyr)*PETIN(mnumcr,nemsyr)+QPRIN(MNUMCR,nemsyr)*PPRIN(mnumcr,nemsyr))/(QETIN(MNUMCR,nemsyr)+QPRIN(MNUMCR,nemsyr))


             pprrs(mnumcr,nemsyr) = plgrs(mnumcr,nemsyr) ! fill in national propane prices as "LPG"
             pprcm(mnumcr,nemsyr) = plgcm(mnumcr,nemsyr)
             pprin(mnumcr,nemsyr) = plgin(mnumcr,nemsyr)
             pprinpf(mnumcr,nemsyr) = (1.0 * lpgbase) * 0.01 * 42.0 / CFPRQ / MC_JPGDP(iy2011) 
             pprtr(mnumcr,nemsyr) = plgtr(mnumcr,nemsyr)

! set up regionality for propane prices
! only do residential, commercial, and transportation (industrial all in CD 7 anyway)
             do icd = 1,9
                plgrs(icd,nemsyr) = plgrs(mnumcr,nemsyr) * (1.0 + resiweights(icd))
                plgcm(icd,nemsyr) = plgcm(mnumcr,nemsyr) * (1.0 + commweights(icd))
                plgtr(icd,nemsyr) = plgtr(mnumcr,nemsyr) * (1.0 + tranweights(icd))
                plgin(icd,nemsyr) = plgin(mnumcr,nemsyr)     ! CD's same as national (not going to fuss with trivial amounts
                plginpf(icd,nemsyr) = plginpf(mnumcr,nemsyr) ! CD's same as national (they only crack it in CD)
             end do

! store national weighted price from just computed regional prices
             resinat = 0.0
             commnat = 0.0
             trannat = 0.0
             do icd = 1,9
                resinat = resinat + plgrs(icd,nemsyr) * qlgrs(icd,nemsyr) / qlgrs(11,nemsyr)
                commnat = commnat + plgcm(icd,nemsyr) * qlgcm(icd,nemsyr) / qlgcm(11,nemsyr)
                trannat = trannat + plgtr(icd,nemsyr) * qlgtr(icd,nemsyr) / qlgtr(11,nemsyr)
             end do

! renomalize CD prices so that their average-weighted sum is = to the desired national price
             do icd = 1,9
                plgrs(icd,nemsyr) = plgrs(icd,nemsyr) * plgrs(11,nemsyr) / resinat
                plgcm(icd,nemsyr) = plgcm(icd,nemsyr) * plgcm(11,nemsyr) / commnat
                plgtr(icd,nemsyr) = plgtr(icd,nemsyr) * plgtr(11,nemsyr) / trannat
             end do

             do icd = 1,9 ! fill in CD-level propane prices as LPG; feedstock and industrial are same as national (see above)
                pprrs(icd,nemsyr) = plgrs(icd,nemsyr)
                pprcm(icd,nemsyr) = plgcm(icd,nemsyr)
                pprin(icd,nemsyr) = plgin(icd,nemsyr)
                pprinpf(icd,nemsyr) = plginpf(icd,nemsyr)
                pprtr(icd,nemsyr) = plgtr(icd,nemsyr)
             end do

! set up historical regional prices for 2011 - 2013
! set up regionality for propane prices
! only do residential, commercial, and transportation (industrial all in CD 7 anyway)
             if (nemsyr.eq.25) then ! only set up historical regional prices when first projection year worked
                do regyear = 22,24    ! loop over 2011 - 2013

                   do icd = 1,9
                      plgrs(icd,regyear) = plgrs(mnumcr,regyear) * (1.0 + resiweights(icd))
                      plgcm(icd,regyear) = plgcm(mnumcr,regyear) * (1.0 + commweights(icd))
                      plgtr(icd,regyear) = plgtr(mnumcr,regyear) * (1.0 + tranweights(icd))
                      plgin(icd,regyear) = plgin(mnumcr,regyear)     ! CD's same as national (not going to fuss with trivial amounts
                      plginpf(icd,regyear) = plginpf(mnumcr,regyear) ! CD's same as national (they only crack it in CD)
                   end do

! store national weighted price from just computed regional prices
                   resinat = 0.0
                   commnat = 0.0
                   trannat = 0.0
                   do icd = 1,9
                      resinat = resinat + plgrs(icd,regyear) * qlgrs(icd,regyear) / qlgrs(11,regyear)
                      commnat = commnat + plgcm(icd,regyear) * qlgcm(icd,regyear) / qlgcm(11,regyear)
                      trannat = trannat + plgtr(icd,regyear) * qlgtr(icd,regyear) / qlgtr(11,regyear)
                   end do

! renomalize CD prices so that their average-weighted sum is = to the desired national price
                   do icd = 1,9
                      plgrs(icd,regyear) = plgrs(icd,regyear) * plgrs(11,regyear) / resinat
                      plgcm(icd,regyear) = plgcm(icd,regyear) * plgcm(11,regyear) / commnat
                      plgtr(icd,regyear) = plgtr(icd,regyear) * plgtr(11,regyear) / trannat
                   end do

                   do icd = 1,9 ! fill in CD-level propane prices as LPG; feedstock and industrial are same as national (see above)
                      pprrs(icd,regyear) = plgrs(icd,regyear)
                      pprcm(icd,regyear) = plgcm(icd,regyear)
                      pprin(icd,regyear) = plgin(icd,regyear)
                      pprinpf(icd,regyear) = plginpf(icd,regyear)
                      pprtr(icd,regyear) = plgtr(icd,regyear)
                   end do
                end do        ! loop over historical years 2011 - 2013
             end if         ! end set up historical regional prices for 2011 - 2013

             write (IROUTDBG,*) 'residential CD1 2011 = ',pprrs(1,22)
             write (IROUTDBG,*) 'residential CD2 2011 = ',pprrs(2,22)

             write (IROUTDBG,*) 'In 1987$/MMBTU '
             write (IROUTDBG,*) 'DLM ethane', petin(mnumcr,nemsyr)
             write (IROUTDBG,*) 'DLM PPRINPF', pprinpf(mnumcr,nemsyr)
             write (IROUTDBG,*) 'DLM PPRIN', pprin(mnumcr,nemsyr)
             write (IROUTDBG,*) 'DLM PPRRS', pprrs(mnumcr,nemsyr)
             write (IROUTDBG,*) 'DLM PPRCM', pprcm(mnumcr,nemsyr)
             write (IROUTDBG,*) 'DLM PPRTR', pprtr(mnumcr,nemsyr)

             write (IROUTDBG,*) 'In 2011$/MMBTU '
             write (IROUTDBG,*) 'DLM ethane', petin(mnumcr,nemsyr) * MC_JPGDP(iy2011)
             write (IROUTDBG,*) 'DLM PPRINPF', pprinpf(mnumcr,nemsyr) * MC_JPGDP(iy2011)
             write (IROUTDBG,*) 'DLM PPRIN', pprin(mnumcr,nemsyr) * MC_JPGDP(iy2011)
             write (IROUTDBG,*) 'DLM PPRRS', pprrs(mnumcr,nemsyr) * MC_JPGDP(iy2011)
             write (IROUTDBG,*) 'DLM PPRCM', pprcm(mnumcr,nemsyr) * MC_JPGDP(iy2011)
             write (IROUTDBG,*) 'DLM PPRTR', pprtr(mnumcr,nemsyr) * MC_JPGDP(iy2011)

          end if

       end do  ! end of do while .not. eof(runit)
       close(runit)
    end if
    return

99  write(6,'(a)')  'Error reading R output file in subroutine stat_price'
    close(runit)
end subroutine NGPL_STAT_PRICE_REPORTING