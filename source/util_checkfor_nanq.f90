!-*- f90 -*-
! !# check convergence variables for NaNq. If found, sets inanq to 1 and writes a message 
! module check_convg_nanq
    ! contains
!*******************************************************************
!* Check_for_NaNq
!*
!* Check's convergence variables for NaNq.  Returns 1 if found.
!*******************************************************************
      SUBROUTINE Check_for_NaNq(inanq)
      IMPLICIT NONE

      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'uefdout'
      include 'ngtdmout'
      include 'cdsparms'
      include 'coalemm'
      include 'coalprc'
      include 'pqchar'
      include 'intout'
      include 'macout'
      include 'pmmrpt'
      include 'emission'
      include 'uso2grp'
      integer ivar,ireg,inanq
      inanq=0 ! initialize to false

!test inputs
!MPVARS(1)="pmgtr"
!ireg=1
!curiyr=1

!WRITE(*,*)"start check_for_nanq()"
!WRITE(*,*)"check_for_nanq(), mnump=",mnump
!WRITE(*,*)"check_for_nanq(), mnumcr=",mnumcr
!WRITE(*,*)"check_for_nanq(), NDRG2=",NDRG2
! Basic PQ variables
      do ivar=1,mnump
        do ireg=1,mnumcr
          call write_NaNq(mprc(ireg,curiyr,ivar),curiyr,curitr,ireg,mpvars(ivar),inanq)
          !test writes
          !WRITE(*,*) mpvars(ivar)
          !WRITE(*,*) curiyr
          !WRITE(*,*) curitr
          !WRITE(*,*) ireg
          !WRITE(*,*) inanq
        enddo
      enddo
      do ivar=1,mnumq
        do ireg=1,mnumcr
          call write_NaNq(mqty(ireg,curiyr,ivar),curiyr,curitr,ireg,mqvars(ivar),inanq)
        enddo
      enddo

! Utility <---> Original Gas variables (firm/itnteruptible/competitive), plus new firm/interuptible variables for two seasons
! original (obsolete)
      do ivar=1,3
        do ireg=1,nngem
           call write_NaNq(muprc(ireg,curiyr,ivar),curiyr,curitr,ireg,mupvars(ivar),inanq)
        enddo
      enddo

      do ivar=1,3
        do ireg=1,nngem
          call write_NaNq(muqty(ireg,curiyr,ivar),curiyr,curitr,ireg,muqvars(ivar),inanq)
        enddo
      enddo
! newer
      do ivar=1,4
        do ireg=1,nngem
           call write_NaNq(smuprc(ireg,curiyr,ivar),curiyr,curitr,ireg,smupvars(ivar),inanq)
        enddo
      enddo

      do ivar=1,4
        do ireg=1,nngem
           call write_NaNq(smuqty(ireg,curiyr,ivar),curiyr,curitr,ireg,smuqvars(ivar),inanq)
        enddo
      enddo


! Utility <---> Coal variables
      do ivar=1,2
        do ireg=1,NDRG2
          call write_NaNq(pclelcdr(ivar,ireg,curiyr),curiyr,curitr,ireg,mcpvarss,inanq)
        enddo
      enddo

      do ivar=1,NCLUT1
        do ireg=1,NDRGG
          call write_NaNq(qclclnr(ireg,curiyr,ivar),curiyr,curitr,ireg,mcqvarss(ivar),inanq)
        enddo
      enddo

      do ivar=1,MNOTH
        ireg=11
        call write_NaNq(convoth(2,ivar,curiyr),curiyr,curitr,ireg,movars(ivar),inanq)
      enddo

      do ireg=1,MX_NCOALS
        call write_NaNq(XCL_PCAP(ireg,curiyr),curiyr,curitr,ireg,mzvars(1),inanq)
      enddo

      do ireg=1,MX_UNITS
        call write_NaNq(EMM_CL_CF(ireg,curiyr),curiyr,curitr,ireg,mzvars(2),inanq)
        call write_NaNq(EMM_CL_BTUs(ireg,curiyr),curiyr,curitr,ireg,mzvars(3),inanq)
      enddo

!WRITE(*,*)"done with check_for_nanq()"

      RETURN
      END SUBROUTINE Check_for_NaNq

! end module check_convg_nanq
! !============================================
!********************************************************************
! Write_NaNq
! Checks for a nanq and if found, sets inanq to 1 and writes a message
! to standard output with the year, iteration, region, and variable name.
! The intrinsic function ISNAN returns true if the real argument is
! an IEEE NaN value.
!*********************************************************************
      subroutine WRITE_NaNq(Rvalue,curiyr,curitr,ireg,vname,inanq)
      implicit none
      character*(*) vname
      real Rvalue, test/100./, zero/0./,testvalue

      integer curiyr,curitr,ireg ! input arguments for year index, iteration, region
      Integer inanq ! output argument:  1 if nanq detected, unchanged otherwise

      if(isnan(rvalue)) then
         inanq=1
         write(6,100)'NaN in '//trim(vname)//', curiyr/curitr/region=', &
         curiyr,curitr,ireg
100 format(1x,a,3i3)
      endif
      return
      end subroutine WRITE_NaNq