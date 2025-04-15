!-*- f90 -*-
!module adjust_epm_price
!    contains


      SUBROUTINE COPY_ADJUSTED
!*******************************************************************
!  THIS SUBROUTINE COPIES PRICES FOR THE VARIABLES IN THE MPBLK
!  COMMON TO THE VARIABLES IN THE EPMMPBLK COMMON.  IF THE EPM IS
!  TURNED ON SUBROUTINE PRICE_ADJUST IS CALLED.  THIS SUBROUTINE
!  CALCULATES THE PRICE ADJUSTMENT OF END-USE FUELS. IN EACH CASE WE
!  FIND THE PRICE THAT THE DEMAND SECTORS RESPONT TO, AND ADJUST THE
!  PRICE BASED ON THE CARBON CONTENT OF THE FUEL AND THE TAX.
!     THIS VERSION WILL USE THE NEW ADJUSTED PRICE BLOCKS
!*******************************************************************

      IMPLICIT NONE
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'mpblk'
      include 'qblk'
      include 'qsblk'
      include 'indout'
      include 'ngtdmout'
      include 'cdsparms'
      include 'uefdout'
      include 'epmmpblk'
      include 'epmngtdm'
      include 'coalprc'
      include 'epmclprc'
      include 'convfact'
      include 'emablk'
      include 'epmcntl'
      include 'emoblk'
      include 'epmbank'
      include 'ponroad'
      include 'epmonrd'
      include 'epmeusprc'
      include 'eusprc'
      include 'ab32'
      include 'emeblk'
      include 'ghgrep'
      include 'calshr'
      include 'lfmmout'
      include 'pmmout'

      INTEGER I,J,K,IR,IYR,IV
      INTEGER X,Y,Z
      integer rtovalue
      external rtovalue ! to get run time option AB32SW
      integer AB32SW
      real AB32P9 ! function to get AB32 adjusted price in Pacific Division (#9)
      external AB32P9
      real emfac_qmgtr ! carbon emission factor for motor gasoline, adjusted for blended ethanol btu content which doesn't count
      real emfac_qettr ! carbon emission factor for E85, adjusted for gasoline content only
      real emfac_qdstr ! carbon emission factor for distillate, adjusted for biodiesel content which doesn't count
      real cal_ind_emis ! industrial carbon emissions for california, total, then net of covered industrial and refining.
      integer nseas
      integer CO2PLFMM  ! if 1, assum LFMM has included CO2 price adjustments in prices.  if 0, add CO2 adjustments here.
      integer file_mgr
      external file_mgr
      logical new,lexist
      integer pacific/0/
      logical done/.false./, doneyet/.false./
      character*16 rname

!     HERE WE COPY THE ADJUSTED PRICES FOR THIS YEAR ONLY

      DO IR=1,MNUMCR
        DO IV=1,MNUMP
           AMPRC(IR,CURIYR,IV)=MPRC(IR,CURIYR,IV)
        ENDDO
      ENDDO

!  COPY NGTDM variables other than PRICES TO ADJUSTED BLOCK
!  use array assignments--allowed in fortran 90
!     AGPRDNGON=OGPRDNGON
!     AGPRDNGOF=OGPRDNGOF
!     ARNG_PADD=PRNG_PADD
!     ALSYNGWP =CLSYNGWP
      APBAJA   =PBAJA
      AQBAJA   =QBAJA

      J=CURIYR

!WRITE(*,*)"start copy_adjusted()"
!WRITE(*,*)"copy_adjusted(): bank_flag=",bank_flag
!WRITE(*,*)"copy_adjusted(): curcalyr=",curcalyr
!WRITE(*,*)"copy_adjusted(): bank_startyr=",bank_startyr
!WRITE(*,*)"copy_adjusted(): (bank_flag.and.curcalyr.ge.bank_startyr) .or. .not. bank_flag)=",((bank_flag.and.curcalyr.ge.bank_startyr) .or. .not. bank_flag)
  if( (bank_flag.and.curcalyr.ge.bank_startyr) .or. .not. bank_flag) then

!	WRITE(*,*)"copy_adjusted(): inside the if-loop"
      DO I=1, MNUMCR
!    NATURAL GAS, CORE (RESID,COMM,TRANS,IND,UTIL)
         AGFRS(I,J) = PGFRS(I,J) +  JGFRS(J)
         AGFCM(I,J) = PGFCM(I,J) +  JGFCM(J)
         AGFTR(I,J) = PGFTR(I,J) +  JGFTR(J)
         AGFIN(I,J) = PGFIN(I,J) +  JGFIN(J)
       IF(QGFIN(I,J) .GT. 0.0) THEN
         AGFIN(I,J) = PGFIN(I,J) + (((QGFIN(I,J) - INQNGPF(I,J)) * &
               JGFIN(J) + JNQNGPF(J) * INQNGPF(I,J)) / QGFIN(I,J))
       ELSE
         AGFIN(I,J) = PGFIN(I,J) + JGFIN(J)
       END IF
         AGFEL(I,J) = PGFEL(I,J) +  JGFEL(J)

!    NATURAL GAS, NONCORE (RESID,COMM,TRANS,IND,UTIL)
         AGIRS(I,J) = PGIRS(I,J) +  JGIRS(J)
         AGICM(I,J) = PGICM(I,J) +  JGICM(J)
         AGITR(I,J) = PGITR(I,J) +  JGITR(J)
         AGIIN(I,J) = PGIIN(I,J) +  JGIIN(J)
         AGIEL(I,J) = PGIEL(I,J) +  JGIEL(J)

!    NATURAL GAS, TOTAL (RESID,COMM,TRANS,IND,UTIL)
         ANGRS(I,J) = PNGRS(I,J) +  JNGRS(J)
         ANGCM(I,J) = PNGCM(I,J) +  JNGCM(J)
         ANGTR(I,J) = PNGTR(I,J) +  JNGTR(J)
!    COMPRESSED and liquefied natural gas
         AGFTRFV(I,J) = PGFTRFV(I,J) +  JNGTR(J)
         AGFTRPV(I,J) = PGFTRPV(I,J) +  JNGTR(J)
         AGLTRFV(I,J) = PGLTRFV(I,J) +  JNGTR(J)
         AGLTRPV(I,J) = PGLTRPV(I,J) +  JNGTR(J)
         AGFTRRAIL(:,I,J) = PGFTRRAIL(:,I,J) + JNGTR(J)      ! rail use of natural gas
         AGLTRRAIL(:,I,J) = PGLTRRAIL(:,I,J) + JNGTR(J)
         AGFTRSHIP(:,I,J) = PGFTRSHIP(:,I,J) + JNGTR(J)      ! shipping use of natural gas
         AGLTRSHIP(:,I,J) = PGLTRSHIP(:,I,J) + JNGTR(J)

         ANGIN(I,J) = PNGIN(I,J) +  JNGIN(J)
       IF(QNGIN(I,J) .GT. 0.0) THEN
         ANGIN(I,J) = PNGIN(I,J) + (((QNGIN(I,J) - INQNGPF(I,J)) * &
              JNGIN(J) + JNQNGPF(J) * INQNGPF(I,J)) / QNGIN(I,J))
       ELSE
         ANGIN(I,J) = PNGIN(I,J) + JNGIN(J)
       END IF
         ANGEL(I,J) = PNGEL(I,J) +  JNGEL(J)

         AGPTR(I,J) = PGPTR(I,J) +  JGPTR(J)
         ALPIN(I,J) = PLPIN(I,J) +  JLPIN(J)

         IF(PCLRS(I,J).GE.15.0) PCLRS(I,J)=15.
         IF(PCLCM(I,J).GE.15.0) PCLCM(I,J)=15.
         IF(PCLIN(I,J).GE.15.0) PCLIN(I,J)=15.
         IF(PCLEL(I,J).GE.15.0) PCLEL(I,J)=15.
         ACLRS(I,J) = PCLRS(I,J) +  JCLRS(J)
         ACLCM(I,J) = PCLCM(I,J) +  JCLCM(J)
         ACLIN(I,J) = PCLIN(I,J) +  JCLIN(J)
         ACLEL(I,J) = PCLEL(I,J) +  JCLEL(J)
         ACLSN(I,J) = PCLSN(I,J) +  JCLIN(J)
         ACLGAS(I,J) = PCLGAS(I,J) +  JCLIN(J)
         AMCIN(I,J) = PMCIN(I,J) +  JMCIN(J)

! LFMM may include CO2 price adjustments in petroleum prices.  If run-time option
! CO2PLFMM=0, assume LFMM has not done so.  If =1, just copy prices.
       CO2PLFMM=rtovalue('CO2PLFMM',0)
       IF(CO2PLFMM.eq.0) then
         AMGCM(I,J) = PMGCM(I,J) +  JMGCM(J)
         AMGTR(I,J) = PMGTR(I,J) +  JMGTR(J)
         AMGIN(I,J) = PMGIN(I,J) +  JMGIN(J)
         AJFTR(I,J) = PJFTR(I,J) +  JJFTR(J)
         ADSRS(I,J) = PDSRS(I,J) +  JDSRS(J)
         ADSCM(I,J) = PDSCM(I,J) +  JDSCM(J)
         ADSTR(I,J) = PDSTR(I,J) +  JDSTR(J)
         ADSTRHWY(I,J) = PDSTRHWY(I,J) +  JDSTR(J)   ! slip on-road distillate in here
         ADSIN(I,J) = PDSIN(I,J) +  JDSIN(J)
         ADSEL(I,J) = PDSEL(I,J) +  JDSEL(J)
         AKSRS(I,J) = PKSRS(I,J) +  JKSRS(J)
         AKSCM(I,J) = PKSCM(I,J) +  JKSCM(J)
         AKSIN(I,J) = PKSIN(I,J) +  JKSIN(J)
         ALGRS(I,J) = PLGRS(I,J) +  JLGRS(J)
         ALGCM(I,J) = PLGCM(I,J) +  JLGCM(J)
         ALGTR(I,J) = PLGTR(I,J) +  JLGTR(J)
         ALGIN(I,J) = PLGIN(I,J) +  JLGIN(J)
         ALGINPF(I,J) = PLGINPF(I,J) + JNQLGPF(J)
         APRRS(I,J) = PPRRS(I,J) + JPRRS(J)
         APRCM(I,J) = PPRCM(I,J) + JPRCM(J)
         APRTR(I,J) = PPRTR(I,J) + JPRTR(J)
         AETIN(I,J) = PETIN(I,J) + JETIN(J)
         APRIN(I,J) = PPRIN(I,J) + JPRIN(J)
         ABUIN(I,J) = PBUIN(I,J) + JBUIN(J)
         AISIN(I,J) = PISIN(I,J) + JISIN(J)
         AETINPF(I,J) = PETINPF(I,J) + JETINPF(J)
         APRINPF(I,J) = PPRINPF(I,J) + JPRINPF(J)
         ABUINPF(I,J) = PBUINPF(I,J) + JBUINPF(J)
         AISINPF(I,J) = PISINPF(I,J) + JISINPF(J)
         APROLENERF(I,J) = PPROLENERF(I,J)              ! not adjusting
         ARLCM(I,J) = PRLCM(I,J) +  JRLCM(J)
         ARLTR(I,J) = PRLTR(I,J) +  JRLTR(J)
         ARLIN(I,J) = PRLIN(I,J) +  JRLIN(J)
         ARLEL(I,J) = PRLEL(I,J) +  JRLEL(J)
         ARHTR(I,J) = PRHTR(I,J) +  JRHTR(J)
         ARHEL(I,J) = PRHEL(I,J) +  JRHEL(J)
         ARSCM(I,J) = PRSCM(I,J) +  JRSCM(J)
         ARSTR(I,J) = PRSTR(I,J) +  JRSTR(J)
         ARSIN(I,J) = PRSIN(I,J) +  JRSIN(J)
         ARSEL(I,J) = PRSEL(I,J) +  JRSEL(J)
         APFIN(I,J) = PPFIN(I,J) +  JPFIN(J)
         APCIN(I,J) = PPCIN(I,J) +  JPCIN(J)
         APPIN(I,J) = PPPIN(I,J) +  JPPIN(J)
         APPINPF(I,J) = PPPINPF(I,J) +  JPPINPF(J)
         ALUIN(I,J) = PLUIN(I,J) +  JLUIN(J)
         AOTIN(I,J) = POTIN(I,J) +  JOTIN(J)
         AOTTR(I,J) = POTTR(I,J) +  JOTTR(J)
         AMETR(I,J) = PMETR(I,J) +  JMETR(J)
         AETTR(I,J) = PETTR(I,J) +  JETTR(J)
       ELSE
         AMGCM(I,J) = PMGCM(I,J)
         AMGTR(I,J) = PMGTR(I,J)
         AMGIN(I,J) = PMGIN(I,J)
         AJFTR(I,J) = PJFTR(I,J)
         ADSRS(I,J) = PDSRS(I,J)
         ADSCM(I,J) = PDSCM(I,J)
         ADSTR(I,J) = PDSTR(I,J)
         ADSTRHWY(I,J) = PDSTRHWY(I,J)
         ADSIN(I,J) = PDSIN(I,J)
         ADSEL(I,J) = PDSEL(I,J)
         AKSRS(I,J) = PKSRS(I,J)
         AKSCM(I,J) = PKSCM(I,J)
         AKSIN(I,J) = PKSIN(I,J)
         ALGRS(I,J) = PLGRS(I,J)
         ALGCM(I,J) = PLGCM(I,J)
         ALGTR(I,J) = PLGTR(I,J)
         ALGIN(I,J) = PLGIN(I,J)
         ALGINPF(I,J) = PLGINPF(I,J)
         APRRS(I,J) = PPRRS(I,J)
         APRCM(I,J) = PPRCM(I,J)
         APRTR(I,J) = PPRTR(I,J)
         AETIN(I,J) = PETIN(I,J)
         APRIN(I,J) = PPRIN(I,J)
         ABUIN(I,J) = PBUIN(I,J)
         AISIN(I,J) = PISIN(I,J)
         AETINPF(I,J) = PETINPF(I,J)
         APRINPF(I,J) = PPRINPF(I,J)
         ABUINPF(I,J) = PBUINPF(I,J)
         AISINPF(I,J) = PISINPF(I,J)
         APROLENERF(I,J) = PPROLENERF(I,J)
         ARLCM(I,J) = PRLCM(I,J)
         ARLTR(I,J) = PRLTR(I,J)
         ARLIN(I,J) = PRLIN(I,J)
         ARLEL(I,J) = PRLEL(I,J)
         ARHTR(I,J) = PRHTR(I,J)
         ARHEL(I,J) = PRHEL(I,J)
         ARSCM(I,J) = PRSCM(I,J)
         ARSTR(I,J) = PRSTR(I,J)
         ARSIN(I,J) = PRSIN(I,J)
         ARSEL(I,J) = PRSEL(I,J)
         APFIN(I,J) = PPFIN(I,J)
         APCIN(I,J) = PPCIN(I,J)
         APPIN(I,J) = PPPIN(I,J)
         APPINPF(I,J) = PPPINPF(I,J)
         ALUIN(I,J) = PLUIN(I,J)
         AOTIN(I,J) = POTIN(I,J)
         AOTTR(I,J) = POTTR(I,J)
         AMETR(I,J) = PMETR(I,J) + JMETR(J)
         AETTR(I,J) = PETTR(I,J)
       ENDIF
! electricity prices at end use level (no tax)
         AELSHRS(I,J) = PELSHRS(I,J)
         AELCLRS(I,J) = PELCLRS(I,J)
         AELWHRS(I,J) = PELWHRS(I,J)
         AELCKRS(I,J) = PELCKRS(I,J)
         AELCDRS(I,J) = PELCDRS(I,J)
         AELRFRS(I,J) = PELRFRS(I,J)
         AELFZRS(I,J) = PELFZRS(I,J)
         AELLTRS(I,J) = PELLTRS(I,J)
         AELOTRS(I,J) = PELOTRS(I,J)
         AELH2RS(I,J) = PELH2RS(I,J)
         AELVHRS(I,J) = PELVHRS(I,J) 
         AELSHCM(I,J) = PELSHCM(I,J)
         AELSCCM(I,J) = PELSCCM(I,J)
         AELWHCM(I,J) = PELWHCM(I,J)
         AELVTCM(I,J) = PELVTCM(I,J)
         AELCKCM(I,J) = PELCKCM(I,J)
         AELLTCM(I,J) = PELLTCM(I,J)
         AELRFCM(I,J) = PELRFCM(I,J)
         AELOPCM(I,J) = PELOPCM(I,J)
         AELONCM(I,J) = PELONCM(I,J)
         AELOTCM(I,J) = PELOTCM(I,J)
         AELP2CM(I,J) = PELP2CM(I,J)
         AELPFCM(I,J) = PELPFCM(I,J) 
         AELSBCM(I,J) = PELSBCM(I,J) 
         AELTBCM(I,J) = PELTBCM(I,J) 
         AELIBCM(I,J) = PELIBCM(I,J)
         AELFNCM(I,J) = PELFNCM(I,J) 
         AELINP(I,J) = PELINP(I,J)
         AELINS(I,J) = PELINS(I,J)
         AELINM(I,J) = PELINM(I,J)
         AELINH2E(I,J) = PELINH2E(I,J)
         AELLTTR(I,J) = PELLTTR(I,J)
!         AELVHTR(I,J) = PELVHTR(I,J)
      ENDDO

!     ADJUST MARGINAL PRICES BY COAL REGIONS

      DO I = 1,NDRG2
        APCLELCDR(1,I,J) = PCLELCDR(1,I,J) + JCLEL(J)
        APCLELCDR(2,I,J) = PCLELCDR(2,I,J) + JCLEL(J)
      END DO

!     ADJUST natural GAS prices by EMM/NGMM REGIONS

      DO I = 1,NNGEM
         AGFELGR(I,J) = PGFELGR(I,J) +  JGFELGR(J)
         AGIELGR(I,J) = PGIELGR(I,J) +  JGIELGR(J)
         AGCELGR(I,J) = PGCELGR(I,J) +  JGCELGR(J)
      ENDDO
! seasonal:
      DO I = 1,NNGEM
        do nseas=1,3
         ASPNGELGR(I,J,nseas) = SPNGELGR(I,J,nseas) +  JNGEL(J)
        enddo
        APNGELGR(I,J) = PNGELGR(I,J) +  JNGEL(J)
      ENDDO
      DO I = 1,NNGEM
        do nseas=1,2
         ASPGFELGR(I,J,nseas) = SPGFELGR(I,J,nseas) +  JGFELGR(J)
         ASPGIELGR(I,J,nseas) = SPGIELGR(I,J,nseas) +  JGIELGR(J)
        enddo
      ENDDO
   else
! If before compliance period in a banking case, to not pass the allowance price
! on to end use prices.  They will, however, be used in capacity decisions.
      DO I=1, MNUMCR
!    NATURAL GAS, CORE (RESID,COMM,TRANS,IND,UTIL)
         AGFRS(I,J) = PGFRS(I,J)
         AGFCM(I,J) = PGFCM(I,J)
         AGFTR(I,J) = PGFTR(I,J)
         AGFIN(I,J) = PGFIN(I,J)
         AGFIN(I,J) = PGFIN(I,J)
         AGFEL(I,J) = PGFEL(I,J)

!    NATURAL GAS, NONCORE (RESID,COMM,TRANS,IND,UTIL)
         AGIRS(I,J) = PGIRS(I,J)
         AGICM(I,J) = PGICM(I,J)
         AGITR(I,J) = PGITR(I,J)
         AGIIN(I,J) = PGIIN(I,J)
         AGIEL(I,J) = PGIEL(I,J)

!    NATURAL GAS, TOTAL (RESID,COMM,TRANS,IND,UTIL)
         ANGRS(I,J) = PNGRS(I,J)
         ANGCM(I,J) = PNGCM(I,J)
         ANGTR(I,J) = PNGTR(I,J)
         AGFTRFV(I,J) = PGFTRFV(I,J)     ! these next two are compressed natural gas
         AGFTRPV(I,J) = PGFTRPV(I,J)
         AGLTRFV(I,J) = PGLTRFV(I,J)     ! these next two are liquefied natural gas
         AGLTRPV(I,J) = PGLTRPV(I,J)
         AGFTRRAIL(:,I,J) = PGFTRRAIL(:,I,J)      ! rail use of natural gas
         AGLTRRAIL(:,I,J) = PGLTRRAIL(:,I,J)
         AGFTRSHIP(:,I,J) = PGFTRSHIP(:,I,J)      ! shipping use of natural gas
         AGLTRSHIP(:,I,J) = PGLTRSHIP(:,I,J)
         ANGIN(I,J) = PNGIN(I,J)
         ANGIN(I,J) = PNGIN(I,J)
         ANGEL(I,J) = PNGEL(I,J)

         AGPTR(I,J) = PGPTR(I,J)
         ALPIN(I,J) = PLPIN(I,J)

! Coal
         ACLRS(I,J) = PCLRS(I,J)
         ACLCM(I,J) = PCLCM(I,J)
         ACLIN(I,J) = PCLIN(I,J)
         ACLEL(I,J) = PCLEL(I,J)
         ACLSN(I,J) = PCLSN(I,J)
         ACLGAS(I,J) = PCLGAS(I,J)
         AMCIN(I,J) = PMCIN(I,J)
         AMGCM(I,J) = PMGCM(I,J)
         AMGTR(I,J) = PMGTR(I,J)
         AMGIN(I,J) = PMGIN(I,J)
         AJFTR(I,J) = PJFTR(I,J)
         ADSRS(I,J) = PDSRS(I,J)
         ADSCM(I,J) = PDSCM(I,J)
         ADSTR(I,J) = PDSTR(I,J)
         ADSTRHWY(I,J) = PDSTRHWY(I,J)               ! slip on-road distillate in here
         ADSIN(I,J) = PDSIN(I,J)
         ADSEL(I,J) = PDSEL(I,J)
         AKSRS(I,J) = PKSRS(I,J)
         AKSCM(I,J) = PKSCM(I,J)
         AKSIN(I,J) = PKSIN(I,J)
         ALGRS(I,J) = PLGRS(I,J)
         ALGCM(I,J) = PLGCM(I,J)
         ALGTR(I,J) = PLGTR(I,J)
         ALGIN(I,J) = PLGIN(I,J)
         ALGINPF(I,J) = PLGINPF(I,J)
         APRRS(I,J) = PPRRS(I,J)
         APRCM(I,J) = PPRCM(I,J)
         APRTR(I,J) = PPRTR(I,J)
         AETIN(I,J) = PETIN(I,J)
         APRIN(I,J) = PPRIN(I,J)
         ABUIN(I,J) = PBUIN(I,J)
         AISIN(I,J) = PISIN(I,J)
         AETINPF(I,J) = PETINPF(I,J)
         APRINPF(I,J) = PPRINPF(I,J)
         ABUINPF(I,J) = PBUINPF(I,J)
         AISINPF(I,J) = PISINPF(I,J)
         APROLENERF(I,J) = PPROLENERF(I,J)
         ARLCM(I,J) = PRLCM(I,J)
         ARLTR(I,J) = PRLTR(I,J)
         ARLIN(I,J) = PRLIN(I,J)
         ARLEL(I,J) = PRLEL(I,J)
         ARHTR(I,J) = PRHTR(I,J)
         ARHEL(I,J) = PRHEL(I,J)
         ARSCM(I,J) = PRSCM(I,J)
         ARSTR(I,J) = PRSTR(I,J)
         ARSIN(I,J) = PRSIN(I,J)
         ARSEL(I,J) = PRSEL(I,J)
         APFIN(I,J) = PPFIN(I,J)
         APCIN(I,J) = PPCIN(I,J)
         APPIN(I,J) = PPPIN(I,J)
         APPINPF(I,J) = PPPINPF(I,J)
         ALUIN(I,J) = PLUIN(I,J)
         AOTIN(I,J) = POTIN(I,J)
         AOTTR(I,J) = POTTR(I,J)
         AMETR(I,J) = PMETR(I,J)
         AETTR(I,J) = PETTR(I,J)

! electricity prices at end use level
         AELSHRS(I,J) = PELSHRS(I,J)
         AELCLRS(I,J) = PELCLRS(I,J)
         AELWHRS(I,J) = PELWHRS(I,J)
         AELCKRS(I,J) = PELCKRS(I,J)
         AELCDRS(I,J) = PELCDRS(I,J)
         AELRFRS(I,J) = PELRFRS(I,J)
         AELFZRS(I,J) = PELFZRS(I,J)
         AELLTRS(I,J) = PELLTRS(I,J)
         AELOTRS(I,J) = PELOTRS(I,J)
         AELH2RS(I,J) = PELH2RS(I,J)
         AELVHRS(I,J) = PELVHRS(I,J) 
         AELSHCM(I,J) = PELSHCM(I,J)
         AELSCCM(I,J) = PELSCCM(I,J)
         AELWHCM(I,J) = PELWHCM(I,J)
         AELVTCM(I,J) = PELVTCM(I,J)
         AELCKCM(I,J) = PELCKCM(I,J)
         AELLTCM(I,J) = PELLTCM(I,J)
         AELRFCM(I,J) = PELRFCM(I,J)
         AELOPCM(I,J) = PELOPCM(I,J)
         AELONCM(I,J) = PELONCM(I,J)
         AELOTCM(I,J) = PELOTCM(I,J)
         AELP2CM(I,J) = PELP2CM(I,J)
         AELPFCM(I,J) = PELPFCM(I,J) 
         AELSBCM(I,J) = PELSBCM(I,J) 
         AELTBCM(I,J) = PELTBCM(I,J) 
         AELIBCM(I,J) = PELIBCM(I,J)
         AELFNCM(I,J) = PELFNCM(I,J) 
         AELINP(I,J) = PELINP(I,J)
         AELINS(I,J) = PELINS(I,J)
         AELINM(I,J) = PELINM(I,J)
         AELLTTR(I,J) = PELLTTR(I,J)
         !AELVHTR(I,J) = PELVHTR(I,J)
      ENDDO

!  MARGINAL PRICES BY COAL REGIONS
      DO I = 1,NDRG2
        APCLELCDR(1,I,J) = PCLELCDR(1,I,J)
        APCLELCDR(2,I,J) = PCLELCDR(2,I,J)
      END DO

!  natural GAS prices by EMM/NGMM REGIONS
      DO I = 1,NNGEM
         AGFELGR(I,J) = PGFELGR(I,J)
         AGIELGR(I,J) = PGIELGR(I,J)
         AGCELGR(I,J) = PGCELGR(I,J)
      ENDDO
! seasonal natural gas prices
      DO I = 1,NNGEM
        do nseas=1,3
         ASPNGELGR(I,J,nseas) = SPNGELGR(I,J,nseas)
        enddo
        APNGELGR(I,J) = PNGELGR(I,J)
      ENDDO
      DO I = 1,NNGEM
        do nseas=1,2
         ASPGFELGR(I,J,nseas) = SPGFELGR(I,J,nseas)
         ASPGIELGR(I,J,nseas) = SPGIELGR(I,J,nseas)
        enddo
      ENDDO

   endif

!WRITE(*,*)"copy_adjusted(): doneyet=",doneyet
! Read California shares of
     if(.not. doneyet) then
        doneyet=.true.
        pacific=file_mgr('O','CAFSHAREX         ',.false.)

!WRITE(*,*)"copy_adjusted(): now call CALL ReadRngXLSX(pacific,cafshare)"
!  read all defined ranges from worksheet
        CALL ReadRngXLSX(pacific,'cafshare')  ! sheet name is cafshare
        CLOSE(pacific)
!  fill arrays from worksheet range data
!  37 columns for each range: first column of range put in position 25 of *_SHR (for 2014 normally), but holds seds-based 2015 share (not 2014).
!  Second column goes into position 26, for 2015) and holds share computed for 2015 which should match 1st column.  37th column is 2050
!  Shares should change over time unless the spreadsheet option to holds shares constant overt time is on.

        rname='ELRS_SHR';call getrngr(rname, ELRS_SHR(25),1,37,1)
        rname='ELCM_SHR';call getrngr(rname, ELCM_SHR(25),1,37,1)
        rname='ELTR_SHR';call getrngr(rname, ELTR_SHR(25),1,37,1)
        rname='ELIN_SHR';call getrngr(rname, ELIN_SHR(25),1,37,1)
        rname='NGRS_SHR';call getrngr(rname, NGRS_SHR(25),1,37,1)
        rname='NGCM_SHR';call getrngr(rname, NGCM_SHR(25),1,37,1)
        rname='NGTR_SHR';call getrngr(rname, NGTR_SHR(25),1,37,1)
        rname='NGIN_SHR';call getrngr(rname, NGIN_SHR(25),1,37,1)
        rname='NGEL_SHR';call getrngr(rname, NGEL_SHR(25),1,37,1)
        rname='GPTR_SHR';call getrngr(rname, GPTR_SHR(25),1,37,1)
        rname='LPIN_SHR';call getrngr(rname, LPIN_SHR(25),1,37,1)
        rname='CLCM_SHR';call getrngr(rname, CLCM_SHR(25),1,37,1)
        rname='CLIN_SHR';call getrngr(rname, CLIN_SHR(25),1,37,1)
        rname='CLEL_SHR';call getrngr(rname, CLEL_SHR(25),1,37,1)
        rname='MGCM_SHR';call getrngr(rname, MGCM_SHR(25),1,37,1)
        rname='MGTR_SHR';call getrngr(rname, MGTR_SHR(25),1,37,1)
        rname='MGIN_SHR';call getrngr(rname, MGIN_SHR(25),1,37,1)
        rname='JFTR_SHR';call getrngr(rname, JFTR_SHR(25),1,37,1)
        rname='DSRS_SHR';call getrngr(rname, DSRS_SHR(25),1,37,1)
        rname='DSCM_SHR';call getrngr(rname, DSCM_SHR(25),1,37,1)
        rname='DSTR_SHR';call getrngr(rname, DSTR_SHR(25),1,37,1)
        rname='DSIN_SHR';call getrngr(rname, DSIN_SHR(25),1,37,1)
        rname='DSEL_SHR';call getrngr(rname, DSEL_SHR(25),1,37,1)
        rname='KSRS_SHR';call getrngr(rname, KSRS_SHR(25),1,37,1)
        rname='KSCM_SHR';call getrngr(rname, KSCM_SHR(25),1,37,1)
        rname='KSIN_SHR';call getrngr(rname, KSIN_SHR(25),1,37,1)
        rname='LGRS_SHR';call getrngr(rname, LGRS_SHR(25),1,37,1)
        rname='LGCM_SHR';call getrngr(rname, LGCM_SHR(25),1,37,1)
        rname='LGTR_SHR';call getrngr(rname, LGTR_SHR(25),1,37,1)
        rname='LGIN_SHR';call getrngr(rname, LGIN_SHR(25),1,37,1)
        rname='RSCM_SHR';call getrngr(rname, RSCM_SHR(25),1,37,1)
        rname='RSTR_SHR';call getrngr(rname, RSTR_SHR(25),1,37,1)
        rname='RSIN_SHR';call getrngr(rname, RSIN_SHR(25),1,37,1)
        rname='RSEL_SHR';call getrngr(rname, RSEL_SHR(25),1,37,1)
        rname='SGIN_SHR';call getrngr(rname, SGIN_SHR(25),1,37,1)
        rname='PCIN_SHR';call getrngr(rname, PCIN_SHR(25),1,37,1)
!        rname='PCEL_SHR';call getrngr(rname, PCEL_SHR(25),1,37,1)
        rname='PCAS_SHR';call getrngr(rname, PCAS_SHR(25),1,37,1)
        rname='ASIN_SHR';call getrngr(rname, ASIN_SHR(25),1,37,1)
        rname='OTTR_SHR';call getrngr(rname, OTTR_SHR(25),1,37,1)
        rname='OTIN_SHR';call getrngr(rname, OTIN_SHR(25),1,37,1)
        rname='ETTR_SHR';call getrngr(rname, ETTR_SHR(25),1,37,1)
        rname='UREL_SHR';call getrngr(rname, UREL_SHR(25),1,37,1)
        rname='HOIN_SHR';call getrngr(rname, HOIN_SHR(25),1,37,1)
        rname='HOEL_SHR';call getrngr(rname, HOEL_SHR(25),1,37,1)
        rname='GERS_SHR';call getrngr(rname, GERS_SHR(25),1,37,1)
        rname='GEIN_SHR';call getrngr(rname, GEIN_SHR(25),1,37,1)
        rname='GEEL_SHR';call getrngr(rname, GEEL_SHR(25),1,37,1)
        rname='BMRS_SHR';call getrngr(rname, BMRS_SHR(25),1,37,1)
        rname='BMCM_SHR';call getrngr(rname, BMCM_SHR(25),1,37,1)
        rname='BMIN_SHR';call getrngr(rname, BMIN_SHR(25),1,37,1)
        rname='BMEL_SHR';call getrngr(rname, BMEL_SHR(25),1,37,1)
        rname='MSIN_SHR';call getrngr(rname, MSIN_SHR(25),1,37,1)
        rname='STRS_SHR';call getrngr(rname, STRS_SHR(25),1,37,1)
        rname='STCM_SHR';call getrngr(rname, STCM_SHR(25),1,37,1)
        rname='STIN_SHR';call getrngr(rname, STIN_SHR(25),1,37,1)
        rname='STEL_SHR';call getrngr(rname, STEL_SHR(25),1,37,1)
        rname='WIIN_SHR';call getrngr(rname, WIIN_SHR(25),1,37,1)
        rname='WIEL_SHR';call getrngr(rname, WIEL_SHR(25),1,37,1)
        rname='EIEL_SHR';call getrngr(rname, EIEL_SHR(25),1,37,1)

     endif


     if(curiyr.eq.lastyr.and. FCRL.eq.1 .and.  .not. done) then
!WRITE(*,*)"copy_adjusted(): inside if(curiyr.eq.lastyr.and. FCRL.eq.1 .and.  .not. done)--------"
! write out Pacific Division energy consumption to csv file. Is used to compile California energy sharing assumptions
! read from input spreadsheet cafshare.xml for ab32 calculations
       done=.true.
925    format(a,',',f8.5,',',<MNUMYR>(F10.3,','))   ! csv format
       new=.true.
       pacific=file_mgr('O','PACIFIC           ',new)


       if(QSELRS(9,MSEDYR).gt.0.) write(pacific,925) 'ELrs', QSELRS(10,MSEDYR) / QSELRS(9,MSEDYR), (QELRS(9,I),i=msedyr,lastyr)!  1 Purchased Electricity - Residential
       if(QSELCM(9,MSEDYR).gt.0.) write(pacific,925) 'ELcm', QSELCM(10,MSEDYR) / QSELCM(9,MSEDYR), (QELCM(9,I),i=msedyr,lastyr)!  2 Purchased Electricity - Commercial
       if(QSELTR(9,MSEDYR).gt.0.) write(pacific,925) 'ELtr', QSELTR(10,MSEDYR) / QSELTR(9,MSEDYR), (QELTR(9,I),i=msedyr,lastyr)!  3 Purchased Electricity - Transportation
       if(QSELIN(9,MSEDYR).gt.0.) write(pacific,925) 'ELin', QSELIN(10,MSEDYR) / QSELIN(9,MSEDYR), (QELIN(9,I),i=msedyr,lastyr)!  4 Purchased Electricity - Industrial
       if(QSELRF(9,MSEDYR).gt.0.) write(pacific,925) 'ELrf', QSELRF(10,MSEDYR) / QSELRF(9,MSEDYR), (QELRF(9,I),i=msedyr,lastyr)!  5 Purchased Electricity - Refinery
       if(QSELHM(9,MSEDYR).gt.0.) write(pacific,925) 'ELhm', QSELHM(10,MSEDYR) / QSELHM(9,MSEDYR), (QELHM(9,I),i=msedyr,lastyr)!  6 Purchased Electricity - Hydrogen

       if(QSNGRS(9,MSEDYR).gt.0.) write(pacific,925) 'NGrs', QSNGRS(10,MSEDYR) / QSNGRS(9,MSEDYR), (QNGRS(9,I),i=msedyr,lastyr)! 24 Natural Gas - Residential
       if(QSNGCM(9,MSEDYR).gt.0.) write(pacific,925) 'NGcm', QSNGCM(10,MSEDYR) / QSNGCM(9,MSEDYR), (QNGCM(9,I),i=msedyr,lastyr)! 25 Natural Gas - Commercial
       if(QSNGTR(9,MSEDYR).gt.0.) write(pacific,925) 'NGtr', QSNGTR(10,MSEDYR) / QSNGTR(9,MSEDYR), (QNGTR(9,I),i=msedyr,lastyr)! 26 Natural Gas - Transportation
       if(QSNGIN(9,MSEDYR).gt.0.) write(pacific,925) 'NGin', QSNGIN(10,MSEDYR) / QSNGIN(9,MSEDYR), (QNGIN(9,I),i=msedyr,lastyr)! 27 Natural Gas - Industrial
       if(QSNGRF(9,MSEDYR).gt.0.) write(pacific,925) 'NGrf', QSNGRF(10,MSEDYR) / QSNGRF(9,MSEDYR), (QNGRF(9,I),i=msedyr,lastyr)! 28 Natural Gas - Refinery
       if(QSNGEL(9,MSEDYR).gt.0.) write(pacific,925) 'NGel', QSNGEL(10,MSEDYR) / QSNGEL(9,MSEDYR), (QNGEL(9,I),i=msedyr,lastyr)! 29 Natural Gas - Electricity
       if(QSNGHM(9,MSEDYR).gt.0.) write(pacific,925) 'NGhm', QSNGHM(10,MSEDYR) / QSNGHM(9,MSEDYR), (QNGHM(9,I),i=msedyr,lastyr)! 30 Natural Gas - Hydrogen
       if(QSGPTR(9,MSEDYR).gt.0.) write(pacific,925) 'GPtr', QSGPTR(10,MSEDYR) / QSGPTR(9,MSEDYR), (QGPTR(9,I),i=msedyr,lastyr)! 32 Natural Gas - Pipeline
       if(QSLPIN(9,MSEDYR).gt.0.) write(pacific,925) 'LPin', QSLPIN(10,MSEDYR) / QSLPIN(9,MSEDYR), (QLPIN(9,I),i=msedyr,lastyr)! 33 Natural Gas - Lease and Plant Fuel

       if(QSCLRS(9,MSEDYR).gt.0.) write(pacific,925) 'CLrs', QSCLRS(10,MSEDYR) / QSCLRS(9,MSEDYR), (QCLRS(9,I),i=msedyr,lastyr)! 34 Coal - Residential
       if(QSCLCM(9,MSEDYR).gt.0.) write(pacific,925) 'CLcm', QSCLCM(10,MSEDYR) / QSCLCM(9,MSEDYR), (QCLCM(9,I),i=msedyr,lastyr)! 35 Coal - Commercial
       if(QSCLIN(9,MSEDYR).gt.0.) write(pacific,925) 'CLin', QSCLIN(10,MSEDYR) / QSCLIN(9,MSEDYR), (QCLIN(9,I),i=msedyr,lastyr)! 36 Coal - Industrial
       if(QSCLRF(9,MSEDYR).gt.0.) write(pacific,925) 'CLrf', QSCLRF(10,MSEDYR) / QSCLRF(9,MSEDYR), (QCLRF(9,I),i=msedyr,lastyr)! 37 Coal - Refinery
       if(QSCLEL(9,MSEDYR).gt.0.) write(pacific,925) 'CLel', QSCLEL(10,MSEDYR) / QSCLEL(9,MSEDYR), (QCLEL(9,I),i=msedyr,lastyr)! 38 Coal - Electricity
       if(QSCLSN(9,MSEDYR).gt.0.) write(pacific,925) 'CLsn', QSCLSN(10,MSEDYR) / QSCLSN(9,MSEDYR), (QCLSN(9,I),i=msedyr,lastyr)! 39 Coal - Synthetics
       if(QSCLHM(9,MSEDYR).gt.0.) write(pacific,925) 'CLhm', QSCLHM(10,MSEDYR) / QSCLHM(9,MSEDYR), (QCLHM(9,I),i=msedyr,lastyr)! 40 Coal - Hydrogen

       if(QSMCIN(9,MSEDYR).gt.0.) write(pacific,925) 'MCin', QSMCIN(10,MSEDYR) / QSMCIN(9,MSEDYR), (QMCIN(9,I),i=msedyr,lastyr)! 42 Metallurgical Coal - Industrial
       if(QSMGCM(9,MSEDYR).gt.0.) write(pacific,925) 'MGcm', QSMGCM(10,MSEDYR) / QSMGCM(9,MSEDYR), (QMGCM(9,I),i=msedyr,lastyr)! 43 Motor Gasoline - Commercial
       if(QSMGTR(9,MSEDYR).gt.0.) write(pacific,925) 'MGtr', QSMGTR(10,MSEDYR) / QSMGTR(9,MSEDYR), (QMGTR(9,I),i=msedyr,lastyr)! 44 Motor Gasoline - Transportation
       if(QSMGIN(9,MSEDYR).gt.0.) write(pacific,925) 'MGin', QSMGIN(10,MSEDYR) / QSMGIN(9,MSEDYR), (QMGIN(9,I),i=msedyr,lastyr)! 45 Motor Gasoline - Industrial

       if(QSJFTR(9,MSEDYR).gt.0.) write(pacific,925) 'JFtr', QSJFTR(10,MSEDYR) / QSJFTR(9,MSEDYR), (QJFTR(9,I),i=msedyr,lastyr)! 47 Jet Fuel - Transportation
       if(QSDSRS(9,MSEDYR).gt.0.) write(pacific,925) 'DSrs', QSDSRS(10,MSEDYR) / QSDSRS(9,MSEDYR), (QDSRS(9,I),i=msedyr,lastyr)! 48 Distillate - Residential
       if(QSDSCM(9,MSEDYR).gt.0.) write(pacific,925) 'DScm', QSDSCM(10,MSEDYR) / QSDSCM(9,MSEDYR), (QDSCM(9,I),i=msedyr,lastyr)! 49 Distillate - Commercial
       if(QSDSTR(9,MSEDYR).gt.0.) write(pacific,925) 'DStr', QSDSTR(10,MSEDYR) / QSDSTR(9,MSEDYR), (QDSTR(9,I),i=msedyr,lastyr)! 50 Distillate - Transportation
       if(QSDSIN(9,MSEDYR).gt.0.) write(pacific,925) 'DSin', QSDSIN(10,MSEDYR) / QSDSIN(9,MSEDYR), (QDSIN(9,I),i=msedyr,lastyr)! 51 Distillate - Industrial
       if(QSDSRF(9,MSEDYR).gt.0.) write(pacific,925) 'DSrf', QSDSRF(10,MSEDYR) / QSDSRF(9,MSEDYR), (QDSRF(9,I),i=msedyr,lastyr)! 52 Distillate - Refinery
       if(QSDSEL(9,MSEDYR).gt.0.) write(pacific,925) 'DSel', QSDSEL(10,MSEDYR) / QSDSEL(9,MSEDYR), (QDSEL(9,I),i=msedyr,lastyr)! 53 Distillate - Electricity (+petroleum coke)

       if(QSKSRS(9,MSEDYR).gt.0.) write(pacific,925) 'KSrs', QSKSRS(10,MSEDYR) / QSKSRS(9,MSEDYR), (QKSRS(9,I),i=msedyr,lastyr)! 55 Kerosene - Residential
       if(QSKSCM(9,MSEDYR).gt.0.) write(pacific,925) 'KScm', QSKSCM(10,MSEDYR) / QSKSCM(9,MSEDYR), (QKSCM(9,I),i=msedyr,lastyr)! 56 Kerosene - Commercial
       if(QSKSIN(9,MSEDYR).gt.0.) write(pacific,925) 'KSin', QSKSIN(10,MSEDYR) / QSKSIN(9,MSEDYR), (QKSIN(9,I),i=msedyr,lastyr)! 57 Kerosene - Industrial

       if(QSLGRS(9,MSEDYR).gt.0.) write(pacific,925) 'LGrs', QSLGRS(10,MSEDYR) / QSLGRS(9,MSEDYR), (QLGRS(9,I),i=msedyr,lastyr)! 59 Liquid Petroleum Gases - Residential
       if(QSLGCM(9,MSEDYR).gt.0.) write(pacific,925) 'LGcm', QSLGCM(10,MSEDYR) / QSLGCM(9,MSEDYR), (QLGCM(9,I),i=msedyr,lastyr)! 60 Liquid Petroleum Gases - Commercial
       if(QSLGTR(9,MSEDYR).gt.0.) write(pacific,925) 'LGtr', QSLGTR(10,MSEDYR) / QSLGTR(9,MSEDYR), (QLGTR(9,I),i=msedyr,lastyr)! 61 Liquid Petroleum Gases - Transportation
       if(QSLGIN(9,MSEDYR).gt.0.) write(pacific,925) 'LGin', QSLGIN(10,MSEDYR) / QSLGIN(9,MSEDYR), (QLGIN(9,I),i=msedyr,lastyr)! 62 Liquid Petroleum Gases - Industrial
       if(QSLGRF(9,MSEDYR).gt.0.) write(pacific,925) 'LGrf', QSLGRF(10,MSEDYR) / QSLGRF(9,MSEDYR), (QLGRF(9,I),i=msedyr,lastyr)! 63 Liquid Petroleum Gases - Refinery

       if(QSRSCM(9,MSEDYR).gt.0.) write(pacific,925) 'RScm', QSRSCM(10,MSEDYR) / QSRSCM(9,MSEDYR), (QRSCM(9,I),i=msedyr,lastyr)! 74 Residual Fuel - Commercial
       if(QSRSTR(9,MSEDYR).gt.0.) write(pacific,925) 'RStr', QSRSTR(10,MSEDYR) / QSRSTR(9,MSEDYR), (QRSTR(9,I),i=msedyr,lastyr)! 75 Residual Fuel - Transportation
       if(QSRSIN(9,MSEDYR).gt.0.) write(pacific,925) 'RSin', QSRSIN(10,MSEDYR) / QSRSIN(9,MSEDYR), (QRSIN(9,I),i=msedyr,lastyr)! 76 Residual Fuel - Industrial
       if(QSRSRF(9,MSEDYR).gt.0.) write(pacific,925) 'RSrf', QSRSRF(10,MSEDYR) / QSRSRF(9,MSEDYR), (QRSRF(9,I),i=msedyr,lastyr)! 77 Residual Fuel - Refinery
       if(QSRSEL(9,MSEDYR).gt.0.) write(pacific,925) 'RSel', QSRSEL(10,MSEDYR) / QSRSEL(9,MSEDYR), (QRSEL(9,I),i=msedyr,lastyr)! 78 Residual Fuel - Electricity

       if(QSPFIN(9,MSEDYR).gt.0.) write(pacific,925) 'PFin', QSPFIN(10,MSEDYR) / QSPFIN(9,MSEDYR), (QPFIN(9,I),i=msedyr,lastyr)! 80 Petrochemical Feedstocks - Industrial
       if(QSSGIN(9,MSEDYR).gt.0.) write(pacific,925) 'SGin', QSSGIN(10,MSEDYR) / QSSGIN(9,MSEDYR), (QSGIN(9,I),i=msedyr,lastyr)! 81 Still Gas - Industrial
       if(QSSGRF(9,MSEDYR).gt.0.) write(pacific,925) 'SGrf', QSSGRF(10,MSEDYR) / QSSGRF(9,MSEDYR), (QSGRF(9,I),i=msedyr,lastyr)! 82 Still Gas - Refinery
       if(QSPCIN(9,MSEDYR).gt.0.) write(pacific,925) 'PCin', QSPCIN(10,MSEDYR) / QSPCIN(9,MSEDYR), (QPCIN(9,I),i=msedyr,lastyr)! 83 Petroleum Coke - Industrial
       if(QSPCRF(9,MSEDYR).gt.0.) write(pacific,925) 'PCrf', QSPCRF(10,MSEDYR) / QSPCRF(9,MSEDYR), (QPCRF(9,I),i=msedyr,lastyr)! 84 Petroleum Coke - Refinery
       if(QSPCEL(9,MSEDYR).gt.0.) write(pacific,925) 'PCel', QSPCEL(10,MSEDYR) / QSPCEL(9,MSEDYR), (QPCEL(9,I),i=msedyr,lastyr)! 85 Petroleum Coke - Electricity
       if(QSPCAS(9,MSEDYR).gt.0.) write(pacific,925) 'PCas', QSPCAS(10,MSEDYR) / QSPCAS(9,MSEDYR), (QPCAS(9,I),i=msedyr,lastyr)! 86 Petroleum Coke - All Sectors
       if(QSASIN(9,MSEDYR).gt.0.) write(pacific,925) 'ASin', QSASIN(10,MSEDYR) / QSASIN(9,MSEDYR), (QASIN(9,I),i=msedyr,lastyr)! 87 Asphalt and Road Oil - Industrial
       if(QSOTTR(9,MSEDYR).gt.0.) write(pacific,925) 'OTtr', QSOTTR(10,MSEDYR) / QSOTTR(9,MSEDYR), (QOTTR(9,I),i=msedyr,lastyr)! 88 Other Petroleum - Transportation
       if(QSOTIN(9,MSEDYR).gt.0.) write(pacific,925) 'OTin', QSOTIN(10,MSEDYR) / QSOTIN(9,MSEDYR), (QOTIN(9,I),i=msedyr,lastyr)! 89 Other Petroleum - Industrial
       if(QSOTRF(9,MSEDYR).gt.0.) write(pacific,925) 'OTrf', QSOTRF(10,MSEDYR) / QSOTRF(9,MSEDYR), (QOTRF(9,I),i=msedyr,lastyr)! 90 Other Petroleum - Refinery

       if(QSMETR(9,MSEDYR).gt.0.) write(pacific,925) 'MEtr', QSMETR(10,MSEDYR) / QSMETR(9,MSEDYR), (QMETR(9,I),i=msedyr,lastyr)! 99 Methanol - Transporation
       if(QSETTR(9,MSEDYR).gt.0.) write(pacific,925) 'ETtr', QSETTR(10,MSEDYR) / QSETTR(9,MSEDYR), (QETTR(9,I),i=msedyr,lastyr)!100 Ethanol - Transporation
       if(QSETHM(9,MSEDYR).gt.0.) write(pacific,925) 'EThm', QSETHM(10,MSEDYR) / QSETHM(9,MSEDYR), (QETHM(9,I),i=msedyr,lastyr)!101 Ethanol - Hydrogen
       if(QSHYTR(9,MSEDYR).gt.0.) write(pacific,925) 'HYtr', QSHYTR(10,MSEDYR) / QSHYTR(9,MSEDYR), (QHYTR(9,I),i=msedyr,lastyr)!102 Liquid Hydrogen - Transportation
       if(QSUREL(9,MSEDYR).gt.0.) write(pacific,925) 'URel', QSUREL(10,MSEDYR) / QSUREL(9,MSEDYR), (QUREL(9,I),i=msedyr,lastyr)!103 Uranium - Electricity
       if(QSURHM(9,MSEDYR).gt.0.) write(pacific,925) 'URhm', QSURHM(10,MSEDYR) / QSURHM(9,MSEDYR), (QURHM(9,I),i=msedyr,lastyr)!104 Uranium - Hydrogen
       if(QSHOIN(9,MSEDYR).gt.0.) write(pacific,925) 'HOin', QSHOIN(10,MSEDYR) / QSHOIN(9,MSEDYR), (QHOIN(9,I),i=msedyr,lastyr)!105 Hydropower - Industrial
       if(QSHOEL(9,MSEDYR).gt.0.) write(pacific,925) 'HOel', QSHOEL(10,MSEDYR) / QSHOEL(9,MSEDYR), (QHOEL(9,I),i=msedyr,lastyr)!106 Hydropower - Electricity

       if(QSGERS(9,MSEDYR).gt.0.) write(pacific,925) 'GErs', QSGERS(10,MSEDYR) / QSGERS(9,MSEDYR), (QGERS(9,I),i=msedyr,lastyr)!108 Geothermal - Residential
       if(QSGEIN(9,MSEDYR).gt.0.) write(pacific,925) 'GEin', QSGEIN(10,MSEDYR) / QSGEIN(9,MSEDYR), (QGEIN(9,I),i=msedyr,lastyr)!109 Geothermal - Industrial
       if(QSGEEL(9,MSEDYR).gt.0.) write(pacific,925) 'GEel', QSGEEL(10,MSEDYR) / QSGEEL(9,MSEDYR), (QGEEL(9,I),i=msedyr,lastyr)!110 Geothermal - Electricity

       if(QSBMRS(9,MSEDYR).gt.0.) write(pacific,925) 'BMrs', QSBMRS(10,MSEDYR) / QSBMRS(9,MSEDYR), (QBMRS(9,I),i=msedyr,lastyr)!112 Biomass - Residential
       if(QSBMCM(9,MSEDYR).gt.0.) write(pacific,925) 'BMcm', QSBMCM(10,MSEDYR) / QSBMCM(9,MSEDYR), (QBMCM(9,I),i=msedyr,lastyr)!113 Biomass - Commercial
       if(QSBMIN(9,MSEDYR).gt.0.) write(pacific,925) 'BMin', QSBMIN(10,MSEDYR) / QSBMIN(9,MSEDYR), (QBMIN(9,I),i=msedyr,lastyr)!114 Biomass - Industrial
       if(QSBMRF(9,MSEDYR).gt.0.) write(pacific,925) 'BMrf', QSBMRF(10,MSEDYR) / QSBMRF(9,MSEDYR), (QBMRF(9,I),i=msedyr,lastyr)!115 Biomass - Refinery
       if(QSBMEL(9,MSEDYR).gt.0.) write(pacific,925) 'BMel', QSBMEL(10,MSEDYR) / QSBMEL(9,MSEDYR), (QBMEL(9,I),i=msedyr,lastyr)!116 Biomass - Electricity
       if(QSBMSN(9,MSEDYR).gt.0.) write(pacific,925) 'BMsn', QSBMSN(10,MSEDYR) / QSBMSN(9,MSEDYR), (QBMSN(9,I),i=msedyr,lastyr)!117 Biomass - Synthetics
       if(QSBMHM(9,MSEDYR).gt.0.) write(pacific,925) 'BMhm', QSBMHM(10,MSEDYR) / QSBMHM(9,MSEDYR), (QBMHM(9,I),i=msedyr,lastyr)!118 Biomass - Hydrogen

       if(QSMSIN(9,MSEDYR).gt.0.) write(pacific,925) 'MSin', QSMSIN(10,MSEDYR) / QSMSIN(9,MSEDYR), (QMSIN(9,I),i=msedyr,lastyr)!120 Municipal Solid Waste - Industrial
       if(QSMSEL(9,MSEDYR).gt.0.) write(pacific,925) 'MSel', QSMSEL(10,MSEDYR) / QSMSEL(9,MSEDYR), (QMSEL(9,I),i=msedyr,lastyr)!121 Municipal Solid Waste - Electricity

       if(QSSTRS(9,MSEDYR).gt.0.) write(pacific,925) 'STrs', QSSTRS(10,MSEDYR) / QSSTRS(9,MSEDYR), (QSTRS(9,I),i=msedyr,lastyr)!123 Solar Thermal - Residential
       if(QSSTCM(9,MSEDYR).gt.0.) write(pacific,925) 'STcm', QSSTCM(10,MSEDYR) / QSSTCM(9,MSEDYR), (QSTCM(9,I),i=msedyr,lastyr)!124 Solar Thermal - Commercial
       if(QSSTIN(9,MSEDYR).gt.0.) write(pacific,925) 'STin', QSSTIN(10,MSEDYR) / QSSTIN(9,MSEDYR), (QSTIN(9,I),i=msedyr,lastyr)!125 Solar Thermal - Industrial
       if(QSSTEL(9,MSEDYR).gt.0.) write(pacific,925) 'STel', QSSTEL(10,MSEDYR) / QSSTEL(9,MSEDYR), (QSTEL(9,I),i=msedyr,lastyr)!126 Solar Thermal - Electricity

       if(QSPVRS(9,MSEDYR).gt.0.) write(pacific,925) 'PVrs', QSPVRS(10,MSEDYR) / QSPVRS(9,MSEDYR), (QPVRS(9,I),i=msedyr,lastyr)!128 Photovoltaic - Residential
       if(QSPVCM(9,MSEDYR).gt.0.) write(pacific,925) 'PVcm', QSPVCM(10,MSEDYR) / QSPVCM(9,MSEDYR), (QPVCM(9,I),i=msedyr,lastyr)!129 Photovoltaic - Commercial
       if(QSPVIN(9,MSEDYR).gt.0.) write(pacific,925) 'PVin', QSPVIN(10,MSEDYR) / QSPVIN(9,MSEDYR), (QPVIN(9,I),i=msedyr,lastyr)!130 Photovoltaic - Industrial
       if(QSPVEL(9,MSEDYR).gt.0.) write(pacific,925) 'PVel', QSPVEL(10,MSEDYR) / QSPVEL(9,MSEDYR), (QPVEL(9,I),i=msedyr,lastyr)!131 Photovoltaic - Electricity

       if(QSWIIN(9,MSEDYR).gt.0.) write(pacific,925) 'WIin', QSWIIN(10,MSEDYR) / QSWIIN(9,MSEDYR), (QWIIN(9,I),i=msedyr,lastyr)!133 Wind - Industrial
       if(QSWIEL(9,MSEDYR).gt.0.) write(pacific,925) 'WIel', QSWIEL(10,MSEDYR) / QSWIEL(9,MSEDYR), (QWIEL(9,I),i=msedyr,lastyr)!134 Wind - Electricity

       if(QSEIEL(9,MSEDYR).gt.0.) write(pacific,925) 'EIel', QSEIEL(10,MSEDYR) / QSEIEL(9,MSEDYR), (QEIEL(9,I),i=msedyr,lastyr)!144 Net Electricity Imports - Electricity
       if(QSCIIN(9,MSEDYR).gt.0.) write(pacific,925) 'CIIN', QSCIIN(10,MSEDYR) / QSCIIN(9,MSEDYR), (QCIIN(9,I),i=msedyr,lastyr)!145 Net Coal Coke Imports - Industrial


       pacific=file_mgr('C','PACIFIC           ',new)
     endif



! check AB32 switch.  If on, apply prices and calculate combustion emissions for fuel sold by fuel producers to
! entities not otherwise explicitly covered.  For petroleum pricing, price adjustments will be added in LFMM so are omitted here by
! commenting out the relevant lines with "!!!!"

   AB32SW=rtovalue('AB32SW  ',1)
!WRITE(*,*)"copy_adjusted(): scedes.all =1, here rtovalue(AB32SW)=",AB32SW
   if(curcalyr.ge.2015) then
    if(AB32SW.eq.1) then
!WRITE(*,*)"copy_adjusted(): (curcalyr.ge.2015) and (AB32SW.eq.1)"
     if(ab_allow_p(curiyr) .gt. 0.0) then

!      add ab32 carbon value to california's prices beginning 2015 for resid, commerc, and trans sectors.
!      re-weight Division 9 price using last year of SEDS data
       ANGRS(9,CURIYR)=AB32P9(ANGRS(9,CURIYR),QSNGRS(9,MSEDYR),QSNGRS(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGRS(CURIYR))
       ADSRS(9,CURIYR)=AB32P9(ADSRS(9,CURIYR),QSDSRS(9,MSEDYR),QSDSRS(10,MSEDYR),AB_ALLOW_P(CURIYR),EDSRS(CURIYR))
       ALGRS(9,CURIYR)=AB32P9(ALGRS(9,CURIYR),QSLGRS(9,MSEDYR),QSLGRS(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRRS(CURIYR))
       APRRS(9,CURIYR)=AB32P9(APRRS(9,CURIYR),QSLGRS(9,MSEDYR),QSLGRS(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRRS(CURIYR))
       AKSRS(9,CURIYR)=AB32P9(AKSRS(9,CURIYR),QSKSRS(9,MSEDYR),QSKSRS(10,MSEDYR),AB_ALLOW_P(CURIYR),EKSRS(CURIYR))

       ANGCM(9,CURIYR)=AB32P9(ANGCM(9,CURIYR),QSNGCM(9,MSEDYR),QSNGCM(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGCM(CURIYR))
       ADSCM(9,CURIYR)=AB32P9(ADSCM(9,CURIYR),QSDSCM(9,MSEDYR),QSDSCM(10,MSEDYR),AB_ALLOW_P(CURIYR),EDSCM(CURIYR))
       ALGCM(9,CURIYR)=AB32P9(ALGCM(9,CURIYR),QSLGCM(9,MSEDYR),QSLGCM(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRCM(CURIYR))
       APRCM(9,CURIYR)=AB32P9(APRCM(9,CURIYR),QSLGCM(9,MSEDYR),QSLGCM(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRCM(CURIYR))
       AMGCM(9,CURIYR)=AB32P9(AMGCM(9,CURIYR),QSMGCM(9,MSEDYR),QSMGCM(10,MSEDYR),AB_ALLOW_P(CURIYR),EMGCM(CURIYR))
       AKSCM(9,CURIYR)=AB32P9(AKSCM(9,CURIYR),QSKSCM(9,MSEDYR),QSKSCM(10,MSEDYR),AB_ALLOW_P(CURIYR),EKSCM(CURIYR))

! for natural gas use by trains (1=freight; 2=intercity; 3=transit; 4=commuter)
!                     and boats (1=domestic shipping; 2=international shipping; 3=recreational)
! the CO2 calculation assumes all of this is covered, despite some being international shipping, so to be consistent with that,
! we are applying the allowance price adder to each price.  this makes ANGTR the same, rather than have to be recalculated
! based on all the pieces, whew.
       AGFTRFV(9,CURIYR)=AB32P9(AGFTRFV(9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRPV(9,CURIYR)=AB32P9(AGFTRPV(9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRFV(9,CURIYR)=AB32P9(AGLTRFV(9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRPV(9,CURIYR)=AB32P9(AGLTRPV(9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRRAIL(1,9,CURIYR)=AB32P9(AGFTRRAIL(1,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRRAIL(2,9,CURIYR)=AB32P9(AGFTRRAIL(2,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRRAIL(3,9,CURIYR)=AB32P9(AGFTRRAIL(3,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRRAIL(4,9,CURIYR)=AB32P9(AGFTRRAIL(4,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRRAIL(1,9,CURIYR)=AB32P9(AGLTRRAIL(1,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRRAIL(2,9,CURIYR)=AB32P9(AGLTRRAIL(2,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRRAIL(3,9,CURIYR)=AB32P9(AGLTRRAIL(3,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRRAIL(4,9,CURIYR)=AB32P9(AGLTRRAIL(4,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRSHIP(1,9,CURIYR)=AB32P9(AGFTRSHIP(1,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRSHIP(2,9,CURIYR)=AB32P9(AGFTRSHIP(2,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGFTRSHIP(3,9,CURIYR)=AB32P9(AGFTRSHIP(3,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRSHIP(1,9,CURIYR)=AB32P9(AGLTRSHIP(1,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRSHIP(2,9,CURIYR)=AB32P9(AGLTRSHIP(2,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       AGLTRSHIP(3,9,CURIYR)=AB32P9(AGLTRSHIP(3,9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       ANGTR(9,CURIYR)=AB32P9(ANGTR(9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
       ALGTR(9,CURIYR)=AB32P9(ALGTR(9,CURIYR),QSLGTR(9,MSEDYR),QSLGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRTR(CURIYR))
       APRTR(9,CURIYR)=AB32P9(APRTR(9,CURIYR),QSLGTR(9,MSEDYR),QSLGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),EPRTR(CURIYR))

       emfac_qmgtr=emgtr(curiyr)
       emfac_qdstr=edstr(curiyr)
       if(qmgtr(9,curiyr).gt.0.) emfac_qmgtr=(em_tran(1,9,curcalyr)+em_tran(3,9,curcalyr))/qmgtr(9,curiyr)
       if(qmgtr(9,curiyr).gt.0.) emfac_qettr= em_tran(2,9,curcalyr)/qmgtr(9,curiyr)
       if(qdstr(9,curiyr).gt.0.) emfac_qdstr=(em_tran(6,9,curcalyr)+em_tran(7,9,curcalyr))/qdstr(9,curiyr)
       emfac_qettr=emgtr(curiyr) * (TRGNE85*CFRBOB(J)) / (TRGNE85*CFRBOB(J)+ETHNE85*CFETQ(J))


       AMGTR(9,CURIYR)=AB32P9(AMGTR(9,CURIYR),QSMGTR(9,MSEDYR),QSMGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),emfac_qmgtr)
       AETTR(9,CURIYR)=AB32P9(AETTR(9,CURIYR),QSMGTR(9,MSEDYR),QSMGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),emfac_qettr)
       ADSTR(9,CURIYR)=AB32P9(ADSTR(9,CURIYR),QSDSTR(9,MSEDYR),QSDSTR(10,MSEDYR),AB_ALLOW_P(CURIYR),emfac_qdstr)
       AJFTR(9,CURIYR)=AB32P9(AJFTR(9,CURIYR),QSJFTR(9,MSEDYR),QSJFTR(10,MSEDYR)*AB32JETCOVER(CURIYR),AB_ALLOW_P(CURIYR),EJFTR(CURIYR))

       ADSTRHWY(9,CURIYR)=AB32P9(ADSTRHWY(9,CURIYR),QSDSTR(9,MSEDYR),QSDSTR(10,MSEDYR),AB_ALLOW_P(CURIYR),EDSTR(CURIYR))
       AGFTRPV (9,CURIYR)=AB32P9(AGFTRPV (9,CURIYR),QSNGTR(9,MSEDYR),QSNGTR(10,MSEDYR),AB_ALLOW_P(CURIYR),ENGTR(CURIYR))
     endif
    endif


! calculate california share of emissions covered by fuel providers for non-covered entities
     ab_covd_em_fue(curiyr)=0.
     ab_covd_em_fue(curiyr)=ab_covd_em_fue(curiyr)+ sum(em_resd(1:5,10,curcalyr))

     ab_covd_em_fue(curiyr)=ab_covd_em_fue(curiyr)+ sum(em_comm(1:7,10,curcalyr))

     ab_covd_em_fue(curiyr)=ab_covd_em_fue(curiyr)+sum(em_tran(1:11,10,curcalyr))

     ab_covd_em_fue(curiyr)=ab_covd_em_fue(curiyr)*.001

! For industrial, we are essentially adding gas and oil CO2 emissions that are not explicitly covered.  So this amounts to total industrial
! emissions minus the covered california emissions as calculated by industrial and refining.
! So first calculate total industrial CO2 emissions for California

     cal_ind_emis=0.
     cal_ind_emis=cal_ind_emis+ sum(em_indy( 1:16,10,curcalyr))

!	 write(6,'(A)') '[INTEGRATION] Module time removed (7968-7974)'
!     write(6,'(a,i5,i3,4F12.3)') 'curcalyr,curitr,ab_covd_em_fue/ind/ref,cal_ind_emis: ', &
!     curcalyr,curitr, &
!     ab_covd_em_fue(curiyr), &
!     ab_covd_em_ind(curiyr), &
!     ab_covd_em_ref(curiyr), &
!     cal_ind_emis*.001
! subtract out emissions from narrow scope covered entities to get broad scope fuel emissions
     cal_ind_emis=cal_ind_emis*.001 - ab_covd_em_ind(curiyr) - ab_covd_em_ref(curiyr)
     if(cal_ind_emis.lt.0) then
       cal_ind_emis=0.
     endif

     ab_covd_em_fue(curiyr)=ab_covd_em_fue(curiyr)+cal_ind_emis

   endif

! calculate the total covered emissions
     ab_covd_em_tot(curiyr)=ab_covd_em_ele(curiyr)+ &
                            ab_covd_em_ref(curiyr)+ &
                            ab_covd_em_ind(curiyr)+ &
                            ab_covd_em_fue(curiyr)+ &
                            ab_covd_em_oth(curiyr)


   CLOSE(10)
   RETURN
   END
   
!end module adjust_epm_price
!============================================
!*******************************************************************
!  FUNCTION AB32P9(Pdiv9,Qsdiv9,Qsca,allow_p,emfact)
!*******************************************************************
   FUNCTION AB32P9(Pdiv9,Qsdiv9,Qsca,allow_p,emfact)
! function to increment Division 9 (Pacific) price after adding allowance price to CA price
   implicit none
   real AB32P9 ! function to increment Division 9 (Pacific) price after adding allowance price to CA price
   real Pdiv9  ! Division 9 price
   real Qsdiv9 ! SEDS history consumption for Div 9
   real Qsca   ! SEDS history consumption for california
   real allow_p ! allowance price in 87$/kg-C
   real emfact  ! emission factor kgC/mmbtu

   real Pca     ! california price after adding allowance cost

   Pca = Pdiv9 + (allow_p*emfact)

   AB32P9= Pdiv9  ! default value if quantity zero

   if(Qsdiv9.gt.0.) then
     AB32P9= (Pdiv9*(Qsdiv9-Qsca) + Pca*Qsca) / Qsdiv9
   endif

   return
   end
