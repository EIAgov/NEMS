! $Header: m:/default/source/RCS/wellon.f,v 1.262 2020/11/06 19:02:46 DH5 Exp $
!***************************************************************
!from OLOGSS.FOR
!***************************************************************
!
       SUBROUTINE OGMAIN_ON

       use ctus_mod, only : read_co2
       
       IMPLICIT NONE

       include 'parametr'     ! nems dimension parameters
       include 'ncntrl'       ! nems control variables
       INCLUDE 'ogsmparm'
       INCLUDE 'ogsml48'
       INCLUDE 'ogsmbfw'

       INTEGER init_time(8),tmp_time(8),cur_time(8)
       CHARACTER(10) curtime
       LOGICAL once
       DATA once/.false./


!  THIS IS THE MAIN PROGRAM FOR THE OLOGSS ECONOMIC/TIMING MODULE

      IF (.not.once)then
!==========================> HSM Code Start <==========================
        ! WRITE(hsm_out,'(*(G0.16,:,","))') 'IF (.not.once)then', curcalyr, L48B4YR, CURIYR, BASEYR, itimeyr, CURITR
!===========================> HSM Code End <===========================
!        ****************************************************************
          CALL DATE_AND_TIME(TIME=curtime, VALUES=init_time)
          write (OGBUG1,*) 'Time Run Started: ',curtime

!        ****************************************************************
!  INITIALIZE ALL VARIABLES FOR THE MODEL
!        call timer(time2)

         CALL DATE_AND_TIME(VALUES=tmp_time)

!        CALL INIT_VAR(1,max_res)                                         

         CALL INIT_AGG
         CALL INIT_vAR(1,MAX_RES,1)                                      
         write(OGBUG1,*) 'OLOGSS: end initialization 1'
         CALL DATE_AND_TIME(VALUES=cur_time)
         CALL ELAPSED_TIME(tmp_time,cur_time)

!        ****************************************************************
!  READ DATA
!        call timer(time2)

        CALL DATE_AND_TIME(VALUES=tmp_time)
        CALL READ_DATA
        write(OGBUG1,*) 'OLOGSS: end read data'

        CALL READ_CO2

        call init_table(1)                                                  
        CALL READ_RESOURCE(1)                                              
        WRITE(OGBUG1,*) 'end read reservoir 1'
         CALL DATE_AND_TIME(TIME=curtime, VALUES=cur_time)
         CALL ELAPSED_TIME(tmp_time,cur_time)

        CALL COVERAGE

!        ****************************************************************
!  SETUP MODEL FILES AND VARIABLES
!        call timer(time2)

         CALL DATE_AND_TIME(VALUES=tmp_time)

        CALL SETUP
        write(OGBUG1,*) 'end set up'
         CALL DATE_AND_TIME(TIME=curtime, VALUES=cur_time)
         CALL ELAPSED_TIME(tmp_time,cur_time)

         once = .true.

         ENDIF

        ITIMEYR = (CURIYR+BASEYR-1)-L48B4YR+1

!     ****************************************************************
!  DECLINE EXISTING OIL AND GAS PROJECTS
        IF (itimeyr == 1) then
!==========================> HSM Code Start <==========================
        ! WRITE(hsm_out,'(*(G0.16,:,","))') 'IF (itimeyr == 1) then', curcalyr, L48B4YR, CURIYR, BASEYR, itimeyr, CURITR
!===========================> HSM Code End <===========================
          CALL DATE_AND_TIME(VALUES=tmp_time)
          CALL EXISTING_PROD
          WRITE(OGBUG1,*) 'end existing production'
          CALL DATE_AND_TIME(TIME=curtime, VALUES=cur_time)
          CALL ELAPSED_TIME(tmp_time,cur_time)

          CALL DATE_AND_TIME(VALUES=tmp_time)
          CALL INIT_VAR(1,MAX_RES,2)  
          write (OGBUG1,*) 'end initialization 2'
          CALL DATE_AND_TIME(VALUES=cur_time)
          CALL ELAPSED_TIME(tmp_time,cur_time)

          CALL DATE_AND_TIME(VALUES=tmp_time)
          call init_table(2)
          CALL READ_RESOURCE(2)      
          CALL PRESCREEN
          write (OGBUG1,*) 'end read resource 2'
          CALL DATE_AND_TIME(TIME=curtime, VALUES=cur_time)
          CALL ELAPSED_TIME(tmp_time,cur_time)
          call restable(1)
        ENDIF

!     ****************************************************************
!  TIMING LOOP
        IF (itimeyr >= 1) then
!==========================> HSM Code Start <==========================
        ! WRITE(hsm_out,'(*(G0.16,:,","))') 'IF (itimeyr >= 1) then', curcalyr, L48B4YR, CURIYR, BASEYR, itimeyr, CURITR
!===========================> HSM Code End <===========================
          CALL DATE_AND_TIME(VALUES=tmp_time)
          CALL TIMING
          if (curiyr == lastyr) write(ogbug1,*) 'usecap', (usecap(i),i=1,5)
          write (OGBUG1,*) 'end timing'
          CALL DATE_AND_TIME(TIME=curtime, VALUES=cur_time)
          CALL ELAPSED_TIME(tmp_time,cur_time)

          CALL DATE_AND_TIME(VALUES=tmp_time)
          CALL RESERVES
          write (OGBUG1,*) 'end reserves'
          CALL DATE_AND_TIME(TIME=curtime, VALUES=cur_time)
          CALL ELAPSED_TIME(tmp_time,cur_time)

          CALL OGTO_OGSM
        ENDIF

        CALL DATE_AND_TIME(TIME=curtime, VALUES=cur_time)
        CALL ELAPSED_TIME(tmp_time,cur_time)

        call restable(2)

!     ****************************************************************
!  END OF MAIN OLOGSS PROGRAM


       RETURN
       END SUBROUTINE

!****************************************************************
!****************************************************************
      SUBROUTINE ELAPSED_TIME(t1,t2)

      IMPLICIT NONE

      include 'parametr'     ! nems dimension parameters
      include 'ncntrl'       ! nems control variables
      INCLUDE 'ogsmparm'
      include 'ogsmbfw'

      INTEGER hh,mm
      INTEGER t1(8),t2(8)
      REAL ss1,ss2,ss3,ss

      ss1=t1(5)*60*60+t1(6)*60+t1(7)+t1(8)/1000.
      ss2=t2(5)*60*60+t2(6)*60+t2(7)+t2(8)/1000.
      ss3 = ss2-ss1
      hh = int(ss3/60/60)
      mm = int((ss3-hh*60*60)/60)
      ss = ss3-hh*60*60-mm*60
      write (OGBUG1,*) 'Elapsed Time: ',hh,':',mm,':',ss

      RETURN
      END SUBROUTINE

!***************************************************************
!from Init_var.for
       SUBROUTINE INIT_VAR(stres,endres,IMODE) 

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmugr'
       include 'ogsml48'
       include 'ogsmbfw'
       include 'ctus'

       INTEGER iyr,iproc,ifuel,ist,IR,iwell,ires,itech,isrc,isc,iprov, &
         iplay,iund,idep,irdr
       INTEGER irs                   
       INTEGER stres,endres
       INTEGER IMODE                                                 !INITIALIZATION CONTROL
                                                                     !1 - ALL VARIABLES
                                                                     !2 - PRODUCTION/ECONOMIC ARRAYS

       WRITE(OGBUG1,*) 'INITIALIZING OLOGSS VARIABLES'
       nat_hoil = 0.0
       nat_hgas = 0.0
       dist_hoil = 0.
       dist_hgas = 0.

!  this subroutine initializes all of the input variables.

!        NOTE: this does not initialize the variables required for the project
!              economic calculations

!        1) resource data for each reservoir
! resource data read into model (discovered & conventional undiscovered)
      IF(IMODE >= 1) THEN
       numdisc = 0      
       do ires = 1,max_und
         und_resid(ires) = '  '
         und_play(ires)  = 0  
         und_item(ires)  = 0 
         und_counter(ires) = 0
       end do                

       do ires = stres,endres
         timed(ires) = .false.
         timedyr(ires) = 0
         do itech = 1,max_tech
            ARESID(ires)        = ' '
            AState(ires)        = ' '
            ARES_TYPE(ires)     = ' '
            AREGION(ires)       = 0
            APADD(ires)         = 0
            APCODE(ires)        = 0
            APLAY_CDE(ires)     = 0
            ARESFLAG(ires)      = 0
            asc(ires)           = 0
            aprov(ires)         = 0
            aresacc(ires)       = 0
            acrdtype(ires)      = 0
            acrdheat(ires)      = 0.0
            apresin(ires)       = 0.0
            ARRC(ires)          = 0.0
            APAY(ires)          = 0.0
            ADEPTH(ires)        = 0.0
            AAPI(ires)          = 0.0
            AGAS_GRAV(ires)     = 0.0
            ABTU(ires)          = 0.0
            ANWELLOIL(ires)     = 0.0
            ANWELLGAS(ires)     = 0.0
            ATOTINJ(ires,itech)       = 0.0
            APATSIZ(ires)       = 0.0
            ALATNUM(ires,itech)       = 0.0
            ALATLEN(ires,itech)       = 0.0
            ATOTPAT(ires,itech)       = 0.0
            ATOTPROD(ires,itech)      = 0.0
            ATEMP(ires)         = 0.0
            AHEATVAL(ires)      = 0.0
            APHI(ires)          = 0.0
            ASOI(ires)          = 0.0
            ASOC(ires)          = 0.0
            ASOR(ires)          = 0.0
            AGOR(ires)          = 0.0
            AWOR(ires)          = 0.0
            ANGL(ires)          = 0.0
            ANGPLET(ires)       = 0.0
            ANGPLPR(ires)       = 0.0
            ANGPLBU(ires)       = 0.0
            ANGPLIS(ires)       = 0.0
            ANGPLPP(ires)       = 0.0
            ABOI(ires)          = 0.0
            APERM(ires)         = 0.0
            AOILVIS(ires)       = 0.0
            AVDP(ires)          = 0.0
            ACO2CONT(ires)      = 0.0
            AN2CONT(ires)       = 0.0
            AH2SCONT(ires)      = 0.0
            ASULFOIL(ires)      = 0.0
            AOOIP(ires)         = 0.0
            ACHOILPROD(ires)    = 0.0
            ACHGASPROD(ires)    = 0.0
            ABO(ires)           = 0.0
            aswi(ires)          = 0.0
            aorgooip(ires)      = 0.0
            ATOTCONV(ires,itech)      = 0.0
            ATOTPS(ires,itech)        = 0.0
            AOGIP(ires)         = 0.0
            ATOTACRES(ires)     = 0.0
            ASGI(ires)          = 0.0
            AFLP(Ires)          = 0.0
            ANUMACC(Ires)       = 0.0
            aflen(ires)         = 0.0
            do iyr = 1,max_yr
               APRODOIL(ires,iyr,itech)  = 0.0
               APRODGAS(ires,iyr,itech)  = 0.0
               APRODNGL(ires,iyr,itech)  = 0.0
               APRODWAT(ires,iyr,itech)  = 0.0
               aprodco2(ires,iyr,itech)  = 0.0
               AWATINJ(ires,iyr,itech)   = 0.0
               AINJINJ(ires,iyr,itech)   = 0.0
               AINJRECY(ires,iyr,itech)  = 0.0
               aresvoil(ires,iyr,itech)  = 0.0
               aresvgas(ires,iyr,itech)  = 0.0
               airsvoil(ires,iyr,itech)  = 0.0
               airsvgas(ires,iyr,itech)  = 0.0
            end do
            do iyr = 1,max_yr+10
               PATDEV(ires,iyr,itech)  = 0.0
            end do
         end do
       end do

       ECO2LIM =0
       ECO2RANKVAL = 0.0 
       ETHANEREC = 0.0
       ETHANEREJ = 0.0

! resource data used in aggregation
            ESUMP = 0.
            EXPATN = 0.
            EPRODOIL = 0.
            EPRODGAS = 0.
            EREMRES = 0.
            egremres = 0.
            eiremres = 0.
            eigremres = 0.
            EGROSSREV = 0.
            EGRAVADJ = 0.
            EADJGROSS = 0.
            EROY = 0.
            ENETREV = 0.
            ETOC = 0.
            EGAEXP = 0.
            EGACAP = 0.
            EINJCOST = 0.
            EPRODWAT = 0.
            EWATINJ = 0.
            ECO2COST = 0.
            ESURFVOL = 0.
            ETORECY = 0.
            ETORECY_CST = 0.
            EFOAM = 0.
            EIGEN = 0.
            EPROC_OAM = 0.
            EOAM = 0.
            EAOAM = 0.
            ESTIM = 0.
            EEXIST_EOAM = 0.
            ENEW_EOAM = 0.
            EEXIST_ECAP = 0.
            ENEW_ECAP = 0.
            EII = 0.
            EIIDRL = 0.
            EICAP2 = 0.
            EINTCAP = 0.
            ETI = 0.
            ETIDRL = 0.
            EOTC = 0.
            ECOMP = 0.
            ETCI = 0.
            ETCIADJ = 0.
            ECAP_BASE = 0.
            EDEPR = 0.
            EAMOR = 0.
            EDEPGGLA = 0.
            ELA = 0.
            EGG = 0.
            EDEP_CRD = 0.
            EADGGLA = 0.
            EDGGLA = 0.
            EEGGLA = 0.
            EXPLA = 0.
            EXPGG = 0.
            ESEV = 0.
            EDEPLET = 0.
            ETAXINC = 0.
            EEORTCA = 0.
            EINTADD = 0.
            EGGLAADD = 0.
            ENIBT = 0.
            ESTTAX = 0.
            EAMINT = 0.
            EFEDTAX = 0.
            EFEDCR = 0.
            ENIAT = 0.
            ETCADD = 0.
            EATCF = 0.
            EDATCF = 0.
            ECATCF = 0.
            EINJDR = 0.
            EPRODDR = 0.
            EPRODWELL = 0.
            EINJWELL = 0.
            ESHUTIN = 0.
      END IF             

!        2) constraint data - both parameters and annual constraint arrays
      IF(IMODE == 1) THEN 

       ntimed = 0

       do iyr = 1,max_yr
         invcap(iyr) = 0.0
         bascap(iyr) = 0.0
         usecap(iyr) = 0.0
         nat_expudrcap(iyr) = 0.0
         nat_expcdrcap(iyr) = 0.0
         nat_expc(iyr) = 0.0
         nat_expu(iyr) = 0.0
         nat_gas(iyr) = 0.0
         nat_oil(iyr) = 0.0
         nat_drcap_g(iyr) = 0.0
         nat_drcap_o(iyr) = 0.0
         nat_hor(iyr) = 0.0
         tothwcap(iyr) = 0.0

         do IR = 1,max_reg
           reg_oil(IR,iyr) = 0.0
           reg_gas(IR,iyr) = 0.0
           drcap_o(IR,iyr) = 0.0
           drcap_g(IR,iyr) = 0.0
           reg_expc(IR,iyr) = 0.0
           reg_expu(IR,iyr) = 0.0
           expcdrcap(IR,iyr) = 0.0
           expudrcap(IR,iyr) = 0.0
           SUM_OIL_CONV(IR,iyr)   = 0.0
           SUM_OIL_UNCONV(IR,iyr) = 0.0
           SUM_GAS_CONV(IR,iyr)   = 0.0
           SUM_GAS_UNCONV(IR,iyr) = 0.0
           co2avail(IR,iyr) = 0.0
           basavail(IR,iyr) = 0.0
           useavail(IR,iyr) = 0.0
           do isrc = 1,max_src
              nat_availco2(IR,isrc,iyr) = 0.0
              bse_availco2(IR,isrc,iyr) = 0.0
              src_availco2(IR,isrc,iyr) = 0.0
              use_availco2(IR,isrc,iyr) = 0.0              
              pur_co2_at_eor(IR,isrc,iyr) = 0.0              
              co2_recycle(IR,isrc,iyr) = 0.0              
              co2_inject(IR,isrc,iyr) = 0.0              
              eor_oil_prod(IR,isrc,iyr) = 0.0              
              eor_gas_prod(IR,isrc,iyr) = 0.0              
           end do
         end do
       end do

       do irdr = 1,max_rdr
         do iyr = 1,max_yr
            RDR_FOOTAGE(iRDR,iYR) = 0.0
            RDR_FT(iRDR,iYR)      = 0.0
         end do
       end do

       do iRDR = 1,max_rdr
         do iYR = 1,2
           RDR(iyr,0:max_reg,iRDR) = 0.0
         end do
       end do


       do isrc = 1,max_src
        YRDT(isrc)            = 0
        YRDI(isrc)            = 0
        YRMA(isrc)            = 0
        UMPCO2(isrc)          = 0.0
          do IR = 1,max_reg
            CO2BASE(IR,iSRC) = 0.0
          end do
          do IR = 1,10
            AARP(IR,isrc) = 0.0
          end do
       end do

!  drilling success rates
       h_success  = 0.0
       do IR = 1,max_reg
         SUCDEVO(IR) = 0.0
         SUCDEVG(IR) = 0.0
         SUCEXP(IR)  = 0.0
         sucexpd(IR) = 0.0
         sucdeve(IR) = 0.0
       end do

!        3) cost parameters
         comp_vc     = 0.0
         polycost    = 0.0
         poly        = 0.0
         co2k        = 0.0
         co2b        = 0.0
         co2om20     = 0.0
         co2om_20    = 0.0
         stimfac     = 0.0
         stim_a      = 0.0
         stim_b      = 0.0
         cost_bhp    = 0.0
         comp_oam    = 0.0
         co2cost     = 0.0
         env_fac     = 0.0
         envop_fac   = 0.0
         baseoil     = 0.0
         cutoil      = 0.0
         basegas     = 0.0
         fply        = 0.0
         fco2        = 0.0
         co2oam      = 0.0
         pwhp        = 0.0
         plypk       = 0.0
         plypa       = 0.0
         chmk        = 0.0
         chma        = 0.0
         chmb        = 0.0
         co2rk       = 0.0
         co2rb       = 0.0
         stmma       = 0.0
         stmga       = 0.0

         Omult_tang  = 0.0
         Omult_int   = 0.0
         Omult_oam   = 0.0
         Gmult_tang  = 0.0
         Gmult_int   = 0.0
         Gmult_oam   = 0.0

        do ist = 1,L48RGN
          co2st(ist) = 0
          co2pr(ist) = 0.0
        end do

        do iproc = 1,max_proc
          oil_oam(iproc) = 0.0
          gas_oam(iproc) = 0.0
          wat_oam(iproc) = 0.0
          inj_oam(iproc) = 0.0
        end do

       do IR = 1,max_reg+1
          do idep = 1,max_dep
             OIL_DWCM(IR,idep)   = 0.0
             oil_dwcD(IR,idep)   = 0.0
             oil_dwck(IR,idep)   = 0.0
             oil_dwca(IR,idep)   = 0.0
             oil_dwcb(IR,idep)   = 0.0
             oil_dwcc(IR,idep)   = 0.0
             gas_dwcm(IR,idep)   = 0.0
             GAS_dwcD(IR,idep)   = 0.0
             gas_dwck(IR,idep)   = 0.0
             gas_dwca(IR,idep)   = 0.0
             gas_dwcb(IR,idep)   = 0.0
             gas_dwcc(IR,idep)   = 0.0
             dry_dwcm(IR,idep)   = 0.0
             DRY_dwcD(IR,idep)   = 0.0
             dry_dwck(IR,idep)   = 0.0
             dry_dwca(IR,idep)   = 0.0
             dry_dwcb(IR,idep)   = 0.0
             dry_dwcc(IR,idep)   = 0.0
             nprm(IR,idep)       = 0.0
             MPRD(IR,idep)       = 0.0
             nprk(IR,idep)       = 0.0
             npra(IR,idep)       = 0.0
             nprb(IR,idep)       = 0.0
             nprc(IR,idep)       = 0.0
             wrkm(IR,idep)       = 0.0
             WRKD(IR,idep)       = 0.0
             WRKK(IR,idep)       = 0.0
             WRKA(IR,idep)       = 0.0
             WRKB(IR,idep)       = 0.0
             WRKC(IR,idep)       = 0.0
             pswm(IR,idep)       = 0.0
             pswD(IR,idep)       = 0.0
             pswk(IR,idep)       = 0.0
             pswa(IR,idep)       = 0.0
             pswb(IR,idep)       = 0.0
             pswc(IR,idep)       = 0.0
             psim(IR,idep)       = 0.0
             psiD(IR,idep)       = 0.0
             psik(IR,idep)       = 0.0
             psia(IR,idep)       = 0.0
             psib(IR,idep)       = 0.0
             psic(IR,idep)       = 0.0
             FACUPM(IR,idep)     = 0.0
             facupd(IR,idep)     = 0.0
             facupk(IR,idep)     = 0.0
             facupa(IR,idep)     = 0.0
             facupb(IR,idep)     = 0.0
             facupc(IR,idep)     = 0.0
             FACgM(IR,idep)      = 0.0
             facgd(IR,idep)      = 0.0
             facgk(IR,idep)      = 0.0
             facga(IR,idep)      = 0.0
             facgb(IR,idep)      = 0.0
             facgc(IR,idep)      = 0.0
             omom(IR,idep)       = 0.0
             omoD(IR,idep)       = 0.0
             omok(IR,idep)       = 0.0
             omoa(IR,idep)       = 0.0
             omob(IR,idep)       = 0.0
             omoc(IR,idep)       = 0.0
             omlD(IR,idep)       = 0.0
             omlk(IR,idep)       = 0.0
             omla(IR,idep)       = 0.0
             omlb(IR,idep)       = 0.0
             omlc(IR,idep)       = 0.0
             omgm(IR,idep)       = 0.0
             OMGD(IR,idep)       = 0.0
             OMGK(IR,idep)       = 0.0
             OMGA(IR,idep)       = 0.0
             OMGB(IR,idep)       = 0.0
             OMGC(IR,idep)       = 0.0
             opsecm(IR,idep)     = 0.0
             opsecD(IR,idep)     = 0.0
             opseck(IR,idep)     = 0.0
             opseca(IR,idep)     = 0.0
             opsecb(IR,idep)     = 0.0
             opsecc(IR,idep)     = 0.0
             opinjm(IR,idep)     = 0.0
             opinjD(IR,idep)     = 0.0
             opinjk(IR,idep)     = 0.0
             opinja(IR,idep)     = 0.0
             opinjb(IR,idep)     = 0.0
             opinjc(IR,idep)     = 0.0
             omlftm(IR,idep)     = 0.0
             omlftD(IR,idep)     = 0.0
             omlftk(IR,idep)     = 0.0
             omlfta(IR,idep)     = 0.0
             omlftb(IR,idep)     = 0.0
             omlftc(IR,idep)     = 0.0
             omswrm(IR,idep)     = 0.0
             omswrd(IR,idep)     = 0.0
             omswrk(IR,idep)     = 0.0
             omswra(IR,idep)     = 0.0
             omswrb(IR,idep)     = 0.0
             omswrc(IR,idep)     = 0.0
          end do
       end do

! -----------------(NEW) ---------------------------------------------
! zero out new D&C cost coefficients array

       DNCC_COEF = 0.0  ! initializes to zero (all array elements)
! -----------------------------------------------------------------
       do IR = 1,max_reg-1           
          do isrc =1,max_src
             cregpr(IR,isrc) = 0.0
          end do
       end do

       WRITE(6,5411) CURIRUN, CURIYR+1989, CURITR, stres, endres, IMODE
 5411  FORMAT(1X,"INIT_VAR",6(":",I12))

       cregpryr = 0.0
       cregpryrS = 999.999

!        4) price arrays
      CO2PRICE = 0.0
      H2SPRICE = 0.0
      NGLPRICE = 0.0
      N2PRICE  = 0.0
      inflac   = 0.0
      do iyr = 1,max_yr
        oilpricec(iyr) = 0.0
        gaspricec(iyr) = 0.0
      end do

      do iyr = 1,max_yr*2
        rinfl(iyr) =0.0
      end do

!        5) aggregation variables
       do iproc = 1,max_proc
         do iyr = 1,max_yr
            timed_proc(iproc,iyr) = 0
            candidate(iproc,iyr) = 0
            potential(iproc,iyr) = 0
            viable(iproc,iyr) = 0
         end do
       end do

       do ires = stres,endres            
         do itech = 1,max_tech          
            ALIFE(Ires,itech) = 0      
            DRESID(Ires) = '  '       
         end do                      
       end do                       
      END IF                       

       END SUBROUTINE
!***************************************************************
!from READ_DATA.FOR
!****************************************************************************************
!     READ_DATA SUBROUTINE:
!     THIS SUBROUTINE READS THE INPUT FILES FOR THE OLOGSS ECONOMIC/TIMING MODULE
!
!     The following OLOGSS files are read, but were merged into wllow48.txt:
!         1) wlcontrol.txt
!         2) wltechnology.txt
!         3) wlcost.txt
!         4) wlconstraints.txt
!         5) wladjust.txt
!         6) wlaccess.txt
!         7) wlpattern.txt
!         8) wlprice.txt
!         9) wldepreciation.txt
!        10) wlplaymap.txt
!        11) wltaxes.txt
!        12) wlgasdemand.txt           !not merged into wllow48.txt, stand-alone only
!         9) wlplay.txt???     Where does this get read???
!****************************************************************************************
       SUBROUTINE READ_DATA
       IMPLICIT NONE

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       INCLUDE 'ogsml48'
       INTEGER*2 count(330),reg(330),src(330),tmp4(330)
       REAL tmp1(330),tmp2(330),tmp3(330)

!  INVOKE FILE_MGR TO OPEN INPUT FILE
       FNAME='WLLOW48'
       NEW=.FALSE.
       IFILE1 = FILE_MGR('O',FNAME,NEW)

!  READ THE RUN OPTIONS DATA
       CALL READ_OPTIONS

!  READ THE SCENARIO FILES
       CALL READ_SCENARIO

!  READ THE COST DATA
       CALL READ_COST

!  READ THE COST ADJUSTMENT DATA
       CALL READ_COSTADJ

!  READ THE CONSTRAINT DATA
       CALL READ_CONSTRAINT

!  READ THE RESOURCE ACCESS DATA
       CALL READ_ACCESS

!  READ THE PROJECT DEVELOPMENT RULES FOR EACH PROCESS
       CALL READ_PATTDEV

!  READ OIL AND GAS PRICES
       CALL READ_PRICES

!  READ OTHER
       CALL READ_OTHER

!  READ TAXES
       CALL READ_TAXES

!  read hydraulic fracturing controls/parameters
       call read_frac

!  READ DEPRECIATION SCHEDULES
       CALL READ_DEPRECIATION

       IFILE1=FILE_MGR('C',FNAME,.FALSE.)


       RETURN
       END SUBROUTINE                            !END READ_DATA SUBROUTINE
!***************************************************************
!     from SETUP_MAIN.FOR

       SUBROUTINE SETUP

       implicit none

       INTEGER iopen

!  iopen = 1 -> open files, iopen = 2 -> close files

       iopen = 1

!***************************************************************
!  SET UP THE CONSTRAINTS

       CALL SETUP_CONSTRAINTS

       END SUBROUTINE

!***************************************************************
!     from SETUP_CONSTRAINTS.FOR

       SUBROUTINE SETUP_CONSTRAINTS()
       
       use ctus_mod, only : set_base_co2_volumes_available
       
       implicit none

       include 'parametr'     ! nems dimension parameters
       include 'ncntrl'       ! nems control variables
       include 'ogsmparm'
       include 'ogsml48'
       include 'ogsmbfw'
       include 'ogsmout'
       include 'pmmout'
       include 'intout'
       include 'qblk'
       include 'ngtdmrep'
      include 'emmparm'
      include 'uecpout'
      include 'macout'
      include 'ctus'

      INTEGER iyr, ir, t_cyr
       

       REAL tot_growth            ! will be replaced by the read statements
       REAL tot_dmdgas
      REAL*4 CCS_EOR_87_EFD2(MNUMYR)
      INTEGER capyr
       INTEGER origord(max_src),sortord(max_src)

! original source order for industrial and natural CO2 - corresponds with the source_labels
 
       DATA origord(1:13) /1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13/    

! final source order for industrial and natural CO2 - based on average economic price

       DATA sortord(1:13) /2, 9, 1, 3, 8, 4, 10, 5, 6, 7, 13, 11, 12/

! Map Price Bin to Industrial Source Indexes, this excludes Natural, Power and XTLs

       DATA MAP_BINS_TO_SRC_INDEX / 1,  2,  3,  5,  6,  9, 12, 13,  0,  0,  0,  0,  0/
       DATA MAP_SRC_INDEX_TO_BINS / 1,  2,  3,  0,  4,  5,  0,  0,  6,  0,  0,  7,  8/

       CHARACTER*30 region_label(max_reg-1)                         

       region_label(1) = 'EAST COAST           '                       
       region_label(2) = 'GULF COAST           '                      
       region_label(3) = 'MIDCONTINENT         '                     
       region_label(4) = 'SOUTHWEST            '                    
       region_label(5) = 'ROCKY MOUNTAINS      '                   
       region_label(7) = 'NORTHERN GREAT PLAINS'                  
       region_label(6) = 'WEST COAST           '                 

!  this subroutine calculates the annual constraint arrays for each of the constraints

!      1) drilling.  Set up using the drilling constraint parameters and the oil cost.
!         Regional drilling is determined using the regional drilling allocation factors
!            This includes the oil, gas, dry, exploration, and development drilling footage available

       DO J=1,MAX_YR
         TOT_GROWTH = (1.0+(J-1)*(RGR-RRR)/100.0) * (1.0+DRILL_OVER/100.0)

         NAT_OIL(J) = (OILA0+OILA1*oilpriced(J)+OILA2*oilpriced(J)**2+OILA3*oilpriced(j)*gaspriced(j))
         NAT_OIL(J) = NAT_OIL(j) * TOTMUL * TOT_GROWTH
          NAT_ODR(J) = (OILD0+OILD1*oilpriced(J)**2+OILD2*oilpriced(j)**3+OILD3*gaspriced(j)+OILD4*gaspriced(j)**2)* TOTMUL * TOT_GROWTH

          nat_oil(j) = nat_oil(j)*oil_adj(j)
          nat_odr(j) = nat_odr(j)*oil_adj(j)


! NOTE -- DECREASE VERTICAL WELL FOOTAGE BY FOOTAGE USED FOR HORIZONTAL WELLS

          NAT_OIL(J) = NAT_OIL(J) * (1.0 - H_PERCENT/100.0)
          NAT_ODR(J) = NAT_ODR(J) * (1.0 - H_PERCENT/100.0)

          NAT_GAS(J) = (GASA0+GASA1*oilpriced(j)+GASA2*gaspriced(J)**2)*TOTMUL * TOT_GROWTH
          NAT_GDR(J) = (GASD0+GASD1*gaspriced(J))*TOTMUL * TOT_GROWTH
          NAT_GDR(J) = (GASD0+GASD1*oilpriced(J)**2+GASD2*oilpriced(j)**3+GASD3*gaspriced(j)+GASD4*gaspriced(j)**2)* TOTMUL * TOT_GROWTH

          nat_gas(j) = nat_gas(j)*gas_adj(j)
          nat_gdr(j) = nat_gdr(j)*gas_adj(j)

          NAT_EXP(J) = (EXPA0+EXPA1*oilpriced(J)**2+EXPA2*gaspriced(j)+EXPA3*gaspriced(j)*oilpriced(j)**2)* EXPMUL * TOT_GROWTH
          nat_expg(j)= (EXPAg0+EXPAg1*oilpriced(j)*gaspriced(J))* EXPMUL * TOT_GROWTH
          nat_expd(j)= (EXPD0+EXPD1*oilpriced(j)+EXPD2*oilpriced(j)**2+EXPD3*oilpriced(j)**3+EXPD4*gaspriced(J))* EXPMUL * TOT_GROWTH

          nat_exp(j) = nat_exp(j)*exp_adj(j)
          nat_expg(j) = nat_expg(j) * exp_adjg(j)

          NAT_HOR(J)= NAT_OIL(J) * H_PERCENT/100.0 * (1.0+(J-1)*H_GROWTH/100.0) * HORMUL* (1.0+DRILL_OVER/100.0)
          nat_hor(j) = nat_hor(j)*hor_adj(j)

!           nat_oil(j) = nat_oil(j) - nat_hor(j)
!           nat_tot(j) = nat_tot(j) - nat_hor(j)

          IF(nat_oil(j) <= 0.0) nat_oil(j) = 0.0
          IF(nat_tot(j) <= 0.0) nat_tot(j) = 0.0
          if(nat_hor(j) <= 0.0) nat_hor(j) = 0.0
          nat_dual(j) = (nat_oil(j)+nat_gas(j))*(drill_res/100.0)
          NAT_TOT(J) = NAT_OIL(J) + NAT_GAS(J)
       END DO

       DO J=1,MAX_REG
          SUCO(J) = ((NAT_OIL(1) - NAT_ODR(1)) / NAT_OIL(1))*100.0
          SUCG(J) = ((NAT_GAS(1) - NAT_odR(1)) / NAT_GAS(1))*100.0
       END DO

! split national drilling to regional
      IF (REG_NAT == 'R'.or.reg_nat == 'r') THEN
          DO J=1,MAX_REG-1              
             DO iyr=1,MAX_YR
               REG_OIL(J,iyr) = NAT_OIL(iyr) * PRO_REGOIL(J)/100.0 * (1.0 - DRILL_TRANS/100.0)
               REG_GAS(J,iyr) = NAT_GAS(iyr) * PRO_REGGAS(J)/100.0 * (1.0 - DRILL_TRANS/100.0)
               REG_EXP(J,iyr) = NAT_EXP(iyr) * PRO_REGEXP(J)/100.0 * (1.0 - DRILL_TRANS/100.0)
                reg_expg(j,iyr) = nat_expg(iyr) * pro_regexpg(j)/100.0 * (1.0 - drill_trans/100.0)
                reg_expd(j,iyr) = nat_expd(iyr) * pro_regexpd(j)/100.0 * (1.0 - drill_trans/100.0)
                reg_dual(j,iyr) = reg_oil(j,iyr) * (drill_res/100.0)       
                reg_dual(j,iyr) = reg_dual(j,iyr) + (reg_gas(j,iyr) * (drill_res/100.0))

                reg_dual(j,iyr) =reg_dual(j,iyr)*(1.0-drill_trans/100.0)  

                reg_oil(j,iyr) = reg_oil(j,iyr)*(1.0 - drill_res/100.0)  
                reg_gas(j,iyr) = reg_gas(j,iyr)*(1.0 - drill_res/100.0) 
             END DO
          END DO
          DO J=1,MAX_REG-1   
             DO iyr=1,MAX_YR
                NAT_OIL(iyr) = NAT_OIL(iyr) - REG_OIL(J,iyr)
                NAT_GAS(iyr) = NAT_GAS(iyr) - REG_GAS(J,iyr)
                NAT_EXP(iyr) = NAT_EXP(iyr) - REG_EXP(J,iyr)
                nat_expg(iyr) = nat_expg(iyr) - reg_expg(j,iyr)
                nat_expd(iyr) = nat_expd(iyr) - reg_expd(j,iyr)
                nat_Dual(iyr) = nat_dual(iyr) - reg_dual(j,iyr)
               IF (NAT_OIL(iyr) <= 0.0) NAT_OIL(iyr) = 0.0
               IF (NAT_GAS(iyr) <= 0.0) NAT_GAS(iyr) = 0.0
               IF (NAT_EXP(iyr) <= 0.0) NAT_EXP(iyr) = 0.0
               if (nat_expg(iyr) <= 0.0) nat_Expg(iyr) = 0.0 
               IF (nat_dual(iyr) <= 0.0) nat_dual(iyr) = 0.0
             END DO
          END DO
       END IF

!  split the exploration drilling between conventional and unconventional resources

       DO J=1,MAX_REG-1             
          DO iyr=1,MAX_YR
             REG_EXPC(J,iyr) = REG_EXP(J,iyr) * EXPL_FRAC/100.0
             REG_EXPU(J,iyr) = REG_EXP(J,iyr) * (1.0 - EXPL_FRAC/100.0)

             reg_expcg(j,iyr) = reg_expg(j,iyr) * expl_fracg/100.0        
             reg_expug(j,iyr) = reg_expg(j,iyr) * (1.0 - expl_fracg/100.0)
          END DO
       END DO
       DO iyr=1,MAX_YR
          NAT_EXPC(iyr)   = NAT_EXP(iyr) * EXPL_FRAC/100.0
          NAT_EXPU(iyr)   = NAT_EXP(iyr) * (1.0 - EXPL_FRAC/100.0)

          nat_expcg(iyr)  = nat_expg(iyr) * expl_fracg/100.0             
          nat_expug(iyr)  = nat_expg(iyr) * (1.0 - expl_fracg/100.0)    
       END DO

! CHECK FOR SPLIT BETWEEN OIL AND GAS
!       IF A SPLIT IS REQUIRED DO NOTHING, IF A TOTAL IS REQUIRED
!       AGGREGATE TO THE OIL SIDE.
!
      IF (SPLIT_OG == 'N'.or.split_og == 'n') THEN
           DO J=1,MAX_REG-1                 
              DO iyr=1,MAX_YR
                 REG_OIL(J,iyr) = REG_OIL(J,iyr) + REG_GAS(J,iyr)
                 reg_oil(j,iyr) = reg_oil(j,iyr) + reg_dual(j,iyr)   
                 REG_GAS(J,iyr) = 0.0
                 reg_dual(j,iyr) = 0.0      
              END DO
           END DO
           DO iyr=1,MAX_YR
              NAT_OIL(iyr) = NAT_OIL(iyr) + NAT_GAS(iyr)
              NAT_GAS(iyr) = 0.0
           END DO
        END IF

      IF (split_ed == 'N'.or.split_og == 'n' ) THEN
           DO J=1,MAX_REG-1           
              DO iyr=1,MAX_YR
                 REG_OIL(J,iyr) = REG_OIL(J,iyr) + REG_EXPU(J,iyr)
                 REG_EXPU(J,iyr) = 0.0
              END DO
           END DO
           DO iyr=1,MAX_YR
              NAT_OIL(iyr) = NAT_OIL(iyr) + NAT_EXPU(iyr)
              NAT_EXPU(iyr) = 0.0
           END DO
       END IF

      IF(drill_trans == 0.0.and.drill_res > 0.0) then  
         do iyr = 1,max_yr                            
            nat_oil(iyr) = 0.0                       
            nat_Gas(iyr) = 0.0                      
         end do                                    
       END if                                     

         write (ogbug1,*) '***** FOOTAGE CONSTRAINTS *****'
         write (ogbug1,1) 'oil prices $/Bbl ',(oilpriced(j),j=1,max_yr-1)
         write (ogbug1,1) 'gas prices $/Mmcf',(gaspriced(j),j=1,max_yr-1)
         write (ogbug1,*)
         write (ogbug1,*)
         write (ogbug1,2) 'national footage: 1000 Ft'
         write (ogbug1,1) 'national oil foot',(nat_oil(j),j=1,max_yr-1)
         write (ogbug1,1) 'nat. oil dry foot',(nat_odr(j),j=1,max_yr-1)
         write (ogbug1,1) 'national gas foot',(nat_gas(j),j=1,max_yr-1)
         write (ogbug1,1) 'nat. gas dry foot',(nat_gdr(j),j=1,max_yr-1)
         write (ogbug1,1) 'national exp oil foot',(nat_exp(j),j=1,max_yr-1)
         write (ogbug1,1) 'national exp gas foot',(nat_expg(j),j=1,max_yr-1)
         write (ogbug1,1) 'national tot foot',(nat_tot(j),j=1,max_yr-1)
         write (ogbug1,1) 'national hor foot',(nat_hor(j),j=1,max_yr-1)

1     format (a30,18x,<max_yr-1>(3x,f15.2))
2     format (a30)
3     format (a30,2x,i2)

! CONFIRM RIG DEPTH RATING LOGIC IF ACTIVE SET UP FOOTAGES, ELSE, SET TO MAX
!
      IF (USE_RDR == 'N'.or.use_rdr == 'n') THEN
          DO J=1,MAX_RDR
             DO iyr=1,MAX_YR
               RDR_FOOTAGE(J,iyr) = 0.
               DO ir=1,MAX_reg-1
                  RDR_FOOTAGE(J,iyr) = RDR_FOOTAGE(J,iyr) + (NAT_OIL(iyr)+NAT_EXP(iyr))*RDR(1,ir,j)/100.0  &
                                     + (NAT_GAS(iyr)+nat_expg(iyr))*RDR(2,ir,J)/100.0
               END DO
             END DO
          END DO
       ELSE
          DO J=1,MAX_RDR
             DO iyr=1,MAX_YR
                RDR_FOOTAGE(J,iyr) = NAT_TOT(iyr) + NAT_EXP(iyr) + nat_expg(iyr) + nat_expd(iyr)
             END DO
          END DO
       END IF

 4     format (a30,3x,f15.5,<max_yr-1>(3x,f15.2))
 6     format (a40,8x,<max_yr-1>(3x,f15.2))
 5     format (a40)

       write (ogbug1,*)
       write (ogbug1,2) 'national rig depth footage'
       do i = 1,max_rdr
         write (ogbug1,3) 'rig depth rating',i
         write (ogbug1,4) 'avl footage',rdr(2,0,i), (rdr_footage(i,iyr),iyr=1,max_yr-1)
       end do

!      2) capital. This is read from 'Read_constraints'.
!
!      Adjust capital in sidecases to get initial projects selected to line up with reference case
        if(it_wop(l48hyr+3,1) > 45 .or. wwop==3) nat_invcap(1) = nat_invcap(1)*(it_wop(l48hyr+3,1)*dladj/baseoil)**0.5   ! high price case
        if(it_wop(l48hyr+3,1) < 25 .or. wwop==1) nat_invcap(1) = nat_invcap(1)*(it_wop(l48hyr+3,1)*dladj/baseoil)**0.25   ! low price case
        if(ogrunop(2) == 23) nat_invcap(1) = nat_invcap(1)*0.85  ! low resource and technology case
        if(ogrunop(2) == 30) nat_invcap(1) = nat_invcap(1)*1.20  ! high resource and technology case

        capyr = 4
        if(it_wop(l48hyr+3,1) > 45 .or. wwop==3) capyr = 3
        if(it_wop(l48hyr+3,1) < 25 .or. wwop==1) capyr = 5

        DO I=1,capyr
            NAT_INVCAP(I) = NAT_INVCAP(1)*(dcrdwhp(mnumor,i+l48b4yr-baseyr)*dladj/baseoil)**0.5*(oghhprng(i+l48b4yr-baseyr)*dladj/basegas)**0.5
        ENDDO

        DO I=capyr+1,max_yr
           NAT_INVCAP(I)=NAT_INVCAP(I)*it_wop(curiyr,1)   ! make sure capital constraint is not triggered after STEO years in all cases
        ENDDO

        write (ogbug1,*)
        write (ogbug1,*) '***** CAPITAL CONSTRAINT *****'
        write (ogbug1,*)
        write (ogbug1,*) 'world oil price (87$)',it_wop(l48b4yr-baseyr+3,1), it_wop(l48hyr+3,1)
        write (ogbug1,1) 'nat cap ($mm)',(nat_invcap(i),i=1,max_yr-1)
        write (ogbug1,*)

        call set_base_co2_volumes_available

!      4) Gas demand.  Gas demand is assigned at the regional and national level.  This is read by
!           'Read_nemscontrol'.

!  THIS IS WHERE THE LEVERS WILL BE INSERTED INTO THE CODE!!!!!!!
!  set the final constraint arrays for the model

       do iyr = 1,max_yr
         invcap(iyr)            = nat_invcap(iyr)
         bascap(iyr)            = invcap(iyr)
         TOTHWCAP(IYr)          = NAT_HOR(IYr)
         NAT_DRCAP_O(iyr)       = nat_oil(iyr) - nat_hor(iyr)
         NAT_DRCAP_G(iyr)       = nat_gas(iyr)
         nat_drcap_d(iyr)       = nat_dual(iyr)
         NAT_EXPCDRCAP(iyr)     = nat_expc(iyr)
         NAT_EXPUDRCAP(iyr)     = nat_expu(iyr)
         nat_expudrcapg(iyr)    = nat_expug(iyr)
         nat_expcdrcapg(iyr)    = nat_expcg(iyr)

         do i = 1, max_reg-1
           DRCAP_O(i,iyr)       = reg_oil(i,iyr)
           DRCAP_G(i,iyr)       = reg_gas(i,iyr)
           drcap_d(i,iyr)       = reg_dual(i,iyr)
           EXPCDRCAP(i,iyr)     = reg_expc(i,iyr)
           EXPUDRCAP(i,iyr)     = reg_expu(i,iyr)
           expcdrcapg(i,iyr)    = reg_expcg(i,iyr)
           expudrcapg(i,iyr)    = reg_expug(i,iyr)
         end do
       end do

       DO I = 1, MAX_REG-1
          SUCDEVO(I) = suco(i)
          SUCDEVG(I) = sucg(i)
          SUCCHDEV    = 100.0 - h_success        
       END DO

       do i = 1,MNL48N                          
         IF(sucexpd(i) <= 10.0) sucexpd(i) = 10.0
       end do                                   


!  STORE THE TOTAL REGIONAL GAS DEMAND - FOR REPORTING PURPOSES
         IF (ogrunop(7).eq.0.and.ogrunop(9).eq.0) THEN    ! put national level demand in all regions since this is what is checked against
           do i = 1,MNL48N            ! position 7 is the volume checked
             do iyr = 1,max_yr-1
            if (l48b4yr-baseyr+iyr <= lastyr) then
                 nat_dmdgas(i,iyr) = qngas(11,l48b4yr-baseyr+iyr)+ngexpvol(4,l48b4yr-baseyr+iyr)
               else
                 nat_dmdgas(i,iyr) = (qngas(11,iyr-1)+ngexpvol(4,iyr-1))*1.02
               endif
                basnat_dmdgas(i,iyr) = nat_dmdgas(i,iyr)
             end do
           end do
           nat_dmdgas = 99999.0
         else
            nat_dmdgas = 99999.0
         ENDIF

         write (ogbug1,111)
         write (ogbug1,111) 'REGIONAL GAS DEMAND (BCF/YR)'
         WRITE (ogbug1,112) 'REGION',('YR ',I,I=1,max_yr-1)
         do i = 1,MNL48N
            write (ogbug1,113) i,(nat_dmdgas(i,iyr),iyr=1,max_yr-1)
         end do

 111   format (a28)
 112   format (a6,<max_yr-1>(4x,3x,a3,i2))
 113   format (i6,<max_yr-1>(4x,f8.2))
 11    format (a30,4(2x,a10))
 15    format (a30,4(2x,f10.3))

       END SUBROUTINE

!***************************************************************
!from EXISTING_MAIN.FOR
       SUBROUTINE EXISTING_PROD

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       INclude 'ogsmbfw'
       INclude 'ogsmugr'
       include 'ogsml48'
       include 'pmmout'
       include 'ngtdmrep'

       INTEGER ires,ir,iYR,dist
       INTEGER ITECH     
       INTEGER time,time2

       logical ifrac

!   This subroutine screens the oil and gas reservoirs which would be subject to decline curve analysis
!   against the filter levers specified in the scenario file.  If the reservoir passes, it is declined
!   and tagged for aggregation.

!  SCREEN THE PROJECTS AGAINST MODEL FILTERS

       WRITE(OGBUG1,*) 'BEGINNING OIL DECLINE',FDEC,LDEC
!  DECLINE OIL PROJECTS
      if (ldec >= 1) then
       do ires = fdec,ldec
           ir = aregion(ires)
           do m=1,max_yr
             oilpricec(m)=dcrdwhp(ir,curiyr)*dladj
             gaspricec(m)=ogwprng(ir,curiyr)*dladj
             oilpriceo(m)=ogprcl48(ir,1,1)*dladj
             gaspriceo(m)=ogprcl48(ir,3,1)*dladj
           enddo

            if (apcode(ires) == 00) then
              call scenario_screen(ires)                                    !set base and advanced technology levers
              ifrac = .false.
              call frac_screen(ires,ifrac)
              if(ifrac) call hydrofrac(ires)

              call oil_decline(ires)

              DO ITECH = 1,MAX_TECH  
                DRESID(Ires) = ARESID(Ires) 
              END DO                          
              DLIFE(IRES)    = AECON_LIFE(IRES)
              DTIMED(IRES)   = TIMED(IRES)    
              DPCODE(IRES)    = APCODE(Ires) 
         else                               
                DRESID(Ires) = ARESID(Ires)
                dpcode(ires) = apcode(ires)
         end if
       end do
       LDEC2 = LDEC
       end if

!  DECLINE GAS PROJECTS
       WRITE(OGBUG1,*) 'BEGINNING GAS DECLINE',FGAS,LGAS
      if (lgas >= 1) then
       do ires = fgas,lgas
           ir = aregion(ires)
           do m=1,max_yr
             oilpricec(m)=dcrdwhp(ir,curiyr)*dladj
             gaspricec(m)=ogwprng(ir,curiyr)*dladj
             oilpriceo(m)=ogprcl48(ir,1,1)*dladj
             gaspriceo(m)=ogprcl48(ir,3,1)*dladj
           enddo
           if (apcode(ires) >= 11.and.apcode(ires) < 16) then            !set base and advanced technology levers
             call scenario_screen(ires)                                    !existing decline curve
             call gas_decline(ires)
           end if

           if (apcode(ires) >= 21.and.apcode(ires) <= 23) then        !set base and advanced technology levers
             call scenario_screen(ires)                                !unconventional gas currently under development
             call gas_decline(ires)
           end if
       end do
       end if

         write (ogbug1,111)
         write (ogbug1,111) 'REGIONAL GAS DEMAND (BCF/YR) -- AFTER DECLINE REMOVED'
         WRITE (ogbug1,112) 'REGION',('YR ',I,I=1,max_yr-1)
         do i = 1,7
            write (ogbug1,113) i,(nat_dmdgas(i,iyr),iyr=1,max_yr-1)
         end do

111   format (a53)
112   format (a6,<max_yr-1>(4x,3x,a3,i2))
113   format (i6,<max_yr-1>(4x,f8.2))


      END SUBROUTINE


!**************************************************************************************
!**************************************************************************************
!**************************************************************************************
!**************************************************************************************
!**************************************************************************************

      subroutine coverage

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
      include 'ogsmbfw'
      include 'ogsml48'
      include 'ogsmout'

      REAL    loilprod,lgasprod
      REAL    REG_LOIL(L48RGN),REG_LGAS(L48RGN),reg_leoil(L48RGN)
      REAL    reg_lcbm(L48RGN),reg_lcnv(L48RGN),reg_lshl(L48RGN),reg_ltht(L48RGN)
      real    reg_moil(L48RGN)                                   

!  this routine adjusts the decline production according to the last historical production
       loilprod = 0.0
       lgasprod = 0.0

       ADJOIL   = 0.0
       ADJGAS   = 0.0

       dist_adjoil = 1.
       dist_adjgas = 1.
       dist_adjadg = 1.

       DO I = 1,L48RGN
         REG_LOIL(I) = 0.0
         REG_LGAS(I) = 0.0
         reg_leoil(i) = 0.0
         REG_ADJOIL(I) = 0.0
         REG_ADJGAS(I) = 0.0
         reg_lcbm(i) = 0.0
         reg_lcnv(i) = 0.0
         reg_ltht(i) = 0.0
         reg_lshl(i) = 0.0
         reg_adjcbm(i) = 0.0
         reg_adjcnv(i) = 0.0
         reg_adjtht(i) = 0.0
         reg_adjshl(i) = 0.0
       END DO

       lgasprod = nat_hgas/scale_day/1000.0
       loilprod = nat_hoil/scale_day

       DO I = 1,L48RGN
         IF(REG_HOIL(I) > 0.0) THEN
            REG_LOIL(I) = REG_HOIL(I)/SCALE_DAY
          END IF

         IF(reg_heoil(i) > 0.0) then
            reg_leoil(i) = reg_heoil(i)/scale_day
          END if

         IF(REG_HGAS(I) > 0.0) THEN
            REG_LGAS(I) = REG_HGAS(I)/SCALE_DAY/1000.0
          END IF

         IF(reg_hcbm(i) > 0.0) then
            reg_lcbm(i) = reg_hcbm(i)/scale_day/1000.0
          END if                                     

         IF(reg_hcnv(i) > 0.0) then                 
            reg_lcnv(i) = reg_hcnv(i)/scale_day/1000.0 
          END if                                      

         IF(reg_htht(i) > 0.0) then                  
            reg_ltht(i) = reg_htht(i)/scale_day/1000.0
          END if                                     

         IF(reg_hshl(i) > 0.0) then                 
            reg_lshl(i) = reg_hshl(i)/scale_day/1000.0
          END if                                     
       END DO

      IF(NAT_HOIL > 0.0) THEN
         ADJOIL = (SCALE_OIL)/LOILPROD
       END IF

      IF(NAT_HGAS > 0.0) THEN
         ADJGAS = (SCALE_GAS)/LGASPROD
       END IF

       DO I = 1,L48RGN
         IF(REG_LOIL(I) > 0.0) REG_ADJOIL(I) = ((regscale_OIL(i)-reg_leoil(i)))/reg_loil(i)

         IF(REG_LGAS(I) > 0.0) REG_ADJGAS(I) = (regscale_GAS(i))/reg_lGAS(i)

         IF(reg_hadg(i) > 0.0) reg_adjadg(i) = histadprd(i,l48b4yr-baseyr)*1000./reg_hadg(i)

         IF(reg_hcnv(i) > 0.0) reg_adjcnv(i) = (histprdl48(i,3,l48b4yr-baseyr)+histprdl48(i,4,l48b4yr-baseyr))*1000./reg_hcnv(i)
         if (reg_adjcnv(i) > 1.) reg_adjcnv(i) = (reg_adjcnv(i)-1.0)*0. + 1.0

         IF(reg_hcbm(i) > 0.0) reg_adjcbm(i) = histprdl48(i,7,l48b4yr-baseyr)*1000./reg_hcbm(i)

         IF(reg_htht(i) > 0.0) reg_adjtht(i) = histprdl48(i,5,l48b4yr-baseyr)*1000./reg_htht(i)

         IF(reg_hshl(i) > 0.0) reg_adjshl(i) = histprdl48(i,6,l48b4yr-baseyr)*1000./reg_hshl(i)

         reg_moil(i) = (reg_adjoil(i)*1000.0)  
         reg_adjoil(i) = float(nint(reg_moil(i)))/1000.0

       END DO

       DO I = 1,66
         if (distmap(i,1) > 0) then
            if(dist_hoil(i,1) > 0.) dist_adjoil(i,1) = ogoilprd(i,1,l48hyr-1)/dist_hoil(i,1)
            if(dist_hoil(i,2) > 0.) dist_adjoil(i,2) = ogoilprd(i,2,l48hyr-1)/dist_hoil(i,2)
            if(dist_hgas(i,1) > 0.) dist_adjgas(i,1) = (ogrnagprd(i,1,l48hyr-1))/(dist_hgas(i,1)*0.001)
            if(dist_hgas(i,2) > 0.) dist_adjgas(i,2) = (ogrnagprd(i,2,l48hyr-1))/(dist_hgas(i,2)*0.001)
            if(dist_hgas(i,3) > 0.) dist_adjgas(i,3) = (ogrnagprd(i,3,l48hyr-1))/(dist_hgas(i,3)*0.001)
            if(dist_hgas(i,4) > 0.) dist_adjgas(i,4) = (ogrnagprd(i,4,l48hyr-1))/(dist_hgas(i,4)*0.001)
            if(dist_hadg(i,1) > 0.) dist_adjadg(i,1) = (ogadgprd(i,1,l48hyr-1))/(dist_hadg(i,1)*0.001)
            if(dist_hadg(i,2) > 0.) dist_adjadg(i,2) = (ogadgprd(i,2,l48hyr-1))/(dist_hadg(i,2)*0.001)
        ENDIF
       END DO

! TEMPORARY
       reg_adjoil(:) = 1.0
       reg_adjcnv(:) = 1.0
       reg_adjcbm(:) = 1.0
       reg_adjtht(:) = 1.0
       reg_adjshl(:) = 1.0
       reg_adjadg(:) = 1.0
!      dist_adjoil(:,:) = 1.0
!      dist_adjgas(:,:) = 1.0
!      dist_adjadg(:,:) = 1.0

       WRITE (OGBUG1,*) '***** COVERAGE.OUT *****'
       write (OGBUG1,1) 'LAST YEAR OF HISTORICAL DATA: ',L48B4YR-1
       WRITE (OGBUG1,2) 'OIL PRODUCTION IN RESOURCE DATA (MBBL): ', (REG_HOIL(I),I=1,L48RGN),NAT_HOIL
       write (OGBUG1,2) 'EOR PRODUCTION IN RESOURCE DATA (MBBL): ', (REG_HEOIL(I),I=1,L48RGN),SUM(REG_HEOIL,DIM=1)

       WRITE (OGBUG1,2) 'DRY GAS PRODUCTION IN RESOURCE DATA (BCF): ', (REG_HGAS(I),I=1,L48RGN),NAT_HGAS/1000.0

       WRITE (OGBUG1,5) 'CBM GAS PRODUCTION IN RESOURCE DATA (BCF): ',(REG_HCBM(I)/1000.0,I=1,L48RGN)
       WRITE (OGBUG1,5) 'CNV GAS PRODUCTION IN RESOURCE DATA (BCF): ', (REG_HCNV(I)/1000.0,I=1,L48RGN)
       WRITE (OGBUG1,5) 'SHL GAS PRODUCTION IN RESOURCE DATA (BCF): ',(REG_HSHL(I)/1000.0,I=1,L48RGN)
       WRITE (OGBUG1,5) 'THT GAS PRODUCTION IN RESOURCE DATA (BCF): ',(REG_HTHT(I)/1000.0,I=1,L48RGN)
       WRITE (OGBUG1,5) 'AD  GAS PRODUCTION IN RESOURCE DATA (BCF): ',(REG_HADG(I)/1000.0,I=1,L48RGN)

       WRITE (OGBUG1,*)
       WRITE (OGBUG1,2) 'EIA HISTORICAL PRODUCTION (MBBL): ', (REGSCALE_OIL(I)*SCALE_DAY,I=1,L48RGN),SCALE_OIL*SCALE_DAY
       WRITE (OGBUG1,2) 'EIA HISTORICAL PRODUCTION (MMCF): ', (REGSCALE_GAS(I)*SCALE_DAY,I=1,L48RGN),SCALE_GAS*SCALE_DAY
       WRITE (OGBUG1,*)
       WRITE (OGBUG1,2) 'OIL ADJUSTMENT FACTOR: ',(REG_ADJOIL(I),I=1,L48RGN), ADJOIL
       WRITE (OGBUG1,2) 'GAS ADJUSTMENT FACTOR: ',(REG_ADJGAS(I),I=1,L48RGN), ADJGAS

       WRITE (OGBUG1,5) 'CBM ADJUSTMENT FACTOR: ',(REG_ADJCBM(I),I=1,L48RGN)
       WRITE (OGBUG1,5) 'CNV ADJUSTMENT FACTOR: ',(REG_ADJCNV(I),I=1,L48RGN)
       WRITE (OGBUG1,5) 'SHL ADJUSTMENT FACTOR: ',(REG_ADJSHL(I),I=1,L48RGN)
       WRITE (OGBUG1,5) 'THT ADJUSTMENT FACTOR: ',(REG_ADJTHT(I),I=1,L48RGN)
       WRITE (OGBUG1,5) 'AD  ADJUSTMENT FACTOR: ',(REG_ADJADG(I),I=1,L48RGN)
       DO I = 1,66
         if (distmap(i,1) > 0) then
           write(ogbug1,*) 'bench_oil1', i, dist_hoil(i,1),ogoilprd(i,1,l48hyr-1),dist_adjoil(i,1)
           write(ogbug1,*) 'bench_oil2', i, dist_hoil(i,2),ogoilprd(i,2,l48hyr-1),dist_adjoil(i,2)
           write(ogbug1,*) 'bench_gas1', i, dist_hgas(i,1)*0.001,ogrnagprd(i,1,l48hyr-1),dist_adjgas(i,1)
           write(ogbug1,*) 'bench_gas2', i, dist_hgas(i,2)*0.001,ogrnagprd(i,2,l48hyr-1),dist_adjgas(i,2)
           write(ogbug1,*) 'bench_gas3', i, dist_hgas(i,3)*0.001,ogrnagprd(i,3,l48hyr-1),dist_adjgas(i,3)
           write(ogbug1,*) 'bench_gas4', i, dist_hgas(i,4)*0.001,ogrnagprd(i,4,l48hyr-1),dist_adjgas(i,4)
           write(ogbug1,*) 'bench_adg1', i, dist_hadg(i,1)*0.001,ogadgprd(i,1,l48hyr-1),dist_adjadg(i,1)
           write(ogbug1,*) 'bench_adg2', i, dist_hadg(i,2)*0.001,ogadgprd(i,2,l48hyr-1),dist_adjadg(i,2)
         ENDIF
       END DO



1     FORMAT (A45,2X,I4)
2     FORMAT (A45,2X,<L48RGN+1>(F16.3,2X))
3     FORMAT (A45)
4     FORMAT (<MAX_YR-1>(F16.3,3X))
5     format (a45,2x,<L48RGN>(f16.3,2x))


      end subroutine
!***************************************************************
!from screen.FOR
       subroutine scenario_screen(ires)

!  THIS SUBROUTINE SETS THE DEFAULT LEVER VALUES FOR THE BASE AND ADVANCED
!  TECHNOLOGY CASES.  IT THEN SCREENS THE RESERVOIR TO SEE WHICH, IF ANY, OF
!  THE USER-DEFINED IMPACTS APPLIES.  IF AN IMPACT APPLIES, THE LEVER VALUES
!  FOR THAT IMPACT ARE ASSIGNED TO THE ADVANCED TECHNOLOGY CASE.

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       INCLUDE 'ogsmugr'
       include 'ogsml48'
       include 'ogsmbfw'

       INTEGER ires,iyr
       INTEGER impct,ist,iproc
       INTEGER ifail,ichk,iimpct                                  !failure code
       REAL iboe
       CHARACTER*2 st2
       CHARACTER*20 fail(8)
       CHARACTER*40 proclabel(max_proc)


       fail(1) = 'PROCESS FAILURE     '
       fail(2) = 'REGION FAILURE      '
       fail(3) = 'STATE FAILURE       '
       fail(4) = 'DEPTH FAILURE       '
       fail(5) = 'API GRAVITY FAILURE '
       fail(6) = 'PERMEABILITY FAILURE'
       fail(7) = 'RATE FAILURE        '
       fail(8) = 'RESERVOIR PASSES    '

       proclabel(1) =  '00: Decline Curve                  '
       proclabel(2) =  '01: Primary                        '
       proclabel(3) =  '02: Water Flooding                 '
       proclabel(4) =  '03: CO2 Flooding                   '
       proclabel(5) =  '04: Steam Flooding                 '
       proclabel(6) =  '05: Polymer Flooding               '
       proclabel(7) =  '06: Infill Drilling                '
       proclabel(8) =  '07: Profile Modification           '
       proclabel(9) =  '08: Horizontal Continuity          '
       proclabel(10) =  '09: Horizontal Profile             '
       proclabel(11) =  '10: Undiscovered Conventional Oil  '
       proclabel(12) =  '11: Conventional Radial Flow       '
       proclabel(13) =  '12: Water Drive                    '
       proclabel(14) =  '13: Tight Sands                    '
       proclabel(15) =  '14: Wet Coal/Shale Gas             '
       proclabel(16) =  '15: Dry Coal/Shale Gas             '
       proclabel(17) =  '16: Undiscovered Conventional Gas  '
       proclabel(18) =  '17: Undiscovered Conventional Oil  '
       proclabel(19) =  '18: Undiscovered Tight Gas         '
       proclabel(20) =  '19: Undiscovered Coalbed Methane   '
       proclabel(21) =  '20: Undiscovered Shale Gas         '
       proclabel(22) =  '21: Developing Shale Gas           '
       proclabel(23) =  '22: Developing Tight Gas           '
       proclabel(24) =  '23: Developing Coalbed Methane     '


       read (aresid(ires)(10:11),'(i2)') iproc

!  get the state number
       ist = 0
       read (aresid(ires)(3:4),'(a2)') st2
       call state_match(st2,ist)

       iboe = 0.0
       iboe = aprodoil(ires,1,1)+(aprodgas(ires,1,1)/5.65)
       iboe = (iboe * 1000.0)/365.0

!  set the defaulted values
       call set_levers(0,ires)

!  screen the reservoirs for the applicable impact
!  SCREEN PROCESS                 fail 1
!  SCREEN REGION                  fail 2
!  SCREEN STATE                   fail 3
!  SCREEN DEPTH                   fail 4
!  SCREEN API GRAVITY             fail 5
!  SCREEN PERM                    fail 6
!  SCREEN RATE                    fail 7

       ichk = 0
       iimpct = 0
       do impct = 1,impacts

       ifail = 1

         if (process_filter(iproc+1,impct).and.ichk == 0) then             !screen for process
            ifail = 2
            if (region_filter(aregion(ires),impct).and.ichk == 0) then    !screen for region
   !             PRINT*, aresid(ires),ist,impct,'2'
              ifail = 4
              if (adepth(ires) >= min_depth(impct).and.adepth(ires) <= max_depth(impct).and.ichk == 0) then      !screen for depth
   !              PRINT*, aresid(ires),adepth(ires),impct,'4'
                ifail = 5
                if (aapi(ires) >= min_api(impct).and.aapi(ires) <= max_api(impct).and.ichk == 0) then        !screen for api gravity
   !                 PRINT*, aresid(ires),aapi(ires),impct,'5'
                  ifail = 6
                  if (((aperm(ires) >= min_perm(impct).and.aperm(ires) <= max_perm(impct)).or.aperm(ires) == -1.0).and.ichk == 0) then
   !                    PRINT*, aresid(ires),aperm(ires),impct,'6'
                    ifail = 7
                    if (iboe >= min_rate(impct).and.iboe <= max_rate(impct).and.ichk == 0) then           !screen for rate
   !                       PRINT*, aresid(ires),iboe,ichk,'7'
                      ifail = 8
                      if (oil_filter(impct).and.ares_type(ires) == 'O'.and.ichk == 0) then  ! oil reservoirs
                        ichk = 1
                        iimpct = impct
                        call set_levers(impct,ires)
                      end if    !end oil reservoirs

                      if (gas_filter(impct).and.ares_type(ires) == 'G'.and.ichk == 0) then  !gas reservoirs
                        ichk = 1
                        iimpct = impct
                        call set_levers(impct,ires)
                      end if   !end gas reservoirs
                    end if        !end rate screen
                  end if        !end screen for perm
                end if        !end api gravity screen
              end if        !end depth screen
            end if        !end region screen
         end if        !end process screen
       end do

1703  format (a11,2x,i3,2x,i3,2x,<max_yr-1>(f10.4,2x))

1     format (a14,1x,a11)
2     format (a14,1x,f10.2)
3     format (a14,1x,i3)
4     format (a14,1x,a40)
5     format (a14,1x,a20)

       end subroutine


!***********************************************************************************
!***********************************************************************************
!***********************************************************************************
!***********************************************************************************
       subroutine set_levers(impct,ires)

!  THIS SUBROUTINE SETS THE LEVERS FOR THE BASE AND ADVANCED TECHNOLOGY CASES.  FOR
!  THE BASE CASE, ONLY THE LEVERS ARE SET.  FOR THE ADVANCED CASE, BOTH THE VARIABLES
!  AND THE TECHNOLOGY PENETRATION CURVE ARE SET.

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include 'ogsmbfw'
       include 'ogsml48'
       include 'ogsmugr'

       integer impct                               !impact number.  0 is default
       INTEGER iyr, ii
       integer ires

       REAL YR_SC_START   ! START YEAR OF SCENARIO ( YS )
       REAL YR_COM_START  ! YEAR TO START COMMERCIALIZATION ( D )
       REAL YR_START      ! YEAR FOR COMERCIALIZATION (YS + D)(START YEAR + YEAR TO DEVLOP TECHNOLOGY)
       REAL YR_COM        ! YEARS TO COMERCIALIZE (YC)
       REAL PROBR         ! PROBABILITY OF R&D SUCCESS (PR)
       REAL PROBI         ! PROBABILITY OF IMPLMENTATION (PI)
       REAL UMP           ! ULTIMATE MARKET PENETRATION (U)
       REAL ATECH         ! TECHNOLOGY PENETRATION RATIO  (TX)
       REAL ATECH1        ! TECHNOLOGY PENETRATION RATIO (TEMPORARY)

       real amlt,bmlt,cmlt,dmlt,emlt,fmlt,gmlt,ooip
       real oilp(max_yr), gasp(max_yr),watp(max_yr)
       real prodtech
       character*1 prodtier

       INTEGER MIDPOINT
       INTEGER IMID
       INTEGER PCURVE     ! PENETRATION CURVE TYPE
!                            1. CONVEX UP (DEFUALT)
!                            2. CONVEX DOWN
!                            3. SIGNOID
!                            4. LINEAR

       read (aresid(ires)(1:1),'(a1)') prodtier
       if (apcode(ires) == 10.or.apcode(ires) == 16) prodtech = 1+togtechon(4,1)*techadj_on(1,1)  ! tier 2
       if (apcode(ires) == 17) prodtech = 1+togtechon(3,2)*techadj_on(1,1)
       if (apcode(ires) == 17.and.prodtier == "E") prodtech = 1+togtechon(4,2)*techadj_on(1,1)    ! tier 2
       if (apcode(ires) >= 3.and.apcode(ires) <= 9) prodtech = 1+togtechon(3,3)*techadj_on(1,1)
       if (apcode(ires) == 18.or.apcode(ires) == 22) prodtech = 1+togtechon(3,4)*techadj_on(1,1)
       if (apcode(ires) == 22.and.prodtier == "E") prodtech = 1+togtechon(4,4)*techadj_on(1,1)    ! tier 2
       if (apcode(ires) == 20.or.apcode(ires) == 21) prodtech = 1+togtechon(3,5)*techadj_on(1,1)
       if (apcode(ires) == 21.and.prodtier == "E") prodtech = 1+togtechon(4,5)*techadj_on(1,1)    ! tier 2
       if (apcode(ires) == 19.or.apcode(ires) == 23) prodtech = 1+togtechon(3,6)*techadj_on(1,1)
       if (impct.eq.0.and.curiyr >= techyr) then
         do iyr = 1,max_yr
            aprodoil(ires,iyr,1) = aprodoil(ires,iyr,1)*prodtech
            aprodgas(ires,iyr,1) = aprodgas(ires,iyr,1)*prodtech
         end do
       endif

       do iyr = 1,max_yr
          advanced(iyr) = 0.0
         if(impct > 0) then
            if(rec_eff_chg(impct) /= rec_eff_fac(1)) then
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,1) * (1. + 1./(comyear(impct)*1.)*(rec_eff_chg(impct)-1.))
               aprodgas(ires,iyr,2) = aprodgas(ires,iyr,1) * (1. + 1./(comyear(impct)*1.)*(rec_eff_chg(impct)-1.))
            endif
          endif
          !set the advanced case values equal to the base case values
          aprodoil(ires,iyr,2) = aprodoil(ires,iyr,1)       
          aprodgas(ires,iyr,2) = aprodgas(ires,iyr,1)
          aprodco2(ires,iyr,2) = aprodco2(ires,iyr,1)
          aprodwat(ires,iyr,2) = aprodwat(ires,iyr,1)
          awatinj(ires,iyr,2) =  awatinj(ires,iyr,1)
          ainjinj(ires,iyr,2) =  ainjinj(ires,iyr,1)
       end do

         !set the advanced case values equal to the base case values
         drill_fac(2)          =    drill_fac(1)
         explr_fac(2)          =    explr_fac(1)
         paycont_fac(2)        =    paycont_fac(1)
         skin_fac(2)           =    skin_fac(1)
         contin_fac(2)         =    contin_fac(1)
         prod_ind_fac(2)       =    prod_ind_fac(1)
         rec_eff_fac(2)        =    rec_eff_fac(1)
         vol_swp_fac(2)        =    vol_swp_fac(1)
         inj_rate_fac(2)       =    inj_rate_fac(1)
         mob_rat_fac(2)        =    mob_rat_fac(1)
         tech01_fac(2)         =    tech01_fac(1)
         tech02_fac(2)         =    tech02_fac(1)
         tech03_fac(2)         =    tech03_fac(1)
         tech04_fac(2)         =    tech04_fac(1)
         tech05_fac(2)         =    tech05_fac(1)
         co2_rat_fac(2)        =    co2_rat_fac(1)
         oil_rat_fac(2)        =    oil_rat_fac(1)
         res_chr_fac(2)        =    res_chr_fac(1)
         chg_drl_fac(2)        =    chg_drl_fac(1)
         chg_stm_fac(2)        =    chg_stm_fac(1)
         chg_comp_fac(2)       =    chg_comp_fac(1)
         chg_fac_fac(2)        =    chg_fac_fac(1)
         chg_secconv_fac(2)    =    chg_secconv_fac(1)
         chg_injconv_fac(2)    =    chg_injconv_fac(1)
         chg_facupg_fac(2)     =    chg_facupg_fac(1)
         chg_wrk_fac(2)        =    chg_wrk_fac(1)
         chg_prdwat_fac(2)     =    chg_prdwat_fac(1)
         chg_chmpnt_fac(2)     =    chg_chmpnt_fac(1)
         chg_plypnt_fac(2)     =    chg_plypnt_fac(1)
         chg_co2pnt_fac(2)     =    chg_co2pnt_fac(1)
         chg_stmgen_fac(2)     =    chg_stmgen_fac(1)
         chg_foam_fac(2)       =    chg_foam_fac(1)
         chg_voam_fac(2)       =    chg_voam_fac(1)
         chg_gna_fac(2)        =    chg_gna_fac(1)
         chg_annsec_fac(2)     =    chg_annsec_fac(1)
         chg_lft_fac(2)        =    chg_lft_fac(1)
         chg_secwrk_fac(2)     =    chg_secwrk_fac(1)
         chg_injc_fac(2)       =    chg_injc_fac(1)
         chg_injt_fac(2)       =    chg_injt_fac(1)
         chg_cmp_fac(2)        =    chg_cmp_fac(1)
         cst_drl_fac(2)        =    cst_drl_fac(1)
         cst_stm_fac(2)        =    cst_stm_fac(1)
         cst_comp_fac(2)       =    cst_comp_fac(1)
         cst_fac_fac(2)        =    cst_fac_fac(1)
         cst_secconv_fac(2)    =    cst_secconv_fac(1)
         cst_injconv_fac(2)    =    cst_injconv_fac(1)
         cst_facupg_fac(2)     =    cst_facupg_fac(1)
         cst_wrk_fac(2)        =    cst_wrk_fac(1)
         cst_foam_fac(2)       =    cst_foam_fac(1)
         cst_voam_fac(2)       =    cst_voam_fac(1)
         cst_gna_fac(2)        =    cst_gna_fac(1)
         cst_annsec_fac(2)     =    cst_annsec_fac(1)
         cst_lft_fac(2)        =    cst_lft_fac(1)
         cst_secwrk_fac(2)     =    cst_secwrk_fac(1)
         cst_injc_fac(2)       =    cst_injc_fac(1)
         cst_cmp_fac(2)        =    cst_cmp_fac(1)

         chg_ooil_fac(2)       =    chg_ooil_fac(1)
         chg_ogas_fac(2)       =    chg_ogas_fac(1)
         chg_owat_fac(2)       =    chg_owat_fac(1)
         chg_oinj_fac(2)       =    chg_oinj_fac(1)

         cstp_drl_fac(2)       =    cstp_drl_fac(1)
         cstp_stm_fac(2)       =    cstp_stm_fac(1)
         cstp_comp_fac(2)      =    cstp_comp_fac(1)
         cstp_fac_fac(2)       =    cstp_fac_fac(1)
         cstp_secconv_fac(2)   =    cstp_secconv_fac(1)
         cstp_injconv_fac(2)   =    cstp_injconv_fac(1)
         cstp_facupg_fac(2)    =    cstp_facupg_fac(1)
         cstp_wrk_fac(2)       =    cstp_wrk_fac(1)
         cstp_foam_fac(2)      =    cstp_foam_fac(1)
         cstp_voam_fac(2)      =    cstp_voam_fac(1)
         cstp_gna_fac(2)       =    cstp_gna_fac(1)
         cstp_annsec_fac(2)    =    cstp_annsec_fac(1)
         cstp_lft_fac(2)       =    cstp_lft_fac(1)
         cstp_secwrk_fac(2)    =    cstp_secwrk_fac(1)
         cstp_injc_fac(2)      =    cstp_injc_fac(1)
         cstp_cmp_fac(2)       =    cstp_cmp_fac(1)

         firstcom_fac(2)       =    firstcom_fac(1)
         comyear_fac(2)        =    comyear_fac(1)
         tech_curve_fac(2)     =    tech_curve_fac(1)
         mark_pen_fac(2)       =    mark_pen_fac(1)
         prob_rd_fac(2)        =    prob_rd_fac(1)
         prob_imp_fac(2)       =    prob_imp_fac(1)
         if (impct > 0) then !start base case calculations
           drill_fac(2)          =    drill_chg(impct)
           explr_fac(2)          =    explr_chg(impct)
           paycont_fac(2)        =    paycont_chg(impct)
           skin_fac(2)           =    skin_chg(impct)
           contin_fac(2)         =    contin_chg(impct)
           prod_ind_fac(2)       =    prod_ind_chg(impct)
           rec_eff_fac(2)        =    rec_eff_chg(impct)
           vol_swp_fac(2)        =    vol_swp_chg(impct)
           inj_rate_fac(2)       =    inj_rate_chg(impct)
           mob_rat_fac(2)        =    mob_rat_chg(impct)
           tech01_fac(2)         =    tech01_chg(impct)
           tech02_fac(2)         =    tech02_chg(impct)
           tech03_fac(2)         =    tech03_chg(impct)
           tech04_fac(2)         =    tech04_chg(impct)
           tech05_fac(2)         =    tech05_chg(impct)
           co2_rat_fac(2)        =    co2_rat_chg(impct)
           oil_rat_fac(2)        =    oil_rat_chg(impct)
           res_chr_fac(2)        =    res_chr_chg(impct)
           chg_drl_fac(1)        =    itimeyr*1./(comyear(impct)*1.)*(chg_drl_cst(impct)-1.)+1.
           chg_stm_fac(1)        =    itimeyr*1./(comyear(impct)*1.)*(chg_stm_cst(impct)-1.)+1.
           chg_comp_fac(1)       =    itimeyr*1./(comyear(impct)*1.)*(chg_comp_cst(impct)-1.)+1.
           chg_fac_fac(1)        =    itimeyr*1./(comyear(impct)*1.)*(chg_fac_cst(impct)-1.)+1.
           chg_secconv_fac(1)    =    itimeyr*1./(comyear(impct)*1.)*(chg_secconv_cst(impct)-1.)+1.
           chg_injconv_fac(1)    =    itimeyr*1./(comyear(impct)*1.)*(chg_injconv_cst(impct)-1.)+1.
           chg_facupg_fac(1)     =    itimeyr*1./(comyear(impct)*1.)*(chg_facupg_cst(impct)-1.)+1.
           chg_wrk_fac(1)        =    itimeyr*1./(comyear(impct)*1.)*(chg_wrk_cst(impct)-1.)+1.
           chg_prdwat_fac(1)     =    itimeyr*1./(comyear(impct)*1.)*(chg_prdwat_cst(impct)-1.)+1.
           chg_chmpnt_fac(1)     =    itimeyr*1./(comyear(impct)*1.)*(chg_chmpnt_cst(impct)-1.)+1.
           chg_plypnt_fac(1)     =    itimeyr*1./(comyear(impct)*1.)*(chg_plypnt_cst(impct)-1.)+1.
           chg_co2pnt_fac(1)     =    itimeyr*1./(comyear(impct)*1.)*(chg_co2pnt_cst(impct)-1.)+1.
           chg_stmgen_fac(1)     =    itimeyr*1./(comyear(impct)*1.)*(chg_stmgen_cst(impct)-1.)+1.
           chg_foam_fac(1)       =    itimeyr*1./(comyear(impct)*1.)*(chg_foam_cst(impct)-1.)+1.
           chg_voam_fac(1)       =    itimeyr*1./(comyear(impct)*1.)*(chg_voam_cst(impct)-1.)+1.
           chg_gna_fac(1)        =    itimeyr*1./(comyear(impct)*1.)*(chg_gna_cst(impct)-1.)+1.
           chg_annsec_fac(1)     =    itimeyr*1./(comyear(impct)*1.)*(chg_annsec_cst(impct)-1.)+1.
           chg_lft_fac(1)        =    itimeyr*1./(comyear(impct)*1.)*(chg_lft_cst(impct)-1.)+1.
           chg_secwrk_fac(1)     =    itimeyr*1./(comyear(impct)*1.)*(chg_secwrk_cst(impct)-1.)+1.
           chg_injc_fac(1)       =    itimeyr*1./(comyear(impct)*1.)*(chg_injc_cst(impct)-1.)+1.
           chg_injt_fac(1)       =    itimeyr*1./(comyear(impct)*1.)*(chg_injt_cst(impct)-1.)+1.
           chg_cmp_fac(1)        =    itimeyr*1./(comyear(impct)*1.)*(chg_cmp_cst(impct)-1.)+1.
           chg_drl_fac(2)        =    chg_drl_cst(impct)
           chg_stm_fac(2)        =    chg_stm_cst(impct)
           chg_comp_fac(2)       =    chg_comp_cst(impct)
           chg_fac_fac(2)        =    chg_fac_cst(impct)
           chg_secconv_fac(2)    =    chg_secconv_cst(impct)
           chg_injconv_fac(2)    =    chg_injconv_cst(impct)
           chg_facupg_fac(2)     =    chg_facupg_cst(impct)
           chg_wrk_fac(2)        =    chg_wrk_cst(impct)
           chg_prdwat_fac(2)     =    chg_prdwat_cst(impct)
           chg_chmpnt_fac(2)     =    chg_chmpnt_cst(impct)
           chg_plypnt_fac(2)     =    chg_plypnt_cst(impct)
           chg_co2pnt_fac(2)     =    chg_co2pnt_cst(impct)
           chg_stmgen_fac(2)     =    chg_stmgen_cst(impct)
           chg_foam_fac(2)       =    chg_foam_cst(impct)
           chg_voam_fac(2)       =    chg_voam_cst(impct)
           chg_gna_fac(2)        =    chg_gna_cst(impct)
           chg_annsec_fac(2)     =    chg_annsec_cst(impct)
           chg_lft_fac(2)        =    chg_lft_cst(impct)
           chg_secwrk_fac(2)     =    chg_secwrk_cst(impct)
           chg_injc_fac(2)       =    chg_injc_cst(impct)
           chg_injt_fac(2)       =    chg_injt_cst(impct)
           chg_cmp_fac(2)        =    chg_cmp_cst(impct)
  
           chg_ooil_fac(2)       =    chg_ooil_cst(impct)       
           chg_ogas_fac(2)       =    chg_ogas_cst(impct)      
           chg_owat_fac(2)       =    chg_owat_cst(impct)     
           chg_oinj_fac(2)       =    chg_oinj_cst(impct)    
  
           cst_drl_fac(2)        =    cst_drl_cst(impct)
           cst_stm_fac(2)        =    cst_stm_cst(impct)
           cst_comp_fac(2)       =    cst_comp_cst(impct)
           cst_fac_fac(2)        =    cst_fac_cst(impct)
           cst_secconv_fac(2)    =    cst_secconv_cst(impct)
           cst_injconv_fac(2)    =    cst_injconv_cst(impct)
           cst_facupg_fac(2)     =    cst_facupg_cst(impct)
           cst_wrk_fac(2)        =    cst_wrk_cst(impct)
           cst_foam_fac(2)       =    cst_foam_cst(impct)
           cst_voam_fac(2)       =    cst_voam_cst(impct)
           cst_gna_fac(2)        =    cst_gna_cst(impct)
           cst_annsec_fac(2)     =    cst_annsec_cst(impct)
           cst_lft_fac(2)        =    cst_lft_cst(impct)
           cst_secwrk_fac(2)     =    cst_secwrk_cst(impct)
           cst_injc_fac(2)       =    cst_injc_cst(impct)
           cst_cmp_fac(2)        =    cst_cmp_cst(impct)
  
           cstp_drl_fac(2)       =    cst_drl_cstp(impct)
           cstp_stm_fac(2)       =    cst_stm_cstp(impct)
           cstp_comp_fac(2)      =    cst_comp_cstp(impct)
           cstp_fac_fac(2)       =    cst_fac_cstp(impct)
           cstp_secconv_fac(2)   =    cst_secconv_cstp(impct)
           cstp_injconv_fac(2)   =    cst_injconv_cstp(impct)
           cstp_facupg_fac(2)    =    cst_facupg_cstp(impct)
           cstp_wrk_fac(2)       =    cst_wrk_cstp(impct)
           cstp_foam_fac(2)      =    cst_foam_cstp(impct)
           cstp_voam_fac(2)      =    cst_voam_cstp(impct)
           cstp_gna_fac(2)       =    cst_gna_cstp(impct)
           cstp_annsec_fac(2)    =    cst_annsec_cstp(impct)
           cstp_lft_fac(2)       =    cst_lft_cstp(impct)
           cstp_secwrk_fac(2)    =    cst_secwrk_cstp(impct)
           cstp_injc_fac(2)      =    cst_injc_cstp(impct)
           cstp_cmp_fac(2)       =    cst_cmp_cstp(impct)
  
           firstcom_fac(2)       =    firstcom(impct)
           comyear_fac(2)        =    comyear(impct)
           tech_curve_fac(2)     =    tech_curve(impct)
           mark_pen_fac(2)       =    mark_pen(impct)
           prob_rd_fac(2)        =    prob_rd(impct)
           prob_imp_fac(2)       =    prob_imp(impct)
  

! set the variables for the technology penetration curve.  Advanced
       YR_SC_START = L48B4YR
       YR_COM_START = firstcom(impct)
       yr_com = comyear(impct)
       PROBR = prob_rd(impct)
       PROBI = prob_imp(impct)
       PCURVE = tech_curve(impct)
       UMP = mark_pen(impct)
       YR_START = YR_COM_START - YR_SC_START


        DO iyr=1, MAX_YR  !start market penetration curve calculation
            IF (iyr < YR_START) THEN
              ATECH = 0.0
            ELSEIF(iyr > (YR_START+YR_COM-1)) THEN
              ATECH = UMP*PROBI*probr
           ELSE
               IF (PCURVE == 1) THEN
                 ATECH1 = - 0.9*0.1**((iyr-YR_START)/YR_COM)     ! Changed 0.4 tp 0.1
                 ATECH = ((-0.6523 - ATECH1)/(-0.6523 + 0.036))
                 ATECH = ATECH * PROBI * PROBR
                  IF (ATECH <= 0.0) ATECH = 0.0
                  IF (ATECH >= UMP*probi*probr) ATECH = UMP*probi*probr
               ELSEIF (PCURVE == 2) THEN
                 ATECH1 = 0.9 * 0.04 ** (1-(iyr-YR_START)/YR_COM)
                 ATECH = ((0.04 - ATECH1)/(0.04 - 0.74678))
                 ATECH = ATECH * PROBI * PROBR
                  IF (ATECH <= 0.0) ATECH = 0.0
                  IF (ATECH >= UMP*probi*probr) ATECH = UMP*probi*probr
               ELSEIF (PCURVE == 3) THEN
                 MIDPOINT = INT(YR_COM/2.0)
                 IMID = iyr - MIDPOINT - YR_START + 1
                 ATECH1 = EXP(FLOAT(IMID))/(1 +EXP(FLOAT(IMID)))
                 ATECH = ATECH1
                 ATECH = ATECH * PROBI * PROBR
                  IF (ATECH <= 0.0) ATECH = 0.0
                  IF (ATECH >= UMP*probi*probr) ATECH = UMP*probi*probr
               ELSEIF (pcurve == 4) then
                 ATECH = (iyr-yr_start) * (UMP * PROBI * PROBR)/YR_COM
                  IF (ATECH <= 0.0) ATECH = 0.0
                  IF (ATECH >= UMP*probi*probr) ATECH = UMP*probi*probr
              END IF
           END IF
           ADVANCED(iyr) = ATECH
        END DO !end market penetration curve calculation

        amlt = 0.0
        bmlt = 0.0
        cmlt = 0.0
        dmlt = 0.0
        emlt = 0.0
        fmlt = 0.0
        gmlt = 0.0

        ooip = 0.0
        if (asoi(ires) .ne. 0.0 .and. aboi(ires) .ne. 0.0) &
            ooip = (7758.0 * (apatsiz(ires)*apay(ires)*aphi(ires)/100.0*asoi(ires)/100.0)/aboi(ires))

!  levers
!  undiscovered & primary oil
        if(apcode(ires) == 1.or.apcode(ires) == 10.or.apcode(ires) == 17) then
        !    skin
          if(skin_fac(2) /= skin_fac(1)) then   
            amlt = techb_var(1)+techb_var(2)*skin_fac(2)
            do iyr = 1,max_yr
              aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * (amlt)
            end do
          end if

        !    recovery efficiency
          if(rec_eff_fac(2) /= rec_eff_fac(1)) then
            do iyr = 1,max_yr
              oilp(iyr) = aprodoil(ires,iyr,1) * (rec_eff_fac(2))
              gasp(iyr) = aprodgas(ires,iyr,1) * (rec_eff_fac(2))
              watp(iyr) = aprodwat(ires,iyr,1) * (rec_eff_fac(2))
              cmlt = cmlt + oilp(iyr)
              if(cmlt >= tech02_fac(2)*ooip) then
                oilp(iyr) = 0.0
                gasp(iyr) = 0.0
                watp(iyr) = 0.0
              else
                oilp(iyr) = aprodoil(ires,iyr,1) * (rec_eff_fac(2)-1.0)
                gasp(iyr) = aprodgas(ires,iyr,1) * (rec_eff_fac(2)-1.0)
                watp(iyr) = aprodwat(ires,iyr,1) * (rec_eff_fac(2)-1.0)
              end if
              aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + oilp(iyr)
              aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + gasp(iyr)
              aprodwat(ires,iyr,2) = aprodwat(ires,iyr,2) + watp(iyr)
            end do
          end if
        end if


!  steam flood
         if(apcode(ires) == 4) then
        !    recovery efficiency
           if(rec_eff_fac(2) /= rec_eff_fac(1)) then
             do iyr = 1,max_yr
                aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * (rec_eff_fac(2)-1.)
             end do
           end if
         end if


!  co2 eor
         if(apcode(ires) == 3) then
        !    injection rate
           if(inj_rate_fac(2) /= inj_rate_fac(1)) then
             AMLT = 0.0
             amlt = tech3_var(1)*(inj_rate_fac(2)-inj_rate_fac(1))
             do iyr = 1,max_yr
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * (amlt)
               awatinj(ires,iyr,2) = awatinj(ires,iyr,2) + awatinj(ires,iyr,1) * (amlt)
               ainjinj(ires,iyr,2) = ainjinj(ires,iyr,2) + ainjinj(ires,iyr,1) * (amlt)
               aprodwat(ires,iyr,2) = aprodwat(ires,iyr,2) + aprodwat(ires,iyr,1) * (amlt)
               aprodco2(ires,iyr,2) = aprodco2(ires,iyr,2) + aprodco2(ires,iyr,1) * (amlt)
               ainjrecy(ires,iyr,2) = ainjrecy(ires,iyr,2) + ainjrecy(ires,iyr,1) * (amlt)
             end do
           end if

         !    sweep efficiency
            if(vol_swp_fac(2) /= vol_swp_fac(1)) then
              amlt = 0.0
              amlt = tech4_var(1)*((vol_swp_fac(2)-vol_swp_fac(1))**2)+tech4_var(2)*(vol_swp_fac(2)-vol_swp_fac(1)) &
                     +tech4_var(3)
              do iyr = 1,max_yr
                aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * (amlt)
              end do
            end if

         !    WAG ratio
            if(tech01_fac(2) /= tech01_fac(1)) then
              amlt = 0.0
              amlt = tech1_var(1)*((tech01_fac(1) - tech01_fac(2))**2) + (tech01_fac(1)-tech01_fac(2))*tech1_var(2)
              do iyr = 1,max_yr
                aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * (amlt)
              end do
            end if

         !    oil derating
            if(oil_rat_fac(2) /= oil_rat_fac(1)) then
              do iyr = 1,max_yr
                aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * (oil_rat_fac(2)-1.0)
              end do
            end if

         !    co2 derating
            if(co2_rat_fac(2) /= co2_rat_fac(1)) then
              do iyr = 1,max_yr
                ainjinj(ires,iyr,2) = ainjinj(ires,iyr,2) + ainjinj(ires,iyr,1) * (co2_rat_fac(2)-1.0)
                aprodco2(ires,iyr,2) = aprodco2(ires,iyr,2) + aprodco2(ires,iyr,1) * (co2_rat_fac(2)-1.0)
              end do
            end if
          end if


!  polymer flooding
         if(apcode(ires) == 5) then
        !    sweep efficiency
           if(rec_eff_fac(2) /= rec_eff_fac(1)) then
             do iyr = 1,max_yr
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * (rec_eff_fac(2)-1.)
               aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (rec_eff_fac(2)-1.)
               aprodwat(ires,iyr,2) = aprodwat(ires,iyr,2) + aprodwat(ires,iyr,1) * (rec_eff_fac(2)-1.)
             end do
           end if

      !    injection rate change
           if(inj_rate_fac(2) /= inj_rate_fac(1)) then
             cmlt = 0.0
             do iyr = 1,max_yr
               cmlt = cmlt + aprodoil(ires,iyr,1)
             end do

             amlt = tech2_var(1)*((inj_rate_fac(2)-1.0)**2) + tech2_var(2)*(inj_rate_fac(2)-1.0)
             dmlt = 0.0
             do iyr = 1,max_yr
               if(iyr == 1) then
                 oilp(iyr) = aprodoil(ires,iyr,1) * (1.0 + amlt)
               else
                 oilp(iyr) = aprodoil(ires,iyr,1) 
               endif
               dmlt = dmlt + oilp(iyr)
               if(dmlt >= cmlt) then
                 oilp(iyr) = max(cmlt-(dmlt-oilp(iyr)),0.0)
               end if
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + oilp(iyr) - aprodoil(ires,iyr,1)
             end do
           end if
         end if

!  profile modification
         if(apcode(ires) == 7) then
         !    sweep efficiency
           if(vol_swp_fac(2) /= vol_swp_fac(1)) then
             do iyr = 1,max_yr
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * (vol_swp_fac(2)-1.)
             end do
           end if
         end if


!  infill drilling
         if(apcode(ires) == 6) then
        !    continuity increase
           if(contin_fac(2) /= contin_fac(1)) then
             do iyr = 1,max_yr
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * ((tech6_var(1)+ (tech6_var(2)*(contin_fac(2)-1.0))))
             end do
           end if

        !    injection rate change
           if(inj_rate_fac(2) /= inj_rate_fac(1)) then
             do iyr = 1,max_yr
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * ((tech8_var(1) + (tech8_var(2)*(inj_rate_fac(2)-1.0))))
             end do
           end if
         end if


!  horizontal continuity
         if(apcode(ires) == 8) then
        !    continuity increase
           if(contin_fac(2) /= contin_fac(1)) then
             do iyr = 1,max_yr
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * ((tech7_var(1)+(tech7_var(2)*(contin_fac(2)-CONtin_fac(1)))))
             end do
           end if
        !    injection rate change
           if(inj_rate_fac(2) /= inj_rate_fac(1)) then
             amlt = 0.0
             amlt = tech9_var(1)*(inj_rate_fac(2)-inj_rate_fac(1))+tech9_var(2)
             do iyr = 1,max_yr
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * (amlt)
             end do
           end if
         end if


!  horizontal profile
         if(apcode(ires) == 9) then
        !    sweep efficiency
           if(vol_swp_fac(2) /= vol_swp_fac(1)) then
             do iyr = 1, max_yr
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * ((tech5_var(1)+(tech5_var(2)*(vol_swp_fac(2)-vol_swp_fac(1)))))
             end do
           end if

        !    injection rate change
           if(inj_rate_fac(2) /= inj_rate_fac(1)) then
             do iyr = 1,max_yr
               aprodoil(ires,iyr,2) = aprodoil(ires,iyr,2) + aprodoil(ires,iyr,1) * ((tech10_var(1)+ &
                                      (tech10_var(2)*(inj_rate_fac(2)-inj_rate_fac(1)))))
             end do
           end if
         end if


!  undiscovered conventional gas
         if(apcode(ires) == 16) then
        !    recovery efficiency change
           if(rec_eff_fac(2) /= rec_eff_fac(1)) then
             do iyr = 1,max_yr
               aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (rec_eff_fac(2)-1.)
             end do
           end if

        !    vertical skin change (default = 3)
            if(tech03_fac(2) /= tech03_fac(1)) then
             amlt = 0.0
             bmlt = 0.0
             cmlt = 0.0
      
             do iyr = 1,max_yr
               bmlt = bmlt + aprodgas(ires,iyr,1)
             end do

             amlt = tech11_var(1)*(tech03_fac(2)-tech03_fac(1))

             do iyr = 1,max_yr
               gasp(iyr) = aprodgas(ires,iyr,1) * (1.0 + amlt)
               cmlt = cmlt + gasp(iyr)
               if(cmlt >= bmlt) then
                 gasp(iyr) =  max(gasp(iyr)-(cmlt-bmlt),0.0)
               end if
               aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + gasp(iyr) - aprodgas(ires,iyr,1)
             end do
           end if

        !    pay continuity
         if(paycont_fac(2) /= paycont_fac(1)) then
             amlt = 0.0
             bmlt = 0.0
             cmlt = 0.0
             dmlt = 0.0

             do iyr = 1,max_yr
               bmlt = bmlt + aprodgas(ires,iyr,1)
             end do

             amlt = tech20_var(1)*(paycont_fac(2)**2) + tech20_var(2)*paycont_fac(2) + tech20_Var(3)
             bmlt = bmlt * (1.0 + amlt)

             dmlt = tech21_var(1)*(paycont_fac(2)**3) + (tech21_var(2)*(paycont_fac(2)**2)) &
                     + tech21_var(3)*paycont_fac(2) + tech21_var(4)

             do iyr = 1,max_yr
               gasp(iyr) = aprodgas(ires,iyr,1) * (1.0 + dmlt)
               cmlt = cmlt + gasp(iyr)
               if(cmlt >= bmlt) then
                 gasp(iyr) =  max(gasp(iyr)-(cmlt-bmlt),0.0)
               end if
               aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + gasp(iyr) - aprodgas(ires,iyr,1)
             end do
           end if
         end if


!  undiscovered tight gas
         if(apcode(ires) == 18) then
        !    recovery efficiency change
           if(rec_eff_fac(2) /= rec_eff_fac(1)) then
             do iyr = 1,max_yr
               aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (rec_eff_fac(2)-1.)
             end do
           end if

        !    fracture length change
           if(tech04_fac(2) /= tech04_fac(1)) then
             amlt = 0.0
             amlt = tech32_var(1)*(tech04_fac(2)) + tech32_var(2)
             do iyr = 1,max_yr
               aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (amlt)
             end do
           end if

        !    fracture conductivity
           if(tech05_fac(2) /= tech05_fac(1)) then
             amlt = 0.0
             amlt = tech30_var(1)*(tech05_fac(2)-tech05_fac(1))

             do iyr = 1,max_yr
               aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (amlt)
             end do
           end if

        !    pay continuity
           if(paycont_fac(2) /= paycont_fac(1)) then
             amlt = 0.0
             bmlt = 0.0
             cmlt = 0.0
             dmlt = 0.0

             do iyr = 1,max_yr
               bmlt = bmlt + aprodgas(ires,iyr,1)
             end do

             amlt = tech22_var(1)*(paycont_fac(2)**2) + tech22_var(2)*paycont_fac(2) + tech22_var(3)
             bmlt = bmlt * (1.0 + amlt)

             dmlt = tech23_var(1)*(paycont_fac(2)**3) + (tech23_var(2)*(paycont_fac(2)**2)) &
                     + tech23_var(3)*paycont_fac(2) + tech23_Var(4)

             do iyr = 1,max_yr
               gasp(iyr) = aprodgas(ires,iyr,1) * (1.0 + dmlt)
               cmlt = cmlt + gasp(iyr)
               if(cmlt >= bmlt) then
                 gasp(iyr) =  max(gasp(iyr)-(cmlt-bmlt),0.0)
               end if
               aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + gasp(iyr) - aprodgas(ires,iyr,1)
             end do
           end if
         end if


!  undiscovered coalbed methane
      if(apcode(ires) == 19) then
        !    recovery efficiency change
         if(rec_eff_fac(2) /= rec_eff_fac(1)) then
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (rec_eff_fac(2)-1.)
           end do
         end if

        !    vertical skin change


         if(tech03_fac(2) /= tech03_fac(1)) then
           amlt = 0.0
           bmlt = 0.0
           cmlt = 0.0
           dmlt = 0.0
          !change in cumulative gas
           amlt = tech12_var(1)*((tech03_fac(2)-tech03_fac(1))**2)+(tech12_var(2)*(tech03_fac(2)-tech03_fac(1)))
           do iyr = 1,max_yr
             bmlt = bmlt + aprodgas(ires,iyr,1)
           end do
           bmlt = bmlt * (1.0 + amlt)

           dmlt = tech13_var(1)*((tech03_fac(2)-tech03_fac(1))**3)+(tech13_var(2) &
                   *(tech03_fac(2)-tech03_fac(1))**2)+tech13_var(3)*(tech03_fac(2)-tech03_fac(1))

          !change in annual rate
           do iyr = 1,max_yr
             gasp(iyr) = aprodgas(ires,iyr,1) * (1.0 + dmlt)
             cmlt = cmlt + gasp(iyr)
             if(cmlt >= bmlt) then
               gasp(iyr) =  max(gasp(iyr)-(cmlt-bmlt),0.0)
             end if
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + gasp(iyr) - aprodgas(ires,iyr,1)
           end do
         end if

        !    fracture conductivity
         if(tech05_fac(2) /= tech05_fac(1)) then
           amlt = 0.0
           amlt = tech26_var(1) * (tech05_fac(2)-tech05_fac(1))**2 + tech26_var(2) * (tech05_fac(2)-tech05_fac(1))
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (amlt)
           end do
         end if
       end if

!  undiscovered shale gas
      if(apcode(ires) == 20) then
        !    recovery efficiency change
         if(rec_eff_fac(2) /= rec_eff_fac(1)) then
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (rec_eff_fac(2)-1.)
           end do
         end if

        !    vertical skin change
         if(tech03_fac(2) /= tech03_fac(1)) then
            amlt = 0.0
            bmlt = 0.0
            cmlt = 0.0
            dmlt = 0.0
           !change in cumulative gas
            amlt = tech14_var(1)*((tech03_fac(2)-tech03_fac(1))**2)+(tech14_var(2)*(tech03_fac(2)-tech03_fac(1)))

            do iyr = 1,max_yr
              bmlt = bmlt + aprodgas(ires,iyr,1)
            end do
            bmlt = bmlt * (1.0 + amlt)
            dmlt = tech15_var(1)*((tech03_fac(2)-tech03_fac(1))**3)+ &
                   (tech15_var(2)*(tech03_fac(2)-tech03_fac(1))**2)+tech15_var(3)*(tech03_fac(2)-tech03_fac(1))
           !change in annual rate
            do iyr = 1,max_yr
              gasp(iyr) = aprodgas(ires,iyr,1) * (1.0 + dmlt)
              cmlt = cmlt + gasp(iyr)
              if(cmlt >= bmlt) then
                gasp(iyr) =  max(gasp(iyr)-(cmlt-bmlt),0.0)
              end if
              aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + gasp(iyr) - aprodgas(ires,iyr,1)
            end do
          end if

        !    fracture conductivity
         if(tech05_fac(2) /= tech05_fac(1)) then
           amlt = 0.0
           amlt = tech27_var(1) * (tech05_fac(2)-tech05_fac(1))**2 + tech27_var(2) * (tech05_fac(2)-tech05_fac(1))
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (amlt)
           end do
         end if
       end if


!  developing shale gas
      if(apcode(ires) == 21) then
        !    recovery efficiency
         if(rec_eff_fac(2) /= rec_eff_fac(1)) then
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (rec_eff_fac(2)-1.)
           end do
         end if

        !    vertical skin change
         if(tech03_fac(2) /= tech03_fac(1)) then
            amlt = 0.0
            bmlt = 0.0
            cmlt = 0.0
            dmlt = 0.0
           !change in cumulative gas
            amlt = tech18_var(1)*((tech03_fac(2)-tech03_fac(1))**2)+(tech18_var(2)*(tech03_fac(2)-tech03_fac(1)))
            do iyr = 1,max_yr
              bmlt = bmlt + aprodgas(ires,iyr,1)
            end do
            bmlt = bmlt * (1.0 + amlt)

            dmlt = tech19_var(1)*((tech03_fac(2)-tech03_fac(1))**3)+(tech19_var(2)*(tech03_fac(2)-tech03_fac(1))**2)+ &
                    tech19_var(3)*(tech03_fac(2)-tech03_fac(1))
           !change in annual rate
            do iyr = 1,max_yr
              gasp(iyr) = aprodgas(ires,iyr,1) * (1.0 + dmlt)
              cmlt = cmlt + gasp(iyr)
              if(cmlt >= bmlt) then
                gasp(iyr) =  max(gasp(iyr)-(cmlt-bmlt),0.0)
              end if
              aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + gasp(iyr) - aprodgas(ires,iyr,1)
            end do
         end if

        !    fracture conductivity
         if(tech05_fac(2) /= tech05_fac(1)) then
           amlt = 0.0
           amlt = tech29_var(1) * (tech05_fac(2)-tech05_fac(1))**2 + tech29_var(2) * (tech05_fac(2)-tech05_fac(1))
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (amlt)
           end do
         end if
       end if


!  developing tight gas
      if(apcode(ires) == 22) then
        !    recovery efficiency change
         if(rec_eff_fac(2) /= rec_eff_fac(1)) then
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (rec_eff_fac(2)-1.)
           end do
         end if

        !    fracture length change
         if(tech04_fac(2) /= tech04_fac(1)) then
           amlt = 0.0
           amlt = tech33_var(1)*tech04_fac(2) + tech33_var(2)
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (amlt)
           end do
         end if

        !    fracture conductivity
         if(tech05_fac(2) /= tech05_fac(1)) then
           amlt = 0.0
           amlt = tech31_var(1)*(tech05_fac(2)-tech05_fac(1))
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (amlt)
           end do
         end if

        !    pay continuity
         if(paycont_fac(2) /= paycont_fac(1)) then
           amlt = 0.0
           bmlt = 0.0
           cmlt = 0.0
           dmlt = 0.0

           do iyr = 1,max_yr
             bmlt = bmlt + aprodgas(ires,iyr,1)
           end do

           amlt = tech24_var(1)*(paycont_fac(2)**2) + tech24_var(2)*paycont_fac(2) + tech24_var(3)
           bmlt = bmlt * (1.0 + amlt)

           dmlt = tech25_var(1)*(paycont_fac(2)**3) + (tech25_var(2)*(paycont_fac(2)**2)) + &
                    tech25_var(3)*paycont_fac(2) + tech25_var(4)

           do iyr = 1,max_yr
             gasp(iyr) = aprodgas(ires,iyr,1) * (1.0 + dmlt)
             cmlt = cmlt + gasp(iyr)
             if(cmlt >= bmlt) then
               gasp(iyr) =  max(gasp(iyr)-(cmlt-bmlt),0.0)
             end if
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + gasp(iyr) - aprodgas(ires,iyr,1)
           end do
         end if
       end if


!  developing coalbed methane
      if(apcode(ires) == 23) then
        !    recovery efficiency change
         if(rec_eff_fac(2) /= rec_eff_fac(1)) then
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (rec_eff_fac(2)-1.)
           end do
         end if

        !    vertical skin change
         if(tech03_fac(2) /= tech03_fac(1)) then
            amlt = 0.0
            bmlt = 0.0
            cmlt = 0.0
            dmlt = 0.0
           !change in cumulative gas
            amlt = tech16_var(1)*((tech03_fac(2)-tech03_fac(1))**2)+(tech16_var(2)*(tech03_fac(2)-tech03_fac(1)))
            do iyr = 1,max_yr
              bmlt = bmlt + aprodgas(ires,iyr,1)
            end do
            bmlt = bmlt * (1.0 + amlt)

            dmlt = tech17_var(1)*((tech03_fac(2)-tech03_fac(1))**3)+ &
                    (tech17_var(2)*(tech03_fac(2)-tech03_fac(1))**2)+tech17_var(3)*(tech03_fac(2)-tech03_fac(1))
           !change in annual rate
            do iyr = 1,max_yr
              gasp(iyr) = aprodgas(ires,iyr,1) * (1.0 + dmlt)
              cmlt = cmlt + gasp(iyr)
              if(cmlt >= bmlt) then
                gasp(iyr) =  max(gasp(iyr)-(cmlt-bmlt),0.0)
              end if
              aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + gasp(iyr) - aprodgas(ires,iyr,1)
            end do
         end if

        !    fracture conductivity
         if(tech05_fac(2) /= tech05_fac(1)) then
           amlt = 0.0
           amlt = tech28_var(1) * (tech05_fac(2)-tech05_fac(1))**2 + tech28_var(2) * (tech05_fac(2)-tech05_fac(1))
           do iyr = 1,max_yr
             aprodgas(ires,iyr,2) = aprodgas(ires,iyr,2) + aprodgas(ires,iyr,1) * (amlt)
           end do
         end if
       end if

!  end of levers
!      if (aplay_cde(ires) == 6761)  &
!        write(6,*) 'dh5eurg', curiyr+1989, aresid(ires), sum(aprodgas(ires,1:max_yr,1)), sum(aprodgas(ires,1:max_yr,2))
!      if (aplay_cde(ires) == 3111)  &
!        write(6,*) 'dh5euro', curiyr+1989, aresid(ires), sum(aprodoil(ires,1:max_yr,1)), sum(aprodoil(ires,1:max_yr,2))

       end if !end of advanced case calculations

       end subroutine


!********************************************************************************
!********************************************************************************
!********************************************************************************

       subroutine check_access(ires)

! THIS SUBROUTINE CHECKS THE RESOURCE ACCESS CATEGORY AGAINST THE YEAR AVAILABLE

!  If the project has not been timed, check against the year in which the resource
!  access category becomes available.  If the current year of analysis is greater
!  than the first year available, tag the reservoir as eligible.

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include 'ogsmugr'
       include 'ogsml48'
       include 'ogsmbfw'

       INTEGER ires,ival
       INTEGER iprov,iacc
       LOGICAL found

       if (.not.timed(ires)) then                                            !if the project has not been timed
         found = .false.
         do iprov = 1,max_prov                                               !match the province
            if (aprov(ires)+5000 == provnum(iprov)) then
              if (L48B4YR-1+itimeyr >= provacc(iprov,aresacc(ires))) then    !check the current year of analysis against the year in the access file
                 eligible(ires) = .true.
              else
                 eligible(ires) = .false.
              end if
              found = .TRUE.
            end if
         end do
         if (.not.found) write(ogbug1,*) 'WARNING: MISSING PROVINCE NUMBER IN ACCESS LIST  ', aresid(ires), aprov(ires)+5000

 1225    format (i4,2x,a11,2x,i8,2x,i8,2x,i6,2x,i6,2x,l8)
       else
         eligible(ires) = .false.
       end if

       end subroutine

!***************************************************************
!from DECLINE_OIL.FOR
       SUBROUTINE OIL_DECLINE(ires)

        implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include 'ogsmugr'
        include 'ogsml48'
        include 'ogsmbfw'

        INTEGER ires,ist,ir, iRRC, dist, iprov
        INTEGER iy,ac
        CHARACTER*2 st1
        LOGICAL drill

!       this subroutine calculates the economics for the oil reservoir undergoing decline curve analysis

        call economics(ires,1,1,0,0,aregion(ires))

        read (aresid(ires)(3:4),'(a2)')   st1
        read (aresid(ires)(10:11),'(i2)') ac
        IR = aregion(ires)
        IPROV = aprov(ires)
        iRRC = int(ARRC(ires))
        call dist_match(st1,irrc,IR,dist)


               timed(ires) = .true.
               timedyr(ires) = 1
               eligible(ires) = .false.


        IF(apcode(ires) == 0) THEN

             DO IY=1,MAX_YR
!              IF(aresflag(ires) == 0) then        !scale the non-eor decline reservoirs
               IF(ac == 0) then        !scale the non-eor decline reservoirs
                 IF (aplay_cde(ires) == 2804.or.(aplay_cde(ires) >= 3110.and.aplay_cde(ires) <= 3115).or.  &
                     aplay_cde(ires) == 4763.or.aplay_cde(ires) == 5875.or.aplay_cde(ires) == 99926039.or.  &
                     aplay_cde(ires) == 4409.or.(aplay_cde(ires) >= 4747.and.aplay_cde(ires) <= 4749).or.  &
                     aplay_cde(ires) == 3904.or.aplay_cde(ires) == 3920.or.aplay_cde(ires) == 99905037.or.  &
                     aplay_cde(ires) == 4473.or.aplay_cde(ires) == 4401.or.aplay_cde(ires) == 99943037.or.  &
                     aplay_cde(ires) == 99949033.or.aplay_cde(ires) == 99949037.or.  &
                     aplay_cde(ires) == 1201.or.aplay_cde(ires) == 99904010.or.aresflag(ires) == 9) then   ! tight oil

                        IF(reg_adjoil(ir) > 0.0) EPRODOIL(ires,iy) = EPRODOIL(ires,iy) * reg_adjoil(ir)
                        IF(reg_adjoil(ir) > 0.0) EREMRES(ires,iy)  = eremres(ires,iy)  * reg_adjoil(ir)
                        IF(reg_adjadg(ir) > 0.0) EPRODGAS(ires,iy) = EPRODGAS(ires,iy) * reg_adjadg(ir)
                        IF(reg_adjadg(ir) > 0.0) EGREMRES(ires,iy)  = egremres(ires,iy)  * reg_adjadg(ir)
                        IF(dist_adjoil(dist,2) >= 0.0) EPRODOIL(ires,iy) = EPRODOIL(ires,iy) * dist_adjoil(dist,2)
                        IF(dist_adjadg(dist,2) >= 0.0) EPRODGAS(ires,iy) = EPRODGAS(ires,iy) * dist_adjadg(dist,2)
                  ELSE
                     IF(dist_adjoil(dist,1) >= 0.0) EPRODOIL(ires,iy) = EPRODOIL(ires,iy) * dist_adjoil(dist,1)
                     IF(dist_adjadg(dist,1) >= 0.0) EPRODGAS(ires,iy) = EPRODGAS(ires,iy) * dist_adjadg(dist,1)
                  END if
               END if
             END DO


               call write_output(ires,1)                              !write the project to the output file
               CALL AGG_OGSM(ires,0)                                  !aggregate the timed projects

               dprodgas(ires,:) = eprodgas(ires,:)
               dprodoil(ires,:) = eprodoil(ires,:)
               dregion(ires) = aregion(ires)
               dpadd(ires) = apadd(ires)
               dcrdtype(ires) = acrdtype(ires)
               dngl(ires) = angl(ires)
               dngplet(ires) = angplet(ires)
               dngplpr(ires) = angplpr(ires)
               dngplbu(ires) = angplbu(ires)
               dngplis(ires) = angplis(ires)
               dngplpp(ires) = angplpp(ires)
               dplay_cde(ires) = aplay_cde(ires)
               drrc(ires) = arrc(ires)
               dresflag(ires) = aresflag(ires)
               dresid(ires) = aresid(ires)
               dprov(ires) = aprov(ires)
               dresacc(ires) = aresacc(ires)
               ddepth(ires) = adepth(ires)
               DTIMED(IRES)   = TIMED(IRES)                               

               viable(apcode(ires)+1,1) = viable(apcode(ires)+1,1)+1   !store the number of economically viable reservoirs

        ENDIF

        end subroutine
!******************************************************************
!******************************************************************
!******************************************************************
!******************************************************************
!******************************************************************

        subroutine waterflood(ires,jres,jtimeyr)
!  THIS SUBROUTINE CALCULATES THE WATERFLOOD PRODUCTION FOR THE RESERVOIR USING THE WATERFLOOD TYPE CURVE
!
!  STEPS:
!       1. TRANSFER VARIABLES INTO
!       2. CALCULATE REQUIRED PROPERTIES FOR FIRST EOR WINDOW YEAR: SOC,BOC,...
!       3. CALCULATE INCREMENTAL PRODUCTION
!       4. CREATE WATERFLOOD PROJECT

        implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include 'ogsmugr'
        INCLUDE 'ogsml48'
        INCLUDE 'ogsmbfw'

        INTEGER ires                                    !newly created waterflood project number
        INTEGER jres                                    !source project number - this data is used to create the waterflood
        INTEGER jtimeyr                                 !the year in which waterflood is being considered
        INTEGER itech
        INTEGER iy,m2,worl


! local variables
        REAL wlspc
        REAL DEPTH_DEC
        REAL POR_DEC
        REAL PAY_DEC
        REAL PERM_DEC
        REAL SOC_DEC
        REAL boc
        REAL apigrav
        REAL visc
        REAL VDP_DEC
        REAL swi
        REAL sorw
        REAL ooip,ooipc
        REAL porevol
        REAL SOI_DEC
        REAL BOI_DEC
        REAL sgg
        REAL temp
        REAL pres
        REAL numwell
        REAL cumoil                            !cumulative oil production (mbbl) until start of waterflood
        REAL annprdrt
        REAL cumwat25
        REAL cumoil25
        REAL maxpat,worc
        real rtemp, rtemp2

        REAL annoil(max_yr),cumprd(max_yr), &
         cumwat(max_yr)           !!!,watinj(max_yr),watprod(max_yr)

! initialization
        wlspc = 0.0
        DEPTH_DEC = 0.0
        POR_DEC = 0.0
        PAY_DEC = 0.0
        PERM_DEC = 0.0
        SOC_DEC = 0.0
        boc = 0.0
        apigrav = 0.0
        visc = 0.0
        VDP_DEC = 0.0
        swi = 0.0
        sorw = 0.0
        ooip = 0.0
        ooipc = 0.0
        porevol = 0.0
        SOI_DEC = 0.0
        BOI_DEC = 0.0
        numwell = 0.0
        cumoil = 0.0
        temp = 0.0
        pres = 0.0
        sgg = 0.0
        annprdrt = 0.0
        cumwat25 = 0.0
        cumoil25 = 0.0
        maxpat = 0.0

        do iy = 1,max_yr
          watinj(iy) = 0.0
          annoil(iy) = 0.0
          cumprd(iy) = 0.0
          cumwat(iy) = 0.0
          watprod(iy) = 0.0
        end do


! STEP 1. TRANSFER VARIABLES INTO
        resid = aresid(jres)
        wlspc = apatsiz(jres)
        DEPTH_DEC = adepth(jres)
        POR_DEC = aphi(jres)
        PAY_DEC = apay(jres)
        PERM_DEC = aperm(jres)
        apigrav = aapi(jres)
        visc = aoilvis(jres)
        VDP_DEC = avdp(jres)
        swi = aswi(jres)/100.0
        sorw = asor(jres)
        SOI_DEC = asoi(jres)
        BOI_DEC = aboi(jres)
        numwell = atotpat(jres,1)
          ooipc = 7758.0*wlspc*PAY_DEC*POR_DEC/100.0*SOI_DEC/100.0/ &
                  BOI_DEC/1000.0
          ooipc = ooipc * numwell

        sgg = agas_grav(jres)
        temp = atemp(jres)
        pres = apresin(jres)

        !adjust the well spacing to the effective wellspacing
        wlspc = MIN(wlspc*2.0,160.0)
        maxpat = atotacres(jres)/wlspc
        maxpat = float(NINT(maxpat))


! STEP 2. CALCULATE REQUIRED PROPERTIES FOR FIRST EOR WINDOW YEAR: SOC,BOC,...

        do iy = 1,jtimeyr
         IF(iy <= max_yr) cumoil = cumoil+eprodoil(jres,iy)
        end do

        call calbo(temp,apigrav,pres,sgg,boc)

      IF(boc >= BOI_DEC) boc = 0.99*BOI_DEC


      IF(BOI_DEC > 0.0) SOC_DEC = SOI_DEC*(1-cumoil/ooipc)*(boc/BOI_DEC)
      IF(SOC_DEC > SOI_DEC) SOC_DEC = SOI_DEC
      IF(SOC_DEC <= 0.0) SOC_DEC = SOI_DEC

        porevol = (7758. * wlspc * POR_DEC/100. * PAY_DEC)/1000.
        ooip = ((7758.*wlspc*PAY_DEC*(POR_DEC/100.)*(SOC_DEC/100.))/boc) &
                 / 1000.

! CALCULATE 25 YEAR WATER INJECTION
            cumwat25 = exp(-0.457 + 0.201*log(ooip) + 0.725* &
                       log(PERM_DEC) &
                      + (-0.03066*visc) + 0.752*log(PAY_DEC) &
                      + 9.334E-05*DEPTH_DEC &
                      + (-0.00039*PERM_DEC) + 0.208*log(wlspc))
            if (cumwat25 <= 0.) then
              cumwat25 = 0.
              return
            endif

! CALCULATE ANNUAL WATER INJECTION RATE
            annprdrt = 0.1 * porevol / VDP_DEC
            if (annprdrt < 0.) then
              annprdrt = 0.
            endif

! CALCULATE CUMULATIVE WATER INJECTION
            cumwat(1) = annprdrt
            do m=2,max_yr
              cumwat(m) = cumwat(m-1) + annprdrt
               if (cumwat(m) > cumwat25) cumwat(m) = cumwat25
               if (annprdrt > cumwat25) cumwat(m) = annprdrt
            enddo

! CALCULATE ANNUAL WATER INJECTION RATE
            watinj(1) = cumwat(1)
            do m=2,max_yr
              watinj(m) = cumwat(m) - cumwat(m-1)
            enddo

! CALCULATE 25 YEAR OIL PRODUCTION
            cumoil25 = exp(2.583 + (-2.112*log(swi)) &
                     + 0.367*log(cumwat25) &
                     + (-1.692*log(VDP_DEC)) + (-2.291*log(sorw)) &
                     + (-0.149*log(visc)) &
                     + 0.597*log(porevol) + (-0.08275*log(DEPTH_DEC)))* &
                       1.25
            if (cumoil25 < 0.) then
              cumoil25 = 0.
            endif

! CALCULATE CUMULATIVE OIL PRODUCTION
            do m=1,max_yr
               cumprd(m) = exp(3.15078+0.12385*log(cumwat(m)) &
                 + (-0.709258*log(100.0-SOC_DEC)) &
                 + (-0.15146*log(visc)) + (1.68611*log(VDP_DEC)) &
                 + (0.10621*log(PERM_DEC)) + (0.00642*log(DEPTH_DEC)) &
                 + (-0.43792*log(wlspc))+ (-0.11843*log(apigrav)) &
                 + (-0.62505*log(BOI_DEC))+(0.83223*log(ooip)) &
                 + (-0.98954*(1.0/m)))
               if (cumprd(m) > cumoil25) cumprd(m) = cumoil25
            enddo

! CALCULATE ANNUAL OIL PRODUCTION
            annoil(1) = cumprd(1)
            do m=2,max_yr
              annoil(m) = cumprd(m) - cumprd(m-1)
!              if (annoil(m) < 0.0) annoil(m) = 0.0
            enddo


! CALCULATE ANNUAL WATER PRODUCTION
            do m=1,max_yr
              watprod(m) = watinj(m) - (annoil(m)*boc)
               if(watprod(m) <= 0.0) watprod(m)=0.0
            enddo

            do m=1,max_yr
               IF(annoil(m) <= 0.0) annoil(m)=0.0
               IF(watprod(m) <= 0.0) watprod(m) = 0.0
               IF(watinj(m) <= 0.0) watinj(m) = 0.0
            end do

! shut dowm project if WOR>95%
! included MC Sept 3, 2008 at HM's instruction - removed 3.28.09 at HM's instruction
!ccccc            worl = 0
!ccccc            do m = 1,max_yr
!ccccc             IF(worl == 0) then
!ccccc              worc = 0.0
!ccccc              IF(annoil(m) > 0.0) worc = watprod(m)/annoil(m)
!ccccc              if (worc > 19.0) then
!ccccc                do m2 = m,max_yr
!ccccc                 annoil(m2)=0.0
!ccccc                 watprod(m2)=0.0
!ccccc                 worl = 1
!ccccc                end do
!ccccc              end if
!ccccc             END if
!ccccc            end do

1401      format (a11,3x,a11,3x,i10,13(3x,f10.3),<max_yr-1>(3x,f10.3))

1          format (a18,2x,a11)
2          format (a18,2x,f10.3)
3          format (a18,<max_yr-1>(2x,i10))
4          format (a18,<max_yr-1>(2x,f10.3))
5          format (a18,2x,i2)

! create additional project for waterflood

        do itech = 1,max_tech
           aprov(ires)         = aprov(jres)
           AREGION(ires)       = AREGION(jres)
           APADD(ires)         = APADD(jres)
           ACRDTYPE(ires)      = ACRDTYPE(jres)
           APCODE(ires)        = 2
           APLAY_CDE(ires)     = APLAY_CDE(jres)
           ARESFLAG(ires)      = ARESFLAG(jres)
           asc(ires)           = asc(jres)
           AState(ires)        = AState(jres)
           ARES_TYPE(ires)     = ARES_TYPE(jres)
           aresid(ires)        = resid(1:9)//'02'
           apatsiz(ires)       = wlspc
           adepth(ires)        = DEPTH_DEC
           aphi(ires)          = POR_DEC
           apay(ires)          = PAY_DEC
           aperm(ires)         = PERM_DEC
           aapi(ires)          = apigrav
           aoilvis(ires)       = visc
           avdp(ires)          = VDP_DEC
           aswi(ires)          = swi
           asor(ires)          = sorw
           asoi(ires)          = SOI_DEC
           aboi(ires)          = BOI_DEC
           atotpat(ires,itech) = numwell
           aorgooip(ires)      = 0.0
           if (maxpat .ne. 0.0) &
             aorgooip(ires)    = ooipc/maxpat
           agas_grav(ires)     = sgg
           atemp(ires)         = temp
           apresin(ires)       = pres
           ARRC(ires)          = ARRC(jres)
           ABTU(ires)          = ABTU(jres)
           ANWELLOIL(ires)     = ANWELLOIL(jres)
           ANWELLGAS(ires)     = ANWELLGAS(jres)
           ATOTINJ(ires,itech) = Atotinj(jres,itech)
           ALATNUM(ires,itech) = ALATNUM(jres,itech)
           ALATLEN(ires,itech) = ALATLEN(jres,itech)
           ATOTPROD(ires,itech)= ATOTPROD(jres,itech)
           AHEATVAL(ires)      = AHEATVAL(jres)
           ASOC(ires)          = SOC_DEC
           AGOR(ires)          = AGOR(jres)
           AWOR(ires)          = AWOR(jres)
           ANGL(ires)          = ANGL(jres)
           ANGPLET(ires)       = ANGPLET(jres)
           ANGPLPR(ires)       = ANGPLPR(jres)
           ANGPLBU(ires)       = ANGPLBU(jres)
           ANGPLIS(ires)       = ANGPLIS(jres)
           ANGPLPP(ires)       = ANGPLPP(jres)
           ACO2CONT(ires)      = ACO2CONT(jres)
           AN2CONT(ires)       = AN2CONT(jres)
           AH2SCONT(ires)      = AH2SCONT(jres)
           ASULFOIL(ires)      = ASULFOIL(jres)
           AOOIP(ires)         = AOOIP(jres)
           ACHOILPROD(ires)    = ACHOILPROD(jres)
           ACHGASPROD(ires)    = ACHGASPROD(jres)
           ABO(ires)           = boc
           ATOTCONV(ires,itech)      = ATOTCONV(jres,itech)
           ATOTPS(ires,itech)        = ATOTPS(jres,itech)
           AOGIP(ires)         = AOGIP(jres)
           ATOTACRES(ires)     = ATOTACRES(jres)
           ASGI(ires)          = ASGI(jres)
           aresacc(ires)       = aresacc(jres)

           do iy = 1,max_yr
              APRODOIL(ires,iy,itech)= annoil(iy)
            IF(agor(ires) > 0.0) APRODGAS(ires,iy,itech) &
                = annoil(iy) * agor(ires)/1000.0
              APRODNGL(ires,iy,itech)= 0.0
              APRODWAT(ires,iy,itech)= watprod(iy)
              aprodco2(ires,iy,itech)= 0.0
              AWATINJ(ires,iy,itech) = watinj(iy)
              AINJINJ(ires,iy,itech) = 0.0
              AINJRECY(ires,iy,itech)= 0.0
           end do

           do iy=1,max_yr+10
              PATDEV(ires,iy,itech) = patdev(jres,iy,itech)
           end do
        end do

        end subroutine

!**************************************************************************************************
!**************************************************************************************************
!**************************************************************************************************
!**************************************************************************************************

       SUBROUTINE CALBO(TEMP,APIX,P,SGG,BO)
! USE STANDING CORRELATION TO CALCULATE BO
! STANDING CORRELATION:
! Bo=0.972+0.000147*F^1.175 where F=Rs*(Rg/Ro)^0.5+1.25*T
      IF(TEMP <= 0.0.OR.APIX <= 0.0.OR.P <= 0.0.OR.SGG <= 0.) THEN
        BO=-1
        GO TO 10
        ELSE
         YG=0.00091*TEMP-0.0125*APIX
         RS=SGG*(P/(18*(10**YG)))**1.204
         YOIL=141.5/(131.5+APIX)
         F=RS*(SGG/YOIL)**0.5+1.25*TEMP
         BO=0.972+0.000147*(F**1.175)
        END IF
10      RETURN
        END SUBROUTINE

!**************************************************************************************************
!**************************************************************************************************
!**************************************************************************************************
!**************************************************************************************************

        SUBROUTINE CREATE_PROJECT(IRES,JRES)

!  THIS SUBROUTINE CREATES DUPLICATE PROJECTS.  THE PRIMARY USE IS THE EVALUATION OF THE INDUSTRIAL
!  SOURCES OF CO2.

        implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include 'ogsmugr'
        INCLUDE 'ogsml48'

        INTEGER ires                                    !newly created CO2 FLOOD project number
        INTEGER jres                                    !source project number - this data is used to create the DUPLICATE CO2 FLOOD
        INTEGER itech
        INTEGER iy,m,m2,worl

        do itech = 1,max_tech
           aprov(ires)         = aprov(jres)
           AREGION(ires)       = AREGION(jres)
           APADD(ires)         = APADD(jres)
           ACRDTYPE(ires)      = ACRDTYPE(jres)
           APCODE(ires)        = APCODE(Jres)
           APLAY_CDE(ires)     = APLAY_CDE(jres)
           ARESFLAG(ires)      = ARESFLAG(jres)
           asc(ires)           = asc(jres)
           AState(ires)        = AState(jres)
           ARES_TYPE(ires)     = ARES_TYPE(jres)
           aresid(ires)        = ARESID(Jres)
           apatsiz(ires)       = apatsiz(Jres)
           adepth(ires)        = adepth(Jres)
           aphi(ires)          = aphi(Jres)
           apay(ires)          = apay(Jres)
           aperm(ires)         = aperm(Jres)
           aapi(ires)          = aapi(Jres)
           aoilvis(ires)       = aoilvis(Jres)
           avdp(ires)          = avdp(Jres)
           aswi(ires)          = aswi(Jres)
           asor(ires)          = asor(Jres)
           asoi(ires)          = asoi(Jres)
           aboi(ires)          = aboi(Jres)
           atotpat(ires,itech)       = atotpat(JRES,itech)
           aorgooip(ires)      = aorgooip(Jres)
           agas_grav(ires)     = agas_grav(Jres)
           atemp(ires)         = atemp(Jres)
           apresin(ires)       = apresin(Jres)
           ARRC(ires)          = ARRC(jres)
           ABTU(ires)          = ABTU(jres)
           ANWELLOIL(ires)     = ANWELLOIL(jres)
           ANWELLGAS(ires)     = ANWELLGAS(jres)
           ATOTINJ(ires,itech)       = Atotinj(jres,itech)
           ALATNUM(ires,itech)       = ALATNUM(jres,itech)
           ALATLEN(ires,itech)       = ALATLEN(jres,itech)
           ATOTPROD(ires,itech)      = ATOTPROD(jres,itech)
           AHEATVAL(ires)      = AHEATVAL(jres)
           ASOC(ires)          = ASOC(Jres)
           AGOR(ires)          = AGOR(jres)
           AWOR(ires)          = AWOR(jres)
           ANGL(ires)          = ANGL(jres)
           ANGPLET(ires)       = ANGPLET(jres)
           ANGPLPR(ires)       = ANGPLPR(jres)
           ANGPLBU(ires)       = ANGPLBU(jres)
           ANGPLIS(ires)       = ANGPLIS(jres)
           ANGPLPP(ires)       = ANGPLPP(jres)
           ACO2CONT(ires)      = ACO2CONT(jres)
           AN2CONT(ires)       = AN2CONT(jres)
           AH2SCONT(ires)      = AH2SCONT(jres)
           ASULFOIL(ires)      = ASULFOIL(jres)
           AOOIP(ires)         = AOOIP(jres)
           ACHOILPROD(ires)    = ACHOILPROD(jres)
           ACHGASPROD(ires)    = ACHGASPROD(jres)
           ABO(ires)           = ABO(Jres)
           ATOTCONV(ires,itech)      = ATOTCONV(jres,itech)
           ATOTPS(ires,itech)        = ATOTPS(jres,itech)
           AOGIP(ires)         = AOGIP(jres)
           ATOTACRES(ires)     = ATOTACRES(jres)
           ASGI(ires)          = ASGI(jres)
           aresacc(ires)       = aresacc(jres)

           do iy = 1,max_yr
              APRODOIL(ires,iy,itech)=APRODOIL(JRES,iy,itech)
              APRODGAS(ires,iy,itech)=APRODGAS(JRES,iy,itech)
              APRODNGL(ires,iy,itech)=APRODNGL(JRES,iy,itech)
              APRODWAT(ires,iy,itech)=APRODWAT(JRES,iy,itech)
              aprodco2(ires,iy,itech)=aprodco2(JRES,iy,itech)
              AWATINJ(ires,iy,itech) =AWATINJ(JRES,iy,itech)
              AINJINJ(ires,iy,itech) =AINJINJ(JRES,iy,itech)
              AINJRECY(ires,iy,itech)=AINJRECY(JRES,iy,itech)
           end do

           do iy=1,max_yr*2
              PATDEV(ires,iy,itech) = patdev(jres,iy,itech)
           end do
        end do


        END SUBROUTINE


!***************************************************************
!from DECLINE_GAS.FOR
       SUBROUTINE GAS_DECLINE(ires)

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include 'ogsmugr'
        include 'ogsml48'
        include 'ogsmbfw'

        INTEGER ires,iy,ir,ii,irrc,dist,iprov
        CHARACTER*2 st1

!       this subroutine calculates the economics for the gas reservoir undergoing decline curve analysis

        call economics(ires,1,1,0,0,aregion(ires))

        read (aresid(ires)(3:4),'(a2)')   st1
        iRRC = int(ARRC(ires))
        IR = aregion(ires)
        iprov = aprov(ires)
        call dist_match(st1,irrc,IR,dist)

        IF(apcode(ires) >= 11.and.apcode(ires) <= 15) then

               timed(ires) = .true.
               timedyr(ires) = 1
               eligible(ires) = .false.

               call remove_constraints(ires,1)

               IF(reg_adjoil(IR) > 0.0) elyroil(ires) = elyroil(ires) * reg_adjoil(IR)

              IF(apcode(ires) == 11 .OR. apcode(ires) == 12) then
                 IF(reg_adjcnv(IR) > 0.0) elyrgas(ires) = elyrgas(ires) * reg_adjcnv(IR)
                 IF(dist_adjgas(dist,1) >= 0.0) elyrgas(ires) = elyrgas(ires) * dist_adjgas(dist,1)
                 IF(dist_adjoil(dist,1) >= 0.0) elyroil(ires) = elyroil(ires) * dist_adjoil(dist,1)
              ELSEIF(apcode(ires) == 13) then
                 IF(reg_adjtht(IR) > 0.0) elyrgas(ires) = elyrgas(ires) * reg_adjtht(IR)
                 IF(dist_adjgas(dist,2) >= 0.0) elyrgas(ires) = elyrgas(ires) * dist_adjgas(dist,2)
                 IF(dist_adjoil(dist,2) >= 0.0) elyroil(ires) = elyroil(ires) * dist_adjoil(dist,2)
              ELSEIF(apcode(ires) == 14.or.apcode(ires) == 15) then
                 IF(aresflag(ires) == 1 .OR. aresflag(ires) == 2) THEN
                    IF(reg_adjcbm(IR) > 0.0) elyrgas(ires)=elyrgas(ires) * reg_adjcbm(IR)
                    IF(dist_adjgas(dist,4) >= 0.0) elyrgas(ires) = elyrgas(ires) * dist_adjgas(dist,4)
                    IF(dist_adjoil(dist,1) >= 0.0) elyroil(ires) = elyroil(ires) * dist_adjoil(dist,1)
                 ELSEIF(aresflag(ires) == 3 .OR. aresflag(ires) == 4) THEN
                    IF(reg_adjshl(IR) > 0.0) elyrgas(ires)=elyrgas(ires) * reg_adjshl(IR)
                    IF(dist_adjgas(dist,3) >= 0.0) elyrgas(ires) = elyrgas(ires) * dist_adjgas(dist,3)
                    IF(dist_adjoil(dist,2) >= 0.0) elyroil(ires) = elyroil(ires) * dist_adjoil(dist,2)
                 ENDIF
              ENDIF


              DO IY=1,MAX_YR

                 IF(reg_adjoil(ir) > 0.0) EPRODOIL(ires,iy) = EPRODOIL(ires,iy) * reg_adjoil(ir)
                 IF(reg_adjoil(ir) > 0.0) EREMRES(ires,iy)  = eremres(ires,iy)  * reg_adjoil(ir)

                 IF(APCODE(IRES) == 11 .OR. APCODE(IRES) == 12) THEN
                    IF(REG_ADJCNV(IR) > 0.0) EPRODGAS(IRES,IY) = EPRODGAS(IRES,IY) *REG_ADJCNV(IR)
                    IF(REG_ADJCNV(IR) > 0.0) Egremres(IRES,IY) = Egremres(IRES,IY) *REG_ADJCNV(IR)
                    IF(dist_adjgas(dist,1) >= 0.0) EPRODGAS(ires,iy) = EPRODGAS(ires,iy) * dist_adjgas(dist,1)
                    IF(dist_adjoil(dist,1) >= 0.0) EPRODOIL(ires,iy) = EPRODOIL(ires,iy) * dist_adjoil(dist,1)
                 ELSEIF(APCODE(IRES) == 13) THEN
                    IF(REG_ADJTHT(IR) > 0.0)EPRODGAS(IRES,IY) =  EPRODGAS(IRES,IY) *  REG_ADJTHT(IR)
                    IF(REG_ADJTHT(IR) > 0.0)Egremres(IRES,IY) =  Egremres(IRES,IY) *  REG_ADJTHT(IR)
                    IF(dist_adjgas(dist,2) >= 0.0) EPRODGAS(ires,iy) = EPRODGAS(ires,iy) * dist_adjgas(dist,2)
                    IF(dist_adjoil(dist,2) >= 0.0) EPRODOIL(ires,iy) = EPRODOIL(ires,iy) * dist_adjoil(dist,2)
                 ELSEIF(APCODE(IRES) == 14.OR.APCODE(IRES) == 15) THEN
                     IF(ARESFLAG(IRES) == 1 .OR. ARESFLAG(IRES) == 2) THEN
                        IF(REG_ADJCBM(IR) > 0.0)EPRODGAS(IRES,IY) =  EPRODGAS(IRES,IY) *REG_ADJCBM(IR)
                        IF(REG_ADJCBM(IR) > 0.0)Egremres(IRES,IY) =  Egremres(IRES,IY) *REG_ADJCBM(IR)
                        IF(dist_adjgas(dist,4) >= 0.0) EPRODGAS(ires,iy) = EPRODGAS(ires,iy) * dist_adjgas(dist,4)
                        IF(dist_adjoil(dist,1) >= 0.0) EPRODOIL(ires,iy) = EPRODOIL(ires,iy) * dist_adjoil(dist,1)
                     ELSEIF(ARESFLAG(IRES) == 3 .OR. ARESFLAG(IRES) == 4) THEN
                        IF(REG_ADJSHL(IR) > 0.0) EPRODGAS(IRES,IY) =  EPRODGAS(IRES,IY) *REG_ADJSHL(IR)
                        IF(REG_ADJSHL(IR) > 0.0) Egremres(IRES,IY) =  Egremres(IRES,IY) *REG_ADJSHL(IR)
                        IF(dist_adjgas(dist,3) >= 0.0) EPRODGAS(ires,iy) = EPRODGAS(ires,iy) * dist_adjgas(dist,3)
                        IF(dist_adjoil(dist,2) >= 0.0) EPRODOIL(ires,iy) = EPRODOIL(ires,iy) * dist_adjoil(dist,2)
                     ENDIF
                  ENDIF

               END DO

               call write_output(ires,1)                                   !write the project to the output file
               CALL AGG_OGSM(ires,0)                                  !aggregate the economic projects

               dprodgas(ires,:) = eprodgas(ires,:)
               dprodoil(ires,:) = eprodoil(ires,:)
               dregion(ires) = aregion(ires)
               dpadd(ires) = apadd(ires)
               dcrdtype(ires) = acrdtype(ires)
               dngl(ires) = angl(ires)
               dngplet(ires) = angplet(ires)
               dngplpr(ires) = angplpr(ires)
               dngplbu(ires) = angplbu(ires)
               dngplis(ires) = angplis(ires)
               dngplpp(ires) = angplpp(ires)
               dplay_cde(ires) = aplay_cde(ires)
               drrc(ires) = arrc(ires)
               dresflag(ires) = aresflag(ires)
               dresid(ires) = aresid(ires)
               dprov(ires) = aprov(ires)
               dresacc(ires) = aresacc(ires)
               ddepth(ires) = adepth(ires)
               DTIMED(IRES)   = TIMED(IRES)

               viable(apcode(ires)+1,1) = viable(apcode(ires)+1,1)+1   !store the number of economically viable reservoirs

        END if

       end subroutine
!***************************************************************
!from READ_OPTIONS.FOR
       SUBROUTINE READ_OPTIONS

!  read the options file

! this file contains run options for the economic module
!    control for printing economic proformas for each project (wrtpro)
!    control for printing all debug files (wrtdbg)
!    number of years before and after economic life is reached in which
!      EOR/ASR processes can be considered for reserves growth
!    number of cells in an exploration drilling package (nwell)
       implicit none

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
       include 'ogsmugr'
       include 'ogsml48'

      LOGICAL  CONVRESP                          ! FUNCTION TO CONVERT RESPONSE TO LOGICAL

      LOW48=IFILE1

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)runtype              ! Runtype

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NWELL                ! NO. EXPLORATION WELLS IN DRILL PACKAGE

      call ogsm_nextdata(LOW48)         ! NUMBER OF PACKAGES DEVELOPED PER PLAY PER YEAR
      read (LOW48,*) numpack

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)PSHUT                ! NO. YRS BEFORE ECONOMIC LIFE OF EOR

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NSHUT                ! NO. YRS AFTER ECONOMIC LIFE FOR EOR

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)ASR_ST               ! NO. YRS BEFORE ECONOMIC LIFE OF ASR

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)ASR_ED               ! NO. YRS AFTER ECONOMIC LIFE FOR ASR

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)RESP
      opt_dbg2 = CONVRESP(RESP)               ! PRINT ECONOMIC PROFORMA?

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)RESP
      econ_rpt = CONVRESP(RESP)        ! PRINT EXPLORATION PACKAGE OUTPUT?

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)RESP
      OPT_DBG = CONVRESP(RESP)           ! PRINT DEBUG FILES?

      call ogsm_nextdata(LOW48)        !exploration model option
      READ(LOW48,*)EXPL_MODEL

      CALL OGSM_NEXTDATA(LOW48)        !base oil price for normalization
      read (LOW48,*) baseoil
      read (LOW48,*) cutoil

      CALL OGSM_NEXTDATA(LOW48)        !base gas price for normalization
      read (LOW48,*) basegas

      CALL OGSM_NEXTDATA(LOW48)        !ranking criteria for sorting: ROR, NPV, INV(estment efficiency)
      read (LOW48,*) rnkval

      call ogsm_nextdata(LOW48)
      read (LOW48,*) opt_rpt           !print outputs at fuel and well classes?

      call ogsm_nextdata(LOW48)
      read (LOW48,*) L48B4YR           !first year of analysis

      call ogsm_nextdata(LOW48)
      read (LOW48,*) years_study       !number of years of analysis
      years_study = lastyr-l48b4yr+1

      call ogsm_nextdata(LOW48)
      read (LOW48,*) ex_prof           !control for exploration probability report

      call ogsm_nextdata(LOW48)
      read (LOW48,*) resp
      econ_rpt = convresp(resp)

      call ogsm_nextdata(LOW48)
      read (LOW48,*) maxwell

      call ogsm_nextdata(LOW48)
      read (LOW48,*) disclag

      call ogsm_nextdata(LOW48)
      read (LOW48,*) co2lag

      call ogsm_nextdata(LOW48)
      read (LOW48,*) veorcp

      call ogsm_nextdata(LOW48)
      read (LOW48,*) resp

      call ogsm_nextdata(LOW48)
      READ (LOW48,*) SCALE_DAY


       RETURN
       END SUBROUTINE
!***************************************************************
!from READ_SCENARIO.FOR
       SUBROUTINE READ_SCENARIO

!  read the scenario file

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsml48'
       include 'ogsmout'

       INTEGER impct
       INTEGER ntech,nproc,nreg,nst,jmc
       INTEGER idum
       CHARACTER*15 dum15
       CHARACTER*1 temp1
       CHARACTER*1 trimtemp1
       LOGICAL convresp
       CHARACTER*1 temp2(max_impct)
       CHARACTER*10 temp3(max_impct)
       INTEGER      temp4(max_impct)
       REAL         temp5(max_impct)

       LOW48 = IFILE1

!  READ THE NUMBER OF TECHNOLOGY IMPACTS IN THE CASE
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) ntech
       impacts = ntech

       call ogsm_nextdata(LOW48)

!  READ THE PROCESS FILTERS
       do nproc = 1,max_proc
       read (LOW48,11) temp1,(temp2(impct),impct=1,ntech)
          process_case(nproc,1) = convresp(temp1)
          do impct = 1,max_impct
             process_filter(nproc,impct) = process_case(nproc,1)
          end do
          do impct = 1,ntech
             process_filter(nproc,impct) = convresp(temp2(impct))
          end do
       END do

       read (LOW48,11) temp1,(temp2(impct),impct=1,ntech)
       oil_case(1) = convresp(temp1)
          do impct = 1,max_impct
             oil_filter(impct) = oil_case(1)
          end do
          do impct = 1,ntech
             oil_filter(impct) = convresp(temp2(impct))
          end do

       read (LOW48,11) temp1,(temp2(impct),impct=1,ntech)
!      write(OGBUG1,11) temp1,(temp2(impct),impct=1,ntech)
       gas_case(1) = convresp(temp1)
          do impct = 1,max_impct
             gas_filter(impct) = gas_case(1)
          end do
          do impct = 1,ntech
             gas_filter(impct) = convresp(temp2(impct))
          end do


!  READ THE PARAMETER FILTERS
       read (LOW48,13) min_depth_case(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
         min_depth(impct) = min_depth_case(1)
       end do
       do impct = 1,ntech
          min_depth(impct) = temp5(impct)
       end do

       read (LOW48,13) max_depth_case(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
         max_depth(impct) = max_depth_case(1)
       end do
       do impct = 1,ntech
          max_depth(impct) = temp5(impct)
       end do

       read (LOW48,13) min_api_case(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
         min_api(impct) = min_api_case(1)
       end do
       do impct = 1,ntech
          min_api(impct) = temp5(impct)
       end do

       read (LOW48,13) max_api_case(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
         max_api(impct) = max_api_case(1)
       end do
       do impct = 1,ntech
          max_api(impct) = temp5(impct)
       end do

       read (LOW48,13) min_perm_Case(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
         min_perm(impct) = min_perm_case(1)
       end do
       do impct = 1,ntech
          min_perm(impct) = temp5(impct)
       end do

       read (LOW48,13) max_perm_case(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
         max_perm(impct) = max_perm_case(1)
       end do
       do impct = 1,ntech
          max_perm(impct) = temp5(impct)
       end do

       read (LOW48,13) min_rate_case(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
         min_rate(impct) = min_rate_case(1)
       end do
       do impct = 1,ntech
          min_rate(impct) = temp5(impct)
       end do

       read (LOW48,13) max_rate_case(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
         max_rate(impct) = max_rate_case(1)
       end do
       do impct = 1,ntech
          max_rate(impct) = temp5(impct)
       end do

!  READ THE REGION FILTERS
       do nreg = 1, max_reg-1        
         read (LOW48,11) temp1,(temp2(impct),impct=1,ntech)
         region_case(nreg,1) = convresp(temp1)
         do impct = 1,max_impct
            region_filter(nreg,impct) = region_case(nreg,1)
         end do
         do impct = 1,ntech
            region_filter(nreg,impct) = convresp(temp2(impct))
         end do
       end do


!  READ THE TECHNOLOGY PENETRATION CURVE LEVERS
       read (LOW48,14) firstcom_fac(1),(temp4(impct),impct=1,ntech)

       do impct = 1,max_impct
          firstcom(impct) = firstcom_fac(1)
       end do
       do impct = 1,ntech
          firstcom(impct) = temp4(impct)
       end do

       read (LOW48,14) comyear_fac(1),(temp4(impct),impct=1,ntech)
       do impct = 1,max_impct
          comyear(impct) = comyear_fac(1)
       end do
       do impct = 1,ntech
          comyear(impct) = temp4(impct)
       end do

       read (LOW48,13) mark_pen_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          mark_pen(impct) = mark_pen_fac(1)
       end do
       do impct = 1,ntech
          mark_pen(impct) = temp5(impct)
       end do

       read (LOW48,14) tech_curve_fac(1),(temp4(impct),impct=1,ntech)
       do impct = 1,max_impct
          tech_curve(impct) = tech_curve_fac(1)
       end do
       do impct = 1,ntech
          tech_curve(impct) = temp4(impct)
       end do

       read (LOW48,13) prob_rd_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          prob_rd(impct) = prob_rd_fac(1)
       end do
       do impct = 1,ntech
          prob_rd(impct) = temp5(impct)
       end do

       read (LOW48,13) prob_imp_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          prob_imp(impct) = prob_imp_fac(1)
       end do
       do impct = 1,ntech
          prob_imp(impct) = temp5(impct)
       end do


!  READ THE TECHNOLOGY IMPACT LEVERS
       read (LOW48,20) drill_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          drill_chg(impct) = drill_fac(1)
       end do
       do impct = 1,ntech
          drill_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) explr_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          explr_chg(impct) = explr_fac(1)
       end do
       do impct = 1,ntech
          explr_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) paycont_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          paycont_chg(impct) = paycont_fac(1)
       end do
       do impct = 1,ntech
          paycont_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) skin_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          skin_chg(impct) = skin_fac(1)
       end do
       do impct = 1,ntech
          skin_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) contin_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          contin_chg(impct) = contin_fac(1)
       end do
       do impct = 1,ntech
          contin_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) prod_ind_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          prod_ind_chg(impct) = prod_ind_fac(1)
       end do
       do impct = 1,ntech
          prod_ind_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) rec_eff_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          rec_eff_chg(impct) = rec_eff_fac(1)
       end do
       do impct = 1,ntech
          rec_eff_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) vol_swp_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          vol_swp_chg(impct) = vol_swp_fac(1)
       end do
       do impct = 1,ntech
          vol_swp_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) inj_rate_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          inj_rate_chg(impct) = inj_rate_fac(1)
       end do
       do impct = 1,ntech
          inj_rate_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) mob_rat_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          mob_rat_chg(impct) = mob_rat_fac(1)
       end do
       do impct = 1,ntech
          mob_rat_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) tech01_fac(1),(temp5(impct),impct=1,ntech) 
       do impct = 1,max_impct                                    
          tech01_chg(impct) = tech01_fac(1)                     
       end do                                                  
       do impct = 1,ntech                                     
          tech01_chg(impct) = temp5(impct)                   
       end do                                               

       read (LOW48,20) tech02_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                    
          tech02_chg(impct) = tech02_fac(1)                      
       end do                                                    
       do impct = 1,ntech                                        
          tech02_chg(impct) = temp5(impct)                       
       end do                                                    

       read (LOW48,20) tech03_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                    
          tech03_chg(impct) = tech03_fac(1)                      
       end do                                                   
       do impct = 1,ntech                                      
          tech03_chg(impct) = temp5(impct)                    
       end do                                                

       read (LOW48,20) tech04_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                    
          tech04_chg(impct) = tech04_fac(1)                      
       end do                                                    
       do impct = 1,ntech                                       
          tech04_chg(impct) = temp5(impct)                     
       end do                                                 

       read (LOW48,20) tech05_fac(1),(temp5(impct),impct=1,ntech) 
       do impct = 1,max_impct                                   
          tech05_chg(impct) = tech05_fac(1)                    
       end do                                                  
       do impct = 1,ntech                                      
          tech05_chg(impct) = temp5(impct)                     
       end do                                                  

       read (LOW48,20) co2_rat_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          co2_rat_chg(impct) = co2_rat_fac(1)
       end do
       do impct = 1,ntech
          co2_rat_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) oil_rat_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          oil_rat_chg(impct) = oil_rat_fac(1)
       end do
       do impct = 1,ntech
          oil_rat_chg(impct) = temp5(impct)
       end do

       read (LOW48,20) res_chr_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                     
          res_chr_chg(impct) = res_chr_fac(1)                     
       end do                                                     
       do impct = 1,ntech                                         
          res_chr_chg(impct) = temp5(impct)                       
       end do                                                     

!  READ THE COST CHANGE LEVERS
       read (LOW48,20) chg_drl_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_drl_cst(impct) = chg_drl_fac(1)
       end do
       do impct = 1,ntech
          chg_drl_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_stm_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_stm_cst(impct) = chg_stm_fac(1)
       end do
       do impct = 1,ntech
          chg_stm_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_comp_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_comp_cst(impct) = chg_comp_fac(1)
       end do
       do impct = 1,ntech
          chg_comp_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_fac_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_fac_cst(impct) = chg_fac_fac(1)
       end do
       do impct = 1,ntech
          chg_fac_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_secconv_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_secconv_cst(impct) = chg_secconv_fac(1)
       end do
       do impct = 1,ntech
          chg_secconv_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_injconv_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_injconv_cst(impct) = chg_injconv_fac(1)
       end do
       do impct = 1,ntech
          chg_injconv_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_facupg_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_facupg_cst(impct) = chg_facupg_fac(1)
       end do
       do impct = 1,ntech
          chg_facupg_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_wrk_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_wrk_cst(impct) = chg_wrk_fac(1)
       end do
       do impct = 1,ntech
          chg_wrk_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_prdwat_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_prdwat_cst(impct) = chg_prdwat_fac(1)
       end do
       do impct = 1,ntech
          chg_prdwat_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_chmpnt_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_chmpnt_cst(impct) = chg_chmpnt_fac(1)
       end do
       do impct = 1,ntech
          chg_chmpnt_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_plypnt_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_plypnt_cst(impct) = chg_plypnt_fac(1)
       end do
       do impct = 1,ntech
          chg_plypnt_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_co2pnt_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_co2pnt_cst(impct) = chg_co2pnt_fac(1)
       end do
       do impct = 1,ntech
          chg_co2pnt_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_stmgen_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_stmgen_cst(impct) = chg_stmgen_fac(1)
       end do
       do impct = 1,ntech
          chg_stmgen_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_foam_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_foam_cst(impct) = chg_foam_fac(1)
       end do
       do impct = 1,ntech
          chg_foam_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_voam_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_voam_cst(impct) = chg_voam_fac(1)
       end do
       do impct = 1,ntech
          chg_voam_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_gna_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_gna_cst(impct) = chg_gna_fac(1)
       end do
       do impct = 1,ntech
          chg_gna_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_annsec_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_annsec_cst(impct) = chg_annsec_fac(1)
       end do
       do impct = 1,ntech
          chg_annsec_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_lft_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_lft_cst(impct) = chg_lft_fac(1)
       end do
       do impct = 1,ntech
          chg_lft_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_secwrk_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_secwrk_cst(impct) = chg_secwrk_fac(1)
       end do
       do impct = 1,ntech
          chg_secwrk_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_injc_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_injc_cst(impct) = chg_injc_fac(1)
       end do
       do impct = 1,ntech
          chg_injc_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_injt_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_injt_cst(impct) = chg_injt_fac(1)
       end do
       do impct = 1,ntech
          chg_injt_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_cmp_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          chg_cmp_cst(impct) = chg_cmp_fac(1)
       end do
       do impct = 1,ntech
          chg_cmp_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) chg_ooil_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                      
          chg_ooil_cst(impct) = chg_ooil_fac(1)                    
       end do                                                      
       do impct = 1,ntech                                          
          chg_ooil_cst(impct) = temp5(impct)                       
       end do                                                      

       read (LOW48,20) chg_ogas_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                      
          chg_ogas_cst(impct) = chg_ogas_fac(1)                    
       end do                                                      
       do impct = 1,ntech                                          
          chg_ogas_cst(impct) = temp5(impct)                       
       end do

       read (LOW48,20) chg_owat_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                      
          chg_owat_cst(impct) = chg_owat_fac(1)                    
       end do                                                      
       do impct = 1,ntech                                          
          chg_owat_cst(impct) = temp5(impct)                       
       end do                                                      

       read (LOW48,20) chg_oinj_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                      
          chg_oinj_cst(impct) = chg_oinj_fac(1)                    
       end do                                                      
       do impct = 1,ntech                                          
          chg_oinj_cst(impct) = temp5(impct)                       
       end do                                                      

!  READ THE COST TO APPLY LEVERS - WELL LEVEL                      
       read (LOW48,20) cst_drl_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_drl_cst(impct) = cst_drl_fac(1)
       end do
       do impct = 1,ntech
          cst_drl_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_stm_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_stm_cst(impct) = cst_stm_fac(1)
       end do
       do impct = 1,ntech
          cst_stm_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_comp_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_comp_cst(impct) = cst_comp_fac(1)
       end do
       do impct = 1,ntech
          cst_comp_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_fac_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_fac_cst(impct) = cst_fac_fac(1)
       end do
       do impct = 1,ntech
          cst_fac_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_secconv_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_secconv_cst(impct) = cst_secconv_fac(1)
       end do
       do impct = 1,ntech
          cst_secconv_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_injconv_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_injconv_cst(impct) = cst_injconv_fac(1)
       end do
       do impct = 1,ntech
          cst_injconv_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_facupg_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_facupg_cst(impct) = cst_facupg_fac(1)
       end do
       do impct = 1,ntech
          cst_facupg_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_wrk_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_wrk_cst(impct) = cst_wrk_fac(1)
       end do
       do impct = 1,ntech
          cst_wrk_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_foam_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_foam_cst(impct) = cst_foam_fac(1)
       end do
       do impct = 1,ntech
          cst_foam_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_voam_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_voam_cst(impct) = cst_voam_fac(1)
       end do
       do impct = 1,ntech
          cst_voam_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_gna_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_gna_cst(impct) = cst_gna_fac(1)
       end do
       do impct = 1,ntech
          cst_gna_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_annsec_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_annsec_cst(impct) = cst_annsec_fac(1)
       end do
       do impct = 1,ntech
          cst_annsec_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_lft_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_lft_cst(impct) = cst_lft_fac(1)
       end do
       do impct = 1,ntech
          cst_lft_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_secwrk_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_secwrk_cst(impct) = cst_secwrk_fac(1)
       end do
       do impct = 1,ntech
          cst_secwrk_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_injc_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_injc_cst(impct) = cst_injc_fac(1)
       end do
       do impct = 1,ntech
          cst_injc_cst(impct) = temp5(impct)
       end do

       read (LOW48,20) cst_cmp_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct
          cst_cmp_cst(impct) = cst_cmp_fac(1)
       end do
       do impct = 1,ntech
          cst_cmp_cst(impct) = temp5(impct)
       end do

!  READ THE COST TO APPLY LEVERS - PROJECT LEVEL                  
       read (LOW48,20) cstp_drl_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                      
          cst_drl_cstp(impct) = cstp_drl_fac(1)                    
       end do                                                      
       do impct = 1,ntech                                          
          cst_drl_cstp(impct) = temp5(impct)                       
       end do                                                      

       read (LOW48,20) cstp_stm_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                      
          cst_stm_cstp(impct) = cstp_stm_fac(1)                    
       end do                                                      
       do impct = 1,ntech                                          
          cst_stm_cstp(impct) = temp5(impct)                       
       end do                                                      

       read (LOW48,20) cstp_comp_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                       
          cst_comp_cstp(impct) = cstp_comp_fac(1)                   
       end do                                                       
       do impct = 1,ntech                                           
          cst_comp_cstp(impct) = temp5(impct)                       
       end do                                                       

       read (LOW48,20) cstp_fac_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                      
          cst_fac_cstp(impct) = cstp_fac_fac(1)                    
       end do                                                      
       do impct = 1,ntech                                          
          cst_fac_cstp(impct) = temp5(impct)                       
       end do                                                      

       read (LOW48,20) cstp_secconv_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                          
          cst_secconv_cstp(impct) = cstp_secconv_fac(1)                
       end do                                                          
       do impct = 1,ntech                                              
          cst_secconv_cstp(impct) = temp5(impct)                       
       end do                                                          

       read (LOW48,20) cstp_injconv_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                          
          cst_injconv_cstp(impct) = cstp_injconv_fac(1)                
       end do                                                          
       do impct = 1,ntech                                              
          cst_injconv_cstp(impct) = temp5(impct)                       
       end do                                                          

       read (LOW48,20) cstp_facupg_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                         
          cst_facupg_cstp(impct) = cstp_facupg_fac(1)                 
       end do                                                         
       do impct = 1,ntech                                             
          cst_facupg_cstp(impct) = temp5(impct)                       
       end do                                                         

       read (LOW48,20) cstp_wrk_fac(1),(temp5(impct),impct=1,ntech)   
       do impct = 1,max_impct                                         
          cst_wrk_cstp(impct) = cstp_wrk_fac(1)                       
       end do                                                         
       do impct = 1,ntech                                             
          cst_wrk_cstp(impct) = temp5(impct)                          
       end do                                                         

       read (LOW48,20) cstp_foam_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                      
          cst_foam_cstp(impct) = cstp_foam_fac(1)                  
       end do                                                      
       do impct = 1,ntech                                          
          cst_foam_cstp(impct) = temp5(impct)                      
       end do                                                      

       read (LOW48,20) cstp_voam_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                       
          cst_voam_cstp(impct) = cstp_voam_fac(1)                   
       end do                                                       
       do impct = 1,ntech                                           
          cst_voam_cstp(impct) = temp5(impct)                       
       end do                                                       

       read (LOW48,20) cstp_gna_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                      
          cst_gna_cstp(impct) = cstp_gna_fac(1)                    
       end do                                                      
       do impct = 1,ntech                                          
          cst_gna_cstp(impct) = temp5(impct)                       
       end do                                                      

       read (LOW48,20) cstp_annsec_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                         
          cst_annsec_cstp(impct) = cstp_annsec_fac(1)                 
       end do                                                         
       do impct = 1,ntech                                             
          cst_annsec_cstp(impct) = temp5(impct)                       
       end do                                                         

       read (LOW48,20) cstp_lft_fac(1),(temp5(impct),impct=1,ntech)  
       do impct = 1,max_impct                                       
          cst_lft_cstp(impct) = cstp_lft_fac(1)                    
       end do                                                      
       do impct = 1,ntech                                         
          cst_lft_cstp(impct) = temp5(impct)                     
       end do                                                   

       read (LOW48,20) cstp_secwrk_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                         
          cst_secwrk_cstp(impct) = cstp_secwrk_fac(1)                 
       end do                                                         
       do impct = 1,ntech                                             
          cst_secwrk_cstp(impct) = temp5(impct)                       
       end do                                                         

       read (LOW48,20) cstp_injc_fac(1),(temp5(impct),impct=1,ntech) 
       do impct = 1,max_impct                                       
          cst_injc_cstp(impct) = cstp_injc_fac(1)                   
       end do                                                       
       do impct = 1,ntech                                           
          cst_injc_cstp(impct) = temp5(impct)                       
       end do                                                       

       read (LOW48,20) cstp_cmp_fac(1),(temp5(impct),impct=1,ntech)
       do impct = 1,max_impct                                      
          cst_cmp_cstp(impct) = cstp_cmp_fac(1)                    
       end do                                                      
       do impct = 1,ntech                                          
          cst_cmp_cstp(impct) = temp5(impct)                       
       end do                                                      


       CALL OGSM_NEXTDATA(LOW48)                                   

!  read the technology lever equation variables
        read(low48,*) (techa_var(jmc),jmc=1,5)                     
        read(low48,*) (techb_var(jmc),jmc=1,5)                     
        read(low48,*) (techc_var(jmc),jmc=1,5)                     
        read(low48,*) (techd_var(jmc),jmc=1,5)                     
        read(low48,*) (teche_var(jmc),jmc=1,5)                     
        read(low48,*) (techf_var(jmc),jmc=1,5)                     
        read(low48,*) (techg_var(jmc),jmc=1,5)                     
        read(low48,*) (techh_var(jmc),jmc=1,5)                     
        read(low48,*) (tech1_var(jmc),jmc=1,5)                     
        read(low48,*) (tech2_var(jmc),jmc=1,5)                     
        read(low48,*) (tech3_var(jmc),jmc=1,5)                     
        read(low48,*) (tech4_var(jmc),jmc=1,5)                     
        read(low48,*) (tech5_var(jmc),jmc=1,5)                     
        read(low48,*) (tech6_var(jmc),jmc=1,5)                     
        read(low48,*) (tech7_var(jmc),jmc=1,5)                     
        read(low48,*) (tech8_var(jmc),jmc=1,5)                     
        read(low48,*) (tech9_var(jmc),jmc=1,5)                     
        read(low48,*) (tech10_var(jmc),jmc=1,5)                    
        read(low48,*) (tech11_var(jmc),jmc=1,5)                    
        read(low48,*) (tech12_var(jmc),jmc=1,5)                    
        read(low48,*) (tech13_var(jmc),jmc=1,5)                    
        read(low48,*) (tech14_var(jmc),jmc=1,5)                    
        read(low48,*) (tech15_var(jmc),jmc=1,5)                    
        read(low48,*) (tech16_var(jmc),jmc=1,5)                    
        read(low48,*) (tech17_var(jmc),jmc=1,5)                    
        read(low48,*) (tech18_var(jmc),jmc=1,5)                   
        read(low48,*) (tech19_var(jmc),jmc=1,5)                  
        read(low48,*) (tech20_var(jmc),jmc=1,5)                 
        read(low48,*) (tech21_var(jmc),jmc=1,5)                
        read(low48,*) (tech22_var(jmc),jmc=1,5)               
        read(low48,*) (tech23_var(jmc),jmc=1,5)              
        read(low48,*) (tech24_var(jmc),jmc=1,5)             
        read(low48,*) (tech25_var(jmc),jmc=1,5)            
        read(low48,*) (tech26_var(jmc),jmc=1,5)           
        read(low48,*) (tech27_var(jmc),jmc=1,5)          
        read(low48,*) (tech28_var(jmc),jmc=1,5)         
        read(low48,*) (tech29_var(jmc),jmc=1,5)        
        read(low48,*) (tech30_var(jmc),jmc=1,5)       
        read(low48,*) (tech31_var(jmc),jmc=1,5)      
        read(low48,*) (tech32_var(jmc),jmc=1,5)     
        read(low48,*) (tech33_var(jmc),jmc=1,5)    

!  read the simple technology assumptions on costs and production
       CALL OGSM_NEXTDATA(LOW48)
       DO K = 1,6   ! FUEL CATEGORY
         READ(LOW48,*) idum, dum15, (tOGTECHON(I,K),I=1,5)
         if (ogrunop(2) == 23) tOGTECHON(4,K) = tOGTECHON(3,K)
         DO I = 1,3
           OGTECHON(I,K,:) = tOGTECHON(I,K)
         ENDDO
       ENDDO


 1     format (89x,9x,a1)
 2     format (89x,i10)
 3     format (89x,f10.1)
 4     format (89x,f10.2)
 5     format (i3,3x,l1)
 6     format (a3,3x,l1)
 11     format (54x,<max_impct>(2x,a9))
 12     format (54x,<max_impct>(f10.2,1x))
 13     format (54x,<max_impct>(f10.3,1x))
 14     format (54x,1x,<max_impct>(1x,i10))
 15     format (i3,2x,<max_impct>(3x,l1,7x))
 16     format (a3,2x,<max_impct>(3x,l1,7x))
 17     format (<max_impct>(f10.1,1x))
 18     format (<max_impct>(f10.3,1x))
 19     format (<max_impct>(i10,1x))
 20     format (54x,<max_impct>(f10.3,1x))

       RETURN
       END SUBROUTINE
!***************************************************************
!from READ_COST.FOR
       SUBROUTINE READ_COST

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsml48'


!  read the cost file

      LOGICAL CONVRESP                           ! FUNCTION TO CONVERT RESPONSE TO LOGICAL
      INTEGER D                                  ! REGION/DEPTH INDEX VARIABLES
      INTEGER SRC                                ! CO2 SOURCE INDEX VARIABLE
      INTEGER PRC                                ! NO. PROCESSES INDEX VARIABLE
      INTEGER YR                                 ! YEAR INDEX VARIABLE
      INTEGER YEAR                               ! HISTORY YEARS
      INTEGER nreg                               ! total number of cost regions
      INTEGER num_proc                           ! number of processes - for discount rates
      INTEGER num_year                           ! number of years of data for CPI indices

      CHARACTER*2 REG                            ! TEMP TO READ REGION
      CHARACTER*3 RESP1,RESP2                    ! READ TEMP TO CONVERT TO LOGICAL

      LOW48 = IFILE1

!  Sections:
!    1. Resource/Process Independent Capital Costs
!    2. Resource/Process Specific Capital Costs
!    3. Fixed Operating Costs
!    4. Variable Operating Costs
!    5. Other Cost Parameters
!    6. Process Discount Rates
!    7. CPI indices
!    8. Oil & Gas Sale Controls

!*****************************************************************************
!
!
!  Section 1: Resource/Process Independent Capital Costs
!
!     A. Vertical Drilling and Completion for Oil
!     B. Horizontal Drilling and Completion for Oil
!     C. Cost to Equip a Primary Producer for Oil
!     D. Vertical Drilling and Completion for Gas
!     E. Dry Hole
!     F. Workover Costs
!     G. Exploratory Drilling Cost Multipliers
!
!      NOTE: THESE ARE REGIONAL FUNCTIONS - COST(K$/WELL) = F(REGION,DEPTH,..)
!*****************************************************************************
!
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NREG                           ! NO. REGIONS
!!!VERTICAL DRILLING AND COMPLETION FOR OIL
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OIL_DWCM(R,1),OIL_DWCD(R,1),OIL_DWCK(R,1), &
         OIL_DWCA(R,1), &
             OIL_DWCB(R,1),OIL_DWCC(R,1)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OIL_DWCM(R,2),OIL_DWCD(R,2),OIL_DWCK(R,2), &
         OIL_DWCA(R,2), &
             OIL_DWCB(R,2),OIL_DWCC(R,2)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OIL_DWCM(R,3),OIL_DWCD(R,3),OIL_DWCK(R,3), &
         OIL_DWCA(R,3), &
             OIL_DWCB(R,3),OIL_DWCC(R,3)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OIL_DWCM(R,4),OIL_DWCD(R,4),OIL_DWCK(R,4), &
         OIL_DWCA(R,4), &
             OIL_DWCB(R,4),OIL_DWCC(R,4)
      ENDDO

!!!COST TO EQUIP A PRIMARY PRODUCER
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,NPRM(R,1),MPRD(R,1),NPRK(R,1),NPRA(R,1), &
             NPRB(R,1),NPRC(R,1)
        do d = 2,4
           nprm(r,d) = nprm(r,1)
           mprd(r,d) = mprd(r,1)
           nprk(r,d) = nprk(r,1)
           npra(r,d) = npra(r,1)
           nprb(r,d) = nprb(r,1)
           nprc(r,d) = nprc(r,1)
        end do
      ENDDO

!!!VERTICAL DRILLING AND COMPLETION FOR GAS
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,GAS_DWCM(R,1),GAS_DWCD(R,1),GAS_DWCK(R,1), &
         GAS_DWCA(R,1), &
             GAS_DWCB(R,1),GAS_DWCC(R,1)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,GAS_DWCM(R,2),GAS_DWCD(R,2),GAS_DWCK(R,2), &
         GAS_DWCA(R,2), &
             GAS_DWCB(R,2),GAS_DWCC(R,2)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,GAS_DWCM(R,3),GAS_DWCD(R,3),GAS_DWCK(R,3), &
         GAS_DWCA(R,3), &
             GAS_DWCB(R,3),GAS_DWCC(R,3)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,GAS_DWCM(R,4),GAS_DWCD(R,4),GAS_DWCK(R,4), &
         GAS_DWCA(R,4), &
             GAS_DWCB(R,4),GAS_DWCC(R,4)
      ENDDO

 !!!DRY HOLE
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,DRY_DWCM(R,1),DRY_DWCD(R,1),DRY_DWCK(R,1), &
         DRY_DWCA(R,1), &
             DRY_DWCB(R,1),DRY_DWCC(R,1)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,DRY_DWCM(R,2),DRY_DWCD(R,2),DRY_DWCK(R,2), &
         DRY_DWCA(R,2), &
             DRY_DWCB(R,2),DRY_DWCC(R,2)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,DRY_DWCM(R,3),DRY_DWCD(R,3),DRY_DWCK(R,3), &
         DRY_DWCA(R,3), &
             DRY_DWCB(R,3),DRY_DWCC(R,3)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,DRY_DWCM(R,4),DRY_DWCD(R,4),DRY_DWCK(R,4), &
         DRY_DWCA(R,4), &
             DRY_DWCB(R,4),DRY_DWCC(R,4)
      ENDDO

! -------- (NEW) ---------- (JULY 2013) ----------------------------------------------------

!D.1  (new) Read D&C Cost Coefficients into 
!       new array DNCC_COEF(3 well types, 7 regions and 5 coefficients)
!       use region 1 values for region 1a and region 5 values for northern great plains region

      CALL OGSM_NEXTDATA(LOW48)         !Subroutine to move to next data location 

      READ(LOW48,*)NREG                 ! Number of Regions in Following Tables
 
      CALL OGSM_NEXTDATA(LOW48)          


      DO WELL_COUNTER = 1,WELL_TYPE             !loop through tables by well type (oil, gas, dry)

          DO R = 1,NREG                         ! loop through oil coefficients by regions

            READ(LOW48,*)REG,DNCC_COEF(WELL_COUNTER,R,1), DNCC_COEF(WELL_COUNTER,R,2), &        
                DNCC_COEF(WELL_COUNTER,R,3), DNCC_COEF(WELL_COUNTER,R,4), &
                DNCC_COEF(WELL_COUNTER,R,5), DNCC_COEF(WELL_COUNTER,R,6)
         if(ogreport(26) == 1) WRITE(9938,*)REG,DNCC_COEF(WELL_COUNTER,R,1), DNCC_COEF(WELL_COUNTER,R,2), &     
                DNCC_COEF(WELL_COUNTER,R,3), DNCC_COEF(WELL_COUNTER,R,4), &
                DNCC_COEF(WELL_COUNTER,R,5), DNCC_COEF(WELL_COUNTER,R,6)
            
            ENDDO
              
          CALL OGSM_NEXTDATA(LOW48)             !Subroutine to move to next data location

      ENDDO

! --------------------------------------------------------------------------------------------



   !! WORKOVER COSTS
!      CALL OGSM_NEXTDATA(LOW48)              -- (NEW) -- Commented out since new code does this call
      DO R = 1,NREG
        READ(LOW48,*)REG,WRKM(R,1),WRKD(R,1),WRKK(R,1),WRKA(R,1), &
             WRKB(R,1),WRKC(R,1)
        do d = 2,4
           wrkm(r,d) = wrkm(r,1)
           wrkd(r,d) = wrkd(r,1)
           wrkk(r,d) = wrkk(r,1)
           wrka(r,d) = wrka(r,1)
           wrkb(r,d) = wrkb(r,1)
           wrkc(r,d) = wrkc(r,1)
        end do
      ENDDO

!
!  Section 2: Resource/Process Specific Capital Costs
!
!     A. Gas Processing and Treatment Facilities
!     B. Cost to convert a primary to secondary well
!     C. Cost to convert a producer to an injector
!     D. Produced Water Handling Plant
!     E. Water Injection Plant
!     F. Polymer/Chemical Handling Plant
!     G. CO2 recycling/injection plant
!
!     NOTE: A IS A FUNCTION OF THE GAS IMPURITIES
!           B & C ARE FUNCTION - COST(K$/WELL) = F(REGION,DEPTH)
!           D - G ARE FUNCTIONS OF THE MAXIMUM FLOW RATE
!*****************************************************************************

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NREG                           ! NO. REGIONS

 !!!GAS PROCESSING AND TREATMENT FACILITIES
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,max_reg
        READ(LOW48,*)REG,IMP_CAPST(R),IMP_CAPCR(R),IMP_CAPTE(R), &
         IMP_CAPSU(R), &
             IMP_THRU(R),IMP_OP_FAC(R)
      ENDDO

      IMP_H2O_LIM = 1.00


      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)IMP_DIS_RATE,IMP_PLT_LFE,IMP_N2_LIM,IMP_CO2_LIM, &
                   IMP_H2S_LIM,IMP_NGL_LIM

  !! COST TO CONVERT A PRIMARY TO SECONDARY WELL
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,PSWM(R,1),PSWD(R,1),PSWK(R,1),PSWA(R,1), &
             PSWB(R,1),PSWC(R,1)
        do d = 2,4
           pswm(r,d) = pswm(r,1)
           pswd(r,d) = pswd(r,1)
           pswk(r,d) = pswk(r,1)
           pswa(r,d) = pswa(r,1)
           pswb(r,d) = pswb(r,1)
           pswc(r,d) = pswc(r,1)
        end do
      ENDDO

   !! COST TO CONVERT A PRODUCER TO INJECTOR
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,PSIM(R,1),PSID(R,1),PSIK(R,1),PSIA(R,1), &
             PSIB(R,1),PSIC(R,1)
        do d = 2,4
           psim(r,d) = psim(r,1)
           psid(r,d) = psid(r,1)
           psik(r,d) = psik(r,1)
           psia(r,d) = psia(r,1)
           psib(r,d) = psib(r,1)
           psic(r,d) = psic(r,1)
        end do
      ENDDO

   !! FACILITIES UPGRADE COST
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,FACUPM(R,1),FACUPD(R,1),FACUPK(R,1), &
             FACUPA(R,1),FACUPB(R,1),FACUPC(R,1)
        do d = 2,4
           facupm(r,d) = facupm(r,1)
           facupd(r,d) = facupd(r,1)
           facupk(r,d) = facupk(r,1)
           facupa(r,d) = facupa(r,1)
           facupb(r,d) = facupb(r,1)
           facupc(r,d) = facupc(r,1)
        end do
      ENDDO

   !! GAS FACILITIES
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,FACGM(R,1),FACGD(R,1),FACGK(R,1),FACGA(R,1), &
             FACGB(R,1),FACGC(R,1)
        do d = 2,4
           facgm(r,d) = facgm(r,1)
           facgd(r,d) = facgd(r,1)
           facgk(r,d) = facgk(r,1)
           facga(r,d) = facga(r,1)
           facgb(r,d) = facgb(r,1)
           facgc(r,d) = facgc(r,1)
        end do
      ENDDO

  !!! ESTIMATED CAPITAL COST ($M):  PRODUCED WATER HANDLING PLANT
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)PWHP

  !!! CAPITAL COST ($):  CHEMICAL HANDLING PLANT
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)CHMK,CHMA,CHMB

  !!! CAPITAL COST ($M):  POLYMER HANDLING PLANT
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)PLYPK,PLYPA

  !!! CAPITAL COST ($MM):  CO2 RECYCLING/INJECTION PLANT
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)CO2RK,CO2RB

  !!! STEAM MANIFOLDS/PIPELINES
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)STMMA

  !!! STEAM GENERATORS
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)STMGA

  !!! SINGLE STAGE COMPRESSOR ($/BHP)
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)COST_BHP

!
!
!  Section 3: Fixed Operating Costs
!
!     A. Fixed annual cost
!     B. Annual cost for secondary production
!
!     NOTE: B IS AT THE STATE LEVEL FOR SPECIFIED STATES.  REGIONAL FOR OTHERS
!           A IS REGIONAL - COST($/BBL) = F(REGION,DEPTH)
!*****************************************************************************
!
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NREG                           ! NO. REGIONS

!!!FIXED ANNUAL COST FOR OIL ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
         READ(LOW48,*)REG,OMOM(R,1),OMOD(R,1),OMOK(R,1),OMOA(R,1), &
          OMOB(R,1), &
             OMOC(R,1)
        do d = 2,4
           omom(r,d) = omom(r,1)
           omod(r,d) = omod(r,1)
           omok(r,d) = omok(r,1)
           omoa(r,d) = omoa(r,1)
           omob(r,d) = omob(r,1)
           omoc(r,d) = omoc(r,1)
        end do

      ENDDO

!!!FIXED ANNUAL COST FOR GAS ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OMGM(R,1),OMGD(R,1),OMGK(R,1),OMGA(R,1), &
         OMGB(R,1), &
             OMGC(R,1)
        do d = 2,4
           omgm(r,d) = omgm(r,1)
           omgd(r,d) = omgd(r,1)
           omgk(r,d) = omgk(r,1)
           omga(r,d) = omga(r,1)
           omgb(r,d) = omgb(r,1)
           omgc(r,d) = omgc(r,1)
        end do

      ENDDO

!!!ANNUAL COST FOR SECONDARY PRODUCTION ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OPSECM(R,1),OPSECD(R,1),OPSECK(R,1), &
         OPSECA(R,1),OPSECB(R,1), &
             OPSECC(R,1)
        do d = 2,4
           opsecm(r,d) = opsecm(r,1)
           opsecd(r,d) = opsecd(r,1)
           opseck(r,d) = opseck(r,1)
           opseca(r,d) = opseca(r,1)
           opsecb(r,d) = opsecb(r,1)
           opsecc(r,d) = opsecc(r,1)
        end do

      ENDDO
!
!  Section 4: Variable Operating Costs
!     A.  Lifting Cost
!     B.  Gas Processing Cost
!     C.  Injection Cost
!     D.  Injectant Cost
!         polymer
!         CO2
!     E.  Environmental Costs
!
!     NOTE: A & C ARE FUNCTIONS OF DEPTH
!           B IS A FUNCTION OF THE IMPURITIES OF THE GAS
!           D (POLYMER) IS A FIXED VALUE ($/Lbs)
!           D (CO2)     IS A FIXED VALUE DEPENDANT ON CO2 SOURCE AND REGION
!*****************************************************************************

!!! PROCESS SPECIFIC O & M COSTS
       CALL OGSM_NEXTDATA(LOW48)
       READ (LOW48,*) NUM_PROC
       CALL OGSM_NEXTDATA(LOW48)
       DO R = 1,NUM_PROC
         READ (LOW48,*) OIL_OAM(R),GAS_OAM(R),WAT_OAM(R),INJ_OAM(R)
       END DO

!!!LIFTING COST ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OMLM(R,1),OMLD(R,1),OMLK(R,1), &
         OMLA(R,1), &
             OMLB(R,1),OMLC(R,1)
        do d = 2,4
           omlm(r,d) = omlm(r,1)
           omld(r,d) = omld(r,1)
           omlk(r,d) = omlk(r,1)
           omla(r,d) = omla(r,1)
           omlb(r,d) = omlb(r,1)
           omlc(r,d) = omlc(r,1)
        end do

      ENDDO

!!!SECONDARY WORKOVER COST ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OMSWRM(R,1),OMSWRD(R,1),OMSWRK(R,1), &
         OMSWRA(R,1), &
             OMSWRB(R,1),OMSWRC(R,1)
        do d = 2,4
           omswrm(r,d) = omswrm(r,1)
           omswrd(r,d) = omswrd(r,1)
           omswrk(r,d) = omswrk(r,1)
           omswra(r,d) = omswra(r,1)
           omswrb(r,d) = omswrb(r,1)
           omswrc(r,d) = omswrc(r,1)
        end do

      ENDDO

!!!INJECTION COST ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OPINJM(R,1),OPINJD(R,1),OPINJK(R,1), &
         OPINJA(R,1), &
             OPINJB(R,1),OPINJC(R,1)
        do d = 2,4
           opinjm(r,d) = opinjm(r,1)
           opinjd(r,d) = opinjd(r,1)
           opinjk(r,d) = opinjk(r,1)
           opinja(r,d) = opinja(r,1)
           opinjb(r,d) = opinjb(r,1)
           opinjc(r,d) = opinjc(r,1)
        end do

      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)POLY                       ! POLYMER COST ($/LB)

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)CO2K,CO2B                      ! CO2 WEST TEXAS COST ($/BBL)

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NREG                           ! NO. REGIONS

      CALL OGSM_NEXTDATA(LOW48)
      DO R=1,NREG
        READ(LOW48,*)CO2ST(R),CO2PR(R)            ! CO2 NON-WEST TEXAS COST
      ENDDO

!!! CO2 COSTS - NATURAL AND INDUSTRIAL ($/MCF)      
      CALL OGSM_NEXTDATA(LOW48)
      do src=1,max_src                             
         read(low48,*)(cregpr(r,src),r=1,max_reg-1)
      enddo 

 !!!O&M COSTS FOR INJECTION OF CO2 ($/MCF)
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)CO2OM20            ! INJECTION RATE > 20 MMCF
      READ(LOW48,*)CO2OM_20           ! INJECTION RATE < 20 MMCF

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)STIMFAC             ! GAS WELL STIMULATION EFFIFIENCY (PERCENT)
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)STIM_A,STIM_B       ! GAS WELL STIMULATION COST ($MM/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      read (LOW48,*)stim_yr            ! NUMBER OF YEARS BETWEEN STIMULATIONS
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)COMP_VC             ! COMPRESSOR O&M ($/MCF)

      call ogsm_nextdata(LOW48)        ! produced water recylcing cost (steam flood
      read (LOW48,*) recy_wat,recy_oil
!
!
!  Section 5: Other Cost Parameters
!     A.  Lease Acquisition Factor
!     B.  Geological and Geophysical Factors
!     C.  G&A Multipliers
!     D.  Overhead Multipliers

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)LBC_FRAC             ! LEASE BONUS COST FACTOR (PERCENT)
      LBC_FRAC = lbc_frac/100.0
      READ(LOW48,*)PLAC                 ! PERCENT LEASE ACQUISITION COST CAPITALIZED
      PLAC = PLAC /100.0

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)GG_FAC               ! GEOLOGICAL & GEOPHYSICAL FACTORS (PERCENT)
      GG_FAC = gg_fac/100.0
      READ(LOW48,*)PGGC                 ! PERCENT G&G DEPLETED
      PGGC = PGGC /100.0

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)GNA_EXP2             ! G&A EXPENSE MULTIPLIER (PERCENT)
      GNA_EXP2 = gna_exp2/100.0
      READ(LOW48,*)GNA_CAP2             ! G&A CAPITAL MULTIPLIER (PERCENT)
      GNA_CAP2 = gna_cap2/100.0

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)ENV_FAC              ! ENVIRONMENTAL CAPITAL COST MULT (PERCENT)
      ENV_FAC = env_fac/100.0

      READ(LOW48,*)ENVOP_FAC            ! ENVIRONMENTAL OPERATING COST MULT (PERCENT)
      ENVOP_FAC = ENVOP_FAC/100.0

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)TANG_RATE        ! PERCENT DEVELOPMENT WELL COST TANGIBLE
      TANG_RATE = TANG_RATE/100.0
      READ(LOW48,*)TANG_fac_RATE            ! PERCENT FACILITIES COST TANGIBLE
      TANG_fac_RATE =TANG_fac_RATE /100.0

!      PRINT*, tang_rate,tang_fac_rate

      call ogsm_nextdata(LOW48)
      read (LOW48,*) omult_int          ! INTANGIBLE COST MULTIPLIER FACTOR FOR OIL
      read (LOW48,*) omult_tang         ! TANGIBLE COST MULTIPLIER FACTOR FOR OIL
      read (LOW48,*) omult_oam          ! O & M COST MULTIPLIER FACTOR FOR OIL

      call ogsm_nextdata(LOW48)
      read (LOW48,*) gmult_int          ! INTANGIBLE COST MULTIPLIER FACTOR FOR GAS
      read (LOW48,*) gmult_tang         ! TANGIBLE COST MULTIPLIER FACTOR FOR GAS
      read (LOW48,*) gmult_oam          ! O & M COST MULTIPLIER FACTOR FOR GAS
!
!
!*****************************************************************************
!
!
!  Section 6: Process Discount Rates
!*****************************************************************************
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NUM_PROC
      CALL OGSM_NEXTDATA(LOW48)
      DO PRC=1,NUM_PROC
        READ(LOW48,*)DISCOUNT_RT(PRC)
        discount_rt(prc) = discount_rt(prc)/100.0      !convert to fraction for calculation purpose
        discount_rt(prc) = disc                        !replace with discount rate calcuated in wellogs.f
      ENDDO
!
!
!  Section 7: Consumer Price Indices
!*****************************************************************************
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NOM_YEAR
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)N_CPI
      CALL OGSM_NEXTDATA(LOW48)

      DO YR=1,N_CPI
        READ(LOW48,*)CPI_YEAR(YR),CPI_AVG(CPI_YEAR(YR)), &
          CPI_FACTOR(CPI_YEAR(YR))
      ENDDO

       CPI_2005 = CPI_FACTOR(2005) / CPI_FACTOR(NOM_YEAR)
       CPI_2003 = CPI_FACTOR(2003) / CPI_FACTOR(NOM_YEAR)

!========================================================================
!     SECTION 8:  OIL & GAS SALES CONTROLS

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NUM_PROC
      CALL OGSM_NEXTDATA(LOW48)
      DO PRC=1,NUM_PROC
        READ(LOW48,*)RESP1,RESP2
          OIL_SALES(PRC) = CONVRESP(RESP1)
          GAS_SALES(PRC) = CONVRESP(RESP2)
      ENDDO

       RETURN
       END SUBROUTINE


!*******************************************************************
!    CONVRESP     FUNCTION TO CONVERT RESPONSE TO LOGICAL
!*******************************************************************

      LOGICAL FUNCTION CONVRESP(RESP)

!!! function that returns logical true if response is yes/YES
      CHARACTER*(*) RESP
      CONVRESP=.FALSE.
      IF(INDEX(RESP,'Y') > 0  .or. INDEX(RESP,'y') > 0) THEN
        CONVRESP=.TRUE.
      ELSEIF(INDEX(RESP,'N') == 0 .AND. INDEX(RESP,'n') == 0) THEN
       WRITE(OGBUG1,*) RESP,' NOT VALID ANSWER TO YES/NO IN OLOGSS'
      ENDIF

      RETURN
      END




!***************************************************************
!from READ_CONSTRAINT.FOR
       SUBROUTINE READ_CONSTRAINT

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsml48'

      REAL*8      T_Price 
      REAL*8      T_Quantity 
      INTEGER*4   T_INDX, J_INDX, I_INDX 
      INTEGER*4   T_REG 
      INTEGER*4   T_ID 
      INTEGER*4   T_SRC_INDX 
      INTEGER*4   T_BIN
      CHARACTER*8 T_SOURCE 

!          open constraints file
      INTEGER   NUM_REG                          ! NO. REGIONS TO SET UP
      INTEGER   RGN                              ! REGION VARIABLE
      INTEGER   YR                               ! YEAR INDEX VARIABLE
      INTEGER   RIG                              ! RIG DEPTH INTERVAL INDEX VARIABLE
      INTEGER   SRC                              ! CO2 SOURCE INDEX VARIABLE
      INTEGER   temp
      REAL*4    TEMPCONV                         ! TEMP TO READ % CONVENTIONAL
      LOGICAL   CONVRESP                         ! FUNCTION TO CONVERT RESPONSE TO LOGICAL

      LOW48=IFILE1

!===================================================================
!     SECTION 1:  RIG DATA
!                 RIG DEPTH RATING

      CALL OGSM_NEXTDATA(LOW48)
      DO RIG = 1,MAX_RDR
        READ(LOW48,*) RDR(1,0,RIG),(RDR(1,r,RIG),r=1,max_reg-1)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO RIG = 1,MAX_RDR
        READ(LOW48,*) RDR(2,0,RIG),(RDR(2,r,RIG),r=1,max_reg-1)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*) USE_RDR               ! USE NATIONAL RIG DEPTH

! controls for the rig/footage distribution           
      call ogsm_nextdata(low48)                       
      do R = 1,max_reg-1                              
         read (low48,*) dreg(r),drega(r),dregb(r)     
      end do                                          
      call ogsm_nextdata(low48)                       
      read (low48,*) dreg_util                        
      read (low48,*) dreg_rate                        
      call ogsm_nextdata(low48)                       
      DO RIG = 1,MAX_RDR                              
        READ(LOW48,*) rigs_conc(rig)                  
      ENDDO                                           

!===================================================================
!     SECTION 2:  DRILLING CONSTRAINTS
!                 A. OIL DRILLING AND DRYHOLE
!                 B. GAS DRILLING AND DRYHOLE
!                 C. EXPLORATION DRILLING
!                 D. DISTRIBUTION CNTRLS AND MULTIPLIERS
!                 NOTE:  ALL DRILLING EQTNS ARE IN FEET

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)OILA0,OILA1,OILA2,OILA3,OILA4                   ! A0,A1 FOR OIL FOOTAGE EQTN

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)OILD0,OILD1,OILD2,OILD3,OILD4                 ! A0,A1 FOR DRYOIL FOOT EQTN

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1, max_reg-1                     
        READ(LOW48,*)RGN,PRO_REGOIL(RGN),sucdeve(rgn)           ! REGIONAL DISTRIBUTION OF
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)GASA0,GASA1,GASA2,GASA3,GASA4                   ! A0,A1 FOR GAS FOOTAGE EQTN

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)GASD0,GASD1,GASD2,GASD3,GASD4                 ! A0,A1 FOR DRYGAS FOOT EQTN

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1, max_reg-1                      
        READ(LOW48,*)RGN,PRO_REGGAS(RGN)           ! REGIONAL DISTRIBUTION OF
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)EXPA0,EXPA1,EXPA2,EXPA3,EXPA4                 ! A0,A1 FOR EXPLORATION - Oil
      read(low48,*)expag0,expag1,expag2,expag3,expag4            ! A0,A1 for exploration - Gas
      read(low48,*)expd0,expd1,expd2,expd3,expd4                 ! A0,A1 for exploration - Dry

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1, max_reg-1                     
        READ(LOW48,*)RGN,SUCEXP(RGN),sucexpd(rgn),PRO_REGEXP(RGN),pro_regexpg(rgn),pro_regexpd(rgn)
      ENDDO

!!! DISTRIBUTION CNTLS & MULTIPLIERS
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*) REG_NAT                 ! USE REGIONAL OR NATIONAL CONSTRAINTS?

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*) SPLIT_OG                ! SPLIT BTWN OIL & GAS?

      call ogsm_nextdata(low48)                                                        
      read (low48,*) drill_res              ! % to be transferred between resources   

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)SPLIT_ED                 ! SPLIT BTWN EXPLORATION & DEVELOPMENT?

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)EXPL_FRAC                 ! % EXP DRILLING FOR CONVENTIONAL
      read(low48,*)expl_fracg                ! % exploration drilling for conventional gas

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*) HOR_VERT                 ! SPLIT BTWN HORIZONTAL & VERTICAL?

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)H_PERCENT                 ! % AVAIL FOR HORIZONTAL DRILLING

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)DRILL_TRANS               ! % TO BE TRANSFERRED BTWN REGIONS

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)DRILL_OVER                ! % AVAIL FOR FOOTAGE OVER RUN

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)RGR                       ! ANNUAL DRILLING GROWTH RATE

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)RRR                       ! ANNUAL DRILLING RETIREMENT RATE

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)EXPMUL                     ! EXPLORATION DRILLING

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)TOTMUL                     ! TOT DRILLING CONSTRAINT

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)HORMUL                     ! HORIZONTAL DRILLING

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)H_GROWTH                   ! HORIZONTAL WELL GROWTH RATE

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)H_SUCCESS                  ! HORIZONTAL SUCCESS RATE

!===================================================================
!     SECTION 3:  CAPITAL CONSTRAINTS (MM$)

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)(NAT_INVCAP(YR),YR=1,max_yr-1)  ! CAPITAL CONSTRAINT

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)CAPMUL                      ! CAPITAL CONSTRAINT MULTIPLIER

!===================================================================
      call ogsm_nextdata(LOW48)
         read (LOW48,115) (oil_adj(yr),yr=1,max_yr)
         read (LOW48,115) (gas_adj(yr),yr=1,max_yr)
         read (LOW48,115) (exp_adj(yr),yr=1,max_yr)
         read (low48,115) (exp_adjg(yr),yr=1,max_yr)
         read (LOW48,115) (hor_adj(yr),yr=1,max_yr)

115   format (19x,<max_yr>(f5.3,6x))

       RETURN
       END SUBROUTINE                          !END READ_CONSTRAINT SUBROUTINE


!     Heapsort - Sorts an array RA of length N into ascending numerical
!     order using the Heapsort Algorithm. N is input; RA is
!     REPLACED ON OUTPUT BY ITS SORTED REARRANGEMENT AND RB
!     IS REARRANGED CORRESPONDINGLY.
!
      SUBROUTINE WELLHEAPSORT(N,RA,RB)
!
      include'emmparm'
      REAL*8 RA(N),RRA
      INTEGER RB(N),RRB
      INTEGER N,L,IR,I
!
      DO I=1,N
!         WRITE(6,5204)'heapsort in',I,N,RA(I),RB(I)
      ENDDO
5204  FORMAT(A25,1x,2(I8,1x),F12.4,1x,I8,1x)      
      
      L = N / 2 + 1
      IR = N
!
!     The index L will be decremented from its initial value down to 1 durin
!     the "hiring" (heap creation) phase. Once it reaches 1, the index IR
!     will be decremented from its initial value down to 1 during the
!     "retirement-and-promotion" (heap selection) phase.
!
   10 CONTINUE
      IF (L .GT. 1) THEN                       ! Still in hiring phase ?
         L = L - 1
         RRA = RA(L)
         RRB = RB(L)
      ELSE                           !  In retirement-and-promotion phase.
         RRA = RA(IR)                  !  Clear a space at end of array.
         RRB = RB(IR)
         RA(IR) = RA(1)              !  Retire the top of the heap into it.
         RB(IR) = RB(1)
         IR = IR - 1                 !  Decrease the size of the corporation
         IF (IR .EQ. 1) THEN            !  Done with the last promotion.
            RA(1) = RRA              !  The least competent worker of all !
            RB(1) = RRB
            RETURN
         ENDIF
      ENDIF
!
!     Whether we are in the hiring phase or promotion phase, we here set
!     up to sift down element RRA to its proper level.
!
      I = L
      J = L + L
   20 IF (J .LE. IR) THEN                             ! Do while J <= IR
         IF (J .LT. IR) THEN
            IF (RA(J) .LT. RA(J + 1)) J = J + 1   ! Comp to the better under
         ENDIF
         IF (RRA .LT. RA(J)) THEN                           ! Demote RRA
            RA(I) = RA(J)
            RB(I) = RB(J)
            I = J
            J = J + J
         ELSE         ! This is RRA's level. Set J to terminate the sift-dow
            J = IR + 1
         END IF
         GO TO 20
      ENDIF
      RA(I) = RRA                                  ! Put RRA into its slot.
      RB(I) = RRB
!        WRITE(6,5204)'heapsort out',I,N,RA(I),RB(I)
      GO TO 10
      

 
    
      
      END

!***************************************************************
!from READ_ACCESS.FOR
       SUBROUTINE READ_ACCESS

!  this subroutine reads the play level resource access classifications for each play.
!  there are 10 resource access categories

       IMPLICIT NONE

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsml48'

       INTEGER acc
       INTEGER temp1,temp2,temp3,temp4

       LOW48=IFILE1

! initialization
       do i = 1,max_prov
         provnum(i) = 0
         do j = 1,max_acc
            provacc(i,j) = 0.0
         end do
       end do

       CALL OGSM_NEXTDATA(LOW48)

       do i = 1,max_prov
         read (LOW48,*,END=100,ERR=100) provnum(i),temp1,temp2,temp3, &
                                        temp4

         do j = 1,4
            provacc(i,j) = temp1
         end do
         do j = 5,7
            provacc(i,j) = temp2
         end do
         provacc(i,8) = temp3
         provacc(i,9) = temp4
         provacc(i,10)= L48B4YR

       end do

100   CONTINUE

1190  format (11(i4,2x))

       RETURN
       END SUBROUTINE
!***************************************************************
!from READ_PATTDEV.FOR
       SUBROUTINE READ_PATTDEV

!  read the pattern development file
!      This file contains the percentage of wells to be developed each year for each
!      process.  In addition, it has the maximum number of wells which can be developed
!      each year
       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsml48'

      INTEGER  PROC                              ! PROCESS INDEX VARIABLE

      do PROC = 1,max_proc
        PATT_DEV(proc)     = 0.0
        PATT_DEV_MAX(proc) = 0.0
        PATT_DEV_MIN(proc) = 0.0
      end do

      LOW48=IFILE1


      CALL OGSM_NEXTDATA(LOW48)
      DO PROC = 1,MAX_PROC
        READ(LOW48,*)PATT_DEV(PROC),PATT_DEV_MAX(PROC), &
         PATT_DEV_MIN(PROC)
      ENDDO

      RETURN
      END SUBROUTINE
!***************************************************************
!from READ_PRICES.FOR
       SUBROUTINE READ_PRICES

!  read the price file for oil and natural gas
       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsml48'
       include 'macout'
       include 'pmmout'
       include 'ngtdmrep'

      INTEGER PYR                                ! PRICE YR INDEX VARIABLE
      INTEGER myr
      REAL*4  FOILPRICE                          ! FIXED OIL PRICE ($/BBL)
      REAL*4  FGASPRICE                          ! FIXED GAS PRICE ($/MCF)
      REAL*4  INFLFAC                            ! ANNUAL INFLATION FACTOR
      REAL temp
      REAL tempoil2,tempgas2

      REAL tempoil(max_yr)
      REAL tempgas(max_yr)
      CHARACTER*1 FPRICE                         ! USE FIXED PRICES?
      CHARACTER*1 temp2


      LOW48=IFILE1

!===================================================================
!     SECTION 1:  FIXED PRICES

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)FPRICE                        ! USE FIXED PRICES?

      IF(FPRICE == 'Y'.OR.FPRICE == 'y')THEN
        READ(LOW48,*)FOILPRICE                   ! FIXED OIL PRICE ($/BBL)
        READ(LOW48,*)FGASPRICE                   ! FIXED NAT. GAS PRICE ($/MCF)
        READ(LOW48,*)INFLFAC                     ! ANNUAL INFLATION FACTOR

        call ogsm_nextdata(LOW48)
        read (LOW48,*) temp
        call ogsm_nextdata(LOW48)
        read (LOW48,*) temp
        read (LOW48,*) temp

        do pyr =1,max_yr
          tempoil(pyr)=foilprice
          tempgas(pyr)=fgasprice
        end do

        do pyr = 1,max_yr
            IF(pyr <= max_yr-5) then
          tempoil2 = 0.0
          tempgas2 = 0.0
          do myr = pyr,pyr+4
             tempoil2 = tempoil2 + tempoil(myr)
             tempgas2 = tempgas2 + tempgas(myr)
          end do
          oilpriced(pyr) = tempoil2/5.0
          gaspriced(pyr) = tempgas2/5.0
         else
          oilpriced(pyr) = oilpriced(pyr-1)
          gaspriced(pyr) = gaspriced(pyr-1)
         END if
        end do


        DO PYR=1,max_yr-1
            if (pyr == 1) then
          OILPRICEo(PYR)=FOILPRICE
          GASpriceo(PYR)=FGASPRICE
         else
          OILPRICEo(PYR)=oilpriceo(pyr-1)+ &
            (oilpriceo(pyr-1)*(INFLFAC/100.0))
          GASpriceo(PYR)=gaspriceo(pyr-1)+ &
            (gaspriceo(pyr-1)*(INFLFAC/100.0))
         end if

        ENDDO
   !        CALL OGSM_NEXTDATA(LOW48)  ! SKIP OVER PRICE TRACK

      ELSE     !!! SECTION 2:  READ PRICE TRACK
        call ogsm_nextdata(LOW48)
        read (LOW48,*) priceyr                    !first year of price track
        CALL OGSM_NEXTDATA(LOW48)                 ! ANNUAL OIL PRICE ($/BBL)
        READ(LOW48,*)(tempoil(PYR),PYR=1,28)
                                                  ! ANNUAL GAS PRICE ($/MCF)
        READ(LOW48,*)(tempgas(PYR),PYR=1,28)

        do pyr =29,max_yr
          tempoil(pyr)=tempoil(pyr-1)
          tempgas(pyr)=tempgas(pyr-1)
        end do

        do pyr = 1,max_yr
            IF(pyr <= max_yr-5) then
          tempoil2 = 0.0
          tempgas2 = 0.0
          do myr = pyr,pyr+4
             tempoil2 = tempoil2 + tempoil(myr)
             tempgas2 = tempgas2 + tempgas(myr)
          end do
          oilpriced(pyr) = tempoil2/5.0
          gaspriced(pyr) = tempgas2/5.0
         else
          oilpriced(pyr) = oilpriced(pyr-1)
          gaspriced(pyr) = gaspriced(pyr-1)
         END if
        end do



         if (L48B4YR <= priceyr) then
          do pyr = 1,max_yr
             oilpriceo(pyr) = tempoil(pyr)
             gaspriceo(pyr) = tempgas(pyr)
          end do
         ELSEIF (L48B4YR > priceyr) then
          do pyr = 1,max_yr
               if (pyr+(L48B4YR-priceyr) <= max_yr) then
             oilpriceo(pyr) = tempoil(pyr+(L48B4YR-priceyr))
             gaspriceo(pyr) = tempgas(pyr+(L48B4YR-priceyr))
            else
             oilpriceo(pyr) = tempoil(max_yr)
             gaspriceo(pyr) = tempgas(max_yr)
            end if
          end do
        end if

        dladj = mc_jpgdp(nom_year-baseyr+1)/mc_jpgdp(-2)
        write(bugout,*) 'dladj', dladj
         do m=1,max_yr
          j=l48b4yr-baseyr+m
            if (j <= lastyr) then
              oilpriced(m) = DCRDWHP(MNUMOR,j)*dladj
              gaspriced(m) = OGWPRNG(MNUMOR,j)*dladj
            else
              oilpriced(m) = DCRDWHP(MNUMOR,lastyr)*dladj
              gaspriced(m) = OGWPRNG(MNUMOR,lastyr)*dladj
            endif
         enddo

      ENDIF



!===================================================================
!     SECTION 3:  PRICE FOR GAS PROCESSING BYPRODUCTS

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)CO2PRICE                      ! CO2 PRICE ($/MCF)
      READ(LOW48,*)H2SPRICE                      ! H2S PRICE ($/METRIC TON)
      READ(LOW48,*)NGLPRICE                      ! NGL PRICE ($/GAL) !!! NOT USED - USING PMM VARIABLE PLGINPF(11,CURIYR)
      READ(LOW48,*)N2PRICE                       ! N2 PRICE ($/MCF)

      call ogsm_nextdata(LOW48)
      read (LOW48,*) temp2
      read (LOW48,*) temp
      read (LOW48,*) temp
      call ogsm_nextdata(LOW48)
      read (LOW48,*) oilco2                      ! FIXED OIL PRICE ($/BBL) USED FOR ECONOMIC PRESCREENING OF INDUSTRIAL CO2 PROJECTS



      RETURN
      END SUBROUTINE

!***************************************************************
!from read_depr.FOR
       SUBROUTINE READ_DEPRECIATION

       implicit none

!  this routine reads the depreciation schedules from the file and fills in the
!  depreciation schedule for each process.
       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsml48'

      INTEGER DYR                                ! DEPRECIATION YR INDEX VARIABLE
      INTEGER NUM_DPROC                          ! NO. OF OVERRIDE PROCESSES
      INTEGER PROC                               ! PROCESS INDEX VARIABLE
      INTEGER OVR                                ! OVERRIDE SCHEDULE INDEX VARIABLE
      INTEGER depr_proc(max_proc)
      INTEGER depr_yr(max_proc)
      REAL*4 depr_ovr(max_qdyr)

 1217  format (a7,2x,8(3x,a5,i2,2x))
 1218  format (i7,2x,8(f10.2,2x))

      LOW48=IFILE1

!  initialize the values
       iamoryr = 0
       do PROC = 1,max_proc
         depr_yrs(proc) = 0
       end do
       do dyr = 1,max_qdyr
          amorschl(dyr) = 0.0
          deprschl(dyr) = 0.0
       end do

       do PROC = 1,max_proc
         do DYR = 1,max_qdyr
            depr_sch(proc,dyr) = 0.0
         end do
       end do



!===================================================================
!     SECTION 1:  DEFAULT DEPRECIATION AND AMORTIZATION SCHEDULES

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)IDEPRYR                  ! NO. YRS IN DEFAULT DEPRECIATION SCHEDULE

      CALL OGSM_NEXTDATA(LOW48)
      DO DYR=1,IDEPRYR
        READ(LOW48,*)DEPRSCHL(DYR)          ! ANNUAL FRACTION DEPRECIATED
   !  default the depreciation schedule array
        do PROC = 1,max_proc
            depr_sch(proc,dyr) = deprschl(dyr)
        end do
      ENDDO

      do proc = 1,max_proc
        depr_yrs(proc)         = idepryr
      end do

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)IAMORYR                  ! NO. YRS IN DEFAULT AMORTIZATION SCHEDULE

      CALL OGSM_NEXTDATA(LOW48)
      DO DYR=1,IAMORYR
        READ(LOW48,*)AMORSCHL(DYR)          ! ANNUAL FRACTION AMORTIZED
      ENDDO

!===================================================================
!     SECTION 2:  PROCESS SPECIFIC DEPRECIATION SCHEDULES

      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NUM_DPROC                ! NO. OF OVERRIDE PROCESSES

      DO PROC=1,NUM_DPROC
        CALL OGSM_NEXTDATA(LOW48)
        READ(LOW48,*)DEPR_PROC(PROC)        ! PROCESS NO.FOR OVERRIDE SCHEDULE
        READ(LOW48,*)DEPR_YR(PROC)          ! NO. YRS FOR OVERRIDE SCHEDULE
          DO OVR=1,DEPR_YR(PROC)
            READ(LOW48,*)DEPR_OVR(OVR)      ! ANNUAL FRACTION TO DEPRECIATE
            depr_sch(depr_proc(proc)+1,ovr)=depr_ovr(ovr)
          ENDDO
          do dyr=depr_yr(proc)+1,MAX_qdyr
             depr_sch(depr_proc(proc)+1,dyr)=0.0
          end do
          depr_yrs(depr_proc(proc)+1) = depr_yr(proc)
      ENDDO

      RETURN
      END SUBROUTINE

!****************************************************************************************
!     READ_OTHER SUBROUTINE:
!      read the playmap file
!****************************************************************************************
       SUBROUTINE READ_OTHER

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       INCLUDE 'ogsml48'
       INCLUDE 'ogsmugr'


      LOW48=IFILE1

      CALL OGSM_NEXTDATA(LOW48)

      do i = 1,max_play
! initialization of the aggregated size classes
         read (LOW48,*) bas_play(i),playnum(i)
         IF(bas_play(i) == 00) EXIT
         playlist(i) = playnum(i)
      ENDDO

      nplay = i-1

      RETURN
      END SUBROUTINE                        !END READ_OTHER SUBROUTINE
!***************************************************************
!from read_taxes.FOR
       SUBROUTINE READ_TAXES

       IMPLICIT NONE

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsml48'

       INTEGER ijk
!       CHARACTER*1 resp
       LOGICAL convresp

       LOW48=IFILE1

       call OGSM_NEXTDATA(LOW48)

!  federal tax rate
       read (LOW48,*) FEDRATE
       fedrate = fedrate / 100.0

!  royalty rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) XROY
       IF (OGRUNOP(21).GE.1) XROY = 16.667
       xroy = xroy / 100.0

!  independent producer depletion rate
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) ipdr
       ipdr = ipdr / 100.0

!  are intangible drilling costs to be capitalized?
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       cidc = CONVRESP(RESP)

!  are other intangibles to be capitalized?
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       coi = CONVRESP(RESP)

!  include evironmental costs?
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       envscn = CONVRESP(RESP)

!  are environmental costs to be capitalized?
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       ce = CONVRESP(RESP)

!  implement alternative minimum taxes?
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       amt = CONVRESP(RESP)

!  allow AMT taxes paid to be used as credits in future years?
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       credamt = CONVRESP(RESP)

!  six month amortization rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) smar
       smar = smar / 100.0

!  intangible drilling cost preference deduction
       call ogsm_nextdata(LOW48)
       read (LOW48,*) ipdr
       ipdr = ipdr / 100.0

!  ACE Rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) acer
       acer = acer / 100.0

!  maximum alternative minimum tax reduction for independents
       call ogsm_nextdata(LOW48)
       read (LOW48,*) ira
       ira = ira / 100.0

!  alternative mimimum tax rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) amtrate
       amtrate = amtrate/100.0

!  expense environmental costs
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       eec = CONVRESP(RESP)

!  allow net income limitations
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       nil = CONVRESP(RESP)

!  net income limitation limit
       call ogsm_nextdata(LOW48)
       read (LOW48,*) nill
       nill = nill / 100.0

!  percent depletion rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) pdr
       pdr = pdr / 100.0

!  percent of intangible investments to capitalized
       call ogsm_nextdata(LOW48)
       read (LOW48,*) piic
       piic = piic/100.0

!  EOR Tax Credit
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       eortc = CONVRESP(RESP)

!  EOR Tax Credit Rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) eortcr
       eortcr = eortcr/100.0

!  EOR Tax Credit Addback
       call ogsm_nextdata(LOW48)
       read (LOW48,*) eortcab
       eortcab = eortcab / 100.0

!  EOR Tax Credit Rate Use by year
       call ogsm_nextdata(LOW48)
       read (LOW48,*) (eortcrp(ijk),ijk=1,10)
       do ijk = 1,10
         eortcrp(ijk) = eortcrp(ijk) / 100.0
       end do

!  EOR Tax Credit phase out oil price
       call ogsm_nextdata(LOW48)
       read (LOW48,*) eortcp

!  Allow G&G tangible tax credit (depletion)
       call OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       ggctc = CONVRESP(RESP)

!  G&G tangible tax credit rate (depletion)
       call ogsm_nextdata(LOW48)
       read (LOW48,*) ggctcr
       ggctcr = ggctcr / 100.0

!  G&G tangible tax credit addback (depletion)
       call ogsm_nextdata(LOW48)
       read (LOW48,*) ggctcab
       ggctcab = ggctcab / 100.0

!  Allow G&G intangible tax credit (expensed)
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       ggetc = CONVRESP(RESP)

!  G&G intangible tax credit rate (expensed)
       call ogsm_nextdata(LOW48)
       read (LOW48,*) ggetcr
       ggetcr = ggetcr / 100.0

!  G&G intangible tax credit addback (expensed)
       call ogsm_nextdata(LOW48)
       read (LOW48,*) ggetcab
       ggetcab = ggetcab / 100.0

!  Allow lease acquisition depletable tax credit
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       lactc = CONVRESP(RESP)

!  lease acquisition tax credit rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) lactcr
       lactcr = lactcr / 100.0

!  lease acquisition tax credit addback
       call ogsm_nextdata(LOW48)
       read (LOW48,*) lactcab
       lactcab = lactcab / 100.0

!  allow tax credit for expensed lease acquisition costs
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       laetc = CONVRESP(RESP)

!  tax credit rate for expensed lease acquisition costs
       call ogsm_nextdata(LOW48)
       read (LOW48,*) laetcr
       laetcr = laetcr / 100.0

!  tax credit for expensed lease acquisition costs addback
       call ogsm_nextdata(LOW48)
       read (LOW48,*) laetcab
       laetcab = laetcab / 100.0

!  allow tangible development tax credit
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       tdtc = CONVRESP(RESP)

!  tangible development tax credit rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) tdtcr
       tdtcr = tdtcr / 100.0

!  tangible development tax credit addback
       call ogsm_nextdata(LOW48)
       read (LOW48,*) tdtcab
       tdtcab = tdtcab / 100.0

!  allow intangible drilling cost tax credit
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       idctc = CONVRESP(RESP)

!  intangible drilling cost tax credit rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) idctcr
       idctcr = idctcr / 100.0

!  intangible drilling cost tax credit addback
       call ogsm_nextdata(LOW48)
       read (LOW48,*) idctcab
       idctcab = idctcab / 100.0

!  allow other intangible tax credit
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       oitc = CONVRESP(RESP)

!  other intangible tax credit rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) oitcr
       oitcr = oitcr / 100.0

!  other intangible tax credit addback
       call ogsm_nextdata(LOW48)
       read (LOW48,*) oitcab
       oitcab = oitcab / 100.0

!  allow environmental tangible tax credit
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       ettc = CONVRESP(RESP)

!  environmental tangible tax credit rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) ettcr
       ettcr = ettcr / 100.0

!  environmental tangible tax credit addback
       call ogsm_nextdata(LOW48)
       read (LOW48,*) ettcab
       ettcab = ettcab / 100.0

!  allow environmental intangible tax credit
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       eitc = CONVRESP(RESP)

!  environmental intangible tax credit rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) eitcr
       eitcr = eitcr / 100.0

!  environmental intangible tax credit addback
       call ogsm_nextdata(LOW48)
       read (LOW48,*) eitcab
       eitcab = eitcab / 100.0

!  allow environmental operating cost tax credit
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       eoctc = CONVRESP(RESP)

!  environmental operating cost tax credit rate
       call ogsm_nextdata(LOW48)
       read (LOW48,*) eoctcr
       eoctcr = eoctcr / 100.0

!  environmental operating cost tax credit addback
       call ogsm_nextdata(LOW48)
       read (LOW48,*) eoctcab
       eoctcab = eoctcab / 100.0

!  allow tax credit on tangible investments
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       tcoti = CONVRESP(RESP)

!  number of years for tax credit on tangible investments
       call ogsm_nextdata(LOW48)
       read (LOW48,*) yr1

!  allow tax credit on intangible investments
       call ogsm_nextdata(LOW48)
       read (LOW48,*) RESP
       tcoii = CONVRESP(RESP)

!  number of years for tax credit on intangible investments
       call ogsm_nextdata(LOW48)
       read (LOW48,*) yr2

      CALL OGSM_NEXTDATA(LOW48)
       read (LOW48,*) RESP
       marginal = CONVRESP(RESP)         !indicates whether the marginal tax credit is to be applied
       write(ogbug1,*)'marginal ',marginal

      CALL OGSM_NEXTDATA(LOW48)
       read (low48,*) m_oil1             !bopd for marginal oil well
       read (low48,*) m_gas1             !mcfd for marginal gas well
      CALL OGSM_NEXTDATA(LOW48)
       read (low48,*) m_oil2             !bopd for marginal oil well - 2nd definition
       read (low48,*) m_wat
      call ogsm_nextdata(low48)
       read (low48,*) m_api
      CALL OGSM_NEXTDATA(LOW48)
       read (low48,*) m_amt_oil
       read (low48,*) m_amt_gas
      CALL OGSM_NEXTDATA(LOW48)
       read (low48,*) m_vol_oil
       read (low48,*) m_vol_gas
      CALL OGSM_NEXTDATA(LOW48)
       read (low48,*) marg_oil_min,marg_oil_max
       read (low48,*) marg_gas_min,marg_gas_max


       RETURN
       END SUBROUTINE

!***********************************************************************************
!
!  This subroutine was created 3.10.10 by MC.  It reads the technology and economic
!  parameters for hydraulic fracturing for oil.
!
!***********************************************************************************
       subroutine read_frac

       IMPLICIT NONE

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsml48'

       LOGICAL convresp

       LOW48=IFILE1

       call OGSM_NEXTDATA(LOW48)

!  apply hydraulic fracture to oil projects?
       read (LOW48,*) resp
       fracture = CONVRESP(RESP)
     write(ogbug1,*)'fracture ', fracture
       call OGSM_NEXTDATA(LOW48)

!  fraction of wells to hydraulically fracture
       read (low48,*) hfp_app
       hfp_app = hfp_app/100.0
       call OGSM_NEXTDATA(LOW48)

!  cutoff permeability for "high permeability fracturing (TSO)"
       read (low48,*) hfp_cut
!       print*, hfp_cut
       call OGSM_NEXTDATA(LOW48)

! parameters for fracture length calculations
       read (low48,*) fw_low,fw_high
       read (low48,*) fc_low,fc_high
       read (low48,*) kf_low,kf_high
       read (low48,*) mxf_low,mxf_high
!       print*, fw_low,fw_high,fc_low,fc_high,kf_low,kf_high,mxf_low,mxf_high
       call OGSM_NEXTDATA(LOW48)

! cost equation parameters
       read (low48,*) min_fd(1),max_fd(1),k_fd(1),a_fd(1),b_fd(1),c_fd(1)
       read (low48,*) min_fd(2),max_fd(2),k_fd(2),a_fd(2),b_fd(2),c_fd(2)
!       print*, k_fd(1),a_fd(1),b_fd(1),c_fd(1)
!       print*, k_fd(2),a_fd(2),b_fd(2),c_fd(2)
       call OGSM_NEXTDATA(LOW48)

! technology levers
       read (low48,*) chg_frac_fac(1),chg_frac_fac(2)
       read (low48,*) cst_frac_fac(1),cst_frac_fac(2)
       read (low48,*) chg_fraclen_fac(1),chg_fraclen_fac(2)
       read (low48,*) cst_fraclen_fac(1),cst_fraclen_fac(2)
       call OGSM_NEXTDATA(LOW48)
!       print*, chg_frac_fac(1),chg_frac_fac(2),cst_frac_fac(1),cst_frac_fac(2)
!       print*, chg_fraclen_fac(1),chg_fraclen_fac(2),cst_fraclen_fac(1),cst_fraclen_fac(2)

! screening criteria
       read (low48,*) hfrac_perm
       read (low48,*) hfrac_api
       read (low48,*) hfrac_depth

       call ogsm_nextdata(low48)
       do i = 1,7
         read (LOW48,*) resp
         hfrac_region(i) = CONVRESP(RESP)
       end do

       call ogsm_nextdata(low48)
       do i = 1,2
         read (low48,*) resp
         hfrac_proc(i) = convresp(resp)
       end do


       end subroutine
!***********************************************************************************
!***********************************************************************************
!***********************************************************************************
!***********************************************************************************

!*******************************************************************
!    OGSM_NEXTDATA    SUBROUTINE TO SKIP OVER COMMENTS IN DATA FILE
!
!*******************************************************************
      SUBROUTINE OGSM_NEXTDATA(UNITNUM)
      IMPLICIT NONE

      INTEGER*4     UNITNUM,I
      CHARACTER*1   CH
      CHARACTER*1   DUM

! READ UNTIL @ IN 2nd COLUMN

      CH = 'A'
      DO WHILE (CH  /=  '@')
        READ(UNITNUM,*) CH
      ENDDO


      RETURN
      END
!*******************************************************************
!from READ_RESOURCE.FOR
       SUBROUTINE READ_RESOURCE(IMODE)

!   This file reads the technical production profile for each reservoir.  The
!   production profile is at the well level.  For the specific layout and formats of
!   the resource files, see the "OLOGSS Input File for Economic/Timing Module
!   Output of Process Type Curve" document.

       IMPLICIT NONE

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsmugr'
       INCLUDE 'ogsml48'

      CHARACTER*11 filename, str1
       INTEGER itech                                          !technology loop counter
       INTEGER ires                                           !reservoir counter
       INTEGER iflag
       INTEGER ires2
       INTEGER im
       INTEGER*4 WLEOR           ! INPUT FILE NUMBER
       INTEGER IMODE                                          !1 - DECLINE RESERVOIRS
                                                              !2 - EXPLORATION & EOR/ASR

       LOGICAL test_exist

       ires = 1
       ires2= 1
      IF(IMODE == 2) NRES = 0

!  DECLINE CURVE
      IF(IMODE == 1) THEN
         IF(runtype == 1.or.runtype == 3) then
           do itech = 1,max_tech
             iflag = 0
             FNAME='WLOIL'
             NEW=.FALSE.
             WLRES = FILE_MGR('O',FNAME,NEW)
!            CALL OGSM_NEXTDATA(WLRES)
             read (WLRES, *)  str1                         !SZ change 7/25/16 to skip the head line
             do ires = 1,60000
               if (iflag == 0) then
                 call read_res_data(ires,itech,iflag)
                 ires2 = ires
               END if
             end do
             WLRES = FILE_MGR('C',FNAME,NEW)
           end do

           IF(imode == 1) fdec = 1
           IF(imode == 1) ldec = ires2-1
           IF(IMODE == 1) nres = ldec

           WRITE(OGBUG1,*) 'FIRST OIL DECLINE RESERVOIR: ',fdec
           WRITE(OGBUG1,*) 'LAST  OIL DECLINE RESERVOIR: ',ldec
         END if

       END IF


      IF(IMODE == 2) THEN
!  EOR OIL
         IF(runtype == 1.or.runtype == 3) then
           IF (OGRUNOP(18) == 1.or.OGRUNOP(18) == 3) THEN
             do itech = 1,max_tech
               iflag = 0
               FNAME='WLEOR'
               NEW=.FALSE.
               WLRES = FILE_MGR('O',FNAME,NEW)
!              CALL OGSM_NEXTDATA(WLRES)
               read (WLRES, *)                           !SZ change 7/25/16 to skip the head line

               do ires = nres+1,nres+60000
                 if (iflag == 0) then
                   call read_res_data(ires,itech,iflag)
                   ires2 = ires
                 END if
               end do
               WLRES = FILE_MGR('C',FNAME,NEW)
             end do
             nres = ires2-1
           END if
         END if

!  ASR OIL
         IF(runtype == 1.or.runtype == 3) then
            IF (OGRUNOP(18) == 1.or.OGRUNOP(18) == 2) THEN
         do itech = 1,max_tech
           iflag = 0
             FNAME='WLASR'
             NEW=.FALSE.
             WLRES = FILE_MGR('O',FNAME,NEW)
!                  CALL OGSM_NEXTDATA(WLRES)
                  read (WLRES, *)                           !SZ change 7/25/16 to skip the head line

           do ires = nres+1,nres+60000
                     if (iflag == 0) then
                call read_res_data(ires,itech,iflag)
                ires2 = ires
             END if
           end do
             WLRES = FILE_MGR('C',FNAME,NEW)
         end do
       nres = ires2-1

       END if
       END if



!  UGRSS RESOURCES
         IF(runtype == 2.or.runtype == 3) then
         do itech = 1,max_tech               
           iflag = 0                        
             FNAME='WLUGR'                
             NEW=.FALSE.                      
             WLRES = FILE_MGR('O',FNAME,NEW)
!               CALL OGSM_NEXTDATA(WLRES)
               read (WLRES, *)                                    !SZ change 7/25/16 to skip the head line

           do ires = nres+1,nres+60000                   
                  if (iflag == 0) then                  
                call read_res_data(ires,itech,iflag)   
                ires2 = ires                          
             END if                                  
           end do                                   
             WLRES = FILE_MGR('C',FNAME,NEW)      
         end do                               
        nres = ires2-1                       
       END IF

       END IF

      IF(IMODE == 1) THEN
       fgas = nres+1

         !  CONVENTIONAL RADIAL GAS
         IF(runtype == 2.or.runtype == 3) then
         do itech = 1,max_tech
           iflag = 0
             FNAME='WLGAS'
             NEW=.FALSE.
             WLRES = FILE_MGR('O',FNAME,NEW)
!               CALL OGSM_NEXTDATA(WLRES)
               read (WLRES, *)                                    !SZ change 7/25/16 to skip the head line

           do ires = nres+1,nres+60000
                  if (iflag == 0) then
                call read_res_data(ires,itech,iflag)
                ires2 = ires
             END if
           end do
             WLRES = FILE_MGR('C',FNAME,NEW)
         end do
       nres = ires2-1

       lgas = ires2-1

       WRITE(OGBUG1,*) 'first gas reservoir:',fgas
       WRITE(OGBUG1,*) 'last gas reservoir:',lgas
       WRITE(OGBUG1,*) 'total reservoir: ',nres
       END if

!==================================================================
       END IF                                               !END - READ DECLINE


      IF(IMODE == 2) THEN                                  !READ EOR/ASR & EXPLORATION
       EX_FCRES = nres+1
       WRITE(ogbug1,*) 'first undiscovered reservoir:',ex_fcres

!  UNDISCOVERED OIL AND GAS RESOURCES
!  THIS IS AN ORDERED LIST CONTAINING ALL UNDISCOVERED RESERVOIRS SET BY THE
!  EXTERNAL EXPLORATION MODULE

         do itech = 1,max_tech
           iflag = 0
           FNAME='WLUND'
           NEW=.FALSE.
           WLRES = FILE_MGR('O',FNAME,NEW)
!            CALL OGSM_NEXTDATA(WLRES)
               read (WLRES, *)                                    !SZ change 7/25/16 to skip the head line
           do ires = nres+1,nres+60000
               if (iflag == 0) then
                call read_res_data(ires,itech,iflag)
                ires2 = ires
             END if
           end do
           WLRES = FILE_MGR('C',FNAME,NEW)
         end do
        WRITE(OGBUG1,*) 'NUMBER OF RESERVOIRS: ',NRES
       EX_CONRES = ires2-1 
       write(ogbug1,*) ex_fcres, ' ex_fcres'
       write(ogbug1,*) ex_conres, ' ex_conres'

            FNAME='WLDISC'
            NEW=.FALSE.
            WLRES = FILE_MGR('O',FNAME,NEW)
           call read_discord                                
             WLRES = FILE_MGR('C',FNAME,NEW)
       END IF                                               !END - READ EOR/ASR & EXPLORATION

        write(ogbug1,*) imode, nres,' end of read resource'

       END SUBROUTINE


!*********************************************************************************************
!*********************************************************************************************
!*********************************************************************************************
      SUBROUTINE READ_RES_DATA(ires,itech,iflag)

   !  this subroutine reads the resource data for one reservoir

      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include 'ogsmugr'
      include 'ogsml48'

      INTEGER ires
      INTEGER itech
      INTEGER iflag
      INTEGER iyr,IR,iRRC,dist
      INTEGER ac, np
      integer rflag
      real TRR_GAS(max_res,max_tech)
      real EUR(max_res,max_tech)
      real EUR_GAS(max_res,max_tech)
      real EUR_OIL(max_res,max_tech)
      real EUR_NGL(max_res,max_tech)
      REAL TRR_OIL(max_res,max_tech)
      REAL TRR_NGL(max_res,max_tech)

      REAL*8 iprann(max_yr)

      CHARACTER*2 st1
      CHARACTER*11 restemp
      character*9  resnum                          
      real         rtemp,rtemp2                   
      real rffact, bngl
      real co2adj
      real oadj, gadj, oEUR_shr(max_yr-1), gEUR_shr(max_yr-1) 
      real cumoil1, cumgas1, cumoil2, cumgas2, cumoil3, cumgas3
      real TPRODOIL(max_res,max_yr-1), TPRODGAS(max_res,max_yr-1), IP_adj
      real sizmin
          

      iflag = 0

! ===SZ change 7/26/16
!?    READ(wlres,*,END=100) ARESID(ires), &
!?       APLAY_CDE(ires), &
!?       ARRC(ires), &
!?       AREGION(ires), &
!?       ADEPTH(ires), &
!?       AAPI(ires), &
!?       AOILVIS(ires), &
!?       AGAS_GRAV(ires), &
!?       AGOR(ires), &
!?       ANGL(ires), &
!?       ANGPLET(ires), &
!?       ANGPLPR(ires), &
!?       ANGPLBU(ires), &
!?       ANGPLIS(ires), &
!?       ANGPLPP(ires), &
!?       AHEATVAL(ires), &
!?       ACO2CONT(ires), &
!?       AH2SCONT(ires), &
!?       AN2CONT(ires), &
!?       ASULFOIL(ires), &
!?       APATSIZ(ires), &
!?       ANWELLOIL(ires), &
!?       ANWELLGAS(ires), &
!?       ANwellINJ(ires), &
!?       ALATNUM(ires,itech), &
!?       ALATLEN(ires,itech), &
!?       APHI(ires), &
!?       APAY(ires), &
!?       APERM(ires), &
!?       ASOI(ires), &
!?       asoc(ires), &
!?       ASOR(ires), &
!?       ASWI(ires), &
!?       ABOI(ires), &
!?       AVDP(ires), &
!?       AORGOOIP(ires), &
!?       ARESFLAG(ires), &               
!?       atotacres(ires), &
!?       aresacc(ires), &
!?       aprov(ires), &
!?       atotprod(ires,itech), &                 
!?       atotinj(ires,itech), &
!?       atotconv(ires,itech), &
!?       atotps(ires,itech), &
!?       atotpat(ires,itech), &
!?       alyroil(ires), &
!?       alyrgas(ires), &
!?       ANUMACC(Ires)
!?      
!?      read (aresid(ires)(10:11),'(i2)') ac
!?      read (aresid(ires)(3:4),'(a2)')   st1
!?      RRC = ARRC(ires)
!?      IR = aregion(ires)
!?      call dist_match(st1,irrc,IR,dist)     ! 
!?
!?      IF (ac == 10.or.(ac == 17.and.aresflag(ires) /= 9)) then
!?         IF (runtype == 2) then
!?           ires = ires - 1
!?              do i = 1,7                   !SZ change 7/25/16 based on new database structure
!?                 read (wlres,*)
!?              end do
!?           GOTO 101
!?         END if
!?    ELSEIF(ac == 16.or.ac == 18.or.ac == 19.or.ac == 20) then
!?       IF(runtype == 1) then
!?           ires = ires - 1
!?              do i = 1,7                   !SZ change 7/25/16 based on new database structure
!?                 read (wlres,*)
!?              end do
!?           GOTO 101
!?         END if
!?      END if
!?      BACKSPACE WLRES


! ===SZ change 7/26/16

      READ(wlres,*,END=100) ARESID(ires), &
         APLAY_CDE(ires), &
         ARRC(ires), &
         AREGION(ires) , &
         ADEPTH(ires), &
         AAPI(ires), &
         AOILVIS(ires), &
         AGAS_GRAV(ires), &
         AGOR(ires), &
         ANGL(ires), &
         ANGPLET(ires), &
         ANGPLPR(ires), &
         ANGPLBU(ires), &
         ANGPLIS(ires), &
         ANGPLPP(ires), &
         AHEATVAL(ires), &
         ACO2CONT(ires), &
         AH2SCONT(ires), &
         AN2CONT(ires), &
         ASULFOIL(ires), &
         APATSIZ(ires), &
         ANWELLOIL(ires), &
         ANWELLGAS(ires), &
         ANwellINJ(ires) , &
         ALATNUM(ires,itech), &
         ALATLEN(ires,itech), &
         APHI(ires), &
         APAY(ires), &
         APERM(ires), &
         ASOI(ires), &
         asoc(ires), &
         ASOR(ires), &
         ASWI(ires), &
         ABOI(ires), &
         AVDP(ires) , &
         AORGOOIP(ires), &
         ARESFLAG(ires), &
         atotacres(ires), &
         aresacc(ires), &
         aprov(ires), &
         atotprod(ires,itech), &
         atotinj(ires,itech), &
         atotconv(ires,itech), &
         atotps(ires,itech), &
         atotpat(ires,itech), &
         alyroil(ires), &
         alyrgas(ires), &
         ANUMACC(Ires), &
        (APRODOIL(IRES,IYR,itech),IYR = 1,max_yr) , &
        (APRODGAS(IRES,IYR,itech),IYR = 1,max_yr), &
        (aprodco2(ires,iyr,itech),iyr = 1,max_yr), &
        (APRODWAT(IRES,IYR,itech),IYR = 1,max_yr), &
        (AWATINJ(IRES,IYR,itech),IYR = 1,max_yr), &
        (AINJINJ(IRES,IYR,itech),IYR = 1,max_yr) 
!     write(6,*) 'dh5out', ARESID(ires)
     
       read (aresid(ires)(10:11),'(i2)') ac
       read (aresid(ires)(3:4),'(a2)')   st1
       iRRC = int(ARRC(ires))
       IR = aregion(ires)
       call dist_match(st1,irrc,IR,dist)
!==========================> HSM Code Start <==========================
!    WRITE(hsm_out,'(*(G0.16,:,","))') ires , aresid(ires), dist
!===========================> HSM Code End <===========================

      AOOIP(ires) = aorgooip(ires)*anwelloil(ires)

      bngl = angl(ires)

!========================================
!*** ASSIGN SULFUR CONTENT AND API GRAVITY IF MISSING
      IF (aapi(ires) > 60.) aapi(ires) = 60.
      IF (ac > 17.and.aapi(ires) <= 10.) aapi(ires) = 52.
        IF (ac <= 9.and.asulfoil(ires) <= 0.) then
         IF (st1 == 'AL') asulfoil(ires) = 2.41
         IF (st1 == 'CA') asulfoil(ires) = 1.41
         IF (st1 == 'CO') asulfoil(ires) = 0.16
         IF (st1 == 'KS') asulfoil(ires) = 0.24
         IF (st1 == 'OK') asulfoil(ires) = 0.30
         IF (st1 == 'TX') asulfoil(ires) = 0.22
        ENDIF
        IF (asulfoil(ires) <= 0) then
          SELECT CASE (aprov(ires))
            CASE (20,21,22,36,38,39,40,45,47,55,58,60,62)
              asulfoil(ires) = 0.2
            CASE (10,18,19,27,28,31,43,44,46,51,52,53,56,59,61,63,64,67)
              asulfoil(ires) = 0.4
            CASE (6,7,8,11,12,13,14,33,34,35,49,50,65)
              asulfoil(ires) = 1.2
            CASE DEFAULT
              asulfoil(ires) = 0.2
          END SELECT
        ENDIF
        IF (aapi(ires) <= 0) then
          SELECT CASE (aprov(ires))
            CASE (6,13)
              aapi(ires) = 25.
            CASE (38,67)
              aapi(ires) = 35.
            CASE DEFAULT
              aapi(ires) = 45.
          END SELECT
        ENDIF
      if (aapi(ires)  >  0.0)  acrdheat(ires) = 158886*exp(-0.0043*AAPI(ires))*42*.000001       ! Crude Heat Content Btu per barrel

!========================================
!     ASSIGN CRUDE TYPE
!     ======================================== 
!     ========================================
!     ==== 1=Light Sweet          === U.S. ===
!     ==== 2=Light Sour
!     ==== 3=Medium Medium Sour   === U.S. ===
!     ==== 4=Medium Sour          === U.S. ===
!     ==== 5=Heavy Sweet
!     ==== 6=Heavy Sour
!     ==== 7=California           === U.S. ===
!     ==== 8=Syncrude
!     ==== 9=DilBit/SynBit
!     ====10=Ultra Light Sweet
!     ====11=Condensates
!     ========================================

      ACRDTYPE(ires)=0
      IF (asulfoil(ires) >= 0.0 .and. aapi(ires) > 0.0 .and.aapi(ires)  <=  70.0) then
         IF (ST1 == 'CA' .or. ST1 == 'ca' .or. ST1 == 'Ca') then
            ACRDTYPE(ires)=7
            WRITE(BUGOUT,*) 'CRUDETYPE 7 Calif   : API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
         ELSEIF (AAPI(ires)  <  27.0) THEN
            IF (ASULFOIL(IRES)  <  1.1) THEN
               ACRDTYPE(ires)=5
               WRITE(BUGOUT,*) 'CRUDETYPE 5 not US  : API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
            ELSE
               ACRDTYPE(ires)=6
               WRITE(BUGOUT,*) 'CRUDETYPE 6,9 not US: API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
            ENDIF
         ELSEIF (AAPI(ires)  <  35.0) THEN
            IF (ASULFOIL(IRES)  <  1.1) THEN
               ACRDTYPE(ires)=3
               WRITE(BUGOUT,*) 'CRUDETYPE 3 US      : API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
            ELSE
               ACRDTYPE(ires)=4
               WRITE(BUGOUT,*) 'CRUDETYPE 4 US      : API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
            ENDIF
         ELSEIF (AAPI(ires)  <  40.0) THEN
            IF (ASULFOIL(IRES)  <  0.5) THEN
               ACRDTYPE(ires)=1
               WRITE(BUGOUT,*) 'CRUDETYPE 1 US      : API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
            ELSE
               ACRDTYPE(ires)=2
               WRITE(BUGOUT,*) 'CRUDETYPE 2 not US  : API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
            ENDIF
         ELSEIF (AAPI(ires)  <  50.0) THEN
            IF (ASULFOIL(IRES)  <  0.5) THEN
               ACRDTYPE(ires)=10
               WRITE(BUGOUT,*) 'CRUDETYPE 10 US      : API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
            ELSE
               ACRDTYPE(ires)=2
               WRITE(BUGOUT,*) 'CRUDETYPE 2 not US  : API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
            ENDIF
         ELSE
            IF (ASULFOIL(IRES)  <  0.5) THEN
               ACRDTYPE(ires)=11
               WRITE(BUGOUT,*) 'CRUDETYPE 11 US      : API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
            ELSE
               ACRDTYPE(ires)=2
               WRITE(BUGOUT,*) 'CRUDETYPE 2 not US  : API, SULF ', IRES, AAPI(ires), ASULFOIL(ires)
            ENDIF
         ENDIF
      endif
      if(ACRDTYPE(ires) == 0) WRITE (ogbug1,*) 'WARNING: CRUDE TYPE NOT ASSIGNED, ',aresid(ires),aapi(ires),asulfoil(ires)

!========================================
!*** ASSIGN PADD REGION
       APADD(ires) = 0
      IF(st1 == 'AL' .and. ARRC(ires) <= 0) APADD(ires) = 5   !Map states to PADD regions
      IF(st1 == 'AL' .and. ARRC(ires) == 1) APADD(ires) = 4   
      IF(st1 == 'AK') APADD(ires) = 8
      IF(st1 == 'AZ') APADD(ires) = 8
      IF(st1 == 'AR') APADD(ires) = 5
      IF(st1 == 'CA') APADD(ires) = 7
      IF(st1 == 'CO') APADD(ires) = 6
      IF(st1 == 'CT'.or.st1 == 'ME'.or.st1 == 'NH' &
         .or.st1 == 'VT'.or.st1 == 'MA'.or.st1 == 'RI') APADD(ires) = 1
      IF(st1 == 'DE') APADD(ires) = 1
      IF(st1 == 'DC') APADD(ires) = 1
      IF(st1 == 'FL') APADD(ires) = 1
      IF(st1 == 'GA') APADD(ires) = 1
      IF(st1 == 'ID') APADD(ires) = 6
      IF(st1 == 'IL') APADD(ires) = 3
      IF(st1 == 'IN') APADD(ires) = 3
      IF(st1 == 'IA') APADD(ires) = 2
      IF(st1 == 'KS') APADD(ires) = 2
      IF(st1 == 'KY') APADD(ires) = 2
      IF(st1 == 'LA' .and. ARRC(ires) <= 0) APADD(ires) = 5   
      IF(st1 == 'LA' .and. ARRC(ires) == 1) APADD(ires) = 4   
      IF(st1 == 'MD') APADD(ires) = 1
      IF(st1 == 'MI') APADD(ires) = 3
      IF(st1 == 'MN') APADD(ires) = 3
      IF(st1 == 'MS' .and. ARRC(ires) <= 0) APADD(ires) = 5   
      IF(st1 == 'MS' .and. ARRC(ires) == 1) APADD(ires) = 4   
      IF(st1 == 'MO') APADD(ires) = 2
      IF(st1 == 'MT') APADD(ires) = 6
      IF(st1 == 'NE') APADD(ires) = 2
      IF(st1 == 'NV') APADD(ires) = 8
      IF(st1 == 'NJ') APADD(ires) = 1
      IF(st1 == 'NM') APADD(ires) = 5
      IF(st1 == 'NY') APADD(ires) = 1
      IF(st1 == 'NC') APADD(ires) = 1
      IF(st1 == 'ND') APADD(ires) = 2
      IF(st1 == 'OH') APADD(ires) = 3
      IF(st1 == 'OK') APADD(ires) = 2
      IF(st1 == 'OR') APADD(ires) = 8
      IF(st1 == 'PA') APADD(ires) = 1
      IF(st1 == 'SC') APADD(ires) = 1
      IF(st1 == 'SD') APADD(ires) = 2
      IF(st1 == 'TN') APADD(ires) = 2
      IF(st1 == 'TX'.AND.IRRC == 1) APADD(ires) = 5
      IF(st1 == 'TX'.AND.IRRC == 2) APADD(ires) = 4
      IF(st1 == 'TX'.AND.IRRC == 3) APADD(ires) = 4
      IF(st1 == 'TX'.AND.IRRC == 4) APADD(ires) = 4
      IF(st1 == 'TX'.AND.IRRC == 5) APADD(ires) = 5
      IF(st1 == 'TX'.AND.IRRC == 6) APADD(ires) = 5
      IF(st1 == 'TX'.AND.IRRC == 72) APADD(ires) = 5
      IF(st1 == 'TX'.AND.IRRC == 73) APADD(ires) = 5
      IF(st1 == 'TX'.AND.IRRC == 8)  APADD(ires) = 5
      IF(st1 == 'TX'.AND.IRRC == 81) APADD(ires) = 5
      IF(st1 == 'TX'.AND.IRRC == 9)  APADD(ires) = 5
      IF(st1 == 'TX'.AND.IRRC == 10) APADD(ires) = 5
      IF(st1 == 'UT') APADD(ires) = 6
      IF(st1 == 'VA') APADD(ires) = 1
      IF(st1 == 'WA') APADD(ires) = 8
      IF(st1 == 'WV') APADD(ires) = 1
      IF(st1 == 'WI') APADD(ires) = 3
      IF(st1 == 'WY') APADD(ires) = 6
      if(APADD(ires) == 0) WRITE (ogbug1,*) 'WARNING: PADD NOT ASSIGNED, ',aresid(ires),st1,iRRC

!========================================
!?    IF(AC >= 0.AND.AC < 10)THEN
! SZ change 7/25/16 already read
!            READ(wlres,*)(APRODOIL(IRES,IYR,itech),IYR = 1,max_yr-1)   
!            READ(wlres,*)(APRODGAS(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(aprodco2(ires,iyr,itech),iyr = 1,max_yr-1)
!            READ(wlres,*)(APRODWAT(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(AWATINJ(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(AINJINJ(IRES,IYR,itech),IYR = 1,max_yr-1)
!oldadj  do iyr = 1,max_yr-1
!           if(aresflag(ires) == 7.and.iyr > 1) aprodoil(ires,iyr,itech) = aprodoil(ires,iyr-1,itech)*0.995
!           if(aresflag(ires) == 7.and.iyr > 1) aprodgas(ires,iyr,itech) = aprodgas(ires,iyr-1,itech)*0.995
!           if(ac == 0.and.(aresid(ires) == 'DOLA6050800'.or.  &
!                             aresid(ires) == 'DOLA6040900'.or.  &
!                             aresid(ires) == 'DOLA6034900'.or.  &
!                             aresid(ires) == 'DOLA7184000'.or.  &
!                             aresid(ires) == 'DOTX6053800'.or.  &
!                             aresid(ires) == 'DOTX7366900')) then
!                 aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)/10.
!           endif
!           if(ac == 0.and.(aresid(ires) == 'DOMI6209000'.or.aresid(ires) == 'DOKY7145100')) then
!                 aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*10.
!           endif
!        end do                                                                      
!?    ELSEIF(AC == 10)THEN
! SZ change 7/25/16 already read
!            READ(wlres,*)(APRODOIL(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(APRODGAS(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(aprodco2(ires,iyr,itech),iyr = 1,max_yr-1)
!            READ(wlres,*)(APRODWAT(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(APRODWAT(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(AWATINJ(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(AINJINJ(IRES,IYR,itech),IYR = 1,max_yr-1)
!?    ELSE
! SZ change 7/25/16 already read         
!            READ(wlres,*)(APRODOIL(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(APRODGAS(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(APRODWAT(IRES,IYR,itech),IYR = 1,max_yr-1)
!            READ(wlres,*)(AINJINJ(IRES,IYR,itech),IYR = 1,max_yr-1)
            do iyr = 1,max_yr-1                                                         
              aprodco2(ires,iyr,itech) = aprodgas(ires,iyr,itech)*aco2cont(ires)/100.0  
              aprodngl(ires,iyr,itech) = aprodgas(ires,iyr,itech)*angl(ires)*0.001
            end do                                                                      
!?        ENDIF

          eligible(ires) = .true.
          timed(ires) = .false.
          aggregated(ires) = .false.
          industrial(ires) = .false.
          frac(ires)       = .false.

          restemp = '           '
          restemp = aresid(ires)
          read (restemp(10:11),'(i2)') apcode(ires)
          ac = apcode(ires)

      IF (ac == 10.or.ac == 17) then  ! ADJUST PRODUCTION PROFILES TO ACCOUNT FOR HYDRAULIC FRACTURING
         if (aperm(ires) < 50.) then
              rffact = 1.0 + 0.0584*exp(-0.0156*APERM(ires))
            else
              rffact = 1.0
            endif
         if (ogrunop(2) == 13) rffact = 1.0    ! NO HYDRAULIC FRACTURING CASE
            do IYR =1,max_yr-1
              aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,1)*rffact
              aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,1)*rffact
            ENDDO
          ENDIF

! ADJUST RESOURCES IN COLORADO TO ACCOUNT FROM INCREASE IN SETBACK REQUIREMENTS (2020)
! https://coloradonewsline.com/2020/09/15/increased-setbacks-a-ban-on-drilling-a-new-study-shows-otherwise/
      IF (st1=='CO' .and. (ac==10.or.ac>=16)) atotpat(ires,itech) = atotpat(ires,itech)*0.9


! ADJUST SHALE GAS RESOURCES FOR HIGH/LOW CASES
      IF (ac == 21) then
         IF (ogrunop(2) == 15) then
              atotpat(ires,itech) = atotpat(ires,itech)*(1+ogrunop(11)/100.)
            ENDIF
         IF (ogrunop(2) == 16) then
              atotpat(ires,itech) = atotpat(ires,itech)*(1-ogrunop(11)/100.)
            ENDIF
         IF (ogrunop(2) == 18.or.ogrunop(2) == 20) then
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1+ogrunop(11)/100.)
              ENDDO
            ENDIF
         IF (ogrunop(2) == 19.or.ogrunop(2) == 21) then
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1-ogrunop(11)/100.)
              ENDDO
            ENDIF
          ENDIF

      IF (ac == 20) then  
         IF (ogrunop(2) == 15) then  
              anumacc(ires) = anumacc(ires)*(1+ogrunop(11)/100.)
            ENDIF
         IF (ogrunop(2) == 16) then  
              anumacc(ires) = anumacc(ires)*(1-ogrunop(11)/100.)
            ENDIF
         IF (ogrunop(2) == 18.or.ogrunop(2) == 20) then
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1+ogrunop(11)/100.)
              ENDDO
            ENDIF
         IF (ogrunop(2) == 19.or.ogrunop(2) == 21) then
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1-ogrunop(11)/100.)
              ENDDO
            ENDIF
          ENDIF
! ADJUST TIGHT OIL RESOURCES FOR HIGH/LOW CASES
      IF (ac == 17) then   ! only include unknown/undiscovered tight oil resource in high resource case
         IF (ogrunop(2) == 20) then
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1+ogrunop(11)/100.)
              ENDDO
            ENDIF
         IF (ogrunop(2) == 21) then
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1-ogrunop(11)/100.)
              ENDDO
            ENDIF
          ENDIF

!! ADJUST TIGHT & SHALE OIL & GAS RESOURCES FOR HIGH/LOW TRR CASES
      IF (ac == 17.or.ac == 18.or.ac == 20.or.ac == 21.or.ac == 22) THEN
         IF (ogrunop(2) == 22.or.ogrunop(2) == 30) then   ! high trr
            IF(aplay_cde(ires)<9900) then
!             sizmin = max(alatlen(ires,1)/3000.*40.,40.)
!             if(apatsiz(ires) > sizmin) atotpat(ires,itech) = min(atotpat(ires,itech)/(1-ogrunop(14)/100.),  &
!                                                               atotpat(ires,itech)*apatsiz(ires)/sizmin)
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1+ogrunop(13)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1+ogrunop(13)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1+ogrunop(13)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1+ogrunop(13)/100.)
              ENDDO
            ENDIF
         ENDIF
         IF (ogrunop(2) == 23) then   ! low EUR
            IF(aplay_cde(ires)<9900) then
!             atotpat(ires,itech) = atotpat(ires,itech)*(1-ogrunop(14)/100.)
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1-ogrunop(13)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1-ogrunop(13)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1-ogrunop(13)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1-ogrunop(13)/100.)
              ENDDO
            ENDIF
         ENDIF
         IF (ogrunop(2) == 24) then   ! high EUR
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1+ogrunop(11)/100.)
              ENDDO
            ENDIF
         IF (ogrunop(2) == 25) then   ! high wls
              atotpat(ires,itech) = atotpat(ires,itech)*(1+ogrunop(11)/100.)
            ENDIF
          ENDIF
! ADJUST OIL & GAS RESOURCES FOR HIGH/LOW TRR CASES
      IF (ac >= 16.or.(ac >= 2.and.ac <= 10).or.(ac == 0.and.aresflag(ires) == 9)) then  
         IF (ogrunop(2) == 28) then   ! very high trr
            if (ac > 16.and.apatsiz(ires) > 80) atotpat(ires,itech) = atotpat(ires,itech)*apatsiz(ires)/80.
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1+ogrunop(11)/100.)
              ENDDO
            ENDIF
          ENDIF
! ADJUST OIL & GAS RESOURCES FOR HIGH/LOW TRR CASES
      IF (ogrunop(2) == 29) then   ! very very high trr
         IF (ac == 17.or.ac == 18.or.ac == 20.or.ac == 21.or.ac == 22) THEN
            if (ac >= 17.and.apatsiz(ires) > ogrunop(14).and.ogrunop(14) > 0) then
                atotpat(ires,itech) = atotpat(ires,itech)*apatsiz(ires)/(ogrunop(14)*1.)
              endif
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1+ogrunop(11)/100.)
              ENDDO
            ENDIF
         IF(ac == 3) then
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1+ogrunop(11)/100.*0.5)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1+ogrunop(11)/100.*0.5)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1+ogrunop(11)/100.*0.5)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1+ogrunop(11)/100.*0.5)
              ENDDO
            ENDIF
          ENDIF

! ADJUST TIGHT OIL RESOURCES ONLY FOR HIGH/LOW CASES
      IF (ac == 17) then  
         IF (ogrunop(2) == 26) then
            if (apatsiz(ires) > 80) atotpat(ires,itech) = atotpat(ires,itech)*apatsiz(ires)/80.
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1+ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1+ogrunop(11)/100.)
              ENDDO
            ENDIF
         IF (ogrunop(2) == 27) then
              do IYR =1,max_yr-1
                aprodgas(ires,iyr,itech) = aprodgas(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodoil(ires,iyr,itech) = aprodoil(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodngl(ires,iyr,itech) = aprodngl(ires,iyr,itech)*(1-ogrunop(11)/100.)
                aprodco2(ires,iyr,itech) = aprodco2(ires,iyr,itech)*(1-ogrunop(11)/100.)
              ENDDO
            ENDIF
          ENDIF

! Exclude phantom plays if not the high resource case
      IF (ogrunop(2) /= 30.and.(aplay_cde(ires) >= 9900.and.aplay_cde(ires) <= 9999)) then    ! exclude phantom plays if not the high resource case
            APRODOIL(ires,:,:)  = 0.0
            APRODGAS(ires,:,:)  = 0.0
            APRODNGL(ires,:,:)  = 0.0
            aresacc(ires) = 3
          ENDIF

! REPORTING EUR AND TRR
      IF (aresacc(ires) > 3) THEN   ! exclude resources in areas where drilling is officially prohibited
            EUR_GAS(ires,itech) = 0.
            EUR_OIL(ires,itech) = 0.
         IF(ac <= 10.or.ac == 17)then
              do IYR =1,max_yr-1
                  EUR_OIL(ires,itech) = EUR_OIL(ires,itech) + APRODOIL(IRES,IYR,ITECH)
                  EUR_GAS(ires,itech) = EUR_GAS(ires,itech) + aprodgas(ires,iyr,itech)
              ENDDO
           ELSE
              do IYR =1,max_yr-1
                  EUR_OIL(ires,itech) = EUR_OIL(ires,itech) + APRODOIL(IRES,IYR,ITECH)
                  EUR_GAS(ires,itech) = EUR_GAS(ires,itech) + aprodgas(ires,iyr,itech)
              ENDDO
           ENDIF
         IF(ac == 0.or.ac == 11.or.ac == 12.or.ac == 13.or.ac == 14.or.ac == 15) then   ! decline 
              TRR_GAS(ires,itech) = EUR_GAS(ires,itech)
              TRR_OIL(ires,itech) = EUR_OIL(ires,itech)
         ELSEIF(ac == 10.or.(ac >= 16.and.ac <= 20)) then
              TRR_GAS(ires,itech) = EUR_GAS(ires,itech) * ATOTPAT(ires,itech) * ANUMACC(ires)
              TRR_OIL(ires,itech) = EUR_OIL(ires,itech) * ATOTPAT(ires,itech) * ANUMACC(ires)
            ELSE
              TRR_GAS(ires,itech) = EUR_GAS(ires,itech) * ATOTPAT(ires,itech)
              TRR_OIL(ires,itech) = EUR_OIL(ires,itech) * ATOTPAT(ires,itech)
            ENDIF
            TRR_NGL(ires,itech) = TRR_GAS(ires,itech)*angl(ires)*0.001
            EUR_NGL(ires,itech) = EUR_GAS(ires,itech)*angl(ires)*0.001
         if(itech == 1) WRITE(ogbug1,1331) 'trr_info', ac, aplay_cde(ires), atotpat(ires,1), &
                eur_GAS(ires,1)/1000.,trr_GAS(ires,1)/1000., eur_OIL(ires,1)/1000.,trr_OIL(ires,1)/1000., &
                eur_NGL(ires,1)/1000.,trr_NGL(ires,1)/1000.,aresflag(ires),aregion(ires),st1,aresid(ires), &
                apatsiz(ires),anumacc(ires),atotacres(ires),aapi(ires),acrdtype(ires),aregion(ires),apadd(ires)

1331  format(a8,2x,i2,2x,i8,2x,7(f10.3,2x),i2,i3,2x,a2,2x,a11,2x,f8.2,2x,f6.0,2x,f12.1,2x,f6.2,2x,3(i4))
           ENDIF

! END REPORTING EUR AND TRR

!  calculate the initial inferred reserves.                                                    
!  method:                                                                                     
!    oil - maximum, of all applicable EOR/ASR processes, technical production for a reservoir  
!    gas - 40 year technical production for decline gas reservoirs                             
      if(itech == 1) then                                                                    !mc change ver 7

              rtemp = 0.0                                                                       !mc change ver 7
              rtemp2 = 0.0                                                                      !mc change ver 7
              do iyr = 1,max_yr-1                                                               !mc change ver 7
                 rtemp = rtemp + (aprodoil(ires,iyr,itech)/1000.0)                              !mc change ver 7
                 rtemp2 = rtemp2 + (aprodgas(ires,iyr,itech)/1000.0)                            !mc change ver 7
              end do                                                                            !mc change ver 7


         if(ac >= 3.and.ac <= 9) then     !oil                                                !mc change ver 7
            if(rcnt == 0) then    !assign the first reservoir read in                          !mc change ver 7
              rcnt = 1                                                                          !mc change ver 7
              read (aresid(ires)(1:9),'(a9)') rname(rcnt)                                       !mc change ver 7
              read (aresid(ires)(10:11),'(i2)') rproc(rcnt)                                     !mc change ver 7
              rreg(rcnt) = aregion(ires)                                                        !mc change ver 7
              rdepth(rcnt) = adepth(ires)                                                       !mc change ver 7
              rtemp = 0.0                                                                       !mc change ver 7
              rtemp2 = 0.0                                                                      !mc change ver 7
              do iyr = 1,max_yr-1                                                               !mc change ver 7
                 rtemp = rtemp + (aprodoil(ires,iyr,itech)/1000.0)                              !mc change ver 7
                 rtemp2 = rtemp2 + (aprodgas(ires,iyr,itech)/1000.0)                            !mc change ver 7
              end do                                                                            !mc change ver 7
              rtemp = rtemp * atotpat(ires,itech)                                               !mc change ver 7
              rtemp2 = rtemp2 * atotpat(ires,itech)                                             !mc change ver 7
              rvalue(rcnt) = rtemp                                                              !mc change ver 7
              rvalue2(rcnt) = rtemp2                                                            !mc change ver 7
             else                  !check if the reservoir has previously been read in          !mc change ver 7
              rflag = 0                                                                         !mc change ver 7
              read (aresid(ires)(1:9),'(a9)') resnum                                            !mc change ver 7
              do i = 1,rcnt                                                                     !mc change ver 7
                  if(resnum == rname(i).and.rflag == 0) then  !the reservoir previously read in   !mc change ver 7
                  rtemp = 0.0                                                                   !mc change ver 7
                  rtemp2 = 0.0                                                                  !mc change ver 7
                  do iyr = 1,max_yr-1                                                           !mc change ver 7
                    rtemp = rtemp + (aprodoil(ires,iyr,itech)/1000.0)                           !mc change ver 7
                    rtemp2 = rtemp2 + (aprodgas(ires,iyr,itech)/1000.0)                         !mc change ver 7
                  end do                                                                        !mc change ver 7
                  rtemp = rtemp * atotpat(ires,itech)                                           !mc change ver 7
                  rtemp2 = rtemp2 * atotpat(ires,itech)                                         !mc change ver 7
                     if(rtemp > rvalue(i)) then                                                   !mc change ver 7
                    rvalue(i) = rtemp                                                           !mc change ver 7
                    rvalue2(i) = rtemp2                                                         !mc change ver 7
                    read (aresid(ires)(10:11),'(i2)') rproc(i)                                  !mc change ver 7
                  end if                                                                        !mc change ver 7
                  rflag = 1                                                                     !mc change ver 7
                end if                          !the reservoir has previously been read in      !mc change ver 7
              end do                                                                            !mc change ver 7
               if(rflag == 0) then  !the reservoir has not been previously read in              !mc change ver 7
                 rcnt = rcnt + 1                                                                !mc change ver 7
                 read (aresid(ires)(1:9),'(a9)') rname(rcnt)                                    !mc change ver 7
                 read (aresid(ires)(10:11),'(i2)') rproc(rcnt)                                  !mc change ver 7
                 rreg(rcnt) = aregion(ires)                                                     !mc change ver 7
                 rdepth(rcnt) = adepth(ires)                                                    !mc change ver 7
                 rtemp = 0.0                                                                    !mc change ver 7
                 rtemp2 = 0.0                                                                   !mc change ver 7
                 do iyr = 1,max_yr-1                                                            !mc change ver 7
                    rtemp = rtemp + (aprodoil(ires,iyr,itech)/1000.0)                           !mc change ver 7
                    rtemp2 = rtemp2 + (aprodgas(ires,iyr,itech)/1000.0)                         !mc change ver 7
                 end do                                                                         !mc change ver 7
                 rtemp = rtemp * atotpat(ires,itech)                                            !mc change ver 7
                 rtemp2 = rtemp2 * atotpat(ires,itech)                                          !mc change ver 7
                 rvalue(rcnt) = rtemp                                                           !mc change ver 7
                 rvalue2(rcnt) = rtemp2                                                         !mc change ver 7
               end if                                                                           !mc change ver 7
             end if                                                                             !mc change ver 7
         elseif(ac == 10.or.ac == 17) then !undiscovered oil                                  !mc change ver 7
             rtemp = 0.0                                                                        !mc change ver 7
             rtemp2 = 0.0                                                                       !mc change ver 7
             do iyr = 1,max_yr-1                                                                !mc change ver 7
               rtemp = rtemp + (aprodoil(ires,iyr,itech)/1000.0)                                !mc change ver 7
               rtemp2 = rtemp2 + (aprodgas(ires,iyr,itech)/1000.0)                              !mc change ver 7
             end do                                                                             !mc change ver 7
             ucnt = ucnt + 1
             udepth(ucnt)  = adepth(ires)
             uname(ucnt)   = aresid(ires)
             ureg(ucnt)    = aregion(ires)
             uvalue(ucnt)  = rtemp * atotpat(ires,itech)
             uvalue2(ucnt) = rtemp2 * atotpat(ires,itech)
             rtemp = rtemp * atotpat(ires,itech) * anumacc(ires)
             rtemp2 = rtemp2 * atotpat(ires,itech) * anumacc(ires)
               undres(1,ir,1) = undres(1,ir,1) + rtemp                    !mc change ver 7
               undares(1,ir,1) = undares(1,ir,1) + rtemp2                 !mc change ver 7
         elseif(ac == 16.or.(ac >= 18.and.ac <= 23)) then !undiscovered gas                   !mc change ver 7
             rtemp = 0.0                                                                        !mc change ver 7
             rtemp2 = 0.0                                                                       !mc change ver 7
             do iyr = 1,max_yr-1                                                                !mc change ver 7
               rtemp = rtemp + (aprodoil(ires,iyr,itech)/1000.0)                                !mc change ver 7
               rtemp2 = rtemp2 + (aprodgas(ires,iyr,itech)/1000.0)                              !mc change ver 7
             end do                                                                             !mc change ver 7
             ucnt = ucnt + 1
             udepth(ucnt)  = adepth(ires)
             uname(ucnt)   = aresid(ires)
             ureg(ucnt)    = aregion(ires)
             uvalue(ucnt)  = rtemp * atotpat(ires,itech)
             uvalue2(ucnt) = rtemp2 * atotpat(ires,itech)
             rtemp  = rtemp  * atotpat(ires,itech) * anumacc(ires)
             rtemp2 = rtemp2 * atotpat(ires,itech) * anumacc(ires)
               if(ac == 16) then                         !conventional                          !mc change ver 7
                  if(adepth(ires) < 10000.0) then                                               !mc change ver 7
                   undares(3,ir,1)= undares(3,ir,1) + rtemp               !mc change ver 7
                   undres(3,ir,1) = undres(3,ir,1) + rtemp2               !mc change ver 7
                 else                                                                           !mc change ver 7
                   undares(4,ir,1) = undares(4,ir,1) + rtemp              !mc change ver 7
                   undres(4,ir,1) = undres(4,ir,1) + rtemp2               !mc change ver 7
                 end if                                                                         !mc change ver 7
               elseif(ac == 18.or.ac == 22) then         !tight gas                             !mc change ver 7
                 undares(5,ir,1) = undares(5,ir,1) + rtemp                !mc change ver 7
                 undres(5,ir,1) = undres(5,ir,1) + rtemp2                 !mc change ver 7
               elseif(ac == 19.or.ac == 23) then         !coalbed methane                       !mc change ver 7
                 undares(7,ir,1) = undares(7,ir,1) + rtemp                !mc change ver 7
                 undres(7,ir,1) = undres(7,ir,1) + rtemp2                 !mc change ver 7
               elseif(ac == 20.or.ac == 21) then         !shale gas                             !mc change ver 7
                 undares(6,ir,1) = undares(6,ir,1) + rtemp                !mc change ver 7
                 undres(6,ir,1) = undres(6,ir,1) + rtemp2                 !mc change ver 7
               end if                                                                           !mc change ver 7
           end if                                                                               !mc change ver 7
         end if                                                                                 !mc change ver 7

! end calculation of initial inferred reserves for oil                      

!  record the number of potential projects

      IF(itech == 1) then
            do IYR = 1,max_yr
              potential(ac+1,iyr) = potential(ac+1,iyr)+1
            end do
          END if

!  For oil and gas decline, the number of patterns is the same as the number
!  of existing oil/gas wells.  For other processes, the number of patterns is
!  based on the area.

      IF(ac == 3) ahcpv(ires) = 1.0                                   !HCPV of incoming data

!  aggregate the last year of historical production
!   to be used in calculating the resource coverage factors

      IF(itech == 1) then
         IF(AC == 0) then    !decline oil
           nat_hoil = nat_hoil + alyroil(ires)
            if (aplay_cde(ires) == 2804.or.(aplay_cde(ires) >= 3110.and.aplay_cde(ires) <= 3115).or.  &
               aplay_cde(ires) == 4763.or.aplay_cde(ires) == 5875.or.aplay_cde(ires) == 99926039.or.  &
               aplay_cde(ires) == 4409.or.(aplay_cde(ires) >= 4747.and.aplay_cde(ires) <= 4749).or.  &
               aplay_cde(ires) == 3904.or.aplay_cde(ires) == 3920.or.aplay_cde(ires) == 99905037.or.  &
               aplay_cde(ires) == 4473.or.aplay_cde(ires) == 4401.or.aplay_cde(ires) == 99943037.or.  &
               aplay_cde(ires) == 99949033.or.aplay_cde(ires) == 99949037.or.  &
               aplay_cde(ires) == 1201.or.aplay_cde(ires) == 99904010.or.aresflag(ires) == 9) then   ! tight oil
                 dist_hoil(dist,2) = dist_hoil(dist,2) + alyroil(ires)/365./1000.
                 dist_hadg(dist,2) = dist_hadg(dist,2) + alyrgas(ires)
           ELSE
                 dist_hoil(dist,1) = dist_hoil(dist,1) + alyroil(ires)/365./1000.
                 dist_hadg(dist,1) = dist_hadg(dist,1) + alyrgas(ires)
           ENDIF
           REG_HOIL(IR) = REG_HOIL(IR)+ALYROIL(Ires)
           REG_HADG(IR) = REG_HADG(IR)+ALYRGAS(Ires)


         ELSEIF(AC == 11.or.AC == 12.or.AC == 13.or.AC == 14.or.AC == 15) THEN    !decline gas
           nat_hoil = nat_hoil + alyroil(ires)
           nat_hgas = nat_hgas + alyrgas(ires)
            if (ac == 11.or.ac == 12) then
             dist_hgas(dist,1) = dist_hgas(dist,1) + alyrgas(ires)
             dist_hoil(dist,1) = dist_hoil(dist,1) + alyroil(ires)/365./1000.
           endif
            if (ac == 13) then
             dist_hgas(dist,2) = dist_hgas(dist,2) + alyrgas(ires)
             dist_hoil(dist,2) = dist_hoil(dist,2) + alyroil(ires)/365./1000.
           endif
            if ((ac == 14.or.ac == 15).and.(aresflag(ires) == 3.or.aresflag(ires) == 4)) then
             dist_hgas(dist,3) = dist_hgas(dist,3) + alyrgas(ires)
             dist_hoil(dist,2) = dist_hoil(dist,2) + alyroil(ires)/365./1000.
           endif
            if ((ac == 14.or.ac == 15).and.(aresflag(ires) == 1.or.aresflag(ires) == 2)) then
             dist_hgas(dist,4) = dist_hgas(dist,4) + alyrgas(ires)
             dist_hoil(dist,1) = dist_hoil(dist,1) + alyroil(ires)/365./1000.
           endif

           REG_HOIL(IR) = REG_HOIL(IR)+ALYROIL(Ires)
           REG_HGAS(IR) = REG_HGAS(IR)+ALYRGAS(Ires)
           IF(ac == 11) reg_hcnv(IR) = reg_hcnv(IR)+alyrgas(ires)
           IF(ac == 12) reg_hcnv(IR) = reg_hcnv(IR)+alyrgas(ires)
           IF(ac == 13) reg_htht(IR) = reg_htht(IR)+alyrgas(ires)
           IF(ac == 14.or.ac == 15) then                                              
             IF(aresflag(ires) == 1 .or. aresflag(ires) ==2) then     !coalbed methane
               reg_hcbm(IR) =  reg_hcbm(IR)+alyrgas(ires)
             ELSEIF(aresflag(ires) == 3 .or. aresflag(ires) ==4) THEN  !shale
               reg_hshl(IR) = reg_hshl(IR)+alyrgas(ires)
             END if                                                          
           END if                                                           

         END if
        END if

      if (ac >= 21.and.ac <= 23.and.itech == 1) then          !transfer end of year reserves
           do iyr = 1,max_yr-1
             aresvoil(ires,iyr,itech) = 0.0
             aresvgas(ires,iyr,itech) = awatinj(ires,iyr,itech)
           end do
          end if

      if (ac >= 17.and.ac <= 20.and.itech == 1) then
            do iyr = 1,max_yr-1
              aresvoil(ires,iyr,itech) = aprodco2(ires,iyr,itech)
              aresvgas(ires,iyr,itech) = awatinj(ires,iyr,itech)
            end do
          end if

      IF (ac == 3) then
            do iyr=1,max_yr
                ainjinj(ires,iyr,itech) = ainjinj(ires,iyr,itech) / 1.2

!               Assign CO2 Production = CO2 Recycled based on Total CO2 Injection

                iprann(iyr) = (0.00003*(iyr**3))+(-0.0025*(iyr**2))+(0.0615*iyr)
            if(iyr  <=  2) then
                   ainjrecy(ires,iyr,itech) = 0.0
                   aprodco2(ires,iyr,itech) = 0.0
                else
                   ainjrecy(ires,iyr,itech) = iprann(iyr) * ainjinj(ires,iyr,itech) 
                   aprodco2(ires,iyr,itech) = iprann(iyr) * ainjinj(ires,iyr,itech) 
                end if

            end do
          END IF

      if (ac >= 16) then
            patdev(ires,1,itech) = atotpat(ires,itech)
          end if

!  end resource data debug file

      if (ac <= 10.or.ac == 17) then
            ares_type(ires) = 'O'
          else
            ares_type(ires) = 'G'
          end if

      if (atotpat(ires,itech) <= 1.0) atotpat(ires,itech) = 1.0

!  store the technical production
      if (itech == 1) then
            do iyr = 1,max_yr-1
            if (ac >= 1.and.ac <= 10.or.ac == 16)then
               tech_oil(ac+1,iyr) = tech_oil(ac+1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
               tech_gas(ac+1,iyr) = tech_gas(ac+1,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               tech_ngl(ac+1,iyr) = tech_ngl(ac+1,iyr) + aprodngl(ires,iyr,itech)*atotpat(ires,itech)
               IF(ac == 1) then ! primary
                 tech_primary(IR,1,iyr) = tech_primary(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_primary(IR,2,iyr) = tech_primary(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               ELSEIF(ac == 2) then ! waterflood
                 tech_wtrfld(IR,1,iyr) = tech_wtrfld(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_wtrfld(IR,2,iyr) = tech_wtrfld(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               ELSEIF(ac == 3) then ! co2 flood
                 tech_co2fld(IR,1,iyr) = tech_co2fld(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_co2fld(IR,2,iyr) = tech_co2fld(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               ELSEIF(ac == 4) then ! steamflood
                 tech_stmfld(IR,1,iyr) = tech_stmfld(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_stmfld(IR,2,iyr) = tech_stmfld(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               ELSEIF(ac == 5) then ! polymer flood
                 tech_plyfld(IR,1,iyr) = tech_plyfld(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_plyfld(IR,2,iyr) = tech_plyfld(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
                  ELSEIF(ac == 6) then ! infill drilling
                 tech_infill(IR,1,iyr) = tech_infill(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_infill(IR,2,iyr) = tech_infill(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               ELSEIF(ac == 7) then ! profile modification
                 tech_prfmod(IR,1,iyr) = tech_prfmod(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_prfmod(IR,2,iyr) = tech_prfmod(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               ELSEIF(ac == 8) then ! horizontal continuity
                 tech_horcon(IR,1,iyr) = tech_horcon(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_horcon(IR,2,iyr) = tech_horcon(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               ELSEIF(ac == 9) then ! horizontal profile modification
                 tech_horprf(IR,1,iyr) = tech_horprf(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_horprf(IR,2,iyr) = tech_horprf(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               ELSEIF(ac == 10) then ! undiscovered conventional oil
                 tech_uconvo(IR,1,iyr) = tech_uconvo(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_uconvo(IR,2,iyr) = tech_uconvo(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               ELSEIF(ac == 16) then ! undiscovered conventional gas
                 tech_uconvg(IR,1,iyr) = tech_uconvg(IR,1,iyr) + aprodoil(ires,iyr,itech)*atotpat(ires,itech)
                 tech_uconvg(IR,2,iyr) = tech_uconvg(IR,2,iyr) + aprodgas(ires,iyr,itech)*atotpat(ires,itech)
               END if
              else
               tech_oil(ac+1,iyr) = tech_oil(ac+1,iyr) + aprodoil(ires,iyr,itech)
               tech_gas(ac+1,iyr) = tech_gas(ac+1,iyr) + aprodgas(ires,iyr,itech)
               tech_ngl(ac+1,iyr) = tech_ngl(ac+1,iyr) + aprodngl(ires,iyr,itech)
               if (ac == 14) then     ! wet coal/shale
                  if (aresflag(ires) == 3) then
                     tech_shale(IR,1,iyr) = tech_shale(IR,1,iyr) + aprodoil(ires,iyr,itech)
                  ELSEIF(aresflag(ires) == 2) then
                     tech_coal(IR,1,iyr) = tech_coal(IR,1,iyr) + aprodoil(ires,iyr,itech)
                  end if
                  if (aresflag(ires) == 3) then
                     tech_shale(IR,2,iyr) = tech_shale(IR,2,iyr) + aprodgas(ires,iyr,itech)
                  ELSEIF(aresflag(ires) == 2) then
                     tech_coal(IR,2,iyr) = tech_coal(IR,2,iyr) + aprodgas(ires,iyr,itech)
                  end if
               ELSEIF(ac == 0) then   ! decline oil
                    tech_decline(IR,1,iyr) = tech_decline(IR,1,iyr) + aprodoil(ires,iyr,itech)
                    tech_decline(IR,2,iyr) = tech_decline(IR,2,iyr) + aprodgas(ires,iyr,itech)
               ELSEIF(ac == 15) then  ! dry coal/shale
                  if (aresflag(ires) == 4) then
                     tech_shale(IR,1,iyr) = tech_shale(IR,1,iyr) + aprodoil(ires,iyr,itech)
                  ELSEIF(aresflag(ires) == 1) then
                     tech_coal(IR,1,iyr) = tech_coal(IR,1,iyr) + aprodoil(ires,iyr,itech)
                  end if
                  if (aresflag(ires) == 4) then
                     tech_shale(IR,2,iyr) = tech_shale(IR,2,iyr) + aprodgas(ires,iyr,itech)
                  ELSEIF(aresflag(ires) == 1) then
                     tech_coal(IR,2,iyr) = tech_coal(IR,2,iyr) + aprodgas(ires,iyr,itech)
                  end if
               ELSEIF(ac == 13) then  ! tight sands
                  tech_tightg(IR,1,iyr) = tech_tight(IR,1,iyr) + aprodoil(ires,iyr,itech)
                  tech_tightg(IR,2,iyr) = tech_tight(IR,2,iyr) + aprodgas(ires,iyr,itech)
               ELSEIF(ac == 21) then  ! develolping shale
                    tech_ugshale(IR,1,iyr) = tech_ugshale(IR,1,iyr) + aprodoil(ires,iyr,itech)
                    tech_ugshale(IR,2,iyr) = tech_ugshale(IR,2,iyr) + aprodgas(ires,iyr,itech)
               ELSEIF(ac == 22) then  ! developing tight gas
                    tech_ugtight(IR,1,iyr) = tech_ugtight(IR,1,iyr) + aprodoil(ires,iyr,itech)
                    tech_ugtight(IR,2,iyr) = tech_ugtight(IR,2,iyr) + aprodgas(ires,iyr,itech)
               ELSEIF(ac == 23) then  ! developing coalbed methane
                     tech_ugcoal(IR,1,iyr) = tech_ugcoal(IR,1,iyr) + aprodoil(ires,iyr,itech)
                     tech_ugcoal(IR,2,iyr) = tech_ugcoal(IR,2,iyr) + aprodgas(ires,iyr,itech)
               ELSEIF(ac == 11) then  ! conventional radial
                     tech_radial(IR,1,iyr) = tech_radial(IR,1,iyr) + aprodoil(ires,iyr,itech)
                     tech_radial(IR,2,iyr) = tech_radial(IR,2,iyr) + aprodgas(ires,iyr,itech)
               ELSEIF(ac == 12) then  ! water drive
                     tech_water(IR,1,iyr) = tech_water(IR,1,iyr) + aprodoil(ires,iyr,itech)
                     tech_water(IR,2,iyr) = tech_water(IR,2,iyr) + aprodgas(ires,iyr,itech)
               ELSEIF(ac == 17) then  ! undiscovered unconventional oil
                 tech_uconto(IR,1,iyr) = tech_uconto(IR,1,iyr) + aprodoil(ires,iyr,itech)
                 tech_uconto(IR,2,iyr) = tech_uconto(IR,2,iyr) + aprodgas(ires,iyr,itech)
               ELSEIF(ac == 18) then  ! undiscovered tight gas
                 tech_utight(IR,1,iyr) = tech_utight(IR,1,iyr) + aprodoil(ires,iyr,itech)
                 tech_utight(IR,2,iyr) = tech_utight(IR,2,iyr) + aprodgas(ires,iyr,itech)
               ELSEIF(ac == 19) then ! undiscovered coalbed methane
                 tech_ushale(IR,1,iyr) = tech_ushale(IR,1,iyr) + aprodoil(ires,iyr,itech)
                 tech_ushale(IR,2,iyr) = tech_ushale(IR,2,iyr) + aprodgas(ires,iyr,itech)
               ELSEIF(ac == 20) then ! undiscovered shale gas
                 tech_ucoalb(IR,1,iyr) = tech_ucoalb(IR,1,iyr) + aprodoil(ires,iyr,itech)
                 tech_ucoalb(IR,2,iyr) = tech_ucoalb(IR,2,iyr) + aprodgas(ires,iyr,itech)
               end if
              end if
             end do
          end if

          do iyr =1,max_yr-1
             ainjrecy(ires,iyr,itech) =aprodco2(ires,iyr,itech)
          end do

        RETURN              !returns to the read_resource routine if file end is NOT reached

100   continue
        iflag = 1
101   continue

        RETURN              !returns to the read_resource routine if file end is reached

1     FORMAT (A30,<max_yr-1>(2X,F10.3))
23    format (a11,2x,i8,2x,f10.3,2x,i1,26(2x,f10.3),2x,f12.3,2x, &
          f10.3,2x,f16.3,2x,i1,2x,i2,2x,f12.3,2x,i2,2x,i2,5(2x,f10.3),2X,F10.3)

        END subroutine


!***********************************************************************************
!***********************************************************************************
!***********************************************************************************
!***********************************************************************************
        SUBROUTINE READ_DISCORD                          
                                                        
! READS THE DISCOVERY ORDER FOR ALL RESERVOIRS/PROJECTS
        IMPLICIT NONE                                 
                                                     
        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include'ogsmbfw'      ! ogsm system variables
        INCLUDE 'ogsmugr'                           
        include 'ogsml48'                          
                                                  
        CALL OGSM_NEXTDATA(WLRES)
        DO I = 1,MAX_UND        
          READ (WLRES,12,END=13) UND_RESID(I),UND_PLAY(I),UND_ITEM(I),UND_COUNTER(I)                                              
          UND_COUNTER(i) = ex_fcres+i-1
        END DO                                                       
12    FORMAT (a11,3x,i8,3x,i5,3x,i5)                                
                                                                   
13    CONTINUE                                                    

        NUMDISC = I-1                                            
        WRITE(ogbug1,*) 'numdisc ', numdisc


        END SUBROUTINE

!***************************************************************
       SUBROUTINE INIT_AGG

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       INCLUDE 'ogsml48'
       INCLUDE 'ogsmeor'
       INCLUDE 'ogsmugr'
       INCLUDE 'ogsmout'

       INTEGER iwell                                       ! well class
       INTEGER iproc                                       ! process number
       INTEGER IR                                          ! OGSM region number
       INTEGER ifuel                                       ! fuel type
       INTEGER isrc                                        ! CO2 source
       INTEGER DIST                                        ! DISTRICTS FOR PMM GAS PROD
       INTEGER iyr
       INTEGER t  ! crude type index


       DO IYR = L48HYR, MNUMYR
         if(iyr > l48hyr) ogqshloil(:,iyr) = 0.
         if(iyr > l48hyr) ogqshlgas(:,iyr) = 0.
        NGPLPRD(:,iyr) = 0.
        NGPLPRDET(:,iyr) = 0.
        NGPLPRDPR(:,iyr) = 0.
        NGPLPRDBU(:,iyr) = 0.
        NGPLPRDIS(:,iyr) = 0.
        NGPLPRDPP(:,iyr) = 0.
        REPPRDAD(:,:,iyr) = 0.
        DO IR = 1, L48RGN
         ADRESAD(IR,IYR)             = 0.0
         DO IFUEL = 1, L48FUEL
           REPPRDL48(IR,IFUEL,IYR)   = 0.0
           REPREVL48(IR,IFUEL,IYR)   = 0.0
           REPNRDL48(IR,IFUEL,IYR)   = 0.0
           REPEXTL48(IR,IFUEL,IYR)   = 0.0
           REPNFWL48(IR,IFUEL,IYR)   = 0.0
           REPPRDWLL48(IR,IFUEL,IYR) = 0.0
           DO IWELL = 1, L48WELL
             REPSUCWLL48(IWELL,IR,IFUEL,IYR) = 0.0
             REPDRYWLL48(IWELL,IR,IFUEL,IYR) = 0.0
             REPOPERL48(IWELL,IR,IFUEL,IYR)  = 0.0
             REPDRILLL48(IWELL,IR,IFUEL,IYR) = 0.0
             REPDRYL48(IWELL,IR,IFUEL,IYR)   = 0.0
             REPLEASL48(IWELL,IR,IFUEL,IYR)  = 0.0
             REPDCFL48(IWELL,IR,IFUEL,IYR)   = 0.0
           ENDDO
         ENDDO
         DO ISRC = 1, MAX_SRC
           EORProduction(IR,isrc,iyr)      = 0.0
           GasProduction(IR,isrc,iyr)      = 0.0
           CO2Injected(IR,isrc,iyr)        = 0.0
           CO2Recycled(IR,isrc,iyr)        = 0.0
           CO2Purchased(IR,isrc,iyr)       = 0.0
           CO2Purchased2(IR,isrc,iyr)       = 0.0
           CO2Purchasedb(IR,isrc,iyr)       = 0.0
           CO2Purchased45Q(IR,isrc,iyr)       = 0.0
         ENDDO
         DO IPROC = 1, MAX_PROC
           REPEORL48(IR,IPROC,IYR) = 0.0
         ENDDO
        ENDDO
        DO IFUEL = 1, MAX_FUEL
         DO DIST = 1, OGDIST
           PMMDG(DIST,IFUEL,IYR)           = 0.0
         ENDDO
        ENDDO
         DO ISRC = 1, MAX_SRC
            EORProduction(MNL48N,isrc,iyr)      = 0.0
            GasProduction(MNL48N,isrc,iyr)      = 0.0
            CO2Injected(MNL48N,isrc,iyr)        = 0.0
            CO2Recycled(MNL48N,isrc,iyr)        = 0.0
            CO2Purchased(MNL48N,isrc,iyr)       = 0.0
            CO2Purchased2(MNL48N,isrc,iyr)       = 0.0
            CO2Purchasedb(MNL48N,isrc,iyr)       = 0.0
            CO2Purchased45Q(MNL48N,isrc,iyr)       = 0.0
         ENDDO
       ENDDO

       DO IR = 1,L48RGN
         DO t=1,mncrud
           DO IYR = 20,MNUMYR
             TOTCRDPRD(IR,IYR) = 0.0
             MissedCRDPRD(IR,t,IYR)  = 0.0
             LF_CRDPRD(IR,t,IYR)  = 0.0
           ENDDO
         ENDDO
       ENDDO
       LF_CRDVOL = 0.

!      LF_CRDPRD_PADD = 0.0  ! initialized in wellogs.f
       TOTCRDPRD_PADD = 0.0  

       RETURN
       END SUBROUTINE

!***************************************************************
!     from AGG_ogsm.for
       SUBROUTINE AGG_OGSM(ires,imc)

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsmugr'
       INCLUDE 'ogsml48'
       INCLUDE 'ogsmeor'
       INCLUDE 'ogsmout'
       INCLUDE 'pmmout'
       INCLUDE 'lfmmout'

       INTEGER ires                                        !reservoir number

       INTEGER iwell                                       ! well class
       INTEGER ist                                         ! state number
       INTEGER iproc                                       ! process number
      INTEGER JR,jrgn                                     ! OGSM region number - 7 regions
      INTEGER IR,irgn                                     ! OGSM region number - 6 regions - 7 is added to 5
       INTEGER ifuel                                       ! fuel type
       INTEGER iplay                                       ! play number
       INTEGER irrc                                        ! rrc number
       INTEGER iacc                                        ! resource access category
       INTEGER iflag                                       ! resource flag
       INTEGER isrc                                        ! CO2 source
       INTEGER DIST                                        ! DISTRICTS FOR PMM GAS PROD
       INTEGER oPADD                                       ! PADD FOR LFMM CRUDE OIL PROD
       INTEGER imc,iy
      INTEGER yr(max_yr-1),y, ogsm_yr
       INTEGER rr,ss

       INTEGER iyr,mprov,jyr,kyr,jmc,jcde,umc,ucde,myr

       logical tempmarg(max_yr-1)

       REAL tempoil(max_yr-1),tempgas(max_yr-1),tempprod(max_yr-1),& 
       tempinj(max_yr-1),&                                          
       tempprodr(max_yr-1),tempinjr(max_yr-1),tempshut(max_yr-1),& 
       tempoil2(80)                                               
       REAL temperes(max_yr-1),tempgres(max_yr-1)                
       REAL o_resadd, g_resadd
       REAL tang_o(mnumyr),intang_o(mnumyr),tang_g(mnumyr),intang_g(mnumyr)
       REAL SUM_PV(MNUMOR,MNUMYR)   ! NEW ARRAY for computing wt. ave wellhead price using new LFMM prices by crude type
       REAL produced, rejected, recovered
      REAL sg
       CHARACTER*2 st1                                     ! state abbreviation
       CHARACTER*1 well
       CHARACTER*3 cat914
       character*9 jmres,imres                                  

! initialize the new array
!
! This array holds the summation of Wellhead Price * Production Volume values of each reservoir in an OGSM region by year.
!
        SUM_PV = 0.0
!

!  STEP 1: USE THE RESERVOIR IDENTIFIERS TO DETERMINE THE RESERVOIR CATEGORIES
!       WELL CLASS 1 = EXPLORATORY/UNDISCOVERED
!       WELL CLASS 2 = DEVELOPMENTAL
!       FUEL TYPE 1 = SHALLOW OIL (PROCESS CODES 00,01,03-10,17
!       FUEL TYPE 2 = CO2 FLOODING (PROCESS CODE 02)
!       FUEL TYPE 3 = 
!       FUEL TYPE 4 = CONV GAS (PROCESS CODES 11,12,16)
!       FUEL TYPE 5 = TIGHT SANDS (PROCESS CODES 13,18)
!       FUEL TYPE 6 = DEVONIAN SHALE (PROCESS CODES 14,15,19,20, IFLAG = 3,4)
!       FUEL TYPE 7 = COALBED METHANE (PROCESS CODES 14,15,19,20, IFLAG = 1,2)

! IFLAG VALUES
!       1: OTHER GAS/OIL
!       2: DRY COAL
!       3: WET COAL
!       4: WET SHALE
!       5: DRY SHALE


      read (aresid(ires)(10:11),'(i2)')apcode(ires) 
      read (aresid(ires)(10:11),'(i2)') iproc
      iproc = iproc + 1
      read (aresid(ires)(3:4),'(a2)')   st1
      read (aresid(ires)(1:1),'(a1)')   well

      JR = aregion(ires)
      IR = JR
!     IF (JR  ==  7) IR = 5     ! Northern Great Plains

      if (well == 'U'.or.well == 'u') then
         iwell = 1
      else
         iwell = 2
      end if

      isrc = 0
      if (iproc == 4) then
         isrc = eco2code(ires)
         jrgn = eco2reg(ires)                
         irgn = jrgn
!        if (jrgn  ==  7) irgn = 5     ! Northern Great Plains
      end if

      iplay = aplay_cde(ires)
      iacc  = aresacc(ires)
      irrc  = INT(arrc(ires))
      iflag = aresflag(ires)

      call dist_match(st1,irrc,JR,dist)     ! for PMM
      oPADD = aPADD(ires)

      IF (iproc  <  4 .OR. iproc  ==  7 .OR. (IPROC  >=  9 .AND. iproc  <=  11) .OR. iproc  ==  18) THEN
         ifuel=1                               ! FUELTYPE 1: CONVENTIONAL OIL
      ELSEIF ((iproc  >=  4 .AND. IPROC  <=  6) .OR. IPROC  ==  8) THEN
         ifuel=2                               ! FUELTYPE 2: EOR/ASR
      ELSEIF (iproc  ==  12 .OR. iproc  ==  13 .OR. iproc  ==  17) THEN
         ifuel=4                               ! FUELTYPE 4: CONVENTIONAL GAS
      ELSEIF (iproc  ==  14 .OR. iproc  ==  19 .or. iproc  ==  23) THEN
         ifuel=5                               ! FUELTYPE 5: TIGHT GAS
      ELSEIF (((iproc  ==  15 .OR. iproc  ==  16) .and. (iflag == 3 .or.iflag == 4)) .OR. iproc  ==  21 .or. iproc  ==  22) THEN
         ifuel=6                               ! FUELTYPE 6: GAS SHALES
      ELSEIF (((iproc  ==  15 .OR. iproc  ==  16) .and. (iflag == 1 .or.iflag == 2)) .OR. iproc  ==  20 .or. iproc  ==  24) THEN
         ifuel=7                               ! FUELTYPE 7: COALBED METHANE
      ELSE
         WRITE(ogbug1,*) 'WARNING: NO FUEL TYPE ASSIGNED ', curiyr+1989, aresid(ires)
      ENDIF

!     running count of the number of projects timed in each year
      timed_proc(iproc,timedyr(ires))=timed_proc(iproc,timedyr(ires))+1
      ntimed = ntimed + 1

      do j = 1,max_yr-1     
         tempoil(j) = 0.0   
         tempgas(j) = 0.0   
         tempprod(j) = 0.0  
         tempinj(j) = 0.0   
         tempprodr(j) = 0.0 
         tempinjr(j) = 0.0  
         tempshut(j) =0.0   
         temperes(j) = 0.0  
         tempgres(j) = 0.0  
         tempmarg(j) = .false.
         IF (j >= timedyr(ires).and.j <= max_yr) then
            tempoil(j) = eprodoil(ires,j-timedyr(ires)+1)
            tempgas(j) = eprodgas(ires,j-timedyr(ires)+1)
            tempprod(j) = eprodwell(ires,j-timedyr(ires)+1)
            tempinj(j) = einjwell(ires,j-timedyr(ires)+1)
            tempprodr(j) = eproddr(ires,j-timedyr(ires)+1)
            tempinjr(j) = einjdr(ires,j-timedyr(ires)+1)
            tempshut(j) = eshutin(ires,j-timedyr(ires)+1)
            temperes(j) = eremres(ires,j-timedyr(ires)+1)
            tempgres(j) = egremres(ires,j-timedyr(ires)+1)
            tempmarg(j) = emarg(ires,j-timedyr(ires)+1)
         END if                                       
      end do                                         

      yr(1)=l48b4yr
      DO y=2,max_yr-1
         yr(y)=yr(y-1)+1
      ENDDO

!     ***** RPT_PRODUCTION.OUT *****

         IF (APCODE(IRES) >= 21.AND.APCODE(IRES) <= 23) ELYRGAS(IRES)=0.0

         IF (AAPI(ires) <= 30.) THEN
           CAT914 = "30L"
         ELSEIF (AAPI(ires) <= 40.) THEN
           CAT914 = "43B"
         ELSEIF (AAPI(ires) <= 50.) THEN
           CAT914 = "54B"
         ELSE
           CAT914 = "50G"
         ENDIF

         IF (IRES == 1)THEN
            write (ofile1,1815)'TIMEDYR','RESID','STATE','PLAY','FUEL','CRUDETYPE','914CAT','API','SULF', &
               'REG','RRC','PROC',&
               l48b4yr-1,(yr(y),y=1,max_yr-1),l48b4yr-1,(yr(y),y=1,max_yr-1)
         ENDIF
         write (ofile1,1814) timedyr(ires),aresid(ires),st1,iplay,ifuel,acrdtype(ires),cat914, &
            aapi(ires),asulfoil(ires),ir,irrc,iproc-1,&
            elyroil(ires),(tempoil(j),j=1,max_yr-1),elyrgas(ires), &
            (tempgas(j),j=1,max_yr-1)
 1814 format (i7,3x,a11,3x,a2,3x,i8,3x,i2,8x,i3,5x,a3,3x,f5.2,3x,f5.2,2x,i3,3x,i6,3x,i6, &
         2(3x,f12.3,<max_yr-1>(3x,f12.3)))

 1815 format (a7,6x,a5,5x,a5,5x,a4,2x,a4,4x,a9,2x,a6,3x,a3,3x,a4,3x,a3,6x,a3,6x,a4,2(3x,i12,<max_yr-1>(3x,i12)))

!     ***** RPT_WELLCOUNT.OUT ******

!        IF (IRES == 1)THEN
!           write (ofile2,1816)'TIMEDYR','RESID','STATE','PLAY','FUEL','REG','PROC',&
!              l48hyr+1989,(yr(y),y=1,max_yr-1)
!        ENDIF
!        write (ofile2,1817) timedyr(ires),aresid(ires),st1,iplay,ifuel,ir,iproc-1,&
!           anwellinj(ires),(tempprodr(j),j=1,max_yr-1)
 1817 format (i7,3x,a11,3x,a2,3x,i8,3x,i2,3x,i6,3x,i6, &
         3x,f12.3,<max_yr-1>(3x,f12.3))

 1816 format (a7,6x,a5,5x,a5,5x,a4,2x,a4,4x,a3,6x,a4,11x,a4,i12,<max_yr-1>(3x,i12))

      if ((apcode(ires) > 0.and.apcode(ires) < 10).and.apcode(ires) /= 2) then 
         jcde = 0                                                             
         jmres = '  '                                                        
         read(aresid(ires)(1:9),'(a9)') jmres                               
         do jmc = 1,rcnt                                                   
            if (rname(jmc) == jmres.and.jcde == 0) then                   
               jcde = jmc 
            end if       
         end do         
      end if           

      if (apcode(ires) == 10.or.apcode(ires) == 2.or.apcode(ires) >= 16) then
         ucde = 0                                                           
         do umc = 1,ucnt                                                   
            if (uname(umc) == aresid(ires).and.ucde == 0) then            
               ucde = umc                                                
            end if                                                      
         end do                                                        
      end if                                                          

      do j = 1,max_yr-1                                              
         tempoil(j)   = 0.0                                         
         tempgas(j)   = 0.0                                        
         tempprod(j)  = 0.0                                       
         tempinj(j)   = 0.0                                      
         tempprodr(j) = 0.0                                     
         tempinjr(j)  = 0.0                                    
         if (j >= timedyr(ires).and.j <= max_yr-1)THEN         
            IF ((apcode(ires) > 0.and.apcode(ires) < 10).and.apcode(ires) /= 2) then
               tempoil(j) = rvalue(jcde)                      
               tempprodr(j)=rvalue2(jcde)                    
            ENDIF
            if (apcode(ires) == 10.or.apcode(ires) == 2.or.apcode(ires) >= 16) then
               tempprod(j)= uvalue(ucde)                    
               tempinj(j) = uvalue2(ucde)                  
            ENDIF
            tempgas(j) = eiremres(ires,j-timedyr(ires)+1)/1000.0
            tempinjr(j)= eigremres(ires,j-timedyr(ires)+1)/1000.0
         end if                                                 
      end do                                                   

!     add the technical production to the initial undiscovered resource base

      if (apcode(ires) == 2) then                                          
         do myr = 1,max_yr                                                
            undres(ifuel,ir,myr) = undres(ifuel,ir,myr) + uvalue(ucde)   
            undares(ifuel,ir,myr) = undares(ifuel,ir,myr) + uvalue2(ucde)
         end do                                                         
      end if                                                           

!     AGGREGATE AT THE REGIONAL LEVEL

      DO iyr =curiyr,lastyr
         tang_o(iyr) = omult_tang*(dcrdwhp(mnumor,iyr)*dladj/baseoil-1) + 1
         intang_o(iyr) = omult_int*(dcrdwhp(mnumor,iyr)*dladj/baseoil-1) + 1
         tang_g(iyr) = gmult_tang*(dcrdwhp(mnumor,iyr)*dladj/baseoil-1) + 1
         intang_g(iyr) = gmult_int*(dcrdwhp(mnumor,iyr)*dladj/baseoil-1) + 1
         jyr = (iyr+baseyr-1) - l48b4yr + 1 - (timedyr(ires)-1)
         IF (timedyr(ires) > 0.and.jyr >= 1.and.jyr <= max_yr) THEN      !eor/asr processes
            if (apcode(ires) > 2.and.apcode(ires) < 10) then          
               infresv(ifuel,ir,iyr-l48b4yr+baseyr) = infresv(ifuel,ir,iyr-l48b4yr+baseyr) - rvalue(jcde) + (eiremres(ires,jyr)/1000.0)
               infarsv(ifuel,ir,iyr-l48b4yr+baseyr) = infarsv(ifuel,ir,iyr-l48b4yr+baseyr) - rvalue2(jcde) + (eigremres(ires,jyr)/1000.0)
               if (infresv(ifuel,ir,iyr-l48b4yr+baseyr) <= 0.0) infresv(ifuel,ir,iyr-l48b4yr+baseyr) = 0.0                             
               if (infarsv(ifuel,ir,iyr-l48b4yr+baseyr) <= 0.0) infarsv(ifuel,ir,iyr-l48b4yr+baseyr) = 0.0                            
               if (iyr == lastyr) then                                                                        
                  do myr=iyr-l48b4yr+baseyr+1,max_yr                                                         
                     infresv(ifuel,ir,myr) = infresv(ifuel,ir,myr) - rvalue(jcde)                           
                     if (infresv(ifuel,ir,myr) <= 0.0) infresv(ifuel,ir,myr) = 0.0                         
                     infarsv(ifuel,ir,myr) = infarsv(ifuel,ir,myr) - rvalue2(jcde)                        
                     if (infarsv(ifuel,ir,myr) <= 0.0) infarsv(ifuel,ir,myr) = 0.0                       
                  end do                                                                                
               end if                                                                                  
            end if                                                         !eor/asr processes         

            if (apcode(ires) == 2) then                                     !waterflood              

!              subtract the technical production from undiscovered resource base when the waterflood begins

               undres(ifuel,ir,iyr-l48b4yr+baseyr) = undres(ifuel,ir,iyr-l48b4yr+baseyr) - uvalue(ucde)   
               undares(ifuel,ir,iyr-l48b4yr+baseyr) = undares(ifuel,ir,iyr-l48b4yr+baseyr) - uvalue2(ucde)
               if (undres(ifuel,ir,iyr-l48b4yr+baseyr) <= 0.0) undres(ifuel,ir,iyr-l48b4yr+baseyr) = 0.0 
               if (undares(ifuel,ir,iyr-l48b4yr+baseyr) <= 0.0) undares(ifuel,ir,iyr-l48b4yr+baseyr) = 0.0
               if (iyr == lastyr) then                                                                   
                  do myr = iyr-l48b4yr+baseyr+1,max_yr                                                  
                     undres(ifuel,ir,myr) = undres(ifuel,ir,myr) - uvalue(ucde)                        
                     undares(ifuel,ir,myr) = undares(ifuel,ir,myr) - uvalue2(ucde)                    
                     if (undres(ifuel,ir,myr) <= 0.0) undres(ifuel,ir,myr) = 0.0                     
                     if (undares(ifuel,ir,myr) <= 0.0) undares(ifuel,ir,myr) = 0.0                  
                  end do                                                                           
               end if                                                                             

!              add the waterflood's inferred reserves to the inferred reserves                   

               infresv(ifuel,ir,iyr-l48b4yr+baseyr) = infresv(ifuel,ir,iyr-l48b4yr+baseyr) + (eiremres(ires,jyr)/1000.0) 
               infarsv(ifuel,ir,iyr-l48b4yr+baseyr) = infarsv(ifuel,ir,iyr-l48b4yr+baseyr) + (eigremres(ires,jyr)/1000.0)
            end if                                                         !waterflood                  

            if (apcode(ires) == 10.or.apcode(ires) == 17) then              !undiscovered oil          

!              subtract the discovered reservoir from the undiscovered resource base                  

               undres(ifuel,ir,iyr-l48b4yr+baseyr) = undres(ifuel,ir,iyr-l48b4yr+baseyr) - uvalue(ucde) 
               if (undres(ifuel,ir,iyr-l48b4yr+baseyr) <= 0.0) undres(ifuel,ir,iyr-l48b4yr+baseyr) = 0.0 
               undares(ifuel,ir,iyr-l48b4yr+baseyr) = undares(ifuel,ir,iyr-l48b4yr+baseyr) - uvalue2(ucde)
               if (undares(ifuel,ir,iyr-l48b4yr+baseyr) <= 0.0) undares(ifuel,ir,iyr-l48b4yr+baseyr) = 0.0

!              add the discovered reservoir's inferred reserves to the inferred reserves base            

               infresv(ifuel,ir,iyr-l48b4yr+baseyr) = infresv(ifuel,ir,iyr-l48b4yr+baseyr) + (eiremres(ires,jyr)/1000.0)
               infarsv(ifuel,ir,iyr-l48b4yr+baseyr) = infarsv(ifuel,ir,iyr-l48b4yr+baseyr) + (eigremres(ires,jyr)/1000.0) 
               if (iyr == lastyr) then                                                                        
                  do myr=iyr-l48b4yr+baseyr+1,max_yr                                                         
                     undres(ifuel,ir,myr) = undres(ifuel,ir,myr) - uvalue(ucde)                             
                     if (undres(ifuel,ir,myr) <= 0.0) undres(ifuel,ir,myr) = 0.0                           
                     undares(ifuel,ir,myr) = undares(ifuel,ir,myr) - uvalue2(ucde)                        
                     if (undares(ifuel,ir,myr) <= 0.0) undares(ifuel,ir,myr) = 0.0                       
                  end do                                                                                
               end if                                                                                  
            end if                                                         !undiscovered oil          

            if (apcode(ires) >= 16.and.apcode(ires) /= 17) then             !undiscovered gas        

!              subtract the discovered reservoir from the undiscovered resource base                

               undres(ifuel,ir,iyr-l48b4yr+baseyr) = undres(ifuel,ir,iyr-l48b4yr+baseyr) - uvalue2(ucde) 
               if (undres(ifuel,ir,iyr-l48b4yr+baseyr) <= 0.0) undres(ifuel,ir,iyr-l48b4yr+baseyr) = 0.0
               undares(ifuel,ir,iyr-l48b4yr+baseyr) = undares(ifuel,ir,iyr-l48b4yr+baseyr) - uvalue(ucde)
               if (undares(ifuel,ir,iyr-l48b4yr+baseyr) <= 0.0) undares(ifuel,ir,iyr-l48b4yr+baseyr) = 0.0

!              add the discovered reservoir's inferred reserves to the inferred reserves base            

               infresv(ifuel,ir,iyr-l48b4yr+baseyr) = infresv(ifuel,ir,iyr-l48b4yr+baseyr) + (eigremres(ires,jyr)/1000.0)
               infarsv(ifuel,ir,iyr-l48b4yr+baseyr) = infarsv(ifuel,ir,iyr-l48b4yr+baseyr) + (eiremres(ires,jyr)/1000.0)
               if (iyr == lastyr) then                                                                       
                  do myr=iyr-l48b4yr+baseyr+1,max_yr                                                        
                     undres(ifuel,ir,myr) = undres(ifuel,ir,myr) - uvalue2(ucde)                           
                     if (undres(ifuel,ir,myr) <= 0.0) undres(ifuel,ir,myr) = 0.0                          
                     undares(ifuel,ir,myr) = undares(ifuel,ir,myr) - uvalue(ucde)                        
                     if (undares(ifuel,ir,myr) <= 0.0) undares(ifuel,ir,myr) = 0.0                      
                  end do                                                                               
               end if                                                                                 
            end if                                                         !undiscovered gas         

            if (jyr == 1) then
               o_resadd = eremres(ires,jyr)
               g_resadd = egremres(ires,jyr)
            else
               o_resadd = (eremres(ires,jyr)-eremres(ires,jyr-1)+eprodoil(ires,jyr))
               g_resadd = (egremres(ires,jyr)-egremres(ires,jyr-1)+eprodgas(ires,jyr))
            endif

            IF (IYR > L48HYR) THEN
               IF (APCODE(ires) == 17.or.APCODE(ires) == 21.or.APCODE(ires) == 22) THEN
                 OGOILPRD(DIST,2,IYR) = OGOILPRD(DIST,2,IYR) + eprodoil(ires,jyr)/1000./365.
               ELSEIF (APCODE(ires) == 3) THEN
                 OGOILPRD(DIST,3,IYR) = OGOILPRD(DIST,3,IYR) + eprodoil(ires,jyr)/1000./365.
               ELSEIF (APCODE(ires) >= 4.and.APCODE(ires) <= 9) THEN
                 OGOILPRD(DIST,4,IYR) = OGOILPRD(DIST,4,IYR) + eprodoil(ires,jyr)/1000./365.
               ELSEIF (aplay_cde(ires) == 2804.or.(aplay_cde(ires) >= 3110.and.aplay_cde(ires) <= 3115).or.  &
                       aplay_cde(ires) == 4763.or.aplay_cde(ires) == 5875.or.aplay_cde(ires) == 99926039.or.  &
                       aplay_cde(ires) == 4409.or.(aplay_cde(ires) >= 4747.and.aplay_cde(ires) <= 4749).or.  &
                       aplay_cde(ires) == 3904.or.aplay_cde(ires) == 3920.or.aplay_cde(ires) == 99905037.or.  &
                       aplay_cde(ires) == 4473.or.aplay_cde(ires) == 4401.or.aplay_cde(ires) == 99943037.or.  &
                       aplay_cde(ires) == 99949033.or.aplay_cde(ires) == 99949037.or.  &
                       aplay_cde(ires) == 1201.or.aplay_cde(ires) == 99904010) THEN
                 OGOILPRD(DIST,2,IYR) = OGOILPRD(DIST,2,IYR) + eprodoil(ires,jyr)/1000./365.
               ELSE
                 OGOILPRD(DIST,1,IYR) = OGOILPRD(DIST,1,IYR) + eprodoil(ires,jyr)/1000./365.
               ENDIF
            ENDIF

            if (apcode(ires) <= 10.or.apcode(ires) == 17) then   ! OIL

               IF (apcode(ires) == 0) then
                  IF (aresflag(ires) == 7) then  !ongoing co2eor in decline curve
                     isrc = 4
                     REPPRDL48(IR,2,IYR) = REPPRDL48(IR,2,IYR) + EPRODOIL(ires,jyr)*.001
                     REPEORL48(IR,4,IYR) = REPEORL48(IR,4,IYR) + EPRODOIL(ires,jyr)*.001
                     REPREVL48(IR,2,IYR) = REPREVL48(IR,2,IYR) + o_resadd*.001
                  ELSEIF(aresflag(ires) == 8) then  !other ongoing eor processes in decline curve
                     REPPRDL48(IR,2,IYR) = REPPRDL48(IR,2,IYR) + EPRODOIL(ires,jyr)*.001
                     REPEORL48(IR,IPROC,IYR) = REPEORL48(IR,IPROC,IYR) + EPRODOIL(ires,jyr)*.001
                     REPREVL48(IR,2,IYR) = REPREVL48(IR,2,IYR) + o_resadd*.001
                  ELSE
                     REPPRDL48(IR,IFUEL,IYR) = REPPRDL48(IR,IFUEL,IYR) + EPRODOIL(ires,jyr)*.001
                     REPREVL48(IR,IFUEL,IYR) = REPREVL48(IR,IFUEL,IYR) + o_resadd*.001
                  END if
               else
                  REPPRDL48(IR,IFUEL,IYR) = REPPRDL48(IR,IFUEL,IYR) + EPRODOIL(ires,jyr)*.001
                  if (apcode(ires) == 3.or.apcode(ires) == 4.OR.  &
                     apcode(ires) == 5.or.apcode(ires) == 7) &
                     REPEORL48(IR,IPROC,IYR) = REPEORL48(IR,IPROC,IYR) + EPRODOIL(ires,jyr)*.001
                  IF (apcode(ires) == 10.or.apcode(ires) == 17) THEN
                     REPNRDL48(IR,IFUEL,IYR) = REPNRDL48(IR,IFUEL,IYR) + o_resadd*.001
                  ELSE
                     REPEXTL48(IR,IFUEL,IYR) = REPEXTL48(IR,IFUEL,IYR) + o_resadd*.001
                  END if
               END if

               IF (IYR > L48HYR) THEN
                  HISTADPRD(IR,IYR)    = HISTADPRD(IR,IYR)      & !associated dissolved gas
                                  + eprodgas(ires,jyr)*.001
                  ADRESAD(IR,IYR)     = ADRESAD(IR,IYR) + g_resadd*.001      !AD Gas reserves
                  if (apcode(ires) == 17.or.aresflag(ires) == 9.or.  &                              ! saving AD gas from tight oil
                      aplay_cde(ires) == 2804.or.(aplay_cde(ires) >= 3110.and.aplay_cde(ires) <= 3115).or.  &
                      aplay_cde(ires) == 4763.or.aplay_cde(ires) == 5875.or.aplay_cde(ires) == 99926039.or.  &
                      aplay_cde(ires) == 4409.or.(aplay_cde(ires) >= 4747.and.aplay_cde(ires) <= 4749).or.  &
                      aplay_cde(ires) == 3904.or.aplay_cde(ires) == 3920.or.aplay_cde(ires) == 99905037.or.  &
                      aplay_cde(ires) == 4473.or.aplay_cde(ires) == 4401.or.aplay_cde(ires) == 99943037.or.  &
                      aplay_cde(ires) == 99949033.or.aplay_cde(ires) == 99949037.or.  &
                      aplay_cde(ires) == 1201.or.aplay_cde(ires) == 99904010)  then
                    REPPRDAD(IR,2,IYR) = REPPRDAD(IR,2,IYR) + EPRODGAS(ires,jyr)*.001
                    OGADGPRD(DIST,2,IYR) = OGADGPRD(DIST,2,IYR) + eprodgas(ires,jyr)*.001
                  else
                    REPPRDAD(IR,1,IYR) = REPPRDAD(IR,1,IYR) + EPRODGAS(ires,jyr)*.001
                    OGADGPRD(DIST,1,IYR) = OGADGPRD(DIST,1,IYR) + eprodgas(ires,jyr)*.001
                  endif
               ENDIF

            else                                                   ! NATURAL GAS

               REPPRDL48(IR,IFUEL,IYR)    =                       & !dry gas
               REPPRDL48(IR,IFUEL,IYR) + EPRODGAS(ires,jyr)*.001
               REPPRDL48(IR,1,IYR)    =                       & !lease condensate
               REPPRDL48(IR,1,IYR) + EPRODOIL(ires,jyr)*.001
               IF (IYR > L48HYR) OGENAGPRD(dist,ifuel-3,iyr) = OGENAGPRD(dist,ifuel-3,iyr) + EPRODGAS(ires,jyr)*0.001
            end if

!           TOTALING CRUDE PRODUCTION BY CRUDE TYPE

            IF (IYR > L48HYR) THEN
            if (acrdtype(ires) >= 1) then
               sg = 141.5/(AAPI(ires)+131.5) ! convert API gravity to specific gravity
               t=ACRDTYPE(ires)          ! aggregate crude production by Region and crude quality
               LF_CRDPRD(IR,t,IYR) = LF_CRDPRD(IR,t,IYR) + EPRODOIL(ires,jyr)*.001
               LF_CRDPRD_PADD(oPADD,t,IYR) = LF_CRDPRD_PADD(oPADD,t,IYR) + EPRODOIL(ires,jyr)*.001
               TOTCRDPRD(IR,IYR) = TOTCRDPRD(IR,IYR) + EPRODOIL(ires,jyr)*.001
               TOTCRDPRD_PADD(IR,IYR) = TOTCRDPRD_PADD(IR,IYR) + EPRODOIL(ires,jyr)*.001
               LF_CRDVOL(t,IYR) = LF_CRDVOL(t,IYR) + (SG * EPRODOIL(ires,jyr)*.001)

 
               if (t == 9) WRITE(BUGOUT,112) t,iyr+1989,jyr, ires, ARESID(IRES), AAPI(ires), ASULFOIL(ires), EPRODOIL(IRES,JYR)
 112              FORMAT('CRUDETYPE 9, S < 0  : ',i6,i6,i6,i6,1x,A11,f8.4,f8.4,f10.4)

            else
               if (EPRODOIL(ires,jyr) > 0.0) then
                  if (acrdtype(ires) == 0) then
                     t=ACRDTYPE(ires)          ! aggregate crude production by Region and crude quality

                     WRITE(BUGOUT,111) t,iyr+1989,jyr, ires, ARESID(IRES), AAPI(ires), ASULFOIL(ires), EPRODOIL(IRES,JYR)
 111                 FORMAT('CRUDETYPE EPRODOIL>0: ',i6,i6,i6,i6,1x,A11,f8.4,f8.4,f10.4)

                  else
                     t=ABS(ACRDTYPE(ires))         ! aggregate crude production by Region and crude quality
                     MissedCRDPRD(IR,t,IYR)  = MissedCRDPRD(IR,t,IYR)  + EPRODOIL(ires,jyr)*.001
                  endif
               endif
            endif
            ENDIF

            IF (jyr == 1.AND.IWELL == 1)THEN                    !FIRST WELL WHEN FIELD IS TIMED
               REPNFWL48(IR,IFUEL,IYR) =  REPNFWL48(IR,IFUEL,IYR)   + 1   ! new field wildcat
            ENDIF

            IF (IFUEL <= 2) THEN
               REPSUCWLL48(2,IR,IFUEL,IYR) = REPSUCWLL48(2,IR,IFUEL,IYR) + EPRODDR(ires,jyr) * 0.90 ! development wells
               REPDRYWLL48(2,IR,IFUEL,IYR) = REPDRYWLL48(2,IR,IFUEL,IYR) + edryhole(ires,jyr) * 0.7
               REPSUCWLL48(1,IR,IFUEL,IYR) = REPSUCWLL48(1,IR,IFUEL,IYR) + EPRODDR(ires,jyr) * 0.10 ! exploratory wells
               REPDRYWLL48(1,IR,IFUEL,IYR) = REPDRYWLL48(1,IR,IFUEL,IYR) + edryhole(ires,jyr) * 0.3
               REPDRILLL48(1,IR,IFUEL,IYR) = REPDRILLL48(1,IR,IFUEL,IYR) &
                                             + Dwc_w*(.3*tang_o(iyr)+.7*intang_o(iyr))*EPRODDR(IRES,JYR)*0.1
               REPDRYL48(1,IR,IFUEL,IYR)   = REPDRYL48(1,IR,IFUEL,IYR) + DRY_w*tang_o(iyr)*Edryhole(IRES,JYR)*0.3
               REPLEASL48(1,IR,IFUEL,IYR)  = REPLEASL48(1,IR,IFUEL,IYR) + NPR_w*EPRODDR(IRES,JYR)*tang_o(iyr)*0.1
               REPDRILLL48(2,IR,IFUEL,IYR) = REPDRILLL48(2,IR,IFUEL,IYR) &
                                             + Dwc_w*(.3*tang_o(iyr)+.7*intang_o(iyr))*EPRODDR(IRES,JYR)*0.9
               REPDRYL48(2,IR,IFUEL,IYR)   = REPDRYL48(2,IR,IFUEL,IYR) + DRY_w*tang_o(iyr)*Edryhole(IRES,JYR)*0.7
               REPLEASL48(2,IR,IFUEL,IYR)  = REPLEASL48(2,IR,IFUEL,IYR) + NPR_w*EPRODDR(IRES,JYR)*tang_o(iyr)*0.9
            ELSEIF (IFUEL <= 4) THEN
               REPSUCWLL48(2,IR,IFUEL,IYR) = REPSUCWLL48(2,IR,IFUEL,IYR) + EPRODDR(ires,jyr) * 0.75 ! development wells
               REPDRYWLL48(2,IR,IFUEL,IYR) = REPDRYWLL48(2,IR,IFUEL,IYR) + edryhole(ires,jyr) * 0.7
               REPSUCWLL48(1,IR,IFUEL,IYR) = REPSUCWLL48(1,IR,IFUEL,IYR) + EPRODDR(ires,jyr) * 0.25 ! exploratory wells
               REPDRYWLL48(1,IR,IFUEL,IYR) = REPDRYWLL48(1,IR,IFUEL,IYR) + edryhole(ires,jyr) * 0.3
               REPDRILLL48(2,IR,IFUEL,IYR) = REPDRILLL48(2,IR,IFUEL,IYR) &
                                             + Dwc_w*(.4*tang_g(iyr)+.6*intang_g(iyr))*EPRODDR(IRES,JYR)*0.75
               REPDRYL48(2,IR,IFUEL,IYR)   = REPDRYL48(2,IR,IFUEL,IYR) + DRY_w*tang_g(iyr)*Edryhole(IRES,JYR)*0.70
               REPLEASL48(2,IR,IFUEL,IYR)  = REPLEASL48(2,IR,IFUEL,IYR) + NPR_w*EPRODDR(IRES,JYR)*tang_g(iyr)*0.75
               REPDRILLL48(1,IR,IFUEL,IYR) = REPDRILLL48(1,IR,IFUEL,IYR) &
                                             + Dwc_w*(.4*tang_g(iyr)+.6*intang_g(iyr))*EPRODDR(IRES,JYR)*0.25
               REPDRYL48(1,IR,IFUEL,IYR)   = REPDRYL48(1,IR,IFUEL,IYR) + DRY_w*tang_g(iyr)*Edryhole(IRES,JYR)*0.30
               REPLEASL48(1,IR,IFUEL,IYR)  = REPLEASL48(1,IR,IFUEL,IYR) + NPR_w*EPRODDR(IRES,JYR)*tang_g(iyr)*0.25
            ELSE
               REPSUCWLL48(2,IR,IFUEL,IYR) = REPSUCWLL48(2,IR,IFUEL,IYR) + EPRODDR(ires,jyr) * 0.99 ! development wells
               REPDRYWLL48(2,IR,IFUEL,IYR) = REPDRYWLL48(2,IR,IFUEL,IYR) + edryhole(ires,jyr) * 0.70
               REPSUCWLL48(1,IR,IFUEL,IYR) = REPSUCWLL48(1,IR,IFUEL,IYR) + EPRODDR(ires,jyr) * 0.01 ! exploratory wells
               REPDRYWLL48(1,IR,IFUEL,IYR) = REPDRYWLL48(1,IR,IFUEL,IYR) + edryhole(ires,jyr) * 0.3
               REPDRILLL48(2,IR,IFUEL,IYR) = REPDRILLL48(2,IR,IFUEL,IYR) &
                                             + Dwc_w*(.4*tang_g(iyr)+.6*intang_g(iyr))*EPRODDR(IRES,JYR)*0.99
               REPDRYL48(2,IR,IFUEL,IYR)   = REPDRYL48(2,IR,IFUEL,IYR) + DRY_w*tang_g(iyr)*Edryhole(IRES,JYR)*0.70
               REPLEASL48(2,IR,IFUEL,IYR)  = REPLEASL48(2,IR,IFUEL,IYR) + NPR_w*EPRODDR(IRES,JYR)*tang_g(iyr)*0.99
               REPDRILLL48(1,IR,IFUEL,IYR) = REPDRILLL48(1,IR,IFUEL,IYR) &
                                             + Dwc_w*(.4*tang_g(iyr)+.6*intang_g(iyr))*EPRODDR(IRES,JYR)*0.01
               REPDRYL48(1,IR,IFUEL,IYR)   = REPDRYL48(1,IR,IFUEL,IYR) + DRY_w*tang_g(iyr)*Edryhole(IRES,JYR)*0.30
               REPLEASL48(1,IR,IFUEL,IYR)  = REPLEASL48(1,IR,IFUEL,IYR) + NPR_w*EPRODDR(IRES,JYR)*tang_g(iyr)*0.01
            ENDIF
            REPPRDWLL48(IR,IFUEL,IYR) = REPPRDWLL48(IR,IFUEL,IYR) + expatn(ires,jyr)
            REPOPERL48(IWELL,IR,IFUEL,IYR)  = REPOPERL48(IWELL,IR,IFUEL,IYR) + ETOC(IRES,JYR)
            REPDCFL48(IWELL,IR,IFUEL,IYR)   = REPDCFL48(IWELL,IR,IFUEL,IYR) + PATNDCF(IRES,JYR)

            ogsm_yr = iyr + baseyr - l48b4yr
            if (isrc  >  0 .and. ogsm_yr  >  0) then

               IF (ARESFLAG(ires)  ==  7) then
                  EORProduction(JR,isrc,iyr) = EORProduction(JR,isrc,iyr) + eprodoil(ires,jyr)
                  GasProduction(JR,isrc,iyr) = GasProduction(JR,isrc,iyr) + eprodgas(ires,jyr)
               END IF

            end if
           
!           FOR PMM

            PMMDG(DIST,IFUEL,IYR)      = PMMDG(DIST,IFUEL,IYR) + eprodgas(ires,jyr)*0.001

            NGPLPRD(DIST,IYR) = NGPLPRD(DIST,IYR) +  eprodgas(ires,jyr)*angl(ires)*0.000001/365.                    ! mmb/d 
            NGPLPRDET(DIST,IYR) = NGPLPRDET(DIST,IYR) +  angplet(ires)*eprodgas(ires,jyr)*angl(ires)*0.000001/365.  ! mmb/d 
            NGPLPRDPR(DIST,IYR) = NGPLPRDPR(DIST,IYR) +  angplpr(ires)*eprodgas(ires,jyr)*angl(ires)*0.000001/365.  ! mmb/d 
            NGPLPRDBU(DIST,IYR) = NGPLPRDBU(DIST,IYR) +  angplbu(ires)*eprodgas(ires,jyr)*angl(ires)*0.000001/365.  ! mmb/d 
            NGPLPRDIS(DIST,IYR) = NGPLPRDIS(DIST,IYR) +  angplis(ires)*eprodgas(ires,jyr)*angl(ires)*0.000001/365.  ! mmb/d 
            NGPLPRDPP(DIST,IYR) = NGPLPRDPP(DIST,IYR) +  angplpp(ires)*eprodgas(ires,jyr)*angl(ires)*0.000001/365.  ! mmb/d 
            ! ethane rejection
              produced = angplet(ires)*eprodgas(ires,jyr)*angl(ires)*0.000001/365.      ! mmb/d 
              rejected = etrejfac(JR,iyr)*produced
              recovered = produced - rejected      ! mmb/d 
            IF (ETHANEMAX(JR,IYR) > 0..and.ETHANEREC(JR,IYR)+recovered > ETHANEMAX(JR,IYR)) THEN
               rejected = rejected + (ethanerec(jr,iyr)+recovered - ethanemax(jr,iyr))
               recovered = ethanemax(jr,iyr) - ethanerec(jr,iyr)
              ENDIF
              ETHANEREC(JR,IYR) = ETHANEREC(JR,IYR) + recovered
              ETHANEREJ(JR,IYR) = ETHANEREJ(JR,IYR) + rejected
              NGPLPRD(DIST,IYR) = NGPLPRD(DIST,IYR) - rejected
              NGPLPRDET(DIST,IYR) = NGPLPRDET(DIST,IYR) - rejected
              eprodgas(ires,jyr) = eprodgas(ires,jyr) + rejected*365./5.61*1000.
            
!           write(ogbug1,*) 'NGPLPRD', curiyr+1989, DIST, NGPLPRD(DIST,IYR),NGPLPRDET(DIST,IYR)

!           if (itimeyr == 1.and.jyr == 1.and.angl(ires) > 0.) write(6,*) 'dh5out ', curiyr+1989, dist, aresid(ires),  &
!                 angl(ires), eprodgas(ires,jyr)*angl(ires)*0.001/365.                            ! 

         ENDIF
      ENDDO

!     END AGGREGATION.  TAG RESERVOIR AS AGGREGATED

      aggregated(ires) = .true.

      RETURN
      END SUBROUTINE

!     ****************************************************************************************
!     ****************************************************************************************
!     ****************************************************************************************
!     ****************************************************************************************
!     ****************************************************************************************

       subroutine dist_match(st1,irrc,IR,dist)

       implicit none

       INTEGER irrc,dist,IR
       CHARACTER*2 st1

!  ST1 - STATE ABBREVIATION
!  IRRC - RRC, FOR TEXAS ONLY
!  IR - OLOGSS REGION - USED FOR SPLITTING NEW MEXICO RESOURCES
!  DIST - PMM DRY GAS REGIONS - OUTPUT OF SUBROUTINE
       dist = 0
      IF(st1 == 'AL'.and.irrc <= 0) dist = 1
      IF(st1 == 'AL'.and.irrc == 1) dist = 2
      IF(st1 == 'AK') dist = 3
      IF(st1 == 'AZ') dist = 4
      IF(st1 == 'AR') dist = 5
      IF(st1 == 'CA') dist = 6
      IF(st1 == 'CO') dist = 7
      IF(st1 == 'CT') dist = 8
      IF(st1 == 'DE') dist = 9
      IF(st1 == 'DC') dist = 10
      IF(st1 == 'FL') dist = 11
      IF(st1 == 'GA') dist = 12
      IF(st1 == 'HI') dist = 13
      IF(st1 == 'ID') dist = 14
      IF(st1 == 'IL') dist = 15
      IF(st1 == 'IN') dist = 16
      IF(st1 == 'IA') dist = 17
      IF(st1 == 'KS') dist = 18
      IF(st1 == 'KY') dist = 19
      IF(st1 == 'LA'.and.irrc <= 0) dist = 20
      IF(st1 == 'LA'.and.irrc == 1) dist = 21
      IF(st1 == 'ME') dist = 22
      IF(st1 == 'MD') dist = 23
      IF(st1 == 'MA') dist = 24
      IF(st1 == 'MI') dist = 25
      IF(st1 == 'MN') dist = 26
      IF(st1 == 'MS'.and.irrc <= 0) dist = 27
      IF(st1 == 'MS'.and.irrc == 1) dist = 28
      IF(st1 == 'MO') dist = 29
      IF(st1 == 'MT') dist = 30
      IF(st1 == 'NE') dist = 31
      IF(st1 == 'NV') dist = 32
      IF(st1 == 'NH') dist = 33
      IF(st1 == 'NJ') dist = 34
      IF(st1 == 'NM'.AND.IR == 4) dist = 35
      IF(st1 == 'NM'.AND.IR == 5) dist = 36
      IF(st1 == 'NY') dist = 37
      IF(st1 == 'NC') dist = 38
      IF(st1 == 'ND') dist = 39
      IF(st1 == 'OH') dist = 40
      IF(st1 == 'OK') dist = 41
      IF(st1 == 'OR') dist = 42
      IF(st1 == 'PA') dist = 43
      IF(st1 == 'RI') dist = 44
      IF(st1 == 'SC') dist = 45
      IF(st1 == 'SD') dist = 46
      IF(st1 == 'TN') dist = 47
      IF(st1 == 'TX'.AND.IRRC == 1) dist = 48
      IF(st1 == 'TX'.AND.IRRC == 2) dist = 49
      IF(st1 == 'TX'.AND.IRRC == 3) dist = 50
      IF(st1 == 'TX'.AND.IRRC == 4) dist = 51
      IF(st1 == 'TX'.AND.IRRC == 5) dist = 52
      IF(st1 == 'TX'.AND.IRRC == 6) dist = 53
      IF(st1 == 'TX'.AND.IRRC == 72) dist = 54
      IF(st1 == 'TX'.AND.IRRC == 73) dist = 55
      IF(st1 == 'TX'.AND.IRRC == 8)  dist = 56
      IF(st1 == 'TX'.AND.IRRC == 81) dist = 57
      IF(st1 == 'TX'.AND.IRRC == 9)  dist = 58
      IF(st1 == 'TX'.AND.IRRC == 10) dist = 59
      IF(st1 == 'UT') dist = 60
      IF(st1 == 'VT') dist = 61
      IF(st1 == 'VA') dist = 62
      IF(st1 == 'WA') dist = 63
      IF(st1 == 'WV') dist = 64
      IF(st1 == 'WI') dist = 65
      IF(st1 == 'WY') dist = 66
       end subroutine

!     ***************************************************************
! FILL TAB_WRITE VARIABLES
!     ***************************************************************

       SUBROUTINE OGTO_OGSM

       implicit none

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include 'ogsmugr'
      include 'ogsml48'
      include 'ogsmeor'
      include 'ogsmout'
      include 'macout'
      include 'ngtdmrep'
      include 'tcs45q'
      include 'emmparm'
      include 'ecpcntl'
      include 'uefdout'
      include 'ctus'
      
      integer s, DIST, JR, ogsm_yr, iyr, isrc, IBIN, JBIN, IREC,JREC
      REAL TOTRSVS
      REAL TOTPRRAT
      REAL TOTRESAD
      REAL TOTEPRD
      REAL ogprr, ngprr
      REAL*4 totprd,totugr
      REAL*8 TARGET_CO2

      INTEGER   RUN45Q                           ! RUN with 45Q, 0=>No
      INTEGER   RTOVALUE

      LOGICAL ONCE
      DATA ONCE/.true./

      RUN45Q = RTOVALUE('RUN45Q  ',0)

      delta1l48    = 0.0
      delta2l48    = 0.0
      delta3l48    = 0.0
      spendirk_l48 = 0.0
      cumr1l48     = 0.0
      cumr2l48     = 0.0
      cumr3l48     = 0.0
      estwellsl48  = 0.0
      rigsl48      = 0.0  !pull eqtn in  reestimate

!     SET LAST HISTORICAL YEAR and FIRST STEO YEAR SO THAT THERE IS NO VARIATION ACROSS CASES
      if (curiyr.eq.l48hyr.and.ogSTEO.eq.1) then
        DO DIST=1,OGDIST
          OGENAGPRD(DIST,GASTYPES,CURIYR) = SUM(OGENAGPRD(DIST,1:GASTYPES-1,CURIYR))
          OGADGPRD(DIST,OILTYPES,CURIYR) = SUM(OGADGPRD(DIST,1:OILTYPES-1,CURIYR))
          DO K=1,GASTYPES-1
              if(OGENAGPRD(DIST,GASTYPES,CURIYR).gt.0.)   &
                   OGENAGPRD(DIST,K,CURIYR) = sOGENAGPRD(DIST,1) * OGENAGPRD(DIST,K,CURIYR)/OGENAGPRD(DIST,GASTYPES,CURIYR)
          ENDDO
          DO K=1,OILTYPES-1
              if(OGADGPRD(DIST,OILTYPES,CURIYR).gt.0.)   &
                   OGADGPRD(DIST,K,CURIYR) = sOGADGPRD(DIST,1) * OGADGPRD(DIST,K,CURIYR)/OGADGPRD(DIST,OILTYPES,CURIYR)
          ENDDO
        ENDDO
      endif
      if ((curiyr.eq.l48hyr+1.and.ogSTEO.eq.1)) then
        DO DIST=1,OGDIST
          OGENAGPRD(DIST,GASTYPES,CURIYR) = SUM(OGENAGPRD(DIST,1:GASTYPES-1,CURIYR))
          OGADGPRD(DIST,OILTYPES,CURIYR) = SUM(OGADGPRD(DIST,1:OILTYPES-1,CURIYR))
          DO K=1,GASTYPES-1
              if(sum(OGENAGPRD(DIST,1:GASTYPES-1,CURIYR)).gt.0.)   &
                   OGENAGPRD(DIST,K,CURIYR) = sOGENAGPRD(DIST,2) * OGENAGPRD(DIST,K,CURIYR)/OGENAGPRD(DIST,GASTYPES,CURIYR)
          ENDDO
          DO K=1,OILTYPES-1
              if(sum(OGADGPRD(DIST,1:OILTYPES-1,CURIYR)).gt.0.)   &
                   OGADGPRD(DIST,K,CURIYR) = sOGADGPRD(DIST,2) * OGADGPRD(DIST,K,CURIYR)/OGADGPRD(DIST,OILTYPES,CURIYR)
          ENDDO
        ENDDO
      endif
      DO DIST=1,OGDIST
        OGENAGPRD(DIST,GASTYPES,CURIYR) = SUM(OGENAGPRD(DIST,1:GASTYPES-1,CURIYR))
        OGADGPRD(DIST,OILTYPES,CURIYR) = SUM(OGADGPRD(DIST,1:OILTYPES-1,CURIYR))
      ENDDO

!     RECLASSIFY SUCCWELL:1=NF DISCOVERY, 2=ALL OTHER DRILLING

      DO R = 1,L48RGN
        write(OGBUG1,*) 'histadprd', curiyr+1989, r, histadprd(r,curiyr)
        DO K = 1,L48FUEL
          DO I = 1,L48WELL
            sucwelll48(I,R,K) = REPSUCWLL48(I,R,K,curiyr)
            drywelll48(I,R,K) = REPDRYWLL48(I,R,K,curiyr)
            dcfl48(I,R,K)     = REPDCFL48(I,R,K,curiyr)*1000./dladj
               IF(repprdwll48(R,K,curiyr) > 0.0)THEN
              operl48(I,R,K)    = REPOPERL48(I,R,K,curiyr)/REPPRDWLL48(R,K,curiyr)*1000./dladj
            ELSE
              operl48(I,R,K)    = 0.0
            ENDIF
               IF(SUCWELLL48(I,R,K) > 0.0)THEN
              leasl48(I,R,K)    = REPLEASL48(I,R,K,curiyr)/SUCWELLL48(I,R,K)*1000./dladj
              drilll48(I,R,K)   = REPDRILLL48(I,R,K,curiyr)/SUCWELLL48(I,R,K)*1000./dladj
            ELSE
              leasl48(I,R,K)    = 0.0
              drilll48(I,R,K)   = 0.0
            ENDIF
               IF(DRYWELLL48(I,R,K) > 0.0)THEN
              dryl48(I,R,K)     = REPDRYL48(I,R,K,curiyr)/DRYWELLL48(I,R,K)*1000./dladj
            ELSE
              dryl48(I,R,K)     = 0.0
            ENDIF
            WELLSL48(I,R,K)   = REPSUCWLL48(I,R,K,curiyr) &
                               + REPDRYWLL48(I,R,K,curiyr)
          ENDDO
          RESADL48(R,K)       = REPRADL48(R,K,curiyr)
          NFWL48(R,K)         = REPNFWL48(R,K,curiyr)
          NRDL48(R,K)         = REPNRDL48(R,K,curiyr) !new reserve discovery
          REVL48(R,K)         = REPREVL48(R,K,curiyr) !revisions
          EXTL48(R,K)         = REPEXTL48(R,K,curiyr) !revisions
          if (curiyr < l48hyr) then
            NRDL48(R,K)         = HISTNRDL48(R,K,curiyr) !new reserve discovery
            REVL48(R,K)         = HISTREVL48(R,K,curiyr) !revisions
            EXTL48(R,K)         = HISTEXTL48(R,K,curiyr) !revisions
            RESADL48(R,K)       = NRDL48(R,K) + REVL48(R,K) + EXTL48(R,K)
          endif
          NDIRL48(R,K)        = REPNDIRL48(R,K,curiyr)!new discovered inferred reserves (added to inferred)

   !           ndirl48+nrdl48 = tot recoverable resource for each new discovery

          DEVL48(R,K)         = SUCWELLL48(2,R,K)
          OEXPL48(R,K)        = SUCWELLL48(1,R,K) - NFWL48(R,K)
            IF(NFWL48(R,K) > 0)THEN
            FR1L48(R,K)       = NRDL48(R,K) / NFWL48(R,K)
          ELSE
            FR1L48(R,K)       = 0.0
          ENDIF
            IF(DEVL48(R,K) > 0)THEN
            FR3L48(R,K)       = (REVl48(R,K)+EXTL48(R,K)) / DEVL48(R,K)
          ELSE
            FR3L48(R,K)       = 0.0
          ENDIF
        ENDDO
      ENDDO

      REPPRDL48(:,4:L48FUEL,CURIYR) = 0.
      DO R=1,OGDIST
      DO K=4,L48FUEL
        if (distmap(r,1).ne.0.and.distmap(r,1) <= L48RGN) &
             REPPRDL48(distmap(r,1),k,CURIYR) = REPPRDL48(distmap(r,1),K,CURIYR) + OGENAGPRD(R,K-3,CURIYR)
      ENDDO
      ENDDO

      RESBOYL48(:,4:L48FUEL) = 0.
      CURPRRL48(:,4:L48FUEL) = 0.
      DO R = 1,L48RGN
        DO K = 1,L48FUEL
          if (repprdl48(r,k,curiyr) > 0.) THEN 
            if (HISTPRRL48(R,K,L48HYR) > 0.) THEN
              RESBOYL48(R,K) = REPPRDL48(R,K,CURIYR)/HISTPRRL48(R,K,L48HYR)
            ELSE
              RESBOYL48(R,K) = REPPRDL48(R,K,CURIYR)/0.1
            ENDIF
          ENDIF
          CURRESL48(R,K) = RESBOYL48(R,K)
          if (curiyr <= l48hyr) CURRESL48(R,K) = HISTRESL48(R,K,CURIYR)
          ogprr = curprrl48(r,k)
          if (resboyl48(r,k) > 0.) then
            curprrl48(R,K) = (repprdl48(r,k,curiyr)) / RESBOYL48(R,K)
          else
            CURPRRL48(R,K)    = 0.
          endif
          if (curiyr <= l48hyr) CURPRRL48(R,K) = HISTPRRL48(R,K,CURIYR)
          if (curiyr <= l48hyr) CURRESL48(R,K) = HISTRESL48(R,K,CURIYR)
        ENDDO
      ENDDO

!  ASSIGN GAS NEMS OUTPUT VARIABLES

!  ASSIGN CRUDE NEMS OUTPUT VARIABLES

      DO R=1,L48RGN
         IF (OGRESCO(R,CURIYR) > 0.0) THEN
           OGPRRCO(R,CURIYR) = (CURPRRL48(R,1)*RESBOYL48(R,1) &
                                 + CURPRRL48(R,2)*RESBOYL48(R,2)) /  &
                                 (resboyl48(r,1) + resboyl48(r,2))
         ELSE
           OGPRRCO(R,CURIYR) = 0.0
         ENDIF

         WRITE(OGBUG1,*) 'cosupcrv', CURIYR+BASEYR-1,R, &
           OGRESCO(R,CURIYR),OGPRRCO(R,CURIYR)

      ENDDO

      iyr = curiyr
      ogsm_yr = iyr + baseyr - l48b4yr
      DO JR = 1 , l48rgn+1 

!        Initialize CO2Purchased for Industrial Source Indexes 

         DO IBIN = 1 , max_src
            JBIN = MAP_SRC_INDEX_TO_BINS(IBIN)
            IF (JBIN .GT. 0) THEN
               CO2Purchased(JR,IBIN,iyr) = 0.0
            END IF
         END DO

         DO isrc = 1 , max_src
            CO2Available(JR,isrc,iyr) = bse_availco2(JR,isrc,ogsm_yr) * 1000.0
            CO2Purchasedb(JR,isrc,iyr) = (bse_availco2(JR,isrc,ogsm_yr) - nat_availco2(JR,isrc,ogsm_yr)) * 1000.0

!           For Industrial Sources Allocate Quantity by Price Bin into Actual Sources

            JBIN = MAP_SRC_INDEX_TO_BINS(isrc)
            IF (JBIN .GT. 0) THEN
               TARGET_CO2 = bse_availco2(JR,isrc,ogsm_yr) - nat_availco2(JR,isrc,ogsm_yr)
               DO IREC = 1 , N_NS_RECS(JR)
                  JREC = NS_PRC_NDX(JR,IREC,iyr)
                  IBIN = NS_SRC(JR,JREC)
                  IF (TARGET_CO2 .GT. 0.0 .AND. NS_BIN(JR,JREC,iyr) .EQ. JBIN) THEN
                     IF (TARGET_CO2 .GT. NS_QUANT(JR,JREC) * 18.0) THEN
                        CO2Purchased(JR,IBIN,iyr) = CO2Purchased(JR,IBIN,iyr) + NS_QUANT(JR,JREC) * 18.0 * 1000.0

                        cregpryrS(JR,IBIN,ogsm_yr) = MAX(cregpryrS(JR,IBIN,ogsm_yr) , NS_PRICE(JR,JREC,iyr) / 18.0)

                        TARGET_CO2 = TARGET_CO2 - NS_QUANT(JR,JREC) * 18.0
                        IF (NS_START(JR,JREC) .EQ. 0) NS_START(JR,JREC) = iyr

                        IF (RUN45Q .GT. 0 .AND. NS_START(JR,JREC) + 1989 .LE. CURCALYR .AND. NS_START(JR,JREC) + I_45Q_Duration + 1988 .GE. CURCALYR) THEN
                           CO2Purchased45Q(JR,IBIN,iyr) = CO2Purchased45Q(JR,IBIN,iyr) + NS_QUANT(JR,JREC) * 18.0 * 1000.0
                        END IF

                        WRITE(OGBUG1,3024) 'a', CURIRUN, iyr+1989, JR, isrc, jbin, irec, jrec, ibin, RUN45Q, NS_BIN(JR,jREC,iyr), TARGET_CO2,  & 
                           CO2Purchased(JR,IBIN,iyr), NS_QUANT(JR,jREC) ,cregpryrS(JR,IBIN,ogsm_yr) , NS_PRICE(JR,jREC,iyr), NS_START(JR,jREC)+1989, I_45Q_Duration, NS_QUANT(JR,JREC) * 18.0 * 1000.0
 3024                   Format(1X,"DBG_FTAB_INFO",",",A1,10(",",I4),5(",",f21.6),2(",",i4),",",f21.6)

                     ELSE
                        CO2Purchased(JR,IBIN,iyr) = CO2Purchased(JR,IBIN,iyr) + TARGET_CO2 * 1000.0
                        cregpryrS(JR,IBIN,ogsm_yr) = MAX(cregpryrS(JR,IBIN,ogsm_yr) , NS_PRICE(JR,jREC,iyr) / 18.0)

                        TARGET_CO2 = 0.0
                        IF (NS_START(JR,jREC) .EQ. 0) NS_START(JR,jREC) = iyr                      
                        IF (RUN45Q .GT. 0 .AND. NS_START(JR,JREC) + 1989 .LE. CURCALYR .AND. NS_START(JR,JREC) + I_45Q_Duration + 1988 .GE. CURCALYR) THEN
                           CO2Purchased45Q(JR,IBIN,iyr) = CO2Purchased45Q(JR,IBIN,iyr) + TARGET_CO2 * 1000.0
                        END IF

                        WRITE(OGBUG1,3024) 'b', CURIRUN, iyr+1989, JR, isrc, jbin, irec, jrec, ibin, RUN45Q, NS_BIN(JR,jREC,iyr), TARGET_CO2,  & 
                           CO2Purchased(JR,IBIN,iyr), NS_QUANT(JR,jREC) ,cregpryrS(JR,IBIN,ogsm_yr) , NS_PRICE(JR,jREC,iyr), NS_START(JR,jREC)+1989, I_45Q_Duration, TARGET_CO2 * 1000.0

                     END IF
                  END IF
               END DO
            ELSE
               CO2Purchased(JR,isrc,iyr) = (bse_availco2(JR,isrc,ogsm_yr) - nat_availco2(JR,isrc,ogsm_yr)) * 1000.0
               IF (isrc .eq. 7 .AND. RUN45Q .GT. 0) then
                  IF (JR .LE. l48rgn) THEN
                     CO2Purchased45Q(JR,isrc,iyr) = CO2_CCS(0,JR,2,iyr)
                  ELSE
                     CO2Purchased45Q(l48rgn+1,isrc,iyr) = CO2_CCS(0,0,2,iyr) - CO2_CCS(0,-1,2,iyr)
                  END IF
               END IF
            END IF
            CO2Purchased2(JR,isrc,iyr) = pur_co2_at_eor(JR,isrc,ogsm_yr) * 1000.0
            CO2Recycled(JR,isrc,iyr) = co2_recycle(JR,isrc,ogsm_yr) * 1000.0
            CO2Injected(JR,isrc,iyr) = co2_inject(JR,isrc,ogsm_yr) * 1000.0
            EORProduction(JR,isrc,iyr) = EORProduction(JR,isrc,iyr) + eor_oil_prod(JR,isrc,ogsm_yr)   
            GasProduction(JR,isrc,iyr) = GasProduction(JR,isrc,iyr) + eor_gas_prod(JR,isrc,ogsm_yr)   
         ENDDO

         DO isrc = 1 , max_src

            IF (bse_availco2(JR,isrc,ogsm_yr) /= 0.0 .OR. src_availco2(JR,isrc,ogsm_yr) /= 0.0) THEN
               WRITE(OGBUG1,3191) CURIRUN, CURIYR+1989, iyr, ogsm_yr, JR, isrc, &
                  CO2Available(JR,isrc,iyr), bse_availco2(JR,isrc,ogsm_yr), &
                  src_availco2(JR,isrc,ogsm_yr), &
                  CO2Purchased(JR,isrc,iyr), &
                  CO2Purchased45Q(JR,isrc,iyr), &
                  CO2Purchasedb(JR,isrc,iyr), nat_availco2(JR,isrc,ogsm_yr), &
                  CO2Purchased2(JR,isrc,iyr), pur_co2_at_eor(JR,isrc,ogsm_yr), &
                  CO2Recycled(JR,isrc,iyr), co2_recycle(JR,isrc,ogsm_yr), &
                  CO2Injected(JR,isrc,iyr), co2_inject(JR,isrc,ogsm_yr), &
                  EORProduction(JR,isrc,iyr), eor_oil_prod(JR,isrc,ogsm_yr), &
                  GasProduction(JR,isrc,iyr), eor_gas_prod(JR,isrc,ogsm_yr)
 3191          FORMAT(1X,"Wellon_CO2Purchased",6(":",I4),17(":",F21.6))
            END IF

         ENDDO
      ENDDO

!     ZERO OUT CO2 PURCHASE ARRAYS FOR YEARS NOT FILLED BY OGSM

      IF (ONCE) THEN
         DO IYR = 1 , CURIYR - 1
            DO s = 1 , max_src             ! source loop
               DO r = 1 , 8                ! regional loop
                  OGCO2PUR(r,s,iyr) = 0.0
                  OGCO2PUR2(r,s,iyr) = 0.0
               END DO
            END DO
         END DO
         ONCE = .false.
      END IF

!     Assign CO2 Available, Purchased, Recycled, and Used by SOURCE,
!     storing the 7 region sum in (empty) 8th regional slot

      DO s=1,max_src             ! source loop
         DO r=1,MNL48N            ! regional loop
            if (curiyr+baseyr-l48b4yr >= 1) then
            OGCO2AVL(R,s,curiyr) = bse_availco2(R,s,curiyr+baseyr-l48b4yr)*1000.
            OGCO2AVLs(R,s,curiyr) = src_availco2(R,s,curiyr+baseyr-l48b4yr)*1000.
          else
            OGCO2AVL(R,s,curiyr) = 0.
            OGCO2AVLs(R,s,curiyr) = 0.
          endif
          OGCO2PUR(r,s,curiyr) = CO2Purchased(r,s,curiyr)   ! conversion factors are in ftab.f, not here
          OGCO245Q(r,s,curiyr) = CO2Purchased45Q(r,s,curiyr)   ! conversion factors are in ftab.f, not here
          OGCO2PUR2(r,s,curiyr) = CO2Purchased2(r,s,curiyr)   ! conversion factors are in ftab.f, not here
          OGCO2REC(r,s,curiyr) = CO2Recycled(r,s,curiyr)
          OGCO2INJ(r,s,curiyr) = CO2Injected(r,s,curiyr)
        ENDDO                    ! end regional loop
         OGCO2AVL(MNL48N+1,s,curiyr) = sum(OGCO2AVL(1:MNL48N,s,curiyr))   ! assign six-region sum 
         OGCO2AVLs(MNL48N+1,s,curiyr) = sum(OGCO2AVLs(1:MNL48N,s,curiyr))   ! assign seven-region sum 
         OGCO2PUR(MNL48N+1,s,curiyr) = sum(OGCO2PUR(1:MNL48N,s,curiyr))
         OGCO245Q(MNL48N+1,s,curiyr) = sum(OGCO245Q(1:MNL48N,s,curiyr))
         OGCO2PUR2(MNL48N+1,s,curiyr) = sum(OGCO2PUR2(1:MNL48N,s,curiyr))
         OGCO2REC(MNL48N+1,s,curiyr) = sum(OGCO2REC(1:MNL48N,s,curiyr))
         OGCO2INJ(MNL48N+1,s,curiyr) = sum(OGCO2INJ(1:MNL48N,s,curiyr))
      ENDDO                      ! end source loop

!     Assign EOR oil production by project REGION
!     storing the 13 source sum in (spare) 13th source slot -- NO FTAB SUMS FROM 1 to 13

      do s=1,max_src          ! source loop
         do r=1,MNL48N               ! regional loop
          OGEORPRD(R,s,curiyr) = EORProduction(R,s,curiyr)
        ENDDO                   ! end source loop
         OGEORPRD(MNL48N+1,s,curiyr) = sum(OGEORPRD(1:MNL48N,s,curiyr)) 

         WRITE(ogbug1,'(a,I5,4F15.5)') 'co2report (mmcf)', s, OGCO2AVL(MNL48N+1,s,curiyr), OGCO2PUR(MNL48N+1,s,curiyr), &
                                                              OGCO2REC(MNL48N+1,s,curiyr), OGCO2INJ(MNL48N+1,s,curiyr)

      enddo                     ! end regional loop          

      DO R=1,L48RGN

!       OGQEORPR(R,curiyr) = sum(REPEORL48(R,1:10,curiyr))*1000.

        OGQEORPR(R,curiyr) = REPPRDL48(R,2,curiyr)*1000.
      ENDDO

!  ASSIGN WELLS to NEMS OUTPUT VARIABLES
      IF (CURIYR > L48HYR) THEN
      DO R=1,L48RGN
      DO K=1,L48FUEL
        ogwellsl48(r,k,curiyr) = wellsl48(1,r,k) + wellsl48(2,r,k)
                 if (ogwellsl48(r,k,curiyr) > 0) then
           ogsrl48(r,k,curiyr) = (sucwelll48(1,r,k)+sucwelll48(2,r,k))/ogwellsl48(r,k,curiyr)
        else
           ogsrl48(r,k,curiyr) = 0.
        endif
      enddo 
      enddo
      ENDIF


      RETURN
      END SUBROUTINE

!****************************************************************************************
!****************************************************************************************

       SUBROUTINE restable(m)

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsmugr'
       INCLUDE 'ogsml48'
       INCLUDE 'ogsmeor'
       INCLUDE 'ogsmout'

       real tinfresv(max_fuel,max_reg)
       real sclfac(max_fuel,max_reg)

       integer imc,yr,jyr

       sclfac = 1.0

      if(m == 1) then

! sets up initial values for each of the resource/reserves categories
! inferred oil and adgas reserves
       do i = 1,rcnt
         imc = rreg(i)
!           if(imc == 7) imc = 5     ! Northern Great Plains
            if((rproc(i) >= 3.and.rproc(i) <= 5).or.rproc(i) == 7) then
           infresv(2,imc,1) = infresv(2,imc,1)+rvalue(i)
               if(rproc(i) /= 7)infarsv(2,imc,1) = infarsv(2,imc,1)+rvalue2(i)
         else
           infresv(1,imc,1) = infresv(1,imc,1)+rvalue(i)
               if(rproc(i) /= 6.and.rproc(i) /= 8.and.rproc(i) /= 9)infarsv(1,imc,1) = infarsv(1,imc,1)+rvalue2(i)
         end if
       end do

! other components
       do i = 1,max_yr
         do j = 1,max_reg-1
           do k = 1,max_fuel
              infresv(k,j,i) = infresv(k,j,1)*sclfac(k,j)
              infarsv(k,j,i) = infarsv(k,j,1)
               undres(k,j,i) =  undres(k,j,1)
              undares(k,j,i) = undares(k,j,1)
           end do
         end do
       end do

       do j = 1,max_reg-1
         do k = 1,max_fuel
            tinfresv(k,j) = infresv(k,j,1)
         end do
       end do

       do i = 1,40
         do k = 4,max_fuel
           do j = 1,max_reg-1
                  if (k <= 4) infresv(k,j,i) = tinfresv(k,j)*(40-(i-1))
                  if (k > 4) infresv(k,j,i) = tinfresv(k,j)
                  if (j < 7) infresv(k,j,i) = infresv(k,j,i) - histresl48(j,k,l48hyr)   ! remove proved reserves
                  if (infresv(k,j,i) < 0.) infresv(k,j,i) = 0.
           end do
         end do
       end do

! store the initial values - before any adjustments
         do j = 1,max_reg-1
           do k = 1,max_fuel
              iinfresv(k,j) = infresv(k,j,1)
              iinfarsv(k,j) = infarsv(k,j,1)
               iundres(k,j) =  undres(k,j,1)
              iundares(k,j) = undares(k,j,1)
           end do
         end do

           DO R = 1,l48rgn
             OGEOYURR(R,1,L48B4YR-baseyr) = (UNDRES(1,R,1)+UNDRES(2,R,1)+UNDARES(3,R,1)+UNDARES(4,R,1)+ &
                                  UNDARES(5,R,1)+UNDARES(6,R,1)+UNDARES(7,R,1))/1000.
             OGEOYURR(R,2,L48B4YR-baseyr) = (UNDRES(3,R,1)+UNDRES(4,R,1))/1000.
             OGEOYINF(R,1,L48B4YR-baseyr) = (INFRESV(1,R,1) + INFRESV(2,R,1))/1000.
             OGEOYINF(R,2,L48B4YR-baseyr) = (INFRESV(3,R,1) + INFRESV(4,R,1))/1000.
             OGEOYUGR(R,1,L48B4YR-baseyr) = (UNDRES(5,R,1) + INFRESV(5,R,1))/1000.
             OGEOYUGR(R,2,L48B4YR-baseyr) = (UNDRES(6,R,1) + INFRESV(6,R,1))/1000.
             OGEOYUGR(R,3,L48B4YR-baseyr) = (UNDRES(7,R,1) + INFRESV(7,R,1))/1000.
             OGEOYAD(R,L48B4YR-baseyr)    = (UNDARES(1,R,1)+UNDARES(2,R,1)+INFARSV(1,R,1)+INFARSV(2,R,1))/1000.
         WRITE(ogbug1,*)'ugr5 ',curiyr,r,UNDRES(5,R,1)/1000.,INFRESV(5,R,1)/1000.
         WRITE(ogbug1,*)'ugr6 ',curiyr,r,UNDRES(6,R,1)/1000.,INFRESV(6,R,1)/1000.
         WRITE(ogbug1,*)'ugr7 ',curiyr,r,UNDRES(7,R,1)/1000.,INFRESV(7,R,1)/1000.
           ENDDO

      else

!lower 48 undiscovered (crude oil & lease condensates)
!  undres(fuel,region,yr) + undares(fuel,region,yr)
!  fuel = 1,2               fuel = 3,4,5,6,7

!lower 48 inferred reserves (crude oil)
!  infresv(fuel,region,yr)
!  fuel = 1,2

!lower 48 undiscovered conventional non-associated gas resource
!  undres(fuel,region,yr)
!  fuel = 3,4

!lower 48 inferred non-associated conventional gas reserves
!  infresv(fuel,region,yr)
!  fuel = 3,4

!lower 48 unconventional gas - tight gas
!  undres(fuel,region,yr)
!  fuel = 5

!lower 48 unconventional gas - coalbed methane
!  undres(fuel,region,yr)
!  fuel = 7

!lower 48 unconventional gas - shale gas
!  undres(fuel,region,yr)
!  fuel = 6

!lower 48 other unproved - AD Gas
!  undares(fuel,region,yr) + infarsv(fuel,region,yr)
!  fuel = 1,2                fuel = 1,2

!LOWER 48 UNCONVENTIONAL GAS - TIGHT GAS - INFERRED          
!  INFRESV(FUEL,REGION,YR)                                  
!  FUEL = 5                                                

!LOWER 48 UNCONVENTIONAL GAS - COALBED METHANE - INFERRED 
!  INFRESV(FUEL,REGION,YR)                               
!  FUEL = 7                                             

!LOWER 48 UNCONVENTIONAL GAS - SHALE GAS - INFERRED    
!  INFRESV(FUEL,REGION,YR)                            
!  FUEL = 6                                          

!      DO YR = l48b4yr-baseyr+1,lastyr
         yr = curiyr
         jyr = yr+baseyr-l48b4yr
         if (jyr >= max_yr) jyr = max_yr-1
         if (jyr >= 1) then
           DO R = 1,l48rgn
             OGEOYURR(R,1,YR) = (UNDRES(1,R,JYR)+UNDRES(2,R,JYR)+UNDARES(3,R,JYR)+UNDARES(4,R,JYR)+ &
                                  UNDARES(5,R,JYR)+UNDARES(6,R,JYR)+UNDARES(7,R,JYR))/1000.
             OGEOYURR(R,2,YR) = (UNDRES(3,R,JYR)+UNDRES(4,R,JYR))/1000.
   !            OGEOYINF(R,1,YR-1) = (INFRESV(1,R,JYR) + INFRESV(2,R,JYR) + infr_oil(R)*1000.0)/1000.
   !            OGEOYINF(R,2,YR-1) = (INFRESV(3,R,JYR) + INFRESV(4,R,JYR))/1000.
   !            OGEOYUGR(R,1,YR-1) = (UNDRES(5,R,JYR) + INFRESV(5,R,JYR) + infr_tht(R)*1000.0)/1000.
   !            OGEOYUGR(R,2,YR-1) = (UNDRES(6,R,JYR) + INFRESV(6,R,JYR) + infr_shl(R)*1000.0)/1000.
   !            OGEOYUGR(R,3,YR-1) = (UNDRES(7,R,JYR) + INFRESV(7,R,JYR) + infr_cbm(R)*1000.0)/1000.
   !            OGEOYAD(R,YR-1)    = (UNDARES(1,R,JYR)+UNDARES(2,R,JYR)+INFARSV(1,R,JYR)+INFARSV(2,R,JYR) + infr_adg(R)*1000.0)/1000.
             OGEOYINF(R,1,YR) = (INFRESV(1,R,JYR) + INFRESV(2,R,JYR))/1000.
             OGEOYINF(R,2,YR) = (INFRESV(3,R,JYR) + INFRESV(4,R,JYR))/1000.
             OGEOYUGR(R,1,YR) = (UNDRES(5,R,JYR) + INFRESV(5,R,JYR))/1000.
             OGEOYUGR(R,2,YR) = (UNDRES(6,R,JYR) + INFRESV(6,R,JYR))/1000.
             OGEOYUGR(R,3,YR) = (UNDRES(7,R,JYR) + INFRESV(7,R,JYR))/1000.
             OGEOYAD(R,YR)    = (UNDARES(1,R,JYR)+UNDARES(2,R,JYR)+INFARSV(1,R,JYR)+INFARSV(2,R,JYR))/1000.
   !*             OGEOYURR(R,1,YR) = 0.
   !*             OGEOYURR(R,2,YR) = 0.
   !*             OGEOYINF(R,1,YR) = 0.
   !*             OGEOYINF(R,2,YR) = 0.
   !*             OGEOYUGR(R,1,YR) = 0.
   !*             OGEOYUGR(R,2,YR) = 0.
   !*             OGEOYUGR(R,3,YR) = 0.
         WRITE(ogbug1,*)'ugr5 ',curiyr,r,UNDRES(5,R,1)/1000.,INFRESV(5,R,1)/1000.
         WRITE(ogbug1,*)'ugr6 ',curiyr,r,UNDRES(6,R,1)/1000.,INFRESV(6,R,1)/1000.
         WRITE(ogbug1,*)'ugr7 ',curiyr,r,UNDRES(7,R,1)/1000.,INFRESV(7,R,1)/1000.
           END DO
         END IF
!      END DO


       end if
       end subroutine


!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
!****************************************************************************************

       subroutine init_table(imode)

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsmugr'
       INCLUDE 'ogsml48'
       INCLUDE 'ogsmeor'

       integer imode

      if(imode == 1) then
        do l = 1,20000
          rname(l)   = ' '
          rproc(l)   = 0
          rreg(l)    = 0
          rvalue(l)  = 0.0
          rvalue2(l) = 0.0
          rdepth(l)  = 0.0
        end do
        rcnt = 0
        ucnt = 0
       do i = 1,max_yr
         do j = 1,max_reg
           do k = 1,max_fuel
              infresv(k,j,i) = 0.0
              infarsv(k,j,i) = 0.0
               undres(k,j,i) = 0.0
              undares(k,j,i) = 0.0
           end do
         end do
       end do

         do j = 1,max_reg
           do k = 1,max_fuel
              iinfresv(k,j) = 0.0
              iinfarsv(k,j) = 0.0
               iundres(k,j) = 0.0
              iundares(k,j) = 0.0
           end do
         end do
       end if

      if(imode == 2) then
       do i = 1,max_yr
         do j = 1,max_reg
           do k = 1,max_fuel
               undres(k,j,i) = 0.0
              undares(k,j,i) = 0.0
           end do
         end do
       end do
       end if


       end subroutine

!***************************************************************
!from AGG_ogsm.for
       SUBROUTINE AGG_ADJ

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsmugr'
       INCLUDE 'ogsml48'
       INCLUDE 'ogsmeor'
       INCLUDE 'ogsmout'
       INCLUDE 'ngtdmrep'

       INTEGER ires                                        !reservoir number

       INTEGER iwell                                       ! well class
       INTEGER ist                                         ! state number
       INTEGER iproc                                       ! process number
       INTEGER IR(max_res)                                 ! OGSM region number
       INTEGER ifuel(max_res)                              ! fuel type
       INTEGER iplay                                       ! play number
       INTEGER irrc                                        ! rrc number
       INTEGER iacc                                        ! resource access category
       INTEGER iflag                                       ! resource flag
       INTEGER isrc                                        ! CO2 source
       INTEGER DIST                                        ! DISTRICTS FOR PMM GAS PROD
       INTEGER imc,iy,ii,iii
       INTEGER yr(max_yr-1),y
       INTEGER IR1(max_res)                                 ! OGSM region number
       INTEGER ifuel1(max_res)                              ! fuel type

       INTEGER iyr, jyr, myr

        real voladj, temp, rdpth
       real delayedprd(ogdist,3:L48FUEL)
       real prdadj(ogdist,3:L48FUEL)
       real newprd(ogdist,3:L48FUEL)
       real oldprd(ogdist,3:L48FUEL)
       real ndradj(ogdist,3:L48FUEL)
       real odradj(ogdist,3:L48FUEL)
       real histadj(ogdist,3:L48FUEL)
       real vintage(l48fuel,max_yr-1), totwell(l48fuel)
       real vintage2(l48fuel,l48rgn,max_yr-1), totwell2(l48fuel,l48rgn)
       real prcadj, prcexp, prodadj, refprice
       real angl0(max_res)
       CHARACTER*2 st1                                     ! state abbreviation
       CHARACTER*1 well

!  STEP 1: USE THE RESERVOIR IDENTIFIERS TO DETERMINE THE RESERVOIR CATEGORIES
!       WELL CLASS 1 = EXPLORATORY/UNDISCOVERED
!       WELL CLASS 2 = DEVELOPMENTAL
!       FUEL TYPE 1 = SHALLOW OIL (PROCESS CODES 00,01,03-10,17
!       FUEL TYPE 2 = CO2 FLOODING (PROCESS CODE 02)
!       FUEL TYPE 3 = 
!       FUEL TYPE 4 = CONV GAS (PROCESS CODES 11,12,16)
!       FUEL TYPE 5 = TIGHT SANDS (PROCESS CODES 13,18)
!       FUEL TYPE 6 = DEVONIAN SHALE (PROCESS CODES 14,15,19,20, IFLAG = 3,4)
!       FUEL TYPE 7 = COALBED METHANE (PROCESS CODES 14,15,19,20, IFLAG = 1,2)

! IFLAG VALUES
!       1: OTHER GAS/OIL
!       2: DRY COAL
!       3: WET COAL
!       4: WET SHALE
!       5: DRY SHALE

        oldprd = 0.

      IF (itimeyr >= 1.and.curiyr <= lastyr) then

        DO ires=1,nres

          if (dtimed(ires)) then
            read (dresid(ires)(10:11),'(i2)') iproc
              read (dresid(ires)(3:4),'(a2)')   st1
              read (dresid(ires)(1:1),'(a1)')   well

              IR1(ires) = dregion(ires)
!              IF(IR1(ires) == 7)IR1(ires) = 5     ! Northern Great Plains
               if (well == 'U'.or.well == 'u') then
                 iwell = 1
              else
                 iwell = 2
              end if

              isrc = 0
               if (iproc == 3) then
                isrc = eco2code(ires)
              end if

              irrc  = INT(drrc(ires))
              iflag = dresflag(ires)
              rdpth = ddepth(ires)

              call dist_match(st1,irrc,IR1(ires),dist)     ! for PMM

               IF(iproc < 3.OR.iproc == 6.OR.(IPROC >= 8.AND.iproc <= 10) &
                     .OR.iproc == 17)THEN
                ifuel1(ires)=1                               ! FUELTYPE 1: CONVENTIONAL OIL
               ELSEIF((iproc >= 3.AND.IPROC <= 5).OR.IPROC == 7)THEN
                ifuel1(ires)=2                               ! FUELTYPE 2: EOR/ASR
               ELSEIF(iproc == 11.OR.iproc == 12.OR.iproc == 16)THEN
                ifuel1(ires)=4                               ! FUELTYPE 4: CONVENTIONAL DEEP GAS
               ELSEIF(iproc == 13.OR.iproc == 18.or.iproc == 22)THEN
                ifuel1(ires)=5                               ! FUELTYPE 5: TIGHT GAS
               ELSEIF(((iproc == 14.OR.iproc == 15).AND.(iflag == 3.or.iflag == 4)).OR. &
                        iproc == 20.OR.iproc == 21) THEN
                ifuel1(ires)=6                               ! FUELTYPE 6: GAS SHALES
               ELSEIF(((iproc == 14.OR.iproc == 15).AND.(iflag == 1.or.iflag == 2)).OR. &
                        iproc == 19.OR.iproc == 23) THEN
                ifuel1(ires)=7                               ! FUELTYPE 7: COALBED METHANE
              ENDIF

            if (ifuel1(ires) >= 4) then
              oldprd(dist,ifuel1(ires)) = oldprd(dist,ifuel1(ires)) + dprodgas(ires,itimeyr)/1000.
            END IF
          END IF

        END DO

        newprd = 0.
        DO ires=1,nres

          if (timed(ires)) then
               IF (itimeyr >= timedyr(ires)) THEN
            jyr = itimeyr - timedyr(ires) + 1
            read (aresid(ires)(10:11),'(i2)') iproc
              read (aresid(ires)(3:4),'(a2)')   st1
              read (aresid(ires)(1:1),'(a1)')   well

              IR(ires) = aregion(ires)
!             IF(IR(ires) == 7)IR(ires) = 5     ! Northern Great Plains
              if (well == 'U'.or.well == 'u') then
                 iwell = 1
              else
                 iwell = 2
              end if

              isrc = 0
              if (iproc == 3) then
                isrc = eco2code(ires)
              end if

              iplay = aplay_cde(ires)
              iacc  = aresacc(ires)
              irrc  = INT(arrc(ires))
              iflag = aresflag(ires)

              call dist_match(st1,irrc,IR(ires),dist)     ! for PMM

                  IF(iproc < 3.OR.iproc == 6.OR.(IPROC >= 8.AND.iproc <= 10) &
                        .OR.iproc == 17)THEN
                ifuel(ires)=1                               ! FUELTYPE 1: CONVENTIONAL OIL
                  ELSEIF((iproc >= 3.AND.IPROC <= 5).OR.IPROC == 7)THEN
                ifuel(ires)=2                               ! FUELTYPE 2: EOR/ASR
                  ELSEIF(iproc == 11.OR.iproc == 12.OR.iproc == 16)THEN
                ifuel(ires)=4                               ! FUELTYPE 4: CONVENTIONAL DEEP GAS
                  ELSEIF(iproc == 13.OR.iproc == 18.or.iproc == 22)THEN
                ifuel(ires)=5                               ! FUELTYPE 5: TIGHT GAS
                  ELSEIF(((iproc == 14.OR.iproc == 15).AND.(iflag == 3.or.iflag == 4)).OR. &
                           iproc == 20.OR.iproc == 21) THEN
                ifuel(ires)=6                               ! FUELTYPE 6: GAS SHALES
                  ELSEIF(((iproc == 14.OR.iproc == 15).AND.(iflag == 1.or.iflag == 2)).OR. &
                           iproc == 19.OR.iproc == 23) THEN
                ifuel(ires)=7                               ! FUELTYPE 7: COALBED METHANE
              ENDIF

                  if (ifuel(ires) >= 4) then
              newprd(dist,ifuel(ires)) = newprd(dist,ifuel(ires)) + eproddr(ires,jyr)*aprodgas(ires,1,1)/1000.
              oldprd(dist,ifuel(ires)) = oldprd(dist,ifuel(ires)) + eprodgas(ires,jyr)/1000. - eproddr(ires,jyr)*aprodgas(ires,1,1)/1000.
            END IF

          END IF
          END IF

        END DO

!  CALIBRATE LAST HISTORICAL YEAR
        IF(CURIYR.EQ.L48HYR) THEN
          HISTADJ = 1.
          DO R=1,OGDIST
            IF (DISTMAP(R,1) > 0 .AND. DISTMAP(R,1) <= L48rgn) THEN
              DO K=4,L48FUEL   ! PROCESS ONLY GAS FUEL TYPES
                if (oldprd(r,k) > 0.) histadj(r,k) = (OGENAGPRD(r,k-3,curiyr)-newprd(r,k))/oldprd(r,k)
              ENDDO
              write(ogbug1,*) 'histadj%',r,(histadj(r,k),k=4,l48fuel)
              write(ogbug1,*) 'histadjV',r,((OGENAGPRD(r,k-3,curiyr)-newprd(r,k)-oldprd(r,k)),k=4,l48fuel)
            ENDIF
          ENDDO
          DO ires=1,nres
!           IF (dtimed(ires).AND.(ifuel1(ires).ge.4.and.ifuel1(ires).ne.6)) then
            IF (dtimed(ires).AND.ifuel1(ires).ge.4) then
              read (dresid(ires)(10:11),'(i2)') iproc
              read (dresid(ires)(3:4),'(a2)')   st1
              irrc  = INT(drrc(ires))
              call dist_match(st1,irrc,IR1(ires),dist)     ! for PMM
              DO iyr=1,40
                prodadj = dprodgas(ires,iyr)*(histadj(dist,ifuel1(ires))-1)
                if (iyr.eq.1) write(ogbug1,*) 'prodadj',ires,st1,aplay_cde(ires),prodadj
                dprodgas(ires,iyr) = dprodgas(ires,iyr) + prodadj
                if (iyr.eq.1) oldprd(dist,ifuel1(ires)) = oldprd(dist,ifuel1(ires)) + prodadj/1000.
                if (iyr.gt.1.and.curiyr+iyr-1.LE.mnumyr)  &
                         ogenagprd(dist,ifuel1(ires)-3,curiyr+iyr-1) = ogenagprd(dist,ifuel1(ires)-3,curiyr+iyr-1) + prodadj/1000.
              ENDDO
            ENDIF
          ENDDO
        ENDIF


        prdadj=1.
        ndradj=1.
        odradj=1.
        DO R=1,OGDIST
          IF (DISTMAP(R,1) > 0 .AND. DISTMAP(R,1) <= L48rgn) THEN
            DO K=4,L48FUEL   ! PROCESS ONLY GAS FUEL TYPES
              DELAYEDPRD(R,K) = OGENAGPRD(R,K-3,curiyr)-OGRNAGPRD(R,K-3,curiyr)
                 if (OGENAGPRD(r,k-3,curiyr) > 0.) prdadj(r,k) = OGRNAGPRD(R,K-3,curiyr)/OGENAGPRD(R,K-3,curiyr)
                 if (itimeyr <= 3) newprd(r,k) = 0.
                 if (newprd(r,k) > 10.) ndradj(r,k) = (newprd(r,k)-delayedprd(r,k))/newprd(r,k)
                 if (ndradj(r,k) < 0.1) then
                   ndradj(r,K) = 0.1
                   temp = delayedprd(r,k) - newprd(r,k)*(1-ndradj(r,k))
                   if (oldprd(r,k) > 0.) odradj(r,k) = (oldprd(r,k)-temp)/oldprd(r,k)
                 endif
                 if (oldprd(r,k) > 0..and.newprd(r,K) <= 10.) odradj(r,k) = (oldprd(r,k)-delayedprd(r,k))/oldprd(r,k)
                 if (odradj(r,k) < 0.) odradj(r,k) = 0.
              write(ogbug1,201) curiyr+1989, itimeyr, r,k,delayedprd(r,k), ogrnagprd(r,k-3,curiyr), ogenagprd(r,k-3,curiyr), newprd(r,k),oldprd(r,k),  &
                                prdadj(r,K), ndradj(r,k), odradj(r,k)
            ENDDO
          ENDIF
        ENDDO
 201   format (1x,'delayed',I6,I4,2I3,5F12.3,3F10.3)

        vintage = 0.
        totwell = 0.

        DO ires=1,nres
          angl0(ires) = angl(ires)
          if (timed(ires)) then
               IF (itimeyr >= timedyr(ires)) THEN
            jyr = itimeyr - timedyr(ires) + 1
            read (aresid(ires)(10:11),'(i2)') iproc
            read (aresid(ires)(3:4),'(a2)')   st1
            IR(ires) = aregion(ires)
!                 IF(IR(ires) == 7)IR(ires) = 5     ! Northern Great Plains
            irrc  = INT(arrc(ires))
            call dist_match(st1,irrc,IR(ires),dist)     ! for PMM
                  if (iproc <= 10.or.iproc == 17) then     ! record select tight oil plays
                     if (jyr == 1) then
                 do myr = 1,max_yr-1
                           if (curiyr+myr-1 > l48hyr.and.curiyr+myr-1 <= mnumyr) then
                              if (aplay_cde(ires) == 2804.or.(aplay_cde(ires) >= 3110.and.aplay_cde(ires) <= 3115)) THEN
                       ogqshloil(1,curiyr+myr-1) = ogqshloil(1,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(8,curiyr+myr-1) = ogqshlgas(8,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                              elseif (aplay_cde(ires) == 4763) THEN
                       ogqshloil(2,curiyr+myr-1) = ogqshloil(2,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(5,curiyr+myr-1) = ogqshlgas(5,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                              elseif (aplay_cde(ires) == 5875.or.aplay_cde(ires) == 99926039) then
                       ogqshloil(3,curiyr+myr-1) = ogqshloil(3,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(4,curiyr+myr-1) = ogqshlgas(4,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                              elseif (aplay_cde(ires) >= 4747.and.aplay_cde(ires) <= 4749)   then
                       ogqshloil(4,curiyr+myr-1) = ogqshloil(4,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(15,curiyr+myr-1) = ogqshlgas(15,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                              elseif (aplay_cde(ires) == 4409)   then
                       ogqshloil(5,curiyr+myr-1) = ogqshloil(5,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(15,curiyr+myr-1) = ogqshlgas(15,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                              elseif (aplay_cde(ires) == 3904.OR.   &
                                 aplay_cde(ires) == 3920.OR.aplay_cde(ires) == 99905037.OR.   &
                                 aplay_cde(ires) == 99943037.OR.   &
                                 aplay_cde(ires) == 99949033.OR.aplay_cde(ires) == 99949037) then
                       ogqshloil(6,curiyr+myr-1) = ogqshloil(6,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(15,curiyr+myr-1) = ogqshlgas(15,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                              elseif (aplay_cde(ires) == 4473)   then
                       ogqshloil(7,curiyr+myr-1) = ogqshloil(7,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(15,curiyr+myr-1) = ogqshlgas(15,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                              elseif (aplay_cde(ires) == 1201.or.aplay_cde(ires) == 99904010) then
                       ogqshloil(8,curiyr+myr-1) = ogqshloil(8,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(15,curiyr+myr-1) = ogqshlgas(15,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                              elseif (aplay_cde(ires) == 4401) then
                       ogqshloil(9,curiyr+myr-1) = ogqshloil(9,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(15,curiyr+myr-1) = ogqshlgas(15,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                              elseif (aplay_cde(ires) == 6792.or.aplay_cde(ires) == 6793) then
                       ogqshloil(10,curiyr+myr-1) = ogqshloil(10,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(9,curiyr+myr-1) = ogqshlgas(9,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                              elseif (iproc == 17) then
                       ogqshloil(15,curiyr+myr-1) = ogqshloil(15,curiyr+myr-1) + eprodoil(ires,myr)/1000./365.
                       ogqshlgas(15,curiyr+myr-1) = ogqshlgas(15,curiyr+myr-1) + eprodgas(ires,myr)/1000000.
                     endif
                    endif
                  enddo
               endif
            endif
                  if (iproc >= 16.and.iproc /= 17) then
                do iyr=1,max_yr-1             ! adjust current year
                        if (jyr+iyr-1 <= max_yr-1) then
                           eprodgas(ires,jyr+iyr-1) = eprodgas(ires,jyr+iyr-1) +  &
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr) * aprodgas(ires,iyr,1)
                        endif
                        if (curiyr+iyr-1 <= lastyr) then
                    REPPRDL48(IR(ires),IFUEL(ires),CURIYR+iyr-1)    = REPPRDL48(IR(ires),IFUEL(ires),CURIYR+iyr-1) +   &
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr) * aprodgas(ires,iyr,1)/1000.
                    REPEXTL48(IR(ires),IFUEL(ires),CURIYR+iyr-1)    = REPEXTL48(IR(ires),IFUEL(ires),CURIYR+iyr-1) +   &
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr) * aprodgas(ires,iyr,1)/1000.
                    if (iyr > 1) OGENAGPRD(DIST,IFUEL(ires)-3,curiyr+IYR-1) = max(0.,OGENAGPRD(DIST,IFUEL(ires)-3,curiyr+IYR-1) +   &
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr) * aprodgas(ires,iyr,1)/1000.)
                    PMMDG(DIST,IFUEL(ires),curiyr+IYR-1) = max(0.,PMMDG(DIST,IFUEL(ires),curiyr+IYR-1) +   &
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr) * aprodgas(ires,iyr,1))
                    NGPLPRD(DIST,curiyr+IYR-1) = max(0.,NGPLPRD(DIST,curiyr+IYR-1) +  &                           ! mmb/d
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr)  &
                                     * aprodgas(ires,iyr,1) * angl(ires)*0.000001/365.)                           ! 
                    NGPLPRDET(DIST,curiyr+IYR-1) = max(0.,NGPLPRDET(DIST,curiyr+IYR-1) +  &                           ! mmb/d
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr)  &
                                     * aprodgas(ires,iyr,1) * angl(ires)*angplet(Ires)*0.000001/365.)     
                    NGPLPRDPR(DIST,curiyr+IYR-1) = max(0.,NGPLPRDPR(DIST,curiyr+IYR-1) +  &                           ! mmb/d
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr)  &
                                     * aprodgas(ires,iyr,1) * angl(ires)*angplpr(Ires)*0.000001/365.)    
                    NGPLPRDBU(DIST,curiyr+IYR-1) = max(0.,NGPLPRDBU(DIST,curiyr+IYR-1) +  &                           ! mmb/d
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr)  &
                                     * aprodgas(ires,iyr,1) * angl(ires)*angplbu(Ires)*0.000001/365.)   
                    NGPLPRDIS(DIST,curiyr+IYR-1) = max(0.,NGPLPRDIS(DIST,curiyr+IYR-1) +  &                           ! mmb/d
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr)  &
                                     * aprodgas(ires,iyr,1) * angl(ires)*angplis(Ires)*0.000001/365.)  
                    NGPLPRDPP(DIST,curiyr+IYR-1) = max(0.,NGPLPRDPP(DIST,curiyr+IYR-1) +  &                           ! mmb/d
                                     (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr)  &
                                     * aprodgas(ires,iyr,1) * angl(ires)*angplpp(Ires)*0.000001/365.) 
                  endif    
                enddo    
                REPSUCWLL48(2,IR(ires),IFUEL(ires),curiYR) = REPSUCWLL48(2,IR(ires),IFUEL(ires),curiYR) +  &
                                    (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr)
                sucwelll48(2,IR(ires),IFUEL(ires)) = sucwelll48(2,IR(ires),IFUEL(ires)) +  &
                                    (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr)
                wellsl48(2,IR(ires),IFUEL(ires)) = wellsl48(2,IR(ires),IFUEL(ires)) +  &
                                    (ndradj(dist,ifuel(ires))-1.)*eproddr(ires,jyr)
                eproddr(ires,jyr) = ndradj(dist,ifuel(ires))*eproddr(ires,jyr)

                     do ii=1,1                    ! determine drilling in next year
                  do iyr=1,max_yr-1          ! adjust price
                   refprice = ogwprng(ir(ires),timedyr(ires)+l48b4yr-baseyr)
                   if (itimeyr > 1.and.(ifuel(ires)==6)) then
                      prcexp = 1.0
                      if(ngexpvol(3,curiyr) > 1000.) then  ! increase drilling response to price based on LNG export growth
                        prcexp = (ngexpvol(3,curiyr)/1000.)**0.04
                        if(ngexpvol(3,curiyr)/ngexpvol(3,curiyr-1).gt.1.0) then
                          if(curiyr <= mnumyr-2) then
                            if(ngexpvol(3,curiyr+2)/ngexpvol(3,curiyr+1).gt.1.0) prcexp = (ngexpvol(3,curiyr)/1000.)**0.08
                          endif 
                        endif 
                      endif
                  ! changed to avoid prices in COVID years
                      if ((itimeyr > timedyr(ires)) .and. (timedyr(ires)+l48b4yr-1) >= 2020 .and. (timedyr(ires)+l48b4yr-1 <= 2024)) refprice = ogwprng(ir(ires),2019-baseyr+1)
                      prcadj = (ogwprng(ir(ires),curiyr)/refprice)**prcexp
                      if (prcadj > 1.) prcadj = prcadj**prcexp
                      if(ogrunop(6) > 0 .or. ogrunop(7) > 0) prcadj = prcadj * (1. - max(ogrunop(6),ogrunop(7))/100.)
                      if(ogrunop(8) > 0 .or. ogrunop(9) > 0) prcadj = prcadj * (1. + max(ogrunop(8),ogrunop(9))/100.)
                   else
                     prcadj = 1.
                   endif
                   if (ii == 1.and.iyr == 1) write(ogbug1,*) 'prcadj-prices0', curiyr+1989, ir(ires), ogwprng(ir(ires),curiyr), ogwprng(ir(ires),timedyr(ires)+l48b4yr-baseyr)
                   if (ii == 1.and.iyr == 1) write(ogbug1,*) 'prcadj-prices', curiyr+1989, ir(ires), refprice, ogwprng(ir(ires),curiyr)/refprice
                   if (ii == 1.and.iyr == 1) write(ogbug1,*) 'prcadj-adjust', curiyr+1989, ir(ires), prcexp, prcadj
                           if (jyr+iyr-1+ii <= max_yr-1) then
                              eprodgas(ires,jyr+iyr-1+ii) = eprodgas(ires,jyr+iyr-1+ii) +  &
                                                (prcadj-1.)*eproddr(ires,jyr+ii) * aprodgas(ires,iyr,1)
                           endif
                           if (curiyr+iyr-1+ii <= lastyr) then
                             angl(ires) = angl0(ires)
                             ! the price adjustment in the STEO years messes up NGPLs so removing the adjustment to NGPL production for projects started in year 1
                             if ((angl0(ires)>0.).and.(ifuel(ires)==6).and.(itimeyr==1)) then  
                                angl(ires) = angl0(ires)*0.
                             endif
                      REPPRDL48(IR(ires),IFUEL(ires),CURIYR+iyr-1+ii)    = REPPRDL48(IR(ires),IFUEL(ires),CURIYR+iyr-1+ii) +   &
                                       (prcadj-1.)*eproddr(ires,jyr+ii)  &
                                       * aprodgas(ires,iyr,1)/1000.
                      REPEXTL48(IR(ires),IFUEL(ires),CURIYR+iyr-1+ii)    = REPEXTL48(IR(ires),IFUEL(ires),CURIYR+iyr-1+ii) +   &
                                       (prcadj-1.)*eproddr(ires,jyr+ii)  &
                                       * aprodgas(ires,iyr,1)/1000.
                      OGENAGPRD(DIST,IFUEL(ires)-3,curiyr+IYR-1+ii) = max(0.,OGENAGPRD(DIST,IFUEL(ires)-3,curiyr+IYR-1+ii) +   &
                                       (prcadj-1.)*eproddr(ires,jyr+ii)  &
                                       * aprodgas(ires,iyr,1)/1000.)
                      PMMDG(DIST,IFUEL(ires),curiyr+IYR-1+ii) = max(0.,PMMDG(DIST,IFUEL(ires),curiyr+IYR-1+ii) +   &
                                       (prcadj-1.)*eproddr(ires,jyr+ii)  &
                                       * aprodgas(ires,iyr,1)/1000.)
                      NGPLPRD(DIST,curiyr+IYR-1+ii) = max(0.,NGPLPRD(DIST,curiyr+IYR-1+ii) +  &                           ! mmb/d
                                       (prcadj-1.)*eproddr(ires,jyr+ii)  &
                                       * aprodgas(ires,iyr,1) * angl(ires)*0.000001/365.)   
                      NGPLPRDET(DIST,curiyr+IYR-1+ii) = max(0.,NGPLPRDET(DIST,curiyr+IYR-1+ii) +  &                       ! mmb/d
                                       (prcadj-1.)*eproddr(ires,jyr+ii)  &
                                       * aprodgas(ires,iyr,1) * angl(ires)*angplet(ires)*0.000001/365.)   
                      NGPLPRDPR(DIST,curiyr+IYR-1+ii) = max(0.,NGPLPRDPR(DIST,curiyr+IYR-1+ii) +  &                       ! mmb/d
                                       (prcadj-1.)*eproddr(ires,jyr+ii)  &
                                       * aprodgas(ires,iyr,1) * angl(ires)*angplpr(ires)*0.000001/365.)   
                      NGPLPRDBU(DIST,curiyr+IYR-1+ii) = max(0.,NGPLPRDBU(DIST,curiyr+IYR-1+ii) +  &                       ! mmb/d
                                       (prcadj-1.)*eproddr(ires,jyr+ii)  &
                                       * aprodgas(ires,iyr,1) * angl(ires)*angplbu(ires)*0.000001/365.)   
                      NGPLPRDIS(DIST,curiyr+IYR-1+ii) = max(0.,NGPLPRDIS(DIST,curiyr+IYR-1+ii) +  &                       ! mmb/d
                                       (prcadj-1.)*eproddr(ires,jyr+ii)  &
                                       * aprodgas(ires,iyr,1) * angl(ires)*angplis(ires)*0.000001/365.)   
                      NGPLPRDPP(DIST,curiyr+IYR-1+ii) = max(0.,NGPLPRDPP(DIST,curiyr+IYR-1+ii) +  &                       ! mmb/d
                                       (prcadj-1.)*eproddr(ires,jyr+ii)  &
                                       * aprodgas(ires,iyr,1) * angl(ires)*angplpp(ires)*0.000001/365.)   
                    endif    
                  enddo    
                        if (curiyr+ii <= lastyr)  &
                    REPSUCWLL48(2,IR(ires),IFUEL(ires),curiYR+ii) = REPSUCWLL48(2,IR(ires),IFUEL(ires),curiYR+ii) +  &
                                      (prcadj-1.)*eproddr(ires,jyr+ii)
                        if (jyr+ii <= max_yr-1) eproddr(ires,jyr+ii) = prcadj*eproddr(ires,jyr+ii)
                enddo    
                do iyr=1,max_yr-1             ! print production by vintage
                        if (jyr+iyr-1 <= max_yr-1) then
                    vintage(ifuel(ires),iyr) = vintage(ifuel(ires),iyr) + eproddr(ires,jyr) * aprodgas(ires,iyr,1)
                    vintage2(ifuel(ires),ir(ires),iyr) = vintage2(ifuel(ires),ir(ires),iyr) + eproddr(ires,jyr) * aprodgas(ires,iyr,1)
                  endif    
                enddo    
                totwell(ifuel(ires)) = totwell(ifuel(ires)) + eproddr(ires,jyr)
                totwell2(ifuel(ires),ir(ires)) = totwell2(ifuel(ires),ir(ires)) + eproddr(ires,jyr)

               ! record select shale gas plays
                     IF (curiyr > l48hyr.and.ifuel(ires) == 6) then
                        if (aplay_cde(ires) >= 4561.and.aplay_cde(ires) <= 4563) then
                   ogqshlgas(1,curiyr) = ogqshlgas(1,curiyr) + eprodgas(ires,jyr)/1000000.
                   ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + eprodoil(ires,jyr)/1000./365.
                        elseif (aplay_cde(ires) == 4774.or.aplay_cde(ires) == 4775)  then
                   ogqshlgas(2,curiyr) = ogqshlgas(2,curiyr) + eprodgas(ires,jyr)/1000000.
                   ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + eprodoil(ires,jyr)/1000./365.
                        elseif (aplay_cde(ires) == 6261.or.aplay_cde(ires) == 6262)  then
                   ogqshlgas(3,curiyr) = ogqshlgas(3,curiyr) + eprodgas(ires,jyr)/1000000.
                   ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + eprodoil(ires,jyr)/1000./365.
                        elseif (aplay_cde(ires) >= 5861.and.aplay_cde(ires) <= 5863) then
                      ogqshlgas(4,curiyr) = ogqshlgas(4,curiyr) + eprodgas(ires,jyr)/1000000.
                      ogqshloil(3,curiyr) = ogqshloil(3,curiyr) + eprodoil(ires,jyr)/1000./365.
                        elseif (aplay_cde(ires) >= 4761.and.aplay_cde(ires) < 4763) then
                      ogqshlgas(5,curiyr) = ogqshlgas(5,curiyr) + eprodgas(ires,jyr)/1000000.
                      ogqshloil(2,curiyr) = ogqshloil(2,curiyr) + eprodoil(ires,jyr)/1000./365.
                        elseif (aplay_cde(ires) == 6361)   then
                   ogqshlgas(6,curiyr) = ogqshlgas(6,curiyr) + eprodgas(ires,jyr)/1000000.
                   ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + eprodoil(ires,jyr)/1000./365.
                        elseif (aplay_cde(ires) == 6761.or.(aplay_cde(ires) >= 6776.and.aplay_cde(ires) <= 6783)) then
                       ogqshlgas(7,curiyr) = ogqshlgas(7,curiyr) + eprodgas(ires,jyr)/1000000.
                       ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + eprodoil(ires,jyr)/1000./365.
                        elseif (aplay_cde(ires) == 6790.or.aplay_cde(ires) == 6791)  then
                   ogqshlgas(9,curiyr) = ogqshlgas(9,curiyr) + eprodgas(ires,jyr)/1000000.
                   ogqshloil(10,curiyr) = ogqshloil(10,curiyr) + eprodoil(ires,jyr)/1000./365.
                 else
                       ogqshlgas(15,curiyr) = ogqshlgas(15,curiyr) + eprodgas(ires,jyr)/1000000.
                       ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + eprodoil(ires,jyr)/1000./365.
                 endif
               END IF
                     IF (curiyr > l48hyr.and.ifuel(ires) == 5) then
                       ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + eprodoil(ires,jyr)/1000./365.
               END IF
            END IF
          END IF
          END IF
        END DO
        do ii = 5,l48fuel             ! print production by vintage
          do iyr=1,max_yr-1 
               if (jyr+iyr-1 <= max_yr-1) then
                  if (totwell(ii) > 0.) then
                vintage(ii,iyr) = vintage(ii,iyr) / totwell(ii)
              else
                vintage(ii,iyr) = 0.
              endif    
            endif    
          enddo    
          write(ogbug1,255) ii, curiyr+1989, totwell(ii), (vintage(ii,iyr),iyr=1,max_yr-1)
 255   format (1x,'vintage',I4,I6,3X,f12.0,<max_yr-1>F12.3)
          do iii=1,l48rgn
          do iyr=1,max_yr-1 
                  if (jyr+iyr-1 <= max_yr-1) then
                     if (totwell2(ii,iii) > 0.) then
                vintage2(ii,iii,iyr) = vintage2(ii,iii,iyr) / totwell2(ii,iii)
              else
                vintage2(ii,iii,iyr) = 0.
              endif    
            endif    
          enddo    
          write(ogbug1,265) ii, curiyr+1989, iii, totwell2(ii,iii), (vintage2(ii,iii,iyr),iyr=1,max_yr-1)
 265   format (1x,'vintage',I4,I6,I3,f12.0,<max_yr-1>F12.3)
          enddo    
        enddo    

        DO ires=1,nres
          if (dtimed(ires)) then
            jyr = itimeyr
            read (dresid(ires)(10:11),'(i2)') iproc
            read (dresid(ires)(3:4),'(a2)')   st1
            IR(ires) = dregion(ires)
!              IF(IR(ires) == 7)IR(ires) = 5     ! Northern Great Plains
            irrc  = INT(drrc(ires))
            call dist_match(st1,irrc,IR(ires),dist)     ! for PMM
               if (iproc >= 11.and.iproc <= 15) then
               REPPRDL48(IR1(ires),IFUEL1(ires),CURIYR)    = REPPRDL48(IR1(ires),IFUEL1(ires),CURIYR) +   &
                                  dprodgas(ires,jyr)*(odradj(dist,ifuel1(ires))-1.)/1000.
!              OGENAGPRD(DIST,IFUEL1(ires)-3,curiyr) = max(0.,OGENAGPRD(DIST,IFUEL1(ires)-3,curiyr) +   &
!                                 dprodgas(ires,jyr)*(odradj(dist,ifuel1(ires))-1.)/1000.)
               PMMDG(DIST,IFUEL1(ires),curIYR) = max(0.,PMMDG(DIST,IFUEL1(ires),curIYR) +   &
                                  dprodgas(ires,jyr)*(odradj(dist,ifuel1(ires))-1.)/1000.)
               NGPLPRD(DIST,curIYR) = max(0.,NGPLPRD(DIST,curIYR) +  &                           ! mmb/d
                                  dprodgas(ires,jyr)*(odradj(dist,ifuel1(ires))-1.)/1000.  &
                                  * dngl(ires)*0.000001/365.)                           ! 
               NGPLPRDET(DIST,curIYR) = max(0.,NGPLPRDET(DIST,curIYR) +  &                           ! mmb/d
                                  dprodgas(ires,jyr)*(odradj(dist,ifuel1(ires))-1.)/1000.  &
                                  * dngl(ires)*dngplet(ires)*0.000001/365.)                           ! 
               NGPLPRDPR(DIST,curIYR) = max(0.,NGPLPRDPR(DIST,curIYR) +  &                           ! mmb/d
                                  dprodgas(ires,jyr)*(odradj(dist,ifuel1(ires))-1.)/1000.  &
                                  * dngl(ires)*dngplpr(ires)*0.000001/365.)                           ! 
               NGPLPRDBU(DIST,curIYR) = max(0.,NGPLPRDBU(DIST,curIYR) +  &                           ! mmb/d
                                  dprodgas(ires,jyr)*(odradj(dist,ifuel1(ires))-1.)/1000.  &
                                  * dngl(ires)*dngplbu(ires)*0.000001/365.)                           ! 
               NGPLPRDIS(DIST,curIYR) = max(0.,NGPLPRDIS(DIST,curIYR) +  &                           ! mmb/d
                                  dprodgas(ires,jyr)*(odradj(dist,ifuel1(ires))-1.)/1000.  &
                                  * dngl(ires)*dngplis(ires)*0.000001/365.)                           ! 
               NGPLPRDPP(DIST,curIYR) = max(0.,NGPLPRDPP(DIST,curIYR) +  &                           ! mmb/d
                                  dprodgas(ires,jyr)*(odradj(dist,ifuel1(ires))-1.)/1000.  &
                                  * dngl(ires)*dngplpp(ires)*0.000001/365.)                           ! 
               dprodgas(ires,jyr) = dprodgas(ires,jyr) * (odradj(dist,ifuel1(ires)))
               ! record select shale gas plays
                  IF (curiyr > l48hyr.and.ifuel1(ires) == 6) then
                  if (dplay_cde(ires) >= 4561.and.dplay_cde(ires) <= 4563)   then
                   ogqshlgas(1,curiyr) = ogqshlgas(1,curiyr) + dprodgas(ires,jyr)/1000000.
                   ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + dprodoil(ires,jyr)/1000./365.
                  elseif (dplay_cde(ires) == 4774.or.dplay_cde(ires) == 4775)    then
                   ogqshlgas(2,curiyr) = ogqshlgas(2,curiyr) + dprodgas(ires,jyr)/1000000.
                   ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + dprodoil(ires,jyr)/1000./365.
                  elseif (dplay_cde(ires) == 6261.or.dplay_cde(ires) == 6262)    then
                   ogqshlgas(3,curiyr) = ogqshlgas(3,curiyr) + dprodgas(ires,jyr)/1000000.
                   ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + dprodoil(ires,jyr)/1000./365.
                  elseif (dplay_cde(ires) >= 5861.and.dplay_cde(ires) <= 5863) then
                      ogqshlgas(4,curiyr) = ogqshlgas(4,curiyr) + dprodgas(ires,jyr)/1000000.
                      ogqshloil(3,curiyr) = ogqshloil(3,curiyr) + dprodoil(ires,jyr)/1000./365.
                  elseif (dplay_cde(ires) >= 4761.and.dplay_cde(ires) < 4763) then
                      ogqshlgas(5,curiyr) = ogqshlgas(5,curiyr) + dprodgas(ires,jyr)/1000000.
                      ogqshloil(2,curiyr) = ogqshloil(2,curiyr) + dprodoil(ires,jyr)/1000./365.
                  elseif (dplay_cde(ires) == 6361)   then
                   ogqshlgas(6,curiyr) = ogqshlgas(6,curiyr) + dprodgas(ires,jyr)/1000000.
                   ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + dprodoil(ires,jyr)/1000./365.
                  elseif (dplay_cde(ires) == 6761.or.(dplay_cde(ires) >= 6776.and.dplay_cde(ires) <= 6783))   then
                       ogqshlgas(7,curiyr) = ogqshlgas(7,curiyr) + dprodgas(ires,jyr)/1000000.
                       ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + dprodoil(ires,jyr)/1000./365.
                  elseif (dplay_cde(ires) == 6790.or.dplay_cde(ires) == 6791)    then
                   ogqshlgas(9,curiyr) = ogqshlgas(9,curiyr) + dprodgas(ires,jyr)/1000000.
                   ogqshloil(10,curiyr) = ogqshloil(10,curiyr) + dprodoil(ires,jyr)/1000./365.
                 else
                       ogqshlgas(15,curiyr) = ogqshlgas(15,curiyr) + dprodgas(ires,jyr)/1000000.
                       ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + dprodoil(ires,jyr)/1000./365.
                 endif
               END IF
                  IF (curiyr > l48hyr.and.ifuel1(ires) == 5) then
                       ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + dprodoil(ires,jyr)/1000./365.
               END IF
            END IF
               if (iproc <= 10.or.iproc == 17) then     ! record select tight oil plays
                  if (curiyr > l48hyr) then
                     if (dplay_cde(ires) == 2804.or.(dplay_cde(ires) >= 3110.and.dplay_cde(ires) <= 3115)) THEN
                       ogqshloil(1,curiyr) = ogqshloil(1,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(8,curiyr) = ogqshlgas(8,curiyr) + dprodgas(ires,jyr)/1000000.
                     elseif (dplay_cde(ires) == 4763) then
                       ogqshloil(2,curiyr) = ogqshloil(2,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(5,curiyr) = ogqshlgas(5,curiyr) + dprodgas(ires,jyr)/1000000.
                     elseif (dplay_cde(ires) == 5875.or.dplay_cde(ires) == 99926039) then
                       ogqshloil(3,curiyr) = ogqshloil(3,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(4,curiyr) = ogqshlgas(4,curiyr) + dprodgas(ires,jyr)/1000000.
                     elseif (dplay_cde(ires) >= 4747.and.dplay_cde(ires) <= 4749)   then
                       ogqshloil(4,curiyr) = ogqshloil(4,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(15,curiyr) = ogqshlgas(15,curiyr) + dprodgas(ires,jyr)/1000000.
                     elseif (dplay_cde(ires) == 4409)   then
                       ogqshloil(5,curiyr) = ogqshloil(5,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(15,curiyr) = ogqshlgas(15,curiyr) + dprodgas(ires,jyr)/1000000.
                     elseif (dplay_cde(ires) == 3904.OR.   &
                           dplay_cde(ires) == 3920.OR.dplay_cde(ires) == 99905037.OR.   &
                           dplay_cde(ires) == 99943037.OR.   &
                           dplay_cde(ires) == 99949033.OR.dplay_cde(ires) == 99949037) then
                       ogqshloil(6,curiyr) = ogqshloil(6,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(15,curiyr) = ogqshlgas(15,curiyr) + dprodgas(ires,jyr)/1000000.
                     elseif (dplay_cde(ires) == 4473) then
                       ogqshloil(7,curiyr) = ogqshloil(7,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(15,curiyr) = ogqshlgas(15,curiyr) + dprodgas(ires,jyr)/1000000.
                     elseif (dplay_cde(ires) == 1201.or.dplay_cde(ires) == 99904010) then
                       ogqshloil(8,curiyr) = ogqshloil(8,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(15,curiyr) = ogqshlgas(15,curiyr) + dprodgas(ires,jyr)/1000000.
                     elseif (dplay_cde(ires) == 4401) then
                       ogqshloil(9,curiyr) = ogqshloil(9,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(15,curiyr) = ogqshlgas(15,curiyr) + dprodgas(ires,jyr)/1000000.
                     elseif (dplay_cde(ires) == 6792.or.dplay_cde(ires) == 6793) then
                       ogqshloil(10,curiyr) = ogqshloil(10,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(9,curiyr) = ogqshlgas(9,curiyr) + dprodgas(ires,jyr)/1000000.
                     elseif (iproc == 17.or.dresflag(ires) == 9) then
                       ogqshloil(15,curiyr) = ogqshloil(15,curiyr) + dprodoil(ires,jyr)/1000./365.
                       ogqshlgas(15,curiyr) = ogqshlgas(15,curiyr) + dprodgas(ires,jyr)/1000000.
                 endif
               endif
            endif
          END IF
        END DO


       END IF   ! ONLY DO IF ITIMEYR GE 1

      ! OVERWRITE TIGHT OIL AND SHALE GAS PLAY-LEVEL PRODUCTION WITH HISTORICAL/STEO VOLUMES 
      if(curiyr == l48hyr+1) then   ! set volumes using inputs from wlbasic
           DO r=1,10
             OGQSHLOIL(r,curiyr) = tOGQSHLOIL(r,1)
            OGQSHLGAS(r,curiyr) = tOGQSHLGAS(r,1)
           ENDDO
           OGQSHLOIL(15,curiyr) = tOGQSHLOIL(15,1)
         OGQSHLGAS(15,curiyr) = tOGQSHLGAS(15,1)
         endif
      if(curiyr >= l48hyr+2.and.curiyr <= l48hyr+steoyrs) then   ! adjust volumes using the volumes specified in wlbasic
           DO r=1,10
             OGQSHLOIL(r,curiyr) = OGQSHLOIL(r,curiyr)+tOGQSHLOIL(r,curiyr-l48hyr)
            OGQSHLGAS(r,curiyr) = OGQSHLGAS(r,curiyr)+tOGQSHLGAS(r,curiyr-l48hyr)
           ENDDO
           OGQSHLOIL(15,curiyr) = OGQSHLOIL(15,curiyr)+tOGQSHLOIL(15,curiyr-l48hyr)
         OGQSHLGAS(15,curiyr) = OGQSHLGAS(15,curiyr)+tOGQSHLGAS(15,curiyr-l48hyr)
         endif


       RETURN
       END SUBROUTINE

!****************************************************************************************
!****************************************************************************************
      SUBROUTINE GET_CO2()

      implicit none

      include 'parametr'     ! nems dimension parameters
      include 'ncntrl'       ! nems control variables
      include 'emmparm'
      include 'ogsmparm'     ! ogsm parameter file
      include 'ogsmbfw'      ! ogsm system variables
      include 'ogsml48'
      include 'emission'
      include 'ogsmout'
      include 'pmmout'      !pmm variables
      include 'ctus'

      integer ifuel,ir,iy,jyr
      integer censusmap(9)
      integer paddmap(5)
      integer coalmap(16)
      integer cnt7, cnt10, cnt11
      real co2avl(7,max_src,max_yr)
      real c_co2_factor  !factor to convert from MMtons carbon to BCF
      real pmm_cap       !amount that is actually captured from PMM
      real pmm_pot       !what could potentially be captured
      real filter        !filter to capture fact that some co2 cannot actually be captured

      data censusmap/1,1,1,3,1,2,2,5,6/
      data coalmap/1,1,1,1,1,1,1,1,2,3,3,2,5,5,5,6/
      data paddmap/1,3,2,5,6/
      data cnt7/0/
      data cnt10/0/
      data cnt11/0/

      co2avl = 0.
      c_co2_factor = 44./12.*18.  ! convert from MMtons carbon to billion cubic feet co2
      filter = 0.5  !only 50% of co2 is actually available

      do iy = 1,max_yr - 2
         jyr = l48b4yr-baseyr + iy   !-1
         if (jyr > mnumyr) jyr = mnumyr
           
!        In assignments that follow,  EMM (utilities) defines Power Plants planned capture
!                                    LFMM (refineries) defines Refineries planned + potential capture

         do ir = 1,7
            co2avl(ir,7,iy) = OGCO2qem(ir,jyr) * 0.001
            co2avl(ir,8,iy) = 0.0
            co2avl(ir,10,iy) = OGCO2qlf(ir,jyr) * 0.001
            co2avl(ir,11,iy) = 0.0

            write(OGBUG1,2311) curirun, curiyr+1989, curitr, ir, iy, jyr, l48b4yr, baseyr, max_yr, lastyr, &
               co2avl(ir,7,iy), OGCO2qem(ir,jyr) * 0.001, co2avl(ir,8,iy), co2avl(ir,10,iy), OGCO2qlf(ir,jyr) * 0.001, co2avl(ir,11,iy)
 2311       FORMAT(1X,"co2avl_1",10(":",I4),6(":",F15.3))

         enddo   
          
!        Save the values of available co2

         do ir = 1,max_reg-1           
            if (jyr  >  max(lastyr,2035-baseyr+1)) then
                co2avl(ir,7,iy) = co2avl(ir,7,iy-1)
                co2avl(ir,8,iy) = co2avl(ir,8,iy-1)
                co2avl(ir,10,iy) = co2avl(ir,10,iy-1)
                co2avl(ir,11,iy) = co2avl(ir,11,iy-1)

!               write(6,2312) curirun, curiyr+1989, curitr, ir, iy, jyr, l48b4yr, baseyr, max_yr, lastyr, &
!                  co2avl(ir,7,iy-1), OGCO2qem(ir,jyr-1) * 0.001, co2avl(ir,8,iy-1), OGCO2qlf(ir,jyr-1) * 0.001
 2312           FORMAT(1X,"co2avl_2",10(":",I4),4(":",F15.3))

             endif
             nat_availco2(ir,7,iy) = co2avl(ir,7,iy)   
             nat_availco2(ir,8,iy) = co2avl(ir,8,iy)  
             nat_availco2(ir,10,iy) = co2avl(ir,10,iy)
             nat_availco2(ir,11,iy) = co2avl(ir,11,iy)

             src_availco2(ir,7,iy) = co2avl(ir,7,iy)   
             src_availco2(ir,8,iy) = co2avl(ir,8,iy)  
             src_availco2(ir,10,iy) = co2avl(ir,10,iy)
             src_availco2(ir,11,iy) = co2avl(ir,11,iy)

             if (sum(co2avl(1:(max_reg-1),7,iy)) > 13.5*18.*filter) then
                if (ir == 1) cnt7 = cnt7+1
                cregpr(ir,7) = max(cregpr(ir,7)*(1.-cnt7/10.),0.)
             endif

             WRITE(ogbug1,1894) 'NEMS_CO2', jyr+1989, ir, co2avl(ir,7,iy), co2avl(ir,8,iy),co2avl(ir,10,iy), co2avl(ir,11,iy), &
                OGCO2qem(ir,jyr) * 0.001, OGCO2qlf(ir,jyr) * 0.001
             WRITE(ogbug1,1894) 'OGSM_CO2', jyr+1989, ir, nat_availco2(ir,7,iy), nat_availco2(ir,8,iy), nat_availco2(ir,10,iy),  & 
                nat_availco2(ir,11,iy), cregpr(ir,7), cregpr(ir,10), cregpr(ir,11)
 1894        FORMAT(A12,1x,2(i4),7(F12.3,1x))                 

!            write(6,2313) curirun, curiyr+1989, curitr, ir, iy, jyr, l48b4yr, baseyr, max_yr, lastyr, &
!               co2avl(ir,7,iy), OGCO2qem(ir,jyr) * 0.001, co2avl(ir,8,iy), OGCO2qlf(ir,jyr) * 0.001
 2313       FORMAT(1X,"co2avl_3",10(":",I4),4(":",F15.3))

         enddo
      enddo
         
      RETURN
      END SUBROUTINE
!***************************************************************
!from READ_COST.FOR
       SUBROUTINE READ_COSTADJ

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsml48'


!  read the cost file

      LOGICAL CONVRESP                           ! FUNCTION TO CONVERT RESPONSE TO LOGICAL
      INTEGER D                                  ! REGION/DEPTH INDEX VARIABLES
      INTEGER SRC                                ! CO2 SOURCE INDEX VARIABLE
      INTEGER PRC                                ! NO. PROCESSES INDEX VARIABLE
      INTEGER YR                                 ! YEAR INDEX VARIABLE
      INTEGER YEAR                               ! HISTORY YEARS
      INTEGER nreg                               ! total number of cost regions
      INTEGER num_proc                           ! number of processes - for discount rates
      INTEGER num_year                           ! number of years of data for CPI indices

      CHARACTER*2 REG                            ! TEMP TO READ REGION
      CHARACTER*3 RESP1,RESP2                    ! READ TEMP TO CONVERT TO LOGICAL

      LOW48 = IFILE1

!*****************************************************************************
!
!
!  Section 1: Resource/Process Independent Capital Costs
!
!*****************************************************************************
!
      CALL OGSM_NEXTDATA(LOW48)
      READ(LOW48,*)NREG                           ! NO. REGIONS
!!!VERTICAL DRILLING AND COMPLETION FOR OIL
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OIL_DWCM_F(R,1),OIL_DWCD_F(R,1),OIL_DWCK_F(R,1), &
         OIL_DWCA_F(R,1), &
             OIL_DWCB_F(R,1),OIL_DWCC_F(R,1)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OIL_DWCM_F(R,2),OIL_DWCD_F(R,2),OIL_DWCK_F(R,2), &
         OIL_DWCA_F(R,2), &
             OIL_DWCB_F(R,2),OIL_DWCC_F(R,2)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OIL_DWCM_F(R,3),OIL_DWCD_F(R,3),OIL_DWCK_F(R,3), &
         OIL_DWCA_F(R,3), &
             OIL_DWCB_F(R,3),OIL_DWCC_F(R,3)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OIL_DWCM_F(R,4),OIL_DWCD_F(R,4),OIL_DWCK_F(R,4), &
         OIL_DWCA_F(R,4), &
             OIL_DWCB_F(R,4),OIL_DWCC_F(R,4)
      ENDDO

!!!COST TO EQUIP A PRIMARY PRODUCER
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,NPRM_F(R,1),MPRD_F(R,1),NPRK_F(R,1),NPRA_F(R,1), &
             NPRB_F(R,1),NPRC_F(R,1)
        do d = 2,4
           nprm_F(r,d) = nprm_F(r,1)
           mprd_F(r,d) = mprd_F(r,1)
           nprk_F(r,d) = nprk_F(r,1)
           npra_F(r,d) = npra_F(r,1)
           nprb_F(r,d) = nprb_F(r,1)
           nprc_F(r,d) = nprc_F(r,1)
        end do
      ENDDO

!!!VERTICAL DRILLING AND COMPLETION FOR GAS
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,GAS_DWCM_F(R,1),GAS_DWCD_F(R,1),GAS_DWCK_F(R,1), &
         GAS_DWCA_F(R,1), &
             GAS_DWCB_F(R,1),GAS_DWCC_F(R,1)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,GAS_DWCM_F(R,2),GAS_DWCD_F(R,2),GAS_DWCK_F(R,2), &
         GAS_DWCA_F(R,2), &
             GAS_DWCB_F(R,2),GAS_DWCC_F(R,2)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,GAS_DWCM_F(R,3),GAS_DWCD_F(R,3),GAS_DWCK_F(R,3), &
         GAS_DWCA_F(R,3), &
             GAS_DWCB_F(R,3),GAS_DWCC_F(R,3)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,GAS_DWCM_F(R,4),GAS_DWCD_F(R,4),GAS_DWCK_F(R,4), &
         GAS_DWCA_F(R,4), &
             GAS_DWCB_F(R,4),GAS_DWCC_F(R,4)
      ENDDO

 !!!D Y HOLE
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,DRY_DWCM_F(R,1),DRY_DWCD_F(R,1),DRY_DWCK_F(R,1), &
         DRY_DWCA_F(R,1), &
             DRY_DWCB_F(R,1),DRY_DWCC_F(R,1)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,DRY_DWCM_F(R,2),DRY_DWCD_F(R,2),DRY_DWCK_F(R,2), &
         DRY_DWCA_F(R,2), &
             DRY_DWCB_F(R,2),DRY_DWCC_F(R,2)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,DRY_DWCM_F(R,3),DRY_DWCD_F(R,3),DRY_DWCK_F(R,3), &
         DRY_DWCA_F(R,3), &
             DRY_DWCB_F(R,3),DRY_DWCC_F(R,3)
      ENDDO

      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,DRY_DWCM_F(R,4),DRY_DWCD_F(R,4),DRY_DWCK_F(R,4), &
         DRY_DWCA_F(R,4), &
             DRY_DWCB_F(R,4),DRY_DWCC_F(R,4)
      ENDDO

   !! WORKOVER COSTS
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,WRKM_F(R,1),WRKD_F(R,1),WRKK_F(R,1),WRKA_F(R,1), &
             WRKB_F(R,1),WRKC_F(R,1)
        do d = 2,4
           wrkm_F(r,d) = wrkm_F(r,1)
           wrkd_F(r,d) = wrkd_F(r,1)
           wrkk_F(r,d) = wrkk_F(r,1)
           wrka_F(r,d) = wrka_F(r,1)
           wrkb_F(r,d) = wrkb_F(r,1)
           wrkc_F(r,d) = wrkc_F(r,1)
        end do
      ENDDO

!
!  Section 2: Resource/Process Specific Capital Costs
!
  !!!COST TO CONVERT A PRIMARY TO SECONDARY WELL
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,PSWM_F(R,1),PSWD_F(R,1),PSWK_F(R,1),PSWA_F(R,1), &
             PSWB_F(R,1),PSWC_F(R,1)
        do d = 2,4
           pswm_F(r,d) = pswm_F(r,1)
           pswd_F(r,d) = pswd_F(r,1)
           pswk_F(r,d) = pswk_F(r,1)
           pswa_F(r,d) = pswa_F(r,1)
           pswb_F(r,d) = pswb_F(r,1)
           pswc_F(r,d) = pswc_F(r,1)
        end do
      ENDDO

   !! COST TO CONVERT A PRODUCER TO INJECTOR
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,PSIM_F(R,1),PSID_F(R,1),PSIK_F(R,1),PSIA_F(R,1), &
             PSIB_F(R,1),PSIC_F(R,1)
        do d = 2,4
           psim_F(r,d) = psim_F(r,1)
           psid_F(r,d) = psid_F(r,1)
           psik_F(r,d) = psik_F(r,1)
           psia_F(r,d) = psia_F(r,1)
           psib_F(r,d) = psib_F(r,1)
           psic_F(r,d) = psic_F(r,1)
        end do
      ENDDO

   !! FACILITIES UPGRADE COST
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,FACUPM_F(R,1),FACUPD_F(R,1),FACUPK_F(R,1), &
             FACUPA_F(R,1),FACUPB_F(R,1),FACUPC_F(R,1)
        do d = 2,4
           facupm_F(r,d) = facupm_F(r,1)
           facupd_F(r,d) = facupd_F(r,1)
           facupk_F(r,d) = facupk_F(r,1)
           facupa_F(r,d) = facupa_F(r,1)
           facupb_F(r,d) = facupb_F(r,1)
           facupc_F(r,d) = facupc_F(r,1)
        end do
      ENDDO

   !! GAS FACILITIES
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,FACGM_F(R,1),FACGD_F(R,1),FACGK_F(R,1),FACGA_F(R,1), &
             FACGB_F(R,1),FACGC_F(R,1)
        do d = 2,4
           facgm_F(r,d) = facgm_F(r,1)
           facgd_F(r,d) = facgd_F(r,1)
           facgk_F(r,d) = facgk_F(r,1)
           facga_F(r,d) = facga_F(r,1)
           facgb_F(r,d) = facgb_F(r,1)
           facgc_F(r,d) = facgc_F(r,1)
        end do
      ENDDO
!
!
!  Section 3: Fixed Operating Costs
!
!*****************************************************************************
!
!!!FIXED ANNUAL COST FOR OIL ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
         READ(LOW48,*)REG,OMOM_F(R,1),OMOD_F(R,1),OMOK_F(R,1),OMOA_F(R,1), &
          OMOB_F(R,1), &
             OMOC_F(R,1)
        do d = 2,4
           omom_F(r,d) = omom_F(r,1)
           omod_F(r,d) = omod_F(r,1)
           omok_F(r,d) = omok_F(r,1)
           omoa_F(r,d) = omoa_F(r,1)
           omob_F(r,d) = omob_F(r,1)
           omoc_F(r,d) = omoc_F(r,1)
        end do

      ENDDO

!!!FIXED ANNUAL COST FOR GAS ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OMGM_F(R,1),OMGD_F(R,1),OMGK_F(R,1),OMGA_F(R,1), &
         OMGB_F(R,1), &
             OMGC_F(R,1)
        do d = 2,4
           omgm_F(r,d) = omgm_F(r,1)
           omgd_F(r,d) = omgd_F(r,1)
           omgk_F(r,d) = omgk_F(r,1)
           omga_F(r,d) = omga_F(r,1)
           omgb_F(r,d) = omgb_F(r,1)
           omgc_F(r,d) = omgc_F(r,1)
        end do

      ENDDO

!!!ANNUAL COST FOR SECONDARY PRODUCTION ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OPSECM_F(R,1),OPSECD_F(R,1),OPSECK_F(R,1), &
         OPSECA_F(R,1),OPSECB_F(R,1), &
             OPSECC_F(R,1)
        do d = 2,4
           opsecm_F(r,d) = opsecm_F(r,1)
           opsecd_F(r,d) = opsecd_F(r,1)
           opseck_F(r,d) = opseck_F(r,1)
           opseca_F(r,d) = opseca_F(r,1)
           opsecb_F(r,d) = opsecb_F(r,1)
           opsecc_F(r,d) = opsecc_F(r,1)
        end do

      ENDDO

!!!LIFTING COST ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OMLM_F(R,1),OMLD_F(R,1),OMLK_F(R,1), &
         OMLA_F(R,1), &
             OMLB_F(R,1),OMLC_F(R,1)
        do d = 2,4
           omlm_F(r,d) = omlm_F(r,1)
           omld_F(r,d) = omld_F(r,1)
           omlk_F(r,d) = omlk_F(r,1)
           omla_F(r,d) = omla_F(r,1)
           omlb_F(r,d) = omlb_F(r,1)
           omlc_F(r,d) = omlc_F(r,1)
        end do

      ENDDO

!!!SECONDARY WORKOVER COST ($/WELL)
      CALL OGSM_NEXTDATA(LOW48)
      DO R = 1,NREG
        READ(LOW48,*)REG,OMSWRM_F(R,1),OMSWRD_F(R,1),OMSWRK_F(R,1), &
         OMSWRA_F(R,1), &
             OMSWRB_F(R,1),OMSWRC_F(R,1)
        do d = 2,4
           omswrm_F(r,d) = omswrm_F(r,1)
           omswrd_F(r,d) = omswrd_F(r,1)
           omswrk_F(r,d) = omswrk_F(r,1)
           omswra_F(r,d) = omswra_F(r,1)
           omswrb_F(r,d) = omswrb_F(r,1)
           omswrc_F(r,d) = omswrc_F(r,1)
        end do

      ENDDO


       RETURN
       END SUBROUTINE
