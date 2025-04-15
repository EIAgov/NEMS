! $Header: m:/default/source/RCS/uefd.f,v 1.519 2021/05/17 15:11:14 LC2 Exp $
!===================================================================================================================================================
! Electricity Fuel Dispatch(EFD) module.

! the module "efd_row_col" contains declarations and data shared throughout the subroutines in this file.  The
! purpose is store LP coefficients in dynamically-declared arrays for the AIMMS EFD interface.
! see the module's code in efd_row_col.f90 and see uaimms.f for aimms interface routines that use it.
!
!     ELEFD
!
      SUBROUTINE ELEFD(IYR)
!
      IMPLICIT NONE
!

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'emission'
      include'cdsparms'
      include'control'
      include'dispuse'
      include'dispcrv'
      include'dispin'
      include'dispinyr'
      include'dispout'
      include'dispett'
      include'bildout'
      include'uettout'
      include'uefdout'
      include'udatout'
      include'elout'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmnercr'
      include'postpr'
      include'eusprc'
      include'edbdef'
      include'efpint'
      include'uso2grp'
      include'ecpcntl'
      include'coalemm'
      include'uecpout'
      include'cogen'
      include'plntctl'
      include'intout'
      include'ngtdmrep'
      include'convfact'
      include'coalrep'
      include'csapr'
      include'emmemis'
      include'ecp_nuc'
      include'emm_aimms'
      include'ccatsdat'
!
      COMMON/COFSHRP/COFGENPC,TOTGENPC,COFGENPN,TOTGENPN
      REAL*4 COFGENPC(ECP_D_CAP,NDREG)
      REAL*4 TOTGENPC(ECP_D_CAP,NDREG)
      REAL*4 COFGENPN(ECP_D_CAP,MNUMNR)
      REAL*4 TOTGENPN(ECP_D_CAP,MNUMNR)

      REAL*8 EEITAJ_ANNUAL, INTRUP_ANNUAL, EOUNT_EOUIPP_ANNUAL, GEN_REQ_ANNUAL

      INTEGER IP, TFR, IC, JEFD, IS, ISL
      INTEGER J,YR,ISP,IYR,ISOL,IRG,IECP,JK,FULLYR,IFL,IFLRG,IOWN,JRG,CRG,ICAP,ISO2
      INTEGER IE2,I,IGRP,IFP,mclock,ivls,MRG,GRP,SEG,VLS,IRNK
      INTEGER NUMTABS
      PARAMETER (NUMTABS = 2)  !number of database tables written in this routine
      INTEGER ISTEP
      INTEGER XPRESEFD
      integer one
      real zero
      REAL*8 t0,t,RPSOVR(MNUMYR),DUMMY
      REAL*8 FACTOR,IMPANN,EXPANN,TOTHRS,ADJUST,INTRUP
!
!     VARIABLES TO CALL FILE_MGR FOR INITIALIZATION
!
      CHARACTER*80 FILENM
      LOGICAL NEW
      INTEGER FILE_MGR,G
      EXTERNAL FILE_MGR
!
      CHARACTER*2 COL

      real ERDSCAR2(MNUMNR)
      COMMON /WAX_PRC/ ERDSCAR2

      INTEGER*4      VALCNT
      COMMON /EFD_VALS/VALCNT
      VALCNT = 0
!
!     call subroutine to initialize GPS subsidies
!
      CALL GPSINIT
!     --- INITIALIZE VALUES FOR NEMS DB
      LOOPING  = 0
      NUMCOLS  = 0
      DYNSTM = ' '
      CHCOLVALS = ' '
      CHCOLV = ' '
      COLVALS = 0.0
      COLV = 0.0
!
      COL = ' :'                                               !//EMMDB//
!
      ISOL = 1

      ERDSCAR2 = 0.0    !initialize
!
!   initialize max gas/oil share variables
!
      DO IC = 1, 2
      DO ICAP = 1, ECP_D_CAP
        DO TFR = 1, MAXNFR
         EDMXGS(IC,ICAP,TFR) = 0.0
         EDMXRS(IC,ICAP,TFR) = 0.0
         EDMXDS(IC,ICAP,TFR) = 0.0
         FSCAPWT(IC,ICAP,TFR) = 0.0
        ENDDO
      ENDDO
      ENDDO
!
!     Initialize ECP Max Gas, Oil Shares and ECP Gas, Oil and Coal Prices
!
      TPMXGAS = 0.0
      TPMXOIL = 0.0
      TPMXCOL = 0.0
      TPMXWGT = 0.0
      TPMXGAS_MR = 0.0
      TPMXOIL_MR = 0.0
      TPMXCOL_MR = 0.0
      TPMXWGT_MR = 0.0
      TPGASPRC = 0.0
      TPGASWGT = 0.0
      TPDISPRC = 0.0
      TPDISWGT = 0.0
      TPRESPRC = 0.0
      TPRESWGT = 0.0
      TPCOLPRC = 0.0
      TPCOLPRA = 0.0
      TPCOLWGT = 0.0
      TPNUCPRC = 0.0
      TPNUCWGT = 0.0

!   initialize final fuel share variables
!
      DO IC = 1, 2
      DO ICAP = 1, ECP_D_CAP
        DO TFR = 1, MAXNFR
         FGSSH(IC,ICAP,TFR) = 0.0
         FCLSH(IC,ICAP,TFR) = 0.0
         FOLSH(IC,ICAP,TFR) = 0.0
         FWDSH(IC,ICAP,TFR) = 0.0
        ENDDO
      ENDDO
      ENDDO
!
!     INITIALIZE FUEL CONSUMPTION, GENERATION, AND ARRAYS TO ZERO
!
      DO IGRP = 1 , EMM_D_GRP
         ULREVS(IGRP) = 0.0
         ULENGREVS(IGRP) = 0.0
         ULVCST(IGRP) = 0.0
         ULTGEN(IGRP) = 0.0
         ULSO2P(IGRP) = 0.0
         ULNOXP(IGRP) = 0.0
         ULRPSP(IGRP) = 0.0
         ULHGP(IGRP) = 0.0
         ULGHG(IGRP) = 0.0
         ULCCS_INV(IGRP) = 0.0
         ULCCS_FOM(IGRP) = 0.0
         ULCCS_VOM(IGRP) = 0.0
         DO ISP = 1 , EFD_D_MSP
            ECDSPE(IGRP,ISP) = 0.0
            ECDSPE_ALT(IGRP,ISP) = 0.0
            ECDSPF(IGRP,ISP) = 0.0
            ECDSPF_ALT(IGRP,ISP) = 0.0
            ECDSPC(IGRP,ISP) = 0.0
            ULGENS(ISP,IGRP) = 0.0
            DO IVLS = 1 , EFD_D_MVS
               ELGENE(IVLS,ISP,IGRP) = 0.0
            END DO
         END DO
         DO IFP = 1 , EFD_D_FPP
            ULGENE(IFP,IGRP) = 0.0
            ULBTUE(IFP,IGRP) = 0.0
            ULSO2W(IFP,IGRP) = 0.0
            ULNOXW(IFP,IGRP) = 0.0
            ULCO2W(IFP,IGRP) = 0.0
            ULCARW(IFP,IGRP) = 0.0
            ULHGQ(IFP,IGRP) = 0.0
            ULFLCST(IFP,IGRP) =  0.0
         END DO
      END DO
!
      IF ((CURIYR+UHBSYR .EQ. UESTYR) .AND. (CURITR .EQ. 1)) THEN
         BTU_CCS = 0.0
         CO2_CCS = 0.0
      ENDIF


      DO IFL = 1 , ENFLTP
         DO IRG = 1 , EFD_D_MFRG
            URFUEL(IFL,IRG) = 0.0
            UXFUEL(IFL,IRG) = 0.0
            DO IFLRG = 1 , UNFLRG(IFL)
               IF (CURIYR+UHBSYR .GT. UESTYR) THEN
                  IF (IFL .NE. UIRH) THEN
                     UQFUEL(IFL,IRG,IFLRG) = MIN(UQFUEL(IFL,IRG,IFLRG),0.000001)
                  ELSE
                     UQFUEL(IFL,IRG,IFLRG) = 0.0
                  END IF
               ELSE
                  UQFUEL(IFL,IRG,IFLRG) = 0.0
               END IF
            END DO
            DO ISP = 1 , EFD_D_MSP
               SQFUEL(IFL,IRG,ISP) = 0.0
            END DO
         END DO
!
         DO IRG = 1 , MNUMNR
            DO IOWN = 1 , USW_OWN
               UQFGENN(IFL,IRG,IOWN) = 0.0
               UQFCONN(IFL,IRG,IOWN) = 0.0
            END DO
            UTSO2N(IFL,IRG) = 0.0
            UTNOXN(IFL,IRG) = 0.0
            UTCO2N(IFL,IRG) = 0.0
            UTCO1N(IFL,IRG) = 0.0
            UTCARN(IFL,IRG) = 0.0
            UTHGN(IFL,IRG) = 0.0
         END DO
!
         DO IRG = 1 , MNUMCR
           IF (IFL .EQ. UIWD) THEN
            DO IOWN = 1 , USW_OWN
               UQBIOM(IRG,IOWN) = 0.0
               URBIOM(IRG,IOWN) = 0.0
            END DO
           END IF
            UQFGENC(IFL,IRG) = 0.0
            UTSO2C(IFL,IRG) = 0.0
            UTNOXC(IFL,IRG) = 0.0
            UTCO2C(IFL,IRG) = 0.0
            UTCARC(IFL,IRG) = 0.0
            UTCO1C(IFL,IRG) = 0.0
            UTVOCC(IFL,IRG) = 0.0
            UTHGC(IFL,IRG) = 0.0
         END DO
      END DO
!
      DO IRG = 1 , NNGEM
         TQDFRLG(IRG) = 0.0
         TQDFRHG(IRG) = 0.0
         TSGCMIN(IRG) = 0.0
         TRGCMIN(IRG) = 0.0
         TSGCMAX(IRG) = 0.0
         TRGCMAX(IRG) = 0.0
         TSGCPAR(IRG) = 0.0
         TRGCPAR(IRG) = 0.0
      END DO
!
      DO IFL = 1 , EFD_D_CAP
         DO IRG = 1 , MNUMNR
            DO IOWN = 1, USW_OWN
               UQPGENN(IFL,IRG,IOWN) = 0.0
            END DO
               UGNRPSN(IFL,IRG) = 0.0
!              end-use CHP
               IF (IFL .LE. 4)UGNCHPN(IFL,IRG) = 0.0
               UGNPTCN(IFL,IRG) = 0.0
               UPYPTCN(IFL,IRG) = 0.0
         END DO
         DO IRG = 1 , MNUMCR
            UQPGENC(IFL,IRG) = 0.0
         END DO
      END DO
!
!     Initialize regional mapping variables for table 62
!
      DO IRG = 1 , MNUMNR
         UGNINR(IRG,IYR) = 0.0
         UGNOTR(IRG,IYR) = 0.0
         UGNSRV(IRG,IYR) = 0.0
         UFLINR(IRG,IYR) = 0.0
         UFLOTR(IRG,IYR) = 0.0
         UFLSRV(IRG,IYR) = 0.0
         USO2INR(IRG,IYR) = 0.0
         USO2OTR(IRG,IYR) = 0.0
         UCO2INR(IRG,IYR) = 0.0
         UCO2OTR(IRG,IYR) = 0.0
         UCARINR(IRG,IYR) = 0.0
         UCAROTR(IRG,IYR) = 0.0
         UNOXINR(IRG,IYR) = 0.0
         UNOXOTR(IRG,IYR) = 0.0
         UHGINR(IRG,IYR) = 0.0
         UHGOTR(IRG,IYR) = 0.0
      END DO
      RPSGOAL(IYR) = 0.0
      RPSACHV(IYR) = 0.0
!
!     INITIALIZE TRADE
!
      DO IRG=1,MNUMNR
         ETDMDERG(IRG) = 0.0
         ETDMMERG(IRG) = 0.0
         ETDIMERG(IRG) = 0.0
         UTDMMF(IRG,CURIYR) = 0.0
         UTEXMF(IRG,CURIYR) = 0.0
         UTDMME(IRG,CURIYR) = 0.0
         UTEXME(IRG,CURIYR) = 0.0
         UTDMDF(IRG,CURIYR) = 0.0
         UTEXDF(IRG,CURIYR) = 0.0
         UTDMDE(IRG,CURIYR) = 0.0
         UTEXDE(IRG,CURIYR) = 0.0
         UTIMPE(IRG,CURIYR) = 0.0
         UTIMPF(IRG,CURIYR) = 0.0
         UTEXPF(IRG,CURIYR) = 0.0
      END DO
      UTEXPE(MNUMNR,CURIYR) = 0.0   ! don't zero regions because we just filled it in in ELDISP
!
      CALL GETIN(1,1)

!     WRITE(6,3979) CURIRUN, CURIYR+1989, CURITR, 1, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3979 FORMAT(1X,"UEFD_00293_EEITAJ_GET",4(":",I4),3(":",F12.3))

!
      DO IRG=1,MNUMNR+EFD_D_PROV
         DO JRG=1,MNUMNR+EFD_D_PROV
            CHKGEN(IRG,JRG) = 0.0
            CHK$(IRG,JRG)=0.0
            UTECON(IRG,JRG) = 0.0
            UTSALES(IRG,JRG) = 0.0
             DO IS = 1, EENSP
              DO ISL = 1 , ELNVCT(IS)
                UTECONSL(IS,ISL,IRG,JRG) = 0.0
                UTCNSTSL(IS,ISL,IRG,JRG) = 0.0
              ENDDO
             ENDDO
         END DO
      END DO
!
      DO IRGIM = 1,MNUMNR + EFD_D_PROV
         DO IRGEX = 1,MNUMNR + EFD_D_PROV
            DO YR = 1,MNUMYR
               PTHRESH1(YR,IRGEX,IRGIM) = 0.0
               PTHRESH2(YR,IRGEX,IRGIM) = 0.0
            END DO
         END DO
      END DO
!
!     Calculate Adjusted Load and Export Market for All Regions, Seasons, and Load Slices
!
!     CALCULATE TOTAL HOURS IN THE YEAR
!
      TOTHRS = 0.0
      DO ISP = 1 , EENSP
         TOTHRS = TOTHRS + EETIME(ISP)
      END DO
!
      DO IRG = 1 , UNRGNS
         CALL GETIN(1,IRG)

!       INITIALIZE SPINNING RESERVE COST ARRAYS
        SRPOOL(CURIYR,IRG) = 0.0
        DO IS = 1, EENSP
           DO VLS = 1 , ELNVCT(IS)
              GRP = ELGRP(VLS,IS)
              SEG = ELSEG(VLS,IS)
              SR_INT_REQ(GRP,SEG,IRG) = 0.0
              SP_RES_REQ(GRP,SEG,IRG) = 0.0
              SP_RES_DUAL(GRP,SEG,IRG) = 0.0
              SP_RES_ACH(GRP,SEG,IRG) = 0.0
              DO IECP = 1, ECP_D_CAP
                SP_ACHBYECP(GRP,SEG,IRG,IECP) = 0.0
              ENDDO
           END DO
        END DO

!        WRITE(6,3977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3977    FORMAT(1X,"UEFD_00325_EEITAJ_GET",4(":",I4),3(":",F12.3))

         GEN_REQ_ANNUAL = 0.0
         EEITAJ_ANNUAL = 0.0
         INTRUP_ANNUAL = 0.0
         EOUNT_EOUIPP_ANNUAL = 0.0

         DO ISP = 1 , EENSP
            UTPEAK(ISP,IRG) = 0.0
!
!           ADJUST FOR INTERREGIONAL TRANSFERS (FIRM CAPACITY SALES)
!           FIRST CALCULATE SEASON'S CANADIAN INTERRUPTIBLE POWER IN GW
!
            IMPANN = ETIMPE/EETIME(ISP)
            EXPANN = ETEXPE/EETIME(ISP)
            INTRUP = (EXPANN * (EETIME(ISP)/TOTHRS)) - &
               (IMPANN * (EETIME(ISP)/TOTHRS))
!

            EEITAJ_ANNUAL = EEITAJ_ANNUAL + EEITAJ(ISP) * EETIME(ISP)
            INTRUP_ANNUAL =INTRUP_ANNUAL + INTRUP * EETIME(ISP)
            EOUNT_EOUIPP_ANNUAL = EOUNT_EOUIPP_ANNUAL + (EOUNT + EOUIPP) * EETIME(ISP)

            ADJUST = (EEITAJ(ISP) + INTRUP + EOUIPP + EOUNT ) *  &
               EETIME(ISP)
!
            DO VLS = 1 , ELNVCT(ISP)
               GRP = ELGRP(VLS,ISP)
               SEG = ELSEG(VLS,ISP)
               ETHGHT(VLS,ISP,IRG) = ELHGHT(VLS,ISP) + EEITAJ(ISP) + &
                   INTRUP + EOUIPP + EOUNT
               ETWDTH(VLS,ISP,IRG) = ELWDTH(VLS,ISP)

               GEN_REQ_ANNUAL = GEN_REQ_ANNUAL + ELHGHT(VLS,ISP) * ELWDTH(VLS,ISP)

               UTHGHT(SEG,GRP,IRG) = UTHGHT(SEG,GRP,IRG) + EEITAJ(ISP) + &
                   INTRUP + EOUIPP + EOUNT

               WRITE(18,9331) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, ISP, VLS, GRP, SEG, EFD_Slice_ID(GRP,SEG), &
                  UTHGHT(SEG,GRP,IRG),  EEITAJ(ISP), INTRUP, EOUIPP, EOUNT, ELWDTH(VLS,ISP)

 9331          FORMAT(1X,"EFDLOAD",9(":",I4),6(":",F15.6))

               UTPEAK(ISP,IRG) = MAX(UTPEAK(ISP,IRG) ,  &
                  UTHGHT(SEG,GRP,IRG))
               UTXMKT(SEG,GRP,IRG) = UTXMKT(SEG,GRP,IRG) +  &
                  EEITAJ(ISP) + INTRUP + EOUIPP + EOUNT
            END DO
         END DO

         WRITE(18,9332) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, QELASN(IRG,CURIYR), NERCtdLoss(IRG), ULOSSADJ(CURIYR), &
            GEN_REQ_ANNUAL, EEITAJ_ANNUAL, INTRUP_ANNUAL, EOUNT_EOUIPP_ANNUAL

 9332    FORMAT(1X,"EFD_LOAD_ANNUAL",4(":",I4),7(":",F15.6))

      END DO
!
      DO IRG = 1 , UNRGNS
         DO MRG = 1 , UNRGNS
            IF (IRG .NE. MRG) THEN
               DO GRP = 1 , UTNGRP
                  ISP = UTSEAS(GRP)
                  IF (CNSTRNTS_EFD(ISP,CURIYR,MRG,IRG) .LT. UTPEAK(ISP,MRG)) THEN
                     FACTOR = CNSTRNTS_EFD(ISP,CURIYR,MRG,IRG) / UTPEAK(ISP,MRG)
                  ELSE
                     FACTOR = 1.0
                  END IF
                  DO SEG = 1 , UTNSEG
                     UTXMKT(SEG,GRP,IRG) = UTXMKT(SEG,GRP,IRG) + MIN(DBLE(CNSTRNTS_EFD(ISP,CURIYR,MRG,IRG)), UTHGHT(SEG,GRP,MRG))
                  END DO
               END DO
            END IF
         END DO
      END DO
!
!     COFIRING SHARES FROM ECP
!
      IF (USW_ECPCF .EQ. 1) THEN
         IF ((CURIYR + UHBSYR) .LT. (UYR_STEO + UPCFCLT)) THEN
            DO CRG = 1 , NDREG
               DO IECP = 1 , ECP_D_DSP
                  UPWDCFR(IECP,CRG) = COFGENPC(IECP,CRG)
                  IF (UPWDCFR(IECP,CRG) .GT. 0.10) THEN

!                    WRITE(6,2317) CURIRUN, CURIYR+1989, CURITR, IECP, CRG, UPWDCFR(IECP,CRG)
!2317                FORMAT(1X,"UPWDCFR_1",5(":",I4),25(":",F15.3))

                     UPWDCFR(IECP,CRG) = 0.1
                  END IF
               END DO
            END DO
            DO IRG = 1 , UNRGNS
               DO IECP = 1 , ECP_D_DSP
                  UPWDCFN(IECP,IRG) = COFGENPN(IECP,IRG)
               END DO
            END DO
         ELSE
            IF (CURIYR + UHBSYR .LT. UPSTYR) THEN
               DO CRG = 1 , NDREG
                  DO IECP = 1 , ECP_D_DSP
                     UPWDCFR(IECP,CRG) = 0.1
                  END DO
               END DO
!           ELSE
!              DO CRG = 1 , NDREG
!                 DO IECP = 1 , ECP_D_DSP
!                    IF (UPWDCFR(IECP,CRG) .GT. 0.0) WRITE(6,2318) CURIRUN, CURIYR+1989, CURITR, IECP, CRG, UPWDCFR(IECP,CRG)
 2318                FORMAT(1X,"UPWDCFR_2",5(":",I4),25(":",F15.3))
!                 END DO
!              END DO
            END IF
            DO IRG = 1 , UNRGNS
               DO IECP = 1 , ECP_D_DSP
                IF (UPCFGNN(IECP,IRG) .GT. 0.0 .AND. UDTLGNN(IECP,IRG) .GT. 0.0)THEN
                  UPWDCFN(IECP,IRG) = UPCFGNN(IECP,IRG) / UDTLGNN(IECP,IRG)
                  UPWDCFN(IECP,IRG) = MIN(UPWDCFN(IECP,IRG),0.15)
                ELSE
                  UPWDCFN(IECP,IRG) = 0.0
                END IF
               END DO
            END DO
         END IF
      END IF
!
      DO IRG = 1 , UNRGNS
         DO IECP = 1 , ECP_D_DSP
            UDTLGNN(IECP,IRG) = 0.0
         END DO
      END DO
!
!     LOOP ACROSS REGIONS
!
      DO 90 IRG = 1 , UNRGNS
!
!        INITIALIZE BALANCE ARRAYS
!
         DO IOWN = 1 , EFD_D_OWN
            BGENOWN(IRG,IOWN) = 0.0
            BTRDOWN(IRG,IOWN) = 0.0
         END DO
         BMEXICAN(IRG) = 0.0
         BNTCOWN(IRG) = 0.0
         BNUGOWN(IRG) = 0.0
         BGENREQ(IRG) = 0.0
!
!        READ INPUT DATA
!
         CALL GETIN(1,IRG)

!        WRITE(6,3975) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3975    FORMAT(1X,"UEFD_00452_EEITAJ_GET",4(":",I4),3(":",F12.3))

         CALL GETBLD(1,IRG)
!
!        READ OUTPUT DATA
!
         CALL GETOUT(IYR,IRG)
!
!        CALL DISPATCH MODEL
!
         CALL ELDISP(IYR,IRG,ISOL)
!
!        STORE DATA IN THE DIRECT ACCESS FILE
!
         CALL STRIN(1,IRG)   ! needed  for planned maintenance

!        WRITE(6,2977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!2977    FORMAT(1X,"UEFD_00469_EEITAJ_STR",4(":",I4),3(":",F12.3))

         CALL STROUT(IYR,IRG)
         CALL STRBLD(1,IRG)
!
   90 CONTINUE

!     calculate avg max oil/gas shares by plt type and fuel region

      DO IC = 1, 2
      DO IP = 1, ECP_D_CAP
       DO TFR = 1, UNFRGN
        IF (FSCAPWT(IC,IP,TFR) .GT. 0.0) THEN
          EDMXGS(IC,IP,TFR) = EDMXGS(IC,IP,TFR) / FSCAPWT(IC,IP,TFR)
          EDMXRS(IC,IP,TFR) = EDMXRS(IC,IP,TFR) / FSCAPWT(IC,IP,TFR)
          EDMXDS(IC,IP,TFR) = EDMXDS(IC,IP,TFR) / FSCAPWT(IC,IP,TFR)
          EDMXCL(IC,IP,TFR) = EDMXCL(IC,IP,TFR) / FSCAPWT(IC,IP,TFR)  !<------  need to enable to test CTN feature
        ENDIF
       ENDDO
      ENDDO
      ENDDO
      if (FCRL .eq. 1) THEN
         DO IP = 1, ECP_D_CAP
            DO TFR = 1, MAXNFR
               IF ( EDMXGS(1,IP,TFR) + EDMXRS(1,IP,TFR) + EDMXDS(1,IP,TFR) + FSCAPWT(1,IP,TFR) + &
                    EDMXGS(2,IP,TFR) + EDMXRS(2,IP,TFR) + EDMXDS(2,IP,TFR) + FSCAPWT(2,IP,TFR) .GT. 0.0) &
                  write(18,92) CURIYR+UHBSYR,CURITR,IP,TFR,(IC,EDMXGS(IC,IP,TFR),EDMXRS(IC,IP,TFR),EDMXDS(IC,IP,TFR),FSCAPWT(IC,IP,TFR),IC=1,2)
  92           FORMAT(1x,'EDMX_FL_SH',4(":",I4),2(":",I2,4(":",E13.6)))
            ENDDO
         ENDDO
      ENDIF
!
!     Calculate Max Oil, Gas and Coal Max Shares for the ECP
!
      DO IRG = 1 , UNFRGN
         DO IP = 1 , ECP_D_DSP
            IF (TPMXWGT(IP,IRG) .GT. 0.0) THEN
               UPMXGAS(IP,IRG,CURIYR) = TPMXGAS(IP,IRG) / TPMXWGT(IP,IRG)
               UPMXOIL(IP,IRG,CURIYR) = TPMXOIL(IP,IRG) / TPMXWGT(IP,IRG)
               UPMXCOL(IP,IRG,CURIYR) = TPMXCOL(IP,IRG) / TPMXWGT(IP,IRG)
            ELSE
               JEFD = UPEFDT(IP)
               DO IFP = 1 , UIFPLT
                  IFL = WFLTP(JEFD,IFP)
                  IF (IFL .GT. 0) THEN
                     IF (UIGAS(IFL) .GT. 0) UPMXGAS(IP,IRG,CURIYR) = 1.0
                     IF (UIRES(IFL) .GT. 0) UPMXOIL(IP,IRG,CURIYR) = 1.0
                     IF (UIDIS(IFL) .GT. 0) UPMXOIL(IP,IRG,CURIYR) = 1.0
                     IF (UICOL(IFL) .GT. 0) UPMXCOL(IP,IRG,CURIYR) = 1.0
                  END IF
               END DO
            END IF
            IF (TPMXWGT_MR(IP,IRG) .GT. 0.0) THEN
               UPMXGAS_MR(IP,IRG,CURIYR) = TPMXGAS_MR(IP,IRG) / TPMXWGT_MR(IP,IRG)
               UPMXOIL_MR(IP,IRG,CURIYR) = TPMXOIL_MR(IP,IRG) / TPMXWGT_MR(IP,IRG)
               UPMXCOL_MR(IP,IRG,CURIYR) = TPMXCOL_MR(IP,IRG) / TPMXWGT_MR(IP,IRG)
            ELSE
               JEFD = UPEFDT(IP)
               DO IFP = 1 , UIFPLT
                  IFL = WFLTP(JEFD,IFP)
                  IF (IFL .GT. 0) THEN
                     IF (UIGAS(IFL) .GT. 0) UPMXGAS_MR(IP,IRG,CURIYR) = 1.0
                     IF (UIRES(IFL) .GT. 0) UPMXOIL_MR(IP,IRG,CURIYR) = 1.0
                     IF (UIDIS(IFL) .GT. 0) UPMXOIL_MR(IP,IRG,CURIYR) = 1.0
                     IF (UICOL(IFL) .GT. 0) UPMXCOL_MR(IP,IRG,CURIYR) = 1.0
                  END IF
               END DO
            END IF
            IF (FCRL .EQ. 1) THEN
               WRITE(18,1193) CURIYR+UHBSYR,CURITR,IRG,IP,TPMXWGT(IP,IRG),TPMXGAS(IP,IRG),TPMXOIL(IP,IRG),TPMXCOL(IP,IRG), &
                  UPMXGAS(IP,IRG,CURIYR),UPMXOIL(IP,IRG,CURIYR),UPMXCOL(IP,IRG,CURIYR)
 1193          FORMAT(1X,"UPMX_FUEL   ",4(":",I4),7(":",E13.6))
               WRITE(18,1194) CURIYR+UHBSYR,CURITR,IRG,IP,TPMXWGT_MR(IP,IRG),TPMXGAS_MR(IP,IRG),TPMXOIL_MR(IP,IRG),TPMXCOL_MR(IP,IRG), &
                  UPMXGAS_MR(IP,IRG,CURIYR),UPMXOIL_MR(IP,IRG,CURIYR),UPMXCOL_MR(IP,IRG,CURIYR)
 1194          FORMAT(1X,"UPMX_FUEL_MR",4(":",I4),7(":",E13.6))
            END IF
         END DO
      END DO
!
!     DO SHORT TERM DISPATCH AS NATIONAL NETWORK
!
!     t0 = mclock()
!
      EFD_MIN = 0.001  ! moved from REVEFD
      ! following ED$GRP call was moved from REVEFD
       if (CURITR .EQ. 1) THEN
         CALL ED$GRP
       end if
      CALL EFD_LP   !try to solve EFD LP using C-whiz if AIMEFD not selected
!
!     t = mclock() - t0
!     write(*,5555)t/100.,curiyr,curitr
!5555 format(1x,'subroutine edn took ',f10.4,' seconds',
!    +   ' curiyr,curitr ',2i4)

      ! move calculation of AB32 EE calculation here, so we can drop the ED$CPP2 routine later (this isn't needed for LP, just accounting)
      CALL CALC_EEAB32        
      
! write trade summary report data
      if (fcrl .eq. 1) then
         one = 1
         zero = 0.0
         DO IRG=1,MNUMNR + EFD_D_PROV
            DO JRG=1, MNUMNR + EFD_D_PROV
               if (CHKGEN(IRG,JRG) .gt. 0.0) &
                  write(UF_ETT,2590) IYR,one,'7',JRG,IRG,zero, &
                  CHKGEN(IRG,JRG),zero,zero,CHK$(IRG,JRG), &
                  zero,zero,zero,zero,zero
            END DO
         END DO
      end if
2590  FORMAT(I4,1X,I4,1X,A5,2(I4,7X),F4.1,F10.3,2F4.1,F10.3,5F4.1)

      DO IRG=1,UNRGNS
         DO I=1,EENSP
            DO IE2 = 1,ETNVCT
               IF (UF_MC .GT. 0 .AND. FCRL.EQ.1) THEN
                  WRITE(UF_MC,2594)'AFT TRADE',IYR,IRG,I,IE2, &
                     NMARCST(IRG,I,IE2,IYR), &
                     NMARTYP(IRG,I,IE2,IYR), &
                     NMARREG(IRG,I,IE2,IYR), &
                     ULWDTH(IE2,I,IRG)
 2594             FORMAT(A10,1X,4I4,2(1X,F5.1,1X),I4,(1X,F5.1,1X))
               END IF

!              WRITE(6,3594) curirun, curiyr+1989, IYR+1989, curitr, irg, I, IE2, NMARREG(IRG,I,IE2,IYR), NMARCST(IRG,I,IE2,IYR), NMARTYP(IRG,I,IE2,IYR), ULWDTH(IE2,I,IRG)
!3594          FORMAT(1X,"NMARCST_UEFD_2",8(":",I4),3(":",F21.6))

            END DO
         END DO
      END DO

      DO IRG=1,UNRGNS
         DO I=1,EENSP
            DO IE2=1,ETNVCT
               IF (IYR.GT. 5 .AND. FCRL.EQ.1) THEN
                  IF (NMARCST(IRG,I,IE2,IYR) .EQ. 80.0) &
                     WRITE(6,*)'YR',IYR,'  RGN',IRG,'  SEASN',I, ' vert sl',IE2, '  DEMAND NOT MET, NMARCST STILL 80.0'
               END IF
            END DO
         END DO
      END DO
!
!     CALCULATE AVERAGE FUEL PRICES BY EMM REGION
!     report 0 if there is no demand -  3/2020
!
      DO IRG = 1 , UNRGNS
         IF (TPGASWGT(IRG) .GT. 0.0) THEN
            UPGASPRC(IRG,CURIYR) = TPGASPRC(IRG) / TPGASWGT(IRG)
         ELSE
            UPGASPRC(IRG,CURIYR) = 0.0
!           UPGASPRC(IRG,CURIYR) = 9.999
!           UPGASPRC(IRG,CURIYR) = OGWPRNG(MNUMOR,CURIYR) + (UPGASPRC(IRG,CURIYR - 1) - OGWPRNG(MNUMOR,CURIYR - 1))
         END IF
         IF (TPDISWGT(IRG) .GT. 0.0) THEN
            UPDISPRC(IRG,CURIYR) = TPDISPRC(IRG) / TPDISWGT(IRG)
         ELSE
            UPDISPRC(IRG,CURIYR) = 0.0
!           UPDISPRC(IRG,CURIYR) = 9.999
!           UPDISPRC(IRG,CURIYR) = IT_WOP(CURIYR,2) + (UPDISPRC(IRG,CURIYR - 1) - IT_WOP(CURIYR - 1,2))
         END IF
         IF (TPRESWGT(IRG) .GT. 0.0) THEN
            UPRESPRC(IRG,CURIYR) = TPRESPRC(IRG) / TPRESWGT(IRG)
         ELSE
            UPRESPRC(IRG,CURIYR) = 0.0
!           UPRESPRC(IRG,CURIYR) = 9.999
!           UPRESPRC(IRG,CURIYR) = IT_WOP(CURIYR,2) + (UPRESPRC(IRG,CURIYR - 1) - IT_WOP(CURIYR - 1,2))
         END IF
         IF (TPCOLWGT(IRG) .GT. 0.0) THEN
            UPCOLPRC(IRG,CURIYR) = TPCOLPRC(IRG) / TPCOLWGT(IRG)
            UPCOALAVG(IRG,CURIYR) = TPCOLPRA(IRG) / TPCOLWGT(IRG)
         ELSE
            UPCOLPRC(IRG,CURIYR) = 0.0
            UPCOALAVG(IRG,CURIYR) = 0.0
!           UPCOLPRC(IRG,CURIYR) = 9.999
!           UPCOALAVG(IRG,CURIYR) = 9.999
         END IF
         IF (TPNUCWGT(IRG) .GT. 0.0) THEN
            UPNUCPRC(IRG,CURIYR) = TPNUCPRC(IRG) / TPNUCWGT(IRG)
         ELSE
            UPNUCPRC(IRG,CURIYR) = 0.0
!           UPNUCPRC(IRG,CURIYR) = 9.999
         END IF
         IF (TPHYDWGT(IRG) .GT. 0.0) THEN
            UPHYDPRC(IRG,CURIYR) = TPHYDPRC(IRG) / TPHYDWGT(IRG)
         ELSE
            UPHYDPRC(IRG,CURIYR) = 0.0
!           UPHYDPRC(IRG,CURIYR) = 9.999
         END IF
      END DO
!
!     ACCUMULATE SO2 EMISSIONS AND ALLOWANCES BY COMPLIANCE GROUP
!
      DO IRG = 1 , UNRGNS
         CALL GETOUT(IYR,IRG)
         CALL GETIN(1,IRG)

!        WRITE(6,3973) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3973    FORMAT(1X,"UEFD_00641_EEITAJ_GET",4(":",I4),3(":",F12.3))

         CALL ELSO2N(ISOL,IRG)
      END DO
!
!  CHECK PENALTY COST(S) FOR CONVERGENCE
!
      DO ISO2 = 1 , NUM_SO2_GRP
         IF (CURITR .GT. 1) THEN
            FLAGSO2 = 1
            IF (EMELPSO2(IYR,ISO2) .GT. COSTSO2 * 1.02 .OR. EMELPSO2(IYR,ISO2) .LT. COSTSO2 * 0.98) FLAGSO2 = 0
            WRITE(UF_DBG,8313) CURIYR,CURITR,ISO2,COSTSO2,EMELPSO2(IYR,ISO2),FLAGSO2
 8313       FORMAT(1X,"SO2_FLAG",3(":",I4),2(":",F12.3),":",I2)
         ELSE
            FLAGSO2 = 0
         END IF
         COSTSO2 = EMELPSO2(IYR,ISO2)
      END DO
!
!     WRITE INPUTS/OUTPUTS TO REPORT FILE
!
      IF (CURIYR .EQ. FIRSYR .AND. CURITR .EQ. 1) WRITE(22,3495)
 3495 FORMAT(1X,'BALANCE',' : YR ',' : IT ',' : IS ', ' : RG ', &
         ' :  GEN 1   ',' :   TRD 1  ',' :  GEN 2   ',' :   TRD 2  ', &
         ' :  GEN 3   ',' :   TRD 3  ',' :  GEN 4   ',' :   TRD 4  ', &
         ' :  GEN 5   ',' :   TRD 5  ',' :   FIRM   ',' :  TCOGEN  ', &
         ' :  MEXICAN ',' :  NTCOWN  ',' :  NUGOWN  ',' :  GENREQ  ', &
         ' :  SO2INR  ',' :   SO2OTR ',' :  NOXINR  ',' :  NOXOTR  ', &
         ' :  EC_IMP  ',' :  NW_IMP  ')
!
      USO2INR(MNUMNR,CURIYR) = 0.0
      USO2OTR(MNUMNR,CURIYR) = 0.0
      UNOXINR(MNUMNR,CURIYR) = 0.0
      UNOXOTR(MNUMNR,CURIYR) = 0.0
      DO IOWN = 1 ,EFD_D_OWN
         BGENOWN(MNUMNR,IOWN) = 0.0
         BTRDOWN(MNUMNR,IOWN) = 0.0
      END DO
      BFIRM(MNUMNR) = 0.0
      BTCOGEN(MNUMNR) = 0.0
      BMEXICAN(MNUMNR) = 0.0
      BNTCOWN(MNUMNR) = 0.0
      BNUGOWN(MNUMNR) = 0.0
      BGENREQ(MNUMNR) = 0.0
      BEC_IMP(MNUMNR) = 0.0
      BNW_IMP(MNUMNR) = 0.0
!
      DO IRG = 1 , MNUMNR
         IF (IRG .LT. MNUMNR) THEN
            USO2INR(MNUMNR,CURIYR) = USO2INR(MNUMNR,CURIYR) +  &
               USO2INR(IRG,CURIYR)
            USO2OTR(MNUMNR,CURIYR) = USO2OTR(MNUMNR,CURIYR) +  &
               USO2OTR(IRG,CURIYR)
            UNOXINR(MNUMNR,CURIYR) = UNOXINR(MNUMNR,CURIYR) +  &
               UNOXINR(IRG,CURIYR)
            UNOXOTR(MNUMNR,CURIYR) = UNOXOTR(MNUMNR,CURIYR) +  &
               UNOXOTR(IRG,CURIYR)
            DO IOWN = 1 ,EFD_D_OWN
               BGENOWN(MNUMNR,IOWN) = BGENOWN(MNUMNR,IOWN) +  &
                  BGENOWN(IRG,IOWN)
               BTRDOWN(MNUMNR,IOWN) = BTRDOWN(MNUMNR,IOWN) +  &
                  BTRDOWN(IRG,IOWN)
            END DO
            BFIRM(MNUMNR) = BFIRM(MNUMNR) + BFIRM(IRG)
            BTCOGEN(MNUMNR) = BTCOGEN(MNUMNR) + BTCOGEN(IRG)
            BMEXICAN(MNUMNR) = BMEXICAN(MNUMNR) + BMEXICAN(IRG)
            BNTCOWN(MNUMNR) = BNTCOWN(MNUMNR) + BNTCOWN(IRG)
            BNUGOWN(MNUMNR) = BNUGOWN(MNUMNR) + BNUGOWN(IRG)
            BGENREQ(MNUMNR) = BGENREQ(MNUMNR) + BGENREQ(IRG)
            BEC_IMP(MNUMNR) = BEC_IMP(MNUMNR) + BEC_IMP(IRG)
            BNW_IMP(MNUMNR) = BNW_IMP(MNUMNR) + BNW_IMP(IRG)
         END IF
         WRITE(22,3493) CURIYR,CURITR,ISOL,IRG,(BGENOWN(IRG,IOWN), &
            BTRDOWN(IRG,IOWN),IOWN = 1 ,EFD_D_OWN),BFIRM(IRG), &
            BTCOGEN(IRG),BMEXICAN(IRG),BNTCOWN(IRG),BNUGOWN(IRG), &
            BGENREQ(IRG),USO2INR(IRG,CURIYR),USO2OTR(IRG,CURIYR), &
            UNOXINR(IRG,CURIYR),UNOXOTR(IRG,CURIYR),BEC_IMP(IRG), &
            BNW_IMP(IRG)
 3493    FORMAT(1X,'BALANCE',4(' : ',I3),22(' : ',F9.3))
!
!        WRITE ENERGY BALANCE TABLES TO EMMDBASE
!
         DUMMY = 9999.99
         IF ((FCRL .EQ. 1) .AND. &
             ((USW_DBS .GT. 0) .OR. (ORCLEFD .EQ. 1))) THEN
!
!           REGION AND OWNER TABLE
!
            DO IOWN = 1 , EFD_D_OWN
               IF (USW_DBS .GT. 0) THEN
                  WRITE(UF_DBS,3500) COL,CURIYR,COL,IRG,COL,CURITR,COL, &
                     ISOL,COL,IOWN,COL,BGENOWN(IRG,IOWN),COL, &
                     BTRDOWN(IRG,IOWN),COL,TRIM(SCEN_DATE)
               END IF
 3500          FORMAT(1X,'DBALRO',5(A2,I2),A2,2(F9.3,A2),A)
!
!              --- efd_DBALRO ---
!
!              TNUM = 1
!              IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!                IF (LOOPING(TNUM) .EQ. 0) THEN
!                  NUMCOLS(TNUM) = 7
!                  DYNSTM(TNUM) = 'INSERT INTO EFD_DBALRO VALUES(?,?,?,?,?,?,?,?)'
!                ENDIF
!                LOOPING(TNUM) = LOOPING(TNUM) + 1
!                COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
!                COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                COLV(TNUM,4,LOOPING(TNUM)) = ISOL
!                COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                COLV(TNUM,6,LOOPING(TNUM)) = BGENOWN(IRG,IOWN)
!                COLV(TNUM,7,LOOPING(TNUM)) = BTRDOWN(IRG,IOWN)
!                IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                 COLVALS(:,:) = COLV(TNUM,:,:)
!                 CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                 LOOPING(TNUM) = 0
!                ENDIF
!              ENDIF
!
!              ---END efd_DBALRO ---
!
            END DO
!
!           REGION TABLE
!
            IF (USW_DBS .GT. 0) THEN
               WRITE(UF_DBS,3510) COL,CURIYR,COL,IRG,COL,CURITR,COL, &
               ISOL,COL,BFIRM(IRG),COL,BTCOGEN(IRG),COL,BMEXICAN(IRG), &
               COL,BNTCOWN(IRG),COL,BNUGOWN(IRG),COL,BGENREQ(IRG),COL, &
               USO2INR(IRG,CURIYR),COL,USO2OTR(IRG,CURIYR),COL, &
               UNOXINR(IRG,CURIYR),COL,UNOXOTR(IRG,CURIYR),COL, &
               UCARINR(IRG,CURIYR),COL,UCAROTR(IRG,CURIYR),COL, &
               UCO2INR(IRG,CURIYR),COL,UCO2OTR(IRG,CURIYR),COL, &
               BEC_IMP(IRG),COL,BNW_IMP(IRG),COL,TRIM(SCEN_DATE)
            END IF
 3510       FORMAT(1X,'DBALR',4(A2,I3),A2,16(F9.3,A2),A)
!
!           --- efd_DBALR ---
!
!           TNUM = 2
!           IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!             IF (LOOPING(TNUM) .EQ. 0) THEN
!               NUMCOLS(TNUM) = 20
!               DYNSTM(TNUM) = 'INSERT INTO EFD_DBALR VALUES(?,?,?,?,?,?,?,?,?,?,?,'  &
!                            //'?,?,?,?,?,?,?,?,?,?)'
!             ENDIF
!             LOOPING(TNUM) = LOOPING(TNUM) + 1
!             COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
!             COLV(TNUM,2,LOOPING(TNUM)) = IRG
!             COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!             COLV(TNUM,4,LOOPING(TNUM)) = ISOL
!             COLV(TNUM,5,LOOPING(TNUM)) = BFIRM(IRG)
!             COLV(TNUM,6,LOOPING(TNUM)) = BTCOGEN(IRG)
!             COLV(TNUM,7,LOOPING(TNUM)) = BMEXICAN(IRG)
!             COLV(TNUM,8,LOOPING(TNUM)) = BNTCOWN(IRG)
!             COLV(TNUM,9,LOOPING(TNUM)) = BNUGOWN(IRG)
!             COLV(TNUM,10,LOOPING(TNUM)) = BGENREQ(IRG)
!             COLV(TNUM,11,LOOPING(TNUM)) = USO2INR(IRG,CURIYR)
!             COLV(TNUM,12,LOOPING(TNUM)) = USO2OTR(IRG,CURIYR)
!             COLV(TNUM,13,LOOPING(TNUM)) = UNOXINR(IRG,CURIYR)
!             COLV(TNUM,14,LOOPING(TNUM)) = UNOXOTR(IRG,CURIYR)
!             COLV(TNUM,15,LOOPING(TNUM)) = UCARINR(IRG,CURIYR)
!             COLV(TNUM,16,LOOPING(TNUM)) = UCAROTR(IRG,CURIYR)
!             COLV(TNUM,17,LOOPING(TNUM)) = UCO2INR(IRG,CURIYR)
!             COLV(TNUM,18,LOOPING(TNUM)) = UCO2OTR(IRG,CURIYR)
!             COLV(TNUM,19,LOOPING(TNUM)) = BEC_IMP(IRG)
!             COLV(TNUM,20,LOOPING(TNUM)) = BNW_IMP(IRG)
!             IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!               COLVALS(:,:) = COLV(TNUM,:,:)
!               CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!               LOOPING(TNUM) = 0
!             ENDIF
!
!           --- END efd_DBALR ---
!
!           ENDIF
         END IF
!
!        END EMMDBASE WRITES
!
      END DO      ! IRG
!
!     CALL SUBROUTINES TO DETERMINE COSTS OF RENEWABLE CREDITS IF
!     RENEWABLE PORTFOLIO CONSTRAINT (NATIONAL AND REGIONAL)
!
      CALL ELRPSCR
      CALL ELRPSCRR
!
!     CALL SUBROUTINE TO DETERMINE GPS SUBSIDIES BY REGION (FOR REGULATED PRICING)
!
      CALL GPSPRC

      IF (PCAP_CAR .EQ. 3 .OR. PCAP_CAR .EQ. 4) THEN  !McCain-Lieberman sensitivity - adj regul prices for carbon allocation
       CALL MLCAR
      ENDIF

      IF (CO2_STDSW .GT. 0) THEN    ! call routine to calculate impact of allowance allocation in quantity case
        CALL ELCPPALL
      ENDIF

      IF (CO2_STDSW .GT. 0 .AND. (CURIYR + UHBSYR) .GE. CO2_STDY1 .AND. CO2_ERCSW .GT. 0) THEN      ! CPP trading case - calculate regional adjustments for inter-regional trading
        write(6,*) 'calling elcpperc'
        CALL ELCPPERC
      ENDIF

!     WRITE OUT SUMMARY DATA FOR EFD TO DEBUG REPORT AND DATABASE
      IF (FCRL .EQ. 1 .AND. UF_RPT .GT. 0) THEN
!
         DO IRG = 1 , UNRGNS
!
!           READ INPUT DATA
!
            CALL GETIN(1,IRG)

!           WRITE(6,3971) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3971       FORMAT(1X,"UEFD_00847_EEITAJ_GET",4(":",I4),3(":",F12.3))

!
!           READ OUTPUT DATA
!
            CALL GETOUT(IYR,IRG)
!
            IF (UF_RPT .GT. 0) THEN
               CALL DEBUG(IYR,IRG)
            END IF
!
         END DO
      END IF
!
!     --- ENSURE ALL RECORDS WRITTEN TO NEMS DB ---
!
!     DO TNUM = 1 , NUMTABS
!       IF (LOOPING(TNUM) .NE. 0) THEN
!         COLVALS(:,:) = COLV(TNUM,:,:)
!         CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!         CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!         LOOPING(TNUM) = 0
!       ENDIF
!     ENDDO
!
!     --- END ENSURE ALL RECORDS WRITTEN TO NEMS DB ---
!
!     CALL EFDBASE ROUTINE TO WRITE OUT DISPOUT AND DISPIN VARIABLES TO EMM DATABASE
!
      IF ((FCRL .EQ. 1) .AND. ( (USW_DBS .GT. 0) .OR.       &
          ((ORCLEFD.EQ.1) .AND. (FNRUN.EQ.1)) )) CALL EFDBASE(IYR)
!
      RETURN
      END
!
!     ED$CPP
!
      SUBROUTINE ED$CPP
!
      use efd_row_col
      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'eusprc'
      include'e111d'
      include'dsmdimen'
      include'dsmsectr'
      include'edbdef'
      include'uefdout'
      include'emission'
      include'cdsparms'
      include'csapr'
      include'emmemis'

      INTEGER IRET,IRG,KRG,YEAR,GRP,REG
      REAL*4 CO2_QTY(EFD_D_MFRG),QTY
      REAL*4 OLDTGT,NEWTGT,RTECHG,ACHRTL,OLDSUM,NEWSUM,NEWAVG,NEWANN
      REAL*8 VALUE
      CHARACTER*16 ROW,COLUMN

      efdsub='ED$CPP'

!     STATE/FUEL REGION CARBON ROWS

      DO IRG = 1 , UNSTAS
!        ROW = 'CARCL' // USTNME(IRG); call makmsk(ROW_mask,':CARCL:' , USTNME(IRG))
!        CALL DROWTYPE(ROW,'N       ',ROW_mask)
!        ROW = 'CAROG' // USTNME(IRG); call makmsk(ROW_mask,':CAROG:' , USTNME(IRG))
!        CALL DROWTYPE(ROW,'N       ',ROW_mask)
!        ROW = 'CONSCL' // USTNME(IRG)
!        CALL DROWTYPE(ROW,'N       ',ROW_mask)
!        ROW = 'CONSOG' // USTNME(IRG)
!        CALL DROWTYPE(ROW,'N       ',ROW_mask)
      END DO

!     FUEL REGION INTENSITY STANDARDS BASED ON BASE YEAR RESULTS

      IF ((CURIYR + UHBSYR) .EQ. UESTYR .AND. CURITR .EQ. 1)THEN
!     INITIALIZE
         ECO2NRRT = 0.0
         ECO2NRQY = 0.0
         CO2_STDRF = 0.0
         CO2_STDRN = 0.0
         CO2_STDRE = 0.0
         CO2_STDQF = 0.0
         CO2_STDQN = 0.0
         CO2_STDQE = 0.0
!     STORE 111D ALLOWANCE PRICES FROM PREVIOUS CYCLE
         IF (CO2_STDSW .GT. 0)THEN
            DO IRG = 1 , UNRGNS
               DO YEAR = 1 , MNUMYR
                  EFDPRCNL(IRG,YEAR) = ECO2NRPR(IRG,YEAR)
                  ECPPRCNL(IRG,YEAR) = ECO2NRPP(IRG,YEAR) * 0.001
               END DO
!        write(6,3232) irg,  &
!                 ECPPRCNL(IRG,31),EFDPRCNL(IRG,31),MAX(0.0,ECPPRCNL(IRG,31)-EFDPRCNL(IRG,31)),  &
!                 ECPPRCNL(IRG,41),EFDPRCNL(IRG,41),MAX(0.0,ECPPRCNL(IRG,41)-EFDPRCNL(IRG,41)),  &
!                 ECPPRCNL(IRG,51),EFDPRCNL(IRG,51),MAX(0.0,ECPPRCNL(IRG,41)-EFDPRCNL(IRG,41))
!3232 format(1h ,'!111dif',i4,10f10.3)
            END DO
         END IF
      END IF
!     print *,'std0',co2_stdby+1,curitr,co2_stdsw
      IF ((CURIYR + UHBSYR) .EQ. (CO2_STDBY + 1) .AND. CURITR .EQ. 1)THEN
!     AGGREGATE STATE STANDARDS TO FUEL REGION LEVEL, IF USING THAT AGGREGATION
         IF (CO2_STDSW .GT. 0)THEN
!      print *,'stdnr',curiyr+1989,curitr
            DO YEAR = 1 , MNUMYR + ECP_D_XPH
!              RATE-BASED
               IF (CO2_STDRS(1,YEAR) .GT. 0.0)THEN
                  DO IRG = 1 , UNRGNS
                     CO2_STDRN(IRG,YEAR) = 0.0
                     CO2_QTY(IRG) = 0.0
                     DO KRG = 1 , EMM_D_ST
                        QTY = 0.0
                        IF (CO2_STDWT .EQ. 1)THEN
                           QTY = CO2_GENSN(KRG,IRG) / 1000.0
                        ELSE IF (CO2_STDWT .EQ. 2)THEN
                           QTY = CO2_GENSN(KRG,IRG) / 1000.0
                        ELSE IF (CO2_STDWT .EQ. 3)THEN
                           QTY = EGEN_NRST(ECP_D_CAP + 2,IRG,KRG)
                        ELSE IF (CO2_STDWT .EQ. 4)THEN
                           QTY = EGEN_NRST(ECP_D_CAP + 1,IRG,KRG)
                        END IF
                        IF (QTY .GT. 0.0)THEN
                           CO2_STDRN(IRG,YEAR) = CO2_STDRN(IRG,YEAR) + CO2_STDRS(KRG,YEAR) * QTY
                           CO2_QTY(IRG) = CO2_QTY(IRG) + QTY
                        END IF
!                          if (curitr .eq. 1. .and. year .eq. 31)write(6,6666) curiyr+1989,year+1989,irg,krg,ustnme(krg),co2_stdrs(krg,year),co2_gensn(krg,irg)/1000.0,  &
!                          egen_nrst(ECP_D_CAP+2,irg,krg),co2_stdrn(irg,year),co2_qty(irg),  &
!                          co2_stdrn(irg,year)/co2_qty(irg)
!6666 format(1h ,'!stdemm',i4,i5,i3,i3,a3,8f10.1)
                     END DO
                     CO2_STDRN(IRG,YEAR) = CO2_STDRN(IRG,YEAR) / CO2_QTY(IRG)
                  END DO
               END IF
!              MASS-BASED
               IF (CO2_STDQS(1,YEAR) .GT. 0.0)THEN
!                 FOR EACH EMM REGION, SHARE OUT CO2 QTY USING PCT OF AFFECTED GEN 
                  DO IRG = 1 , UNRGNS
                     DO KRG = 1 , EMM_D_ST
                        QTY = EGEN_NRST(ECP_D_CAP + 2,IRG,KRG)
                        IF (QTY .GT. 0.0)THEN
                           CO2_STDQN(IRG,YEAR) = CO2_STDQN(IRG,YEAR) + CO2_STDQS(KRG,YEAR) * QTY / EGEN_NRST(ECP_D_CAP + 2,MNUMNR,KRG)
!                 if (curitr .eq. 1. .and. year .eq. 31 .and. irg .eq. 5) write(6,6666) curiyr+1989,year+1989,irg,krg,ustnme(krg),co2_stdqs(krg,year),qty,  &
!                          egen_nrst(ECP_D_CAP+2,irg,krg),egen_nrst(ECP_D_CAP+2,mnumnr,krg),co2_stdqn(irg,year)
!6666 format(1h ,'!stdemm',i4,i5,i3,i3,a3,8f10.2)
                        END IF
                     END DO
                  END DO
               END IF
            END DO
            DO YEAR = 1 , UNYEAR
               DO IRG = 1 , UNRGNS
                  IF (CO2_STDSW .GT. 0)THEN
                     IF (CO2_STDTN(IRG) .LT. 2)THEN
                        ECO2NRRT(IRG,YEAR) = CO2_STDRN(IRG,YEAR)
                     ELSE IF (CO2_STDTN(IRG) .EQ. 2)THEN
                        ECO2NRQY(IRG,YEAR) = CO2_STDQN(IRG,YEAR)
                        ECO2NRQY(MNUMNR,YEAR) = ECO2NRQY(MNUMNR,YEAR) + CO2_STDQN(IRG,YEAR)
                     END IF
                  END IF
               END DO
            END DO
!           DO IRG = 1 , UNRGNS
!              write(6,5434) irg,urgnme(irg)(6:7),(eco2nrrt(irg,year),year=30,41)
!5434 format(1h ,'!stdnrr',i3,a4,12f10.2)
!              write(6,5435) irg,urgnme(irg)(6:7),(eco2nrqy(irg,year),year=30,41)
!5435 format(1h ,'!stdnrq',i3,a4,12f10.2)
!           END DO 
         END IF
!           do irg = 1 , unfrgn
!             write(6,3456) irg,flrgcode(irg),(co2_stdrf(irg,year),year = 2019 - uhbsyr, 2031 - uhbsyr)
!3456 format(1h ,'!e111din',i3,a3,13f8.1)
!           end do
!           do irg = 1 , unrgns
!             write(6,3457) irg,urgnme(irg)(6:7),(co2_stdrn(irg,year),year = 2019 - uhbsyr, 2031 - uhbsyr)
!3457 format(1h ,'!e111din',i3,a3,13f8.1)
!           end do
      END IF

!     EMM REGION INTENSITY STANDARD ROWS
      DO IRG = 1 , UNRGNS
!        if (curitr .eq. 1)write(6,3233) curiyr+1989,irg,CO2_STDSW,co2_stdtn(irg),CO2_STDRn(IRG,CURIYR),co2_stdqn(irg,curiyr)
!3233 format(1h ,'!stdqtyn',i4,i3,i3,i3,2f10.1)
         IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
            ROW = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CO2RNR:' , URGNME(IRG)(6:7))
            IF (CO2_STDTN(IRG) .LT. 2)THEN
!              RATE BASED
               CALL DROWTYPE(ROW,'L       ',ROW_mask)
            ELSE
!              RATE BASED
               CALL DROWTYPE(ROW,'N       ',ROW_mask)
            END IF
         END IF
         ROW = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENQNR:' , URGNME(IRG)(6:7))
         CALL DROWTYPE(ROW,'N       ',ROW_mask)
!     write(6,1234) curiyr+1989,irg,row,co2_stdrn(irg,curiyr)
!1234 format(1h ,'!co2efd',i4,i3,a10,f10.1)
         ROW = 'CO2QNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CO2QNR:' , URGNME(IRG)(6:7))
         IF (CO2_STDSW .GT. 0 .AND. CO2_STDQN(IRG,CURIYR) .GT. 0.0 .AND. CO2_STDTN(IRG) .EQ. 2)THEN
!           MASS BASED
            CALL DROWTYPE(ROW,'L       ',ROW_mask)
            CALL DRHS(EFDRHS,ROW,DBLE(CO2_STDQN(IRG,CURIYR) * 2.204),ROW_mask,'ED$CPP,2')
         ELSE
            CALL DROWTYPE(ROW,'N       ',ROW_mask)
         END IF
         ROW = 'CO2TNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CO2TNR:' , URGNME(IRG)(6:7))
         CALL DROWTYPE(ROW,'N       ',ROW_mask)
      END DO

      RETURN
      END
!
!     ED$CPP2
!
      SUBROUTINE ED$CPP2
!
      use efd_row_col
      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'eusprc'
      include'e111d'
      include'cogen'
      include'dsmdimen'
      include'dsmsectr'
      include'edbdef'
      include'uefdout'
      include'emission'
      include'cdsparms'
      include'csapr'
      include'emmemis'
 
!     COMMON/ERCOUT/ERCQTYDN(MNUMNR,MNUMNR,MNUMYR),ERCPRCDN(MNUMNR,MNUMYR),  &
!                   ERCQTYPN(MNUMNR,MNUMNR,MNUMYR),ERCPRCPN(MNUMNR,MNUMYR)
!     REAL*4 ERCQTYDN
!     REAL*4 ERCPRCDN
!     REAL*4 ERCQTYPN
!     REAL*4 ERCPRCPN

      INTEGER IRET,IRG,KRG,YEAR,GRP,REG,ISC
      INTEGER CA_CR,CA_ST
      REAL*4 QREE,QCEE,QIEE,CREE,CCEE,CIEE,QTYEE(3,MNUMCR),CSTEE(3,MNUMCR)
      REAL*8 VALUE,CA_NR(MNUMNR,3),CA_TOT(3)
      CHARACTER*16 ROW,ROW_ERC,COLUMN
      INTEGER AB32SW

      INTEGER CENSUS,ISTATE               !functions

      efdsub='ED$CPP2'
      
      AB32SW=RTOVALUE('AB32SW  ',1)

!     IDENTIFY CENSUS AND EMM REGIONS FOR CALIFORNIA FOR AB32 VS. CPP EE SAVINGS

      IF ((CURIYR + UHBSYR) .EQ. UESTYR .AND. CURITR .EQ. 1)THEN
         CA_NR = 0.0
         CA_TOT = 0.0
         CA_ST = ISTATE('CA')
         CA_CR = CENSUS(CA_ST)
!      write(6,*) 'CA_CR ',CA_CR,CA_ST
         DO REG = 1 , UNRGNS
            IF (URGNME(REG)(1:2) .EQ. 'ca') THEN 
               DO ISC = 1, 3
                 CA_NR(REG,ISC) = MappCtoN(REG,CA_CR,ISC)
                 CA_TOT(ISC) = CA_TOT(ISC) + MappCtoN(REG,CA_CR,ISC)
               ENDDO
             ENDIF
         END DO
         IF (CA_TOT(1) .EQ. 0 .AND. CA_TOT(2) .EQ. 0 .AND. CA_TOT(3) .EQ. 0) THEN
            WRITE(6,'(" Did not find California among URGNME region names")')
            STOP
         END IF
         DO REG = 1, UNRGNS
            DO ISC = 1,3
              IF (CA_TOT(ISC) .GT. 0.0) THEN
                CA_NR(REG,ISC) = CA_NR(REG,ISC) / CA_TOT(ISC)
!            write(6,*) 'CA_NR = ',REG,ISC,CA_NR(REG,ISC)
              ELSE
                CA_NR(REG,ISC) = 0.0
              ENDIF
            ENDDO
         ENDDO
!        DO IRG = 1 , MNUMCR - 1
!           IF (MAPPCTON(CA_NR,IRG,1) .GT. 0.0) THEN
!              CA_CR = IRG
!           END IF
!        END DO
!        IF (CA_CR .EQ. 0) THEN
!           WRITE(6,'(" Did not find California among MAPPCTON mappings; setting to 9")')
!           CA_CR = 9
!        END IF
      END IF

!     CREATE INDEX IF REGIONS HAVE ERC TRADING PARTNERS
 
      IF ((CURIYR + UHBSYR) .EQ. UESTYR .AND. CURITR .EQ. 1) THEN
         IF (CO2_STDSW .GT. 0) THEN
            CO2_ERCNR = 0
            DO IRG = 1 , UNRGNS
               DO REG = 1 , UNRGNS
                  CO2_ERCNR(IRG) = MAX(CO2_ERCNR(IRG),CO2_STDGN(IRG,REG))
               END DO
!     write(6,2222) curiyr+1989,irg,co2_ercnr(irg)
!2222 format(1h ,'!ercnr',i4,i4,i4)
            END DO
         END IF
      END IF

!     INITIALIZE ARRAY FOR AVAILABLE ERCs

      DO IRG = 1 , MNUMNR
        ERCQAVLN(IRG,CURIYR) = 0.0
      END DO

!     CONNECT REGIONS BY TRANSFERING CREDITS OR ERC TRADING, IF SPECIFIED 
!     EMM REGION
      IF (CO2_STDSW .GT. 0)THEN
         DO IRG = 1 , UNRGNS
            IF (CO2_ERCNR(IRG) .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
!     IN FINAL RULE, CREATE CONSTRAINT TO LIMIT CREDIT TRANSFERS BY QUALIFYING INCREMENTAL GENERATION
               IF (CO2_ERCSW .EQ. 1 .AND. CO2_STDTN(IRG) .LT. 2 .AND. CO2_ERCNR(IRG) .EQ. 2)THEN
                  ROW_ERC = 'ERCTNR' // URGNME(IRG)(6:7); call makmsk(ROW_ERC_mask,':ERCTNR:' , URGNME(IRG)(6:7))
                  CALL DROWTYPE(ROW_ERC,'L       ',ROW_ERC_mask)
                  COLUMN = 'IN' // UPRGCD(IRG) // 'XXNR' ; call makmsk(COLUMN_mask,':IN:' , UPRGCD(IRG) , ':XXNR:')
!                 PUT INCREMENTAL GENERATION COLUMN IN ERC SUPPLY CURVE ROW
                  CALL DVAL(COLUMN,ROW_ERC,DBLE(1.0),COLUMN_mask,ROW_ERC_mask,'ED$CPP2,8')
                  ROW = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                  VALUE = DBLE(0.0 - CO2_STDRN(IRG,CURIYR))
                  CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CPP2,9')
                  ROW = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENQNR:' , URGNME(IRG)(6:7))
                  CALL DVAL(COLUMN,ROW,DBLE(1.0),COLUMN_mask,ROW_mask,'ED$CPP2,10')
               END IF
               DO REG = 1 , UNRGNS
                  IF (IRG .NE. REG .AND. CO2_STDGN(IRG,REG) .GT. 0)THEN
                     IF (CO2_STDGN(IRG,REG) .EQ. 1)THEN
!                    COLUMN TO LINK EMM  REGIONS TO A SINGLE STATE (I.E., NY)
                        COLUMN = 'CM'   // URGNME(IRG)(6:7) // URGNME(REG)(6:7) // 'NR' ; call makmsk(COLUMN_mask,':CM:' , URGNME(IRG)(6:7) , URGNME(REG)(6:7), ':NR:')
                     ELSE
!                    COLUMN TO LINK EMM  REGIONS FOR TRADING ERCS
                        COLUMN = 'CE'   // URGNME(IRG)(6:7) // URGNME(REG)(6:7) // 'NR' ; call makmsk(COLUMN_mask,':CE:' , URGNME(IRG)(6:7) , URGNME(REG)(6:7), ':NR:')
                     END IF
                     CALL DVAL(COLUMN,EFDOBJ,0.0001D0,COLUMN_mask,EFDOBJ,'ED$CPP2,11')
                     IF (CO2_STDTN(IRG) .LT. 2)THEN
                        ROW = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                     ELSE
                        ROW = 'CO2QNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CO2QNR:' , URGNME(IRG)(6:7))
                     END IF
                     IF (CO2_ERCSW .LE. 0)THEN
                        VALUE = DBLE(1.0)
                     ELSE
                        IF (CO2_STDTN(IRG) .LT. 2 .AND. CO2_STDGN(IRG,REG) .EQ. 2)THEN
!                          PUT TRANSFER COLUMN IN SUPPLY CURVE ROW
                           CALL DVAL(COLUMN,ROW_ERC,DBLE(1.0),COLUMN_mask,ROW_ERC_mask,'ED$CPP2,12')
                           VALUE = DBLE((0.0 - CO2_STDRN(IRG,CURIYR)) * -1.0)
                        ELSE
                           VALUE = DBLE(1.0)
                        END IF
                     END IF   
                     CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CPP2,13')
                     IF (CO2_STDTN(IRG) .LT. 2)THEN
                        ROW = 'CO2RNR' // URGNME(REG)(6:7); call makmsk(ROW_mask,':CO2RNR:' , URGNME(REG)(6:7))
                     ELSE
                        ROW = 'CO2QNR' // URGNME(REG)(6:7); call makmsk(ROW_mask,':CO2QNR:' , URGNME(REG)(6:7))
                     END IF
                     IF (CO2_ERCSW .LE. 0)THEN
                        VALUE = DBLE(-1.0)
                     ELSE
                        IF (CO2_STDTN(IRG) .LT. 2 .AND. CO2_STDGN(IRG,REG) .EQ. 2)THEN
                           VALUE = DBLE((0.0 - CO2_STDRN(REG,CURIYR)) *  1.0)
                        ELSE
                           VALUE = DBLE(-1.0)
                        END IF
                     END IF   
                     CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CPP2,14')
                  END IF
               END DO
            END IF
         END DO
      END IF

!     ACCOUNT FOR INCREMENTAL GENERATION, IF APPROPRIATE
!     EMM  REGION
      IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(1,CURIYR) .GT. 0.0)THEN
         DO GRP = 1 , ECP_D_CAP
            IF (CO2_INCSW(GRP) .GT. 0.0)THEN
               DO REG = 1 , UNRGNS
                  VALUE = DBLE(0.0)
                  DO YEAR = 1 , CURIYR
                     VALUE = VALUE + DBLE(EPBLDGEN(GRP,REG,YEAR))
                  END DO
                  IF (VALUE .GT. DBLE(0.0)) THEN
                     COLUMN = 'I' // UPRGCD(REG) // UPLNTCD(GRP) // 'GEN'; call makmsk(COLUMN_mask,':I:' , UPRGCD(REG) , UPLNTCD(GRP), ':GEN:') 
                     CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CPP2,19')
!                    IF ERC TRADING, PUT INTO SUPPLY CURVE ROW
                     IF (CO2_ERCSW .EQ. 1 .AND. CO2_ERCNR(REG) .EQ. 2 .AND. CO2_STDTN(REG) .EQ. 1)THEN
                        ROW_ERC = 'ERCTNR' // URGNME(REG)(6:7); call makmsk(ROW_ERC_mask,':ERCTNR:' , URGNME(IRG)(6:7))
                        CALL DVAL(COLUMN,ROW_ERC,DBLE(-1.0),COLUMN_mask,ROW_ERC_mask,'ED$CPP2,20')
!                       ACCUMULATE AVAILABLE CREDITS
                        ERCQAVLN(REG,CURIYR) = ERCQAVLN(REG,CURIYR) + VALUE
                        ERCQAVLN(MNUMNR,CURIYR) = ERCQAVLN(MNUMNR,CURIYR) + VALUE
                     ELSE
                        ROW = 'GENQNR' // URGNME(REG)(6:7); call makmsk(ROW_mask,':GENQNR:' , URGNME(REG)(6:7))
                        CALL DVAL(COLUMN,ROW,DBLE(CO2_INCSW(GRP)),COLUMN_mask,ROW_mask,'ED$CPP2,21')
                        VALUE = DBLE(0.0 - CO2_STDRN(REG,CURIYR)) * DBLE(CO2_INCSW(GRP))
                        ROW = 'CO2RNR' // URGNME(REG)(6:7); call makmsk(ROW_mask,':CO2RNR:' , URGNME(REG)(6:7))
                        CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CPP2,22')
                     END IF
                  END IF
               END DO
            END IF
         END DO
      END IF

!     ACCOUNT FOR INCREMENTAL END-USE RENEWABLE GENERATION, IF APPROPRIATE
!     EMM  REGION
!     if (curitr .eq. 1)print *,'!endcg',curiyr+1989,co2_stdsw,co2_stdrn(1,CURIYR),Co2_endcg
      IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(1,CURIYR) .GT. 0.0 .AND. CO2_ENDCG .GT. 0)THEN
         DO REG = 1 , UNRGNS
            VALUE = DBLE(0.0)
!           HYDRO
            IF (CO2_INCSW(WIHY) .GT. 0.0)VALUE = VALUE + MAX(0.0,DBLE((CGTOTGENNR(REG,CURIYR,5,1) + CGTOTGENNR(REG,CURIYR,5,2)) -  &
                                         (CGTOTGENNR(REG,CO2_AFFYR - UHBSYR,5,1) + CGTOTGENNR(REG,CO2_AFFYR - UHBSYR,5,2))))
!           GEOTHERMAL
            IF (CO2_INCSW(WIGT) .GT. 0.0)VALUE = VALUE + MAX(0.0,DBLE((CGTOTGENNR(REG,CURIYR,6,1) + CGTOTGENNR(REG,CURIYR,6,2)) -  &
                                         (CGTOTGENNR(REG,CO2_AFFYR - UHBSYR,6,1) + CGTOTGENNR(REG,CO2_AFFYR - UHBSYR,6,2))))
!           SOLAR
            IF (CO2_INCSW(WIPV) .GT. 0.0)VALUE = VALUE + MAX(0.0,DBLE((CGTOTGENNR(REG,CURIYR,10,1) + CGTOTGENNR(REG,CURIYR,10,2)) -  &
                                         (CGTOTGENNR(REG,CO2_AFFYR - UHBSYR,10,1) + CGTOTGENNR(REG,CO2_AFFYR - UHBSYR,10,2))))
!           WIND
            IF (CO2_INCSW(WIWN) .GT. 0.0)VALUE = VALUE + MAX(0.0,DBLE((CGTOTGENNR(REG,CURIYR,12,1) + CGTOTGENNR(REG,CURIYR,12,2)) -  &
                                         (CGTOTGENNR(REG,CO2_AFFYR - UHBSYR,12,1) + CGTOTGENNR(REG,CO2_AFFYR - UHBSYR,12,2))))
            VALUE = VALUE * 0.001
!     if (curitr .gt. 1)write(6,3333) curiyr+1989,reg,value
!3333 format(1h ,'!eucog',i4,i3,f12.3)
            IF (VALUE .GT. DBLE(0.0)) THEN
               COLUMN = 'I' // UPRGCD(REG) // 'EU' // 'COG'; call makmsk(COLUMN_mask,':I:' , UPRGCD(REG) , 'EU', ':COG:') 
               CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CPP2,23')
!              IF ERC TRADING, PUT INTO SUPPLY CURVE ROW
!              IF (CO2_ERCSW .EQ. 1 .AND. CO2_ERCNR(REG) .EQ. 2 .AND. CO2_STDTN(REG) .EQ. 1)THEN
!                 ROW_ERC = 'ERCTNR' // URGNME(REG)(6:7); call makmsk(ROW_ERC_mask,':ERCTNR:' , URGNME(IRG)(6:7))
!                 CALL DVAL(COLUMN,ROW_ERC,DBLE(-1.0),COLUMN_mask,ROW_ERC_mask)
!                 ACCUMULATE AVAILABLE CREDITS
!                 ERCQAVLN(REG,CURIYR) = ERCQAVLN(REG,CURIYR) + VALUE
!                 ERCQAVLN(MNUMNR,CURIYR) = ERCQAVLN(MNUMNR,CURIYR) + VALUE
!              ELSE
                  ROW = 'GENQNR' // URGNME(REG)(6:7); call makmsk(ROW_mask,':GENQNR:' , URGNME(REG)(6:7))
                  CALL DVAL(COLUMN,ROW,DBLE(1.0),COLUMN_mask,ROW_mask,'ED$CPP2,24')
                  VALUE = DBLE(0.0 - CO2_STDRN(REG,CURIYR))
                  ROW = 'CO2RNR' // URGNME(REG)(6:7); call makmsk(ROW_mask,':CO2RNR:' , URGNME(REG)(6:7))
                  CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CPP2,25')
!              END IF
            END IF
         END DO
      END IF

!     INCLUDE PLANNED NUCLEAR GENERATION, IF SPECIFIED
!     EMM REGION
      IF (CO2_STDSW .GT. 0)THEN
         DO REG = 1 , UNRGNS
            IF (CO2_STDRN(REG,CURIYR) .GT. 0.0)THEN
               IF (NUCPLNN(REG,CURIYR) .GT. 0.0)THEN
                  COLUMN = 'G' // URGNME(REG)(6:7) // 'NUPNR'; call makmsk(COLUMN_mask,':G:' , URGNME(REG)(6:7) , ':NUPNR:')
                  VALUE = DBLE(NUCPLNN(REG,CURIYR))
                  CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CPP2,29')
                  ROW = 'GENQNR' // URGNME(REG)(6:7); call makmsk(ROW_mask,':GENQNR:' , URGNME(REG)(6:7))
                  CALL DVAL(COLUMN,ROW,1.0D0,COLUMN_mask,ROW_mask,'ED$CPP2,30')
                  ROW = 'CO2RNR' // URGNME(REG)(6:7); call makmsk(ROW_mask,':CO2RNR:' , URGNME(REG)(6:7))
                  VALUE = DBLE(0.0 - CO2_STDRN(REG,CURIYR))
                  CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CPP2,31')
               END IF
            END IF
         END DO
      END IF
!
!     DETERMINE EE CONTRIBUTION, IF ANY FOR CPP
!
      IF (CO2_STDSW .LE. 0 .OR. CO2_EFFSW .LE. 0)THEN
         IF ((CURIYR + UHBSYR) .EQ. (CO2_STDBY + 1) .AND. CURITR .EQ. 1)THEN
            EGENFREE = 0.0
            ECSTFREE = 0.0
            EGENNREE = 0.0
            ECSTNREE = 0.0
            EERF = 0.0
            EERN = 0.0
            EECF = 0.0
            EECN = 0.0
            EEIF = 0.0
            EEIN = 0.0
         END IF
      ELSE IF ((CURIYR + UHBSYR) .LT. CO2_STDY1)THEN
         DO IRG = 1 , EFD_D_MFRG + 1
            EGENFREE(IRG,CURIYR) = 0.0
            ECSTFREE(IRG,CURIYR) = 0.0
         END DO
         DO IRG = 1 , MNUMNR
            EGENNREE(IRG,CURIYR) = 0.0
            ECSTNREE(IRG,CURIYR) = 0.0
         END DO
      ELSE IF ((CURIYR + UHBSYR) .GE. CO2_STDY1)THEN
         EGENNREE(MNUMNR,CURIYR) = 0.0
         ECSTNREE(MNUMNR,CURIYR) = 0.0
         EERN(MNUMNR,CURIYR) = 0.0
         EECN(MNUMNR,CURIYR) = 0.0
         EEIN(MNUMNR,CURIYR) = 0.0
         DO IRG = 1 , EFD_D_MFRG + 1
            EGENFREE(IRG,CURIYR) = 0.0
            ECSTFREE(IRG,CURIYR) = 0.0
            EERF(IRG,CURIYR) = 0.0
            EECF(IRG,CURIYR) = 0.0
            EEIF(IRG,CURIYR) = 0.0
         END DO
!        INSURE THAT EE SAVINGS IS NONNEGATIVE
         DO IRG = 1 , MNUMCR - 1
!           IF (SAVE111RES(IRG,CURIYR) .LT. 0.0)THEN
!              QTYEE(1,IRG) = 0.0
!              CSTEE(1,IRG) = 0.0
!           ELSE
!              QTYEE(1,IRG) = SAVE111RES(IRG,CURIYR)
!              CSTEE(1,IRG) = COST111RES(IRG,CURIYR)
!           END IF
!           IF (SAVE111COM(IRG,CURIYR) .LT. 0.0)THEN
!              QTYEE(2,IRG) = 0.0
!              CSTEE(2,IRG) = 0.0
!           ELSE
!              QTYEE(2,IRG) = SAVE111COM(IRG,CURIYR)
!              CSTEE(2,IRG) = COST111COM(IRG,CURIYR)
!           END IF
!           IF (SAVE111IND(IRG,CURIYR) .LT. 0.0)THEN
!              QTYEE(3,IRG) = 0.0
!              CSTEE(3,IRG) = 0.0
!           ELSE
!              QTYEE(3,IRG) = SAVE111IND(IRG,CURIYR)
!              CSTEE(3,IRG) = COST111IND(IRG,CURIYR)
!           END IF
               QTYEE(1,IRG) = MAX(0.0,SAVE111RES(IRG,CURIYR))
               CSTEE(1,IRG) = MAX(0.0,COST111RES(IRG,CURIYR))
               QTYEE(2,IRG) = MAX(0.0,SAVE111COM(IRG,CURIYR))
               CSTEE(2,IRG) = MAX(0.0,COST111COM(IRG,CURIYR))
               QTYEE(3,IRG) = MAX(0.0,SAVE111IND(IRG,CURIYR))
               CSTEE(3,IRG) = MAX(0.0,COST111IND(IRG,CURIYR))
!     if (curitr .gt. 1)write(6,3435) curiyr+1989,irg,qtyee(1,irg),qtyee(2,irg),qtyee(3,irg)
!3435 format(1h ,'!effin',i4,i3,3f10.3)
         END DO  
         DO REG = 1 , UNRGNS
            QREE = 0.0
            CREE = 0.0
            QCEE = 0.0
            CCEE = 0.0
            QIEE = 0.0
            CIEE = 0.0
            DO IRG = 1 , MNUMCR - 1
               IF (MAPPCTON(REG,IRG,1) .GT. 0.0)THEN
                  QREE = QREE + QTYEE(1,IRG) * MAPPCTON(REG,IRG,1)
                  CREE = CREE + CSTEE(1,IRG) * MAPPCTON(REG,IRG,1)
               END IF
!  COMM EE is for AB32 and should get mapped to CA, if in place
               IF ((AB32SW .GT. 0) .AND. (IRG .EQ. CA_CR)) THEN
!                 IF (REG .EQ. CA_NR) THEN
                  IF (CA_NR(REG,2) .GT. 0.0) THEN
                     QCEE = QCEE + QTYEE(2,IRG)*CA_NR(REG,2)
                     CCEE = CCEE + CSTEE(2,IRG)*CA_NR(REG,2)
                  ENDIF
               ELSEIF (MAPPCTON(REG,IRG,2) .GT. 0.0)THEN
                  QCEE = QCEE + QTYEE(2,IRG) * MAPPCTON(REG,IRG,2)
                  CCEE = CCEE + CSTEE(2,IRG) * MAPPCTON(REG,IRG,2)
               END IF
               IF (MAPPCTON(REG,IRG,3) .GT. 0.0)THEN
                  QIEE = QIEE + QTYEE(3,IRG) * MAPPCTON(REG,IRG,3)
                  CIEE = CIEE + CSTEE(3,IRG) * MAPPCTON(REG,IRG,3)
               END IF
!           if (reg .eq. 1 .and. curitr .gt. 1)write(6,2345) curiyr+1989,irg,curitr,  &
!              SAVE111RES(IRG,CURIYR) , COST111RES(IRG,CURIYR),  &
!              SAVE111COM(IRG,CURIYR) , COST111COM(IRG,CURIYR),  &
!              SAVE111IND(IRG,CURIYR) , COST111IND(IRG,CURIYR)
!2345 format(1h ,'!effcr',i4,i3,i3,12f10.3)
            END DO
            EGENNREE(REG,CURIYR) = QREE + QCEE + QIEE
            EGENNREE(MNUMNR,CURIYR) = EGENNREE(MNUMNR,CURIYR) + QREE + QCEE + QIEE
            ECSTNREE(REG,CURIYR) = CREE + CCEE + CIEE
            ECSTNREE(MNUMNR,CURIYR) = ECSTNREE(MNUMNR,CURIYR) + CREE + CCEE + CIEE
            EERN(REG,CURIYR) = QREE
            EERN(MNUMNR,CURIYR) = EERN(MNUMNR,CURIYR) + QREE
            EECN(REG,CURIYR) = QCEE
            EECN(MNUMNR,CURIYR) = EECN(MNUMNR,CURIYR) + QCEE
            EEIN(REG,CURIYR) = QIEE
            EEIN(MNUMNR,CURIYR) = EEIN(MNUMNR,CURIYR) + QIEE
!           if (curitr .gt. 1)write(6,3456) curiyr+1989,reg,  &
!              EGENNREE(REG,CURIYR),  &
!              EGENNREE(MNUMNR,CURIYR),  &
!              ECSTNREE(REG,CURIYR),  &
!              ECSTNREE(MNUMNR,CURIYR)
!3456 format(1h ,'!effnr',i4,i3,6f10.3)
            DO IRG = 1 , UNFRGN
               VALUE = DBLE(EGEN_NRFR(ECP_D_CAP + 1,REG,IRG) / EGEN_NRFR(ECP_D_CAP + 1,REG,UNFRGN + 1))
               IF (VALUE .GT. DBLE(0.0)) THEN
                  EGENFREE(IRG,CURIYR) = EGENFREE(IRG,CURIYR) + VALUE * EGENNREE(REG,CURIYR)
                  EGENFREE(EFD_D_MFRG + 1,CURIYR) = EGENFREE(EFD_D_MFRG + 1,CURIYR) + VALUE * EGENNREE(REG,CURIYR)
                  ECSTFREE(IRG,CURIYR) = ECSTFREE(IRG,CURIYR) + VALUE * ECSTNREE(REG,CURIYR)
                  ECSTFREE(EFD_D_MFRG + 1,CURIYR) = ECSTFREE(EFD_D_MFRG + 1,CURIYR) + VALUE * ECSTNREE(REG,CURIYR)
                  EERF(IRG,CURIYR) = EERF(IRG,CURIYR) + VALUE * EERN(REG,CURIYR)
                  EERF(EFD_D_MFRG + 1,CURIYR) = EERF(EFD_D_MFRG + 1,CURIYR) + VALUE * EERN(REG,CURIYR)
                  EECF(IRG,CURIYR) = EECF(IRG,CURIYR) + VALUE * EECN(REG,CURIYR)
                  EECF(EFD_D_MFRG + 1,CURIYR) = EECF(EFD_D_MFRG + 1,CURIYR) + VALUE * EECN(REG,CURIYR)
                  EEIF(IRG,CURIYR) = EEIF(IRG,CURIYR) + VALUE * EEIN(REG,CURIYR)
                  EEIF(EFD_D_MFRG + 1,CURIYR) = EEIF(EFD_D_MFRG + 1,CURIYR) + VALUE * EEIN(REG,CURIYR)
               END IF
            END DO
!           if (reg .eq. unrgns .and. curitr .gt. 1)then
!              do irg = 1 , EFD_D_MFRG
!              write(6,4567) curiyr+1989,irg,curitr,  &
!              EGENFREE(irg,CURIYR),  &
!              EGENFREE(EFD_D_MFRG + 1,CURIYR),  &
!              ECSTFREE(irg,CURIYR),  &
!              ECSTFREE(EFD_D_MFRG + 1,CURIYR)
!4567 format(1h ,'!efffr',i4,i3,i3,6f10.3)
!              end do
!           end if
         END DO
!        CREATE COLUMNS FOR ENERGY EFFICIENCY SAVINGS, IF APPROPRIATE
!        EMM REGIONS
         IF (CO2_STDSW .GT. 0)THEN
            DO REG = 1 , UNRGNS
               IF (EGENNREE(REG,CURIYR) .GT. DBLE(0.0)) THEN
                  COLUMN = 'G' // URGNME(REG)(6:7) // 'EE' // 'SNR'; call makmsk(COLUMN_mask,':G:' , URGNME(REG)(6:7) , ':EE:' , ':SNR:')
                  VALUE = DBLE(EGENNREE(REG,CURIYR))
                  CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CPP2,35')
!                 IF ERC TRADING, PUT INTO SUPPLY CURVE ROW
                  IF (CO2_ERCSW .EQ. 1 .AND. CO2_ERCNR(REG) .EQ. 2 .AND. CO2_STDTN(REG) .LT. 2)THEN
                     ROW_ERC = 'ERCTNR' // URGNME(REG)(6:7); call makmsk(ROW_ERC_mask,':ERCTNR:' , URGNME(REG)(6:7))
                     CALL DVAL(COLUMN,ROW_ERC,DBLE(-1.0),COLUMN_mask,ROW_ERC_mask,'ED$CPP2,36')
!                    ACCUMULATE AVAILABLE CREDITS
                     ERCQAVLN(REG,CURIYR) = ERCQAVLN(REG,CURIYR) + VALUE
                     ERCQAVLN(MNUMNR,CURIYR) = ERCQAVLN(MNUMNR,CURIYR) + VALUE
                  ELSE
                     VALUE = DBLE(0.0 - CO2_STDRN(REG,CURIYR))
                     ROW = 'CO2RNR' // URGNME(REG)(6:7); call makmsk(ROW_mask,':CO2RNR:' , URGNME(REG)(6:7))
                     CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CPP2,37')
                  END IF
               END IF
            END DO
         END IF
      END IF

!     DETERMINE EE SAVINGS AND COSTS FOR CALIFORNIA WHEN AB32/SB32 BUT NOT CPP  ! moved this to a separate subroutine so this one can be dropeped later

!!      IF (AB32SW .GT. 0 .AND. (CO2_STDSW .LE. 0 .OR. (CURIYR + UHBSYR) .LT. CO2_STDY1))THEN
!        IF (SAVE111RES(CA_CR,CURIYR) .LT. 0.0)THEN
!           QTYEE(1,CA_CR) = 0.0
!           CSTEE(1,CA_CR) = 0.0
!        ELSE
!           QTYEE(1,CA_CR) = SAVE111RES(CA_CR,CURIYR)
!           CSTEE(1,CA_CR) = COST111RES(CA_CR,CURIYR)
!        END IF
!        IF (SAVE111COM(CA_CR,CURIYR) .LT. 0.0)THEN
!           QTYEE(2,CA_CR) = 0.0
!           CSTEE(2,CA_CR) = 0.0
!        ELSE
!           QTYEE(2,CA_CR) = SAVE111COM(CA_CR,CURIYR)
!           CSTEE(2,CA_CR) = COST111COM(CA_CR,CURIYR)
!        END IF
!        IF (SAVE111IND(CA_CR,CURIYR) .LT. 0.0)THEN
!           QTYEE(3,CA_CR) = 0.0
!           CSTEE(3,CA_CR) = 0.0
!        ELSE
!           QTYEE(3,CA_CR) = SAVE111IND(CA_CR,CURIYR)
!           CSTEE(3,CA_CR) = COST111IND(CA_CR,CURIYR)
!        END IF
!!         IRG = CA_CR
!!         QTYEE(1,IRG) = MAX(0.0,SAVE111RES(IRG,CURIYR))
!!        CSTEE(1,IRG) = MAX(0.0,COST111RES(IRG,CURIYR))
!!         QTYEE(2,IRG) = MAX(0.0,SAVE111COM(IRG,CURIYR))
!!        CSTEE(2,IRG) = MAX(0.0,COST111COM(IRG,CURIYR))
!!         QTYEE(3,IRG) = MAX(0.0,SAVE111IND(IRG,CURIYR))
!!         CSTEE(3,IRG) = MAX(0.0,COST111IND(IRG,CURIYR))

!!         EGENNREE(MNUMNR,CURIYR) = 0.0
!!         ECSTNREE(MNUMNR,CURIYR) = 0.0
!!         DO REG = 1, UNRGNS
!!           EGENNREE(REG,CURIYR) = QTYEE(1,CA_CR)*CA_NR(REG,1) + QTYEE(2,CA_CR)*CA_NR(REG,2) + QTYEE(3,CA_CR)*CA_NR(REG,3)
!!           ECSTNREE(REG,CURIYR) = CSTEE(1,CA_CR)*CA_NR(REG,1) + CSTEE(2,CA_CR)*CA_NR(REG,2) + CSTEE(3,CA_CR)*CA_NR(REG,3)
!!          EGENNREE(MNUMNR,CURIYR) = EGENNREE(MNUMNR,CURIYR) + EGENNREE(REG,CURIYR)
!!           ECSTNREE(MNUMNR,CURIYR) = ECSTNREE(MNUMNR,CURIYR) + ECSTNREE(REG,CURIYR)
!!         ENDDO
!!      END IF

 !     QTYEE(1,MNUMCR) = 0.0
 !     CSTEE(1,MNUMCR) = 0.0
 !     QTYEE(2,MNUMCR) = 0.0
 !     CSTEE(2,MNUMCR) = 0.0
 !     QTYEE(3,MNUMCR) = 0.0
 !     CSTEE(3,MNUMCR) = 0.0
 !     DO REG = 1 , MNUMCR - 2
 !        write (13,5656) curiyr+1989,curitr,reg,  &
 !        SAVE111RES(REG,CURIYR),SAVE111COM(REG,CURIYR),  &
 !        COST111RES(REG,CURIYR),COST111COM(REG,CURIYR),  &
 !        QTYEE(1,REG),QTYEE(2,REG),QTYEE(1,REG) + QTYEE(2,REG),  &
 !        CSTEE(1,REG),CSTEE(2,REG),CSTEE(1,REG) + CSTEE(2,REG)
 !        QTYEE(1,MNUMCR) = QTYEE(1,MNUMCR) + QTYEE(1,REG)
 !        CSTEE(1,MNUMCR) = CSTEE(1,MNUMCR) + CSTEE(1,REG)
 !        QTYEE(2,MNUMCR) = QTYEE(2,MNUMCR) + QTYEE(2,REG)
 !        CSTEE(2,MNUMCR) = CSTEE(2,MNUMCR) + CSTEE(2,REG)
 !        QTYEE(3,MNUMCR) = QTYEE(3,MNUMCR) + QTYEE(3,REG)
 !        CSTEE(3,MNUMCR) = CSTEE(3,MNUMCR) + CSTEE(3,REG)
 !5656 format(1h ,'!eecr/qri,qci,cri,cci,qro,qco,qto,cro,cco,cto,',i4,i3,i3,12f10.3)
 !     END DO
 !        write (13,5656) curiyr+1989,curitr,mnumcr,  &
 !        SAVE111RES(MNUMCR,CURIYR),SAVE111COM(MNUMCR,CURIYR),  &
 !        COST111RES(MNUMCR,CURIYR),COST111COM(MNUMCR,CURIYR),  &
 !        QTYEE(1,MNUMCR),QTYEE(2,MNUMCR),QTYEE(1,MNUMCR) + QTYEE(2,MNUMCR),  &
 !        CSTEE(1,MNUMCR),CSTEE(2,MNUMCR),CSTEE(1,MNUMCR) + CSTEE(2,MNUMCR)
 !     DO REG = 1 , UNRGNS
 !        write (13,5657) curiyr+1989,curitr,reg,  &
 !        EGENNREE(REG,CURIYR), ECSTNREE(REG,CURIYR)
 !5657 format(1h ,'!eenr/qt,ct',i4,i3,i3,4f10.3)
 !     END DO
 !        write (13,5657) curiyr+1989,curitr,mnumnr,  &
 !        EGENNREE(MNUMNR,CURIYR), ECSTNREE(MNUMNR,CURIYR)

      RETURN
      END

      SUBROUTINE CALC_EEAB32
!
      use efd_row_col
      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'eusprc'
      include'e111d'
      include'cogen'
      include'dsmdimen'
      include'dsmsectr'
      include'edbdef'
      include'uefdout'
      include'emission'
      include'cdsparms'
      include'csapr'
      include'emmemis'
 
      INTEGER IRET,IRG,KRG,YEAR,GRP,REG,ISC
      INTEGER CA_CR,CA_ST
      REAL*4 QTYEE(3,MNUMCR),CSTEE(3,MNUMCR)
      REAL*8 VALUE,CA_NR(MNUMNR,3),CA_TOT(3)
      INTEGER AB32SW

      INTEGER CENSUS,ISTATE               !functions
 
      AB32SW=RTOVALUE('AB32SW  ',1)

!     IDENTIFY CENSUS AND EMM REGIONS FOR CALIFORNIA FOR AB32 VS. CPP EE SAVINGS

      IF ((CURIYR + UHBSYR) .EQ. UESTYR .AND. CURITR .EQ. 1)THEN
         CA_NR = 0.0
         CA_TOT = 0.0
         CA_ST = ISTATE('CA')
         CA_CR = CENSUS(CA_ST)
!      write(6,*) 'CA_CR ',CA_CR,CA_ST
         DO REG = 1 , UNRGNS
            IF (URGNME(REG)(1:2) .EQ. 'ca') THEN 
               DO ISC = 1, 3
                 CA_NR(REG,ISC) = MappCtoN(REG,CA_CR,ISC)
                 CA_TOT(ISC) = CA_TOT(ISC) + MappCtoN(REG,CA_CR,ISC)
               ENDDO
             ENDIF
         END DO
         IF (CA_TOT(1) .EQ. 0 .AND. CA_TOT(2) .EQ. 0 .AND. CA_TOT(3) .EQ. 0) THEN
            WRITE(6,'(" Did not find California among URGNME region names")')
            STOP
         END IF
         DO REG = 1, UNRGNS
            DO ISC = 1,3
              IF (CA_TOT(ISC) .GT. 0.0) THEN
                CA_NR(REG,ISC) = CA_NR(REG,ISC) / CA_TOT(ISC)
!            write(6,*) 'CA_NR = ',REG,ISC,CA_NR(REG,ISC)
              ELSE
                CA_NR(REG,ISC) = 0.0
              ENDIF
            ENDDO
         ENDDO
      END IF


!     DETERMINE EE SAVINGS AND COSTS FOR CALIFORNIA WHEN AB32/SB32 BUT NOT CPP

      IF (AB32SW .GT. 0 .AND. (CO2_STDSW .LE. 0 .OR. (CURIYR + UHBSYR) .LT. CO2_STDY1))THEN
   
         IRG = CA_CR
         QTYEE(1,IRG) = MAX(0.0,SAVE111RES(IRG,CURIYR))
         CSTEE(1,IRG) = MAX(0.0,COST111RES(IRG,CURIYR))
         QTYEE(2,IRG) = MAX(0.0,SAVE111COM(IRG,CURIYR))
         CSTEE(2,IRG) = MAX(0.0,COST111COM(IRG,CURIYR))
         QTYEE(3,IRG) = MAX(0.0,SAVE111IND(IRG,CURIYR))
         CSTEE(3,IRG) = MAX(0.0,COST111IND(IRG,CURIYR))

         EGENNREE(MNUMNR,CURIYR) = 0.0
         ECSTNREE(MNUMNR,CURIYR) = 0.0
         DO REG = 1, UNRGNS
           EGENNREE(REG,CURIYR) = QTYEE(1,CA_CR)*CA_NR(REG,1) + QTYEE(2,CA_CR)*CA_NR(REG,2) + QTYEE(3,CA_CR)*CA_NR(REG,3)
           ECSTNREE(REG,CURIYR) = CSTEE(1,CA_CR)*CA_NR(REG,1) + CSTEE(2,CA_CR)*CA_NR(REG,2) + CSTEE(3,CA_CR)*CA_NR(REG,3)
           EGENNREE(MNUMNR,CURIYR) = EGENNREE(MNUMNR,CURIYR) + EGENNREE(REG,CURIYR)
           ECSTNREE(MNUMNR,CURIYR) = ECSTNREE(MNUMNR,CURIYR) + ECSTNREE(REG,CURIYR)
         ENDDO
      END IF

      QTYEE(1,MNUMCR) = 0.0
      CSTEE(1,MNUMCR) = 0.0
      QTYEE(2,MNUMCR) = 0.0
      CSTEE(2,MNUMCR) = 0.0
      QTYEE(3,MNUMCR) = 0.0
      CSTEE(3,MNUMCR) = 0.0
      DO REG = 1 , MNUMCR - 2
         write (13,5656) curiyr+1989,curitr,reg,  &
         SAVE111RES(REG,CURIYR),SAVE111COM(REG,CURIYR),  &
         COST111RES(REG,CURIYR),COST111COM(REG,CURIYR),  &
         QTYEE(1,REG),QTYEE(2,REG),QTYEE(1,REG) + QTYEE(2,REG),  &
         CSTEE(1,REG),CSTEE(2,REG),CSTEE(1,REG) + CSTEE(2,REG)
         QTYEE(1,MNUMCR) = QTYEE(1,MNUMCR) + QTYEE(1,REG)
         CSTEE(1,MNUMCR) = CSTEE(1,MNUMCR) + CSTEE(1,REG)
         QTYEE(2,MNUMCR) = QTYEE(2,MNUMCR) + QTYEE(2,REG)
         CSTEE(2,MNUMCR) = CSTEE(2,MNUMCR) + CSTEE(2,REG)
         QTYEE(3,MNUMCR) = QTYEE(3,MNUMCR) + QTYEE(3,REG)
         CSTEE(3,MNUMCR) = CSTEE(3,MNUMCR) + CSTEE(3,REG)
 5656 format(1h ,'!eecr/qri,qci,cri,cci,qro,qco,qto,cro,cco,cto,',i4,i3,i3,12f10.3)
      END DO
         write (13,5656) curiyr+1989,curitr,mnumcr,  &
         SAVE111RES(MNUMCR,CURIYR),SAVE111COM(MNUMCR,CURIYR),  &
         COST111RES(MNUMCR,CURIYR),COST111COM(MNUMCR,CURIYR),  &
         QTYEE(1,MNUMCR),QTYEE(2,MNUMCR),QTYEE(1,MNUMCR) + QTYEE(2,MNUMCR),  &
         CSTEE(1,MNUMCR),CSTEE(2,MNUMCR),CSTEE(1,MNUMCR) + CSTEE(2,MNUMCR)
      DO REG = 1 , UNRGNS
         write (13,5657) curiyr+1989,curitr,reg,  &
         EGENNREE(REG,CURIYR), ECSTNREE(REG,CURIYR)
 5657 format(1h ,'!eenr/qt,ct',i4,i3,i3,4f10.3)
      END DO
         write (13,5657) curiyr+1989,curitr,mnumnr,  &
         EGENNREE(MNUMNR,CURIYR), ECSTNREE(MNUMNR,CURIYR)

      RETURN
      END
!
!     ED$GRD
!
      SUBROUTINE ED$GRD
!
      use efd_row_col
      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'uefdout'
      include'ecp_nuc'
      include'cdsparms'
      include'emm_aimms'
      
!      COMMON /GRDSRC/ GRD_CASN,GRD_SRCN,GRD_SRCC
!      INTEGER GRD_CASN                                       ! Number of grid resilience sources 
!      CHARACTER*15 GRD_SRCN(MX_GRDSRC)                       ! Grid resilience source names
!      CHARACTER*1  GRD_SRCC(MX_GRDSRC)                       ! Grid resilience source codes

      INTEGER IRET,IRG,KRG,YEAR,GRP,REG,IGR
      REAL*4 CO2_QTY(EFD_D_MFRG),QTY
      REAL*4 OLDTGT,NEWTGT,RTECHG,ACHRTL,OLDSUM,NEWSUM,NEWAVG,NEWANN
      REAL*8 VALUE,PVCAP,PVGEN
      CHARACTER*16 ROW,ROW_RAT,ROW_GEN,ROW_CAP,COLUMN
      character*30 ROW_RAT_mask,ROW_GEN_mask,ROW_CAP_mask

      efdsub='ED$GRD'

!     EMM REGION GRID RESILIENCE ROWS


      DO IRG = 1 , UNRGNS
         PVCAP = DBLE(DPVTOTCAPNR(IRG,CURIYR) * 0.001)
         PVGEN = DBLE(DPVTOTGENNR(IRG,CURIYR) * 0.001)
!        if (curitr .gt. maxitr)write(6,2000) curiyr+1989,irg,  &
!            PVCAP , PVGEN
!2000 format(1h ,'!dgpv',i4,i3,2f10.3)
         DO IGR = 1 , GRD_CASN
            ROW_RAT = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_RAT_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
            CALL DROWTYPE(ROW_RAT,'G       ',ROW_RAT_mask)
            CALL DRHS(EFDRHS,ROW_RAT,DBLE(0.0),ROW_RAT_mask,'ED$GRD,1')
!           COLUMN = 'GRDRX' // UPRGCD(IRG)(6:7); call makmsk(COLUMN_mask,':GRDRX:' , UPRGCD(IRG)(6:7))
!           CALL DVAL(COLUMN,ROW,DBLE(-1.0),COLUMN_mask,ROW_mask,'ED$GRD,2')
!           CALL DVAL(ROW,EFDOBJ,0.0001D0,ROW_mask,EFDOBJ,'ED$GRD,3')
            ROW_GEN = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GEN_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
            CALL DROWTYPE(ROW_GEN,'G       ',ROW_GEN_mask)
            CALL DRHS(EFDRHS,ROW_GEN,DBLE(0.0),ROW_GEN_mask,'ED$GRD,2')
!           COLUMN = 'GRDGX' // UPRGCD(IRG)(6:7); call makmsk(COLUMN_mask,':GRDGX:' , UPRGCD(IRG)(6:7))
!           CALL DVAL(COLUMN,ROW,DBLE(-1.0),COLUMN_mask,ROW_mask,'ED$GRD,5')
!           CALL DVAL(ROW,EFDOBJ,0.0001D0,ROW_mask,EFDOBJ,'ED$GRD,6')
!           EXCLUDE END-USE PV
            IF (DPVDISPATCH .EQ. .TRUE. ) THEN
               IF (GRD_RATSA(WIPV,IRG,IGR) .GE. 0.0)THEN
                  COLUMN = 'DPVCP' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':DPVCP:' , URGNME(IRG)(6:7))
                  CALL DBND(EFDBND,COLUMN,PVCAP,PVCAP,COLUMN_mask,'ED$GRD,3')
                  IF (PVCAP .GT. DBLE(0.0)) THEN
                     IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                        VALUE = -1.0 * (PVGEN / PVCAP) * (GRD_RATSA(WIPV,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                     ELSE
                        VALUE = -1.0 * (PVGEN / PVCAP) * GRD_RATSA(WIPV,IRG,IGR)
                     END IF
                  ELSE
                     VALUE = -1.0
                  END IF
                  CALL DVAL(COLUMN,ROW_RAT,VALUE,COLUMN_mask,ROW_RAT_mask,'ED$GRD,4')
                  IF (PVCAP .GT. DBLE(0.0)) THEN
                     VALUE = -1.0 * PVGEN / PVCAP
                  ELSE
                     VALUE = -1.0
                  END IF
                  CALL DVAL(COLUMN,ROW_GEN,VALUE,COLUMN_mask,ROW_GEN_mask,'ED$GRD,5')
               END IF
            END IF
         END DO
      END DO

      RETURN
      END

!     - ED$BENCH -
!     THIS SUBROUTINE SETS UP BENCHMARKING TO STEO

      SUBROUTINE ED$BENCH
!
      use efd_row_col
      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include 'dispin'
      include 'dispett'
      include 'postpr'
      include'uefdout'
      include'cogen'
 
      INTEGER IRET,IRG,KRG,YEAR,GRP,REG

      CHARACTER*16 ROW,COLUMN
      REAL*8 VALUE
      REAL*4 TOLADJ 
      INTEGER EMMBMSW,IMRG,EXRG,CRG
      REAL NETIMP,TOTFRMIMP,TOTEXP

      efdsub='ED$BENCH'

      EMMBMSW = RTOVALUE('EMMBENCH',1) !  switch to indicate whether to relax benchmarking for side cases


!     RELAX BENCHMARKING IN CERTAIN SIDE CASES

      TOLADJ = 1.0
      IF (EMMBMSW .EQ. 0) THEN   !turn off benchmarking by increasing tolerance a lot
          TOLADJ = 10.0         
      ELSEIF (EMMBMSW .EQ. 2) THEN  ! loosen benchmarking a little in years 2 and 3
        IF ((CURIYR + UHBSYR) .EQ. UPSTYR + 1) THEN
           TOLADJ = 1.5
        ELSEIF ((CURIYR + UHBSYR) .EQ. UPSTYR+2) THEN
           TOLADJ = 3.0
        ELSEIF ((CURIYR + UHBSYR) .GT. UPSTYR+2) THEN
           TOLADJ = 5.0
        END IF
      ELSEIF (EMMBMSW .EQ. 3) THEN  ! loosen benchmarking more significantly in years 2 and 3
        IF ((CURIYR + UHBSYR) .EQ. UPSTYR + 1) THEN
           TOLADJ = 3.0
        ELSEIF ((CURIYR + UHBSYR) .EQ. UPSTYR+2) THEN
           TOLADJ = 5.0
        ELSEIF ((CURIYR + UHBSYR) .GT. UPSTYR+2) THEN
           TOLADJ = 10.0
        END IF
      ENDIF


!     GENERATION ROWS

!     COAL
      IF (BMCLGEN(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'GENCLB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':GENCLB:' , URGNME(IRG)(6:7))
            IF (IRG .LE. UNRGNS .OR. IRG .EQ. MNUMNR)THEN
               ROW = 'GENCL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENCL:' , URGNME(IRG)(6:7))
               CALL DROWTYPE(ROW,'E       ',ROW_mask)
               CALL DRHS(EFDRHS,ROW,DBLE(0.0),ROW_mask,'ED$BENCH,1')
               CALL DVAL(COLUMN,ROW,DBLE(-1.0),COLUMN_mask,ROW_mask,'ED$BENCH,2')
            END IF
            IF (IRG .NE. MNUMNR)THEN
               ROW = 'GENCL' // URGNME(MNUMNR)(6:7); call makmsk(ROW_mask,':GENCL:' , URGNME(MNUMNR)(6:7))
               CALL DVAL(COLUMN,ROW,DBLE(1.0),COLUMN_mask,ROW_mask,'ED$BENCH,3')
!              INCLUDE ALASKA/HAWAII IN NATIONAL TOTALS
               IF (IRG .GE. MNUMNR-2)THEN
                  VALUE = UGNCLNR(1,IRG,CURIYR) + UGNCLNR(2,IRG,CURIYR) +  &
                          (CGNTGEN(IRG,CURIYR, 1,1) + CGNTGEN(IRG,CURIYR, 1,2)) * 0.001
                  CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$BENCH,4')
               END IF
            ELSE
               CALL DBND(EFDBND,COLUMN,DBLE(BMCLGEN(CURIYR) * (1.0 - BMCLTOL * TOLADJ)),DBLE(BMCLGEN(CURIYR) * (1.0 + BMCLTOL*TOLADJ)),COLUMN_mask,'ED$BENCH,5')
            END IF
         END DO
      END IF
!     GAS
      IF (BMNGGEN(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'GENNGB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':GENNGB:' , URGNME(IRG)(6:7))
            IF (IRG .LE. UNRGNS .OR. IRG .EQ. MNUMNR)THEN
               ROW = 'GENNG' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENNG:' , URGNME(IRG)(6:7))
               CALL DROWTYPE(ROW,'E       ',ROW_mask)
               CALL DRHS(EFDRHS,ROW,DBLE(0.0),ROW_mask,'ED$BENCH,6')
               CALL DVAL(COLUMN,ROW,DBLE(-1.0),COLUMN_mask,ROW_mask,'ED$BENCH,7')
            END IF
            IF (IRG .NE. MNUMNR)THEN
               ROW = 'GENNG' // URGNME(MNUMNR)(6:7); call makmsk(ROW_mask,':GENNG:' , URGNME(MNUMNR)(6:7))
               CALL DVAL(COLUMN,ROW,DBLE(1.0),COLUMN_mask,ROW_mask,'ED$BENCH,8')
!              INCLUDE ALASKA/HAWAII IN NATIONAL TOTALS
               IF (IRG .GE. MNUMNR-2)THEN
                  VALUE = UGNGFNR(1,IRG,CURIYR) + UGNGFNR(2,IRG,CURIYR) +  &
                          UGNGINR(1,IRG,CURIYR) + UGNGINR(2,IRG,CURIYR) +  &
 !                         UGNGCNR(1,IRG,CURIYR) + UGNGCNR(2,IRG,CURIYR) +  &
                          (CGNTGEN(IRG,CURIYR, 3,1) + CGNTGEN(IRG,CURIYR, 3,2)) * 0.001
                  CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$BENCH,9')
               END IF
            ELSE
               CALL DBND(EFDBND,COLUMN,DBLE(BMNGGEN(CURIYR) * (1.0 - BMNGTOL*TOLADJ)),DBLE(BMNGGEN(CURIYR) * (1.0 + BMNGTOL*TOLADJ)),COLUMN_mask,'ED$BENCH,10')
            END IF
         END DO
      END IF
!     OIL
      IF (BMOLGEN(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'GENOLB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':GENOLB:' , URGNME(IRG)(6:7))
            IF (IRG .LE. UNRGNS .OR. IRG .EQ. MNUMNR)THEN
               ROW = 'GENOL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENOL:' , URGNME(IRG)(6:7))
               CALL DROWTYPE(ROW,'E       ',ROW_mask)
               CALL DRHS(EFDRHS,ROW,DBLE(0.0),ROW_mask,'ED$BENCH,11')
               CALL DVAL(COLUMN,ROW,DBLE(-1.0),COLUMN_mask,ROW_mask,'ED$BENCH,12')
            END IF
            IF (IRG .NE. MNUMNR)THEN
               ROW = 'GENOL' // URGNME(MNUMNR)(6:7); call makmsk(ROW_mask,':GENOL:' , URGNME(MNUMNR)(6:7))
               CALL DVAL(COLUMN,ROW,DBLE(1.0),COLUMN_mask,ROW_mask,'ED$BENCH,13')
!              INCLUDE ALASKA/HAWAII IN NATIONAL TOTALS
               IF (IRG .GE. MNUMNR-2)THEN
                  VALUE = UGNDSNR(1,IRG,CURIYR) + UGNDSNR(2,IRG,CURIYR) +  &
                          UGNRLNR(1,IRG,CURIYR) + UGNRLNR(2,IRG,CURIYR) +  &
                          UGNRHNR(1,IRG,CURIYR) + UGNRHNR(2,IRG,CURIYR) +  &
                          (CGNTGEN(IRG,CURIYR, 2,1) + CGNTGEN(IRG,CURIYR, 2,2)) * 0.001
                  CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$BENCH,14')
               END IF
            ELSE
               CALL DBND(EFDBND,COLUMN,DBLE(BMOLGEN(CURIYR) * (1.0 - BMOLTOL*TOLADJ)),DBLE(BMOLGEN(CURIYR) * (1.0 + BMOLTOL*TOLADJ)),COLUMN_mask,'ED$BENCH,15')
            END IF
         END DO
      END IF

!     CONSUMPTION ROWS

!     COAL
      IF (BMCLCON(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'CONCLB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':CONCLB:' , URGNME(IRG)(6:7))
            IF (IRG .LE. UNRGNS .OR. IRG .EQ. MNUMNR)THEN
               ROW = 'CONCL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CONCL:' , URGNME(IRG)(6:7))
!              IF (CURIRUN .EQ. 1) THEN
!                 CALL DROWTYPE(ROW,'N       ',ROW_mask)
!              ELSE
               CALL DROWTYPE(ROW,'E       ',ROW_mask)
!              END IF
               CALL DRHS(EFDRHS,ROW,DBLE(0.0),ROW_mask,'ED$BENCH,16')
               CALL DVAL(COLUMN,ROW,DBLE(-1.0),COLUMN_mask,ROW_mask,'ED$BENCH,17')
            END IF
            IF (IRG .NE. MNUMNR)THEN
               ROW = 'CONCL' // URGNME(MNUMNR)(6:7); call makmsk(ROW_mask,':CONCL:' , URGNME(MNUMNR)(6:7))
               CALL DVAL(COLUMN,ROW,DBLE(1.0),COLUMN_mask,ROW_mask,'ED$BENCH,18')
!              INCLUDE ALASKA/HAWAII IN NATIONAL TOTALS
               IF (IRG .GE. MNUMNR-2)THEN
                  VALUE = UFLCLNR(1,IRG,CURIYR) + UFLCLNR(2,IRG,CURIYR)
                  CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$BENCH,19')
               END IF
            ELSE
               CALL DBND(EFDBND,COLUMN,DBLE(BMCLCON(CURIYR) * (1.0 - BMCLTOL*TOLADJ)),DBLE(BMCLCON(CURIYR) * (1.0 + BMCLTOL*TOLADJ)),COLUMN_mask,'ED$BENCH,20')
            END IF
         END DO
      END IF
!     GAS
      IF (BMNGCON(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'CONNGB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':CONNGB:' , URGNME(IRG)(6:7))
            IF (IRG .LE. UNRGNS .OR. IRG .EQ. MNUMNR)THEN
               ROW = 'CONNG' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CONNG:' , URGNME(IRG)(6:7))
               CALL DROWTYPE(ROW,'E       ',ROW_mask)
               CALL DRHS(EFDRHS,ROW,DBLE(0.0),ROW_mask,'ED$BENCH,21')
               CALL DVAL(COLUMN,ROW,DBLE(-1.0),COLUMN_mask,ROW_mask,'ED$BENCH,22')
            END IF
            IF (IRG .NE. MNUMNR)THEN
               ROW = 'CONNG' // URGNME(MNUMNR)(6:7); call makmsk(ROW_mask,':CONNG:' , URGNME(MNUMNR)(6:7))
               CALL DVAL(COLUMN,ROW,DBLE(1.0),COLUMN_mask,ROW_mask,'ED$BENCH,23')
!              INCLUDE ALASKA/HAWAII IN NATIONAL TOTALS
               IF (IRG .GE. MNUMNR-2)THEN
                  VALUE = UFLGFNR(1,IRG,CURIYR) + UFLGFNR(2,IRG,CURIYR) +  &
                          UFLGINR(1,IRG,CURIYR) + UFLGINR(2,IRG,CURIYR) !+  &
!                          UFLGCNR(1,IRG,CURIYR) + UFLGCNR(2,IRG,CURIYR)
                  CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$BENCH,24')
               END IF
            ELSE
               CALL DBND(EFDBND,COLUMN,DBLE(BMNGCON(CURIYR) * (1.0 - BMNGTOL*TOLADJ)),DBLE(BMNGCON(CURIYR) * (1.0 + BMNGTOL*TOLADJ)),COLUMN_mask,'ED$BENCH,25')
            END IF
         END DO
      END IF
!     OIL
      IF (BMOLCON(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'CONOLB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':CONOLB:' , URGNME(IRG)(6:7))
            IF (IRG .LE. UNRGNS .OR. IRG .EQ. MNUMNR)THEN
               ROW = 'CONOL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CONOL:' , URGNME(IRG)(6:7))
               CALL DROWTYPE(ROW,'E       ',ROW_mask)
               CALL DRHS(EFDRHS,ROW,DBLE(0.0),ROW_mask,'ED$BENCH,26')
               CALL DVAL(COLUMN,ROW,DBLE(-1.0),COLUMN_mask,ROW_mask,'ED$BENCH,27')
            END IF
            IF (IRG .NE. MNUMNR)THEN
               ROW = 'CONOL' // URGNME(MNUMNR)(6:7); call makmsk(ROW_mask,':CONOL:' , URGNME(MNUMNR)(6:7))
               CALL DVAL(COLUMN,ROW,DBLE(1.0),COLUMN_mask,ROW_mask,'ED$BENCH,28')
!              INCLUDE ALASKA/HAWAII IN NATIONAL TOTALS
               IF (IRG .GE. MNUMNR-2)THEN
                  VALUE = UFLDSNR(1,IRG,CURIYR) + UFLDSNR(2,IRG,CURIYR) +  &
                          UFLRLNR(1,IRG,CURIYR) + UFLRLNR(2,IRG,CURIYR) +  &
                          UFLRHNR(1,IRG,CURIYR) + UFLRHNR(2,IRG,CURIYR)
                  CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$BENCH,29')
               END IF
            ELSE
               CALL DBND(EFDBND,COLUMN,DBLE(BMOLCON(CURIYR) * (1.0 - BMOLTOL*TOLADJ)),DBLE(BMOLCON(CURIYR) * (1.0 + BMOLTOL*TOLADJ)),COLUMN_mask,'ED$BENCH,30')
            END IF
         END DO
      END IF
!
!     NET IMPORTS
!
      IF (BMNETIMP(CURIYR) .GT. 0.0) THEN
      TOTFRMIMP = 0.0 
      TOTEXP = 0.0  
        DO IMRG = 1, UNRGNS
!         write(6,*) ' canimp ztimpf ztexpf expci ',ztimpf(imrg)/1000000.0,ztexpf(imrg)/1000000.0,expci(curiyr,imrg)/1000000.0
          TOTFRMIMP =  TOTFRMIMP + ZTIMPF(IMRG)/1000000.0
          TOTEXP =  TOTEXP + (ZTEXPF(IMRG) + EXPCI(CURIYR,IMRG))/1000000.0
        ENDDO
!       
        NETIMP = BMNETIMP(CURIYR) - TOTFRMIMP + TOTEXP
        COLUMN = 'CNIMPBUS' ; call makmsk(COLUMN_mask,':CNIMPBUS:')
        ROW = 'CANIMPUS' ; call makmsk(ROW_mask,':CANIMPUS:')
        CALL DROWTYPE(ROW,'E       ',ROW_mask)
        CALL DRHS(EFDRHS,ROW,DBLE(0.0),ROW_mask,'ED$BENCH,31')
        CALL DVAL(COLUMN,ROW,DBLE(-1.0),COLUMN_mask,ROW_mask,'ED$BENCH,32')
        CALL DBND(EFDBND,COLUMN,DBLE(NETIMP*(1.0-BMIMPTOL*TOLADJ)),DBLE(NETIMP*(1.0+BMIMPTOL*TOLADJ)),COLUMN_mask,'ED$BENCH,33')
!       write(6,*) ' steo net imports ',bmnetimp(curiyr),totfrmimp,totexp,netimp
      END IF

      RETURN
      END
!
!     ==================================================================
!     - ELDISP -
!     THIS SUBROUTINE DISPATCHES AVAILABLE CAPACITY TO MEET A GIVEN
!     LOAD AND TABULATES OPERATING COSTS, FUEL CONSUMPTION AND CAPACITY
!     UTILIZATION RATES.
!     ==================================================================
!
!     INPUT VARIABLES
!     EQEL   = QUANTITY OF ELECTRICITY DEMAMDED
!     EEITAJ = BASE LOAD ADJUSTMENT FOR INTERREGIONAL TRANSFERS
!     ECCOPM = CAPACITY OUT FOR PLANNED MAINTENANCE
!     ECCAP  = TOTAL EXISTING CAPACITY
!     EENSP  = NUMBER OF SEASONAL PERIODS
!     ULHTRT_EFD = HEATRATES BY CAPACITY TYPE AND UTILIZATION
!     ECOMR  = VARIABLE O & M COSTS BY CAPACITY TYPE
!     EPFUEL = FUEL PRICES
!     ECFLTP = FUEL(S) CONSUMED BY EACH CAPACITY TYPE
!     ECMFSH = MAXIMUM FUEL SHARE BY PLANT GROUP
!     ENFLTP = NUMBER OF FUEL TYPES
!     ECNTP  = NUMBER OF CAPACITY TYPES
!     ECCFMR = CAPACITY FACTORS USED FOR MERIT ORDER CALCULATIONS
!     EHCAP  = PONDAGE HYDRO CAPACITY BY CAPACITY FACTOR GROUPING
!     EHNTP  = NUMBER OF PONDAGE HYDRO CAPACITY FACTOR GROUPINGS
!     ECACAP = TOTAL CAPACITY AVAILABLE BY CAPACITY TYPE
!     EHTCAP = TOTAL EXISTING PONDAGE HYDRO
!     ECFOR  = FORCED OUTAGE RATE BY CAPACITY TYPE
!     ECLFR  = LOAD FOLLOWING RATE BY CAPACITY TYPE
!     EQTDLS = TRANSMISSION AND DISTRIBUTION LOSS (PERCENTAGE)
!     EETIME = NUMBER OF HOURS IN SEASONAL PERIOD
!     INTERMEDIATE VARIABLES
!     ECFSHR = FUEL SHARES BETWEEN PRIMARY AND SECONDARY FUELS
!     ECTYP  = CAPACITY TYPE INDEX IN ORDER OF INCREASING OPERATING
!     COSTS
!     OUTPUT VARIABLES
!     ECSDPN = NUMBER OF HORIZONTAL SLICES MADE IN THE LOAD CURVE
!     ECSDPC = CAPACITY ALLOCATED TO EACH HORIZONTAL SLICE OF THE
!     LOAD CURVE
!     ECSDPE = ELECTRICITY GENERATED IN EACH SLICE OF THE LOAD CURVE
!     ECSDPT = TYPE OF CAPACITY ALLOCATED TO EACH SLICE OF THE LOAD
!     CURVE
!     EQTFL  = TOTAL FUEL CONSUMPTION
!     EQTGN = TOTAL GENERATION BY FUEL
!     ERTFL  = TOTAL FUEL COSTS
!     ERTOM  = TOTAL O & M COSTS
!
!     DEBUG SUBCHK
!     END DEBUG
      SUBROUTINE ELDISP(IYR,IRG,ISOL)

!
      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispout'
      include'dispuse'
      include'dispcrv'
      include'fuelin'
      include'bildin'
      include'dispett'
      include'uettout'
      include'postpr'
      include'dsmdimen'
      include'dsmsectr'
      include'dsmunits'
      include'dsmtoefd'
      include'elcntl'
      include'elout'
      include'ecpcntl'
      include'plntctl'
      include'emission'
      include'efdin'
!

      REAL TGSSHR,TRSSHR,TDSSHR,TCLSHR,RENFOR
      INTEGER TFR, IDG,JRNW,IR
      INTEGER IYR,IRG,ISOL,KMXCP,I,J,NSO2,ISO2,IRNW,IVCT,IPGRP
      INTEGER IFL,IFP,G,LDSEG,LDGRP,GRP,SEG
      REAL TOTHRS,TEMP
      INTEGER K,IE1,IE2,IE3,IE4,IOWN,NEXT,IGRP
      REAL AREATV(EFD_D_MVS),AREATX(EFD_D_MVS),AREITV(EFD_D_MVS)
      REAL AREITX(EFD_D_MVS)
      REAL*8 EXTMP(EFD_D_MHS,EFD_D_MVS),IMTMP(EFD_D_MHS,EFD_D_MVS),KFOR,KPMR
      REAL*8 CUMAREA(EFD_D_MHS),CUMAREI(EFD_D_MHS),TQFFL
      INTEGER ICAP,JCAP,KCAP,ITYP(ETT_D_MTG),JTYP(ETT_D_MTG),KTYP(ETT_D_MTG)
      REAL IMPANN,EXPANN,INTRUP
      REAL*8 TMP$CAP(EFD_D_MVS),TCAP,THGHT(EFD_D_SSZ,ELD_D_DAY)
      REAL*8 SEASON,ADJUST,ANNUAL,GEN
      REAL  CAPTSL
      INTEGER IPLNT,IPGRPDB,IECP
      REAL FOR
!
!     MORE VALCAP TYPE VARIABLES
!
      INTEGER FSTGRP(EFD_D_MVS,EFD_D_MSP),FSTSEG(EFD_D_MVS,EFD_D_MSP)
      SAVE FSTGRP,FSTSEG
      INTEGER SLICE
      REAL VMOM(NMOM)
      REAL vex1,vex2,vex3,vex4,xcap
      INTEGER IMOM,IBLK
      REAL XNUMPLNT,NUMPLNT
      REAL PLNTGRPSIZ(EFD_D_CAP)
      DATA PLNTGRPSIZ/3*0.300, 0.400, 5*0.380, 4*0.300, 4*0.160, 2*0.120, 3*0.250, 3*0.400, 0.010, 4*0.600, 2*0.100, 2*0.030, 0.050, 5*0.200, 3*0.100, 3*0.050, 3*0.100, 3*0.005, 0.002, 0.001/
!
!     INITIALIZE TOTAL COST, GENERATION, POLLUTANTS AND CONSUMPTION ARRAYS
!
      PPFUEL = 0
      PPOM = 0
      PPCAP = 0
      DO 5 I = 1 , USW_OWN
         ERTFL(I) = 0.0
         ERTOM(I) = 0.0
         ERTSO2(I) = 0.0
         ERTNOX(I) = 0.0
         ERTHG(I) = 0.0
         ERTGSUB(I) = 0.0
    5 CONTINUE
      ETALLW = 0.0
      ETSO2 = 0.0
      ETNOX = 0.0
      ETCO2 = 0.0
      ETCAR = 0.0
      ETCO1 = 0.0
      ETVOC = 0.0
      ETHG = 0.0
      ETGEN = 0.0
!
!     INITIALIZE ECP FUEL MAP INFORMATION
!
      DO 9 I = 1 , EFD_D_NFL
         DO 6 J = 1 , EFD_D_MFRG
            EPFLRG(I,J) = MAX ( MIN ( 1.0, EPFLRG(I,J)), 0.0)
    6    CONTINUE
         DO 7 J = 1 , MNUMCR
            EPCRMP(I,J) = MAX ( MIN ( 1.0, EPCRMP(I,J)) , 0.0)
    7    CONTINUE
         DO 8 J = 1 , ECP_D_NFL
            EPFMAP(J,I) = MAX ( MIN ( 1.0, EPFMAP(J,I)) , 0.0)
    8    CONTINUE
    9 CONTINUE
!
!     INITIALIZE SO2 ALLOWANCES, PENALTY COST AND PRODUCTION BY COMPLIANCE GROUP
!
      EGALLW = 0.0
      EGSO2 = 0.0
!
!     INITALIZE GENERATION, COSTS, COMPLIANCE AND CONSUMPTION BY PLANT TYPE
!
      DO 12 I = 1,EIPGRP
         DO 112 IOWN = 1 , USW_OWN
            ERPFL(I,IOWN) = 0.0
            ERPOM(I,IOWN) = 0.0
            EQPGN(I,IOWN) = 0.0
            EQPGN_ALT(I,IOWN) = 0.0
  112    CONTINUE
         DO IFP = 1 , EFD_D_NFL
           DO IOWN = 1 , USW_OWN
            EQPFLGN(I,IOWN,IFP) = 0.0
            EQPFLCN(I,IOWN,IFP) = 0.0
           ENDDO
         ENDDO
         EQPCP(I) = 0.0
         EQPFL(I) = 0.0
         EQPSO2(I) = 0.0
         EQPNOX(I) = 0.0
         EQPCO2(I) = 0.0
         EQPHG(I) = 0.0
         DO 11 J = 1, EENSP
            EGENPS(I,J) = 0.0
            ECAPPS(I,J) = 0.0
            EAVLPS(I,J) = 0.0
   11    CONTINUE
   12 CONTINUE
      EQPGNSQ = 0.0
      EQPGNSQ_ALT = 0.0
!
!     INITALIZE GENERATION, COSTS, COMPLIANCE AND CONSUMPTION BY RENWABLE TYPE
!
      DO 14 I = 1,EIHGRP
         DO 114 IOWN = 1 , USW_OWN
            ERHOM(I,IOWN) = 0.0
            EQHGN(I,IOWN) = 0.0
           DO IFP = 1, EFD_D_NFL
            EQPFLGN(I + EIPGRP,IOWN,IFP) = 0.0
            EQPFLCN(I + EIPGRP,IOWN,IFP) = 0.0
           ENDDO
  114    CONTINUE
         EQHCP(I) = 0.0
!
!        INITIALIZE RENEWABLE CAPACITY AND ENERGY SEASONAL OUTPUT ARRAYS
!
         DO 13 J = 1 , EENSP
            ECAPHS(I,J) = 0.0
            EGENHS(I,J) = 0.0
   13    CONTINUE
   14 CONTINUE
!
!     INITALIZE GENERATION, COSTS, COMPLIANCE AND CONSUMPTION BY PLANT TYPE
!
      DO  I = 1,EIDGRP
         DO  IOWN = 1 , USW_OWN
            ERDFL(I,IOWN) = 0.0
            ERDOM(I,IOWN) = 0.0
            EQDGN(I,IOWN) = 0.0
         END DO
         DO IFP = 1 , EFD_D_NFL
           DO IOWN = 1 , USW_OWN
            EQPFLGN(I + EIPGRP + EIHGRP,IOWN,IFP) = 0.0
            EQPFLCN(I + EIPGRP + EIHGRP,IOWN,IFP) = 0.0
           ENDDO
         ENDDO
         EQDCP(I) = 0.0
         EQDFL(I) = 0.0
         EQDSO2(I) = 0.0
         EQDNOX(I) = 0.0
         EQDCO2(I) = 0.0
         EQDHG(I) = 0.0
         DO  J = 1, EENSP
            EGENDS(I,J) = 0.0
            ECAPDS(I,J) = 0.0
            EAVLDS(I,J) = 0.0
         END DO
      END DO
!
!     INITIALIZE COSTS, GENERATION, CONSUMPTION AND EMISSIONS  BY FUEL TYPE
!
      DO 15 I = 1 , ENFLTP
         ERFFL(I) = 0.0
         ERFFC(I) = 0.0
         DO 115 IOWN = 1 , USW_OWN
            EQFGN(I,IOWN) = 0.0
            EQFFL(I,IOWN) = 0.0
  115    CONTINUE
         EQFSO2(I) = 0.0
         UQFSO2(I,IRG) = 0.0
         EQFNOX(I) = 0.0
         EQFCO2(I) = 0.0
         EQFHG(I) = 0.0
         EFHCNT(I) = 0.0
   15 CONTINUE
!
!     CALCULATE TOTAL HOURS IN THE YEAR
!
      TOTHRS = 0.0
      DO 16 J = 1 , EENSP
         TOTHRS = TOTHRS + EETIME(J)
   16 CONTINUE
!
!     CAPTURE ALLOWANCES AND AVERAGE ANNUAL CAPACITY
!
      DO 20 I = 1 , ECNTP
         IPGRP = ECASTS(I)
         IPGRPDB = ECDBID(I)
         IECP = ECTECP(I)
         DO 17 J = 1 , EENSP
            EAVLPS(IPGRP,J) = EAVLPS(IPGRP,J) + ECCAP(I,J)
            EQPCP(IPGRP) = EQPCP(IPGRP) + ECCAP(I,J) * &
             EETIME(J) / TOTHRS
   17    CONTINUE
         DO 18 J = 1 , EIMCG
            IF (IPGRPDB .LE. 0 .OR. IPGRPDB .GT. EMM_D_GRP) THEN
!jj            ISO2 = 1
               NSO2 = 0
               WRITE(6,1073) CURIYR+UHBSYR,CURITR,J,IRG,I,IPGRP,IPGRPDB
 1073          FORMAT(1X,"IPGRPDB_OOPS",7(":",I6))
            ELSE
               IF (ULHGGP(IPGRPDB) .LE. 0 .OR. ULHGGP(IPGRPDB) .GT. NDREG) THEN
!jj               ISO2 = 1
                  NSO2 = 0
                  WRITE(6,1074) CURIYR+UHBSYR,CURITR,J,IRG,I,IPGRP,IPGRPDB,ULHGGP(IPGRPDB)
 1074             FORMAT(1X,"ULHGGP_OOPS",8(":",I6))
               ELSE
!jj               ISO2 = SO2_GRP_BY_CLRG(ULHGGP(IPGRPDB))
                  NSO2 = NUM_SO2_GRP
               END IF
            END IF
!jj         IF (ISO2 .GT. 0) THEN
               ETALLW = ETALLW + ECALLW(I)
!jj            EGALLW(ISO2) = EGALLW(ISO2) + ECALLW(I)
!jj         END IF
            IF (NSO2 .GT. 0) THEN
             DO ISO2 = 1 , NSO2
              IF (SO2_SHR_BY_CLRG(ULHGGP(IPGRPDB),ISO2) .GT. 0.0)THEN
               EGALLW(ISO2) = EGALLW(ISO2) + ECALLW(I) * SO2_SHR_BY_CLRG(ULHGGP(IPGRPDB),ISO2)
              END IF
             END DO
            ELSE
               ISO2 = 1
               EGALLW(ISO2) = EGALLW(ISO2) + ECALLW(I)
            END IF
   18    CONTINUE
         TGSSHR = 0.0
         TRSSHR = 0.0
         TDSSHR = 0.0
         TCLSHR = 0.0
         DO IFL = 1, EIFPLT
          IF (ECFLTP(I,IFL) .GT. 0) THEN
           IF (UIGAS(ECFLTP(I,IFL)) .EQ. 1) THEN
              TGSSHR = MAX(ECMFSH(I,IFL),TGSSHR)
           ELSEIF (UIRES(ECFLTP(I,IFL)) .EQ. 1) THEN
              TRSSHR = MAX(ECMFSH(I,IFL),TRSSHR)
           ELSEIF (UIDIS(ECFLTP(I,IFL)) .EQ. 1) THEN
              TDSSHR = MAX(ECMFSH(I,IFL),TDSSHR)
           ELSEIF (UICOL(ECFLTP(I,IFL)) .EQ. 1) THEN
              TCLSHR = MAX(ECMFSH(I,IFL),TCLSHR)
           ENDIF
          ENDIF
         ENDDO
          TFR = EPNFLRG(ECCR(I),ECLR(I),ECGR(I),ECAR(I))
          if (tfr .eq. 0) &
          write(UF_DBG,*) 'tfr ',I,IPGRP,IPGRPDB,ECCR(I),ECLR(I),ECGR(I),TFR
         IF (IPGRP .LE. UICAS) THEN   !coal plants, sum by ecp type
          EDMXGS(1,IECP,TFR) = EDMXGS(1,IECP,TFR) + TGSSHR * ULCAPC(IPGRPDB)
          EDMXRS(1,IECP,TFR) = EDMXRS(1,IECP,TFR) + TRSSHR * ULCAPC(IPGRPDB)
          IF (TDSSHR  .GT. 0.004) TDSSHR = 0.004
          EDMXDS(1,IECP,TFR) = EDMXDS(1,IECP,TFR) + TDSSHR * ULCAPC(IPGRPDB)
          FSCAPWT(1,IECP,TFR) = FSCAPWT(1,IECP,TFR) + ULCAPC(IPGRPDB)
         ELSE
          EDMXGS(2,IPGRP,TFR) = EDMXGS(2,IPGRP,TFR) + TGSSHR * ULCAPC(IPGRPDB)
          EDMXRS(2,IPGRP,TFR) = EDMXRS(2,IPGRP,TFR) + TRSSHR * ULCAPC(IPGRPDB)
          EDMXDS(2,IPGRP,TFR) = EDMXDS(2,IPGRP,TFR) + TDSSHR * ULCAPC(IPGRPDB)
          EDMXCL(1,IECP,TFR) = EDMXCL(1,IECP,TFR)  + TCLSHR * ULCAPC(IPGRPDB)   !<--this will keep track of ECP NG's coal share per fuel region
          FSCAPWT(1,IECP,TFR) = FSCAPWT(1,IECP,TFR) + ULCAPC(IPGRPDB)
          EDMXCL(2,IPGRP,TFR) = EDMXCL(2,IPGRP,TFR)  + TCLSHR * ULCAPC(IPGRPDB)   !<--this will keep track of CTN's coal share per fuel region and it will be  used for ccalc_cB_rCle
          FSCAPWT(2,IPGRP,TFR) = FSCAPWT(2,IPGRP,TFR) + ULCAPC(IPGRPDB)
         ENDIF
!
!        ECP Max Share Calculations
!
         IF (IECP .GT. 0 .AND. IECP .LE. ECP_D_DSP) THEN
            IF (ULMRUN(IPGRPDB) .EQ. 0) THEN
               TPMXWGT(IECP,TFR) = TPMXWGT(IECP,TFR) + MAX(ULCAPC(IPGRPDB) * 8.76 * 0.01 , ULTGEN(IPGRPDB))
               TPMXGAS(IECP,TFR) = TPMXGAS(IECP,TFR) + TGSSHR * MAX(ULCAPC(IPGRPDB) * 8.76 * 0.01 , ULTGEN(IPGRPDB))
               TPMXOIL(IECP,TFR) = TPMXOIL(IECP,TFR) + (TRSSHR + TDSSHR) * MAX(ULCAPC(IPGRPDB) * 8.76 * 0.01 , ULTGEN(IPGRPDB))
               TPMXCOL(IECP,TFR) = TPMXCOL(IECP,TFR) + TCLSHR * MAX(ULCAPC(IPGRPDB) * 8.76 * 0.01 , ULTGEN(IPGRPDB))
            ELSE
               TPMXWGT_MR(IECP,TFR) = TPMXWGT_MR(IECP,TFR) + MAX(ULCAPC(IPGRPDB) * 8.76 * 0.01 , ULTGEN(IPGRPDB))
               TPMXGAS_MR(IECP,TFR) = TPMXGAS_MR(IECP,TFR) + TGSSHR * MAX(ULCAPC(IPGRPDB) * 8.76 * 0.01 , ULTGEN(IPGRPDB))
               TPMXOIL_MR(IECP,TFR) = TPMXOIL_MR(IECP,TFR) + (TRSSHR + TDSSHR) * MAX(ULCAPC(IPGRPDB) * 8.76 * 0.01 , ULTGEN(IPGRPDB))
               TPMXCOL_MR(IECP,TFR) = TPMXCOL_MR(IECP,TFR) + TCLSHR * MAX(ULCAPC(IPGRPDB) * 8.76 * 0.01 , ULTGEN(IPGRPDB))
            END IF
         END IF

   20 CONTINUE
!
!     CAPTURE AVERAGE ANNUAL RENEWABLES CAPACITY
!
      DO 50 I = 1 , EHNTP
         IPGRP = EHHYTP(I)
         IPGRPDB = EHDBID(I)
         IRNW = EHHYTP(I) - EIPGRP
         DO 51 J = 1 , EENSP
            EQHCP(IRNW) = EQHCP(IRNW) + EHCAP(I,J) * &
             EETIME(J) / TOTHRS
   51    CONTINUE
         TGSSHR = 0.0
         TRSSHR = 0.0
         TDSSHR = 0.0
         DO IFL = 1, EIFPLT
          IF (EHFLTP(I,IFL) .GT. 0) THEN
           IF (UIGAS(EHFLTP(I,IFL)) .EQ. 1) THEN
              TGSSHR = MAX(EHMFSH(I,IFL),TGSSHR)
           ELSEIF (UIRES(EHFLTP(I,IFL)) .EQ. 1) THEN
              TRSSHR = MAX(EHMFSH(I,IFL),TRSSHR)
           ELSEIF (UIDIS(EHFLTP(I,IFL)) .EQ. 1) THEN
              TDSSHR = MAX(EHMFSH(I,IFL),TDSSHR)
           ENDIF
          ENDIF
         ENDDO
          TFR = EPNFLRG(EHCR(I),EHLR(I),EHGR(I),EHAR(I))
          if (tfr .eq. 0) THEN

             write(6,'(A,I2,A,9I6)') "tfr is 0: ",TFR," (see?) ",curiyr+1989,curitr,I,IPGRP,IPGRPDB,EHCR(I),EHLR(I),EHGR(I),EHAR(I)

             tfr = 1
          END IF

!         write(6,*) 'tfr ',I,IPGRP,IPGRPDB,EHCR(I),EHLR(I),EHGR(I),EHAR(I),TFR
          EDMXGS(2,IPGRP,TFR) = EDMXGS(2,IPGRP,TFR) + TGSSHR * ULCAPC(IPGRPDB)
          EDMXRS(2,IPGRP,TFR) = EDMXRS(2,IPGRP,TFR) + TRSSHR * ULCAPC(IPGRPDB)
          EDMXDS(2,IPGRP,TFR) = EDMXDS(2,IPGRP,TFR) + TDSSHR * ULCAPC(IPGRPDB)
!         IF (TGSSHR .GT. 0.0 .OR. TRSSHR .GT. 0.0 .OR. TDSSHR .GT. 0.0) &
              FSCAPWT(2,IPGRP,TFR) = FSCAPWT(2,IPGRP,TFR) + ULCAPC(IPGRPDB)
   50 CONTINUE
!
!     CAPTURE AVERAGE ANNUAL DISTRIBUTED GENERATION CAPACITY
!
      DO I = 1 , EDNTP
         IPGRP = EDASTS(I)
         IDG = EDASTS(I) - EIPGRP - EIHGRP
         DO J = 1 , EENSP
            EAVLDS(IDG,J) = EAVLDS(IDG,J) + EDCAP(I,J)
            EQDCP(IDG) = EQDCP(IDG) + EDCAP(I,J) * &
             EETIME(J) / TOTHRS
         END DO
         TGSSHR = 0.0
         TRSSHR = 0.0
         TDSSHR = 0.0
         DO IFL = 1, EIFPLT
          IF (EDFLTP(I,IFL) .GT. 0) THEN
           IF (UIGAS(EDFLTP(I,IFL)) .EQ. 1) THEN
              TGSSHR = MAX(EDMFSH(I,IFL),TGSSHR)
           ELSEIF (UIRES(EDFLTP(I,IFL)) .EQ. 1) THEN
              TRSSHR = MAX(EDMFSH(I,IFL),TRSSHR)
           ELSEIF (UIDIS(EDFLTP(I,IFL)) .EQ. 1) THEN
              TDSSHR = MAX(EDMFSH(I,IFL),TDSSHR)
           ENDIF
          ENDIF
         ENDDO
          TFR = EPNFLRG(EDCR(I),EDLR(I),EDGR(I),EDAR(I))
          EDMXGS(2,IPGRP,TFR) = EDMXGS(2,IPGRP,TFR) + TGSSHR * ULCAPC(IPGRPDB)
          EDMXRS(2,IPGRP,TFR) = EDMXRS(2,IPGRP,TFR) + TRSSHR * ULCAPC(IPGRPDB)
          EDMXDS(2,IPGRP,TFR) = EDMXDS(2,IPGRP,TFR) + TDSSHR * ULCAPC(IPGRPDB)
!         IF (TGSSHR .GT. 0.0 .OR. TRSSHR .GT. 0.0 .OR. TDSSHR .GT. 0.0) &
              FSCAPWT(2,IPGRP,TFR) = FSCAPWT(2,IPGRP,TFR) + ULCAPC(IPGRPDB)
      END DO
!
!     CAPTURE CANADIAN INTERRUPTIBLES GENERATION

      ETIMPE = 0.0
      UTIMPE(IRG,IYR) = 0.0
      IF(EXPCI(IYR,IRG) .NE. 0.0) THEN
         ETEXPE = EXPCI(IYR,IRG) / 1000.0
         UTEXPE(IRG,IYR) = EXPCI(IYR,IRG) / 1000.0
         ULEIXE(IRG) = EXPCI(IYR,IRG) / 1000.0
      ELSE
         ETEXPE = 0.0
         UTEXPE(IRG,IYR) = 0.0
      ENDIF

!     IF((ETEXPE - ETIMPE).NE.0) THEN
!     WRITE(UF_ETT,366) IYR,IRG,'11',(ETEXPE-ETIMPE)
!366       FORMAT(I4,1X,I4,1X,A5,F10.4)
!
!     ENDIF

!
!     DETERMINE THE MERIT ORDER FOR DISPATCHING PRIORITY
!
      CALL ELMRIT(EENSP + 1,IRG)
!
      DO 90 I = 1,EENSP
!
!        ETT INITIALIZE ETT VARIABLES
!
         IMPANN = 0
         EXPANN = 0
         INTRUP = 0

!        IF(USW_ETT.GT.0) THEN
         DO 167 J = 1,ELNVCT(I)
            AREATX(J) = 0.0
            AREATV(J) = 0.0
            AREITV(J) = 0.0
            MARCST(I,J) = 0
            MARTYP(I,J) = 0.0
            MARCST2(I,J) = 0.0
            NMARCST(IRG,I,J,IYR) = 0
            NMARTYP(IRG,I,J,IYR) = 0.0
            NMARREG(IRG,I,J,IYR) = 0
  167    CONTINUE
         DO 161 K = 1,EFD_D_MHS
!           DO 161 K=1,ECNTP
            DO 162 J = 1,ELNVCT(I)
               EXTMP(K,J) = 0.0
               IMTMP(K,J) = 0.0
  162       CONTINUE
            CUMAREA(K) = DBLE(0.0)
            CUMAREI(K) = DBLE(0.0)
  161    CONTINUE
!
!        DETERMINE THE MERIT ORDER FOR DISPATCHING PRIORITY
!
         CALL ELMRIT(I,IRG)

!        ADJUST AVAILABLE CAPACITY FOR THIS TIME PERIOD BY THE PLANNED MAINTENANCE SCHEDULE

         DO J = 1 , ECNTP
            IGRP = ECDBID(J)
            ECACAP(J) = (ECCAP(J,I) - ECCOPM(J,I)) * 0.001
            KMXCP = ECMXCP(J)
            KFOR = ECFOR(IGRP)
            KPMR = ECPMR(IGRP)

         END DO

!        DETERMINE SEASONAL LOAD REQUIREMENTS (GWH) INCLUDING T&D LOSSES AND CONVERT TO UNITS OF GW_SEASON

         ETNVCT = ELNVCT(I)

!        ADJUST FOR INTERREGIONAL TRANSFERS (FIRM CAPACITY SALES)

!        FIRST CALCULATE SEASON'S CANADIAN INTERRUPTIBLE POWER IN GW

         IMPANN = ETIMPE/EETIME(I)
         EXPANN = ETEXPE/EETIME(I)
         INTRUP = (EXPANN * (EETIME(I) / TOTHRS)) - (IMPANN * (EETIME(I) / TOTHRS))

         SEASON = DBLE(0.0)
         ADJUST = ( EEITAJ(I) + INTRUP + EOUIPP + EOUNT ) * EETIME(I)

         DO IVCT = 1 , ELNVCT(I)
            ETHGHT(IVCT,I,IRG) = ELHGHT(IVCT,I) + EEITAJ(I) + INTRUP + EOUIPP + EOUNT
            ETWDTH(IVCT,I,IRG) = ELWDTH(IVCT,I)
            SEASON = SEASON + ELHGHT(IVCT,I) * ELWDTH(IVCT,I)
            GRP = ELGRP(IVCT,I)
            SEG = ELSEG(IVCT,I)
            UTHGHT(SEG,GRP,IRG) = ELHGHT(IVCT,I) + EEITAJ(I) + INTRUP + EOUIPP + EOUNT

            WRITE(18,3746) CURIRUN, CURIYR+1989, CURITR, IYR, IRG, I, IVCT, SEASON, ADJUST, EEITAJ(I), INTRUP, EOUIPP, EOUNT, &
               EETIME(I), ELHGHT(IVCT,I), ETHGHT(IVCT,I,IRG), ELWDTH(IVCT,I)
 3746       FORMAT(1X,'GENERATION_EFD',7(':',I4),2(':',F18.3),4(':',F12.6),':',F6.0,2(':',F18.6),":",F6.0)

         END DO

         BGENREQ(IRG) = BGENREQ(IRG) + SEASON * 0.001
         BNTCOWN(IRG) = BNTCOWN(IRG) + EOUNT * EETIME(I) * 0.001
         BNUGOWN(IRG) = BNUGOWN(IRG) + EOUIPP * EETIME(I) * 0.001
         BMEXICAN(IRG) = BMEXICAN(IRG) + INTRUP * EETIME(I) * 0.001

         EPEAK(I) = ETHGHT(1,I,IRG)

!        CALCULATE ADJUSTED LOAD

         EQLOAD(1) = 0.0
         DO IVCT = 1, ETNVCT
            ETAREA(IVCT) = ETWDTH(IVCT,I,IRG) * ETHGHT(IVCT,I,IRG)
            EQLOAD(1) = EQLOAD(1) + ETAREA(IVCT)
         END DO
         TEMP = EQLOAD(1)

!
!        Compute Distributed Generation Inputs
!
         CALL ELDGNI(I,IRG)
!
!        Determine Renewable Dispatch
!
!        CALL ELRNEW(IYR,IRG,I)
!
!        CALCULATE LOAD FOR ECONOMIC DISPATCH
!
         EQLOAD(1) = 0.0
         DO 40 IVCT = 1 , ETNVCT
            ETAREA(IVCT) = ETWDTH(IVCT,I,IRG) * ETHGHT(IVCT,I,IRG)
            EQLOAD(1) = EQLOAD(1) + ETAREA(IVCT)
   40    CONTINUE
!
!        CALL ELALOC(I,IRG)
!
                       IF (IRG .EQ. 1) THEN
                            DO IVCT = 1, ETNVCT
                               FSTGRP(IVCT,I) = ELGRP(IVCT,I)
              FSTSEG(IVCT,I) = ELSEG(IVCT,I)
           END DO ! IVCT
         ENDIF ! irg

         CALL ELFACT(I,IRG,FSTGRP,FSTSEG)

   90 CONTINUE

      RETURN
      END
!
!     ELCOST - ! TABULATES FUEL CONSUMPTION, FUEL COSTS, AND O&M COSTS
!
      SUBROUTINE ELCOST(IRG,IYR,ITYPE )
      USE EPHRTS_SWTICHES
      USE EPHRTS_FILE_UNIT_NUMBERS 
!
      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'dispout'
      include 'dispuse'
      include 'fuelin'
      include 'wrenew'
      include 'wwdcomon'
      include 'dispett'
      include 'ecpcntl'
      include 'postpr'
      include 'uefdout'
      include 'uecpout'
      include 'elout'
      include 'elcntl'
      include 'bildin'
      include 'epmbank'
      include 'emablk'
      include 'emoblk'
      include 'efdin'
      include 'cdsparms'
      include 'coalemm'
      include 'acoalprc'
      include 'uso2grp'
      include 'ecp_coal'
      include 'plntctl'
      include 'emission'
      include 'macout'
      include 'e111d'
      include 'dsmdimen'
      include 'dsmtfecp'
      include 'dsmtoefd'
      include 'dsmcaldr'
      include 'csapr'
      include 'emmemis'
      include 'ccatsdat'
      include 'udatout'

      COMMON /TMP_RESTORE/ PM_FRAC
      REAL*8 PM_FRAC(EMM_D_GRP,EFD_D_MSP)
      
      REAL*4    GEN,FUEL,TEMP,TOTGOIL,GENR,FUELR,NOXFAC,HGFAC,ACFAC,ACIFUEL,ACIOM, GEN_ALT, FUEL_ALT
      REAL*8    FACTE,VAL
      INTEGER*4 IPGRP,IFLTP,IFP,ISP,I,N,IMCG,NSO2,ISO2,KSCRB,IFLRG,NFLRG,ICL,JGRP,JN,I_45Q,CTYPE
      INTEGER*4 ICR,INR,IST,IGR,IFOWN,IECP,J,OPR,IVIN,IRNK,JECPT
      INTEGER*4 IE,IDUAL,IGRP,IECPT,IECPFL,ISTEP
      INTEGER*4 F,K,IRG,IYR,ITYPE,IBTP,INCT,IPHASE,NOXYR,FULLYR, FL_RG_25
      INTEGER SLICE
      INTEGER TST_ECP_FUEL(ECP_D_DSP,ECP_D_NFL) ! Test if ECP Fuel Type is used by ECP Plant Type 0=>No 1=>Yes
      INTEGER IFL,ITYP,ISCRB
!
      REAL*4    CRBCST(EFD_D_NFL)
      REAL*4    CRBPRC(EFD_D_NFL)
      REAL*4    CFSUB,tmpsub,nuclim
      REAL*8    EOR_REV               ! EOR Revenue per Million Btu
      REAL*8    AFFEM,sumAFFEM(ECP_D_CAP),DIFF111,CO2LB
      INTEGER   IP
      LOGICAL E_DEBUG_EXIST
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
         INQUIRE(FILE="EPHRTS_DEBUG_FILE.TXT", EXIST=E_DEBUG_EXIST)
         IF (E_DEBUG_EXIST) THEN
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="OLD", POSITION="APPEND", ACTION="WRITE")
         ELSE
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="NEW", ACTION="WRITE")
         END IF
      END IF
      
      FULLYR = USYEAR(CURIYR)

!     FACTE = (2000.0 / 2204.0) / 1000000.0

      FACTE = 1.0 / 1000000.0

      IF (IRG .EQ. 1)THEN
         EGEN_NRST = 0.0
         EGEN_NRFR = 0.0
         EGEN_FRST = 0.0
      END IF

      UGENNGCF(IRG,CURIYR) = 0.0
      IF (IRG .EQ. 1) THEN
          UGENNGCF(MNUMNR,CURIYR) = 0.0
      ENDIF
      
      ULCO2CST(IRG,CURIYR) = 0.0
      sumAFFEM = 0.0
      
      ACITOTB(IRG,CURIYR) = 0.0

      TST_ECP_FUEL = 0
      DO J = 1 , ECP_D_FPP
         DO I = 1 , ECP_D_DSP
            ITYP = UPTTYP(I)
            IFL = UPFLTP(I,J)
            IF (IFL .GT. 0 .AND. IFL .LE. ECP_D_NFL) THEN
               TST_ECP_FUEL(ITYP,IFL) = 1
            END IF
            IFL = UDFLTP(I,J)
            IF (IFL .GT. 0 .AND. IFL .LE. ECP_D_NFL) THEN
               TST_ECP_FUEL(ITYP,IFL) = 1
            END IF
         END DO
      END DO
!
!     IDENTIFY CO2 PENALTY FOR JEFFORDS RUNS
!
      DO IFLTP = 1 , EFD_D_NFL
         CRBCST(IFLTP) = 0.0
         CRBPRC(IFLTP) = 0.0
      END DO
      IF (UPRNWCAS .EQ. 1 .OR. PCAP_CAR .EQ. 1) THEN
         IF ( (BANK_FLAG /= 0) .AND. (CURCALYR .LT. BANK_STARTYR) ) THEN
         ELSE IF ((TAX_FLAG /= 0) .OR. (PERMIT_FLAG /= 0)) THEN
            DO ICL = 1 , NCLUT1
               CRBCST(ICL) = JCLCLNR(CURIYR,ICL)
            END DO
            CRBCST(UIDS) = JDSEL(CURIYR)
            CRBCST(UIRL) = JRLEL(CURIYR)
            CRBCST(UIRH) = JRHEL(CURIYR)
            CRBCST(UIGF) = JGFELGR(CURIYR)
            CRBCST(UIGI) = JGIELGR(CURIYR)
            CRBCST(UIGC) = 0.0 !JGCELGR(CURIYR) NO CARBON PENALTY FOR UIGC (H2 EPHRTS)
         END IF
      END IF

!     identify carbon component of fuel price for all fuels - for sequestration adjustment

      DO ICL = 1 , NCLUT1
         CRBPRC(ICL) = JCLCLNR(CURIYR,ICL)
      END DO
      CRBPRC(UIDS) = JDSEL(CURIYR)
      CRBPRC(UIRL) = JRLEL(CURIYR)
      CRBPRC(UIRH) = JRHEL(CURIYR)
      CRBPRC(UIGF) = JGFELGR(CURIYR)
      CRBPRC(UIGI) = JGIELGR(CURIYR)
      CRBPRC(UIGC) = 0.0 !JGCELGR(CURIYR) NO CARBON PENALTY FOR UIGC (H2 EPHRTS)
!
!     LOOP OVER SEASONS
!
      DO ISP = 1 ,EENSP
!
!        LOOP ACROSS EACH SOLUTION STEP AND STORE RESULTS
!
!        DO 100 I = 1,ECDSPNMX(IRG)
!
!           N = ECDSPT(I)

         DO 100 N = 1, ECNTP
            JN = MAP_ECNTP_EFD_GRPS(N,IRG)
            IGRP = ECDBID(N)
            JGRP = ULIGRP(IGRP)
            IPGRP = ECASTS(N)    !EFD Plant Group
            IFOWN = ECFOWN(N)
            ICR = ECCR(N)
            INR = ECNR(N)
            IGR = ECGR(N)
            IST = ECST(N)
            OPR = ECOPR(N)
            IVIN = ECVIN(N)
            IECPT = ECTECP(N)
            FL_RG_25 = EPNFLRG(ECCR(N),ECLR(N),ECGR(N),ECAR(N))

            GEN_ALT = 0.0
            FUEL_ALT = 0.0

!     if (curiyr .le. 26 .and. isp .eq. 1 .and. curitr .eq. 1)then
!        if (ist .eq. 0)then
!          write(6,3456) curiyr+1989,irg,urgnme(irg),n,igrp,jgrp,ipgrp,icr,inr,igr,  &
!                    ist,fl_rg_25,ivin,ecapps(ipgrp,isp)
!3456 format(1h ,'!nostatedsp',i4,i3,a9,i6,i6,i6,i3,i3,i3,i3,i3,i3,i3,f10.1)
!        else
!          write(6,3457) curiyr+1989,irg,urgnme(irg),n,igrp,jgrp,ipgrp,icr,inr,igr,  &
!                    ist,ustnme(ist),fl_rg_25
!3457 format(1h ,'!pltinfo',i4,i3,a9,i6,i6,i6,i3,i3,i3,i3,i3,a3,i3)
!        end if
!     end if

            IF (JGRP .EQ. 0) THEN
               write(6,709) 'JGRP = 0',CURIYR,INR,N,IGRP,JGRP,IPGRP,IECPT
            ENDIF
  709       FORMAT(a10,7I8)
!
            IF (IPGRP .GT. 0) THEN

               STORAGE_RGN(IGRP) = IRG
               STORAGE_ECPn(IGRP) = IECPT
               STORAGE_ECPc(IGRP) = UPLNTCD(IECPT)
!
               IF (USW_ETT .EQ. 0) THEN
                  IF(ECDSPE(IGRP,ISP).LE.0.0) WRITE(*,*) 'NON POSTIVE SLICE '
                  IF(ECDSPE(IGRP,ISP) .LE. 0.0) GOTO 100 ! IF NO GEN IN HORIZ SLICE
               END IF
!
               IF (N .EQ. 0 .OR. IPGRP .EQ. 0 .OR. IFOWN .EQ. 0) THEN
                  WRITE(UF_ETT,710)'IRG',IRG,'ISP',ISP,'I',I,'N',N, &
                     'IPGRP',IPGRP,'IFOWN',IFOWN
  710             FORMAT(6(A4,1X,I4,1X),4(A4,1X,F8.2,1X))
               END IF
               
!               CALC ECCOPM FRAC BY  SEASON TO APPLY FOR RESTORE CAP
               PM_FRAC(IGRP,ISP) = 0.0
               IF (ECCAP(N,ISP) .GT. 0) then
                  PM_FRAC(IGRP,ISP) = ECCOPM(N,ISP)/ECCAP(N,ISP)
               ENDIF
!
!              ADD PLANT GROUP DATA TO RUNNING TOTALS
!
!              CAPACITY
!
               ECAPPS(IPGRP,ISP) = ECAPPS(IPGRP,ISP) + ECDSPC(IGRP,ISP)
       
               STORAGE_CAP(IGRP,ISP) = (ECCAP(N,ISP) - ECCOPM(N,ISP)) * 0.001 * (1.0 - UG_FOR(JN))   !send capacity net of PM, and adj for FOR
!
!              GENERATION
!
               GEN = ECDSPE(IGRP,ISP)
               GENR = GEN * .001

               STORAGE_GEN(IGRP,ISP) = GENR

               GEN_ALT = ECDSPE_ALT(IGRP,ISP)

               IF (ITYPE .EQ. 1) THEN
                  BGENOWN(IRG,IFOWN) = BGENOWN(IRG,IFOWN) + GEN * 0.001
               ELSE
                  BTRDOWN(IRG,IFOWN) = BTRDOWN(IRG,IFOWN) + GEN * 0.001
               END IF
               IF(IFOWN.LE.3)ETGEN = ETGEN + GEN
               EQPGN(IPGRP,IFOWN) = EQPGN(IPGRP,IFOWN) + GEN
               EQPGN_ALT(IPGRP,IFOWN) = EQPGN_ALT(IPGRP,IFOWN) + GEN_ALT
               IF (IECPT .LE. ECP_D_DSP .AND. IECPT .NE. WIA2 .AND. UPVTYP(IECPT) .LE. 0) THEN
                  IF (UPPCEF(IECPT) .GT. 0.0) THEN
                     EQPGNSQ = EQPGNSQ + GEN
                     EQPGNSQ_ALT = EQPGNSQ_ALT + GEN_ALT
                  END IF
               END IF
               IF(IFOWN.LE.3)EGENPS(IPGRP,ISP) = EGENPS(IPGRP,ISP) + GEN
               ULGENS(ISP,IGRP) = ULGENS(ISP,IGRP)  + GEN
               UDTLGNN(IECPT,IRG) = UDTLGNN(IECPT,IRG)  + GEN * 0.001
               
! track generation by coal/gas cofiring plants here based on fuel share(2) > 0
               IF (IECPT .EQ. WING .AND. ECFLTP(N,2) .GT. 0) THEN
                   UGENNGCF(IRG,CURIYR) = UGENNGCF(IRG,CURIYR) + GEN * 0.001
                   UGENNGCF(MNUMNR,CURIYR) = UGENNGCF(MNUMNR,CURIYR) + GEN * 0.001
               ENDIF
            if (isp .eq. 1 .and. curitr .eq. 1 .and. irg .eq. 1 .and. fl_rg_25 .eq. 1)  &
                 write(6,3333) curiyr+1989,n,igrp,jgrp,icr,inr,igr,ist,uplntcd(iecpt),ecdspc(igrp,isp)
 3333 format(1h ,'!badgen',i4,i6,i6,i6,i3,i3,i3,i3,a3,f10.1)
               IF (IST .GT. 0)THEN
!                 GENERATION BY NERC REGION AND STATE
                  EGEN_NRST(IECPT,IRG,IST) = EGEN_NRST(IECPT,IRG,IST) + GEN * 0.001
                  EGEN_NRST(IECPT,MNUMNR,IST) = EGEN_NRST(IECPT,MNUMNR,IST) + GEN * 0.001
                  EGEN_NRST(IECPT,IRG,EMM_D_ST + 1) = EGEN_NRST(IECPT,IRG,EMM_D_ST + 1) + GEN * 0.001
                  EGEN_NRST(IECPT,MNUMNR,EMM_D_ST + 1) = EGEN_NRST(IECPT,MNUMNR,EMM_D_ST + 1) + GEN * 0.001
                  EGEN_NRST(ECP_D_CAP + 1,IRG,IST) = EGEN_NRST(ECP_D_CAP + 1,IRG,IST) + GEN * 0.001
                  EGEN_NRST(ECP_D_CAP + 1,MNUMNR,IST) = EGEN_NRST(ECP_D_CAP + 1,MNUMNR,IST) + GEN * 0.001
                  EGEN_NRST(ECP_D_CAP + 1,IRG,EMM_D_ST + 1) = EGEN_NRST(ECP_D_CAP + 1,IRG,EMM_D_ST + 1) + GEN * 0.001
                  EGEN_NRST(ECP_D_CAP + 1,MNUMNR,EMM_D_ST + 1) = EGEN_NRST(ECP_D_CAP + 1,MNUMNR,EMM_D_ST + 1) + GEN * 0.001
!                 GENERATION BY NERC REGION AND FUEL REGION
                  EGEN_NRFR(IECPT,IRG,FL_RG_25) = EGEN_NRFR(IECPT,IRG,FL_RG_25) + GEN * 0.001
                  EGEN_NRFR(IECPT,MNUMNR,FL_RG_25) = EGEN_NRFR(IECPT,MNUMNR,FL_RG_25) + GEN * 0.001
                  EGEN_NRFR(IECPT,IRG,UNFRGN + 1) = EGEN_NRFR(IECPT,IRG,UNFRGN + 1) + GEN * 0.001
                  EGEN_NRFR(IECPT,MNUMNR,UNFRGN + 1) = EGEN_NRFR(IECPT,MNUMNR,UNFRGN + 1) + GEN * 0.001
                  EGEN_NRFR(ECP_D_CAP + 1,IRG,FL_RG_25) = EGEN_NRFR(ECP_D_CAP + 1,IRG,FL_RG_25) + GEN * 0.001
                  EGEN_NRFR(ECP_D_CAP + 1,MNUMNR,FL_RG_25) = EGEN_NRFR(ECP_D_CAP + 1,MNUMNR,FL_RG_25) + GEN * 0.001
                  EGEN_NRFR(ECP_D_CAP + 1,IRG,UNFRGN + 1) = EGEN_NRFR(ECP_D_CAP + 1,IRG,UNFRGN + 1) + GEN * 0.001
                  EGEN_NRFR(ECP_D_CAP + 1,MNUMNR,UNFRGN + 1) = EGEN_NRFR(ECP_D_CAP + 1,MNUMNR,UNFRGN + 1) + GEN * 0.001
!                 GENERATION BY FUEL REGION AND STATE
                  EGEN_FRST(IECPT,FL_RG_25,IST) = EGEN_FRST(IECPT,FL_RG_25,IST) + GEN * 0.001
                  EGEN_FRST(IECPT,UNFRGN + 1,IST) = EGEN_FRST(IECPT,UNFRGN + 1,IST) + GEN * 0.001
                  EGEN_FRST(IECPT,FL_RG_25,EMM_D_ST + 1) = EGEN_FRST(IECPT,FL_RG_25,EMM_D_ST + 1) + GEN * 0.001
                  EGEN_FRST(IECPT,UNFRGN + 1,EMM_D_ST + 1) = EGEN_FRST(IECPT,UNFRGN + 1,EMM_D_ST + 1) + GEN * 0.001
                  EGEN_FRST(ECP_D_CAP + 1,FL_RG_25,IST) = EGEN_FRST(ECP_D_CAP + 1,FL_RG_25,IST) + GEN * 0.001
                  EGEN_FRST(ECP_D_CAP + 1,UNFRGN + 1,IST) = EGEN_FRST(ECP_D_CAP + 1,UNFRGN + 1,IST) + GEN * 0.001
                  EGEN_FRST(ECP_D_CAP + 1,FL_RG_25,EMM_D_ST + 1) = EGEN_FRST(ECP_D_CAP + 1,FL_RG_25,EMM_D_ST + 1) + GEN * 0.001
                  EGEN_FRST(ECP_D_CAP + 1,UNFRGN + 1,EMM_D_ST + 1) = EGEN_FRST(ECP_D_CAP + 1,UNFRGN + 1,EMM_D_ST + 1) + GEN * 0.001
!                 GENERATION FROM AFFECTED (111d) PLANTS
                  IF (CO2_PLTSW(IECPT) .GT. 0.0)THEN
                     EGEN_NRST(ECP_D_CAP + 2,IRG,IST) = EGEN_NRST(ECP_D_CAP + 2,IRG,IST) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_NRST(ECP_D_CAP + 2,MNUMNR,IST) = EGEN_NRST(ECP_D_CAP + 2,MNUMNR,IST) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_NRST(ECP_D_CAP + 2,IRG,EMM_D_ST + 1) = EGEN_NRST(ECP_D_CAP + 2,IRG,EMM_D_ST + 1) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_NRST(ECP_D_CAP + 2,MNUMNR,EMM_D_ST + 1) = EGEN_NRST(ECP_D_CAP + 2,MNUMNR,EMM_D_ST + 1) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_NRFR(ECP_D_CAP + 2,IRG,FL_RG_25) = EGEN_NRFR(ECP_D_CAP + 2,IRG,FL_RG_25) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_NRFR(ECP_D_CAP + 2,MNUMNR,FL_RG_25) = EGEN_NRFR(ECP_D_CAP + 2,MNUMNR,FL_RG_25) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_NRFR(ECP_D_CAP + 2,IRG,UNFRGN + 1) = EGEN_NRFR(ECP_D_CAP + 2,IRG,UNFRGN + 1) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_NRFR(ECP_D_CAP + 2,MNUMNR,UNFRGN + 1) = EGEN_NRFR(ECP_D_CAP + 2,MNUMNR,UNFRGN + 1) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_FRST(ECP_D_CAP + 2,FL_RG_25,IST) = EGEN_FRST(ECP_D_CAP + 2,FL_RG_25,IST) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_FRST(ECP_D_CAP + 2,UNFRGN + 1,IST) = EGEN_FRST(ECP_D_CAP + 2,UNFRGN + 1,IST) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_FRST(ECP_D_CAP + 2,FL_RG_25,EMM_D_ST + 1) = EGEN_FRST(ECP_D_CAP + 2,FL_RG_25,EMM_D_ST + 1) + GEN * 0.001 * CO2_PLTSW(IECPT)
                     EGEN_FRST(ECP_D_CAP + 2,UNFRGN + 1,EMM_D_ST + 1) = EGEN_FRST(ECP_D_CAP + 2,UNFRGN + 1,EMM_D_ST + 1) + GEN * 0.001 * CO2_PLTSW(IECPT)
                  END IF
               END IF
!
!              SUM UP SO2 and NOX allowance costs for EFP
!
               ERTSO2(IFOWN) = ERTSO2(IFOWN) + GEN * 0.001 * ELSO2P(ISP,IGRP)
               ERTNOX(IFOWN) = ERTNOX(IFOWN) + GEN * 0.001 *  ELNOXP(ISP,IGRP)
               ERTHG(IFOWN) = ERTHG(IFOWN) + GEN * 0.001 *  ELHGP(ISP,IGRP)
               ACITOTB(IRG,CURIYR) = ACITOTB(IRG,CURIYR) + GEN * 0.001 * ELHGP(ISP,IGRP)   !regional mercury costs for adding to resource costs
!
!              Compute regional mapping ftab variables for generation
!
               IF (FCRL .EQ. 1) THEN
                  IF ( IFOWN .EQ. 1 .OR. IFOWN .EQ. 2) THEN
                     UGNINR(OPR,IYR) = UGNINR(OPR,IYR) + GENR
                     UGNINR(MNUMNR,IYR) = UGNINR(MNUMNR,IYR) + GENR

                     IF (INR .NE. OPR) THEN
                        UGNOTR(INR,IYR) = UGNOTR(INR,IYR) + GENR
                        UGNOTR(MNUMNR,IYR) = UGNOTR(MNUMNR,IYR) + GENR

                        UGNSRV(OPR,IYR) = UGNSRV(OPR,IYR) + GENR
                        UGNSRV(MNUMNR,IYR) = UGNSRV(MNUMNR,IYR) + GENR
                     ENDIF

                  ENDIF
               ENDIF                                   !Last iteration loop
!
!              ACCUM FUEL CONSUMPTION, FUEL COST, OM COST
!
!              IDENTIFY IF DUAL-FIRED PLANT (USES COMPETITIVE GAS)
!
               IDUAL = 0
               TOTGOIL = 0.0
               !DO IFP = 1 , EIFPLT
               !   IF (ECFLTP(N,IFP) .EQ. UIGC .AND. ECMFSH(N,IFP) .GT. 0.0) THEN
               !      IDUAL = 1
               !   END IF
               !END DO
!
!              LOOP OVER FUEL TYPES IN PLANT GROUP
!
               STORAGE_CST(IGRP,ISP) = 0.0
               DO 20 IFP = 1 , EIFPLT

!                 update fuel shares with new LP versions
!                 IF (FULLYR .GT. UESTYR .OR. CURITR .GT. 1) THEN

                  ECFSHR(N,IFP) = ELFLSH(ISP,IGRP,IFP)

!                 ENDIF

                  IF (ECFSHR(N,IFP) .GT. 0.0000000001) THEN
                     IFLTP = ECFLTP(N,IFP)
                     IFLRG = ECFLRG(N,IFP,1)
                     IF (IFLTP .EQ. UIRH) IFLTP = UIRL
                     IF ((IFLRG .LE. 0 .OR. IFLRG .GT. EFD_D_MFRG) .AND. NFLRG .EQ. 1) THEN
                        IFLRG = EFD_D_MFRG
                        PRINT *,' NO FUEL REGION ',N,IFP,NFLRG
                     ENDIF

                     IF (EC_45Q(N) .EQ. 0) THEN
                       IF (IECPT .EQ. WIBI) THEN
                        EOR_REV = (26 * 2.204 * 44/12) * ECSEQS(N) * 0.5 * 0.001 * CENS_VALUE(ICR,CURIYR)
                       ELSE
                        EOR_REV = UFRCO2(IFLTP,IFLRG) * ECSEQS(N) * 0.5 * 0.001 * CENS_VALUE(ICR,CURIYR)
                       ENDIF
                     ELSE
                       IF (IECPT .EQ. WIBI) THEN
                        EOR_REV = (26 * 2.204 * 44/12) * ECSEQS(N) * 0.5 * 0.001 * CENS_VAL_45Q(ICR,CURIYR)
                       ELSE
                        EOR_REV = UFRCO2(IFLTP,IFLRG) * ECSEQS(N) * 0.5 * 0.001 * CENS_VAL_45Q(ICR,CURIYR)
                       ENDIF
                     END IF

                     !                    STORAGE_CST(IGRP,ISP) = STORAGE_CST(IGRP,ISP) + ECFSHR(N,IFP) * 0.001 * &
!                       (ULHTRT_EFD(IGRP,ISP) * (UPFUEL(IFLTP,IFLRG) - EOR_REV + ACIFUEL - CFSUB - ECSEQS(N) * CRBPRC(IFLTP)) + &
!                        ECOMR(N,IFP) + ACIOM)

                     STORAGE_CST(IGRP,ISP) = STORAGE_CST(IGRP,ISP) + ECFSHR(N,IFP) * (0.001 * ULHTRT_EFD(IGRP,ISP) * &
                        (UPFUEL(IFLTP,IFLRG) - EOR_REV + ACIFUEL - CFSUB - ECSEQS(N) * CRBPRC(IFLTP)) + ECOMR(N,IFP) + ACIOM + ELRPSP(ISP,IGRP))

                      IF (FCRL .EQ. 1) THEN
                         WRITE(18,3881) CURIRUN, CURIYR+1989, CURITR, FCRL, IRG, ISP, IFP, IFLTP, N, JN, IGRP, IFLRG, IECPT, &
                             STORAGE_CAP(IGRP,ISP), STORAGE_CST(IGRP,ISP), ECFSHR(N,IFP), ULHTRT_EFD(IGRP,ISP), UPFUEL(IFLTP,IFLRG), EOR_REV, ACIFUEL, CFSUB, ECSEQS(N), CRBPRC(IFLTP), &
                             ECOMR(N,IFP), ACIOM, ECCAP(N,ISP) * 0.001, UG_FOR(JN), ECCOPM(N,ISP)*0.001, ELRPSP(ISP,IGRP)
 3881                   FORMAT(1X,"STORAGE_CST_UEFD",13(",",I6),16(",",F21.8))
                      END IF
                      STORAGE_CST(IGRP,ISP) = MAX(0.0,STORAGE_CST(IGRP,ISP))                      
                  END IF

                  IF (ECFSHR(N,IFP) .GT. 0.0000000001 .AND. GEN .GT. 0.000000001) THEN
                     IF (ECFLTP(N,IFP) .EQ. 0) write(UF_DBG,1068) N,IGRP,IFP,ECFSHR(N,IFP),ECFLTP(N,IFP)
 1068                format(1x,'ELCOST ECFSHR ',3I6,F10.5,I4)

                     IFLTP = ECFLTP(N,IFP)
                     IF (IFLTP .EQ. UIRH) IFLTP = UIRL

                     IF (IFLTP .EQ. UIDS) THEN
                        FUEL = ECDSPF(IGRP,ISP) * ECFSHR(N,IFP) * 1.100
                        FUEL_ALT = ECDSPF_ALT(IGRP,ISP) * ECFSHR(N,IFP) * 1.100
                     ELSE
                        FUEL = ECDSPF(IGRP,ISP) * ECFSHR(N,IFP)
                        FUEL_ALT = ECDSPF_ALT(IGRP,ISP) * ECFSHR(N,IFP)
                     END IF

                     IF (IFLTP .EQ. UIGC) THEN 
                        IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
                              WRITE(unit_num_ephrts_debug_file,*) "Hydrogen from EFD, Year ", CURIYR+1989
                              WRITE(unit_num_ephrts_debug_file,*) "Input Fuel Consumed (molecular h2 consumed in turbine) : ", Fuel
                              WRITE(unit_num_ephrts_debug_file,*) "Output Generation (electricty from turbines): ", Gen
                              WRITE(unit_num_ephrts_debug_file,*) "REGION  : ", irg
                              WRITE(unit_num_ephrts_debug_file,*) "SEASON  : ", isp
                        END IF                        
                        WRITE(18,7317) CURIRUN, CURITR, CURIYR+1989, IYR+1989, ITYPE, ISP, IRG, IGRP, IECPT, N, IFP, IFLTP, UIDS, FUEL, ECDSPF(IGRP,ISP) * ECFSHR(N,IFP), ECFSHR(N,IFP), GEN, ULHTRT_EFD(IGRP,ISP)
7317                   FORMAT(1X,"ELCOST_H2",12(":",I5),6(":",F21.6))
                       
                        H2_TURBINE_CONSUMPTION(CURIYR+1989, ISP,IRG) = H2_TURBINE_CONSUMPTION(CURIYR+1989, ISP,IRG) + FUEL ! fuel units is billion btus
                        H2_TURBINE_GENERATION(CURIYR+1989, ISP,IRG) = H2_TURBINE_GENERATION(CURIYR+1989, ISP,IRG) + GEN ! genation units is GWHrs

                     END IF
                     
                     IFLRG = ECFLRG(N,IFP,1)
                     IF (ECSEQS(N) .GT. 0.0 .AND. ECFSHR(N,IFP) .GT. 0.0) THEN
                        IF (EC_45Q(N) .EQ. 0) THEN
                           I_45Q = 1
                        ELSE
                           I_45Q = 2
                        END IF
                        IF (UPTTYP(IECPT) .LE. NW_COAL) THEN
                           IF (UPVTYP(IECPT) .EQ. 1) THEN
                              CTYPE = 1
                           ELSE
                              CTYPE = 2
                           END IF
                        ELSEIF (IECPT .LE. ECP_D_DSP) THEN !gas
                           IF (UPVTYP(IECPT) .EQ. 1) THEN
                              CTYPE = 3
                           ELSE
                              CTYPE = 4
                           END IF
                        ELSE
                            CTYPE = 5
                        END IF

                      IF (IFP .EQ. 1 .AND. FUEL .GT. 0.0) THEN       !add to annual investment and FOM costs if generating 
                        IF (UPPCEF_MIN(IECPT) .GT. 0 .OR. FUEL - FUEL_ALT .GT. 0.0) THEN ! exclude if only operating in 'ALT' model with 0 capture
                          CST_EMM_INV(CTYPE,ICR,CURIYR) = CST_EMM_INV(CTYPE,ICR,CURIYR) + ULCCS_INV(IGRP)/MC_JPGDP(CURIYR)  ! million 1987$
                          CST_EMM_OM(CTYPE,ICR,CURIYR) = CST_EMM_OM(CTYPE,ICR,CURIYR) + ULCCS_FOM(IGRP)           ! million 1987$
                         ENDIF
                      ENDIF
                      IF (FUEL .GT. 0.0) THEN
                         IF (UPPCEF_MIN(IECPT) .GT. 0 .OR. FUEL - FUEL_ALT .GT. 0.0) THEN
                             CST_EMM_OM(CTYPE,ICR,CURIYR) = CST_EMM_OM(CTYPE,ICR,CURIYR) + ULCCS_VOM(IGRP) * GEN * ECFSHR(N,IFP) * .001   !million 1987$
                         ENDIF
                      ENDIF
                      IF (EC_45Q(N) .EQ. 0) THEN ! fill CCATS variables without 45Q credit
                        IF (CTYPE .LT. 5) THEN          !not BECCS - use UFRCO2
                          SUP_EMM_NTC(CTYPE,ICR,CURIYR) = SUP_EMM_NTC(CTYPE,ICR,CURIYR) + ((ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + &
                                                                      FUEL_ALT * UPPCEF_MIN(IECPT))) * 1000.0/2204.0)      !metric ton CO2
                          SUP_EMM_NTC(CTYPE,MNUMCR,CURIYR) = SUP_EMM_NTC(CTYPE,MNUMCR,CURIYR) + ((ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + &
                                                                      FUEL_ALT * UPPCEF_MIN(IECPT))) * 1000.0/2204.0)      !metric ton CO2
                        ELSE            !BECCS - need to use hardwired value for now
                          SUP_EMM_NTC(CTYPE,ICR,CURIYR) = SUP_EMM_NTC(CTYPE,ICR,CURIYR) + (ECFSHR(N,IFP) * (26 * 44/12) * FUEL * ECSEQS(N) )    ! 26 is in metric ton carbon         
                          SUP_EMM_NTC(CTYPE,MNUMCR,CURIYR) = SUP_EMM_NTC(CTYPE,MNUMCR,CURIYR) + (ECFSHR(N,IFP) * (26 * 44/12) * FUEL * ECSEQS(N) )    ! 26 is in metric ton carbon         
                        ENDIF
                      ELSE      !fill CCATS variables with 45Q credit
                        IF (CTYPE .LT. 5) THEN          !not BECCS - use UFRCO2
                          SUP_EMM_45Q(CTYPE,ICR,CURIYR) = SUP_EMM_45Q(CTYPE,ICR,CURIYR) + ((ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + & 
                                                                      FUEL_ALT * UPPCEF_MIN(IECPT))) * 1000.0/2204.0)  !metric ton CO2
                          SUP_EMM_45Q(CTYPE,MNUMCR,CURIYR) = SUP_EMM_45Q(CTYPE,MNUMCR,CURIYR) + ((ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + & 
                                                                      FUEL_ALT * UPPCEF_MIN(IECPT))) * 1000.0/2204.0)  !metric ton CO2
                        ELSE            !BECCS - need to use hardwired value for now
                          SUP_EMM_45Q(CTYPE,ICR,CURIYR) = SUP_EMM_45Q(CTYPE,ICR,CURIYR) + (ECFSHR(N,IFP) * (26 * 44/12) * FUEL * ECSEQS(N) )   ! 26 is in metric ton carbon     
                          SUP_EMM_45Q(CTYPE,MNUMCR,CURIYR) = SUP_EMM_45Q(CTYPE,MNUMCR,CURIYR) + (ECFSHR(N,IFP) * (26 * 44/12) * FUEL * ECSEQS(N) )   ! 26 is in metric ton carbon     
                        ENDIF
                      ENDIF
                        
                        WRITE(18,2316) CURIRUN, CURCALYR, CURITR, N, ISP, IRG, IECPT, IFP, IFLTP, IFLRG, ICR, I_45Q, CTYPE, &
                           (ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + FUEL_ALT * UPPCEF_MIN(IECPT))*(1.0/2.204)), &
                           ECFSHR(n,IFP), UFRCO2(IFLTP,IFLRG), FUEL, (FUEL - FUEL_ALT), FUEL_ALT, ECSEQS(N), UPPCEF_MIN(IECPT)
 2316                   FORMAT(1X,"CCATS_CCS_INFO",13(",",I5),8(",",F21.6))
                        
                        
                      IF (CTYPE .LT. 5) THEN !skip for now, these variables won't account for BECCS yet, need to decide how to report
                        BTU_CCS(CTYPE,FL_RG_25,I_45Q,CURIYR) = BTU_CCS(CTYPE,FL_RG_25,I_45Q,CURIYR) + (ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + FUEL_ALT * UPPCEF_MIN(IECPT)))
                        BTU_CCS(0,FL_RG_25,I_45Q,CURIYR) = BTU_CCS(0,FL_RG_25,I_45Q,CURIYR) + (ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + FUEL_ALT * UPPCEF_MIN(IECPT)))
                        BTU_CCS(CTYPE,0,I_45Q,CURIYR) = BTU_CCS(CTYPE,0,I_45Q,CURIYR) + (ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + FUEL_ALT * UPPCEF_MIN(IECPT)))
                        BTU_CCS(CTYPE,FL_RG_25,0,CURIYR) = BTU_CCS(CTYPE,FL_RG_25,0,CURIYR) + (ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + FUEL_ALT * UPPCEF_MIN(IECPT)))
                        BTU_CCS(0,0,I_45Q,CURIYR) = BTU_CCS(0,0,I_45Q,CURIYR) + (ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + FUEL_ALT * UPPCEF_MIN(IECPT)))
                        BTU_CCS(0,FL_RG_25,0,CURIYR) = BTU_CCS(0,FL_RG_25,0,CURIYR) + (ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + FUEL_ALT * UPPCEF_MIN(IECPT)))
                        BTU_CCS(CTYPE,0,0,CURIYR) = BTU_CCS(CTYPE,0,0,CURIYR) + (ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + FUEL_ALT * UPPCEF_MIN(IECPT)))
                        BTU_CCS(0,0,0,CURIYR) = BTU_CCS(0,0,0,CURIYR) + (ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + FUEL_ALT * UPPCEF_MIN(IECPT)))

                        WRITE(18,2317) CURIRUN, CURCALYR, CURITR, N, ISP, IRG, IECPT, IFP, IFLTP, IFLRG, FL_RG_25, I_45Q, CTYPE, &
                           (ECFSHR(N,IFP) * UFRCO2(IFLTP,IFLRG) * ((FUEL - FUEL_ALT) * ECSEQS(N) + FUEL_ALT * UPPCEF_MIN(IECPT))), &
                           ECFSHR(n,IFP), UFRCO2(IFLTP,IFLRG), FUEL, (FUEL - FUEL_ALT), FUEL_ALT, ECSEQS(N), UPPCEF_MIN(IECPT)
 2317                   FORMAT(1X,"BTU_CCS_INFO",13(",",I5),8(",",F21.6))
                      ENDIF
                     END IF

!                    IF(IFOWN.LE.3)EQPFL(IPGRP) = EQPFL(IPGRP) + FUEL

                     IF(IFOWN.LE.4)EQPFL(IPGRP) = EQPFL(IPGRP) + FUEL
                     EQFFL(IFLTP,IFOWN) = EQFFL(IFLTP,IFOWN) + FUEL
                     EQPFLCN(IPGRP,IFOWN,IFLTP) = EQPFLCN(IPGRP,IFOWN,IFLTP) + FUEL
                     EQPFLGN(IPGRP,IFOWN,IFLTP) = EQPFLGN(IPGRP,IFOWN,IFLTP) + (GEN * ECFSHR(N,IFP))

!                    ACCOUNT FOR SUBSIDY, IF ANY, FOR BIOMASS COFIRING (CONVERT FROM MILLS)

                     IF (IPGRP .NE. UIBMS .AND. IFLTP .EQ. UIWD)THEN
                        CFSUB = WDCFSUB(CURIYR) / (ULHTRT_EFD(IGRP,ISP) * 0.001)
                     ELSE
                        CFSUB = 0.0
                     END IF
!
!                    Compute regional mapping ftab variables for consumption(TABLE 62)
!                    (exclude nuclear  ifltp=uiuf)
!
                     FUELR = FUEL * .000001
                     IF ((IFOWN .EQ. 1 .OR. IFOWN .EQ. 2) .AND. FCRL .EQ. 1 .AND. IFLTP .NE. UIUF) THEN
                        UFLINR(OPR,IYR) = UFLINR(OPR,IYR) + FUELR
                        UFLINR(MNUMNR,IYR) = UFLINR(MNUMNR,IYR) + FUELR
                        IF (INR .NE. OPR) THEN
                           UFLOTR(INR,IYR) = UFLOTR(INR,IYR) + FUELR
                           UFLOTR(MNUMNR,IYR) = UFLOTR(MNUMNR,IYR) + FUELR
                           UFLSRV(OPR,IYR) = UFLSRV(OPR,IYR) + FUELR
                           UFLSRV(MNUMNR,IYR) = UFLSRV(MNUMNR,IYR) + FUELR
                        ENDIF
                     ENDIF
!
                     DO NFLRG = 1 , UNFLRG(IFLTP)
                        IFLRG = ECFLRG(N,IFP,NFLRG)
                        IF ((IFLRG .LE. 0 .OR. IFLRG .GT. EFD_D_MFRG) .AND. NFLRG .EQ. 1) THEN
                           IFLRG = EFD_D_MFRG
                           PRINT *,' NO FUEL REGION ',N,IFP,NFLRG
                        ENDIF
                        IF (IFOWN .LE. 4) THEN                           ! add nt cons for aeo03
                           IF (IFLTP .GT. EFD_D_NFL .OR. IFLRG .GT. EFD_D_MFRG .OR. NFLRG .GT. EFD_D_OWN) THEN
                              write(6,111) 'UQFUEL indice out of bounds ',CURIYR,CURITR,N,IFP,IPGRP,IECPT,IFLTP,IFLRG,NFLRG
  111                         format(1x,A25,9I6)
                           ENDIF
                           IF (IFLTP .EQ. 0 .OR. IFLRG .EQ. 0 .OR. NFLRG .EQ. 0) THEN
                              write(6,111) 'UQFUEL indice out of bounds ',CURIYR,CURITR,N,IFP,IPGRP,IECPT,IFLTP,IFLRG,NFLRG
                           ENDIF
                           UQFUEL(IFLTP,IFLRG,NFLRG) = UQFUEL(IFLTP,IFLRG,NFLRG) + FUEL
                           IF(NFLRG .EQ. 1)SQFUEL(IFLTP,IFLRG,ISP) = SQFUEL(IFLTP,IFLRG,ISP) + FUEL
                        END IF
                     END DO
!
                     IFLRG = ECFLRG(N,IFP,1)
!
!                    Capture Coal Consumption by ECP Type and Coal Demand Region
!
                     IF ((UPTTYP(IECPT) .LE. NW_COAL .AND. IFLRG .LE. NDREG .AND. IFLTP .LE. UIIS) .OR. (IECPT .EQ. WING .AND. IFLTP .NE. UIGF)) THEN
                        ICL = MAP_TO_COAL_ID(JGRP)
                        IF (ICL .LE. 0) THEN
                           ICL = NUM_CMM_UNITS + 1
                           EMM_CL_ECPT(ICL,CURIYR) = IECPT
                        END IF
                        JECPT = EMM_CL_ECPT(ICL,CURIYR)
                        ISCRB = ECP_SCRUB(JECPT,CURIYR)
                        IF (JECPT .LE. 0 .OR. JECPT .GT. UIIS) THEN
                           WRITE(6,2151) CURIYR+UHBSYR,CURITR,ISP,JGRP,IGRP,ICL,IECPt,JECPT,IFLRG,IFLTP
 2151                      FORMAT(1X,"EMM_CL_ECPT_OOPS",10(":",I5))
                           JECPT = IECPt
                           EMM_CL_ECPT(ICL,CURIYR) = IECPt
                        END IF
                        UQCOAL(JECPT,IFLRG,CURIYR) = UQCOAL(JECPT,IFLRG,CURIYR) + FUEL * 0.001
                        IF (EMM_CL_ECPT(ICL,CURIYR) .NE. IECPT) THEN
                           WRITE(18,2391) CURIYR+UHBSYR,CURITR,ICL,IECPt,EMM_CL_ECPt(ICL,CURIYR),FUEL*0.001
 2391                      FORMAT(1X,"EMM_CL_ECPt_OOPS",5(":",I4),":",F12.3)
                        END IF
                        EMM_CL_BTUs(ICL,CURIYR) = EMM_CL_BTUs(ICL,CURIYR) + FUEL * 0.001
                     END IF
!
!                    CALCULATE ADDERS FOR ACTIVATED CARBON INJECTION - FUEL AND O&M
!
                     ACIFUEL = 0.0
                     ACIOM = 0.0
!
!                    SUMPRODUCT OF FUEL CONSUMPTION and SO2 REMOVAL RATE
!
                     IF (IFOWN.LE.4) THEN
                        EPFLRG(IFLTP,IFLRG) = EPFLRG(IFLTP,IFLRG) + FUEL
                        EPCRMP(IFLTP,ICR) = EPCRMP(IFLTP,ICR) + FUEL
                        IF (IECPT .LE. ECP_D_DSP)THEN
                           ITYP = UPTTYP(IECPt)
                           IECP = UFL_D_ECP(IFLTP,1)
                           J = 1
                           DO WHILE (IECP .GT. 0)
                              IF (TST_ECP_FUEL(ITYP,IECP) .EQ. 1) THEN
                                 EPFMAP(IECP,IFLTP) = EPFMAP(IECP,IFLTP) + FUEL
                              END IF
                              J = J + 1
                              IF (J .LE. ECP_D_FPP) THEN
                                 IECP = UFL_D_ECP(IFLTP,J)
                              ELSE
                                 IECP = 0
                              END IF
                           END DO
!
!                          Computation for Determining average fuel prices by EMM Region
!
                           !TODO: FIGURE OUT IF UIGAS STILL CONTAINS GC OR NOT. THEN ADD NEW VARIABLE, AND REPLICATE CODE TO END UP IN FTAB
                           IF (UIGAS(IFLTP) .EQ. 1) THEN
                              TPGASPRC(IRG) = TPGASPRC(IRG) + FUEL * (UPFUEL(IFLTP,IFLRG) - CRBCST(IFLTP))
                              TPGASWGT(IRG) = TPGASWGT(IRG) + FUEL
                           ELSE IF (UIGAS(IFLTP) .EQ. 2) THEN ! hydrogen uses slot 2 in UIGAS
                              TPHYDPRC(IRG) = TPHYDPRC(IRG) + FUEL * (UPFUEL(IFLTP,IFLRG) - CRBCST(IFLTP))
                              TPHYDWGT(IRG) = TPHYDWGT(IRG) + FUEL
                           ELSE IF (UIDIS(IFLTP) .EQ. 1) THEN
                              TPDISPRC(IRG) = TPDISPRC(IRG) + FUEL * (UPFUEL(IFLTP,IFLRG) - CRBCST(IFLTP))
                              TPDISWGT(IRG) = TPDISWGT(IRG) + FUEL
                           ELSE IF (UIRES(IFLTP) .EQ. 1) THEN
                              TPRESPRC(IRG) = TPRESPRC(IRG) + FUEL * (UPFUEL(IFLTP,IFLRG) - CRBCST(IFLTP))
                              TPRESWGT(IRG) = TPRESWGT(IRG) + FUEL
                           ELSE IF (UICOL(IFLTP) .EQ. 1) THEN
                              TPCOLPRC(IRG) = TPCOLPRC(IRG) + FUEL * (UPFUEL(IFLTP,IFLRG) - CRBCST(IFLTP))
                              TPCOLPRA(IRG) = TPCOLPRA(IRG) + FUEL * (PCLELCDR(ISCRB,IFLRG,CURIYR) - CRBCST(IFLTP))
                              TPCOLWGT(IRG) = TPCOLWGT(IRG) + FUEL
                           ELSE IF (UINUC(IFLTP) .EQ. 1) THEN
                              TPNUCPRC(IRG) = TPNUCPRC(IRG) + FUEL * (UPFUEL(IFLTP,IFLRG) - CRBCST(IFLTP))
                              TPNUCWGT(IRG) = TPNUCWGT(IRG) + FUEL
                           END IF
                        END IF
                     END IF

                     IF (IFLTP .ne. ULFUEL(IFP,IGRP)) THEN
                        write(*,*)'fuel mismatch ifltp,ulfuel,igrp',ifltp, ulfuel(ifp,igrp)     ,igrp
                     ENDIF

                     UQFGENN(IFLTP,INR,IFOWN) = UQFGENN(IFLTP,INR,IFOWN) + GEN * ECFSHR(N,IFP)
                     UQFCONN(IFLTP,INR,IFOWN) = UQFCONN(IFLTP,INR,IFOWN) + FUEL
                     UQPGENN(IPGRP,INR,IFOWN) = UQPGENN(IPGRP,INR,IFOWN) + GEN * ECFSHR(N,IFP)

!                    IF(IFOWN.LE.3)THEN

                     IF (IFOWN.LE.4)THEN
                        UQFGENC(IFLTP,ICR) = UQFGENC(IFLTP,ICR) + GEN * ECFSHR(N,IFP)
                        UQPGENC(IPGRP,ICR) = UQPGENC(IPGRP,ICR) + GEN * ECFSHR(N,IFP)
 !                       IF (CURIYR.EQ.27)WRITE(6,4924)'uqfgenc1 ', CURIYR,IFLTP,ICR,N,IFP,UQFGENC(IFLTP,ICR),  GEN, ECFSHR(N,IFP)
   4924   Format(A25,1x,5(I4,1x),3(F12.3,1x))
 
                     END IF

                     ! THIS CODE IS POPULATING VAIRALBES IN A GENERAL MANNER WITHOUT REGARD FOR TECHNOLOGY TYPE
                     EQFGN(IFLTP,IFOWN) = EQFGN(IFLTP,IFOWN) + GEN * ECFSHR(N,IFP)
                     ERTFL(IFOWN) = ERTFL(IFOWN) + &
                        (FUEL -FUEL_ALT) * (UPFUEL(IFLTP,IFLRG) - EOR_REV + ACIFUEL - CRBCST(IFLTP) - CFSUB - ECSEQS(N) * CRBPRC(IFLTP)) * 0.001
                     ERTFL(IFOWN) = ERTFL(IFOWN) + &
                        FUEL_ALT * (UPFUEL(IFLTP,IFLRG) - EOR_REV + ACIFUEL - CRBCST(IFLTP) - CFSUB - UPPCEF_MIN(IECPT) * CRBPRC(IFLTP)) * 0.001                        
                     ERTOM(IFOWN) = ERTOM(IFOWN) + GEN * ECFSHR(N,IFP) * (ECOMR(N,IFP) + ACIOM) * 0.001
                     ERPOM(IPGRP,IFOWN) = ERPOM(IPGRP,IFOWN) + GEN * ECFSHR(N,IFP) * (ECOMR(N,IFP) + ACIOM) * 0.001

                     ULREVS(IGRP) = ULREVS(IGRP) + FUEL * EOR_REV * 0.001

!                    PTC

                     IF (ECGSUB(N) .GT. 0.0)THEN
                       IF (IECPT .EQ. WICN) THEN       !exis nuclear can have different levels of PTC but all gen should count                  
                        UGNPTCN(IPGRP,INR) = UGNPTCN(IPGRP,INR) + GEN * ECFSHR(N,IFP) * 0.001 
                       ELSE IF (UPGSUBPT(IECPT) .GT. 0) THEN         !use annual PTC variable - no, needs to be by start year which is not same for entire group, just count all
!                        UGNPTCN(IPGRP,INR) = UGNPTCN(IPGRP,INR) + GEN * ECFSHR(N,IFP) * 0.001 * ECGSUB(N) / UPGSUBYR(IECPT,CURIYR)
                        UGNPTCN(IPGRP,INR) = UGNPTCN(IPGRP,INR) + GEN * ECFSHR(N,IFP) * 0.001                         
                       ELSE
                        UGNPTCN(IPGRP,INR) = UGNPTCN(IPGRP,INR) + GEN * ECFSHR(N,IFP) * 0.001 * ECGSUB(N) / UPGSUB(IECPT)
                       ENDIF
                        UPYPTCN(IPGRP,INR) = UPYPTCN(IPGRP,INR) + GEN * ECFSHR(N,IFP) * ECGSUB(N) * 0.001

!                       for nuclear, need to limit subsidy to $125 million per gw (in 2003$), adjust for tax
!                       IF (IECPT .eq. WICN .OR. IECPT .eq. WIAN .OR. IECPT .eq. WISM) then
!                       tmpsub = GEN * ECFSHR(N,IFP) * ECGSUB(N) * 0.001
!                       nuclim = 125.0 / MC_JPGDP(14)
!                       ERTGSUB(IFOWN) = ERTGSUB(IFOWN) + MIN(tmpsub,nuclim * ECCAP(N,ISP) * MC_JPGDP(14)/MC_JPGDP(CURIYR))
!                       ELSE

                        ERTGSUB(IFOWN) = ERTGSUB(IFOWN) + GEN * ECFSHR(N,IFP) * ECGSUB(N) * (1.0 - UPTXRT) * 0.001

!                       if (curitr .gt. maxitr .and. ipgrp .eq. uianc) &
!                       write(6,1234) curiyr+1989,inr,curitr,GEN * ECFSHR(N,IFP) , ECGSUB(N) * (1.0 - UPTXRT), ugnptcn(ipgrp,inr),upyptcn(ipgrp,inr)
!1234                   format(1h ,'!nucptc',i4,i3,i3,4f10.3)
!                       ENDIF

                     END IF
                     TEMP = UPFUEL(IFLTP,IFLRG)
                     ERPFL(IPGRP,IFOWN) = ERPFL(IPGRP,IFOWN) + &
                        (FUEL-FUEL_ALT) * (UPFUEL(IFLTP,IFLRG) - EOR_REV + ACIFUEL - CRBCST(IFLTP) - CFSUB -  ECSEQS(N) * CRBPRC(IFLTP)) * 0.001
                     ERPFL(IPGRP,IFOWN) = ERPFL(IPGRP,IFOWN) + &
                        FUEL_ALT * (UPFUEL(IFLTP,IFLRG) - EOR_REV + ACIFUEL - CRBCST(IFLTP) - CFSUB -  UPPCEF_MIN(IECPT) * CRBPRC(IFLTP)) * 0.001

                     ULFLCST(IFP,IGRP) =  ULFLCST(IFP,IGRP) + &
                        (FUEL-FUEL_ALT) * (UPFUEL(IFLTP,IFLRG) - EOR_REV + ACIFUEL - CRBCST(IFLTP) - CFSUB -  ECSEQS(N) * CRBPRC(IFLTP)) * 0.001
                     ULFLCST(IFP,IGRP) =  ULFLCST(IFP,IGRP) + &
                        FUEL_ALT * (UPFUEL(IFLTP,IFLRG) - EOR_REV + ACIFUEL - CRBCST(IFLTP) - CFSUB -  UPPCEF_MIN(IECPT) * CRBPRC(IFLTP)) * 0.001                        

                     IF (IFOWN .LE. 4) THEN
                        IF (FUEL .LT. 0.0 .OR. UPFUEL(IFLTP,IFLRG) .LT. 0.0) THEN
                           write(6,1010) CURIYR,CURITR,IGRP,IPGRP,IFLTP,IFLRG,UPFUEL(IFLTP,IFLRG),FUEL
 1010                      format(1x,'UPFUEL err ' ,6I7,2F10.2)
                        ENDIF
                        ERFFL(IFLTP) = ERFFL(IFLTP) + &
                           (FUEL-FUEL_ALT) * (UPFUEL(IFLTP,IFLRG) - EOR_REV + ACIFUEL - CRBCST(IFLTP) - CFSUB - ECSEQS(N) * CRBPRC(IFLTP)) * 0.001
                        ERFFL(IFLTP) = ERFFL(IFLTP) + &
                           FUEL_ALT * (UPFUEL(IFLTP,IFLRG) - EOR_REV + ACIFUEL - CRBCST(IFLTP) - CFSUB - UPPCEF_MIN(IECPT) * CRBPRC(IFLTP)) * 0.001
                        ERFFC(IFLTP) = ERFFC(IFLTP) + FUEL * UPFUEL(IFLTP,IFLRG)

!
!                       ACCUMULATE BIOMASS FUEL REVENUES AND CONSUMPTION BY CENSUS TO GET PRICES
!
                        IF (IFLTP .EQ. UIWD) THEN
                           UQBIOM(ICR,IFOWN) = UQBIOM(ICR,IFOWN) + FUEL
                           URBIOM(ICR,IFOWN) = URBIOM(ICR,IFOWN) + FUEL * UPFUEL(IFLTP,IFLRG)
                        END IF
                     END IF
!
!                    EMISSIONS ACCOUNTING
!
                     IF (IFOWN .LE. 4) THEN

!                       CAPTURED CO2 Info

                        IF (ECSEQS(N) .GT. 0.0 .AND. FCRL .EQ. 1) THEN
                           IF (FL_RG_25 .GT. 0) THEN
                              WRITE(18,4972) CURIRUN, CURIYR+1989, CURITR, IRG, IYR+1989, ITYPE, ISP, N, IPGRP, IGRP, IECPT, FL_RG_25, IFLRG, IFP, IFLTP, &
                                 ECSEQS(N), GEN, FUEL, UFRCO2(IFLTP,IFLRG), UPFUEL(IFLTP,IFLRG), ACIFUEL, CRBCST(IFLTP), CFSUB, CRBPRC(IFLTP), &
                                 CENS_VALUE(ICR,CURIYR), EOR_REV
 4972                         FORMAT(1X,"UEFD_CAPTURED_CO2",15(":",I5),11(":",F20.6))
                           ELSE
                              WRITE(18,3972) CURIRUN, CURIYR+1989, CURITR, IRG, IYR+1989, ITYPE, ISP, N, IPGRP, IGRP, IECPT, FL_RG_25,  &
                                 ECCR(N), ECLR(N), ECGR(N), ECAR(N)
 3972                         FORMAT(1X,"UEFD_NO_FL_RG",16(":",I5))
                           END IF
                        END IF
!
!                       SUM PRODUCT OF FUEL CONSUMPTION AND SO2 REMOVAL RATES
!
                        KSCRB = ECSCRB(N)
                        URFUEL(IFLTP,IFLRG) = URFUEL(IFLTP,IFLRG) + FUEL * REAL(KSCRB) * EFACTR
                        UXFUEL(IFLTP,IFLRG) = UXFUEL(IFLTP,IFLRG) + (FUEL - FUEL_ALT) * ECSEQS(N)
                        UXFUEL(IFLTP,IFLRG) = UXFUEL(IFLTP,IFLRG) + (FUEL_ALT) * UPPCEF_MIN(IECPT)

!
!                       DETERMINE NOX EMISSION RATE
!
                        NOXFAC = ECFNOX(N,ISP)
!
!                       DETERMINE HG EMISSION AND REMOVAL RATE
!
                        HGFAC = UFRHG(IFLTP,IFLRG)
!
!                       1. BY EFD DISPATCHABLE CAPACITY TYPE
!
                        EQPSO2(IPGRP) = EQPSO2(IPGRP) + FUEL * UFRSO2R(IFLTP,IFLRG) * (1.0 - UFRASH(IFLTP,IFLRG)) * (1.0 - REAL(KSCRB) * EFACTR) * 0.5
                        EQPNOX(IPGRP) = EQPNOX(IPGRP) + FUEL * NOXFAC * 0.5
                        EQPCO2(IPGRP) = EQPCO2(IPGRP) + (FUEL - FUEL_ALT) * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        EQPCO2(IPGRP) = EQPCO2(IPGRP) + FUEL_ALT * UFRCO2(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5                       
                        EQPHG(IPGRP) = EQPHG(IPGRP) + FUEL * HGFAC * 0.5 * 0.000001
!
!                       2. BY FUEL TYPE
!
                        EQFSO2(IFLTP) = EQFSO2(IFLTP) + FUEL * UFRSO2R(IFLTP,IFLRG) * (1.0 - UFRASH(IFLTP,IFLRG)) * (1.0 - REAL(KSCRB) * EFACTR) * 0.5
                        UQFSO2(IFLTP,IRG) = UQFSO2(IFLTP,IRG) + FUEL * UFRSO2R(IFLTP,IFLRG) * 0.5
                        EQFNOX(IFLTP) = EQFNOX(IFLTP) + FUEL * NOXFAC * 0.5
                        EQFCO2(IFLTP) = EQFCO2(IFLTP) + (FUEL-FUEL_ALT) * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        EQFCO2(IFLTP) = EQFCO2(IFLTP) + FUEL_ALT * UFRCO2(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5                        
                        EQFHG(IFLTP) = EQFHG(IFLTP) + FUEL * HGFAC * 0.5 * 0.000001
                        EFHCNT(IFLTP) = EFHCNT(IFLTP) + FUEL * UFHCNT(IFLTP,IFLRG)
!
!                       3. IN TOTAL
!
                        ETSO2 = ETSO2 + FUEL * UFRSO2R(IFLTP,IFLRG) * (1.0 - UFRASH(IFLTP,IFLRG)) * (1.0 - REAL(KSCRB) * EFACTR) * 0.5
                        ETNOX = ETNOX + FUEL * NOXFAC * 0.5
                        ETCO2 = ETCO2 + (FUEL-FUEL_ALT) * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        ETCO2 = ETCO2 + FUEL_ALT * UFRCO2(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5
                        ETCAR = ETCAR + (FUEL-FUEL_ALT) * UFRCAR(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        ETCAR = ETCAR + FUEL_ALT * UFRCAR(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5
                        ETCO1 = ETCO1 + (FUEL-FUEL_ALT) * UFRCO1(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        ETCO1 = ETCO1 + FUEL_ALT * UFRCO1(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5
                        ETVOC = ETVOC + FUEL * UFRVOC(IFLTP) * 0.5
                        ETHG = ETHG + FUEL * HGFAC * 0.5 * 0.000001
!
!                       3.5 BY REGIONAL MAPPING FTAB VARIABLES FOR TABLE 62
!
!                       EMISSIONS TOTALS ARE COMPUTED IN EMMDSPO AFTER THE
!                       RENEWABLES ARE ADDED TO THE INR VARIABLES
!                       FACTE= CONVERSION FACTOR FROM SHORT TONS TO MILLION METRIC TONS

                        IF (INR .EQ. OPR) THEN
                           USO2INR(OPR,IYR) = USO2INR(OPR,IYR) + &
                              ( FUEL * UFRSO2R(IFLTP,IFLRG) * (1.0 - UFRASH(IFLTP,IFLRG)) * (1.0 - REAL(KSCRB) * EFACTR) * 0.5 )* FACTE
                           UCO2INR(OPR,IYR) = UCO2INR(OPR,IYR) + ( (FUEL-FUEL_ALT) * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5 ) * FACTE
                           UCO2INR(OPR,IYR) = UCO2INR(OPR,IYR) + ( FUEL_ALT * UFRCO2(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5 ) * FACTE
                           UCARINR(OPR,IYR) = UCARINR(OPR,IYR) + ( (FUEL-FUEL_ALT) * UFRCAR(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5 ) * FACTE
                           UCARINR(OPR,IYR) = UCARINR(OPR,IYR) + ( FUEL_ALT * UFRCAR(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5 ) * FACTE
                           UNOXINR(OPR,IYR) = UNOXINR(OPR,IYR) + ( FUEL * NOXFAC * 0.5 ) / 1000000.0
                           UHGINR(OPR,IYR) = UHGINR(OPR,IYR) + ( FUEL * HGFAC * 0.5 )
                        ENDIF
                        IF (INR .NE. OPR) THEN
                           USO2OTR(INR,IYR) = USO2OTR(INR,IYR) + &
                              ( FUEL * UFRSO2R(IFLTP,IFLRG) * (1.0 - UFRASH(IFLTP,IFLRG)) * (1.0 - REAL(KSCRB) * EFACTR) * 0.5 )* FACTE
                           UCO2OTR(INR,IYR) = UCO2OTR(INR,IYR) + ( (FUEL-FUEL_ALT) * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5 ) * FACTE
                           UCO2OTR(INR,IYR) = UCO2OTR(INR,IYR) + ( FUEL_ALT * UFRCO2(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5 ) * FACTE
                           UCAROTR(INR,IYR) = UCAROTR(INR,IYR) + ( (FUEL-FUEL_ALT) * UFRCAR(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5 ) * FACTE
                           UCAROTR(INR,IYR) = UCAROTR(INR,IYR) + ( FUEL_ALT * UFRCAR(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5 ) * FACTE
                           UNOXOTR(INR,IYR) = UNOXOTR(INR,IYR) + ( FUEL * NOXFAC * 0.5 ) / 1000000.0
                           UHGOTR(INR,IYR) = UHGOTR(INR,IYR) + ( FUEL * HGFAC * 0.5 )
                        ENDIF
!
                        UTSO2C(IFLTP,ICR) = UTSO2C(IFLTP,ICR) + &
                           FUEL * UFRSO2R(IFLTP,IFLRG) * (1.0 - UFRASH(IFLTP,IFLRG)) * (1.0 - REAL(KSCRB) * EFACTR) * 0.5

!                       IF (CURIYR .eq. 21) then
!                       write(6,2233) curiyr+uhbsyr,curitr,irg,icr,ifltp,iflrg,n,igrp,UTSO2C(IFLTP,ICR),FUEL,UFRSO2R(IFLTP,IFLRG),UFRASH(IFLTP,IFLRG), &
!                       (REAL(KSCRB) * EFACTR),EFACTR
!2233                   FORMAT(1X,"UTSO2C",8(":",I5),6(":",E15.6))
!                       END IF

                        UTNOXC(IFLTP,ICR) = UTNOXC(IFLTP,ICR) + FUEL * NOXFAC * 0.5
                        UTCO2C(IFLTP,ICR) = UTCO2C(IFLTP,ICR) + (FUEL-FUEL_ALT) * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        UTCO2C(IFLTP,ICR) = UTCO2C(IFLTP,ICR) + FUEL_ALT * UFRCO2(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5
                        UTCARC(IFLTP,ICR) = UTCARC(IFLTP,ICR) + (FUEL-FUEL_ALT) * UFRCAR(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        UTCARC(IFLTP,ICR) = UTCARC(IFLTP,ICR) + FUEL_ALT * UFRCAR(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5
                        UTCO1C(IFLTP,ICR) = UTCO1C(IFLTP,ICR) + (FUEL-FUEL_ALT) * UFRCO1(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        UTCO1C(IFLTP,ICR) = UTCO1C(IFLTP,ICR) + FUEL_ALT * UFRCO1(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5
                        UTVOCC(IFLTP,ICR) = UTVOCC(IFLTP,ICR) + FUEL * UFRVOC(IFLTP) * 0.5
                        UTHGC(IFLTP,ICR) = UTHGC(IFLTP,ICR) + FUEL * HGFAC * 0.5 * 0.000001
                        
  !                      IF (CURIYR.eq.27) WRITE(6,3823)'UTNOXC ', CURIYR,CURITR,IRG,IFLTP,ICR,IGRP,IECPT,UTNOXC(IFLTP,ICR),UTNOXC(IFLTP,ICR) ,FUEL , NOXFAC
  !                      IF (CURIYR.eq.27) WRITE(6,3823)'UTCO2C ', CURIYR,CURITR,IRG,IFLTP,ICR,IGRP,IECPT,UTCO2C(IFLTP,ICR),FUEL,UFRCO2(IFLTP,IFLRG), ECSEQS(N)
  !                      IF (CURIYR.eq.27) WRITE(6,3823)'UTCARC ', CURIYR,CURITR,IRG,IFLTP,ICR,IGRP,IECPT,UTCARC(IFLTP,ICR),FUEL,UFRCAR(IFLTP,IFLRG), ECSEQS(N) 
  !                      IF (CURIYR.eq.27) WRITE(6,3823)'UTCO1C ', CURIYR,CURITR,IRG,IFLTP,ICR,IGRP,IECPT,UTCO1C(IFLTP,ICR),FUEL,UFRCO1(IFLTP,IFLRG), ECSEQS(N)
  !                      IF (CURIYR.eq.27) WRITE(6,3823)'UTVOCC ', CURIYR,CURITR,IRG,IFLTP,ICR,IGRP,IECPT,UTVOCC(IFLTP,ICR),FUEL,UFRVOC(IFLTP) 
  !                      IF (CURIYR.eq.27) WRITE(6,3823)'UTHGC ', CURIYR,CURITR,IRG,IFLTP,ICR,IGRP,IECPT,UTHGC(IFLTP,ICR),FUEL,HGFAC 
  3823 FORMAT(A25,1x,7(I4,1x),4(F12.3,1x))
!
!                       5. BY FUEL TYPE AND EMM REGION
!
                        UTSO2N(IFLTP,INR) = UTSO2N(IFLTP,INR) + &
                           FUEL * UFRSO2R(IFLTP,IFLRG) * (1.0 - UFRASH(IFLTP,IFLRG)) * (1.0 - REAL(KSCRB) * EFACTR) * 0.5
                        UTNOXN(IFLTP,INR) = UTNOXN(IFLTP,INR) + FUEL * NOXFAC * 0.5
                        UTCO2N(IFLTP,INR) = UTCO2N(IFLTP,INR) + (FUEL-FUEL_ALT) * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        UTCO2N(IFLTP,INR) = UTCO2N(IFLTP,INR) + FUEL_ALT * UFRCO2(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5
                        UTCARN(IFLTP,INR) = UTCARN(IFLTP,INR) + (FUEL-FUEL_ALT) * UFRCAR(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        UTCARN(IFLTP,INR) = UTCARN(IFLTP,INR) + FUEL_ALT * UFRCAR(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5
                        UTCO1N(IFLTP,INR) = UTCO1N(IFLTP,INR) + FUEL * UFRCO1(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                        UTHGN(IFLTP,INR) = UTHGN(IFLTP,INR) + FUEL * HGFAC * 0.5 * 0.000001
!
!                       6. BY SO2 COMPLIANCE GROUP
!
                        IF (ULHGGP(IGRP) .LE. 0 .OR. ULHGGP(IGRP) .GT. 14) THEN

!jj                        ISO2 = 1

                           NSO2 = 0
                        ELSE

!jj                        ISO2 = SO2_GRP_BY_CLRG(ULHGGP(IGRP))

                           NSO2 = NUM_SO2_GRP
                        END IF

!jj                     IF (ISO2 .GT. 0) THEN

                        IF (NSO2 .GT. 0) THEN
                           DO ISO2 = 1 , NSO2
                              IF (SO2_SHR_BY_CLRG(ULHGGP(IGRP),ISO2) .GT. 0.0) THEN
                                 EGSO2(ISO2) = EGSO2(ISO2) + &
                                    FUEL * UFRSO2R(IFLTP,IFLRG) * SO2_SHR_BY_CLRG(ULHGGP(IGRP),ISO2) * &
                                    ( 1.0 - UFRASH(IFLTP,IFLRG)) * ( 1.0 - REAL(KSCRB) * EFACTR) * 0.5
                              END IF
                           END DO
                        ELSE
                           ISO2 = 1
                           EGSO2(ISO2) = EGSO2(ISO2) + FUEL * UFRSO2R(IFLTP,IFLRG) * ( 1.0 - UFRASH(IFLTP,IFLRG)) * ( 1.0 - REAL(KSCRB) * EFACTR) * 0.5
                        END IF
!
!                       ACCUM OIL/GAS SWITCHING INFO FOR DUAL-FIRED STEAM PLANTS
!
                        IF (IDUAL .GT. 0) THEN
!
!                          ACCUMULATE CONSUMPTION OF "GOIL"
!
                           TOTGOIL = TOTGOIL + FUEL
!
!                          LS RESID CONSUMPTION
!
                           IF (IFLTP .EQ. UIRL) THEN
                              TQDFRLG(IGR) = TQDFRLG(IGR) + FUEL
                           END IF
!
!                          HS RESID CONSUMPTION
!
                           IF (IFLTP .EQ. UIRH) THEN
                              TQDFRHG(IGR) = TQDFRHG(IGR) + FUEL
                           END IF
                        END IF
                     END IF                               ! IF IFOWN LE 4
!
!                    STORE ENERGY IN OUTPUT ARRAYS
!
                     ULGENE(IFP,IGRP) = ULGENE(IFP,IGRP) + GEN * ECFSHR(N,IFP)
                     ULBTUE(IFP,IGRP) = ULBTUE(IFP,IGRP) + FUEL
                     ULSO2W(IFP,IGRP) = ULSO2W(IFP,IGRP) + FUEL * UFRSO2R(IFLTP,IFLRG) * (1.0 - UFRASH(IFLTP,IFLRG)) * (1.0 - REAL(KSCRB) * EFACTR) * 0.5
                     ULNOXW(IFP,IGRP) = ULNOXW(IFP,IGRP) + FUEL * NOXFAC * 0.5
                     ULCO2W(IFP,IGRP) = ULCO2W(IFP,IGRP) + (FUEL-FUEL_ALT) * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                     ULCO2W(IFP,IGRP) = ULCO2W(IFP,IGRP) + FUEL_ALT * UFRCO2(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5
                     ULCARW(IFP,IGRP) = ULCARW(IFP,IGRP) + (FUEL-FUEL_ALT) * UFRCAR(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * 0.5
                     ULCARW(IFP,IGRP) = ULCARW(IFP,IGRP) + FUEL_ALT * UFRCAR(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * 0.5
                     VAL = FUEL * HGFAC * 0.5 * 0.000001
                     IF (ISNAN(VAL).OR. ABS(VAL) .GT. HUGE(VAL)) THEN   ! check for NaNQ this way
                        WRITE(6,9724) CURIYR+1989,CURITR,IRG,IGRP,IFP,FUEL,HGFAC,VAL
 9724                   FORMAT(1X,"ULHGQ_OOPS",5(":",I6),3(":",F15.3))
                     ELSE
                        ULHGQ(IFP,IGRP) = ULHGQ(IFP,IGRP) +  FUEL * HGFAC * 0.5 * 0.000001
                     ENDIF                                      !

!                    SUM CO2 GPS COMPLIANCE COSTS FOR MARGINAL PRICING

                     IF (IFOWN .LE. 3) THEN
                       CO2LB = ULHTRT_EFD(IGRP,ISP) * 0.001 * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N))
                     ELSE
                       CO2LB = ULHTRT_EFD(IGRP,ISP) * 0.001 * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N))/CO2_ADJNT
                     ENDIF
                     IF (IFLTP .EQ. UIDS) CO2LB = CO2LB * 1.10

                     IF (CO2_STDSW .GT. 0 .AND. ULVINT(IGRP) .EQ. 3) THEN
                      IF (CO2_STDTN(IRG) .EQ. 1) THEN    !rate case
                       IF (ULVINT(IGRP) .EQ. 3 .AND. CO2LB .LT. CO2_STDRN(IRG,CURIYR+1) ) THEN
                         NWGPSGEN(IECPT,IRG,CURIYR) = NWGPSGEN(IECPT,IRG,CURIYR) + GEN * ECFSHR(N,IFP)
                       ENDIF
                      ENDIF
                     ENDIF
!
                     IF (CO2_STDSW .GT. 0) THEN
                        IF (CO2_STDTN(IRG) .EQ. 1) THEN   ! rate
                          IF (CO2_PLTSW(IECPT) .GT. 0) THEN
                             ULGHG(IGRP) = ULGHG(IGRP) + CO2_PLTSW(IECPT) * ECO2NRPR(IRG,CURIYR) * (CO2LB - CO2_STDRN(IRG,CURIYR)) * GEN * ECFSHR(N,IFP) * 0.001
                          ELSEIF (CO2_INCSW(IECPT) .GT. 0 .AND. (ULVINT(IGRP) .EQ. 2 .OR. ULVINT(IGRP) .EQ. 3)) THEN
                             ULGHG(IGRP) = ULGHG(IGRP) + CO2_INCSW(IECPT) * ECO2NRPR(IRG,CURIYR) * (CO2LB - CO2_STDRN(IRG,CURIYR)) * GEN * ECFSHR(N,IFP) * 0.001
                          ENDIF
                        ELSEIF (CO2_STDTN(IRG) .EQ. 2) THEN  ! mass
                          ULGHG(IGRP) = ULGHG(IGRP) + CO2_PLTSW(IECPT) * ECO2NRPR(IRG,CURIYR) * CO2LB * GEN * ECFSHR(N,IFP) * 0.001
                        ENDIF
                     ENDIF
          
                     IF (CO2_STDSW .GT. 0  .AND. CO2_STDRS(1,CURIYR) .GT. 0.0) THEN
                      IF (IFOWN .LE. 3) THEN
                       AFFEM =  (FUEL-FUEL_ALT) * UFRCO2(IFLTP,IFLRG) * (1.0 - ECSEQS(N)) * CO2_PLTSW(IECPT) * 0.001   ! million lbs
                       AFFEM = AFFEM + FUEL_ALT * UFRCO2(IFLTP,IFLRG) * (1.0 - UPPCEF_MIN(IECPT)) * CO2_PLTSW(IECPT) * 0.001   ! million lbs
                      ELSE     ! make adjustment for CHP emission rate 
                       AFFEM =  (FUEL-FUEL_ALT) * (UFRCO2(IFLTP,IFLRG)/CO2_ADJNT) * (1.0 - ECSEQS(N)) * CO2_PLTSW(IECPT) * 0.001   ! million lbs
                       AFFEM = AFFEM + FUEL_ALT * (UFRCO2(IFLTP,IFLRG)/CO2_ADJNT) * (1.0 - UPPCEF_MIN(IECPT)) * CO2_PLTSW(IECPT) * 0.001   ! million lbs
                      ENDIF
                       SUMAFFEM(IECPT) = SUMAFFEM(IECPT) + AFFEM
                       IF (CO2_PRCSW .GT. 0) THEN                    ! EMM REGION CONSTRAINT
                         IF (CO2_PRCSW .EQ. 1) THEN
                            ULCO2CST(IRG,CURIYR) = ULCO2CST(IRG,CURIYR) + AFFEM * ECO2NRPR(IRG,CURIYR)       
                         ELSEIF (CO2_PRCSW .EQ. 2) THEN
                            ULCO2CST(IRG,CURIYR) = ULCO2CST(IRG,CURIYR) + AFFEM * ECO2NRPP(IRG,CURIYR) * 0.001
                         ELSEIF (CO2_PRCSW .EQ. 3) THEN
                           DIFF111 = ECO2NRPP(IRG,CURIYR) * 0.001 - ECO2NRPR(IRG,CURIYR)
                           IF (DIFF111 .GT. 0.0) &
                            ULCO2CST(IRG,CURIYR) = ULCO2CST(IRG,CURIYR) + AFFEM * DIFF111                              ! million $
                         ENDIF
                       ENDIF
                     ENDIF
 
                  END IF                                 ! IF ECFSHR .EQ. 0
   20          CONTINUE                                ! END FUEL TYPE LOOP
!
!              COMPETITIVE GAS -- ACCUMULATE TOTALS FOR DETERMINATION OF
!              WEIGHTED-AVERAGE OF CONSUMPTION/PRICES FOR MIN, MAX, AND PARITY SHARES
!
               IF (IDUAL .GT. 0) THEN
                  TSGCMIN(IGR) = TSGCMIN(IGR) + USGCMIN(N) * TOTGOIL
                  TRGCMIN(IGR) = TRGCMIN(IGR) + URGCMIN(N) * TOTGOIL
                  TSGCMAX(IGR) = TSGCMAX(IGR) + USGCMAX(N) * TOTGOIL
                  TRGCMAX(IGR) = TRGCMAX(IGR) + URGCMAX(N) * TOTGOIL
                  TSGCPAR(IGR) = TSGCPAR(IGR) + USGCPAR(N) * TOTGOIL
                  TRGCPAR(IGR) = TRGCPAR(IGR) + URGCPAR(N) * TOTGOIL
               END IF
            END IF
!
!           IF (CURIYR .EQ. 16 .AND. FCRL .EQ. 1) THEN
!              WRITE(18,3751) CURIYR+UHBSYR, CURITR, ISP, IRG, IGRP, N, IPGRP, JGRP, ULTGEN(IGRP), ECDSPE(IGRP,ISP), ECCAP(N,ISP), &
!                 DBLE(ECMXCP(N)*EFACTR), EETIME(ISP), (ULFUEL(IFL,IGRP), ULGENE(IFL,IGRP), ECFSHR(N,IFL),IFL=1,3)
!3751          FORMAT(1X,"BAD_GRP_DATA_ELCST",8(":",I5),5(":",F12.3),3(":",I2,2(":",F12.3)))
!           END IF
!
  100    CONTINUE                               ! END HORIZONTAL SLICE LOOP
      END DO  ! ISP
!     write(6,'(A10,3I4,47F10.2)') 'AFFEM ',CURIYR,CURITR,IRG,(SUMAFFEM(IP),IP=1,ECP_D_DSP)
!

      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
         CLOSE(unit_num_ephrts_debug_file)
      END IF
      
      RETURN
      END
!
!     ELRNEWO - ! TABULATES OUTPUT FROM RENEWABLES
!
      SUBROUTINE ELRNEWO(ISP,IRG,IYR)
!
      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'dispout'
      include 'dispuse'
      include 'fuelin'
      include 'wrenew'
      include 'wwdcomon'
      include 'dispett'
      include 'ecpcntl'
      include 'postpr'
      include 'uefdout'
      include 'uecpout'
      include 'elout'
      include 'elcntl'
      include 'bildin'
      include 'emablk'
      include 'efdin'
      include 'cdsparms'
      include 'coalemm'
      include 'uso2grp'
      include 'ecp_coal'
      include 'plntctl'
      include 'emission'
      include 'macout'
      include 'eusprc'
      include 'efpint'
      include 'e111d'
      include 'dsmdimen'
      include 'dsmtfecp'
      include 'dsmtoefd'
      include 'dsmcaldr'
      include 'csapr'
      include 'emmemis'

      INTEGER FULLYR,IRNW,JRNW,IGRP,KRNW,ISP,IRG,IST,IYR,IFP,IFL,NFLRG,IFOWN,ICR,INR,IFLRG,FRG,KRG,IFR,ITYPE,INT,JN
      INTEGER IECP
      REAL*8  PSFAC,GEN,NOXFAC,FUEL,PRPS,BRPS,SRPS,CO2LB,CAPFAC,SPRPS,SBRPS,SSRPS,CST_RPS,CST_RPS_ST


      FULLYR = USYEAR(CURIYR)
      IF (FULLYR .eq. UESTYR) THEN
         PRPS = 0.0
      ELSEIF (CURITR .EQ. 1) THEN
         PRPS = RENEWCRR(IRG,CURIYR - 1)
      ELSE
         PRPS = RENEWCRR(IRG,CURIYR)
      ENDIF
         BRPS = MAX(UPRNWBNDR(CURIYR,IRG),UPRNWBND(CURIYR))

      IF (ST_RPS_SW .GT. 0) THEN
         SPRPS = ST_RPS_EMM_P(IRG,CURIYR)
         SBRPS = ST_RNW_BND(CURIYR,IRG)
      END IF

!
!     STORE ENERGY IN OUTPUT ARRAYS
!
      DO IRNW    = 1, EHNTP
         JN = MAP_EHNTP_EFD_GRPS(IRNW,IRG)
         IGRP = EHDBID(IRNW)
         KRNW = EHHYTP(IRNW)
         JRNW = EHHYTP(IRNW) - EIPGRP
         IST = EHST(IRNW)
         IECP = EHTECP(IRNW)
         
        SRPS = MAX( UPRNWSHR(IECP) , UPRNWSHRR(IECP,IRG) )
        IF (ST_RPS_SW .GT. 0) THEN
           SSRPS = ST_RNW_SHR(IECP,IRG,CURIYR)
        END IF

        CST_RPS = PRPS * (BRPS - SRPS)      ! avg rev rec'd for RPS credits from national/regional 
               
        IF (ST_RPS_SW .GT. 0) THEN
           CST_RPS_ST = SPRPS * (SBRPS - SSRPS)
          IF (CST_RPS .LT. 0.0 .AND. CST_RPS_ST .LE. 0.0) THEN  ! if both are negative use the greatest benefit
           IF (ABS(CST_RPS) .LT. ABS(CST_RPS_ST)) CST_RPS = CST_RPS_ST
          ELSEIF (CST_RPS .GT. 0.0 .AND. CST_RPS_ST .GT. 0.0) THEN    ! if both are positive, use the highest positive cost
           IF (CST_RPS .LT. CST_RPS_ST) CST_RPS = CST_RPS_ST
          ELSE                        ! use net value
               CST_RPS = CST_RPS + CST_RPS_ST
          ENDIF                  
        END IF
        ELRPSP(ISP,IGRP) = CST_RPS      


!        IF (CURIYR+1989 .GE. 2020 .AND. CURIYR+1989 .LE. 2040 .AND. FCRL .EQ. 1) THEN
!          WRITE(13,9397) CURIRUN, CURIYR+1989, CURITR, IRG, IGRP, ISP, IRNW, EHTECP(IRNW), ST_RPS_SW, UPRNWREG, &
!             ELRPSP(ISP,IGRP), PRPS, UPRNWSHR(EHTECP(IRNW)), BRPS, RENEWCRR(IRG,CURIYR), UPRNWBNDR(CURIYR,IRG), UPRNWBND(CURIYR), ST_RPS_EMM_P(IRG,CURIYR), ST_RNW_BND(CURIYR,IRG), SRPS, ST_RNW_SHR(IECP,IRG,CURIYR)
!9397      FORMAT(1X,"ELRNEWO_ELRPSP",10(":",I6),11(":",F21.6))
!       END IF
         

         STORAGE_RGN(IGRP) = IRG
         STORAGE_ECPn(IGRP) = IECP
         STORAGE_ECPc(IGRP) = UPLNTCD(IECP)

         CAPFAC = EHHYCF(IRNW,ISP) * 0.001
!
!        if (curitr .eq. 1. .and. isp .eq. 1 .and. ist .eq. 0)write(6,3456) curiyr+1989,irg,urgnme(irg),irnw,igrp,  &
!           ehtecp(irnw),uplntcd(ehtecp(irnw)),krnw,epplcd(krnw),ehcap(irnw,isp)
!3456 format(1h ,'!nostaternw',i4,i3,a9,i6,i6,i3,a3,i3,a4,f10.1)
!
!        IF (CURIYR .EQ. 16 .AND. FCRL .EQ. 1) THEN
!           WRITE(18,3751) CURIYR+UHBSYR,CURITR,ISP,IRG,IGRP,IRNW,KRNW,JRNW,ULTGEN(IGRP),ECDSPE(IGRP,ISP),EHCAP(IRNW,ISP), &
!              DBLE(EHHYCF(IRNW,ISP) * 0.001),EETIME(ISP), &
!              (ULFUEL(IFP,IGRP),ULGENE(IFP,IGRP),EHMFSH(IRNW,IFP),IFP=1,3)
!3751       FORMAT(1X,"BAD_GRP_DATA_ELRNW",8(":",I5),5(":",F12.3),3(":",I2,2(":",F12.3)))
!        END IF
         INT = UCPINTIS(IECP)
         IF (INT .GT. 0 ) THEN
            IF (CAPFAC .GT. 0.0 .AND. IECP .NE. WIPT) THEN
               STORAGE_CAP(IGRP,ISP) = EHCAP(IRNW,ISP) * 0.001 * (CAPFAC / STO_CFefdSEA(INT,ISP,IRG))
            ELSE
               STORAGE_CAP(IGRP,ISP) = EHCAP(IRNW,ISP) * 0.001         
            END IF
         ELSE
            IF (IECP .EQ. WIDS) THEN
               STORAGE_CAP(IGRP,ISP) = EHCAP(IRNW,ISP) * 0.001
            ELSE
               STORAGE_CAP(IGRP,ISP) = EHCAP(IRNW,ISP) * 0.001 * (1.0 - UG_FOR(JN))
            ENDIF
         ENDIF

         STORAGE_GEN(IGRP,ISP) = ECDSPE(IGRP,ISP) * 0.001

         STORAGE_CST(IGRP,ISP) = 0.0

         DO IFP = 1 , EIFPLT
            IFL = ULFUEL(IFP,IGRP)
            IF (IFL .GT. 0) THEN
               IFLRG = ULFLRG(IFP,IGRP)

!              STORAGE_CST(IGRP,ISP) = STORAGE_CST(IGRP,ISP) + EHMFSH(IRNW,IFP) * 0.001 * ((ULHTRT_EFD(IGRP,ISP) * UPFUEL(IFL,IFLRG)) + EHVOMR(IRNW))
				
				! SETTING UPV and DPV COSTS SO THAT DPV IS DISPATCHED FIRST	
				IF (IECP .EQ. WIPV ) THEN
					IF (ULMRUN(IGRP) .EQ. 0) THEN !UPV
						STORAGE_CST(IGRP,ISP) = 0.001
					ELSE !DPV
						STORAGE_CST(IGRP,ISP) = 0.0001
					ENDIF
				ELSE
					STORAGE_CST(IGRP,ISP) = STORAGE_CST(IGRP,ISP) + EHMFSH(IRNW,IFP) * (0.001 * (ULHTRT_EFD(IGRP,ISP) * UPFUEL(IFL,IFLRG)) + EHVOMR(IRNW) + ELRPSP(ISP,IGRP)) 
				END IF

               IF (FCRL .EQ. 1) THEN
                  IF (INT .GT. 0) THEN
                     WRITE(18,3881) CURIRUN, CURIYR+1989, CURITR, FCRL, IRG, ISP, IFP, IFL, IRNW, JN, IGRP, IFLRG, IECP, &
                         STORAGE_CAP(IGRP,ISP), STORAGE_CST(IGRP,ISP), CAPFAC, ULHTRT_EFD(IGRP,ISP), UPFUEL(IFL,IFLRG), STO_CFefdSEA(INT,ISP,IRG), 0.0, 0.0, 0.0, 0.0, &
                         EHVOMR(IRNW), 0.0, EHCAP(IRNW,ISP) * 0.001, UG_FOR(JN),0.0,ELRPSP(ISP,IGRP)
                  ELSE
                     WRITE(18,3881) CURIRUN, CURIYR+1989, CURITR, FCRL, IRG, ISP, IFP, IFL, IRNW, JN, IGRP, IFLRG, IECP, &
                         STORAGE_CAP(IGRP,ISP), STORAGE_CST(IGRP,ISP), 0.0, ULHTRT_EFD(IGRP,ISP), UPFUEL(IFL,IFLRG), 0.0, 0.0, 0.0, 0.0, 0.0, &
                         EHVOMR(IRNW), 0.0, EHCAP(IRNW,ISP) * 0.001, UG_FOR(JN),0.0,ELRPSP(ISP,IGRP)
                  END IF
 3881             FORMAT(1X,"STORAGE_CST_UEFD",13(",",I6),16(",",F21.8))
               END IF
            STORAGE_CST(IGRP,ISP) = MAX(0.0,STORAGE_CST(IGRP,ISP))

            END IF
         END DO
!
       IF ((EHCAP(IRNW,ISP) * CAPFAC) .GT. 0.0 .OR. ECDSPE(IGRP,ISP) .NE. 0.0) THEN

       IF (KRNW .EQ. UIHYR) THEN
         IF (IECP .EQ. WIP2) THEN
           PSFAC = UFACP2
         ELSE
           PSFAC = UFACPS
         ENDIF
       ELSE
          PSFAC = 0.0
       ENDIF


        ULCSTR(ISP,IGRP) = EHVOMR(IRNW) - EHGSUB(IRNW)
        GEN = ECDSPE(IGRP,ISP)
        IF (CO2_STDTN(IRG) .EQ. 1) THEN  ! rate case
          IF (ULVINT(IGRP) .EQ. 3) NWGPSGEN(IECP,IRG,CURIYR) = NWGPSGEN(IECP,IRG,CURIYR) + GEN   !sum new gen for 111d adj
        ENDIF

        IF (CO2_STDSW .GT. 0) THEN
            CO2LB = 0.0
          IF (CO2_STDTN(IRG) .EQ. 1) THEN  ! only adjust if rate case
            ULGHG(IGRP) = ULGHG(IGRP) + CO2_PLTSW(IECP) * ECO2NRPR(IRG,CURIYR) * (CO2LB - CO2_STDRN(IRG,CURIYR)) * GEN * 0.001
            IF (CO2_INCSW(IECP) .GT. 0.0 .AND. (ULVINT(IGRP) .EQ. 2 .OR. ULVINT(IGRP) .EQ. 3)) THEN     !include incremental gen
               ULGHG(IGRP) = ULGHG(IGRP) + CO2_INCSW(IECP) * ECO2NRPR(IRG,CURIYR) * (CO2LB - CO2_STDRN(IRG,CURIYR)) * GEN * 0.001
            ENDIF
          ENDIF
        ENDIF
        NOXFAC = EHFNOX(IRNW,ISP)
        IFOWN = EHFOWN(IRNW)
        DO IFP = 1 , EIFPLT
          IFL = ULFUEL(IFP,IGRP)
           IF (IFL .GT. 0) THEN
             IFLRG = ULFLRG(IFP,IGRP)
             ULGENE(IFP,IGRP) = ULGENE(IFP,IGRP) +  GEN * EHMFSH(IRNW,IFP)

             FUEL = ECDSPF(IGRP,ISP)

             EQPFLCN(KRNW,IFOWN,IFL) =  EQPFLCN(KRNW,IFOWN,IFL) + FUEL
             UQFCONN(IFL,IRG,IFOWN) = UQFCONN(IFL,IRG,IFOWN) + FUEL
             EQPFLGN(KRNW,IFOWN,IFL) =  EQPFLGN(KRNW,IFOWN,IFL) +  (GEN * EHMFSH(IRNW,IFP))
             ULBTUE(IFP,IGRP) = ULBTUE(IFP,IGRP) + FUEL
             ULSO2W(IFP,IGRP) = ULSO2W(IFP,IGRP) +  FUEL * UFRSO2R(IFL,IFLRG) * 0.5
             ULNOXW(IFP,IGRP) = ULNOXW(IFP,IGRP) +  FUEL * NOXFAC * 0.5
             UNOXINR(IRG,IYR) = UNOXINR(IRG,IYR) + ( FUEL * NOXFAC * 0.5 ) / 1000000.0
             ULCO2W(IFP,IGRP) = ULCO2W(IFP,IGRP) +  FUEL * UFRCO2(IFL,1) * 0.5
             ULCARW(IFP,IGRP) = ULCARW(IFP,IGRP) +  FUEL * UFRCAR(IFL,1) * 0.5
             IF (GEN .GT. 0.0) THEN
                ULCSTR(ISP,IGRP) = ULCSTR(ISP,IGRP) +  UPFUEL(IFL,IFLRG) * EHMFSH(IRNW,IFP) * (ECDSPF(IGRP,ISP) / GEN)
                ULFLCST(IFP,IGRP) =  ULFLCST(IFP,IGRP) +  UPFUEL(IFL,IFLRG) * EHMFSH(IRNW,IFP) * (ECDSPF(IGRP,ISP) / GEN)
             ELSE
                ULCSTR(ISP,IGRP) = ULCSTR(ISP,IGRP) +  UPFUEL(IFL,IFLRG) * EHMFSH(IRNW,IFP) *  ULHTRT_EFD(IGRP,ISP) * 0.001
                ULFLCST(IFP,IGRP) =  ULFLCST(IFP,IGRP) +  UPFUEL(IFL,IFLRG) * EHMFSH(IRNW,IFP) *  ULHTRT_EFD(IGRP,ISP) * 0.001
             END IF
!
             DO NFLRG = 1 , UNFLRG(IFL)
                IFLRG = EHFLRG(IRNW,IFP,NFLRG)
                IF (IFLRG .LE. 0) THEN
                   IFLRG = EFD_D_MFRG
                   PRINT *,' NO FUEL REGION ',IRNW,IFP,NFLRG
                ENDIF
                IF (IFOWN .LE. 4) THEN
                   UQFUEL(IFL,IFLRG,NFLRG) = UQFUEL(IFL,IFLRG,NFLRG) + FUEL
                   IF (NFLRG .EQ.1)SQFUEL(IFL,IFLRG,ISP) = SQFUEL(IFL,IFLRG,ISP) + FUEL
                END IF
             END DO
             IF (IST .GT. 0)THEN
!               GENERATION BY NERC REGION AND STATE
                EGEN_NRST(IECP,IRG,IST) = EGEN_NRST(IECP,IRG,IST) + GEN * 0.001
                EGEN_NRST(IECP,MNUMNR,IST) = EGEN_NRST(IECP,MNUMNR,IST) + GEN * 0.001
                EGEN_NRST(IECP,IRG,EMM_D_ST + 1) = EGEN_NRST(IECP,IRG,EMM_D_ST + 1) + GEN * 0.001
                EGEN_NRST(IECP,MNUMNR,EMM_D_ST + 1) = EGEN_NRST(IECP,MNUMNR,EMM_D_ST + 1) + GEN * 0.001
                EGEN_NRST(ECP_D_CAP + 1,IRG,IST) = EGEN_NRST(ECP_D_CAP + 1,IRG,IST) + GEN * 0.001
                EGEN_NRST(ECP_D_CAP + 1,MNUMNR,IST) = EGEN_NRST(ECP_D_CAP + 1,MNUMNR,IST) + GEN * 0.001
                EGEN_NRST(ECP_D_CAP + 1,IRG,EMM_D_ST + 1) = EGEN_NRST(ECP_D_CAP + 1,IRG,EMM_D_ST + 1) + GEN * 0.001
                EGEN_NRST(ECP_D_CAP + 1,MNUMNR,EMM_D_ST + 1) = EGEN_NRST(ECP_D_CAP + 1,MNUMNR,EMM_D_ST + 1) + GEN * 0.001
!               GENERATION BY NERC REGION AND FUEL REGION
!                FRG = EST_FRG(IST)
                FRG = EPNFLRG(EHCR(IRNW),EHLR(IRNW),EHGR(IRNW),EHAR(IRNW))
                EGEN_NRFR(IECP,IRG,FRG) = EGEN_NRFR(IECP,IRG,FRG) + GEN * 0.001
                EGEN_NRFR(IECP,MNUMNR,FRG) = EGEN_NRFR(IECP,MNUMNR,FRG) + GEN * 0.001
                EGEN_NRFR(IECP,IRG,UNFRGN + 1) = EGEN_NRFR(IECP,IRG,UNFRGN + 1) + GEN * 0.001
                EGEN_NRFR(IECP,MNUMNR,UNFRGN + 1) = EGEN_NRFR(IECP,MNUMNR,UNFRGN + 1) + GEN * 0.001
                EGEN_NRFR(ECP_D_CAP + 1,IRG,FRG) = EGEN_NRFR(ECP_D_CAP + 1,IRG,FRG) + GEN * 0.001
                EGEN_NRFR(ECP_D_CAP + 1,MNUMNR,FRG) = EGEN_NRFR(ECP_D_CAP + 1,MNUMNR,FRG) + GEN * 0.001
                EGEN_NRFR(ECP_D_CAP + 1,IRG,UNFRGN + 1) = EGEN_NRFR(ECP_D_CAP + 1,IRG,UNFRGN + 1) + GEN * 0.001
                EGEN_NRFR(ECP_D_CAP + 1,MNUMNR,UNFRGN + 1) = EGEN_NRFR(ECP_D_CAP + 1,MNUMNR,UNFRGN + 1) + GEN * 0.001
!               GENERATION BY FUEL REGION AND STATE
                EGEN_FRST(IECP,FRG,IST) = EGEN_FRST(IECP,FRG,IST) + GEN * 0.001
                EGEN_FRST(IECP,UNFRGN + 1,IST) = EGEN_FRST(IECP,UNFRGN + 1,IST) + GEN * 0.001
                EGEN_FRST(IECP,FRG,EMM_D_ST + 1) = EGEN_FRST(IECP,FRG,EMM_D_ST + 1) + GEN * 0.001
                EGEN_FRST(IECP,UNFRGN + 1,EMM_D_ST + 1) = EGEN_FRST(IECP,UNFRGN + 1,EMM_D_ST + 1) + GEN * 0.001
                EGEN_FRST(ECP_D_CAP + 1,FRG,IST) = EGEN_FRST(ECP_D_CAP + 1,FRG,IST) + GEN * 0.001
                EGEN_FRST(ECP_D_CAP + 1,UNFRGN + 1,IST) = EGEN_FRST(ECP_D_CAP + 1,UNFRGN + 1,IST) + GEN * 0.001
                EGEN_FRST(ECP_D_CAP + 1,FRG,EMM_D_ST + 1) = EGEN_FRST(ECP_D_CAP + 1,FRG,EMM_D_ST + 1) + GEN * 0.001
                EGEN_FRST(ECP_D_CAP + 1,UNFRGN + 1,EMM_D_ST + 1) = EGEN_FRST(ECP_D_CAP + 1,UNFRGN + 1,EMM_D_ST + 1) + GEN * 0.001
!               GENERATION BY AFFECTED (111d) PLANTS
                IF (CO2_PLTSW(IECP) .GT. 0.0)THEN
                   EGEN_NRST(ECP_D_CAP + 2,IRG,IST) = EGEN_NRST(ECP_D_CAP + 2,IRG,IST) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_NRST(ECP_D_CAP + 2,MNUMNR,IST) = EGEN_NRST(ECP_D_CAP + 2,MNUMNR,IST) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_NRST(ECP_D_CAP + 2,IRG,EMM_D_ST + 1) = EGEN_NRST(ECP_D_CAP + 2,IRG,EMM_D_ST + 1) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_NRST(ECP_D_CAP + 2,MNUMNR,EMM_D_ST + 1) = EGEN_NRST(ECP_D_CAP + 2,MNUMNR,EMM_D_ST + 1) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_NRFR(ECP_D_CAP + 2,IRG,FRG) = EGEN_NRFR(ECP_D_CAP + 2,IRG,FRG) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_NRFR(ECP_D_CAP + 2,MNUMNR,FRG) = EGEN_NRFR(ECP_D_CAP + 2,MNUMNR,FRG) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_NRFR(ECP_D_CAP + 2,IRG,UNFRGN + 1) = EGEN_NRFR(ECP_D_CAP + 2,IRG,UNFRGN + 1) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_NRFR(ECP_D_CAP + 2,MNUMNR,UNFRGN + 1) = EGEN_NRFR(ECP_D_CAP + 2,MNUMNR,UNFRGN + 1) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_FRST(ECP_D_CAP + 2,FRG,IST) = EGEN_FRST(ECP_D_CAP + 2,FRG,IST) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_FRST(ECP_D_CAP + 2,UNFRGN + 1,IST) = EGEN_FRST(ECP_D_CAP + 2,UNFRGN + 1,IST) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_FRST(ECP_D_CAP + 2,FRG,EMM_D_ST + 1) = EGEN_FRST(ECP_D_CAP + 2,FRG,EMM_D_ST + 1) + GEN * 0.001 * CO2_PLTSW(IECP)
                   EGEN_FRST(ECP_D_CAP + 2,UNFRGN + 1,EMM_D_ST + 1) = EGEN_FRST(ECP_D_CAP + 2,UNFRGN + 1,EMM_D_ST + 1) + GEN * 0.001 * CO2_PLTSW(IECP)
                END IF
             END IF
!
           END IF
        END DO
!  adjust unit cost for allowance allocations
        ULCSTR(ISP,IGRP) = ULCSTR(ISP,IGRP) - GPSCSUB(KRNW,IRG)
        ELSO2P(ISP,IGRP) = ELSO2P(ISP,IGRP) - GPSSSUB(KRNW,IRG)
        ELNOXP(ISP,IGRP) = ELNOXP(ISP,IGRP) - GPSNSUB(KRNW,IRG)
        ELHGP(ISP,IGRP) = ELHGP(ISP,IGRP) - GPSHSUB(KRNW,IRG)
        ICR = EHCR(IRNW)
        INR = EHNR(IRNW)
        BGENOWN(IRG,IFOWN) = BGENOWN(IRG,IFOWN) + GEN * 0.001
        EGENHS(JRNW,ISP) = EGENHS(JRNW,ISP) + GEN
        ULGENS(ISP,IGRP) = ULGENS(ISP,IGRP) + GEN
        EQHGN(JRNW,IFOWN) = EQHGN(JRNW,IFOWN) +  GEN
        ERTOM(IFOWN) = ERTOM(IFOWN) +  &
           GEN * 0.001 * EHVOMR(IRNW)
       IF (EHGSUB(IRNW) .GT. 0.0)THEN
        IF (UPGSUBPT(EHTECP(IRNW)) .GT. 0.0) THEN
        UGNPTCN(KRNW,INR) = UGNPTCN(KRNW,INR) +  &
           GEN  * 0.001 !*  &           !just count all generation if annual value used
!           EHGSUB(IRNW) / UPGSUBYR(EHTECP(IRNW),CURIYR)  ! year should be start year, but might not be the same for entire group
        ELSE
        UGNPTCN(KRNW,INR) = UGNPTCN(KRNW,INR) +  &
           GEN  * 0.001 *  &
           EHGSUB(IRNW) / UPGSUB(EHTECP(IRNW))
        ENDIF
        UPYPTCN(KRNW,INR) = UPYPTCN(KRNW,INR) +  &
           GEN  * 0.001 * EHGSUB(IRNW)
        ERTGSUB(IFOWN) = ERTGSUB(IFOWN) +  &
           GEN  * 0.001 * EHGSUB(IRNW) * (1.0 - UPTXRT)
       END IF
        ERHOM(JRNW,IFOWN) = ERHOM(JRNW,IFOWN) + &
           GEN  * 0.001 * EHVOMR(IRNW)
        UQPGENN(KRNW,INR,IFOWN) = UQPGENN(KRNW,INR,IFOWN) + GEN
        UQPGENC(KRNW,ICR) = UQPGENC(KRNW,ICR) + GEN
!
      ENDIF
      ENDDO
!
!     STORE NOMINAL CAPACITY VALUES IN EMPTY CELLS
!
      IF (IRG .EQ. UNRGNS .AND. ISP .EQ. EENSP)THEN
         DO IECP = 1 , ECP_D_CAP
            IF (UPVTYP(IECP) .GT. 0)THEN
               DO KRG = 1 , UNRGNS
                  IF (EGEN_NRFR(IECP,KRG,UNFRGN + 1) .LE. 0.0)THEN
                     DO IFR = 1 , UNFRGN
                        IF (EGEN_NRFR(ECP_D_CAP + 1,KRG,IFR) .GT. 0.0)THEN
                           EGEN_NRFR(IECP,KRG,IFR) = EGEN_NRFR(IECP,MNUMNR,IFR) * 0.0001
                           EGEN_NRFR(IECP,KRG,UNFRGN + 1) = EGEN_NRFR(IECP,KRG,UNFRGN + 1) + EGEN_NRFR(IECP,MNUMNR,IFR) * 0.0001
!                          if (iecp .eq. wiwn .and. krg .eq. 12 .and. curitr .eq. 1)write(6,1234) curiyr+1989,ifr,  &
!                          EGEN_NRFR(IECP,KRG,IFR) ,  &
!                          EGEN_NRFR(IECP,KRG,UNFRGN + 1)
!1234 format(1h ,'!egenwn',i4,i3,2f10.5)
                        END IF
                     END DO
                  END IF
               END DO
            END IF
         END DO
      END IF

      RETURN
      END
!
!
!     - ELDGNO -
!     THIS SUBROUTINE TABULATES FUEL CONSUMPTION, FUEL COSTS, AND
!     OPERATION AND MAINTENANCE COSTS ASSOCIATED WITH DISTRIBUTED
!     GENERATION.
!     ==================================================================
!
!     INPUT VARIABLES
!
!     EDASTS = CAPACITY TYPE FOR PLANT GROUP
!     ECDSPN = NUMBER OF HORIZONTAL SLICES MADE IN THE LOAD CURVE
!     ECDSPC = CAPACITY ALLOCATED TO EACH HORIZONTAL LOAD CURVE SLICE
!     ECDSPE = ELECTRICITY GENERATED IN EACH LOAD CURVE SLICE
!     ECDSPT = PLANT TYPE ALLOCATED TO EACH LOAD CURVE SLICE
!     EDFOWN = OWNERSHIP TYPE FOR PLANT GROUP
!     ULHTRT_EFD = HEATRATES BY CAPACITY TYPE
!     EDOMR  = VARIABLE O & M COSTS BY CAPACITY TYPE
!     EDCR   = CENSUS REGION FOR PLANT GROUP
!     EDGR   = GAS REGION FOR PLANT GROUP
!     EDNR   = NERC REGION FOR PLANT GROUP(rgn of plant owner)
!     EPFUEL = FUEL PRICES
!     EDFLTP = FUEL(S) CONSUMED BY EACH CAPACITY TYPE
!     ENFLTP = NUMBER OF FUEL TYPES
!     EETIME = TIME AXIS SCALE FOR SEASONAL LOAD CURVES
!     ISP    = SEASONAL PERIOD
!     IRG    = EMM REGION
!
!     OUTPUT VARIABLES
!
!     SEE DISPOUT COMMON
!
      SUBROUTINE ELDGNO(ISP,IRG,IYR,ITYPE )
!
      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'dispout'
      include 'dispuse'
      include 'fuelin'
      include 'wrenew'
      include 'wwdcomon'
      include 'dispett'
      include 'ecpcntl'
      include 'postpr'
      include 'uefdout'
      include 'uecpout'
      include 'elout'
      include 'elcntl'
      include 'bildin'
      include 'emablk'
      include 'cdsparms'
      include 'coalemm'
      include 'uso2grp'
      include 'ecp_coal'
      include 'efdin'
      include 'emission'
      include 'dsmdimen'
      include 'dsmtfecp'
      include 'dsmtoefd'
      include 'dsmcaldr'
      include 'csapr'
      include 'emmemis'

      REAL*4    GEN,FUEL,TEMP,TOTGOIL,GENR,FUELR,NOXFAC,HGFAC,ACFAC,ACIFUEL,ACIOM
      REAL*4    EDFSHR(EFD_D_MDG,EFD_D_FPP),EDOMR(EFD_D_MDG,EFD_D_FPP)
      REAL*8    FACTE,VAL
      INTEGER*4 IPGRP,IFLTP,IFP,ISP,I,N,IMCG,NSO2,ISO2,KSCRB,IFLRG,NFLRG,JN
      INTEGER*4 IDGN,ICR,INR,IGR,IFOWN,IECP,J,OPR,IVIN,IECPT
      INTEGER*4 IE,IDUAL,IGRP,IRNK,ISTEP
      INTEGER*4 F,K,IRG,IYR,ITYPE,IBTP,INCT,IPHASE,NOXYR
      INTEGER SLICE
!
      REAL*4    CRBCST(EFD_D_NFL)
      REAL*4    CFSUB

!      FACTE = (2000.0 / 2204.0) / 1000000.0

      FACTE = 1.0 / 1000000.0
      
!     IDENTIFY CO2 PENALTY FOR JEFFORDS RUNS
!
      IF (UPRNWCAS .EQ. 1 .OR. PCAP_CAR .EQ. 1) THEN
         CRBCST(UIDD) = JDSEL(CURIYR)
         CRBCST(UIDG) = JGIELGR(CURIYR)
      ELSE
         CRBCST(UIDD) = 0.0
         CRBCST(UIDG) = 0.0
      END IF
!
!     LOOP ACROSS EACH SOLUTION STEP AND STORE RESULTS
!
      DO 100 N = 1,EDNTP
!
!         N = ECDSPT(I)
         JN = MAP_EDNTP_EFD_GRPS(N,IRG)
         IGRP = EDDBID(N)
         IPGRP = EDASTS(N)
         IDGN = IPGRP - EIPGRP - EIHGRP
         IFOWN = EDFOWN(N)
         ICR = EDCR(N)
         INR = EDNR(N)
         IGR = EDGR(N)
         OPR = EDNR(N)
         IECPT = EDTECP(N)

         STORAGE_RGN(IGRP) = IRG
         STORAGE_ECPn(IGRP) = IECPT
         STORAGE_ECPc(IGRP) = UPLNTCD(IECPT)
!
!         IVIN = ECVIN(N)
!
         IF (IPGRP .GT. 0) THEN
!
!           ADD PLANT GROUP DATA TO RUNNING TOTALS
!
!           CAPACITY
!
              ECAPDS(IDGN,ISP) = ECAPDS(IDGN,ISP) + ECDSPC(IGRP,ISP)
       
            STORAGE_CAP(IGRP,ISP) = EDCAP(N,ISP) * 0.001 * (1.0 - UG_FOR(JN))*(1.0 - UG_PMR(JN))
!
!
!           GENERATION
!
            GEN = ECDSPE(IGRP,ISP)
            GENR = GEN * .001

            STORAGE_GEN(IGRP,ISP) = GENR

            IF (ITYPE .EQ. 1) THEN
               BGENOWN(IRG,IFOWN) = BGENOWN(IRG,IFOWN) + GEN * 0.001
            ELSE
               BTRDOWN(IRG,IFOWN) = BTRDOWN(IRG,IFOWN) + GEN * 0.001
            END IF
            IF(IFOWN.LE.3)ETGEN = ETGEN + GEN
            EQDGN(IDGN,IFOWN) = EQDGN(IDGN,IFOWN) + GEN
            IF(IFOWN.LE.3)EGENDS(IDGN,ISP) = EGENDS(IDGN,ISP) + GEN
            ULGENS(ISP,IGRP) = ULGENS(ISP,IGRP)  + GEN
!
!           SUM UP SO2 and NOX allowance costs for EFP
!
            ERTSO2(IFOWN) = ERTSO2(IFOWN) + GEN * 0.001 *  &
                            ELSO2P(ISP,IGRP)
            ERTNOX(IFOWN) = ERTNOX(IFOWN) + GEN * 0.001 *  &
                            ELNOXP(ISP,IGRP)
!
!           Compute regional mapping ftab variables for generation
!
            IF (FCRL .EQ. 1) THEN
               IF ( IFOWN .EQ. 1 .OR. IFOWN .EQ. 2) THEN
                  UGNINR(OPR,IYR) = UGNINR(OPR,IYR) + GENR
                  UGNINR(MNUMNR,IYR) = UGNINR(MNUMNR,IYR) + GENR

                  IF (INR .NE. OPR) THEN
                     UGNOTR(INR,IYR) = UGNOTR(INR,IYR) + GENR
                     UGNOTR(MNUMNR,IYR) = UGNOTR(MNUMNR,IYR) + GENR

                     UGNSRV(OPR,IYR) = UGNSRV(OPR,IYR) + GENR
                     UGNSRV(MNUMNR,IYR) = UGNSRV(MNUMNR,IYR) + GENR
                  ENDIF

               ENDIF
            ENDIF                                   !Last iteration loop
!
!           ACCUM FUEL CONSUMPTION, FUEL COST, OM COST
!
!           IDENTIFY IF DUAL-FIRED PLANT (USES COMPETITIVE GAS)
!
!            IDUAL = 0
!            TOTGOIL = 0.0
!            DO IFP = 1 , EIFPLT
!               IF (ECFLTP(N,IFP) .EQ. UIGC .AND. &
!                   ECMFSH(N,IFP) .GT. 0.0) IDUAL = 1
!            END DO
!
!           LOOP OVER FUEL TYPES IN PLANT GROUP
!
!            DO 20 IFP = 1 , EIFPLT

            STORAGE_CST(IGRP,ISP) = 0.0

            DO 20 IFP = 1 , 1
                  EDFSHR(N,IFP) = 1.0
                  EDOMR(N,IFP) = EDVOMR(N,IFP)
               IF (EDFSHR(N,IFP) .GT. 0.0) THEN
                  IFLTP = EDFLTP(N,IFP)

                  FUEL = ECDSPF(IGRP,ISP)

                  IF(IFOWN.LE.3)EQDFL(IDGN) = EQDFL(IDGN) + FUEL
                  EQFFL(IFLTP,IFOWN) = EQFFL(IFLTP,IFOWN) + FUEL
                  EQPFLCN(IPGRP,IFOWN,IFLTP) =  &
                         EQPFLCN(IPGRP,IFOWN,IFLTP) + FUEL
                  EQPFLGN(IPGRP,IFOWN,IFLTP) =  &
                         EQPFLGN(IPGRP,IFOWN,IFLTP) + &
                            (GEN * EDFSHR(N,IFP))

!  ACCOUNT FOR SUBSIDY, IF ANY, FOR BIOMASS COFIRING (CONVERT FROM MILLS)
               IF (IPGRP .NE. UIBMS .AND. IFLTP .EQ. UIWD)THEN
                   CFSUB = WDCFSUB(CURIYR) / (ULHTRT_EFD(IGRP,ISP) * 0.001)
               ELSE
                   CFSUB = 0.0
               END IF
!
!                 Compute regional mapping ftab variables for consumption(TABLE 62)
!
                  FUELR = FUEL * .000001
                  IF((IFOWN .EQ. 1 .OR. IFOWN .EQ. 2) .AND.  &
                     FCRL .EQ. 1) THEN
                     UFLINR(OPR,IYR) = UFLINR(OPR,IYR) + FUELR
                     UFLINR(MNUMNR,IYR) = UFLINR(MNUMNR,IYR) + FUELR
                     IF(INR .NE. OPR) THEN
                        UFLOTR(INR,IYR) = UFLOTR(INR,IYR) + FUELR
                        UFLOTR(MNUMNR,IYR) = UFLOTR(MNUMNR,IYR) + &
                           FUELR
                        UFLSRV(OPR,IYR) = UFLSRV(OPR,IYR) + FUELR
                        UFLSRV(MNUMNR,IYR) = UFLSRV(MNUMNR,IYR) + &
                           FUELR
                     ENDIF
                  ENDIF
!
                  DO NFLRG = 1 , UNFLRG(IFLTP)
                     IFLRG = EDFLRG(N,IFP,NFLRG)
                     IF (IFLRG .LE. 0) THEN
                        IFLRG = EFD_D_MFRG
                        PRINT *,' NO FUEL REGION ',N,IFP,NFLRG
                     ENDIF
                     IF (IFOWN .LE. 4) THEN
                        UQFUEL(IFLTP,IFLRG,NFLRG) = UQFUEL(IFLTP,IFLRG,NFLRG) + FUEL
                        IF(NFLRG .EQ. 1)SQFUEL(IFLTP,IFLRG,ISP) = SQFUEL(IFLTP,IFLRG,ISP) + FUEL
                     END IF
                  END DO
!
                  IFLRG = EDFLRG(N,IFP,1)
!
!                 CALCULATE ADDERS FOR ACTIVATED CARBON INJECTION - FUEL AND O&M
!
                  ACIFUEL = 0.0
                  ACIOM = 0.0
!
!                 SUMPRODUCT OF FUEL CONSUMPTION and SO2 REMOVAL RATE
!
                 IF(IFOWN.LE.3)THEN
!                  KSCRB = ECSCRB(N)
!                  URFUEL(IFLTP,IFLRG) = URFUEL(IFLTP,IFLRG) +  &
!                     FUEL * REAL(KSCRB) * EFACTR
!                  UXFUEL(IFLTP,IFLRG) = UXFUEL(IFLTP,IFLRG) +  &
!                     FUEL * ECSEQS(N)
                  EPFLRG(IFLTP,IFLRG) = EPFLRG(IFLTP,IFLRG) + FUEL
                  EPCRMP(IFLTP,ICR) = EPCRMP(IFLTP,ICR) + FUEL
                  IECP = UFL_D_ECP(IFLTP,1)
                  J = 1
                  DO WHILE (IECP .GT. 0)
                     EPFMAP(IECP,IFLTP) = EPFMAP(IECP,IFLTP) + FUEL
                     J = J + 1
                     IF (J .LE. ECP_D_FPP) THEN
                        IECP = UFL_D_ECP(IFLTP,J)
                     ELSE
                        IECP = 0
                     END IF
                  END DO
                 END IF
                 IF (IFLTP .ne. ULFUEL(IFP,IGRP)) THEN
                       write(*,*)'fuel mismatch ifltp,ulfuel,igrp',ifltp, &
        ulfuel(ifp,igrp)     ,igrp
                 ENDIF
                  UQFGENN(IFLTP,INR,IFOWN) =  &
                     UQFGENN(IFLTP,INR,IFOWN) + &
                     GEN * EDFSHR(N,IFP)
                  UQFCONN(IFLTP,INR,IFOWN) =  &
                     UQFCONN(IFLTP,INR,IFOWN) + FUEL
                  UQPGENN(IPGRP,INR,IFOWN) =  &
                     UQPGENN(IPGRP,INR,IFOWN) + GEN * EDFSHR(N,IFP)
                 IF(IFOWN.LE.3)THEN
                  UQFGENC(IFLTP,ICR) = UQFGENC(IFLTP,ICR) + &
                     GEN * EDFSHR(N,IFP)
                     WRITE(22,*)'uqfgenc2 ', CURIYR,IFLTP,ICR,N,IFP,UQFGENC(IFLTP,ICR),  GEN, EDFSHR(N,IFP)
                  UQPGENC(IPGRP,ICR) = UQPGENC(IPGRP,ICR) + &
                     GEN * EDFSHR(N,IFP)
                 END IF
                  EQFGN(IFLTP,IFOWN) = EQFGN(IFLTP,IFOWN) + &
                     GEN * EDFSHR(N,IFP)
                  ERTFL(IFOWN) = ERTFL(IFOWN) + FUEL * &
                     (UPFUEL(IFLTP,IFLRG) + ACIFUEL - CRBCST(IFLTP) - CFSUB) * &
                      0.001
                  ERTOM(IFOWN) = ERTOM(IFOWN) + GEN * EDFSHR(N,IFP) * &
                     (EDOMR(N,IFP) + ACIOM) * 0.001
                  ERDOM(IDGN,IFOWN) = ERDOM(IDGN,IFOWN) + GEN *  &
                     EDFSHR(N,IFP) * (EDOMR(N,IFP) + ACIOM) * 0.001
                  TEMP = UPFUEL(IFLTP,IFLRG)
                  ERDFL(IDGN,IFOWN) = ERDFL(IDGN,IFOWN) + FUEL * &
                     (UPFUEL(IFLTP,IFLRG) + ACIFUEL - CRBCST(IFLTP) - CFSUB) * &
                      0.001
                  IF(IFOWN.LE.4)ERFFL(IFLTP) = ERFFL(IFLTP) + FUEL * &
                     (UPFUEL(IFLTP,IFLRG) + ACIFUEL - CRBCST(IFLTP) - CFSUB) * &
                      0.001
                  ULFLCST(IFP,IGRP) =  ULFLCST(IFP,IGRP) + FUEL * &
                     (UPFUEL(IFLTP,IFLRG) + ACIFUEL - CRBCST(IFLTP) - CFSUB) * &
                      0.001

!                 STORAGE_CST(IGRP,ISP) = STORAGE_CST(IGRP,ISP) + EDFSHR(N,IFP) * 0.001 * ((ULHTRT_EFD(IGRP,ISP) * (UPFUEL(IFLTP,IFLRG) + ACIFUEL - CFSUB)) + EDOMR(N,IFP) + ACIOM)

                  STORAGE_CST(IGRP,ISP) = STORAGE_CST(IGRP,ISP) + EDFSHR(N,IFP) * (0.001 * (ULHTRT_EFD(IGRP,ISP) * (UPFUEL(IFLTP,IFLRG) + ACIFUEL - CFSUB)) + EDOMR(N,IFP) + ACIOM + ELRPSP(ISP,IGRP))

                  IF (FCRL .EQ. 1) THEN
                     WRITE(18,3881) CURIRUN, CURIYR+1989, CURITR, FCRL, IRG, ISP, IFP, IFLTP, N, JN, IGRP, IFLRG, IECPt, &
                         STORAGE_CAP(IGRP,ISP), STORAGE_CST(IGRP,ISP), EDFSHR(N,IFP), ULHTRT_EFD(IGRP,ISP), UPFUEL(IFLTP,IFLRG), 0.0, ACIFUEL, CFSUB, 0.0, 0.0, &
                         EDOMR(N,IFP), ACIOM, EDCAP(N,ISP) * 0.001, UG_FOR(Jn),UG_PMR(JN),ELRPSP(ISP,IGRP)
 3881               FORMAT(1X,"STORAGE_CST_UEFD",13(",",I6),16(",",F21.8))
                  END IF
                  STORAGE_CST(IGRP,ISP) = MAX(0.0,STORAGE_CST(IGRP,ISP))

!                 EMISSIONS ACCOUNTING
                 IF(IFOWN.LE.3)THEN
!
!                 DETERMINE NOX EMISSION RATE
!
                  NOXFAC = EDFNOX(N,ISP)
!
!                 DETERMINE HG EMISSION AND REMOVAL RATE
!
                  HGFAC = UFRHG(IFLTP,IFLRG)
!
!                 1. BY EFD DISPATCHABLE CAPACITY TYPE
!
!                  KSCRB = ECSCRB(N)
                  EQDSO2(IDGN) = EQDSO2(IDGN) + FUEL * &
                     UFRSO2R(IFLTP,IFLRG) *  &
                     (1.0 - UFRASH(IFLTP,IFLRG)) * &
                     0.5
                  EQDNOX(IDGN) = EQDNOX(IDGN) + FUEL * &
                     NOXFAC * 0.5
                  EQDCO2(IDGN) = EQDCO2(IDGN) + FUEL * &
                     UFRCO2(IFLTP,IFLRG) * 0.5
                  EQDHG(IDGN) = EQDHG(IDGN) + FUEL * &
                     HGFAC * 0.5 * 0.000001
!
!                 2. BY FUEL TYPE
!
                  EQFSO2(IFLTP) = EQFSO2(IFLTP) + FUEL * &
                     UFRSO2R(IFLTP,IFLRG) * &
                     (1.0 - UFRASH(IFLTP,IFLRG)) * &
                      0.5
                  UQFSO2(IFLTP,IRG) = UQFSO2(IFLTP,IRG) + FUEL * &
                     UFRSO2R(IFLTP,IFLRG) * 0.5
                  EQFNOX(IFLTP) = EQFNOX(IFLTP) + FUEL * &
                     NOXFAC * 0.5
                  EQFCO2(IFLTP) = EQFCO2(IFLTP) + FUEL * &
                     UFRCO2(IFLTP,IFLRG) * 0.5
                  EQFHG(IFLTP) = EQFHG(IFLTP) + FUEL * &
                     HGFAC * 0.5 * 0.000001
                  EFHCNT(IFLTP) = EFHCNT(IFLTP) + FUEL * &
                     UFHCNT(IFLTP,IFLRG)
                  ETSO2 = ETSO2 + FUEL * &
                     UFRSO2R(IFLTP,IFLRG) * &
                     (1.0 - UFRASH(IFLTP,IFLRG)) * &
                     0.5
!
!                  3. IN TOTAL
!
                  ETNOX = ETNOX + FUEL * NOXFAC * 0.5
                  ETCO2 = ETCO2 + FUEL * UFRCO2(IFLTP,IFLRG) * &
                          0.5
                  ETCAR = ETCAR + FUEL * UFRCAR(IFLTP,IFLRG) * &
                          0.5
                  ETCO1 = ETCO1 + FUEL * UFRCO1(IFLTP,IFLRG) *  &
                          0.5
                  ETVOC = ETVOC + FUEL * UFRVOC(IFLTP) * 0.5
                  ETHG = ETHG + FUEL * HGFAC * 0.5 * 0.000001
!
!                 3.5 BY REGIONAL MAPPING FTAB VARIABLES FOR TABLE 62
!
!                 EMISSIONS TOTALS ARE COMPUTED IN EMMDSPO AFTER THE
!                 RENEWABLES ARE ADDED TO THE INR VARIABLES
!                 FACTE= CONVERSION FACTOR FROM SHORT TONS TO MILLION METRIC TONS

                  IF (INR .EQ. OPR) THEN
                     USO2INR(OPR,IYR) = USO2INR(OPR,IYR) + ( FUEL * &
                        UFRSO2R(IFLTP,IFLRG) *  &
                        (1.0 - UFRASH(IFLTP,IFLRG)) * &
                        0.5 ) * FACTE
                     UCO2INR(OPR,IYR) = UCO2INR(OPR,IYR) + &
                        ( FUEL * UFRCO2(IFLTP,IFLRG) * &
                        0.5 ) * FACTE
                     UCARINR(OPR,IYR) = UCARINR(OPR,IYR) + &
                        ( FUEL * UFRCAR(IFLTP,IFLRG) * &
                        0.5 ) * FACTE
                     UNOXINR(OPR,IYR) = UNOXINR(OPR,IYR) + &
                        ( FUEL * NOXFAC * 0.5 ) / 1000000.0
                     UHGINR(OPR,IYR) = UHGINR(OPR,IYR) + &
                        ( FUEL * HGFAC * 0.5 )
                  ENDIF
                  IF (INR .NE. OPR) THEN
                     USO2OTR(INR,IYR) = USO2OTR(INR,IYR) + ( FUEL * &
                        UFRSO2R(IFLTP,IFLRG) *  &
                        (1.0 - UFRASH(IFLTP,IFLRG)) * &
                        0.5 )* FACTE
                     UCO2OTR(INR,IYR) = UCO2OTR(INR,IYR) + &
                        ( FUEL * UFRCO2(IFLTP,IFLRG) * &
                        0.5 ) * FACTE
                     UCAROTR(INR,IYR) = UCAROTR(INR,IYR) + &
                        ( FUEL * UFRCAR(IFLTP,IFLRG) * &
                        0.5 ) * FACTE
                     UNOXOTR(INR,IYR) = UNOXOTR(INR,IYR) + &
                        ( FUEL * NOXFAC * 0.5 ) / 1000000.0
                     UHGOTR(INR,IYR) = UHGOTR(INR,IYR) + &
                        ( FUEL * HGFAC * 0.5 )
                  ENDIF
!
                  UTSO2C(IFLTP,ICR) = UTSO2C(IFLTP,ICR) + FUEL * &
                     UFRSO2R(IFLTP,IFLRG) * &
                     (1.0 - UFRASH(IFLTP,IFLRG)) * &
                     0.5
                  UTNOXC(IFLTP,ICR) = UTNOXC(IFLTP,ICR) + FUEL * &
                     NOXFAC * 0.5
                  UTCO2C(IFLTP,ICR) = UTCO2C(IFLTP,ICR) + FUEL * &
                     UFRCO2(IFLTP,IFLRG) * 0.5
                  UTCARC(IFLTP,ICR) = UTCARC(IFLTP,ICR) + FUEL * &
                     UFRCAR(IFLTP,IFLRG) * 0.5
                  UTCO1C(IFLTP,ICR) = UTCO1C(IFLTP,ICR) + FUEL * &
                     UFRCO1(IFLTP,IFLRG) * 0.5
                  UTVOCC(IFLTP,ICR) = UTVOCC(IFLTP,ICR) + FUEL * &
                     UFRVOC(IFLTP) * 0.5
                  UTHGC(IFLTP,ICR) = UTHGC(IFLTP,ICR) + FUEL * &
                     HGFAC * 0.5 * 0.000001
!
!                 5. BY FUEL TYPE AND EMM REGION
!
                  UTSO2N(IFLTP,INR) = UTSO2N(IFLTP,INR) + FUEL * &
                     UFRSO2R(IFLTP,IFLRG) * &
                     (1.0 - UFRASH(IFLTP,IFLRG)) * &
                     0.5
                  UTNOXN(IFLTP,INR) = UTNOXN(IFLTP,INR) + FUEL * &
                     NOXFAC * 0.5
                  UTCO2N(IFLTP,INR) = UTCO2N(IFLTP,INR) + FUEL * &
                     UFRCO2(IFLTP,IFLRG) * 0.5
                  UTCARN(IFLTP,INR) = UTCARN(IFLTP,INR) + FUEL * &
                     UFRCAR(IFLTP,IFLRG) * 0.5
                  UTCO1N(IFLTP,INR) = UTCO1N(IFLTP,INR) + FUEL * &
                     UFRCO1(IFLTP,IFLRG) * 0.5
                  UTHGN(IFLTP,INR) = UTHGN(IFLTP,INR) + FUEL * &
                     HGFAC * 0.5 * 0.000001
!
!                 6. BY SO2 COMPLIANCE GROUP
!
                  IF (ULHGGP(IGRP) .LE. 0 .OR. ULHGGP(IGRP) .GT. 14) THEN
!jj                  ISO2 = 1
                     NSO2 = 0
                  ELSE
!jj                  ISO2 = SO2_GRP_BY_CLRG(ULHGGP(IGRP))
                     NSO2 = NUM_SO2_GRP
                  END IF
!                 IF (ISO2 .GT. 0) THEN
                  IF (NSO2 .GT. 0) THEN
                   DO ISO2 = 1 , NSO2
                    IF (SO2_SHR_BY_CLRG(ULHGGP(IGRP),ISO2) .GT. 0.0)THEN
                     EGSO2(ISO2) = EGSO2(ISO2) + FUEL * &
                        UFRSO2R(IFLTP,IFLRG) * SO2_SHR_BY_CLRG(ULHGGP(IGRP),ISO2) *  &
                        ( 1.0 - UFRASH(IFLTP,IFLRG)) * &
                        0.5
                    END IF
                   END DO
                  ELSE
                     ISO2 = 1
                     EGSO2(ISO2) = EGSO2(ISO2) + FUEL * &
                        UFRSO2R(IFLTP,IFLRG) *  &
                        ( 1.0 - UFRASH(IFLTP,IFLRG)) * &
                        0.5
                  END IF
                  END IF                               ! IF IFOWN LE 3
!
!                 STORE ENERGY IN OUTPUT ARRAYS
!
                  ULGENE(IFP,IGRP) = ULGENE(IFP,IGRP) +  &
                     GEN * EDFSHR(N,IFP)
                  ULBTUE(IFP,IGRP) = ULBTUE(IFP,IGRP) + FUEL
                  ULSO2W(IFP,IGRP) = ULSO2W(IFP,IGRP) +  &
                     FUEL * UFRSO2R(IFLTP,IFLRG) *  &
                     (1.0 - UFRASH(IFLTP,IFLRG)) * &
                     0.5
                  ULNOXW(IFP,IGRP) = ULNOXW(IFP,IGRP) +  &
                    FUEL * NOXFAC * 0.5
                  ULCO2W(IFP,IGRP) = ULCO2W(IFP,IGRP) +  &
                    FUEL * UFRCO2(IFLTP,IFLRG) * 0.5
                  ULCARW(IFP,IGRP) = ULCARW(IFP,IGRP) +  &
                    FUEL * UFRCAR(IFLTP,IFLRG) * 0.5
                  VAL = FUEL * HGFAC * 0.5 * 0.000001
                  IF (ISNAN(VAL).OR. ABS(VAL) .GT. HUGE(VAL)) THEN   ! check for NaNQ this way
                     WRITE(6,9724) CURIYR+1989,CURITR,IRG,IGRP,IFP,FUEL,HGFAC,VAL
 9724                FORMAT(1X,"ULHGQ_OOPS",5(":",I6),3(":",F15.3))
                  ELSE
                     ULHGQ(IFP,IGRP) = ULHGQ(IFP,IGRP) +  FUEL * HGFAC * 0.5 * 0.000001
                  ENDIF                                      !
!
               END IF                                 ! IF EDFSHR .EQ. 0
   20       CONTINUE                                ! END FUEL TYPE LOOP
         END IF
  100 CONTINUE                               ! END HORIZONTAL SLICE LOOP
!
      RETURN
      END
!
!     ==================================================================
!     - ELMRIT -
!     THIS SUBROUTINE DETERMINES THE MERIT ORDER FOR DISPATCHING
!     DECISIONS BY LISTING EQUIPMENT TYPES IN ORDER OF INCREASING
!     OPERATING COSTS.
!     ==================================================================
!
!     INPUT VARIABLES
!     ULHTRT_EFD = HEATRATES BY CAPACITY TYPE AND UTILIZATION
!     ECOMR  = VARIABLE O & M COSTS BY CAPACITY TYPE
!     EPFUEL = FUEL PRICES
!     ECFLTP = FUEL(S) CONSUMED BY EACH CAPACITY TYPE
!     ECFSHR = FUEL SHARES BETWEEN PRIMARY AND SECONDARY FUELS
!     ENFLTP = NUMBER OF FUEL TYPES
!     ECNTP  = NUMBER OF CAPACITY TYPES
!     OUTPUT VARIABLES
!     ECTYP  = CAPACITY TYPE INDEX IN ORDER OF INCREASING OPERATING
!     COSTS
!
      SUBROUTINE ELMRIT(ISP,IRG)
!
      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'dispout'
      include 'dispuse'
      include 'fuelin'
      include 'wrenew'
      include 'wwdcomon'
      include 'dispett'
      include 'ecpcntl'
      include 'bildout'
      include 'uecpout'
      include 'elout'
      include 'dsmdimen'
      include 'dsmtoefd'
      include 'angtdm'
      include 'emission'
      include 'eusprc'
      include 'efpint'
      include 'efdin'
      include 'cdsparms'
      include 'coalemm'
      include 'uso2grp'
      include 'ecp_coal'
      include 'coalprc'
      include 'csapr'
      include 'emmemis'
      include 'ecp_nuc'
      include 'emm_aimms'
!
      REAL UNTCST(EFD_D_MHS),PSO2,PNOX(NOX_D_GRP),PRPS,BRPS,SRPS,SPRPS,SBRPS,SSRPS,PHG
      REAL ZOMR,ZSHR,ZCFL,ZSEQ,ZSCRB,ZSO2,ZFAC,TEMP,ZHR,ZNOX(NOX_D_GRP),ZHG
      REAL*8 CST_OM,CST_FL,CST_SO2,CST_TOT,T_SO2,CST_NOX,T_NOX,T_RPS,CST_RPS,CST_RPS_ST
      REAL*8 CST_HG,T_HG,CST_PTC, HEATRATE
      INTEGER INDEX(EFD_D_MHS),ISP,KSCRB,I,J,NSO2,ISO2,IFUEL,ILST,JFRST,ITEMP,IECP,FULLYR
      INTEGER IPTYP,IFLRG,IRG,MNEXT,INEXT,IGRP,JSP,IRNK,IPGRP,HGGRP,ISTEP,INOX

!
!     CALCULATE FUEL SHARES
!
      FULLYR = USYEAR(CURIYR)
      IF (FULLYR .EQ. UESTYR .AND. CURITR .EQ. 1) THEN
      CALL ELFSHR(ISP,IRG)
      ENDIF
!
!     CALCULATE UNIT COSTS FOR EACH EQUIPMENT TYPE
!
      DO INOX = 1 , NOX_GRP
         PNOX(INOX) = EPNOXPR(INOX,CURIYR) * 0.5
      END DO
!
      DO I = 1 , ECNTP
         IPGRP = ECDBID(I)
         IPTYP = ECASTS(I)                                 ! DEBUG
         IF (ULHGGP(ipgrp) .EQ. 0) THEN
            HGGRP = 1
!jj         ISO2 = 1
            NSO2 = 0
         ELSE
            HGGRP = HG_GRP(ULHGGP(ipgrp))
!jj         ISO2 = SO2_GRP_BY_CLRG(ULHGGP(IPGRP))
            NSO2 = NUM_SO2_GRP
         END IF
         IECP = ECTECP(I)
         UNTCST(I) = 0.0
         ECVCST(I) = 0.0
         T_SO2 = 0.0
         T_NOX = 0.0
         T_RPS = 0.0
         T_HG  = 0.0
!jj      PSO2 = EMELPSO2(CURIYR,ISO2) * 0.5
         PSO2 = 0.0
         IF ((CURIYR + UHBSYR) .LT. TSO2_YR_BY_CLRG)THEN
            IF (NSO2 .GT. 0)THEN
               DO ISO2 = 1 , NSO2
                  IF (SO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) .GT. 0.0)THEN
!                    PSO2 = PSO2 + EMELPSO2(CURIYR,ISO2) * SO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) * 0.5
                     PSO2 = PSO2 + ECP_PSO2(0,CURIYR,ISO2) * SO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) * 0.5
                  END IF
               END DO
            ELSE
               ISO2 = 1
!              PSO2 = EMELPSO2(CURIYR,ISO2) * 0.5
               PSO2 = ECP_PSO2(0,CURIYR,ISO2) * 0.5
            END IF
         ELSE
            DO ISO2 = 1 , NSO2 - 1
               IF (TSO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) .GT. 0.0)THEN
!                 PSO2 = PSO2 + EMELPSO2(CURIYR,ISO2) * SO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) * 0.5
                  PSO2 = PSO2 + ECP_PSO2CL(CURIYR,ULHGGP(IPGRP)) * TSO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) * 0.5
!     write(6,3333) curiyr+1989,curitr,ulhggp(ipgrp),iso2,  &
!                    ECP_PSO2CL(CURIYR,ULHGGP(IPGRP)) , TSO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) , pso2
!3333 format(1h ,'!pso2efd',i4,i3,i3,i3,3f10.3)
               END IF
            END DO
            DO ISO2 = NSO2 , NSO2
               IF (TSO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) .GT. 0.0)THEN
!                 PSO2 = PSO2 + EMELPSO2(CURIYR,ISO2) * SO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) * 0.5
                  PSO2 = PSO2 + ECP_PSO2(0,CURIYR,ISO2) * TSO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) * 0.5
               END IF
            END DO
         END IF
         PHG = EMEL_PHG(HGGRP,CURIYR) * 0.0005
         IF (FULLYR .eq. UESTYR) THEN
            PRPS = 0.0
         ELSEIF (CURITR .EQ. 1) THEN
            PRPS = RENEWCRR(IRG,CURIYR - 1)
         ELSE
            PRPS = RENEWCRR(IRG,CURIYR)
         ENDIF
            BRPS = MAX(UPRNWBNDR(CURIYR,IRG),UPRNWBND(CURIYR))

         IF (ST_RPS_SW .GT. 0) THEN
            SPRPS = ST_RPS_EMM_P(IRG,CURIYR)
            SBRPS = ST_RNW_BND(CURIYR,IRG)
         END IF

         DO J = 1 , EIFPLT

!           update fuel shares with new LP versions

            IF (FULLYR .GT. UESTYR .OR. CURITR .GT. 1) THEN
               IF (ISP .LE. EENSP) ECFSHR(I,J) = ELFLSH(ISP,IPGRP,J)
            ENDIF
            IFUEL = ECFLTP(I,J)
            IFLRG = ECFLRG(I,J,1)
            ZSHR = ECFSHR(I,J)
            IF ((ZSHR .GT. 0.000000001) .AND. (IFUEL .GT. 0)) THEN
               KSCRB = ECSCRB(I)
               IPTYP = ECASTS(I)                                 ! DEBUG
               IPGRP = ECDBID(I)
               ZOMR = ECOMR(I,J)                                 ! DEBUG
               
               IF (IFUEL .EQ. UIDS) THEN
                  ZHR = ULHTRT_EFD(IPGRP,ISP) * 1.10
                  HEATRATE = ULHTRT_EFD(IPGRP,ISP) * 1.10
               ELSE
                  ZHR = ULHTRT_EFD(IPGRP,ISP)
                  HEATRATE = ULHTRT_EFD(IPGRP,ISP)
               END IF
               
               ZSCRB = REAL(KSCRB)                               ! DEBUG
               ZFAC = EFACTR                                     ! DEBUG
               DO INOX = 1 , NOX_GRP
                  IF (ISP .LE. EENSP) THEN
                     ZNOX(INOX) = ECFNOXC(INOX,I,ISP)            ! ONLY AMT COUNTING IN CAP
                  ELSE
                     ZNOX(INOX) = 0.0
                     DO JSP = 1 , EENSP
                        ZNOX(INOX) = ZNOX(INOX) + ECFNOXC(INOX,I,JSP)
                     END DO
                     ZNOX(INOX) = ZNOX(INOX) / DBLE(EENSP)
                  END IF
               END DO
               ZHG = 0.0
               IF (IFUEL .LE. UIIS) THEN
                  ZHG = PHGCLNR(IFLRG,CURIYR,IFUEL)
               ELSE
                  ZHG = UFRHG(IFUEL,IFLRG) * PHG
               END IF
               IF(IFUEL .GT. 0) THEN
                  ZCFL = UPFUEL(IFUEL,IFLRG) 
                  IF (IFUEL .EQ. UIGF .OR. IFUEL .EQ. UIGI) THEN              !one gas price for all types
                    IF (ISP .LE. EENSP) THEN
                       ZCFL = SPNGELGR(IFLRG,CURIYR,ISP)                                            ! gas seas same as EMM seas
                    ENDIF
                  ENDIF

                  IF (IFUEL .LE. UIIS) THEN
                     ZSO2 = PSLCLNR(IFLRG,CURIYR,IFUEL)
                  ELSE
                     ZSO2 = PSO2* UFRSO2R(IFUEL,IFLRG) * 0.001 * (1.0 - REAL(KSCRB) * EFACTR)
                  END IF
                  IF(ZCFL.GT.0.0 .AND. UPNCAR(IFUEL,IFLRG).GE.ZCFL)THEN

!                    INSURE THAT CARBON PENALTY COST DOES NOT EXCEED TOTAL FUEL COST

                     WRITE(6,2424) CURIYR+1989,CURITR,IFUEL,IFLRG,UPNCAR(IFUEL,IFLRG),ZCFL,ZCFL*0.95
 2424                FORMAT(1H ,'HI CRB',' YR = ',I4,' IT = ',I2,' FL = ',I2, ' RG = ',I2,' PN = ',F7.3,' PR = ',F7.3,' NWPN = ',F7.3)

                     UPNCAR(IFUEL,IFLRG) = 0.95 * ZCFL
                  END IF

!                 DETERMINE ADJUSTMENT TO CARBON TAX, IF ANY, FOR SEQUESTRATION

                  ZSEQ = UPNCAR(IFUEL,IFLRG) * ECSEQS(I)
               ELSE
                  ZCFL = 99.999
                  ZSO2 = 9.999
                  ZSEQ = 0.00
               ENDIF

!              IF (ZSEQ .GT. 0)PRINT *,'ZSEQ',UPNCAR(IFUEL,IFLRG),ECSEQS(I)

               CST_OM = ECOMR(I,J)

!              ADD DSI VOM, IF APPROPRIATE
!              IF ((CURIYR + UHBSYR) .GE. UDSI_YR .AND. IECP .LT. WIPC)THEN
!                 IF (ECP_SCRUB(IECP,CURIYR) .EQ. 2)THEN
!                    IF (UPLNTCD(IECP)(1:1) .EQ. 'B')THEN
!                       CST_OM = CST_OM + UPDSIVOM(1)
!                    ELSE
!                       CST_OM = CST_OM + UPDSIVOM(2)
!                    END IF
!                 END IF
!              END IF

               CST_FL = (ZCFL - ZSEQ) * HEATRATE * 0.001

!              IF SUBSIDY FOR BIOMASS COFIRING, ADJUST FUEL COST
!              ALSO ACCOUNT FOR RPS CREDIT PRICE, IF APPROPRIATE ** don't do with RPS constraint

               IF (IPTYP .NE. UIBMS .AND. IFUEL .EQ. UIWD)THEN
                  CST_FL = CST_FL - WDCFSUB(CURIYR) + WDCFHUR(CURIYR)

!                       - EPRPSPR(CURIYR)   ! do this if not using RPS EFD constraint

               END IF
               CST_SO2 = ZSO2 * 0.001 * HEATRATE
               CST_NOX = 0.0
               DO INOX = 1 , NOX_GRP
                  CST_NOX = CST_NOX + PNOX(INOX) * ZNOX(INOX) * 0.000001 * HEATRATE
               END DO
               CST_HG = ZHG * 0.001 * HEATRATE

               SRPS = MAX( UPRNWSHR(IECP) , UPRNWSHRR(IECP,IRG) )
               IF (ST_RPS_SW .GT. 0) THEN
                  SSRPS = ST_RNW_SHR(IECP,IRG,CURIYR)
               END IF
               
               CST_RPS = PRPS * (BRPS - SRPS)      ! avg rev rec'd for RPS credits from national/regional 
               
               IF (ST_RPS_SW .GT. 0) THEN
                  CST_RPS_ST = SPRPS * (SBRPS - SSRPS)
                 IF (CST_RPS .LT. 0.0 .AND. CST_RPS_ST .LE. 0.0) THEN  ! if both are negative use the greatest benefit
                  IF (ABS(CST_RPS) .LT. ABS(CST_RPS_ST)) CST_RPS = CST_RPS_ST
                 ELSEIF (CST_RPS .GT. 0.0 .AND. CST_RPS_ST .GT. 0.0) THEN     ! if both are positive, use the highest positive cost
                  IF (CST_RPS .LT. CST_RPS_ST) CST_RPS = CST_RPS_ST
                 ELSE                        ! use net value
                    CST_RPS = CST_RPS + CST_RPS_ST
                 ENDIF                  
               END IF
               
               IF (UPRNWREG .EQ. 3 .AND. IECP .EQ. WICN .AND. UPRNWSHR(IECP) .LE. 0.0) CST_RPS = 0.0
               CST_PTC = ECGSUB(I)
               CST_TOT = CST_OM + CST_FL + CST_SO2 + CST_NOX + CST_RPS + CST_HG - CST_PTC
               UNTCST(I) = UNTCST(I) + ECFSHR(I,J) * CST_TOT
               ECVCST(I) = ECVCST(I) + (CST_OM + CST_FL - CST_PTC) * ECFSHR(I,J)
               T_SO2 = T_SO2 + CST_SO2 * ECFSHR(I,J)
               T_NOX = T_NOX + CST_NOX * ECFSHR(I,J)
               T_RPS = T_RPS + CST_RPS * ECFSHR(I,J)
               T_HG = T_HG + CST_HG * ECFSHR(I,J)
            END IF
         END DO   ! END LOOP ON J (EIFPLT) NUMBER OF FUELS PER PLANT GROUP

!        adjust unit cost for allowance allocations

         UNTCST(I) = UNTCST(I) - GPSCSUB(IPTYP,IRG) - GPSSSUB(IPTYP,IRG) - GPSNSUB(IPTYP,IRG) - GPSHSUB(IPTYP,IRG)
!
!        INITIALIZE MERIT ORDER
!
         INDEX(I) = I
!
!        CAPTURE UNIT COST VECTOR FOR ECONOMY TRADE MODULE
!
         IF (ISP .LE. EENSP) THEN
            IGRP = ECDBID(I)
            ULCSTR(ISP,IGRP) = ECVCST(I) - GPSCSUB(IPTYP,IRG)
            ELSO2P(ISP,IGRP) = T_SO2 - GPSSSUB(IPTYP,IRG)
            ELNOXP(ISP,IGRP) = T_NOX - GPSNSUB(IPTYP,IRG)
            ELRPSP(ISP,IGRP) = T_RPS

 !         IF (CURIYR+1989 .GE. 2020 .AND. CURIYR+1989 .LE. 2040 .AND. FCRL .EQ. 1) THEN
 !            WRITE(13,9397) CURIRUN, CURIYR+1989, CURITR, IRG, IGRP, ISP, I, IECP, ST_RPS_SW, UPRNWREG, &
 !               ELRPSP(ISP,IGRP), PRPS, UPRNWSHR(IECP), BRPS, RENEWCRR(IRG,CURIYR), UPRNWBNDR(CURIYR,IRG), UPRNWBND(CURIYR), ST_RPS_EMM_P(IRG,CURIYR), ST_RNW_BND(CURIYR,IRG), &
 !               SRPS, ST_RNW_SHR(IECP,IRG,CURIYR), CST_RPS, T_RPS
!9397         FORMAT(1X,"ELMRIT_ELRPSP ",10(":",I6),13(":",F21.6))
!          END IF

            ELHGP(ISP,IGRP) = T_HG - GPSHSUB(IPTYP,IRG)
         END IF
!
         END DO           ! END LOOP ON I (ECNTP) NUMBER OF PLANT GROUPS
!
!        SORT BY UNIT COSTS AND CAPTURE EQUIPMENT TYPE INDEX IN ORDER OF
!        INCREASING UNIT COSTS
!
         CALL EMSORT(ECNTP,UNTCST,INDEX)
!
!        MOVE MUST RUN UNITS TO FRONT OF MERIT ORDER AND STORE RESULTS
!
         MNEXT = 0
         INEXT = ECNMR
         DO I = 1 , ECNTP
            J = INDEX(I)
         IF (ECMRUN(J) .EQ. 1) THEN
            MNEXT = MNEXT + 1
            ECTYP(irg,MNEXT) = J
!           IF(IRG.EQ.2) write(22,*)'MNEXT',MNEXT
         ELSE
            INEXT = INEXT + 1
            ECTYP(irg,INEXT) = J
!           IF(IRG.EQ.2) write(22,*)'INEXT',INEXT
         END IF
      END DO
!
      RETURN
      END
!
!     ==================================================================
!     - ELFSHR -
!     THIS SUBROUTINE DETERMINES FUEL SHARES FOR ALL CAPACITY TYPES AS A
!     FUNCTION OF FUEL PRICES AND MAXIMUM ALLOWED FUEL SHARES
!     ==================================================================
!
!     INPUT VARIABLES
!     EPFUEL = FUEL PRICES
!     ECFLTP = FUEL(S) CONSUMED BY EACH CAPACITY TYPE
!     ECMFSH = MAXIMUM FUEL SHARE BY PLANT GROUP
!     ECNTP  = NUMBER OF CAPACITY TYPES
!     OUTPUT VARIABLES
!     ECFSHR = FUEL SHARES BETWEEN PRIMARY AND SECONDARY FUELS
!
      SUBROUTINE ELFSHR(ISP,IRG)
!
      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'dispout'
      include 'dispuse'
      include 'fuelin'
      include 'wrenew'
      include 'wwdcomon'
      include 'uefdout'
      include 'ecpcntl'
      include 'bildout'
      include 'uecpout'
      include 'dsmdimen'
      include 'dsmtoefd'
      include 'angtdm'
      include 'emission'
      include 'efdin'
      include 'cdsparms'
      include 'uso2grp'
      include 'ecp_coal'
      include 'coalemm'
      include 'csapr'
      include 'elout'
      include 'emmemis'
      include 'ecp_nuc'
      include 'emm_aimms'
!
      REAL*8 COST(20),PSO2,WGHT(20),PSHR(20),TSHR,TWSHR,PMIN,PMAX
      REAL*8 PDIF,EPFTOL,T2SHR,TEMP,TEMP2,SWGHT(12),TWGHT,FSHR,CSTTMP
      REAL*8 COMTMP,CFLTMP,CSO2TMP,RSHR,MAXSHR(20),TRSSHR,WRSSHR(20)
      REAL*8 MAXOIL,MAXRSH,MAXGAS
      REAL*8 CFSTEO(4)
      REAL*8 COSTOM(20),COSTFL(20),C0STS02(20),MAXOSHR,IWGHT(20),TOTSHR
      REAL*8 CST_OM,CST_FL,CST_SO2,CST_TOT,PHG,CST_HG,COSTHG(20),CHGTMP
      REAL*8 R1,S1,R2,S2,RX,NGSHR,OLSHR,ZCFL,ZHG, HEATRATE
      REAL RSPRCAV,RSSO2AV,RSVOMAV,RSHRTAV
      INTEGER IR(20),ICNT,ISP,IPGRP,KSCRB,I,JSP,J,NSO2,ISO2,K,IRT,CRG
      INTEGER IECP,IFUEL,IFLRG,IRS,IYR,IRG,IWGRP,IRNK
      INTEGER IDUAL,IOLRG,INGRG,HGGRP,IFL
!
!     COFIRING SHARES TO BENCHMARK DURING STEO YEARS (1999 - 2002)
!
      DATA CFSTEO/0.0003,0.0005,0.0010,0.0015/
!
!     SET FUEL SHARES TO MAXIMUM FUEL SHARES FOR CAPACITY TYPES
!
      IYR = CURIYR
      DO I = 1 , ECNTP
         FSHR = 0
         IPGRP = ECASTS(I)
         IWGRP = ECDBID(I)
         IF (ULHGGP(IWGRP) .EQ. 0) THEN
            HGGRP = 1

!jj         ISO2 = 1

            NSO2 = 0
         ELSE
            HGGRP = HG_GRP(ULHGGP(IWGRP))

!jj         ISO2 = SO2_GRP_BY_CLRG(ULHGGP(IWGRP))

            NSO2 = NUM_SO2_GRP
         END IF
         IECP = ECTECP(I)
!
!        FOR ANNUAL MERIT ORDER DETERMINE WEIGHTS FOR SEASONAL FUEL SHARES
!
         IF (ISP .GT. EENSP) THEN
            TWGHT = 0.0
            DO JSP = 1 , EENSP
               SWGHT(JSP) = EETIME(JSP)/8760 * ECCAP(I,JSP)
               TWGHT = TWGHT + SWGHT(JSP)
            END DO
            DO JSP = 1 , EENSP
               SWGHT(JSP) = SWGHT(JSP) / TWGHT
            END DO
         END IF
!
         IDUAL = 0
!
!        RESET HI/LO SULFUR SHARES FOR DUAL FIRED PLANTS
!
         TOTSHR = 0.0
         DO J = 1, EIFPLT
            IF (ECFLTP(I,J) .GT. 0) THEN
               TOTSHR = TOTSHR + ECMFSH(I,J)
            ELSE
               ECMFSH(I,J) = 0.0
            END IF
         END DO
!
         IF (TOTSHR .LT. 0.000001) THEN
            ECMFSH(I,1) = 1.0
            DO J = 2 , EIFPLT
               ECMFSH(I,J) = 0.0
            END DO
         ELSE IF (TOTSHR .LT. 1.0) THEN
            DO J = 1 , EIFPLT
               IF (ECMFSH(I,J) .GT. 0.0 .AND. ECFLTP(I,J) .GT. 0) THEN
                  ECMFSH(I,J) = MAX( 0.000001 , ECMFSH(I,J) / TOTSHR)
               END IF
            END DO
         ELSE
            DO J = 1 , EIFPLT
               IF (ECMFSH(I,J) .LT. 0.000001 .AND. ECFLTP(I,J) .GT. 0) THEN
                  ECMFSH(I,J) = 0.000001
               END IF
            END DO
         END IF
         DO J = 1, EIFPLT
            TOTSHR = TOTSHR + ECMFSH(I,J)
            IF (ECFLTP(I,J) .EQ. UIGC) MAXGAS = ECMFSH(I,J)
            IF (ECFLTP(I,J) .EQ. UIRL) MAXOIL = ECMFSH(I,J)
            IF (ECFLTP(I,J) .EQ. UIRH) MAXRSH = ECMFSH(I,J)
         END DO
         JSP = MIN(ISP,EENSP)
         DO J = 1, EIFPLT
            IF (ECFLTP(I,J) .EQ. UIRL) ECMFSH(I,J) = MAXOIL * (1.0-MAXRSH)
            IF (ECFLTP(I,J) .EQ. UIRH) ECMFSH(I,J) = MAXOIL * MAXRSH
         END DO
         DO J = 1 , EIFPLT
            IF (ECFLTP(I,J) .GT. 0) THEN
               IFUEL = ECFLTP(I,J)
!
!              IDENTIFY IF PLANT CAN COFIRE WITH WOOD/WASTE
!              USE COFIRE SHARE FROM ECP, WHICH HAS FULL SUPPLY CURVES
!
               IF (USW_ECPCF .EQ. 1 .AND. IPGRP .NE. UIBMS .AND. IFUEL .EQ. UIWD) THEN
                  IF ((CURIYR + UHBSYR) .GE. 1999 .AND. (CURIYR + UHBSYR) .LE. 2002) THEN
                     ECMFSH(I,J) = CFSTEO(CURIYR + UHBSYR - 1998)
                  ELSE
                     CRG = ECFLRG(I,J,1)
                     IF (I .GT. EFD_D_MPG .OR. I .LT. 1 .OR. J .GT. EFD_D_FPP .OR. J .LT. 1 .OR. IECP .GT. ECP_D_DSP .OR. &
                        IECP .LT. 1 .OR. CRG .GT. NDREG .OR. CRG .LT. 1) THEN
                        WRITE(UF_DBG,5311) CURIRUN, CURIYR+UHBSYR,CURITR,ISP,IRG,I,J,IECP,CRG,IPGRP,IFUEL,IWGRP
 5311                   FORMAT(1X,"UPWDCFR",12(":",I6))
                     END IF
!                    ECMFSH(I,J) = UPWDCFR(IECP,CRG)
                     ECMFSH(I,J) = UPWDCFN(IECP,IRG)
!                    write(6,2345) curiyr+1989,uplntcd(iecp),irg,crg,ecmfsh(i,j),ecfshr(i,j),upwdcfn(iecp,irg),upwdcfr(iecp,crg)
!2345                format(1h ,'!cfsh',i4,a3,2i3,4f10.3)
                  END IF
               END IF
               IFLRG = ECFLRG(I,J,1)
               IWGHT(J) = DBLE(1.0)
               ECFSHR(I,J) = ECMFSH(I,J)
               MAXSHR(J) = ECMFSH(I,J)
            ELSE
               ECFSHR(I,J) = 0.0
               MAXSHR(J) = 0.0
            END IF
!
            FSHR = FSHR + ECFSHR(I,J)
!
!           IDENTIFY DUAL-FIRED OIL/GAS PLANT
!
            IF (ECFLTP(I,J) .EQ. UIGC .AND. ECMFSH(I,J) .GT. 0.0) IDUAL = 1
         END DO
!
         IF ((FSHR .LT. 1.0) .AND.(FSHR .GT. 0.001)) THEN
            DO J = 1, EIFPLT
               ECFSHR(I,J) = ECFSHR(I,J) / FSHR
            END DO
         END IF
!
!        IF THE CAPACITY IS DUAL DETERMINE SHARES
!
         IF (FSHR .GT. 1.0) THEN
!
!           SORT FUELS FROM LEAST TO MOST EXPENSIVE
!
!           PSO2 = MAX( DBLE(EMELPSO2(CURIYR) * 0.5) , PSO2)
!jj         PSO2 = DBLE(EMELPSO2(CURIYR,ISO2) * 0.5)

            PSO2 = 0.0
            IF ((CURIYR + UHBSYR) .LT. TSO2_YR_BY_CLRG)THEN
               IF (NSO2 .GT. 0)THEN
                  DO ISO2 = 1 , NSO2
                     IF (SO2_SHR_BY_CLRG(ULHGGP(IWGRP),ISO2) .GT. 0.0)THEN

!                       PSO2 = PSO2 + DBLE(EMELPSO2(CURIYR,ISO2) * SO2_SHR_BY_CLRG(ULHGGP(IWGRP),ISO2) * 0.5)

                        PSO2 = PSO2 + DBLE(ECP_PSO2(0,CURIYR,ISO2) * SO2_SHR_BY_CLRG(ULHGGP(IWGRP),ISO2) * 0.5)
                     END IF
                  END DO
               ELSE
                  ISO2 = 1

!                 PSO2 = DBLE(EMELPSO2(CURIYR,ISO2) * 0.5)

                  PSO2 = DBLE(ECP_PSO2(0,CURIYR,ISO2) * 0.5)
               END IF
            ELSE
               DO ISO2 = 1 , NSO2 - 1
                  IF (TSO2_SHR_BY_CLRG(ULHGGP(IWGRP),ISO2) .GT. 0.0)THEN

!                    PSO2 = PSO2 + EMELPSO2(CURIYR,ISO2) * SO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) * 0.5

                     PSO2 = PSO2 + ECP_PSO2CL(CURIYR,ULHGGP(IWGRP)) * TSO2_SHR_BY_CLRG(ULHGGP(IWGRP),ISO2) * 0.5
                  END IF
               END DO
               DO ISO2 = NSO2 , NSO2
                  IF (TSO2_SHR_BY_CLRG(ULHGGP(IWGRP),ISO2) .GT. 0.0)THEN

!                    PSO2 = PSO2 + EMELPSO2(CURIYR,ISO2) * SO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) * 0.5

                     PSO2 = PSO2 + ECP_PSO2(0,CURIYR,ISO2) * TSO2_SHR_BY_CLRG(ULHGGP(IWGRP),ISO2) * 0.5
                  END IF
               END DO
            END IF
            PHG = EMEL_PHG(HGGRP,CURIYR) * 0.0005
            DO J = 1, EIFPLT
               IF (ECFSHR(I,J) .LE. 0.0) THEN
                  IR(J) = 99
                  COST(J) = 99999999.0
                  ECFSHR(I,J) = 0.0
               ELSE
                  IFUEL = ECFLTP(I,J)
                  IFLRG = ECFLRG(I,J,1)

                  IF (IFUEL .EQ. UIDS) THEN
                     HEATRATE = ULHTRT_EFD(IPGRP,ISP) * 1.10
                  ELSE
                     HEATRATE = ULHTRT_EFD(IPGRP,ISP)
                  END IF

                  ZHG = 0.0
                  IF (IFUEL .LE. UIIS) THEN
                     ZHG = PHGCLNR(IFLRG,CURIYR,IFUEL)
                  ELSE
                     ZHG = UFRHG(IFUEL,IFLRG) * PHG
                  END IF
                  KSCRB = ECSCRB(I)
                  CST_OM = ECOMR(I,J)

!                 ADD DSI VOM, IF APPROPRIATE

!                 IF ((CURIYR + UHBSYR) .GE. UDSI_YR .AND. IECP .LT. WIPC)THEN
!                    IF (ECP_SCRUB(IECP,CURIYR) .EQ. 2)THEN
!                       IF (UPLNTCD(IECP)(1:1) .EQ. 'B')THEN
!                          CST_OM = CST_OM + UPDSIVOM(1)
!                       ELSE
!                          CST_OM = CST_OM + UPDSIVOM(2)
!                       END IF
!                    END IF
!                 END IF

                  ZCFL = UPFUEL(IFUEL,IFLRG)
                  IF (IFUEL .EQ. UIGF .OR. IFUEL .EQ. UIGC .OR. IFUEL .EQ. UIGI) THEN              !one gas price for all types
                    IF (ISP .LE. EENSP) THEN
                       ZCFL = SPNGELGR(IFLRG,CURIYR,ISP)                                            ! gas seas same as EMM seas
                    ENDIF
                  ENDIF

                  CST_FL = ZCFL * HEATRATE * 0.001  ! add aci cost to fuel cost
!
!                 IF SUBSIDY FOR BIOMASS COFIRING, ADJUST FUEL COST
!
                  IF (IPGRP .NE. UIBMS .AND. IFUEL .EQ. UIWD) THEN
                     CST_FL = CST_FL - WDCFSUB(CURIYR) + WDCFHUR(CURIYR)
!
!                    IF USING COFIRING SHARE FROM ECP, SET FUEL PRICE TO ZERO SO MAX SHARE IS SET TO ECP SHARE
!
                     IF (USW_ECPCF .EQ. 1)CST_FL = 0.0
                  END IF
                  IF (IFUEL .LE. UIIS) THEN
                     CST_SO2 = PSLCLNR(IFLRG,CURIYR,IFUEL) * 0.001 * HEATRATE
                  ELSE
                     CST_SO2 = PSO2 * UFRSO2R(IFUEL,IFLRG) * 0.000001 * HEATRATE * (1.0 - REAL(KSCRB) * EFACTR)
                  END IF
                  CST_HG = ZHG * 0.001 * HEATRATE
                  CST_TOT = CST_OM + CST_FL + CST_SO2 + CST_HG
                  COSTOM(J) = CST_OM
                  COSTFL(J) = CST_FL
                  C0STS02(J) = CST_SO2
                  COSTHG(J) = CST_HG
                  COST(J) = CST_TOT
                  IR(J) = J
               END IF
            END DO
!
            DO J = 1, EIFPLT - 1
               DO K = J + 1, EIFPLT
                  IF ((COST(J) .GT. COST(K)) .OR. (IR(J) .EQ. 99)) THEN
                     IRT = IR(J)
                     COMTMP = COSTOM(J)
                     CFLTMP = COSTFL(J)
                     CSO2TMP = C0STS02(J)
                     CHGTMP = COSTHG(J)
                     CSTTMP = COST(J)
                     IR(J) = IR(K)
                     COSTOM(J) = COSTOM(K)
                     COSTFL(J) = COSTFL(K)
                     C0STS02(J) = C0STS02(K)
                     COSTHG(J) = COSTHG(K)
                     COST(J) = COST(K)
                     IR(K) = IRT
                     COSTOM(K) = COMTMP
                     COSTFL(K) = CFLTMP
                     C0STS02(K) = CSO2TMP
                     COSTHG(K) = CHGTMP
                     COST(K) = CSTTMP
                  END IF
               END DO
            END DO
!
!           IF PRICES ARE CLOSE THEN SMOOTH SHARES BETWEEN FUELS
!
            ICNT = 1
            FSHR = 1.0
            EPFTOL = UFPTOL
            PMIN = COSTFL(1) + C0STS02(1) + COSTHG(1)
            PMAX = PMIN * (1 + EPFTOL)
            PDIF = PMAX - PMIN
            IF (PDIF .GT. DBLE(0.0)) THEN
               WGHT(1) = IWGHT(IR(1))
               PSHR(1) = ECFSHR(I,IR(1))
               TWSHR = WGHT(1)
               TSHR = PSHR(1)
               DO J = 2 , EIFPLT
                  IF ((IR(J) .LT. 99) .AND.  ((COSTFL(J) + C0STS02(J) + COSTHG(J)) .LE. PMAX)) THEN
                     ICNT = ICNT + 1
                     PSHR(J) = ECFSHR(I,IR(J))
                     TSHR = TSHR + PSHR(J)
                     WGHT(J) = IWGHT(IR(J)) * ((PMAX - (COSTFL(J) + C0STS02(J) + COSTHG(J))) / PDIF)
                     TWSHR = TWSHR + WGHT(J)
                  END IF
               END DO
               IF (TSHR .GT. 1.0) THEN
                  T2SHR = TSHR
                  DO J = 1 , ICNT - 1
                     TEMP = TWSHR - WGHT(J)
                     T2SHR = T2SHR - PSHR(J)
                     TEMP2 = MAX( (FSHR - T2SHR), (WGHT(J)/TWSHR) )
                     ECFSHR(I,IR(J)) = MIN(PSHR(J),TEMP2)
                     FSHR = FSHR - ECFSHR(I,IR(J))
                     IF (FSHR .GT. 0.0) THEN
                        TWSHR = TEMP / FSHR
                     ELSE
                        TWSHR = 0.0
                     END IF
                  END DO
               ELSE
                  ICNT = 1
                  FSHR = 1.0
               END IF
            END IF
!
!           IF THE CAPACITY IS DUAL FIRED USE MAXIMUM FUEL SHARE FOR CHEAPEST
!           FUEL AND ADJUST THE OTHER SHARES SO THAT TOTAL SHARE EQUALS 1
!
            DO J = ICNT, EIFPLT
               IF (IR(J) .LT. 99) THEN
                  ECFSHR(I,IR(J)) = MIN(DBLE(ECFSHR(I,IR(J))),FSHR)
                  FSHR = FSHR - ECFSHR(I,IR(J))
               END IF
            END DO
!
!           INSURE THAT MAXIMUM HS RESID SHARE NOT VIOLATED FOR STEAM UNITS
!
            IRS = 0
            RSHR = 0
            DO J = 1 , EIFPLT
               IF (IR(J) .LT. 99) THEN
                  IF (ECFLTP(I,IR(J)) .EQ. UIRL .OR. ECFLTP(I,IR(J)) .EQ. UIRH) THEN
                     IRS = IRS + 1
                     RSHR = RSHR + ECFSHR(I,IR(J))
                  END IF
               END IF
            END DO
            TRSSHR = RSHR
            IF (IRS .GT. 1) THEN
               DO J = 1, EIFPLT
                  IF (IR(J) .LT. 99) THEN
                     IF (ECFLTP(I,IR(J)) .EQ. UIRH) THEN
                        ECFSHR(I,IR(J)) = MIN(DBLE(ECFSHR(I,IR(J))), MAXSHR(IR(J)) * RSHR)
                        RSHR = RSHR - ECFSHR(I,IR(J))
                     END IF
                  END IF
               END DO
               DO J = 1, EIFPLT
                  IF (IR(J) .LT. 99) THEN
                     IF (ECFLTP(I,IR(J)) .EQ. UIRL) THEN
                        ECFSHR(I,IR(J)) = RSHR
                     END IF
                  END IF
               END DO
            END IF
!
!           STORE DATA ABOUT FUEL SWITCHING CAPABILITY IN DUAL-FIRED UNITS
!
            IF (IDUAL .GT. 0) THEN
!
!              DETERMINE RESID MIX
!
               IF (TRSSHR .GT. 0.001) THEN
                  DO J = 1 , EIFPLT
                     IF (IR(J) .LT. 99) THEN
                        IFUEL = ECFLTP(I,IR(J))
                        IF (IFUEL .EQ. UIRL .OR. IFUEL .EQ. UIRH) THEN
                           WRSSHR(J) = ECFSHR(I,IR(J)) / TRSSHR
                        ELSE
                           WRSSHR(J) = 0.0
                        END IF
                     END IF
                  END DO
               ELSE
                  MAXOSHR = 0.0
                  DO J = 1 , EIFPLT
                     IF (IR(J) .LT. 99) THEN
                        IFUEL = ECFLTP(I,IR(J))
                        IF (IFUEL .EQ. UIRL .OR. IFUEL .EQ. UIRH) THEN
                           WRSSHR(J) = MAXSHR(IR(J))
                           MAXOSHR = MAXOSHR + WRSSHR(J)
                        ELSE
                           WRSSHR(J) = 0.0
                        END IF
                     END IF
                  END DO
                  IF (MAXOSHR .GT. 0.0) THEN
                     DO J = 1 , EIFPLT
                        WRSSHR(J) = WRSSHR(J) / MAXOSHR
                     END DO
                  ELSE
                     DO J = 1 , EIFPLT
                        IF (IR(J) .LT. 99) THEN
                           IFUEL = ECFLTP(I,IR(J))
                           IF (IFUEL .EQ. UIRL) THEN
                              WRSSHR(J) = 1.0
                           ELSE
                              WRSSHR(J) = 0.0
                           END IF
                        END IF
                     END DO
                  END IF
               END IF
               MAXOSHR = 0.0
               RSPRCAV = 0.0
               RSSO2AV = 0.0
               RSVOMAV = 0.0
               RSHRTAV = 0.0
!
!              DETERMINE AVERAGE RESID VARIABLE COSTS
!
               DO J = 1, EIFPLT
                  IF (IR(J) .LT. 99) THEN
                  IFUEL = ECFLTP(I,IR(J))
                     IF (IFUEL .EQ. UIRL .OR. IFUEL .EQ. UIRH) THEN
                        IOLRG = ECFLRG(I,IR(J),1)
                        MAXOSHR = MIN ( MAXOSHR + MAXSHR(IR(J)) , DBLE(1.0))
                        RSPRCAV = RSPRCAV + WRSSHR(J) * UPFUEL(IFUEL,IOLRG)
                        RSVOMAV = RSVOMAV + WRSSHR(J) * COSTOM(J)
                        RSSO2AV = RSSO2AV + WRSSHR(J) * C0STS02(J)
                        RSHRTAV = RSHRTAV + WRSSHR(J) * ULHTRT_EFD(IPGRP,ISP)
                     END IF
                  END IF
               END DO
!
!              RECORD MAX/MIN/PARITY NG SHARES AND NG/OIL RATIOS
!
               DO J = 1, EIFPLT
                  IF (IR(J) .LT. 99) THEN
                     IFUEL = ECFLTP(I,IR(J))
                     IF (IFUEL .EQ. UIGC) THEN
                        INGRG = ECFLRG(I,IR(J),1)
!
!                       MAXIMUM GAS SHARE
!                       COST USING OIL = COST USING NG * ( 1.0 + PTOL)
!
                        USGCMAX(I) = MAXSHR(IR(J))
                        IF (( RSPRCAV * ULHTRT_EFD(IPGRP,ISP) * 0.001) .NE. 0.0) &
                           URGCMAX(I) = ( ( (RSPRCAV * RSHRTAV * 0.001 + &
                           RSVOMAV + RSSO2AV ) / (1.0 + EPFTOL) ) - &
                           COSTOM(J) - C0STS02(J) ) / &
                           ( RSPRCAV * ULHTRT_EFD(IPGRP,ISP) * 0.001)
!
!                       MINIMUM GAS SHARE
!                       COST USING OIL * ( 1.0 + PTOL) = COST USING NG
!
                        USGCMIN(I) = 1.0 - MAXOSHR
                        IF (( RSPRCAV * ULHTRT_EFD(IPGRP,ISP) * 0.001) .NE. 0.0) &
                           URGCMIN(I) = ( ( (RSPRCAV * RSHRTAV * 0.001 + &
                           RSVOMAV + RSSO2AV ) * (1.0 + EPFTOL) ) - &
                           COSTOM(J) - C0STS02(J) ) / &
                           ( RSPRCAV * ULHTRT_EFD(IPGRP,ISP) * 0.001)
!
!                       PARITY
!                       COST USING OIL = COST USING NG
!
                        USGCPAR(I) = MIN ( DBLE(0.50) , MAXSHR(IR(J)) )
                        IF (( RSPRCAV * ULHTRT_EFD(IPGRP,ISP) * 0.001) .NE. 0.0) &
                           URGCPAR(I) = ( (RSPRCAV * RSHRTAV * 0.001 + &
                           RSVOMAV + RSSO2AV ) - &
                           COSTOM(J) - C0STS02(J) ) / &
                           ( RSPRCAV * ULHTRT_EFD(IPGRP,ISP) * 0.001)
                        IF (rsprcav .NE. 0.0) RX = UPFUEL(IFUEL,INGRG) / RSPRCAV
                        IF (RX .LE. URGCPAR(I)) THEN
                           RX = MAX( RX , DBLE(URGCMAX(I)))
                           R1 = URGCPAR(I)
                           S1 = USGCPAR(I)
                           R2 = URGCMAX(I)
                           S2 = USGCMAX(I)
                           IF ((r2-r1) .NE. 0.0) NGSHR = S2 - (((S2 - S1) * (R2 - RX)) / (R2 - R1))
                           OLSHR = DBLE(1.0) - NGSHR
                        ELSE
                           RX = MIN( RX , DBLE(URGCMIN(I)))
                           R1 = URGCMIN(I)
                           S1 = USGCMIN(I)
                           R2 = URGCPAR(I)
                           S2 = USGCPAR(I)
                           IF ((r2-r1) .NE. 0.0) NGSHR = S2 - (((S2 - S1) * (R2 - RX)) / (R2 - R1))
                           OLSHR = DBLE(1.0) - NGSHR
                        END IF
                     END IF
                  END IF
               END DO
!
!              AGAIN INSURE MAXIMUM SHARES ARE NOT VIOLATED
!
               IF (NGSHR .GT. MAXGAS) THEN
                  NGSHR = MAXGAS
                  OLSHR = DBLE(1.0) - NGSHR
               END IF
               IF (OLSHR .GT. MAXOIL) THEN
                  OLSHR = MAXOIL
                  NGSHR = DBLE(1.0) - OLSHR
               END IF
               DO J = 1, EIFPLT
                  IF (IR(J) .LT. 99) THEN
                     IFUEL = ECFLTP(I,IR(J))
                     IF (IFUEL .EQ. UIGC) THEN
                        ECFSHR(I,IR(J)) = NGSHR
                        IFLRG = ECFLRG(I,J,1)
                     END IF
                     IF (IFUEL .EQ. UIRL .OR. IFUEL .EQ. UIRH) THEN
                        ECFSHR(I,IR(J)) = OLSHR * WRSSHR(J)
                        IFLRG = ECFLRG(I,J,1)
                     END IF
                  END IF
               END DO
            END IF
         END IF
         IF (FCRL .eq. 1)  THEN
            write(UF_DBG,1068) I,IWGRP,IPGRP,((ECFLTP(I,IFL),ECFSHR(I,IFL)),IFL=1,EIFPLT)
 1068       format(1x,'ECFSHR elfshr',3I6,3(I4,F10.5))
         ENDIF
      END DO ! I = 1 , ECNTP
!
      RETURN
      END

!     ==================================================================
!     - ELDGNI -
!     THIS SUBROUTINE DETERMINES THE FUEL, O&M, AND EMISSIONS COSTS
!     FOR DISTRIBUTED GENERATION CAPACITY
!     ==================================================================

!     INPUT VARIABLES
!     ULHTRT_EFD = HEATRATES BY CAPACITY TYPE AND UTILIZATION
!     EDVOMR = VARIABLE O & M COSTS BY CAPACITY TYPE
!     EPFUEL = FUEL PRICES
!     EDFLTP = FUEL(S) CONSUMED BY EACH CAPACITY TYPE
!     ENFLTP = NUMBER OF FUEL TYPES
!     EDNTP  = NUMBER OF CAPACITY TYPES
!     OUTPUT VARIABLES

      SUBROUTINE ELDGNI(ISP,IRG)

      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'dispout'
      include 'dispuse'
      include 'fuelin'
      include 'ecpcntl'
      include 'uecpout'
      include 'elout'
      include 'emission'
      include 'eusprc'
      include 'efpint'
      include 'efdin'
      include 'cdsparms'
      include 'uso2grp'
      include 'ecp_coal'
      include 'csapr'
      include 'emmemis'

      REAL PSO2,PNOX(NOX_D_GRP),PRPS,BRPS,SRPS,PHG,SPRPS,SBRPS,SSRPS
      REAL ZOMR,ZCFL,ZSO2,ZNOX(NOX_D_GRP),ZHG
      REAL*8 CST_OM,CST_FL,CST_SO2,CST_TOT,CST_NOX,CST_RPS,CST_HG,CST_RPS_ST
      INTEGER ISP,I,IFUEL,IRNK,IPGRP,HGGRP,INOX,FULLYR
      INTEGER IPTYP,IFLRG,IRG,IGRP,JSP,IECP,NSO2,ISO2

!     CALCULATE UNIT COSTS FOR EACH EQUIPMENT TYPE

      FULLYR = USYEAR(CURIYR)
      DO INOX = 1 , NOX_GRP
         PNOX(INOX) = EPNOXPR(INOX,CURIYR) * 0.5
      END DO

         IF (FULLYR .eq. UESTYR) THEN
           PRPS = 0.0
         ELSEIF (CURITR .EQ. 1) THEN
           PRPS = RENEWCRR(IRG,CURIYR - 1)
         ELSE
           PRPS = RENEWCRR(IRG,CURIYR)
         ENDIF
           BRPS = MAX(UPRNWBNDR(CURIYR,IRG),UPRNWBND(CURIYR))

      IF (ST_RPS_SW .GT. 0) THEN
         SBRPS = ST_RNW_BND(CURIYR,IRG)
         SPRPS = ST_RPS_EMM_P(IRG,CURIYR)
      END IF

      DO I = 1 , EDNTP
         IFUEL = EDFLTP(I,1)
         IFLRG = EDFLRG(I,1,1)
         IPTYP = EDASTS(I)
         IPGRP = EDDBID(I)
         IF (ULHGGP(ipgrp) .EQ. 0) THEN
            HGGRP = 1

!jj         ISO2 = 1

            NSO2 = 0
         ELSE
            HGGRP = HG_GRP(ULHGGP(ipgrp))

!jj         ISO2 = SO2_GRP_BY_CLRG(ULHGGP(IPGRP))

            NSO2 = NUM_SO2_GRP
         END IF

!jj      PSO2 = EMELPSO2(CURIYR,ISO2) * 0.5

       IF (NSO2 .GT. 0)THEN
         PSO2 = 0.0
        DO ISO2 = 1 , NSO2
         IF (SO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) .GT. 0.0)THEN
         PSO2 = PSO2 + ECP_PSO2(0,CURIYR,ISO2) * SO2_SHR_BY_CLRG(ULHGGP(IPGRP),ISO2) * 0.5
         END IF
        END DO
       ELSE
         ISO2 = 1
         PSO2 = ECP_PSO2(0,CURIYR,ISO2) * 0.5
       END IF
         IECP = EDTECP(I)
         PHG = EMEL_PHG(HGGRP,CURIYR) * 0.0005

         DO INOX = 1 , NOX_GRP
            ZNOX(INOX) = EDFNOXC(INOX,I,ISP)
         END DO
         IF (IFUEL .GT. 0) THEN
                  ZHG = UFRHG(IFUEL,IFLRG) * PHG
                  ZOMR = EDVOMR(I,1)
                  ZCFL = UPFUEL(IFUEL,IFLRG)
                  ZSO2 = UFRSO2R(IFUEL,IFLRG)
            IF (ZCFL .GT. 0.0 .AND. UPNCAR(IFUEL,IFLRG) .GE. ZCFL) THEN

!  INSURE THAT CARBON PENALTY COST DOES NOT EXCEED TOTAL FUEL COST

               WRITE(6,2424) CURIYR+1989, CURITR, IFUEL, IFLRG, UPNCAR(IFUEL,IFLRG), ZCFL, ZCFL*0.95
 2424          FORMAT(1H ,'HI CRB',' YR = ',I4,' IT = ',I2,' FL = ',I2, ' RG = ',I2,' PN = ',F7.3,' PR = ',F7.3, ' NWPN = ',F7.3)
                       UPNCAR(IFUEL,IFLRG) = 0.95 * ZCFL
                   END IF
               ELSE
                  ZCFL = 99.999
                  ZSO2 = 9.999
               ENDIF

               CST_OM = ZOMR    ! add aci O&M cost adder
               CST_FL = ZCFL * ULHTRT_EFD(IPGRP,ISP) * 0.001  ! add cost of aci to fuel cost
               CST_SO2 = PSO2 * ZSO2 * 0.000001 * ULHTRT_EFD(IPGRP,ISP)
         CST_NOX = 0.0
         DO INOX = 1 , NOX_GRP
            CST_NOX = CST_NOX + PNOX(INOX) * ZNOX(INOX) * 0.000001 * ULHTRT_EFD(IPGRP,ISP)
         END DO
               CST_HG = ZHG * 0.001 * ULHTRT_EFD(IPGRP,ISP)

         SRPS = MAX( UPRNWSHR(IECP) , UPRNWSHRR(IECP,IRG) )
         IF (ST_RPS_SW .GT. 0) THEN
            SSRPS = ST_RNW_SHR(IECP,IRG,CURIYR)
         END IF

         CST_RPS = PRPS * (BRPS - SRPS)      ! avg rev rec'd for RPS credits
        IF (ST_RPS_SW .GT. 0) THEN
           CST_RPS_ST = SPRPS * (SBRPS - SSRPS)
          IF (CST_RPS .LT. 0.0 .AND. CST_RPS_ST .LE. 0.0) THEN  ! if both are negative use the greatest benefit
           IF (ABS(CST_RPS) .LT. ABS(CST_RPS_ST)) CST_RPS = CST_RPS_ST
          ELSEIF (CST_RPS .GT. 0.0 .AND. CST_RPS_ST .GT. 0.0) THEN     ! if both are positive, use the highest positive cost
           IF (CST_RPS .LT. CST_RPS_ST) CST_RPS = CST_RPS_ST
          ELSE                        ! use net value
               CST_RPS = CST_RPS + CST_RPS_ST
          ENDIF                  
        END IF

        CST_TOT = CST_OM + CST_FL + CST_SO2 + CST_NOX + CST_RPS + CST_HG

!        STORE UNIT COSTS

         IF (ISP .LE. EENSP) THEN
            IGRP = EDDBID(I)
            ULCSTR(ISP,IGRP) = CST_OM + CST_FL - GPSCSUB(IPTYP,IRG)
            ELSO2P(ISP,IGRP) = CST_SO2 - GPSSSUB(IPTYP,IRG)
            ELNOXP(ISP,IGRP) = CST_NOX - GPSNSUB(IPTYP,IRG)
            ELRPSP(ISP,IGRP) = CST_RPS

!           IF (CURIYR+1989 .GE. 2017 .AND. CURIYR+1989 .LE. 2025 .AND. FCRL .EQ. 1) THEN
!              WRITE(6,9397) CURIRUN, CURIYR+1989, CURITR, IRG, IGRP, ISP, I, IECP, ST_RPS_SW, UPRNWREG, &
!                 ELRPSP(ISP,IGRP), PRPS, UPRNWSHR(IECP), BRPS, RENEWCRR(IRG,CURIYR), UPRNWBNDR(CURIYR,IRG), UPRNWBND(CURIYR), ST_RPS_EMM_P(IRG,CURIYR), ST_RNW_BND(CURIYR,IRG), &
!                 SRPS, ST_RNW_SHR(IECP,IRG,CURIYR), CST_RPS
!9397          FORMAT(1X,"ELDGNI_ELRPSP ",10(":",I6),12(":",F21.6))
!           END IF

            ELHGP(ISP,IGRP) = CST_HG - GPSHSUB(IPTYP,IRG)
         END IF

         END DO           ! END LOOP ON I (EDNTP) NUMBER OF PLANT GROUPS

      RETURN
      END
!
!     ==================================================================
!     - ELSO2N -
!     THIS SUBROUTINE ACCUMULATES SO2 EMISSIONS AND ALLOWANCES
!     ACROSS REGIONS FOR EACH COMPLIANCE GROUP
!     ==================================================================
!
      SUBROUTINE ELSO2N(ISOL,IRG)
!
      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispout'
      include'emission'

      INTEGER ISOL,IRG,ISO2
!
      DO ISO2 = 1 , NUM_SO2_GRP
         WRITE(22,1777) ISOL,ISO2,URGNME(IRG),ECNTP,EGALLW(ISO2),EGSO2(ISO2),EMELPSO2(CURIYR,ISO2)
      END DO
 1777 FORMAT(1X,"EGSO2",2(":",I4),":",A35,":",I3,2(":",F12.0),":",F12.2)
      RETURN
      END

!     ==================================================================
!     - ELRNEW -
!     THIS SUBROUTINE REMOVES HYDRO AND OTHER RENEWABLES
!     FROM THE LOAD CURVE
!     ==================================================================

!     INPUT VARIABLES

!     EHNTP  = NUMBER OF PONDAGE HYDRO CAPACITY FACTOR GROUPINGS
!     EHCAP  = HYDRO AND OTHER RENEWABLES CAPACITY
!     EHHYCF = SEASONAL CAPACITY FACTORS
!     EHLDCF = SEASONAL CAPACITY FACTORS BY LOAD SLICE
!     EHCR   = CENSUS REGION INDEX
!     EHNR   = NERC/NEMS REGION INDEX
!     EHFOWN = OWNERSHIP TYPE
!     EHHYTP = GENERIC RENEWABLE/INTERMITTEN CAPACITY TYPE
!     UTXMKT = HEIGHT OF SEASONAL LOAD DURATION CURVE LOCAL AND EXPORT MARKETS
!     UTWDTH = WIDTH OF THE SEASONAL LOAD DURATION CURVE

!     INTERNAL VARIABLES

!     R_CAP  = RENEWABLE CAPACITY FOR CURRENT UNIT
!     R_AREA = REQUIRED ENERGY FROM RENEWABLE CAPACITY FOR CURRENT UNIT
!     GRP    = LOAD GROUP FOR EACH VERTICAL SLICE
!     SEG    = LOAD SEGMENT FOR EACH VERTICAL SLICE
!     HGHT   = REVISED HEIGHT OF SEASONAL LOAD DURATION CURVE
!     WDTH   = WIDTH OF THE SEASONAL LOAD DURATION CURVE
!     CWDTH  = CUMULATIVE WIDTH OF THE SEASONAL LOAD DURATION CURVE
!     HOURS  = HOURS ASSOCIATED WITH RENEWABLE CAPACITY FACTOR
!     ADDGEN = GENERATION REQUIREMENT FOR STORAGE TECHNOLOGIES

!     INPUT/OUTPUT VARIABLES

!     UCRNW  = CUMULATIVE RENEWABLE CAPACITY DISPATCHED BY GROUP AND SEGMENT

!     SUBROUTINE ELRNEW(IYR,IRG,ISP)

!     IMPLICIT NONE

!     include'parametr'
!     include'ncntrl'
!     include'emmparm'
!     include'control'
!     include'dispin'
!     include'dispout'
!     include'dispuse'
!     include'dispcrv'
!     include'dispett'
!     include'dsmdimen'
!     include'dsmsectr'
!     include'dsmunits'
!     include'fuelin'
!     include'elout'
!     include'elcntl'
!     include'ecpcntl'
!     include'eusprc'
!     include'efpint'
!     include'uefdout'
!     include'uecpout'

!     DECLARE INTERNAL ARRAYS

!     REAL*8 R_CAP,R_AREA,HOURS,T_AREA,U_AREA,L_AREA,BOTTOM, &
!        T_CAP,U_CAP,L_CAP,S_AREA,S_CAP,C_AREA,TEMP,A_CAP, &
!        EHSFAC(EFD_D_RNW),ADDGEN,HGHT(EFD_D_MVS + 1),CWDTH(EFD_D_MVS + 1), &
!        WDTH(EFD_D_MVS + 1),XTRGEN,GEN,FUEL,NOXFAC,SGEN(EFD_D_MHG),TGEN
!     REAL*8 THGHT(EFD_D_MVS),TRD_CAP(EFD_D_MVS),TRD_GEN, &
!        TRD_EXP(EFD_D_MVS),REV_GWH(EFD_D_MVS),REV_TOT
!     INTEGER IRNW,IYR,IRG,ISP,ISTART,ISTOP,IVCT,KHYCF,ICHK,LGRP,LSEG, &
!        IOWN,JRNW,ICR,INR,IFOWN,I_UP,I_VAL,IFP,IFL,IGRP,IFLRG,NFLRG,KRNW,ITYPE
!     INTEGER GRP(EFD_D_MVS+1),SEG(EFD_D_MVS+1),NVLS,VLS,ISTEP,FULLYR,IECP
!     CHARACTER*15 LABEL
!     REAL PRPS,BRPS,SRPS

!     MORE VALCAP TYPE VARIABLES

!     REAL VMOM(NMOM)
!     REAL vex1,vex2,vex3,vex4,xcap,RENFOR
!     INTEGER IMOM,I,J,NUMPLNT

!     FULLYR = USYEAR(CURIYR)
!     IF (FULLYR .eq. UESTYR) THEN
!        PRPS = 0.0
!     ELSEIF (CURITR .EQ. 1) THEN
!        PRPS = RENEWCRR(IRG,CURIYR - 1)
!     ELSE
!        PRPS = RENEWCRR(IRG,CURIYR)
!     ENDIF

!        BRPS = MAX(UPRNWBNDR(CURIYR,IRG),UPRNWBND(CURIYR))

!     IF (ST_RPS_SW .GT. 0) THEN
!        BRPS = MAX(BRPS , ST_RNW_BND(CURIYR,IRG))
!        PRPS = ST_RPS_EMM_P(IRG,CURIYR)
!     END IF

!     DO IRNW = 1 , EFD_D_RNW
!       EHSFAC(IRNW) = DBLE(0.0)
!       IF ( ISP .EQ. 1) THEN
!         DO IFP = 1 , EFD_D_NFL
!           DO IOWN = 1 , USW_OWN
!             EQPFLGN(IRNW+EIPGRP,IOWN,IFP) = 0.0
!             EQPFLCN(IRNW+EIPGRP,IOWN,IFP) = 0.0
!           ENDDO
!         ENDDO
!       ENDIF
!     ENDDO
!     DO IRNW = 1 , EFD_D_MHG
!        SGEN(IRNW) = DBLE(0.0)
!     END DO

!     TEMPORARY PATCH - STORAGE FACTOR BY RENEWABLE TYPE
!     (I.E. - RATIO OF ENERGY-IN TO ENERGY-OUT)

!     I = UIHYR - EFD_D_DSP
!     EHSFAC(I) = UFACPS

!     REV_TOT = DBLE(0.0)

!     INITIALIZE GENERATION REQUIREMENT FOR ENERGY STORAGE REQUIREMENTS

!     ADDGEN = DBLE(0.0)
!     IF (ISP .EQ. 1) XTRGEN = DBLE(0.0)

!     USE ARGUMENTS SO THAT THERE ARE NO COMPILE WARNINGS

!     IVCT = IYR + IRG + ISP

!     IDENTIFY ALL LOAD SLICES IN THE SEASON AND SORT FROM HIGHEST TO LOWEST

!     NVLS = 0
!     DO LGRP = 1 , UTNGRP
!        IF (UTSEAS(LGRP) .EQ. ISP) THEN
!           DO LSEG = 1 , UTNSEG
!              NVLS = NVLS + 1
!              GRP(NVLS) = LGRP
!              SEG(NVLS) = LSEG
!              HGHT(NVLS) = UTXMKT(LSEG,LGRP,IRG)
!              WDTH(NVLS) = UTWDTH(LSEG,LGRP)
!              REV_GWH(NVLS) = DBLE(0.0)
!           END DO
!        END IF
!     END DO

!     CALL HSORT3(NVLS,HGHT,GRP,SEG)

!     CWDTH(1) = DBLE(0.0)
!     WDTH(NVLS + 1) = DBLE(0.0)
!     HGHT(NVLS + 1) = DBLE(0.0)
!     T_AREA = DBLE(0.0)
!     GRP(NVLS + 1) = 0
!     SEG(NVLS + 1) = 0

!     DO VLS = 1 , NVLS
!        LGRP = GRP(VLS)
!        LSEG = SEG(VLS)
!        WDTH(VLS) = UTWDTH(LSEG,LGRP)
!        CWDTH(VLS + 1) = CWDTH(VLS) + WDTH(VLS)
!        THGHT(VLS) = HGHT(VLS)
!        T_AREA = T_AREA + HGHT(VLS) * WDTH(VLS)
!        TRD_CAP(VLS) = DBLE(0.0)
!        TRD_EXP(VLS) = DBLE(0.0)
!     END DO

!     IF (T_AREA .GT. DBLE(0.0)) THEN
!        DO IVCT = 1 , NVLS
!           THGHT(IVCT) = THGHT(IVCT) / T_AREA
!        END DO
!     ELSE
!        DO IVCT = 1 , NVLS
!           THGHT(IVCT) = WDTH(IVCT) / CWDTH(NVLS+1)
!        END DO
!     END IF
!     TRD_GEN = DBLE(0.0)

!     LOOP THROUGH RENEWABLE UNITS AND REVISE LOAD CURVE

!     DO IRNW = 1 , EHNTP
!        IF ((EHCAP(IRNW,ISP) * EHHYCF(IRNW,ISP)) .GT. 0.0) THEN

!           IDENTIFY RENEWABLE TYPE

!           IGRP = EHDBID(IRNW)
!           KRNW = EHHYTP(IRNW)
!           JRNW = EHHYTP(IRNW) - EIPGRP
!           IECP = EHTECP(IRNW)

!           IF (IECP .EQ. WIP2) THEN
!             EHSFAC(JRNW) = UFACP2
!           ENDIF

!           DETERMINE CAPACITY AND ENERGY TO REMOVE AND STORE IN OUTPUT ARRAYS

!           KHYCF = EHHYCF(IRNW,ISP)

!           If intermittent plant use capacity factors by load segment

!          IF ((EPPLCD(EHHYTP(IRNW)) .NE. 'WND') .AND. &
!               (EPPLCD(EHHYTP(IRNW)) .NE. 'WFS') .AND. &
!               (EPPLCD(EHHYTP(IRNW)) .NE. 'SPV') .AND. &
!               (EPPLCD(EHHYTP(IRNW)) .NE. 'STH')) THEN

!           HOURS = REAL(KHYCF) * EFACTR * EETIME(ISP)
!           IF (HOURS .LE. EETIME(ISP)) THEN
!              R_CAP = DBLE(EHCAP(IRNW,ISP)) * DBLE(0.001)
!           ELSE
!                 R_CAP = DBLE(EHCAP(IRNW,ISP)) * DBLE(0.001) * DBLE(KHYCF) * DBLE(EFACTR)
!           END IF

!           T_AREA = DBLE(0.0)
!           DO IVCT = 1 , NVLS
!              T_AREA = T_AREA + HGHT(IVCT) * WDTH(IVCT)
!           END DO

!           CAPTURE EXCESS RENEWABLE ENERGY AND CAPACITY

!           IF (T_AREA .LE. R_AREA) THEN
!              TRD_GEN = TRD_GEN + (R_AREA - T_AREA)
!              DO IVCT = 1 , NVLS
!                 T_CAP = MAX( DBLE(0.0) , (R_CAP - HGHT(IVCT)))
!                 TRD_CAP(IVCT) = TRD_CAP(IVCT) + T_CAP
!              END DO
!           END IF
!           R_CAP = MIN( R_CAP , HGHT(1))
!           S_CAP = R_CAP

!           R_AREA = EHCAP(IRNW,ISP) * 0.001 * HOURS
!              ULTGEN(IGRP) = ULTGEN(IGRP) + R_AREA *  (DBLE(1.0) - EHSFAC(JRNW))

!           CHECK MAXIMUM AREA AND SET BOTTOM TO ZERO

!           IF ( R_CAP .GT. HGHT(NVLS)) THEN
!              T_AREA = DBLE(0.0)
!              DO IVCT = 1 , NVLS
!                 T_AREA = T_AREA + HGHT(IVCT) * WDTH(IVCT)
!              END DO
!              IF ( T_AREA .LE. R_AREA ) THEN
!                 BOTTOM = DBLE(0.0)
!                 ISTOP = NVLS
!                 S_CAP = HGHT(1)
!                 GO TO 100
!              END IF
!           END IF

!           CHECK MINUMUM AREA AND ADJUST BOTTOM TO ACHIEVE ENERGY

!           T_CAP = DBLE(0.0)
!           T_AREA = DBLE(0.0)
!           ISTOP = 1
!           DO WHILE (T_CAP .LT. R_CAP)
!              ISTOP = ISTOP + 1
!              U_CAP = HGHT(ISTOP - 1) - HGHT(ISTOP)
!              A_CAP = R_CAP - T_CAP
!              T_CAP = T_CAP + U_CAP
!              U_CAP = MIN(A_CAP , U_CAP)
!              T_AREA = T_AREA + CWDTH(ISTOP) * U_CAP
!           END DO
!           IF ( T_AREA .GE. R_AREA ) THEN
!              S_CAP = DBLE(0.0)
!              T_AREA = DBLE(0.0)
!              ISTOP = 1
!              DO WHILE (T_AREA .LT. (R_AREA - 0.000001))
!                 ISTOP = ISTOP + 1
!                 U_CAP = HGHT(ISTOP - 1) - HGHT(ISTOP)
!                 T_CAP = (R_AREA - T_AREA) / CWDTH(ISTOP)
!                 U_CAP = MIN( U_CAP , T_CAP)
!                 S_CAP = S_CAP + U_CAP
!                 T_AREA = T_AREA + CWDTH(ISTOP) * U_CAP
!              END DO
!              BOTTOM = HGHT(1) - S_CAP
!              ISTOP = ISTOP - 1
!              S_CAP = HGHT(1) - BOTTOM
!              GO TO 100
!           END IF

!           SEARCH CUMULATIVE HOURS UNTIL THEY EXCEED CAPACITY HOURS

!           ISTOP = 1
!              DO WHILE ((CWDTH(ISTOP) .LE. HOURS) .AND. (ISTOP .LE. NVLS))
!              ISTOP = ISTOP + 1
!           END DO
!           T_CAP = MAX ( DBLE(0.0) , HGHT(ISTOP) - R_CAP )

!           FIND WHERE REVISED HEIGHT (T_CAP) CROSSES LOAD CURVE

!           DO WHILE (HGHT(ISTOP) .GT. T_CAP)
!              ISTOP = ISTOP + 1
!           END DO
!           BOTTOM = HGHT(ISTOP)
!           ISTOP = ISTOP - 1

!           FIND AREA FROM LOAD STEP AT ISTOP UP BY R_CAP

!           T_AREA = DBLE(0.0)
!           ICHK = 0
!           ISTART = ISTOP
!           DO IVCT = 1 , ISTOP
!              S_CAP = MIN( R_CAP , HGHT(IVCT) - BOTTOM )
!              T_AREA = T_AREA + S_CAP * WDTH(IVCT)
!              IF ((ICHK .EQ. 0) .AND. (R_CAP .GT. S_CAP)) THEN
!                 ISTART = IVCT - 1
!                 ICHK = 1
!              END IF
!           END DO

!           ADJUST T_AREA DOWN UNTIL IT EQUALS REQUIRED ENERGY

!           DO WHILE (T_AREA .GT. R_AREA)
!              U_CAP = HGHT(ISTART) - BOTTOM - R_CAP
!              L_CAP = HGHT(ISTOP) - BOTTOM
!              T_CAP = MIN (U_CAP , L_CAP)
!              U_AREA = T_CAP * CWDTH(ISTART + 1)
!              L_AREA = T_CAP * CWDTH(ISTOP + 1)
!              C_AREA = L_AREA - U_AREA

!              WHEN ADJUSTED AREA FALLS BELOW REQUIREMENTS MAKE
!              FINAL ADJUSTMENT
!              ELSE MAKE FULL ADJUSTMENT AND MOVE EITHER ISTART OR ISTOP

!              IF (T_AREA - C_AREA .LE. R_AREA) THEN
!                    BOTTOM = BOTTOM + (T_AREA - R_AREA) / (CWDTH(ISTOP + 1) - CWDTH(ISTART + 1))
!                 T_AREA = R_AREA
!              ELSE
!                 BOTTOM = BOTTOM + T_CAP
!                 T_AREA = T_AREA - C_AREA
!                 IF ( U_CAP .EQ. T_CAP ) THEN
!                    IF (ISTART .EQ. 1 ) THEN
!                       GO TO 100
!                    ELSE
!                       ISTART = ISTART - 1
!                    END IF
!                 ELSE
!                    ISTOP = ISTOP - 1
!                 END IF
!              END IF
!           END DO

!           SKIP TO HERE IF MIN OR MAX ENERGY IS VIOLATED

! 100       CONTINUE

!           ADJUST THE LOAD CURVE

!           S_AREA = DBLE(0.0)
!           DO IVCT = 1 , ISTOP
!              T_CAP = MIN( R_CAP , HGHT(IVCT) - BOTTOM)
!              S_AREA = S_AREA + T_CAP * WDTH(IVCT)
!              HGHT(IVCT) = HGHT(IVCT) - T_CAP
!              LGRP = GRP(IVCT)
!              LSEG = SEG(IVCT)
!              VLS = ELMAPV(LSEG,LGRP)
!              ELGENE(VLS,ISP,IGRP) = T_CAP * WDTH(IVCT)
!              UCRNW(LSEG,LGRP,IRG) = UCRNW(LSEG,LGRP,IRG) + T_CAP
!                 UCRPS(LSEG,LGRP,IRG) = UCRPS(LSEG,LGRP,IRG) + UPRNWSHR(EHTECP(IRNW))*T_CAP
!           END DO

!           R_AREA = EHCAP(IRNW,ISP) * 0.001 * HOURS
!           T_AREA = DBLE(0.000001)
!              IF ((R_AREA .LT. (S_AREA - T_AREA)) .OR. (R_AREA .GT. (S_AREA + T_AREA))) THEN
!                 WRITE(22,4739) IYR,IRG,ISP,JRNW,IFOWN,IRNW,IGRP, EHCAP(IRNW,ISP),EHHYCF(IRNW,ISP),R_AREA,S_AREA
!4739          FORMAT(1X,'HY HUH',7I5,F15.3,I9,2F15.3)
!           END IF

!          ELSE IF INTERMITTENT USE CAPACITY FACTORS BY LOAD SEGMENT

!          ELSEIF ((EPPLCD(EHHYTP(IRNW)) .EQ. 'WND') .or. &
!                (EPPLCD(EHHYTP(IRNW)) .EQ. 'WFS') .or. &
!               (EPPLCD(EHHYTP(IRNW)) .EQ. 'SPV') .or. &
!               (EPPLCD(EHHYTP(IRNW)) .EQ. 'STH')) THEN

!           S_AREA = DBLE(0.0)
!           DO IVCT = 1 , NVLS
!              LGRP = GRP(IVCT)
!              LSEG = SEG(IVCT)
!              R_CAP = EHCAP(IRNW,ISP) * .001 * EHLDCF(IRNW,LSEG,LGRP) * EFACTR
!              T_CAP = MIN( R_CAP , HGHT(IVCT) )
!              S_AREA = S_AREA + T_CAP * WDTH(IVCT)
!              VLS = ELMAPV(LSEG,LGRP)
!              ULTGEN(IGRP) = ULTGEN(IGRP) + T_CAP * WDTH(IVCT)
!              ELGENE(VLS,ISP,IGRP) = T_CAP * WDTH(IVCT)
!              UCRNW(LSEG,LGRP,IRG) = UCRNW(LSEG,LGRP,IRG) + T_CAP
!                 UCRPS(LSEG,LGRP,IRG) = UCRPS(LSEG,LGRP,IRG) + UPRNWSHR(EHTECP(IRNW))*T_CAP
!           END DO
!          ENDIF             ! End if Intermittent

!           STORE ENERGY IN OUTPUT ARRAYS

!           SRPS = MAX( UPRNWSHR(IECP) , UPRNWSHRR(IECP,IRG) )
!           IF (ST_RPS_SW .GT. 0) THEN
!              SRPS = MAX( SRPS , ST_RNW_SHR(IECP,IRG,CURIYR) )
!           END IF
!           ELRPSP(ISP,IGRP) = PRPS * (BRPS - SRPS)      ! avg rev rec'd for RPS credits

!           IF (CURIYR+1989 .GE. 2017 .AND. CURIYR+1989 .LE. 2025 .AND. FCRL .EQ. 1) THEN
!              WRITE(6,9397) CURIRUN, CURIYR+1989, CURITR, IRG, IGRP, ISP, IRNW, IECP, ST_RPS_SW, UPRNWREG, &
!                 ELRPSP(ISP,IGRP), PRPS, UPRNWSHR(IECP), BRPS, RENEWCRR(IRG,CURIYR), UPRNWBNDR(CURIYR,IRG), UPRNWBND(CURIYR), ST_RPS_EMM_P(IRG,CURIYR), ST_RNW_BND(CURIYR,IRG), SRPS, ST_RNW_SHR(IECP,IRG,CURIYR)
!9397!         FORMAT(1X,"ELRNEW_ELRPSP ",10(":",I6),11(":",F21.6))
!           END IF

!           ULCSTR(ISP,IGRP) = EHVOMR(IRNW) - EHGSUB(IRNW)
!           GEN = S_AREA * (DBLE(1.0) - EHSFAC(JRNW))
!           NOXFAC = EHFNOX(IRNW,ISP)
!           IFOWN = EHFOWN(IRNW)
!           DO IFP = 1 , EIFPLT
!              IFL = ULFUEL(IFP,IGRP)
!              IF (IFL .GT. 0) THEN
!                 IFLRG = ULFLRG(IFP,IGRP)
!                 ULGENE(IFP,IGRP) = ULGENE(IFP,IGRP) +  GEN * EHMFSH(IRNW,IFP)
!                 FUEL = GEN * EHMFSH(IRNW,IFP) * ULHTRT_EFD(IGRP,ISP) *  0.001

!                 IF (FCRL .EQ. 1) THEN
!                    ITYPE = -5
!                    WRITE(18,7317) CURIRUN, CURIYR+1989, IYR+1989, ITYPE, ISP, IRG, IGRP, IECP, IRNW, IFP, IFL, UIDS, FUEL, ECDSPF(IGRP,ISP) * EHMFSH(IRNW,IFP), EHMFSH(IRNW,IFP), GEN, ULHTRT_EFD(IGRP,ISP)
!7317!               FORMAT(1X,"ELCOST_FUEL",12(":",I5),5(":",F21.6))
!                 END IF

!                 FUEL = ECDSPF(IGRP,ISP)

!                 EQPFLCN(KRNW,IFOWN,IFL) =  EQPFLCN(KRNW,IFOWN,IFL) + FUEL
!                 EQPFLGN(KRNW,IFOWN,IFL) =  EQPFLGN(KRNW,IFOWN,IFL) +  (GEN * EHMFSH(IRNW,IFP))
!                 ULBTUE(IFP,IGRP) = ULBTUE(IFP,IGRP) + FUEL
!                 ULSO2W(IFP,IGRP) = ULSO2W(IFP,IGRP) +  FUEL * UFRSO2(IFL,IFLRG) * 0.5
!                 ULNOXW(IFP,IGRP) = ULNOXW(IFP,IGRP) +  FUEL * NOXFAC * 0.5
!                 UNOXINR(IRG,IYR) = UNOXINR(IRG,IYR) + ( FUEL * NOXFAC * 0.5 ) / 1000000.0
!                 ULCO2W(IFP,IGRP) = ULCO2W(IFP,IGRP) +  FUEL * UFRCO2(IFL,1) * 0.5
!                 ULCARW(IFP,IGRP) = ULCARW(IFP,IGRP) +  FUEL * UFRCAR(IFL,1) * 0.5
!                 IF (GEN .GT. 0.0) THEN
!                    ULCSTR(ISP,IGRP) = ULCSTR(ISP,IGRP) +  UPFUEL(IFL,IFLRG) * EHMFSH(IRNW,IFP) * (ECDSPF(IGRP,ISP) / GEN)
!                    ULFLCST(IFP,IGRP) =  ULFLCST(IFP,IGRP) +  UPFUEL(IFL,IFLRG) * EHMFSH(IRNW,IFP) * (ECDSPF(IGRP,ISP) / GEN)
!                 ELSE
!                    ULCSTR(ISP,IGRP) = ULCSTR(ISP,IGRP) +  UPFUEL(IFL,IFLRG) * EHMFSH(IRNW,IFP) *  ULHTRT_EFD(IGRP,ISP) * 0.001
!                    ULFLCST(IFP,IGRP) =  ULFLCST(IFP,IGRP) +  UPFUEL(IFL,IFLRG) * EHMFSH(IRNW,IFP) *  ULHTRT_EFD(IGRP,ISP) * 0.001
!                 END IF

!                 DO NFLRG = 1 , UNFLRG(IFL)
!                    IFLRG = EHFLRG(IRNW,IFP,NFLRG)
!                    IF (IFLRG .LE. 0) THEN
!                       IFLRG = EFD_D_MFRG
!                       PRINT *,' NO FUEL REGION ',IRNW,IFP,NFLRG
!                    ENDIF
!                    IF (IFOWN .LE. 4) THEN
!                       UQFUEL(IFL,IFLRG,NFLRG) = UQFUEL(IFL,IFLRG,NFLRG) + FUEL
!                       IF (NFLRG .EQ.1)SQFUEL(IFL,IFLRG,ISP) = SQFUEL(IFL,IFLRG,ISP) + FUEL
!                    END IF
!                 END DO

!              END IF
!           END DO

!           adjust unit cost for allowance allocations

!           ULCSTR(ISP,IGRP) = ULCSTR(ISP,IGRP) - GPSCSUB(KRNW,IRG)
!           ELSO2P(ISP,IGRP) = ELSO2P(ISP,IGRP) - GPSSSUB(KRNW,IRG)
!           ELNOXP(ISP,IGRP) = ELNOXP(ISP,IGRP) - GPSNSUB(KRNW,IRG)
!           ELHGP(ISP,IGRP) = ELHGP(ISP,IGRP) - GPSHSUB(KRNW,IRG)
!           ICR = EHCR(IRNW)
!           INR = EHNR(IRNW)
!           ADDGEN = ADDGEN + EHSFAC(JRNW) * S_AREA
!           SGEN(IRNW) = EHSFAC(JRNW) * S_AREA
!           BGENOWN(IRG,IFOWN) = BGENOWN(IRG,IFOWN) + S_AREA * (DBLE(1.0) - EHSFAC(JRNW)) * 0.001
!           ECAPHS(JRNW,ISP) = ECAPHS(JRNW,ISP) + S_CAP * 1000.0
!           EGENHS(JRNW,ISP) = EGENHS(JRNW,ISP) + S_AREA * (DBLE(1.0) - EHSFAC(JRNW))
!           ULGENS(ISP,IGRP) = ULGENS(ISP,IGRP) + S_AREA * (DBLE(1.0) - EHSFAC(JRNW))
!           EQHGN(JRNW,IFOWN) = EQHGN(JRNW,IFOWN) + S_AREA * (DBLE(1.0) - EHSFAC(JRNW))
!           ERTOM(IFOWN) = ERTOM(IFOWN) +  S_AREA * 0.001 * EHVOMR(IRNW)
!          IF (EHGSUB(IRNW) .GT. 0.0)THEN
!           UGNPTCN(KRNW,INR) = UGNPTCN(KRNW,INR) +  S_AREA * 0.001 *  EHGSUB(IRNW) / UPGSUB(EHTECP(IRNW))
!           UPYPTCN(KRNW,INR) = UPYPTCN(KRNW,INR) +  S_AREA * 0.001 * EHGSUB(IRNW)
!           ERTGSUB(IFOWN) = ERTGSUB(IFOWN) +  S_AREA * 0.001 * EHGSUB(IRNW) * (1.0 - UPTXRT)
!          END IF
!           ERHOM(JRNW,IFOWN) = ERHOM(JRNW,IFOWN) + S_AREA * 0.001 * EHVOMR(IRNW)
!           UQPGENN(KRNW,INR,IFOWN) = UQPGENN(KRNW,INR,IFOWN) + S_AREA * (DBLE(1.0) - EHSFAC(JRNW))
!           UQPGENC(KRNW,ICR) = UQPGENC(KRNW,ICR) + S_AREA * (DBLE(1.0) - EHSFAC(JRNW))

!        END IF
!     END DO

!     FILL LOW SPOT ON LOAD CURVE WITH GENERATION REQUIRMENTS FROM STORAGE

!     I_UP = NVLS - 1

!     XTRGEN = XTRGEN + ADDGEN
!     TGEN = ADDGEN
!     IF (ISP .EQ. EENSP) THEN
!        WRITE(UF_ETT,366) IYR,IRG,'10',XTRGEN
! 366    FORMAT(I4,1X,I4,1X,A5,F10.4)
!     ENDIF

!     DO WHILE (ADDGEN .GT. DBLE(0.000001))
!        TEMP = CWDTH(NVLS + 1) - CWDTH(I_UP + 1)
!        R_AREA = (HGHT(I_UP) - HGHT(NVLS)) * TEMP
!        IF (R_AREA .LE. ADDGEN) THEN
!           R_CAP = DBLE( HGHT(I_UP) )
!           ADDGEN = ADDGEN - R_AREA
!        ELSE
!           R_AREA = R_AREA - ADDGEN
!           R_CAP = DBLE( HGHT(I_UP) ) - R_AREA / TEMP
!           ADDGEN = DBLE(0.0)
!        END IF
!        IF ((I_UP .EQ. 1 ) .AND. (ADDGEN .GT. DBLE(0.0))) THEN
!           R_CAP = ADDGEN / CWDTH(NVLS + 1) + HGHT(1)
!           ADDGEN = DBLE(0.0)
!           I_VAL = 1
!        ELSE
!           I_VAL = I_UP + 1
!        END IF
!        DO IVCT = I_VAL , NVLS
!           T_CAP = R_CAP - HGHT(IVCT)
!           HGHT(IVCT) = R_CAP
!           LGRP = GRP(IVCT)
!           LSEG = SEG(IVCT)
!           UTHGHT(LSEG,LGRP,IRG) = UTHGHT(LSEG,LGRP,IRG) + T_CAP
!           VLS = ELMAPV(LSEG,LGRP)
!           ETHGHT(VLS,ISP,IRG) = ETHGHT(VLS,ISP,IRG) + T_CAP
!           REV_GWH(IVCT) = REV_GWH(IVCT) + T_CAP * WDTH(IVCT)
!           REV_TOT = REV_TOT + T_CAP * WDTH(IVCT)
!        END DO
!        I_UP = I_UP - 1
!     END DO

!     DETERMINE HOW TO TRADE EXCESS RENEWABLE ENERGY

!     IF (TRD_GEN .GT. 0.000001) THEN
!        T_AREA = DBLE(0.0)
!        DO IVCT = 1 , NVLS
!           THGHT(IVCT) = THGHT(IVCT) * TRD_GEN
!           THGHT(IVCT) = THGHT(IVCT) + T_AREA / WDTH(IVCT)
!           IF (THGHT(IVCT) .GE. TRD_CAP(IVCT)) THEN
!              TRD_EXP(IVCT) = TRD_CAP(IVCT)
!              T_AREA = (THGHT(IVCT) - TRD_CAP(IVCT)) * WDTH(IVCT)
!           ELSE
!              TRD_EXP(IVCT) = THGHT(IVCT)
!              T_AREA = DBLE(0.0)
!           END IF
!        END DO
!        IF (USW_XP .NE. 1) THEN
!           WRITE(6,3113) CURIYR+UHBSYR,CURITR,ISP,IRG,TRD_GEN, &
!              (TRD_EXP(IVCT),IVCT=1,NVLS)
!3113       FORMAT(1X,"ELRNEW_XMKT",4(":",I4),":",F9.3,18(":",F7.3))
!        END IF
!     END IF

!     REPLACE LOAD HEIGHTS WITH REVISED HEIGHTS

!     DO IVCT = 1 , NVLS
!        DO IRNW = 1 , EHNTP
!           IF (SGEN(IRNW) .GT. 0.0) THEN
!              IGRP = EHDBID(IRNW)
!              JRNW = EHHYTP(IRNW) - EIPGRP
!              LGRP = GRP(IVCT)
!              LSEG = SEG(IVCT)
!              GEN = SGEN(IRNW) * REV_GWH(IVCT) / REV_TOT
!              VLS = ELMAPV(LSEG,LGRP)
!              ELGENE(VLS,ISP,IGRP) = ELGENE(VLS,ISP,IGRP) - GEN
!           END IF
!        END DO
!     END DO

!     RETURN
!     END
!
!     ==================================================================
!     - ELFACT -
!     THIS SUBROUTINE DETERMINES LOAD FACTORS NEEDED TO CALCULATE
!     UNIT SPECIFIC DERATE FACTORS
!     ==================================================================
!
      SUBROUTINE ELFACT(ISP,IRG,FSTGRP,FSTSEG)
!
      IMPLICIT NONE
!
      include'parametr'                                          !//mf//
      include'ncntrl'
      include'emmparm'                                           !//mf//
      include'control'                                           !//mf//
      include'elcntl'
      include'dispin'                                            !//mf//
      include'dispuse'                                           !//mf//
!
      INTEGER*4 VLS,IP,I,ISP,IRG,IGRP
      REAL*8 ABOVE,LF,CUMCAP,FOR,RATIO,KFOR
      INTEGER FSTGRP(EFD_D_MVS,EFD_D_MSP),FSTSEG(EFD_D_MVS,EFD_D_MSP),slice
!
!     DETERMINE AREA ABOVE THE CURVE
!
      ABOVE = DBLE(0.0)
      DO VLS = 1 , ELNVCT(ISP)
         ABOVE = ABOVE + (ETHGHT(1,ISP,IRG) - ETHGHT(VLS,ISP,IRG)) *  &
            ETWDTH(VLS,ISP,IRG)
      END DO
!
!     DETERMINE TOTAL LOAD FOLLOWING REQUIREMENTS FOR ACTIVE CAPACITY
!


      I = 1
      CUMCAP = DBLE(0.0)
      LF = DBLE(0.0)
      DO WHILE ((CUMCAP .LT. ETHGHT(1,ISP,IRG)) .AND. (I .LE. ECNTP))
         IP = ECTYP(irg,I)
         IGRP = ECDBID(IP)
         KFOR = ECFOR(IGRP)
         FOR = DBLE(1.0) - KFOR
         LF = LF + ECLFR(IGRP) * FOR * ECACAP(IP)
         CUMCAP = CUMCAP + FOR * ECACAP(IP)
         I = I + 1
      END DO
      LF = LF * EETIME(ISP)
      RATIO = MIN( DBLE(1.0) , ABOVE / LF)
!
!     DETERMINE LOAD SEGMENT SPECIFIC FACTORS
!
      DO VLS = 1 , ELNVCT(ISP)
        SLICE = ELMAPV(FSTSEG(VLS,ISP),FSTGRP(VLS,ISP))
         IF (ABOVE .NE. 0) THEN
            FAC(SLICE,ISP,IRG) = RATIO * ((ETHGHT(1,ISP,IRG) -  &
             ETHGHT(SLICE,ISP,IRG)) * ETWDTH(SLICE,ISP,IRG)) / ABOVE
         ELSE
            FAC(SLICE,ISP,IRG) = 0.0
         ENDIF
      END DO

      RETURN
      END
!
!     ==================================================================
!     - ELDRAT -
!     THIS SUBROUTINE DETERMINES UNIT SPECIFIC DERATE FACTORS
!     ==================================================================
!
      SUBROUTINE ELDRAT(ISP,IRG,IP,VLS,DRAT)
!
      IMPLICIT NONE
!
      include'parametr'                                          !//mf//
      include'ncntrl'
      include'emmparm'                                           !//mf//
      include'control'                                           !//mf//
      include'elcntl'
      include'dispin'                                            !//mf//
      include'dispuse'                                           !//mf//
!
      INTEGER*4 VLS,IP,ISP,IRG,IGRP,IVLO,IVHI,IVLS
      REAL*8 DRAT,FOR,LFR,DRAT1(18)

!
!     DETERMINE DERATE FACTORS
!
      IGRP = EFD_GRPS_F(IP)
      FOR = DBLE(1.0) - UG_FOR(IP)
      LFR = UG_LFR(IP)
      DRAT = 0.0
      DRAT = (FOR * (ETWDTH(VLS,ISP,IRG) * DBLE(1.0) - (LFR * EETIME(ISP) * FAC(VLS,ISP,IRG)) )) / ETWDTH(VLS,ISP,IRG)

      RETURN
      END
!
!     Heapsort - Sorts an array RA of length N into descending numerical
!     order using the Heapsort Algorithm. N is input; RA is
!     replaced on output by its sorted rearrangement and RB
!     is rearranged correspondingly.
!
      SUBROUTINE EMSORT(N,RA,RB)
!
      IMPLICIT NONE
!
      include'emmparm'
!
      REAL*4 RA(EFD_D_MPG),RRA
      INTEGER*4 RB(EFD_D_MPG),RRB
      INTEGER N,L,IR,I,J
!
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
      GO TO 10
      END

!
!     ==================================================================
!     - ELRPSCR -
!     THIS SUBROUTINE RENEWABLE CREDIT TRADING
!     ==================================================================
!
      SUBROUTINE ELRPSCR
!
      IMPLICIT NONE
!
      include'parametr'                                          !//mf//
      include'ncntrl'                                           !//mf//
      include'emmparm'                                           !//mf//
      include'control'                                           !//mf//
      include'dispin'                                            !//mf//
      include'dispuse'                                           !//mf//
      include'dispout'                                           !//mf//
      include'dispett'                                           !//mf//
      include'uefdout'                                           !//mf//
      include'udatout'                                           !//mf//
      include'ecpcntl'                                           !//mf//
      include'bildin'                                            !//mf//
      include'bildout'                                           !//mf//
      include'uecpout'                                           !//mf//
      include'cogen'                                            !//mf//
      include'eusprc'                                            !//mf//
      include'efpint'                                            !//mf//
      include'dsmdimen'                                          !//mf//
      include'dsmsectr'                                          !//mf//
      include'qblk'                                             !//mf//
      include'emission'                                          !//mf//
      include'cdsparms'                                          !//mf//
      include'csapr'                                             !//mf//
      include'emmemis'
      include'dispinyr'
!
      INTEGER*4 REG,CEN,OWN,PLT,IY,credstart,ICAP,IECP
      INTEGER*4 EFDECP(EFD_D_CAP)
      REAL*4 RENFAC(EFD_D_CAP),TOTGEN(MNUMNR),RENGEN(MNUMNR)
      REAL*4 RENPCT,RENSAL,RENPUR,CRDPRC,TOTCRED
      REAL*4 COFGEN
      REAL*4 URPSCRN(MNUMNR),URPSCRL(MNUMNR)
      REAL*4 EXEMSLS(MNUMNR)
      REAL*4 PLTGEN,RPSGEN,DGEN(EFD_D_CAP)
      REAL*4 DGPVADJ(MNUMNR)
!
!     STORE RENEWABLE AND TOTAL RPS GENERATION FOR LAG YEAR
!
      IF(UPRNWCAS.GT.0.AND.CURIYR.GT.(UESTYR-UHBSYR).AND.CURITR.EQ.1)THEN
         DO REG = 1 , MNUMNR
            URPSRGL(REG) = URPSRGN(REG)
            URPSTGL(REG) = URPSTGN(REG)
            URPSCRL(REG) = URPSCRN(REG)
         END DO
      END IF
!
!     IF RENEWABLE PORTFOLIO STANDARD IS NOT IMPOSED, SET CREDIT TRADES TO 0
!
      IF (UPRNWCAS .LE. 0) THEN
         DO REG = 1 , UNRGNS
            CALL GETOUT(CURIYR,REG)
            ERRPS = 0.0
            CALL STROUT(CURIYR,REG)
            URPSTGN(REG) = QELASN(REG,CURIYR) * 0.001
         END DO
            URPSTGN(MNUMNR) = (QELAS(MNUMCR,CURIYR) / 3.412) - (QELASN(MNUMNR - 1,CURIYR) + QELASN(MNUMNR - 2,CURIYR)) * 0.001
      ELSE
!
!     INITIALIZE EXISTING GENERATION ARRAY
!
      IF ((CURIYR + UHBSYR) .LE. (UPSTYR - 1))THEN
         EXDGEN = 0.0
         EXPGEN = 0.0
      END IF
!
!     IF DPVDISPATCH SWITCH IS ON THEN FILL ADJUSTMENT ARRAY TO BACK OUT FROM EQPGN
!
      DGPVADJ = 0.0
      IF (DPVDISPATCH .AND. (USW_OVER .GT. 0 .AND. CURIYR .GT. (UYR_OVER - UHBSYR)) ) THEN
        DO REG = 1, UNRGNS
          DGPVADJ(REG) = DPVTOTGENNR(REG,CURIYR) * 0.001
        ENDDO
      ENDIF
!
!     FOR EACH EFD RENEWABLE TYPE IDENTIFY CORRESPONDING ECP TYPE
!     AND GET FRACTION INCLUDED IN SATISFYING RPS
!
         DO PLT = 1 , EFD_D_CAP
            EFDECP(PLT) = 0
            RENFAC(PLT) = 0.0
         END DO
            EFDECP(UIBMS) = WIWD
            EFDECP(UIBIG) = WIBI
            EFDECP(UIMSW) = WIMS
            EFDECP(UIGTH) = WIGT
            EFDECP(UIHYC) = WIHY
            EFDECP(UIHYR) = WIPS
            EFDECP(UIDST) = WIDS
            EFDECP(UIWND) = WIWN
            EFDECP(UIWNL) = WIWL
            EFDECP(UIWFS) = WIWF
            EFDECP(UISTH) = WISO
            EFDECP(UISPV) = WIPV
            EFDECP(UIPVT) = WIPT
            EFDECP(UICNU) = WICN
            EFDECP(UIANC) = WIAN
            EFDECP(UISMR) = WISM
            EFDECP(UICAS) = WIIS
            EFDECP(UICOQ) = WIPQ
            EFDECP(UIACS) = WICS
            EFDECP(UIAC2) = WIA2
            EFDECP(UICCG) = WIEC
            EFDECP(UICCX) = WIEC
            EFDECP(UIACC) = WIAC
            EFDECP(UICTG) = WIET
            EFDECP(UICTX) = WIET
            EFDECP(UIACT) = WIAT
         DO PLT = 1 , EFD_D_CAP
          IF (EFDECP(PLT) .GT. 0)THEN
            RENFAC(PLT) = UPRNWSHR(EFDECP(PLT))
          END IF
         END DO
!
!     INITIALIZE TOTALS
!
         DO REG = 1 , MNUMNR
            TOTGEN(REG) = 0.0
            RENGEN(REG) = 0.0
            EXEMSLS(REG) = 0.0
         END DO
         DO REG = 1 , UNRGNS
            DGEN = 0.0
!
!           READ INPUT DATA
!
            CALL GETIN(1,REG)

!           WRITE(6,3979) CURIRUN, CURIYR+1989, CURITR, REG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3979       FORMAT(1X,"UEFD_04794_EEITAJ_GET",4(":",I4),3(":",F12.3))

!
!           READ OUTPUT DATA
!
            CALL GETOUT(CURIYR,REG)
!
!           DETERMINE RENEWABLE AND TOTAL GENERATION
!
!           LOOP OVER FOSSIL PLANTS
!
            DO PLT = 1 , EFD_D_DSP
               ICAP = EFDECP(PLT)
!
!              UTILITY, IPP, AND NONTRADITIONAL COGEN

               PLTGEN = 0.0
               DO OWN = 1 , 4
                  PLTGEN = PLTGEN + EQPGN(PLT,OWN) * 0.001
               END DO

               IF ((CURIYR + UHBSYR) .LE. (UPSTYR - 1))THEN
                  EXDGEN(PLT,REG) = EXDGEN(PLT,REG) + PLTGEN
                  EXDGEN(PLT,MNUMNR) = EXDGEN(PLT,MNUMNR) + PLTGEN
               END IF
               DGEN(PLT) = DGEN(PLT) + PLTGEN
               IF (ICAP .GT. 0)THEN
                  IF ((CURIYR + UHBSYR) .LE. (UPSTYR - 1))THEN
                     EXPGEN(ICAP,REG) = EXPGEN(ICAP,REG) + PLTGEN
                     EXPGEN(ICAP,MNUMNR) = EXPGEN(ICAP,MNUMNR) + PLTGEN
                  END IF
               END IF
!
!              IF SHR FACTOR = 0 THEN EXCLUDE FROM QUALIFYING GEN
!
               IF (RENFAC(PLT) .GT. 0.0) THEN
!                 IF EXISTING GENERATION NOT COUNTED TOWARDS RPS, SKIP AND ADD INCREMENTAL LATER
                  IF (UPRNWEXG(ICAP) .LE. 0.0)THEN
                     RPSGEN = PLTGEN
                     UGNRPSN(PLT,REG) = UGNRPSN(PLT,REG) + RPSGEN
                     UGNRPSN(PLT,MNUMNR) = UGNRPSN(PLT,MNUMNR) + RPSGEN
                     RENGEN(REG) = RENGEN(REG) + RPSGEN * RENFAC(PLT)
                     RENGEN(MNUMNR) = RENGEN(MNUMNR) + RPSGEN * RENFAC(PLT)
                  END IF
               END IF
               TOTGEN(REG) = TOTGEN(REG) + PLTGEN
               TOTGEN(MNUMNR) = TOTGEN(MNUMNR) + PLTGEN
!              IF BAS FACTOR > 0 THEN EXCLUDE EXCLUDE EXISTING GEN FROM TOTAL BASELINE
               IF (ICAP .GT. 0)THEN
                  IF (UPRNWBAS(ICAP) .GT. 0.0) THEN
                     TOTGEN(REG) = TOTGEN(REG) - EXPGEN(ICAP,REG)
                     TOTGEN(MNUMNR) = TOTGEN(MNUMNR) - EXPGEN(ICAP,REG)
                     EXEMSLS(REG) = EXEMSLS(REG) + EXPGEN(ICAP,REG)
!                                   (1.0 + EQTDLS*ULOSSADJ(CURIYR))
                     EXEMSLS(MNUMNR) = EXEMSLS(MNUMNR) + EXPGEN(ICAP,REG)
!                                      (1.0 + EQTDLS*ULOSSADJ(CURIYR))
                  END IF
               END IF
            END DO
!           ADD GENERATION FROM EXISTING PLANTS RETROFITTED WITH CCS, IF COUNTED
            PLT = UICAS
            ICAP = WIC9
            IF (UPRNWSHR(ICAP) .GT. 0.0)THEN
               RPSGEN = EQPGNSQ * 0.001
               UGNRPSN(PLT,REG) = UGNRPSN(PLT,REG) + RPSGEN
               UGNRPSN(PLT,MNUMNR) = UGNRPSN(PLT,MNUMNR) + RPSGEN
               RENGEN(REG) = RENGEN(REG) + RPSGEN * UPRNWSHR(ICAP)
               RENGEN(MNUMNR) = RENGEN(MNUMNR) + RPSGEN * UPRNWSHR(ICAP)
            END IF
            PLT = UICOQ
            ICAP = WIC9
            IF (UPRNWSHR(ICAP) .GT. 0.0)THEN
               RPSGEN = EQPGNSQ * 0.001
               UGNRPSN(PLT,REG) = UGNRPSN(PLT,REG) + RPSGEN
               UGNRPSN(PLT,MNUMNR) = UGNRPSN(PLT,MNUMNR) + RPSGEN
               RENGEN(REG) = RENGEN(REG) + RPSGEN * UPRNWSHR(ICAP)
               RENGEN(MNUMNR) = RENGEN(MNUMNR) + RPSGEN * UPRNWSHR(ICAP)
            END IF            
!           ADD INCREMENTAL GENERATION, IF ALLOWED
            DO PLT = 1 , EFD_D_DSP
               ICAP = EFDECP(PLT)
               IF (ICAP .GT. 0)THEN
                  RPSGEN = 0.0
                  IF (UPRNWBND(CURIYR) .GT. 0.005 .AND. UPRNWEXG(ICAP) .GT. 0.0)THEN
!                 Combine CC into one EFD plant type to match ECP
                     IF (PLT .EQ. UICCG .OR. PLT .EQ. UICCX)THEN
                        IF (PLT .EQ. UICCG)THEN
                           RPSGEN = MAX(0.0,(DGEN(UICCG) + DGEN(UICCX)) - (EXDGEN(UICCG,REG) + EXDGEN(UICCX,REG)) * UPRNWEXG(ICAP))
!     if (curitr .gt. 1)write(6,3434) curiyr+1989,reg,plt,epplcd(plt),icap,uplntcd(icap),  &
!                          (DGEN(UICCG) + DGEN(UICCX)) , (EXDGEN(UICCG,REG) + EXDGEN(UICCX,REG)),  &
!                          RPSGEN
!3434 format(1h ,'!rpsgen',i4,i3,i3,a3,1x,i3,a3,1x,5f10.3)
                        END IF
                     ELSE
                        RPSGEN = MAX(0.0,DGEN(PLT) - EXDGEN(PLT,REG) * UPRNWEXG(ICAP))
!     if (curitr .gt. 1)write(6,3434) curiyr+1989,reg,plt,epplcd(plt),icap,uplntcd(icap),  &
!                       DGEN(PLT) , EXDGEN(PLT,REG),  &
!                       RPSGEN
                     END IF
                     IF (RPSGEN .GT. 0.0 .AND. RENFAC(PLT) .GT. 0.0)THEN
                        UGNRPSN(PLT,REG) = UGNRPSN(PLT,REG) + RPSGEN
                        UGNRPSN(PLT,MNUMNR) = UGNRPSN(PLT,MNUMNR) + RPSGEN
                        RENGEN(REG) = RENGEN(REG) + RPSGEN * RENFAC(PLT)
                        RENGEN(MNUMNR) = RENGEN(MNUMNR) + RPSGEN * RENFAC(PLT)
                     END IF
                  END IF
!     if (curitr .gt. maxitr)write(6,4444) curiyr+1989,reg,plt,icap,uplntcd(icap),dgen(plt),  &
!                     EXDGEN(plt,REG) , EXDGEN(plt,mnumnr),  &
!                     EXPGEN(ICAP,REG) , EXPGEN(icap,mnumnr),  &
!                     EXSGEN(ICAP,REG) , EXSGEN(iCAp,mnumnr)
!4444 format(1h ,'!exgen',i4,i3,i3,i3,a3,1x,8f10.3)
               END IF
            END DO
! account for cofiring (the difference between total biomass generation and dedicated)
              PLT = UIBMS
              COFGEN = 0.0
            DO OWN = 1 , 4
             IF (RENFAC(PLT) .GT. 0.0) THEN
              COFGEN = COFGEN + (EQFGN(UIWD,OWN) - EQPGN(PLT,OWN)) * 0.001
             END IF
            END DO
              COFGEN = MAX(COFGEN,0.0)
            RENGEN(REG) = RENGEN(REG) + COFGEN
            RENGEN(MNUMNR) = RENGEN(MNUMNR) + COFGEN
!
!           ACCOUNT FOR IMPORTS (GEN + NET IMPORTS LESS LOSSES = SALES)
!
            TOTGEN(REG) = TOTGEN(REG) + (ETIMPF + ETIMPE - &
               ETEXPF - ETEXPE) * 0.001
            TOTGEN(MNUMNR) = TOTGEN(MNUMNR) + (ETIMPF + ETIMPE - &
               ETEXPF - ETEXPE) * 0.001
!
!           LOOP OVER RENEWABLE PLANTS
!
            DO PLT = 1 , EFD_D_RNW
               ICAP = PLT + EFD_D_DSP
               IECP = EFDECP(ICAP)
!
!              UTILITY, IPP, AND NONTRADITIONAL
!
               PLTGEN = 0.0
               DO OWN = 1 , 4
                  PLTGEN = PLTGEN + EQHGN(PLT,OWN) * 0.001
               END DO
! adjust for DGPV if needed
               IF (IECP .EQ. WIPV) THEN
                  PLTGEN = PLTGEN - MIN(DGPVADJ(REG),EQHGN(PLT,1) )      ! DGPV taken out of owner 1
               ENDIF
               IF ((CURIYR + UHBSYR) .LE. (UPSTYR - 1))THEN
                  EXDGEN(ICAP,REG) = EXDGEN(ICAP,REG) + PLTGEN
                  EXDGEN(ICAP,MNUMNR) = EXDGEN(ICAP,MNUMNR) + PLTGEN
               END IF
               IF (IECP .GT. 0)THEN
                  IF ((CURIYR + UHBSYR) .LE. (UPSTYR - 1))THEN
                     EXPGEN(IECP,REG) = EXPGEN(IECP,REG) + PLTGEN
                     EXPGEN(IECP,MNUMNR) = EXPGEN(IECP,MNUMNR) + PLTGEN
                  END IF
               END IF
!     if (curitr .gt. maxitr)write(6,4455) curiyr+1989,reg,icap,iecp,uplntcd(iecp),  &
!                     EXDGEN(icap,REG) , EXDGEN(icap,mnumnr),  &
!                     EXPGEN(IecP,REG) , EXPGEN(iecp,mnumnr),  &
!                     EXSGEN(IecP,REG) , EXSGEN(iecp,mnumnr)
!4455 format(1h ,'!exgen',i4,i3,i3,i3,a3,1x,8f10.3)
!
!              IF FACTOR = 0 THEN EXCLUDE FROM QUALIFYING GEN
!
               IF (RENFAC(ICAP) .GT. 0.0) THEN
                  RPSGEN = MAX(0.0,PLTGEN - UPRNWEXG(EFDECP(ICAP)) * EXSGEN(EFDECP(ICAP),REG))
                  UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + RPSGEN
                  UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + RPSGEN
                  RENGEN(REG) = RENGEN(REG) + RPSGEN * RENFAC(ICAP)
                  RENGEN(MNUMNR) = RENGEN(MNUMNR) + RPSGEN * RENFAC(ICAP)
               END IF
               TOTGEN(REG) = TOTGEN(REG) + PLTGEN
               TOTGEN(MNUMNR) = TOTGEN(MNUMNR) + PLTGEN
!              IF BAS FACTOR > 0 THEN EXCLUDE EXISTING GEN FROM TOTAL BASELINE
               IF (IECP .GT. 0)THEN
                  IF (UPRNWBAS(IECP) .GT. 0.0) THEN
                     TOTGEN(REG) = TOTGEN(REG) - EXPGEN(IECP,REG)
                     TOTGEN(MNUMNR) = TOTGEN(MNUMNR) - EXPGEN(IECP,REG)
                     EXEMSLS(REG) = EXEMSLS(REG) + EXPGEN(IECP,REG)
!                                   (1.0 + EQTDLS*ULOSSADJ(CURIYR))
                     EXEMSLS(MNUMNR) = EXEMSLS(MNUMNR) + EXPGEN(IECP,REG)
!                                      (1.0 + EQTDLS*ULOSSADJ(CURIYR))
!     if (curitr .gt. maxitr .and. iecp .eq. wihy)write(6,4456) curiyr+1989,reg,icap,iecp,uplntcd(iecp),  &
!                    expgen(iecp,reg),exemsls(reg),exemsls(mnumnr)
!4456 format(1h ,'!exem',i4,i3,i3,i3,a3,1x,8f10.3)
                  END IF
               END IF
            END DO
!
!           ACCOUNT FOR COGEN -- DOE BILL INCLUDES OWN USE
!
           IF (UPRNWCOG .EQ. 1 .OR. UPRNWCOG .EQ. 2)THEN
            RENGEN(REG) = RENGEN(REG) + EDRPSRG
            RENGEN(MNUMNR) = RENGEN(MNUMNR) + EDRPSRG
!           ACCOUNT FOR BONUS CREDITS FOR END-USE PV AND WIND, IF APPROPRIATE
            IF (ABS(UCSPVSHR(REG) - UPRNWSHR(WIPV)) .GT. ECP_MIN)THEN
               RENGEN(REG) = RENGEN(REG) + ECRPSRG(WIPV) * (UCSPVSHR(REG) - UPRNWSHR(WIPV))
               RENGEN(MNUMNR) = RENGEN(MNUMNR) + ECRPSRG(WIPV) * (UCSPVSHR(REG) - UPRNWSHR(WIPV))
            END IF
            IF (ABS(UCWNDSHR(REG) - UPRNWSHR(WIWN)) .GT. ECP_MIN)THEN
               RENGEN(REG) = RENGEN(REG) + ECRPSRG(WIWN) * (UCWNDSHR(REG) - UPRNWSHR(WIWN))
               RENGEN(MNUMNR) = RENGEN(MNUMNR) + ECRPSRG(WIWN) * (UCWNDSHR(REG) - UPRNWSHR(WIWN))
            END IF
            TOTGEN(REG) = TOTGEN(REG) + EDRPSTG
            TOTGEN(MNUMNR) = TOTGEN(MNUMNR) + EDRPSTG
            IF (UPRNWCAS .EQ. 3 .AND. UPRNWCOG .EQ. 2) THEN
               RENGEN(REG) = RENGEN(REG) + EDRPSRO
               RENGEN(MNUMNR) = RENGEN(MNUMNR) + EDRPSRO
               IF (ABS(UCSPVSHR(REG) - UPRNWSHR(WIPV)) .GT. ECP_MIN)THEN
                  RENGEN(REG) = RENGEN(REG) + ECRPSRG(WIPV) * (UCSPVSHR(REG) - UPRNWSHR(WIPV))
                  RENGEN(MNUMNR) = RENGEN(MNUMNR) + ECRPSRG(WIPV) * (UCSPVSHR(REG) - UPRNWSHR(WIPV))
               END IF
               IF (ABS(UCWNDSHR(REG) - UPRNWSHR(WIWN)) .GT. ECP_MIN)THEN
                  RENGEN(REG) = RENGEN(REG) + ECRPSRG(WIWN) * (UCWNDSHR(REG) - UPRNWSHR(WIWN))
                  RENGEN(MNUMNR) = RENGEN(MNUMNR) + ECRPSRG(WIWN) * (UCWNDSHR(REG) - UPRNWSHR(WIWN))
               END IF
               TOTGEN(REG) = TOTGEN(REG) + EDRPSTO
               TOTGEN(MNUMNR) = TOTGEN(MNUMNR) + EDRPSTO
            END IF
! by plant type
! biomass
               ICAP = UIBMS
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRG(WIWD)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRG(WIWD)
            IF (UPRNWCAS .EQ. 3 .AND. UPRNWCOG .EQ. 2) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRO(WIWD)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRO(WIWD)
             END IF
            END IF
! biomass ccs
               ICAP = UIBIG
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRG(WIBI)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRG(WIBI)
            IF (UPRNWCAS .EQ. 3 .AND. UPRNWCOG .EQ. 2) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRO(WIBI)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRO(WIBI)
             END IF
            END IF
! solar PV
               ICAP = UISPV
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRG(WIPV)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRG(WIPV)
            IF (UPRNWCAS .EQ. 3 .AND. UPRNWCOG .EQ. 2) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRO(WIPV)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRO(WIPV)
              END IF
            END IF
! solar PV - Fixed
               ICAP = UIPVT
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRG(WIPT)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRG(WIPT)
            IF (UPRNWCAS .EQ. 3 .AND. UPRNWCOG .EQ. 2) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRO(WIPT)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRO(WIPT)
              END IF
            END IF
! wind
               ICAP = UIWND
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRG(WIWN)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRG(WIWN)
            IF (UPRNWCAS .EQ. 3 .AND. UPRNWCOG .EQ. 2) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRO(WIWN)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRO(WIWN)
              END IF
            END IF
! wind low speed
               ICAP = UIWNL
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRG(WIWL)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRG(WIWL)
            IF (UPRNWCAS .EQ. 3 .AND. UPRNWCOG .EQ. 2) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRO(WIWL)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRO(WIWL)
              END IF
            END IF
!
! msw
               ICAP = UIMSW
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRG(WIMS)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRG(WIMS)
            IF (UPRNWCAS .EQ. 3 .AND. UPRNWCOG .EQ. 2) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSN(ICAP,REG) = UGNRPSN(ICAP,REG) + ECRPSRO(WIMS)
               UGNRPSN(ICAP,MNUMNR) = UGNRPSN(ICAP,MNUMNR) + ECRPSRO(WIMS)
              END IF
            END IF
!          Bingaman Bill with CHP Credits
           ELSE IF (UPRNWCOG .EQ. 3)THEN
!             Residential
              DO CEN = 1 , MNUMCR - 1
                 RENGEN(REG) = RENGEN(REG) + RCHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,1) * 0.001
                 TOTGEN(REG) = TOTGEN(REG) + RCHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,1) * 0.001
                 UGNCHPN(1,REG) = UGNCHPN(1,REG) + RCHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,1) * 0.001
              END DO
!             Commercial
              DO CEN = 1 , MNUMCR - 1
                 RENGEN(REG) = RENGEN(REG) + KCHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,2) * 0.001
                 TOTGEN(REG) = TOTGEN(REG) + KCHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,2) * 0.001
                 UGNCHPN(2,REG) = UGNCHPN(2,REG) + KCHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,2) * 0.001
              END DO
!             Industrial
              DO CEN = 1 , MNUMCR - 1
                 RENGEN(REG) = RENGEN(REG) + ICHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,3) * 0.001
                 TOTGEN(REG) = TOTGEN(REG) + ICHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,3) * 0.001
                 UGNCHPN(3,REG) = UGNCHPN(3,REG) + ICHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,3) * 0.001
              END DO
!             Refineries
              DO CEN = 1 , MNUMCR - 1
                 RENGEN(REG) = RENGEN(REG) + OCHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,3) * 0.001
                 TOTGEN(REG) = TOTGEN(REG) + OCHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,3) * 0.001
                 UGNCHPN(4,REG) = UGNCHPN(4,REG) + OCHPCESGEN(CEN,CURIYR) * MAPPCTON(REG,CEN,3) * 0.001
              END DO
              IF (REG .EQ. UNRGNS)THEN
                 RENGEN(MNUMNR) = RENGEN(MNUMCR) + (ICHPCESGEN(MNUMCR,CURIYR) + OCHPCESGEN(MNUMCR,CURIYR) +  &
                                  KCHPCESGEN(MNUMCR,CURIYR) + RCHPCESGEN(MNUMCR,CURIYR)) * 0.001
                 TOTGEN(MNUMNR) = TOTGEN(MNUMCR) + (ICHPCESGEN(MNUMCR,CURIYR) + OCHPCESGEN(MNUMCR,CURIYR) +  &
                                  KCHPCESGEN(MNUMCR,CURIYR) + RCHPCESGEN(MNUMCR,CURIYR)) * 0.001
                 UGNCHPN(1,MNUMNR) = RCHPCESGEN(MNUMCR,CURIYR) * 0.001
                 UGNCHPN(2,MNUMNR) = KCHPCESGEN(MNUMCR,CURIYR) * 0.001
                 UGNCHPN(3,MNUMNR) = ICHPCESGEN(MNUMCR,CURIYR) * 0.001
                 UGNCHPN(4,MNUMNR) = OCHPCESGEN(MNUMCR,CURIYR) * 0.001
              END IF
           END IF
!
          URPSRGN(REG) = RENGEN(REG)
          IF (UPRNWCAS .NE. 3) THEN
            URPSTGN(REG) = TOTGEN(REG)
          ELSE
            URPSTGN(REG) = QELASN(REG,CURIYR) * 0.001 - EXEMSLS(REG)
          END IF
!       ACTUAL RPS CREDITS REQUIRED
          IF (UPRNWBND(CURIYR) .GT. 0.001)THEN
            URPSCRN(REG) = UPRNWBND(CURIYR) * URPSTGN(REG)
          ELSE
            URPSCRN(REG) = 0.0
          END IF
!         do icap = 1 , ECP_D_CAP
!            if (curitr .gt. maxitr)write(6,6666) curiyr+1989,reg,uplntcd(icap),  &
!               pgen(icap),exsgen(icap,reg),exsgen(icap,mnumnr)
!6666 format(1h ,'!exsgen',i4,i3,a3,1x,3f10.3)
!         end do
         END DO
!
!       DETERMINE REGIONAL SALES/PURCHASES OF RENEWABLE CREDITS
!       IN DOE BILL, CREDITS ARE BASED ON %RENEWABLE GENERATION
!       OF TOTAL SALES.  OTHER BILLS USE %RENEWABLE SALES VERSUS
!       TOTAL SALES (SO RATIO USES RENEW GEN / TOTAL GEN)
!
            URPSRGN(MNUMNR) = RENGEN(MNUMNR)
         IF (UPRNWCAS .NE. 3) THEN
            URPSTGN(MNUMNR) = TOTGEN(MNUMNR)
         ELSE
            URPSTGN(MNUMNR) = (QELAS(MNUMCR,CURIYR) / 3.412) - (QELASN(MNUMNR-1,CURIYR) + QELASN(MNUMNR-2,CURIYR)) &
                                    * 0.001 - EXEMSLS(MNUMNR)
!            if (curitr .gt. maxitr)write(6,6667) curiyr+1989,  &
!           URPSTGN(MNUMNR) , (QELASN(MNUMNR,CURIYR) - QELASN(MNUMNR-1,CURIYR) - QELASN(MNUMNR-2,CURIYR)) &
!                                   * 0.001 , EXEMSLS(MNUMNR)
!6667 format(1h ,'!cesbas',i4,5f10.3)
         END IF
!       ACTUAL RPS CREDITS REQUIRED
          IF (UPRNWBND(CURIYR) .GT. 0.001)THEN
            URPSCRN(MNUMNR) = UPRNWBND(CURIYR) * URPSTGN(MNUMNR)
          ELSE
            URPSCRN(MNUMNR) = 0.0
          END IF
!       ACTUAL RPS CREDITS ACHIEVED
            URPSPCT(MNUMNR) = URPSRGN(MNUMNR) / URPSTGN(MNUMNR)
!           RENPCT = MIN(URPSPCT(MNUMNR),UPRNWBND(CURIYR))
            RENPCT = UPRNWBND(CURIYR)
!        IF(UPRNWCAP .GT. 0.0)RENPCT = UPRNWBND(CURIYR)
!
!
         IF (UPRNWBND(CURIYR) .GT. 0.001 .AND. CURIYR .GT. (UESTYR - UHBSYR)) THEN
           IF (UPRNWCRD .EQ. 1) THEN          ! Rolling average type credit
!
            credcost(CURIYR) = (URPSCRN(MNUMNR) - URPSCRL(MNUMNR))* &
               EPRPSPR(CURIYR)
!
!           credcost is 0 before the standard
!           this catches the old grandfathered stuff multiplies it by the
!           current credit price, and then catches all the other years
!           this is the equivalent of long-term contracts
!
            TOTCRED =  0.0
            DO IY = (UESTYR - UHBSYR) , CURIYR
               TOTCRED = TOTCRED + credcost(IY)
            END DO
            renewcr(CURIYR) = TOTCRED/URPSCRN(MNUMNR)

           ELSEIF (UPRNWCRD .EQ. 2) THEN          ! no rolling avg, set to ECP credit price
            renewcr(CURIYR) = EPRPSPR(CURIYR)

           ENDIF
         ELSE
           renewcr(CURIYR) = 0.0
           credcost(CURIYR) = 0.0
         END IF
!
!        DETERMINE CREDIT REVENUES, IF ANY
!
         IF (EPRPSPR(CURIYR) .GT. 0.0) THEN
!
!           RECONCILE REGIONAL TOTALS
!
            DO REG = 1 , UNRGNS
               IF (UPRNWCAS .NE. 3) THEN
                  URPSPCT(REG) = RENGEN(REG) / TOTGEN(REG)
                  URPSCRD(REG) = RENGEN(REG) - &
                                 RENPCT * TOTGEN(REG)
               ELSE
                  URPSPCT(REG) = RENGEN(REG) / &
                                (QELASN(REG,CURIYR) * 0.001 - EXEMSLS(REG))
                  URPSCRD(REG) = RENGEN(REG) - &
                     RENPCT * (QELASN(REG,CURIYR) * 0.001 - EXEMSLS(REG))
               END IF
            END DO
            RENPUR = 0.0
            RENSAL = 0.0
            DO REG = 1 , UNRGNS
               IF (URPSCRD(REG) .LT. 0.0) THEN
                  RENPUR = RENPUR - URPSCRD(REG)
               ELSE
                  RENSAL = RENSAL + URPSCRD(REG)
               END IF
            END DO
!
!           INSURE THAT SALES EQUALS PURCHASES, ADD PURCHASES OR SUBTRACT SALES
!           FROM REVENUE REQUIREMENTS
!
            DO REG = 1 , UNRGNS
               CALL GETOUT(CURIYR,REG)
               IF (UPRNWCAS .NE. 3 .AND. URPSCRD(REG) .GT. 0.0) &
                   URPSCRD(REG) = URPSCRD(REG) * RENPUR / RENSAL
               ERRPS = -URPSCRD(REG) * RENEWCR(CURIYR)
               CALL STROUT(CURIYR,REG)
            END DO
         END IF
      END IF
!
      RETURN
      END
!
!     ==================================================================
!     - ELRPSCRR -
!     THIS SUBROUTINE DETERMINES REGIONAL RPS CREDIT PRICES
!     ==================================================================

      SUBROUTINE ELRPSCRR

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'dispout'
      include'dispett'
      include'uefdout'
      include'udatout'
      include'ecpcntl'
      include'bildin'
      include'bildout'
      include'uecpout'
      include'cogen'
      include'eusprc'
      include'efpint'
      include'emission'
      include'cdsparms'
      include'csapr'
      include'emmemis'

      REAL*4 UGNRPSNR(EFD_D_CAP,MNUMNR)
      REAL*4 URPSRGNR(MNUMNR),URPSTGNR(MNUMNR)
      REAL*4 URPSCRNR(MNUMNR),URPSCRLR(MNUMNR)
      REAL*4 URPSPCTR(MNUMNR)
      REAL*4 EXEMSLSR(MNUMNR)
      REAL*4 PLTGEN,RPSGEN
      REAL*4 CREDCOSTR(MNUMNR,CURIYR)

      INTEGER*4 REG,OWN,PLT,IY,credstart,ICAP, IECP
      INTEGER*4 EFDECP(EFD_D_CAP)
      REAL*4 RENFAC(EFD_D_CAP),TOTGEN(MNUMNR),RENGEN(MNUMNR)
      REAL*4 RENPCT,RENSAL,RENPUR,CRDPRC,TOTCRED
      REAL*4 COFGEN

!     LOOP OVER REGIONS AND CHECK IF NATIONAL AND/OR REGIONAL RPS

      DO REG = 1 , UNRGNS
           RENEWCRR(REG,CURIYR) = 0.0
           CREDCOSTR(REG,CURIYR) = 0.0
         IF (UPRNWCAS .GT. 0 .OR. UPRNWCASR(REG) .GT. 0) THEN

!     IF NATIONAL RPS ONLY, THEN REGIONAL PRICE IS SAME AS NATIONAL

            IF (UPRNWCASR(REG) .LE. 0) THEN
!               IF (ST_RPS_SW .EQ. 0) THEN
             RENEWCRR(REG,CURIYR) = RENEWCR(CURIYR)
             CREDCOSTR(REG,CURIYR) = 0.0
 !         ELSE
  !                RENEWCRR(REG,CURIYR) = ST_RPS_EMM_P(REG,CURIYR)
   !            END IF
            ELSE

!     STORE RPS CREDITS FOR LAG YEAR

               IF (CURIYR .GT. (UESTYR-UHBSYR) .AND. CURITR .EQ. 1) THEN
              URPSCRLR(REG) = URPSCRNR(REG)
           END IF

!     FOR EACH EFD RENEWABLE TYPE IDENTIFY CORRESPONDING ECP TYPE
!     AND GET FRACTION INCLUDED IN SATISFYING RPS

         DO PLT = 1 , EFD_D_CAP
            EFDECP(PLT) = 0
            RENFAC(PLT) = 0.0
         END DO
            EFDECP(UIBMS) = WIWD
            EFDECP(UIBIG) = WIBI
            EFDECP(UIMSW) = WIMS
            EFDECP(UIGTH) = WIGT
            EFDECP(UIHYC) = WIHY
            EFDECP(UIHYR) = WIPS
            EFDECP(UIDST) = WIDS
            EFDECP(UIWND) = WIWN
            EFDECP(UIWNL) = WIWL
            EFDECP(UIWFS) = WIWF
            EFDECP(UISTH) = WISO
            EFDECP(UISPV) = WIPV
            EFDECP(UIPVT) = WIPT
            EFDECP(UICNU) = WICN
            EFDECP(UIANC) = WIAN
            EFDECP(UISMR) = WISM            
            EFDECP(UICAS) = WIIS
            EFDECP(UICOQ) = WIPQ     
            EFDECP(UIACS) = WICS
            EFDECP(UIAC2) = WIA2
            EFDECP(UICCG) = WICC
            EFDECP(UICCX) = WICC
            EFDECP(UIACC) = WIAC
         DO PLT = 1 , EFD_D_CAP
                  IF (EFDECP(PLT) .GT. 0) THEN
            RENFAC(PLT) = UPRNWSHRR(EFDECP(PLT),REG)
          END IF
         END DO

!     INITIALIZE TOTALS

            TOTGEN(REG) = 0.0
            RENGEN(REG) = 0.0
            EXEMSLSR(REG) = 0.0
           IF (REG .EQ. 1)THEN
            TOTGEN(MNUMNR) = 0.0
            RENGEN(MNUMNR) = 0.0
            EXEMSLSR(MNUMNR) = 0.0
           END IF

!           READ INPUT DATA

            CALL GETIN(1,REG)

!           WRITE(6,3979) CURIRUN, CURIYR+1989, CURITR, REG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3979       FORMAT(1X,"UEFD_05174_EEITAJ_GET",4(":",I4),3(":",F12.3))

!           READ OUTPUT DATA

            CALL GETOUT(CURIYR,REG)

!           DETERMINE RENEWABLE AND TOTAL GENERATION

!           LOOP OVER FOSSIL PLANTS

            DO PLT = 1 , EFD_D_DSP

!              IF FACTOR < 0 THEN EXCLUDE FROM RENEWABLE AND TOTAL GEN (E.G., JEFFORDS)

               IF (RENFAC(PLT) .GE. 0.0) THEN

!                 UTILITY, IPP, AND NONTRADITIONAL COGEN

                     PLTGEN = 0.0
                  DO OWN = 1 , 4
                     PLTGEN = PLTGEN + EQPGN(PLT,OWN) * 0.001
                  END DO
                    IF (RENFAC(PLT) .GT. 0.0) THEN

!                 EXCLUDE EXISTING GENERATION, IF NOT COUNTED TOWARDS RPS

                     RPSGEN = MAX(0.0,PLTGEN - UPRNWEXGR(EFDECP(PLT),REG) * EXSGEN(EFDECP(PLT),REG))
                     UGNRPSNR(PLT,REG) = UGNRPSNR(PLT,REG) + RPSGEN
                     UGNRPSNR(PLT,MNUMNR) = UGNRPSNR(PLT,MNUMNR) + RPSGEN
                     RENGEN(REG) = RENGEN(REG) + RPSGEN * RENFAC(PLT)
                     RENGEN(MNUMNR) = RENGEN(MNUMNR) + RPSGEN * RENFAC(PLT)
                    END IF
                     TOTGEN(REG) = TOTGEN(REG) + PLTGEN
                     TOTGEN(MNUMNR) = TOTGEN(MNUMNR) + PLTGEN
               ELSE
                  DO OWN = 1, 4
                        EXEMSLSR(REG) = EXEMSLSR(REG) + EQPGN(PLT,OWN)*.001/ (1.0 + EQTDLS*ULOSSADJ(CURIYR))
                        EXEMSLSR(MNUMNR) = EXEMSLSR(MNUMNR) + EQPGN(PLT,OWN)*.001/ (1.0 + EQTDLS*ULOSSADJ(CURIYR))
                  ENDDO
               END IF
            END DO

! account for cofiring (the difference between total biomass generation and dedicated)

              PLT = UIBMS
              COFGEN = 0.0
            DO OWN = 1 , 4
             IF (RENFAC(PLT) .GT. 0.0) THEN
              COFGEN = COFGEN + (EQFGN(UIWD,OWN) - EQPGN(PLT,OWN)) * 0.001
             END IF
            END DO
              COFGEN = MAX(COFGEN,0.0)
            RENGEN(REG) = RENGEN(REG) + COFGEN
            RENGEN(MNUMNR) = RENGEN(MNUMNR) + COFGEN

!           ACCOUNT FOR IMPORTS (GEN + NET IMPORTS LESS LOSSES = SALES)

               TOTGEN(REG) = TOTGEN(REG) + (ETIMPF + ETIMPE - ETEXPF - ETEXPE) * 0.001
               TOTGEN(MNUMNR) = TOTGEN(MNUMNR) + (ETIMPF + ETIMPE - ETEXPF - ETEXPE) * 0.001

!           LOOP OVER RENEWABLE PLANTS

            DO PLT = 1 , EFD_D_RNW
               ICAP = PLT + EFD_D_DSP

!              IF FACTOR < 0 THEN EXCLUDE FROM RENEWABLE AND TOTAL GEN (E.G., JEFFORDS)

               IF (RENFAC(ICAP) .GE. 0.0) THEN

!                 UTILITY, IPP, AND NONTRADITIONAL

                     PLTGEN = 0.0
                  DO OWN = 1 , 4
                     PLTGEN = PLTGEN + EQHGN(PLT,OWN) * 0.001
                  END DO
                    IF (RENFAC(ICAP) .GT. 0.0) THEN

!                 EXCLUDE EXISTING GENERATION, IF NOT COUNTED TOWARDS RPS

                     RPSGEN = MAX(0.0,PLTGEN - UPRNWEXGR(EFDECP(ICAP),REG) * EXSGEN(EFDECP(ICAP),REG))
                     UGNRPSNR(ICAP,REG) = UGNRPSNR(ICAP,REG) + RPSGEN
                     UGNRPSNR(ICAP,MNUMNR) = UGNRPSNR(ICAP,MNUMNR) + RPSGEN
                     RENGEN(REG) = RENGEN(REG) + RPSGEN * RENFAC(ICAP)
                     RENGEN(MNUMNR) = RENGEN(MNUMNR) + RPSGEN * RENFAC(ICAP)
                    END IF
                     TOTGEN(REG) = TOTGEN(REG) + PLTGEN
                     TOTGEN(MNUMNR) = TOTGEN(MNUMNR) + PLTGEN
               ELSE
                  DO OWN = 1, 4
                        EXEMSLSR(REG) = EXEMSLSR(REG) + EQHGN(PLT,OWN)*.001/ (1.0 + EQTDLS*ULOSSADJ(CURIYR))
                        EXEMSLSR(MNUMNR) = EXEMSLSR(MNUMNR) + EQHGN(PLT,OWN)*.001/ (1.0 + EQTDLS*ULOSSADJ(CURIYR))
                  ENDDO
               END IF
            END DO

!           ACCOUNT FOR COGEN -- DOE BILL INCLUDES OWN USE

            RENGEN(REG) = RENGEN(REG) + EDRPSRG
            RENGEN(MNUMNR) = RENGEN(MNUMNR) + EDRPSRG
            TOTGEN(REG) = TOTGEN(REG) + EDRPSTG
            TOTGEN(MNUMNR) = TOTGEN(MNUMNR) + EDRPSTG
            IF (UPRNWCASR(REG) .EQ. 3) THEN
               RENGEN(REG) = RENGEN(REG) + EDRPSRO
               RENGEN(MNUMNR) = RENGEN(MNUMNR) + EDRPSRO
               TOTGEN(REG) = TOTGEN(REG) + EDRPSTO
               TOTGEN(MNUMNR) = TOTGEN(MNUMNR) + EDRPSTO
            END IF

! by plant type

! biomass

               ICAP = UIBMS
               UGNRPSNR(ICAP,REG) = UGNRPSNR(ICAP,REG) + ECRPSRG(WIWD)
               UGNRPSNR(ICAP,MNUMNR) = UGNRPSNR(ICAP,MNUMNR) + ECRPSRG(WIWD)
            IF (UPRNWCASR(REG) .EQ. 3) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSNR(ICAP,REG) = UGNRPSNR(ICAP,REG) + ECRPSRO(WIWD)
               UGNRPSNR(ICAP,MNUMNR) = UGNRPSNR(ICAP,MNUMNR) + ECRPSRO(WIWD)
              END IF
            END IF

! biomass ccs

               ICAP = UIBIG
               UGNRPSNR(ICAP,REG) = UGNRPSNR(ICAP,REG) + ECRPSRG(WIBI)
               UGNRPSNR(ICAP,MNUMNR) = UGNRPSNR(ICAP,MNUMNR) + ECRPSRG(WIBI)
            IF (UPRNWCASR(REG) .EQ. 3) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSNR(ICAP,REG) = UGNRPSNR(ICAP,REG) + ECRPSRO(WIBI)
               UGNRPSNR(ICAP,MNUMNR) = UGNRPSNR(ICAP,MNUMNR) + ECRPSRO(WIBI)
              END IF
            END IF

! solar PV

               ICAP = UISPV
               UGNRPSNR(ICAP,REG) = UGNRPSNR(ICAP,REG) + ECRPSRG(WIPV)
               UGNRPSNR(ICAP,MNUMNR) = UGNRPSNR(ICAP,MNUMNR) + ECRPSRG(WIPV)
            IF (UPRNWCASR(REG) .EQ. 3) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSNR(ICAP,REG) = UGNRPSNR(ICAP,REG) + ECRPSRO(WIPV)
               UGNRPSNR(ICAP,MNUMNR) = UGNRPSNR(ICAP,MNUMNR) + ECRPSRO(WIPV)
              END IF
            END IF

! msw

               ICAP = UIMSW
               UGNRPSNR(ICAP,REG) = UGNRPSNR(ICAP,REG) + ECRPSRG(WIMS)
               UGNRPSNR(ICAP,MNUMNR) = UGNRPSNR(ICAP,MNUMNR) + ECRPSRG(WIMS)
            IF (UPRNWCASR(REG) .EQ. 3) THEN
              IF (RENFAC(ICAP) .GT. 0.0)THEN
               UGNRPSNR(ICAP,REG) = UGNRPSNR(ICAP,REG) + ECRPSRO(WIMS)
               UGNRPSNR(ICAP,MNUMNR) = UGNRPSNR(ICAP,MNUMNR) + ECRPSRO(WIMS)
              END IF
            END IF

               URPSRGNR(REG) = RENGEN(REG)
            IF (UPRNWCASR(REG) .NE. 3) THEN
               URPSTGNR(REG) = TOTGEN(REG)
            ELSE
               URPSTGNR(REG) = QELASN(REG,CURIYR) * 0.001 - EXEMSLSR(REG)
            END IF

!       ACTUAL RPS CREDITS REQUIRED

            IF (UPRNWBNDR(CURIYR,REG) .GT. 0.001)THEN
               URPSCRNR(REG) = UPRNWBNDR(CURIYR,REG) * URPSTGNR(REG)
            ELSE
               URPSCRNR(REG) = 0.0
            END IF

               IF (ST_RPS_SW .GT. 0) THEN
                  URPSCRNR(REG) = URPSCRNR(REG) + ST_RPS_EMM_Q(REG,CURIYR)
               END IF

!       SUM UP NATIONAL TOTALS OF QUALIFIED RENEWABLE GENERATION,
!       TOTAL GENERATION, AND RENEWABLE CREDITS

            URPSRGNR(MNUMNR) = URPSRGNR(MNUMNR) + URPSRGNR(REG)
            URPSTGNR(MNUMNR) = URPSTGNR(MNUMNR) + URPSTGNR(REG)
            URPSCRNR(MNUMNR) = URPSCRNR(MNUMNR) + URPSCRNR(REG)
               IF (REG .EQ. UNRGNS) URPSPCTR(MNUMNR) = URPSRGNR(MNUMNR) / URPSTGNR(MNUMNR)

            IF (UPRNWBNDR(CURIYR,REG) .GT. 0.001 .AND. CURIYR .GT. (UESTYR - UHBSYR)) THEN
             IF (UPRNWCRD .EQ. 1) THEN          ! Rolling average type credit

                     credcostr(REG,CURIYR) = (URPSCRNR(REG) - URPSCRLR(REG))* EPRPSPRR(REG,CURIYR)

!           credcost is 0 before the standard
!           this catches the old grandfathered stuff multiplies it by the
!           current credit price, and then catches all the other years
!           this is the equivalent of long-term contracts

               TOTCRED =  0.0
              DO IY = (UESTYR - UHBSYR) , CURIYR
               TOTCRED = TOTCRED + credcostr(REG,IY)
              END DO
               renewcrr(REG,CURIYR) = TOTCRED/URPSCRNR(REG)

             ELSEIF (UPRNWCRD .EQ. 2) THEN          ! no rolling avg, set to ECP credit price
               renewcrr(REG,CURIYR) = EPRPSPRR(REG,CURIYR)

             ENDIF
            END IF
          END IF                                  ! UPRNWCASR
        END IF                                    ! UPRNWCAS
      END DO                                      ! REG
!
      RETURN
      END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   SUBROUTINE GPSINIT
!
!   Calculates GPS subsidies

      SUBROUTINE GPSINIT

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'emission'
      include'uefdout'
      include'dispout'
      include'control'
      include'ecpcntl'
      include'bildin'
      include'uecpout'
      include'cogen'
      include'ecp_nuc'

      INTEGER IRG,IOWN,ICAP,GPSYR,GYR,DYR
      INTEGER IRNW,IDGN,HISTYR,ISO2
      REAL so2emis,noxemis,carprice,tmpcsub,tmpssub,tmpnsub,tmphsub
      REAL nucgen(MNUMNR),biogen(MNUMNR),mswgen(MNUMNR),histwd(MNUMNR),histmsw(MNUMNR)
      real NUC90(MNUMNR)

      DO IRG = 1, MNUMNR
         basecar(IRG) = 0.0
         DO ISO2 = 1 , NUM_SO2_GRP
            baseso2(IRG,ISO2) = 0.0
         END DO
         basenox(IRG) = 0.0
         basehg(IRG) = 0.0
      ENDDO
!
      GPSYR = CURIYR

      HISTYR = UPSTYR - UHBSYR
      DO IRG = 1, UNRGNS
        histwd(IRG) = (UGNWDNR(1,IRG,HISTYR) + UGNWDNR(2,IRG,HISTYR) -         &
                       UGNCFNR(1,IRG,HISTYR) - UGNCFNR(2,IRG,HISTYR)) * 1000.0 &
                     + CGNTGEN(IRG,HISTYR,7,1) + CGNTGEN(IRG,HISTYR,7,2)
        if (histwd(IRG) .LT. 0.0) histwd(IRG) = 0.0
        histmsw(IRG) = (UGNMSNR(1,IRG,HISTYR) + UGNMSNR(2,IRG,HISTYR)) * 1000.0 &
                     + CGNTGEN(IRG,HISTYR,6,1) + CGNTGEN(IRG,HISTYR,6,2)
        nuc90(IRG) = (UGNURNR(1,IRG,HISTYR) + UGNURNR(2,IRG,HISTYR)) * 1000.0
      ENDDO

      IF (CURIYR .GE. HISTYR+3) THEN
      DO IRG = 1, UNRGNS
        nucgen(IRG) = 0.0
        biogen(IRG) = 0.0
        mswgen(IRG) = 0.0
       DO GYR = GPSYR-3, GPSYR-1
        CALL GETOUT(GYR,IRG)
        DO IOWN = 1, 4
        DO ICAP = 1, EFD_D_DSP
          IF (GPSCSW(ICAP) .eq. 1) THEN
           IF (ICAP .eq. UICNU) THEN
             basecar(IRG) = basecar(IRG) + EQPGN(ICAP,IOWN)
             basecar(MNUMNR) = basecar(MNUMNR) + EQPGN(ICAP,IOWN)
             nucgen(IRG) = nucgen(IRG) + EQPGN(ICAP,IOWN)
           ELSEIF (ICAP .eq. UIBMS .OR. ICAP .eq. UIBIG) THEN
             basecar(IRG) = basecar(IRG) + EQPGN(ICAP,IOWN)
             basecar(MNUMNR) = basecar(MNUMNR) + EQPGN(ICAP,IOWN)
             biogen(IRG) = biogen(IRG) + EQPGN(ICAP,IOWN)
           ELSE
             basecar(IRG) = basecar(IRG) + EQPGN(ICAP,IOWN)
             basecar(MNUMNR) = basecar(MNUMNR) + EQPGN(ICAP,IOWN)
           ENDIF
          ENDIF
          IF (GPSSSW(ICAP) .eq. 1) then
!jj          ISO2 = SO2_GRP_BY_CLRG(UPCLRG(IRG))
           DO ISO2 = 1 , NUM_SO2_GRP
            IF (SO2_SHR_BY_CLRG(UPCLRG(IRG),ISO2) .GT. 0.0)THEN
             baseso2(IRG,ISO2) = baseso2(IRG,ISO2) + EQPGN(ICAP,IOWN) * SO2_SHR_BY_CLRG(UPCLRG(IRG),ISO2)
             baseso2(MNUMNR,ISO2) = baseso2(MNUMNR,ISO2) + EQPGN(ICAP,IOWN) * SO2_SHR_BY_CLRG(UPCLRG(IRG),ISO2)
            END IF
           END DO
          ENDIF
          IF (GPSNSW(ICAP) .eq. 1) then
            basenox(IRG) = basenox(IRG) + EQPGN(ICAP,IOWN)
            basenox(MNUMNR) = basenox(MNUMNR) + EQPGN(ICAP,IOWN)
          ENDIF
          IF (GPSHSW(ICAP) .eq. 1) then
            basehg(IRG)  = basehg(IRG) + EQPGN(ICAP,IOWN)
            basehg(MNUMNR)  = basehg(MNUMNR) + EQPGN(ICAP,IOWN)
          ENDIF
        ENDDO
        DO IRNW = 1, EFD_D_RNW
          ICAP = EFD_D_DSP + IRNW
          IF (GPSCSW(ICAP) .eq. 1) THEN
            IF (ICAP .eq. UIMSW) THEN
              basecar(IRG) = basecar(IRG) + EQHGN(IRNW,IOWN)
              basecar(MNUMNR) = basecar(MNUMNR) + EQHGN(IRNW,IOWN)
              mswgen(IRG) = mswgen(IRG) + EQHGN(IRNW,IOWN)
            ELSE
              basecar(IRG) = basecar(IRG) + EQHGN(IRNW,IOWN)
              basecar(MNUMNR) = basecar(MNUMNR) + EQHGN(IRNW,IOWN)
            ENDIF
          ENDIF
          IF (GPSSSW(ICAP) .eq. 1) then
!jj          ISO2 = SO2_GRP_BY_CLRG(UPCLRG(IRG))
           DO ISO2 = 1 , NUM_SO2_GRP
            IF (SO2_SHR_BY_CLRG(UPCLRG(IRG),ISO2) .GT. 0.0)THEN
             baseso2(IRG,ISO2) = baseso2(IRG,ISO2) + EQHGN(IRNW,IOWN) * SO2_SHR_BY_CLRG(UPCLRG(IRG),ISO2)
             baseso2(MNUMNR,ISO2) = baseso2(MNUMNR,ISO2) + EQHGN(IRNW,IOWN) * SO2_SHR_BY_CLRG(UPCLRG(IRG),ISO2)
            END IF
           END DO
          ENDIF
          IF (GPSNSW(ICAP) .eq. 1) then
             basenox(IRG) = basenox(IRG) + EQHGN(IRNW,IOWN)
             basenox(MNUMNR) = basenox(MNUMNR) + EQHGN(IRNW,IOWN)
          ENDIF
          IF (GPSHSW(ICAP) .eq. 1) then
             basehg(IRG)  = basehg(IRG) + EQHGN(IRNW,IOWN)
             basehg(MNUMNR)  = basehg(MNUMNR) + EQHGN(IRNW,IOWN)
          ENDIF
        ENDDO
        DO IDGN = 1, EFD_D_DGN
          ICAP = EFD_D_DSP + EFD_D_RNW + IDGN
          IF (GPSCSW(ICAP) .eq. 1)  then
             basecar(IRG) = basecar(IRG) + EQDGN(IDGN,IOWN)
             basecar(MNUMNR) = basecar(MNUMNR) + EQDGN(IDGN,IOWN)
          ENDIF
          IF (GPSSSW(ICAP) .eq. 1) then
!jj          ISO2 = SO2_GRP_BY_CLRG(UPCLRG(IRG))
           DO ISO2 = 1 , NUM_SO2_GRP
            IF (SO2_SHR_BY_CLRG(UPCLRG(IRG),ISO2) .GT. 0.0)THEN
             baseso2(IRG,ISO2) = baseso2(IRG,ISO2) + EQDGN(IDGN,IOWN) * SO2_SHR_BY_CLRG(UPCLRG(IRG),ISO2)
             baseso2(MNUMNR,ISO2) = baseso2(MNUMNR,ISO2) + EQDGN(IDGN,IOWN) * SO2_SHR_BY_CLRG(UPCLRG(IRG),ISO2)
            END IF
           END DO
          ENDIF
          IF (GPSNSW(ICAP) .eq. 1) then
             basenox(IRG) = basenox(IRG) + EQDGN(IDGN,IOWN)
             basenox(MNUMNR) = basenox(MNUMNR) + EQDGN(IDGN,IOWN)
          ENDIF
          IF (GPSHSW(ICAP) .eq. 1) then
             basehg(IRG)  = basehg(IRG) + EQDGN(IDGN,IOWN)
             basehg(MNUMNR)  = basehg(MNUMNR) + EQDGN(IDGN,IOWN)
          ENDIF
        ENDDO
        ENDDO  ! IOWN
       ENDDO   ! GYR
       basecar(IRG) = basecar(IRG) - nuc90(IRG) - histwd(IRG) - histmsw(IRG)
       basecar(MNUMNR) = basecar(MNUMNR) - nuc90(IRG) - histwd(IRG) - histmsw(IRG)
      ENDDO

      DO IRG = 1, UNRGNS
      DO ICAP = 1, EFD_D_CAP
       IF (GPSSYR .LE. 0 .OR. (CURIYR + UHBSYR) .LT. GPSSYR)THEN
           GPSCSUB(ICAP,IRG) = 0.0
           GPSSSUB(ICAP,IRG) = 0.0
           GPSNSUB(ICAP,IRG) = 0.0
           GPSHSUB(ICAP,IRG) = 0.0
       ELSE
!      carbon
        IF (PCAP_CAR .EQ. 2 .AND. GPSCSW(ICAP) .eq. 1) THEN
           GPSCSUB(ICAP,IRG) = EMLIM(1,GPSYR)/basecar(MNUMNR) * EMETAX(1,GPSYR) *1000000.0
           IF (ICAP .EQ. UICNU) THEN
            if (nucgen(IRG) .gt. 0.0 .AND. nucgen(IRG) .gt. 3.0*nuc90(IRG)) then
             GPSCSUB(ICAP,IRG) = GPSCSUB(ICAP,IRG) * (nucgen(IRG) - 3.0*nuc90(IRG))/nucgen(IRG)
            else
             GPSCSUB(ICAP,IRG) = 0.0
            endif
           ELSEIF (ICAP .EQ. UIBMS .OR. ICAP .EQ. UIBIG) THEN
             if (biogen(IRG) .gt. 0.0 .AND. biogen(IRG) .gt. 3.0*histwd(IRG)) then
               GPSCSUB(ICAP,IRG) = GPSCSUB(ICAP,IRG) * (biogen(IRG) - 3.0*histwd(IRG))/biogen(IRG)
             else
               GPSCSUB(ICAP,IRG) = 0.0
             endif
           ELSEIF (ICAP .eq. UIMSW) THEN
             if (mswgen(IRG) .gt. 0.0 .AND. mswgen(IRG) .gt. 3.0*histmsw(IRG)) then
               GPSCSUB(ICAP,IRG) = GPSCSUB(ICAP,IRG) * (mswgen(IRG) - 3.0*histmsw(IRG))/mswgen(IRG)
              else
               GPSCSUB(ICAP,IRG) = 0.0
              endif
           ENDIF
        ELSE
           GPSCSUB(ICAP,IRG) = 0.0
        ENDIF
!      SO2
        IF (PCAP_SO2 .EQ. 2 .AND. GPSSSW(ICAP) .eq. 1) THEN
           GPSSSUB(ICAP,IRG) = 0.0
           DO ISO2 = 1 , NUM_SO2_GRP
              so2emis = EMRFSA(GPSYR,ISO2) + EMELBNK(GPSYR-1,ISO2) - EMELBNK(GPSYR,ISO2)
!             GPSSSUB(ICAP,IRG) = GPSSSUB(ICAP,IRG) + so2emis / baseso2(MNUMNR,ISO2) * EMELPSO2(GPSYR,ISO2) * 0.001
              GPSSSUB(ICAP,IRG) = GPSSSUB(ICAP,IRG) + so2emis / baseso2(MNUMNR,ISO2) * ECP_PSO2(0,GPSYR,ISO2) * 0.001
           END DO
        ELSE
           GPSSSUB(ICAP,IRG) = 0.0
        ENDIF
!      NOX
        IF (PCAP_NOX .EQ. 2 .AND. GPSNSW(ICAP) .eq. 1) THEN
           noxemis = EMRFNA(2,GPSYR) + EMRFNA(3,GPSYR)
           GPSNSUB(ICAP,IRG) = noxemis/basenox(MNUMNR) * EPNOXPR(2,GPSYR)
        ELSE
           GPSNSUB(ICAP,IRG) = 0.0
        ENDIF
!      Mercury
        IF (PCAP_HG .EQ. 2 .AND. GPSHSW(ICAP) .eq. 1) THEN
           GPSHSUB(ICAP,IRG) =  EMLIM(4,GPSYR) / basehg(MNUMNR) * EMEL_PHG(1,GPSYR) * 1000.0
        ELSE
           GPSHSUB(ICAP,IRG) = 0.0
        ENDIF

!  calculate discounted value of subsidy over next 3 years
        tmpcsub = 0.0
        tmpssub = 0.0
        tmpnsub = 0.0
        tmphsub = 0.0
        DO DYR = 1, 3
          tmpcsub = tmpcsub + GPSCSUB(ICAP,IRG) / ( (1 + EPDSCRT)**DYR )
          tmpssub = tmpssub + GPSSSUB(ICAP,IRG) / ( (1 + EPDSCRT)**DYR )
          tmpnsub = tmpnsub + GPSNSUB(ICAP,IRG) / ( (1 + EPDSCRT)**DYR )
          tmphsub = tmphsub + GPSHSUB(ICAP,IRG) / ( (1 + EPDSCRT)**DYR )
        ENDDO
          GPSCSUB(ICAP,IRG) = tmpcsub
          GPSSSUB(ICAP,IRG) = tmpssub
          GPSNSUB(ICAP,IRG) = tmpnsub
          GPSHSUB(ICAP,IRG) = tmphsub
       END IF
      ENDDO   ! ICAP
      ENDDO   ! IRG
      ENDIF   ! (UPSTYR - 1)

      RETURN
      END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
!
!     SUBROUTINE GPSPRC
!
!     Subroutine to calculate adjustments to costs based on emission allocation
!
      SUBROUTINE GPSPRC

      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'emission'
      include 'control'
      include 'ecpcntl'
      include 'uecpout'
      include 'dispout'
      include 'uefdout'

      INTEGER IRG,GPSYR,IPGRP,IDGN,ISO2
      real caralloc,so2alloc,noxalloc,hgalloc,so2emis,noxemis

      IF ((CURIYR + UHBSYR) .GE. GPSSYR) THEN
      GPSYR = CURIYR
      DO IRG = 1, UNRGNS
        CALL GETOUT(GPSYR,IRG)
        IF (PCAP_CAR .EQ. 2) THEN
           caralloc = basecar(IRG) / basecar(MNUMNR) * EMLIM(1,GPSYR) * EMETAX(1,GPSYR) * 1000.0
        ELSE
           caralloc = 0.0
        ENDIF
        IF (PCAP_SO2 .EQ. 2) THEN
           so2alloc = 0.0
           DO ISO2 = 1 , NUM_SO2_GRP
              so2emis = EMRFSA(GPSYR,ISO2) + EMELBNK(GPSYR-1,ISO2) - EMELBNK(GPSYR,ISO2)
!             so2alloc = so2alloc + baseso2(IRG,ISO2) / baseso2(MNUMNR,ISO2) * EMELPSO2(GPSYR,ISO2) * 0.000001
              so2alloc = so2alloc + baseso2(IRG,ISO2) / baseso2(MNUMNR,ISO2) * ECP_PSO2(0,GPSYR,ISO2) * 0.000001
           END DO
        ELSE
           so2alloc = 0.0
        ENDIF
        IF (PCAP_NOX .EQ. 2) THEN
           noxemis = EMRFNA(2,GPSYR) + EMRFNA(3,GPSYR)
           noxalloc = basenox(IRG) / basenox(MNUMNR) * noxemis * EPNOXPR(2,GPSYR) * 0.001
        ELSE
           noxalloc = 0.0
        ENDIF
        IF (PCAP_HG .EQ. 2) THEN
           hgalloc = basehg(IRG) / basehg(MNUMNR) * EMLIM(4,GPSYR) * EMEL_PHG(1,GPSYR)
        ELSE
           hgalloc = 0.0
        ENDIF
!   just adjust ownership 1, since it shouldn't matter
        ERTFL(1) = ERTFL(1) - caralloc
!  other fuels are already adjusted, flows through ELCOST in ELSO2P, ELNOXP, ELHGP
!       ERTSO2(1) = ERTSO2(1) - so2alloc
!       ERTNOX(1) = ERTNOX(1) - noxalloc
!       ERTHG(1) = ERTHG(1) - hgalloc
! adjust plant type fuel costs
        DO IPGRP = 1, EFD_D_DSP
           ERPFL(IPGRP,1) = ERPFL(IPGRP,1) - caralloc * EQPCO2(IPGRP) / ETCO2
        ENDDO
        DO IDGN = 1, EFD_D_DGN
           ERDFL(IDGN,1) = ERDFL(IDGN,1) - caralloc * EQDCO2(IDGN) / ETCO2
        ENDDO
        CALL STROUT(GPSYR,IRG)
      ENDDO
      ENDIF

      RETURN
      END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    SUBROUTINE MLCAR
!
!     Subroutine to adjust regulated prices for carbon allocations in McCain-Lieberman
!      or Lierberman-Warner, or HR2454 (Waxman) when PCAP_CAR = 4
!

      SUBROUTINE MLCAR

      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'emission'
      include 'control'
      include 'dispin'
      include 'dispout'
      include 'elout'
      include 'elcntl'
      include 'emoblk'
      include 'ecpcntl'
      include 'udatout'
      include 'macout'
      include 'uefdout'

      real alloc_gen, alloc_ld, cstall_gen,cstall_ld,elallsh(MNUMNR),elallsh1(mnumnr),elallsh2(mnumnr)
      real carass(3),prcadd  !carbon assessment $/kwh, by fuel - coal, gas, oil
      INTEGER IRG,bsyr,carstyr,carendyr,IOWN,CL
      real ERDSCAR2(MNUMNR)

      COMMON /WAX_PRC/ ERDSCAR2

      data carass/0.43,0.22,0.32/   !nominal mills/kwh
!     data elallsh/0.224,0.082,0.057,0.072,0.057,0.023,0.022,0.046,0.206,0.072,0.043,0.059,0.037/
!     data UTALLOC/20*0.0,6*648.2,10*517.7/
!     data UTALLPCT/0.80/
!
      CARSTYR = 2010
      CARENDYR = 2019
! Store regional carbon emissions for "base" year and determine share
! Calculate 3 year running average if base year = 0
! For HR2454/Waxman allocation is based on both sales and a base period carbon emissions
!
      IF (PCAP_CAR .EQ. 3) THEN

       IF (UCAR_AL_YR .GT. 0) THEN

       IF ((CURIYR + UHBSYR) .EQ. (UCAR_AL_YR + 1))THEN
          ELALLSH(MNUMNR) = 0.0
        DO IRG = 1, UNRGNS
          CALL GETOUT(CURIYR - 1,IRG)
          ELALLSH(IRG) = ETCAR / 1000.0
          ELALLSH(MNUMNR) = ELALLSH(MNUMNR) + ETCAR / 1000.0
        END DO
        DO IRG = 1, UNRGNS
          ELALLSH(IRG) = ELALLSH(IRG) / ELALLSH(MNUMNR)
        END DO
       END IF

       ELSE   ! 3 year running average

       IF ((CURIYR + UHBSYR) .GE. (UESTYR + 3)) THEN
          ELALLSH(MNUMNR) = 0.0
        DO IRG = 1, UNRGNS
         ELALLSH(IRG) = 0.0
         DO BSYR = 1,3
          CALL GETOUT(CURIYR - BSYR,IRG)
          ELALLSH(IRG) = ELALLSH(IRG) + ETCAR / 1000.0
          ELALLSH(MNUMNR) = ELALLSH(MNUMNR) + ETCAR / 1000.0
         END DO
        END DO
        DO IRG = 1, UNRGNS
          ELALLSH(IRG) = ELALLSH(IRG) / ELALLSH(MNUMNR)
        END DO
       END IF

       END IF           !UCAR_AL_YR

      ELSEIF (PCAP_CAR .EQ. 4) THEN

       IF ((CURIYR + UHBSYR) .GE. 2009) THEN
          ELALLSH1(MNUMNR) = 0.0
          ELALLSH2(MNUMNR) = 0.0
        DO IRG = 1, UNRGNS
         ELALLSH1(IRG) = 0.0
         ELALLSH2(IRG) = 0.0
         DO BSYR = 17,19    !avg 2006-2008
          CALL GETOUT(BSYR,IRG)
          ELALLSH1(IRG) = ELALLSH1(IRG) + ETCAR / 1000.0
          ELALLSH1(MNUMNR) = ELALLSH1(MNUMNR) + ETCAR / 1000.0

          ELALLSH2(IRG) = ELALLSH2(IRG) + QELASN(IRG,BSYR)
          ELALLSH2(MNUMNR) = ELALLSH2(MNUMNR) + QELASN(IRG,BSYR)
         END DO
        END DO
        DO IRG = 1, UNRGNS
          ELALLSH1(IRG) = ELALLSH1(IRG) / ELALLSH1(MNUMNR)
          ELALLSH2(IRG) = ELALLSH2(IRG) / ELALLSH2(MNUMNR)
          ELALLSH(IRG) = 0.5 * ELALLSH1(IRG) + 0.5 * ELALLSH2(IRG)
        END DO

       ENDIF

      ENDIF           !(PCAP_CAR)

!
!     IF (UCAR_AL_QTY(CURIYR) .GT. 0.0 .OR. UCAR_LDAL_QTY(CURIYR) .GT. 0.0) THEN
      IF (PCAP_CAR .GE. 3) THEN
         if ((TAX_FLAG /= 0) .OR. (PERMIT_FLAG /= 0)) then
            DO IRG = 1, UNRGNS

               CALL GETOUT(CURIYR,IRG)
! For Waxman bill, calculate adder to distribution prices for Sec 114
              prcadd = 0.0
           IF ((CURIYR+UHBSYR) .GE. CARSTYR .and. (CURIYR+UHBSYR) .LE. CARENDYR) THEN
              DO IOWN = 1, 4
               DO CL = 1,34     !coal adder
                  prcadd = prcadd + EQFGN(CL,IOWN) * 0.001 * carass(1) / MC_JPGDP(CURIYR)
               ENDDO
               prcadd = prcadd + (EQFGN(39,IOWN) + EQFGN(40,IOWN) + EQFGN(41,IOWN) + EQFGN(54,IOWN)) * & ! gas
                     carass(2) * 0.001 / MC_JPGDP(CURIYR)
               prcadd = prcadd + (EQFGN(36,IOWN) + EQFGN(37,IOWN) + EQFGN(38,IOWN) + EQFGN(53,IOWN)) * & ! oil
                     carass(3) * 0.001 / MC_JPGDP(CURIYR)
              ENDDO ! iown
           END IF
!
!              Determine the allowances allocated to ratepayers in a region and the resulting revenues
               IF (PCAP_CAR .eq. 3) THEN
               alloc_gen = ELALLSH(IRG) * UCAR_AL_QTY(CURIYR) * UCAR_AL_RTE
               ELSEIF (PCAP_CAR .eq. 4) THEN
               alloc_gen = ELALLSH1(IRG) * UCAR_AL_QTY(CURIYR) * UCAR_AL_RTE
               ENDIF
               alloc_ld  = ELALLSH(IRG) * UCAR_LDAL_QTY(CURIYR)
               cstall_gen = alloc_gen * EMETAX(1,CURIYR) * 1000.0
               cstall_ld  = alloc_ld  * EMETAX(1,CURIYR) * 1000.0
!              if (curiyr .gt. maxitr .or. fcrl .eq. 1)  &
!               write (6,3333) curiyr+1989,irg,ELALLSH(IRG),UCAR_AL_QTY(CURIYR),(1.0 - EM_AUCTION_SH(CURIYR)),UCAR_AL_RTE,  &
!                          EMETAX(1,CURIYR) * 1000.0,alloc,cstall
!3333 format(1h ,'!carall',i4,i3,7f12.3)

               if (FCRL .eq. 1) &
                write(UF_MSG,1668) CURIYR,IRG,CURITR,cstall_gen,cstall_ld,ELALLSH(IRG),ELALLSH1(IRG),ELALLSH2(IRG),prcadd
1668  format(1x,'MLCAR ',3I4,6F16.2)
!              Adjust the revenue requirements for the allowance sales/purchases
               ERTFL(1) = ERTFL(1) - cstall_gen
               ERDSCAR = cstall_ld  - prcadd
               ERDSCAR2(IRG) = cstall_ld  - prcadd

               CALL STROUT(CURIYR,IRG)
            END DO
         END IF
      END IF

      RETURN
      END
!
!     Subroutine to calculate impact of allowance allocation in CPP quantity cases

      SUBROUTINE ELCPPALL
     
      IMPLICIT NONE
 
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispout'
      include'ecpcntl'
      include'e111d'
      include'uefdout'

      INTEGER IR
      REAL ALLOC_GEN,ALLOC_LD,ALLOC_AUC,CSTALL_GEN,CSTALL_LD,CSTALL_AUC

      real ERDSCAR2(MNUMNR)
      COMMON /WAX_PRC/ ERDSCAR2
 
      DO IR = 1, UNRGNS
        IF (CO2_STDTN(IR) .EQ. 2) THEN        ! quantity cap
          CALL GETOUT(CURIYR,IR)

          ALLOC_GEN = CO2_QALLGEN(IR,CURIYR) * CO2_STDQN(IR,CURIYR)     ! co2 in million metric tons
          ALLOC_LD  = CO2_QALLLD(IR,CURIYR) * CO2_STDQN(IR,CURIYR)
          ALLOC_AUC = CO2_QALLAUC(IR,CURIYR) * CO2_STDQN(IR,CURIYR)

          CSTALL_GEN = ALLOC_GEN * ECO2NRPR(IR,CURIYR) * 2204           ! price needs to be converted from $/lb to $/ton
          CSTALL_LD  = ALLOC_LD  * ECO2NRPR(IR,CURIYR) * 2204
          CSTALL_AUC = ALLOC_AUC * ECO2NRPR(IR,CURIYR) * 2204

          IF (CO2_QALLGEN(IR,CURIYR) .LT. 1.0) THEN   ! if all given to generators, then no change to pricing
            ERTFL(1) = ERTFL(1) + CSTALL_AUC + CSTALL_LD        ! o/w increase generation cost by purchases of allowances
            ERDSCAR = CSTALL_LD                                 ! and store amount to reduce distribution costs based on value of allocation to load
            ERDSCAR2(IR) = CSTALL_LD                            ! store separate variable used for competitive prices (same impact to total dist. cost)
          ELSE
            ERDSCAR = 0.0
            ERDSCAR2(IR) = 0.0
          ENDIF
          CALL STROUT(CURIYR,IR)
        ENDIF

        IF (FCRL .EQ. 1) THEN
          WRITE(18,2300)  CURIYR,IR,CO2_QALLGEN(IR,CURIYR),CO2_QALLLD(IR,CURIYR),CO2_QALLAUC(IR,CURIYR),ALLOC_GEN,ALLOC_LD,ALLOC_AUC,CSTALL_GEN,CSTALL_LD,CSTALL_AUC
2300     FORMAT(1X,'CPP ALLOC',2I4,3F6.2,6F15.3)
        ENDIF
      ENDDO

      RETURN
      END

!
!     Subroutine to calculate net trades in CPP cases that allow interregional trading
!
      SUBROUTINE ELCPPERC
     
      IMPLICIT NONE
 
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'eusprc'
      include'edbdef'
      include'e111d'
      include'uefdout'

      INTEGER IMRG,EXRG
      REAL*8 ERCPPTR(MNUMNR,MNUMYR), ERCPPTRQ(MNUMNR,MNUMYR)
      COMMON /ERCPRC/ERCPPTR,ERCPPTRQ
 

      DO IMRG = 1, MNUMNR
        ERCPPTR(IMRG,CURIYR) = 0.0    ! initialize
        ERCPPTRQ(IMRG,CURIYR) = 0.0    ! initialize
      ENDDO

      DO IMRG = 1, UNRGNS
        IF (CO2_ERCNR(IMRG) .EQ. 2) THEN               !trading partners exist
          DO EXRG = 1, UNRGNS
            IF (IMRG .NE. EXRG .AND. CO2_STDGN(EXRG,IMRG) .EQ. 2) THEN         ! regions trade
              IF (CO2_STDTN(EXRG) .EQ. 1 .AND. CO2_STDTN(IMRG) .EQ. 1) THEN    ! check both regions rate-based
!              RATE BASE TARGET
                 ERCPPTR(IMRG,CURIYR) = ERCPPTR(IMRG,CURIYR) + EERCNRQR(EXRG,IMRG,CURIYR) * EERCNRPR(EXRG,CURIYR)
                 ERCPPTR(EXRG,CURIYR) = ERCPPTR(EXRG,CURIYR) - EERCNRQR(EXRG,IMRG,CURIYR) * EERCNRPR(EXRG,CURIYR)

                 ERCPPTRQ(IMRG,CURIYR) = ERCPPTRQ(IMRG,CURIYR) + EERCNRQR(EXRG,IMRG,CURIYR) 
                 ERCPPTRQ(EXRG,CURIYR) = ERCPPTRQ(EXRG,CURIYR) - EERCNRQR(EXRG,IMRG,CURIYR) 

              ELSEIF (CO2_STDTN(EXRG) .EQ. 2 .AND. CO2_STDTN(EXRG) .EQ. 2) THEN  !both regions mass-based
!              MASS BASE TARGET
                 ERCPPTR(IMRG,CURIYR) = ERCPPTR(IMRG,CURIYR) + EERCNRQM(EXRG,IMRG,CURIYR) * EERCNRPM(EXRG,CURIYR)
                 ERCPPTR(EXRG,CURIYR) = ERCPPTR(EXRG,CURIYR) - EERCNRQM(EXRG,IMRG,CURIYR) * EERCNRPM(EXRG,CURIYR)

                 ERCPPTRQ(IMRG,CURIYR) = ERCPPTRQ(IMRG,CURIYR) + EERCNRQM(EXRG,IMRG,CURIYR) 
                 ERCPPTRQ(EXRG,CURIYR) = ERCPPTRQ(EXRG,CURIYR) - EERCNRQM(EXRG,IMRG,CURIYR) 

              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO

      IF (FCRL .EQ. 1) THEN
      DO IMRG = 1, UNRGNS
        WRITE(22,2305) CURIYR,IMRG,ERCPPTR(IMRG,CURIYR),ERCPPTRQ(IMRG,CURIYR)
      ENDDO
2305  FORMAT(1X,'CPPTR ',2(':',I4),2(':',F15.3)) 
      ENDIF

      RETURN
      END
!
!     This subroutine sets up and solves the dispatch LP for the year using LP solver C-whiz when OML is chosen
!
      SUBROUTINE EFD_LP
      use efd_row_col
      use ifport, only : sleepqq,timef
      use ifcore, only : commitqq
!
      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'dispout'
      include 'dispuse'
      include 'dispcrv'
      include 'dispett'
      include 'fuelin'
      include 'elout'
      include 'postpr'
      include 'ecpcntl'
      include 'udatout'
      include 'bildout'
      include 'uecpout'
      include 'eusprc'
      include 'efpint'
      include 'dsmdimen'
      include 'dsmtoefd'
      include 'xprsparm'
      include'ecp_nuc'
      include'cdsparms'
      include'emm_aimms'


      INTEGER*4  STATS(9),FULLYR,IRG,ISP,IVS,IR
      INTEGER*4  IRET,RTCOD,JRET,SV_RET
      INTEGER*4  CPU_TIME_BEGIN,CPU_TIME_END,I,MCLOCK,t,t0,WALL_TIME_BEGIN,WALL_TIME_END,w,w0

      CHARACTER*50 CMD
      CHARACTER*12 BASISIN,BASISOUT                                     !* JCS: null-terminate *!
      CHARACTER*8  TEMP_ACTFILE,TEMP_ACTPROB,ACTFILE,BASISYR,TEMP_MINMAX,TEMP_BOUND
      CHARACTER*16 TEMP_OBJ,TEMP_RHS
      CHARACTER*8  PCKNAM
      CHARACTER*4 YRCHR,ITCYC
      CHARACTER*6 YRCH_ITCYC
      CHARACTER*8 MPSFIL
      CHARACTER*2 CHCODZ,tmpitr
      real*4 timer,timer_AIMMS_LHS_import
      CHARACTER*255  filen/' '/
      LOGICAL file_exists/.false./
      logical(4) lResult
      INTEGER EFDMPS,AMS_KEEPOPEN

      INTEGER*4 SV_ULGRP(MAXEFDSS,MAXEFDS,MAXNRG)
      INTEGER*4 SV_ULSEG(MAXEFDSS,MAXEFDS,MAXNRG)
      COMMON /SAVE_GRP_SEG/ SV_ULGRP,SV_ULSEG

      character*29 c_message         ! for LFOMLPRTCN
      character*216 line

      efdsub='EFD_LP'

      AMS_KEEPOPEN = RTOVALUE('KEEPOPEN',0)

! read CODEUSAGE from aimefd.xlsx to decide whether to disable individiual FORTRAN POST SOLVE ADJUSTMENTS 
        if (CODEUSAGE_AIMEFD_read .eq. 0) then
          
          CALL READAIMEFDOPTIONS         
          CALL SetToUseAIMSEFDPostADJST
        endif

        AIMEFDBG=RTOVALUE('AIMEFDBG',1)  ! if 1, more debug info included in efdcoeff*txt files.
        AIMMKEFD=RTOVALUE('AIMMKEFD',0)  ! if 1, set make_efd_aimms to .true. and set AIMEFDBG to 1 for more output
        !if(AIMMKEFD.eq.1) then
        !  make_efd_aimms=.true.
        !  AIMEFDBG=1
        !ENDIF
        timer=timef()
        SKIP_EFDOML = .TRUE.   !added by AKN to test a new flag to disable EFDOML based LP generation
        call efd_aimms_init  ! initialize aimms-efd coefficient storage arrays and counters
        CALL CheckToDisableEFDOMLLP   !added by AKN to test a new flag to disable EFDOML based LP generation
        write(6,'(a,f9.2)') 'AIMMS Interface: seconds to initialize EFD dynamic storage',timef()-timer


1122  FORMAT(A,I5,A,I3,6(A1,A8))

     
    timer=timef()  
    ! call routines to calculate LP coefficients if using OML, or if debug is on, or if AIMMS input/output variables not done (otherwise, need mappings)
     IF (AIMEFDBG .EQ. 1 .OR. USE_AIMEFD_SLNADJ .EQ. .FALSE. .OR. SKIP_EFDOML .EQ. .FALSE.) THEN 
         CALL REVEFD
     ENDIF
    !
    write(6,'(a,f9.2)') 'Wall seconds for REVEFD:',timef()-timer      
      
        IF (AIMEFDBG .EQ. 1 .OR. USE_AIMEFD_SLNADJ .EQ. .FALSE. .OR. SKIP_EFDOML .EQ. .FALSE. ) THEN
         CALL efd_fill_aimms_coeff  ! fills in  LP coefficients for AIMMS EFD to read
         close(io) !efdcoeff_yyyy_ii.txt'
        ENDIF
         if(.not. make_efd_aimms) then
 !          write(6,*) 'AIMMS interface: opening filen_efdcoeff='//trim(filen_efdcoeff)
 !          open(io,file=filen_efdcoeff,status='old',access='append',BUFFERED='YES',BUFFERCOUNT=10) ! reopen efdcoeff_yyyy.txt so "allzero" error messages get included
           AIM_Phase=1 ! 1: if in LP set up phase, 2: if in LP solution retrieval phase. used because "Call getbout"  only applies in phase 2, AIMMS validation phase, after oml sol retrieval
           CALL AIMMS_EFD('MainExecution')      ! transfers LP coefficients and other info to AIMMS, invokes AIMMS, and transfers results back
 !          close(io)
             timer=timef()  
           IF (AMS_KEEPOPEN .EQ. 0) THEN ! close AIMMS each time
           CALL AIMMS_EFD('end')  
    write(6,'(a,f9.3)') 'Wall seconds for close AIMMS EFD:',timef()-timer      
		   ENDIF        !leave AIMMS open if KEEPOPEN=1 (NEMS_Monitor will loop until monitor.in.txt is updated for next solve
         endif
      
 2222 FORMAT(10X,A,' CPU TIME USED = ',F7.2, ' WALL TIME USED = ',F7.2, ' in ',I4,' iteration',I4)



!   call routines to retrieve solution

           
           timer=timef()
           if (.NOT. USE_AIMEFD_SLNADJ) then   ! read in AIMMS solution Out_to_NEMS and use Fortran output routines 
             call AIMMS_InTxt_efd
             write(6,'(a,i7)') 'AIMMS Interface: number of col sol entries: ', num_efd_col_sol
             write(6,'(a,i7)') 'AIMMS Interface: number of row sol entries: ', num_efd_row_sol
!           write(6,'(a,f9.2)') 'AIMMS Interface: seconds to read EFD solution file',timef()-timer
           else ! if (USE_AIMEFD_SLNADJ) then  
             call AIMMS_InTxtVar_efd
           endif
           write(6,'(a,f9.2)') 'AIMMS Interface: seconds to read EFD solution file',timef()-timer
         CALL MPTIM3(t0,w0)
         timer=timef()
         CALL RETEFD    ! read solution and make post solution adjustments
          write(6,'(a,f9.2)') 'Wall seconds for RETEFD:',timef()-timer 
              ! for AIMMS debugging, check for messages.log file that includes aimms status messages. if found, copy to new file named with model year and iteration.
                write(line,'(a,i4,a,i2.2,a)') 'if exist .\efd\log\messages.log copy /Y .\efd\log\messages.log .\efd\log\messages_',curcalyr,'_',curitr,'.log'
                call callsys(iret,line)  ! calls a subroutine in main.f to send the command to the system  like a console command.
                line=' '   

              ! for AIMMS debugging, check for aimms.err file that includes aimms error messages. if found, copy to new file named with model year and iteration.
                write(line,'(a,i4,a,i2.2,a)') 'if exist .\efd\log\aimms.err copy /Y .\efd\log\aimms.err .\efd\log\aimms_',curcalyr,'_',curitr,'.err'
                call callsys(iret,line)  ! calls a subroutine in main.f to send the command to the system  like a console command.
                line=' '
                
             IF (AIMEFDBG.eq.1) THEN
                  ! for AIMMS validation, check for efd.lis file that includes aimms-to-cplex crosswalk. if found, copy to new file named with model year and iteration.
                    write(line,'(a,i4,a,i2.2,a)') 'if exist .\efd\log\efd.lis copy /Y .\efd\log\efd.lis .\efd\log\efd_',curcalyr,'_',curitr,'.lis'
                    call callsys(iret,line)  ! calls a subroutine in main.f to send the command to the system  like a console command.
                    line=' '  
                  !   call routines to retrieve solution
                  ! for AIMMS validation, check for cplex mps file. if found, copy to new file named with model year and iteration.
                    write(line,'(a,i4,a,i2.2,a)') 'if exist .\efd\cpx00000.mps copy /Y .\efd\cpx00000.mps .\efd\cpx_',curcalyr,'_',curitr,'.mps'
                    call callsys(iret,line)  ! calls a subroutine in main.f to send the command to the system  like a console command.
                    line=' '   

        ! validation/transition process and for comparison to the corresponding aimms-derived version of the variables.
                   AIM_Phase=2 ! 1: if in LP set up phase, 2: if in LP solution retrieval phase. used because "Call getbout"  only applies in phase 2, AIMMS validation phase, after oml sol retrieval
                   write(filen,'(a,i4,a,i2.2,a)')  './efd/composite_',curcalyr,'_',curitr,'.txt'
                   INQUIRE(file=filen, EXIST=file_exists)
                   if (file_exists.eq..true.) then
                        open(iOutTxt,file=filen,status='old',access='append',BUFFERED='YES',BUFFERCOUNT=10)
                        call AIMMS_Transfer_Out_efd
                        lresult=commitqq(iOutTxt)  ! use ifcore: force data to be written to file immediately
                        close(iOutTxt)
                   else
                        write(6,'(a)') 'Composit file '//filen//' is missing. Fortran EMM arrays adjusted after LP solved cannot be passed to AIMMS.'
                   endif
     
        ! print list of rows and columns with flag "needsol" to indicate which columns and rows were named in solution retrieval routines           
                   call efd_list_aimms_rowcols(1) 
             ENDIF   !IF (AIMEFDBG.eq.1) THEN
         CALL MPTIM3(t,w)
         write(6,2222) '** EFD RETRIEVE **',FLOAT(t)/100.-FLOAT(t0)/100.,FLOAT(w)/100.-FLOAT(w0)/100.0,CURCALYR,CURITR
      RETURN
      END        
! 
!     DVAL REVISES COLUMN/ROW INTERSECTIONS
!
      SUBROUTINE DVAL(COL,RW,VAL,colmask,rowmask,called_from)
      use efd_row_col  ! declarations/storage for AIMMS interface LP coefficients
      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
!
      REAL*8 VAL
      CHARACTER*16 COL,RW
      character(len=*),optional :: colmask,rowmask,called_from
      REAL*8 LOCAL_VAL,DIGITS2
      INTEGER IRET,I, ifound, ir
      DATA I/0/

!
!     IF FIRST ITERATION CREATE NEW MATRIX OTHERWISE CALL DATABASE REVISE ROUTINE.
!
      DIGITS_PARM = 6
      LOCAL_VAL = VAL
      AIMEFDBG=1  ! just for RM_EMMOML debug
      if(USW_DIGIT.gt.0.and.LOCAL_val.ne.-1.0.and.LOCAL_val.ne.0.0.and. LOCAL_val.ne.1.0) LOCAL_VAL = DIGITS2(LOCAL_VAL,DIGITS_PARM)
!
      IF (CURITR .EQ. 1 .AND. RW .NE. "EFDCOSTS" .AND. LOCAL_VAL .NE. 0.0 .AND. (ABS(LOCAL_VAL) .GE. 1000.0 .OR. ABS(LOCAL_VAL) .LT. 0.0001)) THEN
         write(UF_DBG,326) CURIYR,CURITR,COL,RW,LOCAL_VAL
  326    format(1x,'DVAL_BIG :',2(I4,':'),2(A8,':'),E20.6)
      ENDIF

      
! store LP coefficients for output to AIMMS      
        efdrownam=rw(1:8)
        efdcolnam=col(1:8)
        efdcoeff=local_val
        efdcoeff8=local_val
        if(present(colmask)) then
! look up the mask in the list and get an ID number, or add it to the list.  
          call efd_set_AIMMS_col_ID(colmask)
        endif
        if(present(rowmask)) then
! look up the mask in the list and get an ID number, or add it to the list.
          call efd_set_AIMMS_row_ID(rowmask)
        else
           write(6,*) 'AIMMS Interface error in DVAL rowmask not present for COL, RW:',col,rw
        endif
        if(present(called_from)) then
          efdsub=called_from
        endif
! store the coefficient, along with its row and col names, and row and col mask ID numbers
        call efd_assign_coeff

! store list of column names and their types to a spot in a hash table for fast lookup.
! based on the col name.
        call usehash(efdcolnam, efd_col_name, max_efd_col_hash, spot, initial_store)
        if(initial_store) then
          cnt_efd_col = cnt_efd_col + 1
        endif
! store list of row names and their types to a spot in a hash table for fast lookup.
! based on the row name.
        call usehash(efdrownam, efd_row_name, max_efd_row_hash, spot, initial_store)

        if(initial_store) then
          cnt_efd_row = cnt_efd_row + 1
          efd_row_sol(spot).row_type=row_aimms(aimms_row_id_num).row_type
        endif

        row_type=' '
        aimms_row_ID_num=-1
        aimms_col_ID_num=-1
        rownam_aimms=' '

!
!
      RETURN
      END
!                                                                              i
!
!     DRHS REVISES ROW RIGHT-HAND SIDE VALUES
!
      SUBROUTINE DRHS(RHS,RW,VAL,rowmask,called_from)
      use efd_row_col  ! declarations/storage for AIMMS interface LP coefficients
!
      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
!
      REAL*8 VAL,DIGITS2
      CHARACTER*16 RW,RHS
      character(len=*),optional :: rowmask,called_from   
      INTEGER IRET,I,ifound,ir
      DATA I/0/
!
!     IF FIRST ITERATION AND CREATING NEW MATRIX THEN CALL DATABASE REVISE ROUTINE.
!
      DIGITS_PARM = 6
      iret=0
      if(USW_DIGIT.gt.0.and.val.ne.-1.0.and.val.ne.0.0.and. val.ne.1.0) VAL = DIGITS2(VAL,DIGITS_PARM) ! round coefficient

      !if(AIMMSEFD.eq.0 .or. AIMEFDBG.eq.1) then
      !  !IF (VAL .NE. 0.0) IRET = 1                    !RM_EMMOML  
      !  IF (VAL .NE. 0.0) IRET = DFMCRHS(RHS,RW,VAL)   !RM_EMMOML
      !ENDIF
       IF (VAL .NE. 0.0) THEN
 ! store LP RHS values for output to AIMMS      
        efdrownam=rw(1:8)                    ! for AIMMS
        efdcolnam=rhs(1:8)                   ! for AIMMS
        efdcoeff=val                         ! for AIMMS
        efdcoeff8=val                        ! for AIMMS
! look up the RHS mask in the list and get an ID number, or add it to the list.  
        call efd_set_AIMMS_col_ID(RHS)

        if(present(rowmask)) then
! look up the mask in the list and get an ID number, or add it to the list.
          call efd_set_AIMMS_row_ID(rowmask)
        else
           write(6,*) 'AIMMS Interface error in DRHS rowmask not present for RW:',rw
        endif
        if(present(called_from)) then
          efdsub=called_from
        endif
        call efd_assign_coeff                ! for AIMMS

! store list of row names and their types to a spot in a hash table for fast lookup.
! based on the row name.
        call usehash(efdrownam, efd_row_name, max_efd_row_hash, spot, initial_store)

        if(initial_store) then
          cnt_efd_row = cnt_efd_row + 1
          efd_row_sol(spot).row_type=row_aimms(aimms_row_id_num).row_type
        endif

        row_type=' '
        aimms_row_ID_num=-1
        aimms_col_ID_num=-1
        rownam_aimms=' '

      ENDIF       
      IF (IRET .NE. 0) THEN
         WRITE(18,100) CURIYR+UHBSYR,IRET,RW,VAL
      ENDIF
!
!     IF (EFDMAT .EQ. 0) THEN
!
!        DUMP ALL COEFFICIENTS
!
!        IF (USYEAR(CURIYR) .EQ. UPSTYR) THEN
!           I = I + 1
!           WRITE(18,102) CURIYR+UHBSYR,I,IRET,RHS,RW,VAL
! 102       FORMAT(1X,"CRHS",":",I4,":",I12,":",I2,2(":",A),":",E20.6)
!        END IF
!
!     ENDIF
!
      IF (IRET .NE. 0 .OR. ISNAN(VAL).OR. ABS(VAL) .GT. HUGE(VAL)) THEN   ! check for NaNQ this way
         WRITE(6,24) IRET,CURIYR+1989,CURITR,RHS,RW,VAL
      ENDIF                                      !
  24  FORMAT(1X,"EFD_DRHS_ERROR_CODE",3(":",I4),2(":",A),":",E20.6)
!
100   FORMAT(1X,'REVISE ERROR DRHS ',I4,I4,1X,A,1X,F12.5)
!
      RETURN
      END
!
!
!     DBND REVISES COLUMN UPPER AND LOWER BOUNDS
!
      SUBROUTINE DBND(BND,COL,LVAL,UVAL,colmask,called_from)
      use efd_row_col  ! declarations/storage for AIMMS interface LP coefficients
!
      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
!
      REAL*8 LOCAL_L,LOCAL_U
      CHARACTER*8  BND
      CHARACTER*16 COL
      character(len=*),optional :: colmask,called_from

      REAL*8       LVAL,UVAL,DIGITS2
      INTEGER      IRET

!     IF FIRST ITERATION AND CREATING NEW MATRIX THEN CALL DATABASE REVISE ROUTINE.

      DIGITS_PARM = 6
      IF (USW_DIGIT .GT. 0)THEN
      LOCAL_L = DIGITS2(LVAL,DIGITS_PARM) ! round lower bound
      LOCAL_U = DIGITS2(UVAL,DIGITS_PARM) ! round upper bound
      ELSE
         LOCAL_L = LVAL                      ! round lower bound
         LOCAL_U = UVAL                      ! round upper bound
      END IF

      iret=0
!      if(AIMMSEFD.eq.0 .or. AIMEFDBG.eq.1) then
!        !IRET = 1                                      !RM_EMMOML
!        IRET = DFMCBND(BND,COL,LOCAL_L,LOCAL_U)      !RM_EMMOML
!        IF (IRET .NE. 0) THEN
!           WRITE(18,100) CURIYR+UHBSYR,IRET,COL,LOCAL_L,LOCAL_U
!        ENDIF
!       
!        IF (IRET .NE. 0 .OR. ISNAN(LOCAL_L).OR. ABS(LOCAL_L) .GT. HUGE(LOCAL_L) .OR. ISNAN(LOCAL_U) .OR. ABS(LOCAL_U) .GT. HUGE(LOCAL_U)) THEN ! check for NaNQ this way
!           WRITE(6,24) IRET,CURIYR+1989,CURITR,COL,BND,LOCAL_L,LOCAL_U
!        ENDIF
24    FORMAT(1X,"EFD_DBND_ERROR_CODE",3(":",I4),2(":",A),2(":",E20.6))
!
100     FORMAT(1X,'REVISE ERROR DBND ',I4,I4,1X,A,1X,F20.12,1X,F20.12)
!      endif
      

        if(present(colmask)) then
! look up the column name mask in the list and get an ID number, or add it to the list.  
          call efd_set_AIMMS_col_ID(colmask)
        endif
        if(present(called_from)) then
          efdsub=called_from
        endif

        if(LOCAL_L .gt. 0.) then
 ! store LP bound values for output to AIMMS      
          efdrownam='BOUNDLO'                  
          efdcolnam=col                        
                
          efdcoeff8=LOCAL_L                    

          rownam_AIMMS='LBOUND'  
          rsetnam(1:max_set)  =  ' '    
          call efd_set_AIMMS_row_ID('LBOUND' )
          call efd_assign_coeff                
        endif

        efdrownam='BOUNDUP'                  
        efdcolnam=col                        
! need to pass upper bound zeroes to AIMMS because these are used to remove columns from solution. Since
! coefficients are only transferred if non zero, temporarily set to -1 and switch to zero when
! writing output in outtxt.
        if(local_u.eq.0.) then
          local_u=-1.0 
        endif
                
        efdcoeff8=LOCAL_U                    
 
        rownam_AIMMS='UBOUND'  
        rsetnam(1:max_set)  =  ' '    
        call efd_set_AIMMS_row_ID('UBOUND' )
        call efd_assign_coeff 
           
        rownam_AIMMS=' '
        aimms_row_ID_num=-1
        aimms_col_ID_num=-1
      
      RETURN
      END
!
!     DROWTYPE invokes OML DFMCRTP to define a row as Nonconstraining/free, Less than or equal, Greater than or equal, or Equality.
!     The argumenet 
!     Equality is the default, so this routine is typically only called to for rows that are not Equalities.
!
      SUBROUTINE DROWTYPE(RW,RTYPE,rowmask)
      use efd_row_col  ! declarations/storage for AIMMS interface LP coefficients
!
      IMPLICIT NONE
!
!
      CHARACTER*16 RW
      character(len=*) :: RTYPE
      character(len=*),optional :: rowmask   
      integer iret,ir,ifound

      !if(AIMMSEFD.eq.0 .or. AIMEFDBG.eq.1) then
      !  !IRET=1                                 !RM_EMMOML
      !  IRET=DFMCRTP(RW,RTYPE)                 !RM_EMMOML
      !endif
      

        efdrownam=rw(1:8)                    ! for AIMMS
        row_type=RTYPE(1:1)
        if(present(rowmask)) then
! look up the mask in the list and get an ID number, or add it to the list.
          call efd_set_AIMMS_row_ID(rowmask)
          row_type=RTYPE(1:1)
        else
          write(6,*) 'AIMMS Interface error in DROWTYPE rowmask not present for RW, rtype=:',rw,rtype
        endif
          
! store list of row names and their types to a spot in a hash table for fast lookup.
! based on the row name.
        call usehash(efdrownam, efd_row_name, max_efd_row_hash, spot, initial_store)
        efd_row_sol(spot).row_type=row_type

        if(initial_store) then
          cnt_efd_row = cnt_efd_row + 1
        endif

        aimms_row_ID_num=-1
        aimms_col_ID_num=-1
        rownam_aimms=' '
        row_type=' '
   
      RETURN
      END SUBROUTINE DROWTYPE

!
!    This subroutine sets up the LP
!

      SUBROUTINE REVEFD
      USE EPHRTS_SWTICHES
      use efd_row_col
      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispout'
      include'dispuse'
      include'dispcrv'
      include'dispett'
      include'fuelin'
      include'elout'
      include'postpr'
      include'ecpcntl'
      include'udatout'
      include'bildout'
      include'uecpout'
      include'cdsparms'
      include'emission'
      include'uefdout'
      include'csapr'
      include'emmemis'

      INTEGER*4  IRET,IRG,IECP,INOX,IHG,ISO2
      CHARACTER*16 ROW_NOX,ROW_HG,ROW_SO2,ROW
      CHARACTER*1 NOXCODE,SO2CODE

      REAL*8 NEG1/-1.0D0/

      efdsub='REVEFD'

!
!      EFD_MIN = 0.001   ! moved out to some place outside REVEFD
      TST_NOX = 0
      TST_HG = 0
      TST_SO2 = 0

!   set OBJ function
      if (EFDMAT .EQ. 0) THEN
        CALL DROWTYPE(EFDOBJ,'N       ',EFDOBJ)
      endif

!     set up CO2 to EOR structure

      CALL ED$CCAP

!     set up CO2 Standards, if appropriate

      CALL ED$CPP
      CALL ED$CPP2

!     set up grid resilience constraints, if appropriate

      CALL ED$GRD

!     if (EFDMAT .EQ. 0) THEN
!     set up BTU balance
        CALL ED$BTU
!     endif

!     set up interregional trade
       if (EFDMAT .EQ. 0) THEN
         CALL ED$TRADE
       endif

!     group like units
!   we need to keep this active regardless of status of SKIP_EFDOML - move outside REVEFD
!       if (CURITR .EQ. 1) THEN
!         CALL ED$GRP
!       end if

! loop over region
      DO IRG = 1, UNRGNS
        CALL GETIN(1,IRG)

!       WRITE(6,3977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3977   FORMAT(1X,"UEFD_06397_EEITAJ_GET",4(":",I4),3(":",F12.3))

        CALL GETBLD(1,IRG)

! for ETIMPE, ETEXPE, get dispout
        CALL GETOUT(CURIYR,IRG)

!     set up load
        CALL ED$LOAD(IRG)

!     set up national rps, if specified
        IF (USW_EFDRPS .GT. 0 .AND. UPRNWCAS .GT. 0 .AND. UPRNWBND(CURIYR) .GT. 0.005)CALL ED$RPS(IRG)

      ENDDO

!     set up planned maintenance
       if (EFDMAT .EQ. 0) THEN
        CALL ED$PM
       endif

!     set up renewable balance (hydro)
        CALL ED$RNW
 
!     call subroutine to set up benchmarking for selected years
        IF (BMCLTOL .GT. 0.0 .OR. BMNGTOL .GT. 0.0 .OR. BMOLTOL .GT. 0.0)CALL ED$BENCH

!     set up operates
        CALL ED$OP

!     set up carbon constraint
        CALL ED$CAR

!     set up coal supply submatrix
        CALL ED$COL
!
!     add coal imports and stock changes into coal supply submatrix
!
        CALL ED$CL_IMPORTS

!     set up NG supply curves
        CALL ED$GAS

        IF (CURIYR+1989 .GE. UPSTYR) THEN 
!     SET UP HYDROGEN SUPPLY CURVES
            CALL ED$HYDROGEN
        END IF

!     set up OL supply curves
        CALL ED$OIL

!     set up WD supply curves
        CALL ED$BIO

!     set Capacity Specific Emission Rows

      DO IECP = 1 , ECP_D_CAP
         DO INOX = 1, NOX_GRP
            WRITE(NOXCODE,'(I1)') INOX
            ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE)
            IF (TST_NOX(IECP,INOX) .GT. 0 .AND. EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
               ROW = 'ELNOX' // NOXCODE // UPLNTCD(IECP); call makmsk(ROW_mask,':ELNOX:' , NOXCODE , UPLNTCD(IECP))
               CALL DROWTYPE(ROW,'L       ',ROW_mask)
               CALL DVAL(ROW,ROW,NEG1,ROW_mask,ROW_mask,'REVEFD,1')
               CALL DVAL(ROW,ROW_NOX,1.0D0,ROW_mask,ROW_NOX_mask,'REVEFD,2')
            END IF
         END DO
      END DO
      DO INOX = 1, NOX_GRP
         WRITE(NOXCODE,'(I1)') INOX
         ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE)
         IECP = ECP_D_CAP+1
         IF (TST_NOX(IECP,INOX) .GT. 0 .AND. EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
            ROW = 'ELNOX' // NOXCODE // 'OT'; call makmsk(ROW_mask,':ELNOX:' , NOXCODE , ':OT:')
            CALL DROWTYPE(ROW,'L       ',ROW_mask)
            CALL DVAL(ROW,ROW,NEG1,ROW_mask,ROW_mask,'REVEFD,3')
            CALL DVAL(ROW,ROW_NOX,1.0D0,ROW_mask,ROW_NOX_mask,'REVEFD,4')
         END IF
      END DO

      DO IECP = 1 , ECP_D_CAP
         DO ISO2 = 1, NUM_SO2_GRP
            WRITE(SO2CODE,'(I1)') ISO2
            ROW_SO2 = 'SULFUR' // SO2CODE; call makmsk(ROW_SO2_mask,':SULFUR:' , SO2CODE)
            IF (TST_SO2(IECP,ISO2) .GT. 0) THEN
               ROW = 'SULF' // UPLNTCD(IECP) // SO2CODE; call makmsk(ROW_mask,':SULF:' , UPLNTCD(IECP) , SO2CODE)
               CALL DROWTYPE(ROW,'L       ',ROW_mask)
               CALL DVAL(ROW,EFDOBJ,EFD_MIN,ROW_mask,EFDOBJ,'REVEFD,5')
               CALL DVAL(ROW,ROW,NEG1,ROW_mask,ROW_mask,'REVEFD,6')
               CALL DVAL(ROW,ROW_SO2,1.0D0,ROW_mask,ROW_SO2_mask,'REVEFD,7')
            END IF
         END DO
      END DO
      DO ISO2 = 1, NUM_SO2_GRP
         WRITE(SO2CODE,'(I1)') ISO2
         ROW_SO2 = 'SULFUR' // SO2CODE; call makmsk(ROW_SO2_mask,':SULFUR:' , SO2CODE)
         IECP = ECP_D_CAP+1
         IF (TST_SO2(IECP,ISO2) .GT. 0) THEN
            ROW = 'SULFOT' // SO2CODE; call makmsk(ROW_mask,':SULFOT:' , SO2CODE)
            CALL DROWTYPE(ROW,'L       ',ROW_mask)
            CALL DVAL(ROW,ROW,-1.0D0,ROW_mask,ROW_mask,'REVEFD,8')
            CALL DVAL(ROW,ROW_SO2,1.0D0,ROW_mask,ROW_SO2_mask,'REVEFD,9')
         END IF
      END DO

      DO IECP = 1 , NUTSEC
         DO IHG = 1 , NDREG
            ROW_HG = 'MERCURY' // UPRGCD(IHG); call makmsk(ROW_HG_mask,':MERCURY:' , UPRGCD(IHG))
            IF (TST_HG(IECP,IHG) .GT. 0) THEN
               ROW = 'MERC_' // UPLNTCD(IECP) // UPRGCD(IHG); call makmsk(ROW_mask,':MERC_:' , UPLNTCD(IECP) , UPRGCD(IHG))
               CALL DROWTYPE(ROW,'L       ',ROW_mask)
               CALL DVAL(ROW,ROW,NEG1,ROW_mask,ROW_mask,'REVEFD,10')
               CALL DVAL(ROW,ROW_HG,1.0D0,ROW_mask,ROW_HG_mask,'REVEFD,11')
            END IF
         END DO
      END DO

      DO IECP = NUTSEC + 1 , NUTSEC + 3
         DO IHG = 1 , NDREG
            ROW_HG = 'MERCURY' // UPRGCD(IHG); call makmsk(ROW_HG_mask,':MERCURY:' , UPRGCD(IHG))
            IF (TST_HG(IECP,IHG) .GT. 0) THEN
               IF (IECP .EQ. NUTSEC + 1) THEN
                  ROW = 'MERC_RS' // UPRGCD(IHG); call makmsk(ROW_mask,':MERC_RS:' , UPRGCD(IHG))
               ELSEIF (IECP .EQ. NUTSEC + 2) THEN
                  ROW = 'MERC_DS' // UPRGCD(IHG); call makmsk(ROW_mask,':MERC_DS:' , UPRGCD(IHG))
               ELSEIF (IECP .EQ. NUTSEC + 3) THEN
                  ROW = 'MERC_OT' // UPRGCD(IHG); call makmsk(ROW_mask,':MERC_OT:' , UPRGCD(IHG))
               END IF
               CALL DROWTYPE(ROW,'L       ',ROW_mask)
               CALL DVAL(ROW,ROW,NEG1,ROW_mask,ROW_mask,'REVEFD,12')
               CALL DVAL(ROW,ROW_HG,1.0D0,ROW_mask,ROW_HG_mask,'REVEFD,13')
            END IF
         END DO
      END DO

      RETURN
      END

!
!     This subroutine sets up the BTU balance row and fuel consumption columns
!     Also sets up the NOX constraint
!
      SUBROUTINE ED$BTU
      USE EPHRTS_SWTICHES
      use efd_row_col ! declarations, shared storage for LP coefficients for AIMMS 

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'cdsparms'
      include'control'
      include'fuelin'
      include'dispin'
      include'ecpcntl'
      include'bildin'
      include'plntctl'
      include'angtdm'
      include'uso2grp'
      include'emission'
      include'uecpout'
      include'coalemm'
      include'emeblk'
      include'emoblk'
      include'ecp_nuc'
      include'csapr'
      include'emmemis'
      include'eusprc'
      include'edbdef'

      INTEGER FRG, CRG, IPLT, IECP, IMD, ICOF,IRET,CFLV,IEFD,IGS,IRS,IDS,IOL,GRG,ORG,HRG,CAR
      INTEGER IFL,IWD,NGSN,FULLYR,RCF,INOX,CRV,IS,CTLPLT
      REAL GASSH,OLSH,OILSH,TOT,CLSH,FLCST,CFSH,VAL,CFCAP,ESCST,NOXP,DSADJ
      REAL*8 COEFF,COEFC,CTLSUM(NDREG),CTLNXRT,NOXEM,SEQRT
      CHARACTER*16 BTUROW,COL,CFROW,CLROW,NGROW,OLROW,WDROW,ROW_NOX,RPSROW,CFGEN,ROWCAR,ROWCARR,ROWCARC,ROWSEQ
      CHARACTER*1 NOXCODE
      CHARACTER*2 BR,CL_CD,GS_CD,OL_CD,CA_CD

      REAL*8 NEG1/-1.0D0/

      efdsub='ED$BTU'

      FULLYR = USYEAR(CURIYR)

!  set up NOX constraint

      
      DO INOX = 1, NOX_GRP
        WRITE(NOXCODE,'(I1)') INOX
        IF (EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
          ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE) ! defined as LE but RHS commented out?
          CALL DROWTYPE(ROW_NOX,'L       ',ROW_NOX_mask)
!         COEFF = DBLE(EMRFNA(INOX,CURIYR) )
!         CALL DRHS(EFDRHS,ROW_NOX,COEFF)
!   create escape vector with ECP price
          NOXP = EPNOXPR(INOX,CURIYR)
          COL = 'ESCNOX0'//NOXCODE; call makmsk(COL_mask,':ESCNOX0:',NOXCODE)
!         CALL DBND(EFDBND,COL,0.0D0,99.9D0)
          CALL DVAL(COL,ROW_NOX,NEG1,COL_mask,ROW_NOX_mask,'ED$BTU,1')
          escst = NOXP / 1000.0
          CALL DVAL(COL,EFDOBJ,DBLE(escst),COL_mask,efdobj,'ED$BTU,2')
        ELSE   ! set up free row
          ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE)
          CALL DROWTYPE(ROW_NOX,'N       ',ROW_NOX_mask)
        ENDIF
      END DO

!  account for NOX from CTL
!  aggregate BTUs by coal region
      CTLSUM = 0.0
      DO CRV = 1, MX_NCOALS
        DO CRG = 1, NDREG
          CTLSUM(CRG) = CTLSUM(CRG) + CTLBTU(CRV,CRG,CURIYR)
        ENDDO
      ENDDO
      DO CRG = 1, NDREG
         write(UF_DBG,326) CRG,CURIYR,CURITR,CTLSUM(CRG)
      ENDDO
326   FORMAT(1x,'CTL SUM',3I5,F15.4)


      DO CRG = 1, NDREG
        DO IS = 1, EENSP
          DO INOX = 1, NOX_GRP
           WRITE(NOXCODE,'(I1)') INOX
           IF (EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
              ROW_NOX = 'ELNOX' // NOXCODE // 'OT'; call makmsk(ROW_NOX_mask,':ELNOX:' , NOXCODE , ':OT:')
           ELSE
              ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE)
           END IF
           COL = 'B'//CHCOD(CRG)//'CTL'//CHCOD(IS)//'N'//NOXCODE; call makmsk(COL_mask,':B:',CHCOD(CRG),':CTL:',CHCOD(IS),':N:',NOXCODE)
           COEFF = CTLSUM(CRG) * EETIME(IS) / 8760.0
           CTLPLT = WIIG   ! IGCC characteristics
           CTLNXRT = NOX_NEW(1,CTLPLT) * (NOX_EFD(IS,INOX) / EETIME(IS)) * NOX_SHR_BY_CLRG(CRG,INOX)
           NOXEM =  CTLNXRT * 0.0005 * 1000.0
           COEFF = COEFF * NOXEM
           IF (COEFF .GT. 0.0) THEN
              IF (EMRFNA(INOX,CURIYR) .GT. 0.0) TST_NOX(ECP_D_CAP+1,INOX) = 1
              CALL DVAL(COL,ROW_NOX,1.0D0,COL_mask,ROW_NOX_mask,'ED$BTU,3')
              CALL DBND(EFDBND,COL,COEFF,COEFF,COL_mask,'ED$BTU,4')
           END IF
          ENDDO   !INOX
        ENDDO     ! IS
      ENDDO       ! CRG

      ROWCAR = 'CARBONXX';ROWCAR_MASK='CARBONXX'
      ROWSEQ = 'CARSEQXX';ROWSEQ_MASK='CARSEQXX'

      DO FRG = 1, UNFRGN

!   set up BTU columns by plant type and fuel options

         CRG = EPCLMP(FRG)
         WRITE(BR,'(I2.2)') CRG
         CL_CD = BR
         WDROW = 'S_WD'// BR; call makmsk(WDROW_mask,':S_WD:', BR)
!
         GRG = EPGSMP(FRG)
         WRITE(BR,'(I2.2)') GRG
         GS_CD = BR
         NGROW = 'NG' // BR; call makmsk(NGROW_mask,':NG:' , BR)
!
         ORG = EPCSMP(FRG)
         WRITE(BR,'(I2.2)') ORG
         OL_CD = BR
         OLROW = 'DS' // BR; call makmsk(OLROW_mask,':DS:' , BR)
!
         HRG = EPCAMP(FRG)
         WRITE(BR,'(I2.2)') HRG
         CA_CD = BR
         ROWCARR = 'CARBON' // CO2_RG(HRG); call makmsk(ROWCARR_mask,':CARBON:' , CO2_RG(HRG))

!   coal

       DO IECP = 1, UIIS
            SEQRT = UPPCEF(IECP)
            IF (CPFLECP(IECP,ORG,CRG,GRG) .GT. 0.0) THEN
               BTUROW = 'BTU'//EPFLCD(FRG)//UPLNTCD(IECP)//'XX'; call makmsk(BTUROW_mask,':BTU:',EPFLCD(FRG),UPLNTCD(IECP)//'X',':X:') ! use first X to make 3-character plant type.

               CLROW = 'CL'//CL_CD//UPLNTCD(IECP); call makmsk(CLROW_mask,':CL:',CL_CD,UPLNTCD(IECP))
               IF (EFDMAT .EQ. 0) THEN
                 CALL DROWTYPE(BTUROW,'L       ',BTUROW_mask)
                 CALL DRHS(EFDRHS,BTUROW,0.0D0,BTUROW_mask,'ED$BTU,5')
               ENDIF

!    loop over cofiring retrofit categories

       IF (FULLYR .GE. UPSTYR .AND. USW_ECPCF .NE. 1) THEN  !cofiring variables filled in after ECP runs
         RCF = ECP_D_RCF
       ELSE
         RCF = 1
       ENDIF

        DO ICOF = 1, RCF

!  create cofiring capacity balance row

        CFROW = 'CFCP'//CHCOD(EPCLMP(FRG))//'X'//CHCOD(ICOF); call makmsk(CFROW_mask,':CFCP:',CHCOD(EPCLMP(FRG)),':X:',CHCOD(ICOF))
        IF (EFDMAT .EQ. 0) THEN
          IF (FULLYR .GE. UPSTYR .AND. USW_ECPCF .NE. 1) THEN
            CALL DROWTYPE(CFROW,'L       ',CFROW_mask)
            CFCAP = UCF_RCAP(ICOF,CRG,CURIYR) * 0.001  !GW
            CALL DRHS(EFDRHS,CFROW,DBLE(CFCAP),CFROW_mask,'ED$BTU,6')
          ELSE
            CALL DROWTYPE(CFROW,'N       ',CFROW_mask)   ! free row, no cofiring data yet
          ENDIF
        ENDIF

!     create cofiring generation free row - by coal region and plant type
        CFGEN = 'CFG'//CHCOD(CRG)//UPLNTCD(IECP)//'XX'; call makmsk(CFGEN_mask,':CFG:',CHCOD(CRG),UPLNTCD(IECP),':XX:')
        IF (EFDMAT .EQ. 0) THEN
          CALL DROWTYPE(CFGEN,'N       ',CFGEN_mask)
        ENDIF

!     no cofiring option:

        IMD = 1
        COL = 'B'//EPFLCD(FRG)//UPLNTCD(IECP)//'XX'//CHCOD(ICOF)//CHCOD(IMD); COL_mask='B(*)(***)(*)(**)'
        GASSH = EDMXGS(1,IECP,FRG)
        OLSH = EDMXDS(1,IECP,FRG)
       IF (USW_ECPCF .EQ. 0) THEN   !no cofiring option
        IF ((GASSH + OLSH) .GT. 1.0) THEN
         write(UF_DBG,*) ' gas oil share > 1 in coal plant', FRG,IECP,GASSH,OLSH
          TOT = GASSH + OLSH
          GASSH = GASSH * 1.0 / TOT
          OLSH = OLSH * 1.0 / TOT
        ENDIF
        CLSH = 1.0 - (GASSH + OLSH)

!       FLCST = GASSH * UPFUEL(UIGF,EPGSMP(FRG)) + &
!             OILSH * UPFUEL(UIRL,EPCSMP(FRG)) + &
!             CLSH  * UPFUEL(IECP,EPCLMP(FRG))
!       FLCST = GASSH * UPFUEL(UIGF,EPGSMP(FRG)) + &
!             OILSH * UPFUEL(UIRL,EPCSMP(FRG))
!       FLCST = MAX(FLCST,0.001)

        FLCST = 0.001

!   put fuel cost in OBJ row
        CALL DVAL(COL,EFDOBJ,DBLE(FLCST),COL_mask,efdobj,'ED$BTU,7')

!   put in BTU balance row
        CALL DVAL(COL,BTUROW,NEG1,COL_mask,BTUROW_mask,'ED$BTU,8')

!   put in coal supply balance row
!       CLROW = 'CL'//CL_CD//UPLNTCD(IECP)
        CALL DVAL(COL,CLROW,DBLE(CLSH),COL_mask,CLROW_mask,'ED$BTU,9')

        write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, CLROW, CLSH
 3343   FORMAT(1X,"EFD_SHARES",3(":",I4),2(":",A16),":",F15.6)

!   account for sequestration - do all fuels combined!
        IF (SEQRT .GT. 0.0) THEN
          COEFF = SEQRT * UFRCAR(IECP,CRG) * (1.0 / 2204.0) * CLSH + &
                  EGFEL(CURIYR) * 0.001 * GASSH * SEQRT + &
                  EDSEL(CURIYR) * 0.001 * OLSH * SEQRT

          IF (COEFF .GT. EFD_MIN) Then
             CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,10')
             CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,11')
          endif
        ENDIF

!   put in NG supply balance row, if used

                 IF (GASSH .GT. 0.0) THEN
                    DO NGSN = 1, 3
                       NGROW = 'NG' // GS_CD // CHCOD(NGSN); call makmsk(NGROW_mask,':NG:' , GS_CD , CHCOD(NGSN))
                       CALL DVAL(COL,NGROW,DBLE(GASSH * 0.5),COL_mask,NGROW_mask,'ED$BTU,12')
                       write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, NGROW, GASSH * .5

                    ENDDO
!   account for sequestration - done above
                 !  IF (SEQRT .GT. 0.0) THEN
                 !     COEFF = EGFEL(CURIYR) * 0.001 * GASSH * SEQRT
                 !     IF (COEFF .GT. EFD_MIN) then
                 !       CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,13')
                 !       CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,14')
                 !     endif
                 !  ENDIF
                 END IF

!   put in OL supply balance row, if used

                  IF (OLSH .GT. 0.0) THEN
                      CALL DVAL(COL,OLROW,DBLE(OLSH),COL_mask,OLROW_mask,'ED$BTU,15')

                      write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, OLROW, OLSH

!   account for sequestration - done above
                  !   IF (SEQRT .GT. 0.0) THEN
                  !    COEFF = EDSEL(CURIYR) * 0.001 * OLSH * SEQRT
                  !    IF (COEFF .GT. EFD_MIN) then
                  !       CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,16')
                  !       CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,17')
                  !    endif
                  !   ENDIF
                  ENDIF

!   account for regional carbon limits, if any
             DO CAR = 1 , CO2_GRP
                COEFF = (1.0 - SEQRT) * (UFRCAR(IECP,CRG) * (1.0 / 2204.0) * CLSH * CO2_CL_BY_FL(FRG,CAR) +  &
                        (EGFEL(CURIYR) * 0.001 * GASSH +  &
                         EDSEL(CURIYR) * 0.001 * OLSH) * CO2_OG_BY_FL(FRG,CAR))
!   account for NJ leaving and reentering RGGI
                IF (FLRGCODE(FRG) .EQ. 'YJ')THEN
                   COEFF = COEFF * (1.0 - CO2NJEXC(CAR,CURIYR))
                ELSEIF (FLRGCODE(FRG) .EQ. 'VA') THEN
                   COEFF = COEFF * (1.0 - CO2VAEXC(CAR,CURIYR))
                END IF
                IF (COEFF .GT. EFD_MIN)THEN
                   ROWCARC = 'CARBON' // CO2_RG(CAR); call makmsk(ROWCARC_mask,':CARBON:' , CO2_RG(CAR))
                   CALL DVAL(COL,ROWCARC,COEFF,COL_mask,ROWCARC_mask,'ED$BTU,18')
                END IF
             END DO

       ELSE  ! use ECP cofiring share - only one option
            CFSH = UPWDCFR(IECP,EPCLMP(FRG))
            IF (GASSH + OLSH + CFSH .GT. 1.0) THEN
           write(UF_DBG,*) ' cofire gas oil share > 1 in coal plant',FRG,IECP,GASSH,OLSH,CFSH
             TOT = GASSH + OLSH + CFSH
             GASSH = GASSH * 1.0 / TOT
             OLSH = OLSH * 1.0 / TOT
             CFSH = CFSH * 1.0 / TOT
            ENDIF
            CLSH = 1.0 - (GASSH + OLSH + CFSH)

!  fuel cost = extra transportation cost for cofiring step
!           FLCST = CFSH * UPCFSTEP(ICOF,CFLV)
!           FLCST = MAX(FLCST,0.001)
            FLCST = 0.001
!   if RPS, subtract credit price from fuel cost in OBJ - if using ECP shares, do on the operates
!           IF (UPRNWREG .EQ. 1) THEN
!              FLCST = FLCST - CFSH / HRTCLNR(CRG,CURIYR,IECP) * 1000.0 * EPRPSPR(CURIYR)
!           ELSEIF (UPRNWREG .GE. 2) THEN
!              FLCST = FLCST - CFSH / HRTCLNR(CRG,CURIYR,IECP) * 1000.0 * EPRPSPRR(CRG,CURIYR)
!           ENDIF

!   put fuel cost in OBJ row
            CALL DVAL(COL,EFDOBJ,DBLE(FLCST),COL_mask,efdobj,'ED$BTU,19')

!   put in BTU balance row
            CALL DVAL(COL,BTUROW,NEG1,COL_mask,BTUROW_mask,'ED$BTU,20')

!   put in coal supply balance row
            CALL DVAL(COL,CLROW,DBLE(CLSH),COL_mask,CLROW_mask,'ED$BTU,21')

            write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, CLROW, CLSH

!   account for sequestration
            IF (SEQRT .GT. 0.0) THEN
              COEFF = SEQRT * UFRCAR(IECP,CRG) * (1.0 / 2204.0) * CLSH + &
                      EGFEL(CURIYR) * 0.001 * GASSH * SEQRT + &
                      EDSEL(CURIYR) * 0.001 * OLSH * SEQRT
              IF (COEFF .GT. EFD_MIN) then
                 CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,22')
                 CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,23')
              endif
            ENDIF

!   put in NG supply balance row, if any

                           IF (GASSH .GT. 0.0) THEN
                              DO NGSN = 1, 3
                                 NGROW = 'NG' // GS_CD // CHCOD(NGSN); call makmsk(NGROW_mask,':NG:' , GS_CD , CHCOD(NGSN))
                                 CALL DVAL(COL,NGROW,DBLE(GASSH * 0.5),COL_mask,NGROW_mask,'ED$BTU,24')
                                 write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, NGROW, GASSH * .5

                              ENDDO
!   account for sequestration
                              !IF (SEQRT .GT. 0.0) THEN
                              ! COEFF = EGFEL(CURIYR) * 0.001 * GASSH * SEQRT
                              ! IF (COEFF .GT. EFD_MIN) then
                              !    CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,25')
                              !    CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,26')
                              ! endif
                              !ENDIF
                           END IF

!   put in OL supply balance row, if any

                  IF (OLSH .GT. 0.0) THEN
                      CALL DVAL(COL,OLROW,DBLE(OLSH),COL_mask,OLROW_mask,'ED$BTU,27')

                      write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, OLROW, OLSH

!   account for sequestration
                      !IF (SEQRT .GT. 0.0) THEN
                      !  COEFF = EDSEL(CURIYR) * 0.001 * OLSH * SEQRT
                      !  IF (COEFF .GT. EFD_MIN) then
                      !    CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,28')
                      !    CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,29')
                      !   endif
                      ! ENDIF
                     ENDIF

!   account for regional carbon limits, if any
             DO CAR = 1 , CO2_GRP
                COEFF = (1.0 - SEQRT) * (UFRCAR(IECP,CRG) * (1.0 / 2204.0) * CLSH * CO2_CL_BY_FL(FRG,CAR) +  &
                        (EGFEL(CURIYR) * 0.001 * GASSH +  &
                         EDSEL(CURIYR) * 0.001 * OLSH) * CO2_OG_BY_FL(FRG,CAR))
!   account for NJ leaving and reentering RGGI
                IF (FLRGCODE(FRG) .EQ. 'YJ')THEN
                   COEFF = COEFF * (1.0 - CO2NJEXC(CAR,CURIYR))
                ELSEIF (FLRGCODE(FRG) .EQ. 'VA') THEN
                   COEFF = COEFF * (1.0 - CO2VAEXC(CAR,CURIYR))
                END IF
                IF (COEFF .GT. EFD_MIN)THEN
                   ROWCARC = 'CARBON' // CO2_RG(CAR); call makmsk(ROWCARC_mask,':CARBON:' , CO2_RG(CAR))
                   CALL DVAL(COL,ROWCARC,COEFF,COL_mask,ROWCARC_mask,'ED$BTU,30')
                END IF
             END DO

!   put in WD supply balance row, if any

            IF (CFSH .GT. 0.0) THEN
               CALL DVAL(COL,WDROW,DBLE(CFSH),COL_mask,WDROW_mask,'ED$BTU,31')

               write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, WDROW, CFSH

            END IF

!   put in cofiring gen free row
            VAL = CFSH / HRTCLNR(CRG,CURIYR,IECP) * 1000000.0
            CALL DVAL(COL,CFGEN,DBLE(VAL),COL_mask,CFGEN_mask,'ED$BTU,32')
!           if(cfsh .gt. 0.0 .and. curitr .gt. maxitr)write(6,3355) curiyr+1989,col,cfgen,uplntcd(iecp),crg,icof, &
!           CFSH , HRTCLNR(CRG,CURIYR,IECP) , VAL
!3355 format(1h ,'!cof',i4,1x,a10,1x,a10,1x,a3,i3,i4,3f10.3)
!   put in RPS row if applicable
!           IF (UPRNWBND(CURIYR) .GT. 0.005 .AND. UPRNWSHR(WIWD) .GT. 0.0) THEN
!              RPSROW = 'RPSREQXX'
!              VAL = CFSH / HRTCLNR(CRG,CURIYR,IECP) * 1000000.0
!              VAL = VAL * 0.001
!              CALL DVAL(COL,RPSROW,DBLE(VAL))
!           ENDIF
       ENDIF   !USW_ECPCF

! cofiring options if after first ECP year

        IF (FULLYR .GE. UPSTYR .AND. USW_ECPCF .NE. 1) THEN
        DO CFLV = 1, UPCFNSTP(ICOF)
         IMD = CFLV + 1
         COL = 'B'//EPFLCD(FRG)//UPLNTCD(IECP)//'XX'//CHCOD(ICOF)//CHCOD(IMD); COL_mask='B(*)(***)(*)(**)'
          IF (UPCFLEV(ICOF,CFLV) .GT. 0.0 .AND. UPCFBTU(ICOF,EPCLMP(FRG)) .GT. 0.0) THEN
            CFSH = UPCFLEV(ICOF,CFLV)
            IF (GASSH + OLSH + CFSH .GT. 1.0) THEN
           write(UF_DBG,*) ' cofire gas oil share > 1 in coal plant',FRG,IECP,GASSH,OLSH,CFSH
             TOT = GASSH + OLSH + CFSH
             GASSH = GASSH * 1.0 / TOT
             OLSH = OLSH * 1.0 / TOT
             CFSH = CFSH * 1.0 / TOT
            ENDIF
            CLSH = 1.0 - (GASSH + OLSH + CFSH)

!           FLCST = GASSH * UPFUEL(UIGF,EPGSMP(FRG)) + &
!             OILSH * UPFUEL(UIRL,EPCSMP(FRG)) + &
!             CLSH  * UPFUEL(IECP,EPCLMP(FRG)) + &
!             CFSH * UPFUEL(UIWD,EPCLMP(FRG))
!           FLCST = GASSH * UPFUEL(UIGF,EPGSMP(FRG)) + &
!             OILSH * UPFUEL(UIRL,EPCSMP(FRG)) + &
!             CFSH * UPFUEL(UIWD,EPCLMP(FRG))
!           FLCST = CFSH * UPFUEL(UIWD,EPCLMP(FRG))
!           FLCST = MAX(FLCST,0.001)

!  fuel cost = extra transportation cost for cofiring step
            FLCST = CFSH * UPCFSTEP(ICOF,CFLV)
            FLCST = MAX(FLCST,0.001)

!   if RPS, subtract credit price from fuel cost in OBJ
            IF (UPRNWREG .EQ. 1) THEN
               FLCST = FLCST - CFSH / HRTCLNR(CRG,CURIYR,IECP) * 1000.0 * EPRPSPR(CURIYR)
!           ELSEIF (UPRNWREG .GE. 2) THEN
!              FLCST = FLCST - CFSH / HRTCLNR(CRG,CURIYR,IECP) * 1000.0 * EPRPSPR(CRG,CURIYR)
            ENDIF

!   put fuel cost in OBJ row
            CALL DVAL(COL,EFDOBJ,DBLE(FLCST),COL_mask,efdobj,'ED$BTU,33')

!   put in BTU balance row
            CALL DVAL(COL,BTUROW,NEG1,COL_mask,BTUROW_mask,'ED$BTU,34')

!   put in coal supply balance row
            CALL DVAL(COL,CLROW,DBLE(CLSH),COL_mask,CLROW_mask,'ED$BTU,35')

            write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, CLROW, CLSH


!   account for sequestration
            IF (SEQRT .GT. 0.0) THEN
              COEFF = SEQRT * UFRCAR(IECP,CRG) * (1.0 / 2204.0) * CLSH + &
                      EGFEL(CURIYR) * 0.001 * GASSH * SEQRT + &
                      EDSEL(CURIYR) * 0.001 * OLSH * SEQRT
              IF (COEFF .GT. EFD_MIN) then
                CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,36')
                CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,37')
              endif
            ENDIF

!   put in NG supply balance row, if any

                          IF (GASSH .GT. 0.0) THEN
                             DO NGSN = 1, 3
                                NGROW = 'NG' // GS_CD // CHCOD(NGSN); call makmsk(NGROW_mask,':NG:' , GS_CD , CHCOD(NGSN))
                                CALL DVAL(COL,NGROW,DBLE(GASSH * 0.5),COL_mask,NGROW_mask,'ED$BTU,38')

                                write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, NGROW, GASSH * .5

!   account for sequestration
                                !IF (SEQRT .GT. 0.0) THEN
                                ! COEFF = EGFEL(CURIYR) * 0.001 * GASSH * SEQRT
                                ! IF (COEFF .GT. EFD_MIN) then
                                !   CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,39')
                                !   CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,40')
                                ! endif
                                !ENDIF
                             ENDDO
                          END IF

!   put in OL supply balance row, if any

                  IF (OLSH .GT. 0.0) THEN
                     CALL DVAL(COL,OLROW,DBLE(OLSH),COL_mask,OLROW_mask,'ED$BTU,41')

                     write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, OLROW, OLSH

!   account for sequestration
                     !IF (SEQRT .GT. 0.0) THEN
                     ! COEFF = EDSEL(CURIYR) * 0.001 * OLSH * SEQRT
                     ! IF (COEFF .GT. EFD_MIN) then
                     !   CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,42')
                     ! endif
                     !ENDIF
                  ENDIF

!   account for regional carbon limits, if any
             DO CAR = 1 , CO2_GRP
                COEFF = (1.0 - SEQRT) * (UFRCAR(IECP,CRG) * (1.0 / 2204.0) * CLSH * CO2_CL_BY_FL(FRG,CAR) +  &
                        (EGFEL(CURIYR) * 0.001 * GASSH +  &
                         EDSEL(CURIYR) * 0.001 * OLSH) * CO2_OG_BY_FL(FRG,CAR))
                IF (COEFF .GT. EFD_MIN)THEN
                   ROWCARC = 'CARBON' // CO2_RG(CAR); call makmsk(ROWCARC_mask,':CARBON:' , CO2_RG(CAR))
                   CALL DVAL(COL,ROWCARC,COEFF,COL_mask,ROWCARC_mask,'ED$BTU,43')
                END IF
             END DO

!   put in WD supply balance row, if any

            IF (CFSH .GT. 0.0) THEN
               CALL DVAL(COL,WDROW,DBLE(CFSH),COL_mask,WDROW_mask,'ED$BTU,44')

               write(UF_DBG,3343) CURIRUN, CURIYR+1989, CURITR, COL, WDROW, CFSH

            END IF

!   put in cofiring capacity balance row

            VAL = 1.0 / UPCFBTU(ICOF,EPCLMP(FRG))
            CALL DVAL(COL,CFROW,DBLE(VAL),COL_mask,CFROW_mask,'ED$BTU,45')

!   put in cofiring gen free row
            VAL = CFSH / HRTCLNR(CRG,CURIYR,IECP) * 1000000.0
            CALL DVAL(COL,CFGEN,DBLE(VAL),COL_mask,CFGEN_mask,'ED$BTU,46')
!   put in RPS row if applicable
!           IF (UPRNWBND(CURIYR) .GT. 0.005 .AND. UPRNWSHR(WIWD) .GT. 0.0) THEN
!              RPSROW = 'RPSREQXX'
!              VAL = CFSH / HRTCLNR(CRG,CURIYR,IECP) * 1000000.0
!              VAL = VAL * 0.001
!              CALL DVAL(COL,RPSROW,DBLE(VAL))
!           ENDIF
          ENDIF
        ENDDO    !cflv
        ENDIF   ! upstyr
       ENDDO     !icof
      ENDIF
      ENDDO    ! iecp

!  loop over other gas/oil types - by efd type 
      
! EDT: LOOPING OVER OTHER TYPES AFTER TYPES. THE NEW TYPE WOULD BE IN THIS CATEGORY
! TODO: TO MAKE IT SIMPLE, PICK A PLANT TYPE AND ASSIGN IT ONLY TO HDYROGEN, IN EMMCTRL CHANGE MAPPING TO THE ONE WE'RE USING NOW UIGC?

      DO IEFD = UICAS + 1, EFD_D_CAP
      
      IF (EPPLCD(IEFD) .NE. 'ICE') THEN 
        DSADJ = 1.0
        IGS = 0
        IRS = 0
        IDS = 0
        IWD = 0
        IF (IEFD .EQ. UICTX .OR. IEFD .EQ. UIACT .OR. IEFD .EQ. UICCX .OR. IEFD .EQ. UIACC .OR. IEFD .EQ. UIACS .OR. IEFD .EQ. UIAC2) THEN
           DSADJ = 1.10
        ENDIF
        IF (IEFD .EQ. UIACS) THEN
             SEQRT = UPPCEF(WICS)
        ELSEIF (IEFD .EQ. UIAC2) THEN
             SEQRT = UPPCEF(WIA2)
        ELSEIF (IEFD .EQ. UICOQ) THEN
             SEQRT = UPPCEF(WIPQ)
        ELSEIF (IEFD .EQ. UIBIG) THEN
             SEQRT = UPPCEF(WIBI)
        ELSE
             SEQRT = 0.0
        ENDIF
        IF (CPFLEFD(IEFD,ORG,CRG,GRG) .GT. 0.0) THEN

        DO IFL = 1, EFD_D_FPP
         if (WFLTP(IEFD,IFL) .NE. 0) THEN
          IF (UIGAS(WFLTP(IEFD,IFL)) .EQ. 1) IGS = 1
          IF (UIRES(WFLTP(IEFD,IFL)) .EQ. 1) IRS = 1
          IF (UIDIS(WFLTP(IEFD,IFL)) .EQ. 1) IDS = 1
          IF (WFLTP(IEFD,IFL) .EQ. UIWD) IWD = 1
         endif
        ENDDO

        IF (IGS .EQ. 1 .AND. (IRS .EQ. 0 .AND. IDS .EQ. 0)) THEN   ! gas only
          IMD = 1
          GASSH = 1.0
            DO NGSN = 1, 3
             BTUROW = 'BTU' // EPFLCD(FRG) // EPPLCD(IEFD) // CHCOD(NGSN); call makmsk(BTUROW_mask,':BTU:' , EPFLCD(FRG) , EPPLCD(IEFD) , CHCOD(NGSN))
             NGROW = 'NG' // GS_CD // CHCOD(NGSN); call makmsk(NGROW_mask,':NG:' , GS_CD , CHCOD(NGSN))
             CALL DROWTYPE(BTUROW,'L       ',BTUROW_mask)
             CALL DRHS(EFDRHS,BTUROW,0.0D0,BTUROW_mask,'ED$BTU,47')
             COL = 'B'//EPFLCD(FRG)//EPPLCD(IEFD)//CHCOD(NGSN)//CHCOD(IMD)//'X'; COL_mask='B(*)(***)(*)(**)'

!   put in BTU balance row
              CALL DVAL(COL,BTUROW,NEG1,COL_mask,BTUROW_mask,'ED$BTU,48')

!   put in NG supply balance row
              CALL DVAL(COL,NGROW,DBLE(GASSH),COL_mask,NGROW_mask,'ED$BTU,49')

              FLCST = 0.001
!   determine sequestered carbon and resulting adjustment to fuel cost
              IF (SEQRT .GT. 0.0) THEN
               COEFF = EGFEL(CURIYR) * 0.001 * GASSH * SEQRT
               IF (COEFF .GT. EFD_MIN) then
                  CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,50')
                  CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,51')
               endif
!   adjust fuel cost for carbon removal
               FLCST = -1.0 * EGFEL(CURIYR) * 0.001 * GASSH * SEQRT * (EMETAX(1,CURIYR) * 1000.0)
              ENDIF

!   put fuel cost in OBJ row
              CALL DVAL(COL,EFDOBJ,DBLE(FLCST),COL_mask,efdobj,'ED$BTU,52')

!   account for regional carbon limits, if any
              DO CAR = 1 , CO2_GRP
                COEFF = (1.0 - SEQRT) * EGFEL(CURIYR) * 0.001 * GASSH * CO2_OG_BY_FL(FRG,CAR)
!   account for NJ leaving and reentering RGGI
                IF (FLRGCODE(FRG) .EQ. 'YJ')THEN
                   COEFF = COEFF * (1.0 - CO2NJEXC(CAR,CURIYR))
                ELSEIF (FLRGCODE(FRG) .EQ. 'VA') THEN
                   COEFF = COEFF * (1.0 - CO2VAEXC(CAR,CURIYR))
                END IF
                IF (COEFF .GT. EFD_MIN)THEN
                   ROWCARC = 'CARBON' // CO2_RG(CAR); call makmsk(ROWCARC_mask,':CARBON:' , CO2_RG(CAR))
                   CALL DVAL(COL,ROWCARC,COEFF,COL_mask,ROWCARC_mask,'ED$BTU,53')
                END IF
              END DO

            ENDDO

         ELSEIF (IGS .EQ. 0 .AND. (IRS .EQ. 1 .OR. IDS .EQ. 1)) THEN ! oil only
           IMD = 1
           OILSH = 1.0
            BTUROW = 'BTU'//EPFLCD(FRG)//EPPLCD(IEFD)//'X'; call makmsk(BTUROW_mask,':BTU:',EPFLCD(FRG),EPPLCD(IEFD),'X')
            CALL DROWTYPE(BTUROW,'L       ',BTUROW_mask)
            CALL DRHS(EFDRHS,BTUROW,0.0D0,BTUROW_mask,'ED$BTU,54')
            IF (IRS .EQ. 1)THEN
              OLROW = 'RS' // OL_CD; call makmsk(OLROW_mask,':RS:' , OL_CD)
            ELSE
              OLROW = 'DS' // OL_CD; call makmsk(OLROW_mask,':DS:' , OL_CD)
            END IF
            COL = 'B'//EPFLCD(FRG)//EPPLCD(IEFD)//'X'//CHCOD(IMD)//'X'; COL_mask='B(*)(***)(*)(**)'

!   put in BTU balance row
              CALL DVAL(COL,BTUROW,NEG1,COL_mask,BTUROW_mask,'ED$BTU,55')

!   put in OL supply balance row
              CALL DVAL(COL,OLROW,DBLE(OILSH),COL_mask,OLROW_mask,'ED$BTU,56')

              FLCST = 0.001
!   determine sequestered carbon and resulting adjustment to fuel cost
             IF (SEQRT .GT. 0.0) THEN
               IF (IRS .EQ. 1) THEN
                   COEFF = ERSEL(CURIYR) * 0.001 * OILSH * SEQRT
                   FLCST = -1.0 * ERSEL(CURIYR) * 0.001 * OILSH * SEQRT * (EMETAX(1,CURIYR) * 1000.0)
               ELSE
                   COEFF = EDSEL(CURIYR) * 0.001 * OILSH * SEQRT
                   FLCST = -1.0 * EDSEL(CURIYR) * 0.001 * OILSH * SEQRT * (EMETAX(1,CURIYR) * 1000.0)
               ENDIF
               IF (COEFF .GT. EFD_MIN) then
                 CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,57')
                 CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,58')
               endif
             ENDIF

!   account for regional carbon limits, if any
             DO CAR = 1 , CO2_GRP
                IF (IRS .EQ. 1) THEN
                   COEFF = ERSEL(CURIYR) * 0.001 * OILSH * (1.0 - SEQRT) * CO2_OG_BY_FL(FRG,CAR)
                ELSE
                   COEFF = EDSEL(CURIYR) * 0.001 * OILSH * (1.0 - SEQRT) * CO2_OG_BY_FL(FRG,CAR)
                END IF
!   account for NJ leaving and reentering RGGI
                IF (FLRGCODE(FRG) .EQ. 'YJ')THEN
                   COEFF = COEFF * (1.0 - CO2NJEXC(CAR,CURIYR))
                ELSEIF (FLRGCODE(FRG) .EQ. 'VA') THEN
                   COEFF = COEFF * (1.0 - CO2VAEXC(CAR,CURIYR))
                END IF
                IF (COEFF .GT. EFD_MIN)THEN
                   ROWCARC = 'CARBON' // CO2_RG(CAR); call makmsk(ROWCARC_mask,':CARBON:' , CO2_RG(CAR))
                   CALL DVAL(COL,ROWCARC,COEFF,COL_mask,ROWCARC_mask,'ED$BTU,59')
                END IF
             END DO

!   put fuel cost in OBJ row
              CALL DVAL(COL,EFDOBJ,DBLE(FLCST),COL_mask,efdobj,'ED$BTU,60')

         ELSEIF (IGS .EQ. 1 .AND. (IRS .EQ. 1 .OR. IDS .EQ. 1)) THEN  !dual fired
           IMD = 1   ! max gas, min oil
           GASSH = MIN(1.0,EDMXGS(2,IEFD,FRG))
           OILSH = 1.0 - GASSH
           IF (IRS) IOL=UIRL
           IF (IDS) IOL=UIDS
           IF (IRS .EQ. 1)THEN
             OLROW = 'RS' // OL_CD; call makmsk(OLROW_mask,':RS:' , OL_CD)
           ELSE
             OLROW = 'DS' // OL_CD; call makmsk(OLROW_mask,':DS:' , OL_CD)
             OILSH = OILSH * DSADJ
           END IF
           DO NGSN = 1, 3
             BTUROW = 'BTU'//EPFLCD(FRG)//EPPLCD(IEFD)//CHCOD(NGSN); call makmsk(BTUROW_mask,':BTU:',EPFLCD(FRG),EPPLCD(IEFD),CHCOD(NGSN))
             CALL DROWTYPE(BTUROW,'L       ',BTUROW_mask)
             CALL DRHS(EFDRHS,BTUROW,0.0D0,BTUROW_mask,'ED$BTU,61')
             NGROW = 'NG' // GS_CD // CHCOD(NGSN); call makmsk(NGROW_mask,':NG:' , GS_CD , CHCOD(NGSN))
             COL = 'B'//EPFLCD(FRG)//EPPLCD(IEFD)//CHCOD(NGSN)//CHCOD(IMD)//'X'; COL_mask='B(*)(***)(*)(**)'
!   put in BTU balance row
              CALL DVAL(COL,BTUROW,NEG1,COL_mask,BTUROW_mask,'ED$BTU,62')

!   put in NG supply balance row
              CALL DVAL(COL,NGROW,DBLE(GASSH),COL_mask,NGROW_mask,'ED$BTU,63')

              FLCST = 0.001
              COEFF = 0.0
!   determine sequestered carbon and resulting adjustment to fuel cost
              IF (SEQRT .GT. 0.0) THEN
               COEFF = COEFF + EGFEL(CURIYR) * 0.001 * GASSH * SEQRT
!              IF (COEFF .GT. EFD_MIN) CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF))
!              IF (COEFF .GT. EFD_MIN) CALL DVAL(COL,ROWSEQ,DBLE(COEFF))
!   adjust fuel cost for carbon removal
               FLCST = -1.0 * EGFEL(CURIYR) * 0.001 * GASSH * SEQRT * (EMETAX(1,CURIYR) * 1000.0)
              ENDIF

!   put in OL supply balance row, if any

              IF (OILSH .GT. 0.0) THEN
                CALL DVAL(COL,OLROW,DBLE(OILSH),COL_mask,OLROW_mask,'ED$BTU,64')

!   determine sequestered carbon and resulting adjustment to fuel cost
                IF (SEQRT .GT. 0.0) THEN
                 IF (IRS .EQ. 1) THEN
                     COEFF = COEFF + ERSEL(CURIYR) * 0.001 * OILSH * SEQRT
                     FLCST = FLCST - ERSEL(CURIYR) * 0.001 * OILSH * SEQRT * (EMETAX(1,CURIYR) * 1000.0)
                 ELSE
                     COEFF = COEFF + EDSEL(CURIYR) * 0.001 * OILSH * SEQRT
                     FLCST = FLCST - EDSEL(CURIYR) * 0.001 * OILSH * SEQRT * (EMETAX(1,CURIYR) * 1000.0)
                 ENDIF
                ENDIF
              ENDIF

!   put in sequestered carbon in appropriate rows
              IF (SEQRT .GT. 0.0) THEN
                IF (COEFF .GT. EFD_MIN) then
                  CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,65')
                  CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,66')
                endif
              ENDIF

!   account for regional carbon limits, if any
             DO CAR = 1 , CO2_GRP
                IF (IRS .EQ. 1) THEN
                   COEFF = (1.0 - SEQRT) * ERSEL(CURIYR) * 0.001 * OILSH
                ELSE
                   COEFF = (1.0 - SEQRT) * EDSEL(CURIYR) * 0.001 * OILSH
                ENDIF
                COEFF = COEFF + (1.0 - SEQRT) * EGFEL(CURIYR) * 0.001 * GASSH
                COEFF = COEFF * CO2_OG_BY_FL(FRG,CAR)
!   account for NJ leaving and reentering RGGI
                IF (FLRGCODE(FRG) .EQ. 'YJ')THEN
                   COEFF = COEFF * (1.0 - CO2NJEXC(CAR,CURIYR))
                ELSEIF (FLRGCODE(FRG) .EQ. 'VA') THEN
                   COEFF = COEFF * (1.0 - CO2VAEXC(CAR,CURIYR))
                END IF
                IF (COEFF .GT. EFD_MIN)THEN
                   ROWCARC = 'CARBON' // CO2_RG(CAR); call makmsk(ROWCARC_mask,':CARBON:' , CO2_RG(CAR))
                   CALL DVAL(COL,ROWCARC,COEFF,COL_mask,ROWCARC_mask,'ED$BTU,67')
                END IF
             END DO

!   put fuel cost in OBJ row
              CALL DVAL(COL,EFDOBJ,DBLE(FLCST),COL_mask,efdobj,'ED$BTU,68')

            ENDDO

           IMD = 2   ! max oil, min gas
           IF (IRS) THEN
             OILSH = EDMXRS(2,IEFD,FRG)
             IOL = UIRL
           ELSE
             OILSH = EDMXDS(2,IEFD,FRG)
             IOL = UIDS
           ENDIF
           IF (OILSH .GT. 1.0) OILSH = 1.0
           GASSH = 1.0 - OILSH

           IF (IDS) OILSH = OILSH * DSADJ

            DO NGSN = 1, 3
             BTUROW = 'BTU'//EPFLCD(FRG)//EPPLCD(IEFD)//CHCOD(NGSN); call makmsk(BTUROW_mask,':BTU:',EPFLCD(FRG),EPPLCD(IEFD),CHCOD(NGSN))
             CALL DROWTYPE(BTUROW,'L       ',BTUROW_mask)
             CALL DRHS(EFDRHS,BTUROW,0.0D0,BTUROW_mask,'ED$BTU,69')
             NGROW = 'NG' // GS_CD // CHCOD(NGSN); call makmsk(NGROW_mask,':NG:' , GS_CD , CHCOD(NGSN))
             COL = 'B'//EPFLCD(FRG)//EPPLCD(IEFD)//CHCOD(NGSN)//CHCOD(IMD)//'X'; COL_mask='B(*)(***)(*)(**)'

!   put in BTU balance row

              CALL DVAL(COL,BTUROW,NEG1,COL_mask,BTUROW_mask,'ED$BTU,70')

              FLCST = 0.001
              COEFF = DBLE(0.0)
!   put in NG supply balance row, if any

              IF (GASSH .GT. 0.0) THEN
                CALL DVAL(COL,NGROW,DBLE(GASSH),COL_mask,NGROW_mask,'ED$BTU,71')
!   determine sequestered carbon and resulting adjustment to fuel cost
                IF (SEQRT .GT. 0.0) THEN
                 COEFF = COEFF + EGFEL(CURIYR) * 0.001 * GASSH * SEQRT
!                IF (COEFF .GT. EFD_MIN) CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF))
!                IF (COEFF .GT. EFD_MIN) CALL DVAL(COL,ROWSEQ,DBLE(COEFF))
!   adjust fuel cost for carbon removal
                 FLCST = -1.0 * EGFEL(CURIYR) * 0.001 * GASSH * SEQRT * (EMETAX(1,CURIYR) * 1000.0)
                ENDIF
              ENDIF

!   put in OL supply balance row
              CALL DVAL(COL,OLROW,DBLE(OILSH),COL_mask,OLROW_mask,'ED$BTU,72')

!   determine sequestered carbon and resulting adjustment to fuel cost
                IF (SEQRT .GT. 0.0) THEN
                 IF (IRS .EQ. 1) THEN
                     COEFF = COEFF + ERSEL(CURIYR) * 0.001 * OILSH * SEQRT
                     FLCST = FLCST - ERSEL(CURIYR) * 0.001 * OILSH * SEQRT * (EMETAX(1,CURIYR) * 1000.0)
                 ELSE
                     COEFF = COEFF + EDSEL(CURIYR) * 0.001 * OILSH * SEQRT
                     FLCST = FLCST - EDSEL(CURIYR) * 0.001 * OILSH * SEQRT * (EMETAX(1,CURIYR) * 1000.0)
                 ENDIF
                 IF (COEFF .GT. EFD_MIN) then
                   CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,73')
                   CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,74')
                 endif
                ENDIF

!   account for regional carbon limits, if any
                DO CAR = 1 , CO2_GRP
                   IF (IRS .EQ. 1) THEN
                      COEFF = (1.0 - SEQRT) * ERSEL(CURIYR) * 0.001 * OILSH
                   ELSE
                      COEFF = (1.0 - SEQRT) * EDSEL(CURIYR) * 0.001 * OILSH
                   ENDIF
                   COEFF = COEFF + (1.0 - SEQRT) * EGFEL(CURIYR) * 0.001 * GASSH
                   COEFF = COEFF * CO2_OG_BY_FL(FRG,CAR)
!   account for NJ leaving and reentering RGGI
                   IF (FLRGCODE(FRG) .EQ. 'YJ')THEN
                      COEFF = COEFF * (1.0 - CO2NJEXC(CAR,CURIYR))
                   ELSEIF (FLRGCODE(FRG) .EQ. 'VA') THEN
                      COEFF = COEFF * (1.0 - CO2VAEXC(CAR,CURIYR))
                   END IF
                   IF (COEFF .GT. EFD_MIN)THEN
                      ROWCARC = 'CARBON' // CO2_RG(CAR); call makmsk(ROWCARC_mask,':CARBON:' , CO2_RG(CAR))
                      CALL DVAL(COL,ROWCARC,COEFF,COL_mask,ROWCARC_mask,'ED$BTU,75')
                   END IF
                END DO

!   put fuel cost in OBJ row
              CALL DVAL(COL,EFDOBJ,DBLE(FLCST),COL_mask,efdobj,'ED$BTU,76')

            ENDDO

         ELSEIF (IWD .EQ. 1) THEN   ! biomass
             IMD = 1
             BTUROW = 'BTU'//EPFLCD(FRG)//EPPLCD(IEFD)//'X'; call makmsk(BTUROW_mask,':BTU:',EPFLCD(FRG),EPPLCD(IEFD),'X')
             CALL DROWTYPE(BTUROW,'L       ',BTUROW_mask)
             CALL DRHS(EFDRHS,BTUROW,0.0D0,BTUROW_mask,'ED$BTU,77')
             COL = 'B'//EPFLCD(FRG)//EPPLCD(IEFD)//'X'//CHCOD(IMD)//'X'; COL_mask='B(*)(***)(*)(**)'

!            FLCST = UPFUEL(UIWD,EPCLMP(FRG))

             FLCST = 0.001

!   put fuel cost in OBJ row
             CALL DVAL(COL,EFDOBJ,DBLE(FLCST),COL_mask,efdobj,'ED$BTU,78')

!   put in BTU balance row
             CALL DVAL(COL,BTUROW,NEG1,COL_mask,BTUROW_mask,'ED$BTU,79')
!   put in WD supply balance row
             CALL DVAL(COL,WDROW,1.0D0,COL_mask,WDROW_mask,'ED$BTU,80')

!   determine sequestered carbon
             IF (SEQRT .GT. 0.0) THEN
               COEFF = (26.0) * 0.001 * SEQRT
               FLCST = 0.001
               IF (COEFF .GT. EFD_MIN) THEN
                  CALL DVAL(COL,ROWCAR,DBLE(-1.0*COEFF),COL_mask,ROWCAR_mask,'ED$BTU,84')
                  CALL DVAL(COL,ROWSEQ,DBLE(COEFF),COL_mask,ROWSEQ_mask,'ED$BTU,85')
               ENDIF
            ENDIF

!   account for regional carbon limits, if any
!            DO CAR = 1 , CO2_GRP
!               COEFF = (1.0 - SEQRT) * (26.0) * 0.001 !* CO2_OG_BY_FL(FRG,CAR)
!               IF (COEFF .GT. EFD_MIN)THEN
!                  ROWCARC = 'CARBON' // CO2_RG(CAR); call makmsk(ROWCARC_mask,':CARBON:' , CO2_RG(CAR))
!                  CALL DVAL(COL,ROWCARC,COEFF,COL_mask,ROWCARC_mask,'ED$BTU,86')
!               END IF
!            END DO
         ENDIF
        ENDIF
	    ENDIF
       ENDDO  ! IEFD
      ENDDO   ! FRG

!  set up nuclear fuel row/column - just nationwide price
      DO IEFD = UICNU, UISMR
        BTUROW = 'BTU'//'X'//EPPLCD(IEFD)//'X'; call makmsk(BTUROW_mask,':BTU:','X',EPPLCD(IEFD),'X')
        IMD = 1
        CALL DROWTYPE(BTUROW,'L       ',BTUROW_mask)
        CALL DRHS(EFDRHS,BTUROW,0.0D0,BTUROW_mask,'ED$BTU,81')
        COL = 'B'//'X'//EPPLCD(IEFD)//'X'//CHCOD(IMD)//'X'; COL_mask='B(*)(***)(*)(**)'

        FLCST = UPFUEL(UIUF,EFD_D_MFRG)
        CALL DVAL(COL,EFDOBJ,DBLE(FLCST),COL_mask,efdobj,'ED$BTU,82')
        CALL DVAL(COL,BTUROW,NEG1,COL_mask,BTUROW_mask,'ED$BTU,83')
       ENDDO
      colnam_AIMMS= ' '
      colnam_mask=' '
      rownam_AIMMS=' '
      rownam_mask=' '
      aimms_col_ID_num=-1
      aimms_row_ID_num=-1
      
    ! SET UP HYDROGEN BALNCE ROWS
      IF (CURIYR+1989 .GE. UPSTYR) THEN
         CALL ED$BTU_HYDROGEN
      END IF
      
      RETURN
      END
      
      SUBROUTINE ED$BTU_HYDROGEN
      ! <EDWARD.THOMAS@EIA.GOV> EDT 5/12/2021
      
      ! THE PURPOSE OF THIS SUBROUTINE IS TO ADD THE "BALANCE ROWS" FOR THE HYDROGEN PLANT. IN THIS CASE, WE ARE 
      ! USING THE ICE PLANT TYPE AS THE HYDROGEN PLANT DUE TO THE COMPLEXITY OF ADDING NEW PLANTS INTO NEMS. 
      ! WE ARE ALSO USING THE COMPETITIVE NATURAL GAS FUEL AS THE HYDROGN GAS FOR THE SAME REASON. 
      ! 
      ! IN THE PLANT DATA FILE (PLTDATA.TXT) CONTAINS THE DEFINATIONS OF THE FUEL MAKE UP FOR EACH PLANT. THERE ARE 
      ! THREE DIFFERENT FUEL TYPES THAT CAN BE ADDED FOR A GIVEN PLANT. iN THIS CASE WE ONLY PUT THE COMPETITIVE NATURAL GAS,
      ! WHICH IS NOW HYDROGNE FOR THE ICE PLANT, AND PUT THE FUEL TYPE 2 & 3 AS NOTHING. 
      !
      ! THIS ROUTINE BELOW LOOPS OVER ALL THE DIFFERENT TYPES OF PLANTS, GOES INTO THE UIGAS ARRAY TO SEE IF ITS ONLY GAS (NOT FOSSIL FUEL TOO)
      ! THEN LOOPS OVER THE WFLTP ARRAY WHICH IS THE MAPPING BETWEEN THE PLANT AND FUEL TYPE.  
      
      USE EFD_ROW_COL ! DEClarations, shared storage for LP coefficients for AIMMS 
      USE EPHRTS_SWTICHES
      USE EPHRTS_FILE_UNIT_NUMBERS 

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'cdsparms'
      include'control'
      include'fuelin'
      include'dispin'
      include'ecpcntl'
      include'bildin'
      include'plntctl'
      include'angtdm'
      include'uso2grp'
      include'emission'
      include'uecpout'
      include'coalemm'
      include'emeblk'
      include'emoblk'
      include'ecp_nuc'
      include'csapr'
      include'emmemis'
      include'eusprc'
      include'edbdef'

      
      INTEGER IEFD, IFL, FRG, IMD, SSN
      REAL*8 NEG1/-1.0D0/, HYDROGEN_SHARE
      LOGICAL HYDROGEN_ONLY, HYDROGEN_FOUND
      CHARACTER*16 BTUROW, ROW_S, COL
      CHARACTER*32 AIMMS_LABEL
      REAL FLCST
      INTEGER CRG, GRG,ORG,HRG
      CHARACTER*2 BR,CL_CD,GS_CD,OL_CD,CA_CD
      LOGICAL E_DEBUG_EXIST
                  
      ! BOOKKEEPING CODE TO FIGURE OUT FUEL MIX UP BEFORE ADDING BALANCE ROW
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
         INQUIRE(FILE="EPHRTS_DEBUG_FILE.TXT", EXIST=E_DEBUG_EXIST)
         IF (E_DEBUG_EXIST) THEN
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="OLD", POSITION="APPEND", ACTION="WRITE")
         ELSE
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="NEW", ACTION="WRITE")
         END IF
            
         WRITE(unit_num_ephrts_debug_file, *) "ATTEMPTING TO ADD HYDROGEN BALANCE VECTORS"
      END IF
      DO FRG = 1, UNFRGN
      
         ! COAL REGIONS 
         CRG = EPCLMP(FRG) 
         WRITE(BR,'(I2.2)') CRG
         CL_CD = BR
!         WDROW = 'S_WD'// BR; call makmsk(WDROW_mask,':S_WD:', BR)
!
         ! GAS REGIONS
         GRG = EPGSMP(FRG)
         WRITE(BR,'(I2.2)') GRG
         GS_CD = BR
!         NGROW = 'NG' // BR; call makmsk(NGROW_mask,':NG:' , BR)
!
         ! CENSUS REGIONS
         ORG = EPCSMP(FRG)
         WRITE(BR,'(I2.2)') ORG
         OL_CD = BR
!         OLROW = 'DS' // BR; call makmsk(OLROW_mask,':DS:' , BR)
!
         ! CARBON REGIONS
         HRG = EPCAMP(FRG)
         WRITE(BR,'(I2.2)') HRG
         CA_CD = BR
!         ROWCARR = 'CARBON' // CO2_RG(HRG); call makmsk(ROWCARR_mask,':CARBON:' , CO2_RG(HRG))
         
         !DO IEFD = 1, EFD$CAP
         HYDROGEN_ONLY = .FALSE.
         HYDROGEN_FOUND = .FALSE.
            
         DO IFL = 1, EFD_D_FPP 
            
            IF (WFLTP(UIICE,1) .GT. 0) THEN 
               IF (UIGAS(WFLTP(UIICE,1)) .EQ. 2) THEN ! THIS CHECKS TO SEE IF IT'S ONLY HYDROGEN GAS. TYPICALLY IT'S 1 OR 0, I PUT 2 SO THAT WAY IT'S UNIQUE. THAT SAID, MORE CODE IS REQUIED TO MIX HYDROGEN W/ FOSSIL FUEL
                  HYDROGEN_ONLY = .TRUE.
               END IF
               IF (WFLTP(UIICE,1) .EQ. UIGC) THEN 
                  HYDROGEN_FOUND = .TRUE.
               ENDIF
            END IF
               
         ENDDO
            
         ! ONLY WORK ON HYDROGEN GAS, ANOTHER BLOCK OF CODE IS REQUIRED FOR MIXED FUEL TYPES ( LIKE HYDROGEN MIXED WITH NATURAL GAS) 
            
         IF ((HYDROGEN_ONLY .EQ. .TRUE.) .AND. (HYDROGEN_FOUND .EQ. .TRUE.)) THEN 
            
            DO SSN = 1, 3
            
               ! CHECK TO SEE IF THERES'S CAPACITY PRIOR TO ADDING ANY BLANCE ROWS
            
               IF (CPFLEFD(UIICE,ORG,CRG,GRG) .GT. 0.0) THEN ! THIS IS POPULATED BY ECP, IT CHECKS TO SEE IF THERE IS CAPACITY BEFORE ADDING A BALANCE ROW, WE COMMENTED IT OUT FOR NOW TO TEST OUR CODE

               ! ONLY HYDROGEN, NO OTHER FUEL TYPES ADDED (FOR EXAMPLE, NATURAL GAS)
                  IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
                     WRITE(unit_num_ephrts_debug_file, *) "INSERING HYDROGEN BALANCE ROWS FOR TECH TYPE ", EPPLCD(UIICE), ", REG ", EPFLCD(FRG), ", Seas ", SSN
                  END IF
                  IMD = 1 ! MODE IS WHICH MIX OF FUEL IS USING (FOR EXAMPLE, DUAL FIRED), BUT RIGHT NOW USE 1
                  HYDROGEN_SHARE = 1.0
                  BTUROW = 'BTU'//EPFLCD(FRG)//EPPLCD(UIICE)//CHCOD(SSN); call makmsk(BTUROW_mask,':BTU:' , EPFLCD(FRG) , EPPLCD(UIICE),CHCOD(SSN) )
                  ROW_S = 'S_H2'//OL_CD//CHCOD(SSN); call makmsk(ROW_S_mask,':S_H2:',OL_CD,CHCOD(SSN)) ! ROW_S IS SUPPLY ROW 

                  CALL DROWTYPE(BTUROW,'L       ',BTUROW_mask)
                  CALL DRHS(EFDRHS,BTUROW,0.0D0,BTUROW_mask,'ED$BTU_H2,1')
                  COL = 'B'//EPFLCD(FRG)//EPPLCD(UIICE)//CHCOD(SSN)//CHCOD(IMD)//'X'; COL_mask='B(*)(***)(*)(**)'
    
                  AIMMS_LABEL = 'B_ED$H2BTU' // OL_CD 
                  IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
                     WRITE(unit_num_ephrts_debug_file, *) " ... ADDING BTU BALANCE ROW ", AIMMS_LABEL    
                  END IF
                  !   PUT IN BTU BALANCE ROW
                  CALL DVAL(COL,BTUROW,NEG1,COL_mask,BTUROW_mask,'ED$BTU_H2,2') 
      
                  AIMMS_LABEL = 'S_ED$H2BTU' // OL_CD 
                  IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
                     WRITE(unit_num_ephrts_debug_file, *) " ... ADDING SUPPLY BALANCE ROW ", AIMMS_LABEL
                  END IF
                  !   PUT IN THE HYDROGEN SUPPLY BALANCE ROW
                  CALL DVAL(COL,ROW_S,DBLE(HYDROGEN_SHARE),COL_mask,ROW_S_mask,'ED$BTU_H2,3')
      
                  FLCST = 0.001 
                  
                  ! NOTE: THE REASON WHY FLCST IS SET TO 0.001, IS BECAUSE THIS IS THE MINIMUM VALUE FOR THE COST PRIOR TO THE ACTUAL OPTIMIZATION. 
                  ! IN THE OPERATE ROUTINE DOWNSTREAM, THERE IS ADDITIONAL CODE THAT IS ACTING ON THIS BTUROW FOR THE RESPECTIVE PLANT TO PROVIDE IT A CERTAIN AMOUNT OF FUEL 
                  IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
                     WRITE(unit_num_ephrts_debug_file, *)" ... ADDING FUEL COST IN OBJ ROW ", AIMMS_LABEL
                  END IF
                  AIMMS_LABEL = 'FLCST_ED$H2BTU' // OL_CD 
      
                  !   PUT FUEL COST IN OBJ ROW
                  CALL DVAL(COL,EFDOBJ,DBLE(FLCST),COL_mask,efdobj,'ED$BTU_H2,4')
               ELSE
               IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
                  WRITE(unit_num_ephrts_debug_file, *) "No Capacity, skip adding hydrogen balance vectors for: REG ", EPFLCD(FRG), ", Seas ", SSN
               END IF
               END IF
            END DO
         END IF 
      END DO
      
      !END DO
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
         CLOSE(unit_num_ephrts_debug_file)
      END IF
      RETURN
      END
!
!     This subroutine sets up the energy balance row for hydro and pumped storage
!
      SUBROUTINE ED$RNW
      use efd_row_col
      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'ecpcntl'
      include'ecp_nuc'
      include'uefdout'

      REAL*8 DIGITS2
      INTEGER IRG,IS,IRET,N,IP,IRNW,IGRP,I_EFD_GRP,IECP,FULLYR
      REAL  MXNRG,CAP,MAXCF,TST
      CHARACTER*5 NUM
      CHARACTER*16 ROWNRG,ROWBMS,COL

      efdsub='ED$RNW'

!
      FULLYR = USYEAR(CURIYR)
!
      DO I_EFD_GRP = 1 , N_EFD_GRPS
        IRG = UG_EMM_RG(I_EFD_GRP)
        IP = UG_EFDt(I_EFD_GRP)
        IGRP = EFD_GRPS_F(I_EFD_GRP)
        IECP = UG_ECPt(I_EFD_GRP)
        IF (IP .EQ. UIHYR .OR. (IP .EQ. UIHYC .AND. CURIYR + UHBSYR .LT. UPSTYR)) THEN
!        IF (IECP .NE. WIP2) THEN
          DO IS = 1, EENSP
            WRITE(NUM,'(I5.5)') IGRP
            CAP = UG_CAP(IS,I_EFD_GRP)
            DIGITS_PARM = 6
            IF (USW_DIGIT .GT. 0)THEN
            TST = DIGITS2(DBLE(CAP * 0.001),DIGITS_PARM)
            ELSE
               TST = DBLE(CAP * 0.001)
            END IF 
            MAXCF = UG_SCF(IS,I_EFD_GRP)

!STEOBM    Adjust max CF for STEO benchmarking hydro (factor will be 1.0 if not benchmarking)
            IF (IECP .EQ. WIHY) THEN
              MAXCF = MAXCF * URHYCFA(CURIYR)
            ENDIF

            MXNRG = CAP * 0.001 * MAXCF * EETIME(IS)
            IF (IP .EQ. UIHYR .AND. TST .GT. 0.0 .AND. IECP .NE. WIP2) THEN    ! UIHYR is reversible hydro/pumped storage. wip2 is other storage
              ROWNRG = 'EN'//NUM//CHCOD(IS); call makmsk(ROWNRG_mask,':EN:',NUM,CHCOD(IS))
              CALL DROWTYPE(ROWNRG,'E       ',ROWNRG_mask)
            ELSE
              ROWNRG = 'EN'//NUM//CHCOD(IS); call makmsk(ROWNRG_mask,':EN:',NUM,CHCOD(IS))
              CALL DROWTYPE(ROWNRG,'L       ',ROWNRG_mask)
            ENDIF
            CALL DRHS(EFDRHS,ROWNRG,DBLE(MXNRG*0.1),ROWNRG_mask,'ED$RNW,1')
          ENDDO
!        ENDIF
        ENDIF
      ENDDO

!  set up biomass generation row if needed for RPS
      IF (EFDMAT .EQ. 0) THEN
        DO IRG = 1 , UNRGNS
          ROWBMS = 'G'//URGNME(IRG)(1:4)//EPPLCD(UIBMS); call makmsk(ROWBMS_mask,':G:',URGNME(IRG)(1:4),EPPLCD(UIBMS))
!         IF ((FULLYR .GE. UPSTYR) .AND.  &
!            (UPRNWBND(CURIYR) .GT. 0.005 .OR. UPRNWBNDR(CURIYR,IRG) .GT. 0.005) .AND.  &
!            (UPRNWSHR(WIWD) .GT. 0.000 .OR. UPRNWSHRR(WIWD,IRG) .GT. 0.000))THEN
!           CALL DROWTYPE(ROWBMS,'G       ',ROWBMS_mask)
!           CALL DRHS(EFDRHS,ROWBMS,DBLE(UPBMGNN(IRG)))
!  escape vector
!           COL = 'X'//URGNME(IRG)(1:4)//EPPLCD(UIBMS)
!           CALL DVAL(COL,EFDOBJ,DBLE(2000.0))
!           CALL DVAL(COL,ROWBMS,DBLE(1.0))
!         ELSE
            CALL DROWTYPE(ROWBMS,'N       ',ROWBMS_mask)
!         ENDIF
        ENDDO
      ENDIF

!  set up biomass CCS generation row if needed for RPS
      IF (EFDMAT .EQ. 0) THEN
        DO IRG = 1 , UNRGNS
          ROWBMS = 'G'//URGNME(IRG)(1:4)//EPPLCD(UIBIG); call makmsk(ROWBMS_mask,':G:',URGNME(IRG)(1:4),EPPLCD(UIBIG))
!         IF ((FULLYR .GE. UPSTYR) .AND.  &
!            (UPRNWBND(CURIYR) .GT. 0.005 .OR. UPRNWBNDR(CURIYR,IRG) .GT. 0.005) .AND.  &
!            (UPRNWSHR(WIBI) .GT. 0.000 .OR. UPRNWSHRR(WIBI,IRG) .GT. 0.000))THEN
!           CALL DROWTYPE(ROWBMS,'G       ',ROWBMS_mask)
!           CALL DRHS(EFDRHS,ROWBMS,DBLE(UPBMGNN(IRG)))
!  escape vector
!           COL = 'X'//URGNME(IRG)(1:4)//EPPLCD(UIBIG)
!           CALL DVAL(COL,EFDOBJ,DBLE(2000.0))
!           CALL DVAL(COL,ROWBMS,DBLE(1.0))
!         ELSE
            CALL DROWTYPE(ROWBMS,'N       ',ROWBMS_mask)
!         ENDIF
        ENDDO
      ENDIF

      RETURN
      END
!
!     This subroutine sets up the trade network
!
      SUBROUTINE ED$TRADE
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'dispin'
      include'postpr'
      include'dispcrv'
      include'dispett'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'ecp_nuc'
      include'uefdout'

      INTEGER IMRG,EXRG,IS,ISTP,ISEG,SL,GRP,CRG,CSTP,ISW,IRET,XCT,TST_RGN(MNUMNR+EFD_D_PROV,MNUMNR+EFD_D_PROV),CAR
      REAL TRANCST,CONVAL,HRS,XVAL,MVAL
      REAL CAIMP(2)
      REAL*8 VAL,COST
      CHARACTER*1 SLCOD
      CHARACTER*16 COL,ROWIM,ROWEX,ROW_TR,ROWBAL,ROW_LD,ROWX,ROWM,ROWCAR,COL_SR,ROW_SR,ROW_TL,ROWEX_SR,ROWIM_SR
      CHARACTER*16 ROW

!     USE SEPARATE CO2 FACTORS FOR TRADE WITH AZNM (1) AND NWPP (2)
      DATA CAIMP/0.2321,0.0886/

      efdsub='ED$TRADE'

      DO IMRG = 1, UNRGNS
         DO IS = 1, EENSP
            DO ISTP = 1 , ELNVCT(IS)
               ISEG = ELSEG(ISTP,IS)
               GRP = ELGRP(ISTP,IS)
               SL = EFD_Slice_ID(GRP,ISEG)
               SLCOD = CHAR(64+SL)
               HRS = UTWDTH(ISEG,GRP)

!              create overall import and export constraints for each region

               ROWX = 'EX'//URGNME(IMRG)(1:4)//CHCOD(IS)//SLCOD; call makmsk(ROWX_mask,':EX:',URGNME(IMRG)(1:4),CHCOD(IS),SLCOD)
               ROWM = 'IM'//URGNME(IMRG)(1:4)//CHCOD(IS)//SLCOD; call makmsk(ROWM_mask,':IM:',URGNME(IMRG)(1:4),CHCOD(IS),SLCOD)
               CALL DROWTYPE(ROWX,'L       ',ROWX_mask)
               CALL DROWTYPE(ROWM,'L       ',ROWM_mask)
               XVAL = URNCSTEX_EFD(IS,IMRG)
               MVAL = URNCSTIM_EFD(IS,IMRG)

               IF (MVAL .LT. 0.0) THEN
                  WRITE(6,3417) CURIRUN, CURIYR+1989, CURITR, IMRG, UNRGNS, IS, EENSP, ISTP, ELNVCT(IS), ISEG, GRP, SL, ROWM, HRS, MVAL
 3417             FORMAT(1X,"TRADE_OOPS",12(":",I6),":",A16,2(":",F21.6))

                  MVAL = 0.0
               END IF

               IF (XVAL .LT. 0.0) THEN
                  WRITE(6,3417) CURIRUN, CURIYR+1989, CURITR, IMRG, UNRGNS, IS, EENSP, ISTP, ELNVCT(IS), ISEG, GRP, SL, ROWM, HRS, XVAL

                  XVAL = 0.0
               END IF

               CALL DRHS(EFDRHS,ROWX,DBLE(XVAL),ROWX_mask,'ED$TRADE,1')
               CALL DRHS(EFDRHS,ROWM,DBLE(MVAL),ROWM_mask,'ED$TRADE,2')

!              loop over exporting regions

               DO EXRG = 1, UNRGNS
                  IF (CNSTRNTS_EFD(IS,CURIYR,IMRG,EXRG) .GT. 0.001) THEN
                     COL = 'TR'//URGNME(EXRG)(6:7)//URGNME(IMRG)(6:7)//CHCOD(IS)//SLCOD; call makmsk(COL_mask,':TR:',URGNME(EXRG)(6:7),URGNME(IMRG)(6:7),CHCOD(IS),SLCOD)
                     CONVAL = CNSTRNTS_EFD(IS,CURIYR,IMRG,EXRG)
                     CALL DBND(EFDBND,COL,0.0D0,DBLE(CONVAL),COL_mask,'ED$TRADE,3')

                     IF (SR_TRAN_CREDIT .GT. 0.0) THEN
                        ROW_TL = 'TL'//URGNME(EXRG)(6:7)//URGNME(IMRG)(6:7)//CHCOD(IS)//SLCOD; call makmsk(ROW_TL_mask,':TL:',URGNME(EXRG)(6:7),URGNME(IMRG)(6:7),CHCOD(IS),SLCOD)
                        CALL DROWTYPE(ROW_TL,'L       ',ROW_TL_mask)
                        CALL DRHS(EFDRHS,ROW_TL,DBLE(CONVAL),ROW_TL_mask,'ED$TRADE,4')
                        CALL DVAL(COL,ROW_TL,1.0D0,COL_mask,ROW_TL_mask,'ED$TRADE,5')

                        COL_SR = 'SR'//URGNME(EXRG)(6:7)//URGNME(IMRG)(6:7)//CHCOD(IS)//SLCOD; call makmsk(COL_SR_mask,':SR:',URGNME(EXRG)(6:7),URGNME(IMRG)(6:7),CHCOD(IS),SLCOD)
                        CALL DBND(EFDBND,COL_SR,0.0D0,DBLE(CONVAL),COL_SR_mask,'ED$TRADE,6')
                     END IF

                     TRANCST = PTHRESH1(CURIYR,EXRG,IMRG) + PTHRESH2(CURIYR,EXRG,IMRG) + BARRIER(CURIYR)

!                    put transmission markup in obj function, OBJ units million $ / GW

                     CALL DVAL(COL,EFDOBJ,DBLE(TRANCST*HRS*0.001),COL_mask,efdobj,'ED$TRADE,7')

                     IF (SR_TRAN_CREDIT .GT. 0.0) THEN
                        CALL DVAL(COL_SR,EFDOBJ,DBLE((1.0-SR_TRAN_CREDIT)*TRANCST*HRS*0.001),COL_SR_mask,EFDOBJ,'ED$TRADE,8')
                     END IF

!                    put trade in load rows of importing and exporting regions

                     ROWIM = 'LD'//URGNME(IMRG)(1:4)//CHCOD(IS)//SLCOD; call makmsk(ROWIM_mask,':LD:',URGNME(IMRG)(1:4),CHCOD(IS),SLCOD)
                     ROWEX = 'LD'//URGNME(EXRG)(1:4)//CHCOD(IS)//SLCOD; call makmsk(ROWEX_mask,':LD:',URGNME(EXRG)(1:4),CHCOD(IS),SLCOD)
                     CALL DVAL(COL,ROWIM,DBLE((1.0-LINELOSS)*HRS*0.1),COL_mask,ROWIM_mask,'ED$TRADE,9')
                     CALL DVAL(COL,ROWEX,DBLE(-1.0*HRS*0.1),COL_mask,ROWEX_mask,'ED$TRADE,10')

                     IF (SR_TRAN_CREDIT .GT. 0.0) THEN
                        CALL DVAL(COL_SR,ROWIM,DBLE((1.0-SR_TRAN_CREDIT)*(1.0-LINELOSS)*HRS*0.1),COL_SR_mask,ROWIM_mask,'ED$TRADE,15')
                        CALL DVAL(COL_SR,ROWEX,DBLE((1.0-SR_TRAN_CREDIT)*-1.0*HRS*0.1),COL_SR_mask,ROWEX_mask,'ED$TRADE,16')

                        ROWIM_SR = 'SR'//URGNME(IMRG)(1:4)//CHCOD(IS)//SLCOD; call makmsk(ROWIM_SR_mask,':SR:',URGNME(IMRG)(1:4),CHCOD(IS),SLCOD)
                        ROWEX_SR = 'SR'//URGNME(EXRG)(1:4)//CHCOD(IS)//SLCOD; call makmsk(ROWEX_SR_mask,':SR:',URGNME(EXRG)(1:4),CHCOD(IS),SLCOD)
                        CALL DVAL(COL_SR,ROWIM_SR,DBLE(SR_TRAN_CREDIT*(1.0-LINELOSS)),COL_SR_mask,ROWIM_SR_mask,'ED$TRADE,17')
                        CALL DVAL(COL_SR,ROWEX_SR,DBLE(SR_TRAN_CREDIT*-1.0),COL_SR_mask,ROWEX_SR_mask,'ED$TRADE,18')
                     END IF

!                    put trade in import and export constraints

                     ROWX = 'EX'//URGNME(EXRG)(1:4)//CHCOD(IS)//SLCOD; call makmsk(ROWX_mask,':EX:',URGNME(EXRG)(1:4),CHCOD(IS),SLCOD)
                     CALL DVAL(COL,ROWX,1.0D0,COL_mask,ROWX_mask,'ED$TRADE,19')
                     ROWM = 'IM'//URGNME(IMRG)(1:4)//CHCOD(IS)//SLCOD; call makmsk(ROWM_mask,':IM:',URGNME(IMRG)(1:4),CHCOD(IS),SLCOD)
                     CALL DVAL(COL,ROWM,1.0D0,COL_mask,ROWM_mask,'ED$TRADE,20')

                     IF (SR_TRAN_CREDIT .GT. 0.0) THEN
                        CALL DVAL(COL_SR,ROWX,1.0D0,COL_SR_mask,ROWX_mask,'ED$TRADE,21')
                        CALL DVAL(COL_SR,ROWM,1.0D0,COL_SR_mask,ROWM_mask,'ED$TRADE,22')
                     END IF

!                    put imports/exports in carbon constraint, if appropriate

                     DO CAR = 1 , CO2_GRP
                        IF (CO2_IM_BY_RG(IMRG,CAR) .GT. 0.0 .AND. CO2_DE_BY_RG(EXRG,CAR) .GT. 0.0)THEN

!                          if (curitr .eq. 1)write(6,1234) curiyr+1989,col,conval,hrs,co2_im_by_rg(imrg,car),  &
!                                                                          conval*hrs*co2_im_by_rg(imrg,car)
!1234 format(1h ,'!catr',i4,1x,a8,5f10.3)

                           ROWCAR = 'CARBON' // CO2_RG(CAR); call makmsk(ROWCAR_mask,':CARBON:' , CO2_RG(CAR))

!                          VAL = DBLE(0.001 * HRS * CO2_IM_BY_RG(IMRG,CAR))

                           IF (CAR .EQ. CARGRP_CA)THEN
                              VAL = DBLE(0.001 * HRS * CO2_DE_BY_CA(CURIYR))
                           ELSE
                              VAL = DBLE(0.001 * HRS * CO2_DE_BY_RG(EXRG,CAR))
                           END IF

!                          USE DIFFERENT EMISSION FACTORS FOR CA IMPORTS FROM AZNM AND NWPP
!
!                          IF (URGNME(IMRG)(1:2) .EQ. 'ca')THEN
!                             IF (URGNME(EXRG)(1:2) .EQ. 'az')THEN
!                                VAL = DBLE(0.001 * HRS * CAIMP(1))
!                             END IF
!                             IF (URGNME(EXRG)(1:2) .EQ. 'nw')THEN
!                                VAL = DBLE(0.001 * HRS * CAIMP(2))
!                             END IF
!                          END IF

                           CALL DVAL(COL,ROWCAR,VAL,COL_mask,ROWCAR_mask,'ED$TRADE,23')

                           IF (SR_TRAN_CREDIT .GT. 0.0) THEN
                              CALL DVAL(COL_SR,ROWCAR,DBLE((1.0-SR_TRAN_CREDIT)*VAL),COL_SR_mask,ROWCAR_mask,'ED$TRADE,24')
                           END IF

                        END IF

!                       IF (CO2_IM_BY_RG(EXRG,CAR) .GT. 0.0)THEN
!                          ROWCAR = 'CARBON' // CO2_RG(CAR)
!                          VAL = DBLE(0.001 * HRS * CO2_IM_BY_RG(EXRG,CAR))
!                          CALL DVAL(COL,ROWCAR,VAL)
!
!                          IF (SR_TRAN_CREDIT .GT. 0.0) THEN
!                             CALL DVAL(COL_SR,ROWCAR,DBLE(((1.0-SR_TRAN_CREDIT)*VAL))
!                          END IF
!
!                       END IF

                     END DO

!                    account for imports/exports in 111d, if appropriate
   
                     IF (CO2_STDSW .GT. 0 .AND. CO2_TRDSW .GT. 0 .AND. CO2_STDGN(EXRG,IMRG) .LE. 0)THEN
                        IF (CO2_STDRN(EXRG,CURIYR) .GT. 0.0 .AND. CO2_STDRN(IMRG,CURIYR) .GT. 0.0)THEN
                           VAL = DBLE(HRS * 0.001 * (CO2_STDRN(EXRG,CURIYR) - CO2_STDRN(IMRG,CURIYR)))
!                          if (exrg .eq. 11 .and. imrg .eq. 15 .and. curitr .eq. 1)write(6,1357) curiyr+1989,col,  &
!                          HRS , CO2_STDRN(EXRG,CURIYR) , CO2_STDRN(IMRG,CURIYR), VAL
!1357 format(1h ,'!co2tr',i4,a10,4f12.4)
                           ROWCAR = 'CO2RNR' // URGNME(EXRG)(6:7); call makmsk(ROWCAR_mask,':CO2RNR:' , URGNME(EXRG)(6:7))
                           CALL DVAL(COL,ROWCAR,-VAL,COL_mask,ROWCAR_mask,'ED$TRADE,25')
                           ROWCAR = 'CO2RNR' // URGNME(IMRG)(6:7); call makmsk(ROWCAR_mask,':CO2RNR:' , URGNME(IMRG)(6:7))
                           CALL DVAL(COL,ROWCAR,VAL,COL_mask,ROWCAR_mask,'ED$TRADE,26')
!                          ALSO ADJUST CO2 ROWS IF MASSED BASED LIMIT
                           IF (CO2_STDTN(EXRG) .EQ. 2 .AND. CO2_STDTN(IMRG) .EQ. 2)THEN
                              VAL = DBLE(HRS * 0.001 * CO2_STDRN(EXRG,CURIYR) * 0.001)
!                             ROWCAR = 'CO2TNR' // URGNME(EXRG)(6:7); call makmsk(ROWCAR_mask,':CO2TNR:' , URGNME(IRG)(6:7))
!                             CALL DVAL(COL,ROWCAR,-VAL)
                              ROWCAR = 'CO2QNR' // URGNME(EXRG)(6:7); call makmsk(ROWCAR_mask,':CO2QNR:' , URGNME(EXRG)(6:7))
                              CALL DVAL(COL,ROWCAR,-VAL,COL_mask,ROWCAR_mask,'ED$TRADE,27')
!                             ROWCAR = 'CO2TNR' // URGNME(IMRG)(6:7); call makmsk(ROWCAR_mask,':CO2TNR:' , URGNME(IRG)(6:7))
!                             CALL DVAL(COL,ROWCAR,VAL)
                              ROWCAR = 'CO2QNR' // URGNME(IMRG)(6:7); call makmsk(ROWCAR_mask,':CO2QNR:' , URGNME(IMRG)(6:7))
                              CALL DVAL(COL,ROWCAR,VAL,COL_mask,ROWCAR_mask,'ED$TRADE,28')
                           END IF
!                          ALSO GENERATION ROWS TO ALLOCATE EXPORTS TO IMPORT REGION
                           VAL = DBLE(HRS * 0.001)
                           ROWCAR = 'GENQNR' // URGNME(EXRG)(6:7); call makmsk(ROWCAR_mask,':GENQNR:' , URGNME(EXRG)(6:7))
                           CALL DVAL(COL,ROWCAR,-VAL,COL_mask,ROWCAR_mask,'ED$TRADE,29')
                           ROWCAR = 'GENQNR' // URGNME(IMRG)(6:7); call makmsk(ROWCAR_mask,':GENQNR:' , URGNME(IMRG)(6:7))
                           CALL DVAL(COL,ROWCAR,VAL,COL_mask,ROWCAR_mask,'ED$TRADE,30')
                        END IF
                     END IF

                  ENDIF !CNSTRNTS_EFD
               ENDDO !EXRG
            ENDDO   !ISTP
         ENDDO      !IS
      ENDDO         !IMRG

!     set up economy Canadian supply steps and trade constraints

      DO IS = 1, EENSP
         DO ISTP = 1 , ELNVCT(IS)
            ISEG = ELSEG(ISTP,IS)
            GRP = ELGRP(ISTP,IS)
            SL = EFD_Slice_ID(GRP,ISEG)
            SLCOD = CHAR(64+SL)
            HRS = UTWDTH(ISEG,GRP)
            TST_RGN = 0
            DO CRG = 1, EFD_D_PROV - 3
               EXRG = MNUMNR + CRG
               DO CSTP = 1, EFD_D_CSS
                  ISW = 0
                  DO IMRG = 1, UNRGNS
                     TRANCST = PTHRESH1(CURIYR,EXRG,IMRG) + PTHRESH2(CURIYR,EXRG,IMRG) + BARRIER(CURIYR)
                     IF (CNSTRNTS_EFD(IS,CURIYR,IMRG,EXRG) .GT. 0.001) THEN

!                       Set up transmission constraint on first pass only

                        ROW_TR = 'TCN'//CHCOD(EXRG)//URGNME(IMRG)(6:7)//CHCOD(IS)//SLCOD; call makmsk(ROW_TR_mask,':TCN:',CHCOD(EXRG),URGNME(IMRG)(6:7),CHCOD(IS),SLCOD)
                        IF (TST_RGN(EXRG,IMRG) .EQ. 0) THEN
                           CALL DROWTYPE(ROW_TR,'L       ',ROW_TR_mask)
                           VAL = DBLE(CNSTRNTS_EFD(IS,CURIYR,IMRG,EXRG))
                           CALL DRHS(EFDRHS,ROW_TR,VAL,ROW_TR_mask,'ED$TRADE,31')
                           TST_RGN(EXRG,IMRG) = 1
                        END IF

                        IF (EFD_GW(ISEG,GRP,CSTP,CRG,CURIYR) .GT. 0.001) THEN
                           COL = 'TC'//CHCOD(EXRG)//CHCOD(CSTP)//URGNME(IMRG)(6:7)//CHCOD(IS)//SLCOD; call makmsk(COL_mask,':TC:',CHCOD(EXRG),CHCOD(CSTP),URGNME(IMRG)(6:7),CHCOD(IS),SLCOD)

!                          put in transmission constraint

                           CALL DVAL(COL,ROW_TR,1.0D0,COL_mask,ROW_TR_mask,'ED$TRADE,32')

                           IF (SR_TRAN_CREDIT .GT. 0.0) THEN
                              COL_SR = 'SC'//CHCOD(EXRG)//CHCOD(CSTP)//URGNME(IMRG)(6:7)//CHCOD(IS)//SLCOD; call makmsk(COL_SR_mask,':SC:',CHCOD(EXRG),CHCOD(CSTP),URGNME(IMRG)(6:7),CHCOD(IS),SLCOD)
                              CALL DVAL(COL_SR,ROW_TR,1.0D0,COL_SR_mask,ROW_TR_mask,'ED$TRADE,33')
                           END IF

!                          put in OBJ

                           COST = DBLE(((CAN_CST(CSTP,CRG,CURIYR)*CAN_CST_SCMULT(CURIYR))+TRANCST)*HRS*0.001)
                           CALL DVAL(COL,EFDOBJ,COST,COL_mask,EFDOBJ,'ED$TRADE,34')

                           IF (SR_TRAN_CREDIT .GT. 0.0) THEN
                              COST = DBLE(1.0 - SR_TRAN_CREDIT) * DBLE(((CAN_CST(CSTP,CRG,CURIYR)*CAN_CST_SCMULT(CURIYR))+TRANCST)*HRS*0.001)
                              CALL DVAL(COL_SR,EFDOBJ,COST,COL_SR_mask,EFDOBJ,'ED$TRADE,35')
                           END IF

!                          put in load row of importing region

                           ROW_LD = 'LD'//URGNME(IMRG)(1:4)//CHCOD(IS)//SLCOD; call makmsk(ROW_LD_mask,':LD:',URGNME(IMRG)(1:4),CHCOD(IS),SLCOD)
                           CALL DVAL(COL,ROW_LD,DBLE((1.0-LINELOSS)*HRS*0.1),COL_mask,ROW_LD_mask,'ED$TRADE,36')

                           IF (SR_TRAN_CREDIT .GT. 0.0) THEN
                              CALL DVAL(COL_SR,ROW_LD,DBLE((1.0-SR_TRAN_CREDIT)*(1.0-LINELOSS)*HRS*0.1),COL_SR_mask,ROW_LD_mask,'ED$TRADE,39')

                              ROW_SR = 'SR' // URGNME(IMRG)(1:4) // CHCOD(IS) // SLCOD; call makmsk(ROW_SR_mask,':SR:' , URGNME(IMRG)(1:4) , CHCOD(IS) , SLCOD)
                              CALL DVAL(COL_SR,ROW_SR,DBLE(SR_TRAN_CREDIT*(1.0-LINELOSS)),COL_SR_mask,ROW_SR_mask,'ED$TRADE,40')

                           END IF

!                          set up capacity balance row for amount available at each step

                           IF (ISW .EQ. 0)  THEN
                              ROWBAL = 'CNCP'//CHCOD(EXRG)//CHCOD(CSTP)//CHCOD(IS)//SLCOD; call makmsk(ROWBAL_mask,':CNCP:',CHCOD(EXRG),CHCOD(CSTP),CHCOD(IS),SLCOD)
                              CALL DROWTYPE(ROWBAL,'L       ',ROWBAL_mask)
                              VAL = DBLE(EFD_GW(ISEG,GRP,CSTP,CRG,CURIYR) * ECANSQZ(CRG,CURIYR) * CAN_QTY_SCMULT(CURIYR))
                              CALL DRHS(EFDRHS,ROWBAL,VAL,ROWBAL_mask,'ED$TRADE,41')

                              WRITE(18,9331) CURIYR+UHBSYR,CURITR,CRG,EXRG,IS,ISTP,GRP,ISEG,CSTP,ROWBAL,VAL, &
                                 EFD_GW(ISEG,GRP,CSTP,CRG,CURIYR), ECANSQZ(CRG,CURIYR),can_cst_scmult(curiyr),can_qty_scmult(curiyr)
 9331                         FORMAT(1X,"EFD_GW_IN_UEFD",9(":",I4),":",A8,5(":",F12.3))

                           ENDIF  !ISW
                           CALL DVAL(COL,ROWBAL,1.0D0,COL_mask,ROWBAL_mask,'ED$TRADE,42')

                           IF (SR_TRAN_CREDIT .GT. 0.0) THEN
                              CALL DVAL(COL_SR,ROWBAL,1.0D0,COL_SR_mask,ROWBAL_mask,'ED$TRADE,43')
                           ENDIF 
!
!                          set up steo benchmarking constraint
!
                           IF (BMNETIMP(CURIYR) .GT. 0.0) THEN
                             ROW = 'CANIMPUS'; call makmsk(ROW_mask,':CANIMPUS:')
                             CALL DVAL(COL,ROW,DBLE((1.0-LINELOSS)*HRS*0.001),COL_mask,ROW_mask,'ED$TRADE,44')
                           ENDIF

                        ENDIF  !EFD_GW
                        ISW = ISW + 1
                     ENDIF  !CNSTRNTS_EFD
                  ENDDO  !IMRG
               ENDDO  !CSTP
            ENDDO  !CRG
         ENDDO  !ISTP
      ENDDO  !IS

      RETURN
      END


!
!     This subroutine sets up the planned maintenance constraints

      SUBROUTINE ED$PM
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'elcntl'
      include'dispcrv'
      include'dsmdimen'
      include'dsmtoefd'
      include'ecpcntl'
      include'ecp_nuc'
      include'uefdout'

      INTEGER  IEFD,IS,IP,IGRP,IRET,IMO,IRG,N,ISTP,ISEG,SL,NRSTP,I_EFD_GRP,IECP
      INTEGER  ISL,GRP,SEG,STP,FSL,MAP_EFD_TO_ECP(EFD_D_CAP)
      REAL  SUMCAP(EFD_D_DSP,EFD_D_MSP,MNUMNR),SUMPM(EFD_D_DSP,MNUMNR),TOTPM
      REAL RELSH(10),RELCST(10),STPCAP,CST,TOTCAP2(EFD_D_MSP,MNUMNR)
      REAL*8 VAL, HRVAL,CAP,NEG1/-1.0D0/
      CHARACTER*5 NUM
      CHARACTER*1 REGCD(MNUMNR),FSLCD,CHCODX(10)
      CHARACTER*16 ROW,CBROW,COL,RELCOL,RELROW
      DATA RELSH/0.01,0.02,0.05,0.1,0.2,5*0.0/
      DATA RELCST/-0.01,-0.005,-0.001,-0.0005,-0.0001,5*0.0/

      efdsub='ED$PM'

!
      NRSTP = 5
      
      DO N=1,MNUMNR
        REGCD(N) = CHCOD(N)
      ENDDO
      
      CHCODX(1) = '1'
      CHCODX(2) = '2'
      CHCODX(3) = '3'
      CHCODX(4) = '4'
      CHCODX(5) = '5'
      CHCODX(6) = '6'
      CHCODX(7) = '7'
      CHCODX(8) = '8'
      CHCODX(9) = '9'
      CHCODX(10) = 'A'

      sumpm = 0.0
      sumcap = 0.0
      totcap2 = 0.0

      MAP_EFD_TO_ECP = 1
      DO IECP = 1 , ECP_D_CAP
         IEFD = UPEFDT(IECP)
         MAP_EFD_TO_ECP(IEFD) = IECP
      END DO

!     loop over all plant groups

      DO I_EFD_GRP = 1 , N_EFD_GRPS
         IRG = UG_EMM_RG(I_EFD_GRP)
         IP = UG_EFDt(I_EFD_GRP)
         IGRP = EFD_GRPS_F(I_EFD_GRP)
         IF (IP .LE. EFD_D_DSP) THEN
            IF (EPPOPM(IP) .EQ. 1)  THEN  ! baseload plant type, do maintenance by group
               totpm = 0.0
               WRITE(NUM,'(I5.5)') IGRP
               ROW = 'MB'//REGCD(IRG)//NUM; call makmsk(ROW_mask,':MB:',REGCD(IRG),NUM)
               CALL DROWTYPE(ROW,'G       ',ROW_mask)
               DO IS = 1, EENSP
                  cap = UG_CAP(IS,I_EFD_GRP) * 0.001
                  totpm = totpm + UG_PMR(I_EFD_GRP) * cap * EETIME(IS) * 0.001

!                 capacity balance row

                  CBROW = 'CB'//NUM//CHCODX(IS); call makmsk(CBROW_mask,':CB:',NUM,CHCODX(IS))
                  CALL DROWTYPE(CBROW,'L       ',CBROW_mask)
                  val = DBLE(UG_CAP(IS,I_EFD_GRP) * 0.001)
                  CALL DRHS(EFDRHS,CBROW,VAL,CBROW_mask,'ED$PM,1')

!                 column for planned maint decision

                  COL = 'MB'//NUM//CHCODX(IS); call makmsk(COL_mask,':MB:',NUM,CHCODX(IS))
                  CALL DBND(EFDBND,COL,0.0D0,VAL,COL_mask,'ED$PM,2')

!                 intersect with capacity balance row

                  CALL DVAL(COL,CBROW,1.0D0,COL_mask,CBROW_mask,'ED$PM,3')

!                 intersect with planned maintenance row

                  HRVAL = DBLE(EETIME(IS)/1000.0)
                  CALL DVAL(COL,ROW,HRVAL,COL_mask,ROW_mask,'ED$PM,4')
 
!                 put nominal cost in OBJ to prevent overscheduling

                  CALL DVAL(COL,EFDOBJ,0.001D0,COL_mask,EFDOBJ,'ED$PM,5')
               ENDDO
               val = MAX(0.0,DBLE(totpm)-0.0005)
               CALL DRHS(EFDRHS,ROW,VAL,ROW_mask,'ED$PM,6')

            ELSE IF (EPPOPM(IP) .EQ. 2)  THEN  !peak/cycling type, do maintenance by plt type
               DO IS = 1, EENSP
                  cap = UG_CAP(IS,I_EFD_GRP) * 0.001
                  sumcap(IP,IS,IRG) = sumcap(IP,IS,IRG) + cap
                  sumpm(IP,IRG) = sumpm(IP,IRG) + UG_PMR(I_EFD_GRP) * cap *  EETIME(IS) / 1000.0
                  totcap2(IS,IRG) = totcap2(IS,IRG) + cap
               ENDDO

            ENDIF !eppopm
         ENDIF ! < EFD_D_DSP
      ENDDO   ! N_EFD_GRPS

      DO IRG = 1 , UNRGNS
         DO IEFD = 1, EFD_D_DSP
            IF (EPPOPM(IEFD) .EQ. 2) THEN
               ROW = 'MP'//REGCD(IRG)//EPPLCD(IEFD)//'XX'; call makmsk(ROW_mask,':MP:',REGCD(IRG),EPPLCD(IEFD),':XX:')
               CALL DROWTYPE(ROW,'G       ',ROW_mask)
               val = DBLE(sumpm(IEFD,IRG))
               if (SUMCAP(IEFD,1,IRG) .LT. 0.00001) val = DBLE(0.0)
               CALL DRHS(EFDRHS,ROW,VAL,ROW_mask,'ED$PM,7')

               IECP = MAP_EFD_TO_ECP(IEFD)

               IF (SR_CREDIT(IECP) .EQ. 0.0) THEN
                  DO IS = 1, EENSP

!                    column for planned maint decision

                     COL = 'MP'//REGCD(IRG)//EPPLCD(IEFD)//CHCODX(IS)//'XX'; call makmsk(COL_mask,':MP:',REGCD(IRG),EPPLCD(IEFD),CHCODX(IS),':XX:')
                     val = DBLE(sumcap(IEFD,IS,IRG))
                     CALL DBND(EFDBND,COL,0.0D0,VAL,COL_mask,'ED$PM,8')

!                    intersect with planned maintenance row

                     HRVAL = DBLE(EETIME(IS)/1000.0)
                     CALL DVAL(COL,ROW,HRVAL,COL_mask,ROW_mask,'ED$PM,9')

!                    set up capacity balance row by slice

                     DO ISL = 1 , ELNVCT(IS)
                        GRP = ELGRP(ISL,IS)
                        SEG = ELSEG(ISL,IS)
                        FSL = EFD_Slice_ID(GRP,SEG)
                        FSLCD = CHAR(64+FSL)
                        CBROW = 'CP'//REGCD(IRG)//EPPLCD(IEFD)//CHCODX(IS)//FSLCD; call makmsk(CBROW_mask,':CP:',REGCD(IRG),EPPLCD(IEFD),CHCODX(IS),FSLCD)
                        CALL DROWTYPE(CBROW,'L       ',CBROW_mask)
                        val = DBLE(sumcap(IEFD,IS,IRG))
                        CALL DRHS(EFDRHS,CBROW,VAL,CBROW_mask,'ED$PM,10')

!                       intersect with capacity balance row

                        CALL DVAL(COL,CBROW,1.0D0,COL_mask,CBROW_mask,'ED$PM,11')

                     ENDDO
                  ENDDO  ! IS
               END IF

!              put nominal cost in OBJ to prevent overscheduling

               CALL DVAL(COL,EFDOBJ,0.001D0,COL_mask,EFDOBJ,'ED$PM,12')
            ENDIF
         ENDDO

!        set up reliability capacity steps constraint
      ENDDO

      RETURN
      END

!
!     This subroutine sets up the load rows, sets up RPS if necessary
!
      SUBROUTINE ED$LOAD(IRG)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispcrv'
      include'ecpcntl'
      include'uecpout'
      include'dispett'
      include'bildin'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'dispuse'
      include'dispout'
      include'wrenew'
      include'ecp_nuc'

      INTEGER IS,ISL,GRP,SEG,STP,FSL,IRET,IRG,TEST_GRP_PV(MAXECPB),TEST_GRP_WN(MAXECPB)
      REAL LOAD,RNWGEN2,VALUE,MAX_HGHT,SP_RES_RQMT_IN(ELD_D_DAY,EFD_D_MVS)
      REAL*8 VAL,escst,TGEN
      CHARACTER*1 FSLCD
      CHARACTER*16 ROW,COL,COLES,COLUMN,COL_SR,ROW_SR, ROW_LOAD

      INTEGER*4 m, d, h, h_old

      REAL IMPANN,EXPANN,INTRUP, TOTHRS

      efdsub='ED$LOAD'

      TGEN = 0.0
      DO IS = 1, EENSP
        DO ISL = 1 , ELNVCT(IS)
          GRP = ELGRP(ISL,IS)
          SEG = ELSEG(ISL,IS)
          FSL = EFD_Slice_ID(GRP,SEG)
          FSLCD = CHAR(64+FSL)
          LOAD = UTHGHT(SEG,GRP,IRG)
          ROW = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(ROW_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)
          VAL = DBLE(LOAD * UTWDTH(SEG,GRP))
          TGEN = TGEN + ULHGHT(ISL,IS,IRG) * UTWDTH(SEG,GRP)
          if (EFDMAT .EQ. 0) THEN
            CALL DROWTYPE(ROW,'G       ',ROW_mask)
          endif
          CALL DRHS(EFDRHS,ROW,DBLE(0.1 * VAL),ROW_mask,'ED$LOAD,1')

          write(18,1212) CURIRUN, CURIYR+1989, CURITR, IRG, IS, ISL, GRP, SEG, STP, FSLCD, ULHGHT(ISL,IS,IRG), &
             UTHGHT(SEG,GRP,IRG), UTWDTH(SEG,GRP), LOAD, VAL
 1212     format(1x,"ED_LOAD",9(":",I4),":",A4,5(":",F10.3))

!  add escape plant to eliminate infeasibilities, mainly in early years
          COL = 'ES'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(COL_mask,':ES:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)
          CALL DBND(EFDBND,COL,0.0D0,99.9D0,COL_mask,'ED$LOAD,2')
          CALL DVAL(COL,ROW,DBLE(UTWDTH(SEG,GRP) * 0.1),COL_mask,ROW_mask,'ED$LOAD,3')
          escst = 999.99 * UTWDTH(SEG,GRP) * 0.001
          CALL DVAL(COL,EFDOBJ,DBLE(escst),COL_mask,EFDOBJ,'ED$LOAD,4')
!  create vector for generation from trad cogen
          COL = 'G'//UPRGCD(IRG)//'CGTRAD'; call makmsk(COL_mask,':G:',UPRGCD(IRG),':CGTRAD:')
          VALUE = BTCOGEN(IRG) / 8.76
          CALL DBND(EFDBND,COL,DBLE(VALUE),DBLE(VALUE),COL_mask,'ED$LOAD,5')
          CALL DVAL(COL,ROW,DBLE(UTWDTH(SEG,GRP) * 0.1),COL_mask,ROW_mask,'ED$LOAD,6')
        ENDDO
      ENDDO

      DO IS = 1, EENSP
         MAX_HGHT = 0.0
         DO ISL = 1 , ELNVCT(IS)
            GRP = ELGRP(ISL,IS)
            SEG = ELSEG(ISL,IS)
            FSL = EFD_Slice_ID(GRP,SEG)
            FSLCD = CHAR(64+FSL)
            LOAD = UTHGHT(SEG,GRP,IRG)
            MAX_HGHT = MAX(MAX_HGHT , LOAD)
         END DO

         DO ISL = 1 , ELNVCT(IS)
            GRP = ELGRP(ISL,IS)
            SEG = ELSEG(ISL,IS)
            FSL = EFD_Slice_ID(GRP,SEG)
            FSLCD = CHAR(64+FSL)
            LOAD = UTHGHT(SEG,GRP,IRG)
            ROW_SR = 'SR' // URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(ROW_SR_mask,':SR:' , URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)

!           CALCULATE SPINNING RESERVE REQUIREMENT

            SP_RES_RQMT_IN(GRP,SEG) = SR_RQMT_HGHT(IRG) * LOAD + SR_RQMT_DIFF(IRG) * (MAX_HGHT - LOAD)

            if (EFDMAT .EQ. 0) THEN
              CALL DROWTYPE(ROW_SR,'G       ',ROW_SR_mask)
            endif

!           REVISE RHS OF SPINNING RESERVE REQUIREMENT ROW

            VAL = DBLE(0.0)
            CALL DRHS(EFDRHS,ROW_SR,VAL,ROW_SR_mask,'ED$LOAD,7')

            COL_SR = 'SP' // URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(COL_SR_mask,':SP:' , URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)

            VAL = SP_RES_RQMT_IN(GRP,SEG)
            CALL DBND(EFDBND,COL_SR,VAL,DBLE(30000.0),COL_SR_mask,'ED$LOAD,8')

            VAL = DBLE(-1.0)
            CALL DVAL(COL_SR,ROW_SR,VAL,COL_SR_mask,ROW_SR_mask,'ED$LOAD,9')

            ROW = EFDOBJ;ROW_mask=ROW
            VAL = DBLE(-0.001)
            CALL DVAL(COL_SR,ROW,VAL,COL_SR_mask,ROW_mask,'ED$LOAD,10')

         END DO
      END DO

      RETURN
      END
!
!     This subroutine sets up the national RPS if necessary
!
      SUBROUTINE ED$RPS(IRG)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'qblk'
      include'emmparm'
      include'control'
      include'dispin'
!     include'dispcrv'
      include'ecpcntl'
      include'udatout'
      include'uefdout'
      include'uecpout'
!     include'dispett'
      include'bildin'
!     include'dsmdimen'
!     include'dsmtoefd'
!     include'dispuse'
      include'cdsparms'
      include'emission'
      include'csapr'
      include'emmemis'

      INTEGER IS,ISL,GRP,SEG,STP,FSL,IRET,IRG,PLT
      REAL LOAD,VALUE
      REAL*8 VAL,escst,TGEN
      CHARACTER*1 FSLCD
      CHARACTER*16 ROW,ROWUS,ROWN,COL,COLES,COLUMN
!
      efdsub='ED$RPS'

      IF (IRG .EQ. 1 ) THEN
         ROWUS = 'RPSREQU' ;ROWUS_MASK=ROW
         COLUMN = 'RPSGENU' ;COLUMN_MASK=COLUMN
         VALUE = (QELAS(MNUMCR,CURIYR) / 3.412) - (QELASN(MNUMNR - 1,CURIYR) + QELASN(MNUMNR - 2,CURIYR)) * 0.001
!        if (curitr .gt. 0)write(6,3434) curiyr+1989,curitr,uprnwbnd(curiyr),  qelas(mnumcr,curiyr) / 3.412, &
!         QELASN(MNUMNR,CURIYR) * 0.001, QELASN(MNUMNR - 1,CURIYR) * 0.001, QELASN(MNUMNR - 2,CURIYR) * 0.001,  &
!        (QELASN(MNUMNR,CURIYR) - QELASN(MNUMNR - 1,CURIYR) - QELASN(MNUMNR - 2,CURIYR)) * 0.001,  &
!        UPRNWBND(CURIYR) * ((QELAS(MNUMCR,CURIYR) / 3.412) - (QELASN(MNUMNR - 1,CURIYR) - QELASN(MNUMNR - 2,CURIYR)) * 0.001)
!3434 format(1h ,'!qel',i4,i3,f6.3,6f10.1)
         CALL DBND(EFDBND,COLUMN,DBLE(VALUE),DBLE(VALUE),COLUMN_mask,'ED$RPS,1')
         IF (EFDMAT .EQ. 0)THEN
            CALL DROWTYPE(ROWUS,'G       ',ROWUS_mask)
!           CALL DROWTYPE(ROWUS,'N       ',ROWUS_mask)
            CALL DVAL(COLUMN,ROWUS,DBLE(-UPRNWBND(CURIYR)),COLUMN_mask,ROWUS_mask,'ED$RPS,2')
            CALL DVAL(COLUMN,EFDOBJ,EFD_MIN,COLUMN_mask,EFDOBJ,'ED$RPS,3')
            COL = 'RPSBNKU' ; COL_Mask=COL
            VALUE = (UCRBKNR(MNUMNR,CURIYR - 1) - UCRBKNR(MNUMNR,CURIYR))
            CALL DVAL(COL,ROWUS,DBLE(1.0),COL_mask,ROWUS_mask,'ED$RPS,4')
            CALL DBND(EFDBND,COL,DBLE(VALUE),DBLE(VALUE),COL_mask,'ED$RPS,5')
!r          ROWN = 'RPSNQRU'
!r          IRET = CALL DROWTYPE(ROWN,'L       ',ROWN_mask)
!r          COL = 'RPSNQGU'
!r          CALL DVAL(COL,ROWN,DBLE(-1.0))
!r          CALL DVAL(COL,EFDOBJ,MAX(DBLE(EPRPSPR(CURIYR)),EFD_MIN))
!r          CALL DBND(EFDBND,COL,DBLE(0.0),DBLE(UPRNWNR(MNUMNR,CURIYR)))
         END IF
      END IF
      ROW = 'RPSREQ' // UPRGCD(IRG); call makmsk(ROW_mask,':RPSREQ:' , UPRGCD(IRG))
      IF (EFDMAT .EQ. 0)THEN
         CALL DROWTYPE(ROW,'G       ',ROW_mask)
         COLUMN = 'RPSGEN' // UPRGCD(IRG); call makmsk(COLUMN_mask,':RPSGEN:' , UPRGCD(IRG))
         CALL DVAL(COLUMN,ROW,DBLE(-1.0),COLUMN_mask,ROW_mask,'ED$RPS,6')
         CALL DVAL(COLUMN,ROWUS,DBLE(1.0),COLUMN_mask,ROWUS_mask,'ED$RPS,7')
!r       ROWN = 'RPSNQR' // UPRGCD(IRG); call makmsk(ROWN_mask,':RPSNQR:' , UPRGCD(IRG))
!r       CALL DROWTYPE(ROWN,'L       ',ROWN_mask)
!r       CALL DROWTYPE(ROWN,'N       ',ROWN_mask)
!r       COL = 'RPSNQG'// UPRGCD(IRG)
!r       CALL DVAL(COL,ROWN,DBLE(-1.0))
!r       ROWN = 'RPSNQRU'
!r       CALL DVAL(COL,ROWN,DBLE(1.0))
         DO PLT = 1 , ECP_D_CAP
!     write(6,2345) curiyr,irg,uplntcd(plt),expgen(plt,irg),exsgen(plt,irg)
!2345 format(1h ,'!exscap',i4,i3,a3,3f10.3)
!p          ROWN = 'RPSPGN' // UPLNTCD(PLT); call makmsk(ROWN_mask,':RPSNQR:' , UPLNTCD(PLT))
!p          CALL DROWTYPE(ROWN,'N       ',ROWN_mask)
!        EXCLUDE EXISTING CAPACITY, IF APPROPRIATE
!n          ROWN = 'RPSNQP' // UPLNTCD(PLT); call makmsk(ROWN_mask,':RPSNQP:' , UPLNTCD(PLT))
!n          CALL DROWTYPE(ROWN,'N       ',ROWN_mask)
            IF (UPRNWEXG(PLT) .GT. 0.0)THEN
               COL = 'RPSX' // UPLNTCD(PLT) // UPRGCD(IRG); call makmsk(COL_mask,':RPSX:' , UPLNTCD(PLT) , UPRGCD(IRG))
               IF (PLT .LE. ECP_D_DSP)THEN
                  VAL = EXPGEN(PLT,IRG) * UPRNWEXG(PLT) * UPRNWSHR(PLT)
               ELSE
                  VAL = EXSGEN(PLT,IRG) * UPRNWEXG(PLT) * UPRNWSHR(PLT)
               END IF
               CALL DBND(EFDBND,COL,VAL,VAL,COL_mask,'ED$RPS,8')
               CALL DVAL(COL,ROW,DBLE(-1.0),COL_mask,ROW_mask,'ED$RPS,9')
!n             CALL DVAL(COL,ROWN,DBLE(1.0))
!n             ROWN = 'RPSNQR' // UPRGCD(IRG)
!n             CALL DVAL(COL,ROWN,DBLE(1.0))
            END IF
         END DO
!p          ROWN = 'RPSPGNCF'
!p          CALL DROWTYPE(ROWN,'N       ',ROWN)
      END IF
!     ACCOUNT FOR RENEWABLES FROM TRADITIONAL COGEN
      IF (UPRNWCOG .GT. 0)THEN
         COLUMN = 'RPSCOG' // UPRGCD(IRG); call makmsk(COLUMN_mask,':RPSCOG:' , UPRGCD(IRG))
         IF (EFDMAT .EQ. 0)THEN
            CALL DVAL(COLUMN,ROW,DBLE(1.0),COLUMN_mask,ROW_mask,'ED$RPS,10')
         END IF
         IF (UPRNWCOG .EQ. 1) THEN
            VALUE = EDRPSRG
         ELSE
            VALUE = EDRPSRG + EDRPSRO
         END IF
         CALL DBND(EFDBND,COLUMN,DBLE(VALUE),DBLE(VALUE),COLUMN_mask,'ED$RPS,11')
      END IF

      RETURN
      END
!
!     This subroutine sets up the operate vectors
!
      SUBROUTINE ED$OP
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'elcntl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'ecp_nuc'
      include'uefdout'
      include'emission'
      include'cdsparms'
      include'csapr'
      include'emmemis'

      INTEGER ITYP,IRG,N,IP,IPGRP,IS,IRET,IVSL,GRP,SEG,STP,IECP,ISP,JRG,FSL
      REAL*8 PS_TEST(MNUMNR),TGEN(ECP_D_CAP,MNUMNR),MX_MRUN,VALUE,VAL
      REAL*8 P2_TEST(MNUMNR),MAX_CAP,NEG1/-1.0D0/
      CHARACTER*16 ROWSTOR,COL,LOAD,ROW,P2ROW
      CHARACTER*1 FSLCD
      CHARACTER*5 NUM

      efdsub='ED$OP'

      TGEN = 0.0
      PS_TEST = 0.0
      P2_TEST = 0.0

!   loop over all EFD Groups

      DO N = 1, N_EFD_GRPS
       IRG = UG_EMM_RG(N)
       IP = UG_EFDt(N)
!
!      ACCUMULATE TOTAL GENERATION BY ECP TYPE FOR MUST RUN CONSTRAINT SAFETY CALCULATION
!
       IECP = UG_ECPt(N)
       IF (UG_MRUN(N) .GT. 0) THEN
          TGEN(IECP,IRG) = TGEN(IECP,IRG) + UG_GEN(0,N)
       END IF
!      IF (IP .EQ. UIHYR) PS_TEST(IRG) = PS_TEST(IRG) + UG_CAP(0,N)
       IF (IP .EQ. UIHYR) THEN
           IF (IECP .EQ. WIP2) THEN
               P2_TEST(IRG) = P2_TEST(IRG) + UG_CAP(0,N)
           ELSE
               PS_TEST(IRG) = PS_TEST(IRG) + UG_CAP(0,N)
           END IF
       ENDIF
      END DO

!   loop over all EFD Groups

      JRG = 0
      DO N = 1, N_EFD_GRPS
       IRG = UG_EMM_RG(N)
       IF (IRG .NE. JRG) THEN
         CALL GETIN(1,IRG)

!        WRITE(6,3977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3977    FORMAT(1X,"UEFD_07927_EEITAJ_GET",4(":",I4),3(":",F12.3))

         CALL GETBLD(1,IRG)
         JRG = IRG
       END IF
       IP = UG_EFDt(N)
!
       IF (EPPOPR(IP) .EQ. 1) THEN
         CALL OPBSLD(N)
       ELSEIF ( EPPOPR(IP) .EQ. 2) THEN
         CALL OPPEAK(N)
       ELSEIF (EPPOPR(IP) .EQ. 3) THEN
         CALL OPRNBS(N)
       ELSEIF (EPPOPR(IP) .EQ. 4) THEN
         CALL OPHYD(N)
       ELSEIF (EPPOPR(IP) .EQ. 5) THEN
         CALL OPINT(N)
       ELSEIF (EPPOPR(IP) .EQ. 6) THEN
         CALL OPDG(N)
       ELSEIF (EPPOPR(IP) .EQ. 7) THEN
         CALL OPSTR(N)
       ELSE
         write(6,*) 'error in operating type ED_OP',N,IP,EPPOPR(IP),IRG,UG_ECPt(N)
       ENDIF
      ENDDO
!
!     CREATE ROW TO FORCE MUSTRUN REQUIREMENTS
!
!     DO IECP = 1 , ECP_D_DSP
!        DO IRG = 1 , UNRGNS
!           IF (MUSTRUN(IECP,IRG) .GT. 0.0) THEN
!              ROW = 'MR' // URGNME(IRG)(1:4) // UPLNTCD(IECP) ; call makmsk(ROW_mask,':MR:',URGNME(IRG)(1:4),UPLNTCD(IECP))
!              IF (EFDMAT .EQ. 0) CALL DROWTYPE(ROW,'G       ',ROW_MASK)
!              MX_MRUN = 0.99 * TGEN(IECP,IRG)
!              VALUE = MIN(MX_MRUN , MUSTRUN(IECP,IRG))
!              IF (CURIYR .LE. 12 .AND. FCRL .EQ. 1) THEN
!                 WRITE(18,3211) CURIYR+UHBSYR,CURITR,IRG,IECP,UPLNTCD(IECP),MX_MRUN,MUSTRUN(IECP,IRG)
!3211             FORMAT(1X,"MUSTRUN",4(":",I5),":",A2,":",2(":",F20.6))
!              END IF
!              CALL DRHS(EFDRHS,ROW,VALUE*0.1)
!           END IF
!        END DO
!     END DO

      DO N = 1, N_EFD_GRPS
       IECP = UG_ECPt(N)
         IF (UG_MRUN(N) .GT. 0 .AND. IECP .LE. (ECP_D_DSP + ECP_D_RNW)) THEN
            IPGRP = EFD_GRPS_F(N)
            WRITE(NUM,'(I5.5)') IPGRP
            ROW = 'MR'//NUM//'X'; call makmsk(ROW_mask,':MR:',NUM,':X:')
            IF (EFDMAT .EQ. 0) CALL DROWTYPE(ROW,'G       ',ROW_mask)
            VALUE = 0.0
            DO WHILE (IPGRP .GT. 0)
               DO IS = 1, EENSP
                 VALUE = VALUE + ECMRUNCF(IPGRP) * EFACTR * UP_CAP(IS,IPGRP) * EETIME(IS) * 0.001
               ENDDO
               IPGRP = EFD_GRPS_N(IPGRP)
         END DO
            VALUE = 0.99 * VALUE
            CALL DRHS(EFDRHS,ROW,VALUE*0.1,ROW_mask,'ED$OP,1')
         ENDIF
      END DO
!

!     lf Pump Storage Units are in this region set up rows to consume electricity in off-peak slices

      DO IRG = 1 , UNRGNS
         IF (PS_TEST(IRG) .GT. 0.0) THEN
            DO IS = 1, EENSP
               ROWSTOR = 'PS_' // URGNME(IRG)(1:4) // CHCOD(IS); call makmsk(ROWSTOR_mask,':PS_:' , URGNME(IRG)(1:4) , CHCOD(IS))
               CALL DROWTYPE(ROWSTOR,'L       ',ROWSTOR_mask)
               DO IVSL = 1 , ELNVCT(IS)
                  GRP = ELGRP(IVSL,IS)
                  SEG = ELSEG(IVSL,IS)
                  FSL = EFD_Slice_ID(GRP,SEG)
                  FSLCD = CHAR(64+FSL)
                  COL = 'ST'// URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(COL_mask,':ST:', URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)
                  CALL DBND(EFDBND,COL,0.0D0,DBLE(PS_TEST(IRG) * 0.001 * ELWDTH(IVSL,IS) * 0.1),col_mask,'ED$OP,2')
                  CALL DVAL(COL,ROWSTOR,NEG1,COL_mask,ROWSTOR_mask,'ED$OP,3')
                  LOAD = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(LOAD_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)
                  CALL DVAL(COL,LOAD,NEG1,COL_mask,LOAD_mask,'ED$OP,4')

               END DO
            END DO
         END IF
         IF (P2_TEST(IRG) .GT. 0.0) THEN
          DO IS = 1, EENSP
             ROWSTOR = 'P2_' // URGNME(IRG)(1:4) // CHCOD(IS); call makmsk(ROWSTOR_mask,':P2_:' , URGNME(IRG)(1:4) , CHCOD(IS))
             CALL DROWTYPE(ROWSTOR,'L       ',ROWSTOR_mask)
               DO IVSL = 1 , ELNVCT(IS)
                  GRP = ELGRP(IVSL,IS)
                  SEG = ELSEG(IVSL,IS)
                  FSL = EFD_Slice_ID(GRP,SEG)
                  FSLCD = CHAR(64+FSL)
                  COL = 'S2'// URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(COL_mask,':S2:', URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)
                  CALL DBND(EFDBND,COL,DBLE(0.0),DBLE(P2_TEST(IRG) * 0.001 * ELWDTH(IVSL,IS) * 0.1),col_mask,'ED$OP,7')
                  CALL DVAL(COL,ROWSTOR,DBLE(-1.0),COL_mask,ROWSTOR_mask,'ED$OP,8')
                  LOAD = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(LOAD_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)
                  CALL DVAL(COL,LOAD,DBLE(-1.0),COL_mask,LOAD_mask,'ED$OP,9')

                  P2ROW = 'P2' // FSLCD // URGNME(IRG)(1:4) // CHCOD(IS); call makmsk(P2ROW_mask,':P2:' , FSLCD , URGNME(IRG)(1:4) , CHCOD(IS))
                  CALL DROWTYPE(P2ROW,'L       ',P2ROW_mask)
                  CALL DVAL(COL,P2ROW,DBLE(1.0),COL_mask,P2ROW_mask,'ED$OP,10')
                  CALL DRHS(EFDRHS,P2ROW,DBLE(P2_TEST(IRG) * 0.001 * ELWDTH(IVSL,IS) * 0.1),P2ROW_mask,'ED$OP,11')

               END DO
         END DO
         END IF
      END DO

      RETURN
      END

      SUBROUTINE OPBSLD(N)
      use efd_row_col
      USE EPHRTS_SWTICHES
      USE EPHRTS_FILE_UNIT_NUMBERS 

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'fuelin'
      include'plntctl'
      include'ecpcntl'
      include'elcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'emission'
      include'uecpout'
      include'dispett'
      include'dispuse'
      include'cdsparms'
      include'uso2grp'
      include'eusprc'
      include'edbdef'
      include'emeblk'
      include'dispcrv'
      include'dispinyr'
      include'ecp_nuc'
      include'csapr'
      include'emmemis'
      include'emm_aimms'

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IFR,IST,STA,CRG,IGS,JVLS,CAR,IGR,I_NOCCS,I_NOCCS_EFD,IEFD,ICS
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,INOX,FSL
      INTEGER*4  CO2_PLTET
      INTEGER*4  RUN45Q
      REAL CF(EFD_D_MVS+1,EFD_D_MSP),VOM,HTRT,CAP(EFD_D_MSP),HRFAC,MAXCF(EFD_D_MSP),PTC,RPSCST,RPSR,RPSU,CFSH,GPSCST, GPSCST_MIN, GPSCST_MAX
      REAL ALT_GPSCST, ALT_GPSCST_MIN, ALT_GPSCST_MAX
      REAL*8 DRAT,VAL,VALCF,GEN,OBJVAL,BTUREQ,NOXEM,GENBMS,VAL_SR,DRAT_MIN,DRAT_MAX,MAX_SP_LOAD,MIN_SP_LOAD,GEN_MIN,GEN_MAX,BTUREQ_MIN,BTUREQ_MAX,DRAT_ORG,SP_HOURS
      CHARACTER*1 FSLCD,NOXCODE
      CHARACTER*1 ALT_SP(EFD_D_MSP)
      CHARACTER*5 NUM
      CHARACTER*16 COL,ROW,LOAD,ROW_NOX,ROWRPS,ROW_MR,ROWBMS,ROWCARR,ROW_GPS,ROW_GPSN,ROW_GRD
      CHARACTER*16 ROW_EOR_CO2,ALT_ROW_EOR_CO2,ROW_SR, COL_MIN_SR, COL_MAX_SR, ROW_ALT
      CHARACTER*16 COL_ALT, COL_MIN_SR_ALT, COL_MAX_SR_ALT
      REAL*8 VAL_CAP_CO2, VAL_MIN_SR, VAL_MAX_SR, VALCF_MIN, VALCF_MAX,VALSH, ADJ_FAC, ALT_VAL, ALT_CO2LB, ALT_CO2LB_MIN, ALT_CO2LB_MAX, SHR_CCS, SHR_NOCCS, CO2_NOCCS, CO2_CCS, CO2_CCS_MIN
      REAL GSPR,OLPR,CLSH,GASSH,OLSH,TOT,AVGCAR,CLCAR,OGCAR,WDCAR,ALT_CLCAR,ALT_OGCAR,ALT_WDCAR,CO2LB,CO2ADJ, CO2LB_MIN, CO2LB_MAX
      INTEGER*4  FUEL_RGN, ECPt, INUC, IFOSBS
      REAL*8 Load_Level, HTRT_ADJ, HTRT_ADJ_MIN, HTRT_ADJ_MAX, Target_EFF, Max_EFF
      CHARACTER*12 FROM_LABEL
      character*30 ROW_GRD_mask
      LOGICAL E_DEBUG_EXIST
      CHARACTER*2 CNSCOD
!
!      COMMON /GRDSRC/ GRD_CASN,GRD_SRCN,GRD_SRCC
!      INTEGER GRD_CASN                                       ! Number of grid resilience sources 
!      CHARACTER*15 GRD_SRCN(MX_GRDSRC)                       ! Grid resilience source names
!      CHARACTER*1  GRD_SRCC(MX_GRDSRC)                       ! Grid resilience source codes
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
         INQUIRE(FILE="EPHRTS_DEBUG_FILE.TXT", EXIST=E_DEBUG_EXIST)
         IF (E_DEBUG_EXIST) THEN
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="OLD", POSITION="APPEND", ACTION="WRITE")
         ELSE
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="NEW", ACTION="WRITE")
         END IF
      END IF
      efdsub='OPBSLD'

      ALT_SP(1) = "X"
      ALT_SP(2) = "Y"
      ALT_SP(3) = "Z"
      ALT_SP(4) = "W"

      RUN45Q=RTOVALUE('RUN45Q  ',0)

      RPSCST = 0.0
      
      INUC=0
      IFOSBS=0

!     calculate CF at each load slice

      CF = 0.0

      DO IS = 1, EENSP
         DO IVSL = 2 , ELNVCT(IS) + 1
            hrfac = ELWDTH(IVSL-1,IS)/EETIME(IS)
            CF(IVSL,IS) = CF(IVSL-1,IS) + hrfac
         ENDDO
      ENDDO

      IRG = UG_EMM_RG(N)
      IP = UG_EFDt(N)
      IECP = UG_ECPt(N)
      IGRP = EFD_GRPS_F(N)
      TFR = UG_FL_RG(N)
      IST = UG_STATE(N)
      ICS = EPCSMP(TFR)
      WRITE(CNSCOD,'("0",I1)') ICS

      IF ((CURIYR + UHBSYR) .GE. UPSTYR) THEN   ! if ECP/RESTORE has run, set flags to use RESTORE CF
       IF (StorageCodes(UCPVSTOR(IECP)) .EQ. 'NC') THEN
            INUC = 1
       ELSEIF (IECP .LE. ECP_D_DSP) THEN
          IF (UPPCFB(IECP,1) .GT. 0.001 .AND. UPAVLYR(IECP) .LT. 9000) THEN
            IFOSBS = 1
          ENDIF
       ENDIF
      ENDIF
       
!     if (curitr .eq. 1)then
!        if (ist .le. 0)then
!           write(6,2345) curiyr+1989,n,irg,urgnme(irg),ist,epclmp(tfr),tfr,uplntcd(iecp),ug_cap(1,n)
!2345       format(1h ,'!nostgrp',i4,i6,i3,a3,i3,i3,i3,a3,f10.1)
!        else
!           if (curiyr .le. 26)write(6,2346) curiyr+1989,n,irg,urgnme(irg),ist,epclmp(tfr),tfr,uplntcd(iecp),ug_cap(1,n)
!2346       format(1h ,'!ysstgrp',i4,i6,i3,a3,i3,i3,i3,a3,f10.1)
!        end if
!     end if

      CRG = EPCLMP(TFR)
      VOM = UG_OMR(N)

!     ADD DSI VOM, IF APPROPRIATE
!
!     IF ((CURIYR + UHBSYR) .GE. UDSI_YR .AND. IECP .LT. WIPC)THEN
!        IF (ECP_SCRUB(IECP,CURIYR) .EQ. 2)THEN
!           IF (UPLNTCD(IECP)(1:1) .EQ. 'B')THEN
!              VOM = VOM + UPDSIVOM(1)
!           ELSE
!              VOM = VOM + UPDSIVOM(2)
!           END IF
!        END IF
!     END IF

      PTC = UG_GSUB(N)

!     STORE 111d SWITCH FOR MASS OR RATE STANDARDS FOR REGION

      CO2_PLTSW(IECP) = CO2_PLTRG(IECP,IRG)

!     STORE 111d SWITCH FOR ET

      IF (IECP .EQ. WIET .OR. IECP .EQ. WICT .OR. IECP .EQ. WIAT)CO2_PLTET = CO2_PLTSW(IECP)

      DO IS = 1, EENSP
         CAP(IS) = UG_CAP(IS,N)
         MAXCF(IS) = UG_SCF(IS,N)
         HTRT = UG_HTRT(IS,N)

         IF (FCRL .EQ. 1 .AND. CURIYR .EQ. 28) THEN
            write(UF_DBG,3263) CURIYR,CURITR,IS,N,IGRP,IP,TFR,VOM,PTC,HTRT,CAP(IS),MAXCF(IS)
 3263       format(1x,'OPBSLD ',7(":",I6),5(":",F10.2))
         ENDIF
      ENDDO

      IGS = 0
      DO IFL = 1, EFD_D_FPP
         if (WFLTP(IP,IFL) .NE. 0) THEN
            IF (UIGAS(WFLTP(IP,IFL)) .EQ. 1) IGS = 1
         endif
      ENDDO

      WRITE(NUM,'(I5.5)') IGRP

!     loop over time season and slices to determine modes

      DO IS = 1, EENSP

         HTRT = UG_HTRT(IS,N)
         SP_HOURS = 0.0
         MAX_SP_LOAD = 0.0
         MIN_SP_LOAD = 500000.0
         DO IVSL = 1 , ELNVCT(IS)
            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)
            MAX_SP_LOAD = MAX(MAX_SP_LOAD , UTHGHT(SEG,GRP,IRG))
            MIN_SP_LOAD = MIN(MIN_SP_LOAD , UTHGHT(SEG,GRP,IRG))
            SP_HOURS = SP_HOURS + ELWDTH(IVSL,IS)
         END DO

         ADJ_FAC = 1.0
         SHR_NOCCS = 0.0
         SHR_CCS = 1.0
         IF (UPPCEF(IECP) .GT. UPPCEF_MIN(IECP) .AND. RUN45Q .GT. 0) THEN 

              IF (UGNOCCS(N) .NE. 1) ADJ_FAC =  ALT_UECP_CPEN_ADJ(IECP) / (1.0 - UGNOCCS(N))

              I_NOCCS = NO_CCS_PLNT_NDX(IECP)
              I_NOCCS_EFD = UPEFDT(I_NOCCS)
              CO2_NOCCS = 1.0 - UPPCEF(I_NOCCS)
              CO2_CCS = 1.0 - UPPCEF(IECP)
              CO2_CCS_MIN = (1.0 - UPPCEF_MIN(IECP))
              SHR_NOCCS = (CO2_CCS_MIN - CO2_CCS) / (CO2_NOCCS - CO2_CCS)
              SHR_CCS = 1.0 - SHR_NOCCS
  
              WRITE(18,6317) CURIRUN, CURIYR+1989, N, IS, IRG, IP, IECP, I_NOCCS, IP, I_NOCCS_EFD, IGRP, TFR, IST, UPLNTCD(IECP), UPLNTCD(I_NOCCS), EPPLCD(IP), EPPLCD(I_NOCCS_EFD), &
                 UPPCEF(IECP), UPPCEF_MIN(IECP), ALT_UECP_CPEN_ADJ(IECP), UGNOCCS(N), UG_CAP(IS,N), ADJ_FAC, CO2_NOCCS, CO2_CCS, CO2_CCS_MIN, SHR_NOCCS, SHR_CCS
 6317         FORMAT(1X,"ADJ_FAC_OP_INFO",13(",",I6),2(",",A2),2(",",A3),11(",",F21.6))


         ENDIF

         DO MODE = 1 , ELNVCT(IS)
            IF (WUPPER(IP) .GE. CF(MODE,IS) .AND. WLOWER(IP) .LE. CF(MODE+1,IS)) THEN
               COL = 'O'//NUM//CHCOD(IS)//CHCOD(MODE); call makmsk(COL_mask,':O:',NUM,CHCOD(IS),CHCOD(MODE))
               COL_ALT = 'O'//NUM//ALT_SP(IS)//CHCOD(MODE); call makmsk(COL_ALT_mask,':O:',NUM,ALT_SP(IS),CHCOD(MODE),':!ALT:')

               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  COL_MIN_SR = 'J' // NUM // CHCOD(IS) // CHCOD(MODE); call makmsk(COL_MIN_SR_mask,':J:' , NUM , CHCOD(IS) , CHCOD(MODE))
                  COL_MIN_SR_ALT = 'J' // NUM // ALT_SP(IS) // CHCOD(MODE); call makmsk(COL_MIN_SR_ALT_mask,':J:' , NUM , ALT_SP(IS) , CHCOD(MODE),':!ALT:')
                  COL_MAX_SR = 'U' // NUM // CHCOD(IS) // CHCOD(MODE); call makmsk(COL_MAX_SR_mask,':U:' , NUM , CHCOD(IS) , CHCOD(MODE))
                  COL_MAX_SR_ALT = 'U' // NUM // ALT_SP(IS) // CHCOD(MODE); call makmsk(COL_MAX_SR_ALT_mask,':U:' , NUM , ALT_SP(IS) , CHCOD(MODE),':!ALT:')
               END IF

               GEN = 0.0
               GEN_MIN = 0.0
               GEN_MAX = 0.0

!              set up intersection with load rows

               DO IVSL = 1, MODE
                  GRP = ELGRP(IVSL,IS)
                  SEG = ELSEG(IVSL,IS)
                  FSL = EFD_Slice_ID(GRP,SEG)
                  FSLCD = CHAR(64+FSL)
                  LOAD = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(LOAD_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)

                  IF (IP .LE. EFD_D_DSP) THEN
                     CALL ELDRAT(IS,IRG,N,IVSL,DRAT_ORG)

!                    For ECP Types which have Spinning Reserve Credit Create Min, Max and Load Following Modes for each of the original modes

                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN

!                       Load Following Mode Follows Load Height drat = (1 - for) * (ld / max_ld)
!                       Use average ReSTORE CF for nuclear and baseload fossil for load following mode to allow for following net load
                        IF (INUC .EQ. 1) THEN
                           DRAT = MAX((1.0 - UG_FOR(N))*(1.0 - UG_LFR(N)) * NUC_CF_EFD(SEG,GRP,IRG),SR_MIN_CF(IECP))
                        ELSEIF (IFOSBS .EQ. 1) THEN
                           DRAT = MAX((1.0 - UG_FOR(N)) *FOS_CF_EFD(SEG,GRP,IRG),SR_MIN_CF(IECP))
                        ELSE

                          DRAT = (1.0 - UG_FOR(N)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD)
                        ENDIF

!                       Minimum Electricity - Maximum Spinning Reserve Credit drat_min = (1 - sr_min_lf) * sr_min_cf * (ld / min_ld) + sr_min_lf * sr_min_cf

                        DRAT_MIN = (1.0 - SR_MIN_LF(IECP)) * SR_MIN_CF(IECP) * (UTHGHT(SEG,GRP,IRG) / MIN_SP_LOAD) + SR_MIN_LF(IECP) * SR_MIN_CF(IECP)

!                       Maximum Electricity - Minimum Spinning Reserve Credit drat_max = (1 - sr_max_lf) * (1 - for) * (ld / max_ld) + sr_max_lf * (1 - for)

                        DRAT_MAX = (1.0 - SR_MAX_LF(IECP)) * (1.0 - UG_FOR(N)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD) + SR_MAX_LF(IECP) * (1.0 - UG_FOR(N))
                   
                        IF (UG_MRUN(N) .GT. 0 .AND. DRAT_MAX .LT. DRAT_ORG) THEN

!                          WRITE(6,2317) CURIRUN, CURIYR+1989, CURITR, IRG, N, IGRP, IECP, COL_MAX_SR, MODE, IS, IVSL, GRP, SEG, LOAD, UTHGHT(SEG,GRP,IRG), &
!                             UG_FOR(N), UG_LFR(N), UG_PMR(N), MIN_SP_LOAD, MAX_SP_LOAD, SR_MIN_LF(IECP), SR_MAX_LF(IECP), SR_MIN_CF(IECP), &
!                             (ECMRUNCF(IGRP) * EFACTR), DRAT, DRAT_MIN, DRAT_MAX, DRAT_ORG, EETIME(IS), ETWDTH(IVSL,IS,IRG), ETHGHT(IVSL,IS,IRG), &
!                             FAC(IVSL,IS,IRG), MAXCF(IS)
!2317                      FORMAT(1X,"DRAT_INFO",7(":",I5),":",A16,5(":",I3),":",A16,19(":",F21.6))
                         
                           DRAT_MAX = MAX(DRAT_MAX , DRAT_ORG)

                        END IF

                        IF (IECP .EQ. WIAN .OR. IECP .EQ. WICN .OR. IECP .EQ. WISM) THEN
                           DRAT_MAX = DRAT_ORG
                        END IF

                        IF (IECP .LE. WIIS .AND. CURIYR + UHBSYR .LE. UYR_STEO + 1) THEN
                           DRAT_MAX = DRAT_ORG
                        END IF
                     ELSE
                        DRAT = DRAT_ORG
                     END IF
                  ELSE
                     DRAT = MAXCF(IS)
                  ENDIF

                  IF (ISNAN(DRAT).OR. ABS(DRAT) .GT. HUGE(DRAT)) THEN   ! check for NaNQ this way
                     WRITE(6,3000) CURIRUN,CURIYR+UHBSYR,CURITR,IS,IRG,N,IVSL,UG_FOR(N),UG_LFR(N),EETIME(IS),MAXCF(IS),(ETWDTH(JVLS,IS,IRG),JVLS=1,ELNVCT(IS))
 3000                FORMAT(1X,"DRAT_OOPS",7(":",I5),13(":",F12.3))
                     DRAT = MAXCF(IS)
                  END IF

!STEOBM           adjust drat (capacity factor) for STEO benchmarking nuclear (factor will be 1.0 if not benchmarking) 

                  IF (IECP .EQ. WICN .OR. IECP .EQ. WIAN .OR. IECP .EQ. WISM)  THEN
                     DRAT = DRAT * URNCCFA(CURIYR)
                     DRAT_MAX = DRAT_MAX * URNCCFA(CURIYR) 
                     DRAT_MIN = DRAT_MIN * URNCCFA(CURIYR) 
                  ENDIF

                  IF (DRAT .LT. 0.01) DRAT = 0.01
                  VAL = DRAT * ELWDTH(IVSL,IS)
                  CALL DVAL(COL,LOAD,DBLE(0.1 * VAL),COL_mask,LOAD_mask,'OPBSLD,1')

                  IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,LOAD,DBLE(0.1 * VAL * ADJ_FAC),COL_ALT_mask,LOAD_mask,'OPBSLD,2')

                  GEN = GEN + VAL

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     ROW_SR = 'SR' // URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(ROW_SR_mask,':SR:' , URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)
                     VAL_SR = (1.0 - DRAT) * SR_CREDIT(IECP)
                     CALL DVAL(COL,ROW_SR,VAL_SR,COL_mask,ROW_SR_mask,'OPBSLD,3')

                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW_SR,DBLE(VAL_SR * ADJ_FAC),COL_ALT_mask,ROW_SR_mask,'OPBSLD,4')

                     VAL_MIN_SR = DRAT_MIN * ELWDTH(IVSL,IS)
                     CALL DVAL(COL_MIN_SR,LOAD,DBLE(0.1 * VAL_MIN_SR),COL_MIN_SR_mask,LOAD_mask,'OPBSLD,5')

                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,LOAD,DBLE(0.1 * VAL_MIN_SR * ADJ_FAC),COL_MIN_SR_ALT_mask,LOAD_mask,'OPBSLD,6')

                     GEN_MIN = GEN_MIN + VAL_MIN_SR
                     VAL_MIN_SR = (1.0 - DRAT_MIN) * SR_CREDIT(IECP) 
                     CALL DVAL(COL_MIN_SR,ROW_SR,VAL_MIN_SR,COL_MIN_SR_mask,ROW_SR_mask,'OPBSLD,7')
                     
                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW_SR,DBLE(VAL_MIN_SR * ADJ_FAC),COL_MIN_SR_ALT_mask,ROW_SR_mask,'OPBSLD,7.1')
                     
                     VAL_MAX_SR = DRAT_MAX * ELWDTH(IVSL,IS)
                     CALL DVAL(COL_MAX_SR,LOAD,DBLE(0.1 * VAL_MAX_SR),COL_MAX_SR_mask,LOAD_mask,'OPBSLD,8')

                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,LOAD,DBLE(0.1 * VAL_MAX_SR * ADJ_FAC),COL_MAX_SR_ALT_mask,LOAD_mask,'OPBSLD,9')

                     GEN_MAX = GEN_MAX + VAL_MAX_SR
                     VAL_MAX_SR = (1.0 - DRAT_MAX) * SR_CREDIT(IECP)
                     CALL DVAL(COL_MAX_SR,ROW_SR,VAL_MAX_SR,COL_MAX_SR_mask,ROW_SR_mask,'OPBSLD,10')
                     
                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW_SR,DBLE(VAL_MAX_SR * ADJ_FAC),COL_MAX_SR_ALT_mask,ROW_SR_mask,'OPBSLD,10.1')

                     IF (ISNAN(DRAT_MAX) .OR. ABS(DRAT_MAX) .GT. HUGE(DRAT_MAX)) THEN   ! check for NaNQ this way
                        WRITE(6,3710) CURIRUN, CURIYR+1989, CURITR, IRG, IECP, COL_MAX_SR, DRAT_MAX, DRAT_ORG, VAL_MAX_SR, &
                           SR_CREDIT(IECP), VAL_MIN_SR, DRAT_MIN, SR_MAX_LF(IECP), UG_FOR(N) * UTHGHT(SEG,GRP,IRG), MAX_SP_LOAD
 3710                   FORMAT(1X,"DRAT_MAX_NAN",5(":",I5),":",A16,10(":",F21.6))
                     END IF

                  END IF

!                 capacity balance if EPPOPM=2

                  IF (EPPOPM(IP) .EQ. 2) THEN
                     ROW = 'CP'//CHCOD(IRG)//EPPLCD(IP)//CHCOD(IS)//FSLCD; call makmsk(ROW_mask,':CP:',CHCOD(IRG),EPPLCD(IP),CHCOD(IS),FSLCD)
                     CALL DVAL(COL,ROW,1.0D0,COL_mask,ROW_mask,'OPBSLD,11')
                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW,1.0D0,COL_ALT_mask,ROW_mask,'OPBSLD,12')
                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        CALL DVAL(COL_MIN_SR,ROW,1.0D0,COL_MIN_SR_mask,ROW_mask,'OPBSLD,13')
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW,1.0D0,COL_MIN_SR_ALT_mask,ROW_mask,'OPBSLD,14')
                        CALL DVAL(COL_MAX_SR,ROW,1.0D0,COL_MAX_SR_mask,ROW_mask,'OPBSLD,15')
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW,1.0D0,COL_MAX_SR_ALT_mask,ROW_mask,'OPBSLD,16')
                     END IF
                  ENDIF

!                 if (curitr .eq. 1 .and. irg .eq. 1 .and. iecp .eq. wiet)write(6,3456) curiyr + 1989,is,mode,ivsl,grp,seg,eetime(is),gen,maxcf(is)
!3456             format(1h ,'!ctcf',i4,i3,i3,i3,i3,i3,4f10.3)

               ENDDO

!              set bound on column

               CALL DBND(EFDBND,COL,0.0D0,DBLE(CAP(IS)*0.001),COL_mask,'OPBSLD,17')
               IF (ADJ_FAC .NE. 1.0) CALL DBND(EFDBND,COL_ALT,0.0D0,DBLE(CAP(IS)*0.001),COL_ALT_mask,'OPBSLD,18')

               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  CALL DBND(EFDBND,COL_MIN_SR,0.0D0,DBLE(CAP(IS)*0.001),COL_MIN_SR_mask,'OPBSLD,19')
                  IF (ADJ_FAC .NE. 1.0) CALL DBND(EFDBND,COL_MIN_SR_ALT,0.0D0,DBLE(CAP(IS)*0.001),COL_MIN_SR_ALT_mask,'OPBSLD,20')
                  CALL DBND(EFDBND,COL_MAX_SR,0.0D0,DBLE(CAP(IS)*0.001),COL_MAX_SR_mask,'OPBSLD,21')
                  IF (ADJ_FAC .NE. 1.0) CALL DBND(EFDBND,COL_MAX_SR_ALT,0.0D0,DBLE(CAP(IS)*0.001),COL_MAX_SR_ALT_mask,'OPBSLD,22')
               END IF

!              intersection with OBJ

!              national RPS only

               RPSU = 0.0
               RPSR = 0.0
               RPSCST = 0.0

!              national RPS only

               IF (UPRNWREG .EQ. 1) THEN
                  RPSCST = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
                  IF (UPTTYP(IECP) .LE. NW_COAL .AND. USW_ECPCF .EQ. 1) THEN    !adjust RPS for cofiring
                     CFSH = UPWDCFR(IECP,EPCLMP(TFR))
                     RPSCST = RPSCST - EPRPSPR(CURIYR) * CFSH * UPRNWSHR(WIWD)
                  END IF
                  IF (UPRNWBAS(IECP) .GT. 0.0)RPSCST = 0.0

!              regional RPS only

               ELSEIF (UPRNWREG .EQ. 2) THEN
                  RPSCST = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
                  IF (UPTTYP(IECP) .LE. NW_COAL .AND. USW_ECPCF .EQ. 1) THEN    !adjust RPS for cofiring
                     CFSH = UPWDCFR(IECP,EPCLMP(TFR))
                     RPSCST = RPSCST - EPRPSPRR(IRG,CURIYR) * CFSH * UPRNWSHRR(WIWD,IRG)
                  END IF

!              national and RPS

               ELSEIF (UPRNWREG .EQ. 3) THEN
                  RPSU = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
                  RPSR = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
                  IF (UPTTYP(IECP) .LE. NW_COAL .AND. USW_ECPCF .EQ. 1) THEN    !adjust RPS for cofiring
                     CFSH = UPWDCFR(IECP,EPCLMP(TFR))
                     RPSU = RPSU - EPRPSPR(CURIYR) * CFSH * UPRNWSHR(WIWD)
                     RPSR = RPSR - EPRPSPRR(IRG,CURIYR) * CFSH * UPRNWSHRR(WIWD,IRG)
                  END IF
                  IF (UPRNWBAS(IECP) .GT. 0.0)RPSU = 0.0
                  IF(EPRPSPR(CURIYR) .GE. EPRPSPRR(IRG,CURIYR))THEN
                     RPSCST = RPSU
                  ELSE
                     RPSCST = RPSR

!                    IF (RPSU .LT. 0.0)RPSCST = RPSR + RPSU
                  END IF

!                 if (curitr .gt. maxitr .and. (iecp .le. wihc))  &
!                    write(6,3230) curiyr+1989,uprnwreg,irg,col,uplntcd(iecp),gen,  &
!                    EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP)),  &
!                    EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG)),  &
!                    UPWDCFR(IECP,EPCLMP(TFR)),rpsu,rpsr,rpscst
!3230             format(1h ,'!efdcl',i4,i3,i3,a8,a3,8f10.3)
!                 if (curitr .gt. maxitr .and. (iecp .eq. wicc .or. iecp .eq. wiac))  &
!                    write(6,3231) curiyr+1989,uprnwreg,irg,col,uplntcd(iecp),gen,rpsu,rpsr,rpscst,vom,ptc,objval
!3231             format(1h ,'!efdcc',i4,i3,i3,a8,a3,8f10.3)
               ENDIF
!               IF (FCRL .EQ. 1) WRITE(13,3230) '!EFD RPS1',CURIYR,CURITR,IRG,IECP,IGRP,RPSCST,EPRPSPR(CURIYR),UPRNWBNDR(CURIYR,IRG),UPRNWSHR(IECP)
3230       FORMAT(1x,A10,5I6,4F15.5)

               IF (ST_RPS_SW .GT. 0) THEN
                  RPSR = ST_RPS_EMM_P(IRG,CURIYR) * (ST_RNW_BND(CURIYR,IRG) - ST_RNW_SHR(IECP,IRG,CURIYR))
                 IF (RPSCST .LT. 0.0 .AND. RPSR .LE. 0.0) THEN  ! if both are negative use the greatest benefit
                  IF (ABS(RPSCST) .LT. ABS(RPSR)) RPSCST = RPSR
                 ELSEIF (RPSCST .GT. 0.0 .AND. RPSR .GT. 0.0) THEN     ! if both are positive, use the highest positive cost
                  IF (RPSCST .LT. RPSR) RPSCST = RPSR
                 ELSE                        ! use net value
                     RPSCST = RPSCST + RPSR
                 ENDIF                  
               END IF
  
!               IF (FCRL .EQ. 1) WRITE(13,3230) 'EFD RPS2',CURIYR,CURITR,IRG,IECP,IGRP,RPSCST,ST_RPS_EMM_P(IRG,CURIYR),ST_RNW_BND(CURIYR,IRG),ST_RNW_SHR(IECP,IRG,CURIYR)

!              IF ((UPRNWREG .EQ. 1 .OR. UPRNWREG .EQ. 3) .AND. UPRNWBAS(IECP) .GT. 0.0)RPSU = 0.0
!              RPSCST = MAX(RPSU,RPSR)
!              RPSCST = RPSU + RPSR
!              if ((uplntcd(iecp) .eq. 'CN' .or. uplntcd(iecp) .eq. 'AN') .and. curitr .eq. 1)  &
!                 write(6,2345) curiyr+1989,uplntcd(iecp),col,rpsu,rpsr,rpscst
!2345          format(1h ,'!colnuc',i4,a3,a10,3f10.3)

!              IF (UPRNWREG .EQ. 1) THEN
!                 RPSCST = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
!                 IF (UPTTYP(IECP) .LE. NW_COAL .AND. USW_ECPCF .EQ. 1) THEN    !adjust RPS for cofiring
!                    CFSH = UPWDCFR(IECP,EPCLMP(TFR))
!                    RPSCST = RPSCST - EPRPSPR(CURIYR) * CFSH * UPRNWSHR(WIWD)
!                 END IF
!
!              regional RPS only
!
!              ELSEIF (UPRNWREG .EQ. 2) THEN
!                 RPSCST = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
!                 IF (UPTTYP(IECP) .LE. NW_COAL .AND. USW_ECPCF .EQ. 1) THEN    !adjust RPS for cofiring
!                    CFSH = UPWDCFR(IECP,EPCLMP(TFR))
!                    RPSCST = RPSCST - EPRPSPRR(IRG,CURIYR) * CFSH * UPRNWSHRR(WIWD,IRG)
!                 END IF
!
!              national and RPS
!
!              ELSEIF (UPRNWREG .EQ. 3) THEN
!                 RPSU = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
!                 RPSR = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
!                 IF (UPTTYP(IECP) .LE. NW_COAL .AND. USW_ECPCF .EQ. 1) THEN    !adjust RPS for cofiring
!                    CFSH = UPWDCFR(IECP,EPCLMP(TFR))
!                    RPSU = RPSU - EPRPSPR(CURIYR) * CFSH * UPRNWSHR(WIWD)
!                    RPSR = RPSR - EPRPSPRR(IRG,CURIYR) * CFSH * UPRNWSHRR(WIWD,IRG)
!                 END IF
!                 IF(EPRPSPR(CURIYR) .GE. EPRPSPRR(IRG,CURIYR))THEN
!                    RPSCST = RPSU
!                 ELSE
!                    RPSCST = RPSR
!                    IF (RPSU .LT. 0.0)RPSCST = RPSR + RPSU
!                 END IF
!              ENDIF
!              IF ((UPRNWREG .EQ. 1 .OR. UPRNWREG .EQ. 3) .AND. UPRNWBAS(IECP) .GT. 0.0)RPSCST = 0.0
!
!              DETERMINE CARBON INTENSITY RATE AND GPS ADDER, IF ANY
!
!              This whole portion might be related to the carbon emissions 
               CLSH  = 0.0
               GASSH = 0.0
               OLSH  = 0.0
               CFSH  = 0.0
               CLCAR = 0.0
               OGCAR = 0.0
               WDCAR = 0.0
               ALT_CLCAR = 0.0
               ALT_OGCAR = 0.0
               ALT_WDCAR = 0.0
               CO2LB = 0.0
               CO2LB_MIN = 0.0
               CO2LB_MAX = 0.0
               ALT_CO2LB = 0.0
               ALT_CO2LB_MIN = 0.0
               ALT_CO2LB_MAX = 0.0
!              ACCOUNT FOR CHP ADJUSTMENT FOR NT COGEN
               IF (UG_MRUN(n) .GT. 9000)THEN
                  CO2ADJ = CO2_ADJNT
               ELSE
                  CO2ADJ = 1.0
               END IF

               FROM_LABEL = "IN_" // COL
               FUEL_RGN = TFR
               ECPt = IECP
               Load_Level = GEN / SP_HOURS
               CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)

               IF (SR_CREDIT(IECP) .GT. 0.0) THEN

                  FROM_LABEL = "IN_" // COL_MIN_SR
                  Load_Level = GEN_MIN / SP_HOURS
                  CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ_MIN, Target_EFF, Max_EFF)

                  FROM_LABEL = "IN_" // COL_MAX_SR
                  Load_Level = GEN_MAX / SP_HOURS
                  CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ_MAX, Target_EFF, Max_EFF)
               END IF

               IF (IECP .LE. WIIS)THEN
                  GASSH = EDMXGS(1,IECP,TFR)
                  OLSH = EDMXDS(1,IECP,TFR)
                  CFSH = UPWDCFR(IECP,EPCLMP(TFR))
                  IF ((GASSH + OLSH + CFSH) .GT. 1.0) THEN
                     TOT = GASSH + OLSH + CFSH
                     GASSH = GASSH * 1.0 / TOT
                     OLSH = OLSH * 1.0 / TOT
                     CFSH = CFSH * 1.0 / TOT
                  ENDIF
                  CLSH = 1.0 - (GASSH + OLSH + CFSH)
                  CLCAR = CLSH * UFRCAR(IECP,CRG) * (1.0 / 2204.0) * (1.0 - UPPCEF(IECP))
                  OGCAR = GASSH *  ENGEL(CURIYR) * 0.001
                  OGCAR = OGCAR + OLSH *  EDSEL(CURIYR) * 0.001
                  OGCAR = OGCAR * (1.0 - UPPCEF(IECP))

!                 WDCAR = CFSH * UFRCAR(UIWD,CRG) * (1.0 / 2204.0) * (1.0 - UPPCEF(IECP))

                  WDCAR = CFSH * CO2_EMSWD * (12.0 / 44.0) * (1.0 / 2204.0) * (1.0 - UPPCEF(IECP))

                  ALT_CLCAR = CLSH * UFRCAR(IECP,CRG) * (1.0 / 2204.0) * (1.0 - UPPCEF_MIN(IECP))
                  ALT_OGCAR = GASSH *  ENGEL(CURIYR) * 0.001
                  ALT_OGCAR = ALT_OGCAR + OLSH *  EDSEL(CURIYR) * 0.001
                  ALT_OGCAR = ALT_OGCAR * (1.0 - UPPCEF_MIN(IECP))

!                 ALT_WDCAR = CFSH * UFRCAR(UIWD,CRG) * (1.0 / 2204.0) * (1.0 - UPPCEF_MIN(IECP))

                  ALT_WDCAR = CFSH * CO2_EMSWD * (12.0 / 44.0) * (1.0 / 2204.0) * (1.0 - UPPCEF_MIN(IECP))                  
                  
                  
                  CO2LB = (HTRT * HTRT_ADJ * 0.001) * (CLCAR + OGCAR + WDCAR) * (44.0 / 12.0) * 2204.0
                  ALT_CO2LB = (HTRT * HTRT_ADJ * 0.001) * (ALT_CLCAR + ALT_OGCAR + ALT_WDCAR) * (44.0 / 12.0) * 2204.0
                  
                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     CO2LB_MIN = (HTRT * HTRT_ADJ_MIN * 0.001) * (CLCAR + OGCAR + WDCAR) * (44.0 / 12.0) * 2204.0
                     ALT_CO2LB_MIN = (HTRT * HTRT_ADJ_MIN * 0.001) * (ALT_CLCAR + ALT_OGCAR + ALT_WDCAR) * (44.0 / 12.0) * 2204.0
                     CO2LB_MAX = (HTRT * HTRT_ADJ_MAX * 0.001) * (CLCAR + OGCAR + WDCAR) * (44.0 / 12.0) * 2204.0
                     ALT_CO2LB_MAX = (HTRT * HTRT_ADJ_MAX * 0.001) * (ALT_CLCAR + ALT_OGCAR + ALT_WDCAR) * (44.0 / 12.0) * 2204.0
                  END IF

                  if (cfsh .gt. 0.0 .and. irg .eq. 1 .and. FCRL .EQ. 1 .AND. CURIYR+1989 .EQ. 2017) write(18,1212) curiyr+1989,tfr,clsh,olsh,gassh,cfsh,ufrcar(iecp,crg),ufrcar(uiwd,crg), &
                      CLCAR,OGCAR,WDCAR,ALT_CLCAR,ALT_OGCAR,ALT_WDCAR
 1212             format(1h ,'!cfcar',":",i4,":",i3,12(":",f10.3))

               ELSEIF (IECP .LT. WICN)THEN
                 GSPR = UPFUEL(UIGF,EPGSMP(TFR))
               IF (IECP .NE. WIST)THEN
                     OLPR = UPFUEL(UIDS,EPCSMP(TFR))
                     IF (GSPR .LE. OLPR)THEN
                        GASSH = EDMXGS(2,IP,TFR)
                        OLSH = 1.0 - GASSH
                     ELSE
                        OLSH = EDMXDS(2,IP,TFR)
                        GASSH = 1.0 - OLSH
                     END IF
                     OGCAR = GASSH *  ENGEL(CURIYR) * 0.001
                     OGCAR = OGCAR + OLSH *  EDSEL(CURIYR) * 0.001
                  ELSE
                     OLPR = UPFUEL(UIRH,EPCSMP(TFR))
                     IF (GSPR .LE. OLPR)THEN
                        GASSH = EDMXGS(2,IP,TFR)
                        OLSH = 1.0 - GASSH
                     ELSE
                        OLSH = EDMXRS(2,IP,TFR)
                        GASSH = 1.0 - OLSH
                     END IF
                     OGCAR = GASSH *  ENGEL(CURIYR) * 0.001
                     OGCAR = OGCAR + OLSH *  ERSEL(CURIYR) * 0.001
                  END IF
                  ALT_OGCAR = OGCAR * (1.0 - UPPCEF_MIN(IECP))
                  OGCAR = OGCAR * (1.0 - UPPCEF(IECP))
                  CO2LB = (HTRT * HTRT_ADJ * 0.001) * OGCAR * (44.0 / 12.0) * 2204.0
                  ALT_CO2LB = (HTRT * HTRT_ADJ * 0.001) * ALT_OGCAR * (44.0 / 12.0) * 2204.0
                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     CO2LB_MIN = (HTRT * HTRT_ADJ_MIN * 0.001) * OGCAR * (44.0 / 12.0) * 2204.0
                     ALT_CO2LB_MIN = (HTRT * HTRT_ADJ_MIN * 0.001) * ALT_OGCAR * (44.0 / 12.0) * 2204.0
                     CO2LB_MAX = (HTRT * HTRT_ADJ_MAX * 0.001) * OGCAR * (44.0 / 12.0) * 2204.0
                     ALT_CO2LB_MAX = (HTRT * HTRT_ADJ_MAX * 0.001) * ALT_OGCAR * (44.0 / 12.0) * 2204.0
                  END IF
               END IF
               CO2LB = CO2LB / CO2ADJ
               ALT_CO2LB = ALT_CO2LB / CO2ADJ
               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  CO2LB_MIN = CO2LB_MIN / CO2ADJ
                  ALT_CO2LB_MIN = ALT_CO2LB_MIN / CO2ADJ
                  CO2LB_MAX = CO2LB_MAX / CO2ADJ
                  ALT_CO2LB_MAX = ALT_CO2LB_MAX / CO2ADJ
               END IF

               GPSCST = 0.0
               GPSCST_MIN = 0.0
               GPSCST_MAX = 0.0
               ALT_GPSCST = 0.0
               ALT_GPSCST_MIN = 0.0
               ALT_GPSCST_MAX = 0.0
!
!              INCLUDE CT IF CF > THRESHHOLD
!
               IF (IECP .EQ. WIET .OR. IECP .EQ. WICT .OR. IECP .EQ. WIAT)THEN
                  IF (CO2_STDTN(IRG) .EQ. 2 .AND. (GEN / EETIME(IS)) .GT. CO2_THRET)THEN
                     CO2_PLTSW(IECP) = 1.0
                  ELSE
                     CO2_PLTSW(IECP) = CO2_PLTET
                  END IF
               END IF
               IF (CO2_PRCSW .EQ. 10 .AND. CO2LB .GT. 0.001 .AND. CO2_PLTSW(IECP) .GT. 0.0)THEN
                  IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.001)THEN
                     GPSCST = (CO2LB - CO2_STDRN(IRG,CURIYR)) * MAX(0.0,(ECPPRCNL(IRG,CURIYR) - EFDPRCNL(IRG,CURIYR)))
                     ALT_GPSCST = (ALT_CO2LB - CO2_STDRN(IRG,CURIYR)) * MAX(0.0,(ECPPRCNL(IRG,CURIYR) - EFDPRCNL(IRG,CURIYR)))
                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        GPSCST_MIN = (CO2LB_MIN - CO2_STDRN(IRG,CURIYR)) * MAX(0.0,(ECPPRCNL(IRG,CURIYR) - EFDPRCNL(IRG,CURIYR)))
                        ALT_GPSCST_MIN = (ALT_CO2LB_MIN - CO2_STDRN(IRG,CURIYR)) * MAX(0.0,(ECPPRCNL(IRG,CURIYR) - EFDPRCNL(IRG,CURIYR)))
                        GPSCST_MAX = (CO2LB_MAX - CO2_STDRN(IRG,CURIYR)) * MAX(0.0,(ECPPRCNL(IRG,CURIYR) - EFDPRCNL(IRG,CURIYR)))
                        ALT_GPSCST_MAX = (ALT_CO2LB_MAX - CO2_STDRN(IRG,CURIYR)) * MAX(0.0,(ECPPRCNL(IRG,CURIYR) - EFDPRCNL(IRG,CURIYR)))
                     END IF
                  ELSE IF (CO2_STDSW .EQ. 2 .AND. CO2_STDRF(TFR,CURIYR) .GT. 0.001)THEN
                     GPSCST = (CO2LB - CO2_STDRF(TFR,CURIYR)) * MAX(0.0,(ECPPRCFL(TFR,CURIYR) - EFDPRCFL(TFR,CURIYR)))
                     ALT_GPSCST = (ALT_CO2LB - CO2_STDRF(TFR,CURIYR)) * MAX(0.0,(ECPPRCFL(TFR,CURIYR) - EFDPRCFL(TFR,CURIYR)))
                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        GPSCST_MIN = (CO2LB_MIN - CO2_STDRF(TFR,CURIYR)) * MAX(0.0,(ECPPRCFL(TFR,CURIYR) - EFDPRCFL(TFR,CURIYR)))
                        ALT_GPSCST_MIN = (ALT_CO2LB_MIN - CO2_STDRF(TFR,CURIYR)) * MAX(0.0,(ECPPRCFL(TFR,CURIYR) - EFDPRCFL(TFR,CURIYR)))
                        GPSCST_MAX = (CO2LB_MAX - CO2_STDRF(TFR,CURIYR)) * MAX(0.0,(ECPPRCFL(TFR,CURIYR) - EFDPRCFL(TFR,CURIYR)))
                        ALT_GPSCST_MAX = (ALT_CO2LB_MAX - CO2_STDRF(TFR,CURIYR)) * MAX(0.0,(ECPPRCFL(TFR,CURIYR) - EFDPRCFL(TFR,CURIYR)))
                     END IF
                  END IF 
                  GPSCST = GPSCST * CO2_PLTSW(IECP)
                  ALT_GPSCST = ALT_GPSCST * CO2_PLTSW(IECP)
                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     GPSCST_MIN = GPSCST_MIN * CO2_PLTSW(IECP)
                     ALT_GPSCST_MIN = ALT_GPSCST_MIN * CO2_PLTSW(IECP)
                     GPSCST_MAX = GPSCST_MAX * CO2_PLTSW(IECP)
                     ALT_GPSCST_MAX = ALT_GPSCST_MAX * CO2_PLTSW(IECP)
                  END IF
               END IF 

!      if (curiyr+1989 .EQ. 2030) &
!       write(6,3232) curiyr+1989,curitr,uprnwreg,irg,col,uplntcd(iecp),gen,rpsu,rpsr,rpscst,vom,ptc,objval
!3232 format(1h ,'!efdbsl',4I6,a10,a3,8f10.3)
               OBJVAL = (VOM + RPSCST + GPSCST - PTC) * GEN * 0.001   ! units million $ / GW
!               IF (FCRL .EQ. 1) write(13,3231) 'EFD OBJ',CURIYR,CURITR,IRG,IECP,IGRP,OBJVAL,VOM,RPSCST,GPSCST,PTC,GEN
3231      FORMAT(1x,A10,5I6,F15.2,4F15.5,F15.2)               
               CALL DVAL(COL,EFDOBJ,OBJVAL,COL_mask,EFDOBJ,'OPBSLD,23')
               OBJVAL = (VOM + RPSCST + ALT_GPSCST - PTC) * GEN * 0.001   ! units million $ / GW
               IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,EFDOBJ,OBJVAL,COL_ALT_mask,EFDOBJ,'OPBSLD,24')

               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  VAL_MIN_SR = (VOM + RPSCST + GPSCST_MIN - PTC) * GEN_MIN * 0.001   ! units million $ / GW
                  CALL DVAL(COL_MIN_SR,EFDOBJ,VAL_MIN_SR,COL_MIN_SR_mask,EFDOBJ,'OPBSLD,25')
                  VAL_MIN_SR = (VOM + RPSCST + ALT_GPSCST_MIN - PTC) * GEN_MIN * 0.001   ! units million $ / GW
                  IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,EFDOBJ,VAL_MIN_SR,COL_MIN_SR_ALT_mask,EFDOBJ,'OPBSLD,26')

                  VAL_MAX_SR = (VOM + RPSCST + GPSCST_MAX - PTC) * GEN_MAX * 0.001   ! units million $ / GW
                  CALL DVAL(COL_MAX_SR,EFDOBJ,VAL_MAX_SR,COL_MAX_SR_mask,EFDOBJ,'OPBSLD,27')
                  VAL_MAX_SR = (VOM + RPSCST + ALT_GPSCST_MAX - PTC) * GEN_MAX * 0.001   ! units million $ / GW
                  IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,EFDOBJ,VAL_MAX_SR,COL_MAX_SR_ALT_mask,EFDOBJ,'OPBSLD,28')
               END IF

!              capacity balance

               IF (EPPOPM(IP) .EQ. 1) THEN
                  ROW = 'CB'//NUM//CHCOD(IS); call makmsk(ROW_mask,':CB:',NUM,CHCOD(IS))
                  CALL DVAL(COL,ROW,1.0D0,COL_mask,ROW_mask,'OPBSLD,29')
                  IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW,1.0D0,COL_ALT_mask,ROW_mask,'OPBSLD,30')

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     CALL DVAL(COL_MIN_SR,ROW,1.0D0,COL_MIN_SR_mask,ROW_mask,'OPBSLD,31')
                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW,1.0D0,COL_MIN_SR_ALT_mask,ROW_mask,'OPBSLD,32')
                     CALL DVAL(COL_MAX_SR,ROW,1.0D0,COL_MAX_SR_mask,ROW_mask,'OPBSLD,33')
                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW,1.0D0,COL_MAX_SR_ALT_mask,ROW_mask,'OPBSLD,34')
                  END IF
               ENDIF

!              BTU row for fossil or biomass or nuclear

               IF (IP .LE. UISMR .OR. IP .EQ. UIBMS .OR. IP .EQ. UIBIG .OR. IP .EQ. UIDGB .OR. IP .EQ. UIDGP) THEN

                  BTUREQ = GEN * HTRT * HTRT_ADJ * 0.000001  !  trill BTU per GW

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN

                     BTUREQ_MIN = GEN_MIN * HTRT * HTRT_ADJ_MIN * 0.000001  !  trill BTU per GW

                     BTUREQ_MAX = GEN_MAX * HTRT * HTRT_ADJ_MAX * 0.000001  !  trill BTU per GW
                  END IF

                  if (BTUREQ .LT. 0.0) write(UF_DBG,*) 'OPBSLD BTUREQ < 0',IP,IECP,IGRP,GEN,HTRT,BTUREQ

!                 Benchmarking for fossil generation and consumption, if enforced

!                 Coal

                  IF (BMCLTOL .GT. 0.0)THEN
                     IF (CLSH .GT. 0.0)THEN
                        IF (BMCLGEN(CURIYR) .GT. 0.0)THEN
                           ROW = 'GENCL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENCL:' , URGNME(IRG)(6:7))
                           VAL = CLSH * GEN * 0.001
                           CALL DVAL(COL,ROW,VAL,COL_mask,ROW_mask,'OPBSLD,35')
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW,DBLE(VAL*ADJ_FAC),COL_ALT_mask,ROW_mask,'OPBSLD,36')
                 
                           IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                              VAL_MIN_SR = CLSH * GEN_MIN * 0.001
                              CALL DVAL(COL_MIN_SR,ROW,VAL_MIN_SR,COL_MIN_SR_mask,ROW_mask,'OPBSLD,37')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW,DBLE(VAL_MIN_SR*ADJ_FAC),COL_MIN_SR_ALT_mask,ROW_mask,'OPBSLD,38')
                              VAL_MAX_SR = CLSH * GEN_MAX * 0.001
                              CALL DVAL(COL_MAX_SR,ROW,VAL_MAX_SR,COL_MAX_SR_mask,ROW_mask,'OPBSLD,39')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW,DBLE(VAL_MAX_SR*ADJ_FAC),COL_MAX_SR_ALT_mask,ROW_mask,'OPBSLD,40')
                           END IF
                        END IF
                        IF (BMCLCON(CURIYR) .GT. 0.0)THEN
                           ROW = 'CONCL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CONCL:' , URGNME(IRG)(6:7))
                           VAL = CLSH * BTUREQ
                           CALL DVAL(COL,ROW,VAL,COL_mask,ROW_mask,'OPBSLD,41')
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW,VAL,COL_ALT_mask,ROW_mask,'OPBSLD,42')
                    
                           IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                              VAL_MIN_SR = CLSH * BTUREQ_MIN
                              CALL DVAL(COL_MIN_SR,ROW,VAL_MIN_SR,COL_MIN_SR_mask,ROW_mask,'OPBSLD,43')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW,VAL_MIN_SR,COL_MIN_SR_ALT_mask,ROW_mask,'OPBSLD,44')
                              VAL_MAX_SR = CLSH * BTUREQ_MAX
                              CALL DVAL(COL_MAX_SR,ROW,VAL_MAX_SR,COL_MAX_SR_mask,ROW_mask,'OPBSLD,45')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW,VAL_MAX_SR,COL_MAX_SR_ALT_mask,ROW_mask,'OPBSLD,46')
                           END IF
                        END IF
                     END IF
                  END IF

!                 Gas 

                  IF (BMNGTOL .GT. 0.0)THEN
                     IF (GASSH .GT. 0.0)THEN
                        IF (BMNGGEN(CURIYR) .GT. 0.0)THEN
                           ROW = 'GENNG' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENNG:' , URGNME(IRG)(6:7))
                           VAL = GASSH * GEN * 0.001
                           CALL DVAL(COL,ROW,VAL,COL_mask,ROW_mask,'OPBSLD,47')
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW,DBLE(VAL*ADJ_FAC),COL_ALT_mask,ROW_mask,'OPBSLD,48')
                  
                           IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                              VAL_MIN_SR = GASSH * GEN_MIN * 0.001
                              CALL DVAL(COL_MIN_SR,ROW,VAL_MIN_SR,COL_MIN_SR_mask,ROW_mask,'OPBSLD,49')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW,DBLE(VAL_MIN_SR*ADJ_FAC),COL_MIN_SR_ALT_mask,ROW_mask,'OPBSLD,50')
                              VAL_MAX_SR = GASSH * GEN_MAX * 0.001
                              CALL DVAL(COL_MAX_SR,ROW,VAL_MAX_SR,COL_MAX_SR_mask,ROW_mask,'OPBSLD,51')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW,DBLE(VAL_MAX_SR*ADJ_FAC),COL_MAX_SR_ALT_mask,ROW_mask,'OPBSLD,52')
                           END IF
                        END IF
                        IF (BMNGCON(CURIYR) .GT. 0.0)THEN
                           ROW = 'CONNG' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CONNG:' , URGNME(IRG)(6:7))
                           VAL = GASSH * BTUREQ
                           CALL DVAL(COL,ROW,VAL,COL_mask,ROW_mask,'OPBSLD,53')
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW,VAL,COL_ALT_mask,ROW_mask,'OPBSLD,54')
                 
                           IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                              VAL_MIN_SR = GASSH * BTUREQ_MIN
                              CALL DVAL(COL_MIN_SR,ROW,VAL_MIN_SR,COL_MIN_SR_mask,ROW_mask,'OPBSLD,55')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW,VAL_MIN_SR,COL_MIN_SR_ALT_mask,ROW_mask,'OPBSLD,56')
                              VAL_MAX_SR = GASSH * BTUREQ_MAX
                              CALL DVAL(COL_MAX_SR,ROW,VAL_MAX_SR,COL_MAX_SR_mask,ROW_mask,'OPBSLD,57')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW,VAL_MAX_SR,COL_MAX_SR_ALT_mask,ROW_mask,'OPBSLD,58')
                           END IF
                        END IF
                     END IF
                  END IF

!                 Oil

                  IF (BMOLTOL .GT. 0.0)THEN
                     IF (OLSH .GT. 0.0)THEN
                        IF (BMOLGEN(CURIYR) .GT. 0.0)THEN
                           ROW = 'GENOL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENOL:' , URGNME(IRG)(6:7))
                           VAL = OLSH * GEN * 0.001
                           CALL DVAL(COL,ROW,VAL,COL_mask,ROW_mask,'OPBSLD,59')
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW,DBLE(VAL*ADJ_FAC),COL_ALT_mask,ROW_mask,'OPBSLD,60')
                  
                           IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                              VAL_MIN_SR = OLSH * GEN_MIN * 0.001
                              CALL DVAL(COL_MIN_SR,ROW,VAL_MIN_SR,COL_MIN_SR_mask,ROW_mask,'OPBSLD,61')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW,DBLE(VAL_MIN_SR*ADJ_FAC),COL_MIN_SR_ALT_mask,ROW_mask,'OPBSLD,62')
                              VAL_MAX_SR = OLSH * GEN_MAX * 0.001
                              CALL DVAL(COL_MAX_SR,ROW,VAL_MAX_SR,COL_MAX_SR_mask,ROW_mask,'OPBSLD,63')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW,DBLE(VAL_MAX_SR*ADJ_FAC),COL_MAX_SR_ALT_mask,ROW_mask,'OPBSLD,64')
                           END IF
                        END IF
                        IF (BMOLCON(CURIYR) .GT. 0.0)THEN
                           ROW = 'CONOL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CONOL:' , URGNME(IRG)(6:7))
                           VAL = OLSH * BTUREQ
                           CALL DVAL(COL,ROW,VAL,COL_mask,ROW_mask,'OPBSLD,65')
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW,VAL,COL_ALT_mask,ROW_mask,'OPBSLD,66')
                    
                           IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                              VAL_MIN_SR = OLSH * BTUREQ_MIN
                              CALL DVAL(COL_MIN_SR,ROW,VAL_MIN_SR,COL_MIN_SR_mask,ROW_mask,'OPBSLD,67')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW,VAL_MIN_SR,COL_MIN_SR_ALT_mask,ROW_mask,'OPBSLD,68')
                              VAL_MAX_SR = OLSH * BTUREQ_MAX
                              CALL DVAL(COL_MAX_SR,ROW,VAL_MAX_SR,COL_MAX_SR_mask,ROW_mask,'OPBSLD,69')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW,VAL_MAX_SR,COL_MAX_SR_ALT_mask,ROW_mask,'OPBSLD,70')
                           END IF
                        END IF
                     END IF
                  END IF

                  IF (UPTTYP(IECP) .LE. NW_COAL) THEN    !coal
                     ROW = 'BTU'//EPFLCD(TFR)//UPLNTCD(IECP)//'XX'; call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),UPLNTCD(IECP)//'X',':X:') ! use first X to make 3-character plant type.
                     IF (ADJ_FAC .NE. 1.0 .AND. SHR_NOCCS .GT. 0.001) THEN
                        ROW_ALT = 'BTU'//EPFLCD(TFR)//UPLNTCD(I_NOCCS)//'XX'; call makmsk(ROW_ALT_mask,':BTU:',EPFLCD(TFR),UPLNTCD(I_NOCCS)//'X',':X:') ! use first X to make 3-character plant type.
                     END IF

!                    ACCOUNT FOR OUT-OF-STATE CARBON EMISSIONS IN CA

                     IF (URGNME(IRG)(1:2) .EQ. 'ca' .AND. FLRGCODE(TFR) .NE. 'CA')THEN
                        DO CAR = 1 , CO2_GRP
                           IF (CO2_OS_BY_RG(IRG,CAR) .GT. 0.0)THEN
                              VAL = BTUREQ * UFRCAR(IECP,CRG) * (1.0 / 2204.0)
                              ROWCARR = 'CARBON' // CO2_RG(CAR); call makmsk(ROWCARR_mask,':CARBON:' , CO2_RG(CAR))
                              CALL DVAL(COL,ROWCARR,VAL,COL_mask,ROWCARR_mask,'OPBSLD,71')
                              IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROWCARR,VAL,COL_ALT_mask,ROWCARR_mask,'OPBSLD,72')

                              IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                                 VAL_MIN_SR = BTUREQ_MIN * UFRCAR(IECP,CRG) * (1.0 / 2204.0)
                                 CALL DVAL(COL_MIN_SR,ROWCARR,VAL_MIN_SR,COL_MIN_SR_mask,ROWCARR_mask,'OPBSLD,73')
                                 IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROWCARR,VAL_MIN_SR,COL_MIN_SR_ALT_mask,ROWCARR_mask,'OPBSLD,74')

                                 VAL_MAX_SR = BTUREQ_MAX * UFRCAR(IECP,CRG) * (1.0 / 2204.0)
                                 CALL DVAL(COL_MAX_SR,ROWCARR,VAL_MAX_SR,COL_MAX_SR_mask,ROWCARR_mask,'OPBSLD,75')
                                 IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROWCARR,VAL_MAX_SR,COL_MAX_SR_ALT_mask,ROWCARR_mask,'OPBSLD,76')
                              END IF

                           END IF
                        END DO
                     END IF

!                    TRACK CAPTURED CO2 FOR CCATS MODEL

                     IF (UPPCEF(IECP) .GT. 0.0) THEN
                        IF (UG_45q(n) .EQ. 0 .OR. RUN45Q .EQ. 0) THEN
                           ROW_EOR_CO2 = 'ZCSRG' // CNSCOD // '_'; call makmsk(ROW_EOR_CO2_mask,':ZCSRG:' , CNSCOD, ':_:')
                        ELSE
                           ROW_EOR_CO2 = 'ZCSRS' // CNSCOD // '_'; call makmsk(ROW_EOR_CO2_mask,':ZCSRS:' , CNSCOD, ':_:')
                        ENDIF
                        VAL_CAP_CO2 = UPPCEF(IECP) * BTUREQ * ECLEL(CURIYR) * 0.001 * (44.0 / 12.0)
                        CALL DVAL(COL,ROW_EOR_CO2,VAL_CAP_CO2,COL_mask,ROW_EOR_CO2_mask,'OPBSLD,77')

                        ALT_VAL = UPPCEF_MIN(IECP) * BTUREQ * ECLEL(CURIYR) * 0.001 * (44.0 / 12.0)                        
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW_EOR_CO2,ALT_VAL,COL_ALT_mask,ROW_EOR_CO2_mask,'OPBSLD,78')

                        IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                           VAL_MIN_SR = UPPCEF(IECP) * BTUREQ_MIN * ECLEL(CURIYR) * 0.001 * (44.0 / 12.0)
                           CALL DVAL(COL_MIN_SR,ROW_EOR_CO2,VAL_MIN_SR,COL_MIN_SR_mask,ROW_EOR_CO2_mask,'OPBSLD,79')
                           ALT_VAL = UPPCEF_MIN(IECP) * BTUREQ_MIN * ECLEL(CURIYR) * 0.001 * (44.0 / 12.0)
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW_EOR_CO2,ALT_VAL,COL_MIN_SR_ALT_mask,ROW_EOR_CO2_mask,'OPBSLD,80')

                           VAL_MAX_SR = UPPCEF(IECP) * BTUREQ_MAX * ECLEL(CURIYR) * 0.001 * (44.0 / 12.0)
                           CALL DVAL(COL_MAX_SR,ROW_EOR_CO2,VAL_MAX_SR,COL_MAX_SR_mask,ROW_EOR_CO2_mask,'OPBSLD,81')

                           ALT_VAL = UPPCEF_MIN(IECP) * BTUREQ_MAX * ECLEL(CURIYR) * 0.001 * (44.0 / 12.0)
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW_EOR_CO2,ALT_VAL,COL_MAX_SR_ALT_mask,ROW_EOR_CO2_mask,'OPBSLD,82')
                        END IF

!                       WRITE(6,9317) CURIRUN, CURIYR+1989, CURITR, COL, ROW_EOR_CO2, IRG, TFR, IECP, BTUREQ, &
!                          VAL_CAP_CO2, UPPCEF(IECP), ECLEL(CURIYR)
!9317                   FORMAT(1X,"EFD_Captured_CO2",3(":",I4),2(":",A16),3(":",I4),4(":",F15.3))

                     END IF

!                    if (irg .eq. 20 .and. tfr .eq. 22)then
!                       if (curitr .eq. 1)write(6,1234) curiyr+1989,col,row,cap(1),gen,btureq,ufrcar(iecp,crg)
!1234                   format(1h ,'!oput',i4,1x,a8,1x,a8,8f10.3)
!                    end if

                  ELSEIF (IGS .EQ. 1) THEN            !gas
!                    NGSN = UNGSSN(IS)
                     NGSN = IS
                     ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//CHCOD(NGSN); call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),CHCOD(NGSN))
                     IF (ADJ_FAC .NE. 1.0 .AND. SHR_NOCCS .GT. 0.001) THEN
                        ROW_ALT = 'BTU'//EPFLCD(TFR)//EPPLCD(I_NOCCS_EFD)//CHCOD(NGSN); call makmsk(ROW_ALT_mask,':BTU:',EPFLCD(TFR),EPPLCD(I_NOCCS_EFD),CHCOD(NGSN))
                     END IF

!                    TRACK CAPTURED CO2 FROM NATURAL GAS UNITS FOR CCATS

                     IF (UPPCEF(IECP) .GT. 0.0) THEN
                        IF (UG_45q(n) .EQ. 0 .OR. RUN45Q .EQ. 0) THEN
                           ROW_EOR_CO2 = 'ZCSRG' // CNSCOD // '_'; call makmsk(ROW_EOR_CO2_mask,':ZCSRG:' , CNSCOD, ':_:')
                        ELSE
                           ROW_EOR_CO2 = 'ZCSRS' // CNSCOD // '_'; call makmsk(ROW_EOR_CO2_mask,':ZCSRS:' , CNSCOD, ':_:')
                        ENDIF
                        VAL_CAP_CO2 = UPPCEF(IECP) * BTUREQ * ENGEL(CURIYR) * 0.001 * (44.0 / 12.0)
                        CALL DVAL(COL,ROW_EOR_CO2,VAL_CAP_CO2,COL_mask,ROW_EOR_CO2_mask,'OPBSLD,83')

                        ALT_VAL = UPPCEF_MIN(IECP) * BTUREQ * ENGEL(CURIYR) * 0.001 * (44.0 / 12.0)
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW_EOR_CO2,ALT_VAL,COL_ALT_mask,ROW_EOR_CO2_mask,'OPBSLD,84')

                        IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                           VAL_MIN_SR = UPPCEF(IECP) * BTUREQ_MIN * ENGEL(CURIYR) * 0.001 * (44.0 / 12.0)
                           CALL DVAL(COL_MIN_SR,ROW_EOR_CO2,VAL_MIN_SR,COL_MIN_SR_mask,ROW_EOR_CO2_mask,'OPBSLD,85')

                           ALT_VAL = UPPCEF_MIN(IECP) * BTUREQ_MIN * ENGEL(CURIYR) * 0.001 * (44.0 / 12.0)
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW_EOR_CO2,ALT_VAL,COL_MIN_SR_ALT_mask,ROW_EOR_CO2_mask,'OPBSLD,86')

                           VAL_MAX_SR = UPPCEF(IECP) * BTUREQ_MAX * ENGEL(CURIYR) * 0.001 * (44.0 / 12.0)
                           CALL DVAL(COL_MAX_SR,ROW_EOR_CO2,VAL_MAX_SR,COL_MAX_SR_mask,ROW_EOR_CO2_mask,'OPBSLD,87')

                           ALT_VAL = UPPCEF_MIN(IECP) * BTUREQ_MAX * ENGEL(CURIYR) * 0.001 * (44.0 / 12.0)
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW_EOR_CO2,ALT_VAL,COL_MAX_SR_ALT_mask,ROW_EOR_CO2_mask,'OPBSLD,88')
                        END IF

!                       WRITE(6,9317) CURIRUN, CURIYR+1989, CURITR, COL, ROW_EOR_CO2, IRG, TFR, IECP, BTUREQ, &
!                          VAL_CAP_CO2, UPPCEF(IECP), ENGEL(CURIYR)

                     END IF
                  ELSEIF (IP .EQ. UIICE) THEN    ! HYDROGEN PLANT TYPE
 
                        ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//CHCOD(IS); call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),'X')
                        IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
                           WRITE(unit_num_ephrts_debug_file,*) "efd ice btu row added: reg ", TFR, ", row ", ROW
                        END IF
                        !IF (ADJ_FAC .NE. 1.0 .AND. SHR_NOCCS .GT. 0.001) THEN
                        !   ROW_ALT = 'BTU'//EPFLCD(TFR)//EPPLCD(I_NOCCS_EFD)//'X'; call makmsk(ROW_ALT_mask,':BTU:',EPFLCD(TFR),EPPLCD(I_NOCCS_EFD),'X')
                        !END IF
                  ELSEIF (IP .NE. UICNU .AND. IP .NE. UIANC .AND. IP .NE. UISMR) THEN    !oil or biomass
                     ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//'X'; call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),'X')
                     IF (ADJ_FAC .NE. 1.0 .AND. SHR_NOCCS .GT. 0.001) THEN
                        ROW_ALT = 'BTU'//EPFLCD(TFR)//EPPLCD(I_NOCCS_EFD)//'X'; call makmsk(ROW_ALT_mask,':BTU:',EPFLCD(TFR),EPPLCD(I_NOCCS_EFD),'X')
                     END IF
                  ELSE                  !nuclear
                     ROW = 'BTU'//'X'//EPPLCD(IP)//'X'; call makmsk(ROW_mask,':BTU:','X',EPPLCD(IP),'X')
                     IF (ADJ_FAC .NE. 1.0 .AND. SHR_NOCCS .GT. 0.001) THEN
                        ROW_ALT = 'BTU'//'X'//EPPLCD(IP)//'X'; call makmsk(ROW_ALT_mask,':BTU:','X',EPPLCD(IP),'X')
                     END IF
                  ENDIF
                  CALL DVAL(COL,ROW,BTUREQ,COL_mask,ROW_mask,'OPBSLD,89')
                  IF (ADJ_FAC .NE. 1.0) THEN
                     CALL DVAL(COL_ALT,ROW,BTUREQ*SHR_CCS,COL_ALT_mask,ROW_mask,'OPBSLD,90.1')
                     CALL DVAL(COL_ALT,ROW_ALT,BTUREQ*SHR_NOCCS,COL_ALT_mask,ROW_ALT_mask,'OPBSLD,90.2')
                  END IF

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     CALL DVAL(COL_MIN_SR,ROW,BTUREQ_MIN,COL_MIN_SR_mask,ROW_mask,'OPBSLD,91')
                     IF (ADJ_FAC .NE. 1.0) THEN
                        CALL DVAL(COL_MIN_SR_ALT,ROW,BTUREQ_MIN*SHR_CCS,COL_MIN_SR_ALT_mask,ROW_mask,'OPBSLD,92.1')
                        CALL DVAL(COL_MIN_SR_ALT,ROW_ALT,BTUREQ_MIN*SHR_NOCCS,COL_MIN_SR_ALT_mask,ROW_ALT_mask,'OPBSLD,92.2')
                     END IF

                     CALL DVAL(COL_MAX_SR,ROW,BTUREQ_MAX,COL_MAX_SR_mask,ROW_mask,'OPBSLD,93')
                     IF (ADJ_FAC .NE. 1.0) THEN
                        CALL DVAL(COL_MAX_SR_ALT,ROW,BTUREQ_MAX*SHR_CCS,COL_MAX_SR_ALT_mask,ROW_mask,'OPBSLD,94.1')
                        CALL DVAL(COL_MAX_SR_ALT,ROW_ALT,BTUREQ_MAX*SHR_NOCCS,COL_MAX_SR_ALT_mask,ROW_ALT_mask,'OPBSLD,94.2')
                     END IF
                  END IF

               ENDIF ! Fossil
!
!              FUEL REGION INTENSITY STANDARD
!
!              ACCUMULATE TOTAL CO2 EMISSIONS

               IF (CO2LB .GT. 0.0)THEN
                  IF (TFR .GT. 0)THEN

!                    IF (CO2LB .GT. 0.0)THEN

!                    ROW_GPS = 'CO2TFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':CO2TFR:' , FLRGCODE(TFR))
                     ROW_GPSN = 'CO2TNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2TNR:' , URGNME(IRG)(6:7))
                     VAL = GEN * 0.001 * CO2LB * CO2ADJ * 0.001
                     ALT_VAL = GEN * 0.001 * ALT_CO2LB * CO2ADJ * 0.001

!                    if (irg .eq. 11 .and. curitr .eq. 1)write(6,4433) curiyr+1989,col,gen,co2lb,co2adj,co2_stdrn(irg,curiyr),val
!4433                format(1h ,'!co2pl',i4,a10,5f12.4)

                     CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPBSLD,95')
                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW_GPSN,ALT_VAL,COL_ALT_mask,ROW_GPSN_mask,'OPBSLD,96')
                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        VAL_MIN_SR = GEN_MIN * 0.001 * CO2LB * CO2ADJ * 0.001
                        CALL DVAL(COL_MIN_SR,ROW_GPSN,VAL_MIN_SR,COL_MIN_SR_mask,ROW_GPSN_mask,'OPBSLD,97')

                        ALT_VAL= GEN_MIN * 0.001 * ALT_CO2LB * CO2ADJ * 0.001                        
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW_GPSN,ALT_VAL,COL_MIN_SR_ALT_mask,ROW_GPSN_mask,'OPBSLD,98')

                        VAL_MAX_SR = GEN_MAX * 0.001 * CO2LB * CO2ADJ * 0.001
                        CALL DVAL(COL_MAX_SR,ROW_GPSN,VAL_MAX_SR,COL_MAX_SR_mask,ROW_GPSN_mask,'OPBSLD,99')

                        ALT_VAL = GEN_MAX * 0.001 * ALT_CO2LB * CO2ADJ * 0.001
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW_GPSN,ALT_VAL,COL_MAX_SR_ALT_mask,ROW_GPSN_mask,'OPBSLD,100')
                     END IF
                     IF (CO2_PLTSW(IECP) .GT. 0.0)THEN
!                       ROW_GPS = 'CO2QFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':CO2QFR:' , FLRGCODE(TFR))
                        ROW_GPSN = 'CO2QNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2QNR:' , URGNME(IRG)(6:7))
                        VAL = GEN * 0.001 * CO2LB * 0.001 * CO2_PLTSW(IECP)
                        CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPBSLD,101')

                        ALT_VAL = GEN * 0.001 * ALT_CO2LB * 0.001 * CO2_PLTSW(IECP)                       
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW_GPSN,ALT_VAL,COL_ALT_mask,ROW_GPSN_mask,'OPBSLD,102')
                        IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                           VAL_MIN_SR = GEN_MIN * 0.001 * CO2LB * 0.001 * CO2_PLTSW(IECP)
                           CALL DVAL(COL_MIN_SR,ROW_GPSN,VAL_MIN_SR,COL_MIN_SR_mask,ROW_GPSN_mask,'OPBSLD,103')

                           ALT_VAL = GEN_MIN * 0.001 * ALT_CO2LB * 0.001 * CO2_PLTSW(IECP)
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW_GPSN,ALT_VAL,COL_MIN_SR_ALT_mask,ROW_GPSN_mask,'OPBSLD,104')

                           VAL_MAX_SR = GEN_MAX * 0.001 * CO2LB * 0.001 * CO2_PLTSW(IECP)
                           CALL DVAL(COL_MAX_SR,ROW_GPSN,VAL_MAX_SR,COL_MAX_SR_mask,ROW_GPSN_mask,'OPBSLD,105')

                           ALT_VAL = GEN_MAX * 0.001 * ALT_CO2LB * 0.001 * CO2_PLTSW(IECP)                           
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW_GPSN,ALT_VAL,COL_MAX_SR_ALT_mask,ROW_GPSN_mask,'OPBSLD,106')
                        END IF
                     END IF

!                    END IF

                  ELSE

                     write(6,3344) curiyr+1989,irg,uplntcd(iecp),col
 3344                format(1h ,'NO FR FOS',i4,i3,a3,a10)

                  END IF
               END IF
               IF (CO2_PLTSW(IECP) .GT. 0.0)THEN

!                 if (irg .eq. 1 .and. curitr .eq. 1)write(6,3232) curiyr+1989,ist,tfr,co2_stdrf(tfr,curiyr),uplntcd(iecp),htrt,ogcar,clcar,co2lb,col,gen*0.001,col_min_sr,gen_min*0.001,col_max_sr,gen_max*0.001
!3232             format(1h ,'!gpsstd',i4,i3,i3,f10.1,a3,4f10.3,3(a10,f10.1))

!                 IF (TFR .GT. 0)THEN

!                 FOSSIL -- HAVE FUEL REGION IDENTITY

                  IF (IECP .NE. WICN .AND. IECP .NE. WIAN .AND. IECP .NE. WISM)THEN
                     IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                        ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                        VAL = GEN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                        CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPBSLD,107')

                        VAL = GEN * 0.001 * (ALT_CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW_GPSN,VAL,COL_ALT_mask,ROW_GPSN_mask,'OPBSLD,108')
                     END IF
!                    ROW_GPS = 'GENQFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':GENQFR:' , FLRGCODE(TFR))
                     ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                     VAL = GEN * 0.001 * CO2_PLTSW(IECP)
                     CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPBSLD,109')

                     IF (ADJ_FAC .NE. 1.0) THEN
                        VAL = GEN * ADJ_FAC * 0.001 * CO2_PLTSW(IECP)
                        CALL DVAL(COL_ALT,ROW_GPSN,VAL,COL_ALT_mask,ROW_GPSN_mask,'OPBSLD,110')
                     END IF
 
                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                           ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                           VAL_MIN_SR = GEN_MIN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                           CALL DVAL(COL_MIN_SR,ROW_GPSN,VAL_MIN_SR,COL_MIN_SR_mask,ROW_GPSN_mask,'OPBSLD,111')
                           IF (ADJ_FAC .NE. 1.0) THEN
                              VAL_MIN_SR = GEN_MIN * 0.001 * (ALT_CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                              CALL DVAL(COL_MIN_SR_ALT,ROW_GPSN,VAL_MIN_SR,COL_MIN_SR_ALT_mask,ROW_GPSN_mask,'OPBSLD,112')
                           END IF

                           VAL_MAX_SR = GEN_MAX * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                           CALL DVAL(COL_MAX_SR,ROW_GPSN,VAL_MAX_SR,COL_MAX_SR_mask,ROW_GPSN_mask,'OPBSLD,113')
                           IF (ADJ_FAC .NE. 1.0) THEN
                              VAL_MAX_SR = GEN_MAX * 0.001 * (ALT_CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                              CALL DVAL(COL_MAX_SR_ALT,ROW_GPSN,VAL_MAX_SR,COL_MAX_SR_ALT_mask,ROW_GPSN_mask,'OPBSLD,114')
                           END IF
                        END IF
!                       ROW_GPS = 'GENQFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':GENQFR:' , FLRGCODE(TFR))
                        ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                        VAL_MIN_SR = GEN_MIN * 0.001 * CO2_PLTSW(IECP)
                        CALL DVAL(COL_MIN_SR,ROW_GPSN,VAL_MIN_SR,COL_MIN_SR_mask,ROW_GPSN_mask,'OPBSLD,115')

                        IF (ADJ_FAC .NE. 1.0) THEN
                           VAL_MIN_SR = GEN_MIN * ADJ_FAC * 0.001 * CO2_PLTSW(IECP)
                           CALL DVAL(COL_MIN_SR_ALT,ROW_GPSN,VAL_MIN_SR,COL_MIN_SR_ALT_mask,ROW_GPSN_mask,'OPBSLD,116')
                        END IF
 
                        VAL_MAX_SR = GEN_MAX * 0.001 * CO2_PLTSW(IECP)
                        CALL DVAL(COL_MAX_SR,ROW_GPSN,VAL_MAX_SR,COL_MAX_SR_mask,ROW_GPSN_mask,'OPBSLD,117')

                        IF (ADJ_FAC .NE. 1.0) THEN
                           VAL_MAX_SR = GEN_MAX * ADJ_FAC * 0.001 * CO2_PLTSW(IECP)
                           CALL DVAL(COL_MAX_SR_ALT,ROW_GPSN,VAL_MAX_SR,COL_MAX_SR_ALT_mask,ROW_GPSN_mask,'OPBSLD,118')
                        END IF
                     END IF

!                    if (tfr .eq. 2 .and. curitr .gt. 1)write(6,4567) curiyr+1989,irg,iecp,uplntcd(iecp),co2_pltsw(iecp),htrt,co2lb,co2_stdrf(tfr,curiyr),  &
!                       col,gen*0.001, GEN * 0.001 * (CO2LB - CO2_STDRF(TFR,CURIYR)) * CO2_PLTSW(IECP),  &
!                       col_min_sr,gen_min*0.001, GEN_MIN * 0.001 * (CO2LB - CO2_STDRF(TFR,CURIYR)) * CO2_PLTSW(IECP),  &
!                       col_max_sr,gen_max*0.001, GEN_MAX * 0.001 * (CO2LB - CO2_STDRF(TFR,CURIYR)) * CO2_PLTSW(IECP)
!4567                format(1h ,'!co2fos',i4,i3,i3,a3,f4.1,f7.0,f6.0,f6.0,3(a10,f5.1,f7.1))

                  ELSE

!                    NUCLEAR AT EMM REGION

                     IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                        ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                        VAL = GEN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                        CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPBSLD,119')
                     END IF
                     ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                     VAL = GEN * 0.001 * CO2_PLTSW(IECP)
                     CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPBSLD,120')

                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                           ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                           VAL_MIN_SR = GEN_MIN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                           CALL DVAL(COL_MIN_SR,ROW_GPSN,VAL_MIN_SR,COL_MIN_SR_mask,ROW_GPSN_mask,'OPBSLD,121')

                           VAL_MAX_SR = GEN_MAX * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                           CALL DVAL(COL_MAX_SR,ROW_GPSN,VAL_MAX_SR,COL_MAX_SR_mask,ROW_GPSN_mask,'OPBSLD,122')
                        END IF
                        ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                        VAL_MIN_SR = GEN_MIN * 0.001 * CO2_PLTSW(IECP)
                        CALL DVAL(COL_MIN_SR,ROW_GPSN,VAL_MIN_SR,COL_MIN_SR_mask,ROW_GPSN_mask,'OPBSLD,123')

                        VAL_MAX_SR = GEN_MAX * 0.001 * CO2_PLTSW(IECP)
                        CALL DVAL(COL_MAX_SR,ROW_GPSN,VAL_MAX_SR,COL_MAX_SR_mask,ROW_GPSN_mask,'OPBSLD,124')
                     END IF
                  END IF
               END IF

!              NOX constraint for dispatchables

               IF (IP .LE. EFD_D_DSP) THEN
                  DO INOX = 1, NOX_GRP
                     WRITE(NOXCODE,'(I1)') INOX
                     IF (EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
                        ROW_NOX = 'ELNOX' // NOXCODE // UPLNTCD(IECP); call makmsk(ROW_NOX_mask,':ELNOX:' , NOXCODE , UPLNTCD(IECP))
                     ELSE
                        ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE)
                     END IF
                     NOXEM = GEN * HTRT * HTRT_ADJ * UG_NOXC(IS,INOX,N) * 0.0005 * 0.001
                     IF (NOXEM .GT. 0.001) THEN
                        IF (EMRFNA(INOX,CURIYR) .GT. 0.0) TST_NOX(IECP,INOX) = 1
                        CALL DVAL(COL,ROW_NOX,NOXEM,COL_mask,ROW_NOX_mask,'OPBSLD,125')
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW_NOX,NOXEM,COL_ALT_mask,ROW_NOX_mask,'OPBSLD,126')
                     END IF
                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        NOXEM = GEN_MIN * HTRT * HTRT_ADJ_MIN * UG_NOXC(IS,INOX,N) * 0.0005 * 0.001
                        IF (NOXEM .GT. 0.001) THEN
                           CALL DVAL(COL_MIN_SR,ROW_NOX,NOXEM,COL_MIN_SR_mask,ROW_NOX_mask,'OPBSLD,127')
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW_NOX,NOXEM,COL_MIN_SR_ALT_mask,ROW_NOX_mask,'OPBSLD,128')
                        END IF
                        NOXEM = GEN_MAX * HTRT * HTRT_ADJ_MAX * UG_NOXC(IS,INOX,N) * 0.0005 * 0.001
                        IF (NOXEM .GT. 0.001) THEN
                           CALL DVAL(COL_MAX_SR,ROW_NOX,NOXEM,COL_MAX_SR_mask,ROW_NOX_mask,'OPBSLD,129')
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW_NOX,NOXEM,COL_MAX_SR_ALT_mask,ROW_NOX_mask,'OPBSLD,130')
                        END IF
                     END IF
                  ENDDO
               END IF
!
!              MUSTRUN CONSTRAINT
!
               IF (UG_MRUN(N) .GT. 0) THEN

!                 ROW_MR = 'MR' // URGNME(IRG)(1:4) // UPLNTCD(IECP)

                  ROW_MR = 'MR' // NUM // 'X'; call makmsk(ROW_MR_mask,':MR:' , NUM , ':X:')
                  CALL DVAL(COL,ROW_MR,0.1*GEN,COL_mask,ROW_MR_mask,'OPBSLD,131')
                  IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW_MR,DBLE(0.1*GEN*ADJ_FAC),COL_ALT_mask,ROW_MR_mask,'OPBSLD,132')

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     CALL DVAL(COL_MIN_SR,ROW_MR,0.1*GEN_MIN,COL_MIN_SR_mask,ROW_MR_mask,'OPBSLD,133')
                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW_MR,DBLE(0.1*GEN_MIN*ADJ_FAC),COL_MIN_SR_ALT_mask,ROW_MR_mask,'OPBSLD,134')

                     CALL DVAL(COL_MAX_SR,ROW_MR,0.1*GEN_MAX,COL_MAX_SR_mask,ROW_MR_mask,'OPBSLD,135')
                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW_MR,DBLE(0.1*GEN_MAX*ADJ_FAC),COL_MAX_SR_ALT_mask,ROW_MR_mask,'OPBSLD,136')
                  END IF

               END IF

!              RPS constraint if in place

!              IF (UPRNWBND(CURIYR) .GT. 0.005 .AND. UPRNWSHR(IECP) .NE. 0.0) THEN
!                 ROWRPS = 'RPSREQXX'
!                 IF (UPRNWSHR(IECP) .GT. 0.0) THEN
!                    VAL = GEN * UPRNWSHR(IECP) * 0.001
!                 ELSE
!                    VAL = GEN * UPRNWBND(CURIYR) * 0.001
!                    IF (UPRNWCAS .EQ. 3) VAL = VAL / (1.0 + EQTDLS * ULOSSADJ(CURIYR))
!                 ENDIF
!                 CALL DVAL(COL,ROWRPS,DBLE(VAL))
!              ENDIF ! RPS

               IF (USW_EFDRPS .GT. 0 .AND. UPRNWCAS .GT. 0 .AND. UPRNWBND(CURIYR) .GT. 0.005)THEN
                  VAL = GEN * UPRNWSHR(IECP) * 0.001
                  VALCF = DBLE(0.0)

                  VAL_MIN_SR = GEN_MIN * UPRNWSHR(IECP) * 0.001
                  VALCF_MIN = DBLE(0.0)

                  VAL_MAX_SR = GEN_MAX * UPRNWSHR(IECP) * 0.001
                  VALCF_MAX = DBLE(0.0)

                  IF (UPTTYP(IECP) .LE. NW_COAL .AND. USW_ECPCF .EQ. 1) THEN    !adjust RPS for cofiring
                     VAL = VAL * DBLE(1.0 - UPWDCFR(IECP,EPCLMP(TFR)))
                     VALCF = DBLE(UPRNWSHR(WIWD) * UPWDCFR(IECP,EPCLMP(TFR)) * GEN * 0.001)

                     VAL_MIN_SR = VAL_MIN_SR * DBLE(1.0 - UPWDCFR(IECP,EPCLMP(TFR)))
                     VALCF_MIN = DBLE(UPRNWSHR(WIWD) * UPWDCFR(IECP,EPCLMP(TFR)) * GEN_MIN * 0.001)

                     VAL_MAX_SR = VAL_MAX_SR * DBLE(1.0 - UPWDCFR(IECP,EPCLMP(TFR)))
                     VALCF_MAX = DBLE(UPRNWSHR(WIWD) * UPWDCFR(IECP,EPCLMP(TFR)) * GEN_MAX * 0.001)
                  END IF

!p                IF (VAL .GT. DBLE(0.0)) THEN
!p                   ROWRPS = 'RPSPGN' // UPLNTCD(IECP)
!p                   CALL DVAL(COL,ROWRPS,VAL)
!p                END IF
!p                IF (VALCF .GT. DBLE(0.0)) THEN
!p                   ROWRPS = 'RPSPGNCF'
!p                   CALL DVAL(COL,ROWRPS,VALCF)
!p                END IF

                  VAL = VAL + VALCF
                  IF (VAL .GT. DBLE(0.0)) THEN
                     ROWRPS = 'RPSREQ' // UPRGCD(IRG); call makmsk(ROWRPS_mask,':RPSREQ:' , UPRGCD(IRG))
                     CALL DVAL(COL,ROWRPS,VAL,COL_mask,ROWRPS_mask,'OPBSLD,137')
                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROWRPS,DBLE(VAL*ADJ_FAC),COL_ALT_mask,ROWRPS_mask,'OPBSLD,138')
                  END IF

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     VAL_MIN_SR = VAL_MIN_SR + VALCF_MIN
                     IF (VAL_MIN_SR .GT. DBLE(0.0)) THEN
                        ROWRPS = 'RPSREQ' // UPRGCD(IRG); call makmsk(ROWRPS_mask,':RPSREQ:' , UPRGCD(IRG))
                        CALL DVAL(COL_MIN_SR,ROWRPS,VAL_MIN_SR,COL_MIN_SR_mask,ROWRPS_mask,'OPBSLD,139')
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROWRPS,DBLE(VAL_MIN_SR*ADJ_FAC),COL_MIN_SR_ALT_mask,ROWRPS_mask,'OPBSLD,140')
                     END IF

                     VAL_MAX_SR = VAL_MAX_SR + VALCF_MAX
                     IF (VAL_MAX_SR .GT. DBLE(0.0)) THEN
                        ROWRPS = 'RPSREQ' // UPRGCD(IRG); call makmsk(ROWRPS_mask,':RPSREQ:' , UPRGCD(IRG))
                        CALL DVAL(COL_MAX_SR,ROWRPS,VAL_MAX_SR,COL_MAX_SR_mask,ROWRPS_mask,'OPBSLD,141')
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROWRPS,DBLE(VAL_MAX_SR*ADJ_FAC),COL_MAX_SR_ALT_mask,ROWRPS_mask,'OPBSLD,142')
                     END IF
                  END IF


!n                IF (UPRNWSHR(IECP) .LT. 1.0) THEN
!n                   VAL = GEN * (1.0 - UPRNWSHR(IECP)) * 0.001
!n                   IF (UPTTYP(IECP) .LE. NW_COAL .AND. USW_ECPCF .EQ. 1) THEN    !adjust RPS for cofiring
!n                      VAL = VAL * (1.0 - UPWDCFR(IECP,EPCLMP(TFR)))
!n                   END IF
!n                   ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!n                   CALL DVAL(COL,ROWRPS,VAL)
!n                   ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!n                   CALL DVAL(COL,ROWRPS,VAL)
!n                ENDIF ! RPS
!                 IF (UPRNWEXG(IECP) .GT. 0.0 .AND. ULVINT(IGRP) .EQ. 1) THEN
!                    VAL = GEN * UPRNWEXG(IECP) * 0.001
!                    ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!                    CALL DVAL(COL,ROWRPS,VAL)
!                    ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!                    CALL DVAL(COL,ROWRPS,VAL)
!                 ENDIF ! RPS

               ENDIF ! RPS

!              Biomass generation row for RPS

               IF (IP .EQ. UIBMS .OR. IP .EQ. UIBIG)THEN
                  GENBMS = GEN * 0.001  !  Billion Kwh per GW
                  ROWBMS = 'G'//URGNME(IRG)(1:4)//EPPLCD(IP); call makmsk(ROWBMS_mask,':G:',URGNME(IRG)(1:4),EPPLCD(IP))
                  CALL DVAL(COL,ROWBMS,GENBMS,COL_mask,ROWBMS_mask,'OPBSLD,143')

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     VAL_MIN_SR = GEN_MIN * 0.001  !  Billion Kwh per GW
                     CALL DVAL(COL_MIN_SR,ROWBMS,VAL_MIN_SR,COL_MIN_SR_mask,ROWBMS_mask,'OPBSLD,144')

                     VAL_MAX_SR = GEN_MAX * 0.001  !  Billion Kwh per GW
                     CALL DVAL(COL_MAX_SR,ROWBMS,VAL_MAX_SR,COL_MAX_SR_mask,ROWBMS_mask,'OPBSLD,145')
                  END IF
               ENDIF
!
!              GRID RESILIENCE ROWS
!
!              if (curitr .eq. 1 .and. irg .eq. 1)write(6,4445) curiyr + 1989, irg, uplntcd(iecp), grd_casn,grd_srcc(grd_casn), col,  &
!                 GEN * 0.001 , GRD_RATSA(IECP,IRG,GRD_CASN) , GRD_TGTS(CURIYR,IRG), val
!4445          format(1h ,'!grddsp',i4,i3,a3,i3,a3,a10,5f10.4)

               DO IGR = 1 , GRD_CASN
                  IF (GRD_RATSA(IECP,IRG,IGR) .GE. 0.0)THEN
                     ROW_GRD = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                     IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                        VAL = GEN * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                     ELSE
                        VAL = GEN * 0.001 * GRD_RATSA(IECP,IRG,IGR)
                     END IF

!                    if (curitr .eq. 1 .and. irg .eq. 1)write(6,4455) curiyr + 1989, irg, uplntcd(iecp),grd_srcc(igr),row_grd, col,  &
!                       GEN * 0.001 , GRD_RATSA(IECP,IRG,IGR) , GRD_TGTS(CURIYR,IRG), val
!4455                format(1h ,'!grddsp',i4,i3,a3,a3,a10,a10,5f10.4)

                     IF (VAL .NE. DBLE(0.0)) THEN
                        CALL DVAL(COL,ROW_GRD,VAL,COL_mask,ROW_GRD_mask,'OPBSLD,146')
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW_GRD,DBLE(VAL * ADJ_FAC),COL_ALT_mask,ROW_GRD_mask,'OPBSLD,146.1')
                     END IF

                     ROW_GRD = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                     VAL = GEN * 0.001
                     CALL DVAL(COL,ROW_GRD,VAL,COL_mask,ROW_GRD_mask,'OPBSLD,147')

                     IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_ALT,ROW_GRD,DBLE(VAL * ADJ_FAC),COL_ALT_mask,ROW_GRD_mask,'OPBSLD,147.1')

!                    if (curitr .eq. 1 .and. irg .eq. 1)write(6,4455) curiyr + 1989, irg, uplntcd(iecp), GRD_SRCC(IGR) ,row_grd, col,  &
!                       GEN * 0.001 , GRD_RATSA(IECP,IRG,IGR) , GRD_TGTS(CURIYR,IRG), val

                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        ROW_GRD = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                        IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                           VAL_MIN_SR = GEN_MIN * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                        ELSE
                           VAL_MIN_SR = GEN_MIN * 0.001 * GRD_RATSA(IECP,IRG,IGR)
                        END IF
                        IF (VAL_MIN_SR .NE. DBLE(0.0)) THEN
                           CALL DVAL(COL_MIN_SR,ROW_GRD,VAL_MIN_SR,COL_MIN_SR_mask,ROW_GRD_mask,'OPBSLD,148')
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW_GRD,DBLE(VAL_MIN_SR * ADJ_FAC),COL_MIN_SR_ALT_mask,ROW_GRD_mask,'OPBSLD,148.1')
                        END IF

                        IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                           VAL_MAX_SR = GEN_MAX * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                        ELSE
                           VAL_MAX_SR = GEN_MAX * 0.001 * GRD_RATSA(IECP,IRG,IGR)
                        END IF
                        IF (VAL_MAX_SR .NE. DBLE(0.0)) THEN
                           CALL DVAL(COL_MAX_SR,ROW_GRD,VAL_MAX_SR,COL_MAX_SR_mask,ROW_GRD_mask,'OPBSLD,149')
                           IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW_GRD,DBLE(VAL_MAX_SR * ADJ_FAC),COL_MAX_SR_ALT_mask,ROW_GRD_mask,'OPBSLD,149.1')
                        END IF

                        ROW_GRD = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                        VAL_MIN_SR = GEN_MIN * 0.001
                        CALL DVAL(COL_MIN_SR,ROW_GRD,VAL_MIN_SR,COL_MIN_SR_mask,ROW_GRD_mask,'OPBSLD,150')
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MIN_SR_ALT,ROW_GRD,DBLE(VAL_MIN_SR * ADJ_FAC),COL_MIN_SR_ALT_mask,ROW_GRD_mask,'OPBSLD,150.1')

                        VAL_MAX_SR = GEN_MAX * 0.001
                        CALL DVAL(COL_MAX_SR,ROW_GRD,VAL_MAX_SR,COL_MAX_SR_mask,ROW_GRD_mask,'OPBSLD,151')
                        IF (ADJ_FAC .NE. 1.0) CALL DVAL(COL_MAX_SR_ALT,ROW_GRD,DBLE(VAL_MAX_SR * ADJ_FAC),COL_MAX_SR_ALT_mask,ROW_GRD_mask,'OPBSLD,151.1')
                     END IF
                  END IF
               END DO
            ENDIF   !upper
         ENDDO   !MODE
      ENDDO    !SEASON
!
!     RESET 111d SWITCH FOR CT
!
      IF (IECP .EQ. WIET .OR. IECP .EQ. WICT .OR. IECP .EQ. WIAT)CO2_PLTSW(IECP) = CO2_PLTET
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
         CLOSE(unit_num_ephrts_debug_file)
      END IF
      RETURN
      END

!
!     This subroutine sets up operates for peaking plants
!       It assumes they can run in each time slice independently
!

      SUBROUTINE OPPEAK(N)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'fuelin'
      include'dispuse'
      include'plntctl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'elcntl'
      include'emission'
      include'emeblk'
      include'dispett'
      include'uecpout'
      include'cdsparms'
      include'dispcrv'
      include'eusprc'
      include'edbdef'
      include'ecp_nuc'
      include'csapr'
      include'emmemis'
      include'emm_aimms'

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IFR,IGS,IEFD,IRET,IGR
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,INOX,FSL
      REAL CF(EFD_D_MVS,EFD_D_MSP),VOM,HTRT,CAP(EFD_D_MSP),HRVAL,MAXCF(EFD_D_MSP),PTC,RPSCST,RPSU,RPSR,GPSCST
      REAL*8 DRAT,GEN,OBJVAL,BTUREQ,MRCAP,NOXEM,GEN_MIN,DRAT_ORG,DRAT_MIN,VAL_SR,BTUREQ_MIN,NOXEM_MIN,VAL_MIN,VALUE,VALSH,VAL
      CHARACTER*1 FSLCD,NOXCODE
      CHARACTER*1 REGCD(MNUMNR)
      CHARACTER*5 NUM
      CHARACTER*16 COL,ROW,LOAD,ROW_NOX,ROWRPS,ROW_MR,COL_MIN,ROW_SR,ROW_MP,COL_MP,ROW_GPS,ROW_GPSN,ROW_GRD
      INTEGER*4 IST,STA
      REAL*4 GSPR,OLPR,GASSH,OLSH,TOT,OGCAR,CO2LB,CO2ADJ
      INTEGER*4  FUEL_RGN, ECPt
      REAL*8 Load_Level, HTRT_ADJ, HTRT_ADJ_MIN, HTRT_ADJ_MAX, Target_EFF, Max_EFF, SP_HOURS
      CHARACTER*12 FROM_LABEL
      character*30 ROW_GRD_mask
!
!      COMMON /GRDSRC/ GRD_CASN,GRD_SRCN,GRD_SRCC
!      INTEGER GRD_CASN                                       ! Number of grid resilience sources 
!      CHARACTER*15 GRD_SRCN(MX_GRDSRC)                       ! Grid resilience source names
!      CHARACTER*1  GRD_SRCC(MX_GRDSRC)                       ! Grid resilience source codes

      efdsub='OPPEAK'
      
      DO N = 1, MNUMNR
        REGCD(N) = CHCOD(N)
      ENDDO

      RPSCST = 0.0

      IRG = UG_EMM_RG(N)
      IP = UG_EFDt(N)
      IEFD = IP
      IECP = UG_ECPt(N)
      IGRP = EFD_GRPS_F(N)
      TFR = UG_FL_RG(N)
      IST = UG_STATE(N)
      VOM = UG_OMR(N)
      PTC = UG_GSUB(N)

      DO IS = 1, EENSP
         CAP(IS) = UG_CAP(IS,N)
         MAXCF(IS) = UG_SCF(IS,N)
         HTRT = UG_HTRT(IS,N)

         IF (FCRL .EQ. 1) THEN
            write(UF_DBG,4263) CURIYR,CURITR,IS,N,IGRP,IP,TFR,VOM,PTC,HTRT,CAP(1),MAXCF(1)
 4263       format(1x,'OPPEAK ',7(":",I6),5(":",F10.2))
         ENDIF
      ENDDO
!
!     STORE 111d SWITCH FOR MASS OR RATE STANDARDS FOR REGION
!
      CO2_PLTSW(IECP) = CO2_PLTRG(IECP,IRG)

      IGS = 0
      DO IFL = 1, EFD_D_FPP
         if (WFLTP(IP,IFL) .NE. 0) THEN
            IF (UIGAS(WFLTP(IP,IFL)) .EQ. 1) IGS = 1
         endif
      ENDDO

      WRITE(NUM,'(I5.5)') IGRP

!     IDENTIFY PLANNED MAINTENANCE ROW WHICH BELONGS TO THIS UNIT

      IF (SR_CREDIT(IECP) .GT. 0.0) THEN
         ROW_MP = 'MP'//REGCD(IRG)//EPPLCD(IEFD)//'XX'; call makmsk(ROW_MP_mask,':MP:',REGCD(IRG),EPPLCD(IEFD),':XX:')
      END IF

!     loop over time season and slices to determine modes

      DO IS = 1, EENSP

         SP_HOURS = 0.0
         DO IVSL = 1 , ELNVCT(IS)
            SP_HOURS = SP_HOURS + ELWDTH(IVSL,IS)
         END DO

         HTRT = UG_HTRT(IS,N)

         IF (SR_CREDIT(IECP) .GT. 0.0) THEN
            COL_MP = 'MP'//NUM//CHCOD(IS); call makmsk(COL_MP_mask,':MP:',NUM,CHCOD(IS))
            CALL DVAL(COL_MP,ROW_MP,DBLE(EETIME(IS)/1000.0),COL_MP_mask,ROW_MP_mask,'OPPEAK,1')
            CALL DBND(EFDBND,COL_MP,0.0D0,DBLE(CAP(IS)*0.001),COL_MP_mask,'OPPEAK,2')
         END IF
         DO IVSL = 1 , ELNVCT(IS)
            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)
            FSL = EFD_Slice_ID(GRP,SEG)
            FSLCD = CHAR(64+FSL)
            COL = 'P'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_mask,':P:',NUM,CHCOD(IS),FSLCD)

            IF (SR_CREDIT(IECP) .GT. 0.0) THEN
               COL_MIN = 'Q'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_MIN_mask,':Q:',NUM,CHCOD(IS),FSLCD)
            END IF

            LOAD = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(LOAD_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)

            IF (IP .LE. EFD_D_DSP) THEN
               CALL ELDRAT(IS,IRG,N,IVSL,DRAT)
            ELSE
               DRAT = MAXCF(IS)
            ENDIF

            CALL ELDRAT(IS,IRG,N,IVSL,DRAT)
            IF (DRAT .LT. 0.01) DRAT = 0.01

            IF (SR_CREDIT(IECP) .GT. 0.0) THEN

               ROW_SR = 'SR' // URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(ROW_SR_mask,':SR:' , URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)

               DRAT_ORG = DRAT
               DRAT = (1.0 - UG_FOR(N))
               VAL_SR = (1.0 - DRAT) * SR_CREDIT(IECP)
               GEN = DRAT * ELWDTH(IVSL,IS)
               CALL DVAL(COL,LOAD,DBLE(0.1 * GEN),COL_mask,LOAD_mask,'OPPEAK,3')
               CALL DVAL(COL,ROW_SR,VAL_SR,COL_mask,ROW_SR_mask,'OPPEAK,4')

               DRAT_MIN = SR_MIN_CF(IECP)
               VAL_SR = (1.0 - DRAT_MIN) * SR_CREDIT(IECP)
               GEN_MIN = DRAT_MIN * ELWDTH(IVSL,IS)
               CALL DVAL(COL_MIN,LOAD,DBLE(0.1 * GEN_MIN),COL_MIN_mask,LOAD_mask,'OPPEAK,5')
               CALL DVAL(COL_MIN,ROW_SR,VAL_SR,COL_MIN_mask,ROW_SR_mask,'OPPEAK,6')
            ELSE
               GEN = DRAT * ELWDTH(IVSL,IS)
               CALL DVAL(COL,LOAD,DBLE(0.1 * GEN),COL_mask,LOAD_mask,'OPPEAK,7')
            END IF

!           set bound on column

            CALL DBND(EFDBND,COL,0.0D0,DBLE(CAP(IS)*0.001),COL_mask,'OPPEAK,8')

            IF (SR_CREDIT(IECP) .GT. 0.0) THEN
               CALL DBND(EFDBND,COL_MIN,0.0D0,DBLE(CAP(IS)*0.001),COL_MIN_mask,'OPPEAK,9')
            END IF

!           intersection with OBJ

            RPSU = 0.0
            RPSR = 0.0
            RPSCST = 0.0
            IF (UPRNWREG .EQ. 1) THEN
               RPSCST = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
               IF (UPRNWBAS(IECP) .GT. 0.0)RPSCST = 0.0
            ELSEIF (UPRNWREG .EQ. 2) THEN
               RPSCST = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
            ELSEIF (UPRNWREG .EQ. 3) THEN
               RPSU = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
               IF (UPRNWBAS(IECP) .GT. 0.0)RPSU = 0.0
               RPSR = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
               IF(EPRPSPR(CURIYR) .GE. EPRPSPRR(IRG,CURIYR))THEN
                  RPSCST = RPSU
               ELSE
                  RPSCST = RPSR
               END IF
            ENDIF

!           RPSCST = MAX(RPSU,RPSR)


            IF (ST_RPS_SW .GT. 0) THEN
               RPSR = ST_RPS_EMM_P(IRG,CURIYR) * (ST_RNW_BND(CURIYR,IRG) - ST_RNW_SHR(IECP,IRG,CURIYR))
               IF (RPSCST .LT. RPSR) RPSCST = RPSR
            END IF
!
!           DETERMINE CARBON INTENSITY RATE AND GPS ADDER, IF ANY
!
            OGCAR = 0.0
!           ACCOUNT FOR CHP ADJUSTMENT FOR NT COGEN
            IF (UG_MRUN(n) .GT. 9000)THEN
               CO2ADJ = CO2_ADJNT
            ELSE
               CO2ADJ = 1.0
            END IF

            IF (IECP .GT. WIIS .AND. IECP .LE. WIFC) THEN
               GSPR = MAX(UPFUEL(UIGF,EPGSMP(TFR)),UPFUEL(UIGI,EPGSMP(TFR)))
               OLPR = UPFUEL(UIDS,EPCSMP(TFR))
               IF (GSPR .LE. OLPR)THEN
                  GASSH = EDMXGS(2,IP,TFR)
                  OLSH = 1.0 - GASSH
               ELSE
                  OLSH = EDMXDS(2,IP,TFR)
                  GASSH = 1.0 - OLSH
               END IF
               OGCAR = GASSH *  ENGEL(CURIYR) * 0.001
               OGCAR = OGCAR + OLSH *  EDSEL(CURIYR) * 0.001
            ELSEIF (IP .EQ. UIDGB .OR. IP .EQ. UIDGP) THEN
               OGCAR = ENGEL(CURIYR) * 0.001
            END IF
            OGCAR = OGCAR * (1.0 - UPPCEF(IECP))
            CO2LB = (HTRT * 0.001) * OGCAR * (44.0 / 12.0) * 2204.0
            CO2LB = CO2LB / CO2ADJ

            GPSCST = 0.0
            IF (CO2_PRCSW .EQ. 10 .AND. CO2LB .GT. 0.001 .AND. CO2_PLTSW(IECP) .GT. 0.0)THEN
               IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.001)THEN
                  GPSCST = (CO2LB - CO2_STDRN(IRG,CURIYR)) * MAX(0.0,(ECPPRCNL(IRG,CURIYR) - EFDPRCNL(IRG,CURIYR)))
               END IF 
               GPSCST = GPSCST * CO2_PLTSW(IECP)
            END IF 

            OBJVAL = (VOM + RPSCST + GPSCST - PTC) * GEN * 0.001

            CALL DVAL(COL,EFDOBJ,OBJVAL,COL_mask,EFDOBJ,'OPPEAK,10')

            IF (SR_CREDIT(IECP) .GT. 0.0) THEN
               OBJVAL = (VOM + RPSCST +GPSCST - PTC) * GEN_MIN * 0.001
               CALL DVAL(COL_MIN,EFDOBJ,OBJVAL,COL_MIN_mask,EFDOBJ,'OPPEAK,11')
            END IF

!           capacity balance

            HRVAL = ELWDTH(IVSL,IS)/EETIME(IS)
            IF (EPPOPM(IP) .EQ. 1) THEN
               ROW = 'CB'//NUM//CHCOD(IS); call makmsk(ROW_mask,':CB:',NUM,CHCOD(IS))
               CALL DVAL(COL,ROW,DBLE(HRVAL),COL_mask,ROW_mask,'OPPEAK,12')
            ELSEIF (EPPOPM(IP) .EQ. 2) THEN

               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  ROW = 'H'//NUM//CHCOD(IS)//FSLCD; call makmsk(ROW_mask,':H:',NUM,CHCOD(IS),FSLCD)
                  CALL DROWTYPE(ROW,'L       ',ROW_mask)
                  CALL DRHS(EFDRHS,ROW,DBLE(CAP(IS)*0.001),ROW_mask,'OPPEAK,13')

                  CALL DVAL(COL,ROW,1.0D0,COL_mask,ROW_mask,'OPPEAK,14')
                  CALL DVAL(COL_MIN,ROW,1.0D0,COL_MIN_mask,ROW_mask,'OPPEAK,15')

!                 column for planned maint decision also uses slice capacity

                  COL_MP = 'MP'//NUM//CHCOD(IS); call makmsk(COL_MP_mask,':MP:',NUM,CHCOD(IS))
                  CALL DVAL(COL_MP,ROW,1.0D0,COL_MP_mask,ROW_mask,'OPPEAK,16')

               ELSE
                  ROW = 'CP'//CHCOD(IRG)//EPPLCD(IP)//CHCOD(IS)//FSLCD; call makmsk(ROW_mask,':CP:',CHCOD(IRG),EPPLCD(IP),CHCOD(IS),FSLCD)
                  CALL DVAL(COL,ROW,1.0D0,COL_mask,ROW_mask,'OPPEAK,17')
               ENDIF
            ENDIF

!           BTU row for fossil or biomass

            IF (IP .LE. UIFCG .OR. IP  .EQ. UIBMS .OR. IP .EQ. UIBIG .OR. IP .EQ. UIDGB .OR. IP .EQ. UIDGP) THEN
               BTUREQ = GEN * HTRT * 0.000001   !trill btu per GW

               if (BTUREQ .LT. 0.0) write(UF_DBG,*) 'OPPEAK BTUREQ < 0',IP,IECP,IGRP,GEN,HTRT,BTUREQ

               IF (UPTTYP(IECP) .LE. NW_COAL) THEN
                  ROW = 'BTU'//EPFLCD(TFR)//UPLNTCD(IECP)//'XX'; call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),UPLNTCD(IECP)//'X',':X:') ! use first X to make 3-character plant type.
               ELSEIF (IGS .EQ. 1) THEN
!                 NGSN = UNGSSN(IS)
                  NGSN = IS
                  ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//CHCOD(NGSN); call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),CHCOD(NGSN))
               ELSE
                  ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//'X'; call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),'X')
               ENDIF

               CALL DVAL(COL,ROW,BTUREQ,COL_mask,ROW_mask,'OPPEAK,18')

               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  BTUREQ_MIN = GEN_MIN * HTRT * 0.000001   !trill btu per GW
                  CALL DVAL(COL_MIN,ROW,BTUREQ_MIN,COL_MIN_mask,ROW_mask,'OPPEAK,19')
               END IF
            ENDIF

!           STATE LEVEL CARBON EMISSIONS
 
            IF ((CURIYR + UHBSYR) .GT. UESTYR)THEN 
               IF (OGCAR .GT. 0.0)THEN
                  IF (IST .GT. 0) THEN
                     ROW = 'CAROG' // USTNME(IST); call makmsk(ROW_mask,':CAROG:' , USTNME(IST))
                     VALUE = BTUREQ * DBLE(OGCAR * 1000.0)
                     CALL DVAL(COL,ROW,VALUE,COL_mask,ROW_mask,'OPPEAK,20')
!                    ROW = 'CONSOG' // USTNME(IST)
!                    CALL DVAL(COL,ROW,BTUREQ)

                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        ROW = 'CAROG' // USTNME(IST); call makmsk(ROW_mask,':CAROG:' , USTNME(IST))
                        VALUE = BTUREQ_MIN * DBLE(OGCAR * 1000.0)
                        CALL DVAL(COL_MIN,ROW,VALUE,COL_MIN_mask,ROW_mask,'OPPEAK,21')
!                       ROW = 'CONSOG' // USTNME(IST)
!                       CALL DVAL(COL_MIN,ROW,BTUREQ_MIN)
                     END IF
!                 FOR UNPLANNED BUILDS, USE FUEL REGION/STATE SHARES
                  ELSE
                     DO STA = 1 , EMM_D_ST
                        VALSH = DBLE(EGEN_FRST(IECP,TFR,STA) / EGEN_FRST(IECP,TFR,EMM_D_ST + 1))
                        IF (VALSH .GT. EFD_MIN)THEN
                           ROW = 'CAROG' // USTNME(STA); call makmsk(ROW_mask,':CAROG:' , USTNME(STA))
                           VALUE = BTUREQ * DBLE(OGCAR * VALSH * 1000.0)
                           IF (VALUE .GT. EFD_MIN)THEN
                              CALL DVAL(COL,ROW,VALUE,COL_mask,ROW_mask,'OPPEAK,22')
                           END IF
!                          ROW = 'CONSOG' // USTNME(STA)
!                          CALL DVAL(COL,ROW,BTUREQ * VALSH

                           IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                              ROW = 'CAROG' // USTNME(STA); call makmsk(ROW_mask,':CAROG:' , USTNME(STA))
                              VALUE = BTUREQ_MIN * DBLE(OGCAR * VALSH * 1000.0)
                              IF (VALUE .GT. EFD_MIN)THEN
                                 CALL DVAL(COL_MIN,ROW,VALUE,COL_MIN_mask,ROW_mask,'OPPEAK,23')
                              END IF
!                             ROW = 'CONSOG' // USTNME(STA)
!                             CALL DVAL(COL_MIN,ROW,BTUREQ_MIN * VALSH
                           END IF
                        END IF
                     END DO
                  END IF

               END IF
!
!              FUEL REGION INTENSITY STANDARD
!
!              ACCUMULATE TOTAL CO2 EMISSIONS
               IF (CO2LB .GT. 0.0)THEN
                  IF (TFR .GT. 0)THEN
!                    ROW_GPS = 'CO2TFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':CO2TFR:' , FLRGCODE(TFR))
                     ROW_GPSN = 'CO2TNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2TNR:' , URGNME(IRG)(6:7))
                     VAL = GEN * 0.001 * CO2LB * CO2ADJ * 0.001
!                    CALL DVAL(COL,ROW_GPS,VAL,COL_mask,ROW_GPS_mask,'OPPEAK,24')
                     CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPPEAK,25')
                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        VAL_MIN = GEN_MIN * 0.001 * CO2LB * CO2ADJ * 0.001
!                       CALL DVAL(COL_MIN,ROW_GPS,VAL_MIN,COL_MIN_mask,ROW_GPS_mask,'OPPEAK,26')
                        CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPPEAK,27')
                     END IF
                     IF (CO2_PLTSW(IECP) .GT. 0.0)THEN
!                       ROW_GPS = 'CO2QFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':CO2QFR:' , FLRGCODE(TFR))
                        ROW_GPSN = 'CO2QNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2QNR:' , URGNME(IRG)(6:7))
                        VAL = GEN * 0.001 * CO2LB * 0.001 * CO2_PLTSW(IECP)
!                       CALL DVAL(COL,ROW_GPS,VAL,COL_mask,ROW_GPS_mask,'OPPEAK,28')
                        CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPPEAK,29')
                        IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                           VAL_MIN = GEN_MIN * 0.001 * CO2LB * 0.001 * CO2_PLTSW(IECP)
!                          CALL DVAL(COL_MIN,ROW_GPS,VAL_MIN,COL_MIN_mask,ROW_GPS_mask,'OPPEAK,30')
                           CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPPEAK,31')
                        END IF
                     END IF
                  ELSE
      write(6,3344) curiyr+1989,irg,uplntcd(iecp),col
 3344 format(1h ,'NO FR FOS',i4,i3,a3,a10)
                  END IF
               END IF
               IF (CO2_PLTSW(IECP) .GT. 0.0)THEN
                  IF (TFR .GT. 0)THEN
                     IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                        VAL = GEN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                        ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                        CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPPEAK,33')
                     END IF
                     VAL = GEN * 0.001 * CO2_PLTSW(IECP)
!                    ROW_GPS = 'GENQFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':GENQFR:' , FLRGCODE(TFR))
!                    CALL DVAL(COL,ROW_GPS,VAL,COL_mask,ROW_GPS_mask,'OPPEAK,34')
                     ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                     CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPPEAK,35')
   
                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                           VAL_MIN = GEN_MIN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                           ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                           CALL DVAL(COL,ROW_GPSN,VAL_MIN,COL_mask,ROW_GPSN_mask,'OPPEAK,37')
                        END IF
                        VAL_MIN = GEN_MIN * 0.001 * CO2_PLTSW(IECP)
!                       ROW_GPS = 'GENQFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':GENQFR:' , FLRGCODE(TFR))
!                       CALL DVAL(COL_MIN,ROW_GPS,VAL_MIN,COL_MIN_mask,ROW_GPS_mask,'OPPEAK,38')
                        ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                        CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPPEAK,39')
                     END IF
                  ELSE
!                    EMM REGION FUEL INTENSITY STANDARDS
                     IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                        ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                        VAL = GEN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                        CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPPEAK,44')
                     END IF
                     VAL = GEN * 0.001 * CO2_PLTSW(IECP)
                     ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                     CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPPEAK,45')

                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                           ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                           VAL_MIN = GEN_MIN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                           CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPPEAK,46')
                        END IF
                        ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                        VAL_MIN = GEN_MIN * 0.001 * CO2_PLTSW(IECP)
                        CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPPEAK,47')
                     END IF
                  END IF
               END IF
            END IF

!           NOX constraint for dispatchables

            IF (IP .LE. EFD_D_DSP) THEN
               DO INOX = 1, NOX_GRP
                  WRITE(NOXCODE,'(I1)') INOX
                  IF (EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
                     ROW_NOX = 'ELNOX' // NOXCODE // UPLNTCD(IECP); call makmsk(ROW_NOX_mask,':ELNOX:' , NOXCODE , UPLNTCD(IECP))
                  ELSE
                     ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE)
                  END IF
                  NOXEM = GEN * HTRT * UG_NOXC(IS,INOX,N) * 0.0005 * 0.001
                  IF (NOXEM .GT. 0.001) THEN
                     IF (EMRFNA(INOX,CURIYR) .GT. 0.0) TST_NOX(IECP,INOX) = 1
                     CALL DVAL(COL,ROW_NOX,NOXEM,COL_mask,ROW_NOX_mask,'OPPEAK,48')

                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                        NOXEM_MIN = GEN_MIN * HTRT * UG_NOXC(IS,INOX,N) * 0.0005 * 0.001
                        CALL DVAL(COL_MIN,ROW_NOX,NOXEM_MIN,COL_MIN_mask,ROW_NOX_mask,'OPPEAK,49')
                     END IF

                  END IF
               ENDDO
            ENDIF  ! IF DISPATCHABLE CAPACITY
!
!           MUSTRUN CONSTRAINT
!
            IF (UG_MRUN(N) .GT. 0) THEN

!              ROW_MR = 'MR' // URGNME(IRG)(1:4) // UPLNTCD(IECP)

               ROW_MR = 'MR' // NUM // 'X'; call makmsk(ROW_MR_mask,':MR:' , NUM , ':X:')
               CALL DVAL(COL,ROW_MR,0.1*GEN,COL_mask,ROW_MR_mask,'OPPEAK,50')

               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  CALL DVAL(COL_MIN,ROW_MR,0.1*GEN_MIN,COL_MIN_mask,ROW_MR_mask,'OPPEAK,51')
               END IF
            END IF

!           RPS constraint if in place
!
!           IF (UPRNWBND(CURIYR) .GT. 0.005 .AND. UPRNWSHR(IECP) .NE. 0.0) THEN
!              ROWRPS = 'RPSREQXX'
!              IF (UPRNWSHR(IECP) .GT. 0.0) THEN
!                 VAL = GEN * UPRNWSHR(IECP) * 0.001
!              ELSE
!                 VAL = GEN * UPRNWBND(CURIYR) * 0.001
!                 IF (UPRNWCAS .EQ. 3) VAL = VAL / (1.0 + EQTDLS * ULOSSADJ(CURIYR))
!              ENDIF
!              CALL DVAL(COL,ROWRPS,DBLE(VAL))
!           ENDIF ! RPS

            IF (USW_EFDRPS .GT. 0 .AND. UPRNWCAS .GT. 0 .AND. UPRNWBND(CURIYR) .GT. 0.005)THEN
               IF (UPRNWSHR(IECP) .GT. 0.0) THEN
                  VAL = GEN * UPRNWSHR(IECP) * 0.001
                  ROWRPS = 'RPSREQ' // UPRGCD(IRG); call makmsk(ROWRPS_mask,':RPSREQ:' , UPRGCD(IRG))
                  CALL DVAL(COL,ROWRPS,DBLE(VAL),COL_mask,ROWRPS_mask,'OPPEAK,52')

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     VAL_MIN = GEN_MIN * UPRNWSHR(IECP) * 0.001
                     CALL DVAL(COL_MIN,ROWRPS,VAL_MIN,COL_MIN_mask,ROWRPS_mask,'OPPEAK,53')
                  END IF

!p                ROWRPS = 'RPSPGN' // UPLNTCD(IECP)
!p                CALL DVAL(COL,ROWRPS,DBLE(VAL))

               ENDIF ! RPS

!n             IF (UPRNWSHR(IECP) .LT. 1.0) THEN
!n                VAL = GEN * (1.0 - UPRNWSHR(IECP)) * 0.001
!n                ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!n                CALL DVAL(COL,ROWRPS,DBLE(VAL))
!n                ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!n                CALL DVAL(COL,ROWRPS,DBLE(VAL))
!n             ENDIF ! RPS
!              IF (UPRNWEXG(IECP) .GT. 0.0 .AND. ULVINT(IGRP) .EQ. 1) THEN
!                 VAL = GEN * UPRNWEXG(IECP) * 0.001
!                 ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!                 CALL DVAL(COL,ROWRPS,DBLE(VAL))
!                 ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!                 CALL DVAL(COL,ROWRPS,DBLE(VAL))
!              ENDIF ! RPS

            ENDIF ! RPS
!
!           GRID RESILIENCE ROWS
!
            DO IGR = 1 , GRD_CASN
               IF (GRD_RATSA(IECP,IRG,IGR) .GE. 0.0)THEN
                  ROW_GRD = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                  IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                     VAL = GEN * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                  ELSE
                     VAL = GEN * 0.001 * GRD_RATSA(IECP,IRG,IGR)
                  END IF
                  IF (VAL .NE. DBLE(0.0))CALL DVAL(COL,ROW_GRD,VAL,COL_mask,ROW_GRD_mask,'OPPEAK,54')
  
                  ROW_GRD = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                  VAL = GEN * 0.001
                  CALL DVAL(COL,ROW_GRD,VAL,COL_mask,ROW_GRD_mask,'OPPEAK,55')

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     ROW_GRD = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                     IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                        VAL_MIN = GEN_MIN * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                     ELSE
                        VAL_MIN = GEN_MIN * 0.001 * GRD_RATSA(IECP,IRG,IGR)
                     END IF
                     IF (VAL_MIN .NE. DBLE(0.0))CALL DVAL(COL_MIN,ROW_GRD,VAL_MIN,COL_MIN_SR_mask,ROW_GRD_mask,'OPPEAK,56')
   
                     ROW_GRD = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                     VAL_MIN = GEN_MIN * 0.001
                     CALL DVAL(COL_MIN,ROW_GRD,VAL_MIN,COL_MIN_SR_mask,ROW_GRD_mask,'OPPEAK,57')
                  END IF
               END IF
            END DO

         ENDDO   !IVSL
      ENDDO    !SEASON

      RETURN
      END
!
!     This subroutine sets up operates for baseload plants that operate in
!      a fixed mode of every slice - used for geothermal and biomass,
!      and for must run units
!

      SUBROUTINE OPRNBS(N)
      use efd_row_col
      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'plntctl'
      include'ecpcntl'
      include'elcntl'
      include'emission'
      include'dispett'
      include'uecpout'
      include'cdsparms'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'dispcrv'
      include'eusprc'
      include'edbdef'
      include'ecp_nuc'
      include'csapr'
      include'emmemis'
      include'emm_aimms'

!
!      COMMON /GRDSRC/ GRD_CASN,GRD_SRCN,GRD_SRCC
!      INTEGER GRD_CASN                                       ! Number of grid resilience sources 
!      CHARACTER*15 GRD_SRCN(MX_GRDSRC)                       ! Grid resilience source names
!      CHARACTER*1  GRD_SRCC(MX_GRDSRC)                       ! Grid resilience source codes

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,IRET,IFR,IGR,ICS
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,INOX,FSL,RUN45Q
      REAL VOM,HTRT,CAP(EFD_D_MSP),HRVAL,MAXCF(EFD_D_MSP),PTC,NOXFAC,RPSCST,RPSU,RPSR
      REAL*8 DRAT,GEN,OBJVAL,BTUREQ,VAL,MRCAP,NOXEM,GENBMS,VALSH
      REAL*8 DRAT_ORG,DRAT_MIN,DRAT_MAX,GEN_MIN,GEN_MAX,BTUREQ_MIN,BTUREQ_MAX,VAL_SR,VAL_MIN,VAL_MAX,NOXEM_MIN,NOXEM_MAX,OBJVAL_MIN,OBJVAL_MAX,VAL_CAP_CO2,VAL_MIN_SR,COL_MIN_SR,VAL_MAX_SR,COL_MAX_SR
      REAL*8 FACTOR, FACTOR_MIN, FACTOR_MAX
      REAL*4 CO2LB,CO2ADJ
      CHARACTER*1 FSLCD,NOXCODE
      CHARACTER*5 NUM
      CHARACTER*16 COL,ROW,LOAD,ROW_NOX,ROWRPS,ROWBMS,ROW_MR,ROW_SR,COL_MIN,COL_MAX,ROW_GPS,ROW_GPSN,ROW_GRD,ROW_EOR_CO2
      character*30 ROW_GRD_mask
      CHARACTER*2 CNSCOD

      REAL*8 MAX_SP_LOAD, MIN_SP_LOAD

      efdsub='OPRNBS'

      RPSCST = 0.0

      IRG = UG_EMM_RG(N)
      IP = UG_EFDt(N)
      IECP = UG_ECPt(N)
      IGRP = EFD_GRPS_F(N)
      TFR = UG_FL_RG(N)
      VOM = UG_OMR(N)
      PTC = UG_GSUB(N)
      ICS = EPCSMP(TFR)
      WRITE(CNSCOD,'("0",I1)') ICS

      DO IS = 1, EENSP
         CAP(IS) = UG_CAP(IS,N)
         MAXCF(IS) = UG_SCF(IS,N)
!STEOBM adjust CF for STEO benchmarking (factor will be 1.0 if no benchmarking)
         IF (IECP .EQ. WIGT) THEN
            MAXCF(IS) = MAXCF(IS) * URGTCFA(CURIYR)    
         ENDIF

         HTRT = UG_HTRT(IS,N)

         IF (FCRL .eq. 1) THEN
            write(UF_DBG,5263) CURIYR,CURITR,IS,N,IGRP,IP,TFR,VOM,PTC,HTRT,CAP(1),MAXCF(1)
 5263       format(1x,'OPRNBS ',7(":",I6),5(":",F10.2))
         ENDIF
      ENDDO
!
!     STORE 111d SWITCH FOR MASS OR RATE STANDARDS FOR REGION
!
      CO2_PLTSW(IECP) = CO2_PLTRG(IECP,IRG)

      IGS = 0
      DO IFL = 1, EFD_D_FPP
         if (WFLTP(IP,IFL) .NE. 0) THEN
            IF (UIGAS(WFLTP(IP,IFL)) .EQ. 1) IGS = 1
         endif
      ENDDO

      WRITE(NUM,'(I5.5)') IGRP

!     loop over time season and slices
!     only one mode that crosses all load slices

      DO IS = 1, EENSP

         HTRT = UG_HTRT(IS,N)

         MAX_SP_LOAD = 0.0
         MIN_SP_LOAD = 500000.0
         DO IVSL = 1 , ELNVCT(IS)
            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)
            MAX_SP_LOAD = MAX(MAX_SP_LOAD , UTHGHT(SEG,GRP,IRG))
            MIN_SP_LOAD = MIN(MIN_SP_LOAD , UTHGHT(SEG,GRP,IRG))
         END DO

         GEN = 0.0
         COL = 'O'//NUM//'X'//CHCOD(IS); call makmsk(COL_mask,':O:',NUM,':X:',CHCOD(IS))

         IF (SR_CREDIT(IECP) .GT. 0.0) THEN
            GEN_MIN = 0.0
            GEN_MAX = 0.0
            COL_MIN = 'J'//NUM//'X'//CHCOD(IS); call makmsk(COL_MIN_mask,':J:',NUM,':X:',CHCOD(IS))
            COL_MAX = 'U'//NUM//'X'//CHCOD(IS); call makmsk(COL_MAX_mask,':U:',NUM,':X:',CHCOD(IS))
         END IF

!        Combined Capacity factor for ECP types with SR_CREDIT can not exceed MAXCF(IS)

         IF (SR_CREDIT(IECP) .GT. 0.0) THEN

            FACTOR = 0.0
            FACTOR_MIN = 0.0
            FACTOR_MAX = 0.0

            DO IVSL = 1 , ELNVCT(IS)
               GRP = ELGRP(IVSL,IS)
               SEG = ELSEG(IVSL,IS)

               DRAT_ORG = MAXCF(IS)

!              Load Following Mode Follows Load Height drat = (1 - for) * (ld / max_ld)

               DRAT = (1.0 - UG_FOR(N)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD)
               FACTOR = FACTOR + DRAT * ELWDTH(IVSL,IS)

!              Min Electricity - Max Spinning Reserve Credit drat_min = (1 - sr_min_lf) * sr_min_cf * (ld / min_ld) + sr_min_lf * sr_min_cf

               DRAT_MIN = (1.0 - SR_MIN_LF(IECP)) * SR_MIN_CF(IECP) * (UTHGHT(SEG,GRP,IRG) / MIN_SP_LOAD) + SR_MIN_LF(IECP) * SR_MIN_CF(IECP)
               FACTOR_MIN = FACTOR_MIN + DRAT_MIN * ELWDTH(IVSL,IS)

!              Max Electricity - Min Spinning Reserve Credit drat_max = (1 - sr_max_lf) * (1 - for) * (ld / max_ld) + sr_max_lf * (1 - for)

               DRAT_MAX = (1.0 - SR_MAX_LF(IECP)) * (1.0 - UG_FOR(N)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD) + &
                           SR_MAX_LF(IECP) * (1.0 - UG_FOR(N))
                   
               IF (UG_MRUN(N) .GT. 0 .AND. DRAT_MAX .LT. DRAT_ORG) THEN
                  DRAT_MAX = MAX(DRAT_MAX , DRAT_ORG)
               END IF
               FACTOR_MAX = FACTOR_MAX + DRAT_MAX * ELWDTH(IVSL,IS)

            END DO

            FACTOR = MIN(1.0 , MAXCF(IS) * EETIME(IS) / FACTOR)
            FACTOR_MIN = MIN(1.0 , MAXCF(IS) * EETIME(IS) / FACTOR_MIN) 
            FACTOR_MAX = MIN(1.0 , MAXCF(IS) * EETIME(IS) / FACTOR_MAX) 

         END IF
         DO IVSL = 1 , ELNVCT(IS)
            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)
            FSL = EFD_Slice_ID(GRP,SEG)
            FSLCD = CHAR(64+FSL)
            LOAD = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(LOAD_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)

!           IF (IP .LE. EFD_D_DSP) THEN
!              CALL ELDRAT(IS,IRG,N,IVSL,DRAT)
!           ELSE

            DRAT_ORG = MAXCF(IS)

!           For ECP Types which have Spinning Reserve Credit Create Min, Max and Load Following Modes for each of the original modes

            IF (SR_CREDIT(IECP) .GT. 0.0) THEN

!              Load Following Mode Follows Load Height drat = (1 - for) * (ld / max_ld)

               DRAT = (1.0 - UG_FOR(N)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD)

!              Min Electricity - Max Spinning Reserve Credit drat_min = (1 - sr_min_lf) * sr_min_cf * (ld / min_ld) + sr_min_lf * sr_min_cf

               DRAT_MIN = (1.0 - SR_MIN_LF(IECP)) * SR_MIN_CF(IECP) * (UTHGHT(SEG,GRP,IRG) / MIN_SP_LOAD) + SR_MIN_LF(IECP) * SR_MIN_CF(IECP)

!              Max Electricity - Min Spinning Reserve Credit drat_max = (1 - sr_max_lf) * (1 - for) * (ld / max_ld) + sr_max_lf * (1 - for)

               DRAT_MAX = (1.0 - SR_MAX_LF(IECP)) * (1.0 - UG_FOR(N)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD) + &
                           SR_MAX_LF(IECP) * (1.0 - UG_FOR(N))
                   
               IF (UG_MRUN(N) .GT. 0 .AND. DRAT_MAX .LT. DRAT_ORG) THEN
                  DRAT_MAX = MAX(DRAT_MAX , DRAT_ORG)
               END IF

               DRAT = DRAT * FACTOR
               DRAT_MIN = DRAT_MIN * FACTOR_MIN
               DRAT_MAX = DRAT_MAX * FACTOR_MAX

               IF (IECP .EQ. WIMS) THEN
                  DRAT_MAX = DRAT_ORG
               END IF
            ELSE
               DRAT = DRAT_ORG
            END IF

!           ENDIF

            IF (SR_CREDIT(IECP) .GT. 0.0) THEN
               ROW_SR = 'SR'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(ROW_SR_mask,':SR:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)

               VAL = DRAT * ELWDTH(IVSL,IS)
               CALL DVAL(COL,LOAD,DBLE(0.1 * VAL),COL_mask,LOAD_mask,'OPRNBS,1')

               VAL_SR = 1.0 - DRAT
               CALL DVAL(COL,ROW_SR,VAL_SR,COL_mask,ROW_SR_mask,'OPRNBS,4')

               GEN = GEN + VAL

               VAL_MIN = DRAT_MIN * ELWDTH(IVSL,IS)
               CALL DVAL(COL_MIN,LOAD,DBLE(0.1 * VAL_MIN),COL_MIN_mask,LOAD_mask,'OPRNBS,5')

               VAL_SR = 1.0 - DRAT_MIN
               CALL DVAL(COL_MIN,ROW_SR,VAL_SR,COL_MIN_mask,ROW_SR_mask,'OPRNBS,6')

               GEN_MIN = GEN_MIN + VAL_MIN

               VAL_MAX = DRAT_MAX * ELWDTH(IVSL,IS)
               CALL DVAL(COL_MAX,LOAD,DBLE(0.1 * VAL_MAX),COL_MAX_mask,LOAD_mask,'OPRNBS,9')

               VAL_SR = 1.0 - DRAT_MAX
               CALL DVAL(COL_MAX,ROW_SR,VAL_SR,COL_MAX_mask,ROW_SR_mask,'OPRNBS,10')

               GEN_MAX = GEN_MAX + VAL_MAX

               IF (ISNAN(DRAT_MAX) .OR. ABS(DRAT_MAX) .GT. HUGE(DRAT_MAX)) THEN   ! check for NaNQ this way
                  WRITE(6,3710) CURIRUN, CURIYR+1989, CURITR, IRG, IECP, COL_MAX, DRAT_MAX, DRAT_ORG, VAL_SR, &
                     SR_CREDIT(IECP), DRAT_MIN, SR_MAX_LF(IECP), UG_FOR(N) * UTHGHT(SEG,GRP,IRG), MAX_SP_LOAD
 3710             FORMAT(1X,"DRAT_MAX_NAN2",5(":",I5),":",A16,10(":",F21.6))
               END IF

            ELSE
               IF (DRAT .LT. 0.01) DRAT = 0.01
               VAL = DRAT * ELWDTH(IVSL,IS)
               CALL DVAL(COL,LOAD,DBLE(0.1 * VAL),COL_mask,LOAD_mask,'OPRNBS,13')
               GEN = GEN + VAL

            END IF

         ENDDO   !IVSL

!        capacity balance

         IF (SR_CREDIT(IECP) .GT. 0.0) THEN
            ROW = 'CB'//NUM//CHCOD(IS); call makmsk(ROW_mask,':CB:',NUM,CHCOD(IS))
            CALL DROWTYPE(ROW,'L       ',ROW_mask)
            CALL DRHS(EFDRHS,ROW,DBLE(CAP(IS)*0.001),ROW_mask,'OPRNBS,16')

            CALL DVAL(COL,ROW,1.0D0,COL_mask,ROW_mask,'OPRNBS,17')
            CALL DVAL(COL_MIN,ROW,1.0D0,COL_MIN_mask,ROW_mask,'OPRNBS,18')
            CALL DVAL(COL_MAX,ROW,1.0D0,COL_MAX_mask,ROW_mask,'OPRNBS,19')

            CALL DBND(EFDBND,COL,0.0D0,DBLE(CAP(IS)* 0.001),COL_mask,'OPRNBS,20')
            CALL DBND(EFDBND,COL_MIN,0.0D0,DBLE(CAP(IS)* 0.001),COL_MIN_mask,'OPRNBS,21')
            CALL DBND(EFDBND,COL_MAX,0.0D0,DBLE(CAP(IS)* 0.001),COL_MAX_mask,'OPRNBS,22')
         ELSE
            IF (EPPOPM(IP) .EQ. 1) THEN
               ROW = 'CB'//NUM//CHCOD(IS); call makmsk(ROW_mask,':CB:',NUM,CHCOD(IS))
               CALL DVAL(COL,ROW,1.0D0,COL_mask,ROW_mask,'OPRNBS,23')
            ELSEIF (EPPOPM(IP) .EQ. 2) THEN
               ROW = 'CP'//CHCOD(IRG)//EPPLCD(IP)//CHCOD(IS)//FSLCD; call makmsk(ROW_mask,':CP:',CHCOD(IRG),EPPLCD(IP),CHCOD(IS),FSLCD)
               CALL DVAL(COL,ROW,1.0D0,COL_mask,ROW_mask,'OPRNBS,24')
            ENDIF

!           set bound on column

            CALL DBND(EFDBND,COL,0.0D0,DBLE(CAP(IS)* 0.001),COL_mask,'OPRNBS,25')
         END IF

!        intersection with OBJ

         RPSR = 0.0
         RPSU = 0.0
         RPSCST = 0.0
         IF (UPRNWREG .EQ. 1) THEN
            RPSCST = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
            IF (UPRNWBAS(IECP) .GT. 0.0)RPSCST = 0.0
         ELSEIF (UPRNWREG .EQ. 2) THEN
            RPSCST = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
         ELSEIF (UPRNWREG .EQ. 3) THEN
            RPSU = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
            IF (UPRNWBAS(IECP) .GT. 0.0)RPSU = 0.0
            RPSR = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
            IF(EPRPSPR(CURIYR) .GE. EPRPSPRR(IRG,CURIYR))THEN
               RPSCST = RPSU
            ELSE
               RPSCST = RPSR
            END IF
         ENDIF

         IF (ST_RPS_SW .GT. 0) THEN
            RPSR = ST_RPS_EMM_P(IRG,CURIYR) * (ST_RNW_BND(CURIYR,IRG) - ST_RNW_SHR(IECP,IRG,CURIYR))
             IF (RPSCST .LT. 0.0 .AND. RPSR .LE. 0.0) THEN  ! if both are negative use the greatest benefit
                  IF (ABS(RPSCST) .LT. ABS(RPSR)) RPSCST = RPSR
             ELSEIF (RPSCST .GT. 0.0 .AND. RPSR .GT. 0.0) THEN     ! if both are positive, use the highest positive cost
                  IF (RPSCST .LT. RPSR) RPSCST = RPSR
             ELSE                        ! use net value
                  RPSCST = RPSCST + RPSR
             ENDIF
         END IF
!       if (curiyr+1989 .eq. 2030) &
!       write(6,3232) curiyr+1989,curitr,uprnwreg,irg,col,uplntcd(iecp),gen,rpsu,rpsr,rpscst,vom,ptc,objval
!3232 format(1h ,'!efdrnw',4I6,a10,a3,8f10.3)

         IF (SR_CREDIT(IECP) .GT. 0.0) THEN
            OBJVAL = (VOM + RPSCST - PTC) * GEN * 0.001
            CALL DVAL(COL,EFDOBJ,OBJVAL,COL_mask,EFDOBJ,'OPRNBS,26')
            OBJVAL_MIN = (VOM + RPSCST - PTC) * GEN_MIN * 0.001
            CALL DVAL(COL_MIN,EFDOBJ,OBJVAL_MIN,COL_MIN_mask,EFDOBJ,'OPRNBS,27')
            OBJVAL_MAX = (VOM + RPSCST - PTC) * GEN_MAX * 0.001
            CALL DVAL(COL_MAX,EFDOBJ,OBJVAL_MAX,COL_MAX_mask,EFDOBJ,'OPRNBS,28')
         ELSE
            OBJVAL = (VOM + RPSCST - PTC) * GEN * 0.001
            CALL DVAL(COL,EFDOBJ,OBJVAL,COL_mask,EFDOBJ,'OPRNBS,29')
         END IF

!        BTU row for fossil or biomass plants
         RUN45Q=RTOVALUE('RUN45Q  ',0)

         IF (IP .LE. UIFCG .OR. IP .EQ. UIBMS .OR. IP .EQ. UIBIG .OR. IP .EQ. UIDGB .OR. IP .EQ. UIDGP) THEN
            IF (SR_CREDIT(IECP) .GT. 0.0) THEN
               BTUREQ = GEN * HTRT * 0.000001  ! trill btu per GW
               BTUREQ_MIN = GEN_MIN * HTRT * 0.000001  ! trill btu per GW
               BTUREQ_MAX = GEN_MAX * HTRT * 0.000001  ! trill btu per GW

               if (BTUREQ .LE. 0.0) write(UF_DBG,'(A20,2I5,I8,3F12.2)') 'OPRNBS BTUREQ <= 0',IP,IECP,IGRP,GEN,HTRT,BTUREQ

               IF (UPTTYP(IECP) .LE. NW_COAL) THEN
                  ROW = 'BTU'//EPFLCD(TFR)//UPLNTCD(IECP)//'XX'; call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),UPLNTCD(IECP)//'X',':X:') ! use first X to make 3-character plant type.
                  CALL DVAL(COL,ROW,BTUREQ,COL_mask,ROW_mask,'OPRNBS,30')
                  CALL DVAL(COL_MIN,ROW,BTUREQ_MIN,COL_MIN_mask,ROW_mask,'OPRNBS,31')
                  CALL DVAL(COL_MAX,ROW,BTUREQ_MAX,COL_MAX_mask,ROW_mask,'OPRNBS,32')
               ELSEIF (IGS .EQ. 1) THEN
                  DO NGSN = 1, 3
                     ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//CHCOD(NGSN); call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),CHCOD(NGSN))
                     CALL DVAL(COL,ROW,BTUREQ,COL_mask,ROW_mask,'OPRNBS,33')
                     CALL DVAL(COL_MIN,ROW,BTUREQ_MIN,COL_MIN_mask,ROW_mask,'OPRNBS,34')
                     CALL DVAL(COL_MAX,ROW,BTUREQ_MAX,COL_MAX_mask,ROW_mask,'OPRNBS,35')
                  ENDDO
               ELSE
                  ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//'X'; call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),'X')
                  CALL DVAL(COL,ROW,BTUREQ,COL_mask,ROW_mask,'OPRNBS,36')
                  CALL DVAL(COL_MIN,ROW,BTUREQ_MIN,COL_MIN_mask,ROW_mask,'OPRNBS,37')
                  CALL DVAL(COL_MAX,ROW,BTUREQ_MAX,COL_MAX_mask,ROW_mask,'OPRNBS,38')
               ENDIF
            ELSE
               BTUREQ = GEN * HTRT * 0.000001  ! trill btu per GW
               if (BTUREQ .LE. 0.0) write(UF_DBG,'(A20,2I5,I8,3F12.2)') 'OPRNBS BTUREQ <= 0',IP,IECP,IGRP,GEN,HTRT,BTUREQ
               IF (UPTTYP(IECP) .LE. NW_COAL) THEN
                  ROW = 'BTU'//EPFLCD(TFR)//UPLNTCD(IECP)//'XX'; call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),UPLNTCD(IECP)//'X',':X:') ! use first X to make 3-character plant type.
                  CALL DVAL(COL,ROW,BTUREQ,COL_mask,ROW_mask,'OPRNBS,39')
               ELSEIF (IGS .EQ. 1) THEN
                  DO NGSN = 1, 3
                     ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//CHCOD(NGSN); call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),CHCOD(NGSN))
                     CALL DVAL(COL,ROW,BTUREQ,COL_mask,ROW_mask,'OPRNBS,40')
                  ENDDO
               ELSE
                  ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//'X'; call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),'X')
                  CALL DVAL(COL,ROW,BTUREQ,COL_mask,ROW_mask,'OPRNBS,41')
               ENDIF
            END IF
!           MAKE CAPTURED CO2 FROM BIOMASS UNITS AVAILABLE TO EOR PROJECTS – BECCS

            IF (UPPCEF(IECP) .GT. 0.0) THEN
               IF (UG_45q(n) .EQ. 0 .OR. RUN45Q .EQ. 0) THEN
                  ROW_EOR_CO2 = 'ZCSRG' // CNSCOD // '_'; call makmsk(ROW_EOR_CO2_mask,':ZCSRG:' , CNSCOD, ':_:')
               ELSE
                  ROW_EOR_CO2 = 'ZCSRS' // CNSCOD // '_'; call makmsk(ROW_EOR_CO2_mask,':ZCSRS:' , CNSCOD, ':_:')
               ENDIF
                  VAL_CAP_CO2 = UPPCEF(IECP) * BTUREQ * (26.0) * 0.001 * (44.0 / 12.0)
                  CALL DVAL(COL,ROW_EOR_CO2,VAL_CAP_CO2,COL_mask,ROW_EOR_CO2_mask,'OPRNBS,96')

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     VAL_MIN_SR = UPPCEF(IECP) * BTUREQ_MIN * (26.0) * 0.001 * (44.0 / 12.0)
                     CALL DVAL(COL_MIN,ROW_EOR_CO2,VAL_MIN_SR,COL_MIN_mask,ROW_EOR_CO2_mask,'OPRNBS,97')

                     VAL_MAX_SR = UPPCEF(IECP) * BTUREQ_MAX * (26.0) * 0.001 * (44.0 / 12.0)
                     CALL DVAL(COL_MAX,ROW_EOR_CO2,VAL_MAX_SR,COL_MAX_mask,ROW_EOR_CO2_mask,'OPRNBS,98')

                  END IF
!                       WRITE(6,9317) CURIRUN, CURIYR+1989, CURITR, COL, ROW_EOR_CO2, IRG, TFR, IECP, BTUREQ, &
!                          VAL_CAP_CO2, UPPCEF(IECP), ENGEL(CURIYR)
            ENDIF
         ENDIF
!
!        FUEL REGION INTENSITY STANDARD
!
         IF ((CURIYR + UHBSYR) .GT. UESTYR)THEN
            IF (IECP .EQ. WIWD .OR. IECP .EQ. WIBI)THEN
               CO2LB = HTRT * 0.001 * CO2_EMSWD
            ELSE
               CO2LB = 0.0
            END IF
            IF (UG_MRUN(N) .GT. 9000)THEN
               CO2ADJ = CO2_ADJNT
            ELSE
               CO2ADJ = 1.0
            END IF
            CO2LB = CO2LB / CO2ADJ
            IF (CO2LB .GT. 0.0 .AND. TFR .GT. 0)THEN
!              ROW_GPS = 'CO2TFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':CO2TFR:' , FLRGCODE(TFR))
               ROW_GPSN = 'CO2TNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2TNR:' , URGNME(IRG)(6:7))
               VAL = GEN * 0.001 * CO2LB * CO2ADJ * 0.001
!              CALL DVAL(COL,ROW_GPS,VAL,COL_mask,ROW_GPS_mask,'OPRNBS,42')
               CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPRNBS,43')
               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  VAL_MIN = GEN_MIN * 0.001 * CO2LB * CO2ADJ * 0.001
!                 CALL DVAL(COL_MIN,ROW_GPS,VAL_MIN,COL_MIN_mask,ROW_GPS_mask,'OPRNBS,44')
                  CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPRNBS,45')

                  VAL_MAX = GEN_MAX * 0.001 * CO2LB * CO2ADJ * 0.001
!                 CALL DVAL(COL_MAX,ROW_GPS,VAL_MAX,COL_MAX_mask,ROW_GPS_mask,'OPRNBS,46')
                  CALL DVAL(COL_MAX,ROW_GPSN,VAL_MAX,COL_MAX_mask,ROW_GPSN_mask,'OPRNBS,47')
               END IF
               IF (CO2_PLTSW(IECP) .GT. 0.0)THEN
!                 ROW_GPS = 'CO2QFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':CO2QFR:' , FLRGCODE(TFR))
                  ROW_GPSN = 'CO2QNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2QNR:' , URGNME(IRG)(6:7))
                  VAL = GEN * 0.001 * CO2LB * 0.001 * CO2_PLTSW(IECP)
!                 CALL DVAL(COL,ROW_GPS,VAL,COL_mask,ROW_GPS_mask,'OPRNBS,48')
                  CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPRNBS,49')
                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     VAL_MIN = GEN_MIN * 0.001 * CO2LB * 0.001 * CO2_PLTSW(IECP)
!                    CALL DVAL(COL_MIN,ROW_GPS,VAL_MIN,COL_MIN_mask,ROW_GPS_mask,'OPRNBS,50')
                     CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPRNBS,51')

                     VAL_MAX = GEN_MAX * 0.001 * CO2LB * 0.001 * CO2_PLTSW(IECP)
!                    CALL DVAL(COL_MAX,ROW_GPS,VAL_MAX,COL_MAX_mask,ROW_GPS_mask,'OPRNBS,52')
                     CALL DVAL(COL_MAX,ROW_GPSN,VAL_MAX,COL_MAX_mask,ROW_GPSN_mask,'OPRNBS,53')
                  END IF
               END IF
            END IF
            IF (CO2_PLTSW(IECP) .GT. 0.0)THEN
               IF (IECP .EQ. WIWD .OR. IECP .EQ. WIBI)THEN
                  IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                     VAL = GEN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                     ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                     CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPRNBS,55')
                  END IF
                  VAL = GEN * 0.001 * CO2_PLTSW(IECP)
!                 ROW_GPS  = 'GENQFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':GENQFR:' , FLRGCODE(TFR))
!                 CALL DVAL(COL,ROW_GPS,VAL,COL_mask,ROW_GPS_mask,'OPRNBS,56')
                  ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                  CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPRNBS,57')
   
                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                        VAL_MIN = GEN_MIN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                        ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                        CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPRNBS,60')

                        VAL_MAX = GEN_MAX * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                        CALL DVAL(COL_MAX,ROW_GPSN,VAL_MAX,COL_MAX_mask,ROW_GPSN_mask,'OPRNBS,61')
                     END IF
                     VAL_MIN = GEN_MIN * 0.001 * CO2_PLTSW(IECP)
!                    ROW_GPS = 'GENQFR' // FLRGCODE(TFR); call makmsk(ROW_GPS_mask,':GENQFR:' , FLRGCODE(TFR))
!                    CALL DVAL(COL_MIN,ROW_GPS,VAL_MIN,COL_MIN_mask,ROW_GPS_mask,'OPRNBS,62')
                     ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                     CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPRNBS,63')

                     VAL_MAX = GEN_MAX * 0.001 * CO2_PLTSW(IECP)
!                    CALL DVAL(COL_MAX,ROW_GPS,VAL_MAX,COL_MAX_mask,ROW_GPS_mask,'OPRNBS,64')
                     CALL DVAL(COL_MAX,ROW_GPSN,VAL_MAX,COL_MAX_mask,ROW_GPSN_mask,'OPRNBS,65')
                  END IF
!              FOR RENEWABLES (EXCLUDING BIOMASS) USE EMM REGION/FUEL REGION SHARE SINCE NO FUEL REGION IDENTITY
               ELSE
                  IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                     VAL = GEN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                     ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                     CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPRNBS,72')
                  END IF
                  ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                  VAL = GEN * 0.001 * CO2_PLTSW(IECP)
                  CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPRNBS,73')
  
                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                        ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                        VAL_MIN = GEN_MIN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                        CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPRNBS,74')
  
                        VAL_MAX = GEN_MAX * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                        CALL DVAL(COL_MAX,ROW_GPSN,VAL_MAX,COL_MAX_mask,ROW_GPSN_mask,'OPRNBS,75')
                     END IF
                     ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                     VAL_MIN = GEN_MIN * 0.001 * CO2_PLTSW(IECP)
                     CALL DVAL(COL_MIN,ROW_GPSN,VAL_MIN,COL_MIN_mask,ROW_GPSN_mask,'OPRNBS,76')

                     VAL_MAX = GEN_MAX * 0.001 * CO2_PLTSW(IECP)
                     CALL DVAL(COL_MAX,ROW_GPSN,VAL_MAX,COL_MAX_mask,ROW_GPSN_mask,'OPRNBS,77')
                  END IF
               END IF
            END IF
         END IF

!        NOX constraint for dispatchables

         IF (IP .LE. EFD_D_DSP) THEN
            DO INOX = 1, NOX_GRP
               NOXFAC = UG_NOXC(IS,INOX,N)
               WRITE(NOXCODE,'(I1)') INOX
               IF (EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
                  ROW_NOX = 'ELNOX' // NOXCODE // UPLNTCD(IECP); call makmsk(ROW_NOX_mask,':ELNOX:' , NOXCODE , UPLNTCD(IECP))
               ELSE
                  ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE)
               END IF
               NOXEM = GEN * HTRT * NOXFAC * 0.0005 * 0.001
               IF (NOXEM .GT. 0.001) THEN
                  IF (EMRFNA(INOX,CURIYR) .GT. 0.0) TST_NOX(IECP,INOX) = 1
                  CALL DVAL(COL,ROW_NOX,NOXEM,COL_mask,ROW_NOX_mask,'OPRNBS,78')
               END IF
               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  NOXEM_MIN = GEN_MIN * HTRT * NOXFAC * 0.0005 * 0.001
                  IF (NOXEM_MIN .GT. 0.001) THEN
                     CALL DVAL(COL_MIN,ROW_NOX,NOXEM_MIN,COL_MIN_mask,ROW_NOX_mask,'OPRNBS,79')
                  END IF
                  NOXEM_MAX = GEN_MAX * HTRT * NOXFAC * 0.0005 * 0.001
                  IF (NOXEM_MAX .GT. 0.001) THEN
                     CALL DVAL(COL_MAX,ROW_NOX,NOXEM_MAX,COL_MAX_mask,ROW_NOX_mask,'OPRNBS,80')
                  END IF
               END IF
            ENDDO
         END IF

!        MUSTRUN CONSTRAINT

         IF (UG_MRUN(N) .GT. 0) THEN
            ROW_MR = 'MR' // NUM // 'X'; call makmsk(ROW_MR_mask,':MR:' , NUM , ':X:')
            CALL DVAL(COL,ROW_MR,0.1*GEN,COL_mask,ROW_MR_mask,'OPRNBS,81')
            IF (SR_CREDIT(IECP) .GT. 0.0) THEN
               CALL DVAL(COL_MIN,ROW_MR,0.1*GEN_MIN,COL_MIN_mask,ROW_MR_mask,'OPRNBS,82')
               CALL DVAL(COL_MAX,ROW_MR,0.1*GEN_MAX,COL_MAX_mask,ROW_MR_mask,'OPRNBS,83')
            END IF
         END IF

!        RPS constraint if in place

!        IF (UPRNWBND(CURIYR) .GT. 0.005 .AND. UPRNWSHR(IECP) .NE. 0.0) THEN
!           ROWRPS = 'RPSREQXX'
!           IF (UPRNWSHR(IECP) .GT. 0.0) THEN
!              VAL = GEN * UPRNWSHR(IECP) * 0.001
!           ELSE
!              VAL = GEN * UPRNWBND(CURIYR) * 0.001
!              IF (UPRNWCAS .EQ. 3) VAL = VAL / (1.0 + EQTDLS * ULOSSADJ(CURIYR))
!           ENDIF
!           CALL DVAL(COL,ROWRPS,DBLE(VAL))
!        ENDIF ! RPS

         IF (USW_EFDRPS .GT. 0 .AND. UPRNWCAS .GT. 0 .AND. UPRNWBND(CURIYR) .GT. 0.005)THEN

!           QUALIFYING GENERATION

            IF (UPRNWSHR(IECP) .GT. 0.0)THEN
               ROWRPS = 'RPSREQ' // UPRGCD(IRG); call makmsk(ROWRPS_mask,':RPSREQ:' , UPRGCD(IRG))
               VAL = GEN * 0.001 * UPRNWSHR(IECP)
               CALL DVAL(COL,ROWRPS,VAL,COL_mask,ROWRPS_mask,'OPRNBS,84')

               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  VAL_MIN = GEN_MIN * 0.001 * UPRNWSHR(IECP)
                  CALL DVAL(COL_MIN,ROWRPS,VAL_MIN,COL_MIN_mask,ROWRPS_mask,'OPRNBS,85')
                  VAL_MAX = GEN_MAX * 0.001 * UPRNWSHR(IECP)
                  CALL DVAL(COL_MAX,ROWRPS,VAL_MAX,COL_MAX_mask,ROWRPS_mask,'OPRNBS,86')
               END IF

!p             ROWRPS = 'RPSPGN' // UPLNTCD(IECP)
!p             CALL DVAL(COL,ROWRPS,VAL)

            END IF

!           NON QUALIFYING GENERATION
!
!n          IF (UPRNWSHR(IECP) .LT. 1.0) THEN
!n             VAL = GEN * (1.0 - UPRNWSHR(IECP)) * 0.001
!n             ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!n             CALL DVAL(COL,ROWRPS,VAL)
!n             ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!n             CALL DVAL(COL,ROWRPS,VAL)
!n          ENDIF ! RPS
!
!           EXCLUDE EXISTING GENERATION, IF APPROPRIATE
!
!           IF (UPRNWEXG(IECP) .GT. 0.0 .AND. ULVINT(IGRP) .EQ. 1) THEN
!              VAL = GEN * UPRNWEXG(IECP) * 0.001
!              ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!              CALL DVAL(COL,ROWRPS,VAL)
!              ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!              CALL DVAL(COL,ROWRPS,VAL)
!           ENDIF ! RPS

         ENDIF ! RPS

!        Biomass generation row for RPS

         IF (IP .EQ. UIBMS .OR. IP .EQ. UIBIG)THEN
            ROWBMS = 'G'//URGNME(IRG)(1:4)//EPPLCD(IP); call makmsk(ROWBMS_mask,':G:',URGNME(IRG)(1:4),EPPLCD(IP))
            GENBMS = GEN * 0.001  !  Billion Kwh per GW
            CALL DVAL(COL,ROWBMS,GENBMS,COL_mask,ROWBMS_mask,'OPRNBS,87')
            IF (SR_CREDIT(IECP) .GT. 0.0) THEN
               VAL_MIN = GEN_MIN * 0.001  !  Billion Kwh per GW
               CALL DVAL(COL_MIN,ROWBMS,VAL_MIN,COL_MIN_mask,ROWBMS_mask,'OPRNBS,88')
               VAL_MAX = GEN_MAX * 0.001  !  Billion Kwh per GW
               CALL DVAL(COL_MAX,ROWBMS,VAL_MAX,COL_MAX_mask,ROWBMS_mask,'OPRNBS,89')
            ENDIF
         ENDIF
!
!        GRID RESILIENCE ROWS
!
         DO IGR = 1 , GRD_CASN
            IF (GRD_RATSA(IECP,IRG,IGR) .GE. 0.0)THEN
               ROW_GRD = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
               IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                  VAL = GEN * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
               ELSE
                  VAL = GEN * 0.001 * GRD_RATSA(IECP,IRG,IGR)
               END IF
               IF (VAL .NE. DBLE(0.0))CALL DVAL(COL,ROW_GRD,VAL,COL_mask,ROW_GRD_mask,'OPRNBS,90')

               ROW_GRD = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
               VAL = GEN * 0.001
               CALL DVAL(COL,ROW_GRD,VAL,COL_mask,ROW_GRD_mask,'OPRNBS,91')

               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  ROW_GRD = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                  IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                     VAL_MIN = GEN_MIN * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                  ELSE
                     VAL_MIN = GEN_MIN * 0.001 * GRD_RATSA(IECP,IRG,IGR)
                  END IF
                  IF (VAL_MIN .NE. DBLE(0.0))CALL DVAL(COL_MIN,ROW_GRD,VAL_MIN,COL_MIN_mask,ROW_GRD_mask,'OPRNBS,92')

                  IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                     VAL_MAX = GEN_MAX * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                  ELSE
                     VAL_MAX = GEN_MAX * 0.001 * GRD_RATSA(IECP,IRG,IGR)
                  END IF
                  IF (VAL_MAX .NE. DBLE(0.0))CALL DVAL(COL_MAX,ROW_GRD,VAL_MAX,COL_MAX_mask,ROW_GRD_mask,'OPRNBS,93')

                  ROW_GRD = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                  VAL_MIN = GEN_MIN * 0.001
                  CALL DVAL(COL_MIN,ROW_GRD,VAL_MIN,COL_MIN_mask,ROW_GRD_mask,'OPRNBS,94')

                  VAL_MAX = GEN_MAX * 0.001
                  CALL DVAL(COL_MAX,ROW_GRD,VAL_MAX,COL_MAX_mask,ROW_GRD_mask,'OPRNBS,95')
               END IF
            END IF
         END DO
      ENDDO  ! IS

      RETURN
      END
!
!     This subroutine sets up operates for intermittent plants
!       It uses slice specific capacity factors
!

      SUBROUTINE OPINT(N)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'plntctl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'elcntl'
      include'emission'
      include'dispett'
      include'uecpout'
      include'cdsparms'
      include'eusprc'
      include'edbdef'
      include'wrenew'
      include'ecp_nuc'
      include'csapr'
      include'emmemis'
      include'emm_aimms'
!
!      COMMON /GRDSRC/ GRD_CASN,GRD_SRCN,GRD_SRCC
!      INTEGER GRD_CASN                                       ! Number of grid resilience sources 
!      CHARACTER*15 GRD_SRCN(MX_GRDSRC)                       ! Grid resilience source names
!      CHARACTER*1  GRD_SRCC(MX_GRDSRC)                       ! Grid resilience source codes

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,IFR, D, M, H,IGR
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,MRSW,INOX,FSL
      REAL VOM,HTRT,CAP(EFD_D_MSP),HRVAL,PTC,CF,VAL,RPSCST,RPSU,RPSR,CO2LB
      REAL*8 DRAT,GEN,OBJVAL,BTUREQ,MRCAP,NOXEM,VAL_SR,VALUE,VALSH
      CHARACTER*1 FSLCD,NOXCODE
      CHARACTER*5 NUM
      CHARACTER*16 COL,LOAD,ROW_NOX,ROWRPS,ROW_SR,ROW_GPS,ROW_GPSN,ROW_GRD
      character*30 ROW_GRD_mask

      efdsub='OPINT'

      RPSCST = 0.0

      IRG = UG_EMM_RG(N)
      IP = UG_EFDt(N)
      IECP = UG_ECPt(N)
      IGRP = EFD_GRPS_F(N)
      TFR = UG_FL_RG(N)
      VOM = UG_OMR(N)
      PTC = UG_GSUB(N)

      MRSW = UG_MRUN(N)
!
!     STORE 111d SWITCH FOR MASS OR RATE STANDARDS FOR REGION
!
      CO2_PLTSW(IECP) = CO2_PLTRG(IECP,IRG)

      WRITE(NUM,'(I5.5)') IGRP

!     loop over time season and slices

      DO IS = 1, EENSP

        HTRT = UG_HTRT(IS,N)

        IF (FCRL .EQ. 1) THEN
           write(UF_DBG,4263) CURIYR,CURITR,IS,N,IGRP,IP,TFR,VOM,PTC,HTRT
 4263      format(1x,'OPINT ',7(":",I6),3(":",F10.2))
        ENDIF

        CAP(IS) = UG_CAP(IS,N)
        DO IVSL = 1 , ELNVCT(IS)
          GRP = ELGRP(IVSL,IS)
          SEG = ELSEG(IVSL,IS)
          CF = UG_GCF(SEG,GRP,N)
!STEOBM  apply CF adjustment for benchmarking (will be 1.0 if no benchmarking) 
          IF (IECP .EQ. WISO) CF = CF * URSOCFA(CURIYR)
          IF (IECP .EQ. WIPV .OR. IECP .EQ. WIPT) CF = CF * URSOCFA(CURIYR)          !use same solar overall factor for PV
          IF (IECP .EQ. WIWN .OR. IECP .EQ. WIWL .OR. IECP .EQ. WIWF) CF = CF * URWNCFA(CURIYR)
!END STEOBM
          IF (CF .GT. 0.0 .AND. CF .LE. 1.0) THEN
            FSL = EFD_Slice_ID(GRP,SEG)
            FSLCD = CHAR(64+FSL)
            COL = 'I'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_mask,':I:',NUM,CHCOD(IS),FSLCD)
            LOAD = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(LOAD_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)
            GEN = CF * ELWDTH(IVSL,IS)
            CALL DVAL(COL,LOAD,DBLE(0.1 * GEN),COL_mask,LOAD_mask,'OPINT,1')
            
            WRITE(18,4911) CURIRUN, CURIYR+1989, CURITR, N, IRG, IP, IECP, IGRP, TFR, IS, IVSL, GRP, SEG, UG_MRUN(N),COL,LOAD, GEN, ELWDTH(IVSL,IS), UG_GCF(SEG,GRP,N), CF, &
                DPVTOTGENNR(IRG,CURIYR)/(DPVTOTCAPNR(IRG,CURIYR)*8.76), URSOCFA(CURIYR), (HREFDCF(1,UCPINTIS(IECP),2,SEG,GRP,IRG)/ HREFDCF(0,UCPINTIS(IECP),1,SEG,GRP,IRG))
4911          FORMAT(1X,"INT_LOAD_EFD_INFO", 14(",",I6),",",A16,",",A16, 8(",",F21.6))
			
            VAL_SR = 0.0
            IF (SR_INT(IECP,IRG) .GT. 0.0) THEN
               VAL_SR = - 1.0 * CF * SR_INT(IECP,IRG)
            END IF

			!If PT can provide SR using its battery, add that amount of procurement
            IF (IECP .EQ. WIPT .AND. SR_CREDIT(IECP) .GT. 0.0) THEN
                VAL_SR = VAL_SR + NET_PT_STORAGE_SR_EFD(SEG,GRP,IRG,CURIYR)
            END IF
            IF (VAL_SR .NE. 0.0) THEN
                ROW_SR = 'SR' // URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(ROW_SR_mask,':SR:' , URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)
                CALL DVAL(COL,ROW_SR,VAL_SR,COL_mask,ROW_SR_mask,'OPINT,2')
            END IF

!           set bound on column

            IF (MRSW .GT. 0) THEN
              MRCAP = CAP(IS) * 0.001
              CALL DBND(EFDBND,COL,MRCAP,MRCAP,COL_mask,'OPINT,4')

            ELSE IF (GEN * 0.1 .GT. EFD_MIN) THEN
              CALL DBND(EFDBND,COL,0.0D0,DBLE(CAP(IS)*0.001),COL_mask,'OPINT,5')
            ELSE
              CALL DBND(EFDBND,COL,DBLE(CAP(IS)*0.001),DBLE(CAP(IS)*0.001),COL_mask,'OPINT,6')
            ENDIF

!           intersection with OBJ

            RPSR = 0.0
            RPSU = 0.0
            RPSCST = 0.0
            IF (UPRNWREG .EQ. 1) THEN
                RPSCST = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
                IF (UPRNWBAS(IECP) .GT. 0.0)RPSCST = 0.0
            ELSEIF (UPRNWREG .EQ. 2) THEN
                RPSCST = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
            ELSEIF (UPRNWREG .EQ. 3) THEN
                RPSU = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
                IF (UPRNWBAS(IECP) .GT. 0.0)RPSU = 0.0
                RPSR = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
                IF(EPRPSPR(CURIYR) .GE. EPRPSPRR(IRG,CURIYR))THEN
                  RPSCST = RPSU
                ELSE
                  RPSCST = RPSR
                END IF
            ENDIF
!           IF (FCRL .EQ. 1) WRITE(13,3229) '!EFD RPS1',CURIYR,CURITR,IRG,IECP,IGRP,RPSCST,EPRPSPR(CURIYR),UPRNWBNDR(CURIYR,IRG),UPRNWSHR(IECP)
3229       FORMAT(1x,A10,5I6,4F15.5)
            IF (ST_RPS_SW .GT. 0) THEN
               RPSR = ST_RPS_EMM_P(IRG,CURIYR) * (ST_RNW_BND(CURIYR,IRG) - ST_RNW_SHR(IECP,IRG,CURIYR))
               IF (RPSCST .LT. 0.0 .AND. RPSR .LE. 0.0) THEN  ! if both are negative use the greatest benefit
                  IF (ABS(RPSCST) .LT. ABS(RPSR)) RPSCST = RPSR
               ELSEIF (RPSCST .GT. 0.0 .AND. RPSR .GT. 0.0) THEN     ! if both are positive, use the highest positive cost
                  IF (RPSCST .LT. RPSR) RPSCST = RPSR
               ELSE                        ! use net value
                     RPSCST = RPSCST + RPSR
               ENDIF
            END IF
!            IF (FCRL .EQ. 1) WRITE(13,3229) 'EFD RPS2',CURIYR,CURITR,IRG,IECP,IGRP,RPSCST,ST_RPS_EMM_P(IRG,CURIYR),ST_RNW_BND(CURIYR,IRG),ST_RNW_SHR(IECP,IRG,CURIYR)

            
            OBJVAL = (VOM + RPSCST - PTC) * GEN * 0.001
!          IF (FCRL .EQ. 1) write(13,3232) 'EFD OBJ',CURIYR,CURITR,IRG,IECP,IGRP,OBJVAL,VOM,RPSCST,PTC,GEN
3232      FORMAT(1x,A10,5I6,F15.2,3F15.5,F15.2) 
!    if (curiyr+1989 .eq. 2030) &
!       write(6,3232) curiyr+1989,curitr,uprnwreg,irg,col,uplntcd(iecp),gen,rpsu,rpsr,rpscst,vom,ptc,objval
!3232 format(1h ,'!efdint',4I6,a10,a3,8f10.3)
            CALL DVAL(COL,EFDOBJ,OBJVAL,COL_mask,EFDOBJ,'OPINT,7')
!
!           EMM  REGION INTENSITY STANDARD
!
            IF ((CURIYR + UHBSYR) .GT. UESTYR)THEN
               IF (CO2_PLTSW(IECP) .GT. 0.0)THEN
!                        if (curitr .eq. 1 .and. iecp .eq. wiwn .and. irg .eq. 12)write(6,3433) curiyr+1989,col,irg,uplntcd(iecp),tfr
!3433 format(1h ,'!genint0',i4,a10,i3,a3,i3)
                  CO2LB = 0.0
                  IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                     ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                     VALUE = GEN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                     CALL DVAL(COL,ROW_GPSN,VALUE,COL_mask,ROW_GPSN_mask,'OPINT,10')
                  END IF
                  ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                  VALUE = GEN * 0.001 * CO2_PLTSW(IECP)
                  CALL DVAL(COL,ROW_GPSN,VALUE,COL_mask,ROW_GPSN_mask,'OPINT,11')
               END IF
            END IF

!           NOX constraint

            IF (IP .LE. EFD_D_DSP) THEN
              DO INOX = 1, NOX_GRP
                WRITE(NOXCODE,'(I1)') INOX
                IF (EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
                   ROW_NOX = 'ELNOX' // NOXCODE // UPLNTCD(IECP); call makmsk(ROW_NOX_mask,':ELNOX:' , NOXCODE , UPLNTCD(IECP))
                ELSE
                  ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE)
                END IF
                NOXEM = GEN * HTRT * UG_NOXC(IS,INOX,N) * 0.0005 * 0.001
                IF (NOXEM .GT. 0.001) THEN
                   IF (EMRFNA(INOX,CURIYR) .GT. 0.0) TST_NOX(IECP,INOX) = 1
                   CALL DVAL(COL,ROW_NOX,NOXEM,COL_mask,ROW_NOX_mask,'OPINT,12')
                END IF
              ENDDO
            END IF

!           RPS constraint if in place

!           IF (UPRNWBND(CURIYR) .GT. 0.005 .AND. UPRNWSHR(IECP) .NE. 0.0) THEN
!             ROWRPS = 'RPSREQXX'
!             IF (UPRNWSHR(IECP) .GT. 0.0) THEN
!               VAL = GEN * UPRNWSHR(IECP) * 0.001
!             ELSE
!               VAL = GEN * UPRNWBND(CURIYR) * 0.001
!               IF (UPRNWCAS .EQ. 3) VAL = VAL / (1.0 + EQTDLS * ULOSSADJ(CURIYR))
!             ENDIF
!             CALL DVAL(COL,ROWRPS,DBLE(VAL))
!           ENDIF ! RPS
            IF (USW_EFDRPS .GT. 0 .AND. UPRNWCAS .GT. 0 .AND. UPRNWBND(CURIYR) .GT. 0.005)THEN
!              QUALIFYING GENERATION
               IF (UPRNWSHR(IECP) .GT. 0.0)THEN
                  VAL = GEN * 0.001 * UPRNWSHR(IECP)
                  ROWRPS = 'RPSREQ' // UPRGCD(IRG); call makmsk(ROWRPS_mask,':RPSREQ:' , UPRGCD(IRG))
                  CALL DVAL(COL,ROWRPS,DBLE(VAL),COL_mask,ROWRPS_mask,'OPINT,13')
!p                ROWRPS = 'RPSPGN' // UPLNTCD(IECP)
!p                CALL DVAL(COL,ROWRPS,DBLE(VAL))
               END IF
!              NON QUALIFYING GENERATION
!n             IF (UPRNWSHR(IECP) .LT. 1.0) THEN
!n                VAL = GEN * (1.0 - UPRNWSHR(IECP)) * 0.001
!n                ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!n                CALL DVAL(COL,ROWRPS,DBLE(VAL))
!n                ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!n                CALL DVAL(COL,ROWRPS,DBLE(VAL))
!n             ENDIF ! RPS
!              EXCLUDE EXISTING GENERATION, IF APPROPRIATE
!              IF (UPRNWEXG(IECP) .GT. 0.0 .AND. ULVINT(IGRP) .EQ. 1) THEN
!                 VAL = GEN * UPRNWEXG(IECP) * 0.001
!                 ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!                 CALL DVAL(COL,ROWRPS,DBLE(VAL))
!                 ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!                 CALL DVAL(COL,ROWRPS,DBLE(VAL))
!              ENDIF ! RPS
            ENDIF ! RPS
!
!              GRID RESILIENCE ROWS
!
!                 if (irg .eq. 1 .and. iecp .eq. wipv .and. curitr .eq. 1)write(6,3344) curiyr+1989,col,  &
!                    GEN * 0.001 , GRD_RATS(IECP,IRG) , GRD_TGTS(CURIYR,IRG),  & 
!                       GEN * 0.001 * (GRD_RATS(IECP,IRG) - GRD_TGTS(CURIYR,IRG)),  &
!                       GEN * 0.001 * GRD_RATS(IECP,IRG)
!3344 format(1h ,'!grdpv',i4,a10,6f10.4)
            DO IGR = 1 , GRD_CASN
               IF (GRD_RATSA(IECP,IRG,IGR) .GE. 0.0)THEN
                  ROW_GRD = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                  IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                     VAL = GEN * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                  ELSE
                     VAL = GEN * 0.001 * GRD_RATSA(IECP,IRG,IGR)
                  END IF
!                 if (curitr .eq. 1 .and. (irg .eq. 1 .or. irg .eq. 20))write(6,4455) curiyr + 1989, irg, uplntcd(iecp),col,  &
!                          GEN * 0.001 , GRD_RATS(IECP,IRG) , GRD_TGTS(CURIYR,IRG), val
!4455 format(1h ,'!grdint',i4,i3,a3,a10,5f10.4)
                  IF (DBLE(VAL) .NE. DBLE(0.0))CALL DVAL(COL,ROW_GRD,DBLE(VAL),COL_mask,ROW_GRD_mask,'OPINT,14')

                  ROW_GRD = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                  VAL = GEN * 0.001
                  CALL DVAL(COL,ROW_GRD,DBLE(VAL),COL_mask,ROW_GRD_mask,'OPINT,15')
               END IF
            END DO

          ENDIF !CF > 0
        ENDDO   !IVSL
      ENDDO    !SEASON

      RETURN
      END

!     This subroutine sets up operates for storage plants

      SUBROUTINE OPSTR(N)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'plntctl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'elcntl'
      include'emission'
      include'dispett'
      include'uecpout'
      include'cdsparms'
      include'eusprc'
      include'edbdef'
      include'wrenew'
      include'ecp_nuc'
      include'csapr'
      include'emmemis'

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,IFR, D, M, H
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,MRSW,INOX,FSL
      REAL VOM,HTRT,CAP(EFD_D_MSP),HRVAL,PTC,CF,VAL,RPSCST,RPSU,RPSR,CO2LB
      REAL*8 DRAT,GEN,GEN_SR,OBJVAL,BTUREQ,MRCAP,NOXEM,VAL_SR,VALUE,VALSH
      CHARACTER*1 FSLCD,NOXCODE
      CHARACTER*5 NUM
      CHARACTER*16 COL,LOAD,ROW_NOX,ROWRPS,ROW_SR,ROW_GPS,ROW_GPSN

      efdsub='OPSTR'

      RPSCST = 0.0

      IRG = UG_EMM_RG(N)
      IP = UG_EFDt(N)
      IECP = UG_ECPt(N)
      IGRP = EFD_GRPS_F(N)
      TFR = UG_FL_RG(N)
      VOM = UG_OMR(N)
      PTC = UG_GSUB(N)

      WRITE(NUM,'(I5.5)') IGRP

!     loop over time season and slices

      DO IS = 1, EENSP

        HTRT = UG_HTRT(IS,N)

        IF (FCRL .EQ. 1) THEN
           write(UF_DBG,4263) CURIYR,CURITR,IS,N,IGRP,IP,TFR,VOM,PTC,HTRT
 4263      format(1x,'OPSTR ',7(":",I6),3(":",F10.2))
        ENDIF

        CAP(IS) = UG_CAP(IS,N)
  
        DO IVSL = 1 , ELNVCT(IS)
          GRP = ELGRP(IVSL,IS)
          SEG = ELSEG(IVSL,IS)
          CF = UG_GCF(SEG,GRP,N)

          IF (NET_STORAGE_LOAD_EFD(SEG,GRP,IRG,CURIYR) .NE. 0.0) THEN
             FSL = EFD_Slice_ID(GRP,SEG)
             FSLCD = CHAR(64+FSL)

             COL = 'I'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_mask,':I:',NUM,CHCOD(IS),FSLCD)

            LOAD = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(LOAD_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)

            GEN = ELWDTH(IVSL,IS) * NET_STORAGE_LOAD_EFD(SEG,GRP,IRG,CURIYR)
            CALL DVAL(COL,LOAD,DBLE(0.1 * GEN),COL_mask,LOAD_mask,'OPSTR,1')

            IF (FCRL .EQ. 1 .AND. CURIYR+198 .LE. 2025) THEN
               WRITE(18,4911) CURIRUN, CURIYR+1989, CURITR, N, IRG, IP, IECP, IGRP, TFR, IS, IVSL, GRP, SEG, COL, GEN, ELWDTH(IVSL,IS), NET_STORAGE_LOAD_EFD(SEG,GRP,IRG,CURIYR), CF
 4911          FORMAT(1X,"NET_STORAGE_LOAD_EFD_INFO", 13(",",I6),",",A16, 4(",",F21.6))

            END IF
            
           IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                ROW_SR = 'SR' // URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(ROW_SR_mask,':SR:' , URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)
                GEN_SR = NET_STORAGE_SR_EFD(SEG,GRP,IRG,CURIYR)
                CALL DVAL(COL,ROW_SR,GEN_SR,COL_mask,ROW_SR_mask,'OPSTR,2')
                
                IF (FCRL .EQ. 1 .AND. CURIYR+198 .LE. 2025) THEN
                   WRITE(18,4912) CURIRUN, CURIYR+1989, CURITR, IRG, IP, IECP, IS, IVSL, GRP, SEG, COL, GEN_SR, NET_STORAGE_SR_EFD(SEG,GRP,IRG,CURIYR)
4912          FORMAT(1X,"NET_STORAGE_SR_EFD_INFO", 10(",",I6),",",A16, 2(",",F21.6))
                END IF
            END IF

!           set bound on column

            CALL DBND(EFDBND,COL,DBLE(CAP(IS)*0.001),DBLE(CAP(IS)*0.001),COL_mask,'OPSTR,4')

          ENDIF !NET_STORAGE_LOAD <> 0
        ENDDO   !IVSL
      ENDDO    !SEASON

      RETURN
      END
!
!     This subroutine sets up operates for hydro and pumped storage
!       It assumes run of river, hydro can be used anywhere within a season,
!       subject to a total energy constraint by season
!       Pumped storage is assumed to use a fixed amount in every time slice
!

      SUBROUTINE OPHYD(N)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'plntctl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'elcntl'
      include'dispett'
      include'uecpout'
      include'cdsparms'
      include'emission'
      include'eusprc'
      include'edbdef'
      include'ecp_nuc'
      include'csapr'
      include'emmemis'
      include'emm_aimms'
!
!      COMMON /GRDSRC/ GRD_CASN,GRD_SRCN,GRD_SRCC
!      INTEGER GRD_CASN                                       ! Number of grid resilience sources 
!      CHARACTER*15 GRD_SRCN(MX_GRDSRC)                       ! Grid resilience source names
!      CHARACTER*1  GRD_SRCC(MX_GRDSRC)                       ! Grid resilience source codes

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,IRET,IFR,IGR
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,MRSW,INOX,FSL
      REAL CF,VOM,HTRT,CAP(EFD_D_MSP),HRVAL,MAXCF(EFD_D_MSP),PTC,RPSCST,RPSU,RPSR,CO2LB,TST
      REAL PSFAC,PSFAC2,MXNRG,NRGUSE,VAL
      REAL*8 DRAT,GEN,OBJVAL,BTUREQ,MRCAP,NOXEM,GEN_MIN,VAL_SR,CF_MIN,VALUE,VALSH
      CHARACTER*1 FSLCD,NOXCODE
      CHARACTER*5 NUM
      CHARACTER*16 COL,LOAD,ROW_NOX,ROWNRG,ROWRPS,ROWSTOR,P2ROW,COL_MIN,ROW_SR,ROW_H,ROW_GPS,ROW_GPSN,ROW_GRD
      character*30 ROW_GRD_mask

      REAL*8   DIGITS2
 
      efdsub='OPHYD'

      RPSCST = 0.0

      IRG = UG_EMM_RG(N)
      IP = UG_EFDt(N)
      IECP = UG_ECPt(N)
      IGRP = EFD_GRPS_F(N)
      TFR = UG_FL_RG(N)
      VOM = UG_OMR(N)
      PTC = UG_GSUB(N)

      MRSW = UG_MRUN(N)
      PSFAC = UFACPS
      PSFAC2 = UFACP2
!
!     STORE 111d SWITCH FOR MASS OR RATE STANDARDS FOR REGION
!
      CO2_PLTSW(IECP) = CO2_PLTRG(IECP,IRG)

      WRITE(NUM,'(I5.5)') IGRP

!     loop over time season and slices

      DO IS = 1, EENSP

        HTRT = UG_HTRT(IS,N)

        IF (FCRL .eq. 1) THEN
           write(UF_DBG,4263) CURIYR,CURITR,IS,N,IGRP,IP,TFR,VOM,PTC,HTRT
 4263      format(1x,'OPHYD ',7(":",I6),3(":",F10.2))
        ENDIF

        CAP(IS) = UG_CAP(IS,N)
        MAXCF(IS) = UG_SCF(IS,N)
  
        DIGITS_PARM = 6
        IF (USW_DIGIT .GT. 0)THEN
        TST = DIGITS2(DBLE(CAP(IS) * 0.001),DIGITS_PARM)
        ELSE
           TST = DBLE(CAP(IS) * 0.001)
        END IF
        
         IF (IECP .NE. WIHY .OR. CURIYR + UHBSYR .LT. UPSTYR) THEN
            ROWNRG = 'EN'//NUM//CHCOD(IS); call makmsk(ROWNRG_mask,':EN:',NUM,CHCOD(IS)) 
         END IF

         DO IVSL = 1 , ELNVCT(IS)
          GRP = ELGRP(IVSL,IS)
          SEG = ELSEG(IVSL,IS)
          FSL = EFD_Slice_ID(GRP,SEG)
          FSLCD = CHAR(64+FSL)

          COL = 'H'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_mask,':H:',NUM,CHCOD(IS),FSLCD)

          IF (IECP .EQ. WIHY .AND. CURIYR + UHBSYR .GE. UPSTYR) THEN
             CF = (1.0 - WFOR(IP)) * HY_CF_EFD(SEG,GRP,IRG)   ! Use results from 864 Model to set Pondage Hydro Generation

             IF (FCRL .EQ. 1) &
                WRITE(18,5263) CURIRUN, CURIYR+1989, CURITR, IRG, IGRP, IP, IECP, IS, SEG, IVSL, COL, CAP(IS), WFOR(IP), HY_CF_EFD(SEG,GRP,IRG), ELWDTH(IVSL,IS), CF, ELWDTH(IVSL,IS) * CF
 5263        FORMAT(1X,"EFD_HY_GEN_IN",10(",",I5),",",A16,6(",",F21.6))

          ELSE
             CF = (1.0 - WFOR(IP))  !can operate up to (1-for) in any given slice
          END IF

          LOAD = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(LOAD_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)
          GEN = CF * ELWDTH(IVSL,IS)
          CALL DVAL(COL,LOAD,DBLE(0.1 * GEN),COL_mask,LOAD_mask,'OPHYD,1')

          IF (SR_CREDIT(IECP) .GT. 0.0 .AND. (IECP .NE. WIHY .OR. CURIYR + UHBSYR .LT. UPSTYR)) THEN
             ROW_H =  'F'//NUM//CHCOD(IS)//FSLCD; call makmsk(ROW_H_mask,':F:',NUM,CHCOD(IS),FSLCD)
             CALL DROWTYPE(ROW_H,'L       ',ROW_H_mask)
             CALL DRHS(EFDRHS,ROW_H,DBLE(CAP(IS)*0.001),ROW_H_mask,'OPHYD,2')

             CALL DVAL(COL,ROW_H,DBLE(1.0),COL_mask,ROW_H_mask,'OPHYD,3')

             ROW_SR = 'SR' // URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(ROW_SR_mask,':SR:' , URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)
             VAL_SR = SR_CREDIT(IECP) * (1.0 - CF)
             CALL DVAL(COL,ROW_SR,VAL_SR,COL_mask,ROW_SR_mask,'OPHYD,4')

             COL_MIN = 'F'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_MIN_mask,':F:',NUM,CHCOD(IS),FSLCD)
             CALL DVAL(COL_MIN,ROW_H,DBLE(1.0),COL_MIN_mask,ROW_H_mask,'OPHYD,5')

             CF_MIN = SR_MIN_CF(IECP)
             VAL_SR = SR_CREDIT(IECP) * (1.0 - CF_MIN)
             CALL DVAL(COL_MIN,ROW_SR,VAL_SR,COL_MIN_mask,ROW_SR_mask,'OPHYD,6')

             GEN_MIN = CF_MIN * ELWDTH(IVSL,IS)
             CALL DVAL(COL_MIN,LOAD,DBLE(0.1 * GEN_MIN),COL_MIN_mask,LOAD_mask,'OPHYD,7')

             CALL DBND(EFDBND,COL_MIN,0.0D0,DBLE(CAP(IS)*0.001),COL_MIN_mask,'OPHYD,8')
          END IF
!
!         set bound on column
!
          CALL DBND(EFDBND,COL,0.0D0,DBLE(CAP(IS)*0.001),COL_mask,'OPHYD,9')
!
!         create energy use row for pumped storage
!
          IF (IP .EQ. UIHYR) THEN
             IF (IECP .EQ. WIP2) THEN
                IF (MAXCF(IS) .GT. 0.0) THEN
                   ROWSTOR = 'P2_' // URGNME(IRG)(1:4) // CHCOD(IS); call makmsk(ROWSTOR_mask,':P2_:' , URGNME(IRG)(1:4) , CHCOD(IS))
                   NRGUSE = GEN * PSFAC2
                   CALL DVAL(COL,ROWSTOR,DBLE(0.1*NRGUSE),COL_mask,ROWSTOR_mask,'OPHYD,10')

                   P2ROW = 'P2' // FSLCD // URGNME(IRG)(1:4) // CHCOD(IS); call makmsk(P2ROW_mask,':P2:' , FSLCD , URGNME(IRG)(1:4) , CHCOD(IS))
                   CALL DVAL(COL,P2ROW,DBLE(0.1*GEN),COL_mask,P2ROW_mask,'OPHYD,11')
                ENDIF
             ELSE
                IF (MAXCF(IS) .GT. 0.0) THEN
                   ROWSTOR = 'PS_' // URGNME(IRG)(1:4) // CHCOD(IS); call makmsk(ROWSTOR_mask,':PS_:' , URGNME(IRG)(1:4) , CHCOD(IS))
                   NRGUSE = GEN * PSFAC
                   CALL DVAL(COL,ROWSTOR,DBLE(0.1*NRGUSE),COL_mask,ROWSTOR_mask,'OPHYD,12')

                   IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                      VAL_SR = GEN_MIN * PSFAC
                      CALL DVAL(COL_MIN,ROWSTOR,DBLE(0.1*VAL_SR),COL_MIN_mask,ROWSTOR_mask,'OPHYD,13')
                   ENDIF
                ENDIF
             ENDIF
          ENDIF
!
!         intersection with OBJ
!
          RPSR = 0.0
          RPSU = 0.0
          RPSCST = 0.0
          IF (UPRNWREG .EQ. 1) THEN

!            CREDIT PRICE ADJUSTMENT DEPENDS ON WHETHER EXISTING GEN IS GIVEN CREDIT

             IF (UPRNWEXG(IECP) .GT. 0.0 .AND. ULVINT(IGRP) .EQ. 1)THEN
                RPSCST = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP) * (1.0 - UPRNWEXG(IECP)))
             ELSE
                RPSCST = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
             ENDIF
             IF (UPRNWBAS(IECP) .GT. 0.0)RPSCST = 0.0
          ELSEIF (UPRNWREG .EQ. 2) THEN
             IF (UPRNWEXGR(IECP,IRG) .GT. 0.0 .AND. ULVINT(IGRP) .EQ. 1)THEN
                RPSCST = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG) * (1.0 - UPRNWEXGR(IECP,IRG)))
             ELSE
                RPSCST = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
             ENDIF
          ELSEIF (UPRNWREG .EQ. 3) THEN
             IF (UPRNWEXG(IECP) .GT. 0.0 .AND. ULVINT(IGRP) .EQ. 1)THEN
                RPSU = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP) * (1.0 - UPRNWEXG(IECP)))
             ELSE
                RPSU = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
             ENDIF
             IF (UPRNWBAS(IECP) .GT. 0.0)RPSU = 0.0
             IF (UPRNWEXGR(IECP,IRG) .GT. 0.0 .AND. ULVINT(IGRP) .EQ. 1)THEN
                RPSR = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG) * (1.0 - UPRNWEXGR(IECP,IRG)))
             ELSE
                RPSR = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
             ENDIF
             IF (EPRPSPR(CURIYR) .GE. EPRPSPRR(IRG,CURIYR))THEN
                RPSCST = RPSU
             ELSE
                RPSCST = RPSR
             END IF
          ENDIF

!          IF (ST_RPS_SW .GT. 0) THEN
!             RPSR = ST_RPS_EMM_P(IRG,CURIYR) * (ST_RNW_BND(CURIYR,IRG) - ST_RNW_SHR(IECP,IRG,CURIYR))
!             IF (RPSCST .LT. RPSR) RPSCST = RPSR
!          END IF
          IF (ST_RPS_SW .GT. 0) THEN
             RPSR = ST_RPS_EMM_P(IRG,CURIYR) * (ST_RNW_BND(CURIYR,IRG) - ST_RNW_SHR(IECP,IRG,CURIYR))
             IF (RPSCST .LT. 0.0 .AND. RPSR .LE. 0.0) THEN  ! if both are negative use the greatest benefit
                  IF (ABS(RPSCST) .LT. ABS(RPSR)) RPSCST = RPSR
             ELSEIF (RPSCST .GT. 0.0 .AND. RPSR .GT. 0.0) THEN     ! if both are positive, use the highest positive cost
                  IF (RPSCST .LT. RPSR) RPSCST = RPSR
             ELSE                        ! use net value
                     RPSCST = RPSCST + RPSR
             ENDIF
          END IF

          OBJVAL = (VOM + RPSCST - PTC) * GEN * 0.001
          CALL DVAL(COL,EFDOBJ,OBJVAL,COL_mask,EFDOBJ,'OPHYD,14')

!         intersection with energy constraint

          IF (IECP .NE. WIHY .OR. CURIYR + UHBSYR .LT. UPSTYR) THEN

             CALL DVAL(COL,ROWNRG,DBLE(0.1*GEN),COL_mask,ROWNRG_mask,'OPHYD,15')
   
             IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                VAL_SR = (VOM + RPSCST - PTC) * GEN_MIN * 0.001
                CALL DVAL(COL_MIN,EFDOBJ,VAL_SR,COL_MIN_mask,EFDOBJ,'OPHYD,16')
   
                CALL DVAL(COL_MIN,ROWNRG,DBLE(0.1*GEN_MIN),COL_MIN_mask,ROWNRG_mask,'OPHYD,17')
             END IF

          ENDIF
!
!         EMM  REGION INTENSITY STANDARD
!
          IF ((CURIYR + UHBSYR) .GT. UESTYR)THEN
             CO2LB = 0.0
             IF (CO2_PLTSW(IECP) .GT. 0.0)THEN
                IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                   ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                   VALUE = GEN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP) 
                   CALL DVAL(COL,ROW_GPSN,VALUE,COL_mask,ROW_GPSN_mask,'OPHYD,22')
                END IF
                ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                VALUE = GEN * 0.001 * CO2_PLTSW(IECP)
                CALL DVAL(COL,ROW_GPSN,VALUE,COL_mask,ROW_GPSN_mask,'OPHYD,23')

                IF (IECP .NE. WIP2 .AND. SR_CREDIT(IECP) .GT. 0.0 .AND. (IECP .NE. WIHY .OR. CURIYR + UHBSYR .LT. UPSTYR)) THEN
                   IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                      ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                      VALUE = GEN_MIN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP) 
                      CALL DVAL(COL_MIN,ROW_GPSN,VALUE,COL_MIN_mask,ROW_GPSN_mask,'OPHYD,24')
                   END IF
                   ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
                   VALUE = GEN_MIN * 0.001 * CO2_PLTSW(IECP)
                   CALL DVAL(COL_MIN,ROW_GPSN,VALUE,COL_MIN_mask,ROW_GPSN_mask,'OPHYD,25')
                END IF
             END IF
          END IF

!         NOX constraint

          IF (IP .LE. EFD_D_DSP) THEN
             DO INOX = 1, NOX_GRP
                WRITE(NOXCODE,'(I1)') INOX
                IF (EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
                   ROW_NOX = 'ELNOX' // NOXCODE // UPLNTCD(IECP); call makmsk(ROW_NOX_mask,':ELNOX:' , NOXCODE , UPLNTCD(IECP))
                ELSE
                   ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE)
                END IF
                NOXEM = GEN * HTRT * UG_NOXC(IS,INOX,N) * 0.0005 * 0.001
                IF (NOXEM .GT. 0.001) THEN
                   IF (EMRFNA(INOX,CURIYR) .GT. 0.0) TST_NOX(IECP,INOX) = 1
                   CALL DVAL(COL,ROW_NOX,NOXEM,COL_mask,ROW_NOX_mask,'OPHYD,26')
                END IF

                IF (SR_CREDIT(IECP) .GT. 0.0 .AND. (IECP .NE. WIHY .OR. CURIYR + UHBSYR .LT. UPSTYR)) THEN
                   VAL_SR = GEN_MIN * HTRT * UG_NOXC(IS,INOX,N) * 0.0005 * 0.001
                   IF (VAL_SR .GT. 0.001) THEN
                      CALL DVAL(COL_MIN,ROW_NOX,VAL_SR,COL_MIN_mask,ROW_NOX_mask,'OPHYD,27')
                   END IF
                END IF

             ENDDO
          END IF

!         RPS constraint if in place

!         IF (UPRNWBND(CURIYR) .GT. 0.005 .AND. UPRNWSHR(IECP) .NE. 0.0) THEN
!            ROWRPS = 'RPSREQXX'
!            IF (UPRNWSHR(IECP) .GT. 0.0) THEN
!               VAL = GEN * UPRNWSHR(IECP) * 0.001
!            ELSE
!               VAL = GEN * UPRNWBND(CURIYR) * 0.001
!               IF (UPRNWCAS .EQ. 3) VAL = VAL / (1.0 + EQTDLS * ULOSSADJ(CURIYR))
!            ENDIF
!            CALL DVAL(COL,ROWRPS,DBLE(VAL))
!         ENDIF ! RPS

          IF (USW_EFDRPS .GT. 0 .AND. UPRNWCAS .GT. 0 .AND. UPRNWBND(CURIYR) .GT. 0.005)THEN

!            if (curitr .gt. 1)write(6,3210) curiyr+1989,irg,igrp,ulvint(igrp),uplntcd(iecp),col,uprnwshr(iecp),uprnwexg(iecp),gen
!3210        format(1h ,'!ophyd',i4,i3,i6,i3,a3,a10,2f6.3,f10.3)

!            QUALIFYING GENERATION

             IF (UPRNWSHR(IECP) .GT. 0.0)THEN
                VAL = GEN * 0.001 * UPRNWSHR(IECP)
                ROWRPS = 'RPSREQ' // UPRGCD(IRG); call makmsk(ROWRPS_mask,':RPSREQ:' , UPRGCD(IRG))
                CALL DVAL(COL,ROWRPS,DBLE(VAL),COL_mask,ROWRPS_mask,'OPHYD,28')

                IF (SR_CREDIT(IECP) .GT. 0.0 .AND. (IECP .NE. WIHY .OR. CURIYR + UHBSYR .LT. UPSTYR)) THEN
                   VAL_SR = GEN_MIN * 0.001 * UPRNWSHR(IECP)
                   CALL DVAL(COL_MIN,ROWRPS,VAL_SR,COL_MIN_mask,ROWRPS_mask,'OPHYD,29')
                END IF

!p              ROWRPS = 'RPSPGN' // UPLNTCD(IECP)
!p              CALL DVAL(COL,ROWRPS,DBLE(VAL))

             END IF

!            NON QUALIFYING GENERATION
!n           IF (UPRNWSHR(IECP) .LT. 1.0) THEN
!n              VAL = GEN * (1.0 - UPRNWSHR(IECP)) * 0.001
!n              ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!n              CALL DVAL(COL,ROWRPS,DBLE(VAL))
!n              ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!n              CALL DVAL(COL,ROWRPS,DBLE(VAL))
!n           ENDIF ! RPS
!            EXCLUDE EXISTING GENERATION, IF APPROPRIATE
!            IF (UPRNWEXG(IECP) .GT. 0.0 .AND. ULVINT(IGRP) .EQ. 1) THEN
!               VAL = GEN * UPRNWEXG(IECP) * 0.001
!               ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!               CALL DVAL(COL,ROWRPS,DBLE(VAL))
!               ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!               CALL DVAL(COL,ROWRPS,DBLE(VAL))
!            ENDIF ! RPS

          ENDIF ! RPS
!
!         GRID RESILIENCE ROWS
!
          DO IGR = 1 , GRD_CASN
             IF (GRD_RATSA(IECP,IRG,IGR) .GE. 0.0)THEN
                ROW_GRD = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                   VAL = GEN * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                ELSE
                   VAL = GEN * 0.001 * GRD_RATSA(IECP,IRG,IGR)
                END IF
                IF (DBLE(VAL) .NE. DBLE(0.0))CALL DVAL(COL,ROW_GRD,DBLE(VAL),COL_mask,ROW_GRD_mask,'OPHYD,30')

                ROW_GRD = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                VAL = GEN * 0.001
                CALL DVAL(COL,ROW_GRD,DBLE(VAL),COL_mask,ROW_GRD_mask,'OPHYD,31')

                IF (SR_CREDIT(IECP) .GT. 0.0 .AND. (IECP .NE. WIHY .OR. CURIYR + UHBSYR .LT. UPSTYR)) THEN
                   ROW_GRD = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                   IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                      VAL_SR = GEN_MIN * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
                   ELSE
                      VAL_SR = GEN_MIN * 0.001 * GRD_RATSA(IECP,IRG,IGR)
                   END IF
                   IF (VAL_SR .NE. DBLE(0.0))CALL DVAL(COL_MIN,ROW_GRD,VAL_SR,COL_MIN_mask,ROW_GRD_mask,'OPHYD,32')
 
                   ROW_GRD = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
                   VAL_SR = GEN_MIN * 0.001
                   CALL DVAL(COL_MIN,ROW_GRD,VAL_SR,COL_MIN_mask,ROW_GRD_mask,'OPHYD,33')
                END IF
             END IF
          END DO

        ENDDO   !IVSL
      ENDDO    !SEASON

      RETURN
      END
!
!     This subroutine sets up operates for distributed generation plants
!       It determines the appropriate operating modes based on min and max CF
!
      SUBROUTINE OPDG(N)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'plntctl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'emission'
      include'dispett'
      include'uecpout'
      include'cdsparms'
      include'emeblk'
      include'eusprc'
      include'edbdef'
      include'ecp_nuc'
      include'csapr'
      include'emmemis'
      include'emm_aimms'
!
!      COMMON /GRDSRC/ GRD_CASN,GRD_SRCN,GRD_SRCC
!      INTEGER GRD_CASN                                       ! Number of grid resilience sources 
!      CHARACTER*15 GRD_SRCN(MX_GRDSRC)                       ! Grid resilience source names
!      CHARACTER*1  GRD_SRCC(MX_GRDSRC)                       ! Grid resilience source codes

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,LOAD,IST,STA,IFR,IGR
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,INOX,FSL
      REAL CF(EFD_D_MVS+1,EFD_D_MSP),VOM,HTRT,CAP(EFD_D_MSP),HRFAC,MAXCF(EFD_D_MSP),PTC,RPSCST,RPSU,RPSR,GPSCST
      REAL MRCAP,ADJ,OGCAR,CO2LB
      REAL*8 DRAT,VAL,GEN,OBJVAL,BTUREQ,NOXEM,VALSH
      CHARACTER*1 FSLCD,NOXCODE
      CHARACTER*5 NUM
      CHARACTER*16 COL,ROW,LD_ROW,ROW_NOX,ROWRPS,ROWCARR,ROW_GPS,ROW_GPSN,SAF,ROW_GRD
      character*30 ROW_GRD_mask

      efdsub='OPDG'


      RPSCST = 0.0

!     calculate CF at each load slice

      CF = 0.0
      DO IS = 1, EENSP
        DO IVSL = 2 , ELNVCT(IS) + 1
          hrfac = ELWDTH(IVSL-1,IS)/EETIME(IS)
          CF(IVSL,IS) = CF(IVSL-1,IS) + hrfac
        ENDDO
      ENDDO

      IRG = UG_EMM_RG(N)
      IP = UG_EFDt(N)
      IECP = UG_ECPt(N)
      IGRP = EFD_GRPS_F(N)
      TFR = UG_FL_RG(N)
      VOM = UG_OMR(N)
      PTC = UG_GSUB(N)

      DO IS = 1, EENSP
          CAP(IS) = UG_CAP(IS,N)
          MAXCF(IS) = UG_SCF(IS,N)
          HTRT = UG_HTRT(IS,N)

          IF (FCRL .EQ. 1 .AND. CURIYR .EQ. 28) THEN
             write(UF_DBG,3263) CURIYR,CURITR,IS,N,IGRP,IP,TFR,VOM,PTC,HTRT,CAP(IS),MAXCF(IS)
 3263        format(1x,'OPDG ',7(":",I6),5(":",F10.2))
          ENDIF
      ENDDO
!
!     STORE 111d SWITCH FOR MASS OR RATE STANDARDS FOR REGION
!
      CO2_PLTSW(IECP) = CO2_PLTRG(IECP,IRG)

      IGS = 0
      DO IFL = 1, EFD_D_FPP
        if (WFLTP(IP,IFL) .NE. 0) THEN
          IF (UIGAS(WFLTP(IP,IFL)) .EQ. 1) IGS = 1
        endif
      ENDDO

      WRITE(NUM,'(I5.5)') IGRP

!     loop over season and slices to determine correct mode

      DO IS = 1, EENSP

        HTRT = UG_HTRT(IS,N)

        DO MODE = 1 , ELNVCT(IS)
          IF (WUPPER(IP) .GE. CF(MODE,IS) .AND. WLOWER(IP) .LE. CF(MODE+1,IS)) THEN
            LOAD = MODE
          ENDIF
        ENDDO
        COL = 'D'//NUM//CHCOD(IS)//CHCOD(LOAD); call makmsk(COL_mask,':D:',NUM,CHCOD(IS),CHCOD(LOAD))
        GEN = 0.0

!       set up intersection with load rows

        DO IVSL = 1, LOAD
          GRP = ELGRP(IVSL,IS)
          SEG = ELSEG(IVSL,IS)
          FSL = EFD_Slice_ID(GRP,SEG)
          FSLCD = CHAR(64+FSL)
          LD_ROW = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(LD_ROW_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)
          DRAT = 1.0
          IF (IVSL .EQ. LOAD) THEN
            ADJ = (WUPPER(IP) - CF(IVSL,IS)) / (CF(IVSL+1,IS) - CF(IVSL,IS))
          ELSE
            ADJ = 1.0
          ENDIF
          VAL = ADJ * DRAT * ELWDTH(IVSL,IS)
          CALL DVAL(COL,LD_ROW,DBLE(0.1 * VAL),COL_mask,LD_ROW_mask,'OPDG,1')
          GEN = GEN + VAL

!         capacity balance if EPPOPM=2

          IF (EPPOPM(IP) .EQ. 2) THEN
            ROW = 'CP'//CHCOD(IRG)//EPPLCD(IP)//CHCOD(IS)//FSLCD; call makmsk(ROW_mask,':CP:',CHCOD(IRG),EPPLCD(IP),CHCOD(IS),FSLCD)
            CALL DVAL(COL,ROW,1.0D0,COL_mask,ROW_mask,'OPDG,2')
          ENDIF
        ENDDO ! IVSL

!       set bound on column

        MRCAP = CAP(IS)*0.001
        CALL DBND(EFDBND,COL,DBLE(MRCAP),DBLE(MRCAP),COL_mask,'OPDG,3')

!       intersection with OBJ

        RPSR = 0.0
        RPSU = 0.0
        RPSCST = 0.0
        IF (UPRNWREG .EQ. 1) THEN
            RPSCST = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
            IF (UPRNWBAS(IECP) .GT. 0.0)RPSCST = 0.0
        ELSEIF (UPRNWREG .EQ. 2) THEN
            RPSCST = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
        ELSEIF (UPRNWREG .EQ. 3) THEN
            RPSU = EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
            IF (UPRNWBAS(IECP) .GT. 0.0)RPSU = 0.0
            RPSR = EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
            IF(EPRPSPR(CURIYR) .GE. EPRPSPRR(IRG,CURIYR))THEN
              RPSCST = RPSU
            ELSE
              RPSCST = RPSR
            END IF
        ENDIF

        IF (ST_RPS_SW .GT. 0) THEN
           RPSR = ST_RPS_EMM_P(IRG,CURIYR) * (ST_RNW_BND(CURIYR,IRG) - ST_RNW_SHR(IECP,IRG,CURIYR))
             IF (RPSCST .LT. 0.0 .AND. RPSR .LE. 0.0) THEN  ! if both are negative use the greatest benefit
                  IF (ABS(RPSCST) .LT. ABS(RPSR)) RPSCST = RPSR
             ELSEIF (RPSCST .GT. 0.0 .AND. RPSR .GT. 0.0)  THEN    ! if both are positive, use the highest positive cost
                  IF (RPSCST .LT. RPSR) RPSCST = RPSR
             ELSE                        ! use net value
                     RPSCST = RPSCST + RPSR
             ENDIF         
        END IF
!
!       DETERMINE CARBON CONTENT AND GPS ADDER, IF ANY
!
        OGCAR = 0.0
        CO2LB = 0.0
        IF (IECP .EQ. WIDB .OR. IECP .EQ. WIDP)THEN
           OGCAR = ENGEL(CURIYR) * 0.001
           CO2LB = (HTRT * 0.001) * OGCAR * (44.0 / 12.0) * 2204.0
        END IF

        GPSCST = 0.0
        IF (CO2_PRCSW .EQ. 10 .AND. CO2LB .GT. 0.001 .AND. CO2_PLTSW(IECP) .GT. 0.0)THEN
           IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.001)THEN
              GPSCST = (CO2LB - CO2_STDRN(IRG,CURIYR)) * MAX(0.0,(ECPPRCNL(IRG,CURIYR) - EFDPRCNL(IRG,CURIYR)))
           END IF 
           GPSCST = GPSCST * CO2_PLTSW(IECP)
        END IF 

        OBJVAL = (VOM + RPSCST + GPSCST - PTC) * GEN * 0.001   ! units million $ / GW
        CALL DVAL(COL,EFDOBJ,OBJVAL,COL_mask,EFDOBJ,'OPDG,4')

!       capacity balance

        IF (EPPOPM(IP) .EQ. 1) THEN
          ROW = 'CB'//NUM//CHCOD(IS); call makmsk(ROW_mask,':CB:',NUM,CHCOD(IS))
          CALL DVAL(COL,ROW,1.0D0,COL_mask,ROW_mask,'OPDG,5')
        ENDIF

!       BTU row for fossil or biomass

        IF (IP .LE. UIFCG .OR. IP  .EQ. UIBMS .OR. IP  .EQ. UIBIG .OR. IP .EQ. UIDGB .OR. IP .EQ. UIDGP) THEN
          BTUREQ = GEN * HTRT * 0.000001  !  trill BTU per GW
          if (BTUREQ .LT. 0.0) write(UF_DBG,*) 'OPBSLD BTUREQ < 0',IP,IECP,IGRP,GEN,HTRT,BTUREQ
          IF (UPTTYP(IECP) .LE. NW_COAL) THEN
            ROW = 'BTU'//EPFLCD(TFR)//UPLNTCD(IECP)//'XX'; call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),UPLNTCD(IECP)//'X',':X:') ! use first X to make 3-character plant type.
          ELSEIF (IGS .EQ. 1) THEN
!           NGSN = UNGSSN(IS)
            NGSN = IS
            ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//CHCOD(NGSN); call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),CHCOD(NGSN))
          ELSE
            ROW = 'BTU'//EPFLCD(TFR)//EPPLCD(IP)//'X'; call makmsk(ROW_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),'X')
            SAF = 'SAF'//EPFLCD(TFR)//EPPLCD(IP)//'X'; call makmsk(SAF_mask,':BTU:',EPFLCD(TFR),EPPLCD(IP),'X')
            VAL = -1.0
            CALL DVAL(SAF,ROW,VAL,SAF_mask,ROW_mask,'OPDG,6')
            VAL = 99999.999
            CALL DVAL(SAF,EFDOBJ,VAL,SAF_mask,EFDOBJ,'OPDG,7')

            WRITE(6,2917) CURIRUN, CURIYR+1989, IRG, TFR, IECP, COL, ROW, SAF
 2917       FORMAT(1X,"DGP_OOPS",5(":",I6),3(":",A16))

          ENDIF
          CALL DVAL(COL,ROW,BTUREQ,COL_mask,ROW_mask,'OPDG,8')
        ENDIF ! fossil

!       EMM REGION LEVEL CARBON EMISSIONS
 
        IF ((CURIYR + UHBSYR) .GT. UESTYR)THEN
           IF (CO2_PLTSW(IECP) .GT. 0.0)THEN
              IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                 ROW_GPSN = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                 VAL = GEN * 0.001 * (CO2LB - CO2_STDRN(IRG,CURIYR)) * CO2_PLTSW(IECP)
                 CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPDG,14')
              END IF
              ROW_GPSN = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':GENQNR:' , URGNME(IRG)(6:7))
              VAL = GEN * 0.001 * CO2_PLTSW(IECP)
              CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPDG,15')
              IF (CO2LB .GT. 0.0)THEN
                 ROW_GPSN = 'CO2QNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2QNR:' , URGNME(IRG)(6:7))
                 VAL = GEN * 0.001 * CO2LB * 0.001 * CO2_PLTSW(IECP)
                 CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPDG,16')
              END IF
           END IF
           IF (CO2LB .GT. 0.0)THEN
              ROW_GPSN = 'CO2TNR' // URGNME(IRG)(6:7); call makmsk(ROW_GPSN_mask,':CO2TNR:' , URGNME(IRG)(6:7))
              VAL = GEN * 0.001 * CO2LB * 0.001
              CALL DVAL(COL,ROW_GPSN,VAL,COL_mask,ROW_GPSN_mask,'OPDG,17')
           END IF
        END IF

!       NOX constraint

        DO INOX = 1, NOX_GRP
          WRITE(NOXCODE,'(I1)') INOX
          IF (EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
            ROW_NOX = 'ELNOX' // NOXCODE // UPLNTCD(IECP); call makmsk(ROW_NOX_mask,':ELNOX:' , NOXCODE , UPLNTCD(IECP))
          ELSE
            ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE)
          END IF
          NOXEM = GEN * HTRT * UG_NOXC(IS,INOX,N) * 0.0005 * 0.001
          IF (NOXEM .GT. 0.001) THEN
            IF (EMRFNA(INOX,CURIYR) .GT. 0.0) TST_NOX(IECP,INOX) = 1
            CALL DVAL(COL,ROW_NOX,NOXEM,COL_mask,ROW_NOX_mask,'OPDG,18')
          END IF
        ENDDO

!       RPS constraint if in place

!       IF (UPRNWBND(CURIYR) .GT. 0.005 .AND. UPRNWSHR(IECP) .NE. 0.0) THEN
!         ROWRPS = 'RPSREQXX'
!         IF (UPRNWSHR(IECP) .GT. 0.0) THEN
!           VAL = GEN * UPRNWSHR(IECP) * 0.001
!         ELSE
!           VAL = GEN * UPRNWBND(CURIYR) * 0.001
!           IF (UPRNWCAS .EQ. 3) VAL = VAL / (1.0 + EQTDLS * ULOSSADJ(CURIYR))
!         ENDIF
!         CALL DVAL(COL,ROWRPS,DBLE(VAL))
!       ENDIF ! RPS
        IF (USW_EFDRPS .GT. 0 .AND. UPRNWCAS .GT. 0 .AND. UPRNWBND(CURIYR) .GT. 0.005)THEN
!          QUALIFYING GENERATION
           IF (UPRNWSHR(IECP) .GT. 0.0)THEN
              VAL = GEN * 0.001 * UPRNWSHR(IECP)
              ROWRPS = 'RPSREQ' // UPRGCD(IRG); call makmsk(ROWRPS_mask,':RPSREQ:' , UPRGCD(IRG))
              CALL DVAL(COL,ROWRPS,VAL,COL_mask,ROWRPS_mask,'OPDG,19')
!p            ROWRPS = 'RPSPGN' // UPLNTCD(IECP)
!p            CALL DVAL(COL,ROWRPS,VAL)
           END IF
!          NON QUALIFYING GENERATION
!n         IF (UPRNWSHR(IECP) .LT. 1.0) THEN
!n            VAL = GEN * (1.0 - UPRNWSHR(IECP)) * 0.001
!n            ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!n            CALL DVAL(COL,ROWRPS,VAL)
!n            ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!n            CALL DVAL(COL,ROWRPS,VAL)
!n         ENDIF ! RPS
!          IF (UPRNWEXG(IECP) .GT. 0.0 .AND. ULVINT(IGRP) .EQ. 1) THEN
!             ROWRPS = 'RPSNQP' // UPLNTCD(IECP)
!             VAL = GEN * UPRNWEXG(IECP) * 0.001
!             CALL DVAL(COL,ROWRPS,VAL)
!             ROWRPS = 'RPSNQR' // UPRGCD(IRG)
!             CALL DVAL(COL,ROWRPS,VAL)
!          ENDIF ! RPS
        ENDIF ! RPS
!
!       GRID RESILIENCE ROWS
!
        DO IGR = 1 , GRD_CASN
           IF (GRD_RATSA(IECP,IRG,IGR) .GE. 0.0)THEN
              ROW_GRD = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
              IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                 VAL = GEN * 0.001 * (GRD_RATSA(IECP,IRG,IGR) - GRD_TGTS(CURIYR,IRG))
              ELSE
                 VAL = GEN * 0.001 * GRD_RATSA(IECP,IRG,IGR)
              END IF
              IF (VAL .NE. DBLE(0.0))CALL DVAL(COL,ROW_GRD,VAL,COL_mask,ROW_GRD_mask,'OPDG,20')

              ROW_GRD = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_GRD_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
              VAL = GEN * 0.001
              CALL DVAL(COL,ROW_GRD,VAL,COL_mask,ROW_GRD_mask,'OPDG,21')
           END IF
        END DO

      ENDDO    !SEASON

      RETURN
      END

!
!
!    This subroutine calls the LP retrieval routines
!

      SUBROUTINE RETEFD
      use efd_row_col
      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispout'
      include'dispuse'
      include'dispcrv'
      include'dispett'
      include'fuelin'
      include'elout'
      include'postpr'
      include'ecpcntl'
      include'udatout'
      include'bildout'
      include'uecpout'
      include'uettout'
      include'cdsparms'
      include'uso2grp'
      include'coalemm'
      include 'wrenew'
      include'e111d'
      include'uefdout'
      include'emission'
      include'csapr'
      include'emmemis'
      include'ecp_nuc'
      include'emm_aimms'
      include'ccatsdat'



      INTEGER*4  IRET,IRG,IS,IFL,IOWN,ITYPE,SIRG,IVSL,GRP,SEG,IECP,IPROV,I,I_FLRG,I_45Q,ICR
      REAL*8 TQFFL

      REAL*8 AVG_HTRT(0:ECP_D_CAP), AVG_HTRT_MR(0:ECP_D_CAP), AVG_HTRT_MOD(0:ECP_D_CAP), AVG_HTRT_MR_MOD(0:ECP_D_CAP)
      REAL*8 ECP_GEN(0:ECP_D_CAP), ECP_GEN_MR(0:ECP_D_CAP), ECP_GEN_MOD(0:ECP_D_CAP), ECP_GEN_MR_MOD(0:ECP_D_CAP)

      CHARACTER*12   FROM_LABEL

      

!     TEMP PATCH FOR BIOMASS CONSUMPTION

      INTEGER*4 CRG,STP
      CHARACTER*2 BR,ST,STAT
      CHARACTER*16 COLUMN
      COMMON /WD_OOPS/ WD_BTUS_PWR, WD_BTUS_H2
      REAL*8 WD_BTUS_PWR(NDREG), WD_BTUS_H2(NDREG)
      REAL*8 SOLVAL(5)
      CHARACTER*8 COLSOL /'ACLUD   '/

      REAL*8 DIFF111,DIFFGEN,AVDEM,CO2LB

      
     
      efdsub='RETEFD'

      DO I_45Q = 0, 2
         DO I_FLRG = 0, MAXNFR
            DO ITYPE = 0, 4
               BTU_CCS(ITYPE,I_FLRG,I_45Q,CURIYR) = 0.0
            END DO
         END DO
      END DO

      DO ICR = 1, MNUMCR
          DO ITYPE = 1,5
            SUP_EMM_NTC(ITYPE,ICR,CURIYR) = 0.0
            SUP_EMM_45Q(ITYPE,ICR,CURIYR) = 0.0
            CST_EMM_INV(ITYPE,ICR,CURIYR) = 0.0
            CST_EMM_OM(ITYPE,ICR,CURIYR) = 0.0
          ENDDO
      ENDDO
      
      if (.NOT. USE_AIMEFD_SLNADJ) then   !added by AKN to bypass FORTRAN post adjustments      
          WD_BTUS_PWR = 0.0
          DO CRG = 1 , NDREG
             WRITE(BR,'(I2.2)') CRG
             DO STP = 1 , NWDSUPP
                WRITE(ST,'(I2.2)') STP
                COLUMN = 'BP' // BR // ST; call makmsk(COLUMN_mask,':BP:' , BR , ST)  !Possible ERROR: Missing BM_TYPE_CD(I_SUPt), where I_SUPt = 1 , MNUMFS
                CALL DWFSCOL(COLUMN,COLSOL,STAT,SOLVAL,COLUMN_mask,IRET)
                IF (IRET .EQ. 0) WD_BTUS_PWR(CRG) = WD_BTUS_PWR(CRG) + SOLVAL(1)
             END DO

    !        Subtract out H2 demand here because we do not want to attribute to electricity the HMM biomass demand
    !              that is forced in here and is drawing from the same supply steps
             WD_BTUS_PWR(CRG) = WD_BTUS_PWR(CRG) - XQSBMEL(CRG,6,CURIYR)
          END DO
      ENDIF
! 
      DO IFL = 1 , NUTSEC
         DO IRG = 1 , NDREG
            UQCOAL(IFL,IRG,CURIYR) = 0.0
         END DO
      END DO
      DO IFL = 1 , NUM_CMM_UNITS + 1
         EMM_CL_BTUs(IFL,CURIYR) = 0.0
      END DO

!     determine fuel shares and consumption
       if (.NOT. USE_AIMEFD_SLNADJ) then   !added by AKN to bypass FORTRAN post adjustments      
          CALL EDO$BTU

    !     determine coal so2 emission rate results
          CALL EDO$COL

    !     determine costs and quantities associated with CO2 Intensity Standard
          CALL EDO$CPP

    !     determine benchmarking adjustments
          CALL EDO$BENCH

    !     determine ACI Costs
          CALL EDO$ACI

    !     determine biomass duals
          CALL EDO$BIO

    !     determine must run price adjustment
          CALL EDO$MRUN

    !     Record CO2 to EOR Results

          CALL EDO$CCAP

    !     determine grid resilience outputs

          CALL EDO$GRD

    !     determine national RPS duals, if any

          IF (USW_EFDRPS .GT. 0 .AND. UPRNWCAS .GT. 0 .AND. UPRNWBND(CURIYR) .GT. 0.005)CALL EDO$RPS
      endif

!  fill SO2 emission rate for non-coal plants (zeroed out in EDO$COL)
      DO IFL = NUTSEC + 1, EFD_D_NFL
        DO I_FLRG = 1, EFD_D_MFRG
          UFRSO2R(IFL,I_FLRG) = UFRSO2(IFL,I_FLRG)
        ENDDO
      ENDDO
            
! loop over region
      DO IRG = 1, UNRGNS
!
!       STORE 111d SWITCH FOR MASS OR RATE STANDARDS FOR REGION
!
        DO IECP = 1 , ECP_D_CAP
           CO2_PLTSW(IECP) = CO2_PLTRG(IECP,IRG)
        END DO

        DO IECP = 1, ECP_D_CAP
           NWGPSGEN(IECP,IRG,CURIYR) = 0.0
        ENDDO

        CALL GETIN(1,IRG)

!       WRITE(6,3977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3977   FORMAT(1X,"UEFD_09379_EEITAJ_GET",4(":",I4),3(":",F12.3))

        CALL GETBLD(1,IRG)
        CALL GETOUT(CURIYR,IRG)
!        
         if (.NOT. USE_AIMEFD_SLNADJ) then   !added by AKN to bypass FORTRAN post adjustments        
!     determine planned maintenance
            CALL EDO$PM(IRG)
!
!     determine dispatch decisions - generation by plant group and slice
            CALL EDO$OP(IRG)
!
    !     get marginal cost from load rows and get trade decisions
            CALL EDO$LOAD(IRG)
         endif

        IF (USE_AIMEFD_SLNADJ) then ! fill fuel shares and write some debug from AIMMS solution         
          Call EFD_Post_calcs(IRG)
        ENDIF
        !
        CALL STRIN(1,IRG)

!       WRITE(6,2977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!2977   FORMAT(1X,"UEFD_09396_EEITAJ_STR",4(":",I4),3(":",F12.3))

!     dispatchable results
        ITYPE = 1
        CALL ELCOST(IRG,CURIYR,ITYPE)
         DO IS = 1, EENSP

!           DG results
            ITYPE = 1
            CALL ELDGNO(IS,IRG,CURIYR,ITYPE)

!           renewable results

            CALL ELRNEWO(IS,IRG,CURIYR)
         ENDDO

         WRITE(18,2319) CURIRUN, CURIYR+1989, CURITR, IRG, (UQPGENN(UISPV,IRG,I),I=1,5)
 2319    FORMAT(1X,"EFD_PV_Curtailment_Summary",4(":",I5),5(":",F21.6))

         WRITE(18,3319) CURIRUN, CURIYR+1989, CURITR, IRG, (UQPGENN(UIWND,IRG,I),I=1,5)
 3319    FORMAT(1X,"EFD_WN_Curtailment_Summary",4(":",I5),5(":",F21.6))

!        MASS-BASED calculate e111d price adj for new renewable gen

         IF (CO2_STDSW .GT. 0  .AND. CO2_STDQS(1,CURIYR) .GT. 0.0 .AND. CO2_PRCSW .EQ. 5) THEN
            DO IECP = 1, ECP_D_CAP
               AVDEM = 0.0
               IF (NWGPSGEN(IECP,IRG,CURIYR) .GT. 0.0) THEN
                  DIFFGEN = MAX(NWGPSGEN(IECP,IRG,CURIYR) - NWGPSGEN(IECP,IRG,CURIYR-1), 0.0)
                  DIFF111 = MAX(ECO2NRPP(IRG,CURIYR) * 0.001 - ECO2NRPR(IRG,CURIYR),0.0)
                  IF (DIFFGEN .GT. 0.0) THEN
                     IF (IECP .EQ. WIWD .OR. IECP .EQ. WIBI) THEN

!                       CO2LB = UECP_HTRT(IECP,IRG,CURIYR) * 0.001 * CO2_EMSWD
!                       AVDEM = DIFFGEN * (CO2_STDRN(IRG,CURIYR) - CO2LB) * 0.001

                     ELSE

!                       AVDEM = DIFFGEN * 0.726  * 0.001

                        AVDEM = DIFFGEN * 0.726
                     ENDIF
                     IF (AVDEM .GT. 0.0) ULCO2CST(IRG,CURIYR) = ULCO2CST(IRG,CURIYR) + AVDEM * DIFF111
                  END IF

                  write(6,7940) CURIRUN, CURIYR, CURITR, IRG, IECP, NWGPSGEN(IECP,IRG,CURIYR), NWGPSGEN(IECP,IRG,CURIYR-1), &
                     DIFF111, AVDEM, ULCO2CST(IRG,CURIYR), ECO2NRPP(IRG,CURIYR) * 0.001, ECO2NRPR(IRG,CURIYR)
 7940             FORMAT(1x,"!co2rnw",5(":",I4),7(":",F21.6))

               ENDIF
            ENDDO

            write(6,7950) CURIRUN, CURIYR, CURITR, IRG, ULCO2CST(IRG,CURIYR-1), ULCO2CST(IRG,CURIYR)
 7950       FORMAT(1x,"!co2cum",4(":",I4),2(":",F21.6))

            ULCO2CST(IRG,CURIYR) = ULCO2CST(IRG,CURIYR - 1) + ULCO2CST(IRG,CURIYR)
         ENDIF

! calculate e111d price adj for new renewable gen
!
      IF (CO2_STDSW .GT. 0  .AND. CO2_STDRS(1,CURIYR) .GT. 0.0 .AND. CO2_PRCSW .EQ. 4 .AND. CO2_STDTN(IRG) .EQ. 1) THEN
        DO IECP = 1, ECP_D_CAP
          AVDEM = 0.0
          IF (CO2_PLTSW(IECP) .GT. 0.0 .OR. CO2_INCSW(IECP) .GT. 0.0) THEN
            DIFFGEN = MAX(NWGPSGEN(IECP,IRG,CURIYR) - NWGPSGEN(IECP,IRG,CURIYR-1), 0.0)
            DIFF111 = MAX(ECO2NRPP(IRG,CURIYR) * 0.001 - ECO2NRPR(IRG,CURIYR),0.0)
            IF (DIFFGEN .GT. 0.0) THEN
              IF (IECP .EQ. WIWD .OR. IECP .EQ. WIBI) THEN

                 FROM_LABEL ="RETEFD"
                 CALL ECP_AVG_HTRT(FROM_LABEL, IRG, 0, IECP, 1, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, ECP_GEN, ECP_GEN_MR, ECP_GEN_MOD, ECP_GEN_MR_MOD)
               CO2LB = AVG_HTRT(IECP) * 0.001 * CO2_EMSWD
               AVDEM = DIFFGEN * (CO2_STDRN(IRG,CURIYR) - CO2LB) * 0.001
              ELSE
               AVDEM = DIFFGEN * CO2_STDRN(IRG,CURIYR)  * 0.001
              ENDIF
              IF (AVDEM .GT. 0) ULCO2CST(IRG,CURIYR) = ULCO2CST(IRG,CURIYR) + AVDEM * DIFF111
            ENDIF
            IF (FCRL .EQ. 1) write(22,3940) '!co2rnw',CURIRUN,CURIYR,CURITR,IRG,IECP,NWGPSGEN(IECP,IRG,CURIYR),NWGPSGEN(IECP,IRG,CURIYR-1),DIFF111,AVDEM,ULCO2CST(IRG,CURIYR)
3940  FORMAT(1x,a10,5I4,6F15.5)
          ENDIF
        ENDDO
            IF (FCRL .EQ. 1) write(22,3950) '!co2cum',CURIRUN,CURIYR,CURITR,IRG,ULCO2CST(IRG,CURIYR-1),ULCO2CST(IRG,CURIYR)
3950  FORMAT(1x,a10,4I4,2F15.5)
        ULCO2CST(IRG,CURIYR) = ULCO2CST(IRG,CURIYR - 1) + ULCO2CST(IRG,CURIYR)
      ENDIF

!COMPUTE AVERAGE FUEL CHARACTERISTICS
!
      DO 100 IFL = 1 , ENFLTP
      TQFFL = DBLE(0.0)
      DO 315 IOWN = 1 , USW_OWN            ! Exclude Trad Cogen
         TQFFL = TQFFL + EQFFL(IFL,IOWN)
  315 CONTINUE
      IF (TQFFL .GT. 0.000001) THEN
      EPFUEL(IFL) = 1000.0 * ERFFL(IFL) / TQFFL
      EPFLPR(IFL) = ERFFC(IFL) / TQFFL
      EFRSO2(IFL) = 2.0 * UQFSO2(IFL,IRG) / TQFFL
      EFRNOX(IFL) = 2.0 * EQFNOX(IFL) / TQFFL
      EFRCO2(IFL) = 2.0 * EQFCO2(IFL) / TQFFL
      EFRHG(IFL) = 2000.0 * EQFHG(IFL) / TQFFL   ! mercury cont in lbs/TRILL
      EFHCNT(IFL) = EFHCNT(IFL) / TQFFL
      ELSE
      EPFUEL(IFL) = 99.99
      EFRSO2(IFL) = UFRSO2R(IFL,1)
      EFRNOX(IFL) = 0.0
      EFRCO2(IFL) = UFRCO2(IFL,1)
      EFRHG(IFL) = UFRHG(IFL,1)
      EFHCNT(IFL) = UFHCNT(IFL,1)
      END IF
  100 CONTINUE

      CALL STROUT(CURIYR,IRG)
      CALL STRBLD(1,IRG)

!   output P2/S2 by slice debug
      IF (FCRL .EQ. 1) THEN
       DO IS = 1, EENSP
          DO IVSL = 1 , ELNVCT(IS)
             GRP = ELGRP(IVSL,IS)
             SEG = ELSEG(IVSL,IS)
           write(UF_DBG,110) CURIYR,IRG,IS,IVSL,GRP,SEG,ELHGHT(IVSL,IS), &
                             UTHTP2(SEG,GRP,IRG),UTHTS2(SEG,GRP,IRG)
110   FORMAT(1x,'P2_disp',6I4,3F12.5)
          END DO
       END DO
      ENDIF

      ENDDO  ! IRG

      DO I_45Q = 0, 2
         DO I_FLRG = 0, MAXNFR
            DO ITYPE = 0, 4
               WRITE(18,2316) CURIRUN, CURCALYR, CURITR, I_45Q, I_FLRG, ITYPE, BTU_CCS(ITYPE,I_FLRG,I_45Q,CURIYR)
 2316          FORMAT(1X,"SUMMARY_BTU_CCS",6(",",I5),",",F21.6)
            END DO
         END DO
      END DO

      DO ICR = 1, MNUMCR
         DO ITYPE = 1, 5
            IF ((SUP_EMM_NTC(ITYPE,ICR,CURIYR) + SUP_EMM_45Q(ITYPE,ICR,CURIYR)) .GT. 0.0) THEN
               CST_EMM_INV(ITYPE,ICR,CURIYR) = (CST_EMM_INV(ITYPE,ICR,CURIYR) * 1000000.0) / (SUP_EMM_NTC(ITYPE,ICR,CURIYR) + SUP_EMM_45Q(ITYPE,ICR,CURIYR))
               CST_EMM_OM(ITYPE,ICR,CURIYR) = (CST_EMM_OM(ITYPE,ICR,CURIYR) * 1000000.0) / (SUP_EMM_NTC(ITYPE,ICR,CURIYR) + SUP_EMM_45Q(ITYPE,ICR,CURIYR))
            ELSE
                CST_EMM_INV(ITYPE,ICR,CURIYR) = 0.0
                CST_EMM_OM(ITYPE,ICR,CURIYR) = 0.0
            ENDIF
            WRITE(18,2317) "NTC", CURIRUN, CURCALYR, CURITR, ICR, ITYPE, SUP_EMM_NTC(ITYPE,ICR,CURIYR)
            WRITE(18,2317) "45Q", CURIRUN, CURCALYR, CURITR, ICR, ITYPE, SUP_EMM_45Q(ITYPE,ICR,CURIYR)
            WRITE(18,2317) "INV", CURIRUN, CURCALYR, CURITR, ICR, ITYPE, CST_EMM_INV(ITYPE,ICR,CURIYR)
            WRITE(18,2317) "OM ", CURIRUN, CURCALYR, CURITR, ICR, ITYPE, CST_EMM_OM(ITYPE,ICR,CURIYR)            
2317        FORMAT(1X,"SUMMARY_CCATS_CCS",A5,5(",",I5),",",F21.6)
         END DO
      END DO

!  fill in trade variables
      DO SIRG = 1, UNRGNS
          CALL GETOUT(CURIYR,SIRG)
!     ECONOMY  TRADES
           ETDMDE = ETDMDERG(SIRG)
           UTDMDE(SIRG,CURIYR) =  ETDMDERG(SIRG)
           ETDMME = ETDMMERG(SIRG)
           UTDMME(SIRG,CURIYR) =  ETDMMERG(SIRG)

!     FIRM TRADES
           ETDMDF = - FTDMDF(SIRG) - CTDMDF(SIRG)
           UTDMDF(SIRG,CURIYR) =  - FTDMDF(SIRG) - CTDMDF(SIRG)
           ETDMMF = - ZTDMMF(SIRG)/1000.0
           UTDMMF(SIRG,CURIYR) =  - ZTDMMF(SIRG)/1000.0
           UTEXDF(SIRG,CURIYR) = ZTEXDF(SIRG)

!     FIRM INTERNATIONAL
           ETIMPF = ZTIMPF(SIRG)/1000.0
           UTIMPF(SIRG,CURIYR) = ZTIMPF(SIRG)/1000.0
           UTEXMF(SIRG,CURIYR) = ZTEXMF(SIRG)/1000.0
           ETEXPF = ZTEXPF(SIRG)/1000.0
           UTEXPF(SIRG,CURIYR) = ZTEXPF(SIRG)/1000.0

!     ADD CANADIAN ECONOMY TRADES TO TOTAL

            ETIMPE =  ETDIMERG(SIRG)
            UTIMPE(SIRG,CURIYR)  =  ETDIMERG(SIRG)

            ETIMPD = 0.0
            DO IPROV = 1, EFD_D_PROV
               ETIMPD = ETIMPD + UTSALES(SIRG,MNUMNR+IPROV)
            ENDDO
!
            BEC_IMP(SIRG) = -ETIMPE*.001
!          CALL ROUTINE TO LOAD NUGS REVENUE VARIABLES
           CALL LOADNG(SIRG,CURIYR)
           ETEXPD =  ULEIXR(SIRG) + FTEXPD(SIRG) + CTEXPD(SIRG)
           ETIMPD = ETIMPD + FTIMPD(SIRG) + CTIMPD(SIRG)

!c  sum national totals -
         UTDMMF(MNUMNR,CURIYR) = UTDMMF(MNUMNR,CURIYR) +  &
           UTDMMF(SIRG,CURIYR)
         UTEXMF(MNUMNR,CURIYR) = UTEXMF(MNUMNR,CURIYR) +  &
           UTEXMF(SIRG,CURIYR)
         UTDMME(MNUMNR,CURIYR) = UTDMME(MNUMNR,CURIYR) +  &
           UTDMME(SIRG,CURIYR)
         UTEXME(MNUMNR,CURIYR) = UTEXME(MNUMNR,CURIYR) +  &
           UTEXME(SIRG,CURIYR)
         UTDMDF(MNUMNR,CURIYR) = UTDMDF(MNUMNR,CURIYR) +  &
           UTDMDF(SIRG,CURIYR)
         UTEXDF(MNUMNR,CURIYR) = UTEXDF(MNUMNR,CURIYR) +  &
           UTEXDF(SIRG,CURIYR)
         UTDMDE(MNUMNR,CURIYR) = UTDMDE(MNUMNR,CURIYR) +  &
           UTDMDE(SIRG,CURIYR)
         UTEXDE(MNUMNR,CURIYR) = UTEXDE(MNUMNR,CURIYR) +  &
           UTEXDE(SIRG,CURIYR)
         UTIMPE(MNUMNR,CURIYR) = UTIMPE(MNUMNR,CURIYR) +  &
           UTIMPE(SIRG,CURIYR)
         UTIMPF(MNUMNR,CURIYR) = UTIMPF(MNUMNR,CURIYR) +  &
           UTIMPF(SIRG,CURIYR)
         UTEXPE(MNUMNR,CURIYR) = UTEXPE(MNUMNR,CURIYR) +  &
           UTEXPE(SIRG,CURIYR)
         UTEXPF(MNUMNR,CURIYR) = UTEXPF(MNUMNR,CURIYR) +  &
           UTEXPF(SIRG,CURIYR)

         CALL STROUT(CURIYR,SIRG)

       ENDDO


      RETURN
      END

!
     
!     This subroutine retrieves the operate vectors
!
      SUBROUTINE EDO$OP(IRG)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'elcntl'
      include'ecpcntl'
      include'dispuse'
      include'plntctl'
      include'elout'
      include'dispcrv'
      include'dsmdimen'
      include'dsmtoefd'
      include'ecp_nuc'
      include'emission'
      include'cdsparms'
      include'csapr'
      include'emmemis'
      include'emm_aimms'
      include'wrenew'

      INTEGER ITYP,IRG,N,IP,IPGRP,MRSW,IFL,TFR,IECP,IS,IVSL,GRP,SEG,FSL,ICL
      REAL   SUMFL,TOL,P2_TEST,ADJ
      REAL*8 SOLVAL(5)
      INTEGER*4 IRET
      CHARACTER*1 FSLCD
      CHARACTER*2 STAT
      CHARACTER*16 COL
      CHARACTER*8 COLSOL /'ACLUD   '/

      efdsub='EDO$OP'


      TOL = 0.0001
      
      
     if (.NOT. USE_AIMEFD_SLNADJ) then   !added by AKN to bypass FORTRAN post adjustments      
          EFDCURT(IRG,:,CURIYR) = 0.0
          P2_TEST = 0.0
          DO IS = 1, EENSP
             DO IVSL = 1 , ELNVCT(IS)
                GRP = ELGRP(IVSL,IS)
                SEG = ELSEG(IVSL,IS)
                UTHTS2(SEG,GRP,IRG) = 0.0
                UTHTP2(SEG,GRP,IRG) = 0.0
             ENDDO
          ENDDO
      endif
!   loop over dispatchable plant types
      ITYP = 1

      DO N = 1, ECNTP
       IP = ECASTS(N)
       IECP = ECTECP(N)
       ICL = ECLR(N)
       TFR = EPNFLRG(ECCR(N),ECLR(N),ECGR(N),ECAR(N))
       IPGRP = ECDBID(N)
       MRSW = ULMRUN(IPGRP)
!
!      IF (CURIYR .EQ. 16 .AND. FCRL .EQ. 1) THEN
!         WRITE(18,3751) CURIYR+UHBSYR,CURITR,IRG,IPGRP,N,ITYP,IP,EPPOPR(IP),IECP,MRSW
!3751     FORMAT(1X,"BAD_GRP_DATA_EDO_OP",10(":",I5))
!      END IF
!
       if (.NOT. USE_AIMEFD_SLNADJ) then   !added by AKN to bypass FORTRAN post adjustments       
           IF (EPPOPR(IP) .EQ. 1) THEN
             CALL RTBSLD(N,IRG,ITYP)
           ELSEIF (EPPOPR(IP) .EQ. 2) THEN
             CALL RTPEAK(N,IRG,ITYP)
           ELSEIF (EPPOPR(IP) .EQ. 3) THEN
             CALL RTRNBS(N,IRG,ITYP)
           ELSEIF (EPPOPR(IP) .EQ. 4) THEN
             CALL RTHYD(N,IRG,ITYP)
           ELSEIF (EPPOPR(IP) .EQ. 5) THEN
             CALL RTINT(N,IRG,ITYP)
           ELSE
             write(6,*) 'error in operating type EDO_OP_ECNTP',N,IP,EPPOPR(IP),IRG,IECP,IPGRP
           ENDIF
       endif
!  fill in fuel shares
       IF (IECP .LE. UIPC) THEN    ! coal plant - not IGCC
        DO IS = 1, EENSP
         ECFLTP(N,1) = IECP
         ULFUEL(1,IPGRP) = IECP
         IF (ECFLRG(N,1,1) .EQ. 0) ECFLRG(N,1,1) = ECLR(N)
         IF (ULFLRG(1,IPGRP) .EQ. 0) ULFLRG(1,IPGRP) = ECLR(N)
         ELFLSH(IS,IPGRP,1) = FCLSH(1,IECP,TFR)
         ECFLTP(N,2) = UIDS
         ULFUEL(2,IPGRP) = UIDS
         IF (ECFLRG(N,2,1) .EQ. 0) ECFLRG(N,2,1) = ECCR(N)
         IF (ULFLRG(2,IPGRP) .EQ. 0) ULFLRG(2,IPGRP) = ECCR(N)
         ELFLSH(IS,IPGRP,2) = FOLSH(1,IECP,TFR)
         ECFLTP(N,3) = UIWD
         ULFUEL(3,IPGRP) = UIWD
         IF (ECFLRG(N,3,1) .EQ. 0) ECFLRG(N,3,1) = ECLR(N)
         IF (ULFLRG(3,IPGRP) .EQ. 0) ULFLRG(3,IPGRP) = ECLR(N)
         ELFLSH(IS,IPGRP,3) = FWDSH(1,IECP,TFR)

!        adjust using EMM region cofiring share instead of fuel (coal) region share

!        write(6,3333) curirun, curiyr+1989, CURITR, irg, tfr, ICL, N, uplntcd(iecp), &
!           elflsh(is,ipgrp,1), elflsh(is,ipgrp,2), elflsh(is,ipgrp,3), UPWDCFN(IECP,IRG), UPWDCFR(IECP,ICL)
!3333    format(1h ,'!cfsh',7(":",i4),":",a3,6(":",f10.3))

!        ELFLSH(IS,IPGRP,1) = ELFLSH(IS,IPGRP,1) + FWDSH(1,IECP,TFR) - UPWDCFN(IECP,IRG)
!        ELFLSH(IS,IPGRP,3) = UPWDCFN(IECP,IRG)
        ENDDO

       ELSEIF (IECP .LE. UIIS) THEN  ! coal plant - IGCC
        DO IS = 1, EENSP
         ECFLTP(N,1) = IECP
         ULFUEL(1,IPGRP) = IECP
         IF (ECFLRG(N,1,1) .EQ. 0) ECFLRG(N,1,1) = ECLR(N)
         IF (ULFLRG(1,IPGRP) .EQ. 0) ULFLRG(1,IPGRP) = ECLR(N)
         ELFLSH(IS,IPGRP,1) = FCLSH(1,IECP,TFR)
         IF (FCLSH(1,IECP,TFR) .LT. 1.0) THEN
           write(UF_DBG,1212) CURIYR,CURITR,N,IECP,FCLSH(1,IECP,TFR),FOLSH(1,IECP,TFR),FWDSH(1,IECP,TFR)
           ELFLSH(IS,IPGRP,1) = 1.0
1212    FORMAT(1x,'IGCC FLSH err',4I6,3F10.6)
         ENDIF
         ECFLTP(N,2) = 0
         ULFUEL(2,IPGRP) = 0
         ECFLRG(N,2,1) = 0
         ULFLRG(2,IPGRP) = 0
         ELFLSH(IS,IPGRP,2) = 0.0
         ECFLTP(N,3) = 0
         ULFUEL(3,IPGRP) = 0
         ECFLRG(N,3,1) = 0
         ULFLRG(3,IPGRP) =  0
         ELFLSH(IS,IPGRP,3) = 0.0
        ENDDO

       ELSE
         DO IFL = 1, EIFPLT
         DO IS = 1, EENSP
         IF (ECFLTP(N,IFL) .NE. 0) THEN
          IF (UIGAS(ECFLTP(N,IFL)) .EQ. 1) THEN
            ELFLSH(IS,IPGRP,IFL) = FGSSH(2,IP,TFR)
          ELSEIF (UIDIS(ECFLTP(N,IFL)) .EQ. 1 .OR. UIRES(ECFLTP(N,IFL)) .EQ. 1) THEN
            ELFLSH(IS,IPGRP,IFL) = FOLSH(2,IP,TFR)
          ELSEIF (ECFLTP(N,IFL) .EQ. UIWD) THEN
            ELFLSH(IS,IPGRP,IFL) = FWDSH(2,IP,TFR)
          ELSEIF (ECFLTP(N,IFL) .EQ. UIUF .OR. ECFLTP(N,IFL) .EQ. UIGT .OR. ECFLTP(N,IFL) .EQ. UIGC) THEN
            ELFLSH(IS,IPGRP,IFL) = 1.0
          ENDIF
         ELSE
           ELFLSH(IS,IPGRP,IFL) = 0.0
         ENDIF
         ENDDO
         ENDDO
        ENDIF
! check fuel shares sum to 1.0
        SUMFL = ELFLSH(1,IPGRP,1) + ELFLSH(1,IPGRP,2) + ELFLSH(1,IPGRP,3)
        IF (SUMFL .LT. (1.0 - TOL) .OR. SUMFL .GT. (1.0 + TOL))  THEN
         write(UF_DBG,1026) CURIYR,CURITR,N,IPGRP,IECP,IP,TFR,((ECFLTP(N,IFL),ELFLSH(1,IPGRP,IFL),ECFLRG(N,IFL,1)),IFL=1,EIFPLT)
1026    format(1x,'ECFSHR error',7I6,3(I4,F10.5,I4))
        do IS = 1, EENSP
         ELFLSH(IS,IPGRP,1) = 1.0
         ELFLSH(IS,IPGRP,2) = 0.0
         ELFLSH(IS,IPGRP,3) = 0.0
        enddo
        ENDIF
         IF (FCRL .eq. 1) THEN
        write(UF_DBG,1028) CURIYR,CURITR,IPGRP,IECP,IP,TFR,((ECFLTP(N,IFL),ELFLSH(1,IPGRP,IFL),ECFLRG(N,IFL,1)),IFL=1,EIFPLT)
1028     format(1x,'ECFSHR final',6I6,3(I4,F10.5,I4))
        ENDIF

      ENDDO

!   loop over renewable plant types
      if (.NOT. USE_AIMEFD_SLNADJ) then   !added by AKN to bypass FORTRAN post adjustments      
          ITYP = 2
          DO N = 1, EHNTP
           IP = EHHYTP(N)
           IECP = EHTECP(N)
           IPGRP = EHDBID(N)
           IF (IECP .EQ. WIP2) P2_TEST = P2_TEST + EHCAP(N,1)
    !
    !      IF (IPGRP .EQ. 2684 .OR. IPGRP .EQ. 3587) THEN
    !         WRITE(18,3751) CURIYR+UHBSYR,CURITR,IRG,IPGRP,N,ITYP,IP,EPPOPR(IP)
    !      END IF
    !
           IF (EPPOPR(IP) .EQ. 3) THEN
             CALL RTRNBS(N,IRG,ITYP)
           ELSEIF (EPPOPR(IP) .EQ. 4) THEN
             CALL RTHYD(N,IRG,ITYP)
           ELSEIF (EPPOPR(IP) .EQ. 5) THEN
             CALL RTINT(N,IRG,ITYP)
           ELSEIF (EPPOPR(IP) .EQ. 7) THEN
             CALL RTSTR(N,IRG,ITYP)
           ELSE
             write(6,*) 'error in operating type EDO_OP_EHNTP',N,IP,EPPOPR(IP),IRG,IECP,IPGRP
           ENDIF
          ENDDO
      endif
!   loop over dist gen plant types
      if (.NOT. USE_AIMEFD_SLNADJ) then   !added by AKN to bypass FORTRAN post adjustments      
          ITYP = 3

          DO N = 1, EDNTP
           CALL RTDG(N,IRG,ITYP)
          ENDDO
      endif
! retrieve P2 storage decisions
      if (.NOT. USE_AIMEFD_SLNADJ) then   !added by AKN to bypass FORTRAN post adjustments      
          IF (P2_TEST .GT. 0.0) THEN
              DO IS = 1, EENSP
                 DO IVSL = 1 , ELNVCT(IS)
                    GRP = ELGRP(IVSL,IS)
                    SEG = ELSEG(IVSL,IS)
                    FSL = EFD_Slice_ID(GRP,SEG)
                    FSLCD = CHAR(64+FSL)
                    COL = 'S2'// URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(COL_mask,':S2:', URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)
                    CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
                    ADJ = SOLVAL(1) / (ELWDTH(IVSL,IS) * 0.1)
                    UTHTS2(SEG,GRP,IRG) = UTHTS2(SEG,GRP,IRG) + ADJ
                 END DO
              END DO
          END IF
      endif
! print spinning reserve by technology
      IF (FCRL .EQ. 1) THEN
      DO IECP = 1, ECP_D_CAP
        WRITE(UF_DBG,1030) CURIYR,CURITR,IRG,IECP,((SP_ACHBYECP(GRP,SEG,IRG,IECP),SEG=1,3),GRP=1,3)
        IF (UCPINTIS(IECP) .GT. 0 ) THEN
          IF (EFDCURT(IRG,UCPINTIS(IECP),CURIYR) .GT. 0.0) THEN
           CURTAIL(UCPINTIS(IECP),IRG,CURIYR) = CURTAIL(UCPINTIS(IECP),IRG,CURIYR) + EFDCURT(IRG,UCPINTIS(IECP),CURIYR)            !add EFD curt to RESTORE value
           WRITE(UF_DBG,1031) CURIYR,CURITR,IRG,IECP,EFDCURT(IRG,UCPINTIS(IECP),CURIYR)
          ENDIF
          CURTAIL(UCPINTIS(IECP),MNUMNR,CURIYR) = CURTAIL(UCPINTIS(IECP),MNUMNR,CURIYR) + CURTAIL(UCPINTIS(IECP),IRG,CURIYR)      !fill national value
        ENDIF
      ENDDO
      ENDIF
1030  FORMAT(1X,'SR_ACHBYECP:',4I6,9F12.4)
1031  FORMAT(1X,'EFDCURT:',4(I6,":"),F12.4)
      RETURN
      END

!
!     This subroutine retrieves operates for baseload plants
!

      SUBROUTINE RTBSLD(N,IRG,ITYP)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'plntctl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'elout'
      include'dispuse'
      include'dispcrv'
      include'dispinyr'
      include'ecp_nuc'
      include'cdsparms'
      include'emm_aimms'

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,JGRP,I_EFD_GRP
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,IRETRT,IRET,IRET_MIN,IRET_MAX,IRETRT_ALT
      INTEGER*4  FUEL_RGN, ECPt, INUC, IFOSBS
      INTEGER*4 RUN45Q
      REAL CF(EFD_D_MVS+1,EFD_D_MSP),VOM,HTRT,CAP(EFD_D_MSP),HRFAC,MAXCF(EFD_D_MSP)
      REAL*8 DRAT,VAL,GEN,OBJVAL,BTUREQ,SOLVAL(5),SHR_C,SHR_G,GEN_ALT,GENSR_ALT,GEN_MIN_ALT,GEN_MINSR_ALT,GEN_MAX_ALT,GEN_MAXSR_ALT
      REAL*8 DRAT_ORG,DRAT_MIN,DRAT_MAX,SOLVAL_MIN(5),SOLVAL_MAX(5),GEN_MIN,GEN_MAX,MAX_SP_LOAD,MIN_SP_LOAD,GENSR,GEN_MINSR,GEN_MAXSR
      REAL*8 SOLVAL_ALT(5), SOLVAL_MIN_ALT(5), SOLVAL_MAX_ALT(5)
      REAL*8 S_GEN, S_GEN_MIN, S_GEN_MAX, F_GEN, F_GEN_MIN, F_GEN_MAX, SHOURS, LL, LL_MIN, LL_MAX
      REAL*8 S_GEN_ALT, S_GEN_MIN_ALT, S_GEN_MAX_ALT, F_GEN_ALT, F_GEN_MIN_ALT, F_GEN_MAX_ALT
      REAL*8 Load_Level, HTRT_ADJ, HTRT_ADJ_MIN, HTRT_ADJ_MAX, Target_EFF, Max_EFF, ADJ_FAC
      CHARACTER*1 FSLCD,CHMOD, ALT_SP(EFD_D_MSP)
      CHARACTER*5 NUM
      CHARACTER*16 MASKOP,COLNAM,COLNAM_MIN,COLNAM_MAX
      CHARACTER*16 MASKOP_ALT,COLNAM_ALT,COLNAM_MIN_ALT,COLNAM_MAX_ALT
      CHARACTER*12 FROM_LABEL
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*2 STAT,STAT_MIN,STAT_MAX
      LOGICAL UseEFDADJST

      efdsub='RTBSLD'

      RUN45Q=RTOVALUE('RUN45Q  ',0)

      ALT_SP(1) = "X"
      ALT_SP(2) = "Y"
      ALT_SP(3) = "Z"
      ALT_SP(4) = "W"

      INUC=0
      IFOSBS=0
      
      IF (ITYP .EQ. 1) THEN
         IGRP = ECDBID(N)
         I_EFD_GRP = MAP_ECNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 2) THEN
         IGRP = EHDBID(N)
         I_EFD_GRP = MAP_EHNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 3) THEN
         IGRP = EDDBID(N)
         I_EFD_GRP = MAP_EDNTP_EFD_GRPS(N,IRG)
      ENDIF
      IECP = UG_ECPt(I_EFD_GRP)
      JGRP = EFD_GRPS_F(I_EFD_GRP)
      SHR_G = UP_GEN(0,IGRP) / UG_GEN(0,I_EFD_GRP)

      IF ((CURIYR + UHBSYR) .GE. UPSTYR) THEN   ! if ECP/RESTORE has run, set flags to use RESTORE CF
       IF (StorageCodes(UCPVSTOR(IECP)) .EQ. 'NC') THEN
            INUC = 1
       ELSEIF (IECP .LE. ECP_D_DSP) THEN
          IF (UPPCFB(IECP,1) .GT. 0.001 .AND. UPAVLYR(IECP) .LT. 9000) THEN
            IFOSBS = 1
          ENDIF
       ENDIF
      ENDIF
      
      ADJ_FAC = 1.0
      IF (UPPCEF(IECP) .GT. UPPCEF_MIN(IECP) .AND. RUN45Q .GT. 0) THEN 

              IF (UGNOCCS(I_EFD_GRP) .NE. 1) ADJ_FAC =  ALT_UECP_CPEN_ADJ(IECP) / (1.0 - UGNOCCS(I_EFD_GRP))
           
          WRITE(18,6317) CURIRUN, CURIYR+1989, I_EFD_GRP, N, IGRP, IRG, IECP, ITYP, JGRP, UPPCEF(IECP),UPPCEF_MIN(IECP),ALT_UECP_CPEN_ADJ(IECP) , UGNOCCS(I_EFD_GRP),UG_CAP(0,I_EFD_GRP), ADJ_FAC
  6317    FORMAT(1X,"ADJ_FAC_RT_INFO",9(",",I6),6(",",F21.6))

      ENDIF

      WRITE(NUM,'(I5.5)') JGRP

      DO IS = 1, EENSP
         IF (ITYP .EQ. 1) THEN
            CAP(IS) = ECCAP(N,IS)
            MAXCF(IS) = ECMXCP(N)*EFACTR
         ELSEIF (ITYP .EQ. 2) THEN
            CAP(IS) = EHCAP(N,IS)
            MAXCF(IS) = EHHYCF(N,IS)*EFACTR
         ELSEIF (ITYP .EQ. 3) THEN
            CAP(IS) = EDCAP(N,IS)
            MAXCF(IS) = EDMXCP(N)*EFACTR
         ENDIF
      ENDDO

!     loop over time season and slices to determine modes

      DO IS = 1, EENSP

         IF (UG_CAP(IS,I_EFD_GRP) .GT. 0.0) THEN
            SHR_C = UP_CAP(IS,IGRP) / UG_CAP(IS,I_EFD_GRP)
         ELSE
            SHR_C = UP_CAP(0,IGRP) / UG_CAP(0,I_EFD_GRP)
         END IF

         MAX_SP_LOAD = 0.0
         MIN_SP_LOAD = 500000.0
         SHOURS = 0.0
         DO IVSL = 1 , ELNVCT(IS)
            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)
            MAX_SP_LOAD = MAX(MAX_SP_LOAD , UTHGHT(SEG,GRP,IRG))
            MIN_SP_LOAD = MIN(MIN_SP_LOAD , UTHGHT(SEG,GRP,IRG))
            SHOURS = SHOURS + DBLE(ELWDTH(IVSL,IS))
         END DO

         MASKOP = 'O'//NUM//CHCOD(IS)//'*'; call makmsk(MASKOP_mask,':O:',NUM,CHCOD(IS),'*')
         !if(aimmsefd.eq.0 .or. aimefdbg.eq.1) then
         !  IRETRT = WFCMASK(MASKOP,COLNAM) ; COLNAM_mask=MASKOP_mask
         !endif
         call get_masked_col(maskop,colnam,iretrt)
         MASKOP = '        '
         COLNAM_MIN = '        '
         COLNAM_MAX = '        '
         COLNAM_ALT = '        '
         COLNAM_MIN_ALT = '        '
         COLNAM_MAX_ALT = '        '
         DO WHILE (IRETRT .EQ. 0)
            CHMOD = COLNAM(8:8)
            MODE = ICHAR(CHMOD) - 48
            COLNAM_MIN = 'J'//NUM//CHCOD(IS)//CHMOD; call makmsk(COLNAM_MIN_mask,':J:',NUM,CHCOD(IS),CHMOD)
            COLNAM_MAX = 'U'//NUM//CHCOD(IS)//CHMOD; call makmsk(COLNAM_MAX_mask,':U:',NUM,CHCOD(IS),CHMOD)

            IF (UPPCEF(IECP) .GT. 0.0) THEN
               COLNAM_ALT = 'O'//NUM//ALT_SP(IS)//CHMOD; call makmsk(COLNAM_ALT_mask,':O:',NUM,ALT_SP(IS),CHMOD)
               COLNAM_MIN_ALT = 'J'//NUM//ALT_SP(IS)//CHMOD; call makmsk(COLNAM_MIN_ALT_mask,':J:',NUM,ALT_SP(IS),CHMOD)
               COLNAM_MAX_ALT = 'U'//NUM//ALT_SP(IS)//CHMOD; call makmsk(COLNAM_MAX_ALT_mask,':U:',NUM,ALT_SP(IS),CHMOD)
            END IF

            IF (MODE .GT. 9) write(6,1026) 'error in retrieving mode ',CHMOD,MODE,MASKOP
1026        format(1x,A25,A4,I4,A10)

            SOLVAL = 0.0
            SOLVAL_MIN = 0.0
            SOLVAL_MAX = 0.0

            SOLVAL_ALT = 0.0
            SOLVAL_MIN_ALT = 0.0
            SOLVAL_MAX_ALT = 0.0

            CALL DWFSCOL(COLNAM,COLSOL,STAT,SOLVAL,COLNAM_mask,IRET)
            IF (ADJ_FAC .NE. 1.0) CALL DWFSCOL(COLNAM_ALT,COLSOL,STAT,SOLVAL_ALT,COLNAM_ALT_mask,IRET)
            IF (SR_CREDIT(IECP) .GT. 0.0) THEN
               CALL DWFSCOL(COLNAM_MIN,COLSOL,STAT_MIN,SOLVAL_MIN,COLNAM_MIN_mask,IRET_MIN)
               IF (ADJ_FAC .NE. 1.0) CALL DWFSCOL(COLNAM_MIN_ALT,COLSOL,STAT,SOLVAL_MIN_ALT,COLNAM_MIN_ALT_mask,IRET_MIN)
               CALL DWFSCOL(COLNAM_MAX,COLSOL,STAT_MAX,SOLVAL_MAX,COLNAM_MAX_mask,IRET_MAX)
               IF (ADJ_FAC .NE. 1.0) CALL DWFSCOL(COLNAM_MAX_ALT,COLSOL,STAT_MAX,SOLVAL_MAX_ALT,COLNAM_MAX_ALT_mask,IRET_MAX)
            END IF

            IF (SOLVAL(1) + SOLVAL_MIN(1) + SOLVAL_MAX(1) + SOLVAL_ALT(1) + SOLVAL_MIN_ALT(1) + SOLVAL_MAX_ALT(1) .GT. 0.0) THEN

               S_GEN = 0.0
               S_GEN_ALT = 0.0
               F_GEN = 0.0
               F_GEN_ALT = 0.0
               LL = 1.0
               HTRT_ADJ = 1.0
               S_GEN_MIN = 0.0
               S_GEN_MIN_ALT = 0.0
               F_GEN_MIN = 0.0
               F_GEN_MIN_ALT = 0.0
               LL_MIN = 1.0
               HTRT_ADJ_MIN = 1.0
               S_GEN_MAX = 0.0
               S_GEN_MAX_ALT = 0.0
               F_GEN_MAX = 0.0
               F_GEN_MAX_ALT = 0.0
               LL_MAX = 1.0
               HTRT_ADJ_MAX = 1.0

               DO IVSL = 1, MODE
                  GRP = ELGRP(IVSL,IS)
                  SEG = ELSEG(IVSL,IS)
                  GEN = 0.0; GENSR = 0.0
                  GEN_MIN = 0.0; GEN_MINSR = 0.0
                  GEN_MAX = 0.0; GEN_MAXSR = 0.0
                  GEN_ALT = 0.0; GENSR_ALT = 0.0
                  GEN_MIN_ALT = 0.0; GEN_MINSR_ALT = 0.0
                  GEN_MAX_ALT = 0.0; GEN_MAXSR_ALT = 0.0
                  IF (ITYP .EQ. 1) THEN
                     CALL ELDRAT(IS,IRG,I_EFD_GRP,IVSL,DRAT_ORG)

!                    For ECP Types which have Spinning Reserve Credit Create Min, Max and Load Following Modes for each of the original modes

                     IF (SR_CREDIT(IECP) .GT. 0.0) THEN

!                       Load Following Mode Follows Load Height drat = (1 - for) * (ld / max_ld)
!                       Use average ReSTORE CF for nuclear and baseload fossil for load following mode to allow for following net load
                        IF (INUC .EQ. 1) THEN
                           DRAT = MAX((1.0 - UG_FOR(I_EFD_GRP)) * (1.0 - UG_LFR(I_EFD_GRP)) * NUC_CF_EFD(SEG,GRP,IRG),SR_MIN_CF(IECP))
                        ELSEIF (IFOSBS .EQ. 1) THEN
                           DRAT = MAX((1.0 - UG_FOR(I_EFD_GRP)) * FOS_CF_EFD(SEG,GRP,IRG),SR_MIN_CF(IECP))
                        ELSE

                          DRAT = (1.0 - UG_FOR(I_EFD_GRP)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD)
                        ENDIF

!                       Min Electricity - Max Spinning Reserve Credit drat_min = (1 - sr_min_lf) * sr_min_cf * (ld / min_ld) + sr_min_lf * sr_min_cf

                        DRAT_MIN = (1.0 - SR_MIN_LF(IECP)) * SR_MIN_CF(IECP) * (UTHGHT(SEG,GRP,IRG) / MIN_SP_LOAD) + SR_MIN_LF(IECP) * SR_MIN_CF(IECP)

!                       Max Electricity - Min Spinning Reserve Credit drat_max = (1 - sr_max_lf) * (1 - for) * (ld / max_ld) + sr_max_lf * (1 - for)

                        DRAT_MAX = (1.0 - SR_MAX_LF(IECP)) * (1.0 - UG_FOR(I_EFD_GRP)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD) + &
                           SR_MAX_LF(IECP) * (1.0 - UG_FOR(I_EFD_GRP))
                   
                        IF (UG_MRUN(I_EFD_GRP) .GT. 0 .AND. DRAT_MAX .LT. DRAT_ORG) THEN
                           DRAT_MAX = MAX(DRAT_MAX , DRAT_ORG)
                        END IF

                        IF (IECP .EQ. WIAN .OR. IECP .EQ. WICN .OR. IECP .EQ. WISM) THEN
                           DRAT_MAX = DRAT_ORG
                        END IF

                        IF (IECP .LE. WIIS .AND. CURIYR + UHBSYR .LE. UYR_STEO + 1) THEN
                           DRAT_MAX = DRAT_ORG
                        END IF
                     ELSE
                        DRAT = DRAT_ORG
                     END IF
                  ELSE
                     DRAT = MAXCF(IS)
                  ENDIF

!                 STEOBM  adjust drat (capacity factor) for STEO benchmarking nuclear (factor will be 1.0 if not benchmarking)

                  IF (IECP .EQ. WICN .OR. IECP .EQ. WIAN.OR. IECP .EQ. WISM) THEN
                     DRAT = DRAT * URNCCFA(CURIYR)
                     DRAT_MAX = DRAT_MAX * URNCCFA(CURIYR) 
                     DRAT_MIN = DRAT_MIN * URNCCFA(CURIYR) 
                  ENDIF

                  IF (DRAT .LT. 0.01) DRAT = 0.01
                  GEN = DRAT * ELWDTH(IVSL,IS) * SOLVAL(1) * SHR_C
                  S_GEN = S_GEN + GEN
                  F_GEN = F_GEN + DRAT * ELWDTH(IVSL,IS)
                  GENSR = (1 - DRAT) * SR_CREDIT(IECP) * SOLVAL(1) * SHR_C
                  IF (ADJ_FAC .NE. 1.0) THEN
                     GEN_ALT = DRAT * ELWDTH(IVSL,IS) * SOLVAL_ALT(1) * SHR_C * ADJ_FAC
                     S_GEN_ALT = S_GEN_ALT + GEN_ALT
                     F_GEN_ALT = F_GEN_ALT + DRAT * ELWDTH(IVSL,IS) * ADJ_FAC
                     GENSR_ALT = (1 - DRAT) * SR_CREDIT(IECP) * SOLVAL_ALT(1) * SHR_C * ADJ_FAC
                  END IF
                  ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN + GEN_ALT
                  ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN + GEN_ALT
                  ECDSPE_ALT(IGRP,IS) = ECDSPE_ALT(IGRP,IS) + GEN_ALT
                  ULTGEN(IGRP) = ULTGEN(IGRP) + GEN + GEN_ALT
                  SP_ACHBYECP(GRP,SEG,IRG,IECP) = SP_ACHBYECP(GRP,SEG,IRG,IECP) + GENSR + GENSR_ALT

                  IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                     IF (DRAT_MIN .LT. 0.01) DRAT_MIN = 0.01
                     GEN_MIN = DRAT_MIN * ELWDTH(IVSL,IS) * SOLVAL_MIN(1) * SHR_C
                     S_GEN_MIN = S_GEN_MIN + GEN_MIN
                     F_GEN_MIN = F_GEN_MIN + DRAT_MIN * ELWDTH(IVSL,IS) * ADJ_FAC
                     GEN_MINSR = (1 - DRAT_MIN) * SR_CREDIT(IECP) * SOLVAL_MIN(1) * SHR_C
                     IF (ADJ_FAC .NE. 1.0)  THEN
                        GEN_MIN_ALT = DRAT_MIN * ELWDTH(IVSL,IS) * SOLVAL_MIN_ALT(1) * SHR_C * ADJ_FAC
                        S_GEN_MIN_ALT = S_GEN_MIN_ALT + GEN_MIN_ALT
                        F_GEN_MIN_ALT = F_GEN_MIN_ALT + DRAT_MIN * ELWDTH(IVSL,IS) * ADJ_FAC
                        GEN_MINSR_ALT = (1 - DRAT_MIN) * SR_CREDIT(IECP) * SOLVAL_MIN_ALT(1) * SHR_C * ADJ_FAC
                     END IF
                     ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN_MIN + GEN_MIN_ALT
                     ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN_MIN + GEN_MIN_ALT
                     ECDSPE_ALT(IGRP,IS) = ECDSPE_ALT(IGRP,IS) + GEN_MIN_ALT
                     ULTGEN(IGRP) = ULTGEN(IGRP) + GEN_MIN + GEN_MIN_ALT
                     SP_ACHBYECP(GRP,SEG,IRG,IECP) = SP_ACHBYECP(GRP,SEG,IRG,IECP) + GEN_MINSR + GEN_MINSR_ALT

                     IF (DRAT_MAX .LT. 0.01) DRAT_MAX = 0.01
                     GEN_MAX = DRAT_MAX * ELWDTH(IVSL,IS) * SOLVAL_MAX(1) * SHR_C
                     S_GEN_MAX = S_GEN_MAX + GEN_MAX
                     F_GEN_MAX = F_GEN_MAX + DRAT_MAX * ELWDTH(IVSL,IS)
                     GEN_MAXSR = (1 - DRAT_MAX) * SR_CREDIT(IECP) * SOLVAL_MAX(1) * SHR_C
                     IF (ADJ_FAC .NE. 1.0)  THEN
                        GEN_MAX_ALT = DRAT_MAX * ELWDTH(IVSL,IS) * SOLVAL_MAX_ALT(1) * SHR_C * ADJ_FAC
                        S_GEN_MAX_ALT = S_GEN_MAX_ALT + GEN_MAX_ALT
                        F_GEN_MAX_ALT = F_GEN_MAX_ALT + DRAT_MAX * ELWDTH(IVSL,IS) * ADJ_FAC
                        GEN_MAXSR_ALT = (1 - DRAT_MAX) * SR_CREDIT(IECP) * SOLVAL_MAX_ALT(1) * SHR_C * ADJ_FAC
                     END IF
                     ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN_MAX + GEN_MAX_ALT
                     ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN_MAX + GEN_MAX_ALT
                     ECDSPE_ALT(IGRP,IS) = ECDSPE_ALT(IGRP,IS) + GEN_MAX_ALT
                     ULTGEN(IGRP) = ULTGEN(IGRP) + GEN_MAX + GEN_MAX_ALT
                     SP_ACHBYECP(GRP,SEG,IRG,IECP) = SP_ACHBYECP(GRP,SEG,IRG,IECP) + GEN_MAXSR + GEN_MAXSR_ALT
                  ENDIF
                  
!				  IF (CURIYR+1989 .EQ. 2020 .AND. FCRL .EQ. 1) THEN
!				     WRITE(6,3751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IVSL, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, COLNAM, COLNAM_MIN, COLNAM_MAX, &
!					    SOLVAL(1)*SHR_C, SOLVAL_MIN(1)*SHR_C, SOLVAL_MAX(1)*SHR_C, &
!						GEN, GEN_MIN, GEN_MAX, &
!						DRAT, DRAT_MIN, DRAT_MAX, DRAT_ORG, SR_CREDIT(IECP), &
!						ULTGEN(IGRP), ECDSPE(IGRP,IS), CAP(IS), MAXCF(IS), SHR_C, ELWDTH(IVSL,IS), UTHGHT(SEG,GRP,IRG), MIN_SP_LOAD, MAX_SP_LOAD
!3751                FORMAT(1X,"GRP_DATA_RTBSLD",11(":",I5),3(":",A8),20(":",F20.6))
!				  END IF

                  IF (UPPCEF(IECP) .GT. 0.0 .AND. FCRL .EQ. 1) THEN
                     WRITE(18,5719) CURIRUN, CURIYR+1989, IVSL, ELWDTH(IVSL,IS), ADJ_FAC, SHR_C, &
                        COLNAM,         SOLVAL(1),         DRAT,     GEN, &
                        COLNAM_ALT,     SOLVAL_ALT(1),     DRAT,     GEN_ALT, &
                        COLNAM_MIN,     SOLVAL_MIN(1),     DRAT_MIN, GEN_MIN, &
                        COLNAM_MIN_ALT, SOLVAL_MIN_ALT(1), DRAT_MIN, GEN_MIN_ALT, &
                        COLNAM_MAX,     SOLVAL_MAX(1),     DRAT_MAX, GEN_MAX, &
                        COLNAM_MAX_ALT, SOLVAL_MAX_ALT(1), DRAT_MAX, GEN_MAX_ALT
 5719                FORMAT(1X,"ALT_DISPATCH",3(",",I5),3(",",F12.3),6(",",A8,",",F21.6,",",F21.6,",",F21.6))
                  END IF

               ENDDO

               LL = (F_GEN + F_GEN_ALT / ADJ_FAC) / SHOURS
               Load_Level = MIN( LL , 1.0)
               ECPt = IECP
               FUEL_RGN = UG_FL_RG(I_EFD_GRP)
               FROM_LABEL = "OUT_" // COLNAM

               CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)

               ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + (S_GEN + S_GEN_ALT / ADJ_FAC) * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ
               ECDSPF_ALT(IGRP,IS) = ECDSPF_ALT(IGRP,IS) + (S_GEN_ALT / ADJ_FAC) * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ
               IF (SR_CREDIT(IECP) .GT. 0.0) THEN
                  LL_MIN = (F_GEN_MIN + F_GEN_MIN_ALT / ADJ_FAC)/ SHOURS

                  Load_Level = MIN( LL_MIN , 1.0)
                  ECPt = IECP
                  FUEL_RGN = UG_FL_RG(I_EFD_GRP)
                  FROM_LABEL = "OUT_" // COLNAM_MIN

                  CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ_MIN, Target_EFF, Max_EFF)

                  ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + (S_GEN_MIN + S_GEN_MIN_ALT / ADJ_FAC) * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ_MIN
                  ECDSPF_ALT(IGRP,IS) = ECDSPF_ALT(IGRP,IS) + (S_GEN_MIN_ALT / ADJ_FAC) * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ_MIN

                  LL_MAX = (F_GEN_MAX + F_GEN_MAX_ALT / ADJ_FAC) / SHOURS

                  Load_Level = MIN( LL_MAX , 1.0)
                  ECPt = IECP
                  FUEL_RGN = UG_FL_RG(I_EFD_GRP)
                  FROM_LABEL = "OUT_" // COLNAM_MAX

                  CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ_MAX, Target_EFF, Max_EFF)

                  ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + (S_GEN_MAX + S_GEN_MAX_ALT / ADJ_FAC) * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ_MAX
                  ECDSPF_ALT(IGRP,IS) = ECDSPF_ALT(IGRP,IS) + (S_GEN_MAX_ALT / ADJ_FAC) * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ_MAX
               END IF

               IF (FCRL .EQ. 1 .AND. ADJ_FAC .NE. 1.0) THEN
                  WRITE(18,5751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, &
                     SR_CREDIT(IECP), 1.0-UG_FOR(I_EFD_GRP), SHOURS, SHR_C, ECDSPE(IGRP,IS), ECDSPF(IGRP,IS), ECDSPE_ALT(IGRP,IS), ECDSPF_ALT(IGRP,IS),  &
                     UP_CAP(IS,IGRP), UG_CAP(IS,I_EFD_GRP), ULHTRT_EFD(IGRP,IS), ADJ_FAC, &
                     COLNAM, S_GEN, S_GEN_ALT, LL, HTRT_ADJ, COLNAM_MIN, S_GEN_MIN, S_GEN_MIN_ALT, LL_MIN, HTRT_ADJ_MIN, COLNAM_MAX, S_GEN_MAX, S_GEN_MAX_ALT, LL_MAX, HTRT_ADJ_MAX, DRAT, DRAT_MIN, DRAT_MAX
 5751             FORMAT(1X,"ECDSPF_DATA_RTBSLD",10(",",I5),12(",",F21.6),3(",",A16,",",F21.6,",",F21.6,",",F21.6,",",F21.6),3(",",F21.6))
               END IF

            ENDIF
            !if(aimmsefd.eq.0 .or. aimefdbg.eq.1) then
            !  IRETRT = WFCMASK(MASKOP,COLNAM) ; COLNAM_mask=MASKOP_mask
            !endif
            call get_masked_col(maskop,colnam,iretrt)

            IF (ADJ_FAC .NE. 1.0) THEN
               !if(aimmsefd.eq.0 .or. aimefdbg.eq.1) then
               !  IRETRT_ALT = WFCMASK(MASKOP_ALT,COLNAM_ALT) ; COLNAM_ALT_mask=MASKOP_ALT_mask
               !endif
               call get_masked_col(maskop,colnam_alt,iretrt_alt)
            END IF
         ENDDO   !WHILE
      ENDDO    !SEASON

      RETURN
      END

!
!     This subroutine retrieves operates for peaking plants
!

      SUBROUTINE RTPEAK(N,IRG,ITYP)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'plntctl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'elcntl'
      include'elout'
      include'dispuse'
      include'dispcrv'
      include'ecp_nuc'
      include'cdsparms'
      include'emm_aimms'

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,IRET,JGRP,I_EFD_GRP,IRET_MIN
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,FSL
      INTEGER*4  FUEL_RGN, ECPt
      REAL CF(EFD_D_MVS,EFD_D_MSP),VOM,HTRT,CAP(EFD_D_MSP),HRVAL,MAXCF(EFD_D_MSP)
      REAL*8 DRAT,GEN,OBJVAL,BTUREQ,MRCAP,SOLVAL(5),SHR_C,SHR_G,DRAT_ORG,DRAT_MIN,GEN_MIN,BTUREQ_MIN,SOLVAL_MIN(5),GENSR,GEN_MINSR
      REAL*8 S_GEN, S_GEN_MIN, S_GEN_MAX, F_GEN, F_GEN_MIN, F_GEN_MAX, SHOURS, LL, LL_MIN, LL_MAX
      REAL*8 Load_Level, HTRT_ADJ, HTRT_ADJ_MIN, HTRT_ADJ_MAX, Target_EFF, Max_EFF
      CHARACTER*1 FSLCD
      CHARACTER*5 NUM
      CHARACTER*16 COL,COL_MIN
      CHARACTER*12 FROM_LABEL
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*2 STAT,STAT_MIN

      efdsub='RTPEAK'


      IF (ITYP .EQ. 1) THEN
         IGRP = ECDBID(N)
         I_EFD_GRP = MAP_ECNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 2) THEN
         IGRP = EHDBID(N)
         I_EFD_GRP = MAP_EHNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 3) THEN
         IGRP = EDDBID(N)
         I_EFD_GRP = MAP_EDNTP_EFD_GRPS(N,IRG)
      ENDIF
      JGRP = EFD_GRPS_F(I_EFD_GRP)
      SHR_G = UP_GEN(0,IGRP) / UG_GEN(0,I_EFD_GRP)

      IECP = UG_ECPt(I_EFD_GRP)

      WRITE(NUM,'(I5.5)') JGRP

      DO IS = 1, EENSP
         IF (ITYP .EQ. 1) THEN
            CAP(IS) = ECCAP(N,IS)
            MAXCF(IS) = ECMXCP(N)*EFACTR
         ELSEIF (ITYP .EQ. 2) THEN
            CAP(IS) = EHCAP(N,IS)
            MAXCF(IS) = EHHYCF(N,IS)*EFACTR
         ELSEIF (ITYP .EQ. 3) THEN
            CAP(IS) = EDCAP(N,IS)
            MAXCF(IS) = EDMXCP(N)*EFACTR
         ENDIF
      ENDDO

!     loop over time season and slices

      DO IS = 1, EENSP

         IF (UG_CAP(IS,I_EFD_GRP) .GT. 0.0) THEN
            SHR_C = UP_CAP(IS,IGRP) / UG_CAP(IS,I_EFD_GRP)
         ELSE
            SHR_C = UP_CAP(0,IGRP) / UG_CAP(0,I_EFD_GRP)
         END IF

         SHOURS = 0.0
         S_GEN = 0.0
         F_GEN = 0.0
         LL = 1.0
         HTRT_ADJ = 1.0
         S_GEN_MIN = 0.0
         F_GEN_MIN = 0.0
         LL_MIN = 1.0
         HTRT_ADJ_MIN = 1.0
         S_GEN_MAX = 0.0
         F_GEN_MAX = 0.0
         LL_MAX = 1.0
         HTRT_ADJ_MAX = 1.0

         DO IVSL = 1 , ELNVCT(IS)
            SHOURS = SHOURS + DBLE(ELWDTH(IVSL,IS))
            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)
            FSL = EFD_Slice_ID(GRP,SEG)
            FSLCD = CHAR(64+FSL)
            COL = 'P'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_mask,':P:',NUM,CHCOD(IS),FSLCD)
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
            IF (SR_CREDIT(IECP) .GT. 0) THEN
               COL_MIN = 'Q'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_MIN_mask,':Q:',NUM,CHCOD(IS),FSLCD)
               CALL DWFSCOL(COL_MIN,COLSOL,STAT_MIN,SOLVAL_MIN,COL_MIN_mask,IRET_MIN)
               IF (ITYP .EQ. 1) THEN
                  CALL ELDRAT(IS,IRG,I_EFD_GRP,IVSL,DRAT_ORG)
               ELSE
                  DRAT_ORG = MAXCF(IS)
               ENDIF
               DRAT = 1.0 - UG_FOR(I_EFD_GRP)
               GEN = DRAT * ELWDTH(IVSL,IS) * SOLVAL(1) * SHR_C
               S_GEN = S_GEN + GEN
               F_GEN = F_GEN + DRAT * ELWDTH(IVSL,IS)
               GENSR = (1.0 - DRAT) * SR_CREDIT(IECP) * SOLVAL(1) * SHR_C
               DRAT_MIN = SR_MIN_CF(IECP)
               GEN_MIN = DRAT_MIN * ELWDTH(IVSL,IS) * SOLVAL_MIN(1) * SHR_C
               S_GEN_MIN = S_GEN_MIN + GEN_MIN
               F_GEN_MIN = F_GEN_MIN + DRAT_MIN * ELWDTH(IVSL,IS)
               GEN_MINSR = (1.0 - DRAT_MIN) * SR_CREDIT(IECP) * SOLVAL_MIN(1) * SHR_C
               ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN + GEN_MIN
               ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN + GEN_MIN
               ULTGEN(IGRP) = ULTGEN(IGRP) + GEN + GEN_MIN
               SP_ACHBYECP(GRP,SEG,IRG,IECP) = SP_ACHBYECP(GRP,SEG,IRG,IECP) + GENSR + GEN_MINSR
            ELSE
               IF (ITYP .EQ. 1) THEN
                  CALL ELDRAT(IS,IRG,I_EFD_GRP,IVSL,DRAT)
               ELSE
                  DRAT = MAXCF(IS)
               ENDIF
               IF (DRAT .LT. 0.01) DRAT = 0.01
               GEN = DRAT * ELWDTH(IVSL,IS) * SOLVAL(1) * SHR_C
               S_GEN = S_GEN + GEN
               F_GEN = F_GEN + DRAT * ELWDTH(IVSL,IS)
               ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN
               ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN
               ULTGEN(IGRP) = ULTGEN(IGRP) + GEN
            END IF

!           IF (CURIYR+1989 .EQ. 2020 .AND. FCRL .EQ. 1) THEN
!              WRITE(6,3751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IVSL, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, COL, COL_MIN, "", &
!                 SOLVAL(1)*SHR_C, SOLVAL_MIN(1)*SHR_C, 0.0, &
!                 GEN, GEN_MIN, 0.0, &
!                 DRAT, DRAT_MIN, 0.0, DRAT_ORG, SR_CREDIT(IECP), &
!                 ULTGEN(IGRP), ECDSPE(IGRP,IS), CAP(IS), MAXCF(IS), SHR_C, ELWDTH(IVSL,IS), UTHGHT(SEG,GRP,IRG), 0.0, 0.0
!3751          FORMAT(1X,"GRP_DATA_RTPEAK",11(":",I5),3(":",A8),20(":",F20.6))
!           END IF

         ENDDO   !IVSL

         LL = F_GEN / SHOURS

         Load_Level = MIN( LL , 1.0)
         ECPt = IECP
         FUEL_RGN = UG_FL_RG(I_EFD_GRP)
         FROM_LABEL = "PKO_" // COL

         CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)

         ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + S_GEN * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ
         IF (SR_CREDIT(IECP) .GT. 0.0) THEN
            LL_MIN = F_GEN_MIN / SHOURS

            Load_Level = MIN( LL_MIN , 1.0)
            ECPt = IECP
            FUEL_RGN = UG_FL_RG(I_EFD_GRP)
            FROM_LABEL = "PKO_" // COL_MIN

            CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ_MIN, Target_EFF, Max_EFF)

            ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + S_GEN_MIN * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ_MIN
         END IF

!        IF (FCRL .EQ. 1 .AND. CURIYR+UHBSYR .LE. 2017) THEN
!           WRITE(18,5751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, &
!              SR_CREDIT(IECP), 1.0-UG_FOR(I_EFD_GRP), SHOURS, SHR_C, ECDSPE(IGRP,IS), ECDSPF(IGRP,IS), UP_CAP(IS,IGRP), UG_CAP(IS,I_EFD_GRP), ULHTRT_EFD(IGRP,IS), &
!              COL, S_GEN, LL, HTRT_ADJ, COL_MIN, S_GEN_MIN, LL_MIN, HTRT_ADJ_MIN, " ", S_GEN_MAX, LL_MAX, HTRT_ADJ_MAX
!5751       FORMAT(1X,"ECDSPF_DATA_RTPEAK",10(":",I5),9(":",F21.6),3(":",A16,":",F21.6,":",F21.6,":",F21.6))
!        END IF

      ENDDO    !SEASON

      RETURN
      END
!
!     This subroutine retrieves operates for baseload plants that operate in
!      a fixed mode of every slice - used for geothermal, biomass and MSW

      SUBROUTINE RTRNBS(N,IRG,ITYP)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'plntctl'
      include'ecpcntl'
      include'elcntl'
      include'elout'
      include'dispuse'
      include'dispcrv'
      include'ecp_nuc'
      include'cdsparms'
      include'emm_aimms'

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,IRET,JGRP,I_EFD_GRP
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP
      INTEGER*4  FUEL_RGN, ECPt
      REAL CF(EFD_D_MVS,EFD_D_MSP),VOM,HTRT,CAP(EFD_D_MSP),HRVAL,MAXCF(EFD_D_MSP)
      REAL*8 DRAT,GEN,OBJVAL,BTUREQ,VAL,MRCAP,SOLVAL(5),SHR_C,SHR_G
      REAL*8 DRAT_ORG,DRAT_MIN,DRAT_MAX,GEN_MIN,GEN_MAX,BTUREQ_MIN,BTUREQ_MAX,VAL_SR,VAL_MIN,VAL_MAX,NOXEM_MIN,NOXEM_MAX,GENSR,GEN_MINSR,GEN_MAXSR
      REAL*8 MAX_SP_LOAD, MIN_SP_LOAD, SOLVAL_MIN(5), SOLVAL_MAX(5), FACTOR, FACTOR_MIN, FACTOR_MAX
      REAL*8 S_GEN, S_GEN_MIN, S_GEN_MAX, F_GEN, F_GEN_MIN, F_GEN_MAX, SHOURS, LL, LL_MIN, LL_MAX
      REAL*8 Load_Level, HTRT_ADJ, HTRT_ADJ_MIN, HTRT_ADJ_MAX, Target_EFF, Max_EFF
      CHARACTER*1 FSLCD
      CHARACTER*5 NUM
      CHARACTER*16 COL, COL_MIN, COL_MAX
      CHARACTER*12 FROM_LABEL
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*2 STAT, STAT_MIN, STAT_MAX

      efdsub='RTRNBS'

      IF (ITYP .EQ. 1) THEN
         IGRP = ECDBID(N)
         I_EFD_GRP = MAP_ECNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 2) THEN
         IGRP = EHDBID(N)
         I_EFD_GRP = MAP_EHNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 3) THEN
         IGRP = EDDBID(N)
         I_EFD_GRP = MAP_EDNTP_EFD_GRPS(N,IRG)
      ENDIF
      JGRP = EFD_GRPS_F(I_EFD_GRP)
      SHR_G = UP_GEN(0,IGRP) / UG_GEN(0,I_EFD_GRP)

      IECP = UG_ECPt(I_EFD_GRP)

      WRITE(NUM,'(I5.5)') JGRP

      DO IS = 1, EENSP
         IF (ITYP .EQ. 1) THEN
            CAP(IS) = ECCAP(N,IS)
            MAXCF(IS) = ECMXCP(N)*EFACTR
         ELSEIF (ITYP .EQ. 2) THEN
            CAP(IS) = EHCAP(N,IS)
            MAXCF(IS) = EHHYCF(N,IS)*EFACTR
         ELSEIF (ITYP .EQ. 3) THEN
            CAP(IS) = EDCAP(N,IS)
            MAXCF(IS) = EDMXCP(N)*EFACTR
         ENDIF
         MAXCF(IS) = MAX(MAXCF(IS) , 0.0001)

!        STEOBM adjust CF for STEO benchmarking (factor will be 1.0 if no benchmarking)

         IF (IECP .EQ. WIGT) THEN
            MAXCF(IS) = MAXCF(IS) * URGTCFA(CURIYR)   
         ENDIF
      ENDDO

!     loop over time season and slices
!     only one mode that crosses all load slices

      DO IS = 1, EENSP

         IF (UG_CAP(IS,I_EFD_GRP) .GT. 0.0) THEN
            SHR_C = UP_CAP(IS,IGRP) / UG_CAP(IS,I_EFD_GRP)
         ELSE
            SHR_C = UP_CAP(0,IGRP) / UG_CAP(0,I_EFD_GRP)
         END IF

         MAX_SP_LOAD = 0.0
         MIN_SP_LOAD = 500000.0

         DO IVSL = 1 , ELNVCT(IS)
            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)
            MAX_SP_LOAD = MAX(MAX_SP_LOAD , UTHGHT(SEG,GRP,IRG))
            MIN_SP_LOAD = MIN(MIN_SP_LOAD , UTHGHT(SEG,GRP,IRG))
         END DO

         IF (SR_CREDIT(IECP) .GT. 0.0) THEN
            COL = 'O'//NUM//'X'//CHCOD(IS); call makmsk(COL_mask,':O:',NUM,':X:',CHCOD(IS))
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
            COL_MIN = 'J'//NUM//'X'//CHCOD(IS); call makmsk(COL_MIN_mask,':J:',NUM,':X:',CHCOD(IS))
            CALL DWFSCOL(COL_MIN,COLSOL,STAT_MIN,SOLVAL_MIN,COL_MIN_mask,IRET)
            COL_MAX = 'U'//NUM//'X'//CHCOD(IS); call makmsk(COL_MAX_mask,':U:',NUM,':X:',CHCOD(IS))
            CALL DWFSCOL(COL_MAX,COLSOL,STAT_MAX,SOLVAL_MAX,COL_MAX_mask,IRET)
         ELSE
            COL = 'O'//NUM//'X'//CHCOD(IS); call makmsk(COL_mask,':O:',NUM,':X:',CHCOD(IS))
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
         END IF

!        Combined Capacity factor for ECP types with SR_CREDIT can not exceed MAXCF(IS)

         IF (SR_CREDIT(IECP) .GT. 0.0) THEN

            FACTOR = 0.0
            FACTOR_MIN = 0.0
            FACTOR_MAX = 0.0

            DO IVSL = 1 , ELNVCT(IS)
               GRP = ELGRP(IVSL,IS)
               SEG = ELSEG(IVSL,IS)

               DRAT_ORG = MAXCF(IS)

!              Load Following Mode Follows Load Height drat = (1 - for) * (ld / max_ld)

               DRAT = (1.0 - UG_FOR(I_EFD_GRP)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD)
               FACTOR = FACTOR + DRAT * ELWDTH(IVSL,IS)

!              Min Electricity - Max Spinning Reserve Credit drat_min = (1 - sr_min_lf) * sr_min_cf * (ld / min_ld) + sr_min_lf * sr_min_cf

               DRAT_MIN = (1.0 - SR_MIN_LF(IECP)) * SR_MIN_CF(IECP) * (UTHGHT(SEG,GRP,IRG) / MIN_SP_LOAD) + SR_MIN_LF(IECP) * SR_MIN_CF(IECP)
               FACTOR_MIN = FACTOR_MIN + DRAT_MIN * ELWDTH(IVSL,IS)

!              Max Electricity - Min Spinning Reserve Credit drat_max = (1 - sr_max_lf) * (1 - for) * (ld / max_ld) + sr_max_lf * (1 - for)

               DRAT_MAX = (1.0 - SR_MAX_LF(IECP)) * (1.0 - UG_FOR(I_EFD_GRP)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD) + &
                           SR_MAX_LF(IECP) * (1.0 - UG_FOR(I_EFD_GRP))

               IF (UG_MRUN(I_EFD_GRP) .GT. 0 .AND. DRAT_MAX .LT. DRAT_ORG) THEN
                  DRAT_MAX = MAX(DRAT_MAX , DRAT_ORG)
               END IF
               FACTOR_MAX = FACTOR_MAX + DRAT_MAX * ELWDTH(IVSL,IS)

            END DO

            FACTOR = MIN(1.0 , MAXCF(IS) * EETIME(IS) / FACTOR)
            FACTOR_MIN = MIN(1.0 , MAXCF(IS) * EETIME(IS) / FACTOR_MIN) 
            FACTOR_MAX = MIN(1.0 , MAXCF(IS) * EETIME(IS) / FACTOR_MAX) 

         END IF

         SHOURS = 0.0
         S_GEN = 0.0
         F_GEN = 0.0
         LL = 1.0
         HTRT_ADJ = 1.0
         S_GEN_MIN = 0.0
         F_GEN_MIN = 0.0
         LL_MIN = 1.0
         HTRT_ADJ_MIN = 1.0
         S_GEN_MAX = 0.0
         F_GEN_MAX = 0.0
         LL_MAX = 1.0
         HTRT_ADJ_MAX = 1.0

         DO IVSL = 1 , ELNVCT(IS)
            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)
            SHOURS = SHOURS + DBLE(ELWDTH(IVSL,IS))

!           IF (ITYP .EQ. 1) THEN
!              CALL ELDRAT(IS,IRG,I_EFD_GRP,IVSL,DRAT)
!           ELSE

            DRAT_ORG = MAXCF(IS)

!           For ECP Types which have Spinning Reserve Credit Create Min, Max and Load Following Modes for each of the original modes

            IF (SR_CREDIT(IECP) .GT. 0.0) THEN

!              Load Following Mode Follows Load Height drat = (1 - for) * (ld / max_ld)

               DRAT = (1.0 - UG_FOR(I_EFD_GRP)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD)

!              Min Electricity - Max Spinning Reserve Credit drat_min = (1 - sr_min_lf) * sr_min_cf * (ld / min_ld) + sr_min_lf * sr_min_cf

               DRAT_MIN = (1.0 - SR_MIN_LF(IECP)) * SR_MIN_CF(IECP) * (UTHGHT(SEG,GRP,IRG) / MIN_SP_LOAD) + SR_MIN_LF(IECP) * SR_MIN_CF(IECP)

!              Max Electricity - Min Spinning Reserve Credit drat_max = (1 - sr_max_lf) * (1 - for) * (ld / max_ld) + sr_max_lf * (1 - for)

               DRAT_MAX = (1.0 - SR_MAX_LF(IECP)) * (1.0 - UG_FOR(I_EFD_GRP)) * (UTHGHT(SEG,GRP,IRG) / MAX_SP_LOAD) + &
                           SR_MAX_LF(IECP) * (1.0 - UG_FOR(I_EFD_GRP))
                   
               IF (UG_MRUN(I_EFD_GRP) .GT. 0 .AND. DRAT_MAX .LT. DRAT_ORG) THEN
                  DRAT_MAX = MAX(DRAT_MAX , DRAT_ORG)
               END IF

               DRAT = DRAT * FACTOR
               DRAT_MIN = DRAT_MIN * FACTOR_MIN
               DRAT_MAX = DRAT_MAX * FACTOR_MAX

               IF (IECP .EQ. WIMS) THEN
                  DRAT_MAX = DRAT_ORG
               END IF
                   
            ELSE
               DRAT = DRAT_ORG
            END IF

!           ENDIF

            IF (SR_CREDIT(IECP) .GT. 0.0) THEN
               IF (DRAT .LT. 0.01) DRAT = 0.01
               GEN = DRAT * ELWDTH(IVSL,IS) * SOLVAL(1) * SHR_C
               S_GEN = S_GEN + GEN
               F_GEN = F_GEN + DRAT * ELWDTH(IVSL,IS)
               GENSR = (1.0 - DRAT) * SR_CREDIT(IECP) * SOLVAL(1) * SHR_C
               ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN
               ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN
               ULTGEN(IGRP) = ULTGEN(IGRP) + GEN
               SP_ACHBYECP(GRP,SEG,IRG,IECP) = SP_ACHBYECP(GRP,SEG,IRG,IECP) + GENSR

               IF (DRAT_MIN .LT. 0.01) DRAT_MIN = 0.01
               GEN_MIN = DRAT_MIN * ELWDTH(IVSL,IS) * SOLVAL_MIN(1) * SHR_C
               S_GEN_MIN = S_GEN_MIN + GEN_MIN
               F_GEN_MIN = F_GEN_MIN + DRAT_MIN * ELWDTH(IVSL,IS)
               GEN_MINSR = (1.0 - DRAT_MIN) * SR_CREDIT(IECP) * SOLVAL_MIN(1) * SHR_C
               ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN_MIN
               ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN_MIN
               ULTGEN(IGRP) = ULTGEN(IGRP) + GEN_MIN
               SP_ACHBYECP(GRP,SEG,IRG,IECP) = SP_ACHBYECP(GRP,SEG,IRG,IECP) + GEN_MINSR

               IF (DRAT_MAX .LT. 0.01) DRAT_MAX = 0.01
               GEN_MAX = DRAT_MAX * ELWDTH(IVSL,IS) * SOLVAL_MAX(1) * SHR_C
               S_GEN_MAX = S_GEN_MAX + GEN_MAX
               F_GEN_MAX = F_GEN_MAX + DRAT_MAX * ELWDTH(IVSL,IS)
               GEN_MAXSR = (1.0 - DRAT_MAX) * SR_CREDIT(IECP) * SOLVAL_MAX(1) * SHR_C
               ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN_MAX
               ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN_MAX
               ULTGEN(IGRP) = ULTGEN(IGRP) + GEN_MAX
               SP_ACHBYECP(GRP,SEG,IRG,IECP) = SP_ACHBYECP(GRP,SEG,IRG,IECP) + GEN_MAXSR
            ELSE
               IF (DRAT .LT. 0.01) DRAT = 0.01
               GEN = DRAT * ELWDTH(IVSL,IS) * SOLVAL(1) * SHR_C
               S_GEN = S_GEN + GEN
               F_GEN = F_GEN + DRAT * ELWDTH(IVSL,IS)
               ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN
               ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN
               ULTGEN(IGRP) = ULTGEN(IGRP) + GEN
            END IF

!           IF (CURIYR+1989 .EQ. 2020 .AND. FCRL .EQ. 1) THEN
!              WRITE(6,3751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IVSL, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, COL, COL_MIN, COL_MAX, &
!                 SOLVAL(1)*SHR_C, SOLVAL_MIN(1)*SHR_C, SOLVAL_MAX(1)*SHR_C, &
!                 GEN, GEN_MIN, GEN_MAX, &
!                 DRAT, DRAT_MIN, DRAT_MAX, DRAT_ORG, SR_CREDIT(IECP), &
!                 ULTGEN(IGRP), ECDSPE(IGRP,IS), CAP(IS), MAXCF(IS), SHR_C, ELWDTH(IVSL,IS), UTHGHT(SEG,GRP,IRG), MIN_SP_LOAD, MAX_SP_LOAD
!3751          FORMAT(1X,"GRP_DATA_RTRNBS",11(":",I5),3(":",A8),20(":",F20.6))
!           END IF

         ENDDO   !IVSL

         LL = F_GEN / SHOURS

         Load_Level = MIN( LL , 1.0)
         ECPt = IECP
         FUEL_RGN = UG_FL_RG(I_EFD_GRP)
         FROM_LABEL = "RNO_" // COL

         CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)


         ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + S_GEN * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ
         IF (SR_CREDIT(IECP) .GT. 0.0) THEN
            LL_MIN = F_GEN_MIN / SHOURS

            Load_Level = MIN( LL_MIN , 1.0)
            ECPt = IECP
            FUEL_RGN = UG_FL_RG(I_EFD_GRP)
            FROM_LABEL = "RNO_" // COL_MIN

            CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ_MIN, Target_EFF, Max_EFF)

            ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + S_GEN_MIN * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ_MIN

            LL_MAX = F_GEN_MAX / SHOURS

            Load_Level = MIN( LL_MAX , 1.0)
            ECPt = IECP
            FUEL_RGN = UG_FL_RG(I_EFD_GRP)
            FROM_LABEL = "RNO_" // COL_MAX

            CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ_MAX, Target_EFF, Max_EFF)

            ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + S_GEN_MAX * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ_MAX
         END IF

!        IF (FCRL .EQ. 1 .AND. CURIYR+UHBSYR .LE. 2017) THEN
!           WRITE(18,5751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, &
!              SR_CREDIT(IECP), 1.0-UG_FOR(I_EFD_GRP), SHOURS, SHR_C, ECDSPE(IGRP,IS), ECDSPF(IGRP,IS), UP_CAP(IS,IGRP), UG_CAP(IS,I_EFD_GRP), ULHTRT_EFD(IGRP,IS), &
!              COL, S_GEN, LL, HTRT_ADJ, COL_MIN, S_GEN_MIN, LL_MIN, HTRT_ADJ_MIN, COL_MAX, S_GEN_MAX, LL_MAX, HTRT_ADJ_MAX
!5751       FORMAT(1X,"ECDSPF_DATA_RTRNBS",10(":",I5),9(":",F21.6),3(":",A16,":",F21.6,":",F21.6,":",F21.6))
!        END IF

      ENDDO  ! IS

      RETURN
      END
!
!     This subroutine retrieves operates for intermittent plants
!

      SUBROUTINE RTINT(N,IRG,ITYP)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'plntctl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtfecp'
      include'dsmtoefd'
      include'elcntl'
      include'elout'
      include'dispuse'
      include'dispcrv'
      include'ecp_nuc'
      include'cdsparms'
      include'emm_aimms'

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,IRET,JGRP,I_EFD_GRP,INT
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,FSL
      INTEGER*4  DOAMINOF, TEST_RUNS
      INTEGER*4  FUEL_RGN, ECPt
      REAL CF,VOM,HTRT,CAP(EFD_D_MSP),HRVAL,MAXCF(EFD_D_MSP)
      REAL*8 DRAT,GEN,OBJVAL,BTUREQ,MRCAP,SOLVAL(5),SHR_C,SHR_G,VAL_SR,CURT
      REAL*8 S_GEN, S_GEN_MIN, S_GEN_MAX, F_GEN, F_GEN_MIN, F_GEN_MAX, SHOURS, LL, LL_MIN, LL_MAX
      REAL*8 Load_Level, HTRT_ADJ, HTRT_ADJ_MIN, HTRT_ADJ_MAX, Target_EFF, Max_EFF
      CHARACTER*1 FSLCD
      CHARACTER*5 NUM
      CHARACTER*16 COL
      CHARACTER*12 FROM_LABEL
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*2 STAT

      efdsub='RTINT'

      DOAMINOF = RTOVALUE('DOAMINOF',1) !  Minimum number of cycles requested
      TEST_RUNS = MIN(NUMIRUNS , DOAMINOF)
      IF (ITYP .EQ. 1) THEN
         IGRP = ECDBID(N)
         I_EFD_GRP = MAP_ECNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 2) THEN
         IGRP = EHDBID(N)
         I_EFD_GRP = MAP_EHNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 3) THEN
         IGRP = EDDBID(N)
         I_EFD_GRP = MAP_EDNTP_EFD_GRPS(N,IRG)
      ENDIF
      JGRP = EFD_GRPS_F(I_EFD_GRP)
      SHR_G = UP_GEN(0,IGRP) / UG_GEN(0,I_EFD_GRP)

      WRITE(NUM,'(I5.5)') JGRP

      IECP = UG_ECPt(I_EFD_GRP)
      INT = UCPINTIS(IECP)

!     loop over time season and slices

      DO IS = 1, EENSP

         IF (UG_CAP(IS,I_EFD_GRP) .GT. 0.0) THEN
            SHR_C = UP_CAP(IS,IGRP) / UG_CAP(IS,I_EFD_GRP)
         ELSE
            SHR_C = UP_CAP(0,IGRP) / UG_CAP(0,I_EFD_GRP)
         END IF

         SHOURS = 0.0
         S_GEN = 0.0
         F_GEN = 0.0
         LL = 1.0
         HTRT_ADJ = 1.0
         S_GEN_MIN = 0.0
         F_GEN_MIN = 0.0
         LL_MIN = 1.0
         HTRT_ADJ_MIN = 1.0
         S_GEN_MAX = 0.0
         F_GEN_MAX = 0.0
         LL_MAX = 1.0
         HTRT_ADJ_MAX = 1.0

         DO IVSL = 1 , ELNVCT(IS)

            SHOURS = SHOURS + DBLE(ELWDTH(IVSL,IS))

            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)
            IF (CURCALYR .GE. UPSTYR) THEN
                IF (IECP .EQ. WIPT) THEN
                    CF = HREFDCF(1,INT,1,SEG,GRP,IRG)
                ! Distributed PV is second RESTORE step
				ELSEIF (IECP .EQ. WIPV .AND. UG_MRUN(I_EFD_GRP) .GT. 0) THEN
					IF (HREFDCF(0,INT,1,SEG,GRP,IRG) .GT. 0.0001) THEN
						CF = (EHLDCF(N,SEG,GRP) * EFACTR) * (HREFDCF(1,INT,2,SEG,GRP,IRG) / HREFDCF(0,INT,1,SEG,GRP,IRG))
                    ELSE
                    	CF = EHLDCF(N,SEG,GRP) * EFACTR
                    ENDIF
                ELSE  
                    IF (HREFDCF(0,INT,1,SEG,GRP,IRG) .GT. 0.0001) THEN
                        CF = (EHLDCF(N,SEG,GRP) * EFACTR) * (HREFDCF(1,INT,1,SEG,GRP,IRG) / HREFDCF(0,INT,1,SEG,GRP,IRG))
                    ELSE
                       CF = EHLDCF(N,SEG,GRP) * EFACTR
                    END IF
                END IF
            ELSE
               CF = EHLDCF(N,SEG,GRP) * EFACTR
            END IF

!STEOBM     apply CF adjustment for benchmarking (will be 1.0 if no benchmarking) 

            IF (IECP .EQ. WISO) CF = CF * URSOCFA(CURIYR)
            IF (IECP .EQ. WIPV .OR. IECP .EQ. WIPT) CF = CF * URSOCFA(CURIYR)   !use same overall solar factor for all
            IF (IECP .EQ. WIWN .OR. IECP .EQ. WIWL .OR. IECP .EQ. WIWF) CF = CF * URWNCFA(CURIYR)

!END STEOBM

            IF (CF .GT. 0.0) THEN
               FSL = EFD_Slice_ID(GRP,SEG)
               FSLCD = CHAR(64+FSL)
               COL = 'I'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_mask,':I:',NUM,CHCOD(IS),FSLCD)
               CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
               GEN = CF * ELWDTH(IVSL,IS) * SOLVAL(1) * SHR_C
               CURT = CF * ELWDTH(IVSL,IS) * (SOLVAL(4) - SOLVAL(1)) * SHR_C
               EFDCURT(IRG,INT,CURIYR) = EFDCURT(IRG,INT,CURIYR) + CURT
               IF (CURT .GT. 0.0 .AND. FCRL .EQ. 1) WRITE (UF_DBG,666) 'EFDCURT grp',CURIYR,IRG,IECP,N,NUM,IGRP,SOLVAL(4),SOLVAL(1),SHR_C,CF,ELWDTH(IVSL,IS),CURT
666    FORMAT(1X,A15,4I6,A10,I6,6F15.5)
               S_GEN = S_GEN + GEN
               F_GEN = F_GEN + CF * ELWDTH(IVSL,IS)
               ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN
               ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN
               ULTGEN(IGRP) = ULTGEN(IGRP) + GEN
               UCRNW(SEG,GRP,IRG) = UCRNW(SEG,GRP,IRG) + SOLVAL(1) * SHR_C
               UCRPS(SEG,GRP,IRG) = UCRPS(SEG,GRP,IRG) + SOLVAL(1) * UPRNWSHR(IECP) * SHR_C

               IF (SR_INT(IECP,IRG) .GT. 0.0) THEN
                  VAL_SR = SOLVAL(1) * CF * SR_INT(IECP,IRG) * SHR_C
                  SR_INT_REQ(GRP,SEG,IRG) = SR_INT_REQ(GRP,SEG,IRG) + VAL_SR
               END IF

               !spinning reserve achieved by PT battery part
               IF (SR_CREDIT(IECP) .GT. 0.0 .AND. IECP .EQ. WIPT) THEN
                  IF (NET_PT_STORAGE_SR_EFD(SEG,GRP,IRG,CURIYR) .GT. 0.0) THEN
                      VAL_SR = SOLVAL(1) * NET_PT_STORAGE_SR_EFD(SEG,GRP,IRG,CURIYR) * SHR_C
                      SP_ACHBYECP(GRP,SEG,IRG,IECP) = SP_ACHBYECP(GRP,SEG,IRG,IECP) + VAL_SR
                  END IF
               END IF

!              IF (SR_INT(IECP,IRG) .GT. 0.0 .AND. FCRL .EQ. 1 .AND. CURIRUN .GE. TEST_RUNS) THEN
!                 VAL_SR = SOLVAL(1) * CF * SR_INT(IECP,IRG) * SHR_C
!                 WRITE(6,3752) CURIRUN, CURIYR+1989, CURITR, IRG, IECP, IS, IVSL, GRP, SEG, COL, &
!                    CF, SOLVAL(1), SHR_C, SR_INT(IECP,IRG), VAL_SR, GEN, ELWDTH(IVSL,IS)
!3752             FORMAT(1X,"SR_INT_INFO",9(":",I5),":",A16,7(":",F21.6))
!              END IF

            ENDIF
         ENDDO   !IVSL

         LL = F_GEN / SHOURS

         Load_Level = MIN( LL , 1.0)
         ECPt = IECP
         FUEL_RGN = UG_FL_RG(I_EFD_GRP)
         FROM_LABEL = "INO_" // COL

         CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)

         ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + S_GEN * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ

!        IF (FCRL .EQ. 1 .AND. CURIYR+UHBSYR .LE. 2017) THEN
!           WRITE(18,5751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, &
!              SR_CREDIT(IECP), 1.0-UG_FOR(I_EFD_GRP), SHOURS, SHR_C, ECDSPE(IGRP,IS), ECDSPF(IGRP,IS), UP_CAP(IS,IGRP), UG_CAP(IS,I_EFD_GRP), ULHTRT_EFD(IGRP,IS), &
!              COL, S_GEN, LL, HTRT_ADJ, " ", S_GEN_MIN, LL_MIN, HTRT_ADJ_MIN, " ", S_GEN_MAX, LL_MAX, HTRT_ADJ_MAX
!5751       FORMAT(1X,"ECDSPF_DATA_RTINT ",10(":",I5),9(":",F21.6),3(":",A16,":",F21.6,":",F21.6,":",F21.6))
!        END IF


!        IF (CURIYR .EQ. 16 .AND. FCRL .EQ. 1) THEN
!           WRITE(18,3751) CURIYR+UHBSYR,CURITR,IS,IRG,IGRP,N,ITYP,COL,SOLVAL(1)*SHR_C,ULTGEN(IGRP),ECDSPE(IGRP,IS),CAP(IS),MAXCF(IS),SHR_C
!3751       FORMAT(1X,"BAD_GRP_DATA_RTINT ",7(":",I5),":",A8,6(":",F12.3))
!        END IF
!
      ENDDO    !SEASON

      RETURN
      END
!
!     This subroutine retrieves operates for storage plants
!

      SUBROUTINE RTSTR(N,IRG,ITYP)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'plntctl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'elcntl'
      include'elout'
      include'dispuse'
      include'dispcrv'
      include'ecp_nuc'
      include'cdsparms'
      include'emm_aimms'

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,IRET,JGRP,I_EFD_GRP
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,FSL
      INTEGER*4  FUEL_RGN, ECPt
      REAL CF,VOM,HTRT,CAP(EFD_D_MSP),HRVAL,MAXCF(EFD_D_MSP), CF_SR
      REAL*8 DRAT,GEN,OBJVAL,BTUREQ,MRCAP,SOLVAL(5),SHR_C,SHR_G,VAL_SR
      REAL*8 S_GEN, S_GEN_MIN, S_GEN_MAX, F_GEN, F_GEN_MIN, F_GEN_MAX, SHOURS, LL, LL_MIN, LL_MAX
      REAL*8 Load_Level, HTRT_ADJ, HTRT_ADJ_MIN, HTRT_ADJ_MAX, Target_EFF, Max_EFF
      CHARACTER*1 FSLCD
      CHARACTER*5 NUM
      CHARACTER*16 COL
      CHARACTER*12 FROM_LABEL
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*2 STAT

      efdsub='RTSTR'


      IF (ITYP .EQ. 1) THEN
         IGRP = ECDBID(N)
         I_EFD_GRP = MAP_ECNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 2) THEN
         IGRP = EHDBID(N)
         I_EFD_GRP = MAP_EHNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 3) THEN
         IGRP = EDDBID(N)
         I_EFD_GRP = MAP_EDNTP_EFD_GRPS(N,IRG)
      ENDIF

      JGRP = EFD_GRPS_F(I_EFD_GRP)
      SHR_G = UP_GEN(0,IGRP) / UG_GEN(0,I_EFD_GRP)

      WRITE(NUM,'(I5.5)') JGRP

      IECP = UG_ECPt(I_EFD_GRP)

!     loop over time season and slices

      DO IS = 1, EENSP

         IF (UG_CAP(IS,I_EFD_GRP) .GT. 0.0) THEN
            SHR_C = UP_CAP(IS,IGRP) / UG_CAP(IS,I_EFD_GRP)
         ELSE
            SHR_C = UP_CAP(0,IGRP) / UG_CAP(0,I_EFD_GRP)
         END IF

         SHOURS = 0.0
         S_GEN = 0.0
         F_GEN = 0.0
         LL = 1.0
         HTRT_ADJ = 1.0
         S_GEN_MIN = 0.0
         F_GEN_MIN = 0.0
         LL_MIN = 1.0
         HTRT_ADJ_MIN = 1.0
         S_GEN_MAX = 0.0
         F_GEN_MAX = 0.0
         LL_MAX = 1.0
         HTRT_ADJ_MAX = 1.0

         DO IVSL = 1 , ELNVCT(IS)

            SHOURS = SHOURS + DBLE(ELWDTH(IVSL,IS))

            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)

!           CF = EHLDCF(N,SEG,GRP) * EFACTR

            CF = NET_STORAGE_LOAD_EFD(SEG,GRP,IRG,CURIYR)
            CF_SR = NET_STORAGE_SR_EFD(SEG,GRP,IRG,CURIYR)

            IF (CF .NE. 0.0) THEN
               FSL = EFD_Slice_ID(GRP,SEG)
               FSLCD = CHAR(64+FSL)
               COL = 'I'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_mask,':I:',NUM,CHCOD(IS),FSLCD)
               CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
               GEN = CF * ELWDTH(IVSL,IS) * SOLVAL(1) * SHR_C
               S_GEN = S_GEN + GEN
               F_GEN = F_GEN + CF * ELWDTH(IVSL,IS)
               ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN
               ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN
               ULTGEN(IGRP) = ULTGEN(IGRP) + GEN
               UCRNW(SEG,GRP,IRG) = UCRNW(SEG,GRP,IRG) + SOLVAL(1) * SHR_C
               UCRPS(SEG,GRP,IRG) = UCRPS(SEG,GRP,IRG) + SOLVAL(1) * UPRNWSHR(IECP) * SHR_C
               
               IF (CF_SR .NE. 0.0) THEN
                  VAL_SR = SOLVAL(1) * CF_SR * SHR_C
                  SP_ACHBYECP(GRP,SEG,IRG,IECP) = SP_ACHBYECP(GRP,SEG,IRG,IECP) + VAL_SR
               END IF


               IF (SR_INT(IECP,IRG) .GT. 0.0) THEN
                  VAL_SR = SOLVAL(1) * CF * SR_INT(IECP,IRG) * SHR_C
                  SR_INT_REQ(GRP,SEG,IRG) = SR_INT_REQ(GRP,SEG,IRG) + VAL_SR
               END IF

            ENDIF
         ENDDO   !IVSL

         LL = F_GEN / SHOURS

         Load_Level = MIN( LL , 1.0)
         ECPt = IECP
         FUEL_RGN = UG_FL_RG(I_EFD_GRP)
         FROM_LABEL = "INO_" // COL

         CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)

         ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + S_GEN * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ

!        IF (FCRL .EQ. 1 .AND. CURIYR+UHBSYR .LE. 2017) THEN
!           WRITE(18,5751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, &
!              SR_CREDIT(IECP), 1.0-UG_FOR(I_EFD_GRP), SHOURS, SHR_C, ECDSPE(IGRP,IS), ECDSPF(IGRP,IS), UP_CAP(IS,IGRP), UG_CAP(IS,I_EFD_GRP), ULHTRT_EFD(IGRP,IS), &
!              COL, S_GEN, LL, HTRT_ADJ, " ", S_GEN_MIN, LL_MIN, HTRT_ADJ_MIN, " ", S_GEN_MAX, LL_MAX, HTRT_ADJ_MAX
!5751       FORMAT(1X,"ECDSPF_DATA_RTSTR ",10(":",I5),9(":",F21.6),3(":",A16,":",F21.6,":",F21.6,":",F21.6))
!        END IF


!        IF (CURIYR .EQ. 16 .AND. FCRL .EQ. 1) THEN
!           WRITE(18,3751) CURIYR+UHBSYR,CURITR,IS,IRG,IGRP,N,ITYP,COL,SOLVAL(1)*SHR_C,ULTGEN(IGRP),ECDSPE(IGRP,IS),CAP(IS),MAXCF(IS),SHR_C
!3751       FORMAT(1X,"BAD_GRP_DATA_RTSTR ",7(":",I5),":",A8,6(":",F12.3))
!        END IF
!
      ENDDO    !SEASON

      RETURN
      END
!
!     This subroutine retrieves operates for hydro and pumped storage plants
!

      SUBROUTINE RTHYD(N,IRG,ITYP)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'plntctl'
      include'ecpcntl'
      include'dsmdimen'
      include'dsmtoefd'
      include'elcntl'
      include'elout'
      include'dispuse'
      include'dispcrv'
      include'ecp_nuc'
      include'cdsparms'
      include'emm_aimms'

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,IRET,JGRP,I_EFD_GRP,IRET_MIN
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,FSL
      INTEGER*4  FUEL_RGN, ECPt
      REAL CF,VOM,HTRT,CAP(EFD_D_MSP),HRVAL,MAXCF(EFD_D_MSP)
      REAL PSUSE,PSFAC
      REAL*8 DRAT,GEN,OBJVAL,BTUREQ,MRCAP,SOLVAL(5),SOLVAL2(5),SHR_C,SHR_G,SOLVAL_MIN(5),SOLVAL_MIN2(5),GEN_MIN,CF_MIN,GENSR,GEN_MINSR
      REAL*8 S_GEN, S_GEN_MIN, S_GEN_MAX, F_GEN, F_GEN_MIN, F_GEN_MAX, SHOURS, LL, LL_MIN, LL_MAX
      REAL*8 Load_Level, HTRT_ADJ, HTRT_ADJ_MIN, HTRT_ADJ_MAX, Target_EFF, Max_EFF
      CHARACTER*1 FSLCD
      CHARACTER*5 NUM
      CHARACTER*16 COL,COL_MIN
      CHARACTER*12 FROM_LABEL
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*2 STAT,STAT_MIN

      efdsub='RTHYD'


      IF (ITYP .EQ. 1) THEN
         IGRP = ECDBID(N)
         I_EFD_GRP = MAP_ECNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 2) THEN
         IGRP = EHDBID(N)
         IECP = EHTECP(N)
         I_EFD_GRP = MAP_EHNTP_EFD_GRPS(N,IRG)
      ELSEIF (ITYP .EQ. 3) THEN
         IGRP = EDDBID(N)
         I_EFD_GRP = MAP_EDNTP_EFD_GRPS(N,IRG)
      ENDIF
      JGRP = EFD_GRPS_F(I_EFD_GRP)
      SHR_G = UP_GEN(0,IGRP) / UG_GEN(0,I_EFD_GRP)

      WRITE(NUM,'(I5.5)') JGRP

      IECP = UG_ECPt(I_EFD_GRP)
      IP = UG_EFDt(I_EFD_GRP)

      PSFAC = 1.36

!     loop over time season and slices

      DO IS = 1, EENSP

         IF (UG_CAP(IS,I_EFD_GRP) .GT. 0.0) THEN
            SHR_C = UP_CAP(IS,IGRP) / UG_CAP(IS,I_EFD_GRP)
         ELSE
            SHR_C = UP_CAP(0,IGRP) / UG_CAP(0,I_EFD_GRP)
         END IF

         MAXCF(IS) = EHHYCF(N,IS) * EFACTR

         DO IVSL = 1 , ELNVCT(IS)
            SHOURS = 0.0
            S_GEN = 0.0
            F_GEN = 0.0
            LL = 1.0
            HTRT_ADJ = 1.0
            S_GEN_MIN = 0.0
            F_GEN_MIN = 0.0
            LL_MIN = 1.0
            HTRT_ADJ_MIN = 1.0
            S_GEN_MAX = 0.0
            F_GEN_MAX = 0.0
            LL_MAX = 1.0
            HTRT_ADJ_MAX = 1.0

            SHOURS = SHOURS + DBLE(ELWDTH(IVSL,IS))

            GRP = ELGRP(IVSL,IS)
            SEG = ELSEG(IVSL,IS)
            IF (IECP .EQ. WIHY .AND. CURIYR + UHBSYR .GE. UPSTYR) THEN
               CF = (1.0 - WFOR(IP)) * HY_CF_EFD(SEG,GRP,IRG)   ! Use results from 864 Model to set Pondage Hydro Generation
            ELSE
               CF = (1.0 - WFOR(IP))  !can operate up to (1-for) in any given slice
            END IF
            FSL = EFD_Slice_ID(GRP,SEG)
            FSLCD = CHAR(64+FSL)
            COL = 'H'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_mask,':H:',NUM,CHCOD(IS),FSLCD)
            SOLVAL = 0.0
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
            IF (IECP .EQ. WIHY .AND. CURIYR + UHBSYR .GE. UPSTYR) THEN
               GEN = CF * ELWDTH(IVSL,IS) * SOLVAL(1) * SHR_C

               IF (FCRL .EQ. 1 .AND. CURIYR+1989 .EQ. 2020) &
                  WRITE(18,5263) CURIRUN, CURIYR+1989, CURITR, IRG, I_EFD_GRP, IGRP, IP, IECP, IS, SEG, IVSL, COL, &
                  UP_CAP(IS,IGRP), UG_CAP(IS,I_EFD_GRP), SHR_C, SOLVAL(1), WFOR(IP), HY_CF_EFD(SEG,GRP,IRG), ELWDTH(IVSL,IS), CF, ELWDTH(IVSL,IS) * CF
 5263          FORMAT(1X,"EFD_HY_GEN_OUT",11(",",I5),",",A16,9(",",F21.6))

            ELSE
               GEN = CF * ELWDTH(IVSL,IS) * SOLVAL(1) * SHR_C
            END IF
            F_GEN = F_GEN + CF * ELWDTH(IVSL,IS)
            GENSR = (1.0 - CF) * SR_CREDIT(IECP) * SOLVAL(1) * SHR_C

            GEN_MIN = 0.0
            GEN_MINSR = 0.0
            SOLVAL_MIN = 0.0
            IF (SR_CREDIT(IECP) .GT. 0.0 .AND. (IECP .NE. WIHY .OR. CURIYR + UHBSYR .LT. UPSTYR)) THEN
               CF_MIN = SR_MIN_CF(IECP)
               COL_MIN = 'F'//NUM//CHCOD(IS)//FSLCD; call makmsk(COL_MIN_mask,':F:',NUM,CHCOD(IS),FSLCD)
               CALL DWFSCOL(COL_MIN,COLSOL,STAT_MIN,SOLVAL_MIN,COL_MIN_mask,IRET_MIN)
               GEN_MIN = CF_MIN * ELWDTH(IVSL,IS) * SOLVAL_MIN(1) * SHR_C
               F_GEN_MIN = F_GEN_MIN + CF_MIN * ELWDTH(IVSL,IS)
               GEN_MINSR = (1.0 - CF_MIN) * SR_CREDIT(IECP) * SOLVAL_MIN(1) * SHR_C
            END IF

            IF (IP .EQ. UIHYR) THEN   !pumped storage
               IF (IECP .EQ. WIP2) THEN
                  PSFAC = UFACP2
               ELSE
                  PSFAC = UFACPS
               ENDIF
               PSUSE = PSFAC * (GEN + GEN_MIN)
               S_GEN = S_GEN + GEN - PSFAC * GEN
               S_GEN_MIN = S_GEN_MIN + GEN_MIN - PSFAC * GEN_MIN
            ELSE
               PSUSE = 0.0
               S_GEN = S_GEN + GEN
               S_GEN_MIN = S_GEN_MIN + GEN_MIN
            ENDIF
            ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN + GEN_MIN - PSUSE
            ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN + GEN_MIN - PSUSE
            ULTGEN(IGRP) = ULTGEN(IGRP) + GEN + GEN_MIN - PSUSE
            SP_ACHBYECP(GRP,SEG,IRG,IECP) = SP_ACHBYECP(GRP,SEG,IRG,IECP) + GENSR + GEN_MINSR
            UCRNW(SEG,GRP,IRG) = UCRNW(SEG,GRP,IRG) + (SOLVAL(1) + SOLVAL_MIN(1)) * SHR_C
            UCRPS(SEG,GRP,IRG) = UCRPS(SEG,GRP,IRG) + (SOLVAL(1) + SOLVAL_MIN(1)) * UPRNWSHR(IECP) * SHR_C
            IF (IECP .EQ. WIP2) THEN
               UTHTP2(SEG,GRP,IRG) = UTHTP2(SEG,GRP,IRG) + (SOLVAL(1) * CF + SOLVAL_MIN(1) * CF_MIN) * SHR_C
            ENDIF

            LL = F_GEN / SHOURS

            Load_Level = MIN( LL , 1.0)
            ECPt = IECP
            FUEL_RGN = UG_FL_RG(I_EFD_GRP)
            FROM_LABEL = "HYO_" // COL

            CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)

            ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + S_GEN * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ

            IF (SR_CREDIT(IECP) .GT. 0.0) THEN
               LL_MIN = F_GEN_MIN / SHOURS

               Load_Level = MIN( LL_MIN , 1.0)
               ECPt = IECP
               FUEL_RGN = UG_FL_RG(I_EFD_GRP)
               FROM_LABEL = "HYO_" // COL_MIN

               CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ_MIN, Target_EFF, Max_EFF)

               ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + S_GEN_MIN * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ_MIN
            END IF

!           IF (CURIYR+1989 .LE. 2020 .AND. FCRL .EQ. 1) THEN
!              WRITE(6,3751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IVSL, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, COL, COL_MIN, "", &
!                 SOLVAL(1)*SHR_C, SOLVAL_MIN(1)*SHR_C, 0.0, &
!                 GEN, GEN_MIN, 0.0, &
!                 CF, CF_MIN, 0.0, CF, SR_CREDIT(IECP), &
!                 ULTGEN(IGRP), ECDSPE(IGRP,IS), UP_CAP(0,IGRP), MAXCF(IS), SHR_C, ELWDTH(IVSL,IS), UTHGHT(SEG,GRP,IRG), 0.0, 0.0
!3751          FORMAT(1X,"GRP_DATA_RTHYD",11(":",I5),3(":",A8),20(":",F20.6))
!           END IF

!           IF (FCRL .EQ. 1 .AND. CURIYR+UHBSYR .LE. 2017) THEN
!              WRITE(18,5751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, &
!                 SR_CREDIT(IECP), 1.0-UG_FOR(I_EFD_GRP), SHOURS, SHR_C, ECDSPE(IGRP,IS), ECDSPF(IGRP,IS), UP_CAP(IS,IGRP), UG_CAP(IS,I_EFD_GRP), ULHTRT_EFD(IGRP,IS), &
!                 COL, S_GEN, LL, HTRT_ADJ, COL_MIN, S_GEN_MIN, LL_MIN, HTRT_ADJ_MIN, " ", S_GEN_MAX, LL_MAX, HTRT_ADJ_MAX
!5751          FORMAT(1X,"ECDSPF_DATA_RTHYD ",10(":",I5),9(":",F21.6),3(":",A16,":",F21.6,":",F21.6,":",F21.6))
!           END IF

         ENDDO   !IVSL

      ENDDO    !SEASON

      RETURN
      END
!
!     This subroutine retrieves distributed generation plants
!

      SUBROUTINE RTDG(N,IRG,ITYP)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'plntctl'
      include'ecpcntl'
      include'elout'
      include'dispuse'
      include'ecp_nuc'

      INTEGER*4  N,IRG,ITYP,IS,IVSL,IP,IECP,IGRP,TFR,IGS,LOAD,JGRP,I_EFD_GRP
      INTEGER*4  NGSN,IFL,MODE,GRP,SEG,STP,INOX,IRET
      INTEGER*4  FUEL_RGN, ECPt
      REAL CF(EFD_D_MVS+1,EFD_D_MSP),VOM,HTRT,CAP(EFD_D_MSP),HRFAC,MAXCF(EFD_D_MSP),PTC
      REAL MRCAP,ADJ
      REAL*8 DRAT,VAL,GEN,OBJVAL,BTUREQ,NOXEM,SOLVAL(5),SHR_C,SHR_G
      REAL*8 S_GEN, S_GEN_MIN, S_GEN_MAX, F_GEN, F_GEN_MIN, F_GEN_MAX, SHOURS, LL, LL_MIN, LL_MAX
      REAL*8 Load_Level, HTRT_ADJ, HTRT_ADJ_MIN, HTRT_ADJ_MAX, Target_EFF, Max_EFF
      CHARACTER*1 FSLCD,NOXCODE
      CHARACTER*5 NUM
      CHARACTER*16 COL
      CHARACTER*12 FROM_LABEL
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*2 STAT

      efdsub='RTDG'

      IGRP = EDDBID(N)
      I_EFD_GRP = MAP_EDNTP_EFD_GRPS(N,IRG)
      JGRP = EFD_GRPS_F(I_EFD_GRP)
      SHR_G = UP_GEN(0,IGRP) / UG_GEN(0,I_EFD_GRP)

      WRITE(NUM,'(I5.5)') JGRP

!     calculate CF at each load slice

      DO IS = 1, EENSP
         DO IVSL = 1, EFD_D_MVS+1
            CF(IVSL,IS) = 0.0
         ENDDO
      ENDDO

      DO IS = 1, EENSP
         DO IVSL = 2 , ELNVCT(IS) + 1
            hrfac = ELWDTH(IVSL-1,IS)/EETIME(IS)
            CF(IVSL,IS) = CF(IVSL-1,IS) + hrfac
         ENDDO
      ENDDO

      IP = UG_EFDt(I_EFD_GRP)
      IECP = UG_ECPt(I_EFD_GRP)
      TFR = UG_FL_RG(I_EFD_GRP)

      DO IS = 1, EENSP
         CAP(IS) = EDCAP(N,IS)
         MAXCF(IS) = EDMXCP(N)*EFACTR
      ENDDO

      WRITE(NUM,'(I5.5)') IGRP

!     loop over season and slices to determine correct mode

      DO IS = 1, EENSP

         IF (UG_CAP(IS,I_EFD_GRP) .GT. 0.0) THEN
            SHR_C = UP_CAP(IS,IGRP) / UG_CAP(IS,I_EFD_GRP)
         ELSE
            SHR_C = UP_CAP(0,IGRP) / UG_CAP(0,I_EFD_GRP)
         END IF

         SHOURS = 0.0
         DO MODE = 1 , ELNVCT(IS)
            SHOURS = SHOURS + DBLE(ELWDTH(MODE,IS))
            IF (WUPPER(IP) .GE. CF(MODE,IS) .AND. WLOWER(IP) .LE. CF(MODE+1,IS)) THEN
               LOAD = MODE
            ENDIF
         ENDDO
         COL = 'D'//NUM//CHCOD(IS)//CHCOD(LOAD); call makmsk(COL_mask,':D:',NUM,CHCOD(IS),CHCOD(LOAD))
         CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
         IF (SOLVAL(1) .GE. 0.0) THEN

!           retrieve results

            S_GEN = 0.0
            F_GEN = 0.0
            LL = 1.0
            HTRT_ADJ = 1.0
            S_GEN_MIN = 0.0
            F_GEN_MIN = 0.0
            LL_MIN = 1.0
            HTRT_ADJ_MIN = 1.0
            S_GEN_MAX = 0.0
            F_GEN_MAX = 0.0
            LL_MAX = 1.0
            HTRT_ADJ_MAX = 1.0

            DO IVSL = 1, LOAD
               DRAT = 1.0
               IF (IVSL .EQ. LOAD) THEN
                  ADJ = (WUPPER(IP) - CF(IVSL,IS)) / (CF(IVSL+1,IS) - CF(IVSL,IS))
               ELSE
                  ADJ = 1.0
               ENDIF
               GEN = ADJ * DRAT * ELWDTH(IVSL,IS) * SOLVAL(1) * SHR_C
               S_GEN = S_GEN + GEN
               F_GEN = F_GEN + ADJ * DRAT * ELWDTH(IVSL,IS)
               ELGENE(IVSL,IS,IGRP) = ELGENE(IVSL,IS,IGRP) + GEN
               ECDSPE(IGRP,IS) = ECDSPE(IGRP,IS) + GEN
               ULTGEN(IGRP) = ULTGEN(IGRP) + GEN
            ENDDO ! IVSL

            LL = F_GEN / SHOURS

            Load_Level = MIN( LL , 1.0)
            ECPt = IECP
            FUEL_RGN = UG_FL_RG(I_EFD_GRP)
            FROM_LABEL = "DGO_" // COL

            CALL HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)

            ECDSPF(IGRP,IS) = ECDSPF(IGRP,IS) + S_GEN * ULHTRT_EFD(IGRP,IS) * 0.001 * HTRT_ADJ

!           IF (FCRL .EQ. 1 .AND. CURIYR+UHBSYR .LE. 2017) THEN
!              WRITE(18,5751) CURIRUN, CURIYR+UHBSYR, CURITR, IS, IRG, IGRP, N, I_EFD_GRP, IECP, ITYP, &
!                 SR_CREDIT(IECP), 1.0-UG_FOR(I_EFD_GRP), SHOURS, SHR_C, ECDSPE(IGRP,IS), ECDSPF(IGRP,IS), UP_CAP(IS,IGRP), UG_CAP(IS,I_EFD_GRP), ULHTRT_EFD(IGRP,IS), &
!                 COL, S_GEN, LL, HTRT_ADJ, " ", S_GEN_MIN, LL_MIN, HTRT_ADJ_MIN, " ", S_GEN_MAX, LL_MAX, HTRT_ADJ_MAX
!5751          FORMAT(1X,"ECDSPF_DATA_RTDG  ",10(":",I5),9(":",F21.6),3(":",A16,":",F21.6,":",F21.6,":",F21.6))
!           END IF

         ENDIF
      ENDDO    !SEASON

      RETURN
      END
!
!     This subroutine retrieves the fuel shares for consumption
!     Also retrieves NOX allowance price
!
      SUBROUTINE EDO$BTU
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'cdsparms'
      include'control'
      include'fuelin'
      include'ecpcntl'
      include'bildin'
      include'plntctl'
      include'angtdm'
      include'uso2grp'
      include'emission'
      include'uefdout'
      include'coalemm'
      include'ab32'
      include'dispin'
      include'ecp_nuc'
      include'emm_aimms'
!moved 3 lower common blocks to emm_aimms
!      COMMON /CAREMM/ CARSEC,CAROTH,ECPLIM,ECPCAR,ECPTAX,EFDLIM,EFDCAR,EFDTAX
!      REAL*4 CARSEC(MNUMYR)         ! Carbon Emissions -- Non Utility
!      REAL*4 CAROTH(MNUMYR)       ! Carbon Emissions -- Utility Geo/MSW
!      REAL*4 ECPLIM(MNUMYR)         ! ECP Carbon Limit
!      REAL*4 ECPCAR(MNUMYR)         ! ECP Carbon Emissions
!      REAL*4 ECPTAX(MNUMYR)         ! ECP Carbon Tax
!      REAL*4 EFDLIM(MNUMYR)         ! EFD Carbon Limit
!      REAL*4 EFDCAR(MNUMYR)         ! EFD Carbon Emissions
!      REAL*4 EFDTAX(MNUMYR)         ! EFD Carbon Tax
!
!      COMMON /AB32OUT/ AB32UTIL,AB32BAVL,AB32BUSE,AB32BBNK,AB32OAVL,AB32OUSE,  &
!                       AB32RAVL,AB32RUSE,AB32RBNK,AB32ESCP
!      REAL*4 AB32UTIL                       ! Utility Covered Emissions
!      REAL*4 AB32BAVL                       ! Banked Allowances Available 
!      REAL*4 AB32BUSE                       ! Banked Allowances Used 
!      REAL*4 AB32BBNK                       ! Banked Allowances Added
!      REAL*4 AB32OAVL                       ! Offsets Available
!      REAL*4 AB32OUSE                       ! Offsets Used 
!      REAL*4 AB32RAVL                       ! Reserves Available
!      REAL*4 AB32RUSE                       ! Reserves Used 
!      REAL*4 AB32RBNK                       ! Reserves Banked
!      REAL*4 AB32ESCP                       ! Escape Vector Value (Shortage)
!
!      COMMON /RGGIOUT/ RGGIUTIL,RGGIBAVL,RGGIBUSE,RGGIBBNK,RGGIOAVL,RGGIOUSE,  &
!                       RGGIRAVL,RGGIRUSE,RGGIRBNK,RGGIEAVL,RGGIEUSE,RGGIESCP
!      REAL*4 RGGIUTIL                       ! Utility Covered Emissions
!      REAL*4 RGGIBAVL                       ! Banked Allowances Available 
!      REAL*4 RGGIBUSE                       ! Banked Allowances Used 
!      REAL*4 RGGIBBNK                       ! Banked Allowances Added
!      REAL*4 RGGIOAVL                       ! Offsets Available
!      REAL*4 RGGIOUSE                       ! Offsets Used 
!      REAL*4 RGGIRAVL                       ! Cost Containment Reserves Available
!      REAL*4 RGGIRUSE                       ! Cost Containment Used 
!      REAL*4 RGGIRBNK                       ! Cost Containment Banked
!      REAL*4 RGGIEAVL                       ! Emissions Containment Reserves Available
!      REAL*4 RGGIEUSE                       ! Emissions Containment Reserves Used 
!      REAL*4 RGGIESCP                       ! Escape Vector Value (Shortage)

      INTEGER FRG, IPLT, IECP, IMD, ICOF,CFLV,IEFD,IGS,IRS,IDS,IOL,CRG,GRG,ORG
      INTEGER IFL,IWD,NGSN,FULLYR,RCF,INOX,SO2,HG,CAR,CRV,RANK,IS
      INTEGER*4 IRET,CTLPLT
      REAL GASSH,OLSH,OILSH,TOT,CLSH,FLCST,CFSH,VAL,CFCAP,AVGGAS,AVGOIL,AVGCL,AVGWD,TOTSH
      REAL CTLNXRT,SO2RMV,COEFF
      REAL*8 SOLVAL(5),prc
      CHARACTER*16 COL,ROW_NOX,ROW_SO2,ROW_HG,ROW_CAR
      CHARACTER*1 NOXCODE,SO2CODE
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*8 ROWSOL /'ASLUP   '/
      CHARACTER*2 STAT,SC
!
      COMMON /TOT_RATES/ TOT_RATE1,TOT_RATE2,TOT_TYPE
      REAL*8 TOT_RATE1(MX_NCOALS+MX_ISCV,NDREG,0:ECP_D_FPH),TOT_RATE2(MX_NCOALS+MX_ISCV,NDREG,0:ECP_D_FPH)
      INTEGER*4 TOT_TYPE(MX_NCOALS+MX_ISCV)

      efdsub='EDO$BTU'

      FULLYR = USYEAR(CURIYR)
      CALL COMBINE_COAL_RATES(CURIYR)

!  retrieve NOX dual
      DO INOX = 1, NOX_GRP
        WRITE(NOXCODE,'(I1)') INOX
        ROW_NOX = 'ELNOX0' // NOXCODE; call makmsk(ROW_NOX_mask,':ELNOX0:' , NOXCODE) 
        IF (EMRFNA(INOX,CURIYR) .GT. 0.0) THEN
          CALL DWFSROW(ROW_NOX,ROWSOL,STAT,SOLVAL,ROW_NOX_mask,IRET)
          prc = SOLVAL(5) * 1000.0
          write(UF_DBG,1026) CURIYR,CURITR,ROW_NOX,INOX,SOLVAL(1),SOLVAL(2),SOLVAL(4),prc
1026      format(1x,'EFD NOX',2(':',I4),':',A10,':',I4,4(':',F15.6))
        ENDIF
      END DO
! SO2
      DO SO2 = 1 , NUM_SO2_GRP
        WRITE(SO2CODE,'(I1)') SO2
        ROW_SO2 = 'SULFUR' // SO2CODE; call makmsk(ROW_SO2_mask,':SULFUR:' , SO2CODE)
        IF (EMRFSA(CURIYR,SO2) .GT. 0.0) THEN
        CALL DWFSROW(ROW_SO2,ROWSOL,STAT,SOLVAL,ROW_SO2_mask,IRET)
        prc = SOLVAL(5) * 1000.0
       write(UF_DBG,1027) CURIYR,CURITR,ROW_SO2,SO2,SOLVAL(1),SOLVAL(2),SOLVAL(4),prc
1027   format(1x,'EFD SO2',2(':',I4),':',A10,':',I4,2(':',F15.6),':',F18.6,':',F12.6)
        ENDIF
      END DO
! HG
      DO HG = 1 , NUM_HG_GRP
        ROW_HG = 'MERCURY' // UPRGCD(HG); call makmsk(ROW_HG_mask,':MERCURY:' , UPRGCD(HG))
        IF (EMEL_QHG(HG,CURIYR) .LT. 100.0)THEN
         CALL DWFSROW(ROW_HG,ROWSOL,STAT,SOLVAL,ROW_HG_mask,IRET)
         prc = SOLVAL(5) * 1000.0
       write(UF_DBG,1028) CURIYR,CURITR,ROW_HG,HG,SOLVAL(1),SOLVAL(2),SOLVAL(4),prc
1028   format(1x,'EFD HG ',2(':',I4),':',A10,':',I4,4(':',F15.6))
        END IF
      END DO

      ROW_CAR = 'CARBONXX';ROW_CAR_MASK='CARBONXX'
      IF ((USW_CAR .EQ. 2 .OR. USW_CAR .EQ. 3) .AND. (CURIYR+UHBSYR) .GE. UYR_CAR) THEN
          CALL DWFSROW(ROW_CAR,ROWSOL,STAT,SOLVAL,ROW_CAR_mask,IRET)
          prc = SOLVAL(5)
        write(UF_DBG,1029) CURIYR,CURITR,ROW_CAR,SOLVAL(1),SOLVAL(2),SOLVAL(4),prc
1029   format(1x,'EFD CAR',2(':',I4),':',A10,4(':',F15.6))
          EFD_LCAR(CURIYR) = solval(4)
          EFD_QCAR(CURIYR) = solval(1)
          EFD_PCAR(CURIYR) = prc
      ENDIF

!     Regional Carbon Constraints, if any
      DO CAR = 1 , CO2_GRP
         UCARPRC(CAR,CURIYR) = 0.0
         CARTGT(CAR,CURIYR) = 9999.9 * 12.0 / 44.0
         CAREFD(CAR,CURIYR) =  0.0
         IF (CAR .EQ. CARGRP_CA)THEN
            AB32UTIL = 0.0
            AB32BAVL = 0.0
            AB32BUSE = 0.0
            AB32BBNK = 0.0
            AB32OAVL = 0.0
            AB32OUSE = 0.0
            AB32RAVL = 0.0
            AB32RUSE = 0.0
            AB32RBNK = 0.0
            AB32ESCP = 0.0
          IF (AB_CAP_TOT(CURIYR) .GT. 0.0 .AND. AB_CAP_TOT(CURIYR) .LT. 9000.0)THEN
            CARTGT(CAR,CURIYR) = AB_CAP_TOT(CURIYR)
            ROW_CAR = 'CARBONAB' ; ROW_CAR_mask='CARBONAB' 
!           Covered Emissions Cap and Unused (Banked) Allowances
            COL = 'CARLIM' // CO2_RG(CAR); call makmsk(COL_mask,':CARLIM:',CO2_RG(CAR))
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
            IF (IRET .EQ. 0)THEN
               CAREFD(CAR,CURIYR) = SOLVAL(1)
               AB32BBNK = SOLVAL(4) - SOLVAL(1)
            END IF
!           Covered Utility Emissions and Banks Used
            COL = 'CARUTL' // CO2_RG(CAR); call makmsk(COL_mask,':CARUTL:',CO2_RG(CAR))
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
            IF (IRET .EQ. 0)THEN
               AB32UTIL = SOLVAL(1)
            END IF
!           Offsets
            COL = 'CARBNK' // CO2_RG(CAR); call makmsk(COL_mask,':CARBNK:',CO2_RG(CAR))
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
            IF (IRET .EQ. 0)THEN
               AB32BAVL = SOLVAL(4)
               AB32BUSE = SOLVAL(1)
            END IF
!     write(6,2345) curiyr+1989,curitr,ab32bavl,ab32buse,ab32bbnk
!2345 format(1h ,'!ab32efd',i4,i4,3f10.3)
!           Offsets
            COL = 'CAROFF' // CO2_RG(CAR); call makmsk(COL_mask,':CAROFF:',CO2_RG(CAR)) 
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
            IF (IRET .EQ. 0)THEN
               AB32OAVL = SOLVAL(4)
               AB32OUSE = SOLVAL(1)
            END IF
!           Reserves
!           DO IS = 1 , 3
!              COL = 'CARRS' // CHCOD(IS) // CO2_RG(CAR)
               COL = 'CARRSV' // CO2_RG(CAR); call makmsk(COL_mask,':CARRSV:',CO2_RG(CAR))
               CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
               IF (IRET .EQ. 0)THEN
!                 AB32RAVL(IS) = SOLVAL(4)
!                 AB32RUSE(IS) = SOLVAL(1)
!                 AB32RBNK(IS) = AB32RAVL(IS) - AB32RUSE(IS)
                  AB32RAVL = SOLVAL(4)
                  AB32RUSE = SOLVAL(1)
                  AB32RBNK = AB32RAVL - AB32RUSE
               END IF
!           END DO
!           Escape (Shortage)
            COL = 'CARESC' // CO2_RG(CAR); call makmsk(COL_mask,':CARESC:',CO2_RG(CAR))
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
            IF (IRET .EQ. 0)THEN
               AB32ESCP = SOLVAL(1)
            END IF
          END IF
         ELSE
            ROW_CAR = 'CARBON' // CO2_RG(CAR); call makmsk(ROW_CAR_mask,':CARBON:' , CO2_RG(CAR))
            IF (CAR .EQ. CARGRP_RG)THEN
!              Offsets
               COL = 'CAROFF' // CO2_RG(CAR); call makmsk(COL_mask,':CAROFF:', CO2_RG(CAR))
               CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
               IF (IRET .EQ. 0)THEN
                  RGGIOAVL = SOLVAL(4)
                  RGGIOUSE = SOLVAL(1)
               END IF
!              Reserves (Cost Containment)
               COL = 'CARRSV' // CO2_RG(CAR); call makmsk(COL_mask,':CARRSV:',CO2_RG(CAR))
               CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
               IF (IRET .EQ. 0)THEN
                  RGGIRAVL = SOLVAL(4)
                  RGGIRUSE = SOLVAL(1)
!                 RGGIRBNK = AB32RAVL - AB32RUSE
               END IF
!              Reserves (Emissions Containment)
               COL = 'CARECR' // CO2_RG(CAR); call makmsk(COL_mask,':CARRSV:',CO2_RG(CAR))
               CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
               IF (IRET .EQ. 0)THEN
                  RGGIEAVL = SOLVAL(4)
                  RGGIEUSE = SOLVAL(1)
               END IF
            END IF
         END IF
         CALL DWFSROW(ROW_CAR,ROWSOL,STAT,SOLVAL,ROW_CAR_mask,IRET)
         IF (IRET .EQ. 0)THEN
            prc = SOLVAL(5)
            UCARPRC(CAR,CURIYR) = prc
            IF (CAR .NE. CARGRP_CA)THEN
               IF (SOLVAL(4) .GT. DBLE(0.0))THEN
                  CARTGT(CAR,CURIYR) = SOLVAL(4)
                  CAREFD(CAR,CURIYR) = SOLVAL(1)
               ELSE
                  COL = 'CARLIM' // CO2_RG(CAR); call makmsk(COL_mask,':CARLIM:',CO2_RG(CAR))
                  CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
                  IF (IRET .EQ. 0)THEN
                     CARTGT(CAR,CURIYR) = SOLVAL(4)
                     CAREFD(CAR,CURIYR) = SOLVAL(1)
                     IF (CAR .EQ. CARGRP_RG)THEN
                        RGGIUTIL = SOLVAL(1)
                        RGGIUTIL = RGGIUTIL + RGGIOUSE + RGGIRUSE
                        CAREFD(CAR,CURIYR) = CAREFD(CAR,CURIYR) + RGGIOUSE + RGGIRUSE
!                       RGGIBBNK = SOLVAL(4) - SOLVAL(1)
                     END IF
                  END IF
               END IF
            END IF
         END IF
!     write(6,2323) curiyr+1989,row_car,iret,solval(1),solval(2),solval(3),solval(4),solval(5),ucarprc(car,curiyr)
!2323 format(1h ,'!carefd',i4,a10,i3,6f10.2)
      END DO
!     IF ((CURIYR + UHBSYR) .EQ. UESTYR .AND. CURITR .EQ. 1)write(13,3455)
!3455 format(1h ,'!ab32efd',T10,'YEAR',T15,'ITER',T21,'  EMIS LMT',T31,'   UTL EMS',T41,'   BNK AVL',T51,'   BNK USE',  &
!                                              T61,'  OFFS AVL',T71,'  OFFS USE',T81,'   RSV AVL',T91,'   RSV USE',  &
!                                             T101,'   ESC QTY',T111,'  ALLW PRC')
!     IF (AB_CAP_TOT(CURIYR) .GT. 0.0 .AND. AB_CAP_TOT(CURIYR) .LT. 9000.0)THEN
!     write(13,3456) curiyr+1989,curitr,ab_cap_tot(curiyr)*(1.0 - ab_cstcont_frac(curiyr))*44.0/12.0,  &
!                    ab32util*44.0/12.0,ab32bavl*44.0/12.0,ab32buse*44.0/12.0,  &
!                    ab32oavl*44.0/12.0,ab32ouse*44.0/12.0,ab32ravl*44.0/12.0,ab32ruse*44.0/12.0,ab32escp*44.0/12.0,  &
!                    ucarprc(cargrp_ca,curiyr)*(12.0/44.0)*scalpr
!3456 format(1h ,'!ab32efd',t10,i4,t15,i4,t21,10f10.1)
!     END IF
!           write(13,2348) curiyr+1989,curitr,carclt,carogt
!2348 format(1h ,'!carstcl/og',i4,i3,'   US',2f10.3)
!     end if

!   fill in CTL emissions for NOX, SO2 and mercury

      DO CRG = 1, NDREG
         CTLSO2EM(CRG,CURIYR) = 0.0
         CTLNOXEM(CRG,CURIYR) = 0.0
         CTLHGEM(CRG,CURIYR) = 0.0
      ENDDO

      DO CRV = 1, MX_NCOALS        ! + MX_ISCV
         IF (XCL_TYPE(CRV) .GT. 0)THEN
            WRITE(SC,'(I2.2)') CRV
            RANK = EFD_RANK(CRV)
            DO CRG = 1, NDREG
               IF (TOT_RATE1(CRV,CRG,0) .LT. 800.0) THEN
                  DO SO2 = 1 , NUM_SO2_GRP
                     IF (SO2_SHR_BY_CLRG(CRG,SO2) .GT. 0.0)THEN
                        WRITE(SO2CODE,'(I1)') SO2
                        COL = 'CTL' // SC // EPFLCD(CRG) // 'S' // SO2CODE; call makmsk(COL_mask,':CTL:' , SC , EPFLCD(CRG) , ':S:' , SO2CODE)
                        CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
                        IF (IRET .EQ. 0) CTLSO2EM(CRG,CURIYR) = CTLSO2EM(CRG,CURIYR) + SOLVAL(1) * 1000.0  !units tons
                     END IF
                  END DO

                  COL = 'CTL' // SC // EPFLCD(CRG) // 'HG'; call makmsk(COL_mask,':CTL:' , SC , EPFLCD(CRG) , ':HG:')
                  CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
                  IF (IRET .EQ. 0) CTLHGEM(CRG,CURIYR) = CTLHGEM(CRG,CURIYR) + SOLVAL(1) * 0.001  !units tons
               ENDIF
            END DO
         ENDIF
      END DO


      DO CRG = 1, NDREG
         DO IS = 1, EENSP
          DO INOX = 1, NOX_GRP
           IF (NOX_RG(INOX) .NE. 'SP') THEN
            WRITE(NOXCODE,'(I1)') INOX
            COL = 'B'//CHCOD(CRG)//'CTL'//CHCOD(IS)//'N'//NOXCODE; call makmsk(COL_mask,':B:',CHCOD(CRG),':CTL:',CHCOD(IS),':N:',NOXCODE)
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
            CTLPLT = WIIG   ! IGCC characteristics
            IF (IRET .EQ. 0) CTLNOXEM(CRG,CURIYR) = CTLNOXEM(CRG,CURIYR) + SOLVAL(1) * 1000.0  !units tons
           ENDIF
          ENDDO
         ENDDO
         write(UF_DBG,918) CRG,CURIYR,CURITR,CTLNOXEM(CRG,CURIYR),CTLHGEM(CRG,CURIYR),CTLSO2EM(CRG,CURIYR)
      ENDDO
918   FORMAT(1x,'CTL EMISSIONS',3I5,3F15.4)


      DO FRG = 1, UNFRGN

!        set up BTU columns by plant type and fuel options

         CRG = EPCLMP(FRG)
         GRG = EPGSMP(FRG)
         ORG = EPCSMP(FRG)

!        coal

         DO IECP = 1, UIIS
            IF (CPFLECP(IECP,ORG,CRG,GRG) .GT. 0.0) THEN
               AVGCL = 0.0
               AVGOIL = 0.0
               AVGWD = 0.0
               TOT = 0.0

!              loop over cofiring retrofit categories

               IF (FULLYR .GE. UPSTYR .AND. USW_ECPCF .NE. 1) THEN  ! cofiring variables only filled in after ECP runs
                  RCF = ECP_D_RCF
               ELSE
                  RCF = 1
               ENDIF
               DO ICOF = 1, RCF

!                 no cofiring option:

                  IMD = 1

                  COL = 'B'//EPFLCD(FRG)//UPLNTCD(IECP)//'XX'//CHCOD(ICOF)//CHCOD(IMD); COL_mask='B(*)(***)(*)(**)'
                  CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
                  OLSH = EDMXDS(1,IECP,FRG)
                  GASSH = EDMXGS(1,IECP,FRG)

                  IF (USW_ECPCF .EQ. 0) THEN   !no cofiring option
                     CLSH = 1.0 - OLSH - GASSH
                     AVGCL = AVGCL + CLSH * SOLVAL(1)
                     AVGOIL = AVGOIL + OLSH * SOLVAL(1)
                     TOT = TOT + SOLVAL(1)
                  ELSE    !fixed cofiring option, use ECP shares
                     CFSH = UPWDCFR(IECP,CRG)
                     IF (OLSH + CFSH .GT. 1.0) THEN

                        write(UF_DBG,*) ' cofire oil share > 1 ',FRG,IECP,GASSH,OLSH,CFSH

                        TOTSH = OLSH + CFSH
                        OLSH = OLSH * 1.0 / TOTSH
                        CFSH = CFSH * 1.0 / TOTSH
                     ENDIF
                     CLSH = 1.0 - (OLSH + CFSH)
                     AVGCL = AVGCL + CLSH * SOLVAL(1)
                     AVGOIL = AVGOIL + OLSH * SOLVAL(1)
                     AVGWD = AVGWD + CFSH * SOLVAL(1)
                     TOT = TOT + SOLVAL(1)
                  ENDIF

!                 cofiring options if after first ECP year

                  IF (FULLYR .GE. UPSTYR .AND. USW_ECPCF .NE. 1) THEN
                     DO CFLV = 1, UPCFNSTP(ICOF)
                        IMD = CFLV + 1

                        COL = 'B'//EPFLCD(FRG)//UPLNTCD(IECP)//'XX'//CHCOD(ICOF)//CHCOD(IMD); COL_mask='B(*)(***)(*)(**)'

                        IF (UPCFLEV(ICOF,CFLV) .GT. 0.0 .AND. UPCFBTU(ICOF,CRG) .GT. 0.0) THEN
                           CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
                           CFSH = UPCFLEV(ICOF,CFLV)
                           IF (OLSH + CFSH .GT. 1.0) THEN

                              write(UF_DBG,*) ' cofire oil share > 1 in coal plant',FRG,IECP,GASSH,OLSH,CFSH

                              TOTSH = OLSH + CFSH
                              OLSH = OLSH * 1.0 / TOTSH
                              CFSH = CFSH * 1.0 / TOTSH
                           ENDIF
                           CLSH = 1.0 - (GASSH + OLSH + CFSH)
                           AVGCL = AVGCL + CLSH * SOLVAL(1)
                           AVGOIL = AVGOIL + OLSH * SOLVAL(1)
                           AVGWD = AVGWD + CFSH * SOLVAL(1)
                           TOT = TOT + SOLVAL(1)
                        ENDIF
                     ENDDO    !cflv
                  ENDIF   ! upstyr
               ENDDO     !icof
               IF (TOT .GT. 0.0) THEN
                  FCLSH(1,IECP,FRG) = AVGCL / TOT
                  FOLSH(1,IECP,FRG) = AVGOIL / TOT
                  FWDSH(1,IECP,FRG) = AVGWD / TOT
               ELSE
                  FCLSH(1,IECP,FRG) = 1.0
                  FOLSH(1,IECP,FRG) = 0.0
                  FWDSH(1,IECP,FRG) = 0.0
               ENDIF

               write(UF_DBG,326) CURIYR,CURITR,IECP,FRG,FCLSH(1,IECP,FRG),FOLSH(1,IECP,FRG),FWDSH(1,IECP,FRG),TOT
  326          format(1x,'EDO_BTU_coal',4(":",I4),3(":",F10.6),":",F12.2)

            ENDIF
         ENDDO    ! iecp

!  loop over other gas/oil types - by efd type
      DO IEFD = UICAS + 1, EFD_D_CAP
       IF (CPFLEFD(IEFD,ORG,CRG,GRG) .GT. 0.0) THEN
        IGS = 0
        IRS = 0
        IDS = 0
        IWD = 0

        AVGCL = 0.0
        AVGOIL = 0.0
        AVGWD = 0.0
        AVGGAS = 0.0
        TOT = 0.0

        DO IFL = 1, EFD_D_FPP
         if (WFLTP(IEFD,IFL) .NE. 0) THEN
          IF (UIGAS(WFLTP(IEFD,IFL)) .EQ. 1) IGS = 1
          IF (UIRES(WFLTP(IEFD,IFL)) .EQ. 1) IRS = 1
          IF (UIDIS(WFLTP(IEFD,IFL)) .EQ. 1) IDS = 1
          IF (WFLTP(IEFD,IFL) .EQ. UIWD) IWD = 1
         endif
        ENDDO

        IF (IGS .EQ. 1 .AND. (IRS .EQ. 0 .AND. IDS .EQ. 0)) THEN   ! gas only
          IMD = 1
          GASSH = 1.0
          FGSSH(2,IEFD,FRG) = 1.0
          FCLSH(2,IEFD,FRG) = 0.0
          FOLSH(2,IEFD,FRG) = 0.0
          FWDSH(2,IEFD,FRG) = 0.0
           write(UF_DBG,327) CURIYR,CURITR,IEFD,FRG,FGSSH(2,IEFD,FRG),FOLSH(2,IEFD,FRG),TOT

         ELSEIF (IGS .EQ. 0 .AND. (IRS .EQ. 1 .OR. IDS .EQ. 1)) THEN ! oil only
          IMD = 1
          OILSH = 1.0
          FGSSH(2,IEFD,FRG) = 0.0
          FCLSH(2,IEFD,FRG) = 0.0
          FOLSH(2,IEFD,FRG) = 1.0
          FWDSH(2,IEFD,FRG) = 0.0
           write(UF_DBG,327) CURIYR,CURITR,IEFD,FRG,FGSSH(2,IEFD,FRG),FOLSH(2,IEFD,FRG),TOT

         ELSEIF (IGS .EQ. 1 .AND. (IRS .EQ. 1 .OR. IDS .EQ. 1)) THEN  !dual fired
           IMD = 1   ! max gas, min oil
           GASSH = MIN(1.0,EDMXGS(2,IEFD,FRG))
           OILSH = 1.0 - GASSH
           IF (IRS) IOL=UIRL
           IF (IDS) IOL=UIDS
            DO NGSN = 1, 3
             COL = 'B'//EPFLCD(FRG)//EPPLCD(IEFD)//CHCOD(NGSN)//CHCOD(IMD)//'X'; COL_mask='B(*)(***)(*)(**)'
             CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
             AVGGAS = AVGGAS + GASSH * SOLVAL(1)
             AVGOIL = AVGOIL + OILSH * SOLVAL(1)
             TOT = TOT + SOLVAL(1)
            ENDDO

           IMD = 2   ! max oil, min gas
           IF (IRS) THEN
             OILSH = EDMXRS(2,IEFD,FRG)
             IOL = UIRL
           ELSE
             OILSH = EDMXDS(2,IEFD,FRG)
             IOL = UIDS
           ENDIF
           IF (OILSH .GT. 1.0) OILSH = 1.0
           GASSH = 1.0 - OILSH

            DO NGSN = 1, 3
             COL = 'B'//EPFLCD(FRG)//EPPLCD(IEFD)//CHCOD(NGSN)//CHCOD(IMD)//'X'; COL_mask='B(*)(***)(*)(**)'
             CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
             AVGGAS = AVGGAS + GASSH * SOLVAL(1)
             AVGOIL = AVGOIL + OILSH * SOLVAL(1)
             TOT = TOT + SOLVAL(1)
            ENDDO
           IF (TOT .GT. 0.0) THEN
            FCLSH(2,IEFD,FRG) = 0.0
            FWDSH(2,IEFD,FRG) = 0.0
            FGSSH(2,IEFD,FRG) = AVGGAS / TOT
            FOLSH(2,IEFD,FRG) = AVGOIL / TOT
           ELSE
            FCLSH(2,IEFD,FRG) = 0.0
            FWDSH(2,IEFD,FRG) = 0.0
            FGSSH(2,IEFD,FRG) = 1.0
            FOLSH(2,IEFD,FRG) = 0.0
           ENDIF
           write(UF_DBG,327) CURIYR,CURITR,IEFD,FRG,FGSSH(2,IEFD,FRG),FOLSH(2,IEFD,FRG),TOT
327    format(1x,'EDO$BTU oil/gas',4I4,2F10.6,F12.2)

         ELSEIF (IWD .EQ. 1) THEN   ! biomass
             IMD = 1
             FWDSH(2,IEFD,FRG) = 1.0
             FCLSH(2,IEFD,FRG) = 0.0
             FGSSH(2,IEFD,FRG) = 0.0
             FOLSH(2,IEFD,FRG) = 0.0
         ENDIF

        ENDIF
       ENDDO  ! IEFD

      ENDDO   ! FRG

      RETURN
      END
!
!     EDO$CPP
!
      SUBROUTINE EDO$CPP
      use efd_row_col

      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'eusprc'
      include'edbdef'
      include'e111d'
      include'uefdout'

!     COMMON/ERCOUT/ERCQTYDN(MNUMNR,MNUMNR,MNUMYR),ERCPRCDN(MNUMNR,MNUMYR),  &
!                   ERCQTYPN(MNUMNR,MNUMNR,MNUMYR),ERCPRCPN(MNUMNR,MNUMYR)
!     REAL*4 ERCQTYDN
!     REAL*4 ERCPRCDN
!     REAL*4 ERCQTYPN
!     REAL*4 ERCPRCPN

      INTEGER IRET,IRG,REG
      REAL*8 SOLVAL(5)
      REAL*4 CO2_STDOC
      CHARACTER*16 ROW,COL
      CHARACTER*8 ROWSOL/'ASLUP   '/
      CHARACTER*8 COLSOL/'ACLUD   '/
      CHARACTER*2 STAT

      efdsub='EDO$CPP'

!     INITIALIZE
!     FUEL REGIONS
      DO IRG = 1 , EFD_D_MFRG + 1
         EGENFRQF(IRG,CURIYR) = 0.0
      END DO
!     EMM  REGIONS
      DO IRG = 1 , MNUMNR
         ECO2NRTL(IRG,CURIYR) = 0.0
         ECO2NRQF(IRG,CURIYR) = 0.0
         EGENNRQF(IRG,CURIYR) = 0.0
         ECO2NRPR(IRG,CURIYR) = 0.0
!        ERC OUTPUTS
         EERCNRPR(IRG,CURIYR) = 0.0
         EERCNRPM(IRG,CURIYR) = 0.0
         DO REG = 1 , MNUMNR
            EERCNRQR(IRG,REG,CURIYR) = 0.0
            EERCNRQM(IRG,REG,CURIYR) = 0.0
         END DO
      END DO
!     INITIALIZE NATIONAL TOTALS FOR AVERAGING INTENSITY STANDARD AND EMISSIONS PRICE
      ECO2NRRT(MNUMNR,CURIYR) = 0.0
!
      IF ((CURIYR + UHBSYR) .GE. (CO2_STDBY + 1))THEN
!        EMM  REGIONS
         DO IRG = 1 , UNRGNS
!           CO2 INTENSITY STANDARD ALLOWANCE PRICE
            CO2_STDOC = 0.0
            IF (CO2_STDSW .GT. 0)THEN
               IF (CO2_STDTN(IRG) .EQ. 1 .AND. CO2_STDRN(IRG,CURIYR) .GT. 0.0)THEN
                  ROW = 'CO2RNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CO2RNR:' , URGNME(IRG)(6:7))
                  CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
                  IF (IRET .EQ. 0)THEN
                     ECO2NRPR(IRG,CURIYR) = SOLVAL(5)
                     CO2_STDOC = SOLVAL(1)
                  END IF
               ELSE IF (CO2_STDTN(IRG) .EQ. 2 .AND. CO2_STDQN(IRG,CURIYR) .GT. 0.0)THEN
                  ROW = 'CO2QNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CO2QNR:' , URGNME(IRG)(6:7))
                  CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
                  IF (IRET .EQ. 0)THEN
                     ECO2NRPR(IRG,CURIYR) = SOLVAL(5) * 0.001
                  END IF
               END IF
            ELSE
               ECO2NRPR(IRG,CURIYR) = 0.0
            END IF
!           CO2 INTENSITY STANDARD AFFECTED PLANTS GENERATION
            ROW = 'GENQNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENQNR:' , URGNME(IRG)(6:7))
            CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
            IF (IRET .EQ. 0)THEN
               EGENNRQF(IRG,CURIYR) = SOLVAL(1)
               EGENNRQF(MNUMNR,CURIYR) = EGENNRQF(MNUMNR,CURIYR) + SOLVAL(1)
!              ACCUMULATE REGIONAL TOTALS FOR NATIONAL INTENSITY STANDARD AND ALLOWANCE PRICE
               IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(1,CURIYR) .GT. 0.0)THEN
                  ECO2NRRT(MNUMNR,CURIYR) = ECO2NRRT(MNUMNR,CURIYR) + (SOLVAL(1) + EGENNREE(IRG,CURIYR)) * ECO2NRRT(IRG,CURIYR)
                  ECO2NRPR(MNUMNR,CURIYR) = ECO2NRPR(MNUMNR,CURIYR) + (SOLVAL(1) + EGENNREE(IRG,CURIYR)) * ECO2NRPR(IRG,CURIYR)
               END IF
            END IF
!           CO2 INTENSITY STANDARD AFFECTED PLANTS EMISSIONS
            ROW = 'CO2QNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CO2QNR:' , URGNME(IRG)(6:7))
            CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
            IF (IRET .EQ. 0)THEN
               ECO2NRQF(IRG,CURIYR) = ECO2NRQF(IRG,CURIYR) + SOLVAL(1)
            END IF
            IF (CO2_STDSW .GT. 0 .AND. CO2_STDRN(1,CURIYR) .GT. 0.0)THEN
!           IF RATE BASED STANDARD, DETERMINE AFFECTED EMISSIONS USING RATE AND GENERATION
               IF (CO2_STDTN(IRG) .LT. 2)THEN
                  CO2_STDOC = CO2_STDOC / (EGENNRQF(IRG,CURIYR) + EGENNREE(IRG,CURIYR))
!                 if (curitr .gt. 1)write(6,3344) curiyr + 1989,irg,ECO2NRRT(IRG,CURIYR) , CO2_STDOC , EGENNRQF(IRG,CURIYR) , EGENNREE(IRG,CURIYR),  &
!                               (ECO2NRRT(IRG,CURIYR) + CO2_STDOC) * (EGENNRQF(IRG,CURIYR) + EGENNREE(IRG,CURIYR)) * 0.001
!3344 format(1h ,'!co2oc',i4,i3,6f12.4)
                  ECO2NRQF(IRG,CURIYR) = (ECO2NRRT(IRG,CURIYR) + CO2_STDOC) * (EGENNRQF(IRG,CURIYR) + EGENNREE(IRG,CURIYR)) * 0.001
!           IF MASS BASED STANDARD, ADJUST EMISSIONS FOR "TRANSFERS", IF REGIONS GROUPED
               ELSE IF (CO2_STDTN(IRG) .EQ. 2)THEN
                  DO REG = 1 , UNRGNS
                     IF (CO2_STDTN(REG) .EQ. 2 .AND. CO2_STDGN(IRG,REG) .EQ. 1)THEN
                        COL = 'CM' // URGNME(REG)(6:7) // URGNME(IRG)(6:7) // 'NR' ; call makmsk(COL_mask,':CM:' , URGNME(REG)(6:7) , URGNME(IRG)(6:7), ':NR:')
                        CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
                        IF (IRET .EQ. 0)THEN
                           ECO2NRQF(IRG,CURIYR) = ECO2NRQF(IRG,CURIYR) - SOLVAL(1)
                           ECO2NRQF(REG,CURIYR) = ECO2NRQF(REG,CURIYR) + SOLVAL(1)
                        END IF
                     END IF
                  END DO
               END IF
            END IF
!           CO2 INTENSITY STANDARD TOTAL EMISSIONS
            ROW = 'CO2TNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CO2TNR:' , URGNME(IRG)(6:7))
            CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
            IF (IRET .EQ. 0)THEN
               ECO2NRTL(IRG,CURIYR) = SOLVAL(1)
               ECO2NRTL(MNUMNR,CURIYR) = ECO2NRTL(MNUMNR,CURIYR) + SOLVAL(1)
            END IF
         END DO
!        SUM UP TOTAL U.S. AFFECTED EMISSIONS
         DO IRG = 1 , UNRGNS
            ECO2NRQF(MNUMNR,CURIYR) = ECO2NRQF(MNUMNR,CURIYR) + ECO2NRQF(IRG,CURIYR)
         END DO
         IF(EGENNRQF(MNUMNR,CURIYR) .GT. 0.0)THEN
            ECO2NRRT(MNUMNR,CURIYR) = ECO2NRRT(MNUMNR,CURIYR) / (EGENNRQF(MNUMNR,CURIYR) + EGENNREE(MNUMNR,CURIYR))
            ECO2NRPR(MNUMNR,CURIYR) = ECO2NRPR(MNUMNR,CURIYR) / (EGENNRQF(MNUMNR,CURIYR) + EGENNREE(MNUMNR,CURIYR))
         END IF
!        EMISSIONS REDUCTION CREDIT (ERC) TRADING
         if (curitr .eq. 1)print *,'!stdy1',curiyr+1989,co2_stdy1,co2_stdyn
         IF (CO2_STDSW .GT. 0 .AND. (CURIYR + UHBSYR) .GE. CO2_STDY1 .AND. CO2_ERCSW .GT. 0)THEN
            DO IRG = 1 , UNRGNS
               IF (CO2_ERCNR(IRG) .EQ. 2)THEN
!                 EFD CREDIT TRADE PRICE
                  IF (CO2_STDTN(IRG) .EQ. 1)THEN
!                    RATE BASE
                     ROW = 'ERCTNR' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':ERCTNR:' , URGNME(IRG)(6:7))
                     CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
                     IF (IRET .EQ. 0)THEN
                        EERCNRPR(IRG,CURIYR) = SOLVAL(5)
                     END IF
                  ELSE IF (CO2_STDTN(IRG) .EQ. 2)THEN
!                    MASS BASE
                     EERCNRPM(IRG,CURIYR) = ECO2NRPR(IRG,CURIYR) * 2204.0
                  END IF
!                 EFD CREDIT TRADE QUANTITIES
                  DO REG = 1 , UNRGNS
                     IF (IRG .NE. REG .AND. CO2_STDGN(IRG,REG) .EQ. 2)THEN
                        COL = 'CE' // URGNME(IRG)(6:7) // URGNME(REG)(6:7) // 'NR' ; call makmsk(COL_mask,':CE:' , URGNME(IRG)(6:7) , URGNME(REG)(6:7), ':NR:')
                        CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
                        IF (IRET .EQ. 0)THEN
                           IF (SOLVAL(1) .GT. DBLE(0.0))THEN
                              IF (CO2_STDTN(IRG) .EQ. 1 .AND. CO2_STDTN(REG) .EQ. 1)THEN
!                             RATE BASE
                                 EERCNRQR(IRG,REG,CURIYR) = SOLVAL(1)
                                 EERCNRQR(IRG,MNUMNR,CURIYR) = EERCNRQR(IRG,MNUMNR,CURIYR) + SOLVAL(1)
                                 EERCNRQR(MNUMNR,REG,CURIYR) = EERCNRQR(MNUMNR,REG,CURIYR) + SOLVAL(1)
                                 EERCNRQR(MNUMNR,MNUMNR,CURIYR) = EERCNRQR(MNUMNR,MNUMNR,CURIYR) + SOLVAL(1)
!                                NATIONAL ERC PRICE
                                 EERCNRPR(MNUMNR,CURIYR) = EERCNRPR(MNUMNR,CURIYR) + SOLVAL(1) * EERCNRPR(IRG,CURIYR)
!                                if (curitr .gt. 1)write(6,3333) curiyr+1989, COL,SOLVAL(1) , ERCPRCDN(IRG,CURIYR),  &
!                                ERCPRCDN(MNUMNR,CURIYR)
 3333 format(1h ,'!ercn',i4,a10,3f10.1)
                              ELSE IF (CO2_STDTN(IRG) .EQ. 2 .AND. CO2_STDTN(REG) .EQ. 2)THEN
!                             MASS BASE
                                 EERCNRQM(IRG,REG,CURIYR) = SOLVAL(1) / 2.204
                                 EERCNRQM(IRG,MNUMNR,CURIYR) = EERCNRQM(IRG,MNUMNR,CURIYR) + SOLVAL(1) / 2.204
                                 EERCNRQM(MNUMNR,REG,CURIYR) = EERCNRQM(MNUMNR,REG,CURIYR) + SOLVAL(1) / 2.204
                                 EERCNRQM(MNUMNR,MNUMNR,CURIYR) = EERCNRQM(MNUMNR,MNUMNR,CURIYR) + SOLVAL(1) / 2.204
!                                NATIONAL ERC PRICE
                                 EERCNRPM(MNUMNR,CURIYR) = EERCNRPM(MNUMNR,CURIYR) + (SOLVAL(1) / 2.204) * EERCNRPM(IRG,CURIYR)
                              END IF
                           END IF
                        END IF
                     END IF
                  END DO
               END IF
            END DO
            IF (EERCNRQR(MNUMNR,MNUMNR,CURIYR) .GT. 0.0)  &
               EERCNRPR(MNUMNR,CURIYR) = EERCNRPR(MNUMNR,CURIYR) / EERCNRQR(MNUMNR,MNUMNR,CURIYR)
            IF (EERCNRQM(MNUMNR,MNUMNR,CURIYR) .GT. 0.0)  &
               EERCNRPM(MNUMNR,CURIYR) = EERCNRPM(MNUMNR,CURIYR) / EERCNRQM(MNUMNR,MNUMNR,CURIYR)
         END IF
      END IF

      RETURN
      END
!
!     This subroutine retrieves the marginal cost from the load rows
!
      SUBROUTINE EDO$LOAD(IRG)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispout'
      include'dispcrv'
      include'dispuse'
      include'dispett'
      include'uettout'
      include'postpr'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'ecpcntl'
      include'uecpout'
      include'ecp_nuc'
      include'uefdout'
      include'emission'
      include'cdsparms'
      include'csapr'
      include'emmemis'
      include'emm_aimms'

      INTEGER IS,ISL,GRP,SEG,STP,FSL,IRET,IRG,EXRG,IRETPL,CSTP,CRG,IRET_SR,I,J
      REAL RNWGEN2
      REAL*8 SOLVAL(5),prc,GEN,REVIMP,REVEX,COST,TRANCST,MAX_HGHT,LOAD,SOLVAL_SR_ROW(5),SOLVAL_SR_COL(5)
      REAL*8 TOT_GEN
      CHARACTER*1 FSLCD
      CHARACTER*16 ROW,COL,MASKPL,COLNAMP,ROW_SR,COL_SR
      CHARACTER*16 ROW_TR
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*8 ROWSOL /'ASLUP   '/
      CHARACTER*2 STAT,STAT_SR_ROW,STAT_SR_COL


      
      efdsub='EDO$LOAD'

      IF (IRG .EQ. 1) NET_XPORT_EFD = 0.0

      DO IS = 1, EENSP

        DO ISL = 1 , ELNVCT(IS)
          GRP = ELGRP(ISL,IS)
          SEG = ELSEG(ISL,IS)
          FSL = EFD_Slice_ID(GRP,SEG)
          FSLCD = CHAR(64+FSL)

          ROW = 'LD'//URGNME(IRG)(1:4)//CHCOD(IS)//FSLCD; call makmsk(ROW_mask,':LD:',URGNME(IRG)(1:4),CHCOD(IS),FSLCD)
          CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
          prc = -1.0 * SOLVAL(5) * 0.1
          NMARCST(IRG,IS,ISL,CURIYR) = prc*1000.0

!         get trade for this import region

          DO EXRG = 1, UNRGNS
           IF (CNSTRNTS_EFD(IS,CURIYR,IRG,EXRG) .GT. 0.001) THEN
            COL = 'TR'//URGNME(EXRG)(6:7)//URGNME(IRG)(6:7)//CHCOD(IS)//FSLCD; call makmsk(COL_mask,':TR:',URGNME(EXRG)(6:7),URGNME(IRG)(6:7),CHCOD(IS),FSLCD)
            CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
            IF (SOLVAL(1) .GT. 0.001) THEN

              NET_XPORT_EFD(SEG,GRP,EXRG) = NET_XPORT_EFD(SEG,GRP,EXRG) + SOLVAL(1)
              NET_XPORT_EFD(SEG,GRP,IRG) = NET_XPORT_EFD(SEG,GRP,IRG) - SOLVAL(1) * (1.0 - LINELOSS)

              GEN = SOLVAL(1) * UTWDTH(SEG,GRP)
              REVEX = -1.0 * GEN * prc * 1000.0
              REVIMP = GEN * prc * 1000.0
              ETDMDERG(EXRG) = ETDMDERG(EXRG) + REVEX/1000.0
              ETDMDERG(IRG) = ETDMDERG(IRG) + REVIMP/1000.0
              ETDMMERG(EXRG) = ETDMMERG(EXRG) - GEN
              ETDMMERG(IRG) = ETDMMERG(IRG) + GEN * (1.0 - LINELOSS)
              CHKGEN(IRG,EXRG) = CHKGEN(IRG,EXRG) + GEN * (1.0 - LINELOSS)
              UTECON(IRG,EXRG) = UTECON(IRG,EXRG) + GEN * (1.0 - LINELOSS)
              UTECONSL(IS,ISL,IRG,EXRG) = SOLVAL(1)
              UTCNSTSL(IS,ISL,IRG,EXRG) = SOLVAL(4)
              CHK$(IRG,EXRG) = CHK$(IRG,EXRG) - REVIMP/1000.0
              UTSALES(IRG,EXRG) = UTSALES(IRG,EXRG) - REVIMP/1000.0
              UTEXME(EXRG,CURIYR) = UTEXME(EXRG,CURIYR) + GEN
              UTEXDE(EXRG,CURIYR) = UTEXDE(EXRG,CURIYR) + REVIMP/1000.0
            ENDIF  ! SOLVAL > 0
           ENDIF   !CNSTRNTS_EFD > 0
          ENDDO   !EXRG

!         get econ imports from Canada

          MASKPL = 'TC'//'**'//URGNME(IRG)(6:7)//CHCOD(IS)//FSLCD; call makmsk(MASKPL_mask,':TC:','*','*',URGNME(IRG)(6:7),CHCOD(IS),FSLCD)
          !if(aimmsefd.eq.0 .or. aimefdbg.eq.1) then
          !  IRETPL = WFCMASK(MASKPL,COLNAMP) ; COLNAMP_mask=MASKPL_mask
          !endif
          call get_masked_col(maskpl,colnamp,iretpl)

          MASKPL = '        '
          DO WHILE (IRETPL .EQ. 0)
            EXRG = ICHAR(COLNAMP(3:3))-64+10
            CALL DWFSCOL(COLNAMP,COLSOL,STAT,SOLVAL,COLNAMP_mask,IRET)

           write(UF_DBG,111) CURIYR,CURITR,IRG,EXRG,COLNAMP,SOLVAL(1)
111        format(1x,'ECON CAN',4I4,A10,F10.2)

            IF (SOLVAL(1) .GT. 0.001) THEN

              NET_XPORT_EFD(SEG,GRP,IRG) = NET_XPORT_EFD(SEG,GRP,IRG) - SOLVAL(1) * (1.0 - LINELOSS)

              GEN = SOLVAL(1) * UTWDTH(SEG,GRP)
              ETDIMERG(IRG) = ETDIMERG(IRG) + GEN * (1.0 - LINELOSS)
              CHKGEN(IRG,EXRG) = CHKGEN(IRG,EXRG) + GEN * (1.0 - LINELOSS)
              UTECON(IRG,EXRG) = UTECON(IRG,EXRG) + GEN * (1.0 - LINELOSS)
              UTECONSL(IS,ISL,IRG,EXRG) = UTECONSL(IS,ISL,IRG,EXRG) + SOLVAL(1)
!             UTCNSTSL(IS,ISL,IRG,EXRG) = MIN(999.99,SOLVAL(4))

!             calculate price of canadian imports

              TRANCST = PTHRESH1(CURIYR,EXRG,IRG) + PTHRESH2(CURIYR,EXRG,IRG) + BARRIER(CURIYR)
              CRG = EXRG - MNUMNR
              CSTP = ICHAR(COLNAMP(4:4)) - 48
              COST = DBLE(((CAN_CST(CSTP,CRG,CURIYR)*CAN_CST_SCMULT(CURIYR))+TRANCST)*0.001)
              CHK$(IRG,EXRG) = CHK$(IRG,EXRG) + ( COST * GEN )
              UTSALES(IRG,EXRG) = UTSALES(IRG,EXRG) + ( COST * GEN )

!             get transmission constraint value           

              ROW_TR = 'TCN'//CHCOD(EXRG)//URGNME(IRG)(6:7)//CHCOD(IS)//FSLCD; call makmsk(ROW_TR_mask,':TCN:',CHCOD(EXRG),URGNME(IRG)(6:7),CHCOD(IS),FSLCD)

              CALL  DWFSROW(ROW_TR,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
              UTCNSTSL(IS,ISL,IRG,EXRG) = SOLVAL(4)

            ENDIF
            !if(aimmsefd.eq.0 .or. aimefdbg.eq.1) then
            !  IRETPL = WFCMASK(MASKPL,COLNAMP) ; COLNAMP_mask=MASKPL_mask
            !endif
             call get_masked_col(maskpl,colnamp,iretpl)

          ENDDO  !while
        ENDDO     !ISL
      ENDDO       ! IS

      DO IS = 1, EENSP
         MAX_HGHT = 0.0
         DO ISL = 1 , ELNVCT(IS)
            GRP = ELGRP(ISL,IS)
            SEG = ELSEG(ISL,IS)
            FSL = EFD_Slice_ID(GRP,SEG)
            FSLCD = CHAR(64+FSL)
            LOAD = UTHGHT(SEG,GRP,IRG)
            MAX_HGHT = MAX(MAX_HGHT , LOAD)
         END DO

         DO ISL = 1 , ELNVCT(IS)
            GRP = ELGRP(ISL,IS)
            SEG = ELSEG(ISL,IS)
            FSL = EFD_Slice_ID(GRP,SEG)
            FSLCD = CHAR(64+FSL)
            LOAD = UTHGHT(SEG,GRP,IRG)
            ROW_SR = 'SR' // URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(ROW_SR_mask,':SR:' , URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)
            CALL DWFSROW(ROW_SR,ROWSOL,STAT_SR_ROW,SOLVAL_SR_ROW,ROW_SR_mask,IRET_SR)

!           CALCULATE SPINNING RESERVE REQUIREMENT

            SP_RES_REQ(GRP,SEG,IRG) = SR_RQMT_HGHT(IRG) * LOAD + SR_RQMT_DIFF(IRG) * (MAX_HGHT - LOAD)
            SP_RES_DUAL(GRP,SEG,IRG) = 0.0
            IF (SOLVAL_SR_ROW(5) .LT. -.001) THEN
               SP_RES_DUAL(GRP,SEG,IRG) = -1.0 * SOLVAL_SR_ROW(5)
            END IF

            SRPOOL(CURIYR,IRG) = SRPOOL(CURIYR,IRG) + SP_RES_DUAL(GRP,SEG,IRG) * SP_RES_REQ(GRP,SEG,IRG)
            SRPOOL(CURIYR,IRG) = SRPOOL(CURIYR,IRG) + SP_RES_DUAL(GRP,SEG,IRG) * SR_INT_REQ(GRP,SEG,IRG)

           
            COL_SR = 'SP' // URGNME(IRG)(1:4) // CHCOD(IS) // FSLCD; call makmsk(COL_SR_mask,':SP:' , URGNME(IRG)(1:4) , CHCOD(IS) , FSLCD)
            CALL DWFSCOL(COL_SR,COLSOL,STAT_SR_COL,SOLVAL_SR_COL,COL_SR_mask,IRET_SR)
            SP_RES_ACH(GRP,SEG,IRG) = SOLVAL_SR_COL(1)

            WRITE(UF_DBG,9425)'SPN_RSV_SRPOOL:af', CURIRUN, CURIYR+1989, CURITR, IRG, IS, ISL, GRP, SEG, SRPOOL(CURIYR,IRG), &
               SP_RES_DUAL(GRP,SEG,IRG), SP_RES_REQ(GRP,SEG,IRG), SR_INT_REQ(GRP,SEG,IRG),SP_RES_ACH(GRP,SEG,IRG),UTWDTH(SEG,GRP)
 9425       FORMAT(1x,A25,1x,8(":",I5),6(":",F21.6))

!           WRITE(6,3751) CURIRUN, CURIYR+1989, CURITR, IRG, IS, ISL, GRP, SEG, STAT_SR_ROW, STAT_SR_COL, &
!              MAX_HGHT, LOAD, SOLVAL_SR_ROW(1), SOLVAL_SR_ROW(2), SOLVAL_SR_ROW(5), &
!              SOLVAL_SR_COL(1), SOLVAL_SR_COL(2), SOLVAL_SR_COL(3), SOLVAL_SR_COL(5), &
!              SP_RES_RQMT(GRP,SEG), SR_RQMT_HGHT(IRG), SR_RQMT_DIFF(IRG)
!3751       FORMAT(1X,"SR_EFD_RQMT_INFO",8(":",I5),2(":",A2),12(":",F21.6))

         END DO
      END DO


      RETURN
      END
!
!     This subroutine retrieves the must-run duals to adjust marginal energy costs

      SUBROUTINE EDO$MRUN
      use efd_row_col

      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispuse'
      include 'emission'
      include 'cdsparms'
      include 'csapr'
      include 'emmemis'

      INTEGER IREG, N, IECP, IPGRP, IRET
      REAL*8 SOLVAL(5),prc,act
      CHARACTER*16 ROW
      CHARACTER*8 ROWSOL /'ASLUP   '/
      CHARACTER*2 STAT
      CHARACTER*5 NUM

      efdsub='EDO$MRUN'

      DO IREG = 1, MNUMNR
         ULMRCST(IREG) = 0.0
      END DO

      DO N = 1, N_EFD_GRPS
       IECP = UG_ECPt(N)
       IREG = UG_EMM_RG(N)
         IF (UG_MRUN(N) .GT. 0 .AND. IECP .LE. ECP_D_DSP) THEN
            IPGRP = EFD_GRPS_F(N)
            WRITE(NUM,'(I5.5)') IPGRP
            ROW = 'MR'//NUM//'X'; call makmsk(ROW_mask,':MR:',NUM,':X:')
            CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
            prc = -1.0 * SOLVAL(5)
            act = SOLVAL(1)
            ULMRCST(IREG) = ULMRCST(IREG) + prc * act
         ENDIF
      END DO

      RETURN
      END
!
!     This subroutine retrieves the planned maintenance constraints

      SUBROUTINE EDO$PM(IRG)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'elcntl'
      include'ecpcntl'
      include'ecp_nuc'

      INTEGER  IEFD,IS,IP,IGRP,IRET,IMO,IRG,N,JGRP,I_EFD_GRP,IECP,MAP_EFD_TO_ECP(EFD_D_CAP)
      REAL     PMSH(EFD_D_DSP,EFD_D_MSP),SHR_C,SHR_G
      REAL*8   DIGITS2,CAP,SOLVAL(5),capout
      CHARACTER*5 NUM
      CHARACTER*1 REGCD(MNUMNR)
      CHARACTER*16 COL
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*2 STAT

      efdsub='EDO$PM'

      DO N = 1, MNUMNR
        REGCD(N) = CHCOD(N)
      ENDDO

      MAP_EFD_TO_ECP = 1
      DO IECP = 1 , ECP_D_CAP
         IEFD = UPEFDT(IECP)
         MAP_EFD_TO_ECP(IEFD) = IECP
      END DO

      DO IEFD = 1, EFD_D_DSP
        IF (EPPOPM(IEFD) .EQ. 2) THEN
          IECP = MAP_EFD_TO_ECP(IEFD)
          IF (SR_CREDIT(IECP) .EQ. 0.0) THEN
             DO IS = 1, EENSP

!              column for planned maint decision

               COL = 'MP'//REGCD(IRG)//EPPLCD(IEFD)//CHCOD(IS)//'XX'; call makmsk(COL_mask,':MP:',REGCD(IRG),EPPLCD(IEFD),CHCOD(IS),':XX:')
               CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
               cap = SOLVAL(4)   ! upper limit = total capacity
               capout = SOLVAL(1) !decision variable
               IF (CAP .GT. 0.0) THEN
                 PMSH(IEFD,IS) = capout/cap
               ELSE
                 PMSH(IEFD,IS) = 0.0
               ENDIF
             ENDDO
           ENDIF
        ENDIF
      ENDDO

!     loop over all plant groups

      DO N = 1, ECNTP
        IP = ECASTS(N)
        IGRP = ECDBID(N)
        IECP = UG_ECPt(N)
        I_EFD_GRP = MAP_ECNTP_EFD_GRPS(N,IRG)
        JGRP = EFD_GRPS_F(I_EFD_GRP)
        SHR_G = UP_GEN(0,IGRP) / UG_GEN(0,I_EFD_GRP)

        IF (EPPOPM(IP) .EQ. 1)  THEN  ! baseload plant type, do maintenance by group

           WRITE(NUM,'(I5.5)') JGRP

           DO IS = 1, EENSP

             IF (UG_CAP(IS,I_EFD_GRP) .GT. 0.0) THEN
                SHR_C = UP_CAP(IS,IGRP) / UG_CAP(IS,I_EFD_GRP)
             ELSE
                SHR_C = UP_CAP(0,IGRP) / UG_CAP(0,I_EFD_GRP)
             END IF

             cap = ECCAP(N,IS) * 0.001

!            column for planned maint decision

             COL = 'MB'//NUM//CHCOD(IS); call makmsk(COL_mask,':MB:',NUM,CHCOD(IS))
             CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
             capout = SOLVAL(1) * SHR_C
             ECCOPM(N,IS) = capout * 1000.0
             ECDSPC(IGRP,IS) = (cap - capout) * 1000.0
           ENDDO

         ELSE IF (EPPOPM(IP) .EQ. 2)  THEN  !peak/cycling type, do maintenance by plt type
           IF (SR_CREDIT(IECP) .GT. 0.0) THEN
             WRITE(NUM,'(I5.5)') JGRP

             DO IS = 1, EENSP

               IF (UG_CAP(IS,I_EFD_GRP) .GT. 0.0) THEN
                  SHR_C = UP_CAP(IS,IGRP) / UG_CAP(IS,I_EFD_GRP)
               ELSE
                  SHR_C = UP_CAP(0,IGRP) / UG_CAP(0,I_EFD_GRP)
               END IF

               cap = ECCAP(N,IS) * 0.001

!              column for planned maint decision

               COL = 'MP'//NUM//CHCOD(IS); call makmsk(COL_mask,':MP:',NUM,CHCOD(IS))
               CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
               capout = SOLVAL(1) * SHR_C
               ECCOPM(N,IS) = capout * 1000.0
               ECDSPC(IGRP,IS) = (cap - capout) * 1000.0
             ENDDO
           ELSE
             DIGITS_PARM = 6
             DO IS = 1, EENSP

               IF (UG_CAP(IS,I_EFD_GRP) .GT. 0.0) THEN
                  SHR_C = UP_CAP(IS,IGRP) / UG_CAP(IS,I_EFD_GRP)
               ELSE
                  SHR_C = UP_CAP(0,IGRP) / UG_CAP(0,I_EFD_GRP)
               END IF

               IF (USW_DIGIT .GT. 0)THEN
               cap = DIGITS2(DBLE(ECCAP(N,IS)*0.001),DIGITS_PARM)
               ELSE
                  cap = DBLE(ECCAP(N,IS)*0.001)
               END IF
               capout = PMSH(IP,IS) * cap
               ECCOPM(N,IS) = capout * 1000.0
               ECDSPC(IGRP,IS) = (cap - capout) * 1000.0
             ENDDO
           END IF

         ENDIF
       ENDDO   !ECNTP

       RETURN
       END
!
!     This subroutine retrieves STEO benchmarking adjustments
!
      SUBROUTINE EDO$BENCH
      use efd_row_col
      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'uefdout'
      include'emission'
      include'cdsparms'
      include'csapr'
      include'emmemis'
 
      INTEGER IRET,IRG,KRG,YEAR,GRP,REG

      CHARACTER*16 ROW,COLUMN
      REAL*8 VALUE, SOLVAL(5)
      REAL*8 BMCLGENO(MNUMYR,MNUMNR),BMNGGENO(MNUMYR,MNUMNR),BMOLGENO(MNUMYR,MNUMNR)
      REAL*8 BMCLCONO(MNUMYR,MNUMNR),BMNGCONO(MNUMYR,MNUMNR),BMOLCONO(MNUMYR,MNUMNR)
      REAL*8 BMCLGENP(MNUMYR),BMCLCONP(MNUMYR),BMNGGENP(CURIYR),BMNGCONP(CURIYR),BMOLGENP(CURIYR),BMOLCONP(CURIYR)
      REAL*8 BMNETIMPP(MNUMYR),BMNETIMPO(MNUMYR)
      REAL*8 CLLB,CLUB,NGLB,NGUB,OLLB,OLUB,IMPLB,IMPUB
      CHARACTER*8 ROWSOL /'ASLUP   '/
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*2 STAT

      efdsub='EDO$BENCH'

      ULBMCST = 0.0

!     GENERATION ROWS

!     COAL
      IF (BMCLGEN(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'GENCLB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':GENCLB:' , URGNME(IRG)(6:7))
            CALL DWFSCOL(COLUMN,COLSOL,STAT,SOLVAL,COLUMN_mask,IRET)
            BMCLGENO(CURIYR,IRG) = SOLVAL(1)
            IF (IRG .EQ. MNUMNR) THEN
               CLLB = SOLVAL(3)
               CLUB = SOLVAL(4)
               ROW = 'GENCL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENCL:' , URGNME(IRG)(6:7))
               CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
               BMCLGENP(CURIYR) = -1.0 * SOLVAL(5)                              
               ULBMCST = ULBMCST + BMCLGENP(CURIYR) * BMCLGENO(CURIYR,IRG)
            ENDIF
         ENDDO
       IF (FCRL .EQ. 1) then
         WRITE(22,9500) CURIYR,(BMCLGENO(CURIYR,IRG),IRG=1,MNUMNR)
         WRITE(22,9510) 'BMCLGUS',CURIYR,BMCLGENP(CURIYR),BMCLGENO(CURIYR,MNUMNR),BMCLGEN(CURIYR),CLLB,CLUB
9500  FORMAT(1X,'BMCLGOUT:',I4,25(':',F8.2))
9510  FORMAT(1X,A10,':',I4,5(':',F10.2))
       ENDIF
      ENDIF
                         
!     GAS
      IF (BMNGGEN(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'GENNGB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':GENNGB:' , URGNME(IRG)(6:7))
            CALL DWFSCOL(COLUMN,COLSOL,STAT,SOLVAL,COLUMN_mask,IRET)
            BMNGGENO(CURIYR,IRG) = SOLVAL(1)
            IF (IRG .EQ. MNUMNR) THEN
               NGLB = SOLVAL(3)
               NGUB = SOLVAL(4)
               ROW = 'GENNG' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENNG:' , URGNME(IRG)(6:7))
               CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
               BMNGGENP(CURIYR) = -1.0 * SOLVAL(5)                              
               ULBMCST = ULBMCST + BMNGGENP(CURIYR) * BMNGGENO(CURIYR,IRG)
            ENDIF
         ENDDO
       IF (FCRL .EQ. 1) THEN
         WRITE(22,9501) CURIYR,(BMNGGENO(CURIYR,IRG),IRG=1,MNUMNR)
         WRITE(22,9510) 'BMNGGUS',CURIYR,BMNGGENP(CURIYR),BMNGGENO(CURIYR,MNUMNR),BMNGGEN(CURIYR),NGLB,NGUB
9501  FORMAT(1X,'BMNGGOUT:',I4,25(':',F8.2))
       ENDIF
      ENDIF

!     OIL
      IF (BMOLGEN(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'GENOLB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':GENOLB:' , URGNME(IRG)(6:7))
            CALL DWFSCOL(COLUMN,COLSOL,STAT,SOLVAL,COLUMN_mask,IRET)
            BMOLGENO(CURIYR,IRG) = SOLVAL(1)
            IF (IRG .EQ. MNUMNR) THEN
               OLLB = SOLVAL(3)
               OLUB = SOLVAL(4)
               ROW = 'GENOL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':GENOL:' , URGNME(IRG)(6:7))
               CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
               BMOLGENP(CURIYR) = -1.0 * SOLVAL(5)                              
               ULBMCST = ULBMCST + BMOLGENP(CURIYR) * BMOLGENO(CURIYR,IRG)
            ENDIF
          ENDDO
       IF (FCRL .EQ. 1) THEN
         WRITE(22,9502) CURIYR,(BMOLGENO(CURIYR,IRG),IRG=1,MNUMNR)
         WRITE(22,9510) 'BMOLGUS',CURIYR,BMOLGENP(CURIYR),BMOLGENO(CURIYR,MNUMNR),BMOLGEN(CURIYR),OLLB,OLUB
9502  FORMAT(1X,'BMOLGOUT:',I4,25(':',F8.2))
       ENDIF
      ENDIF


!     CONSUMPTION ROWS

!     COAL
      IF (BMCLCON(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'CONCLB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':CONCLB:' , URGNME(IRG)(6:7))
            CALL DWFSCOL(COLUMN,COLSOL,STAT,SOLVAL,COLUMN_mask,IRET)
            BMCLCONO(CURIYR,IRG) = SOLVAL(1)
            IF (IRG .EQ. MNUMNR) THEN
               CLLB = SOLVAL(3)
               CLUB = SOLVAL(4)
               ROW = 'CONCL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CONCL:' , URGNME(IRG)(6:7))
               CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
               BMCLCONP(CURIYR) = -1.0 * SOLVAL(5)                              
               ULBMCST = ULBMCST + BMCLCONP(CURIYR) * BMCLCONO(CURIYR,IRG)
            ENDIF
          ENDDO
       IF (FCRL .EQ. 1) THEN
         WRITE(22,9503) CURIYR,(BMCLCONO(CURIYR,IRG),IRG=1,MNUMNR)
         WRITE(22,9510) 'BMCLCUS',CURIYR,BMCLCONP(CURIYR),BMCLCONO(CURIYR,MNUMNR),BMCLCON(CURIYR),CLLB,CLUB
9503  FORMAT(1X,'BMCLCOUT:',I4,25(':',F8.2))
       ENDIF
      ENDIF
                         
!     GAS
      IF (BMNGCON(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'CONNGB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':CONNGB:' , URGNME(IRG)(6:7))
            CALL DWFSCOL(COLUMN,COLSOL,STAT,SOLVAL,COLUMN_mask,IRET)
            BMNGCONO(CURIYR,IRG) = SOLVAL(1)
            IF (IRG .EQ. MNUMNR) THEN
               NGLB = SOLVAL(3)
               NGUB = SOLVAL(4)
               ROW = 'CONNG' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CONNG:' , URGNME(IRG)(6:7))
               CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
               BMNGCONP(CURIYR) = -1.0 * SOLVAL(5)                              
               ULBMCST = ULBMCST + BMNGCONP(CURIYR) * BMNGCONO(CURIYR,IRG)
            ENDIF
          ENDDO
       IF (FCRL .EQ. 1) THEN
         WRITE(22,9504) CURIYR,(BMNGCONO(CURIYR,IRG),IRG=1,MNUMNR)
         WRITE(22,9510) 'BMNGCUS',CURIYR,BMNGCONP(CURIYR),BMNGCONO(CURIYR,MNUMNR),BMNGCON(CURIYR),NGLB,NGUB
9504  FORMAT(1X,'BMNGCOUT:',I4,25(':',F8.2))
       ENDIF
      ENDIF

!     OIL
      IF (BMOLCON(CURIYR) .GT. 0.0)THEN
         DO IRG = 1 , MNUMNR
            COLUMN = 'CONOLB' // URGNME(IRG)(6:7); call makmsk(COLUMN_mask,':CONOLB:' , URGNME(IRG)(6:7))
            CALL DWFSCOL(COLUMN,COLSOL,STAT,SOLVAL,COLUMN_mask,IRET)
            BMOLCONO(CURIYR,IRG) = SOLVAL(1)
            IF (IRG .EQ. MNUMNR) THEN
               OLLB = SOLVAL(3)
               OLUB = SOLVAL(4)
               ROW = 'CONOL' // URGNME(IRG)(6:7); call makmsk(ROW_mask,':CONOL:' , URGNME(IRG)(6:7))
               CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
               BMOLCONP(CURIYR) = -1.0 * SOLVAL(5)                              
               ULBMCST = ULBMCST + BMOLCONP(CURIYR) * BMOLCONO(CURIYR,IRG)
            ENDIF
          ENDDO
       IF (FCRL .EQ. 1) THEN
         WRITE(22,9505) CURIYR,(BMOLCONO(CURIYR,IRG),IRG=1,MNUMNR)
         WRITE(22,9510) 'BMOLCUS',CURIYR,BMOLCONP(CURIYR),BMOLCONO(CURIYR,MNUMNR),BMOLCON(CURIYR),OLLB,OLUB
9505  FORMAT(1X,'BMOLCOUT:',I4,25(':',F8.2))
       ENDIF
      ENDIF

!     NET IMPORTS
      IF (BMNETIMP(CURIYR) .GT. 0.0)THEN
        COLUMN = 'CNIMPBUS' ; call makmsk(COLUMN_mask,':CNIMPBUS:' )
        CALL DWFSCOL(COLUMN,COLSOL,STAT,SOLVAL,COLUMN_mask,IRET)
        BMNETIMPO(CURIYR) = SOLVAL(1)
        IMPLB = SOLVAL(3)
        IMPUB = SOLVAL(4)
        ROW = 'CANIMPUS' ; call makmsk(ROW_mask,':CANIMPUS:')
        CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
        BMNETIMPP(CURIYR) = -1.0 * SOLVAL(5)                              
        ULBMCST = ULBMCST + BMNETIMPP(CURIYR) * BMNETIMPO(CURIYR)

       IF (FCRL .EQ. 1) THEN
!        WRITE(22,9506) CURIYR,(BMNETIMPO(CURIYR)
         WRITE(22,9510) 'BMIMPUS',CURIYR,BMNETIMPP(CURIYR),BMNETIMPO(CURIYR),BMNETIMP(CURIYR),IMPLB,IMPUB
9506  FORMAT(1X,'BMIMPOUT:',I4,25(':',F8.2))
       ENDIF
      ENDIF

      RETURN
      END

!
!     This subroutine retrieves the aci costs
!
      SUBROUTINE EDO$ACI
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecp_coal'
      include'emission'
      include'cdsparms'
      include'csapr'
      include'emmemis'

      INTEGER ACI
      INTEGER IRET
      REAL*8 SOLVAL(5)
      CHARACTER*16 ROW,COL
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*8 ROWSOL /'ASLUP   '/
      CHARACTER*2 STAT
      CHARACTER*1 ACICODE

      efdsub='EDO$ACI'


!     INITIALIZE
          ACICST(CURIYR) = 0.0
          ACIOAM(CURIYR) = 0.0

!     ACI COST
      DO ACI = 1 , NUM_ACSS
        WRITE(ACICODE,'(I1)') ACI
        COL = 'ACISPLY' // ACICODE; call makmsk(COL_mask,':ACISPLY:' , ACICODE)
        CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
!     write(6,1234) curiyr+1989,curitr,col,iret,solval(1),solval(2)
!1234 format(1h ,'!acicst',i4,i3,a19,i3,2f10.4)
        IF (IRET .EQ. 0)THEN
          ACICST(CURIYR) = SOLVAL(1) * SOLVAL(2)
        END IF
      END DO

!     ACI O&M
        ROW = 'ACIOMCST';ROW_mask=ROW
        CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
!     write(6,2345) curiyr+1989,curitr,row,iret,solval(1),solval(2)
!2345 format(1h ,'!acioam',i4,i3,a19,i3,2f10.4)
        IF (IRET .EQ. 0)THEN
          ACIOAM(CURIYR) = SOLVAL(1)
        END IF
!     print *,'!aciout',curiyr+1989,curitr,acicst(curiyr),acioam(curiyr)

      RETURN
      END
!
!     This subroutine retrieves the dual from the biomass rows
!
      SUBROUTINE EDO$BIO
      use efd_row_col
      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'wrenew'
      include'fuelin'

      INTEGER CRG, IRET, I_SUPt, I_PRINT
      REAL*8 SOLVAL(5), COLVAL(5), AVG_PRC, TOT_Q
      CHARACTER*16 WDROW, WDCOL
      CHARACTER*8 ROWSOL /'ASLUP   '/
      CHARACTER*8 COLSOL /'ACLUD   '/
      CHARACTER*2 BR,STAT

      efdsub='EDO$BIO'

      I_PRINT = 18
      AVG_PRC = 0.0
      TOT_Q = 0.0
      QBMPWCL(0,0,CURIYR) = 0.0
      PBMPWCL(0,0,CURIYR) = 0.0
      DO I_SUPt = 1 , MNUMFS
         QBMPWCL(I_SUPt,0,CURIYR) = 0.0
         PBMPWCL(I_SUPt,0,CURIYR) = 0.0
      END DO

      DO CRG = 1 , NDREG
         QBMPWCL(0,CRG,CURIYR) = 0.0

         WRITE(BR,'(I2.2)') CRG
         WDROW = 'S_WD'// BR; call makmsk(WDROW_mask,':S_WD:', BR)
         CALL DWFSROW(WDROW,ROWSOL,STAT,SOLVAL,WDROW_mask,IRET)
         EMMBMDUAL(CRG,CURIYR) = ABS(SOLVAL(5))

         PBMPWCL(0,CRG,CURIYR) = ABS(SOLVAL(5))

!     write(6,1234) curiyr+1989,wdrow,EMMBMDUAL(CRG,CURIYR)
!1234 format(1h ,'!efddual',i4,a10,f10.3)

         DO I_SUPt = 1 , MNUMFS
            IF (WDSUP_AVL(I_SUPt) .EQ. 1 .AND. MP_BM_PW(I_SUPt) .GT. 0.0) THEN
               WDCOL = 'BP' // BR // BM_TYP_CD(I_SUPt) // "XX"; call makmsk(WDCOL_mask,':BP:' , BR , BM_TYP_CD(I_SUPt) , "XX")
               CALL DWFSCOL(WDCOL,COLSOL,STAT,COLVAL,WDCOL_mask,IRET)

               QBMPWCL(I_SUPt,CRG,CURIYR) = COLVAL(1)
               QBMPWCL(0,CRG,CURIYR) = QBMPWCL(0,CRG,CURIYR) + COLVAL(1)
               QBMPWCL(I_SUPt,0,CURIYR) = QBMPWCL(I_SUPt,0,CURIYR) + COLVAL(1)
               QBMPWCL(0,0,CURIYR) = QBMPWCL(0,0,CURIYR) + COLVAL(1)

               WDROW = 'S_WD' // BM_TYP_CD(I_SUPt) // BR; call makmsk(WDROW_mask,':S_WD:' , BM_TYP_CD(I_SUPt) , BR)
               CALL DWFSROW(WDROW,ROWSOL,STAT,SOLVAL,WDROW_mask,IRET)

               PBMPWCL(I_SUPt,CRG,CURIYR) = ABS(SOLVAL(5))
               PBMPWCL(I_SUPt,0,CURIYR) = PBMPWCL(I_SUPt,0,CURIYR) + ABS(SOLVAL(5)) * COLVAL(1)
               PBMPWCL(0,0,CURIYR) = PBMPWCL(0,0,CURIYR) + PBMPWCL(0,CRG,CURIYR) * COLVAL(1)
            END IF
         END DO
      END DO

      IF (QBMPWCL(0,0,CURIYR) .GT. 0.0) THEN
         PBMPWCL(0,0,CURIYR) = PBMPWCL(0,0,CURIYR) / QBMPWCL(0,0,CURIYR)
      END IF

      DO I_SUPt = 1 , MNUMFS
         IF (WDSUP_AVL(I_SUPt) .EQ. 1 .AND. MP_BM_PW(I_SUPt) .GT. 0.0) THEN
            IF (QBMPWCL(I_SUPt,0,CURIYR) .GT. 0.0) THEN
               PBMPWCL(I_SUPt,0,CURIYR) = PBMPWCL(I_SUPt,0,CURIYR) / QBMPWCL(I_SUPt,0,CURIYR)
            END IF
         END IF
      END DO

      DO CRG = 1 , NDREG
         UPFUEL(UIWD,CRG) = PBMPWCL(0,CRG,CURIYR)
      END DO

      DO CRG = 0 , NDREG
         WRITE(I_PRINT,3031) CURIRUN, CURIYR+1989, CURITR, CRG, (QBMPWCL(I_SUPt,CRG,CURIYR), I_SUPt = 0 , MNUMFS)
 3031    FORMAT(1X,"QBMPWCL",4(":",I4),<MNUMFS+1>(":",F12.3))
         WRITE(I_PRINT,3032) CURIRUN, CURIYR+1989, CURITR, CRG, (PBMPWCL(I_SUPt,CRG,CURIYR), I_SUPt = 0 , MNUMFS)
 3032    FORMAT(1X,"PBMPWCL",4(":",I4),<MNUMFS+1>(":",F12.3))
      END DO

      RETURN
      END
!
!     This subroutine retrieves the national RPS duals

      SUBROUTINE EDO$RPS
      use efd_row_col

      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'

      INTEGER IRET,IS
      REAL*8 SOLVAL(5)
      CHARACTER*16 ROW
      CHARACTER*8 ROWSOL /'ASLUP   '/
      CHARACTER*2 STAT

      efdsub='EDO$RPS'

      ROW = 'RPSREQU'; ROW_mask=ROW
      CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
      write(13,2345) curiyr+1989,curitr,row,solval(5)
 2345 format(1h ,'!rpsprc',i4,i3,a10,f10.3)

      RETURN
      END

!
!     This subroutine sets up the Coal Supply Submatrix for the EFD
!
      SUBROUTINE ED$COL
      use efd_row_col
      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'ecp_coal'
      include'cdsparms'
      include'coalemm'
      include'uso2grp'
      include'emission'
      include'emablk'
      include'emoblk'
      include'fuelin'
      include'csapr'
      include'dispinyr'
      include'emmemis'
      include'ecp_nuc'
      include'emm_aimms'
!
!      COMMON/CLCON/CLCONFC
!      REAL*4 CLCONFC(NDREG,MAXNFR + 1)
!
      INTEGER CRV,ICRV,CRG,CTY,STP,FRG,GRG,PLT,ACI,NACI,RANK,SO2,OTH,JSO2,KSO2,HG,IFGD
      INTEGER IRET,CTLPLT
      REAL*4 STSIZE,TMPSTP(11),HTRT,SO2RMV,HGEMF
      REAL*8 PRDLIM,COEFF,COEFFS,CP_CRG(ECP_D_CAP,EFD_D_MFRG)
      CHARACTER*1 ACICD(8),SO2CODE,OTHCODE,ACICODE,CFGD(2),RANK_TYPE
      CHARACTER*2 SC,ND,ST,CL_CD
      CHARACTER*2 CT
      CHARACTER*16 COLUMN
      CHARACTER*16 ROW_C,ROW_DS,ROW_DL,ROW_S,ROW_T,ROW_AC,ROW_AR,ROW_AO,ROW_K,ROWCAR,ROWCARR,ROWCARC
      CHARACTER*16 ROW_SO2,ROW_SO2_PT,ROW_MV,ROW_FR,ROW_TO,ROW_HG,ROW_HGO,ROW_HG_PT
!
      REAL*8 OUT,HR,IN,PRCNT,LIM,GRAM,RHG,EMF_P,EMF_M,F_FGD,F_SCR,A,B,C,D
      REAL*8 EMF_T(NSTEP),RACI(NSTEP),MACT_EMF
      INTEGER*4 Allowed(NSTEP),NMAX,N,CHOICE
      INTEGER ICLS,MACT_SW

      REAL*8 NEG1/-1.0D0/
!
      COMMON /TOT_RATES/ TOT_RATE1,TOT_RATE2,TOT_TYPE
      REAL*8 TOT_RATE1(MX_NCOALS+MX_ISCV,NDREG,0:ECP_D_FPH),TOT_RATE2(MX_NCOALS+MX_ISCV,NDREG,0:ECP_D_FPH)
      INTEGER*4 TOT_TYPE(MX_NCOALS+MX_ISCV)

      REAL*4 SO2SHR

      CHARACTER*2 CLRG(NDREG)
      character*30 save_row_mask

      INTEGER*4 ACI0,ACI1
!
      DATA CLRG/'NE',  &
                'YP',  &
                'S1',  &
                'S2',  &
                'GF',  &
                'OH',  &
                'EN',  &
                'KT',  &
                'AM',  &
                'C1',  &
                'C2',  &
                'WS',  &
                'MT',  &
                'CU',  &
                'ZN',  &
                'PC'/

      efdsub='ED$COL'


! Assign codes for ACI Options

         ACICD(1) = '0'
         ACICD(2) = '1'
         ACICD(3) = '2'
         ACICD(4) = '3'
         ACICD(5) = '4'
         ACICD(6) = '5'
         ACICD(7) = '6'
         ACICD(8) = '7'

         CALL COMBINE_COAL_RATES(CURIYR)

! Assign codes for Scrub/Unscrub

         CFGD(1) = 'S'
         CFGD(2) = 'U'
!
! Declare Row Type and Assign RHS (If any) for SO2 and Hg
!
!     print *,'!mat',curiyr+1989,curitr,efdmat,maxnfr
!
!     INITIALIZE COAL SUPPLY REG/DEM REG/PLT MATRIX
      CLSCPLDR = 0
      ACI0 = 0
      ACI1 = 0

      IF (EFDMAT .EQ. 0)THEN

! SO2

!       CALL SUBROUTINE TO SET UP COAL REGION LEVEL LIMIT FOR EPA TRANSPORT RULE, IF IMPOSED
        IF (TSO2_YR_BY_CLRG .LT. 9999)CALL EPA$TRANRG_EFD(10,1,CRG,1,COLUMN,COLUMN_mask,COEFF)

        DO SO2 = 1 , NUM_SO2_GRP
          WRITE(SO2CODE,'(I1)') SO2
          ROW_SO2 = 'SULFUR' // SO2CODE; call makmsk(ROW_SO2_mask,':SULFUR:' , SO2CODE)
         IF ((UYR_NOCAIR .LE. 0 .OR. (CURIYR + UHBSYR) .LT. UYR_NOCAIR) .AND.  &
             ((CURIYR + UHBSYR) .LT. TRANRULE1 .OR. SO2 .EQ. NUM_SO2_GRP))THEN
          CALL DROWTYPE(ROW_SO2,'L       ',ROW_SO2_mask)

! Account for Banks in SO2 Limit

          COEFF = DBLE((EMRFSA(CURIYR,SO2) + EMELBNK(CURIYR - 1,SO2) - EMELBNK(CURIYR,SO2)) / 1000.0)

!     write(6,2345) curiyr+1989,so2,row_so2,emrfsa(curiyr,so2),coeff
!2345 format(1h ,'!cso2',i4,i3,a10,2f10.2)

          CALL DRHS(EFDRHS,ROW_SO2,COEFF,ROW_SO2_mask,'ED$COL,1')
         ELSE
          CALL DROWTYPE(ROW_SO2,'N       ',ROW_SO2_mask)
         END IF

! If more that one SO2 region then link rows to create national limit prior to initial date

          IF ((CURIYR + UHBSYR) .LT. TRANRULE1 .AND. NUM_SO2_GRP .GT. 1)THEN
            DO OTH = 1 , NUM_SO2_GRP
              IF (SO2 .NE. OTH)THEN
                WRITE(OTHCODE,'(I1)') OTH
                COLUMN = 'MVSO2' // SO2CODE // OTHCODE; call makmsk(COLUMN_mask,':MVSO2:' , SO2CODE , OTHCODE)
                CALL DVAL(COLUMN,EFDOBJ,0.0001D0,COLUMN_mask,EFDOBJ,'ED$COL,2')
                CALL DVAL(COLUMN,ROW_SO2,1.0D0,COLUMN_mask,ROW_SO2_mask,'ED$COL,3')
                COLUMN = 'MVSO2' // OTHCODE // SO2CODE; call makmsk(COLUMN_mask,':MVSO2:' , OTHCODE , SO2CODE)
                CALL DVAL(COLUMN,EFDOBJ,0.0001D0,COLUMN_mask,EFDOBJ,'ED$COL,4')
               IF (SO2_SHR_ALW_GRP(CURIYR,OTH) .GT. 0.0)THEN
                COEFF = DBLE(SO2_SHR_ALW_GRP(CURIYR,SO2) / SO2_SHR_ALW_GRP(CURIYR,OTH))
                CALL DVAL(COLUMN,ROW_SO2,-COEFF,COLUMN_mask,ROW_SO2_mask,'ED$COL,5')
               ELSE
                CALL DVAL(COLUMN,ROW_SO2,NEG1,COLUMN_mask,ROW_SO2_mask,'ED$COL,6')
                CALL DBND(EFDBND,COLUMN,0.0D0,0.0D0,COLUMN_mask,'ED$COL,7')
               END IF
              END IF
            END DO
          END IF

        END DO

!       ACCOUNT FOR VARIABILITY LIMITS IN TRANSPORT RULE, IF APPROPRIATE

        IF ((CURIYR + UHBSYR) .GE. TRANRULE1)THEN
           DO JSO2 = 1 , NDREG
              IF (TSO2_SHR_BY_CLRG(JSO2,1) .GT. 0.0)THEN
                 IF ((CURIYR + UHBSYR) .GE. TSO2_VR_BY_CLRG)THEN
!                   IF ((CURIYR + UHBSYR) .GE. TRANRULE2)THEN
!                      COEFF = DBLE(TSO2_VR3_BY_CLRG(JSO2))
!                   ELSE IF ((CURIYR + UHBSYR) .GE. TRANRULE1)THEN
                       COEFF = DBLE(TSO2_VR1_BY_CLRG(JSO2))
!                   ELSE
!                      COEFF = DBLE(0.0)
!                   END IF
                    ROW_MV = 'MVSO2' // CLRG(JSO2); call makmsk(ROW_MV_mask,':MVSO2:' , CLRG(JSO2))
                    CALL DROWTYPE(ROW_MV,'L       ',ROW_MV_mask)
                    CALL DRHS(EFDRHS,ROW_MV,COEFF,ROW_MV_mask,'ED$COL,8')
                 END IF
                 DO KSO2 = 1 , NDREG
                    IF (JSO2 .NE. KSO2)THEN
                       IF (TSO2_SHR_BY_CLRG(KSO2,1) .GT. 0.0)THEN
                          COLUMN = 'MVS' // CLRG(KSO2) // CLRG(JSO2); call makmsk(COLUMN_mask,':MVS:' , CLRG(KSO2) , CLRG(JSO2))
                          CALL DVAL(COLUMN,EFDOBJ,0.0001D0,COLUMN_mask,EFDOBJ,'ED$COL,9')
                          IF ((CURIYR + UHBSYR) .GE. TSO2_VR_BY_CLRG)THEN
                             CALL DVAL(COLUMN,ROW_MV,DBLE(1.0),COLUMN_mask,ROW_MV_mask,'ED$COL,10')
                          END IF
                          ROW_TO = 'SULFUR' // CLRG(JSO2); call makmsk(ROW_TO_mask,':SULFUR:' , CLRG(JSO2))
                          CALL DVAL(COLUMN,ROW_TO,DBLE(-1.0),COLUMN_mask,ROW_TO_mask,'ED$COL,11')
                          ROW_FR = 'SULFUR' // CLRG(KSO2); call makmsk(ROW_FR_mask,':SULFUR:' , CLRG(KSO2))
                          CALL DVAL(COLUMN,ROW_FR,DBLE(1.0),COLUMN_mask,ROW_FR_mask,'ED$COL,12')
                       END IF
                    END IF
                 END DO
              END IF
           END DO
        END IF

! HG

        DO HG = 1 , NUM_HG_GRP
          ROW_HG = 'MERCURY' // UPRGCD(HG); call makmsk(ROW_HG_mask,':MERCURY:' , UPRGCD(HG))
         IF (EMEL_QHG(HG,CURIYR) .LT. 100.0)THEN
          CALL DROWTYPE(ROW_HG,'L       ',ROW_HG_mask)
          COEFF = DBLE((EMEL_QHG(HG,CURIYR) + BNK_TO_CMM_HG(CURIYR)) * 1000.0)

!     write(6,3456) curiyr+1989,hg,row_hg,emel_qhg(hg,curiyr),coeff
!3456 format(1h ,'!cmer',i4,i3,a10,f10.2)

          CALL DRHS(EFDRHS,ROW_HG,COEFF,ROW_HG_mask,'ED$COL,13')

! CREATE CAP ON HG CREDIT PRICE, IF APPROPRIATE

          IF (UCAP_HG .GT. 0.0)THEN
            COLUMN = 'MERCCAP' // UPRGCD(HG); call makmsk(COLUMN_mask,':MERCCAP:' , UPRGCD(HG))
            COEFF = DBLE(UCAP_HG * 0.001)
            CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$COL,14')
            CALL DVAL(COLUMN,ROW_HG,NEG1,COLUMN_mask,ROW_HG_mask,'ED$COL,15')
          END IF
         ELSE
          CALL DROWTYPE(ROW_HG,'N       ',ROW_HG_mask)

!     if (curitr .gt. maxitr .or. fcrl .eq. 1)  &
!     write(6,3456) curiyr+1989,hg,row_hg,emel_qhg(1,curiyr)

         END IF
        END DO

! ACI SUPPLY CURVE, IF HG LIMITS

        IF (USW_HG .GT. 0)THEN
            ROW_AC = 'ACICSTXX'; ROW_AC_mask='ACICSTXX'
            CALL DROWTYPE(ROW_AC,'L       ',ROW_AC_mask)
            COEFF = DBLE(0.0)
            CALL DRHS(EFDRHS,ROW_AC,COEFF,ROW_AC_mask,'ED$COL,16')

! SUPPLY STEPS

          DO ACI = 1 , NUM_ACSS
            WRITE(ACICODE,'(I1)') ACI
            COLUMN = 'ACISPLY' // ACICODE; call makmsk(COLUMN_mask,':ACISPLY:' , ACICODE)

! SET BOUND

           IF (ACI .LT. NUM_ACSS)THEN
            COEFF = DBLE(UCL_QAC(ACI))
            CALL DBND(EFDBND,COLUMN,0.0D0,COEFF,COLUMN_mask,'ED$COL,17')
           END IF

! PUT IN COEFFICIENT FOR OBJECTIVE FUNCTION

            COEFF = DBLE(UCL_PAC(ACI))
            CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$COL,18')

! PUT IN ACI SUPPLY ROW

            CALL DVAL(COLUMN,ROW_AC,NEG1,COLUMN_mask,ROW_AC_mask,'ED$COL,19')
          END DO

! PUT IN ACI SUPPLY ROW

            ROW_AR = 'ACIRMVXX' ; ROW_AR_mask='ACIRMVXX'
            CALL DROWTYPE(ROW_AR,'N       ',ROW_AR_mask)

! FREE ROW TO ACCUMULATE ACI O&M COSTS

            ROW_AO = 'ACIOMCST' ;ROW_AO_mask='ACIOMCST'
            CALL DROWTYPE(ROW_AO,'N       ',ROW_AO_mask)
        END IF
      END IF

! Contracts Row, Type and RHS

      DO CRV = 1 , MX_NCOALS + MX_ISCV
         IF (CRV .GT. MX_NCOALS)THEN
           XCL_TYPE(CRV)=1
        ENDIF
        IF (XCL_TYPE(CRV) .GT. 0)THEN
          WRITE(SC,'(I2.2)') CRV
          DO CRG = 1 , NDREG
              DO IFGD = 1 , 2
                IF (EFD_CONT(CRV,CRG,IFGD,CURIYR) .GE. 0.0) THEN
                  ROW_K = 'K_' // SC // EPFLCD(CRG) // CFGD(IFGD) // 'XX'; call makmsk(ROW_K_mask,':K_:' , SC , EPFLCD(CRG) , CFGD(IFGD) , ':XX:')
                  CALL DROWTYPE(ROW_K,'G       ',ROW_K_mask)

!  Escape Vector

                  COLUMN = 'KX' // SC // EPFLCD(CRG) // CFGD(IFGD) // 'XX'; call makmsk(COLUMN_mask,':KX:' , SC , EPFLCD(CRG) , CFGD(IFGD) , ':XX:')
                  CALL DVAL(COLUMN,EFDOBJ,15.0D0,COLUMN_mask,EFDOBJ,'ED$COL,20')
                  CALL DVAL(COLUMN,ROW_K,1.0D0,COLUMN_mask,ROW_K_mask,'ED$COL,21')
                  COEFF = DBLE(EFD_CONT(CRV,CRG,IFGD,CURIYR))
                  IF ((CURIYR + UHBSYR) .EQ. UESTYR .AND. CURITR .EQ. 1)COEFF = COEFF / DBLE(4.0)
                  CALL DRHS(EFDRHS,ROW_K,COEFF,ROW_K_mask,'ED$COL,22')
                END IF
              END DO
          END DO
        END IF
      END DO

! Diversity Rows, Type and RHS

      DO CRG = 1 , NDREG
        DO PLT = 1 , NUTSEC

!  Subbituminous

          IF (EFD_SB_DVLIM(CRG,PLT,CURIYR) .GE. 0.0)THEN
            ROW_DS = 'DV' // EPFLCD(CRG) // UPLNTCD(PLT) // 'S' // 'XX'; call makmsk(ROW_DS_mask,':DV:' , EPFLCD(CRG) , UPLNTCD(PLT) , 'S' , ':XX:')

!            IF (EFDMAT .EQ. 0)

            CALL DROWTYPE(ROW_DS,'L       ',ROW_DS_mask)

!d          IF (EFDMAT .EQ. 0)CALL DROWTYPE(ROW_DS,'N       ',ROW_DS_mask)

            COEFF = DBLE(EFD_SB_DVLIM(CRG,PLT,CURIYR))
            IF (CURCALYR .LE. UYR_HIST) COEFF = 9999.0                         !in historical years ignore diversity constraints because CMM not running
            CALL DRHS(EFDRHS,ROW_DS,COEFF,ROW_DS_mask,'ED$COL,23')

!   Escape Vector

!           IF (EFDMAT .EQ. 0)THEN

            COLUMN = 'DX' // EPFLCD(CRG) // UPLNTCD(PLT) // 'S' // 'XX'; call makmsk(COLUMN_mask,':DX:' , EPFLCD(CRG) , UPLNTCD(PLT) , 'S' , ':XX:')
            CALL DVAL(COLUMN,EFDOBJ,9.9D0,COLUMN_mask,EFDOBJ,'ED$COL,24')
            CALL DVAL(COLUMN,ROW_DS,NEG1,COLUMN_mask,ROW_DS_mask,'ED$COL,25')

!          END IF

          END IF

!  Lignite

          IF (EFD_LG_DVLIM(CRG,PLT,CURIYR) .GE. 0.0)THEN
            ROW_DL = 'DV' // EPFLCD(CRG) // UPLNTCD(PLT) // 'L' // 'XX'; call makmsk(ROW_DL_mask,':DV:' , EPFLCD(CRG) , UPLNTCD(PLT) , 'L' , ':XX:')

!            IF (EFDMAT .EQ. 0)

            CALL DROWTYPE(ROW_DL,'L       ',ROW_DL_mask)

!d          IF (EFDMAT .EQ. 0)CALL DROWTYPE(ROW_DL,'N       ')

            COEFF = DBLE(EFD_LG_DVLIM(CRG,PLT,CURIYR))
            IF (CURCALYR .LE. UYR_HIST) COEFF = 9999.0                     !in historical years ignore diversity constraints because CMM not running
            CALL DRHS(EFDRHS,ROW_DL,COEFF,ROW_DL_mask,'ED$COL,26')

!   Escape Vector
!           IF (EFDMAT .EQ. 0)THEN

            COLUMN = 'DX' // EPFLCD(CRG) // UPLNTCD(PLT) // 'L' // 'XX'; call makmsk(COLUMN_mask,':DX:' , EPFLCD(CRG) , UPLNTCD(PLT) , 'L' , ':XX:')
            CALL DVAL(COLUMN,EFDOBJ,9.9D0,COLUMN_mask,EFDOBJ,'ED$COL,27')
            CALL DVAL(COLUMN,ROW_DL,NEG1,COLUMN_mask,ROW_DL_mask,'ED$COL,28')

!           END IF

          END IF
        END DO
      END DO
!
! Set up Production Vectors - Domestic Supply Curves
!
      DO CRV = 1 , MX_NCOALS
       IF (XCL_TYPE(CRV) .GT. 0)THEN
          WRITE(SC,'(I2.2)') CRV
          CT = UPFLCD(XCL_TYPE(CRV))
          ROW_S = 'S_CL' // SC // CT; call makmsk(ROW_S_mask,':S_CL:' , SC , CT)
          CALL DROWTYPE(ROW_S,'L       ',ROW_S_mask)
          COEFF = DBLE(0.0)
          CALL DRHS(EFDRHS,ROW_S,COEFF,ROW_S_mask,'ED$COL,29')

          PRDLIM = DBLE(XCL_PCAP(CRV,CURIYR))                      !use same variable as in ECP 

!         PRDLIM = DBLE(CL_CRV_PROD(CRV,CURIYR))

!     write(6,1222) curiyr+1989,curitr,crv,map_sc_sr(crv),map_sc_cds(crv),ct,xcl(rank(crv),  &
!             xcl_btu_yr(crv,curiyr),xcl_so2_yr(crv,curiyr),xcl_hg_yr(crv,curiyr),xcl_car_yr(crv,curiyr)
!1222 format(1h ,'!ccrv',i4,i3,i3,i3,i3,a3,i3,4f10.4)

! SET UP SUPPLY STEPS

        DO STP = 1 , 11
          WRITE(ST,'(I2.2)') STP
          COLUMN = 'CP' // SC // CT // ST; call makmsk(COLUMN_mask,':CP:' , SC , CT , ST)

! BOUND

          IF (STP .GT. 1)THEN
            STSIZE = XCL_STEPS(STP) - XCL_STEPS(STP - 1)
          ELSE
            STSIZE = 1.0 + XCL_STEPS(STP)
          END IF
          COEFF = DBLE(XCL_QECP(CRV,0,CURIYR) * STSIZE)
          COEFF = MIN(COEFF,PRDLIM)
          COEFF = MAX(COEFF,0.0D0)

!     write(6,1234) curiyr+1989,curitr,crv,stp,column,prdlim,xcl_qecp(crv,0,curiyr),stsize,coeff,xcl_pecp(crv,stp,0,curiyr)
!1234 format(1h ,'!cstp',i4,i3,i3,i3,1x,a8,4f10.4,f10.5)

          PRDLIM = PRDLIM - COEFF
          CALL DBND(EFDBND,COLUMN,0.0D0,COEFF,COLUMN_mask,'ED$COL,30')

! PUT IN COEFFICIENT FOR OBJECTIVE FUNCTION

          COEFF = DBLE(XCL_PECP(CRV,STP,0,CURIYR))
          COEFF = MIN(COEFF,9.999D0)
          CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$COL,31')

! PUT IN COEFFICIENT FOR SUPPLY ROW

          CALL DVAL(COLUMN,ROW_S,NEG1,COLUMN_mask,ROW_S_mask,'ED$COL,32')
        END DO                                                 !STP

! SET UP ESCAPE VECTOR
! Note: moved from below non-utility coal code below.
! It appears coeff will have now have a value assigned from step=11 in above loop, so this needs review 

        STP=12
        WRITE(ST,'(I2.2)') STP
        COLUMN = 'CP' // SC // CT // ST; call makmsk(COLUMN_mask,':CP:' , SC , CT , ST)

        COEFF = MAX(DBLE(3.0) * COEFF,9.999D0) ! <== note: should assign something much bigger than 9.9 to act as safety.
        CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$COL,33')
        CALL DVAL(COLUMN,ROW_S,NEG1,COLUMN_mask,ROW_S_mask,'ED$COL,34')

! ACCOUNT FOR NON-UTILITY COAL

        COLUMN = 'CP' // SC // 'OTXX'; call makmsk(COLUMN_mask,':CP:' , SC , ':OTXX:')
        COEFF = DBLE(XCL_OTHER(CRV,CURIYR))
        CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$COL,35')

! PUT IN COEFFICIENT FOR SUPPLY ROW

        CALL DVAL(COLUMN,ROW_S,1.0D0,COLUMN_mask,ROW_S_mask,'ED$COL,36')

!     write(6,1111) curiyr+1989,curitr,row,xcl_other(crv,curiyr),xcl_pcap(crv,curiyr)
!1111 format(1h ,'!cdat',i4,i2,a10,2f10.4)

!     write(6,2222) curiyr+1989,curitr,row,(xcl_pecp(crv,stp,0,curiyr),stp=1,11)
!2222 format(1h ,'!cprc',i4,i2,a10,11f10.4)

!     write(6,3333) curiyr+1989,curitr,row,(xcl_qecp(crv,0,curiyr)*tmpstp(stp),stp=1,11),xcl_qecp(crv,0,curiyr)
!3333 format(1h ,'!cqty',i4,i2,a10,12f10.4)
       END IF                                                  !CTY
      END DO                                                   !CRV
!
!     Set up Production Vectors - International Supply Curves
!
      DO ICRV = 1 , MX_ISCV
         CRV = ICRV + MX_NCOALS
         IF (XCL_TYPE(CRV) .GT. 0)THEN
            WRITE(SC,'(I2.2)') CRV
            CT = UPFLCD(XCL_TYPE(CRV))
            ROW_S = 'S_CL' // SC // CT; call makmsk(ROW_S_mask,':S_CL:' , SC , CT)
            CALL DROWTYPE(ROW_S,'L       ',ROW_S_mask)
            COEFF = DBLE(0.0)
            CALL DRHS(EFDRHS,ROW_S,COEFF,ROW_S_mask,'ED$COL,37')

!           SET UP SUPPLY STEPS

            DO STP = 1 , 10
               IF (XCL_QIMP(ICRV,STP,CURIYR) .GT. 0.0) THEN
                  WRITE(ST,'(I2.2)') STP
                  COLUMN = 'CP' // SC // CT // ST; call makmsk(COLUMN_mask,':CP:' , SC , CT , ST)

!                 BOUND

                  COEFF = XCL_QIMP(ICRV,STP,CURIYR)

                  CALL DBND(EFDBND,COLUMN,0.0D0,COEFF,COLUMN_mask,'ED$COL,38')

!                 PUT IN COEFFICIENT FOR OBJECTIVE FUNCTION

                  COEFF = XCL_PIMP(ICRV,STP,CURIYR)
                  CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$COL,39')

!                 PUT IN COEFFICIENT FOR SUPPLY ROW

                  CALL DVAL(COLUMN,ROW_S,NEG1,COLUMN_mask,ROW_S_mask,'ED$COL,40')
               END IF
            END DO                                                 !STP

!           ACCOUNT FOR NON-UTILITY COAL

            COLUMN = 'CP' // SC // 'OTXX'; call makmsk(COLUMN_mask,':CP:' , SC , ':OTXX:')
            COEFF = DBLE(XCL_OTHER(CRV,CURIYR))
            CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$COL,41')

!           PUT IN COEFFICIENT FOR SUPPLY ROW

            CALL DVAL(COLUMN,ROW_S,1.0D0,COLUMN_mask,ROW_S_mask,'ED$COL,42')
         END IF                                                  !CTY
      END DO                                                   !CRV
!
! Set up Transportation Vectors
!
      CP_CRG = 0.0
      DO FRG = 1 , UNFRGN
         DO PLT = 1 , NUTSEC
            IF (CPFLECP(PLT,EPCSMP(FRG),EPCLMP(FRG),EPGSMP(FRG)) .GT. 0.0) THEN

               IF (EPCLMP(FRG) .GT. NDREG .OR. EPCLMP(FRG) .LE. 0) WRITE(6,2301) CURIYR+UHBSYR,CURITR,PLT,FRG,EPCSMP(FRG),EPCLMP(FRG),EPGSMP(FRG)
 2301          FORMAT(1X,"WHAT_IS_THIS_FRG",7(":",I4))

               CP_CRG(PLT,EPCLMP(FRG)) = CPFLECP(PLT,EPCSMP(FRG),EPCLMP(FRG),EPGSMP(FRG)) + CP_CRG(PLT,EPCLMP(FRG))
            END IF
         END DO
      END DO

      EFD_SC_DR_PT = 0

      DO CRG = 1 , NDREG
          WRITE(CL_CD,'(I2.2)') CRG

!jj       SO2 = SO2_GRP_BY_CLRG(CRG)
!jj       WRITE(SO2CODE,'(I1)') SO2
!jj       ROW_SO2 = 'SULFUR' // SO2CODE

         HG = HG_GRP(CRG)

         COEFFS = 0.0

         DO CRV = 1 , MX_NCOALS + MX_ISCV
            IF (CRV .GT. MX_NCOALS)THEN
               TOT_TYPE(CRV) = 1
            ENDIF

            IF (TOT_RATE1(CRV,CRG,0) .LT. 800.0 .AND. TOT_TYPE(CRV) .GT. 0) THEN
               DO PLT = 1 , NUTSEC
                  IF (CP_CRG(PLT,CRG) .GT. 0.0) THEN
                     EFD_SC_DR_PT(CRV,CRG,PLT) = 1

                     IF (CURITR .EQ. 1) THEN
                        WRITE(18,2531) CURIRUN, CURIYR+UHBSYR, CURITR, CRG, CRV, PLT, EFD_SC_DR_PT(CRV,CRG,PLT), TOT_TYPE(CRV), TOT_RATE1(CRV,CRG,0), CP_CRG(PLT,CRG)
 2531                   FORMAT(1X,"EFD_SC_DR_PT",8(":",I5),2(":",F20.6))
                     END IF

                  END IF
               END DO
            END IF

!           IF (CRV .GT. MX_NCOALS .AND. TOT_TYPE(CRV) .GT. 0) THEN

!           DO PLT = 1 , NUTSEC
!              WRITE(6,2531) CURIRUN, CURIYR+UHBSYR, CURITR, CRG, CRV, PLT, EFD_SC_DR_PT(CRV,CRG,PLT), TOT_TYPE(CRV), TOT_RATE1(CRV,CRG,0), CP_CRG(PLT,CRG)

!              IF (CURIRUN .EQ. 1 .AND. CP_CRG(PLT,CRG) .GT. 0.0) THEN
!                 EFD_SC_DR_PT(CRV,CRG,PLT) = 1
!              END IF
!           END DO

!           END IF

            IF (TOT_TYPE(CRV) .GT. 0)THEN

! Identify Transportation Links between Supply and Demand Regions

                  WRITE(SC,'(I2.2)') CRV
                  CT = UPFLCD(XCL_TYPE(CRV))
               IF (CRV .LE. MX_NCOALS)THEN
                  RANK = EFD_RANK(CRV)
                  RANK_TYPE = CT(1:1)
                  IF (RANK .EQ. 0) THEN
                     IF (RANK_TYPE .EQ. "B") THEN
                        RANK = 1
                     ELSE IF (RANK_TYPE .EQ. "S") THEN
                        RANK = 2
                     ELSE
                        RANK = 3
                     END IF
                     EFD_RANK(CRV) = RANK
                        WRITE(6,3351) CURIRUN, CURIYR+1989, CURITR, RANK, CRV, CT, CRG
 3351                   FORMAT(1X,"BAD_RANK",5(":",I5),":",A3,":",I5)
                  END IF
                  ELSE
                     RANK = 1
                  ENDIF
                  ROW_S = 'S_CL' // SC // CT; call makmsk(ROW_S_mask,':S_CL:' , SC , CT)
                  IF (EFDMAT .EQ. 0)THEN
                     CALL DROWTYPE(ROW_S,'L       ',ROW_S_mask)
                     CALL DRHS(EFDRHS,ROW_S,0.0D0,ROW_S_mask,'ED$COL,43')
                  END IF
                  DO PLT = 1 , NUTSEC

!                 IF (CRV .LE. MX_NCOALS) THEN
!                    IF (CURIRUN .EQ. 1) THEN
!                       XCL_TRNINDX(PLT,CRV,CRG) = 1
!                       IF (CURIYR+1989 .GE. 2015) THEN
!                          IF (PLT .EQ. WIB1 .OR. &
!                              PLT .EQ. WIB2 .OR. &
!                              PLT .EQ. WIC1 .OR. &
!                              PLT .EQ. WIC2 .OR. & 
!                              PLT .EQ. WIC3 .OR. & 
!                              PLT .EQ. WIH1 .OR. & 
!                              PLT .EQ. WIH2 .OR. &
!                              PLT .EQ. WIH3) THEN
!                             XCL_TRNINDX(PLT,CRV,CRG) = 0
!                             IF (CRV .EQ.  7 .OR. &
!                                 CRV .EQ.  9 .OR. &
!                                 CRV .EQ. 12 .OR. &
!                                 CRV .EQ. 14 .OR. &
!                                 CRV .EQ. 15 .OR. &
!                                 CRV .EQ. 26 .OR. &
!                                 CRV .EQ. 27 .OR. &
!                                 CRV .EQ. 29 .OR. &
!                                 CRV .EQ. 31 .OR. &
!                                 CRV .EQ. 32 .OR. &
!                                 CRV .EQ. 33 .OR. &
!                                 CRV .EQ. 35 .OR. &
!                                 CRV .EQ. 36 .OR. &
!                                 CRV .EQ. 37 .OR. &
!                                 CRV .EQ. 39 .OR. &
!                                 CRV .EQ. 41 )THEN
!                                XCL_TRNINDX(PLT,CRV,CRG) = 1
!                             END IF
!                          END IF
!                       END IF
!                    END IF
!                 END IF

!                     IF (CP_CRG(PLT,CRG) .GT. 0.0 .AND. CRV .LE. MX_NCOALS) THEN
                      IF (CP_CRG(PLT,CRG) .GT. 0.0) THEN
! Check if Coal From Supply Curve Can be Used in Plant

                        IF (EFD_SC_DR_PT(CRV,CRG,PLT) .GT. 0)THEN
                           HTRT = HRTCLNR(CRG,CURIYR,PLT) / 1000.0
                           SO2RMV = (1.0 - RCLCLNR(CRG,CURIYR,PLT))
                           ROW_C = 'CL' // CL_CD // UPLNTCD(PLT); call makmsk(ROW_C_mask,':CL:' , CL_CD , UPLNTCD(PLT))
                           IF (EFDMAT .EQ. 0)THEN
                              CALL DROWTYPE(ROW_C,'L       ',ROW_C_mask)
                              CALL DRHS(EFDRHS,ROW_C,0.0D0,ROW_C_mask,'ED$COL,44')
                           END IF
                           IFGD = ECP_SCRUB(PLT,CURIYR)
                           ROW_T = 'T_' // SC // EPFLCD(CRG) // '_' // CFGD(IFGD) // 'Z'; call makmsk(ROW_T_mask,':T_:' , SC , EPFLCD(CRG) , ':_:' , CFGD(IFGD) , ':Z:')
                           ROW_K = 'K_' // SC // EPFLCD(CRG) // CFGD(IFGD) // 'XX'; call makmsk(ROW_K_mask,':K_:' , SC , EPFLCD(CRG) , CFGD(IFGD) , ':XX:')
                           ROW_DS = 'DV' // EPFLCD(CRG) // UPLNTCD(PLT) // 'S' // 'XX'; call makmsk(ROW_DS_mask,':DV:' , EPFLCD(CRG) , UPLNTCD(PLT) , 'S' , ':XX:')
                           ROW_DL = 'DV' // EPFLCD(CRG) // UPLNTCD(PLT) // 'L' // 'XX'; call makmsk(ROW_DL_mask,':DV:' , EPFLCD(CRG) , UPLNTCD(PLT) , 'L' , ':XX:')
!
! Calculate ACI Requirements
!
                           ICLS = HG_CLASS(PLT)
                           NMAX = NSTEP

                        IF (RANK .EQ. 0) THEN
                        END IF

                           N = ACI_STEPS(PLT,RANK)
                           CHOICE = HG_CHOICE(PLT)
                           OUT = HG_OUTPUT(ICLS,RANK,CURIYR)
                           HR = HRTCLNR(CRG,CURIYR,PLT)
                           IN = HG_INPUT(ICLS,RANK,CURIYR)
                           PRCNT = HG_MEF(ICLS,RANK,CURIYR)
                           LIM = EMLIM(4,CURIYR)
                           GRAM = HG_GRAMS_MWH(ICLS,CURIYR)
                           RHG = XCL_HG_YR(CRV,CURIYR)
                           EMF_P = PLNT_EMF(PLT,RANK)
                           EMF_M = MIN_EMF(PLT,RANK)
                           F_FGD = FGD_FCTR(PLT,RANK)
                           F_SCR = SCR_FCTR(PLT,RANK)
                           A = PARM_A(PLT,RANK)
                           B = PARM_B(PLT,RANK)
                           C = PARM_C(PLT,RANK)
                           D = PARM_D(PLT,RANK)
                           IF (ACI_CST(1,RANK,PLT) .EQ. 0.0) Allowed(1) = 1
!                       IF (CURIRUN .EQ. 1) Allowed(1) = 1

!                          If No CAMR, Set Requirement to Specified Standard or Plant EMF if None

                           IF (USW_CAMR .GT. 0)THEN
                              MACT_EMF = MIN(HG_MEFNC(CRG,CURIYR),EMF_P)
                           END IF
!
                           CALL GET_EMF_AND_ACI(NMAX,N,CHOICE,OUT,HR,IN,PRCNT,LIM,GRAM,RHG,EMF_P,F_FGD,F_SCR,A,B,C,D,EMF_M,Allowed,EMF_T,RACI,MACT_SW,MACT_EMF)
                           DO ACI = 1 , NUM_ACI + 1
                              EMM_MEF(ACI,RANK,PLT) = EMF_T(ACI)
                              ACI_QAC(ACI,RANK,PLT) = RACI(ACI)

!                          COLUMN = 'CT' // SC // EPFLCD(CRG) // UPLNTCD(PLT) // ACICD(ACI)
!                          IF (CRV .LE. MX_NCOALS) THEN
!                             WRITE(6,4443) CURIRUN, CURIYR+1989, PLT, ACI, RANK, CRG, CRV, ACI_OPT(ACI,RANK,PLT,CURIYR), ALLOWED(1), XCL_TRNINDX(PLT,CRV,CRG), &
!                                COLUMN, TOT_RATE1(CRV,CRG,0), TOT_RATE2(CRV,CRG,0), ACI_CST(1,RANK,PLT)
!                          ELSE
!                             WRITE(6,4443) CURIRUN, CURIYR+1989, PLT, ACI, RANK, CRG, CRV, ACI_OPT(ACI,RANK,PLT,CURIYR), ALLOWED(1), 0.0, &
!                                COLUMN, TOT_RATE1(CRV,CRG,0), TOT_RATE2(CRV,CRG,0), ACI_CST(1,RANK,PLT)
!                          END IF
!4443                      FORMAT(1X,"EFD_CT",10(":",I5),":",A16,3(":",F21.6))


!     if (curitr .eq. 1)  &
!     write(6,7777) curiyr+1989,nstep,num_aci,frg,crg,crv,plt,uplntcd(plt),rank, &
!                   hrtclnr(crg,curiyr,plt),  &
!                   aci_opt(aci,rank,plt,curiyr),  &
!                   emm_mef(aci,rank,plt),  &
!                   aci_cst(aci,rank,plt),  &
!                   aci_qac(aci,rank,plt)
!7777 format(1h ,'!acidat',i4,i3,i3,i3,i3,i3,i3,a3,i3,f10.3,i3,3f10.3)

                              IF (ACI_OPT(ACI,RANK,PLT,CURIYR) .GT. 0)THEN

!                    HGEMF = EMM_MEF(ACI,RANK,PLT)

                                 COLUMN = 'CT' // SC // EPFLCD(CRG) // UPLNTCD(PLT) // ACICD(ACI); call makmsk(COLUMN_mask,':CT:' , SC , EPFLCD(CRG) , UPLNTCD(PLT) , ACICD(ACI))

!                                SET NO ACI VECTOR TO 0 WITHOUT CAMR IF ACI NEEDED FOR MACT

                                 IF (Allowed(ACI) .EQ. 2)THEN
                                    COEFF = DBLE(0.0)
!                                IF (CURIRUN .GT. 1) THEN
                                    CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$COL,45')
!                                END IF
                                 END IF

                                 IF (CRV .LE. MX_NCOALS) THEN
                                    IF (EFD_TIER1_LIM(CRV,CRG,IFGD,CURIYR) .GT. 0.0 .AND. PLT .LT. WIPC) THEN
                                       COEFF = DBLE(TOT_RATE1(CRV,CRG,0) )
                                    ELSE
                                       COEFF = DBLE(TOT_RATE2(CRV,CRG,0))
                                    END IF
                                 ELSE
                                    COEFF = DBLE(TOT_RATE1(CRV,CRG,0))
                                 END IF

!     if (curitr .gt. maxitr .or. fcrl .eq. 1) &
!     write(6,4444) curiyr+1989,frg,crg,crv,plt,xcl_sc_dr_pt(crv,crg,plt),column,row_s,coeff,so2rmv,hgemf
!4444 format(1h ,'!ctrn1',i4,i3,i3,i3,i3,i3,a10,a10,3f10.3)

! ADD ACI O&M COST TO OBJ

                                 IF (ACI .GT. 1)THEN
                                    COEFF = COEFF + DBLE(ACI_CST(ACI,RANK,PLT) / HTRT)

! ACCULULATE ACI O&M COSTS

                                    CALL DVAL(COLUMN,ROW_AO,DBLE(ACI_CST(ACI,RANK,PLT) / HTRT),COLUMN_mask,ROW_AO_mask,'ED$COL,46')
                                 ENDIF

! ADD CARBON COST TO OBJ
! not with explicit constraint

                                 IF ((USW_CAR .NE. 2 .AND. USW_CAR .NE. 3) .AND. EMETAX(1,CURIYR) .GT. 0.0)THEN

!                                   COEFF = COEFF + JCLCLNR(CURIYR,PLT) * (1.0 - UPPCEF(PLT))

                                    IF ((TAX_FLAG /= 0) .OR. (PERMIT_FLAG /= 0)) THEN
                                       COEFF = COEFF + UPNCAR(PLT,CRG) * (1.0 - UPPCEF(PLT))
                                    ENDIF
                                 ENDIF

                                 CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$COL,47')

! PUT IN PRODUCTION, COAL SUPPLY, AND COAL TRANSPORTATION ROW COEFFICIENTS

                                 IF (EFDMAT .EQ. 0)THEN
                                    CALL DVAL(COLUMN,ROW_S,1.0D0,COLUMN_mask,ROW_S_mask,'ED$COL,48')
                                    CALL DVAL(COLUMN,ROW_C,NEG1,COLUMN_mask,ROW_C_mask,'ED$COL,49')
                                 END IF

                                 IF (TOT_RATE2(CRV,CRG,0) .GT. TOT_RATE1(CRV,CRG,0) .AND. CRV .LE. MX_NCOALS) THEN
                                    IF (EFD_TIER1_LIM(CRV,CRG,IFGD,CURIYR) .GT. 0.0 .AND. PLT .LT. WIPC) THEN
                                       CALL DVAL(COLUMN,ROW_T,1.0D0,COLUMN_mask,ROW_T_mask,'ED$COL,50')
                                    END IF
                                 END IF

! PUT IN CONTRACTS ROW

                                    IF (EFD_CONT(CRV,CRG,IFGD,CURIYR) .GE. 0.0)THEN

!                                       IF (EFDMAT .EQ. 0)THEN

                                          CALL DVAL(COLUMN,ROW_K,1.0D0,COLUMN_mask,ROW_K_mask,'ED$COL,51')

!                                       END IF

                                 END IF

! PUT IN SUBBITUMINOUS DIVERSITY ROW

                                 IF (EFD_SB_DVLIM(CRG,PLT,CURIYR) .GE. 0.0 .AND. RANK .EQ. 2)THEN
                                    IF (EFDMAT .EQ. 0)THEN
                                       CALL DVAL(COLUMN,ROW_DS,1.0D0,COLUMN_mask,ROW_DS_mask,'ED$COL,52')
                                    END IF
                                 END IF

! PUT IN LIGNITE DIVERSITY ROW

                                 IF (EFD_LG_DVLIM(CRG,PLT,CURIYR) .GE. 0.0 .AND. RANK .EQ. 3)THEN
                                    IF (EFDMAT .EQ. 0)THEN
                                       CALL DVAL(COLUMN,ROW_DL,1.0D0,COLUMN_mask,ROW_DL_mask,'ED$COL,53')
                                    END IF
                                 END IF

! PUT IN SO2 AND HG ROWS

                                 DO SO2 = 1 , NUM_SO2_GRP

                                    IF ((CURIYR + UHBSYR) .LT. TRANRULE1)THEN
                                       SO2SHR = SO2_SHR_BY_CLRG(CRG,SO2)
                                    ELSE
                                       CALL EPA$TRANSHR(CRG,PLT,SO2,SO2SHR)
                                    END IF

!                                   IF (SO2_SHR_BY_CLRG(CRG,SO2) .GT. 0.0)THEN

                                    IF (SO2SHR .GT. 0.0001)THEN
                                       WRITE(SO2CODE,'(I1)') SO2
                                       ROW_SO2_PT = 'SULF' // UPLNTCD(PLT) // SO2CODE; call makmsk(ROW_SO2_PT_mask,':SULF:' , UPLNTCD(PLT) , SO2CODE)

!                                      COEFF = DBLE(XCL_SO2_YR(CRV,CURIYR) * SO2RMV * SO2_SHR_BY_CLRG(CRG,SO2) * 0.5)

                                       COEFF = DBLE(XCL_SO2_YR(CRV,CURIYR) * SO2RMV * SO2SHR * 0.5)
                                       IF (COEFF .GT. EFD_MIN) THEN
                                          CALL DVAL(COLUMN,ROW_SO2_PT,COEFF,COLUMN_mask,ROW_SO2_PT_mask,'ED$COL,54')
                                          TST_SO2(PLT,SO2) = 1
                                       END IF
                                    END IF
                                    IF (SO2 .EQ. 1)THEN
                                       CLSCPLDR(CRV,PLT,CRG) = CLSCPLDR(CRV,PLT,CRG) + 1
                                       IF (ACI .EQ. 1)ACI0 = ACI0 + 1
                                       if (ACI .EQ. 2)ACI1 = ACI1 + 1
                                    END IF
                                 END DO

!                                STATE/COAL REGION LEVEL CONSTRAINTS FOR EPA TRANSPORT RULE

!                                IF (TSO2_YR_BY_ST .LT. 9999 .OR. TSO2_YR_BY_CLRG .LT. 9999)THEN

                                 IF (TSO2_YR_BY_CLRG .LT. 9999)THEN
                                    COEFF = DBLE(XCL_SO2_YR(CRV,CURIYR) * SO2RMV * 0.5)
                                    IF (COEFF .GT. ECP_MIN)THEN

                                       save_row_mask=row_mask
!                                      IF (TSO2_YR_BY_CLRG .LT. 9999)CALL EPA$TRANRG_EFD(11,1,CRG,PLT,COLUMN,COLUMN_mask,COEFF)
                                       CALL EPA$TRANRG_EFD(11,1,CRG,PLT,COLUMN,COLUMN_mask,COEFF)
                                       row_mask=save_row_mask

                                    END IF
                                 END IF

!     if (curitr .gt. maxitr .or. fcrl .eq. 1) &
!     write(6,4445) curiyr+1989,frg,crg,crv,plt,column,row_so2,coeff
!4445 format(1h ,'!cso21',i4,i3,i3,i3,i3,a10,a10,3f10.3)

                                 COEFF = DBLE(XCL_HG_YR(CRV,CURIYR) * EMM_MEF(ACI,RANK,PLT) * 0.5)

!     if (curitr .gt. maxitr .or. fcrl .eq. 1) &
!     write(6,4446) curiyr+1989,frg,crg,crv,plt,column,row_hg,coeff
!4446 format(1h ,'!cmer1',i4,i3,i3,i3,i3,a10,a10,3f10.3)

                                 IF (COEFF .GT. EFD_MIN) THEN
                                    ROW_HG_PT = 'MERC_' // UPLNTCD(PLT) // UPRGCD(HG); call makmsk(ROW_HG_PT_mask,':MERC_:' , UPLNTCD(PLT) , UPRGCD(HG))
                                    CALL DVAL(COLUMN,ROW_HG_PT,COEFF,COLUMN_mask,ROW_HG_PT_mask,'ED$COL,55')
                                    TST_HG(PLT,HG) = 1
                                 END IF

! Mercury MACT EMISSIONS CONSTRAINTS

                                 IF (MACT_SW .EQ. 1) THEN
                                     ROW_HGO = 'HGO_' // EPFLCD(CRG) // UPLNTCD(PLT); call makmsk(ROW_HGO_mask,':HGO_:' , EPFLCD(CRG) , UPLNTCD(PLT))
                                     CALL DROWTYPE(ROW_HGO,'L       ',ROW_HGO_mask)
                                     COEFF = DBLE(0.5) * XCL_HG_YR(CRV,CURIYR) * (EMM_MEF(ACI,RANK,PLT) - MACT_EMF)
                                     IF (COEFF .NE. 0.0) CALL DVAL(COLUMN,ROW_HGO,COEFF,COLUMN_mask,ROW_HGO_mask,'ED$COL,55.1')
                                     IF (COEFF .LE. neg1*0.0001D0 .OR. COEFF .GE. 0.0001D0) CALL DVAL(COLUMN,ROW_HGO,COEFF,COLUMN_mask,ROW_HGO_mask,'ED$COL,55.2')
                                 END IF

! Carbon row

                                    ROWCAR = 'CARBONXX';ROWCAR_MASK='CARBONXX'
                                    COEFF = (1.0 / 2204.0) * XCL_CAR_YR(CRV,CURIYR) * (12.0/44.0)
                                    IF (COEFF .GT. EFD_MIN) THEN
                                       CALL DVAL(COLUMN,ROWCAR,COEFF,COLUMN_mask,ROWCAR_mask,'ED$COL,56')
                                    ENDIF

! ACI ROWS

                                 IF (ACI .GT. 1)THEN
                                    COEFF = DBLE(ACI_QAC(ACI,RANK,PLT) * 0.000001)
                                    COEFF = MAX(COEFF,0.0001D0)
                                    CALL DVAL(COLUMN,ROW_AC,COEFF,COLUMN_mask,ROW_AC_mask,'ED$COL,57')
                                    COEFF = DBLE(XCL_HG_YR(CRV,CURIYR) * (EMM_MEF(1,RANK,PLT) - EMM_MEF(ACI,RANK,PLT)) * 0.5)
                                    CALL DVAL(COLUMN,ROW_AR,COEFF,COLUMN_mask,ROW_AR_mask,'ED$COL,58')
                                 END IF
!                              IF (CRV .LE. MX_NCOALS)THEN
!                                 IF (XCL_TRNINDX(PLT,CRV,CRG) .EQ. 0)THEN
!                                    CALL DBND(EFDBND,COLUMN,DBLE(0.0),DBLE(0.0),COLUMN_mask,'ED$COL,59')
!                                 END  IF
!                                 END  IF
                              END IF
                           END DO                                          !ACI

! Create Vector to Limit Coal Available at 1st Tier Transportation Rates

                           IF (CRV .LE. MX_NCOALS) THEN
                              IF (XCL_TR_T2(CRV,CRG,CURIYR) .GT. XCL_TR_T1(CRV,CRG,CURIYR) .AND. EFD_TIER1_LIM(CRV,CRG,IFGD,CURIYR) .GT. 0.0 .AND. PLT .LT. WIPC) THEN

                                 CALL DROWTYPE(ROW_T,'L       ',ROW_T_mask)
                                 COEFF = DBLE(EFD_TIER1_LIM(CRV,CRG,IFGD,CURIYR))
                                 CALL DRHS(EFDRHS,ROW_T,COEFF,ROW_T_mask,'ED$COL,60')
                                 COLUMN = 'C_' // SC // EPFLCD(CRG) // '_' // CFGD(IFGD) // 'Z'; call makmsk(COLUMN_mask,':C_:' , SC , EPFLCD(CRG) , ':_:' , CFGD(IFGD) , ':Z:')
                                 COEFF = DBLE(XCL_TR_T2(CRV,CRG,CURIYR)*XCL_2TESC(CRV,0,CURIYR,CRG) - XCL_TR_T1(CRV,CRG,CURIYR)*XCL_1TESC(CRV,0,CURIYR,CRG))
                                 COEFF = MAX(0.0001 , COEFF)

!     if (curitr .gt. maxitr .or. fcrl .eq. 1) &
!     write(6,5555) curiyr+1989,frg,crg,crv,plt,column,coeff,XCL_TR_T2(CRV,CRG,CURIYR),XCL_TR_T1(CRV,CRG,CURIYR),efd_tier1_lim(crv,crg,IFGD,curiyr)
!5555 format(1h ,'!ctrn2',i4,i3,i3,i3,i3,a10,4f10.3)

                                 CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$COL,61')
                                 IF (EFDMAT .EQ. 0)THEN
                                    CALL DVAL(COLUMN,ROW_T,NEG1,COLUMN_mask,ROW_T_mask,'ED$COL,62')
                                 END IF
                              END IF
                           END IF
                        END IF
                     END IF
                  END DO                                          !PLT

!                 account for BTU from CTL

                  IF (CRV .LE. MX_NCOALS) THEN
                     IF (CTLBTU(CRV,CRG,CURIYR) .GT. 0.0) THEN
                       CTLPLT = WIIG   !IGCC plt characteristics

!                       PUT IN SO2 ROWS

                        DO SO2 = 1 , NUM_SO2_GRP

                           IF ((CURIYR + UHBSYR) .LT. TRANRULE1)THEN
                              SO2SHR = SO2_SHR_BY_CLRG(CRG,SO2)
                           ELSE
                              CALL EPA$TRANSHR(CRG,CTLPLT,SO2,SO2SHR)
                           END IF

!                          IF (SO2_SHR_BY_CLRG(CRG,SO2) .GT. 0.0)THEN

                           IF (SO2SHR .GT. 0.0001)THEN
                              WRITE(SO2CODE,'(I1)') SO2
                              ROW_SO2 = 'SULFUR' // SO2CODE; call makmsk(ROW_SO2_mask,':SULFUR:' , SO2CODE)
                              COLUMN = 'CTL' // SC // EPFLCD(CRG) // 'S' // SO2CODE; call makmsk(COLUMN_mask,':CTL:' , SC , EPFLCD(CRG) , ':S:' , SO2CODE)
                              SO2RMV = (1.0 - RCLCLNR(CRG,CURIYR,CTLPLT))

!                             COEFF = CTLBTU(CRV,CRG,CURIYR) * DBLE(XCL_SO2_YR(CRV,CURIYR) * SO2RMV * SO2_SHR_BY_CLRG(CRG,SO2) * 0.5)

                              COEFF = CTLBTU(CRV,CRG,CURIYR) * DBLE(XCL_SO2_YR(CRV,CURIYR) * SO2RMV * SO2SHR * 0.5)
                              IF (COEFF .GT. 0.0) THEN
                                 CALL DVAL(COLUMN,ROW_SO2,1.0D0,COLUMN_mask,ROW_SO2_mask,'ED$COL,63')
                                 CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$COL,64')
                              END IF
                           END IF
                        END DO

!                       COAL REGION CONSTRAINTS FOR EPA TRANSPORT RULE - SUM OVER CURVES HERE, PUT IN CONSTRAINT OUTSIDE THIS LOOP

                        IF (TSO2_YR_BY_CLRG .LT. 9999)THEN
                           SO2RMV = (1.0 - RCLCLNR(CRG,CURIYR,CTLPLT))
                           COEFFS = COEFFS + CTLBTU(CRV,CRG,CURIYR) * DBLE(XCL_SO2_YR(CRV,CURIYR) * SO2RMV * 0.5)
                        END IF

!                       PUT IN HG ROW

                        COLUMN = 'CTL' // SC // EPFLCD(CRG) // 'HG'; call makmsk(COLUMN_mask,':CTL:' , SC , EPFLCD(CRG) , ':HG:')
                        ROW_HG = 'MERCURY' // UPRGCD(HG); call makmsk(ROW_HG_mask,':MERCURY:' , UPRGCD(HG))
                        RANK = EFD_RANK(CRV)
                        COEFF = CTLBTU(CRV,CRG,CURIYR) * DBLE(XCL_HG_YR(CRV,CURIYR) * PLNT_EMF(CTLPLT,RANK) * 0.5)
                        IF (COEFF .GT. EFD_MIN) THEN
                           CALL DVAL(COLUMN,ROW_HG,1.0D0,COLUMN_mask,ROW_HG_mask,'ED$COL,65')
                           CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$COL,66')
                        END IF

!                       PUT IN CARBON ROW

                        COLUMN = 'CTL'//SC//EPFLCD(CRG)//'CO'; call makmsk(COLUMN_mask,':CTL:',SC,EPFLCD(CRG),':CO:')
                     COEFF = CTLBTU(CRV,CRG,CURIYR) * (1.0 / 2204.0) * XCL_CAR_YR(CRV,CURIYR) * (12.0/44.0) * (1.0 - UPPCEF(CTLPLT))
                        IF (COEFF .GT. EFD_MIN) THEN
                             CALL DVAL(COLUMN,ROWCAR,1.0D0,COLUMN_mask,ROWCAR_mask,'ED$COL,67')
                             CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$COL,68')

!                       REGIONAL CARBON ROWS

                             DO FRG = 1 , UNFRGN
                                IF (CLCONFC(CRG,FRG) .GT. 0.0)THEN
                                   GRG = EPCAMP(FRG)
                                   ROWCARR = 'CARBON' // CO2_RG(GRG); call makmsk(ROWCARR_mask,':CARBON:' , CO2_RG(GRG))
                                   COEFF = DBLE(CLCONFC(CRG,FRG))
                                   CALL DVAL(COLUMN,ROWCARR,COEFF,COLUMN_mask,ROWCARR_mask,'ED$COL,69')
                                END IF
                             END DO
                        ENDIF
                     END IF

!                    COAL REGION CONSTRAINTS FOR EPA TRANSPORT RULE                  
                      IF (CRV .EQ. MX_NCOALS .AND. COEFFS .GT. ECP_MIN)THEN
                          COLUMN = 'CTL' // 'SO2' // EPFLCD(CRG); call makmsk(COLUMN_mask,':CTL:' , ':SO2:' , EPFLCD(CRG))
                          CALL DBND(EFDBND,COLUMN,1.0D0,1.0D0,COLUMN_mask,'ED$COL,64.1')
                          save_row_mask=row_mask
                          CALL EPA$TRANRG_EFD(11,1,CRG,CTLPLT,COLUMN,COLUMN_mask,COEFFS)
                          row_mask=save_row_mask
                      END IF

                  END IF
               END IF
         END DO                                               !CRV
      END DO                                                   !CRG
!
   
      RETURN
      END
      
      
      SUBROUTINE ED$HYDROGEN
      ! EDT <EDWARD.THOMAS@EIA.GOV> 05/13/2021
      !
      !THE PURPOSE OF THIS SUBROUTINE IS TO ADD HYDROGEN SUPPLY CURVES INTO THE EFD MODULE. TO IMPLEMENT THIS, WE READ IN DATA FROM A CSV FILE
      !WHICH IS THE OUTPUT OF THE HYDROGEN MODULE. FROM THERE, THE READS AND HELPER ROUTINES ARE HANDLED IN THE HYDROGEN_DATA FORTRAN MODULE. 
      !THE DECLARATION OF THIS MODULE IS IN UDAT.F, WHERE IT IS THEN CALLED ONCE TO POPULATE THE VARIABLES IN THE MODULE. 
      !
      !FROM THERE IT'S USED IN THIS SUBROUTINE. THIS SUBROUTINE, LOOPS OVER ALL THE NATURAL GAS REGIONS (SINCE HYDROGEN WILL BE INTERFACING) WITH 
      !THOSE FUEL TYPES IN THE FUTURE. AND FETCHES AND HYDROGEN TYPE OBJECT FOR THAT REIGON CONTAINING ALL THE DATA. tHEN THERE ARE SUBSEQUENT LOOPS 
      !OVER THE STEPS. WHERE THE COEFFIENTS (DRHS), THE VALUES (DVAL), AND THE BOUNDS (DBND) ARE ADDED. 
      !
      !NOTE THAT THE DVAL, DBND, AND DRHS ARE INTERFACE FUNCTIONS DEFINDED IN EFD_ROW_COL MODULE. THIS FUNCTIONS ARE USED FOR OML & AIMS, WHERE THE OPTIONAL
      !ARGUMENTS ARE FOR AIMMS. 
      
      ! LOOP OVER NATURAL GAS REGIONS (WE'RE ASSUMING HYDROGEN IS BEING PRODUCING IN THE SAME FUEL TYPE REGIONS AS NATURAL GAS)
      USE EFD_ROW_COL
      USE EPHRTS_SWTICHES
      USE EPHRTS_FILE_UNIT_NUMBERS 
      
      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'qblk'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'uefdout'
      include'convfact'
      include'angtdm'
      include'ngtdmrep'
      include'emission'
      include'emeblk'
      include'emablk'
      include'emoblk'
      include'cdsparms'
      include'csapr'
      include'emmemis'
      include'hmmblk'
      
      INTEGER MODEL_YEAR, INDEX
      CHARACTER*2 ST,CR
      CHARACTER*16 ROW_G,ROW_S,COLUMN, AIMS_LABEL
      INTEGER CRG,SSN,STEP_H2
      REAL*8 QUANTITYPERSTEP, COEFF
      CHARACTER*1 SSNCD(3)
      LOGICAL E_DEBUG_EXIST
      
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
         INQUIRE(FILE="EPHRTS_DEBUG_FILE.TXT", EXIST=E_DEBUG_EXIST)
         IF (E_DEBUG_EXIST) THEN
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="OLD", POSITION="APPEND", ACTION="WRITE")
         ELSE
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="NEW", ACTION="WRITE")
         END IF
      END IF
            
      ! Assign codes for Seasons 

      SSNCD(1) = '1'
      SSNCD(2) = '2'
      SSNCD(3) = '3'
      
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
         WRITE(unit_num_ephrts_debug_file, *) "ADDING HYDROGEN SUPPLY CURVE"
      END IF
      
      DO CRG = 1 , MNUMCR - 2  !skip last two - unused and US total
        WRITE(CR,'(I2.2)') CRG
       IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN  
          WRITE(unit_num_ephrts_debug_file, *) ""
          WRITE(unit_num_ephrts_debug_file, *) "... TRYING FUEL REGION ", CRG
       END IF 
        ! GET HYDROGEN_REGION OBJECT FROM REGION INDEX. THIS IS FETCHED WITH THE HELPER ROUTINE DECLEARED IN THE MODULE HYDROGEN_DATA, CALLED IN UDAT.F
        
        MODEL_YEAR = CURIYR + 1989
        
        IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
            WRITE(unit_num_ephrts_debug_file, *) "... FOUND FUEL REGION ", CRG
        END IF
        ! THIS ACTS UPON EACH REGION, THIS IS BECAUSE THE PLANTS ARE ASSIGNED FUEL PER REGION  (NOT FUEL PER REGION PER STEP)
 
        IF (EFDMAT .EQ. 0)THEN ! USED TO CREATE (0) OR REVISE (1) MATRIX
                    
            DO SSN = 1, 3
                ROW_S = 'S_H2'//CR//SSNCD(SSN) ; call makmsk(ROW_S_mask,':S_H2:', CR, SSNCD(SSN)) ! ROW_S IS SUPPLY ROW 
        
                CALL DROWTYPE(ROW_S,'L       ',ROW_S_mask) ! L IS THE TYPE OF ROW, IN THIS CASE LESS THAN

                AIMS_LABEL = 'ED$HY_RHS_' // ROW_S
                CALL DRHS(EFDRHS,ROW_S,DBLE(0.0),ROW_S_mask, 'ED$HYDROGEN,1') 
                IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
                    WRITE(unit_num_ephrts_debug_file, *) "... ADDED RIGHT HAND SIDE FOR  ", ROW_S, " COEFF : ", 0.0
                END IF
            ENDDO

        END IF
        
        DO SSN = 1, 3
            DO STEP_H2 = 1, H2STEP
                WRITE(ST,'(I2.2)') STEP_H2 ! CAST THE INTEGER STEP_H2 TO ST

                COLUMN = 'H2'//CR//SSNCD(SSN)//ST; call makmsk(COLUMN_mask, ':H2:',CR,SSNCD(SSN),ST) 
                
                ROW_S = 'S_H2'//CR//SSNCD(SSN) ; call makmsk(ROW_S_mask,':S_H2:',CR,SSNCD(SSN)) ! ROW_S IS SUPPLY ROW 
                
                COEFF = H2SCRV_P(CRG,STEP_H2,SSN,CURIYR)
                AIMS_LABEL = 'ED$HYD_DVAL_' // COLUMN
                CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$HYDROGEN,2') ! DVAL ADDS PRICE PER STEP, PER REGION FOR EACH COLUMN
                              
                IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
                    WRITE(unit_num_ephrts_debug_file, *) "... ADDED VALUE FOR COLUMN  ", COLUMN, " COEFF : ", COEFF, " STEP : ", STEP_H2
                END IF
                              
                CALL DVAL(COLUMN,ROW_S,DBLE(-1.0),COLUMN_mask,ROW_S_mask,'ED$HYDROGEN,3')   ! NOTE: THERE IS ALSO A BALANCE ROW FOR THIS S_H2 ROW
                              
                AIMS_LABEL = 'ED$HYD_DBND_' // COLUMN
                QUANTITYPERSTEP = H2SCRV_Q(CRG,STEP_H2,SSN,CURIYR)
                CALL DBND(EFDBND,COLUMN,0.0D0,QUANTITYPERSTEP,COLUMN_mask,'ED$HYDROGEN,4') ! DBND IS USED FOR BOUNDS, PER REGION, PER STEP. WITH A RESPECTIVE COLUMN LABEL
                IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
                    WRITE(unit_num_ephrts_debug_file, *) "... ADDED BOUNDS FOR COLUMN  ", COLUMN, " MIN : 0.0, MAX : ", QUANTITYPERSTEP
                END IF
            END DO
        END DO
              
      
      END DO
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
         CLOSE(unit_num_ephrts_debug_file)
      END IF
      RETURN
      END
      
      
!
!     This subroutine sets up the Natural Gas Supply Curves for the EFD.
!     It simulates the corresponding curves in the ECP.
!
      SUBROUTINE ED$GAS
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'qblk'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'uefdout'
      include'convfact'
      include'angtdm'
      include'ngtdmrep'
      include'emission'
      include'emeblk'
      include'emablk'
      include'emoblk'
      include'cdsparms'
      include'csapr'
      include'emmemis'
!
      INTEGER FRG,GRG,STP,SSN
      INTEGER IRET
      REAL*4 STSIZE,QTY,TOTSTP
      REAL*4 ELNGSCRV
      REAL*8 COEFF
      CHARACTER*1 SSNCD(3)
      CHARACTER*2 ST,GR
      CHARACTER*16 ROW_G,ROW_S,COLUMN,ROWCAR

      efdsub='ED$GAS'

! Assign codes for Seasons

         SSNCD(1) = '1'
         SSNCD(2) = '2'
         SSNCD(3) = '3'

! Determine Utility Consumption from Last Iteration for Center Point

         QTY = 0.0
      DO FRG = 1 , UNRGNS
         QTY = QTY +  &
                (UFLGFNR(1,FRG,CURIYR) + UFLGFNR(2,FRG,CURIYR)) +  &
                (UFLGINR(1,FRG,CURIYR) + UFLGINR(2,FRG,CURIYR)) +  &
 !               (UFLGCNR(1,FRG,CURIYR) + UFLGCNR(2,FRG,CURIYR)) +  &
                (UFLDGNR(1,FRG,CURIYR) + UFLDGNR(2,FRG,CURIYR))

!        WRITE(6,7776) CURIRUN, CURIYR+1989, CURITR, FRG, &
!           QTY, UFLGFNR(1,FRG,CURIYR), UFLGFNR(2,FRG,CURIYR),  &
!           UFLGINR(1,FRG,CURIYR), UFLGINR(2,FRG,CURIYR),  &
!           UFLGCNR(1,FRG,CURIYR), UFLGCNR(2,FRG,CURIYR),  &
!           UFLDGNR(1,FRG,CURIYR), UFLDGNR(2,FRG,CURIYR)
!7776    FORMAT(1X,"FLRG_NG",4(":",I4),9(":",F20.6))
      end do

! If Gas Consumption = 0 (Probably because of infeasibility) use previous year

      IF (QTY .LE. 0.0)THEN
         QTY = 0.0
       DO FRG = 1 , UNRGNS
         QTY = QTY +  &
                (UFLGFNR(1,FRG,CURIYR-1) + UFLGFNR(2,FRG,CURIYR-1)) +  &
                (UFLGINR(1,FRG,CURIYR-1) + UFLGINR(2,FRG,CURIYR-1)) +  &
!                (UFLGCNR(1,FRG,CURIYR-1) + UFLGCNR(2,FRG,CURIYR-1)) +  &
                (UFLDGNR(1,FRG,CURIYR-1) + UFLDGNR(2,FRG,CURIYR-1))
       end do
      end if

!DKG PATCH
      if (QTY .LE. 0.0) THEN
              QTY = 2000.0
      endif

! Obtain Step Size (Used percent of total consumption from emmcntl and is consistent with ECP)

         STSIZE = UPCRVSIZ * QTY

! Declare Row Type and Assign RHS

         ROW_S = 'S_NGUS';call makmsk(ROW_S_mask,':S_NGUS:')
 
      IF (EFDMAT .EQ. 0)THEN

! Production Row

         CALL DROWTYPE(ROW_S,'L       ',ROW_S_mask)
         COEFF = DBLE(0.0)
         CALL DRHS(EFDRHS,ROW_S,COEFF,ROW_S_mask,'ED$GAS,1')
      END IF

! Production Vectors (Supply Steps) -- eventually use step variable from ECP

      DO STP = 1 , 2 * UPCRVSTP + 1
        WRITE(ST,'(I2.2)') STP
        COLUMN = 'GPUS' // ST; call makmsk(COLUMN_mask,':GPUS:' , ST)

! Step Sizes (Center Point is Reference QTY)

        IF (STP .EQ. 1)THEN
          COEFF = DBLE(QTY - FLOAT(UPCRVSTP) * STSIZE)
          TOTSTP = QTY - FLOAT(UPCRVSTP) * STSIZE
        ELSE
          COEFF = DBLE(STSIZE)
          TOTSTP = TOTSTP + STSIZE
        END IF
          CALL DBND(EFDBND,COLUMN,0.0D0,COEFF,COLUMN_mask,'ED$GAS,2')

! Objective Function - Use Supply Curve Function from NGTDM

          COEFF = DBLE(ELNGSCRV(CURIYR,FLOAT((STP - 1) - UPCRVSTP) * STSIZE))
          IF (COEFF .LE. DBLE(0.001))COEFF = FLOAT(STP) * COEFF
          CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$GAS,3')

!         write(6,7777) curirun, curiyr+1989, curitr, stp,  &
!            ngscrv_min(curiyr), ngscrv_max(curiyr),  &
!            ngscrv_q(curiyr)+float((stp - 1) - 20) * stsize / cfngu(curiyr),  &
!            ngscrv_q(curiyr), ngscrv_p(curiyr),  &
!            qty, totstp, coeff, coeff*scalpr
!7777     format(1h ,'!ngcrv',4(":",i4),9(":",f20.6))

! Put in Production Row

        IF (EFDMAT .EQ. 0)THEN
          COEFF = DBLE(-1.0)
          CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$GAS,4')
        END IF
      END DO

! Transportation Row

      DO GRG = 1 , NNGEM
!         DO SSN = 1 , 2
          DO SSN = 1, 3
             WRITE(GR,'(I2.2)') GRG
             ROW_G = 'NG' // GR // SSNCD(SSN); call makmsk(ROW_G_mask,':NG:' , GR , SSNCD(SSN))
           IF (EFDMAT .EQ. 0)THEN
            CALL DROWTYPE(ROW_G,'L       ',ROW_G_mask)
            COEFF = DBLE(0.0)
            CALL DRHS(EFDRHS,ROW_G,COEFF,ROW_G_mask,'ED$GAS,5')
           END IF

! Set Up Transportation Vectors

             COLUMN = 'GTUS' // GR // SSNCD(SSN); call makmsk(COLUMN_mask,':GTUS:' , GR , SSNCD(SSN))
           IF (EFDMAT .EQ. 0)THEN
            COEFF = DBLE(1.0)
            CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$GAS,6')
            COEFF = DBLE(-1.0)
            CALL DVAL(COLUMN,ROW_G,COEFF,COLUMN_mask,ROW_G_mask,'ED$GAS,7')
           END IF

!   Put in carbon row
               ROWCAR = 'CARBONXX';ROWCAR_MASK='CARBONXX'
               COEFF  = EGFEL(CURIYR) * 0.001
               IF (COEFF .GT. EFD_MIN) CALL DVAL(COLUMN,ROWCAR,COEFF,COLUMN_mask,ROWCAR_mask,'ED$GAS,7.1')

! Objective Function (Markup)

            COEFF = DBLE(SPNGELGR(GRG,CURIYR,SSN) - (OGWPRNG(MNUMOR,CURIYR) / CFNGC(CURIYR)))
!  Remove carbon cost, if EFD constraint included
           IF ((USW_CAR .EQ. 2 .OR. USW_CAR .EQ. 3) .AND. (CURIYR + UHBSYR) .GE. UYR_CAR)THEN
              IF ((TAX_FLAG /= 0) .OR. (PERMIT_FLAG /= 0)) THEN
                 COEFF = COEFF - DBLE(JGFELGR(CURIYR))
              END IF
           END IF
            CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$GAS,8')

          END DO
      END DO
!
      RETURN
      END
!
!     FUNCTION TO DETERMINE WELLHEAD PRICE ASSOCIATED WITH SPECIFIED
!     CHANGE IN UTILITY GAS USE TO CONSTRUCT SUPPLY CURVE
!
      FUNCTION ELNGSCRV(YEAR,VALUE)
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'ngtdmrep'
      include 'convfact'

      INTEGER*4 YEAR            ! YEAR SO ROUTINE CAN BE CALLED BY EFD OR ECP
      REAL*4 ELNGSCRV           ! WELLHEAD PRICE ASSOCIATED WITH INPUT DELTA
      REAL*4 VALUE              ! DIFFERENCE IN NATL CONS FROM LAST YEAR
      REAL*4 QBASE              ! REFERENCE QUANTITY POINT
      REAL*4 PBASE              ! REFERENCE PRICE POINT
      REAL*4 XQBASE             ! RESERVES*(P/R)
      REAL*4 XPBASE             ! PRICE ASSOCIATED W/XQBASE
      REAL*4 AQBASE             ! QUANTITY AT PERCENT BELOW XQBASE
      REAL*4 APBASE             ! PRICE ASSOCIATED W/AQBASE
      REAL*4 BQBASE             ! QUANTITY AT PERCENT ABOVE XQBASE
      REAL*4 BPBASE             ! PRICE ASSOCIATED W/BPBASE
      REAL*4 CQBASE             ! QUANTITY AT PERCENT BELOW AQBASE
      REAL*4 CPBASE             ! PRICE ASSOCIATED W/CQBASE
      REAL*4 DQBASE             ! QUANTITY AT PERCENT ABOVE BQBASE
      REAL*4 DPBASE             ! PRICE ASSOCIATED W/DPBASE

      REAL*4 QVAR               ! TEMPORARY HOLDING ARRAY
      REAL*4 ELAS               ! SUPPLY CURVE "ELASTICITY"

!------------------

!  SET VARIABLES WHICH DON'T CHANGE BASED ON VALUE COMING IN

      XQBASE = NGSCRV_Q0(YEAR)
      XPBASE = NGSCRV_P0(YEAR)

      AQBASE = XQBASE * (1. - NGSCRV_PER(1))
      BQBASE = XQBASE * (1. + NGSCRV_PER(1))
      APBASE = XPBASE * (1.- (NGSCRV_PER(1) /NGSCRV_ELAS(3)))
      BPBASE = XPBASE * (1.+ (NGSCRV_PER(1) /NGSCRV_ELAS(3)))

      CQBASE = AQBASE * (1. - NGSCRV_PER(2))
      DQBASE = BQBASE * (1. + NGSCRV_PER(2))
      CPBASE = APBASE * (1.- (NGSCRV_PER(2) /NGSCRV_ELAS(2)))
      DPBASE = BPBASE * (1.+ (NGSCRV_PER(2) /NGSCRV_ELAS(4)))

!  TRANSLATE CHANGE IN CONSUMPTION TO PLACE ON SUPPLY CURVE

!     IF (CURITR.EQ.1) THEN
!       QVAR = NGSCRV_Q(CURIYR-1) + (VALUE /CFNGU(CURIYR))  ! or N
!     ELSE
        QVAR = NGSCRV_Q(YEAR) + (VALUE /CFNGU(YEAR))    ! or N
!     ENDIF
!  IF BELOW MINIMUM QUANTITY, SET LOW PRICE
!     IF (QVAR.LT.NGSCRV_MIN(CURIYR)) THEN
!       WRITE(6,1111) curiyr+1989,curitr
!1111 format(1h ,'!ngerr - NG QTY LT MIN IN YEAR ',I4,' , ITER ',I2)
!       ELNGSCRV = 0.001
!  IF ABOVE MAXIMUM QUANTITY, SET HI  PRICE
!     ELSE IF (QVAR.GT.NGSCRV_MAX(CURIYR)) THEN
!       WRITE(6,2222) curiyr+1989,curitr
!2222 format(1h ,'!ngerr - NG QTY GT MAX IN YEAR ',I4,' , ITER ',I2)
!       ELNGSCRV = 9.999
!     ELSE

!  OTHERWISE, ESTABLISH PLACE ON CURVE AND SET PRICE

        IF (QVAR .LE. CQBASE) THEN
          QBASE = CQBASE
          PBASE = CPBASE
          ELAS = NGSCRV_ELAS(1)
        ELSE IF (QVAR .LE. AQBASE) THEN
          QBASE = AQBASE
          PBASE = APBASE
          ELAS = NGSCRV_ELAS(2)
        ELSE IF (QVAR .GE. DQBASE) THEN
          QBASE = DQBASE
          PBASE = DPBASE
          ELAS = NGSCRV_ELAS(5)
        ELSE IF (QVAR .GE. BQBASE) THEN
          QBASE = BQBASE
          PBASE = BPBASE
          ELAS = NGSCRV_ELAS(4)
        ELSE
          QBASE = XQBASE
          PBASE = XPBASE
          ELAS  = NGSCRV_ELAS(3)
        END IF
          ELNGSCRV = PBASE * (((1./ELAS)*(QVAR-QBASE)/QBASE) + 1.)

        IF (ELNGSCRV .LE. 0.0) THEN
          ELNGSCRV = XPBASE
          WRITE(6,3333) curiyr+1989,curitr,VALUE,CFNGU(CURIYR),QVAR,QBASE,PBASE,ELAS,XQBASE,XPBASE, &
             NGSCRV_ELAS(1), CQBASE,CPBASE, &
             NGSCRV_ELAS(2), AQBASE,APBASE, &
             NGSCRV_ELAS(3), BQBASE,BPBASE, &
             NGSCRV_ELAS(4), DQBASE,DPBASE, &
             NGSCRV_ELAS(5), NGSCRV_PER(1), NGSCRV_PER(2)
 3333 format(1h ,'!ngerr_elngscrv',2(':',I4),23(':',E10.3))
        ENDIF
!     ENDIF

      END
!
!     This subroutine sets up an Oil Supply Curve (1 step) for the EFD.
!     It simulates the corresponding curve in the ECP.
!
      SUBROUTINE ED$OIL
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'qblk'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'fuelin'
      include'convfact'
      include'intout'
      include'emission'
      include'cdsparms'
      include'emeblk'
      include'emablk'
      include'emoblk'
      include'csapr'
      include'emmemis'
!
      INTEGER FRG,ORG,CRG,STP,SO2,HG
!jj   INTEGER SO2_GRP_BY_ORG(MNUMCR)
      INTEGER IRET
      REAL*4 PRC,QRS,QDS
      REAL*8 COEFF
      CHARACTER*1 SO2CODE
      CHARACTER*2 ST,OR
      CHARACTER*16 ROW_SO2,ROW_HG,ROW_O,ROW_S,COLUMN,ROWCAR

      character*30 save_row_mask

      efdsub='ED$OIL'

! Obtain Prices and Quantities for Center of Supply Curve

         PRC = IT_WOP(CURIYR,1)/CFCRDIMP(CURIYR)
         QRS = QRSEL(MNUMCR,CURIYR)
         QDS = QDSEL(MNUMCR,CURIYR)

!     write(6,1111) curiyr+1989,curitr,prc,qrs,qds
!1111 format(1h ,'!olpr',i4,i3,3f10.3)

!     Create SO2_GRP_BY_OLRG

!jj   DO FRG = 1 , MAXNFR
!jj      CRG = EPCLMP(FRG)
!jj      ORG = EPCSMP(FRG)
!jj      IF (CRG .GT. 0) THEN
!jj         SO2_GRP_BY_ORG(ORG) = SO2_GRP_BY_CLRG(CRG)
!jj      END IF
!jj   END DO

! Resid
! Declare Row Type and Assign RHS

         ROW_S = 'S_RSUS'; ROW_S_mask='S_RSUS'
      IF (EFDMAT .EQ. 0)THEN

! Production Row

         CALL DROWTYPE(ROW_S,'L       ',ROW_S_mask)
         COEFF = DBLE(0.0)
         CALL DRHS(EFDRHS,ROW_S,COEFF,ROW_S_mask,'ED$OIL,1')
      END IF

! Production Vectors (Supply Steps)

      DO STP = 1 , 1
        WRITE(ST,'(I2.2)') STP
        COLUMN = 'RPUS' // ST; call makmsk(COLUMN_mask,':RPUS:' , ST)

! Objective Function (Crude Oil Price)

        COEFF = DBLE(PRC)
        CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$OIL,2')

! Put in Production Row

        IF (EFDMAT .EQ. 0)THEN
          COEFF = DBLE(-1.0)
          CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$OIL,3')
        END IF
      END DO

! Transportation Row

      DO ORG = 1 , MNUMCR - 2
         WRITE(OR,'("0",I1)') ORG
         ROW_O = 'RS' // OR; call makmsk(ROW_O_mask,':RS:' , OR)
           IF (EFDMAT .EQ. 0)THEN
            CALL DROWTYPE(ROW_O,'L       ',ROW_O_mask)
            COEFF = DBLE(0.0)
            CALL DRHS(EFDRHS,ROW_O,COEFF,ROW_O_mask,'ED$OIL,4')
           END IF

! Set Up Transportation Vectors

         COLUMN = 'RTUS' // OR; call makmsk(COLUMN_mask,':RTUS:' , OR)
           IF (EFDMAT .EQ. 0)THEN
            COEFF = DBLE(1.0)
            CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$OIL,5')
            COEFF = DBLE(-1.0)
            CALL DVAL(COLUMN,ROW_O,COEFF,COLUMN_mask,ROW_O_mask,'ED$OIL,6')
           END IF

! Objective Function (Markup)

            COEFF = DBLE(UPFUEL(UIRL,ORG) - PRC)
!  Remove carbon cost, if EFD Constraint
           IF ((USW_CAR .EQ. 2 .OR. USW_CAR .EQ. 3) .AND. (CURIYR + UHBSYR) .GE. UYR_CAR)THEN
              IF ((TAX_FLAG /= 0) .OR. (PERMIT_FLAG /= 0)) THEN
                 COEFF = COEFF - DBLE(JRSEL(CURIYR))
              END IF
           END IF
            CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$OIL,7')

!   Put in carbon row
               ROWCAR = 'CARBONXX';ROWCAR_MASK='CARBONXX'
               COEFF  = ERLEL(CURIYR) * 0.001
               IF (COEFF .GT. EFD_MIN) CALL DVAL(COLUMN,ROWCAR,COEFF,COLUMN_mask,ROWCAR_mask,'ED$OIL,7.1')

!        write(6,2221) curiyr+1989,curitr,frg,org,UPFUEL(UIRL,ORG),prc,coeff
!2221    format(1h ,'!rsmk',i4,i3,i3,i3,3f10.3)

! SO2 Emissions from OIL

!jj      SO2 = SO2_GRP_BY_ORG(ORG)
         DO SO2 = 1 , NUM_SO2_GRP
           IF (SO2_SHR_BY_OLRG(ORG,SO2) .GT. 0.0)THEN
            WRITE(SO2CODE,'(I1)') SO2
            ROW_SO2 = 'SULF' // UPLNTCD(WIST) // SO2CODE; call makmsk(ROW_SO2_mask,':SULF:' , UPLNTCD(WIST) , SO2CODE)
            COEFF = DBLE(UFRSO2(UIRL,ORG) * SO2_SHR_BY_OLRG(ORG,SO2) * 0.5)
            IF (COEFF .GT. EFD_MIN) THEN
               CALL DVAL(COLUMN,ROW_SO2,COEFF,COLUMN_mask,ROW_SO2_mask,'ED$OIL,8')
               TST_SO2(WIST,SO2) = 1
            END IF
           END IF
         END DO

!        COAL REGION CONSTRAINTS FOR EPA TRANSPORT RULE

         IF (TSO2_YR_BY_CLRG .LT. 9999)THEN
            COEFF = DBLE(UFRSO2(UIRL,ORG) * 0.5)
            IF (COEFF .GT. ECP_MIN)THEN
               save_row_mask=row_mask
               CALL EPA$TRANRG_EFD(12,1,ORG,1,COLUMN,COLUMN_mask,COEFF)
               row_mask=save_row_mask
            END IF
         END IF

!     write(6,2222) curiyr+1989,curitr,frg,org,crg,UFRSO2(UIRL,ORG),coeff
!2222 format(1h ,'!rso2',i4,i3,i3,i3,i3,3f10.3)

! Hg  Emissions from Resid

         DO HG = 1 , NUM_HG_GRP
            COEFF = DBLE(UFRHG(UIRL,ORG) * 0.5)
            IF (COEFF .GT. EFD_MIN) THEN
               ROW_HG = 'MERC_RS' // UPRGCD(HG); call makmsk(ROW_HG_mask,':MERC_RS:' , UPRGCD(HG))
               CALL DVAL(COLUMN,ROW_HG,COEFF,COLUMN_mask,ROW_HG_mask,'ED$OIL,9')
               TST_HG(NUTSEC+1,HG) = 1
            END IF
         END DO

      END DO
!
! Distillate
! Declare Row Type and Assign RHS

         ROW_S = 'S_DSUS' ; ROW_S_mask = 'S_DSUS'
      IF (EFDMAT .EQ. 0)THEN

! Production Row

         CALL DROWTYPE(ROW_S,'L       ',ROW_S_mask)
         COEFF = DBLE(0.0)
         CALL DRHS(EFDRHS,ROW_S,COEFF,ROW_S_mask,'ED$OIL,10')
      END IF

! Production Vectors (Supply Steps)

      DO STP = 1 , 1
        WRITE(ST,'(I2.2)') STP
        COLUMN = 'DPUS' // ST; call makmsk(COLUMN_mask,':DPUS:' , ST)

! Objective Function (Crude Oil Price)

        COEFF = DBLE(PRC)
        CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$OIL,11')

! Put in Production Row

        IF (EFDMAT .EQ. 0)THEN
          COEFF = DBLE(-1.0)
          CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$OIL,12')
        END IF
      END DO

! Transportation Row

      DO ORG = 1 , MNUMCR - 2
         WRITE(OR,'("0",I1)') ORG
         ROW_O = 'DS' // OR; call makmsk(ROW_O_mask,':DS:' , OR)
           IF (EFDMAT .EQ. 0)THEN
            CALL DROWTYPE(ROW_O,'L       ',ROW_O_mask)
            COEFF = DBLE(0.0)
            CALL DRHS(EFDRHS,ROW_O,COEFF,ROW_O_mask,'ED$OIL,13')
           END IF

! Set Up Transportation Vectors

         COLUMN = 'DTUS' // OR; call makmsk(COLUMN_mask,':DTUS:' , OR)
           IF (EFDMAT .EQ. 0)THEN
            COEFF = DBLE(1.0)
            CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$OIL,14')
            COEFF = DBLE(-1.0)
            CALL DVAL(COLUMN,ROW_O,COEFF,COLUMN_mask,ROW_O_mask,'ED$OIL,15')
           END IF

! Objective Function (Markup)

            COEFF = DBLE(UPFUEL(UIDS,ORG) - PRC)
!  Remove carbon cost, if EFD Constraint
           IF ((USW_CAR .EQ. 2 .OR. USW_CAR .EQ. 3) .AND. (CURIYR + UHBSYR) .GE. UYR_CAR)THEN
              IF ((TAX_FLAG /= 0) .OR. (PERMIT_FLAG /= 0)) THEN
                 COEFF = COEFF - DBLE(JDSEL(CURIYR))
              END IF
           END IF
            CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$OIL,16')

!   Put in carbon row
               ROWCAR = 'CARBONXX';ROWCAR_MASK='CARBONXX'
               COEFF  = EDSEL(CURIYR) * 0.001
               IF (COEFF .GT. EFD_MIN) CALL DVAL(COLUMN,ROWCAR,COEFF,COLUMN_mask,ROWCAR_mask,'ED$OIL,16.1')

!     write(6,3333) curiyr+1989,curitr,frg,org,UPFUEL(UIDS,ORG),prc,coeff
!3333 format(1h ,'!dsmk',i4,i3,i3,i3,3f10.3)

! Hg  Emissions from Distillate

!        DO HG = 1 , NUM_HG_GRP
         DO HG = 1 , NUM_HG_GRP
            COEFF = DBLE(UFRHG(UIDS,ORG) * 0.5)
            IF (COEFF .GT. EFD_MIN) THEN
               ROW_HG = 'MERC_DS' // UPRGCD(HG); call makmsk(ROW_HG_mask,':MERC_DS:' , UPRGCD(HG))
               CALL DVAL(COLUMN,ROW_HG,COEFF,COLUMN_mask,ROW_HG_mask,'ED$OIL,17')
               TST_HG(NUTSEC+2,HG) = 1
            END IF
         END DO

      END DO

      RETURN
      END

!     This subroutine sets up the Biomass Supply Curves for the EFD.

      SUBROUTINE ED$BIO
      use efd_row_col

      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'wrenew'
      include 'wwdcomon'

      INTEGER FRG, CRG, STP, I_SUPt
      INTEGER IRET
      REAL*4 PRC,QRS,QDS
      REAL*8 COEFF
      CHARACTER*2 ST, BR
      CHARACTER*16 ROW_S,COLUMN,ROW_S_ALL

      efdsub='ED$BIO'

!     Set up Combined Biomass Supply Row

      DO CRG = 1 , NDREG
         WRITE(BR,'(I2.2)') CRG

         ROW_S_ALL = 'S_WD' // BR; call makmsk(ROW_S_ALL_mask,':S_WD:' , BR)

!        Combined Production Row

         IF (EFDMAT .EQ. 0) THEN
            CALL DROWTYPE(ROW_S_ALL,'L       ',ROW_S_ALL_mask)
            COEFF = DBLE(0.0)
            CALL DRHS(EFDRHS,ROW_S_ALL,COEFF,ROW_S_ALL_mask,'ED$BIO,1')
        END IF
      END DO

!     Set up Production Rows by Biomass Supply Type

      DO I_SUPt = 1 , MNUMFS
         IF (WDSUP_AVL(I_SUPt) .EQ. 1 .AND. MP_BM_PW(I_SUPt) .GT. 0.0) THEN

! Declare Row Type and Assign RHS

       DO CRG = 1 , NDREG
           WRITE(BR,'(I2.2)') CRG

               ROW_S = 'S_WD' // BM_TYP_CD(I_SUPt) // BR; call makmsk(ROW_S_mask,':S_WD:' , BM_TYP_CD(I_SUPt) , BR)

! Production Row

               IF (EFDMAT .EQ. 0) THEN
           CALL DROWTYPE(ROW_S,'L       ',ROW_S_mask)
           COEFF = DBLE(0.0)
           CALL DRHS(EFDRHS,ROW_S,COEFF,ROW_S_mask,'ED$BIO,2')

                  COLUMN = 'BP' // BR // BM_TYP_CD(I_SUPt) // "XX"; call makmsk(COLUMN_mask,':BP:' , BR , BM_TYP_CD(I_SUPt) , "XX")

                  ROW_S_ALL = 'S_WD' // BR; call makmsk(ROW_S_ALL_mask,':S_WD:' , BR)
                  COEFF = DBLE(-1.0)
                  CALL DVAL(COLUMN,ROW_S_ALL,COEFF,COLUMN_mask,ROW_S_ALL_mask,'ED$BIO,3')

                  COEFF = DBLE(1.0)
                  CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$BIO,4')
               END IF

! Production Vectors (Supply Steps)

               DO STP = 1 , NM_BM_SUP_STP + 1
                  WRITE(ST,'(I2.2)') STP

                  COLUMN = 'BP' // BR // BM_TYP_CD(I_SUPt) // ST; call makmsk(COLUMN_mask,':BP:' , BR , BM_TYP_CD(I_SUPt) , ST)

!                 Step Size

                  IF (STP .LE. 1)THEN
                     COEFF = DBLE(WDSUP_Q(STP,CRG,CURIYR,I_SUPt))
                  ELSE IF (STP .LE. NM_BM_SUP_STP) THEN
                     COEFF = DBLE(WDSUP_Q(STP,CRG,CURIYR,I_SUPt) - WDSUP_Q(STP - 1,CRG,CURIYR,I_SUPt))
                  ELSE
                     COEFF = DBLE(99999.9)
                  END IF
                  COEFF = MAX(COEFF,DBLE(0.0))

                  IF (COEFF .GT. 0.0) THEN

                     CALL DBND(EFDBND,COLUMN,0.0D0,COEFF,COLUMN_mask,'ED$BIO,5')


! Objective Function

                     IF (STP .LE. NM_BM_SUP_STP)THEN
                        COEFF = DBLE(WDSUP_P(STP,CRG,CURIYR,I_SUPt))
                  ELSE
                     COEFF = DBLE(9999.9)
                  END IF

                  CALL DVAL(COLUMN,EFDOBJ,COEFF,COLUMN_mask,EFDOBJ,'ED$BIO,6')

! Put in Production Row

                  IF (EFDMAT .EQ. 0) THEN
                    COEFF = DBLE(-1.0)
                    CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$BIO,7')
                  END IF
               END IF
            END DO                                        !STP

!              Account for Other Uses of Biomass

!              Residential

               IF (MP_BM_RS(I_SUPt) .GT. 0.0) THEN

                  COLUMN = 'B' // BR // 'RS' // BM_TYP_CD(I_SUPt) // 'X'; COLUMN_mask='B(**)(**)(**)X'

                  IF (EFDMAT .EQ. 0) THEN
          COEFF = DBLE(1.0)
          CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$BIO,8')
        END IF

                  COEFF = QBMRSCL(I_SUPt,CRG,CURIYR)
                  COEFF = MAX(DBLE(0.0) , COEFF)

          CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$BIO,9')
             END IF

!              Commercial

               IF (MP_BM_CM(I_SUPt) .GT. 0.0) THEN

                  COLUMN = 'B' // BR // 'CM' // BM_TYP_CD(I_SUPt) // 'X'; COLUMN_mask='B(**)(**)(**)X'

                  IF (EFDMAT .EQ. 0) THEN
          COEFF = DBLE(1.0)
          CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$BIO,10')
           END IF

                  COEFF = QBMCMCL(I_SUPt,CRG,CURIYR)
                  COEFF = MAX(DBLE(0.0) , COEFF)

          CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$BIO,11')
               END IF

!              Industrial

               IF (MP_BM_IN(I_SUPt) .GT. 0.0) THEN

                  COLUMN = 'B' // BR // 'IN' // BM_TYP_CD(I_SUPt) // 'X'; COLUMN_mask='B(**)(**)(**)X'

                  IF (EFDMAT .EQ. 0) THEN
          COEFF = DBLE(1.0)
          CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$BIO,12')
        END IF

                  COEFF = QBMINCL(I_SUPt,CRG,CURIYR)
                  COEFF = MAX(DBLE(0.0) , COEFF)

                  CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$BIO,13')
               END IF

!              Hydrogen Production

               IF (MP_BM_H2(I_SUPt) .GT. 0.0) THEN

                  COLUMN = 'B' // BR // 'H2' // BM_TYP_CD(I_SUPt) // 'X'; COLUMN_mask='B(**)(**)(**)X'

                  IF (EFDMAT .EQ. 0) THEN
                     COEFF = DBLE(1.0)
                     CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$BIO,14')
                  END IF

                  COEFF = QBMH2CL(I_SUPt,CRG,CURIYR)
                  COEFF = MAX(DBLE(0.0) , COEFF)

                  CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$BIO,15')
               END IF

!              Cellulosic Ethanol Production

               IF (MP_BM_ET(I_SUPt) .GT. 0.0) THEN

                  COLUMN = 'B' // BR // 'ET' // BM_TYP_CD(I_SUPt) // 'X'; COLUMN_mask='B(**)(**)(**)X'

                  IF (EFDMAT .EQ. 0) THEN
                     COEFF = DBLE(1.0)
                     CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$BIO,16')
                  END IF

                  COEFF = QBMETCL(I_SUPt,CRG,CURIYR)
                  COEFF = MAX(DBLE(0.0) , COEFF)

                  CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$BIO,17')
               END IF

!              Biomass to Liquids

               IF (MP_BM_BT(I_SUPt) .GT. 0.0) THEN

                  COLUMN = 'B' // BR // 'BT' // BM_TYP_CD(I_SUPt) // 'X'; COLUMN_mask='B(**)(**)(**)X'

                  IF (EFDMAT .EQ. 0) THEN
                     COEFF = DBLE(1.0)
                     CALL DVAL(COLUMN,ROW_S,COEFF,COLUMN_mask,ROW_S_mask,'ED$BIO,18')
                  END IF

                  COEFF = QBMBTCL(I_SUPt,CRG,CURIYR)
                  COEFF = MAX(DBLE(0.0) , COEFF)

                  CALL DBND(EFDBND,COLUMN,COEFF,COEFF,COLUMN_mask,'ED$BIO,19')
               END IF
            END DO      !CRG
         END IF         !Biomass Supply Type is Available and Can be Used for Power Production
      END DO            !I_SUPt
!
      RETURN
      END
!
      SUBROUTINE ED$CL_IMPORTS
      use efd_row_col
!
!     THIS SUBROUTINE Accounts for Coal Imports and Stock Changes in a way consistent with the CDS
!
      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'cdsparms'
      include 'uso2grp'
      include 'coalrep'      !delete cdscom2l for 2030 version
      include 'emission'
      include 'csapr'
      include 'emmemis'
!
      REAL*8      VAL,IMPORT_VAL,TST,SIGN,STOCK_UP,STOCK_DN,SHR_CDS(ECP_D_DSP,NDREG),SHR_US(ECP_D_DSP,NDREG),T_US,T_CDS(NDREG),A_CDS(NDREG),A_US
      REAL*8      CP_CRG(ECP_D_CAP,EFD_D_MFRG)
      INTEGER*4   IRET,HG,ISO2,CRG,IECP,IRG,IPLT,FRG,PLT
      CHARACTER*16 ROW,COLUMN,ROW_HG,ROW_SO2
      CHARACTER*2 CL_CD
      CHARACTER*1 SO2_CODE
      REAL*8 PRDLIM,COEFF,NEG1/-1.0D0/

      efdsub='ED$CL_IMPORTS'

!
      CP_CRG = 0.0
      DO FRG = 1 , UNFRGN
         DO PLT = 1 , NUTSEC
            IF (CPFLECP(PLT,EPCSMP(FRG),EPCLMP(FRG),EPGSMP(FRG)) .GT. 0.0) THEN
               CP_CRG(PLT,EPCLMP(FRG)) = CPFLECP(PLT,EPCSMP(FRG),EPCLMP(FRG),EPGSMP(FRG)) + CP_CRG(PLT,EPCLMP(FRG))
            END IF
         END DO
      END DO
!
!
      T_US = 0.0
      A_US = 0.0
      DO IRG = 1 , NDREG
         T_CDS(IRG) = 0.0
         A_CDS(IRG) = 0.0
      END DO
!
      DO IRG = 1 , NDREG
         DO IPLT = 1 , NUTSEC
            IF (CP_CRG(IPLT,IRG) .GT. 0) THEN
               A_CDS(IRG) = A_CDS(IRG) + UQCOAL(IPLT,IRG,CURIYR)
               A_US = A_US + UQCOAL(IPLT,IRG,CURIYR)
            END IF
         END DO
      END DO
!
      VAL = 0.0
      DO IRG = 1 ,NDREG
         VAL = VAL + XCL_IMPORT(IRG,CURIYR)
      END DO
!
      WRITE(18,6310) CURIYR+1989,(XCL_IMPORT(IRG,CURIYR),IRG=1,NDREG),VAL
 6310 FORMAT(1X,"EFD_IMPORT",1(":",I4),<NDREG>(":",F10.3))
!
      DO IRG = 1 , NDREG
         DO IPLT = 1 , NUTSEC
            IF (CP_CRG(IPLT,IRG) .GT. 0) THEN
               IF (A_CDS(IRG) .GT. 0.0) THEN
                  SHR_CDS(IPLT,IRG) = UQCOAL(IPLT,IRG,CURIYR) / A_CDS(IRG)
               ELSE
                  SHR_CDS(IPLT,IRG) = 0.0
               END IF
               SHR_US(IPLT,IRG) = UQCOAL(IPLT,IRG,CURIYR) / A_US
            END IF
         END DO
      END DO
!
      COLUMN = 'T_STOCK_';COLUMN_mask='T_STOCK_'
      IF (XCL_STOCK(CURIYR) .GE. 0.0) THEN
         VAL = XCL_STOCK(CURIYR)
         CALL DBND(EFDBND,COLUMN,VAL,VAL,COLUMN_mask,'ED$CL_IMPORTS,1')
         SIGN = 1.0
         STOCK_UP = VAL
         STOCK_DN = 0.0
      ELSE
         VAL = -1.0 * XCL_STOCK(CURIYR)
         CALL DBND(EFDBND,COLUMN,VAL,VAL,COLUMN_mask,'ED$CL_IMPORTS,2')
         SIGN = -1.0
         STOCK_UP = 0.0
         STOCK_DN = VAL
      END IF
!
      HG = 1
      VAL = -1.0 * SIGN * TMPMBTU(CURIYR) * 0.000001 * 1000.0
      IF (VAL .GT. EFD_MIN) THEN
         ROW_HG = 'MERC_OT' // UPRGCD(HG); call makmsk(ROW_HG_mask,':MERC_OT:' , UPRGCD(HG))
         CALL DVAL(COLUMN,ROW_HG,VAL,COLUMN_mask,ROW_HG_mask,'ED$CL_IMPORTS,3')
         TST_HG(NUTSEC+3,1) = 1
      ELSEIF (VAL .LT. -EFD_MIN) THEN
         ROW_HG = 'MERCURY' // UPRGCD(HG); call makmsk(ROW_HG_mask,':MERCURY:' , UPRGCD(HG))
         CALL DVAL(COLUMN,ROW_HG,VAL,COLUMN_mask,ROW_HG_mask,'ED$CL_IMPORTS,4')
      END IF
!
      ISO2 = 1
      WRITE(SO2_CODE,'(I1)') ISO2
      VAL = -1.0 * SIGN * TSPMBTU(CURIYR) * 1000.0
      IF (VAL .GT. EFD_MIN) THEN
         ROW_SO2 = 'SULFOT' // SO2_CODE; call makmsk(ROW_SO2_mask,':SULFOT:' , SO2_CODE)
         CALL DVAL(COLUMN,ROW_SO2,VAL,COLUMN_mask,ROW_SO2_mask,'ED$CL_IMPORTS,5')
         TST_SO2(ECP_D_CAP+1,ISO2) = 1
      ELSEIF (VAL .LT. -EFD_MIN) THEN
         ROW_SO2 = 'SULFUR' // SO2_CODE; call makmsk(ROW_SO2_mask,':SULFUR:' , SO2_CODE)
         CALL DVAL(COLUMN,ROW_SO2,VAL,COLUMN_mask,ROW_SO2_mask,'ED$CL_IMPORTS,6')
      ELSE 
         ROW_SO2 = "                 "
      END IF
      WRITE(18,1324) CURIYR+UHBSYR,CURITR,COLUMN,ROW_SO2,VAL,XCL_STOCK(CURIYR),TSPMBTU(CURIYR)*1000.0,TMPMBTU(CURIYR)*0.001
 1324 FORMAT(1X,"EFD_STOCK",2(":",I4),2(":",A8),4(":",F12.3))
!     Label:EFD_STOCK:CYEAR:ITR:COLUMN:ROW_SO2:VAL:XCL_STOCK:TSPMBTU,TMPMBTU
!
      DO CRG = 1 , NDREG
         DO IECP = 1 , NUTSEC
            IF (CP_CRG(IECP,CRG) .GT. 0.0) THEN
!
               WRITE(CL_CD,'(I2.2)') CRG
               ROW = 'CL' // CL_CD // UPLNTCD(IECP); call makmsk(ROW_mask,':CL:' , CL_CD , UPLNTCD(IECP))
!
               IF (STOCK_UP .GT. 0.0) THEN
                  COLUMN = 'T_SU' // EPFLCD(CRG) // UPLNTCD(IECP) // '_'; call makmsk(COLUMN_mask,':T_SU:' , EPFLCD(CRG) , UPLNTCD(IECP) , ':_:')
                  VAL = STOCK_UP * SHR_US(IECP,CRG)
                  IF (VAL .NE. 0.0) THEN
                     CALL DVAL(COLUMN,ROW,1.0D0,COLUMN_mask,ROW_mask,'ED$CL_IMPORTS,7')
                     CALL DBND(EFDBND,COLUMN,VAL,VAL,COLUMN_mask,'ED$CL_IMPORTS,8')
                  END IF
               END IF
!
               IF (STOCK_DN .GT. 0.0) THEN
                  COLUMN = 'T_SD' // EPFLCD(CRG) // UPLNTCD(IECP) // '_'; call makmsk(COLUMN_mask,':T_SD:' , EPFLCD(CRG) , UPLNTCD(IECP) , ':_:')
                  VAL = STOCK_DN * SHR_US(IECP,CRG)
                  IF (VAL .NE. 0.0) THEN
                     CALL DVAL(COLUMN,ROW,NEG1,COLUMN_mask,ROW_mask,'ED$CL_IMPORTS,9')
                     CALL DBND(EFDBND,COLUMN,VAL,VAL,COLUMN_mask,'ED$CL_IMPORTS,10')
                  END IF
               END IF
!
            END IF
         END DO
      END DO
!
      RETURN
      END

!     This subroutine combines like units (plant groups)

      SUBROUTINE ED$GRP

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'elcntl'
      include'ecpcntl'
      include'plntctl'
      include'elout'
      include'dsmdimen'
      include'dsmtoefd'
      include'uefdout'
      include'emission'
      include'cdsparms'
      include'csapr'
      include'emmemis'

      INTEGER ITYP,IRG, N,IP,IPGRP,IS,IRET,IVSL,IGRP,ISEG,ISTP,IECP,ISP,LGRP,LSEG,INOX,I_EFD_GRPS,INT,JNT
      INTEGER JTYP,JRG,JN,JP,JPGRP,JS,JRET,JVSL,JGRP,JSEG,JSTP,JECP,JSP,KSP,LSP,ITST,IVLS,KPGRP
      REAL*8 TOTHRS,LHRS,T_ACF,MXLFR,TFOR,TPMR,TLFR,MAXCF,FCTR

      INTEGER*4 TST_GRPS(EMM_D_GRP)

!
!     Initialize Variable for Actual Number of EFD Super Groups
!
      N_EFD_GRPS = 0
!
!     Initialize mapping fron ECNTP, EHNTP and EDNTP to EFD Super Group Numbers
!
      MAP_ECNTP_EFD_GRPS = 0
      MAP_EHNTP_EFD_GRPS = 0
      MAP_EDNTP_EFD_GRPS = 0
!
!     Initialize Linked List to Identify EMM Groups in each EFD Super Group
!
      EFD_GRPS_F = 0
      EFD_GRPS_N = 0
!
!     Initialize Data Structure for EFD Super Groups
!
      UG_CAP = 0.0
      UG_GEN = 0.0
      UG_OMR = 0.0
      UG_GSUB = 0.0
      UG_HTRT = 0.0
      UG_ACF = 0.0
      UG_SCF = 0.0
      UG_GCF = 0.0
      UG_FOR = 0.0
      UG_PMR = 0.0
      UG_LFR = 0.0
      UG_NOXC = 0.0
      UG_EMM_RG = 0
      UG_FL_RG = 0
      UG_ECPt = 0
      UG_EFDt = 0
      UG_45Q = 0
      UGNOCCS = 0
      UG_MRUN = 0
!
!     Initialize Data for Plant Groups
!
      UP_CAP = 0.0
      UP_GEN = 0.0
      UP_OMR = 0.0
      UP_GSUB = 0.0
      UP_HTRT = 0.0
      UP_ACF = 0.0
      UP_SCF = 0.0
      UP_GCF = 0.0
      UP_FOR = 0.0
      UP_PMR = 0.0
      UP_LFR = 0.0
      UP_NOXC = 0.0
      UP_EMM_RG = 0
      UP_FL_RG = 0
      UP_ECPt = 0
      UP_EFDt = 0
      UP_45Q = 0
      UPNOCCS = 0.0
      UP_MRUN = 0
!
!     Set Grouping Tolerances
!
      DO IECP = 1 , ECP_D_CAP
         IP = UPEFDT(IECP)
         IF (EPPOPR(IP) .EQ. 1) THEN
            IF (WIET .EQ. IECP .OR. WICT .EQ. IECP) THEN
               TOL_OMR (IECP)= 100.0
               TOL_GSUB(IECP) = 100.0
               TOL_HTRT(IECP) = 14000.0
               TOL_ACF(IECP) = 1.0
               TOL_SCF(IECP) = 1.0
               TOL_GCF(IECP) = 1.0
               TOL_FOR(IECP) = 1.0
               TOL_PMR(IECP) = 1.0
               TOL_LFR(IECP) = 1.0
               TOL_NOXC(IECP) = 1.0
            ELSE IF (WICN .EQ. IECP .OR. WIAN .EQ. IECP.OR. WISM .EQ. IECP) THEN
               TOL_OMR(IECP) = 0.05
               TOL_GSUB(IECP) = 0.001
               TOL_HTRT(IECP) = 500.0
               TOL_ACF(IECP) = 0.1
               TOL_SCF(IECP) = 0.1
               TOL_GCF(IECP) = 0.1
               TOL_FOR(IECP) = 0.1
               TOL_PMR(IECP) = 0.1
               TOL_LFR(IECP) = 0.1
               TOL_NOXC(IECP) = 0.1
            ELSE
               TOL_OMR (IECP)= 1.0
               TOL_GSUB(IECP) = 0.01
               TOL_HTRT(IECP) = 800.0
               TOL_ACF(IECP) = 0.3
               TOL_SCF(IECP) = 0.3
               TOL_GCF(IECP) = 0.3
               TOL_FOR(IECP) = 0.3
               TOL_PMR(IECP) = 0.3
               TOL_LFR(IECP) = 0.3
               TOL_NOXC(IECP) = 0.3
            END IF
         ELSE IF (EPPOPR(IP) .EQ. 2) THEN
            TOL_OMR (IECP)= 10.0
            TOL_GSUB(IECP) = 10.0
            TOL_HTRT(IECP) = 3000.0
            TOL_ACF(IECP) = 0.5
            TOL_SCF(IECP) = 0.5
            TOL_GCF(IECP) = 0.5
            TOL_FOR(IECP) = 0.5
            TOL_PMR(IECP) = 0.5
            TOL_LFR(IECP) = 0.5
            TOL_NOXC(IECP) = 0.5
         ELSE IF (EPPOPR(IP) .EQ. 3) THEN
            TOL_OMR (IECP)= 10.0
            TOL_GSUB(IECP) = 10.0
            TOL_HTRT(IECP) = 3000.0
            TOL_ACF(IECP) = 0.5
            TOL_SCF(IECP) = 0.5
            TOL_GCF(IECP) = 0.5
            TOL_FOR(IECP) = 0.5
            TOL_PMR(IECP) = 0.5
            TOL_LFR(IECP) = 0.5
            TOL_NOXC(IECP) = 0.5
         ELSE IF (EPPOPR(IP) .EQ. 4) THEN
            TOL_OMR (IECP)= 100.0
            TOL_GSUB(IECP) = 100.0
            TOL_HTRT(IECP) = 10000.0
            TOL_ACF(IECP) = 1.0
            TOL_SCF(IECP) = 1.0
            TOL_GCF(IECP) = 1.0
            TOL_FOR(IECP) = 1.0
            TOL_PMR(IECP) = 1.0
            TOL_LFR(IECP) = 1.0
            TOL_NOXC(IECP) = 1.0
         ELSE IF (EPPOPR(IP) .EQ. 5) THEN
            TOL_OMR (IECP)= 10.0
            TOL_GSUB(IECP) = 10.0
            TOL_HTRT(IECP) = 3000.0
            TOL_ACF(IECP) = 0.5
            TOL_SCF(IECP) = 0.5
            TOL_GCF(IECP) = 0.5
            TOL_FOR(IECP) = 0.5
            TOL_PMR(IECP) = 0.5
            TOL_LFR(IECP) = 0.5
            TOL_NOXC(IECP) = 0.5
         END IF
      END DO
!
!     CALCULATE TOTAL HOURS IN THE YEAR
!
      TOTHRS = 0.0
      DO ISP = 1 , EENSP
         TOTHRS = TOTHRS + EETIME(ISP)
      END DO
!
!     loop over region
!
      DO IRG = 1, UNRGNS
         CALL GETIN(1,IRG)

!        WRITE(6,3977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3977    FORMAT(1X,"UEFD_12944_EEITAJ_GET",4(":",I4),3(":",F12.3))

         CALL GETBLD(1,IRG)
!
!        loop over dispatchable plant types
!
         ITYP = 1

         DO N = 1, ECNTP
            IF (MAP_ECNTP_EFD_GRPS(N,IRG) .EQ. 0) THEN
               IP = ECASTS(N)
               IPGRP = ECDBID(N)
               IECP = ECTECP(N)
               N_EFD_GRPS = N_EFD_GRPS + 1
               MAP_ECNTP_EFD_GRPS(N,IRG) = N_EFD_GRPS
               EFD_GRPS_F(N_EFD_GRPS) = IPGRP
               UG_EMM_RG(N_EFD_GRPS) = IRG
               UP_EMM_RG(IPGRP) = IRG
               UG_FL_RG(N_EFD_GRPS) = EPNFLRG(ECCR(N),ECLR(N),ECGR(N),ECAR(N))
               UP_FL_RG(IPGRP) = EPNFLRG(ECCR(N),ECLR(N),ECGR(N),ECAR(N))
               UG_STATE(N_EFD_GRPS) = ECST(N)
               UP_STATE(IPGRP) = ECST(N)
               UG_ECPt(N_EFD_GRPS) = IECP
               UP_ECPt(IPGRP) = IECP
               UG_EFDt(N_EFD_GRPS) = IP
               UP_EFDt(IPGRP) = IP
               UG_MRUN(N_EFD_GRPS) = ULMRUN(IPGRP)
               UP_MRUN(IPGRP) = ULMRUN(IPGRP)
               UG_45Q(N_EFD_GRPS) = EC_45Q(N)
               UP_45Q(IPGRP) = EC_45Q(N)
               UGNOCCS(N_EFD_GRPS) = ECNOCCS(N)
               UPNOCCS(IPGRP) = ECNOCCS(N)

!              WRITE(18,9133) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, ULORGN(IPGRP), N_EFD_GRPS, IPGRP, N, N, IP, IECP, ULECPT(IPGRP), ULEFDT(IPGRP), ULMRUN(IPGRP), &
!                 ULFRGN(IPGRP), ULIGRP(IPGRP), UPLNTCD(IECP), &
!                 (ECCAP(N,ISP),ISP=1,EENSP), UPHTRT(IECP), (ULSCAP_EFD(IPGRP,ISP), ULHTRT_EFD(IPGRP,ISP),ISP=1,EENSP)
!9133          FORMAT(1X,"HTRT_UEFD",16(":",I6),":",A2,<EENSP>(":",F21.6),1(":",F21.6),<EENSP>(":",F21.6,":",F21.6))

               DO ISP = 1 , EENSP
                  UG_CAP(ISP,N_EFD_GRPS) = UG_CAP(ISP,N_EFD_GRPS) + ECCAP(N,ISP)
                  UG_CAP(0,N_EFD_GRPS) = UG_CAP(0,N_EFD_GRPS) + ECCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UP_CAP(ISP,IPGRP) = ECCAP(N,ISP)
                  UP_CAP(0,IPGRP) = UP_CAP(0,IPGRP) + ECCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_GEN(ISP,N_EFD_GRPS) = UG_GEN(ISP,N_EFD_GRPS) + ECCAP(N,ISP) * DBLE(ECMXCP(N)) * EFACTR * EETIME(ISP) * 0.001
                  UG_GEN(0,N_EFD_GRPS) = UG_GEN(0,N_EFD_GRPS) + ECCAP(N,ISP) * DBLE(ECMXCP(N)) * EFACTR * EETIME(ISP) * 0.001
                  UP_GEN(ISP,IPGRP) = ECCAP(N,ISP) * ECMXCP(N) * EFACTR * EETIME(ISP) * 0.001
                  UP_GEN(0,IPGRP) = UP_GEN(0,IPGRP) + ECCAP(N,ISP) * ECMXCP(N) * EFACTR * EETIME(ISP) * 0.001
                  UG_OMR(N_EFD_GRPS) = UG_OMR(N_EFD_GRPS) + ECOMR(N,1) * ECCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_GSUB(N_EFD_GRPS) = UG_GSUB(N_EFD_GRPS) + ECGSUB(N) * ECCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_HTRT(ISP,N_EFD_GRPS) = UG_HTRT(ISP,N_EFD_GRPS) + ULHTRT_EFD(IPGRP,ISP) * ECCAP(N,ISP)
                  UG_ACF(N_EFD_GRPS) = UG_ACF(N_EFD_GRPS) + DBLE(ECMXCP(N)) * EFACTR * ECCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_SCF(ISP,N_EFD_GRPS) = UG_SCF(ISP,N_EFD_GRPS) + DBLE(ECMXCP(N)) * EFACTR * ECCAP(N,ISP)
                  UP_SCF(ISP,IPGRP) = DBLE(ECMXCP(N)) * EFACTR
                  DO IVLS = 1 , ELNVCT(ISP)
                     LGRP = ELGRP(IVLS,ISP)
                     LSEG = ELSEG(IVLS,ISP)
                     LHRS = ELWDTH(IVLS,ISP)
                     UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(ECMXCP(N)) * EFACTR * ECCAP(N,ISP)
                     UP_GCF(LSEG,LGRP,IPGRP) = DBLE(ECMXCP(N)) * EFACTR
                  END DO
                  UG_FOR(N_EFD_GRPS) = UG_FOR(N_EFD_GRPS) + ECFOR(IPGRP) * ECCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_PMR(N_EFD_GRPS) = UG_PMR(N_EFD_GRPS) + ECPMR(IPGRP) * ECCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_LFR(N_EFD_GRPS) = UG_LFR(N_EFD_GRPS) + ECLFR(IPGRP) * ECCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  DO INOX = 1 , NOX_GRP
                     UG_NOXC(ISP,INOX,N_EFD_GRPS) = UG_NOXC(ISP,INOX,N_EFD_GRPS) + ECFNOXC(INOX,N,ISP) * ECCAP(N,ISP)
                     UP_NOXC(ISP,INOX,IPGRP) = ECFNOXC(INOX,N,ISP)
                  END DO
                  UP_HTRT(ISP,IPGRP) = ULHTRT_EFD(IPGRP,ISP)
               END DO
               UP_OMR(IPGRP) = ECOMR(N,1)
               UP_GSUB(IPGRP) = ECGSUB(N)
               UP_ACF(IPGRP) = DBLE(ECMXCP(N)) * EFACTR
               UP_FOR(IPGRP) = ECFOR(IPGRP)
               UP_PMR(IPGRP) = ECPMR(IPGRP)
               UP_LFR(IPGRP) = ECLFR(IPGRP)
               IF (N .LT. ECNTP) THEN
                  DO JN = N + 1 , ECNTP
                     IF (MAP_ECNTP_EFD_GRPS(JN,IRG) .EQ. 0) THEN
                        ITST = 0
                        JP = ECASTS(JN)
                        JPGRP = ECDBID(JN)
                        JECP = ECTECP(JN)
                        IF (IP .NE. JP) ITST = 1
                        IF (ULMRUN(IPGRP) .NE. ULMRUN(JPGRP)) ITST = 1
                        IF (IECP .NE. JECP) ITST = 1
                        IF (UG_FL_RG(N_EFD_GRPS) .NE. EPNFLRG(ECCR(JN),ECLR(JN),ECGR(JN),ECAR(JN))) ITST = 1
                        IF (EC_45Q(JN) .NE. UP_45Q(IPGRP)) ITST = 1
                        IF (ECNOCCS(JN) .NE. UPNOCCS(IPGRP)) ITST = 1

!                       Ignore heatrate, capacity factor and O&M differences for grouping purposes

                        IF (IECP .EQ. WIET .OR. IECP .EQ. WICT .OR. IECP .EQ. WIAT) THEN
                           DO ISP = 1 , EENSP
                              IF (ULHTRT_EFD(JPGRP,ISP) .GT. TOL_HTRT(IECP) .AND. UP_HTRT(ISP,IPGRP) .LE. TOL_HTRT(IECP)) ITST = 1
                              IF (ULHTRT_EFD(JPGRP,ISP) .LT. TOL_HTRT(IECP) .AND. UP_HTRT(ISP,IPGRP) .GE. TOL_HTRT(IECP)) ITST = 1
                           END DO
                        ELSE
                           IF (ECOMR(JN,1) .LT. UP_OMR(IPGRP) - TOL_OMR(IECP)) ITST = 1
                           IF (ECOMR(JN,1) .GT. UP_OMR(IPGRP) + TOL_OMR(IECP)) ITST = 1
                           IF (ECGSUB(JN) .LT. UP_GSUB(IPGRP) - TOL_GSUB(IECP)) ITST = 1
                           IF (ECGSUB(JN) .GT. UP_GSUB(IPGRP) + TOL_GSUB(IECP)) ITST = 1
                           IF (DBLE(ECMXCP(JN)) * EFACTR .LT. UP_ACF(IPGRP) - TOL_ACF(IECP)) ITST = 1
                           IF (DBLE(ECMXCP(JN)) * EFACTR .GT. UP_ACF(IPGRP) + TOL_ACF(IECP)) ITST = 1
                           DO ISP = 1 , EENSP
                              IF (ULHTRT_EFD(JPGRP,ISP) .LT. UP_HTRT(ISP,IPGRP) - TOL_HTRT(IECP)) ITST = 1
                              IF (ULHTRT_EFD(JPGRP,ISP) .GT. UP_HTRT(ISP,IPGRP) + TOL_HTRT(IECP)) ITST = 1
                              IF (DBLE(ECMXCP(JN)) * EFACTR .LT. UP_SCF(ISP,IPGRP) - TOL_SCF(IECP)) ITST = 1
                              IF (DBLE(ECMXCP(JN)) * EFACTR .GT. UP_SCF(ISP,IPGRP) + TOL_SCF(IECP)) ITST = 1
                              DO IVLS = 1 , ELNVCT(ISP)
                                 LGRP = ELGRP(IVLS,ISP)
                                 LSEG = ELSEG(IVLS,ISP)
                                 LHRS = ELWDTH(IVLS,ISP)
                                 IF (DBLE(ECMXCP(JN)) * EFACTR .LT. UP_GCF(LSEG,LGRP,IPGRP) - TOL_GCF(IECP)) ITST = 1
                                 IF (DBLE(ECMXCP(JN)) * EFACTR .GT. UP_GCF(LSEG,LGRP,IPGRP) + TOL_GCF(IECP)) ITST = 1
                              END DO
                              DO INOX = 1 , NOX_GRP
                                 IF (ECFNOXC(INOX,JN,ISP) .LT. UP_NOXC(ISP,INOX,IPGRP) - TOL_NOXC(IECP)) ITST = 1
                                 IF (ECFNOXC(INOX,JN,ISP) .GT. UP_NOXC(ISP,INOX,IPGRP) + TOL_NOXC(IECP)) ITST = 1
                              END DO
                           END DO
                           IF (ECFOR(JPGRP) .LT. UP_FOR(IPGRP) - TOL_FOR(IECP)) ITST = 1
                           IF (ECFOR(JPGRP) .GT. UP_FOR(IPGRP) + TOL_FOR(IECP)) ITST = 1
                           IF (ECPMR(JPGRP) .LT. UP_PMR(IPGRP) - TOL_PMR(IECP)) ITST = 1
                           IF (ECPMR(JPGRP) .GT. UP_PMR(IPGRP) + TOL_PMR(IECP)) ITST = 1
                           IF (ECLFR(JPGRP) .LT. UP_LFR(IPGRP) - TOL_LFR(IECP)) ITST = 1
                           IF (ECLFR(JPGRP) .GT. UP_LFR(IPGRP) + TOL_LFR(IECP)) ITST = 1
                        END IF
!
                        IF (ITST .EQ. 0) THEN
                           UP_EMM_RG(JPGRP) = IRG
                           UP_FL_RG(JPGRP) = EPNFLRG(ECCR(JN),ECLR(JN),ECGR(JN),ECAR(JN))
                           UP_ECPt(JPGRP) = JECP
                           UP_EFDt(JPGRP) = JP
                           UP_45Q(JPGRP) = EC_45Q(JN)
                           UPNOCCS(JPGRP) = ECNOCCS(JN)
                           UP_MRUN(JPGRP) = ULMRUN(JPGRP)
                           MAP_ECNTP_EFD_GRPS(JN,IRG) = N_EFD_GRPS
                           IF (EFD_GRPS_N(IPGRP) .EQ. 0) THEN
                              EFD_GRPS_N(IPGRP) = JPGRP
                              KPGRP = JPGRP
                           ELSE
                              IF (EFD_GRPS_N(KPGRP) .EQ. JPGRP) THEN
                                 WRITE(6,2317) CURIRUN, CURIYR+1989, CURITR, N_EFD_GRPS, JN, IPGRP, JPGRP, KPGRP, &
                                    EFD_GRPS_F(N_EFD_GRPS), EFD_GRPS_N(KPGRP), IECP, UG_CAP(1,N_EFD_GRPS), ECCAP(JN,1)
 2317                            FORMAT(1X,"DUPLICATE_GROUPS",11(":",I5),2(":",F15.3))
                              ELSE
                                 EFD_GRPS_N(KPGRP) = JPGRP
                                 KPGRP = JPGRP
                              END IF
                           END IF

!                          WRITE(18,9133) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, ULORGN(JPGRP), N_EFD_GRPS, JPGRP, N, JN, JP, JECP, ULECPT(JPGRP), ULEFDT(JPGRP), ULMRUN(JPGRP), &
!                            ULFRGN(JPGRP), ULIGRP(JPGRP), UPLNTCD(JECP), &
!                            (ECCAP(JN,ISP),ISP=1,EENSP), UPHTRT(JECP), (ULSCAP_EFD(JPGRP,ISP), ULHTRT_EFD(JPGRP,ISP),ISP=1,EENSP)

                           DO ISP = 1 , EENSP
                              UG_CAP(ISP,N_EFD_GRPS) = UG_CAP(ISP,N_EFD_GRPS) + ECCAP(JN,ISP)
                              UG_CAP(0,N_EFD_GRPS) = UG_CAP(0,N_EFD_GRPS) + ECCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UP_CAP(ISP,JPGRP) = ECCAP(JN,ISP)
                              UP_CAP(0,JPGRP) = UP_CAP(0,JPGRP) + ECCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_GEN(ISP,N_EFD_GRPS) = UG_GEN(ISP,N_EFD_GRPS) + ECCAP(JN,ISP) * DBLE(ECMXCP(JN)) * EFACTR * EETIME(ISP) * 0.001
                              UG_GEN(0,N_EFD_GRPS) = UG_GEN(0,N_EFD_GRPS) + ECCAP(JN,ISP) * DBLE(ECMXCP(JN)) * EFACTR * EETIME(ISP) * 0.001
                              UP_GEN(ISP,JPGRP) = ECCAP(JN,ISP) * ECMXCP(JN) * EFACTR * EETIME(ISP) * 0.001
                              UP_GEN(0,JPGRP) = UP_GEN(0,JPGRP) + ECCAP(JN,ISP) * ECMXCP(JN) * EFACTR * EETIME(ISP) * 0.001
                              UG_OMR(N_EFD_GRPS) = UG_OMR(N_EFD_GRPS) + ECOMR(JN,1) * ECCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_GSUB(N_EFD_GRPS) = UG_GSUB(N_EFD_GRPS) + ECGSUB(JN) * ECCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_HTRT(ISP,N_EFD_GRPS) = UG_HTRT(ISP,N_EFD_GRPS) + ULHTRT_EFD(JPGRP,ISP) * ECCAP(JN,ISP)
                              UG_ACF(N_EFD_GRPS) = UG_ACF(N_EFD_GRPS) + DBLE(ECMXCP(JN)) * EFACTR * ECCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_SCF(ISP,N_EFD_GRPS) = UG_SCF(ISP,N_EFD_GRPS) + DBLE(ECMXCP(JN)) * EFACTR * ECCAP(JN,ISP)
                              UP_SCF(ISP,JPGRP) = DBLE(ECMXCP(JN)) * EFACTR
                              DO IVLS = 1 , ELNVCT(ISP)
                                 LGRP = ELGRP(IVLS,ISP)
                                 LSEG = ELSEG(IVLS,ISP)
                                 LHRS = ELWDTH(IVLS,ISP)
                                 UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(ECMXCP(JN)) * EFACTR * ECCAP(JN,ISP)
                                 UP_GCF(LSEG,LGRP,JPGRP) = DBLE(ECMXCP(JN)) * EFACTR
                              END DO
                              UG_FOR(N_EFD_GRPS) = UG_FOR(N_EFD_GRPS) + ECFOR(JPGRP) * ECCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_PMR(N_EFD_GRPS) = UG_PMR(N_EFD_GRPS) + ECPMR(JPGRP) * ECCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_LFR(N_EFD_GRPS) = UG_LFR(N_EFD_GRPS) + ECLFR(JPGRP) * ECCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              DO INOX = 1 , NOX_GRP
                                 UG_NOXC(ISP,INOX,N_EFD_GRPS) = UG_NOXC(ISP,INOX,N_EFD_GRPS) + ECFNOXC(INOX,JN,ISP) * ECCAP(JN,ISP)
                                 UP_NOXC(ISP,INOX,JPGRP) = ECFNOXC(INOX,JN,ISP)
                              END DO
                              UP_HTRT(ISP,JPGRP) = ULHTRT_EFD(JPGRP,ISP)
                           END DO
                           UP_OMR(JPGRP) = ECOMR(JN,1)
                           UP_GSUB(JPGRP) = ECGSUB(JN)
                           UP_ACF(JPGRP) = DBLE(ECMXCP(JN)) * EFACTR
                           UP_FOR(JPGRP) = ECFOR(JPGRP)
                           UP_PMR(JPGRP) = ECPMR(JPGRP)
                           UP_LFR(JPGRP) = ECLFR(JPGRP)
                        END IF
                     END IF
                  END DO
               END IF
            END IF
         ENDDO
!
!        loop over renewable plant types
!
         ITYP = 2
         DO N = 1, EHNTP
            IF (MAP_EHNTP_EFD_GRPS(N,IRG) .EQ. 0) THEN
               IP = EHHYTP(N)
               IPGRP = EHDBID(N)
               IECP = EHTECP(N)
               INT = UCPINTIS(IECP)
               N_EFD_GRPS = N_EFD_GRPS + 1
               MAP_EHNTP_EFD_GRPS(N,IRG) = N_EFD_GRPS
               EFD_GRPS_F(N_EFD_GRPS) = IPGRP
               UG_EMM_RG(N_EFD_GRPS) = IRG
               UP_EMM_RG(IPGRP) = IRG
               UG_FL_RG(N_EFD_GRPS) = EPNFLRG(EHCR(N),EHLR(N),EHGR(N),EHAR(N))
               UP_FL_RG(IPGRP) = EPNFLRG(EHCR(N),EHLR(N),EHGR(N),EHAR(N))
               UG_ECPt(N_EFD_GRPS) = IECP
               UP_ECPt(IPGRP) = IECP
               UG_EFDt(N_EFD_GRPS) = IP
               UP_EFDt(IPGRP) = IP
               UG_MRUN(N_EFD_GRPS) = ULMRUN(IPGRP)
               UP_MRUN(IPGRP) = ULMRUN(IPGRP)

!              WRITE(18,9133) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, ULORGN(IPGRP), N_EFD_GRPS, IPGRP, N, N, IP, IECP, ULECPT(IPGRP), ULEFDT(IPGRP), ULMRUN(IPGRP), &
!                 ULFRGN(IPGRP), ULIGRP(IPGRP), UPLNTCD(IECP), &
!                 (EHCAP(N,ISP),ISP=1,EENSP), UPHTRT(IECP), (ULSCAP_EFD(IPGRP,ISP), ULHTRT_EFD(IPGRP,ISP),ISP=1,EENSP)

               DO ISP = 1 , EENSP
                  UG_CAP(ISP,N_EFD_GRPS) = UG_CAP(ISP,N_EFD_GRPS) + EHCAP(N,ISP)
                  UG_CAP(0,N_EFD_GRPS) = UG_CAP(0,N_EFD_GRPS) + EHCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UP_CAP(ISP,IPGRP) = EHCAP(N,ISP)
                  UP_CAP(0,IPGRP) = UP_CAP(0,IPGRP) + EHCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_GEN(ISP,N_EFD_GRPS) = UG_GEN(ISP,N_EFD_GRPS) + EHCAP(N,ISP) * DBLE(EHHYCF(N,ISP)) * EFACTR * EETIME(ISP) * 0.001
                  UG_GEN(0,N_EFD_GRPS) = UG_GEN(0,N_EFD_GRPS) + EHCAP(N,ISP) * DBLE(EHHYCF(N,ISP)) * EFACTR * EETIME(ISP) * 0.001
                  UP_GEN(ISP,IPGRP) = EHCAP(N,ISP) * EHHYCF(N,ISP) * EFACTR * EETIME(ISP) * 0.001
                  UP_GEN(0,IPGRP) = UP_GEN(0,IPGRP) + EHCAP(N,ISP) * EHHYCF(N,ISP) * EFACTR * EETIME(ISP) * 0.001
                  UG_OMR(N_EFD_GRPS) = UG_OMR(N_EFD_GRPS) + EHVOMR(N) * EHCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_GSUB(N_EFD_GRPS) = UG_GSUB(N_EFD_GRPS) + EHGSUB(N) * EHCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_HTRT(ISP,N_EFD_GRPS) = UG_HTRT(ISP,N_EFD_GRPS) + ULHTRT_EFD(IPGRP,ISP) * EHCAP(N,ISP)
                  UG_ACF(N_EFD_GRPS) = UG_ACF(N_EFD_GRPS) + DBLE(EHHYCF(N,ISP)) * EFACTR * EHCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UP_ACF(IPGRP) = DBLE(EHHYCF(N,ISP)) * EFACTR * EETIME(ISP) / TOTHRS
                  UG_SCF(ISP,N_EFD_GRPS) = UG_SCF(ISP,N_EFD_GRPS) + DBLE(EHHYCF(N,ISP)) * EFACTR * EHCAP(N,ISP)
                  UP_SCF(ISP,IPGRP) = DBLE(EHHYCF(N,ISP)) * EFACTR
                  DO IVLS = 1 , ELNVCT(ISP)
                     LGRP = ELGRP(IVLS,ISP)
                     LSEG = ELSEG(IVLS,ISP)
                     LHRS = ELWDTH(IVLS,ISP)
                     IF (INT .GT. 0) THEN
                        IF (CURCALYR .GE. UPSTYR) THEN
						    IF (IECP .EQ. WIPT) THEN
                                    UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + EHCAP(N,ISP) * HREFDCF(1,INT,1,LSEG,LGRP,IRG)
                                    UP_GCF(LSEG,LGRP,IPGRP) = HREFDCF(1,INT,1,LSEG,LGRP,IRG)  
							ELSEIF (IECP .EQ. WIPV .AND. UG_MRUN(N_EFD_GRPS) .GT. 0) THEN
								! Distributed PV is second RESTORE step
								IF (HREFDCF(0,INT,1,LSEG,LGRP,IRG) .GT. 0.0001) THEN
								    UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR * EHCAP(N,ISP) * (HREFDCF(1,INT,2,LSEG,LGRP,IRG)/ HREFDCF(0,INT,1,LSEG,LGRP,IRG)) !* (DPVTOTGENNR(IRG,CURIYR) / 8.76 * DPVTOTCAPNR(IRG,CURIYR))
                                    UP_GCF(LSEG,LGRP,IPGRP) = DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR * (HREFDCF(1,INT,2,LSEG,LGRP,IRG)/ HREFDCF(0,INT,1,LSEG,LGRP,IRG)) !* (DPVTOTGENNR(IRG,CURIYR) / 8.76 * DPVTOTCAPNR(IRG,CURIYR))
                                                      
                                    WRITE(18,9133) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, N_EFD_GRPS, IPGRP, N, JN, JP, JECP, IECP,UG_MRUN(N_EFD_GRPS),ISP,LSEG, LGRP,LHRS,DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR, UG_CAP(ISP,I_EFD_GRPS),&
                                        HREFDCF(1,INT,2,LSEG,LGRP,IRG), HREFDCF(0,INT,1,LSEG,LGRP,IRG),ULMRUN(IPGRP), &
                                            DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR  * (HREFDCF(1,INT,2,LSEG,LGRP,IRG)/ HREFDCF(0,INT,1,LSEG,LGRP,IRG)),&
                             DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR * EHCAP(N,ISP) * (HREFDCF(1,INT,2,LSEG,LGRP,IRG)/ HREFDCF(0,INT,1,LSEG,LGRP,IRG)), &
                             EHCAP(N,ISP), UG_GCF(LSEG,LGRP,N_EFD_GRPS), DPVTOTGENNR(IRG,CURIYR) / (8.76 * DPVTOTCAPNR(IRG,CURIYR)), DPVTOTGENNR(IRG,CURIYR),  DPVTOTCAPNR(IRG,CURIYR)
                             
                             9133          FORMAT(1X,"UG_GCF_UEFD1",14(":",I6),":",13(":",F21.6))
                             	ELSE
                             		UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR * EHCAP(N,ISP)
                                	UP_GCF(LSEG,LGRP,IPGRP) = DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR
                             	ENDIF
                                    ELSE
                               IF (HREFDCF(0,INT,1,LSEG,LGRP,IRG) .GT. 0.0001) THEN

                                        UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR * EHCAP(N,ISP) * (HREFDCF(1,INT,1,LSEG,LGRP,IRG) / HREFDCF(0,INT,1,LSEG,LGRP,IRG))
                                        UP_GCF(LSEG,LGRP,IPGRP) = DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR * (HREFDCF(1,INT,1,LSEG,LGRP,IRG) / HREFDCF(0,INT,1,LSEG,LGRP,IRG))
                               ELSE
                                  UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR * EHCAP(N,ISP)
                                  UP_GCF(LSEG,LGRP,IPGRP) = DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR
                               END IF
                            END IF
                        ELSE
                           UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR * EHCAP(N,ISP)
                           UP_GCF(LSEG,LGRP,IPGRP) = DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR
                        END IF
                     ELSE
                        UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR * EHCAP(N,ISP)
                        UP_GCF(LSEG,LGRP,IPGRP) = DBLE(EHLDCF(N,LSEG,LGRP)) * EFACTR
                     END IF
                  END DO
                  UG_FOR(N_EFD_GRPS) = WFOR(IP)
                  UG_PMR(N_EFD_GRPS) = WPMR(IP)
                  UG_LFR(N_EFD_GRPS) = 0.0
                  DO INOX = 1 , NOX_GRP
                     UG_NOXC(ISP,INOX,N_EFD_GRPS) = UG_NOXC(ISP,INOX,N_EFD_GRPS) + EHFNOXC(INOX,N,ISP) * EHCAP(N,ISP)
                     UP_NOXC(ISP,INOX,IPGRP) = EHFNOXC(INOX,N,ISP)
                  END DO
                  UP_HTRT(ISP,IPGRP) = ULHTRT_EFD(IPGRP,ISP)
               END DO
               UP_OMR(IPGRP) = EHVOMR(N)
               UP_GSUB(IPGRP) = EHGSUB(N)
               UP_FOR(IPGRP) = WFOR(IP)
               UP_PMR(IPGRP) = WPMR(IP)
               UP_LFR(IPGRP) = 0.0
               IF (N .LT. EHNTP) THEN
                  DO JN = N + 1 , EHNTP
                     IF (MAP_EHNTP_EFD_GRPS(JN,IRG) .EQ. 0) THEN
                        ITST = 0
                        JP = EHHYTP(JN)
                        JPGRP = EHDBID(JN)
                        JECP = EHTECP(JN)
                        JNT = UCPINTIS(JECP)
                        IF (IP .NE. JP) ITST = 1
                        IF (ULMRUN(IPGRP) .NE. ULMRUN(JPGRP)) ITST = 1
                        IF (IECP .NE. JECP) ITST = 1
                        IF (UG_FL_RG(N_EFD_GRPS) .NE. EPNFLRG(EHCR(JN),EHLR(JN),EHGR(JN),EHAR(JN))) ITST = 1
                        IF (EHVOMR(JN) .LT. UP_OMR(IPGRP) - TOL_OMR(IECP)) ITST = 1
                        IF (EHVOMR(JN) .GT. UP_OMR(IPGRP) + TOL_OMR(IECP)) ITST = 1
                        IF (EHGSUB(JN) .LT. UP_GSUB(IPGRP) - TOL_GSUB(IECP)) ITST = 1
                        IF (EHGSUB(JN) .GT. UP_GSUB(IPGRP) + TOL_GSUB(IECP)) ITST = 1
                        DO ISP = 1 , EENSP
                           T_ACF = DBLE(EHHYCF(JN,ISP)) * EFACTR * EETIME(ISP) / TOTHRS
                        END DO
                        IF (T_ACF .LT. UP_ACF(IPGRP) - TOL_ACF(IECP)) ITST = 1
                        IF (T_ACF .GT. UP_ACF(IPGRP) + TOL_ACF(IECP)) ITST = 1
                        DO ISP = 1 , EENSP
                           IF (ULHTRT_EFD(JPGRP,ISP) .LT. UP_HTRT(ISP,IPGRP) - TOL_HTRT(IECP)) ITST = 1
                           IF (ULHTRT_EFD(JPGRP,ISP) .GT. UP_HTRT(ISP,IPGRP) + TOL_HTRT(IECP)) ITST = 1
                           IF (DBLE(EHHYCF(JN,ISP)) * EFACTR .LT. UP_SCF(ISP,IPGRP) - TOL_SCF(IECP)) ITST = 1
                           IF (DBLE(EHHYCF(JN,ISP)) * EFACTR .GT. UP_SCF(ISP,IPGRP) + TOL_SCF(IECP)) ITST = 1
                           DO IVLS = 1 , ELNVCT(ISP)
                              LGRP = ELGRP(IVLS,ISP)
                              LSEG = ELSEG(IVLS,ISP)
                              LHRS = ELWDTH(IVLS,ISP)
                              IF (DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR .LT. UP_GCF(LSEG,LGRP,IPGRP) - TOL_GCF(IECP)) ITST = 1
                              IF (DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR .GT. UP_GCF(LSEG,LGRP,IPGRP) + TOL_GCF(IECP)) ITST = 1
                           END DO
                           DO INOX = 1 , NOX_GRP
                              IF (EHFNOXC(INOX,JN,ISP) .LT. UP_NOXC(ISP,INOX,IPGRP) - TOL_NOXC(IECP)) ITST = 1
                              IF (EHFNOXC(INOX,JN,ISP) .GT. UP_NOXC(ISP,INOX,IPGRP) + TOL_NOXC(IECP)) ITST = 1
                           END DO
                        END DO
                        IF (WFOR(JP) .LT. UP_FOR(IPGRP) - TOL_FOR(IECP)) ITST = 1
                        IF (WFOR(JP) .GT. UP_FOR(IPGRP) + TOL_FOR(IECP)) ITST = 1
                        IF (WPMR(JP) .LT. UP_PMR(IPGRP) - TOL_PMR(IECP)) ITST = 1
                        IF (WPMR(JP) .GT. UP_PMR(IPGRP) + TOL_PMR(IECP)) ITST = 1
                        IF (0.0 .LT. UP_LFR(IPGRP) - TOL_LFR(IECP)) ITST = 1
                        IF (0.0 .GT. UP_LFR(IPGRP) + TOL_LFR(IECP)) ITST = 1
!
                        IF (ITST .EQ. 0) THEN
                           UP_EMM_RG(JPGRP) = IRG
                           UP_FL_RG(JPGRP) = EPNFLRG(EHCR(JN),EHLR(JN),EHGR(JN),EHAR(JN))
                           UP_ECPt(JPGRP) = JECP
                           UP_EFDt(JPGRP) = JP
                           UP_MRUN(JPGRP) = ULMRUN(JPGRP)
                           MAP_EHNTP_EFD_GRPS(JN,IRG) = N_EFD_GRPS
                           IF (EFD_GRPS_N(IPGRP) .EQ. 0) THEN
                              EFD_GRPS_N(IPGRP) = JPGRP
                              KPGRP = JPGRP
                           ELSE
                              IF (EFD_GRPS_N(KPGRP) .EQ. JPGRP) THEN
                                 WRITE(6,2317) CURIRUN, CURIYR+1989, CURITR, N_EFD_GRPS, JN, IPGRP, JPGRP, KPGRP, &
                                    EFD_GRPS_F(IPGRP), EFD_GRPS_N(KPGRP), IECP, UG_CAP(1,N_EFD_GRPS), EHCAP(JN,1)
                              ELSE
                                 EFD_GRPS_N(KPGRP) = JPGRP
                                 KPGRP = JPGRP
                              END IF
                           END IF

!                          WRITE(18,9133) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, ULORGN(JPGRP), N_EFD_GRPS, JPGRP, N, JN, JP, JECP, ULECPT(JPGRP), ULEFDT(JPGRP), ULMRUN(JPGRP), &
!                             ULFRGN(JPGRP), ULIGRP(JPGRP), UPLNTCD(JECP), &
!                             (EHCAP(JN,ISP),ISP=1,EENSP), UPHTRT(JECP), (ULSCAP_EFD(JPGRP,ISP), ULHTRT_EFD(JPGRP,ISP),ISP=1,EENSP)

                           DO ISP = 1 , EENSP
                              UG_CAP(ISP,N_EFD_GRPS) = UG_CAP(ISP,N_EFD_GRPS) + EHCAP(JN,ISP)
                              UG_CAP(0,N_EFD_GRPS) = UG_CAP(0,N_EFD_GRPS) + EHCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UP_CAP(ISP,JPGRP) = EHCAP(JN,ISP)
                              UP_CAP(0,JPGRP) = UP_CAP(0,JPGRP) + EHCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_GEN(ISP,N_EFD_GRPS) = UG_GEN(ISP,N_EFD_GRPS) + EHCAP(JN,ISP) * DBLE(EHHYCF(JN,ISP)) * EFACTR * EETIME(ISP) * 0.001
                              UG_GEN(0,N_EFD_GRPS) = UG_GEN(0,N_EFD_GRPS) + EHCAP(JN,ISP) * DBLE(EHHYCF(JN,ISP)) * EFACTR * EETIME(ISP) * 0.001
                              UP_GEN(ISP,JPGRP) = EHCAP(JN,ISP) * EHHYCF(JN,ISP) * EFACTR * EETIME(ISP) * 0.001
                              UP_GEN(0,JPGRP) = UP_GEN(0,JPGRP) + EHCAP(JN,ISP) * EHHYCF(JN,ISP) * EFACTR * EETIME(ISP) * 0.001
                              UG_OMR(N_EFD_GRPS) = UG_OMR(N_EFD_GRPS) + EHVOMR(JN) * EHCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_GSUB(N_EFD_GRPS) = UG_GSUB(N_EFD_GRPS) + EHGSUB(JN) * EHCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_HTRT(ISP,N_EFD_GRPS) = UG_HTRT(ISP,N_EFD_GRPS) + ULHTRT_EFD(JPGRP,ISP) * EHCAP(JN,ISP)
                              UG_ACF(N_EFD_GRPS) = UG_ACF(N_EFD_GRPS) + DBLE(EHHYCF(JN,ISP)) * EFACTR * EHCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UP_ACF(JPGRP) = UP_ACF(JPGRP) + DBLE(EHHYCF(JN,ISP)) * EFACTR * EETIME(ISP) / TOTHRS
                              UG_SCF(ISP,N_EFD_GRPS) = UG_SCF(ISP,N_EFD_GRPS) + DBLE(EHHYCF(JN,ISP)) * EFACTR * EHCAP(JN,ISP)
                              UP_SCF(ISP,JPGRP) = DBLE(EHHYCF(JN,ISP)) * EFACTR
                              DO IVLS = 1 , ELNVCT(ISP)
                                 LGRP = ELGRP(IVLS,ISP)
                                 LSEG = ELSEG(IVLS,ISP)
                                 LHRS = ELWDTH(IVLS,ISP)
                                 IF (JNT .GT. 0) THEN
                                    IF (CURCALYR .GE. UPSTYR) THEN
                                        IF(IECP .EQ. WIPT) THEN
                                              UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) +  EHCAP(JN,ISP) * HREFDCF(1,JNT,1,LSEG,LGRP,IRG)
                                              UP_GCF(LSEG,LGRP,JPGRP) =  HREFDCF(1,JNT,1,LSEG,LGRP,IRG) 
                                        ! Distributed PV is second RESTORE step
										ELSE IF (IECP .EQ. WIPV .AND. UG_MRUN(N_EFD_GRPS) .GT. 0) THEN
										      IF (HREFDCF(0,JNT,1,LSEG,LGRP,IRG) .GT. 0.0001) THEN
										      UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) +  EHCAP(JN,ISP) * (HREFDCF(1,JNT,2,LSEG,LGRP,IRG)/ HREFDCF(0,JNT,1,LSEG,LGRP,IRG)) * DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR !* (DPVTOTGENNR(IRG,CURIYR) / 8.76 * DPVTOTCAPNR(IRG,CURIYR))
                                              UP_GCF(LSEG,LGRP,JPGRP) = (HREFDCF(1,JNT,2,LSEG,LGRP,IRG)/ HREFDCF(0,JNT,1,LSEG,LGRP,IRG))*  DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR !* (DPVTOTGENNR(IRG,CURIYR) / 8.76 * DPVTOTCAPNR(IRG,CURIYR))
                                              
                                        WRITE(18,9134) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, N_EFD_GRPS, JPGRP, N, JN, JP, JECP, IECP,UG_MRUN(N_EFD_GRPS),ISP,LSEG, LGRP,LHRS,DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR, UG_CAP(ISP,I_EFD_GRPS),&
                                        HREFDCF(1,JNT,2,LSEG,LGRP,IRG), HREFDCF(0,JNT,1,LSEG,LGRP,IRG),ULMRUN(JPGRP), &
                                            DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR  * (HREFDCF(1,JNT,2,LSEG,LGRP,IRG)/ HREFDCF(0,JNT,1,LSEG,LGRP,IRG)),&
                             DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR * EHCAP(JN,ISP) * (HREFDCF(1,JNT,2,LSEG,LGRP,IRG)/ HREFDCF(0,JNT,1,LSEG,LGRP,IRG)), &
                             EHCAP(JN,ISP), UG_GCF(LSEG,LGRP,N_EFD_GRPS), DPVTOTGENNR(IRG,CURIYR) / (8.76 * DPVTOTCAPNR(IRG,CURIYR)), DPVTOTGENNR(IRG,CURIYR),  DPVTOTCAPNR(IRG,CURIYR)
                             
                          9134          FORMAT(1X,"UG_GCF_UEFD2",14(":",I6),":",13(":",F21.6))
                          					ELSE
                          						UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR * EHCAP(JN,ISP)
                                            	UP_GCF(LSEG,LGRP,JPGRP) = DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR
                          					ENDIF
                                        ELSE
                                    
                                           IF (HREFDCF(0,JNT,1,LSEG,LGRP,IRG) .GT. 0.0001) THEN
                                              UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR * EHCAP(JN,ISP) * (HREFDCF(1,JNT,1,LSEG,LGRP,IRG) / HREFDCF(0,JNT,1,LSEG,LGRP,IRG))
                                              UP_GCF(LSEG,LGRP,JPGRP) = DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR * (HREFDCF(1,JNT,1,LSEG,LGRP,IRG) / HREFDCF(0,JNT,1,LSEG,LGRP,IRG))

                                           ELSE
                                              UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR * EHCAP(JN,ISP)
                                              UP_GCF(LSEG,LGRP,JPGRP) = DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR
                                           END IF
                                        END IF
                                    ELSE
                                       UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR * EHCAP(JN,ISP)
                                       UP_GCF(LSEG,LGRP,JPGRP) = DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR
                                    END IF
                                 ELSE
                                    UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR * EHCAP(JN,ISP)
                                    UP_GCF(LSEG,LGRP,JPGRP) = DBLE(EHLDCF(JN,LSEG,LGRP)) * EFACTR
                                 END IF
                              END DO
                              UG_FOR(N_EFD_GRPS) = WFOR(JP)
                              UG_PMR(N_EFD_GRPS) = WPMR(JP)
                              UG_LFR(N_EFD_GRPS) = 0.0
                              DO INOX = 1 , NOX_GRP
                                 UG_NOXC(ISP,INOX,N_EFD_GRPS) = UG_NOXC(ISP,INOX,N_EFD_GRPS) + EHFNOXC(INOX,JN,ISP) * EHCAP(JN,ISP)
                                 UP_NOXC(ISP,INOX,JPGRP) = EHFNOXC(INOX,JN,ISP)
                              END DO
                              UP_HTRT(ISP,JPGRP) = ULHTRT_EFD(JPGRP,ISP)
                           END DO
                           UP_OMR(JPGRP) = EHVOMR(JN)
                           UP_GSUB(JPGRP) = EHGSUB(JN)
                           UP_FOR(JPGRP) = WFOR(JP)
                           UP_PMR(JPGRP) = WPMR(JP)
                           UP_LFR(JPGRP) = 0.0
                        END IF
                     END IF
                  END DO
               END IF
            END IF
         ENDDO
!
!        loop over dist gen plant types
!
         ITYP = 3
         DO N = 1, EDNTP
            IF (MAP_EDNTP_EFD_GRPS(N,IRG) .EQ. 0) THEN
               IP = EDASTS(N)
               IPGRP = EDDBID(N)
               IECP = EDTECP(N)
               N_EFD_GRPS = N_EFD_GRPS + 1
               MAP_EDNTP_EFD_GRPS(N,IRG) = N_EFD_GRPS
               EFD_GRPS_F(N_EFD_GRPS) = IPGRP
               UG_EMM_RG(N_EFD_GRPS) = IRG
               UP_EMM_RG(IPGRP) = IRG
               UG_FL_RG(N_EFD_GRPS) = EPNFLRG(EDCR(N),EDLR(N),EDGR(N),EDAR(N))
               UP_FL_RG(IPGRP) = EPNFLRG(EDCR(N),EDLR(N),EDGR(N),EDAR(N))
               UG_ECPt(N_EFD_GRPS) = IECP
               UP_ECPt(IPGRP) = IECP
               UG_EFDt(N_EFD_GRPS) = IP
               UP_EFDt(IPGRP) = IP
               UG_MRUN(N_EFD_GRPS) = ULMRUN(IPGRP)
               UP_MRUN(IPGRP) = ULMRUN(IPGRP)

!              WRITE(18,9133) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, ULORGN(IPGRP), N_EFD_GRPS, IPGRP, N, N, IP, IECP, ULECPT(IPGRP), ULEFDT(IPGRP), ULMRUN(IPGRP), &
!                 ULFRGN(IPGRP), ULIGRP(IPGRP), UPLNTCD(IECP), &
!                 (EDCAP(N,ISP),ISP=1,EENSP), UPHTRT(IECP), (ULSCAP_EFD(IPGRP,ISP), ULHTRT_EFD(IPGRP,ISP),ISP=1,EENSP)

               DO ISP = 1 , EENSP
                  UG_CAP(ISP,N_EFD_GRPS) = UG_CAP(ISP,N_EFD_GRPS) + EDCAP(N,ISP)
                  UG_CAP(0,N_EFD_GRPS) = UG_CAP(0,N_EFD_GRPS) + EDCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UP_CAP(ISP,IPGRP) = EDCAP(N,ISP)
                  UP_CAP(0,IPGRP) = UP_CAP(0,IPGRP) + EDCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_GEN(ISP,N_EFD_GRPS) = UG_GEN(ISP,N_EFD_GRPS) + EDCAP(N,ISP) * DBLE(EDMXCP(N)) * EFACTR * EETIME(ISP) * 0.001
                  UG_GEN(0,N_EFD_GRPS) = UG_GEN(0,N_EFD_GRPS) + EDCAP(N,ISP) * DBLE(EDMXCP(N)) * EFACTR * EETIME(ISP) * 0.001
                  UP_GEN(ISP,IPGRP) = EDCAP(N,ISP) * EDMXCP(N) * EFACTR * EETIME(ISP) * 0.001
                  UP_GEN(0,IPGRP) = UP_GEN(0,IPGRP) + EDCAP(N,ISP) * EDMXCP(N) * EFACTR * EETIME(ISP) * 0.001
                  UG_OMR(N_EFD_GRPS) = UG_OMR(N_EFD_GRPS) + EDVOMR(N,1) * EDCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_GSUB(N_EFD_GRPS) = 0.0
                  UG_HTRT(ISP,N_EFD_GRPS) = UG_HTRT(ISP,N_EFD_GRPS) + ULHTRT_EFD(IPGRP,ISP) * EDCAP(N,ISP)
                  UG_ACF(N_EFD_GRPS) = UG_ACF(N_EFD_GRPS) + DBLE(EDMXCP(N)) * EFACTR * EDCAP(N,ISP) * EETIME(ISP) / TOTHRS
                  UG_SCF(ISP,N_EFD_GRPS) = UG_SCF(ISP,N_EFD_GRPS) + DBLE(EDMXCP(N)) * EFACTR * EDCAP(N,ISP)
                  UP_SCF(ISP,IPGRP) = DBLE(EDMXCP(N)) * EFACTR
                  DO IVLS = 1 , ELNVCT(ISP)
                     LGRP = ELGRP(IVLS,ISP)
                     LSEG = ELSEG(IVLS,ISP)
                     LHRS = ELWDTH(IVLS,ISP)
                     UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EDMXCP(N)) * EFACTR * EDCAP(N,ISP)
                     UP_GCF(LSEG,LGRP,IPGRP) = DBLE(EDMXCP(N)) * EFACTR
                  END DO
                  UG_FOR(N_EFD_GRPS) = WFOR(IP)
                  UG_PMR(N_EFD_GRPS) = WPMR(IP)
                  UG_LFR(N_EFD_GRPS) = 0.0
                  DO INOX = 1 , NOX_GRP
                     UG_NOXC(ISP,INOX,N_EFD_GRPS) = UG_NOXC(ISP,INOX,N_EFD_GRPS) + EDFNOXC(INOX,N,ISP) * EDCAP(N,ISP)
                     UP_NOXC(ISP,INOX,IPGRP) = EDFNOXC(INOX,N,ISP)
                  END DO
                  UP_HTRT(ISP,IPGRP) = ULHTRT_EFD(IPGRP,ISP)
               END DO
               UP_OMR(IPGRP) = EDVOMR(N,1)
               UP_GSUB(IPGRP) = 0.0
               UP_ACF(IPGRP) = DBLE(EDMXCP(N)) * EFACTR
               UP_FOR(IPGRP) = WFOR(IP)
               UP_PMR(IPGRP) = WPMR(IP)
               UP_LFR(IPGRP) = 0.0
               IF (N .LT. EDNTP) THEN
                  DO JN = N + 1 , EDNTP
                     IF (MAP_EDNTP_EFD_GRPS(JN,IRG) .EQ. 0) THEN
                        ITST = 0
                        JP = EDASTS(JN)
                        JPGRP = EDDBID(JN)
                        JECP = EDTECP(JN)
                        IF (IP .NE. JP) ITST = 1
                        IF (ULMRUN(IPGRP) .NE. ULMRUN(JPGRP)) ITST = 1
                        IF (IECP .NE. JECP) ITST = 1
                        IF (UG_FL_RG(N_EFD_GRPS) .NE. EPNFLRG(EDCR(JN),EDLR(JN),EDGR(JN),EDAR(JN))) ITST = 1
                        IF (EDVOMR(JN,1) .LT. UP_OMR(IPGRP) - TOL_OMR(IECP)) ITST = 1
                        IF (EDVOMR(JN,1) .GT. UP_OMR(IPGRP) + TOL_OMR(IECP)) ITST = 1
                        IF (0.0 .LT. UP_GSUB(IPGRP) - TOL_GSUB(IECP)) ITST = 1
                        IF (0.0 .GT. UP_GSUB(IPGRP) + TOL_GSUB(IECP)) ITST = 1
                        IF (DBLE(EDMXCP(JN)) * EFACTR .LT. UP_ACF(IPGRP) - TOL_ACF(IECP)) ITST = 1
                        IF (DBLE(EDMXCP(JN)) * EFACTR .GT. UP_ACF(IPGRP) + TOL_ACF(IECP)) ITST = 1
                        DO ISP = 1 , EENSP
                           IF (ULHTRT_EFD(JPGRP,ISP) .LT. UP_HTRT(ISP,IPGRP) - TOL_HTRT(IECP)) ITST = 1
                           IF (ULHTRT_EFD(JPGRP,ISP) .GT. UP_HTRT(ISP,IPGRP) + TOL_HTRT(IECP)) ITST = 1
                           IF (DBLE(EDMXCP(JN)) * EFACTR .LT. UP_SCF(ISP,IPGRP) - TOL_SCF(IECP)) ITST = 1
                           IF (DBLE(EDMXCP(JN)) * EFACTR .GT. UP_SCF(ISP,IPGRP) + TOL_SCF(IECP)) ITST = 1
                           DO IVLS = 1 , ELNVCT(ISP)
                              LGRP = ELGRP(IVLS,ISP)
                              LSEG = ELSEG(IVLS,ISP)
                              LHRS = ELWDTH(IVLS,ISP)
                              IF (DBLE(EDMXCP(JN)) * EFACTR .LT. UP_GCF(LSEG,LGRP,IPGRP) - TOL_GCF(IECP)) ITST = 1
                              IF (DBLE(EDMXCP(JN)) * EFACTR .GT. UP_GCF(LSEG,LGRP,IPGRP) + TOL_GCF(IECP)) ITST = 1
                           END DO
                           DO INOX = 1 , NOX_GRP
                              IF (EDFNOXC(INOX,JN,ISP) .LT. UP_NOXC(ISP,INOX,IPGRP) - TOL_NOXC(IECP)) ITST = 1
                              IF (EDFNOXC(INOX,JN,ISP) .GT. UP_NOXC(ISP,INOX,IPGRP) + TOL_NOXC(IECP)) ITST = 1
                           END DO
                        END DO
                        IF (WFOR(JP) .LT. UP_FOR(IPGRP) - TOL_FOR(IECP)) ITST = 1
                        IF (WFOR(JP) .GT. UP_FOR(IPGRP) + TOL_FOR(IECP)) ITST = 1
                        IF (WPMR(JP) .LT. UP_PMR(IPGRP) - TOL_PMR(IECP)) ITST = 1
                        IF (WPMR(JP) .GT. UP_PMR(IPGRP) + TOL_PMR(IECP)) ITST = 1
                        IF (0.0 .LT. UP_LFR(IPGRP) - TOL_LFR(IECP)) ITST = 1
                        IF (0.0 .GT. UP_LFR(IPGRP) + TOL_LFR(IECP)) ITST = 1
!
                        IF (ITST .EQ. 0) THEN
                           UP_EMM_RG(JPGRP) = IRG
                           UP_FL_RG(JPGRP) = EPNFLRG(EDCR(JN),EDLR(JN),EDGR(JN),EDAR(JN))
                           UP_ECPt(JPGRP) = JECP
                           UP_EFDt(JPGRP) = JP
                           UP_MRUN(JPGRP) = ULMRUN(JPGRP)
                           MAP_EDNTP_EFD_GRPS(JN,IRG) = N_EFD_GRPS
                           IF (EFD_GRPS_N(IPGRP) .EQ. 0) THEN
                              EFD_GRPS_N(IPGRP) = JPGRP
                              KPGRP = JPGRP
                           ELSE
                              IF (EFD_GRPS_N(KPGRP) .EQ. JPGRP) THEN
                                 WRITE(6,2317) CURIRUN, CURIYR+1989, CURITR, N_EFD_GRPS, JN, IPGRP, JPGRP, KPGRP, &
                                    EFD_GRPS_F(IPGRP), EFD_GRPS_N(KPGRP), IECP, UG_CAP(1,N_EFD_GRPS), EDCAP(JN,1)
                              ELSE
                                 EFD_GRPS_N(KPGRP) = JPGRP
                                 KPGRP = JPGRP
                              END IF
                           END IF

!                          WRITE(18,9133) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, ULORGN(JPGRP), N_EFD_GRPS, JPGRP, N, JN, JP, JECP, ULECPT(JPGRP), ULEFDT(JPGRP), ULMRUN(JPGRP), &
!                             ULFRGN(JPGRP), ULIGRP(JPGRP), UPLNTCD(JECP), &
!                             (EDCAP(JN,ISP),ISP=1,EENSP), UPHTRT(JECP), (ULSCAP_EFD(JPGRP,ISP), ULHTRT_EFD(JPGRP,ISP),ISP=1,EENSP)

                           DO ISP = 1 , EENSP
                              UG_CAP(ISP,N_EFD_GRPS) = UG_CAP(ISP,N_EFD_GRPS) + EDCAP(JN,ISP)
                              UG_CAP(0,N_EFD_GRPS) = UG_CAP(0,N_EFD_GRPS) + EDCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UP_CAP(ISP,JPGRP) = EDCAP(JN,ISP)
                              UP_CAP(0,JPGRP) = UP_CAP(0,JPGRP) + EDCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_GEN(ISP,N_EFD_GRPS) = UG_GEN(ISP,N_EFD_GRPS) + EDCAP(JN,ISP) * DBLE(EDMXCP(JN)) * EFACTR * EETIME(ISP) * 0.001
                              UG_GEN(0,N_EFD_GRPS) = UG_GEN(0,N_EFD_GRPS) + EDCAP(JN,ISP) * DBLE(EDMXCP(JN)) * EFACTR * EETIME(ISP) * 0.001
                              UP_GEN(ISP,JPGRP) = EDCAP(JN,ISP) * EDMXCP(JN) * EFACTR * EETIME(ISP) * 0.001
                              UP_GEN(0,JPGRP) = UP_GEN(0,JPGRP) + EDCAP(JN,ISP) * EDMXCP(JN) * EFACTR * EETIME(ISP) * 0.001
                              UG_OMR(N_EFD_GRPS) = UG_OMR(N_EFD_GRPS) + EDVOMR(JN,1) * EDCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_GSUB(N_EFD_GRPS) = 0.0
                              UG_HTRT(ISP,N_EFD_GRPS) = UG_HTRT(ISP,N_EFD_GRPS) + ULHTRT_EFD(JPGRP,ISP) * EDCAP(JN,ISP)
                              UG_ACF(N_EFD_GRPS) = UG_ACF(N_EFD_GRPS) + DBLE(EDMXCP(JN)) * EFACTR * EDCAP(JN,ISP) * EETIME(ISP) / TOTHRS
                              UG_SCF(ISP,N_EFD_GRPS) = UG_SCF(ISP,N_EFD_GRPS) + DBLE(EDMXCP(JN)) * EFACTR * EDCAP(JN,ISP)
                              UP_SCF(ISP,JPGRP) = DBLE(EDMXCP(JN)) * EFACTR
                              DO IVLS = 1 , ELNVCT(ISP)
                                 LGRP = ELGRP(IVLS,ISP)
                                 LSEG = ELSEG(IVLS,ISP)
                                 LHRS = ELWDTH(IVLS,ISP)
                                 UG_GCF(LSEG,LGRP,N_EFD_GRPS) = UG_GCF(LSEG,LGRP,N_EFD_GRPS) + DBLE(EDMXCP(JN)) * EFACTR * EDCAP(JN,ISP)
                                 UP_GCF(LSEG,LGRP,JPGRP) = DBLE(EDMXCP(JN)) * EFACTR
                              END DO
                              UG_FOR(N_EFD_GRPS) = WFOR(JP)
                              UG_PMR(N_EFD_GRPS) = WPMR(JP)
                              UG_LFR(N_EFD_GRPS) = 0.0
                              DO INOX = 1 , NOX_GRP
                                 UG_NOXC(ISP,INOX,N_EFD_GRPS) = UG_NOXC(ISP,INOX,N_EFD_GRPS) + EDFNOXC(INOX,JN,ISP) * EDCAP(JN,ISP)
                                 UP_NOXC(ISP,INOX,JPGRP) = EDFNOXC(INOX,JN,ISP)
                              END DO
                              UP_HTRT(ISP,JPGRP) = ULHTRT_EFD(JPGRP,ISP)
                           END DO
                           UP_OMR(JPGRP) = EDVOMR(JN,1)
                           UP_GSUB(JPGRP) = 0.0
                           UP_ACF(JPGRP) = DBLE(EDMXCP(JN)) * EFACTR
                           UP_FOR(JPGRP) = WFOR(JP)
                           UP_PMR(JPGRP) = WPMR(JP)
                           UP_LFR(JPGRP) = 0.0
                        END IF
                     END IF
                  END DO
               END IF
            END IF
         ENDDO
      END DO
!
      TST_GRPS = 0
      KPGRP = 0
      DO I_EFD_GRPS = 1 , N_EFD_GRPS
         IP = UG_EFDt(I_EFD_GRPS)
         IECP = UG_ECPt(I_EFD_GRPS)
         UG_OMR(I_EFD_GRPS) = UG_OMR(I_EFD_GRPS) / UG_CAP(0,I_EFD_GRPS)
         UG_GSUB(I_EFD_GRPS) = UG_GSUB(I_EFD_GRPS) / UG_CAP(0,I_EFD_GRPS)
         DO ISP = 1 , EENSP
            IF (UG_CAP(ISP,I_EFD_GRPS) .GT. 0.0) THEN
               UG_HTRT(ISP,I_EFD_GRPS) = UG_HTRT(ISP,I_EFD_GRPS) / UG_CAP(ISP,I_EFD_GRPS)
               UG_SCF(ISP,I_EFD_GRPS) = UG_SCF(ISP,I_EFD_GRPS) / UG_CAP(ISP,I_EFD_GRPS)
               DO IVLS = 1 , ELNVCT(ISP)
                  LGRP = ELGRP(IVLS,ISP)
                  LSEG = ELSEG(IVLS,ISP)
                  UG_GCF(LSEG,LGRP,I_EFD_GRPS) = UG_GCF(LSEG,LGRP,I_EFD_GRPS) / UG_CAP(ISP,I_EFD_GRPS)
               END DO
               DO INOX = 1 , NOX_GRP
                  UG_NOXC(ISP,INOX,I_EFD_GRPS) = UG_NOXC(ISP,INOX,I_EFD_GRPS) / UG_CAP(ISP,I_EFD_GRPS)
               END DO
            ELSE
               UG_HTRT(ISP,I_EFD_GRPS) = UPHTRT(IECP)
            END IF
         END DO
         UG_ACF(I_EFD_GRPS) = UG_ACF(I_EFD_GRPS) / UG_CAP(0,I_EFD_GRPS)
         IF (IP .LE. EFD_D_DSP) THEN
            UG_FOR(I_EFD_GRPS) = UG_FOR(I_EFD_GRPS) / UG_CAP(0,I_EFD_GRPS)
            UG_PMR(I_EFD_GRPS) = UG_PMR(I_EFD_GRPS) / UG_CAP(0,I_EFD_GRPS)
            UG_LFR(I_EFD_GRPS) = UG_LFR(I_EFD_GRPS) / UG_CAP(0,I_EFD_GRPS)
         END IF

!        Make PMR, FOR and implied LFR Consistent with MAXCF,
!        Unless LFR Exceeds maximum specified then Use Max LFR and Split difference between FOR and PMR

         IF (IP .LE. EFD_D_DSP) THEN
            MAXCF = UG_ACF(I_EFD_GRPS)
            MAXCF = MIN(MAX(MAXCF,0.001),0.998)
            MXLFR = 0.0
            TFOR = WFOR(IP)
            TPMR = WPMR(IP)
            TLFR = 1.0 - (WMXCP(IP) / ((1.0 - WFOR(IP)) * (1.0 - WPMR(IP))))
            FCTR = (MAXCF / WMXCP(IP)) ** (DBLE(1.0) / DBLE(3.0))
            TLFR = DBLE(1.0) - FCTR * (DBLE(1.0) - TLFR)
            IF (TLFR .LE. MXLFR .AND. TLFR .GE. 0.0) THEN
               TFOR = DBLE(1.0) - FCTR * (DBLE(1.0) - TFOR)
               TPMR = DBLE(1.0) - FCTR * (DBLE(1.0) - TPMR)
            ELSE IF (TLFR .LT. 0.0) THEN
               FCTR = (MAXCF / UPMCF(IECP)) ** (DBLE(1.0) / DBLE(2.0))
               TFOR = DBLE(1.0) - FCTR * (DBLE(1.0) - TFOR)
               TPMR = DBLE(1.0) - FCTR * (DBLE(1.0) - TPMR)
               TLFR = 0.0
            ELSE
               TLFR = MXLFR
               FCTR = (MAXCF / ((1.0 - WFOR(IP)) * (1.0 - WPMR(IP)) * (1.0 - TLFR))) ** 0.5
               TFOR = DBLE(1.0) - FCTR * (DBLE(1.0) - TFOR)
               TPMR = DBLE(1.0) - FCTR * (DBLE(1.0) - TPMR)
            END IF
!
            IF (TLFR .LT. 0.0 .OR. TFOR .LT. 0.0 .OR. TPMR .LE. 0) THEN
               TLFR = 1.0 - (MAXCF ** (1.0 / 3.0))
               TFOR = 1.0 - (MAXCF ** (1.0 / 3.0))
               TPMR = 1.0 - (MAXCF ** (1.0 / 3.0))
            END IF
!
            UG_ACF(I_EFD_GRPS) = MAXCF
            UG_FOR(I_EFD_GRPS) = TFOR
            UG_PMR(I_EFD_GRPS) = TPMR
            UG_LFR(I_EFD_GRPS) = TLFR
         END IF

         IF (CURIYR + 1989 .eq. 2010 .OR. CURIYR+1989 .GE. 2017 .AND. CURIYR+1989 .LE. 2040) THEN

            WRITE(18,2000) CURIRUN,CURIYR+1989,CURITR,I_EFD_GRPS, UG_EMM_RG(I_EFD_GRPS), UG_FL_RG(I_EFD_GRPS), UG_ECPt(I_EFD_GRPS), UG_EFDt(I_EFD_GRPS), UG_45Q(I_EFD_GRPS), &
               UGNOCCS(I_EFD_GRPS), (UG_CAP(ISP,I_EFD_GRPS),ISP=0,EENSP), (UG_GEN(JSP,I_EFD_GRPS)*0.001,JSP=0,EENSP), UG_OMR(I_EFD_GRPS), UG_GSUB(I_EFD_GRPS), &
               (UG_HTRT(ISP,I_EFD_GRPS),ISP=1,EENSP), UG_ACF(I_EFD_GRPS), UG_FOR(I_EFD_GRPS), UG_PMR(I_EFD_GRPS), UG_LFR(I_EFD_GRPS), (UG_SCF(KSP,I_EFD_GRPS),KSP=1,1), &
               ((UG_GCF(LSEG,LGRP,I_EFD_GRPS),LSEG=1,1),LGRP=1,1), ((UG_NOXC(LSP,INOX,I_EFD_GRPS),LSP=1,EENSP),INOX=1,NOX_GRP)
 2000          FORMAT("UG_DATA",",",I2,",",I4,",",I2,",",I5,5(",",I2),",",F7.3,4(",",F7.0),6(",",F9.4),3(",",F7.0),6(",",F6.3),9(",",F6.3))

            IPGRP = EFD_GRPS_F(I_EFD_GRPS)
            DO WHILE (IPGRP .GT. 0)
               WRITE(18,2001) CURIRUN, CURIYR+1989, CURITR, I_EFD_GRPS, IPGRP, UP_EMM_RG(IPGRP), UP_FL_RG(IPGRP), UP_ECPt(IPGRP), UP_EFDt(IPGRP), UP_45Q(IPGRP), &
                  UPNOCCS(IPGRP), ECMRUNCF(IPGRP)*EFACTR,(UP_CAP(ISP,IPGRP),ISP=0,EENSP), (UP_GEN(JSP,IPGRP)*0.001,JSP=0,EENSP), UP_OMR(IPGRP), UP_GSUB(IPGRP), &
                  (UP_HTRT(ISP,IPGRP),ISP=1,EENSP), UP_ACF(IPGRP), UP_FOR(IPGRP), UP_PMR(IPGRP), UP_LFR(IPGRP), (UP_SCF(KSP,IPGRP),KSP=1,1), &
                  ((UP_GCF(LSEG,LGRP,IPGRP),LSEG=1,1),LGRP=1,1), ((UP_NOXC(LSP,INOX,IPGRP),LSP=1,EENSP),INOX=1,NOX_GRP)
 2001          FORMAT("UP_DATA",",",I2,",",I4,",",I2,2(",",I5),5(",",I2),2(",",F7.3),4(",",F7.0),4(",",F7.4),",",F9.4,",",F7.4,3(",",F7.0),6(",",F6.3),9(",",F6.3))
               TST_GRPS(IPGRP) = 1
               IPGRP = EFD_GRPS_N(IPGRP)
               IF (IPGRP .GT. 0) THEN
                  IF (TST_GRPS(IPGRP) .EQ. 1) THEN
                     WRITE(6,3755) CURIRUN, CURIYR+1989, CURITR, I_EFD_GRPS, IPGRP
 3755                FORMAT(1X,"BAD_IPGRP_CHECK_SORT_ORDER_OF_PLTF860_TXT_FILE_1",5(":",I5))
                     IPGRP = 0
                  END IF
                  IF (KPGRP .GE. 100000) THEN
                     WRITE(6,3756) CURIRUN, CURIYR+1989, CURITR, I_EFD_GRPS, IPGRP
 3756                FORMAT(1X,"BAD_IPGRP_CHECK_SORT_ORDER_OF_PLTF860_TXT_FILE_2",5(":",I5))
                     IPGRP = 0
                  END IF
                  KPGRP = KPGRP + 1
               END IF
            END DO

         END IF
      END DO
!
      RETURN
      END
!
!     This subroutine sets up an explicit carbon constraint

      SUBROUTINE ED$CAR
      use efd_row_col

      IMPLICIT NONE



      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'emission'
      include'emoblk'
      include'ecpcntl'
      include'uettout'
      include'cdsparms'
      include'ab32'
      include'uefdout'
      include'csapr'
      include'emmemis'

      REAL*8  VALUE,VALLM,VALRS,VALRB,VALBK,OBJVAL,BNDVAL,DIGITS2
      INTEGER*4  IRET
      CHARACTER*16 ROW,COLUMN,COLLIM,ROWSEQ,ROWAB32,ROWOBJ,ROWOBJ_MASK

      INTEGER*4 GRP,REG

      efdsub='ED$CAR'

      ROW = 'CARBONXX' ; ROW_mask='CARBONXX'
      COLUMN = 'CARESCXX' ; COLUMN_mask='CARESCXX'
      ROWSEQ = 'CARSEQXX' ; ROWSEQ_mask='CARSEQXX'

      IF ((USW_CAR .EQ. 2 .OR. USW_CAR .EQ. 3) .AND. (CURIYR+UHBSYR) .GE. UYR_CAR) THEN

         CALL DROWTYPE(ROW,'L       ',ROW_mask)
         VALUE = -1.0
         CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CAR,1')

         VALUE = EFD_MIN
         IF (ELEC_FLAG /= 0) THEN
            VALUE = EMISSIONS_GOAL(CURIYR)
            OBJVAL = 1000.0
            BNDVAL = 0.0001
         ELSEIF ((PERMIT_FLAG /= 0) .OR. (MARKET_FLAG /= 0)) THEN
            VALUE = EMISSIONS_GOAL(CURIYR) - EMRSC(11,1,CURIYR) - EMCMC(11,1,CURIYR) &
               - EMINCC(11,1,CURIYR) - EMTRC(11,1,CURIYR) - EMNT(11,1,CURIYR) * 0.001
            OBJVAL = 1000.0
            BNDVAL = 0.0001
         ELSEIF (ETAX_FLAG /= 0) THEN
            OBJVAL = EMETAX(1,CURIYR) * 1000.0
            BNDVAL = 1000.0
         ELSE
            OBJVAL = 0.0001
            BNDVAL = 1000.0
         ENDIF

         VALUE = MAX(VALUE, EFD_MIN)
         DIGITS_PARM = 6
         IF (USW_DIGIT .GT. 0)VALUE = DIGITS2(VALUE, DIGITS_PARM)
         CALL DRHS(EFDRHS,ROW,VALUE,ROW_mask,'ED$CAR,2')
         ROWOBJ = EFDOBJ; ROWOBJ_mask=ROWOBJ
         IF (USW_DIGIT .GT. 0)OBJVAL = DIGITS2(OBJVAL,DIGITS_PARM)
         CALL DVAL(COLUMN,ROWOBJ,OBJVAL,COLUMN_mask,ROWOBJ_mask,'ED$CAR,3')
         VALUE = 0.0
         CALL DBND(EFDBND,COLUMN,VALUE,BNDVAL,COLUMN_mask,'ED$CAR,4')
      ELSE

         CALL DROWTYPE(ROW,'N       ',ROW_mask)
      ENDIF

      CALL DROWTYPE(ROWSEQ,'N       ',ROWSEQ_mask)

!     Create vector with carbon emissions from 'other' (MSW & geo)

      COLUMN = 'CARBONOT' ; COLUMN_mask='CARBONOT' 
      VALUE = EMEL(4,1,CURIYR)
      CALL DVAL(COLUMN,ROW,1.0D0,COLUMN_mask,ROW_mask,'ED$CAR,5')
      CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CAR,6')

!     Regional Carbon Constraints, if any
      ROWAB32 = 'CARBONAB' ; ROWAB32_mask='CARBONAB' !
      DO GRP = 1 , CO2_GRP
         IF (GRP .EQ. CARGRP_CA)THEN
            VALLM = DBLE(AB_CAP_TOT(CURIYR) * (1.0 - AB_CSTCONT_FRAC(CURIYR)))

!           IF (CURITR .EQ. 1)THEN
!              IF (VALLM .GT. DBLE(0.0) .AND. VALLM .LT. DBLE(9000.0))THEN
!                 VALRB = DBLE(AB32RBNK)
!              ELSE
!                 VALRB = DBLE(0.0)
!              END IF
!           END IF

         ELSE
            VALLM = DBLE(CO2BYGRP(GRP,CURIYR))
!           IF RGGI, REDUCE CAP BY EMISSIONS CONTAINMENT RESERVE
            IF (GRP .EQ. CARGRP_RG)THEN
               VALLM = VALLM - DBLE(CO2EQGRP(GRP,CURIYR))
            END IF
         END IF

!        For AB32, Adjust Target to Include Industrial and Refinery Caps

         ROW = 'CARBON' // CO2_RG(GRP); call makmsk(ROW_mask,':CARBON:' , CO2_RG(GRP))
         IF (VALLM .GT. DBLE(0.0) .AND. VALLM .LT. DBLE(9000.0))THEN
            CALL DROWTYPE(ROW,'L       ',ROW_mask)
            CALL DRHS(EFDRHS,ROW,DBLE(0.0),ROW_mask,'ED$CAR,7')

!           Column for Cap and Escape Vector

            COLLIM = 'CARLIM' // CO2_RG(GRP); call makmsk(COLLIM_mask,':CARLIM:',CO2_RG(GRP))
            IF (GRP .EQ. CARGRP_CA)THEN
               VALUE = MAX(EFD_MIN,DBLE(AB_AUCTION_P(CURIYR) * 1000.0))

!           ELSE IF (GRP .EQ. CARGRP_RG)THEN
!              VALUE = DBLE(AB_AUCTION_P(CURIYR) * 1000.0)

            ELSE
               VALUE = MAX(ECP_MIN,DBLE(CO2FLGRP(GRP,CURIYR)))
!              if (co2_rg(grp) .eq. 'RG')print *,'!floor',curiyr+1989,co2flgrp(grp,curiyr)
            END IF
            CALL DVAL(COLLIM,EFDOBJ,VALUE,COLLIM_mask,EFDOBJ,'ED$CAR,8')
            CALL DBND(EFDBND,COLLIM,DBLE(0.0),VALLM,COLLIM_mask,'ED$CAR,9')
            VALUE = -1.0
            IF (GRP .NE. CARGRP_CA)THEN
               CALL DVAL(COLLIM,ROW,VALUE,COLLIM_mask,ROW_mask,'ED$CAR,10')
            ELSE
               CALL DVAL(COLLIM,ROWAB32,VALUE,COLLIM_mask,ROWAB32_mask,'ED$CAR,11')
               CALL DRHS(EFDRHS,ROWAB32,DBLE(0.0),ROWAB32_mask,'ED$CAR,12')
            END IF

!           SET ESCAPE VECTOR IN $/METRIC TON CAR

            IF (GRP .NE. CARGRP_CA .AND. (CO2ESGRP(GRP,CURIYR) * SCALPR) .LT. 9000.0)THEN
               COLUMN = 'CARESC' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARESC:',CO2_RG(GRP))
               VALUE = -1.0
               CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CAR,13')
               VALUE = DBLE(CO2ESGRP(GRP,CURIYR) * (44.0 / 12.0))
               CALL DVAL(COLUMN,EFDOBJ,VALUE,COLUMN_mask,EFDOBJ,'ED$CAR,14')
            END IF
         ELSE
            CALL DROWTYPE(ROW,'N       ',ROW_mask)
            IF (GRP .EQ. CARGRP_CA) CALL DROWTYPE(ROWAB32,'N       ',ROWAB32_mask)
         END IF

!        Create vectors to include emissions from firm imports,if appropriate (like AB32)

         DO REG = 1 , UNRGNS
            IF (CO2_IM_BY_RG(REG,GRP) .GT. 0.0)THEN
               COLUMN = 'CARIM' // UPRGCD(REG) // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARIM:',UPRGCD(REG) , CO2_RG(GRP)) 
               VALUE = DBLE(KWH_IM_BY_RG(REG,CURIYR))
               CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CAR,15')
               IF (GRP .EQ. CARGRP_CA)THEN
                  VALUE = DBLE(CO2_IM_BY_CA(CURIYR))
               ELSE
                  VALUE = DBLE(CO2_IM_BY_RG(REG,GRP))
               END IF
               CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CAR,16')
            END IF
            IF (CO2_DF_BY_RG(REG,GRP) .GT. 0.0)THEN
               COLUMN = 'CARDM' // UPRGCD(REG) // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARDM:',UPRGCD(REG) , CO2_RG(GRP))
               VALUE = DBLE(KWH_DM_BY_RG(REG,CURIYR))
               CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CAR,17')
               IF (GRP .EQ. CARGRP_CA)THEN
                  VALUE = DBLE(CO2_DF_BY_CA(CURIYR))
               ELSE
                  VALUE = DBLE(CO2_DF_BY_RG(REG,GRP))
               END IF
               IF (VALUE .GT. EFD_MIN)CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CAR,17.1')
            END IF

!           IF (CO2_OS_BY_RG(REG,GRP) .GT. 0.0)THEN
!              COLUMN = 'CAROS' // UPRGCD(REG) // CO2_RG(GRP)
!              VALUE = DBLE(CO2_OS_BY_RG(REG,GRP))
!              CALL DBND(EFDBND,COLUMN,VALUE,VALUE)
!              VALUE = DBLE(1.0)
!              CALL DVAL(COLUMN,ROW,VALUE)
!           END IF

         END DO

!        For RGGI, include reserves, offsets, and banking, if appropriate

         IF (GRP .EQ. CARGRP_RG)THEN

!           Offsets

            COLUMN = 'CAROFF' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CAROFF:',CO2_RG(GRP))
            VALUE = DBLE(CO2OQGRP(GRP,CURIYR))
            CALL DBND(EFDBND,COLUMN,DBLE(0.0),VALUE,COLUMN_mask,'ED$CAR,18')
            VALUE = DBLE(-1.0)
            CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CAR,19')
            VALUE = MAX(EFD_MIN,DBLE(CO2OPGRP(GRP,CURIYR)) + EFD_MIN)
            CALL DVAL(COLUMN,EFDOBJ,VALUE,COLUMN_mask,EFDOBJ,'ED$CAR,20')

!           Reserves (Cost Containment)

            COLUMN = 'CARRSV' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARRSV:',CO2_RG(GRP))
            VALRS = DBLE(CO2RQGRP(GRP,CURIYR))
            CALL DBND(EFDBND,COLUMN,DBLE(0.0),VALRS,COLUMN_mask,'ED$CAR,21')
            VALUE = DBLE(-1.0)
            CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CAR,22')
            VALUE = MAX(EFD_MIN,DBLE(CO2RPGRP(GRP,CURIYR)))
            CALL DVAL(COLUMN,EFDOBJ,VALUE,COLUMN_mask,EFDOBJ,'ED$CAR,23')

!           Reserves (Emissions Containment)

            COLUMN = 'CARECR' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARECR:',CO2_RG(GRP))
            VALRS = DBLE(CO2EQGRP(GRP,CURIYR))
            CALL DBND(EFDBND,COLUMN,DBLE(0.0),VALRS,COLUMN_mask,'ED$CAR,23.1')
            VALUE = DBLE(-1.0)
            CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CAR,23.2')
            VALUE = MAX(EFD_MIN,DBLE(CO2EPGRP(GRP,CURIYR)))
            CALL DVAL(COLUMN,EFDOBJ,VALUE,COLUMN_mask,EFDOBJ,'ED$CAR,23.3')

!           Banks

!           COLUMN = 'CARBNK' // CO2_RG(GRP)
!           VALBK = DBLE(AB_ALLBANK_AVL(CURIYR - 1))
!           CALL DBND(EFDBND,COLUMN,DBLE(0.0),VALBK)
!           VALUE = DBLE(-1.0)
!           CALL DVAL(COLUMN,ROW,VALUE)
!           VALUE = DBLE(AB_AUCTION_P(CURIYR) * 1000.0) + DBLE(2.0) * EFD_MIN
!           CALL DVAL(COLUMN,EFDOBJ,VALUE)

!           write(6,3344) curiyr+1989,curitr,column,valbk,value
!3344       format(1h ,'!ab32col',i4,i4,1x,a8,2f10.3)
         END IF

!        Create row to accumulate emissions versus cap for AB32

         IF (GRP .EQ. CARGRP_CA)THEN

!           IF (AB_CAP_TOT(CURIYR) .GT. 0.0 .AND. AB_CAP_TOT(CURIYR) .LT. 9000.0)THEN
!              CALL DROWTYPE(ROWAB32,'L       ',ROWAB32_mask)
!              CALL DRHS(EFDRHS,ROWAB32,DBLE(0.0))
!           ELSE
!              ICALL DROWTYPE(ROWAB32,'N       ',ROWAB32_mask)
!           END IF

!           Column for Cap

!           COLUMN = 'CARCAP' // CO2_RG(GRP)
!           VALUE = - 1.0
!           CALL DVAL(COLUMN,ROWAB32,VALUE)
!           VALUE = DBLE(AB_CAP_TOT(CURIYR))
!           CALL DBND(EFDBND,COLUMN,DBLE(0.0),VALUE)
!           CALL DVAL(COLUMN,EFDOBJ,EFD_MIN)

!           Utility Covered emissions

            COLUMN = 'CARUTL' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARUTL:',CO2_RG(GRP))
            VALUE = -1.0
            CALL DVAL(COLUMN,ROW,VALUE,COLUMN_mask,ROW_mask,'ED$CAR,24')
            VALUE = 1.0
            CALL DVAL(COLUMN,ROWAB32,VALUE,COLUMN_mask,ROWAB32_mask,'ED$CAR,25')

!           Industrial covered emissions

            COLUMN = 'CARIND' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARIND:',CO2_RG(GRP))
            VALUE = DBLE(AB_COVD_EM_IND(CURIYR))
            CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CAR,26')
            VALUE = DBLE(1.0)
            CALL DVAL(COLUMN,ROWAB32,VALUE,COLUMN_mask,ROWAB32_mask,'ED$CAR,27')

!           Refinery covered emissions

            COLUMN = 'CARREF' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARREF:',CO2_RG(GRP))
            VALUE = DBLE(AB_COVD_EM_REF(CURIYR))
            CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CAR,28')
            VALUE = DBLE(1.0)
            CALL DVAL(COLUMN,ROWAB32,VALUE,COLUMN_mask,ROWAB32_mask,'ED$CAR,29')

!           Fuel Provider covered emissions

            COLUMN = 'CARFUE' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARFUE:',CO2_RG(GRP))
            VALUE = Max(DBLE(AB_COVD_EM_FUE(CURIYR)), DBLE(0.0))
            CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CAR,30')
            VALUE = DBLE(1.0)
            CALL DVAL(COLUMN,ROWAB32,VALUE,COLUMN_mask,ROWAB32_mask,'ED$CAR,31')

!           Other covered emissions

            COLUMN = 'CAROTH' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CAROTH:',CO2_RG(GRP))
            VALUE = DBLE(AB_COVD_EM_OTH(CURIYR))
            CALL DBND(EFDBND,COLUMN,VALUE,VALUE,COLUMN_mask,'ED$CAR,32')
            VALUE = DBLE(1.0)
            CALL DVAL(COLUMN,ROWAB32,VALUE,COLUMN_mask,ROWAB32_mask,'ED$CAR,33')

!           Offsets

            COLUMN = 'CAROFF' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CAROFF:',CO2_RG(GRP))
            VALUE = DBLE(AB_CAP_TOT(CURIYR) * AB_OFFSET_FRAC(CURIYR))
            CALL DBND(EFDBND,COLUMN,DBLE(0.0),VALUE,COLUMN_mask,'ED$CAR,34')
            VALUE = DBLE(-1.0)
            CALL DVAL(COLUMN,ROWAB32,VALUE,COLUMN_mask,ROWAB32_mask,'ED$CAR,35')
            VALUE = DBLE(AB_AUCTION_P(CURIYR) * 1000.0) + EFD_MIN
            CALL DVAL(COLUMN,EFDOBJ,VALUE,COLUMN_mask,EFDOBJ,'ED$CAR,36')

!           Reserves

            COLUMN = 'CARRSV' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARRSV:',CO2_RG(GRP))
            VALRS = DBLE(AB_CSTCONT_AVL(CURIYR))
            CALL DBND(EFDBND,COLUMN,DBLE(0.0),VALRS,COLUMN_mask,'ED$CAR,37')
            VALUE = DBLE(-1.0)
            CALL DVAL(COLUMN,ROWAB32,VALUE,COLUMN_mask,ROWAB32_mask,'ED$CAR,38')
            VALUE = MAX(EFD_MIN,DBLE(AB_RESERVE_P(2,CURIYR) * 1000.0))
            CALL DVAL(COLUMN,EFDOBJ,VALUE,COLUMN_mask,EFDOBJ,'ED$CAR,39')

!           Banks

            COLUMN = 'CARBNK' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARBNK:',CO2_RG(GRP))
            VALBK = DBLE(AB_ALLBANK_AVL(CURIYR - 1))
            CALL DBND(EFDBND,COLUMN,DBLE(0.0),VALBK,COLUMN_mask,'ED$CAR,40')
            VALUE = DBLE(-1.0)
            CALL DVAL(COLUMN,ROWAB32,VALUE,COLUMN_mask,ROWAB32_mask,'ED$CAR,41')
            VALUE = DBLE(AB_AUCTION_P(CURIYR) * 1000.0) + DBLE(2.0) * EFD_MIN
            CALL DVAL(COLUMN,EFDOBJ,VALUE,COLUMN_mask,EFDOBJ,'ED$CAR,42')

!           Escape Vector (Set just above tier 3 price)

            COLUMN = 'CARESC' // CO2_RG(GRP); call makmsk(COLUMN_mask,':CARESC:',CO2_RG(GRP)) 
            VALUE = -1.0
            CALL DVAL(COLUMN,ROWAB32,VALUE,COLUMN_mask,ROWAB32_mask,'ED$CAR,43')
            VALUE = DBLE(AB_RESERVE_P(3,CURIYR) * 1000.0) + EFD_MIN
            CALL DVAL(COLUMN,EFDOBJ,VALUE,COLUMN_mask,EFDOBJ,'ED$CAR,44')
!           write(6,3344) curiyr+1989,curitr,column,valbk,value
!3344       format(1h ,'!ab32col',i4,i4,1x,a8,2f10.3)
         END IF
      END DO

      RETURN
      END

!     ED$CCAP SET UP STRUCTURE TO TRANSPORT CAPTURED CO2 TO CENSUS REGIONS FOR USE IN CCATS for EOR or storage

      SUBROUTINE ED$CCAP
      use efd_row_col

      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'eusprc'
      include 'edbdef'

      include 'ogsmout'
      include 'uecpout'
      include 'tcs45Q'
      include 'ccatsdat' 


      INTEGER*4 MX_OGSM
      PARAMETER(MX_OGSM=7)   ! Maximum number of OGSM Regions

      INTEGER*4 MX_SECT
      PARAMETER(MX_SECT=13)  ! Maximum number of OGSM Sectors

      REAL*8 VALUE, TOT_PURCH(MX_OGSM)
      REAL*8 PWF, PVV
      REAL*8 OBJVAL
      REAL*8 OBJTRN
      REAL*8 CO2VAL
      REAL*8 ONEVAL
      REAL*8 FACTOR, AVLVAL
      INTEGER*4 I_CNS
      CHARACTER*2 CNSCOD(MNUMCR)
      INTEGER*4 I_FLRG, I_OGSM, NUM_OGSM, SEC_OGSM, I_SEC, IRET, OGSM_YR, CTS_YR, J_OGSM
      CHARACTER*16 COL_TRANS, COL_OTHER, ROW_CSRG, ROW_OGSM, ROW_OTHR, COL_OTHR, ROW_OTHR_FR

      INTEGER*4 RUN45Q

      efdsub='ED$CCAP'

      RUN45Q=RTOVALUE('RUN45Q  ',0)

      NUM_OGSM = 7
      SEC_OGSM = 13
      OGSM_YR = 2008 - 1989
      CTS_YR = 2007 - 1989

      TOT_PURCH = 0.0


       DO I_CNS = 1 , MNUMCR - 2    
         WRITE(CNSCOD(I_CNS),'("0",I1)') I_CNS

!           WITHOUT 45Q Tax Credit Subsidy
         ROW_CSRG = 'ZCSRG' // CNSCOD(I_CNS) // '_'; call makmsk(ROW_CSRG_mask,':ZCSRG:' , CNSCOD(I_CNS), ':_:')
         CALL DROWTYPE(ROW_CSRG,'E       ',ROW_CSRG_mask)                 

            IF (RUN45Q .GT. 0) THEN

!              WITH 45Q Tax Credit Subsidy

             ROW_CSRG = 'ZCSRS' // CNSCOD(I_CNS) // '_'; call makmsk(ROW_CSRG_mask,':ZCSRS:' , CNSCOD(I_CNS) , ':_:')
             CALL DROWTYPE(ROW_CSRG,'E       ',ROW_CSRG_mask)         
         END IF
      END DO


!     Assign costs/revenues from CCATS for transport/storage or sending to EOR

      DO I_CNS = 1 , MNUMCR - 2
         ROW_CSRG = 'ZCSRG' // CNSCOD(I_CNS) // '_'; call makmsk(ROW_CSRG_mask,':ZCSRG:' , CNSCOD(I_CNS), ':_:')

         COL_TRANS = 'NTX' // CNSCOD(I_CNS) // '___'; call makmsk(COL_TRANS_mask,':NTX:' , CNSCOD(I_CNS) , ':___:')
         OBJTRN = CO2_PRC_DIS_NTC(I_CNS,CURIYR)
         CALL DVAL(COL_TRANS,ROW_CSRG,DBLE(-1.0),COL_TRANS_mask,ROW_CSRG_mask,'ED$CCAP,17')
         CALL DVAL(COL_TRANS,EFDOBJ,OBJTRN,COL_TRANS_mask,EFDOBJ,'ED$CCAP,18')

         COL_TRANS = 'NSX' // CNSCOD(I_CNS) // '___'; call makmsk(COL_TRANS_mask,':NSX:' , CNSCOD(I_CNS) , ':___:')
         OBJTRN = CO2_PRC_DIS_45Q(I_CNS,CURIYR)
         IF (RUN45Q .GT. 0) THEN
          ROW_CSRG = 'ZCSRS' // CNSCOD(I_CNS) // '_'; call makmsk(ROW_CSRG_mask,':ZCSRS:' , CNSCOD(I_CNS), ':_:')
            CALL DVAL(COL_TRANS,ROW_CSRG,DBLE(-1.0),COL_TRANS_mask,ROW_CSRG_mask,'ED$CCAP,19')
            CALL DVAL(COL_TRANS,EFDOBJ,OBJTRN,COL_TRANS_mask,EFDOBJ,'ED$CCAP,20')
         END IF
      END DO

      RETURN
      END

!     EDO$CCAP RECORD RESULTS FOR TRANSPORT OF CAPTURED CO2 TO OGSM REGIONS FOR USE IN EOR PROJECTS

      SUBROUTINE EDO$CCAP
      use efd_row_col

      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'eusprc'
      include 'edbdef'
      include 'ogsmout'
      include 'uecpout'
      include 'uefdout'
      include 'tcs45Q'
      include 'emission'
      include 'cdsparms'
      include 'csapr'
      include 'emmemis'

      INTEGER*4 MX_OGSM
      PARAMETER(MX_OGSM=7)   ! Maximum number of OGSM Regions

      INTEGER*4 MX_SECT
      PARAMETER(MX_SECT=13)  ! Maximum number of OGSM Sectors

      REAL*8 VALUE, TOT_PURCH(MX_OGSM)
      REAL*8 OBJVAL
      REAL*8 OBJTRN
      REAL*8 CO2VAL
      REAL*8 ONEVAL
      REAL*8 LEVEL(5), MIN_PRC(MX_OGSM), MAX_PRC(MX_OGSM), TOT_OTHR(MX_OGSM)
      CHARACTER*8 ROWSOL /'ASLUP   '/
      INTEGER*4 I_FLRG, I_OGSM, NUM_OGSM, SEC_OGSM, I_SEC, IRET, OGSM_YR, CTS_YR, J_OGSM, J_YR, I_TYP
      INTEGER*4 RUN45Q
      INTEGER*4 I_CNS
      CHARACTER*2 CNSCOD(MNUMCR)
      CHARACTER*16 COL_TRANS, COL_OTHER, ROW_CSRG, ROW_OGSM, ROW_OTHR, COL_OTHR, ROW_OTHR_FR
      CHARACTER*2 STATUS

      LOGICAL ONCE
      DATA ONCE/.TRUE./

      efdsub='EDO$CCAP'
      RUN45Q=RTOVALUE('RUN45Q  ',0)

      IF (ONCE) THEN
         DO J_YR = 1 , MNUMYR
            DO I_OGSM = 1 , NUM_OGSM + 1
               UTCO2QEM(I_OGSM,J_YR) = 0.0
            END DO
         END DO
         ONCE = .FALSE.
      END IF

      NUM_OGSM = 7
      SEC_OGSM = 13
      OGSM_YR = 2008 - 1989
      CTS_YR = 2007 -1989

      MIN_PRC = 9999.9
      MAX_PRC = 0.0
      TOT_OTHR = 0.0
      TOT_PURCH = 0.0

      DO I_CNS = 1 , MNUMCR - 2    
       WRITE(CNSCOD(I_CNS),'("0",I1)') I_CNS
       ROW_CSRG = 'ZCSRG' // CNSCOD(I_CNS) // '_'; call makmsk(ROW_CSRG_mask,':ZCSRG:' , CNSCOD(I_CNS), ':_:')
         CALL DWFSROW(ROW_CSRG,ROWSOL,STATUS,LEVEL,ROW_CSRG_mask,IRET)
         CENS_VALUE(I_CNS,CURIYR) = -1 * LEVEL(5)

         WRITE(18,2311) CURIRUN, CURIYR+1989, CURITR, I_CNS, ROW_CSRG, LEVEL(5), CENS_VALUE(I_CNS,CURIYR)
 2311    FORMAT(1X,"EMM_TO_EOR_EFD_CSRG",4(":",I4),":",A16,2(":",F15.6))

       ROW_CSRG = 'ZCSRS' // CNSCOD(I_CNS) // '_'; call makmsk(ROW_CSRG_mask,':ZCSRS:' , CNSCOD(I_CNS), ':_:')
         CALL DWFSROW(ROW_CSRG,ROWSOL,STATUS,LEVEL,ROW_CSRG_mask,IRET)
	     CENS_VAL_45Q(I_CNS,CURIYR) = -1 * LEVEL(5)

         WRITE(18,2321) CURIRUN, CURIYR+1989, CURITR, I_CNS, ROW_CSRG, LEVEL(5), CENS_VAL_45Q(I_CNS,CURIYR)
 2321    FORMAT(1X,"EMM_TO_EOR_EFD_CSRS",4(":",I4),":",A16,2(":",F15.6))

         END DO


      RETURN
      END
!
      SUBROUTINE EPA$TRANRG_EFD(ITRAN,IYR,REG,PLT,COLUMN,COLUMN_maskA,EMIS)
      use efd_row_col
!
!     THIS SUBROUTINE SETS UP THE STATE LEVEL SO2 LIMITS
!     FROM THE TRANSPORT RULE, IF APPROPRIATE
!
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'emission'
      include 'cdsparms'
      include 'csapr'
      include 'emmemis'
!
      INTEGER ITRAN,REG,CRG,STATE,IYR,JYR,PLT,IRET
      REAL*8 EMIS,SO2SHR,VALUE
      CHARACTER*16 ROW,COL,COLUMN,COLUMN_maskA*30
      CHARACTER*2 CLRG(NDREG)
!
      DATA CLRG/'NE',  &
                'YP',  &
                'S1',  &
                'S2',  &
                'GF',  &
                'OH',  &
                'EN',  &
                'KT',  &
                'AM',  &
                'C1',  &
                'C2',  &
                'WS',  &
                'MT',  &
                'CU',  &
                'ZN',  &
                'PC'/

      efdsub='EPA$TRANRG_EFD'

      IF (ITRAN .EQ. 10)THEN
         DO CRG = 1 , NDREG
            IF (TSO2_LIM_BY_CLRG(CRG) .GT. 0.0)THEN
               ROW = 'SULFUR' // CLRG(CRG); call makmsk(ROW_mask,':SULFUR:' , CLRG(CRG))
               IF ((CURIYR + UHBSYR) .LT. TSO2_YR_BY_CLRG)THEN
                  CALL DROWTYPE(ROW,'N       ',ROW_mask)
               ELSE
                  CALL DROWTYPE(ROW,'L       ',ROW_mask)
                  IF ((CURIYR + UHBSYR) .GE. TRANRULE2)THEN
                     VALUE = DBLE(TSO2_LIM_BY_CLRG(CRG))
                  ELSE IF ((CURIYR + UHBSYR) .GE. TRANRULE1)THEN
                     VALUE = DBLE(TSO2_LM1_BY_CLRG(CRG))
                  END IF
                  CALL DRHS(EFDRHS,ROW,VALUE,ROW_mask,'EPA$TRANRG_EFD,1')
!                 IF (TSO2_VR_BY_CLRG .GT. 0)THEN
!                    IF (TSO2_VR_BY_CLRG .EQ. 1)THEN
!                       VALUE = VALUE + DBLE(TSO2_VR1_BY_CLRG(CRG))
!                    END IF
!                    IF (TSO2_VR_BY_CLRG .EQ. 3)THEN
!                       VALUE = VALUE + DBLE(TSO2_VR3_BY_CLRG(CRG))
!                    END IF
!                       COL= 'SULFVR' // CLRG(CRG)
!                       CALL DBND(EFDBND,COL,DBLE(0.0),VALUE)
!                       CALL DVAL(COL,ROW,DBLE(-1.0))
!                       CALL DVAL(COL,EFDOBJ,EFD_MIN)
!                 END IF
               END IF
            END IF
         END DO
      END IF
      IF (ITRAN .EQ. 11)THEN
         IF (TSO2_LIM_BY_CLRG(REG) .GT. 0.0)THEN
            ROW = 'SULFUR' // CLRG(REG); call makmsk(ROW_mask,':SULFUR:' , CLRG(REG))
!           VALUE = DBLE(TBTU_SHR_BY_CLRG(REG,PLT,1))
            VALUE = DBLE(TSO2_SHR_BY_CLRG(REG,1))
            VALUE = EMIS * VALUE
            IF (VALUE .GT. EFD_MIN)THEN
              CALL DVAL(COLUMN,ROW,VALUE,COLUMN_maskA,ROW_mask,'EPA$TRANRG_EFD,2')
            ENDIF
         END IF
      END IF
      IF (ITRAN .EQ. 12)THEN
         DO CRG = 1 , NDREG
            IF (TSO2_LIM_BY_CLRG(CRG) .GT. 0.0)THEN
               ROW = 'SULFUR' // CLRG(CRG); call makmsk(ROW_mask,':SULFUR:' , CLRG(CRG))
!              VALUE = EMIS * TSO2_OSH_BY_CLRG(REG)
               VALUE = EMIS * TSO2_OSH_BY_OLCL(REG,CRG,1)
               IF (VALUE .GT. EFD_MIN)THEN
                  CALL DVAL(COLUMN,ROW,VALUE,COLUMN_maskA,ROW_mask,'EPA$TRANRG_EFD,3')
               ENDIF
            END IF
         END DO
      END IF
!
      RETURN
      END

!     SUBROUTINE HTRT_ADJUSTMENT Determines heatrate adjustments due to duty cycle

      SUBROUTINE HTRT_ADJUSTMENT(FROM_LABEL, FUEL_RGN, ECPt, Load_Level, HTRT_ADJ, Target_EFF, Max_EFF)

      IMPLICIT NONE

      include'parametr'
      include'emmparm'
      include'ncntrl'
      include'control'
      include'ecpcntl'
      include'entcntl'
      include'bildin'
      include'wrenew'
      include'wwdcomon'
      include'uefdout'
      include'elcntl'
      include'elout'

      REAL*8 Load_Level, HTRT_ADJ
      REAL*8 EH, EL, LH, LL, Target_EFF, Max_EFF

      INTEGER*4 FUEL_RGN, ECPt
      INTEGER*4 KNOT, N_KNOT
      CHARACTER*12 FROM_LABEL

      N_KNOT = FLRG_HR_KNOTS(FUEL_RGN,ECPt)
      IF (FLRG_HR_KNOTS(FUEL_RGN,ECPt) .EQ. 0) THEN
         HTRT_ADJ = 1.0
         Target_EFF = 1.0
         Max_EFF = 1.0
      ELSE
         Load_Level = MIN(Load_Level , 1.0)
         Max_EFF = FLRG_HR_EFF(FUEL_RGN,ECPt,N_KNOT)
         IF (Load_Level .LE. FLRG_HR_LL(FUEL_RGN,ECPt,1)) THEN
            Target_EFF = FLRG_HR_EFF(FUEL_RGN,ECPt,1)
            HTRT_ADJ =  Max_EFF / Target_EFF
         ELSE
            DO KNOT = 2 , FLRG_HR_KNOTS(FUEL_RGN,ECPt)
               IF (Load_Level .LE. FLRG_HR_LL(FUEL_RGN,ECPt,KNOT)) THEN
                  LL = FLRG_HR_LL(FUEL_RGN,ECPt,KNOT-1)
                  EL = FLRG_HR_EFF(FUEL_RGN,ECPt,KNOT-1)
                  LH = FLRG_HR_LL(FUEL_RGN,ECPt,KNOT)
                  EH = FLRG_HR_EFF(FUEL_RGN,ECPt,KNOT)
                  Target_EFF = EL + ((EH - EL) / (LH - LL)) * (Load_Level - LL)
                  HTRT_ADJ =  Max_EFF / Target_EFF

!                 IF (CURIYR+1989 .LE. 2017 .AND. FCRL .EQ. 1) THEN
!                    WRITE(18,5751) CURIRUN, CURIYR+1989, CURITR, FUEL_RGN, ECPt, KNOT, FLRG_HR_KNOTS(FUEL_RGN,ECPt), FROM_LABEL, LL, Load_Level, LH, EL, Target_EFF, EH, Max_EFF, HTRT_ADJ
!5751                FORMAT(1X,"HTRT_ADJ_INFO",7(":",I4),":",A12,8(":",F21.6))
!                 END IF

                  EXIT
               END IF
            END DO
            IF (KNOT .GT. FLRG_HR_KNOTS(FUEL_RGN,ECPt)) THEN
               HTRT_ADJ = 1.0
               Target_EFF = Max_EFF
            END IF
         END IF
      END IF

      RETURN
      END
!
!     EDO$COL
!
      SUBROUTINE EDO$COL
      use efd_row_col

      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'cdsparms'
      include'coalemm'
      include'coalrep'
      include'uso2grp'
      include'fuelin'
      include'emission'
      include'uefdout'
      include'csapr'
      include'emmemis'

      REAL*4 UFRSO2QT(NUTSEC+1),UTTSO2QT
      INTEGER IRET,CRV,PLT,CRG,ACI
      REAL*8 SOLVAL(5)
      CHARACTER*1 ACICD(2)
      CHARACTER*16 ROW,COL
      CHARACTER*8 ROWSOL/'ASLUP   '/
      CHARACTER*8 COLSOL/'ACLUD   '/
      CHARACTER*2 STAT,SC

      efdsub='EDO$COL'

!     INITIALIZE
      ACICD(1) = '0'
      ACICD(2) = '1'

      UFRSO2R  = 0.0
      UFRSO2Q  = 0.0
      UFRSO2QT = 0.0
      UTTSO2QT = 0.0

      DO CRV = 1 , MX_NCOALS + MX_ISCV
         WRITE(SC,'(I2.2)') CRV
         DO CRG = 1 , NDREG
            DO PLT = 1 , NUTSEC
               IF (CLSCPLDR(CRV,PLT,CRG) .GT. 0)THEN
                  DO ACI = 1 , 2
                     COL = 'CT' // SC // EPFLCD(CRG) // UPLNTCD(PLT) // ACICD(ACI) ; call makmsk(COL_mask,':CT:' , SC , EPFLCD(CRG) , UPLNTCD(PLT) , ACICD(ACI))
                     CALL DWFSCOL(COL,COLSOL,STAT,SOLVAL,COL_mask,IRET)
                     IF (IRET .EQ. 0)THEN
                        UFRSO2R(PLT,CRG) = UFRSO2R(PLT,CRG) + SOLVAL(1) * XCL_SO2_YR(CRV,CURIYR)
                        UFRSO2Q(PLT,CRG) = UFRSO2Q(PLT,CRG) + SOLVAL(1)
                        UFRSO2QT(PLT) = UFRSO2QT(PLT) + SOLVAL(1) * XCL_SO2_YR(CRV,CURIYR) * (1.0 - RCLCLNR(CRG,CURIYR,PLT)) * 0.5 * 0.001
                        UFRSO2QT(NUTSEC + 1) = UFRSO2QT(NUTSEC + 1) + SOLVAL(1) * XCL_SO2_YR(CRV,CURIYR) * (1.0 - RCLCLNR(CRG,CURIYR,PLT)) * 0.5 * 0.001
!    if (curitr .gt. maxitr)write(6,2222) curcalyr,col,solval(1),xcl_so2_yr(crv,curiyr),ufrso2(plt,crg)
 2222 format(1h ,'!so2col',i4,a10,1x,5f10.2)
                     END IF
                  END DO
               END IF
            END DO
         END DO
      END DO

      DO PLT = 1 , NUTSEC
!        if (curitr .gt. maxitr)write(6,3333) curiyr+1989,plt,uttso2(plt,curiyr),ufrso2qt(plt)
!3333 format(1h ,'!so2cmp',i4,i3,5f10.4)
         UTTSO2QT = UTTSO2QT + UTTSO2(PLT,CURIYR)
      END DO
!        if (curitr .gt. maxitr)write(6,3333) curiyr+1989,nutsec + 1,uttso2qt,ufrso2qt(nutsec + 1),  &
!                     sum(so2other(CURIYR,1:NUM_SO2_GRP)) / 1000000. , sum(CTLSO2EM(1:NDREG,CURIYR)) / 1000000.

      DO CRG = 1 , NDREG
         DO PLT = 1 , NUTSEC
            IF (UFRSO2Q(PLT,CRG) .GT. 0.0)THEN
               UFRSO2R(PLT,CRG) = UFRSO2R(PLT,CRG) / UFRSO2Q(PLT,CRG)
            ELSE
               UFRSO2R(PLT,CRG) = UFRSO2(PLT,CRG)
            END IF
!           if (curitr .gt. maxitr)write(6,4444) curiyr+1989,crg,plt,ufrso2q(plt,crg),ufrso2r(plt,crg),ufrso2(plt,crg)
!4444 format(1h ,'!so2ufr',i4,i3,i3,5f10.4)
         END DO
      END DO

      RETURN
      END
!
!     EDO$GRD
!
      SUBROUTINE EDO$GRD
      use efd_row_col

      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'uefdout'
      include'ecp_nuc'
      include'cdsparms'
      include'emm_aimms'
!
!      COMMON /GRDSRC/ GRD_CASN,GRD_SRCN,GRD_SRCC
!      INTEGER GRD_CASN                                       ! Number of grid resilience sources 
!      CHARACTER*15 GRD_SRCN(MX_GRDSRC)                       ! Grid resilience source names
!      CHARACTER*1  GRD_SRCC(MX_GRDSRC)                       ! Grid resilience source codes

      REAL*4 GRD_NRRT(MNUMNR,MNUMYR)
      REAL*4 GRD_NRGN(MNUMNR,MNUMYR)
      REAL*4 GRD_PRCE(MNUMNR,MNUMYR)

      INTEGER IRET,IRG,IYR,IGR
      INTEGER REG(MNUMNR)
      REAL*4 ZERO(MNUMNR)
      REAL*8 SOLVAL(5)
      CHARACTER*16 ROW,COL
      CHARACTER*8 ROWSOL/'ASLUP   '/
      CHARACTER*8 COLSOL/'ACLUD   '/
      CHARACTER*2 STAT

      efdsub='EDO$GRD'
   
!     INITIALIZE RESILIENCY OUTPUT VARIABLES

      IF ((CURIYR + UHBSYR) .EQ. UESTYR .AND. CURITR .EQ. 1)THEN
         UGRD_STC = 0.0
         UGRD_RTC = 0.0
         UGRD_PRC = 0.0
         UGRD_CAP = 0.0
         UGRD_STG = 0.0
         UGRD_RTG = 0.0
         UGRD_PRG = 0.0
         UGRD_GEN = 0.0
      END IF

         DO IRG = 1 , MNUMNR
            REG(IRG) = IRG
            ZERO(IRG) = 0.0
         END DO

      DO IGR = 1 , GRD_CASN
!        INITIALIZE NATIONAL TOTALS
         UGRD_STC(IGR,MNUMNR,CURIYR) = 0.0
         UGRD_RTC(IGR,MNUMNR,CURIYR) = 0.0
         UGRD_PRC(IGR,MNUMNR,CURIYR) = 0.0
         UGRD_CAP(IGR,MNUMNR,CURIYR) = 0.0
         UGRD_STG(IGR,MNUMNR,CURIYR) = 0.0
         UGRD_RTG(IGR,MNUMNR,CURIYR) = 0.0
         UGRD_PRG(IGR,MNUMNR,CURIYR) = 0.0
         UGRD_GEN(IGR,MNUMNR,CURIYR) = 0.0

!        EMM REGION GRID RESILIENCE ROWS

         DO IRG = 1 , UNRGNS
            GRD_NRRT(IRG,CURIYR) = 0.0
            GRD_NRGN(IRG,CURIYR) = 0.0
            GRD_PRCE(IRG,CURIYR) = 0.0
            ROW = 'GRDRT' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_mask,':GRDRT:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
            CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
            IF (IRET .EQ. 0)THEN
               GRD_NRRT(IRG,CURIYR) = SOLVAL(1)
               GRD_PRCE(IRG,CURIYR) = -1.0 * SOLVAL(5)
            END IF
            ROW = 'GRDGN' // URGNME(IRG)(6:7) // GRD_SRCC(IGR); call makmsk(ROW_mask,':GRDGN:' , URGNME(IRG)(6:7), GRD_SRCC(IGR))
            CALL DWFSROW(ROW,ROWSOL,STAT,SOLVAL,ROW_mask,IRET)
            IF (IRET .EQ. 0)THEN
               GRD_NRGN(IRG,CURIYR) = SOLVAL(1)
            END IF
!           IF TARGET APPLIED, THEN RATE IS TARGET PLUS ROW EXCESS
            IF (GRD_NRGN(IRG,CURIYR) .GT. 0.0)THEN
               IF (USW_GRD .EQ. IGR .AND. GRD_TGTS(CURIYR,IRG) .GT. 0.0)THEN
                  GRD_NRRT(IRG,CURIYR) = (GRD_NRRT(IRG,CURIYR) / GRD_NRGN(IRG,CURIYR)) + GRD_TGTS(CURIYR,IRG)
               ELSE
                  GRD_NRRT(IRG,CURIYR) = GRD_NRRT(IRG,CURIYR) / GRD_NRGN(IRG,CURIYR)
               END IF
            ELSE
               GRD_NRRT(IRG,CURIYR) = 0.0
            END IF
!
!           MAP OUTPUT TO GRID RESILIENCE VARIABLES
!
!           GENERATION STANDARD
            IF (GRD_TYP .LE. 1)THEN
               IF (USW_GRD .EQ. IGR)THEN
                  UGRD_STG(IGR,IRG,CURIYR) = GRD_TGTS(CURIYR,IRG)
               ELSE
                  UGRD_STG(IGR,IRG,CURIYR) = 0.0
               END IF
               UGRD_RTG(IGR,IRG,CURIYR) = GRD_NRRT(IRG,CURIYR)
               UGRD_PRG(IGR,IRG,CURIYR) = GRD_PRCE(IRG,CURIYR)
               UGRD_GEN(IGR,IRG,CURIYR) = GRD_NRGN(IRG,CURIYR)
!              NATIONAL TOTALS
               UGRD_STG(IGR,MNUMNR,CURIYR) = UGRD_STG(IGR,MNUMNR,CURIYR) + UGRD_STG(IGR,IRG,CURIYR) * UGRD_GEN(IGR,IRG,CURIYR)
               UGRD_RTG(IGR,MNUMNR,CURIYR) = UGRD_RTG(IGR,MNUMNR,CURIYR) + UGRD_RTG(IGR,IRG,CURIYR) * UGRD_GEN(IGR,IRG,CURIYR)
               UGRD_PRG(IGR,MNUMNR,CURIYR) = UGRD_PRG(IGR,MNUMNR,CURIYR) + UGRD_PRG(IGR,IRG,CURIYR) * UGRD_GEN(IGR,IRG,CURIYR)
               UGRD_GEN(IGR,MNUMNR,CURIYR) = UGRD_GEN(IGR,MNUMNR,CURIYR) + UGRD_GEN(IGR,IRG,CURIYR)
               IF (IRG .EQ. UNRGNS)THEN
                  IF (UGRD_GEN(IGR,MNUMNR,CURIYR) .GT. 0.0)THEN
                     UGRD_STG(IGR,MNUMNR,CURIYR) = UGRD_STG(IGR,MNUMNR,CURIYR) / UGRD_GEN(IGR,MNUMNR,CURIYR)
                     UGRD_RTG(IGR,MNUMNR,CURIYR) = UGRD_RTG(IGR,MNUMNR,CURIYR) / UGRD_GEN(IGR,MNUMNR,CURIYR)
                     UGRD_PRG(IGR,MNUMNR,CURIYR) = UGRD_PRG(IGR,MNUMNR,CURIYR) / UGRD_GEN(IGR,MNUMNR,CURIYR)
                  END IF
               END IF
            ELSEIF (GRD_TYP .EQ. 2)THEN
!           CAPACITY   STANDARD
               IF (USW_GRD .EQ. IGR)THEN
                  UGRD_STC(IGR,IRG,CURIYR) = GRD_TGTS(CURIYR,IRG)
               ELSE
                  UGRD_STC(IGR,IRG,CURIYR) = 0.0
               END IF
               UGRD_RTC(IGR,IRG,CURIYR) = GRD_NRRT(IRG,CURIYR)
               UGRD_PRC(IGR,IRG,CURIYR) = GRD_PRCE(IRG,CURIYR)
               UGRD_CAP(IGR,IRG,CURIYR) = GRD_NRGN(IRG,CURIYR)
!              NATIONAL TOTALS
               UGRD_STC(IGR,MNUMNR,CURIYR) = UGRD_STC(IGR,MNUMNR,CURIYR) + UGRD_STC(IGR,IRG,CURIYR) * UGRD_CAP(IGR,IRG,CURIYR)
               UGRD_RTC(IGR,MNUMNR,CURIYR) = UGRD_RTC(IGR,MNUMNR,CURIYR) + UGRD_RTC(IGR,IRG,CURIYR) * UGRD_CAP(IGR,IRG,CURIYR)
               UGRD_PRC(IGR,MNUMNR,CURIYR) = UGRD_PRC(IGR,MNUMNR,CURIYR) + UGRD_PRC(IGR,IRG,CURIYR) * UGRD_CAP(IGR,IRG,CURIYR)
               UGRD_CAP(IGR,MNUMNR,CURIYR) = UGRD_CAP(IGR,MNUMNR,CURIYR) + UGRD_CAP(IGR,IRG,CURIYR)
               IF (IRG .EQ. UNRGNS)THEN
                  IF (UGRD_CAP(IGR,MNUMNR,CURIYR) .GT. 0.0)THEN
                     UGRD_STC(IGR,MNUMNR,CURIYR) = UGRD_STC(IGR,MNUMNR,CURIYR) / UGRD_CAP(IGR,MNUMNR,CURIYR)
                     UGRD_RTC(IGR,MNUMNR,CURIYR) = UGRD_RTC(IGR,MNUMNR,CURIYR) / UGRD_CAP(IGR,MNUMNR,CURIYR)
                     UGRD_PRC(IGR,MNUMNR,CURIYR) = UGRD_PRC(IGR,MNUMNR,CURIYR) / UGRD_CAP(IGR,MNUMNR,CURIYR)
                  END IF
               END IF
            END IF
         END DO

         IF (CURIYR .EQ. LASTYR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
            WRITE(13,2000) 
 2000 FORMAT(1h ,'!grdo')
            WRITE(13,2100) GRD_SRCN(IGR)
 2100 FORMAT(1h ,'!grdo',T25,'GRID RESILIENCE FACTORS BY REGION AND YEAR -- SOURCE = ',a3)
            WRITE(13,2200) (REG(IRG),IRG = 1 , UNRGNS)
 2200 FORMAT(1h ,'!grdo',5x,22I6)
            DO IYR = (UPSTYR - UHBSYR) + UNXPH - 1 , MNUMYR
               WRITE(13,2300) IYR + 1989, (GRD_NRRT(IRG,IYR), IRG = 1 , UNRGNS)
 2300 FORMAT(1h ,'!grdo',I5,22F6.2)
            END DO
            WRITE(13,2000) 

            WRITE(13,2400) 
 2400 FORMAT(1h ,'!grdt')
            WRITE(13,2500) GRD_SRCN(IGR)
 2500 FORMAT(1h ,'!grdt',T25,'GRID RESILIENCE TARGETS BY REGION AND YEAR: -- SOURCE = ',a3)
            WRITE(13,2600) (REG(IRG),IRG = 1 , UNRGNS)
 2600 FORMAT(1h ,'!grdt',5x,22I6)
            IF (USW_GRD .EQ. IGR)THEN
               DO IYR = (UPSTYR - UHBSYR) + UNXPH - 1 , MNUMYR
                  WRITE(13,2700) IYR + 1989, (GRD_TGTS(IYR,IRG),IRG = 1 , UNRGNS)
 2700 FORMAT(1h ,'!grdt',I5,22F6.2)
               END DO
            ELSE
               WRITE(13,2700) IYR + 1989, (ZERO(IRG),IRG = 1 , UNRGNS)
            END IF
            WRITE(13,2400) 
         END IF

      END DO

      RETURN
      END
!
!     This subroutine does some post-processing after AIMMS LP solves
      SUBROUTINE EFD_Post_calcs(IRG)
      use efd_row_col

      IMPLICIT NONE

      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispin'
      include'dispuse'
      include'elcntl'
      include'ecpcntl'
      include'dispcrv'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'uefdout'
      include'elout'
      include'cdsparms'
      include'emm_aimms'
      include'wrenew'

      INTEGER ITYP,IRG,N,IP,IPGRP,IFL,TFR,IECP,IS,IVSL,GRP,SEG,FSL,ISL,CRG,I_SUPt
      REAL   SUMFL, TOL
      
      TOL = 0.0001
!   loop over dispatchable plant types
      ITYP = 1

      DO N = 1, ECNTP
       IP = ECASTS(N)
       IECP = ECTECP(N)
       TFR = EPNFLRG(ECCR(N),ECLR(N),ECGR(N),ECAR(N))
       IPGRP = ECDBID(N)
!  fill in fuel shares
       IF (IECP .LE. UIPC) THEN    ! coal plant - not IGCC
        DO IS = 1, EENSP
         ECFLTP(N,1) = IECP
         ULFUEL(1,IPGRP) = IECP
         IF (ECFLRG(N,1,1) .EQ. 0) ECFLRG(N,1,1) = ECLR(N)
         IF (ULFLRG(1,IPGRP) .EQ. 0) ULFLRG(1,IPGRP) = ECLR(N)
         ELFLSH(IS,IPGRP,1) = FCLSH(1,IECP,TFR)
         ECFLTP(N,2) = UIDS
         ULFUEL(2,IPGRP) = UIDS
         IF (ECFLRG(N,2,1) .EQ. 0) ECFLRG(N,2,1) = ECCR(N)
         IF (ULFLRG(2,IPGRP) .EQ. 0) ULFLRG(2,IPGRP) = ECCR(N)
         ELFLSH(IS,IPGRP,2) = FOLSH(1,IECP,TFR)
         ECFLTP(N,3) = UIWD
         ULFUEL(3,IPGRP) = UIWD
         IF (ECFLRG(N,3,1) .EQ. 0) ECFLRG(N,3,1) = ECLR(N)
         IF (ULFLRG(3,IPGRP) .EQ. 0) ULFLRG(3,IPGRP) = ECLR(N)
         ELFLSH(IS,IPGRP,3) = FWDSH(1,IECP,TFR)

        ENDDO

       ELSEIF (IECP .LE. UIIS) THEN  ! coal plant - IGCC
        DO IS = 1, EENSP
         ECFLTP(N,1) = IECP
         ULFUEL(1,IPGRP) = IECP
         IF (ECFLRG(N,1,1) .EQ. 0) ECFLRG(N,1,1) = ECLR(N)
         IF (ULFLRG(1,IPGRP) .EQ. 0) ULFLRG(1,IPGRP) = ECLR(N)
         ELFLSH(IS,IPGRP,1) = FCLSH(1,IECP,TFR)
         IF (FCLSH(1,IECP,TFR) .LT. 1.0) THEN
           write(UF_DBG,1212) CURIYR,CURITR,N,IECP,FCLSH(1,IECP,TFR),FOLSH(1,IECP,TFR),FWDSH(1,IECP,TFR)
           ELFLSH(IS,IPGRP,1) = 1.0
1212    FORMAT(1x,'IGCC FLSH err',4I6,3F10.6)
         ENDIF
         ECFLTP(N,2) = 0
         ULFUEL(2,IPGRP) = 0
         ECFLRG(N,2,1) = 0
         ULFLRG(2,IPGRP) = 0
         ELFLSH(IS,IPGRP,2) = 0.0
         ECFLTP(N,3) = 0
         ULFUEL(3,IPGRP) = 0
         ECFLRG(N,3,1) = 0
         ULFLRG(3,IPGRP) =  0
         ELFLSH(IS,IPGRP,3) = 0.0
        ENDDO

       ELSE
         DO IFL = 1, EIFPLT
         DO IS = 1, EENSP
         IF (ECFLTP(N,IFL) .NE. 0) THEN
          IF (UICOL(ECFLTP(N,IFL)) .EQ. 1) THEN    !added to handle coal share under CTN
            ELFLSH(IS,IPGRP,IFL) = FCLSH(2,IP,TFR)    !added to handle coal share under CTN
          ELSEIF (UIGAS(ECFLTP(N,IFL)) .EQ. 1) THEN
            ELFLSH(IS,IPGRP,IFL) = FGSSH(2,IP,TFR)
          ELSEIF (UIDIS(ECFLTP(N,IFL)) .EQ. 1 .OR. UIRES(ECFLTP(N,IFL)) .EQ. 1) THEN
            ELFLSH(IS,IPGRP,IFL) = FOLSH(2,IP,TFR)
          ELSEIF (ECFLTP(N,IFL) .EQ. UIWD) THEN
            ELFLSH(IS,IPGRP,IFL) = FWDSH(2,IP,TFR)
          ELSEIF (ECFLTP(N,IFL) .EQ. UIUF .OR. ECFLTP(N,IFL) .EQ. UIGT .OR. ECFLTP(N,IFL) .EQ. UIGC) THEN
            ELFLSH(IS,IPGRP,IFL) = 1.0
          ENDIF
         ELSE
           ELFLSH(IS,IPGRP,IFL) = 0.0
         ENDIF
         ENDDO
         ENDDO
        ENDIF
! check fuel shares sum to 1.0
        SUMFL = ELFLSH(1,IPGRP,1) + ELFLSH(1,IPGRP,2) + ELFLSH(1,IPGRP,3)
        IF (SUMFL .LT. (1.0 - TOL) .OR. SUMFL .GT. (1.0 + TOL))  THEN
         write(UF_DBG,1026) CURIYR,CURITR,N,IPGRP,IECP,IP,TFR,((ECFLTP(N,IFL),ELFLSH(1,IPGRP,IFL),ECFLRG(N,IFL,1)),IFL=1,EIFPLT)
1026    format(1x,'ECFSHR error',7I6,3(I4,F10.5,I4))
        do IS = 1, EENSP
         ELFLSH(IS,IPGRP,1) = 1.0
         ELFLSH(IS,IPGRP,2) = 0.0
         ELFLSH(IS,IPGRP,3) = 0.0
        enddo
        ENDIF
         IF (FCRL .eq. 1) THEN
        write(UF_DBG,1028) CURIYR,CURITR,IPGRP,IECP,IP,TFR,((ECFLTP(N,IFL),ELFLSH(1,IPGRP,IFL),ECFLRG(N,IFL,1)),IFL=1,EIFPLT)
1028     format(1x,'ECFSHR final',6I6,3(I4,F10.5,I4))
        ENDIF

      ENDDO

      ! print spinning reserve by technology
      IF (FCRL .EQ. 1) THEN
      DO IECP = 1, ECP_D_CAP
        WRITE(UF_DBG,1030) CURIYR,CURITR,IRG,IECP,((SP_ACHBYECP(GRP,SEG,IRG,IECP),SEG=1,3),GRP=1,3)
        IF (UCPINTIS(IECP) .GT. 0 ) THEN
          IF (EFDCURT(IRG,UCPINTIS(IECP),CURIYR) .GT. 0.0) THEN
           CURTAIL(UCPINTIS(IECP),IRG,CURIYR) = CURTAIL(UCPINTIS(IECP),IRG,CURIYR) + EFDCURT(IRG,UCPINTIS(IECP),CURIYR)            !add EFD curt to RESTORE value
           WRITE(UF_DBG,1031) CURIYR,CURITR,IRG,IECP,EFDCURT(IRG,UCPINTIS(IECP),CURIYR)
          ENDIF
          CURTAIL(UCPINTIS(IECP),MNUMNR,CURIYR) = CURTAIL(UCPINTIS(IECP),MNUMNR,CURIYR) + CURTAIL(UCPINTIS(IECP),IRG,CURIYR)      !fill national value
        ENDIF
      ENDDO
      ENDIF
1030  FORMAT(1X,'SR_ACHBYECP:',4I6,9F12.4)
1031  FORMAT(1X,'EFDCURT:',4(I6,":"),F12.4)

     DO IS = 1, EENSP
        DO ISL = 1 , ELNVCT(IS)
            GRP = ELGRP(ISL,IS)
            SEG = ELSEG(ISL,IS)         
               WRITE(UF_DBG,9425)'SPN_RSV_SRPOOL:af', CURIRUN, CURIYR+1989, CURITR, IRG, IS, ISL, GRP, SEG, SRPOOL(CURIYR,IRG), &
              SP_RES_DUAL(GRP,SEG,IRG), SP_RES_REQ(GRP,SEG,IRG), SR_INT_REQ(GRP,SEG,IRG),SP_RES_ACH(GRP,SEG,IRG),UTWDTH(SEG,GRP)
9425          FORMAT(1x,A25,1x,8(":",I5),6(":",F21.6))
        ENDDO
     ENDDO
     
           DO CRG = 0 , NDREG
         WRITE(18,3031) CURIRUN, CURIYR+1989, CURITR, CRG, (QBMPWCL(I_SUPt,CRG,CURIYR), I_SUPt = 0 , MNUMFS)
 3031    FORMAT(1X,"QBMPWCL",4(":",I4),<MNUMFS+1>(":",F12.3))
         WRITE(18,3032) CURIRUN, CURIYR+1989, CURITR, CRG, (PBMPWCL(I_SUPt,CRG,CURIYR), I_SUPt = 0 , MNUMFS)
 3032    FORMAT(1X,"PBMPWCL",4(":",I4),<MNUMFS+1>(":",F12.3))
      END DO

     RETURN
     END
      