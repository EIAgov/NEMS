! $Header: m:/default/source/RCS/hmm.f,v 1.10 2019/08/02 14:30:00 pkc Exp $
!     Program and subroutines above this line are not needed once HMM is integrated with the rest of NEMS
!
      SUBROUTINE HMM_MAIN
!TRPTIM
!     THIS Program Initializes OML, Loads an ACTFILE and Creates an Analyze Pack File
!
!     PARAMETERS:
!
!     WFINIT                    ! OML LIBRARY FUNCTION
!     WFDEF                     ! OML LIBRARY FUNCTION
!     WFMPSIN                   ! OML LIBRARY FUNCTION
!     WFLOAD                    ! OML LIBRARY FUNCTION
!     WFSET                     ! OML LIBRARY FUNCTION
!
!     TO LP MATRIX BEFORE EACH NEW OPT.
!     OML.XSOLPRNT                  ! FLAG TO PRINT LP SOLUTION
!     HMMIO()                   ! OML ARG. DEFINING LP REG. SIZE
!     IOBUF()                   ! OML ARG. DEFINING OML REG. SIZE
!     MPSHMM  (HMM$FILE)        ! DDNAME CONTAINING MPS INPUT
!     ACTHMMI (HMM$DBNM)        ! DDNAME CONTAINING ACTFILE INPUT
!     OML DB CONTAINING LP PROBLEM
!     BASHMMI (BASHMMO)  ! DDNAME CONTAINING BASIS OUTPUT
!
      IMPLICIT NONE
!
!
      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'aponroad'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'uefpout'
      include 'cogen'
      include 'emablk'
      include 'emeblk'
      include 'emission'
      include 'hmmblk'
      include 'macout'
      include 'pmmrpt'
      include 'tranrep'
      include 'wrenew'
      include 'wwdcomon'
      include 'omlall.fi'

      INTEGER TT2NDX
      PARAMETER (TT2NDX=2*(TOTNDX+1))
      INTEGER*4  XYR,FYR,N_XYR,N_FYR,M_FYR,JYR,JYR_SYR,JYR_EYR
      INTEGER*4  IRET,KRET,LRET     ! OML RETURN CODE
      INTEGER*4  getenvqq
      INTEGER*4  FULLYR,NERC,YEAR,LASYR,FIRYR,KYR,JGRP,IP,CRG,IHMM
      INTEGER*4  SV_SW,RRET,CRET,JRET,ISO2,FILE_HMM,TRN_PART
!
      INTEGER HMM_MAX_FYR
      PARAMETER (HMM_MAX_FYR = 20)
      REAL*8 HMM_GNPD(MNUMYR + HMM_MAX_FYR),GRW
      INTEGER HMM_MAX_XYR
      PARAMETER (HMM_MAX_XYR = 5)
      INTEGER HMM_MAX_HYR
      PARAMETER (HMM_MAX_HYR = HMM_MAX_FYR+MNUMYR)
      REAL*8 RORHM,RORPIP,RORTD
      REAL*8 KG(HMM_MAX_FYR),PV_KG
      REAL*8 D_HYD(HMM_MAX_FYR),PV_HYD,AVG_HYD,AVG_H2D(MNUMYR,MNUMCR,HMKT)
      REAL*8 CCST(HMM_MAX_FYR),PV_CCST
      REAL*8 CCST_IP(HMM_MAX_FYR),PV_CCST_IP
      REAL*8 CCST_TD(HMM_MAX_FYR),PV_CCST_TD
      REAL*8 FOM_(HMM_MAX_FYR),PV_FOM_
      REAL*8 FOM__IP(HMM_MAX_FYR),PV_FOM__IP
      REAL*8 FOM__TD(HMM_MAX_FYR),PV_FOM__TD
      REAL*8 VOM_(HMM_MAX_FYR),PV_VOM_
      REAL*8 VOM__IP(HMM_MAX_FYR),PV_VOM__IP
      REAL*8 VOM__TD(HMM_MAX_FYR),PV_VOM__TD
      REAL*8 FUEL(HMM_MAX_FYR),PV_FUEL
      REAL*8 FUEL_IP(HMM_MAX_FYR),PV_FUEL_IP
      REAL*8 FUEL_TD(HMM_MAX_FYR),PV_FUEL_TD
      REAL*8 CARB(HMM_MAX_FYR),PV_CARB
      REAL*8 ELEC(HMM_MAX_FYR),PV_ELEC
      REAL*8 INFL(HMM_MAX_FYR),PV_INFL
      REAL*8 CRF,PWF,PVV,PVBV,CRF2
      REAL*8 ZPELIN(MNUMCR,HMM_MAX_HYR)
      REAL*8 ZPNGEL(MNUMCR,HMM_MAX_HYR)
      REAL*8 ZPCLEL(MNUMCR,HMM_MAX_HYR)
      REAL*8 ZPBMET(MNUMCR,HMM_MAX_HYR)
      REAL*8 ZPUREL(MNUMCR,HMM_MAX_HYR)
      REAL*8 ZPELCM(MNUMCR,HMM_MAX_HYR)
      REAL*8 ZPNGIN(MNUMCR,HMM_MAX_HYR)
      REAL*8 ZJNGEL(HMM_MAX_HYR)
      REAL*8 ZJCLEL(HMM_MAX_HYR)
!
      INTEGER*4 MAX_LCP                ! MAXIMUM # OF YEARS IN CONSTR PROFILE (I.E. ARRAY SIZE)
      PARAMETER (MAX_LCP = 10)
      INTEGER*4 CLT                    ! CONSTRUCTION LEAD TIME -- ACTUAL
      REAL*8 OVRCST                    ! OVERNIGHT COSTS
      REAL*8 PROFILE(MAX_LCP)          ! ANNUAL EXPENDITURE PERCENTAGE
      REAL*8 CAPESC(MAX_LCP)           ! CAPITAL ESCALATION/REAL
      REAL*8 GNPF(MAX_LCP)             ! GENERAL INFLATION
      REAL*8 INTR                      ! INTEREST COSTS
      REAL*8 DEBT_F                    ! CONSTRUCTION DEBT FINANCING
      REAL*8 ROR                       ! RATE OF RETURN
      REAL*8 TXBOOK                    ! TAX BASIS INSTALLED COSTS
      REAL*8 BVBOOK                    ! BOOK VALUE INSTALLED COSTS
!
      INTEGER*4 DL2                    ! DEBT LOAN LIFE <= CONTRACT LIFE
      INTEGER*4 CL                     ! CONTRACT LIFE
      INTEGER*4 TL                     ! TAX LIFE ASSOCIATED WITH TECHNOLOGY
      REAL*8 ROE                       ! ROE
      REAL*8 INT                       ! INTEREST RATE
      REAL*8 DF2                       ! DEBT FRACTION
      REAL*8 TR                        ! MARGINAL FEDERAL INCOME TAX RATE
      REAL*8 RATIO                     ! TX TO BV RATIO
      REAL*8 CAP                       ! Annuity Factor
!
      INTEGER*4  STATS(9),I,HMM_MAT
      INTEGER*4  CPU_TIME_BEGIN,CPU_TIME_END
!
!     UNTIL RUN TIME OVERWRITES SWITCHES ADDED
!
      INTEGER        IY,IRET1,J,K
      INTEGER CINX,MINX,FS

      INTEGER HF_HMM,HF_HMMO,HF_FORDB,HF_MWH
      INTEGER TCHNDX(TCHTCH,4)
      INTEGER TGHNDX(TGHTCH,4)
      INTEGER DOHNDX(DOHTCH,4)
      INTEGER SOHNDX(SOHTCH,4)
      INTEGER YRDOLP,YRDOLD
      REAL*8 PCLEV(MNUMYR,MNUMCR,HMKT,PCHTCH)
      REAL*8 PGLEV(MNUMYR,MNUMCR,HMKT,PGHTCH)
      REAL*8 PFLEV(MNUMYR,MNUMCR,HMKT,PFHTCH)
      REAL*8 TCLEV(MNUMYR,MNUMCR,HMKT,TCHTCH)
      REAL*8 TGLEV(MNUMYR,MNUMCR,HMKT,TGHTCH)
      REAL*8 DOLEV(MNUMYR,MNUMCR,HMKT,DOHTCH)
      REAL*8 SOLEV(MNUMYR,MNUMCR,HMKT,SOHTCH)
      REAL*8 DMDLEV(MNUMYR,MNUMCR,HMKT,HMM_MAX_XYR)
      REAL*8 NUMSTAT(MNUMYR,MNUMCR,HMKT)                ! Number of stations based on 20-year levelized demand, not used for tran
      REAL*8 PCLEX(MNUMYR,MNUMCR,HMKT,PCHTCH)
      REAL*8 PGLEX(MNUMYR,MNUMCR,HMKT,PGHTCH)
      REAL*8 PFLEX(MNUMYR,MNUMCR,HMKT,PFHTCH)
      REAL*8 TCLEX(MNUMYR,MNUMCR,HMKT,TCHTCH)
      REAL*8 TGLEX(MNUMYR,MNUMCR,HMKT,TGHTCH)
      REAL*8 DOLEX(MNUMYR,MNUMCR,HMKT,DOHTCH)
      REAL*8 SOLEX(MNUMYR,MNUMCR,HMKT,SOHTCH)
      REAL*8 PCEFF(HMM_MAX_HYR,PCHTCH)
      REAL*8 PGEFF(HMM_MAX_HYR,PGHTCH)
      REAL*8 PFEFF(HMM_MAX_HYR,PFHTCH)
      REAL*8 MGPD(MNUMYR+HMM_MAX_HYR,MNUMCR,HMKT),TOT_MGPD
      REAL*8 D_MGPD(HMM_MAX_FYR),PV_MGPD
!!
!!    Electricity Consumption (used to capture amt elec used to run hydrogen plants)

      REAL PCELC(HMM_MAX_HYR,PCHTCH)    !Elec Consumpt-central
      REAL PGELC(HMM_MAX_HYR,PGHTCH)    !Elec Consumpt-citygate
      REAL PFELC(HMM_MAX_HYR,PFHTCH)    !Elec Consumpt-forecourt
      REAL SOELC(HMM_MAX_HYR,SOHTCH)    !Elec Consumpt-dispensing
      REAL LIQELC(HMM_MAX_HYR)          !Elec Consumpt-Liquifier
!!
!!    size for each prod technology

      REAL PCSIZ(HMM_MAX_HYR,PCHTCH)    !Size -central
      REAL PGSIZ(HMM_MAX_HYR,PGHTCH)    !Size -citygate
      REAL PFSIZ(HMM_MAX_HYR,PFHTCH)    !Size -forecourt

      COMMON/HMMIN/ &
         PCLEV,PGLEV,PFLEV,TCLEV,TGLEV,DOLEV,SOLEV,DMDLEV,NUMSTAT, &
         PCLEX,PGLEX,PFLEX,TCLEX,TGLEX,DOLEX,SOLEX, &
         PCEFF,PGEFF,PFEFF,MGPD, &
         ZPELIN,ZPNGEL,ZPCLEL,ZPBMET,ZPUREL,ZPELCM,ZPNGIN,ZJNGEL,ZJCLEL, &
         PCELC,PGELC,PFELC,SOELC,LIQELC,  &
         PCSIZ,PGSIZ,PFSIZ,  &
         HF_HMM,HF_HMMO,HF_FORDB,HF_MWH,TCHNDX,TGHNDX,DOHNDX,SOHNDX,YRDOLP,YRDOLD

      INTEGER YEARH
      INTEGER PCLIF(HMM_MAX_HYR,PCHTCH)
      REAL*8 ELECD(MNUMCR,MNUMYR),ELEID(MNUMCR,MNUMYR), &
         MDSG(MNUMCR,MNUMYR),PENRAT(MNUMYR)
      REAL*8 CONFCT(7)
      REAL*8 PCCT(HMM_MAX_HYR,PCHTCH),PCFOM(HMM_MAX_HYR,PCHTCH),PCVOM(HMM_MAX_HYR,PCHTCH), &
         PCSSZ(HMM_MAX_HYR,PCHTCH),PCSFC(HMM_MAX_HYR,PCHTCH), &
         PCUTZ(HMM_MAX_HYR,PCHTCH),PCRCC(MNUMYR,MNUMCR,HMKT,PCHTCH), &
         PCECY(MNUMYR,PCHTCH),PCFRF(HMM_MAX_HYR,PCHTCH),PCESALE(HMM_MAX_HYR,PCHTCH)
      REAL*8 TCSZ1(HMM_MAX_HYR,PCHTCH),TCSZ2(HMM_MAX_HYR,PCHTCH)
      REAL*8 TGSZ1(HMM_MAX_HYR,PGHTCH),TGSZ2(HMM_MAX_HYR,PGHTCH)
      REAL*8 TFSZ1(HMM_MAX_HYR,PFHTCH),TFSZ2(HMM_MAX_HYR,PFHTCH)
      REAL*8 PGCT(HMM_MAX_HYR,PGHTCH),PGFOM(HMM_MAX_HYR,PGHTCH),PGVOM(HMM_MAX_HYR,PGHTCH), &
         PGSSZ(HMM_MAX_HYR,PGHTCH),PGSFC(HMM_MAX_HYR,PGHTCH), &
         PGUTZ(HMM_MAX_HYR,PGHTCH),PGRCC(MNUMYR,MNUMCR,HMKT,PGHTCH), &
         PGECY(MNUMYR,PGHTCH),PGFRF(HMM_MAX_HYR,PGHTCH),PGESALE(HMM_MAX_HYR,PGHTCH)
      INTEGER PGLIF(HMM_MAX_HYR,PGHTCH)
      REAL*8 PFCT(HMM_MAX_HYR,PFHTCH),PFFOM(HMM_MAX_HYR,PFHTCH),PFVOM(HMM_MAX_HYR,PFHTCH), &
         PFSFC(HMM_MAX_HYR,PFHTCH), &
         PFUTZ(HMM_MAX_HYR,PFHTCH),PFRCC(MNUMYR,MNUMCR,HMKT,PFHTCH), &
         PFECY(MNUMYR,PFHTCH),PFFRF(HMM_MAX_HYR,PFHTCH),PFESALE(HMM_MAX_HYR,PFHTCH)
      REAL*8 SOCT(HMM_MAX_HYR,SOHTCH),SOFOM(HMM_MAX_HYR,SOHTCH),SOVOM(HMM_MAX_HYR,SOHTCH), &
               SOUTZ(HMM_MAX_HYR,SOHTCH),SORCC(MNUMYR,MNUMCR,HMKT,SOHTCH)
      INTEGER PFLIF(HMM_MAX_HYR,PFHTCH)
      INTEGER SOLIF(HMM_MAX_HYR,SOHTCH)
      INTEGER CITIES(MNUMCR,MNUMNR),MDCTP
      REAL*8 MTTM(MNUMYR,MNUMCR,HMKT),DPY,EFFFC, &
         AVKG,MTTK(MNUMYR,MNUMCR,HMKT), &
         COEHM,CODHM,EQTYHM,FEDRAT,MAXLIQ, &
         COEPIP,CODPIP,EQTYPIP,COETD,CODTD,EQTYTD
      REAL*8 FUNCPI,AREA(MNUMCR,MNUMYR),UTILZ,KGMO, &
         ADPM,HPDO,KGPHD,PUMPS,HDMD(MNUMCR,HMM_MAX_HYR,HMKT)
      CHARACTER*1 HSW_PRCtoTR ,PLPDCOD(HMM_MAX_XYR),FCOD(5),LCOD(4),MCOD(HMKT),RCOD(MNUMCR-2)
      CHARACTER*2 TCODPC(PCHTCH),TCODPG(PGHTCH),TCODPF(PFHTCH),TCODTC(TCHTCH),  &
         TCODTG(TGHTCH),TCODD(DOHTCH),TCODS(SOHTCH),TCODGR(SGHTCH),TCODAC
      CHARACTER*3 ROWDEF(9)
      CHARACTER*30 RGNAM(30)
      CHARACTER*30 TNAM(TOTNDX)
      CHARACTER*1  TFUEL(PCHTCH+PGHTCH+PFHTCH)
      INTEGER NUMPLPD,PLPDYRS(HMM_MAX_XYR),NUMF,NUML,NUMRW,PLPD_PRC_YR,PLPD_INV_YR,PLPD_SYR(HMM_MAX_XYR),PLPD_EYR(HMM_MAX_XYR)
      INTEGER*4 MAP_PC(PCHTCH,2),MAP_PG(PGHTCH,2),MAP_PF(PFHTCH,2),MAP_TC(TCHTCH,2),MAP_TG(TGHTCH,2),MAP_D(DOHTCH,2),MAP_S(SOHTCH,2),MAP_GR(SGHTCH,2),MAP_AC(2)
      REAL*8 P1(12),P2(12),P3(12),P4(12),P7(12),P8(12),L1(12),L2(12),L3(12),L5(12),L6(12), &
             L7(12),L9(12),L10(12),L11(12),L13(12),G7(12),G9(12),G10(12),G11(12), &
             G13(12),G14(12),G15(12),G17(12),G1(12),G2(12),G3(12),G4(12)
      REAL*8 GPRAMULT(3)

      REAL*8 STATETAX(MNUMCR),FEDTAX

      REAL*8 FA_SWITCH

      COMMON/HMMIN2/MAX_LIFE,ELECD,ELEID,MDSG,CONFCT,PENRAT, &
         PCCT,PCFOM,PCVOM,PCSSZ,PCSFC,PCUTZ,PCRCC, &
         PCECY,PCFRF,PCESALE, &
         TCSZ1,TCSZ2,TGSZ1,TGSZ2,TFSZ1,TFSZ2, &
         PGCT,PGFOM,PGVOM,PGSSZ,PGSFC,PGUTZ,PGRCC, &
         PGECY,PGFRF,PGESALE, &
         SOCT,SOFOM,SOVOM,SOUTZ,SORCC, &
         PFCT,PFFOM,PFVOM,PFSFC,PFUTZ,PFRCC, &
         PFECY,PFFRF,PFESALE,PUMPS,HDMD, &
         FUNCPI,AREA,UTILZ,KGMO,ADPM,HPDO,KGPHD, &
         MTTM,DPY,AVKG,MTTK,EFFFC,MAXLIQ, &
         COEHM,CODHM,EQTYHM,FEDRAT, &
         COEPIP,CODPIP,EQTYPIP,COETD,CODTD,EQTYTD, &
         HANSWR,TEMP_HANSWR,MAX_HANSWR,HMM_GNPD,AVG_HYD,AVG_H2D, &
         P1,P2,P3,P4,P7,P8,L1,L2,L3,L5,L6,L7,L9,L10,L11,L13, &
         G7,G9,G10,G11,G13,G14,G15,G17,G1,G2,G3,G4,GPRAMULT,STATETAX,FEDTAX, FA_SWITCH,&
         PLPD_PRC_YR,PLPD_INV_YR,PLPD_SYR,PLPD_EYR,N_FYR,FYR,N_XYR,XYR, &
         YEARH,PCLIF,PGLIF,PFLIF,SOLIF,CITIES,MDCTP, &
         NUMPLPD,PLPDYRS,NUMF,NUML,NUMRW,   &
         MAP_PC,MAP_PG,MAP_PF,MAP_TC,MAP_TG,MAP_D,MAP_S,MAP_GR,MAP_AC, &
         PLPDCOD,FCOD,LCOD,MCOD,RCOD,TCODPC,TCODPG,TCODPF,TCODTC,  &
         TCODTG,TCODD,TCODS,TCODGR,TCODAC,ROWDEF,RGNAM,TNAM,TFUEL, &
         HSW_PRCtoTR

      REAL*8 HANSW2(3,TT2NDX),PCMWH(PCHTCH),PGMWH(PGHTCH),PFMWH(PFHTCH)
      REAL*8 BM_CONS(MNUMCR,NDREG),BM_TOTAL(MNUMCR),BM_SHARE(NDREG)

      COMMON/MKTSHR/HANSW2,PCMWH,PGMWH,PFMWH,BM_CONS,BM_TOTAL,BM_SHARE

      REAL*8 INSTALL_COST(MNUMYR,MNUMCR,HMKT,TOTNDX)          ! Installed cost of unit capacity of technology in $87/kg
      REAL*8 ANNUITY_COST(MNUMYR,MNUMCR,HMKT,TOTNDX)          ! Annuity payment cost of unit capacity of technology in $87/kg
      REAL*8 NEW_BUILDS(MNUMYR,MNUMCR,HMKT,TOTNDX)            ! New installed capacity by technology in bil. kg/yr

      COMMON/TECHCOSTS/INSTALL_COST,ANNUITY_COST,NEW_BUILDS

      REAL*8 DNAME
      REAL*8 VALUE(5),T_RETRO
      REAL*8 HANSWR(MNUMCR,HMKT,TT2NDX,MNUMYR)
      REAL*8 MAX_HANSWR(MNUMCR,HMKT,TT2NDX,MNUMYR)
      REAL*8 TEMP_HANSWR(MNUMCR,HMKT,TT2NDX)
      INTEGER*4 MAX_LIFE(TT2NDX)
      character*16 NAME,BASISYR,LPSOLYR,ACTFIL,HMM_PROB,CRNAME
      character*16 HMM_BND                       ! BOUND ROW NAME
      character*16 HMM_OBJ                       ! OBJECTIVE FUNCTION NAME
      character*16 HMM_RHS                       ! RIGHT HAND SIDE NAME
      character*16 TEMPACT,RMASK,CMASK,COL,ROW,SOLNAME
      CHARACTER*12 BASISIN,BASISOUT,PCKDDN
      CHARACTER*12 MPS_FILE
      CHARACTER*40 DUMMY
      CHARACTER*50 CMD,MPS
      CHARACTER NULLCH/Z00/
      CHARACTER*4 FYR_CHAR
      CHARACTER*2 STAT
      CHARACTER*2 FSTEP(NWDSUPQ+1)
      LOGICAL ONCE/.TRUE./

      INTEGER*4 NBMSTEPS, I_SUPt
      INTEGER*4 MAP_CR_TO_CL(NDREG)
      COMMON /BM_STEPS/ NBMSTEPS, MAP_CR_TO_CL
      DATA MAP_CR_TO_CL/1,2,5,5,5,3,3,6,6,4,4,7,8,8,8,9/

!     Identifier structure (black-box) for the OML database file:
      TYPE(OMLDBFILE) DBFILE
!     Identifier structure (black-box) for a problem in the OML database:
      TYPE(OMLDBPROB) DB

!     For dynamically allocated model space, declare a scalar pointer to real(8).
!     Initialize it to null. Then wfdef will assign it an address if allocation is successful.
!     Specify the desired size (in KB) to wfdef.
      REAL*8, POINTER :: HMMMODEL=>NULL()
      INTEGER*4  HMMKB
      HMMKB = 25*1024   ! = 8*3200000 bytes rounded up to nearest megabyte

      CALL MAINOMLINIT   ! in main.f

!     DETERMINE NUMBER OF BIOMASS SUPPLY STEPS

      NBMSTEPS = NM_BM_SUP_STP

      DO JYR = 1 , LASTYR + 5
         DO I_SUPt = 1 , MNUMFS
            IF (WDSUP_AVL(I_SUPt) .EQ. 1 .AND. MP_BM_H2(I_SUPt) .EQ. 1) THEN
               DO FS = 1 , NWDSUPQ

                  IF (CURIYR .EQ. 58 .AND. CURIRUN .EQ. 1 .AND. FCRL .EQ. 1) THEN
                     WRITE(HF_HMMO,2617) CURIRUN,CURIYR+1989,I_SUPt,FS,JYR,(WDSUP_P(FS,CRG,JYR,I_SUPt),CRG=1,NDREG),   &
                       (WDSUP_Q(FS,CRG,JYR,I_SUPt),CRG=1,NDREG)
 2617                FORMAT(1X,"WDSUPPLY",5(":",I4),32(":",F12.3))
                  END IF

               END DO
            END IF
         END DO
      END DO

      DO FS = 1 , NWDSUPQ
         IF (FS .LT. 10) THEN
            WRITE(FSTEP(FS),'("0",I1)') FS
         ELSE
            WRITE(FSTEP(FS),'(I2)') FS
         END IF

         IF (CURIYR .EQ. 58 .AND. CURIRUN .EQ. 1 .AND. FCRL .EQ. 1) THEN
            WRITE(HF_HMMO,2618) CURIRUN,CURIYR+1989,FS,FSTEP(FS)
 2618       FORMAT(1X,"FSTEP",3(":",I4),":",A2,":")
         END IF

      END DO
!
!     IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1)  &
!        write(*,'(a,2i6,3f10.5)')'CLDBG:Starting hmmmain,y,r,m,HMCLCNS: ',  &
!        curiyr,11,(HMCLCNS(curiyr,11,I),I=1,3)

      IF (CURIYR .EQ. 1 .AND. CURITR .EQ. 1) THEN
         CALL GETDAT
         YEARH=41
         MAX_HANSWR = 0.0
         MAX_LIFE = 999

         ! Retire forecourt production after 15 years
         MAX_LIFE(10) = 15
         MAX_LIFE(37) = 15
         MAX_LIFE(11) = 15
         MAX_LIFE(38) = 15
         MAX_LIFE(12) = 15
         MAX_LIFE(39) = 15

         ! Retire dispensing capacity after 15 years
         MAX_LIFE(24) = 15
         MAX_LIFE(51) = 15
         MAX_LIFE(25) = 15
         MAX_LIFE(52) = 15

      ENDIF
!     WRITE(6,3475) CURIYR+1989,CURITR,N_FYR,PLPD_INV_YR
 3475 FORMAT(1X,"N_FYRrsc",4(":",I4))

!     Move Investment Decisions from Temporary Array to Year Indexed Array

      IF (CURITR .EQ. 1 .AND. CURIYR + PLPD_INV_YR - 2 .GT. 0) THEN
         DO I = 1 , TT2NDX
            DO MINX = 1 , HMKT
               DO CINX = 1 , MNUMCR
!                  WRITE(6,9922) CURIYR+1989,CURITR,CURIYR+PLPD_INV_YR-2+1989, CURIYR+PLPD_INV_YR+MAX_LIFE(I)-3+1989,MAX_LIFE(I),CINX,MINX,I, TEMP_HANSWR(CINX,MINX,I)
! 9922             FORMAT(1X,"MAX_LIFE0",8(":",I4),1(":",F12.6))
                  IF (I .LE. 26) THEN  ! New Capacity Only
                     DO JYR = CURIYR+PLPD_INV_YR-2 , MIN(CURIYR+PLPD_INV_YR+MAX_LIFE(I)-3,MNUMYR)
                        MAX_HANSWR(CINX,MINX,I,JYR) = MAX_HANSWR(CINX,MINX,I,JYR) + TEMP_HANSWR(CINX,MINX,I)
!                        WRITE(6,9923) CURIYR+1989,CURITR,JYR+1989,CINX,MINX,I, MAX_HANSWR(CINX,MINX,I,JYR), TEMP_HANSWR(CINX,MINX,I)
! 9923                   FORMAT(1X,"MAX_LIFE",6(":",I4),2(":",F12.6))
                     END DO
                  END IF
                  HANSWR(CINX,MINX,I,CURIYR+PLPD_INV_YR-2) = TEMP_HANSWR(CINX,MINX,I)
               END DO
            END DO
         END DO
      END IF
!
!     REVISE GNP DEFLATORS
!
      DO FYR = 1 , LASTYR
         HMM_GNPD(FYR) = MC_JPGDP(FYR)
         IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1)  &
         WRITE(HF_HMMO,4733) CURIYR+1989,CURITR,FYR+1989,HMM_GNPD(FYR)
 4733    FORMAT(1X,"HMM_GNPD",3(":",I4),":",F12.6)
      END DO
      GRW = DBLE(MC_JPGDP(LASTYR) / MC_JPGDP(1)) ** (DBLE(1.0) / DBLE(LASTYR - 1.0))
      DO FYR = LASTYR + 1, LASTYR + N_FYR
         HMM_GNPD(FYR) = HMM_GNPD(FYR - 1) * GRW
         IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1)  &
         WRITE(HF_HMMO,4733) CURIYR+1989,CURITR,FYR+1989,HMM_GNPD(FYR)
      END DO

!     FILE_HMM = 6
!
      SOLNAME = "HMM_SOLN"
      HMM_PROB = "ACTPROB"
      HMM_RHS = "RHS_HMM"
      HMM_BND = "BND_HMM"
      HMM_OBJ = "HMM_COST"

      DO CINX = 1, MNUMCR-2
         FYR = PLPD_SYR(PLPD_PRC_YR) + CURIYR - 1
         M_FYR = MIN(FYR , LASTYR)

!        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1)  &
!           write(*,'(a,3i6,3f10.5)')'CLDBG:InCensusLoop,y,r,m,HMCLCNS: ',  &
!           M_FYR,cinx,11,(HMCLCNS(M_FYR,11,I),I=1,3)

!        QEPHM(CINX,M_FYR) = 0.0
!        QENHM(CINX,M_FYR) = 0.0

         QELHM(CINX,M_FYR) = 0.0
         QGFHM(CINX,M_FYR) = 0.0
         QGIHM(CINX,M_FYR) = 0.0
         QNGHM(CINX,M_FYR) = 0.0
         QCLHM(CINX,M_FYR) = 0.0
         QURHM(CINX,M_FYR) = 0.0
         QBMHM(CINX,M_FYR) = 0.0
         QETHM(CINX,M_FYR) = 0.0
         PH1TR(CINX,M_FYR) = 0.0
         PH2TR(CINX,M_FYR) = 0.0
         PH3TR(CINX,M_FYR) = 0.0

         CGHMMGEN(CINX,M_FYR,1,1) = 0.0
         CGHMMCAP(CINX,M_FYR,1)   = 0.0

         DO I = 1 , PCHTCH + PGHTCH + PFHTCH
            HCNSTECH(M_FYR,CINX,I)=0.0
         ENDDO

         DO MINX = 1, HMKT

            HMGSCNS(M_FYR,CINX,MINX) = 0.0
            HMCLCNS(M_FYR,CINX,MINX) = 0.0
            HMBICNS(M_FYR,CINX,MINX) = 0.0
            HMELCNS(M_FYR,CINX,MINX) = 0.0
            HMURCNS(M_FYR,CINX,MINX) = 0.0
            HMETCNS(M_FYR,CINX,MINX) = 0.0
            DO I=1,TOTNDX
               HMMPRD(M_FYR,CINX,MINX,I) = 0.0
               PHMM(M_FYR,CINX,MINX,I) = 0.0
            ENDDO

!           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1)  &
!              write(*,'(a,3i6,3f10.5)')'CLDBG:InMarketLoop,y,r,m,HMCLCNS: ',  &
!              M_FYR,cinx,11,(HMCLCNS(M_FYR,11,I),I=1,3)

!           for now, comment out loading hdmd from transportation variables
          DO K=1,MNUMYR
            IF (MINX .EQ. 1) THEN
                HDMD(CINX,K,MINX) = QH1TR(CINX,K)

                ! debug
                !write(HF_HMMO,'(a,4i6,1f12.2)')'TRANSVAR1', &
                !    K,CURITR,CINX,MINX,QH1TR(CINX,K)
            ENDIF
            IF (MINX .EQ. 2) THEN
                HDMD(CINX,K,MINX) = QH2TR(CINX,K)

                ! debug
                !write(HF_HMMO,'(a,4i6,1f12.2)')'TRANSVAR2', &
                !    K,CURITR,CINX,MINX,QH2TR(CINX,K)
            ENDIF
            IF (MINX .EQ. 3) THEN
                HDMD(CINX,K,MINX) = QH3TR(CINX,K)

                ! debug
                !write(HF_HMMO,'(a,4i6,1f12.2)')'TRANSVAR3', &
                !    K,CURITR,CINX,MINX,QH3TR(CINX,K)
            ENDIF
          ENDDO
!          IF (HDMD(CINX,LASTYR-5,MINX).GT.0) THEN
!            GRW = DBLE(HDMD(CINX,LASTYR,MINX) / HDMD(CINX,LASTYR-5,MINX)) ** (DBLE(1.0) / DBLE(5.0))
!            GRW = MIN(1.05,GRW)     ! Limit future demand growth to 5% per year
!            GRW = MAX(1.01,GRW)     ! Do not let future demand decline
!          ELSE
!            GRW = 1.01
!          ENDIF
!          DO K = LASTYR+1 , HMM_MAX_HYR
!             HDMD(CINX,K,MINX) = HDMD(CINX,K-1,MINX) * GRW
!          END DO

          IF (CURIYR.GE.6) THEN
             IF (HDMD(CINX,CURIYR-5,MINX).GT.0) THEN               ! Grow future year demands based on the previous 5 years
               GRW = DBLE(HDMD(CINX,CURIYR,MINX) / HDMD(CINX,CURIYR-5,MINX)) ** (DBLE(1.0) / DBLE(5.0))
            GRW = MIN(1.05,GRW)     ! Limit future demand growth to 5% per year
            GRW = MAX(1.01,GRW)     ! Do not let future demand decline
          ELSE
            GRW = 1.01
          ENDIF
          ELSE
             GRW = 1.01
          ENDIF
          DO K = CURIYR+1 , HMM_MAX_HYR
             IF (K.LE.LASTYR) THEN
                HDMD(CINX,K,MINX) = MAX(HDMD(CINX,K-1,MINX)*GRW,HDMD(CINX,K,MINX))
             ELSE
             HDMD(CINX,K,MINX) = HDMD(CINX,K-1,MINX) * GRW
             ENDIF
          END DO


          TOT_MGPD = 0.0
          DO FYR = CURIYR , MIN(CURIYR + N_FYR - 1 , HMM_MAX_HYR)

             MGPD(FYR,CINX,MINX) = HDMD(CINX,FYR,MINX) / 1000.0 / (CONFCT(1) / 1000000.0) / 0.365
             TOT_MGPD = TOT_MGPD + MGPD(FYR,CINX,MINX)

             WRITE(HF_HMMO,3424) CURIYR+1989,CURITR,CINX,MINX,FYR, &
                MGPD(FYR,CINX,MINX),HDMD(CINX,FYR,MINX),TOT_MGPD,CONFCT(1),GRW
3424         FORMAT(1X,"HDMD_3",5(":",I4),5(":",E15.6))

          END DO

          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1)  &
            WRITE(HF_HMMO,3420) CURIYR+1989,CURITR,CINX,MINX,HDMD(CINX,CURIYR,MINX),TOT_MGPD
3420      FORMAT(1X,"HDMD_TOT_MGPD",4(":",I4),2(":",E15.6))

         END DO ! MINX

      END DO ! CINX

      IF (TOT_MGPD .GT. 0.0) THEN
!
         YEARH=MIN(YEARH,CURIYR)
         WRITE(HF_HMMO,2043)'YEARH ',YEARH,CURIYR
 2043    FORMAT(A12,1x,2(I4,1x))

         WRITE(HF_HMMO,*) 'BEGIN OML SECTION'
!
!        REMOVE ACTFILE CHOICE, WITH IN-MEMORY ONE ACTFILE ONLY
!

         FULLYR = CURIYR + 1989
         WRITE(ACTFIL,5215) FULLYR
 5215    FORMAT("HMM_",I4)
!
!        CREATE NEW MATRIX, DO DATABASE INITIALIZATION, REVISIONS AND CLOSES.
!
         CALL MPTIM2(CPU_TIME_BEGIN)
         WRITE(HF_HMMO,1111)'** BEGIN DBINIT ** ',FLOAT(CPU_TIME_BEGIN)/100.
 1111    FORMAT(10X,A,' CPU TIME (SECONDS) = ',F7.2,A,F7.2)
!
         WRITE(HF_HMMO,*) 'CALLING DFOPEN'
         IRET = DFOPEN(DBFILE,ACTFIL)
         IF (IRET .NE. 0) THEN
            WRITE(HF_HMMO,17) IRET
         ENDIF
!
!        SPECIFY A PROBLEM IN DATABASE FOR PROCESSING
!
         WRITE(HF_HMMO,*) 'CALLING DFPINIT'
         IRET = DFPINIT(DB,DBFILE,HMM_PROB)
         IF (IRET .NE. 0) THEN
            WRITE(HF_HMMO,18) IRET
         ENDIF

!        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1)  &
!           write(*,'(a,3i6,3f10.5)')'CLDBG:afterDFPINIT,y,r,m,HMCLCNS: ',  &
!           curiyr,cinx,11,(HMCLCNS(curiyr,11,I),I=1,3)
!
!        Always Create New Matrix
!
         HMM_MAT = 0
         WRITE(HF_HMMO,*) 'CALLING DFMINIT'
         IRET = DFMINIT(DB,HMM_MAT)
         IF (IRET .NE. 0) THEN
            WRITE(HF_HMMO,19) IRET
         ENDIF
!
         CALL MPTIM2(CPU_TIME_END)
         WRITE (HF_HMMO,2222) '** END DBINIT ** ',FLOAT(CPU_TIME_END)/100., FLOAT(CPU_TIME_END)/100. - FLOAT(CPU_TIME_BEGIN)/100.
 2222    FORMAT(10X,A,' CPU TIME (SECONDS) = ',F7.2, ', TIME USED = ',F7.2)

!        CALL SUBROUTINE TO CALL REVISE ROUTINES

         CALL REVISE_HMM
!
         WRITE(HF_HMMO,*) 'AFTER ACTFILE REVISIONS'

         CALL MPTIM2(CPU_TIME_BEGIN)
         WRITE (HF_HMMO,1111) '** BEGIN DF SUBS ** ', FLOAT(CPU_TIME_BEGIN)/100.

!        END OF MATRIX PROCESSING

         IRET = DFMSTAT(STATS)
!
         DO I = 1 , 9
            WRITE(HF_HMMO,*) 'STAT ',I,STATS(I)
         ENDDO
!
         WRITE(HF_HMMO,*) 'CALLING DFMEND'
!
         IRET = DFMEND()
!
         IF (IRET .NE. 0) THEN
            WRITE(HF_HMMO,20) IRET
         ENDIF
!
!        END OF MATRIX PROCESSING
!
         WRITE(HF_HMMO,*) 'CALLING DFCLOSE'
         IRET = DFCLOSE(DBFILE)
         IF (IRET .NE. 0) THEN
            WRITE(HF_HMMO,21) IRET
         ENDIF
!
         CALL MPTIM2(CPU_TIME_END)
         WRITE (HF_HMMO,2222) '** END DF SUBS ** ',FLOAT(CPU_TIME_END)/100.0, FLOAT(CPU_TIME_END)/100.0 - FLOAT(CPU_TIME_BEGIN)/100.0
!
!        DEFINE AND LOAD MODEL.
!
         CALL MPTIM2(CPU_TIME_BEGIN)
         WRITE (HF_HMMO,1111) '** BEGIN MODEL DEF ** ', FLOAT(CPU_TIME_BEGIN)/100.
!
         WRITE(HF_HMMO,*) 'WFDEF'
         IRET = WFDEF(HMMMODEL,HMMKB,'HMMHMM  ')   ! DEFINE LP MODEL SPACE
         IF (IRET .LE. 0) THEN
            WRITE(HF_HMMO,10) IRET
         ENDIF
!
         OML.XUNIQUES = 0
         OML.XFREQINV = 500
         OML.XFREQSUM = 500
         OML.XMINMAX='MIN'
         OML.XOBJ = HMM_OBJ
         OML.XRHS = HMM_RHS
!        OML.XBOUND = HMM_BND
!
         CALL MPTIM2(CPU_TIME_END)
         WRITE (HF_HMMO,2222) '** END MODEL DEF ** ',FLOAT(CPU_TIME_END)/100., FLOAT(CPU_TIME_END)/100. - FLOAT(CPU_TIME_BEGIN)/100.
!
         CALL MPTIM2(CPU_TIME_BEGIN)
         WRITE (HF_HMMO,1111) '** BEGIN MTX LOAD ** ', FLOAT(CPU_TIME_BEGIN)/100.
!
         WRITE(HF_HMMO,*) 'WFLOAD'
!
         IRET = WFLOAD(ACTFIL,HMM_PROB)        ! LOAD MATRIX INTO MEMORY
!
         IF (IRET .NE. 0) THEN
            WRITE(HF_HMMO,12) IRET
         ENDIF
!
         CALL MPTIM2(CPU_TIME_END)
         WRITE (HF_HMMO,2222) '** END MTX LOAD ** ',FLOAT(CPU_TIME_END)/100., FLOAT(CPU_TIME_END)/100. - FLOAT(CPU_TIME_BEGIN)/100.
!
         CALL MPTIM2(CPU_TIME_BEGIN)
         WRITE (HF_HMMO,1111) '** BEGIN BASIS LOAD ** ', FLOAT(CPU_TIME_BEGIN)/100.
!
         WRITE(FYR_CHAR,'(I4)') FULLYR
         BASISYR = 'HMM' // FYR_CHAR
!
         BASISOUT = "HMM" // FYR_CHAR // ".dat" // char(0)
!
!        IF FIRST TIME THROUGH USE BASIS FROM BASHMMI
!
!        WRITE(HF_HMMO,*) 'WFINSRT'                                            !
!        IRET = WFINSRT(BASISIN,BASISYR)                      !  LOAD BASIS
!        IF (IRET .NE. 0) THEN
!           WRITE(HF_HMMO,14) IRET
!        END IF
!
         CALL MPTIM2(CPU_TIME_END)
         WRITE(HF_HMMO,2222) '** END BASIS LOAD ** ',FLOAT(CPU_TIME_END)/100.0, FLOAT(CPU_TIME_END)/100. - FLOAT(CPU_TIME_BEGIN)/100.
!
         OML.XSOLPRNT = 'NO'

         CALL MPTIM2(CPU_TIME_BEGIN)
         WRITE (HF_HMMO,1111) '** BEGIN SOLVING ** ',FLOAT(CPU_TIME_BEGIN)/100.
         WRITE(HF_HMMO,*) 'WFOPT'
         SV_SW = 0
!
!        TURN OFF POST SOLVE AND FIND OPTIMAL SOLUTION OF PROBLEM RESULTING FROM FULL PRESOLVE
!
! commenting out the write of the communication region for now, here and several lines down
! the statement below doesn't compile under the new OML
         WRITE(CRNAME,4311) FYR_CHAR(3:2)
!        IRET = WCWRCR(CRNAME)
!
         OML.XRUNMODE = 8
         IRET = WFOPT()                                  !  OPTIMIZE MATRIX
         LRET = IRET
!
!        IF UNABLE TO FIND A SOLUTION TRY REOPTIMIZING WITH RESULTING BASIS
!
         IF (IRET .NE. 0) THEN
            WRITE(CRNAME,4311) FYR_CHAR(3:2)
 4311       FORMAT("CR",A2,".HMM")
!           IRET = WCWRCR(CRNAME)
            IRET = WFOPT()
         END IF
         KRET = IRET
         IF (IRET .NE. 0) WRITE(6,28) LRET,IRET,FULLYR
!
!        NOW TURN POST SOLVE BACK ON AND TURN PRESOLVE OFF AND REOPTIMIZE TO GET POST SOLVE INFO - ROW DUALS AND COLUMN REDUCED COSTS
!
         OML.XRUNMODE = 0
         OML.XCRASHSW = 0
         IRET = WFOPT()                                  !  OPTIMIZE MATRIX
!
!        TURN PRESOLVE BACK ON
!
         OML.XCRASHSW = 8
!
         CALL MPTIM2(CPU_TIME_END)
         WRITE (HF_HMMO,2222) '** END SOLVING ** ',FLOAT(CPU_TIME_END)/100., &
            FLOAT(CPU_TIME_END)/100. - FLOAT(CPU_TIME_BEGIN)/100.
!
         IF (IRET .NE. 0) THEN
            WRITE(HF_HMMO,15) IRET
            IF (IRET .EQ. 9) THEN
               WRITE(6,26) KRET,IRET,FULLYR
            ELSE IF (IRET .EQ. 12) THEN
               WRITE(6,27) KRET,IRET,FULLYR
            ELSE
               WRITE(6,26) KRET,IRET,FULLYR
            END IF
            IF (IRET .NE. 12) THEN
               SV_SW = IRET
            END IF
         END IF
!
         CALL MPTIM2(CPU_TIME_END)
         WRITE (HF_HMMO,2222) '** END SOLVING ** ',FLOAT(CPU_TIME_END)/100., &
            FLOAT(CPU_TIME_END)/100. - FLOAT(CPU_TIME_BEGIN)/100.
!
         CALL MPTIM2(CPU_TIME_BEGIN)
         WRITE (HF_HMMO,1111) '** BEGIN WRBASIS ** ',FLOAT(CPU_TIME_BEGIN)/100.
         WRITE(HF_HMMO,*) 'WFPUNCH'
!
!         IRET = WFPUNCH(BASISOUT,BASISYR)          !WRITE BASIS TO STD FORM
!         IF (IRET .NE. 0) THEN
!            WRITE(HF_HMMO,16) IRET
!         END IF
!
         CALL MPTIM2(CPU_TIME_END)
         WRITE (HF_HMMO,2222) '** END WRBASIS ** ',FLOAT(CPU_TIME_END)/100., &
            FLOAT(CPU_TIME_END)/100. - FLOAT(CPU_TIME_BEGIN)/100.
!
         CALL MPTIM2(CPU_TIME_BEGIN)
         WRITE (HF_HMMO,1111) '** BEGIN SOLN RETRIEVE ** ', &
            FLOAT(CPU_TIME_BEGIN)/100.
!
!        PUT SOLUTION RETRIEVALS HERE
!
         IF (SV_SW .EQ. 0) THEN
!
            CALL RETRIEVE_HMM
!
         ELSE

            WRITE(6,*)'Solution failed, not retrieving solution!!'

         END IF ! SV_SW 0-Yes >0-No
!
         CALL MPTIM2(CPU_TIME_END)
         WRITE (HF_HMMO,2222) '** END SOLN RETREIVE ** ', &
            FLOAT(CPU_TIME_END)/100., &
            FLOAT(CPU_TIME_END)/100. - FLOAT(CPU_TIME_BEGIN)/100.

      ENDIF

      CALL HMMNATS

      DO I=1,TOTNDX
         DO J=1,HMKT
          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,8942)'NATS DB2',M_FYR,J,I,HMMPRD(M_FYR,MNUMCR,J,I)
8942     FORMAT(A12,1x,3(I4,1x),F15.6)
         ENDDO
      ENDDO

      ! Create the cost report data if this is the final model year
      IF (CURIYR.EQ.LASTYR .AND. FCRL.EQ.1) THEN
         CALL COST_REPORT
      ENDIF

!     ENDIF
!
   10 FORMAT(1X,'RF:WFDEF ERROR, CODE=',I4)
   11 FORMAT(1X,'RF:WFMPSIN ERROR, CODE=',I4)
   12 FORMAT(1X,'RF:WFLOAD ERROR, CODE=',I4)
   13 FORMAT(1X,'RF:WFSET ERROR, CODE=',I4)
   14 FORMAT(1X,'RF:WFINSRT ERROR, CODE=',I4)
   15 FORMAT(1X,'RF:WFOPT ERROR, CODE=',I4)
   16 FORMAT(1X,'RF:WFPUNCH ERROR,CODE=',I4)
   17 FORMAT(1X,'RF:DFOPEN ERROR,CODE=',I4)
   18 FORMAT(1X,'RF:DFPINIT ERROR,CODE=',I4)
   19 FORMAT(1X,'RF:DFMINIT ERROR,CODE=',I4)
   20 FORMAT(1X,'RF:MFEND ERROR,CODE=',I4)
   21 FORMAT(1X,'RF:DFCLOSE ERROR,CODE=',I4)
   22 FORMAT(1X,'RF:WFEND ERROR,CODE=',I4)
   23 FORMAT(1X,'RF:GOMHOT ERROR,CODE=',I4)
   25 FORMAT(1X,/,' WFOPT ERROR, CODES: ',2I4, ' *** WARNING: HMM WAS UNABLE TO COMPLETE THE POST SOLVE IN ',I4)
   26 FORMAT(1X,/,' WFOPT ERROR, CODES: ',2I4, ' *** WARNING: HMM GOT UNRESOLVED COMPUTATIONAL ERRORS IN ',I4)
   27 FORMAT(1X,/,' WFOPT ERROR, CODES: ',2I4, ' *** WARNING: HMM WAS CYCLING - I.E. MAKING PIVOTS BUT NOT IMPROVING THE OBJECTIVE FUNCTION IN ',I4)
   28 FORMAT(1X,/,' WFOPT ERROR, CODE: ',2I4, ' *** WARNING: HMM INFEASIBLE OR CANNOT SOLVE IN ',I4,' THIS IS THE REAL DEAL')

!     FREE THE ALLOCATED MEMORY FOR THE OML MODEL
      IRET = WFFREEMODEL(HMMMODEL)

      RETURN
!
      CONTAINS


      SUBROUTINE REVISE_HMM
! This subroutine creates the HMM Matrix

      IMPLICIT NONE

      INTEGER*4 IRET,I,J,K,L,RWNUM,I2,J2,K2,I3,T_N,T_E,X
      REAL*8 VALUE
      REAL*8 CRF,PWF,PVV,PVBV,CRF2
      REAL*8 PCCST(PCHTCH),PGCST(PGHTCH),PFCST(PFHTCH), &
         TCCST(TCHTCH),TGCST(TGHTCH),DOCST(DOHTCH),SOCST(SOHTCH), &
         DMD,SGCST
      INTEGER CINX,MINX
      character*16 COLUMN,ROW

      character*16    HMM_BND                       ! BOUND ROW NAME
      character*16    HMM_OBJ                       ! OBJECTIVE FUNCTION NAME
      character*16    HMM_RHS                       ! RIGHT HAND SIDE NAME

!     REMEMBER TO CHANGE THESE IF NUMBER OF TECHS CHANGE
!
      CHARACTER*2 TECH
      CHARACTER*1 PCOD(2)
!
      HMM_RHS = "RHS_HMM"
      HMM_BND = "BND_HMM"
      HMM_OBJ = "HMM_COST"

      DATA PCOD/"N","E"/
!
      DO CINX = 1 , MNUMCR - 2  ! census regions

         DO MINX = 1, HMKT          ! markets

            RWNUM = 0
            write(HF_HMMO,*)'IN REVISE_HMM,cinx,minx,curiyr:',CINX,MINX,CURIYR
!
!           Create LP Structure for Each Explicit Planning Period
!
            DO XYR = 1 , N_XYR

!               WRITE(6,3333) CURIYR+1989,CURITR,CINX,MINX,XYR,N_XYR
!3333           FORMAT(1X,"WHERE_AM_I_1",6(":",I4))

               CALL GETCST(CINX,MINX)

               DO I3=1,2    ! new, existing

                  IF (I3 .EQ. 2 .OR. XYR .GE. PLPD_INV_YR) THEN

                     DO I=1,PCHTCH
                        PCCST(I)=0.0
                        PCMWH(I)=0.0
                     ENDDO
                     DO I=1,DOHTCH
                        DOCST(I)=0.0
                     ENDDO
                     DO I=1,PGHTCH
                       PGCST(I)=0.0
                       PGMWH(I)=0.0
                     ENDDO
                     DO I=1,PFHTCH
                       PFCST(I)=0.0
                       PFMWH(I)=0.0
                     ENDDO
                     DO I=1,TCHTCH
                       TCCST(I)=0.0
                       TGCST(I)=0.0
                       SOCST(I)=0.0
                     ENDDO

!                    SGCST = 0.0
!
!                    load SGCST with net present value of nominal electricity price
!
                     ELEC = 0.0
                     INFL = 0.0
                     KG = 0.0
                     DO JYR = 1 , PLPDYRS(XYR)
                        FYR = JYR + PLPD_SYR(XYR) + CURIYR - 2
                        M_FYR = MIN(FYR , LASTYR)
                        ELEC(JYR) = PELBS(CINX,M_FYR) * HMM_GNPD(FYR)
                        INFL(JYR) = HMM_GNPD(FYR)
                        KG(JYR) = 1.0
                     END DO
                     FYR = PLPDYRS(XYR)
                     PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                     PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                     PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                     SGCST = PV_ELEC

                     IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) then
                        write(HF_HMMO,'(a,5i6,5f12.2)')'SGCSTdbg,y,i,r,m,sgcst,pelme,gdp:',  &
                           CURIYR,CURITR,CINX,MINX,XYR,SGCST,PELME(CINX,CURIYR),MC_JPGDP(CURIYR),PV_INFL,PV_KG
                        write(HF_HMMO,6789) CURIYR+1989,CURITR,CINX,MINX,XYR,PLPD_SYR(XYR),FYR,SGCST,PELBS(CINX,CURIYR),MC_JPGDP(CURIYR),PV_INFL,PV_KG,RORHM,PVV(KG,HMM_MAX_FYR,FYR,RORHM),PWF(RORHM,PLPD_SYR(XYR)-1)
 6789                   format(1x,"SGCSTrsc",7(":",I4),8(":",F12.6))
                     endif

!                    DKG - NOTE THAT SGCST needs to be loaded with the electricity cost
!                    DKG - AND PCMWH,PGMWH,PFMWH need to be loaded with MWH by technology

                     IF (I3.EQ.1) THEN

                       DO I=1,PCHTCH
                         PCCST(I) = PCLEV(CURIYR,CINX,MINX,I)

!                        Calculate Net Present Value Average for Electricity Sales to Grid

                         ELEC = 0.0
                         KG = 0.0
                         DO JYR = 1 , PLPDYRS(XYR)
                            FYR = JYR + PLPD_SYR(XYR) + CURIYR - 2
                            M_FYR = MIN(FYR , LASTYR)
                            ELEC(JYR) = PCESALE(M_FYR,I) / 1000000.0
                            KG(JYR) = 1.0
                         END DO
                         FYR = PLPDYRS(XYR)
                         PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                         PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                         PCMWH(I) = PV_ELEC / PV_KG

                         IF (FCRL .EQ. 1 .AND. XYR .EQ. 1 .AND. CINX .EQ. 1 .AND. MINX .EQ. 1) THEN
                            K2 = MAP_PC(I,1)
                            IF (K2 .GT. 0) THEN
                                write(HF_MWH,2500) CURIYR+1989,K2,PCMWH(I)
                            ENDIF
                         ENDIF
2500                     format(1x,"MWH",2(":",I4),1(":",F12.6))

                       ENDDO
                       DO I=1,PGHTCH
                         PGCST(I) = PGLEV(CURIYR,CINX,MINX,I)

!                        Calculate Net Present Value Average for Electricity Sales to Grid

                         ELEC = 0.0
                         KG = 0.0
                         DO JYR = 1 , PLPDYRS(XYR)
                            FYR = JYR + PLPD_SYR(XYR) + CURIYR - 2
                            M_FYR = MIN(FYR , LASTYR)
                            ELEC(JYR) = PGESALE(M_FYR,I) / 1000000.0
                            KG(JYR) = 1.0
                         END DO
                         FYR = PLPDYRS(XYR)
                         PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                         PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                         PGMWH(I) = PV_ELEC / PV_KG

                         IF (FCRL .EQ. 1 .AND. XYR .EQ. 1 .AND. CINX .EQ. 1 .AND. MINX .EQ. 1) THEN
                            K2 = MAP_PG(I,1)
                            IF (K2 .GT. 0) THEN
                                write(HF_MWH,2500) CURIYR+1989,K2,PGMWH(I)
                            ENDIF
                         ENDIF

                       ENDDO
                       DO I=1,PFHTCH
                         PFCST(I) = PFLEV(CURIYR,CINX,MINX,I)

!                        Calculate Net Present Value Average for Electricity Sales to Grid

                         ELEC = 0.0
                         KG = 0.0
                         DO JYR = 1 , PLPDYRS(XYR)
                            FYR = JYR + PLPD_SYR(XYR) + CURIYR - 2
                            M_FYR = MIN(FYR , LASTYR)
                            ELEC(JYR) = PFESALE(M_FYR,I) / 1000000.0
                            KG(JYR) = 1.0
                         END DO
                         FYR = PLPDYRS(XYR)
                         PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                         PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                         PFMWH(I) = PV_ELEC / PV_KG

                         IF (FCRL .EQ. 1 .AND. XYR .EQ. 1 .AND. CINX .EQ. 1 .AND. MINX .EQ. 1) THEN
                            K2 = MAP_PF(I,1)
                            IF (K2 .GT. 0) THEN
                                write(HF_MWH,2500) CURIYR+1989,K2,PFMWH(I)
                            ENDIF
                         ENDIF

                         IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,4i6,2f12.3)')'PFCST-LP,y,rgn,mkt,tech: ',  &
                           curiyr,cinx,minx,I,PFCST(I),PFLEV(CURIYR,CINX,MINX,I)

                       ENDDO
                       DO I=1,TCHTCH
                         TCCST(I) = TCLEV(CURIYR,CINX,MINX,I)
                       ENDDO
                       DO I=1,TGHTCH
                         TGCST(I) = TGLEV(CURIYR,CINX,MINX,I)
                       ENDDO
                       DO I=1,DOHTCH
                         DOCST(I) = DOLEV(CURIYR,CINX,MINX,I)
                       ENDDO
                       DO I=1,SOHTCH
                         SOCST(I) = SOLEV(CURIYR,CINX,MINX,I)
                       ENDDO

                     ELSE IF (I3.EQ.2) THEN
                       DO I=1,PCHTCH
                         PCCST(I) = PCLEX(CURIYR,CINX,MINX,I)

!                        Calculate Net Present Value Average for Electricity Sales to Grid

                         ELEC = 0.0
                         KG = 0.0
                         DO JYR = 1 , PLPD_EYR(XYR)
                            FYR = JYR + CURIYR - 1
                            M_FYR = MIN(FYR , LASTYR)
                            ELEC(JYR) = PCESALE(M_FYR,I) / 1000000.0
                            KG(JYR) = 1.0
                         END DO
                         FYR = PLPDYRS(XYR)
                         PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                         PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                         PCMWH(I) = PV_ELEC / PV_KG
                       ENDDO
                       DO I=1,PGHTCH
                         PGCST(I) = PGLEX(CURIYR,CINX,MINX,I)

!                        Calculate Net Present Value Average for Electricity Sales to Grid

                         ELEC = 0.0
                         KG = 0.0
                         DO JYR = 1 , PLPD_EYR(XYR)
                            FYR = JYR + CURIYR - 1
                            M_FYR = MIN(FYR , LASTYR)
                            ELEC(JYR) = PGESALE(M_FYR,I) / 1000000.0
                            KG(JYR) = 1.0
                         END DO
                         FYR = PLPDYRS(XYR)
                         PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                         PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                         PGMWH(I) = PV_ELEC / PV_KG
                       ENDDO
                       DO I=1,PFHTCH
                         PFCST(I) = PFLEX(CURIYR,CINX,MINX,I)

!                        Calculate Net Present Value Average for Electricity Sales to Grid

                         ELEC = 0.0
                         KG = 0.0
                         DO JYR = 1 , PLPD_EYR(XYR)
                            FYR = JYR + CURIYR - 1
                            M_FYR = MIN(FYR , LASTYR)
                            ELEC(JYR) = PFESALE(M_FYR,I) / 1000000.0
                            KG(JYR) = 1.0
                         END DO
                         FYR = PLPDYRS(XYR)
                         PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                         PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                         PFMWH(I) = PV_ELEC / PV_KG
                       ENDDO
                       DO I=1,TCHTCH
                         TCCST(I) = TCLEX(CURIYR,CINX,MINX,I)
                       ENDDO
                       DO I=1,TGHTCH
                         TGCST(I) = TGLEX(CURIYR,CINX,MINX,I)
                       ENDDO
                       DO I=1,DOHTCH
                         DOCST(I) = DOLEX(CURIYR,CINX,MINX,I)
                       ENDDO
                       DO I=1,SOHTCH
                         SOCST(I) = SOLEX(CURIYR,CINX,MINX,I)
                       ENDDO

                     ENDIF  ! I3

                     ! Make sure demand does not fall period-over-period
                     IF (XYR .EQ. 1) THEN
                        DMD = DMDLEV(CURIYR,CINX,MINX,XYR)
                     ELSE
                        DMD = MAX(DMDLEV(CURIYR,CINX,MINX,XYR),DMDLEV(CURIYR,CINX,MINX,XYR-1)+0.001)
                     END IF
                     IF (CURIYR.GE.51) WRITE(HF_HMMO,3527) CURIYR+1989,CURITR,I,J,XYR,DMD
3527                 FORMAT('REVISE_DMDDB',":",5(I4,":"),F12.6)


!
!                    SET UP COLUMNs
!
                     IRET = DFMCRTP(HMM_OBJ,'N       ')

!                    Only Create New Build/Operate Vectors When Allowed (i.e. XYR >= PLPD_INV_YR)

                     IF (I3 .EQ. 2 .OR. XYR .GE. PLPD_INV_YR) THEN
!
!                       Production
!
                        I=1
!
!                       Central
!
                        J=1
                        DO K=1,PCHTCH
                           IF (MAP_PC(K,I3) .GT. 0) THEN
                              COLUMN = PCOD(I3) //FCOD(I) // LCOD(J) // TCODPC(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                              ROW = HMM_OBJ
                              VALUE = PCCST(K)
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                              CALL HVAL(COLUMN,ROW,VALUE)

!                             Use Biomass From Biomass Supply Curves

                              IF (K.eq.5 .or. K.eq.8) then     !Biomass
                                 IF (I3 .EQ. 1) THEN                 ! New
                                    DO X = XYR , N_XYR
                                       FYR = PLPD_SYR(XYR) + CURIYR - 1
                                       M_FYR = MIN(FYR , LASTYR)
                                       VALUE = PCEFF(M_FYR,K) / 1000.0
                                       ROW = 'FWDCN'  // PLPDCOD(X) // RCOD(CINX)
                                       write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                       CALL HVAL(COLUMN,ROW,VALUE)
                                    END DO
                                 ELSE                                ! Existing
                                    DO X = 1 , XYR
                                       FYR = PLPD_SYR(XYR) + CURIYR - 1
                                       M_FYR = MIN(FYR , LASTYR)
                                       VALUE = PCEFF(M_FYR,K) / 1000.0
                                       ROW = 'FWDCN'  // PLPDCOD(X) // RCOD(CINX)
                                       write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                       CALL HVAL(COLUMN,ROW,VALUE)
                                    END DO
                                 END IF
                              END IF

                              IF (I3.EQ.2) THEN
                                 ROW = PCOD(I3) //FCOD(I) // LCOD(J) // TCODPC(K) // MCOD(MINX) // "_" // RCOD(CINX)
                                 VALUE = 1.0
                                 CALL HVAL(COLUMN,ROW,VALUE)
                                 IF (XYR .EQ. N_XYR) THEN
                                    IRET = DFMCRTP(ROW,'L       ')
                                    COLUMN = HMM_RHS
                                    T_N = MAP_PC(K,1)
                                    T_E = MAP_PC(K,2)
                                    VALUE = MIN(MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
                                            (HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)) + &
                                             HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))))

!                                    WRITE(6,9937) CURIYR+1989,CURITR,T_N,T_E, ROW, MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
!                                       HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))
! 9937                               FORMAT(1X,"MAX_HANSWR",4(":",I4),":",A8,3(":",F12.6))

                                    if (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1004) curiyr,curitr,cinx,minx,HMM_RHS,ROW,0,VALUE

                                    CALL HRHS(COLUMN,ROW,VALUE)
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDDO
!
!                       City Gate
!
                        J=2
                        DO K=1,PGHTCH
                           IF (MAP_PG(K,I3) .GT. 0) THEN
                              COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODPG(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                              ROW = HMM_OBJ
                              VALUE = PGCST(K)
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                              CALL HVAL(COLUMN,ROW,VALUE)
                              IF (I3.EQ.2) THEN
                                 ROW = PCOD(I3) //FCOD(I) // LCOD(J) // TCODPG(K) // MCOD(MINX) // "_" // RCOD(CINX)
                                 VALUE = 1.0
                                 CALL HVAL(COLUMN,ROW,VALUE)
                                 IF (XYR .EQ. N_XYR) THEN
                                    IRET = DFMCRTP(ROW,'L       ')
                                    COLUMN = HMM_RHS
                                    T_N = MAP_PG(K,1)
                                    T_E = MAP_PG(K,2)
                                    VALUE = MIN(MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
                                            (HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)) + &
                                             HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))))

!                                    WRITE(6,9937) CURIYR+1989,CURITR,T_N,T_E, ROW, MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
!                                       HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))

                                    IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1004) curiyr,curitr,cinx,minx,HMM_RHS,ROW,0,VALUE

                                    CALL HRHS(COLUMN,ROW,VALUE)
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDDO
!
!                       Forecourt
!
                        J=3
                        DO K=1,PFHTCH
                           IF (MAP_PF(K,I3) .GT. 0) THEN
                              COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODPF(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                              ROW = HMM_OBJ
                              VALUE = PFCST(K)
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                              CALL HVAL(COLUMN,ROW,VALUE)
                              IF (I3.EQ.2) THEN
                                 ROW = PCOD(I3) //FCOD(I) // LCOD(J) // TCODPF(K) // MCOD(MINX) // "_" // RCOD(CINX)
                                 VALUE = 1.0
                                 CALL HVAL(COLUMN,ROW,VALUE)
                                 IF (XYR .EQ. N_XYR) THEN
                                    IRET = DFMCRTP(ROW,'L       ')
                                    COLUMN = HMM_RHS
                                    T_N = MAP_PF(K,1)
                                    T_E = MAP_PF(K,2)
                                    VALUE = MIN(MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
                                            (HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)) + &
                                             HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))))

!                                    WRITE(6,9937) CURIYR+1989,CURITR,T_N,T_E, ROW, MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
!                                       HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))

                                    IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1004) curiyr,curitr,cinx,minx,HMM_RHS,ROW,0,VALUE

                                    CALL HRHS(COLUMN,ROW,VALUE)
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDDO
!
!                       Transport
!
                        I=2
!
!                       Central
!
                        J=1
                        DO K=1,TCHTCH
                           IF (MAP_TC(K,I3) .GT. 0) THEN
                              COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODTC(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                              ROW = HMM_OBJ
                              VALUE = TCCST(K)
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                              CALL HVAL(COLUMN,ROW,VALUE)
                              IF (I3.EQ.2) THEN
                                 ROW = PCOD(I3) //FCOD(I) // LCOD(J) // TCODTC(K) // MCOD(MINX) // "_" // RCOD(CINX)
                                 VALUE = 1.0
                                 CALL HVAL(COLUMN,ROW,VALUE)
                                 IF (XYR .EQ. N_XYR) THEN
                                    IRET = DFMCRTP(ROW,'L       ')
                                    COLUMN = HMM_RHS
                                    T_N = MAP_TC(K,1)
                                    T_E = MAP_TC(K,2)
                                    VALUE = MIN(MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
                                            (HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)) + &
                                             HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))))

!                                    WRITE(6,9937) CURIYR+1989,CURITR,T_N,T_E, ROW, MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
!                                       HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))

                                    IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1004) curiyr,curitr,cinx,minx,HMM_RHS,ROW,0,VALUE

                                    CALL HRHS(COLUMN,ROW,VALUE)
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDDO
!
!                       City Gate
!
                        J=2
                        DO K=1,TGHTCH
                           IF (MAP_TG(K,I3) .GT. 0) THEN
                              COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODTG(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                              ROW = HMM_OBJ
                              VALUE = TGCST(K)
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                              CALL HVAL(COLUMN,ROW,VALUE)
                              IF (I3.EQ.2) THEN
                                 ROW = PCOD(I3) //FCOD(I) // LCOD(J) // TCODTG(K) // MCOD(MINX) // "_" // RCOD(CINX)
                                 VALUE = 1.0
                                 CALL HVAL(COLUMN,ROW,VALUE)
                                 IF (XYR .EQ. N_XYR) THEN
                                    IRET = DFMCRTP(ROW,'L       ')
                                    COLUMN = HMM_RHS
                                    T_N = MAP_TG(K,1)
                                    T_E = MAP_TG(K,2)
                                    VALUE = MIN(MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
                                            (HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)) + &
                                             HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))))

!                                    WRITE(6,9937) CURIYR+1989,CURITR,T_N,T_E, ROW, MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
!                                       HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))

                                    IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1004) curiyr,curitr,cinx,minx,HMM_RHS,ROW,0,VALUE

                                    CALL HRHS(COLUMN,ROW,VALUE)
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDDO
!
!                       Distribution
!
                        I=3
                        J=4
                        DO K=1,DOHTCH
                           IF (MAP_D(K,I3) .GT. 0) THEN
                              COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODD(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                              ROW = HMM_OBJ
                              VALUE = DOCST(K)
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                              CALL HVAL(COLUMN,ROW,VALUE)
                              IF (I3.EQ.2) THEN
                                 ROW = PCOD(I3) //FCOD(I) // LCOD(J) // TCODD(K) // MCOD(MINX) // "_" // RCOD(CINX)
                                 VALUE = 1.0
                                 CALL HVAL(COLUMN,ROW,VALUE)
                                 IF (XYR .EQ. N_XYR) THEN
                                    IRET = DFMCRTP(ROW,'L       ')
                                    COLUMN = HMM_RHS
                                    T_N = MAP_D(K,1)
                                    T_E = MAP_D(K,2)
                                    VALUE = MIN(MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
                                            (HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)) + &
                                             HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))))

!                                    WRITE(6,9937) CURIYR+1989,CURITR,T_N,T_E, ROW, MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
!                                       HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))

                                    IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1004) curiyr,curitr,cinx,minx,HMM_RHS,ROW,0,VALUE

                                    CALL HRHS(COLUMN,ROW,VALUE)
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDDO
!
!                       Dispensing
!
                        I=4
                        J=4
                        DO K=1,SOHTCH
                           IF (MAP_S(K,I3) .GT. 0) THEN
                              COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODS(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                              ROW = HMM_OBJ
                              VALUE = SOCST(K)
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                              CALL HVAL(COLUMN,ROW,VALUE)
                              IF (I3.EQ.2) THEN
                                 ROW = PCOD(I3) //FCOD(I) // LCOD(J) // TCODS(K) // MCOD(MINX) // "_" // RCOD(CINX)
                                 VALUE = 1.0
                                 CALL HVAL(COLUMN,ROW,VALUE)
                                 IF (XYR .EQ. N_XYR) THEN
                                    IRET = DFMCRTP(ROW,'L       ')
                                    COLUMN = HMM_RHS
                                    T_N = MAP_S(K,1)
                                    T_E = MAP_S(K,2)
                                    VALUE = MIN(MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
                                            (HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)) + &
                                             HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))))

!                                    WRITE(6,9937) CURIYR+1989,CURITR,T_N,T_E, ROW, MAX_HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), &
!                                       HANSWR(CINX,MINX,T_N,MAX(1,CURIYR+PLPD_INV_YR-2)), HANSWR(CINX,MINX,T_E,MAX(1,CURIYR+PLPD_INV_YR-2))

                                    IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1004) curiyr,curitr,cinx,minx,HMM_RHS,ROW,0,VALUE

                                    CALL HRHS(COLUMN,ROW,VALUE)
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDDO

                     ENDIF  ! (I3 .EQ. 2 .OR. XYR .GE. PLPD_INV_YR)

!
!                    Sales to the Grid
!
                     IF (I3.EQ.1) then
                        I=5
                        J=4
                        DO K=1,SGHTCH
                           IF (MAP_GR(K,I3) .GT. 0) THEN
                              COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODGR(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                              ROW = HMM_OBJ
                              VALUE = SGCST * -1.0
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                              CALL HVAL(COLUMN,ROW,VALUE)
                           ENDIF
                        END DO
                     ENDIF

                  END IF ! I3 = 2 or XYR >= PLPD_INV_YR

               ENDDO ! I3
!
!              ROW1 - DEMAND
!
               RWNUM = RWNUM + 1

               ROW = ROWDEF(1) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
               IRET = DFMCRTP(ROW,'G       ')
               COLUMN = HMM_RHS
               VALUE = DMD
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1006) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
               CALL HRHS(COLUMN,ROW,VALUE)

               DO I3=1,2

!                 Set start and end year effected by new or existing build operate vectors

                  IF (I3 .EQ. 1) THEN
                     JYR_SYR = MAX(1 , PLPD_INV_YR)
                     JYR_EYR = MAX(XYR , PLPD_INV_YR)
                  ELSE
                     JYR_SYR = XYR
                     JYR_EYR = N_XYR
                  END IF
!
!                 Production
!
                  I=1
!
!                 Central
!
                  J=1

                  DO K=1,PCHTCH
                     IF (MAP_PC(K,I3) .GT. 0) THEN
                        DO JYR = JYR_SYR , JYR_EYR
                           COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODPC(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                           VALUE = 1
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                           CALL HVAL(COLUMN,ROW,VALUE)
                        ENDDO
                     END IF
                  ENDDO
!
!                 City Gate
!
                  J=2

                  DO K=1,PGHTCH
                     IF (MAP_PG(K,I3) .GT. 0) THEN
                        DO JYR = JYR_SYR , JYR_EYR
                           COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODPG(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                           VALUE = 1
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                           CALL HVAL(COLUMN,ROW,VALUE)
                        ENDDO
                     END IF
                  ENDDO
!
!                 Forecourt
!
                  J=3

                  DO K=1,PFHTCH
                     IF (MAP_PF(K,I3) .GT. 0) THEN
                        DO JYR = JYR_SYR , JYR_EYR
                           COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODPF(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                           VALUE = 1
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                           CALL HVAL(COLUMN,ROW,VALUE)
                        ENDDO
                     END IF
                  ENDDO
!
               ENDDO

               !!!I3=1

!
!              ROW2 - PRODUCTION-TRANSPORTATION
!              ROW3 - PRODUCTION-TRANSPORTATION
!
               I=1  ! FCOD() index

               ! Central = 1, City Gate = 2
               DO J=1,2

                  RWNUM = RWNUM + 1
                  IF (J .EQ. 1) ROW = ROWDEF(2) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                  IF (J .EQ. 2) ROW = ROWDEF(3) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                  IRET = DFMCRTP(ROW,'E       ')
                  COLUMN = HMM_RHS
                  VALUE = 0.0
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1006) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                  CALL HRHS(COLUMN,ROW,VALUE)

                  ! New = 1, Existing = 2
                  DO I3=1,2

!                    Set start and end year effected by new or existing build operate vectors

                     IF (I3 .EQ. 1) THEN
                        JYR_SYR = MAX(1 , PLPD_INV_YR)
                        JYR_EYR = MAX(XYR , PLPD_INV_YR)
                     ELSE
                        JYR_SYR = XYR
                        JYR_EYR = N_XYR
                     END IF

                     ! Production column = 1, Transport column = 2
                     DO I2=1,2
                        IF (J.EQ.1) THEN
!
!                          CENTRAL
!
                           IF (I2.EQ.1) THEN
                              DO K=1,PCHTCH
                                 IF (MAP_PC(K,I3) .GT. 0) THEN
!
!                                   PRODUCTION
!
                                    DO JYR = JYR_SYR , JYR_EYR
                                       COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODPC(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                                       VALUE = -1
                                       IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                       CALL HVAL(COLUMN,ROW,VALUE)
                                    ENDDO
                                 ENDIF
                              ENDDO
                           ENDIF

                           IF (I2.EQ.2) THEN
                              DO K=1,TCHTCH
                                 IF (MAP_TC(K,I3) .GT. 0) THEN
!
!                                   TRANSPORTATION
!
                                    DO JYR = JYR_SYR , JYR_EYR
                                       COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODTC(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                                       VALUE = 1
                                       IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                       CALL HVAL(COLUMN,ROW,VALUE)
                                    ENDDO
                                 END IF
                              ENDDO
                           ENDIF
                        ENDIF    !j=1

                        IF (J.EQ.2) THEN
!
!                          CITYGATE
!
                           IF (I2.EQ.1) THEN
                              DO K=1,PGHTCH
                                 IF (MAP_PG(K,I3) .GT. 0) THEN
!
!                                   PRODUCTION
!
                                    DO JYR = JYR_SYR , JYR_EYR
                                       COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODPG(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                                       VALUE = -1
                                       IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                       CALL HVAL(COLUMN,ROW,VALUE)
                                    ENDDO
                                 END IF
                              ENDDO
                           ENDIF
                           IF (I2.EQ.2) THEN
                              DO K=1,TGHTCH
                                 IF (MAP_TG(K,I3) .GT. 0) THEN
!
!                                   TRANSPORTATION
!
                                    DO JYR = JYR_SYR , JYR_EYR
                                       COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODTG(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                                       VALUE = 1
                                       IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                       CALL HVAL(COLUMN,ROW,VALUE)
                                    ENDDO
                                 END IF
                              ENDDO
                           ENDIF    !I2=2
                        ENDIF    !J=2
                     ENDDO   !I2
                  ENDDO   !I3
               ENDDO   !J
!
               I=2
               DO K=1,3
!
!                 TRANSPORT
!                 TRUCKS COMPRESSED, TRUCKS LIQUID, PIPELINE
!                 ROW4 - TRANSPORT-DISTRIBUTION
!                 ROW5 - TRANSPORT-DISTRIBUTION
!                 ROW6 - TRANSPORT-DISTRIBUTION

                  RWNUM = RWNUM + 1
                  IF (K.EQ.1) ROW = ROWDEF(4) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                  IF (K.EQ.2) ROW = ROWDEF(5) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                  IF (K.EQ.3) ROW = ROWDEF(6) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                  IRET = DFMCRTP(ROW,'E       ')
                  COLUMN = HMM_RHS
                  VALUE = 0.0
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1006) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                  CALL HRHS(COLUMN,ROW,VALUE)

                  DO I3=1,2

!                    Set start and end year effected by new or existing build operate vectors

                     IF (I3 .EQ. 1) THEN
                        JYR_SYR = MAX(1 , PLPD_INV_YR)
                        JYR_EYR = MAX(XYR , PLPD_INV_YR)
                     ELSE
                        JYR_SYR = XYR
                        JYR_EYR = N_XYR
                     END IF

                     DO J=1,2
!
!                       CENTRAL, CITYGATE
!
                        IF (J.EQ.1) TECH = TCODTC(K)
                        IF (J.EQ.2) TECH = TCODTG(K)

                        DO JYR = JYR_SYR , JYR_EYR
                           COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TECH // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                           VALUE = -1
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                           CALL HVAL(COLUMN,ROW,VALUE)
                        END DO

                        IF (K.EQ.1) THEN
!
!                          DISTRIBUTION
!                           TRUCKS COMPRESSED - CONTINUED
!
                           I2=3
                           J2=4
                           K2=2
                        ELSE IF (K.EQ.2) THEN
!
!                          TRUCKS LIQUID - CONTINUED
!
                           I2=3
                           J2=4
                           K2=4
                        ELSE IF (K.EQ.3) THEN
!
!                          PIPELINE
!
                           I2=3
                           J2=4
                           K2=1
                        ENDIF
!
                        DO JYR = JYR_SYR , JYR_EYR
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J2) // TCODD(K2) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                           VALUE = 1
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                           CALL HVAL(COLUMN,ROW,VALUE)
                        END DO
!
                        IF (K.EQ.3) THEN
!
!                          TRUCK LIQUID
!
                           I2=3
                           J2=4
                           K2=3
                           DO JYR = JYR_SYR , JYR_EYR
                              COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J2) // TCODD(K2) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                              VALUE = 1
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                              CALL HVAL(COLUMN,ROW,VALUE)
                           END DO

                           ! Added option to go from pipeline transport
                           !  to gas truck distribution
!
!                          TRUCK GAS
!
                           I2=3
                           J2=4
                           K2=5
                           DO JYR = JYR_SYR , JYR_EYR
                              COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J2) // TCODD(K2) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                              VALUE = 1
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                              CALL HVAL(COLUMN,ROW,VALUE)
                           END DO

                        ENDIF
!
                     ENDDO   !J
                  ENDDO   !I3
               ENDDO   !K
!

               I=3
               DO K2=1,2
!
!                 ROW7 - GASEOUS FORECOURT PRODUCTION+DISTRIBUTION-DISPENSING
!                 ROW8 - LIQUID DISTRIBUTION-DISPENSING
!
                  RWNUM = RWNUM + 1
                  IF (K2.EQ.1) ROW = ROWDEF(7) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                  IF (K2.EQ.2) ROW = ROWDEF(8) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                  IRET = DFMCRTP(ROW,'E       ')
                  COLUMN = HMM_RHS
                  VALUE = 0.0
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1006) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                  CALL HRHS(COLUMN,ROW,VALUE)

                  DO I3=1,2

!                    Set start and end year effected by new or existing build operate vectors

                     IF (I3 .EQ. 1) THEN
                        JYR_SYR = MAX(1 , PLPD_INV_YR)
                        JYR_EYR = MAX(XYR , PLPD_INV_YR)
                     ELSE
                        JYR_SYR = XYR
                        JYR_EYR = N_XYR
                     END IF

                     DO K=1,DOHTCH
                        J=4
                        I2=4
                        J2=4
                        IF (K.EQ.1.OR.K.EQ.2.OR.K.EQ.5) THEN
                           IF (K2.EQ.1) THEN
!
!                             DISTRIBUTION
!
                              DO JYR = JYR_SYR , JYR_EYR
                                 COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODD(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                                 VALUE = -1
                                 IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                 CALL HVAL(COLUMN,ROW,VALUE)
                              END DO
!
!                             DISPENSING-GASIOUS
!
                              DO JYR = JYR_SYR , JYR_EYR
                                 COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J2) // TCODS(K2) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                                 VALUE = 1
                                 IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                 CALL HVAL(COLUMN,ROW,VALUE)
                              END DO
                           ENDIF
                        ELSE IF (K.EQ.3.OR.K.EQ.4) THEN
                           IF (K2.EQ.2) THEN
!
!                             DISTRIBUTION
!
                              DO JYR = JYR_SYR , JYR_EYR
                                 COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODD(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                                 VALUE = -1
                                 IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                 CALL HVAL(COLUMN,ROW,VALUE)
                              END DO
!
!                             DISPENSING-LIQUID
!
                              DO JYR = JYR_SYR , JYR_EYR
                                 COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J2) // TCODS(K2) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                                 VALUE = 1
                                 IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                 CALL HVAL(COLUMN,ROW,VALUE)
                              END DO
                           ENDIF
                        ENDIF
!
                     ENDDO    !K

                     ! Added forecourt gaseous production to the
                     !  gaseous dispensing balance row
                     IF (K2.EQ.1) THEN

                          DO K=1,PFHTCH
                             IF (MAP_PF(K,I3) .GT. 0) THEN
!
!                               FORECOURT PRODUCTION
!
                                DO JYR = JYR_SYR , JYR_EYR
                                   COLUMN = PCOD(I3) // FCOD(1) // LCOD(3) // TCODPF(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                                   VALUE = -1
                                   IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,&
                                        COLUMN,ROW,VALUE
                                   CALL HVAL(COLUMN,ROW,VALUE)
                                ENDDO

!
!                               GASEOUS DISPENSING
!
                                DO JYR = JYR_SYR , JYR_EYR
                                   COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J2) // TCODS(K2) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                                   VALUE = 1
                                   IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                                   CALL HVAL(COLUMN,ROW,VALUE)
                                ENDDO

                             ENDIF
                          ENDDO     !PFHTCH

                     ENDIF  !K2=1

                  ENDDO    !I3
               ENDDO    !K2
!
!
!              ROW9 - Sales to the Grid
!
               RWNUM = RWNUM + 1
               ROW = ROWDEF(9) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
               IRET = DFMCRTP(ROW,'G       ')
               COLUMN = HMM_RHS
               VALUE = 0.0
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1006) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
               CALL HRHS(COLUMN,ROW,VALUE)


               DO I3=1,2

!                 Set start and end year effected by new or existing build operate vectors

                  IF (I3 .EQ. 1) THEN
                     JYR_SYR = MAX(1 , PLPD_INV_YR)
                     JYR_EYR = MAX(XYR , PLPD_INV_YR)
                  ELSE
                     JYR_SYR = XYR
                     JYR_EYR = N_XYR
                  END IF
!
!                 Production
!
                  I=1
!
!                 Central
!
                  J=1
                  DO K=1,PCHTCH
                     IF (MAP_PC(K,I3) .GT. 0) THEN
                        DO JYR = JYR_SYR , JYR_EYR
                           COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODPC(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                           VALUE = PCMWH(K)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                           CALL HVAL(COLUMN,ROW,VALUE)
                        END DO
                     END IF
                  ENDDO
!
!                 City Gate
!
                  J=2
                  DO K=1,PGHTCH
                     IF (MAP_PG(K,I3) .GT. 0) THEN
                        DO JYR = JYR_SYR , JYR_EYR
                           COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODPG(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                           VALUE = PGMWH(K)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                           CALL HVAL(COLUMN,ROW,VALUE)
                        END DO
                     END IF
                  ENDDO
!
!                 Forecourt
!
                  J=3
                  DO K=1,PFHTCH
                     IF (MAP_PF(K,I3) .GT. 0) THEN
                        DO JYR = JYR_SYR , JYR_EYR
                           COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODPF(K) // MCOD(MINX) // PLPDCOD(JYR) // RCOD(CINX)
                           VALUE = PFMWH(K)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                           CALL HVAL(COLUMN,ROW,VALUE)
                        END DO
                     END IF
                  ENDDO
!
                  if (I3.eq. 1) then
                     I=5
                     J=4
                     K=1
                     COLUMN = PCOD(I3) // FCOD(I) // LCOD(J) // TCODGR(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                     VALUE = -1
                     IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1003) curiyr,curitr,cinx,minx,COLUMN,ROW,VALUE
                     CALL HVAL(COLUMN,ROW,VALUE)
                  endif

               ENDDO    !I3

            ENDDO    !XYR
         ENDDO ! MINX
      ENDDO ! CINX


1003  FORMAT(1X,"LP_INPUT-hval",":",4(I4,":"),A8,":",A8,":",E20.6E3)
1004  FORMAT(1X,"LP_INPUT-b4",":",4(I4,":"),A8,":",A8,2(":",E20.6E3))
1006  FORMAT(1X,"LP_INPUT-rhs",":",4(I4,":"),A8,":",A8,":",E20.6E3)


!     CALL SUBROUTINE TO CREATE BIOMASS SUPPLY CURVES

      CALL H2_RFS

      RETURN
      END SUBROUTINE REVISE_HMM


      SUBROUTINE RETRIEVE_HMM
! This subroutine retrieves HMM row and column solution information

      IMPLICIT NONE


      COMMON /WD_OOPS/ WD_BTUS_PWR, WD_BTUS_H2
      REAL*8 WD_BTUS_PWR(NDREG), WD_BTUS_H2(NDREG), P_87

      INTEGER*4 IRET,I,J,K,I2,K2,I3,RWNUM,CINX,MINX,TPROD,T_N,T_E,TECH,I_SUPt
      REAL*8 CRF,PWF,PVV,PVBV,CRF2
      REAL*8 ROW_INFO(5),COL_INFO(5),HTMP1(2),HTMP2(2),T1,T2,T3,T4,T5,T6,T7,HCST,HPRD(MNUMCR,HMKT+1),PQSUM
      REAL*8 MargPrc(MNUMCR,HMKT)
      character*16 COLUMN,ROW
      CHARACTER*2 STATUS
!
      character*16    HMM_BND                       ! BOUND ROW NAME
      character*16    HMM_OBJ                       ! OBJECTIVE FUNCTION NAME
      character*16    HMM_RHS                       ! RIGHT HAND SIDE NAME
!
! REMEMBER TO CHANGE THESE IF NUMBER OF TECHS CHANGE
!
!      CHARACTER*1 FCOD(5)
!      CHARACTER*1 LCOD(4)
!      CHARACTER*2 TCOD(PCHTCH)
!      CHARACTER*2 TCODPC(PCHTCH)
!      CHARACTER*2 TCODPG(PGHTCH)
!      CHARACTER*2 TCODPF(PFHTCH)
!      CHARACTER*2 TCODTC(TCHTCH)
!      CHARACTER*2 TCODTG(TGHTCH)
!      CHARACTER*2 TCODD(DOHTCH)
!      CHARACTER*2 TCODS(SOHTCH)
!      CHARACTER*2 TCODGR(SGHTCH)
      CHARACTER*1 PCOD(2)
!      CHARACTER*2 RWNUMC(30)
!      CHARACTER*3 ROWDEF(9)
!      CHARACTER*1 MCOD(HMKT)
!      CHARACTER*1 PLPDCOD(3)
!      CHARACTER*1 RCOD(MNUMCR-2)
!
      LOGICAL       NEW/.FALSE./
      CHARACTER*18  FILENM
      INTEGER*2     FILE_MGR
      EXTERNAL      FILE_MGR
!
!      DATA FCOD/"P","T","D","F","G"/
!      DATA LCOD/"C","G","F","X"/
!      DATA TCOD/"1","2","3","4","5","6","7",
!                "8","9","10","11","12","13","14" /
!      DATA TCODPC/"SL","S1","SQ","S2","IG","I3","IQ","I4",  &
!                  "BG","B5","GD","G6","NU","N7"/
!      DATA TCODPG/"SM","S3","XX","X2"/
!      DATA TCODPF/"GD","G1","SS","S4","XX","X3"/
!      DATA TCODTC/"CT","LT","PL"/
!      DATA TCODTG/"CT","LT","PL"/
!      DATA TCODD/"PL","CT","LT","LC"/
!      DATA TCODS/"GX","LX","X9"/
!      DATA TCODGR/"GR"/
      DATA PCOD/"N","E"/
!      DATA MCOD/"L","S","R"/
!      DATA PLPDCOD/"1","2","3"/
!      DATA RCOD/"1","2","3","4","5","6","7","8","9"/
!      DATA RWNUMC/"01","02","03","04","05","06","07","08","09","10", &
!                  "11","12","13","14","15","16","17","18","19","20", &
!                  "21","22","23","24","25","26","27","28","29","30"/
!      DATA ROWDEF/"DEM","CPT","CGT","TCD","TLD","PLD","GXR","LXR","GRD"/
!
 1001 FORMAT(1X,"FORDB_INV",":",6(I4,":"),A8,4(":",E20.6E3))
 2001 FORMAT(1X,"FORDB_PRC",":",6(I4,":"),A8,6(":",E20.6E3))
 5001 FORMAT(1X,"FORDB_TST",":",6(I4,":"),A8,7(":",E20.6E3))
 1044 FORMAT(2(I4,1X),F8.3,1X,5(F8.3))

      write(HF_HMMO,*)'TT2NDX dbg1:',TT2NDX
      TEMP_HANSWR = 0.0
      HANSW2  = 0.0
      MargPrc = 0.0
      HPRD    = 0.0

!     RETRIEVE BIOMASS CONSUMPTION BY COAL DEMAND REGION (TRILLS)

      WD_BTUS_H2 = 0.0
      BM_CONS    = 0.0
      BM_TOTAL   = 0.0
      QBMH2CL(0,0,CURIYR) = 0.0
      PBMH2CL(0,0,CURIYR) = 0.0

      DO I_SUPt = 1 , MNUMFS
         QBMH2CL(I_SUPt,0,CURIYR) = 0.0
         IF (WDSUP_AVL(I_SUPt) .EQ. 1 .AND. MP_BM_H2(I_SUPt) .EQ. 1) THEN
            PBMH2CL(I_SUPt,0,CURIYR) = 0.0
         ELSE
            PBMH2CL(I_SUPt,0,CURIYR) = 9.9
         END IF
      END DO

      DO CRG = 1 , NDREG
         CINX = MAP_CR_TO_CL(CRG)

         QBMH2CL(0,CRG,CURIYR) = 0.0
         PBMH2CL(0,CRG,CURIYR) = 0.0

         DO I_SUPt = 1 , MNUMFS
            IF (WDSUP_AVL(I_SUPt) .EQ. 1 .AND. MP_BM_H2(I_SUPt) .EQ. 1) THEN

               COLUMN = 'T' // BM_TYP_CD(I_SUPt) // PLPDCOD(1) // FSTEP(CRG) // FSTEP(CINX)

               IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)

               QBMH2CL(I_SUPt,CRG,CURIYR) = COL_INFO(1)
               QBMH2CL(0,CRG,CURIYR) = QBMH2CL(0,CRG,CURIYR) + COL_INFO(1)
               QBMH2CL(I_SUPt,0,CURIYR) = QBMH2CL(I_SUPt,0,CURIYR) + COL_INFO(1)
               QBMH2CL(0,0,CURIYR) = QBMH2CL(0,0,CURIYR) + COL_INFO(1)

               WD_BTUS_H2(CRG)   = WD_BTUS_H2(CRG) + COL_INFO(1)
               BM_CONS(CINX,CRG) = COL_INFO(1)                       ! Record how much biomass this census division used from each coal region
               BM_TOTAL(CINX)    = BM_TOTAL(CINX) + COL_INFO(1)      ! Record how much biomass this census division used in total

               ROW = 'F' // BM_TYP_CD(I_SUPt) // 'CL' // PLPDCOD(1) // FSTEP(CRG)

               IRET = WFSROW(ROW,'ASLUP   ',STATUS,ROW_INFO)

               P_87 = ABS(ROW_INFO(5)) / MC_JPGDP(CURIYR)
               PBMH2CL(I_SUPt,CRG,CURIYR) = P_87
               PBMH2CL(0,CRG,CURIYR) = PBMH2CL(0,CRG,CURIYR) + COL_INFO(1) * P_87
               PBMH2CL(I_SUPt,0,CURIYR) = PBMH2CL(I_SUPt,0,CURIYR) + COL_INFO(1) * P_87
               PBMH2CL(0,0,CURIYR) = PBMH2CL(0,0,CURIYR) + COL_INFO(1) * P_87

               WRITE(HF_HMMO,3713) CURIRUN,CURIYR+1989,CURITR,CRG,I_SUPt,IRET,COLUMN,COL_INFO(1),WD_BTUS_H2(CRG), &
                  QBMH2CL(I_SUPt,CRG,CURIYR), PBMH2CL(I_SUPt,CRG,CURIYR), MC_JPGDP(CURIYR)
 3713          FORMAT(1X,"BIOMASS_CONSUMPTION",6(":",I4),":",A8,5(":",F12.3))

            END IF
         END DO
      END DO

      IF (QBMH2CL(0,0,CURIYR) .GT. 0.0) THEN
         PBMH2CL(0,0,CURIYR) = PBMH2CL(0,0,CURIYR) / QBMH2CL(0,0,CURIYR)
      ELSE
         PBMH2CL(0,0,CURIYR) = 9.9
      END IF

      DO CRG = 1 , NDREG
         IF (QBMH2CL(0,CRG,CURIYR) .GT. 0.0) THEN
            PBMH2CL(0,CRG,CURIYR) = PBMH2CL(0,CRG,CURIYR) / QBMH2CL(0,CRG,CURIYR)
         ELSE
            PBMH2CL(0,CRG,CURIYR) = PBMH2CL(0,0,CURIYR)
         END IF
      END DO

      DO I_SUPt = 1 , MNUMFS
         IF (WDSUP_AVL(I_SUPt) .EQ. 1 .AND. MP_BM_H2(I_SUPt) .EQ. 1) THEN
            IF (QBMH2CL(I_SUPt,0,CURIYR) .GT. 0.0) THEN
               PBMH2CL(I_SUPt,0,CURIYR) = PBMH2CL(0,0,CURIYR) / QBMH2CL(0,0,CURIYR)
            ELSE
               PBMH2CL(I_SUPt,0,CURIYR) = PBMH2CL(0,0,CURIYR)
            END IF
         END IF
      END DO

      ! Calculate the share biomass consumption from each coal region within a census
      BM_SHARE = 0.0
      DO CINX = 1 , MNUMCR - 2
         DO CRG = 1 , NDREG
             IF (MAP_CR_TO_CL(CRG) .EQ. CINX) THEN
                IF (BM_TOTAL(CINX).GT.0.0) THEN
                   BM_SHARE(CRG) = BM_CONS(CINX,CRG)/BM_TOTAL(CINX)
                ELSE
                   BM_SHARE(CRG) = 0.0
                ENDIF
             ENDIF
         ENDDO
      ENDDO

      DO CINX = 1 , MNUMCR - 2      ! census regions

         DO MINX = 1, HMKT              ! markets

            HCST=0.0
!
!           Get and Store Investment Decisions
!
            DO XYR = 1 , N_XYR      ! planning periods

               DO I3 = 1 , 2            ! new, existing

                  ! Do not carry forward new-build investment decisions that are not in
                  ! the investment planning period (which is defined in the control
                  ! input file)
                  IF ((I3 .EQ. 1 .AND. XYR .EQ. PLPD_INV_YR) .OR. &
                      (I3 .EQ. 2 .AND. XYR .GE. PLPD_INV_YR)) THEN

                     INFL = 0.0
                     KG = 0.0
                     DO JYR = 1 , PLPD_EYR(XYR) - PLPD_SYR(XYR) + 1
                        FYR = JYR + PLPD_SYR(XYR) + CURIYR - 2
                        M_FYR = MIN(FYR , LASTYR)
                        INFL(JYR) = HMM_GNPD(FYR)
                        KG(JYR) = 1.0
                     END DO
                     FYR = PLPD_EYR(XYR) - PLPD_SYR(XYR) + 1
                     PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                     PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
!
!                    Production
!
                     I2=1
!
!                    Central
!
                     J=1
                     DO K=1,PCHTCH
                        K2 = MAP_PC(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODPC(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           TEMP_HANSWR(CINX,MINX,K2) = TEMP_HANSWR(CINX,MINX,K2) + COL_INFO(1)

                           WRITE(HF_HMMO,9876) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,&
                                (COL_INFO(I),I=1,5),PV_INFL,PV_KG
9876                       FORMAT(1X,"MKTSHR",":",6(I4,":"),A8,7(":",F16.6))
                        END IF
                     ENDDO
!
!                    City Gate
!
                     J=2
                     DO K=1,PGHTCH
                        K2 = MAP_PG(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODPG(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           TEMP_HANSWR(CINX,MINX,K2) = TEMP_HANSWR(CINX,MINX,K2) + COL_INFO(1)
                        END IF
                     ENDDO
!
!                    Forecourt
!
                     J=3
                     DO K=1,PFHTCH
                        K2 = MAP_PF(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODPF(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
!                           write(HF_HMMO,*)'K2 in PFHTCH loop:',curiyr,cinx,minx,K2
                           TEMP_HANSWR(CINX,MINX,K2) = TEMP_HANSWR(CINX,MINX,K2) + COL_INFO(1)
                        END IF
                     ENDDO
!
!                    Transport
!
                     I2=2
!
!                    Central
!
                     J=1
                     DO K=1,TCHTCH
                        K2 = MAP_TC(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODTC(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           TEMP_HANSWR(CINX,MINX,K2) = TEMP_HANSWR(CINX,MINX,K2) + COL_INFO(1)
                        END IF
                     ENDDO
!
!                    City Gate
!
                     J=2
                     DO K=1,TGHTCH
                        K2 = MAP_TG(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODTG(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           TEMP_HANSWR(CINX,MINX,K2) = TEMP_HANSWR(CINX,MINX,K2) + COL_INFO(1)
                        END IF
                     ENDDO
!
!                    Distribution
!
                     I2=3
                     J=4
                     DO K=1,DOHTCH
                        K2 = MAP_D(K,I3)
                        IF (K2 .GT. 0) THEN
!                           write(HF_HMMO,*)'K2 in DOHTCH loop:',K2
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODD(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           TEMP_HANSWR(CINX,MINX,K2) = TEMP_HANSWR(CINX,MINX,K2) + COL_INFO(1)
                        END IF
                     ENDDO
!
!                    Dispensing
!
                     I2=4
                     J=4
                     DO K=1,SOHTCH
                        K2 = MAP_S(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODS(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
!                           write(HF_HMMO,*)'K2 in SOHTCH loop:',curiyr,cinx,minx,K2
                           TEMP_HANSWR(CINX,MINX,K2) = TEMP_HANSWR(CINX,MINX,K2) + COL_INFO(1)
                        END IF
                     ENDDO
!
!                    Sales to Grid
!
                     I2=5
                     J=4
                     DO K=1,SGHTCH
                        K2 = MAP_GR(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODGR(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
!                           write(HF_HMMO,*)'K2 in SGHTCH loop:',curiyr,cinx,minx,K2
                           TEMP_HANSWR(CINX,MINX,K2) = TEMP_HANSWR(CINX,MINX,K2) + COL_INFO(1)

                           WRITE(HF_HMMO,9876) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,&
                                (COL_INFO(I),I=1,5),PV_INFL,PV_KG
                        END IF
                     ENDDO

                  END IF    ! (I3 .EQ. 1 .AND. XYR .EQ. PLPD_INV_YR) .OR. &
                            ! (I3 .EQ. 2 .AND. XYR .GE. PLPD_INV_YR)

               ENDDO    ! I3

            END DO  ! XYR

!
!           Get Pricing and Consumption Data
!
            HTMP1 = 0.0
            HTMP2 = 0.0
            HANSW2 = 0

            DO XYR = 1 , N_XYR      ! planning periods

               DO I3 = 1 , 2            ! new, existing

                  IF ((I3 .EQ. 1 .AND. PLPD_PRC_YR .GE. PLPD_INV_YR .AND. PLPD_PRC_YR .EQ. XYR) &
                        .OR. (I3 .EQ. 2 .AND. PLPD_PRC_YR .LE. XYR)) THEN

!                    Calculate Inflation Factor to convert from Nominal Present Value to
!                    Real 87$ and Calculate Net present value KG Represented by Vector
!                    For New Vecotrs from XYR through end of Forecast Horizon
!                    For Existing Vectors from beginning of Forecast Horizon through XYR period

                     INFL = 0.0
                     KG = 0.0

                     IF (I3 .EQ. 1) THEN
                        DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                           FYR = JYR + PLPD_SYR(XYR) + CURIYR - 2
                           INFL(JYR) = HMM_GNPD(FYR)
                           KG(JYR) = 1.0
                        END DO
                        FYR = N_FYR - PLPD_SYR(XYR) + 1
                        PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                        PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                     ELSE
                        DO JYR = 1 , PLPD_EYR(XYR)
                           FYR = JYR + CURIYR - 1
                           INFL(JYR) = HMM_GNPD(FYR)
                           KG(JYR) = 1.0
                        END DO
                        FYR = PLPD_EYR(XYR)
                        PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                        PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                     END IF

!
!                    Production
!
                     I2=1
!
!                    Central
!
                     J=1
                     DO K=1,PCHTCH
                        K2 = MAP_PC(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODPC(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) WRITE(HF_FORDB,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG, &
                                 INSTALL_COST(CURIYR,CINX,MINX,K2),ANNUITY_COST(CURIYR,CINX,MINX,K2)

                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) NEW_BUILDS(CURIYR,CINX,MINX,K2) = COL_INFO(1)

                           ! nak test 03/23/07
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1 .AND. I3 .EQ. 1) WRITE(HF_HMMO,5001) CURIYR+1989, &
                                CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,5),PV_INFL,PV_KG

                           HANSW2(1,K2) = HANSW2(1,K2) + COL_INFO(1)
                           HANSW2(2,K2) = COL_INFO(2) / PV_INFL
                           HANSW2(3,K2) = COL_INFO(5) / PV_INFL         ! Reduced costs
                           HTMP1(I3) = HTMP1(I3) + (COL_INFO(1) * COL_INFO(2))
                           HTMP2(I3) = HTMP2(I3) + COL_INFO(1)
                        END IF
                     ENDDO
!
!                    City Gate
!
                     J=2
                     DO K=1,PGHTCH
                        K2 = MAP_PG(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODPG(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) WRITE(HF_FORDB,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG, &
                                 INSTALL_COST(CURIYR,CINX,MINX,K2),ANNUITY_COST(CURIYR,CINX,MINX,K2)

                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) NEW_BUILDS(CURIYR,CINX,MINX,K2) = COL_INFO(1)

                           ! nak test 03/23/07
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1 .AND. I3 .EQ. 1) WRITE(HF_HMMO,5001) CURIYR+1989, &
                                CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,5),PV_INFL,PV_KG

                           HANSW2(1,K2) = HANSW2(1,K2) + COL_INFO(1)
                           HANSW2(2,K2) = COL_INFO(2) / PV_INFL
                           HANSW2(3,K2) = COL_INFO(5) / PV_INFL         ! Reduced costs
                           HTMP1(I3) = HTMP1(I3) + (COL_INFO(1) * COL_INFO(2))
                           HTMP2(I3) = HTMP2(I3) + COL_INFO(1)
                        END IF
                     ENDDO
!
!                    Forecourt
!
                     J=3
                     DO K=1,PFHTCH
                        K2 = MAP_PF(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODPF(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) WRITE(HF_FORDB,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG, &
                                 INSTALL_COST(CURIYR,CINX,MINX,K2),ANNUITY_COST(CURIYR,CINX,MINX,K2)

                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) NEW_BUILDS(CURIYR,CINX,MINX,K2) = COL_INFO(1)

                           ! nak test 03/23/07
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1 .AND. I3 .EQ. 1) WRITE(HF_HMMO,5001) CURIYR+1989, &
                                CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,5),PV_INFL,PV_KG

!                           write(HF_HMMO,*)'K2 in PFHTCH loop:',curiyr,cinx,minx,K2
                           HANSW2(1,K2) = HANSW2(1,K2) + COL_INFO(1)
                           HANSW2(2,K2) = COL_INFO(2) / PV_INFL
                           HANSW2(3,K2) = COL_INFO(5) / PV_INFL         ! Reduced costs
                           HTMP1(I3) = HTMP1(I3) + (COL_INFO(1) * COL_INFO(2))
                           HTMP2(I3) = HTMP2(I3) + COL_INFO(1)
                        END IF
                     ENDDO
!
!                    Transport
!
                     I2=2
!
!                    Central
!
                     J=1
                     DO K=1,TCHTCH
                        K2 = MAP_TC(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODTC(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) WRITE(HF_FORDB,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG, &
                                 INSTALL_COST(CURIYR,CINX,MINX,K2),ANNUITY_COST(CURIYR,CINX,MINX,K2)

                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) NEW_BUILDS(CURIYR,CINX,MINX,K2) = COL_INFO(1)

                           HANSW2(1,K2) = HANSW2(1,K2) + COL_INFO(1)
                           HANSW2(2,K2) = COL_INFO(2) / PV_INFL
                           HTMP1(I3) = HTMP1(I3) + (COL_INFO(1) * COL_INFO(2))
                        END IF
                     ENDDO
!
!                    City Gate
!
                     J=2
                     DO K=1,TGHTCH
                        K2 = MAP_TG(K,I3)
                     IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODTG(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) WRITE(HF_FORDB,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG, &
                                 INSTALL_COST(CURIYR,CINX,MINX,K2),ANNUITY_COST(CURIYR,CINX,MINX,K2)

                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) NEW_BUILDS(CURIYR,CINX,MINX,K2) = COL_INFO(1)

                           HANSW2(1,K2) = HANSW2(1,K2) + COL_INFO(1)
                           HANSW2(2,K2) = COL_INFO(2) / PV_INFL
                           HTMP1(I3) = HTMP1(I3) + (COL_INFO(1) * COL_INFO(2))
                        END IF
                     ENDDO
!
!                    Distribution
!
                     I2=3
                     J=4
                     DO K=1,DOHTCH
                        K2 = MAP_D(K,I3)
                        IF (K2 .GT. 0) THEN
!                           write(HF_HMMO,*)'K2 in DOHTCH loop:',K2
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODD(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) WRITE(HF_FORDB,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG, &
                                 INSTALL_COST(CURIYR,CINX,MINX,K2),ANNUITY_COST(CURIYR,CINX,MINX,K2)

                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) NEW_BUILDS(CURIYR,CINX,MINX,K2) = COL_INFO(1)

                           HANSW2(1,K2) = HANSW2(1,K2) + COL_INFO(1)
                           HANSW2(2,K2) = COL_INFO(2) / PV_INFL
                           HTMP1(I3) = HTMP1(I3) + (COL_INFO(1) * COL_INFO(2))
                        END IF
                     ENDDO
!
!                    Dispensing
!
                     I2=4
                     J=4
                     DO K=1,SOHTCH
                        K2 = MAP_S(K,I3)
                        IF (K2 .GT. 0) THEN
                           COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODS(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                           IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) WRITE(HF_FORDB,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG, &
                                 INSTALL_COST(CURIYR,CINX,MINX,K2),ANNUITY_COST(CURIYR,CINX,MINX,K2)

                           IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) NEW_BUILDS(CURIYR,CINX,MINX,K2) = COL_INFO(1)

!                           write(HF_HMMO,*)'K2 in SOHTCH loop:',curiyr,cinx,minx,K2
                           HANSW2(1,K2) = HANSW2(1,K2) + COL_INFO(1)
                           HANSW2(2,K2) = COL_INFO(2) / PV_INFL
                           HTMP1(I3) = HTMP1(I3) + (COL_INFO(1) * COL_INFO(2))
                        END IF
                     ENDDO

                  ENDIF ! ((I3 .EQ. 1 .AND. PLPD_PRC_YR .GE. PLPD_INV_YR .AND. PLPD_PRC_YR .EQ. XYR)
                        !  .OR. (I3 .EQ. 2 .AND. PLPD_PRC_YR .LE. XYR))

               ENDDO    !I3

               ! Calculate Inflation Factor to convert from Nominal Present Value to Real 87$
               !  and Calculate Net present value KG Represented by Vector - XYR Period Only
               IF (PLPD_PRC_YR .EQ. XYR) THEN

                  INFL = 0.0
                  KG = 0.0

                  DO JYR = 1 , PLPD_EYR(XYR) - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + CURIYR - 2
                     INFL(JYR) = HMM_GNPD(FYR)
                     KG(JYR) = 1.0
                  END DO
                  FYR = PLPD_EYR(XYR) - PLPD_SYR(XYR) + 1

                  PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
!
!                 Sales to Grid
!
                  I2 = 5
                  J = 4
                  I3 = 1
                  DO K=1,SGHTCH
                     K2 = MAP_GR(K,I3)
                     IF (K2 .GT. 0) THEN
                        COLUMN = PCOD(I3) // FCOD(I2) // LCOD(J) // TCODGR(K) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                        IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2001) CURIYR+1989,CURITR,CINX,MINX,K2, &
                            XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG
                        IF (FCRL .EQ. 1 .AND. I3 .EQ. 1) WRITE(HF_FORDB,2001) CURIYR+1989,CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,2),PV_INFL,PV_KG

                           ! nak test 03/23/07
                           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1 .AND. I3 .EQ. 1) WRITE(HF_HMMO,5001) CURIYR+1989, &
                                CURITR,CINX,MINX,K2,XYR,COLUMN,(COL_INFO(I),I=1,5),PV_INFL,PV_KG

!                        write(HF_HMMO,*)'K2 in SGHTCH loop:',curiyr,cinx,minx,K2
                           HANSW2(1,K2) = HANSW2(1,K2) + COL_INFO(1)
                           HANSW2(2,K2) = COL_INFO(2) / PV_INFL
                           HTMP1(I3) = HTMP1(I3) + (COL_INFO(1) * COL_INFO(2))
                     END IF
                  ENDDO

                  J = 1
                  ROW = ROWDEF(J) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
                  IRET = WFSROW(ROW,'ASLUP   ',STATUS,ROW_INFO)
                  MargPrc(CINX,MINX) = (-1.0) * ( ((ROW_INFO(5)-FEDTAX)/PV_INFL) - &        ! Add federal taxes in nominal dollars
                                                 (STATETAX(CINX)/ HMM_GNPD(2005-1989)) )    ! Add state taxes converted from 2005$ to 1987$ dollars

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,1135) CURIYR+1989,CURITR,XYR,CINX,MINX, &
                        STATETAX(CINX),FEDTAX,ROW_INFO(5)/PV_INFL,MargPrc(CINX,MINX),HMM_GNPD(2005-1989)
 1135             FORMAT(1X,"TAXdbg",5(":",I4),5(":",F10.3))

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1002) CURIYR,CINX,MINX,IRET,ROW,STATUS, &
                        (ROW_INFO(I),I=1,5),PV_INFL
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1005) CURIYR,CINX,MINX,XYR, &
                        MargPrc(cinx,minx),PV_INFL,HMM_GNPD(FYR)
 1002             FORMAT(1X,"ROW_INFO",4(":",I3),":",A8,":",A2,6(":",E20.6E3))
 1005             FORMAT(1X,"MARG_PRC",4(":",I3),3(":",E20.6E3))

               ENDIF   ! PLPD_PRC_YE .EQ. XYR

               ! Look at marginal prices of future planning periods
               INFL = 0.0
               KG   = 0.0
               DO JYR = 1 , PLPD_EYR(XYR) - PLPD_SYR(XYR) + 1
                  FYR = JYR + PLPD_SYR(XYR) + CURIYR - 2
                  INFL(JYR) = HMM_GNPD(FYR)
                  KG(JYR) = 1.0
               END DO
               FYR     = PLPD_EYR(XYR) - PLPD_SYR(XYR) + 1
               PV_KG   = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
               PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
               ROW     = ROWDEF(1) // MCOD(MINX) // PLPDCOD(XYR) // RCOD(CINX)
               IRET    = WFSROW(ROW,'ASLUP   ',STATUS,ROW_INFO)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,1005) CURIYR,CINX,MINX,XYR, &
                     ((-1.0)*ROW_INFO(5)/PV_INFL),PV_INFL,HMM_GNPD(FYR)

            END DO  ! XYR

            ! Call the market sharing algorithm
            CALL MARKET_SHARE(CINX,MINX)

            XYR = PLPD_PRC_YR
            DO I3 = 1 ,2
               K2 = MAP_AC(I3)
               TEMP_HANSWR(CINX,MINX,K2) = HTMP1(I3)

               IF (HTMP2(I3) .GT. 0.0) THEN
                  HANSW2(2,K2) = HTMP1(I3) / HTMP2(I3)
               ELSE
                  HANSW2(2,K2) = 0.0
               ENDIF
            END DO

            FYR = CURIYR + PLPD_SYR(XYR) - 1
            IF (FYR .LE. LASTYR) THEN
               M_FYR = MIN(FYR , LASTYR)
!
!              Production
!
               I2=1
!
!              Central
!
               J=1
               DO K=1,PCHTCH
                  T_N = MAP_PC(K,1)
                  T_E = MAP_PC(K,2)
                  IF (T_N .GT. 0 .AND. T_E .GT. 0) THEN
                     IF (HANSW2(1,T_N) + HANSW2(1,T_E) .GT. 0.0) THEN

                        PHMM(M_FYR,CINX,MINX,T_N) = ((HANSW2(1,T_N) * HANSW2(2,T_N)) + &
                            (HANSW2(1,T_E) * HANSW2(2,T_E))) / (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HMMPRD(M_FYR,CINX,MINX,T_N)= (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HPRD(CINX,MINX) = HPRD(CINX,MINX) + HMMPRD(M_FYR,CINX,MINX,T_N)
                        HCST = HCST + HMMPRD(M_FYR,CINX,MINX,T_N) * PHMM(M_FYR,CINX,MINX,T_N)

                        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,7333) Curiyr+1989,M_FYR+1989,curitr, &
                            cinx,minx,T_N,FCOD(I2),LCOD(J),TCODPC(K), &
                            PHMM(M_FYR,CINX,MINX,T_N),HMMPRD(M_FYR,CINX,MINX,T_N)
7333                    FORMAT(1X,"PHMMrsc",6(":",I4),2(":",A1),":",A2,2(":",F12.6))

                     END IF
                  END IF
               ENDDO
!
!              City Gate
!
               J=2
               DO K=1,PGHTCH
                  T_N = MAP_PG(K,1)
                  T_E = MAP_PG(K,2)
                  IF (T_N .GT. 0 .AND. T_E .GT. 0) THEN
                     IF (HANSW2(1,T_N) + HANSW2(1,T_E) .GT. 0.0) THEN

                        PHMM(M_FYR,CINX,MINX,T_N) = ((HANSW2(1,T_N) * HANSW2(2,T_N)) + &
                            (HANSW2(1,T_E) * HANSW2(2,T_E))) / (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HMMPRD(M_FYR,CINX,MINX,T_N)= (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HPRD(CINX,MINX) = HPRD(CINX,MINX) + HMMPRD(M_FYR,CINX,MINX,T_N)
                        HCST = HCST + HMMPRD(M_FYR,CINX,MINX,T_N) * PHMM(M_FYR,CINX,MINX,T_N)

                        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,7333) Curiyr+1989,M_FYR+1989,curitr, &
                            cinx,minx,T_N,FCOD(I2),LCOD(J),TCODPG(K), &
                            PHMM(M_FYR,CINX,MINX,T_N),HMMPRD(M_FYR,CINX,MINX,T_N)
                     END IF
                  END IF
               ENDDO
!
!              Forecourt
!
               J=3
               DO K=1,PFHTCH
                  T_N = MAP_PF(K,1)
                  T_E = MAP_PF(K,2)
                  IF (T_N .GT. 0 .AND. T_E .GT. 0) THEN
                     IF (HANSW2(1,T_N) + HANSW2(1,T_E) .GT. 0.0) THEN

                        PHMM(M_FYR,CINX,MINX,T_N) = ((HANSW2(1,T_N) * HANSW2(2,T_N)) + &
                            (HANSW2(1,T_E) * HANSW2(2,T_E))) / (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HMMPRD(M_FYR,CINX,MINX,T_N)= (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HPRD(CINX,MINX) = HPRD(CINX,MINX) + HMMPRD(M_FYR,CINX,MINX,T_N)
                        HCST = HCST + HMMPRD(M_FYR,CINX,MINX,T_N) * PHMM(M_FYR,CINX,MINX,T_N)

                        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,7333) Curiyr+1989,M_FYR+1989,curitr, &
                            cinx,minx,T_N,FCOD(I2),LCOD(J),TCODPF(K), &
                            PHMM(M_FYR,CINX,MINX,T_N),HMMPRD(M_FYR,CINX,MINX,T_N)
                     END IF
                  END IF
               ENDDO
!
!              Transport
!
               I2=2
!
!              Central
!
               J=1
               DO K=1,TCHTCH
                  T_N = MAP_TC(K,1)
                  T_E = MAP_TC(K,2)
                  IF (T_N .GT. 0 .AND. T_E .GT. 0) THEN
                     IF (HANSW2(1,T_N) + HANSW2(1,T_E) .GT. 0.0) THEN

                        PHMM(M_FYR,CINX,MINX,T_N) = ((HANSW2(1,T_N) * HANSW2(2,T_N)) + &
                            (HANSW2(1,T_E) * HANSW2(2,T_E))) / (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HMMPRD(M_FYR,CINX,MINX,T_N)= (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HCST = HCST + HMMPRD(M_FYR,CINX,MINX,T_N) * PHMM(M_FYR,CINX,MINX,T_N)

                        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,7333) Curiyr+1989,M_FYR+1989,curitr, &
                            cinx,minx,T_N,FCOD(I2),LCOD(J),TCODTC(K), &
                            PHMM(M_FYR,CINX,MINX,T_N),HMMPRD(M_FYR,CINX,MINX,T_N)
                     END IF
                  END IF
               ENDDO
!
!              City Gate
!
               J=2
               DO K=1,TGHTCH
                  T_N = MAP_TG(K,1)
                  T_E = MAP_TG(K,2)
                  IF (T_N .GT. 0 .AND. T_E .GT. 0) THEN
                     IF (HANSW2(1,T_N) + HANSW2(1,T_E) .GT. 0.0) THEN

                        PHMM(M_FYR,CINX,MINX,T_N) = ((HANSW2(1,T_N) * HANSW2(2,T_N)) + &
                            (HANSW2(1,T_E) * HANSW2(2,T_E))) / (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HMMPRD(M_FYR,CINX,MINX,T_N)= (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HCST = HCST + HMMPRD(M_FYR,CINX,MINX,T_N) * PHMM(M_FYR,CINX,MINX,T_N)

                        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,7333) Curiyr+1989,M_FYR+1989,curitr, &
                            cinx,minx,T_N,FCOD(I2),LCOD(J),TCODTG(K), &
                            PHMM(M_FYR,CINX,MINX,T_N),HMMPRD(M_FYR,CINX,MINX,T_N)
                     END IF
                  END IF
               ENDDO
!
!              Distribution
!
               I2=3
               J=4
               DO K=1,DOHTCH
                  T_N = MAP_D(K,1)
                  T_E = MAP_D(K,2)
                  IF (T_N .GT. 0 .AND. T_E .GT. 0) THEN
                     IF (HANSW2(1,T_N) + HANSW2(1,T_E) .GT. 0.0) THEN

                        PHMM(M_FYR,CINX,MINX,T_N) = ((HANSW2(1,T_N) * HANSW2(2,T_N)) + &
                            (HANSW2(1,T_E) * HANSW2(2,T_E))) / (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HMMPRD(M_FYR,CINX,MINX,T_N)= (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HCST = HCST + HMMPRD(M_FYR,CINX,MINX,T_N) * PHMM(M_FYR,CINX,MINX,T_N)

                        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,7333) Curiyr+1989,M_FYR+1989,curitr, &
                            cinx,minx,T_N,FCOD(I2),LCOD(J),TCODD(K), &
                            PHMM(M_FYR,CINX,MINX,T_N),HMMPRD(M_FYR,CINX,MINX,T_N)
                     END IF
                  END IF
               ENDDO
!
!              Dispensing
!
               I2=4
               J=4
               DO K=1,SOHTCH
                  T_N = MAP_S(K,1)
                  T_E = MAP_S(K,2)
                  IF (T_N .GT. 0 .AND. T_E .GT. 0) THEN
                     IF (HANSW2(1,T_N) + HANSW2(1,T_E) .GT. 0.0) THEN

                        PHMM(M_FYR,CINX,MINX,T_N) = ((HANSW2(1,T_N) * HANSW2(2,T_N)) + &
                            (HANSW2(1,T_E) * HANSW2(2,T_E))) / (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HMMPRD(M_FYR,CINX,MINX,T_N)= (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HCST = HCST + HMMPRD(M_FYR,CINX,MINX,T_N) * PHMM(M_FYR,CINX,MINX,T_N)

                        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,7333) Curiyr+1989,M_FYR+1989,curitr,cinx, &
                            minx,T_N,FCOD(I2),LCOD(J),TCODS(K),PHMM(M_FYR,CINX,MINX,T_N), &
                            HMMPRD(M_FYR,CINX,MINX,T_N)
                     END IF
                  END IF

               ENDDO
!
!              Sales to Grid
!
               I2=5
               J=4
               DO K=1,SGHTCH
                  T_N = MAP_GR(K,1)
                  T_E = MAP_GR(K,2)
                  IF (T_N .GT. 0 .AND. T_E .GT. 0) THEN
                     IF (HANSW2(1,T_N) + HANSW2(1,T_E) .GT. 0.0) THEN

                        PHMM(M_FYR,CINX,MINX,T_N) = ((HANSW2(1,T_N) * HANSW2(2,T_N)) + &
                            (HANSW2(1,T_E) * HANSW2(2,T_E))) / (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HMMPRD(M_FYR,CINX,MINX,T_N)= (HANSW2(1,T_N) + HANSW2(1,T_E))

                        HCST = HCST + HMMPRD(M_FYR,CINX,MINX,T_N) * PHMM(M_FYR,CINX,MINX,T_N)

                        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,7333) Curiyr+1989,M_FYR+1989,curitr, &
                            cinx,minx,T_N,FCOD(I2),LCOD(J),TCODGR(K), &
                            PHMM(M_FYR,CINX,MINX,T_N),HMMPRD(M_FYR,CINX,MINX,T_N)
                     END IF
                  END IF
               ENDDO

! Price sent to Transportation model is either Average or Marginal as set by HSW_PRCtoTR in hmmcntl file

               IF (HSW_PRCtoTR .eq. 'A') THEN              !average prc
                  IF (HPRD(CINX,MINX).GT.0.0000001) THEN
                    IF (MINX .EQ. 1) PH1TR(CINX,M_FYR) = HCST / HPRD(CINX,MINX) / (CONFCT(1) / 1000000.0)
                    IF (MINX .EQ. 2) PH2TR(CINX,M_FYR) = HCST / HPRD(CINX,MINX) / (CONFCT(1) / 1000000.0)
                    IF (MINX .EQ. 3) PH3TR(CINX,M_FYR) = HCST / HPRD(CINX,MINX) / (CONFCT(1) / 1000000.0)
                  ELSE
                    IF (MINX .EQ. 1) PH1TR(CINX,M_FYR) = 40.0
                    IF (MINX .EQ. 2) PH2TR(CINX,M_FYR) = 40.0
                    IF (MINX .EQ. 3) PH3TR(CINX,M_FYR) = 40.0
                  ENDIF
               ELSEIF   (HSW_PRCtoTR .eq. 'M') THEN         !marginal price
                  IF (HPRD(CINX,MINX).GT.0.0000001) THEN
                    IF (MINX .EQ. 1) PH1TR(CINX,M_FYR) = MargPrc(CINX,MINX) / (CONFCT(1) / 1000000.0)
                    IF (MINX .EQ. 2) PH2TR(CINX,M_FYR) = MargPrc(CINX,MINX) / (CONFCT(1) / 1000000.0)
                    IF (MINX .EQ. 3) PH3TR(CINX,M_FYR) = MargPrc(CINX,MINX) / (CONFCT(1) / 1000000.0)
                  ELSE
                    IF (MINX .EQ. 1) PH1TR(CINX,M_FYR) = 40.0
                    IF (MINX .EQ. 2) PH2TR(CINX,M_FYR) = 40.0
                    IF (MINX .EQ. 3) PH3TR(CINX,M_FYR) = 40.0
                  ENDIF
               ELSE
                  write(HF_HMMO,*)'HSW_PRCtoTR not valid:Fix and rerun.',' HSW_PRCtoTR=',HSW_PRCtoTR
                  STOP
                  ENDIF
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,4i6,f15.6,4f12.2,1x,a,f12.2)')'PH1TRdbg:', &
                  CURIYR,CURITR,CINX,MINX,PH1TR(CINX,CURIYR),HCST,HPRD(CINX,MINX),MC_JPGDP(CURIYR), &
                  CONFCT(1),HSW_PRCtoTR,MargPrc(CINX,MINX)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1 .AND. MINX .EQ. 1) write(HF_HMMO,3337) CURIYR+1989,CURITR, &
                    CINX,MINX,HSW_PRCtoTR,PH1TR(CINX,CURIYR),HCST,HPRD(CINX,MINX),MC_JPGDP(CURIYR), &
                    CONFCT(1),MargPrc(CINX,MINX)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1 .AND. MINX .EQ. 2) write(HF_HMMO,3337) CURIYR+1989,CURITR, &
                    CINX,MINX,HSW_PRCtoTR,PH2TR(CINX,CURIYR),HCST,HPRD(CINX,MINX),MC_JPGDP(CURIYR), &
                    CONFCT(1),MargPrc(CINX,MINX)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1 .AND. MINX .EQ. 3) write(HF_HMMO,3337) CURIYR+1989,CURITR, &
                    CINX,MINX,HSW_PRCtoTR,PH3TR(CINX,CURIYR),HCST,HPRD(CINX,MINX),MC_JPGDP(CURIYR), &
                    CONFCT(1),MargPrc(CINX,MINX)
 3337          FORMAT(1X,"PH_M_TRrsc",4(":",I4),":",A1,6(":",F15.6))

!              I => new
!              J => existing

!              central unseq NG

               TECH = 1
               I = MAP_PC(TECH,1)
               J = MAP_PC(TECH,2)
               HMGSCNS(M_FYR,CINX,MINX) = HMGSCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMGSCNS(M_FYR,CINX,MINX)
 2337          FORMAT(1X,"HANSW2rsc",5(":",I4),":",A2,3(":",I3),5(":",F12.6))

!              central seq NG

               TECH = 2
               I = MAP_PC(TECH,1)
               J = MAP_PC(TECH,2)
               HMGSCNS(M_FYR,CINX,MINX) = HMGSCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMGSCNS(M_FYR,CINX,MINX)

!              central unseq Coal

               TECH = 3
               I = MAP_PC(TECH,1)
               J = MAP_PC(TECH,2)
               HMCLCNS(M_FYR,CINX,MINX) = HMCLCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMCLCNS(M_FYR,CINX,MINX)

!              central seq Coal

               TECH = 4
               I = MAP_PC(TECH,1)
               J = MAP_PC(TECH,2)
               HMCLCNS(M_FYR,CINX,MINX) = HMCLCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMCLCNS(M_FYR,CINX,MINX)

!              central biomass

               TECH = 5
               I = MAP_PC(TECH,1)
               J = MAP_PC(TECH,2)
               HMBICNS(M_FYR,CINX,MINX) = HMBICNS(M_FYR,CINX,MINX) +  ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMBICNS(M_FYR,CINX,MINX)

!              central electrolysis

               TECH = 6
               I = MAP_PC(TECH,1)
               J = MAP_PC(TECH,2)
               HMELCNS(M_FYR,CINX,MINX) = HMELCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMELCNS(M_FYR,CINX,MINX)

!              central nuclear

               TECH = 7
               I = MAP_PC(TECH,1)
               J = MAP_PC(TECH,2)
               HMURCNS(M_FYR,CINX,MINX) = HMURCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMURCNS(M_FYR,CINX,MINX)

!              central biomass with sequestration

               TECH = 8
               I = MAP_PC(TECH,1)
               J = MAP_PC(TECH,2)
               HMBICNS(M_FYR,CINX,MINX) = HMBICNS(M_FYR,CINX,MINX) +  ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PCEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMBICNS(M_FYR,CINX,MINX)

!              CityGate NG

               TECH = 1
               I = MAP_PG(TECH,1)
               J = MAP_PG(TECH,2)
               HMGSCNS(M_FYR,CINX,MINX) = HMGSCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PGEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PGEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMGSCNS(M_FYR,CINX,MINX)

!              Forecourt Electrolyzer

               TECH = 1
               I = MAP_PF(TECH,1)
               J = MAP_PF(TECH,2)
               HMELCNS(M_FYR,CINX,MINX) = HMELCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PFEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PFEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMELCNS(M_FYR,CINX,MINX)

!              Forecourt SMR Revised

               TECH = 2
               I = MAP_PF(TECH,1)
               J = MAP_PF(TECH,2)
               HMGSCNS(M_FYR,CINX,MINX) = HMGSCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PFEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I) = HCNSTECH(M_FYR,CINX,I) + ((HANSW2(1,I) + HANSW2(1,J)) * PFEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMGSCNS(M_FYR,CINX,MINX)

!              Forecourt SER Revised

               TECH = 3
               I = MAP_PF(TECH,1)
               J = MAP_PF(TECH,2)
               HMETCNS(M_FYR,CINX,MINX) = HMETCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PFEFF(M_FYR,TECH) / 1000000.0)
               HCNSTECH(M_FYR,CINX,I)  = HCNSTECH(M_FYR,CINX,I)  + ((HANSW2(1,I) + HANSW2(1,J)) * PFEFF(M_FYR,TECH) / 1000000.0)
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2337) CURIYR+1989,M_FYR+1989,&
                    CURITR,CINX,MINX,TCODPC(TECH),TECH,I,J,HANSW2(1,I),HANSW2(1,J), &
                    PCEFF(M_FYR,TECH),HCNSTECH(M_FYR,CINX,I),HMETCNS(M_FYR,CINX,MINX)
!
!              now add in electricity consumption for running hydrogen plants
!
               DO TECH = 1 , PCHTCH
                  IF (MAP_PC(TECH,1) .GT. 0 .AND. TECH .NE. 6) THEN     ! do this for all central techs except Electrolysis
                     I = MAP_PC(TECH,1)   !new
                     J = MAP_PC(TECH,2)   !existing
                     HMELCNS(M_FYR,CINX,MINX) = HMELCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PCELC(M_FYR,TECH) / 1000000.0)
                  END IF
               END DO
!
               DO TECH = 1 , PGHTCH
                  IF (MAP_PG(TECH,1) .GT. 0) THEN     ! do this for the city gate techs
                     I = MAP_PG(TECH,1)   !new
                     J = MAP_PG(TECH,2)   !existing
                     HMELCNS(M_FYR,CINX,MINX) = HMELCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PGELC(M_FYR,TECH) / 1000000.0)
                  END IF
               END DO
!
               DO TECH = 1 , PFHTCH
                  IF (MAP_PF(TECH,1) .GT. 0 .AND. TECH .NE. 1) THEN     ! do this for the forecourt techs except Electrolysis
                     I = MAP_PF(TECH,1)   !new
                     J = MAP_PF(TECH,2)   !existing
                     HMELCNS(M_FYR,CINX,MINX) = HMELCNS(M_FYR,CINX,MINX) + ((HANSW2(1,I) + HANSW2(1,J)) * PFELC(M_FYR,TECH) / 1000000.0)
                  END IF
               END DO
!
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2336) CURIYR+1989,M_FYR+1989,CURITR,CINX,MINX, &
                    HMELCNS(M_FYR,CINX,MINX)
2336           FORMAT(1X,"HMELCNSliq1",5(":",I4),1(":",F12.6))
!
!              now add in liquifier electricity consumption
!
               TECH = 2
               I = MAP_S(TECH,1)   !new
               J = MAP_S(TECH,2)   !existing
               HMELCNS(M_FYR,CINX,MINX) = HMELCNS(M_FYR,CINX,MINX) + &
                   ((HANSW2(1,I) + HANSW2(1,J)) * LIQELC(M_FYR) / 1000000.0)
!
!              add consumption to Q variables
!
               QELHM(CINX,M_FYR) = QELHM(CINX,M_FYR) + HMELCNS(M_FYR,CINX,MINX) * 1000.0
               QGFHM(CINX,M_FYR) = QGFHM(CINX,M_FYR) + HMGSCNS(M_FYR,CINX,MINX) * 1000.0
               QNGHM(CINX,M_FYR) = QNGHM(CINX,M_FYR) + HMGSCNS(M_FYR,CINX,MINX) * 1000.0
               QCLHM(CINX,M_FYR) = QCLHM(CINX,M_FYR) + HMCLCNS(M_FYR,CINX,MINX) * 1000.0
               QURHM(CINX,M_FYR) = QURHM(CINX,M_FYR) + HMURCNS(M_FYR,CINX,MINX) * 1000.0
               QBMHM(CINX,M_FYR) = QBMHM(CINX,M_FYR) + HMBICNS(M_FYR,CINX,MINX) * 1000.0
               QETHM(CINX,M_FYR) = QETHM(CINX,M_FYR) + HMETCNS(M_FYR,CINX,MINX) * 1000.0

               IF (FCRL .EQ. 1) WRITE(HF_HMMO,3455) CURIYR+1989,M_FYR+1989,CURITR,CINX,MINX, &
                      QELHM(CINX,M_FYR),QGFHM(CINX,M_FYR),QNGHM(CINX,M_FYR),QCLHM(CINX,M_FYR),&
                      QURHM(CINX,M_FYR),QBMHM(CINX,M_FYR),QETHM(CINX,M_FYR)
3455           FORMAT(1X,">>>>> H2 consumption vars:  ",5(":",I4),7(":",F16.4))

               IF (ISNAN(QELHM(CINX,M_FYR)) .OR. ISNAN(QGFHM(CINX,M_FYR)) .OR. &
                   ISNAN(QNGHM(CINX,M_FYR)) .OR. ISNAN(QCLHM(CINX,M_FYR)) .OR. &
                   ISNAN(QURHM(CINX,M_FYR)) .OR. ISNAN(QBMHM(CINX,M_FYR)) .OR. &
                   ISNAN(QETHM(CINX,M_FYR)) ) THEN

                   WRITE(HF_HMMO,3456) CURIYR+1989,M_FYR+1989,CURITR,CINX,MINX, &
                      QELHM(CINX,M_FYR),QGFHM(CINX,M_FYR),QNGHM(CINX,M_FYR),QCLHM(CINX,M_FYR),&
                      QURHM(CINX,M_FYR),QBMHM(CINX,M_FYR),QETHM(CINX,M_FYR)
3456               FORMAT(1X,"NaN in H2 consumption vars, stopping!!",5(":",I4),7(":",F16.4))

                   STOP 666

               ENDIF

               ! debug
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2339) CURIYR+1989,M_FYR+1989,CURITR,CINX,MINX, &
                    HMETCNS(M_FYR,CINX,MINX),QETHM(CINX,M_FYR)
2339           FORMAT(1X,"QETHMdbg",5(":",I4),2(":",F12.3))
!
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,2338) CURIYR+1989,M_FYR+1989,CURITR,CINX,MINX, &
                    HMELCNS(M_FYR,CINX,MINX), &
                    ((HANSW2(1,I) + HANSW2(1,J)) * LIQELC(M_FYR) / 1000000.0), &
                    (HANSW2(1,I) + HANSW2(1,J)),QELHM(CINX,M_FYR),LIQELC(M_FYR)
2338           FORMAT(1X,"HMELCNSliq2",5(":",I4),6(":",F12.6))

!              load hmm sales to grid into cogen variables

               CGHMMGEN(CINX,M_FYR,1,1) = CGHMMGEN(CINX,M_FYR,1,1) + (HANSW2(1,TOTNDX) + HANSW2(1,TOTNDX + TOTNDX + 1)) * 1000000000.0 / 3412.0  ! convert to mil kWh to match ind cogen
!              CGHMMCAP(CINX,M_FYR,1)   = CGHMMCAP(CINX,M_FYR,1) + (HANSW2(1,TOTNDX) + HANSW2(1,TOTNDX + TOTNDX + 1)) / 8760.0 * 1000000.0 / 3412.0
!            hardwire CF for now - need to replace with techn utz
               CGHMMCAP(CINX,M_FYR,1) = CGHMMCAP(CINX,M_FYR,1) + (HANSW2(1,TOTNDX) + HANSW2(1,TOTNDX + TOTNDX + 1)) * 1000 / (8760.0 * 0.90) * 1000000000.0 / 3412.0

               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,5i5,6f15.6)')'CGHMMGENinHMM,y,i,r,m,totndx,New,Exist,tot,totAdded,GEN,CAP:',&
                  M_FYR,curitr,cinx,minx,totndx,HANSW2(1,TOTNDX),  &
                  HANSW2(1,TOTNDX+TOTNDX+1),  &
                  (HANSW2(1,TOTNDX) + HANSW2(1,TOTNDX+TOTNDX+1)),  &
                  ((HANSW2(1,TOTNDX) + HANSW2(1,TOTNDX+TOTNDX+1))/8760.0) * 1000000.0/3412.0 ,  &
                  CGHMMGEN(CINX,M_FYR,1,1),CGHMMCAP(CINX,M_FYR,1)

            ENDIF ! FYR <= LASTYR

            ! Add this market's hydrogen production to this region's total
            HPRD(CINX,4) = HPRD(CINX,4) + HPRD(CINX,MINX)

         ENDDO ! MINX

!
!         Compute average marginal hydrogen price over all markets ($/MMbtu)
!
          XYR   = PLPD_PRC_YR
          FYR   = CURIYR + PLPD_SYR(XYR) - 1
          M_FYR = MIN(FYR , LASTYR)

          IF (HPRD(CINX,4) .GT. 0.000001) THEN
              PHYTR(CINX,M_FYR) = (PH1TR(CINX,M_FYR) * HPRD(CINX,1) + &
                         PH2TR(CINX,M_FYR) * HPRD(CINX,2) + &
                         PH3TR(CINX,M_FYR) * HPRD(CINX,3)) / HPRD(CINX,4)
          ELSE
              PHYTR(CINX,M_FYR) = 0.0
          ENDIF

          IF (FCRL .EQ. 1) WRITE(HF_HMMO,1123) CURIYR+1989,M_FYR+1989,CURITR,CINX,MINX, &
                    PHYTR(CINX,M_FYR),PH1TR(CINX,M_FYR),HPRD(CINX,1), &
                    PH2TR(CINX,M_FYR),HPRD(CINX,2), &
                    PH3TR(CINX,M_FYR),HPRD(CINX,3),HPRD(CINX,4)
1123      FORMAT(1X,"PHYTR_dbg",5(":",I4),8(":",F12.6))

      ENDDO ! CINX

!     RECALCULATE BIOMASS CONSUMPTION BY COAL DEMAND REGION (TRILLS) POST-MARKET-SHARING

      WD_BTUS_H2 = 0.0
      DO CRG = 1 , NDREG
         CINX = MAP_CR_TO_CL(CRG)

         WD_BTUS_H2(CRG)   = WD_BTUS_H2(CRG) + BM_SHARE(CRG)*QBMHM(CINX,CURIYR)

         WRITE(HF_HMMO,3714) CURIRUN,CURIYR+1989,CURITR,CRG,CINX,BM_SHARE(CRG),QBMHM(CINX,CURIYR),WD_BTUS_H2(CRG)
 3714    FORMAT(1X,"BIOMASS_CONSUMPTION_2",5(":",I4),3(":",F12.3))
      END DO

      ! Calculate the national average hydrogen price
      PQSUM = 0.0
      DO CINX = 1,9
         PQSUM = PQSUM + PHYTR(CINX,M_FYR)*HPRD(CINX,4)
      ENDDO
      IF (SUM(HPRD(1:9,4)) .GT. 0.000001) THEN
         PHYTR(11,M_FYR) = PQSUM / SUM(HPRD(1:9,4))
      ELSE
         PHYTR(11,M_FYR) = 0.0
      ENDIF

      IF (FCRL .EQ. 1) WRITE(HF_HMMO,1123) CURIYR+1989,M_FYR+1989,CURITR,11,0, &
                PHYTR(11,M_FYR), 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0

      RETURN
      END SUBROUTINE RETRIEVE_HMM

      SUBROUTINE HMMNATS

      IMPLICIT NONE

      INTEGER I,J,K,TECH,TMP,J2,K2,IYR,K3
!      REAL*8 TMPCST(MNUMCR,HMKT,TOTNDX,2)
      REAL*8 TMPCST(MNUMCR,HMKT,TOTNDX)
      REAL*8 MKTTOT


!     FYR = PLPD_SYR(PLPD_PRC_YR)
      FYR = PLPD_SYR(PLPD_PRC_YR) + CURIYR - 1
      M_FYR = MIN(FYR , LASTYR)
      DO J=1,HMKT
         HMGSCNS(M_FYR,MNUMCR,J) = 0.0
         HMCLCNS(M_FYR,MNUMCR,J) = 0.0
         HMBICNS(M_FYR,MNUMCR,J) = 0.0
         HMELCNS(M_FYR,MNUMCR,J) = 0.0
         HMURCNS(M_FYR,MNUMCR,J) = 0.0
         HMETCNS(M_FYR,MNUMCR,J) = 0.0
         DO I=1,TOTNDX
            HMMPRD(M_FYR,MNUMCR,J,I) = 0.0
            PHMM(M_FYR,MNUMCR,J,I) = 0.0
            TMPCST(MNUMCR,J,I) = 0.0
         ENDDO
      ENDDO
      DO I=1,PCHTCH+PGHTCH+PFHTCH
         HCNSTECH(M_FYR,MNUMCR,I)=0.0
      ENDDO

      CGHMMGEN(MNUMCR,M_FYR,1,1) = 0.0
      CGHMMCAP(MNUMCR,M_FYR,1)   = 0.0

      DO J=1,HMKT
         DO K=1,MNUMCR-2
            HMGSCNS(M_FYR,MNUMCR,J)=HMGSCNS(M_FYR,MNUMCR,J) + &
               HMGSCNS(M_FYR,K,J)
            HMCLCNS(M_FYR,MNUMCR,J)=HMCLCNS(M_FYR,MNUMCR,J) + &
               HMCLCNS(M_FYR,K,J)
            HMBICNS(M_FYR,MNUMCR,J)=HMBICNS(M_FYR,MNUMCR,J) + &
               HMBICNS(M_FYR,K,J)
            HMELCNS(M_FYR,MNUMCR,J)=HMELCNS(M_FYR,MNUMCR,J) + &
               HMELCNS(M_FYR,K,J)
            HMURCNS(M_FYR,MNUMCR,J)=HMURCNS(M_FYR,MNUMCR,J) + &
               HMURCNS(M_FYR,K,J)
            HMETCNS(M_FYR,MNUMCR,J)=HMETCNS(M_FYR,MNUMCR,J) + &
               HMETCNS(M_FYR,K,J)
         ENDDO
      ENDDO

      DO I=1,TOTNDX
         DO J=1,HMKT
            DO K=1,MNUMCR-2
               HMMPRD(M_FYR,MNUMCR,J,I)=HMMPRD(M_FYR,MNUMCR,J,I) + &
                    HMMPRD(M_FYR,K,J,I)
               TMPCST(MNUMCR,J,I)=TMPCST(MNUMCR,J,I) + &
                    HMMPRD(M_FYR,K,J,I)*PHMM(M_FYR,K,J,I)
            ENDDO
            IF (HMMPRD(M_FYR,MNUMCR,J,I).GT.0) THEN
               PHMM(M_FYR,MNUMCR,J,I)=TMPCST(MNUMCR,J,I)/  &
                 HMMPRD(M_FYR,MNUMCR,J,I)
            ELSE
               PHMM(M_FYR,MNUMCR,J,I)=0.0
            ENDIF
          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,8942)'NATS DBG',M_FYR,J,I,HMMPRD(M_FYR,MNUMCR,J,I)
8942     FORMAT(A12,1x,3(I4,1x),F15.6)
         ENDDO
      ENDDO

      DO I=1,PCHTCH+PGHTCH+PFHTCH
        DO K=1,MNUMCR-2
           HCNSTECH(M_FYR,MNUMCR,I)= HCNSTECH(M_FYR,MNUMCR,I) +  &
             HCNSTECH(M_FYR,K,I)
        ENDDO
        IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,8943)'HCNSTECH',M_FYR,I,HCNSTECH(M_FYR,MNUMCR,I)
8943    FORMAT(A12,1x,2(I4,1x),F15.6)
      ENDDO

      DO K=1,MNUMCR-2
         CGHMMGEN(MNUMCR,M_FYR,1,1) = CGHMMGEN(MNUMCR,M_FYR,1,1) + CGHMMGEN(K,M_FYR,1,1)
         CGHMMCAP(MNUMCR,M_FYR,1) = CGHMMCAP(MNUMCR,M_FYR,1) + CGHMMCAP(K,M_FYR,1)
         IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,3i6,4f15.5)')'CGHMMGENnatTOT,y,i,r,rgen,rcap,tgen,tcap:', &
           M_FYR,curitr,K,CGHMMGEN(K,M_FYR,1,1),CGHMMCAP(K,M_FYR,1),  &
           CGHMMGEN(MNUMCR,M_FYR,1,1),CGHMMCAP(MNUMCR,M_FYR,1)
      ENDDO

!
!     Hydrogen production by fuel in trills
!
        HMGSPRD(M_FYR,11) = 0.0
        HMCLPRD(M_FYR,11) = 0.0
        HMBIPRD(M_FYR,11) = 0.0
        HMELPRD(M_FYR,11) = 0.0
        HMURPRD(M_FYR,11) = 0.0
        HMETPRD(M_FYR,11) = 0.0

      DO I=1,MNUMCR-2

        HMGSPRD(M_FYR,I) = 0.0
        HMCLPRD(M_FYR,I) = 0.0
        HMBIPRD(M_FYR,I) = 0.0
        HMELPRD(M_FYR,I) = 0.0
        HMURPRD(M_FYR,I) = 0.0
        HMETPRD(M_FYR,I) = 0.0

        DO J=1,HMKT

          ! Central production
          DO K=1,PCHTCH
            IF (MAP_PC(K,1) .GT. 0) THEN
              TECH = MAP_PC(K,1)

              IF (TFUEL(TECH).EQ.'N') THEN
                HMGSPRD(M_FYR,I) = HMGSPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'C') THEN
                HMCLPRD(M_FYR,I) = HMCLPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'W') THEN
                HMBIPRD(M_FYR,I) = HMBIPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'E') THEN
                HMELPRD(M_FYR,I) = HMELPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'U') THEN
                HMURPRD(M_FYR,I) = HMURPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'T') THEN
                HMETPRD(M_FYR,I) = HMETPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ENDIF
            ENDIF
          ENDDO

          ! City-gate production
          DO K=1,PGHTCH
            IF (MAP_PG(K,1) .GT. 0) THEN
              TECH = MAP_PG(K,1)

              IF (TFUEL(TECH).EQ.'N') THEN
                HMGSPRD(M_FYR,I) = HMGSPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'C') THEN
                HMCLPRD(M_FYR,I) = HMCLPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'W') THEN
                HMBIPRD(M_FYR,I) = HMBIPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'E') THEN
                HMELPRD(M_FYR,I) = HMELPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'U') THEN
                HMURPRD(M_FYR,I) = HMURPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'T') THEN
                HMETPRD(M_FYR,I) = HMETPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ENDIF
            ENDIF
          ENDDO

          ! Forecourt production
          DO K=1,PFHTCH
            IF (MAP_PF(K,1) .GT. 0) THEN
              TECH = MAP_PF(K,1)

              IF (TFUEL(TECH).EQ.'N') THEN
                HMGSPRD(M_FYR,I) = HMGSPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'C') THEN
                HMCLPRD(M_FYR,I) = HMCLPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'W') THEN
                HMBIPRD(M_FYR,I) = HMBIPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'E') THEN
                HMELPRD(M_FYR,I) = HMELPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'U') THEN
                HMURPRD(M_FYR,I) = HMURPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ELSEIF (TFUEL(TECH).EQ.'T') THEN
                HMETPRD(M_FYR,I) = HMETPRD(M_FYR,I) + &
                                  HMMPRD(M_FYR,I,J,TECH)*CONFCT(1)/1000

              ENDIF
            ENDIF
          ENDDO

        ENDDO

        ! National totals
        HMGSPRD(M_FYR,11) = HMGSPRD(M_FYR,11) + HMGSPRD(M_FYR,I)
        HMCLPRD(M_FYR,11) = HMCLPRD(M_FYR,11) + HMCLPRD(M_FYR,I)
        HMBIPRD(M_FYR,11) = HMBIPRD(M_FYR,11) + HMBIPRD(M_FYR,I)
        HMELPRD(M_FYR,11) = HMELPRD(M_FYR,11) + HMELPRD(M_FYR,I)
        HMURPRD(M_FYR,11) = HMURPRD(M_FYR,11) + HMURPRD(M_FYR,I)
        HMETPRD(M_FYR,11) = HMETPRD(M_FYR,11) + HMETPRD(M_FYR,I)

      ENDDO

!
!     Inputs to hydrogen production by fuel in trills (Heat and Power)
!
        HMGSHP(M_FYR,11) = 0.0
        HMCLHP(M_FYR,11) = 0.0
        HMBIHP(M_FYR,11) = 0.0
        HMELHP(M_FYR,11) = 0.0
        HMURHP(M_FYR,11) = 0.0
        HMETHP(M_FYR,11) = 0.0

      DO I=1,MNUMCR-2

        HMGSHP(M_FYR,I) = 0.0
        HMCLHP(M_FYR,I) = 0.0
        HMBIHP(M_FYR,I) = 0.0
        HMELHP(M_FYR,I) = 0.0
        HMURHP(M_FYR,I) = 0.0
        HMETHP(M_FYR,I) = 0.0

        ! Natural gas
        MKTTOT = 0.0
        DO J=1,HMKT
           MKTTOT = MKTTOT + HMGSCNS(M_FYR,I,J)*1000
        ENDDO
        HMGSHP(M_FYR,I) = MKTTOT - HMGSPRD(M_FYR,I)
        HMGSHP(M_FYR,11) = HMGSHP(M_FYR,11) + HMGSHP(M_FYR,I)

        ! Coal
        MKTTOT = 0.0
        DO J=1,HMKT
           MKTTOT = MKTTOT + HMCLCNS(M_FYR,I,J)*1000
        ENDDO
        HMCLHP(M_FYR,I) = MKTTOT - HMCLPRD(M_FYR,I)
        HMCLHP(M_FYR,11) = HMCLHP(M_FYR,11) + HMCLHP(M_FYR,I)

        ! Biomass
        MKTTOT = 0.0
        DO J=1,HMKT
           MKTTOT = MKTTOT + HMBICNS(M_FYR,I,J)*1000
        ENDDO
        HMBIHP(M_FYR,I) = MKTTOT - HMBIPRD(M_FYR,I)
        HMBIHP(M_FYR,11) = HMBIHP(M_FYR,11) + HMBIHP(M_FYR,I)

        ! Electricity
        MKTTOT = 0.0
        DO J=1,HMKT
           MKTTOT = MKTTOT + HMELCNS(M_FYR,I,J)*1000
        ENDDO
        HMELHP(M_FYR,I) = MKTTOT - HMELPRD(M_FYR,I)
        HMELHP(M_FYR,11) = HMELHP(M_FYR,11) + HMELHP(M_FYR,I)

        ! Uranium
        MKTTOT = 0.0
        DO J=1,HMKT
           MKTTOT = MKTTOT + HMURCNS(M_FYR,I,J)*1000
        ENDDO
        HMURHP(M_FYR,I) = MKTTOT - HMURPRD(M_FYR,I)
        HMURHP(M_FYR,11) = HMURHP(M_FYR,11) + HMURHP(M_FYR,I)

        ! Ethanol
        MKTTOT = 0.0
        DO J=1,HMKT
           MKTTOT = MKTTOT + HMETCNS(M_FYR,I,J)*1000
        ENDDO
        HMETHP(M_FYR,I) = MKTTOT - HMETPRD(M_FYR,I)
        HMETHP(M_FYR,11) = HMETHP(M_FYR,11) + HMETHP(M_FYR,I)

        ! nak debug
        IF (FCRL .EQ. 1) write(HF_HMMO,1122) M_FYR,CURITR,I, &
                           HMGSPRD(M_FYR,I),HMCLPRD(M_FYR,I),HMBIPRD(M_FYR,I), &
                           HMELPRD(M_FYR,I),HMURPRD(M_FYR,I),HMETPRD(M_FYR,I), &
                           HMGSHP(M_FYR,I),HMCLHP(M_FYR,I),HMBIHP(M_FYR,I), &
                           HMELHP(M_FYR,I),HMURHP(M_FYR,I),HMETHP(M_FYR,I)
1122    FORMAT(1X,"PRDCHP_DBG",3(":",I4),12(":",F14.6))

      ENDDO

      ! nak debug
      IF (FCRL .EQ. 1) write(HF_HMMO,1122) M_FYR,CURITR,11, &
                         HMGSPRD(M_FYR,11),HMCLPRD(M_FYR,11),HMBIPRD(M_FYR,11), &
                         HMELPRD(M_FYR,11),HMURPRD(M_FYR,11),HMETPRD(M_FYR,11), &
                         HMGSHP(M_FYR,11),HMCLHP(M_FYR,11),HMBIHP(M_FYR,11), &
                         HMELHP(M_FYR,11),HMURHP(M_FYR,11),HMETHP(M_FYR,11)


      RETURN
      END SUBROUTINE HMMNATS


      SUBROUTINE GETDAT
! This subroutine gets input data

      IMPLICIT NONE


      INTEGER I,J,K,TMP,J2,K2,IYR,K3,K4,L,rec_cnt,tmpidx
      INTEGER*4 HIYR,TMP1,TMP2,TMP3,yr,NUMBLOCKS
      REAL*8 VMT(MNUMCR,HMKT),POP(MNUMCR,HMKT),RAD(MNUMCR,HMKT),GAST(MNUMCR,HMKT),IMPVMT(MNUMCR,HMKT)
      REAL*8 GALS(MNUMCR)
      REAL*8 MGALD(MNUMCR)
      REAL*8 DIESL(MNUMCR),MGSDD(MNUMCR,MNUMYR),MGSDM(MNUMCR,MNUMYR)
      REAL*8 VMTSH(MNUMCR,HMKT),VMTST(MNUMCR,HMKT),POPST(MNUMCR,HMKT)
      REAL*8 DGG(MNUMCR,HMKT),GPH(MNUMCR,HMKT),GXX(MNUMCR,HMKT)
      REAL*8 TOTS(MNUMCR)
      REAL*8 COCP,COCTD,COCFD,FCT
      REAL*8 MKDMDP,TDIF2,TDIF3,TDIF4,TDIF5,TDIF6,TDIF7, &
                 TDIF8,TDIF9,TDIF10,TDIF11,TDIF12,TDIF13, &
                 TDIF14,TDIF15,TDIF16
      REAL*8 MGSDG(MNUMCR,MNUMYR)
      REAL*8 VAL,TOTA(MNUMCR),GDPFIX

      CHARACTER*230 RDDATA
!      CHARACTER*45 TdName(TTDNDX)
!      common /delnames/  TdName

      LOGICAL       NEW/.FALSE./
      CHARACTER*18  FILENM
      INTEGER       FILE_MGR
      EXTERNAL      FILE_MGR

      CHARACTER*30 TEMP_TNAM
      CHARACTER*1  TEMP_TFUEL


!      OPEN(32,file='n:/projects/aeo05/hydrogen/tstout.txt',status='old', &
!            recl=368)
!      OPEN(32,file='tstout.txt',status='old',recl=368)
      FILENM = 'HMMDBG'
      NEW = .TRUE.
      HF_HMMO = FILE_MGR('O',FILENM,NEW)

      HF_FORDB = 2525
      open(unit=HF_FORDB,file='fordb_prc.txt')

      HF_MWH = 2526
      open(unit=HF_MWH,file='mwh.txt')

      IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
        write(HF_HMMO,1480)
      endif
 1480   format("DELIV COST:","yr:","itr:","census:","mkt:",  &
         "N/E:","TECH:","TName:","Distance:","Quant:","annCap:","annFL:", &
         "NonFL:","annLabor:","TotCost")

      GDPFIX=1.45

      TMP=0.0

      VAL = 0.0
      DO I=1,MNUMCR
        TOTS(I)=0.0
        DO J=1,HMKT
          VMTSH(I,J)=0.0
          VMTST(I,J)=0.0
          POPST(I,J)=0.0
          DGG(I,J)=0.0
          GPH(I,J)=0.0
          GXX(I,J)=0.0
          MSHARE(I,J)=0.0
          IMPVMT(I,J) = 0.0
        ENDDO
      ENDDO

!
      FILENM = 'HMMCNTL'
      NEW = .FALSE.
      HF_HMM = FILE_MGR('O',FILENM,NEW)

!      OPEN(35,file='n:/projects/aeo05/hydrogen/hmmcntl.txt',status='old')
!      READ(HF_HMM,*)RDDATA
!       DO I=1,TCHTCH
!         READ(HF_HMM,*)J,RDDATA,(TCHNDX(J,K),K=1,4)
!       ENDDO
!       DO I=1,TGHTCH
!         READ(HF_HMM,*)J,RDDATA,(TGHNDX(J,K),K=1,4)
!       ENDDO
!       DO I=1,DOHTCH
!         READ(HF_HMM,*)J,RDDATA,(DOHNDX(J,K),K=1,4)
!       ENDDO
!       DO I=1,SOHTCH
!         READ(HF_HMM,*)J,RDDATA,(SOHNDX(J,K),K=1,4)
!       ENDDO

       DO I=1,MNUMYR
         READ(HF_HMM,*)J,RDDATA,PENRAT(I)
       ENDDO
!       READ(HF_HMM,*)RDDATA,AVGMPH
!       READ(HF_HMM,*)RDDATA,TRKMPG
!       READ(HF_HMM,*)RDDATA,TDRIV
       READ(HF_HMM,*)RDDATA,CONFCT(1)
       READ(HF_HMM,*)RDDATA,CONFCT(2)
       READ(HF_HMM,*)RDDATA,CONFCT(3)
       READ(HF_HMM,*)RDDATA,CONFCT(4)
       READ(HF_HMM,*)RDDATA,CONFCT(5)
       READ(HF_HMM,*)RDDATA,CONFCT(6)
       READ(HF_HMM,*)RDDATA,CONFCT(7)
       READ(HF_HMM,*)RDDATA,FCT
       READ(HF_HMM,*)RDDATA,MAXLIQ
       READ(HF_HMM,*)RDDATA,MKDMDP
       READ(HF_HMM,*)RDDATA,EFFFC
       READ(HF_HMM,*)RDDATA,UTILZ
       READ(HF_HMM,*)RDDATA,KGMO
       READ(HF_HMM,*)RDDATA,PUMPS
       READ(HF_HMM,*)RDDATA,HPDO
       READ(HF_HMM,*)RDDATA,ADPM
       READ(HF_HMM,*)RDDATA,KGPHD
       READ(HF_HMM,*)RDDATA,MDCTP
       READ(HF_HMM,*)RDDATA,FUNCPI
       READ(HF_HMM,*)RDDATA,COEHM
       READ(HF_HMM,*)RDDATA,CODHM
       READ(HF_HMM,*)RDDATA,EQTYHM
       READ(HF_HMM,*)RDDATA,COEPIP
       READ(HF_HMM,*)RDDATA,CODPIP
       READ(HF_HMM,*)RDDATA,EQTYPIP
       READ(HF_HMM,*)RDDATA,COETD
       READ(HF_HMM,*)RDDATA,CODTD
       READ(HF_HMM,*)RDDATA,EQTYTD
       READ(HF_HMM,*)RDDATA,FEDRAT
       READ(HF_HMM,*)RDDATA,HSW_PRCtoTR
       if (curitr.eq.1 .and. (HSW_PRCtoTR.ne.'A' .and. HSW_PRCtoTR.ne.'M'))  &
         write(HF_HMMO,*)'HSW_PRCtoTR not set correctly:Fix and rerun.',' HSW_PRCtoTR=',HSW_PRCtoTR
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA,NUMPLPD,PLPD_PRC_YR,PLPD_INV_YR
       N_XYR = NUMPLPD
       N_FYR = 0
       READ(HF_HMM,*)RDDATA
       DO I=1,NUMPLPD
          READ(HF_HMM,*)PLPDCOD(I),PLPDYRS(I)
          PLPD_SYR(I) = N_FYR + 1
          N_FYR = N_FYR + PLPDYRS(I)
          PLPD_EYR(I) = N_FYR
          write(HF_HMMO,*)'HMMCNTL,planning pds: ',i,PLPDCOD(i),PLPDYRS(i),PLPD_SYR(I),PLPD_EYR(I),N_FYR
       ENDDO
!      WRITE(6,3475) CURIYR+1989,CURITR,N_FYR
 3475  FORMAT(1X,"N_FYRrsc",3(":",I4))
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA,NUMF
       READ(HF_HMM,*)RDDATA
       DO I=1,NUMF
          READ(HF_HMM,*)tmpidx,FCOD(I)
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,functions tbl: ',i,FCOD(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA,NUML
       READ(HF_HMM,*)RDDATA
       DO I=1,NUML
          READ(HF_HMM,*)tmpidx,LCOD(I)
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,Locations tbl: ',i,LCOD(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       DO I=1,PCHTCH
          READ(HF_HMM,*)tmpidx,TCODPC(I),(MAP_PC(I,J),J=1,2),TEMP_TNAM,TEMP_TFUEL
          IF (MAP_PC(I,1).GT.0) THEN
             TNAM(MAP_PC(I,1)) = TEMP_TNAM
             TFUEL(MAP_PC(I,1)) = TEMP_TFUEL
          ENDIF
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,PC tbl: ',i,TCODPC(i)
          write(HF_HMMO,'(a,3i4)')'HMMCNTL,MAP_PC: ',i,(MAP_PC(I,J),J=1,2)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       DO I=1,PGHTCH
          READ(HF_HMM,*)tmpidx,TCODPG(I),(MAP_PG(I,J),J=1,2),TEMP_TNAM,TEMP_TFUEL
          IF (MAP_PG(I,1).GT.0) THEN
             TNAM(MAP_PG(I,1)) = TEMP_TNAM
             TFUEL(MAP_PG(I,1)) = TEMP_TFUEL
          ENDIF
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,PG tbl: ',i,TCODPG(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       DO I=1,PFHTCH
          READ(HF_HMM,*)tmpidx,TCODPF(I),(MAP_PF(I,J),J=1,2),TEMP_TNAM,TEMP_TFUEL
          IF (MAP_PF(I,1).GT.0) THEN
             TNAM(MAP_PF(I,1)) = TEMP_TNAM
             TFUEL(MAP_PF(I,1)) = TEMP_TFUEL
          ENDIF
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,PF tbl: ',i,TCODPF(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       DO I=1,TCHTCH
          READ(HF_HMM,*)tmpidx,TCODTC(I),(MAP_TC(I,J),J=1,2),TEMP_TNAM
          IF (MAP_TC(I,1).GT.0) TNAM(MAP_TC(I,1)) = TEMP_TNAM
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,TC tbl: ',i,TCODTC(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       DO I=1,TGHTCH
          READ(HF_HMM,*)tmpidx,TCODTG(I),(MAP_TG(I,J),J=1,2),TEMP_TNAM
          IF (MAP_TG(I,1).GT.0) TNAM(MAP_TG(I,1)) = TEMP_TNAM
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,TG tbl: ',i,TCODTG(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       DO I=1,DOHTCH
          READ(HF_HMM,*)tmpidx,TCODD(I),(MAP_D(I,J),J=1,2),TEMP_TNAM
          IF (MAP_D(I,1).GT.0) TNAM(MAP_D(I,1)) = TEMP_TNAM
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,Dist tbl: ',i,TCODD(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       DO I=1,SOHTCH
          READ(HF_HMM,*)tmpidx,TCODS(I),(MAP_S(I,J),J=1,2),TEMP_TNAM
          IF (MAP_S(I,1).GT.0) TNAM(MAP_S(I,1)) = TEMP_TNAM
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,Disp tbl: ',i,TCODS(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       DO I=1,SGHTCH
          READ(HF_HMM,*)tmpidx,TCODGR(I),(MAP_GR(I,J),J=1,2),TEMP_TNAM
          IF (MAP_GR(I,1).GT.0) TNAM(MAP_GR(I,1)) = TEMP_TNAM
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,SalesGrid tbl: ',i,TCODGR(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)tmpidx,TCODAC,(MAP_AC(J),J=1,2)
       write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,Average Costs: ',1,TCODAC
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       DO I=1,HMKT
          READ(HF_HMM,*)tmpidx,MCOD(I)
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,Market Codes: ',i,MCOD(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA,NUMRW
       READ(HF_HMM,*)RDDATA
       DO I=1,NUMRW
          READ(HF_HMM,*)tmpidx,ROWDEF(I)
          write(HF_HMMO,'(a,i4,a6)')'HMMCNTL,Row Defs: ',i,ROWDEF(i)
       ENDDO
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       READ(HF_HMM,*)RDDATA
       DO I=1,MNUMCR-2
          READ(HF_HMM,*)RCOD(I),RGNAM(I)
          write(HF_HMMO,'(a,i4,a6,a30)')'HMMCNTL,Region Codes: ',i,RCOD(i),RGNAM(i)
       ENDDO


       ! Write out technology names
       DO I=1,TOTNDX
          write(HF_HMMO,'(a,1i4,a)')'HMMCNTL,TNAM,I:',I,TNAM(I)
       ENDDO
       write(HF_HMMO,'(a,1f10.2)')'HMMCNTL,confct(1):',  &
           confct(1)
       write(HF_HMMO,'(a,3f10.2)')'HMMCNTL coehm, codhm, eqtyhm:  ', coehm,  codhm,  eqtyhm
       write(HF_HMMO,'(a,3f10.2)')'HMMCNTL coepip,codpip,eqtypip: ', coepip, codpip, eqtypip
       write(HF_HMMO,'(a,3f10.2)')'HMMCNTL coetd, codtd, eqtytd:  ', coetd,  codtd,  eqtytd
       write(HF_HMMO,'(a,3f10.2)')'HMMCNTL fedrat:                ', fedrat
       write(HF_HMMO,'(a,a)')     'HMMCNTL price sent to Transportation model: ',HSW_PRCtoTR
       write(HF_HMMO,'(a,i6)')    'HMMCNTL number of planning periods:     ',NUMPLPD
       write(HF_HMMO,'(a,<NUMPLPD>a6)')  'HMMCNTL index codes for each planning period:    ',(PLPDCOD(I),I=1,NUMPLPD)
       write(HF_HMMO,'(a,<NUMPLPD>i6)')  'HMMCNTL number of years in each planning period: ',(PLPDYRS(I),I=1,NUMPLPD)

      NEW = .FALSE.
      FILENM='HMMCNTL'
      HF_HMM = FILE_MGR('C',FILENM,NEW)


      FILENM = 'HMMINDAT'
      NEW = .FALSE.
      HF_HMM = FILE_MGR('O',FILENM,NEW)
      rec_cnt=0
      DO K = 1 , (MNUMCR-2) * HMKT
        READ(HF_HMM,*)I,J,VMT(I,J),AREA(I,J),CITIES(I,J)
        write(HF_HMMO,*)'HMMINDAT,I,J,VMT,AREA,CITIES',I,J,VMT(I,J),AREA(I,J),CITIES(I,J)
        rec_cnt = rec_cnt + 1
        !write(HF_HMMO,*)'HMMINDAT, just read record #',rec_cnt
      ENDDO

      DO K2=1,LASTYR
        DO I=1,MNUMCR-2
          TOTS(I)=VMT(I,1)+VMT(I,2)+VMT(I,3)
          DO J=1,3
            MGSDG(I,K2)=QMGAS(I,K2)/(CONFCT(1) / 1000000) /1000
            MGSDM(I,K2)=MGSDG(I,K2)/.365
            MSHARE(I,J)=VMT(I,J)/TOTS(I)
          ENDDO
        ENDDO
      ENDDO

      HIYR=1
      TMP1=1
      READ(HF_HMM,*)RDDATA,YRDOLP,NUMBLOCKS
         rec_cnt=rec_cnt+1
         write(HF_HMMO,*)'HMMINDAT, just read record #',rec_cnt
!      YRDOLP = 2005
      write(HF_HMMO,*)'HMMINDAT: Year of Production cost data is',YRDOLP
      DO K2=1,NUMBLOCKS
        write(HF_HMMO,*)'READING LOOP',K2,'  TMP1=',TMP1
        READ(HF_HMM,*)RDDATA
3204    FORMAT(I2,1x,A200)
        READ(HF_HMM,*)RDDATA
        READ(HF_HMM,*)RDDATA
        READ(HF_HMM,*)RDDATA
         write (HF_HMMO,*)'Reading HMMIN-central, pchtch=',pchtch
        DO K=1,PCHTCH
            READ(HF_HMM,*)RDDATA,HIYR,PCCT(HIYR,K),PCFOM(HIYR,K),PCVOM(HIYR,K), &
            PCEFF(HIYR,K),PCELC(HIYR,K),PCESALE(HIYR,K),PCSSZ(HIYR,K),PCSFC(HIYR,K), &
            TCSZ1(HIYR,K),TCSZ2(HIYR,K),PCUTZ(HIYR,K),PCLIF(HIYR,K),PCFCP(HIYR,K)

            write(HF_HMMO,'(a,4i6,12f12.2,1i6)')'Reading HMMINC1,k2,k,hiyr,..:', &
            K2,K,HIYR,TMP1,           PCCT(HIYR,K),PCFOM(HIYR,K),PCVOM(HIYR,K), &
            PCEFF(HIYR,K),PCELC(HIYR,K),PCESALE(HIYR,K),PCSSZ(HIYR,K),PCSFC(HIYR,K), &
            TCSZ1(HIYR,K),TCSZ2(HIYR,K),PCUTZ(HIYR,K),PCFCP(HIYR,K),PCLIF(HIYR,K)

           IF (HIYR.GT.1) THEN
             TMP2=HIYR-TMP1
             TDIF2=PCCT(HIYR,K)-PCCT(TMP1,K)
             TDIF3=TDIF2/TMP2
             TDIF2=PCFOM(HIYR,K)-PCFOM(TMP1,K)
             TDIF4=TDIF2/TMP2
             TDIF2=PCVOM(HIYR,K)-PCVOM(TMP1,K)
             TDIF5=TDIF2/TMP2
             TDIF2=PCEFF(HIYR,K)-PCEFF(TMP1,K)
             TDIF6=TDIF2/TMP2
             TDIF2=PCSSZ(HIYR,K)-PCSSZ(TMP1,K)
             TDIF7=TDIF2/TMP2
             TDIF2=PCSFC(HIYR,K)-PCSFC(TMP1,K)
             TDIF8=TDIF2/TMP2
             TDIF2=PCUTZ(HIYR,K)-PCUTZ(TMP1,K)
             TDIF9=TDIF2/TMP2
             TDIF2=PCLIF(HIYR,K)-PCLIF(TMP1,K)
             TDIF10=TDIF2/TMP2
             !TDIF2=PCFRF(HIYR,K)-PCFRF(TMP1,K)
             !TDIF11=TDIF2/TMP2
             TDIF2=PCFCP(HIYR,K)-PCFCP(TMP1,K)
             TDIF12=TDIF2/TMP2
             TDIF2=PCELC(HIYR,K)-PCELC(TMP1,K)
             TDIF13=TDIF2/TMP2
             TDIF2=TCSZ1(HIYR,K)-TCSZ1(TMP1,K)
             TDIF14=TDIF2/TMP2
             TDIF2=TCSZ2(HIYR,K)-TCSZ2(TMP1,K)
             TDIF15=TDIF2/TMP2
             TDIF2=PCESALE(HIYR,K)-PCESALE(TMP1,K)
             TDIF16=TDIF2/TMP2
             DO K3=TMP1+1,HIYR-1
               PCCT(K3,K) = PCCT(K3-1,K)+TDIF3
               PCFOM(K3,K) = PCFOM(K3-1,K)+TDIF4
               PCVOM(K3,K) = PCVOM(K3-1,K)+TDIF5
               PCEFF(K3,K) = PCEFF(K3-1,K)+TDIF6
               PCSSZ(K3,K) = PCSSZ(K3-1,K)+TDIF7
               PCSFC(K3,K) = PCSFC(K3-1,K)+TDIF8
               PCUTZ(K3,K) = PCUTZ(K3-1,K)+TDIF9
               PCLIF(K3,K) = PCLIF(K3-1,K)+TDIF10
               !PCFRF(K3,K) = PCFRF(K3-1,K)+TDIF11
               PCFCP(K3,K) = PCFCP(K3-1,K)+TDIF12
               PCELC(K3,K) = PCELC(K3-1,K)+TDIF13
               TCSZ1(K3,K) = TCSZ1(K3-1,K)+TDIF14
               TCSZ2(K3,K) = TCSZ2(K3-1,K)+TDIF15
               PCESALE(K3,K) = PCESALE(K3-1,K)+TDIF16
               if (curitr.eq.1) &
                 write(HF_HMMO,'(a,4i6,6f12.2)')'***Interpolating,k2,k,hiyr,yr,ccst,fom,vom,sz1,sz2:',  &
                 K2,K,HIYR,K3,PCCT(HIYR,K),PCFOM(HIYR,K),PCVOM(HIYR,K),TCSZ1(K3,K),TCSZ2(K3,K),PCFCP(K3,K)
             ENDDO
           ENDIF
        ENDDO   ! PCHTCH
!
        READ(HF_HMM,*)RDDATA
        write (HF_HMMO,*)'Reading HMMIN-citygate, pghtch=',pghtch
        DO K=1,PGHTCH-1
          READ(HF_HMM,*)RDDATA,HIYR,PGCT(HIYR,K),PGFOM(HIYR,K),PGVOM(HIYR,K), &
           PGEFF(HIYR,K),PGELC(HIYR,K),PGESALE(HIYR,K),PGSSZ(HIYR,K),PGSFC(HIYR,K), &
           TGSZ1(HIYR,K),TGSZ2(HIYR,K),PGUTZ(HIYR,K),PGLIF(HIYR,K),PGFCP(HIYR,K)

               if (curitr.eq.1) &
           write(HF_HMMO,'(a,3i6,5f12.2,i6)')'Reading HMMING,k2,k,hiyr,ccst,fom,vom,fcp,elc:',  &
            K2,K,HIYR,PGCT(HIYR,K),PGFOM(HIYR,K),PGVOM(HIYR,K),PGFCP(HIYR,K),PGESALE(HIYR,K),  &
            PGLIF(HIYR,K)

          ! PGCT(HIYR,K)=PGCT(HIYR,K)/GDPFIX
          ! PGFOM(HIYR,K)=PGFOM(HIYR,K)/GDPFIX
          ! PGVOM(HIYR,K)=PGVOM(HIYR,K)/GDPFIX
            IF (HIYR.GT.1) THEN
             TMP2=HIYR-TMP1
             TDIF2=PGCT(HIYR,K)-PGCT(TMP1,K)
             TDIF3=TDIF2/TMP2
             TDIF2=PGFOM(HIYR,K)-PGFOM(TMP1,K)
             TDIF4=TDIF2/TMP2
             TDIF2=PGVOM(HIYR,K)-PGVOM(TMP1,K)
             TDIF5=TDIF2/TMP2
             TDIF2=PGEFF(HIYR,K)-PGEFF(TMP1,K)
             TDIF6=TDIF2/TMP2
             TDIF2=PGSSZ(HIYR,K)-PGSSZ(TMP1,K)
             TDIF7=TDIF2/TMP2
             TDIF2=PGSFC(HIYR,K)-PGSFC(TMP1,K)
             TDIF8=TDIF2/TMP2
             TDIF2=PGUTZ(HIYR,K)-PGUTZ(TMP1,K)
             TDIF9=TDIF2/TMP2
             TDIF2=PGLIF(HIYR,K)-PGLIF(TMP1,K)
             TDIF10=TDIF2/TMP2
               if (curitr.eq.1) &
                 write(HF_HMMO,'(a,4i6,4i6)')  &
                 'Reading HMMIN-citygate, interp loop1,tech,hiyr,tmp1,tmp2,pglif(hiyr),pglif(tmp1),tdif2,tdif3',  &
                  K,HIYR,TMP1,TMP2,PGLIF(HIYR,K),PGLIF(TMP1,K),TDIF2,TDIF3
             !TDIF2=PGFRF(HIYR,K)-PGFRF(TMP1,K)
             !TDIF11=TDIF2/TMP2
             TDIF2=PGFCP(HIYR,K)-PGFCP(TMP1,K)
             TDIF12=TDIF2/TMP2
             TDIF2=PGELC(HIYR,K)-PGELC(TMP1,K)
             TDIF13=TDIF2/TMP2
             TDIF2=TGSZ1(HIYR,K)-TGSZ1(TMP1,K)
             TDIF14=TDIF2/TMP2
             TDIF2=TGSZ2(HIYR,K)-TGSZ2(TMP1,K)
             TDIF15=TDIF2/TMP2
             TDIF2=PGESALE(HIYR,K)-PGESALE(TMP1,K)
             TDIF16=TDIF2/TMP2
             DO K3=TMP1+1,HIYR-1
               PGCT(K3,K) = PGCT(K3-1,K)+TDIF3
               PGFOM(K3,K) = PGFOM(K3-1,K)+TDIF4
               PGVOM(K3,K) = PGVOM(K3-1,K)+TDIF5
               PGEFF(K3,K) = PGEFF(K3-1,K)+TDIF6
               PGSSZ(K3,K) = PGSSZ(K3-1,K)+TDIF7
               PGSFC(K3,K) = PGSFC(K3-1,K)+TDIF8
               PGUTZ(K3,K) = PGUTZ(K3-1,K)+TDIF9
               PGLIF(K3,K) = PGLIF(K3-1,K)+TDIF10
               !PGFRF(K3,K) = PGFRF(K3-1,K)+TDIF11
               PGFCP(K3,K) = PGFCP(K3-1,K)+TDIF12
               PGELC(K3,K) = PGELC(K3-1,K)+TDIF13
               TGSZ1(K3,K) = TGSZ1(K3-1,K)+TDIF14
               TGSZ2(K3,K) = TGSZ2(K3-1,K)+TDIF15
               PGESALE(K3,K) = PGESALE(K3-1,K)+TDIF16
             ENDDO
            ENDIF
        ENDDO
!
        READ(HF_HMM,*)RDDATA
        write (HF_HMMO,*)'Reading HMMIN-forecourt, pfhtch=',pfhtch
        DO K=1,PFHTCH
               if (curitr.eq.1) &
            write(HF_HMMO,*)'Reading HMMIN-forecourt, tech loop',K
          READ(HF_HMM,*)RDDATA,HIYR,PFCT(HIYR,K),PFFOM(HIYR,K),PFVOM(HIYR,K), &
           PFEFF(HIYR,K),PFELC(HIYR,K),PFESALE(HIYR,K),PFSFC(HIYR,K), &
           TFSZ1(HIYR,K),TFSZ2(HIYR,K),PFUTZ(HIYR,K),PFLIF(HIYR,K),PFFCP(HIYR,K)

               if (curitr.eq.1) &
            write(HF_HMMO,'(a,3i6,6f12.2)')'Reading HMMINF,k2,k,hiyr,ccst,fom,vom,fcp,elc,sfc:',  &
             K2,K,HIYR,PFCT(HIYR,K),PFFOM(HIYR,K),PFVOM(HIYR,K),PFFCP(HIYR,K),PFESALE(HIYR,K), &
             PFSFC(HIYR,K)

          ! PFCT(HIYR,K)=PFCT(HIYR,K)/GDPFIX
          ! PFFOM(HIYR,K)=PFFOM(HIYR,K)/GDPFIX
          ! PFVOM(HIYR,K)=PFVOM(HIYR,K)/GDPFIX
            IF (HIYR.GT.1) THEN
             TMP2=HIYR-TMP1
             TDIF2=PFCT(HIYR,K)-PFCT(TMP1,K)
             TDIF3=TDIF2/TMP2
               if (curitr.eq.1) &
                 write(HF_HMMO,'(a,4i6,4f10.1)')  &
                 'Reading HMMIN-forecourt, interp loop1,tech,hiyr,tmp1,tmp2,pfct(hiyr),pfct(tmp1),tdif2,tdif3',  &
                  K,HIYR,TMP1,TMP2,PFCT(HIYR,K),PFCT(TMP1,K),TDIF2,TDIF3
             TDIF2=PFFOM(HIYR,K)-PFFOM(TMP1,K)
             TDIF4=TDIF2/TMP2
             TDIF2=PFVOM(HIYR,K)-PFVOM(TMP1,K)
             TDIF5=TDIF2/TMP2
             TDIF2=PFEFF(HIYR,K)-PFEFF(TMP1,K)
             TDIF6=TDIF2/TMP2
             TDIF2=PFSFC(HIYR,K)-PFSFC(TMP1,K)
             TDIF8=TDIF2/TMP2
             TDIF2=PFUTZ(HIYR,K)-PFUTZ(TMP1,K)
             TDIF9=TDIF2/TMP2
             TDIF2=PFLIF(HIYR,K)-PFLIF(TMP1,K)
             TDIF10=TDIF2/TMP2
             !TDIF2=PFFRF(HIYR,K)-PFFRF(TMP1,K)
             !TDIF11=TDIF2/TMP2
             TDIF2=PFFCP(HIYR,K)-PFFCP(TMP1,K)
             TDIF12=TDIF2/TMP2
             TDIF2=PFELC(HIYR,K)-PFELC(TMP1,K)
             TDIF13=TDIF2/TMP2
             TDIF2=TFSZ1(HIYR,K)-TFSZ1(TMP1,K)
             TDIF14=TDIF2/TMP2
             TDIF2=TFSZ2(HIYR,K)-TFSZ2(TMP1,K)
             TDIF15=TDIF2/TMP2
             TDIF2=PFESALE(HIYR,K)-PFESALE(TMP1,K)
             TDIF16=TDIF2/TMP2
             DO K3=TMP1+1,HIYR-1
               PFCT(K3,K) = PFCT(K3-1,K)+TDIF3
               if (curitr.eq.1) &
                 write(HF_HMMO,'(a,4i6,2f12.1)')  &
                 'Reading HMMIN-forecourt, interp loop2,tech,tmp1+1,hiyr-1,tdif3,pfct',  &
                  K3,K,TMP1+1,HIYR-1,TDIF3,PFCT(K3,K)
               PFFOM(K3,K) = PFFOM(K3-1,K)+TDIF4
               PFVOM(K3,K) = PFVOM(K3-1,K)+TDIF5
               PFEFF(K3,K) = PFEFF(K3-1,K)+TDIF6
               PFSFC(K3,K) = PFSFC(K3-1,K)+TDIF8
               PFUTZ(K3,K) = PFUTZ(K3-1,K)+TDIF9
               PFLIF(K3,K) = PFLIF(K3-1,K)+TDIF10
               !PFFRF(K3,K) = PFFRF(K3-1,K)+TDIF11
               PFFCP(K3,K) = PFFCP(K3-1,K)+TDIF12
               PFELC(K3,K) = PFELC(K3-1,K)+TDIF13
               TFSZ1(K3,K) = TFSZ1(K3-1,K)+TDIF14
               TFSZ2(K3,K) = TFSZ2(K3-1,K)+TDIF15
               PFESALE(K3,K) = PFESALE(K3-1,K)+TDIF16
            ENDDO
            ENDIF
        ENDDO
           IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1)  &
             write(HF_HMMO,'(a,i6,14f12.2)')'PFCT afterInterp,yr,:', curiyr,(PFCT(curiyr,L),L=1,PFHTCH)
!
        ! Read in the dispensing data
        READ(HF_HMM,*)RDDATA
        write (HF_HMMO,*)'Reading HMMIN-dispensing, sohtch=',sohtch
        DO K=1,SOHTCH-1
               if (curitr.eq.1) &
            write(HF_HMMO,*)'Reading HMMIN-forecourt, tech loop',K
          READ(HF_HMM,*)RDDATA,HIYR,SOCT(HIYR,K),SOFOM(HIYR,K),SOVOM(HIYR,K), &
           SOELC(HIYR,K),SOUTZ(HIYR,K),SOLIF(HIYR,K)

               if (curitr.eq.1) &
            write(HF_HMMO,'(a,3i6,6f12.2)')'Reading HMMINF,k2,k,hiyr,ccst,fom,vom,fcp,elc,sfc:',  &
             K2,K,HIYR,SOCT(HIYR,K),SOFOM(HIYR,K),SOVOM(HIYR,K)

            IF (HIYR.GT.1) THEN
             TMP2=HIYR-TMP1
             TDIF2=SOCT(HIYR,K)-SOCT(TMP1,K)
             TDIF3=TDIF2/TMP2
               if (curitr.eq.1) &
                 write(HF_HMMO,'(a,4i6,4f10.1)')  &
                 'Reading HMMIN-forecourt, interp loop1,tech,hiyr,tmp1,tmp2,SOct(hiyr),SOct(tmp1),tdif2,tdif3',  &
                  K,HIYR,TMP1,TMP2,SOCT(HIYR,K),SOCT(TMP1,K),TDIF2,TDIF3

             TDIF2=SOFOM(HIYR,K)-SOFOM(TMP1,K)
             TDIF4=TDIF2/TMP2
             TDIF2=SOVOM(HIYR,K)-SOVOM(TMP1,K)
             TDIF5=TDIF2/TMP2
             TDIF2=SOUTZ(HIYR,K)-SOUTZ(TMP1,K)
             TDIF9=TDIF2/TMP2
             TDIF2=SOLIF(HIYR,K)-SOLIF(TMP1,K)
             TDIF10=TDIF2/TMP2
             TDIF2=SOELC(HIYR,K)-SOELC(TMP1,K)
             TDIF13=TDIF2/TMP2

             DO K3=TMP1+1,HIYR-1
               SOCT(K3,K) = SOCT(K3-1,K)+TDIF3
               if (curitr.eq.1) &
                 write(HF_HMMO,'(a,4i6,2f12.1)')  &
                 'Reading HMMIN-forecourt, interp loop2,tech,tmp1+1,hiyr-1,tdif3,SOct',  &
                  K3,K,TMP1+1,HIYR-1,TDIF3,SOCT(K3,K)
               SOFOM(K3,K) = SOFOM(K3-1,K)+TDIF4
               SOVOM(K3,K) = SOVOM(K3-1,K)+TDIF5
               SOUTZ(K3,K) = SOUTZ(K3-1,K)+TDIF9
               SOLIF(K3,K) = SOLIF(K3-1,K)+TDIF10
               SOELC(K3,K) = SOELC(K3-1,K)+TDIF13
            ENDDO
            ENDIF
        ENDDO

        ! Read in the liquifier elec consumption data
        READ(HF_HMM,*)RDDATA
        write (HF_HMMO,*)'Reading HMMIN-Liquifier'

        READ(HF_HMM,*)RDDATA,HIYR,LIQELC(HIYR)

        if (curitr.eq.1) &
          write(HF_HMMO,'(a,2i6,1f12.2)')'Reading HMMIN-Liq,k2,hiyr,liqelc:',  &
              K2,HIYR,LIQELC(HIYR)

        IF (HIYR.GT.1) THEN

           TMP2 = HIYR - TMP1

           TDIF2  = LIQELC(HIYR) - LIQELC(TMP1)
           TDIF13 = TDIF2/TMP2

           DO K3=TMP1+1,HIYR-1

             LIQELC(K3) = LIQELC(K3-1) + TDIF13

             if (curitr.eq.1) &
               write(HF_HMMO,'(a,3i6,1f12.1)')  &
               'Reading HMMIN-Liq,k3,tmp1+1,hiyr-1,liqelc(k3): ', &
                K3,TMP1+1,HIYR-1,LIQELC(K3)

           ENDDO

        ENDIF

        TMP1 = HIYR

      ENDDO ! K2
!
!
!  debug for new variable
!
      if (curitr.eq.1) then
         do yr=11,LASTYR
             write(HF_HMMO,'(a,i6,14f12.0)')'PCSIZ,yr,:',  &
               yr,(PCSIZ(yr,K),K=1,PCHTCH)
             write(HF_HMMO,'(a,i6,14f12.2)')'PCCT,yr,:',  &
               yr,(PCCT(yr,K),K=1,PCHTCH)
             write(HF_HMMO,'(a,i6,14f12.2)')'PCESALE,yr,:',  &
               yr,(PCESALE(yr,K),K=1,PCHTCH)
             write(HF_HMMO,'(a,i6,14f12.2)')'PGCT,yr,:',  &
               yr,(PGCT(yr,K),K=1,PGHTCH)
             write(HF_HMMO,'(a,i6,14f12.2)')'PFCT,yr,:',  &
               yr,(PFCT(yr,K),K=1,PFHTCH)
             write(HF_HMMO,'(a,i6,14i6)')'PGLIF,yr,:',  &
               yr,(PGLIF(yr,K),K=1,PGHTCH)
             write(HF_HMMO,'(a,i6,14f12.0)')'PGSIZ,yr,:',  &
               yr,(PGSIZ(yr,K),K=1,PGHTCH)
             write(HF_HMMO,'(a,i6,14f12.0)')'PFSIZ,yr,:',  &
               yr,(PFSIZ(yr,K),K=1,PFHTCH)
             write(HF_HMMO,'(a,i6,14f12.2)')'PGESALE,yr,:',  &
               yr,(PGESALE(yr,K),K=1,PGHTCH)
             write(HF_HMMO,'(a,i6,14f12.2)')'PFESALE,yr,:',  &
               yr,(PFESALE(yr,K),K=1,PFHTCH)
         enddo
      endif

      READ(HF_HMM,*)RDDATA,YRDOLD
!      YRDOLD = 2005
      write(HF_HMMO,*)'HMMINDAT: Year of Delivery cost data is',YRDOLD

!     Read in regression coefficients for cost equations
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(P1(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(P2(J),J=1,12)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(P3(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(P4(J),J=1,12)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(P7(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(P8(J),J=1,12)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(L1(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(L2(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(L3(J),J=1,12)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(L5(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(L6(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(L7(J),J=1,12)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(L9(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(L10(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(L11(J),J=1,12)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(L13(J),J=1,12)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(G7(J),J=1,12)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(G9(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(G10(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(G11(J),J=1,12)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(G13(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(G14(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(G15(J),J=1,12)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA,(G17(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(G1(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(G2(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(G3(J),J=1,12)
      READ(HF_HMM,*)RDDATA,(G4(J),J=1,12)

      ! Read in GPRA multipliers
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)(GPRAMULT(J),J=1,3)
      write(HF_HMMO,'(a,3f12.6)') 'GPRAMULT ',(GPRAMULT(J),J=1,3)

      ! Read in state H2 taxes by census region (in 2005 dollars)
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)RDDATA
      READ(HF_HMM,*)(STATETAX(J),J=1,MNUMCR-2)
      WRITE(HF_HMMO,*) '~~STATE TAX:  ',(STATETAX(J),J=1,MNUMCR-2)

      ! Read in federal H2 tax (in NOMINAL dollars)
      READ(HF_HMM,*)RDDATA,FEDTAX
      WRITE(HF_HMMO,*) '~~FED TAX:  ',FEDTAX

      ! Read in fuel availability crossover point for forecourt production
      ! station size.
      READ(HF_HMM,*) RDDATA
      READ(HF_HMM,*) FA_SWITCH


      ! debug
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-P1  ',(P1(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-P2  ',(P2(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-P3  ',(P3(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-P4  ',(P4(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-P7  ',(P7(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-P8  ',(P8(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-L1  ',(L1(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-L2  ',(L2(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-L3  ',(L3(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-L5  ',(L5(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-L6  ',(L6(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-L7  ',(L7(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-L9  ',(L9(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-L10 ',(L10(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-L11 ',(L11(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-L13 ',(L13(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G7  ',(G7(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G9  ',(G9(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G10 ',(G10(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G11 ',(G11(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G13 ',(G13(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G14 ',(G14(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G15 ',(G15(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G17 ',(G17(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G1  ',(G1(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G2  ',(G2(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G3  ',(G3(J),J=1,12)
      write(HF_HMMO,'(a,12f12.6)') 'REGCOEF-G4  ',(G4(J),J=1,12)

!      DO K=1,MNUMCR-2
!         READ(HF_HMM,*)(CITIES(K,I),I=1,3)
!         if (curitr.eq.1) write(HF_HMMO,'(a,4i7)')'CITIESdbg,rgn:',K,  &
!            (CITIES(K,I),I=1,3)
!      ENDDO

!
!     convert elec consumption and sales to btu
!
      do yr=1,LASTYR
        do k=1,PCHTCH
          PCELC(yr,K) = PCELC(yr,K) * 3412.0
          PCESALE(yr,K) = PCESALE(yr,K) * 3412.0
        enddo
        do k=1,PGHTCH
          PGELC(yr,K) = PGELC(yr,K) * 3412.0
          PGESALE(yr,K) = PGESALE(yr,K) * 3412.0
        enddo
        do k=1,PFHTCH
          PFELC(yr,K) = PFELC(yr,K) * 3412.0
          PFESALE(yr,K) = PFESALE(yr,K) * 3412.0
        enddo
      enddo
!
!
!  debug for new variable after btu conversion
!
         do yr=11,LASTYR
             write(HF_HMMO,'(a,i6,7f12.2)')'PCELCinBTU,yr,:',  &
               yr,(PCELC(yr,K),K=1,PCHTCH)
             write(HF_HMMO,'(a,i6,7f12.2)')'PGELCinBTU,yr,:',  &
               yr,(PGELC(yr,K),K=1,PGHTCH-1)
             write(HF_HMMO,'(a,i6,7f12.2)')'PFELCinBTU,yr,:',  &
               yr,(PFELC(yr,K),K=1,PFHTCH)
             write(HF_HMMO,'(a,i6,7f12.2)')'PCESALEinBTU,yr,:',  &
               yr,(PCESALE(yr,K),K=1,PCHTCH)
             write(HF_HMMO,'(a,i6,7f12.2)')'PGESALEinBTU,yr,:',  &
               yr,(PGESALE(yr,K),K=1,PGHTCH-1)
             write(HF_HMMO,'(a,i6,7f12.2)')'PFESALEinBTU,yr,:',  &
               yr,(PFESALE(yr,K),K=1,PFHTCH)
         enddo
!
      FILENM = 'HMMINDAT'
      NEW = .FALSE.
      HF_HMM = FILE_MGR('C',FILENM,NEW)


!assign demands from restart file (before transportation model is run )


      DO I = 1 , MNUMCR
!       DO J = 1 , HMKT
!         DO K=1,MNUMYR
!           IF (J .EQ. 1) HDMD(I,K,J) = QH1TR(I,K)
!           IF (J .EQ. 2) HDMD(I,K,J) = QH2TR(I,K)
!           IF (J .EQ. 3) HDMD(I,K,J) = QH3TR(I,K)
!         MGPD(K,I,J) = HDMD(I,K,J) / 1000.0 / (CONFCT(1) / 1000000.0) / 0.365
!         ENDDO
!         IF (HDMD(I,LASTYR-5,J).GT.0) THEN
!           GRW = DBLE(HDMD(I,LASTYR,J) / HDMD(I,LASTYR-5,J)) ** (DBLE(1.0) / DBLE(5.0))
!         ELSE
!           GRW = 0.0
!         ENDIF
!         DO K = LASTYR+1 , HMM_MAX_HYR
!            HDMD(I,K,J) = HDMD(I,K-1,J) * GRW
!         MGPD(K,I,J) = HDMD(I,K,J) / 1000.0 / (CONFCT(1) / 1000000.0) / 0.365
!     WRITE(6,7849)'DBGH1 ',I,J,HDMD(I,LASTYR,J),HDMD(I,LASTYR-5,J),GRW,HDMD(I,K,J)
7849  FORMAT(A8,1x,2(I4,1x),4(F12.3,1x))
!         END DO
!       END DO


          DO K=1,MNUMYR
            ZPELIN(I,K)=PELIN(I,K)
          ENDDO
          IF (PELIN(I,LASTYR-5).GT.0) THEN
            GRW = DBLE(PELIN(I,LASTYR) / PELIN(I,LASTYR-5)) ** (DBLE(1.0) / DBLE(5.0))
          ELSE
            GRW=0.0
          ENDIF
          DO K = LASTYR+1 , HMM_MAX_HYR
            ZPELIN(I,K) = ZPELIN(I,K-1) * GRW
          END DO

          DO K=1,MNUMYR
            ZPNGEL(I,K)=PNGEL(I,K)
          ENDDO
          IF (PNGEL(I,LASTYR-5).GT.0) THEN
            GRW = DBLE(PNGEL(I,LASTYR) / PNGEL(I,LASTYR-5)) ** (DBLE(1.0) / DBLE(5.0))
          ELSE
            GRW = 0.0
          ENDIF
          DO K = LASTYR+1 , HMM_MAX_HYR
            ZPNGEL(I,K) = ZPNGEL(I,K-1) * GRW
          END DO

          DO K=1,MNUMYR
            ZPCLEL(I,K)=PCLEL(I,K)
          ENDDO
          IF (PCLEL(I,LASTYR-5).GT.0) THEN
            GRW = DBLE(PCLEL(I,LASTYR) / PCLEL(I,LASTYR-5)) ** (DBLE(1.0) / DBLE(5.0))
          ELSE
            GRW = 0.0
          ENDIF
          DO K = LASTYR+1 , HMM_MAX_HYR
            ZPCLEL(I,K) = ZPCLEL(I,K-1) * GRW
          END DO

          DO K=1,MNUMYR
            ZPBMET(I,K)=PBMET(I,K,1)
          ENDDO
          IF (PBMET(I,LASTYR-5,1).GT.0) THEN
            GRW = DBLE(PBMET(I,LASTYR,1) / PBMET(I,LASTYR-5,1)) ** (DBLE(1.0) / DBLE(5.0))
          ELSE
            GRW = 0.0
          ENDIF
          DO K = LASTYR+1 , HMM_MAX_HYR
            ZPBMET(I,K) = ZPBMET(I,K-1) * GRW
          END DO

          DO K=1,MNUMYR
            ZPUREL(I,K)=PUREL(I,K)
          ENDDO
          IF (PUREL(I,LASTYR-5).GT.0) THEN
            GRW = DBLE(PUREL(I,LASTYR) / PUREL(I,LASTYR-5)) ** (DBLE(1.0) / DBLE(5.0))
          ELSE
            GRW = 0.0
          ENDIF
          DO K = LASTYR+1 , HMM_MAX_HYR
            ZPUREL(I,K) = ZPUREL(I,K-1) * GRW
          END DO

          DO K=1,MNUMYR
            ZPELCM(I,K)=PELCM(I,K)
          ENDDO
          IF (PELCM(I,LASTYR-5).GT.0) THEN
            GRW = DBLE(PELCM(I,LASTYR) / PELCM(I,LASTYR-5)) ** (DBLE(1.0) / DBLE(5.0))
          ELSE
            GRW = 0.0
          ENDIF
          DO K = LASTYR+1 , HMM_MAX_HYR
            ZPELCM(I,K) = ZPELCM(I,K-1) * GRW
          END DO

          DO K=1,MNUMYR
            ZPNGIN(I,K)=PNGIN(I,K)
          ENDDO
          IF (PNGIN(I,LASTYR-5).GT.0) THEN
            GRW = DBLE(PNGIN(I,LASTYR) / PNGIN(I,LASTYR-5)) ** (DBLE(1.0) / DBLE(5.0))
          ELSE
           GRW = 0.0
          ENDIF
          DO K = LASTYR+1 , HMM_MAX_HYR
            ZPNGIN(I,K) = ZPNGIN(I,K-1) * GRW
          END DO


          DO K=1,MNUMYR
            ZJNGEL(K)=JNGEL(K)
          ENDDO
          IF (JNGEL(LASTYR-5).GT.0) THEN
            GRW = DBLE(JNGEL(LASTYR) / JNGEL(LASTYR-5)) ** (DBLE(1.0) / DBLE(5.0))
          ELSE
            GRW = 0.0
          ENDIF

          DO K = LASTYR+1 , HMM_MAX_HYR
            ZJNGEL(K) = ZJNGEL(K-1) * GRW
          END DO

          DO K=1,MNUMYR
            ZJCLEL(K)=JCLEL(K)
          ENDDO
          IF (JCLEL(LASTYR-5).GT.0) THEN
            GRW = DBLE(JCLEL(LASTYR) / JCLEL(LASTYR-5)) ** (DBLE(1.0) / DBLE(5.0))
          ELSE
            GRW = 0.0
          ENDIF
          DO K = LASTYR+1 , HMM_MAX_HYR
            ZJCLEL(K) = ZJCLEL(K-1) * GRW
          END DO

      END DO

!      CLOSE(33)
!      CLOSE(34)
!      CLOSE(35)

      RETURN
      END SUBROUTINE GETDAT


      SUBROUTINE GETCST(CINX,MINX)
! This subroutine calculates levelized costs

      IMPLICIT NONE

! New derived variable declarations for levelized delivered cost equations
      INTEGER DMDSTEP                   ! 1 IF Q<25000, ELSE 0
      REAL*8  Q                         ! Average daily hydrogen demand in kg over entire planning horizon
      REAL*8  Q_INC                     ! Incremental 20-year levelized daily hydrogen demand in kg
      REAL*8  DMD_INC                   ! Incremental FIRST-PERIOD daily hydrogen demand in kg
      REAL*8  LOGQ                      ! log(Q) if Q>1, else 0
      REAL*8  SLM                       ! service line miles
      REAL*8  TLM                       ! trunk line miles
      REAL*8  LOGMILES                  ! log(miles) if miles>1, else 0
      REAL*8  TRUCKS                    ! # of trucks required for a given Q and area
      REAL*8  LOGTRUCKS                 ! log(TRUCKS) if TRUCKS>1, else 0
      REAL*8  TRUCK,TERMINAL,LIQUIFIER  ! temp variables for calculating truck costs
      REAL*8  PSI2700A,PSI7000A         ! temp variables used in gas truck capital cost calc
      REAL*8  PSI2700B,PSI7000B
      REAL*8  TMPAREA
      REAL*8  TMPSTATIONS

      ! Temp cost variables
      REAL*8  ANNCCST                   ! annual capital cost for a given tech and year
      REAL*8  ANNFUEL                   ! annual fuel cost for a given tech and year
      REAL*8  ANNOM                     ! annual O&M cost for a given tech and year

      REAL*8  FCR_TRUCK                 ! fixed charge rate for trucks
      REAL*8  FCR_LIQUIFIER             ! fixed charge rate for liquifiers
      REAL*8  FCR_PIPE                  ! fixed charge rate for pipelines

      INTEGER I,J,K,TMP,J2,K2,IYR,K3,CINX,MINX,YRIDX
      INTEGER*4 HIYR,TMP1,TMP2,TMP3
      REAL*8 CRF,PWF,PVV,PVBV,CRF2
      REAL*8 VAL,TOTA(MNUMCR)
      REAL*8 STDCST,CC1,FDIST,CC2,CC3,ASPD,APKG,FUT,TPTH,TBOIL, &
            CC4,AFE,ALFO,NTPY,TKD,MILS,TDTH,TLTH,TDT,OH,NT,NTR,AEU,CS1
      REAL*8 STATNS(SOHTCH),RADIUS(MNUMCR,MNUMYR),XHM(MNUMCR,MNUMYR)
      REAL*8 TEMP1,TEMP2

      REAL*8 ELECFACTOR

      K2 = CURIYR
      I  = CINX
      J  = MINX

      ! This factor is used to adjust the cost of electricity from the fixed price
      !  used to run the econometric equations to the variable price in the NEMS framework.
      !  The factor is (NEMS PRICE in $87/MMBTU)/(ECONOMETRIC PRICE in $87/MMBTU)
      ELECFACTOR = PELIN(I,K2) / ( (0.05/(3412*1.5285))* 1000000 )

      !IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,2i6,1f8.3)')'ELECFACTOR,y,i:', &
      !      K2,I,ELECFACTOR

      DPY           = 0.0
      AVKG          = 0.0
      TOTA(I)       = 0.0
      MTTK(K2,I,J)  = 0.0
      MTTM(K2,I,J)  = 0.0

      DO K=1,PCHTCH
         PCRCC(K2,I,J,K) = 0.0
         PCECY(K2,K) = 0.0
      ENDDO
      DO K=1,PGHTCH
         PGRCC(K2,I,J,K) = 0.0
         PGECY(K2,K) = 0.0
      ENDDO
      DO K=1,PFHTCH
         PFRCC(K2,I,J,K) = 0.0
         PFECY(K2,K) = 0.0
      ENDDO

      DO K=1,PCHTCH
         PCLEV(K2,I,J,K)=0.0
         PCLEX(K2,I,J,K)=0.0
      ENDDO
      DO K=1,DOHTCH
         DOLEV(K2,I,J,K)=0.0
         DOLEX(K2,I,J,K)=0.0
      ENDDO
      DO K=1,TCHTCH
         PFLEV(K2,I,J,K)=0.0
         TCLEV(K2,I,J,K)=0.0
         TGLEV(K2,I,J,K)=0.0
         SOLEV(K2,I,J,K)=0.0
         PFLEX(K2,I,J,K)=0.0
         TCLEX(K2,I,J,K)=0.0
         TGLEX(K2,I,J,K)=0.0
         SOLEX(K2,I,J,K)=0.0
      ENDDO
      DO K=1,PGHTCH
         PGLEV(K2,I,J,K)=0.0
         PGLEX(K2,I,J,K)=0.0
      ENDDO
      DMDLEV(K2,I,J,XYR)=0.0

!     Define Cost of Capital

      RORHM = (CODHM + MC_RMTCM10Y(K2) / 100.0) * (1.0 - EQTYHM) + (COEHM + MC_RMTCM10Y(K2) / 100.0) * EQTYHM
      RORTD = (CODTD + MC_RMTCM10Y(K2) / 100.0) * (1.0 - EQTYTD) + (COETD + MC_RMTCM10Y(K2) / 100.0) * EQTYTD
      RORPIP= (CODPIP + MC_RMTCM10Y(K2) / 100.0) * (1.0 - EQTYPIP) + (COEPIP + MC_RMTCM10Y(K2) / 100.0) * EQTYPIP

      FCR_TRUCK     = CRF(RORTD,15)
      FCR_LIQUIFIER = CRF(RORTD,20)
      FCR_PIPE      = CRF(RORPIP,30)

      IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,4i6,3f8.3)')'RORdbg,y,i,r,m,rorhm,rortd,rorpip:', &
            K2,curitr,I,J,RORHM,RORTD,RORPIP
      IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,4i6,3f8.3)')'FCRdbg:',K2,curitr,I,J,FCR_TRUCK, &
            FCR_LIQUIFIER,FCR_PIPE

!     Calculate Average Demand Over Full Planning Horizon

    IF (XYR .EQ. 1) THEN
      D_MGPD = 0.0
      D_HYD  = 0.0
      KG     = 0.0
      DO JYR = 1 , N_FYR
         FYR = JYR + PLPD_SYR(XYR) + K2 - 2
         M_FYR = MIN(FYR , HMM_MAX_HYR)

         D_HYD(JYR)  = HDMD(CINX,M_FYR,MINX) / 1000.0 / (CONFCT(1) / 1000000.0) / 0.365
         D_MGPD(JYR) = MGPD(M_FYR,I,J)

         IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1)  &
         write(HF_HMMO,9995) K2+1989,I,J,XYR,PLPD_SYR(XYR),JYR,FYR,M_FYR,LASTYR,HMM_MAX_HYR, &
                HDMD(CINX,M_FYR,MINX),D_HYD(JYR),MGPD(M_FYR,I,J),D_MGPD(JYR)
9995     FORMAT(1X,"AVGHYDdbg",10(":",I4),5(":",F14.3))

         KG(JYR) = 1.0
      END DO

      PV_HYD   = PVV(D_HYD ,HMM_MAX_FYR,N_FYR,RORHM) * PWF(RORHM,-1)
      PV_KG    = PVV(KG    ,HMM_MAX_FYR,N_FYR,RORHM) * PWF(RORHM,-1)
      AVG_HYD  = PV_HYD  / PV_KG
      AVG_H2D(K2,CINX,MINX) = AVG_HYD

    ELSE
       AVG_HYD=AVG_H2D(K2,CINX,MINX)       ! million kg H2/day
    ENDIF

      WRITE(HF_HMMO,9001) K2+1989,CURITR,CINX,MINX,XYR,N_FYR, &
            AVG_HYD,PV_HYD,PV_KG,D_HYD(1),D_HYD(N_FYR),D_MGPD(1),D_MGPD(N_FYR)
 9001 FORMAT(1X,"AVG_HYD",6(":",I4),7(":",F12.3))

!     Calculate Average Demand For Explicit Time Period

      D_HYD = 0.0
      KG = 0.0
      DO JYR = 1 , PLPDYRS(XYR)
         FYR = JYR + PLPD_SYR(XYR) + K2 - 2
         M_FYR = MIN(FYR , HMM_MAX_HYR)
!         IF (FYR .LE. LASTYR) THEN
            D_HYD(JYR) = MGPD(M_FYR,I,J) * 365.0 / 1000.0
!         ELSE
!            GRW = DBLE(MGPD(LASTYR,I,J) / MGPD(LASTYR-5,I,J)) ** (DBLE(1.0) / DBLE(5.0))
!            D_HYD(JYR) = MGPD(M_FYR,I,J) * GRW ** DBLE(FYR - LASTYR) * 365.0 / 1000.0
!         END IF
         KG(JYR) = 1.0
      END DO

      ! Present value of hydrogen demand over the number of years in this planning period
      PV_HYD = PVV(D_HYD,HMM_MAX_FYR,PLPDYRS(XYR),RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
      PV_KG = PVV(KG,HMM_MAX_FYR,PLPDYRS(XYR),RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

      ! Levelized demand for this planning period in billions of kg per year
      ! ** This is the demand level used for the RHS in the demand constraint.
      DMDLEV(K2,I,J,XYR) = PV_HYD / PV_KG

      ! Ensure that demand does not fall year over year (or else the demand
      !  constraint will not be binding and the marginal price of hydrogen in
      !  the first planning period will be zero.)
      IF (K2.GE.2) THEN
        DMDLEV(K2,I,J,XYR) = MAX(DMDLEV(K2,I,J,XYR),DMDLEV(K2-1,I,J,XYR)*1.001)
      END IF

      IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) WRITE(HF_HMMO,3520) K2+1989,I,J,XYR,DMDLEV(K2,I,J,XYR)
 3520 FORMAT('DMDDB',":",4(I4,":"),F12.6,":")

!
!     convert fuel prices (87$/MMbtu to dataYearDollars/kwh or gallon)
!
      ELECD(I,K2)  = ZPELCM(I,K2) * 3.4126 * MC_JPGDP(YRDOLD-1989)
      MDSG(I,K2)   = PDSTRHWY(I,K2) / 7.20 * MC_JPGDP(YRDOLD-1989)

      ! Calculate the total area and radius of this census region
      TOTA(I)      = (AREA(I,1) + AREA(I,2) + AREA(I,3))
      RADIUS(I,J)  = (TOTA(I) / FUNCPI) ** 0.5

      TEMP1        = MDCTP      ! NIMBY distance
      TEMP2        = CITIES(I,J)

      IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,5i7,f8.0)')'CITIESdbg2,rgn,mkt:',I,J,CITIES(I,J),TEMP2

      VAL          = 500000.0
      IF (MGPD(PLPD_SYR(XYR)+K2-1,I,J) / (CITIES(I,J) * (VAL / 1000000.0)) .GE. 1.0) THEN
         MTTM(K2,I,J) = MDCTP
      ELSEIF (MGPD(PLPD_SYR(XYR)+K2-1,I,J) / (VAL / 1000000.0) .LE. 1.0) THEN
         MTTM(K2,I,J) = RADIUS(I,J)
      ELSE
         XHM(I,J)     = ( LOG(RADIUS(I,J)) - LOG(TEMP1) ) / LOG(TEMP2)
         MTTM(K2,I,J) = RADIUS(I,J) / (MGPD(PLPD_SYR(XYR)+K2-1,I,J) / (VAL / 1000000.0)) ** (XHM(I,J) * 0.9)
         MTTM(K2,I,J) = MAX(MTTM(K2,I,J) , TEMP1)
      ENDIF
      MTTK(K2,I,J) = MTTM(K2,I,J) / CONFCT(4)

      if (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1 .and. J.LT.3) then
         write(HF_HMMO,'(a,3i6,2f12.2,i6,f12.2,i6,17f12.2)')'DISTANCEdbg:',K2,I,J,  &
            TOTA(I),RADIUS(I,J),  &
            MDCTP,TEMP1,CITIES(I,J),TEMP2,VAL,MGPD(PLPD_SYR(XYR)+K2-1,I,J),XHM(I,J),  &
            CONFCT(4),MTTM(K2,I,J),MTTK(K2,I,J),  &
            MGPD(PLPD_SYR(XYR)+K2-1,I,J)/(CITIES(I,J)*(VAL/1000000)),  &
            MGPD(PLPD_SYR(XYR)+K2-1,I,J)/(VAL/1000000),  &
            LOG(RADIUS(I,J)),LOG(TEMP1),LOG(TEMP2),  &
            MGPD(PLPD_SYR(XYR)+K2-1,I,J)/(VAL/1000000),XHM(I,J)*.9,  &
            (MGPD(PLPD_SYR(XYR)+K2-1,I,J)/(VAL/1000000))**(XHM(I,J)*.9),  &
            RADIUS(I,J) / ((MGPD(PLPD_SYR(XYR)+K2-1,I,J)/(VAL/1000000))**(XHM(I,J)*.9))
      endif

      DO K = 1 , PCHTCH

         IF (MAP_PC(K,1) .GT. 0) THEN
            PCRCC(K2,I,J,K) = (PCSSZ(K2,K) / MIN(AVG_HYD * 1000000.0 , TCSZ1(K2,K))) ** (1.0 - PCSFC(K2,K))
            PCECY(K2,K)     = CONFCT(1) / PCEFF(K2,K)
            PCFRF(K2,K)     = CRF(RORHM,PCLIF(K2,K))

            WRITE(HF_HMMO,8937) K2+1989,CURITR,XYR,I,J,K, &
                  TCSZ1(K2,1),TCSZ2(K2,1),PCSSZ(K2,K),PCSFC(K2,K),PCRCC(K2,I,J,K)
8937        FORMAT(1X,"PCRCC_DBG",6(":",I4),5(":",F16.3))
         ENDIF

      ENDDO

      DO K=1,PGHTCH

         IF (MAP_PG(K,1) .GT. 0) THEN
            PGRCC(K2,I,J,K) = (PGSSZ(K2,K) / MIN(AVG_HYD * 1000000.0 , TGSZ1(K2,K))) ** (1.0 - PGSFC(K2,K))
            PGECY(K2,K)     = CONFCT(1) / PGEFF(K2,K)
            PGFRF(K2,K)     = CRF(RORHM , MAX(1 , PGLIF(K2,K)))

            WRITE(HF_HMMO,8938) K2+1989,CURITR,XYR,I,J,K, &
                  TGSZ1(K2,1),TGSZ2(K2,1),PGSSZ(K2,K),PGSFC(K2,K),PGRCC(K2,I,J,K)
8938       FORMAT(1X,"PGRCC_DBG",6(":",I4),5(":",F16.3))

            IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,5i6,f10.2,i6,f10.2)')'PGFRFdbg,y,i,r,m,t,rorhm,pglif,frf:',K2,curitr,I,J,K,RORHM,PGLIF(K2,K),PGFRF(K2,K)
         ENDIF

      ENDDO

      DO K=1,PFHTCH

         IF (MAP_PF(K,1) .GT. 0) THEN
!           IF (K .GT. 1) THEN
               IF (HFAVL(J,I,K2) .GE. FA_SWITCH) THEN
                  PFRCC(K2,I,J,K) = 1.0
               ELSE
                  PFRCC(K2,I,J,K) = (TFSZ1(K2,K) / TFSZ2(K2,K)) ** (1.0 - PFSFC(K2,K))
               ENDIF
!            ENDIF
            PFECY(K2,K) = CONFCT(1) / PFEFF(K2,K)
            PFFRF(K2,K) = CRF(RORHM , PFLIF(K2,K))

            IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,5i6,3f10.2)')'PFRCCdbg,y,i,r,m,t,rcc,sfc:',K2,curitr,&
               I,J,K,PFRCC(K2,I,J,K),PFSFC(K2,K)

            WRITE(HF_HMMO,8939) K2+1989,CURITR,XYR,I,J,K, &
                  TFSZ1(K2,1),TFSZ2(K2,1),HFAVL(J,I,K2),PFSFC(K2,K),PFRCC(K2,I,J,K)
8939        FORMAT(1X,"PFRCC_DBG",6(":",I4),5(":",F16.3))
         ENDIF

      ENDDO
!
!     Levelized Costs in nominal dollars
!
      DO J2 = 1 , 2
         IF (J2 .EQ. 1) THEN        !new

!           Costing of New Central Production Facilities

            DO K = 1 , PCHTCH
               CLT = 1                                                                                ! CONSTRUCTION LEAD TIME -- ACTUAL
               OVRCST = PCCT(K2,K) * PCRCC(K2,I,J,K) / (PCUTZ(K2,K) * 365.0) / HMM_GNPD(YRDOLP-1989)  ! OVERNIGHT COSTS
               PROFILE(1) = 1.0                                                                       ! ANNUAL EXPENDITURE PERCENTAGE
               CAPESC(1) = 1.0                                                                        ! CAPITAL ESCALATION/REAL
               GNPF(1) = HMM_GNPD(MAX(1 , K2 + PLPD_SYR(XYR) - CLT - 1))                              ! GENERAL INFLATION
               INTR = CODHM + MC_RMTCM10Y(K2) / 100.0                                                 ! INTEREST COSTS
               DEBT_F = 1.0 - EQTYHM                                                                  ! CONSTRUCTION DEBT FINANCING
               ROR = RORHM                                                                            ! RATE OF RETURN

               CALL INSTALL_CST(CLT,MAX_LCP,OVRCST,PROFILE,CAPESC,GNPF,INTR,DEBT_F,ROR,TXBOOK,BVBOOK)

               DL2 = PCLIF(K2,K)                                                                      ! DEBT LOAN LIFE <= CONTRACT LIFE
               CL = PCLIF(K2,K)                                                                       ! CONTRACT LIFE
               TL = PCLIF(K2,K)                                                                       ! TAX LIFE ASSOCIATED WITH TECHNOLOGY
               ROE = COEHM + MC_RMTCM10Y(K2) / 100.0                                                  ! ROE
               INT = CODHM + MC_RMTCM10Y(K2) / 100.0                                                  ! INTEREST RATE
               DF2 = 1.0 - EQTYHM                                                                     ! DEBT FRACTION
               TR = 0.38                                                                              ! MARGINAL FEDERAL INCOME TAX RATE
               RATIO = TXBOOK / BVBOOK                                                                ! TX TO BV RATIO

               CALL ANNUITY_FACTOR(TL,CL,DL2,DF2,RATIO,ROE,INT,TR,CAP,HF_HMMO)

               INSTALL_COST(K2,CINX,MINX,MAP_PC(K,1)) = BVBOOK       / HMM_GNPD(K2)
               ANNUITY_COST(K2,CINX,MINX,MAP_PC(K,1)) = CAP * BVBOOK / HMM_GNPD(K2)

               ! debug
               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                   WRITE(HF_HMMO,9999) K2+1989,CURITR,XYR,FYR,I,J,K, &
                       CLT,MAX_LCP,TL,CL,DL2,PCLIF(K2,K), &
                       OVRCST,PROFILE(1),CAPESC(1),GNPF(1),INTR,DEBT_F,ROR,TXBOOK,BVBOOK, &
                       DF2,RATIO,ROE,INT,TR,CAP,PCCT(K2,K),PCRCC(K2,I,J,K),PCUTZ(K2,K)
9999               FORMAT(1X,"PCCAPEXP",13(":",I4),18(":",F9.3))
               ENDIF

               CCST = 0.0
               FOM_ = 0.0
               VOM_ = 0.0
               FUEL = 0.0
               CARB = 0.0
               ELEC = 0.0
               INFL = 0.0
               KG = 0.0
               DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                  FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                  M_FYR = MIN(FYR , LASTYR)
                  CCST(JYR) = CAP * BVBOOK
                  FOM_(JYR) = PCFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                  VOM_(JYR) = PCVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                  ELEC(JYR) = PCELC(M_FYR,K) / 1000000.0 * ZPELIN(I,M_FYR) * HMM_GNPD(FYR)
                  INFL(JYR) = HMM_GNPD(FYR)
                  KG(JYR) = 1.0
               IF (FCRL.EQ.1) THEN
!              IF (K2.GE.36.AND.K2.LE.46) THEN
!                WRITE(6,2420)'DBGD1 ',J2,K,CURITR,K2,I,J,JYR,N_FYR,XYR,PLPD_SYR(XYR), &
!             FYR,M_FYR,LASTYR,CCST(JYR),FOM_(JYR),VOM_(JYR),ELEC(JYR),INFL
2420    FORMAT(A12,1x,13(I4,1x),5(F12.4,1x))
!              ENDIF
               ENDIF
               END DO
               FYR = N_FYR - PLPD_SYR(XYR) + 1
               PV_CCST = PVV(CCST,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
               PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
               PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
               PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
               PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
               PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

               IF (K .LT. 3) then      !NG

                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                     M_FYR = MIN(FYR , LASTYR)
                     FUEL(JYR) = ZPNGEL(I,M_FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                     CARB(JYR) = (1.0 - PCFCP(M_FYR,K)) * ZJNGEL(M_FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                  END DO
                  FYR = N_FYR - PLPD_SYR(XYR) + 1

                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_CARB = PVV(CARB,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

               ELSEIF (K .LT. 5) then    !Coal

                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                     M_FYR = MIN(FYR , LASTYR)
                     FUEL(JYR) = ZPCLEL(I,M_FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                     CARB(JYR) = (1.0 - PCFCP(M_FYR,K)) * ZJCLEL(M_FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                  END DO
                  FYR = N_FYR - PLPD_SYR(XYR) + 1
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_CARB = PVV(CARB,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

               ELSEIF (K.eq.5) then     !Biomass

                  PV_FUEL = 0.0
                  PV_CARB = 0.0

               ELSEIF (K.eq.6) then       !Electrolysis

                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                     M_FYR = MIN(FYR , LASTYR)
                     FUEL(JYR) = ZPELIN(I,FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                  END DO
                  FYR = N_FYR - PLPD_SYR(XYR) + 1
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_CARB = 0.0

               ELSEIF (K.eq.7) then        !Nuclear

                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                     M_FYR = MIN(FYR , LASTYR)
                     FUEL(JYR) = ZPUREL(I,FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                  END DO
                  FYR = N_FYR - PLPD_SYR(XYR) + 1
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_CARB = 0.0

                ELSEIF (K.eq.8) then     !Biomass with seq

                  PV_FUEL = 0.0

                  ! Give a carbon credit for each unit of biomass carbon sequestered
                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                     M_FYR = MIN(FYR , LASTYR)
                     CARB(JYR) = PCFCP(M_FYR,K) * (EMETAX(1,M_FYR)*EBMHM(M_FYR)) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)

                     IF (FCRL .EQ. 1 .AND. K2.EQ.51) WRITE(HF_HMMO,3912) K2+1989,CURITR,XYR,FYR,M_FYR,I,J,K, &
                        CARB(JYR),PCFCP(M_FYR,K),EMETAX(1,M_FYR),EBMHM(M_FYR),PCEFF(M_FYR,K),HMM_GNPD(FYR)
 3912                FORMAT(1X,"BioSeq",8(":",I4),6(":",F12.3))

                  END DO
                  FYR = N_FYR - PLPD_SYR(XYR) + 1
                  PV_CARB = PVV(CARB,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

               ENDIF

                  PCLEV(K2,I,J,K) = PV_CCST + PV_FOM_ + PV_VOM_ + PV_FUEL + PV_CARB + PV_ELEC


                  IF (FCRL .EQ. 1 .AND. K.EQ.8 .AND. K2.EQ.51) THEN
                     FYR = N_FYR - PLPD_SYR(XYR) + 1
                     WRITE(HF_HMMO,9002) K2+1989,CURITR,XYR,FYR,I,J,K,PCLEV(K2,I,J,K),PV_CCST,PV_FOM_,PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,CAP,BVBOOK,FOM_(1),FOM_(FYR), &
                        VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR),CARB(1),CARB(FYR),ELEC(1),ELEC(FYR),INFL(1),INFL(FYR)
                  END IF


                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,5i5,12f20.5,i6)')'PCLEVdbg,y,i,r,m,tech,frf,rcc,cct,fom,vom,ngel,eff,elc,elin:', &
                     k2,curitr,i,j,k,PCFRF(K2,K),PCRCC(K2,I,J,K),PCCT(K2,K),PCFOM(K2,K),PCVOM(K2,K),  &
                     (ZPNGEL(I,K2)-(PCFCP(K2,K)*ZJNGEL(K2))),PCEFF(K2,K),PCELC(K2,K), &
                     ZPELIN(I,K2),PCLEV(K2,I,J,K),MC_JPGDP(K2),MC_JPGDP(YRDOLP-1989),YRDOLP-1989
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = N_FYR - PLPD_SYR(XYR) + 1
                     WRITE(HF_HMMO,9002) K2+1989,CURITR,XYR,FYR,I,J,K,PCLEV(K2,I,J,K),PV_CCST,PV_FOM_,PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,CAP,BVBOOK,FOM_(1),FOM_(FYR), &
                        VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR),CARB(1),CARB(FYR),ELEC(1),ELEC(FYR),INFL(1),INFL(FYR)
 9002                FORMAT(1X,"PCLEVrsc",7(":",I4),23(":",F9.3))
                  END IF

            ENDDO     !PCHTCH

!           Costing of New City Gate Facilities

            DO K=1,PGHTCH
               IF (K .EQ. 1) then
                  CLT = 1                                                                                ! CONSTRUCTION LEAD TIME -- ACTUAL
                  OVRCST = PGCT(K2,K) * PGRCC(K2,I,J,K) / (PGUTZ(K2,K) * 365.0) / HMM_GNPD(YRDOLP-1989)  ! OVERNIGHT COSTS
                  PROFILE(1) = 1.0                                                                       ! ANNUAL EXPENDITURE PERCENTAGE
                  CAPESC(1) = 1.0                                                                        ! CAPITAL ESCALATION/REAL
                  GNPF(1) = HMM_GNPD(MAX(1 , K2 + PLPD_SYR(XYR) - CLT - 1))                              ! GENERAL INFLATION
                  INTR = CODHM + MC_RMTCM10Y(K2) / 100.0                                                 ! INTEREST COSTS
                  DEBT_F = 1.0 - EQTYHM                                                                  ! CONSTRUCTION DEBT FINANCING
                  ROR = RORHM                                                                            ! RATE OF RETURN

                  CALL INSTALL_CST(CLT,MAX_LCP,OVRCST,PROFILE,CAPESC,GNPF,INTR,DEBT_F,ROR,TXBOOK,BVBOOK)

                  DL2 = PGLIF(K2,K)                                                                      ! DEBT LOAN LIFE <= CONTRACT LIFE
                  CL = PGLIF(K2,K)                                                                       ! CONTRACT LIFE
                  TL = PGLIF(K2,K)                                                                       ! TAX LIFE ASSOCIATED WITH TECHNOLOGY
                  ROE = COEHM + MC_RMTCM10Y(K2) / 100.0                                                  ! ROE
                  INT = CODHM + MC_RMTCM10Y(K2) / 100.0                                                  ! INTEREST RATE
                  DF2 = 1.0 - EQTYHM                                                                     ! DEBT FRACTION
                  TR = 0.38                                                                              ! MARGINAL FEDERAL INCOME TAX RATE
                  RATIO = TXBOOK / BVBOOK                                                                ! TX TO BV RATIO

                  CALL ANNUITY_FACTOR(TL,CL,DL2,DF2,RATIO,ROE,INT,TR,CAP,HF_HMMO)

                  INSTALL_COST(K2,CINX,MINX,MAP_PG(K,1)) = BVBOOK       / HMM_GNPD(K2)
                  ANNUITY_COST(K2,CINX,MINX,MAP_PG(K,1)) = CAP * BVBOOK / HMM_GNPD(K2)

                  CCST = 0.0
                  FOM_ = 0.0
                  VOM_ = 0.0
                  FUEL = 0.0
                  CARB = 0.0
                  ELEC = 0.0
                  INFL = 0.0
                  KG = 0.0
                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                     M_FYR = MIN(FYR , LASTYR)
                     CCST(JYR) = CAP * BVBOOK
                     FOM_(JYR) = PGFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     VOM_(JYR) = PGVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     FUEL(JYR) = ZPNGEL(I,M_FYR) / 1000000.0 * PGEFF(M_FYR,K) * HMM_GNPD(FYR)
                     CARB(JYR) = (1.0 - PGFCP(M_FYR,K)) * ZJNGEL(M_FYR) / 1000000.0 * PGEFF(M_FYR,K) * HMM_GNPD(FYR)
                     ELEC(JYR) = PGELC(M_FYR,K) / 1000000.0 * ZPELIN(I,M_FYR) * HMM_GNPD(FYR)
                     INFL(JYR) = HMM_GNPD(FYR)
                     KG(JYR) = 1.0
                  END DO
                  FYR = N_FYR - PLPD_SYR(XYR) + 1
                  PV_CCST = PVV(CCST,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_CARB = PVV(CARB,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

                  PGLEV(K2,I,J,K) = PV_CCST + PV_FOM_ + PV_VOM_ + PV_FUEL + PV_CARB + PV_ELEC

                  if (FCRL .eq.1 ) write(HF_HMMO,'(a,5i5,12f20.5,i6)')'PGLEVdbg,y,i,r,m,tech,frf,rcc,cct,fom,vom,ngel,eff,elc,elin:', &
                     k2,curitr,i,j,k,PGFRF(K2,K),PGRCC(K2,I,J,K),PGCT(K2,K),PGFOM(K2,K),PGVOM(K2,K),  &
                     (ZPNGEL(I,K2)-(PCFCP(K2,K)*ZJNGEL(K2))),PGEFF(K2,K),PGELC(K2,K), &
                     ZPELIN(I,K2),PGLEV(K2,I,J,K),MC_JPGDP(K2),MC_JPGDP(YRDOLP-1989),YRDOLP-1989
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = N_FYR - PLPD_SYR(XYR) + 1
                     WRITE(HF_HMMO,9003) K2+1989,CURITR,XYR,FYR,I,J,K,PGLEV(K2,I,J,K),PV_CCST,PV_FOM_,PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,CAP,BVBOOK,FOM_(1),FOM_(FYR), &
                        VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR),CARB(1),CARB(FYR),ELEC(1),ELEC(FYR),INFL(1),INFL(FYR)
 9003                FORMAT(1X,"PGLEVrsc",7(":",I4),23(":",F9.3))
                  END IF
               ELSE
                  PGLEV(K2,I,J,K) = 1000.0
               ENDIF
            ENDDO     !PGHTCH

!           Costing of New Forecourt Production Facilities

            DO K=1,PFHTCH

               IF (K .EQ. 1) then
                  CLT = 1                                                                                ! CONSTRUCTION LEAD TIME -- ACTUAL
                  OVRCST = PFCT(K2,K) * PFRCC(K2,I,J,K) / (PFUTZ(K2,K) * 365.0) / HMM_GNPD(YRDOLP-1989)  ! OVERNIGHT COSTS
                  PROFILE(1) = 1.0                                                                       ! ANNUAL EXPENDITURE PERCENTAGE
                  CAPESC(1) = 1.0                                                                        ! CAPITAL ESCALATION/REAL
                  GNPF(1) = HMM_GNPD(MAX(1 , K2 + PLPD_SYR(XYR) - CLT - 1))                              ! GENERAL INFLATION
                  INTR = CODHM + MC_RMTCM10Y(K2) / 100.0                                                 ! INTEREST COSTS
                  DEBT_F = 1.0 - EQTYHM                                                                  ! CONSTRUCTION DEBT FINANCING
                  ROR = RORHM                                                                            ! RATE OF RETURN

!                  WRITE(HF_HMMO,7033)K2+1989,CURITR,XYR,I,J,K,CLT,OVRCST,PFCT(K2,K),PFRCC(K2,I,J,K),PFUTZ(K2,K),HMM_GNPD(YRDOLP-1989),PROFILE(1), &
!                     CAPESC(1),GNPF(1),HMM_GNPD(MAX(1,K2+PLPD_SYR(XYR)-CLT-1)),INTR,CODHM,MC_RMTCM10Y(K2),DEBT_F,EQTYHM,RORHM

                  CALL INSTALL_CST(CLT,MAX_LCP,OVRCST,PROFILE,CAPESC,GNPF,INTR,DEBT_F,ROR,TXBOOK,BVBOOK)

                  DL2 = PFLIF(K2,K)                                                                      ! DEBT LOAN LIFE <= CONTRACT LIFE
                  CL = PFLIF(K2,K)                                                                       ! CONTRACT LIFE
                  TL = PFLIF(K2,K)                                                                       ! TAX LIFE ASSOCIATED WITH TECHNOLOGY
                  ROE = COEHM + MC_RMTCM10Y(K2) / 100.0                                                  ! ROE
                  INT = CODHM + MC_RMTCM10Y(K2) / 100.0                                                  ! INTEREST RATE
                  DF2 = 1.0 - EQTYHM                                                                     ! DEBT FRACTION
                  TR = 0.38                                                                              ! MARGINAL FEDERAL INCOME TAX RATE
                  RATIO = TXBOOK / BVBOOK                                                                ! TX TO BV RATIO

                  CALL ANNUITY_FACTOR(TL,CL,DL2,DF2,RATIO,ROE,INT,TR,CAP,HF_HMMO)

                  INSTALL_COST(K2,CINX,MINX,MAP_PF(K,1)) = BVBOOK       / HMM_GNPD(K2)
                  ANNUITY_COST(K2,CINX,MINX,MAP_PF(K,1)) = CAP * BVBOOK / HMM_GNPD(K2)

                  ! debug
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                      WRITE(HF_HMMO,9998) K2+1989,CURITR,XYR,FYR,I,J,K, &
                          CLT,MAX_LCP,TL,CL,DL2,PFLIF(K2,K), &
                          OVRCST,PROFILE(1),CAPESC(1),GNPF(1),INTR,DEBT_F,ROR,TXBOOK,BVBOOK, &
                          DF2,RATIO,ROE,INT,TR,CAP,PFCT(K2,K),PFRCC(K2,I,J,K),PFUTZ(K2,K)
9998                  FORMAT(1X,"PFCAPEXP",13(":",I4),18(":",F9.3))
                  ENDIF

                  CCST = 0.0
                  FOM_ = 0.0
                  VOM_ = 0.0
                  FUEL = 0.0
                  CARB = 0.0
                  ELEC = 0.0
                  INFL = 0.0
                  KG = 0.0
                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                     M_FYR = MIN(FYR , LASTYR)
                     CCST(JYR) = CAP * BVBOOK
                     FOM_(JYR) = PFFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     VOM_(JYR) = PFVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     FUEL(JYR) = ZPELCM(I,M_FYR) / 1000000.0 * PFEFF(M_FYR,K) * HMM_GNPD(FYR)
                     ELEC(JYR) = PFELC(M_FYR,K) / 1000000.0 * ZPELCM(I,M_FYR) * HMM_GNPD(FYR)
                     INFL(JYR) = HMM_GNPD(FYR)
                     KG(JYR) = 1.0
                  END DO
                  FYR = N_FYR - PLPD_SYR(XYR) + 1
                  PV_CCST = PVV(CCST,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_CARB = 0.0
                  PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

                  PFLEV(K2,I,J,K) = PV_CCST + PV_FOM_ + PV_VOM_ + PV_FUEL + PV_CARB + PV_ELEC

                  if (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,5i5,12f20.5,i6)')'PFLEVdbg,y,i,r,m,tech,frf,rcc,cct,fom,vom,ngel,eff,elc,elin:', &
                     k2,curitr,i,j,k,PFFRF(K2,K),PFRCC(K2,I,J,K),PFCT(K2,K),PFFOM(K2,K),PFVOM(K2,K),  &
                     (ZPNGEL(I,K2)-(PCFCP(K2,K)*ZJNGEL(K2))),PFEFF(K2,K),PFELC(K2,K), &
                     ZPELIN(I,K2),PFLEV(K2,I,J,K),MC_JPGDP(K2),MC_JPGDP(YRDOLP-1989),YRDOLP-1989
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = N_FYR - PLPD_SYR(XYR) + 1
                     WRITE(HF_HMMO,9004) K2+1989,CURITR,XYR,FYR,I,J,K,PFLEV(K2,I,J,K),PV_CCST,PV_FOM_,PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,CAP,BVBOOK,FOM_(1),FOM_(FYR), &
                        VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR),CARB(1),CARB(FYR),ELEC(1),ELEC(FYR),INFL(1),INFL(FYR)
 9004                FORMAT(1X,"PFLEVrsc",7(":",I4),23(":",F9.3))
                  END IF
               ELSE IF (K.EQ.2) then
                  ! New Forecourt SMR

                  CLT = 1                                                                                ! CONSTRUCTION LEAD TIME -- ACTUAL
                  OVRCST = PFCT(K2,K) * PFRCC(K2,I,J,K) / (PFUTZ(K2,K) * 365.0) / HMM_GNPD(YRDOLP-1989)  ! OVERNIGHT COSTS
                  PROFILE(1) = 1.0                                                                       ! ANNUAL EXPENDITURE PERCENTAGE
                  CAPESC(1) = 1.0                                                                        ! CAPITAL ESCALATION/REAL
                  GNPF(1) = HMM_GNPD(MAX(1 , K2 + PLPD_SYR(XYR) - CLT - 1))                              ! GENERAL INFLATION
                  INTR = CODHM + MC_RMTCM10Y(K2) / 100.0                                                 ! INTEREST COSTS
                  DEBT_F = 1.0 - EQTYHM                                                                  ! CONSTRUCTION DEBT FINANCING
                  ROR = RORHM                                                                            ! RATE OF RETURN

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1)  &
                  WRITE(HF_HMMO,7033)K2+1989,CURITR,XYR,I,J,K,CLT,OVRCST,PFCT(K2,K),PFRCC(K2,I,J,K),PFUTZ(K2,K),HMM_GNPD(YRDOLP-1989),PROFILE(1), &
                     CAPESC(1),GNPF(1),HMM_GNPD(MAX(1,K2+PLPD_SYR(XYR)-CLT-1)),INTR,CODHM,MC_RMTCM10Y(K2),DEBT_F,EQTYHM,RORHM
7033              FORMAT(1X,"BVBOOK",7(":",I4),15(":",F9.3))

                  CALL INSTALL_CST(CLT,MAX_LCP,OVRCST,PROFILE,CAPESC,GNPF,INTR,DEBT_F,ROR,TXBOOK,BVBOOK)

                  DL2 = PFLIF(K2,K)                                                                      ! DEBT LOAN LIFE <= CONTRACT LIFE
                  CL = PFLIF(K2,K)                                                                       ! CONTRACT LIFE
                  TL = PFLIF(K2,K)                                                                       ! TAX LIFE ASSOCIATED WITH TECHNOLOGY
                  ROE = COEHM + MC_RMTCM10Y(K2) / 100.0                                                  ! ROE
                  INT = CODHM + MC_RMTCM10Y(K2) / 100.0                                                  ! INTEREST RATE
                  DF2 = 1.0 - EQTYHM                                                                     ! DEBT FRACTION
                  TR = 0.38                                                                              ! MARGINAL FEDERAL INCOME TAX RATE
                  RATIO = TXBOOK / BVBOOK                                                                ! TX TO BV RATIO

                  CALL ANNUITY_FACTOR(TL,CL,DL2,DF2,RATIO,ROE,INT,TR,CAP,HF_HMMO)

                  INSTALL_COST(K2,CINX,MINX,MAP_PF(K,1)) = BVBOOK       / HMM_GNPD(K2)
                  ANNUITY_COST(K2,CINX,MINX,MAP_PF(K,1)) = CAP * BVBOOK / HMM_GNPD(K2)

                  ! debug
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                      WRITE(HF_HMMO,9998) K2+1989,CURITR,XYR,FYR,I,J,K, &
                          CLT,MAX_LCP,TL,CL,DL2,PFLIF(K2,K), &
                          OVRCST,PROFILE(1),CAPESC(1),GNPF(1),INTR,DEBT_F,ROR,TXBOOK,BVBOOK, &
                          DF2,RATIO,ROE,INT,TR,CAP,PFCT(K2,K),PFRCC(K2,I,J,K),PFUTZ(K2,K)
                  ENDIF

                  CCST = 0.0
                  FOM_ = 0.0
                  VOM_ = 0.0
                  FUEL = 0.0
                  CARB = 0.0
                  ELEC = 0.0
                  INFL = 0.0
                  KG = 0.0
                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                     M_FYR = MIN(FYR , LASTYR)
                     CCST(JYR) = CAP * BVBOOK
                     FOM_(JYR) = PFFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     VOM_(JYR) = PFVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     FUEL(JYR) = ZPNGIN(I,M_FYR) / 1000000.0 * PFEFF(M_FYR,K) * HMM_GNPD(FYR)
                     ELEC(JYR) = PFELC(M_FYR,K) / 1000000.0 * ZPELCM(I,M_FYR) * HMM_GNPD(FYR)
                     INFL(JYR) = HMM_GNPD(FYR)
                     KG(JYR) = 1.0
                  END DO
                  FYR = N_FYR - PLPD_SYR(XYR) + 1
                  PV_CCST = PVV(CCST,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_CARB = 0.0
                  PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

                  PFLEV(K2,I,J,K) = PV_CCST + PV_FOM_ + PV_VOM_ + PV_FUEL + PV_CARB + PV_ELEC

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,5i5,12f20.5,i6)')'PFLEVdbg,y,i,r,m,tech,frf,rcc,cct,fom,vom,ngel,eff,elc,elin:', &
                     k2,curitr,i,j,K,PFFRF(K2,K),PFRCC(K2,I,J,K),PFCT(K2,K),PFFOM(K2,K),PFVOM(K2,K),  &
                     (ZPNGEL(I,K2)-(PCFCP(K2,K)*ZJNGEL(K2))),PFEFF(K2,K),PFELC(K2,K), &
                     ZPELIN(I,K2),PFLEV(K2,I,J,K),MC_JPGDP(K2),MC_JPGDP(YRDOLP-1989),YRDOLP-1989
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = N_FYR - PLPD_SYR(XYR) + 1
                     WRITE(HF_HMMO,9018) K2+1989,CURITR,XYR,FYR,I,J,K,PFLEV(K2,I,J,K), &
                        PV_CCST,PV_FOM_,PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,CAP, &
                        BVBOOK,FOM_(1),FOM_(FYR), VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR), &
                        CARB(1),CARB(FYR),ELEC(1),ELEC(FYR),INFL(1),INFL(FYR)
9018                FORMAT(1X,"PFLEVrs2",7(":",I4),23(":",F9.3))
                  END IF

               ELSE IF (K.EQ.3) then
                  ! New Forecourt SER

                  CLT = 1                                                                                ! CONSTRUCTION LEAD TIME -- ACTUAL
                  OVRCST = PFCT(K2,K) * PFRCC(K2,I,J,K) / (PFUTZ(K2,K) * 365.0) / HMM_GNPD(YRDOLP-1989)  ! OVERNIGHT COSTS
                  PROFILE(1) = 1.0                                                                       ! ANNUAL EXPENDITURE PERCENTAGE
                  CAPESC(1) = 1.0                                                                        ! CAPITAL ESCALATION/REAL
                  GNPF(1) = HMM_GNPD(MAX(1 , K2 + PLPD_SYR(XYR) - CLT - 1))                              ! GENERAL INFLATION
                  INTR = CODHM + MC_RMTCM10Y(K2) / 100.0                                                 ! INTEREST COSTS
                  DEBT_F = 1.0 - EQTYHM                                                                  ! CONSTRUCTION DEBT FINANCING
                  ROR = RORHM                                                                            ! RATE OF RETURN

                  WRITE(HF_HMMO,7033)K2+1989,CURITR,XYR,I,J,K,CLT,OVRCST,PFCT(K2,K),PFRCC(K2,I,J,K),PFUTZ(K2,K),HMM_GNPD(YRDOLP-1989),PROFILE(1), &
                     CAPESC(1),GNPF(1),HMM_GNPD(MAX(1,K2+PLPD_SYR(XYR)-CLT-1)),INTR,CODHM,MC_RMTCM10Y(K2),DEBT_F,EQTYHM,RORHM

                  CALL INSTALL_CST(CLT,MAX_LCP,OVRCST,PROFILE,CAPESC,GNPF,INTR,DEBT_F,ROR,TXBOOK,BVBOOK)

                  DL2 = PFLIF(K2,K)                                                                      ! DEBT LOAN LIFE <= CONTRACT LIFE
                  CL = PFLIF(K2,K)                                                                       ! CONTRACT LIFE
                  TL = PFLIF(K2,K)                                                                       ! TAX LIFE ASSOCIATED WITH TECHNOLOGY
                  ROE = COEHM + MC_RMTCM10Y(K2) / 100.0                                                  ! ROE
                  INT = CODHM + MC_RMTCM10Y(K2) / 100.0                                                  ! INTEREST RATE
                  DF2 = 1.0 - EQTYHM                                                                     ! DEBT FRACTION
                  TR = 0.38                                                                              ! MARGINAL FEDERAL INCOME TAX RATE
                  RATIO = TXBOOK / BVBOOK                                                                ! TX TO BV RATIO

                  CALL ANNUITY_FACTOR(TL,CL,DL2,DF2,RATIO,ROE,INT,TR,CAP,HF_HMMO)

                  INSTALL_COST(K2,CINX,MINX,MAP_PF(K,1)) = BVBOOK       / HMM_GNPD(K2)
                  ANNUITY_COST(K2,CINX,MINX,MAP_PF(K,1)) = CAP * BVBOOK / HMM_GNPD(K2)

                  ! debug
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                      WRITE(HF_HMMO,9998) K2+1989,CURITR,XYR,FYR,I,J,K, &
                          CLT,MAX_LCP,TL,CL,DL2,PFLIF(K2,K), &
                          OVRCST,PROFILE(1),CAPESC(1),GNPF(1),INTR,DEBT_F,ROR,TXBOOK,BVBOOK, &
                          DF2,RATIO,ROE,INT,TR,CAP,PFCT(K2,K),PFRCC(K2,I,J,K),PFUTZ(K2,K)
                  ENDIF

                  CCST = 0.0
                  FOM_ = 0.0
                  VOM_ = 0.0
                  FUEL = 0.0
                  CARB = 0.0
                  ELEC = 0.0
                  INFL = 0.0
                  KG = 0.0
                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                     M_FYR = MIN(FYR , LASTYR)
                     CCST(JYR) = CAP * BVBOOK
                     FOM_(JYR) = PFFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     VOM_(JYR) = PFVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     FUEL(JYR) = (PETHM(I,M_FYR)+0.13) / 3500000.0 * PFEFF(M_FYR,K) * HMM_GNPD(FYR)        ! Add markup to wholesale ethanol price
                     ELEC(JYR) = PFELC(M_FYR,K) / 1000000.0 * ZPELCM(I,M_FYR) * HMM_GNPD(FYR)
                     INFL(JYR) = HMM_GNPD(FYR)
                     KG(JYR) = 1.0
                  END DO
                  FYR = N_FYR - PLPD_SYR(XYR) + 1
                  PV_CCST = PVV(CCST,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_CARB = 0.0
                  PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                  PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

                  PFLEV(K2,I,J,K) = PV_CCST + PV_FOM_ + PV_VOM_ + PV_FUEL + PV_CARB + PV_ELEC

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,5i5,12f20.5,i6)')'PFLEVdbg,y,i,r,m,tech,frf,rcc,cct,fom,vom,ngel,eff,elc,elin:', &
                     k2,curitr,i,j,K,PFFRF(K2,K),PFRCC(K2,I,J,K),PFCT(K2,K),PFFOM(K2,K),PFVOM(K2,K),  &
                     (ZPNGEL(I,K2)-(PCFCP(K2,K)*ZJNGEL(K2))),PFEFF(K2,K),PFELC(K2,K), &
                     ZPELIN(I,K2),PFLEV(K2,I,J,K),MC_JPGDP(K2),MC_JPGDP(YRDOLP-1989),YRDOLP-1989
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = N_FYR - PLPD_SYR(XYR) + 1
                     WRITE(HF_HMMO,9018) K2+1989,CURITR,XYR,FYR,I,J,K,PFLEV(K2,I,J,K), &
                        PV_CCST,PV_FOM_,PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,CAP, &
                        BVBOOK,FOM_(1),FOM_(FYR), VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR), &
                        CARB(1),CARB(FYR),ELEC(1),ELEC(FYR),INFL(1),INFL(FYR)
                  END IF

               ENDIF    ! K

            ENDDO    !PFHTCH
!
         ELSE IF (J2.EQ.2) THEN      !existing

!           Costing of Existing Central Production Facilities

            DO K = 1 , PCHTCH
               FOM_ = 0.0
               VOM_ = 0.0
               FUEL = 0.0
               CARB = 0.0
               ELEC = 0.0
               INFL = 0.0
               KG = 0.0
               DO JYR = 1 , PLPD_EYR(XYR)
                  FYR = JYR + K2 - 1
                  M_FYR = MIN(FYR , LASTYR)
                  FOM_(JYR) = PCFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                  VOM_(JYR) = PCVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                  ELEC(JYR) = PCELC(M_FYR,K) / 1000000.0 * ZPELIN(I,M_FYR) * HMM_GNPD(FYR)
                  INFL(JYR) = HMM_GNPD(FYR)
                  KG(JYR) = 1.0
               END DO
               PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
               PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
               PV_ELEC = PVV(ELEC,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
               PV_INFL = PVV(INFL,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
               PV_KG = PVV(KG,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)

               IF (K .LT. 3) then      !NG

                  DO JYR = 1 , PLPD_EYR(XYR)
                     FYR = JYR + K2 - 1
                     M_FYR = MIN(FYR , LASTYR)
                     FUEL(JYR) = ZPNGEL(I,M_FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                     CARB(JYR) = (1.0 - PCFCP(M_FYR,K)) * ZJNGEL(M_FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                  END DO
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM)
                  PV_CARB = PVV(CARB,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM)

               ELSEIF (K .LT. 5) then    !Coal

                  DO JYR = 1 , PLPD_EYR(XYR)
                     FYR = JYR + K2 - 1
                     M_FYR = MIN(FYR , LASTYR)
                     FUEL(JYR) = ZPCLEL(I,M_FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                     CARB(JYR) = (1.0 - PCFCP(M_FYR,K)) * ZJCLEL(M_FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                  END DO
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM)
                  PV_CARB = PVV(CARB,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM)

               ELSEIF (K.eq.5) then     !Biomass

                  PV_FUEL = 0.0
                  PV_CARB = 0.0

               ELSEIF (K.eq.6) then       !Electrolysis

                  DO JYR = 1 , PLPD_EYR(XYR)
                     FYR = JYR + K2 - 1
                     M_FYR = MIN(FYR , LASTYR)
                     FUEL(JYR) = ZPELIN(I,FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                  END DO
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM)
                  PV_CARB = 0.0

               ELSEIF (K.eq.7) then        !Nuclear

                  DO JYR = 1 , PLPD_EYR(XYR)
                     FYR = JYR + K2 - 1
                     M_FYR = MIN(FYR , LASTYR)
                     FUEL(JYR) = ZPUREL(I,FYR) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)
                  END DO
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM)
                  PV_CARB = 0.0

               ELSEIF (K.eq.8) then     !Biomass with seq

                  PV_FUEL = 0.0

                  ! Give a carbon credit for each unit of biomass carbon sequestered
                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                     FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                     M_FYR = MIN(FYR , LASTYR)
                     CARB(JYR) = PCFCP(M_FYR,K) * (EMETAX(1,M_FYR)*EBMHM(M_FYR)) / 1000000.0 * PCEFF(M_FYR,K) * HMM_GNPD(FYR)

                     IF (FCRL .EQ. 1 .AND. K2.EQ.51) WRITE(HF_HMMO,3912) K2+1989,CURITR,XYR,FYR,M_FYR,I,J,K, &
                        CARB(JYR),PCFCP(M_FYR,K),EMETAX(1,M_FYR),EBMHM(M_FYR),PCEFF(M_FYR,K),HMM_GNPD(FYR)

                  END DO
                  FYR = N_FYR - PLPD_SYR(XYR) + 1
                  PV_CARB = PVV(CARB,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

               ENDIF

               PCLEX(K2,I,J,K) = PV_FOM_ + PV_VOM_ + PV_FUEL + PV_CARB + PV_ELEC

               IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                  FYR = PLPD_EYR(XYR)
                  WRITE(HF_HMMO,9005) K2+1989,CURITR,XYR,FYR,I,J,K,PCLEX(K2,I,J,K),PV_FOM_,PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,FOM_(1),FOM_(FYR), &
                     VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR),CARB(1),CARB(FYR),ELEC(1),ELEC(FYR),INFL(1),INFL(FYR)
 9005             FORMAT(1X,"PCLEXrsc",7(":",I4),20(":",F9.3))
               END IF

            ENDDO     !PCHTCH

!           Costing of Existing City Gate Facilities

            DO K=1,PGHTCH
               IF (K .EQ. 1) then      !NG
                  FOM_ = 0.0
                  VOM_ = 0.0
                  FUEL = 0.0
                  CARB = 0.0
                  ELEC = 0.0
                  INFL = 0.0
                  KG = 0.0
                  DO JYR = 1 , PLPD_EYR(XYR)
                     FYR = JYR + K2 - 1
                     M_FYR = MIN(FYR , LASTYR)
                     FOM_(JYR) = PGFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     VOM_(JYR) = PGVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     FUEL(JYR) = ZPNGEL(I,M_FYR) / 1000000.0 * PGEFF(M_FYR,K) * HMM_GNPD(FYR)
                     CARB(JYR) = (1.0 - PGFCP(M_FYR,K)) * ZJNGEL(M_FYR) / 1000000.0 * PGEFF(M_FYR,K) * HMM_GNPD(FYR)
                     ELEC(JYR) = PGELC(M_FYR,K) / 1000000.0 * ZPELIN(I,M_FYR) * HMM_GNPD(FYR)
                     INFL(JYR) = HMM_GNPD(FYR)
                     KG(JYR) = 1.0
                  END DO
                  PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_CARB = PVV(CARB,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_ELEC = PVV(ELEC,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_INFL = PVV(INFL,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_KG = PVV(KG,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)

                  PGLEX(K2,I,J,K) = PV_FOM_ + PV_VOM_ + PV_FUEL + PV_CARB + PV_ELEC

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = PLPD_EYR(XYR)
                     WRITE(HF_HMMO,9006) K2+1989,CURITR,XYR,FYR,I,J,K,PGLEX(K2,I,J,K),PV_FOM_,PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,FOM_(1),FOM_(FYR), &
                        VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR),CARB(1),CARB(FYR),ELEC(1),ELEC(FYR),INFL(1),INFL(FYR)
 9006                FORMAT(1X,"PGLEXrsc",7(":",I4),20(":",F9.3))
                  END IF
               ELSE
                  PGLEX(K2,I,J,K) = 1000.0
               ENDIF
            ENDDO     !PGHTCH

!           Costing of Existing Forecourt Production Facilities

            DO K=1,PFHTCH

               IF (K .EQ. 1) then      !Electrolysis
                  FOM_ = 0.0
                  VOM_ = 0.0
                  FUEL = 0.0
                  CARB = 0.0
                  ELEC = 0.0
                  INFL = 0.0
                  KG = 0.0
                  DO JYR = 1 , PLPD_EYR(XYR)
                     FYR = JYR + K2 - 1
                     M_FYR = MIN(FYR , LASTYR)
                     FOM_(JYR) = PFFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     VOM_(JYR) = PFVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     FUEL(JYR) = ZPELCM(I,M_FYR) / 1000000.0 * PFEFF(M_FYR,K) * HMM_GNPD(FYR)
                     ELEC(JYR) = PFELC(M_FYR,K) / 1000000.0 * ZPELCM(I,M_FYR) * HMM_GNPD(FYR)
                     INFL(JYR) = HMM_GNPD(FYR)
                     KG(JYR) = 1.0
                  END DO
                  PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_CARB = 0.0
                  PV_ELEC = PVV(ELEC,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_INFL = PVV(INFL,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                  PV_KG = PVV(KG,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)

                  PFLEX(K2,I,J,K) = PV_FOM_ + PV_VOM_ + PV_FUEL + PV_CARB + PV_ELEC

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = PLPD_EYR(XYR)
                     WRITE(HF_HMMO,9007) K2+1989,CURITR,XYR,FYR,I,J,K,PFLEX(K2,I,J,K),PV_FOM_,PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,FOM_(1),FOM_(FYR), &
                        VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR),CARB(1),CARB(FYR),ELEC(1),ELEC(FYR),INFL(1),INFL(FYR)
 9007                FORMAT(1X,"PFLEXrsc",7(":",I4),20(":",F9.3))
                  END IF
               ELSE IF (K.EQ.2) then
                  ! Existing Forecourt SMR

                  FOM_ = 0.0
                  VOM_ = 0.0
                  FUEL = 0.0
                  CARB = 0.0
                  ELEC = 0.0
                  INFL = 0.0
                  KG = 0.0
                  DO JYR = 1 , PLPD_EYR(XYR)
                     FYR = JYR + K2 - 1
                     M_FYR = MIN(FYR , LASTYR)
                     FOM_(JYR) = PFFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     VOM_(JYR) = PFVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     FUEL(JYR) = ZPNGIN(I,M_FYR) / 1000000.0 * PFEFF(M_FYR,K) * HMM_GNPD(FYR)
                     ELEC(JYR) = PFELC(M_FYR,K) / 1000000.0 * ZPELCM(I,M_FYR) * HMM_GNPD(FYR)
                     INFL(JYR) = HMM_GNPD(FYR)
                     KG(JYR) = 1.0
                  END DO
                  FYR = PLPD_EYR(XYR)
                  PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                  PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                  PV_CARB = 0.0
                  PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                  PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                  PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)

                  PFLEX(K2,I,J,K) = PV_FOM_ + PV_VOM_ + PV_FUEL + PV_CARB + PV_ELEC

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,5i5,12f20.5,i6)')'PFLEXdbg,y,i,r,m,tech,frf,rcc,cct,fom,vom,ngel,eff,elc,elin:', &
                     k2,curitr,i,j,K,PFFRF(K2,K),PFRCC(K2,I,J,K),PFCT(K2,K),PFFOM(K2,K),PFVOM(K2,K),  &
                     (ZPNGEL(I,K2)-(PCFCP(K2,K)*ZJNGEL(K2))),PFEFF(K2,K),PFELC(K2,K), &
                     ZPELIN(I,K2),PFLEX(K2,I,J,K),MC_JPGDP(K2),MC_JPGDP(YRDOLP-1989),YRDOLP-1989
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = PLPD_EYR(XYR)
                     WRITE(HF_HMMO,8014) K2+1989,CURITR,XYR,FYR,I,J,K,PFLEX(K2,I,J,K),PV_FOM_, &
                        PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,FOM_(1),FOM_(FYR), &
                        VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR),CARB(1),CARB(FYR),ELEC(1), &
                        ELEC(FYR),INFL(1),INFL(FYR)
8014                 FORMAT(1X,"PFLEXrs2",7(":",I4),19(":",F9.3))
                  END IF

               ELSE IF (K.EQ.3) then
                  ! Existing Forecourt SER

                  FOM_ = 0.0
                  VOM_ = 0.0
                  FUEL = 0.0
                  CARB = 0.0
                  ELEC = 0.0
                  INFL = 0.0
                  KG = 0.0
                  DO JYR = 1 , PLPD_EYR(XYR)
                     FYR = JYR + K2 - 1
                     M_FYR = MIN(FYR , LASTYR)
                     FOM_(JYR) = PFFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     VOM_(JYR) = PFVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                     FUEL(JYR) = (PETHM(I,M_FYR)+0.13) / 3500000.0 * PFEFF(M_FYR,K) * HMM_GNPD(FYR)        ! Add markup to wholesale ethanol price
                     ELEC(JYR) = PFELC(M_FYR,K) / 1000000.0 * ZPELCM(I,M_FYR) * HMM_GNPD(FYR)
                     INFL(JYR) = HMM_GNPD(FYR)
                     KG(JYR) = 1.0
                  END DO
                  FYR = PLPD_EYR(XYR)
                  PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                  PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                  PV_FUEL = PVV(FUEL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                  PV_CARB = 0.0
                  PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                  PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)
                  PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,-1)

                  PFLEX(K2,I,J,K) = PV_FOM_ + PV_VOM_ + PV_FUEL + PV_CARB + PV_ELEC

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) write(HF_HMMO,'(a,5i5,12f20.5,i6)')'PFLEXdbg,y,i,r,m,tech,frf,rcc,cct,fom,vom,ngel,eff,elc,elin:', &
                     k2,curitr,i,j,K,PFFRF(K2,K),PFRCC(K2,I,J,K),PFCT(K2,K),PFFOM(K2,K),PFVOM(K2,K),  &
                     (ZPNGEL(I,K2)-(PCFCP(K2,K)*ZJNGEL(K2))),PFEFF(K2,K),PFELC(K2,K), &
                     ZPELIN(I,K2),PFLEX(K2,I,J,K),MC_JPGDP(K2),MC_JPGDP(YRDOLP-1989),YRDOLP-1989
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = PLPD_EYR(XYR)
                     WRITE(HF_HMMO,8014) K2+1989,CURITR,XYR,FYR,I,J,K,PFLEX(K2,I,J,K),PV_FOM_, &
                        PV_VOM_,PV_FUEL,PV_CARB,PV_ELEC,PV_INFL,PV_KG,FOM_(1),FOM_(FYR), &
                        VOM_(1),VOM_(FYR),FUEL(1),FUEL(FYR),CARB(1),CARB(FYR),ELEC(1), &
                        ELEC(FYR),INFL(1),INFL(FYR)
                  END IF

               ENDIF    ! K

            ENDDO    !PFHTCH
!
         ENDIF     ! existing    J2=2


         ! *** Transport Technology Costs ***
         ! =====================================================
         ! *** Levelized cost equations ***

!        Use average levelized daily demand of planning horizon for computing delivery costs
         Q = AVG_H2D(K2,CINX,MINX) * 1000000.0

         ! Ensure demand is not zero
         Q  = Q + 1

         ! Divide the market demand by the number of cities for the large
         !  markets, use unadjuested demand for small and rural markets
         IF (J.EQ.1) THEN
             Q = Q / CITIES(I,J)
         ENDIF

         ! Calculate the natural log of Q
         IF (Q .GE. 1) THEN
             LOGQ = LOG(Q)
         ELSE
             LOGQ = 0
         ENDIF

         ! Refueling station calculations
         IF (XYR .EQ. 1) THEN

            ! Calculate the incremental daily FIRST-PERIOD demand over last year's demand in KG
            ! for computing the number of fueling stations that need to be added this year
            ! ** This is used for the Fuel Availablity calculation in the trans model
            IF (K2.GE.2) THEN
              DMD_INC = (DMDLEV(K2,CINX,MINX,1) - DMDLEV(K2-1,CINX,MINX,1)) * 1000000000.0 /365
            ELSE
              DMD_INC = DMDLEV(K2,CINX,MINX,1) * 1000000000.0 /365
            ENDIF

            ! Calculate the incremental average daily levelized demand over last year's demand in KG
            ! for computing the number of fueling stations that need to be added this year
            IF (K2.GE.2) THEN
              Q_INC = (AVG_H2D(K2,CINX,MINX) - AVG_H2D(K2-1,CINX,MINX)) * 1000000.0
            ELSE
              Q_INC = AVG_H2D(K2,CINX,MINX) * 1000000.0
            ENDIF

            ! Ensure incremental demand is at least 1 to prevent the number of stations from decreasing
            Q_INC   = MAX(Q_INC,1.0)
            DMD_INC = MAX(DMD_INC,1.0)

            ! Assume that larger (1500 kg/day) fueling stations will be built if hydrogen fuel
            ! availability is greater than or equal to FA_SWITCH and that smaller stations (100 kg/day)
            ! will be built if hydrogen fuel availability is less than FA_SWITCH.  The FA_SWITCH should
            ! be approximately the point at which the dollar penalty on fuel availability equals the
            ! cost differential between 1500 kg/day station and the 100 kg/day station.
            IF (K2.GE.2) THEN

               IF (HFAVL(J,I,K2) .GE. FA_SWITCH) THEN

                  ! Calculate the number of gas stations in this CR,market for each dispensing tech
                  ! Liquid
                  STATNS(1) = NUMSTAT(K2-1,I,J) + ( Q_INC / (TFSZ1(K2,1)*SOUTZ(K2,1)) )
                  STATNS(1) = MAX(1.0, ANINT((STATNS(1) * 10.0 + 0.50) / 10.0))
                  ! Gaseous
                  STATNS(2) = NUMSTAT(K2-1,I,J) + ( Q_INC / (TFSZ1(K2,2)*SOUTZ(K2,2)) )
                  STATNS(2) = MAX(1.0, ANINT((STATNS(2) * 10.0 + 0.50) / 10.0))

                  ! Calculate the number of refueling stations in this CR,market based on the incremental demand in
                  ! this year only for the transportation model
                  HSTAT(K2,I,J) = HSTAT(K2-1,I,J) + ( DMD_INC / (TFSZ1(K2,1)*SOUTZ(K2,1)) )
                  HSTAT(K2,I,J) = MAX(1.0, ANINT((HSTAT(K2,I,J) * 10.0 + 0.50) / 10.0))

               ELSE

                  ! Calculate the number of gas stations in this CR,market for each dispensing tech
                  ! Liquid
                  STATNS(1) = NUMSTAT(K2-1,I,J) + ( Q_INC / (TFSZ2(K2,1)*SOUTZ(K2,1)) )
                  STATNS(1) = MAX(1.0, ANINT((STATNS(1) * 10.0 + 0.50) / 10.0))
                  ! Gaseous
                  STATNS(2) = NUMSTAT(K2-1,I,J) + ( Q_INC / (TFSZ2(K2,2)*SOUTZ(K2,2)) )
                  STATNS(2) = MAX(1.0, ANINT((STATNS(2) * 10.0 + 0.50) / 10.0))

                  ! Calculate the number of refueling stations in this CR,market based on the incremental demand in
                  ! this year only for the transportation model
                  HSTAT(K2,I,J) = HSTAT(K2-1,I,J) + ( DMD_INC / (TFSZ2(K2,1)*SOUTZ(K2,1)) )
                  HSTAT(K2,I,J) = MAX(1.0, ANINT((HSTAT(K2,I,J) * 10.0 + 0.50) / 10.0))

               ENDIF

               ! Store this year's value
               NUMSTAT(K2,I,J) = STATNS(1)

            ELSE
               ! Make sure these are not zero to avoid numerical problems
               STATNS(1)       = 1.0
               STATNS(2)       = 1.0
               NUMSTAT(K2,I,J) = 1.0
               HSTAT(K2,I,J)   = 1.0
            ENDIF

         ELSE

            ! Use the value calculated in the the first period
            STATNS(1) = NUMSTAT(K2,I,J)
            STATNS(2) = NUMSTAT(K2,I,J)

         ENDIF ! XYR = 1

         ! For large cities, divide the number of stations by the number of large cities in order to get the
         ! number of stations per city for delivery cost calculation
         IF (J.EQ.1) THEN
            STATNS(1) = STATNS(1)/CITIES(I,J)
            STATNS(1) = MAX(1.0, ANINT((STATNS(1) * 10.0 + 0.50) / 10.0))

            STATNS(2) = STATNS(2)/CITIES(I,J)
            STATNS(2) = MAX(1.0, ANINT((STATNS(2) * 10.0 + 0.50) / 10.0))
         ENDIF

         IF (K2.GT.1) &
             WRITE(HF_HMMO,8999) K2+1989,CURITR,XYR,FYR,I,J, &
                Q,Q_INC,NUMSTAT(K2,I,J),DMD_INC,HSTAT(K2,I,J),STATNS(1),STATNS(2), &
                SOUTZ(K2,1),SOUTZ(K2,2),TFSZ1(K2,1),TFSZ2(K2,1),HFAVL(J,I,K2), &
                DMDLEV(K2,CINX,MINX,1),FA_SWITCH,HSTAT(K2-1,I,J),( DMD_INC / (TFSZ2(K2,1)*SOUTZ(K2,1)) )
8999     FORMAT(1X,"NUMSTAT_DBG",6(":",I4),18(":",F16.3))

         IF (J2.EQ.1) THEN      ! New Builds

            ! Large city transport costs
            IF (J.EQ.1) THEN

               ! From Central Production

               DO K=1,TCHTCH

                  ! Initialize the cost variables
                  DO YRIDX = 1,HMM_MAX_FYR
                      CCST_IP(YRIDX) = 0.0
                      CCST_TD(YRIDX) = 0.0
                      FUEL_IP(YRIDX) = 0.0
                      FUEL_TD(YRIDX) = 0.0
                      VOM__IP(YRIDX) = 0.0
                      VOM__TD(YRIDX) = 0.0
                      FOM__IP(YRIDX) = 0.0
                      FOM__TD(YRIDX) = 0.0
                  END DO

                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1

                    FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                    M_FYR = MIN(FYR , LASTYR)

                    ! Divide the market area by the number of cities for the large
                    !  markets, use unadjuested area for small and rural markets

                    IF (J.EQ.1) THEN
                        TMPAREA = AREA(I,J) / CITIES(I,J)
                    ELSE
                        TMPAREA = AREA(I,J)
                    ENDIF

                    ! Calculate the natural log of transport miles
                    IF (MTTM(K2,CINX,MINX) .GE. 1) THEN
                        LOGMILES = LOG(MTTM(K2,CINX,MINX))
                    ELSE
                        LOGMILES = 0
                    ENDIF

                    ! Calculate the demand step variable
                    IF (Q .LT. 25000) THEN
                        DMDSTEP = 1
                    ELSE
                        DMDSTEP = 0
                    ENDIF

                    ! Calculate the service line miles [P7]
                    SLM = EXP( P7(12)*DMDSTEP + P7(2)*LOGQ + P7(9)*LOG(TMPAREA**0.5) + &
                                P7(4)*LOG(STATNS(2)) )

                    ! Calculate the trunk line miles [P8]
                    TLM = EXP( P8(12)*DMDSTEP + P8(2)*LOGQ + &
                                P8(9)*LOG(TMPAREA**0.5) + &
                                P8(4)*LOG(STATNS(2)) )

                    IF (K .EQ. 1) THEN
                        ! Compressed gas trucks

                        ! Central GASEOUS truck O&M costs [G10+G11]
                        TRUCK       = EXP( G10(1) + G10(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G10(3)*LOGMILES )
                        TERMINAL    = EXP( G11(1) + G11(2)*LOGQ + &
                                            G11(11)*(LOGQ - LOG(STATNS(2))) )
                        ANNOM       = TRUCK + TERMINAL

                        ! Central GASEOUS truck capital costs [(G3 or G4)=G5+G7]
                        ! [G17]
                        TRUCKS = EXP( G17(1) + G17(2)*LOGQ + G17(3)*LOGMILES + &
                                    G17(8)*LOG(TMPAREA) + G17(4)*LOG(STATNS(2)) )

                        ! Ensure TRUCKS is > 1 before applying the LOG function
                        IF (TRUCKS .GE. 1) THEN
                            LOGTRUCKS = LOG(TRUCKS)
                        ELSE
                            LOGTRUCKS = 0
                        ENDIF

                        IF (LOGMILES.GT.0) THEN
                            ! [G3]
                            PSI2700B = EXP( G3(1) + G3(10)*LOGTRUCKS + &
                                        G3(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G3(2)*LOGQ )
                            ! [G4]
                            PSI7000B = EXP( G4(1) + G4(10)*LOGTRUCKS + &
                                        G4(11)*(LOGQ - LOG(STATNS(2))) )
                            TRUCK    = ( FCR_TRUCK * MIN(PSI2700B,PSI7000B) ) / (Q*365)
                        ELSE
                            ! [G1]
                            PSI2700A = EXP( G1(1) + G1(10)*LOGTRUCKS + &
                                        G1(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G1(8)*LOG(TMPAREA) )
                            ! [G2]
                            PSI7000A = EXP( G2(1) + G2(10)*LOGTRUCKS + &
                                        G2(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G2(8)*LOG(TMPAREA) )
                            TRUCK    = ( FCR_TRUCK * MIN(PSI2700A,PSI7000A) ) / (Q*365)
                        ENDIF

                        ! [G7]
                        TERMINAL = ( FCR_TRUCK * 1.11 * EXP( G7(1) + &
                                    G7(2)*LOGQ + G7(4)*LOG(STATNS(2)) ) ) / (Q*365)
                        ANNCCST  = TRUCK + TERMINAL

                        ! Central GASEOUS truck fuel costs [G14+G15]
                        ! [G14]
                        TRUCK    = EXP( G14(1) + G14(2)*LOGQ + &
                                        G14(4)*LOG(STATNS(2)) + &
                                        G14(3)*LOGMILES )
                        ! [G15]
                        TERMINAL    = EXP( G15(1) + G15(2)*LOGQ + &
                                        G15(11)*(LOGQ - LOG(STATNS(2))) )
                        ANNFUEL     = TRUCK + TERMINAL

                        ! Adjusted annual gas truck transport costs
                        CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                        VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        INSTALL_COST(K2,CINX,MINX,MAP_TC(K,1)) = ANNCCST / FCR_TRUCK / MC_JPGDP(YRDOLD-1989)
                        ANNUITY_COST(K2,CINX,MINX,MAP_TC(K,1)) = ANNCCST / MC_JPGDP(YRDOLD-1989)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9100) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                TRUCKS,TRUCK,TERMINAL, &
                                PSI2700B,PSI7000B, &
                                MTTM(K2,CINX,MINX),TMPAREA, &
                                RORTD,FCR_TRUCK
9100                         FORMAT(1X,"TCGAS",9(":",I4),21(":",F16.3))
                          END IF

                    ELSEIF (K .EQ. 2) THEN
                        ! Liquid trucks

                        ! [L13]
                        TRUCKS      = EXP(L13(1) + L13(2)*LOGQ + L13(3)*LOGMILES)

                        ! Ensure TRUCKS is > 1 before applying the LOG function
                        IF (TRUCKS .GE. 1) THEN
                            LOGTRUCKS = LOG(TRUCKS)
                        ELSE
                            LOGTRUCKS = 0
                        ENDIF

                        ! Central LIQUID truck capital cost [L1+L2+L3]
                        ! For Liquifier use total area quantity because at central plant (mult by cities OK, since T only for J=1)
                        ! [L1]
                        TRUCK       = ( FCR_LIQUIFIER * EXP( L1(1) + &
                                        L1(10)*LOGTRUCKS ) ) / (Q*365)
                        ! [L2]
                        TERMINAL    = ( FCR_LIQUIFIER * 1.745 * EXP( L2(1) + &
                                        L2(2)*LOGQ) ) / (Q*365)
                        ! [L3]
                        LIQUIFIER   = ( FCR_LIQUIFIER * 1.745 * EXP( L3(1) + &
                                        L3(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) + &
                                        L3(8)*LOG(TMPAREA) + &
                                        L3(4)*LOG(STATNS(1)) ) ) / (MIN(Q*CITIES(I,J),MAXLIQ)*365)
                        ANNCCST     = TRUCK + TERMINAL + LIQUIFIER

                        ! Central LIQUID truck O&M costs [L5+L6+L7]
                        ! [L5]
                        TRUCK       = EXP( L5(1) + L5(9)*LOG(TMPAREA**0.5) + &
                                        L5(3)*LOGMILES + L5(12)*DMDSTEP )
                        ! [L6]
                        TERMINAL    = EXP( L6(1) + L6(2)*LOGQ + &
                                        L6(8)*LOG(TMPAREA) + &
                                        L6(4)*LOG(STATNS(1)) )
                        ! [L7]
                        LIQUIFIER   = EXP( L7(1) + L7(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                        ANNOM       = TRUCK + TERMINAL + LIQUIFIER

                        ! Central LIQUID truck fuel costs [L9+L10+L11]
                        ! [L9]
                        TRUCK       = EXP( L9(1) + L9(9)*LOG(TMPAREA**0.5) + &
                                        L9(3)*LOGMILES + L9(10)*LOGTRUCKS )
                        ! [L10]
                        TERMINAL    = 0
                        ! [L11]
                        LIQUIFIER   = ELECFACTOR * EXP( L11(1) + L11(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                        ANNFUEL     = TRUCK + TERMINAL + LIQUIFIER

                        ! Adjusted annual liquid truck transport costs
                        CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                        VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        INSTALL_COST(K2,CINX,MINX,MAP_TC(K,1)) = ANNCCST / FCR_LIQUIFIER / MC_JPGDP(YRDOLD-1989)
                        ANNUITY_COST(K2,CINX,MINX,MAP_TC(K,1)) = ANNCCST / MC_JPGDP(YRDOLD-1989)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9101) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(1), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                TRUCKS,TRUCK,TERMINAL,LIQUIFIER
9101                         FORMAT(1X,"TCLQ",9(":",I4),16(":",F16.3))
                          END IF

                    ELSEIF (K .EQ. 3) THEN
                        ! Pipeline

                        ! Central PIPELINE capital costs [P1]
                        ANNCCST      = ( FCR_PIPE * EXP( P1(1) + P1(2)*LOGQ + &
                                        P1(3)*LOGMILES ) ) / (Q*365)

                        ! Central PIPELINE O&M costs [P2]
                        ANNOM        = EXP( P2(1) + P2(2)*LOGQ + &
                                        P2(3)*LOGMILES )

                        ! Central PIPELINE fuel costs
                        ANNFUEL      = 0

                        ! Adjusted annual pipeline transport costs
                        CCST_IP(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_IP(JYR) = 0
                        VOM__IP(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        INSTALL_COST(K2,CINX,MINX,MAP_TC(K,1)) = ANNCCST / FCR_PIPE / MC_JPGDP(YRDOLD-1989)
                        ANNUITY_COST(K2,CINX,MINX,MAP_TC(K,1)) = ANNCCST / MC_JPGDP(YRDOLD-1989)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9102) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_IP(JYR),FUEL_IP(JYR),VOM__IP(JYR)
9102                         FORMAT(1X,"TCPIPE",9(":",I4),12(":",F16.3))
                          END IF

                    ELSE
                        ! *** SHOULD NEVER HAPPEN!! ***
                        ! Transport options are gas truck, liquid truck, and pipeline

                    ENDIF

                  END DO

                  FYR = N_FYR - PLPD_SYR(XYR) + 1

                  ! Calculate the present value of all cost components
                  PV_CCST_IP = PVV(CCST_IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                  PV_CCST_TD = PVV(CCST_TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)
                  PV_FUEL_IP = PVV(FUEL_IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                  PV_FUEL_TD = PVV(FUEL_TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)
                  PV_VOM__IP = PVV(VOM__IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                  PV_VOM__TD = PVV(VOM__TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)
                  PV_FOM__IP = PVV(FOM__IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                  PV_FOM__TD = PVV(FOM__TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)

                  TCLEV(K2,I,J,K) = PV_CCST_IP + PV_CCST_TD + PV_FUEL_IP + &
                                        PV_FUEL_TD + PV_VOM__IP + PV_VOM__TD + &
                                        PV_FOM__IP + PV_FOM__TD

                  ! Price adjustment factors for GPRA
                  IF (K.EQ.1) THEN
                     TCLEV(K2,I,J,K) = GPRAMULT(1)*TCLEV(K2,I,J,K)
                  ELSEIF (K.EQ.2) THEN
                     TCLEV(K2,I,J,K) = GPRAMULT(2)*TCLEV(K2,I,J,K)
                  ELSEIF (K.EQ.3) THEN
                     TCLEV(K2,I,J,K) = GPRAMULT(3)*TCLEV(K2,I,J,K)
                  ENDIF

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = N_FYR - PLPD_SYR(XYR) + 1
                     WRITE(HF_HMMO,9010) K2+1989,J2,XYR,FYR,I,J,K,TCLEV(K2,I,J,K), &
                        PV_CCST_IP,PV_CCST_TD,PV_FUEL_IP,PV_FUEL_TD,PV_VOM__IP,PV_VOM__TD, &
                        PV_FOM__IP,PV_FOM__TD,CCST_IP(1),CCST_IP(FYR),FUEL_IP(1), &
                        FUEL_IP(FYR),VOM__IP(1),VOM__IP(FYR),FOM__IP(1),FOM__IP(FYR), &
                        CCST_TD(1),CCST_TD(FYR),FUEL_TD(1),FUEL_TD(FYR),VOM__TD(1), &
                        VOM__TD(FYR),FOM__TD(1),FOM__TD(FYR)
9010                 FORMAT(1X,"TCLEVrsc",7(":",I4),25(":",F14.3))
                  END IF

               ENDDO    ! K=1,TCHTCH



               ! From City Gate Production

               DO K=1,TGHTCH

                  ! Initialize the cost variables
                  DO YRIDX = 1,HMM_MAX_FYR
                      CCST_IP(YRIDX) = 0.0
                      CCST_TD(YRIDX) = 0.0
                      FUEL_IP(YRIDX) = 0.0
                      FUEL_TD(YRIDX) = 0.0
                      VOM__IP(YRIDX) = 0.0
                      VOM__TD(YRIDX) = 0.0
                      FOM__IP(YRIDX) = 0.0
                      FOM__TD(YRIDX) = 0.0
                  END DO

                  DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1

                    FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                    M_FYR = MIN(FYR , LASTYR)

                    ! Calculate the natural log of transport miles
                    ! Miles = NIMBY distance for city-gate facilities!
                    TEMP1    = MDCTP
                    LOGMILES = LOG(TEMP1)

                    ! Calculate the demand step variable
                    IF (Q .LT. 25000) THEN
                        DMDSTEP = 1
                    ELSE
                        DMDSTEP = 0
                    ENDIF

                    ! Calculate the service line miles [P7]
                    SLM = EXP( P7(12)*DMDSTEP + P7(2)*LOGQ + P7(9)*LOG(TMPAREA**0.5) + &
                                P7(4)*LOG(STATNS(2)) )

                    ! Calculate the trunk line miles [P8]
                    TLM = EXP( P8(12)*DMDSTEP + P8(2)*LOGQ + &
                                P8(9)*LOG(TMPAREA**0.5) + &
                                P8(4)*LOG(STATNS(2)) )

                    IF (K .EQ. 1) THEN
                        ! Compressed gas trucks

                        ! City gate GASEOUS truck O&M costs [G10+G11]
                        TRUCK       = EXP( G10(1) + G10(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G10(3)*LOGMILES )
                        TERMINAL    = EXP( G11(1) + G11(2)*LOGQ + &
                                            G11(11)*(LOGQ - LOG(STATNS(2))) )
                        ANNOM       = TRUCK + TERMINAL

                        ! City gate GASEOUS truck capital costs [(G3 or G4)=G5+G7]
                        ! [G17]
                        TRUCKS = EXP( G17(1) + G17(2)*LOGQ + G17(3)*LOGMILES + &
                                    G17(8)*LOG(TMPAREA) + G17(4)*LOG(STATNS(2)) )

                        ! Ensure TRUCKS is > 1 before applying the LOG function
                        IF (TRUCKS .GE. 1) THEN
                            LOGTRUCKS = LOG(TRUCKS)
                        ELSE
                            LOGTRUCKS = 0
                        ENDIF

                        IF (LOGMILES.GT.0) THEN
                            ! [G3]
                            PSI2700B = EXP( G3(1) + G3(10)*LOGTRUCKS + &
                                        G3(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G3(2)*LOGQ )
                            ! [G4]
                            PSI7000B = EXP( G4(1) + G4(10)*LOGTRUCKS + &
                                        G4(11)*(LOGQ - LOG(STATNS(2))) )
                            TRUCK    = ( FCR_TRUCK * MIN(PSI2700B,PSI7000B) ) / (Q*365)
                        ELSE
                            ! [G1]
                            PSI2700A = EXP( G1(1) + G1(10)*LOGTRUCKS + &
                                        G1(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G1(8)*LOG(TMPAREA) )
                            ! [G2]
                            PSI7000A = EXP( G2(1) + G2(10)*LOGTRUCKS + &
                                        G2(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G2(8)*LOG(TMPAREA) )
                            TRUCK    = ( FCR_TRUCK * MIN(PSI2700A,PSI7000A) ) / (Q*365)
                        ENDIF

                        ! [G7]
                        TERMINAL = ( FCR_TRUCK * 1.11 * EXP( G7(1) + &
                                    G7(2)*LOGQ + G7(4)*LOG(STATNS(2)) ) ) / (Q*365)
                        ANNCCST  = TRUCK + TERMINAL

                        ! City gate GASEOUS truck fuel costs [G14+G15]
                        ! [G14]
                        TRUCK    = EXP( G14(1) + G14(2)*LOGQ + &
                                        G14(4)*LOG(STATNS(2)) + &
                                        G14(3)*LOGMILES )
                        ! [G15]
                        TERMINAL    = EXP( G15(1) + G15(2)*LOGQ + &
                                        G15(11)*(LOGQ - LOG(STATNS(2))) )
                        ANNFUEL     = TRUCK + TERMINAL

                        ! Adjusted annual gas truck transport costs
                        CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                        VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        INSTALL_COST(K2,CINX,MINX,MAP_TG(K,1)) = ANNCCST / FCR_TRUCK / MC_JPGDP(YRDOLD-1989)
                        ANNUITY_COST(K2,CINX,MINX,MAP_TG(K,1)) = ANNCCST / MC_JPGDP(YRDOLD-1989)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9103) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                TRUCKS,TRUCK,TERMINAL, &
                                PSI2700B,PSI7000B, &
                                MTTM(K2,CINX,MINX),TMPAREA, &
                                RORTD,FCR_TRUCK
9103                         FORMAT(1X,"TGGAS",9(":",I4),21(":",F16.3))
                          END IF

                    ELSEIF (K .EQ. 2) THEN
                        ! Liquid trucks

                        ! [L13]
                        TRUCKS      = EXP(L13(1) + L13(2)*LOGQ + L13(3)*LOGMILES)

                        ! Ensure TRUCKS is > 1 before applying the LOG function
                        IF (TRUCKS .GE. 1) THEN
                            LOGTRUCKS = LOG(TRUCKS)
                        ELSE
                            LOGTRUCKS = 0
                        ENDIF

                        ! City gate LIQUID truck capital cost [L1+L2+L3]
                        ! For Liquifier use total area quantity because at central plant (mult by cities OK, since T only for J=1)
                        ! [L1]
                        TRUCK       = ( FCR_LIQUIFIER * EXP( L1(1) + &
                                        L1(10)*LOGTRUCKS ) ) / (Q*365)
                        ! [L2]
                        TERMINAL    = ( FCR_LIQUIFIER * 1.745 * EXP( L2(1) + &
                                        L2(2)*LOGQ) ) / (Q*365)
                        ! [L3]
                        LIQUIFIER   = ( FCR_LIQUIFIER * 1.745 * EXP( L3(1) + &
                                        L3(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) + &
                                        L3(8)*LOG(TMPAREA) + &
                                        L3(4)*LOG(STATNS(1)) ) ) / (MIN(Q*CITIES(I,J),MAXLIQ)*365)
                        ANNCCST     = TRUCK + TERMINAL + LIQUIFIER

                        ! City gate LIQUID truck O&M costs [L5+L6+L7]
                        ! [L5]
                        TRUCK       = EXP( L5(1) + L5(9)*LOG(TMPAREA**0.5) + &
                                        L5(3)*LOGMILES + L5(12)*DMDSTEP )
                        ! [L6]
                        TERMINAL    = EXP( L6(1) + L6(2)*LOGQ + &
                                        L6(8)*LOG(TMPAREA) + &
                                        L6(4)*LOG(STATNS(1)) )
                        ! [L7]
                        LIQUIFIER   = EXP( L7(1) + L7(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                        ANNOM       = TRUCK + TERMINAL + LIQUIFIER

                        ! City gate LIQUID truck fuel costs [L9+L10+L11]
                        ! [L9]
                        TRUCK       = EXP( L9(1) + L9(9)*LOG(TMPAREA**0.5) + &
                                        L9(3)*LOGMILES + L9(10)*LOGTRUCKS )
                        ! [L10]
                        TERMINAL    = 0
                        ! [L11]
                        LIQUIFIER   = ELECFACTOR * EXP( L11(1) + L11(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                        ANNFUEL     = TRUCK + TERMINAL + LIQUIFIER

                        ! Adjusted annual liquid truck transport costs
                        CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                        VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        INSTALL_COST(K2,CINX,MINX,MAP_TG(K,1)) = ANNCCST / FCR_LIQUIFIER / MC_JPGDP(YRDOLD-1989)
                        ANNUITY_COST(K2,CINX,MINX,MAP_TG(K,1)) = ANNCCST / MC_JPGDP(YRDOLD-1989)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9111) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(1), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                TRUCKS,TRUCK,TERMINAL,LIQUIFIER
9111                         FORMAT(1X,"TGLQ",9(":",I4),16(":",F16.3))
                          END IF

                    ELSEIF (K .EQ. 3) THEN
                        ! Pipeline

                        ! City gate PIPELINE capital costs [P1]
                        ANNCCST      = ( FCR_PIPE * EXP( P1(1) + P1(2)*LOGQ + &
                                        P1(3)*LOGMILES ) ) / (Q*365)

                        ! City gate PIPELINE O&M costs [P2]
                        ANNOM        = EXP( P2(1) + P2(2)*LOGQ + &
                                        P2(3)*LOGMILES )

                        ! City gate PIPELINE fuel costs
                        ANNFUEL      = 0

                        ! Adjusted annual pipeline transport costs
                        CCST_IP(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_IP(JYR) = 0
                        VOM__IP(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        INSTALL_COST(K2,CINX,MINX,MAP_TG(K,1)) = ANNCCST / FCR_PIPE / MC_JPGDP(YRDOLD-1989)
                        ANNUITY_COST(K2,CINX,MINX,MAP_TG(K,1)) = ANNCCST / MC_JPGDP(YRDOLD-1989)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9500) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_IP(JYR),FUEL_IP(JYR),VOM__IP(JYR)
9500                         FORMAT(1X,"TGPIPE",9(":",I4),12(":",F16.3))
                          END IF

                    ELSE
                        ! *** SHOULD NEVER HAPPEN!! ***
                        ! Transport options are gas truck, liquid truck, and pipeline

                    ENDIF

                  END DO

                  FYR = N_FYR - PLPD_SYR(XYR) + 1

                  ! Calculate the present value of all cost components
                  PV_CCST_IP = PVV(CCST_IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                  PV_CCST_TD = PVV(CCST_TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)
                  PV_FUEL_IP = PVV(FUEL_IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                  PV_FUEL_TD = PVV(FUEL_TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)
                  PV_VOM__IP = PVV(VOM__IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                  PV_VOM__TD = PVV(VOM__TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)
                  PV_FOM__IP = PVV(FOM__IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                  PV_FOM__TD = PVV(FOM__TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)

                  TGLEV(K2,I,J,K) = PV_CCST_IP + PV_CCST_TD + PV_FUEL_IP + &
                                    PV_FUEL_TD + PV_VOM__IP + PV_VOM__TD + &
                                    PV_FOM__IP + PV_FOM__TD

                  ! Price adjustment factors for GPRA
                  IF (K.EQ.1) THEN
                     TGLEV(K2,I,J,K) = GPRAMULT(1)*TGLEV(K2,I,J,K)
                  ELSEIF (K.EQ.2) THEN
                     TGLEV(K2,I,J,K) = GPRAMULT(2)*TGLEV(K2,I,J,K)
                  ELSEIF (K.EQ.3) THEN
                     TGLEV(K2,I,J,K) = GPRAMULT(3)*TGLEV(K2,I,J,K)
                  ENDIF

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = N_FYR - PLPD_SYR(XYR) + 1
                     WRITE(HF_HMMO,9011) K2+1989,J2,XYR,FYR,I,J,K,TGLEV(K2,I,J,K),PV_CCST_IP,PV_CCST_TD,PV_FUEL_IP,PV_FUEL_TD,PV_VOM__IP,PV_VOM__TD,PV_FOM__IP,PV_FOM__TD, &
                        CCST_IP(1),CCST_IP(FYR),FUEL_IP(1),FUEL_IP(FYR),VOM__IP(1),VOM__IP(FYR),FOM__IP(1),FOM__IP(FYR), &
                        CCST_TD(1),CCST_TD(FYR),FUEL_TD(1),FUEL_TD(FYR),VOM__TD(1),VOM__TD(FYR),FOM__TD(1),FOM__TD(FYR)
9011                     FORMAT(1X,"TGLEVrsc",7(":",I4),25(":",F16.3))
                  END IF

               ENDDO

            ! There are currently no transport costs to small city and rural markets,
            !  these are accounted for in the distribution costs
            ELSE IF (J .EQ. 2 .OR. J .EQ. 3) THEN
                TCLEV(K2,I,J,1) = 0.0002
                TCLEV(K2,I,J,2) = 0.0002
                TCLEV(K2,I,J,3) = 0.0002
                TGLEV(K2,I,J,1) = 0.0002
                TGLEV(K2,I,J,2) = 0.0002
                TGLEV(K2,I,J,3) = 0.0002
            ENDIF


            !====================================================
            ! *** DISTRIBUTION
            !====================================================

            DO K=1,DOHTCH

              IF (K.EQ.2.OR.K.EQ.4) THEN

                    IF (J.EQ.1) THEN

                        ! If this is Liquid or Gas Truck - Continued and this is a
                        !  large market, set distribution costs to 0 since we have
                        !  accounted for them in the transport costs
                        DOLEV(K2,I,J,K) = 0.0002

                    ELSE

                        ! If this is Liquid or Gas Truck - Continued and this is a
                        !  small or rural market, set distribution costs very high
                        !  since there can be no continuation from these markets
                        DOLEV(K2,I,J,K) = 1000

                    ENDIF

              ELSE

                  ! Initialize the cost variables
                    DO YRIDX = 1,HMM_MAX_FYR
                        CCST_IP(YRIDX) = 0.0
                        CCST_TD(YRIDX) = 0.0
                        FUEL_IP(YRIDX) = 0.0
                        FUEL_TD(YRIDX) = 0.0
                        VOM__IP(YRIDX) = 0.0
                        VOM__TD(YRIDX) = 0.0
                        FOM__IP(YRIDX) = 0.0
                        FOM__TD(YRIDX) = 0.0
                    END DO

                    DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1

                        FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                        M_FYR = MIN(FYR , LASTYR)


                        ! Divide the market area by the number of cities for the large
                        !  markets, use unadjuested area for small and rural markets
                        IF (J.EQ.1) THEN
                            TMPAREA = AREA(I,J) / CITIES(I,J)
                        ELSE
                            TMPAREA = AREA(I,J)
                        ENDIF

                        ! Calculate the natural log of transport miles
                        ! Transport miles = 0 for distribution!
                        LOGMILES = 0

                        ! Calculate the demand step variable
                        IF (Q .LT. 25000) THEN
                            DMDSTEP = 1
                        ELSE
                            DMDSTEP = 0
                        ENDIF

                        ! Calculate the service line miles [P7]
                        SLM = EXP( P7(12)*DMDSTEP + P7(2)*LOGQ + P7(9)*LOG(TMPAREA**0.5) + &
                                    P7(4)*LOG(STATNS(2)) )

                        ! Calculate the trunk line miles [P8]
                        TLM = EXP( P8(12)*DMDSTEP + P8(2)*LOGQ + &
                                    P8(9)*LOG(TMPAREA**0.5) + &
                                    P8(4)*LOG(STATNS(2)) )

                        IF (K.EQ.5) THEN
                            ! Compressed gas trucks

                            ! distribution GASEOUS truck capital costs [(G1 or G2)=G5+G7]
                            ! [G17]
                            TRUCKS = EXP( G17(1) + G17(2)*LOGQ + G17(3)*LOGMILES + &
                                        G17(8)*LOG(TMPAREA) + G17(4)*LOG(STATNS(2)) )

                            ! Ensure TRUCKS is > 1 before applying the LOG function
                            IF (TRUCKS .GE. 1) THEN
                                LOGTRUCKS = LOG(TRUCKS)
                            ELSE
                                LOGTRUCKS = 0
                            ENDIF

                            IF (LOGMILES.GT.0) THEN
                                ! [G3]
                                PSI2700B = EXP( G3(1) + G3(10)*LOGTRUCKS + &
                                            G3(11)*(LOGQ - LOG(STATNS(2))) + &
                                            G3(2)*LOGQ )
                                ! [G4]
                                PSI7000B = EXP( G4(1) + G4(10)*LOGTRUCKS + &
                                            G4(11)*(LOGQ - LOG(STATNS(2))) )
                                TRUCK    = ( FCR_TRUCK * MIN(PSI2700B,PSI7000B) ) / (Q*365)
                            ELSE
                                ! [G1]
                                PSI2700A = EXP( G1(1) + G1(10)*LOGTRUCKS + &
                                            G1(11)*(LOGQ - LOG(STATNS(2))) + &
                                            G1(8)*LOG(TMPAREA) )
                                ! [G2]
                                PSI7000A = EXP( G2(1) + G2(10)*LOGTRUCKS + &
                                            G2(11)*(LOGQ - LOG(STATNS(2))) + &
                                            G2(8)*LOG(TMPAREA) )
                                TRUCK    = ( FCR_TRUCK * MIN(PSI2700A,PSI7000A) ) / (Q*365)
                            ENDIF

                            ! [G7]
                            TERMINAL = ( FCR_TRUCK * 1.11 * EXP( G7(1) + &
                                    G7(2)*LOGQ + G7(4)*LOG(STATNS(2)) ) ) / (Q*365)
                            ANNCCST  = TRUCK + TERMINAL

                            ! distribution GASEOUS truck O&M costs [G9+G11]
                            ! [G9]
                            TRUCK    = EXP( G9(1) + G9(11)*(LOGQ - LOG(STATNS(2))) + &
                                            G9(8)*LOG(TMPAREA) )
                            ! [G11]
                            TERMINAL    = EXP( G11(1) + G11(2)*LOGQ + &
                                            G11(11)*(LOGQ - LOG(STATNS(2))) )
                            ANNOM    = TRUCK + TERMINAL

                            ! distribution GASEOUS truck fuel costs [G13+G15]
                            ! [G13]
                            TRUCK    = EXP( G13(1) + G13(2)*LOGQ + &
                                            G13(4)*LOG(STATNS(2)) + &
                                            G13(8)*LOG(TMPAREA) )
                            ! [G15]
                            TERMINAL    = EXP( G15(1) + G15(2)*LOGQ + &
                                            G15(11)*(LOGQ - LOG(STATNS(2))) )
                            ANNFUEL  = TRUCK + TERMINAL

                            ! Adjusted annual gas truck distribution costs
                            CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                            FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                            VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                            INSTALL_COST(K2,CINX,MINX,MAP_D(K,1)) = ANNCCST / FCR_TRUCK / MC_JPGDP(YRDOLD-1989)
                            ANNUITY_COST(K2,CINX,MINX,MAP_D(K,1)) = ANNCCST / MC_JPGDP(YRDOLD-1989)

                            ! debug
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                                 WRITE(HF_HMMO,9200) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                    Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                    ANNCCST,ANNFUEL,ANNOM, &
                                    CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                    TRUCKS,TRUCK,TERMINAL, &
                                    PSI2700B,PSI7000B, &
                                    MTTM(K2,CINX,MINX),TMPAREA, &
                                    RORTD,FCR_TRUCK
9200                             FORMAT(1X,"DCGAS",9(":",I4),21(":",F16.3))
                              END IF

                        ELSEIF (K.EQ.3) THEN
                            ! Liquid trucks

                            ! [L13]
                            TRUCKS      = EXP(L13(1) + L13(2)*LOGQ + L13(3)*LOGMILES)

                            ! Ensure TRUCKS is > 1 before applying the LOG function
                            IF (TRUCKS .GE. 1) THEN
                                LOGTRUCKS = LOG(TRUCKS)
                            ELSE
                                LOGTRUCKS = 0
                            ENDIF

                            ! City gate LIQUID truck capital cost [L1+L2+L3]
                            ! For Liquifier use total area quantity because at central plant (mult by cities OK, since T only for J=1)
                            ! [L1]
                            TRUCK       = ( FCR_LIQUIFIER * EXP( L1(1) + &
                                            L1(10)*LOGTRUCKS ) ) / (Q*365)
                            ! [L2]
                            TERMINAL    = ( FCR_LIQUIFIER * 1.745 * EXP( L2(1) + &
                                            L2(2)*LOGQ) ) / (Q*365)
                            ! [L3]
                            LIQUIFIER   = ( FCR_LIQUIFIER * 1.745 * EXP( L3(1) + &
                                            L3(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) + &
                                            L3(8)*LOG(TMPAREA) + &
                                            L3(4)*LOG(STATNS(1)) ) ) / (MIN(Q*CITIES(I,J),MAXLIQ)*365)
                            ANNCCST     = TRUCK + TERMINAL + LIQUIFIER

                            ! City gate LIQUID truck O&M costs [L5+L6+L7]
                            ! [L5]
                            TRUCK       = EXP( L5(1) + L5(9)*LOG(TMPAREA**0.5) + &
                                            L5(3)*LOGMILES + L5(12)*DMDSTEP )
                            ! [L6]
                            TERMINAL    = EXP( L6(1) + L6(2)*LOGQ + &
                                            L6(8)*LOG(TMPAREA) + &
                                            L6(4)*LOG(STATNS(1)) )
                            ! [L7]
                            LIQUIFIER   = EXP( L7(1) + L7(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                            ANNOM       = TRUCK + TERMINAL + LIQUIFIER

                            ! City gate LIQUID truck fuel costs [L9+L10+L11]
                            ! [L9]
                            TRUCK       = EXP( L9(1) + L9(9)*LOG(TMPAREA**0.5) + &
                                            L9(3)*LOGMILES + L9(10)*LOGTRUCKS )
                            ! [L10]
                            TERMINAL    = 0
                            ! [L11]
                            LIQUIFIER   = ELECFACTOR * EXP( L11(1) + L11(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                            ANNFUEL     = TRUCK + TERMINAL + LIQUIFIER

                            ! Adjusted annual liquid truck transport costs
                            CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                            FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                            VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                            INSTALL_COST(K2,CINX,MINX,MAP_D(K,1)) = ANNCCST / FCR_LIQUIFIER / MC_JPGDP(YRDOLD-1989)
                            ANNUITY_COST(K2,CINX,MINX,MAP_D(K,1)) = ANNCCST / MC_JPGDP(YRDOLD-1989)

                            ! debug
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                                 WRITE(HF_HMMO,9201) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                    Q,LOGQ,LOGMILES,SLM,TLM,STATNS(1), &
                                    ANNCCST,ANNFUEL,ANNOM, &
                                    CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                    TRUCKS,TRUCK,TERMINAL,LIQUIFIER
9201                             FORMAT(1X,"DCLQ",9(":",I4),16(":",F16.3))
                              END IF

                        ELSEIF (K .EQ. 1) THEN
                            ! Pipeline

                            ! distribution PIPELINE capital costs [P3]
                            ANNCCST      = ( FCR_PIPE * EXP( P3(1) + &
                                            P3(12)*DMDSTEP + P3(2)*LOGQ + &
                                            P3(5)*LOG(SLM) + &
                                            P3(4)*LOG(STATNS(2)) ) ) / (Q*365)

                            ! distribution PIPELINE O&M costs [P4]
                            ANNOM        = EXP( P4(1) + P4(2)*LOGQ + &
                                            P4(7)*LOG(SLM+TLM) )

                            ! Adjusted annual pipeline distribution costs
                            CCST_IP(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                            FUEL_IP(JYR) = 0
                            VOM__IP(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                            INSTALL_COST(K2,CINX,MINX,MAP_D(K,1)) = ANNCCST / FCR_PIPE / MC_JPGDP(YRDOLD-1989)
                            ANNUITY_COST(K2,CINX,MINX,MAP_D(K,1)) = ANNCCST / MC_JPGDP(YRDOLD-1989)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9501) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_IP(JYR),FUEL_IP(JYR),VOM__IP(JYR)
9501                         FORMAT(1X,"DPIPE",9(":",I4),12(":",F16.3))
                          END IF

                        ENDIF

                    END DO

                    FYR = N_FYR - PLPD_SYR(XYR) + 1

                    ! Calculate the present value of all cost components
                    PV_CCST_IP = PVV(CCST_IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                    PV_CCST_TD = PVV(CCST_TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)
                    PV_FUEL_IP = PVV(FUEL_IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                    PV_FUEL_TD = PVV(FUEL_TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)
                    PV_VOM__IP = PVV(VOM__IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                    PV_VOM__TD = PVV(VOM__TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)
                    PV_FOM__IP = PVV(FOM__IP,HMM_MAX_FYR,FYR,RORPIP) * PWF(RORPIP,PLPD_SYR(XYR)-1)
                    PV_FOM__TD = PVV(FOM__TD,HMM_MAX_FYR,FYR,RORTD) * PWF(RORTD,PLPD_SYR(XYR)-1)

                    DOLEV(K2,I,J,K) = PV_CCST_IP + PV_CCST_TD + PV_FUEL_IP + &
                                        PV_FUEL_TD + PV_VOM__IP + PV_VOM__TD + &
                                        PV_FOM__IP + PV_FOM__TD

                    ! Price adjustment factors for GPRA
                    IF (K.EQ.5) THEN
                       DOLEV(K2,I,J,K) = GPRAMULT(1)*DOLEV(K2,I,J,K)
                    ELSEIF (K.EQ.3) THEN
                       DOLEV(K2,I,J,K) = GPRAMULT(2)*DOLEV(K2,I,J,K)
                    ELSEIF (K.EQ.1) THEN
                       DOLEV(K2,I,J,K) = GPRAMULT(3)*DOLEV(K2,I,J,K)
                    ENDIF

                    IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                       FYR = N_FYR - PLPD_SYR(XYR) + 1
                       WRITE(HF_HMMO,9017) K2+1989,J2,XYR,FYR,I,J,K,DOLEV(K2,I,J,K), &
                            PV_CCST_IP,PV_CCST_TD, &
                            PV_FUEL_IP,PV_FUEL_TD,PV_VOM__IP,PV_VOM__TD,PV_FOM__IP,PV_FOM__TD, &
                            CCST_IP(1),CCST_IP(FYR),FUEL_IP(1),FUEL_IP(FYR),VOM__IP(1),VOM__IP(FYR), &
                            FOM__IP(1),FOM__IP(FYR),CCST_TD(1),CCST_TD(FYR),FUEL_TD(1),FUEL_TD(FYR), &
                            VOM__TD(1),VOM__TD(FYR),FOM__TD(1),FOM__TD(FYR)
9017                   FORMAT(1X,"DOLEVrsc",7(":",I4),25(":",F16.3))
                    END IF

                ENDIF ! IF K=2 OR K=4

            END DO  ! DOHTCH


            ! *** Dispensing Facilities ***

            DO K = 1 , SOHTCH

               IF (MAP_S(K,J2) .GT. 0) THEN

                   ! CONSTRUCTION LEAD TIME -- ACTUAL
                   CLT = 1

                   ! OVERNIGHT COSTS
                   ! Note: We are using the Forecourt SMR scaling factor here for both liquid and gas dispensing capital costs
                   OVRCST = SOCT(K2,K) * PFRCC(K2,I,J,2) / (SOUTZ(K2,K) * 365.0) / HMM_GNPD(YRDOLP-1989)

                   ! ANNUAL EXPENDITURE PERCENTAGE
                   PROFILE(1) = 1.0

                   ! CAPITAL ESCALATION/REAL
                   CAPESC(1) = 1.0

                   ! GENERAL INFLATION
                   GNPF(1) = HMM_GNPD(MAX(1 , K2 + PLPD_SYR(XYR) - CLT - 1))

                   ! INTEREST COSTS
                   INTR = CODHM + MC_RMTCM10Y(K2) / 100.0

                   ! CONSTRUCTION DEBT FINANCING
                   DEBT_F = 1.0 - EQTYHM

                   ! RATE OF RETURN
                   ROR = RORHM

                   CALL INSTALL_CST(CLT,MAX_LCP,OVRCST,PROFILE,CAPESC,GNPF,INTR,DEBT_F,ROR,TXBOOK,BVBOOK)

                   ! DEBT LOAN LIFE <= CONTRACT LIFE
                   DL2 = SOLIF(K2,K)

                   ! CONTRACT LIFE
                   CL = SOLIF(K2,K)

                   ! TAX LIFE ASSOCIATED WITH TECHNOLOGY
                   TL = SOLIF(K2,K)

                   ! ROE
                   ROE = COEHM + MC_RMTCM10Y(K2) / 100.0

                   ! INTEREST RATE
                   INT = CODHM + MC_RMTCM10Y(K2) / 100.0

                   ! DEBT FRACTION
                   DF2 = 1.0 - EQTYHM

                   ! MARGINAL FEDERAL INCOME TAX RATE
                   TR = 0.38

                   ! TX TO BV RATIO
                   RATIO = TXBOOK / BVBOOK

                   CALL ANNUITY_FACTOR(TL,CL,DL2,DF2,RATIO,ROE,INT,TR,CAP,HF_HMMO)

                   INSTALL_COST(K2,CINX,MINX,MAP_S(K,1)) = BVBOOK       / HMM_GNPD(K2)
                   ANNUITY_COST(K2,CINX,MINX,MAP_S(K,1)) = CAP * BVBOOK / HMM_GNPD(K2)

                  ! debug
                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                      WRITE(HF_HMMO,9118) K2+1989,CURITR,XYR,FYR,I,J,K, &
                          CLT,MAX_LCP,TL,CL,DL2,SOLIF(K2,K), &
                          OVRCST,PROFILE(1),CAPESC(1),GNPF(1),INTR,DEBT_F,ROR,TXBOOK,BVBOOK, &
                          DF2,RATIO,ROE,INT,TR,CAP,SOCT(K2,K),SORCC(K2,I,J,K),SOUTZ(K2,K)
9118                  FORMAT(1X,"SOCAPEXP",13(":",I4),18(":",F9.3))
                  ENDIF

                   ! Initialize the cost variables
                   DO YRIDX = 1,HMM_MAX_FYR
                       CCST(YRIDX) = 0.0
                       ELEC(YRIDX) = 0.0
                       VOM_(YRIDX) = 0.0
                       FOM_(YRIDX) = 0.0
                       INFL(YRIDX) = 0.0
                       KG(YRIDX)   = 0.0
                   END DO

                   DO JYR = 1 , N_FYR - PLPD_SYR(XYR) + 1
                      FYR = JYR + PLPD_SYR(XYR) + K2 - 2
                      M_FYR = MIN(FYR , LASTYR)
                      CCST(JYR) = CAP * BVBOOK
                      FOM_(JYR) = SOFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                      VOM_(JYR) = SOVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                      ELEC(JYR) = SOELC(M_FYR,K) / 1000000.0 * PELIN(I,M_FYR) * HMM_GNPD(FYR)
                      INFL(JYR) = HMM_GNPD(FYR)
                      KG(JYR) = 1.0
                   END DO
                   FYR = N_FYR - PLPD_SYR(XYR) + 1
                   PV_CCST = PVV(CCST,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                   PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                   PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                   PV_ELEC = PVV(ELEC,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                   PV_INFL = PVV(INFL,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)
                   PV_KG = PVV(KG,HMM_MAX_FYR,FYR,RORHM) * PWF(RORHM,PLPD_SYR(XYR)-1)

                   SOLEV(K2,I,J,K) = PV_CCST + PV_FOM_ + PV_VOM_ + PV_ELEC

                   IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                      FYR = N_FYR - PLPD_SYR(XYR) + 1
                      WRITE(HF_HMMO,9015) K2+1989,J2,XYR,FYR,I,J,K, &
                            SOLEV(K2,I,J,K),PV_CCST,PV_FOM_,PV_VOM_,PV_ELEC, PV_KG, &
                            SOCT(K2,K),SOFOM(K2,K),SOVOM(K2,K),  &
                            SOELC(K2,K),PELIN(I,K2),CCST(1),FOM_(1),FOM_(FYR), &
                            VOM_(1),ELEC(1),ELEC(FYR),OVRCST,INFL(1),INFL(FYR),CAP, &
                            CCST(1),CCST(FYR),BVBOOK
    9015                  FORMAT(1X,"SOLEVrsc",7(":",I4),24(":",F10.3))
                   END IF

               ELSE
                    ! Future use
                    SOLEV(K2,I,J,K) = 100

               END IF   ! IF (MAP_S(K,J2) .GT. 0) THEN

            ENDDO

         !+++++++++++++++++++++++++++++++++++++++++++++++++++++
         ELSE             ! **** Existing ****
         !+++++++++++++++++++++++++++++++++++++++++++++++++++++

            ! Large city transport costs
            IF (J.EQ.1) THEN

               ! From Central Production

               DO K=1,TCHTCH

                  ! Initialize the cost variables
                  DO YRIDX = 1,HMM_MAX_FYR
                      CCST_IP(YRIDX) = 0.0
                      CCST_TD(YRIDX) = 0.0
                      FUEL_IP(YRIDX) = 0.0
                      FUEL_TD(YRIDX) = 0.0
                      VOM__IP(YRIDX) = 0.0
                      VOM__TD(YRIDX) = 0.0
                      FOM__IP(YRIDX) = 0.0
                      FOM__TD(YRIDX) = 0.0
                  END DO

                  DO JYR = 1 , PLPD_EYR(XYR)

                    FYR = JYR + K2 - 1
                    M_FYR = MIN(FYR , LASTYR)

                    ! Divide the market area by the number of cities for the large
                    !  markets, use unadjuested area for small and rural markets
                    IF (J.EQ.1) THEN
                        TMPAREA = AREA(I,J) / CITIES(I,J)
                    ELSE
                        TMPAREA = AREA(I,J)
                    ENDIF

                    ! Calculate the natural log of transport miles
                    IF (MTTM(K2,CINX,MINX) .GE. 1) THEN
                        LOGMILES = LOG(MTTM(K2,CINX,MINX))
                    ELSE
                        LOGMILES = 0
                    ENDIF

                    ! Calculate the demand step variable
                    IF (Q .LT. 25000) THEN
                        DMDSTEP = 1
                    ELSE
                        DMDSTEP = 0
                    ENDIF

                    ! Calculate the service line miles [P7]
                    SLM = EXP( P7(12)*DMDSTEP + P7(2)*LOGQ + P7(9)*LOG(TMPAREA**0.5) + &
                                P7(4)*LOG(STATNS(2)) )

                    ! Calculate the trunk line miles [P8]
                    TLM = EXP( P8(12)*DMDSTEP + P8(2)*LOGQ + &
                                P8(9)*LOG(TMPAREA**0.5) + &
                                P8(4)*LOG(STATNS(2)) )

                    IF (K .EQ. 1) THEN
                        ! Compressed gas trucks

                        ! Central GASEOUS truck O&M costs [G10+G11]
                        TRUCK       = EXP( G10(1) + G10(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G10(3)*LOGMILES )
                        TERMINAL    = EXP( G11(1) + G11(2)*LOGQ + &
                                            G11(11)*(LOGQ - LOG(STATNS(2))) )
                        ANNOM       = TRUCK + TERMINAL

                        ! Central GASEOUS truck capital costs [(G3 or G4)=G5+G7]
                        ! [G17]
                        TRUCKS = EXP( G17(1) + G17(2)*LOGQ + G17(3)*LOGMILES + &
                                    G17(8)*LOG(TMPAREA) + G17(4)*LOG(STATNS(2)) )

                        ! Ensure TRUCKS is > 1 before applying the LOG function
                        IF (TRUCKS .GE. 1) THEN
                            LOGTRUCKS = LOG(TRUCKS)
                        ELSE
                            LOGTRUCKS = 0
                        ENDIF

                        IF (LOGMILES.GT.0) THEN
                            ! [G3]
                            PSI2700B = EXP( G3(1) + G3(10)*LOGTRUCKS + &
                                        G3(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G3(2)*LOGQ )
                            ! [G4]
                            PSI7000B = EXP( G4(1) + G4(10)*LOGTRUCKS + &
                                        G4(11)*(LOGQ - LOG(STATNS(2))) )
                            TRUCK    = ( FCR_TRUCK * MIN(PSI2700B,PSI7000B) ) / (Q*365)
                        ELSE
                            ! [G1]
                            PSI2700A = EXP( G1(1) + G1(10)*LOGTRUCKS + &
                                        G1(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G1(8)*LOG(TMPAREA) )
                            ! [G2]
                            PSI7000A = EXP( G2(1) + G2(10)*LOGTRUCKS + &
                                        G2(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G2(8)*LOG(TMPAREA) )
                            TRUCK    = ( FCR_TRUCK * MIN(PSI2700A,PSI7000A) ) / (Q*365)
                        ENDIF

                        ! [G7]
                        TERMINAL = ( FCR_TRUCK * 1.11 * EXP( G7(1) + &
                                    G7(2)*LOGQ + G7(4)*LOG(STATNS(2)) ) ) / (Q*365)
                        ANNCCST  = TRUCK + TERMINAL

                        ! Central GASEOUS truck fuel costs [G14+G15]
                        ! [G14]
                        TRUCK    = EXP( G14(1) + G14(2)*LOGQ + &
                                        G14(4)*LOG(STATNS(2)) + &
                                        G14(3)*LOGMILES )
                        ! [G15]
                        TERMINAL    = EXP( G15(1) + G15(2)*LOGQ + &
                                        G15(11)*(LOGQ - LOG(STATNS(2))) )
                        ANNFUEL     = TRUCK + TERMINAL

                        ! Adjusted annual gas truck transport costs
                        CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                        VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9100) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                TRUCKS,TRUCK,TERMINAL, &
                                PSI2700B,PSI7000B, &
                                MTTM(K2,CINX,MINX),TMPAREA, &
                                RORTD,FCR_TRUCK
                          END IF

                    ELSEIF (K .EQ. 2) THEN
                        ! Liquid trucks

                        ! [L13]
                        TRUCKS      = EXP(L13(1) + L13(2)*LOGQ + L13(3)*LOGMILES)

                        ! Ensure TRUCKS is > 1 before applying the LOG function
                        IF (TRUCKS .GE. 1) THEN
                            LOGTRUCKS = LOG(TRUCKS)
                        ELSE
                            LOGTRUCKS = 0
                        ENDIF

                        ! Central LIQUID truck capital cost [L1+L2+L3]
                        ! For Liquifier use total area quantity because at central plant (mult by cities OK, since T only for J=1)
                        ! [L1]
                        TRUCK       = ( FCR_LIQUIFIER * EXP( L1(1) + &
                                        L1(10)*LOGTRUCKS ) ) / (Q*365)
                        ! [L2]
                        TERMINAL    = ( FCR_LIQUIFIER * 1.745 * EXP( L2(1) + &
                                        L2(2)*LOGQ) ) / (Q*365)
                        ! [L3]
                        LIQUIFIER   = ( FCR_LIQUIFIER * 1.745 * EXP( L3(1) + &
                                        L3(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) + &
                                        L3(8)*LOG(TMPAREA) + &
                                        L3(4)*LOG(STATNS(1)) ) ) / (MIN(Q*CITIES(I,J),MAXLIQ)*365)
                        ANNCCST     = TRUCK + TERMINAL + LIQUIFIER

                        ! Central LIQUID truck O&M costs [L5+L6+L7]
                        ! [L5]
                        TRUCK       = EXP( L5(1) + L5(9)*LOG(TMPAREA**0.5) + &
                                        L5(3)*LOGMILES + L5(12)*DMDSTEP )
                        ! [L6]
                        TERMINAL    = EXP( L6(1) + L6(2)*LOGQ + &
                                        L6(8)*LOG(TMPAREA) + &
                                        L6(4)*LOG(STATNS(1)) )
                        ! [L7]
                        LIQUIFIER   = EXP( L7(1) + L7(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                        ANNOM       = TRUCK + TERMINAL + LIQUIFIER

                        ! Central LIQUID truck fuel costs [L9+L10+L11]
                        ! [L9]
                        TRUCK       = EXP( L9(1) + L9(9)*LOG(TMPAREA**0.5) + &
                                        L9(3)*LOGMILES + L9(10)*LOGTRUCKS )
                        ! [L10]
                        TERMINAL    = 0
                        ! [L11]
                        LIQUIFIER   = ELECFACTOR * EXP( L11(1) + L11(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                        ANNFUEL     = TRUCK + TERMINAL + LIQUIFIER

                        ! Adjusted annual liquid truck transport costs
                        CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                        VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9101) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(1), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                TRUCKS,TRUCK,TERMINAL,LIQUIFIER
                          END IF

                    ELSEIF (K .EQ. 3) THEN
                        ! Pipeline

                        ! Central PIPELINE capital costs [P1]
                        ANNCCST      = ( FCR_PIPE * EXP( P1(1) + P1(2)*LOGQ + &
                                        P1(3)*LOGMILES ) ) / (Q*365)

                        ! Central PIPELINE O&M costs [P2]
                        ANNOM        = EXP( P2(1) + P2(2)*LOGQ + &
                                        P2(3)*LOGMILES )

                        ! Central PIPELINE fuel costs
                        ANNFUEL      = 0

                        ! Adjusted annual pipeline transport costs
                        CCST_IP(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_IP(JYR) = 0
                        VOM__IP(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9102) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_IP(JYR),FUEL_IP(JYR),VOM__IP(JYR)
                          END IF

                    ELSE
                        ! *** SHOULD NEVER HAPPEN!! ***
                        ! Transport options are gas truck, liquid truck, and pipeline

                    ENDIF

                  END DO

                  ! Calculate the present value of all cost components
                  PV_CCST_IP = PVV(CCST_IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                  PV_CCST_TD = PVV(CCST_TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)
                  PV_FUEL_IP = PVV(FUEL_IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                  PV_FUEL_TD = PVV(FUEL_TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)
                  PV_VOM__IP = PVV(VOM__IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                  PV_VOM__TD = PVV(VOM__TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)
                  PV_FOM__IP = PVV(FOM__IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                  PV_FOM__TD = PVV(FOM__TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)

                  TCLEX(K2,I,J,K) = PV_FUEL_IP + PV_FUEL_TD + PV_VOM__IP + PV_VOM__TD + &
                                        PV_FOM__IP + PV_FOM__TD

                  ! Price adjustment factors for GPRA
                  IF (K.EQ.1) THEN
                     TCLEX(K2,I,J,K) = GPRAMULT(1)*TCLEX(K2,I,J,K)
                  ELSEIF (K.EQ.2) THEN
                     TCLEX(K2,I,J,K) = GPRAMULT(2)*TCLEX(K2,I,J,K)
                  ELSEIF (K.EQ.3) THEN
                     TCLEX(K2,I,J,K) = GPRAMULT(3)*TCLEX(K2,I,J,K)
                  ENDIF

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = PLPD_EYR(XYR)
                     WRITE(HF_HMMO,9009) K2+1989,J2,XYR,FYR,I,J,K,TCLEX(K2,I,J,K), &
                        PV_CCST_IP,PV_CCST_TD,PV_FUEL_IP,PV_FUEL_TD,PV_VOM__IP,PV_VOM__TD, &
                        PV_FOM__IP,PV_FOM__TD,CCST_IP(1),CCST_IP(FYR),FUEL_IP(1), &
                        FUEL_IP(FYR),VOM__IP(1),VOM__IP(FYR),FOM__IP(1),FOM__IP(FYR), &
                        CCST_TD(1),CCST_TD(FYR),FUEL_TD(1),FUEL_TD(FYR),VOM__TD(1), &
                        VOM__TD(FYR),FOM__TD(1),FOM__TD(FYR)
9009                 FORMAT(1X,"TCLEXrsc",7(":",I4),25(":",F14.3))
                  END IF

               ENDDO    !TCHTCH



               ! From City Gate Production

               DO K=1,TGHTCH

                  ! Initialize the cost variables
                  DO YRIDX = 1,HMM_MAX_FYR
                      CCST_IP(YRIDX) = 0.0
                      CCST_TD(YRIDX) = 0.0
                      FUEL_IP(YRIDX) = 0.0
                      FUEL_TD(YRIDX) = 0.0
                      VOM__IP(YRIDX) = 0.0
                      VOM__TD(YRIDX) = 0.0
                      FOM__IP(YRIDX) = 0.0
                      FOM__TD(YRIDX) = 0.0
                  END DO

                  DO JYR = 1 , PLPD_EYR(XYR)

                    FYR = JYR + K2 - 1
                    M_FYR = MIN(FYR , LASTYR)

                    ! Divide the market area by the number of cities for the large
                    !  markets, use unadjuested area for small and rural markets
                    IF (J.EQ.1) THEN
                        TMPAREA = AREA(I,J) / CITIES(I,J)
                    ELSE
                        TMPAREA = AREA(I,J)
                    ENDIF

                    ! Calculate the natural log of transport miles
                    ! Miles = NIMBY distance for city-gate facilities!
                    TEMP1    = MDCTP
                    LOGMILES = LOG(TEMP1)

                    ! Calculate the demand step variable
                    IF (Q .LT. 25000.0) THEN
                        DMDSTEP = 1
                    ELSE
                        DMDSTEP = 0
                    ENDIF

                    ! Calculate the service line miles [P7]
                    SLM = EXP( P7(12)*DMDSTEP + P7(2)*LOGQ + P7(9)*LOG(TMPAREA**0.5) + &
                                P7(4)*LOG(STATNS(2)) )

                    ! Calculate the trunk line miles [P8]
                    TLM = EXP( P8(12)*DMDSTEP + P8(2)*LOGQ + &
                                P8(9)*LOG(TMPAREA**0.5) + &
                                P8(4)*LOG(STATNS(2)) )

                    IF (K .EQ. 1) THEN
                        ! Compressed gas trucks

                        ! City gate GASEOUS truck O&M costs [G10+G11]
                        TRUCK       = EXP( G10(1) + G10(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G10(3)*LOGMILES )
                        TERMINAL    = EXP( G11(1) + G11(2)*LOGQ + &
                                            G11(11)*(LOGQ - LOG(STATNS(2))) )
                        ANNOM       = TRUCK + TERMINAL

                        ! City gate GASEOUS truck capital costs [(G3 or G4)=G5+G7]
                        ! [G17]
                        TRUCKS = EXP( G17(1) + G17(2)*LOGQ + G17(3)*LOGMILES + &
                                    G17(8)*LOG(TMPAREA) + G17(4)*LOG(STATNS(2)) )

                        ! Ensure TRUCKS is > 1 before applying the LOG function
                        IF (TRUCKS .GE. 1) THEN
                            LOGTRUCKS = LOG(TRUCKS)
                        ELSE
                            LOGTRUCKS = 0
                        ENDIF

                        IF (LOGMILES.GT.0) THEN
                            ! [G3]
                            PSI2700B = EXP( G3(1) + G3(10)*LOGTRUCKS + &
                                        G3(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G3(2)*LOGQ )
                            ! [G4]
                            PSI7000B = EXP( G4(1) + G4(10)*LOGTRUCKS + &
                                        G4(11)*(LOGQ - LOG(STATNS(2))) )
                            TRUCK    = ( FCR_TRUCK * MIN(PSI2700B,PSI7000B) ) / (Q*365)
                        ELSE
                            ! [G1]
                            PSI2700A = EXP( G1(1) + G1(10)*LOGTRUCKS + &
                                        G1(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G1(8)*LOG(TMPAREA) )
                            ! [G2]
                            PSI7000A = EXP( G2(1) + G2(10)*LOGTRUCKS + &
                                        G2(11)*(LOGQ - LOG(STATNS(2))) + &
                                        G2(8)*LOG(TMPAREA) )
                            TRUCK    = ( FCR_TRUCK * MIN(PSI2700A,PSI7000A) ) / (Q*365)
                        ENDIF

                        ! [G7]
                        TERMINAL = ( FCR_TRUCK * 1.11 * EXP( G7(1) + &
                                    G7(2)*LOGQ + G7(4)*LOG(STATNS(2)) ) ) / (Q*365)
                        ANNCCST  = TRUCK + TERMINAL

                        ! City gate GASEOUS truck fuel costs [G14+G15]
                        ! [G14]
                        TRUCK    = EXP( G14(1) + G14(2)*LOGQ + &
                                        G14(4)*LOG(STATNS(2)) + &
                                        G14(3)*LOGMILES )
                        ! [G15]
                        TERMINAL    = EXP( G15(1) + G15(2)*LOGQ + &
                                        G15(11)*(LOGQ - LOG(STATNS(2))) )
                        ANNFUEL     = TRUCK + TERMINAL

                        ! Adjusted annual gas truck transport costs
                        CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                        VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9103) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                TRUCKS,TRUCK,TERMINAL, &
                                PSI2700B,PSI7000B, &
                                MTTM(K2,CINX,MINX),TMPAREA, &
                                RORTD,FCR_TRUCK
                          END IF

                    ELSEIF (K .EQ. 2) THEN
                        ! Liquid trucks

                        ! [L13]
                        TRUCKS      = EXP(L13(1) + L13(2)*LOGQ + L13(3)*LOGMILES)

                        ! Ensure TRUCKS is > 1 before applying the LOG function
                        IF (TRUCKS .GE. 1) THEN
                            LOGTRUCKS = LOG(TRUCKS)
                        ELSE
                            LOGTRUCKS = 0
                        ENDIF

                        ! City gate LIQUID truck capital cost [L1+L2+L3]
                        ! For Liquifier use total area quantity because at central plant (mult by cities OK, since T only for J=1)
                        ! [L1]
                        TRUCK       = ( FCR_LIQUIFIER * EXP( L1(1) + &
                                        L1(10)*LOGTRUCKS ) ) / (Q*365)
                        ! [L2]
                        TERMINAL    = ( FCR_LIQUIFIER * 1.745 * EXP( L2(1) + &
                                        L2(2)*LOGQ) ) / (Q*365)
                        ! [L3]
                        LIQUIFIER   = ( FCR_LIQUIFIER * 1.745 * EXP( L3(1) + &
                                        L3(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) + &
                                        L3(8)*LOG(TMPAREA) + &
                                        L3(4)*LOG(STATNS(1)) ) ) / (MIN(Q*CITIES(I,J),MAXLIQ)*365)
                        ANNCCST     = TRUCK + TERMINAL + LIQUIFIER

                        ! City gate LIQUID truck O&M costs [L5+L6+L7]
                        ! [L5]
                        TRUCK       = EXP( L5(1) + L5(9)*LOG(TMPAREA**0.5) + &
                                        L5(3)*LOGMILES + L5(12)*DMDSTEP )
                        ! [L6]
                        TERMINAL    = EXP( L6(1) + L6(2)*LOGQ + &
                                        L6(8)*LOG(TMPAREA) + &
                                        L6(4)*LOG(STATNS(1)) )
                        ! [L7]
                        LIQUIFIER   = EXP( L7(1) + L7(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                        ANNOM       = TRUCK + TERMINAL + LIQUIFIER

                        ! City gate LIQUID truck fuel costs [L9+L10+L11]
                        ! [L9]
                        TRUCK       = EXP( L9(1) + L9(9)*LOG(TMPAREA**0.5) + &
                                        L9(3)*LOGMILES + L9(10)*LOGTRUCKS )
                        ! [L10]
                        TERMINAL    = 0
                        ! [L11]
                        LIQUIFIER   = ELECFACTOR * EXP( L11(1) + L11(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                        ANNFUEL     = TRUCK + TERMINAL + LIQUIFIER

                        ! Adjusted annual liquid truck transport costs
                        CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                        VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9111) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(1), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                TRUCKS,TRUCK,TERMINAL,LIQUIFIER
                          END IF

                    ELSEIF (K .EQ. 3) THEN
                        ! Pipeline

                        ! City gate PIPELINE capital costs [P1]
                        ANNCCST      = ( FCR_PIPE * EXP( P1(1) + P1(2)*LOGQ + &
                                        P1(3)*LOGMILES ) ) / (Q*365)

                        ! City gate PIPELINE O&M costs [P2]
                        ANNOM        = EXP( P2(1) + P2(2)*LOGQ + &
                                        P2(3)*LOGMILES )

                        ! City gate PIPELINE fuel costs
                        ANNFUEL      = 0

                        ! Adjusted annual pipeline transport costs
                        CCST_IP(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                        FUEL_IP(JYR) = 0
                        VOM__IP(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9500) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_IP(JYR),FUEL_IP(JYR),VOM__IP(JYR)
                          END IF

                    ELSE
                        ! *** SHOULD NEVER HAPPEN!! ***
                        ! Transport options are gas truck, liquid truck, and pipeline

                    ENDIF

                  END DO

                  ! Calculate the present value of all cost components
                  PV_CCST_IP = PVV(CCST_IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                  PV_CCST_TD = PVV(CCST_TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)
                  PV_FUEL_IP = PVV(FUEL_IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                  PV_FUEL_TD = PVV(FUEL_TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)
                  PV_VOM__IP = PVV(VOM__IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                  PV_VOM__TD = PVV(VOM__TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)
                  PV_FOM__IP = PVV(FOM__IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                  PV_FOM__TD = PVV(FOM__TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)

                  TGLEX(K2,I,J,K) = PV_FUEL_IP + PV_FUEL_TD + PV_VOM__IP + PV_VOM__TD + &
                                        PV_FOM__IP + PV_FOM__TD

                  ! Price adjustment factors for GPRA
                  IF (K.EQ.1) THEN
                     TGLEX(K2,I,J,K) = GPRAMULT(1)*TGLEX(K2,I,J,K)
                  ELSEIF (K.EQ.2) THEN
                     TGLEX(K2,I,J,K) = GPRAMULT(2)*TGLEX(K2,I,J,K)
                  ELSEIF (K.EQ.3) THEN
                     TGLEX(K2,I,J,K) = GPRAMULT(3)*TGLEX(K2,I,J,K)
                  ENDIF

                  IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                     FYR = PLPD_EYR(XYR)
                     WRITE(HF_HMMO,9013) K2+1989,J2,XYR,FYR,I,J,K,TGLEX(K2,I,J,K), &
                        PV_CCST_IP,PV_CCST_TD,PV_FUEL_IP,PV_FUEL_TD,PV_VOM__IP,PV_VOM__TD, &
                        PV_FOM__IP,PV_FOM__TD,CCST_IP(1),CCST_IP(FYR),FUEL_IP(1), &
                        FUEL_IP(FYR),VOM__IP(1),VOM__IP(FYR),FOM__IP(1),FOM__IP(FYR), &
                        CCST_TD(1),CCST_TD(FYR),FUEL_TD(1),FUEL_TD(FYR),VOM__TD(1), &
                        VOM__TD(FYR),FOM__TD(1),FOM__TD(FYR)
9013                 FORMAT(1X,"TGLEXrsc",7(":",I4),25(":",F16.3))
                  END IF

               ENDDO        ! TGHTCH

            ! There are currently no transport costs to small city and rural markets,
            !  these are accounted for in the distribution costs
            ELSE IF (J .EQ. 2 .OR. J .EQ. 3) THEN
                TCLEX(K2,I,J,1) = 0.0002
                TCLEX(K2,I,J,2) = 0.0002
                TCLEX(K2,I,J,3) = 0.0002
                TGLEX(K2,I,J,1) = 0.0002
                TGLEX(K2,I,J,2) = 0.0002
                TGLEX(K2,I,J,3) = 0.0002
            ENDIF


            !==================================================
            ! *** DISTRIBUTION COSTS ***
            !==================================================

            DO K=1,DOHTCH

              IF (K.EQ.2.OR.K.EQ.4) THEN

                    IF (J.EQ.1) THEN

                        ! If this is Liquid or Gas Truck - Continued and this is a
                        !  large market, set distribution costs to 0 since we have
                        !  accounted for them in the transport costs
                        DOLEX(K2,I,J,K) = 0.0002

                    ELSE

                        ! If this is Liquid or Gas Truck - Continued and this is a
                        !  small or rural market, set distribution costs very high
                        !  since there can be no continuation from these markets
                        DOLEX(K2,I,J,K) = 1000.0

                    ENDIF

              ELSE

                  ! Initialize the cost variables
                    DO YRIDX = 1,HMM_MAX_FYR
                        CCST_IP(YRIDX) = 0.0
                        CCST_TD(YRIDX) = 0.0
                        FUEL_IP(YRIDX) = 0.0
                        FUEL_TD(YRIDX) = 0.0
                        VOM__IP(YRIDX) = 0.0
                        VOM__TD(YRIDX) = 0.0
                        FOM__IP(YRIDX) = 0.0
                        FOM__TD(YRIDX) = 0.0
                    END DO

                    DO JYR = 1 , PLPD_EYR(XYR)

                        FYR = JYR + K2 - 1
                        M_FYR = MIN(FYR , LASTYR)


                        ! Divide the market area by the number of cities for the large
                        !  markets, use unadjuested area for small and rural markets
                        IF (J.EQ.1) THEN
                            TMPAREA = AREA(I,J) / CITIES(I,J)
                        ELSE
                            TMPAREA = AREA(I,J)
                        ENDIF

                        ! Calculate the natural log of transport miles
                        ! Transport miles = 0 for distribution!
                        LOGMILES = 0

                        ! Calculate the demand step variable
                        IF (Q .LT. 25000) THEN
                            DMDSTEP = 1
                        ELSE
                            DMDSTEP = 0
                        ENDIF

                        ! Calculate the service line miles [P7]
                        SLM = EXP( P7(12)*DMDSTEP + P7(2)*LOGQ + P7(9)*LOG(TMPAREA**0.5) + &
                                    P7(4)*LOG(STATNS(2)) )

                        ! Calculate the trunk line miles [P8]
                        TLM = EXP( P8(12)*DMDSTEP + P8(2)*LOGQ + &
                                    P8(9)*LOG(TMPAREA**0.5) + &
                                    P8(4)*LOG(STATNS(2)) )

                        IF (K.EQ.5) THEN
                            ! Compressed gas trucks

                            ! distribution GASEOUS truck capital costs [(G1 or G2)=G5+G7]
                            ! [G17]
                            TRUCKS = EXP( G17(1) + G17(2)*LOGQ + G17(3)*LOGMILES + &
                                        G17(8)*LOG(TMPAREA) + G17(4)*LOG(STATNS(2)) )

                            ! Ensure TRUCKS is > 1 before applying the LOG function
                            IF (TRUCKS .GE. 1) THEN
                                LOGTRUCKS = LOG(TRUCKS)
                            ELSE
                                LOGTRUCKS = 0
                            ENDIF

                            IF (LOGMILES.GT.0) THEN
                                ! [G3]
                                PSI2700B = EXP( G3(1) + G3(10)*LOGTRUCKS + &
                                            G3(11)*(LOGQ - LOG(STATNS(2))) + &
                                            G3(2)*LOGQ )
                                ! [G4]
                                PSI7000B = EXP( G4(1) + G4(10)*LOGTRUCKS + &
                                            G4(11)*(LOGQ - LOG(STATNS(2))) )
                                TRUCK    = ( FCR_TRUCK * MIN(PSI2700B,PSI7000B) ) / (Q*365)
                            ELSE
                                ! [G1]
                                PSI2700A = EXP( G1(1) + G1(10)*LOGTRUCKS + &
                                            G1(11)*(LOGQ - LOG(STATNS(2))) + &
                                            G1(8)*LOG(TMPAREA) )
                                ! [G2]
                                PSI7000A = EXP( G2(1) + G2(10)*LOGTRUCKS + &
                                            G2(11)*(LOGQ - LOG(STATNS(2))) + &
                                            G2(8)*LOG(TMPAREA) )
                                TRUCK    = ( FCR_TRUCK * MIN(PSI2700A,PSI7000A) ) / (Q*365)
                            ENDIF

                            ! [G7]
                            TERMINAL = ( FCR_TRUCK * 1.11 * EXP( G7(1) + &
                                    G7(2)*LOGQ + G7(4)*LOG(STATNS(2)) ) ) / (Q*365)
                            ANNCCST  = TRUCK + TERMINAL

                            ! distribution GASEOUS truck O&M costs [G9+G11]
                            ! [G9]
                            TRUCK    = EXP( G9(1) + G9(11)*(LOGQ - LOG(STATNS(2))) + &
                                            G9(8)*LOG(TMPAREA) )
                            ! [G11]
                            TERMINAL    = EXP( G11(1) + G11(2)*LOGQ + &
                                            G11(11)*(LOGQ - LOG(STATNS(2))) )
                            ANNOM    = TRUCK + TERMINAL

                            ! distribution GASEOUS truck fuel costs [G13+G15]
                            ! [G13]
                            TRUCK    = EXP( G13(1) + G13(2)*LOGQ + &
                                            G13(4)*LOG(STATNS(2)) + &
                                            G13(8)*LOG(TMPAREA) )
                            ! [G15]
                            TERMINAL    = EXP( G15(1) + G15(2)*LOGQ + &
                                            G15(11)*(LOGQ - LOG(STATNS(2))) )
                            ANNFUEL  = TRUCK + TERMINAL

                            ! Adjusted annual gas truck distribution costs
                            CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                            FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                            VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                            ! debug
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                                 WRITE(HF_HMMO,9200) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                    Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                    ANNCCST,ANNFUEL,ANNOM, &
                                    CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                    TRUCKS,TRUCK,TERMINAL, &
                                    PSI2700B,PSI7000B, &
                                    MTTM(K2,CINX,MINX),TMPAREA, &
                                    RORTD,FCR_TRUCK
                              END IF

                        ELSEIF (K.EQ.3) THEN
                            ! Liquid trucks

                            ! [L13]
                            TRUCKS      = EXP(L13(1) + L13(2)*LOGQ + L13(3)*LOGMILES)

                            ! Ensure TRUCKS is > 1 before applying the LOG function
                            IF (TRUCKS .GE. 1) THEN
                                LOGTRUCKS = LOG(TRUCKS)
                            ELSE
                                LOGTRUCKS = 0
                            ENDIF

                            ! City gate LIQUID truck capital cost [L1+L2+L3]
                            ! For Liquifier use total area quantity because at central plant (mult by cities OK, since T only for J=1)
                            ! [L1]
                            TRUCK       = ( FCR_LIQUIFIER * EXP( L1(1) + &
                                            L1(10)*LOGTRUCKS ) ) / (Q*365)
                            ! [L2]
                            TERMINAL    = ( FCR_LIQUIFIER * 1.745 * EXP( L2(1) + &
                                            L2(2)*LOGQ) ) / (Q*365)
                            ! [L3]
                            LIQUIFIER   = ( FCR_LIQUIFIER * 1.745 * EXP( L3(1) + &
                                            L3(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) + &
                                            L3(8)*LOG(TMPAREA) + &
                                            L3(4)*LOG(STATNS(1)) ) ) / (MIN(Q*CITIES(I,J),MAXLIQ)*365)
                            ANNCCST     = TRUCK + TERMINAL + LIQUIFIER

                            ! City gate LIQUID truck O&M costs [L5+L6+L7]
                            ! [L5]
                            TRUCK       = EXP( L5(1) + L5(9)*LOG(TMPAREA**0.5) + &
                                            L5(3)*LOGMILES + L5(12)*DMDSTEP )
                            ! [L6]
                            TERMINAL    = EXP( L6(1) + L6(2)*LOGQ + &
                                            L6(8)*LOG(TMPAREA) + &
                                            L6(4)*LOG(STATNS(1)) )
                            ! [L7]
                            LIQUIFIER   = EXP( L7(1) + L7(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                            ANNOM       = TRUCK + TERMINAL + LIQUIFIER

                            ! City gate LIQUID truck fuel costs [L9+L10+L11]
                            ! [L9]
                            TRUCK       = EXP( L9(1) + L9(9)*LOG(TMPAREA**0.5) + &
                                            L9(3)*LOGMILES + L9(10)*LOGTRUCKS )
                            ! [L10]
                            TERMINAL    = 0
                            ! [L11]
                            LIQUIFIER   = ELECFACTOR * EXP( L11(1) + L11(2)*LOG(MIN(Q*CITIES(I,J),MAXLIQ)) )
                            ANNFUEL     = TRUCK + TERMINAL + LIQUIFIER

                            ! Adjusted annual liquid truck transport costs
                            CCST_TD(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                            FUEL_TD(JYR) = ANNFUEL / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)
                            VOM__TD(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                            ! debug
                              IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                                 WRITE(HF_HMMO,9201) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                    Q,LOGQ,LOGMILES,SLM,TLM,STATNS(1), &
                                    ANNCCST,ANNFUEL,ANNOM, &
                                    CCST_TD(JYR),FUEL_TD(JYR),VOM__TD(JYR), &
                                    TRUCKS,TRUCK,TERMINAL,LIQUIFIER
                              END IF

                        ELSEIF (K .EQ. 1) THEN
                            ! Pipeline

                            ! distribution PIPELINE capital costs [P3]
                            ANNCCST      = ( FCR_PIPE * EXP( P3(1) + &
                                            P3(12)*DMDSTEP + P3(2)*LOGQ + &
                                            P3(5)*LOG(SLM) + &
                                            P3(4)*LOG(STATNS(2)) ) ) / (Q*365)

                            ! distribution PIPELINE O&M costs [P4]
                            ANNOM        = EXP( P4(1) + P4(2)*LOGQ + &
                                            P4(7)*LOG(SLM+TLM) )

                            ! Adjusted annual pipeline distribution costs
                            CCST_IP(JYR) = ANNCCST / MC_JPGDP(YRDOLD-1989) * MC_JPGDP(K2)
                            FUEL_IP(JYR) = 0
                            VOM__IP(JYR) = ANNOM / MC_JPGDP(YRDOLD-1989) * HMM_GNPD(FYR)

                        ! debug
                          IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                             WRITE(HF_HMMO,9501) K2+1989,J2,XYR,FYR,JYR,I,J,K,DMDSTEP, &
                                Q,LOGQ,LOGMILES,SLM,TLM,STATNS(2), &
                                ANNCCST,ANNFUEL,ANNOM, &
                                CCST_IP(JYR),FUEL_IP(JYR),VOM__IP(JYR)
                          END IF

                        ENDIF

                    END DO

                    ! Calculate the present value of all cost components
                    PV_CCST_IP = PVV(CCST_IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                    PV_CCST_TD = PVV(CCST_TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)
                    PV_FUEL_IP = PVV(FUEL_IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                    PV_FUEL_TD = PVV(FUEL_TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)
                    PV_VOM__IP = PVV(VOM__IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                    PV_VOM__TD = PVV(VOM__TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)
                    PV_FOM__IP = PVV(FOM__IP,HMM_MAX_FYR,PLPD_EYR(XYR),RORPIP) * PWF(RORPIP,-1)
                    PV_FOM__TD = PVV(FOM__TD,HMM_MAX_FYR,PLPD_EYR(XYR),RORTD) * PWF(RORTD,-1)

                    DOLEX(K2,I,J,K) = PV_FUEL_IP + PV_FUEL_TD + PV_VOM__IP + PV_VOM__TD + &
                                        PV_FOM__IP + PV_FOM__TD

                    ! Price adjustment factors for GPRA
                    IF (K.EQ.5) THEN
                       DOLEX(K2,I,J,K) = GPRAMULT(1)*DOLEX(K2,I,J,K)
                    ELSEIF (K.EQ.3) THEN
                       DOLEX(K2,I,J,K) = GPRAMULT(2)*DOLEX(K2,I,J,K)
                    ELSEIF (K.EQ.1) THEN
                       DOLEX(K2,I,J,K) = GPRAMULT(3)*DOLEX(K2,I,J,K)
                    ENDIF

                    IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                       FYR = PLPD_EYR(XYR)
                       WRITE(HF_HMMO,9014) K2+1989,J2,XYR,FYR,I,J,K,DOLEX(K2,I,J,K), &
                            PV_CCST_IP,PV_CCST_TD,PV_FUEL_IP,PV_FUEL_TD,PV_VOM__IP, &
                            PV_VOM__TD,PV_FOM__IP,PV_FOM__TD,CCST_IP(1),CCST_IP(FYR), &
                            FUEL_IP(1),FUEL_IP(FYR),VOM__IP(1),VOM__IP(FYR), &
                            FOM__IP(1),FOM__IP(FYR),CCST_TD(1),CCST_TD(FYR), &
                            FUEL_TD(1),FUEL_TD(FYR),VOM__TD(1),VOM__TD(FYR), &
                            FOM__TD(1),FOM__TD(FYR)
9014                   FORMAT(1X,"DOLEXrsc",7(":",I4),25(":",F16.3))
                    END IF

                ENDIF ! IF K=2 OR K=4

            END DO  ! DOHTCH


            ! *** Dispensing Facilities ***

            DO K = 1 , SOHTCH

               IF (MAP_S(K,J2) .GT. 0) THEN

                   ! CONSTRUCTION LEAD TIME -- ACTUAL
                   CLT = 1

                   ! OVERNIGHT COSTS
                   OVRCST = SOCT(K2,K) / (SOUTZ(K2,K) * 365.0) / HMM_GNPD(YRDOLP-1989)

                   ! ANNUAL EXPENDITURE PERCENTAGE
                   PROFILE(1) = 1.0

                   ! CAPITAL ESCALATION/REAL
                   CAPESC(1) = 1.0

                   ! GENERAL INFLATION
                   GNPF(1) = HMM_GNPD(MAX(1 , K2 + PLPD_SYR(XYR) - CLT - 1))

                   ! INTEREST COSTS
                   INTR = CODHM + MC_RMTCM10Y(K2) / 100.0

                   ! CONSTRUCTION DEBT FINANCING
                   DEBT_F = 1.0 - EQTYHM

                   ! RATE OF RETURN
                   ROR = RORHM

                   CALL INSTALL_CST(CLT,MAX_LCP,OVRCST,PROFILE,CAPESC,GNPF,INTR,DEBT_F,ROR,TXBOOK,BVBOOK)

                   ! DEBT LOAN LIFE <= CONTRACT LIFE
                   DL2 = SOLIF(K2,K)

                   ! CONTRACT LIFE
                   CL = SOLIF(K2,K)

                   ! TAX LIFE ASSOCIATED WITH TECHNOLOGY
                   TL = SOLIF(K2,K)

                   ! ROE
                   ROE = COEHM + MC_RMTCM10Y(K2) / 100.0

                   ! INTEREST RATE
                   INT = CODHM + MC_RMTCM10Y(K2) / 100.0

                   ! DEBT FRACTION
                   DF2 = 1.0 - EQTYHM

                   ! MARGINAL FEDERAL INCOME TAX RATE
                   TR = 0.38

                   ! TX TO BV RATIO
                   RATIO = TXBOOK / BVBOOK

                   CALL ANNUITY_FACTOR(TL,CL,DL2,DF2,RATIO,ROE,INT,TR,CAP,HF_HMMO)

                   ! Initialize the cost variables
                   DO YRIDX = 1,HMM_MAX_FYR
                       CCST(YRIDX) = 0.0
                       ELEC(YRIDX) = 0.0
                       VOM_(YRIDX) = 0.0
                       FOM_(YRIDX) = 0.0
                       INFL(YRIDX) = 0.0
                       KG(YRIDX)   = 0.0
                   END DO

                   DO JYR = 1 , PLPD_EYR(XYR)
                      FYR = JYR + K2 - 1
                      M_FYR = MIN(FYR , LASTYR)
                      CCST(JYR) = CAP * BVBOOK
                      FOM_(JYR) = SOFOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                      VOM_(JYR) = SOVOM(M_FYR,K) / HMM_GNPD(YRDOLP-1989) * HMM_GNPD(FYR)
                      ELEC(JYR) = SOELC(M_FYR,K) / 1000000.0 * PELIN(I,M_FYR) * HMM_GNPD(FYR)
                      INFL(JYR) = HMM_GNPD(FYR)
                      KG(JYR) = 1.0
                   END DO

                   PV_CCST = PVV(CCST,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                   PV_FOM_ = PVV(FOM_,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                   PV_VOM_ = PVV(VOM_,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                   PV_ELEC = PVV(ELEC,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                   PV_INFL = PVV(INFL,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)
                   PV_KG = PVV(KG,HMM_MAX_FYR,PLPD_EYR(XYR),RORHM) * PWF(RORHM,-1)

                   SOLEX(K2,I,J,K) = PV_FOM_ + PV_VOM_ + PV_ELEC

                   IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) THEN
                       FYR = PLPD_EYR(XYR)

                       WRITE(HF_HMMO,9016) K2+1989,J2,XYR,FYR,I,J,K, &
                            SOLEX(K2,I,J,K),PV_CCST,PV_FOM_,PV_VOM_,PV_ELEC, &
                            SORCC(K2,I,J,K),SOCT(K2,K),SOFOM(K2,K),SOVOM(K2,K),  &
                            SOELC(K2,K),PELIN(I,K2),SOLEV(K2,I,J,K), &
                            CLT,MAX_LCP,OVRCST,PROFILE(1),CAPESC(1),GNPF(1),INTR,DEBT_F, &
                            ROR,TXBOOK,BVBOOK
9016                   FORMAT(1X,"SOLEXrsc",7(":",I4),23(":",F16.3))
                    END IF

                ELSE
                    ! Future use
                    SOLEX(K2,I,J,K) = 100

                END IF      ! IF (MAP_S(K,J2) .GT. 0)

            ENDDO       !SOHTCH

         ENDIF  ! J2

      ENDDO     ! DO J2 = 1 , 2 (new,existing)

      RETURN
      END SUBROUTINE GETCST


      SUBROUTINE MARKET_SHARE(CINX,MINX)
! This subroutine redistributes new production capacity expansion market shares
! for technologies close to being competitive but not having the lowest average costs.  Production
! capacity is only shared within categories, not between central, city-gate, and forecourt categories.

      IMPLICIT NONE


      REAL*8 UNIT_COST(PCHTCH+PGHTCH+PFHTCH)
      REAL*8 MARGINAL_COST(PCHTCH+PGHTCH+PFHTCH)
      REAL*8 NEW_SHARE(PCHTCH+PGHTCH+PFHTCH)
      REAL*8 MC_TOTAL
      REAL*8 TOTAL_BUILDS

      REAL*8 BM_SUPPLY,COL_INFO(5)
      REAL*8 MAX_BM_CAP(2)      ! Maximum new biomass gasification capacity based on remaining biomass supply (1=unseq, 2=seq)
      REAL*8 EXCESS_CAP

      REAL*8 MKT_PROD(HMKT)
      REAL*8 TOTAL_PROD

      character*16 COLUMN
      CHARACTER*2 STATUS

      INTEGER*4 K,CINX,MINX,I_SUPt
      INTEGER*4 LOGIT_EXP       ! Logit exponent

      LOGIT_EXP = 11            ! This could be made an hmmcntl input parameter..

      !
      ! Central production
      !

!     CALCULATE THE AVAILABLE BIOMASS SUPPLY TO THIS CENSUS REGION

      BM_SUPPLY = 0.0
      DO CRG = 1 , NDREG
         IF (MAP_CR_TO_CL(CRG) .EQ. CINX) THEN
            DO I_SUPt = 1 , MNUMFS
               IF (WDSUP_AVL(I_SUPt) .EQ. 1 .AND. MP_BM_H2(I_SUPt) .EQ. 1) THEN
                  DO FS = 1 , NBMSTEPS
                     COLUMN = 'S' // BM_TYP_CD(I_SUPt) // FSTEP(FS) // PLPDCOD(1) // FSTEP(CRG)
                     IRET = WFSCOL(COLUMN,'ACLUD   ',STATUS,COL_INFO)
                     BM_SUPPLY = BM_SUPPLY + COL_INFO(4) - COL_INFO(1)

                     IF (FCRL.EQ.1) WRITE(HF_HMMO,13) CURIRUN,CURIYR+1989,CURITR,CINX,CRG,I_SUPt,IRET,FS,&
                        COLUMN,COL_INFO(1),COL_INFO(4),BM_SUPPLY
   13                FORMAT(1X,"BM_SUPPLY",8(":",I4),":",A8,3(":",F14.5))
                  END DO
               END IF
            END DO
         END IF
      END DO

      ! Give only a partial share of the available biomass supply to each market,
      ! the sum of which equal the total available supply
      IF (CURIYR.GT.1) THEN
        MKT_PROD   = 0.0
        TOTAL_PROD = 0.0
         DO J = 1,HMKT
           DO K=1,PCHTCH
             MKT_PROD(J) = MKT_PROD(J) + HMMPRD(CURIYR-1,CINX,J,MAP_PC(K,1))
           ENDDO
           TOTAL_PROD = TOTAL_PROD + MKT_PROD(J)
         ENDDO
         BM_SUPPLY = BM_SUPPLY * (MKT_PROD(MINX)/TOTAL_PROD)
      ELSE
         BM_SUPPLY = BM_SUPPLY/3
      ENDIF

      ! Calculate the maximum biomass production capacity the current available biomass supply can support
      MAX_BM_CAP(1) = BM_SUPPLY / (PCEFF(CURIYR,5)/1000.0)
      MAX_BM_CAP(2) = BM_SUPPLY / (PCEFF(CURIYR,8)/1000.0)

      UNIT_COST     = 0.0
      MARGINAL_COST = 0.0
      NEW_SHARE     = 0.0
      MC_TOTAL      = 0.0
      TOTAL_BUILDS  = 0.0

      DO K=1,PCHTCH

         IF (MAP_PC(K,1) .GT. 0) THEN

            ! Calculate the unit cost minus co-gen credit
            UNIT_COST(K) = HANSW2(2,MAP_PC(K,1)) + HANSW2(2,MAP_GR(1,1))*PCMWH(K)

            ! Calculate the marginal cost of this technology:  (UC - RC)/UC
            MARGINAL_COST(K) = (UNIT_COST(K) - HANSW2(3,MAP_PC(K,1))) / UNIT_COST(K)

            ! Only give share to technologies that are within 20% of being competitive, or to a technology
            ! that had the cheapest cost but could not be chosen because of biomass supply constraints
            IF (MARGINAL_COST(K) .LT. 0.80 .OR. MARGINAL_COST(K) .GT. 1.0) MARGINAL_COST(K) = 0.0

            ! Do not share to technologies that use biomass if there is not enough biomass supply available
            ! to operate a plant that is at least 5% of the standard size
            IF (TFUEL(MAP_PC(K,1)).EQ.'W') THEN
              IF (BM_SUPPLY .LT. 0.05*(PCSSZ(CURIYR,K)*365/1000000000)*(PCEFF(CURIYR,K)/1000.0) &
                     .AND. MARGINAL_COST(K) .NE. 1.0) &
                 MARGINAL_COST(K) = 0.0
            ENDIF

            ! Total up the marginal costs
            MC_TOTAL = MC_TOTAL + MARGINAL_COST(K)**LOGIT_EXP

            ! Total up the new capacity in this production category
            TOTAL_BUILDS = TOTAL_BUILDS + HANSW2(1,MAP_PC(K,1))

            !IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) &
            IF (FCRL .EQ. 1) &
                WRITE(HF_HMMO,10) CURIYR+1989,CURITR,CINX,MINX,K,MAP_PC(K,1), &
                UNIT_COST(K),MARGINAL_COST(K),HANSW2(1,MAP_PC(K,1)),HANSW2(2,MAP_PC(K,1)),HANSW2(3,MAP_PC(K,1)), &
                HANSW2(2,MAP_GR(1,1)),PCMWH(K),LOGIT_EXP

         END IF

      ENDDO

      ! Only allow share in this technology is if the new share is at least 5% of the standard
      ! size for this technology from the hmmindat.txt file.  This is to prevent "unrealistically"
      ! small market shares.
      DO K=1,PCHTCH

        IF (MARGINAL_COST(K) .GT. 0.0) THEN

            NEW_SHARE(K) = (MARGINAL_COST(K)**LOGIT_EXP) / MC_TOTAL

            IF (NEW_SHARE(K)*TOTAL_BUILDS .LT. 0.05*PCSSZ(CURIYR,K)*365/1000000000) THEN

               ! Back out these marginal costs from the total
               MC_TOTAL = MC_TOTAL - MARGINAL_COST(K)**LOGIT_EXP

               ! Zero out this marginal cost to prevent sharing to this technology
               MARGINAL_COST(K) = 0.0

                WRITE(HF_HMMO,12) "PC",CURIYR+1989,CURITR,CINX,MINX,K,MAP_PC(K,1), &
                  NEW_SHARE(K),TOTAL_BUILDS,NEW_SHARE(K)*TOTAL_BUILDS,PCSSZ(CURIYR,K), &
                  0.05*PCSSZ(CURIYR,K)*365/1000000000

            ENDIF

        ENDIF

      ENDDO

      ! Only share to the cheaper biomass technology if both qualify for sharing
      IF (MARGINAL_COST(5).GT.0.0 .AND. MARGINAL_COST(8).GT.0.0) THEN
         IF (UNIT_COST(5).LE.UNIT_COST(8)) THEN
            MC_TOTAL = MC_TOTAL - MARGINAL_COST(8)**LOGIT_EXP        ! Back out the sequestered biomass marginal costs from the total
            MARGINAL_COST(8) = 0.0                                   ! Zero out this marginal cost to prevent sharing to this technology
         ELSE
            MC_TOTAL = MC_TOTAL - MARGINAL_COST(5)**LOGIT_EXP        ! Back out the unsequestered biomass marginal costs from the total
            MARGINAL_COST(5) = 0.0                                   ! Zero out this marginal cost to prevent sharing to this technology
         ENDIF
      ENDIF

      ! Re-initialize NEW_SHARE(K)
      NEW_SHARE  = 0.0

      EXCESS_CAP = 0.0

      ! Now calculate the new market share for each eligible technology and reallocate accordingly
      DO K=1,PCHTCH

        IF (MARGINAL_COST(K) .GT. 0.0) THEN

            NEW_SHARE(K) = (MARGINAL_COST(K)**LOGIT_EXP) / MC_TOTAL

            ! Make sure you do not share more capacity to biomass technologies then the biomass supplies can support
            IF (K.EQ.5 .OR. K.EQ.8) THEN
               IF (K.EQ.5) THEN
                  HANSW2(1,MAP_PC(K,1))              = MIN(MAX_BM_CAP(1),TOTAL_BUILDS*NEW_SHARE(K))    ! For restart
                  TEMP_HANSWR(CINX,MINX,MAP_PC(K,1)) = MIN(MAX_BM_CAP(1),TOTAL_BUILDS*NEW_SHARE(K))    ! For investment decisions

                  ! This capacity needs to be redistributed to the other qualifying technologies
                  EXCESS_CAP = TOTAL_BUILDS*NEW_SHARE(K) - MAX_BM_CAP(1)
               ELSE
                  HANSW2(1,MAP_PC(K,1))              = MIN(MAX_BM_CAP(2),TOTAL_BUILDS*NEW_SHARE(K))    ! For restart
                  TEMP_HANSWR(CINX,MINX,MAP_PC(K,1)) = MIN(MAX_BM_CAP(2),TOTAL_BUILDS*NEW_SHARE(K))    ! For investment decisions

                  ! This capacity needs to be redistributed to the other qualifying technologies
                  EXCESS_CAP = TOTAL_BUILDS*NEW_SHARE(K) - MAX_BM_CAP(2)
               ENDIF
            ELSE
               HANSW2(1,MAP_PC(K,1))              = TOTAL_BUILDS * NEW_SHARE(K)    ! For restart
               TEMP_HANSWR(CINX,MINX,MAP_PC(K,1)) = TOTAL_BUILDS * NEW_SHARE(K)    ! For investment decisions
            ENDIF

            !IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) &
            IF (FCRL .EQ. 1) &
                WRITE(HF_HMMO,11) CURIYR+1989,CURITR,CINX,MINX,K,MAP_PC(K,1), &
                MC_TOTAL,TOTAL_BUILDS,NEW_SHARE(K), &
                HANSW2(1,MAP_PC(K,1)),TEMP_HANSWR(CINX,MINX,MAP_PC(K,1)), &
                LOGIT_EXP,EXCESS_CAP

        ENDIF

      ENDDO

      ! Now we must redistribute any capacity that would have been shared to biomass but could not be becuase there was not enough
      ! available supply
      IF (EXCESS_CAP.GT.0.0) THEN

         IF (MARGINAL_COST(5).GT.0.0) THEN
            MC_TOTAL = MC_TOTAL - MARGINAL_COST(5)**LOGIT_EXP        ! Back out the unsequestered biomass marginal costs from the total
            MARGINAL_COST(5) = 0.0                                   ! Zero out this marginal cost to prevent sharing to this technology
         ENDIF
         IF (MARGINAL_COST(8).GT.0.0) THEN
            MC_TOTAL = MC_TOTAL - MARGINAL_COST(8)**LOGIT_EXP        ! Back out the sequestered biomass marginal costs from the total
            MARGINAL_COST(8) = 0.0                                   ! Zero out this marginal cost to prevent sharing to this technology
         ENDIF

         DO K=1,PCHTCH

            IF (MARGINAL_COST(K) .GT. 0.0) THEN

                NEW_SHARE(K) = (MARGINAL_COST(K)**LOGIT_EXP) / MC_TOTAL

                HANSW2(1,MAP_PC(K,1))              = HANSW2(1,MAP_PC(K,1)) + EXCESS_CAP * NEW_SHARE(K)                 ! For restart
                TEMP_HANSWR(CINX,MINX,MAP_PC(K,1)) = TEMP_HANSWR(CINX,MINX,MAP_PC(K,1)) + EXCESS_CAP * NEW_SHARE(K)    ! For investment decisions

            ENDIF

         ENDDO

      ENDIF

      !
      ! City-gate production
      !

      UNIT_COST     = 0
      MARGINAL_COST = 0
      NEW_SHARE     = 0
      MC_TOTAL      = 0
      TOTAL_BUILDS  = 0

      DO K=1,PGHTCH

         IF (MAP_PG(K,1) .GT. 0) THEN

            ! Calculate the unit cost minus co-gen credit
            UNIT_COST(K) = HANSW2(2,MAP_PG(K,1)) + HANSW2(2,MAP_GR(1,1))*PGMWH(K)

            ! Calculate the marginal cost of this technology:  (UC - RC)/UC
            MARGINAL_COST(K) = (UNIT_COST(K) - HANSW2(3,MAP_PG(K,1))) / UNIT_COST(K)

            ! Only give share to technologies that are within 20% of being competitive
            IF (MARGINAL_COST(K) .LT. 0.80) MARGINAL_COST(K) = 0.0

            ! Total up the marginal costs
            MC_TOTAL = MC_TOTAL + MARGINAL_COST(K)**LOGIT_EXP

            ! Total up the new capacity in this production category
            TOTAL_BUILDS = TOTAL_BUILDS + HANSW2(1,MAP_PG(K,1))

            IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) &
                WRITE(HF_HMMO,10) CURIYR+1989,CURITR,CINX,MINX,K,MAP_PG(K,1), &
                UNIT_COST(K),MARGINAL_COST(K),HANSW2(1,MAP_PG(K,1)),HANSW2(2,MAP_PG(K,1)),HANSW2(3,MAP_PG(K,1)), &
                HANSW2(2,MAP_GR(1,1)),PGMWH(K),LOGIT_EXP

         END IF

      ENDDO

      ! Only allow share in this technology is if the new share is at least 5% of the standard
      ! size for this technology from the hmmindat.txt file.  This is to prevent "unrealistically"
      ! small market shares.
      DO K=1,PGHTCH

        IF (MARGINAL_COST(K) .GT. 0.0) THEN

            NEW_SHARE(K) = (MARGINAL_COST(K)**LOGIT_EXP) / MC_TOTAL

            IF (NEW_SHARE(K)*TOTAL_BUILDS .LT. 0.05*PGSSZ(CURIYR,K)*365/1000000000) THEN

               ! Back out these marginal costs from the total
               MC_TOTAL = MC_TOTAL - MARGINAL_COST(K)**LOGIT_EXP

               ! Zero out this marginal cost to prevent sharing to this technology
               MARGINAL_COST(K) = 0.0

                WRITE(HF_HMMO,12) "PG",CURIYR+1989,CURITR,CINX,MINX,K,MAP_PG(K,1), &
                  NEW_SHARE(K),TOTAL_BUILDS,NEW_SHARE(K)*TOTAL_BUILDS,PGSSZ(CURIYR,K), &
                  0.05*PGSSZ(CURIYR,K)*365/1000000000

            ENDIF

        ENDIF

      ENDDO

      ! Re-initialize NEW_SHARE(K)
      NEW_SHARE = 0.0

      ! Now calculate the new market share for each technology and reallocate accordingly
      DO K=1,PGHTCH

        IF (MARGINAL_COST(K) .GT. 0.0) THEN

            NEW_SHARE(K) = (MARGINAL_COST(K)**LOGIT_EXP) / MC_TOTAL

            HANSW2(1,MAP_PG(K,1)) = TOTAL_BUILDS * NEW_SHARE(K)                 ! For restart

            TEMP_HANSWR(CINX,MINX,MAP_PG(K,1)) = TOTAL_BUILDS * NEW_SHARE(K)    ! For investment decisions

            IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) &
                WRITE(HF_HMMO,11) CURIYR+1989,CURITR,CINX,MINX,K,MAP_PG(K,1), &
                MC_TOTAL,TOTAL_BUILDS,NEW_SHARE(K), &
                HANSW2(1,MAP_PG(K,1)),TEMP_HANSWR(CINX,MINX,MAP_PG(K,1)), &
                LOGIT_EXP

        ENDIF

      ENDDO

      !
      ! Forecourt production
      !

      UNIT_COST     = 0
      MARGINAL_COST = 0
      NEW_SHARE     = 0
      MC_TOTAL      = 0
      TOTAL_BUILDS  = 0

      DO K=1,PFHTCH

         IF (MAP_PF(K,1) .GT. 0) THEN

            ! Calculate the unit cost minus co-gen credit
            UNIT_COST(K) = HANSW2(2,MAP_PF(K,1)) + HANSW2(2,MAP_GR(1,1))*PFMWH(K)

            ! Calculate the marginal cost of this technology:  (UC - RC)/UC
            MARGINAL_COST(K) = (UNIT_COST(K) - HANSW2(3,MAP_PF(K,1))) / UNIT_COST(K)

            ! Only give share to technologies that are within 20% of being competitive
            IF (MARGINAL_COST(K) .LT. 0.80) MARGINAL_COST(K) = 0.0

            ! Total up the marginal costs
            MC_TOTAL = MC_TOTAL + MARGINAL_COST(K)**LOGIT_EXP

            ! Total up the new capacity in this production category
            TOTAL_BUILDS = TOTAL_BUILDS + HANSW2(1,MAP_PF(K,1))

            IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) &
                WRITE(HF_HMMO,10) CURIYR+1989,CURITR,CINX,MINX,K,MAP_PF(K,1), &
                UNIT_COST(K),MARGINAL_COST(K),HANSW2(1,MAP_PF(K,1)),HANSW2(2,MAP_PF(K,1)),HANSW2(3,MAP_PF(K,1)), &
                HANSW2(2,MAP_GR(1,1)),PFMWH(K),LOGIT_EXP

         END IF

      ENDDO

      ! Only allow share in this technology is if the new share is at least 5% of the standard
      ! size for this technology from the hmmindat.txt file.  This is to prevent "unrealistically"
      ! small market shares.
      DO K=1,PFHTCH

        IF (MARGINAL_COST(K) .GT. 0.0) THEN

            NEW_SHARE(K) = (MARGINAL_COST(K)**LOGIT_EXP) / MC_TOTAL

            ! Use the current forecourt station size as determined by the fuel availability
            IF (HFAVL(MINX,CINX,CURIYR) .GE. FA_SWITCH) THEN

               IF (NEW_SHARE(K)*TOTAL_BUILDS .LT. 0.05*TFSZ1(CURIYR,K)*365/1000000000) THEN

                  ! Back out these marginal costs from the total
                  MC_TOTAL = MC_TOTAL - MARGINAL_COST(K)**LOGIT_EXP

                  ! Zero out this marginal cost to prevent sharing to this technology
                  MARGINAL_COST(K) = 0.0

                  WRITE(HF_HMMO,12) "PF",CURIYR+1989,CURITR,CINX,MINX,K,MAP_PF(K,1), &
                    NEW_SHARE(K),TOTAL_BUILDS,NEW_SHARE(K)*TOTAL_BUILDS,TFSZ1(CURIYR,K), &
                    0.05*TFSZ1(CURIYR,K)*365/1000000000

               ENDIF

            ELSE

               IF (NEW_SHARE(K)*TOTAL_BUILDS .LT. 0.05*TFSZ2(CURIYR,K)*365/1000000000) THEN

                  ! Back out these marginal costs from the total
                  MC_TOTAL = MC_TOTAL - MARGINAL_COST(K)**LOGIT_EXP

                  ! Zero out this marginal cost to prevent sharing to this technology
                  MARGINAL_COST(K) = 0.0

                  WRITE(HF_HMMO,12) "PF",CURIYR+1989,CURITR,CINX,MINX,K,MAP_PF(K,1), &
                    NEW_SHARE(K),TOTAL_BUILDS,NEW_SHARE(K)*TOTAL_BUILDS,TFSZ2(CURIYR,K), &
                    0.05*TFSZ2(CURIYR,K)*365/1000000000


               ENDIF

            ENDIF


        ENDIF

      ENDDO

      ! Re-initialize NEW_SHARE(K)
      NEW_SHARE = 0.0

      ! Now calculate the new market share for each technology and reallocate accordingly
      DO K=1,PFHTCH

        IF (MARGINAL_COST(K) .GT. 0.0) THEN

            NEW_SHARE(K) = (MARGINAL_COST(K)**LOGIT_EXP) / MC_TOTAL

            HANSW2(1,MAP_PF(K,1)) = TOTAL_BUILDS * NEW_SHARE(K)                 ! For restart

            TEMP_HANSWR(CINX,MINX,MAP_PF(K,1)) = TOTAL_BUILDS * NEW_SHARE(K)    ! For investment decisions

            IF (FCRL .EQ. 1 .AND. PRTDBGH .EQ.1) &
                WRITE(HF_HMMO,11) CURIYR+1989,CURITR,CINX,MINX,K,MAP_PF(K,1), &
                MC_TOTAL,TOTAL_BUILDS,NEW_SHARE(K), &
                HANSW2(1,MAP_PF(K,1)),TEMP_HANSWR(CINX,MINX,MAP_PF(K,1)), &
                LOGIT_EXP

        ENDIF

      ENDDO

10    FORMAT(1X,"MKTSHRdbg1",6(":",I4),7(":",F10.3),1(":",I4))
11    FORMAT(1X,"MKTSHRdbg2",6(":",I4),5(":",F10.3),":",I4,":",F10.3)
12    FORMAT(1X,"MKTSHRdbg3",":",A2,6(":",I4),5(":",F14.3))

      RETURN
      END SUBROUTINE MARKET_SHARE



      SUBROUTINE COST_REPORT
! This subroutine produces a text report of the yearly installed total costs for each technology
! and a yearly accounting of total annuity payments by technology taking into account the asset lives.

      IMPLICIT NONE


      REAL*8 TOT_INSTALLED(MNUMYR,MNUMCR,HMKT,TOTNDX)
      REAL*8 TOT_ANNUITY(MNUMYR,MNUMCR,HMKT,TOTNDX)

      INTEGER*4 I,J,K,M,K2,II
      INTEGER*4 HF_COST

      HF_COST = 4567
      open(unit=HF_COST,file='hmm_cost.txt')

20    FORMAT(1X,"CST_REP",4(":",I4),2(":",F14.6))

      TOT_INSTALLED = 0.0
      TOT_ANNUITY   = 0.0

      ! Calculate costs in billions of 87$
      DO I = 1,MNUMYR

         DO J = 1,MNUMCR-2

            DO M = 1,HMKT

!
!              Central
!
               DO K=1,PCHTCH
                  K2 = MAP_PC(K,1)
                  IF (K2 .GT. 0) THEN

                     ! Installed costs
                     TOT_INSTALLED(I,J,M,K2) = INSTALL_COST(I,J,M,K2) * NEW_BUILDS(I,J,M,K2)

                     ! Annuity costs
                     DO II = I-PCLIF(I,K),I              ! We are assuming the asset life does not change over time here!!
                        IF (II.GE.1) TOT_ANNUITY(I,J,M,K2) = TOT_ANNUITY(I,J,M,K2) + ANNUITY_COST(II,J,M,K2) * NEW_BUILDS(II,J,M,K2)
                     END DO

                     WRITE(HF_COST,20) I+1989,J,M,K2,TOT_INSTALLED(I,J,M,K2),TOT_ANNUITY(I,J,M,K2)

                  END IF
               ENDDO
!
!              City Gate
!
               DO K=1,PGHTCH
                  K2 = MAP_PG(K,1)
                  IF (K2 .GT. 0) THEN

                     ! Installed costs
                     TOT_INSTALLED(I,J,M,K2) = INSTALL_COST(I,J,M,K2) * NEW_BUILDS(I,J,M,K2)

                     ! Annuity costs
                     DO II = I-PGLIF(I,K),I              ! We are assuming the asset life does not change over time here!!
                        IF (II.GE.1) TOT_ANNUITY(I,J,M,K2) = TOT_ANNUITY(I,J,M,K2) + ANNUITY_COST(II,J,M,K2) * NEW_BUILDS(II,J,M,K2)
                     END DO

                     WRITE(HF_COST,20) I+1989,J,M,K2,TOT_INSTALLED(I,J,M,K2),TOT_ANNUITY(I,J,M,K2)

                  END IF
               ENDDO
!
!              Forecourt
!
               DO K=1,PFHTCH
                  K2 = MAP_PF(K,1)
                  IF (K2 .GT. 0) THEN

                     ! Installed costs
                     TOT_INSTALLED(I,J,M,K2) = INSTALL_COST(I,J,M,K2) * NEW_BUILDS(I,J,M,K2)

                     ! Annuity costs
                     DO II = I-PFLIF(I,K),I              ! We are assuming the asset life does not change over time here!!
                        IF (II.GE.1) TOT_ANNUITY(I,J,M,K2) = TOT_ANNUITY(I,J,M,K2) + ANNUITY_COST(II,J,M,K2) * NEW_BUILDS(II,J,M,K2)
                     END DO

                     WRITE(HF_COST,20) I+1989,J,M,K2,TOT_INSTALLED(I,J,M,K2),TOT_ANNUITY(I,J,M,K2)

                  END IF
               ENDDO

!
!              Transport Central
!
               DO K=1,TCHTCH
                  K2 = MAP_TC(K,1)
                  IF (K2 .GT. 0) THEN

                     ! Installed costs
                     TOT_INSTALLED(I,J,M,K2) = INSTALL_COST(I,J,M,K2) * NEW_BUILDS(I,J,M,K2)

                     ! Annuity costs (asset life is assumed to be 15 years for truck and 20 years for pipelines)
                     IF (TCODTC(K).EQ."PL") THEN
                        DO II = I-20,I              ! We are assuming the asset life does not change over time here!!
                           IF (II.GE.1) TOT_ANNUITY(I,J,M,K2) = TOT_ANNUITY(I,J,M,K2) + ANNUITY_COST(II,J,M,K2) * NEW_BUILDS(II,J,M,K2)
                        END DO
                     ELSE
                        DO II = I-15,I              ! We are assuming the asset life does not change over time here!!
                           IF (II.GE.1) TOT_ANNUITY(I,J,M,K2) = TOT_ANNUITY(I,J,M,K2) + ANNUITY_COST(II,J,M,K2) * NEW_BUILDS(II,J,M,K2)
                        END DO
                     ENDIF

                     WRITE(HF_COST,20) I+1989,J,M,K2,TOT_INSTALLED(I,J,M,K2),TOT_ANNUITY(I,J,M,K2)

                  END IF
               ENDDO
!
!              Transport City Gate
!
               DO K=1,TGHTCH
                  K2 = MAP_TG(K,1)
                  IF (K2 .GT. 0) THEN

                     ! Installed costs
                     TOT_INSTALLED(I,J,M,K2) = INSTALL_COST(I,J,M,K2) * NEW_BUILDS(I,J,M,K2)

                     ! Annuity costs (asset life is assumed to be 15 years for truck and 20 years for pipelines)
                     IF (TCODTG(K).EQ."PL") THEN
                        DO II = I-20,I              ! We are assuming the asset life does not change over time here!!
                           IF (II.GE.1) TOT_ANNUITY(I,J,M,K2) = TOT_ANNUITY(I,J,M,K2) + ANNUITY_COST(II,J,M,K2) * NEW_BUILDS(II,J,M,K2)
                        END DO
                     ELSE
                        DO II = I-15,I              ! We are assuming the asset life does not change over time here!!
                           IF (II.GE.1) TOT_ANNUITY(I,J,M,K2) = TOT_ANNUITY(I,J,M,K2) + ANNUITY_COST(II,J,M,K2) * NEW_BUILDS(II,J,M,K2)
                        END DO
                     ENDIF

                     WRITE(HF_COST,20) I+1989,J,M,K2,TOT_INSTALLED(I,J,M,K2),TOT_ANNUITY(I,J,M,K2)

                  END IF
               ENDDO
!
!              Distribution
!
               DO K=1,DOHTCH
                  K2 = MAP_D(K,1)
                  IF (K2 .GT. 0) THEN

                     ! Installed costs
                     TOT_INSTALLED(I,J,M,K2) = INSTALL_COST(I,J,M,K2) * NEW_BUILDS(I,J,M,K2)

                     ! Annuity costs (asset life is assumed to be 15 years for truck and 20 years for pipelines)
                     IF (TCODD(K).EQ."PL") THEN
                        DO II = I-20,I              ! We are assuming the asset life does not change over time here!!
                           IF (II.GE.1) TOT_ANNUITY(I,J,M,K2) = TOT_ANNUITY(I,J,M,K2) + ANNUITY_COST(II,J,M,K2) * NEW_BUILDS(II,J,M,K2)
                        END DO
                     ELSE
                        DO II = I-15,I              ! We are assuming the asset life does not change over time here!!
                           IF (II.GE.1) TOT_ANNUITY(I,J,M,K2) = TOT_ANNUITY(I,J,M,K2) + ANNUITY_COST(II,J,M,K2) * NEW_BUILDS(II,J,M,K2)
                        END DO
                     ENDIF

                     WRITE(HF_COST,20) I+1989,J,M,K2,TOT_INSTALLED(I,J,M,K2),TOT_ANNUITY(I,J,M,K2)

                  END IF
               ENDDO
!
!              Dispensing
!
               DO K=1,SOHTCH
                  K2 = MAP_S(K,1)
                  IF (K2 .GT. 0) THEN

                     ! Installed costs
                     TOT_INSTALLED(I,J,M,K2) = INSTALL_COST(I,J,M,K2) * NEW_BUILDS(I,J,M,K2)

                     ! Annuity costs
                     DO II = I-SOLIF(I,K),I              ! We are assuming the asset life does not change over time here!!
                        IF (II.GE.1) TOT_ANNUITY(I,J,M,K2) = TOT_ANNUITY(I,J,M,K2) + ANNUITY_COST(II,J,M,K2) * NEW_BUILDS(II,J,M,K2)
                     END DO

                     WRITE(HF_COST,20) I+1989,J,M,K2,TOT_INSTALLED(I,J,M,K2),TOT_ANNUITY(I,J,M,K2)

                  END IF
               ENDDO

            END DO   ! M

         END DO   ! J

      END DO   ! I

      CLOSE(HF_COST)

      RETURN
      END SUBROUTINE COST_REPORT



      SUBROUTINE H2_RFS
! This subroutine sets up renewable fuel supply curves for biomass

      IMPLICIT NONE

      REAL*8 VALUE,DIGITS2,WDQ
      REAL*8 PWF,PVV,T_BM(HMM_MAX_FYR),T_WT(HMM_MAX_FYR)
      INTEGER*4 IRET,YEAR,NERC,IP,IECP,IFPH,PYR,CYR,K2,I_SUPt,TST_CR(MNUMCR)
      character*16 ROW,COL,COLUMN,ROW2

      TST_CR = 0

      K2 = CURIYR

!     Reset HMM Discount Rate

      RORHM = (CODHM + MC_RMTCM10Y(K2) / 100.0) * (1.0 - EQTYHM) + (COEHM + MC_RMTCM10Y(K2) / 100.0) * EQTYHM

      DO YEAR = 1 , N_XYR

         DO CRG = 1 , NDREG

            CINX = MAP_CR_TO_CL(CRG)

            ROW2 = 'FWDCN' // PLPDCOD(YEAR) // RCOD(CINX)

            IF (TST_CR(CINX) .EQ. 0) THEN
               VALUE = DBLE(0.0)
               CALL HRHS(HMM_RHS,ROW2,VALUE)

               IF (CRG .EQ. 7) &
                  WRITE(HF_HMMO,1012) CURIRUN, CURIYR+1989, CURITR, YEAR, CRG, 0, HMM_RHS, ROW2, VALUE

               IRET = DFMCRTP(ROW2,'L       ')
               TST_CR(CINX) = 1
            END IF

            DO I_SUPt = 1 , MNUMFS
               IF (WDSUP_AVL(I_SUPt) .EQ. 1 .AND. MP_BM_H2(I_SUPt) .EQ. 1) THEN

!                 ROW FOR BIOMASS SUPPLY CURVE

                  ROW = 'F' // BM_TYP_CD(I_SUPt) // 'CL' // PLPDCOD(YEAR) // FSTEP(CRG)
                  VALUE = DBLE(0.0)
                  CALL HRHS(HMM_RHS,ROW,VALUE)

                  IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                     WRITE(HF_HMMO,1012) CURIRUN, CURIYR+1989, CURITR, YEAR, CRG, I_SUPt, HMM_RHS,ROW, VALUE
 1012             FORMAT(1X,"HRHS_H2_RFS",6(":",I4),2(":",A8),":",F12.6)

                  IRET = DFMCRTP(ROW,'L       ')

!                 MAKE SUPPLY AVAILABLE TO APPROPRIATE CENSUS REGIONS

                  COLUMN = 'T' // BM_TYP_CD(I_SUPt) // PLPDCOD(YEAR) // FSTEP(CRG) // FSTEP(CINX)
                  VALUE = DBLE(1.0)
                  CALL HVAL(COLUMN,ROW,VALUE)

                  IF (CRG .EQ. 7) &
                     WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW,VALUE
 1013             FORMAT(1X,"HVAL_H2_RFS",6(":",I4),2(":",A8),":",F12.6)

                  VALUE = -1.0
                  CALL HVAL(COLUMN,ROW2,VALUE)

                  IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                     WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW2,VALUE

!                 PUT FUEL SUPPLY VECTORS IN ROW

                  DO FS = 1 , NBMSTEPS + 1
                     COLUMN = 'S' // BM_TYP_CD(I_SUPt) // FSTEP(FS) // PLPDCOD(YEAR) // FSTEP(CRG)
                     VALUE = DBLE(-1.0)
                     CALL HVAL(COLUMN,ROW,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW,VALUE

                  END DO

!                 Account for Other Uses of Biomass

!                 Residential

                  IF (MP_BM_RS(I_SUPt) .EQ. 1) THEN

                     COLUMN = 'D' // BM_TYP_CD(I_SUPt) // 'RS' // PLPDCOD(YEAR) // FSTEP(CRG)

                     VALUE = DBLE(1.0)
                     CALL HVAL(COLUMN,ROW,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW,VALUE

                     VALUE = QBMRSCL(I_SUPt,CRG,MIN(MNUMYR,K2+YEAR-1))
                     VALUE = MAX(DBLE(0.0) , VALUE)

                     CALL HBND(HMM_BND,COLUMN,VALUE,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1014) CURIRUN,CURIYR+1989,CURITR,YEAR,I_SUPt,CRG,HMM_BND,COLUMN,VALUE
 1014                FORMAT(1X,"HBND_H2_RFS",5(":",I4),2(":",A8),":",F12.6)
                  END IF

!                 Commercial

                  IF (MP_BM_CM(I_SUPt) .EQ. 1) THEN

                     COLUMN = 'D' // BM_TYP_CD(I_SUPt) // 'CM' // PLPDCOD(YEAR) // FSTEP(CRG)

                     VALUE = DBLE(1.0)
                     CALL HVAL(COLUMN,ROW,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW,VALUE

                     VALUE = QBMCMCL(I_SUPt,CRG,MIN(MNUMYR,K2+YEAR-1))
                     VALUE = MAX(DBLE(0.0) , VALUE)

                     CALL HBND(HMM_BND,COLUMN,VALUE,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1014) CURIRUN,CURIYR+1989,CURITR,YEAR,I_SUPt,CRG,HMM_BND,COLUMN,VALUE
                  END IF

!                 Industrial

                  IF (MP_BM_IN(I_SUPt) .EQ. 1) THEN

                     COLUMN = 'D' // BM_TYP_CD(I_SUPt) // 'IN' // PLPDCOD(YEAR) // FSTEP(CRG)

                     VALUE = DBLE(1.0)
                     CALL HVAL(COLUMN,ROW,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW,VALUE

                     VALUE = QBMINCL(I_SUPt,CRG,MIN(MNUMYR,K2+YEAR-1))
                     VALUE = MAX(DBLE(0.0) , VALUE)

                     CALL HBND(HMM_BND,COLUMN,VALUE,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1014) CURIRUN,CURIYR+1989,CURITR,YEAR,I_SUPt,CRG,HMM_BND,COLUMN,VALUE
                  END IF

!                 Electric Generation Sector

                  IF (MP_BM_PW(I_SUPt) .EQ. 1) THEN

                     COLUMN = 'D' // BM_TYP_CD(I_SUPt) // 'PW' // PLPDCOD(YEAR) // FSTEP(CRG)

                     VALUE = DBLE(1.0)
                     CALL HVAL(COLUMN,ROW,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW,VALUE

                     VALUE = QBMPWCL(I_SUPt,CRG,MIN(MNUMYR,K2+YEAR-1))
                     VALUE = MAX(DBLE(0.0) , VALUE)

                     CALL HBND(HMM_BND,COLUMN,VALUE,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1014) CURIRUN,CURIYR+1989,CURITR,YEAR,I_SUPt,CRG,HMM_BND,COLUMN,VALUE
                  END IF

!                 Cellulosic Ethanol Production

                  IF (MP_BM_ET(I_SUPt) .EQ. 1) THEN

                     COLUMN = 'D' // BM_TYP_CD(I_SUPt) // 'ET' // PLPDCOD(YEAR) // FSTEP(CRG)

                     VALUE = DBLE(1.0)
                     CALL HVAL(COLUMN,ROW,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW,VALUE

                     VALUE = QBMETCL(I_SUPt,CRG,MIN(MNUMYR,K2+YEAR-1))
                     VALUE = MAX(DBLE(0.0) , VALUE)

                     CALL HBND(HMM_BND,COLUMN,VALUE,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1014) CURIRUN,CURIYR+1989,CURITR,YEAR,I_SUPt,CRG,HMM_BND,COLUMN,VALUE
                  END IF

!                 Biomass to Liquids

                  IF (MP_BM_BT(I_SUPt) .EQ. 1) THEN

                     COLUMN = 'D' // BM_TYP_CD(I_SUPt) // 'BT' // PLPDCOD(YEAR) // FSTEP(CRG)

                     VALUE = DBLE(1.0)
                     CALL HVAL(COLUMN,ROW,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW,VALUE

                     VALUE = QBMBTCL(I_SUPt,CRG,MIN(MNUMYR,K2+YEAR-1))
                     VALUE = MAX(DBLE(0.0) , VALUE)

                     CALL HBND(HMM_BND,COLUMN,VALUE,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1014) CURIRUN,CURIYR+1989,CURITR,YEAR,I_SUPt,CRG,HMM_BND,COLUMN,VALUE
                  END IF

!                 DETERMINE BOUND FOR EACH BIOMASS FUEL SUPPLY VECTOR

                  DO FS = 1 , NBMSTEPS
                     COLUMN = 'S' // BM_TYP_CD(I_SUPt)  // FSTEP(FS) // PLPDCOD(YEAR) // FSTEP(CRG)
                     DO JYR = 1 , PLPD_EYR(YEAR) - PLPD_SYR(YEAR) + 1
                        PYR = MIN(CURIYR + PLPD_SYR(YEAR) + JYR - 2  , LASTYR + HMM_MAX_FYR)
                        IF (FS .GT. 1) THEN
                           T_BM(JYR) = WDSUP_Q(FS,CRG,PYR,I_SUPt) - WDSUP_Q(FS-1,CRG,PYR,I_SUPt)
                        ELSE
                           T_BM(JYR) = WDSUP_Q(FS,CRG,PYR,I_SUPt)
                        END IF
                        T_BM(JYR) = MAX(DBLE(0.0) , T_BM(JYR))
                        T_WT(JYR) = 1.0

                        IF (CURIYR .EQ. 58 .AND. CRG .EQ. 1 .AND. FCRL .EQ. 1) &
                           WRITE(HF_HMMO,3014) CURIRUN,CURIYR+1989,CURITR,YEAR,JYR,PYR, CRG, I_SUPt, FS, T_BM(JYR), &
                              WDSUP_Q(FS,CRG,PYR,I_SUPt), WDSUP_P(FS,CRG,PYR,I_SUPt), HMM_GNPD(PYR)
 3014                   FORMAT(1X,"HBND_H2_RFS_3",9(":",I4),4(":",F14.6))

                     END DO
                     IFPH = PLPD_EYR(YEAR) - PLPD_SYR(YEAR) + 1
                     VALUE = PVV(T_BM,HMM_MAX_FYR,IFPH,RORHM) / PVV(T_WT,HMM_MAX_FYR,IFPH,RORHM)
                     CALL HBND(HMM_BND,COLUMN,DBLE(0.0),VALUE)

                     IF (CURIYR .EQ. 58 .AND. CRG .EQ. 2 .AND. FCRL .EQ. 1) THEN
                        WRITE(HF_HMMO,2014) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,NBMSTEPS,HMM_BND,COLUMN,VALUE, &
                        IFPH, PLPD_EYR(YEAR), PLPD_SYR(YEAR), RORHM, PVV(T_BM,HMM_MAX_FYR,IFPH,RORHM), PVV(T_WT,HMM_MAX_FYR,IFPH,RORHM)
 2014                   FORMAT(1X,"HBND_H2_RFS_2",7(":",I4),2(":",A8),":",F14.6,3(":",I4),3(":",F14.6))
                     END IF

                  END DO

!                 PUT BTU COSTS FOR BIOMASS FUEL SUPPLY CURVE IN OBJ

                  ROW = HMM_OBJ
                  DO FS = 1 , NBMSTEPS
                     COLUMN = 'S' // BM_TYP_CD(I_SUPt)  // FSTEP(FS) // PLPDCOD(YEAR) // FSTEP(CRG)
                     DO JYR = 1 , PLPD_EYR(YEAR) - PLPD_SYR(YEAR) + 1
                        PYR = MIN(CURIYR + PLPD_SYR(YEAR) + JYR - 2  , LASTYR + 5)
                        T_BM(JYR) = HMM_GNPD(PYR) * WDSUP_P(FS,CRG,PYR,I_SUPt)

                        IF (CRG .EQ. 1 .AND. FS .LE. 10) WRITE(HF_HMMO,3999) CURIYR+1989, YEAR, PYR, PLPD_SYR(YEAR), PLPD_EYR(YEAR), &
                           CRG, I_SUPt, FS, JYR, T_BM(JYR), HMM_GNPD(PYR), RORHM, WDSUP_P(FS,CRG,PYR,I_SUPt), WDSUP_Q(FS,CRG,PYR,I_SUPt)
 3999                   FORMAT(1X,"WOODCOST",9(":",I4),5(":",F12.3))

                     END DO
                     IFPH = PLPD_EYR(YEAR) - PLPD_SYR(YEAR) + 1

!                    objective is in billions of dollars so we need to divide by a thousand since ($/mmmbtu * trills = millions of dollars)

                     VALUE = PVV(T_BM,HMM_MAX_FYR,IFPH,RORHM)  * PWF(RORHM,PLPD_SYR(YEAR)-1) / 1000.0

                     IF (CRG .EQ. 1 .AND. FS .LE. 10) THEN
                        PYR = MIN(CURIYR + PLPD_SYR(YEAR) - 1  , LASTYR + HMM_MAX_FYR)

                        WRITE(HF_HMMO,3972) CURIYR+1989, PYR+1989, YEAR, PLPD_SYR(YEAR)+1989, PLPD_EYR(YEAR)+1989, CRG, I_SUPt, FS, &
                           HMM_GNPD(PYR), RORHM, WDSUP_P(FS,CRG,PYR,I_SUPt), WDSUP_Q(FS,CRG,PYR,I_SUPt), VALUE, &
                           PVV(T_BM,HMM_MAX_FYR,IFPH,RORHM), PWF(RORHM,PLPD_SYR(YEAR)-2)
 3972                   FORMAT(1X,"WDSUP_HMM",8(":",I4),7(":",F12.3))
                     END IF

                     CALL HVAL(COLUMN,ROW,VALUE)

                     IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                        WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW,VALUE

                  END DO
                  COLUMN = 'S' // BM_TYP_CD(I_SUPt)  // FSTEP(NBMSTEPS+1) // PLPDCOD(YEAR) // FSTEP(CRG)
                  VALUE = 99.9
                  CALL HVAL(COLUMN,ROW,VALUE)

                  IF (CRG .EQ. 7 .AND. FS .LE. 10) &
                     WRITE(HF_HMMO,1013) CURIRUN,CURIYR+1989,CURITR,YEAR,CRG,I_SUPt,COLUMN,ROW,VALUE

               END IF ! WDSUP_AVL
            END DO ! I_SUPt
         END DO ! YEAR
      END DO ! CRG

      RETURN
      END SUBROUTINE H2_RFS

 END SUBROUTINE HMM_MAIN

      FUNCTION STDCST(I,J,K,VD,VE,VG,VI,VAD,C3,C5,C6,C7)

      IMPLICIT NONE

      REAL*8 VD,VE,VG,VI,VAD,C3,C5,C6,C7
      REAL*8 STDCST
      INTEGER I,J,K

      STDCST = 0.0
      IF (K.EQ.2.OR.K.EQ.6) THEN
          STDCST = 100000/24*VAD*2000
      ELSEIF (K.EQ.15.OR.K.EQ.19) THEN
          STDCST = 125000*VAD
      ELSEIF (K.EQ.3.OR.K.EQ.8) THEN
          STDCST = 1000/VD*2*3000
      ELSEIF (K.EQ.4) THEN
          STDCST = 100*1000/C5/VD
      ELSEIF (K.EQ.16) THEN
          STDCST = 100*1000/C6/VD
      ELSEIF (K.EQ.7) THEN
          STDCST = 20*100000/C7/VD
      ELSEIF (K.EQ.11) THEN
          STDCST = 10/C3*VG*(VE/VG)**VI
      ELSEIF (K.EQ.13) THEN
          STDCST = 100*1000/C5/VD
      ELSEIF(K.EQ.5) THEN
          STDCST = 15000
      ELSEIF(K.EQ.9) THEN
          STDCST = 700*100000
!          STDCST = 700                          !low cost Liquid Trucks
      ELSEIF(K.EQ.10) THEN
          STDCST = 5*100000
      ELSEIF(K.EQ.12) THEN
          STDCST = 250000
!          STDCST = 250                          !low cost Liquid Trucks
      ELSEIF(K.EQ.17) THEN
          STDCST = 15000
      ELSEIF(K.EQ.20.OR.K.EQ.24) THEN
          STDCST = 1000000
!          STDCST = 10                           !low cost Pipeline
      ENDIF

      RETURN
      END FUNCTION STDCST

      FUNCTION CC1(K,VD,VE,VG,VH,VI,VAE,VBH,VBI,C3,C4)

      IMPLICIT NONE

      REAL*8 VD,VE,VG,VH,VI,VAE,VBH,VBI,C3,C4
      REAL*8 CC1
      INTEGER I,K

      CC1 = 0.0
      IF (K.EQ.2.OR.K.EQ.3.OR.K.EQ.6.OR.K.EQ.8.OR.K.EQ.9.OR. &
               K.EQ.12.OR.K.EQ.15.OR.K.EQ.19) THEN
        CC1 = (VE/VG)**VI*VH
      ELSEIF(K.EQ.4.OR.K.EQ.7.OR.K.EQ.13.OR.K.EQ.16) THEN
        CC1 = (VH*(VE/VG)**VI)*VBH*VBI
      ELSEIF(K.EQ.5.OR.K.EQ.17) THEN
        CC1 = VH
      ELSEIF(K.EQ.10) THEN
        CC1 = (VE/100000)**VI*VH*VD/24/C3
      ELSEIF(K.EQ.11) THEN
        CC1 = VH*VD
      ELSEIF(K.EQ.20.OR.K.EQ.24) THEN
        CC1 = VH*VAE*C4
      ELSEIF(K.EQ.21.OR.K.EQ.25.OR.K.EQ.23.OR.K.EQ.27) THEN
        CC1 = 100000
      ELSEIF(K.EQ.22.OR.K.EQ.26) THEN
        CC1 = 450000
!        CC1 = 450                                 !low cost LLTrucks
      ENDIF

      RETURN
      END FUNCTION CC1

      FUNCTION FDIST(K,TK,TM)

      IMPLICIT NONE

      REAL*8 TK,TM
      REAL*8 FDIST
      INTEGER I,K

      FDIST = 0.0

      IF (K.EQ.20) THEN
          FDIST = TK
      ELSEIF(K.EQ.21.OR.K.EQ.22.OR.K.EQ.23) THEN
          FDIST = TK*2
      ELSEIF(K.EQ.24) THEN
          FDIST = MIN(TM,20.0)
      ELSEIF(K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
           FDIST= MIN(TM,20.0)*2
      ENDIF

      RETURN
      END FUNCTION FDIST

      FUNCTION CC2(K,VN,VO,VP,VQ,VK,VL)
      IMPLICIT NONE

      REAL*8 VN,VO,VP,VQ,VK,VL
      REAL*8 CC2
      INTEGER I,K

      CC2=0.0

      CC2=(1+VN+VO+VP+VQ)*(VK+VL)

      RETURN
      END FUNCTION CC2


      FUNCTION CS1(K,VA,VP)

      IMPLICIT NONE

      REAL*8 VA,VP
      REAL*8 CS1
      INTEGER I,K

      CS1=0.0

      IF (K.EQ.2.OR.K.EQ.6.OR.K.EQ.7.OR.K.EQ.9.OR.K.EQ.10.OR. &
          K.EQ.20.OR.K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR.K.EQ.24.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEn
         CS1= VP
      ELSE
         CS1=VA
      ENDIF

      RETURN
      END FUNCTION CS1


      FUNCTION CC3(K,VR,VE)

      IMPLICIT NONE

      REAL*8 VR,VE
      REAL*8 CC3
      INTEGER I,K

      CC3 = 0.0
        IF(K.NE.21.AND.K.NE.22.AND. &
           K.NE.23.AND.K.NE.24.AND.K.NE.25.AND.K.NE.26.AND. &
           K.NE.27) THEN
           CC3=VR/VE
        ENDIF

      RETURN
      END FUNCTION CC3

      FUNCTION ASPD(K,VX)

      IMPLICIT NONE

      REAL*8 VX
      REAL*8 ASPD
      INTEGER I,K

      ASPD= 0.0
        IF(K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
           K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        ASPD=VX/0.6
        ENDIF

      RETURN
      END FUNCTION  ASPD

      FUNCTION APKG(K,VE,VDP,VKG)

      IMPLICIT NONE

      REAL*8 VE,VDP,VKG
      REAL*8 APKG
      INTEGER I,K

      APKG= 0.0
        IF(K.EQ.2.OR.K.EQ.6.OR.K.EQ.7.OR.K.EQ.9.OR.K.EQ.10.OR. &
           K.EQ.20.OR.K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR.K.EQ.24.OR. &
           K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
             APKG=VE*VDP
        ELSE
             APKG=VKG
        ENDIF

      RETURN
      END FUNCTION APKG

      FUNCTION FUT(K,VE,VAF)

      IMPLICIT NONE

      REAL*8 VE,VAF
      REAL*8 FUT
      INTEGER I,K

      FUT= 0.0
        IF(K.LT.20) THEN
             FUT=VAF/(VE*365)
        ELSE
             FUT=.800
        ENDIF

      RETURN
      END FUNCTION FUT

      FUNCTION TPTH(K,VAE,VY)

      IMPLICIT NONE

      REAL*8 VAE,VY
      REAL*8 TPTH
      INTEGER I,K

      TPTH= 0.0
      IF (K.EQ.21 .OR. K.EQ.22 .OR. K.EQ.23 .OR. K.EQ.25 .OR. K.EQ.26 .OR. K.EQ.27) THEN
             TPTH=VAE/VY
      ENDIF

      RETURN
      END FUNCTION TPTH

      FUNCTION TBOIL(K,VZ,VAX)

      IMPLICIT NONE

      REAL*8 VZ,VAX
      REAL*8 TBOIL
      INTEGER I,K

      TBOIL= 1.0000
      IF     (K.EQ.20 .OR. K.EQ.24) THEN
        TBOIL=0.9500
      ELSEIF (K.EQ.21 .OR. K.EQ.25) THEN
        TBOIL = 250.00/300.00
      ELSEIF (K.EQ.22 .OR. K.EQ.23 .OR. K.EQ.26 .OR. K.EQ.27) THEN
        TBOIL = (1-VZ)**VAX
      ENDIF

      RETURN
      END FUNCTION TBOIL

      FUNCTION CC4(K,VR,VBF,VK,VL)

      IMPLICIT NONE

      REAL*8 VR,VBF,VK,VL
      REAL*8 CC4
      INTEGER I,K

      CC4=0.0

      CC4=VR
      IF(K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
         K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        CC4=VBF*(VK+VL)
      ENDIF

      RETURN
      END FUNCTION CC4

      FUNCTION AFE(K,VI,VAF,VAD,VC,VBG)

      IMPLICIT NONE

      REAL*8 VI,VAF,VAD,VC,VBG
      REAL*8 AFE
      INTEGER I,K

      AFE=0.0

      AFE = VI/1000*VAF*VAD
      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        AFE=VBG*VC
      ENDIF

      IF (ISNAN(AFE).OR. ABS(AFE) .GT. HUGE(AFE)) THEN   ! check for NaNQ this way
         WRITE(6,1000) K,VI,VAF,VAD,VC,VBG,AFE
 1000    FORMAT(1X,"AFErsc",":",I4,6(":",F15.6))
      END IF

      RETURN
      END FUNCTION AFE

      FUNCTION ALFO(K,VAH,VT,VBC,VD)

      IMPLICIT NONE

      REAL*8 VAH,VT,VBC,VD
      REAL*8 ALFO
      INTEGER I,K

      ALFO=0.0

      ALFO=VAH*VT
      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        ALFO=VBC*VD
      ENDIF

      RETURN
      END FUNCTION ALFO

      FUNCTION NTPY(K,VAF,VG)

      IMPLICIT NONE

      REAL*8 VAF,VG
      REAL*8 NTPY
      INTEGER I,K

      NTPY=0.0

      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        NTPY=VAF/VG
      ENDIF

      RETURN
      END FUNCTION NTPY

      FUNCTION TKD(K,VAU,VAE)

      IMPLICIT NONE

      REAL*8 VAU,VAE
      REAL*8 TKD
      INTEGER I,K

      TKD=0.0

      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        TKD=VAU*VAE
      ENDIF

      RETURN
      END FUNCTION TKD

      FUNCTION MILS(K,VAV,C4)

      IMPLICIT NONE

      REAL*8 VAV,C4
      REAL*8 MILS
      INTEGER I,K

      MILS=0.0

      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        MILS=VAV*C4
      ENDIF

      RETURN
      END FUNCTION MILS

      FUNCTION TDTH(K,VAX,VAU)

      IMPLICIT NONE

      REAL*8 VAX,VAU
      REAL*8 TDTH
      INTEGER I,K

      TDTH=0.0

      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        TDTH=VAX*VAU
      ENDIF

      RETURN
      END FUNCTION TDTH

      FUNCTION TLTH(K,VAU,VV)

      IMPLICIT NONE

      REAL*8 VAU,VV
      REAL*8 TLTH
      INTEGER I,K

      TLTH=0.0

      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        TLTH=VAU*VV
      ENDIF

      RETURN
      END FUNCTION TLTH

      FUNCTION TDT(K,VBB,VAX,VAU)

      IMPLICIT NONE

      REAL*8 VBB,VAX,VAU
      REAL*8 TDT
      INTEGER I,K

      TDT=0.0

      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        TDT=VBB+VAX*VAU
      ENDIF

      RETURN
      END FUNCTION TDT

      FUNCTION OH(K,VBC,VY)

      IMPLICIT NONE

      REAL*8 VBC,VY
      REAL*8 OH
      INTEGER I,K

      OH=0.0

      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        OH=VBC/VY
      ENDIF

      RETURN
      END FUNCTION OH

      FUNCTION NT(K,VBD)

      IMPLICIT NONE

      REAL*8 VBD
      REAL*8 NT
      INTEGER I,K

      NT=0.0

      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        NT=VBD/24
      ENDIF

      RETURN
      END FUNCTION NT

      FUNCTION AEU(K,VAW,VW)

      IMPLICIT NONE

      REAL*8 VAW,VW
      REAL*8 AEU
      INTEGER I,K

      AEU=0.0

      IF (K.EQ.21.OR.K.EQ.22.OR.K.EQ.23.OR. &
          K.EQ.25.OR.K.EQ.26.OR.K.EQ.27) THEN
        AEU=VAW/VW
      ENDIF

      RETURN
      END FUNCTION AEU

      FUNCTION NTR(K,VBE,VAA)

      IMPLICIT NONE

      REAL*8 VBE,VAA
      REAL*8 NTR
      INTEGER I,K

      NTR=0.0

      IF (K.EQ.21 .OR. K.EQ.22 .OR. K.EQ.23 .OR. &
          K.EQ.25 .OR. K.EQ.26 .OR. K.EQ.27) THEN
        NTR=VBE/VAA
      ENDIF

      RETURN
      END FUNCTION NTR



      SUBROUTINE HVAL(COL,RW,VAL)
! This subroutine revises column/row intersections

      IMPLICIT NONE

      include 'omlall.fi'

      REAL*8 VAL,CVALUE,RVAL
      character*16 COL,RW
      INTEGER IRET,REVISE,IRET2

      IRET = 0

      IF (VAL .NE. 0.0) IRET = DFMCVAL(COL,RW,VAL)
      IF (IRET .NE. 0) THEN
         WRITE(6,100) IRET,COL,RW,VAL
      ENDIF

      IF (IRET .NE. 0 .OR. ISNAN(VAL).OR. ABS(VAL) .GT. HUGE(VAL)) THEN   ! check for NaNQ this way
         WRITE(6,24) IRET,COL,RW,VAL
      ENDIF                                      !
  24  FORMAT(1X,"HMM_HVAL_ERROR_CODE",1(":",I8),2(":",A8),":",E20.6)

100   FORMAT(1X,'REVISE ERROR HVAL ',I8,1X,A8,1X,A8,1X,F12.5,1X,I4,1X,F12.5)

      RETURN
      END SUBROUTINE HVAL


      SUBROUTINE HRHS(RHS,RW,VAL)
! This subroutine revises row right-hand side values

      IMPLICIT NONE

      include 'omlall.fi'

      REAL*8 VAL,RVAL,VALR
      character*16 COL,RW,RHS
      INTEGER IRET,IRET2

      IRET = 0

!     CALL DATABASE REVISE ROUTINE.

      IF (VAL .NE. 0.0) IRET = DFMCRHS(RHS,RW,VAL)
      IF (IRET .NE. 0) THEN
         WRITE(6,100) IRET,RW,VAL
      ENDIF
!
      IF (IRET .NE. 0 .OR. ISNAN(VAL).OR. ABS(VAL) .GT. HUGE(VAL)) THEN   ! check for NaNQ this way
         WRITE(6,24) IRET,RHS,RW,VAL
      ENDIF                                      !
  24  FORMAT(1X,"HMM_HRHS_ERROR_CODE",1(":",I4),2(":",A8),":",E20.6)

100   FORMAT(1X,'REVISE ERROR HRHS ',I4,1X,A8,1X,F12.5,1X,I4,F12.5)

      RETURN
      END SUBROUTINE HRHS


      SUBROUTINE HBND(BND,COL,LVAL,UVAL)
! This subroutine revises column upper and lower bounds

      IMPLICIT NONE

      include 'omlall.fi'

      REAL*8 VAL,RVAL,LVAL,UVAL,VALL,VALU,LVALR,UVALR
      character*16 COL,RW,RHS,BND
      INTEGER IRET,IRET2

      IRET = 0

!     CALL DATABASE REVISE ROUTINE.

      IRET = DFMCBND(BND,COL,LVAL,UVAL)
      IF (IRET .NE. 0) THEN
         WRITE(6,100) IRET,COL,LVAL,UVAL
      ENDIF

      IF (IRET .NE. 0 .OR. ISNAN(LVAL).OR. ABS(LVAL) .GT. HUGE(LVAL) .OR. ISNAN(UVAL) .OR. ABS(UVAL) .GT. HUGE(UVAL)) THEN ! check for NaNQ this way
         WRITE(6,24) IRET,COL,BND,LVAL,UVAL
      ENDIF                                      !
  24  FORMAT(1X,"HMM_HBND_ERROR_CODE",1(":",I4),2(":",A8),2(":",E20.6))

100   FORMAT(1X,'REVISE ERROR HBND ',I4,1X,A8,1X,F12.5,1X,F20.12,1X,I4,1X,F12.5,1X,F20.12)

      RETURN
      END SUBROUTINE HBND


      SUBROUTINE INSTALL_CST(CLT,LCP,OVRCST,PROFILE,CAPESC,GNPF,INTR,DEBT_F,ROR,TXBOOK,BVBOOK)
! This subroutine calculates the installed cost of a generating unit

      IMPLICIT NONE

      INTEGER*4 CLT                    ! CONSTRUCTION LEAD TIME -- ACTUAL
      INTEGER*4 LCP                    ! MAXIMUM # OF YEARS IN CONSTR PROFILE (I.E. ARRAY SIZE)
      REAL*8 OVRCST                    ! OVERNIGHT COSTS
      REAL*8 PROFILE(LCP)              ! ANNUAL EXPENDITURE PERCENTAGE
      REAL*8 CAPESC(LCP)               ! CAPITAL ESCALATION/REAL
      REAL*8 GNPF(LCP)                 ! GENERAL INFLATION
      REAL*8 INTR                      ! INTEREST COSTS
      REAL*8 DEBT_F                    ! CONSTRUCTION DEBT FINANCING
      REAL*8 ROR                       ! RATE OF RETURN

! RESULTS:
      REAL*8 TXBOOK                    ! TAX BASIS INSTALLED COSTS
      REAL*8 BVBOOK                    ! BOOK VALUE INSTALLED COSTS
      REAL*8 YCWP                      ! ANNUAL CONSTRUCTION WORK IN PROGRESS
      INTEGER*4 I

      TXBOOK = DBLE(0.0)
      BVBOOK = DBLE(0.0)
      DO I = 1 , CLT
         YCWP = OVRCST * PROFILE(I) * CAPESC(I) * GNPF(I)
         TXBOOK = TXBOOK + YCWP + (BVBOOK + (0.5 * YCWP)) * INTR * DEBT_F
         BVBOOK = BVBOOK + YCWP + (BVBOOK + (0.5 * YCWP)) * ROR
      END DO

      RETURN
      END


      SUBROUTINE ANNUITY_FACTOR(TL,CL,DL2,DF2,RATIO,ROE,INT,TR,CAP,AF_HMMO)
! This subroutine calculates the factor that converts the installed cost to a nominal annuity

      IMPLICIT NONE

      INTEGER I, M, N, AF_HMMO, J, K
      INTEGER DL2                 ! DEBT LOAN LIFE <= CONTRACT LIFE
      INTEGER CL                  ! CONTRACT LIFE
      INTEGER TL                  ! TAX LIFE ASSOCIATED WITH TECHNOLOGY
      REAL*8 ROE                  ! ROE
      REAL*8 INT                  ! INTEREST RATE
      REAL*8 DF2                  ! DEBT FRACTION
      REAL*8 TR                   ! MARGINAL FEDERAL INCOME TAX RATE
      REAL*8 RATIO                ! TX TO BV RATIO

! KEY LOCAL VARIABLES ARE:
      REAL*8 EFRAC2               ! EQUITY FRACTION

      REAL*8 DPP                  ! DEBT PRINCIPAL PAYMENT
      REAL*8 PVDPP                ! PV OF DEBT PRINCIPAL PAYMENTS
      REAL*8 PVDEBT               ! PV OF OUTSTANDING DEBT PRINCIPAL
      REAL*8 PVINT                ! PV OF INTEREST PAYMENTS
      REAL*8 PVTB                 ! PV OF TAX DEPR BENEFITS
      REAL*8 CAP                  ! RESULT !!!!


! THE OUTPUT OF THE ROUTINE IS THE FRACTION OF THE INSTALLED COST
! REQUIRED FOR RECOVERY EACH YEAR DURING THE CONTRACT LIFE TO YIELD
! THE TARGET ROE AS AN IRR TO THE PROJECT OWNER (I.E.,CAP)

      REAL*8 CRF                     ! CAPITAL RECOVERY FACTOR  FUNCTION
      REAL*8 PVV                     ! PRESENT VALUE OF VECTOR  FUNCTION
      REAL*8 X
      REAL*8 TAXDEPRH(26,6)

      LOGICAL       DO_ONCE/.FALSE./
      LOGICAL       NEW/.FALSE./
      CHARACTER*18  FILENM
      INTEGER       FILE_MGR
      EXTERNAL      FILE_MGR
      INTEGER*4    RD$TBL,RD$C1,RET_CODE,RD$R82,I_CNTL
      INTEGER*4    DMSZ
      PARAMETER (DMSZ = 300)
      CHARACTER*40 DUMMY(DMSZ)             ! DUMMY COLUMN IN DATA TABLES

      EFRAC2 = 1 - DF2
      X = DBLE(DL2)
      IF (DL2 .GT. CL) THEN
         WRITE(18,*)'DEBT LIFE GT CONTRACT LIFE ',DL2,CL
         DL2 = CL
      ENDIF

      DPP = 1/X * DF2
      PVDPP = DPP/CRF(ROE,DL2)
      PVDEBT = (X/ROE * (1 - ((1 - (1 + ROE) ** ( - X))/(X * ROE))) / X) * DF2
      PVINT = PVDEBT * INT

      IF (TL .EQ. 5) THEN
         I = 1
      ELSEIF (TL .EQ. 7) THEN
         I = 2
      ELSEIF (TL .EQ. 10) THEN
         I = 3
      ELSEIF (TL .EQ. 15) THEN
         I = 4
      ELSEIF (TL .EQ. 20) THEN
         I = 5
      ELSE
         I = 5
      END IF

      N = 21
      M = TL


      IF (.NOT. DO_ONCE) THEN

! OPEN THE EMM CONTROL FILE

         NEW    = .FALSE.
         FILENM = 'EMMCNTL'
         I_CNTL = FILE_MGR('O',FILENM,NEW)

! READ IN TAX DEPRECIATION RATES

         RET_CODE = RD$TBL(I_CNTL,'%TAX DEP RT%',26,AF_HMMO,AF_HMMO)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$R82(TAXDEPRH,1,1,6,26,6)     ! TAX DEPRECIATION RATES
!         DO J=1,26,1
!           WRITE(6,1312) (TAXDEPRH(J,K),K=1,6)
!1312       FORMAT(1X,"ECHOHMMTAXDEPR",6(":",F6.3))
!         ENDDO


! CLOSE THE EMM CONTROL FILE

         I_CNTL   = FILE_MGR('C',FILENM,NEW)
!         WRITE(6,1313) I, TAXDEPRH(1,I)
!1313     FORMAT(1X,"HMMTAXDEPR",":",I4,":",F9.3)
         DO_ONCE = .TRUE.

      ENDIF

      PVTB = PVV(TAXDEPRH(1,I),N,M,ROE) * RATIO

      CAP = (EFRAC2/(1 - TR) + PVINT - (TR/(1 - TR) * PVTB) + PVDPP / (1 - TR))
      CAP = CAP * CRF(ROE,CL)

      RETURN
      END
